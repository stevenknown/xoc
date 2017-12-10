/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Su Zhenyu nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

#ifdef _DEBUG_
static bool checkLogicalOp(IR_TYPE irt, Type const* type, TypeMgr * tm)
{
    switch (irt) {
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_LAND:
    case IR_LOR:
        ASSERT0(type == tm->getBool());
        break;
    default:;
    }
    return true;
}


static bool is_reduction(IR const* ir)
{
    ASSERT0(ir->is_stmt());
    DUMMYUSE(is_reduction);
    if (!ir->is_st() && !ir->is_stpr()) { return false; }
    IR * rhs = ir->getRHS();

    //Make sure self modify stmt is monotonic.
    if (!rhs->is_add() && !rhs->is_sub()) {
        //TODO: support more reduction operation.
        return false;
    }

    IR * op0 = BIN_opnd0(rhs);
    IR * op1 = BIN_opnd1(rhs);

    if (op0->is_const() && !op1->is_const()) {

        IR * t = op0;
        op0 = op1;
        op1 = t;
    } else if ((!op0->is_const() && !op1->is_const()) ||
               (op0->is_const() && op1->is_const())) {
        return false;
    }

    ASSERT0(!op0->is_const() && op1->is_const());

    if (ir->is_st()) {

        if (!op0->is_ld()) { return false; }
        if (LD_idinfo(op0) != ST_idinfo(ir)) { return false; }
        if (LD_ofst(op0) != ST_ofst(ir)) { return false; }
    } else if (ir->is_stpr()) {

        if (!op0->is_pr()) { return false; }
        if (PR_no(op0) != STPR_no(ir)) { return false; }
    }

    if (!g_is_support_dynamic_type) {
        if (!op1->is_int() && !op1->is_fp()) {
            return false;
        }
    }

    return true;
}
#endif


//
//START AnalysisInstrument
//
AnalysisInstrument::AnalysisInstrument(Region * rg) :
    m_mds_mgr(rg, &m_sbs_mgr),
    m_mds_hash_allocator(&m_sbs_mgr),
    m_mds_hash(&m_mds_hash_allocator)
{
    m_ru = rg;
    m_ru_mgr = NULL;
    m_call_list = NULL;
    m_return_list = NULL;
    m_ir_list = NULL;
    m_pass_mgr = NULL;

    //Counter of IR_PR, and do not use '0' as prno.
    m_pr_count = 1;
    m_pool = smpoolCreate(256, MEM_COMM);
    m_du_pool = smpoolCreate(sizeof(DU) * 4, MEM_CONST_SIZE);
    memset(m_free_tab, 0, sizeof(m_free_tab));
}


//Free md's id and local-var's id back to MDSystem and VarMgr.
//The index of MD and VAR is important resource if there
//are a lot of REGIONs in RegionMgr.
//Note this function does NOT process GLOBAL variable.
static void destroyVARandMD(Region * rg, AnalysisInstrument * anainstr)
{
    VarMgr * varmgr = rg->getVarMgr();
    MDSystem * mdsys = rg->getMDSystem();
    VarTabIter c;
    ConstMDIter iter;
    for (VAR * v = ANA_INS_var_tab(anainstr).get_first(c);
         v != NULL; v = ANA_INS_var_tab(anainstr).get_next(c)) {
        ASSERT0(anainstr->verify_var(varmgr, v));
        mdsys->removeMDforVAR(v, iter);
        varmgr->destroyVar(v);
    }
}


AnalysisInstrument::~AnalysisInstrument()
{
    #if defined(_DEBUG_) && defined(DEBUG_SEG)
    //Just dump the seg info if you really need to see.
    //DefSegMgr * segmgr = m_sbs_mgr.getSegMgr();
    //dumpSegMgr(segmgr, g_tfile);
    #endif

    //Destroy pass manager.
    if (m_pass_mgr != NULL) {
        delete m_pass_mgr;
        m_pass_mgr = NULL;
    }

    //Free AIContainer's internal structure.
    //The vector of AIContainer must be destroyed explicitly.
    INT l = m_ir_vector.get_last_idx();
    for (INT i = 1; i <= l; i++) {
        IR * ir = m_ir_vector.get(i);
        ASSERT0(ir);
        //if (ir->is_region()) {
        //    //All region should be deleted by regionmgr.
        //    m_ru_mgr->deleteRegion(REGION_ru(ir));
        //}

        if (IR_ai(ir) != NULL) {
            IR_ai(ir)->destroy_vec();
        }
        ir->freeDUset(m_sbs_mgr);
    }

    //Free local VAR id and related MD id, and destroy the memory.
    destroyVARandMD(m_ru, this);

    //Destroy reference info.
    if (REGION_refinfo(m_ru) != NULL) {
        REF_INFO_mayuse(REGION_refinfo(m_ru)).clean(m_sbs_mgr);
        REF_INFO_maydef(REGION_refinfo(m_ru)).clean(m_sbs_mgr);

        //REGION_refinfo allocated in pool.
        REGION_refinfo(m_ru) = NULL;
    }

    //Destory CALL list.
    if (m_call_list != NULL) {
        delete m_call_list;
        m_call_list = NULL;
    }

    //Destory RETURN list.
    if (m_return_list != NULL) {
        delete m_return_list;
        m_return_list = NULL;
    }

    ////////////////////////////////////////////////////////////
    //Do NOT destroy member which allocated in pool after here//
    ////////////////////////////////////////////////////////////

    //Destroy all IR. IR allocated in the pool.
    smpoolDelete(m_pool);
    m_pool = NULL;

    //Destroy all DUSet which allocated in the du_pool.
    smpoolDelete(m_du_pool);
    m_du_pool = NULL;
    m_ir_list = NULL;
}


bool AnalysisInstrument::verify_var(VarMgr * vm, VAR * v)
{
    CHECK_DUMMYUSE(v);
    CHECK_DUMMYUSE(vm);
    if (m_ru->is_function() || m_ru->is_eh() ||
        REGION_type(m_ru) == REGION_INNER) {
        //If var is global but unallocable, it often be
        //used as placeholder or auxilary var.

        //For these kind of regions, there are only local variable or
        //unablable global variable is legal.
        ASSERT0(VAR_is_local(v) || !VAR_allocable(v));
    } else if (m_ru->is_program()) {
        //Theoretically, only global variable is legal in program region.
        //However even if the program region there may be local
        //variables, e.g: PR, a kind of local variable.
        //ASSERT0(VAR_is_global(v));
    } else {
        ASSERT(0, ("unsupport variable type."));
    }
    return true;
}


size_t AnalysisInstrument::count_mem()
{
    size_t count = 0;
    if (m_call_list != NULL) {
        count += m_call_list->count_mem();
    }

    count += smpoolGetPoolSize(m_pool);
    count += smpoolGetPoolSize(m_du_pool);

    count += m_ru_var_tab.count_mem();
    count += m_ir_bb_mgr.count_mem();
    count += m_prno2var.count_mem();
    count += m_bs_mgr.count_mem();
    count += m_sbs_mgr.count_mem();
    count += m_mds_mgr.count_mem();
    count += m_mds_hash.count_mem();
    count += m_ir_vector.count_mem();
    count += m_ir_bb_list.count_mem();
    return count;
}
//END AnalysisInstrument


//
//START Region
//
#ifdef _DEBUG_
//Make sure IR_ICALL is the largest ir.
static bool ck_max_ir_size()
{
    //Change MAX_OFFSET_AT_FREE_TABLE if IR_ICALL is not the largest.
    for (UINT i = IR_UNDEF; i < IR_TYPE_NUM; i++) {
        ASSERT0(IRTSIZE(i) <= IRTSIZE(IR_ICALL));
    }
    return true;
}
#endif


void Region::init(REGION_TYPE rt, RegionMgr * rm)
{
    //Reset
    //REGION_is_expect_inline(r),
    //REGION_is_inlinable(r),
    //REGION_is_mddu_valid(r),
    //REGION_is_readonly(r)
    m_u2.s1b1 = 0;

    REGION_type(this) = rt;
    REGION_blx_data(this) = NULL;
    m_var = NULL;

    REGION_id(this) = 0;
    REGION_parent(this) = NULL;
    REGION_refinfo(this) = NULL;
    REGION_analysis_instrument(this) = NULL;

    if (is_program() ||
        is_function() ||
        is_eh() ||
        is_subregion()) {
        //All these Region involve ir list.
        REGION_analysis_instrument(this) = new AnalysisInstrument(this);
        if (is_program() || is_function()) {
            //All these Region involve ir list.
            ASSERT0(rm);
            ANA_INS_ru_mgr(REGION_analysis_instrument(this)) = rm;
        }
    }
    ASSERT0(ck_max_ir_size());
}


void Region::destroy()
{
    destroyPassMgr();

    if ((is_subregion() ||
         is_function() ||
         is_eh() ||
         is_program()) &&
        REGION_analysis_instrument(this) != NULL) {
        delete REGION_analysis_instrument(this);
    }
    REGION_analysis_instrument(this) = NULL;

    m_var = NULL;

    //MDSET would be freed by MDSetMgr.
    REGION_refinfo(this) = NULL;
    REGION_id(this) = 0;
    REGION_parent(this) = NULL;
    REGION_type(this) = REGION_UNDEF;
}


size_t Region::count_mem()
{
    size_t count = 0;
    if ((is_subregion() ||
         is_function() ||
         is_eh() ||
         is_program()) &&
        REGION_analysis_instrument(this) != NULL) {
        count += REGION_analysis_instrument(this)->count_mem();
        count += sizeof(*REGION_analysis_instrument(this));
    }

    if (m_ref_info != NULL) {
        count += m_ref_info->count_mem();
    }
    return count;
}


//Build IR_PR operation by specified prno and type id.
IR * Region::buildPRdedicated(UINT prno, Type const* type)
{
    ASSERT0(type);
    IR * ir = allocIR(IR_PR);
    PR_no(ir) = prno;
    IR_dt(ir) = type;
    return ir;
}


//Build IR_PR operation by specified type id.
IR * Region::buildPR(Type const* type)
{
    ASSERT0(type);
    IR * ir = allocIR(IR_PR);
    PR_no(ir) = REGION_analysis_instrument(this)->m_pr_count++;
    IR_dt(ir) = type;
    return ir;
}


//Generate a PR number by specified prno and type id.
//This operation will allocate new PR number.
UINT Region::buildPrno(Type const* type)
{
    ASSERT0(type);
    DUMMYUSE(type);
    return REGION_analysis_instrument(this)->m_pr_count++;
}


//Build IR_LNOT operation.
IR * Region::buildLogicalNot(IR * opnd0)
{
    return buildUnaryOp(IR_LNOT, getTypeMgr()->getBool(), opnd0);
}


//Build Logical operations, include IR_LAND, IR_LOR, IR_XOR.
IR * Region::buildLogicalOp(IR_TYPE irt, IR * opnd0, IR * opnd1)
{
    IR * ir = allocIR(irt);
    ASSERT0(opnd0 && opnd1);
    ASSERT0(irt == IR_LAND || irt == IR_LOR || irt == IR_XOR);
    BIN_opnd0(ir) = opnd0;
    BIN_opnd1(ir) = opnd1;
    IR_parent(opnd0) = ir;
    IR_parent(opnd1) = ir;
    IR_dt(ir) = getTypeMgr()->getBool();
    return ir;
}


//Build IR_ID operation.
IR * Region::buildId(VAR * var)
{
    ASSERT0(var);
    IR * ir = allocIR(IR_ID);
    ASSERT0(var != NULL);
    ID_info(ir) = var;
    IR_dt(ir) = VAR_type(var);
    return ir;
}


IR * Region::buildLdaString(CHAR const* varname, CHAR const * string)
{
    return buildLdaString(varname, getRegionMgr()->addToSymbolTab(string));
}


IR * Region::buildLdaString(CHAR const* varname, SYM * string)
{
    ASSERT0(string);
    VAR * v = getVarMgr()->registerStringVar(varname, string, 1);
    return buildLda(v);
}


//Build IR_LDA operation.
IR * Region::buildLda(VAR * var)
{
    ASSERT0(var);
    IR * ir = allocIR(IR_LDA);
    LDA_idinfo(ir) = var;
    IR_dt(ir) = getTypeMgr()->getPointerType(var->getByteSize(getTypeMgr()));
    return ir;
}


//Build IR_CONST operation.
//The result IR indicates a string.
IR * Region::buildString(SYM const* strtab)
{
    ASSERT0(strtab);
    IR * str = allocIR(IR_CONST);
    CONST_str_val(str) = strtab;
    IR_dt(str) = getTypeMgr()->getSimplexTypeEx(D_STR);
    return str;
}


//Build conditionally selected expression.
//The result depends on the predicator's value.
//e.g: x = a > b ? 10 : 100
//Note predicator may not be judgement expression.
IR * Region::buildSelect(
        IR * pred,
        IR * true_exp,
        IR * false_exp,
        Type const* type)
{
    ASSERT0(type);
    ASSERT0(pred && pred->is_single() && true_exp && false_exp);
    ASSERT0(true_exp->is_exp() && true_exp->is_single());
    ASSERT0(false_exp->is_exp() && false_exp->is_single());

    //Type of true exp may be not equal to false exp.
    //ASSERT0(true_exp->get_type() == false_exp->get_type());
    IR * ir = allocIR(IR_SELECT);
    IR_dt(ir) = type;
    SELECT_pred(ir) = pred;
    SELECT_trueexp(ir) = true_exp;
    SELECT_falseexp(ir) = false_exp;

    IR_parent(pred) = ir;
    IR_parent(true_exp) = ir;
    IR_parent(false_exp) = ir;
    return ir;
}


//Build IR_LABEL operation.
IR * Region::buildIlabel()
{
    IR * ir = allocIR(IR_LABEL);
    IR_dt(ir) = getTypeMgr()->getVoid();
    LAB_lab(ir) = genIlabel();
    return ir;
}


//Build IR_LABEL operation.
IR * Region::buildLabel(LabelInfo const* li)
{
    ASSERT0(li && LABEL_INFO_type(li) != L_UNDEF);
    IR * ir = allocIR(IR_LABEL);
    IR_dt(ir) = getTypeMgr()->getVoid();
    LAB_lab(ir) = li;
    return ir;
}


//Build IR_CVT operation.
//exp: the expression to be converted.
//tgt_ty: the target type that you want to convert.
IR * Region::buildCvt(IR * exp, Type const* tgt_ty)
{
    ASSERT0(tgt_ty);
    ASSERT0(exp);
    IR * ir = allocIR(IR_CVT);
    CVT_exp(ir) = exp;
    IR_dt(ir) = tgt_ty;
    IR_parent(exp) = ir;
    return ir;
}


//Build IR_PHI operation.
//'res': result pr of PHI.
IR * Region::buildPhi(UINT prno, Type const* type, UINT num_opnd)
{
    ASSERT0(type);
    ASSERT0(prno > 0);
    IR * ir = allocIR(IR_PHI);
    PHI_prno(ir) = prno;
    IR_dt(ir) = type;

    IR * last = NULL;
    for (UINT i = 0; i < num_opnd; i++) {
        IR * x = buildPRdedicated(prno, type);
        PR_ssainfo(x) = NULL;
        xcom::add_next(&PHI_opnd_list(ir), &last, x);
        IR_parent(x) = ir;
    }
    return ir;
}


//Build IR_PHI operation.
//'res': result pr of PHI.
IR * Region::buildPhi(UINT prno, Type const* type, IR * opnd_list)
{
    ASSERT0(type);
    ASSERT0(prno > 0);
    IR * ir = allocIR(IR_PHI);
    PHI_prno(ir) = prno;
    IR_dt(ir) = type;
    for (IR * opnd = opnd_list; opnd != NULL; opnd = opnd->get_next()) {
        ASSERT0(opnd->is_pr() || opnd->is_const());
        if (opnd->is_pr()) {
            PR_ssainfo(opnd) = NULL;
        }
        IR_parent(opnd) = ir;
    }
    PHI_opnd_list(ir) = opnd_list;
    return ir;
}


//Build IR_CALL operation.
//'res_list': reture value list.
//'result_prno': indicate the result PR which hold the return value.
//    0 means the call does not have a return value.
//'type': result PR data type.
IR * Region::buildCall(
        VAR * callee,
        IR * param_list,
        UINT result_prno,
        Type const* type)
{
    ASSERT0(type);
    ASSERT0(callee);
    IR * ir = allocIR(IR_CALL);
    CALL_param_list(ir) = param_list;
    CALL_prno(ir) = result_prno;
    CALL_idinfo(ir) = callee;
    IR_dt(ir) = type;
    while (param_list != NULL) {
        IR_parent(param_list) = ir;
        param_list = IR_next(param_list);
    }
    return ir;
}


//Build IR_ICALL operation.
//'res_list': reture value list.
//'result_prno': indicate the result PR which hold the return value.
//    0 means the call does not have a return value.
//'type': result PR data type.
//    0 means the call does not have a return value.
IR * Region::buildIcall(
        IR * callee,
        IR * param_list,
        UINT result_prno,
        Type const* type)
{
    ASSERT0(type);
    ASSERT0(callee);
    IR * ir = allocIR(IR_ICALL);
    ASSERT0(!callee->is_id());
    CALL_param_list(ir) = param_list;
    CALL_prno(ir) = result_prno;
    ICALL_callee(ir) = callee;
    IR_dt(ir) = type;

    IR_parent(callee) = ir;
    while (param_list != NULL) {
        IR_parent(param_list) = ir;
        param_list = IR_next(param_list);
    }
    return ir;
}


//Build IR_REGION operation.
IR * Region::buildRegion(Region * rg)
{
    ASSERT0(rg && !rg->is_undef());
    ASSERT(rg->getRegionVar(), ("region should bond with a variable"));
    IR * ir = allocIR(IR_REGION);
    IR_dt(ir) = getTypeMgr()->getVoid();
    REGION_ru(ir) = rg;
    REGION_parent(rg) = this;

    #ifdef _DEBUG_
    if (rg->is_function()) {
        ASSERT(is_program() || is_function(),
            ("Only program or function region can have a"
             " function region as subregion."));
    }
    #endif

    return ir;
}


//Build IR_IGOTO unconditional multi-branch operation.
//vexp: expression to determine which case entry will be target.
//case_list: case entry list. case entry is consist of expression and label.
IR * Region::buildIgoto(IR * vexp, IR * case_list)
{
    ASSERT0(vexp && vexp->is_exp());
    ASSERT0(case_list);

    IR * ir = allocIR(IR_IGOTO);
    IR_dt(ir) = getTypeMgr()->getVoid();
    IGOTO_vexp(ir) = vexp;
    IGOTO_case_list(ir) = case_list;
    IR_parent(vexp) = ir;

    IR * c = case_list;
    while (c != NULL) {
        ASSERT0(c->is_case());
        IR_parent(c) = ir;
        c = c->get_next();
    }
    return ir;
}


//Build IR_GOTO operation.
IR * Region::buildGoto(LabelInfo const* li)
{
    ASSERT0(li);
    IR * ir = allocIR(IR_GOTO);
    IR_dt(ir) = getTypeMgr()->getVoid();
    ASSERT0(li != NULL);
    GOTO_lab(ir) = li;
    return ir;
}


//Build IR_LD operation.
IR * Region::buildLoad(IN VAR * var)
{
    ASSERT0(var);
    return buildLoad(var, VAR_type(var));
}


//Build IR_LD operation.
//Load value from variable with type 'type'.
//'type': result value type.
IR * Region::buildLoad(VAR * var, Type const* type)
{
    ASSERT0(type);
    ASSERT0(var);
    IR * ir = allocIR(IR_LD);
    LD_idinfo(ir) = var;
    IR_dt(ir) = type;
    if (g_is_hoist_type) {
        //Hoisting I16/U16/I8/U8 to I32, to utilize whole register.
        DATA_TYPE dt = ir->get_dtype();
        if (IS_SIMPLEX(dt)) {
            //Hoist data-type from less than INT to INT.
            IR_dt(ir) =
                getTypeMgr()->getSimplexTypeEx(getTypeMgr()->hoistDtype(dt));
        }
    }
    return ir;
}


//Build IR_ILD operation.
//Result is either register or memory chunk, and the size of ILD
//result equals to 'pointer_base_size' of 'addr'.
//'base': memory address of ILD.
//'ptbase_or_mc_size': if result of ILD is pointer, this parameter records
//    pointer_base_size; or if result is memory chunk, it records
//    the size of memory chunk.
//NOTICE: The ofst of ILD requires to maintain when after return.
IR * Region::buildIload(IR * base, Type const* type)
{
    ASSERT0(type);
    ASSERT(base && base->is_ptr(), ("mem-address of ILD must be pointer"));
    IR * ir = allocIR(IR_ILD);
    IR_dt(ir) = type;
    ILD_base(ir) = base;
    IR_parent(base) = ir;
    return ir;
}


//Build IR_ILD operation.
IR * Region::buildIload(IR * base, UINT ofst, Type const* type)
{
    ASSERT0(type);
    IR * ir = buildIload(base, type);
    ILD_ofst(ir) = ofst;
    return ir;
}


//Build store operation to get value from 'base', and store the result PR.
//'prno': result prno.
//'type': data type of targe pr.
//'offset': byte offset to the start of PR.
//'base: hold the value that expected to extract.
IR * Region::buildGetElem(UINT prno, Type const* type, IR * base, IR * offset)
{
    ASSERT0(type && offset && base && prno > 0 && base->is_exp());
    IR * ir = allocIR(IR_GETELEM);
    GETELEM_prno(ir) = prno;
    GETELEM_base(ir) = base;
    GETELEM_ofst(ir) = offset;
    IR_dt(ir) = type;
    IR_parent(base) = ir;
    IR_parent(offset) = ir;
    return ir;
}


//Build store operation to get value from 'rhs', and store the result PR.
//'type': data type of targe pr.
//'offset': byte offset to the start of rhs PR.
//'base: hold the value that expected to extract.
IR * Region::buildGetElem(Type const* type, IR * base, IR * offset)
{
    ASSERT0(type && base && base->is_exp());
    IR * ir = buildGetElem(REGION_analysis_instrument(this)->m_pr_count,
        type, base, offset);
    REGION_analysis_instrument(this)->m_pr_count++;
    return ir;
}


//Build store operation to store 'rhs' to store value to be one of the
//element of a PR.
//'prno': target prno.
//'type': data type of targe pr.
//'offset': byte offset to the start of result PR.
//'rhs: value expected to store.
IR * Region::buildSetElem(UINT prno, Type const* type, IR * rhs, IR * offset)
{
    ASSERT0(type && offset && rhs && prno > 0 && rhs->is_exp());
    IR * ir = allocIR(IR_SETELEM);
    SETELEM_prno(ir) = prno;
    SETELEM_rhs(ir) = rhs;
    SETELEM_ofst(ir) = offset;
    IR_dt(ir) = type;
    IR_parent(rhs) = ir;
    IR_parent(offset) = ir;
    return ir;
}


//Build store operation to store 'rhs' to store value to be one of the
//element of a PR.
//'type': data type of targe pr.
//'offset': byte offset to the start of result PR.
//'rhs: value expected to store.
IR * Region::buildSetElem(Type const* type, IR * rhs, IR * offset)
{
    ASSERT0(type && rhs && rhs->is_exp());
    IR * ir = buildSetElem(REGION_analysis_instrument(this)->m_pr_count,
        type, rhs, offset);
    REGION_analysis_instrument(this)->m_pr_count++;
    return ir;
}


//Build store operation to store 'rhs' to new pr with type and prno.
//'prno': target prno.
//'type': data type of targe pr.
//'rhs: value expected to store.
IR * Region::buildStorePR(UINT prno, Type const* type, IR * rhs)
{
    ASSERT0(type && prno > 0 && rhs && rhs->is_exp());
    IR * ir = allocIR(IR_STPR);
    STPR_no(ir) = prno;
    STPR_rhs(ir) = rhs;
    IR_dt(ir) = type;
    IR_parent(rhs) = ir;
    return ir;
}


//Build store operation to store 'rhs' to new pr with type.
//'type': data type of targe pr.
//'rhs: value expected to store.
IR * Region::buildStorePR(Type const* type, IR * rhs)
{
    ASSERT0(type && rhs && rhs->is_exp());
    IR * ir = buildStorePR(REGION_analysis_instrument(this)->m_pr_count,
        type, rhs);
    REGION_analysis_instrument(this)->m_pr_count++;
    return ir;
}


//Build IR_ST operation.
//'lhs': memory variable, described target memory location.
//'rhs: value expected to store.
IR * Region::buildStore(VAR * lhs, IR * rhs)
{
    ASSERT0(lhs && rhs);
    return buildStore(lhs, VAR_type(lhs), rhs);
}


//Build IR_ST operation.
//'lhs': target memory location.
//'type: result data type.
//'rhs: value expected to store.
IR * Region::buildStore(VAR * lhs, Type const* type, IR * rhs)
{
    ASSERT0(type);
    ASSERT0(lhs && rhs && rhs->is_exp());
    IR * ir = allocIR(IR_ST);
    ST_idinfo(ir) = lhs;
    ST_rhs(ir) = rhs;
    IR_dt(ir) = type;
    IR_parent(rhs) = ir;
    return ir;
}


//Build IR_ST operation.
//'lhs': target memory location.
//'type: result data type.
//'ofst': memory byte offset relative to lhs.
//'rhs: value expected to store.
IR * Region::buildStore(VAR * lhs, Type const* type, UINT ofst, IR * rhs)
{
    ASSERT0(type);
    IR * ir = buildStore(lhs, type, rhs);
    ST_ofst(ir) = ofst;
    return ir;
}


//Build IR_IST operation.
IR * Region::buildIstore(IR * base, IR * rhs, UINT ofst, Type const* type)
{
    ASSERT0(type);
    IR * ir = buildIstore(base, rhs, type);
    IST_ofst(ir) = ofst;
    return ir;
}


//Build IR_IST operation.
//'lhs': target memory location pointer.
//'rhs: value expected to store.
//'type': result type of indirect memory operation, note type is not the
//data type of lhs.
IR * Region::buildIstore(IR * base, IR * rhs, Type const* type)
{
    ASSERT0(type);
    ASSERT0(base && rhs && rhs->is_exp());
    ASSERT(base->is_ptr(), ("must be pointer"));
    IR * ir = allocIR(IR_IST);
    IR_dt(ir) = type;
    IST_base(ir) = base;
    IST_rhs(ir) = rhs;
    IR_parent(base) = ir;
    IR_parent(rhs) = ir;
    return ir;
}


//Build IR_ARRAY operation.
//'base': base of array operation, it is either LDA or pointer.
//'sublist': subscript expression list.
//'type': result type of array operator.
//    Note that type may NOT be equal to elem_tyid, accroding to
//    ARR_ofst(). If ARR_ofst() is not zero, that means array
//    elem is MC, or VECTOR, and type should be type of member
//    to MC/VECTOR.
//    e.g: struct S{ int a,b,c,d;}
//        struct S pa[100];
//        If youe do access pa[1].c
//        type should be int rather than struct S.
//        and elem_tyid should be struct S.
//
//'elem_tyid': record element-data-type.
//    e.g:vector<int,8> g[100];
//        elem_size is sizeof(vector<int,8>) = 32
//        elem_type is vector.
//    e.g1: struct S{ int a,b,c,d;}
//        struct S * pa[100];
//        elem_size is sizeof(struct S *)
//        elem_type is PTR.
//    e.g2:
//        struct S pa[100];
//        elem_size is sizeof(struct S)
//        elem_type is struct S
//
//'dims': indicate the array dimension.
//'elem_num': point to an integer array that indicate
//    the number of element for each dimension. The length of the integer
//    array should be equal to 'dims'.
//    e.g: int g[12][24];
//        elem_num points to an array with 2 value, [12, 24].
//        the 1th dimension has 12 elements, and the 2th dimension has 24
//        elements, which element type is D_I32.
IR * Region::buildArray(
        IR * base,
        IR * sublist,
        Type const* type,
        Type const* elemtype,
        UINT dims,
        TMWORD const* elem_num_buf)
{
    ASSERT0(type);
    ASSERT0(base && sublist && elemtype);
    ASSERT0(base->is_exp() && base->is_ptr());
    CArray * ir = (CArray*)allocIR(IR_ARRAY);
    IR_dt(ir) = type;
    ARR_base(ir) = base;
    IR_parent(base) = ir;
    ARR_sub_list(ir) = sublist;
    UINT n = 0;
    for (IR * p = sublist; p != NULL; p = p->get_next()) {
        IR_parent(p) = ir;
        n++;
    }
    ASSERT0(n == dims);
    ARR_elemtype(ir) = elemtype;

    if (elem_num_buf != NULL) {
        UINT l = sizeof(TMWORD) * dims;
        TMWORD * ebuf = (TMWORD*)xmalloc(l);
        memcpy(ebuf, elem_num_buf, l);
        ARR_elem_num_buf(ir) = ebuf;
    }
    return ir;
}


//Build IR_STARRAY operation.
//'base': base of array operation, it is either LDA or pointer.
//'sublist': subscript expression list.
//'type': result type of array operator.
//    Note that type may NOT be equal to elem_tyid, accroding to
//    ARR_ofst(). If ARR_ofst() is not zero, that means array
//    elem is MC, or VECTOR, and type should be type of member
//    to MC/VECTOR.
//    e.g: struct S{ int a,b,c,d;}
//        struct S pa[100];
//        If youe do access pa[1].c
//        type should be int rather than struct S.
//        and elem_tyid should be struct S.
//
//'elem_tyid': record element-data-type.
//    e.g:vector<int,8> g[100];
//        elem_size is sizeof(vector<int,8>) = 32
//        elem_type is vector.
//    e.g1: struct S{ int a,b,c,d;}
//        struct S * pa[100];
//        elem_size is sizeof(struct S *)
//        elem_type is PTR.
//    e.g2:
//        struct S pa[100];
//        elem_size is sizeof(struct S)
//        elem_type is struct S
//
//'dims': indicate the array dimension.
//'elem_num': point to an integer array that indicate
//    the number of element for in dimension.
//    The length of the integer array should be equal to 'dims'.
//    e.g: int g[12][24];
//        elem_num points to an array with 2 value, [12, 24].
//        the 1th dimension has 12 elements, and the 2th dimension has 24
//        elements, which element type is D_I32.
//    Note the parameter may be NULL.
//'rhs: value expected to store.
IR * Region::buildStoreArray(
        IR * base,
        IR * sublist,
        Type const* type,
        Type const* elemtype,
        UINT dims,
        TMWORD const* elem_num_buf,
        IR * rhs)
{
    ASSERT0(base && sublist && type);
    ASSERT0(base->is_exp() && (base->is_ptr() || base->is_void()));
    ASSERT0(rhs && rhs->is_exp());
    ASSERT0(allBeExp(sublist));
    CStArray * ir = (CStArray*)allocIR(IR_STARRAY);
    IR_dt(ir) = type;
    ARR_base(ir) = base;
    IR_parent(base) = ir;
    ARR_sub_list(ir) = sublist;
    UINT n = 0;
    for (IR * p = sublist; p != NULL; p = p->get_next()) {
        IR_parent(p) = ir;
        n++;
    }
    ASSERT0(n == dims);
    ARR_elemtype(ir) = elemtype;

    if (elem_num_buf != NULL) {
        UINT l = sizeof(TMWORD) * dims;
        TMWORD * ebuf = (TMWORD*)xmalloc(l);
        memcpy(ebuf, elem_num_buf, l);
        ARR_elem_num_buf(ir) = ebuf;
    }
    STARR_rhs(ir) = rhs;
    IR_parent(rhs) = ir;
    return ir;
}



//Build IR_RETURN operation.
IR * Region::buildReturn(IR * retexp)
{
    IR * ir = allocIR(IR_RETURN);
    IR_dt(ir) = getTypeMgr()->getVoid();
    RET_exp(ir) = retexp;
    if (retexp != NULL) {
        ASSERT0(retexp->is_exp());
        ASSERT0(IR_next(retexp) == NULL);
        ASSERT0(IR_prev(retexp) == NULL);
        IR_parent(retexp) = ir;
    }
    return ir;
}


//Build IR_CONTINUE operation.
IR * Region::buildContinue()
{
    IR * ir = allocIR(IR_CONTINUE);
    IR_dt(ir) = getTypeMgr()->getVoid();
    return ir;
}


//Build IR_BREAK operation.
IR * Region::buildBreak()
{
    IR * ir = allocIR(IR_BREAK);
    IR_dt(ir) = getTypeMgr()->getVoid();
    return ir;
}


//Build IR_CASE operation.
IR * Region::buildCase(IR * casev_exp, LabelInfo const* jump_lab)
{
    ASSERT0(casev_exp && jump_lab);
    ASSERT(casev_exp->is_const(), ("case value-expression must be const"));
    IR * ir = allocIR(IR_CASE);
    IR_dt(ir) = getTypeMgr()->getVoid();
    CASE_lab(ir) = jump_lab;
    CASE_vexp(ir) = casev_exp;
    IR_parent(casev_exp) = ir;
    return ir;
}


//Build Do Loop stmt.
//'iv': induction variable.
//'det': determinate expression.
//'loop_body': stmt list.
//'init': record the stmt that initialize iv.
//'step': record the stmt that update iv.
IR * Region::buildDoLoop(
        IR * iv,
        IR * init,
        IR * det,
        IR * step,
        IR * loop_body)
{
    ASSERT0(det &&
            (det->is_lt() ||
             det->is_le() ||
             det->is_gt() ||
             det->is_ge()));
    ASSERT0(init && step && init->is_exp() && step->is_exp());
    ASSERT0(iv->is_id() || iv->is_pr());
    //ASSERT0(is_reduction(step));

    IR * ir = allocIR(IR_DO_LOOP);
    IR_dt(ir) = getTypeMgr()->getVoid();

    LOOP_iv(ir) = iv;
    IR_parent(iv) = ir;

    LOOP_det(ir) = det;
    IR_parent(det) = ir;

    LOOP_init(ir) = init;
    IR_parent(init) = ir;

    LOOP_step(ir) = step;
    IR_parent(step) = ir;

    LOOP_body(ir) = loop_body;
    IR * c = loop_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build Do While stmt.
//'det': determinate expression.
//'loop_body': stmt list.
IR * Region::buildDoWhile(IR * det, IR * loop_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = allocIR(IR_DO_WHILE);
    IR_dt(ir) = getTypeMgr()->getVoid();
    LOOP_det(ir) = det;
    IR_parent(det) = ir;

    LOOP_body(ir) = loop_body;
    IR * c = loop_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build While Do stmt.
//'det': determinate expression.
//'loop_body': stmt list.
IR * Region::buildWhileDo(IR * det, IR * loop_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = allocIR(IR_WHILE_DO);
    IR_dt(ir) = getTypeMgr()->getVoid();
    LOOP_det(ir) = det;
    IR_parent(det) = ir;

    LOOP_body(ir) = loop_body;
    IR * c = loop_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build IF stmt.
//'det': determinate expression.
//'true_body': stmt list.
//'false_body': stmt list.
IR * Region::buildIf(IR * det, IR * true_body, IR * false_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = allocIR(IR_IF);
    IR_dt(ir) = getTypeMgr()->getVoid();
    IF_det(ir) = det;
    IR_parent(det) = ir;

    IF_truebody(ir) = true_body;
    IR * c = true_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }

    IF_falsebody(ir) = false_body;
    c = false_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build SWITCH multi-select stmt.
//'vexp': expression to determine which case entry will be target.
//'case_list': case entry list. case entry is consist of expression and label.
//    Note that case list is optional.
//'body': stmt list.
//'default_lab': label indicates the default choice, the label is optional.
//
//NOTE: Do not set parent for stmt in 'body'.
IR * Region::buildSwitch(
        IR * vexp,
        IR * case_list,
        IR * body,
        LabelInfo const* default_lab)
{
    ASSERT0(vexp && vexp->is_exp());
    IR * ir = allocIR(IR_SWITCH);
    IR_dt(ir) = getTypeMgr()->getVoid();
    SWITCH_vexp(ir) = vexp;
    SWITCH_case_list(ir) = case_list;
    SWITCH_body(ir) = body;
    SWITCH_deflab(ir) = default_lab;
    IR_parent(vexp) = ir;

    IR * c = case_list;
    while (c != NULL) {
        ASSERT0(c->is_case());
        IR_parent(c) = ir;
        c = c->get_next();
    }

    c = body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build IR_TRUEBR or IR_FALSEBR operation.
IR * Region::buildBranch(bool is_true_br, IR * det, LabelInfo const* lab)
{
    ASSERT0(lab && det && det->is_judge());
    IR * ir;
    if (is_true_br) {
        ir = allocIR(IR_TRUEBR);
    } else {
        ir = allocIR(IR_FALSEBR);
    }
    IR_dt(ir) = getTypeMgr()->getVoid();
    BR_det(ir) = det;
    BR_lab(ir) = lab;
    IR_parent(det) = ir;
    return ir;
}


//Build IR_CONST operation.
//The expression indicates a float point number.
IR * Region::buildImmFp(HOST_FP fp, Type const* type)
{
    ASSERT0(type);
    IR * imm = allocIR(IR_CONST);
    //Convert string to hex value , that is in order to generate
    //single load instruction to load float point value in Code
    //Generator.
    ASSERT0(type->is_fp());
    CONST_fp_val(imm) = fp;
    IR_dt(imm) = type;
    return imm;
}


//Build IR_CONST operation.
//The expression indicates an integer.
//'v': value of integer.
//'type': integer type.
IR * Region::buildImmInt(HOST_INT v, Type const* type)
{
    ASSERT0(type);
    ASSERT0(type->is_int() || type->is_mc());
    IR * imm = allocIR(IR_CONST);
    if (type->is_int()) {
        //Make sure value is sign-extended.
        switch (TY_dtype(type)) {
        case D_B:
        case D_I8:
        case D_U8:
            {
                UINT8 uv = (UINT8)v;
                INT8 sv = (INT8)uv;
                CONST_int_val(imm) = (HOST_INT)sv;
            }
            break;
        case D_I16:
        case D_U16:
            {
                UINT16 uv = (UINT16)v;
                INT16 sv = (INT16)uv;
                CONST_int_val(imm) = (HOST_INT)sv;
            }
            break;
        case D_I32:
        case D_U32:
            {
                UINT32 uv = (UINT32)v;
                INT32 sv = (INT32)uv;
                CONST_int_val(imm) = (HOST_INT)sv;
            }
            break;
        case D_I64:
        case D_U64:
            {
                UINT64 uv = (UINT64)v;
                INT64 sv = (INT64)uv;
                CONST_int_val(imm) = (HOST_INT)sv;
            }
            break;
        case D_I128:
        case D_U128:
            ASSERT(0, ("TODO:unsupport 128 bit integer"));
            break;
        default: ASSERT(0, ("TODO:unsupport integer type"));
        }
    } else {
        CONST_int_val(imm) = v;
    }

    IR_dt(imm) = type;
    return imm;
}


//The function will check and build pointer arithmetic operation.
//To build pointer arithemtic, the addend of pointer must be
//product of the pointer-base-size and rchild if lchild is pointer.
IR * Region::buildPointerOp(IR_TYPE irt, IR * lchild, IR * rchild)
{
    ASSERT0(lchild && rchild);
    if (!lchild->is_ptr() && rchild->is_ptr()) {
        ASSERT(irt == IR_ADD ||
               irt == IR_MUL ||
               irt == IR_XOR ||
               irt == IR_BAND ||
               irt == IR_BOR ||
               irt == IR_LT ||
               irt == IR_GT ||
               irt == IR_LE ||
               irt == IR_GE ||
               irt == IR_EQ ||
               irt == IR_NE, ("illegal pointer operation"));
        ASSERT(lchild->is_int() || lchild->is_mc() || lchild->is_void(),
               ("illegal pointer addend"));
        return buildPointerOp(irt, rchild, lchild);
    }

    Type const* d0 = lchild->get_type();
    Type const* d1 = rchild->get_type();
    DUMMYUSE(d1);
    if (lchild->is_ptr() && rchild->is_ptr()) {
        //CASE: Pointer substraction.
        //  char *p, *q;
        //  p-q => t1=p-q, t2=t1/4, return t2
        switch (irt) {
        case IR_SUB:
            {
                TypeMgr * dm = getTypeMgr();

                //Result is not pointer type.
                ASSERT0(TY_ptr_base_size(d0) > 0);
                ASSERT0(TY_ptr_base_size(d0) == TY_ptr_base_size(d1));
                IR * ret = allocIR(IR_SUB);
                BIN_opnd0(ret) = lchild;
                BIN_opnd1(ret) = rchild;
                IR_dt(ret) = dm->getSimplexTypeEx(
                                    dm->get_dtype(WORD_BITSIZE, true));
                if (TY_ptr_base_size(d0) > BYTE_PER_CHAR) {
                    IR * div = allocIR(IR_DIV);
                    BIN_opnd0(div) = ret;
                    BIN_opnd1(div) = buildImmInt(TY_ptr_base_size(d0),
                                                 ret->get_type());
                    IR_dt(div) = dm->getSimplexTypeEx(
                                     dm->get_dtype(WORD_BITSIZE, true));
                    ret = div;
                }

                //Avoid too much boring pointer operations.
                ret->setParentPointer(true);
                return ret;
            }
        case IR_LT:
        case IR_LE:
        case IR_GT:
        case IR_GE:
        case IR_EQ:
        case IR_NE:
            {
                //Result is not pointer type.
                IR * ret = allocIR(irt);
                BIN_opnd0(ret) = lchild;
                BIN_opnd1(ret) = rchild;
                IR_dt(ret) = getTypeMgr()->getSimplexTypeEx(D_B);
                IR_parent(lchild) = ret;
                IR_parent(rchild) = ret;
                return ret;
            }
        default: ASSERT(0, ("illegal pointers operation"));
        }
        ASSERT(0, ("can not get here."));
    } else if (lchild->is_ptr() && !rchild->is_ptr()) {
        //Result is pointer type.
        //CASE:
        //  int * p;
        //  p + 4 => t1 = p + (4 * sizeof(BASE_TYPE_OF(p)))
        //  p - 4 => t1 = p - (4 * sizeof(BASE_TYPE_OF(p)))
        switch (irt) {
        case IR_ADD:
        case IR_SUB:
            {
                IR * addend = allocIR(IR_MUL);
                BIN_opnd0(addend) = rchild;

                ASSERT(TY_ptr_base_size(d0) > 0, ("multipler is 0"));

                BIN_opnd1(addend) = buildImmInt(TY_ptr_base_size(d0),
                                                rchild->get_type());
                IR_dt(addend) = rchild->get_type();

                IR * ret = allocIR(irt); //ADD or SUB
                BIN_opnd0(ret) = lchild; //lchild is pointer.
                BIN_opnd1(ret) = addend; //addend is not pointer.

                //CASE: 'p = p + 1'
                //so the result type of '+' should still be pointer type.
                ret->setPointerType(TY_ptr_base_size(d0), getTypeMgr());

                //Avoid too much boring pointer operations.
                ret->setParentPointer(true);
                return ret;
            }
        default:
            {
                ASSERT0(irt == IR_LT || irt == IR_LE ||
                        irt == IR_GT || irt == IR_GE ||
                        irt == IR_EQ || irt == IR_NE);

                //Pointer operation may give rise to undefined behavior.
                IR * ret = allocIR(irt);
                BIN_opnd0(ret) = lchild;
                BIN_opnd1(ret) = rchild;
                IR_dt(ret) = getTypeMgr()->getSimplexTypeEx(D_B);
                IR_parent(lchild) = ret;
                IR_parent(rchild) = ret;
                return ret;
            }
        }
    }

    UNREACH();
    return NULL; //just ceases warning.
}


//This function build operation that comparing with 0 by NE node.
//e.g: output is (exp != 0).
//This function always used as helper function to convient to
//generate det-expression if it is not relational/logical.
IR * Region::buildJudge(IR * exp)
{
    ASSERT0(!exp->is_judge());
    Type const* type = exp->get_type();
    TypeMgr * dm = getTypeMgr();
    if (exp->is_ptr()) {
        type = dm->getSimplexTypeEx(dm->getPointerSizeDtype());
    }

    if (!type->is_fp() && !type->is_int() && !type->is_mc()) {
        type = dm->getI32();
    }

    return buildCmp(IR_NE, exp, type->is_fp() ?
        buildImmFp(HOST_FP(0), type) : buildImmInt(0, type));
}


//Build comparision operations.
IR * Region::buildCmp(IR_TYPE irt, IR * lchild, IR * rchild)
{
    ASSERT0(irt == IR_LAND || irt == IR_LOR ||
            irt == IR_LT || irt == IR_LE ||
            irt == IR_GT || irt == IR_GE ||
            irt == IR_NE || irt == IR_EQ);
    ASSERT0(lchild && rchild && lchild->is_exp() && rchild->is_exp());

    if (lchild->is_const() &&
        !rchild->is_const() &&
        (irt == IR_EQ || irt == IR_NE)) {
        return buildCmp(irt, rchild, lchild);
    }

    IR * ir = allocIR(irt);
    BIN_opnd0(ir) = lchild;
    BIN_opnd1(ir) = rchild;
    IR_dt(ir) = getTypeMgr()->getSimplexTypeEx(D_B);
    IR_parent(lchild) = ir;
    IR_parent(rchild) = ir;
    return ir;
}


IR * Region::buildUnaryOp(IR_TYPE irt, Type const* type, IN IR * opnd)
{
    ASSERT0(type);
    ASSERT0(isUnaryOp(irt));
    ASSERT0(opnd && opnd->is_exp());
    ASSERT0(irt != IR_LNOT || type->is_bool());
    IR * ir = allocIR(irt);
    UNA_opnd(ir) = opnd;
    IR_dt(ir) = type;
    IR_parent(opnd) = ir;
    return ir;
}


//Build binary operation without considering pointer arithmetic.
IR * Region::buildBinaryOpSimp(
        IR_TYPE irt,
        Type const* type,
        IR * lchild,
        IR * rchild)
{
    ASSERT0(type);

    if (lchild->is_const() && !rchild->is_const() &&
        (irt == IR_ADD ||
         irt == IR_MUL ||
         irt == IR_XOR ||
         irt == IR_BAND ||
         irt == IR_BOR ||
         irt == IR_EQ ||
         irt == IR_NE)) {
        //Swap operands.
        return buildBinaryOpSimp(irt, type, rchild, lchild);
    }

    ASSERT0(lchild && rchild && lchild->is_exp() && rchild->is_exp());
    ASSERT0(checkLogicalOp(irt, type, getTypeMgr()));
    IR * ir = allocIR(irt);
    BIN_opnd0(ir) = lchild;
    BIN_opnd1(ir) = rchild;
    IR_parent(lchild) = ir;
    IR_parent(rchild) = ir;
    IR_dt(ir) = type;
    return ir;
}


//Build binary operation.
//If rchild/lchild is pointer, the function will attemp to generate pointer
//arithmetic operation instead of normal binary operation.
IR * Region::buildBinaryOp(
        IR_TYPE irt,
        Type const* type,
        IR * lchild,
        IR * rchild)
{
    ASSERT0(type);
    ASSERT0(checkLogicalOp(irt, type, getTypeMgr()));
    ASSERT0(lchild && rchild && lchild->is_exp() && rchild->is_exp());
    if (lchild->is_ptr() || rchild->is_ptr()) {
        return buildPointerOp(irt, lchild, rchild);
    }

    #ifdef _DEBUG_
    //Both lchild and rchild are NOT pointer.
    //Generic binary operation.
    if (type->is_mc()) {
        //mc_size records the memory-chunk size if rtype is D_MC, or else is 0.
        ASSERT(TY_mc_size(type) != 0, ("Size of memory chunck can not be 0"));
        ASSERT0(TY_mc_size(type) == lchild->get_type_size(getTypeMgr()) &&
                TY_mc_size(type) == rchild->get_type_size(getTypeMgr()));
    }
    #endif

    return buildBinaryOpSimp(irt, type, lchild, rchild);
}


//Split list of ir into basic block.
//'irs': a list of ir.
//'bbl': a list of bb.
//'ctbb': marker current bb container.
C<IRBB*> * Region::splitIRlistIntoBB(IR * irs, BBList * bbl, C<IRBB*> * ctbb)
{
    IR_CFG * cfg = getCFG();
    ASSERT(cfg, ("CFG is not available"));

    IRBB * newbb = allocBB();
    cfg->add_bb(newbb);
    ctbb = bbl->insert_after(newbb, ctbb);
    LAB2BB * lab2bb = cfg->get_lab2bb_map();

    while (irs != NULL) {
        IR * ir = xcom::removehead(&irs);
        if (newbb->is_down_boundary(ir)) {
            BB_irlist(newbb).append_tail(ir);
            newbb = allocBB();
            cfg->add_bb(newbb);
            ctbb = bbl->insert_after(newbb, ctbb);
        } else if (newbb->is_up_boundary(ir)) {
            ASSERT0(ir->is_label());

            newbb = allocBB();
            cfg->add_bb(newbb);
            ctbb = bbl->insert_after(newbb, ctbb);

            //Regard label-info as add-on info that attached on newbb, and
            //'ir' will be dropped off.
            LabelInfo const* li = ir->getLabel();
            newbb->addLabel(li);
            lab2bb->set(li, newbb);
            if (!LABEL_INFO_is_try_start(li) && !LABEL_INFO_is_pragma(li)) {
                BB_is_target(newbb) = true;
            }
            freeIRTree(ir); //free label ir.
        } else {
            BB_irlist(newbb).append_tail(ir);
        }
    }
    return ctbb;
}


bool Region::evaluateConstInteger(IR const* ir, OUT ULONGLONG * const_value)
{
    switch (ir->get_code()) {
    case IR_CONST:
        if (!ir->is_int()) { return false; }
        *const_value = CONST_int_val(ir);
        return true;
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND: //logical and &&
    case IR_LOR: //logical or ||
    case IR_BAND: //inclusive and &
    case IR_BOR: //inclusive or |
    case IR_XOR: //exclusive or
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        {
            IR const* opnd0 = BIN_opnd0(ir);
            IR const* opnd1 = BIN_opnd1(ir);

            //TODO: Handle the case if opnd0's type is different with opnd1.
            if (!opnd0->is_int() || !opnd1->is_int()) { return false; }
            if (opnd0->is_uint() ^ opnd1->is_uint()) { return false; }

            ULONGLONG lvalue = 0, rvalue = 0;
            if (!evaluateConstInteger(BIN_opnd0(ir), &lvalue)) { return false; }
            if (!evaluateConstInteger(BIN_opnd1(ir), &rvalue)) { return false; }

            if (opnd0->is_uint()) {
                switch (ir->get_code()) {
                case IR_ADD: *const_value = lvalue + rvalue; break;
                case IR_MUL: *const_value = lvalue * rvalue; break;
                case IR_SUB: *const_value = lvalue - rvalue; break;
                case IR_DIV: *const_value = lvalue / rvalue; break;
                case IR_REM: *const_value = lvalue % rvalue; break;
                case IR_MOD: *const_value = lvalue % rvalue; break;
                case IR_LAND: *const_value = lvalue && rvalue; break;
                case IR_LOR:  *const_value = lvalue || rvalue; break;
                case IR_BAND: *const_value = lvalue & rvalue; break;
                case IR_BOR:  *const_value = lvalue | rvalue; break;
                case IR_XOR:  *const_value = lvalue ^ rvalue; break;
                case IR_LT: *const_value= lvalue < rvalue; break;
                case IR_LE: *const_value= lvalue <= rvalue; break;
                case IR_GT: *const_value= lvalue > rvalue; break;
                case IR_GE: *const_value= lvalue >= rvalue; break;
                case IR_EQ: *const_value= lvalue == rvalue; break;
                case IR_NE: *const_value= lvalue != rvalue; break;
                case IR_ASR: *const_value = lvalue >> rvalue; break;
                case IR_LSR: *const_value = lvalue >> rvalue; break;
                case IR_LSL: *const_value = lvalue << rvalue; break;
                default: return false;
                }
            } else {
                LONGLONG lv = (LONGLONG)lvalue;
                LONGLONG rv = (LONGLONG)rvalue;
                LONGLONG res = 0;
                switch (ir->get_code()) {
                case IR_ADD:  res = lv + rv; break;
                case IR_SUB:  res = lv - rv; break;
                case IR_MUL:  res = lv * rv; break;
                case IR_DIV:  res = lv / rv; break;
                case IR_REM:  res = lv % rv; break;
                case IR_MOD:  res = lv % rv; break;
                case IR_LAND: res = lv && rv; break;
                case IR_LOR:  res = lv || rv; break;
                case IR_BAND: res = lv & rv; break;
                case IR_BOR:  res = lv | rv; break;
                case IR_XOR:  res = lv ^ rv; break;
                case IR_LT: res = lv < rv; break;
                case IR_LE: res = lv <= rv; break;
                case IR_GT: res = lv > rv; break;
                case IR_GE: res = lv >= rv; break;
                case IR_EQ: res = lv == rv; break;
                case IR_NE: res = lv != rv; break;
                case IR_ASR: res = lv >> rv; break;
                case IR_LSR: res = lv >> rv; break;
                case IR_LSL: res = lv << rv; break;
                default: return false;
                }
                *const_value = (ULONGLONG)res;
            }
            return true;
        }
    case IR_BNOT: //bitwise not
    case IR_LNOT: //logical not
    case IR_NEG: //negative
        {
            if (!UNA_opnd(ir)->is_int()) { return false; }

            ULONGLONG value = 0;
            if (!evaluateConstInteger(UNA_opnd(ir), &value)) { return false; }

            switch (ir->get_code()) {
            case IR_BNOT: *const_value = ~value; break;
            case IR_LNOT: *const_value = !value; break;
            case IR_NEG:  *const_value = (ULONGLONG)(-(LONGLONG)value); break;
            default: return false;
            }
            return true;
        }
    case IR_PR:
        {
            IR * defstmt = NULL;
            SSAInfo const* ssainfo = PR_ssainfo(ir);
            if (ssainfo != NULL) {
                defstmt = SSA_def(ssainfo);
                if (defstmt != NULL && !defstmt->is_stpr()) {
                    return false;
                }
            } else {
                DUSet const* defset = ir->readDUSet();
                if (defset == NULL || defset->get_elem_count() != 1) {
                    return false;
                }

                DUIter di = NULL;
                defstmt = getIR(defset->get_first(&di));
                ASSERT0(defstmt && defstmt->is_stmt());

                if (!defstmt->is_stpr()) { return false; }
            }
            return evaluateConstInteger(STPR_rhs(defstmt), const_value);
        }
    case IR_CVT:
        return evaluateConstInteger(CVT_exp(ir), const_value);
    default:;
    }
    return false;
}


//Register gloval variable located in program region.
void Region::registerGlobalVAR()
{
    MD const * common_string_var_md = getRegionMgr()->genDedicateStrMD();
    if (common_string_var_md != NULL) {
        ASSERT0(is_program());
        addToVarTab(common_string_var_md->get_base());
    }
}


//Find the boundary IR generated in BB to update bb-list incremently.
//e.g: Given BB1 has one stmt:
//    BB1:
//    a = b||c
//after some simplification, we get:
//    truebr(b != 0), L1
//    truebr(c != 0), L1
//    pr = 0
//    goto L2
//    L1:
//    pr = 1
//    L2:
//    a = pr
//
//where IR list should be splitted into :
//    BB1:
//    truebr(b != 0), L1
//    BB2:
//    truebr(c != 0), L1
//    BB3:
//    pr = 0
//    goto L2
//    BB4:
//    L1:
//    pr = 1
//    BB5:
//    L2:
//    a = pr
bool Region::reconstructBBlist(OptCtx & oc)
{
    START_TIMER(t, "Reconstruct IRBB list");
    ASSERT(getCFG(), ("CFG is not available"));

    bool change = false;
    C<IRBB*> * ctbb;
    BBList * bbl = getBBList();
    for (bbl->get_head(&ctbb); ctbb != NULL; bbl->get_next(&ctbb)) {
        IRBB * bb = ctbb->val();
        C<IR*> * ctir;
        BBIRList * irlst = &BB_irlist(bb);

        IR * tail = irlst->get_tail();
        for (irlst->get_head(&ctir); ctir != NULL; irlst->get_next(&ctir)) {
            IR * ir = ctir->val();

            if (bb->is_down_boundary(ir) && ir != tail) {
                change = true;

                IR * restirs = NULL; //record rest part in bb list after 'ir'.
                IR * last = NULL;
                irlst->get_next(&ctir);

                for (C<IR*> * next_ctir = ctir;
                     ctir != NULL; ctir = next_ctir) {
                    irlst->get_next(&next_ctir);
                    irlst->remove(ctir);
                    add_next(&restirs, &last, ctir->val());
                }

                ctbb = splitIRlistIntoBB(restirs, bbl, ctbb);
                break;
            } else if (bb->is_up_boundary(ir)) {
                ASSERT0(ir->is_label());

                change = true;
                BB_is_fallthrough(bb) = true;

                IR * restirs = NULL; //record rest part in bb list after 'ir'.
                IR * last = NULL;

                for (C<IR*> * next_ctir = ctir;
                     ctir != NULL; ctir = next_ctir) {
                    irlst->get_next(&next_ctir);
                    irlst->remove(ctir);
                    add_next(&restirs, &last, ctir->val());
                }

                ctbb = splitIRlistIntoBB(restirs, bbl, ctbb);
                break;
            }
        }
    }

    END_TIMER(t, "Reconstruct IRBB list");

    if (change) {
        //Must rebuild CFG and all other structures which are
        //closely related to CFG.
        oc.set_flag_if_cfg_changed();
    }
    return change;
}


//Construct IR list from IRBB list.
//clean_ir_list: clean bb's ir list if it is true.
IR * Region::constructIRlist(bool clean_ir_list)
{
    START_TIMER(t, "Construct IR list from BB");
    IR * ret_list = NULL;
    IR * last = NULL;
    C<IRBB*> * ct;
    for (getBBList()->get_head(&ct);
         ct != getBBList()->end();
         ct = getBBList()->get_next(ct)) {
        IRBB * bb = ct->val();
        C<LabelInfo const*> * lct;
        for (bb->getLabelList().get_head(&lct);
             lct != bb->getLabelList().end();
             lct = bb->getLabelList().get_next(lct)) {
            LabelInfo const* li = lct->val();
            //insertbefore_one(&ret_list, ret_list, buildLabel(li));
            add_next(&ret_list, &last, buildLabel(li));
        }

        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            //insertbefore_one(&ret_list, ret_list, ir);
            add_next(&ret_list, &last, ir);
            if (clean_ir_list) {
                ir->setBB(NULL);
            }
        }
        if (clean_ir_list) {
            BB_irlist(bb).clean();
        }
    }

    //ret_list = reverse_list(ret_list);
    END_TIMER(t, "Construct IR list from BB");
    return ret_list;
}


//1. Split list of IRs into basic-block list.
//2. Set BB propeties. e.g: entry-bb, exit-bb.
void Region::constructIRBBlist()
{
    if (getIRList() == NULL) { return; }
    START_TIMER(t, "Construct IRBB list");
    IRBB * cur_bb = NULL;
    IR * pointer = getIRList();
    while (pointer != NULL) {
        if (cur_bb == NULL) {
            cur_bb = allocBB();
        }

        //Insert IR into individual BB.
        ASSERT0(pointer->isStmtInBB() || pointer->is_lab());
        IR * cur_ir = pointer;
        pointer = IR_next(pointer);
        IR_next(cur_ir) = IR_prev(cur_ir) = NULL;
        //remove(&start_ir, cur_ir);

        if (cur_bb->is_down_boundary(cur_ir)) {
            BB_irlist(cur_bb).append_tail(cur_ir);
            switch (IR_code(cur_ir)) {
            case IR_CALL:
            case IR_ICALL: //indirective call
            case IR_TRUEBR:
            case IR_FALSEBR:
            case IR_SWITCH:
                BB_is_fallthrough(cur_bb) = true;
                break;
            case IR_IGOTO:
            case IR_GOTO:
                //We have no knowledge about whether target BB of GOTO/IGOTO
                //will be followed subsequently on current BB.
                //Leave this problem to CFG builder, and the related
                //attribute should be set at that time.
                break;
            case IR_RETURN:
                //Succeed stmt of 'ir' may be DEAD code
                //IR_BB_is_func_exit(cur_bb) = true;
                BB_is_fallthrough(cur_bb) = true;
                break;
            case IR_REGION:
                BB_is_fallthrough(cur_bb) = true;
                break;
            default: ASSERT(0, ("invalid bb down-boundary IR"));
            } //end switch

            //Generate new BB.
            getBBList()->append_tail(cur_bb);
            cur_bb = allocBB();
        } else if (cur_ir->is_label()) {
            BB_is_fallthrough(cur_bb) = true;
            getBBList()->append_tail(cur_bb);

            //Generate new BB.
            cur_bb = allocBB();

            //label info be seen as add-on info attached on bb, and
            //'ir' be dropped off.
            bool not_merge_label = true;
            for (;;) {
                cur_bb->addLabel(LAB_lab(cur_ir));
                freeIR(cur_ir);
                if (not_merge_label) {
                    break;
                }
                if (pointer != NULL && pointer->is_label()) {
                    cur_ir = pointer;
                    pointer = IR_next(pointer);
                    IR_next(cur_ir) = IR_prev(cur_ir) = NULL;
                } else {
                    break;
                }
            }

            BB_is_target(cur_bb) = true;
        } else {
            //Note that PHI should be placed followed after a LABEL immediately.
            //That is a invalid phi if it has only one operand.
            BB_irlist(cur_bb).append_tail(cur_ir);
        }
    } //end while

    ASSERT0(cur_bb != NULL);
    getBBList()->append_tail(cur_bb);

    //cur_bb is the last bb, it is also the exit bb.
    //IR_BB_is_func_exit(cur_bb) = true;
    END_TIMER(t, "Construct IRBB list");
}


//Do general check for safe optimizing.
// CASE 1:
//     Do NOT optimize the code if meet the VIOLATE variable!
//     e.g1:
//         volatile int x, y;
//         int a[SIZE];
//         void f(void)
//         {
//           for (int i = 0; i < SIZE; i++)
//             a[i] = x + y;
//         }
//
//         Some compilers will hoist the expression (x + y)
//         out of the loop as shown below, which is an
//         incorrect optimization.
//     e.g2:
//         ## Incorrect Definiation! x,y are NOT const.
//         const violate int x,y;
bool Region::isSafeToOptimize(IR const* ir)
{
    if (ir->is_volatile()) {
        return false;
    }

    //Check kids.
    for(INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != NULL) {
            if (!isSafeToOptimize(kid)) {
                return false;
            }
        }
    }
    return true;
}


//Return true if VAR belongs to current region.
bool Region::isRegionVAR(VAR const* var)
{
    return getVarTab()->find(const_cast<VAR*>(var));
}


bool Region::isRegionIR(IR const* ir)
{
    Vector<IR*> * vec = getIRVec();
    for (INT i = 0; i <= vec->get_last_idx(); i++) {
        if (ir == vec->get(i)) { return true; }
    }
    return false;
}


//Generate VAR corresponding to PR load or write.
VAR * Region::genVARforPR(UINT prno, Type const* type)
{
    ASSERT0(type);
    VAR * pr_var = mapPR2Var(prno);
    if (pr_var != NULL) { return pr_var; }

    //Create a new PR VAR.
    CHAR name[128];
    sprintf(name, "pr%d", prno);
    ASSERT0(strlen(name) < 128);
    UINT flag = VAR_LOCAL;
    SET_FLAG(flag, VAR_IS_PR);
    pr_var = getVarMgr()->registerVar(name, type, 0, flag);
    setMapPR2Var(prno, pr_var);

    //Set the pr-var to be unallocable, means do NOT add
    //pr-var immediately as a memory-variable.
    //For now, it is only be regarded as a pseduo-register.
    //And set it to allocable if the PR is in essence need to be
    //allocated in memory.
    VAR_allocable(pr_var) = false;
    addToVarTab(pr_var);
    return pr_var;
}


//Generate MD corresponding to PR load or write.
MD const* Region::genMDforPR(UINT prno, Type const* type)
{
    ASSERT0(type);
    VAR * pr_var = mapPR2Var(prno);
    if (pr_var == NULL) {
        pr_var = genVARforPR(prno, type);
    }

    MD md;
    MD_base(&md) = pr_var; //correspond to VAR
    MD_ofst(&md) = 0;
    if (pr_var->get_type()->is_void()) {
        MD_ty(&md) = MD_UNBOUND;
    } else {
        MD_ty(&md) = MD_EXACT;
        MD_size(&md) = getTypeMgr()->get_bytesize(pr_var->get_type());
    }
    MD const* e = getMDSystem()->registerMD(md);
    ASSERT0(MD_id(e) > 0);
    return e;
}


//Get function unit.
Region * Region::getFuncRegion()
{
    Region * rg = this;
    while (!rg->is_function()) { rg = REGION_parent(rg); }
    ASSERT(rg != NULL, ("Not in func unit"));
    return rg;
}


CHAR const* Region::getRegionName() const
{
    if (getRegionVar() != NULL) {
        ASSERT0(getRegionVar()->get_name());
        return SYM_name(getRegionVar()->get_name());
    }

    //Miss region variable.
    return NULL;
}


//Free ir, ir's sibling, and all its kids.
//We can only utilizing the function to free the IR
//which allocated by 'allocIR'.
//
//NOTICE: If ir's sibling is not NULL, that means the IR is
//a high level type. IRBB consists of only middle/low level IR.
void Region::freeIRTreeList(IR * ir)
{
    if (ir == NULL) { return; }
    IR * head = ir, * next = NULL;
    while (ir != NULL) {
        next = ir->get_next();
        remove(&head, ir);
        freeIRTree(ir);
        ir = next;
    }
}


//Free ir, and all its kids.
//We can only utilizing the function to free the IR which allocated by 'allocIR'.
void Region::freeIRTreeList(IRList & irs)
{
    C<IR*> * next;
    C<IR*> * ct;
    for (irs.get_head(&ct); ct != irs.end(); ct = next) {
        IR * ir = ct->val();
        next = irs.get_next(ct);
        ASSERT(ir->is_single(), ("do not allow sibling node, need to simplify"));
        irs.remove(ir);
        freeIRTree(ir);
    }
}


//Free ir and all its kids, except its sibling node.
//We can only utilizing the function to free the
//IR which allocated by 'allocIR'.
void Region::freeIRTree(IR * ir)
{
    if (ir == NULL) { return; }

    ASSERT(!ir->is_undef(), ("ir has been freed"));
    ASSERT(ir->is_single(), ("chain list should be cut off"));
    for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != NULL) {
            freeIRTreeList(kid);
        }
    }
    freeIR(ir);
}


//This function erases all informations of ir and
//append it into free_list for next allocation.
//If Attach Info exist, this function will erase it rather than delete.
//If DU info exist, this function will retrieve it back
//to region for next allocation.
//Note that this function does NOT free ir's kids and siblings.
void Region::freeIR(IR * ir)
{
    ASSERT0(ir);
    ASSERT(ir->is_single(), ("chain list should be cut off"));
    #ifdef _DEBUG_
    ASSERT0(!REGION_analysis_instrument(this)->
            m_has_been_freed_irs.is_contain(ir->id()));
    REGION_analysis_instrument(this)->m_has_been_freed_irs.bunion(ir->id());
    #endif

    ASSERT0(getMiscBitSetMgr());
    ir->freeDUset(*getMiscBitSetMgr());

    AIContainer * res_ai = IR_ai(ir);
    if (res_ai != NULL) {
        //AICont will be reinitialized while setting.
        res_ai->destroy();
    }

    DU * du = ir->cleanDU();
    if (du != NULL) {
        DU_md(du) = NULL;
        DU_mds(du) = NULL;
        ASSERT0(du->has_clean());
        REGION_analysis_instrument(this)->m_free_du_list.append_head(du);
    }

    //Zero clearing all data fields.
    UINT res_id = ir->id();
    UINT res_irt_sz = getIRTypeSize(ir);
    memset(ir, 0, res_irt_sz);
    IR_id(ir) = res_id;
    IR_ai(ir) = res_ai;
    set_irt_size(ir, res_irt_sz);

    UINT idx = res_irt_sz - sizeof(IR);
    IR * head = REGION_analysis_instrument(this)->m_free_tab[idx];
    if (head != NULL) {
        IR_next(ir) = head;
        IR_prev(head) = ir;
    }
    REGION_analysis_instrument(this)->m_free_tab[idx] = ir;
}


//This function find VAR via iterating VAR table of current region.
VAR * Region::findVarViaSymbol(SYM const* sym)
{
    ASSERT0(sym);
    VarTab * vtab = getVarTab();
    VarTabIter c;
    for (VAR * v = vtab->get_first(c); v != NULL; v = vtab->get_next(c)) {
        if (v->get_name() == sym) {
            return v;
        }
    }
    return NULL;
}


//This function iterate VAR table of current region to
//find all VAR which are formal parameter.
//in_decl_order: if it is true, this function will sort the formal
//parameters in the Left to Right order according to their declaration.
//e.g: foo(a, b, c), varlst will be {a, b, c}.
void Region::findFormalParam(OUT List<VAR const*> & varlst, bool in_decl_order)
{
    VarTabIter c;
    VarTab * vt = getVarTab();
    ASSERT0(vt);

    if (in_decl_order)  {
        for (VAR const* v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
            if (!VAR_is_formal_param(v)) { continue; }

            C<VAR const*> * ctp;
            bool find = false;
            for (VAR const* p = varlst.get_head(&ctp);
                 p != NULL; p = varlst.get_next(&ctp)) {
                if (v->getFormalParamPos() < p->getFormalParamPos()) {
                    varlst.insert_before(v, ctp);
                    find = true;
                    break;
                }
            }

            if (!find) {
                varlst.append_tail(v);
            }
        }
        return;
    }

    //Unordered
    for (VAR const* v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
        if (VAR_is_formal_param(v)) {
            varlst.append_tail(v);
        }
    }
}


//Allocate PassMgr
PassMgr * Region::allocPassMgr()
{
    return new PassMgr(this);
}


//Allocate AliasAnalysis.
IR_AA * Region::allocAliasAnalysis()
{
    return new IR_AA(this);
}


void Region::dumpFreeTab()
{
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n==-- DUMP Region Free Table --==");
    for (UINT i = 0; i <= MAX_OFFSET_AT_FREE_TABLE; i++) {
        IR * lst = REGION_analysis_instrument(this)->m_free_tab[i];
        if (lst == NULL) { continue; }

        UINT sz = i + sizeof(IR);

        UINT count = 0;
        for (IR * ir = lst; ir != NULL; ir = ir->get_next()) {
            count++;
        }

        fprintf(g_tfile, "\nirsize(%d), num(%d):", sz, count);

        for (IR * ir = lst; ir != NULL; ir = ir->get_next()) {
            ASSERT0(getIRTypeSize(ir) == sz);
            fprintf(g_tfile, "ir(%d),", ir->id());
        }
    }
    fflush(g_tfile);
}


//Generate IR, invoke freeIR() or freeIRTree() if it is useless.
//NOTE: Do NOT invoke ::free() to free IR, because all
//    IR are allocated in the pool.
IR * Region::allocIR(IR_TYPE irt)
{
    IR * ir = NULL;
    UINT idx = IRTSIZE(irt) - sizeof(IR);
    bool lookup = false; //lookup freetab will save more memory, but slower.

    #ifndef CONST_IRT_SZ
    //If one is going to lookup freetab, IR_irt_size() must be defined.
    ASSERT0(!lookup);
    #endif

    if (lookup) {
        for (; idx <= MAX_OFFSET_AT_FREE_TABLE; idx++) {
            ir = REGION_analysis_instrument(this)->m_free_tab[idx];
            if (ir == NULL) { continue; }

            REGION_analysis_instrument(this)->m_free_tab[idx] = ir->get_next();
            if (ir->get_next() != NULL) {
                IR_prev(ir->get_next()) = NULL;
            }
            break;
        }
    } else {
        ir = REGION_analysis_instrument(this)->m_free_tab[idx];
        if (ir != NULL) {
            REGION_analysis_instrument(this)->m_free_tab[idx] = ir->get_next();
            if (ir->get_next() != NULL) {
                IR_prev(ir->get_next()) = NULL;
            }
        }
    }

    if (ir == NULL) {
        ir = (IR*)xmalloc(IRTSIZE(irt));
        INT v = MAX(getIRVec()->get_last_idx(), 0);
        IR_id(ir) = (UINT)(v+1);
        getIRVec()->set(ir->id(), ir);
        set_irt_size(ir, IRTSIZE(irt));
    } else {
        ASSERT0(ir->get_prev() == NULL);
        IR_next(ir) = NULL;
        #ifdef _DEBUG_
        REGION_analysis_instrument(this)->m_has_been_freed_irs.diff(ir->id());
        #endif
    }
    IR_code(ir) = irt;
    return ir;
}


//Duplication 'ir' and kids, and its sibling, return list of new ir.
//Duplicate irs start from 'ir' to the end of list.
IR * Region::dupIRTreeList(IR const* ir)
{
    IR * new_list = NULL;
    while (ir != NULL) {
        IR * newir = dupIRTree(ir);
        add_next(&new_list, newir);
        ir = ir->get_next();
    }
    return new_list;
}

//Duplicate 'ir' and its kids, but without ir's sibiling node.
IR * Region::dupIRTree(IR const* ir)
{
    if (ir == NULL) { return NULL; }
    IR * newir = dupIR(ir);
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != NULL) {
            IR * newkid_list = dupIRTreeList(kid);
            newir->setKid(i, newkid_list);
        } else { ASSERT0(newir->getKid(i) == NULL); }
    }
    return newir;
}


//Duplication all contents of 'src', includes AI, except DU info,
//SSA info, kids and siblings IR.
IR * Region::dupIR(IR const* src)
{
    if (src == NULL) { return NULL; }
    IR_TYPE irt = src->get_code();
    IR * res = allocIR(irt);
    ASSERT(res != NULL && src != NULL, ("res/src is NULL"));

    UINT res_id = IR_id(res);
    AIContainer * res_ai = IR_ai(res);
    UINT res_irt_sz = getIRTypeSize(res);
    memcpy(res, src, IRTSIZE(irt));
    IR_id(res) = res_id;
    IR_ai(res) = res_ai;
    set_irt_size(res, res_irt_sz);
    IR_next(res) = IR_prev(res) = IR_parent(res) = NULL;
    res->cleanDU(); //Do not copy DU info.
    res->clearSSAInfo(); //Do not copy SSA info.
    if (IR_ai(src) != NULL) { //need to copy AIContainer.
        if (IR_ai(res) == NULL) {
            IR_ai(res) = allocAIContainer();
        }
        IR_ai(res)->copy(IR_ai(src));
    }
    return res;
}

void Region::scanCallListImpl(
        OUT UINT & num_inner_region,
        IR * irlst,
        OUT List<IR const*> * call_list,
        OUT List<IR const*> * ret_list,
        bool scan_inner_region)
{
    for (IR const* t = irlst; t != NULL; t = t->get_next()) {
        switch (t->get_code()) {
        case IR_CALL:
        case IR_ICALL:
            if (!CALL_is_intrinsic(t)) {
                ASSERT0(call_list);
                call_list->append_tail(t);
            }
            break;
        case IR_DO_WHILE:
        case IR_WHILE_DO:
            scanCallListImpl(num_inner_region, LOOP_body(t),
                             call_list, ret_list, scan_inner_region);
            break;
        case IR_DO_LOOP:
            scanCallListImpl(num_inner_region, LOOP_init(t),
                             call_list, ret_list, scan_inner_region);
            scanCallListImpl(num_inner_region, LOOP_step(t), call_list,
                             ret_list, scan_inner_region);
            scanCallListImpl(num_inner_region, LOOP_body(t), call_list,
                             ret_list, scan_inner_region);
            break;
        case IR_IF:
            scanCallListImpl(num_inner_region, IF_truebody(t), call_list,
                             ret_list, scan_inner_region);
            scanCallListImpl(num_inner_region, IF_falsebody(t), call_list,
                             ret_list, scan_inner_region);
            break;
        case IR_SWITCH:
            scanCallListImpl(num_inner_region, SWITCH_body(t), call_list,
                             ret_list, scan_inner_region);
            break;
        case IR_REGION:
            num_inner_region++;
            if (scan_inner_region) {
                REGION_ru(t)->scanCallAndReturnList(
                    num_inner_region, call_list, ret_list, true);
            }
            break;
        case IR_RETURN:
            if (ret_list != NULL) {
                ret_list->append_tail(t);
            }
            break;
        default:;
        }
    }
}


//num_inner_region: count the number of inner regions.
void Region::scanCallAndReturnList(
        OUT UINT & num_inner_region,
        OUT List<IR const*> * call_list,
        OUT List<IR const*> * ret_list,
        bool scan_inner_region)
{
    if (getIRList() != NULL) {
        scanCallListImpl(num_inner_region, getIRList(),
                         call_list, ret_list, scan_inner_region);
    } else {
        for (IRBB * bb = getBBList()->get_head();
             bb != NULL; bb = getBBList()->get_next()) {
            IR * t = BB_last_ir(bb);
            if (t == NULL) { continue; }
            ASSERT0(t->isStmtInBB());
            ASSERT0(call_list);
            if (t != NULL && t->isCallStmt()) {
                call_list->append_tail(t);
            } else if (ret_list != NULL && t->is_return()) {
                ret_list->append_tail(t);
            } else if (scan_inner_region && t->is_region()) {
                num_inner_region++;
                REGION_ru(t)->scanCallAndReturnList(
                    num_inner_region, call_list, ret_list, true);
            }
        }
    }
}


//Prepare informations for analysis phase, such as record
//which variables have been taken address for both
//global and local variable.
void Region::prescan(IR const* ir)
{
    for (; ir != NULL; ir = ir->get_next()) {
        switch (ir->get_code()) {
        case IR_ST:
            prescan(ST_rhs(ir));
            break;
        case IR_CALL:
        case IR_ICALL:
            if (g_do_call_graph && !CALL_is_intrinsic(ir)) {
                List<IR const*> * cl = getCallList();
                ASSERT0(cl);
                cl->append_tail(ir);
            }

            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k != NULL) {
                    ASSERT0(IR_parent(k) == ir);
                    prescan(k);
                }
            }
            break;
        case IR_LDA:
            {
                ASSERT0(LDA_idinfo(ir));
                VAR * v = LDA_idinfo(ir);
                if (v->is_string()) {
                    if (getRegionMgr()->genDedicateStrMD() != NULL) {
                        //Treat all string variable as the same one.
                        break;
                    }

                    VAR * sv = getVarMgr()->
                                registerStringVar(NULL, VAR_str(v), 1);
                    ASSERT0(sv);
                    VAR_is_addr_taken(sv) = true;
                    VAR_allocable(sv) = true;
                } else if (v->is_label()) {
                    ; //do nothing.
                } else {
                    //general variable.
                    if (!ir->getParent()->is_array()) {
                        //If LDA is the base of ARRAY, say (&a)[..], its
                        //address does not need to mark as address taken.
                        VAR_is_addr_taken(LDA_idinfo(ir)) = true;
                    }

                    // ...=&x.a, address of 'x.a' is taken.
                    MD md;
                    MD_base(&md) = LDA_idinfo(ir); //correspond to VAR
                    MD_ofst(&md) = LDA_ofst(ir);
                    MD_size(&md) = ir->get_type_size(getTypeMgr());
                    MD_ty(&md) = MD_EXACT;
                    getMDSystem()->registerMD(md);
                }
            }
            break;
        case IR_ID:
            //Array base must not be ID. It could be
            //LDA or computational expressions.
            //In C, array base address could be assgined to other variable.
            //Its address should be marked as taken. And it's parent must be LDA.
            //e.g: Address of 'a' is taken.
            //    int a[10];
            //    int * p;
            //    p = a;
            ASSERT0(ID_info(ir));
            break;
        case IR_CONST:
        case IR_LD:
        case IR_GOTO:
        case IR_LABEL:
        case IR_PR:
        case IR_BREAK:
        case IR_CONTINUE:
        case IR_PHI:
        case IR_REGION:
            break;
        case IR_RETURN:
            if (g_do_call_graph) {
                List<IR const*> * cl = getReturnList();
                ASSERT0(cl);
                cl->append_tail(ir);
            }

            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k != NULL) {
                    ASSERT0(IR_parent(k) == ir);
                    prescan(k);
                }
            }
             break;
        default:
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k != NULL) {
                    ASSERT0(IR_parent(k) == ir);
                    prescan(k);
                }
            }
        }
    }
}


//Dump IR and memory usage.
void Region::dumpMemUsage()
{
    if (g_tfile == NULL) { return; }

    size_t count = count_mem();
    CHAR const* str = NULL;
    if (count < 1024) { str = "B"; }
    else if (count < 1024 * 1024) { count /= 1024; str = "KB"; }
    else if (count < 1024 * 1024 * 1024) { count /= 1024 * 1024; str = "MB"; }
    else { count /= 1024 * 1024 * 1024; str = "GB"; }

    note("\n'%s' use %lu%s memory", getRegionName(), count, str);

    Vector<IR*> * v = getIRVec();
    UINT nid = 0;
    UINT nld = 0;
    UINT nst = 0;
    UINT nlda = 0;
    UINT ncall = 0;
    UINT nicall = 0;
    UINT nstpr = 0;
    UINT npr = 0;
    UINT nist = 0;
    UINT nbin = 0;
    UINT nuna = 0;

    for (int i = 0; i <= v->get_last_idx(); i++) {
        IR * ir = v->get(i);
        if (ir == NULL) { continue; }

        if (ir->is_id()) nid++;
        else if (ir->is_ld()) nld++;
        else if (ir->is_st()) nst++;
        else if (ir->is_lda()) nlda++;
        else if (ir->is_call()) ncall++;
        else if (ir->is_icall()) nicall++;
        else if (ir->is_stpr()) nstpr++;
        else if (ir->is_pr()) npr++;
        else if (ir->is_ist()) nist++;
        else if (ir->isBinaryOp()) nbin++;
        else if (ir->isUnaryOp()) nuna++;
    }

    UINT total = (v->get_last_idx() + 1);
    if (total == 0) {
        note("\nThe number of IR Total:0");
        return;
    }

    note("\nThe number of IR Total:%u, id:%u(%.1f)%%, "
         "ld:%u(%.1f)%%, st:%u(%.1f)%%, lda:%u(%.1f)%%,"
         "call:%u(%.1f)%%, icall:%u(%.1f)%%, pr:%u(%.1f)%%, "
         "stpr:%u(%.1f)%%, ist:%u(%.1f)%%,"
         "bin:%u(%.1f)%%, una:%u(%.1f)%%",
         total,
         nid, ((double)nid) / ((double)total) * 100,
         nld, ((double)nld) / ((double)total) * 100,
         nst, ((double)nst) / ((double)total) * 100,
         nlda, ((double)nlda) / ((double)total) * 100,
         ncall, ((double)ncall) / ((double)total) * 100,
         nicall, ((double)nicall) / ((double)total) * 100,
         npr, ((double)npr) / ((double)total) * 100,
         nstpr, ((double)nstpr) / ((double)total) * 100,
         nist, ((double)nist) / ((double)total) * 100,
         nbin, ((double)nbin) / ((double)total) * 100,
         nuna, ((double)nuna) / ((double)total) * 100);
}


void Region::dumpGR(bool dump_inner_region)
{
    note("\n//====---- Dump region '%s' ----====", getRegionName());
    note("\nregion ");
    switch (REGION_type(this)) {
    case REGION_PROGRAM: prt("program "); break;
    case REGION_BLACKBOX: prt("blx "); break;
    case REGION_FUNC: prt("func "); break;
    case REGION_INNER: prt("sub "); break;
    default: ASSERT0(0); //TODO
    }
    if (getRegionVar() != NULL) {
        prt("%s ", SYM_name(getRegionVar()->get_name()));
    }

    prt("(");
    dumpParameter();
    prt(")");
    prt(" {\n");
    g_indent += DUMP_INDENT_NUM;
    dumpVarTab();
    DumpGRCtx ctx;
    ctx.dump_inner_region = dump_inner_region;
    ctx.cfg = getCFG();
    if (getIRList() != NULL) {
        dumpGRList(getIRList(), getTypeMgr(), &ctx);
    } else {
        dumpGRInBBList(getBBList(), getTypeMgr(), &ctx);
    }
    g_indent -= DUMP_INDENT_NUM;
    note("\n}");
}


void Region::dumpVarTab()
{
    VarTab * vt = getVarTab();
    if (vt->get_elem_count() == 0) { return; }
    VarTabIter c;
    StrBuf buf(64);

    //Sort var in id order.
    DefSBitSet set(getMiscBitSetMgr()->getSegMgr());
    for (VAR * v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
        if (v->is_formal_param() || v->is_pr()) { continue; }
        set.bunion(v->id());
    }
    SEGIter * cur = NULL;
    for (INT id = set.get_first(&cur);
         id >= 0; id = set.get_next(id, &cur)) {
        VAR * v = getVarMgr()->get_var(id);
        ASSERT0(v);
        buf.clean();
        note("\n%s;", v->dumpGR(buf, getTypeMgr()));
    }
    set.clean();
}


//Dump formal parameter list.
void Region::dumpParameter()
{
    if (!is_function()) { return; }
    VarTabIter c;
    Vector<VAR*> fpvec;
    for (VAR * v = getVarTab()->get_first(c);
         v != NULL; v = getVarTab()->get_next(c)) {
        if (VAR_is_formal_param(v)) {
            ASSERT0(!v->is_pr());
            fpvec.set(v->getFormalParamPos(), v);
        }
    }
    if (fpvec.get_last_idx() < 0) { return; }
    StrBuf buf(32);
    for (INT i = 0; i <= fpvec.get_last_idx(); i++) {
        VAR * v = fpvec.get(i);
        if (i != 0) {
            prt(",");
        }
        if (v == NULL) {
            //This position may be reserved for other use.
            //ASSERT0(v);
            prt("undefined");
            continue;
        }
        buf.clean();
        prt("%s", v->dumpGR(buf, getTypeMgr()));
    }
}


void Region::dump(bool dump_inner_region)
{
    if (g_tfile == NULL) { return; }
    dumpVARInRegion();

    //Dump imported variables referenced.
    MDSet * ru_maydef = getMayDef();
    if (ru_maydef != NULL) {
        note("\nRegionMayDef(OuterRegion):");
        ru_maydef->dump(getMDSystem(), true);
    }

    MDSet * ru_mayuse = getMayUse();
    if (ru_mayuse != NULL) {
        note("\nRegionMayUse(OuterRegion):");
        ru_mayuse->dump(getMDSystem(), true);
    }

    dumpMemUsage();

    IR * irlst = getIRList();
    if (irlst != NULL) {
        note("\n==---- IR List ----==");
        dump_irs(irlst, getTypeMgr(), NULL, true,
                 true, false, dump_inner_region);
        return;
    }

    BBList * bblst = getBBList();
    if (bblst != NULL) {
        dumpBBList(bblst, this, NULL, dump_inner_region);
    }
}


//Dump all irs and ordering by IR_id.
void Region::dumpAllocatedIR()
{
    if (g_tfile == NULL) return;
    note("\n==---- DUMP ALL IR INFO ----==");
    INT n = getIRVec()->get_last_idx();
    INT i = 1;
    g_indent = 2;
    UINT num_has_du = 0;

    //Dump which IR has allocate DU structure.
    while (i <= n) {
        IR * ir = getIRVec()->get(i);
        ASSERT0(ir);
        i++;
        DU * du = ir->getDU();
        if (du != NULL) {
            num_has_du++;
        }
    }
    if (i > 0) {
        note("\nTotal IR %d, total DU allocated %d, rate:(%.1f)%%",
             i, num_has_du, (float)num_has_du / (float)i * 100);
    }
    //

    //Dump IR dispers in free tab.
    note("\n==---- Dump IR dispersed in free tab ----==");
    for (UINT w = 0; w < MAX_OFFSET_AT_FREE_TABLE + 1; w++) {
        IR * lst = REGION_analysis_instrument(this)->m_free_tab[w];
        note("\nbyte(%d)", (INT)(w + sizeof(IR)));
        if (lst == NULL) { continue; }

        UINT num = 0;
        IR * p = lst;
        while (p != NULL) { p = p->get_next(); num++; }
        fprintf(g_tfile, ", num%d : ", num);

        while (lst != NULL) {
            fprintf(g_tfile, "%s", IRNAME(lst));
            lst = IR_next(lst);
            if (lst != NULL) {
                fprintf(g_tfile, ", ");
            }
        }
    }
    fflush(g_tfile);

    note("\n==---- DUMP IR allocated ----==");

    StrBuf buf(64); //record data-type.
    TypeMgr * dm = getTypeMgr();

    i = 1;
    while (i <= n) {
        IR * ir = getIRVec()->get(i);
        ASSERT0(ir);
        Type const* d = NULL;
        if (!ir->is_undef()) {
            d = IR_dt(ir);
            ASSERT0(d);
            if (d == NULL) {
                note("\nid(%d): %s 0x%.8x", ir->id(), IRNAME(ir), ir);
            } else {
                buf.clean();
                note("\nid(%d): %s r:%s 0x%.8x",
                     ir->id(), IRNAME(ir), dm->dump_type(d, buf), ir);
            }
        } else {
            note("\nid(%d): undef 0x%.8x", ir->id(), ir);
        }

        i++;

        DU * du = ir->getDU();
        if (du != NULL) {
            fprintf(g_tfile, " has du");
        }
    }
    fflush(g_tfile);
}


PassMgr * Region::initPassMgr()
{
    if (REGION_analysis_instrument(this)->m_pass_mgr != NULL) {
        return REGION_analysis_instrument(this)->m_pass_mgr;
    }

    REGION_analysis_instrument(this)->m_pass_mgr = allocPassMgr();

    return REGION_analysis_instrument(this)->m_pass_mgr;
}


void Region::destroyPassMgr()
{
    if (ANA_INS_pass_mgr(REGION_analysis_instrument(this)) == NULL) { return; }
    delete ANA_INS_pass_mgr(REGION_analysis_instrument(this));
    ANA_INS_pass_mgr(REGION_analysis_instrument(this)) = NULL;
}


//Verify MD reference to stmts and expressions.
bool Region::verifyMDRef()
{
    ConstIRIter cii;
    BBList * bbl = getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            cii.clean();
            for (IR const* t = iterInitC(ir, cii);
                 t != NULL; t = iterNextC(cii)) {
                switch (IR_code(t)) {
                case IR_ID:
                    //We do not need MD or MDSET information of IR_ID.
                    //ASSERT0(t->getExactRef());
                    ASSERT0(t->getRefMDSet() == NULL);
                    break;
                case IR_LD:
                    if (g_is_support_dynamic_type) {
                        ASSERT(t->getEffectRef(), ("type is at least effect"));
                        ASSERT(!t->getEffectRef()->is_pr(),
                               ("MD can not present a PR."));
                    } else {
                        ASSERT(t->getExactRef(), ("type must be exact"));
                        ASSERT(!t->getExactRef()->is_pr(),
                               ("MD can not present a PR."));
                    }

                    //MayUse of ld may not empty.
                    //e.g: cvt(ld(id(x,i8)), i32) x has exact md4(size=1), and
                    //an overlapped md5(size=4).

                    if (t->getRefMDSet() != NULL) {
                        ASSERT0(getMDSetHash()->find(*t->getRefMDSet()));
                    }
                    break;
                case IR_PR:
                    if (g_is_support_dynamic_type) {
                        ASSERT(t->getEffectRef(),
                               ("type is at least effect"));
                        ASSERT(t->getEffectRef()->is_pr(),
                               ("MD must present a PR."));
                    } else {
                        ASSERT(t->getExactRef(), ("type must be exact"));
                        ASSERT(t->getExactRef()->is_pr(),
                               ("MD must present a PR."));
                    }

                    ASSERT0(t->getRefMDSet() == NULL);
                    break;
                case IR_STARRAY:
                    {
                        MD const* must = t->getEffectRef();
                        MDSet const* may = t->getRefMDSet();
                        DUMMYUSE(must);
                        DUMMYUSE(may);
                        ASSERT0(must || (may && !may->is_empty()));
                        if (must != NULL) {
                            //PR can not be accessed by indirect operation.
                            ASSERT0(!must->is_pr());
                        }

                        if (may != NULL) {
                            //PR can not be accessed by indirect operation.
                            SEGIter * iter;
                            for (INT i = may->get_first(&iter);
                                 i >= 0; i = may->get_next(i, &iter)) {
                                MD const* x = getMDSystem()->getMD(i);
                                DUMMYUSE(x);
                                ASSERT0(x && !x->is_pr());
                            }
                            ASSERT0(getMDSetHash()->find(*may));
                        }
                    }
                    break;
                case IR_ARRAY:
                case IR_ILD:
                    {
                        MD const* mustuse = t->getEffectRef();
                        MDSet const* mayuse = t->getRefMDSet();
                        DUMMYUSE(mustuse);
                        DUMMYUSE(mayuse);

                        ASSERT0(mustuse || (mayuse && !mayuse->is_empty()));
                        if (mustuse != NULL) {
                            //PR can not be accessed by indirect operation.
                            ASSERT0(!mustuse->is_pr());
                        }

                        if (mayuse != NULL) {
                            //PR can not be accessed by indirect operation.
                            SEGIter * iter;
                            for (INT i = mayuse->get_first(&iter);
                                 i >= 0; i = mayuse->get_next(i, &iter)) {
                                MD const* x = getMDSystem()->getMD(i);
                                DUMMYUSE(x);
                                ASSERT0(x && !x->is_pr());
                            }
                            ASSERT0(getMDSetHash()->find(*mayuse));
                        }
                    }
                    break;
                case IR_ST:
                    if (g_is_support_dynamic_type) {
                        ASSERT(t->getEffectRef(),
                               ("type is at least effect"));
                        ASSERT(!t->getEffectRef()->is_pr(),
                               ("MD can not present a PR."));
                    } else {
                        ASSERT(t->getExactRef(), ("type must be exact"));
                        ASSERT(!t->getExactRef()->is_pr(),
                               ("MD can not present a PR."));
                    }
                    //ST may modify overlapped memory object.
                    if (t->getRefMDSet() != NULL) {
                        ASSERT0(getMDSetHash()->find(*t->getRefMDSet()));
                    }
                    break;
                case IR_STPR:
                case IR_SETELEM:
                case IR_GETELEM:
                    if (g_is_support_dynamic_type) {
                        ASSERT(t->getEffectRef(),
                               ("type is at least effect"));
                        ASSERT(t->getEffectRef()->is_pr(),
                               ("MD must present a PR."));
                    } else {
                        ASSERT(t->getExactRef(), ("type must be exact"));
                        ASSERT(t->getExactRef()->is_pr(),
                               ("MD must present a PR."));
                    }

                    ASSERT0(t->getRefMDSet() == NULL);
                    break;
                case IR_IST:
                    {
                        MD const* mustdef = t->getRefMD();
                        if (mustdef != NULL) {
                            //mustdef may be fake object, e.g: global memory.
                            //ASSERT0(mustdef->is_effect());

                            //PR can not be accessed by indirect operation.
                            ASSERT0(!mustdef->is_pr());
                        }

                        MDSet const* maydef = t->getRefMDSet();
                        ASSERT0(mustdef != NULL ||
                                (maydef != NULL && !maydef->is_empty()));
                        if (maydef != NULL) {
                            //PR can not be accessed by indirect operation.
                            SEGIter * iter;
                            for (INT i = maydef->get_first(&iter);
                                 i >= 0; i = maydef->get_next(i, &iter)) {
                                MD const* x = getMDSystem()->getMD(i);
                                DUMMYUSE(x);
                                ASSERT0(x);
                                ASSERT0(!x->is_pr());
                            }
                            ASSERT0(getMDSetHash()->find(*maydef));
                        }
                    }
                    break;
                case IR_CALL:
                case IR_ICALL:
                    if (t->getRefMDSet() != NULL) {
                        ASSERT0(getMDSetHash()->find(*t->getRefMDSet()));
                    }
                    break;
                case IR_PHI:
                    ASSERT0(t->getEffectRef() && t->getEffectRef()->is_pr());
                    ASSERT0(t->getRefMDSet() == NULL);
                    break;
                case IR_CVT:
                    //CVT should not have any reference. Even if the
                    //operation will genrerate different type memory
                    //accessing.
                case IR_CONST:
                case IR_LDA:
                case IR_ADD:
                case IR_SUB:
                case IR_MUL:
                case IR_DIV:
                case IR_REM:
                case IR_MOD:
                case IR_LAND:
                case IR_LOR:
                case IR_BAND:
                case IR_BOR:
                case IR_XOR:
                case IR_BNOT:
                case IR_LNOT:
                case IR_NEG:
                case IR_LT:
                case IR_LE:
                case IR_GT:
                case IR_GE:
                case IR_EQ:
                case IR_NE:
                case IR_ASR:
                case IR_LSR:
                case IR_LSL:
                case IR_SELECT:
                case IR_CASE:
                case IR_BREAK:
                case IR_CONTINUE:
                case IR_TRUEBR:
                case IR_FALSEBR:
                case IR_GOTO:
                case IR_IGOTO:
                case IR_SWITCH:
                case IR_RETURN:
                case IR_REGION:
                    ASSERT0(t->getRefMD() == NULL && t->getRefMDSet() == NULL);
                    break;
                default: ASSERT(0, ("unsupport ir type"));
                }
            }
        }
    }
    return true;
}


bool Region::verifyRPO(OptCtx & oc)
{
    if (getCFG() == NULL) { return true; }
    ASSERT0(getBBList());
    if (OC_is_rpo_valid(oc)) {
        ASSERT(getCFG()->getBBListInRPO()->get_elem_count() ==
               getBBList()->get_elem_count(),
               ("Previous pass has changed RPO, "
                "and you should set it to be invalid"));
    }
    return true;
}


//Ensure that each IR in ir_list must be allocated in current region.
bool Region::verifyIRinRegion()
{
    IR const* ir = getIRList();
    if (ir == NULL) { return true; }
    for (; ir != NULL; ir = ir->get_next()) {
        ASSERT(getIR(ir->id()) == ir,
               ("ir id:%d is not allocated in region %s", getRegionName()));
    }
    return true;
}


//Verify cond/uncond target label.
bool Region::verifyBBlist(BBList & bbl)
{
    LAB2BB lab2bb;
    for (IRBB * bb = bbl.get_head(); bb != NULL; bb = bbl.get_next()) {
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != NULL; li = bb->getLabelList().get_next()) {
            lab2bb.set(li, bb);
        }
    }

    for (IRBB * bb = bbl.get_head(); bb != NULL; bb = bbl.get_next()) {
        IR * last = BB_last_ir(bb);
        if (last == NULL) { continue; }

        if (last->isConditionalBr()) {
            ASSERT(lab2bb.get(BR_lab(last)),
                    ("branch target cannot be NULL"));
        } else if (last->isMultiConditionalBr()) {
            ASSERT0(last->is_switch());

            for (IR * c = SWITCH_case_list(last); c != NULL; c = c->get_next()) {
                ASSERT(lab2bb.get(CASE_lab(last)),
                        ("case branch target cannot be NULL"));
            }

            if (SWITCH_deflab(last) != NULL) {
                ASSERT(lab2bb.get(SWITCH_deflab(last)),
                        ("default target cannot be NULL"));
            }
        } else if (last->isUnconditionalBr()) {
            if (last->is_goto()) {
                ASSERT(lab2bb.get(GOTO_lab(last)), ("target cannot be NULL"));
            } else {
                for (IR * caseexp = IGOTO_case_list(last); caseexp != NULL;
                    caseexp = caseexp->get_next()) {
                    ASSERT(lab2bb.get(CASE_lab(caseexp)),
                        ("target cannot be NULL"));
                }
            }
        }
    }
    return true;
}


//Dump all MD that related to VAR.
void Region::dumpVarMD(VAR * v, UINT indent)
{
    StrBuf buf(64);
    ConstMDIter iter;
    MDTab * mdtab = getMDSystem()->getMDTab(v);
    if (mdtab != NULL) {
        MD const* x = mdtab->get_effect_md();
        if (x != NULL) {
            dumpIndent(g_tfile, indent);
            buf.clean();
            x->dump(buf, getTypeMgr());
            note("\n%s", buf.buf);
        }

        OffsetTab * ofstab = mdtab->get_ofst_tab();
        ASSERT0(ofstab);
        if (ofstab->get_elem_count() > 0) {
            iter.clean();
            for (MD const* md = ofstab->get_first(iter, NULL);
                 md != NULL; md = ofstab->get_next(iter, NULL)) {
                dumpIndent(g_tfile, indent);
                buf.clean();
                md->dump(buf, getTypeMgr());
                note("\n%s", buf.buf);
            }
        }
    }
    fflush(g_tfile);
}


//Dump each VAR in current region's VAR table.
void Region::dumpVARInRegion()
{
    if (g_tfile == NULL) { return; }
    StrBuf buf(64);

    //Dump Region name.
    if (getRegionVar() != NULL) {
        note("\n==---- REGION(%d):%s:", REGION_id(this), getRegionName());
        getRegionVar()->dumpVARDecl(buf);
        fprintf(g_tfile, "%s ----==", buf.buf);
    } else {
        note("\n==---- REGION(%d): ----==", REGION_id(this));
    }

    //Dump formal parameter list.
    if (is_function()) {
        bool has_param = false;
        VarTabIter c;
        for (VAR * v = getVarTab()->get_first(c);
             v != NULL; v = getVarTab()->get_next(c)) {
            if (VAR_is_formal_param(v)) {
                has_param = true;
                break;
            }
        }

        if (has_param) {
            note("\nFORMAL PARAMETERS:");
            c.clean();
            Vector<VAR*> fpvec;
            for (VAR * v = getVarTab()->get_first(c);
                 v != NULL; v = getVarTab()->get_next(c)) {
                if (VAR_is_formal_param(v)) {
                    fpvec.set(v->getFormalParamPos(), v);
                }
            }
            for (INT i = 0; i <= fpvec.get_last_idx(); i++) {
                VAR * v = fpvec.get(i);
                if (v == NULL) {
                    //This position may be reserved for other use.
                    //ASSERT0(v);
                    g_indent += 2;
                    note("\n--");
                    fprintf(g_tfile, " param%d", i);
                    g_indent -= 2;
                    continue;
                }
                buf.clean();
                v->dump(buf, getTypeMgr());
                g_indent += 2;
                note("\n%s", buf.buf);
                fprintf(g_tfile, " param%d", i);
                fflush(g_tfile);
                g_indent += 2;
                dumpVarMD(v, g_indent);
                g_indent -= 2;
                g_indent -= 2;
            }
        }
    }

    //Dump local varibles.
    VarTab * vt = getVarTab();
    if (vt->get_elem_count() > 0) {
        note("\nVARIABLES:");
        g_indent += 2;
        VarTabIter c;
        for (VAR * v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
            buf.clean();
            v->dump(buf, getTypeMgr());
            note("\n%s", buf.buf);
            fflush(g_tfile);
            g_indent += 2;
            dumpVarMD(v, g_indent);
            g_indent -= 2;
        }
        g_indent -= 2;
    }

    fflush(g_tfile);
}


//Copy src's AI to tgt.
void Region::copyAI(IR const* src, IR * tgt)
{
    if (src->getAI() == NULL) { return; }
    if (IR_ai(tgt) == NULL) {
        IR_ai(tgt) = allocAIContainer();
    }
    IR_ai(tgt)->copy(src->getAI());
}


void Region::checkValidAndRecompute(OptCtx * oc, ...)
{
    BitSet opts;
    UINT num = 0;
    va_list ptr;
    va_start(ptr, oc);
    PASS_TYPE opty = (PASS_TYPE)va_arg(ptr, UINT);
    while (opty != PASS_UNDEF && num < 1000) {
        ASSERT(opty < PASS_NUM, ("You should append PASS_UNDEF to pass list."));
        opts.bunion(opty);
        num++;
        opty = (PASS_TYPE)va_arg(ptr, UINT);
    }
    va_end(ptr);
    ASSERT(num < 1000, ("too many pass queried or miss ending placeholder"));

    if (num == 0) { return; }

    PassMgr * passmgr = getPassMgr();
    ASSERT(passmgr, ("PassMgr is not enable"));

    IR_CFG * cfg = (IR_CFG*)passmgr->queryPass(PASS_CFG);
    IR_AA * aa = NULL;
    IR_DU_MGR * dumgr = NULL;

    if (opts.is_contain(PASS_CFG) && !OC_is_cfg_valid(*oc)) {
        if (cfg == NULL) {
            //CFG is not constructed.
            cfg = (IR_CFG*)getPassMgr()->registerPass(PASS_CFG);
            cfg->initCfg(*oc);
        } else {
            //Caution: the validation of cfg should maintained by user.
            cfg->rebuild(*oc);
            cfg->removeEmptyBB(*oc);
            cfg->computeExitList();
        }
    }

    if (opts.is_contain(PASS_CDG) &&
        !OC_is_aa_valid(*oc) &&
        getBBList() != NULL &&
        getBBList()->get_elem_count() != 0) {
        ASSERT(cfg && OC_is_cfg_valid(*oc),
           ("You should make CFG available first."));
        if (aa == NULL) {
            aa = (IR_AA*)passmgr->registerPass(PASS_AA);
            if (!aa->is_init()) {
                aa->initAliasAnalysis();
            }
        }

        ASSERT0(OC_is_cfg_valid(*oc));
        aa->perform(*oc);
    }

    if (opts.is_contain(PASS_DOM) && !OC_is_dom_valid(*oc)) {
        ASSERT(cfg && OC_is_cfg_valid(*oc),
               ("You should make CFG available first."));
        cfg->computeDomAndIdom(*oc);
    }

    if (opts.is_contain(PASS_PDOM) && !OC_is_pdom_valid(*oc)) {
        ASSERT(cfg && OC_is_cfg_valid(*oc),
               ("You should make CFG available first."));
        cfg->computePdomAndIpdom(*oc);
    }

    if (opts.is_contain(PASS_CDG) && !OC_is_cdg_valid(*oc)) {
        ASSERT0(passmgr);
        CDG * cdg = (CDG*)passmgr->registerPass(PASS_CDG);
        ASSERT0(cdg); //cdg is not enable.
        ASSERT(cfg && OC_is_cfg_valid(*oc),
               ("You should make CFG available first."));
        cdg->rebuild(*oc, *cfg);
    }

    UINT f = 0;
    if (opts.is_contain(PASS_DU_REF) && !OC_is_ref_valid(*oc)) {
        f |= SOL_REF;
    }

    if (opts.is_contain(PASS_LIVE_EXPR) && !OC_is_live_expr_valid(*oc)) {
        f |= SOL_AVAIL_EXPR;
    }

    if (opts.is_contain(PASS_EXPR_TAB) && !OC_is_live_expr_valid(*oc)) {
        f |= SOL_AVAIL_EXPR;
    }

    if (opts.is_contain(PASS_AVAIL_REACH_DEF) &&
        !OC_is_avail_reach_def_valid(*oc)) {
        f |= SOL_AVAIL_REACH_DEF;
    }

    if (opts.is_contain(PASS_DU_CHAIN) &&
        !OC_is_du_chain_valid(*oc) &&
        !OC_is_reach_def_valid(*oc)) {
        f |= SOL_REACH_DEF;
    }

    if ((HAVE_FLAG(f, SOL_REF) || opts.is_contain(PASS_AA)) &&
        !OC_is_aa_valid(*oc) &&
        getBBList() != NULL &&
        getBBList()->get_elem_count() != 0) {
        ASSERT(cfg && OC_is_cfg_valid(*oc),
               ("You should make CFG available first."));
        if (aa == NULL) {
            aa = (IR_AA*)passmgr->registerPass(PASS_AA);

            if (!aa->is_init()) {
                aa->initAliasAnalysis();
            }

            UINT numir = 0;
            for (IRBB * bb = getBBList()->get_head();
                 bb != NULL; bb = getBBList()->get_next()) {
                numir += bb->getNumOfIR();
            }

            if (numir > g_thres_opt_ir_num) {
                aa->set_flow_sensitive(false);
            }
        }

        aa->perform(*oc);
    }

    if (f != 0 &&
        getBBList() != NULL &&
        getBBList()->get_elem_count() != 0) {
        if (dumgr == NULL) {
            dumgr = (IR_DU_MGR*)passmgr->registerPass(PASS_DU_MGR);
        }

        f |= COMPUTE_NOPR_DU|COMPUTE_PR_DU;

        dumgr->perform(*oc, f);
        if (HAVE_FLAG(f, SOL_REF)) {
            ASSERT0(verifyMDRef());
        }
        if (HAVE_FLAG(f, SOL_AVAIL_EXPR)) {
            ASSERT0(dumgr->verifyLiveinExp());
        }
    }

    if (opts.is_contain(PASS_DU_CHAIN) &&
        !OC_is_du_chain_valid(*oc) &&
        getBBList() != NULL &&
        getBBList()->get_elem_count() != 0) {
        if (dumgr == NULL) {
            dumgr = (IR_DU_MGR*)passmgr->registerPass(PASS_DU_MGR);
        }

        UINT flag = COMPUTE_NOPR_DU;

        //If PRs have already been in SSA form, compute
        //DU chain doesn't make any sense.
        PRSSAMgr * ssamgr = (PRSSAMgr*)passmgr->queryPass(PASS_PR_SSA_MGR);
        if (ssamgr == NULL) {
            flag |= COMPUTE_PR_DU;
        }

        if (opts.is_contain(PASS_REACH_DEF)) {
            dumgr->computeMDDUChain(*oc, true, flag);
        } else {
            dumgr->computeMDDUChain(*oc, false, flag);
        }
    }

    if (opts.is_contain(PASS_EXPR_TAB) &&
        !OC_is_expr_tab_valid(*oc) &&
        getBBList() != NULL &&
        getBBList()->get_elem_count() != 0) {
        IR_EXPR_TAB * exprtab =
            (IR_EXPR_TAB*)passmgr->registerPass(PASS_EXPR_TAB);
        ASSERT0(exprtab);
        exprtab->reperform(*oc);
    }

    if (opts.is_contain(PASS_LOOP_INFO) && !OC_is_loopinfo_valid(*oc)) {
        ASSERT(cfg && OC_is_cfg_valid(*oc),
               ("You should make CFG available first."));
        cfg->LoopAnalysis(*oc);
    }

    if (opts.is_contain(PASS_RPO)) {
        ASSERT(cfg && OC_is_cfg_valid(*oc),
               ("You should make CFG available first."));
        if (!OC_is_rpo_valid(*oc)) {
            cfg->computeRPO(*oc);
        } else {
            ASSERT(cfg->getBBListInRPO()->get_elem_count() ==
                   getBBList()->get_elem_count(),
                   ("Previous pass has changed RPO, "
                    "and you should set it to be invalid"));
        }
    }
}


bool Region::partitionRegion()
{
    //----- DEMO CODE ----------
    IR * ir = getIRList();
    IR * start_pos = NULL;
    IR * end_pos = NULL;
    while (ir != NULL) {
        if (ir->is_label()) {
            LabelInfo const* li = LAB_lab(ir);
            if (LABEL_INFO_type(li) == L_CLABEL &&
                strcmp(SYM_name(LABEL_INFO_name(li)), "REGION_START") == 0) {
                start_pos = ir;
                break;
            }
        }
        ir = ir->get_next();
    }
    if (ir == NULL) return false;
    ir = ir->get_next();
    while (ir != NULL) {
        if (ir->is_label()) {
            LabelInfo const* li = LAB_lab(ir);
            if (LABEL_INFO_type(li) == L_CLABEL &&
                strcmp(SYM_name(LABEL_INFO_name(li)), "REGION_END") == 0) {
                end_pos = ir;
                break;
            }
        }
        ir = ir->get_next();
    }
    if (start_pos == NULL || end_pos == NULL) return false;
    ASSERT0(start_pos != end_pos);
    //----------------

    //Generate IR region.
    Type const* type = getTypeMgr()->getMCType(0);
    VAR * ruv = getVarMgr()->registerVar("inner_ru",
        type, 1, VAR_LOCAL|VAR_FAKE);
    VAR_allocable(ruv) = false;
    addToVarTab(ruv);

    Region * inner_ru = getRegionMgr()->allocRegion(REGION_INNER);
    inner_ru->setRegionVar(ruv);
    IR * ir_ru = buildRegion(inner_ru);
    copyDbx(ir, ir_ru, inner_ru);
    //------------

    ir = IR_next(start_pos);
    while (ir != end_pos) {
        IR * t = ir;
        ir = ir->get_next();
        remove(&REGION_analysis_instrument(this)->m_ir_list, t);
        IR * inner_ir = inner_ru->dupIRTree(t);
        freeIRTree(t);
        inner_ru->addToIRList(inner_ir);
    }
    dump_irs(inner_ru->getIRList(), getTypeMgr());
    insertafter_one(&start_pos, ir_ru);
    dump_irs(getIRList(), getTypeMgr());
    //-------------
    OptCtx oc;
    bool succ = REGION_ru(ir_ru)->process(&oc);
    ASSERT0(succ);
    DUMMYUSE(succ);

    dump_irs(getIRList(), getTypeMgr());

    //Merger IR list in inner-region to outer region.
    //remove(&REGION_analysis_instrument(this)->m_ir_list, ir_ru);
    //IR * head = inner_ru->constructIRlist();
    //insertafter(&split_pos, dupIRTreeList(head));
    //dump_irs(getIRList());

    delete inner_ru;
    return false;
}


//Check and rescan call list of region if one of elements in list changed.
void Region::updateCallAndReturnList(bool scan_inner_region)
{
    if (readCallList() == NULL) { return; }
    UINT num_inner_region = 0;
    List<IR const*> * clst = getCallList();
    if (clst == NULL) { return; }

    C<IR const*> * ct;
    for (clst->get_head(&ct); ct != clst->end(); ct = clst->get_next(ct)) {
        IR const* c = ct->val();
        ASSERT0(c);
        if (!c->isCallStmt()) {
            //Call stmt has changed, then rescanning is needed.
            scanCallAndReturnList(num_inner_region, scan_inner_region);
            return;
        }
    }

    List<IR const*> * retlst = getReturnList();
    if (retlst == NULL) { return; }

    for (retlst->get_head(&ct);
         ct != retlst->end(); ct = retlst->get_next(ct)) {
        IR const* c = ct->val();
        ASSERT0(c);
        if (!c->is_return()) {
            //Return stmt has changed, then rescanning is needed.
            scanCallAndReturnList(num_inner_region, scan_inner_region);
            return;
        }
    }
}


bool Region::processBBList(OptCtx & oc)
{
    if (getBBList() == NULL || getBBList()->get_elem_count() == 0) {
        return true;
    }
    return MiddleProcess(oc);
}


bool Region::processIRList(OptCtx & oc)
{
    if (getIRList() == NULL) { return true; }

    START_TIMER(t, "PreScan");
    prescan(getIRList());
    END_TIMER(t, "PreScan");
    if (!HighProcess(oc)) { return false; }
    ASSERT0(getDUMgr()->verifyMDDUChain(COMPUTE_PR_DU|COMPUTE_NOPR_DU));
    if (!MiddleProcess(oc)) { return false; }

    return true;
}


//Return true if all passes finished normally, otherwise return false.
bool Region::process(OptCtx * oc)
{
    ASSERT(oc, ("Need OptCtx"));
    ASSERT0(verifyIRinRegion());
    note("\nREGION_NAME:%s", getRegionName());

    if (getIRList() == NULL &&
        getBBList()->get_elem_count() == 0) {
        return true;
    }

    OC_show_comp_time(*oc) = g_show_comp_time;
    initPassMgr();

    if (g_do_inline && is_program()) {
        //Need to scan call-list.
        getRegionMgr()->buildCallGraph(*oc, true, true);
        if (OC_is_callg_valid(*oc)) {
            Inliner * inl = (Inliner*)getPassMgr()->
                registerPass(PASS_INLINER);
            inl->perform(*oc);
            getPassMgr()->destroyPass(inl);
        }
    }

    PRSSAMgr * ssamgr = NULL;
    MDSSAMgr * mdssamgr = NULL;

    if (getIRList() != NULL) {
        if (!processIRList(*oc)) { goto ERR_RET; }
    } else {
        if (!processBBList(*oc)) { goto ERR_RET; }
    }

    if (g_do_ipa && is_program()) {
        if (!OC_is_callg_valid(*oc)) {
            //processFuncRegion has scanned and collected call-list.
            //Thus it does not need to scan call-list here.
            getRegionMgr()->buildCallGraph(*oc, true, true);
        }

        if (OC_is_callg_valid(*oc)) {
            IPA * ipa = (IPA*)getPassMgr()->registerPass(PASS_IPA);
            ipa->perform(*oc);
            getPassMgr()->destroyPass(ipa);
        }
    }

    ssamgr = (PRSSAMgr*)getPassMgr()->queryPass(PASS_PR_SSA_MGR);
    if (ssamgr != NULL && ssamgr->isSSAConstructed()) {
        ssamgr->destruction();
    }

    mdssamgr = (MDSSAMgr*)getPassMgr()->queryPass(PASS_MD_SSA_MGR);
    if (mdssamgr != NULL && mdssamgr->isMDSSAConstructed()) {
        mdssamgr->destruction();
    }

    if (!g_retain_pass_mgr_for_region) {
        destroyPassMgr();
    }

    updateCallAndReturnList(true);
    tfree();

    return true;

ERR_RET:
    ASSERT0(getPassMgr());
    ssamgr = (PRSSAMgr*)getPassMgr()->queryPass(PASS_PR_SSA_MGR);
    if (ssamgr != NULL && ssamgr->isSSAConstructed()) {
        ssamgr->destruction();
    }

    if (!g_retain_pass_mgr_for_region) {
        destroyPassMgr();
    }

    return false;
}
//END Region

} //namespace xoc
