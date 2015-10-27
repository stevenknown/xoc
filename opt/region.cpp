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

//
//START AnalysisInstrument
//
AnalysisInstrument::AnalysisInstrument(Region * ru) :
    m_mds_mgr(ru, &m_sbs_mgr), m_mds_hash(&m_mds_mgr, &m_sbs_mgr)
{
    m_ru = ru;
    m_ru_mgr = NULL;
    m_call_list = NULL;
    m_ir_list = NULL;
    m_pass_mgr = NULL;

    //Counter of IR_PR, and do not use '0' as prno.
    m_pr_count = 1;
    m_pool = smpoolCreate(256, MEM_COMM);
    m_du_pool = smpoolCreate(sizeof(DU) * 4, MEM_CONST_SIZE);
    memset(m_free_tab, 0, sizeof(m_free_tab));
}


static void destroyVARandMD(Region * ru, AnalysisInstrument * anainstr)
{
    //Free md's id and local-var's id back to MDSystem and VarMgr.
    //The index of MD and VAR is important resource if there
    //are a lot of REGIONs in RegionMgr.
    VarMgr * varmgr = ru->get_var_mgr();
    MDSystem * mdsys = ru->get_md_sys();
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
    //DefSegMgr * segmgr = m_sbs_mgr.get_seg_mgr();
    //dumpSegMgr(segmgr, g_tfile);
    #endif

    if (m_pass_mgr != NULL) {
        delete m_pass_mgr;
        m_pass_mgr = NULL;
    }

    //Free AttachInfo's internal structure.
    //The vector of AttachInfo must be destroyed explicitly.
    INT l = m_ir_vector.get_last_idx();
    for (INT i = 1; i <= l; i++) {
        IR * ir = m_ir_vector.get(i);
        ASSERT0(ir);
        if (ir->is_region()) {
            m_ru_mgr->deleteRegion(REGION_ru(ir));
        }
        if (IR_ai(ir) != NULL) {
            IR_ai(ir)->destroy_vec();
        }
        ir->freeDUset(m_sbs_mgr);
    }

    destroyVARandMD(m_ru, this);

    //Destroy all IR. IR allocated in the pool.
    smpoolDelete(m_pool);
    m_pool = NULL;

    //Destroy all DU structure. DU set allocated in the du_pool.
    smpoolDelete(m_du_pool);
    m_du_pool = NULL;

    m_ir_list = NULL;

    //Destory CALL list.
    if (m_call_list != NULL) {
        delete m_call_list;
        m_call_list = NULL;
    }

    if (REGION_refinfo(m_ru) != NULL) {
        REF_INFO_mayuse(REGION_refinfo(m_ru)).clean(m_sbs_mgr);
        REF_INFO_maydef(REGION_refinfo(m_ru)).clean(m_sbs_mgr);
        REF_INFO_mustdef(REGION_refinfo(m_ru)).clean(m_sbs_mgr);
    }
}


bool AnalysisInstrument::verify_var(VarMgr * vm, VAR * v)
{
    CK_USE(v);
    CK_USE(vm);
    if (m_ru->is_function() || m_ru->is_eh() ||
        REGION_type(m_ru) == RU_SUB) {
        //If var is global but unallocable, it often be
        //used as placeholder or auxilary var.

        //For these kind of regions, there are only local variable or
        //unablable global variable is legal.
        ASSERT0(VAR_is_local(v) || !VAR_allocable(v));
    } else if (m_ru->is_program()) {
        //For program region, only global variable is legal.
        ASSERT0(VAR_is_global(v));
    } else {
        ASSERT(0, ("unsupport variable type."));
    }
    return true;
}


UINT AnalysisInstrument::count_mem()
{
    UINT count = 0;
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
    m_ru_type = rt;
    m_var = NULL;
    id = 0;
    m_parent = NULL;
    m_ref_info = NULL;
    REGION_analysis_instrument(this) = NULL;
    m_u2.s1b1 = 0;
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
    m_ref_info = NULL;
    id = 0;
    m_parent = NULL;
    m_ru_type = RU_UNDEF;
}


UINT Region::count_mem()
{
    UINT count = 0;
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


IRBB * Region::newBB()
{
    IRBB * bb = get_bb_mgr()->newBB();
    return bb;
}


//Build IR_PR operation by specified prno and type id.
IR * Region::buildPRdedicated(UINT prno, Type const* type)
{
    ASSERT0(type);
    IR * ir = newIR(IR_PR);
    PR_no(ir) = prno;
    IR_dt(ir) = type;
    return ir;
}


//Build IR_PR operation by specified type id.
IR * Region::buildPR(Type const* type)
{
    ASSERT0(type);
    IR * ir = newIR(IR_PR);
    PR_no(ir) = REGION_analysis_instrument(this)->m_pr_count++;
    IR_dt(ir) = type;
    return ir;
}


//Generate a PR number by specified prno and type id.
//This operation will allocate new PR number.
UINT Region::buildPrno(Type const* type)
{
    ASSERT0(type);
    UNUSED(type);
    return REGION_analysis_instrument(this)->m_pr_count++;
}


//Build IR_LNOT operation.
IR * Region::buildLogicalNot(IR * opnd0)
{
    return buildUnaryOp(IR_LNOT, get_type_mgr()->getBool(), opnd0);
}


//Build Logical operations, include IR_LAND, IR_LOR, IR_XOR.
//NOTICE: opnd1 of IR_LNOT, IR_BNOT should be NULL.
IR * Region::buildLogicalOp(IR_TYPE irt, IR * opnd0, IR * opnd1)
{
    IR * ir = newIR(irt);
    ASSERT0(opnd0 && opnd1);
    ASSERT0(irt == IR_LAND || irt == IR_LOR || irt == IR_XOR);
    BIN_opnd0(ir) = opnd0;
    BIN_opnd1(ir) = opnd1;
    IR_parent(opnd0) = ir;
    IR_parent(opnd1) = ir;
    IR_dt(ir) = get_type_mgr()->getSimplexTypeEx(D_B);
    return ir;
}


//Build IR_ID operation.
IR * Region::buildId(IN VAR * var)
{
    ASSERT0(var);
    IR * ir = newIR(IR_ID);
    ASSERT0(var != NULL);
    ID_info(ir) = var;
    IR_dt(ir) = VAR_type(var);
    return ir;
}


//Build IR_LDA operation.
IR * Region::buildLda(IR * lda_base)
{
    ASSERT(lda_base &&
            (lda_base->is_id() ||
             lda_base->is_array() ||
             lda_base->is_ild() ||
             lda_base->is_str()),
            ("cannot get base of PR"));
    IR * ir = newIR(IR_LDA);
    LDA_base(ir) = lda_base;
    TypeMgr * dm = get_type_mgr();
    IR_dt(ir) = dm->getPointerType(lda_base->get_dtype_size(dm));
    IR_parent(lda_base) = ir;
    return ir;
}


//Build IR_CONST operation.
//The result IR indicates a string.
IR * Region::buildString(SYM * strtab)
{
    ASSERT0(strtab);
    IR * str = newIR(IR_CONST);
    CONST_str_val(str) = strtab;
    IR_dt(str) = get_type_mgr()->getSimplexTypeEx(D_STR);
    return str;
}


//Build conditionally selected expression.
//e.g: x = a > b ? 10 : 100
IR * Region::buildSelect(
        IR * det,
        IR * true_exp,
        IR * false_exp,
        Type const* type)
{
    ASSERT0(type);
    ASSERT0(det && true_exp && false_exp);
    ASSERT0(det->is_judge());
    ASSERT0(true_exp->is_type_equal(false_exp));
    IR * ir = newIR(IR_SELECT);
    IR_dt(ir) = type;
    SELECT_det(ir) = det;
    SELECT_trueexp(ir) = true_exp;
    SELECT_falseexp(ir) = false_exp;

    IR_parent(det) = ir;
    IR_parent(true_exp) = ir;
    IR_parent(false_exp) = ir;
    return ir;
}


//Build IR_LABEL operation.
IR * Region::buildIlabel()
{
    IR * ir = newIR(IR_LABEL);
    IR_dt(ir) = get_type_mgr()->getVoid();
    LAB_lab(ir) = genIlabel();
    return ir;
}


//Build IR_LABEL operation.
IR * Region::buildLabel(LabelInfo const* li)
{
    ASSERT0(li);
    ASSERT0(LABEL_INFO_type(li) == L_ILABEL ||
            LABEL_INFO_type(li) == L_CLABEL);
    IR * ir = newIR(IR_LABEL);
    IR_dt(ir) = get_type_mgr()->getVoid();
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
    IR * ir = newIR(IR_CVT);
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
    IR * ir = newIR(IR_PHI);
    PHI_prno(ir) = prno;
    IR_dt(ir) = type;

    IR * last = NULL;
    for (UINT i = 0; i < num_opnd; i++) {
        IR * x = buildPRdedicated(prno, type);
        PR_ssainfo(x) = NULL;
        add_next(&PHI_opnd_list(ir), &last, x);
        IR_parent(x) = ir;
    }
    return ir;
}


/* Build IR_CALL operation.
'res_list': reture value list.
'result_prno': indicate the result PR which hold the return value.
    0 means the call does not have a return value.
'type': result PR data type.
    0 means the call does not have a return value */
IR * Region::buildCall(
            VAR * callee,
            IR * param_list,
            UINT result_prno,
            Type const* type)
{
    ASSERT0(type);
    ASSERT0(callee);
    IR * ir = newIR(IR_CALL);
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


/* Build IR_ICALL operation.
'res_list': reture value list.
'result_prno': indicate the result PR which hold the return value.
    0 means the call does not have a return value.
'type': result PR data type.
    0 means the call does not have a return value. */
IR * Region::buildIcall(
        IR * callee,
        IR * param_list,
        UINT result_prno,
        Type const* type)
{
    ASSERT0(type);
    ASSERT0(callee);
    IR * ir = newIR(IR_ICALL);
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
IR * Region::buildRegion(Region * ru)
{
    ASSERT0(ru && !ru->is_undef());
    IR * ir = newIR(IR_REGION);
    IR_dt(ir) = get_type_mgr()->getVoid();
    REGION_ru(ir) = ru;
    REGION_parent(ru) = this;

    #ifdef _DEBUG_
    if (ru->is_function()) {
        ASSERT(is_program(),
            ("Only program region can have a function region as subregion."));
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

    IR * ir = newIR(IR_IGOTO);
    IR_dt(ir) = get_type_mgr()->getVoid();
    IGOTO_vexp(ir) = vexp;
    IGOTO_case_list(ir) = case_list;
    IR_parent(vexp) = ir;

    IR * c = case_list;
    while (c != NULL) {
        ASSERT0(c->is_case());
        IR_parent(c) = ir;
        c = IR_next(c);
    }
    return ir;
}


//Build IR_GOTO operation.
IR * Region::buildGoto(LabelInfo const* li)
{
    ASSERT0(li);
    IR * ir = newIR(IR_GOTO);
    IR_dt(ir) = get_type_mgr()->getVoid();
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
    IR * ir = newIR(IR_LD);
    LD_idinfo(ir) = var;
    IR_dt(ir) = type;
    if (g_is_hoist_type) {
        //Hoisting I16/U16/I8/U8 to I32, to utilize whole register.
        DATA_TYPE dt = ir->get_dtype();
        if (IS_SIMPLEX(dt)) {
            //Hoist data-type from less than INT to INT.
            IR_dt(ir) =
                get_type_mgr()->getSimplexTypeEx(get_type_mgr()->hoistDtype(dt));
        }
    }
    return ir;
}


/* Build IR_ILD operation.
Result is either register or memory chunk, and the size of ILD
result equals to 'pointer_base_size' of 'addr'.

'base': memory address of ILD.
'ptbase_or_mc_size': if result of ILD is pointer, this parameter records
    pointer_base_size; or if result is memory chunk, it records
    the size of memory chunk.

NOTICE:
    The ofst of ILD requires to maintain when after return.
*/
IR * Region::buildIload(IR * base, Type const* type)
{
    ASSERT0(type);
    ASSERT(base && base->is_ptr(), ("mem-address of ILD must be pointer"));
    IR * ir = newIR(IR_ILD);
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


//Build store operation to store 'rhs' to new pr with type and prno.
//'prno': target prno.
//'type': data type of targe pr.
//'rhs: value expected to store.
IR * Region::buildStorePR(UINT prno, Type const* type, IR * rhs)
{
    ASSERT0(type);
    ASSERT0(prno > 0 && rhs);
    IR * ir = newIR(IR_STPR);
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
    ASSERT0(type);
    ASSERT0(rhs);
    IR * ir = newIR(IR_STPR);
    STPR_no(ir) = REGION_analysis_instrument(this)->m_pr_count++;
    STPR_rhs(ir) = rhs;
    IR_dt(ir) = type;
    IR_parent(rhs) = ir;
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
    ASSERT0(lhs != NULL && rhs != NULL);
    IR * ir = newIR(IR_ST);
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
IR * Region::buildIstore(IR * lhs, IR * rhs, UINT ofst, Type const* type)
{
    ASSERT0(type);
    IR * ir = buildIstore(lhs, rhs, type);
    IST_ofst(ir) = ofst;
    return ir;
}


//Build IR_IST operation.
//'lhs': target memory location pointer.
//'rhs: value expected to store.
//'type': result type of indirect memory operation, note type is not the
//data type of lhs.
IR * Region::buildIstore(IR * lhs, IR * rhs, Type const* type)
{
    ASSERT0(type);
    ASSERT(lhs && rhs && (lhs->is_ptr() || lhs->is_array()),
            ("must be pointer"));
    IR * ir = newIR(IR_IST);
    IR_dt(ir) = type;
    IST_base(ir) = lhs;
    IST_rhs(ir) = rhs;
    IR_parent(lhs) = ir;
    IR_parent(rhs) = ir;
    return ir;
}


/* Build IR_ARRAY operation.
'base': base of array operation, it is either LDA or pointer.
'sublist': subscript expression list.
'type': result type of array operator.
    Note that type may NOT be equal to elem_tyid, accroding to
    ARR_ofst(). If ARR_ofst() is not zero, that means array
    elem is MC, or VECTOR, and type should be type of member
    to MC/VECTOR.
    e.g: struct S{ int a,b,c,d;}
        struct S pa[100];
        If youe do access pa[1].c
        type should be int rather than struct S.
        and elem_tyid should be struct S.

'elem_tyid': record element-data-type.
    e.g:vector<int,8> g[100];
        elem_size is sizeof(vector<int,8>) = 32
        elem_type is vector.
    e.g1: struct S{ int a,b,c,d;}
        struct S * pa[100];
        elem_size is sizeof(struct S *)
        elem_type is PTR.
    e.g2:
        struct S pa[100];
        elem_size is sizeof(struct S)
        elem_type is struct S

'dims': indicate the array dimension.
'elem_num': point to an integer array that indicate
    the number of element for each dimension. The length of the integer
    array should be equal to 'dims'.
    e.g: int g[12][24];
        elem_num points to an array with 2 value, [12, 24].
        the 1th dimension has 12 elements, and the 2th dimension has 24
        elements, which element type is D_I32. */
IR * Region::buildArray(
        IR * base,
        IR * sublist,
        Type const* type,
        Type const* elemtype,
        UINT dims,
        TMWORD const* elem_num)
{
    ASSERT0(type);
    ASSERT0(base && sublist && elemtype);
    CArray * ir = (CArray*)newIR(IR_ARRAY);
    IR_dt(ir) = type;
    ARR_base(ir) = base;
    IR_parent(base) = ir;
    ARR_sub_list(ir) = sublist;
    UINT n = 0;
    for (IR * p = sublist; p != NULL; p = IR_next(p)) {
        IR_parent(p) = ir;
        n++;
    }
    ASSERT0(n == dims);
    ARR_elemtype(ir) = elemtype;

    UINT l = sizeof(TMWORD) * dims;
    TMWORD * ebuf = (TMWORD*)xmalloc(l);
    memcpy(ebuf, elem_num, l);
    ARR_elem_num_buf(ir) = ebuf;
    return ir;
}


/* Build IR_STARRAY operation.
'base': base of array operation, it is either LDA or pointer.
'sublist': subscript expression list.
'type': result type of array operator.
    Note that type may NOT be equal to elem_tyid, accroding to
    ARR_ofst(). If ARR_ofst() is not zero, that means array
    elem is MC, or VECTOR, and type should be type of member
    to MC/VECTOR.
    e.g: struct S{ int a,b,c,d;}
        struct S pa[100];
        If youe do access pa[1].c
        type should be int rather than struct S.
        and elem_tyid should be struct S.

'elem_tyid': record element-data-type.
    e.g:vector<int,8> g[100];
        elem_size is sizeof(vector<int,8>) = 32
        elem_type is vector.
    e.g1: struct S{ int a,b,c,d;}
        struct S * pa[100];
        elem_size is sizeof(struct S *)
        elem_type is PTR.
    e.g2:
        struct S pa[100];
        elem_size is sizeof(struct S)
        elem_type is struct S

'dims': indicate the array dimension.
'elem_num': point to an integer array that indicate
    the number of element for each dimension. The length of the integer
    array should be equal to 'dims'.
    e.g: int g[12][24];
        elem_num points to an array with 2 value, [12, 24].
        the 1th dimension has 12 elements, and the 2th dimension has 24
        elements, which element type is D_I32.
'rhs: value expected to store. */
IR * Region::buildStoreArray(
        IR * base,
        IR * sublist,
        Type const* type,
        Type const* elemtype,
        UINT dims,
        TMWORD const* elem_num,
        IR * rhs)
{
    ASSERT0(base && sublist && type && elemtype);
    CStArray * ir = (CStArray*)newIR(IR_STARRAY);
    IR_dt(ir) = type;
    ARR_base(ir) = base;
    IR_parent(base) = ir;
    ARR_sub_list(ir) = sublist;
    UINT n = 0;
    for (IR * p = sublist; p != NULL; p = IR_next(p)) {
        IR_parent(p) = ir;
        n++;
    }
    ASSERT0(n == dims);
    ARR_elemtype(ir) = elemtype;

    UINT l = sizeof(TMWORD) * dims;
    TMWORD * ebuf = (TMWORD*)xmalloc(l);
    memcpy(ebuf, elem_num, l);
    ARR_elem_num_buf(ir) = ebuf;
    STARR_rhs(ir) = rhs;
    IR_parent(rhs) = ir;
    return ir;
}



//Build IR_RETURN operation.
IR * Region::buildReturn(IR * retexp)
{
    IR * ir = newIR(IR_RETURN);
    IR_dt(ir) = get_type_mgr()->getVoid();
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
    IR * ir = newIR(IR_CONTINUE);
    IR_dt(ir) = get_type_mgr()->getVoid();
    return ir;
}


//Build IR_BREAK operation.
IR * Region::buildBreak()
{
    IR * ir = newIR(IR_BREAK);
    IR_dt(ir) = get_type_mgr()->getVoid();
    return ir;
}


//Build IR_CASE operation.
IR * Region::buildCase(IR * casev_exp, LabelInfo const* jump_lab)
{
    ASSERT0(casev_exp && jump_lab);
    IR * ir = newIR(IR_CASE);
    IR_dt(ir) = get_type_mgr()->getVoid();
    CASE_lab(ir) = jump_lab;
    CASE_vexp(ir) = casev_exp;
    IR_parent(casev_exp) = ir;
    return ir;
}


/* Build Do Loop stmt.
'det': determinate expression.
'loop_body': stmt list.
'init': record the stmt that initialize iv.
'step': record the stmt that update iv. */
IR * Region::buildDoLoop(IR * det, IR * init, IR * step, IR * loop_body)
{
    ASSERT0(det &&
            (det->is_lt() || det->is_le() || det->is_gt() || det->is_ge()));
    ASSERT0(init->is_st() || init->is_stpr());
    ASSERT0(step->is_st() || step->is_stpr());
    ASSERT0(step->get_rhs()->is_add());

    IR * ir = newIR(IR_DO_LOOP);
    IR_dt(ir) = get_type_mgr()->getVoid();
    LOOP_det(ir) = det;
    IR_parent(det) = ir;

    ASSERT0(init && init->is_stmt() && step && step->is_stmt());

    LOOP_init(ir) = init;
    LOOP_step(ir) = step;

    IR_parent(init) = ir;
    IR_parent(step) = ir;

    LOOP_body(ir) = loop_body;
    IR * c = loop_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = IR_next(c);
    }
    return ir;
}


/* Build Do While stmt.
'det': determinate expression.
'loop_body': stmt list. */
IR * Region::buildDoWhile(IR * det, IR * loop_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = newIR(IR_DO_WHILE);
    IR_dt(ir) = get_type_mgr()->getVoid();
    LOOP_det(ir) = det;
    IR_parent(det) = ir;

    LOOP_body(ir) = loop_body;
    IR * c = loop_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = IR_next(c);
    }
    return ir;
}


//Build While Do stmt.
//'det': determinate expression.
//'loop_body': stmt list.
IR * Region::buildWhileDo(IR * det, IR * loop_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = newIR(IR_WHILE_DO);
    IR_dt(ir) = get_type_mgr()->getVoid();
    LOOP_det(ir) = det;
    IR_parent(det) = ir;

    LOOP_body(ir) = loop_body;
    IR * c = loop_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = IR_next(c);
    }
    return ir;
}


/* Build IF stmt.
'det': determinate expression.
'true_body': stmt list.
'false_body': stmt list. */
IR * Region::buildIf(IR * det, IR * true_body, IR * false_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = newIR(IR_IF);
    IR_dt(ir) = get_type_mgr()->getVoid();
    IF_det(ir) = det;
    IR_parent(det) = ir;

    IF_truebody(ir) = true_body;
    IR * c = true_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = IR_next(c);
    }

    IF_falsebody(ir) = false_body;
    c = false_body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = IR_next(c);
    }
    return ir;
}


/* Build SWITCH multi-select stmt.
'vexp': expression to determine which case entry will be target.
'case_list': case entry list. case entry is consist of expression and label.
    Note that case list is optional.
'body': stmt list.
'default_lab': label indicates the default choice, the label is optional.

NOTE: Do not set parent for stmt in 'body'. */
IR * Region::buildSwitch(
        IR * vexp,
        IR * case_list,
        IR * body,
        LabelInfo const* default_lab)
{
    ASSERT0(vexp && vexp->is_exp());
    IR * ir = newIR(IR_SWITCH);
    IR_dt(ir) = get_type_mgr()->getVoid();
    SWITCH_vexp(ir) = vexp;
    SWITCH_case_list(ir) = case_list;
    SWITCH_body(ir) = body;
    SWITCH_deflab(ir) = default_lab;
    IR_parent(vexp) = ir;

    IR * c = case_list;
    while (c != NULL) {
        ASSERT0(c->is_case());
        IR_parent(c) = ir;
        c = IR_next(c);
    }

    c = body;
    while (c != NULL) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = IR_next(c);
    }
    return ir;
}


//Build IR_TRUEBR or IR_FALSEBR operation.
IR * Region::buildBranch(bool is_true_br, IR * det, LabelInfo const* lab)
{
    ASSERT0(lab && det && det->is_judge());
    IR * ir;
    if (is_true_br) {
        ir = newIR(IR_TRUEBR);
    } else {
        ir = newIR(IR_FALSEBR);
    }
    IR_dt(ir) = get_type_mgr()->getVoid();
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
    IR * imm = newIR(IR_CONST);
    //Convert string to hex value , that is in order to generate
    //single load instruction to load float point value in Code
    //Generator.
    ASSERT0(get_type_mgr()->is_fp(type));
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
    ASSERT(get_type_mgr()->is_int(type), ("expect integer type"));
    IR * imm = newIR(IR_CONST);
    CONST_int_val(imm) = v;
    IR_dt(imm) = type;
    return imm;
}


//Build IR_ADD operation.
//The expression indicates a pointer arithmetic.
//For pointer arithemtic, the addend of pointer must be
//product of the pointer-base-size and rchild if lchild is pointer.
IR * Region::buildPointerOp(IR_TYPE irt, IR * lchild, IR * rchild)
{
    ASSERT0(lchild && rchild);
    TypeMgr * dm = get_type_mgr();
    if (!lchild->is_ptr() && rchild->is_ptr()) {
        ASSERT0(irt == IR_ADD ||
                 irt == IR_MUL ||
                 irt == IR_XOR ||
                 irt == IR_BAND ||
                 irt == IR_BOR ||
                 irt == IR_EQ ||
                 irt == IR_NE);
        return buildPointerOp(irt, rchild, lchild);
    }

    Type const* d0 = lchild->get_type();
    Type const* d1 = rchild->get_type();
    UNUSED(d1);
    if (lchild->is_ptr() && rchild->is_ptr()) {
        //CASE: Pointer substraction.
        //    char *p, *q;
        //    p-q => t1=p-q, t2=t1/4, return t2
        switch (irt) {
        case IR_SUB:
            {
                TypeMgr * dm = get_type_mgr();
                //Result is not pointer type.
                ASSERT0(TY_ptr_base_size(d0) > 0);
                ASSERT0(TY_ptr_base_size(d0) == TY_ptr_base_size(d1));
                IR * ret = newIR(IR_SUB);
                BIN_opnd0(ret) = lchild;
                BIN_opnd1(ret) = rchild;
                IR_dt(ret) = dm->getSimplexTypeEx(
                                    dm->get_dtype(WORD_BITSIZE, true));
                if (TY_ptr_base_size(d0) > BYTE_PER_CHAR) {
                    IR * div = newIR(IR_DIV);
                    BIN_opnd0(div) = ret;
                    BIN_opnd1(div) = buildImmInt(TY_ptr_base_size(d0),
                                                  IR_dt(ret));
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
                IR * ret = newIR(irt);
                BIN_opnd0(ret) = lchild;
                BIN_opnd1(ret) = rchild;
                IR_dt(ret) = dm->getSimplexTypeEx(D_B);
                IR_parent(lchild) = ret;
                IR_parent(rchild) = ret;
                return ret;
            }
        default: ASSERT(0, ("illegal pointers operation"));
        }
        ASSERT(0, ("can not get here."));
    } else if (lchild->is_ptr() && !rchild->is_ptr()) {
        /* Result is pointer type.
        CASE:
            int * p;
            p + 4 => t1 = p + (4 * sizeof(BASE_TYPE_OF(p)))
            p - 4 => t1 = p - (4 * sizeof(BASE_TYPE_OF(p)))
        */
        switch (irt) {
        case IR_ADD:
        case IR_SUB:
            {
                IR * addend = newIR(IR_MUL);
                BIN_opnd0(addend) = rchild;
                BIN_opnd1(addend) = buildImmInt(TY_ptr_base_size(d0),
                                                 IR_dt(rchild));
                IR_dt(addend) = IR_dt(rchild);

                  IR * ret = newIR(irt); //ADD or SUB
                BIN_opnd0(ret) = lchild; //lchild is pointer.
                BIN_opnd1(ret) = addend; //addend is not pointer.

                //CASE: 'p = p + 1'
                //so the result type of '+' should still be pointer type.
                ret->setPointerType(TY_ptr_base_size(d0), dm);

                //Avoid too much boring pointer operations.
                ret->setParentPointer(true);
                return ret;
            }
        default:
            {
                ASSERT0(irt == IR_LT || irt == IR_LE ||
                         irt == IR_GT || irt == IR_GE ||
                         irt == IR_EQ || irt == IR_NE);
                //Pointer operation will incur undefined behavior.
                  IR * ret = newIR(irt);
                BIN_opnd0(ret) = lchild;
                BIN_opnd1(ret) = rchild;
                IR_dt(ret) = dm->getSimplexTypeEx(D_B);

                IR_parent(lchild) = ret;
                IR_parent(rchild) = ret;
                return ret;
            }
        }
    }
    ASSERT(0, ("can not get here."));
    return NULL; //just ceases warning.
}


//Helper function.
//Det-expression should be a relation-operation,
//so we create a node comparing with ZERO by NE node.
IR * Region::buildJudge(IR * exp)
{
    ASSERT0(!exp->is_judge());
    Type const* type = IR_dt(exp);
    TypeMgr * dm = get_type_mgr();
    if (exp->is_ptr()) {
        type = dm->getSimplexTypeEx(dm->getPointerSizeDtype());
    }
    IR * x = buildCmp(IR_NE, exp, buildImmInt(0, type));
    return x;
}


//Build comparision operations.
IR * Region::buildCmp(IR_TYPE irt, IR * lchild, IR * rchild)
{
    ASSERT0(irt == IR_LAND || irt == IR_LOR ||
            irt == IR_LT || irt == IR_LE ||
            irt == IR_GT || irt == IR_GE ||
            irt == IR_NE || irt == IR_EQ);
    ASSERT0(lchild && rchild);
    if (lchild->is_const() &&
        !rchild->is_const() &&
        (irt == IR_EQ || irt == IR_NE)) {
        return buildCmp(irt, rchild, lchild);
    }
    IR * ir = newIR(irt);
    BIN_opnd0(ir) = lchild;
    BIN_opnd1(ir) = rchild;
    IR_dt(ir) = get_type_mgr()->getSimplexTypeEx(D_B);
    IR_parent(lchild) = ir;
    IR_parent(rchild) = ir;
    return ir;
}


IR * Region::buildUnaryOp(IR_TYPE irt, Type const* type, IN IR * opnd)
{
    ASSERT0(type);
    ASSERT0(is_una_irt(irt));
    ASSERT0(opnd);
    IR * ir = newIR(irt);
    UNA_opnd0(ir) = opnd;
    IR_dt(ir) = type;
    IR_parent(opnd) = ir;
    return ir;

}


//Build binary op without considering pointer arith.
IR * Region::buildBinaryOpSimp(
        IR_TYPE irt,
        Type const* type,
        IR * lchild,
        IR * rchild)
{
    ASSERT0(type);
    ASSERT0(lchild && rchild);
    IR * ir = newIR(irt);
    BIN_opnd0(ir) = lchild;
    BIN_opnd1(ir) = rchild;
    IR_parent(lchild) = ir;
    IR_parent(rchild) = ir;
    IR_dt(ir) = type;
    return ir;
}


//Build binary operation.
//'mc_size': record the memory-chunk size if rtype is D_MC, or else is 0.
IR * Region::buildBinaryOp(
        IR_TYPE irt,
        Type const* type,
        IR * lchild,
        IR * rchild)
{
    ASSERT0(type);
    ASSERT0(lchild && rchild);
    if (lchild->is_ptr() || rchild->is_ptr()) {
        return buildPointerOp(irt, lchild, rchild);
    }
    if (lchild->is_const() &&
        !rchild->is_const() &&
        (irt == IR_ADD ||
         irt == IR_MUL ||
         irt == IR_XOR ||
         irt == IR_BAND ||
         irt == IR_BOR ||
         irt == IR_EQ ||
         irt == IR_NE)) {
        return buildBinaryOp(irt, type, rchild, lchild);
    }

    //Both lchild and rchild are NOT pointer.
    //Generic binary operation.
    UINT mc_size = 0;
    if (TY_dtype(type) == D_MC) {
        mc_size = TY_mc_size(type);
        UNUSED(mc_size);
        ASSERT0(mc_size == lchild->get_dtype_size(get_type_mgr()) &&
                 mc_size == rchild->get_dtype_size(get_type_mgr()));
    }
    ASSERT0(type->is_mc() ^ (mc_size == 0));
    return buildBinaryOpSimp(irt, type, lchild, rchild);
}


/* Split list of ir into basic block.
'irs': a list of ir.
'bbl': a list of bb.
'ctbb': marker current bb container. */
C<IRBB*> * Region::splitIRlistIntoBB(IR * irs, BBList * bbl, C<IRBB*> * ctbb)
{
    IR_CFG * cfg = get_cfg();
    ASSERT(cfg, ("CFG is not available"));

    IRBB * newbb = newBB();
    cfg->add_bb(newbb);
    ctbb = bbl->insert_after(newbb, ctbb);
    LAB2BB * lab2bb = cfg->get_lab2bb_map();

    while (irs != NULL) {
        IR * ir = removehead(&irs);
        if (newbb->is_bb_down_boundary(ir)) {
            BB_irlist(newbb).append_tail(ir);
            newbb = newBB();
            cfg->add_bb(newbb);
            ctbb = bbl->insert_after(newbb, ctbb);
        } else if (newbb->is_bb_up_boundary(ir)) {
            ASSERT0(ir->is_label());

            newbb = newBB();
            cfg->add_bb(newbb);
            ctbb = bbl->insert_after(newbb, ctbb);

            //Regard label-info as add-on info that attached on newbb, and
            //'ir' will be dropped off.
            LabelInfo const* li = ir->get_label();
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


/* Find the boundary IR generated in BB to update bb-list incremently.
e.g: Given BB1 has one stmt:
    BB1:
    a = b||c
after some simplification, we get:
    truebr(b != 0), L1
    truebr(c != 0), L1
    pr = 0
    goto L2
    L1:
    pr = 1
    L2:
    a = pr

where IR list should be splitted into :
    BB1:
    truebr(b != 0), L1
    BB2:
    truebr(c != 0), L1
    BB3:
    pr = 0
    goto L2
    BB4:
    L1:
    pr = 1
    BB5:
    L2:
    a = pr */
bool Region::reconstructBBlist(OptCTX & oc)
{
    START_TIMER("Reconstruct IRBB list");
    ASSERT(get_cfg(), ("CFG is not available"));

    bool change = false;
    C<IRBB*> * ctbb;
    BBList * bbl = get_bb_list();
    for (bbl->get_head(&ctbb); ctbb != NULL; bbl->get_next(&ctbb)) {
        IRBB * bb = C_val(ctbb);
        C<IR*> * ctir;
        BBIRList * irlst = &BB_irlist(bb);

        IR * tail = irlst->get_tail();
        for (irlst->get_head(&ctir); ctir != NULL; irlst->get_next(&ctir)) {
            IR * ir = C_val(ctir);

            if (bb->is_bb_down_boundary(ir) && ir != tail) {
                change = true;

                IR * restirs = NULL; //record rest part in bb list after 'ir'.
                IR * last = NULL;
                irlst->get_next(&ctir);

                for (C<IR*> * next_ctir = ctir;
                     ctir != NULL; ctir = next_ctir) {
                    irlst->get_next(&next_ctir);
                    irlst->remove(ctir);
                    add_next(&restirs, &last, C_val(ctir));
                }

                ctbb = splitIRlistIntoBB(restirs, bbl, ctbb);
                break;
            } else if (bb->is_bb_up_boundary(ir)) {
                ASSERT0(ir->is_label());

                change = true;
                BB_is_fallthrough(bb) = true;

                IR * restirs = NULL; //record rest part in bb list after 'ir'.
                IR * last = NULL;

                for (C<IR*> * next_ctir = ctir;
                     ctir != NULL; ctir = next_ctir) {
                    irlst->get_next(&next_ctir);
                    irlst->remove(ctir);
                    add_next(&restirs, &last, C_val(ctir));
                }

                ctbb = splitIRlistIntoBB(restirs, bbl, ctbb);
                break;
            }
        }
    }

    END_TIMER();

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
    START_TIMER("Construct IRBB list");
    IR * ret_list = NULL;
    IR * last = NULL;
    C<IRBB*> * ct;
    for (get_bb_list()->get_head(&ct);
         ct != get_bb_list()->end();
         ct = get_bb_list()->get_next(ct)) {
        IRBB * bb = ct->val();
        C<LabelInfo const*> * lct;
        for (bb->get_lab_list().get_head(&lct);
             lct != bb->get_lab_list().end();
             lct = bb->get_lab_list().get_next(lct)) {
            LabelInfo const* li = lct->val();
            //insertbefore_one(&ret_list, ret_list, buildLabel(li));
            add_next(&ret_list, &last, buildLabel(li));
        }

        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            //insertbefore_one(&ret_list, ret_list, ir);
            add_next(&ret_list, &last, ir);
            if (clean_ir_list) {
                ir->set_bb(NULL);
            }
        }
        if (clean_ir_list) {
            BB_irlist(bb).clean();
        }
    }

    //ret_list = reverse_list(ret_list);
    END_TIMER();
    return ret_list;
}


//1. Split list of IRs into basic-block list.
//2. Set BB propeties. e.g: entry-bb, exit-bb.
void Region::constructIRBBlist()
{
    if (get_ir_list() == NULL) { return; }
    START_TIMER("Construct IRBB list");
    IRBB * cur_bb = NULL;
    IR * pointer = get_ir_list();
    while (pointer != NULL) {
        if (cur_bb == NULL) {
            cur_bb = newBB();
        }

        //Insert IR into individual BB.
        ASSERT0(pointer->isStmtInBB() || pointer->is_lab());
        IR * cur_ir = pointer;
        pointer = IR_next(pointer);
        IR_next(cur_ir) = IR_prev(cur_ir) = NULL;
        //remove(&start_ir, cur_ir);

        if (cur_bb->is_bb_down_boundary(cur_ir)) {
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
                /* We have no knowledge about whether target BB of GOTO/IGOTO
                will be followed subsequently on current BB.
                Leave this problem to CFG builder, and the related
                attribute should be set at that time. */
                break;
            case IR_RETURN:
                //Succeed stmt of 'ir' may be DEAD code
                //IR_BB_is_func_exit(cur_bb) = true;
                BB_is_fallthrough(cur_bb) = true;
                break;
            default: ASSERT(0, ("invalid bb down-boundary IR"));
            } //end switch

            //Generate new BB.
            get_bb_list()->append_tail(cur_bb);
            cur_bb = newBB();
        } else if (cur_ir->is_label()) {
            BB_is_fallthrough(cur_bb) = true;
            get_bb_list()->append_tail(cur_bb);

            //Generate new BB.
            cur_bb = newBB();

            //label info be seen as add-on info attached on bb, and
            //'ir' be dropped off.
            for (;;) {
                cur_bb->addLabel(LAB_lab(cur_ir));
                freeIR(cur_ir);
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
    get_bb_list()->append_tail(cur_bb);

    //cur_bb is the last bb, it is also the exit bb.
    //IR_BB_is_func_exit(cur_bb) = true;
    END_TIMER();
}


/* Do general check for safe optimizing.
    CASE 1:
        Do NOT optimize the code if meet the VIOLATE variable!
        e.g1:
            volatile int x, y;
            int a[SIZE];
            void f(void)
            {
              for (int i = 0; i < SIZE; i++)
                a[i] = x + y;
            }

            Some compilers will hoist the expression (x + y)
            out of the loop as shown below, which is an
            incorrect optimization.
        e.g2:
            ## Incorrect Definiation! x,y are NOT const.
            const violate int x,y; */
bool Region::isSafeToOptimize(IR const* ir)
{
    if (ir->is_volatile()) {
        return false;
    }

    //Check kids.
    for(INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->get_kid(i);
        if (kid != NULL) {
            if (!isSafeToOptimize(kid)) {
                return false;
            }
        }
    }
    return true;
}


/* This function is just generate conservative memory reference
information of Region as a default setting. User should supply
more accurate referrence information via various analysis, since
the information will affect optimizations. */
void Region::genDefaultRefInfo()
{
    //For conservative, current region may modify all outer regions variables.
    Region * t = this;
    ConstMDIter iter;
    MDSystem * md_sys = get_md_sys();

    initRefInfo();

    MDSet tmp;
    DefMiscBitSetMgr * mbs = getMiscBitSetMgr();
    while (t != NULL) {
        VarTab * vt = t->get_var_tab();
        VarTabIter c;
        for (VAR * v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
            if (VAR_is_fake(v)) { continue; }

            MDTab * mdt = md_sys->get_md_tab(v);
            ASSERT0(mdt != NULL);

            MD const* x = mdt->get_effect_md();
            if (x != NULL) {
                tmp.bunion(x, *mbs);
            }
            OffsetTab * ofstab = mdt->get_ofst_tab();
            ASSERT0(ofstab);
            if (ofstab->get_elem_count() > 0) {
                iter.clean();
                for (MD const* x = ofstab->get_first(iter, NULL);
                     x != NULL; x = ofstab->get_next(iter, NULL)) {
                    tmp.bunion(x, *mbs);
                }
            }
        }
        t = REGION_parent(t);
    }

    if (!tmp.is_empty()) {
        REF_INFO_mayuse(REGION_refinfo(this)).copy(tmp, *mbs);
        REF_INFO_maydef(REGION_refinfo(this)).copy(tmp, *mbs);
    } else {
        REF_INFO_mayuse(REGION_refinfo(this)).clean(*mbs);
        REF_INFO_maydef(REGION_refinfo(this)).clean(*mbs);
    }

    REF_INFO_mustdef(REGION_refinfo(this)).clean(*mbs);

    tmp.clean(*getMiscBitSetMgr());
}


//Generate MD corresponding to PR load or write.
MD const* Region::genMDforPR(UINT prno, Type const* type)
{
    ASSERT0(type);
    VAR * pr_var = mapPR2Var(prno);
    if (pr_var == NULL) {
        //Create a new PR VAR.
        CHAR name[64];
        sprintf(name, "pr%d", prno);
        ASSERT0(strlen(name) < 64);
        UINT flag = VAR_LOCAL;
        SET_FLAG(flag, VAR_IS_PR);
        pr_var = get_var_mgr()->registerVar(name, type, 0, flag);
        setMapPR2Var(prno, pr_var);

        //Set the pr-var to be unallocable, means do NOT add
        //pr-var immediately as a memory-variable.
        //For now, it is only be regarded as a pseduo-register.
        //And set it to allocable if the PR is in essence need to be
        //allocated in memory.
        VAR_allocable(pr_var) = false;
        addToVarTab(pr_var);
    }

    MD md;
    MD_base(&md) = pr_var; //correspond to VAR
    MD_ofst(&md) = 0;

    if (REGION_is_pr_unique_for_same_number(this)) {
        MD_size(&md) =
            get_type_mgr()->get_bytesize(get_type_mgr()->getSimplexTypeEx(D_I32));
    } else {
        MD_size(&md) = get_type_mgr()->get_bytesize(type);
    }

    if (type->is_void()) {
        MD_ty(&md) = MD_UNBOUND;
    } else {
        MD_ty(&md) = MD_EXACT;
    }

    MD const* e = get_md_sys()->registerMD(md);
    ASSERT0(MD_id(e) > 0);
    return e;
}


//Get function unit.
Region * Region::get_func_unit()
{
    Region * ru = this;
    while (!ru->is_function()) { ru = REGION_parent(ru); }
    ASSERT(ru != NULL, ("Not in func unit"));
    return ru;
}


CHAR const* Region::get_ru_name() const
{
    return SYM_name(VAR_name(m_var));
}


/* Free ir, ir's sibling, and all its kids.
We can only utilizing the function to free the IR
which allocated by 'newIR'.

NOTICE: If ir's sibling is not NULL, that means the IR is
a high level type. IRBB consists of only middle/low level IR. */
void Region::freeIRTreeList(IR * ir)
{
    if (ir == NULL) return;
    IR * head = ir, * next = NULL;
    while (ir != NULL) {
        next = IR_next(ir);
        remove(&head, ir);
        freeIRTree(ir);
        ir = next;
    }
}


//Free ir, and all its kids.
//We can only utilizing the function to free the IR which allocated by 'newIR'.
void Region::freeIRTreeList(IRList & irs)
{
    C<IR*> * next;
    C<IR*> * ct;
    for (irs.get_head(&ct); ct != irs.end(); ct = next) {
        IR * ir = ct->val();
        next = irs.get_next(ct);
        ASSERT(IR_next(ir) == NULL && IR_prev(ir) == NULL,
                ("do not allow sibling node, need to simplify"));
        irs.remove(ir);
        freeIRTree(ir);
    }
}


//Free ir and all its kids, except its sibling node.
//We can only utilizing the function to free the
//IR which allocated by 'newIR'.
void Region::freeIRTree(IR * ir)
{
    if (ir == NULL) { return; }

    ASSERT(IR_next(ir) == NULL && IR_prev(ir) == NULL,
            ("chain list should be cut off"));
    for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->get_kid(i);
        if (kid != NULL) {
            freeIRTreeList(kid);
        }
    }
    freeIR(ir);
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


void Region::dump_free_tab()
{
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n==-- DUMP Region Free Table --==");
    for (UINT i = 0; i <= MAX_OFFSET_AT_FREE_TABLE; i++) {
        IR * lst = REGION_analysis_instrument(this)->m_free_tab[i];
        if (lst == NULL) { continue; }

        UINT sz = i + sizeof(IR);

        UINT count = 0;
        for (IR * ir = lst; ir != NULL; ir = IR_next(ir)) {
            count++;
        }

        fprintf(g_tfile, "\nirsize(%d), num(%d):", sz, count);

        for (IR * ir = lst; ir != NULL; ir = IR_next(ir)) {
            ASSERT0(get_irt_size(ir) == sz);
            fprintf(g_tfile, "ir(%d),", IR_id(ir));
        }
    }
    fflush(g_tfile);
}


//Generate IR, invoke freeIR() or freeIRTree() if it is useless.
//NOTE: Do NOT invoke ::free() to free IR, because all
//    IR are allocated in the pool.
IR * Region::newIR(IR_TYPE irt)
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

            REGION_analysis_instrument(this)->m_free_tab[idx] = IR_next(ir);
            if (IR_next(ir) != NULL) {
                IR_prev(IR_next(ir)) = NULL;
            }
            break;
        }
    } else {
        ir = REGION_analysis_instrument(this)->m_free_tab[idx];
        if (ir != NULL) {
            REGION_analysis_instrument(this)->m_free_tab[idx] = IR_next(ir);
            if (IR_next(ir) != NULL) {
                IR_prev(IR_next(ir)) = NULL;
            }
        }
    }

    if (ir == NULL) {
        ir = (IR*)xmalloc(IRTSIZE(irt));
        INT v = MAX(get_ir_vec()->get_last_idx(), 0);
        IR_id(ir) = (UINT)(v+1);
        get_ir_vec()->set(IR_id(ir), ir);
        set_irt_size(ir, IRTSIZE(irt));
    } else {
        ASSERT0(IR_prev(ir) == NULL);
        IR_next(ir) = NULL;
        #ifdef _DEBUG_
        REGION_analysis_instrument(this)->m_has_been_freed_irs.diff(IR_id(ir));
        #endif
    }
    IR_code(ir) = irt;
    return ir;
}


//Just append freed 'ir' into free_list.
//Do NOT free its kids and siblings.
void Region::freeIR(IR * ir)
{
    ASSERT0(ir);
    ASSERT(IR_next(ir) == NULL && IR_prev(ir) == NULL,
            ("chain list should be cut off"));
    #ifdef _DEBUG_
    ASSERT0(!REGION_analysis_instrument(this)->
            m_has_been_freed_irs.is_contain(IR_id(ir)));
    REGION_analysis_instrument(this)->m_has_been_freed_irs.bunion(IR_id(ir));
    #endif

    ASSERT0(getMiscBitSetMgr());
    ir->freeDUset(*getMiscBitSetMgr());

    if (IR_ai(ir) != NULL) {
        IR_ai(ir)->destroy();
    }

    DU * du = ir->cleanDU();
    if (du != NULL) {
        DU_md(du) = NULL;
        DU_mds(du) = NULL;
        ASSERT0(du->has_clean());
        REGION_analysis_instrument(this)->m_free_du_list.append_head(du);
    }

    UINT res_id = IR_id(ir);
    AttachInfo * res_ai = IR_ai(ir);
    UINT res_irt_sz = get_irt_size(ir);
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


//Duplication 'ir' and kids, and its sibling, return list of new ir.
//Duplicate irs start from 'ir' to the end of list.
IR * Region::dupIRTreeList(IR const* ir)
{
    IR * new_list = NULL;
    while (ir != NULL) {
        IR * newir = dupIRTree(ir);
        add_next(&new_list, newir);
        ir = IR_next(ir);
    }
    return new_list;
}

//Duplicate 'ir' and its kids, but without ir's sibiling node.
IR * Region::dupIRTree(IR const* ir)
{
    if (ir == NULL) { return NULL; }
    IR * newir = dupIR(ir);
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->get_kid(i);
        if (kid != NULL) {
            IR * newkid_list = dupIRTreeList(kid);
            newir->set_kid(i, newkid_list);
        } else { ASSERT0(newir->get_kid(i) == NULL); }
    }
    return newir;
}


//Duplication all contents of 'src', except the AI info, kids and siblings IR.
//Since src may be come from other region, we do not copy AI info.
IR * Region::dupIR(IR const* src)
{
    if (src == NULL) { return NULL; }
    IR_TYPE irt = src->get_code();
    IR * res = newIR(irt);
    ASSERT(res != NULL && src != NULL, ("res/src is NULL"));

    UINT res_id = IR_id(res);
    AttachInfo * res_ai = IR_ai(res);
    UINT res_irt_sz = get_irt_size(res);
    memcpy(res, src, IRTSIZE(irt));
    IR_id(res) = res_id;
    IR_ai(res) = res_ai;
    set_irt_size(res, res_irt_sz);
    IR_next(res) = IR_prev(res) = IR_parent(res) = NULL;
    res->cleanDU(); //Do not copy DU info.
    res->clearSSAInfo(); //Do not copy SSA info.
    if (IR_ai(src) != NULL) { //need to copy AttachInfo.
        if (IR_ai(res) == NULL) {
            IR_ai(res) = newAI();
        }
        IR_ai(res)->copy(IR_ai(src));
    }
    return res;
}


void Region::scanCallList(OUT List<IR const*> & call_list)
{
    call_list.clean();
    if (get_ir_list() != NULL) {
        for (IR const* t = get_ir_list(); t != NULL; t = IR_next(t)) {
            if (t->is_calls_stmt()) {
                call_list.append_tail(t);
            }
        }
    } else {
        for (IRBB * bb = get_bb_list()->get_head();
             bb != NULL; bb = get_bb_list()->get_next()) {
            IR * t = BB_last_ir(bb);
            if (t != NULL && t->is_calls_stmt()) {
                call_list.append_tail(t);
            }
        }
    }
}


//Prepare informations for analysis phase, such as record
//which variables have been taken address for both
//global and local variable.
void Region::prescan(IR const* ir)
{
    for (; ir != NULL; ir = IR_next(ir)) {
        switch (IR_code(ir)) {
        case IR_ST:
            prescan(ST_rhs(ir));
            break;
        case IR_CALL:
        case IR_ICALL:
            if (g_do_call_graph && !CALL_is_intrinsic(ir)) {
                List<IR const*> * cl = get_call_list();
                ASSERT0(cl);
                cl->append_tail(ir);
            }

            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->get_kid(i);
                if (k != NULL) {
                    ASSERT0(IR_parent(k) == ir);
                    prescan(k);
                }
            }
            break;
        case IR_LDA:
            {
                IR * base = LDA_base(ir);
                if (base->is_id()) {
                    VAR * v = ID_info(base);
                    ASSERT0(v && IR_parent(ir));
                    if (v->is_string() &&
                        get_region_mgr()->getDedicateStrMD() != NULL) {
                        break;
                    }

                    if (!IR_parent(ir)->is_array()) {
                        //If LDA is the base of ARRAY, say (&a)[..], its
                        //address does not need to mark as address taken.
                        VAR_is_addr_taken(v) = true;
                    }

                    // ...=&x.a, address of 'x.a' is taken.
                    MD md;
                    MD_base(&md) = v; //correspond to VAR
                    MD_ofst(&md) = LDA_ofst(ir);
                    MD_size(&md) = ir->get_dtype_size(get_type_mgr());
                    MD_ty(&md) = MD_EXACT;
                    get_md_sys()->registerMD(md);
                } else if (base->is_array()) {
                    //... = &a[x]; address of 'a' is taken
                    IR * array_base = ARR_base(base);
                    ASSERT0(array_base);
                    if (array_base->is_lda()) {
                        prescan(array_base);
                    }
                } else if (base->is_str()) {
                    VAR * v = get_var_mgr()->
                                registerStringVar(NULL, CONST_str_val(base), 1);
                    ASSERT0(v);
                    VAR_is_addr_taken(v) = true;
                    VAR_allocable(v) = true;
                } else if (base->is_ild()) {
                    //We do not known where to be taken.
                    ASSERT(0, ("ILD should not appeared here, "
                               "one should do refining at first."));
                } else {
                    ASSERT(0, ("Illegal LDA base."));
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
            ASSERT0(0);
            //ASSERT0(ID_info(ir) && VAR_is_array(ID_info(ir)));
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
        default:
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->get_kid(i);
                if (k != NULL) {
                    ASSERT0(IR_parent(k) == ir);
                    prescan(k);
                }
            }
        }
    }
}


void Region::dump_bb_usage(FILE * h)
{
    if (h == NULL) { return; }
    BBList * bbl = get_bb_list();
    if (bbl == NULL) { return; }
    fprintf(h, "\nTotal BB num: %d", bbl->get_elem_count());
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        fprintf(h, "\nBB%d: irnum%d", BB_id(bb), bb->getNumOfIR());
    }
    fflush(h);
}


//Dump IR and memory usage.
void Region::dump_mem_usage(FILE * h)
{
    if (h == NULL) { return; }
    fprintf(h, "\n\n'%s' use %dKB memory",
            get_ru_name(), count_mem()/1024);
    Vector<IR*> * v = get_ir_vec();
    float nid = 0.0;
    float nld = 0.0;
    float nst = 0.0;
    float nlda = 0.0;
    float ncallee = 0.0;
    float nstpr = 0.0;
    float nist = 0.0;
    float nbin = 0.0;
    float nuna = 0.0;

    for (int i = 0; i <= v->get_last_idx(); i++) {
        IR * ir = v->get(i);
        if (ir == NULL) continue;
        if (ir->is_id()) nid+=1.0;
        if (ir->is_ld()) nld+=1.0;
        if (ir->is_st()) nst+=1.0;
        if (ir->is_lda()) nlda+=1.0;
        if (ir->is_call()) ncallee+=1.0;
        if (ir->is_stpr()) nstpr+=1.0;
        if (ir->is_ist()) nist+=1.0;
        if (ir->is_binary_op()) nbin+=1.0;
        if (ir->is_unary_op()) nuna+=1.0;
    }

    float total = (float)(v->get_last_idx() + 1);
    fprintf(h, "\nThe number of IR Total:%d, id:%d(%.1f)%%, "
                    "ld:%d(%.1f)%%, st:%d(%.1f)%%, lda:%d(%.1f)%%,"
                    "callee:%d(%.1f)%%, stpr:%d(%.1f)%%, ist:%d(%.1f)%%,"
                    "bin:%d(%.1f)%%, una:%d(%.1f)%%",
            (int)total,
            (int)nid, nid / total * 100,
            (int)nld, nld / total * 100,
            (int)nst, nst / total * 100,
            (int)nlda, nlda / total * 100,
            (int)ncallee, ncallee / total * 100,
            (int)nstpr, nstpr / total * 100,
            (int)nist, nist / total * 100,
            (int)nbin, nbin / total * 100,
            (int)nuna, nuna / total * 100);
}


//Dump all irs and ordering by IR_id.
void Region::dump_all_ir()
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n==---- DUMP IR VECTOR ----==");
    INT n = get_ir_vec()->get_last_idx();
    INT i = 1;
    g_indent = 2;
    UINT num_has_du = 0;

    //Dump which IR has allocate DU structure.
    while (i <= n) {
        IR * ir = get_ir_vec()->get(i);
        ASSERT0(ir);
        i++;
        DU * du = ir->get_du();
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
    fprintf(g_tfile, "\n== Dump IR dispers in free tab ==");
    for (UINT w = 0; w < MAX_OFFSET_AT_FREE_TABLE + 1; w++) {
        IR * lst = REGION_analysis_instrument(this)->m_free_tab[w];
        fprintf(g_tfile, "\nbyte(%d)", (INT)(w + sizeof(IR)));
        if (lst == NULL) { continue; }

        UINT num = 0;
        IR * p = lst;
        while (p != NULL) { p = IR_next(p); num++; }
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

    CHAR buf[256]; //record data-type.
    TypeMgr * dm = get_type_mgr();

    i = 1;
    while (i <= n) {
        IR * ir = get_ir_vec()->get(i);
        ASSERT0(ir);
        Type const* d = NULL;
        if (!ir->is_undef()) {
            d = IR_dt(ir);
            ASSERT0(d);
            if (d == NULL) {
                note("\nid(%d): %s 0x%.8x", IR_id(ir), IRNAME(ir), ir);
            } else {
                note("\nid(%d): %s r:%s 0x%.8x",
                     IR_id(ir), IRNAME(ir), dm->dump_type(d, buf), ir);
            }
        } else {
            note("\nid(%d): undef 0x%.8x", IR_id(ir), ir);
        }

        i++;

        DU * du = ir->get_du();
        if (du != NULL) {
            fprintf(g_tfile, " has du");
        }
    }
    fflush(g_tfile);
}


//Only dump STMT IR.
void Region::dump_all_stmt()
{
    INT i = get_ir_vec()->get_last_idx(), j = 0;
    g_indent = 1;
    note("  == IR STATEMENT: ==");
    g_indent = 2;
    TypeMgr * dm = get_type_mgr();
    while (j <= i) {
        IR * ir = get_ir_vec()->get(j);
        if (ir->is_stmt()) {
            note("\n%d(%d):  %s  (r:%s:%d>",
                 j, IR_id(ir), IRNAME(ir),
                 get_type_mgr()->get_dtype_name(TY_dtype(IR_dt(ir))),
                 ir->get_dtype_size(dm));
        }
        j++;
    }
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


void Region::process()
{
    if (get_ir_list() == NULL) { return ; }

    ASSERT0(verifyIRinRegion());

    ASSERT(is_function() ||
            REGION_type(this) == RU_EH ||
            REGION_type(this) == RU_SUB,
            ("Are you sure this kind of Region executable?"));

    g_indent = 0;

    note("\nREGION_NAME:%s", get_ru_name());

    if (g_opt_level == NO_OPT) { return; }

    OptCTX oc;

    START_TIMER("PreScan");
    prescan(get_ir_list());
    END_TIMER();

    HighProcess(oc);

    MiddleProcess(oc);

    if (!is_function()) { return; }

    BBList * bbl = get_bb_list();

    if (bbl->get_elem_count() == 0) { return; }

    SimpCTX simp;
    simp.set_simp_cf();
    simp.set_simp_array();
    simp.set_simp_select();
    simp.set_simp_local_or_and();
    simp.set_simp_local_not();
    simp.set_simp_to_lowest_heigh();
    simplifyBBlist(bbl, &simp);

    if (g_cst_bb_list && SIMP_need_recon_bblist(&simp)) {
        if (reconstructBBlist(oc) && g_do_cfg) {
            //Before CFG building.
            get_cfg()->removeEmptyBB(oc);

            get_cfg()->rebuild(oc);

            //After CFG building, it is perform different
            //operation to before building.
            get_cfg()->removeEmptyBB(oc);

            get_cfg()->computeEntryAndExit(true, true);

            if (g_do_cdg) {
                ASSERT0(get_pass_mgr());
                CDG * cdg = (CDG*)get_pass_mgr()->registerPass(PASS_CDG);
                cdg->rebuild(oc, *get_cfg());
            }
        }
    }

    ASSERT0(verifyIRandBB(bbl, this));
    RefineCTX rf;
    refineBBlist(bbl, rf);
    ASSERT0(verifyIRandBB(bbl, this));

    ASSERT0(get_pass_mgr());
    IR_SSA_MGR * ssamgr = (IR_SSA_MGR*)get_pass_mgr()->query_opt(PASS_SSA_MGR);
    if (ssamgr != NULL && ssamgr->is_ssa_constructed()) {
        ssamgr->destructionInBBListOrder();
    }

    destroyPassMgr();
}


//Ensure that each IR in ir_list must be allocated in current region.
bool Region::verifyIRinRegion()
{
    IR const* ir = get_ir_list();
    if (ir == NULL) { return true; }
    for (; ir != NULL; ir = IR_next(ir)) {
        ASSERT(get_ir(IR_id(ir)) == ir,
                ("ir id:%d is not allocated in region %s", get_ru_name()));
    }
    return true;
}


//Verify cond/uncond target label.
bool Region::verifyBBlist(BBList & bbl)
{
    LAB2BB lab2bb;
    for (IRBB * bb = bbl.get_head(); bb != NULL; bb = bbl.get_next()) {
        for (LabelInfo const* li = bb->get_lab_list().get_head();
             li != NULL; li = bb->get_lab_list().get_next()) {
            lab2bb.set(li, bb);
        }
    }

    for (IRBB * bb = bbl.get_head(); bb != NULL; bb = bbl.get_next()) {
        IR * last = BB_last_ir(bb);
        if (last == NULL) { continue; }

        if (last->is_cond_br()) {
            ASSERT(lab2bb.get(BR_lab(last)),
                    ("branch target cannot be NULL"));
        } else if (last->is_multicond_br()) {
            ASSERT0(last->is_switch());

            for (IR * c = SWITCH_case_list(last); c != NULL; c = IR_next(c)) {
                ASSERT(lab2bb.get(CASE_lab(last)),
                        ("case branch target cannot be NULL"));
            }

            if (SWITCH_deflab(last) != NULL) {
                ASSERT(lab2bb.get(SWITCH_deflab(last)),
                        ("default target cannot be NULL"));
            }
        } else if (last->is_uncond_br()) {
            ASSERT(lab2bb.get(GOTO_lab(last)), ("target cannot be NULL"));
        }
    }
    return true;
}


//Dump all MD that related to VAR.
void Region::dump_var_md(VAR * v, UINT indent)
{
    CHAR buf[4096];
    buf[0] = 0;
    ConstMDIter iter;
    MDTab * mdtab = get_md_sys()->get_md_tab(v);
    if (mdtab != NULL) {
        MD const* x = mdtab->get_effect_md();
        if (x != NULL) {
            UINT i = 0;
            while (i < indent) { fprintf(g_tfile, " "); i++; }
            buf[0] = 0;
            x->dump(buf, 4096, get_type_mgr());
            fprintf(g_tfile, "%s\n", buf);
        }

        OffsetTab * ofstab = mdtab->get_ofst_tab();
        ASSERT0(ofstab);
        if (ofstab->get_elem_count() > 0) {
            iter.clean();
            for (MD const* md = ofstab->get_first(iter, NULL);
                 md != NULL; md = ofstab->get_next(iter, NULL)) {
                UINT i = 0;
                while (i < indent) { fprintf(g_tfile, " "); i++; }
                buf[0] = 0;
                md->dump(buf, 4096, get_type_mgr());
                fprintf(g_tfile, "%s\n", buf);
            }
        }
    }
    fflush(g_tfile);
}


//Dump each VAR in current region's VAR table.
void Region::dumpVARInRegion(INT indent)
{
    if (g_tfile == NULL) { return; }
    //Dump Region name.
    fprintf(g_tfile, "\nREGION:%s:", get_ru_name());
    CHAR buf[8192]; //Is it too large?
    buf[0] = 0;
    m_var->dumpVARDecl(buf, 100);

    //Dump formal parameter list.
    if (is_function()) {
        g_indent = indent + 1;
        note("\nFORMAL PARAMETERS:\n");
        VarTabIter c;
        for (VAR * v = get_var_tab()->get_first(c);
             v != NULL; v = get_var_tab()->get_next(c)) {
            if (VAR_is_formal_param(v)) {
                buf[0] = 0;
                v->dump(buf, get_type_mgr());
                g_indent = indent + 2;
                note("%s\n", buf);
                fflush(g_tfile);
                dump_var_md(v, g_indent + 2);
            }
        }
    }

    //Dump local varibles.
    g_indent = indent;
    note(" ");
    fprintf(g_tfile, "\n");

    //Dump variables
    VarTab * vt = get_var_tab();
    if (vt->get_elem_count() > 0) {
        g_indent = indent + 1;
        note("LOCAL VARIABLES:\n");
        VarTabIter c;
        for (VAR * v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
            //fprintf(g_tfile, "        ");
            buf[0] = 0;
            v->dump(buf, get_type_mgr());
            g_indent = indent + 2;
            note("%s\n", buf);
            //fprintf(g_tfile, buf);
            //fprintf(g_tfile, "\n");
            fflush(g_tfile);
            dump_var_md(v, g_indent + 2);
        }
    } else {
        g_indent = indent + 1;
        note("NO VARIBLE:\n");
    }

    //Dump subregion.
    fprintf(g_tfile, "\n");
}


void Region::checkValidAndRecompute(OptCTX * oc, ...)
{
    BitSet opts;
    UINT num = 0;
    va_list ptr;
    va_start(ptr, oc);
    PASS_TYPE opty = (PASS_TYPE)va_arg(ptr, UINT);
    while (opty != PASS_UNDEF && num < 1000) {
        ASSERT0(opty < PASS_NUM);
        opts.bunion(opty);
        num++;
        opty = (PASS_TYPE)va_arg(ptr, UINT);
    }
    va_end(ptr);
    ASSERT(num < 1000, ("miss ending placeholder"));

    if (num == 0) { return; }

    PassMgr * passmgr = get_pass_mgr();
    ASSERT(passmgr, ("PassMgr is not enable"));

    IR_CFG * cfg = (IR_CFG*)passmgr->query_opt(PASS_CFG);
    IR_AA * aa = (IR_AA*)passmgr->query_opt(PASS_AA);
    IR_DU_MGR * dumgr = (IR_DU_MGR*)passmgr->query_opt(PASS_DU_MGR);

    if (opts.is_contain(PASS_CFG) && !OC_is_cfg_valid(*oc)) {
        ASSERT(cfg, ("CFG is not enable"));
        //Caution: the validation of cfg should maintained by user.
        cfg->rebuild(*oc);
        cfg->removeEmptyBB(*oc);
        cfg->computeEntryAndExit(true, true);
    }

    if (opts.is_contain(PASS_CDG) && !OC_is_aa_valid(*oc)) {
        ASSERT(cfg, ("CFG is not enable"));
        ASSERT(aa, ("Alias Analysis is not enable"));
        ASSERT0(OC_is_cfg_valid(*oc));
        aa->perform(*oc);
    }

    if (opts.is_contain(PASS_DOM) && !OC_is_dom_valid(*oc)) {
        ASSERT(cfg, ("CFG is not enable"));
        cfg->computeDomAndIdom(*oc);
    }

    if (opts.is_contain(PASS_PDOM) && !OC_is_pdom_valid(*oc)) {
        ASSERT(cfg, ("CFG is not enable"));
        cfg->computePdomAndIpdom(*oc);
    }

    if (opts.is_contain(PASS_CDG) && !OC_is_cdg_valid(*oc)) {
        ASSERT0(passmgr);
        CDG * cdg = (CDG*)passmgr->registerPass(PASS_CDG);
        ASSERT0(cdg); //cdg is not enable.
        ASSERT(cfg, ("CFG is not enable"));
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
        !OC_is_aa_valid(*oc)) {
        ASSERT(aa, ("Alias Analysis is not enable"));
        aa->perform(*oc);
    }

    if (f != 0) {
        ASSERT(dumgr, ("Alias Analysis is not enable"));
        dumgr->perform(*oc, f);
        if (HAVE_FLAG(f, SOL_REF)) {
            ASSERT0(dumgr->verifyMDRef());
        }
        if (HAVE_FLAG(f, SOL_AVAIL_EXPR)) {
            ASSERT0(dumgr->verifyLiveinExp());
        }
    }

    if (opts.is_contain(PASS_DU_CHAIN) && !OC_is_du_chain_valid(*oc)) {
        ASSERT(dumgr, ("Alias Analysis is not enable"));
        dumgr->computeMDDUChain(*oc);
    }

    if (opts.is_contain(PASS_EXPR_TAB) && !OC_is_expr_tab_valid(*oc)) {
        IR_EXPR_TAB * exprtab =
            (IR_EXPR_TAB*)passmgr->registerPass(PASS_EXPR_TAB);
        ASSERT0(exprtab);
        exprtab->reperform(*oc);
    }

    if (opts.is_contain(PASS_LOOP_INFO) && !OC_is_loopinfo_valid(*oc)) {
        ASSERT(cfg, ("CFG is not enable"));
        cfg->LoopAnalysis(*oc);
    }

    if (opts.is_contain(PASS_RPO)) {
        ASSERT(cfg, ("CFG is not enable"));
        if (!OC_is_rpo_valid(*oc)) {
            cfg->compute_rpo(*oc);
        } else {
            ASSERT0(cfg->get_bblist_in_rpo()->get_elem_count() ==
                     get_bb_list()->get_elem_count());
        }
    }
}


bool Region::partitionRegion()
{
    //----- DEMO CODE ----------
    IR * ir = get_ir_list();
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
        ir = IR_next(ir);
    }
    if (ir == NULL) return false;
    ir = IR_next(ir);
    while (ir != NULL) {
        if (ir->is_label()) {
            LabelInfo const* li = LAB_lab(ir);
            if (LABEL_INFO_type(li) == L_CLABEL &&
                strcmp(SYM_name(LABEL_INFO_name(li)), "REGION_END") == 0) {
                end_pos = ir;
                break;
            }
        }
        ir = IR_next(ir);
    }
    if (start_pos == NULL || end_pos == NULL) return false;
    ASSERT0(start_pos != end_pos);
    //----------------

    //-----------
    //Generate IR region.
    CHAR b[64];
    sprintf(b, "inner_ru");
    Type const* type = get_type_mgr()->getMCType(0);
    VAR * ruv = get_var_mgr()->registerVar(b, type, 1, VAR_LOCAL|VAR_FAKE);
    VAR_allocable(ruv) = false;
    addToVarTab(ruv);

    Region * inner_ru = get_region_mgr()->allocRegion(RU_SUB);
    inner_ru->set_ru_var(ruv);
    IR * ir_ru = buildRegion(inner_ru);
    copyDbx(ir, ir_ru, inner_ru);
    //------------

    ir = IR_next(start_pos);
    while (ir != end_pos) {
        IR * t = ir;
        ir = IR_next(ir);
        remove(&REGION_analysis_instrument(this)->m_ir_list, t);
        IR * inner_ir = inner_ru->dupIRTree(t);
        freeIRTree(t);
        inner_ru->addToIRList(inner_ir);
    }
    dump_irs(inner_ru->get_ir_list(), get_type_mgr());
    insertafter_one(&start_pos, ir_ru);
    dump_irs(get_ir_list(), get_type_mgr());
    //-------------

    REGION_ru(ir_ru)->process();
    dump_irs(get_ir_list(), get_type_mgr());

    /* Merger IR list in inner-region to outer region.
    remove(&REGION_analysis_instrument(this)->m_ir_list, ir_ru);
    IR * head = inner_ru->constructIRlist();
    insertafter(&split_pos, dupIRTreeList(head));
    dump_irs(get_ir_list()); */

    delete inner_ru;
    return false;
}
//END Region

} //namespace xoc
