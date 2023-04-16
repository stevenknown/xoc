/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

#ifdef _DEBUG_
static bool isReduction(IR const* ir)
{
    DUMMYUSE(isReduction);
    ASSERT0(ir->is_stmt());
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


static bool checkLogicalOp(IR_CODE irc, Type const* type, TypeMgr * tm)
{
    switch (irc) {
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
#endif

IRMgr::IRMgr(Region * rg) : Pass(rg)
{
    m_ir_count = IRID_UNDEF + 1;
    m_ir_pool = xcom::smpoolCreate(256, MEM_COMM);
    ::memset(m_free_tab, 0, sizeof(m_free_tab));
    m_tm = rg->getTypeMgr();
    m_rm = rg->getRegionMgr();
    m_vm = rg->getVarMgr();
    m_init_placeholder_var = nullptr;
}


IRMgr::~IRMgr()
{
    xcom::smpoolDelete(m_ir_pool);
    m_ir_pool = nullptr;
}


IR * IRMgr::xmalloc(UINT size)
{
    ASSERTN(m_ir_pool, ("pool does not initialized"));
    IR * p = (IR*)xcom::smpoolMalloc(size, m_ir_pool);
    ASSERT0(p != nullptr);
    ::memset(p, 0, size);
    return p;
}


size_t IRMgr::count_mem() const
{
    //Because analysis_instrument is pointer, sizeof(Region) does
    //not contain its class size.
    size_t count = sizeof(IRMgr);
    ASSERT0(m_ir_pool);
    count += xcom::smpoolGetPoolSize(m_ir_pool);
    return count;
}


bool IRMgr::dump() const
{
    Region const* rg = getRegion();
    if (!rg->isLogMgrInit()) { return true; }
    note(rg, "\n==---- DUMP ALL IR INFO ----==");
    IRMgr * pthis = const_cast<IRMgr*>(this);
    UINT n = pthis->getIRVec().get_elem_count();
    rg->getLogMgr()->incIndent(2);
    UINT num_has_du = 0;

    //Dump which IR has allocate DU structure.
    for (VecIdx i = IRID_UNDEF; ((UINT)i) < n; i++) {
        IR * ir = pthis->getIRVec().get(i);
        //IRMgr may allocate IR id with a given start index.
        //ASSERT0(ir);
        if (ir == nullptr) { continue; }
        DU * du = ir->getDU();
        if (du != nullptr) {
            num_has_du++;
        }
    }
    if (n > 0) {
        note(rg, "\nTotal IR %d, total DU allocated %d, rate:(%.1f)%%",
             n, num_has_du, (float)num_has_du / (float)n * 100);
    }

    //Dump IR dispers in free tab.
    note(rg, "\n==---- DUMP IR DISPERSED IN FREE TAB ----==");
    for (UINT w = 0; w < MAX_OFFSET_AT_FREE_TABLE + 1; w++) {
        IR * lst = pthis->getFreeTabIRHead(w);
        note(rg, "\nbyte(%d)", (INT)(w + sizeof(IR)));
        if (lst == nullptr) { continue; }
        UINT num = 0;
        IR * p = lst;
        while (p != nullptr) { p = p->get_next(); num++; }
        prt(rg, ", num%d : ", num);
        while (lst != nullptr) {
            prt(rg, "%s", IRNAME(lst));
            lst = IR_next(lst);
            if (lst != nullptr) {
                prt(rg, ", ");
            }
        }
    }
    note(rg, "\n==---- DUMP IR ALLOCATED ----==");
    StrBuf buf(64); //record data-type.
    for (VecIdx i = IRID_UNDEF; ((UINT)i) < n; i++) {
        IR * ir = pthis->getIRVec().get(i);
        //IRMgr may allocate IR id with a given start index.
        //ASSERT0(ir);
        if (ir == nullptr) { continue; }
        Type const* d = nullptr;
        if (!ir->is_undef()) {
            d = IR_dt(ir);
            ASSERT0(d);
            if (d == nullptr) {
                note(rg, "\nid(%d): %s 0x%.8x", ir->id(), IRNAME(ir), ir);
            } else {
                buf.clean();
                note(rg, "\nid(%d): %s r:%s 0x%.8x",
                     ir->id(), IRNAME(ir), m_tm->dump_type(d, buf), ir);
            }
        } else {
            note(rg, "\nid(%d): undef 0x%.8x", ir->id(), ir);
        }
        DU * du = ir->getDU();
        if (du != nullptr) {
            prt(rg, " has du");
        }
    }
    rg->getLogMgr()->decIndent(2);
    return true;
}


void IRMgr::freeIR(IR * ir)
{
    ASSERTN(ir && ir->is_single(), ("chain list should be cut off"));
    #ifdef _DEBUG_
    ASSERT0(!m_has_been_freed_irs.is_contain(ir->id()));
    m_has_been_freed_irs.bunion(ir->id());
    #endif

    //Zero clearing all data fields, except the IRID and CodeSize.
    UINT res_id = ir->id();
    UINT res_irc_sz = IR::getIRCodeSize(ir);
    ::memset(ir, 0, res_irc_sz);
    IR_id(ir) = res_id;
    IR::setIRCodeSize(ir, res_irc_sz);

    //Insert ir into freetab.
    UINT idx = res_irc_sz - sizeof(IR);
    IR * head = m_free_tab[idx];
    if (head != nullptr) {
        IR_next(ir) = head;
        IR_prev(head) = ir;
    }
    m_free_tab[idx] = ir;
}


//Generate IR, invoke freeIR() or freeIRTree() if it is useless.
//NOTE: Do NOT invoke ::free() to free IR, because all
//    IR are allocated in the pool.
IR * IRMgr::allocIR(IR_CODE irc)
{
    bool lookup = false; //lookup freetab will save more memory, but slower.
    #ifndef CONST_IRC_SZ
    //If one is going to lookup freetab, IR_irc_size() must be defined.
    ASSERT0(!lookup);
    #endif
    return allocIR(irc, lookup);
}


IR * IRMgr::allocIR(IR_CODE irc, bool lookup)
{
    UINT idx = IRCSIZE(irc) - sizeof(IR);
    ASSERTN(idx < 1000, ("weird index"));
    IR * ir = pickFreeIR(idx, lookup);
    if (ir == nullptr) {
        ir = xmalloc(IRCSIZE(irc));
        IR_id(ir) = m_ir_count;
        m_ir_count++;
        getIRVec().set(ir->id(), ir);
        IR::setIRCodeSize(ir, IRCSIZE(irc));
    } else {
        ASSERT0(ir->get_prev() == nullptr);
        IR_next(ir) = nullptr;
        #ifdef _DEBUG_
        m_has_been_freed_irs.diff(ir->id());
        #endif
    }
    IR_code(ir) = irc;
    return ir;
}


IR * IRMgr::pickFreeIR(UINT idx, bool lookup)
{
    ASSERT0(idx < MAX_OFFSET_AT_FREE_TABLE + 1);
    if (lookup) {
        for (; idx <= MAX_OFFSET_AT_FREE_TABLE; idx++) {
            IR * ir = m_free_tab[idx];
            if (ir == nullptr) { continue; }
            m_free_tab[idx] = ir->get_next();
            if (ir->get_next() != nullptr) {
                IR_prev(ir->get_next()) = nullptr;
            }
            return ir;
        }
        return nullptr;
    }
    IR * ir = m_free_tab[idx];
    if (ir != nullptr) {
        m_free_tab[idx] = ir->get_next();
        if (ir->get_next() != nullptr) {
            IR_prev(ir->get_next()) = nullptr;
        }
    }
    return ir;
}


void IRMgr::dumpFreeTab() const
{
    Region const* rg = getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP Region Free Table --==");
    for (UINT i = 0; i <= MAX_OFFSET_AT_FREE_TABLE; i++) {
        IR * lst = m_free_tab[i];
        if (lst == nullptr) { continue; }
        UINT sz = i + sizeof(IR);
        UINT count = 0;
        for (IR * ir = lst; ir != nullptr; ir = ir->get_next()) {
            count++;
        }
        note(rg, "\nirsize(%d), num(%d):", sz, count);
        for (IR * ir = lst; ir != nullptr; ir = ir->get_next()) {
            ASSERT0(IR::getIRCodeSize(ir) == sz);
            prt(rg, "ir(%d),", ir->id());
        }
    }
}


Var * IRMgr::genInitPlaceHolderVar()
{
    if (m_init_placeholder_var == nullptr) {
        ASSERT0(m_vm);
        m_init_placeholder_var = m_vm->registerVar("#init_placeholder",
                                                   m_tm->getAny(), 1,
                                                   VAR_LOCAL|VAR_FAKE);
    }
    return m_init_placeholder_var;
}


//Build IR_PR operation by specified prno and type id.
IR * IRMgr::buildPRdedicated(PRNO prno, Type const* type)
{
    ASSERT0(type);
    IR * ir = allocIR(IR_PR);
    PR_no(ir) = prno;
    IR_dt(ir) = type;
    if (g_generate_var_for_pr) {
        m_rg->genVarForPR(PR_no(ir), type);
    }
    return ir;
}


//Build IR_PR operation by specified type id.
IR * IRMgr::buildPR(Type const* type)
{
    ASSERT0(type);
    return buildPRdedicated(buildPrno(type), type);
}


//Generate a PR number by specified prno and type id.
//This operation will allocate new PR number.
//Note the function does NOT generate Var for generated PR no.
UINT IRMgr::buildPrno(Type const* type)
{
    ASSERT0(type);
    DUMMYUSE(type);
    UINT prno = m_rg->getAnalysisInstrument()->m_pr_count;
    m_rg->getAnalysisInstrument()->m_pr_count++;
    return prno;
}


//Build IR_LNOT operation.
IR * IRMgr::buildLogicalNot(IR * opnd0)
{
    return buildUnaryOp(IR_LNOT, m_tm->getBool(), opnd0);
}


//Build Logical operations, include IR_LAND, IR_LOR, IR_XOR.
IR * IRMgr::buildLogicalOp(IR_CODE irc, IR * opnd0, IR * opnd1)
{
    IR * ir = allocIR(irc);
    ASSERT0(opnd0 && opnd1);
    ASSERT0(irc == IR_LAND || irc == IR_LOR || irc == IR_XOR);
    BIN_opnd0(ir) = opnd0;
    BIN_opnd1(ir) = opnd1;
    IR_parent(opnd0) = ir;
    IR_parent(opnd1) = ir;
    IR_dt(ir) = m_tm->getBool();
    return ir;
}


//Build IR_ID operation.
IR * IRMgr::buildId(Var * var)
{
    ASSERT0(var);
    IR * ir = allocIR(IR_ID);
    ASSERT0(var != nullptr);
    ID_info(ir) = var;
    IR_dt(ir) = VAR_type(var);
    return ir;
}


IR * IRMgr::buildLdaString(CHAR const* varname, CHAR const * string)
{
    return buildLdaString(varname, m_rm->addToSymbolTab(string));
}


IR * IRMgr::buildLdaString(CHAR const* varname, Sym const* string)
{
    ASSERT0(string);
    Var * v = m_vm->registerStringVar(varname, string, MEMORY_ALIGNMENT);
    return buildLda(v);
}


//Build IR_LDA operation.
IR * IRMgr::buildLda(Var * var)
{
    ASSERT0(var);
    IR * ir = allocIR(IR_LDA);
    LDA_idinfo(ir) = var;
    if (var->is_any()) {
        IR_dt(ir) = m_tm->getPointerType(1);
    } else {
        IR_dt(ir) = m_tm->getPointerType(
            var->getByteSize(m_tm));
    }
    return ir;
}


//Build IR_CONST operation.
//The result IR indicates a string.
IR * IRMgr::buildString(Sym const* strtab)
{
    ASSERT0(strtab);
    IR * str = allocIR(IR_CONST);
    CONST_str_val(str) = strtab;
    IR_dt(str) = m_tm->getSimplexTypeEx(D_STR);
    return str;
}


//Build conditionally selected expression.
//The result depends on the predicator's value.
//e.g: x = a > b ? 10 : 100
//Note predicator may not be judgement expression.
IR * IRMgr::buildSelect(IR * pred, IR * true_exp, IR * false_exp,
                        Type const* type)
{
    ASSERT0(type);
    ASSERT0(pred && pred->is_single() && true_exp && false_exp);
    ASSERT0(true_exp->is_exp() && true_exp->is_single());
    ASSERT0(false_exp->is_exp() && false_exp->is_single());

    //Type of true exp may be not equal to false exp.
    //ASSERT0(true_exp->getType() == false_exp->getType());
    IR * ir = allocIR(IR_SELECT);
    IR_dt(ir) = type;
    SELECT_det(ir) = pred;
    SELECT_trueexp(ir) = true_exp;
    SELECT_falseexp(ir) = false_exp;

    IR_parent(pred) = ir;
    IR_parent(true_exp) = ir;
    IR_parent(false_exp) = ir;
    return ir;
}


//Build IR_LABEL operation.
IR * IRMgr::buildIlabel()
{
    IR * ir = allocIR(IR_LABEL);
    IR_dt(ir) = m_tm->getAny();
    LAB_lab(ir) = m_rg->genILabel();
    return ir;
}


//Build IR_LABEL operation.
IR * IRMgr::buildLabel(LabelInfo const* li)
{
    ASSERT0(li && LABELINFO_type(li) != L_UNDEF);
    IR * ir = allocIR(IR_LABEL);
    IR_dt(ir) = m_tm->getAny();
    LAB_lab(ir) = li;
    return ir;
}


//Build IR_CVT operation.
//exp: the expression to be converted.
//tgt_ty: the target type that you want to convert.
IR * IRMgr::buildCvt(IR * exp, Type const* tgt_ty)
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
//res: result pr of PHI.
IR * IRMgr::buildPhi(PRNO prno, Type const* type, UINT num_opnd)
{
    ASSERT0(type);
    ASSERT0(prno != PRNO_UNDEF);
    IR * ir = allocIR(IR_PHI);
    PHI_prno(ir) = prno;
    IR_dt(ir) = type;

    IR * last = nullptr;
    for (UINT i = 0; i < num_opnd; i++) {
        IR * x = buildPRdedicated(prno, type);
        xcom::add_next(&PHI_opnd_list(ir), &last, x);
        IR_parent(x) = ir;
    }
    return ir;
}


//Build IR_PHI operation.
//res: result pr of PHI.
IR * IRMgr::buildPhi(PRNO prno, Type const* type, IR * opnd_list)
{
    ASSERT0(type);
    ASSERT0(prno != PRNO_UNDEF);
    IR * ir = allocIR(IR_PHI);
    PHI_prno(ir) = prno;
    IR_dt(ir) = type;
    for (IR * opnd = opnd_list; opnd != nullptr; opnd = opnd->get_next()) {
        ASSERT0(opnd->is_pr() || opnd->is_const());
        IR_parent(opnd) = ir;
    }
    PHI_opnd_list(ir) = opnd_list;
    return ir;
}


//Build IR_CALL operation.
//res_list: reture value list.
//result_prno: indicate the result PR which hold the return value.
//    0 means the call does not have a return value.
//type: result PR data type.
IR * IRMgr::buildCall(Var * callee, IR * param_list, UINT result_prno,
                      Type const* type)
{
    ASSERT0(type);
    ASSERT0(callee);
    IR * ir = allocIR(IR_CALL);
    CALL_param_list(ir) = param_list;
    CALL_prno(ir) = result_prno;
    CALL_idinfo(ir) = callee;
    IR_dt(ir) = type;
    while (param_list != nullptr) {
        IR_parent(param_list) = ir;
        param_list = IR_next(param_list);
    }
    return ir;
}


IR * IRMgr::buildInitPlaceHolder(IR * exp)
{
    Var * placeholder_var = genInitPlaceHolderVar();
    ASSERT0(placeholder_var);
    IR * call = buildCall(placeholder_var, exp);
    CALL_is_intrinsic(call) = true;
    return call;
}


//Build IR_ICALL operation.
//res_list: reture value list.
//result_prno: indicate the result PR which hold the return value.
//    0 means the call does not have a return value.
//type: result PR data type.
//    0 means the call does not have a return value.
IR * IRMgr::buildICall(IR * callee, IR * param_list, UINT result_prno,
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
    while (param_list != nullptr) {
        IR_parent(param_list) = ir;
        param_list = IR_next(param_list);
    }
    return ir;
}


//Build IR_REGION operation.
IR * IRMgr::buildRegion(Region * rg)
{
    ASSERT0(rg && !rg->is_undef());
    ASSERTN(rg->getRegionVar(), ("region should bond with a variable"));
    IR * ir = allocIR(IR_REGION);
    IR_dt(ir) = m_tm->getAny();
    REGION_ru(ir) = rg;
    ASSERT0(rg != m_rg);
    REGION_parent(rg) = m_rg;
    #ifdef _DEBUG_
    if (rg->is_function()) {
        ASSERTN(m_rg->is_program() || m_rg->is_function(),
                ("Only program or function region can have a"
                 " function region as subregion."));
    }
    #endif
    return ir;
}


//Build IR_IGOTO unconditional multi-branch operation.
//vexp: expression to determine which case entry will be target.
//case_list: case entry list. case entry is consist of expression and label.
IR * IRMgr::buildIgoto(IR * vexp, IR * case_list)
{
    ASSERT0(vexp && vexp->is_exp());
    ASSERT0(case_list);

    IR * ir = allocIR(IR_IGOTO);
    IR_dt(ir) = m_tm->getAny();
    IGOTO_vexp(ir) = vexp;
    IGOTO_case_list(ir) = case_list;
    IR_parent(vexp) = ir;

    IR * c = case_list;
    while (c != nullptr) {
        ASSERT0(c->is_case());
        IR_parent(c) = ir;
        c = c->get_next();
    }
    return ir;
}


//Build IR_GOTO operation.
IR * IRMgr::buildGoto(LabelInfo const* li)
{
    ASSERT0(li);
    IR * ir = allocIR(IR_GOTO);
    IR_dt(ir) = m_tm->getAny();
    ASSERT0(li != nullptr);
    GOTO_lab(ir) = li;
    return ir;
}


//Build IR_LD operation.
//Load value from variable with type 'type'.
//var: indicates the variable which value will be loaded.
//ofst: memory byte offset relative to var.
//type: result type of value.
IR * IRMgr::buildLoad(Var * var, TMWORD ofst, Type const* type)
{
    ASSERT0(type);
    ASSERT0(var);
    IR * ir = allocIR(IR_LD);
    LD_idinfo(ir) = var;
    LD_ofst(ir) = ofst;
    IR_dt(ir) = type;
    if (!g_is_hoist_type) { return ir; }

    //Hoisting I16/U16/I8/U8 to I32, to utilize whole register.
    DATA_TYPE dt = ir->getDType();
    if (IS_SIMPLEX(dt)) {
        //Hoist data-type from less than INT to INT.
        IR_dt(ir) = m_tm->getSimplexTypeEx(m_tm->hoistDtype(dt));
    }
    return ir;
}


//Build IR_ILD operation.
//Result is either register or memory chunk, and the size of ILD
//result equals to 'pointer_base_size' of 'addr'.
//base: memory address of ILD.
//ptbase_or_mc_size: if result of ILD is pointer, this parameter records
//    pointer_base_size; or if result is memory chunk, it records
//    the size of memory chunk.
//NOTICE: The ofst of ILD requires to maintain when after return.
IR * IRMgr::buildILoad(IR * base, Type const* type)
{
    ASSERT0(type);
    ASSERTN(base && base->isPtr(), ("mem-address of ILD must be pointer"));
    IR * ir = allocIR(IR_ILD);
    IR_dt(ir) = type;
    ILD_base(ir) = base;
    IR_parent(base) = ir;
    return ir;
}


//Build IR_ILD operation.
IR * IRMgr::buildILoad(IR * base, TMWORD ofst, Type const* type)
{
    ASSERT0(type);
    IR * ir = buildILoad(base, type);
    ILD_ofst(ir) = ofst;
    return ir;
}


//Build store operation to get value from 'base', and store the result PR.
//prno: result prno.
//type: data type of targe pr.
//offset: byte offset to the start of PR.
//base: hold the value that expected to extract.
IR * IRMgr::buildGetElem(PRNO prno, Type const* type, IR * base, IR * offset)
{
    ASSERT0(type && offset && base && prno != PRNO_UNDEF && base->is_exp());
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
//type: data type of targe pr.
//offset: byte offset to the start of rhs PR.
//base: hold the value that expected to extract.
IR * IRMgr::buildGetElem(Type const* type, IR * base, IR * offset)
{
    ASSERT0(type && base && base->is_exp());
    IR * ir = buildGetElem(m_rg->getAnalysisInstrument()->m_pr_count,
                           type, base, offset);
    m_rg->getAnalysisInstrument()->m_pr_count++;
    return ir;
}


//Build store operation to store 'rhs' to store value to be one of the
//element of a PR.
//prno: target prno.
//type: data type of targe pr.
//base: base of source.
//value: value that need to be set.
//offset: byte offset to the start of result PR.
//rhs: value expected to store.
IR * IRMgr::buildSetElem(PRNO prno, Type const* type, IR * base, IR * val,
                         IR * offset)
{
    ASSERT0(type && offset && val && prno != PRNO_UNDEF && val->is_exp());
    IR * ir = allocIR(IR_SETELEM);
    SETELEM_prno(ir) = prno;
    SETELEM_base(ir) = base;
    SETELEM_val(ir) = val;
    SETELEM_ofst(ir) = offset;
    IR_dt(ir) = type;
    IR_parent(base) = ir;
    IR_parent(val) = ir;
    IR_parent(offset) = ir;
    return ir;
}


//Build store operation to store 'rhs' to store value to be one of the
//element of a PR.
//type: data type of targe pr.
//offset: byte offset to the start of result PR.
//rhs: value expected to store.
IR * IRMgr::buildSetElem(Type const* type, IR * base, IR * val, IR * offset)
{
    ASSERT0(type && base && val && val->is_exp() &&
            offset && offset->is_exp());
    IR * ir = buildSetElem(m_rg->getAnalysisInstrument()->m_pr_count,
                           type, base, val, offset);
    m_rg->getAnalysisInstrument()->m_pr_count++;
    return ir;
}


//Build store operation to store 'rhs' to new pr with type and prno.
//prno: target prno.
//type: data type of targe pr.
//rhs: value expected to store.
IR * IRMgr::buildStorePR(PRNO prno, Type const* type, IR * rhs)
{
    ASSERT0(type && prno != PRNO_UNDEF && rhs && rhs->is_exp());
    IR * ir = allocIR(IR_STPR);
    STPR_no(ir) = prno;
    STPR_rhs(ir) = rhs;
    IR_dt(ir) = type;
    IR_parent(rhs) = ir;
    return ir;
}


//Build store operation to store 'rhs' to new pr with type.
//type: data type of targe pr.
//rhs: value expected to store.
IR * IRMgr::buildStorePR(Type const* type, IR * rhs)
{
    ASSERT0(type && rhs && rhs->is_exp());
    IR * ir = buildStorePR(m_rg->getAnalysisInstrument()->m_pr_count,
                           type, rhs);
    m_rg->getAnalysisInstrument()->m_pr_count++;
    return ir;
}


//Build IR_ST operation.
//lhs: memory variable, described target memory location.
//rhs: value expected to store.
IR * IRMgr::buildStore(Var * lhs, IR * rhs)
{
    ASSERT0(lhs && rhs);
    return buildStore(lhs, VAR_type(lhs), rhs);
}


//Build IR_ST operation.
//lhs: target memory location.
//type: result data type.
//rhs: value expected to store.
IR * IRMgr::buildStore(Var * lhs, Type const* type, IR * rhs)
{
    ASSERT0(type);
    ASSERT0(lhs && rhs && rhs->is_exp());
    ASSERTN(!lhs->is_readonly(), ("can not write readonly variable"));
    IR * ir = allocIR(IR_ST);
    ST_idinfo(ir) = lhs;
    ST_rhs(ir) = rhs;
    IR_dt(ir) = type;
    IR_parent(rhs) = ir;
    return ir;
}


//Build IR_ST operation.
//lhs: target memory location.
//type: result data type.
//ofst: memory byte offset relative to lhs.
//rhs: value expected to store.
IR * IRMgr::buildStore(Var * lhs, Type const* type, TMWORD ofst, IR * rhs)
{
    ASSERT0(type);
    IR * ir = buildStore(lhs, type, rhs);
    ST_ofst(ir) = ofst;
    return ir;
}


//Build IR_IST operation.
IR * IRMgr::buildIStore(IR * base, IR * rhs, TMWORD ofst, Type const* type)
{
    ASSERT0(type);
    IR * ir = buildIStore(base, rhs, type);
    IST_ofst(ir) = ofst;
    return ir;
}


//Build IR_IST operation.
//lhs: target memory location pointer.
//rhs: value expected to store.
//type: result type of indirect memory operation, note type is not the
//data type of lhs.
IR * IRMgr::buildIStore(IR * base, IR * rhs, Type const* type)
{
    ASSERT0(type);
    ASSERT0(base && rhs && rhs->is_exp());
    ASSERTN(base->isPtr(), ("must be pointer"));
    IR * ir = allocIR(IR_IST);
    IR_dt(ir) = type;
    IST_base(ir) = base;
    IST_rhs(ir) = rhs;
    IR_parent(base) = ir;
    IR_parent(rhs) = ir;
    return ir;
}


//Build IR_ARRAY operation.
//base: base of array operation, it is either LDA or pointer.
//sublist: subscript expression list.
//type: result type of array operator.
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
//elem_tyid: record element-data-type.
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
//dims: indicate the array dimension.
//elem_num: point to an integer array that indicate
//    the number of element for each dimension. The length of the integer
//    array should be equal to 'dims'.
//    e.g: int g[12][24];
//        elem_num points to an array with 2 value, [12, 24].
//        the 1th dimension has 12 elements, and the 2th dimension has 24
//        elements, which element type is D_I32.
IR * IRMgr::buildArray(IR * base, IR * sublist, Type const* type,
                       Type const* elemtype, UINT dims,
                       TMWORD const* elem_num_buf)
{
    ASSERT0(type);
    ASSERT0(base && sublist && elemtype);
    ASSERT0(base->is_exp() && base->isPtr());
    CArray * ir = (CArray*)allocIR(IR_ARRAY);
    IR_dt(ir) = type;
    ARR_base(ir) = base;
    IR_parent(base) = ir;
    ARR_sub_list(ir) = sublist;
    UINT n = 0;
    for (IR * p = sublist; p != nullptr; p = p->get_next()) {
        IR_parent(p) = ir;
        n++;
    }
    ASSERT0(n == dims);
    ARR_elemtype(ir) = elemtype;

    if (elem_num_buf != nullptr) {
        UINT l = sizeof(TMWORD) * dims;
        TMWORD * ebuf = (TMWORD*)xmalloc(l);
        ::memcpy(ebuf, elem_num_buf, l);
        ARR_elem_num_buf(ir) = ebuf;
    }
    return ir;
}


//Build IR_STARRAY operation.
//base: base of array operation, it is either LDA or pointer.
//sublist: subscript expression list.
//type: result type of array operator.
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
//elem_tyid: record element-data-type.
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
//dims: indicate the array dimension.
//elem_num: point to an integer array that indicate
//    the number of element for in dimension.
//    The length of the integer array should be equal to 'dims'.
//    e.g: int g[12][24];
//        elem_num points to an array with 2 value, [12, 24].
//        the 1th dimension has 12 elements, and the 2th dimension has 24
//        elements, which element type is D_I32.
//    Note the parameter may be nullptr.
//rhs: value expected to store.
IR * IRMgr::buildStoreArray(IR * base, IR * sublist, Type const* type,
                            Type const* elemtype, UINT dims,
                            TMWORD const* elem_num_buf, IR * rhs)
{
    ASSERT0(base && sublist && type);
    ASSERT0(base->is_exp() && base->isPtr());
    ASSERT0(rhs && rhs->is_exp());
    ASSERT0(allBeExp(sublist));
    CStArray * ir = (CStArray*)allocIR(IR_STARRAY);
    IR_dt(ir) = type;
    ARR_base(ir) = base;
    if (base->is_lda()) {
        ASSERTN(!LDA_idinfo(base)->is_readonly(),
                ("can not write readonly variable"));
    }
    IR_parent(base) = ir;
    ARR_sub_list(ir) = sublist;
    UINT n = 0;
    for (IR * p = sublist; p != nullptr; p = p->get_next()) {
        IR_parent(p) = ir;
        n++;
    }
    ASSERT0(n == dims);
    ARR_elemtype(ir) = elemtype;

    if (elem_num_buf != nullptr) {
        UINT l = sizeof(TMWORD) * dims;
        TMWORD * ebuf = (TMWORD*)xmalloc(l);
        ::memcpy(ebuf, elem_num_buf, l);
        ARR_elem_num_buf(ir) = ebuf;
    }
    STARR_rhs(ir) = rhs;
    IR_parent(rhs) = ir;
    return ir;
}



//Build IR_RETURN operation.
IR * IRMgr::buildReturn(IR * retexp)
{
    IR * ir = allocIR(IR_RETURN);
    IR_dt(ir) = m_tm->getAny();
    RET_exp(ir) = retexp;
    if (retexp != nullptr) {
        ASSERT0(retexp->is_exp());
        ASSERT0(IR_next(retexp) == nullptr);
        ASSERT0(IR_prev(retexp) == nullptr);
        IR_parent(retexp) = ir;
    }
    return ir;
}


//Build IR_CONTINUE operation.
IR * IRMgr::buildContinue()
{
    IR * ir = allocIR(IR_CONTINUE);
    IR_dt(ir) = m_tm->getAny();
    return ir;
}


//Build IR_BREAK operation.
IR * IRMgr::buildBreak()
{
    IR * ir = allocIR(IR_BREAK);
    IR_dt(ir) = m_tm->getAny();
    return ir;
}


//Build IR_CASE operation.
IR * IRMgr::buildCase(IR * casev_exp, LabelInfo const* jump_lab)
{
    ASSERT0(casev_exp && jump_lab);
    ASSERTN(casev_exp->is_const(), ("case value-expression must be const"));
    IR * ir = allocIR(IR_CASE);
    IR_dt(ir) = m_tm->getAny();
    CASE_lab(ir) = jump_lab;
    CASE_vexp(ir) = casev_exp;
    IR_parent(casev_exp) = ir;
    return ir;
}


//Build Do Loop stmt.
//iv: induction variable.
//det: determinate expression.
//loop_body: stmt list.
//init: record the stmt that initialize iv.
//step: record the stmt that update iv.
IR * IRMgr::buildDoLoop(IR * iv, IR * init, IR * det, IR * step, IR * loop_body)
{
    ASSERT0(det &&
            (det->is_lt() ||
             det->is_le() ||
             det->is_gt() ||
             det->is_ge()));
    ASSERT0(init && step && init->is_exp() && step->is_exp());
    ASSERT0(iv->is_id() || iv->is_pr());
    //ASSERT0(isReduction(step));

    IR * ir = allocIR(IR_DO_LOOP);
    IR_dt(ir) = m_tm->getAny();

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
    while (c != nullptr) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build Do While stmt.
//det: determinate expression.
//loop_body: stmt list.
IR * IRMgr::buildDoWhile(IR * det, IR * loop_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = allocIR(IR_DO_WHILE);
    IR_dt(ir) = m_tm->getAny();
    LOOP_det(ir) = det;
    IR_parent(det) = ir;

    LOOP_body(ir) = loop_body;
    IR * c = loop_body;
    while (c != nullptr) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build While Do stmt.
//det: determinate expression.
//loop_body: stmt list.
IR * IRMgr::buildWhileDo(IR * det, IR * loop_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = allocIR(IR_WHILE_DO);
    IR_dt(ir) = m_tm->getAny();
    LOOP_det(ir) = det;
    IR_parent(det) = ir;

    LOOP_body(ir) = loop_body;
    IR * c = loop_body;
    while (c != nullptr) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build IF stmt.
//det: determinate expression.
//true_body: stmt list.
//false_body: stmt list.
IR * IRMgr::buildIf(IR * det, IR * true_body, IR * false_body)
{
    ASSERT0(det && det->is_judge());

    IR * ir = allocIR(IR_IF);
    IR_dt(ir) = m_tm->getAny();
    IF_det(ir) = det;
    IR_parent(det) = ir;

    IF_truebody(ir) = true_body;
    IR * c = true_body;
    while (c != nullptr) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }

    IF_falsebody(ir) = false_body;
    c = false_body;
    while (c != nullptr) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build SWITCH multi-select stmt.
//vexp: expression to determine which case entry will be target.
//case_list: case entry list. case entry is consist of expression and label.
//    Note that case list is optional.
//body: stmt list.
//default_lab: label indicates the default choice, the label is optional.
//
//NOTE: Do not set parent for stmt in 'body'.
IR * IRMgr::buildSwitch(IR * vexp, IR * case_list, IR * body,
                        LabelInfo const* default_lab)
{
    ASSERT0(vexp && vexp->is_exp());
    IR * ir = allocIR(IR_SWITCH);
    IR_dt(ir) = m_tm->getAny();
    SWITCH_vexp(ir) = vexp;
    SWITCH_case_list(ir) = case_list;
    SWITCH_body(ir) = body;
    SWITCH_deflab(ir) = default_lab;
    IR_parent(vexp) = ir;

    IR * c = case_list;
    while (c != nullptr) {
        ASSERT0(c->is_case());
        IR_parent(c) = ir;
        c = c->get_next();
    }

    c = body;
    while (c != nullptr) {
        IR_parent(c) = ir;
        //Do not check if ir is stmt, it will be canonicalized later.
        c = c->get_next();
    }
    return ir;
}


//Build IR_TRUEBR or IR_FALSEBR operation.
IR * IRMgr::buildBranch(bool is_true_br, IR * det, LabelInfo const* lab)
{
    ASSERT0(lab && det && det->is_judge());
    IR * ir;
    if (is_true_br) {
        ir = allocIR(IR_TRUEBR);
    } else {
        ir = allocIR(IR_FALSEBR);
    }
    IR_dt(ir) = m_tm->getAny();
    BR_det(ir) = det;
    BR_lab(ir) = lab;
    IR_parent(det) = ir;
    return ir;
}


//Build IR_CONST operation.
//The expression indicates a float point number.
IR * IRMgr::buildImmFP(HOST_FP fp, Type const* type)
{
    ASSERT0(type);
    IR * imm = allocIR(IR_CONST);
    //Convert string to hex value , that is in order to generate
    //single load instruction to load float point value in Code
    //Generator.
    ASSERT0(type->is_fp());
    CONST_fp_val(imm) = fp;
    CONST_fp_mant(imm) = DEFAULT_MANTISSA_NUM;
    IR_dt(imm) = type;
    return imm;
}


//Build IR_CONST operation.
//The expression indicates value with dynamic type.
IR * IRMgr::buildImmAny(HOST_INT v)
{
    IR * imm = allocIR(IR_CONST);
    CONST_int_val(imm) = (HOST_INT)v;
    IR_dt(imm) = m_tm->getAny();
    return imm;
}


//Build IR_CONST operation.
//The expression indicates an integer.
//v: value of integer.
//type: integer type.
IR * IRMgr::buildImmInt(HOST_INT v, Type const* type)
{
    ASSERT0(type);
    ASSERT0(type->is_int() || type->is_mc() || type->is_pointer() ||
            type->is_ptr_addend());
    IR * imm = allocIR(IR_CONST);
    if (type->is_int()) {
        //Make sure value is sign-extended.
        switch (TY_dtype(type)) {
        case D_B:
        case D_I8:
        case D_U8: {
            UINT8 uv = (UINT8)v;
            if (type->is_unsigned()) {
                CONST_int_val(imm) = (HOST_INT)uv;
            } else {
                INT8 sv = (INT8)uv;
                CONST_int_val(imm) = (HOST_INT)sv;
            }
            break;
        }
        case D_I16:
        case D_U16: {
            UINT16 uv = (UINT16)v;
            if (type->is_unsigned()) {
                CONST_int_val(imm) = (HOST_INT)uv;
            } else {
                INT16 sv = (INT16)uv;
                CONST_int_val(imm) = (HOST_INT)sv;
            }
            break;
        }
        case D_I32:
        case D_U32: {
            UINT32 uv = (UINT32)v;
            if (type->is_unsigned()) {
                CONST_int_val(imm) = (HOST_INT)uv;
            } else {
                INT32 sv = (INT32)uv;
                CONST_int_val(imm) = (HOST_INT)sv;
            }
            break;
        }
        case D_I64:
        case D_U64: {
            UINT64 uv = (UINT64)v;
            if (type->is_unsigned()) {
                CONST_int_val(imm) = (HOST_INT)uv;
            } else {
                INT64 sv = (INT64)uv;
                CONST_int_val(imm) = (HOST_INT)sv;
            }
            break;
        }
        case D_I128:
        case D_U128:
            ASSERTN(0, ("TODO:unsupport 128 bit integer"));
            break;
        case D_ANY:
            //If IR_CONST operation has ANY type, that indicates the constant
            //value with dynamic type.
            ASSERTN(0, ("should invoke buildImmAny()"));
        default: ASSERTN(0, ("TODO:unsupport integer type"));
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
IR * IRMgr::buildPointerOp(IR_CODE irc, IR * lchild, IR * rchild)
{
    ASSERT0(lchild && rchild);
    if (!lchild->is_ptr() && rchild->is_ptr()) {
        ASSERTN(irc == IR_ADD ||
                irc == IR_MUL ||
                irc == IR_XOR ||
                irc == IR_BAND ||
                irc == IR_BOR ||
                irc == IR_LT ||
                irc == IR_GT ||
                irc == IR_LE ||
                irc == IR_GE ||
                irc == IR_EQ ||
                irc == IR_NE, ("illegal pointer operation"));
        ASSERTN(lchild->is_int() || lchild->is_mc() || lchild->is_any(),
                ("illegal pointer addend"));
        return buildPointerOp(irc, rchild, lchild);
    }

    Type const* d0 = lchild->getType();
    Type const* d1 = rchild->getType();
    DUMMYUSE(d1);
    if (lchild->is_ptr() && rchild->is_ptr()) {
        //CASE: Pointer substraction.
        //  char *p, *q;
        //  p-q => t1=p-q, t2=t1/4, return t2
        switch (irc) {
        case IR_SUB: {
            TypeMgr * dm = m_tm;

            //Result is not pointer type.
            ASSERT0(TY_ptr_base_size(d0) > 0);
            ASSERT0(TY_ptr_base_size(d0) == TY_ptr_base_size(d1));
            IR * ret = allocIR(IR_SUB);
            BIN_opnd0(ret) = lchild;
            BIN_opnd1(ret) = rchild;
            IR_dt(ret) = dm->getSimplexTypeEx(dm->getAlignedDType(
                WORD_BITSIZE, true));
            if (TY_ptr_base_size(d0) > BYTE_PER_CHAR) {
                IR * div = allocIR(IR_DIV);
                BIN_opnd0(div) = ret;
                BIN_opnd1(div) = buildImmInt(TY_ptr_base_size(d0),
                                             ret->getType());
                IR_dt(div) = dm->getSimplexTypeEx(
                    dm->getAlignedDType(WORD_BITSIZE, true));
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
        case IR_NE: {
            //Result is not pointer type.
            IR * ret = allocIR(irc);
            BIN_opnd0(ret) = lchild;
            BIN_opnd1(ret) = rchild;
            IR_dt(ret) = m_tm->getSimplexTypeEx(D_B);
            IR_parent(lchild) = ret;
            IR_parent(rchild) = ret;
            return ret;
        }
        default: ASSERTN(0, ("illegal pointers operation"));
        }
        ASSERTN(0, ("can not get here."));
        return nullptr;
    }

    if (lchild->is_ptr() && !rchild->is_ptr()) {
        //Result is pointer type.
        //CASE:
        //  int * p;
        //  p + 4 => t1 = p + (4 * sizeof(BASE_TYPE_OF(p)))
        //  p - 4 => t1 = p - (4 * sizeof(BASE_TYPE_OF(p)))
        switch (irc) {
        case IR_ADD:
        case IR_SUB: {
            IR * addend = allocIR(IR_MUL);
            BIN_opnd0(addend) = rchild;

            ASSERTN(TY_ptr_base_size(d0) > 0, ("multipler is 0"));

            BIN_opnd1(addend) = buildImmInt(TY_ptr_base_size(d0),
                rchild->getType());
            IR_dt(addend) = rchild->getType();

            IR * ret = allocIR(irc); //ADD or SUB
            BIN_opnd0(ret) = lchild; //lchild is pointer.
            BIN_opnd1(ret) = addend; //addend is not pointer.

            //CASE: 'p = p + 1'
            //so the result type of '+' should still be pointer type.
            ret->setPointerType(TY_ptr_base_size(d0), m_tm);

            //Avoid too much boring pointer operations.
            ret->setParentPointer(true);
            return ret;
        }
        default: {
            ASSERT0(irc == IR_LT || irc == IR_LE ||
                    irc == IR_GT || irc == IR_GE ||
                    irc == IR_EQ || irc == IR_NE);

            //Pointer operation may give rise to undefined behavior.
            IR * ret = allocIR(irc);
            BIN_opnd0(ret) = lchild;
            BIN_opnd1(ret) = rchild;
            IR_dt(ret) = m_tm->getSimplexTypeEx(D_B);
            IR_parent(lchild) = ret;
            IR_parent(rchild) = ret;
            return ret;
        }
        }
    }

    UNREACHABLE();
    return nullptr; //just ceases warning.
}


//This function build operation that comparing with 0 by NE node.
//e.g: output is (exp != 0).
//This function always used as helper function to convient to
//generate det-expression if it is not relational/logical.
IR * IRMgr::buildJudge(IR * exp)
{
    ASSERT0(!exp->is_judge());
    Type const* type = exp->getType();
    TypeMgr * dm = m_tm;
    if (exp->is_ptr()) {
        type = dm->getSimplexTypeEx(dm->getPointerSizeDtype());
    }
    if (!type->is_fp() && !type->is_int() && !type->is_mc()) {
        type = dm->getI32();
    }
    return buildCmp(IR_NE, exp, type->is_fp() ?
           buildImmFP(HOST_FP(0), type) : buildImmInt(0, type));
}


//Build comparision operations.
IR * IRMgr::buildCmp(IR_CODE irc, IR * lchild, IR * rchild)
{
    ASSERT0(irc == IR_LAND || irc == IR_LOR ||
            irc == IR_LT || irc == IR_LE ||
            irc == IR_GT || irc == IR_GE ||
            irc == IR_NE || irc == IR_EQ);
    ASSERT0(lchild && rchild && lchild->is_exp() && rchild->is_exp());

    if (lchild->is_const() &&
        !rchild->is_const() &&
        (irc == IR_EQ || irc == IR_NE)) {
        return buildCmp(irc, rchild, lchild);
    }

    IR * ir = allocIR(irc);
    BIN_opnd0(ir) = lchild;
    BIN_opnd1(ir) = rchild;
    IR_dt(ir) = m_tm->getSimplexTypeEx(D_B);
    IR_parent(lchild) = ir;
    IR_parent(rchild) = ir;
    return ir;
}


IR * IRMgr::buildUnaryOp(IR_CODE irc, Type const* type, IN IR * opnd)
{
    ASSERT0(type);
    ASSERT0(isUnaryOp(irc));
    ASSERT0(opnd && opnd->is_exp());
    ASSERT0(irc != IR_LNOT || type->is_bool());
    IR * ir = allocIR(irc);
    UNA_opnd(ir) = opnd;
    IR_dt(ir) = type;
    IR_parent(opnd) = ir;
    return ir;
}


//Build binary operation without considering pointer arithmetic.
IR * IRMgr::buildBinaryOpSimp(IR_CODE irc, Type const* type,
                              IR * lchild, IR * rchild)
{
    ASSERT0(type);
    if (lchild->is_const() && !rchild->is_const() &&
        (irc == IR_ADD ||
         irc == IR_MUL ||
         irc == IR_XOR ||
         irc == IR_BAND ||
         irc == IR_BOR ||
         irc == IR_EQ ||
         irc == IR_NE)) {
        //Swap operands.
        return buildBinaryOpSimp(irc, type, rchild, lchild);
    }

    ASSERT0(lchild && rchild && lchild->is_exp() && rchild->is_exp());
    ASSERT0(checkLogicalOp(irc, type, m_tm));
    IR * ir = allocIR(irc);
    BIN_opnd0(ir) = lchild;
    BIN_opnd1(ir) = rchild;
    IR_parent(lchild) = ir;
    IR_parent(rchild) = ir;
    IR_dt(ir) = type;
    return ir;
}


IR * IRMgr::buildBinaryOp(IR_CODE irc, DATA_TYPE dt, IR * lchild, IR * rchild)
{
    return buildBinaryOp(irc, m_tm->getSimplexType(dt), lchild, rchild);
}


//Build binary operation.
//If rchild/lchild is pointer, the function will attemp to generate pointer
//arithmetic operation instead of normal binary operation.
IR * IRMgr::buildBinaryOp(IR_CODE irc, Type const* type, IR * lchild,
                          IR * rchild)
{
    ASSERT0(type);
    ASSERT0(checkLogicalOp(irc, type, m_tm));
    ASSERT0(lchild && rchild && lchild->is_exp() && rchild->is_exp());
    if (lchild->is_ptr() || rchild->is_ptr()) {
        return buildPointerOp(irc, lchild, rchild);
    }

    #ifdef _DEBUG_
    //Both lchild and rchild are NOT pointer.
    //Generic binary operation.
    if (type->is_mc()) {
        //mc_size records the memory-chunk size if rtype is D_MC, or else is 0.
        ASSERTN(TY_mc_size(type) != 0, ("Size of memory chunck can not be 0"));
        ASSERT0(TY_mc_size(type) == lchild->getTypeSize(m_tm) &&
                TY_mc_size(type) == rchild->getTypeSize(m_tm));
    }
    #endif

    return buildBinaryOpSimp(irc, type, lchild, rchild);
}

} //namespace xoc
