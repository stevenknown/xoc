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

namespace xoc {

#define LEVEL1_BYTE_SIZE (sizeof(ExprRep**) * IR_EXPR_TAB_LEVEL1_HASH_BUCKET)
#define LEVEL2_BYTE_SIZE (sizeof(ExprRep*) * IR_EXPR_TAB_LEVEL2_HASH_BUCKET)

//
//START ExprRep
//
void ExprRep::clean()
{
    EXPR_occ_list(this).clean();
}


void ExprRep::dump(Region const* rg) const
{
    note(rg, "\n-- EXPR_REP(%d)", EXPR_id(this));
    rg->getLogMgr()->incIndent(2);
    dumpIR(EXPR_ir(this), rg);
    note(rg, "\n  OCC:");
    IREListIter eit;
    bool first = true;
    xcom::StrBuf tmp(8);
    for (IR const* occ = EXPR_occ_list(this).get_head(&eit);
         occ != nullptr; occ = EXPR_occ_list(this).get_next(&eit)) {
        if (first) { first = false; }
        else { prt(rg, ","); }
        prt(rg, "%s", dumpIRName(occ, tmp));
        MDSet const* use_mds = occ->getMayRef();
        if (use_mds != nullptr) {
            prt(rg, "(use:");
            use_mds->dump(rg->getMDSystem(), rg->getVarMgr());
            prt(rg, ")");
        }
    }
    rg->getLogMgr()->decIndent(2);
}
//END ExprRep


//
//START ExprTab
//
ExprTab::ExprTab(Region * rg) : Pass(rg)
{
    m_expr_count = 0;
    m_tm = rg->getTypeMgr();
    ASSERT0(sizeof(m_level1_hash_tab) == LEVEL1_BYTE_SIZE);
    ::memset((void*)m_level1_hash_tab, 0, LEVEL1_BYTE_SIZE);
    m_pool = smpoolCreate(sizeof(ExprRep*) * 128, MEM_COMM);
    m_sc_pool = smpoolCreate(sizeof(xcom::SC<ExprRep*>) * 4, MEM_CONST_SIZE);
    m_ir_expr_lst.set_pool(m_sc_pool);
    m_md_set_mgr = rg->getMDSetMgr();
    m_bs_mgr = rg->getBitSetMgr();
}


ExprTab::~ExprTab()
{
    for (xcom::SC<ExprRep*> * sc = m_ir_expr_lst.get_head();
         sc != m_ir_expr_lst.end(); sc = m_ir_expr_lst.get_next(sc)) {
        ExprRep * ie = sc->val();
        ASSERT0(ie);
        delete ie;
    }
    smpoolDelete(m_pool);
    smpoolDelete(m_sc_pool);
}


size_t ExprTab::count_mem() const
{
    size_t count = 0;
    count += sizeof(m_expr_count);
    count += sizeof(m_rg);
    count += m_ir_expr_vec.count_mem();
    count += m_ir_expr_lst.count_mem();
    count += smpoolGetPoolSize(m_pool);
    count += sizeof(m_level1_hash_tab);
    count += m_map_ir2exprep.count_mem();
    return count;
}


void * ExprTab::xmalloc(INT size)
{
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, size);
    return p;
}


void ExprTab::reset()
{
    for (UINT i = 0; i < m_ir_expr_vec.get_elem_count(); i++) {
        ExprRep * ie = m_ir_expr_vec.get(i);
        if (ie == nullptr) { continue; }
        ie->clean();
    }
    cleanHashTab();
}


//Dump all IR expressions of region and its used MDs.
bool ExprTab::dump() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpExprTab()) { return true; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    getRegion()->getLogMgr()->incIndent(2);
    for (UINT i = 0; i < m_ir_expr_vec.get_elem_count(); i++) {
        ExprRep const* ie = m_ir_expr_vec.get(i);
        if (ie == nullptr) { continue; }
        ASSERT0(EXPR_id(ie) == (UINT)i);
        ie->dump(m_rg);
    }
    getRegion()->getLogMgr()->decIndent(2);
    return true;
}


ExprRep * ExprTab::mapIR2ExprRep(IR const* ir) const
{
    if (ir == nullptr) { return nullptr; }
    return m_map_ir2exprep.get(ir->id());
}


void ExprTab::setMapIR2ExprRep(IR const* ir, ExprRep * ie)
{
    m_map_ir2exprep.set(ir->id(), ie);
}


HOST_UINT ExprTab::compute_hash_key(IR const* ir) const
{
    ASSERT0(ir != nullptr);
    HOST_UINT hval = ir->getCode() + (ir->getOffset() + 1) +
        (UINT)(size_t)ir->getType();
    if (ir->isReadPR()) {
        hval += (HOST_UINT)ir->getPrno();
    }
    if (ir->is_id()) {
        Var const* var = ID_info(ir);
        hval += 5 * (UINT)(size_t)var;
    }
    return hval;
}


ExprRep * ExprTab::encodeExtExp(IR * ir)
{
    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_EXP:
        return nullptr;
    default:UNREACHABLE();
    }
    return nullptr;
}


void ExprTab::encodeExtStmt(IR const* ir)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_STMT:
        encodeAllKids(ir);
        break;
    default:UNREACHABLE();
    }
}


HOST_UINT ExprTab::compute_hash_key_for_tree(IR const* ir)
{
    HOST_UINT hval = 0;
    m_iter.clean();
    for (IR const* x = iterInitC(ir, m_iter);
         x != nullptr; x = iterNextC(m_iter)) {
        hval += compute_hash_key(x);
    }
    return hval;
}


void ExprTab::cleanHashTab()
{
    for (UINT i = 0; i < IR_EXPR_TAB_LEVEL1_HASH_BUCKET; i++) {
        ExprRep ** level2_hash_tab = m_level1_hash_tab[i];
        if (level2_hash_tab == nullptr) { continue; }
        ::memset((void*)level2_hash_tab, 0, LEVEL2_BYTE_SIZE);
    }
    ASSERT0(sizeof(m_level1_hash_tab) == LEVEL1_BYTE_SIZE);
    ::memset((void*)m_level1_hash_tab, 0, LEVEL1_BYTE_SIZE);
}


ExprRep * ExprTab::appendExp(IR * ir)
{
    if (ir == nullptr) { return nullptr; }
    HOST_UINT key = compute_hash_key_for_tree(ir);

    //First level hashing.
    HOST_UINT level1_hashv = key % IR_EXPR_TAB_LEVEL1_HASH_BUCKET;
    ExprRep ** level2_hash_tab = m_level1_hash_tab[level1_hashv];
    if (level2_hash_tab == nullptr) {
        //Generate level2
        level2_hash_tab = (ExprRep*(*))xmalloc(LEVEL2_BYTE_SIZE);
        m_level1_hash_tab[level1_hashv] = level2_hash_tab;

        //Generate copy of 'ir'.
        ExprRep * ie = allocExprRep();
        EXPR_id(ie) = ++m_expr_count;
        EXPR_ir(ie) = m_rg->dupIRTree(ir);
        m_ir_expr_vec.set(EXPR_id(ie), ie);

        //Enter into 'ir'
        HOST_UINT level2_hashv = key % IR_EXPR_TAB_LEVEL2_HASH_BUCKET;
        level2_hash_tab[level2_hashv] = ie;
        return ie;
    }

    //Scanning in level2 hash tab.
    HOST_UINT level2_hashv = key % IR_EXPR_TAB_LEVEL2_HASH_BUCKET;
    ExprRep * ie = level2_hash_tab[level2_hashv];
    if (ie == nullptr) {
        //Generate copy of 'ir'.
        ie = allocExprRep();
        EXPR_id(ie) = ++m_expr_count;
        EXPR_ir(ie) = m_rg->dupIRTree(ir);
        m_ir_expr_vec.set(EXPR_id(ie), ie);

        //Enter into 'ir'
        level2_hash_tab[level2_hashv] = ie;
        return ie;
    }

    //Scanning in ExprRep list in level2 hash tab.
    ExprRep * last = nullptr;
    while (ie != nullptr) {
        if (ir->isIREqual(EXPR_ir(ie), getIRMgr())) {
            return ie;
        }
        last = ie;
        ie = EXPR_next(ie);
    }

    //Generate copy of 'ir'.
    ie = allocExprRep();
    EXPR_id(ie) = ++m_expr_count;
    EXPR_ir(ie) = m_rg->dupIRTree(ir);
    m_ir_expr_vec.set(EXPR_id(ie), ie);

    //Enter into 'ir'
    ASSERT0(level2_hash_tab[level2_hashv] != nullptr);
    xcom::insertafter_one(&last, ie);
    return ie;
}


ExprRep * ExprTab::allocExprRep()
{
    ExprRep * ie = new ExprRep();
    m_ir_expr_lst.append_head(ie);
    return ie;
}


IR * ExprTab::removeOcc(IR * occ)
{
    if (occ == nullptr) { return nullptr; }
    ASSERT0(occ->is_exp());
    ExprRep * ie = mapIR2ExprRep(occ);
    if (ie == nullptr) { return nullptr; }
    return EXPR_occ_list(ie).remove(occ);
}


void ExprTab::removeOccs(IR * ir)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_STPR:
    SWITCH_CASE_DIRECT_MEM_STMT: {
        IR * stv = ir->getRHS();
        if (stv != nullptr && stv->is_const()) { return; }
        removeOcc(stv);
        break;
    }
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_INDIRECT_MEM_STMT: {
        IR * stv = ir->getRHS();
        if (stv != nullptr && !stv->is_const()) {
            removeOcc(stv);
        }
        IR * m = ir->getBase();
        if (m->is_const()) { return; }
        removeOcc(m);
        break;
    }
    SWITCH_CASE_CALL:
        for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
            if (!p->is_const()) {
                removeOcc(p);
            }
        }
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        removeOcc(BR_det(ir));
        break;
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    case IR_IGOTO:
        ASSERT0(ir->getValExp());
        if (!ir->getValExp()->is_const()) {
            removeOcc(ir->getValExp());
        }
        break;
    case IR_RETURN:
        if (RET_exp(ir) != nullptr) {
            if (!RET_exp(ir)->is_const()) {
                removeOcc(RET_exp(ir));
            }
        }
        break;
    case IR_GOTO:
    SWITCH_CASE_CFS_OP:
    SWITCH_CASE_LOOP_ITER_CFS_OP:
    case IR_LABEL:
    case IR_CASE:
    case IR_PHI:
        break;
    default: UNREACHABLE();
    }
}


ExprRep * ExprTab::removeExp(IR * ir)
{
    HOST_UINT key = compute_hash_key_for_tree(ir);

    //First level hashing.
    UINT level1_hashv = key % IR_EXPR_TAB_LEVEL1_HASH_BUCKET;
    ExprRep ** level2_hash_tab = m_level1_hash_tab[level1_hashv];
    if (level2_hash_tab == nullptr) {
        return nullptr;
    }

    //Scanning in level2 hash tab.
    HOST_UINT level2_hashv = key % IR_EXPR_TAB_LEVEL2_HASH_BUCKET;
    ExprRep * ie = level2_hash_tab[level2_hashv];
    if (ie == nullptr) {
        return nullptr;
    }

    //Scanning in ExprRep list in level2 hash tab.
    while (ie != nullptr) {
        if (ir->isIREqual(EXPR_ir(ie), getIRMgr())) {
            xcom::remove(&level2_hash_tab[level2_hashv], ie);
            m_ir_expr_vec.remove(EXPR_id(ie), nullptr);
            return ie;
        }
        ie = EXPR_next(ie);
    }
    return nullptr;
}


ExprRep * ExprTab::findExp(IR * ir)
{
    if (ir == nullptr) { return nullptr; }
    HOST_UINT key = compute_hash_key_for_tree(ir);

    //First level hashing.
    UINT level1_hashv = key % IR_EXPR_TAB_LEVEL1_HASH_BUCKET;
    ExprRep ** level2_hash_tab = m_level1_hash_tab[level1_hashv];
    if (level2_hash_tab == nullptr) {
        return nullptr;
    }

    //Scanning in level2 hash tab.
    UINT level2_hashv = key % IR_EXPR_TAB_LEVEL2_HASH_BUCKET;
    ExprRep * ie = level2_hash_tab[level2_hashv];
    if (ie == nullptr) {
        return nullptr;
    }

    //Scanning in ExprRep list in level2 hash tab.
    while (ie != nullptr) {
        if (ir->isIREqual(EXPR_ir(ie), getIRMgr())) {
            return ie;
        }
        ie = EXPR_next(ie);
    }
    return nullptr;
}


ExprRep * ExprTab::encodeExp(IR * ir)
{
    if (ir == nullptr) { return nullptr; }
    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    case IR_ID:
    case IR_LD:
    case IR_ILD:
    case IR_LDA:
    case IR_CONST:
    case IR_DUMMYUSE:
    SWITCH_CASE_READ_PR:
        return nullptr;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
    case IR_ARRAY:
    case IR_SELECT: {
        ExprRep * ie = appendExp(ir);
        ASSERTN(!EXPR_occ_list(ie).find(ir), ("process same IR repeated."));
        EXPR_occ_list(ie).append_tail(ir);
        return ie;
    }
    default: return encodeExtExp(ir);
    }
    return nullptr;
}


void ExprTab::encodeStmt(IR const* ir)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_SETELEM: {
        ExprRep * ie = encodeExp(((CSetElem*)ir)->getBase());
        if (ie != nullptr) {
            setMapIR2ExprRep(((CSetElem*)ir)->getBase(), ie);
        }
        ExprRep * ie2 = encodeExp(((CSetElem*)ir)->getVal());
        if (ie2 != nullptr) {
            setMapIR2ExprRep(((CSetElem*)ir)->getVal(), ie2);
        }
        break;
    }
    case IR_GETELEM: {
        ExprRep * ie = encodeExp(((CGetElem*)ir)->getBase());
        if (ie != nullptr) {
            setMapIR2ExprRep(((CGetElem*)ir)->getBase(), ie);
        }
        break;
    }
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR: {
        ExprRep * ie = encodeExp(ir->getRHS());
        if (ie != nullptr) {
            setMapIR2ExprRep(ir->getRHS(), ie);
        }
        break;
    }
    SWITCH_CASE_ARRAY_OP: {
        ExprRep * ie = encodeExp(ir->getBase());
        if (ie != nullptr) {
            setMapIR2ExprRep(ir->getBase(), ie);
        }
        for (IR * sub = ARR_sub_list(ir);
             sub != nullptr; sub = sub->get_next()) {
            ExprRep * ie2 = encodeExp(sub);
            if (ie2 != nullptr) {
                setMapIR2ExprRep(sub, ie2);
            }
        }
        ie = encodeExp(ir->getRHS());
        if (ie != nullptr) {
            setMapIR2ExprRep(ir->getRHS(), ie);
        }
        break;
    }
    SWITCH_CASE_INDIRECT_MEM_STMT: {
        ExprRep * ie = encodeExp(ir->getRHS());
        if (ie != nullptr) {
            setMapIR2ExprRep(ir->getRHS(), ie);
        }
        ie = encodeBaseOfIST(ir->getBase());
        if (ie != nullptr) {
            setMapIR2ExprRep(ir->getBase(), ie);
        }
        break;
    }
    SWITCH_CASE_CALL: {
        if (ir->is_icall()) {
            ExprRep * ie = encodeExp(ICALL_callee(ir));
            if (ie != nullptr) {
                setMapIR2ExprRep(ICALL_callee(ir), ie);
            }
        }
        for (IR * p = CALL_param_list(ir);
             p != nullptr; p = p->get_next()) {
            ExprRep * ie = encodeExp(p);
            if (ie != nullptr) {
                setMapIR2ExprRep(p, ie);
            }
        }
        break;
    }
    case IR_GOTO:
        break;
    SWITCH_CASE_CFS_OP:
        ASSERTN(0, ("High level IR should be simplified"));
        break;
    case IR_LABEL:
        break;
    case IR_CASE:
    case IR_REGION:
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP: {
        ExprRep * ie = encodeExp(BR_det(ir));
        if (ie != nullptr) {
           setMapIR2ExprRep(BR_det(ir), ie);
        }
        break;
    }
    case IR_IGOTO:
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP: {
        ExprRep * ie = encodeExp(ir->getValExp());
        if (ie != nullptr) {
           setMapIR2ExprRep(ir->getValExp(), ie);
        }
        break;
    }
    case IR_RETURN: {
        ExprRep * ie = encodeExp(RET_exp(ir));
        if (ie != nullptr) {
            setMapIR2ExprRep(RET_exp(ir), ie);
        }
        break;
    }
    case IR_PHI: {
        for (IR * opnd = PHI_opnd_list(ir);
             opnd != nullptr; opnd = opnd->get_next()) {
            ExprRep * ie = encodeExp(opnd);
            if (ie != nullptr) {
                setMapIR2ExprRep(opnd, ie);
            }
        }
        break;
    }
    default: encodeExtStmt(ir);
    }
}


void ExprTab::encodeAllKids(IR const* ir)
{
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        ExprRep * ie = encodeExp(kid);
        if (ie == nullptr) { continue; }
        setMapIR2ExprRep(kid, ie);
    }
}


void ExprTab::encodeBB(IRBB const* bb)
{
    IRListIter ct = nullptr;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != nullptr; ir = BB_irlist(bb).get_next(&ct)) {
        encodeStmt(ir);
    }
}


//Encode expression for a list of BB.
//Scan IR statement literally, and encoding it for generating
//the unique id for each individual expressions, and update
//the 'GEN-SET' and 'KILL-SET' of IR-EXPR for BB as well as.
bool ExprTab::perform(MOD OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl->get_elem_count() == 0) { return false; }
    reset();
    BBListIter cb;
    for (IRBB * bb = bbl->get_head(&cb);
         bb != nullptr; bb = bbl->get_next(&cb)) {
        encodeBB(bb);
    }
    set_valid(true);
    if (g_dump_opt.isDumpAfterPass()) {
        dump();
    }
    return false;
}
//END ExprTab

} //namespace xoc
