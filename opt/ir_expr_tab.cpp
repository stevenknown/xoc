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

//
//START ExprTab
//
ExprTab::ExprTab(Region * rg) : Pass(rg)
{
    m_expr_count = 0;
    m_tm = rg->getTypeMgr();
    ::memset(m_level1_hash_tab, 0,
        sizeof(ExpRep*) * IR_EXPR_TAB_LEVEL1_HASH_BUCKET);
    m_pool = smpoolCreate(sizeof(ExpRep*) * 128, MEM_COMM);
    m_sc_pool = smpoolCreate(sizeof(xcom::SC<ExpRep*>) * 4, MEM_CONST_SIZE);
    m_ir_expr_lst.set_pool(m_sc_pool);
    m_md_set_mgr = rg->getMDSetMgr();
    m_bs_mgr = rg->getBitSetMgr();
}


ExprTab::~ExprTab()
{
    for (xcom::SC<ExpRep*> * sc = m_ir_expr_lst.get_head();
         sc != m_ir_expr_lst.end(); sc = m_ir_expr_lst.get_next(sc)) {
        ExpRep * ie = sc->val();
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
    count += sizeof(m_level1_hash_tab) * IR_EXPR_TAB_LEVEL1_HASH_BUCKET;
    count += m_map_ir2ir_expr.count_mem();
    return count;
}


void * ExprTab::xmalloc(INT size)
{
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p);
    ::memset(p, 0, size);
    return p;
}


void ExprTab::clean_occ_list()
{
    VecIdx last = m_ir_expr_vec.get_last_idx();
    for (VecIdx i = 0; i <= last; i++) {
        ExpRep * ie = m_ir_expr_vec.get(i);
        if (ie == nullptr) { continue; }
        ASSERT0(EXPR_id(ie) == (UINT)i);
        EXPR_occ_list(ie).clean();
    }
}


//Dump all IR expressions of region and its used MDs.
void ExprTab::dump_ir_expr_tab()
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n==---- DUMP ExprTab ----==");
    VecIdx last = m_ir_expr_vec.get_last_idx();
    for (VecIdx i = 0; i <= last; i++) {
        ExpRep * ie = m_ir_expr_vec.get(i);
        if (ie == nullptr) { continue; }
        ASSERT0(EXPR_id(ie) == (UINT)i);
        note(getRegion(), "\n\n----------- ExpRep(%d)", i);
        dumpIR(EXPR_ir(ie), m_rg);
        note(getRegion(), "\n\tOCC:");
        for (IR * occ = EXPR_occ_list(ie).get_head();
             occ != nullptr; occ = EXPR_occ_list(ie).get_next()) {
            prt(getRegion(), "IR%d", IR_id(occ));
            MDSet const* use_mds = occ->getMayRef();
            if (use_mds != nullptr) {
                prt(getRegion(), "(use:");
                use_mds->dump(m_rg->getMDSystem());
                prt(getRegion(), ")");
            }
            prt(getRegion(), ",");
        }
    }
}


//If 'ir' has been inserted in the table with an ExpRep, get that and return.
ExpRep * ExprTab::map_ir2ir_expr(IR const* ir)
{
    if (ir == nullptr) { return nullptr; }
    return m_map_ir2ir_expr.get(ir->id());
}


void ExprTab::set_map_ir2ir_expr(IR const* ir, ExpRep * ie)
{
    m_map_ir2ir_expr.set(ir->id(), ie);
}


HOST_UINT ExprTab::compute_hash_key(IR const* ir)
{
    ASSERT0(ir != nullptr);
    HOST_UINT hval = ir->getCode() + (ir->getOffset() + 1) +
                  (UINT)(size_t)IR_dt(ir);
    if (ir->isReadPR()) {
        hval += (HOST_UINT)ir->getPrno();
    }
    if (ir->is_id()) {
        Var * var = ID_info(ir);
        hval += 5 * (UINT)(size_t)var;
    }
    return hval;
}


HOST_UINT ExprTab::compute_hash_key_for_tree(IR * ir)
{
    HOST_UINT hval = 0;
    m_iter.clean();
    for (IR const* x = iterInitC(ir, m_iter);
         x != nullptr; x = iterNextC(m_iter)) {
        hval += compute_hash_key(x);
    }
    return hval;
}


//Append IR tree expression into HASH table and return the entry-info.
//If 'ir' has already been inserted in the table with an ExpRep,
//get that and return.
ExpRep * ExprTab::append_expr(IR * ir)
{
    if (ir == nullptr) { return nullptr; }
    HOST_UINT key = compute_hash_key_for_tree(ir);

    //First level hashing.
    HOST_UINT level1_hashv = key % IR_EXPR_TAB_LEVEL1_HASH_BUCKET;
    ExpRep ** level2_hash_tab = m_level1_hash_tab[level1_hashv];
    if (level2_hash_tab == nullptr) {
        //Generate level2
        level2_hash_tab = (ExpRep*(*))xmalloc(sizeof(ExpRep*)*
            IR_EXPR_TAB_LEVEL2_HASH_BUCKET);
        m_level1_hash_tab[level1_hashv] = level2_hash_tab;

        //Generate copy of 'ir'.
        ExpRep * ie = new_ir_expr();
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
    ExpRep * ie = level2_hash_tab[level2_hashv];
    if (ie == nullptr) {
        //Generate copy of 'ir'.
        ie = new_ir_expr();
        EXPR_id(ie) = ++m_expr_count;
        EXPR_ir(ie) = m_rg->dupIRTree(ir);
        m_ir_expr_vec.set(EXPR_id(ie), ie);

        //Enter into 'ir'
        level2_hash_tab[level2_hashv] = ie;
        return ie;
    }

    //Scanning in ExpRep list in level2 hash tab.
    ExpRep * last = nullptr;
    while (ie != nullptr) {
        if (ir->isIREqual(EXPR_ir(ie))) {
            return ie;
        }
        last = ie;
        ie = EXPR_next(ie);
    }

    //Generate copy of 'ir'.
    ie = new_ir_expr();
    EXPR_id(ie) = ++m_expr_count;
    EXPR_ir(ie) = m_rg->dupIRTree(ir);
    m_ir_expr_vec.set(EXPR_id(ie), ie);

    //Enter into 'ir'
    ASSERT0(level2_hash_tab[level2_hashv] != nullptr);
    xcom::insertafter_one(&last, ie);
    return ie;
}


ExpRep * ExprTab::new_ir_expr()
{
    ExpRep * ie = new ExpRep();
    m_ir_expr_lst.append_head(ie);
    return ie;
}


//Remove occurence of ExpRep.
IR * ExprTab::remove_occ(IR * occ)
{
    if (occ == nullptr) { return nullptr; }
    ASSERT0(occ->is_exp());
    ExpRep * ie = map_ir2ir_expr(occ);
    if (ie == nullptr) { return nullptr; }
    return EXPR_occ_list(ie).remove(occ);
}


//Remove all expr for given stmt out of occ list in expr-tab.
void ExprTab::remove_occs(IR * ir)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_STPR:
    SWITCH_CASE_DIRECT_MEM_STMT: {
        IR * stv = ir->getRHS();
        if (stv->is_const()) { return; }
        remove_occ(stv);
        break;
    }
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_INDIRECT_MEM_STMT: {
        IR * stv = ir->getRHS();
        if (!stv->is_const()) {
            remove_occ(stv);
        }
        IR * m = ir->getBase();
        if (m->is_const()) { return; }
        remove_occ(m);
        break;
    }
    SWITCH_CASE_CALL:
        for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
            if (!p->is_const()) {
                remove_occ(p);
            }
        }
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        remove_occ(BR_det(ir));
        break;
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    case IR_IGOTO:
        ASSERT0(ir->getValExp());
        if (!ir->getValExp()->is_const()) {
            remove_occ(ir->getValExp());
        }
        break;
    case IR_RETURN:
        if (RET_exp(ir) != nullptr) {
            if (!RET_exp(ir)->is_const()) {
                remove_occ(RET_exp(ir));
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


//Remove IR tree expression out of HASH table and return the removed
//entry-info if it was existed.
ExpRep * ExprTab::remove_expr(IR * ir)
{
    HOST_UINT key = compute_hash_key_for_tree(ir);

    //First level hashing.
    UINT level1_hashv = key % IR_EXPR_TAB_LEVEL1_HASH_BUCKET;
    ExpRep ** level2_hash_tab = m_level1_hash_tab[level1_hashv];
    if (level2_hash_tab == nullptr) {
        return nullptr;
    }

    //Scanning in level2 hash tab.
    HOST_UINT level2_hashv = key % IR_EXPR_TAB_LEVEL2_HASH_BUCKET;
    ExpRep * ie = level2_hash_tab[level2_hashv];
    if (ie == nullptr) {
        return nullptr;
    }

    //Scanning in ExpRep list in level2 hash tab.
    while (ie != nullptr) {
        if (ir->isIREqual(EXPR_ir(ie))) {
            xcom::remove(&level2_hash_tab[level2_hashv], ie);
            m_ir_expr_vec.remove(EXPR_id(ie), nullptr);
            return ie;
        }
        ie = EXPR_next(ie);
    }
    return nullptr;
}


//Return entry-info if expression has been entered into HASH table,
//otherwise return nullptr.
ExpRep * ExprTab::find_expr(IR * ir)
{
    if (ir == nullptr) { return nullptr; }
    HOST_UINT key = compute_hash_key_for_tree(ir);

    //First level hashing.
    UINT level1_hashv = key % IR_EXPR_TAB_LEVEL1_HASH_BUCKET;
    ExpRep ** level2_hash_tab = m_level1_hash_tab[level1_hashv];
    if (level2_hash_tab == nullptr) {
        return nullptr;
    }

    //Scanning in level2 hash tab.
    UINT level2_hashv = key % IR_EXPR_TAB_LEVEL2_HASH_BUCKET;
    ExpRep * ie = level2_hash_tab[level2_hashv];
    if (ie == nullptr) {
        return nullptr;
    }

    //Scanning in ExpRep list in level2 hash tab.
    while (ie != nullptr) {
        if (ir->isIREqual(EXPR_ir(ie))) {
            return ie;
        }
        ie = EXPR_next(ie);
    }
    return nullptr;
}


ExpRep * ExprTab::encode_expr(IN IR * ir)
{
    if (ir == nullptr) { return nullptr; }

    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    case IR_ID:
    case IR_LD:
    case IR_ILD:
    case IR_LDA:
    case IR_CONST:
    SWITCH_CASE_READ_PR:
        return nullptr;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
    case IR_ARRAY:
    case IR_SELECT: { //formulized determinate-expr?exp:exp
        ExpRep * ie = append_expr(ir);
        ASSERTN(!EXPR_occ_list(ie).find(ir), ("process same IR repeated."));
        EXPR_occ_list(ie).append_tail(ir);
        return ie;
    }
    default: UNREACHABLE();
    }
    return nullptr;
}


//Encode expression for single BB.
//Scan IR statement literally, and encoding it for generating
//the unique id for each individual expressions, and update
//the 'GEN-SET' and 'KILL-SET' of IR-EXPR for BB as well as.
void ExprTab::encode_bb(IRBB * bb)
{
    IRListIter ct = nullptr;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != nullptr; ir = BB_irlist(bb).get_next(&ct)) {
        ASSERT0(ir->is_stmt());
        switch (ir->getCode()) {
        SWITCH_CASE_DIRECT_MEM_STMT:
        case IR_STPR: {
            ExpRep * ie = encode_expr(ir->getRHS());
            if (ie != nullptr) {
                set_map_ir2ir_expr(ir->getRHS(), ie);
            }
            break;
        }
        case IR_STARRAY: {
            ExpRep * ie = encode_expr(ir->getBase());
            if (ie != nullptr) {
                set_map_ir2ir_expr(ir->getBase(), ie);
            }
            for (IR * sub = ARR_sub_list(ir);
                 sub != nullptr; sub = sub->get_next()) {
                ExpRep * ie2 = encode_expr(sub);
                if (ie2 != nullptr) {
                    set_map_ir2ir_expr(sub, ie2);
                }
            }
            ie = encode_expr(ir->getRHS());
            if (ie != nullptr) {
                set_map_ir2ir_expr(ir->getRHS(), ie);
            }
            break;
        }
        SWITCH_CASE_INDIRECT_MEM_STMT: {
            ExpRep * ie = encode_expr(ir->getRHS());
            if (ie != nullptr) {
                set_map_ir2ir_expr(ir->getRHS(), ie);
            }
            ie = encode_istore_memaddr(ir->getBase());
            if (ie != nullptr) {
                set_map_ir2ir_expr(ir->getBase(), ie);
            }
            break;
        }
        case IR_ICALL: { //indirective call
            ExpRep * ie = encode_expr(ICALL_callee(ir));
            if (ie != nullptr) {
                set_map_ir2ir_expr(ICALL_callee(ir), ie);
            }
        }
        //fallthrough
        case IR_CALL:
            for (IR * p = CALL_param_list(ir);
                 p != nullptr; p = p->get_next()) {
                ExpRep * ie = encode_expr(p);
                if (ie != nullptr) {
                    set_map_ir2ir_expr(p, ie);
                }
            }
            break;
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
            ExpRep * ie = encode_expr(BR_det(ir));
            if (ie != nullptr) {
               set_map_ir2ir_expr(BR_det(ir), ie);
            }
            break;
        }
        case IR_IGOTO:
        SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP: {
            ExpRep * ie = encode_expr(ir->getValExp());
            if (ie != nullptr) {
               set_map_ir2ir_expr(ir->getValExp(), ie);
            }
            break;
        }
        case IR_RETURN: {
            ExpRep * ie = encode_expr(RET_exp(ir));
            if (ie != nullptr) {
                set_map_ir2ir_expr(RET_exp(ir), ie);
            }
            break;
        }
        case IR_PHI: {
            for (IR * opnd = PHI_opnd_list(ir);
                 opnd != nullptr; opnd = opnd->get_next()) {
                ExpRep * ie = encode_expr(opnd);
                if (ie != nullptr) {
                    set_map_ir2ir_expr(opnd, ie);
                }
            }
            break;
        }
        default: UNREACHABLE();
        }
    }
    //dump_ir_expr_tab();
}


void ExprTab::reperform(MOD OptCtx & oc)
{
    clean_occ_list();
    perform(oc);
}


//Encode expression for a list of BB.
//Scan IR statement literally, and encoding it for generating
//the unique id for each individual expressions, and update
//the 'GEN-SET' and 'KILL-SET' of IR-EXPR for BB as well as.
bool ExprTab::perform(MOD OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl->get_elem_count() == 0) { return false; }

    BBListIter cb;
    for (IRBB * bb = bbl->get_head(&cb);
         bb != nullptr; bb = bbl->get_next(&cb)) {
        encode_bb(bb);
    }
    OC_is_expr_tab_valid(oc) = true;
    return true;
}
//END ExprTab

} //namespace xoc
