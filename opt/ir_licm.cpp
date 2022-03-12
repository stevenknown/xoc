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
//START LICM
//

void LICM::addInvariantStmt(IR * stmt)
{
    ASSERT0(!m_invariant_stmt.find(stmt));
    m_invariant_stmt.append_tail(stmt);
}


void LICM::addInvariantExp(IR * exp)
{
    ASSERT0(!m_invariant_exp.find(exp));
    m_invariant_exp.append(exp);
}


void LICM::addInvariantExp(IRList const& list)
{
    IRListIter it;
    for (IR * ir = list.get_head(&it); ir != nullptr;
         ir = list.get_next(&it)) {
        if (!markedAsInvExp(ir)) {
            addInvariantExp(ir);
        }
    }
}


void LICM::addHoistCand(IRList const& list)
{
    IRListIter it;
    for (IR * ir = list.get_head(&it); ir != nullptr;
         ir = list.get_next(&it)) {
        ASSERT0(!m_hoist_cand.find(ir));
        m_hoist_cand.append(ir);
    }
}


//Return true if find loop invariant expression.
//Note the function try to recognize the loop invariant expression and stmt.
//So far, the function only regard whole RHS IR tree as loop invariant ONLY
//if all kid IR trees in RHS are loop invariant.
//TODO: recognize the partial IR tree that is loop invariant.
bool LICM::scanBB(IRBB * bb, IN LI<IRBB> * li, bool * islegal, bool first_scan)
{
    bool find = false;
    IRIter irit;
    for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
        if (!ir->isContainMemRef() || ir->isNoMove(true)) { continue; }
        if ((ir->isCallStmt() && !ir->isReadOnly()) || ir->is_region()) {
            //TODO: support call/region.
            //Note PHI has been handled in isLoopInvariantInPRSSA().
            *islegal = false; //prevent loop hoisting.
            return false;
        }
        if (first_scan) { updateMD2Num(ir); }
        //stmt can be loop invariant only if whole RHS expressions
        //of the stmt are invariants.
        find |= chooseStmt(li, ir, irit);
    }
    return find;
}


//Scan operand to find invariant candidate.
//Note in order to reduce the complexity of LICM, the function only handle the
//scenario that whole RHS of stmt is loop-invariant. For cases that
//anticipating to scan and hoist kid IR tree in RHS, will be handled in
//Register Promotion.
//islegal: set to true if loop is legal to perform invariant motion.
//         otherwise set to false to prohibit code motion.
//Return true if find loop invariant expression.
bool LICM::scanLoopBody(IN LI<IRBB> * li, bool * islegal, bool first_scan)
{
    bool find = false;
    IRBB * head = li->getLoopHead();
    UINT headid = head->id();
    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        if (i != (INT)headid && !m_cfg->is_dom(headid, i)) {
            //Loop head should anticipate into analysis as well.
            //The candidate BB must dominate all other loop body BBs.
            continue;
        }
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->getVertex(i));
        find |= scanBB(bb, li, islegal, first_scan);
        if (!(*islegal)) {
            //Whole loop is unsuite to hoist.
            return false;
        }
    }
    return find;
}


//Note if the function invoked, caller has to guarantee all RHS exp of 'ir'
//are loop invariant.
bool LICM::chooseSTandSTPR(LI<IRBB> * li, IR * ir, IRIter & irit)
{
    bool rhs_invariant = false;
    IRList * invlist = m_irs_mgr.alloc();
    bool find = chooseExp(li, ir->getRHS(), irit, &rhs_invariant, invlist);
    addHoistCand(*invlist);
    m_irs_mgr.free(invlist);
    if (!rhs_invariant || ir->isWritePR()) {
        //stmt can NOT be loop invariant because some exp is not invariant.
        return find;
    }
    if (!markedAsInvStmt(ir)) {
        //Push stmt into list that to be analyzed.
        //Stmt is invariant only if both base and RHS are invariant.
        ASSERT0(!m_analysable_stmt_list.find(ir));
        m_analysable_stmt_list.append_tail(ir);
    }
    return find;
}


//Note if the function invoked, caller has to guarantee all RHS exp of 'ir'
//are loop invariant.
bool LICM::chooseIST(LI<IRBB> * li, IR * ir, IRIter & irit)
{
    bool find = false;

    bool rhs_invariant = false;
    IRList * invlist = m_irs_mgr.alloc();
    find |= chooseExp(li, IST_rhs(ir), irit, &rhs_invariant, invlist);
    addHoistCand(*invlist);
    m_irs_mgr.free(invlist);
 
    bool base_invariant = false;
    invlist = m_irs_mgr.alloc();
    find |= chooseExp(li, IST_base(ir), irit, &base_invariant, invlist);
    addHoistCand(*invlist);
    m_irs_mgr.free(invlist);

    if (!rhs_invariant || !base_invariant) {
        //stmt can NOT be loop invariant because some exp is not invariant.
        return find;
    }
    if (!markedAsInvStmt(ir)) {
        //Push stmt into list that to be analyzed.
        //Stmt is invariant only if both base and RHS are invariant.
        ASSERT0(!m_analysable_stmt_list.find(ir));
        m_analysable_stmt_list.append_tail(ir);
    }
    return find;
}


//Note if the function invoked, caller has to guarantee all RHS exp of 'ir'
//are loop invariant.
bool LICM::chooseSTARRAY(LI<IRBB> * li, IR * ir, IRIter & irit)
{
    bool find = false;

    bool rhs_invariant = false;
    IRList * invlist = m_irs_mgr.alloc();
    find |= chooseExp(li, STARR_rhs(ir), irit, &rhs_invariant, invlist);
    addHoistCand(*invlist);
    m_irs_mgr.free(invlist);

    bool base_is_invariant = false;
    invlist = m_irs_mgr.alloc();
    find |= chooseExp(li, STARR_base(ir), irit, &base_is_invariant, invlist);
    addHoistCand(*invlist);
    m_irs_mgr.free(invlist);

    bool all_subexp_is_invariant = true;
    for (IR * subexp = STARR_sub_list(ir);
         subexp != nullptr; subexp = subexp->get_next()) {
        bool subexp_is_invariant = false;
        IRList * invlist = m_irs_mgr.alloc();
        find |= chooseExp(li, subexp, irit, &subexp_is_invariant, invlist);
        addHoistCand(*invlist);
        m_irs_mgr.free(invlist);
        if (!subexp_is_invariant) {
            //stmt can NOT be loop invariant because some exp is not invariant.
            all_subexp_is_invariant = false;
        }
    }

    if (!rhs_invariant || !base_is_invariant || !all_subexp_is_invariant) {
        //stmt can NOT be loop invariant because some exp is not invariant.
        return find;
    }
    if (!markedAsInvStmt(ir)) {
        //Push stmt into list that to be analyzed.
        //Stmt is invariant only if both base and RHS are invariant.
        ASSERT0(!m_analysable_stmt_list.find(ir));
        m_analysable_stmt_list.append_tail(ir);
    }
    return find;
}


//Note if the function invoked, caller has to guarantee all expressions of 'ir'
//are loop invariant.
bool LICM::chooseCallStmt(LI<IRBB> * li, IR * ir, IRIter & irit)
{
    //Hoisting CALL out of loop should generate a guard as well to
    //guarantee CALL will not be exectued if the loop
    //will never execute.
    bool all_param_is_invariant = true;
    bool find = false;
    for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
        bool param_is_invariant = false;
        IRList * invlist = m_irs_mgr.alloc();
        find |= chooseExp(li, p, irit, &param_is_invariant, invlist);
        addHoistCand(*invlist);
        m_irs_mgr.free(invlist);
        if (!param_is_invariant) {
            //stmt can NOT be loop invariant because some exp is not invariant.
            all_param_is_invariant = false;
        }
    }

    if (!all_param_is_invariant || !ir->isReadOnly()) {
        //stmt can NOT be loop invariant because some exp is not invariant.
        return find;
    }
    if (!markedAsInvStmt(ir)) {
        //Push stmt into list that to be analyzed.
        //Stmt is invariant only if both base and RHS are invariant.
        ASSERT0(!m_analysable_stmt_list.find(ir));
        m_analysable_stmt_list.append_tail(ir);
    }
    return false;
}


static bool isJudgeHoistCand(IR const* ir)
{
    ASSERT0(ir->is_judge());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    bool op0_simp = op0->isReadPR() || op0->is_const();
    bool op1_simp = op1->isReadPR() || op1->is_const();
    return !(op0_simp && op1_simp);
}


//Note if the function invoked, caller has to guarantee all expressions of 'ir'
//are loop invariant.
bool LICM::chooseBranch(LI<IRBB> * li, IR * ir, IRIter & irit)
{
    bool exp_invariant;
    IRList * invlist = m_irs_mgr.alloc();
    bool find = chooseExp(li, BR_det(ir), irit, &exp_invariant, invlist);
    if (exp_invariant && !isJudgeHoistCand(BR_det(ir))) {
        //Do not hoist the simplest format judgement expression.
        //There is no profit for such a code hoisting.
        find = false;
    } else {
        addHoistCand(*invlist);
    }
    m_irs_mgr.free(invlist);
    //TODO:hoist branch if it is loop invariant.
    return find;
}


//Note if the function invoked, caller has to guarantee all expressions of 'ir'
//are loop invariant.
bool LICM::chooseSwitch(LI<IRBB> * li, IR * ir, IRIter & irit)
{
    bool exp_invariant;
    IRList * invlist = m_irs_mgr.alloc();
    bool find = chooseExp(li, SWITCH_vexp(ir), irit, &exp_invariant, invlist);
    addHoistCand(*invlist);
    m_irs_mgr.free(invlist);
    //TODO:hoist branch if it is loop invariant.
    return find;
}


//Scan whole IR tree to find loop invariant expression
//and add it into invariant expression list.
//Return true if at least one invariant expression added into list.
//ir: the root IR.
//all_exp_invariant: true if all IR expressions start at 'ir' are
//                   loop invariant.
bool LICM::chooseExp(LI<IRBB> * li, IR * ir, IRIter & irit,
                     OUT bool * all_exp_invariant, OUT IRList * invlist)
{
    ASSERT0(invlist);
    ASSERT0(ir->is_exp());
    *all_exp_invariant = false;
    switch (ir->getCode()) {
    SWITCH_CASE_BIN: {
        if (markedAsInvExp(ir)) {
            *all_exp_invariant = true;
            return false;
        }
        bool find = false;

        bool op0_all_inv = false;
        IRList * invlist0 = m_irs_mgr.alloc();
        find |= chooseExp(li, BIN_opnd0(ir), irit, &op0_all_inv, invlist0);

        bool op1_all_inv = false;
        IRList * invlist1 = m_irs_mgr.alloc();
        find |= chooseExp(li, BIN_opnd1(ir), irit, &op1_all_inv, invlist1);

        if (op0_all_inv && op1_all_inv) {
            invlist->append_tail(ir);
            *all_exp_invariant = true;
            addInvariantExp(ir);
        } else {
            addHoistCand(*invlist0);
            addHoistCand(*invlist1);
        }
        m_irs_mgr.free(invlist0);
        m_irs_mgr.free(invlist1);
        return find;
    }
    SWITCH_CASE_UNA: {
        if (markedAsInvExp(ir)) {
            *all_exp_invariant = true;
            return false;
        }
        bool op0_all_inv = false;
        IRList * invlist0 = m_irs_mgr.alloc();
        bool find = chooseExp(li, UNA_opnd(ir), irit, &op0_all_inv, invlist0);
        if (op0_all_inv) {
            invlist->append_tail(ir);
            *all_exp_invariant = true;
            addInvariantExp(ir);
        } else {
            addHoistCand(*invlist0);
        }
        m_irs_mgr.free(invlist0);
        return find;
    }
    case IR_ARRAY: {
        if (markedAsInvExp(ir)) {
            *all_exp_invariant = true;
            return false;
        }
        List<IRList*> tmp;
        bool find = false;

        bool base_inv = false;
        IRList * invlist0 = m_irs_mgr.alloc();
        tmp.append_tail(invlist0);
        find |= chooseExp(li, ARR_base(ir), irit, &base_inv, invlist0);

        bool all_sub_inv = true;
        for (IR * sub = ARR_sub_list(ir);
             sub != nullptr; sub = sub->get_next()) {
            bool sub_inv = false;
            IRList * invlist1 = m_irs_mgr.alloc();
            find |= chooseExp(li, ARR_base(ir), irit, &sub_inv, invlist1);
            tmp.append_tail(invlist1);
            if (!sub_inv) {
                all_sub_inv = false;
            }
        }

        if (base_inv && all_sub_inv) {
            invlist->append_tail(ir);
            *all_exp_invariant = true;
            addInvariantExp(ir);
        } else {
            for (IRList * l = tmp.get_head(); l != nullptr;
                 l = tmp.get_next()) {
                addHoistCand(*l);
            }
        }
        for (IRList * l = tmp.get_head(); l != nullptr;
             l = tmp.get_next()) {
            m_irs_mgr.free(l);
        }
        return find;
    }
    case IR_ILD: {
        if (markedAsInvExp(ir)) {
            *all_exp_invariant = true;
            return false;
        }
        bool base_inv = false;
        IRList * invlist0 = m_irs_mgr.alloc();
        bool find = chooseExp(li, ILD_base(ir), irit, &base_inv, invlist0);
        if (base_inv) {
            invlist->append_tail(ir);
            *all_exp_invariant = true;
            addInvariantExp(ir);
        } else {
            addHoistCand(*invlist0);
        }
        m_irs_mgr.free(invlist0);
        return find;
    }
    case IR_PR:
        if (markedAsInvExp(ir)) {
            *all_exp_invariant = true;
            return false;
        }
        if (xoc::isLoopInvariant(ir, li, m_rg, &m_invariant_stmt, true)) {
            //Current ir stmt is not suitable to be invariant, neglect.
            *all_exp_invariant = true;
            addInvariantExp(ir);
        }
        return false; //PR should not be regarded as loop invariant.
    case IR_LD:
        if (markedAsInvExp(ir)) {
            *all_exp_invariant = true;
            return false;
        }
        if (xoc::isLoopInvariant(ir, li, m_rg, &m_invariant_stmt, true)) {
            //Current ir stmt is not suitable to be invariant, neglect.
            *all_exp_invariant = true;
            addInvariantExp(ir);
            invlist->append_tail(ir);
            return true;
        }
        return false;
    case IR_CONST:
    case IR_LDA:
    case IR_CASE:
        *all_exp_invariant = true;
        return false;
    case IR_SELECT: {
        if (markedAsInvExp(ir)) {
            *all_exp_invariant = true;
            return false;
        }
        bool find = false;
        //Trueexp
        bool op0_all_inv = false;
        IRList * invlist0 = m_irs_mgr.alloc();
        find |= chooseExp(li, SELECT_trueexp(ir), irit, &op0_all_inv,
                          invlist0);

        //Falseexp
        bool op1_all_inv = false;
        IRList * invlist1 = m_irs_mgr.alloc();
        find |= chooseExp(li, SELECT_falseexp(ir), irit, &op1_all_inv,
                          invlist1);

        //Predexp
        bool op2_all_inv = false;
        IRList * invlist2 = m_irs_mgr.alloc();
        find |= chooseExp(li, SELECT_pred(ir), irit, &op2_all_inv,
                          invlist2);

        if (op0_all_inv && op1_all_inv && op2_all_inv) {
            invlist->append_tail(ir);
            *all_exp_invariant = true;
            addInvariantExp(ir);
        } else {
            addHoistCand(*invlist0);
            addHoistCand(*invlist1);
            addHoistCand(*invlist2);
        }
        m_irs_mgr.free(invlist0);
        m_irs_mgr.free(invlist1);
        m_irs_mgr.free(invlist2);
        return find;
    }
    default: UNREACHABLE();
    }
    return false;
}


//The function find valuable stmt and expression and add it into invariant list.
//Note caller has to guarantee that entire RHS IR tree of ir as invariant.
//The function records the stmt in work list to next round analysis as well.
//Return true if a new invariant is added into list.
bool LICM::chooseStmt(LI<IRBB> * li, IR * ir, IRIter & irit)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_ST:
    case IR_STPR:
        return chooseSTandSTPR(li, ir, irit);
    case IR_IST:
        return chooseIST(li, ir, irit);
    case IR_STARRAY:
        return chooseSTARRAY(li, ir, irit);
    case IR_CALL:
    case IR_ICALL:
        return chooseCallStmt(li, ir, irit);
    case IR_TRUEBR:
    case IR_FALSEBR:
        return chooseBranch(li, ir, irit);
    case IR_SWITCH:
        return chooseSwitch(li, ir, irit);
    default:;
    }
    return false;
}


//Return true if md modified in loop only once.
bool LICM::isUniqueDef(MD const* md) const
{
    ASSERT0(md);
    UINT * n = m_md2num.get(md);
    ASSERTN(n, ("should call updateMD2Num() first"));
    if (*n > 1) { return false; }

    MDTab * mdt = m_md_sys->getMDTab(MD_base(md));
    if (mdt == nullptr) { return true; }

    MD const* x = mdt->get_effect_md();
    if (x != nullptr && x != md && x->is_overlap(md)) {
        UINT * n2 = m_md2num.get(x);
        if (n2 != nullptr && *n2 > 1) { return false; }
    }

    OffsetTab * ofstab = mdt->get_ofst_tab();
    if (ofstab == nullptr) { return true; }
    if (ofstab->get_elem_count() == 0) { return true; }

    ConstMDIter mditer;
    for (MD const* x2 = ofstab->get_first(mditer, nullptr);
         x2 != nullptr; x2 = ofstab->get_next(mditer, nullptr)) {
        if (x2 != md && x2->is_overlap(md)) {
            UINT * n2 = m_md2num.get(x2);
            if (n2 != nullptr && *n2 > 1) { return false; }
        }
    }
    return true;
}


//Propagate invariant property to result.
//This operation will generate more invariant.
//This function will modify m_invariant_stmt, record if the result of
//stmt is loop invariant.
//Note this function assumes whole RHS tree of stmt in
//m_analysable_stmt_list are loop invariant expressions.
//m_analysable_stmt_list will be empty when function return.
bool LICM::scanResult()
{
    bool change = false;
    for (IR * stmt = m_analysable_stmt_list.remove_head(); stmt != nullptr;
         stmt = m_analysable_stmt_list.remove_head()) {
        switch (stmt->getCode()) {
        case IR_ST:
        case IR_STPR: {
            MD const* must = stmt->getRefMD();
            ASSERT0(must);
            if (isUniqueDef(must) && !markedAsInvStmt(stmt)) {
                addInvariantStmt(stmt);
                change = true;
            }
            break;
        }
        case IR_STARRAY:
        case IR_IST: {
            MD const* must = stmt->getRefMD();
            if (must != nullptr && must->is_effect() && isUniqueDef(must) &&
                !markedAsInvStmt(stmt)) {
                addInvariantStmt(stmt);
                change = true;
            }
            break;
        }
        case IR_CALL:
        case IR_ICALL: {
            MD const* must = stmt->getRefMD();
            if ((!stmt->hasReturnValue() || isUniqueDef(must)) &&
                stmt->isReadOnly() && !markedAsInvStmt(stmt)) {
                addInvariantStmt(stmt);
                change = true;
            }
            break;
        }
        default: UNREACHABLE(); //TODO: support more operations.
        }
    }
    return change;
}


void LICM::updateMD2Num(IR * ir)
{
    switch (ir->getCode()) {
    case IR_ST:
    case IR_STPR:
    case IR_PHI:
    case IR_STARRAY:
    case IR_IST: {
        MD const* md = ir->getRefMD();
        if (md != nullptr) {
            UINT * n = m_md2num.get(const_cast<MD*>(md));
            if (n == nullptr) {
                n = (UINT*)xmalloc(sizeof(UINT));
                m_md2num.set(md, n);
            }
            (*n)++;
        }
        MDSet const* mds = ir->getRefMDSet();
        if (mds != nullptr) {
            MDSetIter iter;
            for (INT i = mds->get_first(&iter);
                 i >= 0; i = mds->get_next(i, &iter)) {
                MD * md2 = m_md_sys->getMD(i);
                UINT * n = m_md2num.get(md2);
                if (n == nullptr) {
                    n = (UINT*)xmalloc(sizeof(UINT));
                    m_md2num.set(md2, n);
                }
                (*n)++;
            }
        }
        break;
    }
    case IR_CALL:
    case IR_ICALL: {
        ASSERT0(ir->isReadOnly());
        MD const* md = ir->getRefMD();
        if (md != nullptr) {
            UINT * n = m_md2num.get(const_cast<MD*>(md));
            if (n == nullptr) {
                n = (UINT*)xmalloc(sizeof(UINT));
                m_md2num.set(md, n);
            }
            (*n)++;
        }
        MDSet const* mds = ir->getRefMDSet();
        if (mds != nullptr) {
            MDSetIter iter;
            for (INT i = mds->get_first(&iter);
                 i >= 0; i = mds->get_next(i, &iter)) {
                MD * md2 = m_md_sys->getMD(i);
                UINT * n = m_md2num.get(md2);
                if (n == nullptr) {
                    n = (UINT*)xmalloc(sizeof(UINT));
                    m_md2num.set(md2, n);
                }
                (*n)++;
            }
        }
        break;
    }
    case IR_GOTO:
    case IR_IGOTO:
    case IR_SWITCH:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
        break;
    default: UNREACHABLE(); //Unsupport.
    }
}


//Given loop info li, dump the invariant stmt and invariant expression.
void LICM::dumpInvariantExpStmt(LI<IRBB> const* li) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(),
         "\n==---- DUMP LICM Analysis Result : LoopInfo%d : '%s' ----==\n",
         li->id(), m_rg->getRegionName());
    m_cfg->dumpLoopInfo(m_rg);

    note(getRegion(), "\n");
    if (m_invariant_exp.get_elem_count() > 0) {
        xcom::TTabIter<IR*> ti;
        prt(getRegion(), "-- Invariant Expression (num=%d) -- :",
            m_invariant_exp.get_elem_count());
        getRegion()->getLogMgr()->incIndent(3);
        for (IR * c = m_invariant_exp.get_first(ti);
             c != nullptr; c = m_invariant_exp.get_next(ti)) {
             dumpIR(c, m_rg, nullptr, IR_DUMP_KID);
        }
        getRegion()->getLogMgr()->decIndent(3);
    }

    note(getRegion(), "\n");
    if (m_invariant_stmt.get_elem_count() > 0) {
        prt(getRegion(), "-- Invariant Statement (num=%d) -- :",
            m_invariant_stmt.get_elem_count());
        getRegion()->getLogMgr()->incIndent(3);
        xcom::C<IR*> * it;
        for (IR * c = m_invariant_stmt.get_head(&it);
             c != nullptr; c = m_invariant_stmt.get_next(&it)) {
             dumpIR(c, m_rg);
        }
        getRegion()->getLogMgr()->decIndent(3);
    }

    note(getRegion(), "\n");
    if (m_hoist_cand.get_elem_count() > 0) {
        xcom::TTabIter<IR*> ti;
        prt(getRegion(), "-- Hoist Cand Expression (num=%d) -- :",
            m_hoist_cand.get_elem_count());
        getRegion()->getLogMgr()->incIndent(3);
        for (IR * c = m_hoist_cand.get_first(ti);
             c != nullptr; c = m_hoist_cand.get_next(ti)) {
             dumpIR(c, m_rg, nullptr, IR_DUMP_KID);
        }
        getRegion()->getLogMgr()->decIndent(3);
    }
}


//Analysis loop invariant expression and stmt.
//Return true if find them, otherwise return false.
bool LICM::analysis(IN LI<IRBB> * li)
{
    //Record if the result of stmt is invariant.
    bool change = true;
    bool find = false;
    bool first_scan = true;
    while (change) {
        bool islegal = true;
        change = scanLoopBody(li, &islegal, first_scan);
        if (!islegal) {
            m_hoist_cand.clean();
            return false;
        }

        if (change) {
            find = true;
            //m_analysable_stmt_list will be empty when function return.
            scanResult();
            ASSERT0(m_analysable_stmt_list.get_elem_count() == 0);
        } else {
            //Before next round analysis, we must make sure all
            //stmts in this list is invariant or not.
            m_analysable_stmt_list.clean();
        }
        first_scan = false;
    }
    return find;
}


//Return true if stmt is marked and collected into invariant-stmt set.
bool LICM::markedAsInvStmt(IR const* stmt) const
{
    ASSERT0(stmt->is_stmt());
    return m_invariant_stmt.find(const_cast<IR*>(stmt));
}


//Return true if exp is marked and collected into invariant-stmt set.
bool LICM::markedAsInvExp(IR const* exp) const
{
    ASSERT0(exp->is_exp());
    return m_invariant_exp.find(const_cast<IR*>(exp));
}


//Return true if any stmt that is related to invariant stmt
//is moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
bool LICM::handleDefByDUChain(IR const* exp, OUT IRBB * prehead,
                              OUT LI<IRBB> * li)
{
    ASSERT0(exp->is_exp());
    PRSSAMgr * prssamgr = m_rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid() && exp->isReadPR()) {
        SSAInfo * info = exp->getSSAInfo();
        ASSERTN(info, ("miss PRSSAInfo"));
        //Check if SSA def is loop invariant.
        IR * def = SSA_def(info);
        if (def != nullptr && !tryHoistDefStmt(def, prehead, li)) {
            return false;
        }
        return true;
    }

    MDSSAMgr * mdssamgr = m_rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() && exp->isMemoryOpnd()) {
        //TBD:Does this code useful?
        //MDSSAInfo * info = mdssamgr->getMDSSAInfoIfAny(exp);
        //ASSERTN(info, ("def stmt even not in MDSSA system"));
        //mdssamgr->addMDSSAOcc(use, info);
        return true;
    }

    DUMgr * dumgr = m_rg->getDUMgr();
    DUSet const* defset = exp->readDUSet();
    if (dumgr != nullptr && defset != nullptr) {
        DUSetIter di = nullptr;
        for (INT i = defset->get_first(&di);
             i >= 0; i = defset->get_next(i, &di)) {
            IR * def = m_rg->getIR(i);
            ASSERT0(def);
            if (!tryHoistDefStmt(def, prehead, li)) {
                return false;
            }
        }
    }
    return true;
}


//Return true if any stmt is moved outside from loop.
bool LICM::hoistInvariantStmt(MOD IR * stmt, MOD IRBB * prehead,
                              MOD LI<IRBB> * li)
{
    ASSERT0(stmt->getBB());
    ConstIRIter iriter;
    for (IR const* x = iterExpInitC(stmt, iriter);
         x != nullptr; x = iterExpNextC(iriter)) {
        if (!handleDefByDUChain(x, prehead, li)) {
            //stmt can not be hoisted.
            return false;
        }
        //Go ahead and check next expression.
    }

    //OK, stmt can be hoisted to preheader.
    BB_irlist(stmt->getBB()).remove(stmt);
    BB_irlist(prehead).append_tail_ex(stmt);
    return true;
}


//Return true if any stmt is moved outside from loop.
bool LICM::tryHoistDefStmt(MOD IR * def, MOD IRBB * prehead, MOD LI<IRBB> * li)
{
    ASSERT0(def->is_stmt());
    IRBB * dbb = def->getBB();
    ASSERT0(dbb);
    if (markedAsInvStmt(def)) {
        if (!li->isInsideLoop(dbb->id()) ||
            hoistInvariantStmt(def, prehead, li)) {
            return true;
        }
        return false;
    }

    if (li->isInsideLoop(dbb->id())) {
        return false;
    }

    //def stmt has been moved to prehead.
    return true;
}


//There are two method to choose the target of guard.
//For now we use method1, because method2 will lead more complicated
//incremental update on DOM.
static void chooseTargetBBOfGuard(LI<IRBB> const* li, IRCFG * cfg, IRBB * guard,
                                  OUT LabelInfo const** lab)
{
    #define METHOD1
    #ifdef METHOD1
    //METHOD1:
    //BB_guard
    //  |
    //  v
    //  BB_prehead
    //  |
    //  v
    //  BB_loophead <--
    //after insertion of edge:
    //  BB_guard
    //  |   |
    //  |   v
    //  |  BB_prehead
    //  |   /
    //  |  /
    //  v v
    //  BB_loophead <--
    IRBB * loophead = li->getLoopHead();
    cfg->addEdge(guard, loophead);
    *lab = loophead->getLabelList().get_head();
    #else
    //METHOD2:
    //BB_guard
    //  |
    //  v
    //  BB_preheader
    //  |
    //  v
    //  BB_loophead <--
    //  |
    //  v
    //  BB_loopend
    //after insertion of edge:
    //  BB_guard
    //  |   |
    //  |   v
    //  |  BB_prehead
    //  |   |
    //  |   |
    //  |   v
    //  |  BB_loophead <--
    //  |   |
    //  v   v
    //  BB_loopend
    IRBB * loopend = findFirstLoopEndBB(li, cfg);
    ASSERTN(loopend, ("weird loop structure"));
    cfg->addEdge(guard, loopend);

    //TBD:Which target BB should guard_br set? The loophead, or the loopend?
    *lab = loopend->getLabelList().get_head();
    #endif
}


//Insert guard controlling BB to predominate the execution of 'prehead'.
//This function will maintain RPO of generated guard BB.
//prehead: preheader BB of loop.
//li: LoopInfo.
//Return the new guard controlling BB.
IRBB * LICM::insertGuardBB(LI<IRBB> const* li, IRBB * prehead, HoistCtx & ctx)
{
    ASSERTN(prehead->getIRList().get_elem_count() == 0,
            ("Need empty preheader"));
    //---- Insert guard BB and add fallthrough-edge firstly.
    //  BB_prehead
    //  |
    //  V
    //  BB_loophead <--
    //after inserting guard:
    //  BB_guard
    //  |
    //  V
    //  BB_prehead
    //  |
    //  V
    //  BB_loophead <--
    IRBB * guard = m_rg->allocBB();
    m_cfg->addVertex(guard->id()); //Add vertex to graph before updating RPO.
    if (!m_cfg->tryUpdateRPO(guard, prehead, true)) {
        //TODO: Try update RPO incrementally to avoid recompute whole BB list.
        //m_cfg->tryUpdateRPO(prehead, guard, true);
        OC_is_rpo_valid(*ctx.oc) = false; //Just leave RPO-recomputation
                                          //to next user for now.
    }
    m_cfg->insertBBbefore(prehead, guard);
    m_cfg->addDomInfoByNewIDom(prehead->id(), guard->id());

    //---- Choose and add branch-edge to guard BB.
    LabelInfo const* guard_br_lab = nullptr;
    chooseTargetBBOfGuard(li, m_cfg, guard, &guard_br_lab);

    //Preheader does not have PDOM any more.
    if (!m_cfg->changeDomInfoByAddBypassEdge(prehead->id())) {
        ctx.oc->setDomValid(false);
        m_rg->getPassMgr()->checkValidAndRecompute(
            ctx.oc, PASS_DOM, PASS_UNDEF);
    }
    
    //---- Build guard-branch stmt of guard-BB.
    IRBB * loophead = li->getLoopHead();
    IR * loophead_br = loophead->getLastIR();
    ASSERT0(loophead_br && loophead_br->isConditionalBr());
    ASSERT0(loophead_br->is_single());
    IR * newdet = m_rg->dupIRTree(BR_det(loophead_br));
    if (loophead_br->is_truebr()) {
        //Make sure the guard-branch is FALSEBR because FALSEBR uses
        //fewer instructions than TRUEBR.
        Refine::invertCondition(&newdet, m_rg);
    }

    //---- Set the target label of guard-branch.
    ASSERT0(guard_br_lab);
    IR * guard_br = m_rg->buildBranch(false, newdet, guard_br_lab);

    //Insert the guard-branch into guard-BB.
    guard->getIRList().append_tail_ex(guard_br);

    //---- DU operation
    //Assign MD for all generated new IRs.
    m_rg->getMDMgr()->assignMDForIRList(guard_br, true, true);
    if (m_du != nullptr) {
        //Copy the DU chain for generated IR.
        m_du->addUseForTree(newdet, BR_det(loophead_br));
    }
    addSSADUChainForExp(newdet, ctx);
    return guard;
}


//Try to evaluate the value of loop execution condition.
//Returnt true if this function evaluated successfully, otherwise return false.
bool LICM::tryEvalLoopExecCondition(LI<IRBB> const* li,
                                    OUT bool & must_true,
                                    OUT bool & must_false) const
{
    if (m_rce == nullptr) { return false; }
    IRBB const* head = li->getLoopHead();
    ASSERT0(head);
    IR const* last = const_cast<IRBB*>(head)->getLastIR();
    ASSERT0(last && last->isConditionalBr());

    //Try to evaluate the value of judgement operation.
    return m_rce->calcCondMustVal(BR_det(last), must_true, must_false);
}


//Return true if loop body is executed conditionally which is in charged of
//the judgement stmt in loophead BB.
//e.g:Return true for while-do loop, and false for do-while loop.
bool LICM::isLoopExecConditional(LI<IRBB> const* li) const
{
    IRBB const* head = li->getLoopHead();
    ASSERT0(head);
    IR const* last = const_cast<IRBB*>(head)->getLastIR();
    return last != nullptr && last->isConditionalBr();
}


//Return true if gurard BB of LOOP 'li' has been inserted.
bool LICM::hasInsertedGuardBB(LI<IRBB> const* li) const
{
    return m_insert_guard_bb.get(li) != nullptr;
}


static bool isImmRHS(IR const* exp, IR const* stmt)
{
    ASSERT0(exp->is_exp() && stmt->is_stmt());
    return exp == stmt->getRHS();
}


void LICM::checkAndInsertGuardBB(LI<IRBB> const* li, IRBB * prehead,
                                 HoistCtx & ctx)
{
    TTabIter<IR*> it;
    for (IR * c = m_hoist_cand.get_first(it);
         c != nullptr; c = m_hoist_cand.get_next(it)) {
        ASSERT0(c->is_exp());
        IR * cand_stmt = c->getStmt();
        if (!cand_stmt->isStoreStmt()) { continue; }
        if (!isImmRHS(c, cand_stmt)) { continue; }
        if (!markedAsInvStmt(cand_stmt)) { continue; }
        if (!xoc::isStmtDomAllUseInsideLoop(cand_stmt, li, m_rg)) { continue; }
        if (cand_stmt->getBB() == li->getLoopHead()) { continue; }
        if (!isLoopExecConditional(li)) { continue; }
        if (hasInsertedGuardBB(li)) { continue; }

        bool must_true, must_false;
        if (tryEvalLoopExecCondition(li, must_true, must_false) &&
            must_true) {
            continue; //guard BB is unnecessary
        }

        //Guard BB is necessary.
        IRBB * guard = insertGuardBB(li, prehead, ctx);
        xoc::movePhi(prehead, guard, m_rg);
        m_insert_guard_bb.append(li);
        ctx.inserted_guard_bb = true;
        ctx.cfg_changed = true;
        return;
    }
}


//The function hoist stmt to preheader.
//Note guard BB has been check by checkAndInsertGuardBB() if needed.
void LICM::hoistWholeStmt(IR * cand_exp, OUT IRBB * prehead, OUT LI<IRBB> * li,
                          OUT HoistCtx & ctx)
{
    IR * cand_stmt = cand_exp->getStmt();
    //cand is store-value and the result memory object is ID|PR.
    //NOTE: If we hoist entire stmt out from loop,
    //we should make sure the stmt will be execute at least once
    //or never. Conditional branch should be generated and encapsulate
    //the hoisted stmt to ensure that.
    //    while (a > 0) {
    //        a = 10;
    //        foo();
    //    }
    //    =>
    //    if (a > 0)  {
    //        a = 10;
    //    }
    //    while (a > 0) {
    //        foo();
    //    }
    ASSERT0(cand_stmt->getBB());

    cand_stmt->getBB()->getIRList().remove(cand_stmt);
    prehead->getIRList().append_tail_ex(cand_stmt);

    //The code motion do not modify classic DU chain of 'exp' and
    //'cand_stmt'. So it is no need to revise DU chain.
    //But the live-expr, reach-def, avail-reach-def set
    //info of each BB changed.
    //However, MDSSA DU chain changed, maintain MDSSA DU chain at the end of
    //doLoopTree at once.
    if (useMDSSADU()) {
        ctx.hoisted_stmt.append_tail(cand_stmt);
    }
    ctx.duset_changed = true;
}


//Post-process hoisted-stmt.
void LICM::addSSADUChainForHoistedStmt(HoistCtx & ctx)
{
    if (!useMDSSADU() || ctx.hoisted_stmt.get_elem_count() == 0) { return; }
    ASSERT0(ctx.oc->is_dom_valid());
    for (IR * stmt = ctx.hoisted_stmt.get_head(); stmt != nullptr;
         stmt =  ctx.hoisted_stmt.get_next()) {
        ASSERT0(m_mdssamgr->getMDSSAInfoIfAny(stmt));
        bool succ = m_mdssamgr->tryChangeDefToPrev(stmt);
        ASSERT0(succ);
        m_mdssamgr->insertDefStmt(stmt, *ctx.domtree);
    }
}


//Maintaining the DU chain of generated IR.
void LICM::addSSADUChainForExp(IR * exp, HoistCtx const& ctx)
{
    if (!useMDSSADU()) { return; }
    ASSERT0(ctx.oc->is_dom_valid());
    ASSERT0(exp->is_exp());
    IR * init_stmt = exp->getStmt();
    ASSERT0(init_stmt);
    IRBB * bb = init_stmt->getBB();
    //Note DOM info must be available.
    m_mdssamgr->findAndSetLiveInDefForTree(exp, bb->getPrevIR(init_stmt), bb);
}


//This function will maintain RPO of generated guard BB.
//Return true if BB or STMT changed.
//Note this function may insert guard BB if loop-det is conditional.
bool LICM::hoistCandHelper(OUT IR * cand_exp,
                           OUT IRBB * prehead,
                           OUT LI<IRBB> * li,
                           OUT HoistCtx & ctx)
{
    ASSERT0(cand_exp->is_exp());
    IR * cand_stmt = cand_exp->getStmt();
    if (cand_stmt->isStoreStmt() &&
        isImmRHS(cand_exp, cand_stmt) &&
        markedAsInvStmt(cand_stmt) &&
        xoc::isStmtDomAllUseInsideLoop(cand_stmt, li, m_rg)) {
        hoistWholeStmt(cand_exp, prehead, li, ctx);
        return true;
    }

    if (cand_exp->is_const()) {
        //CASE1: given
        //  n = 0x10; //S1
        //No need to build STPR in preheader.
        return false;
    }

    //CASE2: given
    //  n = cand_exp; //S1
    //Generate new stmt S2, change S1 to S3:
    //  p1 = cand_exp; //S2
    //  n = p1; //S3
    //move S2 into prehead BB.
    IR * t = m_rg->buildPR(cand_exp->getType());
    if (cand_stmt->hasJudgeDet() && cand_exp == cand_stmt->getJudgeDet()) {
        bool f = cand_stmt->replaceKid(cand_exp, m_rg->buildJudge(t), true);
        CHECK0_DUMMYUSE(f);
    } else {
        bool f = cand_stmt->replaceKid(cand_exp, t, true);
        CHECK0_DUMMYUSE(f);
    }

    IR * stpr = m_rg->buildStorePR(PR_no(t), t->getType(), cand_exp);

    //Revise MD info.
    MD const* tmd = m_rg->getMDMgr()->genMDForPR(t);
    t->setRefMD(tmd, m_rg);
    stpr->setRefMD(tmd, m_rg);

    xoc::buildDUChain(stpr, t, m_rg);
    prehead->getIRList().append_tail_ex(stpr);

    addSSADUChainForExp(cand_exp, ctx);
    ctx.duset_changed = true;
    return true;
}


//Try to move and check that each definitions of candidate has been
//already hoisted from loop.
bool LICM::tryMoveAllDefStmtOutFromLoop(IR const* c, IRBB * prehead,
                                        OUT LI<IRBB> * li)
{
    ConstIRIter irit;
    for (IR const* x = iterInitC(c, irit); x != nullptr; x = iterNextC(irit)) {
        if (!handleDefByDUChain(x, prehead, li)) {
            //x's DEF can not be hoisted.
            return false;
        }
    }
    return true;
}


//Hoist candidate IRs to preheader BB.
//This function will maintain RPO if new BB inserted.
//Return true if BB or STMT changed.
void LICM::hoistCand(OUT IRBB * prehead, OUT LI<IRBB> * li, OUT HoistCtx & ctx)
{
    Vector<IR*> removed;
    TTabIter<IR*> ti;

    //IRBB * backedge_bb = li->findBackedgeStartBB(m_cfg);
    while (m_hoist_cand.get_elem_count() > 0) {
        UINT removednum = 0;
        for (IR * c = m_hoist_cand.get_first(ti);
             c != nullptr; c = m_hoist_cand.get_next(ti)) {
            ASSERT0(c->is_exp());
            if (!isWorthHoist(c)) { continue; }
            if (!tryMoveAllDefStmtOutFromLoop(c, prehead, li)) { continue; }

            removed.set(removednum, c);
            removednum++;
            if (!li->isInsideLoop(c->getStmt()->getBB()->id())) {
                //Candidate expression has been moved to preheader.
                //e.g:stpr $1 = add (ld gp, 0x1);  //S1
                //    st m = ild $1;  //S2
                //Both 'add' and 'ild' are cand-expression.
                //First, we choose moving S2 to preheader first.
                //Whereas according to the dependence
                //relation of $1, the DEF stmt of $1 will be moved to
                //preheader, namely, S1.
                //Next, we are going to moving cand-exp 'add', but we find it
                //has been moved to preheader.
                continue;
            }
            hoistCandHelper(c, prehead, li, ctx);
        }
        ASSERTN(removednum > 0, ("not find any hoistable exp?"));

        for (UINT i = 0; i < removednum; i++) {
            IR * c = removed.get(i);
            ASSERT0(c);
            m_hoist_cand.remove(c);
        }
    }
}


void LICM::cleanBeforeLoop()
{
    m_invariant_stmt.clean();
    m_invariant_exp.clean();
    m_analysable_stmt_list.clean();
    m_md2num.clean();
    m_hoist_cand.clean();
}


//Return true if code motion happened.
//The funtion will maintain LoopInfo.
bool LICM::doLoopTree(LI<IRBB> * li, HoistCtx & ctx)
{
    if (li == nullptr) { return false; }
    bool changed = false;
    for (LI<IRBB> * tli = li; tli != nullptr; tli = LI_next(tli)) {
        changed |= doLoopTree(LI_inner_list(tli), ctx);
        cleanBeforeLoop();
        analysis(tli);
        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLICM()) {
            //Dump invariant info here because they will be replaced soon.
            dumpInvariantExpStmt(tli);
        }

        if (m_hoist_cand.get_elem_count() == 0) {
            continue;
        }

        //Always insert a preheader to facilitate the insertion of guard-BB.
        IRBB * preheader = nullptr;
        bool insert_prehead = xoc::insertPreheader(tli, m_rg, &preheader,
                                                   *ctx.oc, true);
        changed |= insert_prehead;
        ctx.cfg_changed |= insert_prehead;
        if (!ctx.oc->is_dom_valid()) {
            m_rg->getPassMgr()->checkValidAndRecompute(
                ctx.oc, PASS_DOM, PASS_UNDEF);
        }
        checkAndInsertGuardBB(tli, preheader, ctx);
        hoistCand(preheader, tli, ctx);
        changed |= ctx.duset_changed;
    
        //---- Maintain BB characters.
        //hoistCand may append stmt into BB which has down-boundary stmt.
        //That makes BB invalid. Split such invalid BB into two or more BBs.
        bool splitnewbb = m_cfg->splitBBIfNeeded(preheader, *ctx.oc);
        ASSERTN(!splitnewbb, ("Would this happen?"));
        ctx.cfg_changed |= splitnewbb;

        //---- DU operation
        m_rg->getPassMgr()->checkValidAndRecompute(
            ctx.oc, PASS_DOM, PASS_UNDEF);
        if (splitnewbb || insert_prehead) {
            ctx.buildDomTree(m_cfg);
        }
        addSSADUChainForHoistedStmt(ctx);
        ctx.cleanAfterLoop();
    }
    return changed;
}


bool LICM::dump() const
{
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    //Invariant Variable info has been dumpped during the transformation.
    Pass::dump();
    return true;
}


bool LICM::perform(OptCtx & oc)
{
    if (m_rg->getBBList() == nullptr ||
        m_rg->getBBList()->get_elem_count() == 0) {
        return false;
    }

    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //DCE use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //DCE use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_LOOP_INFO,
                                               PASS_UNDEF);
    m_rce = (RCE*)m_rg->getPassMgr()->registerPass(PASS_RCE);
    ASSERT0(m_rce);
    if (m_rce->is_use_gvn() && !m_rce->getGVN()->is_valid()) {
        m_rce->getGVN()->perform(oc);
    }

    m_rg->getLogMgr()->startBuffer();
    HoistCtx ctx(&oc);
    ctx.buildDomTree(m_cfg);
    bool change = doLoopTree(m_cfg->getLoopInfo(), ctx);
    if (change) {
        if (ctx.cfg_changed) {
            //For conservative purpose, we hope to recompute RPO BB list
            //when it is needed.
            m_cfg->freeRPOVexList();

            //LOOP, DOM, RPO are maintained, but CDG is not.
            ASSERT0(oc.is_rpo_valid() && oc.is_dom_valid());
            ASSERT0(m_cfg->isRPOValid());
            OC_is_cdg_valid(oc) = false;
        }
        ASSERT0(m_cfg->verifyRPO(oc));
        OC_is_expr_tab_valid(oc) = false;
        if (ctx.duset_changed) {
            OC_is_live_expr_valid(oc) = false;
            OC_is_avail_reach_def_valid(oc) = false;
            OC_is_reach_def_valid(oc) = false;
            if (m_rce != nullptr && m_rce->is_use_gvn()) {
                m_rce->getGVN()->set_valid(false);
            }
        }
        m_cfg->performMiscOpt(oc);

        //DU chain and du ref is maintained.
        ASSERT0(m_rg->verifyMDRef());
        ASSERT0(verifyMDDUChain(m_rg));
        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLICM()) {
            dump();
        }
        ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg));
        ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg));
    } else {
        m_rg->getLogMgr()->cleanBuffer();
    }
    m_rg->getLogMgr()->endBuffer();
    m_irs_mgr.clean();
    END_TIMER(t, getPassName());
    return change;
}
//END LICM

} //namespace xoc
