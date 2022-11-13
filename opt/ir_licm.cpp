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

static bool isImmRHS(IR const* exp, IR const* stmt)
{
    ASSERT0(exp->is_exp() && stmt->is_stmt());
    return exp == stmt->getRHS();
}


static void updateDomTreeByPreheader(LI<IRBB> const* li, IRBB const* preheader,
                                     DomTree & domtree)
{
    ASSERT0(preheader);
    //Only insert preheader into domtree.
    domtree.insertParent(li->getLoopHead()->id(), preheader->id());
}


static void updateDomTreeByGuardRegion(LI<IRBB> const* li,
                                       InsertGuardHelper const& help,
                                       DomTree & domtree)
{
    ASSERT0(help.getGuardEnd());
    domtree.insertParent(li->getLoopHead()->id(), help.getGuardEnd()->id());
    domtree.insertParent(help.getGuardEnd()->id(),
                         help.getGuardStart()->id());
    domtree.insertKid(help.getGuardStart()->id(), help.getGuardedBB()->id());

}


static bool isJudgeHoistCand(IR const* ir)
{
    ASSERT0(ir->is_judge());
    switch (ir->getKidNum()) {
    case 1: {
        IR const* op = UNA_opnd(ir);
        return !(op->isReadPR() || op->is_const());
    }
    case 2: {
        IR const* op0 = BIN_opnd0(ir);
        IR const* op1 = BIN_opnd1(ir);
        bool op0_simp = op0->isReadPR() || op0->is_const();
        bool op1_simp = op1->isReadPR() || op1->is_const();
        return !(op0_simp && op1_simp);
    }
    default: ASSERTN(0, ("TODO"));
    }
    return false;
}


class InsertPreheaderMgr {
    COPY_CONSTRUCTOR(InsertPreheaderMgr);
    Region * m_rg;
    OptCtx * m_oc;
    LI<IRBB> const* m_li;
    IRBB * m_preheader;
    MDSSAMgr * m_mdssamgr;
    IRCFG * m_cfg;
    InsertGuardHelper m_gdhelp;
private:
    //rce: RCE object, may be null.
    void checkAndInsertGuardBB(IRTab const& irtab, OUT LICM * licm,
                               RCE const* rce, OUT HoistCtx & ctx);

    //Return true if loop body is executed conditionally which is in charged of
    //the judgement stmt in loophead BB.
    //e.g:Return true for while-do loop, and false for do-while loop.
    bool isLoopExecConditional() const;

    //Try to evaluate the value of loop execution condition.
    //Returnt true if this function evaluated successfully, otherwise return false.
    bool tryEvalLoopExecCondition(RCE const* rce,
                                  OUT bool & must_true,
                                  OUT bool & must_false,
                                  HoistCtx const& ctx) const;

    //The funtion should be invoked after exp hoisted.
    void updateMDSSADUForExpInPreHeader();

    //The funtion should be invoked after phi modified.
    void updateMDSSADUForLoopHeadPhi(HoistCtx const& ctx);
    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    void updateDomTree(DomTree & domtree);
public:
    InsertPreheaderMgr(Region * rg, OptCtx * oc, LI<IRBB> const* li) :
        m_rg(rg), m_oc(oc), m_li(li), m_preheader(nullptr), m_gdhelp(rg, oc)
    { m_mdssamgr = m_rg->getMDSSAMgr(); m_cfg = m_rg->getCFG(); }

    IRBB * getPreheader() const { return m_preheader; }

    bool needComplicatedGuard() const
    { return m_gdhelp.needComplicatedGuard(m_li); }

    void reviseSSADU(HoistCtx const& ctx);

    void undoCFGChange(HoistCtx & ctx);

    //The function insert preheader before loop.
    //Return true if the function inserted a new BB as preheader, otherwise
    //regard an existing BB as preheader.
    bool perform(IRTab const& irtab, OUT LICM * licm,
                 RCE const* rce, HoistCtx & ctx);
};


//Return true if loop body is executed conditionally which is in charged of
//the judgement stmt in loophead BB.
//e.g:Return true for while-do loop, and false for do-while loop.
bool InsertPreheaderMgr::isLoopExecConditional() const
{
    IRBB const* head = m_li->getLoopHead();
    ASSERT0(head);
    IR const* last = const_cast<IRBB*>(head)->getLastIR();
    return last != nullptr && last->isConditionalBr();
}


//Try to evaluate the value of loop execution condition.
//Returnt true if this function evaluated successfully, otherwise return false.
bool InsertPreheaderMgr::tryEvalLoopExecCondition(RCE const* rce,
                                                  OUT bool & must_true,
                                                  OUT bool & must_false,
                                                  HoistCtx const& ctx) const
{
    if (rce == nullptr) { return false; }
    IRBB const* head = m_li->getLoopHead();
    ASSERT0(head);
    IR const* last = const_cast<IRBB*>(head)->getLastIR();
    ASSERT0(last && last->isConditionalBr());

    //Try to evaluate the value of judgement operation.
    return rce->calcCondMustVal(BR_det(last), must_true, must_false, *m_oc);
}


//rce: RCE object, may be null.
void InsertPreheaderMgr::checkAndInsertGuardBB(IRTab const& irtab,
                                               OUT LICM * licm, RCE const* rce,
                                               OUT HoistCtx & ctx)
 {
    if (!isLoopExecConditional()) { return; }
    ASSERT0(licm);
    bool must_true, must_false;
    if (tryEvalLoopExecCondition(rce, must_true, must_false, ctx) &&
        must_true) {
        return; //guard BB is unnecessary
    }
    IRTabIter it;
    for (IR const* c = irtab.get_first(it); c != nullptr;
         c = irtab.get_next(it)) {
        ASSERT0(c->is_exp());
        IR * stmt = c->getStmt();
        if (!stmt->isStoreStmt()) { continue; }
        if (!isImmRHS(c, stmt)) { continue; }
        if (!licm->markedAsInvStmt(stmt)) { continue; }
        if (!xoc::isStmtDomAllUseInsideLoop(stmt, m_li, m_rg, *m_oc)) {
            continue;
        }
        if (stmt->getBB() == m_li->getLoopHead()) { continue; }
        if (licm->hasInsertedGuardBB(m_li)) { continue; }
        //Guard BB is necessary.
        IRBB * guard = m_gdhelp.insertGuard(m_li, m_preheader);
        xoc::movePhi(m_preheader, guard, m_rg);
        licm->setLoopHasBeenGuarded(m_li);
        ctx.inserted_guard_bb = true;
        ctx.cfg_changed = true;
        return;
    }
}


void InsertPreheaderMgr::undoCFGChange(OUT HoistCtx & ctx)
{
    if (m_gdhelp.hasInsertedGuard()) {
        //Remove the preheader and guard just inserted.
        CfgOptCtx coctx(*m_oc);
        m_gdhelp.removeGuardRegion(*ctx.domtree);
        ASSERT0(!m_cfg->isVertex(m_preheader->id()));
        //Useless preheader has been removed.
        ctx.cleanAfterLoop();
        return;
    }
    if (m_preheader != nullptr) {
        //Remove the preheader just inserted.
        ctx.domtree->remove(m_preheader->id());
        CfgOptCtx coctx(*m_oc);
        bool res = m_cfg->removeSingleEmptyBB(m_preheader, coctx);
        ASSERT0(res);
        ASSERTN(coctx.numOfRemovedEmptyBB() == 1, ("only preheader removed"));
        //removeEmptyBB only maintained these frequently used CFG info.
        OptCtx::setInvalidIfCFGChangedExcept(m_oc, PASS_RPO, PASS_DOM,
                                             PASS_LOOP_INFO, PASS_UNDEF);
    }
    ctx.cleanAfterLoop();
}


void InsertPreheaderMgr::updateMDSSADUForExpInPreHeader()
{
    ASSERT0(useMDSSADU());
    ASSERT0(m_mdssamgr);
    ASSERTN(m_oc->is_dom_valid(), ("DOM info must be available"));
    //Note Phi operand in preheader should have been renamed
    //in InsertPhiHellper.
    BBIRListIter it;
    IRIter irit;
    BBIRList const& irlst = const_cast<IRBB*>(m_preheader)->getIRList();
    IR * prev = nullptr;
    for (IR * ir = irlst.get_head(&it); ir != nullptr;
         prev = ir, ir = irlst.get_next(&it)) {
        irit.clean();
        for (IR * e = xoc::iterExpInit(ir, irit);
             e != nullptr; e = xoc::iterExpNext(irit)) {
            ASSERT0(e->is_exp());
            if (!e->isMemRefNonPR()) { continue; }
            m_mdssamgr->findAndSetLiveInDef(e, prev, m_preheader, *m_oc);
        }
    }
}


void InsertPreheaderMgr::updateMDSSADUForLoopHeadPhi(HoistCtx const& ctx)
{
    ASSERT0(useMDSSADU());
    //DOM info must be available.
    ASSERT0(m_oc->is_dom_valid());
    ASSERT0(m_mdssamgr);
    IRBB * loophead = m_li->getLoopHead();
    MDPhiList const* philist = m_mdssamgr->getPhiList(loophead);
    if (philist == nullptr) { return; }
    m_mdssamgr->recomputeDUAndDDChain(philist, *ctx.domtree);
    m_mdssamgr->recomputeDefForOpnd(philist, *m_oc);
}



void InsertPreheaderMgr::reviseSSADU(HoistCtx const& ctx)
{
    if (m_gdhelp.hasInsertedGuard()) {
        m_gdhelp.insertPhiForGuardedBB(*ctx.domtree);
    }
    ASSERT0(m_oc->is_dom_valid());
    ASSERT0(ctx.is_domtree_valid());
    if (m_preheader != nullptr && useMDSSADU()) {
        updateMDSSADUForExpInPreHeader();
        updateMDSSADUForLoopHeadPhi(ctx);
    }
}


void InsertPreheaderMgr::updateDomTree(DomTree & domtree)
{
    if (m_gdhelp.hasInsertedGuard()) {
        updateDomTreeByGuardRegion(m_li, m_gdhelp, domtree);
        return;
    }
    if (m_preheader != nullptr) {
        updateDomTreeByPreheader(m_li, m_preheader, domtree);
    }
}


bool InsertPreheaderMgr::perform(IRTab const& irtab, OUT LICM * licm,
                                 RCE const* rce, HoistCtx & ctx)
{
    //Always insert a preheader to facilitate the insertion of guard-BB.
    m_preheader = nullptr;
    bool insert_prehead = xoc::insertPreheader(m_li, m_rg, &m_preheader, m_oc,
                                               true);
    ctx.cfg_changed |= insert_prehead;
    ASSERT0(!insert_prehead || m_preheader);
    if (!m_oc->is_dom_valid()) {
        m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_DOM, PASS_UNDEF);
    }
    checkAndInsertGuardBB(irtab, licm, rce, ctx);
    updateDomTree(*ctx.domtree);
    ASSERT0(m_oc->is_dom_valid());
    ASSERT0(ctx.is_domtree_valid());
    return insert_prehead;
}
 

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
        if (!isWorthHoist(ir)) { continue; }
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
    ASSERTN(head, ("loopinfo is ivalid"));
    UINT headid = head->id();
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        if (i != (BSIdx)headid && !m_cfg->is_dom(headid, i)) {
            //Loop head should anticipate into analysis as well.
            //The candidate BB must dominate all other loop body BBs.
            continue;
        }
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && bb->getVex());
        find |= scanBB(bb, li, islegal, first_scan);
        if (!(*islegal)) {
            //Whole loop is unsuite to hoist.
            return false;
        }
    }
    return find;
}


bool LICM::chooseExpList(LI<IRBB> * li, IR * ir, OUT bool & all_exp_invariant,
                         IRIter & irit)
{
    bool find = false;
    for (IR * x = ir; x != nullptr; x = x->get_next()) {
        IRList * invlist = m_irs_mgr.alloc();
        ASSERT0(x->is_exp());
        bool invariant = false;
        find |= chooseExp(li, x, irit, &invariant, invlist);
        if (!invariant) { all_exp_invariant = false; }
        addHoistCand(*invlist);
        m_irs_mgr.free(invlist);
    }
    return find;
}


//ir: may be exp or stmt.
bool LICM::chooseKid(LI<IRBB> * li, IR * ir, OUT bool & all_kid_invariant,
                     IRIter & irit)
{
    bool find = false;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        find |= chooseExpList(li, kid, all_kid_invariant, irit);
    }
    if (!all_kid_invariant) {
        return find;
    }
    if (ir->is_stmt() && !markedAsInvStmt(ir)) {
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
    bool all_param_invariant = true;
    bool find = chooseExpList(li, CALL_param_list(ir), all_param_invariant,
                              irit);
    if (!all_param_invariant || !ir->isReadOnly()) {
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


bool LICM::canBeRegardAsInvExp(IR const* ir) const
{
    switch (ir->getCode()) {
    case IR_CONST:
    case IR_LDA:
    case IR_CASE:
    case IR_ID:
        return true;
    default:
        return markedAsInvExp(ir);
    }
    return false;
}


//Return true if exp in list is marked and collected into invariant-exp set.
bool LICM::canBeRegardAsInvExpList(IR const* explst) const
{
    for (IR const* exp = explst; exp != nullptr; exp = exp->get_next()) {
        ASSERT0(exp->is_exp());
        if (!canBeRegardAsInvExp(exp)) {
            return false;
        }
    }
    return true;
}


bool LICM::chooseBin(LI<IRBB> * li, IR * ir, IRIter & irit,
                     OUT bool * all_exp_invariant, OUT IRList * invlist)
{
    *all_exp_invariant = false;
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
        find = true; //operation can be regarded as invariant.
    } else {
        addHoistCand(*invlist0);
        addHoistCand(*invlist1);
    }
    m_irs_mgr.free(invlist0);
    m_irs_mgr.free(invlist1);
    return find;
}


bool LICM::chooseUna(LI<IRBB> * li, IR * ir, IRIter & irit,
                     OUT bool * all_exp_invariant, OUT IRList * invlist)
{
    *all_exp_invariant = false;
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
        find = true; //operation can be regarded as invariant.
    } else {
        addHoistCand(*invlist0);
    }
    m_irs_mgr.free(invlist0);
    return find;
}


bool LICM::chooseArray(LI<IRBB> * li, IR * ir, IRIter & irit,
                       OUT bool * all_exp_invariant, OUT IRList * invlist)
{
    *all_exp_invariant = false;
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
        find = true; //operation can be regarded as invariant.
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


bool LICM::chooseILD(LI<IRBB> * li, IR * ir, IRIter & irit,
                     OUT bool * all_exp_invariant, OUT IRList * invlist)
{
    *all_exp_invariant = false;
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
        find = true; //operation can be regarded as invariant.
    } else {
        addHoistCand(*invlist0);
    }
    m_irs_mgr.free(invlist0);
    return find;
}


bool LICM::choosePR(LI<IRBB> * li, IR * ir, IRIter & irit,
                    OUT bool * all_exp_invariant, OUT IRList * invlist)
{
    *all_exp_invariant = false;
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
}


bool LICM::chooseLD(LI<IRBB> * li, IR * ir, IRIter & irit,
                    OUT bool * all_exp_invariant, OUT IRList * invlist)
{
    *all_exp_invariant = false;
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
}


bool LICM::chooseSELECT(LI<IRBB> * li, IR * ir, IRIter & irit,
                        OUT bool * all_exp_invariant, OUT IRList * invlist)
{
    *all_exp_invariant = false;
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
    find |= chooseExp(li, SELECT_det(ir), irit, &op2_all_inv,
                      invlist2);

    if (op0_all_inv && op1_all_inv && op2_all_inv) {
        invlist->append_tail(ir);
        *all_exp_invariant = true;
        addInvariantExp(ir);
        find = true; //operation can be regarded as invariant.
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
    switch (ir->getCode()) {
    SWITCH_CASE_BIN:
        return chooseBin(li, ir, irit, all_exp_invariant, invlist);
    SWITCH_CASE_UNA:
        return chooseUna(li, ir, irit, all_exp_invariant, invlist);
    SWITCH_CASE_READ_ARRAY:
        return chooseArray(li, ir, irit, all_exp_invariant, invlist);
    SWITCH_CASE_INDIRECT_MEM_EXP:
        return chooseILD(li, ir, irit, all_exp_invariant, invlist);
    SWITCH_CASE_READ_PR:
        return choosePR(li, ir, irit, all_exp_invariant, invlist);
    SWITCH_CASE_DIRECT_MEM_EXP:
        return chooseLD(li, ir, irit, all_exp_invariant, invlist);
    case IR_CONST:
    case IR_LDA:
    case IR_CASE:
        *all_exp_invariant = true;
        return false;
    case IR_SELECT:
        return chooseSELECT(li, ir, irit, all_exp_invariant, invlist);
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
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    case IR_STPR: { //Usually other PR operations is unrewarding to hoist.
        bool all_kid_invariant = true;
        return chooseKid(li, ir, all_kid_invariant, irit);
    }
    SWITCH_CASE_CALL:
        return chooseCallStmt(li, ir, irit);
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        return chooseBranch(li, ir, irit);
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
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


//Return true if some stmts are marked as invariant-stmt.
bool LICM::scanDirectStmt(IR * stmt, LI<IRBB> * li)
{
    ASSERT0(stmt->isDirectMemOp() || stmt->isPROp());
    if (!stmt->hasRHS()) { return false; }
    ASSERT0(canBeRegardAsInvExp(stmt->getRHS()));
    if (markedAsInvStmt(stmt)) { return false; }
    MD const* must = stmt->getRefMD();
    ASSERT0(must);
    if (isUniqueDef(must)) {
        if (li->atLeastExecOnce(stmt->getBB()->id(), m_cfg)) {
            addInvariantStmt(stmt);
        }
        return true;
    }
    return false;
}


//Return true if some stmts are marked as invariant-stmt.
bool LICM::scanArrayStmt(IR * stmt, LI<IRBB> * li)
{
    ASSERT0(stmt->isArrayOp());
    ASSERT0(canBeRegardAsInvExp(stmt->getRHS()));
    if (markedAsInvStmt(stmt)) { return false; }
    MD const* must = stmt->getRefMD();
    if (must != nullptr && must->is_effect() && isUniqueDef(must)) {
        if (li->atLeastExecOnce(stmt->getBB()->id(), m_cfg)) {
            addInvariantStmt(stmt);
        }
        return true;
    }
    if (!canBeRegardAsInvExp(ARR_base(stmt))) {
        return false;
    }
    for (IR * subexp = ARR_sub_list(stmt);
         subexp != nullptr; subexp = subexp->get_next()) {
        if (!canBeRegardAsInvExp(subexp)) {
            return false;
        }
    }
    if (li->atLeastExecOnce(stmt->getBB()->id(), m_cfg)) {
        addInvariantStmt(stmt);
    }
    return true;
}


//Return true if some stmts are marked as invariant-stmt.
bool LICM::scanInDirectStmt(IR * stmt, LI<IRBB> * li)
{
    ASSERT0(stmt->isIndirectMemOp());
    ASSERT0(canBeRegardAsInvExp(stmt->getRHS()));
    if (markedAsInvStmt(stmt)) { return false; }
    MD const* must = stmt->getRefMD();
    if (must != nullptr && must->is_effect() && isUniqueDef(must)) {
        if (li->atLeastExecOnce(stmt->getBB()->id(), m_cfg)) {
            addInvariantStmt(stmt);
        }
        return true;
    }
    if (!canBeRegardAsInvExp(stmt->getBase())) {
        return false;
    }
    if (li->atLeastExecOnce(stmt->getBB()->id(), m_cfg)) {
        addInvariantStmt(stmt);
    }
    return true;
}


bool LICM::scanCallStmt(IR * stmt, LI<IRBB> * li)
{
    ASSERT0(canBeRegardAsInvExpList(CALL_param_list(stmt)));
    if (markedAsInvStmt(stmt)) { return false; }
    MD const* must = stmt->getRefMD();
    if ((!stmt->hasReturnValue() || isUniqueDef(must)) && stmt->isReadOnly()) {
        addInvariantStmt(stmt);
        return true;
    }
    return false;
}


//Propagate invariant property to result.
//Return true if some stmts are marked as invariant-stmt.
//The function aim is to generate as more as invariants.
//The function will modify m_invariant_stmt, record if the result of
//stmt become loop invariant.
//Note the function assumes whole RHS tree of stmt in m_analysable_stmt_list
//are loop invariant-exp.
bool LICM::scanResult(LI<IRBB> * li)
{
    bool change = false;
    for (IR * stmt = m_analysable_stmt_list.remove_head(); stmt != nullptr;
         stmt = m_analysable_stmt_list.remove_head()) {
        ASSERT0(stmt->is_stmt());
        switch (stmt->getCode()) {
        SWITCH_CASE_DIRECT_MEM_STMT:
        SWITCH_CASE_WRITE_PR:
            change |= scanDirectStmt(stmt, li);
            break;
        SWITCH_CASE_WRITE_ARRAY:
            change |= scanArrayStmt(stmt, li);
            break;
        SWITCH_CASE_INDIRECT_MEM_STMT:
            change |= scanInDirectStmt(stmt, li);
            break;
        SWITCH_CASE_CALL:
            change |= scanCallStmt(stmt, li);
            break;
        default: ASSERTN(!stmt->isMemRef(), ("TODO"));
        }
    }
    return change;
}


void LICM::updateMD2Num(IR * ir)
{
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_WRITE_PR: {
        MD const* md = ir->getMustRef();
        if (md != nullptr) {
            UINT * n = m_md2num.get(const_cast<MD*>(md));
            if (n == nullptr) {
                n = (UINT*)xmalloc(sizeof(UINT));
                m_md2num.set(md, n);
            }
            (*n)++;
        }
        MDSet const* mds = ir->getMayRef();
        if (mds != nullptr) {
            MDSetIter iter;
            for (BSIdx i = mds->get_first(&iter);
                 i != BS_UNDEF; i = mds->get_next(i, &iter)) {
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
    SWITCH_CASE_CALL: {
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
            for (BSIdx i = mds->get_first(&iter);
                 i != BS_UNDEF; i = mds->get_next(i, &iter)) {
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
    default: ASSERTN(!ir->isMemRef(), ("TODO"));
    }
}


void LICM::dumpHoistedIR(IR const* ir) const
{
    if (ir == nullptr || !m_rg->isLogMgrInit()) { return; }
    if (!g_dump_opt.isDumpAfterPass() || !g_dump_opt.isDumpLICM()) { return; }
    note(getRegion(), "\n-- HOISTED IR: --");
    getRegion()->getLogMgr()->incIndent(2);
    dumpIR(ir, m_rg, nullptr, IR_DUMP_DEF);
    getRegion()->getLogMgr()->decIndent(2);
}


//Given loop info li, dump the invariant stmt and invariant expression.
void LICM::dumpInvariantExpStmt(LI<IRBB> const* li) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    if (!g_dump_opt.isDumpAfterPass() || !g_dump_opt.isDumpLICM()) { return; }
    note(getRegion(),
         "\n==---- DUMP LICM Analysis Result : LoopInfo%d : '%s' ----==\n",
         li->id(), m_rg->getRegionName());
    m_cfg->dumpLoopInfo(m_rg);

    note(getRegion(), "\n");
    if (m_invariant_exp.get_elem_count() > 0) {
        xcom::TTabIter<IR*> ti;
        prt(getRegion(), "-- INVARIANT EXP (NUM=%d) -- :",
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
        prt(getRegion(), "-- INVARIANT STMT (NUM=%d) -- :",
            m_invariant_stmt.get_elem_count());
        getRegion()->getLogMgr()->incIndent(3);
        InvStmtListIter it;
        for (IR * c = m_invariant_stmt.get_head(&it);
             c != nullptr; c = m_invariant_stmt.get_next(&it)) {
             dumpIR(c, m_rg);
        }
        getRegion()->getLogMgr()->decIndent(3);
    }

    note(getRegion(), "\n");
    if (m_hoist_cand.get_elem_count() > 0) {
        xcom::TTabIter<IR*> ti;
        prt(getRegion(), "-- HOIST CAND EXP (NUM=%d) -- :",
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
    cleanBeforeAnlysis();
    bool change = true;
    bool find = false;
    bool first_scan = true;
    while (change) {
        bool islegal = true;
        change = scanLoopBody(li, &islegal, first_scan);
        if (!islegal) {
            m_hoist_cand.reinit();
            find = false;
            break;
        }
        if (change) {
            find = true;
            //m_analysable_stmt_list will be empty when function return.
            scanResult(li);

            //Before next round analysis, we must make sure all
            //stmts in this list is invariant or not.
            ASSERT0(m_analysable_stmt_list.get_elem_count() == 0);
        }
        first_scan = false;
    }
    //Dump invariant info here because they will be replaced soon.
    dumpInvariantExpStmt(li);
    cleanAfterAnlysis();
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


//Return true if exp in list is marked and collected into invariant-exp set.
bool LICM::markedAsInvExpList(IR const* explst) const
{
    for (IR const* exp = explst; exp != nullptr; exp = exp->get_next()) {
        ASSERT0(exp->is_exp());
        if (!m_invariant_exp.find(const_cast<IR*>(exp))) {
            return false;
        }
    }
    return true;
}


//Return true if any stmt that is related to invariant stmt
//is moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
bool LICM::hoistDefByPRSSA(IR const* exp, OUT IRBB * prehead,
                           OUT LI<IRBB> * li, MOD HoistCtx & ctx) const
{
    SSAInfo * info = exp->getSSAInfo();
    ASSERTN(info, ("miss PRSSAInfo"));
    //Check if SSA def is loop invariant.
    IR * def = SSA_def(info);
    if (def == nullptr) { return true; }
    if (def->is_phi()) { return false; }
    if (!li->isInsideLoop(def->getBB()->id())) {
        return true;
    }
    if (!isWorthHoist(def) || !tryHoistDefStmt(def, prehead, li, ctx)) {
        return false;
    }
    dumpHoistedIR(def);
    return true;
}


//Return true if any stmt that is related to invariant stmt
//is moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
bool LICM::hoistDefByClassicDU(IR const* exp, OUT IRBB * prehead,
                               OUT LI<IRBB> * li, MOD HoistCtx & ctx) const
{
    ASSERTN(m_rg->getDUMgr(), ("valid DUChain need DUMgr"));
    DUSet const* defset = exp->readDUSet();
    if (defset == nullptr) { return true; }
    DUSetIter di = nullptr;
    BSIdx nexti;
    for (BSIdx i = defset->get_first(&di); i != BS_UNDEF; i = nexti) {
        nexti = defset->get_next(i, &di);
        IR * def = m_rg->getIR(i);
        ASSERT0(def);
        if (!isWorthHoist(def) || !tryHoistDefStmt(def, prehead, li, ctx)) {
            return false;
        }
        dumpHoistedIR(def);
    }
    return true;
}


//Return true if all DEF stmt/phi that is related to invariant stmt
//are moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
//Note some DEF that has been hoisted by this function is recorded in 'ctx'
//even not all of DEF hoisted totally.
bool LICM::hoistDefByMDSSA(IR const* exp, OUT IRBB * prehead,
                           OUT LI<IRBB> * li, MOD HoistCtx & ctx) const
{
    MDSSAInfo * info = m_mdssamgr->getMDSSAInfoIfAny(exp);
    ASSERTN(info, ("def stmt even not in MDSSA system"));
    VOpndSetIter it = nullptr;
    VOpndSet const& vset = info->readVOpndSet();
    UseDefMgr const* udmgr = m_mdssamgr->getUseDefMgr();
    BSIdx nexti;
    for (BSIdx i = vset.get_first(&it); i != BS_UNDEF; i = nexti) {
        nexti = vset.get_next(i, &it);
        VMD const* vmd = (VMD*)udmgr->getVOpnd(i);
        if (vmd == nullptr) {
            //CASE:licm_mdssa2.c
            //current i may have been removed by tryHoistDefStmt at last
            //round of the iteration.
            continue;
        }
        ASSERT0(vmd->is_md());
        MDDef const* def = vmd->getDef();
        if (def == nullptr) {
            //Region livein PR.
            continue;
        }
        ASSERT0(def->getBB());
        if (!li->isInsideLoop(def->getBB()->id())) {
            //Outside loop def.
            continue;
        }
        if (def->is_phi()) {
            if (m_mdssamgr->isOverConservativeDUChain(def, exp)) {
                continue;
            }
            return false;
        }
        IR * stmt = def->getOcc();
        if (!isWorthHoist(stmt) || !tryHoistDefStmt(stmt, prehead, li, ctx)) {
            return false;
        }
        //Note after stmt hoisted, vset may changed.
        if (!vset.is_contain(nexti)) {
            //CASE:rp3.c
            i = vset.get_first(&it);
        }
        dumpHoistedIR(stmt);
    }
    return true;
}


//Return true if any stmt that is related to invariant stmt
//is moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
bool LICM::hoistDefByDUChain(IR const* exp, OUT IRBB * prehead,
                             OUT LI<IRBB> * li, MOD HoistCtx & ctx) const
{
    ASSERT0(exp->is_exp());
    if (!exp->isMemOpnd()) { return true; }
    if (exp->isPROp() && usePRSSADU()) {
        return hoistDefByPRSSA(exp, prehead, li, ctx);
    }
    if (exp->isMemRefNonPR() && useMDSSADU()) {
        return hoistDefByMDSSA(exp, prehead, li, ctx);
    }
    if (ctx.oc->is_pr_du_chain_valid() || ctx.oc->is_nonpr_du_chain_valid()) {
        return hoistDefByClassicDU(exp, prehead, li, ctx);
    }
    return false;
}


void LICM::hoistStmt(LI<IRBB> const* li, MOD IR * stmt, MOD IRBB * prehead,
                     OUT HoistCtx & ctx) const
{
    stmt->getBB()->getIRList().remove(stmt);
    prehead->getIRList().append_tail_ex(stmt);
    ctx.stmt_changed = true;
    ctx.duset_changed = true;
}


//Try hoisting the dependent stmt to 'stmt' firstly.
//Return true if all dependent stmts have been hoisted outside of loop.
bool LICM::tryHoistDependentStmt(MOD IR * stmt, MOD IRBB * prehead,
                                 MOD LI<IRBB> * li, OUT HoistCtx & ctx) const
{
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
    ASSERT0(stmt->getBB());
    if (!isHoistStmt()) { return false; }
    ConstIRIter iriter;
    for (IR const* x = iterExpInitC(stmt, iriter);
         x != nullptr; x = iterExpNextC(iriter, true)) {
        if (!hoistDefByDUChain(x, prehead, li, ctx)) {
            //stmt can not be hoisted.
            return false;
        }
    }
    if (!useMDSSADU()) { return true; }
    bool cross_loophead_phi = false;
    bool cross_nonphi_def = false;
    cross_loophead_phi = m_mdssamgr->isCrossLoopHeadPhi(stmt, li,
                                                        cross_nonphi_def);
    if (cross_nonphi_def) {
        //Illegal hoisting that violate prev-def.
        return false;
    }
    //The code motion do not modify classic DU chain of 'cand_exp' and
    //'stmt'. So it is no need to revise classic DU chain.
    //But the live-expr, reach-def, avail-reach-def set
    //info of each BB changed.
    //However, MDSSA DU chain changed, maintain MDSSA DU chain if
    //doLoopTree is avaiable.
    return true; //stmt can be hoisted to preheader.
}


void LICM::updateMDSSADUForStmtInLoopBody(MOD IR * stmt,
                                          HoistCtx const& ctx) const
{
    if (!useMDSSADU()) { return; }
    ASSERT0(ctx.oc->is_dom_valid());
    if (MDSSAMgr::hasMDSSAInfo(stmt)) {
        m_mdssamgr->recomputeDUAndDDChain(stmt, *ctx.domtree, *ctx.oc);
    }
    IR * startir = stmt->getBB()->getPrevIR(stmt);
    IRIter it;
    for (IR * x = xoc::iterExpInit(stmt, it);
         x != nullptr; x = xoc::iterExpNext(it, true)) {
        if (MDSSAMgr::hasMDSSAInfo(x)) {
            m_mdssamgr->findAndSetLiveInDef(x, startir, stmt->getBB(),
                                            *ctx.oc);
        }
    }
}


//Return true if stmt is successfully moved outside of loop.
bool LICM::tryHoistStmt(MOD IR * stmt, MOD IRBB * prehead,
                        MOD LI<IRBB> * li, OUT HoistCtx & ctx) const
{
    if (!tryHoistDependentStmt(stmt, prehead, li, ctx)) {
        return false;
    }
    hoistStmt(li, stmt, prehead, ctx);
    updateMDSSADUForStmtInLoopBody(stmt, ctx);
    return true;
}


//Return true if any stmt is moved outside from loop.
bool LICM::tryHoistDefStmt(MOD IR * def, MOD IRBB * prehead, MOD LI<IRBB> * li,
                           MOD HoistCtx & ctx) const
{
    ASSERT0(def->is_stmt());
    if (!li->isInsideLoop(def->getBB()->id())) {
        //The DEF has already moved outside loop.
        return true;
    }
    if (!markedAsInvStmt(def)) { return false; }
    return tryHoistStmt(def, prehead, li, ctx);
}


//Return true if gurard BB of LOOP 'li' has been inserted.
void LICM::setLoopHasBeenGuarded(LI<IRBB> const* li)
{
    m_insert_guard_bb.append(li);
}


//Return true if gurard BB of LOOP 'li' has been inserted.
bool LICM::hasInsertedGuardBB(LI<IRBB> const* li) const
{
    return m_insert_guard_bb.get(li) != nullptr;
}


//Return true if BB or STMT changed.
bool LICM::hoistCandHelper(OUT IR * cand_exp, OUT IRBB * prehead,
                           OUT LI<IRBB> * li, OUT HoistCtx & ctx)
{
    ASSERT0(cand_exp->is_exp());
    IR * cand_stmt = cand_exp->getStmt();
    if (cand_stmt->isStoreStmt() &&
        isImmRHS(cand_exp, cand_stmt) &&
        markedAsInvStmt(cand_stmt) &&
        xoc::isStmtDomAllUseInsideLoop(cand_stmt, li, m_rg, *ctx.oc)) {
        if (tryHoistStmt(cand_stmt, prehead, li, ctx)) {
            dumpHoistedIR(cand_stmt);
            return true;
        }
        //Even if hoisting stmt is not success, we still try more for
        //hoisting exp.
    }

    if (cand_exp->is_const()) {
        //CASE1: given
        //  n = 0x10; //S1
        //No need to build STPR in preheader.
        return false;
    }

    dumpHoistedIR(cand_exp);
    //CASE2: given
    //  n = cand_exp; //S1
    //Generate new stmt S2, change S1 to S3:
    //  p1 = cand_exp; //S2
    //  n = p1; //S3
    //move S2 into prehead BB.
    IR * t = m_rg->getIRMgr()->buildPR(cand_exp->getType());
    if (cand_stmt->hasJudgeDet() && cand_exp == cand_stmt->getJudgeDet()) {
        bool f = cand_stmt->replaceKid(cand_exp,
                                       m_rg->getIRMgr()->buildJudge(t), true);
        ASSERT0_DUMMYUSE(f);
    } else {
        bool f = cand_stmt->replaceKid(cand_exp, t, true);
        ASSERT0_DUMMYUSE(f);
    }

    IR * stpr = m_rg->getIRMgr()->buildStorePR(PR_no(t), t->getType(),
                                               cand_exp);

    //Revise MD info.
    MD const* tmd = m_rg->getMDMgr()->genMDForPR(t);
    t->setRefMD(tmd, m_rg);
    stpr->setRefMD(tmd, m_rg);

    xoc::buildDUChain(stpr, t, m_rg, *ctx.oc);
    prehead->getIRList().append_tail_ex(stpr);
    ctx.duset_changed = true;
    return true;
}


//Try to move and check that each definitions of candidate has been
//already hoisted from loop.
//Return true if all DEF stmt of 'c' has been hoisted.
bool LICM::tryHoistAllDefStmt(IR const* c, IRBB * prehead,
                              OUT LI<IRBB> * li, MOD HoistCtx & ctx)
{
    ConstIRIter irit;
    for (IR const* x = iterInitC(c, irit); x != nullptr; x = iterNextC(irit)) {
        if (!hoistDefByDUChain(x, prehead, li, ctx)) {
            //x's DEF can not be hoisted.
            return false;
        }
    }
    return true;
}


//Hoist candidate IRs to preheader BB.
//This function will maintain RPO if new BB inserted.
//Return true if BB or STMT changed.
bool LICM::hoistCand(OUT IRBB * prehead, OUT LI<IRBB> * li, OUT HoistCtx & ctx)
{
    Vector<IR*> removed;
    TTabIter<IR*> ti;
    bool changed = false;
    while (m_hoist_cand.get_elem_count() > 0) {
        removed.clean();
        for (IR * c = m_hoist_cand.get_first(ti);
             c != nullptr; c = m_hoist_cand.get_next(ti)) {
            ASSERT0(c->is_exp());
            if (!isWorthHoist(c)) { continue; }
            if (!li->isInsideLoop(c->getStmt()->getBB()->id())) {
                //Candidate expression has been moved to preheader.
                //e.g:stpr $1 = add (ld gp, 0x1);  //S1
                //    st m = ild $1;  //S2
                //Both 'add' and 'ild' are cand-expression.
                //First, we choose moving S2 to preheader first.
                //Whereas according to the dependence
                //relation of $1, the DEF STMT of $1 will be moved to
                //preheader, namely, S1.
                //Thus when next iteration, we are going to moving cand-exp
                //'add', we find it has been moved to preheader.
                continue;
            }
            HoistCtx lctx(ctx);
            bool all_defs_hoisted = tryHoistAllDefStmt(c, prehead, li, lctx);
            //Record the status if some DEF has been hoisted.
            changed |= lctx.stmt_changed;
            ctx.unionBottomUpInfo(lctx);
            if (!all_defs_hoisted) {
                continue;
            }
            removed.append(c);
            changed |= hoistCandHelper(c, prehead, li, ctx);
        }
        if (removed.get_elem_count() == 0) {
            //No candicate saftified the hoisting condition.
            return changed;
        }
        for (UINT i = 0; i < removed.get_elem_count(); i++) {
            IR * c = removed.get(i);
            ASSERT0(c);
            m_hoist_cand.remove(c);
        }
    }
    return changed;
}


void LICM::cleanBeforeAnlysis()
{
    m_invariant_stmt.reinit();
    m_invariant_exp.reinit();
    m_analysable_stmt_list.reinit();
    m_md2num.reinit();
    m_hoist_cand.reinit();
}


void LICM::cleanAfterAnlysis()
{
    m_invariant_stmt.reinit();
    m_invariant_exp.reinit();
    m_analysable_stmt_list.reinit();
    m_md2num.reinit();
}


bool LICM::processLoop(LI<IRBB> * li, HoistCtx & ctx)
{
    ASSERT0(ctx.oc->is_loopinfo_valid());
    if (!analysis(li)) { return false; }
    ASSERT0(m_hoist_cand.get_elem_count() > 0);
    InsertPreheaderMgr insertmgr(m_rg, ctx.oc, li);
    if (insertmgr.needComplicatedGuard()) { return false; }
    bool insert_prehead = insertmgr.perform(getCandExpTab(), this, m_rce, ctx);
    IRBB * preheader = insertmgr.getPreheader();
    bool succ_hoisted = hoistCand(preheader, li, ctx);
    bool changed = false;
    if (!succ_hoisted) {
        insertmgr.undoCFGChange(ctx);
        return false;
    }
    insertmgr.reviseSSADU(ctx);
    changed |= insert_prehead;
    changed |= ctx.duset_changed;

    //Check if preheader still on CFG.
    bool splitnewbb = false;
    if (m_cfg->isVertex(preheader->id())) {
        //---- Maintain BB characters.
        //hoistCand may append stmt into BB which has down-boundary stmt.
        //That makes BB invalid. Split such invalid BB into two or more BBs.
        splitnewbb = m_cfg->splitBBIfNeeded(preheader, *ctx.oc);
        ASSERTN(!splitnewbb, ("Would this happen?"));
        ctx.cfg_changed |= splitnewbb;
    }
    ctx.cleanAfterLoop();
    return changed;
}


//Return true if code motion happened.
//The funtion will maintain LoopInfo.
bool LICM::doLoopTree(LI<IRBB> * li, HoistCtx & ctx)
{
    if (li == nullptr) { return false; }
    bool changed = false;
    for (LI<IRBB> * tli = li; tli != nullptr; tli = LI_next(tli)) {
        changed |= doLoopTree(LI_inner_list(tli), ctx);
        changed |= processLoop(tli, ctx);
    }
    return changed;
}


bool LICM::dump() const
{
    if (!m_rg->isLogMgrInit()) { return true; }
    if (!g_dump_opt.isDumpAfterPass() || !g_dump_opt.isDumpLICM()) {
        return true;
    }
    m_rg->getLogMgr()->pauseBuffer();
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    //Invariant Variable info has been dumpped during the transformation.
    Pass::dump();
    m_rg->getLogMgr()->resumeBuffer();
    return true;
}


void LICM::postProcessIfChanged(HoistCtx const& hoistctx, OptCtx & oc)
{
    if (hoistctx.cfg_changed) {
        //CASE:compile/rp13.c, can not update RPO for some new BB.
        //ASSERT0(oc.is_rpo_valid());
        ASSERT0(m_cfg->verifyRPO(oc));

        //For conservative purpose, we hope to recompute RPO BB list
        //when it is needed.
        m_cfg->freeRPOVexList();

        //LOOP, DOM are maintained, but CDG is not.
        ASSERT0(oc.is_dom_valid());
    }
    ASSERT0(m_cfg->verifyRPO(oc));
    ASSERT0(m_cfg->verifyLoopInfo(oc));
    OC_is_expr_tab_valid(oc) = false;
    if (hoistctx.duset_changed) {
        OC_is_live_expr_valid(oc) = false;
        OC_is_avail_reach_def_valid(oc) = false;
        OC_is_reach_def_valid(oc) = false;
        if (m_rce != nullptr && m_rce->is_use_gvn()) {
            m_rce->getGVN()->set_valid(false);
        }
    }
    m_cfg->performMiscOpt(oc);
    dump();
    //DU chain and DU ref is maintained.
    ASSERT0(m_dumgr->verifyMDRef());
    ASSERT0(verifyMDDUChain(m_rg, oc));
    ASSERT0(m_cfg->verifyRPO(oc));
    ASSERT0(m_cfg->verifyDomAndPdom(oc));
    ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg));
    ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
}


void LICM::postProcess(HoistCtx const& hoistctx, bool change, OptCtx & oc)
{
    if (change) {
        postProcessIfChanged(hoistctx, oc);
        return;
    }
    m_rg->getLogMgr()->cleanBuffer();
    if (hoistctx.cfg_changed) {
        //CASE:compile/licm_undocfg2.c, needless Label has been added should be
        //remove here. Otherwise the needless label will mess with DCE to
        //return status changed which can not end up ScalarOpt in limiit times.
        if (g_do_cfg_remove_redundant_label) {
            //Note the optimization does not change CFG.
            m_cfg->removeRedundantLabel();
        }
    }
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
        //The pass use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //The pass use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    START_TIMER(t, getPassName());
    if (is_aggressive()) {
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_GVN,
                                                   PASS_RCE, PASS_LOOP_INFO,
                                                   PASS_UNDEF);
    } else {
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_GVN,
                                                   PASS_LOOP_INFO,
                                                   PASS_UNDEF);
    }
    m_rce = (RCE*)m_rg->getPassMgr()->queryPass(PASS_RCE);
    if (m_rce != nullptr && m_rce->is_use_gvn()) {
        GVN * gvn = (GVN*)m_rg->getPassMgr()->queryPass(PASS_GVN);
        if (!gvn->is_valid()) {
            gvn->perform(oc);
        }
    }
    DumpBufferSwitch buff(m_rg->getLogMgr());
    DomTree domtree;
    HoistCtx ctx(&oc, &domtree, m_cfg);
    ctx.buildDomTree(m_cfg);
    bool change = doLoopTree(m_cfg->getLoopInfo(), ctx);
    postProcess(ctx, change, oc);
    m_irs_mgr.clean();
    END_TIMER(t, getPassName());
    return change;
}
//END LICM

} //namespace xoc
