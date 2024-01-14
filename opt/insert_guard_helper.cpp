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

static bool isCopyPropStmt(IR const* ir)
{
    return ir->is_stpr() && ir->getRHS()->is_pr();
}


static IR const* findLoopHeadPhi(LI<IRBB> const* li, IR const* ir)
{
    ASSERT0(ir->is_stmt());
    for (; !ir->is_phi() && ir != nullptr;) {
        if (!isCopyPropStmt(ir)) { return nullptr; }
        SSAInfo const* ssainfo = ir->getRHS()->getSSAInfo();
        ASSERT0(ssainfo);
        ir = ssainfo->getDef();
    }
    if (ir == nullptr || !ir->is_phi() || ir->getBB() != li->getLoopHead()) {
        return nullptr;
    }
    return ir;
}


//
//START xxx
//
MDID2PhiMap::MDID2PhiMap(IRBB const* bb, InsertGuardHelper const& helper)
{
    MDSSAMgr * mgr = helper.getMDSSAMgr();
    MDPhiList * philist = mgr->getPhiList(bb);
    if (philist == nullptr) { return; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        VMD const* res = phi->getResult();
        ASSERT0(res);
        set(res->mdid(), phi);
    }
}
//END xxx


//
//START InsertGuardHelper
//
//Return true if the determinate-expression of loop and related DU chain are too
//complicated to analysz and recompute.
bool InsertGuardHelper::hasComplicatedDefForPR(LI<IRBB> const* li,
                                               IR const* ir) const
{
    ASSERT0(ir->isPROp());
    if (!usePRSSADU()) { return true; }
    SSAInfo const* info = ir->getSSAInfo();
    ASSERT0(info);
    IR const* def = info->getDef();
    if (def == nullptr) {
        //Region livein PR.
        return false;
    }
    ASSERT0(def->getBB());
    if (!li->isInsideLoop(def->getBB()->id())) {
        //Outside loop def.
        return false;
    }
    if (findLoopHeadPhi(li, def) == nullptr) {
        return true;
    }
    ASSERTN(def->getBB() == ir->getStmt()->getBB(),
            ("DEF is in same BB with 'ir'."));
    return false;
}


//Return true if the determinate-expression of loop and related DU chain are too
//complicated to analysz and recompute.
bool InsertGuardHelper::hasComplicatedDefForNonPR(LI<IRBB> const* li,
                                                  IR const* ir) const
{
    ASSERT0(ir->isMemRefNonPR());
    if (!useMDSSADU()) { return true; }
    MDSSAInfo const* info = m_mdssa->getMDSSAInfoIfAny(ir);
    ASSERT0(info);
    VOpndSetIter it = nullptr;
    VOpndSet const& vset = info->readVOpndSet();
    UseDefMgr const* udmgr = m_mdssa->getUseDefMgr();
    for (BSIdx i = vset.get_first(&it); i != BS_UNDEF;
         i = vset.get_next(i, &it)) {
        VMD const* vmd = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(vmd && vmd->is_md());
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
        if (def->is_phi() && def->getBB() == ir->getStmt()->getBB()) {
            //Defined by PHI.
            continue;
        }
        return true;
    }
    return false;
}


bool InsertGuardHelper::hasComplicatedDef(LI<IRBB> const* li,
                                          IR const* ir) const
{
    ASSERT0(ir->is_exp());
    if (ir->isPROp()) {
        return hasComplicatedDefForPR(li, ir);
    }
    return hasComplicatedDefForNonPR(li, ir);
}


void InsertGuardHelper::removeGuardRegion(MOD DomTree & domtree)
{
    ASSERT0(m_guard_start && m_guarded_bb && m_guard_end);
    //Remove stmts in guard_start.
    for (IR * ir = m_guard_start->getIRList().get_head();
         ir != nullptr; ir = m_guard_start->getIRList().get_next()) {
        xoc::removeStmt(ir, m_rg, *getOptCtx());
        m_rg->freeIRTree(ir);
    }
    m_guard_start->getIRList().clean();
    ASSERT0(!useMDSSADU() || !m_mdssa->hasPhi(m_guard_start));

    //Remove stmts in guarded_bb.
    for (IR * ir = m_guarded_bb->getIRList().get_head();
         ir != nullptr; ir = m_guarded_bb->getIRList().get_next()) {
        xoc::removeStmt(ir, m_rg, *getOptCtx());
        m_rg->freeIRTree(ir);
    }
    m_guarded_bb->getIRList().clean();
    ASSERT0(!useMDSSADU() || !m_mdssa->hasPhi(m_guarded_bb));

    //Remove stmts in guard_end.
    for (IR * ir = m_guard_end->getIRList().get_head();
         ir != nullptr; ir = m_guard_end->getIRList().get_next()) {
        xoc::removeStmt(ir, m_rg, *getOptCtx());
        m_rg->freeIRTree(ir);
    }
    m_guard_end->getIRList().clean();
    ASSERT0(!useMDSSADU() || !m_mdssa->hasPhi(m_guard_end));

    //Remove other remained emtpy BB.
    CfgOptCtx coctx(*getOptCtx());
    m_cfg->removeEdge(m_guard_start, m_guard_end, coctx);
    RemoveEmptyBBCtx rmctx(coctx);
    rmctx.setRecordRemovedBB();
    bool res = m_cfg->removeEmptyBB(rmctx);
    ASSERT0_DUMMYUSE(res);

    //Update DomTree if BB removed.
    ASSERT0(rmctx.getRemovedList());
    for (UINT bbid = rmctx.getRemovedList()->get_head();
         bbid != BBID_UNDEF; bbid = rmctx.getRemovedList()->get_next()) {
        domtree.remove(bbid);
    }

    //CFG's removeEmptyBB optimization only maintained these frequently
    //used CFG info. Make sure they are maintained well.
    OptCtx::setInvalidIfCFGChangedExcept(getOptCtx(), PASS_RPO,
                                         PASS_DOM, PASS_LOOP_INFO,
                                         PASS_UNDEF);
    m_cfg->removeRedundantLabel();
}


bool InsertGuardHelper::needComplicatedGuard(LI<IRBB> const* li) const
{
    IRBB * head = li->getLoopHead();
    IR * last = head->getLastIR();
    if (last == nullptr || !last->isConditionalBr()) {
        //CASE:compile/preheader.c
        return false;
    }
    ASSERT0(last->is_single());
    IR const* head_det = BR_det(last);
    ConstIRIter it;
    for (IR const* x = iterInitC(head_det, it, true);
         x != nullptr; x = iterNextC(it, true)) {
        if (!x->isPROp() && !x->isMemRefNonPR()) { continue; }
        if (hasComplicatedDef(li, x)) { return true; }
    }
    return false;
}


LabelInfo const* InsertGuardHelper::addJumpEdge(IRBB * guard_start,
                                                IRBB * guard_end)
{
    //Given guard start, end, guarded, and the next-of-guarded BBs.
    //  BB_guard_start
    //  |
    //  v
    //  BB_guarded_bb
    //  |
    //  v
    //  BB_guard_end
    //  |
    //  v
    //  BB_next_to_guarded <--
    //    st x=...
    //    st y=...
    //    falsebr ...
    //After adding edge:
    //  BB_guard_start
    //  |   |
    //  |   v
    //  |  BB_guarded_bb
    //  |   /
    //  |  /
    //  v v
    //  BB_guard_end
    //   |
    //   v
    //  BB_next_to_guarded <--
    //    st x=...
    //    st y=...
    //    falsebr ...
    LabelInfo const* guard_end_lab = guard_end->getLabelList().get_head();
    if (guard_end_lab == nullptr) {
        guard_end_lab = m_rg->genILabel();
        m_cfg->addLabel(guard_end, guard_end_lab);
    }
    CfgOptCtx ctx(*m_oc);
    m_cfg->addEdge(guard_start, guard_end, ctx);
    return guard_end_lab;
}


IR * InsertGuardHelper::insertGuardIR(IRBB * guard_start, IRBB * nextto_end,
                                      LabelInfo const* guard_end_lab)
{
    //-- Build guard-branch stmt of guard_start.
    IR * nextto_br = nextto_end->getLastIR();
    ASSERT0(nextto_br && nextto_br->isConditionalBr());
    ASSERT0(nextto_br->is_single());
    IR * newdet = m_rg->dupIRTree(BR_det(nextto_br));
    if (nextto_br->is_truebr()) {
        //Make sure the guard-branch is FALSEBR because FALSEBR uses
        //fewer instructions than TRUEBR.
        Refine::invertCondition(&newdet, m_rg);
    }

    //-- Set the target label of guard-branch.
    ASSERT0(guard_end_lab);
    IR * guard_br = m_rg->getIRMgr()->buildBranch(false, newdet,
                                                  guard_end_lab);

    //Append the guard-branch into guard_start.
    guard_start->getIRList().append_tail_ex(guard_br);
    return guard_br;
}


void InsertGuardHelper::updateGuardDUChain(LI<IRBB> const* li, IR * guard_br,
                                           IRBB * guard_end,
                                           IR * loophead_br)
{
    //Assign MD for all generated new IRs.
    m_rg->getMDMgr()->assignMD(guard_br, true, true);
    if (m_du != nullptr) {
        //Copy the DU chain for generated IR.
        m_du->addUseForTree(BR_det(guard_br), BR_det(loophead_br));
    }
    if (usePRSSADU() || useMDSSADU()) {
        setSSALiveInDef(BR_det(guard_br), *m_oc);
    }
    if (usePRSSADU()) {
        reviseGuardDetPRSSA(li, guard_br, guard_end);
    }
}


//Note DOM info must be valid.
void InsertGuardHelper::insertMDSSAPhiForGuardedStmt(IR * ir,
                                                     DomTree const& domtree)
{
    ASSERT0(ir->is_stmt() && ir->isMemRefNonPR());
    ASSERT0(m_guard_end && m_guard_start && m_guarded_bb);
    ASSERT0(useMDSSADU());
    ASSERT0(m_oc->is_dom_valid());
    MDSSAInfo const* mdssainfo = m_mdssa->getMDSSAInfoIfAny(ir);
    ASSERT0(mdssainfo);
    xcom::Vertex * endv = m_cfg->getVertex(m_guard_end->id());
    ASSERT0_DUMMYUSE(endv);
    UINT numpred = m_guard_end->getNumOfPred();
    VOpndSetIter it = nullptr;
    MDID2PhiMap mdid2phi(m_guard_end, *this);
    for (BSIdx i = mdssainfo->readVOpndSet().get_first(&it);
        i != BS_UNDEF; i = mdssainfo->readVOpndSet().get_next(i, &it)) {
        VMD * vmd = m_mdssa->getVMD(i);
        ASSERT0(vmd && vmd->is_md());
        MDPhi * phi = mdid2phi.get(vmd->mdid());
        if (phi == nullptr) {
            phi = m_mdssa->insertPhiWithNewVersion(vmd->mdid(),
                                                   m_guard_end, numpred);
        }
        m_mdssa->recomputeDefDefAndDefUseChain(phi, domtree);
        m_mdssa->recomputeDefForOpnd(phi, *getOptCtx());
    }
}


IR * InsertGuardHelper::insertPRSSAPhiForGuardedStmt(IR * ir)
{
    ASSERT0(ir->is_stmt() && ir->isPROp() && !ir->is_phi());
    ASSERT0(m_guard_end && m_guard_start && m_guarded_bb);
    ASSERT0(usePRSSADU());
    IR * opnds = nullptr;

    //Livein PR as a placeholder which does not have any DEF.
    IR * placeholder = m_rg->getIRMgr()->buildPR(ir->getType());
    m_prssa->genSSAInfo(placeholder);
    xcom::add_next(&opnds, placeholder);

    //A new USE of guarded stmt.
    IR * guardpr = m_rg->getIRMgr()->buildPRdedicated(ir->getPrno(),
                                                      ir->getType());
    xcom::add_next(&opnds, guardpr);

    //A phi that merges the dummy livein and guarded PR.
    IR * phi = m_rg->getIRMgr()->buildPhi(
        m_rg->getIRMgr()->buildPrno(ir->getType()), ir->getType(), opnds);
    m_rg->getMDMgr()->allocRefForIRTree(phi, false);
    m_guard_end->getIRList().append_head(phi);

    //Reorder phi operands in the order of predecessors of guard-end.
    Pred2Opnd p2o;
    p2o.set(m_guard_start->id(), placeholder);
    p2o.set(m_guarded_bb->id(), guardpr);
    m_cfg->sortPhiOpnd(phi, p2o);

    //Substitute DU chain of guarded PR.
    replaceUseOfGuardedStmt(ir, phi);

    //Complete the guarded PR.
    xoc::buildDUChain(ir, guardpr, m_rg, *getOptCtx());
    return phi;
}


//Replace the USE of original stmt to PHI.
//e.g:given guarded stmt in loop body is $15=...
//  after moving to guarded BB, the layout will be:
//       GuardBranchCondition
//      /       |
//  #GuardBB:   |
//  $15=...     |
//      \       |
//       $45 phi=...
//  Replace the USE of $15 to $45.
void InsertGuardHelper::replaceUseOfGuardedStmt(IR * guarded, IR * phi) const
{
    ASSERT0(guarded->isPROp());
    DefMiscBitSetMgr sm;
    IRSet useset(sm.getSegMgr());
    xoc::collectUseSet(guarded, m_rg, &useset);
    IRSetIter it;
    BSIdx ni = BS_UNDEF;
    for (BSIdx i = useset.get_first(&it); it != nullptr; i = ni) {
        ni = useset.get_next(i, &it);
        IR * use = m_rg->getIR(i);
        ASSERT0(use->isPROp());
        IR const* stmt = use->getStmt();
        ASSERT0(stmt && stmt->getBB());
        if (stmt->getBB() == m_guarded_bb) {
            //Some USEs of guarded-stmt have been moved to guarded BB also.
            //Their DU chain do not need change.
            //e.g:licm_insert_guard.gr
            useset.diff(i);
        }
    }
    xoc::changeDefForPartialUseSet(guarded, phi, useset, m_rg);
}


void InsertGuardHelper::insertPhiForGuardedBB(DomTree const& domtree)
{
    ASSERT0(m_guarded_bb);
    BBIRListIter it;
    for (IR * ir = m_guarded_bb->getIRList().get_head(&it);
         ir != nullptr; ir = m_guarded_bb->getIRList().get_next(&it)) {
        if (ir->isPROp() && usePRSSADU() &&
            !haveAllUseMoveToGuardBBInPRSSA(ir)) {
            insertPRSSAPhiForGuardedStmt(ir);
            continue;
        }
        if (ir->isMemRefNonPR() && useMDSSADU() &&
            !haveAllUseMoveToGuardBBInMDSSA(ir)) {
            insertMDSSAPhiForGuardedStmt(ir, domtree);
            continue;
        }
    }
}


bool InsertGuardHelper::haveAllUseMoveToGuardBBInPRSSA(IR const* ir) const
{
    ASSERT0(ir->is_stmt() && ir->isPROp());
    ASSERT0(usePRSSADU());
    SSAInfo const* info = ir->getSSAInfo();
    ASSERT0(info);
    SSAUseIter uit = nullptr;
    for (BSIdx i = info->getUses().get_first(&uit);
         uit != nullptr; i = info->getUses().get_next(i, &uit)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use->isReadPR());
        ASSERT0(use->getStmt() && use->getStmt()->getBB());
        if (use->getStmt()->getBB() != m_guarded_bb) {
            return false;
        }
    }
    return true;
}


bool InsertGuardHelper::haveAllUseMoveToGuardBBInMDSSA(IR const* ir) const
{
    ASSERT0(ir->is_stmt() && ir->isMemRefNonPR());
    ASSERT0(useMDSSADU());
    MDSSAInfo const* mdssainfo = m_mdssa->getMDSSAInfoIfAny(ir);
    ASSERT0(mdssainfo);
    VOpndSetIter it = nullptr;
    for (BSIdx i = mdssainfo->readVOpndSet().get_first(&it);
        i != BS_UNDEF; i = mdssainfo->readVOpndSet().get_next(i, &it)) {
        VMD * vmd = m_mdssa->getVMD(i);
        ASSERT0(vmd && vmd->is_md());
        VMD::UseSetIter vuit;
        for (UINT i = vmd->getUseSet()->get_first(vuit);
             !vuit.end(); i = vmd->getUseSet()->get_next(vuit)) {
            IR const* use = m_rg->getIR(i);
            ASSERT0(use && !use->is_undef());
            if (use->is_id()) {
                MDPhi const* phi = ((CId*)use)->getMDPhi();
                ASSERT0(phi && phi->getBB());
                if (phi->getBB() != m_guarded_bb) {
                    return false;
                }
                continue;
            }
            ASSERT0(use->getStmt() && use->getStmt()->getBB());
            if (use->getStmt()->getBB() != m_guarded_bb) {
                return false;
            }
        }
    }
    return true;
}


//Revise PRSSA for guard branch according to next_to_end BB branch.
//guard branch should equal to next_to_end branch.
void InsertGuardHelper::reviseGuardDetPRSSA(LI<IRBB> const* li, IR * guard_br,
                                            IRBB * guard_end)
{
    ASSERTN(usePRSSADU(), ("meaning in PRSSA"));
    ASSERT0(!needComplicatedGuard(li));
    IRBB * loophead = li->getLoopHead();
    IR * nextto_br = loophead->getLastIR();
    ASSERT0(nextto_br && nextto_br->isConditionalBr());
    ASSERT0(nextto_br->is_single());
    bool is_pred;
    UINT pos = m_cfg->WhichPred(guard_end, loophead, is_pred);
    ASSERT0_DUMMYUSE(is_pred);
    ConstIRIter ith;
    IRIter itg;

    //Memory reference should be identical, but other part can be different.
    //e.g: EQ ld a, ld b;
    //  after transform:
    //     NE ld c, ld d;
    //The function only aware of memory reference, rather the code of NE, EQ.
    //Thus x and y are no need to be identical totally.
    //ASSERT0(x->isIREqual(y, true));

    IR const* x = iterInitC(BR_det(nextto_br), ith, true);
    IR * guard_det = BR_det(guard_br);
    IR * y = iterInit(guard_det, itg, true);
    IR const* nextx;
    IR * nexty;
    for (; x != nullptr; x = nextx, y = nexty) {
        nextx = iterNextC(ith, true);
        nexty = iterNext(itg, true);
        if (!x->isPROp()) {
            //Only need to fix PRSSA.
            continue;
        }
        ASSERT0(x->isIREqual(y, false));
        SSAInfo const* info = x->getSSAInfo();
        ASSERT0(info);
        IR * def = info->getDef();
        if (def == nullptr) {
            //Livein PR.
            continue;
        }
        ASSERT0(def->getBB());
        if (!li->isInsideLoop(def->getBB()->id())) {
            //Nothing to do for outside loop DEF.
            //Only have to fix DEF that comes from PHI.
            continue;
        }

        //Replace PR in guard IR tree with PHI operand.
        ASSERTN(def->getBB() == loophead, ("complicated branch-condition"));
        IR const* phi = findLoopHeadPhi(li, def);
        ASSERT0(phi);
        IR const* marker = ((CPhi const*)phi)->getOpnd(pos);
        ASSERT0(marker);
        xoc::removeUse(y, m_rg);
        IR * newy = m_rg->dupIR(marker);
        bool change = guard_det->replaceKid(y, newy, true);
        ASSERT0_DUMMYUSE(change);
        xoc::addUse(newy, marker, m_rg);
        m_rg->freeIR(y);
    }
    ASSERT0(x == nullptr && y == nullptr);
}


IRBB * InsertGuardHelper::insertGuardStart(IRBB * guarded_bb)
{
    //Add vertex to graph before updating RPO.
    IRBB * guard_start = m_rg->allocBB();
    m_cfg->insertBBBefore(guarded_bb, guard_start);
    if (!m_cfg->tryUpdateRPO(guard_start, guarded_bb, true)) {
        //TODO: Try update RPO incrementally to avoid recompute
        //whole RPO-Vex list in CFG.
        //m_cfg->tryUpdateRPO(prehead, guard_start, true);
        //Just leave RPO-recomputation to next user for now.
        m_oc->setInvalidRPO();
    }
    bool add_pdom_failed = false;
    m_cfg->addDomInfoToNewIDom(guarded_bb->id(), guard_start->id(),
                               add_pdom_failed);
    if (add_pdom_failed) {
        m_oc->setInvalidPDom();
    }
    return guard_start;
}


IRBB * InsertGuardHelper::insertGuardEnd(IRBB * guarded_bb,
                                         IRBB * next_to_guarded_bb)
{
    //Add vertex to graph before updating RPO.
    IRBB * guard_end = m_rg->allocBB();
    BBListIter guarded_bb_it;
    m_rg->getBBList()->find(guarded_bb, &guarded_bb_it);
    ASSERT0(guarded_bb_it);
    ASSERTN(next_to_guarded_bb, ("TODO:guarded BB is the last"));
    BBListIter next_it;
    m_rg->getBBList()->find(next_to_guarded_bb, &next_it);
    ASSERT0(next_it);
    m_cfg->insertBBBetween(guarded_bb, guarded_bb_it, next_to_guarded_bb,
                           next_it, guard_end, m_oc);
    if (!m_cfg->tryUpdateRPO(guard_end, guarded_bb, false)) {
        //TODO: Try update RPO incrementally to avoid recompute whole BB list.
        //m_cfg->tryUpdateRPO(prehead, guard_start, true);
        //Just leave RPO-recomputation to next user for now.
        m_oc->setInvalidRPO();
    }
    bool add_pdom_failed = false;
    m_cfg->addDomInfoToNewIDom(next_to_guarded_bb->id(), guard_end->id(),
                               add_pdom_failed);
    if (add_pdom_failed) {
        m_oc->setInvalidPDom();
    }
    return guard_end;
}


//Insert guard BB to control the execution of 'prehead'.
//Return the guard BB.
//The function will maintain RPO of generated guard BB.
//prehead: preheader BB of loop.
//li: LoopInfo.
//Return the new guard controlling BB.
IRBB * InsertGuardHelper::insertGuard(LI<IRBB> const* li, IRBB * prehead)
{
    ASSERTN(prehead->getIRList().get_elem_count() == 0,
            ("Need empty preheader"));
    //Insert guard BB and add fallthrough-edge firstly.
    //  BB_prehead
    //  |
    //  V
    //  BB_loophead <--
    //after inserting guard:
    //  BB_guard_start
    //  |
    //  V
    //  BB_prehead
    //  |
    //  V
    //  BB_guard_end
    //  |
    //  V
    //  BB_loophead <--
    IRBB * loophead = li->getLoopHead();
    IRBB * guard_start = insertGuardStart(prehead);
    IRBB * guard_end = insertGuardEnd(prehead, loophead);
    if (!getOptCtx()->is_rpo_valid()) {
        //Recompute or maintain the DOM need RPO.
        m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_RPO, PASS_UNDEF);
    }

    //Preheader does not have PDOM any more.
    LabelInfo const* guard_end_lab = addJumpEdge(guard_start, guard_end);
    if (!m_cfg->changeDomInfoByAddBypassEdge(guard_start->id(), prehead->id(),
                                             guard_end->id())) {
        m_oc->setInvalidDom();
        m_oc->setInvalidPDom();
        m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_DOM, PASS_UNDEF);
    }
    li->addBBToAllOuterLoop(guard_start->id());
    li->addBBToAllOuterLoop(guard_end->id());

    IR * guard_br = insertGuardIR(guard_start, loophead, guard_end_lab);

    //-- DU operation
    IR * loophead_br = loophead->getLastIR();
    updateGuardDUChain(li, guard_br, guard_end, loophead_br);

    //Record the result.
    m_guard_start = guard_start;
    m_guard_end = guard_end;
    m_guarded_bb = prehead;
    return m_guard_start;
}
//END InsertGuardHelper

} //namespace xoc
