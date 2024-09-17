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
//START InsertPhiHelper
//
class InsertPhiHelper {
    COPY_CONSTRUCTOR(InsertPhiHelper);
    LI<IRBB> const* m_li;
    IRCFG * m_cfg;
    Region * m_rg;
    IRMgr * m_irmgr;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    MDSystem * m_mdsys;
    OptCtx const& m_oc;
    Vector<UINT> m_pred_order;
    Vector<UINT> m_pred_pos;
    List<IR*> m_prssa_phis;
    List<MDPhi*> m_mdssa_phis;
private:
    void dumpOrder() const;
    void dumpPhi() const;

    Region * getRegion() const { return m_rg; }
    OptCtx const& getOptCtx() const { return m_oc; }

    void insertPRSSAPhi(IRBB * preheader);
    void insertMDSSAPhi(IRBB * preheader);

    void makePRSSAPhiForPreheader(IRBB const* head);
    void makeMDSSAPhiForPreheader(IRBB * head);
    void makePRSSAPhi(IR const* phi);
    void makeMDSSAPhi(MDPhi const* phi);

    void replacePRSSASuccOpnd(IRBB * preheader, UINT prehead_pos);
    void replaceMDSSASuccOpnd(IRBB * preheader, UINT prehead_pos);
public:
    InsertPhiHelper(LI<IRBB> const* li, IRCFG * cfg, OptCtx const& oc)
        : m_li(li), m_cfg(cfg), m_oc(oc)
    {
        m_rg = cfg->getRegion();
        m_irmgr = m_rg->getIRMgr();
        m_prssamgr = m_rg->getPRSSAMgr();
        m_mdssamgr = m_rg->getMDSSAMgr();
        m_mdsys = m_rg->getMDSystem();
    }

    void insertPhiAtPreheader(IRBB * preheader);
    void dump() const;
    bool preparePhiForPreheader();
};


void InsertPhiHelper::dumpOrder() const
{
    note(getRegion(), "\n==-- InsertPhiHelper:PredOrder --==");
    note(getRegion(), "\n");
    for (UINT i = 0; i < m_pred_order.get_elem_count(); i++) {
        prt(getRegion(), "%d ", m_pred_order.get(i));
    }

    note(getRegion(), "\n==-- InsertPhiHelper:PredPos --==");
    note(getRegion(), "\n");
    for (UINT i = 0; i < m_pred_order.get_elem_count(); i++) {
        prt(getRegion(), "%d ", m_pred_pos.get(i));
    }
}


void InsertPhiHelper::dumpPhi() const
{
    List<IR*>::Iter it;
    for (IR * ir = m_prssa_phis.get_head(&it); ir != nullptr;
         ir = m_prssa_phis.get_next(&it)) {
        dumpIR(ir, m_rg, nullptr,
               DumpFlag::combineIRID(IR_DUMP_KID));
    }

    note(getRegion(), "\n");
    C<MDPhi*> * it2;
    for (MDPhi * phi = m_mdssa_phis.get_head(&it2); phi != nullptr;
         phi = m_mdssa_phis.get_next(&it2)) {
        phi->dump(m_rg, m_mdssamgr->getUseDefMgr());
    }
}


void InsertPhiHelper::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    CHAR const* passname = "InsertPhiHelper";
    START_TIMER_FMT(t, ("DUMP %s", passname));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         passname, m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    dumpOrder();
    dumpPhi();
    m_rg->getLogMgr()->decIndent(2);
    END_TIMER_FMT(t, ("DUMP %s", passname));
}


//Return true if need to insert PHI into preheader if there are multiple
//loop outside predecessor BB of loophead, whereas loophead has PHI.
//e.g: If insert a preheader before BB_loophead, one have to insert PHI at
//     preheader if loophead has PHI.
//   BB_2---
//   |     |
//   v     |
//   BB_4  |    ----
//   |     |   |    |
//   v     v   v    |
//   BB_loophead    |
//   |              |
//   v              |
//   BB_loopbody ---  loophead has PHI.
bool InsertPhiHelper::preparePhiForPreheader()
{
    IRBB * head = m_li->getLoopHead();
    bool has_phi = false;
    IR const* ir = head->getIRList().get_head();
    if (ir != nullptr && ir->is_phi()) {
        has_phi = true;
    }

    bool has_mdphi = false;
    MDSSAMgr const* ssamgr = m_rg->getMDSSAMgr();
    if (ssamgr != nullptr && ssamgr->hasPhi(head)) {
        has_mdphi = true;
    }

    UINT outside_pred_num = 0;
    UINT pos = 0;
    for (xcom::EdgeC const* ec = head->getVex()->getInList();
         ec != nullptr; ec = ec->get_next(), pos++) {
        UINT pred = ec->getFromId();
        if (!m_li->isInsideLoop(pred)) {
            outside_pred_num++;
            m_pred_order.append(pred);
            m_pred_pos.append(pos);
        }
    }
    if (outside_pred_num <= 1 || (!has_phi && !has_mdphi)) { return false; }
    makePRSSAPhiForPreheader(head);
    makeMDSSAPhiForPreheader(head);
    return true;
}


void InsertPhiHelper::makeMDSSAPhi(MDPhi const* phi)
{
    ASSERT0(phi->is_phi());
    IR * new_opnd_list = nullptr;
    IR * last = nullptr;
    UINT i = 0;
    UINT c = 0;
    for (IR * opnd = MDPHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next(), i++) {
        if (c == m_pred_pos.get_elem_count()) { break; }
        UINT pos = m_pred_pos.get(c);
        if (i != pos) { continue; }
        MDSSAInfo const* oldmdssainfo = m_mdssamgr->getMDSSAInfoIfAny(opnd);
        ASSERT0(oldmdssainfo);

        //Generate new operand and MDSSAInfo.
        IR * newopnd = m_rg->dupIRTree(opnd);
        ASSERT0(newopnd->getMustRef());
        m_mdssamgr->copyAndAddMDSSAOcc(newopnd, oldmdssainfo);
        xcom::add_next(&new_opnd_list, &last, newopnd);
        c++;
    }
    ASSERT0(c == m_pred_pos.get_elem_count());

    //Generate new PHI.
    //Note BB of MDPhi have to be set at insertPhiAtPreheader() after
    //preheader generated.
    MDPhi * newphi = m_mdssamgr->genMDPhi(phi->getResult()->mdid(),
                                          new_opnd_list, nullptr);
    m_mdssa_phis.append_tail(newphi);
}


void InsertPhiHelper::makePRSSAPhi(IR const* phi)
{
    ASSERT0(phi->is_phi());
    //Generate PHI.
    //Type cast if host compiler does NOT support C11.
    IR * newphi = m_irmgr->buildPhi(m_irmgr->buildPrno(phi->getType()),
                                    phi->getType(), (IR*)nullptr);
    m_rg->getMDMgr()->allocRef(newphi);
    m_prssa_phis.append_tail(newphi);
    //BB_irlist(preheader).append_head(phi);
    IR * new_opnd_list = nullptr;
    IR * last = nullptr;
    IR * next = nullptr;
    //Pick up operands from loophead PHI.
    UINT i = 0;
    UINT c = 0;
    for (IR * opnd = PHI_opnd_list(phi); opnd != nullptr; opnd = next, i++) {
        next = opnd->get_next();
        if (c == m_pred_pos.get_elem_count()) { break; }
        UINT pos = m_pred_pos.get(c);
        if (i != pos) { continue; }
        IR * newopnd = m_rg->dupIRTree(opnd);
        ASSERT0(newopnd->getMustRef() && opnd->getSSAInfo());
        opnd->getSSAInfo()->addUseAndSSAInfo(newopnd);
        xcom::add_next(&new_opnd_list, &last, newopnd);
        c++;
    }
    ASSERT0(c == m_pred_pos.get_elem_count());
    PHI_opnd_list(newphi) = new_opnd_list;
    newphi->setParent(new_opnd_list);
    m_prssamgr->genNewVersionSSAInfoForStmt(newphi);
}


void InsertPhiHelper::makePRSSAPhiForPreheader(IRBB const* head)
{
    BBIRListIter it;
    IRBB * phead = const_cast<IRBB*>(head);
    for (IR * ir = phead->getIRList().get_head(&it);
         ir != nullptr; ir = phead->getIRList().get_next(&it)) {
        if (!ir->is_phi()) { break; }
        makePRSSAPhi(ir);
    }
}


void InsertPhiHelper::makeMDSSAPhiForPreheader(IRBB * head)
{
    MDPhiList const* lst = m_mdssamgr->getPhiList(head);
    if (lst == nullptr) { return; }
    for (MDPhiListIter it = lst->get_head();
         it != lst->end(); it = lst->get_next(it)) {
        makeMDSSAPhi(it->val());
    }
}


void InsertPhiHelper::insertPRSSAPhi(IRBB * preheader)
{
    List<IR*>::Iter it;
    BBIRList & irlst = preheader->getIRList();
    for (IR * phi = m_prssa_phis.get_tail(&it);
         phi != nullptr; phi = m_prssa_phis.get_prev(&it)) {
        irlst.append_head(phi);
    }
}


void InsertPhiHelper::insertMDSSAPhi(IRBB * preheader)
{
    C<MDPhi*> * it;
    MDPhiList * philst = m_mdssamgr->genPhiList(preheader->id());
    for (MDPhi * phi = m_mdssa_phis.get_tail(&it);
         phi != nullptr; phi = m_mdssa_phis.get_prev(&it)) {
        philst->append_head(phi);
    }

}


void InsertPhiHelper::replaceMDSSASuccOpnd(IRBB * preheader, UINT prehead_pos)
{
    IRBB * loophead = m_li->getLoopHead();
    MDPhiList const* lhphis = m_mdssamgr->getPhiList(loophead);
    MDPhiList const* prephis = m_mdssamgr->getPhiList(preheader);
    MDPhiListIter prephiit = prephis->get_head();
    for (MDPhiListIter lhphi = lhphis->get_head(); lhphi != lhphis->end();
         lhphi = lhphis->get_next(lhphi),
         prephiit = prephis->get_next(prephiit)) {
        MDPhi * prephi = prephiit->val();
        ASSERT0(prephi);
        MDPHI_bb(prephi) = preheader; //Complete the MDPhi info.
        IR * oldopnd = lhphi->val()->getOpnd(prehead_pos);
        ASSERT0(oldopnd && oldopnd->is_id() &&
                MDSSAMgr::getMDSSAInfoIfAny(oldopnd));
        MDSSAUpdateCtx ctx(getOptCtx());
        m_mdssamgr->removeMDSSAOccForTree(oldopnd, ctx);
        m_mdssamgr->buildDUChain(prephi, oldopnd);
    }
}


void InsertPhiHelper::replacePRSSASuccOpnd(IRBB * preheader, UINT prehead_pos)
{
    IRBB * loophead = m_li->getLoopHead();
    BBIRListIter itlh;
    BBIRListIter itpre;
    BBIRList const& lhirlst = loophead->getIRList();
    BBIRList const& preirlst = preheader->getIRList();
    IR * ir_pre = preirlst.get_head(&itpre);
    for (IR * ir_lh = lhirlst.get_head(&itlh); ir_lh != nullptr;
         ir_lh = lhirlst.get_next(&itlh),
         ir_pre = preirlst.get_next(&itpre)) {
        if (!ir_lh->is_phi()) { break; }
        ASSERT0(ir_pre);

        IR * oldopnd = ((CPhi*)ir_lh)->getOpnd(prehead_pos);
        ASSERT0(oldopnd && oldopnd->isPROp() && oldopnd->getSSAInfo());
        PRSSAMgr::removePRSSAOcc(oldopnd);

        IR * newopnd = m_irmgr->buildPRdedicated(
            ir_pre->getPrno(), ir_pre->getType());
        m_rg->getMDMgr()->allocRef(newopnd);
        m_prssamgr->buildDUChain(ir_pre, newopnd);

        ir_lh->replaceKid(oldopnd, newopnd, true);
        m_rg->freeIRTree(oldopnd);
    }
}


//The new PHI will be inserted to preheader.
void InsertPhiHelper::insertPhiAtPreheader(IRBB * preheader)
{
    IRBB * loophead = m_li->getLoopHead();
    bool is_pred;
    UINT prehead_pos = ((DGraph*)m_cfg)->WhichPred(
        preheader->id(), loophead->getVex(), is_pred);
    ASSERT0_DUMMYUSE(is_pred);
    insertPRSSAPhi(preheader);
    replacePRSSASuccOpnd(preheader, prehead_pos);
    insertMDSSAPhi(preheader);
    replaceMDSSASuccOpnd(preheader, prehead_pos);
}
//END InsertPhiHelper


//Find the bb that is the start of the unqiue backedge of loop.
//  BB1: loop start bb
//  BB2: body start bb
//  BB3: goto loop start bb
//BB2 is the loop header fallthrough bb.
bool findTwoSuccessorBBOfLoopHeader(LI<IRBB> const* li, IRCFG const* cfg,
                                    UINT * succ1, UINT * succ2)
{
    ASSERT0(li && cfg && succ1 && succ2);
    IRBB * head = li->getLoopHead();
    xcom::Vertex * headvex = head->getVex();
    if (headvex->getOutDegree() != 2) {
        //Not natural loop.
        return false;
    }

    xcom::EdgeC const* ec = headvex->getOutList();
    ASSERT0(ec && EC_next(ec));

    *succ1 = ec->getToId();
    *succ2 = ec->get_next()->getToId();
    return true;
}


//Append GOTO stmt to 'from' BB, in order to it can jump to 'to' BB.
static IR * tryAppendGotoToJumpToBB(IRBB * from, IRBB * to, Region * rg)
{
    ASSERT0(from && to && rg);
    IR const* last = from->getLastIR();
    if (!IRBB::isLowerBoundary(last)) {
        //Pick any label on 'to' BB to be the jump target.
        LabelInfo const* lab = to->getLabelList().get_head();
        if (lab == nullptr) {
            lab = rg->genILabel();
            rg->getCFG()->addLabel(to, lab);
        }
        IR * gotoir = rg->getIRMgr()->buildGoto(lab);
        from->getIRList().append_tail(gotoir);
        return gotoir;
    }
    if (last->isUnconditionalBr() || last->isConditionalBr()) {
        ASSERTN(last->getLabel() && to->hasLabel(last->getLabel()),
                ("No valid label can be used as target"));
        return nullptr;
    }
    UNREACHABLE();
    return nullptr;
}


//Fixup edge that come from loop-inside BB when preheader inserted.
//pred: predecessor of head which is inside loop body.
static void fixupInnerLoopEdgeBetweenHeadAndPreheader(
    LI<IRBB> const* li, Region * rg, IRBB * pred)
{
    IRBB * head = li->getLoopHead();
    IRCFG * cfg = rg->getCFG();
    //BB_p is predecessor of loophead of LI;
    //BB_1 is also predecessor of loop-header, but it belongs to loop.
    //CASE:
    //   BB_p
    //   |
    // ---
    // |  BB_1<--...
    // |  |
    // |  | //fallthrough
    // v  v
    // BB_head
    //
    //After inserting phreader BB_preheader:
    //
    //   BB_p
    //   |
    // ---
    // |  BB_1<--...
    // |  |
    // |  | //can not be fallthrough, have to be fixed.
    // v  v
    // BB_preheader
    //    |
    //    v
    // BB_head
    //
    //Append GOTO to fix it:
    //
    //   BB_p
    //   |
    // ---
    // |  BB_1<--...
    // |  | //Jump to loop-header
    // |  |_________
    // v            |
    // BB_preheader |
    //    |         |
    //    v         |
    // BB_head <--
    LabelInfo const* lab = head->getLabelList().get_head();
    if (lab == nullptr) {
        lab = rg->genILabel();
        cfg->addLabel(head, lab);
    }
    tryAppendGotoToJumpToBB(pred, head, rg);
}


//Return true if inserted a new BB.
//insert_preheader: true if preheader has been inserted, otherwise do insertion.
static void insertAndUpdateOutterLoopEdge(
    LI<IRBB> const* li, Region * rg, IRBB * pred, BBListIter head_it,
    IRBB * preheader, MOD LabelInfo const** preheader_lab,
    OUT bool & insert_preheader, OptCtx * oc)
{
    IRBB * head = li->getLoopHead();
    IRCFG * cfg = rg->getCFG();
    bool inserted_by_cur_time = false;
    if (!insert_preheader) {
        //Insert preheader in front of head.
        //Note preheader must fallthrough to head.
        BBListIter pred_it;
        cfg->getBBList()->find(pred, &pred_it);
        ASSERT0(pred_it);
        cfg->tryUpdateRPOBeforeCFGChanged(preheader, head, true, oc);
        CfgOptCtx ctx(*oc);
        //No need to maintain DomInfo here, it will be recomputed by caller.
        CFGOPTCTX_need_update_dominfo(&ctx) = false;
        cfg->insertBBBetween(pred, head, preheader, ctx);
        insert_preheader = true;
        inserted_by_cur_time = true;
    }
    //CASE1:
    //  BB_pred(goto BB_head)---
    //                          |
    //  BB_head <---------------
    //=>
    //  BB_pred(goto BB_preheader)---
    //                               |
    //  BB_preheader <---------------
    //   |  //fallthrough
    //   v
    //  BB_head(lab1)
    //Try to update the target-label of the last IR of predecessor, and
    //remove edge pred->header, add edge pred->preheader as well.
    IR * last_ir = BB_last_ir(pred);
    if (last_ir == nullptr || !last_ir->isBranch()) {
        if (!inserted_by_cur_time) {
            //Original pred is fallthrough to head.
            //Maintain pred and preheader's edge.
            CfgOptCtx ctx(*oc);
            //No need to maintain DomInfo here, it will be recomputed by caller.
            CFGOPTCTX_need_update_dominfo(&ctx) = false;
            cfg->removeEdge(pred, head, ctx);
            cfg->addEdge(pred, preheader, ctx);
        }
        return;
    }

    ASSERTN(last_ir->isConditionalBr() || last_ir->isUnconditionalBr(),
            ("TODO"));
    if (cfg->findBBbyLabel(last_ir->getLabel()) != head) { return; }

    //Update branch-target of last IR.
    if (*preheader_lab == nullptr) {
        //Preheader needs a label to be branch-target.
        *preheader_lab = rg->genILabel();
    }

    //Add the new label to preheader if not exist.
    if (!preheader->hasLabel(*preheader_lab)) {
        cfg->addLabel(preheader, *preheader_lab);
    }

    //Update branch-target of last IR of predecessor.
    last_ir->setLabel(*preheader_lab);

    if (inserted_by_cur_time) {
        //The edge should already be maintained.
        ASSERT0(cfg->getEdge(pred->id(), head->id()) == nullptr);
        ASSERT0(cfg->getEdge(pred->id(), preheader->id()));
        return;
    }
    //Maintain pred and preheader's edge.
    CfgOptCtx ctx(*oc);
    //No need to maintain DomInfo here, it will be recomputed by caller.
    CFGOPTCTX_need_update_dominfo(&ctx) = false;
    cfg->removeEdge(pred, head, ctx);
    cfg->addEdge(pred, preheader, ctx);
}


//The function will do inserton of preheader and update edges between
//heads and preheader.
//Return true if inserted a new preheader, otherwise there is no loop-outside
//BB.
static bool insertAndUpdateEdge(
    LI<IRBB> const* li, Region * rg, BBListIter head_it, IRBB * preheader,
    OptCtx * oc)
{
    IRCFG * cfg = rg->getCFG();
    List<IRBB*> preds;
    IRBB const* head = li->getLoopHead();
    cfg->get_preds(preds, head);
    LabelInfo const* preheader_lab = nullptr;
    bool insert_preheader = false;
    for (IRBB * p = preds.get_head(); p != nullptr; p = preds.get_next()) {
        if (li->isInsideLoop(p->id())) {
            ASSERTN(preheader->getVex(),
                    ("vex should have been added before current function"));
            fixupInnerLoopEdgeBetweenHeadAndPreheader(li, rg, p);
            continue;
        }
        insertAndUpdateOutterLoopEdge(li, rg, p, head_it, preheader,
                                      &preheader_lab, insert_preheader, oc);
    }
    //Update DOM info at one time.
    bool add_pdom_failed = false;
    cfg->addDomInfoToNewIDom(head->getVex(), preheader->getVex(),
                             add_pdom_failed);
    if (add_pdom_failed) {
        oc->setInvalidPDom();
    }
    return insert_preheader;
}


//Move LabelInfos from head to preheader except LabelInfos that
//are the target of IR that belongs to loop body.
static void tryMoveLabelFromHeadToPreheader(
    LI<IRBB> const* li, IRCFG * cfg, IRBB * preheader)
{
    IRBB * head = li->getLoopHead();
    LabelInfoList & lablst = head->getLabelList();
    if (lablst.get_elem_count() <= 1) {
        //The only label is the target of loop back-edge.
        return;
    }

    //Record if labels which attached on head BB are branch target of
    //IR which inside loop. The rest of labels can be moved to preheader BB.
    TMap<LabelInfo const*, bool> lab_canbe_move_to_preheader;
    for (LabelInfo const* lab = lablst.get_head();
         lab != nullptr; lab = lablst.get_next()) {
        lab_canbe_move_to_preheader.set(lab, false);
    }

    //Mark labels that can not be moved to preheader BB.
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = cfg->getBB(i);
        ASSERT0(bb);
        BBIRListIter it;
        for (IR const* ir = bb->getIRList().get_head(&it);
             ir != nullptr; ir = bb->getIRList().get_next(&it)) {
            if (ir->is_switch()) {
                for (IR * c = SWITCH_case_list(ir);
                     c != nullptr; c = c->get_next()) {
                    LabelInfo const* lab = c->getLabel();
                    ASSERT0(lab);
                    if (lab_canbe_move_to_preheader.find(lab)) {
                        lab_canbe_move_to_preheader.setAlways(lab, true);
                    }
                }
                continue;
            }

            LabelInfo const* lab = ir->getLabel();
            if (lab == nullptr) { continue; }

            if (!lab_canbe_move_to_preheader.find(lab)) { continue; }

            lab_canbe_move_to_preheader.setAlways(lab, true);
        }
    }

    //Move labels to preheader BB.
    LabelInfoListIter ct;
    LabelInfoListIter next_ct;
    for (lablst.get_head(&ct); ct != lablst.end(); ct = next_ct) {
        next_ct = lablst.get_next(ct);
        LabelInfo const* lab = ct->val();
        if (lab_canbe_move_to_preheader.get(lab)) { continue; }

        lablst.remove(ct);
        cfg->addLabel(preheader, lab);
        cfg->getLabel2BBMap()->setAlways(lab, preheader);
    }
}


//Find appropriate BB to be preheader.
//Return the appropriate BB if found it.
//prev: the previous BB of 'head' in BB list.
static IRBB * findAppropriatePreheader(LI<IRBB> const* li, IRCFG * cfg,
                                       IRBB * prev)
{
    IRBB * appropriate_bb = nullptr;
    if (prev == nullptr) { return nullptr; }
    IRBB const* head = li->getLoopHead();
    UINT outsidebbnum = 0;
    for (xcom::EdgeC const* ec = head->getVex()->getInList();
         ec != nullptr; ec = ec->get_next()) {
        UINT pred = ec->getFromId();
        if (li->isInsideLoop(pred)) { continue; }
        outsidebbnum++;
        if (pred == prev->id()) {
            //Try to find fallthrough prev BB.
            //CASE:BB_prev is suitable for preheader of head.
            //      BB_prev
            //        | //fallthrough
            //        v
            // ...-->BB_head
            IR const* last = const_cast<IRBB*>(prev)->getLastIR();
            if (last == nullptr ||
                (last->isUnconditionalBr() && head->isTarget(last))) {
                //prev fallthrough to head BB.
                if (appropriate_bb == nullptr && outsidebbnum <= 1) {
                    appropriate_bb = prev;
                } else {
                    //There are multiple outside-loop predecessor BBs, thus
                    //no appropriate preheader BB.
                    ASSERT0(outsidebbnum >= 2);
                    return nullptr;
                }
                continue;
            }
            if (!IRBB::isLowerBoundary(const_cast<IRBB*>(prev)->getLastIR())) {
                //prev should fallthrough to head BB.
                //Otherwise can not append IR to prev BB.
                if (appropriate_bb == nullptr && outsidebbnum <= 1) {
                    appropriate_bb = prev;
                } else {
                    //There are multiple outside-loop predecessor BBs, thus
                    //no appropriate preheader BB.
                    ASSERT0(outsidebbnum >= 2);
                    return nullptr;
                }
                continue;
            }
            if (outsidebbnum >= 2) {
                //There are multiple outside-loop predecessor BBs, thus
                //no appropriate preheader BB.
                return nullptr;
            }
            continue;
        }
        if (pred != prev->id()) {
            ASSERT0(cfg->getBB(pred));
            IR const* last_ir_of_pred = cfg->getBB(pred)->getLastIR();
            ASSERTN(last_ir_of_pred, ("should be removed by removeEmptyBB"));
            if (last_ir_of_pred->isUnconditionalBr()) {
                //CASE:pred is not fallthrough to head,
                //     but it is an unconditional branch.
                //      BB_pred---  //Jump to head.
                //                |
                //  ............  |
                //                |
                //   -->BB_prev   |
                //  |      |      |
                //  |      v      |
                //  |   BB_head<--
                //  |      |
                //  |      v
                //   ---BB_end
                ASSERT0(head->isTarget(last_ir_of_pred));
                if (appropriate_bb == nullptr && outsidebbnum <= 1) {
                    appropriate_bb = cfg->getBB(pred);
                } else {
                    //There are multiple outside-loop predecessor BBs, thus
                    //no appropriate preheader BB.
                    ASSERT0(outsidebbnum >= 2);
                    return nullptr;
                }
                continue;
            }
            if (outsidebbnum >= 2) {
                //There are multiple outside-loop predecessor BBs, thus
                //no appropriate preheader BB.
                return nullptr;
            }
            continue;
        }
    }
    return appropriate_bb;
}


IRBB * findAndInsertPreheader(LI<IRBB> const* li, Region * rg,
                              OUT bool & insert_bb, bool force, OptCtx * oc)
{
    ASSERT0(li && rg);
    insert_bb = false;
    IRCFG * cfg = rg->getCFG();
    BBList * bblst = rg->getBBList();
    IRBB * head = li->getLoopHead();
    BBListIter head_it = nullptr;
    bblst->find(head, &head_it);
    ASSERT0(head_it);
    BBListIter tt = head_it;
    IRBB * prev = bblst->get_prev(&tt);
    IRBB * appropriate_prev_bb = findAppropriatePreheader(li, cfg, prev);
    if (!force) {
        if (appropriate_prev_bb != nullptr) {
            ASSERT0(appropriate_prev_bb->rpo() != RPO_UNDEF);
            return appropriate_prev_bb;
        }
        return nullptr;
    }

    IRBB * preheader = rg->allocBB();
    cfg->addBB(preheader);

    //Guarantee preheader is fallthrough to head.
    bblst->insert_before(preheader, head_it);
    insert_bb |= insertAndUpdateEdge(li, rg, head_it, preheader, oc);
    ASSERT0(cfg->getBBList()->find(preheader));
    tryMoveLabelFromHeadToPreheader(li, cfg, preheader);
    return preheader;
}


static bool isLoopInvariantInPRSSA(IR const* ir, LI<IRBB> const* li,
                                   InvStmtList const* invariant_stmt)
{
    ASSERT0(ir->is_pr());
    SSAInfo * ssainfo = PR_ssainfo(ir);
    ASSERT0(ssainfo);
    IR const* def = ssainfo->getDef();
    if (def == nullptr) { return true; }

    //Note IR_PHI should have been analyzed and inserted into invariant_stmt
    //if it's operand is invariant.
    IRBB * defbb = def->getBB();
    ASSERT0(defbb);
    if (!li->isInsideLoop(defbb->id()) ||
        (invariant_stmt != nullptr &&
         invariant_stmt->find(const_cast<IR*>(def)))) {
        return true;
    }
    return false;
}


static bool isRealMDDef(MDDef const* mddef, IR const* use,
                        MDSSAMgr const* mdssamgr)
{
    IR const* defocc = mddef->getOcc();
    ASSERT0(defocc);
    bool const is_aggressive = true;
    return xoc::isDependent(defocc, use, is_aggressive, mdssamgr->getRegion());
}


static bool isRealMDDefInvariant(
    MDDef const* mddef, LI<IRBB> const* li, InvStmtList const* invariant_stmt,
    MDSSAMgr const* mdssamgr)
{
    ASSERT0(mddef && !mddef->is_phi());
    IR const* def = mddef->getOcc();
    ASSERT0(def);
    IRBB const* defbb = def->getBB();
    ASSERT0(defbb);
    if (!li->isInsideLoop(defbb->id())) { return true; }
    if (invariant_stmt == nullptr ||
        (invariant_stmt != nullptr &&
         !invariant_stmt->find(const_cast<IR*>(def)))) {
        return false;
    }
    return true;
}


//Return true if 'use' is loop invariant.
//Note if use's DEF is a PHI, we will not simply just check whether
//the DEF's BB is inside the loop, because PHI is not real DEF.
static bool isMDPhiInvariant(
    MDDef const* start, IR const* use, LI<IRBB> const* li,
    InvStmtList const* invariant_stmt, MDSSAMgr const* mdssamgr)
{
    ASSERT0(start && start->is_phi() && mdssamgr);
    ConstMDDefIter ii;
    for (MDDef const* def = mdssamgr->iterDefInitCTillKillingDef(
            start, use, ii);
         def != nullptr; def = mdssamgr->iterDefNextCTillKillingDef(use, ii)) {
        if (def->is_phi() || def == start) {
            continue;
        }
        if (!isRealMDDef(def, use, mdssamgr)) {
            continue;
        }
        if (!isRealMDDefInvariant(def, li, invariant_stmt, mdssamgr)) {
            return false;
        }
    }
    return true;
}


static bool isLoopInvariantInMDSSA(
    IR const* ir, LI<IRBB> const* li, InvStmtList const* invariant_stmt,
    MDSSAMgr const* mdssamgr)
{
    ASSERT0(ir->isMemRefNonPR());
    MDSSAInfo * mdssainfo = UseDefMgr::getMDSSAInfo(ir);
    ASSERT0(mdssainfo);
    VOpndSetIter iter = nullptr;
    for (BSIdx i = mdssainfo->getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
        VMD const* vopnd = (VMD const*)mdssamgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (!vopnd->hasDef()) { continue; }

        MDDef const* mddef = vopnd->getDef();
        ASSERT0(mddef);
        if (mddef->is_phi()) {
            //If ir's DEF is a PHI, we will not simply just check whether
            //the DEF's BB is inside the loop, because PHI is not real DEF.
            if (!isMDPhiInvariant(mddef, ir, li, invariant_stmt, mdssamgr)) {
                return false;
            }
            //PHI just indicates the JOIN point of other definitions, we do
            //not regard PHI as real definition.
            continue;
        }
        if (!isRealMDDef(mddef, ir, mdssamgr)) {
            continue;
        }
        if (!isRealMDDefInvariant(mddef, li, invariant_stmt, mdssamgr)) {
            return false;
        }
    }
    return true;
}


static bool isLoopInvariantInDUMgr(
    IR const* ir, LI<IRBB> const* li, InvStmtList const* invariant_stmt,
    Region const* rg)
{
    DUSet const* duset = ir->readDUSet();
    if (duset == nullptr) { return true; }

    DUSetIter dui = nullptr;
    for (BSIdx i = duset->get_first(&dui);
         i != BS_UNDEF; i = duset->get_next(i, &dui)) {
        IR const* def = const_cast<Region*>(rg)->getIR(i);
        ASSERT0(def->is_stmt());
        IRBB const* defbb = def->getBB();
        bool const is_aggressive = true;
        if (!xoc::isDependent(def, ir, is_aggressive, rg)) {
            continue;
        }
        if (!li->isInsideLoop(defbb->id())) { continue; }
        if (invariant_stmt == nullptr ||
            (invariant_stmt != nullptr &&
             !invariant_stmt->find(const_cast<IR*>(def)))) {
            return false;
        }
    }
    return true;
}


bool isPhiLoopInvariant(IR const* phi, LI<IRBB> const* li, Region const* rg)
{
    ASSERT0(phi && phi->is_phi());
    SSAInfo const* prssainfo = phi->getSSAInfo();
    ASSERT0(prssainfo);
    ASSERT0(prssainfo->getDef() == phi);
    IRSet const& useset = prssainfo->getUses();
    SSAUseIter it = nullptr;
    for (BSIdx i = useset.get_first(&it);
         it != nullptr; i = useset.get_next(i, &it)) {
        IR * use = rg->getIR(i);
        ASSERT0(use->isReadPR());
        if (!li->isInsideLoop(use->getStmt()->getBB()->id())) {
            continue;
        }
        if (xcom::in_list(PHI_opnd_list(phi), use)) {
            continue;
        }
        //There is at least one USE occurrence in loop.
        return false;
    }
    return true;
}


bool isLoopInvariant(IR const* ir, LI<IRBB> const* li, Region const* rg,
                     InvStmtList const* invariant_stmt, bool check_tree)
{
    ASSERT0(ir && ir->is_exp());
    if (ir->isReadPR() && !ir->isReadOnly()) {
        PRSSAMgr * prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            if (!isLoopInvariantInPRSSA(ir, li, invariant_stmt)) {
                return false;
            }
        } else if (!isLoopInvariantInDUMgr(ir, li, invariant_stmt, rg)) {
            return false;
        }
    } else if (ir->isMemRefNonPR() && !ir->isReadOnly()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            if (!isLoopInvariantInMDSSA(ir, li, invariant_stmt, mdssamgr)) {
                return false;
            }
        } else if (!isLoopInvariantInDUMgr(ir, li, invariant_stmt, rg)) {
            return false;
        }
    }
    if (!check_tree) { return true; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        if (!isLoopInvariant(kid, li, rg, invariant_stmt, check_tree)) {
            return false;
        }
    }
    return true;
}


//The functin will insert PHI after inserting preheader.
static bool forceInsertPreheader(LI<IRBB> const* li, Region * rg,
                                 OUT IRBB ** preheader, OptCtx * oc)
{
    IRCFG * cfg = rg->getCFG();
    InsertPhiHelper helper(li, cfg, *oc);
    bool need_phi = helper.preparePhiForPreheader();
    bool inserted = false;
    *preheader = findAndInsertPreheader(li, rg, inserted, true, oc);
    ASSERT0(inserted);
    if (need_phi) {
        helper.insertPhiAtPreheader(*preheader);
    }
    //Do not mark PASS changed if just inserted some BBs rather than
    //code motion, because post CFG optimization will removed the
    //redundant BB.
    if (g_dump_opt.isDumpCFGOpt()) {
        helper.dump();
    }
    return need_phi;
}


//Try inserting preheader BB of loop 'li'.
//The function will try to maintain the RPO, DOM, then
//updating PHI at loophead and preheader, after inserting preheader.
//preheader: record the preheader either inserted BB or existed BB.
//force: force to insert preheader BB whatever it has been exist.
//       If 'force' is false, the function only inserts new preheader if it
//       cand not find an appropriate BB to be preheader.
//       Return the new BB if insertion is successful.
//Return true if inserting a new BB before loop, otherwise false.
//CASE: if we find a preheader, the last IR in it may be CallStmt.
//So if you are going to insert IR at the tail of preheader, the best choose
//is force the function to insert a new phreader.
bool insertPreheader(LI<IRBB> const* li, Region * rg, OUT IRBB ** preheader,
                     MOD OptCtx * oc, bool force)
{
    bool need_phi = false;
    DUMMYUSE(need_phi);
    bool inserted = false;
    IRCFG * cfg = rg->getCFG();
    if (force) {
        need_phi = forceInsertPreheader(li, rg, preheader, oc);
        inserted = true;
    } else {
        IRBB * p = findAndInsertPreheader(li, rg, inserted, false, oc);
        if (p == nullptr || cfg->isRegionEntry(p) ||
            (p->getLastIR() != nullptr &&
             p->getLastIR()->isCallStmt() &&
             CALL_is_intrinsic(p->getLastIR()))) {
            need_phi = forceInsertPreheader(li, rg, &p, oc);
            inserted = true;
        }
        ASSERT0(p);
        ASSERT0(preheader);
        *preheader = p;
    }

    if (!inserted) { return false; }
    ASSERT0(*preheader);
    if ((*preheader)->rpo() == RPO_UNDEF &&
        !cfg->tryUpdateRPO(*preheader, li->getLoopHead(), true)) {
        oc->setInvalidRPO();
    }
    //Sometime RPOMgr can not find usable RPO, e.g:guard.gr
    //ASSERT0((*preheader)->rpo() != RPO_UNDEF);

    //Update outer LoopInfo, add preheader to outer loop body.
    li->addBBToAllOuterLoop((*preheader)->id());
    ASSERT0(li->getLoopHead());
    if (oc->is_dom_valid() || oc->is_pdom_valid()) {
        ASSERTN(cfg->get_idom(*preheader) != VERTEX_UNDEF,
                ("should be maintained"));
    }
    OptCtx::setInvalidIfCFGChangedExcept(oc, PASS_DOM, PASS_PDOM, PASS_RPO,
                                         PASS_LOOP_INFO, PASS_UNDEF);
    return true;
}


//Iterative access LoopInfo tree. This funtion initialize the iterator.
//li: the root of the LoopInfo tree.
//it: iterator. It should be clean already.
template <class LoopInfoT, class LoopInfoIterT>
LoopInfoT iterInitLoopInfoImpl(LoopInfoT li, OUT LoopInfoIterT & it)
{
    if (li == nullptr) { return nullptr; }
    for (LoopInfoT x = li->getInnerList();
         x != nullptr; x = x->get_next()) {
        it.append_tail(x);
    }
    if (li->get_next() != nullptr) {
        it.append_tail(li->get_next());
    }
    return li;
}


//Iterative access LoopInfo tree.
//This function return the next LoopInfo according to 'it'.
//it: iterator.
template <class LoopInfoT, class LoopInfoIterT>
LoopInfoT iterNextLoopInfoImpl(MOD LoopInfoIterT & it)
{
    LoopInfoT li = it.remove_head();
    if (li == nullptr) { return nullptr; }
    for (LoopInfoT x = li->getInnerList();
         x != nullptr; x = x->get_next()) {
        it.append_tail(x);
    }
    if (li->get_next() != nullptr) {
        it.append_tail(li->get_next());
    }
    return li;
}


//Iterative access LoopInfo tree. This funtion initialize the iterator.
//li: the root of the LoopInfo tree.
//it: iterator. It should be clean already.
LI<IRBB> const* iterInitLoopInfoC(LI<IRBB> const* li, OUT CLoopInfoIter & it)
{
    return iterInitLoopInfoImpl<LI<IRBB> const*, CLoopInfoIter>(li, it);
}


//Iterative access LoopInfo tree.
//This function return the next LoopInfo according to 'it'.
//it: iterator.
LI<IRBB> const* iterNextLoopInfoC(OUT CLoopInfoIter & it)
{
    return iterNextLoopInfoImpl<LI<IRBB> const*, CLoopInfoIter>(it);
}


//Iterative access LoopInfo tree. This funtion initialize the iterator.
//li: the root of the LoopInfo tree.
//it: iterator. It should be clean already.
LI<IRBB> * iterInitLoopInfo(LI<IRBB> * li, OUT LoopInfoIter & it)
{
    return iterInitLoopInfoImpl<LI<IRBB>*, LoopInfoIter>(li, it);
}


//Iterative access LoopInfo tree.
//This function return the next LoopInfo according to 'it'.
//it: iterator.
LI<IRBB> * iterNextLoopInfo(OUT LoopInfoIter & it)
{
    return iterNextLoopInfoImpl<LI<IRBB>*, LoopInfoIter>(it);
}


//Find the BB that is the START of the unqiue backedge of loop.
//  BB1: loop-start bb
//  BB2: body
//  BB3: goto loop-start bb
//BB3 is the backedge-start bb.
//Return backedge BB id if found, otherwise return BBID_UNDEF.
IRBB * findBackEdgeStartBB(LI<IRBB> const* li, IRCFG const* cfg)
{
    UINT bbid = li->findBackEdgeStartBB(cfg);
    if (bbid != BBID_UNDEF) {
        return cfg->getBB(bbid);
    }
    return nullptr;
}


//Find the first BB that is the END of loop. The end BB is outside of loop.
//Note there could be multiple end BB if the last IR of head is
//multipl-conditional branch, namely, SWTICH or IGOTO.
//  BB1: loop start bb
//       falsebr END
//  BB2: body
//  BB3: goto loop start bb
//  BB4: END
//BB4 is the loopend bb.
//
IRBB * findFirstLoopEndBB(LI<IRBB> const* li, IRCFG * cfg)
{
    UINT bbid = li->findFirstLoopEndBB(cfg);
    if (bbid != BBID_UNDEF) {
        return cfg->getBB(bbid);
    }
    return nullptr;
}


static bool isCallDomAllUseInsideLoop(IR const* stmt, LI<IRBB> const* li,
                                      Region * rg, OptCtx const& oc)
{
    ASSERT0(stmt->isCallStmt());
    bool retval_dom_all_use = false;
    bool maydef_dom_all_use = false;
    bool retval_checked = false;
    bool maydef_checked = false;
    if (stmt->isCallHasRetVal()) {
        PRSSAMgr * prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            retval_dom_all_use |= prssamgr->isStmtDomAllUseInsideLoop(
                stmt, li, oc);
            retval_checked = true;
        }
    } else {
        retval_checked = true;
    }

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        maydef_dom_all_use |= mdssamgr->isStmtDomAllUseInsideLoop(stmt, li);
        maydef_checked = true;
    }

    if (!retval_checked || !maydef_checked) {
        DUSet const* useset = stmt->readDUSet();
        if (useset != nullptr) {
            DUMgr * dumgr = rg->getDUMgr();
            ASSERT0(dumgr);
            maydef_dom_all_use |= dumgr->isStmtDomAllUseInsideLoop(stmt, li);
        }
    }
    return retval_dom_all_use && maydef_dom_all_use;
}


//Return true if stmt dominates all USE that are inside loop.
bool isStmtDomAllUseInsideLoop(IR const* stmt, LI<IRBB> const* li, Region * rg,
                               OptCtx const& oc)
{
    ASSERT0(stmt->is_stmt());
    if (stmt->isCallHasRetVal()) {
        return isCallDomAllUseInsideLoop(stmt, li, rg, oc);
    }

    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid() && stmt->isWritePR()) {
        return prssamgr->isStmtDomAllUseInsideLoop(stmt, li, oc);
    }

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        stmt->isMemRefNonPR()) {
        return mdssamgr->isStmtDomAllUseInsideLoop(stmt, li);
    }

    DUSet const* useset = stmt->readDUSet();
    if (useset != nullptr) {
        DUMgr * dumgr = rg->getDUMgr();
        ASSERT0(dumgr);
        return dumgr->isStmtDomAllUseInsideLoop(stmt, li);
    }

    //Can not determine.
    return false;
}


static bool verifyLoopInfo(LI<IRBB> const* li, OptCtx const& oc)
{
    if (li == nullptr) { return true; }
    ASSERT0(li->getLoopHead());
    ASSERT0(li->getLoopHead()->id() <=
            oc.getRegion()->getBBMgr()->getBBCount());
    bool try_failed = false;
    ASSERTN_DUMMYUSE(Graph::isReachIn(
        li->getLoopHead()->getVex(), li->getLoopHead()->getVex(),
        (UINT)-1, try_failed), ("loophead is not in cycle"));
    IRCFG const* cfg = oc.getRegion()->getCFG();
    BitSet const* body = li->getBodyBBSet();
    for (BSIdx i = body->get_first(); i != BS_UNDEF; i = body->get_next(i)) {
        ASSERTN_DUMMYUSE(cfg->isVertex(i),
                         ("BB in bodyset is not vertex of CFG"));
    }
    for (LI<IRBB> const* inner = li->getInnerList(); inner != nullptr;
         inner = inner->get_next()) {
        verifyLoopInfoTree(inner, oc);
        ASSERT0(body->is_contain(inner->getLoopHead()->id()));
        ASSERT0(body->is_contain(*inner->getBodyBBSet()));
    }
    return true;
}


bool verifyLoopInfoTree(LI<IRBB> const* li, OptCtx const& oc)
{
    if (li == nullptr || !oc.is_loopinfo_valid()) { return true; }
    for (LI<IRBB> const* tli = li; tli != nullptr; tli = tli->get_next()) {
        verifyLoopInfoTree(tli->getInnerList(), oc);
        verifyLoopInfo(tli, oc);
    }
    return true;
}


bool isBranchTargetOutSideLoop(LI<IRBB> const* li, IRCFG * cfg, IR const* stmt)
{
    LabelInfo const* lab = stmt->getLabel();
    ASSERTN(lab, ("stmt does not have target label"));
    IRBB const* tbb = cfg->findBBbyLabel(lab);
    ASSERT0(tbb);
    return !li->isInsideLoop(tbb->id());
}


//
//START FindRedOp
//
class FindRedOp {
    friend class IterIRTreeForMemRef;
    COPY_CONSTRUCTOR(FindRedOp);
protected:
    MDSSAMgr const* m_mdssamgr;
    PRSSAMgr const* m_prssamgr;
    UseDefMgr const* m_udmgr;
    MD const* m_ref;
    LI<IRBB> const* m_li;
    IR const* m_op;
    Region const* m_rg;
    IRBB const* m_loophead;
    FindRedOpResult m_res;
    xcom::TTab<UINT> m_mddeftab;
    xcom::TTab<UINT> m_irtab;
protected:
    void checkOpRHS();
    void checkCall();
    void checkAssign();

    void findInPRSSA();
    void findInMDSSA();
    void find();

    bool isVisited(MDDef const* mddef) const
    { return m_mddeftab.find(mddef->id()); }
    bool isVisited(IR const* ir) const
    { return m_irtab.find(ir->id()); }

    void setVisited(MDDef const* mddef) { m_mddeftab.append(mddef->id()); }
    void setVisited(IR const* ir) { m_irtab.append(ir->id()); }

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    FindRedOp(LI<IRBB> const* li, IR const* op, Region const* rg) :
        m_li(li), m_op(op), m_rg(rg)
    {
        ASSERT0(op && op->is_stmt());
        m_prssamgr = m_rg->getPRSSAMgr();
        m_mdssamgr = m_rg->getMDSSAMgr();
        if (m_mdssamgr != nullptr) {
            m_udmgr = const_cast<MDSSAMgr*>(m_mdssamgr)->getUseDefMgr();
        }
        m_loophead = li->getLoopHead();
        m_res = OP_UNDEF;
        if (!useMDSSADU() || !usePRSSADU()) {
            m_res = OP_UNKNOWN;
            return;
        }
        find();
    }
    void checkExp(IR const* exp);
    void checkExpInPRSSA(IR const* exp);
    void checkExpInMDSSA(IR const* exp);
    void checkDefInPRSSA(IR const* def);
    void checkMDDefInMDSSA(MDDef const* mddef);

    FindRedOpResult getResult() const { return m_res; }

    void iterIRTreeList(IR const* exp_list);
};


void FindRedOp::find()
{
    ASSERT0(m_op && m_op->is_stmt() && m_op->isMemRef());
    if (m_op->isCallStmt()) {
        checkCall();
        return;
    }
    checkAssign();
}


void FindRedOp::checkExp(IR const* exp)
{
    if (exp->isPROp()) {
        checkExpInPRSSA(exp);
        return;
    }
    if (exp->isMemRefNonPR()) {
        checkExpInMDSSA(exp);
        return;
    }
    UNREACHABLE();
}


void FindRedOp::checkDefInPRSSA(IR const* def)
{
    ASSERT0(def && def->is_stmt());
    if (def->is_phi()) {
        if (def->getBB() != m_loophead) {
            //If exp's DEF is a PHI, for now, we just check whether the phi
            //is located in loophead BB. If it is not the case, we can not
            //determine if 'exp' is in the cycle of reduction op.
            m_res = OP_UNKNOWN;
            return;
        }
        if (isVisited(def)) {
            //Found a cycle in DefUse chain that formed by
            //phi1 -> $3 id:11 -> $3 id:37 -> phi2 -> $1 id:9 -> phi1
            //  $1 phi1 = $0, $3 id:11
            //  ...
            //  $3 phi2 = $1 id:9, $6
            //  ...
            //  st x = $3 id:37
            ASSERT0(m_res == OP_UNDEF);
            m_res = OP_HAS_CYCLE;
            return;
        }
        setVisited(def);
        iterIRTreeList(((CPhi const*)def)->getOpndList());
        return;
    }
    if (def == m_op) {
        //CASE:rp0_2.c
        //     BB_LOOPHEAD:<-----------
        //     phi $17 = $18, $19; #S1 |
        //  ---falsebr                 |
        // |     |                     |
        // |     V                     |
        // |   BODY:                   |
        // |   stpr $18 = $17;         |
        // |   stpr $19 = $18;         |
        // |   goto BB_LOOPHEAD;-------
        // |
        //  -->BB_END:
        //     ist = ... $17; #S2
        //In the case, #S1 and #S2 construct a reduce operation pair,
        //which reduce-variable is $19.
        ASSERT0(m_res == OP_UNDEF);
        m_res = OP_HAS_CYCLE;
        return;
    }
}


void FindRedOp::checkExpInPRSSA(IR const* exp)
{
    ASSERT0(exp && exp->is_exp());
    ASSERT0(exp->isPROp());
    SSAInfo const* prssainfo = exp->getSSAInfo();
    ASSERT0(prssainfo);
    IR const* def = prssainfo->getDef();
    if (def == nullptr) {
        //Region live-in PR can not form reduction operation.
        ASSERT0(m_res == OP_UNDEF);
        m_res = OP_IS_NOT_RED;
        return;
    }
    if (!m_li->isInsideLoop(def->getBB()->id())) {
        //Outside loop stmt can not form reduction operation.
        ASSERT0(m_res == OP_UNDEF);
        m_res = OP_IS_NOT_RED;
        return;
    }
    checkDefInPRSSA(def);
}


void FindRedOp::checkMDDefInMDSSA(MDDef const* mddef)
{
    ASSERT0(mddef);
    if (mddef->is_phi()) {
        if (mddef->getBB() != m_loophead) {
            //If exp's DEF is a PHI, for now, we just check whether the phi
            //is located in loophead BB. If it is not the case, we can not
            //determine if 'exp' is in the cycle of reduction op.
            m_res = OP_UNKNOWN;
            return;
        }
        if (isVisited(mddef)) {
            //Found a cycle in DefUse chain that formed by
            //MDPhi5 -> LD id:39 -> ID id:74 -> MDPhi5.
            //CASE:compile/ir_refine.c,
            //    BB2:<------------------------------------------
            //  --MDPhi5: MD18V1 <- (MD18V0 BB8), (id:74 MD18V1) |
            // |   |                                             |
            // |   V                                             |
            // |  BB3:                                           |
            // |  st:f32 'f00' id:21                             |
            // |    ld:f32 'gf' id:39 -USE:MD18V1                |
            // |  goto BB2;--------------------------------------
            // |
            //  ->BB4:
            //    return
            ASSERT0(m_res == OP_UNDEF);
            m_res = OP_HAS_CYCLE;
            return;
        }
        setVisited(mddef);
        iterIRTreeList(((MDPhi const*)mddef)->getOpndList());
        return;
    }
    IR const* defocc = mddef->getOcc();
    ASSERT0(defocc);
    if (!m_li->isInsideLoop(defocc->getBB()->id())) {
        //No need to consider the outside DefUse relation. It must not
        //be able to form reduction operation. However, since MDSSA
        //represents May-Dependence, we have to keep analysing remain
        //VMD.
        return;
    }
    bool const is_aggressive = true;
    if (xoc::isDependent(defocc, m_op, is_aggressive, m_rg)) {
        //Since MDSSA represents May-Dependence, we just record that
        //the function has found a DefUse chain which form a dependence
        //cycle in the loop.
        ASSERT0(m_res == OP_UNDEF);
        m_res = OP_HAS_CYCLE;
        return;
    }
    if (defocc->hasRHS()) {
        //Note RHS may be NULL if defocc is Virtual OP.
        iterIRTreeList(defocc->getRHS());
        return;
    }
    ASSERTN(0, ("TODO"));
}


void FindRedOp::checkExpInMDSSA(IR const* exp)
{
    ASSERT0(exp && exp->is_exp());
    ASSERT0(exp->isMemRefNonPR());
    MDSSAInfo * mdssainfo = m_mdssamgr->getMDSSAInfoIfAny(exp);
    ASSERT0(mdssainfo);
    //WORKAROUND: ASSERT0(mdssainfo && !mdssainfo->isEmptyVOpndSet());
    if (mdssainfo->isEmptyVOpndSet()) {
        //WORKAROUND:
        m_res = OP_UNKNOWN;
        return;
    }
    VOpndSetIter it;
    for (BSIdx i = mdssainfo->readVOpndSet().get_first(&it);
         i != BS_UNDEF; i = mdssainfo->readVOpndSet().get_next(i, &it)) {
        VMD * vmd = (VMD*)m_udmgr->getVOpnd(i);
        ASSERT0(vmd && vmd->is_md());
        if (!vmd->hasDef()) { continue; }
        MDDef const* mddef = vmd->getDef();
        checkMDDefInMDSSA(mddef);
        if (m_res != OP_UNDEF) {
            //Result has been ready.
            return;
        }
    }
}


void FindRedOp::checkAssign()
{
    m_res = OP_UNDEF;

    //Get the MustDef MD.
    m_ref = m_op->getMustRef();
    if (m_ref == nullptr) {
        m_res = OP_UNKNOWN;
        return;
    }
    if (!m_ref->is_exact() && !m_ref->is_range()) {
        m_res = OP_UNKNOWN;
        return;
    }
    checkOpRHS();
}


void FindRedOp::checkCall()
{
    ASSERT0(m_op && m_op->is_stmt() && m_op->isMemRef());
    m_res = OP_UNDEF;

    //Get the MustDef MD.
    m_ref = m_op->getMustRef();
    if (m_ref == nullptr) {
        m_res = OP_UNKNOWN;
        return;
    }
    if (!m_ref->is_exact() && !m_ref->is_range()) {
        m_res = OP_UNKNOWN;
        return;
    }
    if (m_op->isReadOnly()) {
        //If Call is readonly, it is impossible for DefUse chain to
        //form a cycle.
        m_res = OP_IS_NOT_RED;
        return;
    }
    //For the sake of complexity of call-stmt, we can not easy determine
    //the reduction behaviors of a call, expect more aggressive analysis.
    m_res = OP_UNKNOWN;
}


void FindRedOp::checkOpRHS()
{
    ASSERT0(m_op && m_op->is_stmt());
    ASSERT0(m_op->hasRHS());
    IR const* rhs = m_op->getRHS();
    if (rhs == nullptr) {
        //Virtual OP may not have RHS.
        //If OP does not have RHS, it is impossible for DefUse chain to
        //form a cycle.
        m_res = OP_IS_NOT_RED;
        return;
    }
    iterIRTreeList(rhs);
}


class MemRefVisitFunc {
public:
    FindRedOp & m_findredop;
public:
    bool visitIR(IR const* ir, OUT bool & is_terminate)
    {
        ASSERT0(ir->is_exp());
        if (!ir->isMemRef()) {
            //Return true to keep visiting kid of 'ir'.
            return true;
        }
        m_findredop.checkExp(ir);
        if (m_findredop.getResult() == OP_UNDEF) {
            //Return true to keep processing the next kid.
            return true;
        }
        //The finding result has already been generated, terminate the
        //finding process immediately.
        is_terminate = true;
        return false;
    }
    MemRefVisitFunc(FindRedOp & findredop) : m_findredop(findredop) {}
};


class IterIRTreeForMemRef : public VisitIRTree<MemRefVisitFunc> {
public:
    IterIRTreeForMemRef(IR const* ir, MemRefVisitFunc & vf)
        : VisitIRTree<MemRefVisitFunc>(vf)
    { visitWithSibling(ir);}
};


void FindRedOp::iterIRTreeList(IR const* exp_list)
{
    ASSERT0(exp_list && exp_list->is_exp());
    MemRefVisitFunc vf(*this);
    IterIRTreeForMemRef iterirtree(exp_list, vf);
}
//END FindRedOp


FindRedOpResult findRedOpInLoop(LI<IRBB> const* li, IR const* stmt,
                                Region const* rg)
{
    ASSERT0(stmt && stmt->is_stmt());
    FindRedOp findredop(li, stmt, rg);
    return findredop.getResult();
}


//
//START BB2LI
//
void BB2LI::createMap(LI<IRBB> * li)
{
    if (li == nullptr) { return; }
    for (LI<IRBB> * tli = li; tli != nullptr; tli = tli->get_next()) {
        createMap(tli->getInnerList());
        IRBB const* h = tli->getLoopHead();
        ASSERT0(h);
        m_bb2li.set(h->id(), tli);
    }
}
//END BB2LI

} //namespace xoc
