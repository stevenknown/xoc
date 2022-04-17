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
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    MDSystem * m_mdsys;
    Vector<UINT> m_pred_order;
    Vector<UINT> m_pred_pos;
    List<IR*> m_prssa_phis;
    List<MDPhi*> m_mdssa_phis;
private:
    void dumpOrder() const;
    void dumpPhi() const;

    Region * getRegion() const { return m_rg; }

    void insertPRSSAPhi(IRBB * preheader);
    void insertMDSSAPhi(IRBB * preheader);

    void makePRSSAPhiForPreheader(IRBB const* head);
    void makeMDSSAPhiForPreheader(IRBB * head);
    void makePRSSAPhi(IR const* phi);
    void makeMDSSAPhi(MDPhi const* phi);

    void replacePRSSASuccOpnd(IRBB * preheader, UINT prehead_pos);
    void replaceMDSSASuccOpnd(IRBB * preheader, UINT prehead_pos);
public:
    InsertPhiHelper(LI<IRBB> const* li, IRCFG * cfg)
        : m_li(li), m_cfg(cfg)
    {
        m_rg = cfg->getRegion();
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
    C<IR*> * it;
    for (IR * ir = m_prssa_phis.get_head(&it); ir != nullptr;
         ir = m_prssa_phis.get_next(&it)) {
        dumpIR(ir, m_rg, nullptr, IR_DUMP_KID);
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
    for (xcom::EdgeC const* ec = m_cfg->getVertex(head->id())->getInList();
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
    IR * newphi = m_rg->buildPhi(m_rg->buildPrno(phi->getType()),
                                 phi->getType(), (IR*)nullptr);
    m_rg->allocRefForPR(newphi);
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
    m_prssamgr->genSSAInfoForStmt(newphi);
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
    C<IR*> * it;
    BBIRList & irlst = preheader->getIRList();
    for (IR * phi = m_prssa_phis.get_tail(&it);
         phi != nullptr; phi = m_prssa_phis.get_prev(&it)) {
        irlst.append_head(phi);
    }
}


void InsertPhiHelper::insertMDSSAPhi(IRBB * preheader)
{
    C<MDPhi*> * it;
    MDPhiList * philst = m_mdssamgr->genPhiList(preheader);
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
        MDDEF_bb(prephi) = preheader; //Complete the MDPhi info.
        IR * oldopnd = lhphi->val()->getOpnd(prehead_pos);
        ASSERT0(oldopnd && oldopnd->is_id() &&
                MDSSAMgr::getMDSSAInfoIfAny(oldopnd));
        m_mdssamgr->removeMDSSAOccForTree(oldopnd);
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

        IR * newopnd = m_rg->buildPRdedicated(ir_pre->getPrno(),
                                              ir_pre->getType());
        m_rg->allocRef(newopnd);
        m_prssamgr->buildDUChain(ir_pre, newopnd);

        ir_lh->replaceKid(oldopnd, newopnd, true);
        m_rg->freeIRTree(oldopnd);
    }
}


//The new PHI will be inserted to preheader.
void InsertPhiHelper::insertPhiAtPreheader(IRBB * preheader)
{
    IRBB * loophead = m_li->getLoopHead();
    UINT prehead_pos = ((DGraph*)m_cfg)->WhichPred(
        preheader->id(), m_cfg->getVertex(loophead->id()));
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
bool findTwoSuccessorBBOfLoopHeader(LI<IRBB> const* li, IRCFG * cfg,
                                    UINT * succ1, UINT * succ2)
{
    ASSERT0(li && cfg && succ1 && succ2);
    IRBB * head = li->getLoopHead();

    xcom::Vertex * headvex = cfg->getVertex(head->id());
    if (headvex->getOutDegree() != 2) {
        //Not natural loop.
        return false;
    }

    xcom::EdgeC const* ec = VERTEX_out_list(headvex);
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
        IR * gotoir = rg->buildGoto(lab);
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
static void fixupInnerLoopEdgeBetweenHeadAndPreheader(LI<IRBB> const* li,
                                                      Region * rg,
                                                      IRBB * pred)
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
static void insertAndUpdateOutterLoopEdge(LI<IRBB> const* li, Region * rg,
                                          IRBB * pred, BBListIter head_it,
                                          IRBB * preheader,
                                          MOD LabelInfo const** preheader_lab,
                                          OUT bool & insert_preheader)
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
        cfg->insertVertexBetween(pred->id(), head->id(), preheader->id());
        bool succ = cfg->tryFindLessRPO(preheader, head);
        CHECK0_DUMMYUSE(succ);
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
            cfg->removeEdge(pred, head);
            cfg->addEdge(pred, preheader);
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
    cfg->removeEdge(pred, head);
    cfg->addEdge(pred, preheader);
}


//The function will do inserton of preheader and update edges between
//heads and preheader.
//Return true if inserted a new preheader, otherwise there is no loop-outside
//BB.
static bool insertAndUpdateEdge(LI<IRBB> const* li, Region * rg,
                                BBListIter head_it, IRBB * preheader)
{
    IRCFG * cfg = rg->getCFG();
    List<IRBB*> preds;
    IRBB const* head = li->getLoopHead();
    cfg->get_preds(preds, head);
    LabelInfo const* preheader_lab = nullptr;
    bool insert_preheader = false;
    for (IRBB * p = preds.get_head(); p != nullptr; p = preds.get_next()) {
        if (li->isInsideLoop(p->id())) {
            ASSERTN(cfg->getVertex(preheader->id()),
                    ("vex should have been added before current function"));
            fixupInnerLoopEdgeBetweenHeadAndPreheader(li, rg, p);
            continue;
        }
        insertAndUpdateOutterLoopEdge(li, rg, p, head_it, preheader,
                                      &preheader_lab, insert_preheader);
    }
    return insert_preheader;
}


//Move LabelInfos from head to preheader except LabelInfos that
//are the target of IR that belongs to loop body.
static void tryMoveLabelFromHeadToPreheader(LI<IRBB> const* li, IRCFG * cfg,
                                            IRBB * preheader)
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
        for (IR const* ir = BB_first_ir(bb);
             ir != nullptr; ir = BB_next_ir(bb)) {
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
//head: loophead of loop.
//prev: the previous BB of 'head' in BB list.
static IRBB * findAppropriatePreheader(LI<IRBB> const* li, IRCFG * cfg,
                                       IRBB * prev)
{
    IRBB * appropriate_bb = nullptr;
    if (prev == nullptr) { return nullptr; }

    IRBB const* head = li->getLoopHead();
    for (xcom::EdgeC const* ec = cfg->getVertex(head->id())->getInList();
         ec != nullptr; ec = ec->get_next()) {
        UINT pred = ec->getFromId();
        if (li->isInsideLoop(pred)) { continue; }

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
                if (appropriate_bb == nullptr) {
                    appropriate_bb = prev;
                } else {
                    //There are multiple outside-loop predecessor BBs, thus
                    //no appropriate preheader BB.
                    return nullptr;
                }
                continue;
            }
            if (!IRBB::isLowerBoundary(const_cast<IRBB*>(prev)->getLastIR())) {
                //prev should fallthrough to head BB.
                //Otherwise can not append IR to prev BB.
                if (appropriate_bb == nullptr) {
                    appropriate_bb = prev;
                } else {
                    //There are multiple outside-loop predecessor BBs, thus
                    //no appropriate preheader BB.
                    return nullptr;
                }
                continue;
            }
            if (appropriate_bb != nullptr) {
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
                if (appropriate_bb == nullptr) {
                    appropriate_bb = cfg->getBB(pred);
                } else {
                    //There are multiple outside-loop predecessor BBs, thus
                    //no appropriate preheader BB.
                    return nullptr;
                }
                continue;
            }
            if (appropriate_bb != nullptr) {
                //There are multiple outside-loop predecessor BBs, thus
                //no appropriate preheader BB.
                return nullptr;
            }
            continue;
        }
    }
    return appropriate_bb;
}


//Find preheader BB. If it does not exist, insert one before loop 'li'.
//Return the preheader BB.
//insert_bb: return true if this function insert a new bb before loop,
//           otherwise return false.
//force: force to insert preheader BB whatever it has been exist.
//       Return the new BB if insertion is successful.
//Note if we find the preheader, the last IR of it may be call.
//So if you are going to insert IR at the tail of preheader, the best choose is
//force the function to insert a new bb.
//The function will try to maintain the RPO.
IRBB * findAndInsertPreheader(LI<IRBB> const* li, Region * rg,
                              OUT bool & insert_bb, bool force)
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
    //Guarrantee preheader is fallthrough to head.
    bblst->insert_before(preheader, head_it);
    insert_bb |= insertAndUpdateEdge(li, rg, head_it, preheader);
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


static bool isRealMDDefInvariant(MDDef const* mddef, LI<IRBB> const* li,
                                 InvStmtList const* invariant_stmt,
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
static bool isMDPhiInvariant(MDDef const* start,
                             IR const* use,
                             LI<IRBB> const* li,
                             InvStmtList const* invariant_stmt,
                             MDSSAMgr const* mdssamgr)
{
    ASSERT0(start && start->is_phi() && mdssamgr);
    ConstMDDefIter ii;
    for (MDDef const* def =
            mdssamgr->iterDefInitCTillKillingDef(start, use, ii);
         def != nullptr; def = mdssamgr->iterDefNextCTillKillingDef(use, ii)) {
        if (def->is_phi() || def == start) {
            continue;
        }
        if (!isRealMDDefInvariant(def, li, invariant_stmt, mdssamgr)) {
            return false;
        }
    }
    return true;
}


static bool isLoopInvariantInMDSSA(IR const* ir, LI<IRBB> const* li,
                                   InvStmtList const* invariant_stmt,
                                   MDSSAMgr const* mdssamgr)
{
    ASSERT0(ir->isMemoryRefNonPR());
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
            if (!isMDPhiInvariant(mddef, ir, li, invariant_stmt, mdssamgr)) {
                return false;
            }
            //PHI just indicates the JOIN point of other definitions, we do
            //not regard PHI as real definition.
            continue;
        }
        if (!isRealMDDefInvariant(mddef, li, invariant_stmt, mdssamgr)) {
            return false;
        }
    }
    return true;
}


static bool isLoopInvariantInDUMgr(IR const* ir,
                                   LI<IRBB> const* li,
                                   InvStmtList const* invariant_stmt,
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

        if (!li->isInsideLoop(defbb->id())) { continue; }
        if (invariant_stmt == nullptr ||
            (invariant_stmt != nullptr &&
             !invariant_stmt->find(const_cast<IR*>(def)))) {
            return false;
        }
    }
    return true;
}


//Return true if all the expression on 'ir' tree is loop invariant.
//ir: root node of IR
//li: loop structure
//check_tree: true to perform check recusively for entire IR tree.
//invariant_stmt: a list that records the stmt that is invariant in loop.
//    e.g:loop() {
//          a = b; //S1
//       }
//    stmt S1 is invariant because b is invariant.
//Note the function does not check the sibling node of 'ir'.
bool isLoopInvariant(IR const* ir, LI<IRBB> const* li, Region * rg,
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
    } else if (ir->isMemoryRefNonPR() && !ir->isReadOnly()) {
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
                                 OUT IRBB ** preheader)
{
    IRCFG * cfg = rg->getCFG();
    InsertPhiHelper helper(li, cfg);
    bool need_phi = helper.preparePhiForPreheader();
    bool inserted = false;
    *preheader = findAndInsertPreheader(li, rg, inserted, true);
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
//       Return the new BB if insertion is successful.
//Return true if inserting a new BB before loop, otherwise false.
//CASE: if we find a preheader, the last IR in it may be CallStmt.
//So if you are going to insert IR at the tail of preheader, the best choose
//is force the function to insert a new phreader.
bool insertPreheader(LI<IRBB> const* li, Region * rg, OUT IRBB ** preheader,
                     MOD OptCtx & oc, bool force)
{
    bool need_phi = false;
    bool inserted = false;
    IRCFG * cfg = rg->getCFG();
    if (force) {
        need_phi = forceInsertPreheader(li, rg, preheader);
        inserted = true;
    } else {
        IRBB * p = findAndInsertPreheader(li, rg, inserted, false);
        if (p == nullptr || cfg->isRegionEntry(p) ||
            (p->getLastIR() != nullptr &&
             p->getLastIR()->isCallStmt() &&
             CALL_is_intrinsic(p->getLastIR()))) {
            need_phi = forceInsertPreheader(li, rg, &p);
            inserted = true;
        }
        ASSERT0(p);
        ASSERT0(preheader);
        *preheader = p;
    }

    if (!inserted) { return false; }
    if (!li->isOuterMost()) {
        //Update outer LoopInfo, add preheader to outer loop body.
        li->addBBToAllOuterLoop((*preheader)->id());
    }
    ASSERT0(li->getLoopHead());
    if (oc.is_dom_valid() || oc.is_pdom_valid()) {
        //Update DOM info.
        cfg->addDomInfoByNewIDom(cfg->getVertex(li->getLoopHead()->id()),
                                 cfg->getVertex((*preheader)->id()));
    }
    //Try update RPO.
    if (!oc.is_rpo_valid() ||
        !cfg->tryUpdateRPO(*preheader, li->getLoopHead(), true)) {
        OC_is_rpo_valid(oc) = false;
        OC_is_cdg_valid(oc) = false;
    }
    //TODO:try update CDG
    OC_is_cdg_valid(oc) = false;
    return true;
}


//Iterative access LoopInfo tree. This funtion initialize the iterator.
//'li': the root of the LoopInfo tree.
//'it': iterator. It should be clean already.
//Readonly function.
LI<IRBB> const* iterInitLoopInfoC(LI<IRBB> const* li, OUT CLoopInfoIter & it)
{
    if (li == nullptr) { return nullptr; }
    for (LI<IRBB> const* x = li->getInnerList();
         x != nullptr; x = x->get_next()) {
        it.append_tail(x);
    }
    if (li->get_next() != nullptr) {
        it.append_tail(li->get_next());
    }
    return li;
}


//Iterative access LoopInfo tree.
//This function return the next LoopInfo accroding to 'it'.
//'it': iterator.
//Readonly function.
LI<IRBB> const* iterNextLoopInfoC(MOD CLoopInfoIter & it)
{
    LI<IRBB> const* li = it.remove_head();
    if (li == nullptr) { return nullptr; }
    for (LI<IRBB> const* x = li->getInnerList();
         x != nullptr; x = x->get_next()) {
        it.append_tail(x);
    }
    if (li->get_next() != nullptr) {
        it.append_tail(li->get_next());
    }
    return li;
}


//Find the BB that is the START of the unqiue backedge of loop.
//  BB1: loop-start bb
//  BB2: body
//  BB3: goto loop-start bb
//BB3 is the backedge-start bb.
//Return backedge BB id if found, otherwise return BBID_UNDEF.
IRBB * findBackedgeStartBB(LI<IRBB> const* li, IRCFG * cfg)
{
    UINT bbid = li->findBackedgeStartBB(cfg);
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
                                      Region * rg)
{
    ASSERT0(stmt->isCallStmt());
    bool retval_dom_all_use = false;
    bool maydef_dom_all_use = false;
    bool retval_checked = false;
    bool maydef_checked = false;
    if (stmt->isCallHasRetVal()) {
        PRSSAMgr * prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            retval_dom_all_use |= prssamgr->isStmtDomAllUseInsideLoop(stmt, li);
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
bool isStmtDomAllUseInsideLoop(IR const* stmt, LI<IRBB> const* li, Region * rg)
{
    ASSERT0(stmt->is_stmt());
    if (stmt->isCallHasRetVal()) {
        return isCallDomAllUseInsideLoop(stmt, li, rg);
    }

    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid() && stmt->isWritePR()) {
        return prssamgr->isStmtDomAllUseInsideLoop(stmt, li);
    }

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        stmt->isMemoryRefNonPR()) {
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

} //namespace xoc
