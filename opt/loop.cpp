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

//Find the bb that is the start of the unqiue backedge of loop.
//  BB1: loop start bb
//  BB2: body start bb
//  BB3: goto loop start bb
//
//BB2 is the loop header fallthrough bb.
bool findTwoSuccessorBBOfLoopHeader(LI<IRBB> const* li,
                                    IRCFG * cfg,
                                    UINT * succ1,
                                    UINT * succ2)
{
    ASSERT0(li && cfg && succ1 && succ2);
    IRBB * head = li->getLoopHead();

    xcom::Vertex * headvex = cfg->getVertex(head->id());
    if (cfg->getOutDegree(headvex) != 2) {
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
        from->getIRList()->append_tail(gotoir);
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


//Fix up edge relation when preheader inserted.
//pred: predecessor of head which is inside loop body.
static void fixupInnerLoopEdgeBetweenHeadAndPreheader(LI<IRBB> const* li,
                                                      Region * rg,
                                                      IRBB * head,
                                                      IRBB * pred)
{
    IRCFG * cfg = rg->getCFG();
    //BB_p is predecessor of loop-header that outside from loop;
    //BB_1 is also predecessor of loop-header, but it belongs to loop.
    //CASE:
    //   BB_p
    //   |
    // ---
    // |  BB_1<--...
    // |  |
    // |  | //fallthrough
    // v  v
    // BB_header
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
    // BB_header
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
    // BB_header <--
    LabelInfo const* lab = head->getLabelList().get_head();
    if (lab == nullptr) {
        lab = rg->genILabel();
        cfg->addLabel(head, lab);
    }
    tryAppendGotoToJumpToBB(pred, head, rg);
}


//Return true if inserted a new BB.
static bool updateOutterLoopEdgeBetweenHeadAndPreheader(
    LI<IRBB> const* li,
    Region * rg,
    IRBB * head,
    IRBB * pred,
    IRBB * preheader,
    MOD LabelInfo const** preheader_lab,
    bool insert_bb)
{
    IRCFG * cfg = rg->getCFG();
    if (!insert_bb) {
        //Insert preheader in front of head.
        //Note preheader must fallthrough to head.
        cfg->insertVertexBetween(pred->id(), head->id(), preheader->id());
        cfg->tryFindLessRpo(preheader, head);
        insert_bb = true;
    }

    //CASE1:
    //  BB_p
    //   |
    //   v
    //  BB_header
    //Have to get the last IR of
    //BB and judge if it is conditional branch.
    //if (BB_is_fallthrough(p)) {
    //    //Nothing to do.
    //    continue;
    //}

    //CASE2:
    //  BB_p(goto lab1)
    //   |
    //   ... //a lot of BB
    //   |
    //   v
    //  BB_header(lab1)
    //=>
    //  BB_p(goto lab2)
    //   |
    //   ... //a lot of BB
    //   |
    //   v
    //  BB_preheader(lab2)
    //   |  //fallthrough
    //   v
    //  BB_header(lab2)
    //Try to update the target label of the last IR of predecessor.
    IR * last_ir = BB_last_ir(pred);
    if (last_ir == nullptr) {
        //Nothing to update.
        return insert_bb;
    }

    //Update the last IR.
    if ((last_ir->isConditionalBr() || last_ir->isUnconditionalBr()) &&
        head == cfg->findBBbyLabel(last_ir->getLabel())) {
        if (*preheader_lab == nullptr) {
            //There is no label on preheader, make it.
            *preheader_lab = rg->genILabel();
        }

        //Add the new label to preheader if not exist.
        cfg->addLabel(preheader, *preheader_lab);

        //Update branch-target of last IR of predecessor.
        last_ir->setLabel(*preheader_lab);
    }
    return insert_bb;
}


//Return true if inserted a new BB.
static bool updateEdgeBetweenHeadAndPreheader(LI<IRBB> const* li,
                                              Region * rg,
                                              IRBB * head,
                                              IRBB * preheader)
{
    bool insert_bb = false;
    IRCFG * cfg = rg->getCFG();
    List<IRBB*> preds;
    cfg->get_preds(preds, head);
    LabelInfo const* preheader_lab = nullptr;
    for (IRBB * p = preds.get_head(); p != nullptr; p = preds.get_next()) {
        if (li->isInsideLoop(p->id())) {
            ASSERTN(cfg->getVertex(preheader->id()),
                    ("vex should have been added before current function"));
            fixupInnerLoopEdgeBetweenHeadAndPreheader(li, rg, head, p);
            continue;
        }
        insert_bb |= updateOutterLoopEdgeBetweenHeadAndPreheader(li,
            rg, head, p, preheader, &preheader_lab, insert_bb);
    }
    return insert_bb;
}


//Move LabelInfos from head to preheader except LabelInfos that
//are the target of IR that belongs to loop body.
static void tryMoveLabelFromHeadToPreheader(LI<IRBB> const* li, IRCFG * cfg,
                                            IRBB * head, IRBB * preheader)
{
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
    for (INT i = li->getBodyBBSet()->get_first();
         i >= 0; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = cfg->getBB(i);
        ASSERT0(bb);
        for (IR const* ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
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
static IRBB * findAppropriatePreheader(LI<IRBB> const* li, IRCFG * cfg,
                                       IRBB const* head, IRBB * prev)
{
    IRBB * appropriate_bb = nullptr;
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
                appropriate_bb = prev;
                break;
            }
            if (!IRBB::isLowerBoundary(const_cast<IRBB*>(prev)->getLastIR())) {
                //prev should fallthrough to head BB.
                //Otherwise can not append IR to prev BB.
                appropriate_bb = prev;
                break;
            }
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
                appropriate_bb = cfg->getBB(pred);
                break;
            }
        }
    }
    return appropriate_bb;
}


//Find preheader BB. If it does not exist, insert one before loop 'li'.
//insert_bb: return true if this function insert a new bb before loop,
//           otherwise return false.
//force: force to insert preheader BB whatever it has been exist.
//       Return the new BB if insertion is successful.
//Note if we find the preheader, the last IR of it may be call.
//So if you are going to insert IR at the tail of preheader, the best is
//force to insert a new bb.
IRBB * findAndInsertPreheader(LI<IRBB> const* li, Region * rg,
                              OUT bool & insert_bb, bool force)
{
    ASSERT0(li && rg);
    insert_bb = false;
    IRCFG * cfg = rg->getCFG();
    BBList * bblst = rg->getBBList();
    IRBB * head = li->getLoopHead();

    BBListIter bbholder = nullptr;
    bblst->find(head, &bbholder);
    ASSERT0(bbholder);
    BBListIter tt = bbholder;
    IRBB * prev = bblst->get_prev(&tt);
    IRBB * appropriate_prev_bb = findAppropriatePreheader(li, cfg, head, prev);
    if (!force) {
        if (appropriate_prev_bb != nullptr) {
            ASSERT0(appropriate_prev_bb->rpo() != RPO_UNDEF);
            return appropriate_prev_bb;
        }
        return nullptr;
    }

    IRBB * preheader = rg->allocBB();
    cfg->addBB(preheader);
    bblst->insert_before(preheader, bbholder);
    insert_bb |= updateEdgeBetweenHeadAndPreheader(li, rg, head, preheader);
    tryMoveLabelFromHeadToPreheader(li, cfg,head, preheader);
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
    for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
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
    for (INT i = duset->get_first(&dui);
         i >= 0; i = duset->get_next(i, &dui)) {
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


//Return true if all expressions on 'ir' tree is loop invariant.
//ir: root node of IR tree
//li: loop info structure
//check_tree: true to perform check recusively for entire IR tree.
//Note this function does not check the sibling node of 'ir'.
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


//Try inserting preheader BB of loop 'li'.
//preheader: record the preheader either inserted BB or existed BB.
//Return true if inserting a new BB before loop, otherwise false.
//
//Note if we find the preheader, the last IR of it may be call.
//So if you are going to insert IR at the tail of preheader, the best is
//force to insert a new bb.
bool insertPreheader(LI<IRBB> * li, Region * rg, OUT IRBB ** preheader)
{
    IRCFG * cfg = rg->getCFG();
    bool inserted = false;
    IRBB * p = findAndInsertPreheader(li, rg, inserted, false);
    if (p == nullptr || cfg->isRegionEntry(p)) {
        p = findAndInsertPreheader(li, rg, inserted, true);
        //Do not mark PASS changed if just inserted some BBs rather than
        //code motion, because post CFG optimization will removed the
        //redundant BB.
    }

    ASSERT0(p);
    if (inserted && !li->isOuterMost()) {
        //Update loop body BB set, add preheader to outer loop body.
        li->getOuter()->getBodyBBSet()->bunion(p->id());
    }

    ASSERT0(preheader);
    *preheader = p;
    return inserted;
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


//Find the bb that is the START of the unqiue backedge of loop.
//  BB1: loop-start bb
//  BB2: body
//  BB3: goto loop-start bb
//
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

} //namespace xoc
