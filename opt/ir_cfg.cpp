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
#include "prdf.h"
#include "prssainfo.h"
#include "ir_ssa.h"

namespace xoc {

//IR_CFG
IR_CFG::IR_CFG(CFG_SHAPE cs, BBList * bbl, Region * ru,
               UINT edge_hash_size, UINT vertex_hash_size)
    : CFG<IRBB, IR>(bbl, edge_hash_size, vertex_hash_size)
{
    m_ru = ru;
    m_tm = ru->getTypeMgr();
    setBitSetMgr(ru->getBitSetMgr());
    if (m_bb_list->get_elem_count() == 0) {
        return;
    }

    //Add BB into graph.
    ASSERT0(m_bb_vec.get_last_idx() == -1);
    for (IRBB * bb = m_bb_list->get_tail();
         bb != NULL; bb = m_bb_list->get_prev()) {
        m_bb_vec.set(BB_id(bb), bb);
        addVertex(BB_id(bb));
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != NULL; li = bb->getLabelList().get_next()) {
            ASSERT(m_lab2bb.get(li) == NULL,
                   ("Label has been mapped to BB%d", BB_id(m_lab2bb.get(li))));
            m_lab2bb.set(li, bb);
            if (LABEL_INFO_is_catch_start(li)) {
                BB_is_catch_start(bb) = true;
            }
            if (LABEL_INFO_is_unreachable(li)) {
                BB_is_unreach(bb) = true;
            }
        }
    }

    switch (cs) {
    case C_SESE:
        {
            //Make sure the region has the unique entry.
            m_entry = m_ru->allocBB();
            BB_is_entry(m_entry) = true;
            BB_is_fallthrough(m_entry) = true;
            add_bb(m_entry);
            m_bb_list->append_head(m_entry);

            //Create logical exit BB.
            //NOTICE: In actually, the logical exit BB is ONLY
            //used to solve diverse dataflow equations, whereas
            //considering the requirement of ENTRY BB, EXIT BB.
            IRBB * exit = m_ru->allocBB();
            BB_is_fallthrough(exit) = true;
            add_bb(exit);
            m_bb_list->append_tail(exit);
            m_exit_list.append_tail(exit);
            break;
        }
    case C_SEME:
        {
            //Create entry BB.
            m_entry = m_ru->allocBB();
            BB_is_entry(m_entry) = true;
            //BB_is_fallthrough(entry) = true;
            add_bb(m_entry);
            m_bb_list->append_head(m_entry);

            //Collect exit BB.
            //for (IRBB * bb = m_bb_list->get_head();
            //     bb != NULL; bb = m_bb_list->get_next()) {
            //    if (IR_BB_is_func_exit(bb)) {
            //        m_exit_list.append_tail(bb);
            //    }
            //}
            break;
        }
    default: ASSERT(0, ("strang shape of CFG"));
    }
}


//Control flow optimization
void IR_CFG::cf_opt()
{
    bool change = true;
    while (change) {
        change = false;
        BBList * bbl = getBBList();
        for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
            change = goto_opt(bb);
            if (change) { break; }
            change = if_opt(bb);
            if (change) { break; }
        }
    }
}


//Do early control flow optimization.
void IR_CFG::initCfg(OptCtx & oc)
{
    if (getBBList()->get_elem_count() == 0) {
        //If bb is empty, set CFG is invalid.
        //OC_is_cfg_valid(oc) = true;
        return;
    }

    //cfg->removeEmptyBB();
    build(oc);
    buildEHEdge();

    //Rebuild cfg may generate redundant empty bb, it
    //disturb the computation of entry and exit.
    removeEmptyBB(oc);
    computeExitList();

    bool change = true;
    UINT count = 0;
    while (change && count < 20) {
        change = false;
        if (g_do_cfg_remove_empty_bb &&
            removeEmptyBB(oc)) {
            computeExitList();
            change = true;
        }

        if (g_do_cfg_remove_unreach_bb &&
            removeUnreachBB()) {
            computeExitList();
            change = true;
        }

        if (g_do_cfg_remove_trampolin_bb &&
            removeTrampolinEdge()) {
            computeExitList();
            change = true;
        }

        if (g_do_cfg_remove_unreach_bb &&
            removeUnreachBB()) {
            computeExitList();
            change = true;
        }

        if (g_do_cfg_remove_trampolin_bb &&
            removeTrampolinBB()) {
            computeExitList();
            change = true;
        }
        count++;
    }
    ASSERT0(!change);
    ASSERT0(verify());
}


void IR_CFG::findTargetBBOfMulticondBranch(
        IR const* ir,
        OUT List<IRBB*> & tgt_bbs)
{
    ASSERT0(ir->is_switch());
    tgt_bbs.clean();
    if (m_bb_list == NULL) { return; }

    IR * casev_list = SWITCH_case_list(ir);
    if (SWITCH_deflab(ir) != NULL) {
        IRBB * tbb = findBBbyLabel(SWITCH_deflab(ir));
        ASSERT0(tbb);
        tgt_bbs.append_tail(tbb);
    }

    if (casev_list != NULL) {
        for (IR * casev = casev_list;
             casev != NULL; casev = IR_next(casev)) {
            IRBB * tbb = findBBbyLabel(CASE_lab(casev));
            ASSERT0(tbb);
            tgt_bbs.append_tail(tbb);
        }
    }
}


//This function find BBs which belong to exception handler region.
//catch_start: start BB of an exception handler region.
//mainstreambbs: record BBs which can be reached from entry of
//    region. Note that the BB set only records BBs that in main stream
//    control flow.
//ehbbs: record all BB of the exception handler region.
//Note: this function does not clean ehbbs. Caller is responsible for that.
void IR_CFG::findEHRegion(
        IRBB const* catch_start,
        BitSet const& mainstreambbs,
        OUT BitSet & ehbbs)
{
    ASSERT0(catch_start && catch_start->isExceptionHandler());
    List<Vertex const*> list;
    Vertex const* bbv = get_vertex(BB_id(catch_start));
    ASSERT0(bbv);
    list.append_head(bbv);
    for (Vertex const* v = list.remove_head();
         v != NULL; v = list.remove_head()) {
        UINT id = VERTEX_id(v);
        if (mainstreambbs.is_contain(id) || ehbbs.is_contain(id)) {
            continue;
        }

        ehbbs.bunion(id);

        EdgeC * el = VERTEX_out_list(v);
        while (el != NULL) {
            Vertex const* succ = EDGE_to(EC_edge(el));
            if (!mainstreambbs.is_contain(VERTEX_id(succ)) &&
                !ehbbs.is_contain(VERTEX_id(succ))) {
                list.append_tail(succ);
            }
            el = EC_next(el);
        }
    }
}


//This function find all BB of exception try region, and record them in trybbs.
//trybbs: record all BB of the try region.
//Note: this function does not clean trybbs. Caller is responsible for that.
void IR_CFG::findAllTryRegions(OUT BitSet & trybbs)
{
    C<IRBB*> * ct;
    BitSet t;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        IRBB const* bb = ct->val();
        if (!bb->isTryStart()) { continue; }
        t.clean();
        findTryRegion(bb, t);
        trybbs.bunion(t);
    }
}


//This function find BBs which belong to exception try region.
//try_start: start BB of an entry of exception try region.
//trybbs: record all BB of the try region.
//Note: this function does not clean trybbs. Caller is responsible for that.
void IR_CFG::findTryRegion(IRBB const* try_start, OUT BitSet & trybbs)
{
    ASSERT0(try_start && try_start->isTryStart());
    List<Vertex const*> list;
    Vertex const* bbv = get_vertex(BB_id(try_start));
    ASSERT0(bbv);
    list.append_head(bbv);
    for (Vertex const* v = list.remove_head();
         v != NULL; v = list.remove_head()) {
        UINT id = VERTEX_id(v);
        if (trybbs.is_contain(id)) { continue; }

        trybbs.bunion(id);

        IRBB * bb = get_bb(id);
        ASSERT(bb, ("vertex on CFG correspond to nothing"));

        if (bb->isTryEnd() && bb != try_start) {
            //BB may have both try_start and try_end label.
            //If it is the case, the try_end is always other region's
            //end label, just ignore that.
            continue;
        }

        for (EdgeC * el = VERTEX_out_list(v); el != NULL; el = EC_next(el)) {
            Edge const* e = EC_edge(el);
            CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
            if (ei != NULL && CFGEI_is_eh(ei)) {
                //Do not consider EH edge.
                continue;
            }

            Vertex const* succ = EDGE_to(e);
            if (!trybbs.is_contain(VERTEX_id(succ))) {
                list.append_tail(succ);
            }

        }
    }
}


//Find a list bb that referred labels which is the target of ir.
void IR_CFG::findTargetBBOfIndirectBranch(IR const* ir, OUT List<IRBB*> & tgtlst)
{
    ASSERT0(ir->isIndirectBr());
    for (IR * c = IGOTO_case_list(ir); c != NULL; c = c->get_next()) {
        ASSERT0(c->is_case());
        IRBB * bb = m_lab2bb.get(CASE_lab(c));
        ASSERT0(bb); //no bb is correspond to lab.
        tgtlst.append_tail(bb);

        #ifdef _DEBUG_
        bool find = false;
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != NULL; li = bb->getLabelList().get_next()) {
            if (isSameLabel(CASE_lab(c), li)) {
                find = true;
                break;
            }
        }
        ASSERT0(find);
        #endif
    }
}


//Find natural loop and scan loop body to find call and early exit, etc.
void IR_CFG::LoopAnalysis(OptCtx & oc)
{
    if (getBBList()->get_elem_count() == 0) {
        //If bb is empty, set LoopInfo to be invalid.
        //OC_is_loopinfo_valid(oc) = true;
        return;
    }
    m_ru->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    findLoop();
    collectLoopInfo();
    OC_is_loopinfo_valid(oc) = true;
}


//Find bb that 'lab' attouchemented.
IRBB * IR_CFG::findBBbyLabel(LabelInfo const* lab)
{
    IRBB * bb = m_lab2bb.get(lab);
    if (bb == NULL) { return NULL; }

    #ifdef _DEBUG_
    bool find = false;
    for (LabelInfo const* li = bb->getLabelList().get_head();
         li != NULL; li = bb->getLabelList().get_next()) {
        if (isSameLabel(lab, li)) {
            find = true;
            break;
        }
    }
    ASSERT0(find);
    #endif

    return bb;
}


void IR_CFG::insertBBbetween(
        IN IRBB * from,
        IN C<IRBB*> * from_ct,
        IN IRBB * to,
        IN C<IRBB*> * to_ct,
        IN IRBB * newbb)
{
    //Revise BB list, note that 'from' is either fall-through to 'to',
    //or jumping to 'to'.
    BBList * bblst = getBBList();

    //First, processing edge if 'from'->'to' is fallthrough.
    C<IRBB*> * tmp_ct = from_ct;
    if (BB_is_fallthrough(from) && bblst->get_next(&tmp_ct) == to) {
        bblst->insert_after(newbb, from);
        insertVertexBetween(BB_id(from), BB_id(to), BB_id(newbb));
        BB_is_fallthrough(newbb) = true;
        return;
    }

    //Second, from->to is jump-edge.
    List<IRBB*> preds;
    get_preds(preds, to);
    ASSERT(preds.find(from), ("'from' is not pred of 'to'"));
    C<IRBB*> * pred_ct = NULL;
    for (IRBB * pred = preds.get_head(&pred_ct);
         pred != NULL; pred = preds.get_next(&pred_ct)) {
        C<IRBB*> * tmp_ct2 = to_ct;
        if (BB_is_fallthrough(pred) && bblst->get_prev(&tmp_ct2) == pred) {
            //Given 'to' has a fallthrough in-edge. Insert a tmp BB
            //e.g:
            //    from->bb1->bb2->to, all edges are fallthrough
            //    from->to, jump-edge
            //    bb1->to, jump-edge
            //
            //Here we need to revise the fallthrough-edge 'bb2->to',
            //the result is from->bb1->bb2->tmp_tramp_bb, all
            //edges are fallthrough tmp_tramp_bb->to, jump-edge
            //    from->to, jump-edge
            //    bb1->to, jump-edge
            //
            //    bb2->tmp_tramp_bb, tmp_tramp_bb->to, both are jump-edge.
            //    ir-list of tmp_tramp_bb is:
            //        goto L1:
            //
            //    ir-list of 'to' is:
            //        L1:
            //        ...
            //        ...
            IRBB * tmp_tramp_bb = m_ru->allocBB();
            LabelInfo * li = m_ru->genIlabel();
            IR * goto_ir = m_ru->buildGoto(li);
            BB_irlist(tmp_tramp_bb).append_tail(goto_ir);
            to->addLabel(li);
            m_lab2bb.set(li, to);
            add_bb(tmp_tramp_bb);
            bblst->insert_after(tmp_tramp_bb, pred);

            insertVertexBetween(BB_id(pred), BB_id(to), BB_id(tmp_tramp_bb));

            //Fall through edge has been broken, insert 'newbb' before 'to'.
            break;
        }
    }

    //Revise the target LABEL of last XR in 'from'.
    IR * last_xr_of_from = get_last_xr(from);

    ASSERT0(last_xr_of_from->get_label() &&
            findBBbyLabel(last_xr_of_from->get_label()) == to);
    ASSERT0(last_xr_of_from->get_label() != NULL);

    LabelInfo * li = m_ru->genIlabel();
    last_xr_of_from->set_label(li);

    newbb->addLabel(li);
    m_lab2bb.set(li, newbb);

    //When we get here, there are NOT any fallthrough in-edges of 'to' exist.
    bblst->insert_before(newbb, to_ct);
    insertVertexBetween(BB_id(from), BB_id(to), BB_id(newbb));
    BB_is_fallthrough(newbb) = true;
}


//Migrate Lables from src BB to tgt BB.
void IR_CFG::moveLabels(IRBB * src, IRBB * tgt)
{
    tgt->mergeLabeInfoList(src);

    //Set label2bb map.
    for (LabelInfo const* li = tgt->getLabelList().get_head();
         li != NULL; li = tgt->getLabelList().get_next()) {
        m_lab2bb.setAlways(li, tgt);
    }

    //Cut off the relation between src and Labels.
    src->cleanLabelInfoList();
}


//Cut off the mapping relation bwteen Labels and BB.
void IR_CFG::resetMapBetweenLabelAndBB(IRBB * bb)
{
    for (LabelInfo const* li = bb->getLabelList().get_head();
         li != NULL; li = bb->getLabelList().get_next()) {
        m_lab2bb.setAlways(li, NULL);
    }
    bb->cleanLabelInfoList();
}



//Remove trampoline BB.
//e.g: bb1->bb2->bb3
//    stmt of bb2 is just 'goto bb3', and bb3 is the NEXT BB of bb2 in BB-List.
//    Then bb2 is tramp BB.
//Return true if at least one tramp BB removed.
//
//ALGO:
//    for each pred of BB
//        if (pred is fallthrough && prev of BB == pred)
//            remove edge pred->BB.
//            add edge pred->BB's next.
//            continue;
//        end if
//        duplicate LabelInfo from BB to BB's next.
//        revise LabelInfo of pred to new target BB.
//        remove edge pred->BB.
//        add edge pred->BB's next.
//    end for
bool IR_CFG::removeTrampolinBB()
{
    bool removed = false;
    C<IRBB*> * ct;
    List<IRBB*> succs;
    List<IRBB*> preds;
    for (IRBB * bb = m_bb_list->get_head(&ct);
         bb != NULL; bb = m_bb_list->get_next(&ct)) {
        if (bb->isExceptionHandler()) { continue; }

        IR * uncond_br = get_first_xr(bb);
        if (uncond_br == NULL ||
            !uncond_br->isUnconditionalBr() ||
            bb->getNumOfIR() != 1) {
            continue;
        }

        //CASE: Given pred1->bb, fallthrough edge,
        //  and pred2->bb, jumping edge.
        //  bb:
        //      goto L1
        //
        //  next of bb:
        //      L1:
        //      ...
        //      ...
        //Remove bb and revise CFG.
        get_succs(succs, bb);
        C<IRBB*> * tmp_bb_ct = ct;
        IRBB * next = m_bb_list->get_next(&tmp_bb_ct);
        ASSERT0(succs.get_elem_count() == 1);

        if (next == NULL || //bb may be the last BB in bb-list.
            next != succs.get_head()) { //next BB is not the successor.
            continue;
        }

        tmp_bb_ct = ct;
        IRBB * prev = m_bb_list->get_prev(&tmp_bb_ct);
        preds.clean(); //use list because cfg may be modify.
        get_preds(preds, bb);
        for (IRBB * pred = preds.get_head();
             pred != NULL; pred = preds.get_next()) {
            moveLabels(bb, next);

            if (BB_is_fallthrough(pred) && prev == pred) {
                removeEdge(pred, bb);

                //Add normal control flow edge.
                Edge * e = addEdge(BB_id(pred), BB_id(next));
                CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
                if (ei != NULL && CFGEI_is_eh(ei)) {
                    //If there is already an edge, check if it is an
                    //exception edge. If it is, change the exception edge
                    //to be normal control flow edge.
                    CFGEI_is_eh(ei) = false;
                }
                continue;
            }

            //CASE:
            // pred:
            //     goto L2:
            // ...
            // ...
            // bb: L2
            //     goto L1
            //
            // next of bb:
            //     L1:
            //     ...
            //     ...
            //Remove bb and revise CFG.

            //Revise branch target LabelInfo of xr in 'pred'.
            IR * last_xr_of_pred = get_last_xr(pred);

            ASSERT0(last_xr_of_pred->get_label());
            ASSERT(findBBbyLabel(last_xr_of_pred->get_label()) == next,
                   ("Labels of bb should have already moved to "
                    "next BB by moveLabels()"));
            last_xr_of_pred->set_label(uncond_br->get_label());
            removeEdge(pred, bb);

            Edge * e = addEdge(BB_id(pred), BB_id(next));
            CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
            if (ei != NULL && CFGEI_is_eh(ei)) {
                //If there is already an edge, check if it is an
                //exception edge. If it is, change the exception edge
                //to be normal control flow edge.
                CFGEI_is_eh(ei) = false;
            }
        } //end for each pred of BB.

        //The map between Labels and BB has changed.
        //resetMapBetweenLabelAndBB(bb);
        remove_bb(bb);
        removed = true;
        bb = m_bb_list->get_head(&ct); //reprocessing BB list.
    } //end for each BB
    return removed;
}


//Remove trampoline edge.
//e.g: bb1->bb2->bb3
//stmt of bb2 is just 'goto bb3', then bb1->bb2 is tramp edge.
//And the resulting edges are bb1->bb3, bb2->bb3 respectively.
//Return true if at least one tramp edge removed.
bool IR_CFG::removeTrampolinEdge()
{
    bool removed = false;
    C<IRBB*> * ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        IRBB * bb = ct->val();
        if (bb->getNumOfIR() != 1) { continue; }

        IR * last_xr = get_last_xr(bb);
        if (last_xr->is_goto() && !bb->isAttachDedicatedLabel()) {
            LabelInfo const* tgt_li = last_xr->get_label();
            ASSERT0(tgt_li != NULL);

            List<IRBB*> preds; //use list because cfg may be modify.
            get_preds(preds, bb);

            IRBB * succ = get_first_succ(bb);
            ASSERT0(succ);

            ASSERT0(findBBbyLabel(tgt_li) == succ);

            for (IRBB * pred = preds.get_head();
                 pred != NULL; pred = preds.get_next()) {
                if (pred == bb) {
                    //bb's pred is itself.
                    continue;
                }

                if (pred->getNumOfIR() == 0) {
                    continue;
                }

                IR * last_xr_of_pred = get_last_xr(pred);
                if (!pred->is_down_boundary(last_xr_of_pred)) {
                    // CASE: pred->bb, pred is fallthrough-BB.
                    //  pred is:
                    //      a=b+1
                    //  ...
                    //  bb is:
                    //      L1:
                    //      goto L2
                    //=>
                    //  pred is:
                    //      a=b+1
                    //      goto L2
                    //  ...
                    //  bb is:
                    //      L1:
                    //      goto L2
                    BB_irlist(pred).append_tail(m_ru->dupIRTree(last_xr));
                    BB_is_fallthrough(pred) = false;
                    removeEdge(pred, bb);

                    addEdge(BB_id(pred), BB_id(succ));
                    bb->dupSuccessorPhiOpnd(this, m_ru, WhichPred(bb, succ));
                    removed = true;
                    continue;
                } //end if

                if (last_xr_of_pred->is_goto()) {
                    //CASE: pred->bb,
                    //    pred is:
                    //        goto L1
                    //    ...
                    //    bb is:
                    //        L1:
                    //        goto L2
                    //=>
                    //    pred to be:
                    //        goto L2
                    //    ...
                    //    bb is:
                    //        L1:
                    //        goto L2
                    ASSERT0(last_xr_of_pred->get_label() &&
                            findBBbyLabel(last_xr_of_pred->get_label()) == bb);

                    ASSERT0(last_xr_of_pred->get_label() != NULL);
                    GOTO_lab(last_xr_of_pred) = tgt_li;
                    removeEdge(pred, bb);
                    addEdge(BB_id(pred), BB_id(succ));
                    removed = true;
                    continue;
                } //end if

                if (last_xr_of_pred->isConditionalBr()) {
                    // CASE: pred->f, pred->bb, and pred->f is fall through edge.
                    //  pred is:
                    //      truebr/falsebr L1
                    //
                    //  f is:
                    //      ...
                    //      ...
                    //
                    //  bb is:
                    //      L1:
                    //      goto L2
                    //=>
                    //  pred is:
                    //      truebr/falsebr L2
                    //
                    //  f is:
                    //      ...
                    //      ...
                    //
                    //  bb is:
                    //      L1:
                    //      goto L2
                    C<IRBB*> * prev_of_bb = ct;
                    if (m_bb_list->get_prev(&prev_of_bb) == pred) {
                        //Can not remove jumping-edge if 'bb' is
                        //fall-through successor of 'pred'.
                        continue;
                    }

                    ASSERT0(last_xr_of_pred->get_label() &&
                            findBBbyLabel(last_xr_of_pred->get_label()) == bb);

                    ASSERT0(last_xr_of_pred->get_label() != NULL);
                    if (bb != succ) {
                        //bb should not be the same one with succ.
                        BR_lab(last_xr_of_pred) = tgt_li;

                        //Link up bb's pred and succ.
                        removeEdge(pred, bb);
                        addEdge(BB_id(pred), BB_id(succ));
                        removed = true;
                    }
                    continue;
                } //end if
            }
        } //end if xr is uncond branch.
    } //end for    each BB
    return removed;
}


bool IR_CFG::removeRedundantBranch()
{
    bool removed = CFG<IRBB, IR>::removeRedundantBranch();
    C<IRBB*> * ct;
    IR_DU_MGR * dumgr = m_ru->getDUMgr();
    List<IRBB*> succs;
    for (IRBB * bb = m_bb_list->get_head(&ct);
         bb != NULL; bb = m_bb_list->get_next(&ct)) {
        IR * last_xr = get_last_xr(bb);
        if (last_xr != NULL && last_xr->isConditionalBr()) {
            IR * det = BR_det(last_xr);
            ASSERT0(det != NULL);
            bool always_true = (det->is_const() &&
                                det->is_int() &&
                                CONST_int_val(det) != 0) ||
                                det->is_str();
            bool always_false = det->is_const() &&
                                det->is_int() &&
                                CONST_int_val(det) == 0;

            if ((last_xr->is_truebr() && always_true) ||
                (last_xr->is_falsebr() && always_false)) {
                //Substitute cond_br with 'goto'.
                LabelInfo const* tgt_li = last_xr->get_label();
                ASSERT0(tgt_li != NULL);

                BB_irlist(bb).remove_tail();

                if (dumgr != NULL) {
                    dumgr->removeIROutFromDUMgr(last_xr);
                }

                m_ru->freeIRTree(last_xr);

                IR * uncond_br = m_ru->buildGoto(tgt_li);
                BB_irlist(bb).append_tail(uncond_br);

                //Remove fallthrough edge, leave branch edge.
                get_succs(succs, bb);
                C<IRBB*> * tmp_ct = ct;
                m_bb_list->get_next(&tmp_ct);
                for (IRBB * s = succs.get_head();
                     s != NULL; s = succs.get_next()) {
                    if (s == tmp_ct->val()) {
                        //Remove branch edge, leave fallthrough edge.
                        removeEdge(bb, s);
                    }
                }
                removed = true;
            } else if ((last_xr->is_truebr() && always_false) ||
                       (last_xr->is_falsebr() && always_true)) {
                IR * r = BB_irlist(bb).remove_tail();
                if (dumgr != NULL) {
                    dumgr->removeIROutFromDUMgr(r);
                }
                m_ru->freeIRTree(r);

                //Remove branch edge, leave fallthrough edge.
                get_succs(succs, bb);
                C<IRBB*> * tmp_ct = ct;
                m_bb_list->get_next(&tmp_ct);
                for (IRBB * s = succs.get_head();
                     s != NULL; s = succs.get_next()) {
                    if (s != tmp_ct->val()) {
                        removeEdge(bb, s);
                    }
                }
                removed = true;
            }
        }
    } //for each BB
    return removed;
}


void IR_CFG::dump_dot(CHAR const* name, bool detail, bool dump_eh)
{
    //Note this function does not use g_tfile as output.
    //So it is dispensable to check g_tfile.
    if (name == NULL) {
        name = "graph_cfg.dot";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERT(h, ("%s create failed!!!", name));

    //Print comment
    FILE * org_tfile = g_tfile;
    g_tfile = h;

    fprintf(h, "\n/*");
    for (IRBB * bb = m_bb_list->get_head();
         bb != NULL; bb = m_bb_list->get_next()) {
        fprintf(h, "\n--- BB%d ----", BB_id(bb));
        dumpBBList(m_bb_list, m_ru);
        //fprintf(h, "\n\t%s", dump_ir_buf(ir, buf));
    }
    fprintf(h, "\n*/\n");
    fprintf(h, "digraph G {\n");

    //fprintf(h, "rankdir=LR;\n"); //Layout from Left to Right.
    //fprintf(h, "rankdir=TB;\n");
    //fprintf(h, "rankdir=BT;\n");

    //Print carriage return for dot file.
    bool org_prt_cr = g_prt_carriage_return_for_dot;
    g_prt_carriage_return_for_dot = true;

    //Print Region name.
    fprintf(h, "\nstartnode [fontsize=24,style=filled, "
               "color=gold,shape=none,label=\"RegionName:%s\"];",
               m_ru->getRegionName());

    //Print node
    INT c;
    for (Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        INT id = VERTEX_id(v);

        IRBB * bb = get_bb(id);
        ASSERT0(bb);

        CHAR const* shape = "box";
        CHAR const* font = "courB";
        CHAR const* color = "black";
        CHAR const* style = "bold";
        UINT fontsize = 12;

        if (BB_is_catch_start(bb)) {
            font = "Times Bold";
            fontsize = 18;
            color = "lightblue";
            style = "filled";
        }

        if (BB_is_entry(bb) || BB_is_exit(bb)) {
            font = "Times Bold";
            fontsize = 18;
            color = "cyan";
            style = "filled";
        }

        if (detail) {
            fprintf(h,
                    "\nnode%d [font=\"%s\",fontsize=%d,color=%s,"
                    "shape=%s,style=%s,label=\" BB%d",
                    id,
                    font,
                    fontsize,
                    color,
                    shape,
                    style,
                    id);
            for (IR * ir = BB_first_ir(bb);
                 ir != NULL; ir = BB_next_ir(bb)) {
                fprintf(h, "\\l");

                 //TODO: implement dump_ir_buf();
                dump_ir(ir, m_tm, NULL, true, false, false);
            }

            //The last \l is very important to display DOT in a fine manner.
            fprintf(h, "\\l");

            fprintf(h, "\"];");
        } else {
            fprintf(h,
                    "\nnode%d [font=\"%s\",fontsize=%d,color=%s,"
                    "shape=%s,style=%s,label=\" BB%d\"];",
                    id,
                    font,
                    fontsize,
                    color,
                    shape,
                    style,
                    id);
        }
        fflush(h);
    }

    //Print edge
    for (Edge const* e = m_edges.get_first(c);
         e != NULL;  e = m_edges.get_next(c)) {
        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei == NULL) {
            fprintf(h,
                    "\nnode%d->node%d[style=bold, color=maroon, label=\"%s\"]",
                    VERTEX_id(EDGE_from(e)),
                    VERTEX_id(EDGE_to(e)),
                    "");
        } else if (CFGEI_is_eh(ei)) {
            if (dump_eh) {
                fprintf(h,
                        "\nnode%d->node%d[style=dotted, "
                        "color=darkslategray, label=\"%s\"]",
                        VERTEX_id(EDGE_from(e)),
                        VERTEX_id(EDGE_to(e)),
                        "");
            }
        } else {
            ASSERT(0, ("unsupport EDGE_INFO"));
        }
    }

    g_tfile = org_tfile;
    g_prt_carriage_return_for_dot = org_prt_cr;
    fprintf(h,"\n}\n");
    fclose(h);
}


void IR_CFG::dump_node(FILE * h, bool detail)
{
    ASSERT0(m_bb_list);
    C<IRBB*> * bbct;
    UINT vertical_order = 1;
    for (IRBB * bb = m_bb_list->get_head(&bbct);
         bb != NULL; bb = m_bb_list->get_next(&bbct)) {
        INT id = BB_id(bb);
        Vertex * v = get_vertex(id);
        ASSERT(v, ("bb is not in cfg"));
        CHAR const* shape = "box";
        if (BB_is_catch_start(bb)) {
            shape = "uptrapezoid";
        }

        CHAR const* font = "courB";
        INT scale = 1;
        CHAR const* color = "gold";
        if (BB_is_entry(bb) || BB_is_exit(bb)) {
            font = "Times Bold";
            scale = 2;
            color = "cyan";
        }

        if (detail) {
            fprintf(h,
                "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
                "fontname:\"%s\" scaling:%d label:\"",
                id, vertical_order++, shape, color, font, scale);
            fprintf(h, "   BB%d ", id);
            if (VERTEX_rpo(v) != 0) {
                fprintf(h, " rpo:%d ", VERTEX_rpo(v));
            }

            IRBB * bb2 = get_bb(id);
            ASSERT0(bb2 != NULL);
            dumpBBLabel(bb2->getLabelList(), h);
            fprintf(h, "\n");
            for (IR * ir = BB_first_ir(bb2);
                 ir != NULL; ir = BB_next_ir(bb2)) {
                //fprintf(h, "%s\n", dump_ir_buf(ir, buf));

                //TODO: implement dump_ir_buf();
                dump_ir(ir, m_tm, NULL, true, false);
            }
            fprintf(h, "\"}");
        } else {
            fprintf(h,
                    "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
                    "fontname:\"%s\" scaling:%d label:\"%d",
                    id, vertical_order++, shape, color, font, scale, id);
            if (VERTEX_rpo(v) != 0) {
                fprintf(h, " rpo:%d", VERTEX_rpo(v));
            }
            fprintf(h, "\" }");
        }
    }
}


//Print graph structure description.
void IR_CFG::dump_head(FILE * h)
{
    fprintf(h, "graph: {"
              "title: \"Graph\"\n"
              "shrink:    15\n"
              "stretch: 27\n"
              "layout_downfactor: 1\n"
              "layout_upfactor: 1\n"
              "layout_nearfactor: 1\n"
              "layout_splinefactor: 70\n"
              "spreadlevel: 1\n"
              "treefactor: 0.500000\n"
              "node_alignment: center\n"
              "orientation: top_to_bottom\n"
              "late_edge_labels: no\n"
              "display_edge_labels: yes\n"
              "dirty_edge_labels: no\n"
              "finetuning: no\n"
              "nearedges: no\n"
              "splines: yes\n"
              "ignoresingles: no\n"
              "straight_phase: no\n"
              "priority_phase: no\n"
              "manhatten_edges: no\n"
              "smanhatten_edges: no\n"
              "port_sharing: no\n"
              "crossingphase2: yes\n"
              "crossingoptimization: yes\n"
              "crossingweight: bary\n"
              "arrow_mode: free\n"
              "layoutalgorithm: mindepthslow\n"
              "node.borderwidth: 2\n"
              "node.color: lightcyan\n"
              "node.textcolor: black\n"
              "node.bordercolor: blue\n"
              "edge.color: darkgreen\n");
}


void IR_CFG::dump_edge(FILE * h, bool dump_eh)
{
    INT c;
    for (Edge * e = m_edges.get_first(c); e != NULL; e = m_edges.get_next(c)) {
        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei == NULL) {
            fprintf(h,
                    "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                    " thickness:4 color:darkred }",
                    VERTEX_id(EDGE_from(e)), VERTEX_id(EDGE_to(e)));
        } else if (CFGEI_is_eh(ei)) {
            if (dump_eh) {
                fprintf(h,
                        "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                        "linestyle:dotted color:lightgrey }",
                        VERTEX_id(EDGE_from(e)), VERTEX_id(EDGE_to(e)));
            }
        } else {
            fprintf(h,
                    "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                    " thickness:4 color:darkred }",
                    VERTEX_id(EDGE_from(e)), VERTEX_id(EDGE_to(e)));
        }
    }
}


void IR_CFG::dump_vcg(CHAR const* name, bool detail, bool dump_eh)
{
    ASSERT0(m_ru);

    if (name == NULL) { name = "graph_cfg.vcg"; }

    //Note this function does not use g_tfile as output.
    //So it is dispensable to check g_tfile.
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERT(h != NULL, ("%s create failed!!!",name));
    FILE * old = NULL;

    //Print comment
    //fprintf(h, "\n/*");
    //old = g_tfile;
    //g_tfile = h;
    //dumpBBList(m_bb_list, m_ru);
    //g_tfile = old;
    //fprintf(h, "\n*/\n");
    dump_head(h);

    //Print Region name.
    fprintf(h,
            "\nnode: {title:\"\" vertical_order:0 shape:box color:turquoise "
            "borderwidth:0 fontname:\"Courier Bold\" "
            "scaling:2 label:\"RegionName:%s\" }", m_ru->getRegionName());
    old = g_tfile;
    g_tfile = h;
    dump_node(h, detail);
    dump_edge(h, dump_eh);

    g_tfile = old;
    fprintf(h, "\n}\n");
    fclose(h);
}


void IR_CFG::computeDomAndIdom(IN OUT OptCtx & oc, BitSet const* uni)
{
    if (getBBList()->get_elem_count() == 0) { return; }

    UNUSED(uni);
    START_TIMER_AFTER();
    ASSERT0(OC_is_cfg_valid(oc));
    ASSERT(m_entry, ("ONLY support SESE or SEME"));

    m_ru->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    List<IRBB*> * bblst = getBBListInRPO();
    ASSERT0(bblst->get_elem_count() == m_ru->getBBList()->get_elem_count());

    List<Vertex const*> vlst;
    for (IRBB * bb = bblst->get_head(); bb != NULL; bb = bblst->get_next()) {
        ASSERT0(BB_id(bb) != 0);
        vlst.append_tail(get_vertex(BB_id(bb)));
    }

    //DGraph::computeDom(&vlst, uni);
    //DGraph::computeIdom();

    bool f = DGraph::computeIdom2(vlst);
    UNUSED(f);
    ASSERT0(f);

    f = DGraph::computeDom2(vlst);
    UNUSED(f);
    ASSERT0(f);

    OC_is_dom_valid(oc) = true;
    END_TIMER_AFTER("Compute Dom, IDom");
}


void IR_CFG::computePdomAndIpdom(IN OUT OptCtx & oc, BitSet const* uni)
{
    if (getBBList()->get_elem_count() == 0) { return; }

    START_TIMER("Compute PDom,IPDom");
    ASSERT0(OC_is_cfg_valid(oc));

    m_ru->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    List<IRBB*> * bblst = getBBListInRPO();
    ASSERT0(bblst->get_elem_count() == m_ru->getBBList()->get_elem_count());

    List<Vertex const*> vlst;
    for (IRBB * bb = bblst->get_tail(); bb != NULL; bb = bblst->get_prev()) {
        ASSERT0(BB_id(bb) != 0 && get_vertex(BB_id(bb)));
        vlst.append_tail(get_vertex(BB_id(bb)));
    }

    bool f = false;
    if (uni != NULL) {
        f = DGraph::computePdom(&vlst, uni);
    } else {
        f = DGraph::computePdom(&vlst);
    }
    UNUSED(f);
    ASSERT0(f);

    f = DGraph::computeIpdom();
    ASSERT0(f);

    OC_is_pdom_valid(oc) = true;
    END_TIMER();
}


void IR_CFG::remove_xr(IRBB * bb, IR * ir)
{
    IR_DU_MGR * dumgr = m_ru->getDUMgr();
    if (dumgr != NULL) {
        dumgr->removeIROutFromDUMgr(ir);
    }
    ir->removeSSAUse();
    ir = BB_irlist(bb).remove(ir);
    m_ru->freeIRTree(ir);
}


//Perform miscellaneous control flow optimizations.
//Include removing dead bb which is unreachable, removing empty bb as many
//as possible, simplify and remove the branch like "if (x==x)", removing
//the trampolin branch.
bool IR_CFG::performMiscOpt(OptCtx & oc)
{
    START_TIMER_AFTER();

    bool change = false;
    bool ck_cfg = false;
    bool lchange;
    UINT count = 0;

    do {
        lchange = false;

        if (g_do_cfg_remove_unreach_bb) {
            lchange |= removeUnreachBB();
        }

        if (g_do_cfg_remove_empty_bb) {
            lchange |= removeEmptyBB(oc);
        }

        if (g_do_cfg_remove_redundant_branch) {
            lchange |= removeRedundantBranch();
        }

        if (g_do_cfg_remove_trampolin_bb) {
            lchange |= removeTrampolinEdge();
        }

        if (lchange) {
            OC_is_cdg_valid(oc) = false;
            OC_is_dom_valid(oc) = false;
            OC_is_pdom_valid(oc) = false;
            OC_is_loopinfo_valid(oc) = false;
            OC_is_rpo_valid(oc) = false;
            ck_cfg = true;
        }

        count++;
    } while (lchange && count < 1000);

    ASSERT(!lchange, ("CFG optimization iterated too many times."));

    if (ck_cfg) {
        computeExitList();

        ASSERT0(verifySSAInfo(m_ru));

        #ifdef _DEBUG_
        //Check cfg validation, which
        //need cdg to be available.
        //This check is only in debug mode.
        OC_is_rpo_valid(oc) = false;
        computePdomAndIpdom(oc, NULL);
        CDG * cdg = (CDG*)m_ru->getPassMgr()->registerPass(PASS_CDG);
        cdg->rebuild(oc, *m_ru->getCFG());
        ASSERT0(verifyIfBBRemoved(cdg, oc));
        #endif
    }

    ASSERT0(verifyIRandBB(getBBList(), m_ru));
    ASSERT0(m_ru->verifyRPO(oc));

    END_TIMER_AFTER("CFG Optimization");
    return change;
}

} //namespace xoc
