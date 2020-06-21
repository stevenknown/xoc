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

//IRCFG
IRCFG::IRCFG(CFG_SHAPE cs, BBList * bbl, Region * rg,
             UINT edge_hash_size, UINT vertex_hash_size)
    : CFG<IRBB, IR>(bbl, edge_hash_size, vertex_hash_size)
{
    m_rg = rg;
    m_tm = rg->getTypeMgr();
    m_cs = cs;
    setBitSetMgr(rg->getBitSetMgr());
    initEntryAndExit(m_cs);
}


//Control flow optimization
void IRCFG::cf_opt()
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


//Construct EH edge after cfg built.
void IRCFG::buildEHEdge()
{
    ASSERTN(m_bb_list, ("bb_list is emt"));
    BBListIter ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        IRBB const* bb = ct->val();
        IRListIter ct2;
        IR * x = BB_irlist(const_cast<IRBB*>(bb)).get_tail(&ct2);
        if (x != NULL && x->isMayThrow() && x->getAI() != NULL) {
            EHLabelAttachInfo const* ehlab =
                (EHLabelAttachInfo const*)x->getAI()->get(AI_EH_LABEL);
            if (ehlab == NULL) { continue; }

            xcom::SC<LabelInfo*> * sc;
            SList<LabelInfo*> const& labs = ehlab->read_labels();
            for (sc = labs.get_head();
                 sc != labs.end(); sc = labs.get_next(sc)) {
                ASSERT0(sc);
                IRBB * tgt = findBBbyLabel(sc->val());
                ASSERT0(tgt);
                xcom::Edge * e = DGraph::addEdge(BB_id(bb), BB_id(tgt));
                EDGE_info(e) = xmalloc(sizeof(CFGEdgeInfo));
                CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e)) = true;
                m_has_eh_edge = true;
            }
        }
    }
}


//Construct EH edge after cfg built.
//This function use a conservative method, and this method
//may generate lots of redundant exception edges.
void IRCFG::buildEHEdgeNaive()
{
    ASSERTN(m_bb_list, ("bb_list is emt"));
    List<IRBB*> maythrow;
    List<IRBB*> ehl;
    IRBB * entry = NULL;
    BBListIter ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        IRBB * bb = ct->val();
        if (isRegionEntry(bb)) {
            entry = bb;
            break;
        }
    }

    ASSERTN(entry, ("Region does not have an entry"));
    xcom::BitSet mainstreambbs;
    computeMainStreamBBSet(entry, mainstreambbs);

    xcom::BitSet ehbbs; //Record all BB in exception region.
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        IRBB * bb = ct->val();

        if (bb->isExceptionHandler()) {
            ehl.append_tail(bb);
            findEHRegion(bb, mainstreambbs, ehbbs);
        }

        if (bb->mayThrowException() && !ehbbs.is_contain(BB_id(bb))) {
            maythrow.append_tail(bb);
        }
    }

    if (ehl.get_elem_count() == 0) { return; }

    if (maythrow.get_elem_count() == 0) {
        ASSERTN(entry, ("multi entries"));
        maythrow.append_tail(entry);
    }

    for (ehl.get_head(&ct); ct != ehl.end(); ct = ehl.get_next(ct)) {
         IRBB * b = ct->val();
         BBListIter ct2;
         for (maythrow.get_head(&ct2);
              ct2 != maythrow.end(); ct2 = maythrow.get_next(ct2)) {
             IRBB * a = ct2->val();
             if (ehbbs.is_contain(BB_id(a))) { continue; }

             xcom::Edge * e = DGraph::addEdge(a->id(), b->id());
             EDGE_info(e) = xmalloc(sizeof(CFGEdgeInfo));
             CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e)) = true;
             m_has_eh_edge = true;
         }
    }
}


//Verification at building SSA mode by ir parser.
bool IRCFG::verifyPhiEdge(IR * phi, xcom::TMap<IR*, LabelInfo*> & ir2label)
{
    xcom::Vertex * bbvex = getVertex(phi->getBB()->id());
    xcom::EdgeC * opnd_pred = VERTEX_in_list(bbvex);
    IR * opnd = PHI_opnd_list(phi);
    for (; opnd != NULL && opnd_pred != NULL;
         opnd = opnd->get_next(), opnd_pred = EC_next(opnd_pred)) {
        LabelInfo * opnd_label = ir2label.get(opnd);
        IRBB * incoming_bb = findBBbyLabel(opnd_label);
        ASSERT0(incoming_bb);
        if (opnd_pred->getFromId() != incoming_bb->id()) {
            return false;
        }
    }
    ASSERT0(opnd == NULL && opnd_pred == NULL);
    return true;
}


//Construct CFG edge for BB has phi.
void IRCFG::revisePhiEdge(xcom::TMap<IR*, LabelInfo*> & ir2label)
{
    ASSERTN(m_bb_list, ("bb_list is emt"));
    BBListIter ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        IRBB const* bb = ct->val();
        if (BB_irlist(bb).get_elem_count() == 0) {
            continue;
        }

        INT phi_opnd_num = -1;
        IRListIter ct2;
        for (BB_irlist(const_cast<IRBB*>(bb)).get_head(&ct2);
             ct2 != NULL; BB_irlist(const_cast<IRBB*>(bb)).get_next(&ct2)) {
            IR * x = ct2->val();
            ASSERT0(x);
            if (!x->is_phi()) { continue; }

            //CFG should have been built before revise Vertex order.
            //for (IR * opnd = PHI_opnd_list(x);
            //     opnd != NULL; opnd = opnd->get_next()) {
            //    LabelInfo * opnd_label = ir2label.get(opnd);
            //    ASSERTN(opnd_label, ("no corresponding label to opnd"));
            //    IRBB * incoming_bb = findBBbyLabel(opnd_label);
            //    ASSERT0(incoming_bb);
            //    addEdge(incoming_bb->id(), bb->id());
            //}

            if (phi_opnd_num == -1) {
                phi_opnd_num = xcom::cnt_list(PHI_opnd_list(x));

                //Sort in-edge of bb to make sure the order of them are same
                //with the phi-operands.
                xcom::Vertex * bbvex = getVertex(bb->id());
                xcom::EdgeC * opnd_pred = VERTEX_in_list(bbvex);
                for (IR * opnd = PHI_opnd_list(x);
                     opnd != NULL; opnd = opnd->get_next()) {
                    LabelInfo * opnd_label = ir2label.get(opnd);
                    IRBB * incoming_bb = findBBbyLabel(opnd_label);
                    ASSERT0(incoming_bb);
                    if (opnd_pred->getFromId() == incoming_bb->id()) {
                        opnd_pred = EC_next(opnd_pred);
                        continue;
                    }

                    xcom::EdgeC * q;
                    for (q = EC_next(opnd_pred);
                         q != NULL; q = EC_next(q)) {
                        if (q->getFromId() == incoming_bb->id()) {
                            break;
                        }
                    }
                    ASSERTN(q, ("can not find needed xcom::EdgeC"));
                    xcom::swap(&VERTEX_in_list(bbvex), opnd_pred, q);
                    opnd_pred = EC_next(q);
                }

                ASSERT0(verifyPhiEdge(x, ir2label));
            } else {
                ASSERTN((UINT)phi_opnd_num ==
                    xcom::cnt_list(PHI_opnd_list(x)),
                    ("the number of operand is inconsistent"));
                ASSERT0((UINT)phi_opnd_num ==
                    getInDegree(getVertex(bb->id())));
                ASSERT0(verifyPhiEdge(x, ir2label));
            }
        }
    }
}


void IRCFG::initEntryAndExit(CFG_SHAPE cs)
{
    //This function may be called multiple times.
    m_bb_vec.clean();
    m_exit_list.clean();
    m_lab2bb.clean();
    if (m_bb_list->get_elem_count() == 0) {
        return;
    }

    //Add BB into graph.
    //ASSERT0(m_bb_vec.get_last_idx() == -1);
    for (IRBB * bb = m_bb_list->get_tail();
         bb != NULL; bb = m_bb_list->get_prev()) {
        m_bb_vec.set(BB_id(bb), bb);
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != NULL; li = bb->getLabelList().get_next()) {
            ASSERTN(m_lab2bb.get(li) == NULL,
                   ("Label has been mapped to BB%d", BB_id(m_lab2bb.get(li))));
            m_lab2bb.set(li, bb);
            if (LABEL_INFO_is_catch_start(li)) {
                BB_is_catch_start(bb) = true;
            }
            if (LABEL_INFO_is_terminate(li)) {
                BB_is_terminate(bb) = true;
            }
        }
    }
    if (m_entry != NULL) {
        //Already have entry BB.
        return;
    }
    switch (cs) {
    case C_SESE: {
        //Make sure the region has the unique entry.
        m_entry = m_rg->allocBB();
        BB_is_entry(m_entry) = true;
        BB_is_fallthrough(m_entry) = true;
        addBB(m_entry);
        m_bb_list->append_head(m_entry);

        //Create logical exit BB.
        //NOTICE: In actually, the logical exit BB is ONLY
        //used to solve diverse dataflow equations, whereas
        //considering the requirement of ENTRY BB, EXIT BB.
        IRBB * exit = m_rg->allocBB();
        BB_is_fallthrough(exit) = true;
        addBB(exit);
        m_bb_list->append_tail(exit);
        m_exit_list.append_tail(exit);
        break;
    }
    case C_SEME: {
        //Create entry BB.
        m_entry = m_rg->allocBB();
        BB_is_entry(m_entry) = true;
        //BB_is_fallthrough(entry) = true;
        addBB(m_entry);
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
    default: ASSERTN(0, ("strang shape of CFG"));
    }
}


//Build CFG according to IRBB list.
void IRCFG::build(OptCtx & oc)
{
    CFG<IRBB, IR>::build(oc);
}


//Note if cfg rebuild, SSAInfo and MDSSAInfo should be recomputed.
void IRCFG::rebuild(OptCtx & oc)
{
    ASSERT0(m_cs != C_UNDEF);
    initEntryAndExit(m_cs);
    CFG<IRBB, IR>::rebuild(oc);
    buildEHEdge();

    //After CFG building.
    //Remove empty BB after cfg rebuilding because
    //rebuilding cfg may generate empty BB, it
    //disturb the computation of entry and exit.
    removeEmptyBB(oc);

    //Compute exit BB while CFG rebuilt.
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


//Do early control flow optimization.
void IRCFG::initCfg(OptCtx & oc)
{
    if (getBBList()->get_elem_count() == 0) {
        //If bb is empty, set CFG is invalid.
        //OC_is_cfg_valid(oc) = true;
        return;
    }

    //cfg->removeEmptyBB();
    build(oc);
    buildEHEdge();
    if (g_is_dump_after_pass && g_dump_opt.isDumpCFG()) {
        dumpDOT(g_tfile, true, true);
    }

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


void IRCFG::findTargetBBOfMulticondBranch(IR const* ir,
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
void IRCFG::findEHRegion(
        IRBB const* catch_start,
        xcom::BitSet const& mainstreambbs,
        OUT xcom::BitSet & ehbbs)
{
    ASSERT0(catch_start && catch_start->isExceptionHandler());
    List<xcom::Vertex const*> list;
    xcom::Vertex const* bbv = getVertex(BB_id(catch_start));
    ASSERT0(bbv);
    list.append_head(bbv);
    for (xcom::Vertex const* v = list.remove_head();
         v != NULL; v = list.remove_head()) {
        UINT id = v->id();
        if (mainstreambbs.is_contain(id) || ehbbs.is_contain(id)) {
            continue;
        }

        ehbbs.bunion(id);

        xcom::EdgeC * el = VERTEX_out_list(v);
        while (el != NULL) {
            xcom::Vertex const* succ = el->getTo();
            if (!mainstreambbs.is_contain(succ->id()) &&
                !ehbbs.is_contain(succ->id())) {
                list.append_tail(succ);
            }
            el = EC_next(el);
        }
    }
}


//This function find all BB of exception try region, and record them in trybbs.
//trybbs: record all BB of the try region.
//Note: this function does not clean trybbs. Caller is responsible for that.
void IRCFG::findAllTryRegions(OUT xcom::BitSet & trybbs)
{
    BBListIter ct;
    xcom::BitSet t;
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
void IRCFG::findTryRegion(IRBB const* try_start, OUT xcom::BitSet & trybbs)
{
    ASSERT0(try_start && try_start->isTryStart());
    List<xcom::Vertex const*> list;
    xcom::Vertex const* bbv = getVertex(BB_id(try_start));
    ASSERT0(bbv);
    list.append_head(bbv);
    for (xcom::Vertex const* v = list.remove_head();
         v != NULL; v = list.remove_head()) {
        UINT id = v->id();
        if (trybbs.is_contain(id)) { continue; }

        trybbs.bunion(id);

        IRBB * bb = getBB(id);
        ASSERTN(bb, ("vertex on CFG correspond to nothing"));

        if (bb->isTryEnd() && bb != try_start) {
            //BB may have both try_start and try_end label.
            //If it is the case, the try_end is always other region's
            //end label, just ignore that.
            continue;
        }

        for (xcom::EdgeC * el = VERTEX_out_list(v); el != NULL; el = EC_next(el)) {
            xcom::Edge const* e = EC_edge(el);
            CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
            if (ei != NULL && CFGEI_is_eh(ei)) {
                //Do not consider EH edge.
                continue;
            }

            xcom::Vertex const* succ = EDGE_to(e);
            if (!trybbs.is_contain(succ->id())) {
                list.append_tail(succ);
            }

        }
    }
}


//Find a list bb that referred labels which is the target of ir.
void IRCFG::findTargetBBOfIndirectBranch(
        IR const* ir,
        OUT List<IRBB*> & tgtlst)
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
void IRCFG::LoopAnalysis(OptCtx & oc)
{
    if (getBBList()->get_elem_count() == 0) {
        //If bb is empty, set LoopInfo to be invalid.
        //OC_is_loopinfo_valid(oc) = true;
        return;
    }
    m_rg->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    findLoop();
    collectLoopInfo();
    OC_is_loopinfo_valid(oc) = true;
}


//Find bb that 'lab' attouchemented.
IRBB * IRCFG::findBBbyLabel(LabelInfo const* lab)
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


IRBB * IRCFG::insertBBbetween(IN IRBB * from,
                              IN BBListIter from_ct,
                              IN IRBB * to,
                              IN BBListIter to_ct,
                              IN IRBB * newbb)
{
    //Revise BB list, note that 'from' is either fall-through to 'to',
    //or jumping to 'to'.
    BBList * bblst = getBBList();

    //First, processing edge if 'from'->'to' is fallthrough.
    BBListIter tmp_ct = from_ct;
    if (BB_is_fallthrough(from) && bblst->get_next(&tmp_ct) == to) {
        bblst->insert_after(newbb, from_ct);
        addBB(newbb);
        insertVertexBetween(BB_id(from), BB_id(to), BB_id(newbb));
        BB_is_fallthrough(newbb) = true;
        return NULL;
    }

    //Second, from->to is jump-edge.
    List<IRBB*> preds;
    get_preds(preds, to);
    ASSERTN(preds.find(from), ("'from' is not pred of 'to'"));
    BBListIter pred_ct = NULL;
    IRBB * inserted_tramp_bb = NULL;
    for (IRBB * pred = preds.get_head(&pred_ct);
         pred != NULL; pred = preds.get_next(&pred_ct)) {
        BBListIter tmp_ct2 = to_ct;
        if (BB_is_fallthrough(pred) && bblst->get_prev(&tmp_ct2) == pred) {
            //Given 'to' has a fallthrough in-edge. Insert a tmp BB
            //e.g:
            //    from->bb1->bb2->to, all edges are fallthrough
            //    from->to, jump-edge
            //    bb1->to, jump-edge
            //
            //Here we need to revise the fallthrough-edge 'bb2->to',
            //the result is from->bb1->bb2->inserted_tramp_bb, all
            //edges are fallthrough inserted_tramp_bb->to, jump-edge
            //    from->to, jump-edge
            //    bb1->to, jump-edge
            //
            //    bb2->inserted_tramp_bb, tmp_tramp_bb->to, both are jump-edge.
            //    ir-list of inserted_tramp_bb is:
            //        goto L1:
            //
            //    ir-list of 'to' is:
            //        L1:
            //        ...
            //        ...
            inserted_tramp_bb = m_rg->allocBB();
            LabelInfo * li = m_rg->genIlabel();
            IR * goto_ir = m_rg->buildGoto(li);
            BB_irlist(inserted_tramp_bb).append_tail(goto_ir);
            to->addLabel(li);
            m_lab2bb.set(li, to);
            addBB(inserted_tramp_bb);
            bblst->insert_after(inserted_tramp_bb, pred);

            insertVertexBetween(BB_id(pred),
                BB_id(to), BB_id(inserted_tramp_bb));

            //Fall through edge has been broken, insert 'newbb' before 'to'.
            break;
        }
    }

    //Revise the target LABEL of last XR in 'from'.
    IR * last_xr_of_from = get_last_xr(from);

    ASSERT0(last_xr_of_from->getLabel() &&
            findBBbyLabel(last_xr_of_from->getLabel()) == to);
    ASSERT0(last_xr_of_from->getLabel() != NULL);

    LabelInfo * li = m_rg->genIlabel();
    last_xr_of_from->setLabel(li);

    newbb->addLabel(li);
    m_lab2bb.set(li, newbb);

    //When we get here, there are NOT any fallthrough in-edges of 'to' exist.
    bblst->insert_before(newbb, to_ct);
    insertVertexBetween(BB_id(from), BB_id(to), BB_id(newbb));
    BB_is_fallthrough(newbb) = true;
    return inserted_tramp_bb;
}


//Migrate Lables from src BB to tgt BB.
void IRCFG::moveLabels(IRBB * src, IRBB * tgt)
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
void IRCFG::resetMapBetweenLabelAndBB(IRBB * bb)
{
    for (LabelInfo const* li = bb->getLabelList().get_head();
         li != NULL; li = bb->getLabelList().get_next()) {
        m_lab2bb.setAlways(li, NULL);
    }
    bb->cleanLabelInfoList();
}


//Combine trampoline branch.
//e.g:L2:
//    truebr L4 | false L4
//    goto L3
//    L4
//    ...
//    L3:
//    ...
//=>
//    L2:
//    falsebr L3 | truebr L3
//    EMPTY BB
//    L4:
//    ...
//    L3:
bool IRCFG::inverseAndRemoveTrampolineBranch()
{
    bool changed = false;
    BBListIter ct;
    List<IRBB*> succs;
    List<IRBB*> preds;
    for (IRBB * bb = m_bb_list->get_head(&ct);
         bb != NULL; bb = m_bb_list->get_next(&ct)) {
        if (bb->isExceptionHandler()) { continue; }

        IR * br = get_last_xr(bb);
        if (br == NULL || !br->isConditionalBr()) {
            continue;
        }
        if (br->hasSideEffect()) {
            continue;
        }

        BBListIter nextbbct = ct;
        IRBB * next = m_bb_list->get_next(&nextbbct);
        IR * jmp = NULL;
        if (next == NULL || //bb may be the last BB in bb-list.
            (jmp = get_first_xr(next)) == NULL || //bb can not be empty
            !jmp->is_goto()) { //the only IR must be GOTO
            continue;
        }

        if (next->isExceptionHandler()) { continue; }

        IRBB * next_next = m_bb_list->get_next(&nextbbct);
        if (next_next == NULL || //bb may be the last BB in bb-list.
            !next_next->isContainLabel(BR_lab(br))) {
            continue;
        }

        IRBB * jmp_tgt = findBBbyLabel(GOTO_lab(jmp));
        xcom::Edge const* e_of_jmp = getEdge(next->id(), jmp_tgt->id());
        ASSERT0(e_of_jmp);
        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e_of_jmp);
        if (ei != NULL && CFGEI_is_eh(ei)) {
            //Do not remove exception edge.
            continue;
        }

        xcom::Edge const* e_of_bb = getEdge(bb->id(), next_next->id());
        ASSERT0(e_of_bb);
        CFGEdgeInfo * ei2 = (CFGEdgeInfo*)EDGE_info(e_of_bb);
        if (ei2 != NULL && CFGEI_is_eh(ei2)) {
            //Do not remove exception edge.
            continue;
        }

        //Do replacement
        if (br->is_truebr()) {
            IR_code(br) = IR_FALSEBR;
        } else {
            ASSERT0(br->is_falsebr());
            IR_code(br) = IR_TRUEBR;
        }
        br->setLabel(GOTO_lab(jmp));

        //Change 'next' to be empty BB.
        remove_xr(next, jmp);

        //Remove jmp->jmp_tgt.
        removeEdge(next, jmp_tgt);

        //Add jmp->next_next.
        xcom::Edge * newe = addEdge(next, next_next);
        EDGE_info(newe) = ei;

        //Remove bb->next_next.
        removeEdge(bb, next_next);

        //Add bb->jmp_tgt.
        xcom::Edge * newe2 = addEdge(bb, jmp_tgt);
        EDGE_info(newe2) = ei2;

        changed = true;
    }
    return changed;
}


void IRCFG::remove_bb_impl(IRBB * bb)
{        
    ASSERT0(bb);    
    m_bb_vec.set(BB_id(bb), NULL);

    //C<LabelInfo const*> * ct;
    //for (lablst.get_head(&ct);
    //     ct != lablst.end(); ct = lablst.get_next(ct)) {
    //    m_lab2bb.remove(ct->val());
    //}

    removeVertex(BB_id(bb));
}


//CASE: Given pred1->bb, fallthrough edge, and pred2->bb, jumping edge.
//  pred2:
//    goto bb;
//  ...
//  pred1:
//    a=1;
//  bb:
//    goto next;
//  next:
//    ...
//    ...
//Remove bb and revise CFG.
//ct: container in m_bb_list of CFG. It will be updated if related BB removed.
bool IRCFG::removeTrampolinBBCase1(BBListIter * ct)
{
    bool removed = false;
    List<IRBB*> preds;
    IRBB * bb = (*ct)->val();
    ASSERT0(getSuccsNum(bb) == 1);
    IRBB const* succ = get_first_succ(bb);
    ASSERT0(succ);
    IR const* uncond_br = get_first_xr(const_cast<IRBB*>(bb));
    ASSERT0(uncond_br && uncond_br->isUnconditionalBr());
    BBListIter tmp_bb_ct = *ct;
    IRBB * next = m_bb_list->get_next(&tmp_bb_ct);
    if (next == NULL || //bb may be the last BB in bb-list.
        next != succ) { //next BB is not the successor.
        return removed;
    }

    tmp_bb_ct = *ct;
    IRBB * prev = m_bb_list->get_prev(&tmp_bb_ct);
    preds.clean(); //use list because cfg may be modify.
    get_preds(preds, bb);
    for (IRBB * pred = preds.get_head();
         pred != NULL; pred = preds.get_next()) {
        moveLabels(bb, next);

        if (BB_is_fallthrough(pred) && prev == pred) {
            removeEdge(pred, bb);

            //Add normal control flow edge.
            xcom::Edge * e = addEdge(pred, next);
            CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
            if (ei != NULL && CFGEI_is_eh(ei)) {
                //If there is already an edge, check if it is an
                //exception edge. If it is, change the exception edge
                //to be normal control flow edge.
                CFGEI_is_eh(ei) = false;
            }
            return removed;
        }

        //CASE:
        // pred:
        //   goto bb;
        //   ...
        // bb:
        //   goto next;
        // next:
        //   ...
        //Remove bb and revise CFG.

        //Revise branch target LabelInfo of xr in 'pred'.
        IR * last_xr_of_pred = get_last_xr(pred);
        if (last_xr_of_pred != NULL) {
            ASSERT0(last_xr_of_pred->getLabel());
            ASSERTN(findBBbyLabel(last_xr_of_pred->getLabel()) == next,
                    ("Labels of bb should have already moved to "
                     "next BB by moveLabels()"));
            last_xr_of_pred->setLabel(uncond_br->getLabel());
        }
        removeEdge(pred, bb);

        xcom::Edge * e = addEdge(pred, next);
        //TODO: Add operands of PHI if 'next_bb' has PHI.

        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei != NULL && CFGEI_is_eh(ei)) {
            //If there is already an edge, check if it is an
            //exception edge. If it is, change the exception edge
            //to be normal control flow edge.
            CFGEI_is_eh(ei) = false;
        }
    } //end for each pred of BB.
    
    //The map between Labels and BB has been maintained.
    //resetMapBetweenLabelAndBB(bb);
    remove_bb(bb);
    removed = true;

    //Update ct to reprocess BB list from beginning.
    m_bb_list->get_head(ct);
    return removed;
}


//Remove trampoline BB.
//e.g: bb1->bb2->bb3
//    stmt of bb2 is just 'goto bb3', and bb3 is the NEXT BB
//    of bb2 in BBList.
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
bool IRCFG::removeTrampolinBB()
{
    bool removed = false;
    BBListIter ct;
    List<IRBB*> succs;
    List<IRBB*> preds;
    for (m_bb_list->get_head(&ct); ct != NULL; ct = m_bb_list->get_next(ct)) {
        IRBB const* bb = ct->val();
        if (bb->isExceptionHandler()) { continue; }

        IR const* uncond_br = get_first_xr(const_cast<IRBB*>(bb));
        if (uncond_br == NULL ||
            !uncond_br->isUnconditionalBr() ||
            bb->getNumOfIR() != 1) {
            continue;
        }

        //CASE: Given pred1->bb, fallthrough edge,
        //  and pred2->bb, jumping edge.
        //  bb:
        //      goto L1
        //  next of bb:
        //      L1:
        //      ...
        //      ...
        //Remove bb and revise CFG.
        removed |= removeTrampolinBBCase1(&ct);
    }
    return removed;
}


bool IRCFG::removeTrampolinEdgeForCase2(BBListIter ct)
{
    ASSERT0(ct);
    IRBB * bb = ct->val();
    bool removed = false;
    List<IRBB*> preds; //record preds in list because CFG may be modified.
    get_preds(preds, bb);

    IRBB * succ = get_first_succ(bb);
    ASSERT0(succ);

    IR * last_xr = get_last_xr(bb);
    ASSERT0(last_xr);

    LabelInfo const* tgt_li = last_xr->getLabel();
    ASSERT0(tgt_li != NULL);
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
        if (!pred->isDownBoundary(last_xr_of_pred)) {
            //CASE: pred->bb, pred is fallthrough-BB.
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
            BB_irlist(pred).append_tail(m_rg->dupIRTree(last_xr));
            BB_is_fallthrough(pred) = false;
            removeEdge(pred, bb);

            addEdge(pred, succ);
            //bb->dupSuccessorPhiOpnd(this, m_rg, WhichPred(bb, succ));
            
            removed = true;
            continue;
        }

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
            ASSERT0(last_xr_of_pred->getLabel() &&
                    findBBbyLabel(last_xr_of_pred->getLabel()) == bb);
            ASSERT0(last_xr_of_pred->getLabel() != NULL);
            if (BB_id(succ) == BB_id(bb)) {
                //CASE: pred->bb, bb's target is itself.
                //    pred is:
                //        goto L1;
                //    ...
                //    bb is:
                //        L1:
                //        goto L1;
                //Do nothing for this case.
                continue;
            }

            GOTO_lab(last_xr_of_pred) = tgt_li;
            removeEdge(pred, bb);
            
            addEdge(pred, succ);

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
            BBListIter prev_of_bb = ct;
            if (m_bb_list->get_prev(&prev_of_bb) == pred) {
                //Can not remove jumping-edge if 'bb' is
                //fall-through successor of 'pred'.
                continue;
            }

            ASSERT0(last_xr_of_pred->getLabel() &&
                    findBBbyLabel(last_xr_of_pred->getLabel()) == bb);

            ASSERT0(last_xr_of_pred->getLabel() != NULL);
            if (bb != succ) {
                //bb should not be the same one with succ.
                BR_lab(last_xr_of_pred) = tgt_li;

                //Link up bb's pred and succ.
                removeEdge(pred, bb);
                addEdge(pred, succ);
                removed = true;
            }
            continue;
        } //end if
    } //end for each pred
    return removed;
}


//Remove trampoline edge.
//e.g: bb1->bb2->bb3
//stmt of bb2 is just 'goto bb3', then bb1->bb2 is tramp edge.
//And the resulting edges are bb1->bb3, bb2->bb3 respectively.
//Return true if at least one tramp edge removed.
bool IRCFG::removeTrampolinEdge()
{
    bool removed = false;
    BBListIter ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        IRBB * bb = ct->val();
        if (bb->getNumOfIR() != 1) {
            //BB is almost not a trampoline BB if there are multiples IRs.
            continue;
        }

        IR * last_xr = get_last_xr(bb);
        ASSERT0(last_xr);
        if (last_xr->hasSideEffect() ||
            !last_xr->is_goto() ||
            bb->isAttachDedicatedLabel()) {
            //BB has sideeffect and should not be processed.
            continue;
        }

        LabelInfo const* tgt_li = last_xr->getLabel();
        ASSERT0(tgt_li != NULL);
        BBListIter next_ct = m_bb_list->get_next(ct);
        if (next_ct != NULL) {
            //BB is not the last one.
            IRBB * target = findBBbyLabel(tgt_li);
            if (target == next_ct->val()) {
                //CASE1:Remove the redundant GOTO.
                //  e.g: region func main () {
                //    truebr (eq $1, $2), L2;
                //    goto L1; //goto is actually fallthrough to label L1.
                //             //So it can be removed.
                //    label L1;
                //    goto L1;
                //    label L2;
                //  };
                ASSERT0(bb->getNumOfIR() == 1);
                BB_irlist(bb).remove_tail();
                m_rg->freeIRTree(last_xr);
                removed = true;
                continue;
            }
        }

        removed |= removeTrampolinEdgeForCase2(ct);
    } //end for each BB
    return removed;
}


bool IRCFG::removeRedundantBranch()
{
    bool removed = CFG<IRBB, IR>::removeRedundantBranch();
    BBListIter ct;
    DUMgr * dumgr = m_rg->getDUMgr(); //Query DUMgr in real time.
    List<IRBB*> succs;
    for (IRBB * bb = m_bb_list->get_head(&ct);
         bb != NULL; bb = m_bb_list->get_next(&ct)) {
        IR * last_xr = get_last_xr(bb);
        if (last_xr == NULL ||
            !last_xr->isConditionalBr() ||
            last_xr->hasSideEffect()) {
            continue;
        }

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
            LabelInfo const* tgt_li = last_xr->getLabel();
            ASSERT0(tgt_li != NULL);

            BB_irlist(bb).remove_tail();

            if (dumgr != NULL) {
                dumgr->removeIROutFromDUMgr(last_xr);
            }

            m_rg->freeIRTree(last_xr);

            IR * uncond_br = m_rg->buildGoto(tgt_li);
            BB_irlist(bb).append_tail(uncond_br);

            //Remove fallthrough edge, leave branch edge.
            get_succs(succs, bb);
            BBListIter tmp_ct = ct;
            m_bb_list->get_next(&tmp_ct);
            for (IRBB * s = succs.get_head();
                 s != NULL; s = succs.get_next()) {
                if (s == tmp_ct->val()) {
                    //Remove branch edge, leave fallthrough edge.
                    removeEdge(bb, s);                    
                }
            }
            removed = true;
            continue;
        }

        if ((last_xr->is_truebr() && always_false) ||
            (last_xr->is_falsebr() && always_true)) {
            IR * r = BB_irlist(bb).remove_tail();
            if (dumgr != NULL) {
                dumgr->removeIROutFromDUMgr(r);
            }
            m_rg->freeIRTree(r);

            //Remove branch edge, leave fallthrough edge.
            get_succs(succs, bb);
            BBListIter tmp_ct = ct;
            m_bb_list->get_next(&tmp_ct);
            for (IRBB * s = succs.get_head();
                 s != NULL; s = succs.get_next()) {
                if (s != tmp_ct->val()) {
                    removeEdge(bb, s);
                }
            }
            removed = true;
        }
    } //for each BB
    return removed;
}


void IRCFG::dumpDOT(CHAR const* name, bool detail, bool dump_eh)
{
    //Do not dump if default dump file is not initialized.
    if (g_tfile == NULL) { return; }

    //Note this function does not use g_tfile as output.
    //So it is dispensable to check g_tfile.
    if (name == NULL) {
        name = "graph_cfg.dot";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    dumpDOT(h, detail, dump_eh);
    fclose(h);
}


void IRCFG::dumpDOT(FILE * h, bool detail, bool dump_eh)
{
    if (g_tfile == NULL || h == NULL) { return; }
    FILE * org_tfile = g_tfile;
    g_tfile = h;
    if (detail) {
        //Print comment
        fprintf(h, "\n/*");
        dumpBBList(m_bb_list, m_rg);
        fprintf(h, "\n*/\n");
    }
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
               m_rg->getRegionName());

    MDSSAMgr const* mdssamgr = (MDSSAMgr const*)m_rg->getPassMgr()->queryPass(
        PASS_MD_SSA_MGR);    

    //Print node
    INT c;
    for (xcom::Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        CHAR const* shape = "box";
        CHAR const* font = "courB";
        CHAR const* color = "black";
        CHAR const* style = "bold";
        UINT fontsize = 12;
        IRBB * bb = getBB(v->id());
        ASSERT0(bb);        
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
            StrBuf namebuf(32);
            fprintf(h, "\nnode%d [font=\"%s\",fontsize=%d,color=%s,"
                       "shape=%s,style=%s,label=\"BB%d ",
                    v->id(), font, fontsize, color, shape, style, v->id());

            dumpBBLabel(bb->getLabelList(), h);
            fprintf(h, " ");

            if (VERTEX_rpo(v) != 0) {
                fprintf(h, "rpo:%d ", VERTEX_rpo(v));
            }

            //Dump MDSSA Phi List.
            if (mdssamgr != NULL && mdssamgr->isMDSSAConstructed()) {
                MDPhiList const* philist = mdssamgr->getPhiList(bb);
                if (philist != NULL) {
                    FILE * org = g_tfile;
                    g_tfile = h;
                    mdssamgr->dumpPhiList(philist);
                    g_tfile = org;
                }
            }
            
            //Dump IR list.
            for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
                //The first \l is very important to display
                //DOT in a fine manner.
                fprintf(h, "\\l");

                //TODO: implement dump_ir_buf();
                dumpIR(ir, m_rg, NULL, IR_DUMP_KID);
            }

            //The last \l is very important to display DOT in a fine manner.
            fprintf(h, "\\l");

            //The end char of properties.
            fprintf(h, "\"];");
            fflush(h);
            continue;
        }

        //Dump node without detail.
        fprintf(h, "\nnode%d [font=\"%s\",fontsize=%d,color=%s,"
                   "shape=%s,style=%s,label=\"BB%d\"];",
                v->id(), font, fontsize, color, shape, style, v->id());
        fflush(h);
    }

    //Print edge
    INT c2;
    TTab<xcom::Edge const*> visited;
    for (xcom::Vertex * v = m_vertices.get_first(c2);
         v != NULL; v = m_vertices.get_next(c2)) {
        //Print in-edge list.
        UINT pos = 0;
        for (xcom::EdgeC const* ec = v->getInList();
             ec != NULL; ec = ec->get_next(), pos++) {
            if (visited.find(ec->getEdge())) { continue; }
            visited.append(ec->getEdge());
            xcom::Edge const* e = ec->getEdge();

            CFGEdgeInfo * ei = (CFGEdgeInfo*)e->info();
            if (ei == NULL) {
                fprintf(h, "\nnode%d->node%d[style=bold, "
                           "color=maroon, label=\"p%d %s\"]",
                           e->from()->id(), e->to()->id(), pos, "");
                continue;
            }

            if (CFGEI_is_eh(ei)) {
                if (dump_eh) {
                    fprintf(h, "\nnode%d->node%d[style=dotted, "
                               "color=darkslategray, label=\"p%d %s\"]",
                               e->from()->id(), e->to()->id(), pos, "");                        
                }
                continue;
            }

            ASSERTN(0, ("unsupported CFGEdgeInfo"));
        }
    }

    //Print rest of edges.
    for (xcom::Edge const* e = m_edges.get_first(c);
         e != NULL;  e = m_edges.get_next(c)) {
        if (visited.find(e)) { continue; }

        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei == NULL) {
            fprintf(h,
                    "\nnode%d->node%d[style=bold, color=maroon, label=\"%s\"]",
                    e->from()->id(), e->to()->id(), "");
            continue;
        }

        if (CFGEI_is_eh(ei)) {
            if (dump_eh) {
                fprintf(h, "\nnode%d->node%d[style=dotted, "
                           "color=darkslategray, label=\"%s\"]",
                           e->from()->id(), e->to()->id(), "");                        
            }
            continue;
        }
        ASSERTN(0, ("unsupport CFGEdgeInfo"));
    }

    g_tfile = org_tfile;
    g_prt_carriage_return_for_dot = org_prt_cr;
    fprintf(h,"\n}\n");
    fflush(h);
}


void IRCFG::dump_node(FILE * h, bool detail)
{
    ASSERT0(h);
    ASSERT0(m_bb_list);
    MDSSAMgr const* mdssamgr = (MDSSAMgr const*)m_rg->getPassMgr()->queryPass(
        PASS_MD_SSA_MGR);    
    UINT vertical_order = 1;
    INT c;
    for (xcom::Vertex const* v = get_first_vertex(c);
        v != NULL; v = get_next_vertex(c)) {
        IRBB * bb = getBB(v->id());
        ASSERTN(bb, ("Not find BB%d", v->id()));

        CHAR const* shape = "box";
        CHAR const* font = "courB";
        CHAR const* color = "gold";
        INT scale = 1;

        if (BB_is_catch_start(bb)) {
            shape = "uptrapezoid";
        }
        if (BB_is_entry(bb) || BB_is_exit(bb)) {
            font = "Times Bold";
            scale = 2;
            color = "cyan";
        }

        if (detail) {
            fprintf(h,
                "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
                "fontname:\"%s\" scaling:%d label:\"",
                v->id(), vertical_order++, shape, color, font, scale);
            fprintf(h, "   BB%d ", v->id());
            
            if (VERTEX_rpo(v) != 0) {
                fprintf(h, " rpo:%d ", VERTEX_rpo(v));
            }
            dumpBBLabel(bb->getLabelList(), h);
            fprintf(h, "\n");

            //Dump MDSSA Phi List.
            if (mdssamgr != NULL && mdssamgr->isMDSSAConstructed()) {
                MDPhiList const* philist = mdssamgr->getPhiList(bb);
                if (philist != NULL) {
                    FILE * org = g_tfile;
                    g_tfile = h;
                    mdssamgr->dumpPhiList(philist);
                    g_tfile = org;
                }
            }

            //Dump IR list.
            for (IR * ir = BB_first_ir(bb);
                 ir != NULL; ir = BB_next_ir(bb)) {
                //fprintf(h, "%s\n", dump_ir_buf(ir, buf));

                //TODO: implement dump_ir_buf();
                dumpIR(ir, m_rg, NULL, IR_DUMP_KID);
            }

            fprintf(h, "\"}");
            fflush(h);
            continue;
        }

        //Dump node without detail.
        fprintf(h,
                "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
                "fontname:\"%s\" scaling:%d label:\"%d",
                v->id(), vertical_order++, shape, color, font,
                scale, v->id());
        if (VERTEX_rpo(v) != 0) {
            fprintf(h, " rpo:%d", VERTEX_rpo(v));
        }
        fprintf(h, "\" }");
    }
}


//Print graph structure description.
void IRCFG::dump_head(FILE * h)
{
    ASSERT0(h);
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


void IRCFG::dump_edge(FILE * h, bool dump_eh)
{
    ASSERT0(h);
    INT c2;
    TTab<xcom::Edge const*> visited;
    UINT pos = 0;
    for (xcom::Vertex * v = m_vertices.get_first(c2);
         v != NULL; v = m_vertices.get_next(c2), pos++) {
        //Print in-edge list.
        UINT pos = 0;
        for (xcom::EdgeC const* ec = v->getInList();
             ec != NULL; ec = ec->get_next(), pos++) {
            if (visited.find(ec->getEdge())) { continue; }
            visited.append(ec->getEdge());
            xcom::Edge const* e = ec->getEdge();

            CFGEdgeInfo * ei = (CFGEdgeInfo*)e->info();
            if (ei == NULL) {
                fprintf(h,
                        "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                        " thickness:4 color:darkred label:\"p%d\" }",
                        e->from()->id(), e->to()->id(), pos);
                continue;
            }
            
            if (CFGEI_is_eh(ei)) {
                if (dump_eh) {
                    fprintf(h,
                            "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                            "linestyle:dotted color:lightgrey }",
                            e->from()->id(), e->to()->id());
                }
                continue;
            }

            fprintf(h,
                    "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                    " thickness:4 color:darkred }",
                    e->from()->id(), e->to()->id());
            ASSERTN(0, ("unsupported CFGEdgeInfo"));
        }
    }

    //Print rest of edges.
    INT c;
    for (xcom::Edge * e = m_edges.get_first(c);
         e != NULL; e = m_edges.get_next(c)) {
        if (visited.find(e)) { continue; }

        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei == NULL) {
            fprintf(h,
                    "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                    " thickness:4 color:darkred }",
                    e->from()->id(), e->to()->id());
            continue;
        }

        if (CFGEI_is_eh(ei)) {
            if (dump_eh) {
                fprintf(h,
                        "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                        "linestyle:dotted color:lightgrey }",
                        e->from()->id(), e->to()->id());
            }
            continue;
        }

        fprintf(h,
                "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                " thickness:4 color:darkred }",
                e->from()->id(), e->to()->id());
        ASSERTN(0, ("unsupported CFGEdgeInfo"));
    }
}


void IRCFG::dumpVCG(CHAR const* name, bool detail, bool dump_eh)
{
    if (g_tfile == NULL) { return; }
    ASSERT0(m_rg);

    if (name == NULL) { name = "graph_cfg.vcg"; }

    //Note this function does not use g_tfile as output.
    //So it is dispensable to check g_tfile.
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h != NULL, ("%s create failed!!!",name));
    FILE * old = NULL;

    //Print comment
    //fprintf(h, "\n/*");
    //old = g_tfile;
    //g_tfile = h;
    //dumpBBList(m_bb_list, m_rg);
    //g_tfile = old;
    //fprintf(h, "\n*/\n");
    dump_head(h);

    //Print Region name.
    fprintf(h,
            "\nnode: {title:\"\" vertical_order:0 shape:box color:turquoise "
            "borderwidth:0 fontname:\"Courier Bold\" "
            "scaling:2 label:\"RegionName:%s\" }", m_rg->getRegionName());
    old = g_tfile;
    g_tfile = h;
    dump_node(h, detail);
    dump_edge(h, dump_eh);

    g_tfile = old;
    fprintf(h, "\n}\n");
    fclose(h);
}


void IRCFG::computeDomAndIdom(IN OUT OptCtx & oc, xcom::BitSet const* uni)
{
    if (getBBList()->get_elem_count() == 0) { return; }

    DUMMYUSE(uni);
    START_TIMER(t, "Compute Dom, IDom");
    ASSERT0(OC_is_cfg_valid(oc));
    ASSERTN(m_entry, ("ONLY support SESE or SEME"));

    m_rg->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    List<IRBB*> * bblst = getBBListInRPO();
    ASSERT0(bblst->get_elem_count() == m_rg->getBBList()->get_elem_count());

    List<xcom::Vertex const*> vlst;
    for (IRBB * bb = bblst->get_head(); bb != NULL; bb = bblst->get_next()) {
        ASSERT0(BB_id(bb) != 0);
        vlst.append_tail(getVertex(BB_id(bb)));
    }

    //xcom::DGraph::computeDom(&vlst, uni);
    //xcom::DGraph::computeIdom();

    bool f = xcom::DGraph::computeIdom2(vlst);
    DUMMYUSE(f);
    ASSERT0(f);

    f = xcom::DGraph::computeDom2(vlst);
    DUMMYUSE(f);
    ASSERT0(f);

    OC_is_dom_valid(oc) = true;
    END_TIMER(t, "Compute Dom, IDom");
    if (g_is_dump_after_pass && g_dump_opt.isDumpDOM()) {
        dump_dom(g_tfile, false);
    }
}


void IRCFG::computePdomAndIpdom(IN OUT OptCtx & oc, xcom::BitSet const* uni)
{
    if (getBBList()->get_elem_count() == 0) { return; }

    START_TIMER(t, "Compute PDom,IPDom");
    ASSERT0(OC_is_cfg_valid(oc));

    m_rg->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    List<IRBB*> * bblst = getBBListInRPO();
    ASSERT0(bblst->get_elem_count() == m_rg->getBBList()->get_elem_count());

    List<xcom::Vertex const*> vlst;
    for (IRBB * bb = bblst->get_tail(); bb != NULL; bb = bblst->get_prev()) {
        ASSERT0(BB_id(bb) != 0 && getVertex(BB_id(bb)));
        vlst.append_tail(getVertex(BB_id(bb)));
    }

    bool f = false;
    if (uni != NULL) {
        f = xcom::DGraph::computePdom(&vlst, uni);
    } else {
        f = xcom::DGraph::computePdom(&vlst);
    }
    DUMMYUSE(f);
    ASSERT0(f);

    f = xcom::DGraph::computeIpdom();
    ASSERT0(f);

    OC_is_pdom_valid(oc) = true;
    END_TIMER(t, "Compute PDom,IPDom");
}


void IRCFG::remove_xr(IRBB * bb, IR * ir)
{
    DUMgr * dumgr = m_rg->getDUMgr();
    if (dumgr != NULL) {
        //Revise DU chains.
        //TODO: If ssa form is available, it doesn't need to maintain
        //DU chain of PR in DU manager counterpart.
        dumgr->removeIROutFromDUMgr(ir);
    }

    //Revise PRSSA info if PR is in SSA form.
    ir->removeSSAUse();

    MDSSAMgr * mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->queryPass(
        PASS_MD_SSA_MGR);
    if (mdssamgr != NULL) {
        //Revise MDSSA info if exist.
        mdssamgr->removeMDSSAUse(ir);
    }

    ir = BB_irlist(bb).remove(ir);
    m_rg->freeIRTree(ir);
}


//Perform miscellaneous control flow optimizations.
//Include removing dead bb which is unreachable, removing empty bb as many
//as possible, simplify and remove the branch like "if (x==x)", removing
//the trampolin branch.
bool IRCFG::performMiscOpt(OptCtx & oc)
{
    START_TIMER(t, "CFG Optimizations");
    bool change = false;
    bool lchange;
    UINT count = 0;
    OptCtx org_oc(oc);

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

        if (g_do_cfg_invert_condition_and_remove_trampolin_bb) {
            lchange |= inverseAndRemoveTrampolineBranch();
        }

        if (lchange) {
            oc.set_flag_if_cfg_changed();

            //Each pass maintain CFG by default.
            OC_is_cfg_valid(oc) = true;
        }
        change |= lchange;
        count++;
    } while (lchange && count < 1000);
    ASSERTN(!lchange, ("CFG optimization iterated too many times."));

    if (change) {
        computeExitList();

        if (OC_is_cdg_valid(org_oc)) {
            m_rg->checkValidAndRecompute(&oc, PASS_CDG, PASS_UNDEF);
            ASSERT0(verifyIfBBRemoved((CDG*)m_rg->getPassMgr()->queryPass(
                    PASS_CDG), oc));
        }

        //SSAInfo is invalid by adding new-edge to BB.
        //This will confuse phi insertion.
        ASSERT0(verifyPRSSAInfo(m_rg));
        ASSERT0(verifyMDSSAInfo(m_rg));

    }

    ASSERT0(verifyIRandBB(getBBList(), m_rg));
    ASSERT0(m_rg->verifyRPO(oc));

    END_TIMER(t, "CFG Optimizations");
    return change;
}

} //namespace xoc
