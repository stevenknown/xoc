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
        for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
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
        if (x != nullptr && x->isMayThrow() && x->getAI() != nullptr) {
            EHLabelAttachInfo const* ehlab =
                (EHLabelAttachInfo const*)x->getAI()->get(AI_EH_LABEL);
            if (ehlab == nullptr) { continue; }

            xcom::SC<LabelInfo*> * sc;
            SList<LabelInfo*> const& labs = ehlab->read_labels();
            for (sc = labs.get_head();
                 sc != labs.end(); sc = labs.get_next(sc)) {
                ASSERT0(sc);
                IRBB * tgt = findBBbyLabel(sc->val());
                ASSERT0(tgt);
                xcom::Edge * e = DGraph::addEdge(bb->id(), tgt->id());
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
    IRBB * entry = nullptr;
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

        if (bb->mayThrowException() && !ehbbs.is_contain(bb->id())) {
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
             if (ehbbs.is_contain(a->id())) { continue; }

             xcom::Edge * e = DGraph::addEdge(a->id(), b->id());
             EDGE_info(e) = xmalloc(sizeof(CFGEdgeInfo));
             CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e)) = true;
             m_has_eh_edge = true;
         }
    }
}


//Verification at building SSA mode by ir parser.
bool IRCFG::verifyPhiEdge(IR * phi,
                          xcom::TMap<IR*, LabelInfo*> & ir2label) const
{
    xcom::Vertex * bbvex = getVertex(phi->getBB()->id());
    xcom::EdgeC * opnd_pred = VERTEX_in_list(bbvex);
    IR * opnd = PHI_opnd_list(phi);
    for (; opnd != nullptr && opnd_pred != nullptr;
         opnd = opnd->get_next(), opnd_pred = EC_next(opnd_pred)) {
        LabelInfo * opnd_label = ir2label.get(opnd);
        IRBB * incoming_bb = findBBbyLabel(opnd_label);
        ASSERT0(incoming_bb);
        if (opnd_pred->getFromId() != incoming_bb->id()) {
            return false;
        }
    }
    ASSERT0(opnd == nullptr && opnd_pred == nullptr);
    return true;
}


//Revise CFG edge for BB has phi.
//NOTE:CFG should have been built before revise Vertex order.
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
             ct2 != nullptr; BB_irlist(const_cast<IRBB*>(bb)).get_next(&ct2)) {
            IR * x = ct2->val();
            ASSERT0(x);
            if (!x->is_phi()) { continue; }
            if (phi_opnd_num == -1) {
                phi_opnd_num = xcom::cnt_list(PHI_opnd_list(x));

                //Sort in-edge of bb to guarantee the order of them are same
                //with the phi-operands.
                xcom::Vertex * bbvex = getVertex(bb->id());
                xcom::EdgeC * opnd_pred = bbvex->getInList();
                for (IR * opnd = PHI_opnd_list(x);
                     opnd != nullptr; opnd = opnd->get_next()) {
                    LabelInfo * opnd_label = ir2label.get(opnd);
                    IRBB * incoming_bb = findBBbyLabel(opnd_label);
                    ASSERT0(incoming_bb);
                    if (opnd_pred->getFromId() == incoming_bb->id()) {
                        opnd_pred = opnd_pred->get_next();
                        continue;
                    }

                    xcom::EdgeC * q;
                    for (q = opnd_pred->get_next();
                         q != nullptr; q = q->get_next()) {
                        if (q->getFromId() == incoming_bb->id()) {
                            break;
                        }
                    }
                    ASSERTN(q, ("can not find needed xcom::EdgeC"));
                    xcom::swap(&VERTEX_in_list(bbvex), opnd_pred, q);
                    opnd_pred = q->get_next();
                }

                ASSERT0(verifyPhiEdge(x, ir2label));
                continue;
            }

            //Verify whether the others PHI's operands are in correct order.
            ASSERTN((UINT)phi_opnd_num == xcom::cnt_list(PHI_opnd_list(x)),
                    ("the number of operand is inconsistent"));
            ASSERT0((UINT)phi_opnd_num ==
                    getInDegree(getVertex(bb->id())));
            ASSERT0(verifyPhiEdge(x, ir2label));
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
         bb != nullptr; bb = m_bb_list->get_prev()) {
        m_bb_vec.set(bb->id(), bb);
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != nullptr; li = bb->getLabelList().get_next()) {
            ASSERTN(m_lab2bb.get(li) == nullptr,
                    ("Label has been mapped to BB%d", m_lab2bb.get(li)->id()));
            m_lab2bb.set(li, bb);
            if (LABELINFO_is_catch_start(li)) {
                BB_is_catch_start(bb) = true;
            }
            if (LABELINFO_is_terminate(li)) {
                BB_is_terminate(bb) = true;
            }
        }
    }
    if (m_entry != nullptr) {
        //Already have entry BB.
        return;
    }
    switch (cs) {
    case C_SESE: {
        //Make sure the region has the unique entry.
        m_entry = m_rg->allocBB();
        BB_is_entry(m_entry) = true;
        addBB(m_entry);
        m_bb_list->append_head(m_entry);

        //Create logical exit BB.
        //NOTICE: In actually, the logical exit BB is ONLY
        //used to solve diverse dataflow equations, whereas
        //considering the requirement of ENTRY BB, EXIT BB.
        IRBB * exit = m_rg->allocBB();
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
        //     bb != nullptr; bb = m_bb_list->get_next()) {
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
        //oc.is_cfg_valid() = true;
        return;
    }

    //cfg->removeEmptyBB();
    build(oc);
    buildEHEdge();
    if (g_is_dump_after_pass && g_dump_opt.isDumpCFG()) {
        dumpDOT(getRegion()->getLogMgr()->getFileHandler());
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
    if (m_bb_list == nullptr) { return; }

    IR * casev_list = SWITCH_case_list(ir);
    if (SWITCH_deflab(ir) != nullptr) {
        IRBB * tbb = findBBbyLabel(SWITCH_deflab(ir));
        ASSERT0(tbb);
        tgt_bbs.append_tail(tbb);
    }

    if (casev_list != nullptr) {
        for (IR * casev = casev_list;
             casev != nullptr; casev = IR_next(casev)) {
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
void IRCFG::findEHRegion(IRBB const* catch_start,
                         xcom::BitSet const& mainstreambbs,
                         OUT xcom::BitSet & ehbbs)
{
    ASSERT0(catch_start && catch_start->isExceptionHandler());
    List<xcom::Vertex const*> list;
    xcom::Vertex const* bbv = getVertex(catch_start->id());
    ASSERT0(bbv);
    list.append_head(bbv);
    for (xcom::Vertex const* v = list.remove_head();
         v != nullptr; v = list.remove_head()) {
        UINT id = v->id();
        if (mainstreambbs.is_contain(id) || ehbbs.is_contain(id)) {
            continue;
        }

        ehbbs.bunion(id);

        xcom::EdgeC * el = VERTEX_out_list(v);
        while (el != nullptr) {
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
    xcom::Vertex const* bbv = getVertex(try_start->id());
    ASSERT0(bbv);
    list.append_head(bbv);
    for (xcom::Vertex const* v = list.remove_head();
         v != nullptr; v = list.remove_head()) {
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

        for (xcom::EdgeC * el = VERTEX_out_list(v);
             el != nullptr; el = EC_next(el)) {
            xcom::Edge const* e = EC_edge(el);
            CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
            if (ei != nullptr && CFGEI_is_eh(ei)) {
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
void IRCFG::findTargetBBOfIndirectBranch(IR const* ir,
                                         OUT List<IRBB*> & tgtlst)
{
    ASSERT0(ir->isIndirectBr());
    for (IR * c = IGOTO_case_list(ir); c != nullptr; c = c->get_next()) {
        ASSERT0(c->is_case());
        IRBB * bb = m_lab2bb.get(CASE_lab(c));
        ASSERT0(bb); //no bb is correspond to lab.
        tgtlst.append_tail(bb);

        #ifdef _DEBUG_
        bool find = false;
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != nullptr; li = bb->getLabelList().get_next()) {
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
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    findLoop();
    collectLoopInfo();
    OC_is_loopinfo_valid(oc) = true;
}


//Find bb that 'lab' attouchemented.
IRBB * IRCFG::findBBbyLabel(LabelInfo const* lab) const
{
    IRBB * bb = m_lab2bb.get(lab);
    if (bb == nullptr) { return nullptr; }

    #ifdef _DEBUG_
    bool find = false;
    for (LabelInfo const* li = bb->getLabelList().get_head();
         li != nullptr; li = bb->getLabelList().get_next()) {
        if (isSameLabel(lab, li)) {
            find = true;
            break;
        }
    }
    ASSERT0(find);
    #endif

    return bb;
}


//Split BB into two BBs.
//bb: BB to be splited.
//split_point: the ir in 'bb' used to mark the split point that followed IRs
//             will be moved to fallthrough newbb.
//e.g:bb:
//    ...
//    split_point;
//    ...
//  =>
//    bb:
//    ...
//    split_point; //the last ir in bb.
//    newbb:
//    ...
IRBB * IRCFG::splitBB(IRBB * bb, IRListIter split_point)
{
    IRBB * newbb = m_rg->allocBB();

    //Move rest IRs from bb to newbb.
    for (bb->getIRList()->get_next(&split_point); split_point != nullptr;) {
        IRListIter rm = split_point;
        bb->getIRList()->get_next(&split_point);
        bb->getIRList()->remove(rm);
        ASSERT0(rm->val());
        newbb->getIRList()->append_tail(rm->val());
    }

    //Update CFG info.
    addBB(newbb);

    //Update BB List.
    getBBList()->insert_after(newbb, bb);

    //Move EdgeInfo from old edges to new edges.
    xcom::Vertex const* v = getVertex(bb->id());
    ASSERT0(v);
    INT minsuccrpo = MAX_HOST_INT_VALUE;
    xcom::EdgeC * next_el;
    for (xcom::EdgeC * el = v->getOutList(); el != nullptr; el = next_el) {
        next_el = el->get_next();
        xcom::Edge * e = el->getEdge();
        UINT succ = e->to()->id();
        xcom::Edge * newe = addEdge(newbb->id(), succ);
        newe->copyEdgeInfo(e);
        xcom::Graph::removeEdge(e);

        //Collect the minimal RPO.
        if (succ != bb->id()) {
            IRBB const* succbb = getBB(succ);
            ASSERT0(succbb);
            minsuccrpo = MIN(succbb->rpo(), minsuccrpo);
        }
    }

    addEdge(bb->id(), newbb->id());

    //Update RPO
    INT rpo = bb->rpo() + 1;
    if (rpo < minsuccrpo && xcom::Graph::isValidRPO(rpo)) {
        BB_rpo(newbb) = rpo;
        xcom::Vertex * v = getVertex(newbb->id());
        VERTEX_rpo(v) = rpo;
    } else {
        BB_rpo(newbb) = RPO_UNDEF;
        xcom::Vertex * v = getVertex(newbb->id());
        VERTEX_rpo(v) = RPO_UNDEF;
    }
    return newbb;
}


//Try to update RPO of newbb accroding to RPO of marker.
//newbb_prior_marker: true if newbb's lexicographical order is prior to marker.
//Return true if this function find a properly RPO for 'newbb', otherwise
//return false.
bool IRCFG::tryUpdateRPO(IRBB * newbb,
                         IRBB const* marker,
                         bool newbb_prior_marker)
{
    xcom::Vertex const* v = getVertex(marker->id());
    ASSERT0(v);
    INT rpo = RPO_UNDEF;
    if (newbb_prior_marker) {
        //newbb is prior to marker.
        //Collect the maxmimum RPO of predecessors of marker.
        INT maxpredrpo = MIN_HOST_INT_VALUE;
        for (xcom::EdgeC * el = v->getInList();
             el != nullptr; el = el->get_next()) {
            xcom::Edge * e = el->getEdge();
            UINT pred = e->from()->id();
            if (pred != marker->id()) {
                IRBB const* predbb = getBB(pred);
                ASSERT0(predbb);
                if (xcom::Graph::isValidRPO(predbb->rpo())) {
                    //Exist invalid rpo, recompute them first.
                    return false;
                }
                maxpredrpo = MAX(predbb->rpo(), maxpredrpo);
            }
        }
        rpo = marker->rpo() - 1;
        if (rpo <= maxpredrpo) {
            rpo = RPO_UNDEF;
        }
    } else {
        //newbb is after marker.
        //Collect the minimal RPO of successors of marker.
        INT minsuccrpo = MAX_HOST_INT_VALUE;
        for (xcom::EdgeC * el = v->getOutList();
             el != nullptr; el = el->get_next()) {
            xcom::Edge * e = el->getEdge();
            UINT succ = e->to()->id();
            if (succ != marker->id()) {
                IRBB const* succbb = getBB(succ);
                ASSERT0(succbb);
                if (xcom::Graph::isValidRPO(succbb->rpo())) {
                    //Exist invalid rpo, recompute them first.
                    return false;
                }
                minsuccrpo = MIN(succbb->rpo(), minsuccrpo);
            }
        }
        rpo = marker->rpo() + 1;
        if (rpo >= minsuccrpo) {
            rpo = RPO_UNDEF;
        }
    }

    //Try to update RPO
    if (rpo != RPO_UNDEF && xcom::Graph::isValidRPO(rpo)) {
        BB_rpo(newbb) = rpo;
        xcom::Vertex * nv = getVertex(newbb->id());
        ASSERTN(nv, ("newbb should be added to graph first"));
        VERTEX_rpo(nv) = rpo;
        return true;
    }
    BB_rpo(newbb) = RPO_UNDEF;
    xcom::Vertex * nv = getVertex(newbb->id());
    VERTEX_rpo(nv) = RPO_UNDEF;
    return false;
}


//Insert BB before bb.
//e.g:BB1 BB2 BB3
//      \  |  /
//        BB4
//  after inserting newbb,
//    BB1 BB2 BB3
//      \  |  /
//       newbb
//         |
//        BB4
void IRCFG::insertBBbefore(IN IRBB * bb, IN IRBB * newbb)
{
    addBB(newbb);
    getBBList()->insert_before(newbb, bb);
    xcom::Vertex const* bbv = getVertex(bb->id());
    ASSERT0(bbv);
    xcom::Vertex * newbbv = getVertex(newbb->id());
    ASSERT0(newbbv);
    for (xcom::EdgeC * predlist = bbv->getInList();
         predlist != nullptr; predlist = predlist->get_next()) {
        xcom::Edge * prededge = predlist->getEdge();
        //Make sure the order of new edge is in same order as original edge.
        //e.g:original edge order is: BB1->BB4, BB2->BB4, BB3->BB4. The order
        //of new edge is: BB1->newbb, BB2->newbb, BB3->newbb.
        xcom::Edge * newedge = newEdge(prededge->from(), newbbv);
        EDGE_info(newedge) = prededge->info();
    }
    xcom::EdgeC * next = nullptr;
    for (xcom::EdgeC * predlist = bbv->getInList();
         predlist != nullptr; predlist = next) {
        next = predlist->get_next();
        DGraph::removeEdge(predlist->getEdge());
    }
    DGraph::addEdge(newbbv->id(), bbv->id());
    moveLabels(bb, newbb);
}


//Return the inserted trampolining BB if exist.
//This function will break fallthrough edge of 'to' if necessary.
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
    if (from->is_fallthrough() && bblst->get_next(&tmp_ct) == to) {
        bblst->insert_after(newbb, from_ct);
        addBB(newbb);
        insertVertexBetween(from->id(), to->id(), newbb->id());
        return nullptr;
    }

    //Second, from->to is jump-edge.
    List<IRBB*> preds;
    get_preds(preds, to);
    ASSERTN(preds.find(from), ("'from' is not pred of 'to'"));
    BBListIter pred_ct = nullptr;
    //Third, find the fallthrough previous BB of 'to'. If find it, insert
    //a trampolining BB between the previous BB of 'to' that contains a jump
    //IR.
    IRBB * inserted_tramp_bb = nullptr;
    for (IRBB * pred = preds.get_head(&pred_ct);
         pred != nullptr; pred = preds.get_next(&pred_ct)) {
        BBListIter tmp_ct2 = to_ct;
        if (pred->is_fallthrough() && bblst->get_prev(&tmp_ct2) == pred) {
            //Given 'to' has a fallthrough in-edge. Insert a tmp BB
            //e.g:Given following edges,
            //    from->bb1->bb2->to, where all edges are fallthrough edges;
            //    from->to is jump-edge
            //    bb1->to is jump-edge
            //
            //We got it and have to revise the fallthrough edge 'bb2->to',
            //the result is from->bb1->bb2->inserted_tramp_bb, where all
            //edges are fallthrough, inserted_tramp_bb->to becomes jump-edge
            //    from->to is jump-edge
            //    bb1->to is jump-edge
            //    bb2->inserted_tramp_bb, tmp_tramp_bb->to, both are jump-edge.
            //    ir-list of inserted_tramp_bb is:
            //        goto L1:
            //
            //    ir-list of 'to' is:
            //        L1:
            //        ...
            //        ...
            inserted_tramp_bb = m_rg->allocBB();
            LabelInfo * li = m_rg->genILabel();
            IR * goto_ir = m_rg->buildGoto(li);
            BB_irlist(inserted_tramp_bb).append_tail(goto_ir);
            addLabel(to, li);
            addBB(inserted_tramp_bb);
            bblst->insert_after(inserted_tramp_bb, pred);

            //Insert a trampolining BB between the previous BB of 'to'
            //that contains a jump IR.
            insertVertexBetween(pred->id(), to->id(), inserted_tramp_bb->id());

            //Now, fallthrough edge bb2->to has been broken, we can insert
            //'newbb' before 'to' correctly.
            break;
        }
    }

    //Revise the target LABEL of last XR in 'from'.
    IR * last_xr_of_from = get_last_xr(from);

    ASSERT0(last_xr_of_from->getLabel() &&
            findBBbyLabel(last_xr_of_from->getLabel()) == to);
    ASSERT0(last_xr_of_from->getLabel() != nullptr);

    LabelInfo * li = m_rg->genILabel();
    last_xr_of_from->setLabel(li);

    addLabel(newbb, li);

    //When we get here, there are NOT any fallthrough in-edges of 'to'.
    bblst->insert_before(newbb, to_ct);
    insertVertexBetween(from->id(), to->id(), newbb->id());
    return inserted_tramp_bb;
}


//Migrate Lables from src BB to tgt BB.
void IRCFG::moveLabels(IRBB * src, IRBB * tgt)
{
    tgt->mergeLabeInfoList(src);

    //Set label2bb map.
    for (LabelInfo const* li = tgt->getLabelList().get_head();
         li != nullptr; li = tgt->getLabelList().get_next()) {
        m_lab2bb.setAlways(li, tgt);
    }

    //Cut off the relation between src and Labels.
    src->cleanLabelInfoList();
}


//Cut off the mapping relation bwteen Labels and BB.
void IRCFG::resetMapBetweenLabelAndBB(IRBB * bb)
{
    for (LabelInfo const* li = bb->getLabelList().get_head();
         li != nullptr; li = bb->getLabelList().get_next()) {
        m_lab2bb.setAlways(li, nullptr);
    }
    bb->cleanLabelInfoList();
}


//Combine trampoline branch.
//e.g:L2:
//    truebr L4 | false L4
//    goto L3 //redundant jump
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
         bb != nullptr; bb = m_bb_list->get_next(&ct)) {
        if (bb->isExceptionHandler()) { continue; }

        IR * br = get_last_xr(bb);
        if (br == nullptr || !br->isConditionalBr()) {
            continue;
        }
        if (br->hasSideEffect()) {
            continue;
        }

        BBListIter nextbbct = ct;
        IRBB * next = m_bb_list->get_next(&nextbbct);
        IR * jmp = nullptr;
        if (next == nullptr || //bb may be the last BB in bb-list.
            (jmp = get_first_xr(next)) == nullptr || //bb can not be empty
            !jmp->is_goto()) { //the only IR must be GOTO
            continue;
        }

        if (next->isExceptionHandler()) { continue; }

        IRBB * next_next = m_bb_list->get_next(&nextbbct);
        if (next_next == nullptr || //bb may be the last BB in bb-list.
            !next_next->isContainLabel(BR_lab(br))) {
            continue;
        }

        IRBB * jmp_tgt = findBBbyLabel(GOTO_lab(jmp));
        xcom::Edge const* e_of_jmp = getEdge(next->id(), jmp_tgt->id());
        ASSERT0(e_of_jmp);
        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e_of_jmp);
        if (ei != nullptr && CFGEI_is_eh(ei)) {
            //Do not remove exception edge.
            continue;
        }

        xcom::Edge const* e_of_bb = getEdge(bb->id(), next_next->id());
        ASSERT0(e_of_bb);
        CFGEdgeInfo * ei2 = (CFGEdgeInfo*)EDGE_info(e_of_bb);
        if (ei2 != nullptr && CFGEI_is_eh(ei2)) {
            //Do not remove exception edge.
            continue;
        }

        //Displacement
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

        //Add next->next_next, actually jmp->next_next because 'next' is empty.
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


bool IRCFG::isRPOValid() const
{
    BBListIter ct;
    for (IRBB * bb = m_bb_list->get_head(&ct);
         bb != nullptr; bb = m_bb_list->get_next(&ct)) {
        if (bb->rpo() == RPO_UNDEF) {
            return false;
        }
    }
    return true;
}


void IRCFG::remove_bb_impl(IRBB * bb)
{
    ASSERT0(bb);
    m_bb_vec.set(bb->id(), nullptr);

    //C<LabelInfo const*> * ct;
    //for (lablst.get_head(&ct);
    //     ct != lablst.end(); ct = lablst.get_next(ct)) {
    //    m_lab2bb.remove(ct->val());
    //}

    removeVertex(bb->id());
}


//CASE: Given pred1->bb, fallthrough edge, and pred2->bb, jumping edge.
//  pred2:
//    goto bb;
//  pred1:
//    a=1;
//  bb:
//    goto next;
//  next:
//    ...
//Remove bb and revise CFG.
//ct: container in m_bb_list of CFG. It will be updated if related BB removed.
bool IRCFG::removeTrampolinBBCase1(BBListIter * ct)
{
    List<IRBB*> preds;
    IRBB * bb = (*ct)->val();
    ASSERT0(getSuccsNum(bb) == 1);
    IRBB const* succ = get_first_succ(bb);
    ASSERT0(succ);
    IR const* uncond_br = get_first_xr(const_cast<IRBB*>(bb));
    ASSERT0(uncond_br && uncond_br->isUnconditionalBr());
    BBListIter tmp_bb_ct = *ct;
    IRBB * next = m_bb_list->get_next(&tmp_bb_ct);
    if (next == nullptr || //bb can not be the last BB in bb-list.
        next != succ) { //next lexical bb must also be the successor.
        return false;
    }

    Vertex const* vex = getVertex(next->id());
    ASSERT0(vex);
    if (getInDegree(vex) > 1) {
        //successor is not just the branch target of 'bb', thus it
        //can not be removed.
        return false;
    }

    tmp_bb_ct = *ct;
    IRBB * prev = m_bb_list->get_prev(&tmp_bb_ct);
    preds.clean(); //use list because cfg may be modify.
    get_preds(preds, bb);
    for (IRBB * pred = preds.get_head();
         pred != nullptr; pred = preds.get_next()) {
        moveLabels(bb, next);

        if (pred->is_fallthrough() && prev == pred) {
            removeEdge(pred, bb);

            //Add normal control flow edge.
            xcom::Edge * e = addEdge(pred, next);
            CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
            if (ei != nullptr && CFGEI_is_eh(ei)) {
                //If there is already an edge, check if it is an
                //exception edge. If it is, change the exception edge
                //to be normal control flow edge.
                CFGEI_is_eh(ei) = false;
            }
            return true;
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
        if (last_xr_of_pred != nullptr) {
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
        if (ei != nullptr && CFGEI_is_eh(ei)) {
            //If there is already an edge, check if it is an
            //exception edge. If it is, change the exception edge
            //to be normal control flow edge.
            CFGEI_is_eh(ei) = false;
        }
    } //end for each pred of BB.

    //The map between Labels and BB has been maintained.
    //resetMapBetweenLabelAndBB(bb);
    removeBB(bb);

    //Update ct to reprocess BB list from beginning.
    m_bb_list->get_head(ct);
    return true;
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
    for (m_bb_list->get_head(&ct); ct != nullptr; ct = m_bb_list->get_next(ct)) {
        IRBB const* bb = ct->val();
        if (bb->isExceptionHandler()) { continue; }

        IR const* uncond_br = get_first_xr(const_cast<IRBB*>(bb));
        if (uncond_br == nullptr ||
            !uncond_br->isUnconditionalBr() ||
            bb->getNumOfIR() != 1) {
            continue;
        }
        removed |= removeTrampolinBBCase1(&ct);
    }
    return removed;
}


bool IRCFG::removeTrampolinEdgeCase2(BBListIter bbct)
{
    ASSERT0(bbct);
    IRBB * bb = bbct->val();
    bool removed = false;
    List<IRBB*> preds; //record preds in list because CFG may be modified.
    get_preds(preds, bb);

    IRBB * succ = get_first_succ(bb);
    ASSERT0(succ);

    if (succ == bb) {
        //CASE: BB_pred
        //       |
        //       V
        //      BB <-
        //       |   |
        //       |___|
        //BB and succ are the same BB.
        return false;
    }

    IR * last_xr = get_last_xr(bb);
    ASSERT0(last_xr);

    LabelInfo const* tgt_li = last_xr->getLabel();
    ASSERT0(tgt_li != nullptr);
    ASSERT0(findBBbyLabel(tgt_li) == succ);

    for (IRBB * pred = preds.get_head();
         pred != nullptr; pred = preds.get_next()) {
        if (pred == bb) {
            //bb's pred is itself.
            continue;
        }

        if (pred->getNumOfIR() == 0) {
            continue;
        }

        IR * last_xr_of_pred = get_last_xr(pred);
        if (!IRBB::isLowerBoundary(last_xr_of_pred)) {
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
            ASSERT0(last_xr_of_pred->getLabel() != nullptr);
            if (succ->id() == bb->id()) {
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
            BBListIter prev_of_bb = bbct;
            if (m_bb_list->get_prev(&prev_of_bb) == pred) {
                //Can not remove jumping-edge if 'bb' is
                //fall-through successor of 'pred'.
                continue;
            }

            ASSERT0(last_xr_of_pred->getLabel() &&
                    findBBbyLabel(last_xr_of_pred->getLabel()) == bb);

            ASSERT0(last_xr_of_pred->getLabel() != nullptr);
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


bool IRCFG::removeTrampolinEdgeCase1(BBListIter bbct)
{
    ASSERT0(bbct);
    IRBB * bb = bbct->val();
    BBListIter next_ct = m_bb_list->get_next(bbct);
    if (next_ct == nullptr) { return false; }

    //BB is not the last one in BB list.
    IR * last_xr = get_last_xr(bb);
    ASSERT0(last_xr && last_xr->is_goto());

    LabelInfo const* tgt_li = last_xr->getLabel();
    ASSERT0(tgt_li != nullptr);

    IRBB * target = findBBbyLabel(tgt_li);
    if (target == next_ct->val()) {
        Vertex const* vex = getVertex(target->id());
        ASSERT0(vex);
        if (getInDegree(vex) > 1) {
            //'target' is not just the branch target of 'bb', thus
            //it can not be removed.
            return false;
        }

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
        return true;
    }

    return false;
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

        bool res1 = removeTrampolinEdgeCase1(ct);
        removed |= res1;
        if (res1) {
            continue;
        }
        removed |= removeTrampolinEdgeCase2(ct);
    }
    return removed;
}


bool IRCFG::removeRedundantBranch()
{
    bool removed = CFG<IRBB, IR>::removeRedundantBranch();
    BBListIter ct;
    List<IRBB*> succs;
    for (IRBB * bb = m_bb_list->get_head(&ct);
         bb != nullptr; bb = m_bb_list->get_next(&ct)) {
        IR * last_xr = get_last_xr(bb);
        if (last_xr == nullptr ||
            !last_xr->isConditionalBr() ||
            last_xr->hasSideEffect()) {
            continue;
        }

        IR * det = BR_det(last_xr);
        ASSERT0(det != nullptr);
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
            ASSERT0(tgt_li != nullptr);
            BB_irlist(bb).remove_tail();
            removeStmt(last_xr, m_rg);
            m_rg->freeIRTree(last_xr);

            IR * uncond_br = m_rg->buildGoto(tgt_li);
            BB_irlist(bb).append_tail(uncond_br);

            //Remove fallthrough edge, leave branch edge.
            get_succs(succs, bb);
            BBListIter tmp_ct = ct;
            m_bb_list->get_next(&tmp_ct);
            for (IRBB * s = succs.get_head();
                 s != nullptr; s = succs.get_next()) {
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
            removeStmt(r, m_rg);
            m_rg->freeIRTree(r);

            //Remove branch edge, leave fallthrough edge.
            get_succs(succs, bb);
            BBListIter tmp_ct = ct;
            m_bb_list->get_next(&tmp_ct);
            for (IRBB * s = succs.get_head();
                 s != nullptr; s = succs.get_next()) {
                if (s != tmp_ct->val()) {
                    removeEdge(bb, s);
                }
            }
            removed = true;
        }
    } //for each BB
    return removed;
}


bool IRCFG::verifyRPO(OptCtx const& oc) const
{
    if (!OC_is_rpo_valid(oc)) { return true; }
    IRCFG * pthis = const_cast<IRCFG*>(this);
    ASSERT0(pthis->getBBList());
    ASSERTN(isRPOValid(), ("Miss RPO info or set rpo invalid in OptCtx"));
    if (pthis->getRPOBBList() != nullptr) {
        ASSERTN(pthis->getRPOBBList()->get_elem_count() ==
                pthis->getBBList()->get_elem_count(),
                ("RPO info need to be fixed or set rpo invalid in OptCtx"));
    }
    return true;
}


void IRCFG::dumpDOT(CHAR const* name, UINT flag)
{
    //Do not dump if LogMr is not initialized.
    if (!getRegion()->isLogMgrInit()) { return; }

    //Note this function does not use LogMgr as output.
    //So it is dispensable to check LogMgr.
    if (name == nullptr) {
        name = "graph_cfg.dot";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    dumpDOT(h, flag);
    fclose(h);
}


void IRCFG::dumpDOT(FILE * h, UINT flag)
{
    if (!getRegion()->isLogMgrInit() || h == nullptr) { return; }
    getRegion()->getLogMgr()->push(h, "");

    bool detail = HAVE_FLAG(flag, DUMP_DETAIL);
    bool dump_eh = HAVE_FLAG(flag, DUMP_EH);
    bool dump_mdssa = HAVE_FLAG(flag, DUMP_MDSSA);
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
    bool org_replace = getRegion()->getLogMgr()->isReplaceNewline();
    getRegion()->getLogMgr()->setReplaceNewline(true);

    //Print Region name.
    fprintf(h,
            "\nstartnode [fontsize=24,style=filled, "
            "color=gold,shape=none,label=\"RegionName:%s\"];",
            m_rg->getRegionName());

    MDSSAMgr const* mdssamgr = (MDSSAMgr const*)m_rg->getPassMgr()->queryPass(
                               PASS_MD_SSA_MGR);
    if (mdssamgr == nullptr || !mdssamgr->is_valid()) {
        dump_mdssa = false;
    }

    //Print node
    VertexIter c = VERTEX_UNDEF;
    for (xcom::Vertex * v = get_first_vertex(c);
         v != nullptr; v = get_next_vertex(c)) {
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
            fprintf(h,
                    "\nnode%d [font=\"%s\",fontsize=%d,color=%s,"
                    "shape=%s,style=%s,label=\"BB%d ",
                    v->id(), font, fontsize, color, shape, style, v->id());

            dumpBBLabel(bb->getLabelList(), getRegion());
            fprintf(h, " ");
            fprintf(h, "rpo:%d ", bb->rpo());

            //Dump MDSSA Phi List.
            if (dump_mdssa) {
                MDPhiList const* philist = mdssamgr->getPhiList(bb);
                if (philist != nullptr) {
                    mdssamgr->dumpPhiList(philist);
                }
            }

            //Dump IR list.
            for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
                //The first \l is very important to display
                //DOT in a fine manner.
                fprintf(h, "\\l");

                //TODO: implement dump_ir_buf();
                if (dump_mdssa) {
                    mdssamgr->dumpIRWithMDSSA(ir, IR_DUMP_KID);
                } else {
                    dumpIR(ir, m_rg, nullptr, IR_DUMP_KID);
                }
            }

            //The last \l is very important to display DOT in a fine manner.
            fprintf(h, "\\l");

            //The end char of properties.
            fprintf(h, "\"];");
            fflush(h);
            continue;
        }

        //Dump node without detail.
        fprintf(h,
                "\nnode%d [font=\"%s\",fontsize=%d,color=%s,"
                "shape=%s,style=%s,label=\"BB%d\"];",
                v->id(), font, fontsize, color, shape, style, v->id());
        fflush(h);
    }

    //Print edge
    VertexIter c2 = VERTEX_UNDEF;
    TTab<xcom::Edge const*> visited;
    for (xcom::Vertex * v = get_first_vertex(c2);
         v != nullptr; v = get_next_vertex(c2)) {
        //Print in-edge list.
        UINT pos = 0;
        for (xcom::EdgeC const* ec = v->getInList();
             ec != nullptr; ec = ec->get_next(), pos++) {
            if (visited.find(ec->getEdge())) { continue; }
            visited.append(ec->getEdge());
            xcom::Edge const* e = ec->getEdge();

            CFGEdgeInfo * ei = (CFGEdgeInfo*)e->info();
            if (ei == nullptr) {
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
    EdgeIter ite;
    for (xcom::Edge const* e = get_first_edge(ite);
         e != nullptr;  e = get_next_edge(ite)) {
        if (visited.find(e)) { continue; }

        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei == nullptr) {
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
    getRegion()->getLogMgr()->pop();
    getRegion()->getLogMgr()->setReplaceNewline(org_replace);
    fprintf(h,"\n}\n");
    fflush(h);
}


void IRCFG::dump_node(bool detail, bool dump_mdssa)
{
    FILE * h = getRegion()->getLogMgr()->getFileHandler();
    ASSERT0(h);
    ASSERT0(m_bb_list);
    MDSSAMgr const* mdssamgr = (MDSSAMgr const*)m_rg->getPassMgr()->queryPass(
                               PASS_MD_SSA_MGR);
    if (mdssamgr == nullptr && !mdssamgr->is_valid()) {
        dump_mdssa = false;
    }

    UINT vertical_order = 1;
    INT c;
    for (xcom::Vertex const* v = get_first_vertex(c);
        v != nullptr; v = get_next_vertex(c)) {
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
            fprintf(h, "   BB%d ", bb->rpo());
            fprintf(h, " rpo:%d ", VERTEX_rpo(v));
            dumpBBLabel(bb->getLabelList(), getRegion());
            fprintf(h, "\n");

            //Dump MDSSA Phi List.
            if (dump_mdssa) {
                MDPhiList const* philist = mdssamgr->getPhiList(bb);
                if (philist != nullptr) {
                    mdssamgr->dumpPhiList(philist);
                }
            }

            //Dump IR list.
            for (IR * ir = BB_first_ir(bb);
                 ir != nullptr; ir = BB_next_ir(bb)) {
                //fprintf(h, "%s\n", dump_ir_buf(ir, buf));

                //TODO: implement dump_ir_buf();
                if (dump_mdssa) {
                    mdssamgr->dumpIRWithMDSSA(ir, IR_DUMP_KID);
                } else {
                    dumpIR(ir, m_rg, nullptr, IR_DUMP_KID);
                }
            }

            fprintf(h, "\"}");
            fflush(h);
            continue;
        }

        //Dump node without detail.
        fprintf(h,
                "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
                "fontname:\"%s\" scaling:%d label:\"%d",
                v->id(), vertical_order++, shape, color, font, scale, v->id());
        fprintf(h, " rpo:%d", bb->rpo());
        fprintf(h, "\" }");
    }
}


//Print graph structure description.
void IRCFG::dump_head(FILE * h)
{
    ASSERT0(h);
    fprintf(h,
            "graph: {"
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


void IRCFG::dump_edge(bool dump_eh)
{
    FILE * h = getRegion()->getLogMgr()->getFileHandler();
    ASSERT0(h);
    INT c2;
    TTab<xcom::Edge const*> visited;
    UINT pos = 0;
    for (xcom::Vertex * v = get_first_vertex(c2);
         v != nullptr; v = get_next_vertex(c2), pos++) {
        //Print in-edge list.
        UINT pos2 = 0;
        for (xcom::EdgeC const* ec = v->getInList();
             ec != nullptr; ec = ec->get_next(), pos2++) {
            if (visited.find(ec->getEdge())) { continue; }
            visited.append(ec->getEdge());
            xcom::Edge const* e = ec->getEdge();

            CFGEdgeInfo * ei = (CFGEdgeInfo*)e->info();
            if (ei == nullptr) {
                fprintf(h,
                        "\nedge: { sourcename:\"%d\" targetname:\"%d\" "
                        " thickness:4 color:darkred label:\"p%d\" }",
                        e->from()->id(), e->to()->id(), pos2);
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
    EdgeIter c;
    for (xcom::Edge * e = get_first_edge(c);
         e != nullptr; e = get_next_edge(c)) {
        if (visited.find(e)) { continue; }

        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei == nullptr) {
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


void IRCFG::dumpVCG(CHAR const* name, UINT flag)
{
    if (!getRegion()->isLogMgrInit()) { return; }
    ASSERT0(m_rg);

    bool detail = HAVE_FLAG(flag, DUMP_DETAIL);
    bool dump_eh = HAVE_FLAG(flag, DUMP_EH);
    bool dump_mdssa = HAVE_FLAG(flag, DUMP_MDSSA);

    if (name == nullptr) { name = "graph_cfg.vcg"; }

    //Note this function does not use LogMgr as output.
    //So it is dispensable to check LogMgr.
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h != nullptr, ("%s create failed!!!",name));
    dump_head(h);

    //Print Region name.
    fprintf(h,
            "\nnode: {title:\"\" vertical_order:0 shape:box color:turquoise "
            "borderwidth:0 fontname:\"Courier Bold\" "
            "scaling:2 label:\"RegionName:%s\" }",
            m_rg->getRegionName());
    getRegion()->getLogMgr()->push(h, "");
    dump_node(detail, dump_mdssa);
    dump_edge(dump_eh);
    getRegion()->getLogMgr()->pop();
    fprintf(h, "\n}\n");
    fclose(h);
}


void IRCFG::computeDomAndIdom(IN OUT OptCtx & oc, xcom::BitSet const* uni)
{
    if (getBBList()->get_elem_count() == 0) { return; }

    DUMMYUSE(uni);
    START_TIMER(t, "Compute Dom, IDom");
    ASSERT0(oc.is_cfg_valid());
    ASSERTN(m_entry, ("ONLY support SESE or SEME"));

    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    List<IRBB*> * bblst = getRPOBBList();
    ASSERT0(bblst);
    ASSERT0(bblst->get_elem_count() == m_rg->getBBList()->get_elem_count());

    List<xcom::Vertex const*> vlst;
    for (IRBB * bb = bblst->get_head(); bb != nullptr; bb = bblst->get_next()) {
        ASSERT0(bb->id() != 0);
        vlst.append_tail(getVertex(bb->id()));
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
        dump_dom(getRegion()->getLogMgr()->getFileHandler(), false);
    }
}


void IRCFG::computePdomAndIpdom(IN OUT OptCtx & oc, xcom::BitSet const* uni)
{
    if (getBBList()->get_elem_count() == 0) { return; }

    START_TIMER(t, "Compute PDom,IPDom");
    ASSERT0(oc.is_cfg_valid());

    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    List<IRBB*> * bblst = getRPOBBList();
    ASSERT0(bblst);
    ASSERT0(bblst->get_elem_count() == m_rg->getBBList()->get_elem_count());

    List<xcom::Vertex const*> vlst;
    for (IRBB * bb = bblst->get_tail(); bb != nullptr; bb = bblst->get_prev()) {
        ASSERT0(bb->id() != 0 && getVertex(bb->id()));
        vlst.append_tail(getVertex(bb->id()));
    }

    bool f = false;
    if (uni != nullptr) {
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
    ASSERT0(ir->is_stmt());
    removeStmt(ir, m_rg);
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
            bool res = removeUnreachBB();
            lchange |= res;
        }

        if (g_do_cfg_remove_empty_bb) {
            bool res = removeEmptyBB(oc);
            lchange |= res;
        }

        if (g_do_cfg_remove_redundant_branch) {
            bool res = removeRedundantBranch();
            lchange |= res;
        }

        if (g_do_cfg_remove_trampolin_bb) {
            bool res = removeTrampolinEdge();
            lchange |= res;
        }

        if (g_do_cfg_invert_condition_and_remove_trampolin_bb) {
            bool res = inverseAndRemoveTrampolineBranch();
            lchange |= res;
        }

        if (lchange) {
            oc.setInvalidIfCFGChanged();
        }
        change |= lchange;
        count++;
    } while (lchange && count < 1000);
    ASSERTN(!lchange, ("CFG optimization iterated too many times."));

    if (change) {
        computeExitList();

        if (OC_is_cdg_valid(org_oc)) {
            m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_CDG,
                                                       PASS_UNDEF);
            ASSERT0(verifyIfBBRemoved((CDG*)m_rg->getPassMgr()->
                                      queryPass(PASS_CDG), oc));
        }

        //SSAInfo is invalid by adding new-edge to BB.
        //This will confuse phi insertion.
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg));
    }

    ASSERT0(verifyIRandBB(getBBList(), m_rg));
    ASSERT0(verifyRPO(oc));
    END_TIMER(t, "CFG Optimizations");
    return change;
}

} //namespace xoc
