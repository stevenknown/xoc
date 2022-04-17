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
    : Pass(rg), CFG<IRBB, IR>(bbl, edge_hash_size, vertex_hash_size)
{
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
    BBListIter bbit;
    for (m_bb_list->get_head(&bbit);
         bbit != m_bb_list->end(); bbit = m_bb_list->get_next(bbit)) {
        IRBB const* bb = bbit->val();
        IRListIter irit;
        IR * x = const_cast<IRBB*>(bb)->getIRList().get_tail(&irit);
        if (x != nullptr && x->isMayThrow(true) && x->getAI() != nullptr) {
            EHLabelAttachInfo const* ehlab =
                (EHLabelAttachInfo const*)x->getAI()->get(AI_EH_LABEL);
            if (ehlab == nullptr) { continue; }

            xcom::SC<LabelInfo*> * sc;
            SList<LabelInfo*> const& labs = ehlab->read_labels();
            for (sc = labs.get_head();
                 sc != labs.end(); sc = labs.get_next(sc)) {
                ASSERT0(sc);
                IRBB const* tgt = findBBbyLabel(sc->val());
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


//The function verify whether the branch target is match to the BB.
bool IRCFG::verifyBranchTarget() const
{
    IRCFG * pthis = const_cast<IRCFG*>(this);
    BBListIter it;
    BBList * lst = pthis->getBBList();
    for (IRBB * bb = lst->get_head(&it); bb != nullptr;
         bb = lst->get_next(&it)) {
        IR const* last = BB_last_ir(bb);
        if (last == nullptr || !last->isBranch()) { continue; }
        if (last->isConditionalBr()) {
            LabelInfo const* lab = last->getLabel();
            ASSERT0(lab);
            IRBB * succ0 = getNthSucc(bb, 0);
            IRBB * succ1 = getNthSucc(bb, 1);
            ASSERT0(succ0 && succ1);
            BBListIter next_it = it;
            lst->get_next(&next_it);
            ASSERT0(next_it);
            //Find the taken-branch-target and check whether the label is over
            //there.
            if (succ1 != next_it->val()) {
                ASSERT0(findBBbyLabel(lab) == succ1);
            } else {
                ASSERT0(findBBbyLabel(lab) == succ0);
            }
            continue;
        }
        if (last->isUnconditionalBr()) {
            LabelInfo const* lab = last->getLabel();
            IRBB * succ0 = getNthSucc(bb, 0);
            ASSERT0(succ0);
            //Find the taken-branch-target and check whether the label is over
            //there.
            ASSERT0(findBBbyLabel(lab) == succ0);
            continue;
        }
        if (last->isMultiConditionalBr() || last->isIndirectBr()) {
            List<LabelInfo const*> lablst;
            List<IRBB*> bblst;
            get_succs(bblst, bb);
            last->collectLabel(lablst);
            for (LabelInfo const* lab = lablst.get_head(); lab != nullptr;
                 lab = lablst.get_next()) {
                bool find = false;
                for (IRBB * succ = bblst.get_head(); succ != nullptr;
                     succ = bblst.get_next()) {
                    if (findBBbyLabel(lab) == succ) {
                        find = true;
                        break;
                    }
                }
                ASSERT0(find);
            }
            continue;
        }
    }
    return true;
}


//The function verify the relationship between BB and LabelInfo.
bool IRCFG::verifyLabel2BB() const
{
    IRCFG * pthis = const_cast<IRCFG*>(this);
    for (IRBB * bb = pthis->getBBList()->get_head(); bb != nullptr;
         bb = pthis->getBBList()->get_next()) {
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != nullptr; li = bb->getLabelList().get_next()) {
            ASSERT0(m_lab2bb.get(li) == bb);
        }
        bb->verifyBranchLabel(m_lab2bb);
    }
    return true;
}


//Verification at building SSA mode by ir parser.
bool IRCFG::verifyPhiEdge(IR * phi, TMap<IR*, LabelInfo*> & ir2label) const
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


//Sort the order of predecessor of given BB according to PHI operand layout.
void IRCFG::sortPred(IRBB const* bb, IR * phi, TMap<IR*, LabelInfo*> & ir2label)
{
    //Sort in-edge of bb to guarantee the order of them are same
    //with the phi-operands.
    xcom::Vertex * bbvex = getVertex(bb->id());
    xcom::EdgeC * opnd_pred = bbvex->getInList();
    for (IR * opnd = PHI_opnd_list(phi);
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
        ASSERTN(q, ("can not find expected in-edge for BB%d", bb->id()));
        xcom::swap(&VERTEX_in_list(bbvex), opnd_pred, q);
        opnd_pred = q->get_next();
    }

    ASSERT0(verifyPhiEdge(phi, ir2label));
}


//The function replaces original predecessor with a list of
//new predecessors.
//bb: the predecessor will be replaced.
//succ: the target BB.
//newpreds: list of new predecessors.
//Return the position of 'from' that is in the predecessor list of 'to'.
UINT IRCFG::replacePred(IRBB const* bb, IRBB const* succ,
                        List<UINT> const& newpreds)
{
    UINT orgpos = WhichPred(bb, succ);
    UINT orgnum = getPredsNum(succ);
    UINT newpredstartpos = CFG<IRBB, IR>::replacePred(bb, succ, newpreds);
    PRSSAMgr * prssamgr = getRegion()->getPRSSAMgr();
    MDSSAMgr * mdssamgr = getRegion()->getMDSSAMgr();
    bool useprssa = prssamgr != nullptr && prssamgr->is_valid();
    bool usemdssa = mdssamgr != nullptr && mdssamgr->is_valid();
    if (newpreds.get_elem_count() <= 1) {
        if (getPredsNum(succ) != orgnum) {
            ASSERT0(newpredstartpos == orgpos);
            if (useprssa) {
                //Remove phi information before modify CFG.
                prssamgr->removeSuccessorDesignatedPhiOpnd(succ,
                                                           newpredstartpos);
            }
            if (usemdssa) {
                //Remove phi information before modify CFG.
                mdssamgr->removeSuccessorDesignatedPhiOpnd(succ,
                                                           newpredstartpos);
            }
        }
        //No need to duplicate operand if there is only one predecessor.
        return newpredstartpos;
    }
    //Update phi operation.
    //Copy the operand that is in the replacement position. The original
    //livein value of the operand should livein from new predecessors as well.
    if (useprssa) {
        prssamgr->dupAndInsertPhiOpnd(succ, newpredstartpos,
                                      newpreds.get_elem_count());
    }
    if (usemdssa) {
        mdssamgr->dupAndInsertPhiOpnd(succ, newpredstartpos,
                                      newpreds.get_elem_count());
    }
    return newpredstartpos;
}


//Revise CFG edge for BB has phi.
//NOTE:CFG should have been built before revise Vertex order.
void IRCFG::reorderPhiEdge(xcom::TMap<IR*, LabelInfo*> & ir2label)
{
    ASSERTN(m_bb_list, ("bb_list is emt"));
    BBListIter ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        IRBB const* bb = ct->val();
        if (BB_irlist(bb).get_elem_count() == 0) {
            continue;
        }

        //Used to verify all PHI have the same number of operands.
        INT phi_opnd_num = -1;
        IRListIter ct2;
        for (BB_irlist(const_cast<IRBB*>(bb)).get_head(&ct2);
             ct2 != nullptr; BB_irlist(const_cast<IRBB*>(bb)).get_next(&ct2)) {
            IR * x = ct2->val();
            ASSERT0(x);
            if (!x->is_phi()) { continue; }
            if (phi_opnd_num == -1) {
                //Only sort predecessor once.
                phi_opnd_num = xcom::cnt_list(PHI_opnd_list(x));
                sortPred(bb, x, ir2label);
                continue;
            }

            //Verify whether the others PHI's operands are in correct order.
            ASSERTN((UINT)phi_opnd_num == xcom::cnt_list(PHI_opnd_list(x)),
                    ("the number of operand is inconsistent"));
            ASSERT0((UINT)phi_opnd_num == getVertex(bb->id())->getInDegree());
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
    //ASSERT0(m_bb_vec.get_last_idx() == VEC_UNDEF);
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
        //Note we always create entry BB because original CFG may only
        //contain cyclic graph.
        m_entry = m_rg->allocBB();
        addBB(m_entry);
        m_bb_list->append_head(m_entry);
        BB_is_entry(m_entry) = true;

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
        //Note we always create entry BB because original CFG may only
        //contain cyclic graph.
        m_entry = m_rg->allocBB();
        addBB(m_entry);
        m_bb_list->append_head(m_entry);
        BB_is_entry(m_entry) = true;
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


static LabelInfo const* useFirstAvailLabel(IR * ir, LabelInfo const* li,
                                           IRCFG const* cfg)
{
    if (li->hasSideEffect()) { return li; }
    IRBB * tgt = cfg->findBBbyLabel(li);
    ASSERT0(tgt);
    LabelInfoListIter liit;
    LabelInfoList & lst = tgt->getLabelList();
    for (lst.get_head(&liit); liit != nullptr; liit = lst.get_next(liit)) {
        LabelInfo const* tli = liit->val();
        if (tli->hasSideEffect()) { continue; }
        if (tli != li) {
            ir->setLabel(tli);
        }
        return tli;
    }
    UNREACHABLE();
    return nullptr;
}


void IRCFG::removeDomInfo(C<IRBB*> * bbct, CfgOptCtx const& ctx)
{
    if (ctx.need_update_dominfo()) {
        DGraph::removeDomInfo(bbct->val()->id());
    }
}


//You should clean the relation between Label and BB before removing BB.
void IRCFG::removeBB(C<IRBB*> * bbct, CfgOptCtx const& ctx)
{
    ASSERT0(bbct && m_bb_list->in_list(bbct));
    IRBB * bb = bbct->val();
    removeAllMDPhi(bb);
    removeRPO(bb);
    //Note Label2BB should has been mainated before come to the function.
    removeVertex(bb->id());
    m_bb_vec.set(bb->id(), nullptr);
    m_bb_list->remove(bbct);
}


//The function remove labels that nobody referrenced.
bool IRCFG::removeRedundantLabel()
{
    START_TIMER(t, "Remove Redundant Label");
    bool change = false;
    BBListIter it;
    TTab<LabelInfo const*> useful;
    for (IRBB * bb = getBBList()->get_head(&it); bb != nullptr;
         bb = getBBList()->get_next(&it)) {
        IR * last_xr = get_last_xr(bb);
        if (last_xr == nullptr) { continue; }

        LabelInfo const* li = last_xr->getLabel();
        if (li != nullptr) {
            li = useFirstAvailLabel(last_xr, li, this);
            useful.append(li);
            continue;
        }

        if (!last_xr->hasCaseList()) { continue; }

        IR * caselst = last_xr->getCaseList();
        ASSERT0(caselst);
        for (IR * cs = caselst; cs != nullptr; cs = cs->get_next()) {
            ASSERT0(cs->is_case());
            LabelInfo const* li = cs->getLabel();
            li = useFirstAvailLabel(cs, li, this);
            useful.append(li);
        }
    }

    for (IRBB * bb = getBBList()->get_head(&it); bb != nullptr;
         bb = getBBList()->get_next(&it)) {
        LabelInfoListIter liit;
        LabelInfoListIter liit_next;
        LabelInfoList & lst = bb->getLabelList();
        for (lst.get_head(&liit); liit != nullptr; liit = liit_next) {
            liit_next = lst.get_next(liit);
            LabelInfo const* li = liit->val();
            if (!useful.find(li) && !li->hasSideEffect()) {
                lst.remove(liit);
                change = true;
            }
        }
    }
    END_TIMER(t, "Remove Redundant Label");
    return change;
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
    CfgOptCtx ctx(oc);
    CFGOPTCTX_update_dominfo(&ctx) = false;
    removeEmptyBB(ctx);

    //Compute exit BB while CFG rebuilt.
    computeExitList();

    bool change = true;
    UINT count = 0;
    while (change && count < 20) {
        change = false;
        if (g_do_cfg_remove_empty_bb && removeEmptyBB(ctx)) {
            computeExitList();
            change = true;
        }
        if (g_do_cfg_remove_unreach_bb && removeUnreachBB(ctx)) {
            computeExitList();
            change = true;
        }
        if (g_do_cfg_remove_trampolin_bb && removeTrampolinEdge()) {
            computeExitList();
            change = true;
        }
        if (g_do_cfg_remove_unreach_bb && removeUnreachBB(ctx)) {
            computeExitList();
            change = true;
        }
        if (g_do_cfg_remove_trampolin_bb && removeTrampolinBB(ctx)) {
            computeExitList();
            change = true;
        }
        if (g_do_cfg_remove_trampolin_branch && removeTrampolinBranch(ctx)) {
             computeExitList();
             change = true;
        }
        count++;
    }
    ASSERT0(!change);
    ASSERT0(verify());
}


bool IRCFG::verify() const
{
    verifyLabel2BB();
    CFG<IRBB, IR>::verify();
    return true;
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
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpCFG()) {
        dump();
    }

    //Rebuild cfg may generate redundant empty bb, it
    //disturb the computation of entry and exit.
    CfgOptCtx ctx(oc);
    CFGOPTCTX_update_dominfo(&ctx) = false;
    CFGOPTCTX_do_merge_label(&ctx) = oc.do_merge_label();
    removeEmptyBB(ctx);
    computeExitList();
    bool change = true;
    UINT count = 0;
    bool doopt = false;
    while (change && count < 20) {
        change = false;
        if (g_do_cfg_remove_empty_bb && removeEmptyBB(ctx)) {
            computeExitList();
            change = true;
            doopt = true;
        }
        if (g_do_cfg_remove_unreach_bb && removeUnreachBB(ctx)) {
            computeExitList();
            change = true;
            doopt = true;
        }
        if (g_do_cfg_remove_trampolin_bb && removeTrampolinEdge()) {
            computeExitList();
            change = true;
            doopt = true;
        }
        if (g_do_cfg_remove_unreach_bb && removeUnreachBB(ctx)) {
            computeExitList();
            change = true;
            doopt = true;
        }
        if (g_do_cfg_remove_trampolin_bb && removeTrampolinBB(ctx)) {
            computeExitList();
            change = true;
            doopt = true;
        }
        if (g_do_cfg_remove_trampolin_branch && removeTrampolinBranch(ctx)) {
            computeExitList();
            change = true;
            doopt = true;
        }
        count++;
    }
    ASSERT0(!change);
    if (doopt && g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpCFGOpt()) {
        dump();
    }
    ASSERT0(verify());
}


void IRCFG::findTargetBBOfMulticondBranch(IR const* ir,
                                          OUT List<IRBB*> & tgt_bbs)
{
    ASSERT0(ir->isMultiConditionalBr());
    tgt_bbs.clean();
    if (m_bb_list == nullptr) { return; }

    IR * casev_list = SWITCH_case_list(ir);
    if (SWITCH_deflab(ir) != nullptr) {
        IRBB * tbb = findBBbyLabel(SWITCH_deflab(ir));
        ASSERT0(tbb);
        tgt_bbs.append_tail(tbb);
    }

    for (IR * casev = casev_list; casev != nullptr; casev = IR_next(casev)) {
        IRBB * tbb = findBBbyLabel(CASE_lab(casev));
        ASSERT0(tbb);
        tgt_bbs.append_tail(tbb);
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
IRBB * IRCFG::splitBB(IRBB * bb, IRListIter split_point, OptCtx & oc)
{
    IRBB * newbb = m_rg->allocBB();

    //Move rest IRs from bb to newbb.
    for (bb->getIRList().get_next(&split_point); split_point != nullptr;) {
        IRListIter rm = split_point;
        bb->getIRList().get_next(&split_point);
        bb->getIRList().remove(rm);
        ASSERT0(rm->val());
        newbb->getIRList().append_tail(rm->val());
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

    if (!oc.is_rpo_valid() || tryUpdateRPO(newbb, bb, false)) {
        OC_is_rpo_valid(oc) = false;
    }
    if (oc.is_dom_valid()) {
        addDomInfoByNewIPDom(bb->id(), newbb->id());
    }
    return newbb;
}


//Some optimization may append stmt into BB which has down-boundary stmt.
//That makes BB invalid. Split such invalid BB into two or more BBs.
//The function will try to maintain RPO and DOM info.
bool IRCFG::splitBBIfNeeded(IRBB * bb, OptCtx & oc)
{
    IRListIter it;
    for (bb->getIRList().get_head(&it); it != nullptr;) {
        IRListIter cur = it; bb->getIRList().get_next(&it);
        if (IRBB::isLowerBoundary(cur->val()) && it != nullptr) {
            splitBB(bb, cur, oc);
            return true;
        }
    }
    return false;
}

//Try to update RPO of newbb accroding to RPO of marker.
//newbb_prior_marker: true if newbb's lexicographical order is prior to marker.
//Return true if this function find a properly RPO for 'newbb', otherwise
//return false.
bool IRCFG::tryUpdateRPO(IRBB * newbb, IRBB const* marker,
                         bool newbb_prior_marker)
{
    ASSERT0(newbb != marker);
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
                if (predbb == newbb) { continue; }
                if (predbb->rpo() == RPO_UNDEF) {
                    //Exist invalid rpo, recompute them first.
                    return false;
                }
                if (predbb->rpo() >= marker->rpo()) { continue; }
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
                if (succbb == newbb) { continue; }
                if (succbb->rpo() == RPO_UNDEF) {
                    //Exist invalid rpo, recompute them first.
                    return false;
                }
                if (succbb->rpo() <= marker->rpo()) { continue; }
                minsuccrpo = MIN(succbb->rpo(), minsuccrpo);
            }
        }
        rpo = marker->rpo() + 1;
        if (rpo >= minsuccrpo) {
            rpo = RPO_UNDEF;
        }
    }

    //Try to update RPO
    if (rpo != RPO_UNDEF && xcom::Graph::isUsableRPO(rpo)) {
        BB_rpo(newbb) = rpo;
        xcom::Vertex * nv = getVertex(newbb->id());
        ASSERTN(nv, ("newbb should be added to graph first"));
        VERTEX_rpo(nv) = rpo;
        RPOVexList * vlst = getRPOVexList();
        if (vlst != nullptr) {
            if (newbb_prior_marker) {
                vlst->insert_before(getVertex(newbb->id()),
                                    getVertex(marker->id()));
            } else {
                vlst->insert_after(getVertex(newbb->id()),
                                   getVertex(marker->id()));
            }
        }
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


//The function insert a tampolining BB bewteen prev and bb, that will
//make prev no longer fallthrough to bb.
//prev: the previous BB of bb, note prev must fallthrough to bb.
IRBB * IRCFG::insertTrampolinBB(IRBB * prev, MOD IRBB * bb,
                                BBListIter const bbit, MOD BBList * bblst)
{
    BBListIter tmp = bbit;
    ASSERT0(prev == bblst->get_prev(&tmp));
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
    IRBB * tramp_bb = m_rg->allocBB();
    LabelInfo * li = m_rg->genILabel();
    IR * goto_ir = m_rg->buildGoto(li);
    BB_irlist(tramp_bb).append_tail(goto_ir);
    addLabel(bb, li);
    addBB(tramp_bb);
    bblst->insert_after(tramp_bb, prev);

    //Insert a trampolining BB between the previous BB of 'to'
    //that contains a jump IR.
    insertVertexBetween(prev->id(), bb->id(), tramp_bb->id());

    //Now, fallthrough edge bb2->to has been broken, we can insert
    //'newbb' before 'to' correctly.
    return tramp_bb;
}


//Return true if 'prev' is the previous BB of current BB in BBList.
//it: BBListIter of current BB.
bool IRBB::isPrevBB(IRBB const* prev, BBListIter const it,
                    BBList const* bblst) const
{
    BBListIter tmp = it;
    return prev == bblst->get_prev(&tmp);
}


//The function insert newbb bewteen 'from' and 'to'. As a result, the
//function may break up fallthrough edge of 'to' if necessary.
void IRCFG::insertBBbetween(IN IRBB * from, IN BBListIter from_it,
                            IN IRBB * to, IN BBListIter to_it,
                            IN IRBB * newbb)
{
    ASSERT0(from_it->val() == from && to_it->val() == to);
    //Revise BB list, note that 'from' is either fall-through to 'to',
    //or jumping to 'to'.
    BBList * bblst = getBBList();

    addBB(newbb);
    //First, processing edge if 'from'->'to' is fallthrough.
    BBListIter tmp_it = from_it;
    if (from->is_fallthrough() && bblst->get_next(&tmp_it) == to) {
        bblst->insert_after(newbb, from_it);
        insertVertexBetween(from->id(), to->id(), newbb->id());
        return;
    }

    //Second, from->to is jump-edge.
    List<IRBB*> preds;
    get_preds(preds, to);
    ASSERTN(preds.find(from), ("'from' is not pred of 'to'"));
    BBListIter pred_ct = nullptr;
    //Third, find the fallthrough previous BB of 'to'. If find it, insert
    //a trampolining BB between the previous BB of 'to' that contains a jump
    //IR.
    IRBB * tramp_bb = nullptr;
    for (IRBB * pred = preds.get_head(&pred_ct);
         pred != nullptr; pred = preds.get_next(&pred_ct)) {
        if (pred->is_fallthrough() && to->isPrevBB(pred, to_it ,bblst)) {
            tramp_bb = insertTrampolinBB(pred, to, to_it, bblst);
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
    bblst->insert_before(newbb, to_it);
    insertVertexBetween(from->id(), to->id(), newbb->id());
}


//Move all Labels which attached on src BB to tgt BB.
void IRCFG::moveLabels(IRBB * src, IRBB * tgt)
{
    //Remap label2bb.
    LabelInfoListIter it;
    LabelInfoList & lst = src->getLabelList();
    if (lst.get_elem_count() == 0) { return; }

    for (LabelInfo const* li = lst.get_head(&it);
         li != nullptr; li = lst.get_next(&it)) {
        m_lab2bb.setAlways(li, tgt);
    }
    tgt->mergeLabeInfoList(src);

    //Cutoff the relation between src and Labels.
    src->cleanLabelInfoList();
}


void IRCFG::removeSuccDesignatedPhiOpnd(IRBB const* succ, UINT pos)
{
    //Before removing bb or change bb successor,
    //you need remove the related PHI operand if BB 'succ' has PHI stmt.
    PRSSAMgr * prssamgr = getRegion()->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        prssamgr->removeSuccessorDesignatedPhiOpnd(succ, pos);
    }
    MDSSAMgr * mdssamgr = getRegion()->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->removeSuccessorDesignatedPhiOpnd(succ, pos);
    }
}


//Remove related PHI operand from successor BB.
//Before removing current BB or change BB's successor,
//you need remove the related PHI operand if BB successor has PHI.
void IRCFG::removeSuccPhiOpnd(IRBB const* bb)
{
    UINT pos = 0;
    for (EdgeC const* ec = getVertex(bb->id())->getOutList();
         ec != nullptr; ec = ec->get_next(), pos++) {
        ASSERT0(ec->getFromId() == bb->id());
        IRBB * succ = getBB(ec->getToId());
        removeSuccDesignatedPhiOpnd(succ, WhichPred(bb, succ));
    }
}


//Cut off the mapping relation between Labels and BB.
void IRCFG::resetMapBetweenLabelAndBB(IRBB * bb)
{
    LabelInfoListIter it;
    for (LabelInfo const* li = bb->getLabelList().get_head(&it);
         li != nullptr; li = bb->getLabelList().get_next(&it)) {
        m_lab2bb.setAlways(li, nullptr);
    }
    bb->cleanLabelInfoList();
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


//The function remove all MDPhis in 'bb'.
//Note caller should guarrantee phi is useless and removable.
void IRCFG::removeAllMDPhi(IRBB * bb)
{
    MDSSAMgr * mgr = m_rg->getMDSSAMgr();
    if (mgr != nullptr && mgr->is_valid()) {
        mgr->removePhiFromBB(bb);
    }
}


//Before removing current BB or change BB's successor,
//you need remove the related PHI operand if BB successor has PHI.
void IRCFG::removeRelatedSuccBBPhiOpnd(IRBB * bb)
{
    xcom::Vertex * vex = getVertex(bb->id());
    ASSERT0(vex);
    bool update_prssa = false;
    bool update_mdssa = false;
    PRSSAMgr * prssamgr = getRegion()->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        update_prssa = true;
    }
    MDSSAMgr * mdssamgr = getRegion()->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        update_mdssa = true;
    }
    if (!update_prssa && !update_mdssa) { return; }
    for (xcom::EdgeC * out = vex->getOutList();
         out != nullptr; out = out->get_next()) {
        IRBB * succ = getBB(out->getToId());
        ASSERT0(succ);
        UINT pos = WhichPred(bb, succ);
        if (update_prssa) {
            prssamgr->removeSuccessorDesignatedPhiOpnd(succ, pos);
        }
        if (update_mdssa) {
            mdssamgr->removeSuccessorDesignatedPhiOpnd(succ, pos);
        }
    }
}


void IRCFG::removeEdge(IRBB * from, IRBB * to)
{
    removeSuccDesignatedPhiOpnd(to, WhichPred(from, to));
    CFG<IRBB, IR>::removeEdge(from, to);
}


xcom::Edge * IRCFG::addEdge(IRBB * from, IRBB * to)
{
    PRSSAInfoCollect col(m_rg);
    PRSSAMgr * prssamgr = getRegion()->getPRSSAMgr();
    bool useprssa = prssamgr != nullptr && prssamgr->is_valid();
    if (useprssa) {
        col.collect(from, to);
    }

    xcom::Edge * e = DGraph::addEdge(from->id(), to->id());
    //After adding BB or change bb successor, you need to add the related PHI
    //operand as well if the successor of BB has a PHI stmt.
    if (useprssa) {
        prssamgr->addSuccessorDesignatedPhiOpnd(from, to, col);
    }
    MDSSAMgr * mdssamgr = getRegion()->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->addSuccessorDesignatedPhiOpnd(from, to);
    }
    return e;
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
bool IRCFG::removeTrampolinBBCase1(BBListIter * ct, CfgOptCtx const& ctx)
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
    if (vex->getInDegree() > 1) {
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

    //The mapping between Labels and BB has been maintained by above code.
    //resetMapBetweenLabelAndBB(bb);

    removeBB(*ct, ctx);

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
bool IRCFG::removeTrampolinBB(CfgOptCtx const& ctx)
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
            (uncond_br->isIndirectBr() && uncond_br->hasMultiTarget()) ||
            bb->getNumOfIR() != 1) {
            continue;
        }
        removed |= removeTrampolinBBCase1(&ct, ctx);
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
            //CASE: pred->bb, pred fall-through to bb.
            //  pred is:
            //      a=b+1
            //  bb is:
            //      L1:
            //      goto L2
            //=>
            //  pred is:
            //      a=b+1
            //      goto L2
            //  bb is:
            //      L1:
            //      goto L2
            BB_irlist(pred).append_tail(m_rg->dupIRTree(last_xr));
            removeEdge(pred, bb);
            addEdge(pred, succ);
            removed = true;
            continue;
        }

        if (last_xr_of_pred->is_goto()) {
            //CASE: pred->bb, pred fall-through to bb.
            //  pred is:
            //      goto L1
            //  bb is:
            //      L1:
            //      goto L2
            //=>
            //  pred is:
            //      goto L2
            //  bb is:
            //      L1:
            //      goto L2
            ASSERT0(last_xr_of_pred->getLabel() &&
                    findBBbyLabel(last_xr_of_pred->getLabel()) == bb);
            ASSERT0(last_xr_of_pred->getLabel() != nullptr);
            if (succ->id() == bb->id()) {
                //CASE: pred->bb, pred fall-through to bb, and bb's target
                //is itself.
                //  pred is:
                //      goto L1;
                //  bb is:
                //      L1:
                //      goto L1;
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
            // CASE: pred->f, pred->bb, and pred fall-throught to f.
            //  pred is:
            //      truebr/falsebr L1
            //  f is:
            //      ...
            //      ...
            //  bb is:
            //      L1:
            //      goto L2
            //=>
            //  pred is:
            //      truebr/falsebr L2
            //  f is:
            //      ...
            //      ...
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
        if (vex->getInDegree() > 1) {
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
//e.g:BB1:
//    goto BB3;
//    BB2:
//    some-code1;
//    BB3:
//    some-code2;
//  where BB1->BB2->BB3 are fallthrough path.
//stmt of BB2 is just 'goto bb3', then BB1->BB2 is tramp-edge.
//And the resulting edges are BB1->BB3, BB2->BB3 respectively.
//Return true if at least one tramp-edge removed.
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
        if (last_xr->hasSideEffect(true) ||
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
            last_xr->hasSideEffect(true)) {
            continue;
        }

        IR * det = BR_det(last_xr);
        ASSERT0(det != nullptr);
        bool always_true = (det->is_const() && det->is_int() &&
                            CONST_int_val(det) != 0) ||
                            det->is_str();
        bool always_false = det->is_const() && det->is_int() &&
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


bool IRCFG::verifyDomAndPdom(OptCtx const& oc) const
{
    if (oc.is_dom_valid()) {
        if (!DGraph::verifyDom()) {
            return false;
        }
    }
    if (oc.is_pdom_valid()) {
        if (!DGraph::verifyPdom()) {
            return false;
        }
    }
    return true;
}


bool IRCFG::verifyRPO(OptCtx const& oc) const
{
    if (!OC_is_rpo_valid(oc)) { return true; }
    IRCFG * pthis = const_cast<IRCFG*>(this);
    ASSERT0(pthis->getBBList());
    ASSERTN(isRPOValid(), ("Miss RPO info or set rpo invalid in OptCtx"));
    if (pthis->getRPOVexList() != nullptr) {
        ASSERTN(pthis->getRPOVexList()->get_elem_count() ==
                pthis->getBBList()->get_elem_count(),
                ("RPO info need to be fixed or set rpo invalid in OptCtx"));
    }
    return true;
}


void IRCFG::dumpDOT(CHAR const* name, UINT flag) const
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


//Note the function will use \l instead of \n to avoid DOT complaint.
static void dumpDotNode(FILE * h, UINT flag, IRCFG const* cfg)
{
    bool detail = HAVE_FLAG(flag, IRCFG::DUMP_DETAIL);
    bool dump_mdssa = HAVE_FLAG(flag, IRCFG::DUMP_MDSSA);
    MDSSAMgr const* mdssamgr = (MDSSAMgr const*)cfg->getRegion()->getMDSSAMgr();
    if (mdssamgr == nullptr || !mdssamgr->is_valid()) {
        dump_mdssa = false;
    }
    //Print node
    VertexIter c = VERTEX_UNDEF;
    for (xcom::Vertex * v = cfg->get_first_vertex(c);
         v != nullptr; v = cfg->get_next_vertex(c)) {
        CHAR const* shape = "box";
        CHAR const* font = "courB";
        CHAR const* color = "black";
        CHAR const* style = "bold";
        UINT fontsize = 12;
        IRBB * bb = cfg->getBB(v->id());
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

            dumpBBLabel(bb->getLabelList(), cfg->getRegion());
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
                    dumpIR(ir, cfg->getRegion(), nullptr, IR_DUMP_KID);
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
}


//Note the function will use \l instead of \n to avoid DOT complaint.
static void dumpDotEdge(FILE * h, UINT flag, IRCFG const* cfg)
{
    bool dump_eh = HAVE_FLAG(flag, IRCFG::DUMP_EH);
    VertexIter c2 = VERTEX_UNDEF;
    TTab<xcom::Edge const*> visited;
    for (xcom::Vertex * v = cfg->get_first_vertex(c2);
         v != nullptr; v = cfg->get_next_vertex(c2)) {
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
    for (xcom::Edge const* e = cfg->get_first_edge(ite);
         e != nullptr;  e = cfg->get_next_edge(ite)) {
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
}


void IRCFG::dumpDOT(FILE * h, UINT flag) const
{
    if (!getRegion()->isLogMgrInit() || h == nullptr) { return; }
    getRegion()->getLogMgr()->push(h, "");

    bool detail = HAVE_FLAG(flag, DUMP_DETAIL);
    if (detail) {
        //Print comment
        note(getRegion(), "\n/******************************");
        dumpBBList(m_bb_list, m_rg);
        note(getRegion(), "\n******************************/\n");
    }

    //Start to dump DOT.
    note(getRegion(), "digraph G {\n");

    //fprintf(h, "rankdir=LR;\n"); //Layout from Left to Right.
    //fprintf(h, "rankdir=TB;\n");
    //fprintf(h, "rankdir=BT;\n");

    //Print Region name.
    note(getRegion(),
         "\nstartnode [fontsize=24,style=filled, "
         "color=gold,shape=none,label=\"RegionName:%s\"];",
         m_rg->getRegionName());

    //Print carriage-return for DOT file, use \l instead of \n to
    //avoid DOT complaint.
    bool org_carr = getRegion()->getLogMgr()->isReplaceNewline();
    getRegion()->getLogMgr()->setReplaceNewline(true);
    dumpDotNode(h, flag, this);
    dumpDotEdge(h, flag, this);
    getRegion()->getLogMgr()->pop();
    getRegion()->getLogMgr()->setReplaceNewline(org_carr);
    fprintf(h,"\n}\n");
    fflush(h);
}


static void dumpVCGNodeWithDetail(CHAR const* shape, CHAR const* font,
                                  CHAR const* color,
                                  UINT vertical_order,
                                  INT scale,
                                  UINT flag,
                                  IRBB const* bb,
                                  IRCFG const* cfg)
{
    bool dump_mdssa = HAVE_FLAG(flag, IRCFG::DUMP_MDSSA);
    MDSSAMgr const* mdssamgr = (MDSSAMgr const*)cfg->getRegion()->getMDSSAMgr();
    if (mdssamgr == nullptr || !mdssamgr->is_valid()) {
        dump_mdssa = false;
    }
    FILE * h = cfg->getRegion()->getLogMgr()->getFileHandler();
    ASSERT0(h);
    fprintf(h,
            "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
            "fontname:\"%s\" scaling:%d label:\"",
            bb->id(), vertical_order++, shape, color, font, scale);
    fprintf(h, "   BB%d ", bb->rpo());
    fprintf(h, " rpo:%d ", bb->rpo());
    dumpBBLabel(const_cast<IRBB*>(bb)->getLabelList(), cfg->getRegion());
    fprintf(h, "\n");

    //Dump MDSSA Phi List.
    if (dump_mdssa) {
        MDPhiList const* philist = mdssamgr->getPhiList(bb);
        if (philist != nullptr) {
            mdssamgr->dumpPhiList(philist);
        }
    }

    //Dump IR list.
    for (IR * ir = BB_first_ir(const_cast<IRBB*>(bb));
         ir != nullptr; ir = BB_next_ir(const_cast<IRBB*>(bb))) {
        //fprintf(h, "%s\n", dump_ir_buf(ir, buf));

        //TODO: implement dump_ir_buf();
        if (dump_mdssa) {
            mdssamgr->dumpIRWithMDSSA(ir, IR_DUMP_KID);
        } else {
            dumpIR(ir, cfg->getRegion(), nullptr, IR_DUMP_KID);
        }
    }

    fprintf(h, "\"}");
    fflush(h);
}


static void dumpVCGNode(UINT flag, IRCFG const* cfg)
{
    bool detail = HAVE_FLAG(flag, IRCFG::DUMP_DETAIL);
    FILE * h = cfg->getRegion()->getLogMgr()->getFileHandler();
    ASSERT0(h);
    UINT vertical_order = 1;
    VertexIter c;
    for (xcom::Vertex const* v = cfg->get_first_vertex(c);
        v != nullptr; v = cfg->get_next_vertex(c)) {
        IRBB * bb = cfg->getBB(v->id());
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
            dumpVCGNodeWithDetail(shape, font, color, vertical_order, scale,
                                  flag, bb, cfg);
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
static void dumpVCGHead(FILE * h)
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


static void dumpVCGEdge(UINT flag, IRCFG const* cfg)
{
    bool dump_eh = HAVE_FLAG(flag, IRCFG::DUMP_EH);
    FILE * h = cfg->getRegion()->getLogMgr()->getFileHandler();
    ASSERT0(h);
    VertexIter c2;
    TTab<xcom::Edge const*> visited;
    UINT pos = 0;
    for (xcom::Vertex * v = cfg->get_first_vertex(c2);
         v != nullptr; v = cfg->get_next_vertex(c2), pos++) {
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
    for (xcom::Edge * e = cfg->get_first_edge(c);
         e != nullptr; e = cfg->get_next_edge(c)) {
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


void IRCFG::dumpVCG(CHAR const* name, UINT flag) const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    ASSERT0(m_rg);
    if (name == nullptr) { name = "graph_cfg.vcg"; }

    //Note this function does not use LogMgr as output.
    //So it is dispensable to check LogMgr.
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h != nullptr, ("%s create failed!!!",name));
    dumpVCGHead(h);

    //Start to dump VCG.
    //Print Region name.
    fprintf(h,
            "\nnode: {title:\"\" vertical_order:0 shape:box color:turquoise "
            "borderwidth:0 fontname:\"Courier Bold\" "
            "scaling:2 label:\"RegionName:%s\" }",
            m_rg->getRegionName());
    getRegion()->getLogMgr()->push(h, "");
    dumpVCGNode(flag, this);
    dumpVCGEdge(flag, this);
    getRegion()->getLogMgr()->pop();
    fprintf(h, "\n}\n");
    fclose(h);
}


bool IRCFG::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    dumpDOT(getRegion()->getLogMgr()->getFileHandler(), DUMP_COMBINE);
    m_rg->getLogMgr()->decIndent(2);
    if (g_dump_opt.isDumpDOM()) {
        dumpDom();
    }
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


//Record the Exit BB here.
void IRCFG::computeExitList()
{
    //Clean the Exit flag.
    BBListIter ct = nullptr;
    for (m_exit_list.get_head(&ct);
         ct != m_exit_list.end();
         ct = m_exit_list.get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        BB_is_exit(bb) = false;
    }

    CFG<IRBB, IR>::computeExitList();

    //Record the Exit flag as BB attribute to speed up accessing.
    for (m_exit_list.get_head(&ct);
         ct != m_exit_list.end();
         ct = m_exit_list.get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        BB_is_exit(bb) = true;
    }
}


void IRCFG::computeDomAndIdom(MOD OptCtx & oc, xcom::BitSet const* uni)
{
    if (getBBList()->get_elem_count() == 0) { return; }

    DUMMYUSE(uni);
    START_TIMER(t, "Compute Dom, IDom");
    ASSERT0(oc.is_cfg_valid());
    ASSERTN(m_entry, ("ONLY support SESE or SEME"));

    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    RPOVexList * vlst = getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == m_rg->getBBList()->get_elem_count());
    //xcom::DGraph::computeDom(&vlst, uni);
    //xcom::DGraph::computeIdom();
    bool f = xcom::DGraph::computeIdom2(*vlst);
    DUMMYUSE(f);
    ASSERT0(f);

    f = xcom::DGraph::computeDom2(*vlst);
    DUMMYUSE(f);
    ASSERT0(f);

    OC_is_dom_valid(oc) = true;
    END_TIMER(t, "Compute Dom, IDom");
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpDOM()) {
        note(m_rg, "\n==---- DUMP DOM&IDOM IN IRCFG ----==");
        dumpDom();
    }
}


static void dumpRPO(Region const* rg, BBList const* bblst)
{
    note(rg, "\n==---- DUMP RPO ----==");
    BBListIter it;
    for (IRBB const* bb = bblst->get_head(&it);
         bb != nullptr; bb = bblst->get_next(&it)) {
        note(rg, "\n-- BB%d -- rpo:%d --", bb->id(), bb->rpo());
    }
}


void IRCFG::computeRPO(OptCtx & oc)
{
    CFG<IRBB, IR>::computeRPO(oc);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRPO()) {
        dumpRPO(m_rg, getBBList());
    }
}


void IRCFG::computePdomAndIpdom(MOD OptCtx & oc, xcom::BitSet const* uni)
{
    if (getBBList()->get_elem_count() == 0) { return; }
    START_TIMER(t, "Compute PDom,IPDom");
    ASSERT0(oc.is_cfg_valid());

    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    RPOVexList * vexlst = getRPOVexList();
    ASSERT0(vexlst);
    ASSERT0(vexlst->get_elem_count() == m_rg->getBBList()->get_elem_count());

    List<xcom::Vertex const*> vlst;
    for (Vertex const* v = vexlst->get_tail(); v != nullptr;
         v = vexlst->get_prev()) {
        ASSERT0(v->id() != VERTEX_UNDEF);
        vlst.append_tail(v);
    }
    bool f = false;
    if (uni != nullptr) {
        f = xcom::DGraph::computePdom(vlst, uni);
    } else {
        f = xcom::DGraph::computePdom(vlst);
    }
    DUMMYUSE(f);
    ASSERT0(f);

    f = xcom::DGraph::computeIpdom();
    ASSERT0(f);

    OC_is_pdom_valid(oc) = true;
    END_TIMER(t, "Compute PDom,IPDom");
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpDOM()) {
        note(m_rg, "\n==---- DUMP PDOM&IPDOM IN IRCFG ----==");
        dumpDom();
    }
}


void IRCFG::remove_xr(IRBB * bb, IR * ir)
{
    ASSERT0(ir->is_stmt());
    removeStmt(ir, m_rg);
    ir = BB_irlist(bb).remove(ir);
    m_rg->freeIRTree(ir);
}


bool IRCFG::removeTrampolinBranchForBB(BBListIter & it, CfgOptCtx const& ctx)
{
    IRBB * bb = it->val();
    if (bb->isExceptionHandler()) { return false; }

    IR * br = get_last_xr(bb);
    if (br == nullptr || !br->isConditionalBr() ||
        br->hasSideEffect(true)) {
        return false;
    }

    BBListIter nextbbit = it;
    IRBB * next = m_bb_list->get_next(&nextbbit);
    IR * jmp = nullptr;
    if (next == nullptr || //bb may be the last BB in bb-list.
        (jmp = get_first_xr(next)) == nullptr || //bb can not be empty
        !jmp->is_goto()) { //the only IR must be GOTO
        return false;
    }

    if (next->isExceptionHandler()) { return false; }

    //Get br's target BB, it should be the next of next.
    IRBB * nextnext = m_bb_list->get_next(&nextbbit);
    if (nextnext == nullptr || !nextnext->hasLabel(BR_lab(br))) {
        //Label is not suited to the case.
        return false;
    }

    IRBB * jmp_tgt = findBBbyLabel(GOTO_lab(jmp));
    xcom::Edge const* e_of_jmp = getEdge(next->id(), jmp_tgt->id());
    ASSERT0(e_of_jmp);
    CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e_of_jmp);
    if (ei != nullptr && CFGEI_is_eh(ei)) {
        //Do not remove exception edge.
        return false;
    }

    xcom::Edge const* e_of_bb = getEdge(bb->id(), nextnext->id());
    ASSERT0(e_of_bb);
    CFGEdgeInfo * ei2 = (CFGEdgeInfo*)EDGE_info(e_of_bb);
    if (ei2 != nullptr && CFGEI_is_eh(ei2)) {
        //Do not remove exception edge.
        return false;
    }

    //Begin to displace.
    //Change bb outedge. Make 'next' to empty BB.
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

    //Add next->nextnext, actually jmp->nextnext because 'next' is empty.
    xcom::Edge * newe = addEdge(next, nextnext);
    EDGE_info(newe) = ei;

    //Remove bb->nextnext.
    removeEdge(bb, nextnext);

    //Add bb->jmp_tgt.
    xcom::Edge * newe2 = addEdge(bb, jmp_tgt);
    EDGE_info(newe2) = ei2;

    if (ctx.oc.is_dom_valid()) {
        //Maintain DOM info.
        //e.g:
        //  --BB26
        // |  |
        // |  v
        // |  BB27-----
        // |           |
        // -> BB29--   |
        //    |     |  |
        //    v     |  |
        //    BB30  |  |
        //    |    _|  |
        //    |   /    |
        //    v  v     |
        //    BB34 <---
        //After removing:
        //    BB26-----
        //    |        |
        //    v        |
        //    BB27     |
        //    |        |
        //    v        |
        //    BB29--   |
        //    |     |  |
        //    v     |  |
        //    BB30  |  |
        //    |    _|  |
        //    |   /    |
        //    v  v     |
        //    BB34 <---
        ASSERT0(next && nextnext);
        set_ipdom(next->id(), nextnext->id());
        set_idom(nextnext->id(), next->id());
        get_dom_set(nextnext->id())->bunion(next->id()); 
        get_pdom_set(next->id())->diff(jmp_tgt->id());
    }
    return true;
}


//Remove trampoline branch.
//Note the pass is different from what removeTrampolinEdge() does.
//e.g:L2:
//    truebr L4 | false L4
//    goto L3 //redundant jump
//    L4
//    st = ...
//    L3:
//    ...
//=>
//    L2:
//    falsebr L3 | truebr L3
//    EMPTY BB
//    L4:
//    st = ...
//    L3:
bool IRCFG::removeTrampolinBranch(CfgOptCtx const& ctx)
{
    bool changed = false;
    BBListIter it;
    for (IRBB * bb = m_bb_list->get_head(&it);
         bb != nullptr; bb = m_bb_list->get_next(&it)) {
        changed |= removeTrampolinBranchForBB(it, ctx);
    }
    return changed;
}


//Fix out-edges if BB becomes fallthrough BB.
//The function will remove out-edges of bb except the fallthrough edge, and
//try adding fallthrough edge if it doesn't exist.
//Note it is illegal if empty BB has non-taken branch.
void IRCFG::reviseOutEdgeForFallthroughBB(IRBB * bb, BBListIter & bbit)
{
    ASSERT0(bb && bbit);
    xcom::EdgeC * ec = getVertex(bb->id())->getOutList();
    if (ec == nullptr) { return; }

    ASSERT0(!bb->is_empty());
    BBListIter next_ct = bbit;
    BBList * bbl = getBBList();
    bbl->get_next(&next_ct);
    IRBB * next_bb = nullptr;
    if (next_ct != nullptr) {
        next_bb = next_ct->val();
    }

    bool has_fallthrough = false;
    xcom::EdgeC * next_ec = nullptr;
    for (; ec != nullptr; ec = next_ec) {
        next_ec = ec->get_next();

        xcom::Edge * e = ec->getEdge();
        if (e->info() != nullptr && CFGEI_is_eh((CFGEdgeInfo*)e->info())) {
            continue;
        }

        IRBB * succ = getBB(e->to()->id());
        ASSERT0(succ);
        if (succ == next_bb) {
            //Keep the fall-throught edge unchanged.
            has_fallthrough = true;
            ec = ec->get_next();
            continue;
        }

        //Note removeEdge() will update PHI, whereas in-edge of succ changed,
        //and operands of phi maintained as well.
        removeEdge(bb, succ);
    }

    //Add edge between bb and next_bb if fallthrough edge miss.
    //e.g: Add edge BB->BB_next.
    //      _______
    //     |       |
    //     V       |
    //   _BB       |
    //  |  |       |
    //  |  V       |
    //  | BB_next  |
    //  |  |_______|
    //  |
    //   ->BB5
    if (next_bb != nullptr && !has_fallthrough) {
        //Note the function will add operands of PHI if 'next_bb' has PHI.
        addEdge(bb, next_bb);
    }
}


//Perform miscellaneous control flow optimizations.
//Return true if CFG changed.
//Include removing dead bb which is unreachable, removing empty bb as many
//as possible, simplify and remove the branch like "if (x==x)", removing
//the trampolin branch.
//Note these optimizations performed in the function should NOT use DOM, PDOM,
//LOOPINFO, or RPO information.
bool IRCFG::performMiscOpt(CfgOptCtx const& ctx)
{
    START_TIMER(t, "CFG Optimizations");
    bool change = false;
    bool lchange = false;
    UINT count = 0;
    OptCtx org_oc(ctx.oc);
    do {
        lchange = false;
        if (g_do_cfg_remove_unreach_bb) {
            bool res = removeUnreachBB(ctx);
            lchange |= res;
        }
        if (g_do_cfg_remove_redundant_label) {
            bool res = removeRedundantLabel();
            lchange |= res;
        }
        if (g_do_cfg_remove_empty_bb) {
            bool res = removeEmptyBB(ctx);
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
        if (g_do_cfg_remove_trampolin_branch) {
            bool res = removeTrampolinBranch(ctx);
            lchange |= res;
        }
        if (lchange) {
            ctx.oc.setInvalidIfCFGChanged();
        }
        change |= lchange;
        count++;
    } while (lchange && count < 1000);
    ASSERTN(!lchange, ("CFG optimization iterated too many times."));
    if (change) {
        computeExitList();
        if (org_oc.is_cdg_valid()) {
            m_rg->getPassMgr()->checkValidAndRecompute(&ctx.oc, PASS_CDG,
                                                       PASS_UNDEF);
            ASSERT0(verifyIfBBRemoved((CDG*)m_rg->getPassMgr()->
                                      queryPass(PASS_CDG), ctx.oc));
        }

        //SSAInfo is invalid by adding new-edge to BB.
        //This will confuse phi insertion.
        //CFG optimization should maintain Phi information.
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg));
        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpCFGOpt()) {
            dump();
        }
        ASSERT0(verifyIRandBB(getBBList(), m_rg));
        ASSERT0(verifyRPO(ctx.oc));
        ASSERT0(verifyDomAndPdom(ctx.oc));
    }
    END_TIMER(t, "CFG Optimizations");
    return change;
}

} //namespace xoc
