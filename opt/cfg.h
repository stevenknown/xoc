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
#ifndef _CFG_H_
#define _CFG_H_

namespace xoc {

//Sort Sequence
typedef enum {
    SEQ_UNDEF = 0,
    SEQ_DFS,   //depth first sort
    SEQ_BFS,   //breadth first sort
    SEQ_TOPOL,   //topological sort
} SEQ_TYPE;


//CFG Shape
typedef enum {
    C_UNDEF = 0,
    C_SESE,    //single entry, single exit
    C_SEME,    //single entry, multi exit
} CFG_SHAPE;


//CFG xcom::Edge Info.
#define CFGEI_is_eh(ei)    ((ei)->is_eh)
class CFGEdgeInfo {
public:
    BYTE is_eh:1; //true if edge describe eh edge.
};


//NOTICE:
//1. For accelerating perform operation of each vertex, e.g
//   compute dominator, please try best to add vertex with
//   topological order.
template <class BB, class XR> class CFG : public xcom::DGraph {
protected:
    LI<BB> * m_loop_info; //Loop information
    List<BB*> * m_bb_list;
    Vector<LI<BB>*> m_map_bb2li; //map bb to LI if bb is loop header.
    BB * m_entry; //CFG Graph entry.
    List<BB*> m_exit_list; //CFG Graph ENTRY list
    List<BB*> m_rpo_bblst; //record BB in reverse-post-order.
    SEQ_TYPE m_bb_sort_type;
    xcom::BitSetMgr * m_bs_mgr;
    SMemPool * m_pool;
    UINT m_li_count; //counter to loop.
    BYTE m_has_eh_edge:1;

protected:
    //Collect loop info e.g: loop has call, loop has goto.
    void collectLoopInfo() { collectLoopInfoRecur(m_loop_info); }

    //Do clean before recompute loop info.
    void cleanLoopInfo(bool access_li_by_scan_bb = false);
    void computeRPOImpl(xcom::BitSet & is_visited, IN xcom::Vertex * v,
        OUT INT & order);
    inline void collectLoopInfoRecur(LI<BB> * li);

    inline bool isLoopHeadRecur(LI<BB> * li, BB * bb);
    bool insertLoopTree(LI<BB> ** lilist, LI<BB> * loop);

    void dumpLoopTree(LI<BB> * looplist, UINT indent, FILE * h);

    bool removeRedundantBranchCase1(
            BB *RESTRICT bb,
            BB const*RESTRICT next_bb,
            XR * xr);
    void removeUnreachableBB(UINT id, xcom::BitSet & visited);
    void removeUnreachableBB2(UINT id, xcom::BitSet & visited);
    void sortByDFSRecur(List<BB*> & new_bbl, BB * bb,
        Vector<bool> & visited);

    void * xmalloc(size_t size)
    {
        ASSERT0(m_pool);
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }
public:
    CFG(List<BB*> * bb_list,
        UINT edge_hash_size = 16,
        UINT vertex_hash_size = 16)
        : xcom::DGraph(edge_hash_size, vertex_hash_size)
    {
        ASSERT(bb_list, ("CFG need BB list"));
        m_bb_list = bb_list;
        m_loop_info = NULL;
        m_bs_mgr = NULL;
        m_li_count = 1;
        m_entry = NULL; //entry will be computed during CFG::build().
        m_pool = smpoolCreate(sizeof(CFGEdgeInfo) * 4, MEM_COMM);
        set_dense(true); //We think CFG is always dense graph.
    }
    COPY_CONSTRUCTOR(CFG);
    virtual ~CFG() { smpoolDelete(m_pool); }

    virtual void addBreakOutLoop(BB * loop_head, xcom::BitSet & body_set);
    void rebuild(OptCtx & oc)
    {
        erase();
        UINT newsz = MAX(16, getNearestPowerOf2(m_bb_list->get_elem_count()));
        resize(newsz, newsz);
        build(oc);

        //One should call removeEmptyBB() immediately after this function,
        //because rebuilding cfg may generate redundant empty bb, it
        //disturb the computation of entry and exit.
    }

    //Build the CFG accroding to BB list.
    void build(OptCtx & oc);

    bool computeDom(xcom::BitSet const* uni)
    {
        ASSERT(m_entry, ("Not found entry"));
        List<xcom::Vertex const*> vlst;
        computeRpoNoRecursive(get_vertex(m_entry->id()), vlst);
        return xcom::DGraph::computeDom(&vlst, uni);
    }

    //CFG may have multiple exit. The method computing pdom is not
    //different with dom.
    bool computePdom(xcom::BitSet const* uni)
    {
        //ASSERT(m_exit_list.get_elem_count() == 1,
        //   ("ONLY support SESE or SEME"));
        ASSERT(m_entry, ("Not found entry"));
        return xcom::DGraph::computePdomByRpo(get_vertex(m_entry->id()), uni);
    }

    //Compute all reachable BBs start from 'startbb'.
    //bbset: record the output result.
    //Note caller should clean bbset.
    void computeReachableBBSet(BB * startbb, OUT xcom::BitSet & bbset)
    {
        ASSERT0(startbb);
        List<xcom::Vertex const*> wl;
        UINT id = startbb->id();
        ASSERT0(get_vertex(id));
        wl.append_tail(get_vertex(id));

        xcom::Vertex const* v;
        while ((v = wl.remove_head()) != NULL) {
            UINT id = VERTEX_id(v);
            if (bbset.is_contain(id)) { continue; }

            bbset.bunion(id);
            for (xcom::EdgeC * el = VERTEX_out_list(v);
                 el != NULL; el = EC_next(el)) {
                xcom::Vertex const* succv = EDGE_to(EC_edge(el));
                if (!bbset.is_contain(VERTEX_id(succv))) {
                    wl.append_tail(succv);
                }
            }
        }
    }

    //Compute all reachable BBs start from 'startbb'.
    //bbset: record the output result.
    //Note caller should clean bbset.
    //This function is different to computeReachableBBSet, it only
    //collect BBs in main stream control flow, BBs which in
    //exception handler region are omitted.
    void computeMainStreamBBSet(BB * startbb, OUT xcom::BitSet & bbset)
    {
        ASSERT0(startbb);
        List<xcom::Vertex const*> wl;
        UINT id = startbb->id();
        ASSERT0(get_vertex(id));
        wl.append_tail(get_vertex(id));

        xcom::Vertex const* v;
        while ((v = wl.remove_head()) != NULL) {
            UINT i = VERTEX_id(v);
            if (bbset.is_contain(i) || getBB(i)->isExceptionHandler()) {
                continue;
            }
            bbset.bunion(i);

            for (xcom::EdgeC * el = VERTEX_out_list(v);
                 el != NULL; el = EC_next(el)) {
                xcom::Vertex const* succv = EDGE_to(EC_edge(el));
                UINT sid = VERTEX_id(succv);
                if (!bbset.is_contain(sid) &&
                    !getBB(sid)->isExceptionHandler()) {
                    wl.append_tail(succv);
                }
            }
        }
    }

    //Compute and record exit BBs.
    //Only the function entry can be CFG entry.
    //Connect or handle try and catch BB before perform
    //removing unreachable BB.
    virtual void computeExitList()
    {
        m_exit_list.clean();

        xcom::C<BB*> * ct;
        for (m_bb_list->get_head(&ct);
             ct != NULL; ct = m_bb_list->get_next(ct)) {
            BB * bb = ct->val();
            ASSERT0(bb);

            ASSERT(get_vertex(bb->id()),
                ("No vertex corresponds to BB%d", bb->id()));

            if (isCFGExit(bb->id())) {
                m_exit_list.append_tail(bb);
            }
        }
    }

    void chainPredAndSucc(UINT vid, bool is_single_pred_succ = false);
    void computeRPO(OptCtx & oc);
    UINT count_mem() const
    {
        UINT count = sizeof(*this);
        //count += m_bb_list.count_mem(); //do NOT count up BBs in bb_list.
        count += m_map_bb2li.count_mem();
        count += m_exit_list.count_mem();
        return count;
    }

    virtual void dump_loop(FILE * h)
    {
        if (h == NULL) return;
        fprintf(h, "\n==---- DUMP Natural Loop Tree ----==");
        dumpLoopTree(m_loop_info, 0, h);
        fflush(h);
    }
    virtual void dump_vcg(CHAR const* name = NULL);

    bool findLoop();

    //Find the target bb list.
    //2th parameter indicates a list of bb have found.
    virtual void findTargetBBOfMulticondBranch(XR const*,
        OUT List<BB*> &) = 0;

    //Find the bb that referred given label.
    virtual BB * findBBbyLabel(LabelInfo const*) = 0;

    //Find a list bb that referred labels which is the target of xr.
    //2th parameter indicates a list of bb have found.
    virtual void findTargetBBOfIndirectBranch(XR const*,
        OUT List<BB*> &) = 0;

    UINT getLoopNum() const { return m_li_count - 1; }
    void get_preds(IN OUT List<BB*> & preds, BB const* v);
    void get_succs(IN OUT List<BB*> & succs, BB const* v);
    BB * get_entry() { return m_entry; }
    List<BB*> * getExitList() { return &m_exit_list; }
    List<BB*> * getBBListInRPO() { return &m_rpo_bblst; }
    virtual BB * getFallThroughBB(BB * bb)
    {
        ASSERT0(bb);
        xcom::C<BB*> * ct;
        ASSERT0(m_bb_list->find(bb, &ct));
        m_bb_list->find(bb, &ct);
        BB * res = m_bb_list->get_next(&ct);
        return res;
    }

    //Get the target bb related to the last xr of bb.
    virtual BB * getTargetBB(BB * bb)
    {
        ASSERT0(bb);
        XR * xr = get_last_xr(bb);
        ASSERT(xr != NULL, ("bb is empty"));
        LabelInfo const* lab = xr->getLabel();
        ASSERT(lab != NULL, ("xr does not correspond to a unqiue label"));
        BB * target = findBBbyLabel(lab);
        ASSERT(target != NULL, ("label does not correspond to a BB"));
        return target;
    }

    //Get the first successor of bb.
    BB * get_first_succ(BB const* bb) const
    {
        ASSERT0(bb);
        xcom::Vertex * vex = get_vertex(bb->id());
        ASSERT0(vex);

        xcom::EdgeC * ec = VERTEX_out_list(vex);
        if (ec == NULL) { return NULL; }

        BB * succ = getBB(VERTEX_id(EDGE_to(EC_edge(ec))));
        ASSERT0(succ);
        return succ;
    }

    BB * get_idom(BB * bb)
    {
        ASSERT0(bb != NULL);
        return getBB(xcom::DGraph::get_idom(bb->id()));
    }

    BB * get_ipdom(BB * bb)
    {
        ASSERT0(bb != NULL);
        return getBB(xcom::DGraph::get_ipdom(bb->id()));
    }

    LI<BB> * getLoopInfo() { return m_loop_info; }
    void getKidOfIF(
            BB * bb,
            BB ** true_body,
            BB ** false_body,
            BB ** sibling);
    void getKidOfLoop(IN BB * bb, OUT BB ** sibling, OUT BB ** body_root);

    //Return the last instruction of BB.
    virtual XR * get_last_xr(BB *) = 0;

    //Return the first instruction of BB.
    virtual XR * get_first_xr(BB *) = 0;
    virtual BB * getBB(UINT id) const = 0;

    //True if has exception handler edge.
    bool hasEHEdge() const { return m_has_eh_edge; }

    void identifyNaturalLoop(
            UINT x,
            UINT y,
            IN OUT xcom::BitSet & loop,
            List<UINT> & tmp);

    bool isCFGEntry(UINT bbid)
    { return xcom::Graph::is_graph_entry(get_vertex(bbid)); }

    //Return true if bb is exit BB of CFG.
    bool isCFGExit(UINT bbid)
    { return xcom::Graph::is_graph_exit(get_vertex(bbid)); }

    //Return true if bb is entry BB of function.
    //In some case, BB is not Region entry even if it is the CFG entry.
    virtual bool isRegionEntry(BB *) = 0;

    //Return true if bb is exit BB of function.
    //In some case, BB is not Region exit even if it is the CFG exit.
    virtual bool isRegionExit(BB *) = 0;

    virtual bool isLoopHead(BB * bb) { return isLoopHeadRecur(m_loop_info, bb); }

    LI<BB> * mapBB2LabelInfo(BB * bb) { return m_map_bb2li.get(bb->id()); }

    //Move all Labels which attached on src BB to tgt BB.
    virtual void moveLabels(BB * src, BB * tgt) = 0;
    virtual void resetMapBetweenLabelAndBB(BB * bb) = 0;

    //Remove xr that in bb.
    virtual void remove_xr(BB *, XR *) = 0;

    //You should clean the relation between Label and BB before remove BB.
    virtual void remove_bb(BB * bb) = 0;
    virtual void remove_bb(xcom::C<BB*> * bbcontainer) = 0;

    void removeEdge(BB * from, BB * to)
    {
        xcom::Edge * e = xcom::Graph::get_edge(from->id(), to->id());
        ASSERT0(e != NULL);
        xcom::Graph::removeEdge(e);
    }

    bool removeEmptyBB(OptCtx & oc);
    bool removeUnreachBB();
    bool removeRedundantBranch();

    //Insert unconditional branch to revise fall through bb.
    //e.g: Given bblist is bb1-bb2-bb3-bb4, bb4 is exit-BB,
    //and flow edges are: bb1->bb2->bb3->bb4, bb1->bb3,
    //where bb1->bb2, bb2->bb3, bb3->bb4 are fallthrough edge.
    //
    //Assuming the reordered bblist is bb1-bb3-bb4-bb2, the
    //associated flow edges are
    //bb1->bb3->bb4, bb2->bb3.
    //It is obviously that converting bb1->bb3 to be fallthrough,
    //and converting bb1->bb2 to be conditional branch, converting
    //bb2->bb3 to be unconditional branch.
    void revise_fallthrough(List<BB*> & new_bbl)
    { ASSERT(0, ("Target Dependent Code")); }

    void sortByDFS();
    void sortByBFS();
    void sortByTopological();

    void setBitSetMgr(xcom::BitSetMgr * bs_mgr)
    {
        m_bs_mgr = bs_mgr;
        xcom::DGraph::setBitSetMgr(bs_mgr);
    }

    //Set RPO for BB.
    virtual void setRPO(BB * bb, INT order) = 0;

    bool verifyIfBBRemoved(IN CDG * cdg, OptCtx & oc);
    bool verify()
    {
        //The entry node can not have any predecessors.
        ASSERT0(m_entry);
        xcom::Vertex * vex = get_vertex(m_entry->id());
        CHECK_DUMMYUSE(vex && get_in_degree(vex) == 0);

        //The exit node can not have successors.
        for (BB * bb = m_exit_list.get_head();
             bb != NULL; bb = m_exit_list.get_next()) {
            xcom::Vertex * vex2 = get_vertex(bb->id());
            CHECK_DUMMYUSE(vex2 && get_out_degree(vex2) == 0);
        }
        return true;
    }
};


//Find and Return LOOP_SIBLING and BODY_ROOT.
//e.g:
//    LOOP
//        BODY_ROOT
//    END_LOOP
//    LOOP_SIBLING
template <class BB, class XR>
void CFG<BB, XR>::getKidOfLoop(
        IN BB * bb,
        OUT BB ** sibling,
        OUT BB ** body_root)
{
    LI<BB> * li = mapBB2LabelInfo(bb);
    ASSERT0(li != NULL && LI_loop_head(li) == bb);
    List<BB*> succs;
    get_succs(succs, bb);
    ASSERT0(succs.get_elem_count() == 2);
    BB * s = succs.get_head();
    if (sibling != NULL) {
        *sibling = li->is_inside_loop(s->id()) ? succs.get_tail() : s;
    }

    if (body_root != NULL) {
        *body_root = li->is_inside_loop(s->id()) ? s : succs.get_tail();
    }
}


//Find and Return TRUE_BODY, FALSE_BODY, IF_SIBLING.
//e.g:
//    IF
//        TRUE_BODY
//    ELSE
//        FALSE_BODY
//    END_IF
//    IF_SIBLING
template <class BB, class XR>
void CFG<BB, XR>::getKidOfIF(
        BB * bb,
        BB ** true_body,
        BB ** false_body,
        BB ** sibling)
{
    if (true_body != NULL || false_body != NULL) {
        UINT ipdom = xcom::DGraph::get_ipdom(bb->id());
        ASSERT(ipdom > 0, ("bb does not have ipdom"));
        BB * fallthrough_bb = getFallThroughBB(bb);
        BB * target_bb = getTargetBB(bb);
        XR * xr = get_last_xr(bb);
        ASSERT0(xr != NULL && xr->isConditionalBr());
        if (xr->is_truebr()) {
            if (true_body != NULL) {
                if (ipdom == target_bb->id()) {
                    *true_body = NULL;
                } else {
                    *true_body = target_bb;
                }
            }
            if (false_body != NULL) {
                if (ipdom == fallthrough_bb->id()) {
                    *false_body = NULL;
                } else {
                    *false_body = fallthrough_bb;
                }
            }
        } else {
            ASSERT0(xr->is_falsebr());
            if (true_body != NULL) {
                if (ipdom == fallthrough_bb->id()) {
                    *true_body = NULL;
                } else {
                    *true_body = fallthrough_bb;
                }
            }
            if (false_body != NULL) {
                if (ipdom == target_bb->id()) {
                    *false_body = NULL;
                } else {
                    *false_body = target_bb;
                }
            }
        } //end if
    }

    if (sibling != NULL) {
        UINT ipdom = xcom::DGraph::get_ipdom(bb->id());
        ASSERT(ipdom > 0, ("bb does not have ipdom"));
        *sibling = getBB(ipdom);
    }
}


template <class BB, class XR>
void CFG<BB, XR>::dumpLoopTree(LI<BB> * looplist, UINT indent, FILE * h)
{
    while (looplist != NULL) {
        fprintf(h, "\n");
        for (UINT i = 0; i < indent; i++) { fprintf(h, " "); }
        fprintf(h, "LOOP HEAD:BB%d, BODY:",
            LI_loop_head(looplist)->id());
        for (INT i = LI_bb_set(looplist)->get_first();
             i != -1; i = LI_bb_set(looplist)->get_next((UINT)i)) {
            fprintf(h, "%d,", i);
        }
        dumpLoopTree(LI_inner_list(looplist), indent + 2, h);
        looplist = LI_next(looplist);
    }
}


//Do verification while BB removed.
template <class BB, class XR>
bool CFG<BB, XR>::verifyIfBBRemoved(IN CDG * cdg, OptCtx & oc)
{
    ASSERT(cdg, ("DEBUG: verification need cdg."));
    xcom::C<BB*> * ct, * next_ct;
    List<BB*> succs;
    bool is_cfg_valid = OC_is_cfg_valid(oc);
    for (m_bb_list->get_head(&ct), next_ct = ct; ct != NULL; ct = next_ct) {
        next_ct = m_bb_list->get_next(next_ct);
        BB * bb = ct->val();
        BB * next_bb = NULL;
        if (next_ct != NULL) { next_bb = next_ct->val(); }

        IR const* last_xr = get_last_xr(bb);
        if (last_xr == NULL && !isRegionEntry(bb) &&
            !bb->isExceptionHandler()) {
            if (next_bb == NULL || !is_cfg_valid) { continue; }

            //CASE:
            //    BB1
            //    LOOP_HEADER(BB2)
            //        LOOP_BODY(BB3)
            //    ENDLOOP
            //    BB5
            //
            //There are edges: BB1->BB2->BB3, BB2->BB5, BB3->BB2
            //Where BB3->BB2 is back edge.
            //When we remove BB2, add edge BB3->BB5.
            get_succs(succs, bb);
            if (succs.get_elem_count() <= 1) { continue; }

            for (BB * succ = succs.get_head();
                 succ != NULL; succ = succs.get_next()) {
                if (succ == next_bb || succ == bb) { continue; }

                xcom::Edge * e = get_edge(bb->id(), succ->id());
                if (EDGE_info(e) != NULL &&
                    CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e))) {
                    continue;
                }

                if (!cdg->is_cd(bb->id(), succ->id())) {
                    //bb should not be empty, need goto.
                    UNREACHABLE();
                }
            }
        } else if (last_xr != NULL && last_xr->isConditionalBr()) {
            //CASE:Check legalization of fallthrough edge and target edge.
            //     condbr L1
            //     FallThroughBB
            //     ...
            //     L1:
            get_succs(succs, bb);
            ASSERT(succs.get_elem_count() == 2, ("illegal number of edge"));
            bool find_fallthrough_bb = false;
            for (BB * succ = succs.get_head();
                 succ != NULL; succ = succs.get_next()) {
                if (succ == next_bb) {
                    //find fallthrough bb.
                    find_fallthrough_bb = true;
                    continue;
                }
                ASSERT0(last_xr->getLabel());
                ASSERT(succ == findBBbyLabel(last_xr->getLabel()),
                    ("miss target BB"));
            }
        }
    } //end for each BB
    return true;
}


//Remove empty bb, and merger label info.
template <class BB, class XR>
bool CFG<BB, XR>::removeEmptyBB(OptCtx & oc)
{
    START_TIMER(t, "Remove Empty BB");
    xcom::C<BB*> * ct, * next_ct;
    bool doit = false;
    List<BB*> succs;
    List<BB*> preds;
    bool is_cfg_valid = OC_is_cfg_valid(oc);
    for (m_bb_list->get_head(&ct), next_ct = ct;
         ct != NULL; ct = next_ct) {
        next_ct = m_bb_list->get_next(next_ct);
        BB * bb = ct->val();
        ASSERT0(bb);

        BB * next_bb = NULL;
        if (next_ct != NULL) {
            next_bb = next_ct->val();
        }

        //TODO: confirm if this is right:
        //    isRegionExit() need to update if cfg
        //    changed or ir_bb_list reconstruct.
        //    e.g: void m(bool r, bool y)
        //        {
        //            bool l;
        //            l = y || r;
        //            return 0;
        //        }
        //After initCfg(), there are 2 bb, BB1 and BB3.
        //While IR_LOR simpilified, new bb generated, then func-exit bb flag
        //need to update.
        if (get_last_xr(bb) == NULL &&
            !isRegionEntry(bb) &&
            !bb->isExceptionHandler()) {

            //Do not replace isRegionExit/entry() to isCFGExit/entry().
            //Some redundant cfg has multi bb which
            //satifies cfg-entry condition.
            if (next_bb == NULL) {
                //bb is the last empty bb.
                ASSERT0(next_ct == NULL);
                if (bb->getLabelList().get_elem_count() == 0 &&
                    !isRegionExit(bb)) {
                    bb->removeSuccessorPhiOpnd(this);
                    //resetMapBetweenLabelAndBB(bb); BB does not have Labels.
                    remove_bb(ct);
                    doit = true;
                }
                continue;
            }

            //Move labels of bb to next_bb.
            moveLabels(bb, next_bb);

            if (!is_cfg_valid) { continue; }

            //Revise edge.
            //Connect all predecessors to each successors of bb.
            get_preds(preds, bb);
            if (preds.get_elem_count() > 1 && bb->successorHasPhi(this)) {
                //Remove bb, you need to add more than one predecessors to
                //bb's succ, that will add more than one operand to phi at
                //bb's succ. It complicates the optimization. TODO.
                continue;
            }

            get_succs(succs, bb);
            xcom::C<BB*> * ct2;
            for (preds.get_head(&ct2);
                 ct2 != preds.end(); ct2 = preds.get_next(ct2)) {
                BB * pred = ct2->val();
                ASSERT0(pred);
                if (get_vertex(pred->id()) == NULL) {
                    continue;
                }

                if (get_edge(pred->id(), next_bb->id()) != NULL) {
                    //If bb removed, the number of its successors will decrease.
                    //Then the number of PHI of bb's successors must be revised.
                    bb->removeSuccessorPhiOpnd(this);
                } else {
                    addEdge(pred->id(), next_bb->id());
                }
            }

            for (succs.get_head(&ct2);
                 ct2 != succs.end(); ct2 = succs.get_next(ct2)) {
                BB * succ = ct2->val();
                if (get_vertex(succ->id()) == NULL || succ == next_bb) {
                    continue;
                }

                xcom::C<BB*> * ct_prev_of_succ;
                m_bb_list->find(succ, &ct_prev_of_succ);
                ASSERT0(ct_prev_of_succ != NULL);

                //Get the adjacent previous BB of succ.
                m_bb_list->get_prev(&ct_prev_of_succ);
                if (ct_prev_of_succ == NULL) { continue; }

                BB * prev_bb_of_succ = ct_prev_of_succ->val();
                ASSERT0(prev_bb_of_succ != NULL);

                XR * last_xr = get_last_xr(prev_bb_of_succ);
                if (last_xr == NULL ||
                    (!last_xr->isConditionalBr() &&
                     !last_xr->isUnconditionalBr() &&
                     !last_xr->is_return())) {
                    //Add fall-through edge between prev_of_succ and succ.
                    //e.g:
                    //    bb:
                    //    goto succ; --------
                    //                       |
                    //    ...                |
                    //                       |
                    //    prev_of_succ:      |
                    //    a=1;               |
                    //                       |
                    //                       |
                    //    succ: <-------------
                    //    b=1;
                    addEdge(prev_bb_of_succ->id(), succ->id());
                } else if (last_xr != NULL &&
                           last_xr->isUnconditionalBr() &&
                           !last_xr->isIndirectBr()) {
                    //Add fall-through edge between prev_of_succ and succ.
                    //e.g:
                    //prev_of_succ:
                    //    ...
                    //    goto L1  <--- redundant branch
                    //
                    //succ:
                    //    L1:
                    BB * tgt_bb = findBBbyLabel(last_xr->getLabel());
                    ASSERT0(tgt_bb != NULL);
                    if (tgt_bb == succ) {
                        addEdge(prev_bb_of_succ->id(), succ->id());
                    }
                }
            } //end for each succ

            //The map between bb and its Labels has changed.
            //resetMapBetweenLabelAndBB(bb);
            remove_bb(bb);
            doit = true;
        } //end if
    } //end for each bb
    END_TIMER(t, "Remove Empty BB");
    return doit;
}


//Remove redundant branch edge.
template <class BB, class XR>
bool CFG<BB, XR>::removeRedundantBranchCase1(
        BB *RESTRICT bb,
        BB const*RESTRICT next_bb,
        XR * xr)
{
    ASSERT0(bb && xr);
    //CASE:
    //    BB1:
    //    falsebr L0 //S1
    //
    //    BB2:
    //    L0  //S2
    //    ... //S3
    //
    //S1 is redundant branch.
    xcom::Vertex * v = get_vertex(bb->id());
    xcom::EdgeC * last_el = NULL;
    bool find = false; //find another successor with different target.
    for (xcom::EdgeC * el = VERTEX_out_list(v); el != NULL; el = EC_next(el)) {
        last_el = el;
        BB * succ = getBB(VERTEX_id(EDGE_to(EC_edge(el))));
        if (succ != next_bb) {
            find = true;
            break;
        }
    }

    if (last_el != NULL && !find) {
        //There is only one target for cond-br.
        //Thus the cond-br is redundant.
        xcom::EdgeC * el = last_el;
        while (EC_prev(el) != NULL) {
            xcom::EdgeC * tmp = el;
            el = EC_prev(el);
            xcom::Graph::removeEdge(EC_edge(tmp));
        }
        remove_xr(bb, xr);
        return true;
    }
    return false;
}


//Remove redundant branch edge.
template <class BB, class XR>
bool CFG<BB, XR>::removeRedundantBranch()
{
    START_TIMER(t, "Remove Redundant Branch");
    xcom::C<BB*> * ct, * next_ct;
    bool doit = false;
    for (m_bb_list->get_head(&ct), next_ct = ct;
         ct != m_bb_list->end(); ct = next_ct) {
        next_ct = m_bb_list->get_next(next_ct);
        BB * bb = ct->val();
        BB * next_bb = NULL; //next_bb is fallthrough BB.
        if (next_ct != NULL) {
            next_bb = next_ct->val();
        }

        XR * xr = get_last_xr(bb);
        if (xr == NULL) {
            //Although bb is empty, it may have some labels attached,
            //which may have dedicated usage. Do not remove it for
            //convservative purpose.
            ASSERT(isCFGEntry(bb->id()) || isCFGExit(bb->id()) ||
                   bb->getLabelList().get_elem_count() != 0,
                   ("should call removeEmptyBB() first."));
            continue;
        }

        if (xr->isConditionalBr()) {
            doit |= removeRedundantBranchCase1(bb, next_bb, xr);
        } else if (xr->isUnconditionalBr() && !xr->isIndirectBr()) {
            //BB1:
            //    ...
            //    goto L1  <--- redundant branch
            //
            //BB2:
            //    L1:
            BB * tgt_bb = findBBbyLabel(xr->getLabel());
            ASSERT0(tgt_bb != NULL);
            if (tgt_bb == next_bb) {
                remove_xr(bb, xr);
                doit = true;
            }
        }
    }
    END_TIMER(t, "Remove Redundant Branch");
    return doit;
}


template <class BB, class XR>
void CFG<BB, XR>::sortByDFSRecur(
        List<BB*> & new_bbl,
        BB * bb,
        Vector<bool> & visited)
{
    if (bb == NULL) return;
    visited.set(bb->id(), true);
    new_bbl.append_tail(bb);
    List<BB*> succs;
    get_succs(succs, bb);
    xcom::C<BB*> * ct;
    for (succs.get_head(&ct); ct != succs.end(); ct = succs.get_next(ct)) {
        BB * succ = ct->val();
        ASSERT0(succ);

        if (!visited.get(succ->id())) {
            sortByDFSRecur(new_bbl, succ, visited);
        }
    }
    return;
}


//Sort BBs in order of DFS.
//NOTICE:
//    Be careful use this function. Because we will emit IR in terms of
//    the order which 'm_bbl' holds.
template <class BB, class XR>
void CFG<BB, XR>::sortByDFS()
{
    List<BB*> new_bbl;
    Vector<bool> visited;
    xcom::C<BB*> * ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        BB * bb = ct->val();
        ASSERT0(bb);
        if (!visited.get(bb->id())) {
            sortByDFSRecur(new_bbl, bb, visited);
        }
    }

    #ifdef _DEBUG_
    for (BB * bbc = m_bb_list->get_head();
         bbc != NULL; bbc = m_bb_list->get_next()) {
        ASSERT(visited.get(bbc->id()),
                ("unreachable BB, call removeUnreachBB()"));
    }
    #endif
    revise_fallthrough(new_bbl);
    m_bb_list->copy(new_bbl);
    m_bb_sort_type = SEQ_DFS;
}


//Sort BBs in order of BFS.
//NOTICE:
//    Be careful use this function. Because we will emit IR in terms of
//    the order which 'm_bbl' holds.
template <class BB, class XR>
void CFG<BB, XR>::sortByBFS()
{
    List<BB*> foottrip;
    List<BB*> new_bbl;
    List<BB*> succs;
    Vector<bool> visited;
    xcom::C<BB*> * ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        BB * bb = ct->val();
        if (visited.get(bb->id())) { continue; }

        visited.set(bb->id(), true);
        new_bbl.append_tail(bb);
        foottrip.append_head(bb);

        while (foottrip.get_elem_count() > 0) {
            foottrip.remove_tail();
            get_succs(succs, bb);
            xcom::C<BB*> * ct2;
            for (succs.get_head(&ct2);
                 ct2 != succs.end(); ct2 = succs.get_next(ct2)) {
                BB * succ = ct2->val();
                ASSERT0(succ);
                if (visited.get(succ->id())) { continue; }

                visited.set(succ->id(), true);
                new_bbl.append_tail(succ);
                foottrip.append_head(succ);
            }
        }
    }

    #ifdef _DEBUG_
    for (BB * bbc = m_bb_list->get_head();
         bbc != NULL; bbc = m_bb_list->get_next()) {
        ASSERT(visited.get(bbc->id()),
                ("unreachable BB, call removeUnreachBB()"));
    }
    #endif

    revise_fallthrough(new_bbl);
    m_bb_list->copy(new_bbl);
    m_bb_sort_type = SEQ_BFS;
}


//Sort BBs in order of topology.
//NOTICE:
//    Be careful use this function.
//    Because we will emit IR in terms of
//    the order which 'm_bbl' holds.
template <class BB, class XR>
void CFG<BB, XR>::sortByTopological()
{
    xcom::Graph g;
    g.clone(*this);
    List<BB*> new_bbl;
    List<xcom::Vertex*> succ;
    INT c;
    List<xcom::Vertex*> header;
    while (g.get_vertex_num() > 0) {
        header.clean();
        for (xcom::Vertex * vex = g.get_first_vertex(c);
             vex != NULL; vex = g.get_next_vertex(c)) {
            if (g.get_in_degree(vex) == 0) {
                header.append_tail(vex);
            }
        }

        if (header.get_elem_count() == 0) {
            header.append_tail(succ.get_head());
        }

        succ.clean();
        ASSERT(header.get_elem_count() > 0, ("No entry BB"));
        xcom::C<xcom::Vertex*> * ct;
        for (header.get_head(&ct);
             ct != header.end(); ct = header.get_next(ct)) {
            xcom::Vertex * v = ct->val();
            xcom::EdgeC * el = VERTEX_out_list(v);
            if (el != NULL) {
                //record the first succ.
                succ.append_tail(EDGE_to(EC_edge(el)));
            }
            g.removeVertex(v);
            new_bbl.append_tail(getBB(VERTEX_id(v)));
        }
    }
    revise_fallthrough(new_bbl);
    m_bb_list->copy(new_bbl);
    m_bb_sort_type = SEQ_TOPOL;
}


template <class BB, class XR>
void CFG<BB, XR>::removeUnreachableBB2(UINT id, xcom::BitSet & visited)
{
    visited.bunion(id);
    xcom::EdgeC * el = VERTEX_out_list(get_vertex(id));
    while (el != NULL) {
        UINT succ = VERTEX_id(EDGE_to(EC_edge(el)));
        if (!visited.is_contain(succ)) {
            removeUnreachableBB(succ, visited);
        }
        el = EC_next(el);
    }
}


template <class BB, class XR>
void CFG<BB, XR>::removeUnreachableBB(UINT id, xcom::BitSet & visited)
{
    List<xcom::Vertex*> wl;
    ASSERT0(get_vertex(id));
    wl.append_tail(get_vertex(id));
    visited.bunion(id);

    xcom::Vertex * v;
    while ((v = wl.remove_head()) != NULL) {
        xcom::EdgeC * el = VERTEX_out_list(v);
        while (el != NULL) {
            xcom::Vertex * succv = EDGE_to(EC_edge(el));
            if (!visited.is_contain(VERTEX_id(succv))) {
                wl.append_tail(succv);
                visited.bunion(VERTEX_id(succv));
            }
            el = EC_next(el);
        }
    }
}


//Perform DFS to seek for unreachable BB, removing the 'dead-BB', and
//free its ir-list. Return true if some dead-BB removed.
template <class BB, class XR>
bool CFG<BB, XR>::removeUnreachBB()
{
    bool removed = false;
    ASSERT0(m_bb_list);
    if (m_bb_list->get_elem_count() == 0) { return false; }

    START_TIMER(t, "Remove Unreach BB");

    //There is only one entry point.
    xcom::BitSet visited;
    visited.bunion(m_bb_list->get_elem_count());
    visited.diff(m_bb_list->get_elem_count());

    ASSERT0(m_entry);
    if (!visited.is_contain(m_entry->id())) {
        removeUnreachableBB(m_entry->id(), visited);
    }

    xcom::C<BB*> * next_ct;
    xcom::C<BB*> * ct;
    for (m_bb_list->get_head(&ct); ct != m_bb_list->end(); ct = next_ct) {
        BB * bb = ct->val();
        next_ct = m_bb_list->get_next(ct);
        if (!visited.is_contain(bb->id())) {
            //EH may be redundant and can be removed.
            //ASSERT(!bb->isExceptionHandler(),
            // ("For conservative purpose, "
            //  "exception handler should be reserved."));

            bb->removeSuccessorPhiOpnd(this);
            resetMapBetweenLabelAndBB(bb);
            remove_bb(ct);
            removed = true;
        }
    }

    END_TIMER(t, "Remove Unreach BB");
    return removed;
}


//Connect each predessors to each successors.
template <class BB, class XR>
void CFG<BB, XR>::chainPredAndSucc(UINT vid, bool is_single_pred_succ)
{
    xcom::Vertex * v = get_vertex(vid);
    ASSERT0(v);
    xcom::EdgeC * pred_lst = VERTEX_in_list(v);
    xcom::EdgeC * succ_lst = VERTEX_out_list(v);
    if (is_single_pred_succ) {
        ASSERT(get_in_degree(v) <= 1 &&
                get_out_degree(v) <= 1,
                ("BB only has solely pred and succ."));
    }
    while (pred_lst != NULL) {
        UINT from = VERTEX_id(EDGE_from(EC_edge(pred_lst)));
        xcom::EdgeC * tmp_succ_lst = succ_lst;
        while (tmp_succ_lst != NULL) {
            UINT to = VERTEX_id(EDGE_to(EC_edge(tmp_succ_lst)));
            addEdge(from, to);
            tmp_succ_lst = EC_next(tmp_succ_lst);
        }
        pred_lst = EC_next(pred_lst);
    }
}


//Return all successors.
template <class BB, class XR>
void CFG<BB, XR>::get_succs(IN OUT List<BB*> & succs, BB const* v)
{
    ASSERT0(v);
    xcom::Vertex * vex = get_vertex(v->id());
    xcom::EdgeC * el = VERTEX_out_list(vex);
    succs.clean();
    while (el != NULL) {
        INT succ = VERTEX_id(EDGE_to(EC_edge(el)));
        succs.append_tail(getBB(succ));
        el = EC_next(el);
    }
}


//Return all predecessors.
template <class BB, class XR>
void CFG<BB, XR>::get_preds(IN OUT List<BB*> & preds, BB const* v)
{
    ASSERT0(v);
    xcom::Vertex * vex = get_vertex(v->id());
    xcom::EdgeC * el = VERTEX_in_list(vex);
    preds.clean();
    while (el != NULL) {
        INT pred = VERTEX_id(EDGE_from(EC_edge(el)));
        preds.append_tail(getBB(pred));
        el = EC_next(el);
    }
}


//Construct cfg.
//Append exit bb if necessary when cfg is constructed.
template <class BB, class XR>
void CFG<BB, XR>::build(OptCtx & oc)
{
    ASSERT(m_bb_list, ("bb_list is emt"));
    xcom::C<BB*> * ct = NULL;
    xcom::C<BB*> * next_ct;
    List<BB*> tgt_bbs;
    for (m_bb_list->get_head(&ct); ct != m_bb_list->end(); ct = next_ct) {
        BB * bb = ct->val();
        next_ct = m_bb_list->get_next(ct);
        BB * next = NULL;
        if (next_ct != m_bb_list->end()) {
            next = next_ct->val();
        }

        XR * last = get_last_xr(bb);
        if (last == NULL) {
            //Remove empty bb after CFG done.
            //ASSERT(bb->is_bb_exit(), ("Should be removed!"));
            //Add fall-through edge.
            //The last bb may not terminated by 'return' stmt.
            if (next != NULL && !next->is_terminate()) {
                addEdge(bb->id(), next->id());
            } else {
                addVertex(bb->id());
            }
            continue;
        }

        //Check bb boundary
        if (last->is_terminate()) {
            ; //Do nothing.
        } else if (last->is_call()) {
            //Add fall-through edge
            //The last bb may not be terminated by 'return' stmt.
            if (next != NULL && !next->is_terminate()) {
                addEdge(bb->id(), next->id());
            }
        } else if (last->isConditionalBr()) {
            //Add fall-through edge
            //The last bb may not be terminated by 'return' stmt.
            if (next != NULL && !next->is_terminate()) {
                addEdge(bb->id(), next->id());
            }
            //Add edge between source BB and target BB.
            BB * target_bb = findBBbyLabel(last->getLabel());
            ASSERT(target_bb != NULL, ("target cannot be NULL"));
            addEdge(bb->id(), target_bb->id());
        } else if (last->isMultiConditionalBr()) {
            //Add fall-through edge
            //The last bb may not be terminated by 'return' stmt.
            if (next != NULL && !next->is_terminate()) {
                addEdge(bb->id(), next->id());
            }

            //Add edge between source BB and multi-target BBs.
            tgt_bbs.clean();
            findTargetBBOfMulticondBranch(last, tgt_bbs);

            xcom::C<BB*> * ct2;
            for (tgt_bbs.get_head(&ct2);
                 ct2 != tgt_bbs.end(); ct2 = tgt_bbs.get_next(ct2)) {
                BB * tbb = ct2->val();
                addEdge(bb->id(), tbb->id());
            }
        } else if (last->isUnconditionalBr()) {
            if (last->isIndirectBr()) {
                tgt_bbs.clean();
                findTargetBBOfIndirectBranch(last, tgt_bbs);
                xcom::C<BB*> * ct2;
                for (tgt_bbs.get_head(&ct2);
                     ct2 != tgt_bbs.end(); ct2 = tgt_bbs.get_next(ct2)) {
                    BB * t = ct2->val();
                    addEdge(bb->id(), t->id());
                }
            } else {
                //Add edge between source BB and target BB.
                BB * target_bb = findBBbyLabel(last->getLabel());
                ASSERT(target_bb != NULL, ("target cannot be NULL"));
                addEdge(bb->id(), target_bb->id());
            }
        } else if (!last->is_return()) {
            //Add fall-through edge.
            //The last bb may not end by 'return' stmt.
            if (next != NULL && !next->is_terminate()) {
                addEdge(bb->id(), next->id());
            }
        }
    }
    OC_is_cfg_valid(oc) = true;
}


template <class BB, class XR>
void CFG<BB, XR>::collectLoopInfoRecur(LI<BB> * li)
{
    if (li == NULL) { return; }
    LI<BB> * subli = LI_inner_list(li);
    while (subli != NULL) {
        collectLoopInfoRecur(subli);
        LI_has_call(li) = LI_has_call(subli);
        LI_has_early_exit(li) = LI_has_early_exit(subli);
        subli = LI_next(subli);
    }

    //A BB list is used in the CFG to describing sparse node layout.
    //In actually, such situation is rarely happen.
    //So we just use 'bb_set' for now. (see loop.h)
    xcom::BitSet * bbset = LI_bb_set(li);
    ASSERT0(bbset != NULL);
    for (INT id = bbset->get_first(); id != -1; id = bbset->get_next(id)) {
        BB * bb = getBB(id);
        ASSERT0(bb != NULL);
        if (bb->hasCall()) {
            LI_has_call(li) = true;
        }
    }
}


//Insert loop into loop tree.
template <class BB, class XR>
bool CFG<BB, XR>::insertLoopTree(LI<BB> ** lilist, LI<BB>* loop)
{
    ASSERT0(lilist != NULL && loop != NULL);
    if (*lilist == NULL) {
        *lilist = loop;
        return true;
    }

    LI<BB> * li = *lilist, * cur = NULL;
    while (li != NULL) {
        cur = li;
        li = LI_next(li);
        if (LI_bb_set(cur)->is_contain(*LI_bb_set(loop))) {
            if (insertLoopTree(&LI_inner_list(cur), loop)) {
                return true;
            }
        } else if (LI_bb_set(loop)->is_contain(*LI_bb_set(cur))) {
            remove(lilist, cur);
            insertLoopTree(&LI_inner_list(loop), cur);
            ASSERT(LI_inner_list(loop), ("illegal loop tree"));
        } //end if
    } //end while
    add_next(lilist, loop);
    return true;
}


//Add BB which is break-point of loop into loop.
//e.g:
//    for (i)
//        if (i < 10)
//            foo(A);
//        else
//            foo(B);
//            goto L1;
//        endif
//    endfor
//    ...
//    L1:
//
//where foo(B) and goto L1 are in BBx, and BBx
//should belong to loop body.
template <class BB, class XR>
void CFG<BB, XR>::addBreakOutLoop(BB * loop_head, xcom::BitSet & body_set)
{
    for (INT i = body_set.get_first(); i >= 0; i = body_set.get_next((UINT)i)) {
        if (i == (INT)loop_head->id()) { continue; }
        xcom::Vertex * v = get_vertex((UINT)i);
        ASSERT0(v);
        xcom::EdgeC * out = VERTEX_out_list(v);
        UINT c = 0;
        while (out != NULL) {
            c++;
            if (c >= 2) {
                break;
            }
            out = EC_next(out);
        }
        if (c < 2) { continue; }
        while (out != NULL) {
            UINT succ = VERTEX_id(EDGE_to(EC_edge(out)));
            if (!body_set.is_contain(succ)) {
                BB * p = getBB(succ);
                ASSERT0(p);
                XR * xr = get_last_xr(p);
                if (xr == NULL || xr->isUnconditionalBr()) {
                    body_set.bunion(succ);
                }
            }
            out = EC_next(out);
        }
    }
}


//access_li_by_scan_bb: if true this function will clean loop info
//via scanning BB list.
template <class BB, class XR>
void CFG<BB, XR>::cleanLoopInfo(bool access_li_by_scan_bb)
{
    if (access_li_by_scan_bb) {
        xcom::C<BB*> * ct;
        for (m_bb_list->get_head(&ct);
             ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
            BB * bb = ct->val();
            LI<BB> * li = m_map_bb2li.get(bb->id());
            if (li != NULL) {
                m_bs_mgr->free(LI_bb_set(li));
                m_map_bb2li.set(bb->id(), NULL);
            }
        }
        m_loop_info = NULL;
        return;
    }

    LI<BB> * li = getLoopInfo();
    if (li == NULL) { return; }

    List<LI<BB>*> worklst;
    for (; li != NULL; li = LI_next(li)) {
        worklst.append_tail(li);
    }

    while (worklst.get_elem_count() > 0) {
        LI<BB> * x = worklst.remove_head();

        UINT id = LI_loop_head(x)->id();
        LI<BB> * li2 = m_map_bb2li.get(id);
        ASSERT(li2, ("No any BB correspond to current loop info."));

        m_bs_mgr->free(LI_bb_set(li2));
        m_map_bb2li.set(id, NULL);

        for (LI<BB> * y = LI_inner_list(x); y != NULL; y = LI_next(y)) {
            worklst.append_tail(y);
        }
    }
    m_loop_info = NULL;
}


//Find natural loops.
//NOTICE: DOM set of BB must be avaiable.
template <class BB, class XR>
bool CFG<BB, XR>::findLoop()
{
    cleanLoopInfo();
    List<UINT> tmp;
    TMap<BB*, LI<BB>*> head2li;
    xcom::C<BB*> * ct;
    for (m_bb_list->get_head(&ct);
         ct != m_bb_list->end(); ct = m_bb_list->get_next(ct)) {
        BB * bb = ct->val();

        //Access each sussessor of bb.
        xcom::Vertex * vex = get_vertex(bb->id());
        ASSERT0(vex);
        for (xcom::EdgeC * el = VERTEX_out_list(vex); el != NULL; el = EC_next(el)) {
            BB * succ = getBB(VERTEX_id(EDGE_to(EC_edge(el))));
            ASSERT0(succ);

            xcom::BitSet * dom = m_dom_set.get(bb->id());
            ASSERT(dom, ("should compute dominator first"));
            if (!dom->is_contain(succ->id())) { continue; }

            //If the SUCC is one of the DOMINATOR of bb, then it
            //indicates a back-edge.
            //xcom::Edge:bb->succ is a back-edge, each back-edge descripts a
            //natural loop.
            xcom::BitSet * loop = m_bs_mgr->create();
            identifyNaturalLoop(bb->id(), succ->id(), *loop, tmp);

            //Handle some special cases.
            addBreakOutLoop(succ, *loop);

            //Loop may have multiple back edges.
            LI<BB> * li = head2li.get(succ);
            if (li != NULL) {
                //Multiple natural loops have the same loop header.
                LI_bb_set(li)->bunion(*loop);
                continue;
            }

            li = (LI<BB>*)xmalloc(sizeof(LI<BB>));
            LI_id(li) = m_li_count++;
            LI_bb_set(li) = loop;
            LI_loop_head(li) = succ;
            m_map_bb2li.set(succ->id(), li);
            insertLoopTree(&m_loop_info, li);
            head2li.set(succ, li);
        }
    }
    return true;
}


//Back edge: y dominate x, back-edge is : x->y
template <class BB, class XR>
void CFG<BB, XR>::identifyNaturalLoop(
        UINT x,
        UINT y,
        IN OUT xcom::BitSet & loop,
        List<UINT> & tmp)
{
    //Both x,y are node in loop.
    loop.bunion(x);
    loop.bunion(y);
    if (x != y) {
        tmp.clean();
        tmp.append_head(x);
        while (tmp.get_elem_count() != 0) {
            //Bottom-up scanning and starting with 'x'
            //to handling each node till 'y'.
            //All nodes in the path among from 'x' to 'y'
            //are belong to natural loop.
            UINT bb = tmp.remove_tail();
            xcom::EdgeC const* ec = VERTEX_in_list(get_vertex(bb));
            while (ec != NULL) {
                INT pred = VERTEX_id(EDGE_from(EC_edge(ec)));
                if (!loop.is_contain(pred)) {
                    //If pred is not a member of loop,
                    //add it into list to handle.
                    loop.bunion(pred);
                    tmp.append_head(pred);
                }
                ec = EC_next(ec);
            }
        }
    }
}


template <class BB, class XR>
bool CFG<BB, XR>::isLoopHeadRecur(LI<BB> * li, BB * bb)
{
    if (li == NULL) { return false; }
    LI<BB> * t = li;
    while (t != NULL) {
        ASSERT(LI_loop_head(t) != NULL, ("loop info absent loophead bb"));
        if (LI_loop_head(t) == bb) {
            return true;
        }
        t = LI_next(t);
    }
    return isLoopHeadRecur(LI_inner_list(li), bb);
}


template <class BB, class XR>
void CFG<BB, XR>::dump_vcg(CHAR const* name)
{
    if (!name) {
        name = "graph_cfg.vcg";
    }
    UNLINK(name);
    FILE * hvcg = fopen(name, "a+");
    ASSERT(hvcg, ("%s create failed!!!", name));
    fprintf(hvcg, "graph: {"
              "title: \"Graph\"\n"
              "shrink:  15\n"
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
              "node.textcolor: darkred\n"
              "node.bordercolor: blue\n"
              "edge.color: darkgreen\n");

    //Print node
    UINT vertical_order = 1;
    for (BB * bb = m_bb_list->get_head();
         bb != NULL;  bb = m_bb_list->get_next()) {
        CHAR const* shape = "box";
        CHAR const* font = "courB";
        INT scale = 1;
        CHAR const* color = "gold";
        if (isRegionEntry(bb) || isRegionExit(bb)) {
            font = "Times Bold";
            scale = 2;
            color = "cyan";
        }
        fprintf(hvcg,
            "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
            "fontname:\"%s\" scaling:%d label:\"",
            bb->id(), vertical_order++, shape, color, font, scale);
        xcom::Vertex * v = get_vertex(bb->id());
        fprintf(hvcg, "   BB%d ", bb->id());
        if (VERTEX_rpo(v) != 0) {
            fprintf(hvcg, " rpo:%d", VERTEX_rpo(v));
        }
        fprintf(hvcg, "\" }");
    }

    //Print edge
    INT c;
    for (xcom::Edge * e = m_edges.get_first(c);
         e != NULL;  e = m_edges.get_next(c)) {
        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei == NULL) {
            fprintf(hvcg,
                "\nedge: { sourcename:\"%d\" targetname:\"%d\" }",
                VERTEX_id(EDGE_from(e)), VERTEX_id(EDGE_to(e)));
        } else if (CFGEI_is_eh(ei)) {
            fprintf(hvcg,
              "\nedge: { sourcename:\"%d\" targetname:\"%d\" linestyle:dotted }",
              VERTEX_id(EDGE_from(e)), VERTEX_id(EDGE_to(e)));
        } else {
            UNREACHABLE();
        }
    }
    fprintf(hvcg, "\n}\n");
    fclose(hvcg);
}


template <class BB, class XR>
void CFG<BB, XR>::computeRPOImpl(
        IN OUT xcom::BitSet & is_visited,
        IN xcom::Vertex * v,
        IN OUT INT & order)
{
    is_visited.bunion(VERTEX_id(v));
    xcom::EdgeC * el = VERTEX_out_list(v);
    while (el != NULL) {
        xcom::Vertex * succ = EDGE_to(EC_edge(el));
        ASSERT(getBB(VERTEX_id(succ)) != NULL,
                ("without bb corresponded"));
        if (!is_visited.is_contain(VERTEX_id(succ))) {
            computeRPOImpl(is_visited, succ, order);
        }
        el = EC_next(el);
    }
    setRPO(getBB(VERTEX_id(v)), order);
    order--;
}


//Compute rev-post-order.
template <class BB, class XR>
void CFG<BB, XR>::computeRPO(OptCtx & oc)
{
    if (m_bb_list->get_elem_count() == 0) { return; }

    START_TIMER(t, "Compute Rpo");

    #ifdef _DEBUG_
    //Only for verify.
    for (BB * bb = m_bb_list->get_head();
         bb != NULL; bb = m_bb_list->get_next()) {
        setRPO(bb, -1);
    }
    #endif

    xcom::BitSet is_visited;
    ASSERT(m_entry, ("Not find entry"));

    #ifdef RECURSIVE_ALGO
    INT order = m_bb_list->get_elem_count();
    computeRPOImpl(is_visited, get_vertex(m_entry->id()), order);
    #else
    List<xcom::Vertex const*> vlst;
    computeRpoNoRecursive(get_vertex(m_entry->id()), vlst);
    #endif

    m_rpo_bblst.clean();
    xcom::C<xcom::Vertex const*> * ct;
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        xcom::Vertex const* v = ct->val();
        BB * bb = getBB(VERTEX_id(v));
        ASSERT0(bb);
        setRPO(bb, VERTEX_rpo(v));
        m_rpo_bblst.append_tail(bb);
    }
    OC_is_rpo_valid(oc) = true;
    END_TIMER(t, "Compute Rpo");
}

} //namespace xoc
#endif
