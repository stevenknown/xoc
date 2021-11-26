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

class CDG;

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
//NOTE: BB should define and implement method 'id()' and member field 'm_rpo'.
template <class BB, class XR> class CFG : public xcom::DGraph {
    COPY_CONSTRUCTOR(CFG);
protected:
    LI<BB> * m_loop_info; //Loop information
    List<BB*> * m_bb_list;
    Vector<LI<BB>*> m_map_bb2li; //map bb to LI if bb is loop header.
    BB * m_entry; //CFG Graph entry.
    List<BB*> m_exit_list; //CFG Graph ENTRY list
    List<BB*> * m_rpo_bblst; //record BB in reverse-post-order.
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

    void handleSuccsWhenRemoveEmptyBB(BB * next_bb, List<BB*> & succs);
    void handlePredsWhenRemoveEmptyBB(BB * next_bb, List<BB*> & preds);

    inline bool isLoopHeadRecur(LI<BB> * li, BB * bb);
    bool insertLoopTree(LI<BB> ** lilist, LI<BB> * loop);

    bool removeRedundantBranchCase1(BB *RESTRICT bb, BB const*RESTRICT next_bb,
                                    XR * xr);
    void removeUnreachableBB(UINT id, xcom::BitSet & visited);
    void removeUnreachableBB2(UINT id, xcom::BitSet & visited);
    //Remove empty bb, and merger label info.
    bool removeEmptyBBHelper(BB * bb, BB * next_bb,
                             C<BB*> * bbct, C<BB*> * next_ct,
                             List<BB*> & preds, List<BB*> & succs,
                             bool is_cfg_valid);

    void sortByDFSRecur(List<BB*> & new_bbl, BB * bb, Vector<bool> & visited);

    void * xmalloc(size_t size)
    {
        ASSERT0(m_pool);
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }
public:
    CFG(List<BB*> * bb_list, UINT edge_hash_size = 16,
        UINT vertex_hash_size = 16)
        : xcom::DGraph(edge_hash_size, vertex_hash_size)
    {
        ASSERTN(bb_list, ("CFG requires BB list"));
        m_bb_list = bb_list;
        m_loop_info = nullptr;
        m_bs_mgr = nullptr;
        m_li_count = 1;
        m_has_eh_edge = false;
        m_rpo_bblst = nullptr;
        m_entry = nullptr; //entry will be computed during CFG::build().
        m_pool = smpoolCreate(sizeof(CFGEdgeInfo) * 4, MEM_COMM);
        set_dense(true); //We think CFG is always dense graph.
    }
    virtual ~CFG()
    {
        smpoolDelete(m_pool);
        cleanRPOBBList();
    }

    virtual void addBreakOutLoop(BB * loop_head, xcom::BitSet & body_set);
    virtual xcom::Edge * addEdge(BB * from, BB * to)
    { return xcom::DGraph::addEdge(from->id(), to->id()); }

    //Build the CFG accroding to BB list.
    void build(OptCtx & oc);

    bool computeDom(xcom::BitSet const* uni)
    {
        ASSERTN(m_entry, ("Not found entry"));
        List<xcom::Vertex const*> vlst;
        computeRpoNoRecursive(getVertex(m_entry->id()), vlst);
        return xcom::DGraph::computeDom(&vlst, uni);
    }

    //CFG may have multiple exit. The method computing pdom is not
    //different with dom.
    bool computePdom(xcom::BitSet const* uni)
    {
        //ASSERTN(m_exit_list.get_elem_count() == 1,
        //   ("ONLY support SESE or SEME"));
        ASSERTN(m_entry, ("Not found entry"));
        return xcom::DGraph::computePdomByRpo(getVertex(m_entry->id()), uni);
    }

    //Compute all reachable BBs start from 'startbb'.
    //bbset: record the output result.
    //Note caller should clean bbset. This function is non-recursive.
    void computeReachableBBSet(BB * startbb, OUT xcom::BitSet & bbset)
    {
        ASSERT0(startbb);
        List<xcom::Vertex const*> wl;
        UINT id = startbb->id();
        ASSERT0(getVertex(id));
        wl.append_tail(getVertex(id));

        xcom::Vertex const* v;
        while ((v = wl.remove_head()) != nullptr) {
            UINT id = v->id();
            if (bbset.is_contain(id)) { continue; }

            bbset.bunion(id);
            for (xcom::EdgeC * el = VERTEX_out_list(v);
                 el != nullptr; el = EC_next(el)) {
                xcom::Vertex const* succv = el->getTo();
                if (!bbset.is_contain(succv->id())) {
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
    //This function is non-recursive.
    void computeMainStreamBBSet(BB * startbb, OUT xcom::BitSet & bbset)
    {
        ASSERT0(startbb);
        List<xcom::Vertex const*> wl;
        UINT id = startbb->id();
        ASSERT0(getVertex(id));
        wl.append_tail(getVertex(id));

        xcom::Vertex const* v;
        while ((v = wl.remove_head()) != nullptr) {
            UINT i = v->id();
            if (bbset.is_contain(i) || getBB(i)->isExceptionHandler()) {
                continue;
            }
            bbset.bunion(i);

            for (xcom::EdgeC * el = VERTEX_out_list(v);
                 el != nullptr; el = EC_next(el)) {
                xcom::Vertex const* succv = el->getTo();
                UINT sid = succv->id();
                if (!bbset.is_contain(sid) &&
                    !getBB(sid)->isExceptionHandler()) {
                    wl.append_tail(succv);
                }
            }
        }
    }

    //Compute and record exit BBs.
    //Only the function entry can be CFG entry.
    //Connect or handle TRY and CATCH BB before performing removing
    //unreachable BB.
    virtual void computeExitList()
    {
        m_exit_list.clean();

        xcom::C<BB*> * ct;
        for (m_bb_list->get_head(&ct);
             ct != nullptr; ct = m_bb_list->get_next(ct)) {
            BB * bb = ct->val();
            ASSERT0(bb);

            ASSERTN(getVertex(bb->id()),
                ("No vertex corresponds to BB%d", bb->id()));

            if (isCFGExit(bb->id())) {
                m_exit_list.append_tail(bb);
            }
        }
    }
    void cleanRPOBBList()
    {
        if (m_rpo_bblst != nullptr) {
            delete m_rpo_bblst;
            m_rpo_bblst = nullptr;
        }
    }
    void chainPredAndSucc(UINT vid, bool is_single_pred_succ = false);
    void computeRPO(OptCtx & oc);
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = sizeof(*this);
        //count += m_bb_list.count_mem(); //do NOT count up BBs in bb_list.
        count += m_map_bb2li.count_mem();
        count += m_exit_list.count_mem();
        return count;
    }

    void dumpLoopTree(LI<BB> const* looplist, UINT indent, Region * rg) const;
    virtual void dumpLoopInfo(Region * rg)
    {
        if (!rg->isLogMgrInit()) { return; }
        prt(rg, "\n==---- DUMP Natural Loop Info ----==");
        dumpLoopTree(m_loop_info, 0, rg);
    }
    virtual void dumpVCG(CHAR const* name = nullptr);
    void dumpDOM(Region * rg)
    {
        //Do not dump if LogMr is not initialized.
        if (!rg->isLogMgrInit()) { return; }
        dump_dom(rg->getLogMgr()->getFileHandler(), false);
    }

    bool findLoop();

    //Find the single exit BB if exist for given loop.
    BB * findSingleExitBB(LI<BB> const* li);

    //Find the target bb list.
    //2th parameter records a list of bb have found.
    virtual void findTargetBBOfMulticondBranch(XR const*, OUT List<BB*> &) = 0;

    //Find the bb that referred given label.
    virtual BB * findBBbyLabel(LabelInfo const*) const = 0;

    //Find a list of bb that referrence labels which are target of xr.
    //2th parameter records a list of bb have found.
    virtual void findTargetBBOfIndirectBranch(XR const*, OUT List<BB*> &) = 0;

    List<BB*> * getBBList() const { return m_bb_list; }
    UINT getLoopNum() const { return m_li_count - 1; }
    void get_preds(MOD List<BB*> & preds, BB const* v) const;
    void get_preds(MOD List<BB const*> & preds, BB const* v) const;
    void get_succs(MOD List<BB*> & succs, BB const* v) const;
    void get_succs(MOD List<BB const*> & succs, BB const* v) const;

    //Get the number of successors of bb.
    UINT getSuccsNum(BB const* bb) const
    { return getOutDegree(getVertex(bb->id())); }

    //Get the number of predecessors of bb.
    UINT getPredsNum(BB const* bb) const
    { return getInDegree(getVertex(bb->id())); }
    BB * getEntry() const { return m_entry; }
    List<BB*> * getExitList() { return &m_exit_list; }
    List<BB*> * getRPOBBList() { return m_rpo_bblst; }
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
        ASSERTN(xr != nullptr, ("bb is empty"));
        LabelInfo const* lab = xr->getLabel();
        ASSERTN(lab != nullptr, ("xr does not correspond to a unqiue label"));
        BB * target = findBBbyLabel(lab);
        ASSERTN(target != nullptr, ("label does not correspond to a BB"));
        return target;
    }

    //Get the first successor of bb.
    BB * get_first_succ(BB const* bb) const
    {
        ASSERT0(bb);
        xcom::Vertex * vex = getVertex(bb->id());
        ASSERT0(vex);

        xcom::EdgeC * ec = VERTEX_out_list(vex);
        if (ec == nullptr) { return nullptr; }

        BB * succ = getBB(ec->getToId());
        ASSERT0(succ);
        return succ;
    }

    BB * get_idom(BB * bb)
    {
        ASSERT0(bb != nullptr);
        return getBB(xcom::DGraph::get_idom(bb->id()));
    }

    BB * get_ipdom(BB * bb)
    {
        ASSERT0(bb != nullptr);
        return getBB(xcom::DGraph::get_ipdom(bb->id()));
    }

    //Return the root node of LoopInfo tree.
    LI<BB> * getLoopInfo() { return m_loop_info; }
    void getKidOfIF(BB * bb, BB ** true_body, BB ** false_body, BB ** sibling);
    void getKidOfLoop(IN BB * bb, OUT BB ** sibling, OUT BB ** body_root);

    //Return the last instruction of BB.
    virtual XR * get_last_xr(BB *) = 0;

    //Return the first instruction of BB.
    virtual XR * get_first_xr(BB *) = 0;
    virtual BB * getBB(UINT id) const = 0;

    //True if current CFG has exception-handler edge.
    bool hasEHEdge() const { return m_has_eh_edge; }

    void identifyNaturalLoop(UINT x, UINT y, MOD xcom::BitSet & loop,
                             List<UINT> & tmp);

    bool isCFGEntry(UINT bbid)
    { return xcom::Graph::is_graph_entry(getVertex(bbid)); }

    //Return true if bb is exit BB of CFG.
    bool isCFGExit(UINT bbid)
    { return xcom::Graph::is_graph_exit(getVertex(bbid)); }

    //Return true if bb is entry BB of function-region.
    //In some case, BB is not Region entry even if it is the CFG entry.
    virtual bool isRegionEntry(BB *) = 0;

    //Return true if bb is exit BB of function.
    //In some case, BB is not region-exit even if it is the CFG exit.
    virtual bool isRegionExit(BB *) = 0;

    virtual bool isLoopHead(BB * bb)
    { return isLoopHeadRecur(m_loop_info, bb); }

    //Return true if BB 'pred' control the execution of 'bb'.
    //Note 'pred' must be predecessor of 'bb'.
    bool isControlPred(BB const* pred, BB const* bb) const
    {
        return const_cast<CFG<BB, XR>*>(this)->get_ipdom(const_cast<BB*>(pred))
               != bb;
    }

    LI<BB> * mapBB2LabelInfo(BB * bb) { return m_map_bb2li.get(bb->id()); }

    //Move all Labels which attached on src BB to tgt BB.
    virtual void moveLabels(BB * src, BB * tgt) = 0;

    virtual void removeEdge(BB * from, BB * to)
    {
        xcom::Edge * e = xcom::Graph::getEdge(from->id(), to->id());
        ASSERT0(e != nullptr);
        xcom::Graph::removeEdge(e);
    }
    bool removeEmptyBB(OptCtx & oc);
    bool removeUnreachBB();
    bool removeRedundantBranch();
    void removeLoopInfo(LI<BB>* loop);
    bool reinsertLoopTree(LI<BB> ** lilist, LI<BB>* loop);

    //Insert unconditional branch to revise fall through BB.
    //e.g: Given bblist is bb1=>bb2=>bb3=>bb4, where bb4 is exit-BB,
    //and control flow edges are: bb1->bb2->bb3->bb4, bb1->bb3,
    //where bb1->bb2, bb2->bb3, bb3->bb4 are fallthrough edge.
    //
    //Assuming the reordered bblist is bb1=>bb3=>bb4=>bb2, the
    //associated control flow edges are: bb1->bb3->bb4, bb2->bb3.
    //
    //It is obviously that converting bb1->bb3 to be fallthrough,
    //and converting bb1->bb2 to be conditional branch, converting
    //bb2->bb3 to be unconditional branch.
    void revise_fallthrough(List<BB*> & new_bbl)
    { ASSERTN(0, ("Target Dependent Code")); }

    //Remove xr that in bb.
    virtual void remove_xr(BB *, XR *) = 0;

    //You should clean the relation between Label and BB before remove BB.
    virtual void removeBB(BB * bb) = 0;
    virtual void removeBB(xcom::C<BB*> * bbcontainer) = 0;
    virtual void resetMapBetweenLabelAndBB(BB * bb) = 0;

    //Rebuild CFG.
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

    //Set RPO for BB.
    virtual void setRPO(BB * bb, INT order) = 0;
    void sortByDFS();
    void sortByBFS();
    void sortByTopological();
    void setBitSetMgr(xcom::BitSetMgr * bs_mgr)
    {
        m_bs_mgr = bs_mgr;
        xcom::DGraph::setBitSetMgr(bs_mgr);
    }

    //Return true if find an order of RPO for 'bb' that
    //less than order of 'ref'.
    bool tryFindLessRpo(BB * bb, BB const* ref) const
    {
        Vertex * bb_vex = getVertex(bb->id());
        ASSERT0(bb_vex);
        Vertex const* ref_vex = getVertex(ref->id());
        ASSERT0(ref_vex);
        bool succ = Graph::tryFindLessRpo(bb_vex, ref_vex);
        bb->m_rpo = bb_vex->rpo();
        return succ;
    }

    //Perform verification if BB has been removed.
    bool verifyIfBBRemoved(CDG const* cdg, OptCtx const& oc) const;
    bool verify() const
    {
        //The entry node can not have any predecessors.
        ASSERT0(m_entry);
        xcom::Vertex const* vex = getVertex(m_entry->id());
        CHECK0_DUMMYUSE(vex && getInDegree(vex) == 0);

        //The exit node can not have successors.
        C<BB*> * it;
        for (BB const* bb = m_exit_list.get_head(&it);
             bb != nullptr; bb = m_exit_list.get_next(&it)) {
            xcom::Vertex const* vex2 = getVertex(bb->id());
            CHECK0_DUMMYUSE(vex2 && getOutDegree(vex2) == 0);
        }
        return true;
    }
};


//Find and Return LOOP_SIBLING and BODY_ROOT.
//e.g:LOOP
//        BODY_ROOT
//    END_LOOP
//    LOOP_SIBLING
template <class BB, class XR>
void CFG<BB, XR>::getKidOfLoop(BB * bb, OUT BB ** sibling, OUT BB ** body_root)
{
    LI<BB> * li = mapBB2LabelInfo(bb);
    ASSERT0(li != nullptr && li->getLoopHead() == bb);
    List<BB*> succs;
    get_succs(succs, bb);
    ASSERT0(succs.get_elem_count() == 2);
    BB * s = succs.get_head();
    if (sibling != nullptr) {
        *sibling = li->isInsideLoop(s->id()) ? succs.get_tail() : s;
    }

    if (body_root != nullptr) {
        *body_root = li->isInsideLoop(s->id()) ? s : succs.get_tail();
    }
}


//Find and Return TRUE_BODY, FALSE_BODY, IF_SIBLING.
//e.g:IF
//        TRUE_BODY
//    ELSE
//        FALSE_BODY
//    END_IF
//    IF_SIBLING
template <class BB, class XR>
void CFG<BB, XR>::getKidOfIF(BB * bb, BB ** true_body, BB ** false_body,
                             BB ** sibling)
{
    if (true_body != nullptr || false_body != nullptr) {
        UINT ipdom = xcom::DGraph::get_ipdom(bb->id());
        ASSERTN(ipdom > 0, ("bb does not have ipdom"));
        BB * fallthrough_bb = getFallThroughBB(bb);
        BB * target_bb = getTargetBB(bb);
        XR * xr = get_last_xr(bb);
        ASSERT0(xr != nullptr && xr->isConditionalBr());
        if (xr->is_truebr()) {
            if (true_body != nullptr) {
                if (ipdom == target_bb->id()) {
                    *true_body = nullptr;
                } else {
                    *true_body = target_bb;
                }
            }
            if (false_body != nullptr) {
                if (ipdom == fallthrough_bb->id()) {
                    *false_body = nullptr;
                } else {
                    *false_body = fallthrough_bb;
                }
            }
        } else {
            ASSERT0(xr->is_falsebr());
            if (true_body != nullptr) {
                if (ipdom == fallthrough_bb->id()) {
                    *true_body = nullptr;
                } else {
                    *true_body = fallthrough_bb;
                }
            }
            if (false_body != nullptr) {
                if (ipdom == target_bb->id()) {
                    *false_body = nullptr;
                } else {
                    *false_body = target_bb;
                }
            }
        } //end if
    }

    if (sibling != nullptr) {
        UINT ipdom = xcom::DGraph::get_ipdom(bb->id());
        ASSERTN(ipdom > 0, ("bb does not have ipdom"));
        *sibling = getBB(ipdom);
    }
}


template <class BB, class XR>
void CFG<BB, XR>::dumpLoopTree(LI<BB> const* looplist, UINT indent,
                               Region * rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    while (looplist != nullptr) {
        prt(rg, "\n");
        for (UINT i = 0; i < indent; i++) { prt(rg, " "); }
        ASSERT0(LI_loop_head(looplist));
        prt(rg, "LOOP%d HEAD:BB%d, BODY:", looplist->id(),
            LI_loop_head(looplist)->id());
        if (LI_bb_set(looplist) != nullptr) {
            for (INT i = LI_bb_set(looplist)->get_first();
                 i != -1; i = LI_bb_set(looplist)->get_next((UINT)i)) {
                prt(rg, "%d,", i);
            }
        }
        dumpLoopTree(LI_inner_list(looplist), indent + 2, rg);
        looplist = LI_next(looplist);
    }
}


//Perform verification if BB has been removed.
template <class BB, class XR>
bool CFG<BB, XR>::verifyIfBBRemoved(CDG const* cdg, OptCtx const& oc) const
{
    ASSERTN(cdg, ("DEBUG: verification requires cdg."));
    xcom::C<BB*> * ct, * next_ct;
    List<BB*> succs;
    bool is_cfg_valid = oc.is_cfg_valid();
    CFG<BB, XR> * pthis = const_cast<CFG<BB, XR>*>(this);
    for (m_bb_list->get_head(&ct), next_ct = ct; ct != nullptr; ct = next_ct) {
        next_ct = m_bb_list->get_next(next_ct);
        BB * bb = ct->val();
        BB * next_bb = nullptr;
        if (next_ct != nullptr) { next_bb = next_ct->val(); }

        IR const* last_xr = pthis->get_last_xr(bb);
        if (last_xr == nullptr && !pthis->isRegionEntry(bb) &&
            !bb->isExceptionHandler()) {
            if (next_bb == nullptr || !is_cfg_valid) { continue; }

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
                 succ != nullptr; succ = succs.get_next()) {
                if (succ == next_bb || succ == bb) { continue; }

                xcom::Edge * e = getEdge(bb->id(), succ->id());
                if (EDGE_info(e) != nullptr &&
                    CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e))) {
                    continue;
                }

                if (!cdg->is_control(bb->id(), succ->id())) {
                    //bb should not be empty, requires GOTO at least.
                    UNREACHABLE();
                }
            }
            continue;
        }

        if (last_xr != nullptr && last_xr->isConditionalBr()) {
            //CASE:Check legalization of fallthrough edge and target edge.
            //     condbr L1
            //     FallThroughBB
            //     ...
            //     L1:
            get_succs(succs, bb);
            ASSERTN(succs.get_elem_count() == 2, ("illegal number of edge"));
            for (BB * succ = succs.get_head();
                 succ != nullptr; succ = succs.get_next()) {
                if (succ == next_bb) {
                    //find fallthrough bb.
                    continue;
                }
                ASSERT0(last_xr->getLabel());
                ASSERTN(succ == findBBbyLabel(last_xr->getLabel()),
                        ("miss target BB"));
            }
        }
    } //end for each BB
    return true;
}


//next_bb: the next BB of given bb.
template <class BB, class XR>
void CFG<BB, XR>::handleSuccsWhenRemoveEmptyBB(BB * next_bb, List<BB*> & succs)
{
    C<BB*> * ct2;
    for (succs.get_head(&ct2); ct2 != succs.end(); ct2 = succs.get_next(ct2)) {
        BB * succ = ct2->val();
        if (getVertex(succ->id()) == nullptr || succ == next_bb) {
            continue;
        }

        xcom::C<BB*> * ct_prev_of_succ = nullptr;
        m_bb_list->find(succ, &ct_prev_of_succ);
        ASSERT0(ct_prev_of_succ != nullptr);

        //Get the adjacent previous BB of succ.
        m_bb_list->get_prev(&ct_prev_of_succ);
        if (ct_prev_of_succ == nullptr) { continue; }

        BB * prev_bb_of_succ = ct_prev_of_succ->val();
        ASSERT0(prev_bb_of_succ != nullptr);

        //TODO: confirm whether the following case should be handled.
        XR * last_xr = get_last_xr(prev_bb_of_succ);
        if (last_xr == nullptr ||
            (!last_xr->isConditionalBr() &&
             !last_xr->isUnconditionalBr() &&
             !last_xr->is_terminate())) {
            //Add fall-through edge between prev_bb_of_succ and succ.
            //e.g:bb:
            //    goto succ; --------
            //                       |
            //    ...                |
            //                       |
            //    prev_bb_of_succ:   |
            //    a=1;               |
            //                       |
            //                       |
            //    succ: <------------
            //    b=1;
            //After:
            //    bb:
            //    goto succ; --------
            //                       |
            //    ...                |
            //                       |
            //    prev_bb_of_succ:   |
            //    a=1;               |
            //      |                |
            //      V                |
            //    succ: <------------
            //    b=1;

            ASSERTN(getEdge(prev_bb_of_succ->id(), succ->id()) != nullptr,
                    ("should already have edge"));
            continue;
        }

        //TODO: confirm whether the following case should be handled.
        if (last_xr != nullptr && last_xr->isUnconditionalBr() &&
            !last_xr->isIndirectBr()) {
            if (last_xr->hasSideEffect()) {
                continue;
            }
            //Add fall-through edge between prev_bb_of_succ and succ.
            //e.g:prev_bb_of_succ:
            //    goto succ <--- redundant branch
            //
            //    succ:
            //    a=1;
            ASSERTN(getEdge(prev_bb_of_succ->id(), succ->id()) != nullptr,
                    ("should already have edge"));

            //TBD:Do we need add edge between them?
            //BB * tgt_bb = findBBbyLabel(last_xr->getLabel());
            //ASSERT0(tgt_bb);
            //if (tgt_bb == succ) {
            //    addEdge(prev_bb_of_succ, succ);
            //}
        }
    }
}


//Remove empty bb, and merge label info.
//next_bb: the next BB of given bb.
template <class BB, class XR>
void CFG<BB, XR>::handlePredsWhenRemoveEmptyBB(BB * next_bb, List<BB*> & preds)
{
    xcom::C<BB*> * ct2 = nullptr;
    for (preds.get_head(&ct2); ct2 != preds.end(); ct2 = preds.get_next(ct2)) {
        BB * pred = ct2->val();
        ASSERT0(pred);
        if (getVertex(pred->id()) == nullptr) {
            continue;
        }

        if (getEdge(pred->id(), next_bb->id()) != nullptr) {
            //If bb removed, the number of its successors will decrease.
            //Then the number of PHI of bb's successors must be revised.
            //CASE:BB7->BB8->BB9
            //      |         ^
            //      |_________|
            //There are phi at BB9, BB8 will be removed.
            //bb->removeAllSuccessorsPhiOpnd(this);
        } else {
            //CASE:BB7->BB8->BB9
            //There are phi at BB9, BB8 will be removed.
            //Do not need to add operand to PHI because the number of edge
            //of BB9 is unchanged.
            addEdge(pred, next_bb);
        }
    }
}


//Remove empty bb, and merge label info.
template <class BB, class XR>
bool CFG<BB, XR>::removeEmptyBBHelper(BB * bb, BB * next_bb,
                                      C<BB*> * bbct, C<BB*> * next_ct,
                                      List<BB*> & preds, List<BB*> & succs,
                                      bool is_cfg_valid)
{
    //CASE:do not replace isRegionExit/entry() to isCFGExit/entry().
    //Some redundant CFG has multi BB which satifies cfg-entry condition.
    if (next_bb == nullptr) {
        //'bb' is the last empty BB.
        ASSERT0(next_ct == nullptr);
        if (bb->getLabelList().get_elem_count() == 0 && !isRegionExit(bb)) {
            //BB does not have any label.
            removeBB(bbct);
            return true;
        }
        return false;
    }

    //Only apply restricted removing if CFG is invalid.
    //Especially BB list is ready, whereas CFG is not.
    if (!is_cfg_valid) { return false; }

    //Revise edge.
    //Connect all predecessors to each successors of bb.
    get_preds(preds, bb);
    if (preds.get_elem_count() > 1 && bb->successorHasPhi(this)) {
        //TODO: If you remove current BB, then you have to add more
        //than one predecessors to bb's succ, that will add more than
        //one operand to phi at bb's succ. It complicates the optimization.
        return false;
    }

    //Move labels of bb to next_bb.
    moveLabels(bb, next_bb);
    get_succs(succs, bb);
    handlePredsWhenRemoveEmptyBB(next_bb, preds);
    handleSuccsWhenRemoveEmptyBB(next_bb, succs);

    //BB and Vertex removed.
    //The map between 'bb' and labels has been maintained.
    removeBB(bbct);
    return true;
}


//Remove empty bb, and merger label info.
//Note remove BB will not affect the usage of RPO.
template <class BB, class XR>
bool CFG<BB, XR>::removeEmptyBB(OptCtx & oc)
{
    START_TIMER(t, "Remove Empty BB");
    xcom::C<BB*> * ct, * next_ct;
    bool doit = false;
    List<BB*> succs;
    List<BB*> preds;
    bool is_cfg_valid = oc.is_cfg_valid();
    for (m_bb_list->get_head(&ct), next_ct = ct; ct != nullptr; ct = next_ct) {
        next_ct = m_bb_list->get_next(next_ct);
        BB * bb = ct->val();
        ASSERT0(bb);
        BB * next_bb = nullptr;
        if (next_ct != nullptr) {
            next_bb = next_ct->val();
        }

        //TODO: confirm if this is correct:
        //  isRegionExit() need to update if CFG changed or ir_bb_list
        //  reconstructed.
        //  e.g:void m(bool r, bool y)
        //      {
        //          bool l;
        //          l = y || r;
        //          return 0;
        //      }
        //After initCfg(), there are 2 BBs, BB1 and BB3.
        //When IR_LOR simpilified, and new BB generated, func-exit BB flag
        //has to be updated as well.
        if (get_last_xr(bb) == nullptr && !isRegionEntry(bb) &&
            !bb->isExceptionHandler()) {
            doit |= removeEmptyBBHelper(bb, next_bb, ct, next_ct, preds,
                                        succs, is_cfg_valid);
        }
    }
    END_TIMER(t, "Remove Empty BB");
    return doit;
}


//Remove redundant branch edge.
template <class BB, class XR>
bool CFG<BB, XR>::removeRedundantBranchCase1(BB *RESTRICT bb,
                                             BB const*RESTRICT next_bb,
                                             XR * xr)
{
    ASSERT0(bb && xr);
    //CASE:
    //  BB1:
    //    falsebr BB2 //S1
    //  BB2:
    //  some-code;
    //S1 is redundant branch.
    xcom::Vertex * v = getVertex(bb->id());
    xcom::EdgeC * last_el = nullptr;
    bool find = false; //find another successor with different target.
    for (xcom::EdgeC * el = VERTEX_out_list(v); el != nullptr; el = EC_next(el)) {
        last_el = el;
        BB * succ = getBB(el->getToId());
        if (succ != next_bb) {
            find = true;
            break;
        }
    }

    if (last_el != nullptr && !find) {
        //There is only one target for cond-br.
        //Thus the cond-br is redundant.
        xcom::EdgeC * el = last_el;
        while (EC_prev(el) != nullptr) {
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
        BB * next_bb = nullptr; //next_bb is fallthrough BB.
        if (next_ct != nullptr) {
            next_bb = next_ct->val();
        }

        XR * xr = get_last_xr(bb);
        if (xr == nullptr) {
            //CASE1:Although bb is empty, it may have some labels attached,
            //  which may have dedicated usage. Do not remove it for
            //  convservative purpose.
            //CASE2:If you remove current BB, then you have to add more
            //  than one predecessors to bb's succ, that will add more than
            //  one operand to phi at bb's succ. It complicates the
            //  optimization.
            ASSERTN(isCFGEntry(bb->id()) || isCFGExit(bb->id()) ||
                    bb->successorHasPhi(this) ||
                    bb->getLabelList().get_elem_count() != 0,
                    ("should call removeEmptyBB() first."));
            continue;
        }
        if (xr->hasSideEffect()) {
            continue;
        }
        if (xr->isConditionalBr()) {
            doit |= removeRedundantBranchCase1(bb, next_bb, xr);
            continue;
        }
        if (xr->isUnconditionalBr() && !xr->isIndirectBr()) {
            //BB1:
            //  some-code;
            //  goto L1  <--- redundant branch
            //BB2:
            //  L1:
            BB * tgt_bb = findBBbyLabel(xr->getLabel());
            ASSERT0(tgt_bb != nullptr);
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
void CFG<BB, XR>::sortByDFSRecur(List<BB*> & new_bbl, BB * bb,
                                 Vector<bool> & visited)
{
    if (bb == nullptr) return;
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
         bbc != nullptr; bbc = m_bb_list->get_next()) {
        ASSERTN(visited.get(bbc->id()),
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
         bbc != nullptr; bbc = m_bb_list->get_next()) {
        ASSERTN(visited.get(bbc->id()),
                ("unreachable BB, call removeUnreachBB()"));
    }
    #endif

    revise_fallthrough(new_bbl);
    m_bb_list->copy(new_bbl);
    m_bb_sort_type = SEQ_BFS;
}


//Sort BB in order of topology.
//NOTE:Be careful use this function.
//    Because we will emit IR in terms of
//    the order which 'm_bb_list' holds.
template <class BB, class XR>
void CFG<BB, XR>::sortByTopological()
{
    Vector<Vertex*> vex_vec;
    if (!sortInTopologOrder(vex_vec)) {
        //Graph is cyclic.
        return;
    }
    m_bb_list->clean();
    for (INT i = 0; i <= vex_vec.get_last_idx(); i++) {
        Vertex * v = vex_vec.get(i);
        ASSERT0(v);
        m_bb_list->append_tail(getBB(v->id()));
    }
    revise_fallthrough(*m_bb_list);
    m_bb_sort_type = SEQ_TOPOL;
}


template <class BB, class XR>
void CFG<BB, XR>::removeUnreachableBB2(UINT id, xcom::BitSet & visited)
{
    visited.bunion(id);
    xcom::EdgeC * el = VERTEX_out_list(getVertex(id));
    while (el != nullptr) {
        UINT succ = el->getToId();
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
    ASSERT0(getVertex(id));
    wl.append_tail(getVertex(id));
    visited.bunion(id);

    xcom::Vertex * v = nullptr;
    while ((v = wl.remove_head()) != nullptr) {
        xcom::EdgeC * el = VERTEX_out_list(v);
        while (el != nullptr) {
            xcom::Vertex * succv = el->getTo();
            if (!visited.is_contain(succv->id())) {
                wl.append_tail(succv);
                visited.bunion(succv->id());
            }
            el = EC_next(el);
        }
    }
}


//Perform DFS to seek for unreachable BB, removing the 'dead-BB', and
//free its ir-list. Return true if some dead-BB removed.
//Note remove BB will not affect the usage of RPO.
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
            //ASSERTN(!bb->isExceptionHandler(),
            //        ("For conservative purpose, "
            //         "exception handler should be reserved."));

            resetMapBetweenLabelAndBB(bb);
            removeBB(ct);
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
    xcom::Vertex * v = getVertex(vid);
    ASSERT0(v);
    xcom::EdgeC * pred_lst = v->getInList();
    xcom::EdgeC * succ_lst = v->getOutList();
    if (is_single_pred_succ) {
        ASSERTN(getInDegree(v) <= 1 && getOutDegree(v) <= 1,
                ("BB only has solely pred and succ."));
    }
    while (pred_lst != nullptr) {
        UINT from = pred_lst->getFromId();
        xcom::EdgeC * tmp_succ_lst = succ_lst;
        while (tmp_succ_lst != nullptr) {
            UINT to = tmp_succ_lst->getToId();
            addEdge(getBB(from), getBB(to));
            tmp_succ_lst = tmp_succ_lst->get_next();
        }
        pred_lst = pred_lst->get_next();
    }
}


//Return all successors.
template <class BB, class XR>
void CFG<BB, XR>::get_succs(MOD List<BB*> & succs, BB const* v) const
{
    ASSERT0(v);
    xcom::Vertex const* vex = getVertex(v->id());
    succs.clean();
    for (xcom::EdgeC const* el = vex->getOutList();
         el != nullptr; el = el->get_next()) {
        succs.append_tail(getBB(el->getToId()));
    }
}


//Return all successors.
template <class BB, class XR>
void CFG<BB, XR>::get_succs(MOD List<BB const*> & succs, BB const* v) const
{
    ASSERT0(v);
    xcom::Vertex const* vex = getVertex(v->id());
    succs.clean();
    for (xcom::EdgeC const* el = vex->getOutList();
        el != nullptr; el = el->get_next()) {
        succs.append_tail(getBB(el->getToId()));
    }
}


//Return all predecessors.
template <class BB, class XR>
void CFG<BB, XR>::get_preds(MOD List<BB*> & preds, BB const* v) const
{
    ASSERT0(v);
    xcom::Vertex * vex = getVertex(v->id());
    ASSERT0(vex);
    preds.clean();
    for (xcom::EdgeC * el = vex->getInList();
         el != nullptr; el = el->get_next()) {
        preds.append_tail(getBB(el->getFromId()));
    }
}


//Return all predecessors.
template <class BB, class XR>
void CFG<BB, XR>::get_preds(MOD List<BB const*> & preds, BB const* v) const
{
    ASSERT0(v);
    xcom::Vertex * vex = getVertex(v->id());
    preds.clean();
    for (xcom::EdgeC * el = vex->getInList();
         el != nullptr; el = el->get_next()) {
        preds.append_tail(getBB(el->getFromId()));
    }
}


//Construct cfg.
//Append exit bb if necessary when cfg is constructed.
template <class BB, class XR>
void CFG<BB, XR>::build(OptCtx & oc)
{
    ASSERTN(m_bb_list, ("bb_list is emt"));
    xcom::C<BB*> * ct = nullptr;
    xcom::C<BB*> * next_ct;
    List<BB*> tgt_bbs;
    for (m_bb_list->get_head(&ct); ct != m_bb_list->end(); ct = next_ct) {
        BB * bb = ct->val();
        next_ct = m_bb_list->get_next(ct);
        BB * next = nullptr;
        if (next_ct != m_bb_list->end()) {
            next = next_ct->val();
        }

        XR * last = get_last_xr(bb);
        if (last == nullptr) {
            //Remove empty bb after CFG done.
            //ASSERTN(bb->is_bb_exit(), ("Should be removed!"));
            //Add fall-through edge.
            //The last bb may not terminated by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                DGraph::addEdge(bb->id(), next->id());
            } else {
                addVertex(bb->id());
            }
            continue;
        }

        //Check bb boundary
        if (last->is_terminate()) {
            continue; //Do nothing.
        }

        if (last->is_call()) {
            //Add fall-through edge
            //The last bb may not be terminated by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                DGraph::addEdge(bb->id(), next->id());
            }
            continue;
        }

        if (last->isConditionalBr()) {
            //Add fall-through edge.
            //The last bb may not be terminated by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                DGraph::addEdge(bb->id(), next->id());
            }
            //Add edge between source BB and target BB.
            BB * target_bb = findBBbyLabel(last->getLabel());
            ASSERTN(target_bb != nullptr, ("target cannot be nullptr"));
            DGraph::addEdge(bb->id(), target_bb->id());
            continue;
        }

        if (last->isMultiConditionalBr()) {
            //Add fall-through edge
            //The last bb may not be terminated by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                DGraph::addEdge(bb->id(), next->id());
            }

            //Add edge between source BB and multi-target BBs.
            tgt_bbs.clean();
            findTargetBBOfMulticondBranch(last, tgt_bbs);

            xcom::C<BB*> * ct2;
            for (tgt_bbs.get_head(&ct2);
                 ct2 != tgt_bbs.end(); ct2 = tgt_bbs.get_next(ct2)) {
                BB * tbb = ct2->val();
                DGraph::addEdge(bb->id(), tbb->id());
            }
            continue;
        }

        if (last->isUnconditionalBr()) {
            if (last->isIndirectBr()) {
                tgt_bbs.clean();
                findTargetBBOfIndirectBranch(last, tgt_bbs);
                xcom::C<BB*> * ct2;
                for (tgt_bbs.get_head(&ct2);
                     ct2 != tgt_bbs.end(); ct2 = tgt_bbs.get_next(ct2)) {
                    BB * t = ct2->val();
                    DGraph::addEdge(bb->id(), t->id());
                }
            } else {
                //Add edge between source BB and target BB.
                BB * target_bb = findBBbyLabel(last->getLabel());
                ASSERTN(target_bb != nullptr, ("target cannot be nullptr"));
                DGraph::addEdge(bb->id(), target_bb->id());
            }
            continue;
        }

        if (!last->is_return()) {
            //Add fall-through edge.
            //The last bb may not end by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                DGraph::addEdge(bb->id(), next->id());
            }
            continue;
        }

        addVertex(bb->id());
    }
    OC_is_cfg_valid(oc) = true;
}


template <class BB, class XR>
void CFG<BB, XR>::collectLoopInfoRecur(LI<BB> * li)
{
    if (li == nullptr) { return; }
    LI<BB> * subli = LI_inner_list(li);
    while (subli != nullptr) {
        collectLoopInfoRecur(subli);
        LI_has_call(li) = LI_has_call(subli);
        LI_has_early_exit(li) = LI_has_early_exit(subli);
        subli = LI_next(subli);
    }

    //A BB list is used in the CFG to describing sparse node layout.
    //In actually, such situation is rarely happen.
    //So we just use 'bb_set' for now. (see loop.h)
    xcom::BitSet * bbset = li->getBodyBBSet();
    ASSERT0(bbset != nullptr);
    for (INT id = bbset->get_first(); id != -1; id = bbset->get_next(id)) {
        BB * bb = getBB(id);
        ASSERT0(bb != nullptr);
        if (bb->hasCall()) {
            LI_has_call(li) = true;
        }
    }
}


//Remove 'loop' out of loop tree.
template <class BB, class XR>
void CFG<BB, XR>::removeLoopInfo(LI<BB>* loop)
{
    ASSERT0(loop != nullptr);
    LI<BB> * head = xcom::get_head(loop);
    ASSERT0(head);
    xcom::remove(&head, loop);
    if (LI_outer(loop) != nullptr) {
        //Update inner-list header for outer-loop of 'loop'.
        //Guarantee outer-loop have the correct inner-loop header.
        LI_inner_list(LI_outer(loop)) = head;
    }
    loop->cleanAdjRelation();
}


//Reinsert loop into loop tree.
//NOTE 'loop' has been inserted into the loop-tree.
template <class BB, class XR>
bool CFG<BB, XR>::reinsertLoopTree(LI<BB> ** lilist, LI<BB>* loop)
{
    ASSERT0(lilist != nullptr && loop != nullptr);
    removeLoopInfo(loop);
    return insertLoopTree(lilist, loop);
}


//Insert loop into loop tree.
template <class BB, class XR>
bool CFG<BB, XR>::insertLoopTree(LI<BB> ** lilist, LI<BB>* loop)
{
    ASSERT0(lilist != nullptr && loop != nullptr);
    if (*lilist == nullptr) {
        *lilist = loop;
        return true;
    }

    LI<BB> * li = *lilist, * cur = nullptr;
    while (li != nullptr) {
        cur = li;
        li = LI_next(li);
        if (cur == loop) {
            //loop has already in LoopInfo list.
            return true;
        }
        if (LI_bb_set(cur)->is_contain(*LI_bb_set(loop))) {
            if (insertLoopTree(&LI_inner_list(cur), loop)) {
                if (LI_outer(loop) == nullptr) {
                    //Only record 'cur' as outermost loop when they
                    //at are first meeting.
                    LI_outer(loop) = cur;
                }
                return true;
            }
            continue;
        }
        if (LI_bb_set(loop)->is_contain(*LI_bb_set(cur))) {
            //Loop body of 'loop' contained 'cur'.
            //Adjust inclusive-relation between 'loop' and 'cur' to
            //have 'loop' become loop-parent of 'cur'.
            xcom::remove(lilist, cur);
            insertLoopTree(&LI_inner_list(loop), cur);
            if (LI_outer(cur) == nullptr) {
                //Only record 'loop' as outermost loop when they
                //at are first meeting.
                LI_outer(cur) = loop;
            }
            ASSERTN(LI_inner_list(loop), ("illegal loop tree"));
        }
    }
    xcom::add_next(lilist, loop);
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
        xcom::Vertex * v = getVertex((UINT)i);
        ASSERT0(v);
        xcom::EdgeC * out = VERTEX_out_list(v);
        UINT c = 0;
        while (out != nullptr) {
            c++;
            if (c >= 2) {
                break;
            }
            out = EC_next(out);
        }
        if (c < 2) { continue; }
        while (out != nullptr) {
            UINT succ = out->getToId();
            if (!body_set.is_contain(succ)) {
                BB * p = getBB(succ);
                ASSERT0(p);
                XR * xr = get_last_xr(p);
                if (xr == nullptr || xr->isUnconditionalBr()) {
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
            if (li != nullptr) {
                m_bs_mgr->free(li->getBodyBBSet());
                m_map_bb2li.set(bb->id(), nullptr);
            }
        }
        m_loop_info = nullptr;
        return;
    }

    LI<BB> * li = getLoopInfo();
    if (li == nullptr) { return; }

    List<LI<BB>*> worklst;
    for (; li != nullptr; li = LI_next(li)) {
        worklst.append_tail(li);
    }

    while (worklst.get_elem_count() > 0) {
        LI<BB> * x = worklst.remove_head();

        UINT id = LI_loop_head(x)->id();
        LI<BB> * li2 = m_map_bb2li.get(id);
        ASSERTN(li2, ("No any BB correspond to current loop info."));

        m_bs_mgr->free(LI_bb_set(li2));
        m_map_bb2li.set(id, nullptr);

        for (LI<BB> * y = LI_inner_list(x); y != nullptr; y = LI_next(y)) {
            worklst.append_tail(y);
        }
    }
    m_loop_info = nullptr;
}


//Find the single exit BB if exist for given loop.
//li: represents a loop.
template <class BB, class XR>
BB * CFG<BB, XR>::findSingleExitBB(LI<BB> const* li)
{
    //A BB Set is used in the LoopInfo to describing the loop body BB set.
    xcom::BitSet * bbset = li->getBodyBBSet();
    ASSERT0(bbset);
    INT exit = -1;
    for (INT id = bbset->get_first(); id != -1; id = bbset->get_next(id)) {
        Vertex const* vex = getVertex(id);
        ASSERT0(vex);
        for (EdgeC const* ec = vex->getOutList(); ec != nullptr;
             ec = ec->get_next()) {
            UINT succ = ec->getTo()->id();
            if (!bbset->is_contain(succ)) {
                if (exit == -1) {
                    //Record the exit BB has been found.
                    exit = succ;
                } else if (exit != succ) {
                    //There are more than one exit BB for current loop.
                    return nullptr;
                }
            }
        }
    }
    return exit != -1 ? getBB(exit) : nullptr;
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
        xcom::Vertex * vex = getVertex(bb->id());
        ASSERT0(vex);
        for (xcom::EdgeC * el = VERTEX_out_list(vex);
             el != nullptr; el = EC_next(el)) {
            BB * succ = getBB(el->getToId());
            ASSERT0(succ);

            xcom::BitSet * dom = m_dom_set.get(bb->id());
            ASSERTN(dom, ("should compute dominator first"));
            if (!dom->is_contain(succ->id()) &&
                bb->id() != succ->id()) { //bb's successor is itself.
                continue;
            }

            //If the SUCC is one of the DOMINATOR of bb, then it
            //indicates a back-edge.
            //xcom::Edge:bb->succ is a back-edge, each back-edge descripts a
            //natural loop.
            xcom::BitSet * loop = m_bs_mgr->create();
            identifyNaturalLoop(bb->id(), succ->id(), *loop, tmp);

            //Handle some special cases.
            //addBreakOutLoop(succ, *loop);

            //Loop may have multiple back edges.
            LI<BB> * li = head2li.get(succ);
            if (li != nullptr) {
                //Multiple natural loops have the same loop header.
                li->getBodyBBSet()->bunion(*loop);
                reinsertLoopTree(&m_loop_info, li);
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
void CFG<BB, XR>::identifyNaturalLoop(UINT x, UINT y, MOD xcom::BitSet & loop,
                                      List<UINT> & tmp)
{
    //Both x,y are node in loop.
    loop.bunion(x);
    loop.bunion(y);
    if (x == y) { return; }

    tmp.clean();
    tmp.append_head(x);
    while (tmp.get_elem_count() != 0) {
        //Bottom-up scanning and starting with 'x'
        //to handling each node till 'y'.
        //All nodes in the path among from 'x' to 'y'
        //are belong to natural loop.
        UINT bb = tmp.remove_tail();
        xcom::EdgeC const* ec = VERTEX_in_list(getVertex(bb));
        while (ec != nullptr) {
            INT pred = ec->getFromId();
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


template <class BB, class XR>
bool CFG<BB, XR>::isLoopHeadRecur(LI<BB> * li, BB * bb)
{
    if (li == nullptr) { return false; }
    LI<BB> * t = li;
    while (t != nullptr) {
        ASSERTN(LI_loop_head(t) != nullptr, ("loop info absent loophead bb"));
        if (LI_loop_head(t) == bb) {
            return true;
        }
        t = LI_next(t);
    }
    return isLoopHeadRecur(LI_inner_list(li), bb);
}


template <class BB, class XR>
void CFG<BB, XR>::dumpVCG(CHAR const* name)
{
    if (!name) {
        name = "graph_cfg.vcg";
    }
    UNLINK(name);
    FILE * hvcg = fopen(name, "a+");
    ASSERTN(hvcg, ("%s create failed!!!", name));
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
         bb != nullptr;  bb = m_bb_list->get_next()) {
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
        xcom::Vertex * v = getVertex(bb->id());
        fprintf(hvcg, "   BB%d ", bb->id());
        if (VERTEX_rpo(v) != 0) {
            fprintf(hvcg, " rpo:%d", VERTEX_rpo(v));
        }
        fprintf(hvcg, "\" }");
    }

    //Print edge
    EdgeIter ite;
    for (xcom::Edge * e = get_first_edge(ite);
         e != nullptr;  e = get_next_edge(ite)) {
        CFGEdgeInfo * ei = (CFGEdgeInfo*)EDGE_info(e);
        if (ei == nullptr) {
            fprintf(hvcg,
                    "\nedge: { sourcename:\"%d\" targetname:\"%d\" }",
                    e->from()->id(), e->to()->id());
        } else if (CFGEI_is_eh(ei)) {
            fprintf(hvcg,
              "\nedge: { sourcename:\"%d\" "
              "targetname:\"%d\" linestyle:dotted }",
              e->from()->id(), e->to()->id());
        } else {
            UNREACHABLE();
        }
    }
    fprintf(hvcg, "\n}\n");
    fclose(hvcg);
}


template <class BB, class XR>
void CFG<BB, XR>::computeRPOImpl(MOD xcom::BitSet & is_visited,
                                 IN xcom::Vertex * v, MOD INT & order)
{
    is_visited.bunion(v->id());
    xcom::EdgeC * el = VERTEX_out_list(v);
    while (el != nullptr) {
        xcom::Vertex * succ = el->getTo();
        ASSERTN(getBB(succ->id()) != nullptr, ("without bb corresponded"));
        if (!is_visited.is_contain(succ->id())) {
            computeRPOImpl(is_visited, succ, order);
        }
        el = el->get_next();
    }
    setRPO(getBB(v->id()), order);
    order -= RPO_INTERVAL;
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
         bb != nullptr; bb = m_bb_list->get_next()) {
        setRPO(bb, RPO_UNDEF);
    }
    #endif

    xcom::BitSet is_visited;
    ASSERTN(m_entry, ("Not find entry"));

    #ifdef RECURSIVE_ALGO
    INT order = RPO_INIT_VAL + m_bb_list->get_elem_count() * RPO_INTERVAL;
    computeRPOImpl(is_visited, getVertex(m_entry->id()), order);
    #else
    List<xcom::Vertex const*> vlst;
    computeRpoNoRecursive(getVertex(m_entry->id()), vlst);
    #endif

    if (m_rpo_bblst == nullptr) { m_rpo_bblst = new List<IRBB*>(); }
    m_rpo_bblst->clean();
    xcom::C<xcom::Vertex const*> * ct;
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        xcom::Vertex const* v = ct->val();
        BB * bb = getBB(v->id());
        ASSERT0(bb);
        setRPO(bb, v->rpo());
        m_rpo_bblst->append_tail(bb);
    }
    OC_is_rpo_valid(oc) = true;
    END_TIMER(t, "Compute Rpo");
}

} //namespace xoc
#endif
