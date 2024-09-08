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
    SEQ_DFS, //depth first sort
    SEQ_BFS, //breadth first sort
    SEQ_TOPOL, //topological sort
} SEQ_TYPE;


//CFG xcom::Edge Info.
#define CFGEI_is_eh(ei) ((ei)->m_is_eh)
class CFGEdgeInfo {
public:
    bool m_is_eh; //true if edge describes exception-handling edge.
public:
    bool is_eh() const { return CFGEI_is_eh(this); }
};


//The field transfers information top-down.
//If it is true, CFG optimizer will attempt to merge label to
//next BB if current BB is empty. Default is true.
#define CFGOPTCTX_do_merge_label(x) ((x)->common_info.s1.m_do_merge_label)

//The field transfers information top-down.
//Set to true if caller asks CFG optimizer to maintain DomInfo
//on the fly.
#define CFGOPTCTX_need_update_dominfo(x) ((x)->common_info.s1.m_update_dominfo)

//The field transfers information bottom-up.
//Record the number of time that iterate CFG vertex when updating DomInfo.
//This is local used variable to collect information bottom-up from callee
//to caller.
#define CFGOPTCTX_vertex_iter_time(x) ((x)->m_vertex_iter_time)

class CfgOptCtx {
    CfgOptCtx const& operator = (CfgOptCtx const&);
    void reinit()
    {
        common_info.m_flags = 0;
        CFGOPTCTX_need_update_dominfo(this) = true;
        CFGOPTCTX_do_merge_label(this) = true;
        CFGOPTCTX_vertex_iter_time(this) = 0;
    }
public:
    //The field transfers information bottom-up.
    //Record the number of time that iterate CFG vertex when updating DomInfo.
    //This is local used variable to collect information bottom-up from callee
    //to caller.
    UINT m_vertex_iter_time;

    //Record current OptCtx.
    OptCtx & oc;
    union {
        BYTE m_flags; //union set of optimization flags.
        struct {
            //The field transfers information top-down.
            //Set to true if caller asks CFG optimizer to maintain DomInfo
            //on the fly.
            BYTE m_update_dominfo:1;

            //The field transfers information top-down.
            //If it is true, CFG optimizer will attempt to merge label to
            //next BB if current BB is empty. Default is true.
            BYTE m_do_merge_label:1;
        } s1;
    } common_info;
public:
    CfgOptCtx(OptCtx & toc) : oc(toc) { reinit(); }
    CfgOptCtx(CfgOptCtx const& src) : oc(src.oc)
    { reinit(); copyTopDownInfo(src); }

    void copyTopDownInfo(CfgOptCtx const& src)
    { common_info = src.common_info; }

    //If it is true, CFG optimizer will attempt to merge label to
    //next BB if current BB is empty. Default is true.
    bool do_merge_label() const { return CFGOPTCTX_do_merge_label(this); }

    OptCtx & getOptCtx() { return oc; }

    //Return true if caller asks CFG optimizer to maintain DomInfo on the fly.
    bool needUpdateDomInfo() const
    { return CFGOPTCTX_need_update_dominfo(this) && oc.is_dom_valid(); }

    void setOptCtx(OptCtx const& loc) { oc.copy(loc); }

    //The function unify ctx information that collected by 'src'.
    void unionBottomUpInfo(CfgOptCtx const& src)
    { CFGOPTCTX_vertex_iter_time(this) += CFGOPTCTX_vertex_iter_time(&src); }
};


//NOTICE:
//1. For accelerating perform operation of each vertex, e.g
//   compute dominator, please try best to add vertex with
//   topological order.
//NOTE: BB should define and implement method 'id()' and member field 'm_rpo'.
template <class BB, class XR> class CFG : public xcom::DGraph {
    COPY_CONSTRUCTOR(CFG);
protected:
    UINT m_has_eh_edge:1;
    UINT m_li_count:31; //counter to loop.
    SEQ_TYPE m_bb_sort_type;
    LI<BB> * m_loop_info; //Loop information
    List<BB*> * m_bb_list;
    BB * m_entry; //CFG Graph entry.
    RPOVexList * m_rpo_vexlst; //cache and record Vertex in reverse-post-order.
    xcom::BitSetMgr * m_bs_mgr;
    SMemPool * m_pool;
    List<BB*> m_exit_list; //CFG Graph ENTRY list
protected:
    LI<BB> * allocLoopInfo()
    {
        LI<BB> * li = (LI<BB>*)xmalloc(sizeof(LI<BB>));
        LI_id(li) = m_li_count++;
        return li;
    }

    //Build a loopinfo.
    //bbset: record all BBs inside the loop, include the head.
    LI<BB> * buildLoopInfo(xcom::BitSet * bbset, BB * head)
    {
        ASSERT0(bbset && head);
        LI<BB> * li = allocLoopInfo();
        LI_bb_set(li) = bbset;
        LI_loop_head(li) = head;
        return li;
    }

    //Collect loop info e.g: loop has call, loop has goto.
    void collectLoopInfo() { collectLoopInfoRecur(m_loop_info); }
    void cloneRPOVexList(CFG<BB, XR> const& src);
    void cloneExitList(CFG<BB, XR> const& src);
    void cloneEntry(CFG<BB, XR> const& src);

    //Clean loopinfo structure before recompute loop info.
    void cleanLoopInfo();
    void computeRPOImpl(xcom::BitSet & is_visited, IN xcom::Vertex * v,
                        OUT INT & order);
    inline void collectLoopInfoRecur(LI<BB> * li);

    inline bool isLoopHeadRecur(LI<BB> * li, BB * bb);
    bool insertLoopTree(LI<BB> ** lilist, LI<BB> * loop);
    void identifyNaturalLoop(UINT x, UINT y, MOD xcom::BitSet & loop,
                             List<UINT> & tmp);

    virtual void removeRPO(BB * bb)
    {
        if (m_rpo_vexlst != nullptr) {
            m_rpo_vexlst->remove(bb->getVex());
        }
        xcom::RPOVal rpo = bb->getVex()->rpo();
        if (rpo != RPO_UNDEF) {
            getRPOMgr().freeRPO(rpo);
        }
    }

    void sortByDFSRecur(List<BB*> & new_bbl, BB * bb, Vector<bool> & visited);

    void * xmalloc(size_t size)
    {
        ASSERT0(m_pool);
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, size);
        return p;
    }
public:
    CFG(List<BB*> * bb_list, UINT vertex_hash_size = 16)
        : xcom::DGraph(vertex_hash_size)
    {
        ASSERTN(bb_list, ("CFG requires BB list"));
        m_bb_list = bb_list;
        m_loop_info = nullptr;
        m_bs_mgr = nullptr;
        m_li_count = 1;
        m_has_eh_edge = false;
        m_rpo_vexlst = nullptr;
        m_entry = nullptr; //entry will be computed during CFG::build().
        m_pool = smpoolCreate(sizeof(CFGEdgeInfo) * 4, MEM_COMM);
        set_dense(true); //We think CFG is always dense graph.
    }
    virtual ~CFG()
    {
        smpoolDelete(m_pool);
        freeRPOVexList();
        cleanBBVertex();
    }

    virtual void addBreakOutLoop(BB * loop_head, xcom::BitSet & body_set);

    //Build the CFG according to BB list.
    void build(OptCtx & oc);

    //The function clones almost all contents about graph, loopinfo, and CFG
    //related information from 'src', except the BB list.
    void clone(CFG<BB, XR> const& src, bool clone_edge_info,
               bool clone_vex_info);

    //uni: the universe set.
    bool computeDom(xcom::BitSet const* uni)
    {
        ASSERTN(m_entry, ("Not found entry"));
        RPOVexList vlst;
        computeRPO(m_entry->getVex(), vlst);
        return xcom::DGraph::computeDom(&vlst, uni);
    }

    //CFG may have multiple exit. The method computing pdom is not
    //different with dom.
    bool computePdom(xcom::BitSet const* uni)
    {
        //ASSERTN(m_exit_list.get_elem_count() == 1,
        //   ("ONLY support SESE or SEME"));
        ASSERTN(m_entry, ("Not found entry"));
        return xcom::DGraph::computePdomByRPO(m_entry->getVex(), uni);
    }

    //Compute all reachable BBs start from 'startbb'.
    //bbset: record the output result.
    //Note caller should clean bbset. This function is non-recursive.
    void computeReachableBBSet(BB * startbb, OUT xcom::BitSet & bbset);

    //Compute all reachable BBs start from 'startbb'.
    //bbset: record the output result.
    //Note caller should clean bbset.
    //This function is different to computeReachableBBSet, it only
    //collect BBs in main stream control flow, BBs which in
    //exception handler region are omitted.
    //This function is non-recursive.
    void computeMainStreamBBSet(BB * startbb, OUT xcom::BitSet & bbset);

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
            if (isCFGExit(bb)) {
                m_exit_list.append_tail(bb);
            }
        }
    }
    void cleanBBVertex();
    void computeRPO(OptCtx & oc);
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = sizeof(*this);
        //count += m_bb_list.count_mem(); //do NOT count up BBs in bb_list.
        count += m_exit_list.count_mem();
        return count;
    }

    void dumpLoopTree(LI<BB> const* looplist, UINT indent,
                      Region const* rg) const;
    virtual void dumpLoopInfo(Region const* rg) const
    {
        if (!rg->isLogMgrInit()) { return; }
        note(rg, "\n==---- DUMP Natural Loop Info ----==");
        dumpLoopTree(m_loop_info, 0, rg);
    }
    virtual void dumpVCG(CHAR const* name = nullptr) const;

    //Dump Dom Info to dump file.
    void dumpDomSet(Region * rg) const
    {
        //Do not dump if LogMr is not initialized.
        if (!rg->isLogMgrInit()) { return; }
        xcom::StrBuf buf(32);
        xcom::DGraph::dumpDom(buf);
        note(rg, "%s", buf.getBuf());
    }

    //Dump Dom Info to dump file and dump dom-tree graph into file.
    void dumpDomTree(Region * rg, bool dump_dom_tree,
                     bool dump_pdom_tree) const
    {
        //Do not dump if LogMr is not initialized.
        if (!rg->isLogMgrInit()) { return; }
        xcom::DGraph::dumpDom(rg->getLogMgr()->getFileHandler(),
                              dump_dom_tree, dump_pdom_tree);
    }

    void freeRPOVexList()
    {
        if (m_rpo_vexlst != nullptr) {
            delete m_rpo_vexlst;
            m_rpo_vexlst = nullptr;
        }
    }
    bool findLoop();

    //Find the single exit BB if exist for given loop.
    //li: represents a loop.
    //exitedge: return the exitedge if needed. It can be NULL.
    BB * findSingleExitBB(LI<BB> const* li, Edge const** exitedge = nullptr);

    //Find the target bb list.
    //2th parameter records a list of bb have found.
    virtual void findTargetBBOfMulticondBranch(XR const*, OUT List<BB*> &) = 0;

    //Find the bb that referred given label.
    virtual BB * findBBbyLabel(LabelInfo const*) const = 0;

    //Find a list of bb that referrence labels which are target of xr.
    //2th parameter records a list of bb have found.
    virtual void findTargetBBOfIndirectBranch(XR const*, OUT List<BB*> &) = 0;

    xcom::List<BB*> * getBBList() const { return m_bb_list; }
    xcom::BitSetMgr * getBitSetMgr() const { return m_bs_mgr; }
    UINT getLoopNum() const { return m_li_count - 1; }
    void get_preds(MOD List<BB*> & preds, BB const* bb) const;
    void get_preds(MOD List<BB const*> & preds, BB const* bb) const;
    void get_preds(MOD List<UINT> & predid, BB const* bb) const;
    void get_succs(MOD List<BB*> & succs, BB const* bb) const;
    void get_succs(MOD List<BB const*> & succs, BB const* bb) const;
    void get_succs(MOD List<UINT> & succid, BB const* bb) const;

    //Return the Nth successor, n begins at 0.
    BB * getNthSucc(BB const* bb, UINT n) const
    {
        xcom::Vertex const* vex = bb->getVex()->getNthOutVertex(n);
        ASSERT0(vex);
        return getBB(vex->id());
    }

    //Return the Nth predecessor, n begins at 0.
    BB * getNthPred(BB const* bb, UINT n) const
    {
        xcom::Vertex const* vex = bb->getVex()->getNthInVertex(n);
        ASSERT0(vex);
        return getBB(vex->id());
    }

    //Get the number of successors of bb.
    UINT getSuccsNum(BB const* bb) const
    { return bb->getVex()->getOutDegree(); }

    //Get the number of predecessors of bb.
    UINT getPredsNum(BB const* bb) const { return bb->getVex()->getInDegree(); }

    //Get CFG entry BB.
    BB * getEntry() const { return m_entry; }

    //Get CFG exit BB list.
    List<BB*> * getExitList() { return &m_exit_list; }
    RPOVexList * getRPOVexList() { return m_rpo_vexlst; }

    //Return the fallthrough BB of 'bb'.
    BB * getFallThroughBB(BB * bb);

    //Return the previous BB that fallthrough to 'bb'.
    BB * getFallThroughPrevBB(BB const* bb);

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
    BB * get_first_succ(BB const* bb) const;
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
    LI<BB> * getLoopInfo() const { return m_loop_info; }

    //Return the last instruction of BB.
    virtual XR * get_last_xr(BB *) = 0;

    //Return the first instruction of BB.
    virtual XR * get_first_xr(BB *) = 0;
    virtual BB * getBB(UINT id) const = 0;

    //True if current CFG has exception-handler edge.
    bool hasEHEdge() const { return m_has_eh_edge; }

    bool isCFGEntry(BB * bb) const
    { return xcom::Graph::is_graph_entry(bb->getVex()); }

    //Return true if BB is empty.
    //Note if BB has phi, it is not empty.
    virtual bool isEmptyBB(BB * bb) const
    {
        return const_cast<CFG<BB, XR>*>(this)->get_last_xr(bb) == nullptr &&
               !bb->hasMDPhi(this);
    }

    //Return true if bb is exit BB of CFG.
    bool isCFGExit(BB * bb) const
    { return xcom::Graph::is_graph_exit(bb->getVex()); }

    //Return true if bb is entry BB of function-region.
    //In some case, BB is not Region entry even if it is the CFG entry.
    virtual bool isRegionEntry(BB *) const = 0;

    //Return true if bb is exit BB of function.
    //In some case, BB is not region-exit even if it is the CFG exit.
    virtual bool isRegionExit(BB *) const = 0;

    virtual bool isLoopHead(BB * bb)
    { return isLoopHeadRecur(m_loop_info, bb); }

    //Return true if BB 'pred' control the execution of 'bb'.
    //Note 'pred' must be predecessor of 'bb'.
    bool isControlPred(BB const* pred, BB const* bb) const
    {
        return const_cast<CFG<BB, XR>*>(this)->get_ipdom(const_cast<BB*>(pred))
               != bb;
    }

    void removeLoopInfo(LI<BB>* loop);
    bool reinsertLoopTree(LI<BB> ** lilist, LI<BB>* loop);

    //Insert unconditional branch to revise fall through BB.
    //e.g: Given bblist is bb1=>bb2=>bb3=>bb4, where bb4 is exit-BB,
    //and control flow edges are: bb1->bb2->bb3->bb4, bb1->bb3,
    //where bb1->bb2, bb2->bb3, bb3->bb4 are fallthrough edge.
    //
    //Assuming the reordered bblist is bb1=>bb3=>bb4=>bb2, the
    //associated control flow edges are: bb1->bb3->bb4, bb2->bb3.
    //It is obviously that bb1->bb3 should be converted to be fallthrough,
    //and bb1->bb2 to be conditional branch, bb2->bb3 to be unconditional
    //branch.
    virtual void reviseFallthrough(List<BB*> & new_bbl)
    { ASSERTN(0, ("Target Dependent Code")); }

    //Remove xr that in bb.
    virtual void remove_xr(BB *, XR *, CfgOptCtx const& ctx) = 0;

    //You should clean the relation between Label and BB before remove BB.
    virtual void removeDomInfo(xcom::C<BB*> * bbcontainer,
                               MOD CfgOptCtx & ctx) = 0;
    virtual void removeBB(BB * bb, OUT CfgOptCtx & ctx) = 0;
    virtual void removeBB(xcom::C<BB*> * bbcontainer,
                          OUT CfgOptCtx & ctx) = 0;
    virtual void removeMapBetweenLabelAndBB(BB * bb) = 0;

    //Rebuild CFG.
    void rebuild(OptCtx & oc)
    {
        erase();
        UINT newsz = MAX(16, getNearestPowerOf2(m_bb_list->get_elem_count()));
        resize(newsz);
        cleanBBVertex();
        build(oc);
        //NOTE:one should invoke removeEmptyBB() immediately after this
        //function return, because the rebuilding of CFG may generate
        //redundant empty BBs, thus it will disturb the computation of
        //entry and exit.
    }

    //Set RPO for BB.
    virtual void setRPO(BB * bb, INT order) = 0;
    virtual void setVertex(BB * bb, Vertex * v) = 0;
    virtual void setVertex(BB * from, BB * to, Edge * e) = 0;
    void sortByDFS();
    void sortByBFS();
    void sortByTopological();
    void setBBVertex();
    void setBitSetMgr(xcom::BitSetMgr * bs_mgr)
    {
        m_bs_mgr = bs_mgr;
        xcom::DGraph::setBitSetMgr(bs_mgr);
    }
    void setBBList(List<BB*> * bblst) { m_bb_list = bblst; }

    //Return true if find an order of RPO for 'bb' that
    //less than order of 'ref'.
    bool tryFindLessRPO(BB * bb, BB const* ref)
    {
        ASSERT0(bb->getVex() && ref->getVex());
        return getRPOMgr().tryFindLessRPO(bb->getVex(), ref->getVex());
    }

    //Perform verification if BB has been removed.
    bool verifyIfBBRemoved(CDG const* cdg, OptCtx const& oc) const;
    bool verifyRPOUniqueness() const;
    bool verify() const;
};


template <class BB, class XR>
void CFG<BB, XR>::dumpLoopTree(LI<BB> const* looplist, UINT indent,
                               Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    while (looplist != nullptr) {
        note(rg, "\n");
        for (UINT i = 0; i < indent; i++) { prt(rg, " "); }
        ASSERT0(LI_loop_head(looplist));
        prt(rg, "LOOP%d HEAD:BB%d, BODY:", looplist->id(),
            LI_loop_head(looplist)->id());
        if (LI_bb_set(looplist) != nullptr) {
            for (BSIdx i = LI_bb_set(looplist)->get_first();
                 i != BS_UNDEF; i = LI_bb_set(looplist)->get_next((UINT)i)) {
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
        ASSERTN(visited.get(bbc->id()), ("unreachable BB"));
    }
    #endif
    reviseFallthrough(new_bbl);
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
        ASSERTN(visited.get(bbc->id()), ("unreachable BB"));
    }
    #endif
    reviseFallthrough(new_bbl);
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
    for (VecIdx i = 0; i <= vex_vec.get_last_idx(); i++) {
        Vertex * v = vex_vec.get(i);
        ASSERT0(v);
        m_bb_list->append_tail(getBB(v->id()));
    }
    reviseFallthrough(*m_bb_list);
    m_bb_sort_type = SEQ_TOPOL;
}


//Return all successors.
template <class BB, class XR>
void CFG<BB, XR>::get_succs(MOD List<BB*> & succs, BB const* bb) const
{
    ASSERT0(bb);
    xcom::Vertex const* vex = bb->getVex();
    succs.clean();
    for (xcom::EdgeC const* el = vex->getOutList();
         el != nullptr; el = el->get_next()) {
        succs.append_tail(getBB(el->getToId()));
    }
}


//Return all successors.
template <class BB, class XR>
void CFG<BB, XR>::get_succs(MOD List<BB const*> & succs, BB const* bb) const
{
    ASSERT0(bb);
    xcom::Vertex const* vex = bb->getVex();
    succs.clean();
    for (xcom::EdgeC const* el = vex->getOutList();
        el != nullptr; el = el->get_next()) {
        succs.append_tail(getBB(el->getToId()));
    }
}


//Return all successors.
template <class BB, class XR>
void CFG<BB, XR>::get_succs(MOD List<UINT> & succid, BB const* bb) const
{
    ASSERT0(bb);
    xcom::Vertex const* vex = bb->getVex();
    succid.clean();
    for (xcom::EdgeC const* el = vex->getOutList();
        el != nullptr; el = el->get_next()) {
        succid.append_tail(el->getToId());
    }
}


//Return all predecessors.
template <class BB, class XR>
void CFG<BB, XR>::get_preds(MOD List<BB*> & preds, BB const* bb) const
{
    ASSERT0(bb);
    xcom::Vertex * vex = bb->getVex();
    ASSERT0(vex);
    preds.clean();
    for (xcom::EdgeC * el = vex->getInList();
         el != nullptr; el = el->get_next()) {
        preds.append_tail(getBB(el->getFromId()));
    }
}


//Return all predecessors.
template <class BB, class XR>
void CFG<BB, XR>::get_preds(MOD List<BB const*> & preds, BB const* bb) const
{
    ASSERT0(bb);
    xcom::Vertex * vex = bb->getVex();
    preds.clean();
    for (xcom::EdgeC * el = vex->getInList();
         el != nullptr; el = el->get_next()) {
        preds.append_tail(getBB(el->getFromId()));
    }
}


//Return all predecessors id.
template <class BB, class XR>
void CFG<BB, XR>::get_preds(MOD List<UINT> & predid, BB const* bb) const
{
    ASSERT0(bb);
    xcom::Vertex * vex = bb->getVex();
    predid.clean();
    for (xcom::EdgeC * el = vex->getInList();
         el != nullptr; el = el->get_next()) {
        predid.append_tail(el->getFromId());
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
                Edge * e = DGraph::addEdge(bb->id(), next->id());
                setVertex(bb, next, e);
            } else {
                Vertex * v = addVertex(bb->id());
                setVertex(bb, v);
            }
            continue;
        }
        if (last->is_terminate()) {
            //Meet region boundary.
            continue;
        }
        if (last->is_call()) {
            //Add fall-through edge
            //The last bb may not be terminated by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                Edge * e = DGraph::addEdge(bb->id(), next->id());
                setVertex(bb, next, e);
            }
            continue;
        }
        if (last->isConditionalBr()) {
            //Add fall-through edge.
            //The last bb may not be terminated by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                Edge * e = DGraph::addEdge(bb->id(), next->id());
                setVertex(bb, next, e);
            }
            //Add edge between source BB and target BB.
            BB * target_bb = findBBbyLabel(last->getLabel());
            ASSERTN(target_bb != nullptr, ("target cannot be nullptr"));
            Edge * e = DGraph::addEdge(bb->id(), target_bb->id());
            setVertex(bb, target_bb, e);
            continue;
        }
        if (last->isMultiConditionalBr()) {
            //Add fall-through edge.
            //The last bb may not be terminated by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                Edge * e = DGraph::addEdge(bb->id(), next->id());
                setVertex(bb, next, e);
            }

            //Add edge between source BB and multi-target BBs.
            tgt_bbs.clean();
            findTargetBBOfMulticondBranch(last, tgt_bbs);

            xcom::C<BB*> * ct2;
            for (tgt_bbs.get_head(&ct2);
                 ct2 != tgt_bbs.end(); ct2 = tgt_bbs.get_next(ct2)) {
                BB * tbb = ct2->val();
                Edge * e = DGraph::addEdge(bb->id(), tbb->id());
                setVertex(bb, tbb, e);
            }
            continue;
        }
        if (last->isUnconditionalBr()) {
            if (last->isIndirectBr()) {
                //Add edge to every target of current BB.
                tgt_bbs.clean();
                findTargetBBOfIndirectBranch(last, tgt_bbs);
                xcom::C<BB*> * ct2;
                for (tgt_bbs.get_head(&ct2);
                     ct2 != tgt_bbs.end(); ct2 = tgt_bbs.get_next(ct2)) {
                    BB * t = ct2->val();
                    Edge * e = DGraph::addEdge(bb->id(), t->id());
                    setVertex(bb, t, e);
                }
                continue;
            }
            //Add edge between source BB and target BB.
            BB * target_bb = findBBbyLabel(last->getLabel());
            ASSERTN(target_bb != nullptr, ("target cannot be nullptr"));
            Edge * e = DGraph::addEdge(bb->id(), target_bb->id());
            setVertex(bb, target_bb, e);
            continue;
        }
        if (!last->is_return()) {
            //Add fall-through edge.
            //The last bb may not end by 'return' stmt.
            if (next != nullptr && !next->is_terminate()) {
                Edge * e = DGraph::addEdge(bb->id(), next->id());
                setVertex(bb, next, e);
            }
            continue;
        }
        Vertex * v = addVertex(bb->id());
        setVertex(bb, v);
    }
    oc.setValidPass(PASS_CFG);
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
    for (BSIdx id = bbset->get_first(); id != BS_UNDEF;
         id = bbset->get_next(id)) {
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
    for (BSIdx i = body_set.get_first();
         i != BS_UNDEF; i = body_set.get_next((UINT)i)) {
        if (i == (BSIdx)loop_head->id()) { continue; }
        xcom::Vertex * v = getVertex((UINT)i);
        ASSERT0(v);
        xcom::EdgeC * out = v->getOutList();
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


template <class BB, class XR>
void CFG<BB, XR>::cleanLoopInfo()
{
    LI<BB> * li = getLoopInfo();
    if (li == nullptr) { return; }
    List<LI<BB>*> worklst;
    for (; li != nullptr; li = LI_next(li)) {
        worklst.append_tail(li);
    }
    while (worklst.get_elem_count() > 0) {
        LI<BB> * x = worklst.remove_head();
        m_bs_mgr->free(LI_bb_set(x));
        for (LI<BB> * y = LI_inner_list(x); y != nullptr; y = LI_next(y)) {
            worklst.append_tail(y);
        }
    }
    m_loop_info = nullptr;
}


//Find the single exit BB if exist for given loop.
//li: represents a loop.
//exitedge: return the exitedge if needed. It can be NULL.
template <class BB, class XR>
BB * CFG<BB, XR>::findSingleExitBB(LI<BB> const* li, Edge const** exitedge)
{
    //A BB Set is used in the LoopInfo to describing the loop body BB set.
    xcom::BitSet * bbset = li->getBodyBBSet();
    ASSERT0(bbset);
    BSIdx exit = BS_UNDEF;
    for (BSIdx id = bbset->get_first(); id != BS_UNDEF;
         id = bbset->get_next(id)) {
        Vertex const* vex = getVertex(id);
        ASSERT0(vex);
        for (EdgeC const* ec = vex->getOutList(); ec != nullptr;
             ec = ec->get_next()) {
            UINT succ = ec->getTo()->id();
            if (!bbset->is_contain(succ)) {
                if (exit == BS_UNDEF) {
                    //Record the exit BB has been found.
                    exit = succ;
                    if (exitedge != nullptr) {
                        *exitedge = ec->getEdge();
                    }
                } else if ((UINT)exit != succ) {
                    //There are more than one exit BB for current loop.
                    return nullptr;
                }
            }
        }
    }
    return exit != BS_UNDEF ? getBB(exit) : nullptr;
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
        xcom::Vertex * vex = bb->getVex();
        ASSERT0(vex);
        for (xcom::EdgeC * el = vex->getOutList();
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
            li = buildLoopInfo(loop, succ);
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
void CFG<BB, XR>::dumpVCG(CHAR const* name) const
{
    if (!name) {
        name = "graph_cfg.vcg";
    }
    FileObj fo(name, true, false);
    FILE * hvcg = fo.getFileHandler();
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
        xcom::Vertex * v = bb->getVex();
        fprintf(hvcg, " BB%d ", bb->id());
        fprintf(hvcg, " rpo:%d", v->rpo());
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
}


template <class BB, class XR>
void CFG<BB, XR>::computeRPOImpl(MOD xcom::BitSet & is_visited,
                                 IN xcom::Vertex * v, MOD INT & order)
{
    is_visited.bunion(v->id());
    xcom::AdjVertexIter it;
    for (xcom::Vertex * succ = Graph::get_first_out_vertex(v, it);
         succ != nullptr; succ = Graph::get_next_out_vertex(it)) {
        ASSERTN(getBB(succ->id()), ("without bb corresponded"));
        if (!is_visited.is_contain(succ->id())) {
            computeRPOImpl(is_visited, succ, order);
        }
    }
    VERTEX_rpo(v) = order;
    order -= RPO_INTERVAL;
}


//Compute rev-post-order.
template <class BB, class XR>
void CFG<BB, XR>::computeRPO(OptCtx & oc)
{
    if (m_bb_list->get_elem_count() == 0) { return; }

    START_TIMER(t, "Compute RPO");

    #ifdef _DEBUG_
    //Only for verification in debug mode.
    for (BB * bb = m_bb_list->get_head();
         bb != nullptr; bb = m_bb_list->get_next()) {
        setRPO(bb, RPO_UNDEF);
    }
    #endif

    xcom::BitSet is_visited;
    ASSERTN(m_entry, ("Not find entry"));

    if (m_rpo_vexlst == nullptr) { m_rpo_vexlst = new RPOVexList(); }

    #ifdef RECURSIVE_ALGO
    INT order = RPO_INIT_VAL + m_bb_list->get_elem_count() * RPO_INTERVAL;
    computeRPOImpl(is_visited, m_entry->getVex(), order);
    #else
    getRPOMgr().computeRPO(*this, m_entry->getVex(), *m_rpo_vexlst);
    #endif
    oc.setValidPass(PASS_RPO);
    END_TIMER(t, "Compute RPO");
}


template <class BB, class XR>
void CFG<BB, XR>::setBBVertex()
{
    xcom::C<BB*> * ct;
    if (m_bb_list == nullptr) { return; }
    for (m_bb_list->get_head(&ct);
         ct != nullptr; ct = m_bb_list->get_next(ct)) {
        BB * bb = ct->val();
        Vertex * v = getVertex(bb->id());
        ASSERT0(v);
        setVertex(bb, v);
    }
}


template <class BB, class XR>
void CFG<BB, XR>::cloneEntry(CFG<BB, XR> const& src)
{
    ASSERT0(this != &src);
    if (m_bb_list == nullptr || src.m_entry == nullptr) { return; }
    if (m_bb_list == src.m_bb_list) {
        m_entry = src.m_entry;
        return;
    }
    xcom::C<BB*> * ct;
    m_entry = nullptr;
    for (BB * bb = m_bb_list->get_head(&ct); bb != nullptr;
         bb = m_bb_list->get_next(&ct)) {
        if (bb->id() == src.m_entry->id()) {
            m_entry = bb;
            return;
        }
    }
    ASSERT0(m_entry);
}


template <class BB, class XR>
void CFG<BB, XR>::cloneExitList(CFG<BB, XR> const& src)
{
    ASSERT0(this != &src);
    if (m_bb_list == nullptr) { return; }
    if (m_bb_list == src.m_bb_list) {
        m_exit_list.copy(src.m_exit_list);
        return;
    }
    xcom::C<BB*> * ct;
    for (BB * s = src.m_exit_list.get_head(&ct); s != nullptr;
         s = src.m_exit_list.get_next(&ct)) {
        BB * t = nullptr;
        xcom::C<BB*> * ct2;
        for (t = m_bb_list->get_head(&ct2);
             t != nullptr; t = m_bb_list->get_next(&ct2)) {
            if (t->id() == s->id()) {
                break;
            }
        }
        ASSERT0(t);
        m_exit_list.append_tail(t);
    }
}


template <class BB, class XR>
void CFG<BB, XR>::cloneRPOVexList(CFG<BB, XR> const& src)
{
    ASSERT0(this != &src);
    if (src.m_rpo_vexlst == nullptr) {
        freeRPOVexList();
        return;
    }
    if (m_rpo_vexlst == nullptr) { m_rpo_vexlst = new RPOVexList(); }
    RPOVexListIter it;
    for (Vertex const* s = src.m_rpo_vexlst->get_head(&it);
         s != nullptr; s = src.m_rpo_vexlst->get_next(&it)) {
        Vertex const* t = getVertex(s->id());
        ASSERT0(t);
        m_rpo_vexlst->append_tail(t);
    }
}


template <class BB, class XR>
void CFG<BB, XR>::clone(CFG<BB, XR> const& src, bool clone_edge_info,
                        bool clone_vex_info)
{
    ASSERT0(this != &src);
    xcom::DGraph::clone(src, clone_edge_info, clone_vex_info);
    m_has_eh_edge = src.m_has_eh_edge;
    m_li_count = src.m_li_count;
    m_bb_sort_type = src.m_bb_sort_type;
    m_loop_info = src.m_loop_info;
    cloneEntry(src);
    cloneExitList(src);
    cloneRPOVexList(src);
}

//Get the first successor of bb.
template <class BB, class XR>
BB * CFG<BB, XR>::get_first_succ(BB const* bb) const
{
    ASSERT0(bb);
    xcom::Vertex * vex = bb->getVex();
    ASSERT0(vex);
    xcom::EdgeC * ec = vex->getOutList();
    if (ec == nullptr) { return nullptr; }
    BB * succ = getBB(ec->getToId());
    ASSERT0(succ);
    return succ;
}


template <class BB, class XR>
void CFG<BB, XR>::computeReachableBBSet(BB * startbb, OUT xcom::BitSet & bbset)
{
    ASSERT0(startbb);
    List<xcom::Vertex const*> wl;
    ASSERT0(startbb->getVex());
    wl.append_tail(startbb->getVex());
    xcom::Vertex const* v;
    while ((v = wl.remove_head()) != nullptr) {
        UINT id = v->id();
        if (bbset.is_contain(id)) { continue; }
        bbset.bunion(id);
        for (xcom::EdgeC * el = v->getOutList();
             el != nullptr; el = EC_next(el)) {
            xcom::Vertex const* succv = el->getTo();
            if (!bbset.is_contain(succv->id())) {
                wl.append_tail(succv);
            }
        }
    }
}


template <class BB, class XR>
void CFG<BB, XR>::computeMainStreamBBSet(BB * startbb, OUT xcom::BitSet & bbset)
{
    ASSERT0(startbb);
    List<xcom::Vertex const*> wl;
    ASSERT0(startbb->getVex());
    wl.append_tail(startbb->getVex());
    xcom::Vertex const* v;
    while ((v = wl.remove_head()) != nullptr) {
        UINT i = v->id();
        if (bbset.is_contain(i) || getBB(i)->isExceptionHandler()) {
            continue;
        }
        bbset.bunion(i);
        for (xcom::EdgeC * el = v->getOutList();
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


template <class BB, class XR>
void CFG<BB, XR>::cleanBBVertex()
{
    if (m_bb_list == nullptr) { return; }
    xcom::C<BB*> * ct;
    for (m_bb_list->get_head(&ct);
         ct != nullptr; ct = m_bb_list->get_next(ct)) {
        BB * bb = ct->val();
        bb->cleanVex();
    }
}


template <class BB, class XR>
BB * CFG<BB, XR>::getFallThroughBB(BB * bb)
{
    ASSERT0(bb);
    xcom::C<BB*> * ct;
    ASSERT0(m_bb_list->find(bb, &ct));
    m_bb_list->find(bb, &ct);
    return m_bb_list->get_next(&ct);
}


template <class BB, class XR>
BB * CFG<BB, XR>::getFallThroughPrevBB(BB const* bb)
{
    ASSERT0(bb);
    xcom::C<BB*> * ct;
    ASSERT0(m_bb_list->find(const_cast<BB*>(bb), &ct));
    m_bb_list->find(const_cast<BB*>(bb), &ct);
    return m_bb_list->get_prev(&ct);
}


template <class BB, class XR>
bool CFG<BB, XR>::verify() const
{
    //The entry node can not have any predecessors.
    ASSERT0(m_entry);
    xcom::Vertex const* vex = m_entry->getVex();
    ASSERT0_DUMMYUSE(vex && vex->getInDegree() == 0);

    //The exit node can not have successors.
    C<BB*> * it;
    xcom::TTab<UINT> bbid;
    for (BB const* bb = m_exit_list.get_head(&it);
         bb != nullptr; bb = m_exit_list.get_next(&it)) {
        xcom::Vertex const* vex2 = bb->getVex();
        ASSERT0_DUMMYUSE(vex2 && vex2->getOutDegree() == 0);
        ASSERTN(!bbid.find(bb->id()), ("bb should be unique in list"));
        bbid.append(bb->id());
    }

    //Check the BB list.
    if (m_bb_list == nullptr) { return true; }
    xcom::C<BB*> * ct;
    bbid.clean();
    for (BB * bb = m_bb_list->get_head(&ct); bb != nullptr;
         bb = m_bb_list->get_next(&ct)) {
        xcom::Vertex const* vex3 = bb->getVex();
        ASSERT0_DUMMYUSE(vex3);
        ASSERTN(!bbid.find(bb->id()), ("bb should be unique in list"));
        bbid.append(bb->id());
    }
    return true;
}


//Verify RPO to given region.
template <class BB, class XR>
bool CFG<BB, XR>::verifyRPOUniqueness() const
{
    TMap<UINT, UINT> rpotab;
    xcom::C<BB*> * ct;
    for (BB * bb = m_bb_list->get_head(&ct);
         bb != nullptr; bb = m_bb_list->get_next(&ct)) {
        if (bb->rpo() == RPO_UNDEF) {
            UNREACHABLE();
            return false;
        }
        bool find = false;
        UINT before_bbid = rpotab.get(bb->rpo(), &find);
        ASSERTN(!find, ("duplicated RPO to %d", before_bbid));
        rpotab.set(bb->rpo(), bb->id());
    }
    return true;
}

} //namespace xoc
#endif
