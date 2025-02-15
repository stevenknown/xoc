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
#ifndef _LOOP_H_
#define _LOOP_H_

namespace xoc {

#define LOOPINFO_UNDEF 0

class IRBB;
class Region;
class IRCFG;
template <class BB> class LI;

//List of invariant stmt.
typedef xcom::EList<IR*, IR2Holder> InvStmtList;
typedef xcom::EList<IR*, IR2Holder>::Iter InvStmtListIter;
typedef xcom::List<LI<IRBB>*> LoopInfoIter;
typedef xcom::List<LI<IRBB> const*> CLoopInfoIter;

typedef enum tagFindRedOpResult {
    OP_UNDEF = 0, //Undef status.
    OP_IS_RED, //OP is reduction operation.
    OP_IS_NOT_RED, //OP is definitely not reduction operation.

    //OP may be reduction operation because there exists at least one cycle
    //in DefUse chain. However, since the May-Dependence information is not
    //exact enough, we can not guarantee there exist definitely a reduction
    //operation in the loop.
    OP_HAS_CYCLE,
    OP_UNKNOWN, //I don't know whether OP is reduction operation.
} FindRedOpResult;

//
//START LI<BB>
//
//This file represent loop tree structure and relate algorithms.
//e.g:
//for (i) { #Loop1
//  for (j) { #Loop2
//    ...
//  }
//  ...
//  for (k) { #Loop3
//    ...
//  }
//}
//for (m) { #Loop4
//  ...
//  for (n) { #Loop5
//    ...
//  }
//}
//LoopInfo Tree:
//  Loop1
//   |-Loop2
//   |-Loop3
//
//  Loop4
//   |-Loop5

//CFG Loop Info.
#define LI_id(li) ((li)->uid)
#define LI_next(li) ((li)->next)
#define LI_prev(li) ((li)->prev)
#define LI_has_early_exit(li) ((li)->has_early_exit)
#define LI_has_call(li) ((li)->has_call)
#define LI_inner_list(li) ((li)->inner_list)
#define LI_outer(li) ((li)->outer)
#define LI_bb_set(li) ((li)->bb_set)
#define LI_loop_head(li) ((li)->loop_head)
template <class BB> class LI {
    COPY_CONSTRUCTOR(LI);
public:
    bool has_early_exit;
    bool has_call;
    UINT uid;
    LI * next;
    LI * prev;
    LI * inner_list; //inner loop list
    LI * outer; //outer loop

    //Loop head node, the only one header indicates a natural loop.
    BB * loop_head;
    xcom::BitSet * bb_set; //loop body elements
public:
    LI() {}

    //Add bbid to all outer loops except current loop.
    void addBBToAllOuterLoop(UINT bbid) const;

    //Add bbid to the current loop and all outer loops.
    void addBBToLoopAndAllOuterLoop(UINT bbid) const;

    //Return true if ir in bbid is at least execute once from loophead,
    //otherwise return false means unknown.
    bool atLeastExecOnce(UINT bbid, xcom::DGraph const* cfg) const;

    //Find the BB that is the start of the unqiue backedge of loop.
    //  BB1: loop start bb
    //  BB2: body
    //  BB3: goto loop start bb
    //BB3 is the backedge start bb.
    UINT findBackEdgeStartBB(xcom::Graph const* cfg) const;

    //Find the first BB that is the END of loop. The end BB is outside of loop.
    //Note there could be multiple end BB if the last IR of head is
    //multipl-conditional branch, namely, SWTICH or IGOTO.
    //  BB1: loop start BB
    //       falsebr END
    //  BB2: body
    //  BB3: goto loop start BB
    //  BB4: END
    //BB4 is the loopend BB.
    UINT findFirstLoopEndBB(xcom::Graph * cfg) const;

    //Find all END BB of loop. End BB is outside of loop.
    //Note there could be multiple end BB if the last IR of head is
    //multipl-conditional branch, namely, SWTICH or IGOTO.
    //  BB1: loop start bb
    //  BB2: body
    //  BB3: falsebr cond1 END
    //  BB4: falsebr cond2 END
    //  BB5: falsebr cond3 END
    //  BB6: goto loop start bb
    //  BB7: END
    //BB3, BB4, BB5 are loopend BBs.
    void findAllLoopEndBB(xcom::Graph const* cfg,
                          OUT List<UINT> & endlst) const;

    LI<BB> * getOuter() const { return outer; }
    LI<BB> * getInnerList() const { return inner_list; }

    //Return the number of loops that rooted by current loop.
    UINT getLoopNum() const;
    BB * getLoopHead() const { return loop_head; }
    BitSet * getBodyBBSet() const { return bb_set; }
    LI<BB> * get_next() const { return next; }
    LI<BB> * get_prev() const { return prev; }

    bool hasEarlyExit() const { return has_early_exit; }
    bool hasCall() const { return has_call; }

    UINT id() const { return uid; }
    bool isLoopReduction() const { return !has_early_exit; }
    bool isOuterMost() const { return getOuter() == nullptr; }
    bool isInnerMost() const { return getInnerList() == nullptr; }

    //The function performs a complete comparison between current LoopInfo and
    //'src'.
    //Return true if current loop is complete equal to 'src'.
    bool isEqual(LI<BB> const* src) const;

    //The function compares current LoopInfo with given LoopInfo 'src' in
    //term of Loop Structure.
    //Return true if current loop is isomophic to 'src' in Loop Structure.
    //e.g: current loop info is: loophead is BB2, loopbody is {2,3,5,6},
    //     where src's loop info is: loophead is BB2, loopbody is {2,3,5},
    //     the function return false because the loopbody is not equal.
    bool isLoopStructEqual(LI<BB> const* src) const;

    //The function compares two LoopInfo trees in term of Loop Structure.
    //Return true if li1 and li2 are isomophic equal in Loop Structure.
    //e.g: given two LoopInfo LI1 and LI2, they are isomophic equal even if
    //the LI's id are not equal.
    //  LI1:
    //      LOOP1 HEAD:BB2, BODY:2,3,11,13,
    //      LOOP2 HEAD:BB4, BODY:4,5,6,7,
    //  LI2:
    //      LOOP3 HEAD:BB2, BODY:2,3,11,13,
    //      LOOP4 HEAD:BB4, BODY:4,5,6,7,
    static bool isLoopInfoTreeEqual(LI<BB> const* li1, LI<BB> const* li2);

    //The function compares two LoopInfo trees in term of Loop Structure.
    //Return true if li1 and li2 are approximately equal in Loop Structure.
    //NOTE: two LoopInfos are always approximately equal if they are
    //isomophic equal, but the converse is not true.
    //e.g: given two LoopInfos LI1 and LI2, they are approximately equal if
    //there is a isomophic equal LoopInfo in the same loop-level for given
    //two LoopInfos.
    //  LI1:
    //      LOOP1 HEAD:BB2, BODY:2,3,11,13,
    //        LOOP1 HEAD:BB11, BODY:11,13,
    //      LOOP2 HEAD:BB4, BODY:4,5,6,7,
    //  LI2:
    //      LOOP3 HEAD:BB4, BODY:4,5,6,7,
    //      LOOP4 HEAD:BB2, BODY:2,3,11,13,
    //        LOOP7 HEAD:BB11, BODY:11,13,
    static bool isLoopInfoTreeApproEqual(LI<BB> const* li1, LI<BB> const* li2);

    //Return true if bbid is belong to current loop.
    //bbid: id of BB.
    bool isInsideLoop(UINT bbid) const
    { return LI_bb_set(this)->is_contain(bbid); }

    //Return true if bbid is belong to the LoopInfo tree that rooted by
    //current loop.
    //bbid: id of BB.
    //access_sibling: if true to compare the sibling LoopInfo as well.
    bool isInsideLoopTree(UINT bbid, OUT UINT & nestlevel,
                          bool access_sibling) const;

    //Clean adjacent relation in loop-tree.
    void cleanAdjRelation()
    {
        LI_outer(this) = nullptr;
        LI_next(this) = nullptr;
        LI_prev(this) = nullptr;
    }

    void dump(Region const* rg) const
    {
        note(rg, "\nLOOP%u HEAD:BB%u, BODY:", id(), getLoopHead()->id());
        if (getBodyBBSet() != nullptr) {
            for (BSIdx i = getBodyBBSet()->get_first();
                 i != BS_UNDEF; i = getBodyBBSet()->get_next((UINT)i)) {
                prt(rg, "%u,", i);
            }
        }
    }

    void dumpLoopTree(LI<BB> const* looplist, MOD LogMgr * lm,
                      UINT indent) const
    {
        if (!lm->is_init()) { return; }
        while (looplist != nullptr) {
            note(lm, "\n");
            for (UINT i = 0; i < indent; i++) { prt(lm, " "); }
            ASSERT0(looplist->getLoopHead());
            prt(lm, "LOOP%d HEAD:BB%d, BODY:", looplist->id(),
                looplist->getLoopHead()->id());
            if (looplist->getBodyBBSet() != nullptr) {
                for (BSIdx i = looplist->getBodyBBSet()->get_first();
                     i != BS_UNDEF;
                     i = looplist->getBodyBBSet()->get_next((UINT)i)) {
                    prt(lm, "%d,", i);
                }
            }
            dumpLoopTree(looplist->getInnerList(), lm, indent + 2);
            looplist = looplist->get_next();
        }
    }
    void dumpLoopTree(MOD LogMgr * lm, UINT indent = 0) const
    { dumpLoopTree(this, lm, indent); }
};


template <class BB>
bool LI<BB>::isLoopStructEqual(LI<BB> const* src) const
{
    ASSERT0(src);
    return this == src ||
           (getBodyBBSet()->is_equal(*src->getBodyBBSet()) &&
            getLoopHead() == src->getLoopHead());
}


template <class BB>
bool LI<BB>::isEqual(LI<BB> const* src) const
{
    ASSERT0(src);
    return getBodyBBSet()->is_equal(*src->getBodyBBSet()) &&
           getLoopHead() == src->getLoopHead() &&
           has_early_exit == src->has_early_exit &&
           has_call == src->has_call;
}


template <class BB>
bool LI<BB>::isLoopInfoTreeApproEqual(LI<BB> const* li1, LI<BB> const* li2)
{
    if (li1 == li2) { return true; }
    if ((li1 == nullptr) ^ (li2 == nullptr)) { return false; }
    if (li1 == nullptr) { return true; }
    UINT loopcnt1 = xcom::cnt_list(li1);
    UINT loopcnt2 = xcom::cnt_list(li2);
    if (loopcnt1 != loopcnt2) { return false; }
    for (LI<BB> const* tli1 = li1; tli1 != nullptr; tli1 = tli1->get_next()) {
        LI<BB> const* tli2 = li2;
        for (; tli2 != nullptr; tli2 = tli2->get_next()) {
            if (tli1->isLoopStructEqual(tli2)) { break; }
        }
        if (tli2 == nullptr) {
            //Can not find a isomophic LoopStructure at current loop-level.
            return false;
        }
        if (!isLoopInfoTreeApproEqual(tli1->getInnerList(),
                                      tli2->getInnerList())) {
            //The inner LoopInfos are not approximately equal in
            //minor loop-level.
            return false;
        }
    }
    return true;
}


template <class BB>
bool LI<BB>::isLoopInfoTreeEqual(LI<BB> const* li1, LI<BB> const* li2)
{
    if (li1 == li2) { return true; }
    if ((li1 == nullptr) ^ (li2 == nullptr)) { return false; }
    if (li1 == nullptr) { return true; }
    LI<BB> const* tli1 = li1;
    LI<BB> const* tli2 = li2;
    for (; tli1 != nullptr && tli2 != nullptr;
         tli1 = tli1->get_next(), tli2 = tli2->get_next()) {
        if (!tli1->isLoopStructEqual(tli2)) { return false; }
        if (!isLoopInfoTreeEqual(tli1->getInnerList(), tli2->getInnerList())) {
            return false;
        }
    }
    return tli1 == tli2;
}


template <class BB>
void LI<BB>::addBBToLoopAndAllOuterLoop(UINT bbid) const
{
    addBBToAllOuterLoop(bbid);
    getBodyBBSet()->bunion(bbid);
}


template <class BB>
void LI<BB>::addBBToAllOuterLoop(UINT bbid) const
{
    for (LI<BB> * tli = getOuter(); tli != nullptr; tli = tli->getOuter()) {
        tli->getBodyBBSet()->bunion(bbid);
    }
}


//Find the BB that is the START of the unqiue backedge of loop.
//  BB1: loop-start bb
//  BB2: body
//  BB3: goto loop-start bb
//BB3 is the backedge-start bb.
//Return backedge BB id if found, otherwise return BBID_UNDEF.
template <class BB>
UINT LI<BB>::findBackEdgeStartBB(xcom::Graph const* cfg) const
{
    ASSERT0(cfg);
    DUMMYUSE(cfg);
    BB * head = getLoopHead();
    UINT backedgebbid = BBID_UNDEF;
    UINT backedgecount = 0;
    for (xcom::EdgeC const* ec = head->getVex()->getInList();
         ec != nullptr; ec = ec->get_next()) {
        backedgecount++;
        UINT pred = ec->getFromId();
        if (isInsideLoop(pred)) {
            backedgebbid = pred;
        }
    }
    ASSERT0(backedgebbid != BBID_UNDEF);
    if (backedgecount > 2) {
        //There are multiple backedges.
        return BBID_UNDEF;
    }
    return backedgebbid;
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
template <class BB>
UINT LI<BB>::findFirstLoopEndBB(xcom::Graph * cfg) const
{
    ASSERT0(cfg);
    BB * head = getLoopHead();
    for (xcom::EdgeC const* ec = head->getVex()->getOutList();
         ec != nullptr; ec = ec->get_next()) {
        UINT succ = ec->getToId();
        if (!isInsideLoop(succ)) {
            return succ;
        }
    }
    return BBID_UNDEF;
}


template <class BB>
void LI<BB>::findAllLoopEndBB(xcom::Graph const* cfg,
                              OUT List<UINT> & endlst) const
{
    ASSERT0(getBodyBBSet());
    for (BSIdx i = getBodyBBSet()->get_first();
         i != BS_UNDEF; i = getBodyBBSet()->get_next(i)) {
        Vertex const* vex = cfg->getVertex(i);
        ASSERT0(vex);
        AdjVertexIter it;
        for (Vertex * succ = xcom::Graph::get_first_out_vertex(vex, it);
             succ != nullptr; succ = xcom::Graph::get_next_out_vertex(it)) {
            if (!isInsideLoop(succ->id())) {
                endlst.append_tail((UINT)i);
                break;
            }
        }
    }
}


//Return true if ir in bbid is at least execute once from loophead,
//otherwise return false means unknown.
template <class BB>
bool LI<BB>::atLeastExecOnce(UINT bbid, xcom::DGraph const* cfg) const
{
    BB * head = getLoopHead();
    if (bbid == head->id()) { return true; }
    UINT endbbid = findBackEdgeStartBB(cfg);
    if (endbbid != BBID_UNDEF &&
        (endbbid == bbid || cfg->is_dom(bbid, endbbid))) {
        return true;
    }
    return false;
}


template <class BB>
bool LI<BB>::isInsideLoopTree(UINT bbid, OUT UINT & nestlevel,
                              bool access_sibling) const
{
    LI<BB> const* li = this;
    if (li == nullptr) { return false; }
    for (LI<BB> const* tli = li; tli != nullptr; tli = tli->get_next()) {
        if (tli->getInnerList() != nullptr &&
            tli->getInnerList()->isInsideLoopTree(bbid, nestlevel,
                                                  access_sibling)) {
            nestlevel++;
            return true;
        }
        if (tli->isInsideLoop(bbid)) {
            nestlevel++;
            return true;
        }
        if (!access_sibling) { break; }
    }
    return false;
}
//END LI<BB>


class BB2LI {
    COPY_CONSTRUCTOR(BB2LI);
    xcom::TMap<UINT, LI<IRBB>*> m_bb2li; //map bb to LI if bb is loop header.
public:
    BB2LI() {}

    //The function iterate whole LoopInfo tree that rooted by 'li' to create
    //the mapping between loophead and related LoopInfo.
    void createMap(LI<IRBB> * li);

    //Return the LoopInfo which loophead is 'bb'.
    LI<IRBB> * get(IRBB * bb) { return m_bb2li.get(bb->id()); }
};


//
//START LoopInfoMgr
//
template <class BB> class LoopInfoMgr {
    COPY_CONSTRUCTOR(LoopInfoMgr);
protected:
    UINT m_li_count; //counter to loop.
    xcom::SMemPool * m_pool;
    BitSetMgr m_bs_mgr;
protected:
    LI<BB> * allocLoopInfo()
    {
        LI<BB> * li = (LI<BB>*)xmalloc(sizeof(LI<BB>));
        LI_id(li) = m_li_count++;
        return li;
    }

    void destroy()
    {
        if (m_pool == nullptr) { return; }
        m_bs_mgr.destroy();
        xcom::smpoolDelete(m_pool);
        m_pool = nullptr;
    }

    void init()
    {
        if (m_pool != nullptr) { return; }
        m_bs_mgr.init();
        m_li_count = LOOPINFO_UNDEF + 1;
        m_pool = xcom::smpoolCreate(sizeof(LI<BB>) * 4, MEM_COMM);
    }

    void * xmalloc(size_t size)
    {
        ASSERT0(m_pool);
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, size);
        return p;
    }
public:
    LoopInfoMgr() { m_pool = nullptr; init(); }
    ~LoopInfoMgr() { destroy(); }

    //Clean loopinfo structure before recompute loop info.
    void clean() { destroy(); init(); }
    BitSet * createBitSet() { return m_bs_mgr.create(); }
    LI<BB> * copyLoopTree(LI<BB> const* src);
    LI<BB> * copyLoopInfo(LI<BB> const* src)
    {
        ASSERT0(src);
        BitSet * bs = createBitSet();
        ASSERT0(src->getBodyBBSet());
        bs->copy(*src->getBodyBBSet());
        LI<BB> * li = buildLoopInfo(bs, src->getLoopHead());
        li->has_early_exit = src->has_early_exit;
        li->has_call = src->has_call;
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

    void freeBitSet(BitSet * bs) { ASSERT0(bs); m_bs_mgr.free(bs); }

    UINT getLoopNum() const { return m_li_count - 1; }
};


template <class BB>
LI<BB> * LoopInfoMgr<BB>::copyLoopTree(LI<BB> const* src)
{
    LI<BB> * new_looplist = nullptr;
    LI<BB> * last = nullptr;
    LI<BB> const* looplist = src;
    while (looplist != nullptr) {
        LI<BB> * newli = copyLoopInfo(looplist);
        xcom::add_next(&new_looplist, &last, newli);
        LI_inner_list(newli) = copyLoopTree(looplist->getInnerList());
        looplist = looplist->get_next();
    }
    return new_looplist;
}
//END LoopInfoMgr


//
//START ConstructLoopTree
//
//The class constructs a LoopInfo Tree to describe all LoopNest in 'cfg'.
//e.g: CFG includes three loops.
//  Construct LoopTree by the IRBB and IRCFG:
//    LoopInfoMgr<IRBB> limgr;
//    ConstructLoopTree<IRBB, IR> lt(cfg, limgr);
//    LI<IRBB> const* loopinfo_root = lt.construct();
//    loopinfo_root->dumpLoopTree(getLogMgr());
//  The dumped LoopTree info is:
//    LOOP2 HEAD:BB2, BODY:2,3,4,5,6,7,8,9,
//      LOOP1 HEAD:BB5, BODY:5,6,7,
//      LOOP3 HEAD:BB8, BODY:8,9,
//  The BBSet of outer-most loop is {2,3,4,5,6,7,8,9}, its loop head is BB2.
//  The inner two loops are {5,6,7) and {8,9}, which loophead are BB5 and BB8
//  respectively.
template <class BB, class XR> class ConstructLoopTree {
    COPY_CONSTRUCTOR(ConstructLoopTree);
protected:
    CFG<BB, XR> const* m_cfg;
    xcom::List<BB*> const* m_bb_list;
    LoopInfoMgr<BB> & m_li_mgr;
protected:
    //Identifiy back edge.
    //A back edge is: y dominate x, back-edge is : x->y
    void identifyNaturalLoop(UINT x, UINT y, MOD xcom::BitSet & loop,
                             List<UINT> & tmp);

    //Insert 'loop' into loop forest that started from 'lilst'.
    //Return true if 'loop' has been inserted into LoopInfo forest.
    bool insertLoopTree(LI<BB> ** lilist, LI<BB> * loop);

    bool reinsertLoopTree(LI<BB> ** lilist, LI<BB>* loop);
    void removeLoopInfo(LI<BB> * loop);
public:
    ConstructLoopTree(CFG<BB, XR> const* cfg, LoopInfoMgr<BB> & limgr)
        : m_cfg(cfg), m_bb_list(cfg->getBBList()), m_li_mgr(limgr)
    {}
    LI<BB> * construct(OptCtx const& oc);
};


//Remove 'loop' out of loop tree.
template <class BB, class XR>
void ConstructLoopTree<BB, XR>::removeLoopInfo(LI<BB> * loop)
{
    ASSERT0(loop != nullptr);
    LI<BB> * head = xcom::get_head(loop);
    ASSERT0(head);
    xcom::remove(&head, loop);
    if (loop->getOuter() != nullptr) {
        //Update inner-list for outer-loop of 'loop'.
        //Guarantee outer-loop has the correct inner-loop after
        //removing 'loop'.
        LI_inner_list(loop->getOuter()) = head;
    }
    loop->cleanAdjRelation();
}


template <class BB, class XR>
LI<BB> * ConstructLoopTree<BB, XR>::construct(OptCtx const& oc)
{
    LI<BB> * loopinfo_root = nullptr;
    typename xcom::List<UINT> tmp;
    xcom::TMap<BB const*, LI<BB>*> head2li;
    //typename xcom::List<BB*>::Iter ct;
    RPOVexListIter ct;
    ASSERT0_DUMMYUSE(oc.is_rpo_valid() && oc.is_dom_valid());
    RPOVexList const* rpolst = m_cfg->getRPOVexList();
    ASSERT0(rpolst);
    for (xcom::Vertex const* vex = rpolst->get_head(&ct);
         vex != nullptr; vex = rpolst->get_next(&ct)) {
        ASSERT0(m_cfg->getBB(vex->id()) && m_cfg->isVertex(vex));

        //Access each sussessor of vex.
        for (xcom::EdgeC const* el = vex->getOutList();
             el != nullptr; el = EC_next(el)) {
            BB const* succ = m_cfg->getBB(el->getToId());
            ASSERT0(succ);
            xcom::BitSet const* dom = m_cfg->get_dom_set(vex->id());
            ASSERTN(dom, ("should compute dominator first"));
            if (!dom->is_contain(succ->id()) &&
                vex->id() != succ->id()) { //vex's successor is itself.
                continue;
            }

            //If 'succ' is one of the DOMINATOR of 'vex', then it usually
            //indicates a back-edge.
            //Edge vex->succ is a back-edge, and each back-edge descripts a
            //natural loop.
            xcom::BitSet * loopbody = m_li_mgr.createBitSet();
            identifyNaturalLoop(vex->id(), succ->id(), *loopbody, tmp);

            //TBD: Do we have to handle the special case here? Or the special
            //BB does not affect the analysis of control flow optimizations?
            //addBreakOutLoop(succ, *loopbody);

            //Loop may have multiple backedges.
            LI<BB> * li = head2li.get(succ);
            if (li != nullptr) {
                //Multiple natural loops have the same loop header.
                li->getBodyBBSet()->bunion(*loopbody);
                reinsertLoopTree(&loopinfo_root, li);
                continue;
            }
            li = m_li_mgr.buildLoopInfo(loopbody, const_cast<BB*>(succ));
            insertLoopTree(&loopinfo_root, li);
            head2li.set(succ, li);
        }
    }
    return loopinfo_root;
}


//Reinsert loop into loop tree.
//NOTE 'loop' has been inserted into the loop-tree.
template <class BB, class XR>
bool ConstructLoopTree<BB, XR>::reinsertLoopTree(LI<BB> ** lilist, LI<BB>* loop)
{
    ASSERT0(lilist != nullptr && loop != nullptr);
    removeLoopInfo(loop);
    return insertLoopTree(lilist, loop);
}


template <class BB, class XR>
bool ConstructLoopTree<BB, XR>::insertLoopTree(LI<BB> ** lilist, LI<BB> * loop)
{
    ASSERT0(lilist != nullptr && loop != nullptr);
    if (*lilist == nullptr) {
        *lilist = loop;
        return true;
    }
    ASSERT0(loop->getOuter() == nullptr);

    //Check and insert if loop belong to the inner-loops of li.
    LI<BB> * li = *lilist, * cur = nullptr;
    while (li != nullptr) {
        //Iterate the sibling LoopInfo trees.
        cur = li;
        li = li->get_next();
        if (cur == loop) {
            //loop has already in LoopInfo list.
            return true;
        }
        if (cur->getBodyBBSet()->is_contain(*loop->getBodyBBSet())) {
            if (insertLoopTree(&LI_inner_list(cur), loop)) {
                if (loop->getOuter() == nullptr) {
                    //'loop' belongs to the immediate-inner-loop-list of 'cur'.
                    LI_outer(loop) = cur;
                }
                return true;
            }
            continue;
        }
        if (loop->getBodyBBSet()->is_contain(*cur->getBodyBBSet())) {
            //Loop body of 'loop' contained 'cur'.
            //Adjust inclusive-relation between 'loop' and 'cur' to
            //have 'loop' become loop-parent of 'cur'.
            xcom::remove(lilist, cur);
            LI_outer(cur) = nullptr;
            insertLoopTree(&LI_inner_list(loop), cur);
            if (cur->getOuter() == nullptr) {
                //'cur' belongs to the immediate-inner-loop-list of 'loop'.
                LI_outer(cur) = loop;
            }
            ASSERTN(LI_inner_list(loop), ("illegal loop tree"));
        }
    }
    xcom::add_next(lilist, loop);
    return true;
}


template <class BB, class XR>
void ConstructLoopTree<BB, XR>::identifyNaturalLoop(
    UINT x, UINT y, MOD xcom::BitSet & loop, xcom::List<UINT> & tmp)
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
        for (xcom::EdgeC const* ec = m_cfg->getVertex(bb)->getInList();
             ec != nullptr; ec = ec->get_next()) {
            VexIdx pred = ec->getFromId();
            if (loop.is_contain(pred)) { continue; }

            //If pred is not a member of loop,
            //add it into list to handle.
            loop.bunion(pred);
            tmp.append_head(pred);
        }
    }
}
//END ConstructLoopTree

//The function dump whole LoopInfo Forest that rooted by 'li'.
//NOTE: the function also dump the sibling LoopInfos of 'li'.
void dumpLoopTree(Region const* rg, LI<IRBB> const* li);

//Find the bb that is the START of the unqiue backedge of loop.
//  BB1: loop-start bb
//  BB2: body
//  BB3: goto loop-start bb
//BB3 is the backedge-start bb.
IRBB * findBackEdgeStartBB(LI<IRBB> const* li, IRCFG const* cfg);

//Find the first BB that is the END of loop. The end BB is outside of loop.
//Note there could be multiple end BB if the last IR of head is
//multipl-conditional branch, namely, SWTICH or IGOTO.
//  BB1: loop start bb
//       falsebr END
//  BB2: body
//  BB3: goto loop start bb
//  BB4: END
//BB4 is the loopend bb.
IRBB * findFirstLoopEndBB(LI<IRBB> const* li, IRCFG * cfg);

//Find preheader BB. If it does not exist, insert one before loop 'li'.
//Return the preheader BB.
//insert_bb: return true if this function insert a new bb before loop,
//           otherwise return false.
//force: force to insert preheader BB whatever it has been exist.
//       Return the new BB if insertion is successful.
//Note if we find the preheader, the last IR of it may be call.
//So if you are going to insert IR at the tail of preheader, the best choose is
//force the function to insert a new bb.
//The function try to maintain RPO under some conditions, thus user should
//check whether preheader's PRO is valid after the function return.
IRBB * findAndInsertPreheader(LI<IRBB> const* li, Region * rg,
                              OUT bool & insert_bb, bool force, OptCtx * oc);

//Find the bb that is the start of the unqiue backedge of loop.
//  BB1: loop start bb
//  BB2: body start bb
//  BB3: goto loop start bb
//BB2 is the loop header fallthrough bb.
bool findTwoSuccessorBBOfLoopHeader(LI<IRBB> const* li, IRCFG const* cfg,
                                    UINT * succ1, UINT * succ2);

//The function try to answer whether there may exist a cyclic reference
//that start from 'start' stmt.
//e.g: there exist a cyclic reference of
//MD10v3->MD10v1->MD10v1->MD13->MD10v3.
//  MD10v1 <- PHI(MD10v2, MD10v3)
//  MD13   <- MD10v1
//  MD10v3 <- MD13 #start
FindRedOpResult findRedOpInLoop(LI<IRBB> const* li, IR const* stmt,
                                Region const* rg);

//Return true if all the expression on 'ir' tree is loop invariant.
//ir: root node of IR
//li: loop structure
//check_tree: true to perform check recusively for entire IR tree.
//invariant_stmt: optional parameter, can be nullptr.
//    It is a list that records the stmt that is invariant in loop.
//    e.g:loop() {
//          a = b; //S1
//       }
//    stmt S1 is invariant because b is invariant.
//    If it is nullptr, the function will reason out conservative answer.
//Note the function does not check the sibling node of 'ir'.
bool isLoopInvariant(IR const* ir, LI<IRBB> const* li, Region const* rg,
                     InvStmtList const* invariant_stmt, bool check_tree);

//Return true if Phi does NOT have any USE in loop, except itself operand
//list.
//e.g: phi can be treated as loop invariant.
//  BB2:
//  phi $40 = $41, $42;
//  false br BB4;
//  BB3:
//  NO USE OF $40;
//  BB4:
//  ... = $40;
//The loop body does not have USE of PHI result.
bool isPhiLoopInvariant(IR const* phi, LI<IRBB> const* li, Region const* rg);

//Return true if the target BB of branch-stmt 'stmt' is outside the given
//loop 'li'.
bool isBranchTargetOutSideLoop(LI<IRBB> const* li, IRCFG * cfg, IR const* stmt);

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
                     MOD OptCtx * oc, bool force);

//Iterative access LoopInfo tree. This funtion initialize the iterator.
//li: the root of the LoopInfo tree.
//it: iterator. It should be clean already.
LI<IRBB> const* iterInitLoopInfoC(LI<IRBB> const* li, OUT CLoopInfoIter & it);

//Iterative access LoopInfo tree.
//This function return the next LoopInfo according to 'it'.
//it: iterator.
LI<IRBB> const* iterNextLoopInfoC(OUT CLoopInfoIter & it);

//Iterative access LoopInfo tree. This funtion initialize the iterator.
//li: the root of the LoopInfo tree.
//it: iterator. It should be clean already.
LI<IRBB> * iterInitLoopInfo(LI<IRBB> * li, OUT LoopInfoIter & it);

//Iterative access LoopInfo tree.
//This function return the next LoopInfo according to 'it'.
//it: iterator.
LI<IRBB> * iterNextLoopInfo(OUT LoopInfoIter & it);

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
//bool needInsertPhiToPreheader(LI<IRBB> const* li, IRCFG const* cfg);

//Return true if stmt dominates all USE that are inside loop.
bool isStmtDomAllUseInsideLoop(IR const* stmt, LI<IRBB> const* li, Region * rg,
                               OptCtx const& oc);

//Verify the sanity of LoopInfo structure.
//Return true if LoopInfo tree is sane.
//li: the root of LoopInfo tree.
bool verifyLoopInfoTree(LI<IRBB> const* li, OptCtx const& oc);

//Verify the sanity of LoopInfo structure.
//Return true if LoopInfo tree is sane.
//NOTE: the function perform the verification by reconstructing a duplication
//LoopInfo tree for given 'cfg'.
//li: the root of LoopInfo tree.
bool verifyLoopInfoTreeByRecomp(
    IRCFG const* cfg, LI<IRBB> const* li, OptCtx const& oc);

//
//START LI<BB>
//
template <class BB>
UINT LI<BB>::getLoopNum() const
{
    UINT cnt = 0;
    LoopInfoIter it;
    for (LI<IRBB> * li = iterInitLoopInfo(this, it);
         li != nullptr; li = iterNextLoopInfo(it)) {
        cnt++;
    }
    return cnt;
}
//END LI<BB>

} //namespace xoc
#endif
