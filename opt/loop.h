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

class IRBB;
class Region;
class IRCFG;
template <class BB> class LI;

//List of invariant stmt.
typedef xcom::EList<IR*, IR2Holder> InvStmtList;
typedef xcom::EList<IR*, IR2Holder>::Iter InvStmtListIter;
typedef xcom::List<LI<IRBB>*> LoopInfoIter;
typedef xcom::List<LI<IRBB> const*> CLoopInfoIter;

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
    UINT uid;
    LI * next;
    LI * prev;
    LI * inner_list; //inner loop list
    LI * outer; //outer loop
    UCHAR has_early_exit:1;
    UCHAR has_call:1;
    //Loop head node, the only one header indicates a natural loop.
    BB * loop_head;
    xcom::BitSet * bb_set; //loop body elements
public:
    LI() {}

    //Add bbid to the body BB set of all outer loop.
    void addBBToAllOuterLoop(UINT bbid) const;

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

    //Return true if bb is belong to current loop.
    //'bbid': id of BB.
    bool isInsideLoop(UINT bbid) const
    { return LI_bb_set(this)->is_contain(bbid); }
    bool isInsideLoopTree(UINT bbid, OUT UINT & nestlevel,
                          bool access_sibling) const;

    //Clean adjacent relation in loop-tree.
    void cleanAdjRelation()
    {
        LI_outer(this) = nullptr;
        LI_next(this) = nullptr;
        LI_prev(this) = nullptr;
    }

    void dump(Region * rg) const
    {
        note(rg, "\nLOOP%u HEAD:BB%u, BODY:", id(), getLoopHead()->id());
        if (getBodyBBSet() != nullptr) {
            for (BSIdx i = getBodyBBSet()->get_first();
                 i != BS_UNDEF; i = getBodyBBSet()->get_next((UINT)i)) {
                prt(rg, "%u,", i);
            }
        }
    }
};


//Add bbid to the body BB set of all outer loop.
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
//This function return the next LoopInfo accroding to 'it'.
//it: iterator.
LI<IRBB> const* iterNextLoopInfoC(OUT CLoopInfoIter & it);

//Iterative access LoopInfo tree. This funtion initialize the iterator.
//li: the root of the LoopInfo tree.
//it: iterator. It should be clean already.
LI<IRBB> * iterInitLoopInfo(LI<IRBB> * li, OUT LoopInfoIter & it);

//Iterative access LoopInfo tree.
//This function return the next LoopInfo accroding to 'it'.
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
bool verifyLoopInfoTree(LI<IRBB> const* li, OptCtx const& oc);

} //namespace xoc
#endif
