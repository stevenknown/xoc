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
    BB * loop_head; //loop head node, the only one header
                    //indicates a natural loop.
    xcom::BitSet * bb_set; //loop body elements

public:
    LI() {}

    //Find the bb that is the start of the unqiue backedge of loop.
    //  BB1: loop start bb
    //  BB2: body
    //  BB3: goto loop start bb
    //BB3 is the backedge start bb.
    UINT findBackedgeStartBB(xcom::Graph * cfg) const;

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

    //Return true if bb is belong to current loop.
    //'bbid': id of BB.
    bool isInsideLoop(UINT bbid) const
    { return LI_bb_set(this)->is_contain(bbid); }

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
            for (INT i = getBodyBBSet()->get_first();
                 i != -1; i = getBodyBBSet()->get_next((UINT)i)) {
                prt(rg, "%u,", i);
            }
        }
    }
};


//Find the bb that is the START of the unqiue backedge of loop.
//  BB1: loop-start bb
//  BB2: body
//  BB3: goto loop-start bb
//BB3 is the backedge-start bb.
//Return backedge BB id if found, otherwise return BBID_UNDEF.
template <class BB>
UINT LI<BB>::findBackedgeStartBB(xcom::Graph * cfg) const
{
    ASSERT0(cfg);
    BB * head = getLoopHead();
    UINT backedgebbid = BBID_UNDEF;
    UINT backedgecount = 0;
    for (xcom::EdgeC const* ec = cfg->getVertex(head->id())->getInList();
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


//Find the bb that is the START of the unqiue backedge of loop.
//  BB1: loop-start bb
//  BB2: body
//  BB3: goto loop-start bb
//
//BB3 is the backedge-start bb.
IRBB * findBackedgeStartBB(LI<IRBB> const* li, IRCFG * cfg);
IRBB * findAndInsertPreheader(LI<IRBB> const* li,
                              Region * rg,
                              OUT bool & insert_bb,
                              bool force);
bool findTwoSuccessorBBOfLoopHeader(LI<IRBB> const* li,
                                    IRCFG * cfg,
                                    UINT * succ1,
                                    UINT * succ2);

//List of invariant stmt.
typedef xcom::EList<IR*, IR2Holder> InvStmtList;
typedef xcom::List<LI<IRBB> const*> CLoopInfoIter;

//Return true if all the expression on 'ir' tree is loop invariant.
//ir: root node of IR
//li: loop structure
//check_tree: true to perform check recusively for entire IR tree.
//invariant_stmt: a table that record the stmt that is invariant in loop.
//    e.g:loop() {
//          a = b; //S1
//       }
//    stmt S1 is invariant because b is invariant.
//Note this function does not check the sibling node of 'ir'.
bool isLoopInvariant(IR const* ir, LI<IRBB> const* li, Region * rg,
                     InvStmtList const* invariant_stmt, bool check_tree);

//Try inserting preheader BB of loop 'li'.
//insert_bb: set to true if inserting a new BB before loop, otherwise false.
//Note if we find the preheader, the last IR of it may be call.
//So if you are going to insert IR at the tail of preheader, the best is
//force to insert a new bb.
bool insertPreheader(LI<IRBB> * li, Region * rg, OUT IRBB ** preheader);

//Iterative access LoopInfo tree. This funtion initialize the iterator.
//'li': the root of the LoopInfo tree.
//'it': iterator. It should be clean already.
//Readonly function.
LI<IRBB> const* iterInitLoopInfoC(LI<IRBB> const* li, OUT CLoopInfoIter & it);

//Iterative access LoopInfo tree.
//This function return the next LoopInfo accroding to 'it'.
//'it': iterator.
//Readonly function.
LI<IRBB> const* iterNextLoopInfoC(MOD CLoopInfoIter & it);

} //namespace xoc
#endif
