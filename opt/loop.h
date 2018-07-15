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
#define LI_id(li)                   ((li)->id)
#define LI_next(li)                 ((li)->next)
#define LI_prev(li)                 ((li)->prev)
#define LI_has_early_exit(li)       ((li)->has_early_exit)
#define LI_has_call(li)             ((li)->has_call)
#define LI_inner_list(li)           ((li)->inner_list)
#define LI_outer(li)                ((li)->outer)
#define LI_bb_set(li)               ((li)->bb_set)
#define LI_loop_head(li)            ((li)->loop_head)
template <class BB> class LI {
public:
    UINT id;
    LI * next;
    LI * prev;
    LI * inner_list; //inner loop list
    LI * outer; //outer loop
    UCHAR has_early_exit:1;
    UCHAR has_call:1;
    BB * loop_head; //loop head node, the only one header
                    //indicates a natural loop.
    xcom::BitSet * bb_set; //loop body elements

    bool is_loop_reduction() { return !has_early_exit; }

    //Return true if bb is belong to current loop.
    //'bbid': id of BB.
    bool is_inside_loop(UINT bbid) const
    { return LI_bb_set(this)->is_contain(bbid); }
};


class IRBB;
class Region;
class IR_CFG;

IRBB * findAndInsertPreheader(
            LI<IRBB> const* li, Region * rg,
            OUT bool & insert_bb, bool force);
IRBB * findSingleBackedgeStartBB(LI<IRBB> const* li, IR_CFG * cfg);
bool findTwoSuccessorBBOfLoopHeader(
            LI<IRBB> const* li, IR_CFG * cfg,
            UINT * succ1, UINT * succ2);

} //namespace xoc
#endif
