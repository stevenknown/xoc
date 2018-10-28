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
#ifndef _IF_OPT_H_
#define _IF_OPT_H_

namespace xoc {

//1.  This moves certain 'if' nodes up in the code under
//    some cirumstances that can allow the test for the if
//    to be eliminated.  The ``if'' nodes that are
//    candidates to be hoisted are those that have a
//    condition depending on only a single variable.  If
//    that is the case, and in the code preceeding the
//    'if' (on the same tree_node_list) there is another
//    'if' which assigns a constant to the value of that
//    condition variable in either the 'then' or 'else'
//    part, this will duplicate the original 'if' node and
//    put it in both the 'then' and 'else' parts of the
//    higher 'if' node, if this is legal.  This is useful
//    for code which has 'chains' of 'if' nodes; that
//    is, the body of one sets a variable that is used as a
//    test in a later 'if'.  After hoisting, the constant
//    value can often be propagated into the condition in
//    one of the branches of the 'if'.  In simple cases
//    where the flag is cleared before the higher 'if' and
//    then set only in one of its branches, the test can be
//    eliminated in both parts.
//
//2.  Do simple optimizations on unstructured control flow
//    (branches and labels).  The optimizations are done
//    simultaneously in such a way that the result cannot
//    benefit from any more of these optimizations -- the
//    output run through this pass again will not change.
//    The following optimizations are performed:
//      * Labels that are not the target of any possible
//        branch are removed.
//      * Uses of labels that are followed by unconditional
//        jumps or other labels without any intervening
//        executable code are changed to uses of the last
//        label that must always be executed before some
//        executable code, and those labels are removed.
//      * Unreachable code is removed.
//      * Branches that would end up in the same place
//        before any code is executed as they would if they
//        were not taken are removed.
//      * Conditional branches followed in the code by
//        unconditional branches without any intervening
//        executable code, followed without any intervening
//        executable code by the label that is the target of
//        the conditional branch, are changed to reverse the
//        condition, change its target to that of the
//        unconditional branch, and remove the conditional
//        branch.  That is,
//
//            if (cond){
//                t=1
//                goto L1;
//            }
//            f=1
//            goto L2;
//            L1:
//
//        is replaced by
//
//            if (!cond){
//                f=1
//                goto L2;
//            }
//            t=1
//            L1:
//
//        'goto L1' is removed
//        (and L1 is removed if it is not a target of some
//        other instruction).
typedef enum  {
    NOT_SIMP_IF = 0,
    SIMP_IF_THEN_TYPE,
    SIMP_IF_ELSE_TYPE,

} IF_TYPE;

} //namespace xoc
#endif
