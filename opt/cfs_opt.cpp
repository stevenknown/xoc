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

//If the IF has a nested IF in the ELSE part with identical THEN stmt:
//    IF (A) {
//      S1;
//    } ELSE {
//      IF (B) {
//        S1;
//      } ELSE {
//        S2;
//      }
//    }
//
//transform it to:
//
//    IF (A || B) { S1; } ELSE { S2; }
//
//Note S1 can be the empty statement.
//Since this is done bottom up, multiple occurrences of the identical THEN
//stmts can be transformed. If tranformed, the new IF statement is returned;
//otherwise, the original IF.
bool CfsOpt::transformIf4(IR ** head, IR * ir)
{
    //TODO.
    DUMMYUSE(ir);
    DUMMYUSE(head);
    return false;
}


//If the given IF has a nested IF in the THEN part with identical ELSE stmt:
//
//  IF (A) THEN { if (B) THEN S1; ELSE S2; } ELSE S2;
//
//transform it to:
//
//  IF (A && B) THEN S1; ELSE S2;
//
//S2 can be the empty statement.
//Since this is done bottom up, multiple occurrences of the identical ELSE
//stmts can be commonized.  If tranformed, the new IF statement is returned;
//otherwise, the original IF.
bool CfsOpt::transformIf5(IR ** head, IR * ir)
{
    //TODO.
    DUMMYUSE(ir);
    DUMMYUSE(head);
    return false;
}


//Control flow struct optimizations.
//Transform follow struct to do-while loop
//
//    LABEL:
//    IR-List
//    IF DET
//       GOTO LABEL
//       ...(DEAD CODE)
//    ELSE
//       FALSE-PART
//    ENDIF
//
//is replace by
//
//    DO {
//        IR-List
//    } WHILE DET
//    FALSE-PART
bool CfsOpt::transformToDoWhile(IR ** head, IR * ir)
{
    ASSERTN(head != NULL && *head != NULL, ("invalid parameter"));
    if (!ir->is_lab()) { return false; }

    for (IR * t = ir; t != NULL; t = t->get_next()) {
        if (!t->is_if()) { continue; }

        if (IF_truebody(t) != NULL &&
            IF_truebody(t)->is_goto() &&
            isSameLabel(LAB_lab(ir), GOTO_lab(IF_truebody(t)))) {

            //Start transform.
            IR * dowhile = m_rg->buildDoWhile(m_rg->dupIRTree(IF_det(t)),
                                              NULL);
            IR * if_stmt = t;
            t = ir->get_next();
            while (t != NULL && t != if_stmt) {
                IR * c = t;
                t = t->get_next();
                xcom::remove(head, c);
                xcom::add_next(&LOOP_body(dowhile), c);
            }
            dowhile->setParentPointer(true);

            ASSERTN(t == if_stmt, ("illegal IR layout"));

            xcom::remove(head, if_stmt);
            if (IF_falsebody(if_stmt)) {
                xcom::add_next(&dowhile, IF_falsebody(if_stmt));
                IF_falsebody(if_stmt) = NULL;
            }
            xcom::insertafter(&ir, dowhile);
            m_rg->freeIRTree(if_stmt); //free IF

            //Do not remove label because it might be used as target of
            //other branch stmt.
            //e.g:
            // int test()
            // {
            //  ENTRY:
            //    goto BODY;
            //  BODY:
            //    if (a > 0) {
            //        goto BODY;
            //    }
            //    return 0;
            // }
            //xcom::remove(head, ir);
            //m_rg->freeIRTree(ir); //free LABEL
            return true;
        }
    }

    return false;
}


//ONLY used in this file
static inline bool is_non_branch_ir(IR * ir)
{
    return !ir->isConditionalBr() &&
           !ir->isUnconditionalBr() &&
           !ir->isMultiConditionalBr();
}


//The followed forms
//   if (cond) {
//       t=1
//       a=1
//       goto L1;
//   }
//   f=1
//   goto L2;
//   L1:
//
//is replaced with
//
//   if (!cond) {
//       f=1
//       goto L2;
//   }
//   t=1
//   a=1
//   L1:
//
//'goto L1' is removed and free, and L1 is removed if it is not a target
//of some other instruction.
bool CfsOpt::transformIf1(IR ** head, IR * ir)
{
    ASSERTN(head && *head, ("invalid parameter"));
    if (ir == NULL || !ir->is_if()) { return false; }

    //Check true part.
    IR * t = IF_truebody(ir);
    while (t != NULL) {
        if (!is_non_branch_ir(t)) {
            break;
        }
        t = t->get_next();
    }

    if (t != NULL && t->get_next() == NULL && t->is_goto()) {
        IR * first_goto = t;
        t = ir->get_next();
        while (t != NULL) {
            if (!is_non_branch_ir(t)) { break; }
            t = t->get_next();
        }

        if (t != NULL && t->is_goto()) {
            IR * second_goto = t;
            if (IR_next(second_goto) != NULL &&
                IR_next(second_goto)->is_lab() &&
                isSameLabel(GOTO_lab(first_goto),
                            LAB_lab(IR_next(second_goto)))) {

                //Start transforming.
                Refine::invertCondition(&IF_det(ir), m_rg);
                IR * new_list1 = NULL;
                IR * new_list2 = NULL;

                t = IF_truebody(ir);

                //Split true body of IF.
                IR * last = NULL;
                while (t != first_goto) {
                    IR * c = t;
                    t = t->get_next();
                    xcom::remove(&IF_truebody(ir), c);
                    xcom::add_next(&new_list1, &last, c);
                }
                ASSERTN(t && t == first_goto, ("invalid control flow"));

                xcom::remove(&IF_truebody(ir), first_goto);
                m_rg->freeIRTree(first_goto);

                //Split all irs between IF and L1.
                t = ir->get_next();
                while (t != second_goto) {
                    IR * c = t;
                    t = t->get_next();
                    xcom::remove(head, c);
                    xcom::add_next(&new_list2, c);
                }
                ASSERTN(t != NULL && t == second_goto, ("???"));
                xcom::remove(head, second_goto);
                xcom::add_next(&new_list2, second_goto);

                //Swap new_list1 and new_list2
                xcom::insertbefore(&IF_truebody(ir), IF_truebody(ir),
                                   new_list2);

                //Update the IR_parent for new_list2.
                ir->setParentPointer(true);
                ASSERTN(IF_truebody(ir) == new_list2,
                        ("illegal insertbefore<T>"));

                xcom::insertafter(&ir, new_list1);

                //Update the IR_parent for new_list1.
                for (IR * tmp = new_list1; tmp != NULL; tmp = IR_next(tmp)) {
                    IR_parent(tmp) = IR_parent(ir);
                }

                return true;
            }
        }
    }

    return false;
}


//The followed forms
//   if (cond) {
//
//   } else {
//       IR-list
//   }
//
//is replaced by
//
//   if (!cond) {
//       IR-list
//   }
bool CfsOpt::transformIf2(IR ** head, IR * ir)
{
    ASSERTN(head && *head, ("invalid parameter"));
    if (ir == NULL || !ir->is_if()) { return false; }

    //Check true part
    if (IF_truebody(ir) == NULL) {
        if (IF_falsebody(ir) == NULL) {
            xcom::remove(head, ir);
            m_rg->freeIRTree(ir);
            return true;
        }
        Refine::invertCondition(&IF_det(ir), m_rg);
        //Swap true and false body.
        IF_truebody(ir) = IF_falsebody(ir);
        IF_falsebody(ir) = NULL;
        return true;
    }

    return false;
}


//The followed forms
// x is signed
//     IF(x > 0x7FFFFFFF) {a=1} ELSE {b=1}  =>  b=1
//     IF(x < 0x80000000) {a=1} ELSE {b=1}  =>  b=1
//
// x is unsigned
//     IF(x > 0xFFFFFFFF){a=1} ELSE {b=1}  =>  b=1
//     IF(x < 0x0) {a=1} ELSE {b=1}        =>  b=1
bool CfsOpt::transformIf3(IR ** head, IR * ir)
{
    ASSERTN(head && *head, ("invalid parameter"));
    if (ir == NULL || !ir->is_if()) { return false; }

    IR * det = IF_det(ir);
    if (det->is_gt()) {
        IR * opnd0 = BIN_opnd0(det);
        IR * opnd1 = BIN_opnd1(det);
        if (opnd0->is_ld() &&
            opnd0->is_int() &&
            opnd1->is_const() &&
            opnd1->is_int() &&
            m_rg->getIntegerInDataTypeValueRange(opnd1) ==
              m_rg->getMaxInteger(m_tm->getDTypeBitSize(
                TY_dtype(opnd1->getType())), opnd1->is_signed())) {
            //e.g:
            //x is unsigned, if(x>0xFFFFFFFF) {a=1} else {b=1} => b=1
            //x is signed, if(x>0x7FFFFFFF) {a=1} else {b=1} =>  b=1
            IR * allocir = NULL;
            if (IF_falsebody(ir) != NULL) {
                allocir = m_rg->dupIRTree(IF_falsebody(ir));
            }
            xcom::replace(head, ir, allocir);

            if (allocir != NULL) {
                IR_parent(allocir) = IR_parent(ir);
            }

            m_rg->freeIRTree(ir);
            return true;
        }
    } else if (det->is_lt()) {
        IR * opnd0 = BIN_opnd0(det);
        IR * opnd1 = BIN_opnd1(det);
        if (opnd0->is_ld() &&
            opnd0->is_int() &&
            opnd1->is_const() &&
            opnd1->is_int() &&
            m_rg->getIntegerInDataTypeValueRange(opnd1) ==
              m_rg->getMinInteger(m_tm->getDTypeBitSize(
                TY_dtype(opnd1->getType())), opnd1->is_signed())) {
            //x is signed, IF(x < 0x80000000) {a=1} ELSE {b=1}  =>  b=1
            IR * allocir = NULL;
            if (IF_falsebody(ir) != NULL) {
                allocir = m_rg->dupIRTree(IF_falsebody(ir));
            }
            xcom::replace(head, ir, allocir);

            if (allocir != NULL) {
                IR_parent(allocir) = IR_parent(ir);
            }

            m_rg->freeIRTree(ir);
            return true;
        } else if (opnd0->is_ld() &&
                   opnd1->is_const() &&
                   opnd0->is_uint() &&
                   CONST_int_val(opnd1) == 0) {
            //x is unsigned, if(x<0) {a=1} else {b=1}  =>  b=1
            IR * allocir = NULL;
            if (IF_falsebody(ir) != NULL) {
                allocir = m_rg->dupIRTree(IF_falsebody(ir));
            }
            xcom::replace(head, ir, allocir);

            if (allocir != NULL) {
                IR_parent(allocir) = IR_parent(ir);
            }

            m_rg->freeIRTree(ir);
            return true;
        }
    }

    return false;
}


//Hoist det of loop.
//e.g: while (a=10,b+=3,c<a) {
//        IR-List;
//     }
//
//be replaced by
//
//     a = 10;
//     b += 3;
//     while (c<a) {
//        IR-List;
//        a = 10;
//        b += 3;
//     }
bool CfsOpt::hoistLoop(IR ** head, IR * ir)
{
    ASSERT0(ir->is_dowhile() || ir->is_whiledo() || ir->is_doloop());
    ASSERTN(LOOP_det(ir), ("DET is NULL"));
    IR * det = LOOP_det(ir);

    INT i = 0;
    while (det != NULL) {
        i++;
        det = det->get_next();
    }

    IR * new_list = NULL, * new_body_list = NULL;
    if (i > 1) {
        det = LOOP_det(ir);
        while (i > 1) {
            IR * c = det;
            ASSERTN(c->is_stmt(), ("Non-stmt ir should be remove "
                                   "during reshape_ir_tree()"));
            det = det->get_next();
            xcom::remove(&LOOP_det(ir), c);
            xcom::add_next(&new_list, c);
            i--;
        }
        new_body_list = m_rg->dupIRTreeList(new_list);
        xcom::insertbefore(head, ir, new_list);
        xcom::add_next(&LOOP_body(ir), new_body_list);
        ir->setParentPointer(false);
        return true;
    }
    return false;
}


//Canonicalize det of IF.
//e.g: if (a=10, b+=3, c<a) {...}
//  will be replaced by
//    a = 10;
//    b += 3;
//    if (c<a) {...}
bool CfsOpt::hoistIf(IR ** head, IR * ir)
{
    ASSERTN(ir->is_if(), ("need IF"));
    ASSERTN(IF_det(ir), ("DET is NULL"));

    IR * det = IF_det(ir);
    INT i = 0;
    while (det != NULL) {
        i++;
        det = det->get_next();
    }

    IR * new_list = NULL;
    if (i > 1) {
        det = IF_det(ir);
        while (i > 1) {
            IR * c = det;
            ASSERTN(c->is_stmt(), ("Non-stmt ir should be remove"
                                   " during reshape_ir_tree()"));
            det = det->get_next();
            xcom::remove(&IF_det(ir), c);
            xcom::add_next(&new_list, c);
            i--;
        }
        xcom::insertbefore(head, ir, new_list);
        return true;
    }
    return false;
}


bool CfsOpt::doCfsOpt(IN OUT IR ** ir_list, SimpCtx const& sc)
{
    bool change = false;
    for (IR * ir = *ir_list; ir != NULL;) {
        if (transformToDoWhile(ir_list, ir)) {
            change = true;
            ir = *ir_list;
            continue;
        }

        if (ir->is_if() && transformIf1(ir_list, ir)) {
            change = true;
            ir = *ir_list;
            continue;
        }

        if (ir->is_if() && transformIf2(ir_list, ir)) {
            change = true;
            ir = *ir_list;
            continue;
        }

        if (ir->is_if() && transformIf3(ir_list, ir)) {
            change = true;
            ir = *ir_list;
            continue;
        }

        switch (ir->getCode()) {
        case IR_IF:
            if (hoistIf(ir_list, ir)) {
                change = true;
                ir = *ir_list;
                continue;
            }
            if (doCfsOpt(&IF_truebody(ir), sc)) {
                change = true;
                ir = *ir_list;
                continue;
            }
            if (doCfsOpt(&IF_falsebody(ir), sc)) {
                change = true;
                ir = *ir_list;
                continue;
            }
            break;
        case IR_DO_WHILE:
        case IR_WHILE_DO:
        case IR_DO_LOOP:
            if (hoistLoop(ir_list, ir)) {
                change = true;
                ir = *ir_list;
                continue;
            }
            if (doCfsOpt(&LOOP_body(ir), sc)) {
                change = true;
                ir = *ir_list;
                continue;
            }
            break;
        case IR_SWITCH:
            if (doCfsOpt(&SWITCH_body(ir), sc)) {
                change = true;
                ir = *ir_list;
                continue;
            }
            break;
        default:;
        } //end switch

        ir = ir->get_next();
    }
    return change;
}


//Control flow structure optimization and up to bottom walk through
//the IR tree. High level IRs include IR_IF, IR_WHILE_DO...
//High Level Reshaping phase consist of:
//  1. goto reduction
//  2. if restructure
//  3. loop restructure
bool CfsOpt::perform(SimpCtx const& sc)
{
    START_TIMER(t, getPassName());
    ASSERT0(!SIMP_if(&sc) &&
            !SIMP_doloop(&sc) &&
            !SIMP_dowhile(&sc) &&
            !SIMP_whiledo(&sc) &&
            !SIMP_switch(&sc) &&
            !SIMP_break(&sc) &&
            !SIMP_continue(&sc));
    IR * irs = m_rg->getIRList();
    bool change = doCfsOpt(&irs, sc);
    if (change) {
        m_rg->setIRList(irs);
    }
    END_TIMER(t, getPassName());    
    return change;
}

} //namespace xoc
