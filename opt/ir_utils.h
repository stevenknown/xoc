/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __IR_UTILS_H__
#define __IR_UTILS_H__

//The file defined a list of utilities to better manipulate IR.

namespace xoc {

class IR;
class IRBB;

typedef xcom::List<IR const*> ConstIRIter; //the iter to iterate IR Tree.
typedef xcom::List<IR*> IRIter; //the iter to iterate IR Tree.

//Map IR to its Holder during instrument operation.
typedef xcom::TMap<IR*, xcom::C<IR*>*> IR2Holder;
typedef xcom::EList<IR*, IR2Holder> IREList;

typedef xcom::List<IR*> IRList;
typedef xcom::List<IR*>::Iter IRListIter;

typedef xcom::List<IR const*> ConstIRList;
typedef xcom::List<IR const*>::Iter ConstIRListIter;

typedef xcom::TTab<IR*> IRTab;
typedef xcom::TTabIter<IR*> IRTabIter;

//Type to describe the Prno of PR operation.
typedef UINT PRNO;

//
//These macros defined helper dispatch interface to related IR.
//
//Defined the entry for expression that is binary operation.
#define SWITCH_CASE_BIN \
    SWITCH_CASE_ARITH: \
    SWITCH_CASE_LOGIC_BIN: \
    SWITCH_CASE_SHIFT: \
    SWITCH_CASE_COMPARE: \
    SWITCH_CASE_BITWISE_BIN

//Defined the entry for expression that is logica operation.
#define SWITCH_CASE_LOGIC \
    SWITCH_CASE_LOGIC_BIN: \
    SWITCH_CASE_LOGIC_UNA

//Defined the entry for expression that is logica binary operation.
#define SWITCH_CASE_LOGIC_BIN \
    case IR_LAND: \
    case IR_LOR

//Defined the entry for expression that is bitwise operation.
#define SWITCH_CASE_BITWISE \
    SWITCH_CASE_BITWISE_BIN: \
    SWITCH_CASE_BITWISE_UNA

//Defined the entry for expression that is bitwise binary operation.
#define SWITCH_CASE_BITWISE_BIN \
    case IR_BAND:\
    case IR_BOR: \
    case IR_XOR

//Defined the entry for expression that is bitwise unary operation.
#define SWITCH_CASE_BITWISE_UNA \
    case IR_BNOT

//Defined the entry for expression that is logica unary operation.
#define SWITCH_CASE_LOGIC_UNA \
    case IR_LNOT

//Defined the entry for expression that is arithmetic operation.
#define SWITCH_CASE_ARITH \
    case IR_ADD: \
    case IR_SUB: \
    case IR_MUL: \
    case IR_DIV: \
    case IR_REM: \
    case IR_MOD

//Defined the entry for shift expression.
#define SWITCH_CASE_SHIFT \
    case IR_ASR: \
    case IR_LSR: \
    case IR_LSL

//Defined the entry for compare expression.
#define SWITCH_CASE_COMPARE \
    case IR_LT: \
    case IR_LE: \
    case IR_GT: \
    case IR_GE: \
    case IR_EQ: \
    case IR_NE

//Defined the entry for expression that is unary operation.
#define SWITCH_CASE_UNA \
    SWITCH_CASE_LOGIC_UNA: \
    SWITCH_CASE_BITWISE_UNA: \
    case IR_NEG: \
    case IR_CVT

#define SWITCH_CASE_STMT_IN_BB \
    SWITCH_CASE_DIRECT_MEM_STMT: \
    SWITCH_CASE_INDIRECT_MEM_STMT: \
    SWITCH_CASE_WRITE_PR: \
    SWITCH_CASE_WRITE_ARRAY: \
    SWITCH_CASE_CALL: \
    SWITCH_CASE_BRANCH_OP: \
    case IR_REGION: \
    case IR_RETURN

//Defined the entry for stmt which without any kid and can be put into BB.
#define SWITCH_CASE_STMT_IN_BB_NO_KID \
    case IR_REGION: \
    case IR_GOTO: \
    case IR_LABEL

//Defined the entry for stmt which without any kid.
#define SWITCH_CASE_STMT_NO_KID \
    SWITCH_CASE_STMT_IN_BB_NO_KID: \
    case IR_BREAK: \
    case IR_CONTINUE

//Defined the entry for expression which without any kid.
#define SWITCH_CASE_EXP_NO_KID \
    SWITCH_CASE_READ_PR: \
    SWITCH_CASE_DIRECT_MEM_EXP: \
    case IR_ID: \
    case IR_LDA: \
    case IR_CONST

//Defined the entry for memory load access.
#define SWITCH_CASE_EXP_MEM_OP \
    SWITCH_CASE_READ_PR: \
    SWITCH_CASE_DIRECT_MEM_EXP: \
    SWITCH_CASE_INDIRECT_MEM_EXP: \
    SWITCH_CASE_READ_ARRAY

//Defined the entry for memory write access.
#define SWITCH_CASE_STMT_MEM_OP \
    case IR_ST: \
    case IR_STPR: \
    case IR_IST: \
    case IR_STARRAY

//Defined the entry for function call.
#define SWITCH_CASE_CALL \
    case IR_CALL: \
    case IR_ICALL

#define SWITCH_CASE_WRITE_ARRAY \
    case IR_STARRAY

#define SWITCH_CASE_READ_ARRAY \
    case IR_ARRAY

#define SWITCH_CASE_ARRAY_OP \
    SWITCH_CASE_WRITE_ARRAY: \
    SWITCH_CASE_READ_ARRAY

#define SWITCH_CASE_WRITE_PR \
    case IR_STPR: \
    case IR_GETELEM: \
    case IR_SETELEM: \
    case IR_PHI: \
    SWITCH_CASE_EXT_WRITE_PR

#define SWITCH_CASE_READ_PR \
    case IR_PR

#define SWITCH_CASE_PR_OP \
    SWITCH_CASE_WRITE_PR: \
    SWITCH_CASE_READ_PR \

#define SWITCH_CASE_MAY_WRITE_PR \
    SWITCH_CASE_WRITE_PR: \
    SWITCH_CASE_CALL

#define SWITCH_CASE_MAY_PR_OP \
    SWITCH_CASE_CALL: \
    SWITCH_CASE_PR_OP

#define SWITCH_CASE_DIRECT_MEM_EXP \
    case IR_LD

#define SWITCH_CASE_EXT_INDIRECT_MEM_VSTMT \
    case IR_VIST

#define SWITCH_CASE_DIRECT_MEM_STMT \
    case IR_ST: \
    SWITCH_CASE_EXT_DIRECT_MEM_VSTMT

#define SWITCH_CASE_DIRECT_MEM_OP \
    SWITCH_CASE_DIRECT_MEM_EXP: \
    SWITCH_CASE_DIRECT_MEM_STMT

#define SWITCH_CASE_INDIRECT_MEM_EXP \
    case IR_ILD

#define SWITCH_CASE_INDIRECT_MEM_STMT \
    case IR_IST: \
    SWITCH_CASE_EXT_INDIRECT_MEM_VSTMT

#define SWITCH_CASE_INDIRECT_MEM_OP \
    SWITCH_CASE_INDIRECT_MEM_EXP: \
    SWITCH_CASE_INDIRECT_MEM_STMT

//Defined the entry for loop iteration control flow structure.
#define SWITCH_CASE_LOOP_ITER_CFS_OP \
    case IR_BREAK: \
    case IR_CONTINUE

//Defined the entry for loop control flow structure.
#define SWITCH_CASE_LOOP_CFS_OP \
    case IR_WHILE_DO: \
    case IR_DO_WHILE: \
    case IR_DO_LOOP

//Defined the entry for well-structured reducible control flow structure.
#define SWITCH_CASE_CFS_OP \
    SWITCH_CASE_LOOP_CFS_OP: \
    case IR_IF

#define SWITCH_CASE_HAS_DU \
    SWITCH_CASE_MAY_PR_OP: \
    SWITCH_CASE_DIRECT_MEM_OP: \
    SWITCH_CASE_INDIRECT_MEM_OP: \
    SWITCH_CASE_ARRAY_OP: \
    case IR_ID

#define SWITCH_CASE_MEM_NONPR_OP \
    SWITCH_CASE_DIRECT_MEM_OP: \
    SWITCH_CASE_INDIRECT_MEM_OP: \
    SWITCH_CASE_ARRAY_OP: \
    case IR_ID

#define SWITCH_CASE_CONDITIONAL_BRANCH_OP \
    case IR_TRUEBR: \
    case IR_FALSEBR

#define SWITCH_CASE_UNCONDITIONAL_BRANCH_OP \
    case IR_GOTO: \
    case IR_IGOTO

#define SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP \
    case IR_SWITCH

#define SWITCH_CASE_BRANCH_OP \
    SWITCH_CASE_CONDITIONAL_BRANCH_OP: \
    SWITCH_CASE_UNCONDITIONAL_BRANCH_OP: \
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP

//Defined the entry for all stmt operations.
#define SWITCH_CASE_STMT \
    SWITCH_CASE_DIRECT_MEM_STMT: \
    SWITCH_CASE_INDIRECT_MEM_STMT: \
    SWITCH_CASE_WRITE_ARRAY: \
    SWITCH_CASE_WRITE_PR: \
    SWITCH_CASE_CALL: \
    SWITCH_CASE_BRANCH_OP: \
    SWITCH_CASE_CFS_OP: \
    SWITCH_CASE_LOOP_ITER_CFS_OP: \
    case IR_RETURN: \
    case IR_LABEL: \
    case IR_REGION

//Defined the entry for all expression operations.
#define SWITCH_CASE_EXP \
    SWITCH_CASE_DIRECT_MEM_EXP: \
    SWITCH_CASE_INDIRECT_MEM_EXP: \
    SWITCH_CASE_READ_ARRAY: \
    SWITCH_CASE_READ_PR: \
    SWITCH_CASE_BIN: \
    SWITCH_CASE_UNA: \
    case IR_CASE: \
    case IR_SELECT: \
    case IR_LDA: \
    case IR_ID: \
    case IR_CONST: \
    SWITCH_CASE_EXT_EXP

} //namespace xoc

#endif
