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

typedef xcom::List<IR const*> CIRList;
typedef xcom::List<IR const*>::Iter CIRListIter;

//Type to describe the Prno of PR operation.
typedef UINT PRNO;

//
//These macros defined helper dispatch interface to related IR.
//
//Defined the entry for expression that is binary operation.
#define SWITCH_CASE_BIN \
    case IR_ADD: \
    case IR_SUB: \
    case IR_MUL: \
    case IR_DIV: \
    case IR_REM: \
    case IR_MOD: \
    case IR_LAND:\
    case IR_LOR: \
    case IR_BAND:\
    case IR_BOR: \
    case IR_XOR: \
    case IR_ASR: \
    case IR_LSR: \
    case IR_LSL: \
    case IR_LT:  \
    case IR_LE:  \
    case IR_GT:  \
    case IR_GE:  \
    case IR_EQ:  \
    case IR_NE

//Defined the entry for expression that is unary operation.
#define SWITCH_CASE_UNA \
    case IR_BNOT: \
    case IR_LNOT: \
    case IR_NEG:  \
    case IR_CVT

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
    case IR_ID: \
    case IR_LD: \
    case IR_PR: \
    case IR_LDA: \
    case IR_CONST

//Defined the entry for memory load access.
#define SWITCH_CASE_EXP_MEM_ACC \
    case IR_LD: \
    case IR_PR: \
    case IR_ILD: \
    case IR_ARRAY

//Defined the entry for memory write access.
#define SWITCH_CASE_STMT_MEM_ACC \
    case IR_ST: \
    case IR_STPR: \
    case IR_IST: \
    case IR_STARRAY

//Defined the entry for function call.
#define SWITCH_CASE_CALL \
    case IR_CALL: \
    case IR_ICALL

} //namespace xoc
#endif
