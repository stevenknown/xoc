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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _XCODE_H_
#define _XCODE_H_

namespace xoc {

typedef enum {
    X_CODE_UNDEF,
    X_ID,
    X_LD,
    X_ILD,
    X_ARRAY,
    X_ST,
    X_STRP,
    X_STARRAY,
    X_SETELEM,
    X_GETELEM,
    X_IST,
    X_CALL,
    X_ICALL,
    X_LDA,
    X_ADD,
    X_SUB,
    X_MUL,
    X_DIV,
    X_REM,
    X_MOD,
    X_LAND,
    X_LOR,
    X_BAND,
    X_BOR,
    X_XOR,
    X_ASR,
    X_LSR,
    X_LSL,
    X_LT,
    X_LE,
    X_GT,
    X_GE,
    X_EQ,
    X_NE,
    X_BNOT,
    X_LNOT,
    X_NEG,
    X_ALLOCA,
    X_CVT,
    X_GOTO,
    X_IGOTO,
    X_DO,
    X_WHILE,
    X_DO_LOOP,
    X_LABEL,
    X_TRUEBR,
    X_FALSEBR,
    X_SELECT,
    X_PHI,
    X_REGION,
    X_IF,
    X_ELSE,
    X_BREAK,
    X_RETURN,
    X_CONTINUE,
    X_SWITCH,
    X_CASE,
    X_DEFAULT,
    X_VAR,
    X_FUNC,
    X_PROGRAM,
    X_INNER,
    X_BLACKBOX,
    X_I8,
    X_U8,
    X_I16,
    X_U16,
    X_I32,
    X_U32,
    X_I64,
    X_U64,
    X_I128,
    X_U128,
    X_F32,
    X_F64,
    X_F80,
    X_F128,
    X_MC,
    X_STR,
    X_VEC,
    X_BOOL,
    X_ANY,
    X_READONLY,
    X_TRY_START,
    X_TRY_END,
    X_TERMINATE,
    X_CATCH_START,
    X_ATOM,
    X_RMW, //ReadModifyWrite
    X_THROW,
    X_SIDEEFFECT, //SideEffect
    X_NOMOVE,
    X_USE,
    X_DEF,
    X_PRIVATE,
    X_RESTRICT,
    X_VOLATILE,
    X_FAKE,
    X_GLOBAL,
    X_UNDEFINED,
    X_STRING,
    X_BYTE,
    X_ELEMTYPE,
    X_DIM,
    X_UNALLOCABLE,
    X_ALIGN,
    X_DECL,
    X_CODE_LAST,
} X_CODE;

} //namespace xoc
#endif
