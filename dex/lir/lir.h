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

author: GongKai, JinYue
@*/
#ifndef __LIR_H__
#define __LIR_H__

#define LIRMALLOC drLinearAlloc
#define LIRFREEALL drLinearFree

#define LIRC_num_of_op(l)    ((l)->instrCount)
#define LIRC_op(l, i)        ((l)->lirList[i])

#define LIR_opcode(ir)       ((ir)->opcode)
#define LIR_name(ir)         (gLIROpcodeInfo.opNames[LIR_opcode(ir)])
#define LIR_dt(ir)           ((ir)->flags)

//For CONST
#define LIR_int_imm(ir)      (((LIRConstOp*)ir)->vB)

//For SPUT
#define LIR_sym_ptr(ir)      (((LIRABOp*)ir)->ptr)
#define LIR_val(ir)          (((LIRABOp*)ir)->vA) //store value

//For ADD,SUB,MUL,DIV,REM,AND,OR,SHL,SHR,USHR,
//XOR,CONST,NEW_INSTANCE.
#define LIR_res(ir)          (((LIRABCOp*)ir)->vA)

//For ADD,SUB,MUL,DIV,REM,AND,OR,SHL,SHR,USHR,
//XOR,NEW_INSTANCE.
#define LIR_op0(ir)          (((LIRABCOp*)ir)->vB)

//For ADD,SUB,MUL,DIV,REM,AND,OR,SHL,SHR,USHR,XOR
#define LIR_op1(ir)          (((LIRABCOp*)ir)->vC)

#define LIR_fill_array_data_mark(ir) \
        (((USHORT*)(((LIRSwitchOp*)ir)->data))[0])
#define LIR_switch_kind(ir) \
        (((USHORT*)(((LIRSwitchOp*)ir)->data))[0])
#define LIR_case_num(ir) \
        (((USHORT*)(((LIRSwitchOp*)ir)->data))[1])
#define LIR_packed_switch_base_value(ir) \
        (*((INT*)(&(((LIRSwitchOp*)ir)->data[2]))))
#define LIR_packed_switch_case_entry(ir) \
        ((UINT*)(((USHORT*)((LIRSwitchOp*)ir)->data) + 4))
#define LIR_sparse_switch_case_value(ir) \
        ((UINT*)(((USHORT*)(((LIRSwitchOp*)ir)->data)) + 2))
#define LIR_sparse_switch_case_entry(ir) \
        (UINT*)&(((BYTE*)(((LIRSwitchOp*)ir)->data))[4 + LIR_case_num(ir) * 4])

typedef LIRBaseOp LIR;

#endif
