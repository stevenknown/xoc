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
/*Copyright 2011 Alibaba Group*/
#include "liropcode.h"

#define lirOPCount 81

//GENERATED AUTOMATICALLY BY lir-gen
#define DECLARE_LIR_OPCODES                                                                \
LIR_OPCODE(LOP_NOP                  ,lirContinue                     ,lirFmtV              )\
LIR_OPCODE(LOP_begin1               ,0                               ,lirFmtV              )\
LIR_OPCODE(LOP_RETURN               ,lirReturn                       ,lirFmtA              )\
LIR_OPCODE(LOP_THROW                ,lirThrow                        ,lirFmtA              )\
LIR_OPCODE(LOP_MONITOR_ENTER        ,lirContinue                     ,lirFmtA              )\
LIR_OPCODE(LOP_MONITOR_EXIT         ,lirContinue                     ,lirFmtA              )\
LIR_OPCODE(LOP_MOVE_RESULT          ,lirContinue                     ,lirFmtR              )\
LIR_OPCODE(LOP_MOVE_EXCEPTION       ,lirContinue                     ,lirFmtR              )\
LIR_OPCODE(LOP_GOTO                 ,lirBranch                       ,lirFmtT              )\
LIR_OPCODE(LOP_begin2               ,0                               ,lirFmtV              )\
LIR_OPCODE(LOP_MOVE                 ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_NEG                  ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_NOT                  ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_CONVERT              ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_ADD_ASSIGN           ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_SUB_ASSIGN           ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_MUL_ASSIGN           ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_DIV_ASSIGN           ,lirContinue|lirThrow            ,lirFmtRA             )\
LIR_OPCODE(LOP_REM_ASSIGN           ,lirContinue|lirThrow            ,lirFmtRA             )\
LIR_OPCODE(LOP_AND_ASSIGN           ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_OR_ASSIGN            ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_XOR_ASSIGN           ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_SHL_ASSIGN           ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_SHR_ASSIGN           ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_USHR_ASSIGN          ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_ARRAY_LENGTH         ,lirContinue                     ,lirFmtRA             )\
LIR_OPCODE(LOP_CONST                ,lirContinue                     ,lirFmtRL             )\
LIR_OPCODE(LOP_IFZ                  ,lirContinue|lirBranch           ,lirFmtAT             )\
LIR_OPCODE(LOP_NEW_INSTANCE         ,lirContinue|lirThrow            ,lirFmtRS             )\
LIR_OPCODE(LOP_CONST_STRING         ,lirContinue|lirThrow            ,lirFmtRS             )\
LIR_OPCODE(LOP_CONST_CLASS          ,lirContinue|lirThrow            ,lirFmtRS             )\
LIR_OPCODE(LOP_SGET                 ,lirContinue|lirThrow            ,lirFmtRS             )\
LIR_OPCODE(LOP_CHECK_CAST           ,lirContinue|lirThrow            ,lirFmtAS             )\
LIR_OPCODE(LOP_SPUT                 ,lirContinue|lirThrow            ,lirFmtAS             )\
LIR_OPCODE(LOP_begin3               ,0                               ,lirFmtV              )\
LIR_OPCODE(LOP_APUT                 ,lirContinue|lirThrow            ,lirFmtABC            )\
LIR_OPCODE(LOP_AGET                 ,lirContinue|lirThrow            ,lirFmtRAB            )\
LIR_OPCODE(LOP_CMPL                 ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_CMP_LONG             ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_ADD                  ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_SUB                  ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_MUL                  ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_DIV                  ,lirContinue|lirThrow            ,lirFmtRAB            )\
LIR_OPCODE(LOP_REM                  ,lirContinue|lirThrow            ,lirFmtRAB            )\
LIR_OPCODE(LOP_AND                  ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_OR                   ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_XOR                  ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_SHL                  ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_SHR                  ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_USHR                 ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_IF                   ,lirContinue|lirBranch           ,lirFmtABT            )\
LIR_OPCODE(LOP_ADD_LIT              ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_SUB_LIT              ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_MUL_LIT              ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_DIV_LIT              ,lirContinue|lirThrow            ,lirFmtRAL            )\
LIR_OPCODE(LOP_REM_LIT              ,lirContinue|lirThrow            ,lirFmtRAL            )\
LIR_OPCODE(LOP_AND_LIT              ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_OR_LIT               ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_XOR_LIT              ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_SHL_LIT              ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_SHR_LIT              ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_USHR_LIT             ,lirContinue                     ,lirFmtRAL            )\
LIR_OPCODE(LOP_IPUT                 ,lirContinue|lirThrow            ,lirFmtABS            )\
LIR_OPCODE(LOP_IGET                 ,lirContinue|lirThrow            ,lirFmtRAS            )\
LIR_OPCODE(LOP_INSTANCE_OF          ,lirContinue|lirThrow            ,lirFmtRAS            )\
LIR_OPCODE(LOP_NEW_ARRAY            ,lirContinue|lirThrow            ,lirFmtRAS            )\
LIR_OPCODE(LOP_beginSwitch          ,0                               ,lirFmtV              )\
LIR_OPCODE(LOP_TABLE_SWITCH         ,lirContinue|lirSwitch           ,lirFmtSWITCH         )\
LIR_OPCODE(LOP_LOOKUP_SWITCH        ,lirContinue|lirSwitch           ,lirFmtSWITCH         )\
LIR_OPCODE(LOP_FILL_ARRAY_DATA      ,lirContinue|lirThrow            ,lirFmtSWITCH         )\
LIR_OPCODE(LOP_beginInvoke          ,0                               ,lirFmtV              )\
LIR_OPCODE(LOP_INVOKE               ,lirContinue|lirInvoke|lirThrow  ,lirFmtINVOKE         )\
LIR_OPCODE(LOP_FILLED_NEW_ARRAY     ,lirContinue|lirThrow            ,lirFmtINVOKE         )\
LIR_OPCODE(LOP_CMPG                 ,lirContinue                     ,lirFmtRAB            )\
LIR_OPCODE(LOP_PHI                  ,0                               , 0                   )\
LIR_OPCODE(LOP_PACKED_SWITCH_PAYLOAD,0                               ,lirFmt00X            )\
LIR_OPCODE(LOP_SPARSE_SWITCH_PAYLOAD,0                               ,lirFmt00X            )\




#define LIR_OPCODE(op,flags,du) flags,
static UInt8 gLIROpcodeFlags[lirOPCount] = {
    DECLARE_LIR_OPCODES
};
#undef LIR_OPCODE

#define LIR_OPCODE(op,flags,du) du,
static UInt8 gLIROpcodeFormats[lirOPCount] = {
    DECLARE_LIR_OPCODES
};
#undef LIR_OPCODE

const char* gLirFlags[33] = {
    "LIR_unknown"
    "LIR_JDT_void",
    "LIR_JDT_int",
    "LIR_JDT_object",
    "LIR_JDT_boolean",
    "LIR_JDT_byte",
    "LIR_JDT_char",
    "LIR_JDT_short",
    "LIR_JDT_float",

    "LIR_JDT_wide",
    "LIR_JDT_long",
    "LIR_JDT_double",

    "LIR_cond_EQ",
    "LIR_cond_NE",
    "LIR_cond_LT",
    "LIR_cond_GE",
    "LIR_cond_GT",
    "LIR_cond_LE",

    "LIR_convert_i2l",
    "LIR_convert_i2f",
    "LIR_convert_i2d",
    "LIR_convert_l2i",
    "LIR_convert_l2f",
    "LIR_convert_l2d",
    "LIR_convert_f2i",
    "LIR_convert_f2l",
    "LIR_convert_f2d",
    "LIR_convert_d2i",
    "LIR_convert_d2l",
    "LIR_convert_d2f",
    "LIR_convert_i2b",
    "LIR_convert_i2c",
    "LIR_convert_i2s",
};

const char* gLirOpName[lirOPCount] = {
    "nop",
    "begin1",
    "return",
    "throw",
    "monitor_enter",
    "monitor_exit",
    "move_result",
    "move_exception",
    "goto",
    "begin2",
    "move",
    "neg",
    "not",
    "convert",
    "add_assign",
    "sub_assign",
    "mul_assign",
    "div_assign",
    "rem_assign",
    "and_assign",
    "or_assign",
    "xor_assign",
    "shl_assign",
    "shr_assign",
    "ushr_assign",
    "array_length",
    "const",
    "ifz",
    "new_instance",
    "const_string",
    "const_class",
    "sget",
    "check_cast",
    "sput",
    "begin3",
    "aput",
    "aget",
    "cmpl",
    "cmp_long",
    "add",
    "sub",
    "mul",
    "div",
    "rem",
    "and",
    "or",
    "xor",
    "shl",
    "shr",
    "ushr",
    "if",
    "add_lit",
    "sub_lit",
    "mul_lit",
    "div_lit",
    "rem_lit",
    "and_lit",
    "or_lit",
    "xor_lit",
    "shl_lit",
    "shr_lit",
    "ushr_lit",
    "iput",
    "iget",
    "instance_of",
    "new_array",
    "beginswitch",
    "table_switch",
    "lookup_switch",
    "fill_array_data",
    "begininvoke",
    "invoke",
    "filled_new_array",
    "cmpg",
    "phi",
};


LIROpcodeInfoTables gLIROpcodeInfo = {
    gLIROpcodeFlags,
    gLIROpcodeFormats,
    gLirOpName,
    gLirFlags,
};

