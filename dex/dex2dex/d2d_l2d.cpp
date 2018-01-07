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
#include "cutils/log.h"
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/DexCatch.h"
#include "libdex/InstrUtils.h"
#include "libdex/DexProto.h"
#include "libdex/CmdUtils.h"
#include "io/cio.h"
#include "dir.h"
#include "liropcode.h"
#include <assert.h>
#include <stdio.h>
#include "lir.h"
#include "d2d_comm.h"
#include "d2d_l2d.h"
#include "d2d_d2l.h"

#include "cominc.h"
#include "comopt.h"
#include "xassert.h"
#include "drAlloc.h"
#include "utils/cbytestream.h"
#include "utils/clbe.h"
#include "d2d_dexlib.h"
#include "dex.h"
#include "gra.h"
#include "dex_hook.h"
#include "dex_util.h"
#include "drcode.h"

static inline UInt8 getlirOpcode(ULong codePtr){
    BYTE* data = (BYTE*)codePtr;
     return data[0];
}

static inline bool signedFitsIn16(Int32 value){
    return (Int16) value == value;
}

static inline bool unsignedFitsIn16(UInt32 value) {
    return value == (value & 0xffff);
}

static inline bool signedFitsIn8(Int32 value) {
    return (Int8) value == value;
}

static inline bool unsignedFitsIn8(UInt32 value) {
    return value == (value & 0xff);
}

static inline bool signedFitsIn4(Int32 value) {
    return (value >= -8) && (value <= 7);
}

static inline bool unsignedFitsIn4(UInt32 value) {
    return value == (value & 0xf);
}

static inline bool signedFitsInWide16(Int64 value){
    return (Int64)((Int16) value) == value;
}

static inline bool signedFitsInWide32(Int64 value){
    return (Int64)((Int32) value) == value;
}

#define WRITE_16(instrData,num) \
        *((UInt16*)instrData) = num;\
        instrData += 2;

#define WRITE_32(instrData,num) \
        WRITE_16(instrData,num&0xffff) \
        WRITE_16(instrData,(num >>16)&0xffff)

#define WRITE_FORMATAA(dexInstr,data) \
        {\
        dexInstr->instrSize = 2;\
        BYTE* instrData = (BYTE*)LIRMALLOC(2);\
        dexInstr->instrData = instrData; \
        ASSERT0(unsignedFitsIn8(data));\
        WRITE_16(instrData,((UInt8)(data) << 8)|((UInt8)dexOpCode))\
        }

#define WRITE_FORMATAABBBB(dexInstr,dataA,dataB) \
        dexInstr->instrSize = 4;\
        BYTE* instrData = (BYTE*)LIRMALLOC(4);\
        dexInstr->instrData = instrData; \
        ASSERT0(unsignedFitsIn8(dataA));\
        WRITE_16(instrData,((UInt8)(dataA) << 8)|((UInt8)dexOpCode))\
        WRITE_16(instrData, (UInt16)dataB);

#define WRITE_FORMATAABBBBBBBB(dexInstr,dataA,dataB) \
        dexInstr->instrSize = 6;\
        BYTE* instrData = (BYTE*)LIRMALLOC(6);\
        dexInstr->instrData = instrData; \
        ASSERT0(unsignedFitsIn8(dataA));\
        WRITE_16(instrData,((UInt8)(dataA) << 8)|((UInt8)dexOpCode))\
        WRITE_32(instrData, (UInt32)dataB)

#define WRITE_FORMATAABBBBBBBBBBBBBBBB(dexInstr,dataA,dataB) \
        dexInstr->instrSize = 10; \
        BYTE* instrData = (BYTE*)LIRMALLOC(10);\
        dexInstr->instrData = instrData; \
        ASSERT0(unsignedFitsIn8(dataA));\
        WRITE_16(instrData, ((UInt8)(dataA) << 8)|((UInt8)dexOpCode))\
        ::memcpy(instrData,&dataB,8);

#define WRITE_FORMATAABBCC(dexInstr,dataA,dataB,dataC) \
        dexInstr->instrSize = 4;\
        BYTE* instrData = (BYTE*)LIRMALLOC(4);\
        dexInstr->instrData = instrData; \
        ASSERT0(unsignedFitsIn8(dataA));\
        ASSERT0(unsignedFitsIn8(dataB));\
        ASSERT0(unsignedFitsIn8(dataC));\
        WRITE_16(instrData,((UInt8)(dataA) << 8)|((UInt8)dexOpCode))\
        WRITE_16(instrData,((UInt8)(dataC) << 8)|((UInt8)dataB & 0xff))

#define WRITE_FORMATAABBCCLIT(dexInstr,dataA,dataB,dataC) \
        dexInstr->instrSize = 4;\
        BYTE* instrData = (BYTE*)LIRMALLOC(4);\
        dexInstr->instrData = instrData; \
        ASSERT0(unsignedFitsIn8(dataA));\
        ASSERT0(unsignedFitsIn8(dataB));\
        ASSERT0(signedFitsIn8(dataC));\
        WRITE_16(instrData,((UInt8)(dataA) << 8)|((UInt8)dexOpCode))\
        WRITE_16(instrData,((UInt8)(dataC) << 8)|((UInt8)dataB & 0xff))

#define WRITE_FORMATAABBBBCCCC(dexInstr,dataA,dataB,dataC) \
        dexInstr->instrSize = 6;\
        BYTE* instrData = (BYTE*)LIRMALLOC(6);\
        dexInstr->instrData = instrData; \
        ASSERT0(unsignedFitsIn8(dataA));\
        WRITE_16(instrData,((UInt8)(dataA) << 8)|((UInt8)dexOpCode))\
        WRITE_16(instrData, (UInt16)dataB);\
        WRITE_16(instrData, (UInt16)dataC);

#define WRITE_FORMAT00AAAABBBB(dexInstr,dataA,dataB) \
        WRITE_FORMATAABBBBCCCC(dexInstr, 0, dataA, dataB)

#define WRITE_FORMATABCCCCDDDD(dexInstr,dataA,dataB,dataC,dataD) \
        ASSERT0(unsignedFitsIn4(dataA));\
        ASSERT0(unsignedFitsIn4(dataB));\
        UInt8 va = (UInt8)dataA; \
        UInt8 vb = (UInt8)dataB; \
        WRITE_FORMATAABBBBCCCC(dexInstr, (vb << 4) | (va & 0x0f),dataC,dataD)

#define WRITE_FORMATAB(dexInstr,dataA,dataB) \
        ASSERT0(unsignedFitsIn4(dataA));\
        ASSERT0(unsignedFitsIn4(dataB));\
        UInt8 va = (UInt8)dataA; \
        UInt8 vb = (UInt8)dataB; \
        WRITE_FORMATAA(dexInstr, (vb << 4) | (va & 0x0f))

#define WRITE_FORMATAALIT(dexInstr,data) \
        {\
        dexInstr->instrSize = 2;\
        BYTE* instrData = (BYTE*)LIRMALLOC(2);\
        dexInstr->instrData = instrData; \
        WRITE_16(instrData,((UInt8)(data) << 8)|((UInt8)dexOpCode))\
        }

#define WRITE_FORMATABLIT(dexInstr,dataA,dataB) \
        ASSERT0(unsignedFitsIn4(dataA));\
        ASSERT0(signedFitsIn4(dataB));\
        UInt8 va = (UInt8)dataA; \
        UInt8 vb = (UInt8)dataB; \
        WRITE_FORMATAALIT(dexInstr, (vb << 4) | (va & 0x0f))

#define WRITE_FORMATABCCCC(dexInstr,dataA,dataB,dataC) \
        ASSERT0(unsignedFitsIn4(dataA));\
        ASSERT0(unsignedFitsIn4(dataB));\
         UInt8 va = (UInt8)dataA;\
        UInt8 vb = (UInt8)dataB;\
        WRITE_FORMATAABBBB(dexInstr, (vb << 4) | (va & 0x0f),dataC)

static Int32 processJumpLIRABCOp(D2DdexInstr* dexInstr, LIRABCOp* lir){
    Int32 err = 0;
    UInt8 dexOpCode= 0;

    switch(lir->flags){
        case LIR_cond_EQ: dexOpCode = OP_IF_EQ; break;
        case LIR_cond_NE: dexOpCode = OP_IF_NE; break;
        case LIR_cond_LT: dexOpCode = OP_IF_LT; break;
        case LIR_cond_GE: dexOpCode = OP_IF_GE; break;
        case LIR_cond_GT: dexOpCode = OP_IF_GT; break;
        case LIR_cond_LE: dexOpCode = OP_IF_LE; break;
    }

    WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)

    return err;
}

static Int32 processLIRAOp(D2DdexInstr* dexInstr,LIRAOp* lir)
{
    Int32 err = 0;
    UInt8 dexOpCode = 0;

    switch(lir->opcode){
        case LOP_RETURN:{
            switch(lir->flags){
            case LIR_JDT_void:
                dexOpCode = OP_RETURN_VOID;
                WRITE_FORMATAA(dexInstr, 0)
                break;
            case 0:
                dexOpCode = OP_RETURN;
                WRITE_FORMATAA(dexInstr,lir->vA)
                break;
            case LIR_JDT_object:
                 dexOpCode = OP_RETURN_OBJECT;
                 WRITE_FORMATAA(dexInstr,lir->vA)
                 break;
            case LIR_JDT_wide:
                 dexOpCode = OP_RETURN_WIDE;
                 WRITE_FORMATAA(dexInstr,lir->vA)
                 break;
            }
            break;
        }
        case LOP_THROW:{
            dexOpCode = OP_THROW;
            WRITE_FORMATAA(dexInstr,lir->vA)
            break;
        }
        case LOP_MONITOR_ENTER:{
            dexOpCode = OP_MONITOR_ENTER;
            WRITE_FORMATAA(dexInstr,lir->vA)
            break;
        }
        case LOP_MONITOR_EXIT:{
            dexOpCode = OP_MONITOR_EXIT;
            WRITE_FORMATAA(dexInstr,lir->vA)
            break;
        }
        case LOP_MOVE_RESULT:{
            switch(lir->flags)
            {
                case 0: dexOpCode = OP_MOVE_RESULT; break;
                case LIR_JDT_wide: dexOpCode = OP_MOVE_RESULT_WIDE; break;
                case LIR_JDT_object: dexOpCode = OP_MOVE_RESULT_OBJECT; break;
            }
            WRITE_FORMATAA(dexInstr,lir->vA)
            break;
        }
        case LOP_MOVE_EXCEPTION:{
            dexOpCode = OP_MOVE_EXCEPTION;
            WRITE_FORMATAA(dexInstr,lir->vA)
            break;
        }
    }

    return err;
}

static Int32 processLIRGOTOOp(D2DdexInstr* dexInstr, LIRGOTOOp* lir){
    Int32 err = 0;
    UInt8 dexOpCode = 0;

    UInt32 targetNum = (UInt32)lir->target;
    dexOpCode = OP_GOTO;

    WRITE_FORMATAABBBBBBBB(dexInstr, 0, targetNum)

    return err;
}

static Int32 genMov(D2DdexInstr* dexInstr, LIRABOp* lir)
{
    Int32 err = 0;
    UInt8 dexOpCode = 0;
    int destReg = lir->vA;
    int srcReg = lir->vB;

    switch(lir->flags){
        case 0:
            if ((srcReg | destReg) < 16) {
                dexOpCode = OP_MOVE;
                WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            } else if (destReg < 256) {
                dexOpCode = OP_MOVE_FROM16;
                WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            } else {
                dexOpCode = OP_MOVE_16;
                WRITE_FORMAT00AAAABBBB(dexInstr,lir->vA,lir->vB)
            }
            break;
        case LIR_JDT_wide:
            if ((srcReg | destReg) < 16) {
                dexOpCode = OP_MOVE_WIDE;
                WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            } else if (destReg < 256) {
                dexOpCode = OP_MOVE_WIDE_FROM16;
                WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            } else {
                dexOpCode = OP_MOVE_WIDE_16;
                WRITE_FORMAT00AAAABBBB(dexInstr,lir->vA,lir->vB)
            }
            break;
        case LIR_JDT_object:
            if ((srcReg | destReg) < 16) {
                dexOpCode = OP_MOVE_OBJECT;
                WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            } else if (destReg < 256) {
                dexOpCode = OP_MOVE_OBJECT_FROM16;
                WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            } else {
                dexOpCode = OP_MOVE_OBJECT_16;
                WRITE_FORMAT00AAAABBBB(dexInstr,lir->vA,lir->vB)
            }
            break;
    }
    return err;
}

static Int32 processLIRABOp(D2DdexInstr* dexInstr, LIRABOp* lir)
{
    Int32 err = 0;
    UInt8 dexOpCode = 0;

    switch(lir->opcode){
        case LOP_MOVE:{
            genMov(dexInstr,lir);
            break;
        }
        case LOP_NEG:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_NEG_INT; break;
            case LIR_JDT_long: dexOpCode = OP_NEG_LONG; break;
            case LIR_JDT_float: dexOpCode = OP_NEG_FLOAT; break;
            case LIR_JDT_double: dexOpCode = OP_NEG_DOUBLE; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_NOT:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_NOT_INT; break;
            case LIR_JDT_long: dexOpCode = OP_NOT_LONG; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_CONVERT:{
            switch(lir->flags){
            case LIR_convert_i2l: dexOpCode = OP_INT_TO_LONG; break;
            case LIR_convert_i2f: dexOpCode = OP_INT_TO_FLOAT; break;
            case LIR_convert_i2d: dexOpCode = OP_INT_TO_DOUBLE; break;
            case LIR_convert_l2i: dexOpCode = OP_LONG_TO_INT; break;
            case LIR_convert_l2f: dexOpCode = OP_LONG_TO_FLOAT; break;
            case LIR_convert_l2d: dexOpCode = OP_LONG_TO_DOUBLE; break;
            case LIR_convert_f2i: dexOpCode = OP_FLOAT_TO_INT; break;
            case LIR_convert_f2l: dexOpCode = OP_FLOAT_TO_LONG; break;
            case LIR_convert_f2d: dexOpCode = OP_FLOAT_TO_DOUBLE; break;
            case LIR_convert_d2i: dexOpCode = OP_DOUBLE_TO_INT; break;
            case LIR_convert_d2l: dexOpCode = OP_DOUBLE_TO_LONG; break;
            case LIR_convert_d2f: dexOpCode = OP_DOUBLE_TO_FLOAT; break;
            case LIR_convert_i2b: dexOpCode = OP_INT_TO_BYTE; break;
            case LIR_convert_i2c: dexOpCode = OP_INT_TO_CHAR; break;
            case LIR_convert_i2s: dexOpCode = OP_INT_TO_SHORT; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_ADD_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_ADD_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_ADD_LONG_2ADDR; break;
            case LIR_JDT_float: dexOpCode = OP_ADD_FLOAT_2ADDR; break;
            case LIR_JDT_double: dexOpCode = OP_ADD_DOUBLE_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_SUB_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_SUB_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_SUB_LONG_2ADDR; break;
            case LIR_JDT_float: dexOpCode = OP_SUB_FLOAT_2ADDR; break;
            case LIR_JDT_double: dexOpCode = OP_SUB_DOUBLE_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_MUL_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_MUL_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_MUL_LONG_2ADDR; break;
            case LIR_JDT_float: dexOpCode = OP_MUL_FLOAT_2ADDR; break;
            case LIR_JDT_double: dexOpCode = OP_MUL_DOUBLE_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_DIV_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_DIV_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_DIV_LONG_2ADDR; break;
            case LIR_JDT_float: dexOpCode = OP_DIV_FLOAT_2ADDR; break;
            case LIR_JDT_double: dexOpCode = OP_DIV_DOUBLE_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_REM_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_REM_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_REM_LONG_2ADDR; break;
            case LIR_JDT_float: dexOpCode = OP_REM_FLOAT_2ADDR; break;
            case LIR_JDT_double: dexOpCode = OP_REM_DOUBLE_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_AND_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_AND_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_AND_LONG_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_OR_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_OR_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_OR_LONG_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_XOR_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_XOR_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_XOR_LONG_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_SHL_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_SHL_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_SHL_LONG_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_SHR_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_SHR_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_SHR_LONG_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_USHR_ASSIGN:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_USHR_INT_2ADDR; break;
            case LIR_JDT_long: dexOpCode = OP_USHR_LONG_2ADDR; break;
            }
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_ARRAY_LENGTH:{
            dexOpCode = OP_ARRAY_LENGTH;
            WRITE_FORMATAB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_NEW_INSTANCE:{
            dexOpCode = OP_NEW_INSTANCE;
            WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_CONST_STRING:{
            if(unsignedFitsIn16(lir->vB)){
                dexOpCode = OP_CONST_STRING;
                WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            }else{
                dexOpCode = OP_CONST_STRING_JUMBO;
                WRITE_FORMATAABBBBBBBB(dexInstr,lir->vA,lir->vB)
            }
            break;
        }
        case LOP_CONST_CLASS:{
            dexOpCode = OP_CONST_CLASS;
            WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_SGET:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_SGET; break;
            case LIR_JDT_wide: dexOpCode = OP_SGET_WIDE; break;
            case LIR_JDT_object: dexOpCode = OP_SGET_OBJECT; break;
            case LIR_JDT_boolean: dexOpCode = OP_SGET_BOOLEAN; break;
            case LIR_JDT_byte: dexOpCode = OP_SGET_BYTE; break;
            case LIR_JDT_char: dexOpCode = OP_SGET_CHAR; break;
            case LIR_JDT_short: dexOpCode = OP_SGET_SHORT; break;
            }
            WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_CHECK_CAST:{
            dexOpCode = OP_CHECK_CAST;
            WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            break;
        }
        case LOP_SPUT:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_SPUT; break;
            case LIR_JDT_wide: dexOpCode = OP_SPUT_WIDE; break;
            case LIR_JDT_object: dexOpCode = OP_SPUT_OBJECT; break;
            case LIR_JDT_boolean: dexOpCode = OP_SPUT_BOOLEAN; break;
            case LIR_JDT_byte: dexOpCode = OP_SPUT_BYTE; break;
            case LIR_JDT_char: dexOpCode = OP_SPUT_CHAR; break;
            case LIR_JDT_short: dexOpCode = OP_SPUT_SHORT; break;
            }
            WRITE_FORMATAABBBB(dexInstr,lir->vA,lir->vB)
            break;
        }
    }

    return err;
}

static Int32 processJumpLIRABOp(D2DdexInstr* dexInstr, LIRABOp* lir){
    Int32 err = 0;
    UInt8 dexOpCode = 0;

    switch(lir->flags){
        case LIR_cond_EQ: dexOpCode = OP_IF_EQZ; break;
        case LIR_cond_NE: dexOpCode = OP_IF_NEZ; break;
        case LIR_cond_LT: dexOpCode = OP_IF_LTZ; break;
        case LIR_cond_GE: dexOpCode = OP_IF_GEZ; break;
        case LIR_cond_GT: dexOpCode = OP_IF_GTZ; break;
        case LIR_cond_LE: dexOpCode = OP_IF_LEZ; break;
    }

    WRITE_FORMATAABBBB(dexInstr, lir->vA, lir->vB)
    return err;
}

static Int32 processLIRConstOp(D2DdexInstr* dexInstr,LIRConstOp* lir)
{
    Int32 err = 0;
    UInt8 dexOpCode = 0;
    UInt32 destReg = lir->vA;

    switch(lir->flags){
        case 0: {
            Int32 data = (UInt32)lir->vB;
            if(signedFitsIn4(data) && destReg < 16){
                dexOpCode = OP_CONST_4;
                WRITE_FORMATABLIT(dexInstr, lir->vA, data)
            }else if(signedFitsIn16(data)){
                dexOpCode = OP_CONST_16;
                WRITE_FORMATAABBBB(dexInstr,lir->vA,(Int16)data)
            }else{
                dexOpCode = OP_CONST;
                WRITE_FORMATAABBBBBBBB(dexInstr,lir->vA,data)
            }
            break;
        }
        case LIR_JDT_wide:{
            Int64 data = (Int64)lir->vB;
            if(signedFitsInWide16(data)){
                dexOpCode = OP_CONST_WIDE_16;
                WRITE_FORMATAABBBB(dexInstr,lir->vA, data)
            }else if(signedFitsInWide32(data)){
                dexOpCode = OP_CONST_WIDE_32;
                WRITE_FORMATAABBBBBBBB(dexInstr,lir->vA, lir->vB)
            }else{
                dexOpCode = OP_CONST_WIDE;
                WRITE_FORMATAABBBBBBBBBBBBBBBB(dexInstr, lir->vA, lir->vB)
            }
            break;
        }
    }
    return err;
}

static Int32 processLIRABCOp(D2DdexInstr* dexInstr, LIRABCOp* lir){
    Int32 err = 0;
    UInt8 dexOpCode = 0;

    switch(lir->opcode){
        case LOP_APUT:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_APUT; break;
            case LIR_JDT_wide: dexOpCode = OP_APUT_WIDE; break;
            case LIR_JDT_object: dexOpCode = OP_APUT_OBJECT; break;
            case LIR_JDT_boolean: dexOpCode = OP_APUT_BOOLEAN; break;
            case LIR_JDT_byte: dexOpCode = OP_APUT_BYTE; break;
            case LIR_JDT_char: dexOpCode = OP_APUT_CHAR; break;
            case LIR_JDT_short: dexOpCode = OP_APUT_SHORT; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_AGET:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_AGET; break;
            case LIR_JDT_wide: dexOpCode = OP_AGET_WIDE; break;
            case LIR_JDT_object: dexOpCode = OP_AGET_OBJECT; break;
            case LIR_JDT_boolean: dexOpCode = OP_AGET_BOOLEAN; break;
            case LIR_JDT_byte: dexOpCode = OP_AGET_BYTE; break;
            case LIR_JDT_char: dexOpCode = OP_AGET_CHAR; break;
            case LIR_JDT_short: dexOpCode = OP_AGET_SHORT; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_CMPL:{
            switch(lir->flags){
            case LIR_CMP_float: dexOpCode = OP_CMPL_FLOAT; break;
            //case LIR_JDT_float: dexOpCode = lc_fcmpg; break;
            case LIR_CMP_double: dexOpCode = OP_CMPL_DOUBLE; break;
            //case LIR_JDT_double: dexOpCode = lc_dcmpg; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }

            /*add by gk*/
        case LOP_CMPG:{
            switch(lir->flags){
            case LIR_CMP_float: dexOpCode = OP_CMPG_FLOAT; break;
            //case LIR_JDT_float: dexOpCode = lc_fcmpg; break;
            case LIR_CMP_double: dexOpCode = OP_CMPG_DOUBLE; break;
            //case LIR_JDT_double: dexOpCode = lc_dcmpg; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }

        case LOP_CMP_LONG:{
            dexOpCode = OP_CMP_LONG;
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_ADD:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_ADD_INT; break;
            case LIR_JDT_long: dexOpCode = OP_ADD_LONG; break;
            case LIR_JDT_float: dexOpCode = OP_ADD_FLOAT; break;
            case LIR_JDT_double: dexOpCode = OP_ADD_DOUBLE; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_SUB:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_SUB_INT; break;
            case LIR_JDT_long: dexOpCode = OP_SUB_LONG; break;
            case LIR_JDT_float: dexOpCode = OP_SUB_FLOAT; break;
            case LIR_JDT_double: dexOpCode = OP_SUB_DOUBLE; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_MUL:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_MUL_INT; break;
            case LIR_JDT_long: dexOpCode = OP_MUL_LONG; break;
            case LIR_JDT_float: dexOpCode = OP_MUL_FLOAT; break;
            case LIR_JDT_double: dexOpCode = OP_MUL_DOUBLE; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_DIV:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_DIV_INT; break;
            case LIR_JDT_long: dexOpCode = OP_DIV_LONG; break;
            case LIR_JDT_float: dexOpCode = OP_DIV_FLOAT; break;
            case LIR_JDT_double: dexOpCode = OP_DIV_DOUBLE; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_REM:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_REM_INT; break;
            case LIR_JDT_long: dexOpCode = OP_REM_LONG; break;
            case LIR_JDT_float: dexOpCode = OP_REM_FLOAT; break;
            case LIR_JDT_double: dexOpCode = OP_REM_DOUBLE; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_AND:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_AND_INT; break;
            case LIR_JDT_long: dexOpCode = OP_AND_LONG; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_OR:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_OR_INT; break;
            case LIR_JDT_long: dexOpCode = OP_OR_LONG; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_XOR:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_XOR_INT; break;
            case LIR_JDT_long: dexOpCode = OP_XOR_LONG; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_SHL:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_SHL_INT; break;
            case LIR_JDT_long: dexOpCode = OP_SHL_LONG; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_SHR:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_SHR_INT; break;
            case LIR_JDT_long: dexOpCode = OP_SHR_LONG; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_USHR:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_USHR_INT; break;
            case LIR_JDT_long: dexOpCode = OP_USHR_LONG; break;
            }
            WRITE_FORMATAABBCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_ADD_LIT:{
            if(signedFitsIn8(lir->vC)){
                dexOpCode = OP_ADD_INT_LIT8;
                WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            }else{
                dexOpCode = OP_ADD_INT_LIT16;
                WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            }
            break;
        }
        case LOP_SUB_LIT:{
            if(signedFitsIn8(lir->vC)){
                dexOpCode = OP_RSUB_INT_LIT8;
                WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            }else{
                dexOpCode = OP_RSUB_INT;
                WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            }
            break;
        }
        case LOP_MUL_LIT:{
            if(signedFitsIn8(lir->vC)){
                dexOpCode = OP_MUL_INT_LIT8;
                WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            }else{
                dexOpCode = OP_MUL_INT_LIT16;
                WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            }
            break;
        }
        case LOP_DIV_LIT:{
            if(signedFitsIn8(lir->vC)){
                dexOpCode = OP_DIV_INT_LIT8;
                WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            }else{
                dexOpCode = OP_DIV_INT_LIT16;
                WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            }
            break;
        }
        case LOP_REM_LIT:{
            if(signedFitsIn8(lir->vC)){
                dexOpCode = OP_REM_INT_LIT8;
                WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            }else{
                dexOpCode = OP_REM_INT_LIT16;
                WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            }
            break;
        }
        case LOP_AND_LIT:{
            if(signedFitsIn8(lir->vC)){
                dexOpCode = OP_AND_INT_LIT8;
                WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            }else{
                dexOpCode = OP_AND_INT_LIT16;
                WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            }
            break;
        }
        case LOP_OR_LIT:{
            if(signedFitsIn8(lir->vC)){
                dexOpCode = OP_OR_INT_LIT8;
                WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            }else{
                dexOpCode = OP_OR_INT_LIT16;
                WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            }
            break;
        }
        case LOP_XOR_LIT:{
            if(signedFitsIn8(lir->vC)){
                dexOpCode = OP_XOR_INT_LIT8;
                WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            }else{
                dexOpCode = OP_XOR_INT_LIT16;
                WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            }
            break;
        }
        case LOP_SHL_LIT:{
            dexOpCode = OP_SHL_INT_LIT8;
            WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_SHR_LIT:{
            dexOpCode = OP_SHR_INT_LIT8;
            WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_USHR_LIT:{
            dexOpCode = OP_USHR_INT_LIT8;
            WRITE_FORMATAABBCCLIT(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_IPUT:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_IPUT; break;
            case LIR_JDT_wide: dexOpCode = OP_IPUT_WIDE; break;
            case LIR_JDT_object: dexOpCode = OP_IPUT_OBJECT; break;
            case LIR_JDT_boolean: dexOpCode = OP_IPUT_BOOLEAN; break;
            case LIR_JDT_byte: dexOpCode = OP_IPUT_BYTE; break;
            case LIR_JDT_char: dexOpCode = OP_IPUT_CHAR; break;
            case LIR_JDT_short: dexOpCode = OP_IPUT_SHORT; break;
            }
            WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_IGET:{
            switch(lir->flags){
            case LIR_JDT_int: dexOpCode = OP_IGET; break;
            case LIR_JDT_wide: dexOpCode = OP_IGET_WIDE; break;
            case LIR_JDT_object: dexOpCode = OP_IGET_OBJECT; break;
            case LIR_JDT_boolean: dexOpCode = OP_IGET_BOOLEAN; break;
            case LIR_JDT_byte: dexOpCode = OP_IGET_BYTE; break;
            case LIR_JDT_char: dexOpCode = OP_IGET_CHAR; break;
            case LIR_JDT_short: dexOpCode = OP_IGET_SHORT; break;
            }
            WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_INSTANCE_OF:{
            dexOpCode = OP_INSTANCE_OF;
            WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
        case LOP_NEW_ARRAY:{
            dexOpCode = OP_NEW_ARRAY;
            WRITE_FORMATABCCCC(dexInstr,lir->vA,lir->vB,lir->vC)
            break;
        }
    }

    return err;
}

static Int32 processLIRSwitchOp(D2DdexInstr* dexInstr, CBSHandle dataHandler, LIRSwitchOp* lir){
    Int32 err = 0;
    UInt8 dexOpCode=0;
    UInt32 offset = 0;
    UInt16 signature=0;
    UInt16* data;
    UInt32 dataSize=0;

    data = lir->data;
    switch(lir->opcode){
        case LOP_TABLE_SWITCH:{
            // TABLE_SWITCH should be 4-byte aligned.
            alignLbs(dataHandler);
            dexOpCode = OP_PACKED_SWITCH;
            signature = L2D_kPackedSwitchSignature;
            dataSize = 8 + data[1] * 4;
            break;
        }
        case LOP_LOOKUP_SWITCH:{
            // TLOOKUP_SWITCH should be 4-byte aligned.
            alignLbs(dataHandler);
            dexOpCode = OP_SPARSE_SWITCH;
            signature = L2D_kSparseSwitchSignature;
            dataSize = 4 + data[1] * 8;
            break;
        }
        case LOP_FILL_ARRAY_DATA:{
            // FILL_ARRAY_DATA should be 4-byte aligned.
            alignLbs(dataHandler);
            dexOpCode = OP_FILL_ARRAY_DATA;
            signature = L2D_kArrayDataSignature;
            UInt16 elemWidth = data[1];
            UInt32 len = data[2] | (((UInt32)data[3]) << 16);
            dataSize = (4 + (elemWidth * len + 1)/2) * 2;
            break;
        }
    }

    offset = cbsGetSize(dataHandler);
    err += cbsWrite16(dataHandler, signature);
    err += cbsWrite(dataHandler, data + 1, dataSize - 2);

    WRITE_FORMATAABBBBBBBB(dexInstr, lir->value, offset);
    return err;
}

static Int32 processLIRInvokeOp(D2DdexInstr* dexInstr, LIRInvokeOp* lir)
{
    Int32 err = 0;
    UInt8 dexOpCode = 0;
    UInt8 flag = lir->flags;

   //clear high 4bit
    UInt8 flag1 = flag & 0x0f;
    //clear low 4bit
    UInt8 flag2  = flag & 0xf0;

    /*in dalvik, there is no execute inline*/
#if 0
    /*is not rang but a inline*/
    if((flag & LIR_invoke_inline) != 0 && (((flag2) & LIR_Range) == 0))
    {
        dexOpCode = lc_execute_inline;
        lir->ref = lir->exeRef;
        goto END;
    }
#endif
    //flag2 &= ~(LIR_invoke_inline);

    switch(flag1){
        case LIR_invoke_virtual:{
            switch(flag2)
            {
            case 0: dexOpCode = OP_INVOKE_VIRTUAL; break;
            case LIR_Range: dexOpCode = OP_INVOKE_VIRTUAL_RANGE; break;
            }
            break;
        }
        case LIR_invoke_super:{
            switch(flag2)
            {
            case 0: dexOpCode = OP_INVOKE_SUPER; break;
            case LIR_Range: dexOpCode = OP_INVOKE_SUPER_RANGE; break;
            }
            break;
        }
        case LIR_invoke_direct:{
            switch(flag2)
            {
            case 0: dexOpCode = OP_INVOKE_DIRECT; break;
            case LIR_Range: dexOpCode = OP_INVOKE_DIRECT_RANGE; break;
            }
            break;
        }
        case LIR_invoke_static:{
            switch(flag2)
            {
            case 0: dexOpCode = OP_INVOKE_STATIC; break;
            case LIR_Range: dexOpCode = OP_INVOKE_STATIC_RANGE; break;
            }
            break;
        }
        case LIR_invoke_interface:{
            switch(flag2)
            {
            case 0: dexOpCode = OP_INVOKE_INTERFACE; break;
            case LIR_Range: dexOpCode = OP_INVOKE_INTERFACE_RANGE; break;
            }
            break;
        }
        case 0:{
            switch(flag2)
            {
            case 0: dexOpCode = OP_FILLED_NEW_ARRAY; break;
            case LIR_Range: dexOpCode = OP_FILLED_NEW_ARRAY_RANGE; break;
            }
            break;
        }
    }

    if(flag2 != LIR_Range){
        UInt32 i,count;
        UInt8 args5 = 0;
        UInt16 regList = 0;
        UInt16 methodIdx;

        methodIdx = lir->ref;
        count = lir->argc;

        if(count == 5){
            args5 = lir->args[4];
            count --;
        }
        for(i = 0; i < count; i++){
            regList |= lir->args[count - i - 1];
            if( i != count - 1)
                regList<<= 4;
        }
        WRITE_FORMATABCCCCDDDD(dexInstr,args5,lir->argc,methodIdx,regList)
    }else{
        WRITE_FORMATAABBBBCCCC(dexInstr,lir->argc,lir->ref,lir->args[0])
    }

    return err;
}

static inline UInt32 getDexOffset(D2DdexInstrList const* instrList, UInt32 num){
    return instrList->instr[num].instrOffset;
}

static UInt32 checkGOTOFits(TargetInsn* insn){
    UInt32 oldSize;
    UInt32 newSize;
    UInt32 targetOffset = (Int32)(insn->targetOffset - insn->offset) / 2;

    oldSize = insn->size;
    switch(oldSize){
    case 2:
        insn->lastfixSize = 0;
        break;
    case 4:
        if(signedFitsIn8(targetOffset)){
            insn->size = 2;
            insn->lastfixSize = 2;
        }else{
            insn->lastfixSize = 0;
        }
        break;
    case 6:
        if(signedFitsIn8(targetOffset)){
            insn->size = 2;
            insn->lastfixSize = 4;
        }else if(signedFitsIn16(targetOffset)){
            insn->size = 4;
            insn->lastfixSize = 2;
        }else{
            insn->lastfixSize = 0;
        }
        break;
    }

    return     insn->lastfixSize;
}

static Int32 writeByteCodeAndFixOffset(CBSHandle regIns, D2DdexInstrList* dexInstrList){
    UInt32 instrCount = dexInstrList->instrCount;
    D2DdexInstr* instrList = dexInstrList->instr;
    UInt32 i;
    Int32 err = 0;

    UInt32 fixOffset = 0;
    for(i = 0; i < instrCount; i++){
        D2DdexInstr* dexInstr =  instrList + i;
        BYTE* ip = dexInstr->instrData;

        switch(ip[0]){
            case OP_GOTO:
            {
                //sub first
                dexInstr->instrOffset -= fixOffset;

                UInt32 instrNum = cReadLE32(ip + 2);
                if(dexInstr->instrSize == 2){
                    fixOffset += 4;
                    //cWriteLE32 = instrNum;
                    err += cbsWrite8(regIns, (UInt8)OP_GOTO);
                    err += cbsWrite8(regIns, (Int8)0);
                }else if(dexInstr->instrSize == 4){
                    fixOffset += 2;
                    err += cbsWrite8(regIns, (UInt8)OP_GOTO_16);
                    err += cbsWrite8(regIns, 0);
                    err += cbsWrite16(regIns, (Int16)instrNum);
                }else{
                    err += cbsWrite8(regIns, (UInt8)OP_GOTO_32);
                    err += cbsWrite8(regIns, 0);
                    err += cbsWrite32(regIns, (UInt32)instrNum);
                }

            break;
        }
        default:{
            err += cbsWrite(regIns, dexInstr->instrData, dexInstr->instrSize);
            dexInstr->instrOffset -= fixOffset;
            }
        }
    }

    return err;
}

/*it just recompute the goto's instr size, but not fix the offset*/
static void genGOTO(D2DdexInstrList* dexInstrList, UInt32 numGoto)
{
    UInt32 instrCount = dexInstrList->instrCount;
    D2DdexInstr* instrList = dexInstrList->instr;
    UInt32 i,j;
    UInt32 curTarget = 0;

    TargetInsn targetList[numGoto];
    //1. gen target list
    for(i = 0; i < instrCount; i++)
    {
        D2DdexInstr* dexInstr = instrList + i;
        BYTE* ip = dexInstr->instrData;

        if(ip[0] == OP_GOTO){
            UInt16 targetInstrNum = cReadLE32(ip + 2);
            TargetInsn* insn = targetList + curTarget;

            insn->targetNum = targetInstrNum;
            insn->num = i;
            insn->size = 6;
            insn->offset = dexInstr->instrOffset;

            insn->targetOffset = getDexOffset(dexInstrList, targetInstrNum);
            curTarget ++;
        }
    }

    //2.fixOffset until right
    //UInt32 fixOffset = 0;
    while(true)
    {
        UInt32 curFixOffset = 0;

        for(i = 0; i < curTarget; i++){
            TargetInsn* insn = targetList + i;
            insn->offset -= curFixOffset;

            if(insn->targetNum > insn->num){
                insn->targetOffset -= curFixOffset;
            }else{
                UInt32 tmpFixOffset = 0;
                for(j = 0; j < i; j++){
                        TargetInsn* tinsn = targetList + j;
                        if(tinsn->num > insn->targetNum)
                            break;
                        tmpFixOffset += tinsn->lastfixSize;
                }
                insn->targetOffset -= tmpFixOffset;
            }

            curFixOffset += checkGOTOFits(insn);
        }

        if(curFixOffset != 0){
            curFixOffset = 0;
        }else{
            break;
        }
    }

    //3.rewrite lex instr offset
    for(i = 0; i < curTarget; i++){
        TargetInsn* insn = targetList + i;

        D2DdexInstr* dexInstr =  instrList + insn->num;
        dexInstr->instrSize = insn->size;
    }

    return;
}

static Int32 fixJumpOffset_orig(D2Dpool* pool, CBSHandle regIns, CBSHandle dataHandler,
        D2DdexInstrList* dexInstrList, DexCode* nCode)
{
    UInt32 instrCount = dexInstrList->instrCount;
    D2DdexInstr* instrList = dexInstrList->instr;
    UInt8 opcode;
    UInt32 i;
    Int32 err = 0;
    UInt32 tCodeSize = 0;

    tCodeSize = cbsGetSize(regIns);
    if(dataHandler != 0){
        BYTE* data = cbsGetData(dataHandler);
        UInt32 dataSize = cbsGetSize(dataHandler);
        /*the switch data should be 4byte aligne*/
        alignLbs(regIns);

        tCodeSize = cbsGetSize(regIns);
        err += cbsWrite(regIns, data, dataSize);

        cbsDestroy(dataHandler);
    }
    nCode->insnsSize = cbsGetSize(regIns) / 2;

    alignLbs(regIns);

    BYTE* ip = cbsGetData(regIns);
    BYTE* switchBase = ip + tCodeSize;

    //2.fix jump offset
    for(i = 0; i < instrCount; i++){
        D2DdexInstr* dexInstr =  instrList + i;

        switch(ip[0]){
            case OP_IF_EQ:
            case OP_IF_NE:
            case OP_IF_LT:
            case OP_IF_GE:
            case OP_IF_GT:
            case OP_IF_LE:
            case OP_IF_EQZ:
            case OP_IF_NEZ:
            case OP_IF_LTZ:
            case OP_IF_GEZ:
            case OP_IF_GTZ:
            case OP_IF_LEZ:
            {
                UInt16 instrNum = *((UInt16*)(ip + 2));
                UInt32 targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                Int32  targetOffset = (Int32)(targetInstrOffset - dexInstr->instrOffset)/2;
                if(!signedFitsIn16(targetOffset)){
                    ABORT();
                }
                *((Int16*)(ip + 2)) = (Int16)targetOffset;
                break;
            }
            case OP_GOTO_16:
            {
                UInt32 instrNum = cReadLE32(dexInstr->instrData + 2);
                UInt32 targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                Int32  targetOffset = (Int32)(targetInstrOffset - dexInstr->instrOffset)/2;
                if(!signedFitsIn16(targetOffset)){
                    ABORT();
                }
                *((Int16*)(ip + 2)) = (Int16)targetOffset;
                break;
            }
            case OP_GOTO:
            {
                UInt32 instrNum = cReadLE32(dexInstr->instrData + 2);
                UInt32 targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                Int32  targetOffset = (Int32)(targetInstrOffset - dexInstr->instrOffset)/2;
                if(!signedFitsIn8(targetOffset)){
                    abort();
                }
                *(ip + 1) = (Int8)targetOffset;
                break;
            }
            case OP_GOTO_32:
            {
                UInt32 instrNum = cReadLE32(dexInstr->instrData + 2);
                UInt32 targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                Int32  targetOffset = (Int32)(targetInstrOffset - (Int32)dexInstr->instrOffset)/2;
                if(targetOffset == 0)
                    abort();
                cWriteLE32(ip + 2, targetOffset);
                break;
            }
            case OP_PACKED_SWITCH:
            {
                UInt32 j;
                Int32 targetOffset;
                Int32* target;
                Int32 dexInstrOffset;
                UInt32 size;
                UInt16* data;

                targetOffset = (UInt32)cReadLE32(ip + 2);
                data = (UInt16*)(switchBase + targetOffset);
                size =  data[1];
                target = (Int32*)(((BYTE*)data) + 8);

                dexInstrOffset = dexInstr->instrOffset;
                for(j = 0; j < size; j++){
                    Int32 targetInstrOffset;
                    UInt32 instrNum;

                    instrNum = (UInt32)target[j];

                    targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                    target[j] = ((Int32)(targetInstrOffset - (Int32)dexInstrOffset))/2;
                }

                targetOffset = (tCodeSize + targetOffset - (Int32)dexInstrOffset)/2;
                cWriteLE32(ip + 2, targetOffset);
                break;
            }
            case OP_SPARSE_SWITCH:
            {
                UInt32 j;
                Int32 targetOffset;
                Int32 dexInstrOffset;
                Int32* target;
                UInt32 size;
                UInt16* data;

                targetOffset = (UInt32)cReadLE32(ip + 2);
                data = (UInt16*)(switchBase + targetOffset);
                size =  data[1];
                target = (Int32*)(((BYTE*)data) + 4 + 4*size);
                dexInstrOffset = dexInstr->instrOffset;

                for(j = 0; j < size; j++){
                    Int32 targetInstrOffset;
                    UInt32 instrNum;

                    instrNum = (UInt32)target[j];
                    targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                    target[j] = (Int32)((targetInstrOffset - (Int32)dexInstrOffset)/2);
                }

                targetOffset = (tCodeSize + targetOffset - (Int32)dexInstrOffset)/2;
                cWriteLE32(ip + 2, targetOffset);
                break;
            }
            case OP_FILL_ARRAY_DATA:
            {
                Int32 targetOffset = (UInt32)cReadLE32(ip + 2);
                targetOffset = (tCodeSize + targetOffset - dexInstr->instrOffset)/2;
                cWriteLE32(ip + 2, targetOffset);
            }
                break;
        }

        ip += dexInstr->instrSize;
    }

    return err;
}

static Int32 fixJumpOffset(CBSHandle regIns, CBSHandle dataHandler,
                           D2DdexInstrList const* dexInstrList)
{
    UInt32 instrCount = dexInstrList->instrCount;
    D2DdexInstr* instrList = dexInstrList->instr;
    UInt8 opcode;
    UInt32 i;
    Int32 err = 0;
    UInt32 tCodeSize = 0;

    tCodeSize = cbsGetSize(regIns);
    if(dataHandler != 0){
        BYTE* data = cbsGetData(dataHandler);
        UInt32 dataSize = cbsGetSize(dataHandler);
        /*the switch data should be 4byte aligne*/
        /*Fix: not align here.
        This will add extra false nop instuctions.*/
        alignLbs(regIns);

        tCodeSize = cbsGetSize(regIns);
        err += cbsWrite(regIns, data, dataSize);

        cbsDestroy(dataHandler);
    }
    /*Fix: not align here.
    This will add extra false nop instuctions.*/
    // alignLbs(regIns);

    BYTE* ip = cbsGetData(regIns);
    BYTE* switchBase = ip + tCodeSize;

    //2.fix jump offset
    for(i = 0; i < instrCount; i++){
        D2DdexInstr* dexInstr =  instrList + i;

        switch(ip[0]){
            case OP_IF_EQ:
            case OP_IF_NE:
            case OP_IF_LT:
            case OP_IF_GE:
            case OP_IF_GT:
            case OP_IF_LE:
            case OP_IF_EQZ:
            case OP_IF_NEZ:
            case OP_IF_LTZ:
            case OP_IF_GEZ:
            case OP_IF_GTZ:
            case OP_IF_LEZ:
            {
                UInt16 instrNum = *((UInt16*)(ip + 2));
                UInt32 targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                Int32  targetOffset = (Int32)(targetInstrOffset - dexInstr->instrOffset)/2;
                if(!signedFitsIn16(targetOffset)){
                    ABORT();
                }
                *((Int16*)(ip + 2)) = (Int16)targetOffset;
                break;
            }
            case OP_GOTO_16:
            {
                UInt32 instrNum = cReadLE32(dexInstr->instrData + 2);
                UInt32 targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                Int32  targetOffset = (Int32)(targetInstrOffset - dexInstr->instrOffset)/2;
                if(!signedFitsIn16(targetOffset)){
                    ABORT();
                }
                *((Int16*)(ip + 2)) = (Int16)targetOffset;
                break;
            }
            case OP_GOTO:
            {
                UInt32 instrNum = cReadLE32(dexInstr->instrData + 2);
                UInt32 targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                Int32  targetOffset = (Int32)(targetInstrOffset - dexInstr->instrOffset)/2;
                if(!signedFitsIn8(targetOffset)){
                    abort();
                }
                *(ip + 1) = (Int8)targetOffset;
                break;
            }
            case OP_GOTO_32:
            {
                UInt32 instrNum = cReadLE32(dexInstr->instrData + 2);
                UInt32 targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                Int32  targetOffset = (Int32)(targetInstrOffset - (Int32)dexInstr->instrOffset)/2;
                if(targetOffset == 0)
                    abort();
                cWriteLE32(ip + 2, targetOffset);
                break;
            }
            case OP_PACKED_SWITCH:
            {
                UInt32 j;
                Int32 targetOffset;
                Int32* target;
                Int32 dexInstrOffset;
                UInt32 size;
                UInt16* data;

                targetOffset = (UInt32)cReadLE32(ip + 2);
                data = (UInt16*)(switchBase + targetOffset);
                size =  data[1];
                target = (Int32*)(((BYTE*)data) + 8);

                dexInstrOffset = dexInstr->instrOffset;
                for(j = 0; j < size; j++){
                    Int32 targetInstrOffset;
                    UInt32 instrNum;

                    instrNum = (UInt32)target[j];

                    targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                    target[j] = ((Int32)(targetInstrOffset - (Int32)dexInstrOffset))/2;
                }

                targetOffset = (tCodeSize + targetOffset - (Int32)dexInstrOffset)/2;
                cWriteLE32(ip + 2, targetOffset);
                break;
            }
            case OP_SPARSE_SWITCH:
            {
                UInt32 j;
                Int32 targetOffset;
                Int32 dexInstrOffset;
                Int32* target;
                UInt32 size;
                UInt16* data;

                targetOffset = (UInt32)cReadLE32(ip + 2);
                data = (UInt16*)(switchBase + targetOffset);
                size =  data[1];
                target = (Int32*)(((BYTE*)data) + 4 + 4*size);
                dexInstrOffset = dexInstr->instrOffset;

                for(j = 0; j < size; j++){
                    Int32 targetInstrOffset;
                    UInt32 instrNum;

                    instrNum = (UInt32)target[j];
                    targetInstrOffset = getDexOffset(dexInstrList, instrNum);
                    target[j] = (Int32)((targetInstrOffset - (Int32)dexInstrOffset)/2);
                }

                targetOffset = (tCodeSize + targetOffset - (Int32)dexInstrOffset)/2;
                cWriteLE32(ip + 2, targetOffset);
                break;
            }
            case OP_FILL_ARRAY_DATA:
            {
                Int32 targetOffset = (UInt32)cReadLE32(ip + 2);
                targetOffset = (tCodeSize + targetOffset - dexInstr->instrOffset)/2;
                cWriteLE32(ip + 2, targetOffset);
            }
                break;
        }

        ip += dexInstr->instrSize;
    }

    return err;
}

#define CHECK true
void writeSignedLeb128ToCbs(CBSHandle handle, Int32 data)
{
    UInt32 size = signedLeb128Size(data);
    Int8 lebData[size];

    writeSignedLeb128(lebData, data);

#if CHECK
    const UInt8* ptr = (const UInt8*)lebData;
    Int32 tData = readSignedLeb128(&ptr);
    ASSERT0(tData == data);
#endif

    cbsWrite(handle, lebData, size);

    return;
}

Int32 writeUnSignedLeb128ToCbs(CBSHandle handle, UInt32 data)
{
    UInt32 size = unsignedLeb128Size(data);
    UInt8 lebData[size];

    writeUnsignedLeb128(lebData, data);

#if CHECK
    const BYTE* ptr = lebData;
    UInt32 tData = readUnsignedLeb128(&ptr);
    ASSERT0(tData == data);
    ASSERT0(((ULong)ptr - (ULong)lebData) == size);
#endif

    cbsWrite(handle, lebData, size);

    return size;
}
#undef CHECK

static void cbsCopy(CBSHandle dst, CBSHandle src)
{
    UInt32 codeSize = cbsGetSize(src);
    BYTE* codeData = cbsGetData(src);

    cbsWrite(dst, codeData, codeSize);

    return;
}

/* try item
 *
 *  UInt32 start_pc
 *  UInt16 insn_count
 *  UInt16 handler_off
 *
 * -------------------------------
         * |    uleb128 size
         * |    encode_handler------------------------
         * |----------------------------             |
         *                                           |
         *                                 ----------|
         *                                 |
         *                                 V
         * --------------------------------
         *  | sleb128       size
         *  | handlers[ads(size)]            --------------
         *  | uleb128       catch_all_addr                |
         *  ------------------------------                |
         *                                                V
         *                                     ------------------
         *                                     | uleb128 type_idx
         *                                     | uleb128 addr
         *                                     ------------------
         *                                     */
static Int32 fixTryCatches(CBSHandle regIns, LIRCode const* code,
                           D2DdexInstrList const* instrList)
{
    LIROpcodeTry* trys = code->trys;
    UInt32 i,j;
    Int32 err = 0;
    UInt32 numException = 0;
    UInt32 end = 0;
    UInt32 triesSize = code->triesSize;
    UInt32 baseOffset = unsignedLeb128Size(triesSize);
    D2DdexInstr const* dexInstr;

    CBSHandle hTryBuff = cbsInitialize(0);
    CBSHandle catchBuff = cbsInitialize(0);

    for(i = 0; i < triesSize; i++)
    {
        UInt32 tryHandleSize = 0;
        LIROpcodeTry* ltry = trys + i;
        UInt32 start = getDexOffset(instrList, ltry->start_pc);
        UInt32 end_pc = ltry->end_pc;

        if(end_pc == (UInt32)(code->instrCount)) {
            dexInstr = &(instrList->instr[instrList->instrCount - 1]);
            end = (UInt32)(dexInstr->instrOffset);
            tryHandleSize = (end - start) / 2 + 1 ;
        } else {
            end = getDexOffset(instrList, end_pc);
            tryHandleSize = (end - start) / 2;
        }

        ASSERT0(tryHandleSize > 0);
        /*fill the try item*/
        cbsWrite32(hTryBuff, (start/2));
        cbsWrite16(hTryBuff, tryHandleSize);

        UInt32 offset;
        offset = baseOffset + cbsGetSize(catchBuff);

        cbsWrite16(hTryBuff, (UInt16)offset);
        ASSERT0(offset < (1<<16));
        Int32 catchSize = ltry->catchSize;
        bool ifHaveCatchAll = false;

        ASSERT0(catchSize >= 0);
        for(j = 0; j < (UInt32)catchSize; j++)
        {
           LIROpcodeCatch* lcatch = ltry->catches + j;
           UInt32 catch_type = lcatch->catch_type;

           if(0 == catch_type)
               ifHaveCatchAll = true;
        }

        ASSERT0(catchSize >= 0);
        if(ifHaveCatchAll)
            catchSize = -(catchSize - 1 );

        /*start to write the catch handle*/
        writeSignedLeb128ToCbs(catchBuff, catchSize);
        for(j = 0; j < ltry->catchSize; j++)
        {
            LIROpcodeCatch* lcatch = ltry->catches + j;
            UInt32 catch_type = lcatch->catch_type;
            UInt32 handlePc = getDexOffset(instrList, lcatch->handler_pc) / 2;

            if(catch_type == 0)
                writeUnSignedLeb128ToCbs(catchBuff, handlePc);
            else
            {
                writeUnSignedLeb128ToCbs(catchBuff, catch_type);
                writeUnSignedLeb128ToCbs(catchBuff, handlePc);
            }
        }
    }

    if(triesSize)
    {
        writeUnSignedLeb128ToCbs(hTryBuff, triesSize);
        //ASSERT0(!(cbsGetSize(regIns) % 4));
        cbsCopy(regIns, hTryBuff);
        //ASSERT0(!(cbsGetSize(regIns) % 4));
        cbsCopy(regIns, catchBuff);
        //ASSERT0(!(cbsGetSize(regIns) % 4));
    }
    cbsDestroy(hTryBuff);
    cbsDestroy(catchBuff);
    return numException;
}

static Int32 fixTryCatches_orig(D2Dpool* pool, CBSHandle regIns, LIRCode* code, D2DdexInstrList* instrList, DexCode* nCode)
{
    LIROpcodeTry* trys = code->trys;
    UInt32 i,j;
    Int32 err = 0;
    UInt32 numException = 0;
    UInt32 end = 0;
    UInt32 triesSize = code->triesSize;
    UInt32 baseOffset = unsignedLeb128Size(triesSize);
    D2DdexInstr* dexInstr;

    CBSHandle hTryBuff = cbsInitialize(0);
    CBSHandle catchBuff = cbsInitialize(0);

    for(i = 0; i < triesSize; i++)
    {
        UInt32 tryHandleSize = 0;
        LIROpcodeTry* ltry = trys + i;
        UInt32 start = getDexOffset(instrList, ltry->start_pc);

        UInt32 end_pc = ltry->end_pc;

        if(end_pc == (UInt32)(code->instrCount))
        {
            dexInstr = &(instrList->instr[instrList->instrCount - 1]);
            end = (UInt32)(dexInstr->instrOffset + dexInstr->instrSize - 1);
        }
        else
            end = getDexOffset(instrList, end_pc);

        tryHandleSize = (end - start) / 2;

        /*fill the try item*/
        cbsWrite32(hTryBuff, (start/2));
        cbsWrite16(hTryBuff, tryHandleSize);

        UInt32 offset;
        offset = baseOffset + cbsGetSize(catchBuff);

        cbsWrite16(hTryBuff, (UInt16)offset);
        ASSERT0(offset < (1<<16));
        Int32 catchSize = ltry->catchSize;
        bool ifHaveCatchAll = false;

        ASSERT0(catchSize >= 0);
        for(j = 0; j < (UInt32)catchSize; j++)
        {
           LIROpcodeCatch* lcatch = ltry->catches + j;
           UInt32 catch_type = lcatch->catch_type;

           if(0 == catch_type)
               ifHaveCatchAll = true;
        }

        ASSERT0(catchSize >= 0);
        if(ifHaveCatchAll)
            catchSize = -(catchSize - 1 );

        /*start to write the catch handle*/
        writeSignedLeb128ToCbs(catchBuff, catchSize);
        for(j = 0; j < ltry->catchSize; j++)
        {
            LIROpcodeCatch* lcatch = ltry->catches + j;
            UInt32 catch_type = lcatch->catch_type;
            UInt32 handlePc = getDexOffset(instrList, lcatch->handler_pc) / 2;

            if(catch_type == 0)
                writeUnSignedLeb128ToCbs(catchBuff, handlePc);
            else
            {
                writeUnSignedLeb128ToCbs(catchBuff, catch_type);
                writeUnSignedLeb128ToCbs(catchBuff, handlePc);
            }
        }
    }

    if(triesSize)
    {
        writeUnSignedLeb128ToCbs(hTryBuff, triesSize);
        //ASSERT0(!(cbsGetSize(regIns) % 4));
        cbsCopy(regIns, hTryBuff);
        //ASSERT0(!(cbsGetSize(regIns) % 4));
        cbsCopy(regIns, catchBuff);
        //ASSERT0(!(cbsGetSize(regIns) % 4));
    }

    nCode->triesSize = triesSize;

    cbsDestroy(hTryBuff);
    cbsDestroy(catchBuff);

    return numException;
}

Int32 transformCode_orig(D2Dpool* pool, LIRCode* code, DexCode* nCode)
{
   ULong* lirList = (ULong*)code->lirList;
   Int32 instrCount = code->instrCount;
   Int32 i;
   Int32 err = 0;
   Int32 numGoto = 0;
   UInt32 codeLength = 0;
   CBSHandle dataHandler = 0;

   CBSHandle regIns = cbsInitialize(0);
   D2DdexInstrList dexInstrList;
   UInt32 instrOffset = 0;

   dexInstrList.instrCount = instrCount;

   dexInstrList.instr = (D2DdexInstr*)malloc(sizeof(D2DdexInstr) * instrCount);
   ::memset(dexInstrList.instr, 0, sizeof(D2DdexInstr) * instrCount);

   for(i = 0; i < instrCount; i++)
   {
       D2DdexInstr* dexInstr = dexInstrList.instr + i;
       LIRBaseOp* codePtr = (LIRBaseOp*)lirList[i];

       UInt8 lirOpCode = getlirOpcode((ULong)codePtr);
       UInt32 formats = gLIROpcodeInfo.formats[lirOpCode];

       switch(formats){
            case lirFmtV:
            {
                LIRBaseOp* lir =  codePtr;
                UInt8 dexOpCode= OP_NOP;
                WRITE_FORMATAA(dexInstr, 0)
                break;
            }
            case lirFmtA:
            case lirFmtR:
            {
                LIRAOp* lir = (LIRAOp*)codePtr;
                err += processLIRAOp(dexInstr, lir);
                break;
            }
            case lirFmtT:
            {
                /*TODO the goto's target is not ok!! should be processed next*/
                LIRGOTOOp* lir = (LIRGOTOOp*)codePtr;
                err += processLIRGOTOOp(dexInstr,lir);
                numGoto ++;
                break;
            }
            case lirFmtRA:
            {
                LIRABOp* lir = (LIRABOp*)codePtr;
                err += processLIRABOp(dexInstr,lir);
                break;
            }
            case lirFmtRS:
            case lirFmtAS:
            {
                LIRABOp* lir = (LIRABOp*)codePtr;
                err += processLIRABOp(dexInstr,lir);
                break;
            }
            //ifz
            case lirFmtAT:
            {
                LIRABOp* lir = (LIRABOp*)codePtr;
                err += processJumpLIRABOp(dexInstr,lir);
                break;
            }
            //const
            case lirFmtRL:
            {
                LIRConstOp* lir = (LIRConstOp*)codePtr;
                err += processLIRConstOp(dexInstr,lir);
                break;
            }
            case lirFmtABC:
            case lirFmtRAB:
            case lirFmtRAL:
            {
                LIRABCOp* lir = (LIRABCOp*)codePtr;
                err += processLIRABCOp(dexInstr,lir);
                break;
            }
            case lirFmtABS:
            case lirFmtRAS:
            {
                LIRABCOp* lir = (LIRABCOp*)codePtr;
                err += processLIRABCOp(dexInstr,lir);
                break;
            }
            case lirFmtABT:
            {
                LIRABCOp* lir = (LIRABCOp*)codePtr;
                err += processJumpLIRABCOp(dexInstr,lir);
                break;
            }
            case lirFmtSWITCH:
            {
                LIRSwitchOp* lir = (LIRSwitchOp*)codePtr;
                if(dataHandler == 0){
                    dataHandler =  cbsInitialize(0);
                }
                err += processLIRSwitchOp(dexInstr,dataHandler,lir);
                break;
            }
            case lirFmtINVOKE:
            {
                LIRInvokeOp* lir = (LIRInvokeOp*)codePtr;
                err += processLIRInvokeOp(dexInstr, lir);
                break;
            }
           default:
            {
                ASSERT0(false);
                break;
            }
      }
        dexInstr->instrOffset = instrOffset;
        instrOffset += dexInstr->instrSize;
    }

   if(numGoto != 0)
       genGOTO(&dexInstrList, numGoto);

   err += writeByteCodeAndFixOffset(regIns, &dexInstrList);
   err += fixJumpOffset_orig(pool, regIns, dataHandler, &dexInstrList, nCode);
   err += fixTryCatches_orig(pool, regIns, code, &dexInstrList, nCode);

   nCode->insSize = code->numArgs;
   nCode->registersSize = code->maxVars;

   free(dexInstrList.instr);

   return regIns;
}

Int32 transformCode(LIRCode const* code, DexCode* nCode)
{
   ULong* lirList = (ULong*)code->lirList;
   Int32 instrCount = code->instrCount;
   Int32 i;
   Int32 err = 0;
   Int32 numGoto = 0;
   UInt32 codeLength = 0;
   CBSHandle dataHandler = 0;

   CBSHandle regIns = cbsInitialize(0);
   D2DdexInstrList dexInstrList;
   UInt32 instrOffset = 0;

   dexInstrList.instrCount = instrCount;

   dexInstrList.instr = (D2DdexInstr*)malloc(sizeof(D2DdexInstr) * instrCount);
   ::memset(dexInstrList.instr, 0, sizeof(D2DdexInstr) * instrCount);

   for(i = 0; i < instrCount; i++)
   {
       D2DdexInstr* dexInstr = dexInstrList.instr + i;
       LIRBaseOp* codePtr = (LIRBaseOp*)lirList[i];

       UInt8 lirOpCode = getlirOpcode((ULong)codePtr);
       UInt32 formats = gLIROpcodeInfo.formats[lirOpCode];

       switch(formats){
            case lirFmtV:
            {
                LIRBaseOp* lir =  codePtr;
                UInt8 dexOpCode= OP_NOP;
                WRITE_FORMATAA(dexInstr, 0)
                break;
            }
            case lirFmtA:
            case lirFmtR:
            {
                LIRAOp* lir = (LIRAOp*)codePtr;
                err += processLIRAOp(dexInstr, lir);
                break;
            }
            case lirFmtT:
            {
                /*TODO the goto's target is not ok!! should be processed next*/
                LIRGOTOOp* lir = (LIRGOTOOp*)codePtr;
                err += processLIRGOTOOp(dexInstr,lir);
                numGoto ++;
                break;
            }
            case lirFmtRA:
            {
                LIRABOp* lir = (LIRABOp*)codePtr;
                err += processLIRABOp(dexInstr,lir);
                break;
            }
            case lirFmtRS:
            case lirFmtAS:
            {
                LIRABOp* lir = (LIRABOp*)codePtr;
                err += processLIRABOp(dexInstr,lir);
                break;
            }
            //ifz
            case lirFmtAT:
            {
                LIRABOp* lir = (LIRABOp*)codePtr;
                err += processJumpLIRABOp(dexInstr,lir);
                break;
            }
            //const
            case lirFmtRL:
            {
                LIRConstOp* lir = (LIRConstOp*)codePtr;
                err += processLIRConstOp(dexInstr,lir);
                break;
            }
            case lirFmtABC:
            case lirFmtRAB:
            case lirFmtRAL:
            {
                LIRABCOp* lir = (LIRABCOp*)codePtr;
                err += processLIRABCOp(dexInstr,lir);
                break;
            }
            case lirFmtABS:
            case lirFmtRAS:
            {
                LIRABCOp* lir = (LIRABCOp*)codePtr;
                err += processLIRABCOp(dexInstr,lir);
                break;
            }
            case lirFmtABT:
            {
                LIRABCOp* lir = (LIRABCOp*)codePtr;
                err += processJumpLIRABCOp(dexInstr,lir);
                break;
            }
            case lirFmtSWITCH:
            {
                LIRSwitchOp* lir = (LIRSwitchOp*)codePtr;
                if(dataHandler == 0){
                    dataHandler =  cbsInitialize(0);
                }
                err += processLIRSwitchOp(dexInstr,dataHandler,lir);
                break;
            }
            case lirFmtINVOKE:
            {
                LIRInvokeOp* lir = (LIRInvokeOp*)codePtr;
                err += processLIRInvokeOp(dexInstr, lir);
                break;
            }
           default:
            {
                ASSERT0(false);
                break;
            }
      }
        dexInstr->instrOffset = instrOffset;
        instrOffset += dexInstr->instrSize;
    }

   if(numGoto != 0)
       genGOTO(&dexInstrList, numGoto);

   err += writeByteCodeAndFixOffset(regIns, &dexInstrList);

   //Function will modify cbsGetSize(regIns).
   err += fixJumpOffset(regIns, dataHandler, &dexInstrList);
   nCode->insnsSize = cbsGetSize(regIns) / 2;

   //Try/Catch buf will be added.
   //Fix: Trycatched should be 4-byte aligned.
   //And then the code_item will also be 4-byte aligned.
   //whenever tries size is zero or not.
   if (nCode->insnsSize & 0x01){
     cbsWrite16(regIns, (UInt16)0);
   }
   err += fixTryCatches(regIns, code, &dexInstrList);
   nCode->triesSize = code->triesSize;
   nCode->insSize = code->numArgs;
   nCode->registersSize = code->maxVars;

   free(dexInstrList.instr);
   return regIns;
}

void alignLbs(CBSHandle lbs)
{
    BYTE* data = cbsGetData(lbs);
    data += cbsGetSize(lbs);

    while(((ULong)data & 3) != 0)
    {
        cbsWrite8(lbs, 0);
        data++;
    }

    return;
}

void aligmentBy4Bytes(D2Dpool* pool)
{
    BYTE*  data = (BYTE*)cbsGetData(pool->lbs) + pool->currentSize;
    BYTE* nData = data;
    nData = (BYTE*)(((ULong)nData + 3) & (~3));

    UInt32 size = nData - data;
    UInt32 i;

    for(i = 0; i < size; i++)
    {
        cbsWrite8(pool->lbs, 0);
        pool->currentSize++;
    }

    data = cbsGetData(pool->lbs);
    data += pool->currentSize;
    ASSERT0(((ULong)data & 3) == 0);

    return;
}

static Int32 writeCodeItem_orig(D2Dpool* pool, CBSHandle cbsCode, DexCode* nCode)
{
    CBSHandle lbs = pool->lbs;
    Int32 writeSize = 0;

    /*local to the end of the buff*/
    cbsSeek(lbs, pool->currentSize);
    aligmentBy4Bytes(pool);

    Int32 prevSize = cbsGetSize(lbs);
    /*write maxVars*/
    cbsWrite16(lbs, nCode->registersSize);
    /*num args*/
    cbsWrite16(lbs, nCode->insSize);
    /*num of out args*/
    cbsWrite16(lbs, nCode->outsSize);
    /*write try size*/
    cbsWrite16(lbs, nCode->triesSize);
    /*debug offSet, put 0 first, it will be fixed when we fix all the offsets*/
    cbsWrite32(lbs, nCode->debugInfoOff);
    /*write code length*/
    cbsWrite32(lbs, nCode->insnsSize);
    /*write code array,
     * tries,
     * handles*/
    cbsCopy(lbs, cbsCode);
    writeSize = cbsGetSize(lbs) - prevSize;

    DexCode* pCode = (DexCode*)((BYTE*)cbsGetData(lbs) + cbsGetSize(lbs) - writeSize);

    ASSERT0((UInt32)(writeSize) == dexGetDexCodeSize(pCode));

    /*to record the current code off*/
    pool->codeOff = pool->currentSize;
    pool->currentSize += writeSize;
    pool->codeItemSize++;
    /*to destory the code buff*/
    cbsDestroy(cbsCode);
    ASSERT0(pool->currentSize == cbsGetSize(pool->lbs));
    return writeSize;
}

DexCode * writeCodeItem(D2Dpool* pool,
                        CBSHandle cbsCode,
                        UInt16 registersSize,
                        UInt16 insSize,
                        UInt16 outsSize,
                        UInt16 triesSize,
                        UInt32 debugInfoOff,
                        UInt32 insnsSize)
{
    CBSHandle lbs = pool->lbs;
    Int32 writeSize = 0;

    /*local to the end of the buff*/
    cbsSeek(lbs, pool->currentSize);
    aligmentBy4Bytes(pool);

    Int32 prevSize = cbsGetSize(lbs);
    /*write maxVars*/
    cbsWrite16(lbs, registersSize);
    /*num args*/
    cbsWrite16(lbs, insSize);
    /*num of out args*/
    cbsWrite16(lbs, outsSize);
    /*write try size*/
    cbsWrite16(lbs, triesSize);
    /*debug offSet, put 0 first, it will be fixed when we fix all the offsets*/
    cbsWrite32(lbs, debugInfoOff);
    /*write code length*/
    cbsWrite32(lbs, insnsSize);
    /*write code array,
     * tries,
     * handles*/
    cbsCopy(lbs, cbsCode);
    writeSize = cbsGetSize(lbs) - prevSize;

    DexCode* pCode = (DexCode*)((BYTE*)cbsGetData(lbs) + cbsGetSize(lbs) - writeSize);

    ASSERT0((UInt32)(writeSize) == dexGetDexCodeSize(pCode));

    /*to record the current code off*/
    pool->codeOff = pool->currentSize;
    pool->currentSize += writeSize;
    pool->codeItemSize++;
    /*to destory the code buff*/
    cbsDestroy(cbsCode);
    ASSERT0(pool->currentSize == cbsGetSize(pool->lbs));
    return pCode;
}

void lir2dexCode_orig(D2Dpool* pool, const DexCode* pCode, LIRCode* code)
{
    DexCode npCode;
    ::memset(&npCode, 0, sizeof(DexCode));

    CBSHandle cbsCode = transformCode_orig(pool, code, &npCode);
    npCode.debugInfoOff = pCode->debugInfoOff;
    npCode.outsSize = pCode->outsSize;

    writeCodeItem_orig(pool, cbsCode, &npCode);

    return;
}

void lir2dexCode(D2Dpool* pool, const DexCode* dexCode, LIRCode* lircode)
{
    DexCode x;
    ::memset(&x, 0, sizeof(DexCode));
    CBSHandle cbsCode = transformCode(lircode, &x);
    writeCodeItem(pool, cbsCode, x.registersSize, x.insSize,
                  dexCode->outsSize, x.triesSize,
                  dexCode->debugInfoOff, x.insnsSize);
    return;
}

