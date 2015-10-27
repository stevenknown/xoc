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
#ifndef LIR_OPCODE_H
#define LIR_OPCODE_H

#include "std/cstd.h"

//begin typedef
//defined in <lircontext.h>

enum LIROperandStyle {
    LIR_OS_U, //U: unknown
    LIR_OS_R, //R: result
    LIR_OS_A, //A: inputA
    LIR_OS_B, //B: inputB
    LIR_OS_C, //C: inputC
    LIR_OS_L, //L: literal
    LIR_OS_T, //T: jump target
    LIR_OS_S, //S: symbols, ref
};

//

enum LIR_Condition_Kind{
    LIR_cond_unknown = 0,
    LIR_cond_EQ = 1,
    LIR_cond_LT = 2,
    LIR_cond_GT = 3,
    LIR_cond_LE = 4,
    LIR_cond_GE = 5,
    LIR_cond_NE = 6,
    LIR_cond_reverse_mark,
};

enum LIR_Invoke_Kind{
    LIR_invoke_unknown = 0,
    LIR_invoke_virtual = 1,
    LIR_invoke_direct = 2,
    LIR_invoke_super = 3,
    LIR_invoke_interface = 4,
    LIR_invoke_static = 5,

    LIR_invoke_inline = 1 << 4,
    LIR_Range = 1 << 7,
};

enum LIR_Switch_Kind{
    LIR_switch_unknown = 0,
    LIR_switch_table = 1,
    LIR_switch_lookup = 2,
};

/*
 *  one letter means 4 bit
 */
enum LIR_Move_Kind{
    LIR_mov_unknown = 0,
    LIR_mov_RA,//4
    LIR_mov_RRAAAA,//f16
    LIR_mov_00RRRRAAAA,//32
};

//for const and const_string
enum LIR_Const_Kind{
    LIR_const_unknown = 0,
    LIR_const_RL,//4
    LIR_const_RRLLLL,//16
    LIR_const_RRLLLLLLLL,//32
    LIR_const_hight_RRLLLL,//h16
    LIR_const_RRLLLLLLLLLLLLLLLL,//64
};

enum LIR_Goto_Kind{
    LIR_goto_unknown = 0,
    LIR_goto_TT,//8
    LIR_goto_TTTT,//16
    LIR_goto_TTTTTTTT,//32
};

enum LIR_CMP_Kind{
    LIR_CMP_float,
    LIR_CMP_double,
};

//java data type
//array filed static field
enum LIR_JDT_Kind{
    LIR_JDT_unknown = 0,
    LIR_JDT_void = 1,
    LIR_JDT_int = 2,
    LIR_JDT_object = 3,
    LIR_JDT_boolean = 4,
    LIR_JDT_byte = 5,
    LIR_JDT_char = 6,
    LIR_JDT_short = 7,
    LIR_JDT_float = 8,
    LIR_JDT_none = 9,

    LIR_wide = 15,
    LIR_JDT_wide = 16,
    LIR_JDT_long = 17,
    LIR_JDT_double = 18,

    //LIR_JDT_high,
    //LIR_JDT_high_w,

};


enum LIR_Convert_Kind{
    LIR_convert_unknown = 0,
    LIR_convert_i2f = 1,
    LIR_convert_l2i = 2,
    LIR_convert_l2f = 3,
    LIR_convert_f2i = 4,
    LIR_convert_d2i = 5,
    LIR_convert_d2f = 6,
    LIR_convert_i2b = 7,
    LIR_convert_i2c = 8,
    LIR_convert_i2s = 9,

    //LIR_wide = 15,
    LIR_convert_i2l = 16,
    LIR_convert_f2l = 17,
    LIR_convert_d2l = 18,
    LIR_convert_i2d = 19,
    LIR_convert_f2d = 20,
    LIR_convert_l2d = 21,
};


enum LIR_OPCODE_Type{
    LIR_unknown = 0,
    LIR_VOID,
    LIR_RAW,
    LIR_WIDE,
    LIR_OBJECT,

    LIR_LIT8,
    LIR_LIT16,
    LIR_2ADDR,
    LIR_REG,
};

typedef UInt8 LIROpcode;

enum _LIROpcode{
    LOP_NOP                    = 0,

LOP_begin1                       = 1,
//----------------------------one
    //A
    LOP_RETURN                 = 2,//return
    LOP_THROW                   = 3,//exceptionObject = a
    LOP_MONITOR_ENTER          = 4,//AA lock
    LOP_MONITOR_EXIT           = 5,//AA unlock

    //R
    LOP_MOVE_RESULT            = 6, //result =  retvalue
    LOP_MOVE_EXCEPTION         = 7, //result =  exceptionObject

    //T
    LOP_GOTO                   = 8,//goto target

LOP_begin2                       = 9,
//----------------------------two
    //RA
    LOP_MOVE                   = 10, //result =  inputA
    LOP_NEG                       = 11,//AB VA = -VB
    LOP_NOT                       = 12,//AB VA = VB^xffffffffffffffffULL
       LOP_CONVERT                   = 13,//AB VA = convert(VB)

    LOP_ADD_ASSIGN             = 14,//2addr result += b
    LOP_SUB_ASSIGN             = 15,
    LOP_MUL_ASSIGN             = 16,
    LOP_DIV_ASSIGN             = 17,
    LOP_REM_ASSIGN             = 18,
    LOP_AND_ASSIGN             = 19,
    LOP_OR_ASSIGN              = 20,
    LOP_XOR_ASSIGN             = 21,
    LOP_SHL_ASSIGN             = 22,
    LOP_SHR_ASSIGN             = 23,
    LOP_USHR_ASSIGN            = 24,

    LOP_ARRAY_LENGTH           = 25,//AB  result = arr(B) length

    //RL
    LOP_CONST                   = 26,//result  = #L

    //AT
    LOP_IFZ                    = 27,//AABBBB if A<0 continue ,else goto BBBB

    //RS
    LOP_NEW_INSTANCE           = 28,//AABBBB dst(VA) = new ref(BBBB) class object
    LOP_CONST_STRING           = 29,//result  = #string object
    LOP_CONST_CLASS            = 30,//result  = #class object
    LOP_SGET                   = 31,//AACCCC

    //AS
    LOP_CHECK_CAST             = 32,//AABBBB if obj(AA) can cast ref(BBBB) continue ,else throw exception
    LOP_SPUT                   = 33,

//----------------------------three
LOP_begin3                       = 34,
    //ABC
    LOP_APUT                   = 35,

    //RAB
    LOP_AGET                   = 36,//AABBCC VAA = object(BB) idx(CC)
    LOP_CMPL                    = 37,//AABBCC AA = BB < CC
    LOP_CMP_LONG               = 38,
    LOP_ADD                       = 39,//reg lit8 AABBCC  lit16 ABCCCC 2addr AB a = b + c
    LOP_SUB                    = 40,
    LOP_MUL                    = 41,
    LOP_DIV                    = 42,
    LOP_REM                    = 43,
    LOP_AND                       = 44,
    LOP_OR                     = 45,
    LOP_XOR                    = 46,
    LOP_SHL                    = 47,
    LOP_SHR                    = 48,
    LOP_USHR                   = 49,

    //ABT
    LOP_IF                     = 50,//ABBBBB if A<B continue ,else goto BBBB

    //RAL
    LOP_ADD_LIT                = 51,//lit8 AABBCC  lit16 result = a + #l
    LOP_SUB_LIT                = 52,
    LOP_MUL_LIT                = 53,
    LOP_DIV_LIT                = 54,
    LOP_REM_LIT                = 55,
    LOP_AND_LIT                = 56,
    LOP_OR_LIT                 = 57,
    LOP_XOR_LIT                = 58,
    LOP_SHL_LIT                = 59,
    LOP_SHR_LIT                = 60,
    LOP_USHR_LIT               = 61,

    //ABS
    LOP_IPUT                   = 62,

    //RAS
    LOP_IGET                   = 63,//ABCCCC VA = object(VBB) ref(CCCC)
    LOP_INSTANCE_OF            = 64,//RABBBB if obj(AA) instanceof ref(BBBB), dst = 1 ,else 0
    LOP_NEW_ARRAY              = 65,//ABCCCC dst(VA) = new size(VB) ref(CCCC) array

LOP_beginSwitch                   = 66,
    //switch
    LOP_TABLE_SWITCH           = 67,//AABBBBBBBB tableswitch
    LOP_LOOKUP_SWITCH          = 68,//AABBBBBBBB lookupswitch
    LOP_FILL_ARRAY_DATA        = 69,//AABBBBBBBB  arr(AA) offset(BBBBBBBB)

LOP_beginInvoke                   = 70,
    //invoke
    LOP_INVOKE                 = 71,//ABCCCCDEFG argc(B)  method(CCCC) args(vD, vE, vF, vG, vA)
                                       //AABBBBCCCC argc(AA)  method(BBBB) args(CCCC + i)
    LOP_FILLED_NEW_ARRAY       = 72,//ABCCCCDEFG retval = create array obj(A),put count(B) DEFG in it, ref(CCCC),AABBBBCCCC
    LOP_CMPG                       = 73,
    LOP_PHI                        = 74,
    LOP_PACKED_SWITCH_PAYLOAD = 75,
    LOP_SPARSE_SWITCH_PAYLOAD = 76,
};

typedef UInt8 LIROpkind;

typedef UInt8 LIROptype;

typedef struct LIRBaseOpStruct {
    LIROpcode opcode;
    UInt8 flags;
} LIRBaseOp;


//LOP_FILLED_NEW_ARRAY
typedef struct LIRInvokeOpStruct {
    LIROpcode opcode;
    UInt8 flags;
    UInt16     argc;

    union {
    UInt32 ref;
    //LIRMethod*
    void* method;
    };

    UInt32 exeRef;
    const char* shorty;
    UInt16* args;
}LIRInvokeOp;

//LOP_TABLE_SWITCH LOP_LOOKUP_SWITCH LOP_FILL_ARRAY_DATA
typedef struct LIRSwitchOpStruct{
    LIROpcode opcode;
    UInt8     flags;
    UInt16     value;
    UInt16* data;
}LIRSwitchOp;

typedef struct LIRAOpStruct{
    LIROpcode opcode;
    UInt8     flags;
    UInt16  vA;
}LIRAOp;

typedef struct LIRGOTOOpStruct{
    LIROpcode opcode;
    UInt8     flags;
    UInt16     reserved;
    union{
        UInt32     target;
        void*   targetLir;
    };
}LIRGOTOOp;

typedef struct LIRABOpStruct{
    LIROpcode opcode;
    UInt8     flags;
    UInt16     vA;
    union{
        ULong     vB;
        void*   ptr;
    };
}LIRABOp;

typedef struct LIRConstOpStruct{
    LIROpcode opcode;
    UInt8     flags;
    UInt16     vA;
    UInt64     vB;
}LIRConstOp;

typedef struct LIRABCOpStruct{
    LIROpcode opcode;
    UInt8     flags;
    UInt16 vA;
    UInt32 vB;
    union{
        ULong     vC;
        void*   ptr;
    };
}LIRABCOp;

typedef UInt8 opcodeFlags;

typedef UInt8 opcodeFormats;

enum _opcodeFlags{
    lirReturn = 1,

    lirThrow =    1 << 1,
    lirBranch =   1 << 2,
    lirSwitch =   1 << 3,
    lirInvoke =   1 << 4,
    lirContinue = 1 << 5,
};

enum _opcodeFormats{
    lirFmtV = 0,
    lirFmtA = 1,
    lirFmtB = 1 << 1,
    lirFmtC = 1 << 2,
    lirFmtR = 1 << 3,
    lirFmtT = 1 << 4,
    lirFmtS = 1 << 5,
    lirFmtL = 1 << 6,
    lirFmtSWITCH = 1 << 7,
    lirFmtPhi = 1 << 8,
    lirFmtINVOKE = lirFmtSWITCH + 1,
    lirFmtRA = lirFmtR|lirFmtA,
    lirFmtRL = lirFmtR|lirFmtL,
    lirFmtAT = lirFmtA|lirFmtT,
    lirFmtRS = lirFmtR|lirFmtS,
    lirFmtAS = lirFmtA|lirFmtS,
    lirFmtABC = lirFmtA|lirFmtB|lirFmtC,
    lirFmtRAB = lirFmtR|lirFmtA|lirFmtB,
    lirFmtABT = lirFmtA|lirFmtB|lirFmtT,
    lirFmtRAL = lirFmtR|lirFmtA|lirFmtL,
    lirFmtABS = lirFmtA|lirFmtB|lirFmtS,
    lirFmtRAS = lirFmtR|lirFmtA|lirFmtS,
    lirFmt00X = 3,  // For pseudo code format.
};

typedef struct LIROpcodeCatchStruct{
    UInt32 handler_pc;
    union{
        UInt32 catch_type;
        UInt32 class_type;
        //LIRClass* class_type;
    };
}LIROpcodeCatch;

typedef struct LIROpcodeTryStruct{
    UInt32 start_pc;
    UInt32 end_pc;
    UInt32 catchSize;
    LIROpcodeCatch* catches;
}LIROpcodeTry;



typedef struct LIROpcodeInfoTablesStruct{
    UInt8* flags;
    UInt8* formats;
    const char** opNames;
    const char** opFlagNames;
}LIROpcodeInfoTables;

extern LIROpcodeInfoTables gLIROpcodeInfo;

enum _LIROpcodeFlags{
    LIR_FLAGS_ISSTATIC = 1,
    LIR_FLAGS_HASGOTO = 1 << 1,
    LIR_FLAGS_HASINVOKE = 1 << 2,
    LIR_FLAGS_ISEMPTY = 1 << 3,
    LIR_FLAGS_ONEINSTRUCTION = 1 << 4,
};

typedef struct LIRCodeStruct{
    int instrCount;
    LIRBaseOp** lirList;
    UInt32  flags;
    UInt32  triesSize;
    LIROpcodeTry* trys;
    UInt32 maxVars;
    const char* strClass;
    const char* strName;
    const char* shortName;
    UInt32 numArgs;
}LIRCode;

struct LIRCodeItemPtrStruct{
    LIRCode* ptr;
};

#endif
