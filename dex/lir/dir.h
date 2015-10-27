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
#ifndef DEX_READER_DIR
#define DEX_READER_DIR

#include "std/cstd.h"

typedef struct DIRDecodedInsn{
    UInt32 vA;
    ULong vB;
    UInt64 vB_wide;
    UInt32 vC;
    UInt32 arg[5];
    UInt16 opcode;
}DIRDecodedInsn;

typedef UInt8 DIROpcodeFlagsBits;

enum _DIROpcodeFlagsBits {
    dInstrCanBranch     = 1,        // conditional or unconditional branch
    dInstrCanContinue   = 1 << 1,   // flow can continue to next statement
    dInstrCanSwitch     = 1 << 2,   // switch statement
    dInstrCanThrow      = 1 << 3,   // could cause an exception to be thrown
    dInstrCanReturn     = 1 << 4,   // returns, no additional statements
    dInstrInvoke        = 1 << 5,   // a flavor of invoke
};

typedef UInt8 DIRInstructionWidth;

typedef struct DIRInstructionInfoTables {
    UInt8*                formats;    /* InstructionFormat elements */
    DIROpcodeFlagsBits*   flags;
    DIRInstructionWidth*  widths;
}DIRInstructionInfoTables;

/*
 * Global InstructionInfoTables struct.
 */
extern DIRInstructionInfoTables gDIROpcodeInfo;

typedef UInt8 DIRInstructionFormat;

enum _DIRInstructionFormat {
    dFmt00x = 0,    // unknown format (also used for "breakpoint" opcode)
    dFmt10x,        // op
    dFmt12x,        // op vA, vB
    dFmt11n,        // op vA, #+B
    dFmt11x,        // op vAA
    dFmt10t,        // op +AA
    dFmt20bc,       // [opt] op AA, thing@BBBB
    dFmt20t,        // op +AAAA
    dFmt22x,        // op vAA, vBBBB
    dFmt21t,        // op vAA, +BBBB
    dFmt21s,        // op vAA, #+BBBB
    dFmt21h,        // op vAA, #+BBBB00000[00000000]
    dFmt21c,        // op vAA, thing@BBBB
    dFmt23x,        // op vAA, vBB, vCC
    dFmt22b,        // op vAA, vBB, #+CC
    dFmt22t,        // op vA, vB, +CCCC
    dFmt22s,        // op vA, vB, #+CCCC
    dFmt22c,        // op vA, vB, thing@CCCC
    dFmt22cs,       // [opt] op vA, vB, field offset CCCC
    dFmt30t,        // op +AAAAAAAA
    dFmt32x,        // op vAAAA, vBBBB
    dFmt31i,        // op vAA, #+BBBBBBBB
    dFmt31t,        // op vAA, +BBBBBBBB
    dFmt31c,        // op vAA, string@BBBBBBBB
    dFmt35c,        // op {vC,vD,vE,vF,vG}, thing@BBBB
    dFmt35ms,       // [opt] invoke-virtual+super
    dFmt3rc,        // op {vCCCC .. v(CCCC+AA-1)}, thing@BBBB
    dFmt3rms,       // [opt] invoke-virtual+super/range
    dFmt51l,        // op vAA, #+BBBBBBBBBBBBBBBB
    dFmt35mi,       // [opt] inline invoke
    dFmt3rmi,       // [opt] inline invoke/range
    dFmt33x,        // exop vAA, vBB, vCCCC
    dFmt32s,        // exop vAA, vBB, #+CCCC
    dFmt40sc,       // [opt] exop AAAA, thing@BBBBBBBB
    dFmt41c,        // exop vAAAA, thing@BBBBBBBB
    dFmt52c,        // exop vAAAA, vBBBB, thing@CCCCCCCC
    dFmt5rc,        // exop {vCCCC .. v(CCCC+AAAA-1)}, thing@BBBBBBBB
};

typedef UInt16 DIROpcode;

void DIRDecodeInstruction(const UInt16* insns, DIRDecodedInsn* pDec);

typedef struct DIR2LIRInfoTables{
    UInt8* opcodes;
    UInt8* flags;
}DIR2LIRInfoTables;

extern DIR2LIRInfoTables gDIR2LIRInfo;

#endif
