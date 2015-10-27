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
#include "dir.h"
#include <assert.h>
#include "trace/ctrace.h"

#define dMaxOpcodeValue 0xffff
#define dNumPackedOpcodes 0x200

static DIRInstructionWidth gDIRInstructionWidthTable[dNumPackedOpcodes] = {
    // BEGIN(libdex-widths); GENERATED AUTOMATICALLY BY opcode-gen
    1, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 2, 3, 2, 2, 3, 5, 2, 2, 3, 2, 1, 1, 2,
    2, 1, 2, 2, 3, 3, 3, 1, 1, 2, 3, 3, 3, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0,
    0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3,
    3, 3, 3, 0, 3, 3, 3, 3, 3, 0, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 3, 3,
    3, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 2, 2, 2, 0,
    4, 4, 5, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 5, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4,
    // END(libdex-widths)
};

/*
 * Table that maps each opcode to the flags associated with that
 * opcode.
 */
static UInt8 gDIROpcodeFlagsTable[dNumPackedOpcodes] = {
    // BEGIN(libdex-flags); GENERATED AUTOMATICALLY BY opcode-gen
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanReturn,
    dInstrCanReturn,
    dInstrCanReturn,
    dInstrCanReturn,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue,
    dInstrCanThrow,
    dInstrCanBranch,
    dInstrCanBranch,
    dInstrCanBranch,
    dInstrCanContinue|dInstrCanSwitch,
    dInstrCanContinue|dInstrCanSwitch,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    dInstrCanContinue|dInstrCanBranch,
    0,
    0,
    0,
    0,
    0,
    0,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    0,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    0,
    0,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    0,
    dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanReturn,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    0,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    dInstrCanContinue|dInstrCanThrow|dInstrInvoke,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanContinue|dInstrCanThrow,
    dInstrCanThrow,
    // END(libdex-flags)
};

/*
 * Table that maps each opcode to the instruction format associated
 * that opcode.
 */
static UInt8 gDIRInstructionFormatTable[dNumPackedOpcodes] = {
    // BEGIN(libdex-formats); GENERATED AUTOMATICALLY BY opcode-gen
    dFmt10x,  dFmt12x,  dFmt22x,  dFmt32x,  dFmt12x,  dFmt22x,  dFmt32x,
    dFmt12x,  dFmt22x,  dFmt32x,  dFmt11x,  dFmt11x,  dFmt11x,  dFmt11x,
    dFmt10x,  dFmt11x,  dFmt11x,  dFmt11x,  dFmt11n,  dFmt21s,  dFmt31i,
    dFmt21h,  dFmt21s,  dFmt31i,  dFmt51l,  dFmt21h,  dFmt21c,  dFmt31c,
    dFmt21c,  dFmt11x,  dFmt11x,  dFmt21c,  dFmt22c,  dFmt12x,  dFmt21c,
    dFmt22c,  dFmt35c,  dFmt3rc,  dFmt31t,  dFmt11x,  dFmt10t,  dFmt20t,
    dFmt30t,  dFmt31t,  dFmt31t,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,
    dFmt23x,  dFmt22t,  dFmt22t,  dFmt22t,  dFmt22t,  dFmt22t,  dFmt22t,
    dFmt21t,  dFmt21t,  dFmt21t,  dFmt21t,  dFmt21t,  dFmt21t,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt23x,  dFmt23x,
    dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,
    dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt22c,  dFmt22c,
    dFmt22c,  dFmt22c,  dFmt22c,  dFmt22c,  dFmt22c,  dFmt22c,  dFmt22c,
    dFmt22c,  dFmt22c,  dFmt22c,  dFmt22c,  dFmt22c,  dFmt21c,  dFmt21c,
    dFmt21c,  dFmt21c,  dFmt21c,  dFmt21c,  dFmt21c,  dFmt21c,  dFmt21c,
    dFmt21c,  dFmt21c,  dFmt21c,  dFmt21c,  dFmt21c,  dFmt35c,  dFmt35c,
    dFmt35c,  dFmt35c,  dFmt35c,  dFmt00x,  dFmt3rc,  dFmt3rc,  dFmt3rc,
    dFmt3rc,  dFmt3rc,  dFmt00x,  dFmt00x,  dFmt12x,  dFmt12x,  dFmt12x,
    dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,
    dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,
    dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt23x,  dFmt23x,  dFmt23x,
    dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,
    dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,
    dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,
    dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,  dFmt23x,
    dFmt23x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,
    dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,
    dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,
    dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,
    dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt12x,  dFmt22s,  dFmt22s,
    dFmt22s,  dFmt22s,  dFmt22s,  dFmt22s,  dFmt22s,  dFmt22s,  dFmt22b,
    dFmt22b,  dFmt22b,  dFmt22b,  dFmt22b,  dFmt22b,  dFmt22b,  dFmt22b,
    dFmt22b,  dFmt22b,  dFmt22b,  dFmt22c,  dFmt22c,  dFmt21c,  dFmt21c,
    dFmt22c,  dFmt22c,  dFmt22c,  dFmt21c,  dFmt21c,  dFmt00x,  dFmt20bc,
    dFmt35mi, dFmt3rmi, dFmt35c,  dFmt10x,  dFmt22cs, dFmt22cs, dFmt22cs,
    dFmt22cs, dFmt22cs, dFmt22cs, dFmt35ms, dFmt3rms, dFmt35ms, dFmt3rms,
    dFmt22c,  dFmt21c,  dFmt21c,  dFmt00x,  dFmt41c,  dFmt41c,  dFmt52c,
    dFmt41c,  dFmt52c,  dFmt5rc,  dFmt52c,  dFmt52c,  dFmt52c,  dFmt52c,
    dFmt52c,  dFmt52c,  dFmt52c,  dFmt52c,  dFmt52c,  dFmt52c,  dFmt52c,
    dFmt52c,  dFmt52c,  dFmt52c,  dFmt41c,  dFmt41c,  dFmt41c,  dFmt41c,
    dFmt41c,  dFmt41c,  dFmt41c,  dFmt41c,  dFmt41c,  dFmt41c,  dFmt41c,
    dFmt41c,  dFmt41c,  dFmt41c,  dFmt5rc,  dFmt5rc,  dFmt5rc,  dFmt5rc,
    dFmt5rc,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,  dFmt00x,
    dFmt00x,  dFmt5rc,  dFmt52c,  dFmt52c,  dFmt52c,  dFmt52c,  dFmt52c,
    dFmt52c,  dFmt41c,  dFmt41c,  dFmt41c,  dFmt41c,  dFmt41c,  dFmt41c,
    dFmt40sc,
    // END(libdex-formats)
};


/*
 * Global InstructionInfoTables struct.
 */
DIRInstructionInfoTables gDIROpcodeInfo = {
    gDIRInstructionFormatTable,
    gDIROpcodeFlagsTable,
    gDIRInstructionWidthTable
};


/*
 * Handy macros for helping decode instructions.
 */
#define FETCH(_offset)      (insns[(_offset)])
#define FETCH_u4(_offset)   (FETCH_u4_impl((_offset), insns))
#define INST_A(_inst)       (((UInt16)(_inst) >> 8) & 0x0f)
#define INST_B(_inst)       ((UInt16)(_inst) >> 12)
#define INST_AA(_inst)      ((_inst) >> 8)

/* Helper for FETCH_u4, above. */
static inline UInt32 FETCH_u4_impl(UInt32 offset, const UInt16* insns) {
    return insns[offset] | ((UInt32) insns[offset+1] << 16);
}


static inline DIROpcode getOpcodeFromCodeUnit(UInt16 codeUnit) {
    int lowByte = codeUnit & 0xff;
#if (ANDROID_MAJOR_VERSION >= 40)
    if (lowByte != 0xff) {
        return (DIROpcode) lowByte;
    } else {
        return (DIROpcode) ((codeUnit >> 8) | 0x100);
    }
#else
    return (DIROpcode) lowByte;
#endif
}

/*
 * Decode the instruction pointed to by "insns".
 *
 * Fills out the pieces of "pDec" that are affected by the current
 * instruction.  Does not touch anything else.
 */
void DIRDecodeInstruction(const UInt16* insns, DIRDecodedInsn* pDec)
{
    UInt16 inst = *insns;
    DIROpcode opcode = getOpcodeFromCodeUnit(inst);
    DIRInstructionFormat format = gDIRInstructionFormatTable[opcode];

    pDec->opcode = opcode;

    switch (format) {
    case dFmt10x:       // op
        /* nothing to do; copy the AA bits out for the verifier */
        pDec->vA = INST_AA(inst);
        break;
    case dFmt12x:       // op vA, vB
        pDec->vA = INST_A(inst);
        pDec->vB = INST_B(inst);
        break;
    case dFmt11n:       // op vA, #+B
        pDec->vA = INST_A(inst);
        pDec->vB = (int) (INST_B(inst) << 28) >> 28; // sign extend 4-bit value
        break;
    case dFmt11x:       // op vAA
        pDec->vA = INST_AA(inst);
        break;
    case dFmt10t:       // op +AA
        pDec->vA = (signed char) INST_AA(inst);              // sign-extend 8-bit value
        break;
    case dFmt20t:       // op +AAAA
        pDec->vA = (short) FETCH(1);                   // sign-extend 16-bit value
        break;
    case dFmt20bc:      // [opt] op AA, thing@BBBB
    case dFmt21c:       // op vAA, thing@BBBB
    case dFmt22x:       // op vAA, vBBBB
        pDec->vA = INST_AA(inst);
        pDec->vB = FETCH(1);
        break;
    case dFmt21s:       // op vAA, #+BBBB
    case dFmt21t:       // op vAA, +BBBB
        pDec->vA = INST_AA(inst);
        pDec->vB = (short) FETCH(1);                   // sign-extend 16-bit value
        break;
    case dFmt21h:       // op vAA, #+BBBB0000[00000000]
        pDec->vA = INST_AA(inst);
        /*
         * The value should be treated as right-zero-extended, but we don't
         * actually do that here. Among other things, we don't know if it's
         * the top bits of a 32- or 64-bit value.
         */
        pDec->vB = FETCH(1);
        break;
    case dFmt23x:       // op vAA, vBB, vCC
        pDec->vA = INST_AA(inst);
        pDec->vB = FETCH(1) & 0xff;
        pDec->vC = FETCH(1) >> 8;
        break;
    case dFmt22b:       // op vAA, vBB, #+CC
        pDec->vA = INST_AA(inst);
        pDec->vB = FETCH(1) & 0xff;
        pDec->vC = (signed char) (FETCH(1) >> 8);            // sign-extend 8-bit value
        break;
    case dFmt22s:       // op vA, vB, #+CCCC
    case dFmt22t:       // op vA, vB, +CCCC
        pDec->vA = INST_A(inst);
        pDec->vB = INST_B(inst);
        pDec->vC = (short) FETCH(1);                   // sign-extend 16-bit value
        break;
    case dFmt22c:       // op vA, vB, thing@CCCC
    case dFmt22cs:      // [opt] op vA, vB, field offset CCCC
        pDec->vA = INST_A(inst);
        pDec->vB = INST_B(inst);
        pDec->vC = FETCH(1);
        break;
    case dFmt30t:       // op +AAAAAAAA
        pDec->vA = FETCH_u4(1);                     // signed 32-bit value
        break;
    case dFmt31t:       // op vAA, +BBBBBBBB
    case dFmt31c:       // op vAA, string@BBBBBBBB
        pDec->vA = INST_AA(inst);
        pDec->vB = FETCH_u4(1);                     // 32-bit value
        break;
    case dFmt32x:       // op vAAAA, vBBBB
        pDec->vA = FETCH(1);
        pDec->vB = FETCH(2);
        break;
    case dFmt31i:       // op vAA, #+BBBBBBBB
        pDec->vA = INST_AA(inst);
        pDec->vB = FETCH_u4(1);                     // signed 32-bit value
        break;
    case dFmt35c:       // op {vC, vD, vE, vF, vG}, thing@BBBB
    case dFmt35ms:      // [opt] invoke-virtual+super
    case dFmt35mi:      // [opt] inline invoke
        {
            /*
             * Note that the fields mentioned in the spec don't appear in
             * their "usual" positions here compared to most formats. This
             * was done so that the field names for the argument count and
             * reference index match between this format and the corresponding
             * range formats (3rc and friends).
             *
             * Bottom line: The argument count is always in vA, and the
             * method constant (or equivalent) is always in vB.
             */
            UInt16 regList;
            int i, count;

            pDec->vA = INST_B(inst); // This is labeled A in the spec.
            pDec->vB = FETCH(1);
            regList = FETCH(2);

            count = pDec->vA;

            /*
             * Copy the argument registers into the arg[] array, and
             * also copy the first argument (if any) into vC. (The
             * DecodedInstruction structure doesn't have separate
             * fields for {vD, vE, vF, vG}, so there's no need to make
             * copies of those.) Note that cases 5..2 fall through.
             */
            switch (count) {
            case 5: {
                if (format == dFmt35mi) {
                    /* A fifth arg is verboten for inline invokes. */
                    LOGE("Invalid arg count in 35mi (5)");
                    goto bail;
                }
                /*
                 * Per note at the top of this format decoder, the
                 * fifth argument comes from the A field in the
                 * instruction, but it's labeled G in the spec.
                 */
                pDec->arg[4] = INST_A(inst);
            }
            case 4: pDec->arg[3] = (regList >> 12) & 0x0f;
            case 3: pDec->arg[2] = (regList >> 8) & 0x0f;
            case 2: pDec->arg[1] = (regList >> 4) & 0x0f;
            case 1: pDec->vC = pDec->arg[0] = regList & 0x0f; break;
            case 0: break; // Valid, but no need to do anything.
            default:
                LOGW("Invalid arg count in 35c/35ms/35mi (%d)", count);
                goto bail;
            }
        }
        break;
    case dFmt3rc:       // op {vCCCC .. v(CCCC+AA-1)}, meth@BBBB
    case dFmt3rms:      // [opt] invoke-virtual+super/range
    case dFmt3rmi:      // [opt] execute-inline/range
        pDec->vA = INST_AA(inst);
        pDec->vB = FETCH(1);
        pDec->vC = FETCH(2);
        break;
    case dFmt51l:       // op vAA, #+BBBBBBBBBBBBBBBB
        pDec->vA = INST_AA(inst);
        pDec->vB_wide = FETCH_u4(1) | ((UInt64) FETCH_u4(3) << 32);
        break;
    case dFmt33x:       // exop vAA, vBB, vCCCC
        pDec->vA = FETCH(1) & 0xff;
        pDec->vB = FETCH(1) >> 8;
        pDec->vC = FETCH(2);
        break;
    case dFmt32s:       // exop vAA, vBB, #+CCCC
        pDec->vA = FETCH(1) & 0xff;
        pDec->vB = FETCH(1) >> 8;
        pDec->vC = (short) FETCH(2);                   // sign-extend 16-bit value
        break;
    case dFmt40sc:      // [opt] exop AAAA, thing@BBBBBBBB
    case dFmt41c:       // exop vAAAA, thing@BBBBBBBB
        /*
         * The order of fields for this format in the spec is {B, A},
         * to match formats 21c and 31c.
         */
        pDec->vB = FETCH_u4(1);                     // 32-bit value
        pDec->vA = FETCH(3);
        break;
    case dFmt52c:       // exop vAAAA, vBBBB, thing@CCCCCCCC
        /*
         * The order of fields for this format in the spec is {C, A, B},
         * to match formats 22c and 22cs.
         */
        pDec->vC = FETCH_u4(1);                     // 32-bit value
        pDec->vA = FETCH(3);
        pDec->vB = FETCH(4);
        break;
    case dFmt5rc:       // exop {vCCCC .. v(CCCC+AAAA-1)}, meth@BBBBBBBB
        /*
         * The order of fields for this format in the spec is {B, A, C},
         * to match formats 3rc and friends.
         */
        pDec->vB = FETCH_u4(1);                     // 32-bit value
        pDec->vA = FETCH(3);
        pDec->vC = FETCH(4);
        break;
    default:
        LOGW("Can't decode unexpected format %d (op=%d)", format, opcode);
        assert(false);
        break;
    }

bail:
    ;
}
