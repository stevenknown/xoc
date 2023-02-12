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
#include "../elf/elfinc.h"
#include "../score/score_elf_targinfo.h"

static CHAR const* g_reltype_name[] = {
    //PC relatived reloc types
    "R_SCORE7_PC19",
    "R_SCORE7_16_PC8",

    //Absolutely addressing reloc types
    "R_SCORE7_JMP24",
    "R_SCORE7_16_JMP11",
    "R_SCORE7_HI16",
    "R_SCORE7_LO16",
    "R_SCORE7_GP16",
    "R_SCORE7_ABS32",
};
 
CHAR const* Score7ELFTargInfo::getRelTypeName(elf::Word r) const
{
    switch (r) {
    case R_SCORE7_PC19: return g_reltype_name[0];
    case R_SCORE7_16_PC8: return g_reltype_name[1];
    case R_SCORE7_JMP24: return g_reltype_name[2];
    case R_SCORE7_16_JMP11: return g_reltype_name[3];
    case R_SCORE7_HI16: return g_reltype_name[4];
    case R_SCORE7_LO16: return g_reltype_name[5];
    case R_SCORE7_GPREL15: return g_reltype_name[6];
    case R_SCORE7_ABS32: return g_reltype_name[7];
    default:;
    }
    return nullptr;
}
