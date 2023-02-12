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
#include "../arm/arm_elf_targinfo.h"

static CHAR const* g_reltype_name[] = {
    "R_ARM_NONE",
    "R_ARM_PC24",
    "R_ARM_ABS32",
    "R_ARM_REL32",
    "R_ARM_CALL",
    "R_ARM_JUMP24",
    "R_ARM_TARGET1",
    "R_ARM_V4BX",
    "R_ARM_PREL31",
    "R_ARM_MOVW_ABS_NC",
    "R_ARM_MOVT_ABS",
    "R_ARM_MOVW_PREL_NC",
    "R_ARM_MOVT_PREL",
    "R_ARM_ALU_PC_G0_NC",
    "R_ARM_ALU_PC_G1_NC",
    "R_ARM_LDR_PC_G2",
    "R_ARM_THM_CALL",
    "R_ARM_THM_JUMP24",
    "R_ARM_THM_MOVW_ABS_NC",
    "R_ARM_THM_MOVT_ABS",
    "R_ARM_THM_MOVW_PREL_NC",
    "R_ARM_THM_MOVT_PREL",
};

CHAR const* ARMELFTargInfo::getRelTypeName(elf::Word r) const
{
    switch (r) {
    case R_ARM_NONE: return g_reltype_name[0];
    case R_ARM_PC24: return g_reltype_name[1];
    case R_ARM_ABS32: return g_reltype_name[2];
    case R_ARM_REL32: return g_reltype_name[3];
    case R_ARM_CALL: return g_reltype_name[4];
    case R_ARM_JUMP24: return g_reltype_name[5];
    case R_ARM_TARGET1: return g_reltype_name[6];
    case R_ARM_V4BX: return g_reltype_name[7];
    case R_ARM_PREL31: return g_reltype_name[8];
    case R_ARM_MOVW_ABS_NC: return g_reltype_name[9];
    case R_ARM_MOVT_ABS: return g_reltype_name[10];
    case R_ARM_MOVW_PREL_NC: return g_reltype_name[11];
    case R_ARM_MOVT_PREL: return g_reltype_name[12];
    case R_ARM_ALU_PC_G0_NC: return g_reltype_name[13];
    case R_ARM_ALU_PC_G1_NC: return g_reltype_name[14];
    case R_ARM_LDR_PC_G2: return g_reltype_name[15];
    case R_ARM_THM_CALL: return g_reltype_name[16];
    case R_ARM_THM_JUMP24: return g_reltype_name[17];
    case R_ARM_THM_MOVW_ABS_NC: return g_reltype_name[18];
    case R_ARM_THM_MOVT_ABS: return g_reltype_name[19];
    case R_ARM_THM_MOVW_PREL_NC: return g_reltype_name[20];
    case R_ARM_THM_MOVT_PREL: return g_reltype_name[21];
    default:;
    }
    return nullptr;
}
