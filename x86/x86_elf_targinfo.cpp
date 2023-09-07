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
#include "../x86/x86_elf_targinfo.h"

static CHAR const* g_reltype_name[] = {
    "R_386_NONE",
    "R_386_32",
    "R_386_PC32",
    "R_386_GOT32",
    "R_386_PLT32",
    "R_386_COPY",
    "R_386_GLOB_DAT",
    "R_386_JMP_SLOT",
    "R_386_RELATIVE",
    "R_386_GOTOFF",
    "R_386_GOTPC",
    "R_386_32PLT",
    "R_386_16",
    "R_386_PC16",
    "R_386_8",
    "R_386_PC8",
    "R_386_SIZE32",
};

CHAR const* X86ELFTargInfo::getRelTypeName(elf::Word r) const
{
    switch (r) {
    case R_386_NONE: return g_reltype_name[0];
    case R_386_32: return g_reltype_name[1];
    case R_386_PC32: return g_reltype_name[2];
    case R_386_GOT32: return g_reltype_name[3];
    case R_386_PLT32: return g_reltype_name[4];
    case R_386_COPY: return g_reltype_name[5];
    case R_386_GLOB_DAT: return g_reltype_name[6];
    case R_386_JMP_SLOT: return g_reltype_name[7];
    case R_386_RELATIVE: return g_reltype_name[8];
    case R_386_GOTOFF: return g_reltype_name[9];
    case R_386_GOTPC: return g_reltype_name[10];
    case R_386_32PLT: return g_reltype_name[11];
    case R_386_16: return g_reltype_name[12];
    case R_386_PC16: return g_reltype_name[13];
    case R_386_8: return g_reltype_name[14];
    case R_386_PC8: return g_reltype_name[15];
    case R_386_SIZE32: return g_reltype_name[16];
    default:;
    }
    return nullptr;
}
