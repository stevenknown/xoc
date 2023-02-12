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
#include "../x64/x64_elf_targinfo.h"

static CHAR const* g_reltype_name[] = {
    "R_X8664_NONE",
    "R_X8664_64",
    "R_X8664_PC32",
    "R_X8664_GOT32",
    "R_X8664_PLT32",
    "R_X8664_COPY",
    "R_X8664_GLOB_DAT",
    "R_X8664_JUMP_SLOT",
    "R_X8664_RELATIVE",
    "R_X8664_GOTPCREL",
    "R_X8664_32",
    "R_X8664_32S",
    "R_X8664_16",
    "R_X8664_PC16",
    "R_X8664_8",
    "R_X8664_PC8",
    "R_X8664_DPTMOD64",
    "R_X8664_DTPOFF64",
    "R_X8664_TPOFF64",
    "R_X8664_TLSGD",
    "R_X8664_TLSLD",
    "R_X8664_DTPOFF32",
    "R_X8664_GOTTPOFF",
    "R_X8664_TPOFF32",
    "R_X8664_PC64",
    "R_X8664_GOTOFF64",
    "R_X8664_GOTPC32",
    "R_X8664_SIZE32",
    "R_X8664_SIZE64",
};

CHAR const* X64ELFTargInfo::getRelTypeName(elf::Word r) const
{
    //TBD: Is the relocation type inherited from 32bit x86.
    //if (is32bit()) { return X86ELFMgr::getRelTypeName(r); }
    switch (r) {
    case R_X8664_NONE: return g_reltype_name[0];
    case R_X8664_64: return g_reltype_name[1];
    case R_X8664_PC32: return g_reltype_name[2];
    case R_X8664_GOT32: return g_reltype_name[3];
    case R_X8664_PLT32: return g_reltype_name[4];
    case R_X8664_COPY: return g_reltype_name[5];
    case R_X8664_GLOB_DAT: return g_reltype_name[6];
    case R_X8664_JUMP_SLOT: return g_reltype_name[7];
    case R_X8664_RELATIVE: return g_reltype_name[8];
    case R_X8664_GOTPCREL: return g_reltype_name[9];
    case R_X8664_32: return g_reltype_name[10];
    case R_X8664_32S: return g_reltype_name[11];
    case R_X8664_16: return g_reltype_name[12];
    case R_X8664_PC16: return g_reltype_name[13];
    case R_X8664_8: return g_reltype_name[14];
    case R_X8664_PC8: return g_reltype_name[15];
    case R_X8664_DPTMOD64: return g_reltype_name[16];
    case R_X8664_DTPOFF64: return g_reltype_name[17];
    case R_X8664_TPOFF64: return g_reltype_name[18];
    case R_X8664_TLSGD: return g_reltype_name[19];
    case R_X8664_TLSLD: return g_reltype_name[20];
    case R_X8664_DTPOFF32: return g_reltype_name[21];
    case R_X8664_GOTTPOFF: return g_reltype_name[22];
    case R_X8664_TPOFF32: return g_reltype_name[23];
    case R_X8664_PC64: return g_reltype_name[24];
    case R_X8664_GOTOFF64: return g_reltype_name[25];
    case R_X8664_GOTPC32: return g_reltype_name[26];
    case R_X8664_SIZE32: return g_reltype_name[27];
    case R_X8664_SIZE64: return g_reltype_name[28];
    default:;
    }
    return nullptr;
}
