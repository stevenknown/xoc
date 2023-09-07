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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _X64_ELF_TARGINFO_H_
#define _X64_ELF_TARGINFO_H_

//The macro defines 64bit X64 relocation types.
#define R_X8664_NONE 0
#define R_X8664_64 1
#define R_X8664_PC32 2
#define R_X8664_GOT32 3
#define R_X8664_PLT32 4
#define R_X8664_COPY 5
#define R_X8664_GLOB_DAT 6
#define R_X8664_JUMP_SLOT 7
#define R_X8664_RELATIVE 8
#define R_X8664_GOTPCREL 9
#define R_X8664_32 10
#define R_X8664_32S 11
#define R_X8664_16 12
#define R_X8664_PC16 13
#define R_X8664_8 14
#define R_X8664_PC8 15
#define R_X8664_DPTMOD64 16
#define R_X8664_DTPOFF64 17
#define R_X8664_TPOFF64 18
#define R_X8664_TLSGD 19
#define R_X8664_TLSLD 20
#define R_X8664_DTPOFF32 21
#define R_X8664_GOTTPOFF 22
#define R_X8664_TPOFF32 23
#define R_X8664_PC64 24
#define R_X8664_GOTOFF64 25
#define R_X8664_GOTPC32 26
#define R_X8664_SIZE32 32
#define R_X8664_SIZE64 33

class X64ELFTargInfo : public X86ELFTargInfo {
public:
    X64ELFTargInfo(elf::ELFMgr * mgr) : X86ELFTargInfo(mgr) {}
    virtual ~X64ELFTargInfo() {}

    //Return the machine type.
    virtual CHAR const* getMachineTypeName() const { return "X86-64"; }

    //Return the relocation type.
    virtual CHAR const* getRelTypeName(elf::Word r) const;
};

#endif
