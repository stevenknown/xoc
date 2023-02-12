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
#ifndef _SCORE_ELF_TARGINFO_H_
#define _SCORE_ELF_TARGINFO_H_

#define HEAP_START_NAME "_score_heap_start"
#define HEAP_END_NAME "_score_heap_end"

#define SHN_SCORE_ACOMMON 0xff00
#define SHN_SCORE_TEXT 0xff01
#define SHN_SCORE_DATA 0xff02
#define SHN_SCORE_SCOMMON 0xff03
#define SHN_SCORE_SUNDEFINED 0xff04

#define R_SCORE7_PC19 5
#define R_SCORE7_16_JMP11 6
#define R_SCORE7_16_PC8 7
#define R_SCORE7_JMP24 4
#define R_SCORE7_HI16 1
#define R_SCORE7_LO16 2
#define R_SCORE7_GPREL15 11
#define R_SCORE7_ABS32 8

class Score7ELFTargInfo : public elf::ELFTargInfo {
public:
    Score7ELFTargInfo(elf::ELFMgr * mgr) : elf::ELFTargInfo(mgr) {}
    virtual ~Score7ELFTargInfo() {}
 
    //Return the machine type.
    virtual CHAR const* getMachineTypeName() const { return "Score7"; }

    //Return the relocation type.
    virtual CHAR const* getRelTypeName(elf::Word r) const;
};

#endif
