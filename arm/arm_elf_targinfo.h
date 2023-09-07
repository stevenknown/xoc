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
#ifndef _ARM_ELF_TARGINFO_H_
#define _ARM_ELF_TARGINFO_H_

//The macro defines 32bit ARM relocation types.
#define R_ARM_NONE 0
#define R_ARM_PC24 1
#define R_ARM_ABS32 2
#define R_ARM_REL32 3
#define R_ARM_CALL 28
#define R_ARM_JUMP24 29
#define R_ARM_TARGET1 38
#define R_ARM_V4BX 40
#define R_ARM_PREL31 42
#define R_ARM_MOVW_ABS_NC 43
#define R_ARM_MOVT_ABS 44
#define R_ARM_MOVW_PREL_NC 45
#define R_ARM_MOVT_PREL 46
#define R_ARM_ALU_PC_G0_NC 57
#define R_ARM_ALU_PC_G1_NC 59
#define R_ARM_LDR_PC_G2 63

//The macro defines ARM THUMB relocation types.
#define R_ARM_THM_CALL 10
#define R_ARM_THM_JUMP24 30
#define R_ARM_THM_MOVW_ABS_NC 47
#define R_ARM_THM_MOVT_ABS 48
#define R_ARM_THM_MOVW_PREL_NC 49
#define R_ARM_THM_MOVT_PREL 50

class ARMELFTargInfo : public elf::ELFTargInfo {
public:
    ARMELFTargInfo(elf::ELFMgr * mgr) : elf::ELFTargInfo(mgr) {}
    virtual ~ARMELFTargInfo() {}

    //Return the machine type.
    virtual CHAR const* getMachineTypeName() const { return "ARM"; }

    //Return the relocation type.
    virtual CHAR const* getRelTypeName(elf::Word r) const;
};

#endif
