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
#include "../elf/elfinc.h"

//The target machines that ELFMgr supported.
#ifdef FOR_ARM
#include "../arm/arm_elf_targinfo.h"
#endif

#ifdef FOR_SCORE
#include "../score/score_elf_targinfo.h"
#endif

#ifdef FOR_X86
#include "../x86/x86_elf_targinfo.h"
#endif

#ifdef FOR_X64
#include "../x64/x64_elf_targinfo.h"
#endif

namespace elf {

void MiscELFMgr::allocTargInfo()
{
    switch (m_elf_hdr.e_machine) {
    case EM_NONE: ASSERTN(0, ("illegal machine information"));
    #ifdef FOR_ARM
    case EM_ARM: m_ti = new ARMELFTargInfo(this); break;
    #endif
    #ifdef FOR_X86
    case EM_386: m_ti = new X86ELFTargInfo(this); break;
    #endif
    #ifdef FOR_SCORE
    case EM_SCORE7: m_ti = new Score7ELFTargInfo(this); break;
    #endif
    #ifdef FOR_X64
    case EM_X86_64: m_ti = new X64ELFTargInfo(this); break;
    #endif
    default:;
    }
}

} //namespace
