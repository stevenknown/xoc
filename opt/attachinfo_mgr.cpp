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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

void AttachInfoMgr::copyAI(OUT IR * tgtir, IR const* srcir)
{
    ASSERT0(tgtir && srcir && tgtir != srcir);
    AIContainer * tgt = IR_ai(tgtir);
    AIContainer const* src = srcir->getAI();
    ASSERT0(tgt && src && tgt != src);

    UINT sz = src->getContainer()->get_capacity();
    if (sz > 0 && !tgt->getContainer()->is_init()) {
        tgt->init(sz, m_rg->getAttachInfoMgr()->get_pool());
    }
    for (UINT i = 0; i < sz; i++) {
        BaseAttachInfo * srcac = src->getContainer()->get(i);
        if (srcac == nullptr) { continue; }
        
        switch (srcac->getType()) {
        case AI_DBX:
        case AI_PROF:
        case AI_TBAA:
        case AI_EH_LABEL:
        case AI_USER_DEF:
            tgt->set(srcac, m_rg);
            break;
        case AI_MD_SSA: {
            //Do NOT copy MDSSAInfo, it will be generated for individual IR
            //if necessary.
            break;
        }
        default: UNREACHABLE();
        }
    }
}

} //namespace xoc
