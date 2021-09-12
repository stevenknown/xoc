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

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START AIContainer
//
void AIContainer::copy(AIContainer const* ai, Region * rg)
{
    ASSERT0(ai && ai->is_init());
    //Copy of cont will do initialization if needed.
    cont.copy(ai->cont, rg->getAttachInfoMgr()->get_pool());
}


//The function formats string name for XOC recognized attach-info.
CHAR const* AIContainer::getAIName(AI_TYPE type) const
{
    switch (type) {
    case AI_UNDEF: return "Undef";
    case AI_DBX: return "Dbx";
    case AI_PROF: return "Prof";
    case AI_TBAA: return "TBAA";
    case AI_EH_LABEL: return "EH";
    case AI_USER_DEF: return "UserDef";
    case AI_MD_SSA: return "MDSSA";
    case AI_LAST:;
    default: UNREACHABLE();
    }
    return nullptr;
}


void AIContainer::set(BaseAttachInfo * c, Region * rg)
{
    ASSERTN(c, ("Can not set empty AI"));
    ASSERT0(is_init());

    AI_TYPE type = c->getType();
    ASSERT0(type > AI_UNDEF && type < AI_LAST);

    UINT i = 0;
    INT emptyslot = -1;
    INT existslot = -1;
    for (; i < cont.get_capacity(); i++) {
        BaseAttachInfo * ac = cont.get(i);
        if (ac == nullptr) {
            emptyslot = (INT)i;
        } else if (ac->getType() == type) {
            existslot = (INT)i;
            break;
        }
    }

    if (existslot != -1) {
        //Note c will override the prior AIContainer that has same type.
        cont.set((UINT)existslot, c, rg->getAttachInfoMgr()->get_pool());
        return;
    }

    if (emptyslot != -1) {
        cont.set((UINT)emptyslot, c, rg->getAttachInfoMgr()->get_pool());
        return;
    }

    //AIContainer buffer will grow bigger.
    cont.set(i, c, rg->getAttachInfoMgr()->get_pool());
}
//END

} //namespace xoc
