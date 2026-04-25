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

//Store AI in a sparse manner.
//e.g: By a given AI object of type AI_TBAA, it is stored at the position
//that indexed by AI_TBAA in the container, regardless of whether the
//container is empty.
#define SPARSE_STORE_AI

//
//START AIContainer
//
void AIContainer::copy(AIContainer const* ai, Region * rg)
{
    ASSERT0(ai && ai->is_init());
    //Copy of m_cont will do initialization if needed.
    m_cont.copy(ai->m_cont, rg->getAttachInfoMgr()->get_pool());
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
    case AI_TENSOR: return "TENSOR";
    case AI_LAST:;
    default: UNREACHABLE();
    }
    return nullptr;
}


void AIContainer::cleanBySparse(AI_TYPE type)
{
    if (!is_init()) { return; }
    ASSERT0(type > AI_UNDEF && type < AI_LAST);
    if (((UINT)type) >= m_cont.get_capacity()) { return; }
    m_cont[type] = nullptr;
}


void AIContainer::cleanByDense(AI_TYPE type)
{
    if (!is_init()) { return; }
    ASSERT0(type > AI_UNDEF && type < AI_LAST);
    for (UINT i = 0; i < m_cont.get_capacity(); i++) {
        BaseAttachInfo * ac = m_cont.get(i);
        if (ac != nullptr && ac->getType() == type) {
            m_cont[i] = nullptr;
            return;
        }
    }
}


void AIContainer::clean(AI_TYPE type)
{
    #ifdef SPARSE_STORE_AI
    cleanBySparse(type);
    #else
    cleanByDense(type);
    #endif
}


BaseAttachInfo * AIContainer::getBySparse(AI_TYPE type) const
{
    if (!is_init()) {
        //To faciliate the use of getAI(), disable the assertion of init.
        return nullptr;
    }
    ASSERT0(type > AI_UNDEF && type < AI_LAST);
    return m_cont.get(type);
}


BaseAttachInfo * AIContainer::getByDense(AI_TYPE type) const
{
    if (!is_init()) {
        //To faciliate the use of getAI(), disable the assertion of init.
        return nullptr;
    }
    ASSERT0(type > AI_UNDEF && type < AI_LAST);
    for (UINT i = 0; i < m_cont.get_capacity(); i++) {
        BaseAttachInfo * ac = m_cont.get(i);
        if (ac != nullptr && ac->getType() == type) {
            return ac;
        }
    }
    return nullptr;
}


BaseAttachInfo * AIContainer::get(AI_TYPE type) const
{
    #ifdef SPARSE_STORE_AI
    return getBySparse(type);
    #else
    return getByDense(type);
    #endif
}


void AIContainer::set(BaseAttachInfo * c, Region * rg)
{
    #ifdef SPARSE_STORE_AI
    setBySparse(c, rg);
    #else
    setByDense(c, rg);
    #endif
}


void AIContainer::setBySparse(BaseAttachInfo * c, Region * rg)
{
    ASSERTN(c, ("Can not set empty AI"));
    ASSERT0(is_init());
    AI_TYPE type = c->getType();
    ASSERT0(type > AI_UNDEF && type < AI_LAST);

    //AIContainer buffer will grow bigger.
    //NOTE: c will override the prior AIContainer that has same type.
    m_cont.set(type, c, rg->getAttachInfoMgr()->get_pool());
}


void AIContainer::setByDense(BaseAttachInfo * c, Region * rg)
{
    ASSERTN(c, ("Can not set empty AI"));
    ASSERT0(is_init());

    AI_TYPE type = c->getType();
    ASSERT0(type > AI_UNDEF && type < AI_LAST);

    UINT i = 0;
    INT emptyslot = -1;
    INT existslot = -1;
    for (; i < m_cont.get_capacity(); i++) {
        BaseAttachInfo * ac = m_cont.get(i);
        if (ac == nullptr) {
            emptyslot = (INT)i;
            continue;
        }
        if (ac->getType() == type) {
            existslot = (INT)i;
            break;
        }
    }

    if (existslot != -1) {
        //Note c will override the prior AIContainer that has same type.
        m_cont.set((UINT)existslot, c, rg->getAttachInfoMgr()->get_pool());
        return;
    }

    if (emptyslot != -1) {
        m_cont.set((UINT)emptyslot, c, rg->getAttachInfoMgr()->get_pool());
        return;
    }

    //AIContainer buffer will grow bigger.
    m_cont.set(i, c, rg->getAttachInfoMgr()->get_pool());
}
//END

} //namespace xoc
