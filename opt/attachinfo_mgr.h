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
#ifndef __ATTACHINFO_MGR_H__
#define __ATTACHINFO_MGR_H__

namespace xoc {

class AttachInfoMgr {
    friend class Region;
    friend class AIContainer;
    COPY_CONSTRUCTOR(AttachInfoMgr);
protected:
    Region * m_rg;
    SMemPool * m_pool;

    //Allocate AIContainer that describes attach info to IR.
    inline AIContainer * allocAIContainer()
    {
        AIContainer * ai = (AIContainer*)xmalloc(sizeof(AIContainer));
        ASSERT0(ai);
        ai->init();
        return ai;
    }

    SMemPool * get_pool() const { return m_pool; }

    void * xmalloc(UINT size)
    {
        ASSERTN(m_pool != nullptr, ("pool does not initialized"));
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p != nullptr);
        ::memset(p, 0, size);
        return p;
    }
public:
    AttachInfoMgr(Region * rg) : m_rg(rg)
    { m_pool = smpoolCreate(256, MEM_COMM); }
    virtual ~AttachInfoMgr() { smpoolDelete(m_pool); }
    virtual void copyAI(MOD IR * tgt, IR const* src);
};

} //namespace xoc
#endif
