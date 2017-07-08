/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#ifndef _DEX_REGION_MGR_H_
#define _DEX_REGION_MGR_H_

class DexRegionMgr : public RegionMgr {
protected:
    SMemPool * m_pool;
    Var2UINT m_var2blt;
    UINT2Var m_blt2var;

protected:
    VAR * addVarForBuiltin(CHAR const* name);
    void initBuiltin();

public:
    DexRegionMgr()
    { m_pool = smpoolCreate(128, MEM_COMM); }

    COPY_CONSTRUCTOR(DexRegionMgr);
    virtual ~DexRegionMgr() { smpoolDelete(m_pool); }

    virtual Region * allocRegion(REGION_TYPE rt)
    { return new DexRegion(rt, this); }

    virtual CallGraph * allocCallGraph(UINT edgenum, UINT vexnum)
    { return new DexCallGraph(edgenum, vexnum, this); }

    Region * getProgramRegion()
    {
        //The first region should be program-region.
        return getRegion(1);
    }
    SMemPool * get_pool() { return m_pool; }
    Var2UINT & getVar2Builtin() { return m_var2blt; }
    UINT2Var & getBuiltin2Var() { return m_blt2var; }
    Var2UINT const& getVar2BuiltinC() const { return m_var2blt; }
    UINT2Var const& getBuiltin2VarC() const { return m_blt2var; }

    //Do some initialization.
    void init() { initBuiltin(); }

    DexRegionMgr * self() { return this; }

    void * xmalloc(UINT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }

    virtual bool processProgramRegion(Region * program, OptCtx * oc);
};

#endif
