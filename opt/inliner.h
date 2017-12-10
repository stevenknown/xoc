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
#ifndef _INLINER_H_
#define _INLINER_H_

namespace xoc {

#define INLINFO_need_el(i)        ((i)->need_el)
#define INLINFO_has_ret(i)        ((i)->has_ret)
class InlineInfo {
public:
    BYTE need_el:1;
    BYTE has_ret:1;
};


class Inliner : public Pass {
protected:
    RegionMgr * m_rumgr;
    SMemPool * m_pool;
    CallGraph * m_call_graph;
    Region * m_program;
    TMap<Region*, InlineInfo*> m_ru2inl;

    void checkRegion(IN Region * rg,
                     OUT bool & need_el,
                     OUT bool & has_ret) const;

    void * xmalloc(UINT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }

    InlineInfo * mapRegion2InlineInfo(Region * rg, bool alloc)
    {
        InlineInfo * ii = m_ru2inl.get(rg);
        if (ii == NULL && alloc) {
            ii = (InlineInfo*)xmalloc(sizeof(InlineInfo));
            m_ru2inl.set(rg, ii);
        }
        return ii;
    }

    IR * replaceReturnImpl(
            Region * caller,
            IR * caller_call,
            IR * new_irs,
            LabelInfo * el);
public:
    Inliner(Region * program)
    {
        ASSERT0(program && program->is_program());
        m_rumgr = program->getRegionMgr();
        ASSERT0(m_rumgr);
        m_program = program;
        m_call_graph = m_rumgr->getCallGraph();
        ASSERT0(m_call_graph);
        m_pool = smpoolCreate(16, MEM_COMM);
    }
    virtual ~Inliner() { smpoolDelete(m_pool); }

    bool can_be_cand(Region * rg);

    bool do_inline_c(Region * caller, Region * callee);
    void do_inline(Region * cand);

    inline bool is_call_site(IR * call, Region * rg);

    virtual PASS_TYPE getPassType() const { return PASS_INLINER; }
    virtual CHAR const* getPassName() const { return "Inliner"; }

    IR * replaceReturn(
            Region * caller,
            IR * caller_call,
            IR * new_irs,
            InlineInfo * ii);
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
