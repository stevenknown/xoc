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
#ifndef _IPA_H_
#define _IPA_H_

namespace xoc {

class IPA : public Pass {
protected:
    RegionMgr * m_rumgr;
    Region * m_program;
    SMemPool * m_pool;
    MDSystem * m_mdsys;
    bool m_is_keep_dumgr; //true to keep AA and DU mgr if computed.
    bool m_is_keep_reachdef; //true to keep Reachdef.
    bool m_is_recompute_du_ref; //true to recompute DU reference.

protected:
    void * xmalloc(UINT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }

    void createCallDummyuse(IR * call, Region * callru);
    void createCallDummyuse(Region * rg);
    void createCallDummyuse(OptCtx & oc);

    void recomputeDUChain(Region * rg, OptCtx & oc);

    Region * findRegion(IR * call, Region * callru);
public:
    IPA(Region * program)
    {
        ASSERT0(program && program->is_program());
        m_rumgr = program->getRegionMgr();
        ASSERT0(m_rumgr);
        m_program = program;
        m_mdsys = m_rumgr->getMDSystem();
        m_pool = smpoolCreate(16, MEM_COMM);
        m_is_keep_dumgr = false;
        m_is_keep_reachdef = false;
        m_is_recompute_du_ref = true;
    }
    virtual ~IPA() { smpoolDelete(m_pool); }

    void computeCallRefForAllRegion();

    void setKeepDUMgr(bool keep) { m_is_keep_dumgr = keep; }
    void setKeepReachdef(bool keep) { m_is_keep_reachdef = keep; }
    void setRecomputeDURef(bool doit) { m_is_recompute_du_ref = doit; }
    virtual CHAR const* getPassName() const { return "IPA"; }
    virtual PASS_TYPE getPassType() const { return PASS_IPA; }
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
