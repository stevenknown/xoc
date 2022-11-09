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
#ifndef _REFINE_DUCHAIN_H_
#define _REFINE_DUCHAIN_H_

namespace xoc {

class RefineDUChain : public Pass {
    COPY_CONSTRUCTOR(RefineDUChain);
protected:
    DUMgr * m_du;
    TypeMgr * m_tm;
    GVN const* m_gvn;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    bool m_is_use_gvn;
    DefMiscBitSetMgr * m_sbs_mgr;
protected:
    bool processExpViaMDSSA(IR const* exp);
    bool processNormalExpByClassicDU(IR const* exp);
    bool processNormalExpByMDSSA(IR const* exp);
    bool processNormalExp(IR const* exp);
    //Return true if DU chain changed.
    bool processIndirectExpViaGVN(IR const* exp);
    bool processArrayExpViaGVN(IR const* exp);
    bool processIndirectExp(IR const* exp);
    bool processArrayExp(IR const* exp);
    bool processBB(IRBB const* bb);
    bool process();

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    explicit RefineDUChain(Region * rg) :
        Pass(rg), m_gvn(nullptr), m_prssamgr(nullptr), m_mdssamgr(nullptr),
        m_is_use_gvn(false)
    {
        ASSERT0(rg != nullptr);
        m_du = rg->getDUMgr();
        m_tm = rg->getTypeMgr();
        m_sbs_mgr = rg->getMiscBitSetMgr();
    }
    virtual ~RefineDUChain() {}

    virtual bool dump() const;

    //Return true if indirect operation ir1 has same base expression with ir2.
    //TODO: use gvn to utilize value flow.
    bool hasSameBase(IR const* ir1, IR const* ir2);

    virtual CHAR const* getPassName() const
    { return "Refine DefUse Chain"; }
    PASS_TYPE getPassType() const { return PASS_PRE; }

    //True to use GVN and classic DU chain to perform optimization.
    void setUseGvn(bool use_gvn) { m_is_use_gvn = use_gvn; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
