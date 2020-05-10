/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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
    Region * m_rg;
    DUMgr * m_du;
    GVN const* m_gvn;
    PRSSAMgr * m_ssamgr;
    MDSSAMgr * m_mdssamgr;
    bool m_is_use_gvn;
    DefMiscBitSetMgr * m_sbs_mgr;
protected:
    //This function try to require VN of base of ir.
    //Return the VN if found, and the indirect operation level.
    //e.g: given ILD(ILD(p)), return p and ist_star_level is 2.
    //e.g2: given IST(ILD(q)), return q and ist_star_level is 2.
    VN const* getVNOfIndirectOp(IR const* ir, UINT * indirect_level);

    void processExpressionViaMDSSA(IR const* exp);
    void processExpressionViaGVN(IR const* exp);
    void processBB(IRBB const* bb);
    void process();

public:
    explicit RefineDUChain(Region * rg) :
        m_rg(rg), m_gvn(NULL), m_mdssamgr(NULL), m_is_use_gvn(false)
    {
        ASSERT0(rg != NULL);
        m_rg = rg;
        m_du = rg->getDUMgr();
        m_sbs_mgr = rg->getMiscBitSetMgr();
    }
    virtual ~RefineDUChain() {}

    bool dump();

    //True to use GVN and classic DU chain to perform optimization.
    void setUseGvn(bool use_gvn) { m_is_use_gvn = use_gvn; }

    virtual CHAR const* getPassName() const
    { return "Refine DefUse Chain"; }
    PASS_TYPE getPassType() const { return PASS_PRE; }
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
