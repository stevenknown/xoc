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
#ifndef __IR_BCP_H__
#define __IR_BCP_H__

namespace xoc {

//The class represents the Branch Condition Propagation.

class BCPCtx : public PassCtx {
    COPY_CONSTRUCTOR(BCPCtx);
protected:
    CDG * m_cdg;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
public:
    BCPCtx(OptCtx * oc, ActMgr * am);
    CDG const* getCDG() const { return m_cdg; }
    IRCFG const* getCFG() const { return m_cfg; }
    IRMgr * getIRMgr() const { return m_irmgr; }
    GVN * getGVN() const { return this->m_gvn; }
    void tryInvalidPassInfoBeforeFreeIR(IR const* ir) const;
};

class BrCondProp : public Pass {
    COPY_CONSTRUCTOR(BrCondProp);
protected:
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    ActMgr m_am;
protected:
    //Return true if branch conditions in branch stmt have been changed.
    bool doProp(MOD BCPCtx & ctx);
    ActMgr & getActMgr() { return m_am; }
    bool initSSAMgr(OptCtx const& oc);
    bool initDepPass(OptCtx & oc);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }

public:
    BrCondProp(Region * rg);
    virtual ~BrCondProp();

    //The function dump pass relative information.
    bool dump(BCPCtx const& ctx) const;

    virtual CHAR const* getPassName() const
    { return "Branch Condition Propagation"; }
    PASS_TYPE getPassType() const { return PASS_BCP; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc

#endif
