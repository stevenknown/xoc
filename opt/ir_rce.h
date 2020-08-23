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
#ifndef _IR_RCE_H_
#define _IR_RCE_H_

namespace xoc {

//Perform Redundant Code Elimination.
class RCE : public Pass {
    COPY_CONSTRUCTOR(RCE);
protected:
    Region * m_rg;
    IRCFG * m_cfg;
    GVN * m_gvn;
    DUMgr * m_du;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    Refine * m_refine;
    //Use GVN info to determine if code is redundant.
    //Note that compute GVN is expensive.
    bool m_use_gvn;

    bool useMDSSADU() const
    { return m_mdssamgr != NULL && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != NULL && m_prssamgr->is_valid(); }
public:
    RCE(Region * rg, GVN * gvn)
    {
        ASSERT0(rg != NULL);
        m_rg = rg;
        m_gvn = gvn;
        m_cfg = rg->getCFG();
        m_du = m_rg->getDUMgr();
        ASSERT0(m_cfg && m_du);
        m_use_gvn = true;
        m_prssamgr = NULL;
        m_mdssamgr = NULL;
        m_refine = NULL;
    }
    virtual ~RCE() {}

    //If 'ir' is always true, set 'must_true', or if it is
    //always false, set 'must_false'.
    //Return true if this function is able to determine the result of 'ir',
    //otherwise return false that it does know nothing about ir.
    bool calcCondMustVal(IR const* ir,
                         OUT bool & must_true,
                         OUT bool & must_false) const;

    //If 'ir' is always true, set 'must_true', or if it is
    //always false, set 'must_false'.
    //Return the changed ir.
    IR * calcCondMustVal(IN IR * ir,
                         OUT bool & must_true,
                         OUT bool & must_false,
                         bool & changed);

    virtual bool dump() const;

    virtual CHAR const* getPassName() const
    { return "Redundant Code Elimination"; }
    PASS_TYPE getPassType() const { return PASS_RCE; }
    GVN * getGVN() const { return m_gvn; }

    bool is_use_gvn() const { return m_use_gvn; }

    void set_use_gvn(bool use_gvn) { m_use_gvn = use_gvn; }

    IR * processStore(IR * ir);
    IR * processStorePR(IR * ir);
    IR * processBranch(IR * ir, IN OUT bool * cfg_mod);
    bool performSimplyRCE(IN OUT bool * cfg_mod);
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
