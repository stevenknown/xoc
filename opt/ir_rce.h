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
public:
    class RCECtx {
    public:
        bool cfg_mod;
        bool retry_bblist;
        OptCtx * oc;
    public:
        RCECtx(OptCtx * t) : cfg_mod(false), retry_bblist(false), oc(t) {}
        ~RCECtx() {}
    };
protected:
    IRCFG * m_cfg;
    GVN * m_gvn;
    DUMgr * m_du;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    Refine * m_refine;
    //Use GVN info to determine if code is redundant.
    //Note that compute GVN is expensive.
    bool m_use_gvn;
private:
    IR * processTruebr(IR * ir, IR * new_det, bool must_true,
                       bool must_false, bool changed, MOD RCECtx & ctx);
    IR * processFalsebr(IR * ir, IR * new_det, bool must_true,
                        bool must_false, bool changed, MOD RCECtx & ctx);
    bool performSimplyRCEForBB(IRBB * bb, MOD RCECtx & ctx);
    IR * processStore(IR * ir, RCECtx const& ctx);
    IR * processStorePR(IR * ir, RCECtx const& ctx);
    IR * processBranch(IR * ir, MOD RCECtx & ctx);
    bool performSimplyRCE(MOD RCECtx & ctx);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    RCE(Region * rg, GVN * gvn) : Pass(rg)
    {
        ASSERT0(rg != nullptr);
        m_gvn = gvn;
        m_cfg = rg->getCFG();
        m_du = m_rg->getDUMgr();
        ASSERT0(m_cfg && m_du);
        m_use_gvn = true;
        m_prssamgr = nullptr;
        m_mdssamgr = nullptr;
        m_refine = nullptr;
    }
    virtual ~RCE() {}

    //If 'ir' is always true, set 'must_true', or if it is
    //always false, set 'must_false'.
    //Return true if this function is able to determine the result of 'ir',
    //otherwise return false that it does know nothing about ir.
    bool calcCondMustVal(IR const* ir, OUT bool & must_true,
                         OUT bool & must_false, OptCtx const& oc) const;

    //If 'ir' is always true, set 'must_true', or if it is
    //always false, set 'must_false'.
    //Return the changed ir.
    IR * calcCondMustVal(MOD IR * ir, OUT bool & must_true,
                         OUT bool & must_false, OUT bool & changed,
                         MOD OptCtx & oc);

    virtual bool dump() const { return true; }

    virtual CHAR const* getPassName() const
    { return "Redundant Code Elimination"; }
    PASS_TYPE getPassType() const { return PASS_RCE; }
    GVN * getGVN() const { return m_gvn; }
    IRCFG * getCFG() const { return m_cfg; }

    bool is_use_gvn() const { return m_use_gvn; }

    void set_use_gvn(bool use_gvn) { m_use_gvn = use_gvn; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
