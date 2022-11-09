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
#ifndef _IR_CP_H_
#define _IR_CP_H_

namespace xoc {

//Record Context info during Copy Propagation.
#define CPC_change(c) (c).change
#define CPC_need_recomp_aa(c) (c).need_recompute_alias_info
class CPCtx {
public:
    bool change;
    bool need_recompute_alias_info;

    CPCtx()
    {
        change = false;
        need_recompute_alias_info = false;
    }

    //Perform bit or operation.
    void bor(CPCtx & c)
    {
        change |= c.change;
        need_recompute_alias_info |= c.need_recompute_alias_info;
    }
};


//Propagate the constant operation, include CONST, LDA, CVT for const.
#define CP_PROP_CONST 1

//Propagate the simplex operation, include CONST, PR, LDA, CVT for simplex.
#define CP_PROP_SIMPLEX 2

//Propagate unary and simplex operations, include CONST, PR, LDA, CVT, LD,
//ID, NEG, BNOT, LNOT, ILD.
#define CP_PROP_UNARY_AND_SIMPLEX 3

//Perform Copy Propagation
class CopyProp : public Pass {
    COPY_CONSTRUCTOR(CopyProp);
protected:
    MDSystem * m_md_sys;
    DUMgr * m_dumgr;
    IRCFG * m_cfg;
    MDSetMgr * m_md_set_mgr;
    TypeMgr * m_tm;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    GVN * m_gvn;
    OptCtx * m_oc;
    UINT m_prop_kind;
protected:
    //Return true if CP allows propagating memory object with inexact MD.
    bool allowInexactMD() const
    { return m_prop_kind == CP_PROP_UNARY_AND_SIMPLEX; }

    bool computeUseSet(IR const* def_stmt, OUT IRSet * useset,
                       OUT bool & prssadu, OUT bool & mdssadu);
    bool checkTypeConsistency(IR const* ir, IR const* cand_exp) const;

    bool doPropUseSet(IRSet const* useset, IR const* def_stmt,
                      IR const* prop_value, IRListIter cur_iter,
                      IRListIter * next_iter,
                      bool prssadu, bool mdssadu);
    bool doPropForMDPhi(IR const* prop_value, MOD IR * use);
    bool doPropForNormalStmt(IRListIter cur_iter, IRListIter* next_iter,
                             IR const* prop_value, MOD IR * use,
                             IRBB * def_bb);
    //useset: for local used.
    bool doPropIR(IR * def_stmt, IN IRSet * useset,
                  IRListIter cur_iter, IRListIter * next_iter);
    bool doPropBB(IN IRBB * bb, IN IRSet * useset);
    void doFinalRefine(OptCtx & oc);
    void dumpCopyPropAction(IR const* def_stmt, IR const* prop_value,
                            IR const* use);

    bool existMayDefTillBB(IR const* exp, IRBB const* start,
                           IRBB const* meetup) const;

    DefSegMgr  * getSegMgr() const { return getSBSMgr()->getSegMgr(); }
    DefMiscBitSetMgr  * getSBSMgr() const { return m_rg->getMiscBitSetMgr(); }
    OptCtx const* getOptCtx() const { return m_oc; }

    //Return the value expression that to be propagated.
    virtual IR * getPropagatedValue(IR * stmt);

    virtual bool isLowCostExp(IR const* ir) const
    {
        switch (ir->getCode()) {
        case IR_LDA:
        case IR_CONST:
        SWITCH_CASE_READ_PR:
            return true;
        default: return isLowCostCVT(ir);
        }
        UNREACHABLE();
        return false;
    }
    //Return true if CVT with simply cvt-exp that can be regard as
    //copy-propagate candidate.
    virtual bool isSimpCVT(IR const* ir) const;

    //Return true if ir is CVT with cvt-exp that always include low-cost
    //expression. These low-cost always profitable and may bring up new
    //optimization opportunity.
    virtual bool isLowCostCVT(IR const* ir) const;

    //Return true if 'prop_value' does not be modified till meeting 'use_stmt'.
    //e.g:xx = prop_value //def_stmt
    //    ..
    //    ..
    //    use_bb:
    //    yy = xx  //use_stmt|use_phi
    //
    //def_stmt: ir stmt.
    //prop_value: expression that will be propagated.
    //Note either use_phi or use_stmt is nullptr.
    virtual bool isAvailable(IR const* def_stmt, IR const* prop_value,
                             IR const* repexp) const;
    virtual bool isCopyOP(IR * ir) const;

    bool performDomTree(IN xcom::Vertex * v, IN xcom::Graph & domtree);

    //prop_value: the expression that is going to propagate.
    //repexp: the expression that is expected to be replaced.
    //def_stmt: the stmt of prop_value.
    //The layout of parameters is:
    //  def_stmt <- prop_value
    //       ... <- repexp
    IR const* pickupCandExp(IR const* prop_value, IR const* repexp,
                            IR const* def_stmt,
                            bool prssadu, bool mdssadu) const;

    void replaceExp(MOD IR * exp, IR const* cand_exp, MOD CPCtx & ctx);

    //Check if the CVT can be discarded and the cvt-expression will be regarded
    //as the recommended propagate value.
    //prop_value: indicates the value that will be propagated, must be CVT.
    //Note that user can implement target dependent interface to enable
    //more policies.
    virtual IR const* tryDiscardCVT(IR const* prop_value) const;

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    CopyProp(Region * rg) : Pass(rg)
    {
        m_md_sys = rg->getMDSystem();
        m_dumgr = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_md_set_mgr = rg->getMDSetMgr();
        m_tm = rg->getTypeMgr();
        m_gvn = nullptr;
        m_oc = nullptr;
        m_mdssamgr = nullptr;
        m_prssamgr = nullptr;
        ASSERT0(m_cfg && m_dumgr && m_md_sys && m_tm && m_md_set_mgr);
        m_prop_kind = CP_PROP_UNARY_AND_SIMPLEX;
    }
    virtual ~CopyProp() {}

    //Check if ir is appropriate for propagation.
    virtual bool canBeCandidate(IR const* ir) const;

    virtual CHAR const* getPassName() const { return "Copy Propagation"; }
    virtual PASS_TYPE getPassType() const { return PASS_CP; }
    IR const* getSimpCVTValue(IR const* ir) const;

    void setPropagationKind(UINT kind) { m_prop_kind = kind; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
