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
private:
    Region * m_rg;
    MDSystem * m_md_sys;
    DUMgr * m_du;
    IRCFG * m_cfg;
    MDSetMgr * m_md_set_mgr;
    TypeMgr * m_tm;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    Refine * m_refine;
    GVN const* m_gvn;
    UINT m_prop_kind;

private:
    //Return true if CP allows propagating memory object with inexact MD.
    bool allowInexactMD() const
    { return m_prop_kind == CP_PROP_UNARY_AND_SIMPLEX; }

    bool checkTypeConsistency(IR const* ir, IR const* cand_exp) const;

    bool doPropUseSet(IRSet * useset, IR * def_stmt,
                      IR const* prop_value, MDSSAInfo * mdssainfo,
                      IRListIter cur_iter, IRListIter * next_iter,
                      bool prssadu, bool mdssadu);
    bool doPropForMDPhi(IR const* prop_value, IN IR * use);
    bool doPropForNormalStmt(IRListIter cur_iter, IRListIter* next_iter,
                             IR const* prop_value, IN IR * use,
                             IRBB * def_bb);
    //useset: for local used.
    bool doPropIR(IR * def_stmt, IN IRSet * useset,
                  IRListIter cur_iter, IRListIter * next_iter);
    bool doPropBB(IN IRBB * bb, IN IRSet * useset);
    void doFinalRefine(OptCtx & oc);
    void dumpCopyPropagationAction(IR const* def_stmt, IR const* prop_value,
                                   IR const* use);

    bool existMayDefTillBB(IR const* exp, IRBB const* start,
                           IRBB const* meetup) const;

    DefSegMgr  * getSegMgr() const { return getSBSMgr()->getSegMgr(); }
    DefMiscBitSetMgr  * getSBSMgr() const { return m_rg->getMiscBitSetMgr(); }

    bool isLowCostExp(IR const* ir) const
    {
        switch (ir->getCode()) {
        case IR_LDA:
        case IR_CONST:
        case IR_PR:
            return true;
        default: return isLowCostCVT(ir);
        }
        UNREACHABLE();
        return false;
    }
    //Return true if CVT with simply cvt-exp that can be regard as
    //copy-propagate candidate.
    bool isSimpCVT(IR const* ir) const;
    //Return true if ir is CVT with cvt-exp that always include low-cost
    //expression. These low-cost always profitable and may bring up new
    //optimization opportunity.
    bool isLowCostCVT(IR const* ir) const;
    bool is_available(IR const* def_stmt, IR const* prop_value,
                      IR * use_stmt, MDPhi * use_phi, IRBB * usebb) const;
    inline bool isCopyOR(IR * ir) const;

    bool performDomTree(IN xcom::Vertex * v, IN xcom::Graph & domtree);
    //repexp: the expression that is expected to be replaced.
    IR const* pickupCandExp(IR const* prop_value, IR const* repexp,
                            IR const* def_stmt, MDSSAInfo const* mdssainfo,
                            bool prssadu, bool mdssadu) const;

    void replaceExp(IR * exp, IR const* cand_exp, MOD CPCtx & ctx);
    void replaceExpViaSSADu(IR * exp, IR const* cand_exp, MOD CPCtx & ctx);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }

public:
    CopyProp(Region * rg)
    {
        ASSERT0(rg != nullptr);
        m_rg = rg;
        m_md_sys = rg->getMDSystem();
        m_du = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_md_set_mgr = rg->getMDSetMgr();
        m_tm = rg->getTypeMgr();
        m_refine = nullptr;
        m_gvn = nullptr;
        m_mdssamgr = nullptr;
        m_prssamgr = nullptr;
        ASSERT0(m_cfg && m_du && m_md_sys && m_tm && m_md_set_mgr);
        m_prop_kind = CP_PROP_UNARY_AND_SIMPLEX;
    }
    virtual ~CopyProp() {}

    //Check if ir is appropriate for propagation.
    virtual bool canBeCandidate(IR const* ir) const
    {
        switch (m_prop_kind) {
        case CP_PROP_CONST:
            return ir->is_lda() || ir->isConstExp();
        case CP_PROP_SIMPLEX:
            switch (ir->getCode()) {
            case IR_LDA:
            case IR_ID:
            case IR_CONST:
            case IR_PR:
                return true;
            default: return isSimpCVT(ir);
            }
        case CP_PROP_UNARY_AND_SIMPLEX:
            switch (ir->getCode()) {
            case IR_LDA:
            case IR_ID:
            case IR_CONST:
            case IR_PR:
                return true;
            case IR_LD:
                ASSERT0(ir->getRefMD());
                return true;
            case IR_ILD:
                if (ir->getRefMD() == nullptr || !ir->getRefMD()->is_exact()) {
                    //TBD:In aggressive mode, we anticipate propagating RHS
                    //expression even if it is inexact.
                    //e.g: s is MC type.
                    //    s = *p
                    //    *q = s
                    //  *p can be propagated.
                    //return false;
                }
                ASSERT0(ir->getRefMD() || ir->getRefMDSet());
                return true;
            default: return isSimpCVT(ir);
            }
        default: UNREACHABLE();
        }
        return false;
    }
    Region * getRegion() const { return m_rg; }
    virtual CHAR const* getPassName() const { return "Copy Propagation"; }
    virtual PASS_TYPE getPassType() const { return PASS_CP; }
    IR const* getSimpCVTValue(IR const* ir) const;

    void setPropagationKind(UINT kind) { m_prop_kind = kind; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
