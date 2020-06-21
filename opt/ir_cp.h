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
private:
    Region * m_rg;
    MDSystem * m_md_sys;
    DUMgr * m_du;
    IRCFG * m_cfg;
    MDSetMgr * m_md_set_mgr;
    TypeMgr * m_tm;
    UINT m_prop_kind;

private:
    bool checkTypeConsistency(IR const* ir, IR const* cand_expr) const;

    bool doPropToMDPhi(bool prssadu,
                       bool mdssadu,
                       IN IR const* prop_value,
                       IN IR * use,
                       MDSSAMgr * mdssamgr);
    bool doPropToNormalStmt(IRListIter cur_iter,
                            IRListIter* next_iter,
                            bool prssadu,
                            bool mdssadu,
                            IN IR const* prop_value,
                            IN IR * use,
                            IN IR * use_stmt,
                            IN IRBB * def_bb,
                            IN OUT IRBB * use_bb,
                            MDSSAMgr * mdssamgr);
    bool doProp(IN IRBB * bb, IN DefSBitSetCore * useset, MDSSAMgr * mdssamgr);
    void doFinalRefine(OptCtx & oc);
    void dumpCopyPropagationAction(IR const* def_stmt,
                                   IR const* prop_value,
                                   IR const* use,
                                   MDSSAMgr * mdssamgr);

    bool existMayDefTillBB(IR const* exp,
                           IRBB const* start,
                           IRBB const* meetup) const;

    bool isSimpCVT(IR const* ir) const;
    bool isConstCVT(IR const* ir) const;
    bool is_available(IR const* def_stmt,
                      IR const* prop_value,
                      IR * use_stmt,
                      MDPhi * use_phi,
                      IRBB * usebb);
    inline bool isCopyOR(IR * ir) const;

    bool performDomTree(IN xcom::Vertex * v, IN xcom::Graph & domtree);

    void replaceExp(IR * exp,
                    IR const* cand_expr,
                    IN OUT CPCtx & ctx,
                    bool stmt_use_ssadu,
                    bool stmt_use_mdssadu,
                    MDSSAMgr * mdssamgr);
    void replaceExpViaSSADu(IR * exp,
                            IR const* cand_expr,
                            IN OUT CPCtx & ctx);

public:
    CopyProp(Region * rg)
    {
        ASSERT0(rg != NULL);
        m_rg = rg;
        m_md_sys = rg->getMDSystem();
        m_du = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_md_set_mgr = rg->getMDSetMgr();
        m_tm = rg->getTypeMgr();
        ASSERT0(m_cfg && m_du && m_md_sys && m_tm && m_md_set_mgr);
        m_prop_kind = CP_PROP_UNARY_AND_SIMPLEX;
    }
    COPY_CONSTRUCTOR(CopyProp);
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
            case IR_ILD:
                if (ir->getRefMD() != NULL && ir->getRefMD()->is_exact()) {
                    return true;
                }
                return false;
            default: return isSimpCVT(ir);
            }
        default: UNREACHABLE();
        }
        return false;
    }
    virtual CHAR const* getPassName() const { return "Copy Propagation"; }
    virtual PASS_TYPE getPassType() const { return PASS_CP; }
    IR const* getSimpCVTValue(IR const* ir) const;

    void setPropagationKind(UINT kind) { m_prop_kind = kind; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
