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
#define CPC_change(c)            (c).change
#define CPC_need_recomp_aa(c)    (c).need_recompute_alias_info
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
#define CP_PROP_CONST                1

//Propagate the simplex operation, include CONST, PR, LDA, CVT for simplex.
#define CP_PROP_SIMPLEX                2

//Propagate unary and simplex operations, include CONST, PR, LDA, CVT, LD,
//ID, NEG, BNOT, LNOT, ILD.
#define CP_PROP_UNARY_AND_SIMPLEX    3

//Perform Copy Propagation
class IR_CP : public Pass {
protected:
    Region * m_ru;
    MDSystem * m_md_sys;
    IR_DU_MGR * m_du;
    IR_CFG * m_cfg;
    MDSetMgr * m_md_set_mgr;
    TypeMgr * m_tm;
    UINT m_prop_kind;

    inline bool checkTypeConsistency(
            IR const* ir,
            IR const* cand_expr) const;
    bool doProp(IN IRBB * bb, Vector<IR*> & usevec);
    void doFinalRefine();

    bool is_simp_cvt(IR const* ir) const;
    bool is_const_cvt(IR const* ir) const;
    bool is_available(IR const* def_ir, IR const* occ, IR * use_ir);
    inline bool is_copy(IR * ir) const;

    bool performDomTree(IN Vertex * v, IN Graph & domtree);

    void replaceExp(
            IR * exp,
            IR const* cand_expr,
            IN OUT CPCtx & ctx,
            bool exp_use_ssadu);
    void replaceExpViaSSADu(
            IR * exp,
            IR const* cand_expr,
            IN OUT CPCtx & ctx);
public:
    IR_CP(Region * ru)
    {
        ASSERT0(ru != NULL);
        m_ru = ru;
        m_md_sys = ru->get_md_sys();
        m_du = ru->get_du_mgr();
        m_cfg = ru->get_cfg();
        m_md_set_mgr = ru->get_mds_mgr();
        m_tm = ru->get_type_mgr();
        ASSERT0(m_cfg && m_du && m_md_sys && m_tm && m_md_set_mgr);
        m_prop_kind = CP_PROP_UNARY_AND_SIMPLEX;
    }
    virtual ~IR_CP() {}

    //Check if ir is appropriate for propagation.
    virtual bool canBeCandidate(IR const* ir) const
    {
        switch (m_prop_kind) {
        case CP_PROP_CONST:
            return ir->is_lda() || ir->is_const_exp();
        case CP_PROP_SIMPLEX:
            switch (IR_code(ir)) {
            case IR_LDA:
            case IR_ID:
            case IR_CONST:
            case IR_PR:
                return true;
            default: return is_simp_cvt(ir);
            }
        case CP_PROP_UNARY_AND_SIMPLEX:
            switch (IR_code(ir)) {
            case IR_LD:
            case IR_LDA:
            case IR_ID:
            case IR_CONST:
            case IR_PR:
            case IR_NEG:
            case IR_BNOT:
            case IR_LNOT:
            case IR_ILD:
                return true;
            default: return is_simp_cvt(ir);
            }
        default:;
        }
        UNREACH();
        return false;
    }

    virtual CHAR const* get_pass_name() const { return "Copy Propagation"; }
    virtual PASS_TYPE get_pass_type() const { return PASS_CP; }

    void set_prop_kind(UINT kind) { m_prop_kind = kind; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
