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
#ifndef _IR_DCE_H_
#define _IR_DCE_H_

namespace xoc {

class EffectStmt : public xcom::BitSet {
public:
    EffectStmt() {}
    COPY_CONSTRUCTOR(EffectStmt);
    void bunion(INT elem)
    {
        xcom::BitSet::bunion(elem);
    }
};

//Perform dead code and redundant control flow elimination.
class DeadCodeElim : public Pass {
    COPY_CONSTRUCTOR(DeadCodeElim);
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    Region * m_rg;
    IRCFG * m_cfg;
    CDG * m_cdg;
    DUMgr * m_du;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    ConstIRIter m_citer;
    bool m_is_elim_cfs; //Eliminate control flow structure if necessary.
    EffectStmt m_is_stmt_effect;
    xcom::BitSet m_is_bb_effect;

    //Whether utilize MD du chain to find effect stmt.
    //If the value is false, all memory operations are considered used
    //except the operations which operate on PR.
    bool m_is_use_md_du;

    bool check_stmt(IR const* ir);
    bool check_call(IR const* ir) const;
    bool collectByPRSSA(IR const* x, IN OUT List<IR const*> * pwlst2);
    bool collectAllDefThroughDefChain(MDDef const* tdef,
                                      IN OUT List<IR const*> * pwlst2);
    bool collectByMDSSA(IR const* x, IN OUT List<IR const*> * pwlst2);
    bool collectByDU(IR const* x, IN OUT List<IR const*> * pwlst2);

    void fix_control_flow(List<IRBB*> & bblst, List<C<IRBB*>*> & ctlst);
    bool find_effect_kid(IRBB const* bb, IR const* ir) const;

    bool is_effect_write(Var * v) const
    { return VAR_is_global(v) || VAR_is_volatile(v); }
    bool is_effect_read(Var * v) const { return VAR_is_volatile(v); }
    bool is_cfs(IR const* ir) const
    {
        switch (ir->getCode()) {
        case IR_TRUEBR:
        case IR_FALSEBR:
        case IR_GOTO:
        case IR_IGOTO:
            return true;
        default: ASSERT0(ir->isStmtInBB());
        }
        return false;
    }
    void iter_collect(IN OUT List<IR const*> & work_list);

    void mark_effect_ir(IN OUT List<IR const*> & work_list);

    bool preserve_cd(IN OUT List<IR const*> & act_ir_lst);

    void reinit();
    void reviseSuccForFallthroughBB(IRBB * bb, 
                                    BBListIter bbct,
                                    BBList * bbl) const;
    bool remove_ineffect_ir() const;
    bool removeRedundantPhi();

    //Set control-dep bb to be effective.
    bool setControlDepBBToBeEffect(IRBB const* bb,
                                   IN OUT List<IR const*> & act_ir_lst);
    void setEffectStmt(IR const* stmt,
                       IN OUT xcom::BitSet * is_bb_effect,
                       IN OUT List<IR const*> * act_ir_lst);

    bool useMDSSADU() const
    { return m_mdssamgr != NULL && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != NULL && m_prssamgr->is_valid(); }
public:
    explicit DeadCodeElim(Region * rg)
    {
        ASSERT0(rg != NULL);
        m_rg = rg;
        m_tm = rg->getTypeMgr();
        m_cfg = rg->getCFG();
        m_du = rg->getDUMgr();
        m_mdssamgr = NULL;
        m_prssamgr = NULL;
        m_md_sys = rg->getMDSystem();
        ASSERT0(m_cfg && m_du && m_md_sys && m_tm);
        m_is_elim_cfs = true;
        m_is_use_md_du = true;
        m_cdg = NULL;
    }
    virtual ~DeadCodeElim() {}

    virtual bool dump() const;

    virtual CHAR const* getPassName() const
    { return "Dead Code Eliminiation"; }
    virtual PASS_TYPE getPassType() const { return PASS_DCE; }

    void set_elim_cfs(bool doit) { m_is_elim_cfs = doit; }
    void set_use_md_du(bool use_md_du) { m_is_use_md_du = use_md_du; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
