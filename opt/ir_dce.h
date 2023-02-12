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
    void bunion(INT elem) { xcom::BitSet::bunion(elem); }
};

typedef TTab<MDDef const*> EffectMDDef;

//Perform dead code and redundant control flow elimination.
class DeadCodeElim : public Pass {
    COPY_CONSTRUCTOR(DeadCodeElim);
    BYTE m_is_elim_cfs:1; //Eliminate control flow structure if necessary.

    //Sometime, we might expect to keep PHI there even it does not even have
    //any USE. Because PHI could make the maintaining of Def-Chain much easy.
    //e.g:md1v1, md1v2, md1v3, md1v4 are in different BB, PHI is act as
    //a disjoint-holder to link md1v3 and md1v4.
    //        md1v1<-
    //          |
    //          V
    //        md1v2<-PHI
    //       /         |
    //       V         V
    //  md1v3<-        md1v4<-
    BYTE m_is_reserve_phi:1;

    //Whether utilize MD du chain to find effect stmt.
    //If the value is false, all memory operations are considered used
    //except the operations which operate on PR.
    BYTE m_is_use_md_du:1;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    CDG * m_cdg;
    DUMgr * m_dumgr;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    OptCtx * m_oc;
    ConstIRIter m_citer;
    EffectStmt m_is_stmt_effect;
    EffectMDDef m_is_mddef_effect;
    //Record if BB is effect. Note the BB effect info is not always
    //identical to Stmt effect info, one BB may be not be marked as Effect even
    //if it contains effect stmt.
    xcom::BitSet m_is_bb_effect;

    void checkValidAndRecomputeCDG();
    bool check_stmt(IR const* ir);
    bool check_call(IR const* ir) const;
    bool collectByDU(IR const* x, MOD List<IR const*> * pwlst2,
                     bool usemdssa, bool useprssa);
    bool collectByPRSSA(IR const* x, MOD List<IR const*> * pwlst2);
    bool collectAllDefThroughDefChain(MDDef const* tdef, IR const* use,
                                      MOD List<IR const*> * pwlst2);
    bool collectByMDSSA(IR const* x, MOD List<IR const*> * pwlst2);
    bool collectByDUSet(IR const* x, MOD List<IR const*> * pwlst2);

    //Return true if there are effect BBs that controlled by ir's BB.
    //ir: stmt.
    bool find_effect_kid_condbr(IR const* ir) const;
    //Return true if there are effect BBs that controlled by ir's BB.
    //ir: stmt.
    bool find_effect_kid_uncondbr(IR const* ir) const;
    bool find_effect_kid(IR const* ir) const;

    bool isEffectBB(UINT id) const { return m_is_bb_effect.is_contain(id); }
    bool isEffectBB(IRBB const* bb) const { return isEffectBB(bb->id()); }
    bool is_effect_write(Var * v) const
    { return v->is_global() || v->is_volatile(); }
    bool is_effect_read(Var * v) const { return v->is_volatile(); }
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
    void iter_collect(MOD List<IR const*> & work_list);

    void mark_effect_ir(MOD List<IR const*> & work_list);

    bool preserve_cd(MOD List<IR const*> & act_ir_lst);

    void reinit();
    bool removeIneffectIR(OUT bool & remove_branch_stmt);
    bool removeRedundantPhi();

    bool tryMarkBranch(IRBB const* bb, OUT List<IR const*> & act_ir_lst);
    bool tryMarkUnconditionalBranch(IRBB const* bb,
                                    MOD List<IR const*> & act_ir_lst);

    //The function marks possible predecessor in CFG to be effect BB,
    //e.g back-edge.
    bool markCFGPred(IRBB const* bb);
    bool markControlPredAndStmt(IRBB const* bb,
                                OUT List<IR const*> & act_ir_lst);

    //Set control-dep bb to be effective.
    bool setControlDepBBToBeEffect(IRBB const* bb,
                                   MOD List<IR const*> & act_ir_lst);
    void setEffectBB(IRBB const* bb) { m_is_bb_effect.bunion(bb->id()); }
    void setEffectStmt(IR const* stmt, OUT xcom::BitSet * is_bb_effect,
                       OUT List<IR const*> * act_ir_lst);
    void setEffectMDDef(MDDef const* mddef, OUT xcom::BitSet * is_bb_effect);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    explicit DeadCodeElim(Region * rg) : Pass(rg)
    {
        ASSERT0(rg != nullptr);
        m_tm = rg->getTypeMgr();
        m_cfg = rg->getCFG();
        m_dumgr = rg->getDUMgr();
        m_mdssamgr = nullptr;
        m_prssamgr = nullptr;
        m_md_sys = rg->getMDSystem();
        ASSERT0(m_cfg && m_dumgr && m_md_sys && m_tm);
        m_is_elim_cfs = true;
        m_is_reserve_phi = false;
        m_is_use_md_du = true;
    }
    virtual ~DeadCodeElim() {}

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const;

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    OptCtx * getOptCtx() const { return m_oc; }
    IRCFG * getCFG() const { return m_cfg; }
    virtual CHAR const* getPassName() const
    { return "Dead Code Eliminiation"; }
    virtual PASS_TYPE getPassType() const { return PASS_DCE; }

    bool isEffectStmt(IR const* ir) const
    { return m_is_stmt_effect.is_contain(ir->id()); }

    void set_reserve_phi(bool reserve) { m_is_reserve_phi = reserve; }
    void set_elim_cfs(bool doit) { m_is_elim_cfs = doit; }
    void set_use_md_du(bool use_md_du) { m_is_use_md_du = use_md_du; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
