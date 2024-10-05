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

typedef TTab<MDDef const*> EffectMDDef;

typedef xcom::DefSBitSetIter StmtSetIter;
class StmtSet : public xcom::DefSBitSet {
    COPY_CONSTRUCTOR(StmtSet);
public:
    StmtSet(DefSegMgr & sm) : DefSBitSet(&sm) {}
    void bunion(BSIdx elem) { xcom::DefSBitSet::bunion(elem); }
};

class DCECtx {
    COPY_CONSTRUCTOR(DCECtx);
public:
    bool m_is_remove_cfs;
    OptCtx * m_oc;

    //Record if BB is effect. Note the BB effect info is not always
    //identical to Stmt effect info, one BB may be not be marked as Effect even
    //if it contains effect stmt.
    BBSet m_effect_bb;
    StmtSet m_effect_stmt;
    StmtSet m_exclude_stmt;
public:
    DCECtx(OptCtx * oc, DefSegMgr & sm) :
        m_effect_bb(&sm), m_effect_stmt(sm), m_exclude_stmt(sm)
    { m_oc = oc; m_is_remove_cfs = true; }

    void addEffectStmt(IR const* stmt) { m_effect_stmt.bunion(stmt->id()); }
    void addEffectBB(IRBB const* bb) { m_effect_bb.bunion(bb->id()); }
    void addExcludeStmt(IR const* stmt) { m_exclude_stmt.bunion(stmt->id()); }

    void dump(Region const* rg) const;

    OptCtx * getOptCtx() const { return m_oc; }

    bool isRemoveCFS() const { return m_is_remove_cfs; }
    bool isEffectStmt(IR const* ir) const
    { return m_effect_stmt.is_contain(ir->id()); }
    bool isEffectBB(UINT id) const { return m_effect_bb.is_contain(id); }
    bool isEffectBB(IRBB const* bb) const { return isEffectBB(bb->id()); }
    bool isExcluded(IR const* ir) const
    { return m_exclude_stmt.is_contain(ir->id()); }

    //The function will reinitialize current ctx for subsequently process.
    void reinit()
    {
        //Only clean local used data.
        m_effect_stmt.clean();
        m_effect_bb.clean();
    }

    //Demand DCE pass whether to remove control-flow-structure.
    void setRemoveCFS(bool remove) { m_is_remove_cfs = remove; }
};


//Perform dead code and redundant control flow elimination.
class DeadCodeElim : public Pass {
    COPY_CONSTRUCTOR(DeadCodeElim);
protected:
    //Set to true to eliminate control flow structure if necessary.
    //The pass will try to fixup DomInfo and related SSA info. However, it is
    //costly to revise the DomInfo because the controlflow modification is
    //too compilcate.
    //e.g:BB1->BB2->BB3
    //     |         ^
    //     \________/
    //BB1 is the idom of BB3, after removing BB1->BB3, BB2 is add to the BB3's
    //DomSet, and being the idom of BB3 as well.
    //Note the invalidation to DomInfo and MDSSA cause costly recomputation of
    //all the necessary data-structure and information which will increase
    //dramatically the compilation time.
    BYTE m_is_elim_cfs:1;

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

    //Check the MD Must and May references of NonPR memory operaion if the flag
    //is true.
    //If the flag is false, all NonPR memory operations are regarded as effect
    //by default.
    //Note the flag only applies to NonPR operation.
    BYTE m_check_md_ref:1;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    CDG * m_cdg;
    DUMgr * m_dumgr;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    EffectMDDef m_is_mddef_effect;
    ActMgr m_act_mgr;
    DefMiscBitSetMgr m_sbs_mgr;
protected:
    void addEffectIRAndBB(ConstIRList const& efflist,
                          OUT DCECtx & dcectx) const;

    //Return true if ir is effect.
    bool checkEffectStmt(IR const* ir);

    //Return true if ir is effect.
    bool checkCall(IR const* ir) const;
    void checkValidAndRecomputeCDG(DCECtx const& dcectx);
    bool collectByDU(IR const* x, MOD ConstIRList * pwlst2,
                     MOD DCECtx & dcectx, bool usemdssa, bool useprssa);
    bool collectByPRSSA(IR const* x, MOD ConstIRList * pwlst2,
                        MOD DCECtx & dcectx);
    bool collectAllDefThroughDefChain(
        MDDef const* tdef, IR const* use, MOD ConstIRList * pwlst2,
        MOD DCECtx & dcectx);
    bool collectByMDSSA(IR const* x, MOD ConstIRList * pwlst2,
                        MOD DCECtx & dcectx);
    bool collectByDUSet(IR const* x, MOD ConstIRList * pwlst2,
                        MOD DCECtx & dcectx);

    bool elimImpl(OptCtx & oc, OUT DCECtx & dcectx,
                  OUT bool & remove_branch_stmt);

    //Return true if there are effect BBs that controlled by ir's BB.
    //ir: stmt.
    bool find_effect_kid_condbr(IR const* ir, MOD DCECtx & dcectx) const;

    //Return true if there are effect BBs that controlled by ir's BB.
    //ir: stmt.
    bool find_effect_kid_uncondbr(IR const* ir, MOD DCECtx & dcectx) const;
    bool find_effect_kid(IR const* ir, MOD DCECtx & dcectx) const;

    DefMiscBitSetMgr & getSBSMgr() { return m_sbs_mgr; }

    bool initSSAMgr(OptCtx const& oc);
    void iterCollect(MOD ConstIRList & efflist, MOD DCECtx & dcectx);
    bool iterCollectAndElim(ConstIRList & efflist, OUT DCECtx & dcectx,
                            OUT bool & remove_branch_stmt);

    void markEffectIRForRegion(
        IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx);
    void markEffectIRForReturn(
        IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx);
    void markEffectIRForCall(
        IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx);
    void markEffectIRForBranch(
        IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx);
    void markEffectIRForStmt(
        IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx);
    void markEffectIRForBBIRList(
        IRBB const* bb, MOD ConstIRList & work_list, MOD DCECtx & dcectx);
    void markEffectIR(MOD ConstIRList & work_list, MOD DCECtx & dcectx);

    bool preserveControlDep(MOD ConstIRList & act_ir_lst, MOD DCECtx & dcectx);

    bool tryMarkBranch(IRBB const* bb, OUT ConstIRList & act_ir_lst,
                       MOD DCECtx & dcectx);
    bool tryMarkUnconditionalBranch(
        IRBB const* bb, MOD ConstIRList & act_ir_lst, MOD DCECtx & dcectx);

    //The function marks possible predecessor in CFG to be effect BB,
    //e.g back-edge.
    bool markCFGPred(IRBB const* bb, MOD DCECtx & dcectx);
    bool markControlPredAndStmt(
        IRBB const* bb, OUT ConstIRList & act_ir_lst, MOD DCECtx & dcectx);

    //Set control-dep BB to be effective.
    bool setControlDepBBToBeEffect(
        IRBB const* bb, MOD ConstIRList & act_ir_lst, MOD DCECtx & dcectx);

    //Set stmt to be effect.
    void setEffectStmt(IR const* stmt, bool set_bb_effect,
                       OUT ConstIRList * act_ir_lst, MOD DCECtx & dcectx);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    explicit DeadCodeElim(Region * rg) : Pass(rg), m_act_mgr(rg)
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
        m_check_md_ref = true;
        m_cdg = nullptr;
    }
    virtual ~DeadCodeElim() {}

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const;

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    void dump(DCECtx const& dcectx) const;

    MDSystem * getMDSystem() const { return m_md_sys; }
    ActMgr & getActMgr() { return m_act_mgr; }
    IRCFG * getCFG() const { return m_cfg; }
    virtual CHAR const* getPassName() const
    { return "Dead Code Eliminiation"; }
    virtual PASS_TYPE getPassType() const { return PASS_DCE; }

    bool hasSideEffect(IR const* ir) const
    {
        ASSERT0(ir);
        return ir->isMayThrow(false) || ir->hasSideEffect(false) ||
               ir->isNoMove(false);
    }

    bool isAggressive() const { return m_is_elim_cfs; }
    bool isCheckMDRef() const { return m_check_md_ref; }
    bool is_effect_write(Var * v) const
    { return v->is_global() || v->is_volatile(); }
    bool is_effect_read(Var * v) const { return v->is_volatile(); }

    void setReservePhi(bool reserve) { m_is_reserve_phi = reserve; }
    void setElimCFS(bool doit) { m_is_elim_cfs = doit; }
    void setCheckMDRef(bool check_md_ref) { m_check_md_ref = check_md_ref; }
    void setAggressive(bool doit) { setElimCFS(doit); }

    //The funtion performs iterative collection according to given effect IR
    //stmt list 'efflist' and eliminate ineffect IR and BB.
    //efflist: record the effect IR stmts that provided by user.
    //remove_branch_stmt: return true if the function eliminated branch-stmt.
    //Return true if there are IR stmts removed.
    bool performByEffectIRList(
        ConstIRList & efflist, OUT DCECtx & dcectx,
        OUT bool remove_branch_stmt);
    virtual bool perform(OptCtx & oc);

    //The function is an interface to eliminate ineffect IR that are not
    //recorded in 'dcectx'.
    //remove_branch_stmt: true to tell the interface function to remove
    //    branch stmt, such as truebr, goto control-flow-stmt.
    //Return true if some IR removed.
    //Note:DefUse chains have to be available before calling the function.
    //The function will maintain DefUse chain.
    bool removeIneffectIR(DCECtx const& dcectx, OUT bool & remove_branch_stmt);

    //The function is an interface to remove reundant PHI, include PRSSA Phi
    //and MDSSA Phi.
    //Return true if some PHI removed.
    //Note:DefUse chains have to be available before calling the function.
    //The function will maintain DefUse chain.
    bool removeRedundantPhi(MOD OptCtx & oc);
};

} //namespace xoc
#endif
