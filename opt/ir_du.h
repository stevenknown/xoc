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
#ifndef _IR_DU_
#define _IR_DU_

namespace xoc {

//Util Functions supplied by DUMgr
// These functions manipulate the reference of IR.
// IR may reference MD and MDSet.
//    collectMayUse
//    collectMayUseRecursive
//    changeUse
//    changeDef
//    getExactAndUniqueDef
//    isMayDef
//    isExactAndUniqueDef
//    isCallMayDef
//
//* These functions manipulate the DU chain.
//    addUse
//    buildDUChain
//    genDUSet
//    removeDUChain
//    removeExpiredDU
//    removeDef
//    removeUseFromDefset
//    removeDefFromUseset
//    removeIRFromDUMgr

class DUMgr;
class AliasAnalysis;

//Mapping from IR to index.
typedef HMap<IR const*, UINT, HashFuncBase2<IR const*> > IR2UINT;

//Mapping from IR to DUSet.
typedef HMap<IR*, DUSet*> IR2DU;

//Mapping from MD to IR list, and to be responsible for
//allocating and destroy List<IR*> objects.
class MD2IRSet : public TMap<MDIdx, DefSBitSetCore*> {
    COPY_CONSTRUCTOR(MD2IRSet);
    //Indicate if there exist stmt which only have MayDef.
    BYTE m_are_stmts_defed_ineffect_md:1;
    Region * m_rg;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    DUMgr * m_du;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    xcom::TMapIter<UINT, DefSBitSetCore*> m_iter;
    xcom::DefSBitSetCore m_global_md;
public:
    explicit MD2IRSet(Region * rg);
    ~MD2IRSet();

    void append(MD const* md, IR const* ir)
    {
        ASSERT0(ir);
        append(MD_id(md), ir->id());
    }
    void append(MD const* md, UINT irid) { append(MD_id(md), irid); }
    void append(MDIdx mdid, IR * ir)
    {
        ASSERT0(ir);
        append(mdid, ir->id());
    }
    void append(MDIdx mdid, UINT irid);

    void clean();

    void dump() const;

    Region * getRegion() const { return m_rg; }

    bool hasIneffectDef() const { return m_are_stmts_defed_ineffect_md; }

    void set(MDIdx mdid, IR * ir);
    void setIneffectDef() { m_are_stmts_defed_ineffect_md = true; }
};

//Def|Use information computation flag.
enum DUCOMP_FLAG {
    COMP_UNDEF = 0x0,
    //Recompute MD reference, completely needs POINT-TO info.
    COMP_EXP_RECOMPUTE = 0x1,
    COMP_EXP_UPDATE_DU = 0x2,
    COMP_EXP_COLLECT_MUST_USE = 0x4,
};

enum DUOPT_FLAG {
    DUOPT_UNDEF = 0x0,
    DUOPT_SOL_AVAIL_REACH_DEF = 0x1, //compute MustAvailable ReachDefinition.
    DUOPT_SOL_REACH_DEF = 0x2, //compute MayAvailable ReachDefinition.
    DUOPT_SOL_AVAIL_EXPR = 0x4, //compute MustAvailable expression.
    //compute Region referenced MayDef/MayUse MDSet.
    DUOPT_SOL_REGION_REF = 0x8,
    DUOPT_COMPUTE_PR_DU = 0x10, //compute PR DU chain.
    DUOPT_COMPUTE_NONPR_DU = 0x20, //compute Non-PR DU chain.
    DUOPT_COMPUTE_PR_REF = 0x40, //compute PR stmt/exp referenced MD/MDSet.
    //compute Non-PR stmt/exp referenced MD/MDSet.
    DUOPT_COMPUTE_NONPR_REF = 0x80,
};

class CompFlag : public UFlag {
public:
    CompFlag(UINT v) : UFlag(v) {}
};

class DUOptFlag : public UFlag {
public:
    DUOptFlag(UINT v) : UFlag(v) {}
    bool do_nothing() const { return only_have(DUOPT_UNDEF); }
};

//Def|Use information manager.
class DUMgr : public Pass {
    COPY_CONSTRUCTOR(DUMgr);
    friend class MD2IRSet;
    friend class DUSet;
protected:
    TypeMgr * m_tm;
    VarMgr * m_vm;
    AliasAnalysis * m_aa;
    IRCFG * m_cfg;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    MDSetMgr * m_mds_mgr;
    MDSetHash * m_mds_hash;
    //Used by DU chain.
    xcom::TTab<UINT> * m_is_init; //for tmp use.
    MD2IRSet * m_md2irs; //for tmp use.
    OptCtx * m_oc; //used for tmp, and should be initialized before any use.
    ConstIRIter m_citer; //for tmp use.
    ConstIRIter m_citer2; //for tmp use.
    IRIter m_iter; //for tmp use.
    IRIter m_iter2; //for tmp use.
    ConstMDIter m_tab_iter;

    //Used to cache overlapping MDSet for individual MD.
    TMap<MD const*, MDSet const*> * m_cached_overlap_mdset;

    //Indicate whether MDSet is cached for individual MD.
    DefSBitSetCore * m_is_cached_mdset;

    SolveSetMgr m_solve_set_mgr;
    xcom::DefMiscBitSetMgr m_sbs_mgr;
protected:
    bool buildLocalDUChain(IRBB * bb, IR const* exp,
                           MD const* expmd, DUSet * expdu,
                           IRListIter ct, bool * has_local_nonkilling_def);
    void buildLocalDUChainForNonKillingDef(IRBB * bb,
                                           xcom::C<IR*> const* ct,
                                           IR const* exp, MD const* expmd,
                                           DUSet * expdu);

    void cleanDUSet(UINT irid, DUSet * set);
    void checkDefSetToBuildDUChainPR(IR const* exp, MD const* expmd,
                                     MDSet const* expmds,
                                     DUSet * expdu,
                                     DefSBitSetCore const* defset,
                                     IRBB * curbb);
    void checkDefSetToBuildDUChainNonPR(IR const* exp, MD const* expmd,
                                        MDSet const* expmds,
                                        DUSet * expdu,
                                        DefSBitSetCore const* defset,
                                        IRBB * curbb);
    bool checkIsLocalKillingDefForDirectAccess(MD const* defmd,
                                               MD const* usemd,
                                               IR const* stmt,
                                               bool * has_nonkilling_local_def);
    void checkDefSetToBuildDUChain(IR const* exp, MD const* expmd,
                                   MDSet const* expmds, DUSet * expdu,
                                   DefSBitSetCore const* defset,
                                   IRBB * curbb);
    void checkMDSetAndBuildDUChain(IR const* exp, MD const* expmd,
                                   MDSet const& expmds, DUSet * expdu);
    void checkMustMDAndBuildDUChainForPotentialDefList(IR const* exp,
                                                       MD const* expmd,
                                                       DUSet * expdu);
    bool checkIsLocalKillingDefForDirectAccess(MD const* defmd, MD const* usemd,
                                               IR const* stmt);
    UINT checkIsLocalKillingDefForIndirectAccess(IR const* stmt, IR const* exp,
                                                 xcom::C<IR*> const* expct);
    UINT checkIsNonLocalKillingDef(IR const* stmt, IR const* exp);
    inline bool canBeLiveExprCand(IR const* ir) const;
    void computeOverlapSetForWorstCase();
    void computeArrayRefAtIStoreBase(IR * ir);
    virtual void computeExtExpression(IR * ir, OUT MDSet * ret_mds,
                                      CompFlag compflag, DUOptFlag duflag);
    void computeExpressionList(IR * ir, OUT MDSet * ret_mds,
                               CompFlag compflag, DUOptFlag duflag);
    void computeExpression(IR * ir, MDSet * ret_mds, CompFlag compflag,
                           DUOptFlag duflag);
    void computeArrayRef(IR * ir, OUT MDSet * ret_mds, CompFlag compflag,
                         DUOptFlag duflag);
    void computeLiveInBB(DefMiscBitSetMgr & bsmgr);
    void checkAndBuildChainForMemOp(IRBB * bb, IR * exp, IRListIter ct);
    void checkAndBuildChainRecursiveIRList(IRBB * bb, IR * exp, IRListIter ct,
                                           DUOptFlag flag);
    void checkAndBuildChainRecursive(IRBB * bb, IR * exp, IRListIter ct,
                                     DUOptFlag flag);
    void checkAndBuildChainForAllKid(IR * ir, IRBB * bb, IRListIter ct,
                                     DUOptFlag flag);
    virtual void checkAndBuildChainExtStmt(IR * stmt, IRListIter ct,
                                           DUOptFlag flag);
    void checkAndBuildChain(IR * stmt, IRListIter ct, DUOptFlag flag);
    void collectNonPRMayDef(IR const* ir, DefMiscBitSetMgr & bsmgr,
                            OUT MDSet * maydefmds) const;
    void computeMayDef(IR const* ir, MDSet * bb_maydefmds,
                       SolveSet * maygen_stmt,
                       DefMiscBitSetMgr & bsmgr, DUOptFlag flag);
    void computeMustExactDef(IR const* ir, MDSet * bb_mustdefmds,
                             SolveSet * mustgen_stmt,
                             ConstMDIter & mditer,
                             DefMiscBitSetMgr & bsmgr, DUOptFlag flag);
    void computeMustExactDefMayDefMayUseForBB(
        IRBB * bb, ConstMDIter & mditer,
        OUT Vector<MDSet*> * mustdefmds,
        OUT Vector<MDSet*> * maydefmds, OUT MDSet * mayusemds,
        MDSet * bb_mustdefmds, MDSet * bb_maydefmds,
        SolveSet * mustgen_stmt, SolveSet * maygen_stmt,
        DUOptFlag flag, DefMiscBitSetMgr & bsmgr);
    void computeMustExactDefMayDefMayUse(OUT Vector<MDSet*> * mustdef,
                                         OUT Vector<MDSet*> * maydef,
                                         OUT MDSet * mayuse,
                                         DUOptFlag flag);

    void freeDUSetForAllIR();

    void genDummyuseForCallStmt(IR * ir, MDSet const* mayref);

    inline void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, size);
        return p;
    }

    bool hasSingleDefToMD(DUSet const& defset, MD const* md) const;

    virtual void inferExtStmt(IR * ir, DUOptFlag duflag);
    void initMD2IRSet(IRBB const* bb);
    void inferStoreArray(IR * ir, DUOptFlag duflag);
    void inferDirectMemStmt(IR * ir, DUOptFlag duflag);
    void inferIndirectMemStmt(IR * ir, DUOptFlag duflag);
    void inferAllKidMDRef(IR * ir, OUT MDSet * ret_mds,
                          CompFlag compflag, DUOptFlag duflag);
    void inferCallStmtForNonPR(IR * ir, DUOptFlag duflag);
    //Return true if the output result has unify call's MayDef.
    bool inferCallStmtForNonPRViaCallGraph(IR const* ir,
                                           OUT MDSet & maydefuse);
    void inferCallStmt(IR * ir, DUOptFlag duflag);

    //Return true if stmt dominate use's stmt, otherwise return false.
    bool isStmtDomUseInsideLoop(IR const* stmt, IR const* use,
                                LI<IRBB> const* li) const;

    void solveSet(MOD OptCtx & oc, DUOptFlag flag);
    //Set given set to be more conservative MD reference set.
    void setToConservative(OUT MDSet & maydefuset);
    void setToWorstCase(IR * ir);
    DUMgr * self() { return this; }

    void updateDefSetAccordingToMayRef(IR * ir, MD const* mustexact);
    void updateDefWithMustEffectMD(IR * ir, MD const* musteffect);
    void updateDefWithMustExactMD(IR * ir, MD const* mustexact);
    void updateDef(IR * ir, DUOptFlag flag);

    virtual bool verifyMDRefForExtIR(IR const* ir, ConstIRIter & cii);
    bool verifyMDRefForIR(IR const* ir, ConstIRIter & cii);
public:
    explicit DUMgr(Region * rg);
    ~DUMgr();

    //Build DU chain : def->use.
    //Build DU chain from stmt 'def' to expression 'use'.
    //Note the function does NOT check the consistency of Prno if def or use
    //operate on PR.
    void buildDUChain(IR * def, IR * use)
    {
        ASSERT0(def && def->is_stmt() && use && use->is_exp());
        ASSERT0(def->isMemRef() && use->isMemOpnd());
        genDUSet(def)->addUse(use, *getSBSMgr());
        genDUSet(use)->addDef(def, *getSBSMgr());
    }
    //Return true if built DU chain.
    bool buildDUChain(IR * def, IR * use, OptCtx const& oc);

    //Compute the MDSet that might overlap ones which 'ir' defined.
    //e.g: int A[100], there are two referrence of array A: A[i], A[j]
    //    A[i] might overlap A[j].
    //'tmp': regard as input data, compute overlapped MD of its element.
    //NOTE: Be careful 'mds' would be modified.
    //Note if ir has MustRef, the function will only compute the overlapped MD
    //which are overlapped with the MustRef, otherwise it will compute the
    //overlapped MD set which is overlapped to every elements in current MayRef
    //of ir.
    void computeOverlapMDSet(IR * ir, bool recompute);

    //Collect MustUse MDSet for both PR operation and Non-PR operation.
    //e.g: = a + b + *p;
    //    assume p->w,u, the MustUse is {a,b,p}, not include w,u.
    void collectMustUsedMDs(IR const* ir, OUT MDSet & mustuse);

    //The function check user command options and determine whether the
    //compiler should perform classic PR DUChain and NonPR DUChain.
    bool checkAndComputeClassicDUChain(MOD OptCtx & oc);
    void computeGenForBB(IN IRBB * bb, OUT SolveSet & expr_univers,
                         DefMiscBitSetMgr & bsmgr);
    void computeMDRefForBB(IRBB * bb, MOD OptCtx & oc, DUOptFlag duflag);
    void computeMDDUForBB(IRBB const* bb, DUOptFlag flag);
    void computeCallRef(DUOptFlag duflag);
    virtual void computeAtomMDRef(IR * ir);
    void computeMDRef(MOD OptCtx & oc, DUOptFlag duflag);
    void computeKillSet(DefDBitSetCoreReserveTab & dbitsetchash,
                        Vector<MDSet*> const* mustdefs,
                        Vector<MDSet*> const* maydefs,
                        DefMiscBitSetMgr & bsmgr);
    void computeAuxSetForExpression(DefDBitSetCoreReserveTab & dbitsetchash,
                                    OUT SolveSet * expr_universe,
                                    Vector<MDSet*> const* maydefmds,
                                    DefMiscBitSetMgr & bsmgr);
    void computeMDDUChain(MOD OptCtx & oc, bool retain_reach_def,
                          DUOptFlag duflag);
    void computeRegionMDDU(Vector<MDSet*> const* mustdefmds,
                           Vector<MDSet*> const* maydefmds,
                           MDSet const* mayusemds);
    //The function will free DUSet for all IRs in region.
    void cleanDUSet() { freeDUSetForAllIR(); }

    //The function copy MustUse and MayUse mds from tree 'from' to tree 'to'
    //and build new DU chain for 'to'.
    //The function will establish new DU chain between the DEF of 'from'
    //and 'to'.
    //to: root expression of target tree.
    //from: root expression of source tree.
    //NOTE: IR tree 'to' and 'from' must be isomorphic structure.
    //Both 'to' and 'from' must be expression.
    void addUseForTree(IR * to, IR const* from);

    //Count the memory usage to DUMgr.
    size_t count_mem() const;
    size_t count_mem_duset();
    size_t count_mem_local_data(SolveSet * expr_univers,
                                Vector<MDSet*> * maydef_mds,
                                Vector<MDSet*> * mustexactdef_mds,
                                MDSet * mayuse_mds,
                                MDSet mds_arr_for_must[],
                                MDSet mds_arr_for_may[],
                                UINT elemnum);

    //Collect must and may memory reference.
    static void collectMayUseRecursive(IR const* ir, Region const* rg,
                                       bool comp_pr, DefMiscBitSetMgr & bsmgr,
                                       OUT MDSet & mayuse);
    static void collectMayUseRecursiveIRList(IR const* ir, Region const* rg,
                                             bool comp_pr,
                                             DefMiscBitSetMgr & bsmgr,
                                             OUT MDSet & mayuse);

    //Collect may memory reference.
    void collectMayUse(IR const* ir, MDSet & mayuse, bool comp_pr);

    //DU chain operation.
    //Change Def stmt from 'from' to 'to'.
    //'to': copy to stmt's id.
    //'from': copy from stmt's id.
    //'useset': each element is USE, it is the USE expression set of 'from'.
    //e.g: from->USE change to to->USE.
    void changeDef(UINT to, UINT from,  DUSet * useset_of_to,
                   DUSet * useset_of_from, DefMiscBitSetMgr * m)
    {
        ASSERT0(m_rg->getIR(from)->is_stmt() &&
                m_rg->getIR(to)->is_stmt() &&
                useset_of_to && useset_of_from && m);
        if (to == from) { return; }
        DUSetIter di = nullptr;
        for (BSIdx i = useset_of_from->get_first(&di);
             di != nullptr; i = useset_of_from->get_next(i, &di)) {
            IR const* exp = m_rg->getIR(i);
            ASSERT0(exp->is_exp() && exp->isMemRef());
            DUSet * defset = exp->getDUSet();
            if (defset == nullptr) { continue; }
            defset->diff(from, *getSBSMgr());
            defset->bunion(to, *getSBSMgr());
        }
        useset_of_to->bunion(*useset_of_from, *m);
        useset_of_from->clean(*m);
    }

    //DU chain operation.
    //Change Def stmt from 'from' to 'to'.
    //'to': target stmt.
    //'from': source stmt.
    //e.g: from->USE change to to->USE.
    inline void changeDef(IR * to, IR * from)
    {
        ASSERT0(to && from && to->is_stmt() && from->is_stmt());
        DUSet * useset_of_from = from->getDUSet();
        if (useset_of_from == nullptr) { return; }
        DUSet * useset_of_to = genDUSet(to);
        changeDef(to->id(), from->id(), useset_of_to, useset_of_from,
                  getSBSMgr());
    }

    //DU chain operation.
    //Change Use expression from 'from' to 'to'.
    //'to': indicate the target expression which copy to.
    //'from': indicate the source expression which copy from.
    //'defset': it is the DEF stmt set of 'from'.
    //e.g: DEF->from change to DEF->to.
    void changeUse(UINT to, UINT from,  DUSet * defset_of_to,
                   DUSet * defset_of_from, DefMiscBitSetMgr * m)
    {
        ASSERT0(m_rg->getIR(from)->is_exp() && m_rg->getIR(to)->is_exp() &&
                defset_of_from && defset_of_to && m);
        if (to == from) { return; }
        DUSetIter di = nullptr;
        for (BSIdx i = defset_of_from->get_first(&di);
             di != nullptr; i = defset_of_from->get_next(i, &di)) {
            IR * stmt = m_rg->getIR(i);
            ASSERT0(stmt->is_stmt());
            DUSet * useset = stmt->getDUSet();
            if (useset == nullptr) { continue; }
            useset->diff(from, *getSBSMgr());
            useset->bunion(to, *getSBSMgr());
        }
        defset_of_to->bunion(*defset_of_from, *m);
        defset_of_from->clean(*m);
    }

    //DU chain operation.
    //Change Use expression from 'from' to 'to'.
    //'to': indicate the exp which copy to.
    //'from': indicate the expression which copy from.
    //e.g: change DEF->from to be DEF->to.
    void changeUse(IR * to, IR const* from)
    {
        ASSERT0(to && from && to->is_exp() && from->is_exp());
        DUSet * defset_of_from = from->getDUSet();
        if (defset_of_from == nullptr) { return; }
        DUSet * defset_of_to = genDUSet(to);
        changeUse(to->id(), from->id(), defset_of_to, defset_of_from,
                  getSBSMgr());
    }

    //Coalesce DU chain of 'from' to 'to'.
    //The function replace definition of USE of 'from' to DEF of 'to'.
    //e.g: to_def=...
    //     from=to
    //     ...=from_use
    //=> after coalescing, p1 is src, p0 is tgt
    //     to_def=...
    //     ------ //removed
    //     ...=to
    //from: stmt
    //to: expression
    void coalesceDUChain(IR const* from, IR const* to);

    void dumpMemUsageForMDRef() const;
    void dumpDUChain() const;
    void dumpDUChainDetail() const;
    void dumpBBDUChainDetail(UINT bbid) const;
    void dumpBBDUChainDetail(IRBB * bb) const;

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "DU Manager"; }
    virtual PASS_TYPE getPassType() const { return PASS_DU_MGR; }

    //Try allocate DUSet for memory reference.
    DUSet * genDUSet(IR * ir);
    //Get sparse bitset mgr.
    xcom::DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }
    SolveSetMgr * getSolveSetMgr() { return &m_solve_set_mgr; }
    IR const* getExactAndUniqueDef(IR const* exp) const;

    //Return true if 'def' may or must modify MDSet that 'use' referenced.
    //'def': STPR stmt.
    //'use': must be expression.
    //'is_recur': true if one intend to compute the mayuse MDSet to walk
    //            through IR tree recusively.
    bool isStprMayDef(IR const* def, IR const* use, bool is_recur);

    //Return true if 'call' may or must modify MDSet that 'use' referenced.
    //'call': CALL/ICALL stmt.
    //'use': must be expression.
    //'is_recur': true if one intend to compute the mayuse MDSet to walk
    //            through IR tree recusively.
    bool isCallMayDef(IR const* def, IR const* use, bool is_recur);

    //Return true if 'def' may or must modify MDSet that 'use' referenced.
    //'def': must be stmt.
    //'use': must be expression.
    //'is_recur': true if one intend to compute the mayuse MDSet to walk
    //            through IR tree recusively.
    bool isMayDef(IR const* def, IR const* use, bool is_recur);

    //Return true if 'def_stmt' is the exact and unique reach-definition
    //to the operands of 'use_stmt', otherwise return false.
    //def_stmt: should be stmt.
    //use_stmt: should be stmt.
    bool isExactAndUniqueDef(IR const* def, IR const* exp);

    //Return true if ir dominates all its USE expressions which inside loop.
    bool isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const;

    //The equation needs May Kill Def and Must Gen Def.
    bool ForAvailReachDef(UINT bbid, List<IRBB*> & preds, List<IRBB*> * lst,
                          DefMiscBitSetMgr & bsmgr);
    bool ForReachDef(UINT bbid, List<IRBB*> & preds, List<IRBB*> * lst,
                     DefMiscBitSetMgr & bsmgr);
    bool ForAvailExpression(UINT bbid, List<IRBB*> & preds, List<IRBB*> * lst,
                            DefMiscBitSetMgr & bsmgr);

    //Find the nearest dominated DEF stmt of 'exp'.
    //NOTE: RPO of bb of stmt must be available.
    //exp: expression
    //defset: def set of exp.
    //omit_self: true if we do not consider the 'exp_stmt' itself.
    IR * findNearestDomDef(IR const* exp, DUSet const* defset) const;
    IR * findNearestDomDef(IR const* exp, IRSet const* defset) const
    { return findNearestDomDef(exp, (DUSet const*)defset); }
    IR * findNearestDomDef(IR const* exp) const
    { return findNearestDomDef(exp, exp->getDUSet()); }

    //Find nearest killing def to expmd in its bb.
    //Here we search exactly killing DEF from current stmt to previous
    //for expmd even if it is exact,
    //has_overlapped_def: record if find local non-killing def(overlapped).
    //
    //e.g: g is global variable, it is exact.
    //x is a pointer that we do not know where it pointed to.
    //    1. *x += 1;
    //    2. g = 0;
    //    3. *x += 2; # *x may overlapped with global variable g.
    //    4. return g;
    //In the case, the last reference of g in stmt 4 may be defined by
    //stmt 2 or 3.
    IR const* findKillingLocalDef(IRBB * bb, xcom::C<IR*> const* ct,
                                  IR const* exp, MD const* md,
                                  bool * has_local_nonkilling_def);
    //Find the unique DEF of 'exp' that is inside given loop.
    //Note the DEF may not be killing-def of 'exp'.
    static IR * findUniqueDefInLoopForMustRef(IR const* exp, LI<IRBB> const* li,
                                              Region const* rg,
                                              OUT IRSet * set = nullptr);

    //Find all USEs of stmt that is inside the given loop.
    //useset: record USEs that is inside the loop as return result.
    static bool findUseInLoop(IR const* stmt, LI<IRBB> const* li,
                              Region const* rg, OUT IRSet * useset);

    void setMustKilledDef(UINT bbid, SolveSet * set);
    void setMayKilledDef(UINT bbid, SolveSet * set);
    void setKilledIRExpr(UINT bbid, SolveSet * set);

    //DU chain operation.
    //Cut off the chain bewteen 'def' and 'use'.
    //The related function is buildDUChain().
    inline void removeDUChain(IR const* def, IR const* use)
    {
        ASSERT0(def->is_stmt() && use->is_exp());
        DUSet * useset = def->getDUSet();
        if (useset != nullptr) { useset->remove(IR_id(use), *getSBSMgr()); }

        DUSet * defset= use->getDUSet();
        if (defset != nullptr) { defset->remove(def->id(), *getSBSMgr()); }
    }

    //The function will remove DU Chain for all IRs.
    void removeAllDUChain() { cleanDUSet(); }

    //Check if the DEF of stmt's operands still modify the same memory object.
    //e.g: Revise DU chain if stmt's rhs has been changed.
    //    x=10 //S1
    //    ...
    //    c=x*0 //S2
    //after changed =>
    //    x=10 //S1
    //    ...
    //    c=0 //S2
    //where S1 is DEF, S2 is USE, after ir refinement, x in S2
    //is removed, remove the data dependence between S1
    //and S2's operand.
    bool removeExpiredDUForOperand(IR const* stmt);

    //Check each DEF of exp, remove the expired stmt which is not the memory
    //any more that exp referenced.
    //Check if the DEF of stmt's operands still modify the same memory object.
    //Return true if DU changed.
    //e.g: Revise DU chain if stmt's rhs has been changed.
    //    x=10 //S1
    //    ...
    //    c=x*0 //S2
    //after changed =>
    //    x=10 //S1
    //    ...
    //    c=0 //S2
    //where S1 is DEF, S2 is USE, after ir refinement, x in S2
    //is removed, remove the data dependence between S1
    //and S2's operand.
    //Note the function only process ir, not include its kid and sibling.
    bool removeExpiredDU(IR const* ir);

    //The function handles entire IR tree rooted by 'stmt'.
    //Check if the DEF of stmt's operands still modify the same memory object.
    //Check each kid of stmt, remove the expired one which is not reference
    //the memory any more that stmt defined.
    //Return true if DU changed.
    //e.g: Revise DU chain if stmt or stmt's rhs has been changed.
    //    x=10 //S1
    //    ...
    //    c=x*0 //S2
    //after changed =>
    //    x=10 //S1
    //    ...
    //    c=0 //S2
    //where S1 is DEF, S2 is USE, after ir refinement, x in S2
    //is removed, remove the data dependence between S1
    //and S2's operand.
    bool removeExpiredDUIRTree(IR const* stmt);

    //Remove 'def' out of ir's DEF set. ir is exp.
    void removeDef(IR const* ir, IR const* def);

    //Remove all DU info of 'ir' from DU mgr.
    void removeIRFromDUMgr(IR const* ir);

    //The function check all USE of memory references of ir tree and
    //cut its du-chain. 'ir' may be stmt or expression, if ir is stmt,
    //check its right-hand-side.
    //The function will process SSA info if it exists.
    //'ir': indicate the root of IR tree.
    //e.g: d1, d2 are def-stmt of stmt's operands.
    //the functin cut off du-chain between d1, d2 and their use.
    void removeUseFromDefset(IR const* ir);

    //Remove Use-Def chain.
    //exp: the expression to be removed.
    //e.g: ir = ...
    //    = ir //S1
    //If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
    //NOTE: the function only process exp itself.
    void removeUse(IR const* ir);

    //The function only handle Call/ICall stmt, it find PR and remove
    //them out of UseSet.
    //Note the function does not maintain DU chain between call and its use.
    void removePRFromDUSet(IR const* ir);

    //Note that do NOT use the function to remove SSA def.
    //The function handle the MD DU chain and cut
    //off the DU chain between MD def and its MD use expression.
    //Remove 'def' from its use's def-list.
    //e.g:u1, u2 are its use expressions.
    //cut off the du chain between def->u1 and def->u2.
    void removeDefFromUseset(IR const* def);

    //Verify MD reference to each stmts and expressions which described memory.
    bool verifyMDRef();
    bool verifyLiveinExp();

    //Verify if DU chain is correct between each Def and Use of MD.
    bool verifyMDDUChainForIR(IR const* ir, DUOptFlag duflag);

    virtual bool perform(OptCtx &)
    {
        UNREACHABLE();
        return false;
    }
    bool perform(MOD OptCtx & oc,
                 DUOptFlag f = DUOptFlag(DUOPT_SOL_AVAIL_REACH_DEF|
                    DUOPT_SOL_AVAIL_EXPR|
                    DUOPT_SOL_REACH_DEF|DUOPT_COMPUTE_PR_REF|
                    DUOPT_COMPUTE_NONPR_REF|DUOPT_SOL_REGION_REF));
};

//Verify DU chain's sanity.
//Verify if DU chain is correct between each Def and Use of MD.
bool verifyMDDUChain(Region * rg, DUOptFlag duflag);
bool verifyMDDUChain(Region * rg, OptCtx const& oc);

} //namespace xoc
#endif
