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
// IR may reference MD, or MDSet, or both MD and MDSet.
//
//    computeOverlapDefMDSet
//    computeOverlapUseMDSet
//    collectMayUse
//    collectMayUseRecursive
//    copyRefAndAddDUChain
//    changeUse
//    changeDef
//    getMayDef
//    getMustUse
//    getMayUse
//    get_must_def
//    getEffectRef
//    get_effect_def_md
//    get_exact_def_md
//    get_effect_use_md
//    get_exact_use_md
//    getExactAndUniqueDef
//    freeDUSetAndCleanMDRefs
//    isMayDef
//    isMayKill
//    isMustKill
//    isExactAndUniqueDef
//    isCallMayDef
//
//* These functions manipulate the DU chain.
//
//    buildDUChain
//    freeDUSetAndCleanMDRefs
//    copyDUSet
//    get_du_c
//    getAndAllocDUSet
//    is_du_exist
//    unionUse
//    unionUseSet
//    unionDef
//    unionDefSet
//    removeDUChain
//    removeExpiredDU
//    removeDef
//    removeUseFromDefset
//    removeDefFromUseset
//    removeIRFromDUMgr

//Mapping from IR to index.
typedef HMap<IR const*, UINT, HashFuncBase2<IR const*> > IR2UINT;

class DUMgr;
class AliasAnalysis;

//Mapping from MD to IR list, and to be responsible for
//allocating and destroy List<IR*> objects.
class MD2IRSet : public TMap<UINT, DefSBitSetCore*> {
    COPY_CONSTRUCTOR(MD2IRSet);
    Region * m_rg;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    DUMgr * m_du;
    xcom::DefMiscBitSetMgr * m_misc_bs_mgr;
    xcom::TMapIter<UINT, DefSBitSetCore*> m_iter;
    xcom::DefSBitSetCore m_global_md;

    //Indicate if there exist stmt which only have MayDef.
    bool m_are_stmts_defed_ineffect_md;

public:
    explicit MD2IRSet(Region * rg);
    ~MD2IRSet();

    void append(MD const* md, IR const* ir)
    {
        ASSERT0(ir);
        append(MD_id(md), ir->id());
    }
    void append(MD const* md, UINT irid) { append(MD_id(md), irid); }
    void append(UINT mdid, IR * ir)
    {
        ASSERT0(ir);
        append(mdid, ir->id());
    }
    void append(UINT mdid, UINT irid);

    void clean();

    void dump() const;

    Region * getRegion() const { return m_rg; }

    bool hasIneffectDef() const { return m_are_stmts_defed_ineffect_md; }

    void set(UINT mdid, IR * ir);
    void setHasIneffectDef() { m_are_stmts_defed_ineffect_md = true; }
};


//Ud chains describe all of the might uses of the prior DEFINITION of md.
//Du chains describe all effective USEs of once definition of md.
//e.g:
//  d1:a=   d2:a=   d3:a=
//  \      |      /
//     b   =   a
//     |       |
//  d4:..=b      d5:...=b
//  Ud chains:  a use d1,d2,d3 stmt
//  Du chains:  b's value used by d4,d5 stmt
//If ir is stmt, this class indicates a set of USE expressions.
//If ir is expression, this class indicates a set of DEF stmts.

class DefDBitSetCoreHashAllocator {
    COPY_CONSTRUCTOR(DefDBitSetCoreHashAllocator);
    DefMiscBitSetMgr * m_sbs_mgr;

public:
    DefDBitSetCoreHashAllocator(DefMiscBitSetMgr * sbsmgr)
    { ASSERT0(sbsmgr); m_sbs_mgr = sbsmgr; }

    DefSBitSetCore * alloc() { return m_sbs_mgr->allocDBitSetCore(); }

    void free(DefSBitSetCore * set)
    { m_sbs_mgr->freeDBitSetCore((DefDBitSetCore*)set); }

    DefMiscBitSetMgr * getBsMgr() const { return m_sbs_mgr; }
};


#define HASH_DBITSETCORE

class DefDBitSetCoreReserveTab : public
    SBitSetCoreHash<DefDBitSetCoreHashAllocator> {
    COPY_CONSTRUCTOR(DefDBitSetCoreReserveTab);
    #ifdef HASH_DBITSETCORE
    #else
    List<DefDBitSetCore*> m_allocated;
    #endif

public:
    DefDBitSetCoreReserveTab(DefDBitSetCoreHashAllocator * allocator) :
        SBitSetCoreHash<DefDBitSetCoreHashAllocator>(allocator) {}
    virtual ~DefDBitSetCoreReserveTab()
    {
        #ifdef HASH_DBITSETCORE
        #else
        xcom::C<DefDBitSetCore*> * ct;
        for (m_allocated.get_head(&ct);
             ct != m_allocated.end(); ct = m_allocated.get_next(ct)) {
            DefDBitSetCore * s = ct->val();
            ASSERT0(s);
            get_allocator()->free(s);
        }
        #endif
    }

    DefDBitSetCore const* append(DefDBitSetCore const& set)
    {
        #ifdef HASH_DBITSETCORE
        return (DefDBitSetCore const*)SBitSetCoreHash
               <DefDBitSetCoreHashAllocator>::append(set);
        #else
        DefDBitSetCore * s = (DefDBitSetCore*)m_allocator->alloc();
        ASSERT0(s);
        m_allocated.append_tail(s);
        s->copy(set, *m_allocator->getBsMgr());
        return s;
        #endif
    }

    void dump(FILE * h) const
    {
        #ifdef HASH_DBITSETCORE
        dump_hashed_set(h);
        #else
        if (h == nullptr) { return; }
        fprintf(h, "\n==---- DUMP DefDBitSetCoreReserveTab ----==");
        xcom::C<DefDBitSetCore*> * ct;
        for (m_allocated.get_head(&ct);
             ct != m_allocated.end(); ct = m_allocated.get_next(ct)) {
            DefDBitSetCore * s = ct->val();
            ASSERT0(s);
            fprintf(h, "\n");
            s->dump(h);
        }
        fflush(h);
        #endif
    }
};


//Mapping from IR to DUSet.
typedef HMap<IR*, DUSet*> IR2DU;

//Def|Use information manager.
#define COMP_EXP_RECOMPUTE 1 //Recompute MD reference, completely
                             //needs POINT-TO info.
#define COMP_EXP_UPDATE_DU 2
#define COMP_EXP_COLLECT_MUST_USE 4

#define DUOPT_UNDEF 0
#define DUOPT_SOL_AVAIL_REACH_DEF 0x1 //compute Must-Available Reach-definition.
#define DUOPT_SOL_REACH_DEF 0x2 //compute May-Available Reach-definition.
#define DUOPT_SOL_AVAIL_EXPR 0x4 //compute Must-Available expression.
//compute Region referenced MayDef/MayUse MDSet.
#define DUOPT_SOL_REGION_REF 0x8
#define DUOPT_COMPUTE_PR_DU 0x10 //compute PR DU chain.
#define DUOPT_COMPUTE_NONPR_DU 0x20 //compute Non-PR DU chain.
#define DUOPT_COMPUTE_PR_REF 0x40 //compute PR stmt/exp referenced MD/MDSet.
//compute Non-PR stmt/exp referenced MD/MDSet.
#define DUOPT_COMPUTE_NONPR_REF 0x80

class DUMgr : public Pass {
    COPY_CONSTRUCTOR(DUMgr);
    friend class MD2IRSet;
    friend class DUSet;
protected:
    Region * m_rg;
    TypeMgr * m_tm;
    AliasAnalysis * m_aa;
    IRCFG * m_cfg;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    MDSetMgr * m_mds_mgr;
    MDSetHash * m_mds_hash;
    xcom::DefMiscBitSetMgr * m_misc_bs_mgr;

    ConstIRIter m_citer; //for tmp use.
    ConstIRIter m_citer2; //for tmp use.
    IRIter m_iter; //for tmp use.
    IRIter m_iter2; //for tmp use.
    ConstMDIter m_tab_iter;

    //Used to cache overlapping MDSet for specified MD.
    TMap<MD const*, MDSet const*> m_cached_overlap_mdset;

    //Indicate whether MDSet is cached for specified MD.
    DefSBitSetCore m_is_cached_mdset;

    //Used by DU chain.
    xcom::BitSet * m_is_init;
    MD2IRSet * m_md2irs;
    OptCtx * m_oc;

    //Available reach-def computes the definitions
    //which must be the last definition of result variable,
    //but it may not reachable meanwhile.
    //e.g:
    //    BB1:
    //    a=1  //S1
    //    *p=3
    //    a=4  //S2
    //    goto BB3
    //
    //    BB2:
    //    a=2 //S3
    //    goto BB3
    //
    //    BB3:
    //    f(a)
    //
    //    Here we do not known where p pointed to.
    //    The available-reach-def of BB3 is {S1, S3}
    //
    //Compare to available reach-def, reach-def computes the definition
    //which may-live at each BB.
    //e.g:
    //    BB1:
    //    a=1  //S1
    //    *p=3
    //    goto BB3
    //
    //    BB2:
    //    a=2 //S2
    //    goto BB3
    //
    //    BB3:
    //    f(a)
    //
    //    Here we do not known where p pointed to.
    //    The reach-def of BB3 is {S1, S2}
    //avail reach-in def of STMT
    Vector<DefDBitSetCore*> m_bb_avail_in_reach_def;
    //avail reach-out def of STMT
    Vector<DefDBitSetCore*> m_bb_avail_out_reach_def;
    Vector<DefDBitSetCore*> m_bb_in_reach_def; //reach-in def of STMT
    Vector<DefDBitSetCore*> m_bb_out_reach_def; //reach-out def of STMT
    Vector<DefDBitSetCore*> m_bb_may_gen_def; //generated-def of STMT
    Vector<DefDBitSetCore*> m_bb_must_gen_def; //generated-def of STMT
    //must-killed def of STMT
    Vector<DefDBitSetCore const*> m_bb_must_killed_def;
    Vector<DefDBitSetCore const*> m_bb_may_killed_def; //may-killed def of STMT
    Vector<DefSBitSetCore*> m_livein_bb; //live-in BB
    BSVec<DefDBitSetCore*> m_bb_gen_exp; //generate EXPR
    BSVec<DefDBitSetCore const*> m_bb_killed_exp; //killed EXPR
    BSVec<DefDBitSetCore*> m_bb_availin_exp; //available-in EXPR
    BSVec<DefDBitSetCore*> m_bb_availout_ir_expr; //available-out EXPR

protected:
    bool buildLocalDUChain(IRBB * bb,
                           IR const* exp,
                           MD const* expmd,
                           DUSet * expdu,
                           IRListIter ct,
                           bool * has_local_nonkilling_def);
    void buildLocalDUChainForNonKillingDef(IRBB * bb,
                                           xcom::C<IR*> const* ct,
                                           IR const* exp,
                                           MD const* expmd,
                                           DUSet * expdu);

    bool checkIsLocalKillingDefForDirectAccess(MD const* defmd,
                                               MD const* usemd,
                                               IR const* stmt,
                                               bool * has_nonkilling_local_def);
    void checkDefSetToBuildDUChain(IR const* exp,
                                   MD const* expmd,
                                   MDSet const* expmds,
                                   DUSet * expdu,
                                   DefSBitSetCore const* defset,
                                   IRBB * curbb);
    void checkMDSetAndBuildDUChain(IR const* exp,
                                   MD const* expmd,
                                   MDSet const& expmds,
                                   DUSet * expdu);
    void checkMustMDAndBuildDUChainForPotentialDefList(IR const* exp,
                                                       MD const* expmd,
                                                       DUSet * expdu);    
    bool checkIsLocalKillingDefForDirectAccess(MD const* defmd,
                                               MD const* usemd,
                                               IR const* stmt);
    UINT checkIsLocalKillingDefForIndirectAccess(IR const* stmt,
                                                 IR const* exp,
                                                 xcom::C<IR*> const* expct);
    UINT checkIsNonLocalKillingDef(IR const* stmt, IR const* exp);
    inline bool canBeLiveExprCand(IR const* ir) const;
    void computeArrayRefAtIStoreBase(IR * ir);
    void computeExpression(IR * ir,
                           MDSet * ret_mds,
                           UINT compflag,
                           UINT duflag);
    void computeArrayRef(IR * ir,
                         OUT MDSet * ret_mds,
                         UINT compflag,
                         UINT duflag);
    void computeLiveInBB(DefMiscBitSetMgr & bsmgr);
    void checkAndBuildChainForMemIR(IRBB * bb, IR * exp, IRListIter ct);
    void checkAndBuildChainRecursiveIRList(IRBB * bb,
                                           IR * exp,
                                           IRListIter ct,
                                           UINT flag);
    void checkAndBuildChainRecursive(IRBB * bb,
                                     IR * exp,
                                     IRListIter ct,
                                     UINT flag);
    void checkAndBuildChain(IR * stmt, IRListIter ct, UINT flag);
    void computeMayDef(IR const* ir,
                       MDSet * bb_maydefmds,
                       DefDBitSetCore * maygen_stmt,
                       DefMiscBitSetMgr & bsmgr,
                       UINT flag);
    void computeMustExactDef(IR const* ir,
                             MDSet * bb_mustdefmds,
                             DefDBitSetCore * mustgen_stmt,
                             ConstMDIter & mditer,
                             DefMiscBitSetMgr & bsmgr,
                             UINT flag);
    void computeMustExactDefMayDefMayUse(OUT Vector<MDSet*> * mustdef,
                                         OUT Vector<MDSet*> * maydef,
                                         OUT MDSet * mayuse,
                                         UINT flag,
                                         DefMiscBitSetMgr & bsmgr);

    DefDBitSetCore * genMayGenDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * genMustGenDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * genAvailOutReachDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * genOutReachDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * genGenIRExpr(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * genAvailOutExpr(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore const* getMustKilledDef(UINT bbid) const;
    DefDBitSetCore const* getMayKilledDef(UINT bbid) const;
    DefDBitSetCore const* getKilledIRExpr(UINT bbid) const;

    inline void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }

    bool hasSingleDefToMD(DUSet const& defset, MD const* md) const;

    bool isOverlapDefUse(MD const* mustdef,
                         MDSet const* maydef,
                         IR const* use);
    void initMD2IRSet(IRBB * bb);
    void inferRegion(IR * ir, bool ruinfo_avail, IN MDSet * tmp);
    void inferIStore(IR * ir, UINT duflag);
    void inferStore(IR * ir, UINT duflag);
    void inferStorePR(IR * ir, UINT duflag);
    void inferGetelem(IR * ir, UINT duflag);
    void inferSetelem(IR * ir, UINT duflag);
    void inferStoreArray(IR * ir, UINT duflag);
    void inferPhi(IR * ir, UINT duflag);
    void inferCallAndICall(IR * ir, UINT duflag, IN MD2MDSet * mx);

    void solve(DefDBitSetCore const& expr_universe,
               UINT const flag,
               DefMiscBitSetMgr & bsmgr);
    void resetLocalAuxSet(DefMiscBitSetMgr & bsmgr);
    void resetLiveInBB(DefMiscBitSetMgr & bsmgr);
    void resetAvailReachDefInSet(bool cleanMember);
    void resetAvailExpInSet(bool cleanMember);
    void resetReachDefInSet(bool cleanMember);
    void resetGlobalSet(bool cleanMember);
    void updateDefWithMustEffectMD(IR * ir, MD const* musteffect);
    void updateDefWithMustExactMD(IR * ir, MD const* mustexact);
    void updateDef(IR * ir, UINT flag);    
public:
    explicit DUMgr(Region * rg);
    ~DUMgr();

    //Build DU chain : def->use.
    void buildDUChain(IR * def, IR * use)
    {
        ASSERT0(def && def->is_stmt() && use && use->is_exp());
        ASSERT0(def->isMemoryRef() && use->isMemoryOpnd());
        getAndAllocDUSet(def)->addUse(use, *m_misc_bs_mgr);
        getAndAllocDUSet(use)->addDef(def, *m_misc_bs_mgr);
    }

    //Compute the MDSet that might overlap ones which 'ir' defined.
    //e.g: int A[100], there are two referrence of array A: A[i], A[j]
    //    A[i] might overlap A[j].
    //'tmp': regard as input data, compute overlapped MD of its element.
    //NOTE: Be careful 'mds' would be modified.
    void computeOverlapDefMDSet(IR * ir, bool recompute)
    { computeOverlapUseMDSet(ir, recompute); }

    void computeOverlapUseMDSet(IR * ir, bool recompute);

    //Collect MustUse MDSet for both PR operation and Non-PR operation.
    //e.g: = a + b + *p;
    //    assume p->w,u, the MustUse is {a,b,p}, not include w,u.
    void collectMustUsedMDs(IR const* ir, OUT MDSet & mustuse);

    void computeGenForBB(IN IRBB * bb,
                         OUT DefDBitSetCore & expr_univers,
                         DefMiscBitSetMgr & bsmgr);
    void computeMDDUforBB(IRBB * bb, UINT flag);
    void computeCallRef(UINT duflag);
    virtual void computeAtomMDRef(IR * ir);
    void computeMDRef(IN OUT OptCtx & oc, UINT duflag);
    void computeKillSet(DefDBitSetCoreReserveTab & dbitsetchash,
                        Vector<MDSet*> const* mustdefs,
                        Vector<MDSet*> const* maydefs,
                        DefMiscBitSetMgr & bsmgr);
    void computeAuxSetForExpression(DefDBitSetCoreReserveTab & dbitsetchash,
                                    OUT DefDBitSetCore * expr_universe,
                                    Vector<MDSet*> const* maydefmds,
                                    DefMiscBitSetMgr & bsmgr);
    void computeMDDUChain(IN OUT OptCtx & oc,
                          bool retain_reach_def,
                          UINT duflag);
    void computeRegionMDDU(Vector<MDSet*> const* mustdefmds,
                           Vector<MDSet*> const* maydefmds,
                           MDSet const* mayusemds);

    //DU chain and Memory Object reference operation.
    //This function copy MustUse and MayUse mds from tree 'from' to tree 'to'
    //and build new DU chain for 'to'.
    //add_duchain: if true to add DU chain from tree 'from' to tree 'to'.
    //  this operation will establish new DU chain between the DEF of 'from'
    //  and 'to'.
    //'to': root expression of target tree.
    //'from': root expression of source tree.
    //NOTE: IR tree 'to' and 'from' must be identical structure.
    //'to' and 'from' must be expression.
    void copyRefAndAddDUChain(IR * to, IR const* from, bool add_duchain);

    //Count the memory usage to DUMgr.
    size_t count_mem() const;
    size_t count_mem_duset();
    size_t count_mem_local_data(DefDBitSetCore * expr_univers,
                                Vector<MDSet*> * maydef_mds,
                                Vector<MDSet*> * mustexactdef_mds,
                                MDSet * mayuse_mds,
                                MDSet mds_arr_for_must[],
                                MDSet mds_arr_for_may[],
                                UINT elemnum);

    //Collect must and may memory reference.
    void collectMayUseRecursive(IR const* ir,
                                MDSet & mayUse,
                                bool computePR,
                                DefMiscBitSetMgr & bsmgr);
    void collectMayUseRecursiveIRList(IR const* ir,
                                      OUT MDSet & mayUse,
                                      bool computePR,
                                      DefMiscBitSetMgr & bsmgr);

    //Collect may memory reference.
    void collectMayUse(IR const* ir, MDSet & mayUse, bool computePR);

    //DU chain operation.
    //Copy DUSet from 'src' to 'tgt'. src and tgt must
    //both to be either stmt or expression.
    void copyDUSet(IR * tgt, IR const* src)
    {
        //tgt and src should either both be stmt or both be exp.
        ASSERT0(!(tgt->is_stmt() ^ src->is_stmt()));
        DUSet const* srcduinfo = src->readDUSet();
        if (srcduinfo == nullptr) {
            DUSet * tgtduinfo = tgt->getDUSet();
            if (tgtduinfo != nullptr) {
                tgtduinfo->clean(*m_misc_bs_mgr);
            }
            return;
        }

        DUSet * tgtduinfo = getAndAllocDUSet(tgt);
        tgtduinfo->copy(*srcduinfo, *m_misc_bs_mgr);
    }

    //DU chain operation.
    //Copy DUSet from 'srcset' to 'tgt'.
    //If srcset is empty, then clean tgt's duset.
    inline void copyDUSet(IR * tgt, DUSet const* srcset)
    {
        if (srcset == nullptr) {
            DUSet * tgtduinfo = tgt->getDUSet();
            if (tgtduinfo != nullptr) {
                tgtduinfo->clean(*m_misc_bs_mgr);
            }
            return;
        }

        DUSet * tgtduinfo = getAndAllocDUSet(tgt);
        tgtduinfo->copy(*srcset, *m_misc_bs_mgr);
    }

    //Copy and maintain tgt DU chain.
    //This function will establish tgt's DU chain accroding to src'
    //DU chain information. e.g, The DEF stmt S will be the DEF of tgt.
    //And tgt will be the USE of S.
    void copyDUChain(IR * tgt, IR * src)
    {
        copyDUSet(tgt, src);
        DUSet const* from_du = src->readDUSet();

        DUIter di = nullptr;
        for (UINT i = (UINT)from_du->get_first(&di);
             di != nullptr; i = (UINT)from_du->get_next(i, &di)) {
            IR const* ref = m_rg->getIR(i);
            //ref may be stmt or exp.

            DUSet * ref_defuse_set = ref->getDUSet();
            if (ref_defuse_set == nullptr) { continue; }
            ref_defuse_set->add(IR_id(tgt), *m_misc_bs_mgr);
        }
    }

    //DU chain operation.
    //Change Def stmt from 'from' to 'to'.
    //'to': copy to stmt's id.
    //'from': copy from stmt's id.
    //'useset': each element is USE, it is the USE expression set of 'from'.
    //e.g: from->USE change to to->USE.
    void changeDef(UINT to,
                   UINT from,
                   DUSet * useset_of_to,
                   DUSet * useset_of_from,
                   DefMiscBitSetMgr * m)
    {
        ASSERT0(m_rg->getIR(from)->is_stmt() &&
                m_rg->getIR(to)->is_stmt() &&
                useset_of_to && useset_of_from && m);
        if (to == from) { return; }
        DUIter di = nullptr;
        for (INT i = useset_of_from->get_first(&di);
             di != nullptr; i = useset_of_from->get_next((UINT)i, &di)) {
            IR const* exp = m_rg->getIR((UINT)i);
            ASSERT0(exp->is_exp() && exp->isMemoryRef());
            DUSet * defset = exp->getDUSet();
            if (defset == nullptr) { continue; }

            defset->diff(from, *m_misc_bs_mgr);
            defset->bunion(to, *m_misc_bs_mgr);
        }
        useset_of_to->bunion(*useset_of_from, *m);
        useset_of_from->clean(*m);
    }

    //DU chain operation.
    //Change Def stmt from 'from' to 'to'.
    //'to': target stmt.
    //'from': source stmt.
    //e.g: from->USE change to to->USE.
    inline void changeDef(IR * to, IR * from, DefMiscBitSetMgr * m)
    {
        ASSERT0(to && from && to->is_stmt() && from->is_stmt());
        DUSet * useset_of_from = from->getDUSet();
        if (useset_of_from == nullptr) { return; }

        DUSet * useset_of_to = getAndAllocDUSet(to);
        changeDef(IR_id(to), IR_id(from), useset_of_to, useset_of_from, m);
    }

    //DU chain operation.
    //Change Use expression from 'from' to 'to'.
    //'to': indicate the target expression which copy to.
    //'from': indicate the source expression which copy from.
    //'defset': it is the DEF stmt set of 'from'.
    //e.g: DEF->from change to DEF->to.
    void changeUse(UINT to,
                   UINT from,
                   DUSet * defset_of_to,
                   DUSet * defset_of_from,
                   DefMiscBitSetMgr * m)
    {
        ASSERT0(m_rg->getIR(from)->is_exp() && m_rg->getIR(to)->is_exp() &&
                defset_of_from && defset_of_to && m);
        if (to == from) { return; }
        DUIter di = nullptr;
        for (INT i = defset_of_from->get_first(&di);
             di != nullptr; i = defset_of_from->get_next((UINT)i, &di)) {
            IR * stmt = m_rg->getIR((UINT)i);
            ASSERT0(stmt->is_stmt());
            DUSet * useset = stmt->getDUSet();
            if (useset == nullptr) { continue; }

            useset->diff(from, *m_misc_bs_mgr);
            useset->bunion(to, *m_misc_bs_mgr);
        }
        defset_of_to->bunion(*defset_of_from, *m);
        defset_of_from->clean(*m);
    }

    //DU chain operation.
    //Change Use expression from 'from' to 'to'.
    //'to': indicate the exp which copy to.
    //'from': indicate the expression which copy from.
    //e.g: change DEF->from to be DEF->to.
    inline void changeUse(IR * to, IR const* from, DefMiscBitSetMgr * m)
    {
        ASSERT0(to && from && to->is_exp() && from->is_exp());
        DUSet * defset_of_from = from->getDUSet();
        if (defset_of_from == nullptr) { return; }

        DUSet * defset_of_to = getAndAllocDUSet(to);
        changeUse(IR_id(to), IR_id(from), defset_of_to, defset_of_from, m);
    }

    //Coalesce DU chain of 'from' to 'to'.
    //This function replace definition of USE of 'from' to DEF of 'to'.
    //e.g: to_def=...
    //     from=to
    //     ...=from_use
    //=> after coalescing, p1 is src, p0 is tgt
    //     to_def=...
    //     ------ //removed
    //     ...=to
    //from: stmt
    //to: expression
    void coalesceDUChain(IR * from, IR * to);

    void dumpMemUsageForEachSet() const;
    void dumpMemUsageForMDRef() const;
    void dumpSet(bool is_bs = false) const;
    void dumpDUChain() const;
    void dumpDUChainDetail() const;
    void dumpBBDUChainDetail(UINT bbid) const;
    void dumpBBDUChainDetail(IRBB * bb) const;
    virtual bool dump() const;

    DefSBitSetCore * genLiveInBB(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * genAvailInExpr(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * genInReachDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * genAvailInReachDef(UINT bbid, DefMiscBitSetMgr * mgr);
    virtual CHAR const* getPassName() const { return "DU Manager"; }
    virtual PASS_TYPE getPassType() const { return PASS_DU_MGR; }

    //DU chain operation.
    DUSet * getAndAllocDUSet(IR * ir);
    //Get sparse bitset mgr.
    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_misc_bs_mgr; }
    //Return the MustDef MD.
    MD const* get_must_def(IR const* ir)
    {
        ASSERT0(ir && ir->is_stmt());
        return ir->getRefMD();
    }

    MD const* get_effect_def_md(IR const* ir)
    {
        ASSERT0(ir && ir->is_stmt());
        return ir->getEffectRef();
    }

    //Return exact MD if ir defined.
    inline MD const* get_exact_def_md(IR const* ir)
    {
        ASSERT0(ir && ir->is_stmt());
        MD const* md = get_effect_def_md(ir);
        if (md == nullptr || !md->is_exact()) { return nullptr; }
        return md;
    }

    //Return the MayDef MD set.
    MDSet const* getMayDef(IR const* ir)
    {
        ASSERT0(ir && ir->is_stmt());
        return ir->getRefMDSet();
    }

    //Return the MustUse MD.
    MD const* getMustUse(IR const* ir)
    {
        ASSERT0(ir && ir->is_exp());
        return ir->getEffectRef();
    }

    //Return the MayUse MD set.
    MDSet const* getMayUse(IR const* ir)
    {
        ASSERT0(ir && ir->is_exp());
        return ir->getRefMDSet();
    }

    //Return exact MD if ir defined.
    inline MD const* get_exact_use_md(IR const* ir)
    {
        ASSERT0(ir && ir->is_exp());
        MD const* md = get_effect_use_md(ir);
        return md != nullptr && md->is_exact() ? md : nullptr;
    }

    MD const* get_effect_use_md(IR const* ir)
    {
        ASSERT0(ir && ir->is_exp());
        return ir->getEffectRef();
    }
    IR const* getExactAndUniqueDef(IR const* exp);
    Region * getRegion() const { return m_rg; }

    inline bool is_du_exist(IR const* def, IR const* use) const
    {
        ASSERT0(def->is_stmt() && use->is_exp());
        DUSet const* du = def->readDUSet();
        if (du == nullptr) { return false; }
        return du->is_contain(IR_id(use));
    }

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

    //Return true if 'def1' may modify md-set that 'def2' generated.
    //'def1': should be stmt.
    //'def2': should be stmt.
    bool isMayKill(IR const* def1, IR const* def2);

    //Return true if 'def1' exactly modified md that 'def2' generated.
    //'def1': should be stmt.
    //'def2': should be stmt.
    bool isMustKill(IR const* def1, IR const* def2);

    //Return true if 'def_stmt' is the exact and unique reach-definition
    //to the operands of 'use_stmt', otherwise return false.
    //
    //'def_stmt': should be stmt.
    //'use_stmt': should be stmt.
    bool isExactAndUniqueDef(IR const* def, IR const* exp);

    //Return true if stmt dominate use's stmt, otherwise return false.
    bool isStmtDomUseInsideLoop(IR const* stmt,
                                IR const* use,
                                LI<IRBB> const* li) const;

    //Return true if ir dominates all its USE expressions which inside loop.
    bool isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const;

    //This equation needs May Kill Def and Must Gen Def.
    bool ForAvailReachDef(UINT bbid,
                          List<IRBB*> & preds,
                          List<IRBB*> * lst,
                          DefMiscBitSetMgr & bsmgr);
    bool ForReachDef(UINT bbid,
                     List<IRBB*> & preds,
                     List<IRBB*> * lst,
                     DefMiscBitSetMgr & bsmgr);
    bool ForAvailExpression(UINT bbid,
                            List<IRBB*> & preds,
                            List<IRBB*> * lst,
                            DefMiscBitSetMgr & bsmgr);

    //Find the nearest dominated DEF stmt of 'exp'.
    //NOTE: RPO of bb of stmt must be available.
    //
    //'exp': expression
    //'exp_stmt': stmt that exp is belong to.
    //'expdu': def set of exp.
    //'omit_self': true if we do not consider the 'exp_stmt' itself.
    IR * findDomDef(IR const* exp,
                    IR const* exp_stmt,
                    DUSet const* expdefset,
                    bool omit_self);

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
    IR const* findKillingLocalDef(IRBB * bb,
                                  xcom::C<IR*> const* ct,
                                  IR const* exp,
                                  MD const* md,
                                  bool * has_local_nonkilling_def);

    //Clean all DU-Chain and Defined/Used-MD reference info.
    //Return the DU structure if has to facilitate other
    //free or destroy process.
    inline DU * freeDUSetAndCleanMDRefs(IR * ir)
    {
        DU * du = ir->getDU();
        if (du == nullptr) { return nullptr; }

        if (DU_duset(du) != nullptr) {
            //Free DUSet back to DefSegMgr, or it will
            //complain and make an assertion.
            ASSERT0(m_misc_bs_mgr);
            m_misc_bs_mgr->freeSBitSetCore(DU_duset(du));
            DU_duset(du) = nullptr;
        }

        //Clean MD refs.
        DU_mds(du) = nullptr;
        DU_md(du) = nullptr;
        return du;
    }

    void setMustKilledDef(UINT bbid, DefDBitSetCore const* set);
    void setMayKilledDef(UINT bbid, DefDBitSetCore const* set);
    void setKilledIRExpr(UINT bbid, DefDBitSetCore const* set);

    //DU chain operations.
    //Set 'use' to be USE of 'stmt'.
    //e.g: given stmt->{u1, u2}, the result will be stmt->{u1, u2, use}
    inline void unionUse(IR * stmt, IN IR * use)
    {
        ASSERT0(stmt && stmt->is_stmt());
        if (use == nullptr) { return; }
        ASSERT0(use->is_exp());
        getAndAllocDUSet(stmt)->addUse(use, *m_misc_bs_mgr);
    }

    //DU chain operations.
    //Set 'exp' to be USE of stmt which is held in 'stmtset'.
    //e.g: given stmt and its use chain is {u1, u2}, the result will be
    //stmt->{u1, u2, exp}
    inline void unionUse(DUSet const* stmtset, IR * exp)
    {
        ASSERT0(stmtset && exp && exp->is_exp());
        DUIter di = nullptr;
        for (INT i = stmtset->get_first(&di);
             i >= 0; i = stmtset->get_next((UINT)i, &di)) {
            IR * d = m_rg->getIR((UINT)i);
            ASSERT0(d->is_stmt());
            getAndAllocDUSet(d)->addUse(exp, *m_misc_bs_mgr);
        }
    }
    //DU chain operation.
    //Set element in 'set' as USE to stmt.
    //e.g: given set is {u3, u4}, and stmt->{u1},
    //=> stmt->{u1, u1, u2}
    inline void unionUseSet(IR * stmt, DefSBitSetCore const* set)
    {
        ASSERT0(stmt->is_stmt());
        if (set == nullptr) { return; }
        getAndAllocDUSet(stmt)->bunion(*set, *m_misc_bs_mgr);
    }
    //DU chain operation.
    //Set element in 'set' as DEF to ir.
    //e.g: given set is {d1, d2}, and {d3}->ir,
    //=>{d1, d2, d3}->ir
    inline void unionDefSet(IR * ir, DefSBitSetCore const* set)
    {
        ASSERT0(ir->is_exp());
        if (set == nullptr) { return; }
        getAndAllocDUSet(ir)->bunion(*set, *m_misc_bs_mgr);
    }
    //DU chain operation.
    //Set 'def' to be DEF of 'ir'.
    //e.g: given def is d1, and {d2}->ir,
    //the result will be {d1, d2}->ir
    inline void unionDef(IR * ir, IN IR * def)
    {
        ASSERT0(ir->is_exp());
        if (def == nullptr) return;
        ASSERT0(def->is_stmt());
        getAndAllocDUSet(ir)->addDef(def, *m_misc_bs_mgr);
    }
    //DU chain operations.
    //Set 'stmt' to be DEF of each expressions which is held in 'expset'.
    //e.g: given stmt and its use-chain is {u1, u2}, the result will be
    //stmt->{u1, u2, use}
    inline void unionDef(DUSet const* expset, IR * stmt)
    {
        ASSERT0(expset && stmt && stmt->is_stmt());
        DUIter di = nullptr;
        for (INT i = expset->get_first(&di);
             i >= 0; i = expset->get_next((UINT)i, &di)) {
            IR * u = m_rg->getIR((UINT)i);
            ASSERT0(u->is_exp());
            getAndAllocDUSet(u)->addDef(stmt, *m_misc_bs_mgr);
        }
    }

    //DU chain operation.
    //Cut off the chain bewteen 'def' and 'use'.
    //The related function is buildDUChain().
    inline void removeDUChain(IR const* def, IR const* use)
    {
        ASSERT0(def->is_stmt() && use->is_exp());
        DUSet * useset = def->getDUSet();
        if (useset != nullptr) { useset->remove(IR_id(use), *m_misc_bs_mgr); }

        DUSet * defset= use->getDUSet();
        if (defset != nullptr) { defset->remove(def->id(), *m_misc_bs_mgr); }
    }

    //Check each USE of stmt, remove the expired one which is not reference
    //the memory any more that stmt defined.
    //Return true if DU changed.
    bool removeExpiredDUForStmt(IR * stmt);

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
    bool removeExpiredDUForOperand(IR * stmt);

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
    bool removeExpiredDU(IR * stmt);

    //Remove 'def' out of ir's DEF set. ir is exp.
    void removeDef(IR const* ir, IR const* def);

    //This function check all USE of memory references of ir tree and
    //cut its du-chain. 'ir' may be stmt or expression, if ir is stmt,
    //check its right-hand-side.
    //This function will process SSA info if it exists.
    //'ir': indicate the root of IR tree.
    //e.g: d1, d2 are def-stmt of stmt's operands.
    //this functin cut off du-chain between d1, d2 and their use.
    void removeUseFromDefset(IR * ir);

    //Note that do NOT use this function to remove SSA def.
    //This function handle the MD DU chain and cut
    //off the DU chain between MD def and its MD use expression.
    //Remove 'def' from its use's def-list.
    //e.g:u1, u2 are its use expressions.
    //cut off the du chain between def->u1 and def->u2.
    void removeDefFromUseset(IR * def);

    //Remove all DU info of 'ir' from DU mgr.
    void removeIRFromDUMgr(IR * ir);

    bool verifyLiveinExp();

    //Verify if DU chain is correct between each Def and Use of MD.
    bool verifyMDDUChainForIR(IR const* ir, UINT duflag);

    virtual bool perform(OptCtx &)
    {
        UNREACHABLE();
        return false;
    }
    bool perform(IN OUT OptCtx & oc,
                 UINT flag = DUOPT_SOL_AVAIL_REACH_DEF|DUOPT_SOL_AVAIL_EXPR|
                             DUOPT_SOL_REACH_DEF|DUOPT_COMPUTE_PR_REF|
                             DUOPT_COMPUTE_NONPR_REF|DUOPT_SOL_REGION_REF);
};

//Verify DU chain's sanity.
//Verify if DU chain is correct between each Def and Use of MD.    
bool verifyMDDUChain(Region * rg, UINT duflag = DUOPT_COMPUTE_PR_DU |
                                                DUOPT_COMPUTE_NONPR_DU);

} //namespace xoc
#endif
