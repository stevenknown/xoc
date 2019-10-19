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

//Util Functions supplied by IR_DU_MGR
// These functions manipulate the reference of IR.
// IR may reference MD, or MDSet, or both MD and MDSet.
//
//    computeOverlapDefMDSet
//    computeOverlapUseMDSet
//    collectMayUse
//    collectMayUseRecursive
//    copyIRTreeDU
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
//    is_may_def
//    is_may_kill
//    is_must_kill
//    isExactAndUniqueDef
//    is_call_may_def
//
//* These functions manipulate the DU chain.
//
//    buildDUChain
//    freeDUSetAndCleanMDRefs
//    copyDUSet
//    get_du_c
//    getAndAllocDUSet
//    freeDU
//    is_du_exist
//    unionUse
//    unionUseSet
//    unionDef
//    unionDefSet
//    removeDUChain
//    removeExpiredDU
//    removeDef
//    removeUseOutFromDefset
//    removeDefOutFromUseset
//    removeIROutFromDUMgr

//Mapping from IR to index.
typedef HMap<IR const*, UINT, HashFuncBase2<IR const*> > IR2UINT;

class IR_DU_MGR;

//Mapping from MD to IR list, and to be responsible for
//allocating and destroy List<IR*> objects.
class MDId2IRlist : public TMap<UINT, DefSBitSetCore*> {
    Region * m_ru;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    IR_DU_MGR * m_du;
    DefMiscBitSetMgr * m_misc_bs_mgr;
    TMapIter<UINT, DefSBitSetCore*> m_iter;
    DefSBitSetCore m_global_md;

    //Indicate if there exist stmt which only have MayDef.
    bool m_are_stmts_defed_ineffect_md;

public:
    explicit MDId2IRlist(Region * rg);
    COPY_CONSTRUCTOR(MDId2IRlist);
    ~MDId2IRlist();

    void append(MD const* md, IR const* ir)
    {
        ASSERT0(ir);
        append(MD_id(md), ir->id());
    }
    void append(MD const* md, UINT irid)
    {
        append(MD_id(md), irid);
    }
    void append(UINT mdid, IR * ir)
    {
        ASSERT0(ir);
        append(mdid, ir->id());
    }
    void append(UINT mdid, UINT irid);

    void clean();

    void dump();

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
    DefMiscBitSetMgr * m_sbs_mgr;

public:
    DefDBitSetCoreHashAllocator(DefMiscBitSetMgr * sbsmgr)
    { ASSERT0(sbsmgr); m_sbs_mgr = sbsmgr; }
    COPY_CONSTRUCTOR(DefDBitSetCoreHashAllocator);

    DefSBitSetCore * alloc() { return m_sbs_mgr->allocDBitSetCore(); }

    void free(DefSBitSetCore * set)
    { m_sbs_mgr->freeDBitSetCore((DefDBitSetCore*)set); }

    DefMiscBitSetMgr * getBsMgr() const { return m_sbs_mgr; }
};


#define HASH_DBITSETCORE

class DefDBitSetCoreReserveTab : public
    SBitSetCoreHash<DefDBitSetCoreHashAllocator> {
    #ifdef HASH_DBITSETCORE
    #else
    List<DefDBitSetCore*> m_allocated;
    #endif

public:
    DefDBitSetCoreReserveTab(DefDBitSetCoreHashAllocator * allocator) :
        SBitSetCoreHash<DefDBitSetCoreHashAllocator>(allocator) {}
    COPY_CONSTRUCTOR(DefDBitSetCoreReserveTab);
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

    void dump(FILE * h)
    {
        #ifdef HASH_DBITSETCORE
        dump_hashed_set(h);
        #else
        if (h == NULL) { return; }
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
#define COMP_EXP_RECOMPUTE         1 //Recompute MD reference, completely
                                     //needs POINT-TO info.
#define COMP_EXP_UPDATE_DU         2
#define COMP_EXP_COLLECT_MUST_USE  4

#define SOL_UNDEF                  0
#define SOL_AVAIL_REACH_DEF        0x1  //compute Must-Available Reach-definition.
#define SOL_REACH_DEF              0x2  //compute May-Available Reach-definition.
#define SOL_AVAIL_EXPR             0x4  //compute Must-Available expression.
#define SOL_RU_REF                 0x8  //compute Region referenced MayDef/MayUse.
#define SOL_REF                    0x10 //compute IR referenced MD/MDSet.
#define COMPUTE_PR_DU              0x20 //compute PR du chain.
#define COMPUTE_NONPR_DU           0x40 //compute Non-PR du chain.

class IR_DU_MGR : public Pass {
    friend class MDId2IRlist;
    friend class DUSet;
protected:
    Region * m_ru;
    TypeMgr * m_tm;
    IR_AA * m_aa;
    IR_CFG * m_cfg;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    MDSetMgr * m_mds_mgr;
    MDSetHash * m_mds_hash;
    DefMiscBitSetMgr * m_misc_bs_mgr;

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
    MDId2IRlist * m_md2irs;

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
    Vector<DefDBitSetCore*> m_bb_avail_in_reach_def; //avail reach-in def of STMT
    Vector<DefDBitSetCore*> m_bb_avail_out_reach_def; //avail reach-out def of STMT
    Vector<DefDBitSetCore*> m_bb_in_reach_def; //reach-in def of STMT
    Vector<DefDBitSetCore*> m_bb_out_reach_def; //reach-out def of STMT
    Vector<DefDBitSetCore*> m_bb_may_gen_def; //generated-def of STMT
    Vector<DefDBitSetCore*> m_bb_must_gen_def; //generated-def of STMT
    Vector<DefDBitSetCore const*> m_bb_must_killed_def; //must-killed def of STMT
    Vector<DefDBitSetCore const*> m_bb_may_killed_def; //may-killed def of STMT

    BSVec<DefDBitSetCore*> m_bb_gen_exp; //generate EXPR
    BSVec<DefDBitSetCore const*> m_bb_killed_exp; //killed EXPR
    BSVec<DefDBitSetCore*> m_bb_availin_exp; //available-in EXPR
    BSVec<DefDBitSetCore*> m_bb_availout_ir_expr; //available-out EXPR

protected:
    bool buildLocalDUChain(IRBB * bb,
                           IR const* exp,
                           MD const* expmd,
                           DUSet * expdu,
                           xcom::C<IR*> * ct,
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
    bool checkIsTruelyDep(IR const* def, IR const* use);
    bool checkIsLocalKillingDefForDirectAccess(MD const* defmd,
                                               MD const* usemd,
                                               IR const* stmt);
    UINT checkIsLocalKillingDefForIndirectAccess(IR const* stmt,
                                                 IR const* exp,
                                                 xcom::C<IR*> const* expct);
    UINT checkIsNonLocalKillingDef(IR const* stmt, IR const* exp);
    inline bool canBeLiveExprCand(IR const* ir) const;
    void computeArrayRefAtIStoreBase(IR * ir);
    void computeExpression(IR * ir, MDSet * ret_mds, UINT compflag, UINT duflag);
    void computeArrayRef(IR * ir,
                         OUT MDSet * ret_mds,
                         UINT compflag,
                         UINT duflag);
    void checkAndBuildChainForMemIR(IRBB * bb, IR * exp, xcom::C<IR*> * ct);
    void checkAndBuildChainRecursiveIRList(IRBB * bb,
                                           IR * exp,
                                           xcom::C<IR*> * ct,
                                           UINT flag);
    void checkAndBuildChainRecursive(IRBB * bb,
                                     IR * exp,
                                     xcom::C<IR*> * ct,
                                     UINT flag);
    void checkAndBuildChain(IR * stmt, xcom::C<IR*> * ct, UINT flag);
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

    DefDBitSetCore * getMayGenDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * getMustGenDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * getAvailOutReachDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * getOutReachDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * getGenIRExpr(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * getAvailOutExpr(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore const* getMustKilledDef(UINT bbid);
    DefDBitSetCore const* getMayKilledDef(UINT bbid);
    DefDBitSetCore const* getKilledIRExpr(UINT bbid);

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
    void initMD2IRList(IRBB * bb);
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
    void resetAvailReachDefInSet(bool cleanMember);
    void resetAvailExpInSet(bool cleanMember);
    void resetReachDefInSet(bool cleanMember);
    void resetGlobalSet(bool cleanMember);
    void updateDefWithMustEffectMD(IR * ir, MD const* musteffect);
    void updateDefWithMustExactMD(IR * ir, MD const* mustexact);
    void updateDef(IR * ir, UINT flag);

public:
    explicit IR_DU_MGR(Region * rg);
    COPY_CONSTRUCTOR(IR_DU_MGR);
    ~IR_DU_MGR();

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
    void copyIRTreeDU(IR * to, IR const* from, bool copy_du_info);

    //Count the memory usage to IR_DU_MGR.
    size_t count_mem();
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
        if (srcduinfo == NULL) {
            DUSet * tgtduinfo = tgt->getDUSet();
            if (tgtduinfo != NULL) {
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
        if (srcset == NULL) {
            DUSet * tgtduinfo = tgt->getDUSet();
            if (tgtduinfo != NULL) {
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

        DUIter di = NULL;
        for (UINT i = (UINT)from_du->get_first(&di);
             di != NULL; i = (UINT)from_du->get_next(i, &di)) {
            IR const* ref = m_ru->getIR(i);
            //ref may be stmt or exp.

            DUSet * ref_defuse_set = ref->getDUSet();
            if (ref_defuse_set == NULL) { continue; }
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
        ASSERT0(m_ru->getIR(from)->is_stmt() &&
                m_ru->getIR(to)->is_stmt() &&
                useset_of_to && useset_of_from && m);
        DUIter di = NULL;
        for (INT i = useset_of_from->get_first(&di);
             di != NULL; i = useset_of_from->get_next((UINT)i, &di)) {
            IR const* exp = m_ru->getIR((UINT)i);
            ASSERT0(exp->isMemoryRef());

            DUSet * defset = exp->getDUSet();
            if (defset == NULL) { continue; }
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
        if (useset_of_from == NULL) { return; }

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
        ASSERT0(m_ru->getIR(from)->is_exp() && m_ru->getIR(to)->is_exp() &&
                defset_of_from && defset_of_to && m);
        DUIter di = NULL;
        for (INT i = defset_of_from->get_first(&di);
             di != NULL; i = defset_of_from->get_next((UINT)i, &di)) {
            IR * stmt = m_ru->getIR((UINT)i);
            ASSERT0(stmt->is_stmt());
            DUSet * useset = stmt->getDUSet();
            if (useset == NULL) { continue; }
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
    //e.g: DEF->from change to DEF->to
    inline void changeUse(IR * to, IR const* from, DefMiscBitSetMgr * m)
    {
        ASSERT0(to && from && to->is_exp() && from->is_exp());
        DUSet * defset_of_from = from->getDUSet();
        if (defset_of_from == NULL) { return; }

        DUSet * defset_of_to = getAndAllocDUSet(to);
        changeUse(IR_id(to), IR_id(from), defset_of_to, defset_of_from, m);
    }

    void dumpMemUsageForEachSet();
    void dumpMemUsageForMDRef();
    void dumpSet(bool is_bs = false);
    void dumpBBRef(IN IRBB * bb, UINT indent);
    void dumpRef(UINT indent = 4);
    void dumpDUChain();
    void dumpDUChainDetail();
    void dumpBBDUChainDetail(UINT bbid);
    void dumpBBDUChainDetail(IRBB * bb);
    void dumpDUGraph(CHAR const* name = NULL, bool detail = true);
    void dump(CHAR const* name);

    DefDBitSetCore * getAvailInExpr(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * getInReachDef(UINT bbid, DefMiscBitSetMgr * mgr);
    DefDBitSetCore * getAvailInReachDef(UINT bbid, DefMiscBitSetMgr * mgr);
    virtual CHAR const* getPassName() const { return "DU Manager"; }
    virtual PASS_TYPE getPassType() const { return PASS_DU_MGR; }

    //DU chain operation.
    DUSet * getAndAllocDUSet(IR * ir);

    //Get information set to BB.

    //DefMiscBitSetMgr * getMiscBitSetMgr() { return m_misc_bs_mgr; }

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
        if (md == NULL || !md->is_exact()) { return NULL; }
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
        return md != NULL && md->is_exact() ? md : NULL;
    }

    MD const* get_effect_use_md(IR const* ir)
    {
        ASSERT0(ir && ir->is_exp());
        return ir->getEffectRef();
    }
    IR const* getExactAndUniqueDef(IR const* exp);


    inline bool is_du_exist(IR const* def, IR const* use) const
    {
        ASSERT0(def->is_stmt() && use->is_exp());
        DUSet const* du = def->readDUSet();
        if (du == NULL) { return false; }
        return du->is_contain(IR_id(use));
    }
    bool is_stpr_may_def(IR const* def, IR const* use, bool is_recur);
    bool is_call_may_def(IR const* def, IR const* use, bool is_recur);
    bool is_may_def(IR const* def, IR const* use, bool is_recur);
    bool is_may_kill(IR const* def1, IR const* def2);
    bool is_must_kill(IR const* def1, IR const* def2);
    bool isExactAndUniqueDef(IR const* def, IR const* exp);

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
    void freeDU();
    IR * findDomDef(IR const* exp,
                    IR const* exp_stmt,
                    DUSet const* expdefset,
                    bool omit_self);
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
        if (du == NULL) { return NULL; }

        if (DU_duset(du) != NULL) {
            //Free DUSet back to DefSegMgr, or it will
            //complain and make an assertion.
            ASSERT0(m_misc_bs_mgr);
            m_misc_bs_mgr->freeSBitSetCore(DU_duset(du));
            DU_duset(du) = NULL;
        }

        //Clean MD refs.
        DU_mds(du) = NULL;
        DU_md(du) = NULL;
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
        if (use == NULL) { return; }
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
        DUIter di = NULL;
        for (INT i = stmtset->get_first(&di);
             i >= 0; i = stmtset->get_next((UINT)i, &di)) {
            IR * d = m_ru->getIR((UINT)i);
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
        if (set == NULL) { return; }
        getAndAllocDUSet(stmt)->bunion(*set, *m_misc_bs_mgr);
    }
    //DU chain operation.
    //Set element in 'set' as DEF to ir.
    //e.g: given set is {d1, d2}, and {d3}->ir,
    //=>{d1, d2, d3}->ir
    inline void unionDefSet(IR * ir, DefSBitSetCore const* set)
    {
        ASSERT0(ir->is_exp());
        if (set == NULL) { return; }
        getAndAllocDUSet(ir)->bunion(*set, *m_misc_bs_mgr);
    }
    //DU chain operation.
    //Set 'def' to be DEF of 'ir'.
    //e.g: given def is d1, and {d2}->ir,
    //the result will be {d1, d2}->ir
    inline void unionDef(IR * ir, IN IR * def)
    {
        ASSERT0(ir->is_exp());
        if (def == NULL) return;
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
        DUIter di = NULL;
        for (INT i = expset->get_first(&di);
             i >= 0; i = expset->get_next((UINT)i, &di)) {
            IR * u = m_ru->getIR((UINT)i);
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
        if (useset != NULL) { useset->remove(IR_id(use), *m_misc_bs_mgr); }

        DUSet * defset= use->getDUSet();
        if (defset != NULL) { defset->remove(def->id(), *m_misc_bs_mgr); }
    }
    //DU chain operations.
    //See function definition.
    bool removeExpiredDUForStmt(IR * stmt);
    bool removeExpiredDUForOperand(IR * stmt);
    bool removeExpiredDU(IR * stmt);
    void removeDef(IR const* ir, IR const* def);
    void removeUseOutFromDefset(IR * ir);
    void removeDefOutFromUseset(IR * def);
    void removeIROutFromDUMgr(IR * ir);

    bool verifyMDDUChain(UINT duflag);
    bool verifyMDDUChainForIR(IR const* ir, UINT duflag);
    bool verifyLiveinExp();

    virtual bool perform(OptCtx &)
    {
        UNREACHABLE();
        return false;
    }
    bool perform(IN OUT OptCtx & oc,
                 UINT flag = SOL_AVAIL_REACH_DEF|
                             SOL_AVAIL_EXPR|
                             SOL_REACH_DEF|
                             SOL_REF|
                             SOL_RU_REF);
};

} //namespace xoc
#endif
