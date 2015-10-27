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

/* Util Functions supplied by IR_DU_MGR
* These functions manipulate the reference of IR.
  IR may reference MD, or MDSet, or both MD and MDSet.

    computeOverlapDefMDSet
    computeOverlapUseMDSet
    collectMayUse
    collectMayUseRecursive
    copyIRTreeDU
    changeUse
    changeDef
    get_may_def
    get_must_use
    get_may_use
    get_must_def
    get_effect_ref
    get_effect_def_md
    get_exact_def_md
    get_effect_use_md
    get_exact_use_md
    getExactAndUniqueDef
    freeDUSetAndMDRefs
    is_may_def
    is_may_kill
    is_must_kill
    isExactAndUniqueDef
    is_call_may_def

* These functions manipulate the DU chain.

    buildDUChain
    freeDUSetAndMDRefs
    copyDUSet
    get_du_c
    getAndAllocDUSet
    freeDU
    is_du_exist
    unionUse
    unionUseSet
    unionDef
    unionDefSet
    removeDUChain
    removeExpiredDU
    removeDef
    removeUseOutFromDefset
    removeDefOutFromUseset
    removeIROutFromDUMgr
*/

//Mapping from IR to index.
typedef HMap<IR const*, UINT, HashFuncBase2<IR const*> > IR2UINT;


class IR_DU_MGR;

//Mapping from MD to IR list, and to be responsible for
//allocating and destroy List<IR*> objects.
class MDId2IRlist : public TMap<UINT, DefSBitSetCore*> {
    Region * m_ru;
    MDSystem * m_md_sys;
    TypeMgr * m_dm;
    IR_DU_MGR * m_du;
    DefMiscBitSetMgr * m_misc_bs_mgr;
    TMapIter<UINT, DefSBitSetCore*> m_iter;
    DefSBitSetCore m_global_md;

    //Indicate if there exist stmt which only have MayDef.
    bool m_has_stmt_which_only_have_maydef;
public:
    explicit MDId2IRlist(Region * ru);
    COPY_CONSTRUCTOR(MDId2IRlist);
    ~MDId2IRlist();

    void append(MD const* md, IR const* ir)
    {
        ASSERT0(ir);
        append(MD_id(md), IR_id(ir));
    }
    void append(MD const* md, UINT irid)
    {
        append(MD_id(md), irid);
    }
    void append(UINT mdid, IR * ir)
    {
        ASSERT0(ir);
        append(mdid, IR_id(ir));
    }
    void append(UINT mdid, UINT irid);

    void clean();

    void dump();

    bool hasIneffectDef() const
    { return m_has_stmt_which_only_have_maydef; }

    void set(UINT mdid, IR * ir);

    void markStmtOnlyHasMaydef()
    { m_has_stmt_which_only_have_maydef = true; }
};


/* Ud chains describe all of the might uses of the prior DEFINITION of md.
Du chains describe all effective USEs of once definition of md.
e.g:
        d1:a=   d2:a=   d3:a=
              \       |         /
                  b = a
              /            \
             d4: =b         d5: =b
        Ud chains:  a use def d1,d2,d3 stmt
        Du chains:  b's value will be used by d4,d5 stmt
If ir is stmt, this class indicate the USE expressions set.
If ir is expression, this class indicate the DEF stmt set. */

//Mapping from IR to DUSet.
typedef HMap<IR*, DUSet*> IR2DU;

//Def|Use information manager.
#define COMP_EXP_RECOMPUTE             1 //Recompute DU
                                         //completely needs POINT-TO info.
#define COMP_EXP_UPDATE_DU             2
#define COMP_EXP_COLLECT_MUST_USE      4

#define SOL_UNDEF                      0
#define SOL_AVAIL_REACH_DEF            1  //available reach-definition.
#define SOL_REACH_DEF                  2  //may reach-definition.
#define SOL_AVAIL_EXPR                 4  //live expr.
#define SOL_RU_REF                     8  //region's def/use mds.
#define SOL_REF                        16 //referrenced mds.
class IR_DU_MGR : public Pass {
    friend class MDId2IRlist;
    friend class DUSet;
protected:
    Region * m_ru;
    TypeMgr * m_dm;
    IR_AA * m_aa;
    IR_CFG * m_cfg;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    MDSetMgr * m_mds_mgr;
    MDSetHash * m_mds_hash;
    DefMiscBitSetMgr * m_misc_bs_mgr;

    /* Set to true if there is single map between
    PR and MD. If PR may corresponde to multiple
    MD, set it to false.

    e.g: If one use same pr with different type U8 and U32,
    there will have two mds refer to pr1(U8) and pr1(U32),
    MD2->pr1(U8), MD8->pr1(U32). */
    BYTE m_is_pr_unique_for_same_no:1;

    /* Indicate whether compute the DU chain for PR.
    This flag is often set to false if PR SSA has constructed to reduce
    compilation time and memory consumption.
    If the flag is true, reach-def will not be computed. */
    BYTE m_is_compute_pr_du_chain:1;

    ConstIRIter m_citer; //for tmp use.
    ConstIRIter m_citer2; //for tmp use.
    IRIter m_iter; //for tmp use.
    IRIter m_iter2; //for tmp use.
    MDSet m_tmp_mds; //for tmp use.
    ConstMDIter m_tab_iter;

    //Used to cache overlapping MDSet for specified MD.
    TMap<MD const*, MDSet const*> m_cached_overlap_mdset;

    //Indicate whether MDSet is cached for specified MD.
    DefSBitSetCore m_is_cached_mdset;

    //Used by DU chain.
    BitSet * m_is_init;
    MDId2IRlist * m_md2irs;

    OptCTX * m_oc;

    /* Available reach-def computes the definitions
    which must be the last definition of result variable,
    but it may not reachable meanwhile.
    e.g:
        BB1:
        a=1  //S1
        *p=3
        a=4  //S2
        goto BB3

        BB2:
        a=2 //S3
        goto BB3

        BB3:
        f(a)

        Here we do not known where p pointed to.
        The available-reach-def of BB3 is {S1, S3}

    Compare to available reach-def, reach-def computes the definition
    which may-live at each BB.
    e.g:
        BB1:
        a=1  //S1
        *p=3
        goto BB3

        BB2:
        a=2 //S2
        goto BB3

        BB3:
        f(a)

        Here we do not known where p pointed to.
        The reach-def of BB3 is {S1, S2} */
    Vector<DefDBitSetCore*> m_bb_avail_in_reach_def; //avail reach-in def of IR STMT
    Vector<DefDBitSetCore*> m_bb_avail_out_reach_def; //avail reach-out def of IR STMT
    Vector<DefDBitSetCore*> m_bb_in_reach_def; //reach-in def of IR STMT
    Vector<DefDBitSetCore*> m_bb_out_reach_def; //reach-out def of IR STMT
    Vector<DefDBitSetCore*> m_bb_may_gen_def; //generated-def of IR STMT
    Vector<DefDBitSetCore*> m_bb_must_gen_def; //generated-def of IR STMT
    Vector<DefDBitSetCore*> m_bb_must_killed_def; //must-killed def of IR STMT
    Vector<DefDBitSetCore*> m_bb_may_killed_def; //may-killed def of IR STMT

    BSVec<DefDBitSetCore*> m_bb_gen_ir_expr; //generate IR EXPR
    BSVec<DefDBitSetCore*> m_bb_killed_ir_expr; //killed IR EXPR
    BSVec<DefDBitSetCore*> m_bb_availin_ir_expr; //available in IR EXPR
    BSVec<DefDBitSetCore*> m_bb_availout_ir_expr; //available out IR EXPR
protected:
    void addOverlappedExactMD(
                OUT MDSet * mds,
                MD const* x,
                ConstMDIter & mditer);

    bool buildLocalDUChain(
                IR const* stmt, IR const* exp,
                MD const* expmd, DUSet * expdu,
                C<IR*> * ct);
    void buildChainForMD(IR const* exp, MD const* expmd, DUSet * expdu);
    void buildChainForMDSet(IR const* exp,  MD const* expmd,
                            MDSet const& expmds, DUSet * expdu);
    void buildChainForMust(IR const* exp, MD const* expmd, DUSet * expdu);

    bool checkIsTruelyDep(MD const* mustdef, MDSet const* maydef,
                          IR const* use);
    UINT checkIsLocalKillingDef(IR const* stmt, IR const* exp, C<IR*> * expct);
    UINT checkIsNonLocalKillingDef(IR const* stmt, IR const* exp);
    inline bool canBeLiveExprCand(IR const* ir) const;
    void computeArrayRefAtIstoreBase(IR * ir);
    void computeExpression(IR * ir, MDSet * ret_mds, UINT flag);
    void computeArrayRef(IR * ir, OUT MDSet * ret_mds, UINT flag);
    void checkAndBuildChainRecursive(IR * stmt, IR * exp, C<IR*> * ct);
    void checkAndBuildChain(IR * stmt, C<IR*> * ct);
    void collectMustUseForLda(IR const* lda, OUT MDSet * ret_mds);
    void computeMustDefForRegion(IR const* ir, MDSet * bb_mustdefmds);
     void computeBBMayDef(
            IR const* ir,
            MDSet * bb_maydefmds,
            DefDBitSetCore * maygen_stmt);
    void computeBBMustDef(
            IR const* ir,
            MDSet * bb_mustdefmds,
            DefDBitSetCore * mustgen_stmt,
            ConstMDIter & mditer);
    void computeMustDef_MayDef_MayUse(
            OUT Vector<MDSet*> * mustdef,
            OUT Vector<MDSet*> * maydef,
            OUT MDSet * mayuse,
            UINT flag);

    bool ForAvailReachDef(UINT bbid, List<IRBB*> & preds, List<IRBB*> * lst);
    bool ForReachDef(UINT bbid, List<IRBB*> & preds, List<IRBB*> * lst);
    bool ForAvailExpression(UINT bbid, List<IRBB*> & preds, List<IRBB*> * lst);
    inline void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }

    bool hasSingleDefToMD(DUSet const& defset, MD const* md) const;

    bool is_overlap_def_use(
            MD const* mustdef,
            MDSet const* maydef,
            IR const* use);
    void initMD2IRList(IRBB * bb);
    void inferRegion(IR * ir, bool ruinfo_avail, IN MDSet * tmp);
    void inferIstore(IR * ir);
    void inferStore(IR * ir);
    void inferStorePR(IR * ir);
    void inferStoreArray(IR * ir);
    void inferPhi(IR * ir);
    void inferCall(IR * ir, IN MD2MDSet * mx);

    void solve(DefDBitSetCore const* expr_universe, UINT const flag);
    void resetLocalAuxSet(bool cleanMember);
    void resetGlobalSet(bool cleanMember);
    void resetReachDefInSet(bool cleanMember);
    void resetReachDefOutSet(bool cleanMember);
    void updateDef(IR * ir);
    void updateRegion(IR * ir);
public:
    explicit IR_DU_MGR(Region * ru);
    COPY_CONSTRUCTOR(IR_DU_MGR);
    ~IR_DU_MGR();

    //Build DU chain : def->use.
    void buildDUChain(IR * def, IR * use)
    {
        ASSERT0(def && def->is_stmt() && use && use->is_exp());
        getAndAllocDUSet(def)->add_use(use, *m_misc_bs_mgr);
        getAndAllocDUSet(use)->add_def(def, *m_misc_bs_mgr);
    }

    /* Compute the MDSet that might overlap ones which 'ir' defined.
    e.g: int A[100], there are two referrence of array A: A[i], A[j]
        A[i] might overlap A[j].

    'tmp': regard as input data, compute overlapped MD of its element.

    NOTE: Be careful 'mds' would be modified. */
    void computeOverlapDefMDSet(IR * ir, bool recompute)
    { computeOverlapUseMDSet(ir, recompute); }

    void computeOverlapUseMDSet(IR * ir, bool recompute);
    void collect_must_use(IR const* ir, OUT MDSet & mustuse);
    void computeGenForBB(
            IN IRBB * bb,
            IN OUT DefDBitSetCore & expr_univers,
            MDSet & tmp);
    void computeMDDUforBB(IRBB * bb);
    void computeMDRef();
    void computeKillSet(
            Vector<MDSet*> const* mustdefs,
            Vector<MDSet*> const* maydefs);
    void computeAuxSetForExpression(
            OUT DefDBitSetCore * expr_universe,
            Vector<MDSet*> const* maydefmds);
    void computeMDDUChain(IN OUT OptCTX & oc);
    void computeRegionMDDU(
            Vector<MDSet*> const* mustdefmds,
            Vector<MDSet*> const* maydefmds,
            MDSet const* mayusemds);

    //Clean all DU-Chain and Defined/Used-MD reference info.
    //Return the DU structure if has to facilitate other free or destroy process.
    inline DU * freeDUSetAndMDRefs(IR * ir)
    {
        DU * du = ir->get_du();
        if (du == NULL) { return NULL; }

        if (DU_duset(du) != NULL) {
            //Free DUSet back to DefSegMgr, or it will
            //complain and make an assertion.
            ASSERT0(m_misc_bs_mgr);
            m_misc_bs_mgr->free_sbitsetc(DU_duset(du));
            DU_duset(du) = NULL;
        }
        DU_mds(du) = NULL;
        DU_md(du) = NULL;
        return du;
    }

    //DU chain and Memory Object reference operation.
    void copyIRTreeDU(IR * to, IR const* from, bool copy_du_info);

    //Count the memory usage to IR_DU_MGR.
    UINT count_mem();
    UINT count_mem_duset();
    UINT count_mem_local_data(
            DefDBitSetCore * expr_univers,
            Vector<MDSet*> * maydef_mds,
            Vector<MDSet*> * mustdef_mds,
            MDSet * mayuse_mds,
            MDSet mds_arr_for_must[],
            MDSet mds_arr_for_may[],
            UINT elemnum);

    //Collect must and may memory reference.
    void collectMayUseRecursive(IR const* ir, MDSet & may_use, bool computePR);

    //Collect may memory reference.
    void collectMayUse(IR const* ir, MDSet & may_use, bool computePR);

    //DU chain operation.
    //Copy DUSet from 'src' to 'tgt'. src and tgt must
    //both to be either stmt or expression.
    void copyDUSet(IR * tgt, IR const* src)
    {
        //tgt and src should either both be stmt or both be exp.
        ASSERT0(!(tgt->is_stmt() ^ src->is_stmt()));
        DUSet const* srcduinfo = src->get_duset_c();
        if (srcduinfo == NULL) {
            DUSet * tgtduinfo = tgt->get_duset();
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
            DUSet * tgtduinfo = tgt->get_duset();
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
        DUSet const* from_du = src->get_duset_c();

        DU_ITER di = NULL;
        for (UINT i = (UINT)from_du->get_first(&di);
             di != NULL; i = (UINT)from_du->get_next(i, &di)) {
            IR const* ref = get_ir(i);
            //ref may be stmt or exp.

            DUSet * ref_defuse_set = ref->get_duset();
            if (ref_defuse_set == NULL) { continue; }
            ref_defuse_set->add(IR_id(tgt), *m_misc_bs_mgr);
        }
    }

    /* DU chain operation.
    Change Def stmt from 'from' to 'to'.
    'to': copy to stmt's id.
    'from': copy from stmt's id.
    'useset': each element is USE, it is the USE expression set of 'from'.
    e.g:
        from->USE change to to->USE */
    void changeDef(
            UINT to,
            UINT from,
            DUSet * useset_of_to,
            DUSet * useset_of_from,
            DefMiscBitSetMgr * m)
    {
        ASSERT0(get_ir(from)->is_stmt() && get_ir(to)->is_stmt() &&
                useset_of_to && useset_of_from && m);
        DU_ITER di = NULL;
        for (INT i = useset_of_from->get_first(&di);
             di != NULL; i = useset_of_from->get_next((UINT)i, &di)) {
            IR const* exp = get_ir((UINT)i);
            ASSERT0(exp->is_memory_ref());

            DUSet * defset = exp->get_duset();
            if (defset == NULL) { continue; }
            defset->diff(from, *m_misc_bs_mgr);
            defset->bunion(to, *m_misc_bs_mgr);
        }
        useset_of_to->bunion(*useset_of_from, *m);
        useset_of_from->clean(*m);
    }

    /* DU chain operation.
    Change Def stmt from 'from' to 'to'.
    'to': target stmt.
    'from': source stmt.
    e.g:
        from->USE change to to->USE */
    inline void changeDef(IR * to, IR * from, DefMiscBitSetMgr * m)
    {
        ASSERT0(to && from && to->is_stmt() && from->is_stmt());
        DUSet * useset_of_from = from->get_duset();
        if (useset_of_from == NULL) { return; }

        DUSet * useset_of_to = getAndAllocDUSet(to);
        changeDef(IR_id(to), IR_id(from), useset_of_to, useset_of_from, m);
    }

    /* DU chain operation.
    Change Use expression from 'from' to 'to'.
    'to': indicate the target expression which copy to.
    'from': indicate the source expression which copy from.
    'defset': it is the DEF stmt set of 'from'.
    e.g:
        DEF->from change to DEF->to */
    void changeUse(
            UINT to,
            UINT from,
            DUSet * defset_of_to,
            DUSet * defset_of_from,
            DefMiscBitSetMgr * m)
    {
        ASSERT0(get_ir(from)->is_exp() && get_ir(to)->is_exp() &&
                 defset_of_from && defset_of_to && m);
        DU_ITER di = NULL;
        for (INT i = defset_of_from->get_first(&di);
             di != NULL; i = defset_of_from->get_next((UINT)i, &di)) {
            IR * stmt = get_ir((UINT)i);
            ASSERT0(stmt->is_stmt());
            DUSet * useset = stmt->get_duset();
            if (useset == NULL) { continue; }
            useset->diff(from, *m_misc_bs_mgr);
            useset->bunion(to, *m_misc_bs_mgr);
        }
        defset_of_to->bunion(*defset_of_from, *m);
        defset_of_from->clean(*m);
    }

    /* DU chain operation.
    Change Use expression from 'from' to 'to'.
    'to': indicate the exp which copy to.
    'from': indicate the expression which copy from.
    e.g:
        DEF->from change to DEF->to */
    inline void changeUse(IR * to, IR const* from, DefMiscBitSetMgr * m)
    {
        ASSERT0(to && from && to->is_exp() && from->is_exp());
        DUSet * defset_of_from = from->get_duset();
        if (defset_of_from == NULL) { return; }

        DUSet * defset_of_to = getAndAllocDUSet(to);
        changeUse(IR_id(to), IR_id(from), defset_of_to, defset_of_from, m);
    }

    void destroy();
    void dumpMemUsageForEachSet();
    void dumpMemUsageForMDRef();
    void dump_set(bool is_bs = false);
    void dump_ir_ref(IN IR * ir, UINT indent);
    void dump_ir_list_ref(IN IR * ir, UINT indent);
    void dump_bb_ref(IN IRBB * bb, UINT indent);
    void dump_ref(UINT indent = 4);
    void dump_du_chain();
    void dump_du_chain2();
    void dump_bb_du_chain2(UINT bbid);
    void dump_bb_du_chain2(IRBB * bb);
    void dump_du_graph(CHAR const* name = NULL, bool detail = true);

    virtual CHAR const* get_pass_name() const { return "DU Manager"; }
    virtual PASS_TYPE get_pass_type() const { return PASS_DU_MGR; }

    //DU chain operation.
    DUSet * getAndAllocDUSet(IR * ir);

    //Get set to BB.
    DefDBitSetCore * get_may_gen_def(UINT bbid);
    DefDBitSetCore * get_must_gen_def(UINT bbid);
    DefDBitSetCore * get_avail_in_reach_def(UINT bbid);
    DefDBitSetCore * get_avail_out_reach_def(UINT bbid);
    DefDBitSetCore * get_in_reach_def(UINT bbid);
    DefDBitSetCore * get_out_reach_def(UINT bbid);
    DefDBitSetCore * get_must_killed_def(UINT bbid);
    DefDBitSetCore * get_may_killed_def(UINT bbid);
    DefDBitSetCore * get_gen_ir_expr(UINT bbid);
    DefDBitSetCore * get_killed_ir_expr(UINT bbid);
    DefDBitSetCore * get_availin_expr(UINT bbid);
    DefDBitSetCore * get_availout_expr(UINT bbid);

    DefMiscBitSetMgr * getMiscBitSetMgr() { return m_misc_bs_mgr; }

    //Return the MustDef MD.
    MD const* get_must_def(IR const* ir)
    {
        ASSERT0(ir && ir->is_stmt());
        return ir->get_ref_md();
    }

    MD const* get_effect_def_md(IR const* ir)
    {
        ASSERT0(ir && ir->is_stmt());
        return ir->get_effect_ref();
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
    MDSet const* get_may_def(IR const* ir)
    {
        ASSERT0(ir && ir->is_stmt());
        return ir->get_ref_mds();
    }

    //Return the MustUse MD.
    MD const* get_must_use(IR const* ir)
    {
        ASSERT0(ir && ir->is_exp());
        return ir->get_effect_ref();
    }

    //Return the MayUse MD set.
    MDSet const* get_may_use(IR const* ir)
    {
        ASSERT0(ir && ir->is_exp());
        return ir->get_ref_mds();
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
        return ir->get_effect_ref();
    }

    IR const* getExactAndUniqueDef(IR const* exp);
    IR * get_ir(UINT irid);

    void freeDU();

    bool isComputePRDU() const { return m_is_compute_pr_du_chain; }
    inline bool is_du_exist(IR const* def, IR const* use) const
    {
        ASSERT0(def->is_stmt() && use->is_exp());
        DUSet const* du = def->get_duset_c();
        if (du == NULL) { return false; }
        return du->is_contain(IR_id(use));
    }
    bool isPRUniqueForSameNo() const { return m_is_pr_unique_for_same_no; }
    bool is_stpr_may_def(IR const* def, IR const* use, bool is_recur);
    bool is_call_may_def(IR const* def, IR const* use, bool is_recur);
    bool is_may_def(IR const* def, IR const* use, bool is_recur);
    bool is_may_kill(IR const* def1, IR const* def2);
    bool is_must_kill(IR const* def1, IR const* def2);
    bool isExactAndUniqueDef(IR const* def, IR const* exp);
    IR * findDomDef(
            IR const* exp,
            IR const* exp_stmt,
            DUSet const* expdu,
            bool omit_self);
    IR const* findKillingLocalDef(
            IR const* stmt,
            C<IR*> * ct,
            IR const* exp,
            MD const* md);

    void setComputePRDU(bool compute)
    { m_is_compute_pr_du_chain = (BYTE)compute; }

    /* DU chain operations.
    Set 'use' to be USE of 'stmt'.
    e.g: given stmt->{u1, u2}, the result will be stmt->{u1, u2, use} */
    inline void unionUse(IR * stmt, IN IR * use)
    {
        ASSERT0(stmt && stmt->is_stmt());
        if (use == NULL) return;
        ASSERT0(use->is_exp());
        getAndAllocDUSet(stmt)->add_use(use, *m_misc_bs_mgr);
    }

    /* DU chain operations.
    Set 'exp' to be USE of stmt which is held in 'stmtset'.
    e.g: given stmt and its use chain is {u1, u2}, the result will be
    stmt->{u1, u2, exp} */
    inline void unionUse(DUSet const* stmtset, IR * exp)
    {
        ASSERT0(stmtset && exp && exp->is_exp());
        DU_ITER di = NULL;
        for (INT i = stmtset->get_first(&di);
             i >= 0; i = stmtset->get_next((UINT)i, &di)) {
            IR * d = m_ru->get_ir((UINT)i);
            ASSERT0(d->is_stmt());
            getAndAllocDUSet(d)->add_use(exp, *m_misc_bs_mgr);
        }
    }

    /* DU chain operation.
    Set element in 'set' as USE to stmt.
    e.g: given set is {u3, u4}, and stmt->{u1},
    =>
    stmt->{u1, u1, u2} */
    inline void unionUseSet(IR * stmt, IN DefSBitSetCore const* set)
    {
        ASSERT0(stmt->is_stmt());
        if (set == NULL) { return; }
        getAndAllocDUSet(stmt)->bunion(*set, *m_misc_bs_mgr);
    }

    /* DU chain operation.
    Set element in 'set' as DEF to ir.
    e.g: given set is {d1, d2}, and {d3}->ir,
    =>
    {d1, d2, d3}->ir */
    inline void unionDefSet(IR * ir, IN DefSBitSetCore const* set)
    {
        ASSERT0(ir->is_exp());
        if (set == NULL) { return; }
        getAndAllocDUSet(ir)->bunion(*set, *m_misc_bs_mgr);
    }

    /* DU chain operation.
    Set 'def' to be DEF of 'ir'.
    e.g: given def is d1, and {d2}->ir,
    the result will be {d1, d2}->ir */
    inline void unionDef(IR * ir, IN IR * def)
    {
        ASSERT0(ir->is_exp());
        if (def == NULL) return;
        ASSERT0(def->is_stmt());
        getAndAllocDUSet(ir)->add_def(def, *m_misc_bs_mgr);
    }


    /* DU chain operations.
    Set 'stmt' to be DEF of each expressions which is held in 'expset'.
    e.g: given stmt and its use-chain is {u1, u2}, the result will be
    stmt->{u1, u2, use} */
    inline void unionDef(DUSet const* expset, IR * stmt)
    {
        ASSERT0(expset && stmt && stmt->is_stmt());
        DU_ITER di = NULL;
        for (INT i = expset->get_first(&di);
             i >= 0; i = expset->get_next((UINT)i, &di)) {
            IR * u = m_ru->get_ir((UINT)i);
            ASSERT0(u->is_exp());
            getAndAllocDUSet(u)->add_def(stmt, *m_misc_bs_mgr);
        }
    }

    /* DU chain operation.
    Cut off the chain bewteen 'def' and 'use'.
    The related function is buildDUChain(). */
    inline void removeDUChain(IR const* def, IR const* use)
    {
        ASSERT0(def->is_stmt() && use->is_exp());
        DUSet * useset = def->get_duset();
        if (useset != NULL) { useset->remove(IR_id(use), *m_misc_bs_mgr); }

        DUSet * defset= use->get_duset();
        if (defset != NULL) { defset->remove(IR_id(def), *m_misc_bs_mgr); }
    }

    //DU chain operations.
    //See function definition.
    bool removeExpiredDUForStmt(IR * stmt);
    bool removeExpiredDUForOperand(IR * stmt);
    bool removeExpiredDU(IR * stmt);
    void removeDef(IR const* ir, IR const* def);
    void removeUseOutFromDefset(IR * stmt);
    void removeDefOutFromUseset(IR * def);
    void removeIROutFromDUMgr(IR * ir);

    bool verifyMDRef();
    bool verifyMDDUChain();
    bool verifyMDDUChainForIR(IR const* ir);
    bool verifyLiveinExp();

    virtual bool perform(OptCTX &)
    {
        ASSERT0(0);
        return false;
    }
    bool perform(IN OUT OptCTX & oc,
                 UINT flag = SOL_AVAIL_REACH_DEF|
                             SOL_AVAIL_EXPR|
                             SOL_REACH_DEF|
                             SOL_REF|
                             SOL_RU_REF);
};

} //namespace xoc
#endif
