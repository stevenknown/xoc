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
#ifndef _IR_ALIAS_ANALYSIS_H_
#define _IR_ALIAS_ANALYSIS_H_

namespace xoc {

class IRCFG;
class GSCC;
class PRSSAMgr;

//PtPair
#define PP_id(pp) ((pp)->id)
#define PP_from(pp) ((pp)->from)
#define PP_to(pp) ((pp)->to)
class PtPair {
public:
    UINT id;
    UINT from;
    UINT to;
};


//PtPairSet
//Since PtPair's id is densely allocated, using xcom::BitSet is plausible.
//CASE:some case introduce sparse PtPair, so we switch BitSet to SBitSet.
typedef xcom::DefSBitSetCore PtPairSet;
typedef xcom::DefSBitSetIter PtPairSetIter;


//MD Addendum
#define MDA_md(m) ((m)->md)
#define MDA_mds(m) ((m)->mds)
class MDA {
public:
    MD const* md; //record single MD for IR such like, PR, LD, ST, ID.
    MDSet * mds; //record multpile MD for IR such like, ILD, IST, ARRAY.

public:
    COPY_CONSTRUCTOR(MDA);
};


//PPSetMgr
class PPSetMgr {
    COPY_CONSTRUCTOR(PPSetMgr);
    SMemPool * m_pool;
    SMemPool * m_ppset_pool;
    SList<PtPairSet*> m_ppset_list;
    DefMiscBitSetMgr m_sbs_mgr;

    PtPairSet * xmalloc_ppset()
    {
        PtPairSet * p = (PtPairSet*)smpoolMallocConstSize(
            sizeof(PtPairSet), m_ppset_pool);
        ASSERT0(p);
        ::memset(p, 0, sizeof(PtPairSet));
        p->init();
        return p;
    }
public:
    PPSetMgr()
    {
        m_pool = smpoolCreate(sizeof(xcom::SC<PtPairSet*>) * 4, MEM_CONST_SIZE);
        m_ppset_pool = smpoolCreate(sizeof(PtPairSet) * 4, MEM_CONST_SIZE);
        m_ppset_list.set_pool(m_pool);
    }
    ~PPSetMgr()
    {
        for (xcom::SC<PtPairSet*> * sc = m_ppset_list.get_head();
             sc != m_ppset_list.end(); sc = m_ppset_list.get_next(sc)) {
            PtPairSet * pps = sc->val();
            ASSERT0(pps);
            pps->clean(*getSBSMgr());
        }
        smpoolDelete(m_pool);
        smpoolDelete(m_ppset_pool);
    }

    PtPairSet * allocPtPairSet()
    {
        PtPairSet * pps = xmalloc_ppset();
        m_ppset_list.append_head(pps);
        return pps;
    }

    //Count memory usage for current object.
    size_t count_mem() const;

    DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }
};


//PtPairMgr
class PtPairMgr {
    TMap<UINT, TMap<UINT, PtPair*>*> m_from_tmap;
    Vector<PtPair*> m_id2pt_pair;
    SMemPool * m_pool_pt_pair; //pool of PtPair
    SMemPool * m_pool_tmap; //pool of TMap<UINT, PtPair*>
    UINT m_pp_count;

    inline PtPair * xmalloc_pt_pair()
    {
        PtPair * p = (PtPair*)smpoolMallocConstSize(sizeof(PtPair),
                                                    m_pool_pt_pair);
        ASSERT0(p);
        ::memset(p, 0, sizeof(PtPair));
        return p;
    }

    inline TMap<UINT, PtPair*> * xmalloc_tmap()
    {
        TMap<UINT, PtPair*> * p = (TMap<UINT, PtPair*>*)smpoolMallocConstSize(
            sizeof(TMap<UINT, PtPair*>), m_pool_tmap);
        ASSERT0(p);
        ::memset(p, 0, sizeof(TMap<UINT, PtPair*>));
        return p;
    }

public:
    PtPairMgr()
    {
        m_pool_pt_pair = nullptr;
        m_pool_tmap = nullptr;
        init();
    }
    COPY_CONSTRUCTOR(PtPairMgr);
    ~PtPairMgr() { destroy(); }

    void init()
    {
        if (m_pool_pt_pair != nullptr) { return; }
        m_pp_count = 1;
        m_pool_pt_pair = smpoolCreate(sizeof(PtPair), MEM_CONST_SIZE);
        m_pool_tmap = smpoolCreate(sizeof(TMap<UINT, PtPair*>),
                                   MEM_CONST_SIZE);
    }

    void destroy()
    {
        if (m_pool_pt_pair == nullptr) { return; }

        TMapIter<UINT, TMap<UINT, PtPair*>*> ti;
        TMap<UINT, PtPair*> * v = nullptr;
        for (m_from_tmap.get_first(ti, &v);
             v != nullptr; m_from_tmap.get_next(ti, &v)) {
            v->destroy();
        }
        m_pp_count = 0;

        smpoolDelete(m_pool_pt_pair);
        smpoolDelete(m_pool_tmap);
        m_pool_pt_pair = nullptr;
        m_pool_tmap = nullptr;
    }

    void clean()
    {
        destroy();
        m_from_tmap.clean();
        m_id2pt_pair.clean();
    }

    //Get PT PAIR from 'id'.
    PtPair * get(UINT id) { return m_id2pt_pair.get(id); }

    PtPair * add(UINT from, UINT to);
    //Count memory usage for current object.
    size_t count_mem() const;
};


#define AC_is_mds_mod(c) ((c)->u1.s1.is_mds_modify)
#define AC_is_lda_base(c) ((c)->u1.s1.is_lda_base)
#define AC_is_addr_taken(c) ((c)->u1.s1.is_addr_taken)
#define AC_comp_pts(c) ((c)->u1.s1.comp_pts)
#define AC_hashed_mds(c) ((c)->u2.hashed_mds)
class AACtx {
public:
    union {
        struct {
            //Transfer flag bottom up to indicate whether new
            //MDS generate while processing kids.
            UINT is_mds_modify:1; //Analysis context for AliasAnalysis.

            //Transfer flag top down to indicate the Parent or Grandfather
            //IR (or Parent of grandfather ...) is IR_LDA/IR_ARRAY.
            //This flag tell the current process whether if we are processing
            //the base of LDA/ARRAY.
            UINT is_lda_base:1; //Set true to indicate we are
                                //analysing the base of LDA.

            //Transfer flag bottom up to indicate whether we has taken address
            //of ID.
            //e.g: If p=q,q->x; We would propagate q and get p->x.
            //    But if p=&a; We should get p->a, with offset is 0.
            //    Similarly, if p=(&a)+n+m+z; We also get p->a,
            //    but with offset is INVALID.
            UINT is_addr_taken:1;

            //Transfer flag top down to indicate that we need
            //current function to compute the MD that the IR
            //expression may pointed to.
            //e.g: Given (p+1), we want to know the expression IR_ADD
            //pointed to. The POINT-TO set recorded in hashed_mds or parameter
            //that used to record output MDSet.
            //Presumedly, given p->&a[0], we can figure out MD that
            //dereferencing the expression *(p+1) is identical to a[1].
            UINT comp_pts:1;
        } s1;
        UINT i1;
    } u1;

    union {
        //Transfer hashed POINT-TO set bottom up when finish processing kids.
        //Note inference of POINT-TO set can transfer the middle result either
        //through 'hashed_mds' or 'mds' of parameter.
        MDSet const* hashed_mds;
    } u2;

public:
    AACtx() { clean(); }
    AACtx const& operator = (AACtx const&);
    AACtx(AACtx const& ic) { copy(ic); }

    void copy(AACtx const& ic)
    {
        u1.i1 = ic.u1.i1;
        AC_hashed_mds(this) = nullptr;
    }

    //Only copy top down flag.
    inline void copyTopDownFlag(AACtx const& ic)
    {
        AC_comp_pts(this) = AC_comp_pts(&ic);
        AC_is_lda_base(this) = AC_is_lda_base(&ic);
        AC_hashed_mds(this) = nullptr;
    }

    void clean() { u1.i1 = 0; AC_hashed_mds(this) = nullptr; }

    //Clean these flag when processing each individiual IR trees.
    inline void cleanBottomUpFlag()
    {
        AC_is_addr_taken(this) = 0;
        AC_is_mds_mod(this) = 0;
        AC_hashed_mds(this) = nullptr;
    }

    //Collect the bottom-up flag and use them to direct parent action.
    //Clean these flag when processing each individiual IR trees.
    inline void copyBottomUpFlag(AACtx const& ic)
    {
        AC_is_addr_taken(this) = AC_is_addr_taken(&ic);
        AC_is_mds_mod(this) = AC_is_mds_mod(&ic);
        AC_hashed_mds(this) = AC_hashed_mds(&ic);
    }

    MDSet const* get_hashed() const { return AC_hashed_mds(this); }

    bool is_mds_mod() const { return AC_is_mds_mod(this); }
    bool is_comp_pts() const { return AC_comp_pts(this); }
    bool is_addr_taken() const { return AC_is_addr_taken(this); }
};


typedef TMap<Var*, MD const*, CompareVar> Var2MD;
typedef TMap<IR const*, MD const*> IR2Heapobj;


//Alias Analysis.
//1. Compute the Memory Address Set for IR expression.
//2. Compute the POINT TO Set for individual memory address.
//
//NOTICE:
//Heap is an unique object. That means the whole
//HEAP is modified/referrenced if a LOAD/STORE operates
//MD that describes variable belongs to HEAP.
class AliasAnalysis : public Pass {
protected:
    //If the flag is true, flow sensitive analysis is performed.
    //Or perform flow insensitive.
    BYTE m_flow_sensitive:1;
    IRCFG * m_cfg;
    GSCC * m_scc;
    VarMgr * m_var_mgr;
    TypeMgr * m_tm;
    Region * m_rg;
    RegionMgr * m_rgmgr;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    MDSetHash * m_mds_hash; //MDSet hash table.
    PRSSAMgr * m_prssamgr;

    //This is a dummy global variable.
    //It is used used as a placeholder if there
    //is not any effect global variable.
    MD * m_dummy_global;

    //This class contains those variables that can be referenced by
    //pointers (address-taken variables)
    MDSet const* m_maypts; //initialized by initMayPointToSet()
    DefMiscBitSetMgr m_sbs_mgr;
    IR2Heapobj m_ir2heapobj;
    Vector<PtPairSet*> m_in_pp_set;
    Vector<PtPairSet*> m_out_pp_set;
    Var2MD m_dedicated_var2md; //Record MD to individual variables.
    PtPairMgr m_ppmgr;
    xcom::BitSet m_is_visit;
    xcom::BitSet m_id2heap_md_map;
    MD2MDSet m_unique_md2mds;

    //Analysis context. Record MD->MDSet for each BB.
    Vector<MD2MDSet*> m_md2mds_vec;
protected:
    MD const* allocHeapobj(IR * ir);
    MD const* assignIdMD(IN IR * ir, MOD MDSet * mds, MOD AACtx * ic);
    MD const* assignLoadMD(IN IR * ir, MOD MDSet * mds, MOD AACtx * ic,
                           MOD MD2MDSet * mx);
    MD const* assignPRMD(IN IR * ir, MOD MDSet * mds, MOD AACtx * ic,
                         MOD MD2MDSet * mx);

    //The function unites POINT-TO set into 'output' for each element in 'mds'.
    void collectPointToForElem(MDSet const& mds, MD2MDSet const* mx,
                               OUT MDSet * united, OUT MDSet const** hashed);
    void computeResultSet(MDSet const& input_mds, MDSet const* input_hashed,
                          OUT MDSet & output_mds, OUT AACtx * output_ic);

    //Function return the POINT-TO pair for each BB.
    //Only used in flow-sensitive analysis.
    inline MD2MDSet * genMD2MDSetForBB(UINT bbid)
    {
        MD2MDSet * mx = m_md2mds_vec.get(bbid);
        if (mx == nullptr) {
            mx = (MD2MDSet*)xmalloc(sizeof(MD2MDSet));
            mx->init();
            m_md2mds_vec.set(bbid, mx);
        }
        return mx;
    }

    void convertPT2MD2MDSet(PtPairSet const& pps, PtPairMgr const& ppmgr,
                            MOD MD2MDSet * ctx);
    bool convertMD2MDSet2PT(OUT PtPairSet * pps, IN PtPairMgr & ppmgr,
                            IN PPSetMgr & ppsetmgr, IN MD2MDSet * mx);
    void convertExact2Unbound(MDSet const& src, MDSet * tgt);

    //Do NOT public functions related to PtPair.
    //They are inavailable after AA finished.
    PtPairSet * getInPtPairSet(IRBB const* bb)
    {
        ASSERTN(m_in_pp_set.get(bb->id()),
                ("IN set is not yet initialized for BB%d", bb->id()));
        return m_in_pp_set.get(bb->id());
    }
    //Do NOT public functions related to PtPair.
    //They are inavailable after AA finished.
    PtPairSet * getOutPtPairSet(IRBB const* bb)
    {
        ASSERTN(m_out_pp_set.get(bb->id()),
                ("OUT set is not yet initialized for BB%d", bb->id()));
        return m_out_pp_set.get(bb->id());
    }

    //Return true if POINT-TO is evaluated from LDA.
    bool evaluateFromLda(IR const* ir);

    bool isInLoop(IR const* ir);
    //Determine if flow sensitive analysis is properly.
    bool isFlowSensitiveProperly();
    void initBBPPSet(PPSetMgr & ppsetmgr);
    void initFlowSensitiveEntryPTS(PPSetMgr & ppsetmgr);
    void initEntryPTS(PPSetMgr & ppsetmgr);
    void initGlobalAndParameterPTS(Var * v, MD2MDSet * mx);
    void inferPointerArithByHashedPTS(IR const* ir, OUT MDSet & mds,
                                      MOD AACtx * opnd0_ic);
    void inferPointerArithByUnHashedPTS(IR const* ir, OUT MDSet & mds,
                                        MDSet const& opnd0_mds);
    void inferPointerArith(IR const* ir, OUT MDSet & mds,
                           MDSet const& opnd0_mds, MOD AACtx * opnd0_ic,
                           MOD MD2MDSet * mx);
    //rhs: RHS of ir, ir should be stmt. It's reference MD will be computed.
    void inferRHSAndUpdateLHS(IR const* ir, IR * rhs, MD const* mustref,
                              MDSet const* mayref, AACtx const* ic,
                              MOD MD2MDSet * mx);
    void inferStoreValue(IR const* ir, IR * rhs, MD const* lhs_md,
                         AACtx const* ic, IN MD2MDSet * mx);
    //The function compute may memory address or point-to set for array operation.
    //Note the function handle the worst case when infer Point-To for array base
    //expression.
    //'ir': array|starray operator.
    //'is_ofst_predicable': true if array element offset is constant.
    //This function will set the Ref MD and Ref MD set of array operation.
    void inferArrayExpBaseHashedMDSet(IR * ir, MDSet const* hashed_mds,
                                      OUT MDSet & mds, OUT AACtx * ic);
    void inferArrayInfinite(INT ofst, bool is_ofst_pred, UINT md_size,
                            MDSet const& in, OUT MDSet & out);
    MD const* inferArrayLdabase(IR * ir, IR * array_base, bool is_ofst_pred,
                                UINT ofst, MOD AACtx * ic);
    void inferExpression(IR * ir, MOD MDSet & mds, MOD AACtx * ic,
                         MOD MD2MDSet * mx);
    void inferArrayExpBase(IR * ir, IR * array_base, bool is_ofst_predicable,
                           UINT ofst, OUT MDSet & mds,
                           MOD AACtx * ic, MOD MD2MDSet * mx);

    MD const* processLda(IR * ir, MOD AACtx * ic);
    void processCvt(IR const* ir, MOD MDSet & mds, MOD AACtx * ic,
                    MOD MD2MDSet * mx);
    void processGetelem(IR * ir, MOD MDSet & mds, MOD AACtx * ic,
                        MOD MD2MDSet * mx);
    void processGetelem(IR * ir, MOD MD2MDSet * mx);
    void processSetelem(IR * ir, MOD MD2MDSet * mx);
    void processILoad(IR * ir, MOD MDSet & mds, MOD AACtx * ic,
                      MOD MD2MDSet * mx);
    void processPointerArith(IR * ir, MOD MDSet & mds, MOD AACtx * ic,
                             MOD MD2MDSet * mx);
    void processArray(IR * ir, MOD MDSet & mds, MOD AACtx * ic,
                      MOD MD2MDSet * mx);
    void processConst(IR * ir, MOD MDSet & mds, MOD AACtx * ic);
    void processStore(IN IR * ir, MOD MD2MDSet * mx);
    void processStorePR(IN IR * ir, MOD MD2MDSet * mx);
    void processIStore(IN IR * ir, MOD MD2MDSet * mx);
    void processStoreArray(IN IR * ir, MOD MD2MDSet * mx);
    void processPhi(IN IR * ir, MOD MD2MDSet * mx);
    void processCallSideeffect(MOD MD2MDSet & mx, MDSet const& by_addr_mds);
    void processCall(IN IR * ir, MOD MD2MDSet * mx);
    void processReturn(IN IR * ir, MOD MD2MDSet * mx);
    void processRegionSideeffect(MOD MD2MDSet & mx);
    void processRegion(IR const* ir, MOD MD2MDSet * mx);
    void processArrayHashed(IR * ir, MDSet const* hashed,
                            MD2MDSet const* mx, OUT MDSet & mds,
                            MOD AACtx * ic);

    //This function set MustAddr or MayAddr of ir by analyszing given MDSet.
    //mds: mds may be the MayAddr MDSet. Note mds should have been hashed.
    void setMustOrMayAddr(MDSet const* mds, IR * ir);
    //hashed_mds: MDSet that already be hashed.
    void setIRRefHashed(IR * ir, MDSet const* hashed_mds);
    //mds: MDSet that may NOT be hashed.
    void setIRRef(IR * ir, MDSet const* mds);

    //Return true if new POINT_TO info generated.
    bool tryToEvaluateConstOffset(IR const* ir, OUT MDSet & mds,
                                  MDSet const& opnd0_mds,
                                  MOD AACtx * opnd0_ic);
    //Reshape MD in 'mds' if one of them need to be reshape.
    //Record reshaped MD in 'newmds' and return true.
    //This function will iterate MD in 'mds', new MD will be generated either
    //ir's MD size is different to MD in 'mds' or ir has offset.
    //Return true if new MD generated, and new MD record in 'newmds'.
    bool tryReshapeMDSet(IR const* ir, MDSet const* mds, OUT MDSet * newmds);
    bool tryComputeConstOffset(IR const* ir, MDSet const& opnd0_mds,
                               IR const* opnd1, MOD MDSet & mds,
                               bool * changed);

    void recomputeDataType(IR const* ir, AACtx const& ic, MOD MDSet & pts);
    MD const* reviseMDSize(IR const* ir, MD const* md);
    void reviseMDSize(MOD MDSet & mds, UINT size);

    //refmds: ref MDSet of ir.
    //ir: given indirect operation, such as IST, ILD.
    //comp_ir_pt: true if caller require to compute the POINT-TO set of ir.
    //Return POINT-TO set of ir, if comp_ir_pts is true.
    MDSet const* updateIndirectOpAddrAndPointToSet(MDSet const* refmds,
                                                   IR * ir, bool comp_ir_pts,
                                                   MD2MDSet * mx);
    bool usePRSSADU() const;

    inline void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }
public:
    explicit AliasAnalysis(Region * rg);
    COPY_CONSTRUCTOR(AliasAnalysis);
    virtual ~AliasAnalysis();

    //Attemp to compute the type based may point to MD set.
    //Return true if this function find the point-to MD set, otherwise
    //return false.
    virtual MD const* computePointToViaType(IR const*) { return nullptr; }

    void clean();
    void cleanSBSMgr();
    void cleanPointTo(UINT mdid, MOD MD2MDSet & ctx)
    { ctx.setAlways(mdid, nullptr); }

    //Compute and update point_to_set with TBAA info.
    //pointer: IR expression that pointed to memory.
    //point_to_set: POINT_TO set of pointer, genernate new set if TBAA exist.
    //Return true if pointer pointed to MAY-POINT-TO set.
    MDSet const* computeMayPointToViaTBAA(IR const* pointer,
                                          MDSet const* point_to_set);
    bool computeFlowSensitive(List<IRBB*> const& bbl, PPSetMgr & ppsetmgr);
    void computeStmt(IRBB const* bb, MOD MD2MDSet * mx);
    void computeFlowInsensitive();
    //Count memory usage for current object.
    size_t count_mem() const;
    size_t countMD2MDSetMemory() const;
    void cleanContext();

    void destroyContext();
    void dumpMD2MDSet(MD2MDSet const* mx, bool dump_ptg) const;
    void dumpMD2MDSet(MD const* md, MD2MDSet const* mx) const;
    void dumpIRPointTo(IR const* ir, bool dump_kid, MD2MDSet const* mx) const;
    void dumpIRPointToForBB(IRBB const* bb, bool dump_kid) const;
    void dumpIRPointToForRegion(bool dump_kid) const;
    void dumpPtPairSet(PtPairSet const& pps) const;
    void dumpInOutPointToSetForBB() const;
    void dumpMD2MDSetForRegion(bool dump_pt_graph) const;
    void dumpMayPointTo() const;
    virtual bool dump() const;

    void ElemUnionPointTo(MDSet const& mds, MDSet const& in_set,
                          IN MD2MDSet * mx);
    void ElemUnionPointTo(MDSet const& mds, MD const* in_elem,
                          IN MD2MDSet * mx);
    void ElemCopyPointToAndMayPointTo(MDSet const& mds, IN MD2MDSet * mx);
    void ElemCopyAndUnionPointTo(MDSet const& mds, MDSet const& pt_set,
                                 IN MD2MDSet * mx);
    void ElemCleanPointTo(MDSet const& mds, IN MD2MDSet * mx);
    void ElemCleanExactPointTo(MDSet const& mds, IN MD2MDSet * mx);

    Region * getRegion() const { return m_rg; }
    virtual CHAR const* getPassName() const { return "Alias Analysis"; }
    virtual PASS_TYPE getPassType() const { return PASS_AA; }
    DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }

    //For given MD2MDSet, return the MDSet that 'md' pointed to.
    //ctx: context of point-to analysis.
    MDSet const* getPointTo(UINT mdid, MD2MDSet const& ctx) const
    { return ctx.get(mdid); }

    //Return the may-point-to Memory Descriptor Set.
    MDSet const* getMayPointToMDSet() const { return m_maypts; }
    MDSet const* getWorstCase() const { return getMayPointToMDSet(); }
    MD2MDSet * getUniqueMD2MDSet() { return &m_unique_md2mds; }

    void initAliasAnalysis();
    //Return true if Alias Analysis has initialized.
    bool is_init() const { return m_maypts != nullptr; }
    bool isFlowSensitive() const { return m_flow_sensitive; }
    bool isValidStmtToAA(IR const* ir) const;
    bool isHeapMem(UINT mdid) const
    { //return mdid == MD_HEAP_MEM ? true : false;
      DUMMYUSE(mdid);
      return false;
    }
    //Return true if the MD of each PR corresponded is unique.
    void initMayPointToSet();
    //Return true if the set indicates the worst case of MD reference set.
    bool isWorstCase(MDSet const* set) const
    { return set == getMayPointToMDSet(); }

    void setMayPointToMDSet(MDSet const* set) { m_maypts = set; }

    //For given MD2MDSet, set the point-to set to 'md'.
    //ctx: context of point-to analysis.
    void setPointTo(UINT mdid, MD2MDSet & ctx, MDSet const* pts)
    {
        ASSERT0(pts);
        ASSERTN(m_mds_hash->find(*pts), ("pts should be in hash"));
        ctx.setAlways(mdid, pts);
    }

    //Set pointer points to 'target'.
    inline void setPointToUniqueMD(UINT pointer_mdid, MD2MDSet & ctx,
                                   MD const* target)
    {
        ASSERT0(target);
        MDSet tmp;
        tmp.bunion(target, *getSBSMgr());
        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer_mdid, ctx, hashed);
        tmp.clean(*getSBSMgr());
    }

    //Set pointer points to 'target_set' in the context.
    void setPointToMDSet(UINT pointer_mdid, MD2MDSet & ctx,
                         MDSet const& target_set)
    {
        ASSERTN(!m_mds_hash->find(target_set),
                ("already hashed, there might be redundant function call"));
        MDSet const* hashed = m_mds_hash->append(target_set);
        setPointTo(pointer_mdid, ctx, hashed);
    }

    //Set pointer points to new MDSet by appending a new element 'newmd'
    //in the context.
    inline void setPointToMDSetByAddMD(UINT pointer_mdid, MD2MDSet & ctx,
                                       MD const* newmd)
    {
        MDSet tmp;
        MDSet const* pts = getPointTo(pointer_mdid, ctx);
        if (pts != nullptr) {
            if (pts->is_contain(newmd) || isWorstCase(pts)) {
                ASSERT0(m_mds_hash->find(*pts));
                return;
            }
            tmp.bunion(*pts, *getSBSMgr());
        }

        tmp.bunion(newmd, *getSBSMgr());
        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer_mdid, ctx, hashed);
        tmp.clean(*getSBSMgr());
    }

    //Set pointer points to MD set by appending a MDSet.
    //pt_set: POINT-TO set that has been hashed.
    inline void setPointToMDSetByAddMDSet(UINT pointer_mdid, MD2MDSet & ctx,
                                          MDSet const& pt_set)
    {
        ASSERT0(m_mds_hash->find(pt_set));
        MDSet const* pts = getPointTo(pointer_mdid, ctx);
        if (pts == nullptr) {
            setPointTo(pointer_mdid, ctx, &pt_set);
            return;
        }

        if (isWorstCase(&pt_set)) {
            setPointTo(pointer_mdid, ctx, &pt_set);
            return;
        }

        //Generate new POINT-TO set and set.
        MDSet tmp;
        tmp.copy(*pts, *getSBSMgr());
        tmp.bunion(pt_set, *getSBSMgr());
        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer_mdid, ctx, hashed);
        tmp.clean(*getSBSMgr());
    }

    //Set 'md' points to whole memory.
    void setPointToAllMem(UINT mdid, OUT MD2MDSet & ctx)
    { setPointToMDSetByAddMD(mdid, ctx, m_md_sys->getMD(MD_FULL_MEM)); }

    //Set md points to whole global memory.
    void setPointToGlobalMem(UINT mdid, OUT MD2MDSet & ctx)
    { setPointToMDSetByAddMD(mdid, ctx, m_md_sys->getMD(MD_GLOBAL_VAR)); }

    //Set md points to imported variable.
    void setPointToImportVar(UINT mdid, OUT MD2MDSet & ctx)
    { setPointToMDSetByAddMD(mdid, ctx, m_md_sys->getMD(MD_IMPORT_VAR)); }

    void set_flow_sensitive(bool is_sensitive)
    { m_flow_sensitive = (BYTE)is_sensitive; }

    //Set the POINT-TO set of LHS MD and LHS MDSet.
    //pts: POINT-TO set that have been hashed.
    void setLHSPointToSet(MD const* lhs_mustaddr, MDSet const* lhs_mayaddr,
                          MDSet const* pts, IN MD2MDSet * mx);

    //Function return the POINT-TO pair for each BB.
    //Only used in flow-sensitive analysis.
    MD2MDSet * mapBBtoMD2MDSet(UINT bbid) const
    { return m_md2mds_vec.get(bbid); }

    //This function update LHS's POINT-TO set accroding to RHS.
    //is_lhs_pointer: true if transit rhs's POINT-TO set to lhs.
    //rhs: RHS expression of stmt, which are IR_ST, IR_IST, IR_STARRAY.
    //rhsrefmds: record memory descriptor of 'rhs' if AC_comp_pts() is false, or
    //           record the POINT-TO set of 'rhs' if AC_comp_pts() is true.
    //           Note if AC_comp_pts() is true, the returned POINT-TO set may be
    //           recorded in 'rhsrefmds' or AC_hashed_mds.
    //hashed_mds: record the POINT-TO set of 'rhs' if AC_comp_pts() is true.
    //            Note if AC_comp_pts() is true, the returned POINT-TO set may
    //            be recorded in 'rhsrefmds' or AC_hashed_mds.
    void updateLHSPointToSet(bool is_lhs_pointer, bool rhs_taken_address,
                             MD const* lhs_mustaddr, MDSet const* lhs_mayaddr,
                             IR const* rhs, MDSet & rhsrefmds,
                             MDSet const* hashed_mds, MD2MDSet * mx);
    //Union POINT-TO set for each element in 'mds', and hash the unified result
    //return it.
    //mds: represents address of current ILD.
    MDSet const* unifyPointToSet(MDSet const& mds, MD2MDSet const* mx);

    bool verifyIR(IR * ir);
    bool verify();

    MD const* queryTBAA(IR const* ir);

    bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
