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

class IR_CFG;

//PtPair
#define PP_id(pp)        ((pp)->id)
#define PP_from(pp)      ((pp)->from)
#define PP_to(pp)        ((pp)->to)
class PtPair {
public:
    UINT id;
    MD const* from;
    MD const* to;
};


//PtPairSet
typedef BitSet PtPairSet;


//MD Addendum
#define MDA_md(m)        ((m)->md)
#define MDA_mds(m)       ((m)->mds)
class MDA {
public:
    MD const* md; //record single MD for IR such like, PR, LD, ST, ID.
    MDSet * mds; //record multpile MD for IR such like, ILD, IST, ARRAY.
};


//PPSetMgr
class PPSetMgr {
protected:
    SMemPool * m_pool;
    SList<PtPairSet*> m_free_pp_set;
    SList<PtPairSet*> m_pp_set_list;
public:
    PPSetMgr()
    {
        m_pool = smpoolCreate(sizeof(SC<PtPairSet*>) * 4,
                              MEM_CONST_SIZE);
        m_free_pp_set.set_pool(m_pool);
        m_pp_set_list.set_pool(m_pool);
    }
    COPY_CONSTRUCTOR(PPSetMgr);
    ~PPSetMgr()
    {
        for (SC<PtPairSet*> * sc = m_pp_set_list.get_head();
             sc != m_pp_set_list.end(); sc = m_pp_set_list.get_next(sc)) {
            PtPairSet * pps = sc->val();
            ASSERT0(pps);
            delete pps;
        }
        smpoolDelete(m_pool);
    }

    UINT count_mem();

    void free(PtPairSet * pps)
    {
        pps->clean();
        m_free_pp_set.append_head(pps);
    }

    inline PtPairSet * newPtPairSet()
    {
        PtPairSet * pps = m_free_pp_set.remove_head();
        if (pps == NULL) {
            pps = new PtPairSet();
            m_pp_set_list.append_head(pps);
        }
        return pps;
    }
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
        memset(p, 0, sizeof(PtPair));
        return p;
    }

    inline TMap<UINT, PtPair*> * xmalloc_tmap()
    {
        TMap<UINT, PtPair*> * p =
            (TMap<UINT, PtPair*>*)smpoolMallocConstSize(
                                    sizeof(TMap<UINT, PtPair*>),
                                    m_pool_tmap);
        ASSERT0(p);
        memset(p, 0, sizeof(TMap<UINT, PtPair*>));
        return p;
    }
public:
    PtPairMgr()
    {
        m_pool_pt_pair = NULL;
        m_pool_tmap = NULL;
        init();
    }
    COPY_CONSTRUCTOR(PtPairMgr);
    ~PtPairMgr() { destroy(); }

    void init()
    {
        if (m_pool_pt_pair != NULL) { return; }
        m_pp_count = 1;
        m_pool_pt_pair = smpoolCreate(sizeof(PtPair), MEM_CONST_SIZE);
        m_pool_tmap = smpoolCreate(sizeof(TMap<UINT, PtPair*>),
                                   MEM_CONST_SIZE);
    }

    void destroy()
    {
        if (m_pool_pt_pair == NULL) { return; }

        TMapIter<UINT, TMap<UINT, PtPair*>*> ti;
        TMap<UINT, PtPair*> * v = NULL;
        for (m_from_tmap.get_first(ti, &v);
             v != NULL; m_from_tmap.get_next(ti, &v)) {
            v->destroy();
        }
        m_pp_count = 0;

        smpoolDelete(m_pool_pt_pair);
        smpoolDelete(m_pool_tmap);
        m_pool_pt_pair = NULL;
        m_pool_tmap = NULL;
    }

    inline void clobber()
    {
        destroy();
        m_from_tmap.clean();
        m_id2pt_pair.clean();
    }

    //Get PT PAIR from 'id'.
    PtPair * get(UINT id) { return m_id2pt_pair.get(id); }

    PtPair * add(MD const* from, MD const* to);

    UINT count_mem() const;
};


//IRAA_CTX
#define AC_is_mds_mod(c)        ((c)->u1.s1.is_mds_modify)
#define AC_is_lda_base(c)       ((c)->u1.s1.is_lda_base)
#define AC_has_comp_lda(c)      ((c)->u1.s1.has_comp_lda)
#define AC_comp_pt(c)           ((c)->u1.s1.comp_pt)
class AACTX {
public:
    union {
        struct {
            //Transfer flag bottom up to indicate whether new
            //MDS generate while processing kids.
            UINT is_mds_modify:1; //Analysis context for IR_AA.

            /* Transfer flag top down to indicate the Parent or Grandfather
            IR (or Parent of grandfather ...) is IR_LDA/IR_ARRAY.
            This flag tell the current process whether if we are processing
            the base of LDA/ARRAY. */
            UINT is_lda_base:1; //Set true to indicate we are
                                //analysing the base of LDA.

            /* Transfer flag bottom up to indicate whether we has taken address
            of ID.
            e.g: If p=q,q->x; We would propagate q and get p->x.
                But if p=&a; We should get p->a, with offset is 0.
                Similarly, if p=(&a)+n+m+z; We also get p->a,
                but with offset is INVALID. */
            UINT has_comp_lda:1;

            /* Transfer flag top down to indicate that we need
            current function to compute the MD that the IR
            expression may pointed to.
            e.g: Given (p+1), we want to know the expression pointed to.
            Presumedly, p->&a[0], we can figure out MD that dereferencing
            the expression *(p+1) is a[1]. */
            UINT comp_pt:1;
        } s1;
        UINT i1;
    } u1;

    AACTX() { clean(); }
    AACTX(AACTX const& ic) { copy(ic); }

    inline void copy(AACTX const& ic) { u1.i1 = ic.u1.i1; }

    //Only copy top down flag.
    inline void copyTopDownFlag(AACTX const& ic)
    {
        AC_comp_pt(this) = AC_comp_pt(&ic);
        AC_is_lda_base(this) = AC_is_lda_base(&ic);
    }

    void clean() { u1.i1 = 0; }

    //Clean these flag when processing each individiual IR trees.
    inline void cleanBottomUpFlag()
    {
        AC_has_comp_lda(this) = 0;
        AC_is_mds_mod(this) = 0;
    }

    //Collect the bottom-up flag and use them to direct parent action.
    //Clean these flag when processing each individiual IR trees.
    inline void copyBottomUpFlag(AACTX const& ic)
    {
        AC_has_comp_lda(this) = AC_has_comp_lda(&ic);
        AC_is_mds_mod(this) = AC_is_mds_mod(&ic);
    }
};


typedef TMap<VAR*, MD const*, CompareVar> Var2MD;
typedef TMap<IR const*, MD const*> IR2Heapobj;


/* Alias Analysis.
1. Compute the Memory Address Set for IR expression.
2. Compute the POINT TO Set for individual memory address.

NOTICE:
Heap is a unique object. That means the whole
HEAP is modified/referrenced if a LOAD/STORE operates
MD that describes variable belongs to HEAP. */
class IR_AA : public Pass {
protected:
    IR_CFG * m_cfg;
    VarMgr * m_var_mgr;
    TypeMgr * m_dm;
    Region * m_ru;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    MDSetMgr * m_mds_mgr; //MDSet manager.
    MDSetHash * m_mds_hash; //MDSet hash table.
    DefMiscBitSetMgr * m_misc_bs_mgr;

    //This is a dummy global variable.
    //It is used used as a placeholder if there
    //is not any effect global variable.
    MD * m_dummy_global;

    IR2Heapobj m_ir2heapobj;
    Vector<PtPairSet*> m_in_pp_set;
    Vector<PtPairSet*> m_out_pp_set;
    Var2MD m_var2md;
    PtPairMgr m_pt_pair_mgr;
    BitSet m_is_visit;

    //This class contains those variables that can be referenced by
    //pointers (address-taken variables)
    MDSet const* m_hashed_maypts; //initialized by initMayPointToSet()

    //Analysis context. Record MD->MDSet for each BB.
    Vector<MD2MDSet*> m_md2mds_vec;
    BitSet m_id2heap_md_map;
    MD2MDSet m_unique_md2mds;

    //If the flag is true, flow sensitive analysis is performed.
    //Or perform flow insensitive.
    BYTE m_flow_sensitive:1;

    //If the flag is true, PR is correspond to a unique MD.
    BYTE m_is_pr_unique_for_same_no:1;

protected:
    MD const* allocHeapobj(IR * ir);
    MD const* assignStringConst(
                    IN IR * ir,
                    IN OUT MDSet * mds,
                    IN OUT AACTX * ic);
    MD const* assignStringIdentifier(
                    IN IR * ir,
                    IN OUT MDSet * mds,
                    IN OUT AACTX * ic);
    MD const* assignIdMD(
                    IN IR * ir,
                    IN OUT MDSet * mds,
                    IN OUT AACTX * ic);
    MD const* assignLoadMD(
                    IN IR * ir,
                    IN OUT MDSet * mds,
                    IN OUT AACTX * ic,
                    IN OUT MD2MDSet * mx);
    MD const* assignPRMD(
                    IN IR * ir,
                    IN OUT MDSet * mds,
                    IN OUT AACTX * ic,
                    IN OUT MD2MDSet * mx);
    MD const* allocIdMD(IR * ir);
    MD const* allocLoadMD(IR * ir);
    MD const* allocStoreMD(IR * ir);
    MD const* allocPRMD(IR * ir);
    MD const* allocPhiMD(IR * phi);
    MD const* allocStorePRMD(IR * ir);
    MD const* allocCallResultPRMD(IR * ir);
    MD const* allocSetelemMD(IR * ir);
    MD const* allocGetelemMD(IR * ir);
    MD const* allocStringMD(IR * ir);

    void convertPT2MD2MDSet(
            PtPairSet const& pps,
            IN PtPairMgr & pt_pair_mgr,
            IN OUT MD2MDSet * ctx);
    void convertMD2MDSet2PT(
            OUT PtPairSet & pps,
            IN PtPairMgr & pt_pair_mgr,
            IN MD2MDSet * mx);

    bool evaluateFromLda(IR const* ir);

    void initEntryPtset(PtPairSet ** ptset_arr);
    void initGlobalAndParameterVarPtset(
            VAR * v,
            MD2MDSet * mx,
            ConstMDIter & iter);
    void inferPtArith(
            IR const* ir, IN OUT MDSet & mds,
            IN OUT MDSet & opnd0_mds,
            IN OUT AACTX * opnd0_ic,
            IN OUT MD2MDSet * mx);
    void inferStoreValue(
            IN IR * ir,
            IN IR * rhs,
            MD const* lhs_md,
            IN AACTX * ic,
            IN MD2MDSet * mx);
    void inferStoreArrayValue(IN IR * ir, IN AACTX * ic, IN MD2MDSet * mx);
    void inferIstoreValue(IN IR * ir, IN AACTX * ic, IN MD2MDSet * mx);
    void inferArrayInfinite(
            INT ofst,
            bool is_ofst_pred,
            UINT md_size,
            MDSet const& in,
            OUT MDSet & out);
    void inferArrayLdabase(
            IR * ir,
            IR * array_base,
            bool is_ofst_pred,
            UINT ofst,
            OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);
    void inferExpression(
            IR * ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);

    void processLda(
            IR * ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);
    void processArrayLdabase(
            IR * ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);
    void processCvt(
            IR const* ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);
    void processGetelem(
            IR * ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);
    void processGetelem(IR * ir, IN MD2MDSet * mx);
    void processSetelem(IR * ir, IN MD2MDSet * mx);
    void processIload(
            IR * ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);
    void processPointerArith(
            IR * ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);
    void processArray(
            IR * ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);
    void processConst(
            IR * ir,
            IN OUT MDSet & mds,
            IN OUT AACTX * ic);
    void processStore(IN IR * ir, IN OUT MD2MDSet * mx);
    void processStorePR(IN IR * ir, IN MD2MDSet * mx);
    void processIstore(IN IR * ir, IN OUT MD2MDSet * mx);
    void processStoreArray(IN IR * ir, IN MD2MDSet * mx);
    void processPhi(IN IR * ir, IN MD2MDSet * mx);
    void processCallSideeffect(
            IN OUT MD2MDSet & mx,
            bool by_addr,
            MDSet const& by_addr_mds);
    void processCall(IN IR * ir, IN OUT MD2MDSet * mx);
    void processReturn(IN IR * ir, IN MD2MDSet * mx);
    void processRegionSideeffect(IN OUT MD2MDSet & mx);
    void processRegion(IR const* ir, IN MD2MDSet * mx);
    void inferArrayExpBase(
            IR * ir,
            IR * array_base,
            bool is_ofst_predicable,
            UINT ofst,
            OUT MDSet & mds,
            OUT bool mds_is_may_pt,
            IN OUT AACTX * ic,
            IN OUT MD2MDSet * mx);

    void reviseMDsize(IN OUT MDSet & mds, UINT size);

    inline void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        memset(p, 0, size);
        return p;
    }
public:
    explicit IR_AA(Region * ru);
    virtual ~IR_AA();

    //Attemp to compute the type based may point to MD set.
    //Return true if this function find the point-to MD set, otherwise
    //return false.
    virtual MD const* computePointToViaType(IR const*) { return NULL; }

    void clean();
    void cleanPointTo(MD const* md, IN OUT MD2MDSet & ctx)
    { ctx.setAlways(MD_id(md), NULL); }

    void computeFlowSensitive(List<IRBB*> const& bbl);
    void computeStmt(IRBB const* bb, IN OUT MD2MDSet * mx);
    void computeFlowInsensitive();
    void computeMayPointTo(IR * pointer, IN MD2MDSet * mx, OUT MDSet & mds);
    void computeMayPointTo(IR * pointer, OUT MDSet & mds);
    UINT count_mem();
    UINT countMD2MDSetMemory();

    void dumpMD2MDSet(IN MD2MDSet * mx, bool dump_ptg);
    void dumpMD2MDSet(MD const* md, IN MD2MDSet * mx);
    void dumpIRPointTo(IN IR * ir, bool dump_kid, IN MD2MDSet * mx);
    void dumpIRPointToForBB(IRBB * bb, bool dump_kid);
    void dumpIRPointToForRegion(bool dump_kid);
    void dumpPtPairSet(PtPairSet & pps);
    void dumpInOutPointToSetForBB();
    void dumpMD2MDSetForRegion(bool dump_pt_graph);
    void dumpMayPointTo();

    void ElemUnionPointTo(MDSet const& mds, IN MDSet & in_set, IN MD2MDSet * mx);
    void ElemUnionPointTo(MDSet const& mds, IN MD * in_elem, IN MD2MDSet * mx);
    void ElemCopyPointTo(MDSet const& mds, IN MDSet & in_set, IN MD2MDSet * mx);
    void ElemCopyPointToAndMayPointTo(MDSet const& mds, IN MD2MDSet * mx);
    void ElemCopyAndUnionPointTo(
            MDSet const& mds,
            IN MDSet & pt_set,
            IN MD2MDSet * mx);
    void ElemCleanPointTo(MDSet const& mds, IN MD2MDSet * mx);
    void ElemCleanExactPointTo(MDSet const& mds, IN MD2MDSet * mx);

    virtual CHAR const* get_pass_name() const { return "Alias Analysis"; }
    virtual PASS_TYPE get_pass_type() const { return PASS_AA; }
    inline PtPairSet * getInPtPairSet(IRBB const* bb)
    {
        PtPairSet * pps = m_in_pp_set.get(BB_id(bb));
        ASSERT(pps, ("IN set is not yet initialized for BB%d", BB_id(bb)));
        return pps;
    }

    inline PtPairSet * getOutPtPairSet(IRBB const* bb)
    {
        PtPairSet * pps = m_out_pp_set.get(BB_id(bb));
        ASSERT(pps, ("OUT set is not yet initialized for BB%d", BB_id(bb)));
        return pps;
    }

    //For given MD2MDSet, return the MDSet that 'md' pointed to.
    //ctx: context of point-to analysis.
    inline MDSet const* getPointTo(MD const* md, MD2MDSet & ctx) const
    {
        ASSERT0(md);
        return getPointTo(MD_id(md), ctx);
    }

    //For given MD2MDSet, return the MDSet that 'md' pointed to.
    //ctx: context of point-to analysis.
    inline MDSet const* getPointTo(UINT mdid, MD2MDSet & ctx) const
    { return ctx.get(mdid); }

    //Return the Memory Descriptor Set for given ir may describe.
    MDSet const* get_may_addr(IR const* ir) { return ir->get_ref_mds(); }

    //Return the MemoryAddr for 'ir' must be.
    MD const* get_must_addr(IR const* ir) { return ir->get_ref_md(); }

    MD2MDSet * get_unique_md2mds() { return &m_unique_md2mds; }

    void initAliasAnalysis();
    bool is_flow_sensitive() const { return m_flow_sensitive; }
    bool isValidStmtToAA(IR * ir);

    bool is_all_mem(MD const* md) const
    { return (MD_id(md) == MD_ALL_MEM) ? true : false; }

    bool is_heap_mem(MD const*) const
    { //return (MD_id(md) == MD_HEAP_MEM) ? true : false;
        return false;
    }

    //Return true if the MD of each PR corresponded is unique.
    bool isPRUniqueForSameNo() const { return m_is_pr_unique_for_same_no; }
    void initMayPointToSet();

    void cleanContext(OptCTX & oc);
    void destroyContext(OptCTX & oc);

    void set_must_addr(IR * ir, MD const* md)
    {
        ASSERT0(ir != NULL && md);
        ir->set_ref_md(md, m_ru);
    }
    void set_may_addr(IR * ir, MDSet const* mds)
    {
        ASSERT0(ir && mds && !mds->is_empty());
        ir->set_ref_mds(mds, m_ru);
    }

    //For given MD2MDSet, set the point-to set to 'md'.
    //ctx: context of point-to analysis.
    inline void setPointTo(MD const* md, MD2MDSet & ctx, MDSet const* ptset)
    {
        ASSERT0(md && ptset);
        ASSERT(m_mds_hash->find(*ptset), ("ptset should be in hash"));
        ctx.setAlways(MD_id(md), ptset);
    }

    //Set pointer points to 'target'.
    inline void setPointToUniqueMD(
                    MD const* pointer,
                    MD2MDSet & ctx,
                    MD const* target)
    {
        ASSERT0(pointer && target);
        MDSet tmp;
        tmp.bunion(target, *m_misc_bs_mgr);
        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer, ctx, hashed);
        tmp.clean(*m_misc_bs_mgr);
    }

    //Set pointer points to 'target_set' in the context.
    inline void setPointToMDSet(
                   MD const* pointer,
                   MD2MDSet & ctx,
                   MDSet const& target_set)
    {
        ASSERT0(pointer);
        MDSet const* hashed = m_mds_hash->append(target_set);
        setPointTo(pointer, ctx, hashed);
    }

    //Set pointer points to new MDSet by appending a new element 'newmd'
    //in the context.
    inline void setPointToMDSetByAddMD(
                    MD const* pointer,
                    MD2MDSet & ctx,
                    MD const* newmd)
    {
        MDSet tmp;
        tmp.bunion(newmd, *m_misc_bs_mgr);

        MDSet const* pts = getPointTo(pointer, ctx);
        if (pts != NULL) {
            tmp.bunion(*pts, *m_misc_bs_mgr);
        }

        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer, ctx, hashed);
        tmp.clean(*m_misc_bs_mgr);
    }

    //Set pointer points to MD set by appending a MDSet.
    inline void setPointToMDSetByAddMDSet(
                    MD const* pointer,
                    MD2MDSet & ctx,
                    MDSet const& set)
    {
        MDSet const* pts = getPointTo(pointer, ctx);
        if (pts == NULL) {
            MDSet const* hashed = m_mds_hash->append(set);
            setPointTo(pointer, ctx, hashed);
            return;
        }

        MDSet tmp;
        tmp.copy(*pts, *m_misc_bs_mgr);
        tmp.bunion(set, *m_misc_bs_mgr);

        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer, ctx, hashed);
        tmp.clean(*m_misc_bs_mgr);
    }

    //Set 'md' points to whole memory.
    void setPointToAllMem(MD const* md, OUT MD2MDSet & ctx)
    { setPointToMDSetByAddMD(md, ctx, m_md_sys->get_md(MD_ALL_MEM)); }

    //Set md points to whole global memory.
    void setPointToGlobalMem(MD const* md, OUT MD2MDSet & ctx)
    { setPointToMDSetByAddMD(md, ctx, m_md_sys->get_md(MD_GLOBAL_MEM)); }

    void set_flow_sensitive(bool is_sensitive)
    { m_flow_sensitive = (BYTE)is_sensitive; }

    //Function return the POINT-TO pair for each BB.
    //Only used in flow-sensitive analysis.
    MD2MDSet * mapBBtoMD2MDSet(IRBB const* bb) const
    { return m_md2mds_vec.get(BB_id(bb)); }

    //Function return the POINT-TO pair for each BB.
    //Only used in flow-sensitive analysis.
    inline MD2MDSet * allocMD2MDSetForBB(IRBB const* bb)
    {
        MD2MDSet * mx = m_md2mds_vec.get(BB_id(bb));
        if (mx == NULL) {
            mx = (MD2MDSet*)xmalloc(sizeof(MD2MDSet));
            mx->init();
            m_md2mds_vec.set(BB_id(bb), mx);
        }
        return mx;
    }

    void markMayAlias(IN IRBB * bb, IN MD2MDSet * mx);
    bool verifyIR(IR * ir);
    bool verify();
    bool perform(OptCTX & oc);
};

} //namespace xoc
#endif
