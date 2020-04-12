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
typedef xcom::BitSet PtPairSet;


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
protected:
    SMemPool * m_pool;
    SList<PtPairSet*> m_free_pp_set;
    SList<PtPairSet*> m_pp_set_list;

public:
    PPSetMgr()
    {
        m_pool = smpoolCreate(sizeof(xcom::SC<PtPairSet*>) * 4, MEM_CONST_SIZE);
        m_free_pp_set.set_pool(m_pool);
        m_pp_set_list.set_pool(m_pool);
    }
    COPY_CONSTRUCTOR(PPSetMgr);
    ~PPSetMgr()
    {
        for (xcom::SC<PtPairSet*> * sc = m_pp_set_list.get_head();
             sc != m_pp_set_list.end(); sc = m_pp_set_list.get_next(sc)) {
            PtPairSet * pps = sc->val();
            ASSERT0(pps);
            delete pps;
        }
        smpoolDelete(m_pool);
    }

    //Count memory usage for current object.
    size_t count_mem();

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

    PtPair * add(UINT from, UINT to);
    //Count memory usage for current object.
    size_t count_mem() const;
};


#define AC_is_mds_mod(c) ((c)->u1.s1.is_mds_modify)
#define AC_is_lda_base(c) ((c)->u1.s1.is_lda_base)
#define AC_has_comp_lda(c) ((c)->u1.s1.has_comp_lda)
#define AC_comp_pt(c) ((c)->u1.s1.comp_pt)
#define AC_returned_pts(c) ((c)->u2.returned_pts)
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
            UINT has_comp_lda:1;

            //Transfer flag top down to indicate that we need
            //current function to compute the MD that the IR
            //expression may pointed to.
            //e.g: Given (p+1), we want to know the expression IR_ADD
            //pointed to. The POINT-TO set recorded in returned_pts or parameter
            //that used to record output MDSet.
            //Presumedly, p->&a[0], we can figure out MD that dereferencing
            //the expression *(p+1) is a[1].
            UINT comp_pt:1;
        } s1;
        UINT i1;
    } u1;

    union {
        //Transfer hashed POINT-TO set bottom up when finish processing kids.
        //Note inference of POINT-TO set can transfer the middle result either
        //through 'returned_ptr' or 'mds' of parameter.
        MDSet const* returned_pts;
    } u2;

public:
    AACtx() { clean(); }
    AACtx const& operator = (AACtx const&);
    AACtx(AACtx const& ic) { copy(ic); }

    void copy(AACtx const& ic)
    {
        u1.i1 = ic.u1.i1;
        AC_returned_pts(this) = NULL;
    }

    //Only copy top down flag.
    inline void copyTopDownFlag(AACtx const& ic)
    {
        AC_comp_pt(this) = AC_comp_pt(&ic);
        AC_is_lda_base(this) = AC_is_lda_base(&ic);
        AC_returned_pts(this) = NULL;
    }

    void clean() { u1.i1 = 0; AC_returned_pts(this) = NULL; }

    //Clean these flag when processing each individiual IR trees.
    inline void cleanBottomUpFlag()
    {
        AC_has_comp_lda(this) = 0;
        AC_is_mds_mod(this) = 0;
        AC_returned_pts(this) = NULL;
    }

    //Collect the bottom-up flag and use them to direct parent action.
    //Clean these flag when processing each individiual IR trees.
    inline void copyBottomUpFlag(AACtx const& ic)
    {
        AC_has_comp_lda(this) = AC_has_comp_lda(&ic);
        AC_is_mds_mod(this) = AC_is_mds_mod(&ic);
        AC_returned_pts(this) = AC_returned_pts(&ic);
    }
};


typedef TMap<VAR*, MD const*, CompareVar> Var2MD;
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
    IRCFG * m_cfg;
    VarMgr * m_var_mgr;
    TypeMgr * m_tm;
    Region * m_rg;
    RegionMgr * m_rgmgr;
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
    xcom::BitSet m_is_visit;

    //This class contains those variables that can be referenced by
    //pointers (address-taken variables)
    MDSet const* m_maypts; //initialized by initMayPointToSet()

    //Analysis context. Record MD->MDSet for each BB.
    Vector<MD2MDSet*> m_md2mds_vec;
    xcom::BitSet m_id2heap_md_map;
    MD2MDSet m_unique_md2mds;

    //If the flag is true, flow sensitive analysis is performed.
    //Or perform flow insensitive.
    BYTE m_flow_sensitive:1;

protected:
    MD const* allocHeapobj(IR * ir);
    MD const* assignIdMD(IN IR * ir,
                         IN OUT MDSet * mds,
                         IN OUT AACtx * ic);
    MD const* assignLoadMD(IN IR * ir,
                           IN OUT MDSet * mds,
                           IN OUT AACtx * ic,
                           IN OUT MD2MDSet * mx);
    MD const* assignPRMD(IN IR * ir,
                         IN OUT MDSet * mds,
                         IN OUT AACtx * ic,
                         IN OUT MD2MDSet * mx);

    void convertPT2MD2MDSet(PtPairSet const& pps,
                            IN PtPairMgr & pt_pair_mgr,
                            IN OUT MD2MDSet * ctx);
    bool convertMD2MDSet2PT(OUT PtPairSet & pps,
                            IN PtPairMgr & pt_pair_mgr,
                            IN MD2MDSet * mx);
    void convertExact2Unbound(MDSet const* src, MDSet * tgt);

    bool evaluateFromLda(IR const* ir);

    bool isInLoop(IR const* ir);
    bool isFlowSensitiveProperly();
    void initEntryPtset(PtPairSet ** ptset_arr);
    void initGlobalAndParameterVarPtset(VAR * v,
                                        MD2MDSet * mx,
                                        ConstMDIter & iter);
    void inferPointerArith(IR const* ir,
                           IN OUT MDSet & mds,
                           IN OUT MDSet & opnd0_mds,
                           IN OUT AACtx * opnd0_ic,
                           IN OUT MD2MDSet * mx);
    void inferStoreValue(IR const* ir,
                         IR * rhs,
                         MD const* lhs_md,
                         AACtx const* ic,
                         IN MD2MDSet * mx);
    void inferStoreArrayValue(IR const* ir, AACtx const* ic, IN MD2MDSet * mx);
    void inferIStoreValue(IR const* ir, AACtx const* ic, IN MD2MDSet * mx);
    void inferArrayInfinite(INT ofst,
                            bool is_ofst_pred,
                            UINT md_size,
                            MDSet const& in,
                            OUT MDSet & out);
    MD const* inferArrayLdabase(IR * ir,
                                IR * array_base,
                                bool is_ofst_pred,
                                UINT ofst,
                                IN OUT AACtx * ic);
    void inferExpression(IR * ir,
                         IN OUT MDSet & mds,
                         IN OUT AACtx * ic,
                         IN OUT MD2MDSet * mx);
    void inferArrayExpBase(IR * ir,
                           IR * array_base,
                           bool is_ofst_predicable,
                           UINT ofst,
                           OUT MDSet & mds,
                           OUT bool * mds_is_may_pt,
                           IN OUT AACtx * ic,
                           IN OUT MD2MDSet * mx);

    MD const* processLda(IR * ir, IN OUT AACtx * ic);
    void processCvt(IR const* ir,
                    IN OUT MDSet & mds,
                    IN OUT AACtx * ic,
                    IN OUT MD2MDSet * mx);
    void processGetelem(IR * ir,
                        IN OUT MDSet & mds,
                        IN OUT AACtx * ic,
                        IN OUT MD2MDSet * mx);
    void processGetelem(IR * ir, IN MD2MDSet * mx);
    void processSetelem(IR * ir, IN MD2MDSet * mx);
    void processILoad(IR * ir,
                    IN OUT MDSet & mds,
                    IN OUT AACtx * ic,
                    IN OUT MD2MDSet * mx);
    void processPointerArith(IR * ir,
                             IN OUT MDSet & mds,
                             IN OUT AACtx * ic,
                             IN OUT MD2MDSet * mx);
    void processArray(IR * ir,
                      IN OUT MDSet & mds,
                      IN OUT AACtx * ic,
                      IN OUT MD2MDSet * mx);
    void processConst(IR * ir,
                      IN OUT MDSet & mds,
                      IN OUT AACtx * ic);
    void processStore(IN IR * ir, IN OUT MD2MDSet * mx);
    void processStorePR(IN IR * ir, IN MD2MDSet * mx);
    void processIStore(IN IR * ir, IN OUT MD2MDSet * mx);
    void processStoreArray(IN IR * ir, IN MD2MDSet * mx);
    void processPhi(IN IR * ir, IN MD2MDSet * mx);
    void processCallSideeffect(IN OUT MD2MDSet & mx, MDSet const& by_addr_mds);
    void processCall(IN IR * ir, IN OUT MD2MDSet * mx);
    void processReturn(IN IR * ir, IN MD2MDSet * mx);
    void processRegionSideeffect(IN OUT MD2MDSet & mx);
    void processRegion(IR const* ir, IN MD2MDSet * mx);

    //This function set MustAddr or MayAddr of ir by analyszing given MDSet.
    //mds: mds may be the MayAddr MDSet. Note mds should have been hashed.
    void setMustOrMayAddr(MDSet const* mds, IR * ir);

    //Reshape MD in 'mds' if one of them need to be reshape.
    //Record reshaped MD in 'newmds' and return true.
    //This function will iterate MD in 'mds', new MD will be generated either
    //ir's MD size is different to MD in 'mds' or ir has offset.
    //Return true if new MD generated, and new MD record in 'newmds'.
    bool tryReshapeMDSet(IR const* ir, MDSet const* mds, OUT MDSet * newmds);
    bool tryComputeConstOffset(IR const* ir,
                               IR const* opnd1,
                               IN OUT MDSet & mds,
                               IN OUT MDSet & opnd0_mds);
    void recomputeDataType(AACtx const& ic, IR const* ir, OUT MDSet & pts);
    void reviseMDSize(IN OUT MDSet & mds, UINT size);

    //refmds: ref MDSet of ir.
    //ir: given indirect operation, such as IST, ILD.
    //comp_ir_pt: true if caller require to compute the POINT-TO set of ir.
    //Return POINT-TO set of ir, if comp_ir_pts is true.
    MDSet const* updateIndirectOpAddrAndPointToSet(MDSet const* refmds,
                                                   IR * ir,
                                                   bool comp_ir_pts,
                                                   MD2MDSet * mx);

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
    virtual MD const* computePointToViaType(IR const*) { return NULL; }

    void clean();
    void cleanPointTo(UINT mdid, IN OUT MD2MDSet & ctx)
    { ctx.setAlways(mdid, NULL); }

    //Compute and update point_to_set with TBAA info.
    //pointer: IR expression that pointed to memory.
    //point_to_set: POINT_TO set of pointer, genernate new set if TBAA exist.
    //Return true if pointer pointed to MAY-POINT-TO set.
    MDSet const* computeMayPointToViaTBAA(IR const* pointer,
                                          MDSet const* point_to_set);
    bool computeFlowSensitive(List<IRBB*> const& bbl);
    void computeStmt(IRBB const* bb, IN OUT MD2MDSet * mx);
    void computeFlowInsensitive();
    void computeMayPointTo(IR * pointer, IN MD2MDSet * mx, OUT MDSet & mds);
    void computeMayPointTo(IR * pointer, OUT MDSet & mds);
    //Count memory usage for current object.
    size_t count_mem();
    size_t countMD2MDSetMemory();

    void dumpMD2MDSet(IN MD2MDSet * mx, bool dump_ptg);
    void dumpMD2MDSet(MD const* md, IN MD2MDSet * mx);
    void dumpIRPointTo(IN IR * ir, bool dump_kid, IN MD2MDSet * mx);
    void dumpIRPointToForBB(IRBB * bb, bool dump_kid);
    void dumpIRPointToForRegion(bool dump_kid);
    void dumpPtPairSet(PtPairSet & pps);
    void dumpInOutPointToSetForBB();
    void dumpMD2MDSetForRegion(bool dump_pt_graph);
    void dumpMayPointTo();
    void dump(CHAR const* name);

    void ElemUnionPointTo(MDSet const& mds,
                          MDSet const& in_set,
                          IN MD2MDSet * mx);
    void ElemUnionPointTo(MDSet const& mds,
                          MD const* in_elem,
                          IN MD2MDSet * mx);
    void ElemCopyPointTo(MDSet const& mds, IN MDSet & in_set, IN MD2MDSet * mx);
    void ElemCopyPointToAndMayPointTo(MDSet const& mds, IN MD2MDSet * mx);
    void ElemCopyAndUnionPointTo(MDSet const& mds,
                                 MDSet const& pt_set,
                                 IN MD2MDSet * mx);
    void ElemCleanPointTo(MDSet const& mds, IN MD2MDSet * mx);
    void ElemCleanExactPointTo(MDSet const& mds, IN MD2MDSet * mx);

    virtual CHAR const* getPassName() const { return "Alias Analysis"; }
    virtual PASS_TYPE getPassType() const { return PASS_AA; }
    PtPairSet * getInPtPairSet(IRBB const* bb)
    {
        ASSERTN(m_in_pp_set.get(BB_id(bb)),
                ("IN set is not yet initialized for BB%d", BB_id(bb)));
        return m_in_pp_set.get(BB_id(bb));
    }
    PtPairSet * getOutPtPairSet(IRBB const* bb)
    {
        ASSERTN(m_out_pp_set.get(BB_id(bb)),
                ("OUT set is not yet initialized for BB%d", BB_id(bb)));
        return m_out_pp_set.get(BB_id(bb));
    }

    //For given MD2MDSet, return the MDSet that 'md' pointed to.
    //ctx: context of point-to analysis.
    MDSet const* getPointTo(UINT mdid, MD2MDSet & ctx) const
    { return ctx.get(mdid); }

    //Return the may-point-to Memory Descriptor Set.
    MDSet const* getMayPointToMDSet() const { return m_maypts; }
    MD2MDSet * getUniqueMD2MDSet() { return &m_unique_md2mds; }

    void initAliasAnalysis();
    //Return true if Alias Analysis has initialized.
    bool is_init() const { return m_maypts != NULL; }
    bool isFlowSensitive() const { return m_flow_sensitive; }
    bool isValidStmtToAA(IR * ir);
    bool isHeapMem(UINT mdid) const
    { //return mdid == MD_HEAP_MEM ? true : false;
      DUMMYUSE(mdid);
      return false;
    }
    //Return true if the MD of each PR corresponded is unique.
    void initMayPointToSet();

    void cleanContext(OptCtx & oc);
    void destroyContext(OptCtx & oc);

    //For given MD2MDSet, set the point-to set to 'md'.
    //ctx: context of point-to analysis.
    void setPointTo(UINT mdid, MD2MDSet & ctx, MDSet const* ptset)
    {
        ASSERT0(ptset);
        ASSERTN(m_mds_hash->find(*ptset), ("ptset should be in hash"));
        ctx.setAlways(mdid, ptset);
    }

    //Set pointer points to 'target'.
    inline void setPointToUniqueMD(UINT pointer_mdid,
                                   MD2MDSet & ctx,
                                   MD const* target)
    {
        ASSERT0(target);
        MDSet tmp;
        tmp.bunion(target, *m_misc_bs_mgr);
        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer_mdid, ctx, hashed);
        tmp.clean(*m_misc_bs_mgr);
    }

    //Set pointer points to 'target_set' in the context.
    void setPointToMDSet(UINT pointer_mdid,
                         MD2MDSet & ctx,
                         MDSet const& target_set)
    {
        ASSERTN(!m_mds_hash->find(target_set),
                ("already hashed, there might be redundant function call"));
        MDSet const* hashed = m_mds_hash->append(target_set);
        setPointTo(pointer_mdid, ctx, hashed);
    }

    //Set pointer points to new MDSet by appending a new element 'newmd'
    //in the context.
    inline void setPointToMDSetByAddMD(UINT pointer_mdid,
                                       MD2MDSet & ctx,
                                       MD const* newmd)
    {
        MDSet tmp;
        MDSet const* pts = getPointTo(pointer_mdid, ctx);
        if (pts != NULL) {
            if (pts->is_contain(newmd)) {
                ASSERT0(m_mds_hash->find(*pts));
                //setPointTo(pointer_mdid, ctx, pts);
                return;
            }
            tmp.bunion(*pts, *m_misc_bs_mgr);
        }

        tmp.bunion(newmd, *m_misc_bs_mgr);
        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer_mdid, ctx, hashed);
        tmp.clean(*m_misc_bs_mgr);
    }

    //Set pointer points to MD set by appending a MDSet.
    //pt_set: POINT-TO set that has been hashed.
    inline void setPointToMDSetByAddMDSet(UINT pointer_mdid,
                                          MD2MDSet & ctx,
                                          MDSet const& pt_set)
    {
        ASSERT0(m_mds_hash->find(pt_set));
        MDSet const* pts = getPointTo(pointer_mdid, ctx);
        if (pts == NULL) {
            setPointTo(pointer_mdid, ctx, &pt_set);
            return;
        }
        if (&pt_set == m_maypts) {
            setPointTo(pointer_mdid, ctx, m_maypts);
            return;
        }
        MDSet tmp;
        tmp.copy(*pts, *m_misc_bs_mgr);
        tmp.bunion(pt_set, *m_misc_bs_mgr);

        MDSet const* hashed = m_mds_hash->append(tmp);
        setPointTo(pointer_mdid, ctx, hashed);
        tmp.clean(*m_misc_bs_mgr);
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
    void setLHSPointToSet(MD const* lhs_mustaddr,
                          MDSet const* lhs_mayaddr,
                          MDSet const* pts,
                          IN MD2MDSet * mx);

    //Function return the POINT-TO pair for each BB.
    //Only used in flow-sensitive analysis.
    MD2MDSet * mapBBtoMD2MDSet(UINT bbid) const
    { return m_md2mds_vec.get(bbid); }

    //Function return the POINT-TO pair for each BB.
    //Only used in flow-sensitive analysis.
    inline MD2MDSet * allocMD2MDSetForBB(UINT bbid)
    {
        MD2MDSet * mx = m_md2mds_vec.get(bbid);
        if (mx == NULL) {
            mx = (MD2MDSet*)xmalloc(sizeof(MD2MDSet));
            mx->init();
            m_md2mds_vec.set(bbid, mx);
        }
        return mx;
    }

    //This function update LHS's POINT-TO set accroding to RHS.
    //is_lhs_pointer: true if transit rhs's POINT-TO set to lhs.
    //rhs: RHS expression of stmt, which are IR_ST, IR_IST, IR_STARRAY.
    //rhsrefmds: record memory descriptor of 'rhs' if AC_comp_pt() is false, or
    //           record the POINT-TO set of 'rhs' if AC_comp_pt() is true.
    //           Note if AC_comp_pt() is true, the returned POINT-TO set may be
    //           recorded in 'rhsrefmds' or AC_returned_pts.
    //returned_pts: record the POINT-TO set of 'rhs' if AC_comp_pt() is true.
    //              Note if AC_comp_pt() is true, the returned POINT-TO set may
    //              be recorded in 'rhsrefmds' or AC_returned_pts.
    void updateLHSPointToSet(bool is_lhs_pointer,
                             bool rhs_taken_address,
                             MD const* lhs_mustaddr,
                             MDSet const* lhs_mayaddr,
                             IR const* rhs,
                             MDSet & rhsrefmds,
                             MDSet const* returned_pts,
                             MD2MDSet * mx);
    //Union POINT-TO set for each element in 'mds', and hash the unified result
    //return it.
    //mds: represents address of current ILD.
    MDSet const* unifyPointToSet(MDSet const& mds, MD2MDSet const* mx);

    bool verifyIR(IR * ir);
    bool verify();
    bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
