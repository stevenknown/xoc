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
#ifndef _IR_SSA_H_
#define _IR_SSA_H_

namespace xoc {

class PRSSAMgr;

//Dominace Frontier manager
class DfMgr {
protected:
    //PRSSAMgr * m_ssa_mgr;
    xcom::BitSetMgr m_bs_mgr;
    Vector<xcom::BitSet*> m_df_vec;
    UINT m_thres;

    void buildRecur(xcom::Vertex const* v,
        xcom::DGraph const& g,
        DomTree const& domtree);

    //Generate the DF control set
    xcom::BitSet * genDFControlSet(UINT vid);

public:
    explicit DfMgr(UINT thres = THRESHOLD_HIGH_DOMINATOR_FRONTIER_DENSITY) :
        m_thres(thres) {}
    COPY_CONSTRUCTOR(DfMgr);

    void clean();
    void build(xcom::DGraph const& g);
    void build(xcom::DGraph const& g, DomTree const& domtree);
    void dump(xcom::DGraph & g);

    //Count Dominator Frontier Density for each xcom::Vertex.
    //Return true if there exist vertex that might inserting
    //ton of phis which will blow up memory.
    bool hasHighDFDensityVertex(xcom::DGraph const& g);

    //Return the BB set controlled by vid.
    xcom::BitSet const* getDFControlSet(UINT vid) const
    { return m_df_vec.get(vid); }

    void rebuild(xcom::DGraph & g) { clean(); build(g); }
};


//SSAGraph
class SSAGraph : xcom::Graph {
    Region * m_ru;
    PRSSAMgr * m_ssa_mgr;
    TMap<UINT, VP*> m_vdefs;
public:
    SSAGraph(Region * rg, PRSSAMgr * ssamgr);
    COPY_CONSTRUCTOR(SSAGraph);
    void dump(CHAR const* name = NULL, bool detail = true);
};


typedef Vector<TMap<UINT, VP*>*> BB2VPMap;


//Perform SSA based optimizations.
class PRSSAMgr : public Pass {
protected:
    Region * m_ru;
    SMemPool * m_vp_pool;
    TypeMgr * m_tm;
    IR_CFG * m_cfg;
    DefSegMgr * m_seg_mgr;
    bool m_is_ssa_constructed;
    UINT m_vp_count;
    IRIter m_iter; //for tmp use.

    //Record versions for each PRs.
    UINT2VPVec m_map_prno2vp_vec;

    //Record version stack during renaming.
    UINT2VPStack m_map_prno2stack;
    Vector<VP*> m_vp_vec;
    Vector<UINT> m_max_version; //record version number counter for pr.

    //Record the duplicated IR* to each prno.
    //Be used to generate phi for each prno.
    Vector<IR*> m_prno2ir;

protected:
    inline void init()
    {
        if (m_vp_pool != NULL) { return; }
        m_vp_count = 1;
        m_is_ssa_constructed = false;
        m_vp_pool = smpoolCreate(sizeof(VP)*2, MEM_CONST_SIZE);
    }

    void clean()
    {
        m_ru = NULL;
        m_tm = NULL;
        m_seg_mgr = NULL;
        m_cfg = NULL;
        m_vp_count = 1;
        m_is_ssa_constructed = false;
        m_vp_pool = NULL;
    }
    void cleanPRSSAInfo();
    void constructMDDUChainForPR();
    void cleanPRNO2Stack();
    void collectDefinedPR(IN IRBB * bb, OUT DefSBitSet & mustdef_pr);

    void destructBBSSAInfo(IRBB * bb);
    void destructionInDomTreeOrder(IRBB * root, xcom::Graph & domtree);

    void handleBBRename(IRBB * bb,
                        IN DefSBitSet const& defined_prs,
                        IN OUT BB2VPMap & bb2vp);

    Stack<VP*> * mapPRNO2VPStack(UINT prno);
    IR * mapPRNO2IR(UINT prno) { return m_prno2ir.get(prno); }

    VP * allocVP()
    {
        ASSERTN(m_vp_pool != NULL, ("not init"));
        VP * p = (VP*)smpoolMallocConstSize(sizeof(VP), m_vp_pool);
        ASSERT0(p);
        ::memset(p, 0, sizeof(VP));
        return p;
    }

    void refinePhi(List<IRBB*> & wl);
    void rename(DefSBitSet const& effect_prs,
                Vector<DefSBitSet*> const& defined_prs_vec,
                xcom::Graph const& domtree);
    void renameBB(IRBB * bb);
    void renameInDomTreeOrder(
                IRBB * root,
                xcom::Graph const& dtree,
                Vector<DefSBitSet*> const& defined_prs_vec);

    void stripVersionForBBList();
    void stripVersionForAllVP();
    void stripPhi(IR * phi, xcom::C<IR*> * phict);
    void stripSpecifiedVP(VP * vp);
    void stripStmtVersion(IR * stmt, xcom::BitSet & visited);

    void placePhiForPR(UINT prno,
                       IN List<IRBB*> * defbbs,
                       DfMgr const& dfm,
                       xcom::BitSet & visited,
                       List<IRBB*> & wl,
                       Vector<DefSBitSet*> & defined_prs_vec);
    void placePhi(DfMgr const& dfm,
                  IN OUT DefSBitSet & effect_prs,
                  DefMiscBitSetMgr & bs_mgr,
                  Vector<DefSBitSet*> & defined_prs_vec,
                  List<IRBB*> & wl);

public:
    explicit PRSSAMgr(Region * rg)
    {
        clean();
        ASSERT0(rg);
        m_ru = rg;

        m_tm = rg->getTypeMgr();
        ASSERT0(m_tm);

        ASSERT0(rg->getMiscBitSetMgr());
        m_seg_mgr = rg->getMiscBitSetMgr()->getSegMgr();
        ASSERT0(m_seg_mgr);

        m_cfg = rg->getCFG();
        ASSERTN(m_cfg, ("cfg is not available."));
    }
    COPY_CONSTRUCTOR(PRSSAMgr);
    ~PRSSAMgr()
    {
        ASSERTN(!isSSAConstructed(), ("should be destructed"));
        destroy(false);
    }

    void buildDUChain(IR * def, IR * use)
    {
        ASSERT0(def->isWritePR() || def->isCallHasRetVal());
        ASSERT0(use->isReadPR());
        SSAInfo * ssainfo = def->getSSAInfo();
        if (ssainfo == NULL) {
            ssainfo = allocSSAInfo(def->getPrno());
            def->setSSAInfo(ssainfo);
            SSA_def(ssainfo) = def;
        }

        //You may be set multiple defs for use.
        ASSERTN(use->getSSAInfo() == NULL, ("use already has SSA info."));
        use->setSSAInfo(ssainfo);
        SSA_uses(ssainfo).append(use);
    }

    //is_reinit: this function is invoked in reinit().
    void destroy(bool is_reinit)
    {
        if (m_vp_pool == NULL) { return; }

        //Caution: if you do not destruct SSA prior to destory().
        //The reference to IR's SSA info will lead to undefined behaviors.
        //ASSERTN(!m_is_ssa_constructed,
        //   ("Still in ssa mode, you should out of "
        //    "SSA before the destruction."));

        for (INT i = 0; i <= m_map_prno2vp_vec.get_last_idx(); i++) {
            Vector<VP*> * vpv = m_map_prno2vp_vec.get((UINT)i);
            if (vpv != NULL) { delete vpv; }
        }

        cleanPRNO2Stack();

        for (INT i = 0; i <= m_vp_vec.get_last_idx(); i++) {
            VP * v = m_vp_vec.get((UINT)i);
            if (v != NULL) {
                v->destroy();
            }
        }

        if (is_reinit) {
            m_map_prno2vp_vec.clean();
            m_vp_vec.clean();
            m_max_version.clean();
            m_prno2ir.clean();
        }

        //Do not free irs in m_prno2ir.
        smpoolDelete(m_vp_pool);
        m_vp_pool = NULL;
    }

    void destruction(DomTree & domtree);
    void destruction(OptCtx * oc);
    void dump();
    void dumpAllVP(bool have_renamed);
    CHAR * dumpVP(IN VP * v, OUT CHAR * buf);
    void dumpSSAGraph(CHAR * name = NULL);

    //Note: Non-SSA DU Chains of read/write PR will be clean and
    //unusable after SSA construction.
    void construction(OptCtx & oc);
    bool construction(DomTree & domtree);

    //Compute SSAInfo for IRs in region that are in SSA mode.
    void computeSSAInfo();
    size_t count_mem();

    Vector<VP*> const* getVPVec() const { return &m_vp_vec; }
    VP * getVP(UINT id) const { return m_vp_vec.get(id); }

    IR * initVP(IN IR * ir);
    void insertPhi(UINT prno, IN IRBB * bb);

    //Return true if PR ssa is constructed.
    //This flag will direct the behavior of optimizations.
    //If SSA constructed, DU mananger will not compute any information for PR.
    bool isSSAConstructed() const { return m_is_ssa_constructed; }

    //Return true if phi is redundant, otherwise return false.
    //If all opnds have same defintion or defined by current phi,
    //the phi is redundant.
    //common_def: record the common_def if the definition
    //  of all opnd is the same.
    bool isRedundantPHI(IR const* phi, OUT IR ** common_def) const;

    //Allocate VP and ensure it is unique according to 'version' and 'prno'.
    VP * allocVP(UINT prno, UINT version)
    {
        ASSERT0(prno != PRNO_UNDEF);
        Vector<VP*> * vec = m_map_prno2vp_vec.get(prno);
        if (vec == NULL) {
            vec = new Vector<VP*>();
            m_map_prno2vp_vec.set(prno, vec);
        }

        VP * v = vec->get(version);
        if (v != NULL) {
            return v;
        }

        ASSERTN(m_seg_mgr, ("SSA manager is not initialized"));
        v = allocVP();
        v->initNoClean(m_seg_mgr);
        VP_prno(v) = prno;
        VP_ver(v) = version;
        SSA_id(v) = m_vp_count++;
        SSA_def(v) = NULL;
        vec->set(version, v);
        m_vp_vec.set(SSA_id(v), v);
        return v;
    }

    //Allocate SSAInfo for specified PR indicated by 'prno'.
    SSAInfo * allocSSAInfo(UINT prno)
    {
        ASSERT0(prno != PRNO_UNDEF);
        return (SSAInfo*)allocVP(prno, 0);
    }

    //Reinitialize SSA manager.
    //This function will clean all informations and recreate them.
    inline void reinit()
    {
        if (isSSAConstructed()) {
            destroy(true);
        }
        init();
    }

    bool verifyPhi(bool is_vpinfo_avail);
    bool verifyPRNOofVP(); //Only used in PRSSAMgr.
    bool verifyVP(); //Only used in PRSSAMgr.
    bool verifySSAInfo(); //Can be used in any module.

    virtual CHAR const* getPassName() const
    { return "PR SSA Manager"; }

    PASS_TYPE getPassType() const { return PASS_PR_SSA_MGR; }

    virtual bool perform(OptCtx & oc)
    { construction(oc); return true; }
};


bool verifySSAInfo(Region * rg);

} //namespace xoc
#endif
