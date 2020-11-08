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
typedef TMap<UINT, VPR*> PRNO2VP;
typedef TMap<UINT, VPR*> VertexID2VP;
typedef Vector<PRNO2VP*> BB2VPMap;

//Dominace Frontier manager
class DfMgr {
    COPY_CONSTRUCTOR(DfMgr);
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

    void build(xcom::DGraph const& g);
    void build(xcom::DGraph const& g, DomTree const& domtree);
    void clean();
    void dump(xcom::DGraph const& g, Region * rg) const;

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
    COPY_CONSTRUCTOR(SSAGraph);
    Region * m_rg;
    PRSSAMgr * m_ssa_mgr;
    VertexID2VP m_vdefs;
public:
    SSAGraph(Region * rg, PRSSAMgr * ssamgr);
    void dump(CHAR const* name = nullptr, bool detail = true) const;
};


//Perform SSA based optimizations.
class PRSSAMgr : public Pass {
    COPY_CONSTRUCTOR(PRSSAMgr);
    Region * m_rg;
    SMemPool * m_vp_pool;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    DefSegMgr * m_seg_mgr;
    bool m_is_valid;
    UINT m_vp_count;
    IRIter m_iter; //for tmp use.
    //Record virtual PR for each PR.
    UINT2VPVec m_map_prno2vpr_vec;
    //Record version stack during renaming.
    UINT2VPRStack m_map_prno2stack;
    //Record virutal PR that indexed by VPR id.
    VPRVec m_vpr_vec;
    //Record version number counter for PR that indexed by PRNO.
    Vector<UINT> m_max_version;
    //Record the duplicated IR* to each prno.
    //Be used to generate phi for each prno.
    Vector<IR*> m_prno2ir;

    inline void init()
    {
        if (m_vp_pool != nullptr) { return; }
        m_vp_count = 1;
        m_is_valid = false;
        m_map_prno2vpr_vec.set(m_rg->getPRCount(), nullptr);
        m_max_version.set(m_rg->getPRCount(), 0);
        m_vp_pool = smpoolCreate(sizeof(VPR)*2, MEM_CONST_SIZE);
    }

    void clean()
    {
        m_rg = nullptr;
        m_tm = nullptr;
        m_seg_mgr = nullptr;
        m_cfg = nullptr;
        m_vp_count = 1;
        m_is_valid = false;
        m_vp_pool = nullptr;
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

    xcom::Stack<VPR*> * mapPRNO2VPStack(UINT prno);
    IR * mapPRNO2IR(UINT prno) { return m_prno2ir.get(prno); }

    VPR * allocVPR()
    {
        ASSERTN(m_vp_pool != nullptr, ("not init"));
        VPR * p = (VPR*)smpoolMallocConstSize(sizeof(VPR), m_vp_pool);
        ASSERT0(p);
        ::memset(p, 0, sizeof(VPR));
        return p;
    }

    //Rename opnd, not include phi.
    //Walk through rhs expression of 'ir' to rename PR's VPR.
    //bb: the BB that ir belongs to.
    void renameRHS(IR * ir, IRBB * bb);
    bool refinePhi(List<IRBB*> & wl);
    void rename(DefSBitSet const& effect_prs,
                Vector<DefSBitSet*> const& defined_prs_vec,
                xcom::Graph const& domtree);
    void renameBB(IRBB * bb);
    void renameInDomTreeOrder(
        IRBB * root,
        xcom::Graph const& dtree,
        Vector<DefSBitSet*> const& defined_prs_vec);
    void removePhiFromBB();

    void stripVersionForBBList();
    void stripPhi(IR * phi, IRListIter phict);
    void stripSpecifiedVP(VPR * vp);
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

    bool verifyPRNOofVP(); //Only used in PRSSAMgr.
    bool verifyVPR(); //Only used in PRSSAMgr.
public:
    explicit PRSSAMgr(Region * rg)
    {
        clean();
        ASSERT0(rg);
        m_rg = rg;

        m_tm = rg->getTypeMgr();
        ASSERT0(m_tm);

        ASSERT0(rg->getMiscBitSetMgr());
        m_seg_mgr = rg->getMiscBitSetMgr()->getSegMgr();
        ASSERT0(m_seg_mgr);

        m_cfg = rg->getCFG();
        ASSERTN(m_cfg, ("cfg is not available."));
    }
    ~PRSSAMgr()
    {
        ASSERTN(!is_valid(), ("should be destructed"));
        destroy(false);
    }

    //Allocate VPR and ensure it is unique according to 'version' and 'prno'.
    //prno: describ the Versioned PR
    //version: current version of Versioned PR
    //orgtype: data type of orginal prno
    VPR * allocVPR(UINT prno, UINT version, Type const* orgtype);

    //After adding BB or change BB successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    void addSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ);

    //Allocate SSAInfo for specified PR indicated by 'prno'.
    SSAInfo * allocSSAInfo(UINT prno)
    {
        ASSERT0(prno != PRNO_UNDEF);
        return (SSAInfo*)allocVPR(prno, 0, m_tm->getAny());
    }

    void buildDUChain(IR * def, IR * use);

    //Note: Non-SSA DU Chains of read/write PR will be clean and
    //unusable after SSA construction.
    void construction(OptCtx & oc);
    bool construction(DomTree & domtree);

    //Compute SSAInfo for IRs in region that are in SSA mode.
    void computeSSAInfo();
    size_t count_mem() const;

    //is_reinit: this function is invoked in reinit().
    void destroy(bool is_reinit);
    void destruction(DomTree & domtree);
    void destruction(OptCtx * oc);
    virtual bool dump() const;
    void dumpAllVPR() const;
    CHAR * dumpVP(IN VPR * v, OUT CHAR * buf) const;
    void dumpSSAGraph(CHAR * name = nullptr) const;

    Region * getRegion() const { return m_rg; }
    VPRVec const* getVPRVec() const { return &m_vpr_vec; }
    VPR * getVPR(UINT id) const { return m_vpr_vec.get(id); }
    //Map PRNO to VPRVec that recorded all VPR during SSA.
    VPRVec const* getVPRVecByPRNO(UINT prno) const
    { return m_map_prno2vpr_vec.get(prno); }
    virtual CHAR const* getPassName() const { return "PRSSA Manager"; }
    PASS_TYPE getPassType() const { return PASS_PR_SSA_MGR; }

    IR * initVP(IN IR * ir);
    void insertPhi(UINT prno, IN IRBB * bb);

    //Insert operand at given position.
    //pos: position of operand, start at 0.
    //     Each operand correspond to in-edge on CFG.
    IR * insertOpndAt(IR * phi, UINT pos, IRBB const* pred);

    //Return true if PR ssa is constructed.
    //This flag will direct the behavior of optimizations.
    //If SSA constructed, DU mananger will not compute any information for PR.
    bool is_valid() const { return m_is_valid; }

    //Return true if phi is redundant, otherwise return false.
    //If all opnds have same defintion or defined by current phi,
    //the phi is redundant.
    //common_def: record the common_def if the definition
    //  of all opnd is the same.
    bool isRedundantPHI(IR const* phi, OUT IR ** common_def) const;

    //Return true if stmt dominate use's stmt, otherwise return false.
    bool isStmtDomUseInsideLoop(IR const* stmt,
                                IR const* use,
                                LI<IRBB> const* li) const;

    //Return true if ir dominates all its USE expressions which inside loop.
    //In ssa mode, stmt's USE may be placed in operand list of PHI.
    bool isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const;

    //Move IR_PHI from 'from' to 'to'.
    //This function often used in updating PHI when adding new dominater
    //BB to 'to'.
    static void movePhi(IRBB * from, IRBB * to);

    //This function revise phi data type, and remove redundant phi.
    //Return true if there is phi removed.
    bool refinePhi();
    //Reinitialize SSA manager.
    //This function will clean all informations and recreate them.
    inline void reinit()
    {
        destroy(true);        
        init();
    }
    //Before removing BB or change BB successor,
    //you need remove the related PHI operand if BB successor has PHI.
    void removeSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ);

    //Remove PR-SSA Use-Def chain.
    //e.g:ir=...
    //    ...=ir //S1
    //If S1 deleted, ir should be removed from its UseSet in SSAInfo.
    //NOTE: If ir is an IR tree, e.g: add(pr1, pr2), removing 'add' means
    //pr1 and pr2 will be removed as well. Therefore pr1 pr2's SSAInfo will be
    //updated as well.
    static void removePRSSAUse(IR * ir);

    //Check each USE of stmt, remove the expired one which is not reference
    //the memory any more that stmt defined.
    //Return true if DU changed.
    static bool removeExpiredDUForStmt(IR * stmt, Region * rg);

    bool verifyPhi(bool is_vpinfo_avail, bool before_strip_version);
    bool verifySSAInfo(); //Can be used in any module.
    static bool verifyPRSSAInfo(Region * rg);

    virtual bool perform(OptCtx & oc) { construction(oc); return true; }
};

} //namespace xoc
#endif
