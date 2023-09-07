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
class LivenessMgr;
typedef TMap<PRNO, VPR*> PRNO2VPR;
typedef TMap<UINT, VPR*> VertexID2VP;
typedef DefSBitSet PRSet;
typedef DefSBitSetIter PRSetIter;
typedef DefSBitSet BBSet;
typedef DefSBitSetIter BBSetIter;
typedef Vector<Type const*> PRNO2Type;

class BB2PRSet : public Vector<PRSet*> {
    COPY_CONSTRUCTOR(BB2PRSet);
    DefMiscBitSetMgr * m_sbsmgr;
public:
    BB2PRSet(DefMiscBitSetMgr * sm) : m_sbsmgr(sm) {}

    void addDefBB(PRNO prno, UINT bbid);

    PRSet * genPRSet(UINT bbid);
    void genPRSet(BBSet const& bbset);
};


//
//START PRSSARegion
//
//The class represents a region that is consist of BB in bbset, which
//is expected to perform SSA construction for the region.
class PRSSARegion {
    COPY_CONSTRUCTOR(PRSSARegion);
public:
    TTab<UINT> m_idtab;
    IRList m_irlist;
    BBSet m_bbset;
    IRBB * m_root;
    OptCtx * m_oc;
public:
    PRSSARegion(xcom::DefMiscBitSetMgr * sbs, OptCtx * oc) :
        m_bbset(sbs->getSegMgr()), m_root(nullptr), m_oc(oc) {}

    //The function will find PR that assigned 'prno' into current
    //SSA construction region.
    //ir: stmt or expression start to find.
    void add(PRNO prno, IR * start);

    //Add ir to current SSA construction region that expected to transform
    //to SSA mode.
    //Note ir must be PR op.
    void add(IR * ir);

    //Add BB into current SSA construction region.
    //Note even if there is no occurrence of PR operation in 'bb', the BB that
    //belong to the SSARegion also should be add into bbset.
    void add(IRBB const* bb) { ASSERT0(bb); getBBSet().bunion(bb->id()); }

    //Add a set of BB into current SSA construction region.
    //The construction will not exceed these BBs.
    void add(BitSet const& bbset) { getBBSet().bunion(bbset); }

    //Walk through each predecessors from 'start' to guarrantee all vertexs in
    //path from root to start have been added.
    void addPredBBUntillRoot(IRBB const* start, IRCFG const* cfg);

    //Return true if bb can be regarded as the root of SSA region.
    bool canBeRoot(IRBB const* bb) const;

    void dump(Region const* rg) const;

    //The function attempt to find properly root BB of SSA region.
    //A properly root BB either does not have any PHI operation, or all
    //predecessors of root BBs are located within current SSA region.
    IRBB * findRootBB(xcom::DomTree const& domtree, Region const* rg,
                      IRBB * start);

    //Get the bbset of region.
    BBSet & getBBSet() { return m_bbset; }

    //Get the irlist of current SSA construction region.
    IRList & getIRList() { return m_irlist; }

    //Get the root BB of current SSA construction region.
    IRBB * getRootBB() const { return m_root; }
    OptCtx * getOptCtx() const { return m_oc; }

    //Return true if BB id is in the SSA region.
    //id: the BB id.
    bool isInRegion(UINT id) const { return m_bbset.is_contain(id); }

    //Return true if all predecessors of 'bb' are located in SSA region.
    bool isAllPredInRegion(IRBB const* bb) const;

    //Infer and add those BBs that should be also handled in PRSSA construction.
    void inferAndAddRelatedBB(Region const* rg, IRCFG const* cfg);

    //Set the root BB of current SSA construction region.
    //root: root BB for CFG region that is consisted of BB which is
    //in SSA construction region.
    void setRootBB(IRBB * root) { add(root); m_root = root; }

    //Verify whether the SSA region is legal enough to construct.
    bool verify() const;
};
//END PRSSARegion


class BB2VPMap : public Vector<PRNO2VPR*> {
    COPY_CONSTRUCTOR(BB2VPMap);
public:
    BB2VPMap(INT size) : Vector<PRNO2VPR*>(size) {}

    //The allocated object will be destroied at destoryPRNO2VPR().
    PRNO2VPR * allocPRNO2VPR(UINT bbid);
    void destoryPRNO2VPR(UINT bbid);
    //Verify if vpmap of each BB has been deleted.
    bool verify();
};

//Dominace Frontier manager
class DfMgr {
    COPY_CONSTRUCTOR(DfMgr);
protected:
    //PRSSAMgr * m_ssa_mgr;
    xcom::BitSetMgr m_bs_mgr;
    Vector<xcom::BitSet*> m_df_vec;
    UINT m_thres;

    void buildRecur(xcom::Vertex const* v, xcom::DGraph const& g,
                    xcom::DomTree const& domtree);

    //Generate the DF control set
    xcom::BitSet * genDFControlSet(UINT vid);

public:
    explicit DfMgr(UINT thres = THRESHOLD_HIGH_DOMINATOR_FRONTIER_DENSITY) :
        m_thres(thres) {}

    //This function compute dominance frontier to graph g.
    void build(xcom::DGraph const& g);

    //This function compute dominance frontier to graph g recursively.
    void build(xcom::DGraph const& g, xcom::DomTree const& domtree);
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


//The class collects PRSSA information before CFG changed.
//The informatino will be used to update PRSSA incrmentally.
class PRSSAInfoCollect {
    COPY_CONSTRUCTOR(PRSSAInfoCollect);
    PRSSAMgr * m_ssamgr;
    Region * m_rg;
    IRCFG * m_cfg;
    OptCtx const* m_oc;
    TMap<IR const*, IR const*> m_phi2livein;
private:
    //Find livein BB through given 'pred'.
    //The function will iterate dom tree bottom up from 'pred' until the
    //anticipated BB found.
    //pred: the predecessor of bb.
    //Note dom info must be avaiable.
    bool isDomLiveinBBFromPred(UINT bb, UINT pred, UINT meetup);
public:
    PRSSAInfoCollect() {}

    //The function collect livein PR for PHI before adding an edge from 'from'
    //to 'to'. If no livein existing is found, the new operand will be new
    //livein-PR.
    void collect(IRBB const* from, IRBB const* to);

    IR const* getLiveinRef(IR const* phi) const;
    OptCtx const* getOptCtx() const { return m_oc; }

    void init(PRSSAMgr * mgr, OptCtx const& oc);
};


//Perform SSA based optimizations.
class PRSSAMgr : public Pass {
    friend class SSAGraph;
    class PR2DefBBSet {
        COPY_CONSTRUCTOR(PR2DefBBSet);
        //All objects allocated and recorded in pr2defbb are used
        //for local purpose.
        Vector<List<IRBB*>*> m_pr2defbblst;
        void init(IRBB * bb, BB2PRSet const& bb2definedprs)
        {
            PRSet * prs = bb2definedprs.get(bb->id());
            if (prs == nullptr) { return; }
            //Record which BB defined these effect prs.
            DefSBitSetIter cur = nullptr;
            for (BSIdx j = prs->get_first(&cur);
                 j != BS_UNDEF; j = prs->get_next(j, &cur)) {
                List<IRBB*> * bbs = m_pr2defbblst.get((VecIdx)j);
                if (bbs == nullptr) {
                    bbs = new List<IRBB*>();
                    m_pr2defbblst.set((VecIdx)j, bbs);
                }
                bbs->append_tail(bb);
            }
        }
    public:
        PR2DefBBSet(BBSet const& bbset, BB2PRSet const& bb2definedprs,
                    Region * rg)
        {
            BBSetIter it;
            for (BSIdx i = bbset.get_first(&it); i != BS_UNDEF;
                 i = bbset.get_next(i, &it)) {
                IRBB * bb = rg->getCFG()->getBB((UINT)i);
                ASSERT0(bb);
                init(bb, bb2definedprs);
            }
        }
        PR2DefBBSet(BBList const* bblst, BB2PRSet const& bb2definedprs)
        {
            BBListIter it;
            for (IRBB * bb = bblst->get_head(&it);
                 bb != nullptr; bb = bblst->get_next(&it)) {
                init(bb, bb2definedprs);
            }
        }
        ~PR2DefBBSet()
        {
            for (VecIdx i = 0; i <= m_pr2defbblst.get_last_idx(); i++) {
                List<IRBB*> * bbs = m_pr2defbblst.get(i);
                if (bbs != nullptr) {
                    delete bbs;
                }
            }
        }
        List<IRBB*> * get(PRNO prno) const
        { return m_pr2defbblst.get((VecIdx)prno); }
    };
    COPY_CONSTRUCTOR(PRSSAMgr);
    BYTE m_is_semi_pruned:1;
    BYTE m_is_pruned:1;
    UINT m_vp_count;
    SMemPool * m_vp_pool;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    DefSegMgr * m_seg_mgr;
    LivenessMgr * m_livemgr;
    //Record OptCtx in used. It is always updated in perform().
    IRIter m_iter; //for tmp use.

    //Record virtual PR for each PR.
    PRNO2VPRVec m_prno2vprvec;

    //Record version stack during renaming.
    PRNO2VPRStack m_prno2stack;

    //Record virutal PR that indexed by VPR id.
    VPRVec m_vpr_vec;

    //Record virutal PR that indexed by PRNO.
    VPRVec m_prno2vpr;

    //Record version number counter for PR that indexed by PRNO.
    Vector<UINT> m_prno2maxversion;

    //Record data type for each prno, which is used to generate Phi
    PRNO2Type m_prno2type;
private:
    VPR * allocVPR()
    {
        ASSERTN(m_vp_pool != nullptr, ("not init"));
        VPR * p = (VPR*)smpoolMallocConstSize(sizeof(VPR), m_vp_pool);
        ASSERT0(p);
        ::memset(p, 0, sizeof(VPR));
        return p;
    }

    //Allocate VPR and ensure it is unique according to 'version' and 'prno'.
    //orgprno: describ the PRNO that expect to be versioned.
    //version: current version of Versioned PR
    //orgtype: data type of orginal prno
    VPR * allocVPR(PRNO orgprno, UINT version, Type const* orgtype);

    //Allocate SSAInfo and ensure it is unique according to prno.
    //prno: the function will generate SSAInfo for the prno.
    //version: expect version of given prno.
    //type: data type of prno.
    SSAInfo * allocSSAInfo(PRNO prno, Type const* type);
    VPR * allocVPRImpl(PRNO orgprno, PRNO newprno, UINT version,
                       Type const* orgtype, VPRVec * vprvec);

    void clean()
    {
        m_tm = nullptr;
        m_seg_mgr = nullptr;
        m_cfg = nullptr;
        m_vp_count = 1;

        //Set to true if PR ssa is constructed.
        //This flag will direct the behavior of optimizations.
        //If SSA constructed, DU mananger should not compute information
        //for PR any more.
        m_is_valid = false;
        m_is_semi_pruned = true;
        m_is_pruned = true;
        m_vp_pool = nullptr;
        m_livemgr = nullptr;
    }
    //Clean VPR info for IR in 'lst'.
    //The function always used in initialization in partial computation to
    //SSAInfo of given IR.
    void cleanPRNO2VPRForIRList(IRList const& lst);
    void cleanPRNO2MaxVersion();

    //Set SSAInfo of IR to be nullptr to inform optimizer that IR is
    //not in SSA form.
    void cleanPRSSAInfo();
    void cleanPRNO2VPRStack();
    void cleanPRNO2Type();
    //definedprs: record PRs which defined in 'bb'.
    void collectPRAndInitVPRForBB(IRBB const* bb, OUT PRSet & mustdef_pr);
    void collectPRAndInitVPRForIRList(IRList const& irlist, BBSet const& bbset,
                                      MOD DefMiscBitSetMgr & sm,
                                      OUT BB2PRSet & bb2definedprs,
                                      OUT PRSet & prset);
    void constructMDDUChainForPR();
    void computeDefinedPR(IRList const& irlist, MOD DefMiscBitSetMgr & sm,
                          OUT BB2PRSet & bb2definedprs,
                          OUT PRSet & prset, OUT BBSet & bbset);

    void destructBBSSAInfo(IRBB * bb, OptCtx const& oc);
    void destructionInDomTreeOrder(IRBB * root, xcom::DomTree & domtree,
                                   OptCtx const& oc);

    VPR * getVPR(UINT id) const { return m_vpr_vec.get(id); }
    VPR * getVPR(PRNO prno, UINT version) const
    {
        VPRVec const* vprvec = getVPRVecByPRNO(prno);
        return vprvec != nullptr ? vprvec->get(version) : nullptr;
    }

    //Map PRNO to VPRVec that recorded all VPR during SSA.
    VPRVec const* getVPRVecByPRNO(PRNO prno) const
    { return m_prno2vprvec.get((VecIdx)prno); }
    void genSSAInfoForExp();
    SSAInfo * genSSAInfoForExp(IR * exp);
    void genSSAInfoForBBList();
    VPRVec const * getVPRVec() const { return &m_vpr_vec; }
    VPR * getVPRByPRNO(PRNO prno) const
    { return m_prno2vpr.get((VecIdx)prno); }

    //The function rename PR in BB.
    //defined_prs: record the PR set that defined in 'bb' if exist.
    void handleBBRename(PRSet const* defined_prs,
                        BBSet const* bbset, PRSet const* prset,
                        MOD IRBB * bb, MOD BB2VPMap & bb2vp);
    void handleSuccOfBB(IRBB * bb, BBSet const* bbset, PRSet const* prset);
    //idx: index of operand, start from 0.
    void handlePhiOpndInSucc(IR * ir, UINT idx, PRSet const* prset);

    inline void init()
    {
        if (m_vp_pool != nullptr) { return; }
        m_vp_count = 1;
        m_is_valid = false;
        m_prno2vprvec.set(m_rg->getPRCount(), nullptr);
        m_prno2maxversion.set(m_rg->getPRCount(), 0);
        m_vp_pool = smpoolCreate(sizeof(VPR)*2, MEM_CONST_SIZE);
    }
    void initMapInfo(DefMiscBitSetMgr & bs_mgr, OUT BB2PRSet & bb2definedprs,
                     OUT PRSet & prset);
    void initVPRStack(PRSet const& prset);
    //The function generate Phi with 'prno' and insert into 'bb'.
    //Note the function does not perform renaming of PR.
    IR * insertPhi(PRNO prno, IN IRBB * bb);
    void insertCopy(IRBB * pred, IR * store_to_phicopy);
    bool isLiveOut(PRNO prno, UINT bbid);

    //Return true if stmt dominate use's stmt, otherwise return false.
    bool isStmtDomUseInsideLoop(IR const* stmt, IR const* use,
                                LI<IRBB> const* li,
                                OptCtx const& oc) const;

    xcom::Stack<VPR*> * mapPRNO2VPRStack(PRNO prno);
    Type const* mapPRNO2Type(PRNO prno)
    { return m_prno2type.get((VecIdx)prno); }

    //Rename opnd, except PHI.
    //Walk through RHS expression of 'ir' to rename PR's VPR.
    void renameRHS(MOD IR * ir, PRSet const* prset);
    //ir: may be Phi.
    void renameLHS(MOD IR * ir, PRSet const* prset);
    bool refinePhiImpl(MOD IRBB * bb, MOD IR * ir,
                       MOD List<IRBB*> & wl, MOD BitSet & in_list,
                       IRListIter irct, OptCtx const& oc);
    void rename(PRSet const& effect_prs, BB2PRSet const& bb2definedprs,
                xcom::DomTree const& domtree);
    void renameBB(IRBB const* bb, PRSet const* prset);
    //Linear renaming algorithm.
    //bb2definedprs: for each BB, indicate PRs which has been defined.
    //bbset: if not null, indicates perform the renaming inside designated
    //       BB set.
    void renameInDomTreeOrder(MOD IRBB * root, xcom::DomTree const& domtree,
                              BB2PRSet const& bb2definedprs,
                              BBSet const* bbset, PRSet const* prset);
    void removePhiList();

    void setVPR(PRNO prno, VPR * vp);
    void stripVersionForBBSet(BBSet const& bbset, PRSet const* prset);
    void stripVersionForBBList(BBList const& bblst);
    void stripPhi(IR * phi, IRListIter phict, OptCtx const& oc);
    void stripSpecificVPR(VPR * vp);
    void stripStmtVersion(IR * stmt, PRSet const* prset,
                          xcom::BitSet & visited);

    //Place phi and assign the v0 for each PR.
    //prset: record set of PR which need to version.
    void placePhiHelper(DfMgr const& dfm, PRSet const& prset,
                        BBSet const* bbset, PR2DefBBSet const& pr2defbbset,
                        BB2PRSet const& bb2definedprs);

    //Insert phi for PR.
    //defbbs: record a list of BB that defined given PR identified by 'prno'.
    void placePhiForPR(PRNO prno, List<IRBB*> const* defbbs,
                       DfMgr const& dfm, xcom::BitSet & visited,
                       List<IRBB*> & wl, BB2PRSet const& bb2definedprs,
                       BBSet const* bbset);

    //Place phi and assign the v0 for each PR.
    //prset: record set of PR which need to version.
    void placePhi(DfMgr const& dfm, PRSet const& prset,
                  BBList const& bblist, BB2PRSet const& bb2definedprs);
    void placePhi(DfMgr const& dfm, PRSet const& prset,
                  BBSet const& bbset, BB2PRSet const& bb2definedprs);

    void verifyPhiResult(IR const* ir, List<IRBB*> const& preds,
                         bool is_vpinfo_avail, bool before_strip_version) const;
    bool verifyPrnoOfVPR() const; //Only used in PRSSAMgr.
    bool verifyVPR() const; //Only used in PRSSAMgr.
public:
    explicit PRSSAMgr(Region * rg) : Pass(rg)
    {
        clean();
        ASSERT0(rg);
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

    //After adding BB or change BB successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    void addSuccessorDesignatedPhiOpnd(IRBB * bb, IRBB * succ,
                                       PRSSAInfoCollect const& col);

    //The function add 'to' to SSAInfo of 'from'.
    //to: target IR expression.
    //from: source IR, can be stmt or expression.
    static void addUse(IR * to, IR const* from);

    //The function iterates PR operations in the IR tree that rooted by 'to',
    //and add PR to SSAInfo of 'from'.
    //to: target IR expression.
    //from: source IR, can be stmt or expression.
    static void addUseForTree(IR * to, IR const* from);

    //Build Def-Use chain for 'def' and 'use'.
    //def: def stmt that writes PR.
    //use: use expression that reads PR.
    //Note caller should guarrentee 'use' does not belong to other Def-Use
    //chain.
    //Note the function does NOT check the consistency of Prno if def or use
    //operate on PR.
    void buildDUChain(MOD IR * def, MOD IR * use);

    //The function construct SSA for IR in given region.
    //The IR list and related BB set are represented in region.
    //ssarg: a region that records a list of PR that expect to transform to
    //       SSA mode. It specified a set of BB that is used to describing
    //       the region.
    bool constructDesignatedRegion(PRSSARegion & ssarg,
                                   xcom::DomTree const& domtree);

    //Note: Non-SSA DU Chains of read/write PR will be clean and
    //unusable after SSA construction.
    void construction(OptCtx & oc);
    bool construction(xcom::DomTree & domtree, OptCtx & oc);
    size_t count_mem() const;

    //DU chain operation.
    //The function changes DEF of PRSSA DU chain from 'olddef' to 'newdef'.
    //olddef: original stmt.
    //newdef: new stmt.
    //e.g: given 'olddef'->USE, the result is 'newdef'->USE.
    void changeDef(IR * olddef, IR * newdef);

    //is_reinit: this function is invoked in reinit().
    void destroy(bool is_reinit);

    //This function perform SSA destruction via scanning BB in preorder
    //traverse dominator tree.
    //Return true if inserting copy at the head of fallthrough BB
    //of current BB's predessor.
    void destruction(xcom::DomTree & domtree, OptCtx const& oc);

    //This function perform SSA destruction via scanning BB in sequential order.
    //Note PRSSA will change PR no during PRSSA destruction. If classic DU chain
    //is valid meanwhile, it might be disrupted as well. A better way is user
    //maintain the classic DU chain, alternatively a conservative way to
    //avoid subsequent verification complaining is set the prdu invalid.
    void destruction(MOD OptCtx & oc);

    //The function dump PR info rather than specific IR stmt and exp details.
    bool dumpBrief() const;
    virtual bool dump() const;

    //This function dumps VPR structure and SSA DU info.
    //have_renamed: set true if PRs have been renamed in construction.
    void dumpAllVPR() const;
    CHAR * dumpVPR(VPR const* v, OUT CHAR * buf) const;
    void dumpSSAGraph(CHAR const* name = nullptr) const;

    //Duplicate Phi operand that is at the given position, and insert after
    //given position sequently.
    //pos: given position
    //num: the number of duplication.
    //Note caller should guarrentee the number of operand is equal to the
    //number predecessors of BB of Phi.
    void dupAndInsertPhiOpnd(IRBB const* bb, UINT pos, UINT num);

    //exp: the expression that expected to set livein.
    void findAndSetLiveinDef(IR * exp);

    //Compute SSAInfo for IRs in region that are in SSA mode.
    //Note the function does NOT maintain Version info for PR.
    void genSSAInfoForRegion();
    SSAInfo * genSSAInfoForStmt(IR * stmt);
    SSAInfo * genSSAInfo(IR * ir)
    {
        if (ir->is_exp()) { return genSSAInfoForExp(ir); }
        return genSSAInfoForStmt(ir);
    }
    SSAInfo * getSSAInfoByPRNO(PRNO prno) const { return getVPRByPRNO(prno); }
    virtual CHAR const* getPassName() const { return "PRSSA Manager"; }
    PASS_TYPE getPassType() const { return PASS_PRSSA_MGR; }

    //Generate Label for the predecessor BB that corresponding to the specific
    //phi operand.
    static void genLabForInputBBOfPhiOpnd(IRCFG const* cfg);

    //Return true if bb has PHI.
    static bool hasPhi(IRBB const* bb)
    {
        IR const* ir = BB_first_ir(const_cast<IRBB*>(bb));
        return ir != nullptr && ir->is_phi();
    }

    //Return true if the value of ir1 and ir2 are definitely same, otherwise
    //return false to indicate unknown.
    static bool hasSameValue(IR const* ir1, IR const* ir2);

    //Initialize VPR and Type for each PR.
    //ir can be stmt or expression.
    //Note the function record the type that first met as the initial type of
    //specific Prno.
    void initVPR(MOD IR * ir, PRSet const* prset);

    //Insert operand at given position.
    //pos: position of operand, start at 0.
    //     Each operand correspond to in-edge on CFG.
    //pred: the predecessor that corresponding to the position in operand list.
    IR * insertOpndAt(IR * phi, UINT pos, IRBB const* pred);

    //Return true if ir dominates all its USE expressions which inside loop.
    //In ssa mode, stmt's USE may be placed in operand list of PHI.
    bool isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li,
                                   OptCtx const& oc) const;

    //Move IR_PHI from 'from' to 'to'.
    //This function often used in updating PHI when adding new dominater
    //BB to 'to'.
    static void movePhi(IRBB * from, IRBB * to);

    //This function revise phi data type, and remove redundant phi.
    //Return true if there is phi removed.
    bool refinePhi(OptCtx const& oc);

    //Reinitialize SSA manager.
    //This function will clean all informations and recreate them.
    inline void reinit()
    {
        destroy(true);
        init();
    }
    //Before removing BB or change BB successor,
    //you need remove the related PHI operand if BB successor has PHI.
    void removeSuccessorDesignatedPhiOpnd(IRBB const* succ, UINT ps);

    //Remove PR-SSA Use-Def chain.
    //e.g:ir=...
    //    ...=ir //S1
    //If S1 deleted, ir should be removed from its UseSet in SSAInfo.
    //NOTE: If ir is an IR tree, e.g: add(pr1, pr2), removing 'add' means
    //pr1 and pr2 will be removed as well. Therefore pr1 pr2's SSAInfo will be
    //updated as well.
    static void removePRSSAOcc(IR const* ir);

    //Remove Use-Def chain.
    //exp: the expression to be removed.
    //e.g: ir = ...
    //    = ir //S1
    //If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
    //NOTE: the function only process exp itself.
    static void removeUse(IR const* ir);

    //Remove Use-Def chain for all memory references in IR Tree that rooted by
    //exp.
    //exp: it is the root of IR tree that to be removed.
    //e.g: ir = ...
    //        = exp //S1
    //If S1 will be deleted, exp should be removed from its UseSet in MDSSAInfo.
    //NOTE: If exp is an IR tree, e.g: ild(x, ld(y)), remove ild(x) means
    //ld(y) will be removed as well. And ld(y)'s MDSSAInfo will be
    //updated as well.
    static void removeUseForTree(IR const* ir);

    //Check each USE of stmt, remove the expired one which is not reference
    //the memory any more that stmt defined.
    //Return true if DU changed.
    static bool removeExpiredDUForStmt(IR const* stmt, Region * rg);

    //Check each USE of stmt, remove the expired one which is not reference
    //the memory any more that stmt defined.
    static bool removeExpiredDU(IR const* ir, Region * rg);

    //Remove Def-Use chain between 'def' and 'use'.
    //def: def stmt that writes PR.
    //use: use expression that reads PR.
    //Note caller should guarrentee 'use' does not belong to other Def-Use
    //chain.
    //Note the function does NOT check the consistency of Prno if def or use
    //operate on PR.
    static void removeDUChain(IR * def, IR * use);

    bool verifyPhi(bool is_vpinfo_avail, bool before_strip_version) const;

    //The verification check the DU info in SSA form.
    //Current IR must be in SSA form.
    bool verifySSAInfo() const; //Can be used in any module.
    static bool verifyPRSSAInfo(Region const* rg);

    virtual bool perform(OptCtx & oc) { construction(oc); return true; }
};


inline void copySSAInfo(IR * tgt, IR const* src)
{
    ASSERT0(tgt && src);
    ASSERT0(tgt->isPROp() && src->isPROp());
    tgt->setSSAInfo(src->getSSAInfo());
}

} //namespace xoc
#endif
