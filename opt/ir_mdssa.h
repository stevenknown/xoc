/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _IR_MDSSA_H_
#define _IR_MDSSA_H_

namespace xoc {

typedef xcom::TMap<UINT, VMD*> MD2VMD;
typedef xcom::TMap<UINT, MDPhiList*> BB2MDPhiList;
typedef xcom::TMapIter<UINT, MDPhiList*> BB2MDPhiListIter;
typedef xcom::List<MDDef*> MDDefIter;
typedef TTab<UINT> LiveInMDTab;
typedef TTabIter<UINT> LiveInMDTabIter;
typedef xcom::DefSBitSet DefMDSet;
typedef xcom::DefSBitSetIter DefMDSetIter;

class BB2DefMDSet : public Vector<DefMDSet*> {
    COPY_CONSTRUCTOR(BB2DefMDSet);
public:
    BB2DefMDSet() {}
    void dump(Region const* rg) const;
};


class BB2VMDMap : public Vector<MD2VMD*> {
    COPY_CONSTRUCTOR(BB2VMDMap);
    bool checkClean()
    {
        //Verify if vpmap of each BB has been deleted.
        for (VecIdx i = 0; i <= get_last_idx(); i++) {
            ASSERT0(get(i) == nullptr);
        }
        return true;
    }
public:
    BB2VMDMap() {}
    BB2VMDMap(UINT n) : Vector<MD2VMD*>(n) {}
    ~BB2VMDMap() { ASSERT0(checkClean()); }

    MD2VMD * gen(UINT bbid)
    {
        MD2VMD * mdid2vmd = get(bbid);
        if (mdid2vmd == nullptr) {
            mdid2vmd = new MD2VMD();
            set(bbid, mdid2vmd);
        }
        return mdid2vmd;
    }

    void erase(UINT bbid)
    {
        MD2VMD * mdid2vmd = get(bbid);
        if (mdid2vmd != nullptr) {
            delete mdid2vmd;
            set(bbid, nullptr);
        }
    }

    void setElemNum(UINT n) { grow(n); }
};


//Mapping from MD id to Stack of VMD.
class MD2VMDStack : public Vector<Stack<VMD*>*> {
    COPY_CONSTRUCTOR(MD2VMDStack);
public:
    MD2VMDStack() {}
    ~MD2VMDStack() { destroy(); }

    void clean() { destroy(); Vector<Stack<VMD*>*>::init(); }

    void dump(Region const* rg) const;
    void destroy();

    Stack<VMD*> * gen(UINT mdid);
    VMD * get_top(VMD const* md) const { return get_top(md->mdid()); }
    VMD * get_top(UINT mdid) const;

    void push(VMD * vmd) { push(vmd->mdid(), vmd); }
    void push(UINT mdid, VMD * vmd);
};


//The class define the iterator that used to iterate IR occurrence for
//each VOpnd in MDSSA mode.
class ConstMDSSAUSEIRIter {
    COPY_CONSTRUCTOR(ConstMDSSAUSEIRIter);
public:
    ConstMDSSAUSEIRIter() : vopndset_iter(nullptr),
        current_pos_in_vopndset(BS_UNDEF),
        current_pos_in_useset(BS_UNDEF),
        current_useset(nullptr) {}

    VOpndSet * vopndset;
    VOpndSetIter vopndset_iter;
    BSIdx current_pos_in_vopndset;
    VMD::UseSetIter useset_iter;
    BSIdx current_pos_in_useset;
    VMD::UseSet const* current_useset;

    void clean()
    {
        vopndset_iter = nullptr;
        current_pos_in_vopndset = BS_UNDEF;
        useset_iter.clean();
        current_pos_in_useset = BS_UNDEF;
        current_useset = nullptr;
    }
};


class ConstMDDefIter : public xcom::List<MDDef const*> {
    COPY_CONSTRUCTOR(ConstMDDefIter);
    TTab<UINT> m_is_visited;
public:
    ConstMDDefIter() {}
    bool is_visited(MDDef const* def) const
    { return m_is_visited.find(def->id()); }
    void set_visited(MDDef const* def) { m_is_visited.append(def->id()); }
};


class LiveSet {
    COPY_CONSTRUCTOR(LiveSet);
    xcom::DefMiscBitSetMgr * m_sbsmgr;
    VOpndSet m_set;
public:
    LiveSet(VMD const* vmd, xcom::DefMiscBitSetMgr * sbsmgr)
    {
        m_set.bunion(vmd->id(), *sbsmgr);
        m_sbsmgr = sbsmgr;
    }
    LiveSet(VOpndSet const& set, xcom::DefMiscBitSetMgr * sbsmgr)
    {
        m_set.copy(set, *sbsmgr);
        m_sbsmgr = sbsmgr;
    }
    ~LiveSet()
    {
        //Should call clean() before destruction,
        //otherwise it will incur SegMgr assertion.
        m_set.clean(*m_sbsmgr);
    }

    bool all_killed() const { return m_set.is_empty(); }

    void copy(LiveSet const& src) { copy(src.m_set); }
    void copy(VOpndSet const& vopndset) { m_set.copy(vopndset, *m_sbsmgr); }

    void dump(MDSSAMgr const* mgr) const;

    VOpndSet & getSet() { return m_set; }

    bool is_live(UINT id) const { return m_set.is_contain(id); }

    void set_killed(UINT id) { m_set.diff(id, *m_sbsmgr); }
};


class Vertex2LiveSet {
    COPY_CONSTRUCTOR(Vertex2LiveSet);
    xcom::TMap<UINT, LiveSet*> m_bbid2set;
    xcom::DefMiscBitSetMgr m_sbsmgr;
public:
    Vertex2LiveSet() {}
    ~Vertex2LiveSet()
    {
        TMapIter<UINT, LiveSet*> it;
        LiveSet * set;
        for (m_bbid2set.get_first(it, &set); set != nullptr;
             m_bbid2set.get_next(it, &set)) {
            delete set;
        }
    }
    void dump(Region const* rg) const;

    LiveSet * genAndCopy(UINT bbid, LiveSet const& src)
    { return genAndCopy(bbid, const_cast<LiveSet&>(src).getSet()); }
    LiveSet * genAndCopy(UINT bbid, VOpndSet const& src)
    {
        LiveSet * liveset = m_bbid2set.get(bbid);
        if (liveset == nullptr) {
            liveset = new LiveSet(src, &m_sbsmgr);
            m_bbid2set.set(bbid, liveset);
        } else {
            liveset->getSet().copy(src, m_sbsmgr);
        }
        return liveset;

    }
    LiveSet * genAndCopy(UINT bbid, VMD const* vmd)
    {
        LiveSet * liveset = new LiveSet(vmd, &m_sbsmgr);
        ASSERT0(m_bbid2set.get(bbid) == nullptr);
        m_bbid2set.set(bbid, liveset);
        return liveset;
    }
    LiveSet * get(UINT bbid) const { return m_bbid2set.get(bbid); }
    void free(UINT bbid)
    {
        LiveSet * set = m_bbid2set.remove(bbid);
        if (set != nullptr) { delete set; }
    }
};


//
//START RenameDef
//
//The class generates VMD for 'newstmt' or 'newphi', then insert VMD into
//DefDef chain, rename following USE as well.
class RenameDef {
    COPY_CONSTRUCTOR(RenameDef);
    bool m_is_build_ddchain; //true to build DefDef chain with DomTree.

    //Record the stmt that inserted.
    //The class will generate MDSSAInfo if it does not have one.
    IR * m_newstmt;
    MDPhi const* m_newphi; //record the phi that inserted
    DomTree const& m_domtree;
    LiveSet * m_liveset;
    MDSSAMgr * m_mgr;
    IRCFG * m_cfg;
    Vertex2LiveSet m_vex2liveset;
private:
    void connect(Vertex const* defvex, LiveSet * defliveset,
                 IRBB const* start_bb, IR const* start_ir);

    //stmtbbid: indicates the BB of inserted stmt
    //v: the vertex on DomTree.
    //bb: the BB that to be renamed
    //dompred: indicates the predecessor of 'bb' in DomTree
    //Note stmtbbid have to dominate 'bb'.
    void connectDefInBBTillPrevDef(IRBB const* bb,
                                   BBIRListIter & irlistit,
                                   MOD LiveSet & liveset);
    void connectIRTillPrevDef(IRBB const* bb,
                              BBIRListIter & irlistit,
                              MOD LiveSet & liveset);
    void connectPhiTillPrevDef(IRBB const* bb,
                               BBIRListIter & irlistit,
                               MOD LiveSet & liveset);
    void connectDefInterBBTillPrevDef(Vertex const* defvex,
                                      MOD LiveSet & stmtliveset,
                                      IRBB const* start_bb);

    void iterBBPhiListToKillLivedVMD(IRBB const* bb, LiveSet & liveset);
    void iterSuccBBPhiListToRename(Vertex const* defvex, IRBB const* succ,
                                   UINT opnd_idx, MOD LiveSet & liveset);
    //defvex: domtree vertex.
    void iterSuccBB(Vertex const* defvev, MOD LiveSet & liveset);

    void killLivedVMD(MDPhi const* phi, MOD LiveSet & liveset);

    void processStmt();
    void processPhi();

    void renamePhiOpnd(MDPhi const* phi, UINT opnd_idx, MOD VMD * vmd);

    //stmtbb: the BB of inserted stmt
    //newinfo: MDSSAInfo that intent to be swap-in.
    bool renameVMDForDesignatedPhiOpnd(MDPhi * phi, UINT opnd_pos,
                                       LiveSet & liveset);
    //vmd: intent to be swap-in.
    //irtree: may be stmt or exp.
    //irit: for local used.
    void renameVMDForIRTree(IR * irtree, VMD * vmd, MOD IRIter & irit,
                            bool & no_exp_has_ssainfo);
    //ir: may be stmt or exp
    //irit: for local used.
    void renameLivedVMDForIRTree(IR * ir, MOD IRIter & irit,
                                 LiveSet const& liveset);
    void renameIRTillNextDef(IRBB const* bb, BBIRListIter & irlistit,
                             MOD LiveSet & liveset);

    //stmtbbid: indicates the BB of inserted stmt
    //bb: the BB that to be renamed
    //dompred: indicates the predecessor of 'bb' in DomTree
    //Note stmtbbid have to dominate 'bb'.
    void renameUseInBBTillNextDef(Vertex const* defvex, IRBB const* bb,
                                  bool include_philist,
                                  BBIRListIter & irlistit,
                                  OUT LiveSet & liveset);
    //start_ir: if it is nullptr, the renaming will start at the first IR in bb.
    //          otherwise the renaming will start at the NEXT IR of start_ir.
    void renameFollowUseIntraBBTillNextDef(Vertex const* defvex,
                                           MOD LiveSet & stmtliveset,
                                           IRBB const* start_bb,
                                           IR const* start_ir);
    void renameFollowUseInterBBTillNextDef(Vertex const* defvex,
                                           MOD LiveSet & stmtliveset,
                                           IRBB const* start_bb);
    void rename(Vertex const* defvex, LiveSet * defliveset,
                IRBB const* start_bb, IR const* start_ir);

    bool tryInsertDDChainForDesigatedVMD(MDPhi * phi, VMD * vmd,
                                         MOD LiveSet & liveset);
    //vmd: new generated VMD that to be inserted.
    //before: true to insert 'vmd' in front of 'ir'.
    bool tryInsertDDChainForDesigatedVMD(IR * ir, VMD * vmd, bool before,
                                         MOD LiveSet & liveset);
    bool tryInsertDDChainForStmt(IR * ir, bool before, MOD LiveSet & liveset);
    bool tryInsertDDChainForPhi(MDPhi * phi, MOD LiveSet & liveset);
public:
    RenameDef(MOD IR * stmt, DomTree const& dt, bool build_ddchain,
              MDSSAMgr * mgr);
    RenameDef(MDPhi const* phi, DomTree const& dt, bool build_ddchain,
              MDSSAMgr * mgr);

    MDSSAMgr * getMgr() const { return m_mgr; }

    void perform();
};
//END RenameDef


//The class reconstruct MDSSA info for given region in DomTree order.
//The region recorded in 'm_vextab'
class ReconstructMDSSA : public VisitTree {
    COPY_CONSTRUCTOR(ReconstructMDSSA);
    //Record the vertex on CFG that need to revise.
    xcom::VexTab const& m_vextab;
    MDSSAMgr * m_mdssamgr;
    xcom::Graph const* m_cfg;
    Region const* m_rg;
private:
    void renameBBIRList(IRBB const* bb) const;
    void renameBBPhiList(IRBB const* bb) const;
public:
    ReconstructMDSSA(xcom::Vertex const* root, xcom::VexTab const& vextab,
                     xcom::DomTree const& domtree, xcom::Graph const* cfg,
                     MDSSAMgr * mgr);

    //The interface of VisitTree to access each Vertex.
    //v: the vertex on DomTree.
    virtual void visitWhenFirstMeet(Vertex const* v)
    {
        Vertex const* cfgv = m_cfg->getVertex(v->id());
        ASSERT0(cfgv);
        if (!m_vextab.find(cfgv)) { return; }
        IRBB * bb = m_rg->getBB(cfgv->id());
        ASSERT0(bb);
        renameBBPhiList(bb);
        renameBBIRList(bb);
    }
};


//MDSSA Update Context
//The class records and propagates auxiliary information to maintain MDSSA
//information during miscellaneous optimizations.
#define MDSSAUPDATECTX_update_duchain(x) ((x)->m_update_duchain_by_dominfo)
class MDSSAUpdateCtx {
public:
    //Pass info top-down.
    //True to ask MDSSAMgr to maintain MDSSA DU chain by DomInfo.
    BYTE m_update_duchain_by_dominfo:1;
    OptCtx const& m_oc;
public:
    MDSSAUpdateCtx(OptCtx const& oc) : m_oc(oc)
    { MDSSAUPDATECTX_update_duchain(this) = true; }
    MDSSAUpdateCtx const& operator = (MDSSAUpdateCtx const&);

    OptCtx const& getOptCtx() const { return m_oc; }

    bool need_update_duchain() const
    { return MDSSAUPDATECTX_update_duchain(this); }
};


//The class construct MDSSA form and manage the MDSSA information for
//stmt and expression.
//MDSSAInfo is only avaiable to Memory Reference IR operations, include
//IR_LD, IR_ST, IR_ILD, IR_IST, IR_ARRAY, IR_STARRAY, IR_ID.
//MDSSA information is constructed for each MD of IR, thus for a given
//IR stmt/expression, one can get a set of virtual MD. Each virtual MD
//have an unique DEF IR stmt, and a list of USE IR expressions. There
//is double link in bewteen any of two virtual MD that describe same real MD.
//The DU Chain of stmt and expression is consist a DEF and its USE set.
//The DU Chain of stmt and stmt is consist of a list of linked DEF of
//virtual MD with different version.
//If you are going to remove USE of virtual MD, remove IR expression from
//USE set in paticular virtual MD, and remove the virtual MD from current
//IR's MDSSAInfo.
class MDSSAMgr : public Pass {
    friend class MDPhi;
    friend class RenameInDomTree;
    COPY_CONSTRUCTOR(MDSSAMgr);
protected:
    BYTE m_is_semi_pruned:1;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    xcom::DefSegMgr * m_seg_mgr;
    IRIter m_iter; //for tmp use.

    //Record version stack during renaming.
    MD2VMDStack m_map_md2stack;

    //record version number counter for pr.
    xcom::Vector<UINT> m_max_version;

    UseDefMgr m_usedef_mgr;
protected:
    //Add ir to given mdssainfo as an USE.
    //ir: occurence to be added.
    //mdssainfo: add ir to the UseSet of VOpnd that recorded in 'mdssainfo'.
    //Note mdssainfo must be unique for each IR.
    void addUseToMDSSAInfo(IR const* ir, MDSSAInfo * mdssainfo);
    void addDefChain(MDDef * def1, MDDef * def2);

    //NOTE the function only should be called at constructor.
    void cleanInConstructor()
    {
        m_is_valid = false;
        m_is_semi_pruned = true;
    }
    void cleanIRSSAInfo(IRBB * bb);
    void cleanMDSSAInfoAI();
    void cutoffDefChain(MDDef * def);

    //The function destroy data structures that allocated during SSA
    //construction, and these data structures are only useful in construction.
    void cleanLocalUsedData();
    void collectDefinedMD(IRBB const* bb, OUT DefMDSet & maydef) const;
    void collectDefinedMDAndInitVMD(IN IRBB * bb, OUT DefMDSet & maydef);
    void collectUseMD(IR const* ir, OUT LiveInMDTab & livein_md);

    //livein_md: record MDs that defined in 'bb'.
    void computeLiveInMD(IRBB const* bb, OUT LiveInMDTab & livein_md);

    //Destruction of MDSSA.
    //The function perform SSA destruction via scanning BB in preorder
    //traverse dominator tree.
    //Return true if inserting copy at the head of fallthrough BB
    //of current BB's predessor.
    void destruction(DomTree & domtree);
    void destructBBSSAInfo(IRBB * bb);
    void destructionInDomTreeOrder(IRBB * root, DomTree & domtree);
    //The function dump all possible DEF of 'vopnd' by walking through the
    //Def Chain.
    void dumpDefByWalkDefChain(List<MDDef const*> & wl, IRSet & visited,
                               VMD const* vopnd) const;
    void dumpExpDUChainIter(IR const* ir, List<IR*> & lst,
                            List<IR*> & opnd_lst,
                            OUT bool * parting_line) const;
    void dumpDUChainForStmt(IR const* ir, bool & parting_line) const;
    void dumpDUChainForStmt(IR const* ir,
                            xcom::List<IR*> & lst,
                            xcom::List<IR*> & opnd_lst) const;
    void dumpBBRef(IN IRBB * bb, UINT indent);
    void dumpIRWithMDSSAForStmt(IR const* ir, UINT flag,
                                bool & parting_line) const;
    void dumpIRWithMDSSAForExp(IR const* ir, UINT flag,
                               bool & parting_line) const;
    bool doOpndHaveValidDef(MDPhi const* phi) const;
    bool doOpndHaveSameDef(MDPhi const* phi, OUT VMD ** common_def) const;

    //id: input ID.
    //ssainfo: MDSSAInfo of id.
    //olddef: old DEF of id.
    void findNewDefForID(IR * id, MDSSAInfo * ssainfo, MDDef * olddef,
                         OptCtx const& oc);
    VMD * findLiveInDefFrom(IRBB const* bb, MDIdx mdid, IR const* startir,
                            IRBB const* startbb,
                            VMDVec const* vmdvec) const;
    void freeBBPhiList(IRBB * bb);
    void freePhiList();

    VMD * genVMD(UINT mdid, UINT version)
    { return getUseDefMgr()->allocVMD(mdid, version); }
    MDDef * genMDDefStmt(IR * ir, VMD * result);

    //Replace opnd of PHI of 'succ' with top SSA version.
    void handlePhiInSuccBB(IRBB * succ, UINT opnd_idx, MD2VMDStack & md2vmdstk);
    void handleBBRename(IRBB * bb, DefMDSet const& effect_mds,
                        DefMDSet const& defed_mds,
                        MOD BB2VMDMap & bb2vmdmap,
                        MD2VMDStack & md2vmdstk);

    void init()
    {
        if (m_usedef_mgr.m_mdssainfo_pool != nullptr) { return; }
        m_is_valid = false;
    }
    void initVMD(IN IR * ir, OUT DefMDSet & maydef);

    //Insert a new PHI into bb according to given MDIdx.
    //Note the operand of PHI will be initialized in initial-version.
    MDPhi * insertPhi(UINT mdid, IN IRBB * bb)
    {
        UINT num_opnd = bb->getVex()->getInDegree();
        return insertPhi(mdid, bb, num_opnd);
    }

    //Return true if phi is killing-def.
    bool isPhiKillingDef(MDPhi const* phi) const;

    //Return true if stmt dominate use's stmt, otherwise return false.
    bool isStmtDomUseInsideLoop(IR const* stmt, IR const* use,
                                LI<IRBB> const* li) const;

    void renamePhiOpndInSuccBB(IRBB * bb, MD2VMDStack & md2vmdstk);
    void renamePhiResult(IN IRBB * bb, MD2VMDStack & md2vmdstk);
    void renameUse(IR * ir, MD2VMDStack & md2vmdstk);
    void renameDef(IR * ir, IRBB * bb, MD2VMDStack & md2vmdstk);
    void rename(DefMDSet const& effect_mds,
                BB2DefMDSet & defed_mds_vec,
                DomTree & domtree,
                MD2VMDStack & md2vmdstk);
    void renameBB(IRBB * bb, MD2VMDStack & md2vmdstk);

    //The function remove 'vopnd' from MDSSAInfo of each ir in its UsetSet.
    //Note the UseSet will be clean.
    //ctx: if ctx is nullptr, the function perform normal update.
    void removeVOpndForAllUse(MOD VMD * vopnd, MDSSAUpdateCtx const& ctx);

    //The function changes VOpnd of 'from' to 'to', for each elements in 'from'
    //UseSet.
    //Note all elements in UseSet of 'from' will be removed.
    void replaceVOpndForAllUse(MOD VMD * to, MOD VMD * from);

    //The function remove and clean all informations of 'vmd' from MDSSAMgr.
    void removeVMD(VMD * vmd) { getUseDefMgr()->removeVMD(vmd); }

    //Remove all VMD in set from MDSSAMgr. The function will clean all information
    //about these VMDs.
    void removeVMDInSet(VOpndSet const& set);

    //wl: is an optional parameter to record BB which expected to deal with.
    //    It is a work-list that is used to drive iterative collection and
    //    elimination of redundant PHI.
    bool removePhiHasNoValidDef(List<IRBB*> * wl, MDPhi * phi,
                                OptCtx const& oc);

    //wl: is an optional parameter to record BB which expected to deal with.
    //    It is a work-list that is used to drive iterative collection and
    //    elimination of redundant PHI.
    bool removePhiHasCommonDef(List<IRBB*> * wl, MDPhi * phi, OptCtx const& oc);

    //Remove PHI that without any USE.
    //Return true if any PHI removed, otherwise return false.
    bool removePhiNoUse(MDPhi * phi, OptCtx const& oc);

    //Record all modified MDs which will be versioned later.
    void recordEffectMD(IRBB const* bb, OUT DefMDSet & effect_md);

    //Remove MDDef from DefDef chain.
    //Note the function does not deal with MDSSAInfo of IR occurrence, and just
    //process DefDef chain that built on MDDef.
    //mddef: will be removed from DefDef chain, and be modified as well.
    //prev: previous Def to mddef, and will be modified.
    //e.g:D1->D2
    //     |->D3
    //     |  |->D5
    //     |  |->D6
    //     |->D4
    //  where D1 is predecessor of D2, D3 and D4; D3 is predecssor of D5, D6.
    //  After remove D3:
    //e.g:D1->D2
    //     |->D5
    //     |->D6
    //     |->D4
    //  where D1 is predecessor of D2, D5, D6, D4.
    void removeDefFromDDChainHelper(MDDef * mddef, MDDef * prev);

    bool prunePhi(List<IRBB*> & wl, OptCtx const& oc);
    bool prunePhiForBB(IRBB const* bb, List<IRBB*> * wl, OptCtx const& oc);

    //Insert PHI for VMD.
    //defbbs: record BBs which defined the VMD identified by 'mdid'.
    //visited: record visited BB id
    void placePhiForMD(UINT mdid, List<IRBB*> const* defbbs,
                       DfMgr const& dfm, xcom::BitSet & visited,
                       List<IRBB*> & wl,
                       BB2DefMDSet & defmds_vec);
    void placePhi(DfMgr const& dfm, MOD DefMDSet & effect_md,
                  DefMiscBitSetMgr & bs_mgr,
                  BB2DefMDSet & defined_md_vec,
                  List<IRBB*> & wl);

    //Union successors in NextSet from 'from' to 'to'.
    void unionSuccessors(MDDef const* from, MDDef const* to);

    bool verifyDUChainAndOccForPhi(MDPhi const* phi) const;
    bool verifyPhiOpndList(MDPhi const* phi, UINT prednum) const;
    bool verifyMDSSAInfoUniqueness() const;
    void verifyDef(MDDef const* def, VMD const* vopnd) const;
    //Check SSA uses.
    void verifyUseSet(VMD const* vopnd) const;
    void verifyMDSSAInfoForIR(IR const* ir) const;
    bool verifyRefedVMD() const;
    bool verifyAllVMD() const;
public:
    explicit MDSSAMgr(Region * rg) :
        Pass(rg),
        m_sbs_mgr(rg->getMiscBitSetMgr()),
        m_seg_mgr(rg->getMiscBitSetMgr()->getSegMgr()),
        m_usedef_mgr(rg, this)
    {
        cleanInConstructor();
        ASSERT0(rg);
        m_tm = rg->getTypeMgr();
        m_irmgr = rg->getIRMgr();
        ASSERT0(m_tm);
        ASSERT0(rg->getMiscBitSetMgr());
        ASSERT0(m_seg_mgr);
        m_cfg = rg->getCFG();
        ASSERTN(m_cfg, ("cfg is not available."));
        m_md_sys = rg->getMDSystem();
    }
    ~MDSSAMgr()
    {
        //CAUTION: If you do not finish out-of-SSA prior to destory(),
        //the reference to IR's MDSSA info will lead to undefined behaviors.
        //ASSERTN(!is_valid(), ("should be destructed"));

        destroy();
    }

    //Add occurence to each vopnd in mdssainfo.
    //ir: occurence to be added.
    //ref: the reference that is isomorphic to 'ir'.
    //     It is used to retrieve MDSSAInfo.
    void addMDSSAOccForTree(IR * ir, IR const* ref);

    //After adding BB or change BB successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    void addSuccessorDesignatedPhiOpnd(IRBB * bb, IRBB * succ, OptCtx const& oc);
    void addStmtToMDSSAMgr(IR * ir, IR const* ref);

    //Build DU chain from 'def' to 'exp'.
    //The function will add VOpnd of phi to 'exp'.
    void buildDUChain(MDDef const* def, MOD IR * exp);

    //Construction of MDSSA form.
    //Note: Non-SSA DU Chains will be maintained after construction.
    void construction(OptCtx & oc);

    //Construction of MDSSA form.
    bool construction(DomTree & domtree, OptCtx & oc);
    size_t count_mem() const;

    //DU chain operation.
    //Change Def stmt from 'olddef' to 'newdef'.
    //olddef: original stmt.
    //newdef: target stmt.
    //e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
    void changeDef(IR * olddef, IR * newdef);

    //DU chain operation.
    //Change Def stmt from orginal MDDef to 'newdef'.
    //oldvmd: original VMD.
    //newdef: target MDDef.
    //e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
    void changeDef(VMD * oldvmd, MDDef * newdef);

    //DU chain operation.
    //Change Defined VMD from 'oldvmd' to 'newvmd'.
    //Note the function is similar to changeDef(), however it handle VMD rather
    //than IR.
    //oldvmd: original VMD.
    //newvmd: target VMD.
    //e.g: oldvmd:MD5V1->{USE1,USE2}, newvmd:MD5V2->{USE3,USE4}
    //after change: MD5V2->{USE1,USE2,USE3,USE4}.
    void changeVMD(VMD * oldvmd, VMD * newvmd);

    //DU chain operation.
    //Change Use expression from 'olduse' to 'newuse'.
    //olduse: single source expression.
    //newuse: single target expression.
    //e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
    void changeUse(IR * olduse, IR * newuse);

    //DU chain operation.
    //Change VOpnd of exp to a set of VOpnd that defined outside the 'li'.
    //exp: expression to be changed, its MDSSAInfo should be available.
    //li: given loopinfo.
    //e.g: given oldef->USE, change to newdef->USE.
    void changeDefToOutsideLoopDef(IR * exp, LI<IRBB> const* li);
    void changeDefToOutsideLoopDefForTree(IR * exp, LI<IRBB> const* li);

    //DU chain operation.
    //Change Use expression from 'olduse' to 'newuse'.
    //olduse: source expression as tree root.
    //newuse: target expression as tree root.
    //e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
    void changeUseForTree(IR * olduse, IR * newuse, MDSSAUpdateCtx const& ctx);

    //Coalesce DU chain, actually the version of MD, from 'src' to 'tgt'.
    //'src' and 'tgt' refered the same MD.
    //This function replace definition of USE of src to tgt's defintion.
    //There is always removeStmt() following the function call.
    //'src' and 'tgt' is the form of copy operation.
    //e.g: p0 =...
    //     p1 = p0
    //     ...= p1
    //=> after coalescing, p1 is src, p0 is tgt
    //     p0 = ...
    //     ------ //removed
    //     ... = p0
    void coalesceDUChain(IR const* src, IR const* tgt);
    void cleanMDSSAInfo(IR * ir) { getUseDefMgr()->cleanMDSSAInfo(ir); }

    //The function copy MDSSAInfo from 'src' to tgt.
    void copyMDSSAInfo(IR * tgt, IR const* src);

    //The function copy MDSSAInfo from tree 'src' to tree tgt.
    //Note src and tgt must be isomorphic.
    void copyMDSSAInfoForTree(IR * tgt, IR const* src);

    //The function copy MDSSAInfo from 'src' to ir. Then add ir as an USE of the
    //new MDSSAInfo.
    MDSSAInfo * copyAndAddMDSSAOcc(IR * ir, MDSSAInfo const* src);
    void collectDefinedMDForBBList(MOD DefMiscBitSetMgr & bs_mgr,
                                   OUT BB2DefMDSet & bb2defmds) const;

    //Return true if VMDs of stmt cross version when moving stmt
    //outside of loop.
    bool isCrossLoopHeadPhi(IR const* stmt, LI<IRBB> const* li,
                            OUT bool & cross_nonphi_def) const;

    //Return true if the DU chain between 'def' and 'use' can be ignored during
    //DU chain manipulation.
    //def: one of DEF of exp.
    //exp: expression.
    bool isOverConservativeDUChain(MDDef const* def, IR const* exp) const;

    //Return true if the DU chain between 'def' and 'use' can be ignored during
    //DU chain manipulation.
    //ir1: related DEF or USE to same VMD, can be stmt/exp.
    //ir2: related DEF or USE to same VMD, can be stmt/exp.
    static bool isOverConservativeDUChain(IR const* ir1, IR const* ir2,
                                          Region const* rg);

    //Destroy all objects in MDSSAMgr.
    void destroy();

    //Destruction of MDSSA.
    void destruction(OptCtx & oc);

    //Dump Phi List.
    void dumpPhiList(MDPhiList const* philist) const;

    //Dump MDSSA reference info.
    virtual bool dump() const;

    //Dump MDSSA DU chain.
    void dumpDUChain() const;

    //Dump MDSSA VOpnd reference.
    void dumpVOpndRef() const;

    //Dump IRBB list with MDSSA info.
    void dumpBBList() const;

    //The function dumps VMD structure and SSA DU info.
    void dumpAllVMD() const;

    //Dump VMD info into given buffer.
    //Note the buffer have to at least 128 bytes.
    CHAR * dumpVMD(IN VMD * v, OUT CHAR * buf) const;

    //Dump IR tree and MDSSAInfo if any.
    //ir: can be stmt or expression.
    //flag: the flag to dump IR.
    void dumpIRWithMDSSA(IR const* ir, UINT flag = IR_DUMP_COMBINE) const;

    //Duplicate Phi operand that is at the given position, and insert after
    //given position sequently.
    //pos: given position
    //num: the number of duplication.
    //Note caller should guarrentee the number of operand is equal to the
    //number predecessors of BB of Phi.
    void dupAndInsertPhiOpnd(IRBB const* bb, UINT pos, UINT num);

    //Find killing must-def IR stmt for expression ir.
    //Return the IR stmt if found.
    //e.g: g is global variable, it is exact.
    //x is a pointer that we do not know where it pointed to.
    //    1. *x += 1; # *x may overlapped with g
    //    2. g = 0; # exactly defined g
    //    3. call foo(); # foo may overlapped with g
    //    4. return g;
    //In the case, the last reference of g in stmt 4 may be defined by
    //stmt 1, 2, 3, there is no nearest killing def.
    IR * findKillingDefStmt(IR const* ir) const;

    //Find killing must-def Virtual-DEF for expression ir.
    //Return the MDDef if found.
    //e.g: g is global variable, it is exact.
    //x is a pointer that we do not know where it pointed to.
    //    1. *x += 1; # *x may overlapped with g
    //    2. g = 0; # exactly defined g
    //    3. call foo(); # foo may overlapped with g
    //    4. return g;
    //In the case, the last reference of g in stmt 4 may be defined by
    //stmt 1, 2, 3, there is no nearest killing def.
    MDDef * findKillingMDDef(IR const* ir) const;

    //Find nearest virtual DEF in VOpndSet of 'ir'.
    MDDef * findNearestDef(IR const* ir) const;

    //Find the MustDef of 'ir'.
    MDDef * findMustDef(IR const* ir) const;

    //Find VMD from ir list and phi list.
    VMD * findLastMayDef(IRBB const* bb, MDIdx mdid) const;

    //Find VMD from ir list and phi list.
    VMD * findLastMayDefFrom(IRBB const* bb, IR const* start, MDIdx mdid) const;
    VMD * findVMDFromPhiList(IRBB const* bb, MDIdx mdid) const;

    //The function try to find the unique MDDef for given def that is outside
    //of the loop.
    //Return the MDDef if found, otherwise nullptr.
    MDDef const* findUniqueOutsideLoopDef(MDDef const* phi,
                                          LI<IRBB> const* li) const;

    //Find livein def-stmt through given start IR at start BB.
    //Note DOM info must be available.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until the
    //         CFG entry.
    //startbb: the BB that begin to do searching.
    VMD * findDomLiveInDefFrom(MDIdx mdid, IR const* startir,
                               IRBB const* startbb, OptCtx const& oc) const;

    //The function do searching that begin at the IDom BB of marker.
    //Note DOM info must be available.
    VMD * findDomLiveInDefFromIDomOf(IRBB const* marker, MDIdx mdid,
                                     OptCtx const& oc) const;

    //Note DOM info must be available.
    //exp: the expression that expected to set livein.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until the
    //         CFG entry.
    //startbb: the BB that begin to do searching.
    void findAndSetLiveInDefForTree(IR * exp, IR const* startir,
                                    IRBB const* startbb, OptCtx const& oc);
    void findAndSetLiveInDefForTree(IR * exp, IRBB const* startbb,
                                    OptCtx const& oc)
    {
        findAndSetLiveInDefForTree(exp, const_cast<IRBB*>(startbb)->getLastIR(),
                                   startbb, oc);
    }

    //Note DOM info must be available.
    //exp: the expression that expected to set livein.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until the
    //         CFG entry.
    //startbb: the BB that begin to do searching.
    void findAndSetLiveInDef(MOD IR * exp, IR const* startir,
                             IRBB const* startbb, OptCtx const& oc);
    void findAndSetLiveInDef(MOD IR * exp, IRBB const* startbb,
                             OptCtx const& oc)
    {
        findAndSetLiveInDef(exp, const_cast<IRBB*>(startbb)->getLastIR(),
                            startbb, oc);
    }

    //Find the VOpnd if 'ir' must OR may referenced 'md'.
    //Return the VMD if found.
    VMD * findMayRef(IR const* ir, MDIdx mdid) const;

    UseDefMgr * getUseDefMgr() { return &m_usedef_mgr; }
    IRCFG * getCFG() const { return m_cfg; }
    IRMgr * getIRMgr() const { return m_irmgr; }

    //Get specific virtual operand.
    VOpnd * getVOpnd(UINT i) const { return m_usedef_mgr.getVOpnd(i); }
    VMD * getVMD(UINT i) const { return (VMD*)getVOpnd(i); }
    virtual CHAR const* getPassName() const { return "MDSSA Manager"; }
    PASS_TYPE getPassType() const { return PASS_MDSSA_MGR; }

    //Get the BB for given expression.
    static IRBB * getExpBB(IR const* ir)
    { return ir->is_id() ? ID_phi(ir)->getBB() : ir->getStmt()->getBB(); }

    //Get MDSSAInfo if exist.
    static MDSSAInfo * getMDSSAInfoIfAny(IR const* ir)
    { return hasMDSSAInfo(ir) ? UseDefMgr::getMDSSAInfo(ir) : nullptr; }

    //Get PhiList if any.
    MDPhiList * getPhiList(UINT bbid) const
    { return m_usedef_mgr.getBBPhiList(bbid); }
    MDPhiList * getPhiList(IRBB const* bb) const
    { return getPhiList(bb->id()); }

    //Gen PhiList for given bbid.
    MDPhiList * genPhiList(UINT bbid)
    { return m_usedef_mgr.genBBPhiList(bbid); }
    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_sbs_mgr; }

    //Generate MDSSAInfo and generate VMD for referrenced MD that both include
    //The function will generate MDSSAInfo for 'exp' according to the refinfo.
    //that defined inside li. The new info for 'exp' will be VMD that defined
    //outside of li or the initial version of VMD.
    void genMDSSAInfoToOutsideLoopDef(IR * exp,
                                      MDSSAInfo const* refinfo,
                                      LI<IRBB> const* li);

    //Generate MDSSAInfo and generate VOpnd for referrenced MD that both include
    //must-ref MD and may-ref MDs.
    MDSSAInfo * genMDSSAInfoAndVOpnd(IR * ir, UINT version);

    //Generate MDSSAInfo and generate VOpnd for referrenced MD that both
    //include must-ref MD and may-ref MDs.
    //ir: must be stmt.
    MDSSAInfo * genMDSSAInfoAndNewVesionVMD(IR * ir);

    //The function is a wrapper of UseDefMgr's function.
    MDSSAInfo * genMDSSAInfo(MOD IR * ir)
    { return getUseDefMgr()->genMDSSAInfo(ir); }

    //The function will generate new version which is used to idenify MD.
    UINT genNewVersion(UINT mdid)
    {
        UINT newversion = m_max_version.get(mdid) + 1;
        m_max_version.set(mdid, newversion);
        return newversion;
    }
    MDPhi * genMDPhi(MDIdx mdid, UINT num_opnd, IRBB * bb, VMD * result);
    MDPhi * genMDPhi(MDIdx mdid, IR * opnd_list, IRBB * bb, VMD * result);
    MDPhi * genMDPhi(MDIdx mdid, IR * opnd_list, IRBB * bb)
    {
        VMD * result = genNewVersionVMD(mdid);
        MDPhi * phi = genMDPhi(mdid, opnd_list, bb, result);
        VMD_def(result) = phi;
        return phi;
    }
    MDPhi * genMDPhi(MDIdx mdid, UINT num_opnd, IRBB * bb)
    {
        VMD * result = genNewVersionVMD(mdid);
        MDPhi * phi  = genMDPhi(mdid, num_opnd, bb, result);
        VMD_def(result) = phi;
        return phi;
    }

    //The function generates new VMD with initial version.
    VMD * genInitVersionVMD(UINT mdid)
    { return genVMD(mdid, MDSSA_INIT_VERSION); }

    //The function generates new VMD with latest version.
    VMD * genNewVersionVMD(UINT mdid)
    { return genVMD(mdid, genNewVersion(mdid)); }

    //Return true if ir might have MDSSAInfo.
    static bool hasMDSSAInfo(IR const* ir)
    { return ir->isMemRefNonPR() || ir->isCallStmt(); }

    //Return true if exist USE to 'ir'.
    //The is a helper function to provid simple query, an example to
    //show how to retrieval VOpnd and USE occurences as well.
    //ir: stmt
    bool hasUse(IR const* ir) const;

    //Return true if bb has PHI.
    bool hasPhi(UINT bbid) const
    {
        return getPhiList(bbid) != nullptr &&
               getPhiList(bbid)->get_elem_count() > 0;
    }
    bool hasPhi(IRBB const* bb) const { return hasPhi(bb->id()); }

    //Return true if the value of ir1 and ir2 are definitely same, otherwise
    //return false to indicate unknown.
    static bool hasSameValue(IR const* ir1, IR const* ir2);

    //Insert a new PHI into bb according to given MDIdx.
    //Note the operand and PHI's result will be initialized in initial-version.
    MDPhi * insertPhi(UINT mdid, IN IRBB * bb, UINT num_opnd);

    //Insert a new PHI into bb according to given MDIdx.
    //Note the operand will be initialized in initial-version, whereas
    //the PHI's result will be initialized in latest-version.
    MDPhi * insertPhiWithNewVersion(UINT mdid, IN IRBB * bb, UINT num_opnd);

    //The function insert def1 in front of def2 in DefDef chain.
    //Note def1 should dominate def2.
    void insertDefBefore(MDDef * def1, MOD MDDef * def2);
    void insertDefBefore(VMD const* def1, VMD const* def2)
    { insertDefBefore(def1->getDef(), def2->getDef()); }

    //Iterative access MDDef chain.
    //The funtion initialize the iterator.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //PHI.
    //def: the beginning MDDef of the chain.
    //it: iterator. It should be clean already.
    //Readonly function.
    MDDef const* iterDefInitC(MDDef const* def, OUT ConstMDDefIter & it) const;

    //Iterative access MDDef chain.
    //The function return the next MDDef node accroding to 'it'.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //it: iterator.
    //Readonly function.
    MDDef const* iterDefNextC(MOD ConstMDDefIter & it) const;

    //Iterative access USE in MDSSAInfo. The USE always an IR occurrence that
    //describes a memory expression.
    //The funtion initialize the iterator.
    //def: the MDDef of the chain.
    //it: iterator. It should be clean already.
    //Readonly function.
    //Note the function may iterate same IR multiple times because it may
    //belong different VOpnd.
    //e.g: global int g; local int b;
    //     g = b;
    //The MDSSA info of ST is:
    // st:i32 'g'
    //  --DEFREF:(MD2V2, PrevDEF:MD2V1, NextDEF : MD2V3) | UsedBy : ld b(id:15)
    //  --DEFREF : (MD5V2, PrevDEF:MD5V1) | UsedBy : ld b(id:15), id(id:23)
    //  ld b is both USE of VOpnd(MD2V2) and VOpnd(MD5V2).
    IR const* iterUseInitC(IR const* def, OUT ConstMDSSAUSEIRIter & it) const;

    //Iterative access USE in MDSSAInfo. The USE always an IR occurrence that
    //describes a memory expression.
    //The function return the next USE accroding to 'it'.
    //it: iterator.
    //Readonly function.
    //Note the function may iterate same IR multiple times because it may
    //belong different VOpnd.
    //e.g: global int g; local int b;
    //     g = b;
    //The MDSSA info of ST is:
    // st:i32 'g'
    //  --DEFREF:(MD2V2, PrevDEF:MD2V1, NextDEF : MD2V3) | UsedBy : ld b(id:15)
    //  --DEFREF : (MD5V2, PrevDEF:MD5V1) | UsedBy : ld b(id:15), id(id:23)
    //  ld b is both USE of VOpnd(MD2V2) and VOpnd(MD5V2).
    IR const* iterUseNextC(MOD ConstMDSSAUSEIRIter & it) const;

    //Iterative access MDDef chain.
    //The funtion initialize the iterator.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //def: the beginning MDDef of the chain.
    //use: indicate the USE expression of the 'def'.
    //it: iterator. It should be clean already.
    //Readonly function.
    MDDef const* iterDefInitCTillKillingDef(MDDef const* def, IR const* use,
                                            OUT ConstMDDefIter & it) const;

    //Iterative access MDDef chain.
    //The function return the next MDDef node accroding to 'it'.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //it: iterator.
    //use: indicate the USE expression of the 'def'.
    //Readonly function.
    MDDef const* iterDefNextCTillKillingDef(IR const* use,
                                            MOD ConstMDDefIter & it) const;

    //Return true if ir dominates all its USE expressions which inside loop.
    //In ssa mode, stmt's USE may be placed in operand list of PHI.
    bool isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const;
    bool isDom(MDDef const* def1, MDDef const* def2) const;

    //Return true if all vopnds of 'def' can reach 'exp'.
    bool isMustDef(IR const* def, IR const* exp) const;

    //Move PHI from 'from' to 'to'.
    //The function often used in updating PHI when adding new dominater
    //BB to 'to'.
    void movePhi(IRBB * from, IRBB * to);

    //Reinitialize MDSSA manager.
    //The function will clean all informations and recreate them.
    void reinit();

    //Remove MDSSA Use-Def chain.
    //Note the function will remove IR tree from all VOpnds and MDSSAMgr.
    //And do NOT remove stmt from BBIRList before call the function.
    //The function does NOT deal with sibling node of ir.
    //e.g:ir = ...
    //       = ir //S1
    //If S1 will be deleted, ir should be removed from its UseSet in MDSSAInfo.
    //NOTE: If ir is a IR tree, say ild(x, ld(y)), remove ild(x) means
    //ld(y) will be removed too. Meanwhile ld(y)'s MDSSAInfo will be
    //updated as well.
    void removeMDSSAOccForTree(IR const* ir, MDSSAUpdateCtx const& ctx);

    //Remove DEF-USE chain if exist in between 'stmt' and 'exp'.
    //The function will remove 'exp' from occurence set.
    //stmt: IR stmt that is DEF of 'exp'.
    //exp: IR expression to be removed.
    void removeDUChain(IR const* stmt, IR const* exp);

    //Remove all virtual USEs of 'stmt'.
    //stmt' will not have any USE expression when function returned.
    void removeAllUse(IR const* stmt, MDSSAUpdateCtx const& ctx);

    //Remove all MDSSAInfo of 'stmt' from MDSSAMgr.
    //The MDSSAInfo includes Def and UseSet info.
    //Note this function only handle stmt's MDSSAInfo, thus it will not
    //process its RHS expression.
    void removeStmtMDSSAInfo(IR const* stmt, MDSSAUpdateCtx const& ctx);

    //Remove given IR expression from UseSet of each vopnd in MDSSAInfo.
    //Note current MDSSAInfo is the SSA info of 'exp', the VOpndSet will be
    //emtpy when exp is removed from all VOpnd's useset.
    //exp: IR expression to be removed.
    //NOTE: the function only process exp itself.
    void removeExpFromAllVOpnd(IR const* exp);

    //Remove Use-Def chain.
    //exp: the expression to be removed.
    //e.g: ir = ...
    //    = ir //S1
    //If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
    //NOTE: the function only process exp itself.
    void removeUse(IR const* exp);

    //The function remove 'phi' out from MDSSA system.
    //It will cut off DU chain of phi's operands, and the DU chain of phi
    //itself as well, then free all resource.
    //phi: to be removed.
    //prev: previous DEF that is used to maintain DefDef chain, and it can be
    //      NULL if there is no previous DEF.
    void removePhiFromMDSSAMgr(MDPhi * phi, MDDef * prev,
                               MDSSAUpdateCtx const& ctx);

    //The function remove all MDPhis in 'bb'.
    //Note caller should guarrantee phi is useless and removable.
    void removePhiList(IRBB * bb, MDSSAUpdateCtx const& ctx);

    //Remove MDDef from DefDef chain.
    //e.g:D1<->D2
    //     |<->D3
    //     |   |<->D5
    //     |   |<->D6
    //     |<->D4
    //  where predecessor of D3 is D1, successors of D3 are D5, D6
    //  After remove D3:
    //e.g:D1<->D2
    //    D1<->D5
    //    D1<->D6
    //    D1<->D4
    //    D3<->nullptr
    //  where predecessor of D5, D6 is D1, successor of D1 includes D5, D6.
    void removeDefFromDDChain(MDDef * mddef);
    void removePhiFromDDChain(MDPhi * phi, MDDef * prev);
    bool removeRedundantPhi(IRBB const* bb, OptCtx const& oc)
    { return prunePhiForBB(bb, nullptr, oc); }

    //Return true if any PHI was removed.
    bool removeRedundantPhi(OptCtx const& oc);

    //Before removing bb or change bb successor,
    //you need remove the related PHI operand if BB 'succ' has PHI stmt.
    void removeSuccessorDesignatedPhiOpnd(IRBB const* succ, UINT pos,
                                          MDSSAUpdateCtx const& ctx);

    //Check each USE of stmt, remove the expired expression which is not
    //reference the memory any more that stmt defined.
    //Return true if DU changed.
    bool removeExpiredDU(IR const* ir);

    //Remove DU chain from 'def' to 'exp'.
    //The function will add VOpnd of phi to 'exp'.
    void removeDUChain(MDDef const* def, IR * exp);

    //The function handle the DU chain and cut off the DU chain between MDPHI
    //and its USE expression.
    //Remove 'phi' from its use's vopnd-list.
    //e.g:u1, u2 are its use expressions.
    //cut off the DU chain between def->u1 and def->u2.
    void removeDefFromUseSet(MDPhi const* phi, MDSSAUpdateCtx const& ctx);
    void recomputeDUAndDDChain(MDPhi const* phi, DomTree const& domtree);
    void recomputeDUAndDDChain(MDPhiList const* philist,
                               DomTree const& domtree);
    void recomputeDUAndDDChain(MOD IR * stmt, DomTree const& domtree,
                               OptCtx const& oc);
    void recomputeDUAndDDChain(List<IR*> const& irlist, DomTree const& domtree,
                               OptCtx const& oc);
    void recomputeDefForOpnd(MDPhi const* phi, OptCtx const& oc);
    void recomputeDefForOpnd(MDPhiList const* philist, OptCtx const& oc);
    //irit: for local used.
    void recomputeDefForRHS(IR const* stmt, IRIter & it, OptCtx const& oc);

    //The function will attempt to remove the USE that located in outside loop.
    //Note the function will NOT cross MDPHI.
    bool tryRemoveOutsideLoopUse(MDDef * def, LI<IRBB> const* li);

    bool verifyDUChainAndOcc() const;
    bool verifyDDChain() const;

    //The function verify the operand and VMD info for MDPhi.
    //NOTE: some pass, e.g:DCE, will remove MDPhi step by step, thus
    //do NOT invoke the function during the removing.
    bool verifyPhi() const;

    //Note the verification is relatively slow.
    bool verifyVersion(OptCtx const& oc) const;
    bool verifyVMD(VMD const* vmd, BitSet * defset = nullptr) const;
    bool verify() const;
    static bool verifyMDSSAInfo(Region const* rg, OptCtx const& oc);

    virtual bool perform(OptCtx & oc) { construction(oc); return true; }
};

} //namespace xoc
#endif
