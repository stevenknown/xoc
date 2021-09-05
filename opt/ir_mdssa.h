/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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

typedef xcom::Vector<xcom::TMap<UINT, VMD*>*> BB2VMDMap;
typedef xcom::TMap<UINT, MDPhiList*> BB2MDPhiList;
typedef xcom::TMapIter<UINT, MDPhiList*> BB2MDPhiListIter;
typedef xcom::List<MDDef*> MDDefIter;
typedef TTab<UINT> LiveInMDTab;
typedef TTabIter<UINT> LiveInMDTabIter;

//This class define the iterator that used to iterate IR occurrence for
//each VOpnd in MDSSA mode.
class ConstMDSSAUSEIRIter {
    COPY_CONSTRUCTOR(ConstMDSSAUSEIRIter);
public:
    ConstMDSSAUSEIRIter() : vopndset_iter(nullptr), current_pos_in_vopndset(-1),
        useset_iter(nullptr), current_pos_in_useset(-1),
        current_useset(nullptr) {}

    VOpndSet * vopndset;
    VOpndSetIter vopndset_iter;
    INT current_pos_in_vopndset;
    IRSetIter useset_iter;
    INT current_pos_in_useset;
    IRSet const* current_useset;

    void clean()
    {
        vopndset_iter = nullptr;
        current_pos_in_vopndset = -1;
        useset_iter = nullptr;
        current_pos_in_useset = -1;
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

//This class construct MDSSA form and manage the MDSSA information for
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
    COPY_CONSTRUCTOR(MDSSAMgr);
protected:
    BYTE m_is_valid:1;
    BYTE m_is_semi_pruned:1;
    Region * m_rg;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    xcom::DefSegMgr * m_seg_mgr;
    IRIter m_iter; //for tmp use.

    //Record version stack during renaming.
    UINT2VMDStack m_map_md2stack;

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

    //NOTE this function only be called at constructor.
    void cleanInConstructor()
    {
        m_is_valid = false;
        m_is_semi_pruned = true;
    }
    void cleanIRSSAInfo(IRBB * bb);
    void cleanMDSSAInfoAI();
    void cutoffDefChain(MDDef * def);
    void cleanMD2Stack();
    //The function destroy data structures that allocated during SSA
    //construction, and these data structures are only useful in construction.
    void cleanLocalUsedData();
    void collectDefinedMDAndInitVMD(IN IRBB * bb, OUT DefSBitSet & mustdef_pr);
    void collectUseMD(IR const* ir, OUT LiveInMDTab & livein_md);
    //maydef_md: record MDs that defined in 'bb'.
    void computeLiveInMD(IRBB const* bb, OUT LiveInMDTab & livein_md);

    void freeBBPhiList(IRBB * bb);
    void freePhiList();

    void destructBBSSAInfo(IRBB * bb);
    void destructionInDomTreeOrder(IRBB * root, xcom::Graph & domtree);
    //The function dump all possible DEF of 'vopnd' by walking through the
    //Def Chain.
    void dumpDefByWalkDefChain(List<MDDef const*> & wl, IRSet & visited,
                               VMD const* vopnd) const;
    void dumpExpDUChainIter(IR const* ir, List<IR*> & lst,
                            List<IR*> & opnd_lst,
                            OUT bool * parting_line) const;
    void dumpDUChainForStmt(IR const* ir, bool & parting_line) const;
    void dumpDUChainForIR(IR const* ir,
                          xcom::List<IR*> & lst,
                          xcom::List<IR*> & opnd_lst) const;
    void dumpBBRef(IN IRBB * bb, UINT indent);
    bool doOpndHaveValidDef(MDPhi const* phi) const;
    bool doOpndHaveSameDef(MDPhi const* phi, OUT VMD ** common_def) const;

    void handleBBRename(IRBB * bb, xcom::DefSBitSet const& effect_mds,
                        IN DefSBitSet & defed_mds,
                        MOD BB2VMDMap & bb2vmdmap);

    void init()
    {
        if (m_usedef_mgr.m_mdssainfo_pool != nullptr) { return; }
        m_is_valid = false;
    }
    void initVMD(IN IR * ir, OUT DefSBitSet & maydef_md);
    void insertPhi(UINT mdid, IN IRBB * bb);

    xcom::Stack<VMD*> * mapMD2VMDStack(UINT mdid);

    void renamePhiResult(IN IRBB * bb);
    void renameUse(IR * ir);
    void renameDef(IR * ir, IRBB * bb);
    void rename(DefSBitSet const& effect_mds,
                Vector<DefSBitSet*> & defed_mds_vec,
                xcom::Graph & domtree);
    void renameBB(IRBB * bb);
    void renameInDomTreeOrder(xcom::DefSBitSet const& effect_mds,
                              IRBB * root, xcom::Graph & dtree,
                              Vector<DefSBitSet*> & defed_mds_vec);
    //The function remove 'vopnd' from MDSSAInfo for each ir in the 'irset'.
    void removeVOpndFromUseSet(VOpnd const* vopnd, IRSet const* irset);
    //The function remove and clean all information of 'vmd' from MDSSAMgr.
    void removeVMD(VMD * vmd) { getUseDefMgr()->removeVMD(vmd); }

    //wl: is an optional parameter to record BB which expected to deal with.
    //    It is a work-list that is used to drive iterative collection and
    //    elimination of redundant PHI elmination.
    bool removePhiHasNoValidDef(List<IRBB*> * wl,MDPhi * phi);

    //wl: is an optional parameter to record BB which expected to deal with.
    //    It is a work-list that is used to drive iterative collection and
    //    elimination of redundant PHI elmination.
    bool removePhiHasCommonDef(List<IRBB*> * wl, MDPhi * phi);

    //Remove PHI that without any USE.
    //Return true if any PHI was removed, otherwise return false.
    bool removePhiNoUse(MDPhi * phi);

    //Record all modified MDs which will be versioned later.
    void recordEffectMD(IRBB const* bb, OUT DefSBitSet & effect_md);
    void removeDefFromDDChainHelper(MDDef * mddef, MDDef * prev);

    bool prunePhi(List<IRBB*> & wl);
    bool prunePhiForBB(IRBB const* bb, List<IRBB*> * wl);

    //Insert phi for VMD.
    //defbbs: record BBs which defined the VMD identified by 'mdid'.
    //visited: record visited BB id
    void placePhiForMD(UINT mdid, List<IRBB*> const* defbbs,
                       DfMgr const& dfm, xcom::BitSet & visited,
                       List<IRBB*> & wl,
                       Vector<DefSBitSet*> & defmds_vec);
    void placePhi(DfMgr const& dfm, MOD DefSBitSet & effect_md,
                  DefMiscBitSetMgr & bs_mgr,
                  Vector<DefSBitSet*> & defined_md_vec,
                  List<IRBB*> & wl);

    //Union successors in NextSet from 'from' to 'to'.
    void unionSuccessors(MDDef const* from, MDDef const* to);

    bool verifyMDSSAInfoUniqueness() const;
    void verifyDef(MDDef const* def, VMD const* vopnd) const;
    //Check SSA uses.
    void verifyUseSet(VMD const* vopnd) const;
    void verifyMDSSAInfoForIR(IR const* ir) const;
public:
    explicit MDSSAMgr(Region * rg) :
        m_sbs_mgr(rg->getMiscBitSetMgr()),
        m_seg_mgr(rg->getMiscBitSetMgr()->getSegMgr()),
        m_usedef_mgr(rg, this)
    {
        cleanInConstructor();
        ASSERT0(rg);
        m_rg = rg;
        m_tm = rg->getTypeMgr();
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

    void addStmtToMDSSAMgr(IR * ir, IR const* ref);
    //Add occurence to each vopnd in mdssainfo.
    //ir: occurence to be added.
    //ref: the reference that is isomorphic to 'ir'.
    //     It is used to retrieve MDSSAInfo.
    void addMDSSAOcc(IR * ir, IR const* ref);
    //After adding BB or change BB successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    void addSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ);

    //Construction of MDSSA form.
    //Note: Non-SSA DU Chains will be maintained after construction.
    void construction(OptCtx & oc);
    //Construction of MDSSA form.
    bool construction(DomTree & domtree);
    size_t count_mem() const;

    //DU chain operation.
    //Change Def stmt from 'olddef' to 'newdef'.
    //olddef: source stmt.
    //newdef: target stmt.
    //e.g: given oldef->USE, change to newdef->USE.
    void changeDef(IR * olddef, IR * newdef);

    //DU chain operation.
    //Change Use expression from 'olduse' to 'newuse'.
    //olduse: source expression.
    //newuse: target expression.
    //e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
    void changeUse(IR * olduse, IR * newuse);

    //Replace oldir with newir.
    void changeIR(IR * oldir, IR * newir);

    //Coalesce version of MD from 'src' to 'tgt'.
    //This function replace definitiond of USE of src to tgt's defintion.
    //e.g: p0=...
    //     p1=p0
    //     ...=p1
    //=> after coalescing, p1 is src, p0 is tgt
    //     p0=...
    //     ------ //removed
    //     ...=p0
    void coalesceDUChain(IR const* src, IR const* tgt);
    void cleanMDSSAInfo(IR * ir) { getUseDefMgr()->cleanMDSSAInfo(ir); }

    //The function copy MDSSAInfo from 'src' to ir. Then add ir as an USE of the
    //new MDSSAInfo.
    void copyAndAddMDSSAOcc(IR * ir, MDSSAInfo const* src);
    void copyMDSSAInfo(IR * tgt, IR const* src);

    //Destroy memory in MDSSAMgr.
    void destroy();
    //Destruction of MDSSA.
    void destruction(DomTree & domtree);
    //Destruction of MDSSA.
    void destruction(OptCtx * oc);
    //Dump Phi List.
    void dumpPhiList(MDPhiList const* philist) const;
    //Dump MDSSA reference info.
    virtual bool dump() const;
    //Dump MDSSA DU chain.
    void dumpDUChain() const;
    //Dump MDSSA VOpnd reference.
    void dumpVOpndRef() const;
    //This function dumps VMD structure and SSA DU info.
    void dumpAllVMD();
    //Dump VMD info into given buffer.
    //Note the buffer have to at least 128 bytes.
    CHAR * dumpVMD(IN VMD * v, OUT CHAR * buf);
    //Dump MDSSA DU stmt graph.
    void dumpSSAGraph(CHAR * name = nullptr);
    //Dump IR tree and MDSSAInfo if any.
    void dumpIRWithMDSSA(IR const* ir, UINT flag = IR_DUMP_COMBINE) const;

    //Find killing must-def for expression ir.
    MDDef * findKillingDef(IR const* ir) const;
    //Find nearest virtual DEF in VOpndSet of 'ir'.
    MDDef * findNearestDef(IR const* ir) const;

    Region * getRegion() const { return m_rg; }
    UseDefMgr * getUseDefMgr() { return &m_usedef_mgr; }

    //Get specific virtual operand.
    VOpnd * getVOpnd(UINT i) const { return m_usedef_mgr.getVOpnd(i); }
    virtual CHAR const* getPassName() const { return "MDSSA Manager"; }
    PASS_TYPE getPassType() const { return PASS_MD_SSA_MGR; }

    //Get MDSSAInfo if exist.
    static MDSSAInfo * getMDSSAInfoIfAny(IR const* ir)
    { return hasMDSSAInfo(ir) ? UseDefMgr::getMDSSAInfo(ir) : nullptr; }
    MDPhiList * getPhiList(IRBB const* bb) const
    { return m_usedef_mgr.getBBPhiList(bb->id()); }
    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_sbs_mgr; }

    //Generate both MDSSAInfo and VOpnd for related MD.
    MDSSAInfo * genMDSSAInfoAndVOpnd(IR * ir, UINT version);
    MDSSAInfo * genMDSSAInfo(IR * ir)
    { return getUseDefMgr()->genMDSSAInfo(ir); }
    //The function will generate new version which is used to idenify MD.
    UINT genNewVersion(UINT mdid)
    {
        UINT newversion = m_max_version.get(mdid) + 1;
        m_max_version.set(mdid, newversion);
        return newversion;
    }
    //The function generates new VMD with initial version.
    VMD * genInitVersionVMD(UINT mdid)
    { return getUseDefMgr()->allocVMD(mdid, MDSSA_INIT_VERSION); }
    //Return true if ir might have MDSSAInfo.
    static bool hasMDSSAInfo(IR const* ir)
    { return ir->isMemoryRefNonPR() || ir->isCallStmt(); }
    //Return true if exist USE to 'ir'.
    //This is a helper function to provid simple query, an example to
    //show how to retrieval VOpnd and USE occurences as well.
    //ir: stmt
    bool hasUse(IR const* ir) const;

    //Return true if bb has PHI.
    bool hasPhi(IRBB const* bb) const
    {
        return getPhiList(bb) != nullptr &&
               getPhiList(bb)->get_elem_count() > 0;
    }

    //Return true if MDSSA is constructed.
    //This flag will direct the behavior of optimizations.
    //If MDSSA constructed, DU mananger should not compute any information.
    bool is_valid() const { return m_is_valid; }

    //Iterative access MDDef chain.
    //This funtion initialize the iterator.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //PHI.
    //def: the beginning MDDef of the chain.
    //it: iterator. It should be clean already.
    //Readonly function.
    MDDef const* iterDefInitC(MDDef const* def, OUT ConstMDDefIter & it) const;

    //Iterative access MDDef chain.
    //This function return the next MDDef node accroding to 'it'.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //it: iterator.
    //Readonly function.
    MDDef const* iterDefNextC(MOD ConstMDDefIter & it) const;

    //Iterative access USE in MDSSAInfo. The USE always an IR occurrence that
    //describes a memory expression.
    //This funtion initialize the iterator.
    //def: the MDDef of the chain.
    //it: iterator. It should be clean already.
    //Readonly function.
    //Note this function may iterate same IR multiple times because it may
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
    //This function return the next USE accroding to 'it'.
    //it: iterator.
    //Readonly function.
    //Note this function may iterate same IR multiple times because it may
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
    //This funtion initialize the iterator.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //def: the beginning MDDef of the chain.
    //use: indicate the USE expression of the 'def'.
    //it: iterator. It should be clean already.
    //Readonly function.
    MDDef const* iterDefInitCTillKillingDef(MDDef const* def, IR const* use,
                                            OUT ConstMDDefIter & it) const;

    //Iterative access MDDef chain.
    //This function return the next MDDef node accroding to 'it'.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //it: iterator.
    //use: indicate the USE expression of the 'def'.
    //Readonly function.
    MDDef const* iterDefNextCTillKillingDef(IR const* use,
                                            MOD ConstMDDefIter & it) const;

    //Return true if stmt dominate use's stmt, otherwise return false.
    bool isStmtDomUseInsideLoop(IR const* stmt, IR const* use,
                                LI<IRBB> const* li) const;

    //Return true if ir dominates all its USE expressions which inside loop.
    //In ssa mode, stmt's USE may be placed in operand list of PHI.
    bool isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const;

    //Move PHI from 'from' to 'to'.
    //This function often used in updating PHI when adding new dominater
    //BB to 'to'.
    void movePhi(IRBB * from, IRBB * to);

    //Reinitialize MDSSA manager.
    //This function will clean all informations and recreate them.
    void reinit();

    //Remove MDSSA Use-Def chain.
    //e.g: ir = ...
    //    = ir //S1
    //If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
    //NOTE: If ir is a IR tree, e.g: ild(x, ld(y)), remove ild(x) means
    //ld(y) will be removed as well. And ld(y)'s MDSSAInfo will be
    //updated as well.
    void removeMDSSAOcc(IR * ir);

    //Remove DEF-USE chain if exist in between 'stmt' and 'exp'.
    //This function will remove 'exp' from occurence set.
    //stmt: IR stmt that is DEF of 'exp'.
    //exp: IR expression to be removed.
    void removeDUChain(IR const* stmt, IR const* exp);

    //Remove all virtual USEs of 'stmt'.
    //stmt' will have not any USE expression when function returned.
    void removeAllUse(IR const* stmt);

    //Remove all MDSSAInfo of 'stmt' from MDSSAMgr.
    void removeStmtFromMDSSAMgr(IR const* stmt);

    //This function remove 'phi' out from MDSSA system.
    //It will cut off DU chain of phi's operands, and the DU chain of phi
    //itself as well, then free all resource.
    //phi: to be removed.
    //prev: previous DEF that is used to maintain Def-Def chain, and it can be
    //      NULL if there is no previous DEF.
    void removePhiFromMDSSAMgr(MDPhi * phi, MDDef * prev);

    //Remove MDDef from Def-Def chain.
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
    bool removeRedundantPhi(IRBB const* bb)
    { return prunePhiForBB(bb, nullptr); }
    //Return true if any PHI was removed.
    bool removeRedundantPhi();

    //Before removing bb or change bb successor,
    //you need remove the related PHI operand if BB successor has PHI stmt.
    void removeSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ);

    //Check each USE of stmt, remove the expired expression which is not
    //reference the memory any more that stmt defined.
    //Return true if DU changed.
    bool removeExpiredDU(IR * ir);

    bool verifyDUChainAndOcc() const;
    bool verifyDDChain() const;
    //The function verify the operand and VMD info for MDPhi.
    //NOTE: some pass, e.g:DCE, will remove MDPhi step by step, thus
    //do NOT invoke the function during the removing.
    bool verifyPhi() const;
    bool verifyVMD() const;
    bool verify() const;
    static bool verifyMDSSAInfo(Region const* rg);

    virtual bool perform(OptCtx & oc) { construction(oc); return true; }
};

} //namespace xoc
#endif
