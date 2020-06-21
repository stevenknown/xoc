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
    COPY_CONSTRUCTOR(MDSSAMgr);
protected:
    Region * m_rg;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    xcom::DefSegMgr * m_seg_mgr;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    bool m_is_ssa_constructed;

    IRIter m_iter; //for tmp use.

    //Record version stack during renaming.
    UINT2VMDStack m_map_md2stack;

    //record version number counter for pr.
    xcom::Vector<UINT> m_max_version;

    UseDefMgr m_usedef_mgr;
protected:
    void addDefChain(MDDef * def1, MDDef * def2);

    //NOTE this function only be called at constructor.
    void cleanInConstructor()
    {
        m_rg = NULL;
        m_tm = NULL;
        m_seg_mgr = NULL;
        m_sbs_mgr = NULL;
        m_cfg = NULL;
        m_is_ssa_constructed = false;
    }
    void cleanIRSSAInfo(IRBB * bb);
    void cleanMDSSAInfoAI();
    void cutoffDefChain(MDDef * def);
    void cleanMD2Stack();
    void collectDefinedMD(IN IRBB * bb, OUT DefSBitSet & mustdef_pr);

    void freeBBPhiList(IRBB * bb);
    void freePhiList();
   
    void destructBBSSAInfo(IRBB * bb);
    void destructionInDomTreeOrder(IRBB * root, xcom::Graph & domtree);
    void dumpDefChain(List<MDDef const*> & wl,
                      IRSet & visited,
                      VMD const* vopnd);
    void dumpExpDUChainIter(IR const* ir,
                            List<IR const*> & lst,
                            List<IR const*> & opnd_lst,
                            OUT bool * parting_line);
    void dumpBBRef(IN IRBB * bb, UINT indent);
    bool doOpndHaveValidDef(MDPhi const* phi) const;
    bool doOpndHaveSameDef(MDPhi const* phi, OUT VMD ** common_def) const;

    void handleBBRename(IRBB * bb,
                        IN DefSBitSet & defed_prs,
                        IN OUT BB2VMDMap & bb2vmdmap);

    void init()
    {
        if (m_usedef_mgr.m_mdssainfo_pool != NULL) { return; }
        m_is_ssa_constructed = false;
    }
    void initVMD(IN IR * ir, OUT DefSBitSet & maydef_md);
    void insertPhi(UINT mdid, IN IRBB * bb);

    xcom::Stack<VMD*> * mapMD2VMDStack(UINT mdid);

    void renamePhiResult(IN IRBB * bb);
    void renameUse(IR * ir);
    void renameDef(IR * ir, IRBB * bb);
    void rename(DefSBitSet & effect_prs,
                Vector<DefSBitSet*> & defed_prs_vec,
                xcom::Graph & domtree);
    void renameBB(IRBB * bb);
    void renameInDomTreeOrder(IRBB * root,
                              xcom::Graph & dtree,
                              Vector<DefSBitSet*> & defed_prs_vec);
    bool removePHIHasNoValidDef(List<IRBB*> & wl,
                                MDPhi * phi,
                                MDPhiList * philist);
    bool removePHIHasCommonDef(List<IRBB*> & wl,
                               MDPhi * phi,
                               MDPhiList * philist);
    void removeDefFromDDChainHelper(MDDef const* mddef, MDDef * prev);

    bool prunePhi(List<IRBB*> & wl);
    bool prunePhiForBB(List<IRBB*> & wl, IRBB * bb);
    void placePhiForMD(UINT mdid,
                       IN List<IRBB*> * defbbs,
                       DfMgr const& dfm,
                       xcom::BitSet & visited,
                       List<IRBB*> & wl,
                       Vector<DefSBitSet*> & defmds_vec);
    void placePhi(DfMgr const& dfm,
                  IN OUT DefSBitSet & effect_md,
                  DefMiscBitSetMgr & bs_mgr,
                  Vector<DefSBitSet*> & defined_md_vec,
                  List<IRBB*> & wl);

    //Union successors in NextSet from 'from' to 'to'.
    void unionSuccessors(MDDef const* from, MDDef const* to);

    void verifySSAInfo(IR const* ir);
public:
    explicit MDSSAMgr(Region * rg) : m_usedef_mgr(rg, this)
    {
        cleanInConstructor();
        ASSERT0(rg);
        m_rg = rg;
        m_tm = rg->getTypeMgr();
        ASSERT0(m_tm);
        ASSERT0(rg->getMiscBitSetMgr());
        m_sbs_mgr = rg->getMiscBitSetMgr();
        m_seg_mgr = rg->getMiscBitSetMgr()->getSegMgr();
        ASSERT0(m_seg_mgr);
        m_cfg = rg->getCFG();
        ASSERTN(m_cfg, ("cfg is not available."));
        m_md_sys = rg->getMDSystem();
    }
    ~MDSSAMgr()
    {
        //CAUTION: If you do not finish out-of-SSA prior to destory(),
        //the reference to IR's MDSSA info will lead to undefined behaviors.
        //ASSERTN(!isMDSSAConstructed(), ("should be destructed"));

        destroy();
    }

    //Add occurence to each VOpnd in mdssainfo.
    void addMDSSAOcc(IR * ir, MDSSAInfo * mdssainfo);
    //After adding BB or change BB successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    void addSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ);

    //Construction of MDSSA form.
    //Note: Non-SSA DU Chains will be maintained after construction.
    void construction(OptCtx & oc);
    //Construction of MDSSA form.
    bool construction(DomTree & domtree);
    size_t count_mem();

    //DU chain operation.
    //Change Def stmt from 'olddef' to 'newdef'.
    //'olddef': source stmt.
    //'newdef': target stmt.
    //e.g: oldef->USE change to newdef->USE.
    void changeDef(IR * olddef, IR * newdef);

    //DU chain operation.
    //Change Use expression from 'olduse' to 'newuse'.
    //'olduse': source expression.
    //'newuse': target expression.
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
    void coalesceVersion(IR const* src, IR const* tgt);
    void cleanMDSSAInfo(IR * ir) { getUseDefMgr()->cleanMDSSAInfo(ir); }

    //Destroy memory in MDSSAMgr.
    void destroy();
    //Destruction of MDSSA.
    void destruction(DomTree & domtree);
    //Destruction of MDSSA.
    void destruction(OptCtx * oc);
    //Dump Phi List.
    void dumpPhiList(MDPhiList const* philist) const;
    //Dump MDSSA reference info.
    void dump();
    //Dump MDSSA DU chain.
    void dumpDUChain();
    //This function dumps VMD structure and SSA DU info.
    void dumpAllVMD();
    //Dump VMD info into given buffer.
    //Note the buffer have to at least 128 bytes.
    CHAR * dumpVMD(IN VMD * v, OUT CHAR * buf);
    //Dump MDSSA DU stmt graph.
    void dumpSSAGraph(CHAR * name = NULL);
    //Dump IR tree's MD reference, where ir may be stmt or exp.
    void dumpRef(UINT indent);

    //Find killing must-def for expression ir.
    MDDef * findKillingDef(IR const* ir);
    //Find nearest virtual DEF in VOpndSet of 'ir'.
    MDDef * findNearestDef(IR const* ir);

    Region * getRegion() const { return m_rg; }
    UseDefMgr * getUseDefMgr() { return &m_usedef_mgr; }
    //Get specific virtual operand.
    VOpnd * getVOpnd(UINT i) const { return m_usedef_mgr.getVOpnd(i); }
    virtual CHAR const* getPassName() const { return "MD SSA Manager"; }
    PASS_TYPE getPassType() const { return PASS_MD_SSA_MGR; }
    //Get MDSSAInfo if exist.
    MDSSAInfo * getMDSSAInfoIfAny(IR const* ir)
    { return hasMDSSAInfo(ir) ? getUseDefMgr()->getMDSSAInfo(ir) : NULL; }
    MDPhiList const* getPhiList(IRBB const* bb) const
    { return m_usedef_mgr.getBBPhiList(bb->id()); }
    //Generate MDSSAInfo and generate VOpnd for related MD.
    MDSSAInfo * genMDSSAInfoAndVOpnd(IR * ir, UINT version);

    //Return true if ir might have MDSSAInfo.
    bool hasMDSSAInfo(IR const* ir) const
    { return ir->isMemoryRefNotOperatePR() || ir->isCallStmt(); }

    //Return true if MDSSA is constructed.
    //This flag will direct the behavior of optimizations.
    //If MDSSA constructed, DU mananger should not compute any information.
    bool isMDSSAConstructed() const { return m_is_ssa_constructed; }

    //Iterative access MDDef chain.
    //This funtion initialize the iterator.
    //'def': the beginning MDDef of the chain.
    //'ii': iterator. It should be clean already.
    //Readonly function.
    MDDef const* iterDefInitC(MDDef const* def,
                              OUT ConstMDDefIter & ii);
    //Iterative access MDDef chain.
    //This function return the next MDDef node accroding to 'ii'.
    //'ii': iterator.
    //Readonly function.
    MDDef const* iterDefNextC(IN OUT ConstMDDefIter & ii);

    //Reinitialize MDSSA manager.
    //This function will clean all informations and recreate them.
    void reinit();

    //Remove MD-SSA and PR-SSA use-def chain.
    //e.g: ir=...
    //    =ir //S1
    //If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
    //NOTE: If ir is a IR tree, e.g: ild(x, ld(y)), remove ild(x) means
    //ld(y) will be removed as well. And ld(y)'s MDSSAInfo will be
    //updated as well.
    void removeMDSSAUse(IR * ir);

    //Remove DEF-USE chain if exist in between 'stmt' and 'exp'.
    //This function will remove 'exp' from occurence set.
    //stmt: IR stmt that is DEF of 'exp'.
    //exp: IR expression to be removed.
    void removeDUChain(IR const* stmt, IR const* exp);

    //Remove all virtual USEs of 'stmt'.
    void removeAllUse(IR const* stmt);

    //Remove all MDSSAInfo of 'stmt' from MDSSAMgr.
    void removeStmtFromMDSSAMgr(IR const* stmt);

    //Remove MDDef from Def-Def chain.
    //e.g:D1<->D2
    //     |<->D3
    //     |   |<->D5
    //     |   |<->D6
    //     |->D4
    //  where predecessor of D3 is D1, successors of D3 are D5, D6
    //  After remove D3:
    //e.g:D1<->D2
    //    D1<->D5
    //    D1<->D6
    //    D1<->D4
    //    D3<->NULL
    //  where predecessor of D5, D6 is D1, successor of D1 includes D5, D6.
    void removeDefFromDDChain(MDDef const* mddef);
    void removePHIFromDDChain(MDPhi const* phi, MDDef * prev);

    //Before removing bb or change bb successor,
    //you need remove the related PHI operand if BB successor has PHI stmt.
    void removeSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ);

    bool verifyDDChain();
    bool verifyPhi(bool is_vpinfo_avail);
    bool verifyVMD();
    bool verify();

    virtual bool perform(OptCtx & oc) { construction(oc); return true; }
    //Remove redundant phi.
    //Return true if there is phi removed.
    bool prunePhi();
};

bool verifyMDSSAInfo(Region * rg);

} //namespace xoc
#endif
