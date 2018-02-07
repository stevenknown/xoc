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

typedef Vector<TMap<UINT, VMD*>*> BB2VMDMap;
typedef TMap<UINT, MDPhiList*> BB2MDPhiList;
typedef TMapIter<UINT, MDPhiList*> BB2MDPhiListIter;

class MDSSAMgr : public Pass {
protected:
    Region * m_ru;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    IR_CFG * m_cfg;
    DefSegMgr * m_seg_mgr;
    DefMiscBitSetMgr * m_sbs_mgr;
    bool m_is_ssa_constructed;

    IRIter m_iter; //for tmp use.

    //Record version stack during renaming.
    UINT2VMDStack m_map_md2stack;

    //record version number counter for pr.
    xcom::Vector<UINT> m_max_version;

    UseDefMgr m_usedef_mgr;
protected:
    void init()
    {
        if (m_usedef_mgr.m_mdssainfo_pool != NULL) { return; }
        m_is_ssa_constructed = false;
    }

    //NOTE this function only be called at constructor.
    void cleanInConstructor()
    {
        m_ru = NULL;
        m_tm = NULL;
        m_seg_mgr = NULL;
        m_sbs_mgr = NULL;
        m_cfg = NULL;
        m_is_ssa_constructed = false;
    }
    void cleanMDSSAInfoAI();

    void addDefChain(MDDef * def1, MDDef * def2);

    void cutoffDefChain(MDDef * def);
    void cleanMD2Stack();
    void collectDefinedMD(IN IRBB * bb, OUT DefSBitSet & mustdef_pr);

    void destructBBSSAInfo(IRBB * bb);
    void destructionInDomTreeOrder(IRBB * root, Graph & domtree);
    void dumpExpDUChainIter(
            IR const* ir,
            List<IR const*> & lst,
            List<IR const*> & opnd_lst,
            OUT bool * parting_line);

    void freePhiList();

    void handleBBRename(
            IRBB * bb,
            IN DefSBitSet & defed_prs,
            IN OUT BB2VMDMap & bb2vmdmap);

    Stack<VMD*> * mapMD2VMDStack(UINT mdid);

    void renamePhiResult(IN IRBB * bb);
    void renameUse(IR * ir);
    void renameDef(IR * ir, IRBB * bb);
    void rename(DefSBitSet & effect_prs,
                Vector<DefSBitSet*> & defed_prs_vec,
                Graph & domtree);
    void renameBB(IRBB * bb);
    void renameInDomTreeOrder(
            IRBB * root,
            Graph & dtree,
            Vector<DefSBitSet*> & defed_prs_vec);

    void stripPhi(MDPhi * phi);

    void prunePhi(List<IRBB*> & wl);
    void prunePhiForBB(List<IRBB*> & wl, IRBB * bb);
    void placePhiForMD(
            UINT mdid,
            IN List<IRBB*> * defbbs,
            DfMgr const& dfm,
            BitSet & visited,
            List<IRBB*> & wl,
            Vector<DefSBitSet*> & defmds_vec);
    void placePhi(DfMgr const& dfm,
                  IN OUT DefSBitSet & effect_md,
                  DefMiscBitSetMgr & bs_mgr,
                  Vector<DefSBitSet*> & defined_md_vec,
                  List<IRBB*> & wl);
    void verifySSAInfo(IR const* ir);
public:
    explicit MDSSAMgr(Region * rg) : m_usedef_mgr(rg)
    {
        cleanInConstructor();
        ASSERT0(rg);
        m_ru = rg;

        m_tm = rg->getTypeMgr();
        ASSERT0(m_tm);

        ASSERT0(rg->getMiscBitSetMgr());
        m_sbs_mgr = rg->getMiscBitSetMgr();
        m_seg_mgr = rg->getMiscBitSetMgr()->getSegMgr();
        ASSERT0(m_seg_mgr);

        m_cfg = rg->getCFG();
        ASSERT(m_cfg, ("cfg is not available."));

        m_md_sys = rg->getMDSystem();
    }
    COPY_CONSTRUCTOR(MDSSAMgr);
    ~MDSSAMgr()
    {
        ASSERT(!isMDSSAConstructed(), ("should be destructed"));
        destroy();
    }

    //Add occurence to each vopnd in mdssainfo.
    void addMDSSAOcc(IR * ir, MDSSAInfo * mdssainfo);

    void buildDUChain(IR * def, IR * use)
    {
        DUMMYUSE(def);
        DUMMYUSE(use);
        UNREACH();
        //ASSERT0(def->isWritePR() || def->isCallHasRetVal());
        //ASSERT0(use->isReadPR());
        //MDSSAInfo * ssainfo = def->getMDSSAInfo();
        //if (ssainfo == NULL) {
        //    ssainfo = allocMDSSAInfo(def->get_prno());
        //    def->setSSAInfo(ssainfo);
        //    SSA_def(ssainfo) = def;

            //You may be set multiple defs for use.
        //    ASSERT(use->getSSAInfo() == NULL, ("use already has SSA info."));

        //    use->setSSAInfo(ssainfo);
        //}

        //SSA_uses(ssainfo).append(use);
    }

    //Note: Non-SSA DU Chains of read/write PR will be clean and
    //unusable after SSA construction.
    void construction(OptCtx & oc);
    bool construction(DomTree & domtree);
    size_t count_mem();
    void changeDef(IR * olddef, IR * newdef);
    void changeUse(IR * olduse, IR * newuse);
    void changeIR(IR * oldir, IR * newir);
    //Coalesce version of MD from 'src' to 'tgt'.
    //This function replace definitiond of USE of src to tgt's defintion.//
    //e.g: p0=...
    //     p1=p0
    //     ...=p1
    //=> after coalescing, p1 is src, p0 is tgt
    //     p0=...
    //     ------ //removed
    //     ...=p0
    void coalesceVersion(IR const* src, IR const* tgt);


    void destroy();
    void destruction(DomTree & domtree);
    void destruction();
    void dump();
    void dumpDUChain();
    void dumpAllVMD();
    CHAR * dumpVMD(IN VMD * v, OUT CHAR * buf);
    void dumpSSAGraph(CHAR * name = NULL);

    //Find killing must-def for expression ir.
    MDDef * findKillingDef(IR const* ir);
    MDDef * findNearestDef(IR const* ir);

    UseDefMgr * getUseDefMgr() { return &m_usedef_mgr; }

    void initVMD(IN IR * ir, OUT DefSBitSet & maydef_md);
    void insertPhi(UINT mdid, IN IRBB * bb);

    //Return true if PR ssa is constructed.
    //This flag will direct the behavior of optimizations.
    //If SSA constructed, DU mananger will not compute any information for PR.
    bool isMDSSAConstructed() const { return m_is_ssa_constructed; }

    //Return true if phi is redundant, otherwise return false.
    //If all opnds have same defintion or defined by current phi,
    //the phi is redundant.
    //common_def: record the common_def if the definition
    //  of all opnd is the same.
    bool isRedundantPHI(MDPhi const* phi, OUT VMD ** common_def) const;

    //Reinitialize MD SSA manager.
    //This function will clean all informations and recreate them.
    void reinit();

    //Remove MD-SSA and PR-SSA use-def chain.
    //e.g: ir=...
    //    =ir //S1
    //If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
    //NOTE: If ir is a IR tree, e.g: ild(x, ld(y)), remove ild(x) means ld(y) will
    //be removed as well. And ld(y)'s MDSSAInfo will be updated as well.
    void removeMDSSAUseRecur(IR * ir);

    bool verifyPhi(bool is_vpinfo_avail);
    bool verifyVMD();
    bool verify();

    virtual CHAR const* getPassName() const
    { return "MD SSA Manager"; }

    PASS_TYPE getPassType() const { return PASS_MD_SSA_MGR; }
};

} //namespace xoc
#endif
