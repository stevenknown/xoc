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

typedef Vector<Vector<VMD*>*> BB2VMD;
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
    
    xcom::Vector<UINT> m_max_version; //record version number counter for pr.

    UseDefMgr m_usedef_mgr;
    BB2MDPhiList m_map_bb2philist; //map from IRBB id to MDPhiList.
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
    
    void cleanMD2Stack();
    void collectDefinedMD(IN IRBB * bb, OUT DefSBitSet & mustdef_pr);

    void destroyMDSSAInfo();
    void destructBBSSAInfo(IRBB * bb);
    void destructionInDomTreeOrder(IRBB * root, Graph & domtree);

    void freePhiList();

    void handleBBRename(
            IRBB * bb,
            IN DefSBitSet & defed_prs,
            IN OUT BB2VMD & bb2vp);

    Stack<VMD*> * mapMD2VMDStack(UINT mdid);
    
    void renamePhiResult(IN IRBB * bb);
    void renameUse(IR * ir);
    void renameDef(IR * ir, IRBB * bb);
    void refinePhi(List<IRBB*> & wl);
    void refinePhiForBB(List<IRBB*> & wl, IRBB * bb);
    void rename(DefSBitSet & effect_prs,
                Vector<DefSBitSet*> & defed_prs_vec,
                Graph & domtree);
    void renameBB(IRBB * bb);
    void renameInDomTreeOrder(
            IRBB * root,
            Graph & dtree,
            Vector<DefSBitSet*> & defed_prs_vec);

    void stripPhi(MDPhi * phi);

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
    void verifySSAInfo(IR * ir);
public:
    explicit MDSSAMgr(Region * ru) : m_usedef_mgr(ru)
    {
        cleanInConstructor();
        ASSERT0(ru);
        m_ru = ru;

        m_tm = ru->getTypeMgr();
        ASSERT0(m_tm);

        ASSERT0(ru->getMiscBitSetMgr());
        m_sbs_mgr = ru->getMiscBitSetMgr();
        m_seg_mgr = ru->getMiscBitSetMgr()->getSegMgr();
        ASSERT0(m_seg_mgr);

        m_cfg = ru->getCFG();
        ASSERT(m_cfg, ("cfg is not available."));

        m_md_sys = ru->getMDSystem();
    }
    COPY_CONSTRUCTOR(MDSSAMgr);
    ~MDSSAMgr() { destroy(false); }

    void buildDUChain(IR * def, IR * use)
    {
        UNUSED(def);
        UNUSED(use);
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
    void construction(DomTree & domtree);
    size_t count_mem();
    
    void destroy(bool is_reinit);
    void destruction(DomTree & domtree);
    void destruction();
    void dump();
    void dumpAllVMD();
    CHAR * dumpVMD(IN VMD * v, OUT CHAR * buf);
    void dumpSSAGraph(CHAR * name = NULL);

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
    
    //Reinitialize SSA manager.
    //This function will clean all informations and recreate them.
    inline void reinit()
    {
        if (isMDSSAConstructed()) {
            destroy(true);
        }
        init();
    }

    bool verifyPhi(bool is_vpinfo_avail);
    bool verifyVMD();
    bool verify();

    virtual CHAR const* getPassName() const
    { return "MD SSA Manager"; }

    PASS_TYPE getPassType() const { return PASS_MD_SSA_MGR; }
};

} //namespace xoc
#endif
