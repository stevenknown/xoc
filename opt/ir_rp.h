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
#ifndef _IR_REG_PROMOTION_H_
#define _IR_REG_PROMOTION_H_

namespace xoc {

class GVN;

//
//START MDLT
//
#define MDLT_id(g) ((g)->id)
#define MDLT_md(g) ((g)->md)
#define MDLT_livebbs(g) ((g)->livebbs)

//This class represents life time of MD.
class MDLT {
public:
    UINT id;
    MD const* md;
    xcom::BitSet * livebbs;
};
//END MDLT

typedef HMap<MD*, MDLT*> MD2MDLifeTime;

class DontPromoteTab : public MDSet {
    COPY_CONSTRUCTOR(DontPromoteTab);
    MDSystem * m_md_sys;
    Region * m_rg;
public:
    explicit DontPromoteTab(Region * rg)
    {
        ASSERT0(rg);
        m_rg = rg;
        m_md_sys = rg->getMDSystem();
    }
    inline bool is_overlap(MD const* md)
    {
        if (MDSet::is_overlap(md, m_rg)) { return true; }
        MDSetIter iter;
        for (INT i = get_first(&iter); i >= 0; i = get_next(i, &iter)) {
            MD const* t = m_md_sys->getMD(i);
            ASSERT0(t);
            if (t->is_overlap(md)) { return true; }
        }
        return false;
    }

    bool dump() const
    {
        if (!m_rg->isLogMgrInit()) { return false; }
        note(m_rg, "\n==---- DUMP Dont Promotion Table ----==\n");
        MDSetIter iter;
        for (INT i = get_first(&iter); i >= 0; i = get_next(i, &iter)) {
            MD const* t = m_md_sys->getMD(i);
            ASSERT0(t);
            t->dump(m_md_sys->getTypeMgr());
        }
        return true;
    }
};


#define RP_UNKNOWN 0
#define RP_SAME_ARRAY 1
#define RP_DIFFERENT_ARRAY 2
#define RP_SAME_OBJ 3
#define RP_DIFFERENT_OBJ 4

//Perform Register Promotion.
//Register Promotion combines multiple memory load of the
//same memory location into one PR.
class RegPromot : public Pass {
    COPY_CONSTRUCTOR(RegPromot);
private:
    MD2MDLifeTime * m_md2lt_map;
    Region * m_rg;
    UINT m_mdlt_count;
    SMemPool * m_pool;
    SMemPool * m_ir_ptr_pool;
    MDSystem * m_md_sys;
    MDSetMgr * m_mds_mgr;
    xcom::DefMiscBitSetMgr * m_misc_bs_mgr;
    IRCFG * m_cfg;
    TypeMgr * m_tm;
    DUMgr * m_du;
    GVN * m_gvn;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    DontPromoteTab m_dont_promote;
    xcom::BitSetMgr m_bs_mgr;
    MDLivenessMgr * m_liveness_mgr;
    bool m_is_insert_bb; //indicate if new bb inserted and cfg changed.
    //WORKAROUND|FIXME:For now, we do not support incremental update PRSSA.
    //rebuild PRSSA if DU changed.
    bool m_need_rebuild_prssa;

private:
    UINT analyzeIndirectAccessStatus(IR const* ref1, IR const* ref2);
    UINT analyzeArrayStatus(IR const* ref1, IR const* ref2);
    void addExactAccess(OUT TMap<MD const*, IR*> & exact_access,
                        OUT List<IR*> & exact_occs,
                        MD const* exact_md,
                        IR * ir);
    void addInexactAccess(TTab<IR*> & inexact_access, IR * ir);
    void addDontPromote(MD const* md);
    void addDontPromote(MDSet const& mds);

    void buildPRDUChain(IR * def, IR * use);

    void checkAndRemoveInvalidExactOcc(List<IR*> & exact_occs);
    void clobberAccess(IR * ir,
                       OUT TMap<MD const*, IR*> & exact_access,
                       OUT List<IR*> & exact_occs,
                       OUT TTab<IR*> & inexact_access);
    bool checkArrayIsLoopInvariant(IN IR * ir, LI<IRBB> const* li);
    bool checkIndirectAccessIsLoopInvariant(IN IR * ir, LI<IRBB> const* li);
    void createDelegateInfo(
        IR * delegate,
        TMap<IR*, IR*> & delegate2pr,
        TMap<IR*, SList<IR*>*> & delegate2has_outside_uses_ir_list);
    void computeOuterDefUse(IR * ref,
                            IR * delegate,
                            TMap<IR*, DUSet*> & delegate2def,
                            TMap<IR*, DUSet*> & delegate2use,
                            DefMiscBitSetMgr * sbs_mgr,
                            LI<IRBB> const* li);

    void dump_mdlt();
    void dumpInexact(TTab<IR*> & access);
    void dumpExact(TMap<MD const*, IR*> & access, List<IR*> & occs);

    bool EvaluableScalarReplacement(List<LI<IRBB> const*> & worklst);

    IRBB * findSingleExitBB(LI<IRBB> const* li);
    void freeLocalStruct(TMap<IR*, DUSet*> & delegate2use,
                         TMap<IR*, DUSet*> & delegate2def,
                         TMap<IR*, IR*> & delegate2pr,
                         DefMiscBitSetMgr * sbs_mgr);

    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_misc_bs_mgr; }
    MDLivenessMgr * getMDLivenessMgr() const { return m_liveness_mgr; }
    MDLT * getMDLifeTime(MD * md);

    void handleAccessInBody(
        IR * ref,
        IR * delegate,
        IR const* delegate_pr,
        TMap<IR*, SList<IR*>*> const& delegate2has_outside_uses_ir_list,
        OUT TTab<IR*> & restore2mem,
        OUT List<IR*> & fixup_list,
        TMap<IR*, IR*> const& delegate2stpr,
        LI<IRBB> const* li,
        IRIter & ii);
    void handleRestore2Mem(
        TTab<IR*> & restore2mem,
        TMap<IR*, IR*> & delegate2stpr,
        TMap<IR*, IR*> & delegate2pr,
        TMap<IR*, DUSet*> & delegate2use,
        TMap<IR*, SList<IR*>*> & delegate2has_outside_uses_ir_list,
        TTabIter<IR*> & ti,
        IRBB * exit_bb);
    void handlePrelog(IR * delegate,
                      IR * pr,
                      TMap<IR*, IR*> & delegate2stpr,
                      TMap<IR*, DUSet*> & delegate2def,
                      IRBB * preheader);
    bool hasLoopOutsideUse(IR const* stmt, LI<IRBB> const* li);
    bool handleArrayRef(IN IR * ir,
                        LI<IRBB> const* li,
                        OUT TMap<MD const*, IR*> & exact_access,
                        OUT List<IR*> & exact_occs,
                        OUT TTab<IR*> & inexact_access);
    bool handleGeneralRef(IR * ir,
                          LI<IRBB> const* li,
                          OUT TMap<MD const*, IR*> & exact_access,
                          OUT List<IR*> & exact_occs,
                          OUT TTab<IR*> & inexact_access);

    bool isMayThrow(IR * ir, IRIter & iter);

    bool mayBeGlobalRef(IR * ref)
    {
        MD const* md = ref->getRefMD();
        if (md != nullptr && md->is_global()) { return true; }

        MDSet const* mds = ref->getRefMDSet();
        if (mds == nullptr) { return false; }

        MDSetIter iter = nullptr;
        for (INT i = mds->get_first(&iter);
             i >= 0; i = mds->get_next(i, &iter)) {
            MD const* md2 = m_md_sys->getMD(i);
            ASSERT0(md2);
            if (md2->is_global()) { return true; }
        }
        return false;
    }

    bool scanOpnd(IR * ir,
                  LI<IRBB> const* li,
                  OUT TMap<MD const*, IR*> & exact_access,
                  OUT List<IR*> & exact_occs,
                  OUT TTab<IR*> & inexact_access,
                  IRIter & ii);
    bool scanResult(IN IR * ir,
                    LI<IRBB> const* li,
                    OUT TMap<MD const*, IR*> & exact_access,
                    OUT List<IR*> & exact_occs,
                    OUT TTab<IR*> & inexact_access);
    bool scanBB(IN IRBB * bb,
                LI<IRBB> const* li,
                OUT TMap<MD const*, IR*> & exact_access,
                OUT TTab<IR*> & inexact_access,
                OUT List<IR*> & exact_occs,
                IRIter & ii);
    bool shouldBePromoted(IR const* occ, List<IR*> & exact_occs);

    bool promoteInexactAccess(LI<IRBB> const* li,
                              IRBB * preheader,
                              IRBB * exit_bb,
                              TTab<IR*> & inexact_access,
                              IRIter & ii, TTabIter<IR*> & ti);
    bool promoteExactAccess(LI<IRBB> const* li,
                            IRIter & ii,
                            TTabIter<IR*> & ti,
                            IRBB * preheader,
                            IRBB * exit_bb,
                            TMap<MD const*, IR*> & cand_list,
                            List<IR*> & occ_list);
    //Return true if there are memory locations have been promoted.
    bool promote(LI<IRBB> const* li,
                 IRBB * exit_bb,
                 IRIter & ii,
                 TTabIter<IR*> & ti,
                 TMap<MD const*, IR*> & exact_access,
                 TTab<IR*> & inexact_access,
                 List<IR*> & exact_occs);

    void removeRedundantDUChain(List<IR*> & fixup_list);
    void replaceUseForTree(IR * oldir, IR * newir);

    bool tryPromote(LI<IRBB> const* li,
                    IRBB * exit_bb,
                    IRIter & ii,
                    TTabIter<IR*> & ti,
                    TMap<MD const*, IR*> & exact_access,
                    TTab<IR*> & inexact_access,
                    List<IR*> & exact_occs);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }

    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool != nullptr);
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p != nullptr);
        ::memset(p, 0, size);
        return p;
    }

public:
    RegPromot(Region * rg) : m_dont_promote(rg)
    {
        ASSERT0(rg != nullptr);
        m_rg = rg;
        m_md_sys = rg->getMDSystem();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_du = rg->getDUMgr();
        m_mds_mgr = rg->getMDSetMgr();
        m_misc_bs_mgr = rg->getMiscBitSetMgr();
        m_gvn = nullptr;
        m_prssamgr = nullptr;
        m_mdssamgr = nullptr;
        m_is_insert_bb = false;
        m_need_rebuild_prssa = false;
        m_liveness_mgr = (MDLivenessMgr*)m_rg->getPassMgr()->registerPass(
                             PASS_MDLIVENESS_MGR);

        UINT c = MAX(11, m_rg->getMDSystem()->getNumOfMD());
        m_md2lt_map = new MD2MDLifeTime(c);
        m_mdlt_count = 0;
        m_pool = smpoolCreate(2 * sizeof(MDLT), MEM_COMM);
        m_ir_ptr_pool = smpoolCreate(4 * sizeof(xcom::SC<IR*>), MEM_CONST_SIZE);
    }
    virtual ~RegPromot()
    {
        m_dont_promote.clean(*m_misc_bs_mgr);

        delete m_md2lt_map;
        m_md2lt_map = nullptr;
        smpoolDelete(m_pool);
        smpoolDelete(m_ir_ptr_pool);
    }

    void buildLifeTime();

    void cleanLiveBBSet();

    virtual bool dump() const;

    //Prepare context before doing reg promotion.
    void init();
    //Return true if 'ir' can be promoted.
    //Note ir must be memory reference.
    virtual bool isPromotable(IR const* ir) const;

    Region * getRegion() const { return m_rg; }
    virtual CHAR const* getPassName() const { return "Register Promotion"; }
    PASS_TYPE getPassType() const { return PASS_RP; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
