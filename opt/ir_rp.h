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
class ExactAccTab;
class InexactAccTab;
class RegPromot;

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

class DontPromoteTab : public TTab<IR const*> {
    COPY_CONSTRUCTOR(DontPromoteTab);
    Region * m_rg;
public:
    explicit DontPromoteTab(Region * rg)
    {
        ASSERT0(rg);
        m_rg = rg;
    }
    inline bool is_overlap(IR const* ir)
    {
        TTabIter<IR const*> it;
        for (IR const* t = get_first(it); t != nullptr; t = get_next(it)) {
            if (!t->isNotOverlapViaMDRef(ir, m_rg)) {
                return true;
            }
        }
        return false;
    }

    bool dump() const
    {
        if (!m_rg->isLogMgrInit()) { return false; }
        note(m_rg, "\n==-- DUMP Dont Promotion Table --==\n");
        TTabIter<IR const*> it;
        for (IR const* t = get_first(it); t != nullptr; t = get_next(it)) {
            dumpIR(t, m_rg, nullptr, IR_DUMP_DEF);
        }
        return true;
    }
};


class RefHashFunc {
    GVN * m_gvn;
public:
    void initMem(GVN * gvn);

    bool compareArray(IR * t1, IR * t2) const;
    bool compareIndirectAccess(IR * t1, IR * t2) const;
    bool compareDirectAccess(IR * t1, IR * t2) const;
    bool compare(IR * t1, IR * t2) const;

    //The function will modify m_iter.
    UINT get_hash_value(IR * t, UINT bucket_size) const;
};


typedef VecIdx RefTabIter;
class RefTab : public Hash<IR*, RefHashFunc> {
public:
    RefTab(UINT bucksize) : Hash<IR*, RefHashFunc>(bucksize) {}

    void dump(Region * rg) const
    {
        ASSERT0(rg);
        if (!rg->isLogMgrInit()) { return; }

        note(rg, "\n==---- DUMP Delegate Table ----==");
        VecIdx cur = 0;
        for (IR const* dele = get_first(cur);
             !IS_VECUNDEF(cur); dele = get_next(cur)) {
            //Dump IR tree for complex delegate.
            dumpIR(dele, rg, nullptr, IR_DUMP_DEF|IR_DUMP_KID);
        }
    }

    void initMem(GVN * gvn) { m_hf.initMem(gvn); }
};


class DelegateMgr {
    COPY_CONSTRUCTOR(DelegateMgr);
    //True if DUChain of restore has been built.
    bool m_is_restore_duchain_built;
    RegPromot * m_rp;
    //Record a delegate to IR expressions which have same value in
    //array base and subexpression.
    RefTab m_deletab;

    class CompareFuncOfIR {
    public:
        bool is_less(IR * t1, IR * t2) const { return t1->id() < t2->id(); }
        bool is_equ(IR * t1, IR * t2) const { return t1 == t2; }
        IR * createKey(IR * t) { return t; }
    };

    //Map IR expression to promoted PR.
    //Each delegate has its own delegated PR.
    //A delegated PR will used to replace occurrences of delegate.
    //Note the delegated PR is always used as a duplication, thus do NOT
    //insert it into IR tree directly.
    TMap<IR*, IR*, CompareFuncOfIR> m_dele2pr;

    //Map IR expression to STPR which generates the initialized value.
    //The initialized STPR should be placed in preheader BB.
    TMap<IR*, IR*, CompareFuncOfIR> m_dele2init;

    //Map IR expression to STMT which restore the PR into memory.
    //The restore STMT should be placed in epilog BB.
    TMap<IR*, IR*, CompareFuncOfIR> m_dele2restore;
protected:
    void clean();
    void collectOutsideLoopDef(IR const* delegate, IRSet const& set,
                               LI<IRBB> const* li);
    void collectOutsideLoopUse(IR const* delegate, IRSet const& set,
                               LI<IRBB> const* li);

    DUSet * genOutsideDefSet(IR const* delegate)
    {
        DUSet * defset = m_dele2outsidedefset.get(delegate);
        if (defset == nullptr) {
            defset = (DUSet*)getSBSMgr()->allocSBitSetCore();
            m_dele2outsidedefset.set(delegate, defset);
        }
        return defset;
    }
    DUSet * genOutsideUseSet(IR const* delegate)
    {
        DUSet * useset = m_dele2outsideuseset.get(delegate);
        if (useset == nullptr) {
            useset = (DUSet*)getSBSMgr()->allocSBitSetCore();
            m_dele2outsideuseset.set(delegate, useset);
        }
        return useset;
    }

    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool);
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }
    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
public:
    Region * m_rg;
    MDSSAMgr * m_mdssamgr;
    SMemPool * m_pool;
    xcom::DefMiscBitSetMgr m_sbs_mgr;

    //Map delegate to USE set.
    //The field records outside loop USE for 'delegate' if delegate is stmt.
    TMap<IR const*, DUSet*> m_dele2outsideuseset;

    //Map delegate to DEF set.
    //The field records outside loop DEF for 'delegate' if delegate is exp.
    TMap<IR const*, DUSet*> m_dele2outsidedefset;
public:
    DelegateMgr(RegPromot * rp, Region * rg, GVN * gvn, UINT acc_num) :
        m_deletab(acc_num)
    {
        m_is_restore_duchain_built = false;
        m_rp = rp;
        m_rg = rg;
        m_deletab.initMem(gvn);
        m_pool = smpoolCreate(4 * sizeof(xcom::SC<IR*>), MEM_COMM);
        m_mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->queryPass(
            PASS_MDSSA_MGR));
    }
    ~DelegateMgr() { clean(); }

    void addToOutsideUseSet(IR const* delegate, IR * ir);

    void collectOutsideLoopDefUse(IR const* occ, IR const* delegate,
                                  LI<IRBB> const* li);

    //The function add delegate using straightforward strategy. Note user must
    //ensure the delegate is unique.
    void createDelegateInfo(IR * delegate);

    //The function using RefTab to create delegate for 'ref' to keep the
    //delegate is unique. Note the delegate may be 'ref' itself.
    IR * createUniqueDelegate(IR * ref);
    
    void dumpDele2Restore() const;
    bool dump() const;

    //Get promoted PR for given delegate.
    IR const* getPR(IR const* delegate) const
    { return m_dele2pr.get(const_cast<IR*>(delegate)); }
    RefTab * getDelegateTab() { return &m_deletab; }
    xcom::DefSegMgr * getSegMgr() { return m_sbs_mgr.getSegMgr(); }
    xcom::DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }

    DUSet const* getOutsideUseSet(IR const* delegate) const
    { return m_dele2outsideuseset.get(const_cast<IR*>(delegate)); }

    //The function generates the initialing stmt for delegated PR.
    //rhs: the RHS of initialing stmt.
    IR * genInitStmt(IR const* delegate, IR * rhs);
    //Get promoted PR for given delegate.
    IR * getInitStmt(IR const* delegate) const
    { return m_dele2init.get(const_cast<IR*>(delegate)); }

    //Generate code to fulfill epilog of delegate.
    //pr: the PR to be restored to memory.
    IR * genRestoreStmt(IR const* delegate, IR * pr);
    IR * getRestoreStmt(IR const* delegate) const
    { return m_dele2restore.get(const_cast<IR*>(delegate)); }

    bool hasLoopOutsideUse(IR const* delegate) const
    {
        ASSERT0(delegate);
        DUSet const* useset = getOutsideUseSet(delegate);
        return useset != nullptr && !useset->is_empty();
    }

    //Return true if dele is a delegate.
    bool isDelegate(IR const* dele) const
    { return getInitStmt(dele) != nullptr; }
    //Return true if DUChain of restore has been built.
    bool isRestoreDUChainBuilt() const { return m_is_restore_duchain_built; }

    void setRestoreDUChainBuilt(bool has_built)
    { m_is_restore_duchain_built = has_built; }
};


#define RP_UNKNOWN 0
#define RP_SAME_ARRAY 1
#define RP_DIFFERENT_ARRAY 2
#define RP_SAME_OBJ 3
#define RP_DIFFERENT_OBJ 4

typedef TMapIter<MD const*, IR*> ExactAccTabIter;

//The table records the IR with exact MD accessing.
class ExactAccTab : public TMap<MD const*, IR*> {
    COPY_CONSTRUCTOR(ExactAccTab);
    typedef TMap<UINT, IRList*> MD2OccLst;
    typedef TMapIter<UINT, IRList*> MD2OccLstIter;
    MD2OccLst m_md2occlst;
public:
    ExactAccTab() {}
    ~ExactAccTab() { clean(); }

    //The function will add 'ir' as occurrence for specific MD.
    //The first ir will be regarded as a delegate for all those IRs which
    //reference the same MD.
    //Note the first ir to specific MD is the delegate.
    void addOcc(IR * ir);

    void clean()
    {
        IRList * lst = nullptr;
        MD2OccLstIter it;
        for (MDIdx mdid = m_md2occlst.get_first(it, &lst); mdid != MD_UNDEF;
             mdid = m_md2occlst.get_next(it, &lst)) {
            ASSERT0(lst);
            delete lst;
        }
        m_md2occlst.clean();
        TMap<MD const*, IR*>::clean();
    }
    //Collect outside loop DefUse information.
    void collectOutsideLoopDU(MD const* md, IR const* delegate,
                              LI<IRBB> const* li,
                              MOD DelegateMgr & delemgr);

    void dump(Region const* rg) const;

    IR * getDele(MD const* md) const { return get(md); }
    IRList * getOccs(MD const* md) const { return m_md2occlst.get(md->id()); }

    //Return true if ir in occs list is unique.
    bool isOccUnique(DefMiscBitSetMgr * sm) const;
    
    void remove(MD const* md);

    bool verify() const;
};

typedef TTabIter<IR*> InexactAccTabIter;

//The table records the IR with inexact MD accessing or even without a must MD.
class InexactAccTab : public TTab<IR*> {
    COPY_CONSTRUCTOR(InexactAccTab);
public:
    InexactAccTab() {}
    void addOcc(IR * ir) { append_and_retrieve(ir); }
    void dump(Region * rg) const;
};

//Perform Register Promotion.
//Register Promotion combines multiple memory load of the
//same memory location into one PR.
class RegPromot : public Pass {
    COPY_CONSTRUCTOR(RegPromot);
    typedef TTabIter<IR const*> ConstIRTabIter;
    typedef TTab<IR const*> ConstIRTab;
    typedef DMapEx<IR*, IR*> Occ2Occ;
    typedef DMapEx<IR*, IR*>::Tsrc2TtgtIter Occ2OccIter;
    class RPCtx {
    public:
        bool need_rebuild_domtree;
        DomTree * domtree;
        OptCtx * oc;
    public:
        RPCtx(OptCtx * t) : need_rebuild_domtree(false),
            domtree(nullptr), oc(t) {}
        ~RPCtx()
        {
            if (domtree != nullptr) {
                delete domtree;
                domtree = nullptr;
            }
        }
        void buildDomTree(IRCFG * cfg)
        {
            ASSERT0(oc->is_dom_valid());
            if (domtree == nullptr) {
                domtree = new DomTree();
            } else {
                domtree->erase();
            }
            cfg->genDomTree(*domtree);
            need_rebuild_domtree = false;
        }
    };

protected:
    MD2MDLifeTime * m_md2lt_map;
    UINT m_mdlt_count;
    SMemPool * m_pool;
    MDSystem * m_md_sys;
    MDSetMgr * m_mds_mgr;
    xcom::DefMiscBitSetMgr * m_misc_bs_mgr;
    IRCFG * m_cfg;
    TypeMgr * m_tm;
    DUMgr * m_dumgr;
    GVN * m_gvn;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    DontPromoteTab m_dont_promote;
    xcom::BitSetMgr m_bs_mgr;
    MDLivenessMgr * m_liveness_mgr;

protected:
    //Return true if the loop is promotable.
    bool analyszLoop(LI<IRBB> const* li, ExactAccTab & exact_tab,
                     InexactAccTab & inexact_tab);
    void addDUChainForExpTree(IR * root, IR * startir, IRBB * startbb,
                              RPCtx const& ctx);
    void addSSADUChainForExpOfRestore(IR const* dele,
                                      DelegateMgr const& delemgr,
                                      RPCtx const& ctx);
    void addDUChainForRHSOfInitDef(IR const* dele, IR * init_stmt,
                                   RPCtx const& ctx);
    //Build DU chain for initialization-def and outside loop exposed-use.
    void addDUChainForInitDefAndExposedUse(IR const* dele,
                                           IR * init_stmt,
                                           DelegateMgr const& delemgr,
                                           RPCtx const& ctx);
    //Build DU chain for intra-loop-def and its USE.
    void addDUChainForIntraDefAndUseSet(Occ2Occ const& occ2newocc,
                                        IRSet const& useset,
                                        IR * newocc_def, RPCtx const& ctx);
    //Process inexact access tab.
    //Build DU chain for initialization-def and intra-loop-use.
    void addDUChainForInitDef(IR const* dele, IR * init_stmt,
                              Occ2Occ const& occ2newocc,
                              InexactAccTab const& inexact_tab,
                              OUT IRList & deflst, RPCtx const& ctx);
    //Process exact access tab.
    //Build DU chain for initialization-def and intra-loop-use.
    void addDUChainForInitDef(IR const* dele, IR * init_stmt,
                              Occ2Occ const& occ2newocc,
                              ExactAccTab const& exact_tab,
                              OUT IRList & deflst, RPCtx const& ctx);
    //Build DU chain for intra-loop-def and its USE.
    void addDUChainForIntraDef(Occ2Occ const& occ2newocc,
                               IRList const& deflst, RPCtx const& ctx);
    void addDUChainForRestoreToOutsideUse(IR const* dele,
                                          DelegateMgr const& delemgr,
                                          RPCtx const& ctx);
    UINT analyzeIndirectAccessStatus(IR const* ref1, IR const* ref2);
    UINT analyzeArrayStatus(IR const* ref1, IR const* ref2);
    void addExactAccess(OUT ExactAccTab & exact_tab,
                        MD const* exact_md, IR * ir);
    void addDontPromote(IR const* ir);
    void addDUChainForInexactAcc(DelegateMgr const& delemgr,
                                 Occ2Occ const& occ2newocc,
                                 InexactAccTab const& inexact_tab,
                                 LI<IRBB> const* li,
                                 IRBB * preheader,
                                 MOD RPCtx & ctx);
    void addDUChainForInexactAccDele(IR const* dele,
                                     DelegateMgr const& delemgr,
                                     Occ2Occ const& occ2newocc,
                                     InexactAccTab const& inexact_tab,
                                     MOD RPCtx & ctx);
    void addDUChainForExactAccDele(IR const* dele, Occ2Occ const& occ2newocc,
                                   DelegateMgr const& delemgr,
                                   ExactAccTab const& exact_tab,
                                   MOD RPCtx & ctx,
                                   IRBB * preheader,
                                   LI<IRBB> const* li);

    void buildDUChainOnDemandForPROp(IR * def, IR * use, RPCtx const& ctx);
    void buildDUChainOnDemand(IR * def, IR * use, RPCtx const& ctx);
    //Return true if success.
    bool buildPRSSADUChainForInexactAcc(Occ2Occ const& occ2newocc,
                                        DelegateMgr const& delemgr,
                                        LI<IRBB> const* li,
                                        IRBB * preheader,
                                        MOD RPCtx & ctx);
    bool buildPRSSADUChainForExactAcc(IR const* dele,
                                      Occ2Occ const& occ2newocc,
                                      DelegateMgr const& delemgr,
                                      LI<IRBB> const* li,
                                      IRBB * preheader,
                                      MOD RPCtx & ctx);

    void clean();
    void cleanLiveBBSet();
    void checkAndRemoveInvalidExactOcc(ExactAccTab & acctab);
    void clobberExactAccess(IR const* ir, MOD ExactAccTab & exact_tab);
    void clobberInexactAccess(IR const* ir, MOD InexactAccTab & inexact_tab);
    void clobberAccess(IR const* ir,
                       MOD ExactAccTab & exact_tab,
                       MOD InexactAccTab & inexact_tab);
    bool checkArrayIsLoopInvariant(IN IR * ir, LI<IRBB> const* li);
    bool checkIndirectAccessIsLoopInvariant(IR const* ir, LI<IRBB> const* li);

    bool EvaluableScalarReplacement(List<LI<IRBB> const*> & worklst,
                                    MOD RPCtx & ctx);

    void findAndRecordRestore(IR * occ, IR const* delegate,
                              MOD DelegateMgr & delemgr,
                              OUT ConstIRTab & restore2mem);
    IRBB * findSingleExitBB(LI<IRBB> const* li);
    void freeLocalStruct(TMap<IR*, DUSet*> & delegate2use,
                         TMap<IR*, DUSet*> & m_delegate2def,
                         TMap<IR*, IR*> & delegate2pr,
                         DefMiscBitSetMgr * sbs_mgr);

    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_misc_bs_mgr; }
    xcom::DefSegMgr * getSegMgr() const { return getSBSMgr()->getSegMgr(); }
    MDLivenessMgr * getMDLivenessMgr() const { return m_liveness_mgr; }
    MDLT * getMDLifeTime(MD * md);

    void handleInexactAccOcc(MOD DelegateMgr & delemgr,
                             InexactAccTab & inexact_tab,
                             OUT ConstIRTab & restore2mem,
                             OUT Occ2Occ & occ2newocc,
                             InexactAccTabIter & ti, RPCtx const& ctx);
    void handleExactAccOcc(IR const* dele,
                           MOD DelegateMgr & delemgr,
                           LI<IRBB> const* li,
                           OUT ConstIRTab & restore2mem,
                           OUT Occ2Occ & occ2newocc,
                           IRIter & ii,
                           ExactAccTab & exact_tab, RPCtx const& ctx);
    void handleExpInBody(IR * ref, IR const* delegate,
                         DelegateMgr const& delemgr,
                         OUT Occ2Occ & occ2newocc, RPCtx const& ctx);

    //restore2mem: record the delegate that need to restore.
    void handleStmtInBody(IR * ref, IR const* delegate,
                          MOD DelegateMgr & delemgr,
                          OUT ConstIRTab & restore2mem,
                          OUT Occ2Occ & occ2newocc, RPCtx const& ctx);

    //restore2mem: record the delegate that need to restore.
    void handleAccessInBody(IR * ref, IR const* delegate,
                            MOD DelegateMgr & delemgr,
                            OUT ConstIRTab & restore2mem,
                            OUT Occ2Occ & occ2newocc, RPCtx const& ctx);

    //Generate code to fulfill epilog of delegate.
    void handleEpilog(ConstIRTab const& restore2mem, DelegateMgr & delemgr,
                      IRBB * exit_bb, MOD RPCtx & ctx);

    //The function generates iniailization code of promoted PR.
    //Note the function leaves the work that to build DU chain of PR and STPR
    //to the sebsequent function, it will be done at
    //buildDUChainForDeleRelatedPR().
    void handlePrologForExp(IR const* delegate, IR const* promoted_pr,
                            DelegateMgr & delemgr, IR * rhs,
                            IRBB * preheader);

    //The function generates iniailization code of promoted PR.
    //Note the function leaves the work that to build DU chain of PR and STPR
    //to the sebsequent function, it will be done at
    //buildDUChainForDeleRelatedPR().
    void handlePrologForStmt(IR const* delegate, IR const* promoted_pr,
                             DelegateMgr & delemgr, IR * rhs,
                             IRBB * preheader);

    //The function generates iniailization code of PR that is corresponding to
    //each delegate. And building DU chain between all USE occurrences of
    //the PR.
    void handleProlog(IR const* delegate, IR const* pr, DelegateMgr & delemgr,
                      IRBB * preheader);
    bool handleArrayRef(IN IR * ir, LI<IRBB> const* li,
                        OUT ExactAccTab & exact_tab,
                        OUT InexactAccTab & inexact_tab, bool * added);
    bool handleGeneralRef(IR * ir, LI<IRBB> const* li,
                          OUT ExactAccTab & exact_tab,
                          OUT InexactAccTab & inexact_tab, bool * added);
    //Return true if the caller can keep doing the analysis.
    //That means there are no memory referrences clobbered the
    //candidate in of exact_tab.
    //Return false if find unpromotable memory reference, this may
    //prevent entire loop be promoted.
    //ir: stmt or expression to be handled.
    bool handleGeneralMustRef(IR * ir, LI<IRBB> const* li,
                              OUT ExactAccTab & exact_tab,
                              OUT InexactAccTab & inexact_tab, bool * added);
    bool handleInexactOrMayRef(IR * ir, LI<IRBB> const* li,
                               OUT ExactAccTab & exact_tab,
                               OUT InexactAccTab & inexact_tab, bool * added);
    bool handleIndirect(IR * ir, LI<IRBB> const* li,
                        OUT ExactAccTab & exact_tab,
                        OUT InexactAccTab & inexact_tab, bool * added);

    bool isMayThrow(IR * ir, IRIter & iter);

    bool mayBeGlobalRef(IR * ref)
    {
        MD const* md = ref->getRefMD();
        if (md != nullptr && md->is_global()) { return true; }

        MDSet const* mds = ref->getRefMDSet();
        if (mds == nullptr) { return false; }

        MDSetIter iter = nullptr;
        for (BSIdx i = mds->get_first(&iter);
             i != BS_UNDEF; i = mds->get_next(i, &iter)) {
            MD const* md2 = m_md_sys->getMD((MDIdx)i);
            ASSERT0(md2);
            if (md2->is_global()) { return true; }
        }
        return false;
    }

    //The function sweep out the Access Expression or Stmt from 'exact_tab' and
    //'inexact_tab' which MD reference may or must overlaped with given 'ir'
    //except the ones that are exactly covered by 'ir'.
    //The function uses MD reference and consider both MustRef MD and MayRef
    //MDSet, whereas will not consider special characters of ir.
    //Return true if find overlapped reference with 'ir'.
    bool sweepOutAccess(IR * ir, MOD ExactAccTab & exact_tab,
                        MOD InexactAccTab & inexact_tab);

    //The function sweep out the Access Expression or Stmt from 'exact_tab'
    //which MD reference may or must overlaped with given 'ir'
    //except the ones that are exactly covered by 'ir'.
    //This function consider both MustRef MD and MayRef MDSet.
    //Return true if find overlapped reference with 'ir'.
    bool sweepOutExactAccess(IR * ir, MOD ExactAccTab & exact_tab);

    //The function sweep out the Access Expression or Stmt from 'inexact_tab'
    //which MD reference may or must overlaped with given 'ir'
    //except the ones that are exactly covered by 'ir'.
    //This function consider both MustRef MD and MayRef MDSet.
    //Return true if find overlapped reference with 'ir'.
    bool sweepOutInexactAccess(IR * ir, MOD InexactAccTab & inexact_tab);
    bool scanIRTreeList(IR * root, LI<IRBB> const* li,
                        OUT ExactAccTab & exact_tab,
                        OUT InexactAccTab & inexact_tab);
    //Find promotable memory references.
    //Return true if current memory referense does not clobber other
    //candidate in list. Or else return false means there are ambiguous
    //memory reference.
    //Return false if find unpromotable memory reference, this may
    //prevent entire loop to be promoted.
    bool scanStmt(IR * ir, LI<IRBB> const* li,
                  OUT ExactAccTab & exact_tab,
                  OUT InexactAccTab & inexact_tab);
    //Scan BB and find promotable memory reference.
    //If this function will find out unpromotable accessing that with ambiguous
    //memory reference. Those related promotable accesses will NOT be promoted.
    //e.g:a[0] = ...
    //    a[i] = ...
    //    a[0] is promotable, but a[i] is not, then a[0] can not be promoted.
    //If there exist memory accessing that we do not know where it access,
    //whole loop is unpromotable.
    //Return false if loop is unpromotable.
    bool scanBB(IN IRBB * bb, LI<IRBB> const* li,
                OUT ExactAccTab & exact_tab,
                OUT InexactAccTab & inexact_tab);

    //The function promote IR.
    //dele: the delegate which indicates an exact-accessing reference.
    void promoteExactAccessDelegate(IR const* dele, DelegateMgr & delemgr,
                                    LI<IRBB> const* li, IRIter & ii,
                                    IRBB * preheader, IRBB * exit_bb,
                                    ExactAccTab & exact_tab,
                                    MOD RPCtx & ctx);
    //Return true if IR is promoted, otherwise false.
    bool promoteInexactAccessDelegate(DelegateMgr & delemgr,
                                      LI<IRBB> const* li,
                                      IRBB * preheader,
                                      IRBB * exit_bb,
                                      InexactAccTab & inexact_tab,
                                      IRIter & ii,
                                      MOD RPCtx & ctx);
    void promoteInexactAccess(LI<IRBB> const* li, IRBB * preheader,
                              IRBB * exit_bb, InexactAccTab & inexact_tab,
                              IRIter & ii, MOD RPCtx & ctx);
    void promoteExactAccess(LI<IRBB> const* li, IRIter & ii, IRBB * preheader,
                            IRBB * exit_bb, ExactAccTab & exact_tab,
                            MOD RPCtx & ctx);
    //The function will promote occ in exact_tab or inexact_tab.
    void promote(LI<IRBB> const* li, IRBB * exit_bb, IRBB * preheader,
                 IRIter & ii, ExactAccTab & exact_tab,
                 InexactAccTab & inexact_tab, MOD RPCtx & ctx);
    bool preventByDontPromoteTab(IR const* ir);

    //Fixup DU chain if there is untrue dependence.
    //occ2newocc: record the IR stmt/exp that need to fixup.
    void removeDUChainForOrgOcc(Occ2Occ & occ2newocc, RPCtx const& ctx);
    void removeMDPhiDUChain(IR const* dele, LI<IRBB> const* li,
                            DelegateMgr const& delemgr);
    void removeMDPhiForInexactAcc(DelegateMgr const& delemgr,
                                  InexactAccTab & inexact_tab,
                                  LI<IRBB> const* li);
    void removeRedundantDUForInexactAcc(Occ2Occ & occ2newocc,
                                        DelegateMgr const& delemgr,
                                        InexactAccTab & inexact_tab,
                                        LI<IRBB> const* li,
                                        RPCtx const& ctx);

    //The function try to insert stub-BB before 'exit_bb' if there is MDPhi in
    //the BB.
    IRBB * tryInsertStubExitBB(IRBB * exit_bb, xcom::Edge const* exitedge,
                               MOD RPCtx & ctx);
    //Return true if there are memory locations have been promoted.
    bool tryPromoteLoop(LI<IRBB> const* li, IRIter & ii, MOD RPCtx & ctx);

    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool != nullptr);
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p != nullptr);
        ::memset(p, 0, size);
        return p;
    }

public:
    RegPromot(Region * rg) : Pass(rg), m_dont_promote(rg)
    {
        ASSERT0(rg != nullptr);
        m_md_sys = rg->getMDSystem();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_dumgr = rg->getDUMgr();
        m_mds_mgr = rg->getMDSetMgr();
        m_misc_bs_mgr = rg->getMiscBitSetMgr();
        m_gvn = nullptr;
        m_prssamgr = nullptr;
        m_mdssamgr = nullptr;
        m_liveness_mgr = (MDLivenessMgr*)m_rg->getPassMgr()->registerPass(
                             PASS_MDLIVENESS_MGR);

        UINT c = MAX(11, m_rg->getMDSystem()->getNumOfMD());
        m_md2lt_map = new MD2MDLifeTime(c);
        m_mdlt_count = 0;
        m_pool = smpoolCreate(2 * sizeof(MDLT), MEM_COMM);
    }
    virtual ~RegPromot()
    {
        delete m_md2lt_map;
        m_md2lt_map = nullptr;
        smpoolDelete(m_pool);
    }

    void buildLifeTime();

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const;

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    //Prepare context before doing reg promotion.
    void init() {}
    //Return true if 'ir' can be promoted.
    //Note ir must be memory reference.
    virtual bool isPromotable(IR const* ir) const;

    virtual CHAR const* getPassName() const { return "Register Promotion"; }
    PASS_TYPE getPassType() const { return PASS_RP; }

    virtual bool perform(OptCtx & oc);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
};

} //namespace xoc
#endif
