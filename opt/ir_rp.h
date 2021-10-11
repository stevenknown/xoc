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
            if (!t->isNotOverlapViaMDRef(ir)) {
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


class RefTab : public Hash<IR*, RefHashFunc> {
public:
    RefTab(UINT bucksize) : Hash<IR*, RefHashFunc>(bucksize) {}

    void dump(Region * rg) const
    {
        ASSERT0(rg);
        if (!rg->isLogMgrInit()) { return; }

        note(rg, "\n==---- DUMP Delegate Table ----==");
        INT cur = 0;
        for (IR const* dele = get_first(cur);
             cur >= 0; dele = get_next(cur)) {
            dumpIR(dele, rg);
        }
    }

    void initMem(GVN * gvn) { m_hf.initMem(gvn); }
};


class DelegateMgr {
    COPY_CONSTRUCTOR(DelegateMgr);
    //Record a delegate to IR expressions which have same value in
    //array base and subexpression.
    RefTab m_deletab;

    //Map IR expression to promoted PR.
    //Each delegate has its own delegated PR.
    //A delegated PR will used to replace occurrences of delegate.
    //Note the delegated PR is always used as a duplication, thus do NOT
    //insert it into IR tree directly.
    TMap<IR*, IR*> m_dele2pr;

    //Map IR expression to STPR which generates the initialized value.
    //The initialized STPR should be placed in preheader BB.
    TMap<IR*, IR*> m_dele2init;

    //Map IR expression to STMT which restore the PR into memory.
    //The restore STMT should be placed in epilog BB.
    TMap<IR*, IR*> m_dele2restore;
protected:
    void clean();
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
    DelegateMgr(Region * rg, GVN * gvn, UINT acc_num) : m_deletab(acc_num)
    {
        m_rg = rg;
        m_deletab.initMem(gvn);
        m_pool = smpoolCreate(4 * sizeof(xcom::SC<IR*>), MEM_COMM);
        m_mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->queryPass(
            PASS_MD_SSA_MGR));
    }
    ~DelegateMgr() { clean(); }

    void addToOutsideUseSet(IR const* delegate, IR * ir);

    void collectOutsideLoopDefUse(IR * ref, IR * delegate, LI<IRBB> const* li);

    //The function add delegate using straightforward strategy. Note user must
    //ensure the delegate is unique.
    void createDelegateInfo(IR * delegate);

    //The function using RefTab to create delegate for 'ref' to keep the
    //delegate is unique. Note the delegate may be 'ref' itself.
    IR * createUniqueDelegate(IR * ref);

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
    IR * genInitStmt(IR const* delegate, IN IR * rhs);
    //Get promoted PR for given delegate.
    IR * getInitStmt(IR const* delegate) const
    { return m_dele2init.get(const_cast<IR*>(delegate)); }

    //Generate code to fulfill epilog of delegate.
    //pr: the PR to be restored to memory.
    IR * genRestoreStmt(IR const* delegate, IR * pr);
    IR * getRestoreStmt(IR const* delegate) const
    { return m_dele2restore.get(const_cast<IR*>(delegate)); }
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
    TMap<MD const*, IRList*> m_md2occlst;
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
        TMapIter<MD const*, List<IR*>*> it;
        for (MD const* md = m_md2occlst.get_first(it, &lst); md != nullptr;
             md = m_md2occlst.get_next(it, &lst)) {
            ASSERT0(lst);
            delete lst;
        }
        m_md2occlst.clean();
        TMap<MD const*, IR*>::clean();
    }

    void dump(Region const* rg) const;

    IR * getDele(MD const* md) const { return get(md); }
    IRList * getOccs(MD const* md) const { return m_md2occlst.get(md); }

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
        OptCtx * oc;
        RPCtx(OptCtx * t) : oc(t) {}
    };

private:
    MD2MDLifeTime * m_md2lt_map;
    Region * m_rg;
    UINT m_mdlt_count;
    SMemPool * m_pool;
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

private:
    void addDUChainForIntraDefAndUseSet(Occ2Occ const& occ2newocc,
                                        IRSet const& useset,
                                        LI<IRBB> const* li,
                                        IR * restore_stmt, IR * newocc_def);
    //Build DU chain for initialization-def and intra-loop-use.
    void addDUChainForInitDef(IR const* dele, IR * init_stmt,
                              Occ2Occ const& occ2newocc,
                              ExactAccTab const& exact_tab,
                              OUT IRList & deflst);
    //Build DU chain for intra-loop-def and its USE.
    void addDUChainForIntraDef(Occ2Occ const& occ2newocc,
                               IR * restore_stmt, IRList const& deflst,
                               LI<IRBB> const* li);
    void addDUChainForRestoreStmt(DelegateMgr & delemgr,
                                  ConstIRTab const& restore2mem);
    UINT analyzeIndirectAccessStatus(IR const* ref1, IR const* ref2);
    UINT analyzeArrayStatus(IR const* ref1, IR const* ref2);
    void addExactAccess(OUT ExactAccTab & exact_access,
                        MD const* exact_md, IR * ir);
    void addDontPromote(IR const* ir);
    void addDUChainForInexactAccDele(IR const* dele,
                                     DelegateMgr const& delemgr,
                                     Occ2Occ const& occ2newocc);
    void addDUChainForExactAccDele(IR const* dele, Occ2Occ const& occ2newocc,
                                   DelegateMgr const& delemgr,
                                   ExactAccTab const& exact_tab,
                                   LI<IRBB> const* li);
    void clean();
    void cleanLiveBBSet();
    void checkAndRemoveInvalidExactOcc(ExactAccTab & acctab);
    void clobberExactAccess(IR const* ir, MOD ExactAccTab & exact_acc);
    void clobberInexactAccess(IR const* ir, MOD InexactAccTab & inexact_acc);
    void clobberAccess(IR const* ir,
                       MOD ExactAccTab & exact_access,
                       MOD InexactAccTab & inexact_access);
    bool checkArrayIsLoopInvariant(IN IR * ir, LI<IRBB> const* li);
    bool checkIndirectAccessIsLoopInvariant(IR const* ir, LI<IRBB> const* li);

    void dump_mdlt();
    void dumpInexact(InexactAccTab & access);

    bool EvaluableScalarReplacement(List<LI<IRBB> const*> & worklst,
                                    MOD RPCtx & ctx);

    IRBB * findSingleExitBB(LI<IRBB> const* li);
    void freeLocalStruct(TMap<IR*, DUSet*> & delegate2use,
                         TMap<IR*, DUSet*> & m_delegate2def,
                         TMap<IR*, IR*> & delegate2pr,
                         DefMiscBitSetMgr * sbs_mgr);

    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_misc_bs_mgr; }
    xcom::DefSegMgr * getSegMgr() const { return getSBSMgr()->getSegMgr(); }
    MDLivenessMgr * getMDLivenessMgr() const { return m_liveness_mgr; }
    MDLT * getMDLifeTime(MD * md);

    //fixup_list: record the IR that need to fix up duset.
    void handleExpInBody(IR * ref, IR const* delegate,
                         DelegateMgr const& delemgr,
                         OUT Occ2Occ & occ2newocc, IRIter & ii);

    //fixup_list: record the IR that need to fix up duset.
    //restore2mem: record the delegate that need to restore.
    void handleStmtInBody(IR * ref, IR const* delegate,
                          MOD DelegateMgr & delemgr,
                          OUT ConstIRTab & restore2mem,
                          OUT Occ2Occ & occ2newocc,
                          LI<IRBB> const* li);

    //fixup_list: record the IR that need to fix up duset.
    //restore2mem: record the delegate that need to restore.
    void handleAccessInBody(IR * ref, IR const* delegate,
                            MOD DelegateMgr & delemgr,
                            OUT ConstIRTab & restore2mem,
                            OUT Occ2Occ & occ2newocc,
                            LI<IRBB> const* li, IRIter & ii);

    //Generate code to fulfill epilog of delegate.
    void handleEpilog(ConstIRTab const& restore2mem, DelegateMgr & delemgr,
                      IRBB * exit_bb);

    //The function generates iniailization code of PR that is corresponding to
    //each delegate. And building DU chain between all USE occurrences of
    //the PR.
    void handleProlog(IR const* delegate, IR const* pr, DelegateMgr & delemgr,
                      IRBB * preheader);
    bool handleArrayRef(IN IR * ir, LI<IRBB> const* li,
                        OUT ExactAccTab & exact_access,
                        OUT InexactAccTab & inexact_access, bool * added);
    bool handleGeneralRef(IR * ir, LI<IRBB> const* li,
                          OUT ExactAccTab & exact_access,
                          OUT InexactAccTab & inexact_access, bool * added);
    //Return true if the caller can keep doing the analysis.
    //That means there are no memory referrences clobbered the
    //candidate in of exact_acc.
    //Return false if find unpromotable memory reference, this may
    //prevent entire loop be promoted.
    //ir: stmt or expression to be handled.
    bool handleGeneralMustRef(IR * ir, LI<IRBB> const* li,
                              OUT ExactAccTab & exact_acc,
                              OUT InexactAccTab & inexact_acc, bool * added);
    bool handleInexactOrMayRef(IR * ir, LI<IRBB> const* li,
                               OUT ExactAccTab & exact_acc,
                               OUT InexactAccTab & inexact_acc, bool * added);
    bool handleIndirect(IR * ir, LI<IRBB> const* li,
                        OUT ExactAccTab & exact_acc,
                        OUT InexactAccTab & inexact_acc, bool * added);

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

    //The function sweep out the Access Expression or Stmt from 'exact_acc' and
    //'inexact_acc' which MD reference may or must overlaped with given 'ir'
    //except the ones that are exactly covered by 'ir'.
    //This function consider both MustRef MD and MayRef MDSet.
    void sweepOutAccess(IR * ir, MOD ExactAccTab & exact_acc,
                        MOD InexactAccTab & inexact_acc);

    //The function sweep out the Access Expression or Stmt from 'exact_acc'
    //which MD reference may or must overlaped with given 'ir'
    //except the ones that are exactly covered by 'ir'.
    //This function consider both MustRef MD and MayRef MDSet.
    void sweepOutExactAccess(IR * ir, MOD ExactAccTab & exact_acc);

    //The function sweep out the Access Expression or Stmt from 'inexact_acc'
    //which MD reference may or must overlaped with given 'ir'
    //except the ones that are exactly covered by 'ir'.
    //This function consider both MustRef MD and MayRef MDSet.
    void sweepOutInexactAccess(IR * ir, MOD InexactAccTab & inexact_acc);
    bool scanIRTreeList(IR * root, LI<IRBB> const* li,
                        OUT ExactAccTab & exact_acc,
                        OUT InexactAccTab & inexact_acc);
    //Find promotable memory references.
    //Return true if current memory referense does not clobber other
    //candidate in list. Or else return false means there are ambiguous
    //memory reference.
    //Return false if find unpromotable memory reference, this may
    //prevent entire loop to be promoted.
    bool scanStmt(IR * ir, LI<IRBB> const* li,
                  OUT ExactAccTab & exact_access,
                  OUT InexactAccTab & inexact_access);
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
                OUT ExactAccTab & exact_access,
                OUT InexactAccTab & inexact_access);

    //Return true if there is IR to be promoted, otherwise return false.
    //dele: the delegate which indicates an exact-accessing reference.
    bool promoteExactAccessDelegate(IR const* dele, DelegateMgr & delemgr,
                                    LI<IRBB> const* li, IRIter & ii,
                                    IRBB * preheader, IRBB * exit_bb,
                                    ExactAccTab & exact_tab);
    //Return true if there is IR being promoted, otherwise return false.
    bool promoteInexactAccessDelegate(IR const* dele,
                                      DelegateMgr & delemgr,
                                      LI<IRBB> const* li,
                                      IRBB * preheader,
                                      IRBB * exit_bb,
                                      InexactAccTab & inexact_acc,
                                      IRIter & ii);
    bool promoteInexactAccess(LI<IRBB> const* li, IRBB * preheader,
                              IRBB * exit_bb, InexactAccTab & inexact_access,
                              IRIter & ii);
    bool promoteExactAccess(LI<IRBB> const* li, IRIter & ii, IRBB * preheader,
                            IRBB * exit_bb, ExactAccTab & exact_tab);
    //Return true if there are memory locations have been promoted.
    bool promote(LI<IRBB> const* li, IRBB * exit_bb, IRIter & ii,
                 ExactAccTab & exact_access, InexactAccTab & inexact_access,
                 MOD RPCtx & ctx);

    void removeRedundantDUChain(Occ2Occ & occ2newocc);

    bool tryInsertPreheader(LI<IRBB> const* li, OUT IRBB ** preheader,
                            MOD RPCtx & ctx);
    bool tryPromote(LI<IRBB> const* li, IRBB * exit_bb, IRIter & ii,
                    ExactAccTab & exact_access, InexactAccTab & inexact_access,
                    MOD RPCtx & ctx);

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

    virtual bool dump() const;

    //Prepare context before doing reg promotion.
    void init() {}
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
