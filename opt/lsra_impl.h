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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _LSRA_IMPL_H_
#define _LSRA_IMPL_H_

namespace xoc {

class LSRAImpl;

//This enum is used to indicate the different inconsistent type after the
//linear scan register allocation.
typedef enum {
    //Inconsistent type is undefined.
    INCONSIST_UNDEF = 0,

    //Inconsistent type from PR to PR.
    //e.g: $p is split and renamed to $q. For an edge of CFG, $p is used in
    //the source BB, and $q is used in the destination BB, $p and $q are
    //inconsistent if they are assigned to different registers during LSRA,
    //the value of $q is not same as $p.
    INCONSIST_PR2PR = 1,

    //Inconsistent type from memory location of spilled PR to the new PR.
    //e.g: $p is split and renamed to $q. For an edge of CFG, $p is spilled to
    //memory in the source BB, and $q is used in the destination BB, memory of
    //$p and $q are inconsistent because the value of $p is in memory, not
    //in $q in in source BB.
    INCONSIST_MEM2PR = 2,

    //Inconsistent type from spilled PR to memory location of spilled PR.
    //e.g: $p is split and renamed to $q. For an edge of CFG, $p is used
    //in the source BB, and the spilled memory of $p is reloaded to $q in the
    //destination BB, $p and the spilled memory of $p are inconsistent because
    //the $p is not spilled to that memory in source BB.
    INCONSIST_PR2MEM = 3,
} INCONSIST_TYPE;

//CASE:Define local used type in global scope because some compiler
//complained 'uses local type'.
typedef struct tagVexPair {
    VexIdx fromid;
    VexIdx toid;
} VexPair;
class CompareVexPair {
public:
    bool is_less(VexPair t1, VexPair t2) const
    { return t1.fromid < t2.fromid || t1.toid < t2.toid; }
    bool is_equ(VexPair t1, VexPair t2) const
    { return t1.fromid == t2.fromid && t1.toid == t2.toid; }
    VexPair createKey(VexPair t) { return t; }
};

class InConsistPair {
public:
    VexIdx from_vex_id;
    VexIdx to_vex_id;
    LifeTime const* from_lt;
    LifeTime const* to_lt;

    //The spilled memory location var.
    Var const* mem_var;
    INCONSIST_TYPE type;
public:
    InConsistPair() {}
    InConsistPair(UINT id)
    {
        from_vex_id = VERTEX_UNDEF;
        to_vex_id = VERTEX_UNDEF;
        from_lt = nullptr;
        to_lt = nullptr;
        mem_var = nullptr;
        type = INCONSIST_UNDEF;
    }
};

typedef List<InConsistPair>::Iter InConsistPairListIter;
class InConsistPairList : public List<InConsistPair> {
public:
    void dump(Region const* rg) const
    {
        note(rg, "\n==-- DUMP %s --==", "InConsistPairList");
        InConsistPairListIter it;
        UINT i = 0;
        for (InConsistPair pair = get_head(&it);
             i < get_elem_count(); pair = get_next(&it), i++) {
            switch (pair.type) {
            case INCONSIST_PR2PR:
                ASSERT0(pair.from_lt && pair.to_lt);
                note(rg,
                    "\nNEED insert bb between BB%u and BB%u, and move:$%u->$%u",
                    pair.from_vex_id, pair.to_vex_id,
                    pair.from_lt->getPrno(), pair.to_lt->getPrno());
                break;
            case INCONSIST_MEM2PR:
                ASSERT0(pair.mem_var && pair.to_lt);
                note(rg,
                    "\nNEED insert bb between BB%u and BB%u, and copy:$%s->$%u",
                    pair.from_vex_id, pair.to_vex_id,
                    pair.mem_var->get_name()->getStr(), pair.to_lt->getPrno());
                break;
            case INCONSIST_PR2MEM:
                ASSERT0(pair.mem_var && pair.from_lt);
                note(rg,
                    "\nNEED insert bb between BB%u and BB%u, and copy:$%u->$%s",
                    pair.from_vex_id, pair.to_vex_id, pair.from_lt->getPrno(),
                    pair.mem_var->get_name()->getStr());
                break;
            default: UNREACHABLE(); break;
            }
        }
    }
};

//
//START LTConsistencyMgr
//
class LTConsistencyMgr {
public:
    //This enum is used to describe the status of the PR for a BB.
    typedef enum tagCONSIST_STATUS {
        //Initial state for PR consistency status.
        CONSIST_UNDEF = 0,

        //The PR does not cross the boundary of a BB.
        CONSIST_INVALID = 1,

        //The PR crosses the boundary of a BB.
        CONSIST_VALID = 2,
    } CONSIST_STATUS;

    //This enum is used to describe the status of the PR in a BB.
    typedef enum tagPR_STATUS {
        //The PR status is undefined for the current BB.
        PR_STATUS_UNDEF = 0,

        //The PR is spilled in the current BB.
        PR_STATUS_SPILLED = 1,

        //The PR is reloaded in the current BB.
        PR_STATUS_RELOADED = 2,

        //The PR is spilled and reloaded in the current BB.
        PR_STATUS_SPILLED_AND_RELOAD = 3,

        //The PR is already in memory for the current BB.
        PR_STATUS_IN_MEMORY = 4,
    } PR_STATUS;
protected:
    COPY_CONSTRUCTOR(LTConsistencyMgr);
    Region * m_rg;
    BBList * m_bb_list;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    LSRAImpl & m_impl;
    bool m_is_insert_bb;
    typedef TMap<VexPair, IRBB*, CompareVexPair> LatchMap;
    typedef TMapIter<LifeTime const*, CONSIST_STATUS> LT2STIter;
    typedef TMap<LifeTime const*, CONSIST_STATUS> LT2ST;
    typedef TMap<Var const*, PR_STATUS> Var2PRST;
    Vector<LT2ST*> m_lt2st_in_vec;
    Vector<LT2ST*> m_lt2st_out_vec;
    Vector<Var2PRST*> m_var2prst_vec;
protected:
    void addInStatus(LifeTime const* lt, CONSIST_STATUS st, UINT bbid)
    { genInTab(bbid)->setAlways(lt, st); }
    void addOutStatus(LifeTime const* lt, CONSIST_STATUS st, UINT bbid)
    { genOutTab(bbid)->setAlways(lt, st); }

    //This function will be used to record the status a PR through the var
    //generated by the PR spilling operation.
    //v: the var generated by the PR spilling operation, it is possible to be
    //   a null pointer if the spilling and reloaded is not required during the
    //   splitting.
    //st: the status of the PR in current BB.
    //bbid: the bbid of current BB.
    void addPRStatus(Var const* v, PR_STATUS st, UINT bbid)
    { genPRTab(bbid)->setAlways(v, st); }

    //This function shall add the status of PR for current BB per the spill and
    //reload bbid information.
    //
    //There are some rules need to be followed:
    //  1. If the PR in current BB is not related to spilling and reloading,
    //     set the status to PR_STATUS_UNDEF.
    //  2. If the PR is spilled in current BB, set the status to
    //     PR_STATUS_SPILLED.
    //  3. If the PR is reloaded in current BB, set the status to
    //     PR_STATUS_RELOADED.
    //  4. If the PR is spilled and reloaded in current BB, set the status to
    //     PR_STATUS_SPILLED_AND_RELOAD.
    //  5. If the start position and the end position of cur_bbid are included
    //     in the range from the end of anct lifetime to the start of new
    //     lifetime, that means the PR status is PR_STATUS_IN_MEMORY in
    //     cur_bbid, because the PR has already been spilled into memory in this
    //     range.
    //anct: the ancestor lifetime of the PR.
    //newlt: the new lifetime of the PR.
    //cur_bbid: current bbid.
    void addPRStatus(LifeTime const* anct, LifeTime const* newlt,
                     UINT cur_bbid);

    //Add the consist status for BB per the specified position, if the lifetime
    //of PR cross the pos, the status is CONSIST_VALID, or else is
    //CONSIST_INVALID.
    //lt: the lifetime of a PR.
    //start: the start position, it should be the start boundary of a BB.
    //end: the end position, it should be the end boundary of a BB.
    //bbid: current bbid.
    void addConsistStatus(LifeTime const* lt, Pos start, Pos end, UINT bbid);

    void computeLTConsistency();
    void computeEdgeConsistency(OUT InConsistPairList & inconsist_lst);
    void computeEdgeConsistencyImpl(xcom::Edge const* e,
                                    OUT InConsistPairList & inconsist_lst);

    //This function implements the inconsistency check based on forward
    //analysis.
    //anct: the ancestor lifetime of the PR.
    //newlt: the new lifetime of the PR after splited.
    //from: the source BBID of the edge in CFG.
    //to: the destination BBID of the edge in CFG.
    //inconsist_lst: the list to record the inconsistency information.
    //Returen value: return true if there is no inconsistency problem for
    //  anct and newlt; return false if it is uncertain that there is no
    //  inconsistency problem althrough current check passed, but the further
    //  check is still required.
    bool computeForwardConsistencyImpl(LifeTime const* anct,
        LifeTime const* newlt, UINT from, UINT to,
        OUT InConsistPairList & inconsist_lst);

    //This function implements the inconsistency check based on backward
    //analysis.
    //anct: the ancestor lifetime of the PR.
    //newlt: the new lifetime of the PR after splited.
    //from: the source BBID of the edge in CFG.
    //to: the destination BBID of the edge in CFG.
    //inconsist_lst: the list to record the inconsistency information.
    void computeBackwardConsistencyImpl(LifeTime const* anct,
        LifeTime const* newlt, UINT from, UINT to,
        OUT InConsistPairList & inconsist_lst);

    LT2ST * genInTab(UINT bbid)
    {
        LT2ST * tab = m_lt2st_in_vec.get(bbid);
        if (tab == nullptr) {
            tab = new LT2ST();
            m_lt2st_in_vec.set(bbid, tab);
        }
        return tab;
    }
    //This function generates and inserts a latch BB to hold the IRs for
    //inconsistency recovery.
    //The latch BB is a BB that need to be inserted on an edge in CFG.
    // For example: The latch BB below will be inserted between BB FROM and TO.
    //
    //       BB: FROM
    //       ...
    //          |
    //          V
    //       BB: LATCH
    //       ...
    //          |
    //          V
    //       BB: TO
    //       ...
    //
    IRBB * genLatchBB(MOD LatchMap & latch_map, InConsistPair const& pair);
    Var2PRST * genPRTab(UINT bbid)
    {
        Var2PRST * tab = m_var2prst_vec.get(bbid);
        if (tab == nullptr) {
            tab = new Var2PRST();
            m_var2prst_vec.set(bbid, tab);
        }
        return tab;
    }
    LT2ST * genOutTab(UINT bbid)
    {
        LT2ST * tab = m_lt2st_out_vec.get(bbid);
        if (tab == nullptr) {
            tab = new LT2ST();
            m_lt2st_out_vec.set(bbid, tab);
        }
        return tab;
    }
    LT2ST * getInTab(UINT bbid) const
    { return m_lt2st_in_vec.get(bbid); }
    Var2PRST * getPRTab(UINT bbid) const
    { return m_var2prst_vec.get(bbid); }
    LT2ST * getOutTab(UINT bbid) const
    { return m_lt2st_out_vec.get(bbid); }
    CONSIST_STATUS getInSt(LifeTime const* lt, UINT bbid) const
    {
        LT2ST * tab = getInTab(bbid);
        ASSERT0(tab);
        return tab->get(lt);
    }
    PR_STATUS getPRSt(Var const* v, UINT bbid) const
    {
        Var2PRST * tab = getPRTab(bbid);
        ASSERT0(tab);
        return tab->get(v);
    }
    CONSIST_STATUS getOutSt(LifeTime const* lt, UINT bbid) const
    {
        LT2ST * tab = getOutTab(bbid);
        ASSERT0(tab);
        return tab->get(lt);
    }
    CHAR const* getStName(CONSIST_STATUS st) const
    {
        switch (st) {
        case CONSIST_INVALID: return "invalid";
        case CONSIST_VALID: return "valid";
        default: UNREACHABLE();
        }
        return nullptr;
    }

    IRBB * insertLatch(IRBB const* from, MOD IRBB * to);

    void reviseEdgeConsistency(InConsistPairList const& inconsist_lst);
    void reviseTypeMEM2PR(MOD LatchMap & latch_map, InConsistPair const& pair);
    void reviseTypePR2MEM(MOD LatchMap & latch_map, InConsistPair const& pair);
    void reviseTypePR2PR(MOD LatchMap & latch_map, InConsistPair const& pair);
public:
    LTConsistencyMgr(LSRAImpl & impl);
    ~LTConsistencyMgr()
    {
        for (UINT i = 0; i < m_lt2st_in_vec.get_elem_count(); i++) {
            LT2ST * in = m_lt2st_in_vec.get(i);
            if (in != nullptr) { delete in; }
            LT2ST * out = m_lt2st_out_vec.get(i);
            if (out != nullptr) { delete out; }
            Var2PRST * prtbl = m_var2prst_vec.get(i);
            if (prtbl != nullptr) { delete prtbl; }
        }
    }

    void dump() const;

    void perform();
};
//END LTConsistencyMgr


//
//START SplitCtx
//
class SplitCtx {
public:
    //Top-down propagate information.
    IN Pos split_pos;

    //Top-down propagate information.
    IN IR const* split_pos_ir;

    //Top-down propagate information.
    IN LifeTime const* split_lt;

    //Bottom-up propagate information.
    OUT Pos reload_pos;

    //Bottom-up propagate information.
    OUT Occ reload_occ;
public:
    SplitCtx(Pos p)
    {
        split_pos = p;
        split_pos_ir = nullptr;
        split_lt = nullptr;
        reload_pos = POS_UNDEF;
        reload_occ = Occ(POS_UNDEF);
    }
    SplitCtx(Pos p, IR const* o, LifeTime const* lt)
    {
        split_pos = p;
        split_pos_ir = o;
        split_lt = lt;
        reload_pos = POS_UNDEF;
        reload_occ = Occ();
    }
};
//END SplitCtx


//
//START SplitMgr
//
class SplitMgr {
    COPY_CONSTRUCTOR(SplitMgr);
    LSRAImpl & m_impl;
    LinearScanRA & m_ra;
    Region * m_rg;
    IRMgr * m_irmgr;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    LivenessMgr * m_live_mgr;
    LTTab m_dont_split_tab;
private:
    //The function shrinks lifetime to properly position.
    void cutoffLTFromSpillPos(LifeTime * lt, Pos pos);

    //The function do all actions for the spill after the split position
    IR * doSpillAfterSplitPos(LifeTime * lt, SplitCtx const& ctx);

    //The function do all actions for the spill before the split position
    IR * doSpillBeforeSplitPos(LifeTime * lt, SplitCtx const& ctx);

    //The function inserts spill operation before or after split_pos.
    IR * insertSpillAroundSplitPos(LifeTime * lt, SplitCtx const& ctx);
    void insertSpillDuringSplitting(LifeTime * lt, SplitCtx const& ctx,
                                    bool canberemat,
                                    RematCtx const& rematctx,
                                    OUT IR *& spill);
    void insertReloadDuringSplitting(LifeTime * lt, LifeTime * newlt,
                                     SplitCtx const& ctx,
                                     bool canberemat,
                                     RematCtx const& rematctx,
                                     IR * spill);

    //Return true if 'stmt' defined the PR that lt represented.
    bool isDefLT(IR const* stmt, LifeTime const* lt) const;

    //The function shrinks lifetime to properly position.
    void shrinkLTToSplitPos(LifeTime * lt, Pos split_pos,
                            IR const* split_pos_ir);
    void selectSplitCandFromSet(LTSet const& set, SplitCtx const& ctx,
                                OUT LTSet & candlst,
                                OUT Vector<SplitCtx> & candctxvec);
    LifeTime * selectSplitCandImpl(LTSet & set, LifeTime * lt, bool tryself,
                                   OUT SplitCtx & ctx);
    LifeTime * selectSplitCandByDensity(LTSet & set, LifeTime * lt,
                                        bool tryself, OUT SplitCtx & ctx);
private:
    LifeTime * selectSplitCandFromActive(LifeTime * lt, bool tryself,
                                         OUT SplitCtx & ctx);
    LifeTime * selectSplitCandFromInActive(LifeTime * lt, bool tryself,
                                           OUT SplitCtx & ctx);

    //The function select a lifetime from 'lst' which the next-range from
    //the range of 'pos' is the furthest.
    //e.g:given pos is 10, and there are two lifetime in 'lst'.
    //    lt1:  <10-20>,<30-40>
    //    lt2:  <5-11>,<60-40>
    // the function return lt2.
    //Return nullptr if there is no lifetime has next-range.
    //pos: the position that begin to seek.
    LifeTime * selectLTByFurthestNextRange(LTSet const& lst, Pos pos,
                                           OUT Occ & reload_occ);

    //The function select a lifetime from 'lst' which the next-occ from
    //given position is the furthest.
    //e.g:given pos is 10, and there are two lifetime in 'lst'.
    //    lt1:  <5-40>, next-occ is in 20
    //    lt2:  <5-25>, next-occ is in 25
    // the function return lt2.
    //Return nullptr if there is no lifetime has next-occ.
    LifeTime * selectLTByFurthestNextOcc(LTSet const& lst, Pos pos,
                                         Vector<SplitCtx> const& ctxvec,
                                         OUT Occ & reload_occ);

    //The function splits 'lt' into two lifetimes, lt and newlt, at ctx's
    //reload_pos. The original 'lt' will be termiated at the reload_pos.
    //newlt will start at reload_pos and renamed to new PRNO.
    LifeTime * splitIntoTwoLT(LifeTime * lt, SplitCtx const& ctx);
public:
    SplitMgr(LSRAImpl & impl);

    //Add lt to dont-split-table that avoid lt to be splitting-candidate.
    void addToDontSplitTab(LifeTime * lt) { m_dont_split_tab.append(lt); }

    //The function checks whether if 'lt' can be splitted at 'split_pos', and
    //try to find the properly reload position if it can be splitted.
    //Return true if lt is splitting-candidate, otherwise means lt should not
    //be split at 'split_pos' or there is no properly reload position.
    bool checkIfCanBeSplitCand(LifeTime const* lt, Pos split_pos,
                               OUT Pos & reload_pos, OUT Occ & reload_occ);

    //Split lt into two lifetime accroding to the information given in 'ctx'.
    //Note after splitting the second half of 'lt' will be renamed to a newlt.
    //Return the newlt.
    LifeTime * splitAt(LifeTime * lt, SplitCtx const& ctx);

    //If there is no available register to assign to 'lt', the function try
    //to find alternative lifetime or 'lt' itself to split.
    LifeTime * selectSplitCand(LifeTime * lt, bool tryself, OUT SplitCtx & ctx);
};
//END SplitMgr

//This union is used to describe the split info of a PR if it was split in
//LSRA, that should be include the BBID of the spilled position, and the BBID
//of the reload position.
typedef union {
    //The complete data for the whole info.
    UINT64 value;
    struct {
        //The BBID of spilled position.
        UINT spill_bbid;

        //The BBID of reloaded position.
        UINT reload_bbid;
    } bb;
} PRSplitBB;

//
//START LSARImpl
//
//The class implements a default linear-scan algorithm.
class LSRAImpl {
public:
    enum REG_PREFER {
        PREFER_UNDEF = 0,
        PREFER_CALLER = 1,
        PREFER_CALLEE = 2,
    };
    typedef xcom::TMap<LifeTime const*, REG_PREFER> LT2Prefer;
    typedef xcom::TMapIter<LifeTime const*, REG_PREFER> LT2PreferIter;
    typedef xcom::TMap<Var const*, UINT64> Var2Split;
private:
    COPY_CONSTRUCTOR(LSRAImpl);
protected:
    bool m_is_insert_bb;
    bool m_use_expose;
    LinearScanRA & m_ra;
    RegSetImpl & m_rsimpl;
    Region * m_rg;
    TypeMgr * m_tm;
    IRMgr * m_irmgr;
    LivenessMgr * m_live_mgr;
    BBList * m_bb_list;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    xcom::List<LifeTime const*> m_splitted_newlt_lst;
    LT2Prefer m_lt2prefer;
    Var2Split m_var2split;
protected:
    //Dedicated register must be satefied in the highest priority.
    void assignDedicatedLT(Pos curpos, IR const* ir, LifeTime * lt);

    //The function assigns lt focibly with given reg.
    void forceAssignRegister(LifeTime const* lt, Reg reg);

    REG_PREFER const getLTPrefer(LifeTime const* lt) const
    { return m_lt2prefer.get(lt); }

    IR * insertSpillAtEntry(Reg r);
    void insertReloadAtExit(Reg r, Var * spill_loc);
    IRListIter insertSpillAtBBEnd(IR * spill, IRBB * bb);
    IRListIter insertReloadAtBB(IR * reload, IRBB * bb, bool start);
    IR * insertReloadAtBB(PRNO prno, Var * spill_loc, Type const* ty,
                          IRBB * bb, bool start);
public:
    LSRAImpl(LinearScanRA & ra, RegSetImpl & rsimpl, bool use_expose = false)
        : m_ra(ra), m_rsimpl(rsimpl)
    {
        m_is_insert_bb = false;
        m_use_expose = use_expose;
        m_rg = ra.getRegion();
        m_tm = m_rg->getTypeMgr();
        m_irmgr = m_rg->getIRMgr();
        m_cfg = m_ra.getCFG();
        m_bb_list = m_ra.getBBList();
        m_live_mgr = nullptr;
        m_oc = nullptr;
    }
    ~LSRAImpl() {}
    void computeRAPrefer();
    void computeLTPrefer(LifeTime const* lt);

    void dumpBBList() const;
    void dump() const;
    static void dumpAssign(LSRAImpl & lsra, LifeTime const* lt,
                           CHAR const* format, ...);

    static Var * findSpillLoc(IR const* ir);

    TargInfoMgr & getTIMgr() const { return m_ra.getTIMgr(); }
    LivenessMgr * getLiveMgr() const { return m_live_mgr; }
    LinearScanRA & getRA() const { return m_ra; }
    OptCtx * getOptCtx() const { return m_oc; }
    Region * getRegion() const { return m_rg; }
    LifeTimeMgr & getLTMgr() { return m_ra.getLTMgr(); }
    ActMgr & getActMgr() { return m_ra.getActMgr(); }
    Reg getReg(PRNO prno) const { return m_ra.getReg(prno); }
    Reg getReg(LifeTime const* lt) const { return getReg(lt->getPrno()); }
    CHAR const* getRegName(Reg r) const { return m_ra.getRegName(r); }
    BBList * getBBList() const { return m_bb_list; }
    IRCFG * getCFG() const { return m_cfg; }
    RegSetImpl & getRegSetImpl() const { return m_rsimpl; }
    List<LifeTime const*> const& getSplittedLTList() const
    { return m_splitted_newlt_lst; }
    PRSplitBB getPrSplitBB(Var const* v) const
    {
        PRSplitBB splitbb;
        splitbb.value = m_var2split.get(v);
        return splitbb;
    }

    bool isRematLikeOp(IR const* ir) const;
    static bool isSpillLikeOp(IR const* ir);
    static bool isReloadLikeOp(IR const* ir);

    void insertRematBefore(IR * remat, IR const* marker);
    void insertRematBefore(PRNO newres, RematCtx const& rematctx,
                           Type const* loadvalty, IR const* marker);
    IR * insertMove(PRNO from, PRNO to, Type const* fromty, Type const* toty,
                    IRBB * bb);
    IR * insertSpillAtBBEnd(PRNO prno, Type const* ty, IRBB * bb);
    IR * insertSpillAtBBEnd(PRNO prno, Var * var, Type const* ty, IRBB * bb);
    void insertSpillAtHead(IR * spill, MOD IRBB * bb);
    void insertSpillAfter(IR * spill, IR const* marker);
    IR * insertSpillAfter(PRNO prno, Type const* ty, IR const* marker);
    void insertSpillBefore(IR * spill, IR const* marker);
    IR * insertSpillBefore(PRNO prno, Type const* ty, IR const* marker);
    IR * insertSpillBefore(Reg r, IR const* marker);
    IR * insertReload(PRNO to, Var * v, Type const* ty, IRBB * bb);
    void insertReloadBefore(IR * reload, IR const* marker);
    IR * insertReloadBefore(PRNO newres, Var * spill_loc,
                            Type const* ty, IR const* marker);
    bool perform(OptCtx & oc);

    //Record the newlt that generated by SplitMgr.
    void recordSplittedNewLT(LifeTime const* newlt);
    void recordVar2Split(Var const* v, UINT spill_bbid, UINT reload_bbid)
    {
        PRSplitBB splitbb;
        splitbb.bb.spill_bbid = spill_bbid;
        splitbb.bb.reload_bbid = reload_bbid;
        m_var2split.set(v, splitbb.value);
    }

    //The function check each CFG edge to fixup the lifetime conflict while the
    //linearization allocation flattening the CFG.
    //The function check consistency for each newlt that generated by SplitMgr
    //and insert appropriately store/load/move to guarantee the lifetime
    //consistency.
    void reviseLTConsistency();

    void saveCallee();

    LifeTime * selectAssignDefCand(Pos curpos, IR const* curstmt);
    LifeTime * selectAssignUseCand(Pos curpos, IR const* curstmt,
                                   OUT IR const** curir);

    //lt: split or spill other lifetime to make register for lt.
    //curpos: the position that need a register.
    //curir: the stmt/exp that need a register.
    void solveConflict(LifeTime * lt, Pos curpos, IR const* curir);

    //The function split all lifetimes in Active LifeTime Set that assigned
    //given register 'r' before 'ir'.
    void splitActiveLTWithReg(Pos curpos, IR const* ir, Reg r);
    //Spill LT that assigned referred register in given LTSet.
    void splitAllLTWithReg(Pos curpos, IR const* ir, Reg r,
                           MOD LTSet & set);

    //The function split all lifetime that assigned caller-saved register
    //before call-stmt.
    void splitCallerSavedLT(Pos curpos, IR const* ir);

    //The function split all lifetimes in InActive LifeTime Set that assigned
    //given register 'r' before 'ir'.
    void splitInActiveLTWithReg(Pos curpos, IR const* ir, Reg r);

    //The function split all lifetimes that assigned link register
    //before call-stmt.
    void splitLinkLT(Pos curpos, IR const* ir);

    void transferActive(Pos curpos);
    void transferInActive(Pos curpos);

    bool tryAssignCallee(IR const* ir, LifeTime const* lt);
    bool tryAssignCaller(IR const* ir, LifeTime const* lt);
    bool tryAssignDedicatedRegister(LifeTime const* lt);

    //Try assign register for given ir which at 'pos'.
    //ir: may be expression or stmt.
    //lt: lifetime that corresponding to 'ir'.
    void tryAssignRegForIR(Pos pos, IR const* ir, LifeTime * lt);

    //Default register assign oder.
    bool tryAssignRegister(IR const* ir, LifeTime const* lt);

    bool tryAssignRegisterDefault(IR const* ir, LifeTime const* lt);
    bool tryAssignRegisterByPrefer(IR const* ir, LifeTime const* lt);

    void tryUpdateRPO(OUT IRBB * newbb, OUT IRBB * tramp, IRBB const* marker,
                      bool newbb_prior_marker);
    void tryUpdateDom(IRBB const* newbb, IRBB const* marker);
    void tryUpdateLiveness(IRBB const* newbb, IRBB const* marker);
};
//END LSARImpl

} //namespace xoc
#endif
