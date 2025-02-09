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
    tagVexPair(VexIdx t) { fromid = t; toid = t; }
    tagVexPair() { fromid = VERTEX_UNDEF; toid = VERTEX_UNDEF; }
} VexPair;
class CompareVexPair {
public:
    bool is_less(VexPair t1, VexPair t2) const
    {
        return (t1.fromid < t2.fromid) ||
               (t1.fromid == t2.fromid && t1.toid < t2.toid);
    }
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
    COPY_CONSTRUCTOR(LTConsistencyMgr);
protected:
    typedef TMap<VexPair, IRBB*, CompareVexPair> LatchMap;
    typedef TMapIter<VexPair, IRBB*> LatchMapIter;
    typedef TMap<PRNO, LifeTime const*> PR2LT;
    typedef TMapIter<PRNO, LifeTime const*> PR2LTIter;
    bool m_is_insert_bb;
    Region * m_rg;
    BBList * m_bb_list;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    LivenessMgr * m_live_mgr;
    LSRAImpl & m_impl;
    Vector<PR2LT*> m_pr2lt_in_vec;
    Vector<PR2LT*> m_pr2lt_out_vec;
protected:
    PR2LT * genInPR2LT(UINT bbid)
    {
        PR2LT * tab = m_pr2lt_in_vec.get(bbid);
        if (tab == nullptr) {
            tab = new PR2LT();
            m_pr2lt_in_vec.set(bbid, tab);
        }
        return tab;
    }
    PR2LT * genOutPR2LT(UINT bbid)
    {
        PR2LT * tab = m_pr2lt_out_vec.get(bbid);
        if (tab == nullptr) {
            tab = new PR2LT();
            m_pr2lt_out_vec.set(bbid, tab);
        }
        return tab;
    }


    void addInPR2Lt(PRNO prno, LifeTime const* lt, UINT bbid)
    { genInPR2LT(bbid)->setAlways(prno, lt); }

    void addOutPR2Lt(PRNO prno, LifeTime const* lt, UINT bbid)
    { genOutPR2LT(bbid)->setAlways(prno, lt); }

    void computePR2LtInfo();

    //This function is used to compute the PR2LT at the input boundary
    //or output boundary of the specified BB.
    //bbid: the id for the specified BB.
    //is_input: true stands for the input boundary mode, and false means
    //the output boundary mode.
    void computePR2LtInfoForBB(UINT bbid, bool is_input);

    LifeTime const* getLifetimeChild(LTList const& lt_list, Pos pos);

    void computeEdgeConsistency(OUT InConsistPairList & inconsist_lst);
    void computeEdgeConsistencyImpl(xcom::Edge const* e,
                                    OUT InConsistPairList & inconsist_lst);

    //This function implements the inconsistency check based on forward
    //analysis.
    //anct: the ancestor lifetime of the PR.
    //newlt: the new lifetime of the PR after splited.
    //from: the source BBID of the edge in CFG.
    //to: the destination BBID of the edge in CFG.
    //spill_loc: the memory location for the spilling operation.
    //inconsist_lst: the list to record the inconsistency information.
    void computeForwardConsistencyImpl(LifeTime const* anct,
        LifeTime const* newlt, UINT from, UINT to, Var const* spill_loc,
        OUT InConsistPairList & inconsist_lst);

    //This function implements the inconsistency check based on backward
    //analysis.
    //anct: the ancestor lifetime of the PR.
    //newlt: the new lifetime of the PR after splited.
    //from: the source BBID of the edge in CFG.
    //to: the destination BBID of the edge in CFG.
    //spill_loc: the memory location for the spilling operation.
    //inconsist_lst: the list to record the inconsistency information.
    void computeBackwardConsistencyImpl(LifeTime const* anct,
        LifeTime const* newlt, UINT from, UINT to, Var const* spill_loc,
        OUT InConsistPairList & inconsist_lst);

    void dumpBBAfterReorder(IRBB const* bb) const;

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
    LinearScanRA & getRA() const;
    PR2LT * getInPR2Lt(UINT bbid) const
    { return m_pr2lt_in_vec.get(bbid); }
    PR2LT * getOutPR2Lt(UINT bbid) const
    { return m_pr2lt_out_vec.get(bbid); }

    LifeTime const* getInLt(PRNO prno, UINT bbid) const
    {
        PR2LT * tab = getInPR2Lt(bbid);
        if (tab == nullptr) { return nullptr; }
        ASSERT0(tab);
        return tab->get(prno);
    }
    LifeTime const* getOutLt(PRNO prno, UINT bbid) const
    {
        PR2LT * tab = getOutPR2Lt(bbid);
        if (tab == nullptr) { return nullptr; }
        ASSERT0(tab);
        return tab->get(prno);
    }

    IRBB * insertLatch(IRBB const* from, MOD IRBB * to);

    //This func will check the reorder for the MOV IRs is required or not in
    //the specified BB, and also collect some infomations used in the coming
    //reorder phase.
    //bb: the specified latch BB.
    //reg_use_cnt: the register use count for the registers involved in MOV IRs.
    //move_info: contains the source and destination registers for the MOV IRs.
    //def_irs: contains the original MOV IR responding to the defined register.
    //marker: the marker IR indicates where the adjusted IR should be inserted.
    //return value: true if it is needed to do the reorder; false means don't
    //              need to do reorder.
    //For example:
    //  The group of MOV IRs are in the order below:
    //    $10 <- MOV $8    #S1
    //    $7  <- MOV $10   #S2
    //
    //  Because $10 is used in #S2, but before it is used, it will be
    //  overwrote by $8 in #S1, so this two MOV IRs need to be reordered,
    //  The expected sequence should be:
    //    $7  <- MOV $10   #S2
    //    $10 <- MOV $8    #S1
    //
    bool isReorderRequired(MOD IRBB * bb, OUT UINT *& reg_use_cnt,
                           OUT UINT *& move_info, OUT IR **& def_irs,
                           OUT IR *& marker);

    //This func will reorder the Move IRs inserted at the all latch BBs.
    void reorderMoveIRInLatchBB(MOD LatchMap & latch_map);

    //This func will do the reorder for the MOV IRs in the latch BB.
    //bb: the specified latch BB.
    //reg_use_cnt: the register use count for the registers involved in MOV IRs.
    //move_info: contains the source and destination registers for the MOV IRs.
    //def_irs: contains the original MOV IR responding to the defined register.
    //marker: the marker IR indicates where the adjusted IR should be inserted.
    void reorderMoveIRForBB(MOD IRBB * bb, MOD UINT *& reg_use_cnt,
        MOD UINT *& move_info, IN IR ** def_irs, IR const* marker);

    void reviseEdgeConsistency(InConsistPairList const& inconsist_lst);
    void reviseTypeMEM2PR(MOD LatchMap & latch_map, InConsistPair const& pair);
    void reviseTypePR2MEM(MOD LatchMap & latch_map, InConsistPair const& pair);
    void reviseTypePR2PR(MOD LatchMap & latch_map, InConsistPair const& pair);

    //Select the correct lifetime derived from an ancestor lifetime at a
    //specified position.
    //antc: the ancestor lifetime.
    //pos: the specified position used to determine which descendant lifetime
    //     will be used.
    //lt: the output lifetime choosed.
    //return: true if a lifetime is choosed; false if the lifetime is in the
    //        memory.
    bool selectLifetimeAtPos(LifeTime * anct, Pos pos, OUT LifeTime const*& lt);

    //This function will verify the reorder result.
    bool verifyReorderResult(UINT const* move_info, UINT max_reg_num) const;

    //This function will verify the swap condition.
    bool verifySwapCondition(PRNO prno1, PRNO prno2, Type const* ty1,
                             Type const* ty2) const;
public:
    LTConsistencyMgr(LSRAImpl & impl);
    ~LTConsistencyMgr()
    {
        for (UINT i = 0; i < m_pr2lt_in_vec.get_elem_count(); i++) {
            PR2LT * lt2st = m_pr2lt_in_vec.get(i);
            if (lt2st != nullptr) { delete lt2st; }
            lt2st = m_pr2lt_out_vec.get(i);
            if (lt2st != nullptr) { delete lt2st; }
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

    bool isUsedBySuccessors(PRNO prno, SplitCtx const& ctx);

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

    //The function selects a lifetime from 'lst' which has the least priority
    //and the next-occ from given position is the furthest.
    //e.g: given pos is 10, and two lifetimes with the same priority in 'lst'.
    //    lt1:  <5-40>, next-occ is in 20
    //    lt2:  <5-25>, next-occ is in 25
    // the function return lt2.
    //Return nullptr if there is no lifetime has next-occ.
    LifeTime * selectLTByPrioAndNextOcc(LTSet const& lst, Pos pos,
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

    //Checks if the lifetime of the current PR conflicts
    //with the target lifetime.
    bool isConflictedWithTargetLT(LifeTime const* target_lt, PRNO cur_pr);

    //Spill the lifetime only if there is no occurence after the split
    //position.
    void spillForced(LifeTime * lt, MOD SplitCtx & ctx);

    //Split lt into two lifetime according to the information given in 'ctx'.
    //Note after splitting the second half of 'lt' will be renamed to a newlt.
    //Return the newlt.
    LifeTime * splitAt(LifeTime * lt, MOD SplitCtx & ctx);

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
    typedef xcom::TMap<PRNO, UINT64> PR2Split;
private:
    COPY_CONSTRUCTOR(LSRAImpl);
protected:
    bool m_is_insert_bb;
    bool m_use_expose;
    bool m_is_dominfo_valid;
    LinearScanRA & m_ra;
    RegSetImpl & m_rsimpl;
    Region * m_rg;
    TypeMgr * m_tm;
    IRMgr * m_irmgr;
    LivenessMgr * m_live_mgr;
    BBList * m_bb_list;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    ArgPasser * m_argpasser;
    xcom::List<LifeTime const*> m_splitted_newlt_lst;
    LT2Prefer m_lt2prefer;
    PR2Split m_pr2split;
protected:
    //Dedicated register must be satefied in the highest priority.
    void assignDedicatedLT(Pos curpos, IR const* ir, LifeTime * lt);

    //The function assigns lt focibly with given reg.
    void forceAssignRegister(LifeTime const* lt, Reg reg);

    REG_PREFER const getLTPrefer(LifeTime const* lt) const
    { return m_lt2prefer.get(lt); }
    ArgPasser * getArgPasser();

    IR * insertSpillCalleeAtEntry(Reg r);
    void insertReloadCalleeAtExit(Reg r, Var * spill_loc);
    IRListIter insertSpillAtBBEnd(IR * spill, IRBB * bb);
    IRListIter insertReloadAtBB(IR * reload, IRBB * bb, bool start);
    IR * insertReloadAtBB(PRNO prno, Var * spill_loc, Type const* ty,
                          IRBB * bb, bool start);
public:
    LSRAImpl(LinearScanRA & ra, RegSetImpl & rsimpl, bool use_expose = false)
        : m_is_dominfo_valid(true), m_ra(ra), m_rsimpl(rsimpl)
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
        m_argpasser = nullptr;
    }
    ~LSRAImpl() {}

    //Add liveness info for an new latch BB.
    //latch_bb: the new latch BB.
    //from: the predecessor BB of the new latch_BB.
    void addLivenessForEmptyLatchBB(IRBB const* latch_bb, IRBB const* from);

    void computeRAPrefer();
    void computeLTPrefer(LifeTime const* lt);

    //This func will check the force-reload is necessary or not first, and
    //then insert the reload IR for lt before the curir. Normally, it is
    //need to do the force reload operation when the lt and cand are both
    //livein to the entry of the destination BB of backward edge.
    //lt: the lifetime need to be checked and inserted with the reload IR.
    //curpos: the position of current IR using the lt.
    //curir: the IR using the lt.
    //cand: the candidate lifetime that selected by the process of
    //      solveConflict when assigned the register for lt.
    void checkAndDoForceReload(LifeTime * lt, Pos curpos, IR const* curir,
                               LifeTime const* cand);

    void dumpBBList() const;
    void dump() const;
    static void dumpAssign(LSRAImpl & lsra, LifeTime const* lt,
                           CHAR const* format, ...);

    static Var * findSpillLoc(IR const* ir);

    TargInfoMgr & getTIMgr() const
    { return *(m_rg->getRegionMgr()->getTargInfoMgr()); }
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

    PRSplitBB getPrSplitBB(PRNO v) const
    {
        PRSplitBB splitbb;
        splitbb.value = m_pr2split.get(v);
        return splitbb;
    }

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
    IR * insertReload(PRNO to, Var * v, Type const* ty, IRBB * bb);
    void insertReloadBefore(IR * reload, IR const* marker);
    IR * insertReloadBefore(PRNO newres, Var * spill_loc,
                            Type const* ty, IR const* marker);

    bool isRematLikeOp(IR const* ir) const;
    static bool isSpillLikeOp(IR const* ir);
    static bool isReloadLikeOp(IR const* ir);
    bool isDomInfoValid() const { return m_is_dominfo_valid; }
    bool isLTUsedInFakeOp(LifeTime const* lt) const
    {
        ASSERT0(lt);
        OccList & lt_occ_list = const_cast<LifeTime*>(lt)->getOccList();
        IR * lt_first = lt_occ_list.get_head().getIR();
        ASSERT0(lt_first);
        IR * lt_stmt = lt_first->is_stmt() ? lt_first : lt_first->getStmt();
        ASSERT0(lt_stmt);
        return getRA().isFakeUseAtLexFirstBBInLoop(lt_stmt);
    }

    //Return true if the split position of the specified lifetime is before
    //the fake-use IR at the last BB of loop by lexicographical order.
    //lt: the lifetime will be split.
    //split_pos: the split position
    //e.g: The #S4 is a fake-use IR of $1, it is located at the last BB of
    //     loop by lexicographical order. The split position is right before
    //     this fake-use IR #S4 and after the normal IR #S2.
    //       ----->BB1
    //      |      $1<-x               #S1
    //      |      |
    //      |      V
    //      |      BB2
    //      |      |
    //      |      V
    //      |      BB3
    //      |      $4<-mov $1          #S2
    //      |      |
    //      |      V
    //      |      BB4
    //      |      $6<-$3              #S3     <-- split_pos
    //      |      [mem]<-fake_use $1  #S4
    //      |      |
    //       ------|
    //             V
    bool isLtSplitBeforeFakeUseAtLexLastBBInLoop(LifeTime const* lt,
                                                 Pos split_pos) const;

    bool perform(OptCtx & oc);

    //Record the newlt that generated by SplitMgr.
    void recordSplittedNewLT(LifeTime const* newlt);
    void recordPR2Split(PRNO prno, UINT spill_bbid, UINT reload_bbid)
    {
        PRSplitBB splitbb;
        splitbb.bb.spill_bbid = spill_bbid;
        splitbb.bb.reload_bbid = reload_bbid;
        m_pr2split.set(prno, splitbb.value);
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
    void setDomInfoValid(bool valid) { m_is_dominfo_valid = valid; }

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

    //This func will select a strategy for the lifetime based on the split
    //position. If the split position of the specified lifetime is before
    //the fake-use IR at the last BB of loop by lexicographical order, the
    //lifetime will be forced to spill, or else it will be split into two
    //lifetimes.
    void splitOrSpillLT(LifeTime * t, Pos split_pos, MOD SplitCtx & ctx,
                        SplitMgr & spltmgr);

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
    void tryUpdateDom(IRBB const* from, IRBB const* newbb,
                      IRBB const* to);
};
//END LSARImpl

} //namespace xoc
#endif
