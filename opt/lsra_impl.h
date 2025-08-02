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
class LSRAPostOpt;

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

    //Inconsistent type for the PR is rematerialized.
    //e.g: $p is split and renamed to $q. For an edge of CFG, $p is used
    //in the source BB, and $p is rematerialized when split, $q is used in the
    //destination BB, the rematerialization of $p and $q are inconsistent
    //because the data of $p is not in $q in source BB, a remat IR need to be
    //inserted to move the data of $p in $q through remat expression.
    INCONSIST_REMAT = 4,
} INCONSIST_TYPE;

//CASE:Define local used type in global scope because some compiler
//complained 'uses local type'.


//
//START VexPair
//
class VexPair {
public:
    VexIdx fromid;
    VexIdx toid;
public:
    VexPair(VexIdx t) { fromid = t; toid = t; }
    VexPair() { fromid = VERTEX_UNDEF; toid = VERTEX_UNDEF; }
    VexPair(VexIdx from, VexIdx to) : fromid(from), toid(to) { }
    //Overload the below operator because this class will be used as the key
    //of xcom::TTab.
    bool operator != (VexPair const& other) const
    { return fromid != other.fromid || toid != other.toid; }
};
//END VexPair

//
//START CompareVexPair
//
class CompareVexPair {
public:
    VexPair createKey(VexPair t) { return t; }
    bool is_less(VexPair t1, VexPair t2) const
    {
        return (t1.fromid < t2.fromid) ||
               (t1.fromid == t2.fromid && t1.toid < t2.toid);
    }
    bool is_equ(VexPair t1, VexPair t2) const
    { return t1.fromid == t2.fromid && t1.toid == t2.toid; }
};
//END CompareVexPair

//
//START InConsistPair
//
class InConsistPair {
public:
    VexIdx from_vex_id;
    VexIdx to_vex_id;
    INCONSIST_TYPE type;
    LifeTime const* from_lt;
    LifeTime const* to_lt;

    //The spilled memory location var.
    Var const* mem_var;

    //Record the remat expression when do the inconsist revise.
    IR const* remat_exp;
public:
    InConsistPair() { init(); }
    InConsistPair(UINT id)  { DUMMYUSE(id); init(); }
    void init()
    {
        from_vex_id = VERTEX_UNDEF;
        to_vex_id = VERTEX_UNDEF;
        from_lt = nullptr;
        to_lt = nullptr;
        mem_var = nullptr;
        type = INCONSIST_UNDEF;
        remat_exp = nullptr;
    }
};
//END InConsistPair

//
//START InConsistPairList
//
typedef List<InConsistPair*>::Iter InConsistPairListIter;
class InConsistPairList : public List<InConsistPair*> {
public:
    void dump(Region const* rg) const
    {
        note(rg, "\n==-- DUMP %s --==", "InConsistPairList");
        InConsistPairListIter it;
        UINT i = 0;
        for (InConsistPair const* pair = get_head(&it);
             i < get_elem_count(); pair = get_next(&it), i++) {
            switch (pair->type) {
            case INCONSIST_PR2PR:
                ASSERT0(pair->from_lt && pair->to_lt);
                note(rg,
                    "\nNEED insert bb between BB%u and BB%u, and move:$%u->$%u",
                    pair->from_vex_id, pair->to_vex_id,
                    pair->from_lt->getPrno(), pair->to_lt->getPrno());
                break;
            case INCONSIST_MEM2PR:
                ASSERT0(pair->mem_var && pair->to_lt);
                note(rg,
                    "\nNEED insert bb between BB%u and BB%u, and copy:$%s->$%u",
                    pair->from_vex_id, pair->to_vex_id,
                    pair->mem_var->get_name()->getStr(),
                    pair->to_lt->getPrno());
                break;
            case INCONSIST_PR2MEM:
                ASSERT0(pair->mem_var && pair->from_lt);
                note(rg,
                    "\nNEED insert bb between BB%u and BB%u, and copy:$%u->$%s",
                    pair->from_vex_id, pair->to_vex_id,
                    pair->from_lt->getPrno(),
                    pair->mem_var->get_name()->getStr());
                break;
            case INCONSIST_REMAT:
                ASSERT0(pair->remat_exp && pair->to_lt);
                note(rg,
                    "\nNEED insert bb between BB%u and BB%u, remat:id$%u->$%u",
                    pair->from_vex_id, pair->to_vex_id, pair->remat_exp->id(),
                    pair->to_lt->getPrno());
                break;
            default: UNREACHABLE(); break;
            }
        }
    }
};
//END InConsistPairList

//
//START LTConsistencyMgr
//
typedef TMap<VexPair, IRBB*, CompareVexPair> LatchMap;
typedef TMapIter<VexPair, IRBB*> LatchMapIter;
class LTConsistencyMgr {
    COPY_CONSTRUCTOR(LTConsistencyMgr);
protected:
    typedef TTab<VexPair, CompareVexPair> InConsistVexPairTab;
    typedef TTabIter<VexPair> InConsistVexPairTabIter;
    typedef TMap<PRNO, LifeTime const*> PR2LT;
    typedef TMapIter<PRNO, LifeTime const*> PR2LTIter;
    bool m_is_insert_bb;
    Region * m_rg;
    BBList * m_bb_list;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    LivenessMgr * m_live_mgr;
    SMemPool * m_pool;
    LSRAImpl & m_impl;

    //Used to store the info of inconsistent edge, which use the composition
    //of bbid of 'from' BB and 'to' BB as the key of table.
    InConsistVexPairTab m_inconsist_vexpair_tab;
    Vector<PR2LT*> m_pr2lt_in_vec;
    Vector<PR2LT*> m_pr2lt_out_vec;
protected:
    void addInconsitVexpairTab(VexPair const& pair)
    { m_inconsist_vexpair_tab.append(pair); }
    InConsistPair * allocInconsistPair()
    { return (InConsistPair*)xmalloc(sizeof(InConsistPair)); }
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

    //Return true if the lifetime do not need to do the inconsist check.
    bool canIngoreInconsistCheck(LifeTime const* lt) const
    {
        ASSERT0(lt);
        //If the lifetime is not split or spilled or not rematerialized,
        //don't consider the inconsistency.
        LTList const& lt_list = const_cast<LifeTime*>(lt)->getChild();
        Var const* spill_loc = getRA().getSpillLoc(lt->getPrno());
        return lt_list.get_elem_count() == 0 && spill_loc == nullptr &&
               !lt->isRematerialized();
    }

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

    //This function is used to compute the inconsisteny info for the lifetimes
    //descendanted from the same ancestor liftime on a specified edge (
    //indicated by a 'from' BB and a 'to' BB).
    //inconsist_lst: the updated inconsistency list.
    //from: the start BB id of 'from' BB.
    //to: the start BB id of 'to' BB.
    //from_lt: the lifetime covered the out boundary of 'from' BB.
    //to_lt: the lifetime covered the in boundary of 'to' BB.
    //spill_loc: the spill location of the lifetime.
    void computeInconsistency(OUT InConsistPairList & inconsist_lst, UINT from,
        UINT to, LifeTime const* from_lt, LifeTime const* to_lt,
        Var const* spill_loc);

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
    IRBB * genLatchBB(MOD LatchMap & latch_map, InConsistPair const* pair);
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

    //Get the first real occ of the lifetime if it is used in the fake-use IR.
    Occ getFirstRealOccOfFakeUseAtLexFirstBBInLoop(LifeTime * lt);

    IRBB * insertLatch(IRBB const* from, MOD IRBB * to,
                       MOD LatchMap & latch_map);
    bool isLatchBBRequired(VexPair const& pair) const
    { return m_inconsist_vexpair_tab.find(pair); }

    bool isLTSplitBetweenLeadingFakeUseAndRealUse(LifeTime const* lt) const
    {  ASSERT0(lt); return lt->getOccNum() == 1; }

    void reviseEdgeConsistency(InConsistPairList const& inconsist_lst);
    void reviseTypeMEM2PR(MOD LatchMap & latch_map, InConsistPair const* pair);
    void reviseTypePR2MEM(MOD LatchMap & latch_map, InConsistPair const* pair);
    void reviseTypePR2PR(MOD LatchMap & latch_map, InConsistPair const* pair);
    void reviseTypeRemat(MOD LatchMap & latch_map, InConsistPair const* pair);

    //Select the correct lifetime derived from an ancestor lifetime at a
    //specified position.
    //antc: the ancestor lifetime.
    //pos: the specified position used to determine which descendant lifetime
    //     will be used.
    //lt: the output lifetime choosed.
    //return: true if a lifetime is choosed; false if the lifetime is in the
    //        memory.
    bool selectLifetimeAtPos(LifeTime * anct, Pos pos, OUT LifeTime const*& lt);

    //This func selects the proper lifetime for the lifetime that has been
    //used in the fake-use IR at the first BB of loop in lexicographical order.
    //antc: the ancestor lifetime.
    //pos: the specified position used to determine which descendant lifetime
    //     will be used.
    //lt: the output lifetime choosed.
    //return: true if a lifetime is choosed; false if the lifetime is in the
    //        memory.
    bool selectLTAtPosForFakeUseAtLexFirstBBOfLoop(LifeTime * anct, Pos pos,
                                                   OUT LifeTime const*& lt);

    //Try to use the tramp BB as a latch BB if it is inconsistent of a
    //lifetime between the precedessor BB and the successor BB of the
    //tramp BB.
    //tramp: the tramp BB.
    //latch_map: the map recording the latch BB info.
    //e.g:
    //  CFG before the tramp BB generated:
    //   BB1
    //    |
    //    V
    //   BB2<---.
    //    |     |
    //    V     |
    //   BB3    |
    //    |_____|
    //    |
    //    V
    //
    //  CFG after the tramp BB generated:
    //   BB1
    //    |
    //    V
    //   BB_tramp
    //    |
    //    V
    //   BB2<---.
    //    |     |
    //    V    BB_latch
    //   BB3    |
    //    |_____|
    //    |
    //    V
    //  BB_tramp is generated during the insertion of latch BB BB_latch
    //  between BB3 and BB2. The BB_tramp can be used as the latch BB if the
    //  latch BB is really needed to be inserted between BB1 and BB2.
    void tryUseTrampBBAsLatchBB(IRBB const* tramp, MOD LatchMap & latch_map);

    //Verify the latch BB after the latch BB are generated.
    bool verifyLatchBBs(LatchMap const& latch_map) const;

    //Verify the latch BB by checking the following two parts:
    //    1. The count of the physical registers used in the spill IRs.
    //        If the count if more than 1, that means there is an error
    //        in the previous lsra phase.
    //    1. The count of the physical registers defined in the
    //       reload/remat/move IRs. If the count if more than 1, that means
    //       there is an error in the previous lsra phase.
    bool verifyLatchBB(IRBB * bb, MOD Vector<BYTE> & use_reg_cnt,
                       MOD Vector<BYTE> & def_reg_cnt) const;

    void * xmalloc(size_t size)
    {
        ASSERTN(m_pool != nullptr, ("pool does not initialized"));
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p != nullptr);
        ::memset(p, 0, size);
        return p;
    }
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
        if (m_pool != nullptr) {
            smpoolDelete(m_pool);
            m_pool = nullptr;
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
    IN LifeTime const* alloc_lt;

    //Bottom-up propagate information.
    OUT Pos reload_pos;

    //Bottom-up propagate information.
    OUT Occ reload_occ;
public:
    SplitCtx(Pos p)
    {
        split_pos = p;
        split_pos_ir = nullptr;
        alloc_lt = nullptr;
        reload_pos = POS_UNDEF;
        reload_occ = Occ(POS_UNDEF);
    }
    SplitCtx(Pos p, IR const* o, LifeTime const* lt)
    {
        split_pos = p;
        split_pos_ir = o;
        alloc_lt = lt;
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
        bool canberemat, RematCtx const& rematctx, OUT IR *& spill);
    void insertReloadDuringSplitting(LifeTime * lt, LifeTime * newlt,
        SplitCtx const& ctx, bool canberemat, RematCtx const& rematctx,
        IR * spill, OUT IR *& reload);

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
    //This function determines a lifetime can be a candidate during the split
    //based on the split context and force spill only flag.
    //lt: the lifetime used to check duing the split.
    //ctx: the split context including the split position and the liftime to
    //     to be assigned a physical register.
    //force_spill_only: the flag to indicate the force to select a candidate
    //     to spill only. Usually this flag is set to true after the fail to
    //     try to select a candidate for the target lifetime, we need to adopt
    //     this force to spill only strategy during the candidate selection.
    //return true if the 'lt' can be used as a candidate, or else return false.
    bool isCandidateForSplit(LifeTime const* lt, MOD SplitCtx & ctx,
        bool force_spill_only);

    LifeTime * selectSplitCandFromActive(LifeTime * lt, bool tryself,
                                         OUT SplitCtx & ctx);
    LifeTime * selectSplitCandFromInActive(LifeTime * lt, bool tryself,
                                           OUT SplitCtx & ctx);

    //The function selects a lifetime from 'lst' which the next-range from
    //the range of 'pos' is the furthest.
    //e.g:given pos is 10, and there are two lifetime in 'lst'.
    //    lt1:  <10-20>,<30-40>
    //    lt2:  <5-11>,<60-40>
    // the function return lt2.
    //Return nullptr if there is no lifetime has next-range.
    //pos: the position that begin to seek.
    LifeTime * selectLTByFurthestNextRange(LTSet const& lst, Pos pos,
                                           OUT Occ & reload_occ);

    //Select the lifetime to be splitted based on multiple strategies.
    //1. Loop nesting level. Select the lower.
    //2. Use location. Select the further.
    //3. Use frequency. Select the lower.
    //4. Spillage cost. Select the lower.
    //
    //[PENDING] Experiments show that the point 5 is not a reasonable
    //          optimization point and will not be adopted.
    //5. Lifetime length. Select the higher.
    LifeTime * selectLTBaseMultiStrategies(LTSet const& lst,
        Vector<SplitCtx> const& ctxvec, MOD SplitCtx & cur_ctx);

    //Implementation of lifetime selection strategies.
    void selectLTImpl(OUT Occ & next_occ, OUT LifeTime *& select_lt,
        IN LifeTime *const cur_lt, SplitCtx const& ctx);

    //This function shrinks the split position to the last occ of the lifetime
    //if it is spill only.
    void shrinkSplitPosForSpillOnly(LifeTime * lt, MOD SplitCtx & ctx);

    //This function shrinks the 'lt' forward to the last occ before the
    //split position when it is spill only.
    void shrinkLTForSpillOnly(LifeTime * lt, Pos split_pos,
                              IR const* split_pos_ir);
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
        OUT Pos & reload_pos, OUT Occ & reload_occ, bool force_spill_only);

    //Checks if the lifetime of the current PR conflicts
    //with the target lifetime.
    bool isConflictedWithTargetLT(LifeTime const* target_lt, PRNO cur_pr);

    //Spill the lifetime only if there is no occurence after the split
    //position.
    void spillOnly(LifeTime * lt, MOD SplitCtx & ctx);

    //Split lt into two lifetime according to the information given in 'ctx'.
    //Note after splitting the second half of 'lt' will be renamed to a newlt.
    //Return the newlt.
    LifeTime * splitAt(LifeTime * lt, MOD SplitCtx & ctx);

    //If there is no available register to assign to 'lt', the function try
    //to find alternative lifetime or 'lt' itself to split.
    LifeTime * selectSplitCand(LifeTime * lt, bool tryself, OUT SplitCtx & ctx);

    //This function shall force to select a candidate if there is no available
    //register to assign to 'lt'. Normally this function will be called after
    //the call of function 'selectSplitCand' if there is no candidate found.
    LifeTime * forceSelectSplitCand(LifeTime * lt, OUT SplitCtx & ctx);
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
    LSRAPostOpt * m_post_opt;
    xcom::List<LifeTime const*> m_splitted_newlt_lst;
    LT2Prefer m_lt2prefer;
protected:
    LSRAPostOpt * allocLSRAPostOpt();

    //Pre-assigned register must be satefied in the highest priority.
    void assignPreAssignedLT(Pos curpos, IR const* ir, LifeTime * lt);

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
    LSRAImpl(LinearScanRA & ra, RegSetImpl & rsimpl, bool use_expose = false);
    ~LSRAImpl();

    //Add liveness info for an new latch BB.
    //latch_bb: the new latch BB.
    //from: the predecessor BB of the new latch_BB.
    void addLivenessForEmptyLatchBB(IRBB const* latch_bb, IRBB const* from);

    void computeRAPrefer();
    void computeLTPrefer(LifeTime const* lt);

    void dumpBBList() const;
    void dump() const;
    static void dumpAssign(LSRAImpl & lsra, LifeTime const* lt,
                           CHAR const* format, ...);

    static Var * findSpillLoc(IR const* ir);
    TargInfoMgr & getTIMgr() const
    { return *(m_rg->getRegionMgr()->getTargInfoMgr()); }
    LivenessMgr * getLiveMgr() const { return m_live_mgr; }
    LifeTimeMgr & getLTMgr() { return m_ra.getLTMgr(); }
    LinearScanRA & getRA() const { return m_ra; }
    OptCtx * getOptCtx() const { return m_oc; }
    Region * getRegion() const { return m_rg; }
    ActMgr & getActMgr() { return m_ra.getActMgr(); }
    LSRAPostOpt * getPostOpt() { return m_post_opt; }
    Reg getReg(PRNO prno) const { return m_ra.getReg(prno); }
    Reg getReg(LifeTime const* lt) const { return getReg(lt->getPrno()); }
    CHAR const* getRegName(Reg r) const { return m_ra.getRegName(r); }
    BBList * getBBList() const { return m_bb_list; }
    IRCFG * getCFG() const { return m_cfg; }
    RegSetImpl & getRegSetImpl() const { return m_rsimpl; }
    List<LifeTime const*> const& getSplittedLTList() const
    { return m_splitted_newlt_lst; }

    IR * insertRemat(PRNO to, IR const* exp, Type const* ty, IRBB * bb);
    void insertRematBefore(IR * remat, IR const* marker);
    IR * insertRematBefore(PRNO newres, RematCtx const& rematctx,
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

    //Check whether the IRs are different.
    //NOTE: This interface only checks RematOp, ReloadOp, MoveOp, and SpillOp.
    //Returns true for all other IRs.
    bool isDomInfoValid() const { return m_is_dominfo_valid; }

    //Check the lifetime whether has an occ in the fake-use IR at the head of
    //the first BB in a loop by lexicographical order.
    bool isLTUsedInHeadFakeOp(LifeTime const* lt) const
    {
        ASSERT0(lt);
        OccList & lt_occ_list = const_cast<LifeTime*>(lt)->getOccList();
        IR * lt_first = lt_occ_list.get_head().getIR();
        ASSERT0(lt_first);
        IR * lt_stmt = lt_first->is_stmt() ? lt_first : lt_first->getStmt();
        ASSERT0(lt_stmt);
        return getRA().isFakeUseAtLexFirstBBInLoop(lt_stmt);
    }
    //Check the lifetime whether has an occ in the fake-use IR at the tail of
    //the last BB in a loop by lexicographical order.
    bool isLTUsedInTailFakeOp(LifeTime const* lt) const
    {
        ASSERT0(lt);
        OccList & lt_occ_list = const_cast<LifeTime*>(lt)->getOccList();
        IR * lt_tail = lt_occ_list.get_tail().getIR();
        ASSERT0(lt_tail);
        IR * lt_stmt = lt_tail->is_stmt() ? lt_tail : lt_tail->getStmt();
        ASSERT0(lt_stmt);
        return getRA().isFakeUseAtLexLastBBInLoop(lt_stmt);
    }

    //Return true when the lifetime can do spill only for the lifetime has occ
    //in the fake-use IR at the tail of the last BB in a loop by
    //lexicographical order. Current we have two cases for the spill only.
    //1. the split position of lt' is before the fake-use IR at the last BB
    //   of loop by lexicographical order.
    //2. The split position is at the IR in the last BB of loop by
    //   lexicographical order, that means there is no occ of 'lt' after
    //   the split position, so it can do the spill only.
    //lt: the lifetime will be split.
    //split_pos: the split position
    //e.g. for case 1:
    //     The #S4 is a fake-use IR of $1, it is located at the last BB of
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
    //e.g. for case 2:
    //     The #S4 is a fake-use IR of $1, it is located at the last BB of
    //     loop by lexicographical order. The split position is at IR #S5,
    //     $1 can be forced to spill only before the split position.
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
    //      |      $6<-$3              #S3
    //      |      [mem]<-fake_use $1  #S4
    //      |      |
    //      |      beq $5, 0           #S5     <-- split_pos
    //       ------|
    //             V
    bool isLTCanDoSpillOnly(LifeTime const* lt, Pos split_pos) const;

    //When the lifetime split position is inside the loop, the resulting spill
    //or reload operation will be repeated multiple times, thereby reducing
    //performance.
    //Try moving the split position outside the loop so that spill or reload
    //operation is executed less times to improve performance. For example:
    //
    //Lifetime:  occ0----occ1----occ2----occ3----occ4----split pos----occ5
    //nestlevel:   0       1       0       1       2                    0
    //
    //Normally we would select "occ4" as the split position of the lifetime,
    //but the nest level of "occ4" is 2 and is within the loop. We will try to
    //find occurrence with smaller nest level backward until we find "occ2".
    //Note that the nest levels of "occ0" and "occ2" are both 0, and we select
    //"occ2" which is closer to origin split position.
    //
    void moveSplitPosOutsideLoop(LifeTime * lt, MOD SplitCtx & ctx) const;

    bool perform(OptCtx & oc);

    //Record the newlt that generated by SplitMgr.
    void recordSplittedNewLT(LifeTime const* newlt);

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
    //lifetime will be spilled only, or else it will be split into two
    //lifetimes.
    void splitOrSpillOnly(LifeTime * t, Pos split_pos, MOD SplitCtx & ctx,
                          SplitMgr & spltmgr);

    //Optimize the lifetime split position to improve performance. There may
    //be multiple optimization options possible.
    void splitPosOpt(LifeTime * lt, MOD SplitCtx & ctx);

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

    void tryUpdateRPO(OUT IRBB * newbb, IRBB const* marker,
                      bool newbb_prior_marker);
    void tryUpdateDom(IRBB const* from, IRBB const* newbb,
                      IRBB const* to);
    bool verifyLSRAOverStrict(OptCtx & oc) const;
};
//END LSARImpl


//
//START RegisterVerify
//
//
//This class implements the verification of register allocation algorithms.
//In register allocation, for variables with conflicting lifecycles,
//the same register cannot be allocated. This algorithm mainly verifies this
//and gives warnings for undefined use of variables. The core data of the
//algorithm is input_state, which is a hash map that stores the allocation
//relationship between physical registers and pseudo registers/variables.
//The algorithm starts from the entrance basic block and traverses the entire
//CFG. When processing each basic block BB, each IR in BB is traversed,
//and the allocation of input operands in the IR is compared with input_state.
//If there is inconsistency, it indicates an allocation error.
//Then, the status of the output operands in the IR is updated to input_state.
//input_state is a mapping from a physical register to a virtual register.
//input_state stores the physical register allocation status of the current
//instruction predecessor, such as {r1->$0}, indicating that the physical
//register r1 is assigned to the virtual register $0. When iterating to the
//current instruction, if r1 is assigned to $1, it indicates that the two
//lifetime conflicting variables are evenly distributed to the same physical
//register, which is an error.
//For example:
//                   BB0
//                   $a<-1    assume r1->$a (assign physical register r1 to $a)
//                   $b<-$a*2 assume r2->$b
//                   |
//                   | input_state={r1->$a,r2->$b}
//          -----------------------------
//          |                           |
//          |                           V input_state={r1->$a,r2->$b}
//          |                           BB1
//          |                           $c<-$b/2 assume r3->$c
//          |                           |
//          |                           | $b is the lase occ, so r2->null
//          |                           | input_state={r1->$a,r3->$c}
//          |                           |
//          Vinput_state={r1->$a,r2->$b}|
//          BB2                         |
//          $d<-$a*2 assume r4->$d      |
//          |$a is the last occ         |
//          |input_state={r2->$b,r4->$d}|
//          -----------------------------
//                       |
//                       V
//                       BB3
//                       ret $c+$d
//  Assuming there are four physical registers r1, r2, r3, r4,
//  and the processing order is BB0, BB1, BB2, BB3.
//  (1)At the exit of BB0, input_state is {r1->$a,r2->$b}.
//  (2)At the exit of BB1, input_state is {r1->$a,r3->$c}.
//  (3)At the exit of BB2, input_state is {r2->$b,r4->$d}.
//  (4)When dealing with the successor BB3 of BB2,
//     because BB1 needs to be processed first, the input_state at the entrance
//     of BB3 is {r1->$a,r3->$c}. At this point, it is necessary to compare the
//     state at the BB2 exit with the previously saved state. In this example,
//     there is no contradiction between the two input_state,
//     so the test passed.
//  When processing basic block BB0, ignoring the processing of function
//  parameter registers, input_state is empty. When encountering "$a<-1",
//  input_state becomes {r1->$a}. Currently, when encountering "$b<-$a*2",
//  first check whether the registers assigned in a are consistent with those
//  in input_state. If they are not consistent, an error will be reported.
//  At this point, it is correct. Then update the status of the physical
//  registers allocated by $b to input_state, and input_state
//  will become {r1->$a,r2->$b}.
class RegisterVerify {
    COPY_CONSTRUCTOR(RegisterVerify);
public:
    //A map: physical register -> lifetime/virtual register.
    typedef xcom::Vector<UINT> PhyReg2VirReg;
    typedef xcom::TMap<IRBB const*, xcom::C<IRBB const*>*> IRBB2Holder;
private:
    UINT m_physical_reg_num;
    LinearScanRA & m_lsra;
    RegSetImpl & m_reg_set;
    Region * m_rg;
    IRCFG * m_cfg;

    //Physical Caller Registers.
    xcom::Vector<UINT> m_caller_regs;

    //All blocks that must be processed.
    xcom::EList<IRBB const*, IRBB2Holder> m_work_list;

    //A map: block -> input_state,
    //'state' is a PhyReg2VirReg object that stores the mapping relationship
    //between physical registers and pseudo registers.
    //m_saved_states saves information of previous check.
    xcom::TMap<IRBB const*, PhyReg2VirReg const*> m_saved_states;

    //A map: prno -> last occurrence of ir.
    xcom::Vector<IR const*> m_pr2lastocc;

    //A List: save all input_state pointers.
    xcom::List<PhyReg2VirReg const*> m_input_states;
protected:
    PhyReg2VirReg * allocPhyReg2VirReg(UINT size = 0)
    {
        PhyReg2VirReg * obj = nullptr;
        obj = new RegisterVerify::PhyReg2VirReg(size);
        m_input_states.append_tail(obj);
        return obj;
    }

    //If the mapping of reg->prno is consistent with that in input_date,
    //return false. Otherwise, return true to indicate an error has occurred.
    bool checkState(PhyReg2VirReg const* input_state, Reg reg, PRNO prno) const;

    //Calculate the location of the last occ of a prno.
    void computeLastOcc();

    //Create an input_state object.
    PhyReg2VirReg * createPhyReg2VirReg();

    //Remove some physical registers that do not require verification.
    bool canSkipCheck(Reg reg) const;

    void dumpInitInfo() const;

    void dumpPhyReg2VirReg(PhyReg2VirReg const* input_state) const;

    //Release the space pointed to by input_state.
    void destroyPhyReg2VirReg(PhyReg2VirReg const* input_state) const;

    //Free all the space occupied by input_states.
    void destroyPhyReg2VirRegs() const;

    //Calculate the location of the last occurence of a PR operation on RHS.
    void genLastOccRHS(IR const* ir);

    //Calculate the location of the last occurence of a PR operation on LHS.
    void genLastOccLHS(IR const* ir);

    //Perform a deep copy of the input_state object.
    PhyReg2VirReg * getAndGenPhyReg2VirReg(PhyReg2VirReg const* input_state);

    //Obtain the status at the entrance of the basic block bb.
    PhyReg2VirReg const* getPhyReg2VirReg(IRBB const* bb) const;

    //Determine if 'input' is the last occurence in 'ir'.
    bool isLastOcc(IR const* ir, IR const* input) const;

    //If work_list is empty, return true.
    bool isEmptyWorkList() const;

    //Initialize the caller physical register.
    void initPhysicalRegCaller();

    //Initialize the number of physical registers.
    void initPhysicalRegNum();

    //Get physical register number.
    UINT getPhysicalRegNum() const;

    //Retrieve and delete header.
    IRBB const* popWorkList();

    //Process basic block bb.
    void processBlock(IRBB const* bb);

    //Verify if the status of each IR in the basic block bb is correct.
    void processOperations(IRBB const* bb, MOD PhyReg2VirReg * input_state);

    //Process each operand in ir, and if ir is a function call,
    //update the status of the caller register.
    bool processOperation(IR const* ir, MOD PhyReg2VirReg * input_state);

    //Process all successors of the basic block bb.
    void processSuccessor(IRBB const* bb, PhyReg2VirReg const* input_state);

    //Insert a element 'bb' into the end of the list.
    void pushWorkList(IRBB const* bb);

    //Invalidate all caller save registers.
    void setAllCallerInvalid(MOD PhyReg2VirReg * input_state) const;

    //Set the state at the entrance of the basic block bb to saved_state.
    void setPhyReg2VirRegs(IRBB const* bb, IN PhyReg2VirReg * saved_state);

    //Set the status of input_state and update the prno corresponding to reg.
    void setPhyReg2VirReg(MOD PhyReg2VirReg * input_state, Reg reg, PRNO prno)
        const;

    //Setup input registers (method arguments) for first block.
    void setupInputStateForEntryBB(MOD PhyReg2VirReg * input_state) const;

    //Verify register allocation algorithm from BB 'start'.
    void verify(IRBB const* start);
public:
    RegisterVerify(LinearScanRA & lsra, RegSetImpl & reg_set,
        Region * rg, IRCFG * cfg) : m_lsra(lsra), m_reg_set(reg_set)
    {
        ASSERT0(cfg != nullptr);
        m_cfg = cfg;
        ASSERT0(rg != nullptr);
        m_rg = rg;
        initPhysicalRegNum();
        initPhysicalRegCaller();
    }

    ~RegisterVerify() { destroyPhyReg2VirRegs(); m_saved_states.destroy(); }

    bool verify();
};
//End RegisterVerify

} //namespace xoc
#endif
