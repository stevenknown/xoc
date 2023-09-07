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

class InConsistPair {
public:
    VexIdx from_vex_id;
    VexIdx to_vex_id;
    LifeTime const* from_lt;
    LifeTime const* to_lt;
public:
    InConsistPair() {}
    InConsistPair(UINT id)
    {
        from_vex_id = VERTEX_UNDEF;
        to_vex_id = VERTEX_UNDEF;
        from_lt = nullptr;
        to_lt = nullptr;
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
            ASSERT0(pair.from_lt && pair.to_lt);
            note(rg,
                 "\nNEED insert bb between BB%u and BB%u, and move:$%u->$%u",
                 pair.from_vex_id, pair.to_vex_id,
                 pair.from_lt->getPrno(), pair.to_lt->getPrno());
        }
    }
};

class LTConsistencyMgr {
public:
    typedef enum tagCONSIST_STATUS {
        CONSIST_UNDEF = 0,
        CONSIST_INVALID = 1,
        CONSIST_VALID = 2,
    } CONSIST_STATUS;
protected:
    COPY_CONSTRUCTOR(LTConsistencyMgr);
    Region * m_rg;
    BBList * m_bb_list;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    LSRAImpl & m_impl;
    bool m_is_insert_bb;
    typedef TMapIter<LifeTime const*, CONSIST_STATUS> LT2STIter;
    typedef TMap<LifeTime const*, CONSIST_STATUS> LT2ST;
    Vector<LT2ST*> m_lt2st_in_vec;
    Vector<LT2ST*> m_lt2st_out_vec;
protected:
    void addInStatus(LifeTime const* lt, CONSIST_STATUS st, UINT bbid)
    { genInTab(bbid)->setAlways(lt, st); }
    void addOutStatus(LifeTime const* lt, CONSIST_STATUS st, UINT bbid)
    { genOutTab(bbid)->setAlways(lt, st); }

    void computeLTConsistency();
    void computeEdgeConsistency(OUT InConsistPairList & inconsist_lst);
    void computeEdgeConsistencyImpl(xcom::Edge const* e,
                                    OUT InConsistPairList & inconsist_lst);

    CHAR const* getStName(CONSIST_STATUS st) const
    {
        switch (st) {
        case CONSIST_INVALID: return "invalid";
        case CONSIST_VALID: return "valid";
        default: UNREACHABLE();
        }
        return nullptr;
    }
    LT2ST * genInTab(UINT bbid)
    {
        LT2ST * tab = m_lt2st_in_vec.get(bbid);
        if (tab == nullptr) {
            tab = new LT2ST();
            m_lt2st_in_vec.set(bbid, tab);
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
    LT2ST * getOutTab(UINT bbid) const
    { return m_lt2st_out_vec.get(bbid); }
    CONSIST_STATUS getInSt(LifeTime const* lt, UINT bbid) const
    {
        LT2ST * tab = getInTab(bbid);
        ASSERT0(tab);
        return tab->get(lt);
    }
    CONSIST_STATUS getOutSt(LifeTime const* lt, UINT bbid) const
    {
        LT2ST * tab = getOutTab(bbid);
        ASSERT0(tab);
        return tab->get(lt);
    }

    IRBB * insertLatch(IRBB const* from, MOD IRBB * to);

    void reviseEdgeConsistency(InConsistPairList const& inconsist_lst);
public:
    LTConsistencyMgr(LSRAImpl & impl);
    ~LTConsistencyMgr()
    {
        for (UINT i = 0; i < m_lt2st_in_vec.get_elem_count(); i++) {
            LT2ST * in = m_lt2st_in_vec.get(i);
            if (in != nullptr) { delete in; }
            LT2ST * out = m_lt2st_out_vec.get(i);
            if (out != nullptr) { delete out; }
        }
    }

    void dump() const;

    void perform();
};


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


//
//START LSARImpl
//
//The class implements a default linear-scan algorithm.
class LSRAImpl {
    COPY_CONSTRUCTOR(LSRAImpl);
    bool m_is_insert_bb;
    bool m_use_expose;
    LinearScanRA & m_ra;
    Region * m_rg;
    TypeMgr * m_tm;
    IRMgr * m_irmgr;
    LivenessMgr * m_live_mgr;
    BBList * m_bb_list;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    RegSet m_avail_callee;
    RegSet m_avail_caller;
    RegSet m_avail_param;
    RegSet m_avail_return_value;
    RegSet m_avail_allocable;
    RegSet m_used_callee; //record the used callee-saved register.
    xcom::List<LifeTime const*> m_splitted_newlt_lst;
private:
    //Dedicated register must be satefied in the highest priority.
    void assignDedicatedLT(Pos curpos, IR const* ir, LifeTime * lt);

    void computeUsedCaller(OUT RegSet & used);

    //The function assigns lt focibly with given reg.
    void forceAssignRegister(LifeTime const* lt, Reg reg);
    void freeReg(Reg reg);
    void freeReg(LifeTime const* lt);

    void initRegSet();
    IR * insertSpillAtEntry(Reg r);
    void insertReloadAtExit(Reg r, Var * spill_loc);
    IR * insertSpillAtBBEnd(PRNO prno, Type const* ty, IRBB * bb);
    IRListIter insertSpillAtBBEnd(IR * spill, IRBB * bb);
    IRListIter insertReloadAtBB(IR * reload, IRBB * bb, bool start);
    IR * insertReloadAtBB(PRNO prno, Var * spill_loc, Type const* ty,
                          IRBB * bb, bool start);

    //Pick reg from all allocable register sets.
    void pickRegFromAllocable(Reg reg);

    //Record the allocation of callee.
    void recordUsedCallee(Reg r) { m_used_callee.bunion(r); }

    void saveCallee();
public:
    LSRAImpl(LinearScanRA & ra, bool use_expose = false) : m_ra(ra)
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
        initRegSet();
    }
    ~LSRAImpl() {}

    void dumpAvailRegSet() const;
    void dumpBBList() const;
    void dump() const;

    static Var * findSpillLoc(IR const* ir);

    RegSet const& getAvailAllocable() const { return m_avail_allocable; }
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
    List<LifeTime const*> const& getSplittedLTList() const
    { return m_splitted_newlt_lst; }

    bool isRematLikeOp(IR const* ir) const;
    static bool isSpillLikeOp(IR const* ir);
    static bool isReloadLikeOp(IR const* ir);
    bool isAvailAllocable(Reg r) const
    { return m_avail_allocable.is_contain(r); }
    void insertRematBefore(IR * remat, IR const* marker);
    void insertRematBefore(PRNO newres, RematCtx const& rematctx,
                           Type const* loadvalty, IR const* marker);
    IR * insertMove(PRNO from, PRNO to, Type const* fromty, Type const* toty,
                    IRBB * bb);
    void insertSpillAtHead(IR * spill, MOD IRBB * bb);
    void insertSpillAfter(IR * spill, IR const* marker);
    IR * insertSpillAfter(PRNO prno, Type const* ty, IR const* marker);
    void insertSpillBefore(IR * spill, IR const* marker);
    IR * insertSpillBefore(PRNO prno, Type const* ty, IR const* marker);
    IR * insertSpillBefore(Reg r, IR const* marker);
    void insertReloadBefore(IR * reload, IR const* marker);
    IR * insertReloadBefore(PRNO newres, Var * spill_loc,
                            Type const* ty, IR const* marker);

    //Record the newlt that generated by SplitMgr.
    void recordSplittedNewLT(LifeTime const* newlt);

    //The function check each CFG edge to fixup the lifetime conflict while the
    //linearization allocation flattening the CFG.
    //The function check consistency for each newlt that generated by SplitMgr
    //and insert appropriately store/load/move to guarantee the lifetime
    //consistency.
    void reviseLTConsistency();

    void tryUpdateRPO(OUT IRBB * newbb, OUT IRBB * tramp, IRBB const* marker);
    void tryUpdateDom(IRBB const* newbb, IRBB const* marker);
    void tryUpdateLiveness(IRBB const* newbb, IRBB const* marker);
    void transferActive(Pos curpos);
    void transferInActive(Pos curpos);
    bool tryAssignDedicatedRegister(LifeTime const* lt);
    bool tryAssignCallee(LifeTime const* lt);
    bool tryAssignCaller(LifeTime const* lt);
    bool tryAssignRegister(LifeTime const* lt);

    //Try assign register for given ir which at 'pos'.
    //ir: may be expression or stmt.
    //lt: lifetime that corresponding to 'ir'.
    void tryAssignRegForIR(Pos pos, IR const* ir, LifeTime * lt);

    //Spill LT that assigned referred register in given LTSet.
    void splitAllLTWithReg(Pos curpos, IR const* ir, Reg r,
                           MOD LTSet & set);
    //The function split all lifetime that assigned caller-saved register
    //before call-stmt.
    void splitCallerSavedLT(Pos curpos, IR const* ir);

    //The function split all lifetimes that assigned link register
    //before call-stmt.
    void splitLinkLT(Pos curpos, IR const* ir);

    //The function split all lifetimes in Active LifeTime Set that assigned
    //given register 'r' before 'ir'.
    void splitActiveLTWithReg(Pos curpos, IR const* ir, Reg r);

    //The function split all lifetimes in InActive LifeTime Set that assigned
    //given register 'r' before 'ir'.
    void splitInActiveLTWithReg(Pos curpos, IR const* ir, Reg r);

    //lt: split or spill other lifetime to make register for lt.
    //curpos: the position that need a register.
    //curir: the stmt/exp that need a register.
    void solveConflict(LifeTime * lt, Pos curpos, IR const* curir);
    LifeTime * selectAssignUseCand(Pos curpos, IR const* curstmt,
                                   OUT IR const** curir);
    LifeTime * selectAssignDefCand(Pos curpos, IR const* curstmt);

    bool perform(OptCtx & oc);
};
//END LSARImpl

} //namespace xoc
#endif
