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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _LIFETIME_H_
#define _LIFETIME_H_

namespace xoc {

#define POS_UNDEF 0
#define POS_INIT_VAL 1

class LifeTime;
class LifeTimeMgr;
class LinearScanRA;

typedef UINT32 Pos;
typedef xcom::TMapIter<PRNO, Reg> PRNO2RegIter;
typedef xcom::TMap<PRNO, Reg> PRNO2Reg;

typedef List<LifeTime*>::Iter LTListIter;
class LTList : public List<LifeTime*> {
public:
    void dump(Region const* rg) const;
};

class DedicatedMgr : public PRNO2Reg {
public:
    //prno: indicates the PR that is dedicated.
    //antireg: the target-machine register that 'prno' anticipated.
    void add(PRNO prno, Reg antireg) { set(prno, antireg); }
    void dump(Region const* rg, TargInfoMgr const& timgr) const;
    bool is_dedicated(PRNO prno) const { return find(prno); }
};

class UpdatePos {
    bool m_use_expose;
    Pos m_pos;
    LinearScanRA const& m_ra;
private:
    //Increase update position while building lifetime.
    void update(OUT Pos & defpos, OUT Pos & usepos)
    {
        usepos = m_pos++;
        defpos = m_pos++;
    }
public:
    UpdatePos(LinearScanRA const& ra) : m_ra(ra)
    { m_use_expose = true; m_pos = POS_INIT_VAL; }
    Pos getPos() const { return m_pos; }
    LinearScanRA const& getRA() const { return m_ra; }

    //Decrease position in step.
    static void dec(MOD Pos & pos) { pos--; }

    //Decrease position to lastest DEF.
    //e.g:given pos is 15 or 16, the output pos is 14.
    //  [14] <= [13]
    //  [16] <= [15]
    static void decToLastDef(MOD Pos & pos);

    //Decrease position to lastest DEF.
    //e.g:given pos is 16, the output pos is 15.
    //  [14] <= [13]
    //  [16] <= [15]
    static void decToLastUse(MOD Pos & pos);

    //Increase position to next DEF.
    //e.g:given pos is 16, the output pos is 16.
    //  [16] <= [15]
    //e.g2:given pos is 15, the output pos is 16.
    //  [16] <= [15]
    static void incToNextDef(MOD Pos & pos);

    //Increase position in step.
    static void inc(MOD Pos & pos) { pos++; }

    //Return true if pos indicates LHS.
    static bool isDef(Pos pos);

    //Return true if pos indicates RHS.
    static bool isUse(Pos pos);

    //Return true if user expect to generate Def/Use position at exposed-use
    //and exposed-def.
    bool useExpose() const { return m_use_expose; }
    bool updateAtRegionEntry(OUT Pos & dpos, OUT Pos & upos);
    bool updateAtRegionExit(OUT Pos & dpos, OUT Pos & upos);
    bool updateAtBBEntry(OUT Pos & dpos, OUT Pos & upos);
    bool updateAtBBExit(OUT Pos & dpos, OUT Pos & upos);
    bool updateAtIR(IR const* ir, OUT Pos & dpos, OUT Pos & upos);

    void setUseExpose(bool use_exp) { m_use_expose = use_exp; }
};


#define RG_start(r) ((r).m_start)
#define RG_end(r) ((r).m_end)
class Range {
public:
    Pos m_start;
    Pos m_end;
public:
    //Range is point.
    Range() : m_start(0), m_end(0) {}
    Range(Pos p) : m_start(p), m_end(p) {}
    Range(Pos s, Pos e) : m_start(s), m_end(e) {}

    void dump(Region const* rg) const;
    void dumpG(Pos start, Region const* rg) const;
    bool is_less(Range const src) const { return end() < src.start(); }
    bool is_great(Range const src) const { return start() > src.end(); }
    bool is_contain(Pos pos) const { return start() <= pos && end() >= pos; }
    //Return true if current range intersects with src.
    //e.g:four cases of intersection:
    // cur: |----|
    // src:    |---|
    //
    // cur:   |----|
    // src: |---------|
    //
    // cur:   |----|
    // src: |----|
    //
    // cur: |----|
    // src:  |--|
    bool is_intersect(Range const src) const
    {
        return (src.end() >= end() && src.start() <= end()) ||
               (src.end() <= end() && src.end() >= start());
    }
    Pos end() const { return m_end; }
    Pos start() const { return m_start; }
};


class RangeVec : public Vector<Range> {
    COPY_CONSTRUCTOR(RangeVec);
public:
    RangeVec() {}
    Range const* get_vec() const
    { return const_cast<RangeVec*>(this)->Vector<Range>::get_vec(); }
};


class Occ {
public:
    bool m_is_def;
    Pos m_pos;
    IR * m_ir; //may be stmt or exp.
public:
    Occ() : m_is_def(false), m_pos(POS_UNDEF), m_ir(nullptr) {}
    Occ(Pos p) : m_is_def(false), m_pos(p), m_ir(nullptr) {}
    Occ(bool is_d, Pos p, IR * occ) :
        m_is_def(is_d), m_pos(p), m_ir(occ) {}

    bool is_def() const { return m_is_def; }
    Pos pos() const { return m_pos; }

    //Note the ir may be modified.
    IR * getIR() const { return m_ir; }
    IRBB * getBB() const { return getStmt()->getBB(); }
    IR * getStmt() const
    { return getIR()->is_stmt() ? getIR() : getIR()->getStmt(); }
};


typedef SC<Occ> OccListCt; //container
typedef SC<Occ> * OccListIter; //iterator

class OccList : public xcom::SListCoreEx<Occ> {
    COPY_CONSTRUCTOR(OccList);
public:
    OccList() {}
    void dump() const;
    void append_tail(Occ occ, LifeTimeMgr & mgr);
    void remove(OccListIter prev, OccListIter it, LifeTimeMgr & mgr);
};


class LifeTime {
    COPY_CONSTRUCTOR(LifeTime);
    bool m_is_dedicated;
    PRNO m_prno;

    //Used to record the number of calls intersected with the whole lifetime
    //except holes.
    UINT m_calls_crossed_num;
    LifeTime const* m_ancestor;
    double m_priority;
    double m_spill_cost;
    RangeVec m_range_vec;
    OccList m_occ_list;
    //Used to save the the positions of crossed calls in incremental order
    //during the whole lifetime.
    Vector<Pos> m_call_pos_vec;
public:
    LifeTime(PRNO prno) :
        m_is_dedicated(false), m_prno(prno),
        m_calls_crossed_num(0), m_ancestor(nullptr)
    {}
    Range addRange(Pos start, Pos end);
    Range addRange(Pos start) { return addRange(start, start); }
    void addOcc(Occ occ, LifeTimeMgr & mgr);

    //Clean the position from 'pos'(include pos).
    //e.g:lifetime range is <1-10><15-30>, given position is 16, the
    //    lifetime will be <1-10><15>.
    //Note given 'pos' must be in one of Ranges in current lifetime, otherwise
    //the function do nothing.
    void cleanRangeFrom(Pos pos);

    //This func will move the cross call info of 'src' from the position.
    //position 'pos' (include pos) into current lifetime.
    // e.g. 1:
    // input:  src.m_calls_crossed_num = 4
    //         src.m_call_pos_vec = {4ï¼?10ï¼? 30ï¼? 35}
    //                             | -   -     -     - |
    //                                         ^
    //                                         |
    //                                         pos = 30
    // output: src.m_calls_crossed_num = 2
    //         src.m_call_pos_vec = {4ï¼?10}
    //         cur.m_calls_crossed_num = 2
    //         cur.m_call_pos_vec = {30ï¼?35}
    // e.g. 2:
    // input:  src.m_calls_crossed_num = 4
    //         src.m_call_pos_vec = {4ï¼?10ï¼? 30ï¼? 35}
    //                             | -   -     -     - |
    //                                              ^
    //                                              |
    //                                              pos = 34
    // output: src.m_calls_crossed_num = 3
    //         src.m_call_pos_vec =  {4ï¼?10ï¼?30}
    //         cur.m_calls_crossed_num = 1
    //         cur.m_call_pos_vec = {35}
    void moveCrossedCallInfoFrom(LifeTime * src, Pos pos);
    //The function will move Range and Occ of 'src' that starting from
    //position 'pos' (include pos) into current lifetime.
    void moveRangeVecFrom(LifeTime * src, Pos pos);
    void moveOccListFrom(LifeTime * src, Pos pos, LifeTimeMgr & mgr);

    //The function moves lifetime from 'src' to current lifetime from position
    //'pos'(include pos).
    //e.g:src is <1-3><10-20>, pos is 19, current lifetime will be <19-20>,
    //while src will be <1-3><10-18>.
    void moveFrom(LifeTime * src, Pos pos, LifeTimeMgr & mgr);

    void dump(Region const* rg) const;

    //Return true and occ if there is occ at given pos.
    bool findOcc(Pos pos, OUT OccListIter & it) const;
    bool findOccAfter(Pos pos, OUT OccListIter & it) const;

    //Return true and range if there is range include the given pos.
    bool findRange(Pos pos, OUT Range & r, OUT VecIdx & ridx,
                   OUT VecIdx * less = nullptr,
                   OUT VecIdx * great = nullptr) const;

    UINT getCallCrossedNum() const { return m_calls_crossed_num; }
    Vector<Pos> const& getCallPosVec() const { return m_call_pos_vec; }
    UINT getRangeNum() const
    { return const_cast<LifeTime*>(this)->getRangeVec().get_elem_count(); }
    Pos getFirstPos() const { return getFirstRange().start(); }
    Pos getLastPos() const
    { return const_cast<LifeTime*>(this)->getLastRange().end(); }
    Range getFirstRange() const
    { return const_cast<LifeTime*>(this)->getRangeVec().get(0); }
    Range getLastRange()
    {
        VecIdx i = m_range_vec.get_last_idx();
        if (i == VEC_UNDEF) {
            return Range(POS_UNDEF);
        }
        return m_range_vec.get(i);
    }
    VecIdx getLastRangeIdx() const { return m_range_vec.get_last_idx(); }
    OccList & getOccList() { return m_occ_list; }
    RangeVec & getRangeVec() { return m_range_vec; }
    Range const* getRangeVecBuf() const { return m_range_vec.get_vec(); }
    PRNO getPrno() const { return m_prno; }
    Range getRange(VecIdx idx) const { return m_range_vec.get(idx); }
    LifeTime const* getAncestor() const { return m_ancestor; }
    Type const* getFirstOccType() const
    {
        return const_cast<LifeTime*>(this)->getOccList().get_head()->val().
               getIR()->getType();
    }
    double getPriority() const { return m_priority; }
    double getSpillCost() const { return m_spill_cost; }

    //Return the length of lifetime, which describes the number of program
    //points of lifetime.
    UINT getLength() const
    {
        ASSERT0(getLastPos() >= getFirstPos());
        return getLastPos() - getFirstPos() + 1;
    }

    //Updated the crossed call info.
    void updateCallCrossedInfo(Pos pos)
    {
        ASSERT0(pos != POS_UNDEF);
        m_calls_crossed_num++;
        m_call_pos_vec.append(pos);
    }

    //Remove the call postion in m_call_pos_vec from idx, and update the
    //m_calls_crossed_num at the same time.
    void removeCallPosVecFrom(VecIdx idx)
    {
        ASSERT0(m_calls_crossed_num == m_call_pos_vec.get_elem_count());
        ASSERT0(idx < (VecIdx)m_call_pos_vec.get_elem_count());
        m_call_pos_vec.cleanFrom(idx);
        m_calls_crossed_num -= m_call_pos_vec.get_elem_count() - idx - 1;
    }

    //Return true if current lifetime contains the position.
    //e.g:contain the pos
    //    | ----   ---  -- |
    //         ^
    //         |
    //         pos
    //e.g2:not contain the pos
    //    | ----   ---  -- |
    //          ^
    //          |
    //          pos
    bool is_contain(Pos pos) const;

    //Return true if current lifetime covers the position, otherwise pos exceed
    //the lifetime.
    //e.g:cover the pos
    //    | ----   ---  -- |
    //         ^
    //         |
    //         pos
    //e.g2:not cover the pos
    //    | ----   ---  -- |
    //     ^              ^
    //     |              |
    //     pos            pos
    bool is_cover(Pos pos) const;

    //Return true if current lifetime intersects with lt.
    //e.g:intersect
    // current: |-----|     |-------|
    //      lt:      |---|
    //e.g:not intersect
    // current: |-----|     |-------|
    //      lt:        |---|         |--|
    bool is_intersect(LifeTime const* lt) const;

    //Return true if current lifetime is dedicated.
    bool is_dedicated() const { return m_is_dedicated; }

    //Return true if 'ir' is an DEF occurrence of current lt.
    bool isDefOcc(IR const* ir) const
    {
        if (!ir->is_stmt()) { return false; }
        IR const* pr = const_cast<IR*>(ir)->getResultPR();
        return pr != nullptr && pr->getPrno() == getPrno() ? true : false;
    }

    //Return true if 'ir' is an USE occurrence of current lt.
    bool isUseOcc(IR const* ir) const
    {
        return ir->isReadPR() && ir->getPrno() == getPrno() ? true : false;
    }

    void removeOccFrom(OccListIter prev, OccListIter it, MOD LifeTimeMgr & mgr);
    void removeRangeFrom(VecIdx idx);

    //Set current lifetime to be deidcated, that means the lifetime must
    //assign dedicated register.
    void set_dedicated(bool is) { m_is_dedicated = is; }
    void setRange(VecIdx idx, Range r);
    void setLastRange(Range r)
    {
        m_range_vec.get_last_idx() == VEC_UNDEF ?
            m_range_vec.append(r) :
            m_range_vec.set(m_range_vec.get_last_idx(), r);
    }
    void setAncestor(LifeTime const* anc) { m_ancestor = anc; }
    void setPriority(double pri) { m_priority = pri; }
    void setSpillCost(double cost) { m_spill_cost = cost; }

    bool verify() const;
};

typedef xcom::TMap<IR const*, Pos> Call2PosMap;
typedef xcom::TMapIter<IR const*, Pos> Call2PosMapIter;
typedef Vector<LifeTime*> PRNO2LT;
class LifeTimeMgr {
    friend class OccList;
    COPY_CONSTRUCTOR(LifeTimeMgr);
    bool m_use_expose; //true to compute exposed def/use for each BB.
    Region * m_rg;
    SMemPool * m_pool;
    OccListCt * m_freect_list;
    LTList m_lt_list;
    PRNO2LT m_prno2lt;
    Vector<Pos> m_bb_entry_pos;
    Vector<Pos> m_bb_exit_pos;
    #ifdef _DEBUG_
    xcom::TMap<IR const*, Pos> m_ir2pos;
    #endif
private:
    OccListCt * allocOccListCt();
    LifeTime * allocLifeTime(PRNO prno);
    void computeLifeTimeBB(UpdatePos & up, IRBB const* bb,
                           DedicatedMgr const& dedicated_mgr,
                           Pos livein_def, IRIter & irit,
                           OUT Call2PosMap & call2pos);
    void destroy();
    void init(Region * rg);
    void * xmalloc(size_t size);

    //The function return the address of freect_list, so the callee could
    //modify the list.
    OccListCt ** getFreeCtListHeader() { return &m_freect_list; }
    bool useExpose() const { return m_use_expose; }
public:
    LifeTimeMgr(Region * rg)
    { m_pool = nullptr; m_use_expose = false; init(rg); }
    ~LifeTimeMgr() { destroy(); }

    //dedicated_tab: record PRNO that is dedicated, a dedicated PRNO must be
    //               assigned specific register.
    void computeLifeTime(UpdatePos & up, BBList const* bblst,
                         DedicatedMgr const& dedicated_mgr);

    void dumpAllLT(UpdatePos & up, BBList const* bblst,
                   bool dumpir = true) const;
    void dump() const;

    //This func generates the call crossed info for each life time.
    void genCallCrossedInfo(BBList const* bblst, Call2PosMap const& call2pos);

    LifeTime * genLifeTime(PRNO prno);
    LifeTime * getLifeTime(PRNO prno) const { return m_prno2lt.get(prno); }
    LTList const& getLTList() const { return m_lt_list; }
    Pos getBBStartPos(UINT bbid) const { return m_bb_entry_pos.get(bbid); }
    Pos getBBEndPos(UINT bbid) const { return m_bb_exit_pos.get(bbid); }
    PRNO2LT const& getPrno2LT() const { return m_prno2lt; }

    //Clean the lifetime info before computation.
    void reset();
    void recomputeLifeTime(UpdatePos & up, BBList const* bblst,
                           DedicatedMgr const& dedicated_mgr);
    void recordPos(IR const* ir, Pos pos);

    //Rename each occurrences of 'lt' to 'newprno'.
    void renameLifeTimeOcc(LifeTime const* lt, PRNO newprno);

    bool verifyPos(IR const* ir, Pos pos) const;
    bool verify() const;
};

} //namespace xoc
#endif
