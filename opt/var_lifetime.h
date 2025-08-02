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
@*/
#ifndef __VAR_LIFETIME_H__
#define __VAR_LIFETIME_H__

namespace xoc {

#define LT_ID_UNDEF 0

class LinearScanRA;

//
//START VarUpdatePos
//
class VarUpdatePos : public UpdatePos {
public:
    virtual bool updateAtIR(
        IR const* ir, OUT Pos & dpos, OUT Pos & upos) override
    {
        DUMMYUSE(ir);
        update(dpos, upos);
        return true;
    }

    VarUpdatePos(LinearScanRA const* ra) : UpdatePos(ra) {}
};
//END VarUpdatePos

//
//START VarLifeTime
//
typedef DefSBitSetCore VarBBSet;
class VarLifeTime {
    COPY_CONSTRUCTOR(VarLifeTime);
    UINT m_id;
    UINT m_var_id;
    RangeVec m_range_vec;
public:
    VarLifeTime(UINT var_id) : m_id(LT_ID_UNDEF), m_var_id(var_id) {}
    ~VarLifeTime() {}

    Range addRange(Pos start, Pos end)
    {
       ASSERT0(getLastRange().is_less(Range(start, end)));
       m_range_vec.append(Range(start, end));
       return Range(start, end);
    }
    Range addRange(Pos start) { return addRange(start, start); }

    void cleanRangeVec() { return m_range_vec.clean(); }

    void dump(Region const* rg) const;

    UINT getVarId() const { return m_var_id; }
    RangeVec const& getRangeVec() const { return m_range_vec; }
    Range getFirstRange() const
    { return const_cast<VarLifeTime*>(this)->getRangeVec().get(0); }
    Range getLastRange() const
    {
        VecIdx i = m_range_vec.get_last_idx();
        if (i == VEC_UNDEF) {
            return Range(POS_UNDEF);
        }
        return m_range_vec.get(i);
    }

    UINT id() const { return m_id; }
    bool isIntersect(VarLifeTime const* lt2) const
    {
        return VarLifeTime::isIntersect(
            &getRangeVec(), &lt2->getRangeVec());
    }
    static bool isIntersect(RangeVec const* rv1, RangeVec const* rv2);

    void setId(UINT id) { m_id = id; }
    void setLastRange(Range r)
    {
        m_range_vec.get_last_idx() == VEC_UNDEF ?
            m_range_vec.append(r) :
            m_range_vec.set(m_range_vec.get_last_idx(), r);
    }
};
//END VarLifeTime

//
//START VarLTList
//
typedef xcom::List<VarLifeTime*>::Iter VarLTListIter;
class VarLTList : public xcom::List<VarLifeTime*> {
public:
    void dump(Region const* rg) const
    {
        note(rg, "\n==-- DUMP %s --==", "VarLTList");
        VarLTListIter it;
        for (VarLifeTime const* lt = get_head(&it);
            lt != nullptr; lt = get_next(&it)) {
            lt->dump(rg);
        }
    }
};
//END VarLTList

//
//START VarLifeTimeMgr
//
typedef xcom::Vector<VarLifeTime*> Var2LT;
class VarLifeTimeMgr {
    COPY_CONSTRUCTOR(VarLifeTimeMgr);
    UINT m_cnt;
    Region * m_rg;
    VarLivenessMgr * m_var_liveness;
    LinearScanRA & m_ra;
    Var2LT m_var2lt;
    VarLTList m_lt_list;
    xcom::Vector<Pos> m_bb_entry_pos;
    xcom::Vector<Pos> m_bb_exit_pos;
public:
    VarLifeTimeMgr(Region * rg, LinearScanRA & ra,
        VarLivenessMgr * var_liveness) : m_rg(rg),
        m_var_liveness(var_liveness), m_ra(ra)
    { init(); }
    virtual ~VarLifeTimeMgr() { destroy(); }

    VarLifeTime * allocLifeTime(UINT var_id)
    {
        VarLifeTime * lt = new VarLifeTime(var_id);
        m_lt_list.append_tail(lt);
        m_cnt++;
        lt->setId(m_cnt);
        return lt;
    }

    void computeLifeTime();
    void computeLifeTimeBB(UpdatePos & up, IRBB const* bb,
                           Pos livein_def, IRIter & irit);
    void computeLifeTimeStmt(IR * ir, Pos dpos, Pos upos, Pos livein_pos);
    void computeStmtLHS(IR * ir, Pos pos);
    void computeStmtRHS(IR * ir, Pos pos, Pos livein_def);

    void destroy()
    {
        for (VarLifeTime * lt = m_lt_list.get_head();
             lt != nullptr; lt = m_lt_list.get_next()) {
            delete lt;
        }
        m_lt_list.destroy();
        m_var2lt.destroy();
        m_cnt = LT_ID_UNDEF;
        m_bb_entry_pos.destroy();
        m_bb_exit_pos.destroy();
    }

    void dump() const;

    VarLifeTime * genLifeTime(UINT var_id)
    {
        ASSERT0(var_id != VAR_ID_UNDEF);
        VarLifeTime * lt = getLifeTime(var_id);
        if (lt == nullptr) {
            lt = allocLifeTime(var_id);
            m_var2lt.set(var_id, lt);
        }
        return lt;
    }
    VarLifeTime * getLifeTime(UINT varid) const { return m_var2lt.get(varid); }
    LinearScanRA const& getLSRA() const { return m_ra; }
    VarLTList const& getLTList() const { return m_lt_list; }

    void init()
    {
        m_cnt = LT_ID_UNDEF;
        m_bb_entry_pos.init();
        m_bb_exit_pos.init();
        m_var2lt.init();
        m_lt_list.init();
    }

    //Return whether the lifetime of the operand on the left needs
    //to be calculated, and return true if it needs to be calculated.
    virtual bool isNeedComputeLHS(IR const* ir) const;

    //Return whether the lifetime of the operand on the right needs
    //to be calculated, and return true if it needs to be calculated.
    virtual bool isNeedComputeRHS(IR const* ir) const;

    void reset()
    { destroy(); init(); }
};
//END VarLifeTimeMgr

} //namespace xoc
#endif
