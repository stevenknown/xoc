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

class LinearScanRA;

//
//START VarUpdatePos
//
class VarUpdatePos : public UpdatePos {
public:
    virtual bool updateAtIR(IR const* ir, OUT Pos & dpos,
                            OUT Pos & upos) override
    {
        update(dpos, upos);
        return true;
    }

    VarUpdatePos(LinearScanRA const* ra) : UpdatePos(ra) {}
};
//END VarUpdatePos

//
//START VarRange
//
#define VRG_start(r) ((r).m_start)
#define VRG_end(r) ((r).m_end)
class VarRange {
public:
    Pos m_start;
    Pos m_end;
public:
    //VarRange is point.
    VarRange() : m_start(0), m_end(0) {}
    VarRange(Pos p) : m_start(p), m_end(p) {}
    VarRange(Pos s, Pos e) : m_start(s), m_end(e) {}

    void dump(Region const* rg) const;
    void dumpG(Pos start, Region const* rg) const;
    bool is_less(VarRange const src) const { return end() < src.start(); }
    bool is_great(VarRange const src) const { return start() > src.end(); }
    bool is_contain(Pos pos) const { return start() <= pos && end() >= pos; }
    //Return true if current VarRange intersects with src.
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
    bool is_intersect(VarRange const src) const
    {
        return (src.end() >= end() && src.start() <= end()) ||
               (src.end() <= end() && src.end() >= start());
    }
    Pos end() const { return m_end; }
    Pos start() const { return m_start; }
};
//END VarRange

//
//START VarRangeVec
//
class VarRangeVec : public Vector<VarRange> {
    COPY_CONSTRUCTOR(VarRangeVec);
public:
    VarRangeVec() {}
    VarRange const* get_vec() const
    { return const_cast<VarRangeVec*>(this)->Vector<VarRange>::get_vec(); }
};
//END VarRangeVec

//
//START VarLifeTime
//
typedef DefSBitSetCore VarBBSet;
typedef DefSEGIter VarBBSetIter;
class VarLifeTime {
    COPY_CONSTRUCTOR(VarLifeTime);
    UINT m_id;
    UINT m_var_id;
    VarRangeVec m_range_vec;
    VarRangeVec m_accu_range_vec;
    VarBBSet * m_bb_set;
    DefMiscBitSetMgr & m_sbs_mgr;
public:
    VarLifeTime(UINT var_id, DefMiscBitSetMgr & sbs_mgr)
        : m_id(LT_ID_UNDEF), m_var_id(var_id), m_sbs_mgr(sbs_mgr)
    { m_bb_set = m_sbs_mgr.allocSBitSetCore(); }
    ~VarLifeTime() { m_sbs_mgr.freeSBitSetCore(m_bb_set); }

    void addBB(UINT bbid) { m_bb_set->bunion((BSIdx)bbid, m_sbs_mgr); }
    VarRange addRange(Pos start, Pos end)
    {
      ASSERT0(getLastRange().is_less(VarRange(start, end)));
       m_range_vec.append(VarRange(start, end));
       return VarRange(start, end);
    }
    VarRange addAccuRange(Pos start, Pos end)
    {
       ASSERT0(getLastAccuRange().is_less(VarRange(start, end)));
       m_accu_range_vec.append(VarRange(start, end));
       return VarRange(start, end);
    }
    VarRange addRange(Pos start) { return addRange(start, start); }
    VarRange addAccuRange(Pos start) { return addAccuRange(start, start); }

    void cleanRangeVec() { return m_range_vec.clean(); }

    void dump(Region const* rg) const;

    VarBBSet * getBBSet() { return m_bb_set; }
    UINT getVarId() const { return m_var_id; }
    VarRangeVec & getRangeVec() { return m_range_vec; }
    VarRangeVec & getAccuRangeVec() { return m_accu_range_vec; }

    VarRange getFirstRange() const
    { return const_cast<VarLifeTime*>(this)->getRangeVec().get(0); }
    VarRange getLastRange() const
    {
        VecIdx i = m_range_vec.get_last_idx();
        if (i == VEC_UNDEF) {
            return VarRange(POS_UNDEF);
        }
        return m_range_vec.get(i);
    }

    VarRange getLastAccuRange() const
    {
        VecIdx i = m_accu_range_vec.get_last_idx();
        if (i == VEC_UNDEF) {
            return VarRange(POS_UNDEF);
        }
        return m_accu_range_vec.get(i);
    }

    UINT id() const { return m_id; }

    bool isIntersect(VarLifeTime * lt2)
    {
        return VarLifeTime::isIntersect(&getAccuRangeVec(),
                                        &lt2->getAccuRangeVec());
    }

    static bool isIntersect(VarRangeVec const* rv1, VarRangeVec const* rv2);

    void setId(UINT id) { m_id = id; }

    void setLastRange(VarRange r)
    {
        m_range_vec.get_last_idx() == VEC_UNDEF ?
            m_range_vec.append(r) :
            m_range_vec.set(m_range_vec.get_last_idx(), r);
    }

    void setLastAccuRange(VarRange r)
    {
        m_accu_range_vec.get_last_idx() == VEC_UNDEF ?
            m_accu_range_vec.append(r) :
            m_accu_range_vec.set(m_accu_range_vec.get_last_idx(), r);
    }
};
//END VarLifeTime

//
//START VarLTList
//
typedef List<VarLifeTime*>::Iter VarLTListIter;
class VarLTList : public List<VarLifeTime*> {
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
typedef Vector<VarLifeTime*> Var2LT;
class VarLifeTimeMgr {
    COPY_CONSTRUCTOR(VarLifeTimeMgr);
    UINT m_cnt;
    Region * m_rg;
    VarLivenessMgr * m_var_liveness;
    LinearScanRA & m_ra;
    Var2LT m_var2lt;
    VarLTList m_lt_list;
    DefMiscBitSetMgr m_sbs_mgr;
    Vector<Pos> m_bb_entry_pos;
    Vector<Pos> m_bb_exit_pos;
public:
    VarLifeTimeMgr(Region * rg, LinearScanRA & ra,
                   VarLivenessMgr * var_liveness) : m_ra(ra)
    { m_rg = rg; m_var_liveness = var_liveness; m_cnt = LT_ID_UNDEF; }
    ~VarLifeTimeMgr() { destroy(); }

    VarLifeTime * allocLifeTime(UINT var_id)
    {
        VarLifeTime * lt = new VarLifeTime(var_id, m_sbs_mgr);
        m_lt_list.append_tail(lt);
        m_cnt++;
        lt->setId(m_cnt);
        return lt;
    }

    void computeLifeTime();
    void computeLifeTimeBB(UpdatePos & up, IRBB const* bb,
                           Pos livein_def, IRIter & irit);
    void computeLifeTimeStmt(IR * ir, MOD Pos & pos, Pos livein_pos);

    void computeAccuLifeTime();
    void computeAccuLifeTimeBB(UpdatePos & up, IRBB const* bb,
                               Pos livein_def, IRIter & irit);
    void computeAccuLifeTimeStmt(IR * ir, MOD Pos & pos, Pos livein_pos);

    void destroy()
    {
        for (VarLifeTime * lt = m_lt_list.get_head(); lt != nullptr;
             lt = m_lt_list.get_next()) {
            delete lt;
        }
        m_lt_list.destroy();
        m_var2lt.destroy();
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
    LinearScanRA & getLSRA() { return m_ra; }
    VarLTList const& getLTList() const { return m_lt_list; }
};
//END VarLifeTimeMgr

//
//START VarInterfGraph
//
typedef xcom::TMap<UINT, UINT> LTId2VarId;
typedef xcom::TMapIter<UINT, UINT> LTId2VarIdIter;
class VarInterfGraph : public Graph {
    COPY_CONSTRUCTOR(VarInterfGraph);
    Region * m_rg;

    //m_next_color_index is used to record the next available index respondng
    //to the color pool if a new color is required durng the color process,
    //and also it can indicate the total count of the colors used when the color
    //process is finished.
    UINT m_next_color_index;
    VarLifeTimeMgr & m_lt_mgr;
    LTId2VarId m_lt2var;
    Vector<VarLifeTime*> m_color_pool;
    Vector<Vector<VarLifeTime*>*> m_color_allocated;
protected:
    virtual void dumpVertexDesc(
        xcom::Vertex const* v, OUT xcom::DefFixedStrBuf & buf) const override;
public:
    VarInterfGraph(Region * rg, VarLifeTimeMgr & mgr) : m_rg(rg),
        m_next_color_index(0), m_lt_mgr(mgr)
    { set_direction(false); set_dense(false); }

    ~VarInterfGraph()
    {
        for (VecIdx i = 0; i < (VecIdx)m_color_allocated.get_elem_count();
             i++) {
            Vector<VarLifeTime*> * color = getColor(i);
            ASSERT0(color);
            delete color;
        }
    }

   void assignColor(VarLifeTime const* lt, UINT color_idx)
   { m_lt2var.set(lt->id(), m_color_pool.get(color_idx)->getVarId()); }

    //Build interference graph for lifetime in VarLifeTimeMgr.
    void build();

    void color();
    void colorFast();

    void dump();
    void dumpGraph(CHAR const* name) const { dumpDOT(name); }

    Vector<VarLifeTime*> * genColor(UINT index)
    {
        Vector<VarLifeTime*> * color = new Vector<VarLifeTime*>();
        m_color_allocated.set(index, color);
        return color;
    }

    Vector<VarLifeTime*> * getColor(UINT index) const
    {  return m_color_allocated.get(index); }

    void initColorPool()
    {
        VarLTListIter it;
        for (VarLifeTime * lt = m_lt_mgr.getLTList().get_head(&it);
             lt != nullptr; lt = m_lt_mgr.getLTList().get_next(&it)) {
            m_color_pool.append(lt);
        }
    }

    //This function is used to check the input lifetime is interferenced with
    //the allocated colors.
    //lt: the input lifetime of var.
    //id: the color id, valid only if return false.
    //return true if it interferenced with all the color allocated; return false
    //means it is not interferenced with a color allocated, and the color id
    //will be outputted.
    bool isInterferWithAllocatedColors(VarLifeTime const* lt, OUT VecIdx & id);

    //Rewrite the var in the IRs to the new assigned var per the mapping table.
    void rewrite();
    void rewriteBB(IRBB const* bb);
    void rewriteStmt(IR * ir);
};
//END VarInterfGraph

} //namespace xoc
#endif
