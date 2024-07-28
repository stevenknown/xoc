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
#include "cominc.h"
#include "comopt.h"
#include "targinfo_mgr.h"
#include "lifetime.h"
#include "lt_interf_graph.h"
#include "linear_scan.h"
#include "lsra_impl.h"
#include "lsra_scan_in_pos.h"
#include "lt_prio_mgr.h"
#include "lsra_scan_in_prio.h"

namespace xoc {

//
//START DedicatedMgr
//
void DedicatedMgr::dump(Region const* rg, TargInfoMgr const& timgr) const
{
    if (get_elem_count() == 0) { return; }
    note(rg, "\n==-- DUMP %s --==", "DedicatedMgr");
    PRNO2RegIter it;
    Reg r;
    for (PRNO p = get_first(it, &r); !it.end(); p = get_next(it, &r)) {
        note(rg, "\n$%u:%s", p, timgr.getRegName(r));
    }
}
//END DedicatedMgr


//
//START UpdatePos
//
static inline bool is_even(Pos pos)
{
    return (pos & 0x1) == 0;
}


static inline bool is_odd(Pos pos)
{
    return (pos & 0x1) == 1;
}


void UpdatePos::decToLastUse(MOD Pos & pos)
{
    if (isDef(pos)) {
        dec(pos);
        return;
    }
}


void UpdatePos::incToNextDef(MOD Pos & pos)
{
    if (isUse(pos)) {
        inc(pos);
        return;
    }
}


void UpdatePos::decToLastDef(MOD Pos & pos)
{
    if (isDef(pos)) {
        dec(pos);
        dec(pos);
        return;
    }
    dec(pos);
}


//Return true if pos indicates LHS.
bool UpdatePos::isDef(Pos pos)
{
    return pos != POS_UNDEF && is_even(pos);
}


//Return true if pos indicates RHS.
bool UpdatePos::isUse(Pos pos)
{
    return pos != POS_UNDEF && is_odd(pos);
}


bool UpdatePos::updateAtRegionEntry(OUT Pos & dpos, OUT Pos & upos)
{
    update(dpos, upos);
    return true;
}


bool UpdatePos::updateAtRegionExit(OUT Pos & dpos, OUT Pos & upos)
{
    update(dpos, upos);
    return true;
}


bool UpdatePos::updateAtBBEntry(OUT Pos & dpos, OUT Pos & upos)
{
    if (useExpose()) {
        //BB start position
        update(dpos, upos);
        return true;
    }
    return false;
}


bool UpdatePos::updateAtBBExit(OUT Pos & dpos, OUT Pos & upos)
{
    if (useExpose()) {
        //BB end position
        update(dpos, upos);
        return true;
    }
    return false;
}


//Return true if ir will be encoded at current position, otherwise false that
//indicates ir is not belong to any lifetime.
bool UpdatePos::updateAtIR(IR const* ir, OUT Pos & dpos, OUT Pos & upos)
{
    if (m_ra.isSpillOp(ir) || m_ra.isReloadOp(ir) ||
        m_ra.isRematOp(ir) || m_ra.isMoveOp(ir)) {
        //No need to handle spill/reload/remat/move.
        //Their occ did not encoded with a position and therefore not
        //resided in any lifetime.
        return false;
    }
    update(dpos, upos);
    return true;
}
//END UpdatePos


//
//START Range
//
void Range::dumpG(Pos init_pos, Region const* rg) const
{
    ASSERT0(rg);
    StrBuf buf(64);
    for (Pos pos = init_pos; pos <= end(); pos++) {
        if (pos < start()) {
            //prt(rg, " ");
            buf.strcat(" ");
            continue;
        }
        //prt(rg, "-");
        buf.strcat("-");
    }
    prt(rg, "%s", buf.buf);
}


void Range::dump(Region const* rg) const
{
    ASSERT0(rg);
    prt(rg, "<");
    if (start() == POS_UNDEF) {
        prt(rg, "UNDEF");
    } else {
        prt(rg, "%u", start());
    }
    if (start() != end()) {
        prt(rg, "-");
        if (end() == POS_UNDEF) {
            prt(rg, "UNDEF");
        } else {
            prt(rg, "%u", end());
        }
    }
    prt(rg, ">");
}
//END Range


//
//START OccList
//
void OccList::append_tail(Occ occ)
{
   xcom::List<Occ>::append_tail(occ);
}


void OccList::remove(OccListIter it)
{
    xcom::List<Occ>::remove(it);
}
//END OccList


//
//START LifeTime
//
class RangeCompare : public BinarySearchCompareBase<Range> {
public:
    //Return true if t1 < t2.
    bool is_less(Range t1, Range t2) const { return t1.end() < t2.start(); }

    //Return true if t1 > t2.
    bool is_great(Range t1, Range t2) const { return t1.start() > t2.end(); }

    //Return true if t1 equal or contain t2.
    bool is_equ(Range t1, Range t2) const
    { return t1.start() <= t2.start() && t1.end() >= t2.end(); }
};


void LifeTime::cleanRangeFrom(Pos pos)
{
    //Find the range at 'pos'.
    Range r(POS_UNDEF);
    VecIdx rangeidx;
    bool f = findRange(pos, r, rangeidx);
    if (!f) {
        //Current lifetime does not contain range that cover 'pos'.
        return;
    }
    VecIdx remove_start = VEC_UNDEF;
    if (r.start() == pos) {
        //The range in 'rangeidx' will be moved to current lifetime.
        remove_start = rangeidx;
    } else {
        remove_start = rangeidx + 1;
        //Change the range in 'rangeidx' to be second half of before, while
        //the first half of range will be moved to current lifetime.
        //e.g:original range: <10-20>, pos is 19, the range in 'rangeidx' will
        //be set to <19-20>.
        ASSERT0(pos <= r.end());
        UpdatePos::dec(pos);
        setRange(rangeidx, Range(r.start(), pos));
    }
    //Clean the rest of range.
    if (remove_start < (VecIdx)getRangeNum()) {
        removeRangeFrom(remove_start);
    }
}


void LifeTime::moveRangeVecFrom(LifeTime * src, Pos pos)
{
    //Find the range in src at 'pos'.
    Range srcr(POS_UNDEF);
    VecIdx rangeidx;
    bool f = src->findRange(pos, srcr, rangeidx);
    ASSERT0_DUMMYUSE(f);
    Pos newr_start = pos;
    Pos newr_end = srcr.end();
    addRange(newr_start, newr_end);
    VecIdx src_remove_start = VEC_UNDEF;
    if (srcr.start() == pos) {
        //The range in 'rangeidx' will be moved to current lifetime.
        src_remove_start = rangeidx;
    } else {
        src_remove_start = rangeidx + 1;
        //Change the range in 'rangeidx' to be second half of before, while
        //the first half of range will be moved to current lifetime.
        //e.g:original range: <10-20>, pos is 19, the range in 'rangeidx' will
        //be set to <19-20>.
        ASSERT0(pos <= srcr.end());
        UpdatePos::dec(pos);
        src->setRange(rangeidx, Range(srcr.start(), pos));
    }
    //Copy the rest of src's range to current lifetime.
    RangeVec & srcrv = const_cast<LifeTime*>(src)->getRangeVec();
    for (VecIdx i = rangeidx + 1; i < (VecIdx)srcrv.get_elem_count(); i++) {
        Range srcr = srcrv.get(i);
        addRange(srcr.start(), srcr.end());
    }
    //Clean the rest of src's range.
    if (src_remove_start < (VecIdx)src->getRangeNum()) {
        src->removeRangeFrom(src_remove_start);
    }
}


void LifeTime::removeOccFrom(OccListIter it)
{
    for (OccListIter nit = it; it != getOccList().end(); it = nit) {
        nit = getOccList().get_next(it);
        getOccList().remove(it);
    }
}


void LifeTime::moveOccListFrom(LifeTime * src, Pos pos)
{
    LifeTime * psrc = const_cast<LifeTime*>(src);
    OccListIter it = nullptr;
    OccListIter remove_point = nullptr;
    for (Occ occ = psrc->getOccList().get_head(&it);
         it != psrc->getOccList().end();
        occ = psrc->getOccList().get_next(&it)) {
        ASSERTN(occ.getIR() && !occ.getIR()->is_undef(), ("ilegal occ"));
        if (occ.pos() >= pos) {
            addOcc(occ);
            remove_point = it;
            break;
        }
    }
    if (it == psrc->getOccList().end()) { return; }
    for (Occ occ = psrc->getOccList().get_next(&it);
         it != psrc->getOccList().end();
         occ = psrc->getOccList().get_next(&it)) {
        ASSERTN(occ.getIR() && !occ.getIR()->is_undef(), ("ilegal occ"));
        ASSERT0(occ.pos() >= pos);
        addOcc(occ);
    }
    src->removeOccFrom(remove_point);
}


void LifeTime::shrinkForwardToLastOccPos()
{
    Occ occ = getOccList().get_tail();
    Range temp = getLastRange();
    temp.m_end = occ.pos();
    setLastRange(temp);
}


void LifeTime::moveFrom(LifeTime * src, Pos pos)
{
    moveRangeVecFrom(src, pos);
    moveOccListFrom(src, pos);
    //m_call_crossed_num is just an rough esimation number, so use the number
    //from source lifetime directly.
    m_call_crossed_num = src->getCallCrossedNum();
}


bool LifeTime::findRange(Pos pos, OUT Range & r, OUT VecIdx & ridx,
                         OUT VecIdx * less, OUT VecIdx * great) const
{
    //Find the range in src at 'pos'.
    xcom::BinarySearch<Range, RangeCompare> bs;
    ridx = VEC_UNDEF;
    RangeVec & rv = const_cast<LifeTime*>(this)->getRangeVec();
    bs.search(rv, Range(pos, pos), &ridx, less, great);
    if (ridx == VEC_UNDEF) { return false; }
    r = rv.get(ridx);
    ASSERT0(r.is_contain(pos));
    return true;
}


bool LifeTime::findOcc(Pos pos, OUT OccListIter & it) const
{
    OccList & ol = const_cast<LifeTime*>(this)->getOccList();
    for (Occ occ = ol.get_head(&it); it != ol.end(); occ = ol.get_next(&it)) {
        if (occ.pos() == pos) {
            return true;
        }
    }
    return false;
}


bool LifeTime::findOccAfter(Pos pos, OUT OccListIter & it) const
{
    OccList & ol = const_cast<LifeTime*>(this)->getOccList();
    for (Occ occ = ol.get_head(&it); it != ol.end(); occ = ol.get_next(&it)) {
        if (occ.pos() > pos) {
            return true;
        }
    }
    return false;
}


void LifeTime::addOcc(Occ occ)
{
    ASSERTN(occ.getIR() && !occ.getIR()->is_undef(), ("ilegal occ"));
    m_occ_list.append_tail(occ);
}


Range LifeTime::addRange(Pos start, Pos end)
{
    ASSERT0(getLastRange().is_less(Range(start, end)));
    m_range_vec.append(Range(start, end));
    return Range(start, end);
}


bool LifeTime::is_cover(Pos pos) const
{
    Range r = const_cast<LifeTime*>(this)->getRangeVec().get(0);
    if (pos < r.start()) { return false; }
    r = const_cast<LifeTime*>(this)->getLastRange();
    if (pos > r.end()) { return false; }
    return true;
}


bool LifeTime::is_contain(Pos pos) const
{
    xcom::BinarySearch<Range, RangeCompare> bs;
    return bs.search(const_cast<LifeTime*>(this)->getRangeVec(),
                     Range(pos, pos));
}


bool LifeTime::is_intersect(LifeTime const* lt) const
{
    LifeTime * lt1 = const_cast<LifeTime*>(this);
    LifeTime * lt2 = const_cast<LifeTime*>(lt);
    RangeVec const* rv1 = &lt1->getRangeVec();
    RangeVec const* rv2 = &lt2->getRangeVec();
    UINT i = 0;
    UINT j = 0;
    UINT rv1n = lt1->getRangeNum();
    UINT rv2n = lt2->getRangeNum();
    Range r1 = rv1->get(i);
    Range r2 = rv2->get(j);
    for (;;) {
        if (r1.start() > r2.start()) {
            xcom::swap(rv1, rv2);
            xcom::swap(r1, r2);
            xcom::swap(i, j);
            xcom::swap(rv1n, rv2n);
        }
        bool find = false;
        for (; i < rv1n; i++) {
            r1 = rv1->get(i);
            if (!r1.is_less(r2)) {
                find = true;
                break;
            }
        }
        if (!find) {
            return false;
        }
        if (r1.is_intersect(r2)) { return true; }
        ASSERT0(r1.is_great(r2));
        j++;
        r2 = rv2->get(j);
    }
    UNREACHABLE();
    return true;
}


static void dumpOccList(OccList const& ol, Region const* rg)
{
    ASSERT0(rg);
    OccListIter it;
    for (Occ occ = ol.get_head(&it); it != ol.end(); occ = ol.get_next(&it)) {
        if (occ.is_def()) {
            prt(rg, "d");
        } else {
            prt(rg, "u");
        }
        prt(rg, "%u ", occ.pos());
    }
}


static void dumpOccListG(OccList const& ol, Region const* rg)
{
    ASSERT0(rg);
    Pos p = POS_INIT_VAL;
    OccListIter it = nullptr;
    for (Occ occ = ol.get_head(&it); it != ol.end(); occ = ol.get_next(&it)) {
        for (; p < occ.pos(); p++) {
            prt(rg, " ");
        }
        if (occ.is_def()) {
            prt(rg, "d");
        } else {
            prt(rg, "u");
        }
        p++;
    }
}


static void dumpRangeVec(RangeVec const& rv, Region const* rg)
{
    for (VecIdx i = 0; i < (VecIdx)rv.get_elem_count(); i++) {
        Range const& r = rv.get(i);
        r.dump(rg);
    }
}


static void dumpRangeVecG(RangeVec const& rv, Region const* rg)
{
    Pos prev_end = POS_INIT_VAL;
    for (VecIdx i = 0; i < (VecIdx)rv.get_elem_count(); i++) {
        Range const& r = rv.get(i);
        r.dumpG(prev_end, rg);
        prev_end = r.end() + 1;
    }
}


void LifeTime::dump(Region const* rg) const
{
    note(rg, "\nLT:$%u,prio:%0.2f,", getPrno(), getPriority());
    if (getCallCrossedNum() != 0) {
        prt(rg, "call_crossed_num:%u,", getCallCrossedNum());
    }
    prt(rg, "range:");
    dumpRangeVec(m_range_vec, rg);
    //note(rg, "\n |");
    //dumpOccList(m_occ_list, rg);
    DUMMYUSE(dumpOccList);
    note(rg, "\n |");
    dumpRangeVecG(m_range_vec, rg);
    note(rg, "\n |");
    dumpOccListG(m_occ_list, rg);
}


static bool verifyOccList(LifeTime const* lt)
{
    Pos prev_pos = POS_INIT_VAL;
    LifeTime * plt = const_cast<LifeTime*>(lt);
    OccListIter it = nullptr;
    for (Occ occ = plt->getOccList().get_head(&it);
         it != plt->getOccList().end(); occ = plt->getOccList().get_next(&it)) {
        ASSERTN(occ.getIR() && !occ.getIR()->is_undef(), ("ilegal occ"));
        ASSERTN_DUMMYUSE(prev_pos <= occ.pos(),
                         ("pos should be incremental order"));
        prev_pos = occ.pos();
    }
    return true;
}


static bool verifyRangeVec(LifeTime const* lt)
{
    Range prev(POS_UNDEF);
    RangeVec & rv = const_cast<LifeTime*>(lt)->getRangeVec();
    for (VecIdx i = 0; i < (VecIdx)rv.get_elem_count(); i++) {
        Range const& r = rv.get(i);
        ASSERT0(r.start() <= r.end());
        ASSERT0(r.start() > prev.end());
        prev = r;
    }
    return true;
}


void LifeTime::setRange(VecIdx idx, Range r)
{
    ASSERT0(idx < (VecIdx)getRangeVec().get_elem_count());
    getRangeVec().set(idx, r);
}


void LifeTime::removeRangeFrom(VecIdx idx)
{
    ASSERT0(idx < (VecIdx)getRangeNum());
    m_range_vec.cleanFrom(idx);
}


bool LifeTime::verify() const
{
    verifyOccList(this);
    verifyRangeVec(this);
    return true;
}
//END LifeTime


//
//START LifeTimeMgr
//
LifeTime * LifeTimeMgr::allocLifeTime(PRNO prno)
{
    LifeTime * lt = new LifeTime(prno);
    m_lt_list.append_tail(lt);
    return lt;
}


void * LifeTimeMgr::xmalloc(size_t size)
{
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, size);
    return p;
}


LifeTime * LifeTimeMgr::genLifeTime(PRNO prno)
{
    ASSERT0(prno != PRNO_UNDEF);
    LifeTime * lt = getLifeTime(prno);
    if (lt == nullptr) {
        lt = allocLifeTime(prno);
        m_prno2lt.set(prno, lt);
    }
    return lt;
}


void LifeTimeMgr::init(Region * rg)
{
    if (m_pool != nullptr) { return; }
    m_pool = smpoolCreate(64, MEM_COMM);
    m_rg = rg;
    m_lt_list.init();
    m_prno2lt.init();
    #ifdef _DEBUG_
    m_ir2pos.init();
    #endif
}


void LifeTimeMgr::destroy()
{
    if (m_pool == nullptr) { return; }
    for (LifeTime * lt = m_lt_list.get_head(); lt != nullptr;
         lt = m_lt_list.get_next()) {
        delete lt;
    }
    m_lt_list.destroy();
    m_prno2lt.destroy();
    #ifdef _DEBUG_
    m_ir2pos.destroy();
    #endif
    smpoolDelete(m_pool);
    m_pool = nullptr;
}


static void computeLHS(IR * ir, LifeTimeMgr & mgr, Pos pos,
                       DedicatedMgr const& dedmgr,
                       MOD CrossedCallCounter & cross_call_counter)
{
    ASSERT0(ir && ir->is_stmt());
    IR const* res = const_cast<IR*>(ir)->getResultPR();
    if (res == nullptr) { return; }
    PRNO prno = res->getPrno();
    LifeTime * lt = mgr.genLifeTime(prno);
    lt->set_dedicated(dedmgr.is_dedicated(prno));
    ASSERT0(lt);
    lt->addRange(pos);
    lt->addOcc(Occ(true, pos, ir));
    mgr.recordPos(ir, pos);
    cross_call_counter.updateForDef(lt);
}


static void computeUSE(IR * ir, LifeTimeMgr & mgr, Pos pos, Pos livein_def,
                       DedicatedMgr const& dedmgr,
                       MOD CrossedCallCounter & cross_call_counter)
{
    ASSERT0(ir && ir->isPROp());
    PRNO prno = ir->getPrno();
    LifeTime * lt = mgr.genLifeTime(prno);
    lt->set_dedicated(dedmgr.is_dedicated(prno));
    ASSERT0(lt);
    Range r = lt->getLastRange();
    if (r.start() == POS_UNDEF) {
        //PR is region livein.
        RG_start(r) = livein_def;
        RG_end(r) = pos;
    } else {
        RG_end(r) = pos;
    }
    lt->setLastRange(r);
    lt->addOcc(Occ(false, pos, ir));
    mgr.recordPos(ir, pos);
    cross_call_counter.updateForUse(lt, r.start());
}


static void computeRHS(IR * ir, LifeTimeMgr & mgr, Pos pos,
                       Pos livein_def, IRIter & irit,
                       DedicatedMgr const& dedmgr,
                       MOD CrossedCallCounter & cross_call_counter)
{
    ASSERT0(ir && ir->is_stmt());
    irit.clean();
    for (IR * e = xoc::iterExpInit(ir, irit); e != nullptr;
         e = xoc::iterExpNext(irit)) {
        if (e->isReadPR()) {
            computeUSE(e, mgr, pos, livein_def, dedmgr, cross_call_counter);
        }
    }
}


void CrossedCallCounter::updateForUse(LifeTime * lt, Pos def_pos)
{
    ASSERT0(lt);
    ASSERT0(def_pos != POS_UNDEF);
    if (m_call2pos.get_elem_count() == 0 ||
        lt->getCallCrossedNum() >= CROSS_CALL_NUM_THRESHOLD) {
        //Implement the 2.1 of the algorithm.
        //Don't update the call_crossed_num if there is no call or the
        //call_crossed_num is greater than the CROSS_CALL_NUM_THRESHOLD.
        return;
    }

    PRNO prno = lt->getPrno();
    VecIdx current_call_id = m_call2pos.get_last_idx();

    //If the position of current_call_id is less than the start of a range
    //(DEF position), that means this call is in the hole of the lifetime,
    //so it will not impact the call_crossed_num.
    if (m_call2pos[current_call_id] < def_pos) {
        //Implement the 2.2 of the algorithm.
        return;
    }

    //If the prno cannot find in the map m_prno2callid, that means the
    //baseline_call_id is not recorded, it needs to be recorded first.
    bool find = false;
    VecIdx baseline_call_id = m_prno2callid.get(prno, &find);
    if (!find) {
        //Implement the 2.3.1 of the algorithm.
        lt->incCallCrossedNum(current_call_id + 1);
        m_prno2callid.set(prno, current_call_id);
        return;
    }

    if (baseline_call_id < current_call_id) {
        //Implement the 2.3.2 of the algorithm.
        //Add the call_crossed_num with the number of calls not counted.
        lt->incCallCrossedNum(current_call_id - baseline_call_id);
        m_prno2callid.setAlways(prno, current_call_id);
    }
}


void CrossedCallCounter::updateForDef(LifeTime * lt)
{
    ASSERT0(lt);
    if (m_call2pos.get_elem_count() == 0 ||
        lt->getCallCrossedNum() >= CROSS_CALL_NUM_THRESHOLD) {
        //Implement the 3.1 of the algorithm.
        //Don't update the call_crossed_num if there is no call or the
        //call_crossed_num is greater than the CROSS_CALL_NUM_THRESHOLD.
        return;
    }
    //Implement the 3.2 of the algorithm.
    //Record the baseline_call_id to current_call_id at the DEF postion,
    //because this DEF position will start a new range of lifetime.
    m_prno2callid.setAlways(lt->getPrno(), m_call2pos.get_last_idx());
}


void LifeTimeMgr::recordPos(IR const* ir, Pos pos)
{
    #ifdef _DEBUG_
    m_ir2pos.set(ir, pos);
    #endif
}


void LifeTimeMgr::reset()
{
    Region * rg = m_rg;
    destroy();
    init(rg);
}


void LifeTimeMgr::recomputeLifeTime(UpdatePos & up, BBList const* bblst,
                                    DedicatedMgr const& dedmgr)
{
    reset();
    computeLifeTime(up, bblst, dedmgr);
}


bool LifeTimeMgr::verifyPos(IR const* ir, Pos pos) const
{
    #ifdef _DEBUG_
    Pos p = m_ir2pos.get(ir);
    return p == pos;
    #endif
    return true;
}


void LifeTimeMgr::computeLifeTimeBB(UpdatePos & up, IRBB const* bb,
                                    DedicatedMgr const& dedmgr,
                                    Pos livein_def, IRIter & irit,
                                    MOD CrossedCallCounter & cross_call_counter)
{
    Pos dpos_bb_start = 0, upos_bb_start = 0;
    up.updateAtBBEntry(dpos_bb_start, upos_bb_start);
    m_bb_entry_pos.set(bb->id(), dpos_bb_start);
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter bbirit;
    for (IR * ir = irlst.get_head(&bbirit);
         ir != nullptr; ir = irlst.get_next(&bbirit)) {
        Pos dpos, upos;
        if (!up.updateAtIR(ir, dpos, upos)) {
            continue;
        }
        //Record the call IR and the position info.
        if (ir->isCallStmt() && !CALL_is_intrinsic(ir)) {
            cross_call_counter.addNewCall(dpos);
        }
        computeRHS(ir, *this, upos, livein_def, irit, dedmgr,
                   cross_call_counter);
        computeLHS(ir, *this, dpos, dedmgr, cross_call_counter);
    }
    Pos dpos_bb_end = 0, upos_bb_end = 0;
    up.updateAtBBExit(dpos_bb_end, upos_bb_end);
    m_bb_exit_pos.set(bb->id(), upos_bb_end);
}


void LifeTimeMgr::computeLifeTime(UpdatePos & up, BBList const* bblst,
                                  DedicatedMgr const& dedmgr)
{
    //Create entry position.
    Pos dpos_start, upos_start;
    bool valid = up.updateAtRegionEntry(dpos_start, upos_start);
    ASSERT0_DUMMYUSE(valid);
    CrossedCallCounter cross_call_counter;
    Pos livein_def = dpos_start;
    BBListIter bbit;
    IRIter irit;
    for (IRBB * bb = bblst->get_head(&bbit);
         bb != nullptr; bb = bblst->get_next(&bbit)) {
        computeLifeTimeBB(up, bb, dedmgr, livein_def, irit, cross_call_counter);
    }
    Pos dpos_end, upos_end;
    bool valid2 = up.updateAtRegionExit(dpos_end, upos_end);
    ASSERT0_DUMMYUSE(valid2);
    ASSERT0(verify());
}


static void dumpPhyReg(PRNO prno, LinearScanRA const& ra, Region const* rg)
{
    Reg r = ra.getReg(prno);
    if (r == REG_UNDEF) { return; }
    REGFILE rf = ra.getRegFile(r);
    ASSERT0(rf != RF_UNDEF);
    prt(rg, ":%s(%s)", ra.getRegName(r), ra.getRegFileName(rf));
}


//in_lt: true if ir is part of a lifetime.
static void dumpStmtUsage(IR const* ir, Region const* rg, bool in_lt,
                          Pos dpos, Pos upos, LinearScanRA const& ra,
                          ConstIRIter & irit)
{
    note(rg, "\n");
    //Dump LHS.
    if (in_lt) {
        prt(rg, "[%u] ", dpos);
    } else {
        prt(rg, "[?] ");
    }
    IR * res = const_cast<IR*>(ir)->getResultPR();
    if (res != nullptr) {
        prt(rg, "$%u", res->getPrno());
        dumpPhyReg(res->getPrno(), ra, rg);
    } else { prt(rg, "--"); }

    prt(rg, " <= ");
    if (!in_lt) {
        ASSERT0(ra.isSpillOp(ir) || ra.isReloadOp(ir) ||
                ra.isRematOp(ir) || ra.isMoveOp(ir));
        CHAR const* role = nullptr;
        if (ra.isSpillOp(ir)) { role = "spill"; }
        else if (ra.isReloadOp(ir)) { role = "reload"; }
        else if (ra.isRematOp(ir)) { role = "remat"; }
        else if (ra.isMoveOp(ir)) { role = "move"; }
        else { UNREACHABLE(); }
        prt(rg, "%s == ", role);
    }

    //Dump RHS.
    bool find_readpr = false;
    irit.clean();
    for (IR const* e = xoc::iterExpInitC(ir, irit);
         e != nullptr; e = xoc::iterExpNextC(irit)) {
        if (!e->isReadPR()) { continue; }
        if (find_readpr) {
            prt(rg, " ");
        }
        find_readpr = true;
        prt(rg, "$%u", e->getPrno());
        dumpPhyReg(e->getPrno(), ra, rg);
    }
    if (!find_readpr) {
        prt(rg, "--");
    }
    if (in_lt) {
        prt(rg, " [%u]", upos);
    } else {
        prt(rg, " [?]");
    }
}


static void dumpPROverView(Region const* rg, BBList const* bblst,
                           bool dumpir, UpdatePos & up)
{
    note(rg, "\n==-- DUMP %s --==", "PR OverView");
    Pos dpos_start, upos_start;
    bool valid = up.updateAtRegionEntry(dpos_start, upos_start);
    if (valid) {
        note(rg, "\n[%u] RegionExposedDef <= -- [%u]", dpos_start, upos_start);
    }
    BBListIter bbit;
    ConstIRIter irit;
    for (IRBB * bb = bblst->get_head(&bbit);
         bb != nullptr; bb = bblst->get_next(&bbit)) {
        BBIRList & irlst = bb->getIRList();
        BBIRListIter bbirit;
        bb->dumpDigest(rg);
        Pos dpos_bb_start, upos_bb_start;
        if (up.updateAtBBEntry(dpos_bb_start, upos_bb_start)) {
            note(rg, "\n[%u] ExposedDef <= -- [%u]",
                 dpos_bb_start, upos_bb_start);
        }
        for (IR * ir = irlst.get_head(&bbirit);
             ir != nullptr; ir = irlst.get_next(&bbirit)) {
            Pos upos, dpos;
            bool in_lt = up.updateAtIR(ir, dpos, upos);
            dumpStmtUsage(ir, rg, in_lt, dpos, upos, up.getRA(), irit);
            if (dumpir) {
                rg->getLogMgr()->incIndent(4);
                xoc::dumpIR(ir, rg);
                rg->getLogMgr()->decIndent(4);
            }
        }
        Pos dpos_bb_end, upos_bb_end;
        if (up.updateAtBBExit(dpos_bb_end, upos_bb_end)) {
            note(rg, "\n[%u] -- <= ExposedUse [%u]", dpos_bb_end, upos_bb_end);
        }
        note(rg, "\n");
    }
    Pos dpos_end, upos_end;
    bool valid2 = up.updateAtRegionExit(dpos_end, upos_end);
    if (valid2) {
        note(rg, "\n[%u] RegionExposedUse <= -- [%u]", dpos_end, upos_end);
    }
}


//
//START LTList
void LTList::dump(Region const* rg) const
{
    note(rg, "\n==-- DUMP %s --==", "LTList");
    LTListIter it;
    for (LifeTime const* lt = get_head(&it);
         lt != nullptr; lt = get_next(&it)) {
        lt->dump(rg);
    }
}
//END LTList


void LifeTimeMgr::dump() const
{
    m_lt_list.dump(m_rg);
}


void LifeTimeMgr::dumpAllLT(UpdatePos & up, BBList const* bblst,
                            bool dumpir) const
{
    if (m_lt_list.get_elem_count() == 0) { return; }
    note(m_rg, "\n==-- DUMP ALL LifeTime --==");
    m_rg->getLogMgr()->incIndent(2);
    dumpPROverView(m_rg, bblst, dumpir, up);
    m_lt_list.dump(m_rg);
    m_rg->getLogMgr()->decIndent(2);
}


bool LifeTimeMgr::verify() const
{
    LTListIter it;
    LifeTimeMgr * pthis = const_cast<LifeTimeMgr*>(this);
    for (LifeTime const* lt = pthis->getLTList().get_head(&it);
         lt != nullptr; lt = pthis->getLTList().get_next(&it)) {
        ASSERT0(lt->verify());
    }
    return true;
}


void LifeTimeMgr::renameLifeTimeOcc(LifeTime const* lt, PRNO newprno)
{
    ASSERT0(newprno != PRNO_UNDEF);
    LifeTime * plt = const_cast<LifeTime*>(lt);
    OccListIter it = nullptr;
    for (Occ occ = plt->getOccList().get_head(&it);
         it != plt->getOccList().end(); occ = plt->getOccList().get_next(&it)) {
        ASSERTN(occ.getIR() && !occ.getIR()->is_undef(), ("ilegal occ"));
        IR * ir = occ.getIR();
        ASSERT0(ir->isPROp());
        ir->setPrno(newprno);
    }
}
//END LifeTimeMgr

} //namespace xoc
