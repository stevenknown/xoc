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

namespace xoc {

//
//START PreAssignedMgr
//
void PreAssignedMgr::dump(Region const* rg, TargInfoMgr const& timgr) const
{
    if (get_elem_count() == 0) { return; }
    note(rg, "\n==-- DUMP PreAssignedMgr --==");
    PRNO2RegIter it;
    Reg r;
    for (PRNO p = get_first(it, &r); !it.end(); p = get_next(it, &r)) {
        note(rg, "\n$%u:%s", p, timgr.getRegName(r));
    }
}
//END PreAssignedMgr


//
//START DedicatedMgr
//
void DedicatedMgr::dump(Region const* rg, TargInfoMgr const& timgr) const
{
    if (get_elem_count() == 0) { return; }
    note(rg, "\n==-- DUMP DedicatedMgr --==");
    DualMapPRNO2RegIter it;
    Reg r;
    for (PRNO p = get_first(it, &r); !it.end(); p = get_next(it, &r)) {
        note(rg, "\n$%u:%s", p, timgr.getRegName(r));
    }
}
//END DedicatedMgr


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
    VecIdx near_lessidx;
    VecIdx near_greatidx;
    bool f = findRange(pos, r, rangeidx, &near_lessidx, &near_greatidx);
    if (!f) {
        if (near_greatidx == VEC_UNDEF) {
            //CASE:given pos:3447, LT:$2,range:<3446>
            //In this case, we could not find any properly near greater range.
            return;
        }
        //Current lifetime does not contain range that cover 'pos'.
        //CASE: This case expects that clean the range after the pos 902 until
        //      the end of LT:$1.
        //pos = 902
        //LT:$1,range:<901><3394-3413><3416-3419>
        rangeidx = near_greatidx;
    }
    VecIdx remove_start = VEC_UNDEF;
    if (r.start() == pos || !f) {
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


bool LifeTime::findOccBefore(Pos pos, OUT OccListIter & it) const
{
    OccList & ol = const_cast<LifeTime*>(this)->getOccList();
    OccListIter it2 = nullptr;
    bool res = false;
    for (Occ occ = ol.get_head(&it2); it2 != ol.end();
         occ = ol.get_next(&it2)) {
        ASSERTN(occ.getIR() && !occ.getIR()->is_undef(), ("ilegal occ"));
        if (occ.pos() >= pos) { break; }
        it = it2;
        res = true;
    }
    return res;
}


void LifeTime::addOcc(Occ occ)
{
    ASSERTN(occ.getIR() && !occ.getIR()->is_undef(), ("ilegal occ"));
    m_occ_list.append_tail(occ);
    if (!isDefOcc(occ.getIR())) { return; }
    if (isOccHasDef()) { removeOneDefOnly(); return; }
    setOccHasDef();
    setOneDefOnly(occ);
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
    ASSERT0(lt);

    LifeTime * lt2 = const_cast<LifeTime*>(lt);
    RangeVec const& rv2 = lt2->getRangeVec();

    return is_intersect(rv2);
}


bool LifeTime::is_intersect(RangeVec const& rv2_in) const
{
    LifeTime * lt1 = const_cast<LifeTime*>(this);
    RangeVec const* rv1 = &lt1->getRangeVec();
    RangeVec const* rv2 = &rv2_in;

    UINT i = 0;
    UINT j = 0;
    UINT rv1n = rv1->get_elem_count();
    UINT rv2n = rv2->get_elem_count();
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


static void dumpRangeVecGWithPos(RangeVec const& rv, Region const* rg,
                                 Pos start, Pos end)
{
    Pos prev_end = POS_INIT_VAL;
    for (VecIdx i = 0; i < (VecIdx)rv.get_elem_count(); i++) {
        Range const& r = rv.get(i);
        r.dumpGWithPos(prev_end, rg, start, end);
        prev_end = r.end() + 1;
    }
}


void LifeTime::dumpReg2LifeTime(Region const* rg, Reg r) const
{
    TargInfoMgr const* timgr = rg->getRegionMgr()->getTargInfoMgr();
    ASSERT0(timgr);
    REGFILE rf = timgr->getRegFile(r);
    ASSERT0(rf != RF_UNDEF);
    note(rg, "\n%s(%s):", timgr->getRegName(r), timgr->getRegFileName(rf));
    dumpRangeVec(m_range_vec, rg);
    DUMMYUSE(dumpOccList);
    note(rg, "\n |");
    dumpRangeVecG(m_range_vec, rg);
    note(rg, "\n |");
    dumpOccListG(m_occ_list, rg);
}


void LifeTime::dumpReg2LifeTimeWithPos(
    Region const* rg, Reg r, Pos start, Pos end, bool open_range = false) const
{
    TargInfoMgr const* timgr = rg->getRegionMgr()->getTargInfoMgr();
    ASSERT0(timgr);
    REGFILE rf = timgr->getRegFile(r);
    ASSERT0(rf != RF_UNDEF);
    note(rg, "\n%s(%s):", timgr->getRegName(r), timgr->getRegFileName(rf));
    if (open_range) { dumpRangeVec(m_range_vec, rg); }
    DUMMYUSE(dumpOccList);
    note(rg, "\n |");
    dumpRangeVecGWithPos(m_range_vec, rg, start, end);
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
    ASSERT0(idx >= 0 && idx < (VecIdx)getRangeNum());
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
    m_mdmgr = rg->getMDMgr();
    ASSERT0(m_mdmgr);
    m_max_pos = POS_UNDEF;
    m_lt_list.init();
    m_prno2lt.init();
    m_bb_entry_pos.clean();
    m_bb_exit_pos.clean();
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
    m_max_pos = POS_UNDEF;
}


static void computeLHS(IR * ir, LifeTimeMgr & mgr, Pos pos,
                       PreAssignedMgr const& preassigned_mgr,
                       MOD CrossedCallCounter & cross_call_counter)
{
    ASSERT0(ir && ir->is_stmt());
    IR const* res = const_cast<IR*>(ir)->getResultPR();
    mgr.recordPos(ir, pos);
    if (res == nullptr) { return; }
    PRNO prno = res->getPrno();

    //Call statements must always be considered because they affect both the
    //splitting of lifetimes and the generation of spill&reload IRs.
    if (!ir->isCallStmt() && !mgr.canBeCandidate(prno)) { return; }
    LifeTime * lt = mgr.genLifeTime(prno);
    ASSERT0(lt);
    if (preassigned_mgr.isPreAssigned(prno)) { lt->setPreAssigned(); }
    lt->addRange(pos);
    lt->addOcc(Occ(true, pos, ir));
    cross_call_counter.update(lt);
}


static void computeUSE(IR * ir, LifeTimeMgr & mgr, Pos pos, Pos livein_def,
                       PreAssignedMgr const& preassigned_mgr,
                       MOD CrossedCallCounter & cross_call_counter)
{
    ASSERT0(ir && ir->isPROp());
    PRNO prno = ir->getPrno();
    LifeTime * lt = mgr.genLifeTime(prno);
    ASSERT0(lt);
    if (preassigned_mgr.isPreAssigned(prno)) { lt->setPreAssigned(); }
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
    cross_call_counter.update(lt);
}


static void computeRHS(IR * ir, LifeTimeMgr & mgr, Pos pos,
                       Pos livein_def, IRIter & irit,
                       PreAssignedMgr const& preassigned_mgr,
                       MOD CrossedCallCounter & cross_call_counter)
{
    ASSERT0(ir && ir->is_stmt());
    irit.clean();
    for (IR * e = xoc::iterExpInit(ir, irit); e != nullptr;
         e = xoc::iterExpNext(irit)) {
        if (e->isReadPR() && mgr.canBeCandidate(e->getPrno())) {
            computeUSE(e, mgr, pos, livein_def, preassigned_mgr,
                       cross_call_counter);
        }
    }
}


void CrossedCallCounter::update(LifeTime * lt)
{
    ASSERT0(lt);
    if (m_call2pos.get_elem_count() == 0 ||
        lt->getCallCrossedNum() >= CROSS_CALL_NUM_THRESHOLD) {
        //Implement the 2.1 of the algorithm.
        //Don't update the call_crossed_num if there is no call or the
        //call_crossed_num is greater than the CROSS_CALL_NUM_THRESHOLD.
        return;
    }

    PRNO prno = lt->getPrno();
    bool find = false;
    VecIdx baseline_call_id = m_prno2callid.get(prno, &find);

    //Implement the 2.2.1 and 2.2.2 of the algorithm.
    //If this prno doesn't record any call id, we have to traverse the call
    //from the beginning, or else just start to traverse the call from the
    //baseline call id recorded before.
    baseline_call_id = find ? baseline_call_id + 1 : 0;
    VecIdx current_call_id = m_call2pos.get_last_idx();

    //Implement the 2.2.3 of the algorithm.
    for (VecIdx i = baseline_call_id; i <= current_call_id; i++) {
        if (!lt->is_contain(m_call2pos[i])) { continue; }
        //Implement the 2.2.3.1 of the algorithm.
        lt->incCallCrossedNum(1);
    }
    m_prno2callid.setAlways(prno, current_call_id);
}


bool LifeTimeMgr::canBeCandidate(PRNO prno)
{
    RA_STRATEGY strategy = getStrategy();
    if (strategy == RA_STRATEGY_FULL) { return true; }
    Var * var = m_rg->getVarByPRNO(prno);
    ASSERT0(var);
    Type const* tp = var->getType();
    ASSERT0(tp);
    return tp->is_vector() ? strategy == RA_STRATEGY_VECTOR :
        strategy == RA_STRATEGY_SCALAR;
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
                                    PreAssignedMgr const& preassigned_mgr)
{
    reset();
    computeLifeTime(up, bblst, preassigned_mgr);
}


bool LifeTimeMgr::verifyPos(IR const* ir, Pos pos) const
{
    #ifdef _DEBUG_
    Pos p = m_ir2pos.get(ir);
    return p == pos;
    #endif
    return true;
}


void LifeTimeMgr::updateBBExitPos(IRBB const* bb, MOD UpdatePos & up,
    MOD Pos & dpos_bb_end, MOD Pos & upos_bb_end)
{
    up.updateAtBBExit(dpos_bb_end, upos_bb_end);
    m_bb_exit_pos.set(bb->id(), upos_bb_end);
}


void LifeTimeMgr::updateBBEntryPos(IRBB const* bb, MOD UpdatePos & up,
    MOD Pos & dpos_bb_start, MOD Pos & upos_bb_start)
{
    up.updateAtBBEntry(dpos_bb_start, upos_bb_start);
    m_bb_entry_pos.set(bb->id(), dpos_bb_start);
}


void LifeTimeMgr::computeLifeTimeBB(UpdatePos & up, IRBB const* bb,
                                    PreAssignedMgr const& preassigned_mgr,
                                    Pos livein_def, IRIter & irit,
                                    MOD CrossedCallCounter & cross_call_counter)
{
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
            addCallerLTPos(dpos, ir);
        }
        computeRHS(ir, *this, upos, livein_def, irit, preassigned_mgr,
                   cross_call_counter);
        computeLHS(ir, *this, dpos, preassigned_mgr, cross_call_counter);
    }
}


void LifeTimeMgr::computeLifeTime(UpdatePos & up, BBList const* bblst,
                                  PreAssignedMgr const& preassigned_mgr)
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
        Pos dpos_bb_start = 0, upos_bb_start = 0;
        Pos dpos_bb_end = 0, upos_bb_end = 0;

        //Update the def pos and the use pos at the entry of BB.
        updateBBEntryPos(bb, up, dpos_bb_start, upos_bb_start);

        computeLifeTimeBB(up, bb, preassigned_mgr, livein_def, irit,
                          cross_call_counter);

        //Update the def pos and the use pos at the exit of BB.
        updateBBExitPos(bb, up, dpos_bb_end, upos_bb_end);
    }
    Pos dpos_end, upos_end;
    bool valid2 = up.updateAtRegionExit(dpos_end, upos_end);

    //The max position will be used to calculte the distance between any
    //position inside the region and the end of the region.
    setMaxPos(upos_end);
    ASSERT0_DUMMYUSE(valid2);
    ASSERT0(verify());
}


static void dumpPhyReg(Region const* rg, RegAllocMgr * ramgr, PRNO prno)
{
    Reg r = ramgr->getReg(prno);
    if (r == REG_UNDEF) { return; }
    TargInfoMgr const* timgr = rg->getRegionMgr()->getTargInfoMgr();
    ASSERT0(timgr);
    REGFILE rf = timgr->getRegFile(r);
    ASSERT0(rf != RF_UNDEF);
    prt(rg, ":%s(%s)", timgr->getRegName(r), timgr->getRegFileName(rf));
}


//in_lt: true if ir is part of a lifetime.
static void dumpStmtUsage(
    Region const* rg, RegAllocMgr * ramgr, IR const* ir, bool in_lt, Pos dpos,
    Pos upos, ConstIRIter & irit)
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
        dumpPhyReg(rg, ramgr, res->getPrno());
    } else { prt(rg, "--"); }

    prt(rg, " <= ");
    if (!in_lt) {
        ASSERT0(ramgr->isOpInPosGap(ir));
        CHAR const* role = nullptr;
        if (ramgr->isSpillOp(ir)) { role = "spill"; }
        else if (ramgr->isReloadOp(ir)) { role = "reload"; }
        else if (ramgr->isRematOp(ir)) { role = "remat"; }
        else if (ramgr->isMoveOp(ir)) { role = "move"; }
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
        dumpPhyReg(rg, ramgr, e->getPrno());
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


static void dumpPROverView(
    Region const* rg, RegAllocMgr * ramgr, BBList const* bblst, bool dumpir,
    UpdatePos & up)
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
            dumpStmtUsage(rg, ramgr, ir, in_lt, dpos, upos, irit);
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
//
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


LifeTimeMgr::LifeTimeMgr(Region * rg)
{
    m_pool = nullptr;
    m_use_expose = false;
    m_strategy = RA_STRATEGY_UNDEF;
    m_ramgr = (RegAllocMgr*)rg->getPassMgr()->registerPass(PASS_REGALLOC_MGR);
    init(rg);
}


void LifeTimeMgr::dump() const
{
    m_lt_list.dump(m_rg);
}


void LifeTimeMgr::dumpAllLT(UpdatePos & up, BBList const* bblst,
                            bool dumpir) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    if (m_lt_list.get_elem_count() == 0) { return; }
    note(m_rg, "\n==-- DUMP ALL LifeTime --==");
    m_rg->getLogMgr()->incIndent(2);
    dumpPROverView(m_rg, m_ramgr, bblst, dumpir, up);
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
        getMDMgr()->allocRef(ir);
    }
}


bool LifeTimeMgr::canMergeWithPreRange(RangeVec & range_vec, UINT last_idx,
                                       Range & cur_range)
{
    ASSERT0(last_idx <= range_vec.get_elem_count());

    //It is the first range in 'range_vec'.
    if (last_idx == 0) { return false; }

    //Get the last range in 'range_vec'.
    Range r_pre = range_vec.get((VecIdx)(last_idx - 1));
    if (r_pre.end() < cur_range.start()) { return false; }

    //CASE: 'cur_range.start() is_interect range_vec.get(last_idx)',
    //      thus these two range can be merged into a range.
    //range_vec:   | [S    E]    |
    //cur_range:   |    [S    E] |
    //after_merge: | [S       E] |
    ASSERT0(r_pre.is_contain(cur_range.start()));
    r_pre.setEnd(cur_range.end());
    return true;
}
//END LifeTimeMgr


//
//START LifeTime2DMgr
//
void LifeTime2DMgr::mergeLiveIn(IRBB const* bb, UpdatePos & up,
                                Pos dpos_bb_start)
{
    ASSERT0(bb);
    PRLiveSet * live_in = m_live_mgr->get_livein(bb->id());
    ASSERT0(live_in);
    PRLiveSetIter * iter = nullptr;

    for (UINT id = (UINT)live_in->get_first(&iter);
         id != BS_UNDEF; id = (UINT)live_in->get_next(id, &iter)) {
        if (!canBeCandidate(id)) { continue; }
        LifeTime * lt = genLifeTime(id);
        ASSERT0(lt);

        //Add the exposed-def of current BB to the lifetime if it is the
        //livein of the current BB.
        lt->addRange(dpos_bb_start);
    }
}


void LifeTime2DMgr::mergeLiveOut(IRBB const* bb, UpdatePos & up,
                                 Pos livein_def, Pos upos_bb_end)
{
    ASSERT0(bb);
    PRLiveSet * live_out = m_live_mgr->get_liveout(bb->id());
    ASSERT0(live_out);
    PRLiveSetIter * iter = nullptr;

    for (UINT id = (UINT)live_out->get_first(&iter);
         id != BS_UNDEF; id = (UINT)live_out->get_next(id, &iter)) {
        if (!canBeCandidate(id)) { continue; }
        LifeTime * lt = genLifeTime(id);
        ASSERT0(lt);
        Range r = lt->getLastRange();
        ASSERT0(r.start() != POS_UNDEF);

        //Modify the lifetime to extend it to the exposed-use of the current BB
        //if it is the liveout of the current BB.
        RG_end(r) = upos_bb_end;
        lt->setLastRange(r);
    }
}


void LifeTime2DMgr::computeLifeTime(UpdatePos & up, BBList const* bblst,
                                    PreAssignedMgr const& preassigned_mgr)
{
    m_live_mgr = (LivenessMgr*)m_rg->getPassMgr()->queryPass(
        PASS_PRLIVENESS_MGR);
    ASSERT0(m_live_mgr);

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
        Pos dpos_bb_start = 0, upos_bb_start = 0;
        Pos dpos_bb_end = 0, upos_bb_end = 0;
        updateBBEntryPos(bb, up, dpos_bb_start, upos_bb_start);

        //Merge the 2D livein info at the entry of BB.
        mergeLiveIn(bb, up, dpos_bb_start);

        //Compute the normal 1D lifetime .
        computeLifeTimeBB(up, bb, preassigned_mgr, livein_def, irit,
                          cross_call_counter);

        updateBBExitPos(bb, up, dpos_bb_end, upos_bb_end);

        //Merge the 2D liveout info at the exit of BB.
        mergeLiveOut(bb, up,livein_def, upos_bb_end);
    }
    Pos dpos_end, upos_end;
    bool valid2 = up.updateAtRegionExit(dpos_end, upos_end);
    setMaxPos(upos_end);
    ASSERT0_DUMMYUSE(valid2);
    ASSERT0(verify());
}
//END LifeTime2DMgr


//
//START RegLifeTimeMgr
//
RegLifeTimeMgr::~RegLifeTimeMgr()
{
    for (LifeTime * lt = m_reg2lt_list.get_head(); lt != nullptr;
         lt = m_reg2lt_list.get_next()) {
        delete lt;
    }
    m_reg2lt_list.destroy();
}


void RegLifeTimeMgr::initReg2LifeTimeInfo()
{
    ASSERT0(m_rg && m_rg->getRegionMgr());
    ASSERT0(m_rg->getRegionMgr()->getTargInfoMgr());

    TargInfoMgr const* tim = m_rg->getRegionMgr()->getTargInfoMgr();
    ASSERT0(tim);

    UINT reg_num = tim->getNumOfRegister();
    for (Reg r = REG_UNDEF + 1; r < reg_num; r++) {
        //Allocate lifetime with dummy PRNO. Since there isn't a PRNO provided
        //for allocated lifetime in this reg2lt process, a dummy PRNO generated
        //via '0xFFFFFFFF - r'.
        LifeTime * lt = allocReg2LifeTime((PRNO)(0xFFFFFFFF - (UINT)r));
        ASSERT0(lt);
        m_reg2lt.set(r, lt);
    }
}


LifeTime * RegLifeTimeMgr::allocReg2LifeTime(PRNO prno)
{
    LifeTime * lt = new LifeTime(prno);
    m_reg2lt_list.append_tail(lt);
    return lt;
}


void RegLifeTimeMgr::addCallerLTPos(Pos pos, IR const* ir)
{
    ASSERT0(ir);
    ASSERT0(pos != POS_UNDEF);
    xgen::RegSet const* caller_set = m_rg->getRegionMgr()->getTargInfoMgr()->
        getCallerRegSet();
    PRNO pr = CALL_prno(ir);
    Reg def_reg = REG_UNDEF;
    if (pr != PRNO_UNDEF) { def_reg = m_ramgr->getReg(pr); }
    for (BSIdx i = caller_set->get_first(); i != BS_UNDEF;
         i = caller_set->get_next(i)) {
        if (i == def_reg) { continue; }
        LifeTime * lt = getRegLifeTime(Reg(i));
        lt->addRange(pos);
        lt->addOcc(Occ(true, pos, const_cast<IR*>(ir)));
    }
}


void RegLifeTimeMgr::mergeRegLifetimeWithRange(Reg reg,
    RangeVec const& range_vec)
{
    ASSERT0(reg != REG_UNDEF);
    //Insert 'range_vec' into the corresponding position of RangeVec of 'reg'.
    LifeTime * lt_ori = getRegLifeTime(reg);
    ASSERT0(lt_ori);
    //Insert 'range_vec' into the RangeVec of 'lt_ori'.
    mergeLifeTime(lt_ori->getRangeVec(), range_vec);

    //Consider the alias register.
    TargInfoMgr * tg = m_rg->getRegionMgr()->getTargInfoMgr();
    SRegSet const* alias_regset = tg->getAliasRegSet(reg);
    if (alias_regset == nullptr) { return; }

    //Merge the range into all alias registers.
    RegDSetIter iter;
    for (BSIdx i = alias_regset->get_first(&iter); i != BS_UNDEF;
         i = alias_regset->get_next(i, &iter)) {
        if (i == reg) { continue; }
        LifeTime * lt = getRegLifeTime((Reg)i);
        ASSERT0(lt);
        //Insert 'range_vec' into the RangeVec of 'lt'.
        mergeLifeTime(lt->getRangeVec(), range_vec);
    }
}


void RegLifeTimeMgr::mergeLifeTime(RangeVec & rv_ori, RangeVec const& rv_new)
{
    //Merge 'lt_new' with original lifetime.
    //1.Compute the total size of these two lifetime.
    UINT total_size = rv_ori.get_elem_count() + rv_new.get_elem_count();
    RangeVec vec;
    vec.init(total_size);

    UINT vec_idx = 0;
    UINT vec_new_idx = 0;
    UINT vec_ori_idx = 0;
    for (; vec_idx < total_size; vec_idx++) {
        if (vec_new_idx >= rv_new.get_elem_count() ||
            vec_ori_idx >= rv_ori.get_elem_count()) { break; }

        //2.Get the Range from 'rv_ori' and 'rv_new' respectively.
        Range r_ori = rv_ori.get((VecIdx)vec_ori_idx);
        Range r_new = rv_new.get((VecIdx)vec_new_idx);

        //3.Case: 'r_ori.start() > r_new.end()'.
        //  r_ori: |        [S  E] |
        //  r_new: | [S  E]        |
        if (r_ori.is_great(r_new)) {
            if (canMergeWithPreRange(vec, vec_idx, r_new)) {
                //3.1.Case: 'r_ori.start() > r_new.end() &&
                //          'r_new.start() is_interect r_pre'.
                //r_pre: | [S  E]
                //r_ori: |           [S  E] |
                //r_new: |    [S  E]        |
                //after: | [S     E]        |
                vec_new_idx++;
                continue;
            }

            //3.2.Case: 'r_ori.start() > r_new.end() &&
            //          'r_new.start() no_interect r_pre'.
            //r_pre: | [S  E]
            //r_ori: |               [S  E] |
            //r_new: |        [S  E]        |
            //after: | [S  E] [S  E]        |
            vec.set(vec_idx, r_new);
            vec_new_idx++;
            continue;
        }

        //4.Case: 'r_ori.end() < r_new.start()'.
        //  r_ori: | [S  E]        |
        //  r_new: |        [S  E] |
        if (r_ori.is_less(r_new)) {
            if (canMergeWithPreRange(vec, vec_idx, r_ori)) {
                //4.1.Case: 'r_ori.end() < r_new.start() &&
                //          'r_ori.start() is_interect r_pre'.
                //r_pre: | [S  E]           |
                //r_ori: |    [S  E]        |
                //r_new: |           [S  E] |
                //after: | [S     E]        |
                vec_ori_idx++;
                continue;
            }

            //4.2.Case: 'r_ori.end() < r_new.start() &&
            //          'r_ori.start() no_interect r_pre'.
            //r_pre: | [S  E]               |
            //r_ori: |        [S  E]        |
            //r_new: |               [S  E] |
            //after: | [S  E] [S  E]        |
            vec.set(vec_idx, r_ori);
            vec_ori_idx++;
            continue;
        }

        ASSERT0(!r_ori.is_intersect(r_new));
    }

    //Process the rest Range in 'lt_new'.
    if (vec_new_idx < rv_new.get_elem_count()) {
        for (UINT i = vec_new_idx; i < rv_new.get_elem_count(); i++) {
            ASSERT0(vec_idx < total_size);
            Range r = rv_new.get((VecIdx)i);
            vec.set(vec_idx++, r);
        }
    }

    //Process the rest Range in 'rv_ori'.
    if (vec_ori_idx < rv_ori.get_elem_count()) {
        for (UINT i = vec_ori_idx; i < rv_ori.get_elem_count(); i++) {
            ASSERT0(vec_idx < total_size);
            Range r = rv_ori.get((VecIdx)i);
            vec.set(vec_idx++, r);
        }
    }

    //Reset the 'rv_ori' RangeVec with 'vec'.
    rv_ori.clean();
    rv_ori.copy(vec);
}
//END RegLifeTimeMgr

} //namespace xoc
