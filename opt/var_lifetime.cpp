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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

static void dumpRangeVec(RangeVec const& rv, Region const* rg)
{
    for (VecIdx i = 0; i < (VecIdx)rv.get_elem_count(); i++) {
        Range const& r = rv.get(i);
        r.dump(rg);
    }
}


//in_lt: true if ir is part of a lifetime.
static void dumpStmtUsage(IR const* ir, Region const* rg, bool in_lt,
                          Pos dpos, Pos upos, LinearScanRA const& ra,
                          ConstIRIter & irit)
{
    ASSERT0(ir);
    note(rg, "\n");
    //Dump LHS.
    if (in_lt) {
        prt(rg, "[%u] ", dpos);
    } else {
        prt(rg, "[?] ");
    }
    if (ra.isSpillOp(ir)) {
        Var const* v = ir->getIdinfo();
        ASSERT0(v);
        prt(rg, "V%u,%s", v->id(), v->get_name()->getStr());
    } else {
        prt(rg, "--");
    }

    prt(rg, " <-- ");
    if (!in_lt) {
        ASSERT0(0);
        ASSERT0(ra.isOpInPosGap(ir));
        CHAR const* role = nullptr;
        if (ra.isSpillOp(ir)) { role = "spill"; }
        else if (ra.isReloadOp(ir)) { role = "reload"; }
        else if (ra.isRematOp(ir)) { role = "remat"; }
        else if (ra.isMoveOp(ir)) { role = "move"; }
        else { UNREACHABLE(); }
        prt(rg, "%s == ", role);
    }

    //Dump RHS.
    if (ra.isReloadOp(ir)) {
        Var const* v = ir->getRHS()->getIdinfo();
        ASSERT0(v);
        prt(rg, "V%u,%s", v->id(), v->get_name()->getStr());
    } else {
        prt(rg, "--");
    }

    if (in_lt) {
        prt(rg, " [%u]", upos);
    } else {
        prt(rg, " [?]");
    }
}


static void dumpVarOverView(Region const* rg, BBList const* bblst,
                            bool dumpir, VarUpdatePos & up)
{
    note(rg, "\n==-- DUMP %s --==", "Var OverView");
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
            dumpStmtUsage(ir, rg, in_lt, dpos, upos, *up.getRA(), irit);
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
//START VarLifeTime
//
void VarLifeTime::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    ASSERT0(rg);
    note(rg, "\nVarLT:%u, Var%u:\n", m_id, m_var_id);
    note(rg, "\n  LifeTime: ");
    dumpRangeVec(m_range_vec, rg);
}


bool VarLifeTime::isIntersect(RangeVec const* rv1, RangeVec const* rv2)
{
    ASSERT0(rv1 && rv2);
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
//END VarLifeTime


//
//START VarLifeTimeMgr
//
void VarLifeTimeMgr::dump() const
{
    m_lt_list.dump(m_rg);
    BBList const* bblst = m_rg->getBBList();
    VarUpdatePos up(&const_cast<VarLifeTimeMgr*>(this)->getLSRA());
    dumpVarOverView(m_rg, bblst, false, up);
}


void VarLifeTimeMgr::computeLifeTimeBB(UpdatePos & up, IRBB const* bb,
                                       Pos livein_def, IRIter & irit)
{
    ASSERT0(bb);
    VarLiveSet * live_in = m_var_liveness->get_livein(bb->id());
    VarLiveSet * live_out = m_var_liveness->get_liveout(bb->id());
    ASSERT0(live_in && live_out);

    VarLiveSetIter * iter = nullptr;
    Pos dpos_bb_start = 0, upos_bb_start = 0;
    up.updateAtBBEntry(dpos_bb_start, upos_bb_start);
    m_bb_entry_pos.set(bb->id(), dpos_bb_start);

    for (UINT id = (UINT)live_in->get_first(&iter);
         id != BS_UNDEF; id = (UINT)live_in->get_next(id, &iter)) {
        VarLifeTime * var_lt = genLifeTime(id);
        ASSERT0(var_lt);

        //Add the exposed-def of current BB to the lifetime if it is the livein
        //of the current BB.
        var_lt->addRange(dpos_bb_start);
    }

    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter bbirit;
    for (IR * ir = irlst.get_head(&bbirit);
         ir != nullptr; ir = irlst.get_next(&bbirit)) {
        Pos dpos, upos;
        if (!up.updateAtIR(ir, dpos, upos)) {
            continue;
        }
        computeLifeTimeStmt(ir, dpos, upos, livein_def);
    }

    Pos dpos_bb_end = 0, upos_bb_end = 0;
    up.updateAtBBExit(dpos_bb_end, upos_bb_end);
    m_bb_exit_pos.set(bb->id(), upos_bb_end);

    for (UINT id = (UINT)live_out->get_first(&iter);
         id != BS_UNDEF; id = (UINT)live_out->get_next(id, &iter)) {
        VarLifeTime * var_lt = genLifeTime(id);
        ASSERT0(var_lt);
        Range r = var_lt->getLastRange();
        ASSERT0(r.start() != POS_UNDEF);

        //Modify the lifetime to extend it to the exposed-use of the current BB
        //if it is the liveout of the current BB.
        RG_end(r) = upos_bb_end;
        var_lt->setLastRange(r);
    }
}


void VarLifeTimeMgr::computeLifeTime()
{
    VarUpdatePos up(nullptr);
    Pos dpos_start, upos_start;
    bool valid = up.updateAtRegionEntry(dpos_start, upos_start);
    ASSERT0_DUMMYUSE(valid);

    Pos livein_def = dpos_start;
    BBList const* bblst = m_rg->getBBList();
    BBListIter bbit;
    IRIter irit;
    for (IRBB * bb = bblst->get_head(&bbit);
         bb != nullptr; bb = bblst->get_next(&bbit)) {
        computeLifeTimeBB(up, bb, livein_def, irit);
    }
    Pos dpos_end, upos_end;
    bool valid2 = up.updateAtRegionExit(dpos_end, upos_end);
    ASSERT0_DUMMYUSE(valid2);
}


void VarLifeTimeMgr::computeLifeTimeStmt(IR * ir, Pos dpos, Pos upos,
                                         Pos livein_pos)
{
    ASSERT0(ir);
    computeStmtRHS(ir, upos, livein_pos);
    computeStmtLHS(ir, dpos);
}


void VarLifeTimeMgr::computeStmtLHS(IR * ir, Pos pos)
{
    ASSERT0(ir && ir->is_stmt());
    if (!isNeedComputeLHS(ir)) { return; }
    Var const* v = ir->getIdinfo();
    ASSERT0(v);
    VarLifeTime * var_lt = genLifeTime(v->id());
    var_lt->addRange(pos);
}


void VarLifeTimeMgr::computeStmtRHS(IR * ir, Pos pos, Pos livein_def)
{
    ASSERT0(ir && ir->is_stmt());
    if (!isNeedComputeRHS(ir)) { return; }
    Var const* v = ir->getRHS()->getIdinfo();
    ASSERT0(v);
    VarLifeTime * var_lt = genLifeTime(v->id());
    Range r = var_lt->getLastRange();
    if (r.start() == POS_UNDEF) {
        //PR is region livein.
        RG_start(r) = livein_def;
        RG_end(r) = pos;
    } else {
        RG_end(r) = pos;
    }
    var_lt->setLastRange(r);
}


bool VarLifeTimeMgr::isNeedComputeLHS(IR const* ir) const
{
    return getLSRA().isSpillOp(ir);
}


bool VarLifeTimeMgr::isNeedComputeRHS(IR const* ir) const
{
    return getLSRA().isReloadOp(ir);
}
//END VarLifeTimeMgr

} //namespace xoc
