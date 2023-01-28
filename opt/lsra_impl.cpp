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
#include "lsra_impl.h"
#include "lsra_scan_in_pos.h"
#include "lt_prio_mgr.h"
#include "lsra_scan_in_prio.h"

namespace xoc {

static void dumpInsertMove(LSRAImpl & lsra, IRBB const* mvbb, IR const* mv,
                           CHAR const* format, ...)
{
    StrBuf buf(64);
    va_list args;
    va_start(args, format);
    buf.vstrcat(format, args);
    va_end(args);
    lsra.getActMgr().dump(
        "INSERT_MV:insert move ir id:%u at BB%u, reason:%s",
        mv->id(), mvbb->id(), buf.buf);
}


static void dumpInsertBB(LSRAImpl & lsra, IRBB const* from, IRBB const* to,
                         IRBB const* newbb, CHAR const* format, ...)
{
    StrBuf buf(64);
    va_list args;
    va_start(args, format);
    buf.vstrcat(format, args);
    va_end(args);
    lsra.getActMgr().dump(
        "INSERT_BB:insert BB%u between BB%u and BB%u, reason:%s",
        newbb->id(), from->id(), to->id(), buf.buf);
}


static void dumpSplitTwo(LSRAImpl & lsra, LifeTime const* lt,
                         LifeTime const* newlt,
                         Pos lt_end_pos, Pos newlt_start_pos)
{
    lsra.getActMgr().dump(
        "SPLIT:$%u into $%u and $%u, $%u end at pos:%u, $%u start from pos:%u",
        lt->getPrno(), lt->getPrno(), newlt->getPrno(),
        lt->getPrno(),  lt_end_pos,
        newlt->getPrno(), newlt_start_pos);
}


static void dumpSpill(LSRAImpl & lsra, IR const* spill, Reg reg,
                      IRBB const* bb, CHAR const* format, ...)
{
    if (format != nullptr) {
        StrBuf buf(64);
        va_list args;
        va_start(args, format);
        buf.vstrcat(format, args);
        va_end(args);
        lsra.getActMgr().dump(
            "SPILL:insert spill ir id:%u at BB%u to spill %s, reason:%s",
            spill->id(), bb->id(), lsra.getTIMgr().getRegName(reg), buf.buf);
        return;
    }
    lsra.getActMgr().dump(
        "SPILL:insert spill ir id:%u at BB%u to spill %s",
        spill->id(), bb->id(), lsra.getTIMgr().getRegName(reg));
}


static void dumpSpill(LSRAImpl & lsra, IR const* spill, LifeTime const* lt,
                      IR const* marker, bool before)
{
    lsra.getActMgr().dump(
        "SPILL:insert spill ir id:%u %s ir id:%u while splitting $%u",
        spill->id(), before ? "before" : "after",
        marker->id(), lt->getPrno());
}


static void dumpReload(LSRAImpl & lsra, IR const* reload, Reg reg,
                       IRBB const* bb, CHAR const* format, ...)
{
    if (format != nullptr) {
        StrBuf buf(64);
        va_list args;
        va_start(args, format);
        buf.vstrcat(format, args);
        va_end(args);
        lsra.getActMgr().dump(
            "RELOAD:insert reload ir id:%u at BB%u to reload %s, reason:%s",
            reload->id(), bb->id(), lsra.getTIMgr().getRegName(reg),
            buf.buf);
        return;
    }
    lsra.getActMgr().dump(
        "RELOAD:insert reload ir id:%u at BB%u to reload %s",
        reload->id(), bb->id(), lsra.getTIMgr().getRegName(reg));
}


static void dumpReload(LSRAImpl & lsra, IR const* reload, LifeTime const* lt,
                       LifeTime const* newlt, IR const* marker)
{
    lsra.getActMgr().dump(
        "RELOAD:insert reload ir id:%u before ir id:%u "
        "while splitting $%u, $%u rename to $%u",
        reload->id(), marker->id(), lt->getPrno(), lt->getPrno(),
        newlt->getPrno());
}


static void dumpAssign(LSRAImpl & lsra, LifeTime const* lt,
                       CHAR const* format, ...)
{
    Reg r = lsra.getReg(lt);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        StrBuf buf(64);
        buf.vstrcat(format, args);
        va_end(args);
        lsra.getActMgr().dump("ASSIGN:$%u with %s, reason:%s",
            lt->getPrno(), lsra.getRegName(r), buf.buf);
    } else {
        lsra.getActMgr().dump("ASSIGN:$%u with %s",
            lt->getPrno(), lsra.getRegName(r));
    }
}


static void dumpSelectSplitCand(LSRAImpl & lsra, LifeTime const* lt,
                                Pos split_pos,
                                bool canbe, CHAR const* format, ...)
{
    if (canbe) {
        if (format != nullptr) {
            va_list args;
            va_start(args, format);
            StrBuf buf(64);
            buf.vstrcat(format, args);
            va_end(args);
            lsra.getActMgr().dump(
                "SELECT_SPLIT_CAND:$%u is split-candidate at pos:%u, reason:%s",
                lt->getPrno(), split_pos, buf.buf);
        } else {
            lsra.getActMgr().dump(
                "SELECT_SPLIT_CAND:$%u is split-candidate at pos:%u",
                lt->getPrno(), split_pos);
        }
        return;
    }
    //Reason is necessary.
    ASSERT0(format);
    va_list args;
    va_start(args, format);
    StrBuf buf(64);
    buf.vstrcat(format, args);
    va_end(args);
    lsra.getActMgr().dump(
        "SELECT_SPLIT_CAND:$%u can NOT be splitted at pos:%u, reason:%s",
        lt->getPrno(), split_pos, buf.buf);
}


static LifeTime * pickFromSet(PRNO prno, MOD LTSet & set)
{
    LTSetIter it;
    LTSetIter nit;
    for (set.get_head(&it), nit = it; it != nullptr; it = nit) {
        set.get_next(&nit);
        LifeTime * lt = it->val();
        if (lt->getPrno() == prno) {
            set.remove(it);
            return lt;
        }
    }
    return nullptr;
}


//Pick up a physical register from allocable register set.
static Reg pickReg(RegSet & set)
{
    BSIdx i = set.get_first();
    if (i == BS_UNDEF) {
        return REG_UNDEF;
    }
    set.diff(i);
    return (Reg)i;
}


static Reg pickReg(RegSet & set, Reg r)
{
    if (set.is_contain(r)) {
        set.diff(r);
        return r;
    }
    return REG_UNDEF;
}


//
//START LTConsistencyMgr
//
LTConsistencyMgr::LTConsistencyMgr(LSRAImpl & impl) : m_impl(impl)
{
    m_rg = impl.getRegion();
    m_bb_list = impl.getBBList();
    m_cfg = impl.getCFG();
    m_oc = impl.getOptCtx();
    m_is_insert_bb = false;
}

void LTConsistencyMgr::dump() const
{
    Region const* rg = m_rg;
    BBList const* bblst = m_bb_list;
    note(rg, "\n==-- DUMP %s --==", "LTConsistencyMgr");
    UINT ind = 2;
    rg->getLogMgr()->incIndent(ind);
    BBListIter cb;
    for (IRBB * bb = bblst->get_head(&cb); bb != nullptr;
         bb = bblst->get_next(&cb)) {
        UINT bbid = bb->id();
        note(rg, "\n\n-- BB%d --", bbid);
        LT2ST const* intab = getInTab(bbid);
        LT2STIter it;
        CONSIST_STATUS st;
        note(rg, "\nLT2ST-IN:");
        for (LifeTime const* lt = intab->get_first(it, &st);
             lt != nullptr; lt = intab->get_next(it, &st)) {
            ASSERT0(st != CONSIST_UNDEF);
            note(rg, "\n$%u:%s", lt->getPrno(), getStName(st));
        }
        LT2ST const* outtab = getOutTab(bbid);
        note(rg, "\nLT2ST-OUT:");
        for (LifeTime const* lt = outtab->get_first(it, &st);
             lt != nullptr; lt = outtab->get_next(it, &st)) {
            ASSERT0(st != CONSIST_UNDEF);
            note(rg, "\n$%u:%s", lt->getPrno(), getStName(st));
        }
    }
    rg->getLogMgr()->decIndent(ind);
}


void LTConsistencyMgr::computeLTConsistency()
{
    BBListIter bbit;
    for (IRBB * bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        xcom::List<LifeTime const*>::Iter ltit;
        Pos startpos = m_impl.getLTMgr().getBBStartPos(bb->id());
        Pos endpos = m_impl.getLTMgr().getBBEndPos(bb->id());
        for (LifeTime const* newlt = m_impl.getSplittedLTList().get_head(&ltit);
             newlt != nullptr;
             newlt = m_impl.getSplittedLTList().get_next(&ltit)) {
            LifeTime const* anct = newlt->getAncestor();
            ASSERT0(anct);
            if (newlt->is_contain(startpos)) {
                addInStatus(newlt, LTConsistencyMgr::CONSIST_VALID, bb->id());
            } else {
                addInStatus(newlt, LTConsistencyMgr::CONSIST_INVALID, bb->id());
            }
            if (anct->is_cover(startpos)) {
                addInStatus(anct, LTConsistencyMgr::CONSIST_VALID, bb->id());
            } else {
                addInStatus(anct, LTConsistencyMgr::CONSIST_INVALID, bb->id());
            }
            if (newlt->is_cover(endpos)) {
                addOutStatus(newlt, LTConsistencyMgr::CONSIST_VALID, bb->id());
            } else {
                addOutStatus(newlt, LTConsistencyMgr::CONSIST_INVALID, bb->id());
            }
            if (anct->is_cover(endpos)) {
                addOutStatus(anct, LTConsistencyMgr::CONSIST_VALID, bb->id());
            } else {
                addOutStatus(anct, LTConsistencyMgr::CONSIST_INVALID, bb->id());
            }
        }
    }
}


void LTConsistencyMgr::computeEdgeConsistency(
    OUT InConsistPairList & inconsist_lst)
{
    List<LifeTime const*> const& splitlst = m_impl.getSplittedLTList();
    xcom::EdgeIter it;
    for (Edge * e = m_cfg->get_first_edge(it); e != nullptr;
         e = m_cfg->get_next_edge(it)) {
        List<LifeTime const*>::Iter ltit;
        for (LifeTime const* newlt = splitlst.get_head(&ltit);
             newlt != nullptr; newlt = splitlst.get_next(&ltit)) {
            LifeTime const* anct = newlt->getAncestor();
            ASSERT0(anct);
            Vertex const* from = e->from();
            Vertex const* to = e->to();
            CONSIST_STATUS fromst = getOutSt(newlt, from->id());
            CONSIST_STATUS tost = getInSt(newlt, to->id());
            if (tost == CONSIST_VALID && fromst != CONSIST_VALID) {
                ASSERT0(getOutSt(anct, from->id()) == CONSIST_VALID);
                //Insert copy anct->newlt.
                InConsistPair pair;
                pair.from_vex_id = from->id();
                pair.to_vex_id = to->id();
                pair.from_lt = anct;
                pair.to_lt = newlt;
                inconsist_lst.append_tail(pair);
                continue;
            }

            fromst = getOutSt(anct, from->id());
            tost = getInSt(anct, to->id());
            if (tost == CONSIST_VALID && fromst != CONSIST_VALID) {
                ASSERT0(getOutSt(newlt, from->id()) == CONSIST_VALID);
                //Insert copy newlt->anct.
                InConsistPair pair;
                pair.from_vex_id = from->id();
                pair.to_vex_id = to->id();
                pair.from_lt = newlt;
                pair.to_lt = anct;
                inconsist_lst.append_tail(pair);
                continue;
            }
        }
    }
}


IRBB * LTConsistencyMgr::insertLatch(IRBB const* from, MOD IRBB * to)
{
    IRBB * newbb = m_rg->allocBB();
    BBListIter fromit;
    BBListIter toit;
    m_bb_list->find(from, &fromit);
    m_bb_list->find(to, &toit);
    ASSERT0(fromit && toit);
 
    //Insert newbb that must be fallthrough BB prior to occbb.
    IRBB * tramp = m_cfg->insertBBBetween(from, fromit, to, toit,
                                          newbb, m_oc);
    m_impl.tryUpdateRPO(newbb, tramp, to);
    m_impl.tryUpdateDom(newbb, to);
    m_impl.tryUpdateLiveness(newbb, to);
    dumpInsertBB(m_impl, from, to, newbb, "fix lifetime consistency");
    m_is_insert_bb = true;
    return newbb;
}


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


void LTConsistencyMgr::reviseEdgeConsistency(
    InConsistPairList const& inconsist_lst)
{
    TMap<VexPair, IRBB*, CompareVexPair> inserted_latch;
    InConsistPairListIter it;
    UINT i = 0;
    for (InConsistPair pair = inconsist_lst.get_head(&it);
         i < inconsist_lst.get_elem_count();
         pair = inconsist_lst.get_next(&it), i++) {
        VexPair vp;
        vp.fromid = pair.from_vex_id;
        vp.toid = pair.to_vex_id;
        IRBB * latch = inserted_latch.get(vp);
        if (latch == nullptr) {
            IRBB * frombb = m_cfg->getBB(pair.from_vex_id);
            IRBB * tobb = m_cfg->getBB(pair.to_vex_id);
            latch = insertLatch(frombb, tobb);
            inserted_latch.set(vp, latch);
        }
        Type const* fromty = pair.from_lt->getFirstOccType();
        Type const* toty = pair.to_lt->getFirstOccType();
        ASSERT0(fromty && toty);
        IR * mv = m_impl.insertMove(pair.from_lt->getPrno(),
            pair.to_lt->getPrno(), fromty, toty, latch);
        dumpInsertMove(m_impl, latch, mv,
                       "fix lifetime consistency $%u->$%u",
                       pair.from_lt->getPrno(), pair.to_lt->getPrno());
    }
}


void LTConsistencyMgr::perform()
{
    computeLTConsistency();
    InConsistPairList inconsist_lst;
    computeEdgeConsistency(inconsist_lst);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLSRA()) {
        inconsist_lst.dump(m_rg);
    }
    reviseEdgeConsistency(inconsist_lst);
}
//END LTConsistencyMgr


//
//START SplitMgr
//
SplitMgr::SplitMgr(LSRAImpl & impl) : m_impl(impl), m_ra(impl.getRA())
{
    m_rg = m_impl.getRegion();
    m_irmgr = m_rg->getIRMgr();
    m_cfg = m_ra.getCFG();
    m_oc = m_impl.getOptCtx();
    m_live_mgr = impl.getLiveMgr();
}


LifeTime * SplitMgr::selectLTByFurthestNextOcc(LTSet const& lst, Pos pos,
                                               Vector<SplitCtx> const& ctxvec,
                                               OUT Occ & reload_occ)
{
    VecIdx cnt = 0;
    ASSERT0(ctxvec.get_elem_count() == lst.get_elem_count());
    Occ furthest_occ;
    LifeTime * furthest_lt = nullptr;
    LTSetIter it;
    for (LifeTime * t = lst.get_head(&it);
         t != nullptr; t = lst.get_next(&it), cnt++) {
        SplitCtx l = ctxvec.get(cnt);
        if (furthest_lt == nullptr) {
            furthest_occ = l.reload_occ;
            furthest_lt = t;
            continue;
        }
        if (furthest_occ.pos() > l.reload_pos) {
            continue;
        }
        furthest_occ = l.reload_occ;
        furthest_lt = t;
    }
    if (furthest_lt == nullptr) { return nullptr; }
    reload_occ = furthest_occ;
    dumpSelectSplitCand(m_impl, furthest_lt, pos, true,
                        "$%u has furthest reload-occ", furthest_lt->getPrno());
    return furthest_lt;
}


LifeTime * SplitMgr::selectLTByFurthestNextRange(LTSet const& lst, Pos pos,
                                                 OUT Occ & reload_occ)
{
    LTSetIter it;
    Range furthest_range(POS_UNDEF);
    LifeTime * cand = nullptr;
    VecIdx cnt = 0;
    VecIdx cand_cnt = VEC_UNDEF;
    for (LifeTime * t = lst.get_head(&it);
         t != nullptr; t = lst.get_next(&it), cnt++) {
        VecIdx ridx, less, great;
        Range r1(POS_UNDEF);
        bool find = t->findRange(pos, r1, ridx, &less, &great);
        if (find) {
            ASSERT0(ridx != VEC_UNDEF);
            VecIdx nextrange = ridx + 1;
            if (nextrange > t->getLastRangeIdx()) {
                //No remaining range.
                continue;
            }
            Range r3 = t->getRange(nextrange);
            if (furthest_range.start() == POS_UNDEF ||
                r3.start() > furthest_range.start()) {
                furthest_range = r3;
                cand = t;
                cand_cnt = cnt;
            }
            continue;
        }
        if (great == VEC_UNDEF) {
            //There is not range after given 'pos'.
            continue;
        }
        Range r2 = t->getRange(great);
        ASSERT0(r2.start() != POS_UNDEF && r2.end() != POS_UNDEF);
        if (furthest_range.start() == POS_UNDEF ||
            r2.start() > furthest_range.start()) {
            furthest_range = r2;
            cand = t;
            cand_cnt = cnt;
        }
    }
    if (cand == nullptr) { return nullptr; }
    OccListIter it2;
    bool succ = cand->findOcc(furthest_range.start(), it2);
    ASSERT0(succ);
    reload_occ = it2->val();
    dumpSelectSplitCand(m_impl, cand, pos, true,
                        "$%u has furthest next range", cand->getPrno());
    return cand;
}


void SplitMgr::selectSplitCandFromSet(LTSet const& set, SplitCtx const& ctx,
                                      OUT LTSet & candlst,
                                      OUT Vector<SplitCtx> & candctxvec)
{
    LTSetIter it;
    LTSetIter nit;
    for (set.get_head(&it), nit = it; it != nullptr; it = nit) {
        set.get_next(&nit);
        LifeTime * t = it->val();
        ASSERTN(m_ra.hasReg(t), ("it should not be in InActiveSet"));
        SplitCtx lctx(ctx);
        bool canbe = checkIfCanBeSplitCand(t, ctx.split_pos, lctx.reload_pos,
                                           lctx.reload_occ);
        if (canbe) {
            candlst.append_tail(t);
            candctxvec.append(lctx);
        }
    }
}


LifeTime * SplitMgr::selectSplitCandByDensity(LTSet & set, LifeTime * lt,
                                              bool tryself, OUT SplitCtx & ctx)
{
    //TODO:select split-candidate by choosing the least occurrence-density.
    //lifetime's occurrence-density = the number of occ /
    //                                the length of lifetime.
    return nullptr;
}


LifeTime * SplitMgr::selectSplitCandImpl(LTSet & set, LifeTime * lt,
                                         bool tryself, OUT SplitCtx & ctx)
{
    LTSet candlst;
    Vector<SplitCtx> candctxvec;
    selectSplitCandFromSet(set, ctx, candlst, candctxvec);
    if (tryself) {
        OccListIter it;
        bool succ = lt->findOccAfter(ctx.split_pos, it);
        SplitCtx lctx(ctx);
        if (succ) {
            lctx.reload_pos = it->val().pos();
            lctx.reload_occ = it->val();
        } else {
            //There is no reload-occ of lt.
            lctx.reload_pos = POS_UNDEF;
        }
        candlst.append_tail(lt);
        candctxvec.append(lctx);
    }
    if (candlst.get_elem_count() == 0) {
        return nullptr;
    }
    //Attempt to select a lifetime with the biggest hole to contain the entire
    //given 'lt'.
    LifeTime * cand = selectLTByFurthestNextOcc(candlst, ctx.split_pos,
                                                candctxvec, ctx.reload_occ);
    //LifeTime * cand = selectLTByFurthestNextRange(candlst, ctx.split_pos,
    //                                              ctx.reload_occ);
    ASSERT0(cand);
    ctx.reload_pos = ctx.reload_occ.pos();
    //candidate may not have reload_pos.
    //ASSERT0(ctx.reload_pos != POS_UNDEF);
    set.remove(cand);
    return cand;
}


LifeTime * SplitMgr::selectSplitCandFromInActive(LifeTime * lt, bool tryself,
                                                 OUT SplitCtx & ctx)
{
    return selectSplitCandImpl(m_ra.getInActive(), lt, tryself, ctx);
}


LifeTime * SplitMgr::selectSplitCandFromActive(LifeTime * lt, bool tryself,
                                               OUT SplitCtx & ctx)

{
    LifeTime * cand = selectSplitCandByDensity(m_ra.getActive(), lt,
                                               tryself, ctx);
    if (cand != nullptr) { return cand; }
    return selectSplitCandImpl(m_ra.getActive(), lt, tryself, ctx);
}


LifeTime * SplitMgr::selectSplitCand(LifeTime * lt, bool tryself,
                                     OUT SplitCtx & ctx)
{
    //lt is active lifetime, thus do not mix it up with inactive lifetimes.
    bool mixup_with_inactive = false;
    LifeTime * t = selectSplitCandFromInActive(lt, mixup_with_inactive, ctx);
    if (t != nullptr) {
        return t;
    }
    return selectSplitCandFromActive(lt, tryself, ctx);
}


bool SplitMgr::checkIfCanBeSplitCand(LifeTime const* lt, Pos split_pos,
                                     OUT Pos & reload_pos, OUT Occ & reload_occ)
{
    OccList & occlst = const_cast<LifeTime*>(lt)->getOccList();
    for (OccListIter it = occlst.get_head();
         it != occlst.end(); it = occlst.get_next(it)) {
        Occ occ = it->val();
        if (occ.pos() == split_pos) {
            //lt can not be splitted at given position because lt also
            //has an occurrence at the position right there.
            dumpSelectSplitCand(m_impl, lt, split_pos, false,
                                "lt interferred at pos:%u", split_pos);
            return false;
        }
        if (occ.pos() > split_pos) {
            ASSERT0(occ.getIR());
            reload_occ = occ;
            reload_pos = occ.pos();
            dumpSelectSplitCand(m_impl, lt, split_pos, true, nullptr);
            return true;
        }
    }
    return false;
}


//If there is no real-occurrence at split_pos, the lifetime 'lt'
//should shrink to the nearest USE, namely either the RHS of spill or RHS at
//split_pos.
void SplitMgr::shrinkLTToSplitPos(LifeTime * lt, Pos split_pos,
                                  IR const* split_pos_ir)
{
    //When spill operation inserted before split_pos, lt will be terminated
    //at the previous USE position, even if IR at split_pos may not be the
    //occurrence of lt.
    if (UpdatePos::isDef(split_pos)) {
        //CASE:[10] x <- y [9]
        //  If 'split_pos' is 10, spill operation will be inserted before the
        //  stmt at split_pos.
        //  lt's lifetime will be terminated at RHS of previous stmt.
        UpdatePos::decToLastUse(split_pos);
    }
    if (lt->isUseOcc(split_pos_ir)) {
        //CASE:[10] x <- $1 [9]
        //  If 'split_pos' is 9, spill operation will be inserted before the
        //  stmt at split_pos.
        //  lt's lifetime will be terminated at split_pos.
        UpdatePos::incToNextDef(split_pos);
    }
    lt->cleanRangeFrom(split_pos);
}


void SplitMgr::cutoffLTFromSpillPos(LifeTime * lt, Pos split_pos)
{
    //When spill operation inserted after 'split_pos', lt will be terminated
    //at split_pos.
    //e.g:lt termiated at position 10.
    //    [10] $1 <- 1
    //         ... <- $1 //spill operation of $1
    Pos nextpos = split_pos;
    UpdatePos::inc(nextpos);
    lt->cleanRangeFrom(nextpos);
}


bool SplitMgr::isDefLT(IR const* stmt, LifeTime const* lt) const
{
    ASSERT0(stmt->is_stmt());
    IR const* pr = const_cast<IR*>(stmt)->getResultPR();
    return pr != nullptr && pr->getPrno() == lt->getPrno() ? true : false;
}


IR * SplitMgr::insertSpillAroundSplitPos(LifeTime * lt, SplitCtx const& ctx)
{
    ASSERT0(lt->is_contain(ctx.split_pos));
    IR * spill = nullptr;
    ASSERT0(ctx.split_pos_ir);
    if (UpdatePos::isDef(ctx.split_pos) && lt->isDefOcc(ctx.split_pos_ir)) {
        //There is no need to insert spill code in some cases:
        //1. split_pos is in a hole of lt.
        //Even if split_pos is DEF position, spill code is also needed, e.g:
        //spill caller-saved register at call-stmt, the position indicates
        //the call-stmt which is DEF position.
        cutoffLTFromSpillPos(lt, ctx.split_pos);
        spill = m_impl.insertSpillAfter(lt->getPrno(),
                                        ctx.split_pos_ir->getType(),
                                        ctx.split_pos_ir);
        ASSERT0(spill);
        dumpSpill(m_impl, spill, lt, ctx.split_pos_ir, false);
    } else {
        shrinkLTToSplitPos(lt, ctx.split_pos, ctx.split_pos_ir);
        spill = m_impl.insertSpillBefore(lt->getPrno(),
                                         ctx.split_pos_ir->getType(),
                                         ctx.split_pos_ir);
        ASSERT0(spill);
        dumpSpill(m_impl, spill, lt, ctx.split_pos_ir, true);
    }
    return spill;
}


void SplitMgr::insertSpillDuringSplitting(LifeTime * lt, SplitCtx const& ctx,
                                          bool canberemat,
                                          RematCtx const& rematctx,
                                          OUT IR *& spill)
{
    if (canberemat) { return; }
    if (!lt->is_contain(ctx.split_pos)) { return; }
    spill = insertSpillAroundSplitPos(lt, ctx);
}


void SplitMgr::insertReloadDuringSplitting(LifeTime * lt, LifeTime * newlt,
                                           SplitCtx const& ctx,
                                           bool canberemat,
                                           RematCtx const& rematctx,
                                           IR * spill)
{
    if (!UpdatePos::isUse(ctx.reload_pos)) { return; }
    if (canberemat) {
        m_impl.insertRematBefore(newlt->getPrno(), rematctx,
                                 ctx.reload_occ.getIR()->getType(),
                                 ctx.reload_occ.getIR());
        return;
    }
    //There is no need to insert reload newlt's register in some cases:
    //1. reload_pos is already a DEF operation.
    ASSERT0(ctx.reload_occ.getIR());
    ASSERTN(spill, ("illegal splitting strategy"));
    Var * spill_loc = m_impl.findSpillLoc(spill);
    ASSERT0(spill_loc);
    IR * reload = m_impl.insertReloadBefore(newlt->getPrno(), spill_loc,
                                            ctx.reload_occ.getIR()->getType(),
                                            ctx.reload_occ.getIR());
    dumpReload(m_impl, reload, lt, newlt, ctx.reload_occ.getIR());
}


LifeTime * SplitMgr::splitAt(LifeTime * lt, SplitCtx const& ctx)
{
    LifeTime * newlt = splitIntoTwoLT(lt, ctx);
    RematCtx rematctx;
    bool canberemat = m_ra.checkLTCanBeRematerialized(lt, rematctx);
    IR * spill = nullptr;
    insertSpillDuringSplitting(lt, ctx, canberemat, rematctx, spill);
    insertReloadDuringSplitting(lt, newlt, ctx, canberemat, rematctx, spill);
    m_impl.recordSplittedNewLT(newlt);
    return newlt;
}


LifeTime * SplitMgr::splitIntoTwoLT(LifeTime * lt, SplitCtx const& ctx)
{
    ASSERT0(ctx.reload_occ.getIR());
    PRNO newprno = m_irmgr->buildPrno(ctx.reload_occ.getIR()->getType());
    //Note newlt will start from the reload_pos, therefore there is a free hole
    //between split_pos and reload_pos of lt.
    LifeTime * newlt = m_impl.getLTMgr().genLifeTime(newprno);
    newlt->setAncestor(lt);
    newlt->moveFrom(lt, ctx.reload_pos, m_impl.getLTMgr());
    newlt->set_dedicated(lt->is_dedicated());
    m_ra.setReg(newlt->getPrno(), REG_UNDEF);
    if (newlt->is_dedicated()) {
        Reg antireg = m_ra.getDedicatedReg(lt->getPrno());
        ASSERT0(antireg != REG_UNDEF);
        //Both lt and newlt expect the same dedicated register.
        m_ra.setDedicatedReg(newlt->getPrno(), antireg);
    }
    m_impl.getLTMgr().renameLifeTimeOcc(newlt, newprno);
    dumpSplitTwo(m_impl, lt, newlt, ctx.split_pos, ctx.reload_pos);
    return newlt;
}
//END SplitMgr


//
//START LSRAImpl
//
void LSRAImpl::initRegSet()
{
    #ifdef DEBUG_LSRA
    //User can customize the register set that used in register allocation.
    //e.g: given calling convention has 3 regisetr allocable, one is callee, 
    // one is caller, and the other is link-register.
    //m_avail_callee.bunion(5);
    //m_avail_caller.bunion(6);
    //m_avail_caller.bunion(getLink());
    //m_avail_allocable.bunion(m_avail_callee);
    //m_avail_allocable.bunion(m_avail_caller);
    #else
    m_avail_callee.copy(getTIMgr().getCallee());
    m_avail_caller.copy(getTIMgr().getCaller());
    m_avail_param.copy(getTIMgr().getParam());
    m_avail_return_value.copy(getTIMgr().getReturnValue());
    m_avail_allocable.copy(getTIMgr().getAllocable());
    #endif
}


void LSRAImpl::pickRegFromAllocable(Reg reg)
{
    ASSERT0(isAvailAllocable(reg));
    pickReg(m_avail_callee, reg);
    pickReg(m_avail_caller, reg);
}


void LSRAImpl::dumpBBList() const
{
    xoc::dumpBBList(m_bb_list, m_rg);
}


void LSRAImpl::forceAssignRegister(LifeTime const* lt, Reg reg)
{
    pickRegFromAllocable(reg);
    if (getTIMgr().isCallee(reg)) {
        recordUsedCallee(reg);
    }
    m_ra.setReg(lt->getPrno(), reg);
    dumpAssign(*this, lt, "assign dedicated register");
}


bool LSRAImpl::tryAssignCallee(LifeTime const* lt)
{
    Reg r = pickReg(m_avail_callee);
    if (r != REG_UNDEF) {
        ASSERT0(isAvailAllocable(r));
        m_ra.setReg(lt->getPrno(), r);
        recordUsedCallee(r);
        dumpAssign(*this, lt, nullptr);
        return true;
    }
    return false;
}


bool LSRAImpl::tryAssignCaller(LifeTime const* lt)
{
    Reg r = pickReg(m_avail_caller);
    if (r != REG_UNDEF) {
        m_ra.setReg(lt->getPrno(), r);
        dumpAssign(*this, lt, nullptr);
        return true;
    }
    return false;
}


bool LSRAImpl::tryAssignRegister(LifeTime const* lt)
{
    ASSERT0(!lt->is_dedicated());
    if (m_ra.preferCallee(lt) && tryAssignCallee(lt)) {
        return true;
    }
    if (tryAssignCaller(lt)) {
        return true;
    }
    getActMgr().dump("ASSIGN:can NOT find register for $%u",
                     lt->getPrno());
    return false;
}


LifeTime * LSRAImpl::selectAssignDefCand(Pos curpos, IR const* curstmt)
{
    IR const* res = const_cast<IR*>(curstmt)->getResultPR();
    if (res == nullptr) { return nullptr; }
    return pickFromSet(res->getPrno(), m_ra.getUnhandled());
}


LifeTime * LSRAImpl::selectAssignUseCand(Pos curpos, IR const* curstmt,
                                         OUT IR const** curir)
{
    IR const* cand = nullptr;
    ConstIRIter it;
    for (IR const* e = xoc::iterExpInitC(curstmt, it);
         e != nullptr; e = xoc::iterExpNextC(it)) {
        if (!e->isPROp()) { continue; }
        if (m_ra.hasReg(e->getPrno())) { continue; }
        cand = e;
        break;
    }
    if (cand == nullptr) { return nullptr; }
    LifeTime * candlt = pickFromSet(cand->getPrno(), m_ra.getUnhandled());
    ASSERT0(candlt);
    *curir = cand;
    return candlt;
}


void LSRAImpl::computeUsedCaller(OUT RegSet & used)
{
    xcom::bs_diff(getTIMgr().getCaller(), m_avail_caller, used);
}


//Spill LT that assigned referred register in given LTSet.
void LSRAImpl::splitAllLTWithReg(Pos curpos, IR const* ir, Reg r,
                                 MOD LTSet & set)
{
    ASSERT0(r != REG_UNDEF);
    LTSetIter it;
    LTSetIter nit;
    for (set.get_head(&it), nit = it; it != nullptr; it = nit) {
        set.get_next(&nit);
        LifeTime * t = it->val();
        if (!m_ra.isAlias(getReg(t), r)) { continue; }
        SplitMgr spltmgr(*this);
        SplitCtx ctx(curpos);
        ctx.split_pos = curpos;
        ctx.split_pos_ir = ir;
        ctx.split_lt = t;
        bool canbe = spltmgr.checkIfCanBeSplitCand(t, ctx.split_pos,
                                                   ctx.reload_pos,
                                                   ctx.reload_occ);
        ASSERT0(canbe);
        dumpSelectSplitCand(*this, t, curpos, true,
                            "split $%u that assigned %s",
                            t->getPrno(), m_ra.getRegName(r));
        LifeTime * newlt = spltmgr.splitAt(t, ctx);
        m_ra.addUnhandled(newlt);
        set.remove(it);
        m_ra.getHandled().append_tail(t);
        freeReg(t);
    }
}


void LSRAImpl::splitLinkLT(Pos curpos, IR const* ir)
{
    ASSERT0(ir->isCallStmt());
    Reg l = getTIMgr().getLink();
    if (ir->isIntrinsicOp() || !isAvailAllocable(l)) { return; }
    splitActiveLTWithReg(curpos, ir, l);
}


void LSRAImpl::splitInActiveLTWithReg(Pos curpos, IR const* ir, Reg r)
{
    splitAllLTWithReg(curpos, ir, r, m_ra.getInActive());
}


void LSRAImpl::splitActiveLTWithReg(Pos curpos, IR const* ir, Reg r)
{
    splitAllLTWithReg(curpos, ir, r, m_ra.getActive());
}


void LSRAImpl::splitCallerSavedLT(Pos curpos, IR const* ir)
{
    ASSERT0(ir->isCallStmt());
    if (ir->isIntrinsicOp()) { return; }
    RegSet used;
    computeUsedCaller(used);
    for (BSIdx i = used.get_first(); i != BS_UNDEF; i = used.get_next(i)) {
        ASSERT0(i != REG_UNDEF);
        splitAllLTWithReg(curpos, ir, (Reg)i, m_ra.getActive());
    }
}


void LSRAImpl::saveCallee()
{
    for (BSIdx i = m_used_callee.get_first();
         i != BS_UNDEF; i = m_used_callee.get_next(i)) {
        ASSERT0(getTIMgr().isCallee(i));
        IR * spill = insertSpillAtEntry((Reg)i);
        insertReloadAtExit(i, findSpillLoc(spill));
    }
}


//Dedicated register must be satefied in the highest priority.
void LSRAImpl::assignDedicatedLT(Pos curpos, IR const* ir, LifeTime * lt)
{
    ASSERT0(lt->is_dedicated());
    Reg antireg = m_ra.getDedicatedReg(lt);
    ASSERT0(antireg != REG_UNDEF);
    splitActiveLTWithReg(curpos, ir, antireg);
    splitInActiveLTWithReg(curpos, ir, antireg);
    forceAssignRegister(lt, antireg);
    ASSERT0(getReg(lt) == antireg);
    m_ra.addActive(lt);
    ASSERT0(m_ra.verify4List());
}


//Try assign register for given ir which at 'pos'.
//ir: may be expression or stmt.
//lt: lifetime that corresponding to 'ir'.
void LSRAImpl::tryAssignRegForIR(Pos pos, IR const* ir, LifeTime * lt)
{
    ASSERT0(ir && (ir->is_stmt() || ir->is_exp()));
    if (lt->is_dedicated()) {
        assignDedicatedLT(pos, ir, lt);
        return;
    }
    //Normal lifetime.
    if (tryAssignRegister(lt)) {
        Reg r = getReg(lt);
        ASSERT0(r != REG_UNDEF);
        splitInActiveLTWithReg(pos, ir, r);
        m_ra.addActive(lt);
        ASSERT0(m_ra.verify4List());
        return;
    }
    ASSERT0(ir);
    solveConflict(lt, pos, ir);
}


void LSRAImpl::transferInActive(Pos curpos)
{
    LTSetIter it;
    LTSetIter nit;
    LTSet & act = m_ra.getActive();
    LTSet & handled = m_ra.getHandled();
    LTSet & inact = m_ra.getInActive();
    for (inact.get_head(&it), nit = it; it != nullptr; it = nit) {
        inact.get_next(&nit);
        LifeTime * lt = it->val();
        if (!lt->is_cover(curpos)) {
            //lt even not conver 'curpos', it has been handled.
            //Transfer lt to handled and free targ-machine resource.
            inact.remove(it);
            handled.append_tail(lt);
            freeReg(lt);
            continue;
        }
        if (lt->is_contain(curpos)) {
            //lt is not only conver 'curpos' but also in a range.
            //Transfer lt to active.
            inact.remove(it);
            act.append_tail(lt);
        }
    }
}


void LSRAImpl::transferActive(Pos curpos)
{
    LTSetIter it;
    LTSetIter nit;
    LTSet & act = m_ra.getActive();
    LTSet & handled = m_ra.getHandled();
    LTSet & inact = m_ra.getInActive();
    for (act.get_head(&it), nit = it; it != nullptr; it = nit) {
        act.get_next(&nit);
        LifeTime * lt = it->val();
        if (!lt->is_cover(curpos)) {
            //lt even not conver 'curpos', it has been handled.
            //Transfer lt to handled and free targ-machine resource.
            act.remove(it);
            handled.append_tail(lt);
            freeReg(lt);
            return;
        }
        if (!lt->is_contain(curpos)) {
            //lt convers 'curpos' but in a hole.
            //Transfer lt to inactive.
            act.remove(it);
            inact.append_tail(lt);
        }
    }
}


//The function check each CFG edge to fixup the lifetime conflict while the
//linearization allocation flattening the CFG.
void LSRAImpl::reviseLTConsistency()
{
    LTConsistencyMgr mgr(*this);
    mgr.perform();
}


bool LSRAImpl::isSpillLikeOp(IR const* ir)
{
    if (!ir->is_st()) { return false; }
    if (!ir->getRHS()->is_pr()) { return false; }
    if (!ir->getIdinfo()->is_local()) { return false; }
    return true;
}


bool LSRAImpl::isRematLikeOp(IR const* ir) const
{
    return m_ra.isRematLikeOp(ir);
}


bool LSRAImpl::isReloadLikeOp(IR const* ir)
{
    if (!ir->is_stpr()) { return false; }
    if (!ir->getRHS()->is_ld()) { return false; }
    if (!ir->getRHS()->getIdinfo()->is_local()) { return false; }
    return true;
}


Var * LSRAImpl::findSpillLoc(IR const* ir)
{
    if (isSpillLikeOp(ir)) {
        return ir->getIdinfo();
    }
    ASSERT0(isReloadLikeOp(ir));
    return ir->getRHS()->getIdinfo();
}


IR * LSRAImpl::insertSpillAtEntry(Reg r)
{
    ASSERT0(r != REG_UNDEF);
    IRBB * bb = m_cfg->getEntry();
    Type const* ty = m_tm->getTargMachRegisterType();
    PRNO prno = m_irmgr->buildPrno(ty);
    m_ra.setReg(prno, r);
    IR * spill = insertSpillAtBBEnd(prno, ty, bb);
    dumpSpill(*this, spill, r, bb, "spill callee-saved at entry");
    return spill;
}


void LSRAImpl::insertReloadAtExit(Reg r, Var * spill_loc)
{
    ASSERT0(r != REG_UNDEF && spill_loc);
    List<IRBB*>::Iter it;
    Type const* ty = m_tm->getTargMachRegisterType();
    PRNO prno = m_irmgr->buildPrno(ty);
    m_ra.setReg(prno, r);
    for (IRBB * bb = m_cfg->getExitList()->get_head(&it);
         bb != nullptr; bb = m_cfg->getExitList()->get_next(&it)) {
        IR * reload = insertReloadAtBB(prno, spill_loc, ty, bb, false);
        dumpReload(*this, reload, r, bb, "reload callee-saved at exit");
    }
}


void LSRAImpl::insertSpillAtHead(IR * spill, MOD IRBB * bb)
{
    ASSERT0(!bb->hasPhi(m_cfg));
    bb->getIRList().append_head(spill);
}


void LSRAImpl::insertSpillAfter(IR * spill, IR const* marker)
{
    ASSERT0(isSpillLikeOp(spill) && marker);
    m_ra.setSpill(spill);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERT0(stmt->is_stmt());
    if (stmt->isCallStmt()) {
        IRBB * followed_bb = m_cfg->insertFallThroughBBAfter(stmt->getBB(),
                                                             m_oc);
        ASSERT0(followed_bb);
        insertSpillAtHead(spill, followed_bb);
        return;
    }
    ASSERTN(!IRBB::isLowerBoundary(stmt), ("need insert new BB"));
    stmt->getBB()->getIRList().insert_after(spill, stmt);
}


void LSRAImpl::insertRematBefore(PRNO newres, RematCtx const& rematctx,
                                 Type const* loadvalty, IR const* marker)
{
    IR * remat = m_ra.buildRemat(newres, rematctx, loadvalty);
    insertRematBefore(remat, marker);
}


void LSRAImpl::insertRematBefore(IR * remat, IR const* marker)
{
    ASSERT0(isRematLikeOp(remat) && marker);
    m_ra.setRemat(remat);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERT0(stmt->is_stmt());
    ASSERT0(!stmt->is_phi());
    stmt->getBB()->getIRList().insert_before(remat, stmt);
}


void LSRAImpl::insertReloadBefore(IR * reload, IR const* marker)
{
    ASSERT0(isReloadLikeOp(reload) && marker);
    m_ra.setReload(reload);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERT0(stmt->is_stmt());
    ASSERT0(!stmt->is_phi());
    stmt->getBB()->getIRList().insert_before(reload, stmt);
}


IR * LSRAImpl::insertMove(PRNO from, PRNO to, Type const* fromty,
                          Type const* toty, IRBB * bb)
{
    IR * mv = m_ra.buildMove(from, to, fromty, toty);
    m_ra.setMove(mv);
    bb->getIRList().append_tail_ex(mv);
    return mv;
}


void LSRAImpl::recordSplittedNewLT(LifeTime const* newlt)
{
    m_splitted_newlt_lst.append_tail(newlt);
}


void LSRAImpl::tryUpdateDom(IRBB const* newbb, IRBB const* marker)
{
    //TODO:update DomInfo incrementally.
    m_oc->setInvalidDom();
    m_oc->setInvalidPDom();
    m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_DOM, PASS_UNDEF);
}


void LSRAImpl::tryUpdateLiveness(IRBB const* newbb, IRBB const* marker)
{
    //TODO:update liveness incrementally.
    m_live_mgr->set_valid(false);
    m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_LIVENESS_MGR,
                                               PASS_UNDEF);
}


void LSRAImpl::tryUpdateRPO(OUT IRBB * newbb, OUT IRBB * tramp,
                            IRBB const* marker)
{
    if (newbb->rpo() == RPO_UNDEF &&
        !m_cfg->tryUpdateRPO(newbb, marker, true)) {
        //TODO: Try update RPO incrementally to avoid recompute
        //whole RPO-Vex list in CFG.
        //m_cfg->tryUpdateRPO(prehead, guard_start, true);
        //Just leave RPO-recomputation to next user for now.
        m_oc->setInvalidRPO();
    }
    if (tramp != nullptr && tramp->rpo() == RPO_UNDEF &&
        !m_cfg->tryUpdateRPO(tramp, marker, true)) {
        //TODO: Try update RPO incrementally to avoid recompute
        //whole RPO-Vex list in CFG.
        //m_cfg->tryUpdateRPO(prehead, guard_start, true);
        //Just leave RPO-recomputation to next user for now.
        m_oc->setInvalidRPO();
    }
}


IR * LSRAImpl::insertSpillAfter(PRNO prno, Type const* ty, IR const* marker)
{
    IR * spill = m_ra.buildSpill(prno, ty);
    insertSpillAfter(spill, marker);
    return spill;
}


IR * LSRAImpl::insertSpillBefore(Reg r, IR const* marker)
{
    Type const* ty = m_tm->getTargMachRegisterType();
    PRNO prno = m_irmgr->buildPrno(ty);
    m_ra.setReg(prno, r);
    return insertSpillBefore(prno, ty, marker);
}


IR * LSRAImpl::insertSpillBefore(PRNO prno, Type const* ty, IR const* marker)
{
    IR * spill = m_ra.buildSpill(prno, ty);
    insertSpillBefore(spill, marker);
    return spill;
}


IR * LSRAImpl::insertReloadAtBB(PRNO prno, Var * spill_loc,
                                Type const* ty, IRBB * bb, bool start)
{
    IR * reload = m_ra.buildReload(prno, spill_loc, ty);
    insertReloadAtBB(reload, bb, start);
    return reload;
}


IR * LSRAImpl::insertSpillAtBBEnd(PRNO prno, Type const* ty, IRBB * bb)
{
    IR * spill = m_ra.buildSpill(prno, ty);
    insertSpillAtBBEnd(spill, bb);
    return spill;
}


IRListIter LSRAImpl::insertSpillAtBBEnd(IR * spill, IRBB * bb)
{
    ASSERT0(isSpillLikeOp(spill) && bb);
    m_ra.setSpill(spill);
    return bb->getIRList().append_tail_ex(spill);
}


IRListIter LSRAImpl::insertReloadAtBB(IR * reload, IRBB * bb, bool start)
{
    ASSERT0(isReloadLikeOp(reload) && bb);
    m_ra.setReload(reload);
    if (start) {
        return bb->getIRList().append_head_ex(reload);
    }
    return bb->getIRList().append_tail_ex(reload);
}


void LSRAImpl::insertSpillBefore(IR * spill, IR const* marker)
{
    ASSERT0(isSpillLikeOp(spill) && marker);
    m_ra.setSpill(spill);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERT0(!stmt->is_phi());
    BBIRList & irlst = stmt->getBB()->getIRList();
    BBIRListIter it = nullptr;
    irlst.find(const_cast<IR*>(stmt), &it);
    ASSERT0(it);
    for (;it->get_prev() != nullptr; it = irlst.get_prev(it)) {
        if (!m_ra.isReloadOp(it->get_prev()->val())) {
            break;
        }
    }
    irlst.insert_before(spill, it);
}


IR * LSRAImpl::insertReloadBefore(PRNO newres, Var * spill_loc,
                                  Type const* ty, IR const* marker)
{
    IR * reload = m_ra.buildReload(newres, spill_loc, ty);
    insertReloadBefore(reload, marker);
    return reload;
}


void LSRAImpl::freeReg(Reg reg)
{
    if (getTIMgr().getCallee().is_contain(reg)) {
        m_avail_callee.bunion((BSIdx)reg);
        return;
    }
    if (getTIMgr().getCaller().is_contain(reg)) {
        m_avail_caller.bunion((BSIdx)reg);
        return;
    }
    UNREACHABLE();
}


void LSRAImpl::freeReg(LifeTime const* lt)
{
    Reg reg = getReg(lt->getPrno());
    ASSERT0(reg != REG_UNDEF);
    freeReg(reg);
}


void LSRAImpl::solveConflict(LifeTime * lt, Pos curpos, IR const* curir)
{
    ASSERT0(m_ra.getLTMgr().verifyPos(curir, curpos));
    bool succ = false;
    bool tryself = true;
    UINT count = 0;
    SplitMgr spltmgr(*this);
    do {
        SplitCtx ctx(curpos, curir, lt);
        LifeTime * cand = spltmgr.selectSplitCand(lt, tryself, ctx);
        ASSERTN(cand, ("no enough resource to remedy splitting"));
        if (cand == lt) {
            //Only try itself once.
            tryself = false;
        }

        //If the candidate does not have a reload_pos, splitAt() can NOT
        //cutoff lifetime into two.
        ASSERT0(ctx.reload_pos != POS_UNDEF);

        LifeTime * newlt = spltmgr.splitAt(cand, ctx);
        m_ra.addUnhandled(newlt);
        //CASE:lsra_split.gr, cand may not have reload-occ.
        //ASSERT0(ctx.reload_pos != POS_UNDEF);
        if (m_ra.hasReg(cand)) {
            m_ra.addHandled(cand);
            freeReg(cand);
        } else {
            //CASE:lt may be assigning-candidate, whereas lt is selected as the
            //splitting-candidate meanwhile. Thus after the function return, lt
            //will be assigned a register and newlt will be waiting for assigning.
            //ASSERT0(m_ra.getUnhandled().find(lt));
        }
        succ = tryAssignRegister(lt);
        count++;
    } while (!succ && count < 20);
    ASSERT0(succ);
    dumpAssign(*this, lt, nullptr);
    m_ra.addActive(lt);
    splitInActiveLTWithReg(curpos, curir, getReg(lt));
    ASSERT0(m_ra.verify4List());
}


void LSRAImpl::dumpAvailRegSet() const
{
    note(m_rg, "\n==-- DUMP %s --==", "AvaiableRegisterSet");
    StrBuf buf(32);
    m_avail_caller.dump(buf);
    note(m_rg, "\nAVAIL_CALLER:%s", buf.buf);

    buf.clean();
    m_avail_callee.dump(buf);
    note(m_rg, "\nAVAIL_CALLEE:%s", buf.buf);

    buf.clean();
    m_avail_param.dump(buf);
    note(m_rg, "\nAVAIL_PARAM:%s", buf.buf);

    buf.clean();
    m_avail_return_value.dump(buf);
    note(m_rg, "\nAVAIL_RETURN_VALUE:%s", buf.buf);

    buf.clean();
    m_avail_allocable.dump(buf);
    note(m_rg, "\nAVAIL_ALLOCABLE:%s", buf.buf);
}


void LSRAImpl::dump() const
{
    dumpAvailRegSet();
}


bool LSRAImpl::perform(OptCtx & oc)
{
    m_oc = &oc;
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_LIVENESS_MGR,
                                               PASS_UNDEF);
    m_live_mgr = (LivenessMgr*)m_rg->getPassMgr()->queryPass(PASS_LIVENESS_MGR);
    ASSERT0(m_live_mgr && m_live_mgr->is_valid());
    ScanInPosOrder scan(*this);
    scan.perform();
    reviseLTConsistency();
    saveCallee();
    if (m_is_insert_bb) {
        m_live_mgr->set_valid(false);
    }
    bool changed = m_is_insert_bb;
    changed |= m_ra.isInsertOp();
    return changed;
}
//END LSRAImpl

} //namespace xoc
