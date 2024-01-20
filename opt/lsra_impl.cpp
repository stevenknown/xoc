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

static void dumpInsertMove(LSRAImpl & lsra, IRBB const* mvbb, IR const* mv,
                           CHAR const* format, ...)
{
    xcom::StrBuf buf(64);
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
    xcom::StrBuf buf(64);
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
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf tmpbuf(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }
    ActHandler acth = lsra.getActMgr().dump(
        "SPILL:insert spill ir id:%u at BB%u to spill %s",
        spill->id(), bb->id(), lsra.getTIMgr().getRegName(reg));
    if (format != nullptr) {
        ASSERT0(acth.info);
        acth.info->strcat(", reason:%s", tmpbuf.getBuf());
    }
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
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf tmpbuf(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }
    ActHandler acth = lsra.getActMgr().dump(
        "RELOAD:insert reload ir id:%u at BB%u to reload %s",
        reload->id(), bb->id(), lsra.getTIMgr().getRegName(reg));
    if (format != nullptr) {
        ASSERT0(acth.info);
        acth.info->strcat(", reason:%s", tmpbuf.getBuf());
    }
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


static void dumpSelectSplitCand(LSRAImpl & lsra, LifeTime const* lt,
                                Pos split_pos,
                                bool canbe, CHAR const* format, ...)
{
    if (!lsra.getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf tmpbuf(64);
    if (canbe) {
        if (format != nullptr) {
            va_list args;
            va_start(args, format);
            tmpbuf.vstrcat(format, args);
            va_end(args);
        }
        ActHandler acth = lsra.getActMgr().dump(
            "SELECT_SPLIT_CAND:$%u is split-candidate at pos:%u",
            lt->getPrno(), split_pos);
        if (format != nullptr) {
            ASSERT0(acth.info);
            acth.info->strcat(", reason:%s", tmpbuf.getBuf());
        }
        return;
    }
    //Reason is necessary.
    ASSERT0(format);
    va_list args;
    va_start(args, format);
    tmpbuf.vstrcat(format, args);
    va_end(args);
    lsra.getActMgr().dump(
        "SELECT_SPLIT_CAND:$%u can NOT be splitted at pos:%u, reason:%s",
        lt->getPrno(), split_pos, tmpbuf.getBuf());
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


static void genInconsistPair(OUT InConsistPair & pair, UINT from, UINT to,
                             LifeTime const* anct, LifeTime const* newlt,
                             Var const* v, INCONSIST_TYPE type)
{
    ASSERT0(from != BBID_UNDEF && to != BBID_UNDEF);
    ASSERT0(type != INCONSIST_UNDEF);
    pair.from_vex_id = from;
    pair.to_vex_id = to;
    pair.from_lt = anct;
    pair.to_lt = newlt;
    pair.mem_var = v;
    pair.type = type;
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
        if (intab != nullptr) {
            rg->getLogMgr()->incIndent(ind);
            for (LifeTime const* lt = intab->get_first(it, &st);
                 lt != nullptr; lt = intab->get_next(it, &st)) {
                ASSERT0(st != CONSIST_UNDEF);
                note(rg, "\n$%u:%s", lt->getPrno(), getStName(st));
            }
            rg->getLogMgr()->decIndent(ind);
        }

        LT2ST const* outtab = getOutTab(bbid);
        note(rg, "\nLT2ST-OUT:");
        if (outtab != nullptr) {
            rg->getLogMgr()->incIndent(ind);
            for (LifeTime const* lt = outtab->get_first(it, &st);
                 lt != nullptr; lt = outtab->get_next(it, &st)) {
                ASSERT0(st != CONSIST_UNDEF);
                note(rg, "\n$%u:%s", lt->getPrno(), getStName(st));
            }
            rg->getLogMgr()->decIndent(ind);
        }
    }
    rg->getLogMgr()->decIndent(ind);
}


void LTConsistencyMgr::addConsistStatus(LifeTime const* lt, Pos start, Pos end,
                                        UINT bbid)
{
    ASSERT0(lt);
    ASSERT0(start != POS_UNDEF && end != POS_UNDEF);
    if (lt->is_cover(start)) {
        addInStatus(lt, LTConsistencyMgr::CONSIST_VALID, bbid);
    } else {
        addInStatus(lt, LTConsistencyMgr::CONSIST_INVALID, bbid);
    }

    if (lt->is_cover(end)) {
        addOutStatus(lt, LTConsistencyMgr::CONSIST_VALID, bbid);
    } else {
        addOutStatus(lt, LTConsistencyMgr::CONSIST_INVALID, bbid);
    }
}


void LTConsistencyMgr::addPRStatus(LifeTime const* anct, LifeTime const* newlt,
                                   UINT cur_bbid)
{
    ASSERT0(anct && newlt);
    ASSERT0(cur_bbid != BBID_UNDEF);
    Var const* v = m_impl.getRA().getSpillLoc(anct->getPrno());
    if (v == nullptr) {
        return addPRStatus(v, LTConsistencyMgr::PR_STATUS_UNDEF, cur_bbid);
    }
    PRSplitBB splitbb = m_impl.getPrSplitBB(v);
    UINT spill_bbid = splitbb.bb.spill_bbid;
    UINT reload_bbid = splitbb.bb.reload_bbid;

    if (reload_bbid == cur_bbid && spill_bbid == cur_bbid) {
        return addPRStatus(v, LTConsistencyMgr::PR_STATUS_SPILLED_AND_RELOAD,
                           cur_bbid);
    }

    if (spill_bbid == cur_bbid) {
        return addPRStatus(v, LTConsistencyMgr::PR_STATUS_SPILLED, cur_bbid);
    }

    if (reload_bbid == cur_bbid) {
        return addPRStatus(v, LTConsistencyMgr::PR_STATUS_RELOADED, cur_bbid);
    }

    //If the start position and the end position of cur_bbid are included in
    //the range from the end of anct lifetime to the start of new lifetime,
    //that means the PR status of cur_bbid must be PR_STATUS_IN_MEMORY, because
    //the PR has already been spilled into memory in this range.
    Pos anct_last = anct->getLastPos();
    Pos new_first = newlt->getFirstPos();
    Pos startpos = m_impl.getLTMgr().getBBStartPos(cur_bbid);
    Pos endpos = m_impl.getLTMgr().getBBEndPos(cur_bbid);
    if (startpos > anct_last && endpos < new_first) {
        return addPRStatus(v, LTConsistencyMgr::PR_STATUS_IN_MEMORY, cur_bbid);
    }
    //when goes to here, that means current bbid is not related to the spilling
    //or reloading, just set to inital status.
    addPRStatus(v, LTConsistencyMgr::PR_STATUS_UNDEF, cur_bbid);
}


void LTConsistencyMgr::computeLTConsistency()
{
    List<LifeTime const*> const& splitlst = m_impl.getSplittedLTList();
    BBListIter bbit;
    for (IRBB * bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        xcom::List<LifeTime const*>::Iter ltit;
        Pos startpos = m_impl.getLTMgr().getBBStartPos(bb->id());
        Pos endpos = m_impl.getLTMgr().getBBEndPos(bb->id());
        for (LifeTime const* newlt = splitlst.get_head(&ltit);
             newlt != nullptr; newlt = splitlst.get_next(&ltit)) {
            LifeTime const* anct = newlt->getAncestor();
            ASSERT0(anct);
            addConsistStatus(newlt, startpos, endpos, bb->id());
            addConsistStatus(anct, startpos, endpos, bb->id());
            addPRStatus(anct, newlt, bb->id());
        }
    }
}


//This function implements the inconsistency on forward edge, and there are
//five scenarios need to be processed:
//
// Scenario 1:
//    $anct crosses the OUT boundary of FROM BB, however it does NOT cross the
//    IN boundary of TO BB.
//    $new is reloaded from [mem] in TO BB, so it does NOT cross the IN boundary
//    of TO BB.
//
//    Solution:
//         1. Generate and insert a latch BB between BB FROM and BB TO.
//         2. Add spilling operation '[mem] <-- $anct' in the latch BB.
//    e.g:
//      Original CFG:
//         BB: FROM
//         ...
//         ... <-- $anct
//         ...
//            |
//            V
//         BB: TO
//         $new <-- [mem]
//         ... <-- $new
//         ...
//
//      Modified CFG:
//         BB: FROM
//         ...
//         ... <-- $anct
//         ...
//            |
//            V
//         BB: LATCH
//         [mem] <-- $anct
//            |
//            V
//         BB: TO
//         $new <-- [mem]
//         ... <-- $new
//         ...
//
// Scenario 2:
//    $anct is used in the FROM BB, and it crosses the OUT boundary of FROM BB.
//    $new is a local register in BB SUCCESSOR, the PR status in BB TO is
//    PR_STATUS_IN_MEMORY.
//
//    Solution:
//         1. Generate and insert a latch BB between BB FROM and BB TO.
//         2. Add spilling operation '[mem] <-- $anct' in the latch BB.
//
//    e.g:
//      Original CFG:
//         BB: FROM
//         ...
//         ... <-- $anct
//         ...
//            |
//            V
//         BB: TO
//         ...
//            |
//            V
//         ...
//            |
//            V
//         BB: SUCCESSOR
//         $new <-- [mem]
//         ... <-- $new
//         ...
//
//      Modified CFG:
//         BB: FROM
//         ...
//         ... <-- $anct
//         ...
//            |
//            V
//         BB: LATCH
//         [mem] <-- $anct
//            |
//            V
//         BB: TO
//         ...
//            |
//            V
//         ...
//            |
//            V
//         BB: SUCCESSOR
//         $new <-- [mem]
//         ... <-- $new
//         ...
//
// Scenario 3:
//    $anct is split in the FROM BB, so it does NOT cross the OUT boundary
//    of FROM BB.
//    $new crosses the IN boundary of TO BB, however it does NOT cross the OUT
//    boundary of FROM BB.
//
//    Solution:
//         1. Generate and insert a latch BB between BB FROM and BB TO.
//         2. Add reloading operation '$new <-- [mem]' in the latch BB.
//
//    e.g:
//      Original CFG:
//         BB: FROM
//         ...
//         ... <-- $anct
//         [mem] <-- $anct
//         ...
//            |
//            V
//         BB: TO
//         ...
//         ... <-- $new
//         ...
//
//      Modified CFG:
//         BB: FROM
//         ...
//         ... <-- $anct
//         [mem] <-- $anct
//         ...
//            |
//            V
//         BB: LATCH
//         $new <-- [mem]
//            |
//            V
//         BB: TO
//         ...
//         ... <-- $new
//         ...
//
// Scenario 4:
//    $anct is split in the BB PREDECESSOR, so it does NOT cross the OUT
//    boundary of FROM BB. The PR status in BB FROM is PR_STATUS_IN_MEMORY.
//    $new crosses the IN boundary of TO BB.
//
//    Solution:
//         1. Generate and insert a latch BB between BB FROM and BB TO.
//         2. Add reloading operation '$new <-- [mem]' in the latch BB.
//
//    e.g:
//      Original CFG:
//         BB: PREDECESSOR
//         ...
//         ... <-- $anct
//         [mem] <-- $anct
//         ...
//            |
//            V
//         ...
//            |
//            V
//         BB: FROM
//         ...
//            |
//            V
//         BB: TO
//         ...
//         ... <-- $new
//         ...
//
//      Modified CFG:
//         BB: PREDECESSOR
//         ...
//         ... <-- $anct
//         [mem] <-- $anct
//         ...
//            |
//            V
//         ...
//            |
//            V
//         BB: FROM
//         ...
//            |
//            V
//         BB: LATCH
//         $new <-- [mem]
//            |
//            V
//         BB: TO
//         ...
//         ... <-- $new
//         ...
//
// Scenario 5:
//    $anct crosses the OUT boundary of FROM BB, however it does NOT cross the
//    IN boundary of TO BB.
//    $new crosses the IN boundary of TO BB, however it does NOT cross the OUT
//    boundary of FROM BB.
//
//    Solution:
//         1. Generate and insert a latch BB between BB FROM and BB TO.
//         2. Add moving operation '$new <-- $anct' in the latch BB.
//    e.g:
//      Original CFG:
//         BB: FROM
//         ...
//         ... <-- $anct
//         ...
//            |
//            V
//         BB: TO
//         ...
//         ... <-- $new
//         ...
//
//      Modified CFG:
//         BB: FROM
//         ...
//         ... <-- $anct
//         ...
//            |
//            V
//         BB: LATCH
//         $new <-- $anct
//            |
//            V
//         BB: TO
//         ...
//         ... <-- $new
//         ...
//
bool LTConsistencyMgr::computeForwardEdgeConsistencyImpl(LifeTime const* anct,
    LifeTime const* newlt, UINT from, UINT to,
    OUT InConsistPairList & inconsist_lst)
{
    ASSERT0(anct && newlt);
    ASSERT0(from != BBID_UNDEF && to != BBID_UNDEF);
    Var const* v = m_impl.getRA().getSpillLoc(anct->getPrno());
    CONSIST_STATUS const fromst = getOutSt(newlt, from);
    CONSIST_STATUS const tost = getInSt(newlt, to);
    PR_STATUS const prst_from = getPRSt(v, from);
    PR_STATUS const prst_to = getPRSt(v, to);

    if (tost != CONSIST_VALID && fromst != CONSIST_VALID) {
        if (getOutSt(anct, from) != CONSIST_VALID) {
            //CASE:
            //$anct is not live-out from 'from' BB, it may be
            //localized in 'from' BB. $new is not live-in to 'to' BB, there will
            //be no data flow between BB 'from' and BB 'to' for this PR, so
            //there is no inconsistency problem.
            return true;
        }
        if (PR_STATUS_RELOADED != prst_to && PR_STATUS_IN_MEMORY != prst_to) {
            //CASE:
            //$anct is live-out from 'from' BB. $new is not live-in to 'to'
            //BB, this PR is PR_STATUS_SPILLED/PR_STATUS_SPILLED_AND_RELOAD/
            //PR_STATUS_UNDEF in 'to' BB, the data flow between BB 'from' and
            //BB 'to' works well, there is no inconsistency problem.
            return true;
        }

        //When goes here, that means the spilling operation will be added
        //before the control flow goes into BB TO, because we should ensure the
        //content of $anct is in the memory.

        //Implemented scenario 1, add spilling operation '[mem] <-- $anct' if
        //prst_to is PR_STATUS_RELOADED.
        //Implemented scenario 2, add spilling operation '[mem] <-- $anct' if
        //prst_to is PR_STATUS_IN_MEMORY.
        InConsistPair pair;
        genInconsistPair(pair, from, to, anct, nullptr, v,
                         INCONSIST_PR2MEM);
        inconsist_lst.append_tail(pair);
        return true;
    }

    if (tost == CONSIST_VALID && fromst != CONSIST_VALID) {
        if (getOutSt(anct, from) != CONSIST_VALID) {
            if (prst_from != PR_STATUS_SPILLED &&
                prst_from != PR_STATUS_IN_MEMORY) {
                //CASE:lsra_fix_inconsist.gr
                //$anct is not live-out from 'from' BB, it may be localized
                //in 'from' BB. Thus there is dispensable to insert
                //the copy operation.
                return true;
            }

            //When goes here, that means the reloading operation will be added
            //before the control flow goes into BB TO, because we should ensure
            //the content of $anct (already in memory) is in $new.

            //Implemented scenario 3, add reloading operation '$new <-- [mem]'
            //if prst_from is PR_STATUS_RELOADED.
            //Implemented scenario 4, add reloading operation '$new <-- [mem]'
            //if prst_from is PR_STATUS_IN_MEMORY.
            InConsistPair pair;
            genInconsistPair(pair, from, to, nullptr, newlt, v,
                             INCONSIST_MEM2PR);
            inconsist_lst.append_tail(pair);
            return true;
        }
        //Implemented scenario 5, add moving operation '$new <-- $anct'.
        InConsistPair pair;
        genInconsistPair(pair, from, to, anct, newlt, nullptr, INCONSIST_PR2PR);
        inconsist_lst.append_tail(pair);
        return true;
    }
    //When comes here, it is not certain that there is no inconsistency problem
    //in current check, need to do further check.
    return false;
}


//This function implements the inconsistency on backward edge, and there is
//one scenario need to be processed:
//
// Scenario 1:
//    $anct crosses the OUT boundary of TO BB, however, it dees NOT cross the IN
//    boundary of FROM BB.
//    $new crosses the OUT boundary of FROM BB, however, it does NOT cross the
//    IN boundary of TO BB.
//
//    Solution:
//         1. Generate and insert a latch BB between BB FROM and BB TO.
//         2. Add moving operation '$anct <-- $new' in the latch BB.
//    e.g:
//      Original CFG:
//            _________________
//            |               |
//            V               |
//         BB: TO             |
//         ...                |
//         ... <-- $anct      |
//         ...                |
//            |               |
//            V               |
//         ...                |
//            |               |
//            V               |
//         BB: FROM           |
//         ...                |
//         ... <-- $new       |
//         ...                |
//            |_______________|
//
//      Modified CFG:
//            _________________
//            |               |
//            V               |
//         BB: TO             |
//         ...                |
//         ... <-- $anct      |
//         ...                |
//            |               |
//            V            BB: LATCH
//         ...             $anct <-- $new
//            |               ^
//            V               |
//         BB: FROM           |
//         ...                |
//         ... <-- $new       |
//         ...                |
//            |_______________|
//
void LTConsistencyMgr::computeBackwardEdgeConsistencyImpl(LifeTime const* anct,
    LifeTime const* newlt, UINT from, UINT to,
    OUT InConsistPairList & inconsist_lst)
{
    ASSERT0(anct && newlt);
    ASSERT0(from != BBID_UNDEF && to != BBID_UNDEF);
    CONSIST_STATUS fromst = getOutSt(anct, from);
    CONSIST_STATUS tost = getInSt(anct, to);
    if (tost == CONSIST_VALID && fromst != CONSIST_VALID) {
        if (getOutSt(newlt, from) != CONSIST_VALID) {
            //CASE:lsra_fix_inconsist.gr
            //'newlt' is not live-out from 'from' BB, it may be
            //localized in 'from' BB. Thus there is dispensable to insert
            //the copy operation.
            return;
        }
        //Implemented scenario 1, add moving operation '$anct <-- $new'.
        InConsistPair pair;
        genInconsistPair(pair, from, to, newlt, anct, nullptr, INCONSIST_PR2PR);
        inconsist_lst.append_tail(pair);
    }
}


void LTConsistencyMgr::computeEdgeConsistencyImpl(
    xcom::Edge const* e,
    OUT InConsistPairList & inconsist_lst)
{
    List<LifeTime const*> const& splitlst = m_impl.getSplittedLTList();
    List<LifeTime const*>::Iter ltit;
    xcom::VexIdx from = e->from()->id();
    xcom::VexIdx to = e->to()->id();
    for (LifeTime const* newlt = splitlst.get_head(&ltit);
         newlt != nullptr; newlt = splitlst.get_next(&ltit)) {
        LifeTime const* anct = newlt->getAncestor();
        ASSERT0(anct);
        //If the inconsistency is found on forward edge or there is no
        //inconsistency problem, go to the next loop, or else need to do the
        //further check on backward edge.
        if (computeForwardEdgeConsistencyImpl(anct, newlt, from, to,
            inconsist_lst)) {
            continue;
        }
        computeBackwardEdgeConsistencyImpl(anct, newlt, from, to,
                                           inconsist_lst);
    }
}


void LTConsistencyMgr::computeEdgeConsistency(
    OUT InConsistPairList & inconsist_lst)
{
    xcom::EdgeIter it;
    for (xcom::Edge * e = m_cfg->get_first_edge(it); e != nullptr;
         e = m_cfg->get_next_edge(it)) {
        computeEdgeConsistencyImpl(e, inconsist_lst);
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


IRBB * LTConsistencyMgr::genLatchBB(MOD LatchMap & latch_map,
                                    InConsistPair const& pair)
{
    VexPair vp;
    vp.fromid = pair.from_vex_id;
    vp.toid = pair.to_vex_id;
    IRBB * latch = latch_map.get(vp);
    if (latch == nullptr) {
        IRBB * frombb = m_cfg->getBB(pair.from_vex_id);
        IRBB * tobb = m_cfg->getBB(pair.to_vex_id);
        latch = insertLatch(frombb, tobb);
        latch_map.set(vp, latch);
    }
    return latch;
}


void LTConsistencyMgr::reviseTypePR2PR(MOD LatchMap & latch_map,
                                       InConsistPair const& pair)
{
    IRBB * latch = genLatchBB(latch_map, pair);
    Type const* fromty = pair.from_lt->getFirstOccType();
    Type const* toty = pair.to_lt->getFirstOccType();
    ASSERT0(fromty && toty);
    IR * mv = m_impl.insertMove(pair.from_lt->getPrno(),
        pair.to_lt->getPrno(), fromty, toty, latch);
    dumpInsertMove(m_impl, latch, mv,
                   "fix lifetime consistency pr to pr $%u->$%u",
                   pair.from_lt->getPrno(), pair.to_lt->getPrno());
}


void LTConsistencyMgr::reviseTypeMEM2PR(MOD LatchMap & latch_map,
                                        InConsistPair const& pair)
{
    IRBB * latch = genLatchBB(latch_map, pair);
    Type const* toty = pair.to_lt->getFirstOccType();
    ASSERT0(toty);

    IR * reload = m_impl.insertReload(pair.to_lt->getPrno(),
                                      const_cast<Var*>(pair.mem_var),
                                      toty, latch);
    dumpReload(m_impl, reload, pair.to_lt->getPrno(), latch,
               "fix lifetime consistency memory to pr %s->$%u",
               pair.mem_var->get_name()->getStr(), pair.to_lt->getPrno());
}


void LTConsistencyMgr::reviseTypePR2MEM(MOD LatchMap & latch_map,
                                        InConsistPair const& pair)
{
    IRBB * latch = genLatchBB(latch_map, pair);
    Type const* fromty = pair.from_lt->getFirstOccType();
    ASSERT0(fromty);

    IR * spill = m_impl.insertSpillAtBBEnd(pair.from_lt->getPrno(),
                                           fromty, latch);
    dumpSpill(m_impl, spill, pair.from_lt->getPrno(), latch,
              "fix lifetime consistency pr to memory $%u->%s",
              pair.from_lt->getPrno(), pair.mem_var->get_name()->getStr());
}


void LTConsistencyMgr::reviseEdgeConsistency(
    InConsistPairList const& inconsist_lst)
{
    LatchMap inserted_latch;
    InConsistPairListIter it;
    UINT i = 0;
    for (InConsistPair pair = inconsist_lst.get_head(&it);
         i < inconsist_lst.get_elem_count();
         pair = inconsist_lst.get_next(&it), i++) {
        switch (pair.type) {
        case INCONSIST_PR2PR: reviseTypePR2PR(inserted_latch, pair); break;
        case INCONSIST_MEM2PR: reviseTypeMEM2PR(inserted_latch, pair); break;
        case INCONSIST_PR2MEM: reviseTypePR2MEM(inserted_latch, pair); break;
        default: UNREACHABLE(); break;
        }
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
        }
    }
    if (cand == nullptr) { return nullptr; }
    OccListIter it2;
    bool succ = cand->findOcc(furthest_range.start(), it2);
    ASSERT0_DUMMYUSE(succ);
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
            //lt can not be split at the given position because lt also
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


IR * SplitMgr::doSpillAfterSplitPos(LifeTime * lt, SplitCtx const& ctx)
{
    //There is no need to insert spill code in some cases:
    //1. split_pos is in a hole of lt.
    //Even if split_pos is DEF position, spill code is also needed, e.g:
    //spill caller-saved register at call-stmt, the position indicates
    //the call-stmt which is DEF position.
    IR * spill = nullptr;
    cutoffLTFromSpillPos(lt, ctx.split_pos);

    //Prepare the correct register type for spill
    Type const* reg_type = m_ra.getRegType(lt->getPrno());
    ASSERT0(reg_type);

    spill = m_impl.insertSpillAfter(lt->getPrno(), reg_type, ctx.split_pos_ir);
    ASSERT0(spill);
    dumpSpill(m_impl, spill, lt, ctx.split_pos_ir, false);
    return spill;
}


IR * SplitMgr::doSpillBeforeSplitPos(LifeTime * lt, SplitCtx const& ctx)
{
    IR * spill = nullptr;
    //Prepare the correct register type for spill
    Type const* reg_type = m_ra.getRegType(lt->getPrno());
    ASSERT0(reg_type);

    shrinkLTToSplitPos(lt, ctx.split_pos, ctx.split_pos_ir);
    spill = m_impl.insertSpillBefore(lt->getPrno(), reg_type, ctx.split_pos_ir);
    ASSERT0(spill);
    dumpSpill(m_impl, spill, lt, ctx.split_pos_ir, true);
    return spill;
}


IR * SplitMgr::insertSpillAroundSplitPos(LifeTime * lt, SplitCtx const& ctx)
{
    ASSERT0(lt->is_contain(ctx.split_pos));
    ASSERT0(ctx.split_pos_ir);

    if (UpdatePos::isDef(ctx.split_pos) && lt->isDefOcc(ctx.split_pos_ir)) {
        return doSpillAfterSplitPos(lt, ctx);
    }

    return doSpillBeforeSplitPos(lt, ctx);
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
                                            m_ra.getRegType(newlt->getPrno()),
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
    if (spill == nullptr) { return newlt; }

    //Record the spill and reload BBID info for current split.
    UINT bbid_split =
        ctx.split_pos_ir->is_stmt() ? ctx.split_pos_ir->getBB()->id() :
        ctx.split_pos_ir->getStmt()->getBB()->id();
    UINT bbid_reload = ctx.reload_occ.getBB()->id();
    Var const* v = spill->getIdinfo();
    m_impl.recordVar2Split(v, bbid_split, bbid_reload);
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


void LSRAImpl::dumpAssign(LSRAImpl & lsra, LifeTime const* lt,
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


void LSRAImpl::dumpBBList() const
{
    xoc::dumpBBList(m_bb_list, m_rg);
}


void LSRAImpl::forceAssignRegister(LifeTime const* lt, Reg reg)
{
    m_rsimpl.pickRegFromAllocable(reg);
    if (m_rsimpl.isCallee(reg)) {
        m_rsimpl.recordUsedCallee(reg);
    }
    if (m_rsimpl.isCaller(reg)) {
        m_rsimpl.recordUsedCaller(reg);
    }
    m_ra.setReg(lt->getPrno(), reg);
    dumpAssign(*this, lt, "assign dedicated register");
}


bool LSRAImpl::tryAssignCallee(IR const* ir, LifeTime const* lt)
{
    if (!m_ra.isCalleePermitted(lt)) { return false; }

    Reg r = m_rsimpl.pickCallee(ir);
    if (r != REG_UNDEF) {
        ASSERT0(m_rsimpl.isAvailAllocable(r));
        m_rsimpl.pickRegisterFromCalleeAliasSet(r);
        ASSERT0(m_rsimpl.isCallee(r));
        m_ra.setReg(lt->getPrno(), r);
        m_rsimpl.recordUsedCallee(r);
        dumpAssign(*this, lt, nullptr);
        return true;
    }
    return false;
}


bool LSRAImpl::tryAssignCaller(IR const* ir, LifeTime const* lt)
{
    Reg r = m_rsimpl.pickCaller(ir);
    if (r != REG_UNDEF) {
        ASSERT0(m_rsimpl.isAvailAllocable(r));
        ASSERT0(m_rsimpl.isCaller(r));
        m_rsimpl.pickRegisterFromCallerAliasSet(r);
        m_ra.setReg(lt->getPrno(), r);
        m_rsimpl.recordUsedCaller(r);
        dumpAssign(*this, lt, nullptr);
        return true;
    }
    return false;
}


bool LSRAImpl::tryAssignRegisterByPrefer(IR const* ir, LifeTime const* lt)
{
    ASSERT0(!lt->is_dedicated());
    ASSERT0(getLTPrefer(lt) != PREFER_UNDEF);

    if (getLTPrefer(lt) == PREFER_CALLEE) {
        if (tryAssignCallee(ir, lt)) { return true; }
        if (tryAssignCaller(ir, lt)) { return true; }
    }

    if (getLTPrefer(lt) == PREFER_CALLER) {
        return tryAssignRegisterDefault(ir, lt);
    }

    getActMgr().dump("ASSIGN:can NOT find register for $%u",
                     lt->getPrno());
    return false;
}


bool LSRAImpl::tryAssignRegisterDefault(IR const* ir, LifeTime const* lt)
{
    ASSERT0(!lt->is_dedicated());

    if (tryAssignCaller(ir, lt)) { return true; }
    if (tryAssignCallee(ir, lt)) { return true; }

    getActMgr().dump("ASSIGN:can NOT find register for $%u",
                      lt->getPrno());
    return false;
}


bool LSRAImpl::tryAssignRegister(IR const* ir, LifeTime const* lt)
{
    ASSERT0(!lt->is_dedicated());
    return (getLTPrefer(lt) == PREFER_UNDEF) ?
        tryAssignRegisterDefault(ir, lt) : tryAssignRegisterByPrefer(ir, lt);
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
        ASSERT0_DUMMYUSE(canbe);
        dumpSelectSplitCand(*this, t, curpos, true,
                            "split $%u that assigned %s",
                            t->getPrno(), m_ra.getRegName(r));
        LifeTime * newlt = spltmgr.splitAt(t, ctx);
        computeLTPrefer(newlt);
        m_ra.addUnhandled(newlt);
        set.remove(it);
        m_ra.getHandled().append_tail(t);
        m_rsimpl.freeReg(t);
    }
}


void LSRAImpl::splitLinkLT(Pos curpos, IR const* ir)
{
    ASSERT0(ir->isCallStmt());
    Reg l = getTIMgr().getLink();
    if (ir->isIntrinsicOp() || !m_rsimpl.isAvailAllocable(l)) { return; }
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
    RegSet used = m_rsimpl.getUsedCaller();
    for (BSIdx i = used.get_first(); i != BS_UNDEF; i = used.get_next(i)) {
        ASSERT0(i != REG_UNDEF);
        splitAllLTWithReg(curpos, ir, (Reg)i, m_ra.getActive());
    }
}


void LSRAImpl::saveCallee()
{
    RegSet used_callee = m_rsimpl.getUsedCallee();
    for (BSIdx i = used_callee.get_first();
         i != BS_UNDEF; i = used_callee.get_next(i)) {
        ASSERT0(m_rsimpl.isCallee(i));
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
    if (tryAssignRegister(ir, lt)) {
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
            m_rsimpl.freeReg(lt);
            continue;
        }
        if (lt->is_contain(curpos)) {
            //lt is not only conver 'curpos' but also in a range.
            //Transfer lt to active.
            inact.remove(it);
            act.append_tail(lt);
            continue;
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
            m_rsimpl.freeReg(lt);
            continue;
        }
        if (!lt->is_contain(curpos)) {
            //lt convers 'curpos' but in a hole.
            //Transfer lt to inactive.
            act.remove(it);
            inact.append_tail(lt);
            continue;
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
    ASSERTN(!stmt->is_phi(), ("LSRA does not support SSA mode"));
    stmt->getBB()->getIRList().insert_before(remat, stmt);
}


void LSRAImpl::insertReloadBefore(IR * reload, IR const* marker)
{
    ASSERT0(isReloadLikeOp(reload) && marker);
    m_ra.setReload(reload);
    IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
    ASSERT0(stmt->is_stmt());
    ASSERTN(!stmt->is_phi(), ("LSRA does not support SSA mode"));
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
    ASSERTN(!stmt->is_phi(), ("LSRA does not support SSA mode"));
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
    IR * reload = m_ra.buildReload(newres, spill_loc,
        ty->is_any() ? m_tm->getTargMachRegisterType() : ty);
    insertReloadBefore(reload, marker);
    return reload;
}


IR * LSRAImpl::insertReload(PRNO to, Var * v, Type const* ty, IRBB * bb)
{
    IR * reload = m_ra.buildReload(to, v,
        ty->is_any() ? m_tm->getTargMachRegisterType() : ty);
    m_ra.setReload(reload);
    bb->getIRList().append_tail_ex(reload);
    return reload;
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
            m_rsimpl.freeReg(cand);
        } else {
            //CASE:lt may be assigning-candidate, whereas lt is selected as the
            //splitting-candidate meanwhile. Thus after the function return, lt
            //will be assigned a register and newlt will be waiting for
            //assigning.
            //ASSERT0(m_ra.getUnhandled().find(lt));

            //Update the lifetime prefer only if it is not handled.
            computeLTPrefer(cand);
        }
        computeLTPrefer(newlt);
        succ = tryAssignRegister(curir, lt);
        count++;
    } while (!succ && count < 20);
    ASSERT0(succ);
    dumpAssign(*this, lt, nullptr);
    m_ra.addActive(lt);
    splitInActiveLTWithReg(curpos, curir, getReg(lt));
    ASSERT0(m_ra.verify4List());
}


void LSRAImpl::dump() const
{
    m_rsimpl.dumpAvailRegSet();
}


void LSRAImpl::computeRAPrefer()
{
    PRNO2LT const& prno2lt = getRA().getLTMgr().getPrno2LT();

    for (VecIdx i = 0; i <= prno2lt.get_last_idx(); i++) {
        if (prno2lt[i] == nullptr) { continue; }
        computeLTPrefer(prno2lt[i]);
    }
}


void LSRAImpl::computeLTPrefer(LifeTime const* lt)
{
    REG_PREFER prefer = PREFER_UNDEF;

    if (lt->getCallCrossedNum() > 1) {
        prefer = PREFER_CALLEE;
    }
    if (lt->getCallCrossedNum() == 1) {
        prefer = PREFER_CALLER;
    }
    m_lt2prefer.setAlways(lt, prefer);
}


bool LSRAImpl::perform(OptCtx & oc)
{
    m_oc = &oc;
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_LIVENESS_MGR,
                                               PASS_UNDEF);
    m_live_mgr = (LivenessMgr*)m_rg->getPassMgr()->queryPass(PASS_LIVENESS_MGR);
    ASSERT0(m_live_mgr && m_live_mgr->is_valid());

    //Compute the lifetime prefer for Register Allocation.
    computeRAPrefer();

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
