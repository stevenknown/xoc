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

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

#define DEFINE_SOLVESET_OPERATION(SET_NAME) \
    void SolveSetMgr::copy##SET_NAME(OUT SolveSet * set, SolveSet const& src) \
    { \
        if (isKeep##SET_NAME()) { \
            set->copy(src, *getGlobalSBSMgr()); \
        } else { \
            set->copy(src, *getLocalSBSMgr()); \
        } \
    } \
    void SolveSetMgr::clean##SET_NAME(OUT SolveSet * set) \
    { \
        if (isKeep##SET_NAME()) { \
            set->clean(*getGlobalSBSMgr()); \
        } else { \
            set->clean(*getLocalSBSMgr()); \
        } \
    } \
    void SolveSetMgr::intersect##SET_NAME(OUT SolveSet * set, \
                                          SolveSet const& src) \
    { \
        if (isKeep##SET_NAME()) { \
            set->intersect(src, *getGlobalSBSMgr()); \
        } else { \
            set->intersect(src, *getLocalSBSMgr()); \
        } \
    } \
    void SolveSetMgr::diff##SET_NAME(OUT SolveSet * set, SolveSet const& src) \
    { \
        if (isKeep##SET_NAME()) { \
            set->diff(src, *getGlobalSBSMgr()); \
        } else { \
            set->diff(src, *getLocalSBSMgr()); \
        } \
    } \
    void SolveSetMgr::diff##SET_NAME(OUT SolveSet * set, BSIdx elem) \
    { \
        if (isKeep##SET_NAME()) { \
            set->diff(elem, *getGlobalSBSMgr()); \
        } else { \
            set->diff(elem, *getLocalSBSMgr()); \
        } \
    } \
    void SolveSetMgr::bunion##SET_NAME(OUT SolveSet * set, \
                                       SolveSet const& src) \
    { \
        if (isKeep##SET_NAME()) { \
            set->bunion(src, *getGlobalSBSMgr()); \
        } else { \
            set->bunion(src, *getLocalSBSMgr()); \
        } \
    } \
    void SolveSetMgr::bunion##SET_NAME(OUT SolveSet * set, BSIdx elem) \
    { \
        if (isKeep##SET_NAME()) { \
            set->bunion(elem, *getGlobalSBSMgr()); \
        } else { \
            set->bunion(elem, *getLocalSBSMgr()); \
        } \
    } \
    void SolveSetMgr::reset##SET_NAME##IfLocal() \
    { \
        if (isKeep##SET_NAME()) { return; } \
        Vector<SolveSet*> & vec = get##SET_NAME##Vec(); \
        resetSolveSetVec(&vec, getLocalSBSMgr()); \
    } \
    void SolveSetMgr::reset##SET_NAME##IfGlobal() \
    { \
        if (!isKeep##SET_NAME()) { return; } \
        Vector<SolveSet*> & vec = get##SET_NAME##Vec(); \
        for (VecIdx i = 0; i <= vec.get_last_idx(); i++) { \
            SolveSet * bs = vec.get(i); \
            if (bs == nullptr) { continue; } \
            getGlobalSBSMgr()->destroySEGandFreeDBitSetCore(bs); \
        } \
        vec.destroy(); \
        vec.init(); \
    } \
    SolveSet * SolveSetMgr::gen##SET_NAME(UINT bbid) \
    { \
        ASSERT0(m_cfg->getBB(bbid)); \
        Vector<SolveSet*> & vec = get##SET_NAME##Vec(); \
        SolveSet * set = vec.get(bbid); \
        if (set == nullptr) { \
            if (isKeep##SET_NAME()) { \
                set = (SolveSet*)getGlobalSBSMgr()->allocDBitSetCore(); \
            } else { \
                set = (SolveSet*)getLocalSBSMgr()->allocDBitSetCore(); \
            } \
            set->set_sparse(SOL_SET_IS_SPARSE); \
            vec.set(bbid, set); \
        } \
        return set; \
    } \
    SolveSet * SolveSetMgr::get##SET_NAME(UINT bbid) const \
    { \
        ASSERT0(m_cfg->getBB(bbid)); \
        return const_cast<SolveSetMgr*>(this)->get##SET_NAME##Vec().get(bbid); \
    }


//Set the interal data attribute to non-sparse bitset if you think the
//analysis objects are few enough, moreover non-sparse set may speed up
//compilation.
#define SOL_SET_IS_SPARSE (true)

//Iterative methodology.
//TBD: In practical scenario, work-list based algorithm does not perform better
//than RPO based algorithm.
//#define WORK_LIST_DRIVE

static char const* getSolveFlagName(UFlag flag)
{
    static char g_name_buf[128];
    g_name_buf[0] = 0;
    bool is_first = true;
    if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
        if (!is_first) {
            xcom::xstrcat(g_name_buf, 128, ", ");
        }
        is_first = false;
        xcom::xstrcat(g_name_buf, 128, "DUOPT_SOL_AVAIL_REACH_DEF");
        flag.remove(DUOPT_SOL_AVAIL_REACH_DEF);
    }
    if (flag.have(DUOPT_SOL_REACH_DEF)) {
        if (!is_first) {
            xcom::xstrcat(g_name_buf, 128, ", ");
        }
        is_first = false;
        xcom::xstrcat(g_name_buf, 128, "DUOPT_SOL_REACH_DEF");
        flag.remove(DUOPT_SOL_REACH_DEF);
    }
    if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
        if (!is_first) {
            xcom::xstrcat(g_name_buf, 128, ", ");
        }
        is_first = false;
        xcom::xstrcat(g_name_buf, 128, "DUOPT_SOL_AVAIL_EXPR");
        flag.remove(DUOPT_SOL_AVAIL_EXPR);
    }
    if (flag.have(DUOPT_SOL_REGION_REF)) {
        if (!is_first) {
            xcom::xstrcat(g_name_buf, 128, ", ");
        }
        is_first = false;
        xcom::xstrcat(g_name_buf, 128, "DUOPT_SOL_REGION_REF");
        flag.remove(DUOPT_SOL_REGION_REF);
    }
    if (flag.have(DUOPT_COMPUTE_PR_REF)) {
        if (!is_first) {
            xcom::xstrcat(g_name_buf, 128, ", ");
        }
        is_first = false;
        xcom::xstrcat(g_name_buf, 128, "DUOPT_COMPUTE_PR_REF");
        flag.remove(DUOPT_COMPUTE_PR_REF);
    }
    if (flag.have(DUOPT_COMPUTE_NONPR_REF)) {
        if (!is_first) {
            xcom::xstrcat(g_name_buf, 128, ", ");
        }
        is_first = false;
        xcom::xstrcat(g_name_buf, 128, "DUOPT_COMPUTE_NONPR_REF");
        flag.remove(DUOPT_COMPUTE_NONPR_REF);
    }
    if (flag.have(DUOPT_COMPUTE_PR_DU)) {
        if (!is_first) {
            xcom::xstrcat(g_name_buf, 128, ", ");
        }
        is_first = false;
        xcom::xstrcat(g_name_buf, 128, "DUOPT_COMPUTE_PR_DU");
        flag.remove(DUOPT_COMPUTE_PR_DU);
    }
    if (flag.have(DUOPT_COMPUTE_NONPR_DU)) {
        if (!is_first) {
            xcom::xstrcat(g_name_buf, 128, ", ");
        }
        is_first = false;
        xcom::xstrcat(g_name_buf, 128, "DUOPT_COMPUTE_NONPR_DU");
        flag.remove(DUOPT_COMPUTE_NONPR_DU);
    }
    return g_name_buf;
}


static void resetSolveSetVec(Vector<SolveSet*> * ptr, DefMiscBitSetMgr * bsmgr)
{
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();
}


DEFINE_SOLVESET_OPERATION(ReachDefIn)
DEFINE_SOLVESET_OPERATION(ReachDefOut)
DEFINE_SOLVESET_OPERATION(AvailReachDefIn)
DEFINE_SOLVESET_OPERATION(AvailReachDefOut)
DEFINE_SOLVESET_OPERATION(GenIRExpr)
DEFINE_SOLVESET_OPERATION(AvailExprIn)
DEFINE_SOLVESET_OPERATION(AvailExprOut)
DEFINE_SOLVESET_OPERATION(MayGenDef)
DEFINE_SOLVESET_OPERATION(MustGenDef)
DEFINE_SOLVESET_OPERATION(MayKilledDef)
DEFINE_SOLVESET_OPERATION(MustKilledDef)
DEFINE_SOLVESET_OPERATION(KilledIRExpr)
DEFINE_SOLVESET_OPERATION(LiveInBB)


SolveSetMgr::~SolveSetMgr()
{
    resetLocalSet();
    resetGlobalSet();
}


void SolveSetMgr::resetGlobalSet()
{
    resetReachDefInIfGlobal();
    resetReachDefOutIfGlobal();
    resetAvailReachDefInIfGlobal();
    resetAvailReachDefOutIfGlobal();
    resetGenIRExprIfGlobal();
    resetAvailExprInIfGlobal();
    resetAvailExprOutIfGlobal();
    resetMayGenDefIfGlobal();
    resetMustGenDefIfGlobal();
    resetMayKilledDefIfGlobal();
    resetMustKilledDefIfGlobal();
    resetKilledIRExprIfGlobal();
    resetLiveInBBIfGlobal();
    getGlobalSBSMgr()->destroy();
    getGlobalSBSMgr()->init();
}


void SolveSetMgr::resetAllKillSet()
{
    resetMustKilledDefIfLocal();
    resetMayKilledDefIfLocal();
    resetKilledIRExprIfLocal();
}


//Free auxiliary data structure used in solving.
void SolveSetMgr::resetLocalSet()
{
    resetReachDefInIfLocal();
    resetReachDefOutIfLocal();
    resetAvailReachDefInIfLocal();
    resetAvailReachDefOutIfLocal();
    resetGenIRExprIfLocal();
    resetAvailExprInIfLocal();
    resetAvailExprOutIfLocal();
    resetMayGenDefIfLocal();
    resetMustGenDefIfLocal();
    resetLiveInBBIfLocal();
    resetAllKillSet();
    getLocalSBSMgr()->destroy();
    getLocalSBSMgr()->init();
}


//This equation needs May Kill Def and Must Gen Def.
bool SolveSetMgr::ForAvailReachDef(IRBB const* bb, List<IRBB*> * lst,
                                   DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    SolveSet news(SOL_SET_IS_SPARSE);
    SolveSet * availreachdefin = genAvailReachDefIn(bb->id());
    bool first = true;
    AdjVertexIter it;
    for (Vertex const* in = Graph::get_first_in_vertex(bb->getVex(), it);
         in != nullptr; in = Graph::get_next_in_vertex(it)) {
        if (first) {
            first = false;
            copyAvailReachDefIn(availreachdefin,
                                *genAvailReachDefOut(in->id()));
        } else {
            intersectAvailReachDefIn(availreachdefin,
                                     *genAvailReachDefOut(in->id()));
        }
    }
    news.copy(*availreachdefin, bsmgr);
    SolveSet const* killset = getMayKilledDef(bb->id());
    if (killset != nullptr) {
        news.diff(*killset, bsmgr);
    }
    news.bunion(*genMustGenDef(bb->id()), bsmgr);

    SolveSet * out = genAvailReachDefOut(bb->id());
    if (!out->is_equal(news)) {
        copyAvailReachDefOut(out, news);
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        for (xcom::EdgeC const* ecs = bb->getVex()->getOutList();
             ecs != nullptr;ecs = EC_next(ecs);) {
            UINT succ = ecs->getToId();
            ASSERT0(m_cfg->getBB(succ));
            lst->append_tail(m_cfg->getBB(succ));
        }
        #endif
    }
    news.clean(bsmgr);
    return change;
}


bool SolveSetMgr::ForReachDef(IRBB const* bb, List<IRBB*> * lst,
                              DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    SolveSet * in_reach_def = genReachDefIn(bb->id());
    SolveSet news(SOL_SET_IS_SPARSE);
    bool first = true;
    AdjVertexIter it;
    for (Vertex const* in = Graph::get_first_in_vertex(bb->getVex(), it);
         in != nullptr; in = Graph::get_next_in_vertex(it)) {
        SolveSet * outreach = getReachDefOut(in->id());
        ASSERTN(outreach, ("should be initialized"));
        if (first) {
            copyReachDefIn(in_reach_def, *outreach);
            first = false;
            continue;
        }
        bunionReachDefIn(in_reach_def, *outreach);
    }
    if (first) {
        //bb does not have predecessor.
        ASSERT0(in_reach_def->is_empty());
    }

    news.copy(*in_reach_def, bsmgr);
    SolveSet const* killset = getMustKilledDef(bb->id());
    if (killset != nullptr) {
        news.diff(*killset, bsmgr);
    }

    SolveSet const* maygen = genMayGenDef(bb->id());
    if (maygen != nullptr) {
        news.bunion(*maygen, bsmgr);
    }

    SolveSet * out_reach_def = getReachDefOut(bb->id());
    ASSERTN(out_reach_def, ("should be initialized"));
    if (!out_reach_def->is_equal(news)) {
        copyReachDefOut(out_reach_def, news);
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        for (xcom::EdgeC const* ecs = bb->getVex()->getOutList();
             ecs != nullptr; ecs = ecs->get_next()) {
            UINT succ = ecs->getToId();
            ASSERT0(m_cfg->getBB(succ));
            lst->append_tail(m_cfg->getBB(succ));
        }
        #endif
    }
    news.clean(bsmgr);
    return change;
}


bool SolveSetMgr::ForAvailExpression(IRBB const* bb, List<IRBB*> * lst,
                                     DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    SolveSet news(SOL_SET_IS_SPARSE);
    SolveSet * availexprin = genAvailExprIn(bb->id());
    bool first = true;
    AdjVertexIter it;
    for (Vertex const* in = Graph::get_first_in_vertex(bb->getVex(), it);
         in != nullptr; in = Graph::get_next_in_vertex(it)) {
        SolveSet * exprout = genAvailExprOut(in->id());
        if (first) {
            first = false;
            copyAvailExprIn(availexprin, *exprout);
        } else {
            intersectAvailExprIn(availexprin, *exprout);
        }
    }
    news.copy(*availexprin, bsmgr);
    SolveSet const* set = getKilledIRExpr(bb->id());
    if (set != nullptr) {
        news.diff(*set, bsmgr);
    }
    news.bunion(*genGenIRExpr(bb->id()), bsmgr);
    SolveSet * out = genAvailExprOut(bb->id());
    if (!out->is_equal(news)) {
        copyAvailExprOut(out, news);
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        for (xcom::EdgeC const* ecs = bb->getVex()->getOutList();
             ecs != nullptr; ecs = ecs->get_next()) {
            UINT succ = ecs->getToId();
            ASSERT0(m_cfg->getBB(succ));
            lst->append_tail(m_cfg->getBB(succ));
        }
        #endif
    }
    news.clean(bsmgr);
    return change;
}


void SolveSetMgr::solveByRPO(RPOVexList const* rpovexlst, UFlag const flag,
                             MOD DefMiscBitSetMgr & bsmgr)
{
    bool change;
    UINT count = 0;
    do {
        change = false;
        RPOVexListIter ct;
        for (rpovexlst->get_head(&ct); ct != rpovexlst->end();
             ct = rpovexlst->get_next(ct)) {
            IRBB * bb = m_cfg->getBB(ct->val()->id());
            if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
                change |= ForAvailReachDef(bb, nullptr, bsmgr);
            }
            if (flag.have(DUOPT_SOL_REACH_DEF)) {
                change |= ForReachDef(bb, nullptr, bsmgr);
            }
            if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
                change |= ForAvailExpression(bb, nullptr, bsmgr);
            }
        }
        count++;
    } while (change && count < 20);
    //UINT i = count * tbbl->get_elem_count(); //time of bb accessed.
    ASSERT0(!change);
}


void SolveSetMgr::solveByWorkList(List<IRBB*> * tbbl, UFlag const flag,
                                  MOD DefMiscBitSetMgr & bsmgr)
{
    BBListIter ct;
    List<IRBB*> lst;
    for (tbbl->get_head(&ct); ct != tbbl->end(); ct = tbbl->get_next(ct)) {
        IRBB * p = ct->val();
        lst.append_tail(p);
    }
    UINT count = tbbl->get_elem_count() * 20;
    UINT i = 0; //time of bb accessed.
    do {
        IRBB * bb = lst.remove_head();
        if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
            ForAvailReachDef(bb, &lst, bsmgr);
        }
        if (flag.have(DUOPT_SOL_REACH_DEF)) {
            ForReachDef(bb, &lst, bsmgr);
        }
        if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
            ForAvailExpression(bb, &lst, bsmgr);
        }
        i++;
    } while (lst.get_elem_count() != 0);
    ASSERT0(i < count);
    DUMMYUSE(count);
}


//Solve reaching definitions problem for IR STMT and
//computing LIVE IN and LIVE OUT IR expressions.
//expr_univers: the Universal SET for ExpRep.
void SolveSetMgr::solve(SolveSet const& expr_univers, UFlag const flag,
                        MOD DefMiscBitSetMgr & bsmgr)
{
    START_TIMER(t7, "Solve DU set");
    IRBB const* entry = m_cfg->getEntry();
    ASSERT0(entry && BB_is_entry(entry));
    for (IRBB * bb = m_bblst->get_tail();
         bb != nullptr; bb = m_bblst->get_prev()) {
        UINT bbid = bb->id();
        if (flag.have(DUOPT_SOL_REACH_DEF)) {
            //Initialize reach-def-in, reach-def-out.
            cleanReachDefIn(genReachDefIn(bbid));
            cleanReachDefOut(genReachDefOut(bbid));
        }

        if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
            cleanAvailReachDefIn(genAvailReachDefIn(bbid));
        }

        if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
            //Initialize available in, available out expression.
            //IN-SET of BB must be universal of all IR-expressions.
            SolveSet * availin = genAvailExprIn(bbid);
            SolveSet * availout = genAvailExprOut(bbid);
            if (bbid == entry->id()) {
                //AvailIn and AvailOut of entry should be empty.
                cleanAvailExprIn(availin);
                cleanAvailExprOut(availout);
            } else {
                copyAvailExprIn(availin, expr_univers);
                copyAvailExprOut(availout, *availin);
                SolveSet const* set = getKilledIRExpr(bbid);
                if (set != nullptr) {
                    diffAvailExprOut(availout, *set);
                }
                bunionAvailExprOut(availout, *genGenIRExpr(bbid));
            }
        }
    }

    //Rpo already checked to be available. Here double check again.
    RPOVexList * vexlst = m_cfg->getRPOVexList();
    ASSERT0(vexlst);
    ASSERT0(vexlst->get_elem_count() == m_bblst->get_elem_count());
    #ifdef WORK_LIST_DRIVE
    solveByWorkList(vexlst, flag, bsmgr);
    #else
    solveByRPO(vexlst, flag, bsmgr);
    #endif
    END_TIMER(t7, "Solve DU set");
}


void SolveSetMgr::setMustKilledDef(UINT bbid, SolveSet * set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_must_killed_def.set(bbid, set);
}


void SolveSetMgr::setMayKilledDef(UINT bbid, SolveSet * set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_may_killed_def.set(bbid, set);
}


//Return IR expression-id set.
void SolveSetMgr::setKilledIRExpr(UINT bbid, SolveSet * set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_killed_ir_exp.set(bbid, set);
}


//NOTE: MD referrence must be available.
//mustdefs: record must modified MD for each bb.
//maydefs: record may modified MD for each bb.
//mayuse: record may used MD for each bb.
//        collect mayuse (NOPR-DU) to compute Region referred MD.
void SolveSetMgr::computeMustExactDefMayDefMayUse(
    OUT Vector<MDSet*> * mustdefmds,
    OUT Vector<MDSet*> * maydefmds,
    OUT MDSet * mayusemds,
    UFlag flag)
{
    START_TIMER_FMT(t3, ("Build MustDef, MayDef, MayUse: %s",
                         getSolveFlagName(flag)));
    if (flag.have(DUOPT_SOL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
        ASSERT0(mustdefmds);
    }
    if (flag.have(DUOPT_SOL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_EXPR)) {
        ASSERT0(maydefmds);
    }
    if (flag.have(DUOPT_SOL_REGION_REF)) {
        ASSERT0(mayusemds);
    }
    BBListIter ct;
    ConstMDIter mditer;
    xcom::DefMiscBitSetMgr * lsbsmgr = getLocalSBSMgr();
    for (m_bblst->get_head(&ct);
         ct != m_bblst->end(); ct = m_bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        SolveSet * maygendef = nullptr;
        SolveSet * mustgendef = nullptr;
        MDSet * bb_mustdefmds = nullptr;
        MDSet * bb_maydefmds = nullptr;
        UINT bbid = bb->id();
        if (mustdefmds != nullptr) {
            //if (mustdefmds != nullptr &&
            //    flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
            //For now, only the computation of available-reach-def need
            //MustGenStmt information. It is very slowly when compiling large
            //region.
            //UPDATE: It is not arrurate or even correct if we only
            //compute mustgendef when computing available-reach-def.
            //May be both available-reach-def, reach-def, and available-exp
            //need mustgendef information.
            //Consider case:
            //   BB1:p1 = 1               |
            //   /             \          |
            //BB2:p1 = 2  |  BB3:p1 = 3   |
            //   \            /           |
            //   BB4: ... = p1            |
            //where BB1 is precessor of BB2 and BB3.
            //BB1:p1 should not reach-def at BB4.
            bb_mustdefmds = mustdefmds->get(bbid);
            mustgendef = genMustGenDef(bbid);
            cleanMustGenDef(mustgendef);
        }
        if (maydefmds != nullptr) {
            bb_maydefmds = maydefmds->get(bbid);
            maygendef = genMayGenDef(bbid);
            cleanMayGenDef(maygendef);
        }
        computeMustExactDefMayDefMayUseForBB(
            bb, mditer, mustdefmds, maydefmds, mayusemds, bb_mustdefmds,
            bb_maydefmds, mustgendef, maygendef, flag, *lsbsmgr);
    }
    END_TIMER_FMT(t3, ("Build MustDef, MayDef, MayUse: %s",
                       getSolveFlagName(flag)));
}


void SolveSetMgr::computeMustExactDefMayDefMayUseForBB(
    IRBB * bb, ConstMDIter & mditer,
    OUT Vector<MDSet*> * mustdefmds, OUT Vector<MDSet*> * maydefmds,
    OUT MDSet * mayusemds, MDSet * bb_mustdefmds, MDSet * bb_maydefmds,
    SolveSet * mustgendef, SolveSet * maygendef,
    UFlag flag, DefMiscBitSetMgr & bsmgr)
{
    //may_def_mds, must_def_mds should be already clean.
    CollectMayUseRecur co(m_rg);
    IRListIter irct;
    for (BB_irlist(bb).get_head(&irct);
         irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
        IR const* ir = irct->val();
        ASSERT0(ir);
        if (mayusemds != nullptr) {
            co.collect(ir, flag.have(DUOPT_COMPUTE_PR_DU), bsmgr, *mayusemds);
            //collectMayUse(ir, *mayusemds, isComputePRDU());
        }

        if (!ir->hasResult()) { continue; }

        //Do not compute MustExactDef/MayDef for PR.
        if (ir->isWritePR() && !flag.have(DUOPT_COMPUTE_PR_DU)) {
            continue;
        }

        //Collect mustdef MDSet.
        if (bb_mustdefmds != nullptr) {
            //For now, only the computation of available-reach-def need
            //MustGenStmt information. It is very slowly when compiling large
            //region.
            //UPDATE: It is not arrurate or even correct if we only
            //compute mustgendef when computing available-reach-def.
            //May be both available-reach-def, reach-def, and available-exp
            //need mustgendef information.
            //Consider case:
            //   BB1:p1 = 1              |
            //   /            \          |
            //BB2:p1 = 2  |  BB3:p1 = 3  |
            //   \            /          |
            //   BB4: ... = p1           |
            //where BB1 is precessor of BB2 and BB3.
            //BB1:p1 should not reach-def at BB4.
            //ASSERT0(flag.have(DUOPT_SOL_AVAIL_REACH_DEF));

            ASSERT0(mustgendef);
            computeMustExactDef(ir, bb_mustdefmds, mustgendef, mditer,
                                bsmgr, flag);
        }
        if (bb_maydefmds != nullptr) {
            computeMayDef(ir, bb_maydefmds, maygendef, bsmgr, flag);
        }
    }
}


void SolveSetMgr::collectNonPRMayDef(
    IR const* ir, DefMiscBitSetMgr & bsmgr, OUT MDSet * maydefmds) const

{
    if (ir->isWritePR()) {
        //Note call-stmt does not belong to WritePR.
        return;
    }

    MD const* ref = ir->getMustRef();
    if (ref != nullptr && !ref->is_exact()) {
        maydefmds->bunion(ref, bsmgr);
    }

    //Collect maydef mds.
    MDSet const* refs = const_cast<IR*>(ir)->getMayRef();
    if (refs != nullptr && !refs->is_empty()) {
        maydefmds->bunion(*refs, bsmgr);
    }
}


void SolveSetMgr::computeMayDef(
    IR const* ir, MDSet * bb_maydefmds, SolveSet * maygendef,
    DefMiscBitSetMgr & bsmgr, UFlag flag)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
        if (flag.have(DUOPT_SOL_REGION_REF)) {
            collectNonPRMayDef(ir, bsmgr, bb_maydefmds);
        }
        if (!flag.have(DUOPT_COMPUTE_NONPR_DU)) { return; }
        break;
    SWITCH_CASE_CALL:
        if (flag.have(DUOPT_SOL_REGION_REF)) {
            collectNonPRMayDef(ir, bsmgr, bb_maydefmds);
        }
        if (!flag.have(DUOPT_COMPUTE_PR_DU)) { return; }
        break;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_PHI:
        if (!flag.have(DUOPT_COMPUTE_PR_DU)) { return; }
        break;
    case IR_REGION:
        //Region does not have any def.
        break;
    default: //Handle general stmt.
        ASSERT0(!ir->isMemRef());
    }

    //Computing May GEN set of reach-definition.
    //The computation of reach-definition problem is conservative.
    //If we can not say whether a DEF is killed, regard it as lived STMT.
    DefSBitSetIter st = nullptr;
    BSIdx ni;
    for (BSIdx i = maygendef->get_first(&st); i != BS_UNDEF; i = ni) {
        ni = maygendef->get_next(i, &st);
        IR * gened_ir = m_rg->getIR(i);
        ASSERT0(gened_ir != nullptr && gened_ir->is_stmt());
        if (isMustKill(ir, gened_ir)) {
            diffMayGenDef(maygendef, i);
        }
    }
    bunionMayGenDef(maygendef, (BSIdx)ir->id());
}


//Return true if 'def1' may modify md-set that 'def2' generated.
//'def1': should be stmt.
//'def2': should be stmt.
bool SolveSetMgr::isMayKill(IR const* def1, IR const* def2)
{
    ASSERT0(def1->is_stmt() && def2->is_stmt());
    if (def1->is_stpr() && def2->is_stpr() && STPR_no(def1) == STPR_no(def2)) {
        return true;
    }

    MD const* md1 = const_cast<IR*>(def1)->getMustRef();
    MDSet const* mds1 = const_cast<IR*>(def1)->getMayRef();
    MD const* md2 = const_cast<IR*>(def2)->getMustRef();
    MDSet const* mds2 = const_cast<IR*>(def2)->getMayRef();

    if (md1 != nullptr) {
        if (md2 != nullptr && md1 == md2) {
            return true;
        }
        if (mds2 != nullptr && (mds1 == mds2 || mds2->is_contain(md1, m_rg))) {
            return true;
        }
        return false;
    }

    if (mds1 != nullptr) {
        if (md2 != nullptr && mds1->is_contain(md2, m_rg)) {
            return true;
        }
        if (mds2 != nullptr && (mds2 == mds1 || mds1->is_intersect(*mds2))) {
            return true;
        }
    }
    return false;
}


//Return true if 'def1' exactly modified md that 'def2' generated.
//'def1': should be stmt.
//'def2': should be stmt.
bool SolveSetMgr::isMustKill(IR const* def1, IR const* def2)
{
    ASSERT0(def1->is_stmt() && def2->is_stmt());
    if ((def1->isWritePR() || def1->isCallStmt()) &&
        (def2->isWritePR() || def2->isCallStmt())) {
        if ((def1->isCallStmt() && !def1->hasReturnValue()) ||
            (def2->isCallStmt() && !def2->hasReturnValue())) {
            return false;
        }
        return def1->getPrno() == def2->getPrno();
    }

    MD const* md1 = def1->getExactRef();
    MD const* md2 = def2->getExactRef();
    if (md1 != nullptr && md2 != nullptr && md1 == md2)  {
        return true;
    }
    return false;
}


void SolveSetMgr::computeMustExactDef(
    IR const* ir, OUT MDSet * bb_mustdefmds, SolveSet * mustgendef,
    ConstMDIter & mditer, DefMiscBitSetMgr & bsmgr, UFlag flag)
{
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
        //NonPR ref.
        if (!flag.have(DUOPT_COMPUTE_NONPR_DU)) { return; }
        break;
    SWITCH_CASE_CALL:
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_PHI:
        //PR ref.
        if (!flag.have(DUOPT_COMPUTE_PR_DU)) { return; }
        break;
    case IR_REGION:
        //Region does not have any def.
        break;
    default:
        ASSERTN(!ir->isMemRef(), ("miss handling entry for memory ref"));
    }
    MD const* x = ir->getExactRef();
    if (x != nullptr) {
        //call may not have return value.
        bb_mustdefmds->bunion(x, bsmgr);

        //Add MD which is exact and overlapped with x.
        m_md_sys->computeOverlapExactMD(x, bb_mustdefmds, mditer, bsmgr);
    } else if (ir->isWritePR() || ir->isCallHasRetVal()) {
        ASSERT0(ir->getRefMD());
        bb_mustdefmds->bunion(ir->getRefMD(), bsmgr);
    }

    //Computing Must GEN set of reach-definition.
    DefSBitSetIter st = nullptr;
    BSIdx ni;
    for (BSIdx i = mustgendef->get_first(&st); i != BS_UNDEF; i = ni) {
        ni = mustgendef->get_next(i, &st);
        IR * gened_ir = m_rg->getIR(i);
        ASSERT0(gened_ir != nullptr && gened_ir->is_stmt());
        if (isMayKill(ir, gened_ir)) {
            diffMustGenDef(mustgendef, i);
        }
    }
    bunionMustGenDef(mustgendef, (BSIdx)ir->id());
}


void SolveSetMgr::computeLiveInBB(DefMiscBitSetMgr & bsmgr)
{
    START_TIMER(t4, "Compute LiveInBB");
    bool change = true;
    RPOVexList const* vexlst = m_cfg->getRPOVexList();
    ASSERT0(vexlst);
    SolveSet tmp(SOL_SET_IS_SPARSE);
    UINT count = 0;
    while (change && count < 20) {
        change = false;
        RPOVexListIter it;
        for (vexlst->get_head(&it); it != vexlst->end();
             it = vexlst->get_next(it)) {
            IRBB const* bb = m_cfg->getBB(it->val()->id());
            SolveSet * bs = genLiveInBB(bb->id());
            tmp.clean(bsmgr);
            for (xcom::EdgeC * el = bb->getVex()->getInList();
                 el != nullptr; el = el->get_next()) {
                IRBB const* pred = m_cfg->getBB(el->getFromId());
                if (pred == nullptr) { continue; }
                tmp.bunion(*genLiveInBB(pred->id()), bsmgr);
                tmp.bunion(pred->id(), bsmgr);
            }
            if (bs->is_equal(tmp)) { continue; }
            copyLiveInBB(bs, tmp);
            change = true;
        }
        count++;
    }
    tmp.clean(bsmgr);
    ASSERT0(!change);
    END_TIMER(t4, "Compute LiveInBB");
}


void SolveSetMgr::computeKillSetByMayGenDef(
    UINT bbid, bool comp_must, bool comp_may, MDSet const* bb_mustdefmds,
    MDSet const* bb_maydefmds, DefMiscBitSetMgr & bsmgr, OUT SolveSet & output)
{
    SolveSet const* maygendef = genMayGenDef(bbid);
    ASSERT0(maygendef);
    SolveSetIter it;
    for (BSIdx i = maygendef->get_first(&it);
         i != BS_UNDEF; i = maygendef->get_next(i, &it)) {
        IR const* stmt = m_rg->getIR(i);
        ASSERT0(stmt->is_stmt());
        if (comp_must) {
            MD const* stmt_mustexactdef_md = stmt->getExactRef();
            if (stmt_mustexactdef_md == nullptr) { continue; }
            if (bb_mustdefmds->is_contain(
                    stmt_mustexactdef_md, m_rg)) {
                output.bunion(i, bsmgr);
            }
        }
        if (comp_may) {
            //Compute may killed stmts, for avail-reach-def.
            MD const* stmt_effectdef_md = stmt->getMustRef();
            if (stmt_effectdef_md != nullptr &&
                bb_mustdefmds->is_contain(stmt_effectdef_md, m_rg)) {
                output.bunion(i, bsmgr);
                continue;
            }

            MDSet const* maydef = const_cast<IR*>(stmt)->getMayRef();
            if (maydef == nullptr) { continue; }
            if (bb_maydefmds->is_intersect(*maydef)) {
                output.bunion(i, bsmgr);
            }
        }
    }
}


//Compute must and may killed stmt.
//NOTE: computation of maykill and mustkill both need may-gen-def.
void SolveSetMgr::computeKillSetByLiveInBB(
    UINT bbid, bool comp_must, bool comp_may, MDSet const* bb_mustdefmds,
    MDSet const* bb_maydefmds, DefMiscBitSetMgr & bsmgr, OUT SolveSet & output)
{
    SolveSet const* liveinbbs = genLiveInBB(bbid);
    ASSERT0(liveinbbs);
    SolveSetIter it;
    for (BSIdx in = liveinbbs->get_first(&it);
         in != BS_UNDEF; in = liveinbbs->get_next(in, &it)) {
        if (in == (BSIdx)bbid) { continue; }
        ASSERT0(m_cfg->getBB(in));
        computeKillSetByMayGenDef(in, comp_must, comp_may, bb_mustdefmds,
                                  bb_maydefmds, bsmgr, output);
    }
}


//Compute must and may killed stmt.
//mustdefs: record must modified MD for each bb.
//maydefs: record may modified MD for each bb.
//NOTE: computation of maykill and mustkill both need may-gen-def.
void SolveSetMgr::computeKillSet(
    DefDBitSetCoreReserveTab & dbitsetchash,
    Vector<MDSet*> const* mustexactdefmds, Vector<MDSet*> const* maydefmds,
    DefMiscBitSetMgr & bsmgr)
{
    START_TIMER(t4, "Build KillSet");
    ASSERT0(mustexactdefmds || maydefmds);
    computeLiveInBB(bsmgr);
    BBListIter ct;
    SolveSet tmp_must_killed_def(SOL_SET_IS_SPARSE);
    SolveSet tmp_may_killed_def(SOL_SET_IS_SPARSE);
    for (m_bblst->get_head(&ct);
         ct != m_bblst->end(); ct = m_bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        UINT bbid = bb->id();
        bool comp_must = false;
        MDSet const* bb_mustdefmds = nullptr;
        if (mustexactdefmds != nullptr) {
            bb_mustdefmds = mustexactdefmds->get(bbid);
            if (bb_mustdefmds != nullptr && !bb_mustdefmds->is_empty()) {
                //Caller expect to compute MustKilledDef
                comp_must = true;
            }
        }
        bool comp_may = false;
        MDSet const* bb_maydefmds = nullptr;
        if (maydefmds != nullptr) {
            bb_maydefmds = maydefmds->get(bbid);
            if (bb_maydefmds != nullptr && !bb_maydefmds->is_empty()) {
                //Caller expect to compute MayKilledDef.
                comp_may = true;
            }
        }
        if (!comp_must && !comp_may) {
            setMustKilledDef(bbid, nullptr);
            setMayKilledDef(bbid, nullptr);
            continue;
        }
        computeKillSetByLiveInBB(bbid, comp_must, comp_may, bb_mustdefmds,
                                 bb_maydefmds, bsmgr, tmp_must_killed_def);
        setMustKilledDef(bbid, comp_must ?
                         dbitsetchash.append(tmp_must_killed_def) : nullptr);
        setMayKilledDef(bbid, comp_may ?
                        dbitsetchash.append(tmp_may_killed_def) : nullptr);
        tmp_must_killed_def.clean(bsmgr);
        tmp_may_killed_def.clean(bsmgr);
    }
    ASSERT0(tmp_must_killed_def.is_empty() && tmp_may_killed_def.is_empty());
    resetLiveInBBIfLocal(); //clean the resource ASAP.
    END_TIMER(t4, "Build KillSet");
}


//Return true if ir can be candidate of live-expr.
static bool canBeLiveExprCand(IR const* ir)
{
    ASSERT0(ir);
    switch (ir->getCode()) {
    SWITCH_CASE_BIN:
    SWITCH_CASE_READ_PR:
    SWITCH_CASE_BITWISE_UNA:
    SWITCH_CASE_LOGIC_UNA:
    case IR_NEG:
    case IR_SELECT:
        return true;
    default: break;
    }
    return false;
}


void SolveSetMgr::computeGenExprForMayDef(
    IR const* ir, OUT SolveSet * gen_ir_expr, DefMiscBitSetMgr & bsmgr)
{
    ASSERT0(ir->is_stmt());
    //Compute lived IR expression after current statement executed.
    //e.g:
    //  i = i + 1 //S1
    //
    //  lhs 'i' killed the rhs expression: 'i + 1', that means
    //  'i + 1' is dead after S1 statement.
    MDSet const* maydef = const_cast<IR*>(ir)->getMayRef();
    MD const* mustdef = ir->getMustRef();
    if (maydef == nullptr && mustdef == nullptr) { return; }
    DefSBitSetIter st = nullptr;
    MDSet tmp;
    CollectMayUseRecur co(m_rg);
    for (BSIdx j = gen_ir_expr->get_first(&st), nj; j != BS_UNDEF; j = nj) {
        nj = gen_ir_expr->get_next(j, &st);
        IR * tir = m_rg->getIR(j);
        ASSERT0(tir != nullptr);
        if (tir->is_lda() || tir->is_const()) {
            continue;
        }
        tmp.clean(bsmgr);
        co.collect(tir, true, bsmgr, tmp);
        //collectMayUse(tir, tmp, true);
        if ((maydef != nullptr && maydef->is_intersect(tmp)) ||
            (mustdef != nullptr && tmp.is_contain(mustdef, m_rg))) {
            //'ir' killed 'tir'.
            diffGenIRExpr(gen_ir_expr, j);
        }
    }
    tmp.clean(bsmgr);
}


//The function collect the IR expression that being the GenExpr.
//expr_univers: local set.
void SolveSetMgr::computeGenExprForStmt(
    IR const* ir, OUT SolveSet * gen_ir_expr, OUT SolveSet & expr_univers,
    DefMiscBitSetMgr & bsmgr)
{
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
        if (canBeLiveExprCand(ST_rhs(ir))) {
            //Compute the generated expressions set.
            bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(ST_rhs(ir)));
            expr_univers.bunion(IR_id(ST_rhs(ir)), bsmgr);
        }
        computeGenExprForMayDef(ir, gen_ir_expr, bsmgr);
        break;
    case IR_STPR:
        if (canBeLiveExprCand(STPR_rhs(ir))) {
            //Compute the generated expressions set.
            bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(STPR_rhs(ir)));
            expr_univers.bunion(IR_id(STPR_rhs(ir)), bsmgr);
        }
        computeGenExprForMayDef(ir, gen_ir_expr, bsmgr);
        break;
    SWITCH_CASE_WRITE_ARRAY:
        if (canBeLiveExprCand(STARR_rhs(ir))) {
            //Compute the generated expressions set.
            bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(STARR_rhs(ir)));
            expr_univers.bunion(IR_id(STARR_rhs(ir)), bsmgr);
        }
        computeGenExprForMayDef(ir, gen_ir_expr, bsmgr);
        break;
    SWITCH_CASE_INDIRECT_MEM_STMT:
        //Compute the generated expressions set.
        if (canBeLiveExprCand(IST_rhs(ir))) {
            bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(IST_rhs(ir)));
            expr_univers.bunion(IR_id(IST_rhs(ir)), bsmgr);
        }
        if (canBeLiveExprCand(IST_base(ir))) {
            //e.g: *(int*)0x1000 = 10, IST_base(ir) is nullptr.
            bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(IST_base(ir)));
            expr_univers.bunion(IR_id(IST_base(ir)), bsmgr);
        }
        computeGenExprForMayDef(ir, gen_ir_expr, bsmgr);
        break;
    SWITCH_CASE_CALL: {
        //Compute the generated expressions set.
        if (ir->is_icall()) {
            ASSERT0(ICALL_callee(ir)->is_ld());
            if (canBeLiveExprCand(ICALL_callee(ir))) {
                bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(ICALL_callee(ir)));
                expr_univers.bunion(IR_id(ICALL_callee(ir)), bsmgr);
            }
        }
        for (IR * p = CALL_arg_list(ir); p != nullptr; p = p->get_next()) {
            if (canBeLiveExprCand(p)) {
                bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(p));
                expr_univers.bunion(IR_id(p), bsmgr);
            }
        }
        computeGenExprForMayDef(ir, gen_ir_expr, bsmgr);
        break;
    }
    case IR_IGOTO:
        //Compute the generated expressions.
        if (canBeLiveExprCand(IGOTO_vexp(ir))) {
            bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(IGOTO_vexp(ir)));
            expr_univers.bunion(IR_id(IGOTO_vexp(ir)), bsmgr);
        }
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        //Compute the generated expressions.
        if (canBeLiveExprCand(BR_det(ir))) {
            bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(BR_det(ir)));
            expr_univers.bunion(IR_id(BR_det(ir)), bsmgr);
        }
        break;
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
        //Compute the generated expressions.
        if (canBeLiveExprCand(SWITCH_vexp(ir))) {
            bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(SWITCH_vexp(ir)));
            expr_univers.bunion(IR_id(SWITCH_vexp(ir)), bsmgr);
        }
        break;
    case IR_RETURN:
        if (RET_exp(ir) != nullptr) {
            if (canBeLiveExprCand(RET_exp(ir))) {
                bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(RET_exp(ir)));
                expr_univers.bunion(IR_id(RET_exp(ir)), bsmgr);
            }
        }
        break;
    case IR_PHI:
        //Since phis are always at head of BB, no live-expr killed by them.
        for (IR * p = PHI_opnd_list(ir); p != nullptr; p = p->get_next()) {
            if (canBeLiveExprCand(p)) {
                bunionGenIRExpr(gen_ir_expr, (BSIdx)IR_id(p));
                expr_univers.bunion(IR_id(p), bsmgr);
            }
        }
        break;
    case IR_REGION:
    case IR_GOTO:
        break;
    default: UNREACHABLE();
    }
}


//Compute generated-EXPR for BB.
//expr_univers: local set.
void SolveSetMgr::computeGenExprForBB(
    IRBB * bb, OUT SolveSet & expr_univers, DefMiscBitSetMgr & bsmgr)
{
    SolveSet * gen_ir_expr = genGenIRExpr(bb->id());
    cleanGenIRExpr(gen_ir_expr);
    IRListIter ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end(); ct = BB_irlist(bb).get_next(ct)) {
        IR const* ir = ct->val();
        ASSERT0(ir->is_stmt());
        computeGenExprForStmt(ir, gen_ir_expr, expr_univers, bsmgr);
    }
}


//Compute local-gen IR-EXPR set and killed IR-EXPR set.
//'expr_universe': record the universal of all ir-expr of region.
void SolveSetMgr::computeAuxSetForExpression(
    DefDBitSetCoreReserveTab & bshash, OUT SolveSet * expr_universe,
    Vector<MDSet*> const* maydefmds, DefMiscBitSetMgr & bsmgr)
{
    START_TIMER(t5, "Build AvailableExp");
    ASSERT0(expr_universe && maydefmds);
    BBListIter ct;
    for (m_bblst->get_head(&ct);
         ct != m_bblst->end(); ct = m_bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        computeGenExprForBB(bb, *expr_universe, bsmgr);
    }

    //Compute kill-set.
    //The defined MDSet of current ir, killed all
    //other exprs which used MDSet that modified by 'ir'.
    SolveSet tmp_killed_set(SOL_SET_IS_SPARSE);
    MDSet tmp;
    CollectMayUseRecur co(m_rg);
    for (m_bblst->get_head(&ct);
         ct != m_bblst->end(); ct = m_bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        MDSet const* bb_maydefmds = maydefmds->get(bb->id());
        ASSERT0(bb_maydefmds != nullptr);

        DefSBitSetIter st = nullptr;
        for (BSIdx i = expr_universe->get_first(&st);
             i != BS_UNDEF; i = expr_universe->get_next(i, &st)) {
            IR * ir = m_rg->getIR(i);
            ASSERT0(ir->is_exp());
            if (ir->is_lda() || ir->is_const()) { continue; }

            tmp.clean(bsmgr);
            co.collect(ir, true, bsmgr, tmp);
            //collectMayUse(ir, *tmp, true);

            if (bb_maydefmds->is_intersect(tmp)) {
                tmp_killed_set.bunion(i, bsmgr);
            }
        }

        setKilledIRExpr(bb->id(), bshash.append(tmp_killed_set));
        tmp_killed_set.clean(bsmgr);
    }
    tmp.clean(bsmgr);
    END_TIMER(t5, "Build AvailableExp");
}


//Compute maydef, mustdef, mayuse information for current region.
void SolveSetMgr::computeRegionMDDU(
    Vector<MDSet*> const* mustexactdef_mds, Vector<MDSet*> const* maydef_mds,
    MDSet const* mayuse_mds)
{
    START_TIMER(t6, "Build Region DefUse MDSet");
    ASSERT0(mustexactdef_mds && maydef_mds && mayuse_mds);
    m_rg->initRefInfo();

    //Note the SBSMgr of region MayDef and MayUse should use Region's
    //SBSMgr. The MayDef and MayUse set will be freed at Region's destructor.
    xcom::DefMiscBitSetMgr * sbsmgr = m_rg->getMiscBitSetMgr();

    MDSet * rg_maydef = m_rg->genMayDef();
    ASSERT0(rg_maydef);
    rg_maydef->clean(*sbsmgr);

    MDSet * rg_mayuse = m_rg->genMayUse();
    ASSERT0(rg_mayuse);
    rg_mayuse->clean(*sbsmgr);

    BBListIter ct = nullptr;
    VarTab const* vtab = m_rg->getVarTab();
    for (m_bblst->get_head(&ct);
         ct != m_bblst->end(); ct = m_bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        MDSet const* mds = mustexactdef_mds->get(bb->id());
        ASSERT0(mds != nullptr);
        MDSetIter iter;
        for (BSIdx i = mds->get_first(&iter);
             i != BS_UNDEF; i = mds->get_next(i, &iter)) {
            MD const* md = m_md_sys->getMD(i);
            ASSERT0(md->get_base());
            if (!md->is_pr() && !vtab->find(md->get_base())) {
                //Only record the Var defined in outer region.
                rg_maydef->bunion(md, *sbsmgr);
            }
        }

        mds = maydef_mds->get(bb->id());
        ASSERT0(mds != nullptr);
        for (BSIdx i = mds->get_first(&iter);
             i != BS_UNDEF; i = mds->get_next(i, &iter)) {
            MD const* md = m_md_sys->getMD(i);
            ASSERT0(md->get_base());
            if (!md->is_pr() && !vtab->find(md->get_base())) {
                //Only record the Var defined in outer region.
                rg_maydef->bunion(md, *sbsmgr);
            }
        }
    }

    MDSetIter iter = nullptr;
    for (BSIdx i = mayuse_mds->get_first(&iter);
         i != BS_UNDEF; i = mayuse_mds->get_next(i, &iter)) {
        MD const* md = m_md_sys->getMD(i);
        ASSERT0(md->get_base());
        if (!md->is_pr() && !vtab->find(md->get_base())) {
            //Only record the Var defined in outer region.
            rg_mayuse->bunion(md, *sbsmgr);
        }
    }

    REGION_is_ref_valid(m_rg) = true;
    END_TIMER(t6, "Build Region DefUse MDSet");
}


//set: the set of IRBB.
static void dumpBBSet(Region const* rg, SolveSet const* set,
                      CHAR const* set_name)
{
    if (set == nullptr) { return; }
    ASSERT0(rg->isLogMgrInit());
    StrBuf buf(32);
    buf.strcat("%s:(%lubyte)", set_name, (ULONG)set->count_mem());
    note(rg, "\n%s", buf.buf);
    set->dump(rg->getLogMgr()->getFileHandler());
}


//set: the set of IR stmt or expression.
static void dumpIRSet(Region const* rg, SolveSet const* set,
                      CHAR const* set_name, UINT ind, bool is_dump_bs)
{
    if (set == nullptr) { return; }
    ASSERT0(rg->isLogMgrInit());
    xcom::StrBuf buf(32);
    buf.strcat("%s:(%lubyte) ", set_name, (ULONG)set->count_mem());
    note(rg, "\n%s", buf.buf);
    SolveSetIter it = nullptr;
    for (BSIdx i = set->get_first(&it);
         i != BS_UNDEF; i = set->get_next(i, &it)) {
        IR * ir = rg->getIR(i);
        ASSERT0(ir);
        prt(rg, "%s, ", dumpIRName(ir, buf));
    }
    if (is_dump_bs) {
        rg->getLogMgr()->incIndent(ind);
        note(rg, "\n");
        set->dump(rg->getLogMgr()->getFileHandler());
        rg->getLogMgr()->decIndent(ind);
    }
}


//is_bs: true to dump bitset info.
void SolveSetMgr::dump(bool is_dump_bs) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n\n==---- DUMP SolveSetMgr '%s' ----==\n",
         m_rg->getRegionName());
    BBListIter cb;
    SolveSetMgr * pthis = const_cast<SolveSetMgr*>(this);
    UINT ind = 2;
    for (IRBB * bb = m_bblst->get_head(&cb); bb != nullptr;
         bb = m_bblst->get_next(&cb)) {
        UINT bbid = bb->id();
        note(getRegion(), "\n---- BB%d ----", bbid);
        dumpIRSet(m_rg, pthis->getReachDefIn(bbid),
                  "REACH-DEF-IN", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getReachDefOut(bbid),
                "REACH-DEF-OUT", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getAvailReachDefIn(bbid),
                "AVAIL-REACH-DEF-IN", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getAvailReachDefOut(bbid),
                  "AVAIL-REACH-DEF-OUT", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getMustGenDef(bbid),
                  "MUST-GEN-DEF", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getMayGenDef(bbid),
                  "MAY-GEN-DEF", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getMustKilledDef(bbid),
                  "MUST-KILLED-DEF", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getMayKilledDef(bbid),
                  "MAY-KILLED-DEF", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getAvailExprIn(bbid),
                  "AVAIL-EXPR-IN", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getAvailExprOut(bbid),
                  "AVAIL-EXPR-OUT", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getGenIRExpr(bbid),
                  "GEN-IR-EXPR", ind, is_dump_bs);
        dumpIRSet(m_rg, pthis->getKilledIRExpr(bbid),
                  "KILLED-IR-EXPR", ind, is_dump_bs);
        dumpBBSet(m_rg, pthis->genLiveInBB(bbid), "LIVE-IN-BB");
    }
}


static size_t dumpMemUsageImpl(Region const* rg, SolveSet const* set,
                               CHAR const* set_name)
{
    if (set == nullptr) { return 0; }
    ASSERT0(rg->isLogMgrInit());
    size_t n = set->count_mem();
    CHAR const* str = nullptr;
    if (n < 1024) { str = "B"; }
    else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
    else  { n /= 1024*1024; str = "MB"; }
    rg->getLogMgr()->incIndent(2);
    StrBuf buf(32);
    SolveSetIter it;
    buf.strcat("%s:%lu%s, %d elems, last %d", set_name,
               (ULONG)n, str, set->get_elem_count(), set->get_last(&it));
    note(rg, "\n%s", buf.buf);
    rg->getLogMgr()->decIndent(2);
    return n;
}


//Dump mem usage for each internal set of bb.
void SolveSetMgr::dumpMemUsage() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(),
         "\n==---- DUMP '%s' DUMgr : Memory Usage for Value Set ----==",
         m_rg->getRegionName());
    size_t count = 0;
    SolveSetMgr * pthis = const_cast<SolveSetMgr*>(this);
    for (IRBB * bb = m_bblst->get_head();
         bb != nullptr; bb = m_bblst->get_next()) {
        note(getRegion(), "\n--- BB%d ---", bb->id());
        UINT bbid = bb->id();
        count += dumpMemUsageImpl(m_rg, pthis->getReachDefIn(bbid),
                                  "REACH-DEF-IN");
        count += dumpMemUsageImpl(m_rg, pthis->getReachDefOut(bbid),
                                  "REACH-DEF-OUT");
        count += dumpMemUsageImpl(m_rg, pthis->getAvailReachDefIn(bbid),
                                  "AVAIL-REACH-DEF-IN");
        count += dumpMemUsageImpl(m_rg, pthis->getAvailReachDefOut(bbid),
                                  "AVAIL-REACH-DEF-OUT");
        count += dumpMemUsageImpl(m_rg, pthis->getMustGenDef(bbid),
                                  "MUST-GEN-DEF");
        count += dumpMemUsageImpl(m_rg, pthis->getMayGenDef(bbid),
                                  "MAY-GEN-DEF");
        count += dumpMemUsageImpl(m_rg, pthis->getMustKilledDef(bbid),
                                  "MUST-KILLED-DEF");
        count += dumpMemUsageImpl(m_rg, pthis->getMayKilledDef(bbid),
                                  "MAY-KILLED-DEF");
        count += dumpMemUsageImpl(m_rg, pthis->getAvailExprIn(bbid),
                                  "AVAIL-EXPR-IN");
        count += dumpMemUsageImpl(m_rg, pthis->getAvailExprOut(bbid),
                                  "AVAIL-EXPR-OUT");
        count += dumpMemUsageImpl(m_rg, pthis->getGenIRExpr(bbid),
                                  "GEN-IR-EXPR");
        count += dumpMemUsageImpl(m_rg, pthis->getKilledIRExpr(bbid),
                                  "KILLED-IR-EXPR");
        count += dumpMemUsageImpl(m_rg, pthis->getKilledIRExpr(bbid),
                                  "LIVE-IN-BB");
    }
    CHAR const* str = nullptr;
    if (count < 1024) { str = "B"; }
    else if (count < 1024 * 1024) { count /= 1024; str = "KB"; }
    else { count /= 1024*1024; str = "MB"; }
    note(getRegion(), "\nTotal %u%s", (UINT)count, str);
}


//Count up the memory has been allocated.
size_t SolveSetMgr::count_mem() const
{
    Vector<SolveSet*> const* ptr;
    size_t count = 0;
    SolveSetMgr * pthis = const_cast<SolveSetMgr*>(this);
    count += m_avail_reach_def_in.count_mem();
    ptr = &m_avail_reach_def_in;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_avail_reach_def_out.count_mem();
    ptr = &m_avail_reach_def_out;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_reach_def_in.count_mem();
    ptr = &m_reach_def_in;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_reach_def_out.count_mem();
    ptr = &m_reach_def_out;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_may_gen_def.count_mem();
    ptr = &m_may_gen_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_must_gen_def.count_mem();
    ptr = &m_must_gen_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_must_killed_def.count_mem();
    for (VecIdx i = 0; i <= m_must_killed_def.get_last_idx(); i++) {
        SolveSet const* dset = m_must_killed_def.get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_may_killed_def.count_mem();
    for (VecIdx i = 0; i <= m_may_killed_def.get_last_idx(); i++) {
        SolveSet const* dset = m_may_killed_def.get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += pthis->getGenIRExprVec().count_mem();
    ptr = &pthis->getGenIRExprVec();
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_killed_ir_exp.count_mem();
    for (VecIdx i = 0; i <= m_killed_ir_exp.get_last_idx(); i++) {
        SolveSet const* dset = m_killed_ir_exp.get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_avail_exp_in.count_mem();
    ptr = &m_avail_exp_in;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_avail_exp_out.count_mem();
    ptr = &m_avail_exp_out;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        SolveSet * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    return count;
}


static void finiSolveSetVec(BBList const* bblst,
                            MDSet *& mds_arr_for_must, MDSet *& mds_arr_for_may,
                            MDSet *& mayuse_mds,
                            Vector<MDSet*> *& mustexactdef_mds,
                            Vector<MDSet*> *& maydef_mds,
                            DefMiscBitSetMgr * lbsmgr)
{
    if (mustexactdef_mds != nullptr) {
        ASSERT0(mds_arr_for_must);
        for (UINT i2 = 0; i2 < bblst->get_elem_count(); i2++) {
            mds_arr_for_must[i2].clean(*lbsmgr);
        }
        delete [] mds_arr_for_must;
        delete mustexactdef_mds;
        mustexactdef_mds = nullptr;
    }
    if (maydef_mds != nullptr) {
        ASSERT0(mds_arr_for_may);
        for (UINT i2 = 0; i2 < bblst->get_elem_count(); i2++) {
            mds_arr_for_may[i2].clean(*lbsmgr);
        }
        delete [] mds_arr_for_may;
        delete maydef_mds;
        maydef_mds = nullptr;
    }
    if (mayuse_mds != nullptr) {
        mayuse_mds->clean(*lbsmgr);
        delete mayuse_mds;
    }
}


static void initSolveSetVec(
    BBList const* bblst, MDSet *& mds_arr_for_must, MDSet *& mds_arr_for_may,
    MDSet *& mayuse_mds, Vector<MDSet*> *& mustexactdef_mds,
    Vector<MDSet*> *& maydef_mds, UFlag const& flag)
{
    ASSERT0(maydef_mds == nullptr &&
            mustexactdef_mds == nullptr &&
            mayuse_mds == nullptr &&
            mds_arr_for_must == nullptr &&
            mds_arr_for_may == nullptr);
    if (flag.have(DUOPT_SOL_REGION_REF)) {
        mayuse_mds = new MDSet();
    }
    if (flag.have(DUOPT_SOL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_REACH_DEF) ||
        flag.have(DUOPT_SOL_REGION_REF)) {
        mustexactdef_mds = new Vector<MDSet*>();
    }
    if (flag.have(DUOPT_SOL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_EXPR) ||
        flag.have(DUOPT_SOL_REGION_REF)) {
        maydef_mds = new Vector<MDSet*>();
    }
    START_TIMER(t2, "Allocate May/Must MDS table");
    if (mustexactdef_mds != nullptr) {
        mds_arr_for_must = new MDSet[bblst->get_elem_count()]();
    }
    if (maydef_mds != nullptr) {
        mds_arr_for_may = new MDSet[bblst->get_elem_count()]();
    }
    UINT i = 0;
    BBListIter it;
    for (IRBB * bb = bblst->get_tail(&it);
         bb != nullptr; bb = bblst->get_prev(&it), i++) {
        if (mustexactdef_mds != nullptr) {
            mustexactdef_mds->set(bb->id(), &mds_arr_for_must[i]);
        }
        if (maydef_mds != nullptr) {
            maydef_mds->set(bb->id(), &mds_arr_for_may[i]);
        }
    }
    END_TIMER(t2, "Allocate May/Must MDS table");
}


//Return true if region status changed.
bool SolveSetMgr::perform(MOD OptCtx & oc, UFlag flag)
{
    ASSERT0(oc.is_ref_valid());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    ASSERT0(m_cfg->getRPOVexList());
    Vector<MDSet*> * maydef_mds = nullptr;
    Vector<MDSet*> * mustexactdef_mds = nullptr;
    MDSet * mayuse_mds = nullptr;
    MDSet * mds_arr_for_must = nullptr;
    MDSet * mds_arr_for_may = nullptr;
    initSolveSetVec(m_bblst, mds_arr_for_must, mds_arr_for_may, mayuse_mds,
                    mustexactdef_mds, maydef_mds, flag);
    DefMiscBitSetMgr * lbsmgr = getLocalSBSMgr();
    computeMustExactDefMayDefMayUse(mustexactdef_mds, maydef_mds,
                                    mayuse_mds, flag);
    DefDBitSetCoreHashAllocator dbitsetchashallocator(lbsmgr);
    DefDBitSetCoreReserveTab * dbitsetchash =
        new DefDBitSetCoreReserveTab(&dbitsetchashallocator);
    if (flag.have(DUOPT_SOL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
        computeKillSet(*dbitsetchash, mustexactdef_mds, maydef_mds, *lbsmgr);
    }

    SolveSet tmp_expr_univers(SOL_SET_IS_SPARSE);
    if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
        //Compute GEN, KILL IR-EXPR.
        computeAuxSetForExpression(*dbitsetchash, &tmp_expr_univers, maydef_mds,
                                   *lbsmgr);
    }
    delete dbitsetchash; //destroy useless resource as soon as possible.
    dbitsetchash = nullptr; //destroy useless resource as soon as possible.
    resetAllKillSet();

    if (flag.have(DUOPT_SOL_REGION_REF)) {
        //Compute DEF,USE mds for Region.
        computeRegionMDDU(mustexactdef_mds, maydef_mds, mayuse_mds);
    }

    if (flag.have(DUOPT_SOL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_EXPR)) {
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
        solve(tmp_expr_univers, flag, *lbsmgr);
    }

    //Free and destroy local used resource.
    tmp_expr_univers.clean(*lbsmgr);
    finiSolveSetVec(m_bblst, mds_arr_for_must, mds_arr_for_may, mayuse_mds,
                    mustexactdef_mds, maydef_mds, lbsmgr);
    resetLocalSet();

    //Set opt-context variables.
    if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
        oc.setValidPass(PASS_AVAIL_REACH_DEF);
    }
    if (flag.have(DUOPT_SOL_REACH_DEF)) {
        oc.setValidPass(PASS_REACH_DEF);
    }
    if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
        oc.setValidPass(PASS_LIVE_EXPR);
    }
    return false;
}

} //namespace xoc
