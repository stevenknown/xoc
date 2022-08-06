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

//Set the interal data attribute to no sparse
//if you think the analysis objects are few enough,
//and no-sparse set may speed up compilation.
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


SolveSet::~SolveSet()
{
    resetLocalSet();
    resetGlobalSet();
}


void SolveSet::resetGlobalSet()
{
    resetReachDefInSet();
    getGlobalSBSMgr()->destroy();
    getGlobalSBSMgr()->init();
}


void SolveSet::resetKillSet()
{
    DefMiscBitSetMgr * bsmgr = getLocalSBSMgr();
    Vector<DefDBitSetCore*> * ptr = nullptr;

    ptr = &m_must_killed_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    ptr = &m_may_killed_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    ptr = &m_killed_ir_exp;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();
}


//Free auxiliary data structure used in solving.
void SolveSet::resetLocalSet()
{
    DefMiscBitSetMgr * bsmgr = getLocalSBSMgr();
    Vector<DefDBitSetCore*> * ptr = nullptr;

    ptr = &m_avail_in_reach_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    ptr = &m_avail_out_reach_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    //Note that only reach-def in is useful for computing DU chain.
    //reach-def out can be destroied.
    ptr = &m_out_reach_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    ptr = &m_may_gen_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    ptr = &m_must_gen_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    ptr = &m_gen_ir_exp;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    ptr = &m_avail_in_exp;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    ptr = &m_avail_out_exp;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) { dset->clean(*bsmgr); }
    }
    ptr->destroy();
    ptr->init();

    resetKillSet();
    resetLiveInBB();

    getLocalSBSMgr()->destroy();
    getLocalSBSMgr()->init();
}


void SolveSet::resetLiveInBB()
{
    for (VecIdx i = 0; i <= m_livein_bb.get_last_idx(); i++) {
        DefSBitSetCore * bs = m_livein_bb.get(i);
        if (bs == nullptr) { continue; }
        bs->clean(*getLocalSBSMgr());
    }
    m_livein_bb.destroy();
    m_livein_bb.init();
}


void SolveSet::resetReachDefInSet()
{
    for (VecIdx i = 0; i <= m_in_reach_def.get_last_idx(); i++) {
        DefDBitSetCore * bs = m_in_reach_def.get(i);
        if (bs == nullptr) { continue; }
        getGlobalSBSMgr()->destroySEGandFreeDBitSetCore(bs);
    }
    m_in_reach_def.destroy();
    m_in_reach_def.init();
}


//This equation needs May Kill Def and Must Gen Def.
bool SolveSet::ForAvailReachDef(UINT bbid, List<IRBB*> & preds,
                                List<IRBB*> * lst, DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    DefDBitSetCore news(SOL_SET_IS_SPARSE);
    DefDBitSetCore * in = genAvailInReachDef(bbid);
    bool first = true;
    BBListIter ct;
    for (preds.get_head(&ct); ct != preds.end(); ct = preds.get_next(ct)) {
        IRBB * p = ct->val();
        //Intersect
        if (first) {
            first = false;
            in->copy(*genAvailOutReachDef(p->id()), bsmgr);
        } else {
            in->intersect(*genAvailOutReachDef(p->id()), bsmgr);
            //in->bunion(*getAvailOutReachDef(p)->id(), *getSBSMgr());
        }
    }

    news.copy(*in, bsmgr);
    DefDBitSetCore const* killset = getMayKilledDef(bbid);
    if (killset != nullptr) {
        news.diff(*killset, bsmgr);
    }
    news.bunion(*genMustGenDef(bbid), bsmgr);

    DefDBitSetCore * out = genAvailOutReachDef(bbid);
    if (!out->is_equal(news)) {
        out->copy(news, bsmgr);
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        xcom::Vertex * bbv = m_cfg->getVertex(bbid);
        for (xcom::EdgeC const* ecs = bbv->getOutList();
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


bool SolveSet::ForReachDef(UINT bbid, List<IRBB*> & preds, List<IRBB*> * lst,
                           DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    DefDBitSetCore * in_reach_def = genInReachDef(bbid);
    DefDBitSetCore news(SOL_SET_IS_SPARSE);

    bool first = true;
    BBListIter ct;
    for (preds.get_head(&ct); ct != preds.end(); ct = preds.get_next(ct)) {
        IRBB const* p = ct->val();
        xcom::DefDBitSetCore * outreach = getOutReachDef(p->id());
        ASSERTN(outreach, ("should be initialized"));
        if (first) {
            in_reach_def->copy(*outreach, *getGlobalSBSMgr());
            first = false;
            continue;
        }
        in_reach_def->bunion(*outreach, *getGlobalSBSMgr());
    }

    if (first) {
        //bb does not have predecessor.
        ASSERT0(in_reach_def->is_empty());
    }

    news.copy(*in_reach_def, bsmgr);
    DefDBitSetCore const* killset = getMustKilledDef(bbid);
    if (killset != nullptr) {
        news.diff(*killset, bsmgr);
    }

    DefDBitSetCore const* maygen = genMayGenDef(bbid);
    if (maygen != nullptr) {
        news.bunion(*maygen, bsmgr);
    }

    DefDBitSetCore * out_reach_def = getOutReachDef(bbid);
    ASSERTN(out_reach_def, ("should be initialized"));
    if (!out_reach_def->is_equal(news)) {
        out_reach_def->copy(news, bsmgr);
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        xcom::Vertex * bbv = m_cfg->getVertex(bbid);
        for (xcom::EdgeC const* ecs = bbv->getOutList();
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


bool SolveSet::ForAvailExpression(UINT bbid, List<IRBB*> & preds,
                                  List<IRBB*> * lst, DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    DefDBitSetCore news(SOL_SET_IS_SPARSE);

    bool first = true;
    DefDBitSetCore * in = genAvailInExpr(bbid);
    BBListIter ct;
    for (preds.get_head(&ct); ct != preds.end(); ct = preds.get_next(ct)) {
        IRBB * p = ct->val();
        DefDBitSetCore * liveout = genAvailOutExpr(p->id());
        if (first) {
            first = false;
            in->copy(*liveout, *getLocalSBSMgr());
        } else {
            in->intersect(*liveout, *getLocalSBSMgr());
        }
    }

    news.copy(*in, bsmgr);
    DefDBitSetCore const* set = getKilledIRExpr(bbid);
    if (set != nullptr) {
        news.diff(*set, bsmgr);
    }
    news.bunion(*genGenIRExpr(bbid), bsmgr);
    DefDBitSetCore * out = genAvailOutExpr(bbid);
    if (!out->is_equal(news)) {
        out->copy(news, *getLocalSBSMgr());
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        xcom::Vertex * bbv = m_cfg->getVertex(bbid);
        for (xcom::EdgeC const* ecs = bbv->getOutList();
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


void SolveSet::solveByRPO(RPOVexList const* rpovexlst, UFlag const flag,
                          MOD DefMiscBitSetMgr & bsmgr)
{
    List<IRBB*> preds;
    bool change;
    UINT count = 0;
    do {
        change = false;
        RPOVexListIter ct;
        for (rpovexlst->get_head(&ct); ct != rpovexlst->end();
             ct = rpovexlst->get_next(ct)) {
            IRBB * bb = m_cfg->getBB(ct->val()->id());
            UINT bbid = bb->id();
            preds.clean();
            m_cfg->get_preds(preds, bb);
            if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
                change |= ForAvailReachDef(bbid, preds, nullptr, bsmgr);
            }
            if (flag.have(DUOPT_SOL_REACH_DEF)) {
                change |= ForReachDef(bbid, preds, nullptr, bsmgr);
            }
            if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
                change |= ForAvailExpression(bbid, preds, nullptr, bsmgr);
            }
        }
        count++;
    } while (change && count < 20);
    //UINT i = count * tbbl->get_elem_count(); //time of bb accessed.
    ASSERT0(!change);
}


void SolveSet::solveByWorkList(List<IRBB*> * tbbl, UFlag const flag,
                               MOD DefMiscBitSetMgr & bsmgr)
{
    BBListIter ct;
    List<IRBB*> lst;
    for (tbbl->get_head(&ct); ct != tbbl->end(); ct = tbbl->get_next(ct)) {
        IRBB * p = ct->val();
        lst.append_tail(p);
    }

    List<IRBB*> preds;
    UINT count = tbbl->get_elem_count() * 20;
    UINT i = 0; //time of bb accessed.
    do {
        IRBB * bb = lst.remove_head();
        UINT bbid = bb->id();
        preds.clean();
        m_cfg->get_preds(preds, bb);
        if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
            ForAvailReachDef(bbid, preds, &lst, bsmgr);
        }
        if (flag.have(DUOPT_SOL_REACH_DEF)) {
            ForReachDef(bbid, preds, &lst, bsmgr);
        }
        if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
            ForAvailExpression(bbid, preds, &lst, bsmgr);
        }
        i++;
    } while (lst.get_elem_count() != 0);
    ASSERT0(i < count);
    DUMMYUSE(count);
}


//Solve reaching definitions problem for IR STMT and
//computing LIVE IN and LIVE OUT IR expressions.
//'expr_univers': the Universal SET for ExpRep.
void SolveSet::solve(DefDBitSetCore const& expr_univers, UFlag const flag,
                     MOD DefMiscBitSetMgr & bsmgr)
{
    START_TIMER(t7, "Solve DU set");
    BBList * bbl = m_rg->getBBList();
    IRBB const* entry = m_cfg->getEntry();
    ASSERT0(entry && BB_is_entry(entry));
    for (IRBB * bb = bbl->get_tail(); bb != nullptr; bb = bbl->get_prev()) {
        UINT bbid = bb->id();
        if (flag.have(DUOPT_SOL_REACH_DEF)) {
            //Initialize reach-def IN, reach-def OUT.
            genInReachDef(bbid)->clean(*getGlobalSBSMgr());
            genOutReachDef(bbid)->clean(*getLocalSBSMgr());
        }

        if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
            genAvailInReachDef(bbid)->clean(bsmgr);
        }

        if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
            //Initialize available in, available out expression.
            //IN-SET of BB must be universal of all IR-expressions.
            DefDBitSetCore * availin = genAvailInExpr(bbid);
            DefDBitSetCore * availout = genAvailOutExpr(bbid);
            if (bbid == entry->id()) {
                //AvailIn and AvailOut of entry should be empty.
                availin->clean(bsmgr);
                availout->clean(bsmgr);
            } else {
                availin->copy(expr_univers, bsmgr);
                availout->copy(*availin, bsmgr);
                DefDBitSetCore const* set = getKilledIRExpr(bbid);
                if (set != nullptr) {
                    availout->diff(*set, bsmgr);
                }
                availout->bunion(*genGenIRExpr(bbid), bsmgr);
            }
        }
    }

    //Rpo already checked to be available. Here double check again.
    RPOVexList * vexlst = m_cfg->getRPOVexList();
    ASSERT0(vexlst);
    ASSERT0(vexlst->get_elem_count() == bbl->get_elem_count());
    #ifdef WORK_LIST_DRIVE
    solveByWorkList(vexlst, flag, bsmgr);
    #else
    solveByRPO(vexlst, flag, bsmgr);
    #endif
    END_TIMER(t7, "Solve DU set");
}


DefDBitSetCore * SolveSet::getMustGenDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_must_gen_def.get(bbid);
}


DefDBitSetCore * SolveSet::getAvailInReachDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_avail_in_reach_def.get(bbid);
}


DefDBitSetCore * SolveSet::getAvailOutReachDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_avail_out_reach_def.get(bbid);
}


DefSBitSetCore * SolveSet::getLiveInBB(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_livein_bb.get(bbid);
}


DefDBitSetCore * SolveSet::getInReachDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_in_reach_def.get(bbid);
}


DefDBitSetCore * SolveSet::getOutReachDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_out_reach_def.get(bbid);
}


//Return IR stmt-id set.
DefDBitSetCore * SolveSet::genMayGenDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_may_gen_def.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_may_gen_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * SolveSet::genMustGenDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_must_gen_def.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_must_gen_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * SolveSet::genAvailInReachDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_avail_in_reach_def.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_avail_in_reach_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * SolveSet::genAvailOutReachDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_avail_out_reach_def.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_avail_out_reach_def.set(bbid, set);
    }
    return set;
}


DefSBitSetCore * SolveSet::genLiveInBB(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefSBitSetCore * set = m_livein_bb.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocSBitSetCore();
        m_livein_bb.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * SolveSet::genInReachDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_in_reach_def.get(bbid);
    if (set == nullptr) {
        set = getGlobalSBSMgr()->allocDBitSetCore();
        //set->set_sparse(SOL_SET_IS_SPARSE);
        m_in_reach_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * SolveSet::genOutReachDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_out_reach_def.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_out_reach_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore const* SolveSet::getMustKilledDef(UINT bbid) const
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_must_killed_def.get(bbid);
}


void SolveSet::setMustKilledDef(UINT bbid, DefDBitSetCore * set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_must_killed_def.set(bbid, set);
}


DefDBitSetCore const* SolveSet::getMayKilledDef(UINT bbid) const
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_may_killed_def.get(bbid);
}


void SolveSet::setMayKilledDef(UINT bbid, DefDBitSetCore * set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_may_killed_def.set(bbid, set);
}


//Return IR expression-id set.
DefDBitSetCore * SolveSet::getGenIRExpr(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_gen_ir_exp.get(bbid);
}


//Return IR expression-id set.
DefDBitSetCore * SolveSet::genGenIRExpr(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_gen_ir_exp.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_gen_ir_exp.set(bbid, set);
    }
    return set;
}


//Return IR expression-id set.
DefDBitSetCore const* SolveSet::getKilledIRExpr(UINT bbid) const
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_killed_ir_exp.get(bbid);
}


//Return IR expression-id set.
void SolveSet::setKilledIRExpr(UINT bbid, DefDBitSetCore * set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_killed_ir_exp.set(bbid, set);
}


//Return livein set for IR expression. Each element in the set is IR id.
DefDBitSetCore * SolveSet::getAvailInExpr(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_avail_in_exp.get(bbid);
}


//Return liveout set for IR expression. Each element in the set is IR id.
DefDBitSetCore * SolveSet::getAvailOutExpr(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_avail_out_exp.get(bbid);
}


//Return livein set for IR expression. Each element in the set is IR id.
DefDBitSetCore * SolveSet::genAvailInExpr(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_avail_in_exp.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_avail_in_exp.set(bbid, set);
    }
    return set;
}


//Return liveout set for IR expression. Each element in the set is IR id.
DefDBitSetCore * SolveSet::genAvailOutExpr(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_avail_out_exp.get(bbid);
    if (set == nullptr) {
        set = getLocalSBSMgr()->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_avail_out_exp.set(bbid, set);
    }
    return set;
}


//NOTE: MD referrence must be available.
//mustdefs: record must modified MD for each bb.
//maydefs: record may modified MD for each bb.
//mayuse: record may used MD for each bb.
//        collect mayuse (NOPR-DU) to compute Region referred MD.
void SolveSet::computeMustExactDefMayDefMayUse(OUT Vector<MDSet*> * mustdefmds,
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

    BBList * bbl = m_rg->getBBList();
    BBListIter ct;
    ConstMDIter mditer;
    xcom::DefMiscBitSetMgr * lsbsmgr = getLocalSBSMgr();
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        DefDBitSetCore * maygen_stmt = nullptr;
        DefDBitSetCore * mustgen_stmt = nullptr;
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
            //compute mustgen_stmt when computing available-reach-def.
            //May be both available-reach-def, reach-def, and available-exp
            //need mustgen_stmt information.
            //Consider case:
            //   BB1:p1 = 1               |
            //   /             \          |
            //BB2:p1 = 2  |  BB3:p1 = 3   |
            //   \            /           |
            //   BB4: ... = p1            |
            //where BB1 is precessor of BB2 and BB3.
            //BB1:p1 should not reach-def at BB4.
            bb_mustdefmds = mustdefmds->get(bbid);
            mustgen_stmt = genMustGenDef(bbid);
            mustgen_stmt->clean(*lsbsmgr);
        }

        if (maydefmds != nullptr) {
            bb_maydefmds = maydefmds->get(bbid);
            maygen_stmt = genMayGenDef(bbid);
            maygen_stmt->clean(*lsbsmgr);
        }

        computeMustExactDefMayDefMayUseForBB(bb, mditer,
                                             mustdefmds, maydefmds,
                                             mayusemds, bb_mustdefmds,
                                             bb_maydefmds, mustgen_stmt,
                                             maygen_stmt, flag, *lsbsmgr);
    }
    END_TIMER_FMT(t3, ("Build MustDef, MayDef, MayUse: %s",
                       getSolveFlagName(flag)));
}


void SolveSet::computeMustExactDefMayDefMayUseForBB(
    IRBB * bb, ConstMDIter & mditer,
    OUT Vector<MDSet*> * mustdefmds, OUT Vector<MDSet*> * maydefmds,
    OUT MDSet * mayusemds, MDSet * bb_mustdefmds, MDSet * bb_maydefmds,
    DefDBitSetCore * mustgen_stmt, DefDBitSetCore * maygen_stmt,
    UFlag flag, DefMiscBitSetMgr & bsmgr)
{
    //may_def_mds, must_def_mds should be already clean.
    IRListIter irct;
    for (BB_irlist(bb).get_head(&irct);
         irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
        IR const* ir = irct->val();
        ASSERT0(ir);
        if (mayusemds != nullptr) {
            m_du->collectMayUseRecursive(ir, *mayusemds,
                                         flag.have(DUOPT_COMPUTE_PR_DU), bsmgr);
            //collectMayUse(ir, *mayusemds, isComputePRDU());
        }

        if (!ir->hasResult()) { continue; }

        //Do not compute MustExactDef/MayDef for PR.
        if (ir->isWritePR() && !flag.have(DUOPT_COMPUTE_PR_DU)) {
            continue;
        }

        //Collect mustdef mds.
        if (bb_mustdefmds != nullptr) {
            //For now, only the computation of available-reach-def need
            //MustGenStmt information. It is very slowly when compiling large
            //region.
            //UPDATE: It is not arrurate or even correct if we only
            //compute mustgen_stmt when computing available-reach-def.
            //May be both available-reach-def, reach-def, and available-exp
            //need mustgen_stmt information.
            //Consider case:
            //   BB1:p1 = 1              |
            //   /            \          |
            //BB2:p1 = 2  |  BB3:p1 = 3  |
            //   \            /          |
            //   BB4: ... = p1           |
            //where BB1 is precessor of BB2 and BB3.
            //BB1:p1 should not reach-def at BB4.
            //ASSERT0(flag.have(DUOPT_SOL_AVAIL_REACH_DEF));

            ASSERT0(mustgen_stmt);
            computeMustExactDef(ir, bb_mustdefmds, mustgen_stmt, mditer,
                                bsmgr, flag);
        }

        if (bb_maydefmds != nullptr) {
            computeMayDef(ir, bb_maydefmds, maygen_stmt, bsmgr, flag);
        }
    }
}


void SolveSet::collectNonPRMayDef(IR const* ir, DefMiscBitSetMgr & bsmgr,
                                  OUT MDSet * maydefmds) const
                          
{
    if (ir->isWritePR()) {
        //Note call-stmt does not belong WritePR.
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


void SolveSet::computeMayDef(IR const* ir, MDSet * bb_maydefmds,
                             DefDBitSetCore * maygen_stmt,
                             DefMiscBitSetMgr & bsmgr, UFlag flag)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
        if (flag.have(DUOPT_SOL_REGION_REF)) {
            collectNonPRMayDef(ir, bsmgr, bb_maydefmds);
        }
        if (!flag.have(DUOPT_COMPUTE_NONPR_DU)) { return; }
        break;
    case IR_CALL:
    case IR_ICALL:
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
        ASSERT0(!ir->isMemoryRef());
    }

    //Computing May GEN set of reach-definition.
    //The computation of reach-definition problem is conservative.
    //If we can not say whether a DEF is killed, regard it as lived STMT.
    DefSBitSetIter st = nullptr;
    BSIdx ni;
    for (BSIdx i = maygen_stmt->get_first(&st); i != BS_UNDEF; i = ni) {
        ni = maygen_stmt->get_next(i, &st);
        IR * gened_ir = m_rg->getIR(i);
        ASSERT0(gened_ir != nullptr && gened_ir->is_stmt());
        if (isMustKill(ir, gened_ir)) {
            maygen_stmt->diff(i, bsmgr);
        }
    }
    maygen_stmt->bunion(ir->id(), bsmgr);
}


//Return true if 'def1' may modify md-set that 'def2' generated.
//'def1': should be stmt.
//'def2': should be stmt.
bool SolveSet::isMayKill(IR const* def1, IR const* def2)
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
        if (mds2 != nullptr && (mds1 == mds2 || mds2->is_contain(md1))) {
            return true;
        }
        return false;
    }

    if (mds1 != nullptr) {
        if (md2 != nullptr && mds1->is_contain(md2)) {
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
bool SolveSet::isMustKill(IR const* def1, IR const* def2)
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


void SolveSet::computeMustExactDef(IR const* ir,
                                   OUT MDSet * bb_mustdefmds,
                                   DefDBitSetCore * mustgen_stmt,
                                   ConstMDIter & mditer,
                                   DefMiscBitSetMgr & bsmgr,
                                   UFlag flag)
{
    switch (ir->getCode()) {
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
        if (!flag.have(DUOPT_COMPUTE_NONPR_DU)) { return; }
        break;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_CALL:
    case IR_ICALL:
    case IR_PHI:
        if (!flag.have(DUOPT_COMPUTE_PR_DU)) { return; }
        break;
    case IR_REGION:
        //Region does not have any def.
        break;
    default: //Handle general stmt.
        ASSERT0(!ir->isMemoryRef());
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
    for (BSIdx i = mustgen_stmt->get_first(&st); i != BS_UNDEF; i = ni) {
        ni = mustgen_stmt->get_next(i, &st);
        IR * gened_ir = m_rg->getIR(i);
        ASSERT0(gened_ir != nullptr && gened_ir->is_stmt());
        if (isMayKill(ir, gened_ir)) {
            mustgen_stmt->diff(i, bsmgr);
        }
    }
    mustgen_stmt->bunion(ir->id(), bsmgr);
}


void SolveSet::computeLiveInBB(DefMiscBitSetMgr & bsmgr)
{
    START_TIMER(t4, "Compute LiveInBB");
    bool change = true;
    RPOVexList const* vexlst = m_cfg->getRPOVexList();
    ASSERT0(vexlst);
    DefSBitSetCore tmp;
    UINT count = 0;
    while (change && count < 20) {
        change = false;
        RPOVexListIter it;
        for (vexlst->get_head(&it); it != vexlst->end();
             it = vexlst->get_next(it)) {
            IRBB const* bb = m_cfg->getBB(it->val()->id());
            DefSBitSetCore * bs = genLiveInBB(bb->id());
            tmp.clean(bsmgr);
            for (xcom::EdgeC * el = bb->getVex()->getInList();
                 el != nullptr; el = el->get_next()) {
                IRBB const* pred = m_cfg->getBB(el->getFromId());
                if (pred == nullptr) { continue; }
                tmp.bunion(*genLiveInBB(pred->id()), bsmgr);
                tmp.bunion(pred->id(), bsmgr);
            }
            if (bs->is_equal(tmp)) { continue; }
            bs->copy(tmp, bsmgr);
            change = true;
        }
        count++;
    }
    tmp.clean(bsmgr);
    ASSERT0(!change);
    END_TIMER(t4, "Compute LiveInBB");
}


//Compute must and may killed stmt.
//mustdefs: record must modified MD for each bb.
//maydefs: record may modified MD for each bb.
//NOTE: computation of maykill and mustkill both need may-gen-def.
void SolveSet::computeKillSet(DefDBitSetCoreReserveTab & dbitsetchash,
                              Vector<MDSet*> const* mustexactdefmds,
                              Vector<MDSet*> const* maydefmds,
                              DefMiscBitSetMgr & bsmgr)
{
    START_TIMER(t4, "Build KillSet");
    ASSERT0(mustexactdefmds || maydefmds);
    computeLiveInBB(bsmgr);

    BBList * bbl = m_rg->getBBList();
    BBListIter ct;
    DefDBitSetCore must_killed_set(SOL_SET_IS_SPARSE);
    DefDBitSetCore may_killed_set(SOL_SET_IS_SPARSE);
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        UINT bbid = bb->id();
        bool comp_must = false;
        MDSet const* bb_mustdef_mds = nullptr;
        if (mustexactdefmds != nullptr) {
            bb_mustdef_mds = mustexactdefmds->get(bbid);
            if (bb_mustdef_mds != nullptr && !bb_mustdef_mds->is_empty()) {
                comp_must = true;
            }
        }

        bool comp_may = false;
        MDSet const* bb_maydef_mds = nullptr;
        if (maydefmds != nullptr) {
            //Compute may killed stmts.
            bb_maydef_mds = maydefmds->get(bbid);
            if (bb_maydef_mds != nullptr && !bb_maydef_mds->is_empty()) {
                comp_may = true;
            }
        }

        if (!comp_must && !comp_may) {
            setMustKilledDef(bbid, nullptr);
            setMayKilledDef(bbid, nullptr);
            continue;
        }

        DefSBitSetCore const* livein_bbs = genLiveInBB(bbid);
        ASSERT0(livein_bbs);
        DefSBitSetIter st = nullptr;
        for (BSIdx b = livein_bbs->get_first(&st);
             b != BS_UNDEF; b = livein_bbs->get_next(b, &st)) {
            if (b == (BSIdx)bbid) { continue; }
            DefDBitSetCore const* livein_maygendef = genMayGenDef(
                m_cfg->getBB(b)->id());
            ASSERT0(livein_maygendef);
            for (BSIdx i = livein_maygendef->get_first(&st);
                 i != BS_UNDEF; i = livein_maygendef->get_next(i, &st)) {
                IR const* stmt = m_rg->getIR(i);
                ASSERT0(stmt->is_stmt());
                if (comp_must) {
                    MD const* stmt_mustexactdef_md = stmt->getExactRef();
                    if (stmt_mustexactdef_md == nullptr) { continue; }
                    if (bb_mustdef_mds->is_contain(stmt_mustexactdef_md)) {
                        must_killed_set.bunion(i, bsmgr);
                    }
                }

                if (comp_may) {
                    //Compute may killed stmts, for avail-reach-def.
                    MD const* stmt_effectdef_md = stmt->getMustRef();
                    if (stmt_effectdef_md != nullptr &&
                        bb_mustdef_mds->is_contain(stmt_effectdef_md)) {
                        may_killed_set.bunion(i, bsmgr);
                        continue;
                    }

                    MDSet const* maydef = const_cast<IR*>(stmt)->getMayRef();
                    if (maydef == nullptr) { continue; }
                    if (bb_maydef_mds->is_intersect(*maydef)) {
                        may_killed_set.bunion(i, bsmgr);
                    }
                }
            } //end for livein_maygendef
        } //end for each livein

        setMustKilledDef(bbid,
            comp_must ? dbitsetchash.append(must_killed_set) : nullptr);
        setMayKilledDef(bbid,
            comp_may ? dbitsetchash.append(may_killed_set) : nullptr);
        must_killed_set.clean(bsmgr);
        may_killed_set.clean(bsmgr);
    }
    ASSERT0(must_killed_set.is_empty() && may_killed_set.is_empty());
    resetLiveInBB();
    END_TIMER(t4, "Build KillSet");
}


//Return true if ir can be candidate of live-expr.
static bool canBeLiveExprCand(IR const* ir)
{
    ASSERT0(ir);
    switch (ir->getCode()) {
    SWITCH_CASE_BIN:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_SELECT:
    case IR_PR:
        return true;
    default: break;
    }
    return false;
}


//Compute generated-EXPR for BB.
void SolveSet::computeGenForBB(IRBB * bb,
                               OUT DefDBitSetCore & expr_univers,
                               DefMiscBitSetMgr & bsmgr)
{
    MDSet tmp;
    DefDBitSetCore * gen_ir_exprs = genGenIRExpr(bb->id());
    gen_ir_exprs->clean(bsmgr);

    IRListIter ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end(); ct = BB_irlist(bb).get_next(ct)) {
        IR const* ir = ct->val();
        ASSERT0(ir->is_stmt());
        switch (ir->getCode()) {
        case IR_ST:
            if (canBeLiveExprCand(ST_rhs(ir))) {
                //Compute the generated expressions set.
                gen_ir_exprs->bunion(IR_id(ST_rhs(ir)), bsmgr);
                expr_univers.bunion(IR_id(ST_rhs(ir)), bsmgr);
            }
            //Fall through.
        case IR_STPR:
            if (ir->is_stpr() && canBeLiveExprCand(STPR_rhs(ir))) {
                //Compute the generated expressions set.
                gen_ir_exprs->bunion(IR_id(STPR_rhs(ir)), bsmgr);
                expr_univers.bunion(IR_id(STPR_rhs(ir)), bsmgr);
            }
            //Fall through.
        case IR_STARRAY:
            if (ir->is_starray() && canBeLiveExprCand(STARR_rhs(ir))) {
                //Compute the generated expressions set.
                gen_ir_exprs->bunion(IR_id(STARR_rhs(ir)), bsmgr);
                expr_univers.bunion(IR_id(STARR_rhs(ir)), bsmgr);
            }
            //Fall through.
        case IR_IST:
            if (ir->is_ist()) {
                //Compute the generated expressions set.
                if (canBeLiveExprCand(IST_rhs(ir))) {
                    gen_ir_exprs->bunion(IR_id(IST_rhs(ir)), bsmgr);
                    expr_univers.bunion(IR_id(IST_rhs(ir)), bsmgr);
                }

                if (canBeLiveExprCand(IST_base(ir))) {
                    //e.g: *(int*)0x1000 = 10, IST_base(ir) is nullptr.
                    gen_ir_exprs->bunion(IR_id(IST_base(ir)), bsmgr);
                    expr_univers.bunion(IR_id(IST_base(ir)), bsmgr);
                }
            }

            {
                //Compute lived IR expression after current statement executed.
                //e.g:
                //  i = i + 1 //S1
                //
                //  lhs 'i' killed the rhs expression: 'i + 1', that means
                //  'i + 1' is dead after S1 statement.
                MDSet const* maydef = const_cast<IR*>(ir)->getMayRef();
                MD const* mustdef = ir->getMustRef();
                if (maydef != nullptr || mustdef != nullptr) {
                    DefSBitSetIter st = nullptr;
                    for (BSIdx j = gen_ir_exprs->get_first(&st), nj;
                         j != BS_UNDEF; j = nj) {
                        nj = gen_ir_exprs->get_next(j, &st);

                        IR * tir = m_rg->getIR(j);
                        ASSERT0(tir != nullptr);

                        if (tir->is_lda() || tir->is_const()) {
                            continue;
                        }

                        tmp.clean(bsmgr);

                        m_du->collectMayUseRecursive(tir, tmp, true, bsmgr);
                        //collectMayUse(tir, tmp, true);

                        if ((maydef != nullptr &&
                             maydef->is_intersect(tmp)) ||
                            (mustdef != nullptr &&
                             tmp.is_contain(mustdef))) {
                            //'ir' killed 'tir'.
                            gen_ir_exprs->diff(j, bsmgr);
                        }
                    }
                }
            }
            break;
        case IR_CALL:
        case IR_ICALL: {
            //Compute the generated expressions set.
            if (ir->is_icall()) {
                ASSERT0(ICALL_callee(ir)->is_ld());
                if (canBeLiveExprCand(ICALL_callee(ir))) {
                    gen_ir_exprs->bunion(IR_id(ICALL_callee(ir)), bsmgr);
                    expr_univers.bunion(IR_id(ICALL_callee(ir)), bsmgr);
                }
            }

            for (IR * p = CALL_param_list(ir);
                 p != nullptr; p = p->get_next()) {
                if (canBeLiveExprCand(p)) {
                    gen_ir_exprs->bunion(IR_id(p), bsmgr);
                    expr_univers.bunion(IR_id(p), bsmgr);
                }
            }

            //Compute lived IR expression after current statement executed.
            //e.g:
            //  i = i + 1 //S1
            //
            //  lhs 'i' killed the rhs expression: 'i + 1', that means
            //  'i + 1' is dead after S1 statement.
            MDSet const* maydef = const_cast<IR*>(ir)->getMayRef();
            MD const* mustdef = ir->getMustRef();
            if (maydef != nullptr || mustdef != nullptr) {
                DefSBitSetIter st = nullptr;
                for (BSIdx j = gen_ir_exprs->get_first(&st), nj;
                     j != BS_UNDEF; j = nj) {
                    nj = gen_ir_exprs->get_next(j, &st);
                    IR * tir = m_rg->getIR(j);
                    ASSERT0(tir != nullptr);
                    if (tir->is_lda() || tir->is_const()) {
                        continue;
                    }

                    tmp.clean(bsmgr);

                    m_du->collectMayUseRecursive(tir, tmp, true, bsmgr);
                    //collectMayUse(tir, tmp, true);

                    if ((maydef != nullptr && maydef->is_intersect(tmp)) ||
                        (mustdef != nullptr && tmp.is_contain(mustdef))) {
                        //'ir' killed 'tir'.
                        gen_ir_exprs->diff(j, bsmgr);
                    }
                }
            }
            break;
        }
        case IR_REGION:
        case IR_GOTO:
            break;
        case IR_IGOTO:
            //Compute the generated expressions.
            if (canBeLiveExprCand(IGOTO_vexp(ir))) {
                gen_ir_exprs->bunion(IR_id(IGOTO_vexp(ir)), bsmgr);
                expr_univers.bunion(IR_id(IGOTO_vexp(ir)), bsmgr);
            }
            break;
        case IR_DO_WHILE:
        case IR_WHILE_DO:
        case IR_DO_LOOP:
        case IR_IF:
        case IR_LABEL:
        case IR_CASE:
            ASSERTN(0, ("TODO"));
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            //Compute the generated expressions.
            if (canBeLiveExprCand(BR_det(ir))) {
                gen_ir_exprs->bunion(IR_id(BR_det(ir)), bsmgr);
                expr_univers.bunion(IR_id(BR_det(ir)), bsmgr);
            }
            break;
        case IR_SWITCH:
            //Compute the generated expressions.
            if (canBeLiveExprCand(SWITCH_vexp(ir))) {
                gen_ir_exprs->bunion(IR_id(SWITCH_vexp(ir)), bsmgr);
                expr_univers.bunion(IR_id(SWITCH_vexp(ir)), bsmgr);
            }
            break;
        case IR_RETURN:
            if (RET_exp(ir) != nullptr) {
                if (canBeLiveExprCand(RET_exp(ir))) {
                    gen_ir_exprs->bunion(IR_id(RET_exp(ir)), bsmgr);
                    expr_univers.bunion(IR_id(RET_exp(ir)), bsmgr);
                }
            }
            break;
        case IR_PHI:
            //Since phis are always at head of BB, no live-expr killed by them.
            for (IR * p = PHI_opnd_list(ir); p != nullptr; p = p->get_next()) {
                if (canBeLiveExprCand(p)) {
                    gen_ir_exprs->bunion(IR_id(p), bsmgr);
                    expr_univers.bunion(IR_id(p), bsmgr);
                }
            }
            break;
        default: UNREACHABLE();
        }
    }
    tmp.clean(bsmgr);
}


//Compute local-gen IR-EXPR set and killed IR-EXPR set.
//'expr_universe': record the universal of all ir-expr of region.
void SolveSet::computeAuxSetForExpression(DefDBitSetCoreReserveTab & bshash,
                                          OUT DefDBitSetCore * expr_universe,
                                          Vector<MDSet*> const* maydefmds,
                                          DefMiscBitSetMgr & bsmgr)
{
    START_TIMER(t5, "Build AvailableExp");
    ASSERT0(expr_universe && maydefmds);
    BBList * bbl = m_rg->getBBList();
    BBListIter ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        computeGenForBB(bb, *expr_universe, bsmgr);
    }

    //Compute kill-set.
    //The defined MDSet of current ir, killed all
    //other exprs which used MDSet that modified by 'ir'.
    DefDBitSetCore killed_set(SOL_SET_IS_SPARSE);
    MDSet tmp;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        MDSet const* bb_maydef = maydefmds->get(bb->id());
        ASSERT0(bb_maydef != nullptr);

        DefSBitSetIter st = nullptr;
        for (BSIdx i = expr_universe->get_first(&st);
             i != BS_UNDEF; i = expr_universe->get_next(i, &st)) {
            IR * ir = m_rg->getIR(i);
            ASSERT0(ir->is_exp());
            if (ir->is_lda() || ir->is_const()) { continue; }

            tmp.clean(bsmgr);
            m_du->collectMayUseRecursive(ir, tmp, true, bsmgr);
            //collectMayUse(ir, *tmp, true);

            if (bb_maydef->is_intersect(tmp)) {
                killed_set.bunion(i, bsmgr);
            }
        }

        setKilledIRExpr(bb->id(), bshash.append(killed_set));
        killed_set.clean(bsmgr);
    }

    tmp.clean(bsmgr);
    END_TIMER(t5, "Build AvailableExp");
}


//Compute maydef, mustdef, mayuse information for current region.
void SolveSet::computeRegionMDDU(Vector<MDSet*> const* mustexactdef_mds,
                                 Vector<MDSet*> const* maydef_mds,
                                 MDSet const* mayuse_mds)
{
    START_TIMER(t6, "Build Region DefUse MDSet");
    ASSERT0(mustexactdef_mds && maydef_mds && mayuse_mds);
    m_rg->initRefInfo();

    //Note the SBSMgr of region MayDef and MayUse should use Region's
    //SBSMgr. The MayDef and MayUse set will be freed at Region's destructor.
    xcom::DefMiscBitSetMgr * sbsmgr = m_rg->getMiscBitSetMgr();

    MDSet * ru_maydef = m_rg->genMayDef();
    ASSERT0(ru_maydef);
    ru_maydef->clean(*sbsmgr);

    MDSet * ru_mayuse = m_rg->genMayUse();
    ASSERT0(ru_mayuse);
    ru_mayuse->clean(*sbsmgr);

    BBList * bbl = m_rg->getBBList();
    BBListIter ct = nullptr;
    VarTab const* vtab = m_rg->getVarTab();
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
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
                ru_maydef->bunion(md, *sbsmgr);
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
                ru_maydef->bunion(md, *sbsmgr);
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
            ru_mayuse->bunion(md, *sbsmgr);
        }
    }

    REGION_is_ref_valid(m_rg) = true;
    END_TIMER(t6, "Build Region DefUse MDSet");
}


//is_bs: true to dump bitset info.
void SolveSet::dump(bool is_bs) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n\n==---- DUMP DUMgr SET '%s' ----==\n",
         m_rg->getRegionName());
    BBList * bbl = m_rg->getBBList();
    BBListIter cb;
    SolveSet * pthis = const_cast<SolveSet*>(this);
    FILE * file = m_rg->getLogMgr()->getFileHandler();
    ASSERT0(file);
    for (IRBB * bb = bbl->get_head(&cb); bb != nullptr;
         bb = bbl->get_next(&cb)) {
        UINT bbid = bb->id();
        note(getRegion(), "\n---- BB%d ----", bbid);
        DefDBitSetCore * def_in = pthis->getInReachDef(bbid);
        DefDBitSetCore * def_out = pthis->getOutReachDef(bbid);
        DefDBitSetCore * avail_def_in = pthis->getAvailInReachDef(bbid);
        DefDBitSetCore * avail_def_out = pthis->getAvailOutReachDef(bbid);
        DefDBitSetCore * must_def_gen = pthis->getMustGenDef(bbid);
        DefDBitSetCore * gen_ir = pthis->getGenIRExpr(bbid);
        DefDBitSetCore * livein_ir = pthis->getAvailInExpr(bbid);
        DefDBitSetCore * liveout_ir = pthis->getAvailOutExpr(bbid);
        DefDBitSetCore const* must_killed = pthis->getMustKilledDef(bbid);
        DefDBitSetCore const* may_def_kill = pthis->getMayKilledDef(bbid);
        DefDBitSetCore const* killed_exp = pthis->getKilledIRExpr(bbid);
        DefSBitSetCore * livein_bb = pthis->genLiveInBB(bbid);
        DefSBitSetIter st = nullptr;
        if (def_in != nullptr) {
            note(getRegion(), "\nREACH-DEF IN STMT: %lu byte ",
                 (ULONG)def_in->count_mem());
            for (BSIdx i = def_in->get_first(&st);
                 i != BS_UNDEF; i = def_in->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                def_in->dump(file);
            }
        }

        if (def_out != nullptr) {
            note(getRegion(), "\nREACH-DEF OUT STMT: %lu byte ",
                 (ULONG)def_out->count_mem());
            for (BSIdx i = def_out->get_first(&st);
                 i != BS_UNDEF; i = def_out->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                def_out->dump(file);
            }
        }

        if (avail_def_in != nullptr) {
            note(getRegion(), "\nAVAIL-REACH-DEF IN STMT: %lu byte ",
                 (ULONG)avail_def_in->count_mem());
            for (BSIdx i = avail_def_in->get_first(&st);
                 i != BS_UNDEF; i = avail_def_in->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                avail_def_in->dump(file);
            }
        }

        if (avail_def_out != nullptr) {
            note(getRegion(), "\nAVAIL-REACH-DEF OUT STMT: %lu byte ",
                 (ULONG)avail_def_out->count_mem());
            for (BSIdx i = avail_def_out->get_first(&st);
                 i != BS_UNDEF; i = avail_def_out->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                avail_def_out->dump(file);
            }
        }

        if (must_def_gen != nullptr) {
            note(getRegion(), "\nMUST GEN STMT: %lu byte ",
                 (ULONG)must_def_gen->count_mem());
            for (BSIdx i = must_def_gen->get_first(&st);
                 i != BS_UNDEF; i = must_def_gen->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                must_def_gen->dump(file);
            }
        }

        if (must_killed != nullptr) {
            note(getRegion(), "\nMUST KILLED STMT: %lu byte ",
                 (ULONG)must_killed->count_mem());
            for (BSIdx i = must_killed->get_first(&st);
                 i != BS_UNDEF; i = must_killed->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                must_killed->dump(file);
            }
        }

        if (may_def_kill != nullptr) {
            note(getRegion(), "\nMAY KILLED STMT: %lu byte ",
                 (ULONG)may_def_kill->count_mem());
            for (BSIdx i = may_def_kill->get_first(&st);
                 i != BS_UNDEF; i = may_def_kill->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                may_def_kill->dump(file);
            }
        }

        if (livein_ir != nullptr) {
            note(getRegion(), "\nLIVE IN EXPR: %lu byte ",
                 (ULONG)livein_ir->count_mem());
            for (BSIdx i = livein_ir->get_first(&st);
                 i != BS_UNDEF; i = livein_ir->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                livein_ir->dump(file);
            }
        }

        if (liveout_ir != nullptr) {
            note(getRegion(), "\nLIVE OUT EXPR: %lu byte ",
                 (ULONG)liveout_ir->count_mem());
            for (BSIdx i = liveout_ir->get_first(&st);
                 i != BS_UNDEF; i = liveout_ir->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                liveout_ir->dump(file);
            }
        }

        if (gen_ir != nullptr) {
            note(getRegion(), "\nGEN EXPR: %lu byte ",
                 (ULONG)gen_ir->count_mem());
            for (BSIdx i = gen_ir->get_first(&st);
                 i != BS_UNDEF; i = gen_ir->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                gen_ir->dump(file);
            }
        }

        if (killed_exp != nullptr) {
            note(getRegion(), "\nKILLED EXPR: %lu byte ",
                 (ULONG)killed_exp->count_mem());
            for (BSIdx i = killed_exp->get_first(&st);
                 i != BS_UNDEF; i = killed_exp->get_next(i, &st)) {
                IR * ir = m_rg->getIR(i);
                ASSERT0(ir != nullptr);
                prt(getRegion(), "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                note(getRegion(), "\n             ");
                killed_exp->dump(file);
            }
        }

        if (livein_bb != nullptr) {
            note(getRegion(), "\nLIVE IN BB: %lu byte ",
                 (ULONG)livein_bb->count_mem());
            livein_bb->dump(file);
        }
    }
}


//Dump mem usage for each internal set of bb.
void SolveSet::dumpMemUsage() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(),
         "\n==---- DUMP '%s' DUMgr : Memory Usage for Value Set ----==",
         m_rg->getRegionName());

    BBList * bbs = m_rg->getBBList();
    size_t count = 0;
    CHAR const* str = nullptr;
    for (IRBB * bb = bbs->get_head(); bb != nullptr; bb = bbs->get_next()) {
        note(getRegion(), "\n--- BB%d ---", bb->id());
        size_t n;
        DefSBitSetIter st = nullptr;
        DefDBitSetCore * irs = m_avail_in_reach_def.get(bb->id());
        if (irs != nullptr) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tAvaInReachDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_avail_out_reach_def.get(bb->id());
        if (irs != nullptr) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tAvaOutReachDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_in_reach_def.get(bb->id());
        if (irs != nullptr) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tInReachDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_out_reach_def.get(bb->id());
        if (irs != nullptr) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tOutReachDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_may_gen_def.get(bb->id());
        if (irs != nullptr) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tMayGenDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_must_gen_def.get(bb->id());
        if (irs != nullptr) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tMustGenDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        DefDBitSetCore const* dbs = m_may_killed_def.get(bb->id());
        if (dbs != nullptr) {
            n = dbs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tMayKilledDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, dbs->get_elem_count(), dbs->get_last(&st));
        }

        dbs = m_must_killed_def.get(bb->id());
        if (dbs != nullptr) {
            n = dbs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tMustKilledDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, dbs->get_elem_count(), dbs->get_last(&st));
        }

        DefDBitSetCore * bs = m_gen_ir_exp.get(bb->id());
        if (bs != nullptr) {
            n = bs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tMayKilledDef:%lu%s, %d elems, last %d",
                 (ULONG)n, str, bs->get_elem_count(), bs->get_last(&st));
        }

        dbs = m_killed_ir_exp.get(bb->id());
        if (dbs != nullptr) {
            n = dbs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tKilledIrExp:%lu%s, %d elems, last %d",
                 (ULONG)n, str, dbs->get_elem_count(), dbs->get_last(&st));
        }

        bs = m_avail_in_exp.get(bb->id());
        if (bs != nullptr) {
            n = bs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tLiveInIrExp:%lu%s, %d elems, last %d",
                 (ULONG)n, str, bs->get_elem_count(), bs->get_last(&st));
        }

        bs = m_avail_out_exp.get(bb->id());
        if (bs != nullptr) {
            n = bs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            note(getRegion(), "\n\tLiveOutIrExp:%lu%s, %d elems, last %d",
                 (ULONG)n, str, bs->get_elem_count(), bs->get_last(&st));
        }
    }

    if (count < 1024) { str = "B"; }
    else if (count < 1024 * 1024) { count /= 1024; str = "KB"; }
    else { count /= 1024*1024; str = "MB"; }
    note(getRegion(), "\nTotal %u%s", (UINT)count, str);
}


//Count up the memory has been allocated.
size_t SolveSet::count_mem() const
{
    Vector<DefDBitSetCore*> const* ptr;
    size_t count = 0;

    count += m_avail_in_reach_def.count_mem();
    ptr = &m_avail_in_reach_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_avail_out_reach_def.count_mem();
    ptr = &m_avail_out_reach_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_in_reach_def.count_mem();
    ptr = &m_in_reach_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_out_reach_def.count_mem();
    ptr = &m_out_reach_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_may_gen_def.count_mem();
    ptr = &m_may_gen_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_must_gen_def.count_mem();
    ptr = &m_must_gen_def;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_must_killed_def.count_mem();
    for (VecIdx i = 0; i <= m_must_killed_def.get_last_idx(); i++) {
        DefDBitSetCore const* dset = m_must_killed_def.get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_may_killed_def.count_mem();
    for (VecIdx i = 0; i <= m_must_killed_def.get_last_idx(); i++) {
        DefDBitSetCore const* dset = m_must_killed_def.get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_gen_ir_exp.count_mem();
    ptr = &m_gen_ir_exp;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_killed_ir_exp.count_mem();
    for (VecIdx i = 0; i <= m_killed_ir_exp.get_last_idx(); i++) {
        DefDBitSetCore const* dset = m_killed_ir_exp.get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_avail_in_exp.count_mem();
    ptr = &m_avail_in_exp;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    count += m_avail_out_exp.count_mem();
    ptr = &m_avail_out_exp;
    for (VecIdx i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != nullptr) {
            count += dset->count_mem();
        }
    }

    return count;
}


//Return true if region status changed.
bool SolveSet::perform(MOD OptCtx & oc, UFlag flag)
{
    ASSERT0(oc.is_ref_valid());
    BBList * bbl = m_rg->getBBList();
    m_du = m_rg->getDUMgr();
    m_cfg = m_rg->getCFG();

    Vector<MDSet*> * maydef_mds = nullptr;
    Vector<MDSet*> * mustexactdef_mds = nullptr;
    MDSet * mayuse_mds = nullptr;

    if (flag.have(DUOPT_SOL_REGION_REF)) {
        mayuse_mds = new MDSet();
    }

    MDSet * mds_arr_for_must = nullptr;
    MDSet * mds_arr_for_may = nullptr;

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
        mds_arr_for_must = new MDSet[bbl->get_elem_count()]();
    }
    if (maydef_mds != nullptr) {
        mds_arr_for_may = new MDSet[bbl->get_elem_count()]();
    }
    UINT i = 0;
    for (IRBB * bb = bbl->get_tail();
         bb != nullptr; bb = bbl->get_prev(), i++) {
        if (mustexactdef_mds != nullptr) {
            mustexactdef_mds->set(bb->id(), &mds_arr_for_must[i]);
        }
        if (maydef_mds != nullptr) {
            maydef_mds->set(bb->id(), &mds_arr_for_may[i]);
        }
    }
    END_TIMER(t2, "Allocate May/Must MDS table");

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

    DefDBitSetCore expr_univers(SOL_SET_IS_SPARSE);
    if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
        //Compute GEN, KILL IR-EXPR.
        computeAuxSetForExpression(*dbitsetchash, &expr_univers, maydef_mds,
                                   *lbsmgr);
    }
    delete dbitsetchash; //destroy useless resource as soon as possible.
    dbitsetchash = nullptr; //destroy useless resource as soon as possible.
    resetKillSet();

    if (flag.have(DUOPT_SOL_REGION_REF)) {
        //Compute DEF,USE mds for Region.
        computeRegionMDDU(mustexactdef_mds, maydef_mds, mayuse_mds);
    }

    if (flag.have(DUOPT_SOL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_REACH_DEF) ||
        flag.have(DUOPT_SOL_AVAIL_EXPR)) {
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
        solve(expr_univers, flag, *lbsmgr);
    }

    //Free and destroy local used resource.
    expr_univers.clean(*lbsmgr);
    if (mustexactdef_mds != nullptr) {
        ASSERT0(mds_arr_for_must);
        for (UINT i2 = 0; i2 < bbl->get_elem_count(); i2++) {
            mds_arr_for_must[i2].clean(*lbsmgr);
        }
        delete [] mds_arr_for_must;
        delete mustexactdef_mds;
        mustexactdef_mds = nullptr;
    }

    if (maydef_mds != nullptr) {
        ASSERT0(mds_arr_for_may);
        for (UINT i2 = 0; i2 < bbl->get_elem_count(); i2++) {
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

    resetLocalSet();

    //Set opt-context variables.
    if (flag.have(DUOPT_SOL_AVAIL_REACH_DEF)) {
        OC_is_avail_reach_def_valid(oc) = true;
    }
    if (flag.have(DUOPT_SOL_REACH_DEF)) {
        OC_is_reach_def_valid(oc) = true;
    }
    if (flag.have(DUOPT_SOL_AVAIL_EXPR)) {
        OC_is_live_expr_valid(oc) = true;
    }
    return false;
}

} //namespace xoc
