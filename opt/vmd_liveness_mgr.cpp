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

//
//START VMDLivenessMgr
//
VMDLivenessMgr::VMDLivenessMgr(Region * rg) : LivenessMgr(rg)
{
    m_need_reach_def = false;
    m_mdssamgr = nullptr;
    m_mdssaudmgr = nullptr;
}


bool VMDLivenessMgr::useMDSSADU() const
{
    return m_mdssamgr != nullptr && m_mdssamgr->is_valid();
}


void VMDLivenessMgr::computeGlobal(IRCFG const* cfg)
{
    computeGlobalLive(cfg);
    if (!needReachDef()) { return; }
    computeKillReach(cfg);
    computeGlobalReach(cfg);
}


static void collectDefMDSet(
    LiveSet const* def, OUT MDSet & mdset, VMDLivenessMgr * vmdlivemgr)
{
    LiveSetIter vit;
    MDSSAMgr * mdssamgr = vmdlivemgr->getMDSSAMgr();
    DefMiscBitSetMgr * sbsmgr = vmdlivemgr->getSBSMgr();
    for (BSIdx i = def->get_first(&vit);
         i != BS_UNDEF; i = def->get_next(i, &vit)) {
        VMD const* vmd = mdssamgr->getVMD(i);
        ASSERT0(vmd);
        ASSERT0(vmd->is_md());
        mdset.bunion(vmd->mdid(), *sbsmgr);
    }
}


void VMDLivenessMgr::computeKillReach(IRCFG const* cfg)
{
    BBListIter it;
    LiveSet universe_def_set;
    for (IRBB const* bb = cfg->getBBList()->get_head(&it);
         bb != nullptr; bb = cfg->getBBList()->get_next(&it)) {
        LiveSet const* def = get_def(bb->id());
        if (def == nullptr) { continue; }
        universe_def_set.bunion(*def, m_sbs_mgr);
    }
    for (IRBB const* bb = cfg->getBBList()->get_head(&it);
         bb != nullptr; bb = cfg->getBBList()->get_next(&it)) {
        LiveSet const* def = get_def(bb->id());
        if (def == nullptr) { continue; }
        LiveSet * kill = gen_kill(bb->id());
        LiveSetIter vit;
        for (BSIdx i = def->get_first(&vit);
             i != BS_UNDEF; i = def->get_next(i, &vit)) {
            VMD const* defvmd = m_mdssamgr->getVMD(i);
            ASSERT0(defvmd);
            ASSERT0(defvmd->is_md());
            MDIdx defvmdmdid = defvmd->mdid();
            LiveSetIter vit2;
            for (BSIdx u = universe_def_set.get_first(&vit2);
                 u != BS_UNDEF; u = universe_def_set.get_next(u, &vit2)) {
                VMD const* uvmd = m_mdssamgr->getVMD(u);
                if (def->is_contain(uvmd->id())) {
                    //VMD is in bb.
                    continue;
                }
                if (uvmd->mdid() == defvmdmdid) {
                    kill->bunion(u, m_sbs_mgr);
                }
            }
        }
    }
    universe_def_set.clean(m_sbs_mgr);
}


void VMDLivenessMgr::computeGlobalReach(IRCFG const* cfg)
{
    ASSERT0(cfg->getEntry() && BB_is_entry(cfg->getEntry()));
    //RPO should be available.
    RPOVexList const* vlst = const_cast<IRCFG*>(cfg)->getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == cfg->getBBList()->get_elem_count());
    bool change;
    UINT count = 0;
    UINT thres = 1000;
    LiveSet news;
    do {
        change = false;
        RPOVexListIter ct2;
        for (vlst->get_head(&ct2);
             ct2 != vlst->end(); ct2 = vlst->get_next(ct2)) {
            IRBB const* bb = cfg->getBB(ct2->val()->id());
            ASSERT0(bb);
            UINT bbid = bb->id();
            LiveSet * reach_in = get_livein_reach(bbid);
            AdjVertexIter it;
            Vertex const* o = Graph::get_first_in_vertex(bb->getVex(), it);
            if (o != nullptr) {
                ASSERT0(get_liveout_reach(o->id()));
                news.copy(*get_liveout_reach(o->id()), m_sbs_mgr);
                o = Graph::get_next_in_vertex(it);
                for (; o != nullptr; o = Graph::get_next_in_vertex(it)) {
                    ASSERTN(get_liveout_reach(o->id()), ("BB miss liveness"));
                    news.bunion(*get_liveout_reach(o->id()), m_sbs_mgr);
                }
                if (!reach_in->is_equal(news)) {
                    reach_in->copy(news, m_sbs_mgr);
                    change = true;
                }
            }
            //Compute reach_out by reach_in.
            LiveSet const* livein = get_livein(bbid);
            LiveSet const* kill = get_kill(bbid);
            LiveSet const* def = get_def(bbid);
            news.copy(*reach_in, m_sbs_mgr);
            if (def != nullptr) {
                news.bunion(*def, m_sbs_mgr);
            }
            //ASSERT0(livein);
            //news.diff(*livein, m_sbs_mgr);
            if (kill != nullptr) {
                news.diff(*kill, m_sbs_mgr);
            }
            get_liveout_reach(bbid)->copy(news, m_sbs_mgr);
        }
        count++;
    } while (change && count < thres);
    ASSERTN(!change, ("result of equation is convergent slowly"));
    news.clean(m_sbs_mgr);
}


void VMDLivenessMgr::cleanReachSet()
{
    for (VecIdx i = 0; i <= m_livein_reach.get_last_idx(); i++) {
        LiveSet * bs = m_livein_reach.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_livein_reach.reinit();

    for (VecIdx i = 0; i <= m_liveout_reach.get_last_idx(); i++) {
        LiveSet * bs = m_liveout_reach.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_liveout_reach.reinit();

    for (VecIdx i = 0; i <= m_kill.get_last_idx(); i++) {
        LiveSet * bs = m_kill.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_kill.reinit();
}


void VMDLivenessMgr::computeGlobalLive(IRCFG const* cfg)
{
    ASSERT0(cfg->getEntry() && BB_is_entry(cfg->getEntry()));
    //RPO should be available.
    RPOVexList const* vlst = const_cast<IRCFG*>(cfg)->getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == cfg->getBBList()->get_elem_count());
    bool change;
    UINT count = 0;
    UINT thres = 1000;
    LiveSet news;
    do {
        change = false;
        RPOVexListIter ct2;
        for (vlst->get_tail(&ct2);
             ct2 != vlst->end(); ct2 = vlst->get_prev(ct2)) {
            IRBB const* bb = cfg->getBB(ct2->val()->id());
            ASSERT0(bb);
            UINT bbid = bb->id();
            LiveSet * out = get_liveout(bbid);
            AdjVertexIter ito;
            Vertex const* o = Graph::get_first_out_vertex(bb->getVex(), ito);
            if (o != nullptr) {
                ASSERT0(get_livein(o->id()));
                news.copy(*get_livein(o->id()), m_sbs_mgr);
                o = Graph::get_next_out_vertex(ito);
                for (; o != nullptr; o = Graph::get_next_out_vertex(ito)) {
                    ASSERTN(get_livein(o->id()), ("BB miss liveness"));
                    news.bunion(*get_livein(o->id()), m_sbs_mgr);
                }
                if (!out->is_equal(news)) {
                    out->copy(news, m_sbs_mgr);
                    change = true;
                }
            }
            //Compute in by out.
            LiveSet const* use = get_use(bbid);
            LiveSet const* def = get_def(bbid);
            news.copy(*out, m_sbs_mgr);
            if (def != nullptr) {
                news.diff(*def, m_sbs_mgr);
            }
            if (use != nullptr) {
                news.bunion(*use, m_sbs_mgr);
            }
            get_livein(bbid)->copy(news, m_sbs_mgr);
        }
        count++;
    } while (change && count < thres);
    mergeMDPhiOpndToLiveIn();

    //Check whether there are redundant livein and liveout info in entry_bb.
    //'livein(entry) - use(entry) = NULL' means that each element in livein
    //of entry_bb is useful. And it doesn't need to be removed.
    news.copy(*get_livein(cfg->getEntry()->id()), m_sbs_mgr);
    news.diff(*get_use(cfg->getEntry()->id()), m_sbs_mgr);
    if (!news.is_empty()) {
        //Eliminate redundant liveness.
        eliminateRedundantLiveness(cfg);
    }

    ASSERTN(!change, ("result of equation is convergent slowly"));
    news.clean(m_sbs_mgr);
}


LiveSet * VMDLivenessMgr::gen_kill(UINT bbid)
{
    LiveSet * x = m_kill.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_kill.set(bbid, x);
    }
    return x;
}


LiveSet * VMDLivenessMgr::gen_liveout_reach(UINT bbid)
{
    LiveSet * x = m_liveout_reach.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_liveout_reach.set(bbid, x);
    }
    return x;
}


LiveSet * VMDLivenessMgr::gen_livein_reach(UINT bbid)
{
    LiveSet * x = m_livein_reach.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_livein_reach.set(bbid, x);
    }
    return x;
}


void VMDLivenessMgr::updateSetByRHS(BSIdx idx, MOD LiveSet * use)
{
    ASSERT0(use);
    ASSERT0(idx != BS_UNDEF);
    use->bunion(idx, *getSBSMgr());
}


void VMDLivenessMgr::updateSetByLHS(
    BSIdx idx, MOD LiveSet * use, MOD LiveSet * gen)
{
    ASSERT0(gen && use);
    ASSERT0(idx != BS_UNDEF);
    gen->bunion(idx, *getSBSMgr());
    use->diff(idx, *getSBSMgr());
}


void VMDLivenessMgr::computeExpImpl(
    IR const* exp, MOD LiveSet * use, MOD LiveSet * gen)
{
    ASSERT0(exp->is_exp());
    MDSSAInfo const* info = m_mdssamgr->getMDSSAInfoIfAny(exp);
    if (info == nullptr || info->isEmptyVOpndSet()) { return; }
    VOpndSet const& set = info->readVOpndSet();
    VOpndSetIter vit = nullptr;
    for (BSIdx i = set.get_first(&vit);
         i != BS_UNDEF; i = set.get_next(i, &vit)) {
        VMD const* vmd = (VMD*)m_mdssaudmgr->getVOpnd(i);
        ASSERT0(vmd);
        if (!vmd->is_md()) { continue; }
        updateSetByRHS((BSIdx)vmd->id(), use);
    }
}


void VMDLivenessMgr::computeMDPhiOpndList(MDPhi const* mdphi, MOD LiveSet * use)
{
    ASSERT0(mdphi);
    for (IR const* opnd = MDPHI_opnd_list(mdphi);
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD const* opndvmd = ((MDPhi*)mdphi)->getOpndVMD(opnd, m_mdssaudmgr);
        ASSERT0(opndvmd);
        updateSetByRHS((BSIdx)opndvmd->id(), use);
    }
}


void VMDLivenessMgr::mergeMDPhiOpndList(
    MDPhi const* mdphi, MOD LiveSet * livein)
{
    ASSERT0(livein);
    ASSERT0(mdphi && mdphi->is_phi());
    for (IR const* opnd = MDPHI_opnd_list(mdphi);
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD const* opndvmd = ((MDPhi*)mdphi)->getOpndVMD(opnd, m_mdssaudmgr);
        ASSERT0(opndvmd);
        livein->bunion((BSIdx)opndvmd->id(), *getSBSMgr());
    }
}


void VMDLivenessMgr::computeMDPhi(
    MDPhi const* mdphi, MOD LiveSet * use, MOD LiveSet * gen)
{
    ASSERT0(mdphi->is_phi());
    VMD const* res = mdphi->getResult();
    ASSERT0(res && res->is_md());
    updateSetByLHS((BSIdx)res->id(), use, gen);

    //CASE: Do NOT record MDPhi opnd's VMD as liveness-USE.
    //Add them into liveness-USE set when global iteration finished.
    //Becase in actually, the MDDef of phi opnd should not participate in
    //dataflow solving.
    //computeMDPhiOpndList(mdphi, use);
}


void VMDLivenessMgr::mergeMDPhiOpndToLiveIn()
{
    BBList const* bbl = m_rg->getBBList();
    BBListIter it;
    for (IRBB const* bb = bbl->get_head(&it);
         bb != nullptr; bb = bbl->get_next(&it)) {
        MDPhiList * philist = m_mdssamgr->getPhiList(bb->id());
        if (philist == nullptr) { continue; }
        LiveSet * livein = get_livein(bb->id());
        ASSERT0(livein);
        for (MDPhiListIter it = philist->get_head();
             it != philist->end(); it = philist->get_next(it)) {
            MDPhi * phi = it->val();
            ASSERT0(phi);
            mergeMDPhiOpndList(phi, livein);
        }
    }
}


void VMDLivenessMgr::updateSetByLHSAndConsiderVersion(
    VMD const* lhsvmd, MOD LiveSet * use, MOD LiveSet * gen)
{
    updateSetByLHS((BSIdx)lhsvmd->id(), use, gen);
    MDIdx lhsvmd_mdid = lhsvmd->mdid();
    LiveSetIter vit;
    for (BSIdx i = use->get_first(&vit);
         i != BS_UNDEF; i = use->get_next(i, &vit)) {
        VMD const* t = m_mdssamgr->getVMD(i);
        ASSERT0(t);
        ASSERT0(t->is_md());
        if (t->mdid() != lhsvmd_mdid) { continue; }
        updateSetByLHS((BSIdx)t->id(), use, gen);
    }
}


void VMDLivenessMgr::computeStmtImpl(
    IR const* stmt, MOD LiveSet * use, MOD LiveSet * gen)
{
    ASSERT0(stmt->is_stmt());
    MDSSAInfo const* info = m_mdssamgr->getMDSSAInfoIfAny(stmt);
    if (info == nullptr || info->isEmptyVOpndSet()) { return; }
    VOpndSet const& set = info->readVOpndSet();
    VOpndSetIter vit = nullptr;
    for (BSIdx i = set.get_first(&vit);
         i != BS_UNDEF; i = set.get_next(i, &vit)) {
        VMD const* vmd = (VMD*)m_mdssaudmgr->getVOpnd(i);
        ASSERT0(vmd && vmd->is_md());
        updateSetByLHSAndConsiderVersion(vmd, use, gen);
    }
}


void VMDLivenessMgr::dumpVMDLivenessSet(
    VMDLivenessSet const* set, bool detail) const
{
    ASSERT0(set);
    Region const* rg = getRegion();
    FILE * file = rg->getLogMgr()->getFileHandler();
    set->dump(file);
    if (!detail) { return; }
    xcom::StrBuf tmp(8);
    VMDLivenessSetIter vit = nullptr;
    rg->getLogMgr()->incIndent(2);
    for (BSIdx i = set->get_first(&vit);
         i != BS_UNDEF; i = set->get_next(i, &vit)) {
        VMD const* vmd = m_mdssamgr->getVMD(i);
        ASSERT0(vmd);
        ASSERT0(vmd->is_md());
        note(rg, "\nVMD%u:MD%uV%u", vmd->id(), vmd->mdid(), vmd->version());
        MDDef const* mddef = vmd->getDef();
        if (mddef == nullptr) { prt(rg, ":NOMDDEF"); continue; }
        if (mddef->is_phi()) {
            prt(rg, ":DEF:(mdphi%u,BB%u)", mddef->id(), mddef->getBB()->id());
            continue;
        }
        IR const* occ = mddef->getOcc();
        ASSERT0(occ);
        prt(rg, ":DEF:(%s,BB%u)",
            xoc::dumpIRName(occ, tmp), occ->getBB()->id());
    }
    rg->getLogMgr()->decIndent(2);
}


void VMDLivenessMgr::dumpAllSet() const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    Region const* rg = getRegion();
    note(rg, "\n-- VMDLivenessMgr : liveness of each BB --\n");
    List<IRBB*> * bbl = rg->getBBList();
    bool detail = true;
    rg->getLogMgr()->incIndent(2);
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(rg, "\n-- BB%d --", bb->id());
        LiveSet * live_in = get_livein(bb->id());
        LiveSet * live_out = get_liveout(bb->id());
        LiveSet * def = get_def(bb->id());
        LiveSet * use = get_use(bb->id());
        LiveSet * kill = get_kill(bb->id());
        LiveSet * livein_reach = get_livein_reach(bb->id());
        LiveSet * liveout_reach = get_liveout_reach(bb->id());
        note(rg, "\nLIVE-IN: ");
        if (live_in != nullptr) {
            dumpVMDLivenessSet((VMDLivenessSet const*)live_in, detail);
        }

        note(rg, "\nLIVE-OUT: ");
        if (live_out != nullptr) {
            dumpVMDLivenessSet((VMDLivenessSet const*)live_out, detail);
        }

        note(rg, "\nDEF: ");
        if (def != nullptr) {
            dumpVMDLivenessSet((VMDLivenessSet const*)def, detail);
        }

        note(rg, "\nUSE: ");
        if (use != nullptr) {
            dumpVMDLivenessSet((VMDLivenessSet const*)use, detail);
        }

        note(rg, "\nKILL: ");
        if (kill != nullptr) {
            dumpVMDLivenessSet((VMDLivenessSet const*)kill, detail);
        }

        note(rg, "\nLIVE-IN-REACH: ");
        if (livein_reach != nullptr) {
            dumpVMDLivenessSet((VMDLivenessSet const*)livein_reach, detail);
        }

        note(rg, "\nLIVE-OUT-REACH: ");
        if (liveout_reach != nullptr) {
            dumpVMDLivenessSet((VMDLivenessSet const*)liveout_reach, detail);
        }
    }
    rg->getLogMgr()->decIndent(2);
}


void VMDLivenessMgr::computeLocalIterMDPhiList(
    IRBB const* bb, LiveSet * use, LiveSet * gen)
{
    MDPhiList * philist = m_mdssamgr->getPhiList(bb->id());
    if (philist == nullptr) { return; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi);
        computeMDPhi(phi, use, gen);
    }
}


void VMDLivenessMgr::initSet(BBList const& bblst)
{
    BBListIter it;
    for (bblst.get_head(&it); it != bblst.end(); it = bblst.get_next(it)) {
        IRBB * bb = it->val();
        ASSERT0(bb);
        gen_livein_reach(bb->id())->clean(m_sbs_mgr);
        gen_liveout_reach(bb->id())->clean(m_sbs_mgr);
    }
    LivenessMgr::initSet(bblst);
}


void VMDLivenessMgr::computeLocal(IRBB const* bb)
{
    LiveSet * use = gen_use(bb->id());
    LiveSet * gen = gen_def(bb->id());
    use->clean(m_sbs_mgr);
    gen->clean(m_sbs_mgr);
    computeLocalIterBBIRList(bb, use, gen);
    computeLocalIterMDPhiList(bb, use, gen);
}


bool VMDLivenessMgr::dump() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpPass(PASS_VMDLIVENESS_MGR))
    { return true; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    dumpAllSet();
    m_rg->getLogMgr()->decIndent(2);
    return true;
}


bool VMDLivenessMgr::initDepPass()
{
    m_mdssamgr = m_rg->getMDSSAMgr();
    if (!useMDSSADU()) { return false; }
    m_mdssaudmgr = m_mdssamgr->getUseDefMgr();
    return true;
}


bool VMDLivenessMgr::perform(OptCtx & oc)
{
    if (!initDepPass()) { return false; }
    START_TIMER(t, getPassName());
    bool changed = LivenessMgr::perform(oc);
    END_TIMER(t, getPassName());
    return changed;
}
//END VMDLivenessMgr

} //namespace xoc
