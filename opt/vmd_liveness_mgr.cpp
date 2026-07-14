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
    m_mdssamgr = nullptr;
    m_mdssaudmgr = nullptr;
}


bool VMDLivenessMgr::useMDSSADU() const
{
    return m_mdssamgr != nullptr && m_mdssamgr->is_valid();
}


//hack
void VMDLivenessMgr::computeGlobal(IRCFG const* cfg)
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
            LiveSet const* use = get_use(bbid);


            //hack start
            if (use != nullptr) {
                out->bunion(*use, m_sbs_mgr);
            }
            //hack end



            //Compute in by out.
            news.copy(*out, m_sbs_mgr);
            LiveSet const* def = get_def(bbid);
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


LiveSet * VMDLivenessMgr::gen_livethrough(UINT bbid)
{
    LiveSet * x = m_livethrough.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_livethrough.set(bbid, x);
    }
    return x;
}


void VMDLivenessMgr::updateSetByRHS(
    BSIdx idx, MOD LiveSet * use)
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


void VMDLivenessMgr::computeMDPhiOpndList(
    MDPhi const* mdphi, MOD LiveSet * use)
{
    ASSERT0(mdphi);
    for (IR const* opnd = MDPHI_opnd_list(mdphi);
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * opndvmd = ((MDPhi*)mdphi)->getOpndVMD(opnd, m_mdssaudmgr);
        ASSERT0(opndvmd);
        updateSetByRHS((BSIdx)opndvmd->id(), use);
    }
}


void VMDLivenessMgr::computeMDPhi(
    MDPhi const* mdphi, MOD LiveSet * use, MOD LiveSet * gen)
{
    ASSERT0(mdphi->is_phi());
    VMD const* res = mdphi->getResult();
    ASSERT0(res && res->is_md());
    updateSetByLHS((BSIdx)res->id(), use, gen);
    computeMDPhiOpndList(mdphi, use);
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
        updateSetByLHS((BSIdx)vmd->id(), use, gen);
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
            prt(rg, ":DEF:(mdphi,BB%u)", mddef->getBB()->id());
            continue;
        }
        IR const* occ = mddef->getOcc();
        ASSERT0(occ);
        prt(rg, ":DEF:(%s)", xoc::dumpIRName(occ, tmp));
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


void VMDLivenessMgr::computeLocal(IRBB const* bb)
{
    LiveSet * use = gen_use(bb->id());
    LiveSet * gen = gen_def(bb->id());
    LiveSet * livethrough = gen_livethrough(bb->id());
    use->clean(m_sbs_mgr);
    gen->clean(m_sbs_mgr);
    livethrough->clean(m_sbs_mgr);
    computeLocalIterMDPhiList(bb, use, gen);
    computeLocalIterBBIRList(bb, use, gen);
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
