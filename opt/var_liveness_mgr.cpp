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
#include "linear_scan.h"
#include "var_liveness_mgr.h"

namespace xoc {

bool VarCheck::m_is_vec_only = true;

static void dumpRangeVec(VarRangeVec const& rv, Region const* rg)
{
    for (VecIdx i = 0; i < (VecIdx)rv.get_elem_count(); i++) {
        VarRange const& r = rv.get(i);
        r.dump(rg);
    }
}


static void computeLHS(IR * ir, VarLifeTimeMgr & mgr, Pos pos)
{
    ASSERT0(ir && ir->is_stmt());
    if (!mgr.getLSRA().isSpillOp(ir)) { return; }
    Var const* v = ir->getIdinfo();
    ASSERT0(v);
    if (VarCheck::checkVarType(v)) { return; }
    VarLifeTime * var_lt = mgr.genLifeTime(v->id());
    var_lt->addRange(pos);
}


static void computeRHS(IR * ir, VarLifeTimeMgr & mgr, Pos pos,
                       Pos livein_def, IRIter & irit)
{
    ASSERT0(ir && ir->is_stmt());
    if (!mgr.getLSRA().isReloadOp(ir)) { return; }
    Var const* v = ir->getRHS()->getIdinfo();
    ASSERT0(v);
    if (VarCheck::checkVarType(v)) { return; }
    VarLifeTime * var_lt = mgr.genLifeTime(v->id());
    VarRange r = var_lt->getLastRange();
    if (r.start() == POS_UNDEF) {
        //PR is region livein.
        VRG_start(r) = livein_def;
        VRG_end(r) = pos;
    } else {
        VRG_end(r) = pos;
    }
    var_lt->setLastRange(r);
}


static void computeLHSAccu(IR * ir, VarLifeTimeMgr & mgr, Pos pos)
{
    ASSERT0(ir && ir->is_stmt());
    if (!mgr.getLSRA().isSpillOp(ir)) { return; }
    Var const* v = ir->getIdinfo();
    ASSERT0(v);
    if (VarCheck::checkVarType(v)) { return; }
    VarLifeTime * var_lt = mgr.genLifeTime(v->id());
    var_lt->addAccuRange(pos);
}


static void computeRHSAccu(IR * ir, VarLifeTimeMgr & mgr, Pos pos,
                           Pos livein_def, IRIter & irit)
{
    ASSERT0(ir && ir->is_stmt());
    if (!mgr.getLSRA().isReloadOp(ir)) { return; }
    Var const* v = ir->getRHS()->getIdinfo();
    ASSERT0(v);
    if (VarCheck::checkVarType(v)) { return; }
    VarLifeTime * var_lt = mgr.genLifeTime(v->id());
    VarRange r = var_lt->getLastAccuRange();
    if (r.start() == POS_UNDEF) {
        //PR is region livein.
        VRG_start(r) = livein_def;
        VRG_end(r) = pos;
    } else {
        VRG_end(r) = pos;
    }
    var_lt->setLastAccuRange(r);
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
//START VarLivenessMgr
//
bool VarLivenessMgr::dump() const
{
    if (!m_rg->isLogMgrInit()) { return true; }
    note(m_rg, "\n==---- DUMP VarLivenessMgr : liveness of Var ----==\n");
    List<IRBB*> * bbl = m_rg->getBBList();
    FILE * file = getRegion()->getLogMgr()->getFileHandler();
    if (file == nullptr) { return false; }
    getRegion()->getLogMgr()->incIndent(2);
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(getRegion(), "\n-- BB%d --", bb->id());
        VarLiveSet * live_in = get_livein(bb->id());
        VarLiveSet * live_out = get_liveout(bb->id());
        VarLiveSet * def = get_def(bb->id());
        VarLiveSet * use = get_use(bb->id());
        note(getRegion(), "\nLIVE-IN: ");
        if (live_in != nullptr) {
            live_in->dump(file);
        }

        note(getRegion(), "\nLIVE-OUT: ");
        if (live_out != nullptr) {
            live_out->dump(file);
        }

        note(getRegion(), "\nDEF: ");
        if (def != nullptr) {
            def->dump(file);
        }

        note(getRegion(), "\nUSE: ");
        if (use != nullptr) {
            use->dump(file);
        }
    }
    getRegion()->getLogMgr()->decIndent(2);
    return true;
}


void VarLivenessMgr::cleanGlobal()
{
    for (VecIdx i = 0; i <= m_livein.get_last_idx(); i++) {
        VarLiveSet * bs = m_livein.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_livein.reinit();
    for (VecIdx i = 0; i <= m_liveout.get_last_idx(); i++) {
        VarLiveSet * bs = m_liveout.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_liveout.reinit();
}


void VarLivenessMgr::cleanLocal()
{
    for (VecIdx i = 0; i <= m_def.get_last_idx(); i++) {
        VarLiveSet * bs = m_def.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_def.reinit();
    for (VecIdx i = 0; i <= m_use.get_last_idx(); i++) {
        VarLiveSet * bs = m_use.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_use.reinit();
}


void VarLivenessMgr::computeStmt(IR const* stmt, MOD VarLiveSet * use,
                                 MOD VarLiveSet * gen)
{
    ASSERT0(stmt && use && gen);
    ASSERT0(stmt->is_stmt());
    if (m_lsra.isSpillOp(stmt)) {
        Var * v = stmt->getIdinfo();
        if (VarCheck::checkVarType(v)) { return; }
        gen->bunion((BSIdx)v->id(), m_sbs_mgr);
        use->diff((BSIdx)v->id(), m_sbs_mgr);
        return;
    }

    if (m_lsra.isReloadOp(stmt)) {
        Var * v = stmt->getRHS()->getIdinfo();
        if (VarCheck::checkVarType(v)) { return; }
        use->bunion((BSIdx)v->id(), m_sbs_mgr);
    }
}


void VarLivenessMgr::computeLocal(BBList const& bblst)
{
    BBListIter it;
    for (bblst.get_head(&it); it != bblst.end(); it = bblst.get_next(it)) {
        IRBB const* bb = it->val();
        ASSERT0(bb);
        computeLocal(bb);
    }
}


void VarLivenessMgr::computeLocal(IRBB const* bb)
{
    VarLiveSet * use = gen_use(bb->id());
    VarLiveSet * gen = gen_def(bb->id());
    use->clean(m_sbs_mgr);
    gen->clean(m_sbs_mgr);
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter irit;
    for (IR * x = irlst.get_tail(&irit);
         x != nullptr; x = irlst.get_prev(&irit)) {
        computeStmt(x, use, gen);
    }
}


void VarLivenessMgr::computeGlobal(IRCFG const* cfg)
{
    ASSERT0(cfg->getEntry() && BB_is_entry(cfg->getEntry()));
    //RPO should be available.
    RPOVexList const* vlst = const_cast<IRCFG*>(cfg)->getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == cfg->getBBList()->get_elem_count());
    bool change;
    UINT count = 0;
    UINT thres = 1000;
    VarLiveSet news;
    do {
        change = false;
        RPOVexListIter ct2;
        for (vlst->get_tail(&ct2);
             ct2 != vlst->end(); ct2 = vlst->get_prev(ct2)) {
            IRBB const* bb = cfg->getBB(ct2->val()->id());
            ASSERT0(bb);
            UINT bbid = bb->id();
            VarLiveSet * out = get_liveout(bbid);
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
            news.copy(*out, m_sbs_mgr);
            VarLiveSet const* def = get_def(bbid);
            if (def != nullptr) {
                news.diff(*def, m_sbs_mgr);
            }
            VarLiveSet const* use = get_use(bbid);
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
        //Eliminate redundant liveness in entry bb.
        eliminateRedundantLivenessInEntryBB(cfg);
    }
    ASSERTN(!change, ("result of equation is convergent slowly"));
    news.clean(m_sbs_mgr);
}


VarLiveSet * VarLivenessMgr::gen_def(UINT bbid)
{
    VarLiveSet * x = m_def.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_def.set(bbid, x);
    }
    return x;
}


VarLiveSet * VarLivenessMgr::gen_use(UINT bbid)
{
    VarLiveSet * x = m_use.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_use.set(bbid, x);
    }
    return x;
}


VarLiveSet * VarLivenessMgr::gen_livein(UINT bbid)
{
    VarLiveSet * x = m_livein.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_livein.set(bbid, x);
    }
    return x;
}


VarLiveSet * VarLivenessMgr::gen_liveout(UINT bbid)
{
    VarLiveSet * x = m_liveout.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_liveout.set(bbid, x);
    }
    return x;
}


void VarLivenessMgr::init_livein(UINT bbid)
{
    VarLiveSet const* use = get_use(bbid);
    if (use != nullptr) {
        gen_livein(bbid)->copy(*use, m_sbs_mgr);
    } else {
        gen_livein(bbid)->clean(m_sbs_mgr);
    }
}


void VarLivenessMgr::initSet(BBList const& bblst)
{
    BBListIter it;
    for (bblst.get_head(&it); it != bblst.end(); it = bblst.get_next(it)) {
        IRBB * bb = it->val();
        ASSERT0(bb);
        init_livein(bb->id());
        gen_liveout(bb->id())->clean(m_sbs_mgr);
    }
}


void VarLivenessMgr::eliminateRedundantLivenessInEntryBB(IRCFG const* cfg)
{
    //There is a problem that entry_bb will be attached redundant livein and
    //liveout info after completed liveness computing via the function of
    //'computeGlobal'. The main reason for this phenomenon is that there is a
    //(are) loop edge(s) in the CFG. Both livein and liveout info will be flowed
    //back into entry_bb along with the loop edge. Thus it needs to remove
    //redundant livein and liveout info from entry_bb and it's successor node
    //until occurs a node with more than one indegree.
    //
    //livein and liveout info after completed 'computeGlobal' function:
    //                                 |-----------------------------
    //                                 v                            |
    // |----------|  |---------|  |-----------|  |--------  |  |---------|
    // | BB_entry |->|   BB_1  |->|   BB_2    |->|  BB_3    |->|  BB_4   |->
    // |----------|  |---------|  |-----------|  |--------  |  |---------|
    // |gen:      |  |gen:a,b  |  |gen:c      |  |gen:      |  |gen:d    |
    // |use:      |  |use:     |  |use:       |  |use:a     |  |use:c    |
    // |in:d      |  |in:d     |  |in:a,b,d   |  |in:a,b,c,d|  |in:b,c,d |
    // |out:d     |  |out:a,b,d|  |out:a,b,c,d|  |out:b,c,d |  |out:b,d  |
    //
    //livein and liveout info after remove redundant info:
    //                                 |-----------------------------
    //                                 v                            |
    // |----------|  |---------|  |-----------|  |--------  |  |---------|
    // | BB_entry |->|   BB_1  |->|   BB_2    |->|  BB_3    |->|  BB_4   |->
    // |----------|  |---------|  |-----------|  |--------  |  |---------|
    // |gen:      |  |gen:a,b  |  |gen:c      |  |gen:      |  |gen:d    |
    // |use:      |  |use:     |  |use:       |  |use:a     |  |use:c    |
    // |in:       |  |in:      |  |in:a,b,d   |  |in:a,b,c,d|  |in:b,c,d |
    // |out:      |  |out:a,b  |  |out:a,b,c,d|  |out:b,c,d |  |out:b,d  |
    //      ^            ^             ^
    //      |            |             |
    //    remove       remove    stop: in_degree > 1

    //The formula of re-compute livein:
    //
    //livein_new(cur) =
    //  livein_old(cur) - { livein_old(entry) - liveout_new(pre) - use(cur) }
    //
    //    livein_old(cur): livein info that have been computed of current node.
    //    livein_old(entry): livein info that have been computed of entry_bb.
    //    liveout_new(pre): liveout info that have been re-computed of
    //                      predecessor node.
    //    use(cur): be used liveness info in current node.
    //
    //a.The purpose of re-computed liveness is to exclude useless livein info
    //  from old livein. And the useless liveness info has existed in livein of
    //  entry_bb, since entry_bb is final convergence node in liveness computed.
    //  So the livein_old(entry) is full set of useless liveness info in CFG.
    //
    //    livein_new(cur) = livein_old(cur) - 'useless liveness info'
    //      or:
    //    livein_new(cur) = livein_old(cur) - livein_old(entry)
    //
    //b.The useless liveness info of current node is considered as a subset of
    //  livein_old(entry). Since there may be some useful liveness info in
    //  livein_old(entry) for current node. Thus these useful liveness info need
    //  to be excluded from the full set of useless liveness info.
    //
    //    useless liveness info of current node is:
    //                           |
    //                           V
    //    livein_old(entry) - 'useful liveness info for current node'
    //
    //c.The useful liveness info of current node include liveout from it's
    //  predecessor node and liveness info that be used in current node. Liveout
    //  from predecessor node represents these liveness will be used in current
    //  or successor node.
    //
    //    useful liveness info of current node is:
    //      liveout_new(pred) and 'liveness info that be used in current node'
    //        or:
    //      liveout_new(pred) and use(cur)
    //
    //
    //d.Exclude useful liveness info from the full set of useless liveness info
    //  or livein_old(entry) for current node.
    //
    //    useless liveness info of current node is:
    //      livein_old(entry) - liveout_new(pre) - use(cur)
    //
    //e.Thus the formula of re-computed livein is:
    //    livein_old(cur) - { livein_old(entry) - liveout_new(pre) - use(cur) }

    //The formula of re-compute liveout.
    //
    //liveout_new(cur) =
    //  liveout_old(cur) - { livein_old(entry) - liveout_new(pre) - gen(cur) }
    //
    //    gen(cur): liveness info that generated in current node.
    //
    //a.The purpose of re-computed liveness is to exclude useless liveout info
    //  from old liveout. And the useless liveness info has existed in livein of
    //  entry_bb, since entry_bb is final convergence node in liveness computed.
    //  So the Livein_old(entry) is full set of useless liveness info in CFG.
    //
    //    liveout_new(cur) = liveout_old(cur) - 'useless liveness info'
    //      or:
    //    liveout_new(cur) = liveout_old(cur) - livein_old(entry)
    //
    //b.The useless liveness info of current node is considered as a subset of
    //  livein_old(entry). Since there may be some useful liveness info in
    //  livein_old(entry) for current node. Thus these useful liveness info need
    //  to be excluded from the full set of useless liveness info.
    //
    //
    //    useless liveness info of current node is:
    //                           |
    //                           V
    //    livein_old(entry) - 'useful liveness info for current node'
    //
    //c.The useful liveness info of current node include liveout from it's
    //  predecessor node and liveness info that generated in current node.
    //  Liveout from predecessor node represents these liveness will be used
    //  in current or successor node.
    //
    //    useful liveness info of current node is:
    //      liveout_new(pred) and 'liveness info that generated in current node'
    //        or:
    //      liveout_new(pred) and gen(cur)
    //
    //d.Exclude useful livenes info from the full set of useless liveness info
    //  or livein_old(entry) for current node.
    //
    //    useless liveness info of current node is:
    //      livein_old(entry) - liveout_new(pre) - gen(cur)
    //
    //e.Thus the formula of re-computed liveout is:
    //    liveout_old(cur) - { livein_old(entry) - liveout_new(pre) - gen(cur) }

    ASSERT0(cfg->getEntry() && BB_is_entry(cfg->getEntry()));
    //RPO should be available.
    RPOVexList const* vlst = const_cast<IRCFG*>(cfg)->getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == cfg->getBBList()->get_elem_count());

    AdjVertexIter ito;
    PRLiveSet news, tmp_live, entry_livein;
    xcom::List<Vertex const*> vertex_list;
    UINT entry_id = cfg->getEntry()->id();

    //Save livein info of entry_bb.
    entry_livein.copy(*get_livein(entry_id), m_sbs_mgr);
    //Reset livein and liveout of entry_bb.
    get_livein(entry_id)->copy(*get_use(entry_id), m_sbs_mgr);
    get_liveout(entry_id)->copy(*get_def(entry_id), m_sbs_mgr);

    //Iterate all successors node of current node.
    Vertex const* entry_vertex = cfg->Graph::getVertex(entry_id);
    ASSERT0(entry_vertex);
    Vertex const* o = Graph::get_first_out_vertex(entry_vertex, ito);
    for (; o != nullptr; o = Graph::get_next_out_vertex(ito)) {
        if (o->getInDegree() > 1) { continue; }
        vertex_list.append_tail(o);
    }

    //Re-compute the liveness of all successor nodes of entry_bb.
    while (vertex_list.get_elem_count() != 0) {
        Vertex const* v = vertex_list.get_head();
        ASSERT0(v);
        vertex_list.remove_head();
        //Get predecessor node of current node.
        Vertex const* pre_vertex = Graph::get_first_in_vertex(v, ito);
        ASSERT0(pre_vertex && pre_vertex->getInDegree() < 2);

        //Re-computed livein.
        //a.Get the full set useless liveness info(livein_old(entry)).
        tmp_live.copy(entry_livein, m_sbs_mgr);
        //b.Exclude useful liveness info(liveout_new(pre)) from full set.
        tmp_live.diff(*get_liveout(pre_vertex->id()), m_sbs_mgr);
        //c.Exclude useful liveness info(be used in current node) from full set.
        tmp_live.diff(*get_use(v->id()), m_sbs_mgr);
        //d.Get livein_old(cur).
        news.copy(*get_livein(v->id()), m_sbs_mgr);
        //e.Exclude useless liveness info(tmp_live) from livein_old(cur).
        news.diff(tmp_live, m_sbs_mgr);
        //f.Reset.
        get_livein(v->id())->copy(news, m_sbs_mgr);

        //Re-computed liveout.
        //a.Get the full set useless liveness info(livein_old(entry)).
        tmp_live.copy(entry_livein, m_sbs_mgr);
        //b.Exclude useful liveness info(liveout_new(pre)) from full set.
        tmp_live.diff(*get_liveout(pre_vertex->id()), m_sbs_mgr);
        //c.Exclude useful liveness info(generated in current node).
        tmp_live.diff(*get_def(v->id()), m_sbs_mgr);
        //d.Get liveout_old(cur).
        news.copy(*get_liveout(v->id()), m_sbs_mgr);
        //e.Exclude useless liveness info(tmp_live) from liveout_old(cur).
        news.diff(tmp_live, m_sbs_mgr);
        //f.Reset.
        get_liveout(v->id())->copy(news, m_sbs_mgr);

        //Iterate all successor nodes of current node.
        Vertex const* o = Graph::get_first_out_vertex(v, ito);
        for (; o != nullptr; o = Graph::get_next_out_vertex(ito)) {
            if (o->getInDegree() > 1) { continue; }
            vertex_list.append_tail(o);
        }
    }

    news.clean(m_sbs_mgr);
    tmp_live.clean(m_sbs_mgr);
    entry_livein.clean(m_sbs_mgr);
}


bool VarLivenessMgr::perform()
{
    BBList const* bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return false; }

    computeLocal(*bblst);
    initSet(*bblst);
    computeGlobal(m_rg->getCFG());
    return true;
}
//END VarLivenessMgr


//
//START VarRange
//
void VarRange::dump(Region const* rg) const
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
//END VarRange


//
//START VarLifeTime
//
void VarLifeTime::dump(Region const* rg) const
{
    ASSERT0(rg);
    note(rg, "\nLT:%u, VarId%u:\n", m_id, m_var_id);
    prt(rg, "  BB Set:");
    FILE * file = rg->getLogMgr()->getFileHandler();
    if (file == nullptr) { return; }
    m_bb_set->dump(file);
    fprintf(file, "\n  BB range: [");
    VarBBSetIter * iter = nullptr;
    UINT pre_bbid = 1;
    for (UINT bbid = (PRNO)m_bb_set->get_first(&iter);
         bbid != BS_UNDEF; bbid = (PRNO)m_bb_set->get_next(bbid, &iter)) {
        if (bbid - pre_bbid > 1) {
            UINT i = 1;
            while (i < bbid - pre_bbid) {
                fprintf(file, " ");
                i++;
            }
        }
        fprintf(file, "-");
        pre_bbid = bbid;
    }
    fprintf(file, "]\n");
    fprintf(file, "      Lifetime: ");
    dumpRangeVec(m_range_vec, rg);
    fprintf(file, "\n  AccuLifetime: ");
    dumpRangeVec(m_accu_range_vec, rg);
}


bool VarLifeTime::isIntersect(VarRangeVec const* rv1, VarRangeVec const* rv2)
{
    ASSERT0(rv1 && rv2);
    UINT i = 0;
    UINT j = 0;
    UINT rv1n = rv1->get_elem_count();
    UINT rv2n = rv2->get_elem_count();
    VarRange r1 = rv1->get(i);
    VarRange r2 = rv2->get(j);
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


void VarLifeTimeMgr::computeLifeTimeStmt(IR * ir, MOD Pos & pos, Pos livein_pos)
{
    ASSERT0(ir);
    if (getLSRA().isSpillOp(ir)) {
        Var * v = ir->getIdinfo();
        if (VarCheck::checkVarType(v)) { return; }
        VarLifeTime * var_lt = genLifeTime(v->id());
        var_lt->addRange(pos);
        return;
    }

    if (getLSRA().isReloadOp(ir)) {
        Var * v = ir->getRHS()->getIdinfo();
        if (VarCheck::checkVarType(v))  { return; }
        VarLifeTime * var_lt = genLifeTime(v->id());
        ASSERT0(var_lt);
        VarRange r = var_lt->getLastRange();
        if (r.start() == POS_UNDEF) {
            //VAR is region livein.
            VRG_start(r) = livein_pos;
            VRG_end(r) = pos;
        } else {
            VRG_end(r) = pos;
        }
        var_lt->setLastRange(r);
    }
}


void VarLifeTimeMgr::computeLifeTimeBB(UpdatePos & up, IRBB const* bb,
                                       Pos livein_def, IRIter & irit)
{
    ASSERT0(bb);
    VarLiveSet * live_in = m_var_liveness->get_livein(bb->id());
    VarLiveSet * live_out = m_var_liveness->get_liveout(bb->id());
    ASSERT0(live_in && live_out);

    VarLiveSetIter * iter = nullptr;
    for (UINT id = (UINT)live_in->get_first(&iter);
         id != BS_UNDEF; id = (UINT)live_in->get_next(id, &iter)) {
        VarLifeTime * var_lt = genLifeTime(id);
        ASSERT0(var_lt);
        var_lt->addBB(bb->id());
    }

    for (UINT id = (UINT)live_out->get_first(&iter);
         id != BS_UNDEF; id = (UINT)live_out->get_next(id, &iter)) {
        VarLifeTime * var_lt = genLifeTime(id);
        ASSERT0(var_lt);
        var_lt->addBB(bb->id());
    }

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

        computeRHS(ir, *this, upos, livein_def, irit);
        computeLHS(ir, *this, dpos);
    }
    Pos dpos_bb_end = 0, upos_bb_end = 0;
    up.updateAtBBExit(dpos_bb_end, upos_bb_end);
    m_bb_exit_pos.set(bb->id(), upos_bb_end);
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


void VarLifeTimeMgr::computeAccuLifeTimeBB(UpdatePos & up, IRBB const* bb,
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
        VarRange r = var_lt->getLastAccuRange();
        if (r.start() == POS_UNDEF) {
            //VAR is region livein.
            VRG_start(r) = livein_def;
            VRG_end(r) = dpos_bb_start;
        } else {
            VRG_end(r) = dpos_bb_start;
        }
        var_lt->setLastAccuRange(r);
    }

    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter bbirit;
    for (IR * ir = irlst.get_head(&bbirit);
         ir != nullptr; ir = irlst.get_next(&bbirit)) {
        Pos dpos, upos;
        if (!up.updateAtIR(ir, dpos, upos)) {
            continue;
        }

        computeRHSAccu(ir, *this, upos, livein_def, irit);
        computeLHSAccu(ir, *this, dpos);
    }

    Pos dpos_bb_end = 0, upos_bb_end = 0;
    up.updateAtBBExit(dpos_bb_end, upos_bb_end);
    m_bb_exit_pos.set(bb->id(), upos_bb_end);

    for (UINT id = (UINT)live_out->get_first(&iter);
         id != BS_UNDEF; id = (UINT)live_out->get_next(id, &iter)) {
        VarLifeTime * var_lt = genLifeTime(id);
        ASSERT0(var_lt);
        VarRange r = var_lt->getLastAccuRange();
        if (r.start() == POS_UNDEF) {
            //VAR is region liveout.
            VRG_start(r) = livein_def;
            VRG_end(r) = upos_bb_end;
        } else {
            VRG_end(r) = upos_bb_end;
        }
        var_lt->setLastAccuRange(r);
    }
}


void VarLifeTimeMgr::computeAccuLifeTime()
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
        computeAccuLifeTimeBB(up, bb, livein_def, irit);
    }
    Pos dpos_end, upos_end;
    bool valid2 = up.updateAtRegionExit(dpos_end, upos_end);
    ASSERT0_DUMMYUSE(valid2);
}
//END VarLifeTimeMgr


//
//START VarInterfGraph
//
void VarInterfGraph::build()
{
    erase();
    VarLTListIter it1;
    VarLTList const& ltlst = m_lt_mgr.getLTList();
    for (VarLifeTime * lt1 = ltlst.get_head(&it1);
         lt1 != nullptr; lt1 = ltlst.get_next(&it1)) {
        addVertex(lt1->id());
        VarLTListIter it2;
        for (VarLifeTime * lt2 = ltlst.get_head(&it2);
             lt2 != nullptr; lt2 = ltlst.get_next(&it2)) {
            if (lt1->id() == lt2->id()) { continue; }
            if (lt1->isIntersect(lt2)) {
                addEdge(lt1->id(), lt2->id());
                continue;
            }
            addVertex(lt2->id());
        }
    }
}


void VarInterfGraph::dumpVertexDesc(Vertex const* v, OUT StrBuf & buf) const
{
    ASSERT0(v);
    buf.sprint("$%u", v->id());
    UINT varid = m_lt2var.get(v->id());
    if (varid == LT_ID_UNDEF) { return; }
    buf.strcat(", %u", varid);
}


void VarInterfGraph::color()
{
    //Build the Interference graph first.
    build();

    typedef DefSBitSet VarSet;
    typedef DefSBitSetIter VarSetIter;
    DefMiscBitSetMgr sbs_mgr;
    VarSet total_var_set(sbs_mgr.getSegMgr());
    VarSet used_var_set(sbs_mgr.getSegMgr());

    VarLTListIter it;
    VarLTList const& ltlst = m_lt_mgr.getLTList();
    for (VarLifeTime * lt = ltlst.get_head(&it);
         lt != nullptr; lt = ltlst.get_next(&it)) {
        total_var_set.bunion(lt->getVarId());
    }

    for (VarLifeTime * lt = ltlst.get_head(&it);
         lt != nullptr; lt = ltlst.get_next(&it)) {
        xcom::Vertex const* v = getVertex(lt->id());
        ASSERT0(v);
        AdjVertexIter it;
        for (Vertex * in_v = Graph::get_first_in_vertex(v, it); in_v!= nullptr;
             in_v = Graph::get_next_in_vertex(it)) {
            UINT var_id = m_lt2var.get(in_v->id());
            if (var_id == VAR_ID_UNDEF) { continue; }
            used_var_set.bunion(var_id);
            total_var_set.diff(var_id);
        }

        for (Vertex * out_v = Graph::get_first_out_vertex(v, it);
             out_v!= nullptr; out_v = Graph::get_next_out_vertex(it)) {
            UINT var_id = m_lt2var.get(out_v->id());
            if (var_id == VAR_ID_UNDEF) { continue; }
            used_var_set.bunion(var_id);
            total_var_set.diff(var_id);
        }
        VarSetIter set_it;
        BSIdx avail_id = total_var_set.get_first(&set_it);
        ASSERT0(avail_id != VAR_ID_UNDEF);
        m_lt2var.set(lt->id(), avail_id);
        total_var_set.bunion(used_var_set);
    }
}


bool VarInterfGraph::isInterferWithAllocatedColors(VarLifeTime const* lt,
                                                   OUT VecIdx & id)
{
    for (VecIdx i = 0; i < (VecIdx)m_color_allocated.get_elem_count(); i++) {
        Vector<VarLifeTime*> * color = getColor(i);
        ASSERT0(color);
        bool is_interference_cur_color = false;
        for (VecIdx j = 0; j < (VecIdx)color->get_elem_count(); j++) {
            VarLifeTime * occupied_lt = color->get(j);
            ASSERT0(occupied_lt);
            if (const_cast<VarLifeTime*>(lt)->isIntersect(occupied_lt)) {
                is_interference_cur_color = true;
                break;
            }
        }
        if (!is_interference_cur_color) {
            id = i;
            return false;
        }
    }
    return true;
}


void VarInterfGraph::colorFast()
{
    initColorPool();

    VarLTListIter it;
    VarLTList const& ltlst = m_lt_mgr.getLTList();
    for (VarLifeTime * lt = ltlst.get_head(&it); lt != nullptr;
         lt = ltlst.get_next(&it)) {
        VecIdx avail_color_id = VEC_UNDEF;
        if (isInterferWithAllocatedColors(lt, avail_color_id)) {
            Vector<VarLifeTime*> * color = genColor(m_next_color_index);
            color->append(lt);
            assignColor(lt, m_next_color_index++);
            continue;
        }
        ASSERT0(avail_color_id != VEC_UNDEF);
        ASSERT0(getColor(avail_color_id));
        getColor(avail_color_id)->append(lt);
        assignColor(lt, avail_color_id);
    }
}


void VarInterfGraph::dump()
{
    note(m_rg, "\n==-- DUMP %s --==", "VLT2VarMap");
    prt(m_rg, "\n  [LT Info] --> [Var Info]\n");
    LTId2VarIdIter it;
    UINT vid = 0;
    UINT id = 0;
    for (UINT lt_id = m_lt2var.get_first(it, &vid); !it.end();
         lt_id = m_lt2var.get_next(it, &vid)) {
        prt(m_rg, "  %u: [%u] --> [%s, %u]\n", id++, lt_id,
        m_rg->getVarMgr()->get_var(vid)->get_name()->getStr(), vid);
    }
}


void VarInterfGraph::rewrite()
{
    if (m_next_color_index == m_color_pool.get_elem_count()) {
        //If all the stack slots are used, it is not necessary to do the
        //rewrite process.
        return;
    }
    BBList const* bblst = m_rg->getBBList();
    BBListIter it;
    for (bblst->get_head(&it); it != bblst->end(); it = bblst->get_next(it)) {
        IRBB const* bb = it->val();
        ASSERT0(bb);
        rewriteBB(bb);
    }
}


void VarInterfGraph::rewriteBB(IRBB const* bb)
{
    ASSERT0(bb);
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter irit;
    for (IR * x = irlst.get_tail(&irit);
         x != nullptr; x = irlst.get_prev(&irit)) {
        rewriteStmt(x);
    }
}


void VarInterfGraph::rewriteStmt(IR * ir)
{
    ASSERT0(ir);
    if (m_lt_mgr.getLSRA().isSpillOp(ir)) {
        Var * v = ir->getIdinfo();
        ASSERT0(v);
        if (VarCheck::checkVarType(v)) { return; }
        VarLifeTime * lt = m_lt_mgr.getLifeTime(v->id());
        ASSERT0(lt);
        Var * assign_var =
            m_rg->getVarMgr()->get_var(m_lt2var.get(lt->id()));
        ASSERT0(assign_var);
        ir->setIdinfo(assign_var);
        return;
    }

    if (m_lt_mgr.getLSRA().isReloadOp(ir)) {
        Var * v = ir->getRHS()->getIdinfo();
        ASSERT0(v);
        if (VarCheck::checkVarType(v)) { return; }
        VarLifeTime * lt = m_lt_mgr.getLifeTime(v->id());
        ASSERT0(lt);
        Var * assign_var =
            m_rg->getVarMgr()->get_var(m_lt2var.get(lt->id()));
        ASSERT0(assign_var);
        ir->getRHS()->setIdinfo(assign_var);
    }
}
//END VarInterfGraph

} //namespace xoc
