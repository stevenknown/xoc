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


class VFExp {
public:
    VarLivenessMgr * var_liveness_mgr;
    PRLiveSet * use;
    MOD PRLiveSet * gen;
    IR const* stmt;
public:
    bool visitIR(IR const* ir, OUT bool & is_terminate)
    {
        if (!ir->is_exp()) { return true; }
        if (!ir->hasIdinfo()) { return true; }
        if (!var_liveness_mgr->canBeExpCand(stmt, ir))  {
            return true; //Keep visiting next kid.
        }
        Var * v = ir->getIdinfo();
        use->bunion((BSIdx)v->id(), var_liveness_mgr->getSBSMgr());
        return true; //Keep visiting next kid.
    }
};

class IterExp : public VisitIRTree<VFExp> {
public:
    IterExp(IR const* ir, VFExp & vf) : VisitIRTree(vf) { visit(ir); }
};


void VarLivenessMgr::computeExp(
    IR const* stmt, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    ASSERT0(stmt->is_stmt());
    VFExp vf;
    vf.var_liveness_mgr = this;
    vf.use = use;
    vf.gen = gen;
    vf.stmt = stmt;
    IterExp it(stmt, vf);
}


void VarLivenessMgr::computeStmt(IR const* stmt, MOD VarLiveSet * use,
                                 MOD VarLiveSet * gen)
{
    ASSERT0(stmt && use && gen);
    ASSERT0(stmt->is_stmt());
    if (stmt->hasIdinfo() && canBeStmtCand(stmt)) {
        Var * v = stmt->getIdinfo();
        gen->bunion((BSIdx)v->id(), getSBSMgr());
        use->diff((BSIdx)v->id(), getSBSMgr());
    }
    computeExp(stmt, use, gen);
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

} //namespace xoc
