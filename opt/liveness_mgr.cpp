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
#include "liveness_mgr.h"

namespace xoc {

//#define STATISTIC_LIVENESS
#ifdef STATISTIC_LIVENESS
static UINT g_max_times = 0;
#endif

static void statistic_liveness(Region const* rg)
{
    #ifdef STATISTIC_LIVENESS
    g_max_times = MAX(g_max_times, count);
    FileObj fo("liveness.sat.dump", false, false);
    fprintf(fo.getFileHandler(), "\n%s run %u times, maxtimes %u",
            rg->getRegionName(), count, g_max_times);
    #endif
}


//
//START AuxLivenessMgr
//
PRLiveSet * AuxLivenessMgr::genNewLiveIn(UINT bbid)
{
    PRLiveSet * x = m_new_livein_vec.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_new_livein_vec.set(bbid, x);
    }
    return x;
}


PRLiveSet * AuxLivenessMgr::genNewLiveOut(UINT bbid)
{
    PRLiveSet * x = m_new_liveout_vec.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_new_liveout_vec.set(bbid, x);
    }
    return x;
}


void AuxLivenessMgr::init()
{
    BBListIter it;
    BBList const* bblst = m_rg->getBBList();
    ASSERT0(bblst);

    for (bblst->get_head(&it); it != bblst->end(); it = bblst->get_next(it)) {
        IRBB * bb = it->val();
        ASSERT0(bb);

        UINT bbid = bb->id();
        genNewLiveIn(bbid)->clean(m_sbs_mgr);
        genNewLiveOut(bbid)->clean(m_sbs_mgr);
    }
}


void AuxLivenessMgr::destroy()
{
    for (VecIdx i = 0; i <= m_new_liveout_vec.get_last_idx(); i++) {
        PRLiveSet * bs = m_new_liveout_vec.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_new_liveout_vec.clean();

    for (VecIdx i = 0; i <= m_new_livein_vec.get_last_idx(); i++) {
        PRLiveSet * bs = m_new_livein_vec.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_new_livein_vec.clean();
}
//END AuxLivenessMgr


//
//START LivenessMgr
//
void LivenessMgr::cleanGlobal()
{
    for (VecIdx i = 0; i <= m_livein.get_last_idx(); i++) {
        PRLiveSet * bs = m_livein.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_livein.reinit();
    for (VecIdx i = 0; i <= m_liveout.get_last_idx(); i++) {
        PRLiveSet * bs = m_liveout.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_liveout.reinit();
}


void LivenessMgr::cleanLocal()
{
    for (VecIdx i = 0; i <= m_def.get_last_idx(); i++) {
        PRLiveSet * bs = m_def.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_def.reinit();
    for (VecIdx i = 0; i <= m_use.get_last_idx(); i++) {
        PRLiveSet * bs = m_use.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_use.reinit();
}


bool LivenessMgr::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return true; }
    note(getRegion(), "\n==---- DUMP LivenessMgr : liveness of PR ----==\n");
    List<IRBB*> * bbl = m_rg->getBBList();
    FILE * file = getRegion()->getLogMgr()->getFileHandler();
    getRegion()->getLogMgr()->incIndent(2);
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(getRegion(), "\n-- BB%d --", bb->id());
        PRLiveSet * live_in = get_livein(bb->id());
        PRLiveSet * live_out = get_liveout(bb->id());
        PRLiveSet * def = get_def(bb->id());
        PRLiveSet * use = get_use(bb->id());
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
    return Pass::dump();
}


size_t LivenessMgr::count_mem() const
{
    size_t count = m_sbs_mgr.count_mem();
    count += m_def.count_mem();
    count += m_use.count_mem();
    count += m_livein.count_mem();
    count += m_liveout.count_mem();
    for (VecIdx i = 0; i <= m_def.get_last_idx(); i++) {
        PRLiveSet * bs = m_def.get((UINT)i);
        if (bs != nullptr) {
            count += bs->count_mem();
        }
    }
    for (VecIdx i = 0; i <= m_use.get_last_idx(); i++) {
        PRLiveSet * bs = m_use.get((UINT)i);
        if (bs != nullptr) {
            count += bs->count_mem();
        }
    }
    for (VecIdx i = 0; i <= m_livein.get_last_idx(); i++) {
        PRLiveSet * bs = m_livein.get((UINT)i);
        if (bs != nullptr) {
            count += bs->count_mem();
        }
    }
    for (VecIdx i = 0; i <= m_liveout.get_last_idx(); i++) {
        PRLiveSet * bs = m_liveout.get((UINT)i);
        if (bs != nullptr) {
            count += bs->count_mem();
        }
    }
    return count;
}


PRLiveSet * LivenessMgr::gen_def(UINT bbid)
{
    PRLiveSet * x = m_def.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_def.set(bbid, x);
    }
    return x;
}


PRLiveSet * LivenessMgr::gen_use(UINT bbid)
{
    PRLiveSet * x = m_use.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_use.set(bbid, x);
    }
    return x;
}


PRLiveSet * LivenessMgr::gen_livein(UINT bbid)
{
    PRLiveSet * x = m_livein.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_livein.set(bbid, x);
    }
    return x;
}


PRLiveSet * LivenessMgr::gen_liveout(UINT bbid)
{
    PRLiveSet * x = m_liveout.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_liveout.set(bbid, x);
    }
    return x;
}


void LivenessMgr::processMayDef(PRNO prno, MOD PRLiveSet * gen,
                                MOD PRLiveSet * use)
{
    ASSERT0(gen && use);
    gen->bunion((BSIdx)prno, m_sbs_mgr);
    use->diff((BSIdx)prno, m_sbs_mgr);
}


void LivenessMgr::processMayUse(PRNO prno, MOD PRLiveSet * use)
{
    ASSERT0(use);
    use->bunion((BSIdx)prno, m_sbs_mgr);
}


void LivenessMgr::processMay(IR const* pr, MOD PRLiveSet * gen,
                             MOD PRLiveSet * use, bool is_lhs)
{
    if (!m_handle_may) { return; }
    MDSet const* mds = pr->getMayRef();
    if (mds == nullptr) { return; }

    MD const* prmd = pr->getExactRef();
    ASSERT0(prmd);
    MDSetIter iter;
    for (BSIdx i = mds->get_first(&iter);
         i != BS_UNDEF; i = mds->get_next(i, &iter)) {
        MD const* md = m_md_sys->getMD((MDIdx)i);
        ASSERT0(md);
        if (MD_base(md) == MD_base(prmd)) { continue; }

        bool find;
        ASSERT0(m_var2pr); //One should initialize m_var2pr.
        PRNO prno = m_var2pr->get(MD_base(md), &find);
        ASSERT0(find);
        if (is_lhs) {
            processMayDef(prno, gen, use);
            continue;
        }
        processMayUse(prno, use);
    }
}


void LivenessMgr::processOpnd(
    IR const* ir, ConstIRIter & it, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    for (IR const* k = iterInitC(ir, it); k != nullptr; k = iterNextC(it)) {
        if (k->isReadPR()) {
            use->bunion((BSIdx)k->getPrno(), m_sbs_mgr);
            processMay(k, gen, use, false);
        }
    }
}


void LivenessMgr::processPHI(
    IR const* x, ConstIRIter & it, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    gen->bunion((BSIdx)PHI_prno(x), m_sbs_mgr);
    use->diff((BSIdx)PHI_prno(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(PHI_opnd_list(x), it, use, gen);
}


void LivenessMgr::processSTPR(
    IR const* x, ConstIRIter & it, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    gen->bunion((BSIdx)STPR_no(x), m_sbs_mgr);
    use->diff((BSIdx)STPR_no(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(STPR_rhs(x), it, use, gen);
}


void LivenessMgr::processSETELEM(
    IR const* x, ConstIRIter & it, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    gen->bunion((BSIdx)GETELEM_prno(x), m_sbs_mgr);
    use->diff((BSIdx)GETELEM_prno(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(GETELEM_base(x), it, use, gen);

    it.clean();
    processOpnd(GETELEM_ofst(x), it, use, gen);
}


void LivenessMgr::processGETELEM(
    IR const* x, ConstIRIter & it, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    gen->bunion((BSIdx)SETELEM_prno(x), m_sbs_mgr);
    use->diff((BSIdx)SETELEM_prno(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(SETELEM_base(x), it, use, gen);

    it.clean();
    processOpnd(SETELEM_val(x), it, use, gen);

    it.clean();
    processOpnd(SETELEM_ofst(x), it, use, gen);
}


void LivenessMgr::processCallStmt(
    IR const* x, ConstIRIter & it, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    if (x->hasReturnValue()) {
        gen->bunion((BSIdx)CALL_prno(x), m_sbs_mgr);
        use->diff((BSIdx)CALL_prno(x), m_sbs_mgr);
        processMay(x, gen, use, true);
    }

    it.clean();
    processOpnd(CALL_arg_list(x), it, use, gen);

    if (x->is_icall() && ICALL_callee(x)->is_pr()) {
        use->bunion((BSIdx)PR_no(ICALL_callee(x)), m_sbs_mgr);
        processMay(ICALL_callee(x), gen, use, false);
    }
}


void LivenessMgr::computeExp(
    IR const* stmt, ConstIRIter & it, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    ASSERT0(stmt->is_stmt());
    it.clean();
    for (IR const* k = iterExpInitC(stmt, it);
         k != nullptr; k = iterExpNextC(it)) {
        ASSERT0(k->is_exp());
        if (!k->isReadPR()) { continue; }
        use->bunion((BSIdx)k->getPrno(), m_sbs_mgr);
        processMay(k, gen, use, false);
    }
}


void LivenessMgr::computeStmt(
    IR const* stmt, ConstIRIter & it, MOD PRLiveSet * use, MOD PRLiveSet * gen)
{
    ASSERT0(stmt->is_stmt());
    if (stmt->isWritePR() || stmt->isCallStmt() || stmt->is_region()) {
        IR * result = const_cast<IR*>(stmt)->getResultPR();
        if (result != nullptr) {
            PRNO prno = result->getPrno();
            gen->bunion((BSIdx)prno, m_sbs_mgr);
            use->diff((BSIdx)prno, m_sbs_mgr);
        }
        processMay(stmt, gen, use, true);
    }
    computeExp(stmt, it, use, gen);
}


void LivenessMgr::computeLocal(BBList const& bblst)
{
    ConstIRIter irit;
    BBListIter it;
    for (bblst.get_head(&it); it != bblst.end(); it = bblst.get_next(it)) {
        IRBB const* bb = it->val();
        ASSERT0(bb);
        computeLocal(bb, irit);
    }
}


void LivenessMgr::computeLocal(IRBB const* bb, MOD ConstIRIter & it)
{
    PRLiveSet * use = gen_use(bb->id());
    PRLiveSet * gen = gen_def(bb->id());
    use->clean(m_sbs_mgr);
    gen->clean(m_sbs_mgr);
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter irit;
    for (IR * x = irlst.get_tail(&irit);
         x != nullptr; x = irlst.get_prev(&irit)) {
        computeStmt(x, it, use, gen);
    }
}


void LivenessMgr::init_livein(UINT bbid)
{
    PRLiveSet const* use = get_use(bbid);
    if (use != nullptr) {
        gen_livein(bbid)->copy(*use, m_sbs_mgr);
    } else {
        gen_livein(bbid)->clean(m_sbs_mgr);
    }
}


void LivenessMgr::initSet(BBList const& bblst)
{
    BBListIter it;
    for (bblst.get_head(&it); it != bblst.end(); it = bblst.get_next(it)) {
        IRBB * bb = it->val();
        ASSERT0(bb);
        init_livein(bb->id());
        gen_liveout(bb->id())->clean(m_sbs_mgr);
    }
}


void LivenessMgr::setLivenessForEmptyBB(IRBB const* empty_bb, IRBB const* from)
{
    ASSERT0(empty_bb && from);
    ASSERT0(empty_bb->is_empty());
    ASSERT0(m_rg->getCFG()->is_pred(empty_bb->getVex(), from->getVex()));
    ASSERT0(empty_bb->getVex()->getInDegree() == 1);
    ASSERT0(empty_bb->getVex()->getOutDegree() == 1);
    PRLiveSet const* live_set = get_liveout(from->id());
    ASSERT0(live_set);

    //Here we use the liveout of the predecessor BB as the liveset to be set
    //for the empty latch BB, because this liveset is the subset of the livein
    //of the successor BB, and this liveset is more accurate.
    set_liveout(empty_bb->id(), live_set);
    set_livein(empty_bb->id(), live_set);
}


//Note this function still not consider PHI effect properly.
//e.g:  BB1:          BB2:
//      st $4 = 0     st $3 = 1
//           \        /
//            \      /
//    BB3:     |    |
//    phi $5 = $4, $3
//    ...
//
//In actually , $4 only lived out from BB1, and $3 only lived out
//from BB2. For present, $4 both live out from BB1 and BB2, and $3
//is similar.
void LivenessMgr::computeGlobal(IRCFG const* cfg)
{
    ASSERT0(cfg->getEntry() && BB_is_entry(cfg->getEntry()));
    //RPO should be available.
    RPOVexList const* vlst = const_cast<IRCFG*>(cfg)->getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == cfg->getBBList()->get_elem_count());
    bool change;
    UINT count = 0;
    UINT thres = 1000;
    PRLiveSet news;
    do {
        change = false;
        RPOVexListIter ct2;
        for (vlst->get_tail(&ct2);
             ct2 != vlst->end(); ct2 = vlst->get_prev(ct2)) {
            IRBB const* bb = cfg->getBB(ct2->val()->id());
            ASSERT0(bb);
            UINT bbid = bb->id();
            PRLiveSet * out = get_liveout(bbid);
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
            PRLiveSet const* def = get_def(bbid);
            if (def != nullptr) {
                news.diff(*def, m_sbs_mgr);
            }
            PRLiveSet const* use = get_use(bbid);
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
    statistic_liveness(m_rg);
}


void LivenessMgr::eliminateRedundantLiveness(IRCFG const* cfg)
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
    //
    //How to get useful liveout info from predecessor in 'c step' ?
    //a.If there is loop edge in CFG, the predecessor maybe not visited when a
    //  node is prepared to compute new liveness info. Thus two temporary vector
    //  of 'new_livein' and 'new_liveout' are introduced. Firstly, these two
    //  vector are initialized to empty. Then their will be constantly updated
    //  after new liveness info of node have been computed. Empty value of these
    //  new liveness info will be get if the node doesn't be visitd yet. In the
    //  whole algorithm, new liveness info after computed will just be updated
    //  into 'new_livein' and 'new_liout' vector instead of the original vector
    //  'm_livein' and 'm_liveout'.
    //b.The algorithm of computed new liveness info will be repeated utill the
    //  new liveness info in 'new_livein' and 'new_liveout' of each node don't
    //  be changed.
    //  ba.As shown in the following figure, redundant liveness '$1' is passed
    //     on to 'entry' and [N1] node from 'loop1' and '$2' is passed on to
    //     'entry' and 'N[1:4]' node from 'loop2'. These redundant info need to
    //     be removed by this algorithm.
    //  bb.'$1' in entry and [N1] node will be removed in the first iteration.
    //  bc.It is more complexly calculation for removed redundant liveness info
    //     '$2' from 'loop1'. In the first iteration, '$1' generated from [N4]
    //     and $2 generated from [N8] will be regarded as redundant livenss
    //     info for [N2]. Since both [N4] and [N8] still don't be visited and
    //     their new liveness vector of 'new_livein' and 'new_liveout' are
    //     empty. Thus '$1' and '$2' can't be affected the new liveness info
    //     computed of [N2]. '$1' and '$2' updated to new liveness info vector
    //     untill [N4] and [N8] visited.
    //  bd.In the second iteration, '$1' can't be regarded as redundant info
    //     for [N2] and it will be updated to new liveness info of [N2] too,
    //     because the new liveness of [N4] is no longer empty. But there is
    //     no chances for '$2' to updated to the new liveness vector of [N2].
    //  be.If the new liveness info computed of a node aren't equal to the
    //     original liveness info that have been stored in 'new_livein' and
    //     'new_liveout' by last iteration. The iteration will be continued.
    //
    //                     |-------$1------|               |------$2-------|
    //   $1,$2             V      loop1    |               V     loop2     |
    // [entry] -> [N1] -> [N2] -> [N3] -> [N4] -> [N5] -> [N6] -> [N7] -> [N8]
    //                     |
    //                     |---> [N9] -> [N10] -> [exit]
    //
    ASSERT0(cfg && cfg->getEntry() && BB_is_entry(cfg->getEntry()));
    AuxLivenessMgr auxmgr(m_rg);

    //RPO should be available.
    RPOVexList const* vlst = const_cast<IRCFG*>(cfg)->getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == cfg->getBBList()->get_elem_count());

    UINT count = 0;
    bool change = false;
    UINT entry_id = cfg->getEntry()->id();
    PRLiveSet entry_use, entry_def, redundant_live;

    //The livein of entry_id is the full set of redundant live.
    redundant_live.copy(*get_livein(cfg->getEntry()->id()), auxmgr.getSBSMgr());

    //Re-compute livein and liveout of entry_bb.
    entry_use.copy(*get_use(entry_id), auxmgr.getSBSMgr());
    entry_def.copy(*get_def(entry_id), auxmgr.getSBSMgr());

    //livein(new) = livein(old) intersect 'use info'.
    //e.g: [23] = [20, 23] intersect [23].
    entry_use.intersect(*get_livein(entry_id), auxmgr.getSBSMgr());
    //liveout(new) = liveout(old) intersect 'def info'.
    //e.g: [25] = [20, 25] intersect [24, 25].
    entry_def.intersect(*get_liveout(entry_id), auxmgr.getSBSMgr());

    //Reset liveness info of entry bb.
    get_livein(entry_id)->copy(entry_use, m_sbs_mgr);
    get_liveout(entry_id)->copy(entry_def, m_sbs_mgr);

    //Compute new livein and new liveout.
    do {
        change = eliminateRedundantLivenessImpl(cfg, auxmgr, redundant_live);
        count++;
    } while (change && count < 100);

    //Copy 'm_new_livein/out' to 'm_livein/out' after
    //redundant liveness have been removed.
    resetLivenessAfterRemoveRedundantLiveness(cfg, auxmgr);

    entry_use.clean(auxmgr.getSBSMgr());
    entry_def.clean(auxmgr.getSBSMgr());
    redundant_live.clean(auxmgr.getSBSMgr());
}


bool LivenessMgr::eliminateRedundantLivenessImpl(
    IRCFG const* cfg, AuxLivenessMgr & auxmgr, PRLiveSet const& redundant_live)
{
    ASSERT0(cfg);

    AdjVertexIter ito;
    xcom::TTab<UINT> visit;
    bool change = false;
    UINT entry_id = cfg->getEntry()->id();
    xcom::List<Vertex const*> vertex_list;
    PRLiveSet news, tmp_live, pre_vertex_new_live_out;

    //Identify entry vertex has been visited(re-computed).
    visit.append(entry_id);

    //Iterate all successors node of current node.
    Vertex const* o = Graph::get_first_out_vertex(
        cfg->Graph::getVertex(entry_id), ito);
    for (; o != nullptr; o = Graph::get_next_out_vertex(ito)) {
        vertex_list.append_tail(o);
    }

    //Re-compute the liveness of all successor nodes of entry_bb.
    while (vertex_list.get_elem_count() != 0) {
        Vertex const* v = vertex_list.get_head();
        ASSERT0(v);
        vertex_list.remove_head();

        //Whether 'v' has been visited.
        if (visit.find(v->id())) { continue; }

        //Identify 'v' has been visited(re-computed).
        visit.append(v->id());

        //Clean.
        pre_vertex_new_live_out.clean(auxmgr.getSBSMgr());

        //Get the liveout of predecessor node.
        getPreVertexNewLiveOut(pre_vertex_new_live_out, v, auxmgr);

        //Re-computed livein.
        //a.Get the full set useless liveness info(livein_old(entry)).
        tmp_live.copy(redundant_live, auxmgr.getSBSMgr());

        //b.Exclude useful liveness info(liveout_new(pre)) from full set.
        tmp_live.diff(pre_vertex_new_live_out, auxmgr.getSBSMgr());

        //c.Exclude useful liveness info(be used in current node) from full set.
        tmp_live.diff(*get_use(v->id()), auxmgr.getSBSMgr());

        //d.Get livein_old(cur).
        news.copy(*get_livein(v->id()), auxmgr.getSBSMgr());

        //e.Exclude useless liveness info(tmp_live) from livein_old(cur).
        news.diff(tmp_live, auxmgr.getSBSMgr());

        //f.If the 'news' is different with the livein which has been recorded
        //  in 'm_new_livein' by last time, it represents that the livein info
        //  has been changed.
        if (!(auxmgr.getNewLiveIn(v->id())->is_equal(news))) {
            change = true;
        }
        //g.Update the livein in 'm_new_livein'.
        auxmgr.getNewLiveIn(v->id())->copy(news, auxmgr.getSBSMgr());

        //Re-computed liveout.
        //a.Get the full set useless liveness info(livein_old(entry)).
        tmp_live.copy(redundant_live, auxmgr.getSBSMgr());

        //b.Exclude useful liveness info(liveout_new(pre)) from full set.
        tmp_live.diff(pre_vertex_new_live_out, auxmgr.getSBSMgr());

        //c.Exclude useful liveness info(generated in current node).
        tmp_live.diff(*get_def(v->id()), auxmgr.getSBSMgr());

        //d.Get liveout_old(cur).
        news.copy(*get_liveout(v->id()), auxmgr.getSBSMgr());

        //e.Exclude useless liveness info(tmp_live) from liveout_old(cur).
        news.diff(tmp_live, auxmgr.getSBSMgr());

        //f.If the 'news' is different with the liveout which has been recorded
        //  in 'm_new_liveout' by last time, it represents that the liveout info
        //  has been changed.
        if (!(auxmgr.getNewLiveOut(v->id())->is_equal(news))) {
            change = true;
        }
        //g.Update the liveout in 'm_new_liveout'.
        auxmgr.getNewLiveOut(v->id())->copy(news, auxmgr.getSBSMgr());

        //Iterate all successor nodes of current node.
        Vertex const* o = Graph::get_first_out_vertex(v, ito);
        for (; o != nullptr; o = Graph::get_next_out_vertex(ito)) {
            if (visit.find(o->id())) { continue; }
            vertex_list.append_tail(o);
        }
    }

    news.clean(auxmgr.getSBSMgr());
    tmp_live.clean(auxmgr.getSBSMgr());
    pre_vertex_new_live_out.clean(auxmgr.getSBSMgr());

    return change;
}


void LivenessMgr::resetLivenessAfterRemoveRedundantLiveness(
    IRCFG const* cfg, AuxLivenessMgr & auxmgr)
{
    ASSERT0(cfg);

    AdjVertexIter ito;
    xcom::TTab<UINT> visit;
    xcom::List<Vertex const*> vertex_list;
    UINT entry_id = cfg->getEntry()->id();

    //Identify entry vertex has been visited.
    visit.append(entry_id);

    Vertex const* o = Graph::get_first_out_vertex(
        cfg->Graph::getVertex(entry_id), ito);
    for (; o != nullptr; o = Graph::get_next_out_vertex(ito)) {
        vertex_list.append_tail(o);
    }

    while (vertex_list.get_elem_count() != 0) {
        Vertex const* v = vertex_list.get_head();
        ASSERT0(v);

        vertex_list.remove_head();
        //Identify 'v' has been visited.
        if (visit.find(v->id())) { continue; }
        //Identify vertex has been visited.
        visit.append(v->id());

        //Copy 'm_new_liveout' to 'm_liveout'.
        get_liveout(v->id())->copy(
            *auxmgr.getNewLiveOut(v->id()), m_sbs_mgr);
        //Copy 'm_new_livein' to 'm_livein'.
        get_livein(v->id())->copy(
            *auxmgr.getNewLiveIn(v->id()), m_sbs_mgr);

        Vertex const* o = Graph::get_first_out_vertex(v, ito);
        for (; o != nullptr; o = Graph::get_next_out_vertex(ito)) {
            if (visit.find(o->id())) { continue; }
            vertex_list.append_tail(o);
        }
    }
}


void LivenessMgr::getPreVertexNewLiveOut(
    PRLiveSet & live_new_out, Vertex const* v, AuxLivenessMgr & auxmgr)
{
    ASSERT0(v);
    xcom::VexIter iter;
    for (xcom::Vertex * c = v->getFirstFromVex(&iter);
         c != nullptr; c = v->getNextFromVex(&iter)) {
        live_new_out.bunion(*auxmgr.getNewLiveOut(c->id()), auxmgr.getSBSMgr());
    }
}


bool LivenessMgr::perform(BBList const* bblst, IRCFG const* cfg, OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    if (bblst->get_elem_count() == 0) { return false; }
    computeLocal(*bblst);
    initSet(*bblst);
    computeGlobal(cfg);
    END_TIMER(t, getPassName());
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLivenessMgr()) {
        dump();
    }
    if (!m_keep_local) {
        cleanLocal();
    }
    set_valid(true);
    return false;
}


bool LivenessMgr::perform(OptCtx & oc)
{
    return perform(m_rg->getBBList(), m_rg->getCFG(), oc);
}
//END LivenessMgr

} //namespace xoc
