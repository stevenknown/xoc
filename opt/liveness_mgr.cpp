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
    FILE * h = fopen("liveness.sat.dump", "a+");
    fprintf(h, "\n%s run %u times, maxtimes %u",
            rg->getRegionName(), count, g_max_times);
    fclose(h);
    #endif
}


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


void LivenessMgr::processOpnd(IR const* ir, ConstIRIter & it,
                              MOD PRLiveSet * use,
                              MOD PRLiveSet * gen)
{
    for (IR const* k = iterInitC(ir, it); k != nullptr; k = iterNextC(it)) {
        if (k->isReadPR()) {
            use->bunion((BSIdx)k->getPrno(), m_sbs_mgr);
            processMay(k, gen, use, false);
        }
    }
}


void LivenessMgr::processPHI(IR const* x, ConstIRIter & it,
                             MOD PRLiveSet * use,
                             MOD PRLiveSet * gen)
{
    gen->bunion((BSIdx)PHI_prno(x), m_sbs_mgr);
    use->diff((BSIdx)PHI_prno(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(PHI_opnd_list(x), it, use, gen);
}


void LivenessMgr::processSTPR(IR const* x, ConstIRIter & it,
                              MOD PRLiveSet * use,
                              MOD PRLiveSet * gen)
{
    gen->bunion((BSIdx)STPR_no(x), m_sbs_mgr);
    use->diff((BSIdx)STPR_no(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(STPR_rhs(x), it, use, gen);
}


void LivenessMgr::processSETELEM(IR const* x, ConstIRIter & it,
                                 MOD PRLiveSet * use,
                                 MOD PRLiveSet * gen)
{
    gen->bunion((BSIdx)GETELEM_prno(x), m_sbs_mgr);
    use->diff((BSIdx)GETELEM_prno(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(GETELEM_base(x), it, use, gen);

    it.clean();
    processOpnd(GETELEM_ofst(x), it, use, gen);
}


void LivenessMgr::processGETELEM(IR const* x, ConstIRIter & it,
                                 MOD PRLiveSet * use,
                                 MOD PRLiveSet * gen)
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


void LivenessMgr::processCallStmt(IR const* x, ConstIRIter & it,
                                  MOD PRLiveSet * use,
                                  MOD PRLiveSet * gen)
{
    if (x->hasReturnValue()) {
        gen->bunion((BSIdx)CALL_prno(x), m_sbs_mgr);
        use->diff((BSIdx)CALL_prno(x), m_sbs_mgr);
        processMay(x, gen, use, true);
    }

    it.clean();
    processOpnd(CALL_param_list(x), it, use, gen);

    if (x->is_icall() && ICALL_callee(x)->is_pr()) {
        use->bunion((BSIdx)PR_no(ICALL_callee(x)), m_sbs_mgr);
        processMay(ICALL_callee(x), gen, use, false);
    }
}


void LivenessMgr::computeExp(IR const* stmt, ConstIRIter & it,
                             MOD PRLiveSet * use,
                             MOD PRLiveSet * gen)
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


void LivenessMgr::computeStmt(IR const* stmt, ConstIRIter & it,
                              MOD PRLiveSet * use,
                              MOD PRLiveSet * gen)
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
    ASSERTN(!change, ("result of equation is convergent slowly"));
    news.clean(m_sbs_mgr);
    statistic_liveness(m_rg);
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
