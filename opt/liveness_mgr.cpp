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

//
//START LivenessMgr
//
void LivenessMgr::cleanGlobal()
{
    for (VecIdx i = 0; i <= m_livein.get_last_idx(); i++) {
        DefSBitSetCore * bs = m_livein.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_livein.reinit();
    for (VecIdx i = 0; i <= m_liveout.get_last_idx(); i++) {
        DefSBitSetCore * bs = m_liveout.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_liveout.reinit();
}


void LivenessMgr::cleanLocal()
{
    for (VecIdx i = 0; i <= m_def.get_last_idx(); i++) {
        DefSBitSetCore * bs = m_def.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_def.reinit();
    for (VecIdx i = 0; i <= m_use.get_last_idx(); i++) {
        DefSBitSetCore * bs = m_use.get((UINT)i);
        if (bs != nullptr) {
            m_sbs_mgr.freeSBitSetCore(bs);
        }
    }
    m_use.reinit();
}


DefSBitSetCore * LivenessMgr::gen_use(UINT bbid)
{
    DefSBitSetCore * set = m_use.get(bbid);
    if (set == nullptr) {
        set = m_sbs_mgr.allocSBitSetCore();
        m_use.set(bbid, set);
    }
    return set;
}


DefSBitSetCore * LivenessMgr::gen_def(UINT bbid)
{
    DefSBitSetCore * set = m_def.get(bbid);
    if (set == nullptr) {
        set = m_sbs_mgr.allocSBitSetCore();
        m_def.set(bbid, set);
    }
    return set;
}


bool LivenessMgr::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return true; }
    note(getRegion(), "\n==---- DUMP LivenessMgr : liveness of PR ----==\n");
    List<IRBB*> * bbl = m_rg->getBBList();
    FILE * file = getRegion()->getLogMgr()->getFileHandler();
    getRegion()->getLogMgr()->incIndent(2);
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(getRegion(), "\n\n\n-- BB%d --", bb->id());
        DefSBitSetCore * live_in = get_livein(bb->id());
        DefSBitSetCore * live_out = get_liveout(bb->id());
        DefSBitSetCore * def = get_def(bb->id());
        DefSBitSetCore * use = get_use(bb->id());

        note(getRegion(), "\nLIVE-IN: ");
        live_in->dump(file);

        note(getRegion(), "\nLIVE-OUT: ");
        live_out->dump(file);

        note(getRegion(), "\nDEF: ");
        def->dump(file);

        note(getRegion(), "\nUSE: ");
        use->dump(file);
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
        DefSBitSetCore * bs = m_def.get((UINT)i);
        if (bs != nullptr) {
            count += bs->count_mem();
        }
    }
    for (VecIdx i = 0; i <= m_use.get_last_idx(); i++) {
        DefSBitSetCore * bs = m_use.get((UINT)i);
        if (bs != nullptr) {
            count += bs->count_mem();
        }
    }
    for (VecIdx i = 0; i <= m_livein.get_last_idx(); i++) {
        DefSBitSetCore * bs = m_livein.get((UINT)i);
        if (bs != nullptr) {
            count += bs->count_mem();
        }
    }
    for (VecIdx i = 0; i <= m_liveout.get_last_idx(); i++) {
        DefSBitSetCore * bs = m_liveout.get((UINT)i);
        if (bs != nullptr) {
            count += bs->count_mem();
        }
    }
    return count;
}


DefSBitSetCore * LivenessMgr::gen_livein(UINT bbid)
{
    DefSBitSetCore * x = m_livein.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_livein.set(bbid, x);
    }
    return x;
}


DefSBitSetCore * LivenessMgr::gen_liveout(UINT bbid)
{
    DefSBitSetCore * x = m_liveout.get(bbid);
    if (x == nullptr) {
        x = m_sbs_mgr.allocSBitSetCore();
        m_liveout.set(bbid, x);
    }
    return x;
}


void LivenessMgr::processMay(IR const* pr, MOD DefSBitSetCore * gen,
                             MOD DefSBitSetCore * use, bool is_lhs)
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
            ASSERT0(gen && use);
            gen->bunion((BSIdx)prno, m_sbs_mgr);
            use->diff((BSIdx)prno, m_sbs_mgr);
            continue;
        }
        ASSERT0(use);
        use->bunion((BSIdx)prno, m_sbs_mgr);
    }
}


void LivenessMgr::processOpnd(IR const* ir, ConstIRIter & it,
                              MOD DefSBitSetCore * use,
                              MOD DefSBitSetCore * gen)
{
    for (IR const* k = iterInitC(ir, it); k != nullptr; k = iterNextC(it)) {
        if (k->isReadPR()) {
            use->bunion((BSIdx)k->getPrno(), m_sbs_mgr);
            processMay(k, gen, use, false);
        }
    }
}


void LivenessMgr::processPHI(IR const* x, ConstIRIter & it,
                             MOD DefSBitSetCore * use,
                             MOD DefSBitSetCore * gen)
{
    gen->bunion((BSIdx)PHI_prno(x), m_sbs_mgr);
    use->diff((BSIdx)PHI_prno(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(PHI_opnd_list(x), it, use, gen);
}


void LivenessMgr::processSTPR(IR const* x, ConstIRIter & it,
                              MOD DefSBitSetCore * use,
                              MOD DefSBitSetCore * gen)
{
    gen->bunion((BSIdx)STPR_no(x), m_sbs_mgr);
    use->diff((BSIdx)STPR_no(x), m_sbs_mgr);
    processMay(x, gen, use, true);

    it.clean();
    processOpnd(STPR_rhs(x), it, use, gen);
}


void LivenessMgr::processSETELEM(IR const* x, ConstIRIter & it,
                                 MOD DefSBitSetCore * use,
                                 MOD DefSBitSetCore * gen)
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
                                 MOD DefSBitSetCore * use,
                                 MOD DefSBitSetCore * gen)
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
                                  MOD DefSBitSetCore * use,
                                  MOD DefSBitSetCore * gen)
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
                             MOD DefSBitSetCore * use,
                             MOD DefSBitSetCore * gen)
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
                              MOD DefSBitSetCore * use,
                              MOD DefSBitSetCore * gen)
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


void LivenessMgr::computeLocal2(IRBB const* bb, ConstIRIter & it)
{
    DefSBitSetCore * use = gen_use(bb->id());
    DefSBitSetCore * gen = gen_def(bb->id());
    use->clean(m_sbs_mgr);
    gen->clean(m_sbs_mgr);
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter irit;
    for (IR * x = irlst.get_tail(&irit);
         x != nullptr; x = irlst.get_prev(&irit)) {
        computeStmt(x, it, use, gen);
    }
}


void LivenessMgr::computeLocal(IRBB * bb, ConstIRIter & it)
{
    DefSBitSetCore * gen = gen_def(bb->id());
    DefSBitSetCore * use = gen_use(bb->id());
    gen->clean(m_sbs_mgr);
    use->clean(m_sbs_mgr);
    BBIRList & irlst = bb->getIRList();
    for (IR * x = irlst.get_tail(); x != nullptr; x = irlst.get_prev()) {
        ASSERT0(x->is_stmt());
        switch (x->getCode()) {
        case IR_ST:
            it.clean();
            processOpnd(ST_rhs(x), it, use, gen);
            break;
        case IR_STPR:
            processSTPR(x, it, use, gen);
            break;
        case IR_SETELEM:
            processSETELEM(x, it, use, gen);
            break;
        case IR_GETELEM:
            processGETELEM(x, it, use, gen);
            break;
        case IR_STARRAY:
            it.clean();
            processOpnd(x, it, use, gen);
            break;
        case IR_IST:
            it.clean();
            processOpnd(x, it, use, gen);
            break;
        case IR_SWITCH:
            it.clean();
            processOpnd(SWITCH_vexp(x), it, use, gen);
            break;
        case IR_IGOTO:
            it.clean();
            processOpnd(IGOTO_vexp(x), it, use, gen);
            break;
        case IR_GOTO:
            break;
        case IR_CALL:
        case IR_ICALL:
            processCallStmt(x, it, use, gen);
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            it.clean();
            processOpnd(BR_det(x), it, use, gen);
            break;
        case IR_RETURN:
            it.clean();
            processOpnd(RET_exp(x), it, use, gen);
            break;
        case IR_PHI:
            processPHI(x, it, use, gen);
            break;
        case IR_REGION:
            break;
        default: UNREACHABLE();
        }
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
#ifdef STATISTIC_LIVENESS
static UINT g_max_times = 0;
#endif
void LivenessMgr::computeGlobal()
{
    ASSERT0(m_cfg->getEntry() && BB_is_entry(m_cfg->getEntry()));

    //RPO should be available.
    RPOVexList * vlst = m_cfg->getRPOVexList();
    ASSERT0(vlst);
    ASSERT0(vlst->get_elem_count() == m_rg->getBBList()->get_elem_count());

    RPOVexListIter it;
    for (vlst->get_head(&it); it != vlst->end(); it = vlst->get_next(it)) {
        Vertex const* v = it->val();
        IRBB * bb = m_cfg->getBB(v->id());
        ASSERT0(bb);
        UINT bbid = bb->id();
        gen_livein(bbid)->clean(m_sbs_mgr);
        gen_liveout(bbid)->clean(m_sbs_mgr);
    }

    bool change;
    UINT count = 0;
    UINT thres = 1000;
    DefSBitSetCore news;
    do {
        change = false;
        RPOVexListIter ct2;
        for (vlst->get_tail(&ct2);
             ct2 != vlst->end(); ct2 = vlst->get_prev(ct2)) {
            IRBB * bb = m_cfg->getBB(ct2->val()->id());
            ASSERT0(bb);
            UINT bbid = bb->id();
            xcom::DefSBitSetCore * out = get_liveout(bbid);
            xcom::EdgeC const* ec = bb->getVex()->getOutList();
            if (ec != nullptr) {
                news.copy(*get_livein(ec->getToId()), m_sbs_mgr);
                ec = ec->get_next();

                for (; ec != nullptr; ec = ec->get_next()) {
                    news.bunion(*get_livein(ec->getToId()), m_sbs_mgr);
                }

                if (!out->is_equal(news)) {
                    out->copy(news, m_sbs_mgr);
                    change = true;
                }
            }

            //Compute in by out.
            news.copy(*out, m_sbs_mgr);
            ASSERT0(m_def.get(bbid));
            news.diff(*m_def.get(bbid), m_sbs_mgr);
            news.bunion(*m_use.get(bbid), m_sbs_mgr);
            get_livein(bbid)->copy(news, m_sbs_mgr);
        }
        count++;
    } while (change && count < thres);
    ASSERTN(!change, ("result of equation is convergent slowly"));

    news.clean(m_sbs_mgr);

    #ifdef STATISTIC_LIVENESS
    g_max_times = MAX(g_max_times, count);
    FILE * h = fopen("liveness.sat.dump", "a+");
    fprintf(h, "\n%s run %u times, maxtimes %u",
            m_rg->getRegionName(), count, g_max_times);
    fclose(h);
    #endif
}


bool LivenessMgr::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    BBList * bbl = m_rg->getBBList();
    if (bbl->get_elem_count() == 0) { return false; }
    ConstIRIter it;
    C<IRBB*> * ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        //computeLocal(bb, it);
        computeLocal2(bb, it);
    }
    computeGlobal();
    END_TIMER(t, getPassName());
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLivenessMgr()) {
        dump();
    }
    cleanLocal();
    set_valid(true);
    return false;
}
//END LivenessMgr

} //namespace xoc
