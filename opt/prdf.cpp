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
#include "prdf.h"

namespace xoc {

//#define STATISTIC_PRDF

//
//START PRDF
//
void PRDF::dump()
{
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n==---- DUMP PRDF : liveness of PR ----==\n");
    List<IRBB*> * bbl = m_ru->getBBList();
    g_indent = 2;
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        fprintf(g_tfile, "\n\n\n-- BB%d --", BB_id(bb));
        DefSBitSetCore * live_in = get_livein(BB_id(bb));
        DefSBitSetCore * live_out = get_liveout(BB_id(bb));
        DefSBitSetCore * def = get_def(BB_id(bb));
        DefSBitSetCore * use = get_use(BB_id(bb));

        fprintf(g_tfile, "\nLIVE-IN: ");
        live_in->dump(g_tfile);

        fprintf(g_tfile, "\nLIVE-OUT: ");
        live_out->dump(g_tfile);

        fprintf(g_tfile, "\nDEF: ");
        def->dump(g_tfile);

        fprintf(g_tfile, "\nUSE: ");
        use->dump(g_tfile);
    }
    fflush(g_tfile);
}


void PRDF::processMay(
            IR const* pr,
            DefSBitSetCore * gen,
            DefSBitSetCore * use,
            bool is_lhs)
{
    if (!m_handle_may) { return; }

    MDSet const* mds = pr->getRefMDSet();
    if (mds != NULL) {
        MD const* prmd = pr->getExactRef();
        ASSERT0(prmd);
        SEGIter * iter;
        for (INT i = mds->get_first(&iter);
             i >= 0; i = mds->get_next(i, &iter)) {
            MD const* md = m_md_sys->getMD(i);
            ASSERT0(md);
            if (MD_base(md) != MD_base(prmd)) {
                bool find;
                ASSERT0(m_var2pr); //One should initialize m_var2pr.
                UINT prno = m_var2pr->get(MD_base(md), &find);
                ASSERT0(find);
                if (is_lhs) {
                    ASSERT0(gen && use);
                    gen->bunion(prno, m_sbs_mgr);
                    use->diff(prno, m_sbs_mgr);
                } else {
                    ASSERT0(use);
                    use->bunion(prno, m_sbs_mgr);
                }
            }
        }
    }
}


void PRDF::processOpnd(
        IR const* ir,
        List<IR const*> & lst,
        DefSBitSetCore * use,
        DefSBitSetCore * gen)
{
    for (IR const* k = iterInitC(ir, lst);
         k != NULL; k = iterNextC(lst)) {
        if (k->is_pr()) {
            use->bunion(k->getPrno(), m_sbs_mgr);
            processMay(k, gen, use, false);
        }
    }
}


void PRDF::computeLocal(IRBB * bb, List<IR const*> & lst)
{
    DefSBitSetCore * gen = get_def(BB_id(bb));
    DefSBitSetCore * use = get_use(BB_id(bb));
    gen->clean(m_sbs_mgr);
    use->clean(m_sbs_mgr);
    for (IR * x = BB_last_ir(bb); x != NULL; x = BB_prev_ir(bb)) {
        ASSERT0(x->is_stmt());
        switch (x->getCode()) {
        case IR_ST:
            lst.clean();
            processOpnd(ST_rhs(x), lst, use, gen);
            break;
        case IR_STPR:
            gen->bunion(STPR_no(x), m_sbs_mgr);
            use->diff(STPR_no(x), m_sbs_mgr);
            processMay(x, gen, use, true);

            lst.clean();
            processOpnd(STPR_rhs(x), lst, use, gen);
            break;
        case IR_SETELEM:
            gen->bunion(SETELEM_prno(x), m_sbs_mgr);
            use->diff(SETELEM_prno(x), m_sbs_mgr);
            processMay(x, gen, use, true);

            lst.clean();
            processOpnd(SETELEM_rhs(x), lst, use, gen);

            lst.clean();
            processOpnd(SETELEM_ofst(x), lst, use, gen);
            break;
        case IR_GETELEM:
            gen->bunion(GETELEM_prno(x), m_sbs_mgr);
            use->diff(GETELEM_prno(x), m_sbs_mgr);
            processMay(x, gen, use, true);

            lst.clean();
            processOpnd(GETELEM_base(x), lst, use, gen);

            lst.clean();
            processOpnd(GETELEM_ofst(x), lst, use, gen);
            break;
        case IR_STARRAY:
            lst.clean();
            processOpnd(x, lst, use, gen);
            break;
        case IR_IST:
            lst.clean();
            processOpnd(x, lst, use, gen);
            break;
        case IR_SWITCH:
            lst.clean();
            processOpnd(SWITCH_vexp(x), lst, use, gen);
            break;
        case IR_IGOTO:
            lst.clean();
            processOpnd(IGOTO_vexp(x), lst, use, gen);
            break;
        case IR_GOTO: break;
        case IR_CALL:
        case IR_ICALL:
            if (x->hasReturnValue()) {
                gen->bunion(CALL_prno(x), m_sbs_mgr);
                use->diff(CALL_prno(x), m_sbs_mgr);
                processMay(x, gen, use, true);
            }

            lst.clean();
            processOpnd(CALL_param_list(x), lst, use, gen);

            if (x->is_icall() && ICALL_callee(x)->is_pr()) {
                use->bunion(PR_no(ICALL_callee(x)), m_sbs_mgr);
                processMay(ICALL_callee(x), gen, use, false);
            }
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            lst.clean();
            processOpnd(BR_det(x), lst, use, gen);
            break;
        case IR_RETURN:
            lst.clean();
            processOpnd(RET_exp(x), lst, use, gen);
            break;
        case IR_PHI:
            gen->bunion(PHI_prno(x), m_sbs_mgr);
            use->diff(PHI_prno(x), m_sbs_mgr);
            processMay(x, gen, use, true);

            lst.clean();
            processOpnd(PHI_opnd_list(x), lst, use, gen);
            break;
        case IR_REGION: break;
        default: UNREACHABLE();
        }
    }
}


//Note this function does not consider PHI effect.
//e.g:
//BB1:             BB2:
//st $4 = 0        st $3 = 1
//     \               /
//      \             /
//      BB3:
//      phi $5   =  $4, $4
//      ...
//
//In actually , $4 only lived out from BB1, and $3 only lived out
//from BB2. For present, $4 both live out from BB1 and BB2, and $3
//is similar.
#ifdef STATISTIC_PRDF
static UINT g_max_times = 0;
#endif
void PRDF::computeGlobal()
{
    ASSERT0(m_cfg->get_entry() && BB_is_entry(m_cfg->get_entry()));

    //Rpo should be available.
    List<IRBB*> * vlst = m_cfg->getBBListInRPO();
    ASSERT0(vlst->get_elem_count() == m_ru->getBBList()->get_elem_count());

    C<IRBB*> * ct;
    for (vlst->get_head(&ct); ct != vlst->end(); ct = vlst->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        UINT bbid = BB_id(bb);
        get_livein(bbid)->clean(m_sbs_mgr);
        get_liveout(bbid)->clean(m_sbs_mgr);
    }

    bool change;
    UINT count = 0;
    UINT thres = 1000;
    DefSBitSetCore news;
    do {
        change = false;
        C<IRBB*> * ct2;
        for (vlst->get_tail(&ct2);
             ct2 != vlst->end(); ct2 = vlst->get_prev(ct2)) {
            IRBB * bb = ct2->val();
            ASSERT0(bb);
            UINT bbid = BB_id(bb);

            DefSBitSetCore * out = m_liveout.get(bbid);
            news.copy(*out, m_sbs_mgr);

            ASSERT0(m_def.get(bbid));
            news.diff(*m_def.get(bbid), m_sbs_mgr);
            news.bunion(*m_use.get(bbid), m_sbs_mgr);
            m_livein.get(bbid)->copy(news, m_sbs_mgr);

            xcom::EdgeC const* ec = VERTEX_out_list(
                m_cfg->get_vertex(BB_id(bb)));
            if (ec != NULL) {
                INT succ = VERTEX_id(EDGE_to(EC_edge(ec)));
                news.copy(*m_livein.get(succ), m_sbs_mgr);
                ec = EC_next(ec);

                for (; ec != NULL; ec = EC_next(ec)) {
                    news.bunion(*m_livein.get(succ), m_sbs_mgr);
                }

                if (!out->is_equal(news)) {
                    out->copy(news, m_sbs_mgr);
                    change = true;
                }
            }
        }
        count++;
    } while (change && count < thres);
    ASSERT(!change, ("result of equation is convergent slowly"));

    news.clean(m_sbs_mgr);

    #ifdef STATISTIC_PRDF
    g_max_times = MAX(g_max_times, count);
    FILE * h = fopen("prdf.sat.dump", "a+");
    fprintf(h, "\n%s run %u times, maxtimes %u",
            m_ru->getRegionName(), count, g_max_times);
    fclose(h);
    #endif
}


bool PRDF::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_ru->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
    List<IRBB*> * bbl = m_ru->getBBList();
    if (bbl->get_elem_count() == 0) { return false; }

    List<IR const*> lst;
    C<IRBB*> * ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        computeLocal(bb, lst);
    }

    computeGlobal();

    //dump();
    END_TIMER(t, getPassName());
    return false;
}
//END PRDF

} //namespace xoc
