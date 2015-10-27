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
#include "prssainfo.h"
#include "ir_ssa.h"
#include "ir_licm.h"

namespace xoc {

//
//START IR_LICM
//

/* Scan operand to find invariant candidate.
'invariant_stmt': Record if the result of stmt is loop invariant.
'is_legal': set to true if loop is legal to perform invariant motion.
    otherwise set to false to prohibit code motion.
Return true if find loop invariant expression. */
bool IR_LICM::scanOpnd(IN LI<IRBB> * li,
                        OUT TTab<IR*> & invariant_exp,
                        TTab<IR*> & invariant_stmt,
                        bool * is_legal, bool first_scan)
{
    bool change = false;
    IRBB * head = LI_loop_head(li);
    UINT headid = BB_id(head);
    for (INT i = LI_bb_set(li)->get_first();
         i != -1; i = LI_bb_set(li)->get_next(i)) {
        if ((UINT)i == headid) { continue; }

        if (!m_cfg->is_dom(headid, i)) {
            //The candidate BB must dominate all other loop body BBs.
            continue;
        }

        IRBB * bb = m_cfg->get_bb(i);
        ASSERT0(bb && m_cfg->get_vertex(i));
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            if (!ir->isContainMemRef()) { continue; }
            if ((ir->is_calls_stmt() && !ir->is_readonly_call()) ||
                ir->is_region() || ir->is_phi()) {
                //TODO: support call.
                *is_legal = false;
                return false;
            }
            if (first_scan) { updateMD2Num(ir); }

            //Check if rhs is loop invariant.
            bool is_cand = true;
            m_iriter.clean();
            for (IR const* x = iterRhsInitC(ir, m_iriter);
                 x != NULL; x = iterRhsNextC(m_iriter)) {
                if (!x->is_memory_opnd() ||
                    x->is_readonly_exp() ||
                    invariant_exp.find(const_cast<IR*>(x))) {
                    continue;
                }

                if (x->is_pr() && x->get_ssainfo() != NULL) {
                    //Check if SSA def is loop invariant.
                    IR const* def = x->get_ssainfo()->get_def();

                    if (def == NULL) { continue; }

                    ASSERT0(def->is_write_pr() || def->isCallHasRetVal());

                    if (!invariant_stmt.find(const_cast<IR*>(def)) &&
                        li->is_inside_loop(BB_id(def->get_bb()))) {
                        is_cand = false;
                        break;
                    }

                    continue;
                }

                DUSet const* defset = x->get_duset_c();
                if (defset == NULL) { continue; }

                DU_ITER di = NULL;
                for (INT i = defset->get_first(&di);
                     i >= 0; i = defset->get_next(i, &di)) {
                    IR const* d = m_ru->get_ir(i);
                    ASSERT0(d->get_bb() && d->is_stmt());
                    if (!invariant_stmt.find(const_cast<IR*>(d)) &&
                        li->is_inside_loop(BB_id(d->get_bb()))) {
                        is_cand = false;
                        break;
                    }
                }

                if (!is_cand) { break; }
            }

            if (!is_cand) { continue; }

            change |= markExpAndStmt(ir, invariant_exp);
        }
    }
    return change;
}


//Record rhs of ir to be invariant, and record the stmt in
//work list to next round analysis.
//Return true if we find new invariant expression.
bool IR_LICM::markExpAndStmt(IR * ir, TTab<IR*> & invariant_exp)
{
    bool change = false;
    IR * e;
    switch (IR_code(ir)) {
    case IR_ST:
        e = ST_rhs(ir);
        if (!e->is_const_exp() && !e->is_pr()) {
            //e.g: ST(P1, P2), do not move P2 out of loop.
            if (!invariant_exp.find(e)) {
                invariant_exp.append(e);
                change = true;
            }
        }

        if (!VAR_is_volatile(ST_idinfo(ir))) {
            ASSERT0(!m_analysable_stmt_list.find(ir));
            m_analysable_stmt_list.append_tail(ir);
        }
        break;
    case IR_STPR:
        e = STPR_rhs(ir);
        if (!e->is_const_exp() && !e->is_pr()) {
            //e.g: ST(P1, P2), do not move P2 out of loop.
            if (!invariant_exp.find(e)) {
                invariant_exp.append(e);
                change = true;
            }
        }
        ASSERT0(!m_analysable_stmt_list.find(ir));
        m_analysable_stmt_list.append_tail(ir);
        break;
    case IR_IST:
        e = IST_rhs(ir);
        if (!e->is_const_exp() && !e->is_pr()) {
            if (!invariant_exp.find(e)) {
                invariant_exp.append(e);
                change = true;
            }
        }

        e = IST_base(ir);
        if (!e->is_pr() && !e->is_array()) {
            //e.g: IST(a+b, P2), regard a+b as cand.
            if (!invariant_exp.find(e)) {
                invariant_exp.append(e);
                change = true;
            }
        }

        ASSERT0(!m_analysable_stmt_list.find(ir));
        m_analysable_stmt_list.append_tail(ir);
        break;
    case IR_CALL:
    case IR_ICALL:
        {
            //Hoisting CALL out of loop may be unsafe if the loop
            //will never execute.
            IR * param = CALL_param_list(ir);
            while (param != NULL) {
                if (!param->is_const_exp() && !param->is_pr()) {
                    if (!invariant_exp.find(param)) {
                        invariant_exp.append(param);
                        change = true;
                    }
                }
                param = IR_next(param);
            }

            //The result of call may not be loop invariant.
            if (ir->is_readonly_call()) {
                ASSERT0(!m_analysable_stmt_list.find(ir));
                m_analysable_stmt_list.append_tail(ir);
            }
        }
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        e = BR_det(ir);
        if (!BIN_opnd0(e)->is_leaf() ||
            !BIN_opnd1(e)->is_leaf()) {
            if (!invariant_exp.find(e)) {
                invariant_exp.append(e);
                change = true;
            }
        }
        break;
    case IR_SWITCH:
        e = SWITCH_vexp(ir);
        if (!e->is_const_exp() && !e->is_pr()) {
            //e.g: SWITCH(P2), do not move P2 out of loop.
            if (!invariant_exp.find(e)) {
                invariant_exp.append(e);
                change = true;
            }
        }
        break;
    default:;
    }
    return change;
}


//Return true if md is modified in loop only once.
bool IR_LICM::isUniqueDef(MD const* md)
{
    UINT * n = m_md2num.get(md);
    ASSERT0(n);

    if (*n > 1) { return false; }

    MDTab * mdt = m_md_sys->get_md_tab(MD_base(md));
    if (mdt == NULL) { return true; }

    MD const* x = mdt->get_effect_md();
    if (x != NULL && x != md && x->is_overlap(md)) {
        UINT * n = m_md2num.get(x);
        if (*n > 1) { return false; }
    }

    OffsetTab * ofstab = mdt->get_ofst_tab();
    if (ofstab == NULL) { return true; }

    if (ofstab->get_elem_count() == 0) { return true; }

    m_mditer.clean();
    for (MD const* x = ofstab->get_first(m_mditer, NULL);
         x != NULL; x = ofstab->get_next(m_mditer, NULL)) {
        if (x != md && x->is_overlap(md)) {
            UINT * n = m_md2num.get(x);
            if (n != NULL && *n > 1) { return false; }
        }
    }
    return true;
}


//Propagate invariant property to result.
//This operation will generate more invariant.
//'invariant_stmt': Record if the result of stmt is loop invariant.
bool IR_LICM::scanResult(OUT TTab<IR*> & invariant_stmt)
{
    bool change = false;
    for (IR * stmt = m_analysable_stmt_list.remove_head(); stmt != NULL;
         stmt = m_analysable_stmt_list.remove_head()) {
        switch (IR_code(stmt)) {
        case IR_ST:
        case IR_STPR:
            {
                MD const* must = stmt->get_ref_md();
                ASSERT0(must);
                if (isUniqueDef(must) &&
                    !invariant_stmt.find(stmt)) {
                    invariant_stmt.append(stmt);
                    change = true;
                }
            }
            break;
        case IR_IST:
            {
                MD const* must = stmt->get_ref_md();
                if (must != NULL && must->is_effect() &&
                    isUniqueDef(must) &&
                    !invariant_stmt.find(stmt)) {
                    invariant_stmt.append(stmt);
                    change = true;
                }
            }
            break;
        case IR_CALL:
        case IR_ICALL:
            //TODO: hoist readonly call.
            break;
        default: ASSERT0(0); //TODO: support more operations.
        }
    }
    return change;
}


void IR_LICM::updateMD2Num(IR * ir)
{
    switch (IR_code(ir)) {
    case IR_ST:
    case IR_STPR:
        {
            MD const* md = ir->get_ref_md();
            ASSERT0(md);
            UINT * n = m_md2num.get(const_cast<MD*>(md));
            if (n == NULL) {
                n = (UINT*)xmalloc(sizeof(UINT));
                m_md2num.set(md, n);
            }
            (*n)++;
        }
        break;
    case IR_STARRAY:
    case IR_IST:
        {
            MDSet const* mds = NULL;
            MD const* md = ir->get_ref_md();
            if (md != NULL) {
                UINT * n = m_md2num.get(const_cast<MD*>(md));
                if (n == NULL) {
                    n = (UINT*)xmalloc(sizeof(UINT));
                    m_md2num.set(md, n);
                }
                (*n)++;
            } else if ((mds = ir->get_ref_mds()) != NULL) {
                SEGIter * iter;
                for (INT i = mds->get_first(&iter);
                     i >= 0; i = mds->get_next(i, &iter)) {
                    MD * md = m_md_sys->get_md(i);
                    UINT * n = m_md2num.get(md);
                    if (n == NULL) {
                        n = (UINT*)xmalloc(sizeof(UINT));
                        m_md2num.set(md, n);
                    }
                    (*n)++;
                }
            }
        }
        break;
    case IR_CALL:
    case IR_ICALL:
        ASSERT0(ir->is_readonly_call());
        //TODO: support not readonly call.
        break;
    case IR_GOTO:
    case IR_IGOTO:
    case IR_SWITCH:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
        break;
    default: ASSERT0(0); //Unsupport.
    }
}


//Given loop info li, dump the invariant stmt and invariant expression.
void IR_LICM::dump(TTab<IR*> const& invariant_stmt,
                TTab<IR*> const& invariant_exp)
{
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n==---- DUMP LICM Analysis Result ----==\n");
    if (invariant_exp.get_elem_count() > 0) {
        TabIter<IR*> ti;
        fprintf(g_tfile, "-- Invariant Expression (num=%d) -- :",
                invariant_exp.get_elem_count());
        g_indent = 3;
        for (IR * c = invariant_exp.get_first(ti);
             c != NULL; c = invariant_exp.get_next(ti)) {
             dump_ir(c, m_dm);
        }

    }
    fprintf(g_tfile, "\n");
    if (invariant_stmt.get_elem_count() > 0) {
        TabIter<IR*> ti;
        fprintf(g_tfile, "-- Invariant Statement (num=%d) -- :",
                invariant_stmt.get_elem_count());
        g_indent = 3;
        for (IR * c = invariant_stmt.get_first(ti);
             c != NULL; c = invariant_stmt.get_next(ti)) {
             dump_ir(c, m_dm);
        }
    }
}


//Analysis loop invariant expression and stmt.
//Return true if find them, otherwise return false.
bool IR_LICM::analysis(IN LI<IRBB> * li,
                    OUT TTab<IR*> & invariant_stmt,
                    OUT TTab<IR*> & invariant_exp)
{
    invariant_stmt.clean();
    invariant_exp.clean();
    m_analysable_stmt_list.clean();
    m_md2num.clean();

    //Record if the result of stmt is invariant.
    bool change = true;
    bool find = false;
    bool first_scan = true;
    while (change) {
        bool is_legal = true;
        change = scanOpnd(li, invariant_exp, invariant_stmt,
                           &is_legal, first_scan);
        if (!is_legal) {
            invariant_exp.clean();
            return false;
        }

        if (change) {
            find = true;
            if (m_analysable_stmt_list.get_elem_count() > 0) {
                change |= scanResult(invariant_stmt);
            }
            ASSERT0(m_analysable_stmt_list.get_elem_count() == 0);
        } else {
            //Before next round analysis, we must make sure all
            //stmts in this list is invariant or not.
            m_analysable_stmt_list.clean();
        }
        first_scan = false;
    }
    return find;
}


bool IR_LICM::is_stmt_dom_its_use(IR const* stmt, IR const* use,
                                LI<IRBB> const* li, IRBB const* stmtbb) const
{
    IR const* ustmt = use->get_stmt();
    UINT ubbid = BB_id(ustmt->get_bb());
    if (!li->is_inside_loop(ubbid)) { return true; }

    UINT stmtbbid = BB_id(stmtbb);
    if ((stmtbbid != ubbid && m_cfg->is_dom(stmtbbid, ubbid)) ||
        (stmtbbid == ubbid && stmtbb->is_dom(stmt, ustmt, true))) {
        return true;
    }

    return false;
}


//Return true if ir dominate all USE which in loop.
bool IR_LICM::is_dom_all_use_in_loop(IR const* ir, LI<IRBB> * li)
{
    ASSERT0(ir->is_stmt());

    IRBB * irbb = ir->get_bb();

    if (ir->get_ssainfo() != NULL) {
        //Check if SSA def is loop invariant.
        ASSERT0(ir->get_ssainfo()->get_def() == ir);

        SSAInfo * ssainfo = ir->get_ssainfo();
        SSAUseIter iter;
        for (INT i = SSA_uses(ssainfo).get_first(&iter);
             iter != NULL; i = SSA_uses(ssainfo).get_next(i, &iter)) {
            IR const* use = m_ru->get_ir(i);

            if (!use->is_pr()) {
                ASSERT0(!use->is_read_pr());
                continue;
            }

            ASSERT(PR_no(use) == ir->get_prno(), ("prno is unmatch"));
            ASSERT0(PR_ssainfo(use) == ssainfo);

            if (!is_stmt_dom_its_use(ir, use, li, irbb)) {
                return false;
            }
        }
        return true;
    }

    DUSet const* useset = ir->get_duset_c();
    if (useset == NULL) { return true; }

    DU_ITER di = NULL;
    for (INT i = useset->get_first(&di);
         i >= 0; i = useset->get_next(i, &di)) {
        IR const* u = m_ru->get_ir(i);
        ASSERT0(u->is_exp() && u->get_stmt());

        if (!is_stmt_dom_its_use(ir, u, li, irbb)) {
            return false;
        }
    }

    return true;
}


bool IR_LICM::isStmtCanBeHoisted(IR * stmt, IRBB * backedge_bb)
{
    //Loop has multiple exits.
    if (backedge_bb == NULL) { return false; }

    //Stmt is at the dominate path in loop.
    if (stmt->get_bb() != backedge_bb &&
        !m_cfg->is_dom(BB_id(stmt->get_bb()), BB_id(backedge_bb))) {
        return false;
    }
    return true;
}


//Hoist candidate IRs to preheader BB.
bool IR_LICM::hoistCand(TTab<IR*> & invariant_exp,
                         TTab<IR*> & invariant_stmt,
                         IN IRBB * prehead, IN LI<IRBB> * li)
{
    bool du_set_info_changed = false;
    Vector<IR*> removed;
    TabIter<IR*> ti;
    IRBB * backedge_bb = ::findSingleBackedgeStartBB(li, m_cfg);
    while (invariant_exp.get_elem_count() > 0) {
        UINT removednum = 0;
        for (IR * c = invariant_exp.get_first(ti);
             c != NULL; c = invariant_exp.get_next(ti)) {
            ASSERT0(c->is_exp());

            if (!is_worth_hoist(c)) { continue; }

            IR * stmt = c->get_stmt();

            //Check that each definitions of candidate have been
            //already hoisted out of the loop.
            bool do_hoist_now = true;
            m_iriter.clean();
            for (IR const* x = iterInitC(c, m_iriter);
                 x != NULL; x = iterNextC(m_iriter)) {
                if (x->get_ssainfo() != NULL) {
                    //Check if SSA def is loop invariant.
                    IR const* def = x->get_ssainfo()->get_def();

                    if (def == NULL) { continue; }

                    ASSERT0(def->is_stmt());

                    IRBB * dbb = def->get_bb();
                    ASSERT0(dbb);
                    if (li->is_inside_loop(BB_id(dbb))) {
                        do_hoist_now = false;
                        break;
                    }

                    continue;
                }

                 DUSet const* defset = x->get_duset_c();
                if (defset == NULL) { continue; }

                DU_ITER di = NULL;
                for (INT i = defset->get_first(&di);
                     i >= 0; i = defset->get_next(i, &di)) {
                    IR const* d = m_ru->get_ir(i);
                    ASSERT0(d->is_stmt());

                    IRBB * dbb = d->get_bb();
                    ASSERT0(dbb);
                    if (li->is_inside_loop(BB_id(dbb))) {
                        do_hoist_now = false;
                        break;
                    }
                }
            }

            if (!do_hoist_now) { continue; }

            removed.set(removednum, c);
            removednum++;

            if ((stmt->is_st() || stmt->is_stpr()) &&
                c == stmt->get_rhs() &&
                invariant_stmt.find(stmt) &&
                backedge_bb != NULL && //loop only have one exit.
                is_dom_all_use_in_loop(stmt, li)) {
                //isStmtCanBeHoisted(stmt, invariant_stmt, backedge_bb)

                //cand is store value and the result memory object is ID|PR.

                /* Fix bug: If we hoist whole stmt out of loop,
                we should make sure the stmt will be execute at least once
                or never. TRUEBR should be generated and encapsulate
                the hoisted stmt to ensure that.
                    while (a > 0) {
                        a = 10;
                        foo();
                    }
                    =>
                    if (a > 0)  {
                        a = 10;
                    }
                    while (a > 0) {
                        foo();
                    }
                */

                ASSERT0(stmt->get_bb());
                BB_irlist(stmt->get_bb()).remove(stmt);
                BB_irlist(prehead).append_tail_ex(stmt);

                /* The code motion do not modify DU chain info of 'exp' and
                'stmt'. So it is no need to revise DU chain.
                But the live-expr, reach-def, avail-reach-def set
                info of each BB changed. */
            } else {
                /* e.g: given
                    n = cand_exp; //S1

                Generate new stmt S2, change S1 to S3:
                    p1 = cand_exp; //S2
                    n = p1; //S3
                move S2 into prehead BB. */
                IR * t = m_ru->buildPR(IR_dt(c));
                bool f = stmt->replaceKid(c, t, false);
                CK_USE(f);

                IR * stpr = m_ru->buildStorePR(PR_no(t), IR_dt(t), c);

                if (m_is_in_ssa_form) {
                    //Generate SSA DU chain bewteen 'st' and 't'.
                    m_ssamgr->buildDUChain(stpr, t);
                } else {
                    //Generate MD DU chain bewteen 'st' and 't'.
                    m_du->buildDUChain(stpr, t);
                }

                BB_irlist(prehead).append_tail(stpr);

                //Revise MD info.
                MD const* tmd = m_ru->genMDforPR(t);
                t->set_ref_md(tmd, m_ru);
                stpr->set_ref_md(tmd, m_ru);
            }
            du_set_info_changed = true;
        }

        ASSERT(removednum > 0, ("not find any hoistable exp?"));

        for (UINT i = 0; i < removednum; i++) {
            IR * c = removed.get(i);
            ASSERT0(c);
            invariant_exp.remove(c);
        }
    } //end while

    return du_set_info_changed;
}


//Return true if do code motion successfully.
//This funtion maintain LOOP INFO.
bool IR_LICM::doLoopTree(LI<IRBB> * li,
                        OUT bool & du_set_info_changed,
                        OUT bool & insert_bb,
                        TTab<IR*> & invariant_stmt,
                        TTab<IR*> & invariant_exp)
{
    if (li == NULL) { return false; }
    bool doit = false;
    LI<IRBB> * tli = li;
    while (tli != NULL) {
        doLoopTree(LI_inner_list(tli), du_set_info_changed,
                     insert_bb, invariant_stmt, invariant_exp);
        analysis(tli, invariant_stmt, invariant_exp);
        if (invariant_exp.get_elem_count() == 0) {
            tli = LI_next(tli);
            continue;
        }

        doit = true;
        bool flag;
        IRBB * prehead = ::findAndInsertPreheader(tli, m_ru, flag, false);
        ASSERT0(prehead);
        insert_bb |= flag;
        if (flag && LI_outer(tli) != NULL) {
            LI_bb_set(LI_outer(tli))->bunion(BB_id(prehead));
        }

        du_set_info_changed |=
            hoistCand(invariant_exp, invariant_stmt, prehead, li);
        tli = LI_next(tli);
    }
    return doit;
}


bool IR_LICM::perform(OptCTX & oc)
{
    START_TIMER_AFTER();
    m_ru->checkValidAndRecompute(&oc, PASS_DOM, PASS_DU_REF, PASS_LOOP_INFO,
                                    PASS_DU_CHAIN, PASS_UNDEF);

    bool du_set_info_changed = false;
    bool insert_bb = false;
    TTab<IR*> invariant_stmt;
    TTab<IR*> invariant_exp;

    m_is_in_ssa_form = false;
    IR_SSA_MGR * ssamgr =
            (IR_SSA_MGR*)m_ru->get_pass_mgr()->query_opt(PASS_SSA_MGR);
    if (ssamgr != NULL && ssamgr->is_ssa_constructed()) {
        m_is_in_ssa_form = true;
        m_ssamgr = ssamgr;
    } else {
        m_is_in_ssa_form = false;
        m_ssamgr = NULL;
    }

    bool change = doLoopTree(m_cfg->get_loop_info(),
                             du_set_info_changed,
                             insert_bb,
                             invariant_stmt,
                             invariant_exp);
    if (change) {
        m_cfg->performMiscOpt(oc);

        OC_is_expr_tab_valid(oc) = false;

        //DU chain and du ref is maintained.
        ASSERT0(m_du->verifyMDRef());
        ASSERT0(m_du->verifyMDDUChain());

        if (du_set_info_changed) {
            OC_is_live_expr_valid(oc) = false;
            OC_is_avail_reach_def_valid(oc) = false;
            OC_is_reach_def_valid(oc) = false;
        }

        if (insert_bb) {
            //Loop info is maintained, but dom pdom and cdg is changed.

            OC_is_dom_valid(oc) = false;
            OC_is_pdom_valid(oc) = false;
            OC_is_cdg_valid(oc) = false;
        }
    }
    END_TIMER_AFTER(get_pass_name());
    return change;
}
//END IR_LICM

} //namespace xoc
