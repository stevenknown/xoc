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

//Scan operand to find invariant candidate.
//'invariant_stmt': Record if the result of stmt is loop invariant.
//'is_legal': set to true if loop is legal to perform invariant motion.
//  otherwise set to false to prohibit code motion.
//Return true if find loop invariant expression.
bool IR_LICM::scanOpnd(
        IN LI<IRBB> * li,
        OUT TTab<IR*> & invariant_exp,
        TTab<IR*> & invariant_stmt,
        bool * is_legal,
        bool first_scan)
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

        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->get_vertex(i));
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            if (!ir->isContainMemRef()) { continue; }
            if ((ir->isCallStmt() && !ir->isReadOnlyCall()) ||
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
                if (!x->isMemoryOpnd() ||
                    x->isReadOnlyExp() ||
                    invariant_exp.find(const_cast<IR*>(x))) {
                    continue;
                }

                if (x->is_pr() && x->getSSAInfo() != NULL) {
                    //Check if SSA def is loop invariant.
                    IR const* def = x->getSSAInfo()->get_def();

                    if (def == NULL) { continue; }

                    ASSERT0(def->isWritePR() || def->isCallHasRetVal());

                    if (!invariant_stmt.find(const_cast<IR*>(def)) &&
                        li->is_inside_loop(BB_id(def->getBB()))) {
                        is_cand = false;
                        break;
                    }

                    continue;
                }

                DUSet const* defset = x->readDUSet();
                if (defset == NULL) { continue; }

                DUIter di = NULL;
                for (INT i2 = defset->get_first(&di);
                     i2 >= 0; i2 = defset->get_next(i2, &di)) {
                    IR const* d = m_ru->getIR(i2);
                    ASSERT0(d->getBB() && d->is_stmt());
                    if (!invariant_stmt.find(const_cast<IR*>(d)) &&
                        li->is_inside_loop(BB_id(d->getBB()))) {
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
    switch (ir->get_code()) {
    case IR_ST:
        e = ST_rhs(ir);
        if (!e->isConstExp() && !e->is_pr()) {
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
        if (!e->isConstExp() && !e->is_pr()) {
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
        if (!e->isConstExp() && !e->is_pr()) {
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
            for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
                if (!p->isConstExp() && !p->is_pr()) {
                    if (!invariant_exp.find(p)) {
                        invariant_exp.append(p);
                        change = true;
                    }
                }
            }

            //The result of call may not be loop invariant.
            if (ir->isReadOnlyCall()) {
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
        if (!e->isConstExp() && !e->is_pr()) {
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

    MDTab * mdt = m_md_sys->getMDTab(MD_base(md));
    if (mdt == NULL) { return true; }

    MD const* x = mdt->get_effect_md();
    if (x != NULL && x != md && x->is_overlap(md)) {
        UINT * n2 = m_md2num.get(x);
        if (*n2 > 1) { return false; }
    }

    OffsetTab * ofstab = mdt->get_ofst_tab();
    if (ofstab == NULL) { return true; }

    if (ofstab->get_elem_count() == 0) { return true; }

    m_mditer.clean();
    for (MD const* x2 = ofstab->get_first(m_mditer, NULL);
         x2 != NULL; x2 = ofstab->get_next(m_mditer, NULL)) {
        if (x2 != md && x2->is_overlap(md)) {
            UINT * n2 = m_md2num.get(x2);
            if (n2 != NULL && *n2 > 1) { return false; }
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
                MD const* must = stmt->getRefMD();
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
                MD const* must = stmt->getRefMD();
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
        default: UNREACH(); //TODO: support more operations.
        }
    }
    return change;
}


void IR_LICM::updateMD2Num(IR * ir)
{
    switch (ir->get_code()) {
    case IR_ST:
    case IR_STPR:
        {
            MD const* md = ir->getRefMD();
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
            MD const* md = ir->getRefMD();
            if (md != NULL) {
                UINT * n = m_md2num.get(const_cast<MD*>(md));
                if (n == NULL) {
                    n = (UINT*)xmalloc(sizeof(UINT));
                    m_md2num.set(md, n);
                }
                (*n)++;
            } else if ((mds = ir->getRefMDSet()) != NULL) {
                SEGIter * iter;
                for (INT i = mds->get_first(&iter);
                     i >= 0; i = mds->get_next(i, &iter)) {
                    MD * md2 = m_md_sys->getMD(i);
                    UINT * n = m_md2num.get(md2);
                    if (n == NULL) {
                        n = (UINT*)xmalloc(sizeof(UINT));
                        m_md2num.set(md2, n);
                    }
                    (*n)++;
                }
            }
        }
        break;
    case IR_CALL:
    case IR_ICALL:
        ASSERT0(ir->isReadOnlyCall());
        //TODO: support not readonly call.
        break;
    case IR_GOTO:
    case IR_IGOTO:
    case IR_SWITCH:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
        break;
    default: UNREACH(); //Unsupport.
    }
}


//Given loop info li, dump the invariant stmt and invariant expression.
void IR_LICM::dumpInvariantExpStmt(
        TTab<IR*> const& invariant_stmt,
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
             dump_ir(c, m_tm);
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
             dump_ir(c, m_tm);
        }
    }
}


//Analysis loop invariant expression and stmt.
//Return true if find them, otherwise return false.
bool IR_LICM::analysis(
        IN LI<IRBB> * li,
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


bool IR_LICM::is_stmt_dom_its_use(
        IR const* stmt,
        IR const* use,
        LI<IRBB> const* li,
        IRBB const* stmtbb) const
{
    IR const* ustmt = use->get_stmt();
    UINT ubbid = BB_id(ustmt->getBB());
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

    IRBB * irbb = ir->getBB();

    if (ir->getSSAInfo() != NULL) {
        //Check if SSA def is loop invariant.
        ASSERT0(ir->getSSAInfo()->get_def() == ir);

        SSAInfo * ssainfo = ir->getSSAInfo();
        SSAUseIter iter;
        for (INT i = SSA_uses(ssainfo).get_first(&iter);
             iter != NULL; i = SSA_uses(ssainfo).get_next(i, &iter)) {
            IR const* use = m_ru->getIR(i);

            if (!use->is_pr()) {
                ASSERT0(!use->isReadPR());
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

    DUSet const* useset = ir->readDUSet();
    if (useset == NULL) { return true; }

    DUIter di = NULL;
    for (INT i = useset->get_first(&di);
         i >= 0; i = useset->get_next(i, &di)) {
        IR const* u = m_ru->getIR(i);
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
    if (stmt->getBB() != backedge_bb &&
        !m_cfg->is_dom(BB_id(stmt->getBB()), BB_id(backedge_bb))) {
        return false;
    }
    return true;
}


bool IR_LICM::hoistInvariantStmt(
        TTab<IR*> & invariant_stmt,
        IR * stmt,
        IRBB * prehead,
        IN LI<IRBB> * li)
{
    ASSERT0(stmt->getBB());

    ConstIRIter iriter;
    for (IR const* x = iterRhsInitC(stmt, iriter);
         x != NULL; x = iterRhsNextC(iriter)) {
        if (x->getSSAInfo() != NULL) {
            //Check if SSA def is loop invariant.
            IR * def = SSA_def(x->getSSAInfo());
            if (def == NULL) { continue; }

            if (!checkDefStmt(def, invariant_stmt, prehead, li)) {
                return false;
            }

            //Go ahead and check next kid.
            continue;
        }

        DUSet const* defset = x->readDUSet();
        if (defset == NULL) { continue; }

        DUIter di = NULL;
        for (INT i = defset->get_first(&di);
             i >= 0; i = defset->get_next(i, &di)) {
            IR * d = m_ru->getIR(i);
            ASSERT0(d);

            if (!checkDefStmt(d, invariant_stmt, prehead, li)) {
                return false;
            }
        }
    }

    //OK, stmt can be moved to prehead.
    BB_irlist(stmt->getBB()).remove(stmt);
    BB_irlist(prehead).append_tail(stmt);
    return true;
}


bool IR_LICM::checkDefStmt(
        IR * def,
        TTab<IR*> & invariant_stmt,
        IN IRBB * prehead,
        IN LI<IRBB> * li)
{
    ASSERT0(def->is_stmt());

    IRBB * dbb = def->getBB();
    ASSERT0(dbb);
    if (invariant_stmt.find(def)) {
        if (!li->is_inside_loop(BB_id(dbb)) ||
            hoistInvariantStmt(invariant_stmt, def, prehead, li)) {
            return true;
        }
        return false;
    }

    if (li->is_inside_loop(BB_id(dbb))) {
        return false;
    }

    //def stmt has been moved to prehead.
    return true;
}


//Hoist candidate IRs to preheader BB.
bool IR_LICM::hoistCand(
        TTab<IR*> & invariant_exp,
        TTab<IR*> & invariant_stmt,
        IN IRBB * prehead,
        IN LI<IRBB> * li)
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
            if (!isWorthHoist(c)) { continue; }

            IR * stmt = c->get_stmt();

            //Check that each definitions of candidate have been
            //already hoisted out of the loop.
            bool do_hoist_now = true;
            m_iriter.clean();
            for (IR const* x = iterInitC(c, m_iriter);
                 x != NULL; x = iterNextC(m_iriter)) {
                if (x->getSSAInfo() != NULL) {
                    //Check if SSA def is loop invariant.
                    IR * def = SSA_def(x->getSSAInfo());
                    if (def == NULL) { continue; }

                    if (checkDefStmt(def, invariant_stmt, prehead, li)) {
                        //Go ahead and check next kid.
                        continue;
                    }

                    do_hoist_now = false;
                    break;
                }

                DUSet const* defset = x->readDUSet();
                if (defset == NULL) { continue; }

                DUIter di = NULL;
                for (INT i = defset->get_first(&di);
                     i >= 0; i = defset->get_next(i, &di)) {
                    IR * d = m_ru->getIR(i);
                    ASSERT0(d->is_stmt());

                    if (checkDefStmt(d, invariant_stmt, prehead, li)) {
                        //Go ahead and check next kid.
                        continue;
                    }

                    do_hoist_now = false;
                    break;
                }
            }

            if (!do_hoist_now) { continue; }

            removed.set(removednum, c);
            removednum++;

            if ((stmt->is_st() || stmt->is_stpr()) &&
                c == stmt->getRHS() &&
                invariant_stmt.find(stmt) &&
                backedge_bb != NULL && //loop only have one exit.
                is_dom_all_use_in_loop(stmt, li)) {
                //isStmtCanBeHoisted(stmt, invariant_stmt, backedge_bb)

                //cand is store value and the result memory object is ID|PR.

                //Fix bug: If we hoist whole stmt out of loop,
                //we should make sure the stmt will be execute at least once
                //or never. TRUEBR should be generated and encapsulate
                //the hoisted stmt to ensure that.
                //    while (a > 0) {
                //        a = 10;
                //        foo();
                //    }
                //    =>
                //    if (a > 0)  {
                //        a = 10;
                //    }
                //    while (a > 0) {
                //        foo();
                //    }
                ASSERT0(stmt->getBB());
                BB_irlist(stmt->getBB()).remove(stmt);
                BB_irlist(prehead).append_tail_ex(stmt);

                //The code motion do not modify DU chain info of 'exp' and
                //'stmt'. So it is no need to revise DU chain.
                //But the live-expr, reach-def, avail-reach-def set
                //info of each BB changed.
            } else {
                //e.g: given
                //    n = cand_exp; //S1
                //
                //Generate new stmt S2, change S1 to S3:
                //    p1 = cand_exp; //S2
                //    n = p1; //S3
                //move S2 into prehead BB.
                IR * t = m_ru->buildPR(IR_dt(c));
                bool f = stmt->replaceKid(c, t, false);
                CK_USE(f);

                IR * stpr = m_ru->buildStorePR(PR_no(t), IR_dt(t), c);

                if (m_ssamgr != NULL) {
                    //Generate SSA DU chain bewteen 'st' and 't'.
                    m_ssamgr->buildDUChain(stpr, t);
                } else {
                    //Generate MD DU chain bewteen 'st' and 't'.
                    m_du->buildDUChain(stpr, t);
                }

                BB_irlist(prehead).append_tail(stpr);

                //Revise MD info.
                MD const* tmd = m_ru->genMDforPR(t);
                t->setRefMD(tmd, m_ru);
                stpr->setRefMD(tmd, m_ru);
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
bool IR_LICM::doLoopTree(
        LI<IRBB> * li,
        OUT bool & du_set_info_changed,
        OUT bool & insert_bb,
        TTab<IR*> & invariant_stmt,
        TTab<IR*> & invariant_exp)
{
    if (li == NULL) { return false; }
    bool doit = false;
    for (LI<IRBB> * tli = li; tli != NULL; tli = LI_next(tli)) {
        doit |= doLoopTree(LI_inner_list(tli), du_set_info_changed,
                   insert_bb, invariant_stmt, invariant_exp);
        analysis(tli, invariant_stmt, invariant_exp);
        //dumpInvariantExpStmt(invariant_stmt, invariant_exp);

        if (invariant_exp.get_elem_count() == 0) {
            continue;
        }

        doit = true;
        bool flag;
        IRBB * prehead = ::findAndInsertPreheader(tli, m_ru, flag, false);
        ASSERT0(prehead);
        insert_bb |= flag;
        if (flag && LI_outer(tli) != NULL) {
            //Add preheader to outer loop body.
            LI_bb_set(LI_outer(tli))->bunion(BB_id(prehead));
        }

        du_set_info_changed |=
            hoistCand(invariant_exp, invariant_stmt, prehead, li);
    }
    return doit;
}


bool IR_LICM::perform(OptCtx & oc)
{
    START_TIMER_AFTER();
    m_ru->checkValidAndRecompute(&oc, PASS_DOM, PASS_DU_REF, PASS_LOOP_INFO,
                                 PASS_DU_CHAIN, PASS_UNDEF);

    if (!OC_is_du_chain_valid(oc)) {
        END_TIMER_AFTER(getPassName());
        return false;
    }

    bool du_set_info_changed = false;
    bool insert_bb = false;
    TTab<IR*> invariant_stmt;
    TTab<IR*> invariant_exp;

    m_ssamgr = NULL;
    IR_SSA_MGR * ssamgr =
            (IR_SSA_MGR*)m_ru->getPassMgr()->queryPass(PASS_PR_SSA_MGR);
    if (ssamgr != NULL && ssamgr->isSSAConstructed()) {
        m_ssamgr = ssamgr;
    }

    bool change = doLoopTree(m_cfg->getLoopInfo(),
                             du_set_info_changed,
                             insert_bb,
                             invariant_stmt,
                             invariant_exp);
    if (change) {
        m_cfg->performMiscOpt(oc);

        OC_is_expr_tab_valid(oc) = false;

        //DU chain and du ref is maintained.
        ASSERT0(m_ru->verifyMDRef());
        ASSERT0(m_du->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NOPR_DU));

        if (du_set_info_changed) {
            OC_is_live_expr_valid(oc) = false;
            OC_is_avail_reach_def_valid(oc) = false;
            OC_is_reach_def_valid(oc) = false;
        }

        if (insert_bb) {
            //Loop info is maintained, but dom pdom and cdg is changed.
            OC_is_rpo_valid(oc) = false;
            OC_is_dom_valid(oc) = false;
            OC_is_pdom_valid(oc) = false;
            OC_is_cdg_valid(oc) = false;
        }
    }
    END_TIMER_AFTER(getPassName());
    return change;
}
//END IR_LICM

} //namespace xoc
