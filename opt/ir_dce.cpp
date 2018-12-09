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
#include "ir_dce.h"
#include "prdf.h"
#include "prssainfo.h"
#include "ir_ssa.h"

namespace xoc {

//
//START IR_DCE
//
void IR_DCE::dump(EFFECT_STMT const& is_stmt_effect,
                  xcom::BitSet const& is_bb_effect,
                  IN Vector<Vector<IR*>*> & all_ir)
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP IR_DCE ----==\n");
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        note("\n------- BB%d", BB_id(bb));
        if (!is_bb_effect.is_contain(BB_id(bb))) {
            prt("\t\tineffect BB!");
        }
        Vector<IR*> * ir_vec = all_ir.get(BB_id(bb));
        if (ir_vec == NULL) {
            continue;
        }
        for (INT j = 0; j <= ir_vec->get_last_idx(); j++) {
            IR * ir = ir_vec->get(j);
            ASSERT0(ir != NULL);
            note("\n");
            dumpIR(ir, m_ru);
            if (!is_stmt_effect.is_contain(ir->id())) {
                prt("\t\tremove!");
            }
        }
    }

    note("\n\n============= REMOVED IR ==================\n");
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        note("\n------- BB%d", BB_id(bb));
        if (!is_bb_effect.is_contain(BB_id(bb))) {
            prt("\t\tineffect BB!");
        }
        Vector<IR*> * ir_vec = all_ir.get(BB_id(bb));
        if (ir_vec == NULL) {
            continue;
        }
        for (INT j = 0; j <= ir_vec->get_last_idx(); j++) {
            IR * ir = ir_vec->get(j);
            ASSERT0(ir != NULL);
            if (!is_stmt_effect.is_contain(ir->id())) {
                note("\n");
                dumpIR(ir, m_ru);
                prt("\t\tremove!");
            }
        }
    }
    fflush(g_tfile);
}


//Return true if ir can be optimized.
bool IR_DCE::check_stmt(IR const* ir)
{
    if (ir->isMayThrow() || ir->hasSideEffect() || ir->isNoMove()) {
        return true;
    }

    if (!m_is_use_md_du && ir->isMemoryRefNotOperatePR()) {
        return true;
    }

    MD const* mustdef = ir->getRefMD();
    if (mustdef != NULL) {
        if (is_effect_write(mustdef->get_base())) {
            return true;
        }
    } else {
        MDSet const* maydefs = ir->getRefMDSet();
        if (maydefs != NULL) {
            SEGIter * iter;
            for (INT i = maydefs->get_first(&iter);
                 i >= 0; i = maydefs->get_next(i, &iter)) {
                MD * md = m_md_sys->getMD(i);
                ASSERT0(md);
                if (is_effect_write(md->get_base())) {
                    return true;
                }
            }
        }
    }

    m_citer.clean();
    for (IR const* x = iterRhsInitC(ir, m_citer);
         x != NULL; x = iterRhsNextC(m_citer)) {
        if (!m_is_use_md_du && x->isMemoryRefNotOperatePR()) {
            return true;
        }

        if (!x->isMemoryRef()) { continue; }

        //Check if using volatile variable.
        //e.g: volatile int g = 0;
        //    while(g); # The stmt has effect.
        MD const* md = x->getRefMD();
        if (md != NULL) {
            if (is_effect_read(md->get_base())) {
                return true;
            }
        } else {
            MDSet const* mds = x->getRefMDSet();
            if (mds != NULL) {
                SEGIter * iter;
                for (INT i = mds->get_first(&iter);
                     i != -1; i = mds->get_next(i, &iter)) {
                    MD * md2 = m_md_sys->getMD(i);
                    ASSERT0(md2 != NULL);
                    if (is_effect_read(md2->get_base())) {
                        return true;
                    }
                }
            }
        }
    }
    return false;
}


//Return true if ir is effect.
bool IR_DCE::check_call(IR const* ir)
{
    ASSERT0(ir->isCallStmt());
    return !ir->isReadOnlyCall() || IR_has_sideeffect(ir) || IR_no_move(ir);
}


void IR_DCE::mark_effect_ir(IN OUT EFFECT_STMT & is_stmt_effect,
                            IN OUT xcom::BitSet & is_bb_effect,
                            IN OUT List<IR const*> & work_list)
{
    List<IRBB*> * bbl = m_ru->getBBList();
    xcom::C<IRBB*> * ct;
    for (IRBB * bb = bbl->get_head(&ct);
         bb != NULL; bb = bbl->get_next(&ct)) {
        for (IR const* ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            switch (ir->getCode()) {
            case IR_RETURN:
                //Do NOT set exit-bb to be effect.
                //That will generate redundant control-flow dependence.
                //CASE:
                //    IF (...)
                //        ...
                //    ENDIF
                //    RETURN //EXIT BB
                //IF clause stmt is redundant code.
                is_bb_effect.bunion(BB_id(bb));
                is_stmt_effect.bunion(ir->id());
                work_list.append_tail(ir);
                break;
            case IR_CALL:
            case IR_ICALL:
                if (check_call(ir)) {
                    is_stmt_effect.bunion(ir->id());
                    is_bb_effect.bunion(BB_id(bb));
                    work_list.append_tail(ir);
                }
                break;
            case IR_TRUEBR:
            case IR_FALSEBR:
            case IR_GOTO:
            case IR_IGOTO:
                if (!m_is_elim_cfs) {
                    is_stmt_effect.bunion(ir->id());
                    is_bb_effect.bunion(BB_id(bb));
                    work_list.append_tail(ir);
                }
                break;
            default:
                if (check_stmt(ir)) {
                    is_stmt_effect.bunion(ir->id());
                    is_bb_effect.bunion(BB_id(bb));
                    work_list.append_tail(ir);
                }
            }
        }
    }
}


bool IR_DCE::find_effect_kid(IN IRBB * bb,
                             IN IR * ir,
                             IN EFFECT_STMT & is_stmt_effect)
{
    ASSERT0(m_cfg && m_cdg);
    ASSERT0(ir->getBB() == bb);
    if (ir->isConditionalBr() || ir->isMultiConditionalBr()) {
        xcom::EdgeC const* ec = VERTEX_out_list(m_cdg->get_vertex(BB_id(bb)));
        while (ec != NULL) {
            IRBB * succ = m_cfg->getBB(VERTEX_id(EDGE_to(EC_edge(ec))));
            ASSERT0(succ != NULL);
            for (IR * r = BB_irlist(succ).get_head();
                 r != NULL; r = BB_irlist(succ).get_next()) {
                if (is_stmt_effect.is_contain(IR_id(r))) {
                    return true;
                }
            }
            ec = EC_next(ec);
        }
    } else if (ir->isUnconditionalBr()) {
        xcom::EdgeC const* ecp = VERTEX_in_list(m_cdg->get_vertex(BB_id(bb)));
        while (ecp != NULL) {
            INT cd_pred = VERTEX_id(EDGE_from(EC_edge(ecp)));
            xcom::EdgeC const* ecs = VERTEX_out_list(m_cdg->get_vertex(cd_pred));
            while (ecs != NULL) {
                INT cd_succ = VERTEX_id(EDGE_to(EC_edge(ecs)));
                IRBB * succ = m_cfg->getBB(cd_succ);
                ASSERTN(succ, ("BB%d does not on CFG", cd_succ));
                for (IR * r = BB_irlist(succ).get_head();
                     r != NULL; r = BB_irlist(succ).get_next()) {
                    if (is_stmt_effect.is_contain(IR_id(r))) {
                        return true;
                    }
                }
                ecs = EC_next(ecs);
            }
            ecp = EC_next(ecp);
        }
    } else {
        UNREACHABLE();
    }
    return false;
}


bool IR_DCE::preserve_cd(IN OUT xcom::BitSet & is_bb_effect,
                         IN OUT EFFECT_STMT & is_stmt_effect,
                         IN OUT List<IR const*> & act_ir_lst)
{
    ASSERT0(m_cfg && m_cdg);
    bool change = false;
    List<IRBB*> lst_2;
    BBList * bbl = m_ru->getBBList();
    xcom::C<IRBB*> * ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        if (is_bb_effect.is_contain(BB_id(bb))) {
            UINT bbid = BB_id(bb);
            //Set control dep bb to be effective.
            ASSERT0(m_cdg->get_vertex(bbid));
            for (xcom::EdgeC const* ec = VERTEX_in_list(m_cdg->get_vertex(bbid));
                 ec != NULL; ec = EC_next(ec)) {
                INT cd_pred = VERTEX_id(EDGE_from(EC_edge(ec)));
                if (!is_bb_effect.is_contain(cd_pred)) {
                    is_bb_effect.bunion(cd_pred);
                    change = true;
                }
            }

            ASSERT0(m_cfg->get_vertex(bbid));
            xcom::EdgeC const* ec = VERTEX_in_list(m_cfg->get_vertex(bbid));
            if (cnt_list(ec) >= 2) {
                ASSERT0(BB_rpo(bb) >= 0);
                UINT bbto = BB_rpo(bb);
                while (ec != NULL) {
                    IRBB * pred =
                        m_cfg->getBB(VERTEX_id(EDGE_from(EC_edge(ec))));
                    ASSERT0(pred);

                    if (BB_rpo(pred) > (INT)bbto &&
                        !is_bb_effect.is_contain(BB_id(pred))) {
                        is_bb_effect.bunion(BB_id(pred));
                        change = true;
                    }
                    ec = EC_next(ec);
                }
            }

            if (BB_irlist(bb).get_elem_count() == 0) { continue; }

            IR * ir = BB_last_ir(bb); //last IR of BB.
            ASSERT0(ir != NULL);
            if ((ir->isConditionalBr() || ir->isMultiConditionalBr()) &&
                !is_stmt_effect.is_contain(ir->id())) {
                //switch might have multiple succ-BB.
                if (find_effect_kid(bb, ir, is_stmt_effect)) {
                    is_stmt_effect.bunion(ir->id());
                    act_ir_lst.append_tail(ir);
                    change = true;
                }
            }
        }

        //CASE: test_pre1()
        //    GOTO 0xa4f634 id:23
        //        CLABEL (name:L1) 0xa4f5e4 id:22 branch-target
        //    in BB3
        //    BB3 is ineffective, but GOTO can not be removed!
        if (BB_irlist(bb).get_elem_count() == 0) { continue; }

        IR * ir = BB_last_ir(bb); //last IR of BB.
        ASSERT0(ir);

        if (ir->isUnconditionalBr() && !is_stmt_effect.is_contain(ir->id())) {
            if (find_effect_kid(bb, ir, is_stmt_effect)) {
                is_stmt_effect.bunion(ir->id());
                is_bb_effect.bunion(BB_id(bb));
                act_ir_lst.append_tail(ir);
                change = true;
            }
        }
    } //for each BB
    return change;
}


//Iterative record effect IRs, according to DU chain,
//and preserving the control flow dependence.
void IR_DCE::iter_collect(IN OUT EFFECT_STMT & is_stmt_effect,
                          IN OUT xcom::BitSet & is_bb_effect,
                          IN OUT List<IR const*> & work_list)
{
    List<IR const*> work_list2;
    List<IR const*> * pwlst1 = &work_list;
    List<IR const*> * pwlst2 = &work_list2;
    bool change = true;
    List<IRBB*> succs;
    while (change) {
        change = false;
        for (IR const* ir = pwlst1->get_head();
             ir != NULL; ir = pwlst1->get_next()) {
            m_citer.clean();
            for (IR const* x = iterRhsInitC(ir, m_citer);
                 x != NULL; x = iterRhsNextC(m_citer)) {
                if (!x->isMemoryOpnd()) { continue; }

                if (x->isReadPR() && PR_ssainfo(x) != NULL) {
                    IR const* d = PR_ssainfo(x)->get_def();
                    if (d != NULL) {
                        ASSERT0(d->is_stmt());
                        ASSERT0(d->isWritePR() || d->isCallHasRetVal());

                        if (!is_stmt_effect.is_contain(IR_id(d))) {
                            change = true;
                            pwlst2->append_tail(d);
                            is_stmt_effect.bunion(IR_id(d));
                            ASSERT0(d->getBB() != NULL);
                            is_bb_effect.bunion(BB_id(d->getBB()));
                        }
                    }
                } else {
                    DUSet const* defset = x->readDUSet();
                    if (defset == NULL) { continue; }

                    DUIter di = NULL;
                    for (INT i = defset->get_first(&di);
                         i >= 0; i = defset->get_next(i, &di)) {
                        IR const* d = m_ru->getIR(i);
                        ASSERT0(d->is_stmt());
                        if (!is_stmt_effect.is_contain(IR_id(d))) {
                            change = true;
                            pwlst2->append_tail(d);
                            is_stmt_effect.bunion(IR_id(d));
                            ASSERT0(d->getBB() != NULL);
                            is_bb_effect.bunion(BB_id(d->getBB()));
                        }
                    }
                }
            }
        }

        //dumpIRList((IRList&)*pwlst2);
        if (m_is_elim_cfs) {
            change |= preserve_cd(is_bb_effect, is_stmt_effect, *pwlst2);
        }

        //dumpIRList((IRList&)*pwlst2);
        pwlst1->clean();
        List<IR const*> * tmp = pwlst1;
        pwlst1 = pwlst2;
        pwlst2 = tmp;
    } //end while
}


//Fix control flow if BB is empty.
//It will be illegal if empty BB has non-taken branch.
void IR_DCE::fix_control_flow(List<IRBB*> & bblst, List<C<IRBB*>*> & ctlst)
{
    BBList * bbl = m_ru->getBBList();
    xcom::C<IRBB*> * ct = ctlst.get_head();

    xcom::C<IRBB*> * bbct;
    for (bblst.get_head(&bbct); bbct != bblst.end();
         bbct = bblst.get_next(bbct), ct = ctlst.get_next()) {
        IRBB * bb = bbct->val();
        ASSERT0(ct && bb);
        if (BB_irlist(bb).get_elem_count() != 0) { continue; }

        xcom::EdgeC * vout = VERTEX_out_list(m_cfg->get_vertex(BB_id(bb)));
        if (vout == NULL || cnt_list(vout) <= 1) { continue; }

        xcom::C<IRBB*> * next_ct = ct;
        bbl->get_next(&next_ct);
        IRBB * next_bb = NULL;
        if (next_ct != NULL) {
            next_bb = next_ct->val();
        }

        while (vout != NULL) {
            xcom::Edge * e = EC_edge(vout);
            if (EDGE_info(e) != NULL && CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e))) {
                vout = EC_next(vout);
                continue;
            }

            xcom::Vertex * s = EDGE_to(e);
            if (VERTEX_id(s) == BB_id(bb) ||
                (next_bb != NULL && VERTEX_id(s) == BB_id(next_bb))) {
                vout = EC_next(vout);
                continue;
            }

            if (!m_cdg->is_cd(BB_id(bb), VERTEX_id(s))) {
                //See dce.c:lexrun(), bb5 control bb6, but not control bb8.
                //if bb5 is empty, insert goto to bb8.
                IRBB * tgt = m_cfg->getBB(VERTEX_id(s));
                ASSERT0(tgt);

                //Find a normal label as target.
                LabelInfo const* li;
                for (li = tgt->getLabelList().get_head();
                     li != NULL; li = tgt->getLabelList().get_next()) {
                    if (LABEL_INFO_is_catch_start(li) ||
                        LABEL_INFO_is_try_start(li) ||
                        LABEL_INFO_is_try_end(li) ||
                        LABEL_INFO_is_pragma(li)) {
                        continue;
                    }
                    break;
                }
                ASSERT0(li);

                IR * g = m_ru->buildGoto(li);
                BB_irlist(bb).append_tail(g);
                bool change = true;
                xcom::Vertex * bbv = m_cfg->get_vertex(BB_id(bb));
                while (change) {
                    xcom::EdgeC * ec = VERTEX_out_list(bbv);
                    change = false;
                    while (ec != NULL) {
                        if (EC_edge(ec) != e) {
                            //May be remove multi edges.
                            ((xcom::Graph*)m_cfg)->removeEdgeBetween(
                                EDGE_from(EC_edge(ec)), EDGE_to(EC_edge(ec)));
                            change = true;
                            break;
                        }
                        ec = EC_next(ec);
                    }
                }
                break;
            } else {
                ASSERT0(BB_irlist(m_cfg->getBB(VERTEX_id(s))).
                    get_elem_count() == 0);
            }
            vout = EC_next(vout);
        }
    }
}


void IR_DCE::record_all_ir(IN OUT Vector<Vector<IR*>*> & all_ir)
{
    DUMMYUSE(all_ir);
    #ifdef _DEBUG_
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        if (BB_irlist(bb).get_elem_count() == 0) { continue; }
        Vector<IR*> * ir_vec = new Vector<IR*>();
        all_ir.set(BB_id(bb), ir_vec);
        UINT j = 0;
        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            ir_vec->set(j, ir);
            j++;
        }
    }
    #endif
}


//Fix control flow if BB is empty.
//It is illegal if empty BB has non-taken branch.
void IR_DCE::revise_successor(IRBB * bb, xcom::C<IRBB*> * bbct, BBList * bbl)
{
    ASSERT0(bb && bbct);
    xcom::EdgeC * ec = VERTEX_out_list(m_cfg->get_vertex(BB_id(bb)));
    if (ec == NULL) { return; }

    xcom::C<IRBB*> * next_ct = bbct;
    bbl->get_next(&next_ct);
    IRBB * next_bb = NULL;
    if (next_ct != NULL) {
        next_bb = next_ct->val();
    }

    while (ec != NULL) {
        xcom::Edge * e = EC_edge(ec);
        if (EDGE_info(e) != NULL && CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e))) {
            ec = EC_next(ec);
            continue;
        }

        IRBB * succ_bb = m_cfg->getBB(VERTEX_id(EDGE_to(e)));
        ASSERT0(succ_bb);

        if (succ_bb != next_bb) {
            bb->removeSuccessorPhiOpnd(m_cfg);
        }

        xcom::EdgeC * next_ec = EC_next(ec);
        ((xcom::Graph*)m_cfg)->removeEdge(e);
        ec = next_ec;
    }

    if (next_bb != NULL) {
        m_cfg->addEdge(BB_id(bb), BB_id(next_bb));
    }
}


//An aggressive algo will be used if cdg is avaliable.
bool IR_DCE::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    if (m_is_elim_cfs) {
        m_ru->checkValidAndRecompute(&oc, PASS_DU_REF, PASS_CDG,PASS_PDOM,
            PASS_DU_CHAIN, PASS_CDG, PASS_UNDEF);
        m_cdg = (CDG*)m_ru->getPassMgr()->registerPass(PASS_CDG);
    } else {
        m_ru->checkValidAndRecompute(&oc, PASS_DU_REF, PASS_PDOM,
            PASS_DU_CHAIN, PASS_UNDEF);
        m_cdg = NULL;
    }

    if (!OC_is_du_chain_valid(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }

    //#define _DEBUG_DCE_
    #ifdef _DEBUG_DCE_
    Vector<Vector<IR*>*> all_ir;
    if (g_tfile != NULL) {
        record_all_ir(all_ir);
    }
    #endif

    //Mark effect IRs.
    EFFECT_STMT is_stmt_effect;
    List<IR const*> work_list;
    xcom::BitSet is_bb_effect;

    //Mark effect IRs.
    mark_effect_ir(is_stmt_effect, is_bb_effect, work_list);

    #ifdef _DEBUG_DCE_
    dump(is_stmt_effect, is_bb_effect, all_ir);
    #endif

    iter_collect(is_stmt_effect, is_bb_effect, work_list);

    bool change = false;
    BBList * bbl = m_ru->getBBList();
    xcom::C<IRBB*> * ctbb;
    List<IRBB*> bblst;
    List<C<IRBB*>*> ctlst;
    for (IRBB * bb = bbl->get_head(&ctbb);
         bb != NULL; bb = bbl->get_next(&ctbb)) {
        xcom::C<IR*> * ctir, * next;
        bool tobecheck = false;
        for (BB_irlist(bb).get_head(&ctir), next = ctir;
             ctir != NULL; ctir = next) {
            IR * stmt = ctir->val();
            BB_irlist(bb).get_next(&next);
            if (!is_stmt_effect.is_contain(IR_id(stmt))) {
                //Revise SSA info if PR is in SSA form.
                stmt->removeSSAUse();

                //Revise DU chains.
                //TODO: If ssa form is available, it doesn't need to maintain
                //DU chain of PR in DU manager counterpart.
                m_du->removeIROutFromDUMgr(stmt);

                if (stmt->isConditionalBr() || stmt->isUnconditionalBr() ||
                    stmt->isMultiConditionalBr()) {
                    revise_successor(bb, ctbb, bbl);
                }

                BB_irlist(bb).remove(ctir);

                m_ru->freeIRTree(stmt);

                change = true;

                tobecheck = true;
            }
        }

        if (tobecheck) {
            bblst.append_tail(bb);
            ctlst.append_tail(ctbb);
        }
    }

    //fix_control_flow(bblst, ctlst);

    #ifdef _DEBUG_DCE_
    if (g_tfile != NULL) {
        dump(is_stmt_effect, is_bb_effect, all_ir);
        for (INT i = 0; i <= all_ir.get_last_idx(); i++) {
            Vector<IR*> * ir_vec = all_ir.get(i);
            if (ir_vec != NULL) {
                delete ir_vec;
            }
        }
    }
    #endif

    if (change) {
        m_cfg->performMiscOpt(oc);

        //AA, DU chain and du reference are maintained.
        ASSERT0(m_ru->verifyMDRef() &&
            m_du->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NOPR_DU));
        OC_is_expr_tab_valid(oc) = false;
        OC_is_live_expr_valid(oc) = false;
        OC_is_reach_def_valid(oc) = false;
        OC_is_avail_reach_def_valid(oc) = false;

        ASSERT0(verifySSAInfo(m_ru));
    }

    END_TIMER(t, getPassName());
    return change;
}
//END IR_DCE

} //namespace xoc
