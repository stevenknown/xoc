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
#include "prssainfo.h"
#include "prdf.h"
#include "ir_ssa.h"
#include "ir_cp.h"

namespace xoc {

//
//START IR_CP
//
//Return true if ir's type is consistent with 'cand_expr'.
bool IR_CP::checkTypeConsistency(IR const* ir, IR const* cand_expr) const
{
    Type const* t1 = ir->get_type();
    Type const* t2 = cand_expr->get_type();

    //Do copy-prog even if data type is VOID.
    if (t1 == t2) { return true; }

    if (t1->is_scalar() && t2->is_scalar()) {
        if (t1->is_signed() ^ t2->is_signed()) {
            //Sign must be consistent.
            return false;
        }
        if (m_tm->get_bytesize(t1) < m_tm->get_bytesize(t2)) {
            //ir size must be equal or great than cand.
            return false;
        }
        return true;
    }

    return false;
}


//Check and replace 'exp' with 'cand_expr' if they are
//equal, and update SSA info. If 'cand_expr' is NOT leaf,
//that will create redundant computation, and
//depends on later Redundancy Elimination to reverse back.
//
//'cand_expr': substitute cand_expr for exp.
//    e.g: exp is pr1 of S2, cand_expr is 10.
//        pr1 = 10 //S1
//        g = pr1 //S2
//    =>
//        pr1 = 10
//        g = 10
//
//NOTE: Do NOT handle stmt.
void IR_CP::replaceExpViaSSADu(IR * exp, IR const* cand_expr,
                                    IN OUT CPCtx & ctx)
{
    ASSERT0(exp && exp->is_exp() && cand_expr && cand_expr->is_exp());
    ASSERT0(exp->getExactRef());

    if (!checkTypeConsistency(exp, cand_expr)) {
        return;
    }

    IR * parent = IR_parent(exp);
    if (parent->is_ild()) {
        CPC_need_recomp_aa(ctx) = true;
    } else if (parent->is_ist() && exp == IST_base(parent)) {
        if (!cand_expr->is_ld() &&
            !cand_expr->is_pr() &&
            !cand_expr->is_lda()) {
            return;
        }
        CPC_need_recomp_aa(ctx) = true;
    }

    IR * newir = m_ru->dupIRTree(cand_expr);

    if (cand_expr->isReadPR() && PR_ssainfo(cand_expr) != NULL) {
        PR_ssainfo(newir) = PR_ssainfo(cand_expr);
        SSA_uses(PR_ssainfo(newir)).append(newir);
    } else {
        m_du->copyIRTreeDU(newir, cand_expr, true);
    }

    //cand_expr may be IR tree. And there might be PR or LD on the tree.
    newir->copyRefForTree(cand_expr, m_ru);

    //Add SSA use for new exp.
    SSAInfo * cand_ssainfo = NULL;
    if ((cand_ssainfo = cand_expr->getSSAInfo()) != NULL) {
        SSA_uses(cand_ssainfo).append(newir);
    }

    //Remove old exp SSA use.
    SSAInfo * exp_ssainfo = exp->getSSAInfo();
    ASSERT0(exp_ssainfo);
    ASSERT0(SSA_uses(exp_ssainfo).find(exp));
    SSA_uses(exp_ssainfo).remove(exp);

    CPC_change(ctx) = true;

    ASSERT0(exp->get_stmt());
    bool doit = parent->replaceKid(exp, newir, false);
    ASSERT0(doit);
    UNUSED(doit);
    m_ru->freeIRTree(exp);
}


//Check and replace 'ir' with 'cand_expr' if they are
//equal, and update DU info. If 'cand_expr' is NOT leaf,
//that will create redundant computation, and
//depends on later Redundancy Elimination to reverse back.
//exp: expression which will be replaced.
//
//cand_expr: substitute cand_expr for exp.
//    e.g: cand_expr is *p, cand_expr_md is MD3
//        *p(MD3) = 10 //p point to MD3
//        ...
//        g = *q(MD3) //q point to MD3
//
//exp_use_ssadu: true if exp used SSA du info.
//
//NOTE: Do NOT handle stmt.
void IR_CP::replaceExp(IR * exp, IR const* cand_expr,
                        IN OUT CPCtx & ctx, bool exp_use_ssadu)
{
    ASSERT0(exp && exp->is_exp() && cand_expr);
    ASSERT0(exp->getExactRef());

    if (!checkTypeConsistency(exp, cand_expr)) {
        return;
    }

    IR * parent = IR_parent(exp);
    if (parent->is_ild()) {
        CPC_need_recomp_aa(ctx) = true;
    } else if (parent->is_ist() && exp == IST_base(parent)) {
        if (!cand_expr->is_ld() && !cand_expr->is_pr() && !cand_expr->is_lda()) {
            return;
        }
        CPC_need_recomp_aa(ctx) = true;
    }

    IR * newir = m_ru->dupIRTree(cand_expr);
    m_du->copyIRTreeDU(newir, cand_expr, true);

    ASSERT0(cand_expr->get_stmt());
    if (exp_use_ssadu) {
        //Remove exp SSA use.
        ASSERT0(exp->getSSAInfo());
        ASSERT0(exp->getSSAInfo()->get_uses().find(exp));

        exp->removeSSAUse();
    } else {
        m_du->removeUseOutFromDefset(exp);
    }
    CPC_change(ctx) = true;

    ASSERT0(exp->get_stmt());
    bool doit = parent->replaceKid(exp, newir, false);
    ASSERT0(doit);
    UNUSED(doit);
    m_ru->freeIRTree(exp);
}


bool IR_CP::is_copy(IR * ir) const
{
    switch (ir->get_code()) {
    case IR_ST:
    case IR_STPR:
    case IR_IST:
        return canBeCandidate(ir->getRHS());
    case IR_PHI:
        if (cnt_list(PHI_opnd_list(ir)) == 1) {
            return true;
        }
    default: break;
    }
    return false;
}


//Return true if 'occ' does not be modified till meeting 'use_ir'.
//e.g:
//    xx = occ  //def_ir
//    ..
//    ..
//    yy = xx  //use_ir
//
//'def_ir': ir stmt.
//'occ': opnd of 'def_ir'
//'use_ir': stmt in use-list of 'def_ir'.
bool IR_CP::is_available(IR const* def_ir, IR const* occ, IR * use_ir)
{
    if (def_ir == use_ir) {    return false; }
    if (occ->is_const()) { return true; }

    //Need check overlapped MDSet.
    //e.g: Suppose occ is '*p + *q', p->a, q->b.
    //occ can NOT reach 'def_ir' if one of p, q, a, b
    //modified during the path.

    IRBB * defbb = def_ir->get_bb();
    IRBB * usebb = use_ir->get_bb();
    if (defbb == usebb) {
        //Both def_ir and use_ir are in same BB.
        C<IR*> * ir_holder = NULL;
        bool f = BB_irlist(defbb).find(const_cast<IR*>(def_ir), &ir_holder);
        CK_USE(f);
        IR * ir;
        for (ir = BB_irlist(defbb).get_next(&ir_holder);
             ir != NULL && ir != use_ir;
             ir = BB_irlist(defbb).get_next(&ir_holder)) {
            if (m_du->is_may_def(ir, occ, true)) {
                return false;
            }
        }
        if (ir == NULL) {
            ;//use_ir appears prior to def_ir. Do more check via live_in_expr.
        } else {
            ASSERT(ir == use_ir, ("def_ir should be in same bb to use_ir"));
            return true;
        }
    }

    ASSERT0(use_ir->is_stmt());
    DefDBitSetCore const* availin_expr =
        m_du->getAvailInExpr(BB_id(usebb), NULL);
    ASSERT0(availin_expr);

    if (availin_expr->is_contain(IR_id(occ))) {
        IR * u;
        for (u = BB_first_ir(usebb); u != use_ir && u != NULL;
             u = BB_next_ir(usebb)) {
            //Check if 'u' override occ's value.
            if (m_du->is_may_def(u, occ, true)) {
                return false;
            }
        }
        ASSERT(u != NULL && u == use_ir,
                ("Not find use_ir in bb, may be it has "
                 "been removed by other optimization"));
        return true;
    }
    return false;
}


//CVT with simply cvt-exp is copy-propagate candidate.
bool IR_CP::isSimpCVT(IR const* ir) const
{
    if (!ir->is_cvt()) return false;

    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
        } else if (ir->is_ld() || ir->is_const() || ir->is_pr()) {
            return true;
        } else {
            break;
        }
    }
    return false;
}


//Get the value expression that to be propagated.
inline static IR * get_propagated_value(IR * stmt)
{
    switch (IR_code(stmt)) {
    case IR_ST: return ST_rhs(stmt);
    case IR_STPR: return STPR_rhs(stmt);
    case IR_IST: return IST_rhs(stmt);
    case IR_PHI: return PHI_opnd_list(stmt);
    default:;
    }
    UNREACH();
    return NULL;
}


//'usevec': for local used.
bool IR_CP::doProp(IN IRBB * bb, Vector<IR*> & usevec)
{
    bool change = false;
    C<IR*> * cur_iter, * next_iter;

    for (BB_irlist(bb).get_head(&cur_iter),
         next_iter = cur_iter; cur_iter != NULL; cur_iter = next_iter) {

        IR * def_stmt = cur_iter->val();

        BB_irlist(bb).get_next(&next_iter);

        if (!is_copy(def_stmt)) { continue; }

        DUSet const* useset = NULL;
        UINT num_of_use = 0;
        SSAInfo * ssainfo = NULL;
        bool ssadu = false;
        if ((ssainfo = def_stmt->getSSAInfo()) != NULL &&
            SSA_uses(ssainfo).get_elem_count() != 0) {
            //Record use_stmt in another vector to facilitate this function
            //if it is not in use-list any more after copy-propagation.
            SEGIter * sc;
            for    (INT u = SSA_uses(ssainfo).get_first(&sc);
                 u >= 0; u = SSA_uses(ssainfo).get_next(u, &sc)) {
                IR * use = m_ru->getIR(u);
                ASSERT0(use);
                usevec.set(num_of_use, use);
                num_of_use++;
            }
            ssadu = true;
        } else if (def_stmt->getExactRef() == NULL &&
                   !def_stmt->is_void()) {
            //Allowing copy propagate exact or VOID value.
            continue;
        } else if ((useset = def_stmt->readDUSet()) != NULL &&
                   useset->get_elem_count() != 0) {
            //Record use_stmt in another vector to facilitate this function
            //if it is not in use-list any more after copy-propagation.
            DUIter di = NULL;
            for (INT u = useset->get_first(&di);
                 u >= 0; u = useset->get_next(u, &di)) {
                IR * use = m_ru->getIR(u);
                usevec.set(num_of_use, use);
                num_of_use++;
            }
        } else  {
            continue;
        }

        IR const* prop_value = get_propagated_value(def_stmt);

        for (UINT i = 0; i < num_of_use; i++) {
            IR * use = usevec.get(i);
            ASSERT0(use->is_exp());
            IR * use_stmt = use->get_stmt();
            ASSERT0(use_stmt->is_stmt());

            ASSERT0(use_stmt->get_bb() != NULL);
            IRBB * use_bb = use_stmt->get_bb();
            if (!ssadu &&
                !(bb == use_bb && bb->is_dom(def_stmt, use_stmt, true)) &&
                !m_cfg->is_dom(BB_id(bb), BB_id(use_bb))) {
                //'def_stmt' must dominate 'use_stmt'.
                //e.g:
                //    if (...) {
                //        g = 10; //S1
                //    }
                //    ... = g; //S2
                //g can not be propagted since S1 is not dominate S2.
                continue;
            }

            if (!is_available(def_stmt, prop_value, use_stmt)) {
                //The value that will be propagated can
                //not be killed during 'ir' and 'use_stmt'.
                //e.g:
                //    g = a; //S1
                //    if (...) {
                //        a = ...; //S3
                //    }
                //    ... = g; //S2
                //g can not be propagted since a is killed by S3.
                continue;
            }

            if (!ssadu && !m_du->isExactAndUniqueDef(def_stmt, use)) {
                //Only single definition is allowed.
                //e.g:
                //    g = 20; //S3
                //    if (...) {
                //        g = 10; //S1
                //    }
                //    ... = g; //S2
                //g can not be propagted since there are
                //more than one definitions are able to get to S2.
                continue;
            }

            if (!canBeCandidate(prop_value)) {
                continue;
            }

            CPCtx lchange;
            IR * old_use_stmt = use_stmt;

            replaceExp(use, prop_value, lchange, ssadu);

            ASSERT(use_stmt && use_stmt->is_stmt(),
                    ("ensure use_stmt still legal"));
            change |= CPC_change(lchange);

            if (!CPC_change(lchange)) { continue; }

            //Indicate whether use_stmt is the next stmt of def_stmt.
            bool is_next = false;
            if (next_iter != NULL && use_stmt == next_iter->val()) {
                is_next = true;
            }

            RefineCtx rf;
            use_stmt = m_ru->refineIR(use_stmt, change, rf);
            if (use_stmt == NULL && is_next) {
                //use_stmt has been optimized and removed by refineIR().
                next_iter = cur_iter;
                BB_irlist(bb).get_next(&next_iter);
            }

            if (use_stmt != NULL && use_stmt != old_use_stmt) {
                //use_stmt has been removed and new stmt generated.
                ASSERT(old_use_stmt->is_undef(), ("the old one should be freed"));

                C<IR*> * irct = NULL;
                BB_irlist(use_bb).find(old_use_stmt, &irct);
                ASSERT0(irct);
                BB_irlist(use_bb).insert_before(use_stmt, irct);
                BB_irlist(use_bb).remove(irct);
            }
        } //end for each USE
    } //end for IR
    return change;
}


void IR_CP::doFinalRefine()
{
    RefineCtx rf;
    RC_insert_cvt(rf) = false;
    m_ru->refineBBlist(m_ru->getBBList(), rf);
}


bool IR_CP::perform(OptCtx & oc)
{
    START_TIMER_AFTER();
    ASSERT0(OC_is_cfg_valid(oc));

    if (m_prop_kind == CP_PROP_CONST) {

        m_ru->checkValidAndRecompute(&oc, PASS_DOM, PASS_DU_REF,
            PASS_DU_CHAIN, PASS_UNDEF);
    } else {
        m_ru->checkValidAndRecompute(&oc, PASS_DOM, PASS_DU_REF,
            PASS_LIVE_EXPR, PASS_DU_CHAIN, PASS_UNDEF);
    }

    if (!OC_is_du_chain_valid(oc)) {
        END_TIMER_AFTER(getPassName());
        return false;
    }

    bool change = false;
    IRBB * entry = m_ru->getCFG()->get_entry();
    ASSERT(entry, ("Not unique entry, invalid Region"));
    Graph domtree;
    m_cfg->get_dom_tree(domtree);
    List<Vertex*> lst;
    Vertex * root = domtree.get_vertex(BB_id(entry));
    m_cfg->sortDomTreeInPreorder(root, lst);
    Vector<IR*> usevec;

    for (Vertex * v = lst.get_head(); v != NULL; v = lst.get_next()) {
        IRBB * bb = m_cfg->get_bb(VERTEX_id(v));
        ASSERT0(bb);
        change |= doProp(bb, usevec);
    }

    if (change) {
        doFinalRefine();
        OC_is_expr_tab_valid(oc) = false;
        OC_is_aa_valid(oc) = false;
        OC_is_du_chain_valid(oc) = true; //already update.
        OC_is_ref_valid(oc) = true; //already update.
        ASSERT0(m_ru->verifyMDRef() && 
            m_du->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NOPR_DU));
        ASSERT0(verifySSAInfo(m_ru));
    }

    END_TIMER_AFTER(getPassName());
    return change;
}
//END IR_CP

} //namespace xoc
