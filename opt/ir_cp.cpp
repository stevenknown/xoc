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
//START IR_CP
//
//Return true if ir's type is consistent with 'cand_expr'.
bool IR_CP::checkTypeConsistency(IR const* ir, IR const* cand_expr) const
{
    Type const* t1 = ir->getType();
    Type const* t2 = cand_expr->getType();

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
void IR_CP::replaceExpViaSSADu(
        IR * exp,
        IR const* cand_expr,
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

    ASSERT0(exp->getStmt());
    bool doit = parent->replaceKid(exp, newir, false);
    ASSERT0(doit);
    DUMMYUSE(doit);
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
//stmt_use_ssadu: true if stmt in which cand_expr located used SSA DU info.
//stmt_use_mdssadu: true if stmt in which cand_exp used Memory SSA DU info.
void IR_CP::replaceExp(
        IR * exp,
        IR const* cand_expr,
        IN OUT CPCtx & ctx,
        bool stmt_use_ssadu,
        bool stmt_use_mdssadu,
        MDSSAMgr * mdssamgr)
{
    ASSERT0(exp && exp->is_exp() && cand_expr);
    ASSERT0(exp->getExactRef() ||
            exp->is_id() || //exp is operand of MD PHI
            exp->is_pr());  //exp is operand of PR PHI
    if (!checkTypeConsistency(exp, cand_expr)) {
        return;
    }

    //The memory that 'exp' pointed to is same to 'cand_expr' because
    //cand_expr has been garanteed that will not change in propagation
    //interval.
    //IR * parent = exp->getParent();
    //if (parent->is_ild()) {
    //    CPC_need_recomp_aa(ctx) = true;
    //} else if (parent->is_ist() && exp == IST_base(parent)) {
    //    if (!cand_expr->is_ld() && !cand_expr->is_pr() && !cand_expr->is_lda()) {
    //        return;
    //    }
    //    CPC_need_recomp_aa(ctx) = true;
    //}

    IR * newir = m_ru->dupIRTree(cand_expr);

    //Build DU chain between propagated exp 'newir' and its DEF.
    //TODO: build MDSSA DU chain.
    m_du->copyIRTreeDU(newir, cand_expr, true);

    ASSERT0(cand_expr->getStmt());
    SSAInfo * exp_ssainfo = exp->getSSAInfo();
    MDSSAInfo * exp_mdssainfo = mdssamgr->getMDSSAInfoIfAny(exp);
    if (exp_ssainfo != NULL) {
        //Remove exp SSA use.
        ASSERT0(exp->getSSAInfo());
        ASSERT0(exp->getSSAInfo()->get_uses().find(exp));
        exp->removeSSAUse();
        ASSERT0(exp->getStmt());
        bool doit = exp->getParent()->replaceKid(exp, newir, false);
        ASSERT0(doit);
        DUMMYUSE(doit);
    } else if (exp_mdssainfo != NULL) {
        //Remove exp MD SSA use.
        ASSERT0(mdssamgr);
        mdssamgr->removeMDSSAUseRecur(exp);
        if (exp->is_id()) {
            ASSERT0(ID_phi(exp));
            ID_phi(exp)->replaceOpnd(exp, newir);
        } else {
            bool doit = exp->getParent()->replaceKid(exp, newir, false);
            ASSERT0(doit);
            DUMMYUSE(doit);
        }

        MDSSAInfo * cand_exp_mdssainfo =
            mdssamgr->getMDSSAInfoIfAny(cand_expr);
        if (cand_exp_mdssainfo != NULL) {
            mdssamgr->getUseDefMgr()->setMDSSAInfo(newir, cand_exp_mdssainfo);
            mdssamgr->addMDSSAOcc(newir, cand_exp_mdssainfo);
        }

        //To take care of classic DU info that used/maintained by
        //other pass/phase even if MDSSA enabled, update classic
        //DU chain as well. Nevertheless, some verification
        //might complain.
        m_du->removeUseOutFromDefset(exp);
    } else {
        m_du->removeUseOutFromDefset(exp);
        ASSERT0(exp->getStmt());
        bool doit = exp->getParent()->replaceKid(exp, newir, false);
        ASSERT0(doit);
        DUMMYUSE(doit);
    }
    CPC_change(ctx) = true;
    m_ru->freeIRTree(exp);
}


bool IR_CP::isCopyOR(IR * ir) const
{
    switch (ir->getCode()) {
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
//'def_stmt': ir stmt.
//'prop_value': exp that will be propagated.
//'use_stmt': stmt in use-list of 'def_ir'.
//'use_bb': IRBB that use_stmt be placed in.
bool IR_CP::is_available(
        IR const* def_stmt,
        IR const* prop_value,
        IR * use_stmt,
        MDPhi * use_phi,
        IRBB * usebb)
{
    ASSERT0(usebb);
    ASSERT0(use_stmt || use_phi);

    if (def_stmt == use_stmt) { return false; }
    if (isSimpCVT(prop_value)) {
        prop_value = getSimpCVTValue(prop_value);
        ASSERT0(prop_value);
    }
    if (prop_value->is_const() || prop_value->is_lda()) { return true; }

    //Need to check overlapped MDSet.
    //e.g: Suppose occ is '*p + *q', {p->a}, {q->b}.
    //occ can NOT get reach to 'def_ir' if one of p, q, a, b
    //be modified during the path.
    IRBB * defbb = def_stmt->getBB();
    if (defbb != usebb && !m_cfg->is_dom(defbb->id(), usebb->id())) {
        return false;
    }

    //Both def_ir and use_ir are in same BB.
    xcom::C<IR*> * ir_holder = NULL;
    bool f = BB_irlist(defbb).find(const_cast<IR*>(def_stmt), &ir_holder);
    CHECK_DUMMYUSE(f);
    IR * ir;
    for (ir = BB_irlist(defbb).get_next(&ir_holder);
         ir != NULL && ir != use_stmt;
         ir = BB_irlist(defbb).get_next(&ir_holder)) {
        if (m_du->is_may_def(ir, prop_value, false)) {
            return false;
        }
    }

    if (ir == use_stmt) {
        ASSERTN(defbb == usebb,
                ("def_stmt should be in same bb with use_stmt"));
        return true;
    }

    if (use_phi != NULL || use_stmt->is_phi()) {
        //Propagate value to phi operand.
        //Nothing to do.
    }

    List<IRBB*> wl;
    m_cfg->get_preds(wl, usebb);
    xcom::BitSet visited;
    while (wl.get_elem_count() != 0) {
        IRBB * t = wl.remove_head();
        if (t == defbb) { continue; }

        xcom::C<IR*> * tir_holder = NULL;
        for (IR * tir = BB_irlist(t).get_head(&tir_holder);
             tir != NULL; tir = BB_irlist(t).get_next(&tir_holder)) {
            if (m_du->is_may_def(tir, prop_value, false)) {
                return false;
            }
        }

        visited.bunion(t->id());
        for (xcom::EdgeC * el = VERTEX_in_list(m_cfg->get_vertex(t->id()));
             el != NULL; el = EC_next(el)) {
            INT pred = VERTEX_id(EDGE_from(EC_edge(el)));
            if (!visited.is_contain(pred)) {
                wl.append_tail(m_cfg->getBB(pred));
            }
        }
    }

    return true;
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


//CVT with simply cvt-exp is copy-propagate candidate.
IR const* IR_CP::getSimpCVTValue(IR const* ir) const
{
    if (!ir->is_cvt()) { return NULL; }

    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
        } else if (ir->is_ld() || ir->is_const() || ir->is_pr()) {
            return ir;
        } else {
            break;
        }
    }
    return NULL;
}


//Get the value expression that to be propagated.
inline static IR * get_propagated_value(IR * stmt)
{
    switch (stmt->getCode()) {
    case IR_ST: return ST_rhs(stmt);
    case IR_STPR: return STPR_rhs(stmt);
    case IR_IST: return IST_rhs(stmt);
    case IR_PHI: return PHI_opnd_list(stmt);
    default:;
    }
    UNREACHABLE();
    return NULL;
}


//'usevec': for local used.
bool IR_CP::doPropToMDPhi(
        bool prssadu,
        bool mdssadu,
        IN IR const* prop_value,
        IN IR * use,
        MDSSAMgr * mdssamgr)
{
    CPCtx lchange;
    replaceExp(use, prop_value, lchange, prssadu, mdssadu, mdssamgr);
    return CPC_change(lchange);
}


//'usevec': for local used.
bool IR_CP::doPropToNormalStmt(
        xcom::C<IR*> * cur_iter,
        xcom::C<IR*> ** next_iter,
        bool prssadu,
        bool mdssadu,
        IN IR const* prop_value,
        IN IR * use,
        IN IR * use_stmt,
        IN IRBB * def_bb,
        IN OUT IRBB * use_bb,
        MDSSAMgr * mdssamgr)
{
    bool change = false;
    CPCtx lchange;
    IR * old_use_stmt = use_stmt;
    replaceExp(use, prop_value, lchange, prssadu, mdssadu, mdssamgr);
    ASSERTN(use_stmt->is_stmt(), ("ensure use_stmt still legal"));
    if (!CPC_change(lchange)) { return false; }

    //Indicate whether use_stmt is the next stmt of def_stmt.
    bool is_next = false;
    if (*next_iter != NULL && use_stmt == (*next_iter)->val()) {
        is_next = true;
    }

    RefineCtx rf;
    use_stmt = m_ru->refineIR(use_stmt, change, rf);
    if (use_stmt == NULL && is_next) {
        //use_stmt has been optimized and removed by refineIR().
        *next_iter = cur_iter;
        BB_irlist(def_bb).get_next(next_iter);
    }

    if (use_stmt != NULL && use_stmt != old_use_stmt) {
        //use_stmt has been removed and new stmt generated.
        ASSERTN(old_use_stmt->is_undef(), ("the old one should be freed"));

        xcom::C<IR*> * irct = NULL;
        BB_irlist(use_bb).find(old_use_stmt, &irct);
        ASSERT0(irct);
        BB_irlist(use_bb).insert_before(use_stmt, irct);
        BB_irlist(use_bb).remove(irct);
    }
    return true;
}


void IR_CP::dumpCopyPropagationAction(
        IR const* def_stmt,
        IR const* prop_value,
        IR const* use,
        MDSSAMgr * mdssamgr)
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP CopyPropagation ----==");
    note("\nPropagating Candidate is %s(id:%d), that located in Stmt:",
        IRNAME(prop_value), prop_value->id());
    dumpIR(def_stmt, m_ru);
    note("\nWill replace %s(id:%d), that located in Stmt:",
        IRNAME(use), use->id());
    if (use->is_id()) {
        ASSERT0(mdssamgr);
        MDSSAInfo * mdssainfo = mdssamgr->getUseDefMgr()->getMDSSAInfo(use);
		CHECK_DUMMYUSE(mdssainfo);
        ASSERT0(ID_phi(use));
        note("\n");
        ID_phi(use)->dump(m_ru, mdssamgr->getUseDefMgr());
    } else {
        ASSERT0(use->getStmt());
        dumpIR(use->getStmt(), m_ru);
    }
    fflush(g_tfile);
}


//bool IR_CP::isAllVMDReachAllUse(
//        IR * ir,
//        MDSSAInfo * mdssainfo,
//        IN DefSBitSetCore & useset)
//{
//    ASSERT0(ir && ir->isMemoryRef() && mdssainfo);
//    SEGIter * segiter;
//    for (INT i = useset.get_first(&segiter);
//         i != -1; i = useset.get_next(i, &segiter)) {
//        IR * use = m_ru->getIR(i);
//        ASSERT0(use && use->is_exp());
//        if (use->is_id() && !prop_value->is_const()) {
//            //Do NOT propagate non-const value to operand of MD PHI.
//            continue;
//        }
//    }
//}


//'usevec': for local used.
bool IR_CP::doProp(
        IN IRBB * bb,
        IN DefSBitSetCore * useset,
        MDSSAMgr * mdssamgr)
{
    bool change = false;
    xcom::C<IR*> * cur_iter, * next_iter;

    for (BB_irlist(bb).get_head(&cur_iter),
         next_iter = cur_iter; cur_iter != NULL; cur_iter = next_iter) {
        IR * def_stmt = cur_iter->val();
        BB_irlist(bb).get_next(&next_iter);
        if (!isCopyOR(def_stmt)) {
            continue;
        }

        if (def_stmt->getExactRef() == NULL && !def_stmt->isWritePR()) {
            //Do NOT progate value through inexact memory reference, except PR.
            continue;
        }

        IR const* prop_value = get_propagated_value(def_stmt);
        if (!canBeCandidate(prop_value)) {
            continue;
        }

        SSAInfo * ssainfo = def_stmt->getSSAInfo();
        MDSSAInfo * mdssainfo = mdssamgr->getMDSSAInfoIfAny(def_stmt);
        DUSet const* duset = NULL;
        bool prssadu = false;
        bool mdssadu = false;
        useset->clean(*m_ru->getMiscBitSetMgr());
        if (ssainfo != NULL && SSA_uses(ssainfo).get_elem_count() != 0) {
            //Record use_stmt in another vector to facilitate this function
            //if it is not in use-list any more after copy-propagation.
            SEGIter * sc;
            for (INT u = SSA_uses(ssainfo).get_first(&sc);
                 u >= 0; u = SSA_uses(ssainfo).get_next(u, &sc)) {
                IR * use = m_ru->getIR(u);
                ASSERT0(use);
                useset->bunion(use->id(), *m_ru->getMiscBitSetMgr());
            }
            prssadu = true;
        } else if (mdssainfo != NULL &&
                   mdssainfo->readVOpndSet() != NULL &&
                   !mdssainfo->readVOpndSet()->is_empty()) {
            if (def_stmt->getRefMD() == NULL ||
                !def_stmt->getRefMD()->is_exact()) {
                continue;
            }

            mdssainfo->collectUse(useset, mdssamgr->getUseDefMgr(),
                m_ru->getMiscBitSetMgr());
            mdssadu = true;
        } else if ((duset = def_stmt->readDUSet()) != NULL &&
                   duset->get_elem_count() != 0) {
            if (def_stmt->getRefMD() == NULL ||
                !def_stmt->getRefMD()->is_exact()) {
                continue;
            }
            //Record use_stmt in another Set to facilitate this function
            //if it is not in use-list any more after copy-propagation.
            useset->copy(*duset, *m_ru->getMiscBitSetMgr());
        } else {
            continue;
        }

        SEGIter * segiter;
        for (INT i = useset->get_first(&segiter);
             i != -1; i = useset->get_next(i, &segiter)) {
            IR * use = m_ru->getIR(i);
            ASSERT0(use && use->is_exp());
            if (use->is_id() && !prop_value->is_const()) {
                //Do NOT propagate non-const value to operand of MD PHI.
                continue;
            }

            if (use->getExactRef() == NULL && !use->isReadPR()) {
                //Do NOT progate value to inexact memory reference, except PR.
                continue;
            }

            if (mdssainfo != NULL &&
                !mdssainfo->isUseReachable(mdssamgr->getUseDefMgr(), use)) {
                continue;
            }

            IR * use_stmt = NULL;
            MDPhi * use_phi = NULL;
            IRBB * use_bb = NULL;

            if (use->is_id()) {
                ASSERT0(ID_phi(use));
                use_bb = ID_phi(use)->getBB();
                use_phi = ID_phi(use);
                ASSERT0(use_phi && use_phi->is_phi());
            } else {
                use_stmt = use->getStmt();
                ASSERT0(use_stmt->is_stmt() && use_stmt->getBB());
                use_bb = use_stmt->getBB();
                use_phi = NULL;
            }

            if (!is_available(def_stmt, prop_value,
                              use_stmt, use_phi, use_bb)) {
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

            if (!prssadu &&
                !mdssadu &&
                !m_du->isExactAndUniqueDef(def_stmt, use)) {
                //Only single definition is allowed.
                //e.g:
                //    g = 20; //S3
                //    if (...) {
                //        g = 10; //S1
                //    }
                //    ... = g; //S2
                //g can not be propagated since there are
                //more than one definitions are able to get to S2.
                continue;
            }

            if (g_is_dump_after_pass && g_dump_opt.isDumpCP()) {
                dumpCopyPropagationAction(def_stmt, prop_value, use, mdssamgr);
            }

            if (use->is_id()) {
                change |= doPropToMDPhi(prssadu, mdssadu,
                    prop_value, use, mdssamgr);
            } else {
                change |= doPropToNormalStmt(cur_iter, &next_iter, prssadu,
                    mdssadu, prop_value, use, use_stmt, bb, use_bb, mdssamgr);
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
    START_TIMER(t, getPassName());
    ASSERT0(OC_is_cfg_valid(oc));

    m_ru->checkValidAndRecompute(&oc, PASS_DOM, PASS_DU_REF, PASS_UNDEF);

    PRSSAMgr * prssamgr = (PRSSAMgr*)m_ru->getPassMgr()->
        registerPass(PASS_PR_SSA_MGR);
    if (!prssamgr->isSSAConstructed()) {
        prssamgr->construction(oc);
    }

    MDSSAMgr * mdssamgr = (MDSSAMgr*)m_ru->getPassMgr()->
        registerPass(PASS_MD_SSA_MGR);
    if (!mdssamgr->isMDSSAConstructed()) {
        mdssamgr->construction(oc);
    }

    bool change = false;
    IRBB * entry = m_ru->getCFG()->get_entry();
    ASSERTN(entry, ("Not unique entry, invalid Region"));
    xcom::Graph domtree;
    m_cfg->get_dom_tree(domtree);
    List<xcom::Vertex*> lst;
    xcom::Vertex * root = domtree.get_vertex(entry->id());
    m_cfg->sortDomTreeInPreorder(root, lst);
    DefSBitSetCore useset;

    for (xcom::Vertex * v = lst.get_head(); v != NULL; v = lst.get_next()) {
        IRBB * bb = m_cfg->getBB(VERTEX_id(v));
        ASSERT0(bb);
        change |= doProp(bb, &useset, mdssamgr);
    }
    useset.clean(*m_ru->getMiscBitSetMgr());
    if (change) {
        ASSERT0(mdssamgr == NULL || mdssamgr->verify());
        doFinalRefine();
    }
    END_TIMER(t, getPassName());

    if (change) {
        OC_is_expr_tab_valid(oc) = false;
        OC_is_aa_valid(oc) = false;
        OC_is_ref_valid(oc) = true; //already update.
        ASSERT0(m_ru->verifyMDRef());
        if (mdssamgr != NULL && mdssamgr->isMDSSAConstructed()) {
            ASSERT0(mdssamgr->verify());
            OC_is_du_chain_valid(oc) = false; //not update.
        } else {
            //Use classic DU chain.
            m_du->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NONPR_DU);
            OC_is_du_chain_valid(oc) = true; //already update.
        }
        if (prssamgr != NULL && prssamgr->isSSAConstructed()) {
            ASSERT0(verifySSAInfo(m_ru));
            OC_is_du_chain_valid(oc) = false; //not update.
        }
    }
    return change;
}
//END IR_CP

} //namespace xoc
