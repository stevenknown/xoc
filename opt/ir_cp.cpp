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
//START CopyProp
//
//Return true if ir's type is consistent with 'cand_expr'.
bool CopyProp::checkTypeConsistency(IR const* ir, IR const* cand_expr) const
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
        if (m_tm->getByteSize(t1) < m_tm->getByteSize(t2)) {
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
void CopyProp::replaceExpViaSSADu(IR * exp,
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

    IR * newir = m_rg->dupIRTree(cand_expr);

    if (cand_expr->isReadPR() && PR_ssainfo(cand_expr) != nullptr) {
        PR_ssainfo(newir) = PR_ssainfo(cand_expr);
        PR_ssainfo(newir)->addUse(newir);
    } else {
        m_du->addUse(newir, cand_expr);
    }

    //cand_expr may be IR tree. And there might be PR or LD on the tree.
    newir->copyRefForTree(cand_expr, m_rg);

    //Add SSA use for new exp.
    SSAInfo * cand_ssainfo = nullptr;
    if ((cand_ssainfo = cand_expr->getSSAInfo()) != nullptr) {
        cand_ssainfo->addUse(newir);
    }

    //Remove old exp SSA use.
    SSAInfo * exp_ssainfo = exp->getSSAInfo();
    ASSERT0(exp_ssainfo && exp_ssainfo->findUse(exp));
    exp_ssainfo->removeUse(exp);

    CPC_change(ctx) = true;

    ASSERT0(exp->getStmt());
    bool doit = parent->replaceKid(exp, newir, false);
    ASSERT0(doit);
    DUMMYUSE(doit);
    m_rg->freeIRTree(exp);
}


//Update DU info for 'exp', it will be replaced by 'newir'.
void CopyProp::removeExpDUInfo(IR * newir, IR * exp)
{
    SSAInfo * exp_prssainfo = usePRSSADU() ? exp->getSSAInfo() : nullptr;
    MDSSAInfo * exp_mdssainfo = useMDSSADU() ? 
        m_mdssamgr->getMDSSAInfoIfAny(exp) : nullptr;

    if (exp_prssainfo != nullptr) {
        //Remove exp SSA use.
        ASSERT0(exp_prssainfo->getUses().find(exp));
        exp->removeSSAUse();
        ASSERT0(exp->getStmt());
    }

    if (exp_mdssainfo != nullptr) {
        //Remove exp MD SSA use.
        ASSERT0(m_mdssamgr);
        m_mdssamgr->removeMDSSAOcc(exp);
        if (exp->is_id()) {
            ASSERT0(ID_phi(exp));
            ID_phi(exp)->replaceOpnd(exp, newir);
        }
    }

    //To take care of classic DU info that used/maintained by
    //other pass/phase even if MDSSA enabled, update classic
    //DU chain as well. Nevertheless, some verification
    //might complain.
    m_du->removeUseFromDefset(exp);
    ASSERT0(exp->getStmt());
}


void CopyProp::addDUInfoForDupOfCandExp(IR * dup, IR const* cand_exp)
{
    xoc::addUse(dup, cand_exp, m_rg);
}


//Check and replace 'ir' with 'cand_expr' if they are
//equal, and update DU info. If 'cand_expr' is NOT leaf,
//that will create redundant computation, and
//depends on later Redundancy Elimination to reverse back.
//exp: expression which will be replaced.
//cand_expr: substitute cand_expr for exp.
//    e.g: cand_expr is *p, cand_expr_md is MD3
//        *p(MD3) = 10 //p point to MD3
//        ...
//        g = *q(MD3) //q point to MD3
//
//stmt_use_ssadu: true if stmt in which cand_expr located used SSA DU info.
//stmt_use_mdssadu: true if stmt in which cand_exp used Memory SSA DU info.
void CopyProp::replaceExp(IR * exp,
                          IR const* cand_expr,
                          IN OUT CPCtx & ctx,
                          bool stmt_use_ssadu,
                          bool stmt_use_mdssadu)
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
    //    if (!cand_expr->is_ld() &&
    //        !cand_expr->is_pr() &&
    //        !cand_expr->is_lda()) {
    //        return;
    //    }
    //    CPC_need_recomp_aa(ctx) = true;
    //}
    IR * dup = m_rg->dupIRTree(cand_expr);
    ASSERT0(cand_expr->getStmt());
    removeExpDUInfo(dup, exp);
    addDUInfoForDupOfCandExp(dup, cand_expr);
    CPC_change(ctx) = true;

    bool doit = exp->getParent()->replaceKid(exp, dup, false);
    ASSERT0(doit);
    DUMMYUSE(doit);
    m_rg->freeIRTree(exp);
}


bool CopyProp::isCopyOR(IR * ir) const
{
    switch (ir->getCode()) {
    case IR_ST:
    case IR_STPR:
    case IR_IST: {
        if (ir->is_stpr() && !isLowCostExp(ir->getRHS())) {
            //CASE:Propagate LD/ILD through PR may degrade performance, e.g:
            //  pr1 = LD(x)
            //  while (..) {
            //     = pr1 
            //  }
            //====> after propagate LD(x)
            //  pr1 = LD(x)
            //  while (..) {
            //     = LD(x)
            //  }
            //This might lead perform degradation.
            return false;
        }
        return canBeCandidate(ir->getRHS());
       }
    case IR_PHI:
        if (xcom::cnt_list(PHI_opnd_list(ir)) == 1) {
            return true;
        }
    default: break;
    }
    return false;
}


bool CopyProp::existMayDefTillBB(IR const* exp, IRBB const* start,
                                 IRBB const* meetup) const
{
    xcom::List<IRBB const*> wl;
    m_cfg->get_preds(wl, start);
    xcom::TTab<UINT> visited;
    while (wl.get_elem_count() != 0) {
        IRBB const* t = wl.remove_head();
        if (t == meetup) { continue; }

        IRListIter tir_holder = nullptr;
        for (IR const* tir = BB_irlist(t).get_head(&tir_holder);
             tir != nullptr; tir = BB_irlist(t).get_next(&tir_holder)) {
            if (m_du->isMayDef(tir, exp, false)) {
                return true;
            }
        }

        visited.append(t->id());
        for (xcom::EdgeC * el = m_cfg->getVertex(t->id())->getInList();
             el != nullptr; el = el->get_next()) {
            UINT pred = (UINT)el->getFromId();
            if (!visited.find(pred)) {
                wl.append_tail(m_cfg->getBB(pred));
            }
        }
    }
    return false;
}


//Return true if 'prop_value' does not be modified till meeting 'use_stmt'.
//e.g:xx = prop_value //def_stmt
//    ..
//    ..
//    use_bb:
//    yy = xx  //use_stmt|use_phi
//
//'def_stmt': ir stmt.
//'prop_value': expression that will be propagated.
//'use_stmt': stmt in use-list of 'def_stmt'.
//'use_phi': stmt in use-list of 'def_stmt'.
//'use_bb': IRBB that use_stm or use_phi be placed in.
//Note either use_phi or use_stmt is nullptr.
bool CopyProp::is_available(IR const* def_stmt, IR const* prop_value,
                            IR * use_stmt, MDPhi * use_phi, IRBB * usebb)
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
    IRListIter ir_holder = nullptr;
    bool f = BB_irlist(defbb).find(const_cast<IR*>(def_stmt), &ir_holder);
    CHECK0_DUMMYUSE(f);
    IR * ir;
    for (ir = BB_irlist(defbb).get_next(&ir_holder);
         ir != nullptr && ir != use_stmt;
         ir = BB_irlist(defbb).get_next(&ir_holder)) {
        if (m_du->isMayDef(ir, prop_value, false)) {
            return false;
        }
    }

    if (ir == use_stmt) {
        ASSERTN(defbb == usebb,
                ("def_stmt should be in same bb with use_stmt"));
        return true;
    }

    if (use_phi != nullptr || use_stmt->is_phi()) {
        //Propagate value to phi operand.
        //Nothing to do.
    }

    if (existMayDefTillBB(prop_value, usebb, defbb)) {
        return false;
    }

    return true;
}


//Return true if ir is CVT with cvt-exp that always include low-cost
//expression. These low-cost always profitable and may bring up new
//optimization opportunity.
bool CopyProp::isLowCostCVT(IR const* ir) const
{
    if (!ir->is_cvt()) { return false; }

    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
            continue;
        }
        switch (ir->getCode()) {
        case IR_CONST:
        case IR_PR:
        case IR_LDA:
            return true;
        default:
            return false;
        }
    }
    UNREACHABLE();
    return false;
}


//Return true if CVT with simply cvt-exp that can be regard as
//copy-propagate candidate.
bool CopyProp::isSimpCVT(IR const* ir) const
{
    if (!ir->is_cvt()) { return false; }

    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
            continue;
        }
        switch (ir->getCode()) {
        case IR_LD:
        case IR_CONST:
        case IR_PR:
        case IR_LDA:
            return true;
        default:
            return false;
        }
    }
    UNREACHABLE();
    return false;
}


//CVT with simply cvt-exp is copy-propagate candidate.
IR const* CopyProp::getSimpCVTValue(IR const* ir) const
{
    if (!ir->is_cvt()) { return nullptr; }

    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
            continue;
        }

        switch (ir->getCode()) {
        case IR_LD:
        case IR_CONST:
        case IR_PR:
        case IR_LDA:
            return ir;
        default:
            return nullptr;
        }
    }
    UNREACHABLE();
    return nullptr;
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
    return nullptr;
}


//'usevec': for local used.
bool CopyProp::doPropToMDPhi(bool prssadu,
                             bool mdssadu,
                             IN IR const* prop_value,
                             IN IR * use)
{
    CPCtx lchange;
    replaceExp(use, prop_value, lchange, prssadu, mdssadu);
    return CPC_change(lchange);
}


//'usevec': for local used.
bool CopyProp::doPropToNormalStmt(IRListIter cur_iter,
                                  IRListIter * next_iter,
                                  bool prssadu,
                                  bool mdssadu,
                                  IN IR const* prop_value,
                                  IN IR * use,
                                  IN IR * use_stmt,
                                  IN IRBB * def_bb,
                                  IN OUT IRBB * use_bb)
{
    bool change = false;
    CPCtx lchange;
    IR * old_use_stmt = use_stmt;
    replaceExp(use, prop_value, lchange, prssadu, mdssadu);
    ASSERTN(use_stmt->is_stmt(), ("ensure use_stmt still legal"));
    if (!CPC_change(lchange)) { return false; }

    //Indicate whether use_stmt is the next stmt of def_stmt.
    bool is_next = false;
    if (*next_iter != nullptr && use_stmt == (*next_iter)->val()) {
        is_next = true;
    }

    RefineCtx rf;
    use_stmt = m_refine->refineIR(use_stmt, change, rf);
    if (use_stmt == nullptr && is_next) {
        //old_use_stmt has been optimized and removed by refineIR().
        *next_iter = cur_iter;
        BB_irlist(def_bb).get_next(next_iter);
    }

    if (use_stmt != nullptr && use_stmt != old_use_stmt) {
        //old_use_stmt has been removed and new stmt generated.
        ASSERTN(old_use_stmt->is_undef(), ("the old one should be freed"));

        IRListIter irct = nullptr;
        BB_irlist(use_bb).find(old_use_stmt, &irct);
        ASSERTN(irct, ("old one should still left in BB"));
        BB_irlist(use_bb).insert_before(use_stmt, irct);
        BB_irlist(use_bb).remove(irct);
        if (is_next) {
            //Update next_iter to reflect the change of old_use_stmt.
            //e.g: st x = lda y;
            //     ist(x) = 0; #old_use_stmt
            // ====>
            //     st x = lda y;
            //     st y = 0; #use_stmt(new generated)
            *next_iter = cur_iter;
            BB_irlist(def_bb).get_next(next_iter);
        }
    }
    return true;
}


void CopyProp::dumpCopyPropagationAction(IR const* def_stmt,
                                         IR const* prop_value,
                                         IR const* use)
{
    if (!getRegion()->isLogMgrInit()) { return; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    note(getRegion(),
         "\nPROPAGATING CANDIDATE: %s(id:%d) THAT LOCATED IN STMT:",
         IRNAME(prop_value), prop_value->id());

    m_rg->getLogMgr()->incIndent(4);
    dumpIR(def_stmt, m_rg, nullptr,
           IR_DUMP_KID|IR_DUMP_INNER_REGION|IR_DUMP_VAR_DECL);
    m_rg->getLogMgr()->decIndent(4);

    note(getRegion(), "\nWILL REPLACE %s(id:%d) THAT LOCATED IN STMT:",
         IRNAME(use), use->id());

    m_rg->getLogMgr()->incIndent(4);
    if (use->is_id()) {
        ASSERT0(m_mdssamgr);
        MDSSAInfo * mdssainfo = m_mdssamgr->getUseDefMgr()->getMDSSAInfo(use);
        CHECK0_DUMMYUSE(mdssainfo);
        ASSERT0(ID_phi(use));
        note(getRegion(), "\n");
        ID_phi(use)->dump(m_rg, m_mdssamgr->getUseDefMgr());
    } else {
        ASSERT0(use->getStmt());
        dumpIR(use->getStmt(), m_rg, nullptr,
               IR_DUMP_KID|IR_DUMP_INNER_REGION|IR_DUMP_VAR_DECL);
    }
    m_rg->getLogMgr()->decIndent(4);
}


//bool CopyProp::isAllVMDReachAllUse(IR * ir,
//                                   MDSSAInfo * mdssainfo,
//                                   IN DefSBitSetCore & useset)
//{
//    ASSERT0(ir && ir->isMemoryRef() && mdssainfo);
//    DefSBitSetIter segiter;
//    for (INT i = useset.get_first(&segiter);
//         i != -1; i = useset.get_next(i, &segiter)) {
//        IR * use = m_rg->getIR(i);
//        ASSERT0(use && use->is_exp());
//        if (use->is_id() && !prop_value->is_const()) {
//            //Do NOT propagate non-const value to operand of MD PHI.
//            continue;
//        }
//    }
//}


//cur_iter: the iter to current IR.
//next_iter: the iter to next IR in 'bb'. It may be changed.
bool CopyProp::doPropUseSet(IRSet * useset, IR * def_stmt,
                            IR const* prop_value, MDSSAInfo * mdssainfo,
                            IRListIter cur_iter, IRListIter * next_iter,
                            IRBB * bb, bool prssadu, bool mdssadu)
{
    bool change = false;
    IRSetIter it;
    for (INT i = useset->get_first(&it);
         i != -1; i = useset->get_next(i, &it)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use && use->is_exp());
        if (use->is_id()) {
            if (!prop_value->is_const()) {
                //Do NOT propagate non-const value to operand of MD PHI.
                continue;
            }

            //TODO: For now, we will not propagate CONST value to PHI for the
            //time being. Because IR_DCE will remove the
            //'def_stmt' if there is no USE of it. And we should insert an
            //assignment of the CONST value during stripping-PHI of MDSSA.
            //e.g:i1 = 1; //S1
            //    LOOP:
            //    i3 = phi(i1, i2); //S2
            //    i2 = i3 + 1;
            //    truebr LOOP;
            //  After IR_CP, 1 has been propagated to S2:
            //    i1 = 1;
            //    LOOP:
            //    i3 = phi(1, i2);
            //    i2 = i3 + 1;
            //    truebr LOOP;
            //  And after IR_DCE, S1 is removed:
            //    LOOP:
            //    i3 = phi(1, i2);
            //    i2 = i3 + 1;
            //    truebr LOOP;
            // phi stripping should deal with the situation.
            ASSERT0(ID_phi(use) && ID_phi(use)->is_phi());
            continue;
        }

        if (use->getExactRef() == nullptr && !use->isReadPR()) {
            //Do NOT progate value to inexact memory reference, except PR.
            continue;
        }

        if (mdssainfo != nullptr &&
            !mdssainfo->isUseReachable(m_mdssamgr->getUseDefMgr(), use)) {
            continue;
        }

        IR * use_stmt = nullptr;
        MDPhi * use_phi = nullptr;
        IRBB * use_bb = nullptr;

        if (use->is_id()) {
            ASSERT0(ID_phi(use));
            use_bb = ID_phi(use)->getBB();
            use_phi = ID_phi(use);
            ASSERT0(use_phi && use_phi->is_phi());
        } else {
            use_stmt = use->getStmt();
            ASSERT0(use_stmt->is_stmt() && use_stmt->getBB());
            use_bb = use_stmt->getBB();
            use_phi = nullptr;
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

        if (!prssadu && !mdssadu &&
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
            dumpCopyPropagationAction(def_stmt, prop_value, use);
        }

        if (use->is_id()) {
            change |= doPropToMDPhi(prssadu, mdssadu, prop_value,
                                    use);
        } else {
            change |= doPropToNormalStmt(cur_iter, next_iter, prssadu,
                                         mdssadu, prop_value, use,
                                         use_stmt, bb, use_bb);
        }
    } //end for each USE
    return change;
}


//'useset': for local used.
bool CopyProp::doProp(IN IRBB * bb, IN IRSet * useset)
{
    bool change = false;
    IRListIter cur_iter;
    IRListIter next_iter;
    for (BB_irlist(bb).get_head(&cur_iter),
         next_iter = cur_iter; cur_iter != nullptr; cur_iter = next_iter) {
        IR * def_stmt = cur_iter->val();
        BB_irlist(bb).get_next(&next_iter);
        if (!isCopyOR(def_stmt)) {
            continue;
        }

        if (def_stmt->getExactRef() == nullptr && !def_stmt->isWritePR()) {
            //Do NOT progate value through inexact memory reference, except PR.
            continue;
        }

        IR const* prop_value = get_propagated_value(def_stmt);
        if (!canBeCandidate(prop_value)) {
            continue;
        }

        SSAInfo * ssainfo = def_stmt->getSSAInfo();
        MDSSAInfo * mdssainfo = useMDSSADU() ?
            m_mdssamgr->getMDSSAInfoIfAny(def_stmt) : nullptr;
        DUSet const* duset = nullptr;
        bool prssadu = false;
        bool mdssadu = false;
        useset->clean();

        if (ssainfo != nullptr) {
            //Record use_stmt in another vector to facilitate this function
            //if it is not in use-list any more after copy-propagation.
            SSAUseIter sc;
            for (INT u = SSA_uses(ssainfo).get_first(&sc);
                 u >= 0; u = SSA_uses(ssainfo).get_next(u, &sc)) {
                IR * use = m_rg->getIR(u);
                ASSERT0(use);
                useset->bunion(use->id());
            }
            prssadu = true;
        } else if (mdssainfo != nullptr &&
                   mdssainfo->readVOpndSet() != nullptr &&
                   !mdssainfo->readVOpndSet()->is_empty()) {
            if (def_stmt->getRefMD() == nullptr ||
                !def_stmt->getRefMD()->is_exact()) {
                continue;
            }

            mdssainfo->collectUse(m_mdssamgr->getUseDefMgr(), useset);
            mdssadu = true;
        } else if ((duset = def_stmt->readDUSet()) != nullptr &&
                   !duset->is_empty()) {
            if (def_stmt->getRefMD() == nullptr ||
                !def_stmt->getRefMD()->is_exact()) {
                continue;
            }
            //Record use_stmt in another Set to facilitate this function
            //if it is not in use-list any more after copy-propagation.
            useset->copy((DefSBitSetCore&)*duset);
        } else {
            continue;
        }

        change |= doPropUseSet(useset, def_stmt, prop_value, mdssainfo,
                               cur_iter, &next_iter,
                               bb, prssadu, mdssadu);
    } //end for IR
    return change;
}


void CopyProp::doFinalRefine(OptCtx & oc)
{
    RefineCtx rf;
    RC_insert_cvt(rf) = false;
    m_refine->refineBBlist(m_rg->getBBList(), rf, oc);
}


bool CopyProp::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    ASSERT0(oc.is_cfg_valid());
    bool is_org_pr_du_chain_valid = oc.is_pr_du_chain_valid();
    bool is_org_nonpr_du_chain_valid = oc.is_nonpr_du_chain_valid();
    DUMMYUSE(is_org_pr_du_chain_valid);
    DUMMYUSE(is_org_nonpr_du_chain_valid);

    if (!oc.is_ref_valid()) { return false; }
    m_refine = (Refine*)m_rg->getPassMgr()->registerPass(PASS_REFINE);
    m_mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_MD_SSA_MGR);
    m_prssamgr = (PRSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_PR_SSA_MGR);
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //DCE use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //DCE use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);

    bool change = false;
    IRBB * entry = m_rg->getCFG()->getEntry();
    ASSERTN(entry, ("Not unique entry, invalid Region"));
    xcom::Graph domtree;
    m_cfg->get_dom_tree(domtree);
    List<xcom::Vertex*> lst;
    xcom::Vertex * root = domtree.getVertex(entry->id());
    m_cfg->sortDomTreeInPreorder(root, lst);
    IRSet useset(getSegMgr()); //for local used
    for (xcom::Vertex * v = lst.get_head(); v != nullptr; v = lst.get_next()) {
        IRBB * bb = m_cfg->getBB(v->id());
        ASSERT0(bb);
        change |= doProp(bb, &useset);
    }
    useset.clean();

    if (change) {
        ASSERT0(m_mdssamgr == nullptr || m_mdssamgr->verify());
        doFinalRefine(oc);
    }
    END_TIMER(t, getPassName());

    if (!change) { return false; }
    
    OC_is_expr_tab_valid(oc) = false;
    OC_is_aa_valid(oc) = false;
    OC_is_ref_valid(oc) = true; //already update.
    ASSERT0(m_rg->verifyMDRef());
    if (useMDSSADU()) {
        ASSERT0(m_mdssamgr->verify());
        OC_is_nonpr_du_chain_valid(oc) = false; //not update.
    } else {
        //Use classic DU chain.
        ASSERT0(verifyMDDUChain(m_rg));
        OC_is_pr_du_chain_valid(oc) = true; //already update.
        OC_is_nonpr_du_chain_valid(oc) = true; //already update.
    }
    
    if (usePRSSADU()) {
        //Use PRSSA.
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
        OC_is_pr_du_chain_valid(oc) = false; //not update.
    } else {
        //Use classic DU chain.
        //Keep DU chain unchanged.
        ASSERT0(oc.is_pr_du_chain_valid() == is_org_pr_du_chain_valid);
        ASSERT0(oc.is_nonpr_du_chain_valid() == is_org_nonpr_du_chain_valid);
    }    
    return true;
}
//END CopyProp

} //namespace xoc
