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
//START LCSE
//
LCSE::LCSE(Region * rg) : Pass(rg)
{
    ASSERT0(rg != nullptr);
    m_tm = rg->getTypeMgr();
    m_du = m_rg->getDUMgr();
    ASSERT0(m_du && m_tm);
    m_expr_tab = nullptr;
    m_enable_filter = true;
}


//Hoist CSE's computation, and replace its occurrence with the result pr.
IR * LCSE::hoistCse(IN IRBB * bb, IN IR * ir_pos, IN ExprRep * ie)
{
    IRListIter pos_holder = nullptr;
    bool f = BB_irlist(bb).find(ir_pos, &pos_holder);
    ASSERT0_DUMMYUSE(f);
    switch (ir_pos->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    case IR_STPR: {
        //return the pr that hold the cse value.
        IR * ret = nullptr;
        //Move STORE_VAL to temp PR.
        IR * rhs = ir_pos->getRHS();
        ExprRep * tie = m_expr_tab->mapIR2ExprRep(rhs);
        if (tie != nullptr && tie == ie) {
            //e.g: a = 10, expression of store_val is nullptr.
            ret = m_rg->getIRMgr()->buildPR(rhs->getType());
            ret->setRefMD(m_rg->getMDMgr()->genMDForPR(ret), m_rg);
            IR * new_st = m_rg->getIRMgr()->buildStorePR(
                PR_no(ret), ret->getType(), rhs);

            //Insert into IR list of BB.
            BB_irlist(bb).insert_before(new_st, pos_holder);
            ir_pos->setRHS(ret);
        }
        if (ir_pos->is_ist()) {
            //Move MEM ADDR to Temp PR.
            tie = m_expr_tab->mapIR2ExprRep(ir_pos->getBase());
            if (tie != nullptr && tie == ie) {
                if (ret == nullptr) {
                    IR * x = ir_pos->getBase();
                    ret = m_rg->getIRMgr()->buildPR(IR_dt(x));
                    ret->setRefMD(m_rg->getMDMgr()->genMDForPR(ret), m_rg);
                    IR * new_st = m_rg->getIRMgr()->buildStorePR(PR_no(ret),
                        ret->getType(), m_rg->dupIRTree(x));
                    new_st->setRefMD(ret->getRefMD(), m_rg);
                    //Insert into IR list of BB.
                    BB_irlist(bb).insert_before(new_st, pos_holder);
                } else {
                    ret = m_rg->dupIRTree(ret);
                }

                //Replace orignial referenced IR with the new PR.
                ir_pos->setBase(ret);
            }
        }
        return ret;
    }
    case IR_CALL:
    case IR_ICALL: {
        IR * p = CALL_arg_list(ir_pos);
        IR * ret = nullptr; //return the pr that hold the cse value.
        IR * next_parm = nullptr;
        while (p != nullptr) {
            next_parm = p->get_next();
            ExprRep * tie = m_expr_tab->mapIR2ExprRep(p);
            if (tie != nullptr && tie == ie) {
                bool insert_st;
                if (ret == nullptr) {
                    ret = m_rg->getIRMgr()->buildPR(IR_dt(p));
                    ret->setRefMD(m_rg->getMDMgr()->genMDForPR(ret), m_rg);
                    xcom::replace(&CALL_arg_list(ir_pos), p, ret);
                    insert_st = true;
                } else {
                    xcom::replace(&CALL_arg_list(ir_pos), p,
                                  m_rg->dupIRTree(ret));
                    insert_st = false;
                }
                ASSERT0(IR_prev(p) == nullptr && p->get_next() == nullptr);

                if (insert_st) {
                    ASSERT0(ret->getRefMD());
                    IR * new_st = m_rg->getIRMgr()->buildStorePR(PR_no(ret),
                                                     ret->getType(), p);
                    new_st->setRefMD(ret->getRefMD(), m_rg);
                    BB_irlist(bb).insert_before(new_st, pos_holder);
                }
            }
            p = next_parm;
        }
        ir_pos->setParentPointer(false);
        return ret;
    }
    case IR_GOTO:
        break;
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_LABEL:
    case IR_CASE:
        ASSERTN(0, ("TODO"));
        break;
    case IR_TRUEBR:
    case IR_FALSEBR: {
        ExprRep * tie = m_expr_tab->mapIR2ExprRep(BR_det(ir_pos));
        if (tie != nullptr && tie == ie) {
            IR * x = BR_det(ir_pos);
            IR * ret = m_rg->getIRMgr()->buildPR(IR_dt(x));
            ret->setRefMD(m_rg->getMDMgr()->genMDForPR(ret), m_rg);
            IR * new_st = m_rg->getIRMgr()->buildStorePR(PR_no(ret),
                                                         ret->getType(), x);
            new_st->setRefMD(ret->getRefMD(), m_rg);
            BB_irlist(bb).insert_before(new_st, pos_holder);
            BR_det(ir_pos) = ret;
            ir_pos->setParentPointer(false);
            return ret;
        }
        break;
    }
    case IR_IGOTO: {
        ExprRep * tie = m_expr_tab->mapIR2ExprRep(IGOTO_vexp(ir_pos));
        if (tie != nullptr && tie == ie) {
            IR * x = IGOTO_vexp(ir_pos);
            IR * ret = m_rg->getIRMgr()->buildPR(IR_dt(x));
            ret->setRefMD(m_rg->getMDMgr()->genMDForPR(ret), m_rg);
            IR * new_st = m_rg->getIRMgr()->buildStorePR(PR_no(ret),
                                                         ret->getType(), x);
            new_st->setRefMD(ret->getRefMD(), m_rg);
            BB_irlist(bb).insert_before(new_st, pos_holder);
            IGOTO_vexp(ir_pos) = ret;
            ir_pos->setParentPointer(false);
            return ret;
        }
        break;
    }
    case IR_SWITCH: {
        ExprRep * tie = m_expr_tab->mapIR2ExprRep(SWITCH_vexp(ir_pos));
        if (tie != nullptr && tie == ie) {
            IR * x = SWITCH_vexp(ir_pos);
            IR * ret = m_rg->getIRMgr()->buildPR(IR_dt(x));
            ret->setRefMD(m_rg->getMDMgr()->genMDForPR(ret), m_rg);
            IR * new_st = m_rg->getIRMgr()->buildStorePR(PR_no(ret),
                                                         ret->getType(), x);
            new_st->setRefMD(ret->getRefMD(), m_rg);
            BB_irlist(bb).insert_before(new_st, pos_holder);
            SWITCH_vexp(ir_pos) = ret;
            ir_pos->setParentPointer(false);
            return ret;
        }
        break;
    }
    case IR_RETURN: {
        ExprRep * tie = m_expr_tab->mapIR2ExprRep(RET_exp(ir_pos));
        if (tie != nullptr && tie == ie) {
            IR * x = RET_exp(ir_pos);
            IR * ret = m_rg->getIRMgr()->buildPR(IR_dt(x));
            ret->setRefMD(m_rg->getMDMgr()->genMDForPR(ret), m_rg);
            IR * new_st = m_rg->getIRMgr()->buildStorePR(PR_no(ret),
                                                         ret->getType(), x);
            new_st->setRefMD(ret->getRefMD(), m_rg);
            BB_irlist(bb).insert_before(new_st, pos_holder);
            RET_exp(ir_pos) = ret;
            ir_pos->setParentPointer(false);
            return ret;
        }
        ir_pos->setParentPointer(false);
        break;
    }
    default: UNREACHABLE();
    }
    return nullptr;
}


bool LCSE::processBranch(IN IRBB * bb, IN IR * ir,
                         MOD xcom::BitSet & avail_ir_expr,
                         MOD Vector<IR*> & map_expr2avail_pos,
                         MOD Vector<IR*> & map_expr2avail_pr)
{
    ASSERT0(ir->isConditionalBr());
    bool change = false;
    if (!canBeCandidate(BR_det(ir))) { return false; }
    ExprRep * ie = m_expr_tab->mapIR2ExprRep(BR_det(ir));
    if (ie != nullptr) {
        avail_ir_expr.bunion(EXPR_id(ie));
        IR * ir_pos = map_expr2avail_pos.get(EXPR_id(ie));
        if (ir_pos != nullptr) {
            IR * pr = map_expr2avail_pr.get(EXPR_id(ie));
            if (pr == nullptr) {
                //e.g: before:
                //    =a||b
                //    ...
                //    falsebr(a||b)
                //after:
                //    t=a||b
                //    =t
                //    ...
                //    =a||b
                pr = hoistCse(bb, ir_pos, ie);
                ASSERT0(pr != nullptr);
                map_expr2avail_pr.set(EXPR_id(ie), pr);
                change = true;
            }
            BR_det(ir) = m_rg->getIRMgr()->buildJudge(m_rg->dupIRTree(pr));
            ir->setParentPointer(false);
            change = true;
        } else {
            map_expr2avail_pos.set(EXPR_id(ie), ir);
        }
    }
    return change;
}


//Return new IR_PR if 'ie' has been regarded as cse candidate expression.
//e.g:
//        call(a+b, a+b);
//    =>
//        p1 = a+b;
//        call(p1, p1);
//    return p1 as new expression.
//'ie': cse candidate expression indicator.
//'stmt': the stmt contains 'exp'.
IR * LCSE::processExp(IN IRBB * bb, IN ExprRep * ie, IN IR * stmt,
                      MOD xcom::BitSet & avail_ir_expr,
                      MOD Vector<IR*> & map_expr2avail_pos,
                      MOD Vector<IR*> & map_expr2avail_pr)
{
    ASSERT0(ie);
    avail_ir_expr.bunion(EXPR_id(ie));

    //Find which stmt that the expression start to live at.
    IR * ir_pos = map_expr2avail_pos.get(EXPR_id(ie));
    if (ir_pos != nullptr) {
        IR * cse_val_pr = map_expr2avail_pr.get(EXPR_id(ie));
        if (cse_val_pr == nullptr) {
            //e.g:
            //before:
            //    =a||b
            //    ...
            //    falsebr(a||b)
            //after:
            //    t=a||b
            //    =t
            //    ...
            //    =a||b
            cse_val_pr = hoistCse(bb, ir_pos, ie);
            ASSERT0(cse_val_pr != nullptr);
            map_expr2avail_pr.set(EXPR_id(ie), cse_val_pr);
        }
        return cse_val_pr;
    }

    //First meet the expression, and record its stmt.
    map_expr2avail_pos.set(EXPR_id(ie), stmt);
    return nullptr;
}


bool LCSE::canBeCandidate(IR * ir)
{
    ASSERT0(ir);
    if (!m_enable_filter) {
        return ir->isBinaryOp() ||
               ir->is_bnot() ||
               ir->is_lnot() ||
               ir->is_neg();
    }
    ASSERT0(ir->is_exp());
    if (ir->is_ild()) {
        //Avoid perform the opposite behavior to Copy-Propagation.
        //e.g:
        //    ST(P1, ILD(P2))
        //    ST(P3, ILD(P2))
        //    after LCSE, we get:
        //    ST(P1, ILD(P2))
        //    ST(P3, P1)
        //    But CP will progagate P1 because ILD is the copy-prop candidate.
        return false;
    }
    return ir->isBinaryOp() || ir->is_bnot() || ir->is_lnot() || ir->is_neg();
}


bool LCSE::processRHS(IN IRBB * bb, IN IR * ir,
                      MOD xcom::BitSet & avail_ir_expr,
                      MOD xcom::Vector<IR*> & map_expr2avail_pos,
                      MOD xcom::Vector<IR*> & map_expr2avail_pr)
{
    ASSERT0(ir->hasRHS());
    bool change = false;
    IR * rhs = ir->getRHS();
    if (rhs == nullptr || !canBeCandidate(rhs)) {
        return false;
    }
    ExprRep * ie = m_expr_tab->mapIR2ExprRep(rhs);
    if (ie != nullptr) {
        avail_ir_expr.bunion(EXPR_id(ie));
        //e.g: a = 10, expression of store_val is nullptr.
        IR * ir_pos = map_expr2avail_pos.get(EXPR_id(ie));
        if (ir_pos != nullptr) {
            IR * pr = map_expr2avail_pr.get(EXPR_id(ie));
            if (pr == nullptr) {
                //e.g: before:
                //    =a+b
                //    ...
                //        =a+b
                //after:
                //    t=a+b
                //    =t
                //    ...
                //    =a+b
                pr = hoistCse(bb, ir_pos, ie);
                ASSERT0(pr != nullptr);
                map_expr2avail_pr.set(EXPR_id(ie), pr);
                change = true;
            }
            ir->setRHS(m_rg->dupIRTree(pr));
            change = true;
        } else {
            //Record position of IR stmt.
            map_expr2avail_pos.set(EXPR_id(ie), ir);
        }
    }
    if (ir->isIndirectMemOp()) {
        ie = m_expr_tab->mapIR2ExprRep(ir->getBase());
        if (ie != nullptr) {
            avail_ir_expr.bunion(EXPR_id(ie));
            IR * ir_pos = map_expr2avail_pos.get(EXPR_id(ie));
            if (ir_pos != nullptr) {
                IR * pr = map_expr2avail_pr.get(EXPR_id(ie));
                if (pr == nullptr) {
                    //e.g: before:
                    //    =a+b
                    //    ...
                    //        =a+b
                    //after:
                    //    t=a+b
                    //    =t
                    //    ...
                    //    =a+b
                    pr = hoistCse(bb, ir_pos, ie);
                    ASSERT0(pr != nullptr);
                    map_expr2avail_pr.set(EXPR_id(ie), pr);
                    change = true;
                }
                ir->setBase(m_rg->dupIRTree(pr));
                change = true;
            } else {
                map_expr2avail_pos.set(EXPR_id(ie), ir);
            }
        }
    }
    return change;
}


bool LCSE::processParamList(IN IRBB * bb, IN IR * ir,
                            MOD xcom::BitSet & avail_ir_expr,
                            MOD Vector<IR*> & map_expr2avail_pos,
                            MOD Vector<IR*> & map_expr2avail_pr)
{
    bool change = false;
    bool lchange = true;
    while (lchange) {
        //Iterative analyse cse, e.g:
        //  CALL(ADD(x,y), SUB(a,b), ADD(x,y), SUB(a,b))
        IR * p = CALL_arg_list(ir);
        lchange = false;
        while (p != nullptr) {
            if (canBeCandidate(p)) {
                ExprRep * ie = m_expr_tab->mapIR2ExprRep(p);
                IR * newparam = processExp(bb, ie, ir,
                                           avail_ir_expr, map_expr2avail_pos,
                                           map_expr2avail_pr);
                if (newparam != nullptr) {
                    change = true;
                    lchange = true;
                    break;
                }
            }
            p = p->get_next();
        }
    }
    return change;
}


bool LCSE::processUse(IN IRBB * bb, IN IR * ir,
                      MOD xcom::BitSet & avail_ir_expr,
                      MOD Vector<IR*> & map_expr2avail_pos,
                      MOD Vector<IR*> & map_expr2avail_pr)
{
    bool change = false;
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    case IR_STPR:
        change |= processRHS(bb, ir, avail_ir_expr, map_expr2avail_pos,
                             map_expr2avail_pr);
        break;
    SWITCH_CASE_CALL:
        change |= processParamList(bb, ir, avail_ir_expr, map_expr2avail_pos,
                                   map_expr2avail_pr);
        break;
    case IR_GOTO:
        break;
    SWITCH_CASE_CFS_OP:
    case IR_LABEL:
    case IR_CASE:
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP: {
        ASSERT0(ir->getJudgeDet());
        if (!canBeCandidate(ir->getJudgeDet())) { break; }
        ExprRep * ie = m_expr_tab->mapIR2ExprRep(ir->getJudgeDet());
        ASSERT0(ie);
        IR * cse_val = processExp(bb, ie, ir, avail_ir_expr,
                                  map_expr2avail_pos,
                                  map_expr2avail_pr);
        if (cse_val != nullptr) {
            if (!cse_val->is_judge()) {
                cse_val = m_rg->getIRMgr()->buildJudge(
                    m_rg->dupIRTree(cse_val));
                ir->setJudgeDet(cse_val);
            } else {
                ir->setJudgeDet(m_rg->dupIRTree(cse_val));
            }
            change = true;
        }
        break;
    }
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    case IR_IGOTO: {
        ASSERT0(ir->getValExp());
        if (!canBeCandidate(ir->getValExp())) { break; }
        ExprRep * ie = m_expr_tab->mapIR2ExprRep(ir->getValExp());
        ASSERT0(ie);
        IR * cse_val = processExp(bb, ie, ir, avail_ir_expr,
                                  map_expr2avail_pos,
                                  map_expr2avail_pr);
        if (ir->getValExp() != cse_val) {
            if (!cse_val->is_judge()) {
                cse_val = m_rg->getIRMgr()->buildJudge(
                    m_rg->dupIRTree(cse_val));
                ir->setValExp(cse_val);
            } else {
                ir->setValExp(m_rg->dupIRTree(cse_val));
            }
            change = true;
        }
        break;
    }
    case IR_RETURN: {
        if (RET_exp(ir) == nullptr || !canBeCandidate(RET_exp(ir))) {
            break;
        }
        ExprRep * ie = m_expr_tab->mapIR2ExprRep(RET_exp(ir));
        ASSERT0(ie);
        IR * cse_val = processExp(bb, ie, ir, avail_ir_expr,
                                  map_expr2avail_pos, map_expr2avail_pr);
        if (RET_exp(ir) != cse_val) {
            RET_exp(ir) = m_rg->dupIRTree(cse_val);
            ir->setParentPointer();
            change = true;
        }
        break;
    }
    default: UNREACHABLE();
    }
    return change;
}


//Return true if common expression has been substituted.
bool LCSE::processDef(IN IRBB * bb, IN IR * ir,
                      MOD xcom::BitSet & avail_ir_expr,
                      MOD Vector<IR*> & map_expr2avail_pos,
                      MOD Vector<IR*> & map_expr2avail_pr,
                      IN MDSet & tmp)
{
    CollectMayUseRecur co(m_rg);
    bool change = false;
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_CALL:
    case IR_STPR:
    case IR_RETURN: {
        //Compute killed ir-expr.
        MDSet const* maydef = ir->getMayRef();
        MD const* mustdef = ir->getMustRef();
        if ((maydef == nullptr || maydef->is_empty()) && mustdef == nullptr) {
            return change;
        }
        for (BSIdx j = avail_ir_expr.get_first();
             j != BS_UNDEF; j = avail_ir_expr.get_next(j)) {
            ExprRep * ie = m_expr_tab->getExpVec().get(j);
            ASSERT0(ie != nullptr);
            for (IR * occ = EXPR_occ_list(ie).get_head();
                 occ != nullptr; occ = EXPR_occ_list(ie).get_next()) {
                IR * occ_stmt = occ->getStmt();
                ASSERT0(occ_stmt != nullptr && occ_stmt->getBB());
                ASSERT0(ir->getBB() == bb);
                if (occ_stmt->getBB() != bb) {
                    continue;
                }
                tmp.clean(m_misc_bs_mgr);
                co.collect(occ, true, m_misc_bs_mgr, tmp);
                if ((maydef != nullptr && maydef->is_intersect(tmp)) ||
                    (mustdef != nullptr && tmp.is_contain(mustdef, m_rg))) {
                    avail_ir_expr.diff(EXPR_id(ie));
                    map_expr2avail_pos.set(EXPR_id(ie), nullptr);
                    map_expr2avail_pr.set(EXPR_id(ie), nullptr);
                }
            }
        }
        return change;
    }
    SWITCH_CASE_BRANCH_OP:
    SWITCH_CASE_CFS_OP:
    case IR_LABEL:
    case IR_CASE:
        return change;
    default: UNREACHABLE();
    }
    return change;
}


bool LCSE::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_ref_valid()) { return false; }

    //Check PR DU chain.
    PRSSAMgr * ssamgr = (PRSSAMgr*)(m_rg->getPassMgr()->queryPass(
        PASS_PRSSA_MGR));
    if (ssamgr != nullptr && ssamgr->is_valid()) {
        m_ssamgr = ssamgr;
    } else {
        m_ssamgr = nullptr;
    }
    if (!oc.is_pr_du_chain_valid() && m_ssamgr == nullptr) {
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    //Check NONPR DU chain.
    MDSSAMgr * mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->queryPass(
        PASS_MDSSA_MGR));
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        m_mdssamgr = mdssamgr;
    } else {
        m_mdssamgr = nullptr;
    }
    if (!oc.is_nonpr_du_chain_valid() && m_mdssamgr == nullptr) {
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_EXPR_TAB, PASS_UNDEF);
    m_expr_tab = (ExprTab*)m_rg->getPassMgr()->registerPass(PASS_EXPR_TAB);
    ASSERT0(m_expr_tab);
    bool change = false;

    //Record lived expression during analysis.
    xcom::BitSet avail_ir_expr;

    //Record ir stmt's address as a position.
    xcom::Vector<IR*> map_expr2avail_pos;

    //Record pr that hold the value of expression.
    xcom::Vector<IR*> map_expr2avail_pr;
    BBListIter ctbb = nullptr;
    MDSet tmp;
    for (bbl->get_head(&ctbb); ctbb != bbl->end(); ctbb = bbl->get_next(ctbb)) {
        IRBB * bb = ctbb->val();
        ASSERT0(bb);
        map_expr2avail_pos.clean();
        map_expr2avail_pr.clean();
        avail_ir_expr.clean();
        IRListIter ct = nullptr;
        for (BB_irlist(bb).get_head(&ct);
             ct != BB_irlist(bb).end(); ct = BB_irlist(bb).get_next(ct)) {
            IR * ir = ct->val();
            if (ir->hasSideEffect(true) || ir->isDummyOp()) { continue; }
            change |= processUse(bb, ir, avail_ir_expr,
                                 map_expr2avail_pos, map_expr2avail_pr);
            if (!ir->hasResult()) { continue; }

            //There may have expressions be killed.
            //Remove them out the avail_ir_expr.
            change |= processDef(bb, ir, avail_ir_expr,
                                 map_expr2avail_pos, map_expr2avail_pr, tmp);
        }
    }
    tmp.clean(m_misc_bs_mgr);

    ASSERT0(verifyIRandBB(bbl, m_rg));
    if (change) {
        //Found CSE and processed them.
        oc.setInvalidPass(PASS_EXPR_TAB);
        oc.setInvalidPass(PASS_AA);
        oc.setInvalidPass(PASS_CLASSIC_DU_CHAIN);
        oc.setInvalidPass(PASS_MD_REF);
    }
    END_TIMER(t, getPassName());
    return change;
}
//END LCSE

} //namespace xoc
