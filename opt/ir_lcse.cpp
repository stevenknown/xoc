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
#include "ir_lcse.h"

namespace xoc {

//
//START IR_LCSE
//
IR_LCSE::IR_LCSE(Region * rg)
{
    ASSERT0(rg != NULL);
    m_ru = rg;
    m_tm = rg->getTypeMgr();
    m_du = m_ru->getDUMgr();
    ASSERT0(m_du && m_tm);
    m_expr_tab = NULL;
    m_expr_vec = NULL;
    m_enable_filter = true;
}


//Hoist CSE's computation, and replace its occurrence with the result pr.
IR * IR_LCSE::hoist_cse(IN IRBB * bb, IN IR * ir_pos, IN ExpRep * ie)
{
    xcom::C<IR*> * pos_holder = NULL;
    bool f = BB_irlist(bb).find(ir_pos, &pos_holder);
    CHECK_DUMMYUSE(f);
    switch (ir_pos->getCode()) {
    case IR_ST:
    case IR_IST: {
        //return the pr that hold the cse value.
        IR * ret = NULL;
        //Move STORE_VAL to temp PR.
        ExpRep * tie = m_expr_tab->map_ir2ir_expr(ST_rhs(ir_pos));
        if (tie != NULL && tie == ie) {
            //e.g: a = 10, expression of store_val is NULL.
            IR * x = ST_rhs(ir_pos);
            ret = m_ru->buildPR(IR_dt(x));
            IR * new_st = m_ru->buildStorePR(PR_no(ret), ret->getType(), x);

            //Insert into IR list of BB.
            BB_irlist(bb).insert_before(new_st, pos_holder);
            ST_rhs(ir_pos) = ret;
            ir_pos->setParentPointer(false);
        } //end if

        if (ir_pos->is_ist()) {
            //Move MEM ADDR to Temp PR.
            tie = m_expr_tab->map_ir2ir_expr(IST_base(ir_pos));
            if (tie != NULL && tie == ie) {
                if (ret == NULL) {
                    IR * x = IST_base(ir_pos);
                    ret = m_ru->buildPR(IR_dt(x));
                    IR * new_st = m_ru->buildStorePR(PR_no(ret),
                        ret->getType(), m_ru->dupIRTree(x));

                    //Insert into IR list of BB.
                    BB_irlist(bb).insert_before(new_st, pos_holder);
                } else {
                    ret = m_ru->dupIRTree(ret);
                }

                //Replace orignial referenced IR with the new PR.
                IST_base(ir_pos) = ret;
                ir_pos->setParentPointer(false);
            }
        }
        return ret;
    }
    case IR_CALL:
    case IR_ICALL: {
        IR * p = CALL_param_list(ir_pos);
        IR * ret = NULL; //return the pr that hold the cse value.
        IR * next_parm = NULL;
        while (p != NULL) {
            next_parm = p->get_next();
            ExpRep * tie = m_expr_tab->map_ir2ir_expr(p);
            if (tie != NULL && tie == ie) {
                bool insert_st;
                if (ret == NULL) {
                    ret = m_ru->buildPR(IR_dt(p));
                    replace(&CALL_param_list(ir_pos), p, ret);
                    insert_st = true;
                } else {
                    replace(&CALL_param_list(ir_pos), p,
                            m_ru->dupIRTree(ret));
                    insert_st = false;
                }
                ASSERT0(IR_prev(p) == NULL && p->get_next() == NULL);

                if (insert_st) {
                    IR * new_st = m_ru->buildStorePR(
                                    PR_no(ret), ret->getType(), p);
                    BB_irlist(bb).insert_before(new_st, pos_holder);
                }
            }
            p = next_parm;
        } //end while
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
        ASSERT(0, ("TODO"));
        break;
    case IR_TRUEBR:
    case IR_FALSEBR: {
        ExpRep * tie = m_expr_tab->map_ir2ir_expr(BR_det(ir_pos));
        if (tie != NULL && tie == ie) {
            IR * x = BR_det(ir_pos);
            IR * ret = m_ru->buildPR(IR_dt(x));
            IR * new_st = m_ru->buildStorePR(PR_no(ret), ret->getType(), x);
            BB_irlist(bb).insert_before(new_st, pos_holder);
            BR_det(ir_pos) = ret;
            ir_pos->setParentPointer(false);
            return ret;
        }
        break;
    }
    case IR_IGOTO: {
        ExpRep * tie = m_expr_tab->map_ir2ir_expr(IGOTO_vexp(ir_pos));
        if (tie != NULL && tie == ie) {
            IR * x = IGOTO_vexp(ir_pos);
            IR * ret = m_ru->buildPR(IR_dt(x));
            IR * new_st = m_ru->buildStorePR(PR_no(ret), ret->getType(), x);
            BB_irlist(bb).insert_before(new_st, pos_holder);
            IGOTO_vexp(ir_pos) = ret;
            ir_pos->setParentPointer(false);
            return ret;
        }
        break;
    }
    case IR_SWITCH: {
        ExpRep * tie = m_expr_tab->map_ir2ir_expr(SWITCH_vexp(ir_pos));
        if (tie != NULL && tie == ie) {
            IR * x = SWITCH_vexp(ir_pos);
            IR * ret = m_ru->buildPR(IR_dt(x));
            IR * new_st = m_ru->buildStorePR(PR_no(ret), ret->getType(), x);
            BB_irlist(bb).insert_before(new_st, pos_holder);
            SWITCH_vexp(ir_pos) = ret;
            ir_pos->setParentPointer(false);
            return ret;
        }
        break;
    }
    case IR_RETURN: {
        ExpRep * tie = m_expr_tab->map_ir2ir_expr(RET_exp(ir_pos));
        if (tie != NULL && tie == ie) {
            IR * x = RET_exp(ir_pos);
            IR * ret = m_ru->buildPR(IR_dt(x));
            IR * new_st = m_ru->buildStorePR(PR_no(ret), ret->getType(), x);
            BB_irlist(bb).insert_before(new_st, pos_holder);
            RET_exp(ir_pos) = ret;
            ir_pos->setParentPointer(false);
            return ret;
        }
        ir_pos->setParentPointer(false);
        break;
    }
    default: UNREACHABLE();
    } //end switch
    return NULL;
}


bool IR_LCSE::processBranch(
        IN IRBB * bb,
        IN IR * ir,
        IN OUT xcom::BitSet & avail_ir_expr,
        IN OUT Vector<IR*> & map_expr2avail_pos,
        IN OUT Vector<IR*> & map_expr2avail_pr)
{
    ASSERT0(ir->isConditionalBr());
    bool change = false;
    if (!canBeCandidate(BR_det(ir))) { return false; }
    ExpRep * ie = m_expr_tab->map_ir2ir_expr(BR_det(ir));
    if (ie != NULL) {
        avail_ir_expr.bunion(EXPR_id(ie));
        IR * ir_pos = map_expr2avail_pos.get(EXPR_id(ie));
        if (ir_pos != NULL) {
            IR * pr = map_expr2avail_pr.get(EXPR_id(ie));
            if (pr == NULL) {
                //e.g: before:
                //    =a||b
                //    ...
                //    falsebr(a||b)
                //after:
                //    t=a||b
                //    =t
                //    ...
                //    =a||b
                pr = hoist_cse(bb, ir_pos, ie);
                ASSERT0(pr != NULL);
                map_expr2avail_pr.set(EXPR_id(ie), pr);
                change = true;
            }
            BR_det(ir) = m_ru->buildJudge(m_ru->dupIRTree(pr));
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
IR * IR_LCSE::processExp(
        IN IRBB * bb,
        IN ExpRep * ie,
        IN IR * stmt,
        IN OUT xcom::BitSet & avail_ir_expr,
        IN OUT Vector<IR*> & map_expr2avail_pos,
        IN OUT Vector<IR*> & map_expr2avail_pr)
{
    ASSERT0(ie);
    avail_ir_expr.bunion(EXPR_id(ie));

    //Find which stmt that the expression start to live at.
    IR * ir_pos = map_expr2avail_pos.get(EXPR_id(ie));
    if (ir_pos != NULL) {
        IR * cse_val_pr = map_expr2avail_pr.get(EXPR_id(ie));
        if (cse_val_pr == NULL) {
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
            cse_val_pr = hoist_cse(bb, ir_pos, ie);
            ASSERT0(cse_val_pr != NULL);
            map_expr2avail_pr.set(EXPR_id(ie), cse_val_pr);
        }
        return cse_val_pr;
    }

    //First meet the expression, and record its stmt.
    map_expr2avail_pos.set(EXPR_id(ie), stmt);
    return NULL;
}


bool IR_LCSE::canBeCandidate(IR * ir)
{
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


bool IR_LCSE::processRhsOfStore(
        IN IRBB * bb, IN IR * ir,
        IN OUT xcom::BitSet & avail_ir_expr,
        IN OUT Vector<IR*> & map_expr2avail_pos,
        IN OUT Vector<IR*> & map_expr2avail_pr)
{
    ASSERT0(ir->is_st() || ir->is_ist());
    bool change = false;
    if (!canBeCandidate(ST_rhs(ir))) {
        return false;
    }
    ExpRep * ie = m_expr_tab->map_ir2ir_expr(ST_rhs(ir));
    if (ie != NULL) {
        avail_ir_expr.bunion(EXPR_id(ie));
        //e.g: a = 10, expression of store_val is NULL.
        IR * ir_pos = map_expr2avail_pos.get(EXPR_id(ie));
        if (ir_pos != NULL) {
            IR * pr = map_expr2avail_pr.get(EXPR_id(ie));
            if (pr == NULL) {
                //e.g: before:
                //    =a+b
                //    ...
                //        =a+b
                //after:
                //    t=a+b
                //    =t
                //    ...
                //    =a+b
                pr = hoist_cse(bb, ir_pos, ie);
                ASSERT0(pr != NULL);
                map_expr2avail_pr.set(EXPR_id(ie), pr);
                change = true;
            }
            ST_rhs(ir) = m_ru->dupIRTree(pr);
            ir->setParentPointer(false);
            change = true;
        } else {
            //Record position of IR stmt.
            map_expr2avail_pos.set(EXPR_id(ie), ir);
        }
    }

    if (ir->is_ist()) {
        ie = m_expr_tab->map_ir2ir_expr(IST_base(ir));
        if (ie != NULL) {
            avail_ir_expr.bunion(EXPR_id(ie));
            IR * ir_pos = map_expr2avail_pos.get(EXPR_id(ie));
            if (ir_pos != NULL) {
                IR * pr = map_expr2avail_pr.get(EXPR_id(ie));
                if (pr == NULL) {
                    //e.g: before:
                    //    =a+b
                    //    ...
                    //        =a+b
                    //after:
                    //    t=a+b
                    //    =t
                    //    ...
                    //    =a+b
                    pr = hoist_cse(bb, ir_pos, ie);
                    ASSERT0(pr != NULL);
                    map_expr2avail_pr.set(EXPR_id(ie), pr);
                    change = true;
                }
                IST_base(ir) = m_ru->dupIRTree(pr);
                ir->setParentPointer(false);
                change = true;
            } else {
                map_expr2avail_pos.set(EXPR_id(ie), ir);
            }
        }
    }
    return change;
}


bool IR_LCSE::processParamList(IN IRBB * bb, IN IR * ir,
                                 IN OUT xcom::BitSet & avail_ir_expr,
                                 IN OUT Vector<IR*> & map_expr2avail_pos,
                                 IN OUT Vector<IR*> & map_expr2avail_pr)
{
    bool change = false;
    bool lchange = true;
    while (lchange) {
        //Iterative analyse cse, e.g:
        //    CALL(ADD(x,y), SUB(a,b), ADD(x,y), SUB(a,b))
        IR * p = CALL_param_list(ir);
        lchange = false;
        while (p != NULL) {
            if (canBeCandidate(p)) {
                ExpRep * ie = m_expr_tab->map_ir2ir_expr(p);
                IR * newparam = processExp(bb,    ie, ir,
                                            avail_ir_expr,
                                            map_expr2avail_pos,
                                            map_expr2avail_pr);
                if (newparam != NULL) {
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


bool IR_LCSE::processUse(IN IRBB * bb, IN IR * ir,
                          IN OUT xcom::BitSet & avail_ir_expr,
                          IN OUT Vector<IR*> & map_expr2avail_pos,
                          IN OUT Vector<IR*> & map_expr2avail_pr)
{
    bool change = false;
    switch (ir->getCode()) {
    case IR_ST:
    case IR_IST:
        change |= processRhsOfStore(bb, ir, avail_ir_expr, map_expr2avail_pos,
                                    map_expr2avail_pr);
        break;
    case IR_CALL:
    case IR_ICALL:
        change |= processParamList(bb, ir, avail_ir_expr, map_expr2avail_pos,
                                   map_expr2avail_pr);
        break;
    case IR_GOTO:
        break;
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_LABEL:
    case IR_CASE:
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        {
            ASSERT0(BR_det(ir));
            if (!canBeCandidate(BR_det(ir))) { break; }
            ExpRep * ie = m_expr_tab->map_ir2ir_expr(BR_det(ir));
            ASSERT0(ie);
            IR * cse_val = processExp(bb, ie, ir, avail_ir_expr,
                                      map_expr2avail_pos,
                                      map_expr2avail_pr);
            if (cse_val != NULL) {
                if (!cse_val->is_judge()) {
                    cse_val = m_ru->buildJudge(m_ru->dupIRTree(cse_val));
                    BR_det(ir) = cse_val;
                } else {
                    BR_det(ir) = m_ru->dupIRTree(cse_val);
                }
                ir->setParentPointer();
                change = true;
            }
        }
        break;
    case IR_IGOTO:
        {
            ASSERT0(IGOTO_vexp(ir));
            if (!canBeCandidate(IGOTO_vexp(ir))) { break; }
            ExpRep * ie = m_expr_tab->map_ir2ir_expr(IGOTO_vexp(ir));
            ASSERT0(ie);
            IR * cse_val = processExp(bb, ie, ir, avail_ir_expr,
                                      map_expr2avail_pos,
                                      map_expr2avail_pr);
            if (IGOTO_vexp(ir) != cse_val) {
                if (!cse_val->is_judge()) {
                    cse_val = m_ru->buildJudge(m_ru->dupIRTree(cse_val));
                    IGOTO_vexp(ir) = cse_val;
                } else {
                    IGOTO_vexp(ir) = m_ru->dupIRTree(cse_val);
                }
                ir->setParentPointer();
                change = true;
            }
        }
        break;
    case IR_SWITCH:
        {
            ASSERT0(SWITCH_vexp(ir));
            if (!canBeCandidate(SWITCH_vexp(ir))) { break; }
            ExpRep * ie = m_expr_tab->map_ir2ir_expr(SWITCH_vexp(ir));
            ASSERT0(ie);
            IR * cse_val = processExp(bb, ie, ir, avail_ir_expr,
                                       map_expr2avail_pos,
                                       map_expr2avail_pr);
            if (SWITCH_vexp(ir) != cse_val) {
                if (!cse_val->is_judge()) {
                    cse_val = m_ru->buildJudge(m_ru->dupIRTree(cse_val));
                    SWITCH_vexp(ir) = cse_val;
                } else {
                    SWITCH_vexp(ir) = m_ru->dupIRTree(cse_val);
                }
                ir->setParentPointer();
                change = true;
            }
        }
        break;
    case IR_RETURN:
        {
            ASSERT0(SWITCH_vexp(ir));
            if (!canBeCandidate(RET_exp(ir))) { break; }
            ExpRep * ie = m_expr_tab->map_ir2ir_expr(RET_exp(ir));
            ASSERT0(ie);
            IR * cse_val = processExp(bb, ie, ir, avail_ir_expr,
                                       map_expr2avail_pos,
                                       map_expr2avail_pr);
            if (RET_exp(ir) != cse_val) {
                SWITCH_vexp(ir) = m_ru->dupIRTree(cse_val);
                ir->setParentPointer();
                change = true;
            }
        }
        break;
    default: UNREACHABLE();
    } //end switch
    return change;
}


//Return true if common expression has been substituted.
bool IR_LCSE::processDef(
        IN IRBB * bb,
        IN IR * ir,
        IN OUT xcom::BitSet & avail_ir_expr,
        IN OUT Vector<IR*> & map_expr2avail_pos,
        IN OUT Vector<IR*> & map_expr2avail_pr,
        IN MDSet & tmp)
{
    bool change = false;
    switch (ir->getCode()) {
    case IR_ST:
    case IR_IST:
    case IR_CALL:
    case IR_ICALL:
    case IR_RETURN:
        {
            //Compute killed ir-expr.
            MDSet const* maydef = m_du->getMayDef(ir);
            MD const* mustdef = m_du->get_must_def(ir);
            if ((maydef != NULL && !maydef->is_empty()) ||
                mustdef != NULL) {
                for (INT j = avail_ir_expr.get_first();
                     j != -1; j = avail_ir_expr.get_next(j)) {
                    ExpRep * ie = m_expr_vec->get(j);
                    ASSERT0(ie != NULL);
                    for (IR * occ = EXPR_occ_list(ie).get_head();
                         occ != NULL; occ = EXPR_occ_list(ie).get_next()) {
                        IR * occ_stmt = occ->getStmt();
                        ASSERT0(occ_stmt != NULL && occ_stmt->getBB());
                        ASSERT0(ir->getBB() == bb);
                        if (occ_stmt->getBB() != bb) {
                            continue;
                        }

                        tmp.clean(m_misc_bs_mgr);
                        m_du->collectMayUseRecursive(occ,
                            tmp, true, m_misc_bs_mgr);
                        if ((maydef != NULL && maydef->is_intersect(tmp)) ||
                            (mustdef != NULL && tmp.is_contain(mustdef))) {
                            avail_ir_expr.diff(EXPR_id(ie));
                            map_expr2avail_pos.set(EXPR_id(ie), NULL);
                            map_expr2avail_pr.set(EXPR_id(ie), NULL);
                        }
                    }
                }
            }
        }
        break;
    case IR_GOTO:
    case IR_IGOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_SWITCH:
    case IR_LABEL:
    case IR_CASE:
    case IR_TRUEBR:
    case IR_FALSEBR:
        break;
    default: UNREACHABLE();
    } //end switch
    return change;
}


bool IR_LCSE::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_ru->checkValidAndRecompute(&oc, PASS_DU_REF, PASS_DU_CHAIN,
        PASS_EXPR_TAB, PASS_UNDEF);

    if (!OC_is_du_chain_valid(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }

    m_expr_tab = (IR_EXPR_TAB*)m_ru->getPassMgr()->registerPass(PASS_EXPR_TAB);
    ASSERT0(m_expr_tab);

    m_expr_vec = m_expr_tab->get_expr_vec();
    ASSERT0(m_expr_vec);

    BBList * bbl = m_ru->getBBList();

    bool change = false;

    //Record lived expression during analysis.
    xcom::BitSet avail_ir_expr;

    //Record ir stmt's address as a position.
    Vector<IR*> map_expr2avail_pos;

    //Record pr that hold the value of expression.
    Vector<IR*> map_expr2avail_pr;
    xcom::C<IRBB*> * ctbb;
    MDSet tmp;
    for (bbl->get_head(&ctbb); ctbb != bbl->end(); ctbb = bbl->get_next(ctbb)) {
        IRBB * bb = ctbb->val();
        ASSERT0(bb);
        map_expr2avail_pos.clean();
        map_expr2avail_pr.clean();
        avail_ir_expr.clean();
        xcom::C<IR*> * ct = NULL;
        for (BB_irlist(bb).get_head(&ct);
             ct != BB_irlist(bb).end(); ct = BB_irlist(bb).get_next(ct)) {
            IR * ir = ct->val();
            change |= processUse(bb, ir, avail_ir_expr,
                map_expr2avail_pos, map_expr2avail_pr);

            if (ir->hasResult()) {
                //There may have expressions be killed.
                //Remove them out the avail_ir_expr.
                change |= processDef(bb, ir, avail_ir_expr,
                    map_expr2avail_pos, map_expr2avail_pr, tmp);
            }
        } //end for each IR
    }
    tmp.clean(m_misc_bs_mgr);

    ASSERT0(verifyIRandBB(bbl, m_ru));
    if (change) {
        //Found CSE and processed them.
        OC_is_expr_tab_valid(oc) = false;
        OC_is_aa_valid(oc) = false;
        OC_is_du_chain_valid(oc) = false;
        OC_is_ref_valid(oc) = false;
    }
    END_TIMER(t, getPassName());
    return change;
}
//END IR_LCSE

} //namespace xoc
