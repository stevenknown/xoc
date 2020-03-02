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
//START GCSE
//

//Replace use cse with PR related to gen.
//e.g: ...=a+b <--generate CSE
//     ...
//     ...=a+b <--use CSE
//This function do replacement via gvn info.
//
//'use': the referrence of cse.
//'use_stmt': the stmt contains use.
//'gen': the referrence of cse.
//
//NOTE: 'use' should be freed.
//      'use' must be rhs of 'use_stmt'.
void GCSE::elimCseAtStore(IR * use, IR * use_stmt, IR * gen)
{
    ASSERT0(use_stmt->is_st() || use_stmt->is_stpr() || use_stmt->is_ist());
    #ifdef _DEBUG_
    m_elimed.append(use->id());
    #endif
    ASSERT0(use->is_exp() && gen->is_exp());
    ASSERT0(use_stmt->getRHS() == use);

    //Cut off du chain for use and its definitions.
    m_du->removeUseOutFromDefset(use);

    //gen_pr hold the CSE value come from gen-stmt.
    //We eliminate the redundant computation via replace use by gen_pr.
    IR * gen_pr = m_exp2pr.get(gen);
    ASSERT0(gen_pr);

    IR * newrhs_pr = m_rg->dupIRTree(gen_pr);
    use_stmt->setRHS(newrhs_pr);
    IR_parent(newrhs_pr) = use_stmt;

    //Use stmt is just a move.
    IR_may_throw(use_stmt) = false;

    //Add du chain.
    IR * gen_stmt = gen->getStmt();
    ASSERT0(gen_stmt->isPREqual(gen_pr));
    if (m_ssamgr != NULL) {
        m_ssamgr->buildDUChain(gen_stmt, newrhs_pr);
    } else {
        m_du->buildDUChain(gen_stmt, newrhs_pr);
    }

    //Assign the identical vn to newrhs.
    if (m_gvn != NULL) {
        VN * vn = m_gvn->mapIR2VN(gen);
        ASSERT0(vn);
        m_gvn->setMapIR2VN(newrhs_pr, vn);
        m_gvn->setMapIR2VN(use_stmt, vn);
    }

    //Assign MD to newrhs.
    MD const* r_md = m_rg->genMDforPR(newrhs_pr);
    ASSERT0(r_md);
    newrhs_pr->setRefMD(r_md, m_rg);

    if (m_mdssamgr != NULL) {
        m_mdssamgr->removeMDSSAUseRecur(use);
    }
    m_rg->freeIRTree(use);
}


void GCSE::elimCseAtBranch(IR * use, IR * use_stmt, IN IR * gen)
{
    #ifdef _DEBUG_
    m_elimed.append(use->id());
    #endif
    ASSERT0(use->is_exp() && gen->is_exp());

    //Cut off du chain for use and its definitions.
    m_du->removeUseOutFromDefset(use);

    IR * gen_pr = m_exp2pr.get(gen);
    ASSERT0(gen_pr);
    ASSERT0(BR_det(use_stmt) == use);
    IR * new_pr = m_rg->dupIRTree(gen_pr);

    //Add du chain.
    IR * gen_stmt = gen->getStmt();
    ASSERT0(gen_stmt->isPREqual(gen_pr));

    if (m_ssamgr != NULL) {
        m_ssamgr->buildDUChain(gen_stmt, new_pr);
    } else {
        m_du->buildDUChain(gen_stmt, new_pr);
    }

    //Assign the idential vn to r.
    ASSERT0(m_gvn);
    VN * vn = m_gvn->mapIR2VN(gen);
    ASSERT0(vn);
    m_gvn->setMapIR2VN(new_pr, vn);

    //Assign MD to PR.
    MD const* r_md = m_rg->genMDforPR(new_pr);
    ASSERT0(r_md);
    new_pr->setRefMD(r_md, m_rg);

    //Det of branch stmt have to be judgement operation.
    IR * newdet = m_rg->buildJudge(new_pr);
    IR_parent(newdet) = use_stmt;
    BR_det(use_stmt) = newdet;
    IR_may_throw(use_stmt) = false;

    if (m_mdssamgr != NULL) {
        m_mdssamgr->removeMDSSAUseRecur(use);
    }
    m_rg->freeIRTree(use);
}


//Replace use_cse with PR related to gen_cse.
//This function do replacement via gvn info.
//
//e.g: ...=a+b <--generate CSE
//     ...
//     ...call, a+b, a+b <--two use CSE.
//
//'use': the referrence expression of cse.
//'use_stmt': the stmt which 'use' is belong to.
//'gen': the first occurrence of CSE.
//
//NOTE: 'use' should be freed.
void GCSE::elimCseAtCall(IR * use, IR * use_stmt, IR * gen)
{
    #ifdef _DEBUG_
    m_elimed.append(use->id());
    #endif
    ASSERT0(use->is_exp() && gen->is_exp() && use_stmt->is_stmt());

    //Cut off du chain for use and its definitions.
    m_du->removeUseOutFromDefset(use);

    IR * gen_pr = m_exp2pr.get(gen);
    ASSERT0(gen_pr && gen_pr->is_pr());
    IR * use_pr = m_rg->dupIRTree(gen_pr);

    //Set identical vn to use_pr with CSE.
    IR * gen_stmt = gen->getStmt();
    ASSERT0(m_gvn);
    VN * vn = m_gvn->mapIR2VN(gen);
    ASSERT0(vn);
    m_gvn->setMapIR2VN(use_pr, vn);

    //Allocate MD to use_pr to make up DU manager request.
    MD const* r_md = m_rg->genMDforPR(use_pr);
    ASSERT0(r_md);
    use_pr->setRefMD(r_md, m_rg);

    //Add du chain from gen_pr's stmt to the use of pr.
    bool f = use_stmt->replaceKid(use, use_pr, false);
    CHECK_DUMMYUSE(f);
    if (m_mdssamgr != NULL) {
        m_mdssamgr->removeMDSSAUseRecur(use);
    }
    m_rg->freeIRTree(use);

    if (m_ssamgr != NULL) {
        m_ssamgr->buildDUChain(gen_stmt, use_pr);
    } else {
        m_du->buildDUChain(gen_stmt, use_pr);
    }
}


//Replace use_cse with PR related to gen_cse.
//e.g: ...=a+b <--generate CSE
//     ...
//     ...return, a+b, a+b <--two use CSE.
//
//'use': the referrence expression of cse.
//'use_stmt': the stmt which 'use' is belong to.
//'gen': the first occurrence of CSE.
//
//NOTE: 'use' should be freed.
void GCSE::elimCseAtReturn(IR * use, IR * use_stmt, IR * gen)
{
    return elimCseAtCall(use, use_stmt, gen);
}


//Process the expression in CSE generation.
//This function do replacement via gvn info.
//
//e.g: ...=a+b <--generate CSE
//     ...
//     ...=a+b <--use CSE
//'gen': generated cse.
void GCSE::processCseGen(IN IR * gen, IR * gen_stmt, bool & change)
{
    ASSERT0(gen->is_exp() && gen_stmt->is_stmt());
    //Move STORE_VAL to temp PR.
    //e.g: a = 10, expression of store_val is NULL.
    IRBB * bb = gen_stmt->getBB();
    ASSERT0(bb);
    IR * tmp_pr = m_exp2pr.get(gen);
    if (tmp_pr != NULL) { return; }

    //First process cse generation point.
    tmp_pr = m_rg->buildPR(IR_dt(gen));
    m_exp2pr.set(gen, tmp_pr);

    //Assign MD to PR.
    MD const* tmp_pr_md = m_rg->genMDforPR(tmp_pr);
    ASSERT0(tmp_pr_md);
    tmp_pr->setRefMD(tmp_pr_md, m_rg);

    //Assign MD to ST.
    IR * new_stpr = m_rg->buildStorePR(PR_no(tmp_pr), IR_dt(tmp_pr), gen);
    new_stpr->setRefMD(tmp_pr_md, m_rg);

    if (m_gvn != NULL) {
        ASSERT0(m_gvn->mapIR2VN(gen));
        m_gvn->setMapIR2VN(new_stpr, m_gvn->mapIR2VN(gen));
    }

    copyDbx(new_stpr, gen_stmt, m_rg);

    //The 'find()' is fast because it is implemented with hash.
    xcom::C<IR*> * holder = NULL;
    bool f = BB_irlist(bb).find(gen_stmt, &holder);
    CHECK_DUMMYUSE(f);
    CHECK_DUMMYUSE(holder);
    BB_irlist(bb).insert_before(new_stpr, holder);

    IR * newkid = tmp_pr;
    if (gen_stmt->isConditionalBr() && gen == BR_det(gen_stmt)) {
        //Det of branch stmt have to be judgement expression.
        newkid = m_rg->buildJudge(tmp_pr);
        copyDbx(newkid, tmp_pr, m_rg);
    }

    bool v = gen_stmt->replaceKid(gen, newkid, false);
    CHECK_DUMMYUSE(v);

    //Keep original du unchange, add new du chain for new stmt.
    ASSERT0(tmp_pr->is_pr());
    if (m_ssamgr != NULL) {
        m_ssamgr->buildDUChain(new_stpr, tmp_pr);
    } else {
        m_du->buildDUChain(new_stpr, tmp_pr);
    }

    IR_may_throw(gen_stmt) = false;
    change = true;
}


bool GCSE::isCseCandidate(IR * ir)
{
    ASSERT0(ir);
    switch (ir->getCode()) {
    SWITCH_CASE_BIN:
    case IR_SELECT:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_ILD:
        return true;
    default: break;
    }
    return false;
}


bool GCSE::elim(IR * use, IR * use_stmt, IR * gen, IR * gen_stmt)
{
    //exp is CSE.
    //e.g: ...=a+b <--generate CSE
    //     ...
    //     ...=a+b <--use CSE
    bool change = false;
    processCseGen(gen, gen_stmt, change);
    switch (use_stmt->getCode()) {
    case IR_ST:
    case IR_STPR:
    case IR_IST:
        elimCseAtStore(use, use_stmt, gen);
        change = true;
        break;
    case IR_CALL:
    case IR_ICALL:
        elimCseAtCall(use, use_stmt, gen);
        change = true;
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        elimCseAtBranch(use, use_stmt, gen);
        change = true;
        break;
    case IR_RETURN:
        elimCseAtReturn(use, use_stmt, gen);
        change = true;
        break;
    default: break;
    }
    return change;
}


// If find 'exp' is cse, replace it with related pr.
//NOTE: exp should be freed.
bool GCSE::findAndElim(IR * exp, IR * gen)
{
    ASSERT0(exp && gen);
    ASSERT0(exp != gen);
    IR * exp_stmt = exp->getStmt();
    IR * gen_stmt = gen->getStmt();
    ASSERT0(exp_stmt->getBB() && gen_stmt->getBB());

    IRBB * gen_bb;
    IRBB * exp_bb;
    if (m_cfg->hasEHEdge()) {
        ASSERT0(m_tg);
        if ((gen_bb = gen_stmt->getBB()) == (exp_bb = exp_stmt->getBB())) {
            if (!gen_bb->is_dom(gen_stmt, exp_stmt, true)) {
                return false;
            }
        } else if (!m_tg->is_dom(BB_id(gen_bb), BB_id(exp_bb))) {
            return false;
        }
    } else {
        if ((gen_bb = gen_stmt->getBB()) == (exp_bb = exp_stmt->getBB())) {
            if (!gen_bb->is_dom(gen_stmt, exp_stmt, true)) {
                return false;
            }
        } else if (!m_cfg->is_dom(BB_id(gen_bb), BB_id(exp_bb))) {
            return false;
        }
    }
    return elim(exp, exp_stmt, gen, gen_stmt);
}


//If find 'exp' is cse, replace it with related pr.
//NOTE: exp should be freed.
bool GCSE::processCse(IN IR * exp, IN List<IR*> & livexp)
{
    IR * expstmt = exp->getStmt();
    ExpRep * irie = m_expr_tab->map_ir2ir_expr(exp);
    ASSERT0(irie && expstmt->getBB());
    xcom::C<IR*> * ct;
    bool change = false;
    for (IR * gen = livexp.get_head(&ct);
         gen != NULL; gen = livexp.get_next(&ct)) {
        ExpRep * xie = m_expr_tab->map_ir2ir_expr(gen);
        ASSERT0(xie);
        if (irie != xie) { continue; }
        IR * gen_stmt = gen->getStmt();
        ASSERT0(gen_stmt->getBB());
        UINT iid = BB_id(expstmt->getBB());
        UINT xid = BB_id(gen_stmt->getBB());
        if (!m_cfg->get_dom_set(iid)->is_contain(xid)) {
            continue;
        }
        return elim(exp, expstmt, gen, gen_stmt);
    }
    return change;
}


void GCSE::handleCandidate(IR * exp, IRBB * bb,
                              UINT entry_id, bool & change)
{
    VN const* vn = NULL;
    IR * gen = NULL;
    if ((vn = m_gvn->mapIR2VN(exp)) != NULL &&
        (gen = m_vn2exp.get(vn)) != NULL &&
        findAndElim(exp, gen)) {
        //Found cse and replaced it with pr.
        change = true;
    } else if (vn != NULL && gen == NULL) {
        if (m_cfg->hasEHEdge()) {
            ASSERT0(m_tg);
            if (m_tg->is_pdom(BB_id(bb), entry_id)) {
                m_vn2exp.set(vn, exp);
            }
        } else if (m_cfg->is_pdom(BB_id(bb), entry_id)) {
            m_vn2exp.set(vn, exp);
        }
    }
}


//Determine if det-exp of truebr/falsebr ought to be cse.
bool GCSE::shouldBeCse(IR * det)
{
    ASSERT0(det->is_judge());

    //If the det if simply enough, cse is dispensable.
    if (!IR_parent(det)->is_truebr() && !IR_parent(det)->is_falsebr()) {
        return true;
    }

    if (!det->is_relation()) {
        //det is complex operation.
        return true;
    }

    IR const* op0 = BIN_opnd0(det);
    IR const* op1 = BIN_opnd1(det);
    if (!op0->is_pr() && !op0->is_const()) {
        return true;
    }

    if (!op1->is_pr() && !op1->is_const()) {
        return true;
    }

    return false;
}


bool GCSE::doPropInDomTreeOrder(xcom::Graph const* domtree)
{
    IRBB * entry = m_cfg->get_entry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    ASSERT0(domtree);
    xcom::Vertex * root = domtree->getVertex(BB_id(entry));
    ASSERT0(root);
    BitSet is_visited;
    Vertex * v;
    Stack<Vertex*> stk;
    stk.push(root);
    bool changed = false;
    List<IR*> livexp;
    while ((v = stk.pop()) != NULL) {
        UINT vid = VERTEX_id(v);
        if (!is_visited.is_contain(vid)) {
            is_visited.bunion(vid);
            //May be push root more than once.
            stk.push(v);
            //The only place to process vertex.
            changed |= doProp(m_cfg->getBB(vid), livexp);
        }

        //Visit children.
        EdgeC * el = VERTEX_out_list(v);
        Vertex * succ;
        while (el != NULL) {
            succ = EDGE_to(EC_edge(el));
            if (!is_visited.is_contain(VERTEX_id(succ))) {
                stk.push(v);
                stk.push(succ);
                break;
            }
            el = EC_next(el);
        }
    }
    return changed;
}


bool GCSE::doPropVNInDomTreeOrder(xcom::Graph const* domtree)
{
    IRBB * entry = m_cfg->get_entry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    ASSERT0(domtree);
    xcom::Vertex * root = domtree->getVertex(BB_id(entry));
    ASSERT0(root);
    BitSet is_visited;
    Vertex * v;
    Stack<Vertex*> stk;
    stk.push(root);
    bool changed = false;
    while ((v = stk.pop()) != NULL) {
        UINT vid = VERTEX_id(v);
        if (!is_visited.is_contain(vid)) {
            is_visited.bunion(vid);
            //May be push root more than once.
            stk.push(v);
            //The only place to process vertex.
            changed |= doPropVN(m_cfg->getBB(vid), vid);
        }

        //Visit children.
        EdgeC * el = VERTEX_out_list(v);
        Vertex * succ;
        while (el != NULL) {
            succ = EDGE_to(EC_edge(el));
            if (!is_visited.is_contain(VERTEX_id(succ))) {
                stk.push(v);
                stk.push(succ);
                break;
            }
            el = EC_next(el);
        }
    }
    return changed;
}


//Do propagation according to value numbering.
bool GCSE::doPropVN(IRBB * bb, UINT entry_id)
{
    bool change = false;
    xcom::C<IR*> * ct;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != NULL; ir = BB_irlist(bb).get_next(&ct)) {
        switch (ir->getCode()) {
        case IR_ST:
        case IR_STPR:
        case IR_IST: {
            IR * rhs = ir->getRHS();
            //Find cse and replace it with properly pr.
            if (isCseCandidate(rhs)) {
                handleCandidate(rhs, bb, entry_id, change);
            }
            break;
        }
        case IR_CALL:
        case IR_ICALL: {
            IR * p = CALL_param_list(ir);
            IR * next = NULL;
            bool lchange = false;
            m_newst_lst.clean();
            while (p != NULL) {
                next = p->get_next();
                if (isCseCandidate(p)) {
                    handleCandidate(p, bb, entry_id, lchange);
                }
                p = next;
            }
            change |= lchange;
            break;
        }
        case IR_TRUEBR:
        case IR_FALSEBR:
            //Find cse and replace it with properly pr.
            ASSERT0(BR_det(ir));
            if (isCseCandidate(BR_det(ir)) && shouldBeCse(BR_det(ir))) {
                handleCandidate(BR_det(ir), bb, entry_id, change);
            }
            break;
        case IR_RETURN:
            if (RET_exp(ir) != NULL && isCseCandidate(RET_exp(ir))) {
                handleCandidate(RET_exp(ir), bb, entry_id, change);
            }
            break;
        default: break;
        }
    }
    return change;
}


//Do propagation according to lexciographic equivalence.
bool GCSE::doProp(IRBB * bb, List<IR*> & livexp)
{
    livexp.clean();
    DefDBitSetCore * x = m_du->getAvailInExpr(BB_id(bb), NULL);
    SEGIter * st = NULL;
    for (INT i = x->get_first(&st); i != -1; i = x->get_next(i, &st)) {
        IR * y = m_rg->getIR(i);
        if (y->is_undef() || y->is_pr()) { continue; }
        ASSERT0(y && y->is_exp());
        livexp.append_tail(y);
    }

    bool change = false;
    xcom::C<IR*> * ct;
    MDSet tmp;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != NULL; ir = BB_irlist(bb).get_next(&ct)) {
        switch (ir->getCode()) {
        case IR_ST:
            //Find cse and replace it with properly pr.
            if (isCseCandidate(ST_rhs(ir))) {
                if (processCse(ST_rhs(ir), livexp)) {
                    //Has found cse and replaced cse with pr.
                    change = true;
                } else {
                    //Generate new cse.
                    livexp.append_tail(ST_rhs(ir));
                }
            }
            break;
        case IR_STPR:
            //Find cse and replace it with properly pr.
            if (isCseCandidate(STPR_rhs(ir))) {
                if (processCse(STPR_rhs(ir), livexp)) {
                    //Has found cse and replaced cse with pr.
                    change = true;
                } else {
                    //Generate new cse.
                    livexp.append_tail(STPR_rhs(ir));
                }
            }
            break;
        case IR_IST:
            //Find cse and replace it with properly pr.
            if (isCseCandidate(IST_rhs(ir))) {
                if (processCse(IST_rhs(ir), livexp)) {
                    //Has found cse and replaced cse with pr.
                    change = true;
                } else {
                    //Generate new cse.
                    livexp.append_tail(IST_rhs(ir));
                }
            }
            break;
        case IR_CALL:
        case IR_ICALL:
            {
                IR * param = CALL_param_list(ir);
                IR * next = NULL;
                while (param != NULL) {
                    next = param->get_next();
                    if (isCseCandidate(param)) {
                        if (processCse(param, livexp)) {
                            //Has found cse and replaced cse with pr.
                            change = true;
                        } else {
                            //Generate new cse.
                            livexp.append_tail(param);
                        }
                    }
                    param = next;
                }
            }
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            if (isCseCandidate(BR_det(ir)) && shouldBeCse(BR_det(ir))) {
                if (processCse(BR_det(ir), livexp)) {
                    //Has found cse and replaced cse with pr.
                    change = true;
                } else {
                    //Generate new cse.
                    livexp.append_tail(BR_det(ir));
                }
            }
            break;
        case IR_RETURN:
            if (RET_exp(ir) != NULL &&
                isCseCandidate(RET_exp(ir)) &&
                shouldBeCse(RET_exp(ir))) {
                if (processCse(RET_exp(ir), livexp)) {
                    //Has found cse and replaced cse with pr.
                    change = true;
                } else {
                    //Generate new cse.
                    livexp.append_tail(RET_exp(ir));
                }
            }
            break;
        default: break;
        }

        //Remove may-killed live-expr.
        switch (ir->getCode()) {
        case IR_ST:
        case IR_STPR:
        case IR_IST:
        case IR_CALL:
        case IR_ICALL:
            {
                MDSet const* maydef = m_du->getMayDef(ir);
                if (maydef != NULL && !maydef->is_empty()) {
                    xcom::C<IR*> * ct2, * next;
                    for (livexp.get_head(&ct2), next = ct2;
                         ct2 != NULL; ct2 = next) {
                        livexp.get_next(&next);
                        IR * x2 = ct2->val();
                        tmp.clean(m_misc_bs_mgr);
                        m_du->collectMayUseRecursive(x2,
                            tmp, true, m_misc_bs_mgr);
                        if (maydef->is_intersect(tmp)) {
                            livexp.remove(ct2);
                        }
                    }
                }

                MD const* mustdef = m_du->get_must_def(ir);
                if (mustdef != NULL) {
                    xcom::C<IR*> * ct2, * next;
                    for (livexp.get_head(&ct2), next = ct2;
                         ct2 != NULL; ct2 = next) {
                        livexp.get_next(&next);
                        IR * x2 = ct2->val();
                        tmp.clean(m_misc_bs_mgr);
                        m_du->collectMayUseRecursive(x2,
                            tmp, true, m_misc_bs_mgr);
                        if (tmp.is_overlap(mustdef, m_rg)) {
                            livexp.remove(ct2);
                        }
                    }
                }
            }
            break;
        default: break;
        }
    }

    tmp.clean(m_misc_bs_mgr);
    return change;
}


void GCSE::dump()
{
    note("\n==---- DUMP GCSE '%s' ----==\n", m_rg->getRegionName());
    note("\nNumOfEliminatedCSE:%d", m_elimed.get_elem_count());
    note("\nEliminated IR: ");
    for (INT i = 0; i <= m_elimed.get_last_idx(); i++) {
        if (i != 0) {
            note(",");
        }
        note("id:%d", m_elimed.get(i));
    }
}


bool GCSE::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    if (m_gvn != NULL) {
        m_rg->checkValidAndRecompute(&oc, PASS_DOM, PASS_PDOM,
            PASS_DU_REF, PASS_DU_CHAIN, PASS_UNDEF);
        if (!m_gvn->is_valid()) {
            m_gvn->reperform(oc);
        }
        m_expr_tab = NULL;
    } else {
        m_rg->checkValidAndRecompute(&oc, PASS_DOM, PASS_PDOM, PASS_EXPR_TAB,
            PASS_DU_REF, PASS_DU_CHAIN, PASS_UNDEF);
        m_expr_tab = (ExprTab*)m_rg->getPassMgr()->
            registerPass(PASS_EXPR_TAB);
    }

    if (!OC_is_du_chain_valid(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }

    #ifdef _DEBUG_
    m_num_of_elim = 0;
    m_elimed.clean();
    #endif

    PRSSAMgr * ssamgr = (PRSSAMgr*)(m_rg->getPassMgr()->
        queryPass(PASS_PR_SSA_MGR));
    if (ssamgr != NULL && ssamgr->isSSAConstructed()) {
        m_ssamgr = ssamgr;
    } else {
        m_ssamgr = NULL;
    }

    MDSSAMgr * mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->
        queryPass(PASS_MD_SSA_MGR));
    if (mdssamgr != NULL && mdssamgr->isMDSSAConstructed()) {
        m_mdssamgr = mdssamgr;
    } else {
        m_mdssamgr = NULL;
    }

    bool change = false;
    IRBB * entry = m_cfg->get_entry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    xcom::Graph domtree;
    m_cfg->get_dom_tree(domtree);
    xcom::Vertex * root = domtree.getVertex(BB_id(entry));
    if (m_cfg->hasEHEdge()) {
        //Initialize Temp CFG and pick out exception-edge.
        m_tg = new TG(m_rg);
        m_tg->clone(*m_cfg);
        m_tg->pick_eh();
        m_tg->removeUnreachNode(BB_id(entry));
        m_tg->computeDomAndIdom();
        m_tg->computePdomAndIpdom(root);
    }

    if (m_gvn != NULL) {
        m_vn2exp.clean();
        m_exp2pr.clean();
        change |= doPropVNInDomTreeOrder(&domtree);

        //Access IRBB in preorder on domtree.
        //List<xcom::Vertex*> lst;
        //m_cfg->sortDomTreeInPreorder(root, lst);
        //for (xcom::Vertex * v = lst.get_head(); v != NULL; v = lst.get_next()) {
        //    IRBB * bb = m_cfg->getBB(VERTEX_id(v));
        //    ASSERT0(bb);
        //    change |= doPropVN(bb, BB_id(entry));
        //}
    } else {
        m_vn2exp.clean();
        m_exp2pr.clean();
        change |= doPropInDomTreeOrder(&domtree);

        //List<xcom::Vertex*> lst;
        //List<IR*> livexp;
        //m_cfg->sortDomTreeInPreorder(root, lst);
        ////Access IRBB in preorder on domtree.
        //for (xcom::Vertex * v = lst.get_head(); v != NULL; v = lst.get_next()) {
        //    IRBB * bb = m_cfg->getBB(VERTEX_id(v));
        //    ASSERT0(bb);
        //    change |= doProp(bb, livexp);
        //}
    }
    END_TIMER(t, getPassName());
    if (g_is_dump_after_pass && g_dump_opt.isDumpGCSE()) {
        dump();
    }
    if (change) {
        //no new expr generated, only new pr.
        OC_is_expr_tab_valid(oc) = false;
        OC_is_aa_valid(oc) = false;
        OC_is_live_expr_valid(oc) = false;
        OC_is_reach_def_valid(oc) = false;

        //DU reference and du chain has maintained.
        ASSERT0(m_rg->verifyMDRef());
        ASSERT0(m_du->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NONPR_DU));
        if (m_ssamgr != NULL) {
            ASSERT0(verifySSAInfo(m_rg));
        }
        //For now, gvn has updated correctly.
    }
    if (m_cfg->hasEHEdge()) {
        ASSERT0(m_tg);
        delete m_tg;
        m_tg = NULL;
    }
    ASSERT0(m_tg == NULL);
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    return change;
}
//END GCSE

} //namespace xoc
