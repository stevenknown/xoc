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

static bool needBuildDUChain(SimpCtx const& ctx)
{
    Region const* rg = ctx.getOptCtx()->getRegion();
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        return true;
    }
    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        return true;
    }
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr &&
        (ctx.getOptCtx()->is_pr_du_chain_valid() ||
         ctx.getOptCtx()->is_nonpr_du_chain_valid())) {
        return true;
    }
    return false;
}


static bool isLowest(IR const* ir)
{
    ASSERT0(ir->is_exp());
    if (ir->is_leaf()) { return true; }
    IR * parent = ir->getParent();
    if (parent == nullptr) { return true; }
    if (!parent->is_stmt()) {
        //tree height is more than 2.
        return false;
    }

    if (parent->isCallStmt()) {
        //If parent is CALL/ICALL, we always intend to reduce the
        //height for parameter/callee even if its height is not more than 2.
        return false;
    }

    if (parent->is_ist() && ir == IST_base(parent)) {
        //At lowest mode, IST's base expression must be leaf.
        return false;
    }

    if (parent->is_starray() && ir == ARR_base(parent)) {
        //At lowest mode, STARRAY's base and sublist must be leaf.
        return false;
    }

    return true;
}


//At lowest mode, the predicator, trueexp, falseexp must be leaf.
//Note the lowest height means tree height is more than 2.
//e.g: ... = add ld a, ld b; ADD is the lowest height.
bool IRSimp::isLowestHeightSelect(IR const* ir) const
{
    ASSERT0(ir->is_select());
    if (!isLowest(ir)) { return false; }

    ASSERT0(SELECT_det(ir));
    if (!SELECT_det(ir)->is_leaf()) { return false; }

    ASSERT0(SELECT_trueexp(ir));
    if (!SELECT_trueexp(ir)->is_leaf()) { return false; }

    ASSERT0(SELECT_falseexp(ir));
    if (!SELECT_falseexp(ir)->is_leaf()) { return false; }

    return true;
}


//At lowest mode, the array base, array subscript-expression must be leaf.
bool IRSimp::isLowestHeightArrayOp(IR const* ir) const
{
    ASSERT0(ir->isArrayOp());
    if (ir->is_array() && !isLowest(ir)) { return false; }

    if (!ARR_base(ir)->is_leaf()) { return false; }

    for (IR const* s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
        if (!s->is_leaf()) { return false; }
    }
    return true;
}


bool IRSimp::isLowestHeightExp(IR const* ir, SimpCtx const* ctx) const
{
    if (ir->is_leaf()) { return true; }

    ASSERT0(ctx);
    switch (ir->getCode()) {
    case IR_LAND:
    case IR_LOR:
    case IR_LNOT:
        return false;
    case IR_ARRAY:
        return isLowestHeightArrayOp(ir);
    case IR_ILD:
        if (!SIMP_ild_ist(ctx)) { return true; }
        return isLowest(ir);
    SWITCH_CASE_ARITH:
    SWITCH_CASE_BITWISE:
    SWITCH_CASE_COMPARE:
    SWITCH_CASE_SHIFT:
    case IR_NEG:
    case IR_CVT:
        return isLowest(ir);
    case IR_SELECT:
        return isLowestHeightSelect(ir);
    default: UNREACHABLE();
    }

    return true;
}


bool IRSimp::isLowestHeight(IR const* ir, SimpCtx const* ctx) const
{
    ASSERT0(ir && ir->is_stmt() && ctx);
    switch (ir->getCode()) {
    SWITCH_CASE_CALL:
        for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
            if (!p->is_leaf()) {
                return false;
            }
        }
        return true;
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR:
        return isLowestHeightExp(ir->getRHS(), ctx);
    SWITCH_CASE_WRITE_ARRAY:
        if (SIMP_array(ctx)) { return false; }
        if (!isLowestHeightArrayOp(ir)) { return false; }
        return isLowestHeightExp(ir->getRHS(), ctx);
    SWITCH_CASE_INDIRECT_MEM_STMT:
        if (SIMP_ild_ist(ctx) && !isLowestHeightExp(ir->getBase(), ctx)) {
            return false;
        }
        return isLowestHeightExp(ir->getRHS(), ctx);
    case IR_GOTO:
    case IR_LABEL:
    case IR_CASE:
        return true;
    case IR_RETURN:
        if (RET_exp(ir) != nullptr) {
            return isLowestHeightExp(RET_exp(ir), ctx);
        }
        return true;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        ASSERT0(BR_det(ir));
        return isLowestHeightExp(BR_det(ir), ctx);
    SWITCH_CASE_CFS_OP:
    SWITCH_CASE_LOOP_ITER_CFS_OP:
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    case IR_IGOTO:
        return false;
    default: UNREACHABLE();
    }
    return true;
}


//Transform if to:
//  falsebr(label(ELSE_START))
//  ...
//  TRUE-BODY-STMT-List;
//  ...
//  goto IF_END;
//  ELSE_START:
//  ...
//  FALSE-BODY-STMT-List;
//  ...
//  IF_END:
IR * IRSimp::simplifyIfSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) { return nullptr; }
    ASSERTN(ir->is_if(), ("expect IR_IF node"));
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;

    //Det exp.
    //When we first lowering CFS, det-expression should not be TRUEBR/FASLEBR.
    ASSERT0(IF_det(ir)->is_judge());
    IR * det = simplifyDet(IF_det(ir), &tcont);
    IR * last = removetail(&det);
    ASSERTN(last->is_exp(), ("invalide det exp"));
    if (!last->is_judge()) {
        //det-expression should be judgement.
        last = m_irmgr->buildJudge(last);
    }
    IR * falsebr = m_irmgr->buildBranch(false, last,
                                        m_rg->genILabel());
    copyDbx(falsebr, IF_det(ir), m_rg);
    xcom::add_next(&det, falsebr);

    IR * truebody = simplifyStmtList(IF_truebody(ir), ctx);
    IR * elsebody = nullptr;
    if (IF_falsebody(ir) != nullptr) { //Simplify ELSE body
        //append GOTO following end of true body
        IR * go = m_irmgr->buildGoto(m_rg->genILabel());
        copyDbx(go, IF_det(ir), m_rg);
        xcom::add_next(&truebody, go);

        //truebody end label
        xcom::add_next(&truebody,
                       m_irmgr->buildLabel(BR_lab(falsebr)));

        //simplify false body
        elsebody = simplifyStmtList(IF_falsebody(ir), ctx);

        //falsebody end label
        xcom::add_next(&elsebody, m_irmgr->buildLabel(GOTO_lab(go)));
    } else {
        //end label of truebody.
        xcom::add_next(&truebody, m_irmgr->buildLabel(BR_lab(falsebr)));
    }

    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = nullptr;

        ASSERT0(SIMP_cfs_mgr(ctx));
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_IF);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(falsebr, ci);
        CFS_INFO_ir(ci) = falsebr;

        SIMP_cfs_mgr(ctx)->recordStmt(truebody, *CFS_INFO_true_body(ci));
        SIMP_cfs_mgr(ctx)->recordStmt(elsebody, *CFS_INFO_false_body(ci));
    }

    IR * ret_list = nullptr;
    xcom::add_next(&ret_list, det);
    xcom::add_next(&ret_list, truebody);
    xcom::add_next(&ret_list, elsebody);
    xoc::cleanParentForIRList(ret_list);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return ret_list;
}


//Transform while-do to:
//  LABEL: start
//  WHILE-DO-DET
//  FALSEBR L1
//  BODY-STMT
//  GOTO start
//  LABEL: L1
IR * IRSimp::simplifyWhileDoSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) return nullptr;
    SimpCtx local(ctx->getOptCtx());
    local.copyTopDownFlag(*ctx);
    IR * ret_list = nullptr;
    ASSERTN(ir->is_whiledo(), ("expect IR_WHILE_DO node"));
    LabelInfo * startl = m_rg->genILabel();

    //det exp
    //When we first lowering CFS,
    //det-expression should not be TRUEBR/FASLEBR.
    ASSERT0(LOOP_det(ir)->is_judge());
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    IR * det = simplifyDet(LOOP_det(ir), &tcont);
    IR * last = xcom::removetail(&det);
    ASSERTN(last->is_exp(), ("invalide det exp"));
    if (!last->is_judge()) {
        //det-expression should be judgement.
        last = m_irmgr->buildJudge(last);
    }

    IR * falsebr = m_irmgr->buildBranch(false, last, m_rg->genILabel());
    copyDbx(falsebr, ir, m_rg);
    xcom::add_next(&det, falsebr);

    SIMP_break_label(&local) = BR_lab(falsebr);
    SIMP_continue_label(&local) = startl;

    //loop body
    IR * body = simplifyStmtList(LOOP_body(ir), &local);
    xcom::add_next(&body, m_irmgr->buildGoto(startl));

    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = nullptr;
        ASSERT0(SIMP_cfs_mgr(ctx));
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_WHILE_DO);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(falsebr, ci);
        CFS_INFO_ir(ci) = falsebr;
        SIMP_cfs_mgr(ctx)->recordStmt(body, *CFS_INFO_loop_body(ci));

        //Falsebr is executed each iter.
        CFS_INFO_loop_body(ci)->bunion(IR_id(falsebr));
    }

    xcom::add_next(&ret_list, m_irmgr->buildLabel(startl));
    xcom::add_next(&ret_list, det);
    xcom::add_next(&ret_list, body);
    xcom::add_next(&ret_list, m_irmgr->buildLabel(BR_lab(falsebr)));
    xoc::cleanParentForIRList(ret_list);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return ret_list;
}


//Transform do-while to:
//  LABEL: start
//  BODY-STMT
//  LABEL: det_start
//  DO-WHILE-DET
//  TRUEBR start
IR * IRSimp::simplifyDoWhileSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) return nullptr;
    SimpCtx local(ctx->getOptCtx());
    local.copyTopDownFlag(*ctx);
    IR * ret_list = nullptr;
    ASSERTN(ir->is_dowhile(), ("expect IR_DO_WHILE node"));

    LabelInfo * startl = m_rg->genILabel();
    LabelInfo * endl = m_rg->genILabel();
    LabelInfo * det_startl = m_rg->genILabel();

    //det exp
    //When we first lowering CFS, det-expression should not be TRUEBR/FASLEBR.
    ASSERT0(LOOP_det(ir)->is_judge());
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    IR * det = simplifyDet(LOOP_det(ir), &tcont);
    IR * last = removetail(&det);
    ASSERTN(last->is_exp(), ("invalide det exp"));
    if (!last->is_judge()) {
        //det-expression should be judgement.
        last = m_irmgr->buildJudge(last);
    }

    IR * truebr = m_irmgr->buildBranch(true, last, startl);
    copyDbx(truebr, ir, m_rg);
    xcom::add_next(&det, truebr);

    SIMP_break_label(&local) = endl;
    SIMP_continue_label(&local) = det_startl;

    IR * body = simplifyStmtList(LOOP_body(ir), &local);
    insertbefore_one(&body, body, m_irmgr->buildLabel(startl));

    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = nullptr;
        ASSERT0(SIMP_cfs_mgr(ctx));
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_DO_WHILE);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(truebr, ci);
        CFS_INFO_ir(ci) = truebr;
        SIMP_cfs_mgr(ctx)->recordStmt(body, *CFS_INFO_loop_body(ci));

        //'truebr' is executed during each iteration.
        CFS_INFO_loop_body(ci)->bunion(IR_id(truebr));
    }

    last = xcom::get_last(ret_list);
    xcom::add_next(&ret_list, &last, body);
    xcom::add_next(&ret_list, &last, m_irmgr->buildLabel(det_startl));
    xcom::add_next(&ret_list, &last, det);
    xcom::add_next(&ret_list, &last, m_irmgr->buildLabel(endl));
    xoc::cleanParentForIRList(ret_list);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return ret_list;
}


//Transform do-loop to:
//  INIT-STMT
//  LABEL: start
//    WHILE-DO-DET
//  FALSEBR L1
//  BODY-STMT
//  LABEL: step
//  STEP
//  GOTO start
//  LABEL: L1
IR * IRSimp::simplifyDoLoopSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) return nullptr;
    SimpCtx local(ctx->getOptCtx());
    local.copyTopDownFlag(*ctx);
    IR * ret_list = nullptr;
    ASSERTN(ir->is_doloop(), ("expect IR_DO_LOOP node"));

    LabelInfo * startl = m_rg->genILabel();

    IR * iv = simplifyExpression(LOOP_iv(ir), &local);
    ASSERT0(iv->is_id() || iv->is_pr());

    //det exp
    //When we first lowering CFS, det-expression should not be TRUEBR/FASLEBR.
    ASSERT0(LOOP_det(ir)->is_judge());
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    IR * upperbound = simplifyDet(LOOP_det(ir), &tcont);
    IR * last = xcom::removetail(&upperbound);
    ASSERTN(last->is_exp(), ("invalide det exp"));
    //det-expression should be judgement.
    //last = m_irmgr->buildCmp(IR_LE, iv->is_id() ?
    //    m_irmgr->buildLoad(ID_info(iv)) : iv, last);
    IR * falsebr = m_irmgr->buildBranch(false, last, m_rg->genILabel());
    copyDbx(falsebr, LOOP_det(ir), m_rg);

    LabelInfo * stepl = m_rg->genILabel();

    SIMP_break_label(&local) = BR_lab(falsebr);
    SIMP_continue_label(&local) = stepl;

    IR * init = simplifyExpression(LOOP_init(ir), &local);
    if (iv->is_id()) {
        init = m_irmgr->buildStore(ID_info(iv), init);
    } else {
        init = m_irmgr->buildStorePR(PR_no(iv), iv->getType(), init);
    }
    IR * step = simplifyExpression(LOOP_step(ir), &local);
    IR * body = simplifyStmtList(LOOP_body(ir), &local);

    //step label, for simp 'continue' used
    xcom::add_next(&body, m_irmgr->buildLabel(stepl));
    if (iv->is_id()) {
        xcom::add_next(&body, m_irmgr->buildStore(ID_info(iv), step));
    } else {
        xcom::add_next(&body, m_irmgr->buildStorePR(PR_no(iv),
                                                    iv->getType(), step));
    }
    xcom::add_next(&body, m_irmgr->buildGoto(startl));

    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = nullptr;
        ASSERT0(SIMP_cfs_mgr(ctx) != nullptr);
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_DO_LOOP);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(falsebr, ci);
        CFS_INFO_ir(ci) = falsebr;
        SIMP_cfs_mgr(ctx)->recordStmt(body, *CFS_INFO_loop_body(ci));

        //'falsebr' is executed during each iteration.
        CFS_INFO_loop_body(ci)->bunion(IR_id(falsebr));
    }

    xcom::add_next(&ret_list, init);
    xcom::add_next(&ret_list, m_irmgr->buildLabel(startl));
    xcom::add_next(&ret_list, falsebr);
    xcom::add_next(&ret_list, body);
    xcom::add_next(&ret_list, m_irmgr->buildLabel(BR_lab(falsebr)));
    xoc::cleanParentForIRList(ret_list);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return ret_list;
}


//Simplify determination of Control Flow Structure.
IR * IRSimp::simplifyDet(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) return nullptr;
    IR * ret_exp_list = nullptr;
    IR * next;
    while (ir != nullptr) {
        next = ir->get_next();
        IR_next(ir) = IR_prev(ir) = nullptr;
        if (ir->is_stmt()) {
            SimpCtx tcont(*ctx);
            IR * new_stmt_list = simplifyStmt(ir, &tcont);
            ctx->unionBottomUpInfo(tcont);

            #ifdef _DEBUG_
            IR * x = new_stmt_list;
            while (x != nullptr) {
                ASSERT0(x->is_stmt());
                x = x->get_next();
            }
            #endif
            ctx->appendStmt(new_stmt_list);
        } else if (ir->is_exp()) {
            SimpCtx tcont(*ctx);
            IR * new_exp = simplifyJudgeDet(ir, &tcont);
            ASSERT0(new_exp->is_exp());
            xcom::add_next(&ret_exp_list, new_exp);
            ctx->appendStmt(tcont);
            ctx->unionBottomUpInfo(tcont);
        } else {
            ASSERTN(0, ("unknonw IR code"));
        }
        ir = next;
    }
    xoc::cleanParentForIRList(ret_exp_list);
    return ret_exp_list;
}


//Return expression's value.s
//e.g: a = !(exp), where rhs would be translated to:
//    ----------
//    truebr(exp != 0), L1
//    pr = 1
//    goto L2
//    L1:
//    pr = 0
//    L2:
//    ----------
//    a = pr
//In the case, return 'pr'.
IR * IRSimp::simplifyLogicalNot(IN IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_lnot());
    LabelInfo * label1 = m_rg->genILabel();
    IR * pr = m_irmgr->buildPR(ir->getType());
    m_rg->getMDMgr()->allocRef(pr);
    IR * ret_list = nullptr;

    //truebr(exp != 0), L1
    IR * opnd0 = UNA_opnd(ir);
    UNA_opnd(ir) = nullptr;
    if (!opnd0->is_judge()) {
        opnd0 = m_irmgr->buildJudge(opnd0);
    }
    IR * true_br = m_irmgr->buildBranch(true, opnd0, label1);
    copyDbx(true_br, ir, m_rg);
    xcom::add_next(&ret_list, true_br);

    TypeMgr * dm = m_tm;
    //pr = 1
    Type const* t = dm->getSimplexTypeEx(
        dm->getAlignedDType(WORD_LENGTH_OF_TARGET_MACHINE, true));
    IR * imm0 = m_irmgr->buildImmInt(1, t);
    IR * x = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), imm0);
    m_rg->getMDMgr()->allocRef(x);
    copyDbx(x, imm0, m_rg);
    xcom::add_next(&ret_list, x);

    //goto L2
    LabelInfo * label2 = m_rg->genILabel();
    xcom::add_next(&ret_list, m_irmgr->buildGoto(label2));

    //L1:
    xcom::add_next(&ret_list, m_irmgr->buildLabel(label1));

    //pr = 0
    Type const* t2 = dm->getSimplexTypeEx(
        dm->getAlignedDType(WORD_LENGTH_OF_TARGET_MACHINE, true));
    IR * imm1 = m_irmgr->buildImmInt(0, t2);

    IR * x2 = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), imm1);
    m_rg->getMDMgr()->allocRef(x2);
    copyDbx(x2, imm1, m_rg);
    xcom::add_next(&ret_list, x2);

    //L2:
    xcom::add_next(&ret_list, m_irmgr->buildLabel(label2));
    ctx->appendStmt(ret_list);
    m_rg->freeIRTree(ir);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return pr;
}


//Return expression's value.
//e.g: a = b&&c, where rhs would be translated to:
//  ----------
//  falsebr(b != 0), L0
//  truebr(c != 0), L1
//  L0:
//  pr = 0
//  goto L2
//  L1:
//  pr = 1
//  L2:
//  ----------
//  a = pr
//In the case, return 'pr'.
IR * IRSimp::simplifyLogicalAnd(IN IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_land());
    LabelInfo * label1 = m_rg->genILabel();
    IR * pr = m_irmgr->buildPR(ir->getType());
    m_rg->getMDMgr()->allocRef(pr);
    IR * ret_list = simplifyLogicalAndAtTruebr(ir, label1);
    TypeMgr * tm = m_tm;
    Type const* t = tm->getSimplexTypeEx(tm->getAlignedDType(
        WORD_LENGTH_OF_TARGET_MACHINE, true));
    IR * imm0 = m_irmgr->buildImmInt(0, t);
    IR * x = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), imm0);
    m_rg->getMDMgr()->allocRef(x);
    copyDbx(x, imm0, m_rg);
    xcom::add_next(&ret_list, x);

    LabelInfo * label2 = m_rg->genILabel();
    xcom::add_next(&ret_list, m_irmgr->buildGoto(label2));
    xcom::add_next(&ret_list, m_irmgr->buildLabel(label1));
    Type const* t2 = tm->getSimplexTypeEx(tm->getAlignedDType(
        WORD_LENGTH_OF_TARGET_MACHINE, true));
    IR * imm1 = m_irmgr->buildImmInt(1, t2);
    IR * x2 = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), imm1);
    m_rg->getMDMgr()->allocRef(x2);
    copyDbx(x2, imm1, m_rg);
    xcom::add_next(&ret_list, x2);
    xcom::add_next(&ret_list, m_irmgr->buildLabel(label2));
    ctx->appendStmt(ret_list);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return pr;
}


//L1 is tgt_label.
//e.g: b&&c,L1
//would be translated to:
//  ----------
//  falsebr(b != 0), L2
//  truebrbr(c != 0), L1
//  L2:
//  ----------
//NOTE: ir's parent can NOT be FALSEBR.
IR * IRSimp::simplifyLogicalAndAtTruebr(IR * ir, LabelInfo const* tgt_label)
{
    ASSERT0(ir->is_land() && tgt_label != nullptr);
    IR * ret_list = nullptr;

    //Process opnd0.
    IR * opnd0 = BIN_opnd0(ir);
    BIN_opnd0(ir) = nullptr;
    if (!opnd0->is_judge()) {
        opnd0 = m_irmgr->buildJudge(opnd0);
    }
    //Generate falsebranch label.
    LabelInfo * lab = m_rg->genILabel();

    IR * br = m_irmgr->buildBranch(false, opnd0, lab);
    copyDbx(br, ir, m_rg);
    xcom::add_next(&ret_list, br);

    //Process opnd1.
    IR * opnd1 = BIN_opnd1(ir);
    BIN_opnd1(ir) = nullptr;
    if (!opnd1->is_judge()) {
        opnd1 = m_irmgr->buildJudge(opnd1);
    }
    br = m_irmgr->buildBranch(true, opnd1, tgt_label);
    copyDbx(br, ir, m_rg);
    xcom::add_next(&ret_list, br);

    //Add false-branch label.
    xcom::add_next(&ret_list, m_irmgr->buildLabel(lab));
    m_rg->freeIRTree(ir);
    return ret_list;
}


//L1 is tgt_label.
//e.g: falsebr(b&&c),L1
//would be translated to:
//  ----------
//  falsebr(b != 0), L2
//  truebrbr(c != 0), L1
//  L2:
//  ----------
//NOTE: ir's parent must be FALSEBR.
IR * IRSimp::simplifyLogicalAndAtFalsebr(IR * ir, LabelInfo const* tgt_label)
{
    ASSERT0(ir->is_land() && tgt_label != nullptr);
    IR * ret_list = nullptr;

    //Process opnd0.
    IR * opnd0 = BIN_opnd0(ir);
    BIN_opnd0(ir) = nullptr;
    if (!opnd0->is_judge()) {
        opnd0 = m_irmgr->buildJudge(opnd0);
    }
    IR * false_br = m_irmgr->buildBranch(false, opnd0, tgt_label);
    copyDbx(false_br, ir, m_rg);
    xcom::add_next(&ret_list, false_br);

    //Process opnd1
    IR * opnd1 = BIN_opnd1(ir);
    BIN_opnd1(ir) = nullptr;
    if (!opnd1->is_judge()) {
        opnd1 = m_irmgr->buildJudge(opnd1);
    }
    false_br = m_irmgr->buildBranch(false, opnd1, tgt_label);
    copyDbx(false_br, ir, m_rg);
    xcom::add_next(&ret_list, false_br);
    m_rg->freeIRTree(ir);
    return ret_list;
}


//e.g: b||c,L1
//would be translated to:
//  ----------
//  truebr(b != 0), L1
//  truebr(c != 0), L1
//  ----------
//or
//  ----------
//  truebr(b != 0), L1
//  pr = (c != 0)
//  ----------
//NOTE: ir's parent can NOT be FALSEBR.
IR * IRSimp::simplifyLogicalOrAtTruebr(IR * ir, LabelInfo const* tgt_label)
{
    ASSERT0(ir->is_lor() && tgt_label != nullptr);
    IR * ret_list = nullptr;

    //Process opnd0.
    IR * opnd0 = BIN_opnd0(ir);
    BIN_opnd0(ir) = nullptr;
    if (!opnd0->is_judge()) {
        opnd0 = m_irmgr->buildJudge(opnd0);
    }
    IR * true_br = m_irmgr->buildBranch(true, opnd0, tgt_label);
    copyDbx(true_br, ir, m_rg);
    xcom::add_next(&ret_list, true_br);

    //Process opnd1.
    IR * opnd1 = BIN_opnd1(ir);
    BIN_opnd1(ir) = nullptr;
    if (!opnd1->is_judge()) {
        opnd1 = m_irmgr->buildJudge(opnd1);
    }
    IR * op = nullptr;
    //if (SIMP_lnot(ctx)) {
        op = m_irmgr->buildBranch(true, opnd1, tgt_label);
    //} else {
    //    In the case ir's parent is if(a||b),L1, generate STORE is invalid.
    //    ASSERT0(res_pr != nullptr);
    //    op = buildStore(res_pr, opnd1);
    //}
    copyDbx(op, ir, m_rg);
    xcom::add_next(&ret_list, op);
    m_rg->freeIRTree(ir);
    return ret_list;
}


//e.g: falsebr(b||c),L1
//would be translated to:
//  ----------
//  truebr(b != 0), L2
//  falsebr(c != 0), L1
//  L2:
//  ----------
//NOTE: ir's parent must be be FALSEBR.
IR * IRSimp::simplifyLogicalOrAtFalsebr(IR * ir, LabelInfo const* tgt_label)
{
    ASSERT0(ir->is_lor() && tgt_label != nullptr);
    IR * ret_list = nullptr;

    //ir is FALSEBR
    IR * opnd0 = BIN_opnd0(ir);
    BIN_opnd0(ir) = nullptr;
    if (!opnd0->is_judge()) {
        opnd0 = m_irmgr->buildJudge(opnd0);
    }
    LabelInfo * true_lab = m_rg->genILabel();
    IR * true_br = m_irmgr->buildBranch(true, opnd0, true_lab);
    copyDbx(true_br, ir, m_rg);
    xcom::add_next(&ret_list, true_br);

    IR * opnd1 = BIN_opnd1(ir);
    BIN_opnd1(ir) = nullptr;
    if (!opnd1->is_judge()) {
        opnd1 = m_irmgr->buildJudge(opnd1);
    }
    IR * false_br = m_irmgr->buildBranch(false, opnd1, tgt_label);
    copyDbx(false_br, ir, m_rg);
    xcom::add_next(&ret_list, false_br);
    xcom::add_next(&ret_list, m_irmgr->buildLabel(true_lab));
    m_rg->freeIRTree(ir);
    return ret_list;
}


//Return expression's value.
//e.g: a = b||c, where rhs would be translated to:
//  ----------
//  truebr(b != 0), L1
//  truebr(c != 0), L1
//  pr = 0
//  goto L2
//  L1:
//  pr = 1
//  L2:
//  ----------
//  a = pr
//or
//  ----------
//  truebr(b != 0), L1
//  pr = (c != 0)
//  goto L2
//  L1:
//  pr = 1
//  L2:
//  ----------
//  a = pr
//
//In the case, return 'pr'.
IR * IRSimp::simplifyLogicalOr(IN IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_lor());
    LabelInfo * label1 = m_rg->genILabel();
    IR * pr = m_irmgr->buildPR(ir->getType());
    m_rg->getMDMgr()->allocRef(pr);
    IR * ret_list = simplifyLogicalOrAtTruebr(ir, label1);
    TypeMgr * dm = m_tm;
    Type const* type = dm->getSimplexTypeEx(dm->getAlignedDType(
        WORD_LENGTH_OF_TARGET_MACHINE, true));
    IR * imm0 = m_irmgr->buildImmInt(0, type);
    IR * x = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), imm0);
    m_rg->getMDMgr()->allocRef(x);
    copyDbx(x, imm0, m_rg);
    xcom::add_next(&ret_list, x);

    LabelInfo * label2 = m_rg->genILabel();
    xcom::add_next(&ret_list, m_irmgr->buildGoto(label2));
    xcom::add_next(&ret_list, m_irmgr->buildLabel(label1));

    type = dm->getSimplexTypeEx(dm->getAlignedDType(
        WORD_LENGTH_OF_TARGET_MACHINE, true));
    IR * imm1 = m_irmgr->buildImmInt(1, type);
    IR * x2 = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), imm1);
    m_rg->getMDMgr()->allocRef(x2);
    copyDbx(x2, imm1, m_rg);
    xcom::add_next(&ret_list, x2);
    xcom::add_next(&ret_list, m_irmgr->buildLabel(label2));
    ctx->appendStmt(ret_list);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return pr;
}


//Simplify logical OR, logical AND operations into comparision operations.
//Return generate IR stmts.
IR * IRSimp::simplifyLogicalDet(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) { return nullptr; }
    ASSERT0(ir->isConditionalBr());
    ASSERT0(BR_det(ir)->is_logical());
    IR * ret_list = nullptr;
    if (BR_det(ir)->is_lor()) {
        if (ir->is_truebr()) {
            ret_list = simplifyLogicalOrAtTruebr(BR_det(ir), BR_lab(ir));
            BR_det(ir) = nullptr;
            m_rg->freeIRTree(ir);
            SIMP_changed(ctx) = true;
            SIMP_need_recon_bblist(ctx) = true;
            return ret_list;
        }

        //ir is FALSEBR
        ret_list = simplifyLogicalOrAtFalsebr(BR_det(ir), BR_lab(ir));
        BR_det(ir) = nullptr;
        m_rg->freeIRTree(ir);
        SIMP_changed(ctx) = true;
        SIMP_need_recon_bblist(ctx) = true;
        return ret_list;
    } else if (BR_det(ir)->is_land()) {
        if (ir->is_truebr()) {
            ret_list = simplifyLogicalAndAtTruebr(BR_det(ir), BR_lab(ir));
            BR_det(ir) = nullptr;
            m_rg->freeIRTree(ir);
            SIMP_changed(ctx) = true;
            SIMP_need_recon_bblist(ctx) = true;
            return ret_list;
        }

        //ir is FALSEBR
        ret_list = simplifyLogicalAndAtFalsebr(BR_det(ir), BR_lab(ir));
        BR_det(ir) = nullptr;
        m_rg->freeIRTree(ir);
        SIMP_changed(ctx) = true;
        SIMP_need_recon_bblist(ctx) = true;
        return ret_list;
    } else if (BR_det(ir)->is_lnot()) {
        if (ir->is_truebr()) {
            IR_code(ir) = IR_FALSEBR;
        } else {
            IR_code(ir) = IR_TRUEBR;
        }
        BR_det(ir) = UNA_opnd(BR_det(ir));
        if (!BR_det(ir)->is_judge()) {
            IR * old = BR_det(ir);
            BR_det(ir) = m_irmgr->buildJudge(old);
            copyDbx(BR_det(ir), old, m_rg);
        }
        IR_parent(BR_det(ir)) = ir;
        return simplifyStmt(ir, ctx);
    }
    ASSERTN(0, ("TODO"));
    return ret_list;
}


//Simplify kids of SELECT.
//Does not change ir itself.
void IRSimp::simplifySelectKids(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir && ir->is_select());
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        IR * new_kid = simplifyExpression(kid, ctx);

        if (SIMP_to_lowest_height(ctx)) {
            if (!new_kid->is_leaf()) {
                //Lower new_kid to PR.
                ir->setKid(i, simpToPR(new_kid, ctx));
                continue;
            }
        }

        ir->setKid(i, new_kid);
    }
}


//Generate comparision and branch.
IR * IRSimp::simplifySelect(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) { return nullptr; }
    ASSERTN(ir->is_select(), ("expect select node"));
    if (!SIMP_select(ctx)) {
        simplifySelectKids(ir, ctx);
        if (!SIMP_to_lowest_height(ctx) || isLowestHeightExp(ir, ctx)) {
            return ir;
        }

        //Lower SELECT as a leaf node.
        return simpToPR(ir, ctx);
    }

    //Transform SELECT to:
    // falsebr det, label(ELSE_START)
    // res = true_exp
    // goto END
    // ELSE_START:
    // res = false_exp
    // END:
    SimpCtx predctx(*ctx);
    SIMP_ret_array_val(&predctx) = true;

    //Simplify determinant.
    ASSERT0(SELECT_det(ir));
    IR * newpred = simplifyExpression(SELECT_det(ir), &predctx);
    ASSERT0(newpred->is_single());
    ctx->appendStmt(predctx);

    //Build false-branch.
    if (!newpred->is_judge()) {
        newpred = m_irmgr->buildJudge(newpred);
    }
    IR * falsebr = m_irmgr->buildBranch(false, newpred, m_rg->genILabel());
    copyDbx(falsebr, SELECT_det(ir), m_rg);

    IR * lst = nullptr;
    IR * last = nullptr;
    xcom::add_next(&lst, &last, falsebr);

    //Trueexp's type may be different to Falseexp.
    //ASSERT0(SELECT_trueexp(ir)->getType() == SELECT_falseexp(ir)->getType());
    IR * res = m_irmgr->buildPR(ir->getType());
    m_rg->getMDMgr()->allocRef(res);

    //Simplify true exp.
    SimpCtx truectx(*ctx);
    SIMP_ret_array_val(&truectx) = true;
    IR * true_exp = simplifyExpression(SELECT_trueexp(ir), &truectx);
    ctx->appendStmt(truectx);
    IR * mv = m_irmgr->buildStorePR(PR_no(res), res->getType(), true_exp);
    m_rg->getMDMgr()->allocRef(mv);
    copyDbx(mv, true_exp, m_rg);
    xcom::add_next(&lst, &last, mv);

    //Simplify false expression.
    ASSERT0(SELECT_falseexp(ir) != nullptr);
    //append GOTO following end of true body
    IR * gotoend = m_irmgr->buildGoto(m_rg->genILabel());
    copyDbx(gotoend, SELECT_det(ir), m_rg);
    xcom::add_next(&lst, &last, gotoend);

    //Generate true body END label.
    xcom::add_next(&lst, &last, m_irmgr->buildLabel(BR_lab(falsebr)));

    //Simplify false expression.
    SimpCtx falsectx(*ctx);
    SIMP_ret_array_val(&falsectx) = true;
    IR * else_exp = simplifyExpression(SELECT_falseexp(ir), &falsectx);
    ctx->appendStmt(falsectx);
    IR * mv2 = m_irmgr->buildStorePR(PR_no(res), res->getType(), else_exp);
    m_rg->getMDMgr()->allocRef(mv2);
    copyDbx(mv2, else_exp, m_rg);
    xcom::add_next(&lst, &last, mv2);
    //---

    //Generate the last END label.
    xcom::add_next(&lst, &last, m_irmgr->buildLabel(GOTO_lab(gotoend)));

    xoc::cleanParentForIRList(lst);
    ctx->appendStmt(lst);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = nullptr;
        ASSERT0(SIMP_cfs_mgr(ctx) != nullptr);
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_IF);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(falsebr, ci);
        CFS_INFO_ir(ci) = falsebr;
        SIMP_cfs_mgr(ctx)->recordStmt(true_exp, *CFS_INFO_true_body(ci));
        SIMP_cfs_mgr(ctx)->recordStmt(else_exp, *CFS_INFO_false_body(ci));
    }
    SIMP_need_rebuild_pr_du_chain(ctx) = true; //inform PRSSA rebuild.
    return res;
}


IR * IRSimp::simplifyIgoto(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) { return nullptr; }
    ASSERTN(ir->is_igoto(), ("expect igoto"));

    IGOTO_vexp(ir) = simplifyExpression(IGOTO_vexp(ir), ctx);
    return ir;
}


IR * IRSimp::simplifySwitchSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) { return nullptr; }
    ASSERTN(ir->is_switch(), ("expect switch node"));

    IR * vexp_stmt = nullptr;
    IR * swt_val = SWITCH_vexp(ir);
    if (!swt_val->is_pr()) {
        SimpCtx vexpctx(ctx->getOptCtx());
        swt_val = simpToPR(swt_val, &vexpctx);
        vexp_stmt = SIMP_stmtlist(&vexpctx);
    }
    SWITCH_vexp(ir) = nullptr;

    IR * case_lst = xcom::get_last(SWITCH_case_list(ir));
    IR * prev_ir_tree = nullptr;
    if (case_lst == nullptr) {
        //Switch body is useless and neglected.
        return nullptr;
    }

    LabelInfo * switch_endlab = nullptr;

    //Simplify CASE list into IF as default to enable
    //more advanced middle level optimizations.
    if (SWITCH_deflab(ir) != nullptr) {
        prev_ir_tree = m_irmgr->buildGoto(SWITCH_deflab(ir));
        copyDbx(prev_ir_tree, ir, m_rg);
        SWITCH_deflab(ir) = nullptr;
    } else {
        if (switch_endlab == nullptr) {
            switch_endlab = m_rg->genILabel();
        }
        IR * goto_switch_end = m_irmgr->buildGoto(switch_endlab);
        xcom::add_next(&prev_ir_tree, goto_switch_end);
        copyDbx(goto_switch_end, ir, m_rg);
    }

    for (; case_lst != nullptr; case_lst = IR_prev(case_lst)) {
        IR * ifstmt = m_irmgr->buildIf(
            m_irmgr->buildCmp(IR_EQ, m_rg->dupIRTree(swt_val),
                              CASE_vexp(case_lst)),
            m_irmgr->buildGoto(CASE_lab(case_lst)),
            prev_ir_tree);
        copyDbx(ifstmt, case_lst, m_rg);
        CASE_vexp(case_lst) = nullptr;
        prev_ir_tree = ifstmt;
    }

    xcom::add_next(&prev_ir_tree, SWITCH_body(ir));
    SWITCH_body(ir) = nullptr;

    if (switch_endlab != nullptr) {
        xcom::add_next(&prev_ir_tree, m_irmgr->buildLabel(switch_endlab));
    }

    if (SIMP_if(ctx)) {
        //Simpilify IF to TRUEBR/FALSEBR.
        //Generate the ending-label of SWITCH to serve as the target
        //branch label of TRUEBR/FALSEBR.
        if (switch_endlab == nullptr) {
            switch_endlab = m_rg->genILabel();
            xcom::add_next(&prev_ir_tree, m_irmgr->buildLabel(switch_endlab));
        }

        SimpCtx tctx(*ctx);
        SIMP_break_label(&tctx) = switch_endlab;
        prev_ir_tree = simplifyStmtList(prev_ir_tree, &tctx);
    }

    xoc::cleanParentForIRList(prev_ir_tree);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;

    IR * ret_list = prev_ir_tree;
    if (vexp_stmt != nullptr) {
        xcom::add_next(&vexp_stmt, prev_ir_tree);
        ret_list = vexp_stmt;
    }
    m_rg->freeIRTree(ir);
    return ret_list;
}


//The function return a subscript expression to linearize array accessing.
static IR * simplifySubExpList(IR * ir, IRSimp * simp, SimpCtx * ctx)
{
    ASSERT0(ir->isArrayOp());
    Region * rg = simp->getRegion();
    IRMgr * irmgr = rg->getIRMgr();
    //For n dimension array, elemnum records the number
    //of elements at 0~(n-1) dimension.
    UINT elemnum = 0;
    UINT dim = 0;
    TMWORD const* elemnumbuf = ARR_elem_num_buf(ir);
    Type const* indextyid = rg->getTargetMachineArrayIndexType();
    IR * ofst_exp = nullptr;
    for (IR * s = xcom::removehead(&ARR_sub_list(ir));
         s != nullptr; dim++, s = xcom::removehead(&ARR_sub_list(ir))) {
        SimpCtx tcont(*ctx);
        SIMP_ret_array_val(&tcont) = true;
        IR * newsub = simp->simplifyExpression(s, &tcont);
        Dbx * dbx = xoc::getDbx(s);
        ctx->appendStmt(tcont);
        ctx->unionBottomUpInfo(tcont);

        //'ir' is exact array.
        //CASE1: elem-type v[n]
        //    can simply to: &v + n*sizeof(BASETYPE)
        //CASE2: a = (*p)[n]
        //can simply to: (*p) + n*sizeof(BASETYPE)
        //
        //CASE3: struct S {int a, b;} s[10];
        //  the elem_ty is struct S.
        //  s[1].b has ARR_ofst(ir)==4
        //  can simply to: &s + 1*sizeof(struct S) + offset(4)
        if (newsub->is_const() && newsub->is_int()) {
            //Subexp is const.
            if (elemnum != 0) {
                IR * t = irmgr->buildImmInt(((HOST_INT)elemnum) *
                                            CONST_int_val(newsub), indextyid);
                rg->freeIRTree(newsub);
                newsub = t;
            }
        } else {
            if (elemnum != 0) {
                newsub = irmgr->buildBinaryOp(IR_MUL, indextyid, newsub,
                    irmgr->buildImmInt(elemnum, indextyid));

                if (dbx != nullptr) { 
                    xoc::copyDbxForList(newsub, s, rg);
                }
            }
        }

        if (ofst_exp == nullptr) {
            ofst_exp = newsub;
        } else {
            ofst_exp = irmgr->buildBinaryOpSimp(IR_ADD, indextyid,
                                                ofst_exp, newsub);
            if (dbx != nullptr) { 
                xoc::copyDbxForList(ofst_exp, s, rg);
            }
        }

        if (elemnumbuf == nullptr) { continue; }

        if (dim == 0) {
            //The element-size of dim0 may be zero.
            //e.g: User can define an array with zero size.
            //     In C-language: struct { int buf[]; };
            elemnum = (UINT)elemnumbuf[dim];
            continue;
        }

        //The higher dimesion size should not be zero, otherwise
        //we could not generate the address computation expression to
        //linearize the array, because it needs to know how many total
        //elements in dimension 0 to dimension dim-1.
        ASSERTN(elemnumbuf[dim - 1] != 0, ("Incomplete array dimension"));
        elemnum *= (UINT)elemnumbuf[dim];
    }
    return ofst_exp;
}


//Simplify array operator IR_ARRAY to a list of address computing expressions.
//ir: the IR_ARRAY/IR_STARRAY that to be simplified.
//Note m_rg function does not free ir becase ir's Reference info and
//DUSet is still useful.
//
//In C language, the ARRAY operator is also avaiable for
//dereference of pointer,
//e.g: char * p; p[x] = 10; the operator is actually invoke an ILD operator,
//    namely, its behavor is *(p + x) = 10, and the will generate:
//        t1 = [p]
//        t2 = t1 + x
//        [t2] = 10
//    In the contrast, char q[]; q[x] = 10, will generate:
//        t1 = &q
//        t2 = t1 + x
//        [t2] = 10
//For the sake of correctness, p[] already be converted to *p
//via replacing IR_ARRAY/IR_STARRAY with IR_ILD before
//coming into m_rg function.
IR * IRSimp::simplifyArrayAddrExp(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir && SIMP_array(ctx) && ir->isArrayOp());
    ASSERT0(ARR_sub_list(ir));

    TypeMgr * dm = m_tm; //may generate new pointer type.
    ASSERT0(ir->getTypeSize(dm) > 0);
    IR * ofst_exp = simplifySubExpList(ir, this, ctx);
    ASSERT0(ofst_exp && ofst_exp->is_single());
    Dbx * subexpdbx = getDbx(ofst_exp);

    Type const* indextyid = m_rg->getTargetMachineArrayIndexType();
    UINT elemsize = dm->getByteSize(ARR_elemtype(ir));
    if (elemsize != 1) {
        //e.g: short g[i], subexp is i*sizeof(short)
        ofst_exp = m_irmgr->buildBinaryOp(IR_MUL, indextyid, ofst_exp,
            m_irmgr->buildImmInt(elemsize, indextyid));
        if (subexpdbx != nullptr) {
            xoc::setDbx(ofst_exp, subexpdbx, m_rg);
        }
    }

    if (ARR_ofst(ir) != 0) {
        //CASE: struct S {int a, b;} s[10];
        //the elem_ty is struct S.
        //s[1].b has ARR_ofst(ir)==4
        //can simply to: 1*sizeof(struct S) + offset(4) + LDA(s).
        IR * imm = m_irmgr->buildImmInt((HOST_INT)(ARR_ofst(ir)),
                                        indextyid);
        ofst_exp = m_irmgr->buildBinaryOpSimp(IR_ADD, indextyid,
                                              ofst_exp, imm);
        if (subexpdbx != nullptr) {
            xoc::setDbx(ofst_exp, subexpdbx, m_rg);
        }
    }

    ASSERT0(ARR_base(ir) &&
            (ARR_base(ir)->is_ptr() || ARR_base(ir)->is_any()));
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = false;
    IR * newbase = simplifyExpression(ARR_base(ir), &tcont);
    ctx->appendStmt(tcont);
    ctx->unionBottomUpInfo(tcont);

    ASSERT0(newbase && (newbase->is_ptr() || newbase->is_any()));
    ARR_base(ir) = nullptr;

    //'array_addr' is address of an ARRAY, and it is pointer type.
    //Given 'array_addr + n', the result-type of '+' must
    //be pointer type as well.
    //Note do not call buildBinaryOp(IR_ADD...) to generate ir.
    //Because that when 'sub' is pointer, the extra IR_MUL
    //operation will be generated.
    IR * array_addr = m_irmgr->buildBinaryOpSimp(IR_ADD,
        dm->getPointerType(ir->getTypeSize(dm)), newbase, ofst_exp);
    if (subexpdbx != nullptr) {
        xoc::setDbx(array_addr, subexpdbx, m_rg);
    }

    if (SIMP_to_pr_mode(ctx) && !array_addr->is_pr()) {
        SimpCtx ttcont(*ctx);
        SIMP_ret_array_val(&ttcont) = true;
        array_addr->setParentPointer(true);
        array_addr = simplifyExpression(array_addr, &ttcont);
        ctx->appendStmt(ttcont);
        ctx->unionBottomUpInfo(ttcont);
    }

    if (SIMP_to_lowest_height(ctx)) {
       //CASE: If IR_parent is nullptr, the situation is
       //caller attempt to enforce simplification to array expression
       //whenever possible.
       SimpCtx ttcont(*ctx);
       SIMP_ret_array_val(&ttcont) = true;
       array_addr = simplifyExpression(array_addr, &ttcont);
       ctx->appendStmt(ttcont);
       ctx->unionBottomUpInfo(ttcont);

       //IR * t = buildPR(array_addr->getType());
       //IR * mv = buildStorePR(PR_no(t), t->getType(), array_addr);
       //allocRefForPR(mv);
       //ctx->appendStmt(mv);
       //array_addr = t;
    }

    //freeIRTree(ir); //Should free ir in caller's function.
    SIMP_changed(ctx) = true;
    return array_addr;
}


IR * IRSimp::simplifyBinAndUniExpression(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir && (ir->isBinaryOp() || ir->isUnaryOp() || ir->is_ild()));

    if (ir->is_ild() && !SIMP_ild_ist(ctx)) { return ir; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            IR * x = simplifyExpression(kid, ctx);
            ir->setKid(i, x);
        }
    }

    if (!SIMP_to_lowest_height(ctx) || isLowestHeightExp(ir, ctx)) {
        return ir;
    }

    //Do lowering.
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k == nullptr) { continue; }

        //Lower ARRAY arith :
        //  a[i] + b[i]
        //=>
        //  t1 = a[i]
        //  t2 = b[i]
        //  t1 + t2
        if (SIMP_array_to_pr_mode(ctx) && k->is_array()) {
            ir->setKid(i, simpToPR(k, ctx));
        }
    }

    return simpToPR(ir, ctx);
}


IR * IRSimp::simpToPR(IR * ir, SimpCtx * ctx)
{
    IR * pr = m_irmgr->buildPR(ir->getType());
    m_rg->getMDMgr()->allocRef(pr);
    copyDbx(pr, ir, m_rg); //keep dbg info for new EXP.

    IR * stpr = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), ir);
    m_rg->getMDMgr()->allocRef(stpr);
    if (needBuildDUChain(*ctx)) {
        xoc::buildDUChain(stpr, pr, m_rg, *ctx->getOptCtx());
    }

    copyDbx(stpr, ir, m_rg); //keep dbg info for new STMT.
    ctx->appendStmt(stpr);
    SIMP_changed(ctx) = true;

    //Do NOT free ir here, caller will do that.
    return pr;
}


//Return new generated expression's value.
//'ir': ir may be in parameter list if its prev or next is not empty.
IR * IRSimp::simplifyExpression(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) { return nullptr; }
    ASSERTN(ir->is_exp(), ("expect non-statement node"));

    //ir can not in list, or it may incur illegal result.
    ASSERT0(ir->is_single());
    switch (ir->getCode()) {
    case IR_CONST: return ir;
    case IR_ID: return ir;
    SWITCH_CASE_DIRECT_MEM_EXP:
        if (SIMP_to_pr_mode(ctx)) {
            return simpToPR(ir, ctx);
        }
        return ir;
    SWITCH_CASE_READ_PR: return ir;
    SWITCH_CASE_READ_ARRAY: return simplifyArray(ir, ctx);
    case IR_LDA: //&sym, get address of 'sym'
        if (SIMP_to_pr_mode(ctx)) {
            return simpToPR(ir, ctx);
        }
        return ir;
    SWITCH_CASE_LOGIC:
    SWITCH_CASE_COMPARE:
        return simplifyJudgeDet(ir, ctx);
    SWITCH_CASE_ARITH:
    SWITCH_CASE_SHIFT:
    SWITCH_CASE_BITWISE:
    SWITCH_CASE_INDIRECT_MEM_EXP:
    case IR_NEG: //negative
    case IR_CVT: //data type convertion
        return simplifyBinAndUniExpression(ir, ctx);
    case IR_SELECT: return simplifySelect(ir, ctx);
    default: return simplifyExtExp(ir, ctx);
    }
    return nullptr;
}


//Return new generated expression's value.
IR * IRSimp::simplifyJudgeDet(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) return nullptr;
    ASSERT0(ir->is_judge());
    ASSERT0(ir->is_single());
    switch (ir->getCode()) {
    case IR_LAND: //logical and &&
    case IR_LOR: { //logical or ||
        if (SIMP_lor_land(ctx)) {
            SimpCtx tcont(*ctx);
            SIMP_ret_array_val(&tcont) = true;
            IR * newir = nullptr;
            if (ir->is_lor()) {
                newir = simplifyLogicalOr(ir, &tcont);
            } else {
                newir = simplifyLogicalAnd(ir, &tcont);
            }
            ctx->unionBottomUpInfo(tcont);

            ASSERT0(newir->is_exp());
            IR * lst = SIMP_stmtlist(&tcont);
            ASSERT0(newir != ir);
            SimpCtx t_tcont(tcont);
            lst = simplifyStmtList(lst, &t_tcont);
            ctx->appendStmt(lst);
            ir = newir;
        } else {
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * kid = ir->getKid(i);
                if (kid != nullptr) {
                    ir->setKid(i, simplifyExpression(kid, ctx));
                }
            }
        }

        if (SIMP_to_lowest_height(ctx) && !isLowest(ir)) {
            ir = simpToPR(ir, ctx);
        }
        return ir;
    }
    case IR_LNOT: //logical not
        if (SIMP_lnot(ctx)) {
            SimpCtx tcont(*ctx);
            SIMP_ret_array_val(&tcont) = true;
            IR * newir = simplifyLogicalNot(ir, &tcont);
            ASSERT0(newir->is_exp());
            IR * lst = SIMP_stmtlist(&tcont);
            ASSERTN(newir->is_pr(),
                   ("For now, newir will fairly be IR_PR. But it is not "
                    "certain in the future."));
            SimpCtx t_tcont(tcont);
            lst = simplifyStmtList(lst, &t_tcont);
            ctx->appendStmt(lst);
            ir = newir;
            ctx->unionBottomUpInfo(tcont);
        } else {
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * kid = ir->getKid(i);
                if (kid != nullptr) {
                    ir->setKid(i, simplifyExpression(kid, ctx));
                }
            }
        }

        if (SIMP_to_lowest_height(ctx) && !isLowest(ir)) {
            ir = simpToPR(ir, ctx);
        }
        return ir;
    case IR_EQ:
    case IR_NE:
    case IR_LT:
    case IR_GT:
    case IR_GE:
    case IR_LE: {
        ASSERT0(IR_parent(ir));
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * kid = ir->getKid(i);
            if (kid != nullptr) {
                ir->setKid(i, simplifyExpression(kid, ctx));
            }
        }

        if (SIMP_to_lowest_height(ctx) && !isLowest(ir)) {
            ir = simpToPR(ir, ctx);
        }
        return ir;
    }
    default: UNREACHABLE();
    } //end switch
    return nullptr;
}


//Simplify array's ingredient.
IR * IRSimp::simplifyArrayIngredient(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_array() || ir->is_starray());

    IR * stmtlist = nullptr;
    IR * last = nullptr;

    //Simplify base.
    SimpCtx basectx(*ctx);
    SIMP_ret_array_val(&basectx) = true;
    IR * newbase = simplifyExpression(ARR_base(ir), &basectx);
    if (SIMP_changed(&basectx)) {
        ARR_base(ir) = newbase;
        IR_parent(newbase) = ir;
    }
    ctx->unionBottomUpInfo(basectx);
    if (SIMP_stmtlist(&basectx) != nullptr) {
        xcom::add_next(&stmtlist, &last, SIMP_stmtlist(&basectx));
    }

    //Simplify sublist.
    IR * newsublist = nullptr;
    IR * newsublast = nullptr;
    IR * s = xcom::removehead(&ARR_sub_list(ir));
    for (; s != nullptr; s = xcom::removehead(&ARR_sub_list(ir))) {
        SimpCtx subctx(*ctx);
        SIMP_ret_array_val(&subctx) = true;
        IR * news = simplifyExpression(s, &subctx);

        if (SIMP_to_lowest_height(ctx) && !news->is_leaf()) {
            news = simpToPR(news, &subctx);
        }

        xcom::add_next(&newsublist, &newsublast, news);

        if (SIMP_stmtlist(&subctx) != nullptr) {
            xcom::add_next(&stmtlist, &last, SIMP_stmtlist(&subctx));
        }
        ctx->unionBottomUpInfo(subctx);
    }
    ARR_sub_list(ir) = newsublist;
    ir->setParent(newsublist);
    return stmtlist;
}


void IRSimp::simplifyStoreArrayRHS(IR * ir,
                                   OUT IR ** ret_list,
                                   OUT IR ** last,
                                   SimpCtx * ctx)
{
    ASSERT0(ir->is_starray());
    SimpCtx rhsctx(*ctx);
    SIMP_ret_array_val(&rhsctx) = true;
    IR * newrhs = simplifyExpression(STARR_rhs(ir), &rhsctx);
    if (SIMP_changed(&rhsctx)) {
        STARR_rhs(ir) = newrhs;
        IR_parent(newrhs) = ir;
    }
    ctx->unionBottomUpInfo(rhsctx);
    xcom::add_next(ret_list, last, SIMP_stmtlist(&rhsctx));
}


IR * IRSimp::simplifyStoreArrayAddr(IR * ir,
                                    OUT IR ** ret_list,
                                    OUT IR ** last,
                                    SimpCtx * ctx)
{
    ASSERT0(ir->is_starray());
    SimpCtx tcont(*ctx);
   //Simplify array address expression for STARRAY stmt.
    SIMP_ret_array_val(&tcont) = false; //We need array address expression.

    //simplyArray will free ir. That will make RHS invalid.
    STARR_rhs(ir) = nullptr;

    //Need to free ir in current function.
    IR * array_addr = simplifyArray(ir, &tcont);

    ctx->unionBottomUpInfo(tcont);
    xcom::add_next(ret_list, last, SIMP_stmtlist(&tcont));
    return array_addr;
}


IR * IRSimp::simplifyStoreArrayLHS(IR * ir,
                                   OUT IR ** ret_list,
                                   OUT IR ** last,
                                   SimpCtx * ctx)
{
    ASSERT0(ir->is_starray());
    Type const* type = ir->getType();
    IR * rhs = STARR_rhs(ir);
    IR * array_addr = simplifyStoreArrayAddr(ir, ret_list, last, ctx);
    IR * ret = m_irmgr->buildIStore(array_addr, rhs, type);
    ret->copyRef(ir, m_rg);
    xoc::copyDbx(ret, ir, m_rg);
    ret->copyAI(ir, m_rg);
    xoc::changeDef(ir, ret, m_rg);
    m_rg->freeIRTree(ir);
    return ret;
}


IR * IRSimp::simplifyStoreArray(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_starray());
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    IR * ret_list = nullptr;
    IR * last = nullptr;
    if (!SIMP_array(ctx)) {
        IR * stmtlst = simplifyArrayIngredient(ir, ctx);
        if (stmtlst != nullptr) {
            xcom::add_next(&ret_list, &last, stmtlst);
        }
    }

    simplifyStoreArrayRHS(ir, &ret_list, &last, ctx);

    IR * ret = nullptr;
    if (SIMP_array(ctx)) {
        ret = simplifyStoreArrayLHS(ir, &ret_list, &last, ctx);
    } else {
        ret = ir;
    }
    ASSERT0(ret->is_stmt());
    xcom::add_next(&ret_list, &last, ret);
    return ret_list;
}


IR * IRSimp::simplifyArrayAddrID(IR * ir, IR * array_addr, SimpCtx * ctx)
{
    ASSERT0(ir->is_array() && array_addr);
    IR * ld = m_irmgr->buildLoad(ID_info(array_addr),
                                          array_addr->getType());
    //Load variable which is an array.
    ld->copyRef(ir, m_rg);
    ld->copyAI(ir, m_rg);
    xoc::changeUse(ir, ld, m_rg);
    m_rg->freeIRTree(ir);
    m_rg->freeIRTree(array_addr);
    return ld;
}


IR * IRSimp::simplifyArrayPRMode(IR * ir, IR * array_addr, SimpCtx * ctx)
{
    ASSERT0(ir->is_array() && array_addr);
    if (!array_addr->is_pr()) {
        array_addr = simpToPR(array_addr, ctx);
    }

    //Load array element value.
    IR * elem_val = m_irmgr->buildILoad(array_addr, ir->getType());
    elem_val->copyRef(ir, m_rg);
    elem_val->copyAI(ir, m_rg);
    xoc::changeUse(ir, elem_val, m_rg);
    m_rg->freeIRTree(ir);

    IR * pr = m_irmgr->buildPR(elem_val->getType());
    m_rg->getMDMgr()->allocRef(pr);

    IR * stpr = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), elem_val);
    m_rg->getMDMgr()->allocRef(stpr);

    //keep dbg info for new STMT.
    copyDbx(stpr, elem_val, m_rg);
    ctx->appendStmt(stpr);
    return pr;
}


IR * IRSimp::simplifyArrayLowestHeight(IR * ir, IR * array_addr, SimpCtx * ctx)
{
    ASSERT0(ir->is_array() && array_addr);
    if (!array_addr->is_leaf()) {
        array_addr = simpToPR(array_addr, ctx);
    }

    //Load array element's value.
    IR * elem_val = m_irmgr->buildILoad(array_addr, ir->getType());
    elem_val->copyRef(ir, m_rg);
    elem_val->copyAI(ir, m_rg);
    xoc::changeUse(ir, elem_val, m_rg);
    m_rg->freeIRTree(ir);

    IR * pr = m_irmgr->buildPR(elem_val->getType());
    m_rg->getMDMgr()->allocRef(pr);

    IR * stpr = m_irmgr->buildStorePR(PR_no(pr), pr->getType(), elem_val);
    m_rg->getMDMgr()->allocRef(stpr);

    //keep dbg info for new STMT.
    copyDbx(stpr, array_addr, m_rg);
    ctx->appendStmt(stpr);
    if (needBuildDUChain(*ctx)) {
        xoc::buildDUChain(stpr, pr, m_rg, *ctx->getOptCtx());
    }
    return pr;
}


IR * IRSimp::simplifyArraySelf(IR * ir, IR * array_addr, SimpCtx * ctx)
{
    ASSERT0(ir->is_array() && array_addr);
    //Load array element value.
    IR * elem_val = m_irmgr->buildILoad(array_addr, ir->getType());
    elem_val->copyRef(ir, m_rg);
    xoc::changeUse(ir, elem_val, m_rg);
    elem_val->copyAI(ir, m_rg);
    m_rg->freeIRTree(ir);
    return elem_val;
}


//Simplify array operator.
//Note ir may be freed.
IR * IRSimp::simplifyArray(IR * ir, SimpCtx * ctx)
{
    if (!SIMP_array(ctx)) {
        IR * stmtlst = simplifyArrayIngredient(ir, ctx);
        if (stmtlst != nullptr) {
            ctx->appendStmt(stmtlst);
        }
        return ir;
    }

    IR * array_addr = simplifyArrayAddrExp(ir, ctx);
    //Need to free ir in current function.
    ASSERTN(!ir->is_undef(), ("ir has been freed"));

    SIMP_changed(ctx) = true;
    if (!SIMP_ret_array_val(ctx)) {
        return array_addr;
    }

    //Simplify array elem value.
    if (array_addr->is_id()) {
        return simplifyArrayAddrID(ir, array_addr, ctx);
    }

    if (SIMP_to_pr_mode(ctx)) {
        return simplifyArrayPRMode(ir, array_addr, ctx);
    }

    if (SIMP_to_lowest_height(ctx)) {
        return simplifyArrayLowestHeight(ir, array_addr, ctx);
    }

    return simplifyArraySelf(ir, array_addr, ctx);
}


bool IRSimp::simplifyCallParamList(IR * ir, IR ** ret_list, IR ** last,
                                   SimpCtx * ctx)
{
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    bool origin_to_lowest_height = SIMP_to_lowest_height(&tcont);
    bool changed = false;
    ASSERT0(ir->isCallStmt());
    IR * newp = nullptr;
    IR * newplast = nullptr;
    while (CALL_param_list(ir) != nullptr) {
        IR * p = xcom::removehead(&CALL_param_list(ir));
        if (g_is_simplify_parameter && !p->isMemOpnd() && !p->is_lda()) {
            //We always simplify parameters to the lowest height to
            //facilitate the query of point-to set.
            //e.g: DUMgr is going to compute may-point-to while
            //ADD is pointer type. But only MD can hold point-to set.
            //The query of point-to to ADD(id:6) is failed.
            //So we need to store the add's value to a PR,
            //so as to it could reserve point-to set.
            //e.g: call 'getNeighbour'
            //       add ptr<24> id:6
            //         lda ptr<96> 'pix_a'
            //         mul u32 id:13
            //           ld i32 'i'
            //           intconst 24 u32 id:14
            SIMP_to_lowest_height(&tcont) = true;
        }

        p = simplifyExpression(p, &tcont);

        if (SIMP_to_lowest_height(&tcont) && !p->is_leaf()) {
            p = simpToPR(p, &tcont);
        }

        SIMP_to_lowest_height(&tcont) = origin_to_lowest_height;

        xcom::add_next(&newp, &newplast, p);

        changed |= SIMP_changed(&tcont);

        if (SIMP_changed(&tcont)) {
            IR_parent(p) = ir;
        }

        SIMP_changed(&tcont) = false;
    }

    CALL_param_list(ir) = newp;
    if (newp != nullptr) {
        ir->setParent(newp);
    }

    xcom::add_next(ret_list, last, SIMP_stmtlist(&tcont));
    ctx->unionBottomUpInfo(tcont);
    return changed;
}


IR * IRSimp::simplifyCall(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->isCallStmt());
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    IR * ret_list = nullptr;
    IR * last = nullptr;
    bool lchange = simplifyCallParamList(ir, &ret_list, &last, ctx);
    if (ir->is_icall()) {
        //Simplify callee expression of ICALL
        SimpCtx tctx2(*ctx);
        simplifyCalleeExp(ir, &tctx2);
        lchange |= SIMP_changed(&tctx2);
        xcom::add_next(&ret_list, SIMP_stmtlist(&tctx2));
        ctx->unionBottomUpInfo(tctx2);
    }

    xcom::add_next(&ret_list, ir);
    SIMP_changed(ctx) |= lchange;
    return ret_list;
}


void IRSimp::simplifyCalleeExp(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_icall());
    ICALL_callee(ir) = simplifyExpression(ICALL_callee(ir), ctx);
}


IR * IRSimp::simplifySetelem(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_setelem());

    ASSERT0(SIMP_stmtlist(ctx) == nullptr);

    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    SIMP_ret_array_val(&tcont) = true;
    SETELEM_base(ir) = simplifyExpression(SETELEM_base(ir), &tcont);
    IR_parent(SETELEM_base(ir)) = ir;
    ctx->unionBottomUpInfo(tcont);

    SimpCtx tcont2(*ctx);
    SIMP_ret_array_val(&tcont2) = true;
    SETELEM_val(ir) = simplifyExpression(SETELEM_val(ir), &tcont2);
    IR_parent(SETELEM_val(ir)) = ir;
    ctx->unionBottomUpInfo(tcont2);

    IR * ret_list = nullptr;
    IR * last = nullptr;
    if (SIMP_stmtlist(&tcont2) != nullptr) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont2));
    }

    //Process offset.
    SimpCtx tcont3(*ctx);
    SIMP_ret_array_val(&tcont3) = true;
    SETELEM_ofst(ir) = simplifyExpression(SETELEM_ofst(ir), &tcont3);
    IR_parent(SETELEM_ofst(ir)) = ir;
    ctx->unionBottomUpInfo(tcont3);
    last = nullptr;
    if (SIMP_stmtlist(&tcont3) != nullptr) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont3));
    }
    xcom::add_next(&ret_list, &last, ir);
    return ret_list;
}


IR * IRSimp::simplifyGetelem(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_getelem());

    ASSERT0(SIMP_stmtlist(ctx) == nullptr);

    //Process base.
    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    SIMP_ret_array_val(&tcont) = true;
    GETELEM_base(ir) = simplifyExpression(GETELEM_base(ir), &tcont);
    IR_parent(GETELEM_base(ir)) = ir;
    ctx->unionBottomUpInfo(tcont);

    IR * ret_list = nullptr;
    IR * last = nullptr;
    if (SIMP_stmtlist(&tcont) != nullptr) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }

    //Process offset.
    SimpCtx tcont2(*ctx);
    SIMP_ret_array_val(&tcont2) = true;
    GETELEM_ofst(ir) = simplifyExpression(GETELEM_ofst(ir), &tcont2);
    IR_parent(GETELEM_ofst(ir)) = ir;
    ctx->unionBottomUpInfo(tcont2);
    last = nullptr;
    if (SIMP_stmtlist(&tcont2) != nullptr) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont2));
    }
    xcom::add_next(&ret_list, &last, ir);
    return ret_list;
}


IR * IRSimp::simplifyIndirectMemOp(IR * ir, SimpCtx * ctx)
{
    IR * ret_list = nullptr;
    IR * last = nullptr;
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    SimpCtx basectx(*ctx);
    SIMP_ret_array_val(&basectx) = true;
    if (SIMP_ild_ist(ctx)) {
        //Reduce the tree height.
        ir->setBase(simplifyExpression(ir->getBase(), &basectx));
        ctx->unionBottomUpInfo(basectx);
    }

    if (SIMP_stmtlist(&basectx) != nullptr) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&basectx));
    }

    if (SIMP_to_lowest_height(ctx) && ir->getBase()->is_ild() &&
        SIMP_ild_ist(ctx)) {
        //Handle a special case, the following tree is obscure,
        //transform it to be more understandable.
        //  IST(ILD(PR1), ...)
        //=>
        //  PR2 = ILD(PR1)
        //  IST(PR2, ...)
        SimpCtx istbasectx(ctx->getOptCtx());
        ir->setBase(simpToPR(ir->getBase(), &istbasectx));
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&istbasectx));
        SIMP_changed(ctx) = true;
    }

    //Simplify RHS.
    SimpCtx rhsctx(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    SIMP_ret_array_val(&rhsctx) = true;
    ir->setRHS(simplifyExpression(ir->getRHS(), &rhsctx));
    ctx->unionBottomUpInfo(rhsctx);

    if (SIMP_stmtlist(&rhsctx) != nullptr) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&rhsctx));
    }
    xcom::add_next(&ret_list, &last, ir);
    return ret_list;
}


IR * IRSimp::simplifyDirectMemOp(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->hasRHS());
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    SIMP_ret_array_val(&tcont) = true;
    ir->setRHS(simplifyExpression(ir->getRHS(), &tcont));
    ctx->unionBottomUpInfo(tcont);

    IR * ret_list = nullptr;
    IR * last = nullptr;
    if (SIMP_stmtlist(&tcont) != nullptr) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }
    xcom::add_next(&ret_list, &last, ir);
    return ret_list;
}


IR * IRSimp::simplifyGoto(IR * ir, SimpCtx * ctx)
{
    return simplifyNormal(ir, ctx);
}


IR * IRSimp::simplifyCase(IR * ir, SimpCtx * ctx)
{
    return simplifyNormal(ir, ctx);
}


IR * IRSimp::simplifyNormal(IR * ir, SimpCtx * ctx)
{
    IR * ret_list = nullptr;
    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    SIMP_ret_array_val(&tcont) = true;
    for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            ir->setKid(i, simplifyExpression(kid, &tcont));
            ctx->unionBottomUpInfo(tcont);
            if (SIMP_stmtlist(&tcont) != nullptr) {
                xcom::add_next(&ret_list, SIMP_stmtlist(&tcont));
                SIMP_stmtlist(&tcont) = nullptr;
            }
        }
    }
    xcom::add_next(&ret_list, ir);
    return ret_list;
}


IR * IRSimp::simplifyReturn(IR * ir, SimpCtx * ctx)
{
    return simplifyNormal(ir, ctx);
}


IR * IRSimp::simplifyBranch(IR * ir, SimpCtx * ctx)
{
    IR * ret_list = nullptr;
    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    SIMP_ret_array_val(&tcont) = true;

    if ((SIMP_lor_land(ctx) &&
         (BR_det(ir)->is_lor() || BR_det(ir)->is_land())) ||
        (SIMP_lnot(ctx) && BR_det(ir)->is_lnot())) {
        ret_list = simplifyLogicalDet(ir, &tcont);
        ret_list = simplifyStmtList(ret_list, &tcont);
    } else {
        for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * kid = ir->getKid(i);
            if (kid != nullptr) {
                ir->setKid(i, simplifyExpression(kid, &tcont));
                if (SIMP_stmtlist(&tcont) != nullptr) {
                    xcom::add_next(&ret_list, SIMP_stmtlist(&tcont));
                    SIMP_stmtlist(&tcont) = nullptr;
                }
            }
        }
        xcom::add_next(&ret_list, ir);
    }
    ctx->unionBottomUpInfo(tcont);
    return ret_list;
}


IR * IRSimp::simplifyIf(IR * ir, SimpCtx * ctx)
{
    if (SIMP_if(ctx)) {
        return simplifyIfSelf(ir, ctx);
    }

    //Det
    SimpCtx detctx(*ctx);
    IF_det(ir) = simplifyExpression(IF_det(ir), &detctx);
    ir->setParent(IF_det(ir));
    ASSERTN(SIMP_stmtlist(&detctx) == nullptr, ("invalid determinator"));

    //Truebody
    SimpCtx truectx(*ctx);
    IF_truebody(ir) = simplifyStmtList(IF_truebody(ir), &truectx);
    if (SIMP_changed(&truectx)) {
        ir->setParent(IF_truebody(ir));
    }
    ASSERTN(SIMP_stmtlist(&truectx) == nullptr,
           ("should already be added to truebody"));

    //Falsebody
    SimpCtx falsectx(*ctx);
    IF_falsebody(ir) = simplifyStmtList(IF_falsebody(ir), &falsectx);
    if (SIMP_changed(&falsectx)) {
        ir->setParent(IF_falsebody(ir));
    }
    ASSERTN(SIMP_stmtlist(&falsectx) == nullptr,
           ("should already be added to falsebody"));

    return ir;
}


IR * IRSimp::simplifyLoopIngredient(IR * ir, SimpCtx * ctx)
{
    //Det
    SimpCtx detctx(*ctx);
    LOOP_det(ir) = simplifyExpression(LOOP_det(ir), &detctx);
    ir->setParent(LOOP_det(ir));
    ASSERTN(SIMP_stmtlist(&detctx) == nullptr, ("invalid determinator"));

    SimpCtx initctx(*ctx);
    if (ir->is_doloop()) {
        //init
        LOOP_init(ir) = simplifyStmtList(LOOP_init(ir), &initctx);
        if (SIMP_changed(&initctx)) {
            ir->setParent(LOOP_init(ir));
        }

        //step
        SimpCtx stepctx(*ctx);
        LOOP_step(ir) = simplifyStmtList(LOOP_step(ir), &stepctx);
        if (SIMP_changed(&stepctx)) {
            ir->setParent(LOOP_step(ir));
        }
        ASSERTN(SIMP_stmtlist(&stepctx) == nullptr,
               ("step of DoLoop is too complex, the operation "
                "should only be reduction"));
    }

    //loopbody
    SimpCtx bodyctx(*ctx);
    LOOP_body(ir) = simplifyStmtList(LOOP_body(ir), &bodyctx);
    if (SIMP_changed(&bodyctx)) {
        ir->setParent(LOOP_body(ir));
    }
    ASSERTN(SIMP_stmtlist(&bodyctx) == nullptr,
           ("should already be added to truebody"));

    IR * ret_list = nullptr;
    IR * last = nullptr;
    if (ir->is_doloop() && SIMP_stmtlist(&initctx) != nullptr) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&initctx));
    }
    xcom::add_next(&ret_list, &last, ir);

    return ret_list;
}


IR * IRSimp::simplifyDoWhile(IR * ir, SimpCtx * ctx)
{
    if (SIMP_dowhile(ctx)) {
        return simplifyDoWhileSelf(ir, ctx);
    }

    ASSERTN(!SIMP_break(ctx) && !SIMP_continue(ctx),
           ("Must simplify Loop-stmt if you want to simply Break/Continue."));

    return simplifyLoopIngredient(ir, ctx);
}


IR * IRSimp::simplifyWhileDo(IR * ir, SimpCtx * ctx)
{
    if (SIMP_whiledo(ctx)) {
        return simplifyWhileDoSelf(ir, ctx);
    }

    ASSERTN(!SIMP_break(ctx) && !SIMP_continue(ctx),
           ("Must simplify Loop-stmt if you want to simply Break/Continue."));

    return simplifyLoopIngredient(ir, ctx);
}


IR * IRSimp::simplifyDoLoop(IR * ir, SimpCtx * ctx)
{
    if (SIMP_doloop(ctx)) {
        return simplifyDoLoopSelf(ir, ctx);
    }

    ASSERTN(!SIMP_break(ctx) && !SIMP_continue(ctx),
           ("Must simplify Loop-stmt if you want to simply Break/Continue."));

    return simplifyLoopIngredient(ir, ctx);
}


IR * IRSimp::simplifySwitch(IR * ir, SimpCtx * ctx)
{
    if (SIMP_switch(ctx)) {
        return simplifySwitchSelf(ir, ctx);
    }

    IR * body = SWITCH_body(ir);
    if (body == nullptr) { return ir; }

    ASSERTN(!SIMP_break(ctx),
            ("Must simplify Swich-stmt if you want to simply Break."));

    SWITCH_body(ir) = nullptr;
    body = simplifyStmtList(body, ctx);
    if (ir->getParent() == nullptr) {
        xoc::cleanParentForIRList(body);
    }
    xcom::add_next(&ir, body);

    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return ir;
}


IR * IRSimp::simplifyExtExp(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_EXP:
        return ir;
    default: UNREACHABLE();
    }
    return nullptr;
}


IR * IRSimp::simplifyExtStmt(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_STMT:
        return ir;
    default: UNREACHABLE();
    }
    return nullptr;
}


//Simply IR and its kids.
//Remember copy Dbx info for new STMT to conveninent to dump use.
//Return new ir stmt-list.
IR * IRSimp::simplifyStmt(IR * ir, SimpCtx * ctx)
{
    if (ir == nullptr) { return nullptr; }
    ASSERTN(ir->is_stmt(), ("expect statement node"));
    ASSERTN(ir->is_single(), ("ir should be remove out of list"));
    if (ctx->isSimpCFSOnly() && ir->isStmtInBB()) { return ir; }

    bool has_side_effect = ir->hasSideEffect(false);
    bool is_no_move = ir->isNoMove(false);
    IR * ret_list = nullptr;
    switch (ir->getCode()) {
    SWITCH_CASE_CALL:
        ret_list = simplifyCall(ir, ctx);
        break;
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR:
        ret_list = simplifyDirectMemOp(ir, ctx);
        break;
    SWITCH_CASE_WRITE_ARRAY:
        ret_list = simplifyStoreArray(ir, ctx);
        break;
    case IR_SETELEM:
        ret_list = simplifySetelem(ir, ctx);
        break;
    case IR_GETELEM:
        ret_list = simplifyGetelem(ir, ctx);
        break;
    SWITCH_CASE_INDIRECT_MEM_STMT:
        ret_list = simplifyIndirectMemOp(ir, ctx);
        break;
    case IR_IGOTO:
        ASSERT0(SIMP_stmtlist(ctx) == nullptr);
        ret_list = simplifyIgoto(ir, ctx);
        break;
    case IR_GOTO:
        ret_list = simplifyGoto(ir, ctx);
        break;
    case IR_LABEL:
        ret_list = simplifyNormal(ir, ctx);
        break;
    case IR_CASE:
        ret_list = simplifyCase(ir, ctx);
        break;
    case IR_RETURN:
        ret_list = simplifyReturn(ir, ctx);
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        ret_list = simplifyBranch(ir, ctx);
        break;
    case IR_BREAK:
        ASSERT0(SIMP_stmtlist(ctx) == nullptr);
        if (SIMP_break(ctx)) {
            ASSERT0(SIMP_break_label(ctx));
            IR * go = m_irmgr->buildGoto(SIMP_break_label(ctx));
            copyDbx(go, ir, m_rg);
            m_rg->freeIRTree(ir);
            ir = go;
        }
        xcom::add_next(&ret_list, ir);
        break;
    case IR_CONTINUE:
        ASSERT0(SIMP_stmtlist(ctx) == nullptr);
        if (SIMP_continue(ctx)) {
            ASSERT0(SIMP_continue_label(ctx));
            IR * go = m_irmgr->buildGoto(SIMP_continue_label(ctx));
            copyDbx(go, ir, m_rg);
            m_rg->freeIRTree(ir);
            ir = go;
        }
        xcom::add_next(&ret_list, ir);
        break;
    case IR_IF:
        ASSERT0(SIMP_stmtlist(ctx) == nullptr);
        ret_list = simplifyIf(ir, ctx);
        break;
    case IR_DO_WHILE:
        ASSERT0(SIMP_stmtlist(ctx) == nullptr);
        ret_list = simplifyDoWhile(ir, ctx);
        break;
    case IR_WHILE_DO:
        ASSERT0(SIMP_stmtlist(ctx) == nullptr);
        ret_list = simplifyWhileDo(ir, ctx);
        break;
    case IR_DO_LOOP:
        ASSERT0(SIMP_stmtlist(ctx) == nullptr);
        ret_list = simplifyDoLoop(ir, ctx);
        break;
    case IR_SWITCH:
        ASSERT0(SIMP_stmtlist(ctx) == nullptr);
        ret_list = simplifySwitch(ir, ctx);
        break;
    case IR_REGION:
    case IR_PHI:
        return ir;
    default:
        ret_list = simplifyExtStmt(ir, ctx);
    }
    setParentPointerForIRList(ret_list);
    if (has_side_effect || is_no_move) {
        for (IR * t = ret_list; t != nullptr; t = t->get_next()) {
            IR_has_sideeffect(t) = has_side_effect ?
                                   true : t->hasSideEffect(false);
            IR_no_move(t) = is_no_move ? true : t->isNoMove(false);
        }
    }
    return ret_list;
}


//Return new generated IR stmt.
IR * IRSimp::simplifyStmtList(IR * ir_list, SimpCtx * ctx)
{
    if (ir_list == nullptr) { return nullptr; }
    IR * ret_list = nullptr;
    ASSERT0(SIMP_stmtlist(ctx) == nullptr);
    bool redo_simp = true;
    UINT count = 0;
    while (redo_simp) {
        count++;
        ASSERTN(count < 50, ("dead lock"));
        redo_simp = false;

        IR * last = nullptr;
        for (IR * stmt = xcom::removehead(&ir_list);
             stmt != nullptr; stmt = xcom::removehead(&ir_list)) {
            IR * new_stmt_list = simplifyStmt(stmt, ctx);
            ASSERT0(SIMP_stmtlist(ctx) == nullptr);
            if (SIMP_to_lowest_height(ctx)) {
                //Check if new generated stmts still need to resimplify.
                for (IR * x = new_stmt_list; x != nullptr; x = x->get_next()) {
                    if (!isLowestHeight(x, ctx)) {
                        redo_simp = true;
                        break;
                    }
                }
            }

            xcom::add_next(&ret_list, &last, new_stmt_list);
        }

        if (redo_simp) {
            ir_list = ret_list;
            ret_list = nullptr;
        }
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpSimp()) {
        note(m_rg, "\n==---- DUMP AFTER SIMPLIFY STMT LIST ----==");
        m_rg->getLogMgr()->incIndent(2);
        xoc::dumpIRList(ret_list, m_rg);
        m_rg->getLogMgr()->decIndent(2);    
    }
    return ret_list;
}


//NOTE: simplification should not generate indirect memory operation.
void IRSimp::simplifyBB(IRBB * bb, SimpCtx * ctx)
{
    List<IR*> new_ir_list;
    IRListIter ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end();
         ct = BB_irlist(bb).get_next(ct)) {
        IR * stmt = ct->val();
        ASSERT0(stmt && stmt->is_single());
        IR * newstmt_lst = simplifyStmt(stmt, ctx);
        while (newstmt_lst != nullptr) {
            IR * newir = xcom::removehead(&newstmt_lst);
            if (newir->isStmtInBB()) {
                //Some non-stmt-in-bb stmt generated and BB list have to be
                //reconstruct, e.g:IR_LABEL.
                newir->setBB(bb);
            } else {
                ASSERT0(newir->is_lab());
            }
            ASSERT0(newir->verify(m_rg));
            new_ir_list.append_tail(newir);
        }
    }
    BB_irlist(bb).copy(new_ir_list);
}


void IRSimp::simplifyIRList(SimpCtx * ctx)
{
    m_rg->setIRList(simplifyStmtList(m_rg->getIRList(), ctx));
    ASSERT0(verifySimp(m_rg->getIRList(), *ctx));
    ASSERT0(verifyIRList(m_rg->getIRList(), nullptr, m_rg));
}


//NOTE: simplification should not generate indirect memory operation.
void IRSimp::simplifyBBlist(BBList * bbl, SimpCtx * ctx)
{
    START_TIMER(t, "Simplify IRBB list");
    BBListIter ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        simplifyBB(bb, ctx);
    }
    END_TIMER(t, "Simplify IRBB list");

    //NOTE: Do NOT dumpBBList here because new IR_LABEL may be generated.
}

} //namespace xoc
