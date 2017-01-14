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
#include "cfs_mgr.h"

namespace xoc {

#ifdef _DEBUG_
static bool isBinAndUniExp(IR const* ir)
{
    switch (ir->get_code()) {
    case IR_ILD:
    case IR_BAND: //inclusive and &
    case IR_BOR: //inclusive or  |
    case IR_XOR: //exclusive or ^
    case IR_BNOT: //bitwise not
    case IR_NEG: //negative
    case IR_EQ: //==
    case IR_NE: //!=
    case IR_LT:
    case IR_GT:
    case IR_GE:
    case IR_LE:
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LABEL:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_CVT: //data type convertion
        return true;
    default: break;
    }
    return false;
}
#endif


static bool isLowest(IR const* ir)
{
    ASSERT0(ir->is_exp());
    if (ir->is_leaf()) { return true; }
    IR * parent = ir->get_parent();
    if (parent == NULL) { return true; }
    if (!parent->is_stmt()) {
        //tree height is more than 2.
        return false;
    }

    if (parent->is_calls_stmt()) {
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
bool Region::isLowestHeightSelect(IR const* ir) const
{
    ASSERT0(ir->is_select());
    if (!isLowest(ir)) { return false; }

    ASSERT0(SELECT_pred(ir));
    if (!SELECT_pred(ir)->is_leaf()) { return false; }

    ASSERT0(SELECT_trueexp(ir));
    if (!SELECT_trueexp(ir)->is_leaf()) { return false; }

    ASSERT0(SELECT_falseexp(ir));
    if (!SELECT_falseexp(ir)->is_leaf()) { return false; }

    return true;
}


//At lowest mode, the array base, array subscript-expression must be leaf.
bool Region::isLowestHeightArrayOp(IR const* ir) const
{
    ASSERT0(ir->is_array_op());
    if (ir->is_array() && !isLowest(ir)) { return false; }

    if (!ARR_base(ir)->is_leaf()) { return false; }

    for (IR const* s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
        if (!s->is_leaf()) { return false; }
    }
    return true;
}


bool Region::isLowestHeightExp(IR const* ir, SimpCtx const* ctx) const
{
    if (ir->is_leaf()) { return true; }

    ASSERT0(ctx);
    switch (ir->get_code()) {
    case IR_LAND:
    case IR_LOR:
    case IR_LNOT:
        return false;
    case IR_ARRAY:
        return isLowestHeightArrayOp(ir);
    case IR_ILD:
        if (!SIMP_ild_ist(ctx)) { return true; }
        return isLowest(ir);
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_BNOT:
    case IR_NEG:
    case IR_EQ:
    case IR_NE:
    case IR_LT:
    case IR_GT:
    case IR_GE:
    case IR_LE:
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_CVT:
        return isLowest(ir);
    case IR_SELECT:
        return isLowestHeightSelect(ir);
    default: UNREACH();
    }

    return true;
}


bool Region::isLowestHeight(IR const* ir, SimpCtx const* ctx) const
{
    ASSERT0(ir && ir->is_stmt() && ctx);
    switch (ir->get_code()) {
    case IR_CALL:
    case IR_ICALL:
        for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
            if (!p->is_leaf()) {
                return false;
            }
        }
        return true;
    case IR_ST:
        return isLowestHeightExp(ST_rhs(ir), ctx);
    case IR_STPR:
        return isLowestHeightExp(STPR_rhs(ir), ctx);
    case IR_STARRAY:
        if (SIMP_array(ctx)) { return false; }
        if (!isLowestHeightArrayOp(ir)) { return false; }
        return isLowestHeightExp(STARR_rhs(ir), ctx);
    case IR_IST: //indirective store
        if (SIMP_ild_ist(ctx) && !isLowestHeightExp(IST_base(ir), ctx)) {
            return false;
        }
        return isLowestHeightExp(IST_rhs(ir), ctx);
    case IR_GOTO:
    case IR_LABEL:
    case IR_CASE:
        return true;
    case IR_RETURN:
        if (RET_exp(ir) != NULL) {
            return isLowestHeightExp(RET_exp(ir), ctx);
        }
        return true;
    case IR_TRUEBR:
    case IR_FALSEBR:
        ASSERT0(BR_det(ir));
        return isLowestHeightExp(BR_det(ir), ctx);
    case IR_BREAK:
    case IR_CONTINUE:
    case IR_IF:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_SWITCH:
    case IR_IGOTO:
        return false;
    default: UNREACH();
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
IR * Region::simplifyIfSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) return NULL;
    ASSERT(ir->is_if(), ("expect IR_IF node"));
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;

    //Det exp.
    //When we first lowering CFS, det-expression should not be TRUEBR/FASLEBR.
    ASSERT0(IF_det(ir)->is_judge());
    IR * det = simplifyDet(IF_det(ir), &tcont);
    IR * last = removetail(&det);
    ASSERT(last->is_exp(), ("invalide det exp"));
    if (!last->is_judge()) {
        //det-expression should be judgement.
        last = buildJudge(last);
    }
    IR * falsebr = buildBranch(false, last, genIlabel());
    copyDbx(falsebr, IF_det(ir), this);
    xcom::add_next(&det, falsebr);

    IR * truebody = simplifyStmtList(IF_truebody(ir), ctx);
    IR * elsebody = NULL;
    if (IF_falsebody(ir) != NULL) { //Simplify ELSE body
        //append GOTO following end of true body
        IR * go = buildGoto(genIlabel());
        copyDbx(go, IF_det(ir), this);
        xcom::add_next(&truebody, go);

        //truebody end label
        xcom::add_next(&truebody, buildLabel(BR_lab(falsebr)));

        //simplify false body
        elsebody = simplifyStmtList(IF_falsebody(ir), ctx);

        //falsebody end label
        xcom::add_next(&elsebody, buildLabel(GOTO_lab(go)));
    } else {
        //end label    of truebody.
        xcom::add_next(&truebody, buildLabel(BR_lab(falsebr)));
    }

    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = NULL;

        ASSERT0(SIMP_cfs_mgr(ctx));
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_IF);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(falsebr, ci);
        CFS_INFO_ir(ci) = falsebr;

        SIMP_cfs_mgr(ctx)->recordStmt(truebody, *CFS_INFO_true_body(ci));
        SIMP_cfs_mgr(ctx)->recordStmt(elsebody, *CFS_INFO_false_body(ci));
    }

    IR * ret_list = NULL;
    xcom::add_next(&ret_list, det);
    xcom::add_next(&ret_list, truebody);
    xcom::add_next(&ret_list, elsebody);
    for (IR * p = ret_list; p != NULL; p = p->get_next()) {
        IR_parent(p) = NULL;
    }

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
IR * Region::simplifyWhileDoSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) return NULL;
    SimpCtx local;
    local.copyTopdownFlag(*ctx);
    IR * ret_list = NULL;
    ASSERT(ir->is_whiledo(), ("expect IR_WHILE_DO node"));
    LabelInfo * startl = genIlabel();

    //det exp
    //When we first lowering CFS,
    //det-expression should not be TRUEBR/FASLEBR.
    ASSERT0(LOOP_det(ir)->is_judge());
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    IR * det = simplifyDet(LOOP_det(ir), &tcont);
    IR * last = xcom::removetail(&det);
    ASSERT(last->is_exp(), ("invalide det exp"));
    if (!last->is_judge()) {
        //det-expression should be judgement.
        last = buildJudge(last);
    }

    IR * falsebr = buildBranch(false, last, genIlabel());
    copyDbx(falsebr, ir, this);
    xcom::add_next(&det, falsebr);

    SIMP_break_label(&local) = BR_lab(falsebr);
    SIMP_continue_label(&local) = startl;

    //loop body
    IR * body = simplifyStmtList(LOOP_body(ir), &local);
    xcom::add_next(&body, buildGoto(startl));

    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = NULL;
        ASSERT0(SIMP_cfs_mgr(ctx));
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_WHILE_DO);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(falsebr, ci);
        CFS_INFO_ir(ci) = falsebr;
        SIMP_cfs_mgr(ctx)->recordStmt(body, *CFS_INFO_loop_body(ci));

        //Falsebr is executed each iter.
        CFS_INFO_loop_body(ci)->bunion(IR_id(falsebr));
    }

    xcom::add_next(&ret_list, buildLabel(startl));
    xcom::add_next(&ret_list, det);
    xcom::add_next(&ret_list, body);
    xcom::add_next(&ret_list, buildLabel(BR_lab(falsebr)));
    for (IR * p = ret_list; p != NULL; p = p->get_next()) {
        IR_parent(p) = NULL;
    }
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
IR * Region::simplifyDoWhileSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) return NULL;
    SimpCtx local;
    local.copyTopdownFlag(*ctx);
    IR * ret_list = NULL;
    ASSERT(ir->is_dowhile(), ("expect IR_DO_WHILE node"));

    LabelInfo * startl = genIlabel();
    LabelInfo * endl = genIlabel();
    LabelInfo * det_startl = genIlabel();

    //det exp
    //When we first lowering CFS, det-expression should not be TRUEBR/FASLEBR.
    ASSERT0(LOOP_det(ir)->is_judge());
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    IR * det = simplifyDet(LOOP_det(ir), &tcont);
    IR * last = removetail(&det);
    ASSERT(last->is_exp(), ("invalide det exp"));
    if (!last->is_judge()) {
        //det-expression should be judgement.
        last = buildJudge(last);
    }

    IR * truebr = buildBranch(true, last, startl);
    copyDbx(truebr, ir, this);
    xcom::add_next(&det, truebr);

    SIMP_break_label(&local) = endl;
    SIMP_continue_label(&local) = det_startl;

    IR * body = simplifyStmtList(LOOP_body(ir), &local);
    insertbefore_one(&body, body, buildLabel(startl));

    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = NULL;
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
    xcom::add_next(&ret_list, &last, buildLabel(det_startl));
    xcom::add_next(&ret_list, &last, det);
    xcom::add_next(&ret_list, &last, buildLabel(endl));
    for (IR * p = ret_list; p != NULL; p = p->get_next()) {
        IR_parent(p) = NULL;
    }
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
IR * Region::simplifyDoLoopSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) return NULL;
    SimpCtx local;
    local.copyTopdownFlag(*ctx);
    IR * ret_list = NULL;
    ASSERT(ir->is_doloop(), ("expect IR_DO_LOOP node"));

    LabelInfo * startl = genIlabel();

    //det exp
    //When we first lowering CFS, det-expression should not be TRUEBR/FASLEBR.
    ASSERT0(LOOP_det(ir)->is_judge());
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    IR * det = simplifyDet(LOOP_det(ir), &tcont);
    IR * last = removetail(&det);
    ASSERT(last->is_exp(), ("invalide det exp"));
    if (!last->is_judge()) {
        //det-expression should be judgement.
        last = buildJudge(last);
    }

    IR * falsebr = buildBranch(false, last, genIlabel());
    copyDbx(falsebr, LOOP_det(ir), this);
    xcom::add_next(&det, falsebr);

    LabelInfo * stepl = genIlabel();

    SIMP_break_label(&local) = BR_lab(falsebr);
    SIMP_continue_label(&local) = stepl;

    IR * init = simplifyStmtList(LOOP_init(ir), &local);
    IR * step = simplifyStmtList(LOOP_step(ir), &local);
    IR * body = simplifyStmtList(LOOP_body(ir), &local);

    //step label , for simp 'continue' used
    xcom::add_next(&body, buildLabel(stepl));
    xcom::add_next(&body, step);
    xcom::add_next(&body, buildGoto(startl));

    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = NULL;
        ASSERT0(SIMP_cfs_mgr(ctx) != NULL);
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_DO_LOOP);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(falsebr, ci);
        CFS_INFO_ir(ci) = falsebr;
        SIMP_cfs_mgr(ctx)->recordStmt(body, *CFS_INFO_loop_body(ci));

        //'falsebr' is executed during each iteration.
        CFS_INFO_loop_body(ci)->bunion(IR_id(falsebr));
    }

    xcom::add_next(&ret_list, init);
    xcom::add_next(&ret_list, buildLabel(startl));
    xcom::add_next(&ret_list, det);
    xcom::add_next(&ret_list, body);
    xcom::add_next(&ret_list, buildLabel(BR_lab(falsebr)));
    for (IR * p = ret_list; p != NULL; p = p->get_next()) {
        IR_parent(p) = NULL;
    }
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return ret_list;
}


//Simplify determination of Control Flow Structure.
IR * Region::simplifyDet(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) return NULL;
    IR * ret_exp_list = NULL;
    IR * next;
    while (ir != NULL) {
        next = ir->get_next();
        IR_next(ir) = IR_prev(ir) = NULL;
        if (ir->is_stmt()) {
            SimpCtx tcont(*ctx);
            IR * new_stmt_list = simplifyStmt(ir, &tcont);
            ctx->unionBottomupFlag(tcont);

            #ifdef _DEBUG_
            IR * x = new_stmt_list;
            while (x != NULL) {
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
            ctx->unionBottomupFlag(tcont);
        } else {
            ASSERT(0, ("unknonw ir type"));
        }
        ir = next;
    }
    for (IR * p = ret_exp_list; p != NULL; p = p->get_next()) {
        IR_parent(p) = NULL;
    }
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
IR * Region::simplifyLogicalNot(IN IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_lnot());
    LabelInfo * label1 = genIlabel();
    IR * pr = buildPR(ir->get_type());
    allocRefForPR(pr);
    IR * ret_list = NULL;

    //truebr(exp != 0), L1
    IR * opnd0 = UNA_opnd0(ir);
    UNA_opnd0(ir) = NULL;
    if (!opnd0->is_judge()) {
        opnd0 = buildJudge(opnd0);
    }
    IR * true_br = buildBranch(true, opnd0, label1);
    copyDbx(true_br, ir, this);
    xcom::add_next(&ret_list, true_br);

    TypeMgr * dm = get_type_mgr();
    //pr = 1
    Type const* t = dm->getSimplexTypeEx(
        dm->get_dtype(WORD_LENGTH_OF_HOST_MACHINE, true));
    IR * imm0 = buildImmInt(1, t);
    IR * x = buildStorePR(PR_no(pr), pr->get_type(), imm0);
    allocRefForPR(x);
    copyDbx(x, imm0, this);
    xcom::add_next(&ret_list, x);

    //goto L2
    LabelInfo * label2 = genIlabel();
    xcom::add_next(&ret_list, buildGoto(label2));

    //L1:
    xcom::add_next(&ret_list, buildLabel(label1));

    //pr = 0
    Type const* t2 = dm->getSimplexTypeEx(
                    dm->get_dtype(WORD_LENGTH_OF_HOST_MACHINE, true));
    IR * imm1 = buildImmInt(0, t2);

    IR * x2 = buildStorePR(PR_no(pr), pr->get_type(), imm1);
    allocRefForPR(x2);
    copyDbx(x2, imm1, this);
    xcom::add_next(&ret_list, x2);

    //L2:
    xcom::add_next(&ret_list, buildLabel(label2));
    ctx->appendStmt(ret_list);
    freeIRTree(ir);
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
IR * Region::simplifyLogicalAnd(IN IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_land());
    LabelInfo * label1 = genIlabel();
    IR * pr = buildPR(ir->get_type());
    allocRefForPR(pr);
    IR * ret_list = simplifyLogicalAndAtTruebr(ir, label1);
    TypeMgr * tm = get_type_mgr();
    Type const* t = tm->getSimplexTypeEx(
                tm->get_dtype(WORD_LENGTH_OF_HOST_MACHINE, true));
    IR * imm0 = buildImmInt(0, t);
    IR * x = buildStorePR(PR_no(pr), pr->get_type(), imm0);
    allocRefForPR(x);
    copyDbx(x, imm0, this);
    xcom::add_next(&ret_list, x);

    LabelInfo * label2 = genIlabel();
    xcom::add_next(&ret_list, buildGoto(label2));
    xcom::add_next(&ret_list, buildLabel(label1));
    Type const* t2 = tm->getSimplexTypeEx(
                tm->get_dtype(WORD_LENGTH_OF_HOST_MACHINE, true));
    IR * imm1 = buildImmInt(1, t2);
    IR * x2 = buildStorePR(PR_no(pr), pr->get_type(), imm1);
    allocRefForPR(x2);
    copyDbx(x2, imm1, this);
    xcom::add_next(&ret_list, x2);
    xcom::add_next(&ret_list, buildLabel(label2));
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
IR * Region::simplifyLogicalAndAtTruebr(IR * ir, LabelInfo const* tgt_label)
{
    ASSERT0(ir->is_land() && tgt_label != NULL);
    IR * ret_list = NULL;

    //Process opnd0.
    IR * opnd0 = BIN_opnd0(ir);
    BIN_opnd0(ir) = NULL;
    if (!opnd0->is_judge()) {
        opnd0 = buildJudge(opnd0);
    }
    //Generate falsebranch label.
    LabelInfo * lab = genIlabel();

    IR * br = buildBranch(false, opnd0, lab);
    copyDbx(br, ir, this);
    xcom::add_next(&ret_list, br);

    //Process opnd1.
    IR * opnd1 = BIN_opnd1(ir);
    BIN_opnd1(ir) = NULL;
    if (!opnd1->is_judge()) {
        opnd1 = buildJudge(opnd1);
    }
    br = buildBranch(true, opnd1, tgt_label);
    copyDbx(br, ir, this);
    xcom::add_next(&ret_list, br);

    //Add false-branch label.
    xcom::add_next(&ret_list, buildLabel(lab));
    freeIRTree(ir);
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
IR * Region::simplifyLogicalAndAtFalsebr(IR * ir, LabelInfo const* tgt_label)
{
    ASSERT0(ir->is_land() && tgt_label != NULL);
    IR * ret_list = NULL;

    //Process opnd0.
    IR * opnd0 = BIN_opnd0(ir);
    BIN_opnd0(ir) = NULL;
    if (!opnd0->is_judge()) {
        opnd0 = buildJudge(opnd0);
    }
    IR * false_br = buildBranch(false, opnd0, tgt_label);
    copyDbx(false_br, ir, this);
    xcom::add_next(&ret_list, false_br);

    //Process opnd1
    IR * opnd1 = BIN_opnd1(ir);
    BIN_opnd1(ir) = NULL;
    if (!opnd1->is_judge()) {
        opnd1 = buildJudge(opnd1);
    }
    false_br = buildBranch(false, opnd1, tgt_label);
    copyDbx(false_br, ir, this);
    xcom::add_next(&ret_list, false_br);
    freeIRTree(ir);
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
IR * Region::simplifyLogicalOrAtTruebr(IR * ir, LabelInfo const* tgt_label)
{
    ASSERT0(ir->is_lor() && tgt_label != NULL);
    IR * ret_list = NULL;

    //Process opnd0.
    IR * opnd0 = BIN_opnd0(ir);
    BIN_opnd0(ir) = NULL;
    if (!opnd0->is_judge()) {
        opnd0 = buildJudge(opnd0);
    }
    IR * true_br = buildBranch(true, opnd0, tgt_label);
    copyDbx(true_br, ir, this);
    xcom::add_next(&ret_list, true_br);

    //Process opnd1.
    IR * opnd1 = BIN_opnd1(ir);
    BIN_opnd1(ir) = NULL;
    if (!opnd1->is_judge()) {
        opnd1 = buildJudge(opnd1);
    }
    IR * op = NULL;
    //if (SIMP_lnot(ctx)) {
        op = buildBranch(true, opnd1, tgt_label);
    //} else {
    //    In the case ir's parent is if(a||b),L1, generate STORE is invalid.
    //    ASSERT0(res_pr != NULL);
    //    op = buildStore(res_pr, opnd1);
    //}
    copyDbx(op, ir, this);
    xcom::add_next(&ret_list, op);
    freeIRTree(ir);
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
IR * Region::simplifyLogicalOrAtFalsebr(IR * ir, LabelInfo const* tgt_label)
{
    ASSERT0(ir->is_lor() && tgt_label != NULL);
    IR * ret_list = NULL;

    //ir is FALSEBR
    IR * opnd0 = BIN_opnd0(ir);
    BIN_opnd0(ir) = NULL;
    if (!opnd0->is_judge()) {
        opnd0 = buildJudge(opnd0);
    }
    LabelInfo * true_lab = genIlabel();
    IR * true_br = buildBranch(true, opnd0, true_lab);
    copyDbx(true_br, ir, this);
    xcom::add_next(&ret_list, true_br);

    IR * opnd1 = BIN_opnd1(ir);
    BIN_opnd1(ir) = NULL;
    if (!opnd1->is_judge()) {
        opnd1 = buildJudge(opnd1);
    }
    IR * false_br = buildBranch(false, opnd1, tgt_label);
    copyDbx(false_br, ir, this);
    xcom::add_next(&ret_list, false_br);
    xcom::add_next(&ret_list, buildLabel(true_lab));
    freeIRTree(ir);
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
IR * Region::simplifyLogicalOr(IN IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_lor());
    LabelInfo * label1 = genIlabel();
    IR * pr = buildPR(ir->get_type());
    allocRefForPR(pr);
    IR * ret_list = simplifyLogicalOrAtTruebr(ir, label1);
    TypeMgr * dm = get_type_mgr();
    Type const* type = dm->getSimplexTypeEx(
                         dm->get_dtype(WORD_LENGTH_OF_HOST_MACHINE, true));
    IR * imm0 = buildImmInt(0, type);
    IR * x = buildStorePR(PR_no(pr), pr->get_type(), imm0);
    allocRefForPR(x);
    copyDbx(x, imm0, this);
    xcom::add_next(&ret_list, x);

    LabelInfo * label2 = genIlabel();
    xcom::add_next(&ret_list, buildGoto(label2));
    xcom::add_next(&ret_list, buildLabel(label1));

    type = dm->getSimplexTypeEx(dm->get_dtype(WORD_LENGTH_OF_HOST_MACHINE, true));
    IR * imm1 = buildImmInt(1, type);
    IR * x2 = buildStorePR(PR_no(pr), pr->get_type(), imm1);
    allocRefForPR(x2);
    copyDbx(x2, imm1, this);
    xcom::add_next(&ret_list, x2);
    xcom::add_next(&ret_list, buildLabel(label2));
    ctx->appendStmt(ret_list);
    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return pr;
}


//Simplify logical OR, logical AND operations into comparision operations.
//Return generate IR stmts.
IR * Region::simplifyLogicalDet(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) { return NULL; }
    ASSERT0(ir->is_cond_br());
    ASSERT0(BR_det(ir)->is_logical());
    IR * ret_list = NULL;
    if (BR_det(ir)->is_lor()) {
        if (ir->is_truebr()) {
            ret_list = simplifyLogicalOrAtTruebr(BR_det(ir), BR_lab(ir));
            BR_det(ir) = NULL;
            freeIRTree(ir);
            SIMP_changed(ctx) = true;
            SIMP_need_recon_bblist(ctx) = true;
            return ret_list;
        }

        //ir is FALSEBR
        ret_list = simplifyLogicalOrAtFalsebr(BR_det(ir), BR_lab(ir));
        BR_det(ir) = NULL;
        freeIRTree(ir);
        SIMP_changed(ctx) = true;
        SIMP_need_recon_bblist(ctx) = true;
        return ret_list;
    } else if (BR_det(ir)->is_land()) {
        if (ir->is_truebr()) {
            ret_list = simplifyLogicalAndAtTruebr(BR_det(ir), BR_lab(ir));
            BR_det(ir) = NULL;
            freeIRTree(ir);
            SIMP_changed(ctx) = true;
            SIMP_need_recon_bblist(ctx) = true;
            return ret_list;
        }

        //ir is FALSEBR
        ret_list = simplifyLogicalAndAtFalsebr(BR_det(ir), BR_lab(ir));
        BR_det(ir) = NULL;
        freeIRTree(ir);
        SIMP_changed(ctx) = true;
        SIMP_need_recon_bblist(ctx) = true;
        return ret_list;
    } else if (BR_det(ir)->is_lnot()) {
        if (ir->is_truebr()) {
            IR_code(ir) = IR_FALSEBR;
        } else {
            IR_code(ir) = IR_TRUEBR;
        }
        BR_det(ir) = UNA_opnd0(BR_det(ir));
        if (!BR_det(ir)->is_judge()) {
            IR * old = BR_det(ir);
            BR_det(ir) = buildJudge(old);
            copyDbx(BR_det(ir), old, this);
        }
        IR_parent(BR_det(ir)) = ir;
        return simplifyStmt(ir, ctx);
    }
    ASSERT(0, ("TODO"));
    return ret_list;
}


//Simplify kids of SELECT.
//Does not change ir itself.
void Region::simplifySelectKids(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir && ir->is_select());
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->get_kid(i);
        if (kid == NULL) { continue; }
        IR * new_kid = simplifyExpression(kid, ctx);

        if (SIMP_to_lowest_height(ctx)) {
            if (!new_kid->is_leaf()) {
                //Lower new_kid to PR.
                ir->set_kid(i, simpToPR(new_kid, ctx));
                continue;
            }
        }

        ir->set_kid(i, new_kid);
    }
}


//Generate comparision and branch.
IR * Region::simplifySelect(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) { return NULL; }
    ASSERT(ir->is_select(), ("expect select node"));
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

    //Predicator.
    ASSERT0(SELECT_pred(ir));
    IR * newpred = simplifyExpression(SELECT_pred(ir), &predctx);
    ASSERT0(newpred->is_single());
    ctx->appendStmt(predctx);

    //Build false-branch.
    if (!newpred->is_judge()) {
        newpred = buildJudge(newpred);
    }
    IR * falsebr = buildBranch(false, newpred, genIlabel());
    copyDbx(falsebr, SELECT_pred(ir), this);

    IR * lst = NULL;
    IR * last = NULL;
    xcom::add_next(&lst, &last, falsebr);

    ASSERT0(SELECT_trueexp(ir)->get_type() == SELECT_falseexp(ir)->get_type());
    IR * res = buildPR(SELECT_trueexp(ir)->get_type());
    allocRefForPR(res);

    //Simp true exp.
    SimpCtx truectx(*ctx);
    SIMP_ret_array_val(&truectx) = true;
    IR * true_exp = simplifyExpression(SELECT_trueexp(ir), &truectx);
    ctx->appendStmt(truectx);
    IR * mv = buildStorePR(PR_no(res), res->get_type(), true_exp);
    allocRefForPR(mv);
    copyDbx(mv, true_exp, this);
    xcom::add_next(&lst, &last, mv);

    //---
    //Simp false expression
    ASSERT0(SELECT_falseexp(ir) != NULL);
    //append GOTO following end of true body
    IR * gotoend = buildGoto(genIlabel());
    copyDbx(gotoend, SELECT_pred(ir), this);
    xcom::add_next(&lst, &last, gotoend);

    //true body end label
    xcom::add_next(&lst, &last, buildLabel(BR_lab(falsebr)));

    //simplify false expression
    SimpCtx falsectx(*ctx);
    SIMP_ret_array_val(&falsectx) = true;
    IR * else_exp = simplifyExpression(SELECT_falseexp(ir), &falsectx);
    ctx->appendStmt(falsectx);
    IR * mv2 = buildStorePR(PR_no(res), res->get_type(), else_exp);
    allocRefForPR(mv2);
    copyDbx(mv2, else_exp, this);
    xcom::add_next(&lst, &last, mv2);
    //---

    //generate END label.
    xcom::add_next(&lst, &last, buildLabel(GOTO_lab(gotoend)));
    //

    for (IR * p = lst; p != NULL; p = p->get_next()) {
        IR_parent(p) = NULL;
    }
    ctx->appendStmt(lst);

    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    if (SIMP_is_record_cfs(ctx)) {
        //Record high level control flow structure.
        CFS_INFO * ci = NULL;
        ASSERT0(SIMP_cfs_mgr(ctx) != NULL);
        ci = SIMP_cfs_mgr(ctx)->new_cfs_info(IR_IF);
        SIMP_cfs_mgr(ctx)->set_map_ir2cfsinfo(falsebr, ci);
        CFS_INFO_ir(ci) = falsebr;
        SIMP_cfs_mgr(ctx)->recordStmt(true_exp, *CFS_INFO_true_body(ci));
        SIMP_cfs_mgr(ctx)->recordStmt(else_exp, *CFS_INFO_false_body(ci));
    }
    return res;
}


IR * Region::simplifyIgoto(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) { return NULL; }
    ASSERT(ir->is_igoto(), ("expect igoto"));

    IGOTO_vexp(ir) = simplifyExpression(IGOTO_vexp(ir), ctx);
    return ir;
}


IR * Region::simplifySwitchSelf(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) { return NULL; }
    ASSERT(ir->is_switch(), ("expect switch node"));

    IR * vexp_stmt = NULL;
    IR * swt_val = SWITCH_vexp(ir);
    if (!swt_val->is_pr()) {
        SimpCtx vexpctx;
        swt_val = simpToPR(swt_val, &vexpctx);
        vexp_stmt = SIMP_stmtlist(&vexpctx);
    }
    SWITCH_vexp(ir) = NULL;

    IR * case_lst = xcom::get_last(SWITCH_case_list(ir));
    IR * prev_ir_tree = NULL;
    if (case_lst == NULL) {
        //Switch body is useless and neglected.
        return NULL;
    }

    LabelInfo * switch_endlab = NULL;
    
    //Simplify CASE list into IF as default to enable
    //more advantage high level optimizations.
    if (SWITCH_deflab(ir) != NULL) {
        prev_ir_tree = buildGoto(SWITCH_deflab(ir));
        copyDbx(prev_ir_tree, ir, this);
        SWITCH_deflab(ir) = NULL;
    } else {
        if (switch_endlab == NULL) {
            switch_endlab = genIlabel();
        }
        IR * goto_switch_end = buildGoto(switch_endlab);
        xcom::add_next(&prev_ir_tree, goto_switch_end);        
        copyDbx(goto_switch_end, ir, this);
    }
    
    for (; case_lst != NULL; case_lst = IR_prev(case_lst)) {
        IR * ifstmt = buildIf(
            buildCmp(IR_EQ, dupIRTree(swt_val), CASE_vexp(case_lst)),
            buildGoto(CASE_lab(case_lst)), 
            prev_ir_tree);
        copyDbx(ifstmt, case_lst, this);
        CASE_vexp(case_lst) = NULL;
        prev_ir_tree = ifstmt;
    }

    xcom::add_next(&prev_ir_tree, SWITCH_body(ir));
    SWITCH_body(ir) = NULL;
    
    if (switch_endlab != NULL) {
        xcom::add_next(&prev_ir_tree, buildLabel(switch_endlab));
    }

    if (SIMP_if(ctx)) {
        //Simpilify IF to TRUEBR/FALSEBR.
        //Generate the ending-label of SWITCH to serve as the target
        //branch label of TRUEBR/FALSEBR.
        if (switch_endlab == NULL) {
            switch_endlab = genIlabel();
            xcom::add_next(&prev_ir_tree, buildLabel(switch_endlab));
        }

        SimpCtx tctx(*ctx);
        SIMP_break_label(&tctx) = switch_endlab;
        prev_ir_tree = simplifyStmtList(prev_ir_tree, &tctx);
    }

    for (IR * p = prev_ir_tree; p != NULL; p = p->get_next()) {
        IR_parent(p) = NULL;
    }

    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;

    IR * ret_list = prev_ir_tree;
    if (vexp_stmt != NULL) {
        xcom::add_next(&vexp_stmt, prev_ir_tree);
        ret_list = vexp_stmt;
    }
    freeIRTree(ir);
    return ret_list;
}


//Simplify array operator IR_ARRAY to a list of address computing expressions.
//ir: the IR_ARRAY/IR_STARRAY that to be simplified.
//Note this function does not free ir becase ir's Reference info and
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
//coming into this function.
IR * Region::simplifyArrayAddrExp(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir && SIMP_array(ctx) && ir->is_array_op());
    ASSERT0(ARR_sub_list(ir));

    TypeMgr * dm = get_type_mgr(); //may generate new pointer type.
    ASSERT0(ir->get_dtype_size(dm) > 0);

    //For n dimension array, enumb record the number
    //of elements at 0~n-1 dimension.
    UINT enumb = 0;

    Type const* indextyid = getTargetMachineArrayIndexType();
    UINT dim = 0;
    IR * ofst_exp = NULL;
    TMWORD const* elemnumbuf = ARR_elem_num_buf(ir);

    for (IR * s = xcom::removehead(&ARR_sub_list(ir));
         s != NULL; dim++, s = xcom::removehead(&ARR_sub_list(ir))) {
        SimpCtx tcont(*ctx);
        SIMP_ret_array_val(&tcont) = true;
        IR * newsub = simplifyExpression(s, &tcont);
        ctx->appendStmt(tcont);
        ctx->unionBottomupFlag(tcont);

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
            if (enumb != 0) {
                IR * t = buildImmInt(((HOST_INT)enumb) *
                    CONST_int_val(newsub), indextyid);
                freeIRTree(newsub);
                newsub = t;
            }
        } else {
            if (enumb != 0) {
                newsub = buildBinaryOp(IR_MUL, indextyid,
                    newsub, buildImmInt(enumb, indextyid));
            }
        }

        if (ofst_exp == NULL) {
            ofst_exp = newsub;
        } else {
            ofst_exp = buildBinaryOpSimp(IR_ADD, indextyid, ofst_exp, newsub);
        }

        ASSERT(elemnumbuf[dim] != 0,
               ("Incomplete array dimension info, we need to "
                "know how many elements in each dimension."));

        if (dim == 0) {
            enumb = elemnumbuf[dim];
        } else {
            enumb *= elemnumbuf[dim];
        }
    }

    ASSERT0(ofst_exp);

    UINT elemsize = dm->get_bytesize(ARR_elemtype(ir));
    if (elemsize != 1) {
        //e.g: short g[i], subexp is i*sizeof(short)
        ofst_exp = buildBinaryOp(IR_MUL, indextyid,
            ofst_exp, buildImmInt(elemsize, indextyid));
    }

    if (ARR_ofst(ir) != 0) {
        //CASE: struct S {int a, b;} s[10];
        //the elem_ty is struct S.
        //s[1].b has ARR_ofst(ir)==4
        //can simply to: 1*sizeof(struct S) + offset(4) + lda(s).
        IR * imm = buildImmInt((HOST_INT)(ARR_ofst(ir)), indextyid);
        ofst_exp = buildBinaryOpSimp(IR_ADD, indextyid, ofst_exp, imm);
    }

    ASSERT0(ARR_base(ir) && ARR_base(ir)->is_ptr());
    SimpCtx tcont(*ctx);
    SIMP_ret_array_val(&tcont) = false;
    IR * newbase = simplifyExpression(ARR_base(ir), &tcont);
    ctx->appendStmt(tcont);
    ctx->unionBottomupFlag(tcont);

    ASSERT0(newbase && newbase->is_ptr());
    ARR_base(ir) = NULL;

    //'array_addr' is address of an ARRAY, and it is pointer type.
    //Given 'array_addr + n', the result-type of '+' must
    //be pointer type as well.
    //Note do not call buildBinaryOp(IR_ADD...) to generate ir.
    //Because that when 'sub' is pointer, the extra IR_MUL
    //operation will be generated.
    IR * array_addr = buildBinaryOpSimp(IR_ADD,
        dm->getPointerType(ir->get_dtype_size(dm)), newbase, ofst_exp);

    if (SIMP_to_pr_mode(ctx) && !array_addr->is_pr()) {
        SimpCtx ttcont(*ctx);
        SIMP_ret_array_val(&ttcont) = true;
        array_addr->setParentPointer(true);
        array_addr = simplifyExpression(array_addr, &ttcont);
        ctx->appendStmt(ttcont);
        ctx->unionBottomupFlag(ttcont);
    }

    if (SIMP_to_lowest_height(ctx)) {
       //CASE: If IR_parent is NULL, the situation is
       //caller attempt to enforce simplification to array expression
       //whenever possible.
       SimpCtx ttcont(*ctx);
       SIMP_ret_array_val(&ttcont) = true;
       array_addr = simplifyExpression(array_addr, &ttcont);
       ctx->appendStmt(ttcont);
       ctx->unionBottomupFlag(ttcont);

       //IR * t = buildPR(array_addr->get_type());
       //IR * mv = buildStorePR(PR_no(t), t->get_type(), array_addr);
       //allocRefForPR(mv);
       //ctx->appendStmt(mv);
       //array_addr = t;
    }

    //freeIRTree(ir); //Should free ir in caller's function.
    SIMP_changed(ctx) = true;
    return array_addr;
}


IR * Region::simplifyBinAndUniExpression(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir && isBinAndUniExp(ir));

    if (ir->is_ild() && !SIMP_ild_ist(ctx)) { return ir; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->get_kid(i);
        if (kid != NULL) {
            IR * x = simplifyExpression(kid, ctx);
            ir->set_kid(i, x);
        }
    }

    if (!SIMP_to_lowest_height(ctx) || isLowestHeightExp(ir, ctx)) {
        return ir;
    }

    //Do lowering.
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->get_kid(i);
        if (k == NULL) { continue; }

        //Lower ARRAY arith :
        //  a[i] + b[i]
        //=>
        //  t1 = a[i]
        //  t2 = b[i]
        //  t1 + t2
        if (SIMP_array_to_pr_mode(ctx) && k->is_array()) {
            ir->set_kid(i, simpToPR(k, ctx));
        }
    }

    return simpToPR(ir, ctx);
}


IR * Region::simpToPR(IR * ir, SimpCtx * ctx)
{
    IR * pr = buildPR(ir->get_type());
    allocRefForPR(pr);

    IR * st = buildStorePR(PR_no(pr), pr->get_type(), ir);
    allocRefForPR(st);

    copyDbx(st, ir, this); //keep dbg info for new STMT.
    ctx->appendStmt(st);
    SIMP_changed(ctx) = true;

    //Do NOT free ir here, caller will do that.
    return pr;
}


//Return new generated expression's value.
//'ir': ir may be in parameter list if its prev or next is not empty.
IR * Region::simplifyExpression(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) { return NULL; }
    ASSERT(ir->is_exp(), ("expect non-statement node"));

    //ir can not in list, or it may incur illegal result.
    ASSERT0(ir->is_single());
    switch (ir->get_code()) {
    case IR_CONST: return ir;
    case IR_ID: return ir;
    case IR_LD:
        if (SIMP_to_pr_mode(ctx)) {
            return simpToPR(ir, ctx);
        }
        return ir;
    case IR_PR: return ir;
    case IR_ARRAY: return simplifyArray(ir, ctx);
    case IR_LDA: //&sym, get address of 'sym'
        if (SIMP_to_pr_mode(ctx)) {
            return simpToPR(ir, ctx);
        }
        return ir;
    case IR_LAND: //logical and &&
    case IR_LOR: //logical or ||
    case IR_LNOT: //logical not
    case IR_EQ: //==
    case IR_NE: //!=
    case IR_LT:
    case IR_GT:
    case IR_GE:
    case IR_LE:
        return simplifyJudgeDet(ir, ctx);
    case IR_ILD:
    case IR_BAND: //inclusive and &
    case IR_BOR: //inclusive or  |
    case IR_XOR: //exclusive or ^
    case IR_BNOT: //bitwise not
    case IR_NEG: //negative
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LABEL:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_CVT: //data type convertion
        return simplifyBinAndUniExpression(ir, ctx);
    case IR_SELECT: return simplifySelect(ir, ctx);
    default: ASSERT(0, ("cannot simplify '%s'", IRNAME(ir)));
    } //end switch
    return NULL;
}


//Return new generated expression's value.
IR * Region::simplifyJudgeDet(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) return NULL;
    ASSERT0(ir->is_judge());
    ASSERT0(ir->is_single());
    switch (IR_code(ir)) {
    case IR_LAND: //logical and &&
    case IR_LOR: //logical or ||
        {
            if (SIMP_lor_land(ctx)) {
                SimpCtx tcont(*ctx);
                SIMP_ret_array_val(&tcont) = true;
                IR * newir = NULL;
                if (ir->is_lor()) {
                    newir = simplifyLogicalOr(ir, &tcont);
                } else {
                    newir = simplifyLogicalAnd(ir, &tcont);
                }
                ctx->unionBottomupFlag(tcont);

                ASSERT0(newir->is_exp());
                IR * lst = SIMP_stmtlist(&tcont);
                ASSERT0(newir != ir);
                SimpCtx t_tcont(tcont);
                lst = simplifyStmtList(lst, &t_tcont);
                ctx->appendStmt(lst);
                ir = newir;
            } else {
                for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                    IR * kid = ir->get_kid(i);
                    if (kid != NULL) {
                        ir->set_kid(i, simplifyExpression(kid, ctx));
                    }
                }
            }

            if (SIMP_to_lowest_height(ctx) && !isLowest(ir)) {
                ir = simpToPR(ir, ctx);
            }
            return ir;
        }
    case IR_LNOT: //logical not
        {
            if (SIMP_lnot(ctx)) {
                SimpCtx tcont(*ctx);
                SIMP_ret_array_val(&tcont) = true;
                IR * newir = simplifyLogicalNot(ir, &tcont);
                ASSERT0(newir->is_exp());
                IR * lst = SIMP_stmtlist(&tcont);
                ASSERT(newir->is_pr(),
                       ("For now, newir will fairly be IR_PR. But it is not "
                        "certain in the future."));
                SimpCtx t_tcont(tcont);
                lst = simplifyStmtList(lst, &t_tcont);
                ctx->appendStmt(lst);
                ir = newir;
                ctx->unionBottomupFlag(tcont);
            } else {
                for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                    IR * kid = ir->get_kid(i);
                    if (kid != NULL) {
                        ir->set_kid(i, simplifyExpression(kid, ctx));
                    }
                }
            }

            if (SIMP_to_lowest_height(ctx) && !isLowest(ir)) {
                ir = simpToPR(ir, ctx);
            }
            return ir;
        }
    case IR_EQ:
    case IR_NE:
    case IR_LT:
    case IR_GT:
    case IR_GE:
    case IR_LE:
        {
            ASSERT0(IR_parent(ir));
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * kid = ir->get_kid(i);
                if (kid != NULL) {
                    ir->set_kid(i, simplifyExpression(kid, ctx));
                }
            }

            if (SIMP_to_lowest_height(ctx) && !isLowest(ir)) {
                ir = simpToPR(ir, ctx);
            }
            return ir;
        }
        break;
    default: UNREACH();
    } //end switch
    return NULL;
}


//Simplify array's ingredient.
IR * Region::simplifyArrayIngredient(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_array() || ir->is_starray());

    IR * stmtlist = NULL;
    IR * last = NULL;

    //Simplify base.
    SimpCtx basectx(*ctx);
    SIMP_ret_array_val(&basectx) = true;
    IR * newbase = simplifyExpression(ARR_base(ir), &basectx);
    if (SIMP_changed(&basectx)) {
        ARR_base(ir) = newbase;
        IR_parent(newbase) = ir;
    }
    ctx->unionBottomupFlag(basectx);
    if (SIMP_stmtlist(&basectx) != NULL) {
        xcom::add_next(&stmtlist, &last, SIMP_stmtlist(&basectx));
    }

    //Simplify sublist.
    IR * newsublist = NULL;
    IR * newsublast = NULL;
    IR * s = xcom::removehead(&ARR_sub_list(ir));
    for (; s != NULL; s = xcom::removehead(&ARR_sub_list(ir))) {
        SimpCtx subctx(*ctx);
        SIMP_ret_array_val(&subctx) = true;
        IR * news = simplifyExpression(s, &subctx);

        if (SIMP_to_lowest_height(ctx) && !news->is_leaf()) {
            news = simpToPR(news, &subctx);
        }

        xcom::add_next(&newsublist, &newsublast, news);

        if (SIMP_stmtlist(&subctx) != NULL) {
            xcom::add_next(&stmtlist, &last, SIMP_stmtlist(&subctx));
        }
        ctx->unionBottomupFlag(subctx);
    }
    ARR_sub_list(ir) = newsublist;
    ir->setParent(newsublist);
    return stmtlist;
}


IR * Region::simplifyStoreArray(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_starray());
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    IR * ret_list = NULL;
    IR * last = NULL;
    Type const* type = ir->get_type();

    if (!SIMP_array(ctx)) {
        IR * stmtlst = simplifyArrayIngredient(ir, ctx);
        if (stmtlst != NULL) {
            xcom::add_next(&ret_list, &last, stmtlst);
        }
    }

    //Simplify rhs.
    SimpCtx rhsctx(*ctx);
    SIMP_ret_array_val(&rhsctx) = true;
    IR * rhsval = simplifyExpression(STARR_rhs(ir), &rhsctx);
    if (SIMP_changed(&rhsctx)) {
        STARR_rhs(ir) = rhsval;
        IR_parent(rhsval) = ir;
    }
    ctx->unionBottomupFlag(rhsctx);
    if (SIMP_stmtlist(&rhsctx) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&rhsctx));
    }

    SimpCtx tcont(*ctx);
    IR * array_addr = NULL;
    IR * ret = NULL;

    if (!SIMP_array(ctx)) { ret = ir; goto FIN; }

    //Simplify array address expression for STARRAY stmt.
    SIMP_ret_array_val(&tcont) = false; //We need array address expression.

    //simplyArray will free ir. That will make rhs invalid.
    STARR_rhs(ir) = NULL;

    //Need to free ir in current function.
    array_addr = simplifyArray(ir, &tcont);

    ctx->unionBottomupFlag(tcont);

    if (SIMP_stmtlist(&tcont) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }

    ret = buildIstore(array_addr, rhsval, type);
    ret->setRefMD(ir->getRefMD(), this);
    ret->setRefMDSet(ir->getRefMDSet(), this);
    if (get_du_mgr() != NULL) {
        get_du_mgr()->changeDef(ret, ir, getMiscBitSetMgr());
    }
    freeIRTree(ir);
FIN:
    xcom::add_next(&ret_list, &last, ret);
    return ret_list;
}


//Simplify array operator.
//Note ir may be freed.
IR * Region::simplifyArray(IR * ir, SimpCtx * ctx)
{
    if (!SIMP_array(ctx)) {
        IR * stmtlst = simplifyArrayIngredient(ir, ctx);
        if (stmtlst != NULL) {
            ctx->appendStmt(stmtlst);
        }
        return ir;
    }

    IR * array_addr = simplifyArrayAddrExp(ir, ctx);
    //Need to free ir in current function.
    ASSERT(!ir->is_undef(), ("ir has been freed"));

    SIMP_changed(ctx) = true;
    if (!SIMP_ret_array_val(ctx)) {
        return array_addr;
    }

    //Simplify array elem value.
    if (array_addr->is_id()) {
        IR * ld = buildLoad(ID_info(array_addr), array_addr->get_type());
        //Load variable which is an array.
        ld->setRefMD(ir->getRefMD(), this);
        ld->setRefMDSet(ir->getRefMDSet(), this);
        freeIRTree(ir);
        freeIRTree(array_addr);
        if (get_du_mgr() != NULL) {
            get_du_mgr()->changeUse(ld, ir, getMiscBitSetMgr());
        }
        return ld;
    }

    if (SIMP_to_pr_mode(ctx)) {
        if (!array_addr->is_pr()) {
            array_addr = simpToPR(array_addr, ctx);
        }

        //Load array element value.
        IR * elem_val = buildIload(array_addr, ir->get_type());
        elem_val->setRefMD(ir->getRefMD(), this);
        elem_val->setRefMDSet(ir->getRefMDSet(), this);
        if (get_du_mgr() != NULL) {
            get_du_mgr()->changeUse(elem_val, ir, getMiscBitSetMgr());
        }
        freeIRTree(ir);

        IR * pr = buildPR(elem_val->get_type());
        allocRefForPR(pr);

        IR * stpr = buildStorePR(PR_no(pr), pr->get_type(), elem_val);
        allocRefForPR(stpr);

        //keep dbg info for new STMT.
        copyDbx(stpr, elem_val, this);
        ctx->appendStmt(stpr);
        return pr;
    }

    if (SIMP_to_lowest_height(ctx)) {
        if (!array_addr->is_leaf()) {
            array_addr = simpToPR(array_addr, ctx);
        }

        //Load array element's value.
        IR * elem_val = buildIload(array_addr, ir->get_type());
        elem_val->setRefMD(ir->getRefMD(), this);
        elem_val->setRefMDSet(ir->getRefMDSet(), this);
        if (get_du_mgr() != NULL) {
            get_du_mgr()->changeUse(elem_val, ir, getMiscBitSetMgr());
        }
        freeIRTree(ir);

        IR * pr = buildPR(elem_val->get_type());
        allocRefForPR(pr);

        IR * st = buildStorePR(PR_no(pr), pr->get_type(), elem_val);
        allocRefForPR(st);

        //keep dbg info for new STMT.
        copyDbx(st, array_addr, this);
        ctx->appendStmt(st);
        return pr;
    }

    //Load array element value.
    IR * elem_val = buildIload(array_addr, ir->get_type());
    elem_val->setRefMD(ir->getRefMD(), this);
    elem_val->setRefMDSet(ir->getRefMDSet(), this);
    if (get_du_mgr() != NULL) {
        get_du_mgr()->changeUse(elem_val, ir, getMiscBitSetMgr());
    }
    freeIRTree(ir);
    return elem_val;
}


IR * Region::simplifyCall(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_calls_stmt());
    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    SIMP_ret_array_val(&tcont) = true;

    bool origin_to_lowest_height = SIMP_to_lowest_height(&tcont);

    IR * newp = NULL;
    IR * last = NULL;
    bool lchange = false;
    while (CALL_param_list(ir) != NULL) {
        IR * p = xcom::removehead(&CALL_param_list(ir));

        if (g_is_simplify_parameter && !p->is_memory_opnd() && !p->is_lda()) {
            //We always simplify parameters to lowest height to
            //facilitate the query of point-to set.
            //e.g: IR_DU_MGR is going to compute may point-to while
            //ADD is pointer type. But only MD has point-to set.
            //The query of point-to to ADD(id:6) is failed.
            //So we need to store the add's value to a PR,
            //and it will reserved the point-to set information.
            //
            //    call 'getNeighbour'
            //       add (ptr<24>) param4 id:6
            //           lda (ptr<96>) id:31
            //               id (mc<96>, 'pix_a')
            //           mul (u32) id:13
            //               ld (i32 'i')
            //               intconst 24|0x18 (u32) id:14
            SIMP_to_lowest_height(&tcont) = true;
        }

        p = simplifyExpression(p, &tcont);

        if (SIMP_to_lowest_height(&tcont) && !p->is_leaf()) {
            p = simpToPR(p, &tcont);
        }

        SIMP_to_lowest_height(&tcont) = origin_to_lowest_height;

        xcom::add_next(&newp, &last, p);

        lchange |= SIMP_changed(&tcont);

        if (SIMP_changed(&tcont)) {
            IR_parent(p) = ir;
        }

        SIMP_changed(&tcont) = false;
    }

    CALL_param_list(ir) = newp;
    if (newp != NULL) {
        ir->setParent(newp);
    }

    IR * ret_list = SIMP_stmtlist(&tcont);
    xcom::add_next(&ret_list, ir);
    SIMP_changed(ctx) |= lchange;
    ctx->unionBottomupFlag(tcont);
    return ret_list;
}


IR * Region::simplifyStorePR(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_stpr());

    ASSERT0(SIMP_stmtlist(ctx) == NULL);

    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    SIMP_ret_array_val(&tcont) = true;
    STPR_rhs(ir) = simplifyExpression(STPR_rhs(ir), &tcont);
    IR_parent(STPR_rhs(ir)) = ir;
    ctx->unionBottomupFlag(tcont);

    IR * ret_list = NULL;
    IR * last = NULL;
    if (SIMP_stmtlist(&tcont) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }
    xcom::add_next(&ret_list, &last, ir);
    return ret_list;
}


IR * Region::simplifySetelem(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_setelem());

    ASSERT0(SIMP_stmtlist(ctx) == NULL);

    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    SIMP_ret_array_val(&tcont) = true;
    SETELEM_rhs(ir) = simplifyExpression(SETELEM_rhs(ir), &tcont);
    IR_parent(SETELEM_rhs(ir)) = ir;
    ctx->unionBottomupFlag(tcont);

    IR * ret_list = NULL;
    IR * last = NULL;
    if (SIMP_stmtlist(&tcont) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }
    xcom::add_next(&ret_list, &last, ir);

    //Process offset.
    tcont.copy(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    SETELEM_ofst(ir) = simplifyExpression(SETELEM_ofst(ir), &tcont);
    IR_parent(SETELEM_ofst(ir)) = ir;
    ctx->unionBottomupFlag(tcont);
    last = NULL;
    if (SIMP_stmtlist(&tcont) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }
    xcom::add_next(&ret_list, &last, ir);

    return ret_list;
}


IR * Region::simplifyGetelem(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_getelem());

    ASSERT0(SIMP_stmtlist(ctx) == NULL);

    //Process base.
    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    SIMP_ret_array_val(&tcont) = true;
    GETELEM_base(ir) = simplifyExpression(GETELEM_base(ir), &tcont);
    IR_parent(GETELEM_base(ir)) = ir;
    ctx->unionBottomupFlag(tcont);

    IR * ret_list = NULL;

    IR * last = NULL;
    if (SIMP_stmtlist(&tcont) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }
    xcom::add_next(&ret_list, &last, ir);

    //Process offset.
    tcont.copy(*ctx);
    SIMP_ret_array_val(&tcont) = true;
    GETELEM_ofst(ir) = simplifyExpression(GETELEM_ofst(ir), &tcont);
    IR_parent(GETELEM_ofst(ir)) = ir;
    ctx->unionBottomupFlag(tcont);
    last = NULL;
    if (SIMP_stmtlist(&tcont) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }
    xcom::add_next(&ret_list, &last, ir);

    return ret_list;
}


IR * Region::simplifyIstore(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_ist());
    IR * ret_list = NULL;
    IR * last = NULL;
    ASSERT0(SIMP_stmtlist(ctx) == NULL);

    SimpCtx basectx(*ctx);
    SIMP_ret_array_val(&basectx) = true;

    if (SIMP_ild_ist(ctx)) {
        //Reduce the tree height.
        IST_base(ir) = simplifyExpression(IST_base(ir), &basectx);
        IR_parent(IST_base(ir)) = ir;
        ctx->unionBottomupFlag(basectx);
    }

    if (SIMP_stmtlist(&basectx) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&basectx));
    }

    if (SIMP_to_lowest_height(ctx) &&
        IST_base(ir)->is_ild() &&
        SIMP_ild_ist(ctx)) {
        //Handle a special case, the following tree is obscure,
        //transform it to be more understandable.
        //  IST(ILD(PR1), ...)
        //=>
        //  PR2 = ILD(PR1)
        //  IST(PR2, ...)
        SimpCtx istbasectx;
        IST_base(ir) = simpToPR(IST_base(ir), &istbasectx);
        ir->setParent(IST_base(ir));
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&istbasectx));
        SIMP_changed(ctx) = true;
    }

    //Simplify rhs.
    SimpCtx rhsctx(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    SIMP_ret_array_val(&rhsctx) = true;
    IST_rhs(ir) = simplifyExpression(IST_rhs(ir), &rhsctx);
    ir->setParent(IST_rhs(ir));
    ctx->unionBottomupFlag(rhsctx);

    if (SIMP_stmtlist(&rhsctx) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&rhsctx));
    }
    xcom::add_next(&ret_list, &last, ir);
    return ret_list;
}


IR * Region::simplifyStore(IR * ir, SimpCtx * ctx)
{
    ASSERT0(ir->is_st());

    ASSERT0(SIMP_stmtlist(ctx) == NULL);

    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    SIMP_ret_array_val(&tcont) = true;
    ST_rhs(ir) = simplifyExpression(ST_rhs(ir), &tcont);
    IR_parent(ST_rhs(ir)) = ir;

    ctx->unionBottomupFlag(tcont);
    IR * ret_list = NULL;
    IR * last = NULL;
    if (SIMP_stmtlist(&tcont) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&tcont));
    }
    xcom::add_next(&ret_list, &last, ir);
    return ret_list;
}


IR * Region::simplifyBranch(IR * ir, SimpCtx * ctx)
{
    IR * ret_list = NULL;
    SimpCtx tcont(*ctx);
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    SIMP_ret_array_val(&tcont) = true;

    if ((SIMP_lor_land(ctx) &&
         (BR_det(ir)->is_lor() || BR_det(ir)->is_land())) ||
        (SIMP_lnot(ctx) && BR_det(ir)->is_lnot())) {
        ret_list = simplifyLogicalDet(ir, &tcont);
        ret_list = simplifyStmtList(ret_list, &tcont);
    } else {
        for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * kid = ir->get_kid(i);
            if (kid != NULL) {
                ir->set_kid(i, simplifyExpression(kid, &tcont));
                if (SIMP_stmtlist(&tcont) != NULL) {
                    xcom::add_next(&ret_list, SIMP_stmtlist(&tcont));
                    SIMP_stmtlist(&tcont) = NULL;
                }
            }
        }
        xcom::add_next(&ret_list, ir);
    }
    ctx->unionBottomupFlag(tcont);
    return ret_list;
}


IR * Region::simplifyIf(IR * ir, SimpCtx * ctx)
{
    if (SIMP_if(ctx)) {
        return simplifyIfSelf(ir, ctx);
    }

    //Det
    SimpCtx detctx(*ctx);
    IF_det(ir) = simplifyExpression(IF_det(ir), &detctx);
    ir->setParent(IF_det(ir));
    ASSERT(SIMP_stmtlist(&detctx) == NULL, ("invalid determinator"));

    //Truebody
    SimpCtx truectx(*ctx);
    IF_truebody(ir) = simplifyStmtList(IF_truebody(ir), &truectx);
    if (SIMP_changed(&truectx)) {
        ir->setParent(IF_truebody(ir));
    }
    ASSERT(SIMP_stmtlist(&truectx) == NULL,
           ("should already be added to truebody"));

    //Falsebody
    SimpCtx falsectx(*ctx);
    IF_falsebody(ir) = simplifyStmtList(IF_falsebody(ir), &falsectx);
    if (SIMP_changed(&falsectx)) {
        ir->setParent(IF_falsebody(ir));
    }
    ASSERT(SIMP_stmtlist(&falsectx) == NULL,
           ("should already be added to falsebody"));

    return ir;
}


IR * Region::simplifyLoopIngredient(IR * ir, SimpCtx * ctx)
{
    //Det
    SimpCtx detctx(*ctx);
    LOOP_det(ir) = simplifyExpression(LOOP_det(ir), &detctx);
    ir->setParent(LOOP_det(ir));
    ASSERT(SIMP_stmtlist(&detctx) == NULL, ("invalid determinator"));

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
        ASSERT(SIMP_stmtlist(&stepctx) == NULL,
               ("step of DoLoop is too complex, the operation "
                "should only be reduction"));
    }

    //loopbody
    SimpCtx bodyctx(*ctx);
    LOOP_body(ir) = simplifyStmtList(LOOP_body(ir), &bodyctx);
    if (SIMP_changed(&bodyctx)) {
        ir->setParent(LOOP_body(ir));
    }
    ASSERT(SIMP_stmtlist(&bodyctx) == NULL,
           ("should already be added to truebody"));

    IR * ret_list = NULL;
    IR * last = NULL;
    if (ir->is_doloop() && SIMP_stmtlist(&initctx) != NULL) {
        xcom::add_next(&ret_list, &last, SIMP_stmtlist(&initctx));
    }
    xcom::add_next(&ret_list, &last, ir);

    return ret_list;
}


IR * Region::simplifyDoWhile(IR * ir, SimpCtx * ctx)
{
    if (SIMP_dowhile(ctx)) {
        return simplifyDoWhileSelf(ir, ctx);
    }

    ASSERT(!SIMP_break(ctx) && !SIMP_continue(ctx),
           ("Must simplify Loop-stmt if you want to simply Break/Continue."));

    return simplifyLoopIngredient(ir, ctx);
}


IR * Region::simplifyWhileDo(IR * ir, SimpCtx * ctx)
{
    if (SIMP_whiledo(ctx)) {
        return simplifyWhileDoSelf(ir, ctx);
    }

    ASSERT(!SIMP_break(ctx) && !SIMP_continue(ctx),
           ("Must simplify Loop-stmt if you want to simply Break/Continue."));

    return simplifyLoopIngredient(ir, ctx);
}


IR * Region::simplifyDoLoop(IR * ir, SimpCtx * ctx)
{
    if (SIMP_doloop(ctx)) {
        return simplifyDoLoopSelf(ir, ctx);
    }

    ASSERT(!SIMP_break(ctx) && !SIMP_continue(ctx),
           ("Must simplify Loop-stmt if you want to simply Break/Continue."));

    return simplifyLoopIngredient(ir, ctx);
}


IR * Region::simplifySwitch(IR * ir, SimpCtx * ctx)
{
    if (SIMP_switch(ctx)) {
        return simplifySwitchSelf(ir, ctx);
    }

    IR * body = SWITCH_body(ir);
    if (body == NULL) { return ir; }

    ASSERT(!SIMP_break(ctx),
           ("Must simplify Swich-stmt if you want to simply Break."));

    SWITCH_body(ir) = NULL;
    body = simplifyStmtList(body, ctx);
    if (ir->get_parent() == NULL) {
        for (IR * t = body; t != NULL; t = t->get_next()) {
            IR_parent(t) = NULL;
        }
    }
    xcom::add_next(&ir, body);

    SIMP_changed(ctx) = true;
    SIMP_need_recon_bblist(ctx) = true;
    return ir;
}


//Simply IR and its kids.
//Remember copy Dbx info for new STMT to conveninent to dump use.
//Return new ir stmt-list.
IR * Region::simplifyStmt(IR * ir, SimpCtx * ctx)
{
    if (ir == NULL) { return NULL; }
    ASSERT(ir->is_stmt(), ("expect statement node"));
    ASSERT(ir->is_single(), ("ir should be remove out of list"));

    IR * ret_list = NULL;
    switch (ir->get_code()) {
    case IR_CALL:
    case IR_ICALL: //indirective call
        ret_list = simplifyCall(ir, ctx);
        break;
    case IR_ST:
        ret_list = simplifyStore(ir, ctx);
        break;
    case IR_STPR:
        ret_list = simplifyStorePR(ir, ctx);
        break;
    case IR_STARRAY:
        ret_list = simplifyStoreArray(ir, ctx);
        break;
    case IR_SETELEM:
        ret_list = simplifySetelem(ir, ctx);
        break;
    case IR_GETELEM:
        ret_list = simplifyGetelem(ir, ctx);
        break;
    case IR_IST:
        ret_list = simplifyIstore(ir, ctx);
        break;
    case IR_IGOTO:
        ASSERT0(SIMP_stmtlist(ctx) == NULL);
        ret_list = simplifyIgoto(ir, ctx);
        break;
    case IR_GOTO:
    case IR_LABEL:
    case IR_CASE:
    case IR_RETURN:
        {
            SimpCtx tcont(*ctx);
            ASSERT0(SIMP_stmtlist(ctx) == NULL);
            SIMP_ret_array_val(&tcont) = true;
            for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * kid = ir->get_kid(i);
                if (kid != NULL) {
                    ir->set_kid(i, simplifyExpression(kid, &tcont));
                    ctx->unionBottomupFlag(tcont);
                    if (SIMP_stmtlist(&tcont) != NULL) {
                        xcom::add_next(&ret_list, SIMP_stmtlist(&tcont));
                        SIMP_stmtlist(&tcont) = NULL;
                    }
                }
            }
            xcom::add_next(&ret_list, ir);
        }
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        ret_list = simplifyBranch(ir, ctx);
        break;
    case IR_BREAK:
        ASSERT0(SIMP_stmtlist(ctx) == NULL);
        if (SIMP_break(ctx)) {
            ASSERT0(SIMP_break_label(ctx));
            IR * go = buildGoto(SIMP_break_label(ctx));
            copyDbx(go, ir, this);
            freeIRTree(ir);
            ir = go;
        }
        xcom::add_next(&ret_list, ir);
        break;
    case IR_CONTINUE:
        ASSERT0(SIMP_stmtlist(ctx) == NULL);
        if (SIMP_continue(ctx)) {
            ASSERT0(SIMP_continue_label(ctx));
            IR * go = buildGoto(SIMP_continue_label(ctx));
            copyDbx(go, ir, this);
            freeIRTree(ir);
            ir = go;
        }
        xcom::add_next(&ret_list, ir);
        break;
    case IR_IF:
        ASSERT0(SIMP_stmtlist(ctx) == NULL);
        ret_list = simplifyIf(ir, ctx);
        break;
    case IR_DO_WHILE:
        ASSERT0(SIMP_stmtlist(ctx) == NULL);
        ret_list = simplifyDoWhile(ir, ctx);
        break;
    case IR_WHILE_DO:
        ASSERT0(SIMP_stmtlist(ctx) == NULL);
        ret_list = simplifyWhileDo(ir, ctx);
        break;
    case IR_DO_LOOP:
        ASSERT0(SIMP_stmtlist(ctx) == NULL);
        ret_list = simplifyDoLoop(ir, ctx);
        break;
    case IR_SWITCH:
        ASSERT0(SIMP_stmtlist(ctx) == NULL);
        ret_list = simplifySwitch(ir, ctx);
        break;
    case IR_REGION:
    case IR_PHI:
        return ir;
    default: ASSERT(0, ("'%s' is not a statement",IRNAME(ir)));
    }
    setParentPointerForIRList(ret_list);
    return ret_list;
}


//Return new generated IR stmt.
IR * Region::simplifyStmtList(IR * ir_list, SimpCtx * ctx)
{
    if (ir_list == NULL) { return NULL; }
    IR * ret_list = NULL;
    ASSERT0(SIMP_stmtlist(ctx) == NULL);
    bool redo_simp = true;
    UINT count = 0;
    while (redo_simp) {
        count++;
        ASSERT(count < 50, ("dead lock"));
        redo_simp = false;

        IR * last = NULL;
        for (IR * stmt = xcom::removehead(&ir_list);
             stmt != NULL; stmt = xcom::removehead(&ir_list)) {
            IR * new_stmt_list = simplifyStmt(stmt, ctx);
            ASSERT0(SIMP_stmtlist(ctx) == NULL);
            if (SIMP_to_lowest_height(ctx)) {
                //Check if new generated stmts still need to resimplify.
                for (IR * x = new_stmt_list; x != NULL; x = x->get_next()) {
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
            ret_list = NULL;
        }
    }

    return ret_list;
}


//NOTE: simplification should not generate indirect memory operation.
void Region::simplifyBB(IRBB * bb, SimpCtx * ctx)
{
    List<IR*> new_ir_list;
    C<IR*> * ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end();
         ct = BB_irlist(bb).get_next(ct)) {
        IR * stmt = ct->val();
        ASSERT0(stmt && stmt->is_single());
        IR * newstmt_lst = simplifyStmt(stmt, ctx);
        while (newstmt_lst != NULL) {
            IR * newir = xcom::removehead(&newstmt_lst);
            newir->set_bb(bb);
            ASSERT0(newir->verify(this));
            new_ir_list.append_tail(newir);
        }
    }
    BB_irlist(bb).copy(new_ir_list);
}


//NOTE: simplification should not generate indirect memory operation.
void Region::simplifyBBlist(BBList * bbl, SimpCtx * ctx)
{
    START_TIMER("Simplify IRBB list");
    C<IRBB*> * ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        simplifyBB(bb, ctx);
    }
    END_TIMER();
}

} //namespace xoc
