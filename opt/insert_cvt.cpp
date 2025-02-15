/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START InsertCvt
//
InsertCvt::InsertCvt(Region * rg) : Pass(rg)
{
    ASSERT0(rg != nullptr);
    m_tm = rg->getTypeMgr();
}


IR * InsertCvt::convertILoad(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->is_ild());
    ASSERT0(ir->is_single());
    IR * base = ILD_base(ir);
    ILD_base(ir) = convertIR(base, change, rc);
    if (change) {
        IR_parent(ILD_base(ir)) = ir;
    }
    return ir;
}


IR * InsertCvt::convertIStore(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->is_ist());
    ir->setRHS(convertIR(ir->getRHS(), change, rc));
    ir->setRHS(insertCvtImpl(ir, ir->getRHS(), change));
    return ir;
}


IR * InsertCvt::convertDirectStore(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->hasRHS());
    ir->setRHS(convertIR(ir->getRHS(), change, rc));
    ir->setRHS(insertCvtImpl(ir, ir->getRHS(), change));
    return ir;
}


IR * InsertCvt::convertSetelem(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->is_setelem());
    IR * base = convertIR(SETELEM_base(ir), change, rc);
    if (base != SETELEM_base(ir)) {
        ir->setParent(base);
        SETELEM_base(ir) = base;
    }
    IR * val = convertIR(SETELEM_val(ir), change, rc);
    if (val != SETELEM_val(ir)) {
        ir->setParent(val);
        SETELEM_val(ir) = val;
    }
    IR * ofst = convertIR(SETELEM_ofst(ir), change, rc);
    if (ofst != SETELEM_ofst(ir)) {
        ir->setParent(ofst);
        SETELEM_ofst(ir) = ofst;
    }
    return ir;
}


IR * InsertCvt::convertGetelem(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->is_getelem());
    IR * base = convertIR(GETELEM_base(ir), change, rc);
    if (base != GETELEM_base(ir)) {
        ir->setParent(base);
        GETELEM_base(ir) = base;
    }
    IR * ofst = convertIR(GETELEM_ofst(ir), change, rc);
    if (ofst != GETELEM_ofst(ir)) {
        ir->setParent(ofst);
        GETELEM_ofst(ir) = ofst;
    }
    return ir;
}


IR * InsertCvt::convertCall(IR * ir, bool & change, InsertCvtCtx & rc)
{
    bool lchange = false;
    if (CALL_arg_list(ir) != nullptr) {
        IR * arg = xcom::removehead(&CALL_arg_list(ir));
        IR * newarglst = nullptr;
        IR * last = nullptr;
        while (arg != nullptr) {
            IR * newp = convertIR(arg, lchange, rc);
            xcom::add_next(&newarglst, &last, newp);
            last = newp;
            arg = xcom::removehead(&CALL_arg_list(ir));
        }
        CALL_arg_list(ir) = newarglst;
    }
    if (lchange) {
        change = true;
        ir->setParentPointer(false);
    }
    return ir;
}


IR * InsertCvt::convertICall(IR * ir, bool & change, InsertCvtCtx & rc)
{
    return convertCall(ir, change, rc);
}


IR * InsertCvt::convertSwitch(IR * ir, bool & change, InsertCvtCtx & rc)
{
    bool lchange = false;
    SWITCH_vexp(ir) = convertIR(SWITCH_vexp(ir), lchange, rc);
    if (lchange) {
        IR_parent(SWITCH_vexp(ir)) = ir;
        change = true;
    }
    lchange = false;
    SWITCH_body(ir) = convertIRlist(SWITCH_body(ir), lchange, rc);
    change |= lchange;
    return ir;
}


IR * InsertCvt::convertBr(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->isConditionalBr());
    bool lchange = false;
    BR_det(ir) = convertDet(BR_det(ir), lchange, rc);
    change |= lchange;
    return ir;
}


IR * InsertCvt::convertUnaryOp(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->isUnaryOp());
    bool lchange = false;
    UNA_opnd(ir) = convertIR(UNA_opnd(ir), lchange, rc);
    if (lchange) {
        change = true;
        ir->setParentPointer(false);
    }
    return ir;
}


IR * InsertCvt::convertReturn(IR * ir, bool & change, InsertCvtCtx & rc)
{
    if (RET_exp(ir) == nullptr) { return ir; }
    bool lchange = false;
    RET_exp(ir) = convertIR(RET_exp(ir), lchange, rc);
    if (lchange) {
        change = true;
        ir->setParentPointer(false);
    }
    return ir;
}


IR * InsertCvt::convertSelect(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->is_select());
    SELECT_det(ir) = convertDet(SELECT_det(ir), change, rc);
    SELECT_trueexp(ir) = convertIRlist(SELECT_trueexp(ir), change, rc);
    SELECT_falseexp(ir) = convertIRlist(SELECT_falseexp(ir), change, rc);
    IR * det = convertConstExp(SELECT_det(ir), change, rc);
    if (det != SELECT_det(ir)) {
        SELECT_det(ir) = det;
        ir->setParent(det);
    }
    return ir; //No need to update DU.
}


IR * InsertCvt::convertNeg(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->is_neg());
    return convertConstExp(ir, change, rc);
}


//Logic not: !(0001) = 0000
//Bitwise not: !(0001) = 1110
IR * InsertCvt::convertNot(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->is_lnot() || ir->is_bnot());
    ASSERT0(UNA_opnd(ir)->is_single());
    bool lchange = false;
    UNA_opnd(ir) = convertIR(UNA_opnd(ir), lchange, rc);
    if (lchange) {
        IR_parent(UNA_opnd(ir)) = ir;
    }
    change |= lchange;
    return convertConstExp(ir, change, rc);
}


IR * InsertCvt::convertAllKids(IR * ir, bool & change, InsertCvtCtx & rc)
{
    for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        IR * new_kid = convertIR(kid, change, rc);
        if (new_kid != kid) {
            ir->setKid(i, new_kid);
        }
    }
    return ir;
}


//InsertCvt binary operations.
IR * InsertCvt::convertBinaryOp(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->isBinaryOp());
    ASSERT0(BIN_opnd0(ir)->is_single());
    ASSERT0(BIN_opnd1(ir)->is_single());
    bool lchange = false;
    BIN_opnd0(ir) = convertIR(BIN_opnd0(ir), lchange, rc);
    BIN_opnd1(ir) = convertIR(BIN_opnd1(ir), lchange, rc);
    if (lchange) {
        change |= lchange;
        ir->setParentPointer(false);
    }
    bool lchange2 = false;
    BIN_opnd0(ir) = insertCvtImpl(ir, BIN_opnd0(ir), lchange2);
    BIN_opnd1(ir) = insertCvtImpl(ir, BIN_opnd1(ir), lchange2);
    if (lchange2) {
        change |= lchange2;
        ir->setParentPointer(false);
    }
    insertCvtForBinaryOp(ir, change);
    return ir;
}


IR * InsertCvt::convertStoreArray(IR * ir, bool & change, InsertCvtCtx & rc)
{
    ASSERT0(ir->is_starray());
    ir->setRHS(convertIR(ir->getRHS(), change, rc));
    ir->setRHS(insertCvtImpl(ir, ir->getRHS(), change));
    return ir;
}


IR * InsertCvt::convertArray(IR * ir, bool & change, InsertCvtCtx & rc)
{
    IR * newbase = convertIR(ARR_base(ir), change, rc);
    if (newbase != ARR_base(ir)) {
        ARR_base(ir) = newbase;
        IR_parent(newbase) = ir;
    }
    IR * newsublist = nullptr;
    IR * last = nullptr;
    IR * s = xcom::removehead(&ARR_sub_list(ir));
    for (; s != nullptr;) {
        IR * newsub = convertIR(s, change, rc);
        if (newsub != s) {
            IR_parent(newsub) = ir;
        }
        xcom::add_next(&newsublist, &last, newsub);
        s = xcom::removehead(&ARR_sub_list(ir));
    }
    ARR_sub_list(ir) = newsublist;
    return ir;
}


IR * InsertCvt::convertIRUntilUnchange(IR * ir, bool & change,
                                       InsertCvtCtx & rc)
{
    bool lchange = true;
    IR * newir = nullptr;
    for (; lchange;) {
        lchange = false;
        newir = convertIR(ir, lchange, rc);
        if (lchange) {
            ir = newir;
            change = true;
        }
    }
    return newir;
}


//Perform peephole optimizations.
//m_rg function also responsible for normalizing IR and reassociation.
//NOTE: m_rg function do NOT generate new STMT.
IR * InsertCvt::convertIR(IR * ir, bool & change, InsertCvtCtx & rc)
{
    if (ir == nullptr) { return nullptr; }
    if (ir->hasSideEffect(false) || ir->isDummyOp()) { return ir; }
    bool tmpc = false;
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_EXP:
        break;
    SWITCH_CASE_INDIRECT_MEM_EXP:
        ir = convertILoad(ir, tmpc, rc);
        break;
    SWITCH_CASE_WRITE_ARRAY:
        ir = convertStoreArray(ir, tmpc, rc);
        break;
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR:
        ir = convertDirectStore(ir, tmpc, rc);
        break;
    case IR_SETELEM:
        ir = convertSetelem(ir, tmpc, rc);
        break;
    case IR_GETELEM:
        ir = convertGetelem(ir, tmpc, rc);
        break;
    SWITCH_CASE_INDIRECT_MEM_STMT:
        ir = convertIStore(ir, tmpc, rc);
        break;
    case IR_CALL:
        ir = convertCall(ir, tmpc, rc);
        break;
    case IR_ICALL:
        ir = convertICall(ir, tmpc, rc);
        break;
    SWITCH_CASE_ARITH:
    SWITCH_CASE_LOGIC_BIN:
    SWITCH_CASE_SHIFT:
    SWITCH_CASE_BITWISE_BIN:
        ir = convertBinaryOp(ir, tmpc, rc);
        break;
    case IR_BNOT:
    case IR_LNOT:
        ir = convertNot(ir, tmpc, rc);
        break;
    case IR_NEG:
        ir = convertNeg(ir, tmpc, rc);
        break;
    case IR_ALLOCA:
        ir = convertUnaryOp(ir, tmpc, rc);
        break;
    SWITCH_CASE_UNA_TRIGONOMETRIC:
        ir = convertAllKids(ir, tmpc, rc);
        break;
    SWITCH_CASE_COMPARE: {
        //According input setting to do convertment.
        bool lchange = false;
        BIN_opnd0(ir) = convertIR(BIN_opnd0(ir), lchange, rc);
        BIN_opnd1(ir) = convertIR(BIN_opnd1(ir), lchange, rc);
        if (lchange) { ir->setParentPointer(false); }
        change |= lchange;
        ir = convertBinaryOp(ir, tmpc, rc);
        break;
    }
    case IR_DO_WHILE:
    case IR_WHILE_DO:
        LOOP_det(ir) = convertDet(LOOP_det(ir), tmpc, rc);
        LOOP_body(ir) = convertIRlist(LOOP_body(ir), tmpc, rc);
        break;
    case IR_DO_LOOP:
        LOOP_det(ir) = convertDet(LOOP_det(ir), tmpc, rc);
        LOOP_init(ir) = convertIRlist(LOOP_init(ir), tmpc, rc);
        LOOP_step(ir) = convertIRlist(LOOP_step(ir), tmpc, rc);
        LOOP_body(ir) = convertIRlist(LOOP_body(ir), tmpc, rc);
        break;
    case IR_IF:
        IF_det(ir) = convertDet(IF_det(ir), tmpc, rc);
        IF_truebody(ir) = convertIRlist(IF_truebody(ir), tmpc, rc);
        IF_falsebody(ir) = convertIRlist(IF_falsebody(ir), tmpc, rc);
        break;
    case IR_IGOTO:
        IGOTO_vexp(ir) = convertIR(IGOTO_vexp(ir), tmpc, rc);
        break;
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
        ir = convertSwitch(ir, tmpc, rc);
        break;
    case IR_ARRAY:
        ir = convertArray(ir, tmpc, rc);
        break;
    case IR_CVT:
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        ir = convertBr(ir, tmpc, rc);
        break;
    case IR_RETURN:
        ir = convertReturn(ir, tmpc, rc);
        break;
    case IR_SELECT:
        ir = convertSelect(ir, tmpc, rc);
        break;
    SWITCH_CASE_LOOP_ITER_CFS_OP:
        break;
    case IR_PHI:
        break;
    case IR_CONST:
    case IR_ID:
    case IR_LDA:
    case IR_LABEL:
    case IR_CASE:
    SWITCH_CASE_READ_PR:
    case IR_GOTO:
    case IR_REGION:
        break;
    default: ir = convertExtOp(ir, tmpc, rc);
    }
    if (tmpc && ir != nullptr && ir->is_stmt()) {
        ir->setParentPointer(true);
    }
    change |= tmpc;
    return ir;
}


IR * InsertCvt::convertExtOp(IR * ir, bool & change, InsertCvtCtx & rc)
{
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_STMT:
    SWITCH_CASE_EXT_EXP:
        //TODO:convert the extended operation.
        break;
    default:;
    }
    return ir;
}


//Reshaping determinate expression.
//Only the last non-stmt expression can be reserved to perform determinating.
IR * InsertCvt::convertDet(IR * ir, bool & change, InsertCvtCtx & rc)
{
    return convertIR(ir, change, rc);
}


IR * InsertCvt::convertIRlist(IR * ir_list, bool & change,
                              MOD InsertCvtCtx & rc)
{
    bool lchange = true; //local flag
    while (lchange) {
        lchange = false;
        IR * new_list = nullptr;
        IR * last = nullptr;
        while (ir_list != nullptr) {
            IR * ir = xcom::removehead(&ir_list);
            IR * newIR = convertIR(ir, lchange, rc);
            xcom::add_next(&new_list, &last, newIR);
        }
        change |= lchange;
        ir_list = new_list;
    }
    return ir_list;
}


bool InsertCvt::convertStmtList(MOD BBIRList & ir_list, MOD InsertCvtCtx & rc)
{
    bool change = false;
    IRListIter next_ct;
    ir_list.get_head(&next_ct);
    IRListIter ct = next_ct;
    for (; ct != nullptr; ct = next_ct) {
        IR * ir = ct->val();
        next_ct = ir_list.get_next(next_ct);
        bool lchange = false;
        InsertCvtCtx lrc(rc);
        IR * newir = convertIRUntilUnchange(ir, lchange, lrc);
        change |= lchange;
        if (newir == ir) { continue; }
        change = true;
        if (!RC_stmt_removed(lrc)) {
            //If the old ir still not be removed by
            //callee, remove the old one here.
            //NOTE ir may be IR_UDNEF which has been freed.
            ir_list.EList<IR*, IR2Holder>::remove(ct);
        }
        if (newir == nullptr) { continue; }
        if (next_ct != nullptr) {
            ir_list.insert_before(newir, next_ct);
        } else {
            ir_list.append_tail(newir);
        }
    }
    rc.cleanBottomUpFlag();
    return change;
}


bool InsertCvt::convertBBlist(MOD BBList * ir_bb_list, MOD InsertCvtCtx & rc)
{
    START_TIMER(t, "InsertCvt IRBB list");
    bool change = false;
    BBListIter ct;
    for (ir_bb_list->get_head(&ct);
         ct != ir_bb_list->end(); ct = ir_bb_list->get_next(ct)) {
        change |= convertStmtList(BB_irlist(ct->val()), rc);
    }
    END_TIMER(t, "InsertCvt IRBB list");
    if (!change) { return false; }
    if (rc.getOptCtx()->is_ref_valid()) {
        ASSERT0(m_rg->getDUMgr() && m_rg->getDUMgr()->verifyMDRef());
        //DU chain is kept by convertment.
        ASSERT0(verifyMDDUChain(m_rg, *rc.getOptCtx()));
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, *rc.getOptCtx()));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, *rc.getOptCtx()));
        ASSERT0(verifyIRandBB(ir_bb_list, m_rg));
    }
    return true;
}


void InsertCvt::insertCvtForBinaryOpByPtrType(IR * ir, bool & change)
{
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0->is_ptr() || op1->is_ptr());
    if (!op0->is_ptr() && op1->is_ptr()) {
        xcom::swap(op0, op1);
    }
    ASSERT0(op0->is_ptr());
    if (op1->getTypeSize(m_tm) > op0->getTypeSize(m_tm)) {
        //If longer data type is compared with pointer, it always have to
        //be truncated to the same size of pointer. Otherwise, the pointer
        //comparison is meaningless.
        ASSERTN(op1->getType()->is_ptr_addend() && !op1->is_ptr(),
                ("illegal pointer arith"));
        DATA_TYPE t = m_tm->getPointerSizeDtype();
        IR * newop1 = m_rg->getIRMgr()->buildCvt(op1,
            m_tm->getSimplexTypeEx(t));
        bool find = ir->replaceKid(op1, newop1, false);
        ASSERT0_DUMMYUSE(find);
        copyDbx(newop1, op1, m_rg);
        change = true;
        return;
    }
    //Smaller data size no need to process.
    //e.g: char * p; if (p:ptr < a:i8) { ... }
    //     CVT of a:i8 is dispensable.
}


void InsertCvt::insertCvtForBinaryOp(IR * ir, bool & change)
{
    ASSERT0(ir->isBinaryOp());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    if (op0->is_any() || op1->is_any()) { return; }
    if (op0->getType() == op1->getType()) {
        if (op0->is_mc()) {
            ASSERTN(TY_mc_size(op0->getType()) == TY_mc_size(op1->getType()),
                    ("invalid binop for two D_MC operands"));
        }
        return;
    }
    if (op0->is_ptr() || op1->is_ptr()) {
        insertCvtForBinaryOpByPtrType(ir, change);
        return;
    }
    if (ir->is_logical()) {
        //Type of operand of logical operation does not need to be consistent.
        return;
    }

    //Both op0 and op1 are NOT pointer.
    if (op0->is_vec() || op1->is_vec()) {
        //ASSERT0(op0->getType() == op1->getType());
        //op0 may be vector and op1 may be MC.
        return;
    }

    //If ir is relation operation, op1 should have been swapped to first
    //operand if it is a pointer.
    ASSERTN(!op1->is_ptr(), ("illegal binop for Non-pointer and Pointer"));

    //Both op0 and op1 are NOT vector type.
    Type const* type = m_tm->hoistDTypeForBinOp(op0, op1);
    UINT dt_size = m_tm->getByteSize(type);
    if (op0->getTypeSize(m_tm) != dt_size) {
        BIN_opnd0(ir) = m_rg->getIRMgr()->buildCvt(op0, type);
        copyDbx(BIN_opnd0(ir), op0, m_rg);
        change = true;
        ir->setParentPointer(false);
    }
    if (op1->getTypeSize(m_tm) != dt_size) {
        if (ir->is_asr() || ir->is_lsl() || ir->is_lsr()) {
            //CASE:Second operand of Shift operantion need NOT to be converted.
            //     Second operand indicates the bit that expected to be shifted.
            //e.g: $2(u64) = $8(u64) >> j(u32);
            //  stpr $2:u64 id:37 attachinfo:Dbx
            //      lsr:u64 id:31 attachinfo:Dbx
            //          $8:u64 id:59
            //          ld:u32 'j' id:30 attachinfo:Dbx,MDSSA
        } else {
            BIN_opnd1(ir) = m_rg->getIRMgr()->buildCvt(op1, type);
            copyDbx(BIN_opnd1(ir), op1, m_rg);
            change = true;
            ir->setParentPointer(false);
        }
    }
}


//Kid is float.
IR * InsertCvt::insertCvtForFloatCase2(IR * parent, IR * kid, bool & change)
{
    ASSERT0(kid->is_fp());
    UINT tgt_size = parent->getTypeSize(m_tm);
    UINT src_size = kid->getTypeSize(m_tm);
    bool build = false;
    if (parent->getType()->is_int() || parent->getType()->is_pointer()) {
        //Build the conversion between integer and float.
        build = true;
    } else if (parent->is_fp()) {
        if (tgt_size != src_size) {
            //Build the conversion between different precision float.
            build = true;
        } else {
            //No need to convert.
        }
    } else {
        ASSERTN(0, ("unknown types in convertion"));
    }
    if (!build) { return kid; }
    Type const* cvtty = parent->getType();
    if (parent->is_add() &&
        parent->is_ptr() &&
        kid == ((CBin*)parent)->getOpnd1() &&
        BIN_opnd0(parent)->is_ptr()) {
        //IR_ADD could not add two pointer type. The addend type should be
        //integer.
        cvtty = m_tm->getPointerSizeType();
    }
    ASSERT0(cvtty);
    IR * new_kid = m_rg->getIRMgr()->buildCvt(kid, cvtty);
    copyDbx(new_kid, kid, m_rg);
    change = true;
    return new_kid;
}


//Parent is float.
IR * InsertCvt::insertCvtForFloatCase1(IR * parent, IR * kid, bool & change)
{
    ASSERT0(parent->is_fp());
    UINT tgt_size = parent->getTypeSize(m_tm);
    UINT src_size = kid->getTypeSize(m_tm);
    bool build = false;
    if (kid->getType()->is_int() || kid->getType()->is_pointer()) {
        //Build the conversion between float and integer.
        build = true;
    } else if (kid->is_fp()) {
        if (tgt_size != src_size) {
            //Build the conversion between different precision float.
            build = true;
        } else {
            //No need to convert.
        }
    } else {
        ASSERTN(0, ("unknown types in convertion"));
    }
    if (!build) { return kid; }
    IR * new_kid = m_rg->getIRMgr()->buildCvt(kid, parent->getType());
    copyDbx(new_kid, kid, m_rg);
    change = true;
    return new_kid;
}


//Insert CVT for float if necessary.
IR * InsertCvt::insertCvtForFloat(IR * parent, IR * kid, bool & change)
{
    //Need to insert CVT between different FP by default.
    if (parent->is_fp()) {
        return insertCvtForFloatCase1(parent, kid, change);
    }
    ASSERT0(kid->is_fp());
    return insertCvtForFloatCase2(parent, kid, change);
}


//Insert CVT between 'parent' and 'kid' if need, otherwise return kid.
IR * InsertCvt::insertCvtImpl(IR * parent, IR * kid, bool & change)
{
    switch (parent->getCode()) {
    SWITCH_CASE_COMPARE:
    SWITCH_CASE_LOGIC_BIN:
        return kid;
    SWITCH_CASE_DIRECT_MEM_OP:
    SWITCH_CASE_INDIRECT_MEM_OP:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_CALL:
    SWITCH_CASE_ARITH:
    SWITCH_CASE_BITWISE:
    SWITCH_CASE_LOGIC_UNA:
    SWITCH_CASE_CFS_OP:
    SWITCH_CASE_BRANCH_OP:
    SWITCH_CASE_SHIFT:
    SWITCH_CASE_UNA_REST:
    case IR_STPR:
    case IR_LDA:
    case IR_LABEL:
    case IR_CASE:
    case IR_ARRAY:
    case IR_RETURN:
    case IR_SELECT: {
        ASSERT0(parent && kid);
        Type const* tgt_ty = parent->getType();
        if (tgt_ty->is_any() || kid->getType()->is_any()) {
            //Nothing need to do if converting to ANY.
            return kid;
        }
        if (parent->is_stpr() && !isNeedInsertCvtForStpr(parent, kid)) {
            return kid;
        }
        UINT tgt_size = parent->getTypeSize(m_tm);
        UINT src_size = kid->getTypeSize(m_tm);
        if (parent->is_vec() || kid->is_vec()) {
            return checkSizeForVector(parent, kid);
        }
        if (parent->is_fp() || kid->is_fp()) {
            return insertCvtForFloat(parent, kid, change);
        }
        if (tgt_size <= src_size) {
            //Do not hoist type.
            return kid;
        }
        if (parent->is_ptr() && parent->is_add() &&
            BIN_opnd0(parent)->is_ptr()) {
            //Skip pointer arithmetics.
            return kid;
        }
        if ((parent->is_asr() || parent->is_lsl() || parent->is_lsr()) &&
            kid == BIN_opnd1(parent)) {
            //CASE: Second operand of Shift operantion need NOT to be converted.
            //      Second operand indicates the bit that expected to be
            //      shifted.
            //e.g: $2(u64) = $8(u64) >> j(u32);
            //  stpr $2 : u64 id:37
            //      lsr : u64 id:31
            //          $8 : u64 id:59
            //          ld : u32 'j' id:30
            return kid;
        }
        if (kid->is_const() && kid->is_int()) {
            //kid is integer literal.
            if (tgt_ty->is_string()) {
                IR * new_kid = m_rg->getIRMgr()->buildCvt(kid, tgt_ty);
                copyDbx(new_kid, kid, m_rg);
                kid = new_kid;
                change = true;
            }
            return kid;
        }
        IR * new_kid = m_rg->getIRMgr()->buildCvt(kid, parent->getType());
        copyDbx(new_kid, kid, m_rg);
        change = true;
        return new_kid;
    }
    default: UNREACHABLE();
    }
    return nullptr;
}


IR * InsertCvt::convertConstIntBinary(IR * ir, bool & change)
{
    ASSERT0(ir->isBinaryOp());
    ASSERT0(BIN_opnd0(ir)->is_const());
    ASSERT0(BIN_opnd1(ir)->is_const());
    switch (ir->getCode()) {
    SWITCH_CASE_ARITH:
    SWITCH_CASE_LOGIC_BIN:
    SWITCH_CASE_BITWISE_BIN:
    SWITCH_CASE_COMPARE:
    case IR_ASR:
    case IR_LSL: {
        if (ir->is_fp()) {
            //The result type of binary operation is
            //float point, insert CVT.
            IR * x = m_rg->getIRMgr()->buildCvt(ir, ir->getType());
            copyDbx(x, ir, m_rg);
            ir = x;
            change = true;
            return ir;
        }
        break;
    }
    case IR_LSR: break;
    default: UNREACHABLE();
    }
    return ir;
}


IR * InsertCvt::convertConstExp(IR * ir, bool & change, InsertCvtCtx const& rc)
{
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        IR * new_kid = convertConstExp(kid, change, rc);
        ASSERT0(new_kid);
        if (new_kid != kid) {
            ir->setKid(i, new_kid);
        }
    }
    switch (ir->getCode()) {
    SWITCH_CASE_BIN: {
        IR * t0 = BIN_opnd0(ir);
        IR * t1 = BIN_opnd1(ir);
        ASSERT0(ir->isBinaryOp());
        ASSERTN(t0 && t1, ("binary op"));
        if (t0->is_const() && t1->is_const()) {
            if (t0->is_int() && t1->is_int()) {
                return convertConstIntBinary(ir, change);
            }
        }
        break;
    }
    default: break;
    }
    return ir;
}


bool InsertCvt::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    if (m_rg->getIRList() != nullptr) {
        dumpIRList(m_rg->getIRList(), m_rg);
    } else if (m_rg->getBBList() != nullptr) {
        dumpBBList(m_rg->getBBList(), m_rg);
    }
    m_rg->getLogMgr()->decIndent(2);
    return true;
}


bool InsertCvt::perform(OptCtx & oc, MOD InsertCvtCtx & rc)
{
    bool change = false;
    if (m_rg->getIRList() != nullptr) {
        //Do primitive convertment.
        START_TIMER(t, "Do InsertCvt");
        IR * irs = convertIRlist(m_rg->getIRList(), change, rc);
        ASSERT0(xoc::verifyIRList(irs, nullptr, m_rg));
        m_rg->setIRList(irs);
        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpInsertCvt()) {
            dump();
        }
        END_TIMER(t, "Do InsertCvt");
        return change;
    }
    START_TIMER(t, "Do InsertCvt");
    change = convertBBlist(m_rg->getBBList(), rc);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpInsertCvt()) {
        dump();
    }
    END_TIMER(t, "Do InsertCvt");
    return change;
}


bool InsertCvt::perform(OptCtx & oc)
{
    InsertCvtCtx rc(&oc);
    return perform(oc, rc);
}
//END InsertCvt

} //namespace xoc
