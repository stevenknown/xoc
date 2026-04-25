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
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

IRMgrExt::IRMgrExt(Region * rg) : IRMgr(rg)
{
    ASSERT0(m_rg->getPassMgr());
}


IR * IRMgrExt::buildSelectStoreStmtViaIsomoIR(
    IR const* reflhs, IR * op, IR * base, Type const* ty)
{
    ASSERT0(reflhs->is_exp() || reflhs->is_stmt());
    IR * isomo_stmt = getRegion()->dupIsomoStmtExceptRHS(reflhs);
    ASSERT0(isomo_stmt->is_stmt());
    IR * masked_sel = buildSelectToRes(op, base, isomo_stmt->getType());
    ASSERT0(isomo_stmt->hasRHS());
    isomo_stmt->setRHS(masked_sel);
    return isomo_stmt;
}


IR * IRMgrExt::buildSelectToRes(IR * op, IR * base, Type const* ty)
{
    ASSERT0(op && base && ty);
    ASSERT0(base->is_exp() && op->is_exp());
    IR * ir = allocIR(IR_SELECT_TO_RES);
    SELECTTORES_op(ir) = op;
    SELECTTORES_base(ir) = base;
    IR_parent(op) = ir;
    IR_parent(base) = ir;
    IR_dt(ir) = ty;
    return ir;
}


IR * IRMgrExt::buildMaskOp(IR * op, IR * mask,
    CMaskOp::MASK_STRATEGY mask_strategy, Type const* ty)
{
    ASSERT0(op && mask && ty);
    ASSERT0(mask->is_exp() && op->is_exp());
    IR * ir = allocIR(IR_MASK_OP);
    MASKOP_mask_strategy(ir) = mask_strategy;
    MASKOP_op(ir) = op;
    MASKOP_mask(ir) = mask;
    IR_parent(op) = ir;
    IR_parent(mask) = ir;
    IR_dt(ir) = ty;
    return ir;
}


IR * IRMgrExt::buildDynLenOp(IR * op, IR * len,
    CDynLenOp::TAIL_STRATEGY tail_strategy, Type const* ty)
{
    ASSERT0(op && len && ty);
    ASSERT0(len->is_exp() && op->is_exp());
    IR * ir = allocIR(IR_DYNLEN_OP);
    DYNLENOP_tail_strategy(ir) = tail_strategy;
    DYNLENOP_op(ir) = op;
    DYNLENOP_len(ir) = len;
    IR_parent(op) = ir;
    IR_parent(len) = ir;
    IR_dt(ir) = ty;
    return ir;
}


IR * IRMgrExt::buildVIStore(
    IR * base, TMWORD ofst, IR * rhs, IR * dummyuse, Type const* ty)
{
    ASSERT0(ty && base);
    ASSERT0(base->is_exp());
    IR * ir = allocIR(IR_VIST);
    VIST_base(ir) = base;
    VIST_ofst(ir) = ofst;
    VIST_rhs(ir) = rhs;
    IR_dt(ir) = ty;
    IR_parent(base) = ir;
    if (rhs != nullptr) {
        ASSERT0(rhs->is_single());
        IR_parent(rhs) = ir;
    }
    VIST_dummyuse(ir) = dummyuse;
    if (dummyuse != nullptr) {
        ASSERT0(dummyuse->is_dummyuse());
        ASSERT0(dummyuse->is_single());
        IR_parent(dummyuse) = ir;
    }
    return ir;
}


IR * IRMgrExt::buildVStore(
    Var * lhs, TMWORD ofst, IR * rhs, IR * dummyuse, Type const* ty)
{
    ASSERT0(ty && lhs);
    IR * ir = allocIR(IR_VST);
    VST_idinfo(ir) = lhs;
    VST_ofst(ir) = ofst;
    VST_rhs(ir) = rhs;
    IR_dt(ir) = ty;
    if (rhs != nullptr) {
        ASSERT0(rhs->is_single());
        IR_parent(rhs) = ir;
    }
    VST_dummyuse(ir) = dummyuse;
    if (dummyuse != nullptr) {
        ASSERT0(dummyuse->is_dummyuse());
        ASSERT0(dummyuse->is_single());
        IR_parent(dummyuse) = ir;
    }
    return ir;
}


IR * IRMgrExt::buildVStorePR(
    PRNO resprno, IR * rhs, IR * dummyuse, Type const* ty)
{
    ASSERT0(resprno != PRNO_UNDEF);
    IR * ir = allocIR(IR_VSTPR);
    VSTPR_no(ir) = resprno;
    IR_dt(ir) = ty;
    VSTPR_rhs(ir) = rhs;
    if (rhs != nullptr) {
        ASSERT0(rhs->is_single());
        IR_parent(VSTPR_rhs(ir)) = ir;
    }
    VSTPR_dummyuse(ir) = dummyuse;
    if (dummyuse != nullptr) {
        ASSERT0(dummyuse->is_dummyuse());
        ASSERT0(dummyuse->is_single());
        IR_parent(dummyuse) = ir;
    }
    return ir;
}


IR * IRMgrExt::buildBroadCast(IR * src, IR * res_list, Type const* ty)
{
    ASSERT0(src && res_list);
    IR * ir = allocIR(IR_BROADCAST);
    BROADCAST_src(ir) = src;
    IR_parent(src) = ir;
    BROADCAST_res_list(ir) = res_list;
    for (IR * res = res_list; res != nullptr; res = res->get_next()) {
        IR_parent(res) = ir;
    }
    IR_dt(ir) = ty;
    return ir;
}


bool IRMgrExt::isMoveOp(IR const* ir) const
{
    ASSERT0(ir);
    if (IRMgr::isMoveOp(ir)) { return true; }
    return isRegardAsMoveOp(ir);
}


bool IRMgrExt::isRegardAsMoveOp(IR const* ir) const
{
    ASSERT0(ir);

    //TODO:
    //This class can be extended in the future to recognize additional
    //move-like operations. For example,
    //c = a + 0. would also be treated as a move.
    return false;
}


bool IRMgrExt::isMaskResult(IR const* ir)
{
    if (ir->hasResult() && ir->hasRHS() &&
        ir->getRHS()->is_select_to_res()) {
        return true;
    }
    return false;
}


bool IRMgrExt::hasMultiRes(IR * stmt) const
{
    switch (stmt->getCode()) {
    case IR_ATOMCAS:
    case IR_ATOMINC:
    case IR_BROADCAST: return true;
    default:;
    }
    return false;
}


IR * IRMgrExt::getAlterResDescList(IR * stmt) const
{
    switch (stmt->getCode()) {
    case IR_BROADCAST:
        return BROADCAST_res_list(stmt);
    case IR_ATOMCAS:
        return ATOMCAS_multires(stmt);
    case IR_ATOMINC:
        return ATOMINC_multires(stmt);
    default: UNREACHABLE();
    }
    return nullptr;
}


IR * IRMgrExt::buildAtomCas(
    Type const* type, IR * memory, IR * oldval, IR * newval, IR * reslst)
{
    ASSERT0(memory && newval && oldval && reslst);
    ASSERT0(type && (type->is_i32() || type->is_i64()));
    IR * ir = allocIR(IR_ATOMCAS);
    IR_is_atomic(ir) = true;
    ATOMCAS_memory(ir) = memory;
    ATOMCAS_newval(ir) = newval;
    ATOMCAS_oldval(ir) = oldval;
    ATOMCAS_multires(ir) = reslst;
    IR_parent(memory) = ir;
    IR_parent(newval) = ir;
    IR_parent(oldval) = ir;
    for (IR * res = reslst; res != nullptr; res = res->get_next()) {
        IR_parent(res) = ir;
    }
    IR_dt(ir) = type;
    return ir;
}


IR * IRMgrExt::buildAtomInc(
    Type const* type, IR * memory, IR * reslst, IR * addend)
{
    ASSERT0(type && memory && reslst);
    IR * ir = allocIR(IR_ATOMINC);
    IR_is_atomic(ir) = true;
    ATOMINC_memory(ir) = memory;
    ATOMINC_multires(ir) = reslst;
    IR_parent(memory) = ir;
    for (IR * res = reslst; res != nullptr; res = res->get_next()) {
        IR_parent(res) = ir;
    }

    //Some architectures require explicit specification, some do not.
    if (addend != nullptr) {
        ATOMINC_addend(ir) = addend;
        IR_parent(addend) = ir;
    }
    IR_dt(ir) = type;
    return ir;
}

#ifdef REF_TARGMACH_INFO

IR * IRMgrExt::buildPhyReg(xgen::Reg reg, RegPhi * regphi)
{
    ASSERT0(regphi && reg != REG_UNDEF);
    IR * ir = allocIR(IR_PHYREG);
    PHYREG_phi(ir) = regphi;
    PHYREG_reg(ir) = reg;
    IR_dt(ir) = m_tm->getTargMachRegisterType();
    return ir;
}

#endif


IR const* IRMgrExt::getFullSizeOp(IR const* ir) const
{
    ASSERT0(ir && ir->is_vec());
    IR const* op = ir->hasRHS() ? ir->getRHS() : ir;
    IR_CODE irc = op->getCode();

    switch (irc) {
    //If it is a full-size operation, return directly.
    //If there are new full-size operations, please add them here.
    case IR_PR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_DIV: return op;

    //If there are other IR operations that are not
    //full-size, please add this type here.
    case IR_MASK_OP: return getFullSizeOp(MASKOP_op(op));
    case IR_DYNLEN_OP: return getFullSizeOp(DYNLENOP_op(op));
    default: ASSERTN(0, ("Unsupported IR type, please implement it"));
        return nullptr;
    }
}

} //namespace xoc
