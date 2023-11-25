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

static bool needBuildDUChain(RefineCtx const& ctx)
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


static HOST_INT calcIntVal(IR_CODE ty, HOST_INT v0, HOST_INT v1)
{
    switch (ty) {
    case IR_ADD:
        v1 = v0 + v1;
        break;
    case IR_SUB:
        v1 = v0 - v1;
        break;
    case IR_MUL:
        v1 = v0 * v1;
        break;
    case IR_DIV:
        v1 = v0 / v1;
        break;
    case IR_REM:
        v1 = v0 % v1;
        break;
    case IR_MOD:
        v1 = v0 % v1;
        break;
    case IR_LAND:
        v1 = v0 && v1;
        break;
    case IR_LOR:
        v1 = v0 || v1;
        break;
    case IR_BAND:
        v1 = v0 & v1;
        break;
    case IR_BOR:
        v1 = v0 | v1;
        break;
    case IR_XOR:
        v1 = v0 ^ v1;
        break;
    case IR_BNOT:
        v1 = ~v0;
        break;
    case IR_LNOT:
        v1 = !v0;
        break;
    case IR_LT:
        v1 = v0 < v1;
        break;
    case IR_LE:
        v1 = v0 <= v1;
        break;
    case IR_GT:
        v1 = v0 > v1;
        break;
    case IR_GE:
        v1 = v0 >= v1;
        break;
    case IR_EQ:
        v1 = v0 == v1;
        break;
    case IR_NE:
        v1 = v0 != v1;
        break;
    case IR_ASR:
        v1 = v0 >> v1;
        break;
    case IR_LSR:
        ASSERTN(0, ("the case must be handled in calcLSRIntVal()"));
        //v1 = ((HOST_UINT)v0) >> v1;
        break;
    case IR_LSL:
        v1 = v0 << v1;
        break;
    default: UNREACHABLE();
    }
    return v1;
}


//Make sure v0 is sign-extended if its bits length less than HOST_INT.
static HOST_INT calcLSRIntVal(Type const* type, HOST_INT v0, HOST_INT v1)
{
    HOST_INT res = 0;
    switch (TY_dtype(type)) {
    case D_B:
    case D_I8:
        res = (HOST_INT) (((INT8)(UINT8)v0) >> v1);
        break;
    case D_U8:
        res = (HOST_INT) (HOST_UINT) (((UINT8)v0) >> v1);
        break;
    case D_I16:
        res = (HOST_INT) (((INT16)(UINT16)v0) >> v1);
        break;
    case D_U16:
        res = (HOST_INT) (HOST_UINT) (((UINT16)v0) >> v1);
        break;
    case D_I32:
        res = (HOST_INT) (((INT32)(UINT32)v0) >> v1);
        break;
    case D_U32:
        res = (HOST_INT) (HOST_UINT) (((UINT32)v0) >> v1);
        break;
    case D_I64:
        res = (HOST_INT) (((INT64)(UINT64)v0) >> v1);
        break;
    case D_U64:
        res = (HOST_INT) (HOST_UINT) (((UINT64)v0) >> v1);
        break;
    case D_I128:
        #ifdef INT128
        res = (HOST_INT) (((INT128)(UINT128)v0) >> v1);
        break;
        #endif
    case D_U128:
        #ifdef UINT128
        res = (HOST_INT) (HOST_UINT) (((UINT128)v0) >> v1);
        break;
        #endif
    default: ASSERTN(0, ("Need to support"));
    }
    return res;
}


template <class T>
static bool calcBoolVal(IR_CODE ty, T v0, T v1)
{
    bool res = false;
    switch (ty) {
    case IR_LT:
        res = v0 < v1;
        break;
    case IR_LE:
        res = v0 <= v1;
        break;
    case IR_GT:
        res = v0 > v1;
        break;
    case IR_GE:
        res = v0 >= v1;
        break;
    case IR_EQ:
        res = v0 == v1;
        break;
    case IR_NE:
        res = v0 != v1;
        break;
    default:
        UNREACHABLE();
    }
    return res;
}


static double calcFloatVal(IR_CODE ty, double v0, double v1)
{
    switch (ty) {
    case IR_ADD:
        v1 = v0 + v1;
        break;
    case IR_SUB:
        v1 = v0 - v1;
        break;
    case IR_MUL:
        v1 = v0 * v1;
        break;
    case IR_DIV:
        v1 = v0 / v1;
        break;
    case IR_LNOT:
        v1 = !v0;
        break;
    case IR_LT:
        v1 = v0 < v1;
        break;
    case IR_LE:
        v1 = v0 <= v1;
        break;
    case IR_GT:
        v1 = v0 > v1;
        break;
    case IR_GE:
        v1 = v0 >= v1;
        break;
    case IR_EQ:
        v1 = v0 == v1;
        break;
    case IR_NE:
        v1 = v0 != v1;
        break;
    default:
        ;
    }
    return v1;
}


//
//START Refine
//
Refine::Refine(Region * rg) : Pass(rg)
{
    ASSERT0(rg != nullptr);
    m_tm = rg->getTypeMgr();
}


//Algebraic identities.
IR * Refine::refineILoad1(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_ild());
    //Convert
    //    ILD,ofst
    //     LDA
    //=>
    //    LD,ofst
    //e.g: (&q)->s => q.s
    IR * base = ILD_base(ir);
    ASSERTN(base->is_lda() && LDA_ofst(base) == 0, ("not the case"));

    //ILD offset may not be 0.
    TMWORD ild_ofst = ILD_ofst(ir);

    IR * ld = m_rg->getIRMgr()->buildLoad(LDA_idinfo(base), ir->getType());
    copyDbx(ld, base, m_rg);

    LD_ofst(ld) += ild_ofst;

    ld->copyRef(ir, m_rg);
    //Consider the ir->getOffset() and copying MDSet info from 'ir' to 'ld.
    m_rg->getMDMgr()->allocRef(ld);
    recomputeMayRef(ld);
    //The new MustRef may be not overlapped with the MayRef.
    ld->copyAI(ir, m_rg);

    if (rc.maintainDU()) {
        //Note: the recomputation of MustRef may generate new MD that not
        //versioned by MDSSAMgr. Use MDSSA API to fix the SSA information.
        xoc::changeUse(ir, ld, m_rg);
        xoc::removeExpiredDU(ld, m_rg);
    }
    m_rg->freeIRTree(ir);
    change = true;
    //No need to set parent.
    return ld;
}


IR * Refine::refineILoad2(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_ild() && ILD_base(ir)->is_lda() &&
            LDA_ofst(ILD_base(ir)) != 0);
    //Convert
    //    ILD,ofst1
    //     LDA,ofst2
    //=>
    //    LD,ofst1+ofst2
    IR * base = ILD_base(ir);
    IR * ld = m_rg->getIRMgr()->buildLoad(LDA_idinfo(base), ir->getType());
    LD_ofst(ld) = LDA_ofst(base) + ILD_ofst(ir);
    copyDbx(ld, ir, m_rg);
    ld->copyRef(ir, m_rg);
    m_rg->getMDMgr()->allocRef(ld);
    recomputeMayRef(ld);

    //The new MustRef may be not overlapped with the MayRef.
    //ASSERT0_DUMMYUSE(checkMDSetContain(ld, ld->getMustRef()));
    ld->copyAI(ir, m_rg);

    if (rc.maintainDU()) {
        //Note: the recomputation of MustRef may generate new MD that not
        //versioned by MDSSAMgr. Use MDSSA API to fix the SSA information.
        xoc::changeUse(ir, ld, m_rg);
        xoc::removeExpiredDU(ld, m_rg);
    }
    m_rg->freeIRTree(ir);
    change = true;
    //No need to set parent pointer.
    return ld;
}


IR * Refine::refineILoad3(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_ild() && ILD_base(ir)->is_add());
    ASSERT0(BIN_opnd1(ILD_base(ir))->is_int() &&
            BIN_opnd1(ILD_base(ir))->is_const());
    //Convert
    //---------------------
    //  ILD
    //    ADD
    //      AnyKid
    //      IMM 4
    //=>
    //  ILD, 4
    //    AnyKid
    //---------------------
    //  ILD, ofst2
    //    ADD
    //      AnyKid
    //      IMM 4
    //=>
    //  ILD, ofst2 + 4
    //    AnyKid
    IR * add = ILD_base(ir);
    HOST_INT imm = ((CConst*)BIN_opnd1(add))->getInt();
    if (imm < 0) { return ir; }

    ir->setOffset(ir->getOffset() + (UINT)imm);

    IR * anykid = BIN_opnd0(add);
    BIN_opnd0(add) = nullptr;
    ILD_base(ir) = anykid;
    ir->setParent(anykid);

    m_rg->freeIRTree(add);
    change = true;
    return ir;
}


IR * Refine::refineILoad(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_ild());
    ASSERT0(ir->is_single());
    IR * base = ILD_base(ir);
    if (base->is_lda() && LDA_ofst(base) == 0) {
        //Convert
        //    ILD,ofst
        //     LDA
        //=>
        //    LD,ofst
        return refineILoad1(ir, change, rc);
    }

    if (base->is_lda() && LDA_ofst(base) != 0) {
        //Convert
        //    ILD,ofst1
        //     LDA,ofst2
        //=>
        //    LD,ofst1+ofst2
        return refineILoad2(ir, change, rc);
    }

    if (base->is_add() && BIN_opnd1(base)->is_int() &&
        BIN_opnd1(base)->is_const()) {
        //Convert
        //---------------------
        //  ILD
        //    ADD
        //      AnyKid
        //      IMM 4
        //=>
        //  ILD, 4
        //    AnyKid
        //---------------------
        //  ILD, ofst2
        //    ADD
        //      AnyKid
        //      IMM 4
        //=>
        //  ILD, ofst2 + 4
        //    AnyKid
        return refineILoad3(ir, change, rc);
    }

    ILD_base(ir) = refineIR(base, change, rc);
    if (change) {
        IR_parent(ILD_base(ir)) = ir;
    }
    return ir;
}


//The function does not change MD reference of ir.
IR * Refine::refineIStore1(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_ist() && IST_base(ir)->is_add());
    ASSERT0(BIN_opnd1(IST_base(ir))->is_int() &&
            BIN_opnd1(IST_base(ir))->is_const());
    //Convert
    //---------------------
    //  IST
    //    ADD
    //      AnyKid
    //      IMM 4
    //=>
    //  IST, 4
    //    AnyKid
    //---------------------
    //  IST, ofst2
    //    ADD
    //      AnyKid
    //      IMM 4
    //=>
    //  IST, ofst2 + 4
    //    AnyKid
    IR * add = IST_base(ir);
    HOST_INT imm = ((CConst*)BIN_opnd1(add))->getInt();
    if (imm < 0) { return ir; }

    ir->setOffset(ir->getOffset() + (UINT)imm);

    IR * anykid = BIN_opnd0(add);
    BIN_opnd0(add) = nullptr;
    IST_base(ir) = anykid;
    ir->setParent(anykid);

    m_rg->freeIRTree(add);
    change = true;
    return ir;
}


//The function will attempt to recompute the MayRef for given 'ir'.
//Note the computation require that DUMgr has been ready.
void Refine::recomputeMayRef(IR * ir)
{
    if (m_rg->getDUMgr() != nullptr) {
        m_rg->getDUMgr()->computeOverlapMDSet(ir, true);
    }
}


IR * Refine::refineIStore(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->isIndirectMemOp());
    bool t = false;
    bool lchange = false;
    IST_base(ir) = refineIR(IST_base(ir), t, rc);
    if (t) { ir->setParent(IST_base(ir)); }
    lchange |= t;

    bool t2 = false;
    IST_rhs(ir) = refineIR(IST_rhs(ir), t2, rc);
    if (t2) { ir->setParent(IST_rhs(ir)); }
    lchange |= t2;

    IR * base = IST_base(ir);
    IR * rhs = IST_rhs(ir);

    if (base->is_add() && BIN_opnd1(base)->is_int() &&
        BIN_opnd1(base)->is_const()) {
        //Convert
        //---------------------
        //  IST
        //    ADD
        //      AnyKid
        //      IMM 4
        //=>
        //  IST, 4
        //    AnyKid
        //---------------------
        //  IST, ofst2
        //    ADD
        //      AnyKid
        //      IMM 4
        //=>
        //  IST, ofst2 + 4
        //    AnyKid
        IR * newir = refineIStore1(ir, change, rc);
        ASSERTN_DUMMYUSE(newir == ir, ("stmt ir should not be changed"));
        lchange |= change;
    }

    if (base->is_lda()) {
        //Convert :
        //1. IST(LDA(var))=X to ST(var)=X
        //2. IST(LDA(var), ofst)=X to ST(var, ofst)=X
        //3. IST(LDA(var,ofst))=X to ST(var, ofst)=X
        //4. IST(LDA(var,ofst1), ofst2)=X to ST(var, ofst1+ofst2)=X
        IR * newir = m_rg->getIRMgr()->buildStore(LDA_idinfo(base),
            ir->getType(), LDA_ofst(base) + IST_ofst(ir), IST_rhs(ir));
        newir->copyRef(ir, m_rg);
        //Direct memory opertion must have MustRef, however ir's MustRef
        //may be empty.
        m_rg->getMDMgr()->allocRef(newir);
        recomputeMayRef(ir);
        newir->copyAI(ir, m_rg);
        if (rc.maintainDU()) {
            //Change IST to ST may result the DU chain invalid.
            //There may be USEs that would not reference the MD that ST
            //modified.
            //e.g: p = &a; p = &b;
            //IST(p, 10), IST may be defined a, b;
            //After change to ST(a, 10), ST only define a, and will not
            //define b any more.
            xoc::changeDef(ir, newir, m_rg);
            xoc::removeExpiredDU(newir, m_rg);
        }
        IST_rhs(ir) = nullptr;
        m_rg->freeIRTree(ir);
        ir = newir;
        lchange = true; //Keep the result type of ST unchanged.
        rhs = ST_rhs(ir); //No need to update DU.
        //RC_stmt_removed(rc) = true;
    }

    rhs = ir->getRHS();
    if (rhs->is_ild() && ILD_base(rhs)->is_lda()) {
        //ILD(LDA(var)) => LD(var)
        IR * newrhs = m_rg->getIRMgr()->buildLoad(LDA_idinfo(ILD_base(rhs)),
                                      rhs->getType());
        ir->setRHS(newrhs);
        copyDbx(newrhs, rhs, m_rg);
        newrhs->copyRef(rhs, m_rg);
        //Direct memory opertion must have MustRef, however ir's MustRef
        //may be empty.
        m_rg->getMDMgr()->allocRef(newrhs);
        recomputeMayRef(newrhs);
        //The new MustRef may be not overlapped with the MayRef.
        //ASSERT0_DUMMYUSE(checkMDSetContain(newrhs, newrhs->getMustRef()));
        newrhs->copyAI(rhs, m_rg);

        if (rc.maintainDU()) {
            //Note: the recomputation of MustRef may generate new MD that not
            //versioned by MDSSAMgr. Use MDSSA API to fix the SSA information.
            xoc::changeUse(rhs, newrhs, m_rg);
            xoc::removeExpiredDU(newrhs, m_rg);
        }
        ASSERTN(rhs->is_single(), ("expression cannot be linked to chain"));
        m_rg->freeIRTree(rhs);
        lchange = true;
        rhs = newrhs;
    }
    if (lchange) {
        ir->setParentPointer(false);
    }
    change |= lchange;
    return ir;
}


#ifdef _DEBUG_
//Return true if CVT is redundant.
static inline bool is_redundant_cvt(IR * ir)
{
    if (ir->is_cvt()) {
        if (CVT_exp(ir)->is_cvt() ||
            CVT_exp(ir)->getType() == ir->getType()) {
            return true;
        }
    }
    return false;
}
#endif


IR * Refine::refineDirectStore(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0((ir->isDirectMemOp() || ir->is_stpr()) && ir->hasRHS());
    if (ir->isDummyOp()) { return ir; }
    bool lchange = false;
    IR * rhs = ir->getRHS();
    if (rhs == nullptr) {
        //VirtualOp may not have RHS.
        return ir;
    }
    if (RC_refine_stmt(rc) && rhs->is_pr() && ir->is_stpr() &&
        PR_no(rhs) == STPR_no(ir)) {
        //Remove pr1 = pr1.
        if (rc.maintainDU() && needBuildDUChain(rc)) {
            xoc::coalesceDUChain(ir, rhs, m_rg);
            xoc::removeStmt(ir, m_rg, *rc.getOptCtx());
        }
        IRBB * bb = ir->getBB();
        if (bb != nullptr) {
            BB_irlist(bb).remove(ir);
            RC_stmt_removed(rc) = true;
        }
        m_rg->freeIRTree(ir);
        change = true;
        return nullptr;
    }

    rhs = refineIR(rhs, lchange, rc);
    ir->setRHS(rhs);
    ASSERT0(!::is_redundant_cvt(rhs));
    if (RC_refine_stmt(rc)) {
        MD const* umd = rhs->getExactRef();
        if (umd != nullptr && umd == ir->getExactRef()) {
            //Result and operand referenced the same md.
            //CASE: st(x) = ld(x);
            if (rhs->is_cvt()) {
                //CASE: pr(i64) = cvt(i64, pr(i32))
                //Do NOT remove 'cvt'.
                ;
            } else {
                change = true;
                if (rc.maintainDU() && needBuildDUChain(rc)) {
                    xoc::coalesceDUChain(ir, rhs, m_rg);
                    xoc::removeStmt(ir, m_rg, *rc.getOptCtx());
                }
                IRBB * bb = ir->getBB();
                if (bb != nullptr) {
                    BB_irlist(bb).remove(ir);
                    RC_stmt_removed(rc) = true;
                }
                m_rg->freeIRTree(ir);
                return nullptr;
            }
        }
    }
    change |= lchange;
    if (lchange) {
        ir->setParentPointer(false);
    }
    return ir;
}


IR * Refine::refineSetelem(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_setelem());
    IR * base = refineIR(SETELEM_base(ir), change, rc);
    if (base != SETELEM_base(ir)) {
        ir->setParent(base);
        SETELEM_base(ir) = base;
    }
    IR * val = refineIR(SETELEM_val(ir), change, rc);
    if (val != SETELEM_val(ir)) {
        ir->setParent(val);
        SETELEM_val(ir) = val;
    }
    IR * ofst = refineIR(SETELEM_ofst(ir), change, rc);
    if (ofst != SETELEM_ofst(ir)) {
        ir->setParent(ofst);
        SETELEM_ofst(ir) = ofst;
    }
    return ir;
}


IR * Refine::refineGetelem(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_getelem());
    IR * base = refineIR(GETELEM_base(ir), change, rc);
    if (base != GETELEM_base(ir)) {
        ir->setParent(base);
        GETELEM_base(ir) = base;
    }
    IR * ofst = refineIR(GETELEM_ofst(ir), change, rc);
    if (ofst != GETELEM_ofst(ir)) {
        ir->setParent(ofst);
        GETELEM_ofst(ir) = ofst;
    }
    return ir;
}


IR * Refine::refineCall(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->isCallStmt());
    bool lchange = false;
    if (CALL_param_list(ir) != nullptr) {
        IR * param = xcom::removehead(&CALL_param_list(ir));
        IR * newparamlst = nullptr;
        IR * last = nullptr;
        while (param != nullptr) {
            IR * newp = refineIR(param, lchange, rc);
            xcom::add_next(&newparamlst, &last, newp);
            last = newp;
            param = xcom::removehead(&CALL_param_list(ir));
        }
        CALL_param_list(ir) = newparamlst;
    }

    if (lchange) {
        change = true;
        ir->setParentPointer(false);
    }
    return ir;
}


IR * Refine::refineICall(IR * ir, bool & change, RefineCtx & rc)
{
    refineCall(ir, change, rc);
    return ir;
}


IR * Refine::refineSwitch(IR * ir, bool & change, RefineCtx & rc)
{
    bool l = false;
    SWITCH_vexp(ir) = refineIR(SWITCH_vexp(ir), l, rc);
    if (l) {
        IR_parent(SWITCH_vexp(ir)) = ir;
        change = true;
    }

    l = false;
    SWITCH_body(ir) = refineIRlist(SWITCH_body(ir), l, rc);
    change |= l;
    return ir;
}


IR * Refine::refineBr(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->isConditionalBr());
    bool l = false;
    BR_det(ir) = refineDet(BR_det(ir), l, rc);
    ir = refineBranch(ir);
    if (l) {
        change = true;
    }
    return ir;
}


IR * Refine::refineReturn(IR * ir, bool & change, RefineCtx & rc)
{
    if (RET_exp(ir) == nullptr) { return ir; }

    bool lchange = false;
    RET_exp(ir) = refineIR(RET_exp(ir), lchange, rc);

    if (lchange) {
        change = true;
        ir->setParentPointer(false);
    }
    return ir;
}


//Prefer usety which is the type in USE point. Use opndty if usety is ANY.
static Type const* evalImmType(Type const* usety, Type const* opndty)
{
    if (!usety->is_any()) { return usety; }
    return opndty;
}


//IR already has built ssa info.
IR * Refine::refinePhi(IR * ir, bool & change, RefineCtx & rc)
{
    //phi(1, 1, ...) => 1
    bool all_be_same_const = true;
    IR * immopnd = nullptr;
    IR * opnd = PHI_opnd_list(ir);
    HOST_INT val = 0;
    if (opnd->is_const() && opnd->is_int()) {
        immopnd = opnd;
        val = CONST_int_val(opnd);
        for (opnd = opnd->get_next(); opnd != nullptr;
             opnd = opnd->get_next()) {
            if (opnd->is_const() && opnd->is_int() &&
                val == CONST_int_val(opnd)) {
                continue;
            }
            all_be_same_const = false;
            break;
        }
    } else {
        all_be_same_const = false;
    }

    if (!all_be_same_const) { return ir; }

    SSAInfo * ssainfo = PHI_ssainfo(ir);
    ASSERT0(ssainfo);

    SSAUseIter sc;
    for (BSIdx u = SSA_uses(ssainfo).get_first(&sc);
         u != BS_UNDEF; u = SSA_uses(ssainfo).get_next(u, &sc)) {
        IR * use = m_rg->getIR(u);
        ASSERT0(use && use->is_pr());
        IR * lit = m_rg->getIRMgr()->buildImmInt(val,
            evalImmType(use->getType(), immopnd->getType()));
        ASSERT0(IR_parent(use));
        IR_parent(use)->replaceKid(use, lit,  false);
        m_rg->freeIR(use);
    }

    ssainfo->cleanDU();

    change = true;

    if (RC_refine_stmt(rc)) {
        IRBB * bb = ir->getBB();
        ASSERT0(bb);
        BB_irlist(bb).remove(ir);
        RC_stmt_removed(rc) = true;
        m_rg->freeIRTree(ir);
    }
    return nullptr;
}


//Transform ir to IR_LNOT.
//Return the transformed ir if changed, or the original.
//Note m_rg function will not free ir, since it is the caller's responsibility.
//
//CASE1:
//    st:i32 $6
//    cvt:i32
//        select:i8
//            ne:bool
//                $6:i32
//                intconst:i32 0
//            intconst:i8 0 true_exp
//            intconst:i8 1 false_exp
//to
//    st:i32 $6
//    cvt:i32
//        lnot:i8
//           $6:i32
//
//Other analogous cases:
//   b=(a==0?1:0) => b=!a
static inline IR * hoistSelectToLnot(IR * ir, Region * rg)
{
    ASSERT0(ir->is_select());
    IR * det = SELECT_det(ir);
    if (det->is_ne()) {
        IR * trueexp = SELECT_trueexp(ir);
        IR * falseexp = SELECT_falseexp(ir);
        if (BIN_opnd1(det)->isConstIntValueEqualTo(0) &&
            trueexp->isConstIntValueEqualTo(0) &&
            falseexp->isConstIntValueEqualTo(1)) {
            IR * lnot = rg->getIRMgr()->buildUnaryOp(
                           IR_LNOT,
                           rg->getTypeMgr()->getBool(),
                           BIN_opnd0(det));
            BIN_opnd0(det) = nullptr;
            return lnot;
        }
    }

    if (det->is_eq()) {
        IR * trueexp = SELECT_trueexp(ir);
        IR * falseexp = SELECT_falseexp(ir);
        if (BIN_opnd1(det)->isConstIntValueEqualTo(0) &&
            trueexp->isConstIntValueEqualTo(1) &&
            falseexp->isConstIntValueEqualTo(0)) {
            IR * lnot = rg->getIRMgr()->buildLogicalNot(BIN_opnd0(det));
            BIN_opnd0(det) = nullptr;
            return lnot;
        }
    }

    return ir;
}


IR * Refine::refineSelect(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_select());
    SELECT_det(ir) = refineDet(SELECT_det(ir), change, rc);
    SELECT_trueexp(ir) = refineIRlist(SELECT_trueexp(ir), change, rc);
    SELECT_falseexp(ir) = refineIRlist(SELECT_falseexp(ir), change, rc);
    IR * det = foldConst(SELECT_det(ir), change, rc);
    if (det != SELECT_det(ir)) {
        SELECT_det(ir) = det;
        ir->setParent(det);
    }

    IR * gen = nullptr;
    if (det->is_const() && det->is_int()) {
        HOST_INT v = CONST_int_val(det);
        if (v == 0) {
            // select(0) ? a : b => b
            IR * keep = SELECT_falseexp(ir);
            SELECT_falseexp(ir) = nullptr;
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = keep;
            ASSERT0(ir->is_exp());
            change = true;
        } else {
            // select(1) ? a : b => a
            IR * keep = SELECT_trueexp(ir);
            SELECT_trueexp(ir) = nullptr;
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = keep;
            ASSERT0(ir->is_exp());
            change = true;
        }
    } else if (det->is_const() && det->is_fp()) {
        double v = CONST_fp_val(det);
        if (v < HOST_FP(EPSILON)) { //means v == 0.0
            // select(0) ? a : b => b
            IR * keep = SELECT_falseexp(ir);
            SELECT_falseexp(ir) = nullptr;
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = keep;
            ASSERT0(ir->is_exp());
            change = true;
        } else {
            // select(1) ? a : b => a
            IR * keep = SELECT_trueexp(ir);
            SELECT_trueexp(ir) = nullptr;
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = keep;
            ASSERT0(ir->is_exp());
            change = true;
        }
    } else if (det->is_str()) {
        // select(1) ? a : b => a
        IR * keep = SELECT_trueexp(ir);
        SELECT_trueexp(ir) = nullptr;
        if (rc.maintainDU()) {
            xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
        }
        m_rg->freeIRTree(ir);
        ir = keep;
        ASSERT0(ir->is_exp());
        change = true;
    } else if (RC_hoist_to_lnot(rc) &&
               (gen = hoistSelectToLnot(ir, m_rg)) != ir) {
        if (rc.maintainDU()) {
            xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
        }
        m_rg->freeIRTree(ir);
        ir = gen;
        change = true;
    }

    if (change) {
        if (ir->is_select() && !SELECT_det(ir)->is_judge()) {
            SELECT_det(ir) = m_rg->getIRMgr()->buildJudge(SELECT_det(ir));
            ir->setParent(SELECT_det(ir));
        } else {
            ir->setParentPointer(false);
        }
    }
    return ir; //No need to update DU.
}


IR * Refine::refineNeg(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_neg());
    bool lchange = false;
    ir = foldConst(ir, lchange, rc);
    change |= lchange;
    if (!lchange && UNA_opnd(ir)->is_neg()) {
        //-(-x) => x
        IR * tmp = UNA_opnd(UNA_opnd(ir));
        UNA_opnd(UNA_opnd(ir)) = nullptr;
        m_rg->freeIRTree(ir);
        change = true;
        return tmp;
    }
    return ir;
}


//Logic not: !(0001) = 0000
//Bitwise not: !(0001) = 1110
IR * Refine::refineNot(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_lnot() || ir->is_bnot());
    UNA_opnd(ir) = refineIR(UNA_opnd(ir), change, rc);
    if (change) {
        IR_parent(UNA_opnd(ir)) = ir;
    }

    if (ir->is_lnot()) {
        IR * op0 = UNA_opnd(ir);
        bool lchange = false;
        switch (op0->getCode()) {
        case IR_LT:
        case IR_LE:
        case IR_GT:
        case IR_GE:
        case IR_EQ:
        case IR_NE:
            op0 = IR::invertIRCode(op0, m_rg);
            lchange = true;
            break;
        default: break;
        }
        if (lchange) {
            UNA_opnd(ir) = nullptr;
            m_rg->freeIRTree(ir);
            change = true;
            ir = op0;
        }
    }

    ir = foldConst(ir, change, rc);
    return ir;
}


//If the value of opnd0 is not a multiple of opnd1,
//((opnd0 div opnd1) mul opnd1) may not equal to opnd0.
IR * Refine::refineDiv(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_div());
    IR * op1 = BIN_opnd1(ir);
    IR * op0 = BIN_opnd0(ir);
    if (g_is_opt_float && op1->is_const() && op1->is_fp() && !op0->is_const()) {
        HOST_FP fp_imm = CONST_fp_val(op1);
        if (fp_imm == 1.0) {
            //X/1.0 => X
            IR * newIR = op0;
            BIN_opnd0(ir) = nullptr;
            m_rg->freeIRTree(ir);
            ir = newIR;
            change = true;
            return ir;
        }
        if (fp_imm == 0) {
            //X/0
            return ir;
        }
        if (xcom::isPowerOf2(::abs((INT)(fp_imm))) ||
            xcom::isPowerOf5(fp_imm)) {
            //X/n => X*(1.0/n)
            IR_code(ir) = IR_MUL;
            CONST_fp_val(op1) = ((HOST_FP)1.0) / fp_imm;
            change = true;
            return ir;
        }
        return ir;
    }
    if (op1->is_const() && op1->is_int() &&
        xcom::isPowerOf2(CONST_int_val(op1)) &&
        rc.refine_div_const()) {
        //X/2^power => X>>power, arith shift right to floor-division.
        if (op0->is_sint()) {
            IR_code(ir) = IR_ASR;
        } else if (op0->is_uint()) {
            IR_code(ir) = IR_LSR;
        } else {
            //Only handle integer.
            return ir;
        }
        CONST_int_val(op1) = xcom::getPowerOf2(CONST_int_val(op1));
        change = true;
        return ir; //No need to update DU.
    }
    if (op0->isIREqual(op1, true)) {
        //X/X => 1.
        IR * tmp = ir;
        Type const* ty;
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = m_tm->getSimplexTypeEx(D_U32);
        } else {
            ty = op0->getType();
        }

        if (ty->is_fp()) {
            ir = m_rg->getIRMgr()->buildImmFP(1.0f, ty);
        } else {
            ir = m_rg->getIRMgr()->buildImmInt(1, ty);
        }

        if (rc.maintainDU()) {
            //Cut du chain for opnd0, opnd1 and their def-stmt.
            xoc::removeUseForTree(tmp, m_rg, *rc.getOptCtx());
        }
        copyDbx(ir, tmp, m_rg);
        m_rg->freeIRTree(tmp);
        change = true;
        return ir;
    }
    if (op0->is_mul()) {
        //(x * y) / y => x
        IR * op0_of_op0 = BIN_opnd0(op0);
        IR * op1_of_op0 = BIN_opnd1(op0);
        if (op1_of_op0->isIREqual(op1, true)) {
            BIN_opnd0(op0) = nullptr;
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = op0_of_op0;
            change = true;
        }
        return ir;
    }

    return ir;
}


IR * Refine::refineMod(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_mod());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0_DUMMYUSE(op0);
    ASSERT0_DUMMYUSE(op1);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 1) {
        //mod X,1 => 0
        IR * tmp = ir;
        ir = m_rg->dupIRTree(op1);
        CONST_int_val(ir) = 0;
        if (rc.maintainDU()) {
            xoc::removeUseForTree(tmp, m_rg, *rc.getOptCtx());
        }
        m_rg->freeIRTree(tmp);
        change = true;
        return ir;
    }
    return ir;
}


IR * Refine::refineRem(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_rem());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0_DUMMYUSE(op0);
    ASSERT0_DUMMYUSE(op1);
    if (op1->is_const() && op1->is_int()) {
        if (CONST_int_val(op1) == 1) {
            //rem X,1 => 0
            IR * tmp = ir;
            ir = m_rg->dupIRTree(op1);
            CONST_int_val(ir) = 0;
            if (rc.maintainDU()) {
                xoc::removeUseForTree(tmp, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(tmp);
            change = true;
            return ir;
        }
        if (xcom::isPowerOf2(CONST_int_val(op1))) {
            //rem X,2^N => band X,2^N-1
            IR_code(ir) = IR_BAND;
            CONST_int_val(op1) = CONST_int_val(op1) - 1;
            change = true;
            return ir; //No need to update DU.
        }
    }
    return ir;
}


IR * Refine::refineComparisonAndLogic(IR * ir, bool & change,
                                      RefineCtx const& rc)
{
    //Logical expression equvialence substitution.
    switch (ir->getCode()) {
    case IR_LT: {
        IR * opnd1 = BIN_opnd1(ir);
        if (BIN_opnd0(ir)->is_unsigned() &&
            opnd1->is_const() &&
            opnd1->is_int() &&
            CONST_int_val(opnd1) == 0) {
            //LT(UNSIGNED, 0) always be false.
            IR * x = m_rg->getIRMgr()->buildImmInt(0, ir->getType());
            copyDbx(x, ir, m_rg);
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = x;
            change = true;
        }
        break;
    }
    case IR_GE: {
        IR * opnd1 = BIN_opnd1(ir);
        if (BIN_opnd0(ir)->is_unsigned() &&
            opnd1->is_const() &&
            opnd1->is_int() &&
            CONST_int_val(opnd1) == 0) {
            //GE(UNSIGNED, 0) always be true.
            IR * x = m_rg->getIRMgr()->buildImmInt(1, ir->getType());
            copyDbx(x, ir, m_rg);
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = x;
            change = true;
        }
        break;
    }
    case IR_NE: {
        //address of string always not be 0x0.
        //NE(LDA(Sym), 0) -> 1
        IR * opnd0 = BIN_opnd0(ir);
        IR * opnd1 = BIN_opnd1(ir);
        if ((opnd0->is_lda() &&
             LDA_idinfo(opnd0)->is_string() &&
             opnd1->is_const() && opnd1->is_int() &&
             CONST_int_val(opnd1) == 0)
            ||
            (opnd1->is_lda() &&
             LDA_idinfo(opnd1)->is_string() &&
             opnd0->is_const() &&
             opnd0->is_int() &&
             CONST_int_val(opnd0) == 0)) {
            IR * x = m_rg->getIRMgr()->buildImmInt(1, ir->getType());
            copyDbx(x, ir, m_rg);
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = x;
            change = true;
        }
        break;
    }
    case IR_EQ: {
        //address of string always not be 0x0.
        //EQ(LDA(Sym), 0) -> 0
        IR * opnd0 = BIN_opnd0(ir);
        IR * opnd1 = BIN_opnd1(ir);
        if ((opnd0->is_lda() &&
             LDA_idinfo(opnd0)->is_string() &&
             opnd1->is_const() &&
             opnd1->is_int() &&
             CONST_int_val(opnd1) == 0)
            ||
            (opnd1->is_lda() &&
             LDA_idinfo(opnd1)->is_string() &&
             opnd0->is_const() &&
             opnd0->is_int() &&
             CONST_int_val(opnd0) == 0)) {
            IR * x = m_rg->getIRMgr()->buildImmInt(0, ir->getType());
            copyDbx(x, ir, m_rg);
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = x;
            change = true;
        }
        break;
    }
    case IR_ASR: //>>
    case IR_LSR: { //>>
        IR * opnd0 = BIN_opnd0(ir);
        IR * opnd1 = BIN_opnd1(ir);
        if (opnd0->is_const() && opnd0->is_int() && CONST_int_val(opnd0) == 0) {
            IR * newir = m_rg->getIRMgr()->buildImmInt(0, ir->getType());
            copyDbx(newir, ir, m_rg);
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = newir;
            change = true;
            break;
        }
        if (opnd1->is_const() && opnd1->is_int() && CONST_int_val(opnd1) == 0) {
            IR * newir = opnd0;
            BIN_opnd0(ir) = nullptr;
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = newir;
            change = true;
        }
        break;
    }
    case IR_LSL: { //<<
        IR * opnd0 = BIN_opnd0(ir);
        IR * opnd1 = BIN_opnd1(ir);
        if (opnd0->is_const() && opnd0->is_int() && CONST_int_val(opnd0) == 0) {
            IR * newir = m_rg->getIRMgr()->buildImmInt(0, ir->getType());
            copyDbx(newir, ir, m_rg);
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = newir;
            change = true;
            break;
        }
        if (opnd1->is_const() && opnd1->is_int()) {
            if (CONST_int_val(opnd1) == 0) {
                //x<<0 => x
                IR * newir = opnd0;
                BIN_opnd0(ir) = nullptr;
                if (rc.maintainDU()) {
                    xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
                }
                m_rg->freeIRTree(ir);
                ir = newir;
                change = true;
                break;
            }
            if (opnd0->getTypeSize(m_tm) == 4 && CONST_int_val(opnd1) == 32) {
                //x<<32 => 0, x is 32bit
                IR * newir = m_rg->getIRMgr()->buildImmInt(0, ir->getType());
                copyDbx(newir, ir, m_rg);
                if (rc.maintainDU()) {
                    xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
                }
                m_rg->freeIRTree(ir);
                ir = newir;
                change = true;
            }
        }
        break;
    }
    default:;
    }
    return ir; //No need to update DU.
}


IR * Refine::refineAdd(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_add());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 0) {
        //add X,0 => X
        BIN_opnd0(ir) = nullptr;
        m_rg->freeIRTree(ir);
        ir = op0;
        change = true;
        return ir; //No need to update DU.
    }

    if (op1->is_const() && op1->is_fp() && CONST_fp_val(op1) == HOST_FP(0.0)) {
        //For conservative purpose, we do not optimize ADD of float-type.
        //e.g: add X,0.0 => X
        //The reason is under the default rounding mode, in add X,0.0, if x
        //is not -0.0, then add X,0 is identical to X.
        //If x is -0.0, then the output of add X,0 must be +0.0,
        //which is not bitwise identical to -0.0.
        return ir;
    }

    if (op0->is_sub()) {
        //(x - y) + y => x
        IR * op0_of_op0 = BIN_opnd0(op0);
        IR * op1_of_op0 = BIN_opnd1(op0);
        if (op1_of_op0->isIREqual(op1, true)) {
            BIN_opnd0(op0) = nullptr;
            m_rg->freeIRTree(ir);
            ir = op0_of_op0;
            change = true;
        }
        return ir;
    }

    if (op0->is_lda() && op1->is_const() && op1->is_int()) {
        //  add:*<80>
        //    lda:*<1600> 's'
        //    intconst:u32 200
        //====>
        //  lda:*<80>, offset=200 's'
        HOST_INT imm = ((CConst*)op1)->getInt();
        if (imm >= 0) {
            IR_dt(op0) = ir->getType();
            op0->setOffset(op0->getOffset() + (UINT)imm);
            BIN_opnd0(ir) = nullptr;

            IR * tmp = ir;
            ir = op0;
            m_rg->freeIRTree(tmp);
            change = true;
        }
        return ir;
    }

    return ir;
}


IR * Refine::refineMul(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_mul());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op1->is_const() && op1->is_fp()) {
        if (g_is_opt_float && CONST_fp_val(op1) == HOST_FP(2.0)) {
            //mul.fp X,2.0 => add.fp X,X
            IR_code(ir) = IR_ADD;
            m_rg->freeIRTree(BIN_opnd1(ir));
            BIN_opnd1(ir) = m_rg->dupIRTree(BIN_opnd0(ir));
            if (rc.maintainDU()) {
                xoc::addUseForTree(BIN_opnd1(ir), BIN_opnd0(ir), m_rg);
            }
            ir->setParentPointer(false);
            change = true;
            return ir; //No need to update DU.
        }
        if (CONST_fp_val(op1) == HOST_FP(1.0)) {
            //For multiplication, float optimization is always safe.
            //e.g: optimize x*1.0 => x.
            //Under the default rounding mode, if x is a (sub)normal number,
            //x*1.0 == x always. If x is +/- infinity, then the output value
            //is +/- infinity of the same sign. If x is NaN, the exponent and
            //mantissa of NaN*1.0 are unchanged from NaN. The sign is also
            //identical to the input NaN.
            //mul X,1.0 => X
            BIN_opnd0(ir) = nullptr;
            m_rg->freeIRTree(ir);
            ir = op0;
            change = true;
            return ir; //No need to update DU.
        }
        return ir;
    }
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 2 &&
        RC_refine_mul_const(rc)) {
        //mul.int X,2 => add.int X,X
        IR_code(ir) = IR_ADD;
        m_rg->freeIRTree(BIN_opnd1(ir));
        BIN_opnd1(ir) = m_rg->dupIRTree(BIN_opnd0(ir));
        if (rc.maintainDU()) {
            xoc::addUseForTree(BIN_opnd1(ir), BIN_opnd0(ir), m_rg);
        }
        ir->setParentPointer(false);
        change = true;
        return ir; //No need to update DU.
    }
    if (op1->is_const() && op1->is_int()) {
        if (CONST_int_val(op1) == 1) {
            //mul X,1 => X
            IR * newir = op0;
            BIN_opnd0(ir) = nullptr;
            //No need revise DU, just keep X original DU info.
            m_rg->freeIRTree(ir);
            change = true;
            return newir;
        }
        if (CONST_int_val(op1) == 0) {
            //mul X,0 => 0
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            IR * newir = op1;
            BIN_opnd1(ir) = nullptr;
            m_rg->freeIRTree(ir);
            change = true;
            return newir;
        }
        if (RC_refine_mul_const(rc) &&
            op0->is_int() &&
            xcom::isPowerOf2(CONST_int_val(op1))) {
            //mul X,2^power => lsl X,power, logical shift left.
            CONST_int_val(op1) = xcom::getPowerOf2(CONST_int_val(op1));
            IR_code(ir) = IR_LSL;
            change = true;
            return ir; //No need to update DU.
        }
        return ir;
    }
    if (op0->is_div()) {
        //(x / y) * y => x
        IR * op0_of_op0 = BIN_opnd0(op0);
        IR * op1_of_op0 = BIN_opnd1(op0);
        if (op1_of_op0->isIREqual(op1, true)) {
            BIN_opnd0(op0) = nullptr;
            if (rc.maintainDU()) {
                xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
            }
            m_rg->freeIRTree(ir);
            ir = op0_of_op0;
            change = true;
        }
        return ir;
    }
    return ir;
}


IR * Refine::refineBand(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_band());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == -1) {
        //BAND X,-1 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = nullptr;
        ir = op0;
        m_rg->freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Refine::refineBor(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_bor());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 0) {
        //BOR X,0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = nullptr;
        ir = op0;
        m_rg->freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Refine::refineLand(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_land());
    IR * op0 = BIN_opnd0(ir);
    if (op0->is_const() && op0->is_int() && CONST_int_val(op0) == 1) {
        //1 && x => x
        IR * tmp = BIN_opnd1(ir);
        BIN_opnd1(ir) = nullptr;
        m_rg->freeIRTree(ir);
        change = true;
        return tmp;
    }
    if (op0->is_select()) {
        //(det ? b2 : b22 && c3) && c33 ==> b2&&c3
        IR * b2 = SELECT_trueexp(op0);
        IR * falseexp = SELECT_falseexp(op0);
        if (falseexp->is_cvt()) {
            falseexp = ((CCvt*)falseexp)->getLeafExp();
            ASSERT0(falseexp);
        }
        if (falseexp->getCode() == ir->getCode()) {
            IR * lor = falseexp;
            IR * b22 = BIN_opnd0(lor);
            IR * c3 = BIN_opnd1(lor);
            IR * c33 = BIN_opnd1(ir);
            if (b2->isIREqual(b22) && c3->isIREqual(c33)) {
                SELECT_falseexp(op0) = nullptr;
                if (rc.maintainDU()) {
                    xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
                }
                m_rg->freeIRTree(ir);
                ir = lor;
                change = true;
                return ir;
            }
        }
    }
    return ir;
}


IR * Refine::refineLor(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_lor());
    IR * op0 = BIN_opnd0(ir);
    if (op0->is_const() && op0->is_int() && CONST_int_val(op0) == 1) {
        //1 || x => 1
        if (rc.maintainDU()) {
            xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
        }
        IR * tmp = BIN_opnd0(ir);
        BIN_opnd0(ir) = nullptr;
        m_rg->freeIRTree(ir);
        change = true;
        return tmp;
    }
    if (op0->is_select()) {
        //(det ? b2 : b22 || c3) || c33 ==> b2||c3
        IR * b2 = SELECT_trueexp(op0);
        IR * falseexp = SELECT_falseexp(op0);
        if (falseexp->is_cvt()) {
            falseexp = ((CCvt*)falseexp)->getLeafExp();
            ASSERT0(falseexp);
        }
        if (falseexp->getCode() == ir->getCode()) {
            IR * lor = falseexp;
            IR * b22 = BIN_opnd0(lor);
            IR * c3 = BIN_opnd1(lor);
            IR * c33 = BIN_opnd1(ir);
            if (b2->isIREqual(b22) && c3->isIREqual(c33)) {
                SELECT_falseexp(op0) = nullptr;
                if (rc.maintainDU()) {
                    xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
                }
                m_rg->freeIRTree(ir);
                ir = lor;
                change = true;
                return ir;
            }
        }
    }
    return ir;
}


IR * Refine::refineSub(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_sub());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op0->isIRListEqual(op1)) {
        //sub X,X => 0
        if (rc.maintainDU()) {
            xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
        }
        IR * tmp = ir;
        Type const* ty;
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = m_tm->getSimplexTypeEx(D_U32);
        } else {
            ty = op0->getType();
        }

        if (ty->is_fp()) {
            ir = m_rg->getIRMgr()->buildImmFP(HOST_FP(0.0f), ty);
        } else {
            ir = m_rg->getIRMgr()->buildImmInt(0, ty);
        }

        copyDbx(ir, tmp, m_rg);
        m_rg->freeIRTree(tmp);
        change = true;
        return ir;
    }

    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 0) {
        // X - 0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = nullptr;
        ir = op0;
        m_rg->freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }

    if (op1->is_const() && op1->is_fp() && CONST_fp_val(op1) == HOST_FP(0.0)) {
        // X - 0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = nullptr;
        ir = op0;
        m_rg->freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Refine::refineXor(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_xor());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op0->isIRListEqual(op1)) {
        //xor X,X => 0
        if (rc.maintainDU()) {
            xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
        }
        IR * tmp = ir;
        Type const* ty;
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = m_tm->getSimplexTypeEx(D_U32);
        } else {
            ty = op0->getType();
        }
        ASSERT0(ty->is_sint() || ty->is_uint());
        ir = m_rg->getIRMgr()->buildImmInt(0, ty);
        copyDbx(ir, tmp, m_rg);
        m_rg->freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Refine::refineEq(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_eq());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op0->isIRListEqual(op1) && RC_do_fold_const(rc)) {
        //eq X,X => 1
        if (rc.maintainDU()) {
            xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
        }
        IR * tmp = ir;
        ir = m_rg->getIRMgr()->buildImmInt(1, m_tm->getSimplexTypeEx(D_B));
        copyDbx(ir, tmp, m_rg);
        m_rg->freeIRTree(tmp);
        change = true;
        //TODO: Inform its parent stmt IR to remove use
        //of the stmt out of du-chain.
        return ir;
    }
    return ir;
}


IR * Refine::refineNe(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_ne());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op0->isIRListEqual(op1) && RC_do_fold_const(rc)) {
        //ne X,X => 0
        if (rc.maintainDU()) {
            xoc::removeUseForTree(ir, m_rg, *rc.getOptCtx());
        }
        IR * tmp = ir;
        ir = m_rg->getIRMgr()->buildImmInt(0, m_tm->getSimplexTypeEx(D_B));
        copyDbx(ir, tmp, m_rg);
        m_rg->freeIRTree(tmp);
        change = true;
        //TODO: Inform its parent stmt IR to remove use
        //of the stmt out of du-chain.
        return ir;
    }
    return ir;
}


IR * Refine::refineAsr(IR * ir, bool & change)
{
    ASSERT0(ir->is_asr());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 0) {
        //asr X >> 0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = nullptr;
        ir = op0;
        m_rg->freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Refine::refineLsl(IR * ir, bool & change)
{
    ASSERT0(ir->is_lsl());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 0) {
        //X << 0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = nullptr;
        ir = op0;
        m_rg->freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Refine::refineLsr(IR * ir, bool & change)
{
    ASSERT0(ir->is_lsr());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != nullptr && op1 != nullptr);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 0) {
        //X >> 0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = nullptr;
        ir = op0;
        m_rg->freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}

static bool mayCauseHardWareException(IR_CODE ty, HOST_INT v0, HOST_INT v1)
{
    switch (ty) {
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
        return v1 == 0;
    default:;
    }
    return false;
}


IR * Refine::reassociation(IR * ir, bool & change)
{
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    IR_CODE opt1 = ir->getCode();
    IR_CODE opt2 = op0->getCode();

    if (op1->is_const() && op1->is_int() &&
        op0->is_associative() && ir->is_associative() &&
        BIN_opnd1(op0)->is_const() && BIN_opnd1(op0)->is_int() &&
        getArithPrecedence(opt1) == getArithPrecedence(opt2)) {
        if (mayCauseHardWareException(opt1, CONST_int_val(BIN_opnd1(op0)),
                                      CONST_int_val(op1))) {
            //Keep IR unchanged because it will trigger runtime exception.
            return ir;
        }

        //If n1,n2 is int const. OP1((OP2 X,n1), n2) => OP2(X, OP1(n1,n2))
        //where OP1, OP2 must be identical precedence.
        HOST_INT v = calcIntVal(opt1, CONST_int_val(BIN_opnd1(op0)),
                                CONST_int_val(op1));
        DATA_TYPE dt = ir->is_ptr() ?
            m_tm->getPointerSizeDtype() :
            ir->is_mc() ? m_tm->getAlignedDType(WORD_BITSIZE, true) :
                          op1->getDType();
        IR * new_const = m_rg->getIRMgr()->buildImmInt(v,
            m_tm->getSimplexTypeEx(dt));
        copyDbx(new_const, BIN_opnd0(ir), m_rg);
        IR_parent(op0) = nullptr;
        BIN_opnd0(ir) = nullptr;
        m_rg->freeIRTree(ir);
        m_rg->freeIRTree(BIN_opnd1(op0));
        BIN_opnd1(op0) = new_const;
        change = true;
        op0->setParentPointer(false);
        return op0; //No need to update DU.
    }
    return ir;
}


//Refine binary operations.
IR * Refine::refineBinaryOp(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(BIN_opnd0(ir) != nullptr && BIN_opnd1(ir) != nullptr);
    BIN_opnd0(ir) = refineIR(BIN_opnd0(ir), change, rc);
    BIN_opnd1(ir) = refineIR(BIN_opnd1(ir), change, rc);
    if (change) { ir->setParentPointer(false); }
    bool lchange = false;
    if (RC_do_fold_const(rc)) {
        ir = foldConst(ir, lchange, rc);
        change |= lchange;
        if (lchange) {
            return ir;
        }
    }
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_MUL:
    case IR_XOR:
    case IR_BAND:
    case IR_BOR:
    case IR_EQ:
    case IR_NE:
        //Operation commutative: ADD(CONST, ...) => ADD(..., CONST)
        if ((BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) ||
            BIN_opnd1(ir)->is_ptr() || BIN_opnd1(ir)->is_any()) {
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
        }

        if (BIN_opnd1(ir)->is_const()) {
            ir = reassociation(ir, lchange);
            change |= lchange;
            if (lchange) { break; }
        }

        if (ir->is_add()) { ir = refineAdd(ir, change, rc); }
        else if (ir->is_xor()) { ir = refineXor(ir, change, rc); }
        else if (ir->is_band()) { ir = refineBand(ir, change, rc); }
        else if (ir->is_bor()) { ir = refineBor(ir, change, rc); }
        else if (ir->is_mul()) { ir = refineMul(ir, change, rc); }
        else if (ir->is_eq()) { ir = refineEq(ir, change, rc); }
        else if (ir->is_ne()) { ir = refineNe(ir, change, rc); }
        break;
    case IR_SUB:
        if (BIN_opnd1(ir)->is_const()) {
            ir = reassociation(ir, lchange);
            change |= lchange;

            lchange = false;
            if (ir->is_sub()) {
                ir = refineSub(ir, lchange, rc);
            } else {
                //ir may not be IR_SUB anymore.
                ir = refineIR(ir, lchange, rc);
            }
            change |= lchange;
            break;
        }
        ir = refineSub(ir, change, rc);
        break;
    case IR_DIV:
        ir = refineDiv(ir, change, rc);
        break;
    case IR_REM:
        ir = refineRem(ir, change, rc);
        break;
    case IR_MOD:
        ir = refineMod(ir, change, rc);
        break;
    case IR_LAND:
        ir = refineLand(ir, change, rc);
        break;
    case IR_LOR:
        ir = refineLor(ir, change, rc);
        break;
    case IR_ASR:
        ir = refineAsr(ir, change);
        break;
    case IR_LSL:
        ir = refineLsl(ir, change);
        break;
    case IR_LSR:
        ir = refineLsr(ir, change);
        break;
    case IR_LT:
        if ((BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) ||
            (!BIN_opnd0(ir)->isPtr() && BIN_opnd1(ir)->isPtr())) {
            //Invert code: 0 < a ==> a > 0
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
            IR_code(ir) = IR_GT;
        }
        break;
    case IR_LE:
        if ((BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) ||
            (!BIN_opnd0(ir)->isPtr() && BIN_opnd1(ir)->isPtr())) {
            //Invert code: 0 <= a ==> a >= 0
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
            IR_code(ir) = IR_GE;
        }
        break;
    case IR_GT:
        if ((BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) ||
            (!BIN_opnd0(ir)->isPtr() && BIN_opnd1(ir)->isPtr())) {
            //Invert code: 0 > a ==> a < 0
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
            IR_code(ir) = IR_LT;
        }
        break;
    case IR_GE:
        if ((BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) ||
            (!BIN_opnd0(ir)->isPtr() && BIN_opnd1(ir)->isPtr())) {
            //Invert code: 0 >= a ==> a <= 0
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
            IR_code(ir) = IR_LE;
        }
        break;
    default: UNREACHABLE();
    }
    if (change) {
        ir->setParentPointer(false);
    }
    return ir;
}


IR * Refine::refineStoreArray(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->isArrayOp());
    IR * newir = refineArray(ir, change, rc);
    ASSERT0_DUMMYUSE(newir == ir);

    bool lchange = false;
    IR * newrhs = refineIR(ir->getRHS(), lchange, rc);
    if (lchange) {
        ir->setRHS(newrhs);
        IR_parent(newrhs) = ir;
        change = lchange;
    }

    ASSERT0(!::is_redundant_cvt(newrhs));
    if (RC_refine_stmt(rc)) {
        MD const* umd = newrhs->getExactRef();
        if (umd != nullptr && umd == ir->getExactRef()) {
            //Result and operand refered the same md.
            if (newrhs->is_cvt()) {
                //CASE: pr(i64) = cvt(i64, pr(i32))
                //Do NOT remove 'cvt'.
                ;
            } else {
                change = true;
                if (rc.maintainDU() && needBuildDUChain(rc)) {
                    xoc::coalesceDUChain(ir, newrhs, m_rg);
                    xoc::removeStmt(ir, m_rg, *rc.getOptCtx());
                }
                IRBB * bb = ir->getBB();
                if (bb != nullptr) {
                    BB_irlist(bb).remove(ir);
                    RC_stmt_removed(rc) = true;
                }
                m_rg->freeIRTree(ir);
                return nullptr;
            }
        }
    }
    return ir;
}


IR * Refine::refineArray(IR * ir, bool & change, RefineCtx & rc)
{
    IR * newbase = refineIR(ARR_base(ir), change, rc);
    if (newbase != ARR_base(ir)) {
        ARR_base(ir) = newbase;
        IR_parent(newbase) = ir;
    }

    IR * newsublist = nullptr;
    IR * last = nullptr;
    IR * s = xcom::removehead(&ARR_sub_list(ir));

    for (; s != nullptr;) {
        IR * newsub = refineIR(s, change, rc);
        if (newsub != s) {
            IR_parent(newsub) = ir;
        }
        xcom::add_next(&newsublist, &last, newsub);
        s = xcom::removehead(&ARR_sub_list(ir));
    }
    ARR_sub_list(ir) = newsublist;
    return ir;
}


IR * Refine::refineBranch(IR * ir)
{
    if (ir->is_falsebr() && BR_det(ir)->is_ne()) {
        IR_code(ir) = IR_TRUEBR;
        IR_code(BR_det(ir)) = IR_EQ;
    }
    return ir;
}


IR * Refine::refineLoad(IR * ir)
{
    Var * var = ir->getIdinfo();
    ASSERT0(var);
    if (var->is_array()) {
        //Convert LD(v) to LDA(ID(v)) if ID is array.
        //I think the convert is incorrect. If var a is array,
        //then LD(a,U32) means load 32bit element from a,
        //e.g: load a[0]. So do not convert LD into LDA.
        //IR * rm = ir;
        //ir = m_rg->getIRMgr()->buildLda(m_rg->getIRMgr()->
        //    buildId(LD_info(ir)));
        //m_rg->freeIR(rm);
    }
    return ir;
}


IR * Refine::refineCvt(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_cvt());
    CVT_exp(ir) = refineIR(CVT_exp(ir), change, rc);
    if (change) {
        IR_parent(CVT_exp(ir)) = ir;
    }

    if (CVT_exp(ir)->is_cvt()) {
        //cvt1(cvt2,xxx) => cvt1(xxx)
        IR * tmp = CVT_exp(ir);
        CVT_exp(ir) = CVT_exp(CVT_exp(ir));
        CVT_exp(tmp) = nullptr;
        m_rg->freeIRTree(tmp);
        IR_parent(CVT_exp(ir)) = ir;
        change = true;
    }

    if (ir->getType() == CVT_exp(ir)->getType()) {
        //cvt(i64, ld(i64)) => ld(i64)
        IR * tmp = CVT_exp(ir);
        IR_parent(tmp) = IR_parent(ir);
        CVT_exp(ir) = nullptr;
        m_rg->freeIRTree(ir);
        ir = tmp;
        change = true;
    }

    if (ir->is_cvt() && CVT_exp(ir)->is_const() &&
        ((ir->is_int() && CVT_exp(ir)->is_int()) ||
         (ir->is_fp() && CVT_exp(ir)->is_fp()))) {
        //cvt(i64, const) => const(i64)
        IR * tmp = CVT_exp(ir);
        IR_dt(tmp) = ir->getType();
        IR_parent(tmp) = IR_parent(ir);
        CVT_exp(ir) = nullptr;
        m_rg->freeIRTree(ir);
        ir = tmp;
        change = true;
    }

    return ir;
}


IR * Refine::refineDetViaSSADU(IR * ir, bool & change)
{
    ASSERT0(ir->is_judge());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    SSAInfo * op0_ssainfo = op0->isPROp() ? op0->getSSAInfo() : nullptr;
    SSAInfo * op1_ssainfo = op1->isPROp() ? op1->getSSAInfo() : nullptr;
    if (op0_ssainfo == nullptr || op1_ssainfo == nullptr) { return ir; }

    IR const* def0 = op0_ssainfo->getDef();
    IR const* def1 = op1_ssainfo->getDef();
    if (def0 == nullptr || def1 == nullptr) { return ir; }
    if (!def0->is_phi() || !def1->is_phi()) { return ir; }

    //Check if operand is the same const.
    IR const* phi_opnd0 = PHI_opnd_list(def0);
    IR const* phi_opnd1 = PHI_opnd_list(def1);
    for (; phi_opnd1 != nullptr && phi_opnd0 != nullptr;
         phi_opnd1 = IR_next(phi_opnd1), phi_opnd0 = IR_next(phi_opnd0)) {
        if (!phi_opnd0->is_const() ||
            !phi_opnd1->is_const() ||
            CONST_int_val(phi_opnd0) != CONST_int_val(phi_opnd1)) {
            return ir;
        }
    }

    if (phi_opnd0 != nullptr || phi_opnd1 != nullptr) {
        //These two PHIs does not have same number of operands.
        return ir;
    }

    Type const* ty = ir->getType();

    //Remove SSA DU for op0, op1.
    op0_ssainfo->removeUse(op0);
    op1_ssainfo->removeUse(op1);

    m_rg->freeIRTree(ir);
    change = true;
    return m_rg->getIRMgr()->buildImmInt(1, ty);
}


IR * Refine::refineIRUntilUnchange(IR * ir, bool & change, RefineCtx & rc)
{
    bool lchange = true;
    IR * newir = nullptr;
    for (; lchange;) {
        lchange = false;
        newir = refineIR(ir, lchange, rc);
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
IR * Refine::refineIR(IR * ir, bool & change, RefineCtx & rc)
{
    if (!g_do_refine) { return ir; }
    if (ir == nullptr) { return nullptr; }
    if (ir->hasSideEffect(false) || ir->isDummyOp()) { return ir; }
    bool tmpc = false;
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_EXP:
        ir = refineLoad(ir);
        break;
    SWITCH_CASE_INDIRECT_MEM_EXP:
        ir = refineILoad(ir, tmpc, rc);
        break;
    SWITCH_CASE_WRITE_ARRAY:
        ir = refineStoreArray(ir, tmpc, rc);
        break;
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR:
        ir = refineDirectStore(ir, tmpc, rc);
        break;
    case IR_SETELEM:
        ir = refineSetelem(ir, tmpc, rc);
        break;
    case IR_GETELEM:
        ir = refineGetelem(ir, tmpc, rc);
        break;
    SWITCH_CASE_INDIRECT_MEM_STMT:
        ir = refineIStore(ir, tmpc, rc);
        break;
    case IR_CALL:
        ir = refineCall(ir, tmpc, rc);
        break;
    case IR_ICALL:
        ir = refineICall(ir, tmpc, rc);
        break;
    SWITCH_CASE_ARITH:
    SWITCH_CASE_LOGIC_BIN:
    SWITCH_CASE_SHIFT:
    SWITCH_CASE_BITWISE_BIN:
        ir = refineBinaryOp(ir, tmpc, rc);
        break;
    case IR_BNOT:
    case IR_LNOT:
        ir = refineNot(ir, tmpc, rc);
        break;
    case IR_NEG:
        ir = refineNeg(ir, tmpc, rc);
        break;
    SWITCH_CASE_COMPARE: {
        //According input setting to do refinement.
        bool lchange = false;
        BIN_opnd0(ir) = refineIR(BIN_opnd0(ir), lchange, rc);
        BIN_opnd1(ir) = refineIR(BIN_opnd1(ir), lchange, rc);
        if (lchange) { ir->setParentPointer(false); }
        change |= lchange;

        //Do NOT do foldConst for conditional expr.
        //e.g: If NE(1, 0) => 1, one should generate NE(1, 0) again,
        //because of TRUEBR/FALSEBR do not accept IR_CONST.
        RefineCtx t(rc);
        RC_do_fold_const(t) = false;
        ir = refineBinaryOp(ir, tmpc, t);
        if (!ir->is_const()) {
            ir = refineDetViaSSADU(ir, tmpc);
        }
        break;
    }
    case IR_DO_WHILE:
    case IR_WHILE_DO:
        LOOP_det(ir) = refineDet(LOOP_det(ir), tmpc, rc);
        LOOP_body(ir) = refineIRlist(LOOP_body(ir), tmpc, rc);
        break;
    case IR_DO_LOOP:
        LOOP_det(ir) = refineDet(LOOP_det(ir), tmpc, rc);
        LOOP_init(ir) = refineIRlist(LOOP_init(ir), tmpc, rc);
        LOOP_step(ir) = refineIRlist(LOOP_step(ir), tmpc, rc);
        LOOP_body(ir) = refineIRlist(LOOP_body(ir), tmpc, rc);
        break;
    case IR_IF:
        IF_det(ir) = refineDet(IF_det(ir), tmpc, rc);
        IF_truebody(ir) = refineIRlist(IF_truebody(ir), tmpc, rc);
        IF_falsebody(ir) = refineIRlist(IF_falsebody(ir), tmpc, rc);
        break;
    case IR_IGOTO:
        IGOTO_vexp(ir) = refineIR(IGOTO_vexp(ir), tmpc, rc);
        break;
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
        ir = refineSwitch(ir, tmpc, rc);
        break;
    case IR_ARRAY:
        ir = refineArray(ir, tmpc, rc);
        break;
    case IR_CVT:
        ir = refineCvt(ir, tmpc, rc);
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        ir = refineBr(ir, tmpc, rc);
        break;
    case IR_RETURN:
        ir = refineReturn(ir, tmpc, rc);
        break;
    case IR_SELECT:
        ir = refineSelect(ir, tmpc, rc);
        break;
    SWITCH_CASE_LOOP_ITER_CFS_OP:
        break;
    case IR_PHI:
        ir = refinePhi(ir, tmpc, rc);
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
    default: ir = refineExtOp(ir, tmpc, rc);
    }
    if (tmpc && ir != nullptr && ir->is_stmt()) {
        ir->setParentPointer(true);
    }
    change |= tmpc;
    return ir;
}


IR * Refine::refineExtOp(IR * ir, bool & change, RefineCtx & rc)
{
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_STMT:
    SWITCH_CASE_EXT_EXP:
        //TODO:refine the extended operation.
        break;
    default:;
    }
    return ir;
}


//Reshaping determinate expression.
//Only the last non-stmt expression can be reserved to perform determinating.
IR * Refine::refineDet(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir);
    ir = refineIR(ir, change, rc);
    if (!ir->is_judge()) {
        IR * old = ir;
        ir = m_rg->getIRMgr()->buildJudge(ir);
        copyDbx(ir, old, m_rg);
        change = true;
    }
    return ir;
}


//Perform amendment for IRs that via primary
//convertion in order to generate legal IR tree.
//1. Mergering IR node like as :
//  ST(LD(v)) => ST(ID(v))
//
//  Generating icall instead of the deref of a function-pointer:
//  DEREF(CALL) => ICALL
//
//2. Delete non-statement node from statement list.
//3. Checking OFST of IR.
//4. Complementing det of control-flow node
//        IF(pr100, TRUE_PART, FALSE_PART) =>
//        IF(NE(pr100, 0), TRUE_PART, FALSE_PART)
//
//'ir_list': list to refine.
//
//NOTICE:
//  While m_rg function completed, IR's parent-pointer would be
//  overrided, setParentPointer() should be invoked at all.
IR * Refine::refineIRlist(IR * ir_list, bool & change, MOD RefineCtx & rc)
{
    bool lchange = true; //local flag
    while (lchange) {
        lchange = false;
        IR * new_list = nullptr;
        IR * last = nullptr;
        while (ir_list != nullptr) {
            IR * ir = xcom::removehead(&ir_list);
            IR * newIR = refineIR(ir, lchange, rc);
            xcom::add_next(&new_list, &last, newIR);
        }
        change |= lchange;
        ir_list = new_list;
    }
    return ir_list;
}


bool Refine::refineStmtList(MOD BBIRList & ir_list, MOD RefineCtx & rc)
{
    if (!g_do_refine) { return false; }
    bool change = false;
    IRListIter next_ct;
    ir_list.get_head(&next_ct);
    IRListIter ct = next_ct;
    for (; ct != nullptr; ct = next_ct) {
        IR * ir = ct->val();
        next_ct = ir_list.get_next(next_ct);

        bool lchange = false;
        RefineCtx lrc(rc);
        IR * newir = refineIRUntilUnchange(ir, lchange, lrc);
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


bool Refine::refineBBlist(MOD BBList * ir_bb_list, MOD RefineCtx & rc)
{
    if (!g_do_refine) { return false; }
    START_TIMER(t, "Refine IRBB list");
    bool change = false;
    BBListIter ct;
    for (ir_bb_list->get_head(&ct);
         ct != ir_bb_list->end(); ct = ir_bb_list->get_next(ct)) {
        change |= refineStmtList(BB_irlist(ct->val()), rc);
    }
    END_TIMER(t, "Refine IRBB list");
    if (!change) { return false; }
    if (rc.getOptCtx()->is_ref_valid()) {
        ASSERT0(m_rg->getDUMgr() && m_rg->getDUMgr()->verifyMDRef());
        //DU chain is kept by refinement.
        ASSERT0(verifyMDDUChain(m_rg, *rc.getOptCtx()));
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, *rc.getOptCtx()));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, *rc.getOptCtx()));
        ASSERT0(verifyIRandBB(ir_bb_list, m_rg));
    }
    rc.getOptCtx()->setInvalidPass(PASS_EXPR_TAB);
    OC_is_live_expr_valid(*rc.getOptCtx()) = false;
    OC_is_reach_def_valid(*rc.getOptCtx()) = false;
    return true;
}


IR * Refine::foldConstIntUnary(IR * ir, bool & change)
{
    ASSERT0(ir->isUnaryOp());
    ASSERT0(UNA_opnd(ir)->is_const());
    HOST_INT v0 = CONST_int_val(UNA_opnd(ir));
    if (ir->is_neg()) {
        ASSERTN(m_tm->getByteSize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = m_rg->getIRMgr()->buildImmInt(-v0, ir->getType());
        copyDbx(ir, oldir, m_rg);
        m_rg->freeIRTree(oldir);
        change = true;
        return ir;
    }
    if (ir->is_lnot()) {
        ASSERTN(m_tm->getByteSize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = m_rg->getIRMgr()->buildImmInt(!v0, ir->getType());
        copyDbx(ir, oldir, m_rg);
        m_rg->freeIRTree(oldir);
        change = true;
        return ir;
    }
    if (ir->is_bnot()) {
        ASSERTN(m_tm->getByteSize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = m_rg->getIRMgr()->buildImmInt(~v0, ir->getType());
        copyDbx(ir, oldir, m_rg);
        m_rg->freeIRTree(oldir);
        change = true;
        return ir;
    }
    return ir;
}


//Fold const for binary operation.
IR * Refine::foldConstIntBinary(IR * ir, bool & change)
{
    ASSERT0(ir->isBinaryOp());
    ASSERT0(BIN_opnd0(ir)->is_const());
    HOST_INT v0 = CONST_int_val(BIN_opnd0(ir));

    ASSERT0(BIN_opnd1(ir)->is_const());
    HOST_INT v1 = CONST_int_val(BIN_opnd1(ir));

    if (mayCauseHardWareException(ir->getCode(), v0, v1)) {
        //Keep IR unchanged because it will trigger runtime exception.
        return ir;
    }

    switch (ir->getCode()) {
    SWITCH_CASE_ARITH:
    SWITCH_CASE_LOGIC_BIN:
    SWITCH_CASE_BITWISE_BIN:
    SWITCH_CASE_COMPARE:
    case IR_ASR:
    case IR_LSL: {
        IR * x = nullptr;
        if (ir->is_bool()) {
            x = m_rg->getIRMgr()->buildImmInt(calcIntVal(ir->getCode(), v0, v1),
                    m_tm->getSimplexTypeEx(D_U32));
        } else if (ir->is_fp()) {
            //The result type of binary operation is
            //float point, inserting IR_CVT.
            Type const* ty = m_tm->hoistDTypeForBinOp(BIN_opnd0(ir),
                                                      BIN_opnd1(ir));
            x = m_rg->getIRMgr()->buildCvt(m_rg->getIRMgr()->buildImmInt(
                    calcIntVal(ir->getCode(), v0, v1), ty), ir->getType());
        } else {
            ASSERT0(ir->is_int());
            x = m_rg->getIRMgr()->buildImmInt(calcIntVal(ir->getCode(), v0, v1),
                                  ir->getType());
        }
        copyDbx(x, ir, m_rg);
        m_rg->freeIRTree(ir);
        ir = x;
        change = true;
        break;
    }
    case IR_LSR: {
        ASSERT0(ir->is_int());
        IR * x = m_rg->getIRMgr()->buildImmInt(
            calcLSRIntVal(ir->getType(), v0, v1), ir->getType());
        copyDbx(x, ir, m_rg);
        m_rg->freeIRTree(ir);
        ir = x;
        change = true;
        break;
    }
    default: UNREACHABLE();
    }

    return ir; //No need to update DU.
}


IR * Refine::foldConstFloatUnary(IR * ir, bool & change)
{
    ASSERT0(ir->isUnaryOp());
    if (ir->is_neg()) {
        ASSERTN(m_tm->getByteSize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = m_rg->getIRMgr()->buildImmFP(-CONST_fp_val(UNA_opnd(ir)),
                                          ir->getType());
        copyDbx(ir, oldir, m_rg);
        m_rg->freeIRTree(oldir);
        change = true;
        return ir; //No need to update DU.
    } else if (ir->is_lnot()) {
        ASSERTN(m_tm->getByteSize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        HOST_FP t = CONST_fp_val(UNA_opnd(ir));
        if (t == HOST_FP(0.0)) {
            t = 1.0;
        } else {
            t = HOST_FP(0.0);
        }
        ir = m_rg->getIRMgr()->buildImmFP(t, ir->getType());
        copyDbx(ir, oldir, m_rg);
        m_rg->freeIRTree(oldir);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


//Binary operation.
//The function prefer to compute ir's const value by float point type.
IR * Refine::foldConstFloatBinary(IR * ir, bool & change)
{
    ASSERT0(ir->isBinaryOp());
    ASSERT0(BIN_opnd0(ir)->is_const() &&BIN_opnd1(ir)->is_const());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    ASSERT0((op0->is_fp() || op0->is_int()) &&
            (op1->is_fp() || op1->is_int()));
    double v0 = op0->is_fp() ? CONST_fp_val(op0) : HOST_FP(CONST_int_val(op0));
    double v1 = op1->is_fp() ? CONST_fp_val(op1) : HOST_FP(CONST_int_val(op1));
    UINT tylen = MAX(m_tm->getByteSize(op0->getType()),
                     m_tm->getByteSize(op1->getType()));
    ASSERTN(tylen <= sizeof(HOST_FP), ("TODO"));
    DUMMYUSE(tylen);
    IR * oldir = ir;
    bool lchange = false;
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV: {
        //Choose FP type as immediate's type between op0 and op1.
        Type const* immty = ir->is_fp() ?
            ir->getType() : m_tm->getHostFPType(false);
        ASSERT0(immty->is_fp());
        ir = m_rg->getIRMgr()->buildImmFP(
            calcFloatVal(ir->getCode(), v0, v1), immty);
        copyDbx(ir, oldir, m_rg);
        lchange = true;
        break;
    }
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE: {
        //Choose Int type as immediate's type between op0 and op1.
        Type const* immty = ir->is_int() ?
            ir->getType() : m_tm->getHostUIntType();
        ASSERT0(immty->is_int());
        ir = m_rg->getIRMgr()->buildImmInt(calcBoolVal(ir->getCode(), v0, v1),
                                           immty);
        copyDbx(ir, oldir, m_rg);
        lchange = true;
        break;
    }
    default:;
    }
    if (lchange) {
        m_rg->freeIRTree(oldir);
        change = true;
    }
    return ir; //No need to update DU.
}


IR * Refine::foldConst(IR * ir, bool & change, RefineCtx const& rc)
{
    bool doit = false;
    for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            IR * new_kid = foldConst(kid, change, rc);
            if (new_kid != kid) {
                doit = true;
                ir->setKid(i, new_kid);
            }
        }
    }
    if (doit) {
        ir->setParentPointer();
    }
    switch (ir->getCode()) {
    SWITCH_CASE_BIN: {
        IR * t0 = BIN_opnd0(ir);
        IR * t1 = BIN_opnd1(ir);
        ASSERT0(ir->isBinaryOp());
        ASSERTN(IR_MAX_KID_NUM(ir) == 2, ("binary op"));
        ASSERTN(t0 && t1, ("binary op"));
        if (t0->is_const() && t1->is_const()) {
            if (t0->is_fp() && t1->is_fp() && g_is_opt_float) {
                return foldConstFloatBinary(ir, change);
            } else if ((t0->is_fp() && t1->is_int()) ||
                        (t1->is_fp() && t0->is_int())) {
                return foldConstFloatBinary(ir, change);
            } else if (t0->is_int() && t1->is_int()) {
                return foldConstIntBinary(ir, change);
            }
        }
        break;
    }
    case IR_BNOT:
    case IR_LNOT: //Boolean logical not e.g LNOT(0x0001) = 0x0000
    case IR_NEG:
        //NEG(1.0) => INT(-1.0)
        ASSERTN(IR_MAX_KID_NUM(ir) == 1, ("unary op"));
        ASSERT0(UNA_opnd(ir) != nullptr);
        if (UNA_opnd(ir)->is_const() &&
            UNA_opnd(ir)->is_fp() && g_is_opt_float) {
            ir = foldConstFloatUnary(ir, change);
            if (change) { return ir; }
        } else if (UNA_opnd(ir)->is_const() && UNA_opnd(ir)->is_int()) {
            ir = foldConstIntUnary(ir, change);
            if (change) { return ir; }
        }
        break;
    default: return ir;
    }
    return refineComparisonAndLogic(ir, change, rc);
}


IR * Refine::StrengthReduce(MOD IR * ir, MOD bool & change, RefineCtx & rc)
{
    return foldConst(ir, change, rc);
}


//User must invoke 'setParentPointer' to maintain the validation of IR tree.
void Refine::invertCondition(IR ** cond, Region * rg)
{
    switch ((*cond)->getCode()) {
    case IR_LAND:
    case IR_LOR: {
        IR * parent = IR_parent(*cond);
        IR * newir = rg->getIRMgr()->buildLogicalNot(*cond);
        copyDbx(newir, *cond, rg);
        IR_parent(newir) = parent;
        *cond = newir;
        newir->setParentPointer(true);
        //Or if you want, can generate ir as following rule:
        //    !(a||b) = !a && !b;
        //    !(a&&b) = !a || !b;
        break;
    }
    case IR_LT:
        IR_code(*cond) = IR_GE;
        break;
    case IR_LE:
        IR_code(*cond) = IR_GT;
        break;
    case IR_GT:
        IR_code(*cond) = IR_LE;
        break;
    case IR_GE:
        IR_code(*cond) = IR_LT;
        break;
    case IR_EQ:
        IR_code(*cond) = IR_NE;
        break;
    case IR_NE:
        IR_code(*cond) = IR_EQ;
        break;
    default:
        ASSERTN(0, ("TODO"));
    }
}


bool Refine::dump() const
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


bool Refine::perform(OptCtx & oc, MOD RefineCtx & rc)
{
    bool change = false;
    if (m_rg->getIRList() != nullptr) {
        //Do primitive refinement.
        START_TIMER(t, "Do Primitive Refinement");
        IR * irs = refineIRlist(m_rg->getIRList(), change, rc);
        ASSERT0(xoc::verifyIRList(irs, nullptr, m_rg));
        m_rg->setIRList(irs);
        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRefine()) {
            dump();
        }
        END_TIMER(t, "Do Primitive Refinement");
        return change;
    }

    //Do primitive refinement.
    START_TIMER(t, "Do Primitive Refinement");
    change = refineBBlist(m_rg->getBBList(), rc);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRefine()) {
        dump();
    }
    END_TIMER(t, "Do Primitive Refinement");
    return change;
}


bool Refine::perform(OptCtx & oc)
{
    RefineCtx rc(&oc);
    return perform(oc, rc);
}
//END Refine

} //namespace xoc
