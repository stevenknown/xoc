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
#include "prdf.h"
#include "prssainfo.h"
#include "ir_ssa.h"
#include "mdssainfo.h"
#include "ir_mdssa.h"

namespace xoc {

//Algebraic identities.
IR * Region::refineIload1(IR * ir, bool & change)
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
    INT ild_ofst = ILD_ofst(ir);

    IR * ld = buildLoad(LDA_idinfo(base), ir->getType());
    copyDbx(ld, base, this);

    LD_ofst(ld) += ild_ofst;
    IR_DU_MGR * dumgr = getDUMgr();
    if (dumgr != NULL) {
        //Consider the ir->getOffset() and copying MDSet info from 'ir' to 'ld.
        //Note: do not recompute MD and overlap set to ld because that
        //might generate new MD that not versioned by MDSSAMgr.
        //ir's MD ref must be equivalent to ld.
        ld->copyRef(ir, this);
        if (ld->getEffectRef() == NULL) {
            MD const* t = allocRefForLoad(ld);
            CHECK_DUMMYUSE(ld->getRefMDSet() && ld->getRefMDSet()->is_contain(t));
        }
        dumgr->changeUse(ld, ir, getMiscBitSetMgr());
        if (getMDSSAMgr() != NULL) {
            getMDSSAMgr()->changeUse(ir, ld);
        }
    }
    copyAI(ir, ld);
    freeIRTree(ir);
    change = true;

    //Do not need set parent.
    return ld;
}


IR * Region::refineIload2(IR * ir, bool & change)
{
    ASSERT0(ir->is_ild() &&
            ILD_base(ir)->is_lda() &&
            LDA_ofst(ILD_base(ir)) != 0);
    //Convert
    //    ILD,ofst1
    //     LDA,ofst2
    //=>
    //    LD,ofst1+ofst2
    IR * base = ILD_base(ir);
    IR * ld = buildLoad(LDA_idinfo(base), ir->getType());
    LD_ofst(ld) = LDA_ofst(base) + ILD_ofst(ir);
    copyDbx(ld, ir, this);
    if (getDUMgr() != NULL) {
        ld->copyRef(ir, this);
        if (ld->getEffectRef() == NULL) {
            MD const* t = allocRefForLoad(ld);
            CHECK_DUMMYUSE(ld->getRefMDSet() && ld->getRefMDSet()->is_contain(t));
        }
        getDUMgr()->changeUse(ld, ir, getMiscBitSetMgr());
        if (getMDSSAMgr() != NULL) {
            getMDSSAMgr()->changeUse(ir, ld);
        }
    }
    copyAI(ir, ld);
    freeIRTree(ir);
    change = true;
    //Do not need to set parent pointer.

    return ld;
}


IR * Region::refineIload(IR * ir, bool & change, RefineCtx & rc)
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
        return refineIload1(ir, change);
    } else if (base->is_lda() && LDA_ofst(base) != 0) {
        //Convert
        //    ILD,ofst1
        //     LDA,ofst2
        //=>
        //    LD,ofst1+ofst2
        return refineIload2(ir, change);
    } else {
        ILD_base(ir) = refineIR(base, change, rc);
        if (change) {
            IR_parent(ILD_base(ir)) = ir;
        }
    }
    return ir;
}


IR * Region::refineIstore(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_ist());
    bool t = false;
    bool lchange = false;
    IST_base(ir) = refineIR(IST_base(ir), t, rc);
    if (t) { ir->setParent(IST_base(ir)); }
    lchange |= t;

    t = false;
    IST_rhs(ir) = refineIR(IST_rhs(ir), t, rc);
    if (t) { ir->setParent(IST_rhs(ir)); }
    lchange |= t;

    IR * lhs = IST_base(ir);
    IR * rhs = IST_rhs(ir);
    IR_DU_MGR * dumgr = getDUMgr();
    if (lhs->is_lda()) {
        //Convert :
        //1. IST(LDA(var))=X to ST(var)=X
        //2. IST(LDA(var), ofst)=X to ST(var, ofst)=X
        //3. IST(LDA(var,ofst))=X to ST(var, ofst)=X
        //4. IST(LDA(var,ofst1), ofst2)=X to ST(var, ofst1+ofst2)=X
        IR * newir = buildStore(LDA_idinfo(lhs),
                                ir->getType(),
                                LDA_ofst(lhs) + IST_ofst(ir),
                                IST_rhs(ir));
        if (dumgr != NULL) {
            newir->copyRef(ir, this);
            if (newir->getEffectRef() == NULL) {
                MD const* t2 = allocRefForStore(newir);
                CHECK_DUMMYUSE(newir->getRefMDSet() &&
                    newir->getRefMDSet()->is_contain(t2));
            }
            if (getMDSSAMgr() != NULL) {
                getMDSSAMgr()->changeDef(ir, newir);
            }

            bool maybe_exist_expired_du = false;
            if (newir->getRefMD() == NULL) {
                newir->setRefMD(genMDforStore(newir), this);
                newir->cleanRefMDSet();

                //Change IST to ST may result the du chain invalid.
                //There may be USEs that would not reference the MD that ST modified.
                //e.g: p = &a; p = &b;
                //ist(p, 10), ist may def a, b
                //After change to st(a, 10), st only define a, and is not define b any more.
                maybe_exist_expired_du = true;
            }

            dumgr->changeDef(newir, ir, getMiscBitSetMgr());

            if (maybe_exist_expired_du) {
                dumgr->removeExpiredDUForStmt(newir);
            }
        }
        copyAI(ir, newir);

        IST_rhs(ir) = NULL;
        freeIR(ir);
        ir = newir;
        lchange = true; //Keep the result type of ST unchanged.
        rhs = ST_rhs(ir); //No need to update DU.
    }

    rhs = ir->getRHS();
    if (rhs->is_ild() && ILD_base(rhs)->is_lda()) {
        //ILD(LDA(var)) => LD(var)
        IR * newrhs = buildLoad(LDA_idinfo(ILD_base(rhs)), rhs->getType());
        ir->setRHS(newrhs);
        copyDbx(newrhs, rhs, this);
        if (dumgr != NULL) {
            newrhs->copyRef(rhs, this);
            if (newrhs->getEffectRef() == NULL) {
                MD const* t2 = allocRefForLoad(newrhs);
                CHECK_DUMMYUSE(newrhs->getRefMDSet() &&
                    newrhs->getRefMDSet()->is_contain(t2));
            }
            dumgr->copyDUSet(newrhs, rhs);
            dumgr->changeUse(newrhs, rhs, getMiscBitSetMgr());
            if (getMDSSAMgr() != NULL) {
                getMDSSAMgr()->changeUse(rhs, newrhs);
            }
        }
        copyAI(rhs, newrhs);

        ASSERTN(rhs->is_single(), ("expression cannot be linked to chain"));
        freeIRTree(rhs);
        lchange = true;
        rhs = newrhs;
    }

    if (RC_insert_cvt(rc)) {
        ir->setRHS(insertCvt(ir, rhs, change));
    }

    if (lchange) {
        ir->setParentPointer(false);
    }

    if (lchange && dumgr != NULL) {
        ASSERT0(!dumgr->removeExpiredDU(ir));
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


IR * Region::refineStore(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_st() || ir->is_stpr());

    bool lchange = false;

    IR * rhs = ir->is_st() ? ST_rhs(ir) : STPR_rhs(ir);

    if (RC_refine_stmt(rc) &&
        rhs->is_pr() &&
        ir->is_stpr() &&
        PR_no(rhs) == STPR_no(ir)) {

        //Remove pr1 = pr1.
        if (getDUMgr() != NULL) {
            getDUMgr()->removeIROutFromDUMgr(ir);
        }

        IRBB * bb = ir->getBB();
        if (bb != NULL) {
            BB_irlist(bb).remove(ir);
            RC_stmt_removed(rc) = true;
        }

        freeIRTree(ir);
        change = true;
        return NULL;
    }

    rhs = refineIR(rhs, lchange, rc);
    ir->setRHS(rhs);

    ASSERT0(!::is_redundant_cvt(rhs));
    if (RC_refine_stmt(rc)) {
        MD const* umd = rhs->getExactRef();
        if (umd != NULL && umd == ir->getExactRef()) {
            //Result and operand referenced the same md.
            if (rhs->is_cvt()) {
                //CASE: pr(i64) = cvt(i64, pr(i32))
                //Do NOT remove 'cvt'.
                ;
            } else {
                change = true;
                if (getMDSSAMgr() != NULL) {
                    getMDSSAMgr()->dump();
                    getMDSSAMgr()->coalesceVersion(ir, rhs);
                    getMDSSAMgr()->dump();
                }
                if (getDUMgr() != NULL) {
                    getDUMgr()->removeIROutFromDUMgr(ir);
                }

                IRBB * bb = ir->getBB();
                if (bb != NULL) {
                    BB_irlist(bb).remove(ir);
                    RC_stmt_removed(rc) = true;
                }

                freeIRTree(ir);
                return NULL;
            }
        }
    }

    if (RC_insert_cvt(rc)) {
        ir->setRHS(insertCvt(ir, ir->getRHS(), lchange));
    }

    change |= lchange;

    if (lchange) {
        ir->setParentPointer(false);
        IR_DU_MGR * dumgr = getDUMgr();
        if (dumgr != NULL) {
            ASSERT0(!dumgr->removeExpiredDU(ir));
        }
    }

    return ir;
}


IR * Region::refineSetelem(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_setelem());
    IR * rhs = refineIR(SETELEM_rhs(ir), change, rc);
    if (rhs != SETELEM_rhs(ir)) {
        ir->setParent(rhs);
        ir->setRHS(rhs);
    }
    IR * ofst = refineIR(SETELEM_ofst(ir), change, rc);
    if (ofst != SETELEM_ofst(ir)) {
        ir->setParent(ofst);
        SETELEM_ofst(ir) = ofst;
    }
    return ir;
}


IR * Region::refineGetelem(IR * ir, bool & change, RefineCtx & rc)
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


IR * Region::refineCall(IR * ir, bool & change, RefineCtx & rc)
{
    bool lchange = false;
    if (CALL_param_list(ir) != NULL) {
        IR * param = xcom::removehead(&CALL_param_list(ir));
        IR * newparamlst = NULL;
        IR * last = NULL;
        while (param != NULL) {
            IR * newp = refineIR(param, lchange, rc);
            add_next(&newparamlst, &last, newp);
            last = newp;
            param = xcom::removehead(&CALL_param_list(ir));
        }
        CALL_param_list(ir) = newparamlst;
    }

    if (lchange) {
        change = true;
        ir->setParentPointer(false);
    }

    if (lchange) {
        IR_DU_MGR * dumgr = getDUMgr();
        if (dumgr != NULL) {
            ASSERT0(!dumgr->removeExpiredDU(ir));
        }
    }
    return ir;
}


IR * Region::refineIcall(IR * ir, bool & change, RefineCtx & rc)
{
    refineCall(ir, change, rc);
    return ir;
}


IR * Region::refineSwitch(IR * ir, bool & change, RefineCtx & rc)
{
    bool l = false;
    SWITCH_vexp(ir) = refineIR(SWITCH_vexp(ir), l, rc);
    if (l) {
        if (getDUMgr() != NULL) {
            ASSERT0(!getDUMgr()->removeExpiredDU(ir));
        }
        IR_parent(SWITCH_vexp(ir)) = ir;
        change = true;
    }

    l = false;
    SWITCH_body(ir) = refineIRlist(SWITCH_body(ir), l, rc);
    change |= l;
    return ir;
}


IR * Region::refineBr(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->isConditionalBr());
    bool l = false;
    BR_det(ir) = refineDet(BR_det(ir), l, rc);
    ir = refineBranch(ir);
    if (l) {
        if (getDUMgr() != NULL) {
            ASSERT0(!getDUMgr()->removeExpiredDU(ir));
        }
        change = true;
    }
    return ir;
}


IR * Region::refineReturn(IR * ir, bool & change, RefineCtx & rc)
{
    if (RET_exp(ir) == NULL) { return ir; }

    bool lchange = false;
    RET_exp(ir) = refineIR(RET_exp(ir), lchange, rc);

    if (lchange) {
        change = true;
        ir->setParentPointer(false);
        if (getDUMgr() != NULL) {
            ASSERT0(!getDUMgr()->removeExpiredDU(ir));
        }
    }
    return ir;
}


//IR already has built ssa info.
IR * Region::refinePhi(IR * ir, bool & change, RefineCtx & rc)
{
    //phi(1, 1, ...) => 1
    bool all_be_same_const = true;
    IR * opnd = PHI_opnd_list(ir);
    HOST_INT val = 0;
    if (opnd->is_const() && opnd->is_int()) {
        val = CONST_int_val(opnd);
        for (opnd = opnd->get_next(); opnd != NULL; opnd = opnd->get_next()) {
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

    SEGIter * sc;
    for (INT u = SSA_uses(ssainfo).get_first(&sc);
         u >= 0; u = SSA_uses(ssainfo).get_next(u, &sc)) {
        IR * use = getIR(u);
        ASSERT0(use && use->is_pr());
        IR * lit = buildImmInt(val, use->getType());
        ASSERT0(IR_parent(use));
        IR_parent(use)->replaceKid(use, lit,  false);
        freeIR(use);
    }

    ssainfo->cleanDU();

    change = true;

    if (RC_refine_stmt(rc)) {
        IRBB * bb = ir->getBB();
        ASSERT0(bb);
        BB_irlist(bb).remove(ir);
        RC_stmt_removed(rc) = true;
        freeIRTree(ir);
    }
    return NULL;
}


//Transform ir to IR_LNOT.
//Return the transformed ir if changed, or the original.
//Note this function will not free ir, since it is the caller's responsibility.
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
    IR * det = SELECT_pred(ir);
    if (det->is_ne()) {
        IR * trueexp = SELECT_trueexp(ir);
        IR * falseexp = SELECT_falseexp(ir);
        if (BIN_opnd1(det)->isConstIntValueEqualTo(0) &&
            trueexp->isConstIntValueEqualTo(0) &&
            falseexp->isConstIntValueEqualTo(1)) {
            IR * lnot = rg->buildUnaryOp(
                           IR_LNOT,
                           rg->getTypeMgr()->getBool(),
                           BIN_opnd0(det));
            BIN_opnd0(det) = NULL;
            return lnot;
        }
    }

    if (det->is_eq()) {
        IR * trueexp = SELECT_trueexp(ir);
        IR * falseexp = SELECT_falseexp(ir);
        if (BIN_opnd1(det)->isConstIntValueEqualTo(0) &&
            trueexp->isConstIntValueEqualTo(1) &&
            falseexp->isConstIntValueEqualTo(0)) {
            IR * lnot = rg->buildLogicalNot(BIN_opnd0(det));
            BIN_opnd0(det) = NULL;
            return lnot;
        }
    }

    return ir;
}


IR * Region::refineSelect(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_select());
    SELECT_pred(ir) = refineDet(SELECT_pred(ir), change, rc);
    SELECT_trueexp(ir) = refineIRlist(SELECT_trueexp(ir), change, rc);
    SELECT_falseexp(ir) = refineIRlist(SELECT_falseexp(ir), change, rc);
    IR * det = foldConst(SELECT_pred(ir), change);

    IR * gen;
    if (det->is_const() && det->is_int()) {
        HOST_INT v = CONST_int_val(det);
        if (v == 0) {
            IR * rm = SELECT_trueexp(ir);
            IR * rm2 = det;
            ir = SELECT_falseexp(ir);
            ASSERT0(ir->is_exp());
            freeIRTree(rm);
            freeIRTree(rm2);
            change = true;
        } else {
            IR * rm = SELECT_falseexp(ir);
            IR * rm2 = det;
            ir = SELECT_trueexp(ir);
            ASSERT0(ir->is_exp());
            freeIRTree(rm);
            freeIRTree(rm2);
            change = true;
        }
    } else if (det->is_const() && det->is_fp()) {
        double v = CONST_fp_val(det);
        if (v < EPSILON) { //means v == 0.0
            IR * rm = SELECT_trueexp(ir);
            IR * rm2 = det;
            ir = SELECT_falseexp(ir);
            ASSERT0(ir->is_exp());
            freeIRTree(rm);
            freeIRTree(rm2);
            change = true;
        } else {
            IR * rm = SELECT_falseexp(ir);
            IR * rm2 = det;
            ir = SELECT_trueexp(ir);
            ASSERT0(ir->is_exp());
            freeIRTree(rm);
            freeIRTree(rm2);
            change = true;
        }
    } else if (det->is_str()) {
        IR * rm = SELECT_falseexp(ir);
        IR * rm2 = det;
        ir = SELECT_trueexp(ir);
        ASSERT0(ir->is_exp());
        freeIRTree(rm);
        freeIRTree(rm2);
        change = true;
    } else if (RC_hoist_to_lnot(rc) &&
               (gen = hoistSelectToLnot(ir, this)) != ir) {
        freeIRTree(ir);
        ir = gen;
        change = true;
    }

    if (change) {
        if (ir->is_select() && !SELECT_pred(ir)->is_judge()) {
            SELECT_pred(ir) = buildJudge(SELECT_pred(ir));
            ir->setParent(SELECT_pred(ir));
        } else {
            ir->setParentPointer(false);
        }
    }
    return ir; //No need to update DU.
}


IR * Region::refineNeg(IR * ir, bool & change)
{
    ASSERT0(ir->is_neg());
    bool lchange = false;
    ir = foldConst(ir, lchange);
    change |= lchange;
    if (!lchange && UNA_opnd(ir)->is_neg()) {
        //-(-x) => x
        IR * tmp = UNA_opnd(UNA_opnd(ir));
        UNA_opnd(UNA_opnd(ir)) = NULL;
        freeIRTree(ir);
        change = true;
        return tmp;
    }
    return ir;
}


//Logic not: !(0001) = 0000
//Bitwise not: !(0001) = 1110
IR * Region::refineNot(IR * ir, bool & change, RefineCtx & rc)
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
            op0->invertIRType(this);
            lchange = true;
            break;
        default: break;
        }
        if (lchange) {
            UNA_opnd(ir) = NULL;
            freeIRTree(ir);
            change = true;
            ir = op0;
        }
    }

    ir = foldConst(ir, change);
    return ir;
}


//If the value of opnd0 is not a multiple of opnd1,
//((opnd0 div opnd1) mul opnd1) may not equal to opnd0.
IR * Region::refineDiv(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_div());
    IR * op1 = BIN_opnd1(ir);
    IR * op0 = BIN_opnd0(ir);
    if (g_is_opt_float && op1->is_const() && op1->is_fp() && !op0->is_const()) {
        HOST_FP fp_imm = CONST_fp_val(op1);
        if (fp_imm == 1.0) {
            //X/1.0 => X
            IR * newIR = op0;
            BIN_opnd0(ir) = NULL;
            freeIRTree(ir);
            ir = newIR;
            change = true;
            return ir;
        }
        if (fp_imm == 0) {
            //X/0
            return ir;
        }
        if ((xcom::isPowerOf2(abs((INT)(fp_imm))) || xcom::isPowerOf5(fp_imm))) {
            //X/n => X*(1.0/n)
            IR_code(ir) = IR_MUL;
            CONST_fp_val(op1) = ((HOST_FP)1.0) / fp_imm;
            change = true;
            return ir;
        }
    } else if (op1->is_const() &&
               op1->is_int() &&
               xcom::isPowerOf2(CONST_int_val(op1)) &&
               RC_refine_div_const(rc)) {
        //X/2 => X>>1, arith shift right.
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
    } else if (op0->isIREqual(op1, true)) {
        //X/X => 1.
        IR * tmp = ir;
        Type const* ty;
        TypeMgr * dm = getTypeMgr();
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = dm->getSimplexTypeEx(D_U32);
        } else {
            ty = op0->getType();
        }

        if (ty->is_fp()) {
            ir = buildImmFp(1.0f, ty);
        } else {
            ir = buildImmInt(1, ty);
        }

        //Cut du chain for opnd0, opnd1 and their def-stmt.
        if (getDUMgr() != NULL) {
            getDUMgr()->removeUseOutFromDefset(tmp);
        }

        copyDbx(ir, tmp, this);
        freeIRTree(tmp);
        change = true;
        return ir;
    }
    return ir;
}


IR * Region::refineMod(IR * ir, bool & change)
{
    ASSERT0(ir->is_mod());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    CHECK_DUMMYUSE(op0);
    CHECK_DUMMYUSE(op1);

    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 1) {
        //mod X,1 => 0
        IR * tmp = ir;
        ir = dupIRTree(op1);
        CONST_int_val(ir) = 0;
        if (getDUMgr() != NULL) {
            getDUMgr()->removeUseOutFromDefset(tmp);
        }
        freeIRTree(tmp);
        change = true;
        return ir;
    }
    return ir;
}


IR * Region::refineRem(IR * ir, bool & change)
{
    ASSERT0(ir->is_rem());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);

    CHECK_DUMMYUSE(op0);
    CHECK_DUMMYUSE(op1);

    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 1) {
        //rem X,1 => 0
        IR * tmp = ir;
        ir = dupIRTree(op1);
        CONST_int_val(ir) = 0;
        if (getDUMgr() != NULL) {
            getDUMgr()->removeUseOutFromDefset(tmp);
        }
        freeIRTree(tmp);
        change = true;
        return ir;
    }
    return ir;
}


IR * Region::refineAdd(IR * ir, bool & change)
{
    ASSERT0(ir->is_add());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 0) {
        //add X,0 => X
        BIN_opnd0(ir) = NULL;
        freeIRTree(ir);
        ir = op0;
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Region::refineMul(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_mul());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (g_is_opt_float &&
        op1->is_const() &&
        op1->is_fp() &&
        CONST_fp_val(op1) == 2.0) {
        //mul X,2.0 => add.fp X,X
        IR_code(ir) = IR_ADD;
        freeIRTree(BIN_opnd1(ir));
        BIN_opnd1(ir) = dupIRTree(BIN_opnd0(ir));

        if (getDUMgr() != NULL) {
            getDUMgr()->copyIRTreeDU(BIN_opnd1(ir), BIN_opnd0(ir), true);
        }

        ir->setParentPointer(false);
        change = true;
        return ir; //No need to update DU.
    } else if (op1->is_const() &&
               op1->is_int() &&
               CONST_int_val(op1) == 2) {
        //mul X,2 => add.int X,X
        IR_code(ir) = IR_ADD;
        freeIRTree(BIN_opnd1(ir));
        BIN_opnd1(ir) = dupIRTree(BIN_opnd0(ir));

        if (getDUMgr() != NULL) {
            getDUMgr()->copyIRTreeDU(BIN_opnd1(ir),
                                            BIN_opnd0(ir), true);
        }

        ir->setParentPointer(false);
        change = true;
        return ir; //No need to update DU.
    } else if (op1->is_const() && op1->is_int()) {
        if (CONST_int_val(op1) == 1) {
            //mul X,1 => X
            IR * newir = op0;
            BIN_opnd0(ir) = NULL;
            //Do MOT need revise IR_DU_MGR, just keep X original DU info.
            freeIRTree(ir);
            change = true;
            return newir;
        } else if (CONST_int_val(op1) == 0) {
            //mul X,0 => 0
            if (getDUMgr() != NULL) {
                getDUMgr()->removeUseOutFromDefset(ir);
            }
            IR * newir = op1;
            BIN_opnd1(ir) = NULL;
            freeIRTree(ir);
            change = true;
            return newir;
        } else if (RC_refine_mul_const(rc) &&
                   op0->is_int() &&
                   xcom::isPowerOf2(CONST_int_val(op1))) {
            //mul X,4 => lsl X,2, logical shift left.
            CONST_int_val(op1) = xcom::getPowerOf2(CONST_int_val(op1));
            IR_code(ir) = IR_LSL;
            change = true;
            return ir; //No need to update DU.
        }
    }
    return ir;
}


IR * Region::refineBand(IR * ir, bool & change)
{
    ASSERT0(ir->is_band());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == -1) {
        //BAND X,-1 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = NULL;
        ir = op0;
        freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Region::refineBor(IR * ir, bool & change)
{
    ASSERT0(ir->is_bor());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op1->is_const() && op1->is_int() && CONST_int_val(op1) == 0) {
        //BOR X,0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = NULL;
        ir = op0;
        freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Region::refineLand(IR * ir, bool & change)
{
    ASSERT0(ir->is_land());
    IR * op0 = BIN_opnd0(ir);
    if (op0->is_const() && op0->is_int() && CONST_int_val(op0) == 1) {
        //1 && x => x
        IR * tmp = BIN_opnd1(ir);
        BIN_opnd1(ir) = NULL;
        freeIRTree(ir);
        change = true;
        return tmp;
    }
    return ir;
}


IR * Region::refineLor(IR * ir, bool & change)
{
    ASSERT0(ir->is_lor());
    IR * op0 = BIN_opnd0(ir);
    if (op0->is_const() && op0->is_int() && CONST_int_val(op0) == 1) {
        //1 || x => 1
        if (getDUMgr() != NULL) {
            getDUMgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = BIN_opnd0(ir);
        BIN_opnd0(ir) = NULL;
        freeIRTree(ir);
        change = true;
        return tmp;
    }
    return ir;
}


IR * Region::refineSub(IR * ir, bool & change)
{
    ASSERT0(ir->is_sub());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op0->isIRListEqual(op1)) {
        //sub X,X => 0
        if (getDUMgr() != NULL) {
            getDUMgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = ir;
        Type const* ty;
        TypeMgr * dm = getTypeMgr();
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = dm->getSimplexTypeEx(D_U32);
        } else {
            ty = op0->getType();
        }

        if (ty->is_fp()) {
            ir = buildImmFp(0.0f, ty);
        } else {
            ir = buildImmInt(0, ty);
        }

        copyDbx(ir, tmp, this);
        freeIRTree(tmp);
        change = true;
        return ir;
    }
    return ir;
}


IR * Region::refineXor(IR * ir, bool & change)
{
    ASSERT0(ir->is_xor());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op0->isIRListEqual(op1)) {
        //xor X,X => 0
        if (getDUMgr() != NULL) {
            getDUMgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = ir;
        Type const* ty;
        TypeMgr * dm = getTypeMgr();
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = dm->getSimplexTypeEx(D_U32);
        } else {
            ty = op0->getType();
        }
        ASSERT0(ty->is_sint() || ty->is_uint());
        ir = buildImmInt(0, ty);
        copyDbx(ir, tmp, this);
        freeIRTree(tmp);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


IR * Region::refineEq(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_eq());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op0->isIRListEqual(op1) && RC_do_fold_const(rc)) {
        //eq X,X => 1
        if (getDUMgr() != NULL) {
            getDUMgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = ir;
        ir = buildImmInt(1, getTypeMgr()->getSimplexTypeEx(D_B));
        copyDbx(ir, tmp, this);
        freeIRTree(tmp);
        change = true;
        //TODO: Inform its parent stmt IR to remove use
        //of the stmt out of du-chain.
        return ir;
    }
    return ir;
}


IR * Region::refineNe(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir->is_ne());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op0->isIRListEqual(op1) && RC_do_fold_const(rc)) {
        //ne X,X => 0
        if (getDUMgr() != NULL) {
            getDUMgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = ir;
        ir = buildImmInt(0, getTypeMgr()->getSimplexTypeEx(D_B));
        copyDbx(ir, tmp, this);
        freeIRTree(tmp);
        change = true;
        //TODO: Inform its parent stmt IR to remove use
        //of the stmt out of du-chain.
        return ir;
    }
    return ir;
}


IR * Region::reassociation(IR * ir, bool & change)
{
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    IR_TYPE opt1 = ir->getCode();
    IR_TYPE opt2 = op0->getCode();

    if (op1->is_const() &&
        op1->is_int() &&
        op0->is_associative() &&
        ir->is_associative() &&
        BIN_opnd1(op0)->is_const() &&
        BIN_opnd1(op0)->is_int() &&
        getArithPrecedence(opt1) == getArithPrecedence(opt2)) {
        //If n1,n2 is int const. OP1((OP2 X,n1), n2) => OP2(X, OP1(n1,n2))
        //where OP1, OP2 must be identical precedence.
        HOST_INT v = calcIntVal(opt1,
                                CONST_int_val(BIN_opnd1(op0)),
                                CONST_int_val(op1));
        DATA_TYPE dt =
            ir->is_ptr() ?
                getTypeMgr()->getPointerSizeDtype():
                ir->is_mc() ?
                    getTypeMgr()->getDType(WORD_BITSIZE, true):
                    op1->getDType();
        IR * new_const = buildImmInt(v, getTypeMgr()->getSimplexTypeEx(dt));
        copyDbx(new_const, BIN_opnd0(ir), this);
        IR_parent(op0) = NULL;
        BIN_opnd0(ir) = NULL;
        freeIRTree(ir);
        freeIRTree(BIN_opnd1(op0));
        BIN_opnd1(op0) = new_const;
        change = true;
        op0->setParentPointer(false);
        return op0; //No need to update DU.
    }
    return ir;
}


//Refine binary operations.
IR * Region::refineBinaryOp(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(BIN_opnd0(ir) != NULL && BIN_opnd1(ir) != NULL);
    BIN_opnd0(ir) = refineIR(BIN_opnd0(ir), change, rc);
    BIN_opnd1(ir) = refineIR(BIN_opnd1(ir), change, rc);
    if (change) { ir->setParentPointer(false); }
    bool lchange = false;
    if (RC_do_fold_const(rc)) {
        ir = foldConst(ir, lchange);
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
    case IR_NE: {
        //Operation commutative: ADD(CONST, ...) => ADD(..., CONST)
        if (BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) {
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
        }
        if (BIN_opnd1(ir)->is_const()) {
            ir = reassociation(ir, lchange);
            change |= lchange;
            if (lchange) { break; }
        }

        if (ir->is_add()) { ir = refineAdd(ir, change); }
        else if (ir->is_xor()) { ir = refineXor(ir, change); }
        else if (ir->is_band()) { ir = refineBand(ir, change); }
        else if (ir->is_bor()) { ir = refineBor(ir, change); }
        else if (ir->is_mul()) { ir = refineMul(ir, change, rc); }
        else if (ir->is_eq()) { ir = refineEq(ir, change, rc); }
        else if (ir->is_ne()) { ir = refineNe(ir, change, rc); }
        break;
    }
    case IR_SUB:
        if (BIN_opnd1(ir)->is_const()) {
            ir = reassociation(ir, lchange);
            change |= lchange;
        } else {
            ir = refineSub(ir, change);
        }
        break;
    case IR_DIV:
        ir = refineDiv(ir, change, rc);
        break;
    case IR_REM:
        ir = refineRem(ir, change);
        break;
    case IR_MOD:
        ir = refineMod(ir, change);
        break;
    case IR_LAND:
        ir = refineLand(ir, change);
        break;
    case IR_LOR:
        ir = refineLor(ir, change);
        break;
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        break;
    case IR_LT:
        if (BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) {
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
            IR_code(ir) = IR_GT;
        }
        break;
    case IR_LE:
        if (BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) {
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
            IR_code(ir) = IR_GE;
        }
        break;
    case IR_GT:
        if (BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) {
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
            IR_code(ir) = IR_LT;
        }
        break;
    case IR_GE:
        if (BIN_opnd0(ir)->is_const() && !BIN_opnd1(ir)->is_const()) {
            IR * tmp = BIN_opnd0(ir);
            BIN_opnd0(ir) = BIN_opnd1(ir);
            BIN_opnd1(ir) = tmp;
            IR_code(ir) = IR_LE;
        }
        break;
    default: UNREACHABLE();
    }

    //Insert convert if need.
    if (ir->isBinaryOp() && RC_insert_cvt(rc)) {
        BIN_opnd0(ir) = insertCvt(ir, BIN_opnd0(ir), change);
        BIN_opnd1(ir) = insertCvt(ir, BIN_opnd1(ir), change);
        if (change) { ir->setParentPointer(false); }
        insertCvtForBinaryOp(ir, change);
    } else if (change) {
        ir->setParentPointer(false);
    }
    return ir;
}


IR * Region::refineStoreArray(IR * ir, bool & change, RefineCtx & rc)
{
    IR * newir = refineArray(ir, change, rc);
    CHECK_DUMMYUSE(newir == ir);

    bool lchange = false;
    IR * newrhs = refineIR(STARR_rhs(ir), lchange, rc);
    if (lchange) {
        ir->setRHS(newrhs);
        IR_parent(newrhs) = ir;
        change = lchange;
    }

    ASSERT0(!::is_redundant_cvt(newrhs));
    if (RC_refine_stmt(rc)) {
        MD const* umd = newrhs->getExactRef();
        if (umd != NULL && umd == ir->getExactRef()) {
            //Result and operand refered the same md.
            if (newrhs->is_cvt()) {
                //CASE: pr(i64) = cvt(i64, pr(i32))
                //Do NOT remove 'cvt'.
                ;
            } else {
                change = true;
                if (getDUMgr() != NULL) {
                    getDUMgr()->removeIROutFromDUMgr(ir);
                }

                IRBB * bb = ir->getBB();
                if (bb != NULL) {
                    BB_irlist(bb).remove(ir);
                    RC_stmt_removed(rc) = true;
                }

                freeIRTree(ir);
                return NULL;
            }
        }
    }
    return ir;
}


IR * Region::refineArray(IR * ir, bool & change, RefineCtx & rc)
{
    IR * newbase = refineIR(ARR_base(ir), change, rc);
    if (newbase != ARR_base(ir)) {
        ARR_base(ir) = newbase;
        IR_parent(newbase) = ir;
    }

    IR * newsublist = NULL;
    IR * last = NULL;
    IR * s = xcom::removehead(&ARR_sub_list(ir));

    for (; s != NULL;) {
        IR * newsub = refineIR(s, change, rc);
        if (newsub != s) {
            IR_parent(newsub) = ir;
        }
        add_next(&newsublist, &last, newsub);
        s = xcom::removehead(&ARR_sub_list(ir));
    }
    ARR_sub_list(ir) = newsublist;
    return ir;
}


IR * Region::refineBranch(IR * ir)
{
    if (ir->is_falsebr() && BR_det(ir)->is_ne()) {
        IR_code(ir) = IR_TRUEBR;
        IR_code(BR_det(ir)) = IR_EQ;
    }
    return ir;
}


IR * Region::refineLoad(IR * ir)
{
    ASSERT0(LD_idinfo(ir));
    VAR * var = LD_idinfo(ir);
    if (VAR_is_array(var)) {
        //Convert LD(v) to LDA(ID(v)) if ID is array.
        //I think the convert is incorrect. If var a is array,
        //then LD(a,U32) means load 32bit element from a,
        //e.g: load a[0]. So do not convert LD into LDA.
        //IR * rm = ir;
        //ir = buildLda(buildId(LD_info(ir)));
        //freeIR(rm);
    }
    return ir;
}


IR * Region::refineCvt(IR * ir, bool & change, RefineCtx & rc)
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
        CVT_exp(tmp) = NULL;
        freeIRTree(tmp);
        IR_parent(CVT_exp(ir)) = ir;
        change = true;
    }

    if (ir->getType() == CVT_exp(ir)->getType()) {
        //cvt(i64, ld(i64)) => ld(i64)
        IR * tmp = CVT_exp(ir);
        IR_parent(tmp) = IR_parent(ir);
        CVT_exp(ir) = NULL;
        freeIRTree(ir);
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
        CVT_exp(ir) = NULL;
        freeIRTree(ir);
        ir = tmp;
        change = true;
    }

    return ir;
}


IR * Region::refineDetViaSSAdu(IR * ir, bool & change)
{
    ASSERT0(ir->is_judge());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);

    SSAInfo * op0_ssainfo = op0->getSSAInfo();
    SSAInfo * op1_ssainfo = op1->getSSAInfo();

    if (op0_ssainfo == NULL || op1_ssainfo == NULL) { return ir; }

    IR const* def0 = op0_ssainfo->get_def();
    IR const* def1 = op1_ssainfo->get_def();

    if (def0 == NULL || def1 == NULL) { return ir; }

    if (!def0->is_phi() || !def1->is_phi()) { return ir; }

    //Check if operand is the same const.
    IR const* phi_opnd0 = PHI_opnd_list(def0);
    IR const* phi_opnd1 = PHI_opnd_list(def1);
    for (; phi_opnd1 != NULL && phi_opnd0 != NULL;
         phi_opnd1 = IR_next(phi_opnd1), phi_opnd0 = IR_next(phi_opnd0)) {
        if (!phi_opnd0->is_const() ||
            !phi_opnd1->is_const() ||
            CONST_int_val(phi_opnd0) != CONST_int_val(phi_opnd1)) {
            return ir;
        }
    }

    if (phi_opnd0 != NULL || phi_opnd1 != NULL) {
        //These two PHIs does not have same number of operands.
        return ir;
    }

    Type const* ty = ir->getType();

    //Reset SSA du for op0, op1.
    SSA_uses(op0_ssainfo).remove(op0);
    SSA_uses(op1_ssainfo).remove(op1);

    freeIRTree(ir);
    change = true;
    return buildImmInt(1, ty);
}


//Perform peephole optimizations.
//This function also responsible for normalizing IR and reassociation.
//NOTE: This function do NOT generate new STMT.
IR * Region::refineIR(IR * ir, bool & change, RefineCtx & rc)
{
    if (!g_do_refine) return ir;
    if (ir == NULL) return NULL;
    bool tmpc = false;
    switch (ir->getCode()) {
    case IR_CONST:
    case IR_ID:
        break;
    case IR_LD:
        ir = refineLoad(ir);
        break;
    case IR_ILD:
        ir = refineIload(ir, tmpc, rc);
        break;
    case IR_STARRAY:
        ir = refineStoreArray(ir, tmpc, rc);
        break;
    case IR_ST:
    case IR_STPR:
        ir = refineStore(ir, tmpc, rc);
        break;
    case IR_SETELEM:
        ir = refineSetelem(ir, tmpc, rc);
        break;
    case IR_GETELEM:
        ir = refineGetelem(ir, tmpc, rc);
        break;
    case IR_IST:
        ir = refineIstore(ir, tmpc, rc);
        break;
    case IR_LDA:
        break;
    case IR_CALL:
        ir = refineCall(ir, tmpc, rc);
        break;
    case IR_ICALL:
        ir = refineIcall(ir, tmpc, rc);
        break;
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        ir = refineBinaryOp(ir, tmpc, rc);
        break;
    case IR_BNOT:
    case IR_LNOT:
        ir = refineNot(ir, tmpc, rc);
        break;
    case IR_NEG:
        ir = refineNeg(ir, tmpc);
        break;
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
        //Do NOT do foldConst for conditional expr.
        //e.g: If NE(1, 0) => 1, one should generate NE(1, 0) again,
        //because of TRUEBR/FALSEBR do not accept IR_CONST.
        {
            RefineCtx t(rc);
            RC_do_fold_const(t) = false;
            ir = refineBinaryOp(ir, tmpc, t);
            if (!ir->is_const()) {
                ir = refineDetViaSSAdu(ir, tmpc);
            }
        }
        break;
    case IR_GOTO:
        break;
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
    case IR_LABEL:
        break;
    case IR_IGOTO:
        IGOTO_vexp(ir) = refineIR(IGOTO_vexp(ir), tmpc, rc);
        break;
    case IR_SWITCH:
        ir = refineSwitch(ir, tmpc, rc);
        break;
    case IR_CASE:
        break;
    case IR_ARRAY:
        ir = refineArray(ir, tmpc, rc);
        break;
    case IR_CVT:
        ir = refineCvt(ir, tmpc, rc);
        break;
    case IR_PR:
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        ir = refineBr(ir, tmpc, rc);
        break;
    case IR_RETURN:
        ir = refineReturn(ir, tmpc, rc);
        break;
    case IR_SELECT:
        ir = refineSelect(ir, tmpc, rc);
        break;
    case IR_BREAK:
    case IR_CONTINUE:
        break;
    case IR_PHI:
        ir = refinePhi(ir, tmpc, rc);
        break;
    case IR_REGION:
        break;
    default: UNREACHABLE();
    }

    if (tmpc && ir != NULL && ir->is_stmt()) {
        ir->setParentPointer(true);
    }

    change |= tmpc;
    return ir;
}


//Reshaping determinate expression.
//Only the last non-stmt expression can be reserved to perform determinating.
IR * Region::refineDet(IR * ir, bool & change, RefineCtx & rc)
{
    ASSERT0(ir);
    ir = refineIR(ir, change, rc);
    if (!ir->is_judge()) {
        IR * old = ir;
        ir = buildJudge(ir);
        copyDbx(ir, old, this);
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
//  While this function completed, IR's parent-pointer would be
//  overrided, setParentPointer() should be invoked at all.
IR * Region::refineIRlist(IR * ir_list, bool & change, RefineCtx & rc)
{
    bool lchange = true; //local flag
    while (lchange) {
        lchange = false;
        IR * new_list = NULL;
        IR * last = NULL;
        while (ir_list != NULL) {
            IR * ir = xcom::removehead(&ir_list);
            IR * newIR = refineIR(ir, lchange, rc);
            add_next(&new_list, &last, newIR);
        }
        change |= lchange;
        ir_list = new_list;
    }
    return ir_list;
}


bool Region::refineStmtList(IN OUT BBIRList & ir_list, RefineCtx & rc)
{
    if (!g_do_refine) return false;
    bool change = false;
    xcom::C<IR*> * next_ct;
    ir_list.get_head(&next_ct);
    xcom::C<IR*> * ct = next_ct;
    for (; ct != NULL; ct = next_ct) {
        IR * ir = ct->val();
        next_ct = ir_list.get_next(next_ct);

        bool tmpc = false;
        IR * newir = refineIR(ir, tmpc, rc);
        if (newir != ir) {
            if (!RC_stmt_removed(rc)) {
                //If the returned ir changed, try to remove it.
                ir_list.remove(ct);
            }

            if (newir != NULL) {
                if (next_ct != NULL) {
                    ir_list.insert_before(newir, next_ct);
                } else {
                    ir_list.append_tail(newir);
                }
            }
            tmpc = true;
        }
        change |= tmpc;
    }
    return change;
}


bool Region::refineBBlist(IN OUT BBList * ir_bb_list, RefineCtx & rc)
{
    if (!g_do_refine) { return false; }
    START_TIMER(t, "Refine IRBB list");
    bool change = false;
    xcom::C<IRBB*> * ct;
    for (ir_bb_list->get_head(&ct);
         ct != ir_bb_list->end(); ct = ir_bb_list->get_next(ct)) {
         IRBB * bb = ct->val();
        change |= refineStmtList(BB_irlist(bb), rc);
    }
    END_TIMER(t, "Refine IRBB list");
    return change;
}


void Region::insertCvtForBinaryOp(IR * ir, bool & change)
{
    ASSERT0(ir->isBinaryOp());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    if (op0->is_void() || op1->is_void()) { return; }

    if (op0->getType() == op1->getType()) {
        if (op0->is_mc()) {
            ASSERTN(TY_mc_size(op0->getType()) ==
                    TY_mc_size(op1->getType()),
                    ("invalid binop for two D_MC operands"));
        }
        return;
    }

    if (op0->is_ptr()) {
        if (op1->getTypeSize(getTypeMgr()) > op0->getTypeSize(getTypeMgr())) {
            ASSERTN(op1->getType()->is_ptr_addend() && !op1->is_ptr(),
                   ("illegal pointer arith"));
            DATA_TYPE t = getTypeMgr()->getPointerSizeDtype();
            BIN_opnd1(ir) = buildCvt(op1, getTypeMgr()->getSimplexTypeEx(t));
            copyDbx(BIN_opnd1(ir), op1, this);
            ir->setParentPointer(false);
            change = true;
            return;
        }
        return;
    }

    ASSERTN(!op1->is_ptr(), ("illegal binop for Non-pointer and Pointer"));

    //Both op0 and op1 are NOT pointer.
    if (op0->is_vec() || op1->is_vec()) {
        //ASSERT0(op0->getType() == op1->getType());
        //op0 may be vector and op1 may be MC.
        return;
    }

    //Both op0 and op1 are NOT vector type.
    TypeMgr * dm = getTypeMgr();
    Type const* type = dm->hoistDtypeForBinop(op0, op1);
    UINT dt_size = dm->get_bytesize(type);
    if (op0->getTypeSize(dm) != dt_size) {
        BIN_opnd0(ir) = buildCvt(op0, type);
        copyDbx(BIN_opnd0(ir), op0, this);
        change = true;
        ir->setParentPointer(false);
    }

    if (op1->getTypeSize(dm) != dt_size) {
        BIN_opnd1(ir) = buildCvt(op1, type);
        copyDbx(BIN_opnd1(ir), op1, this);
        change = true;
        ir->setParentPointer(false);
    }
}


//Insert CVT for float if necessary.
IR * Region::insertCvtForFloat(IR * parent, IR * kid, bool &)
{
    CHECK_DUMMYUSE(parent->is_fp() || kid->is_fp());
    return kid;
}


//Insert CVT if need.
IR * Region::insertCvt(IR * parent, IR * kid, bool & change)
{
    switch (parent->getCode()) {
    case IR_CONST:
    case IR_PR:
    case IR_ID:
    case IR_BREAK:
    case IR_CONTINUE:
        UNREACHABLE();
        return kid;
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_LAND:
    case IR_LOR:
        return kid;
    case IR_ST:
    case IR_STPR:
    case IR_LD:
    case IR_IST:
    case IR_ILD:
    case IR_LDA:
    case IR_CALL:
    case IR_ICALL:
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_GOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_LABEL:
    case IR_IGOTO:
    case IR_SWITCH:
    case IR_CASE:
    case IR_ARRAY:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_CVT:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
    case IR_SELECT:
        {
            TypeMgr * dm = getTypeMgr();
            Type const* tgt_ty = parent->getType();
            if (tgt_ty->is_void()) { return kid; }

            UINT tgt_size = parent->getTypeSize(dm);
            UINT src_size = kid->getTypeSize(dm);

            if (parent->is_vec() || kid->is_vec()) {
                //Do not do hoisting for vector type.
                ASSERTN(tgt_size == src_size, ("different size vector"));
                return kid;
            }

            if (parent->is_fp() || kid->is_fp()) {
                return insertCvtForFloat(parent, kid, change);
            }

            if (tgt_size <= src_size) {
                //Do not hoist type.
                return kid;
            }

            if (parent->is_ptr() &&
                parent->is_add() &&
                BIN_opnd0(parent)->is_ptr()) {
                //Skip pointer arithmetics.
                return kid;
            }

            if (kid->is_const() && kid->is_int()) {
                //kid is integer literal.
                if (tgt_ty->is_pointer()) {
                    IR_dt(kid) = dm->getSimplexTypeEx(dm->getPointerSizeDtype());
                } else if (tgt_ty->is_string()) {
                    IR * new_kid = buildCvt(kid, tgt_ty);
                    copyDbx(new_kid, kid, this);
                    kid = new_kid;
                } else {
                    IR_dt(kid) = tgt_ty;
                }
                change = true;
                return kid;
            }

            IR * new_kid = buildCvt(kid, parent->getType());
            copyDbx(new_kid, kid, this);
            change = true;
            return new_kid;
        }
        break;
    default:;
    }

    UNREACHABLE();
    return NULL;
}


//Make sure v0 is sign-extended if its bits length less than HOST_INT.
HOST_INT Region::calcLSRIntVal(Type const* type, HOST_INT v0, HOST_INT v1)
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


HOST_INT Region::calcIntVal(IR_TYPE ty, HOST_INT v0, HOST_INT v1)
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
    } //end switch
    return v1;
}


IR * Region::foldConstIntUnary(IR * ir, bool & change)
{
    ASSERT0(ir->isUnaryOp());
    TypeMgr * dm = getTypeMgr();
    CHECK_DUMMYUSE(dm);

    ASSERT0(UNA_opnd(ir)->is_const());
    HOST_INT v0 = CONST_int_val(UNA_opnd(ir));
    if (ir->is_neg()) {
        ASSERTN(dm->get_bytesize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = buildImmInt(-v0, ir->getType());
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir;
    } else if (ir->is_lnot()) {
        ASSERTN(dm->get_bytesize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = buildImmInt(!v0, ir->getType());
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir;
    } else if (ir->is_bnot()) {
        ASSERTN(dm->get_bytesize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = buildImmInt(~v0, ir->getType());
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir;
    }
    return ir;
}


//Fold const for binary operation.
IR * Region::foldConstIntBinary(IR * ir, bool & change)
{
    ASSERT0(ir->isBinaryOp());
    ASSERT0(BIN_opnd0(ir)->is_const());
    HOST_INT v0 = CONST_int_val(BIN_opnd0(ir));

    ASSERT0(BIN_opnd1(ir)->is_const());
    HOST_INT v1 = CONST_int_val(BIN_opnd1(ir));

    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_ASR:
    case IR_LSL:
        {
            IR * x = NULL;
            if (ir->is_bool()) {
                x = buildImmInt(calcIntVal(ir->getCode(), v0, v1),
                    getTypeMgr()->getSimplexTypeEx(D_U32));
            } else if (ir->is_fp()) {
                //The result type of binary operation is
                //float point, inserting IR_CVT.
                Type const* ty = getTypeMgr()->hoistDtypeForBinop(
                    BIN_opnd0(ir), BIN_opnd1(ir));
                x = buildCvt(buildImmInt(calcIntVal(ir->getCode(), v0, v1),
                    ty), ir->getType());
            } else {
                ASSERT0(ir->is_int());
                x = buildImmInt(calcIntVal(ir->getCode(), v0, v1),
                    ir->getType());
            }
            copyDbx(x, ir, this);
            freeIRTree(ir);
            ir = x;
            change = true;
        }
        break;
    case IR_LSR:
        {
            ASSERT0(ir->is_int());
            IR * x = buildImmInt(calcLSRIntVal(ir->getType(), v0, v1),
                ir->getType());
            copyDbx(x, ir, this);
            freeIRTree(ir);
            ir = x;
            change = true;
        }
        break;
    default: UNREACHABLE();
    }

    return ir; //No need to update DU.
}


double Region::calcFloatVal(IR_TYPE ty, double v0, double v1)
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
    } //end switch
    return v1;
}


IR * Region::foldConstFloatUnary(IR * ir, bool & change)
{
    ASSERT0(ir->isUnaryOp());
    TypeMgr * dm = getTypeMgr();
    DUMMYUSE(dm);

    if (ir->is_neg()) {
        ASSERTN(dm->get_bytesize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = buildImmFp(-CONST_fp_val(UNA_opnd(ir)), ir->getType());
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir; //No need to update DU.
    } else if (ir->is_lnot()) {
        ASSERTN(dm->get_bytesize(UNA_opnd(ir)->getType()) <= 8, ("TODO"));
        IR * oldir = ir;
        HOST_FP t = CONST_fp_val(UNA_opnd(ir));
        if (t == 0.0) {
            t = 1.0;
        } else {
            t = 0.0;
        }
        ir = buildImmFp(t, ir->getType());
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir; //No need to update DU.
    }
    return ir;
}


//Binary operation.
IR * Region::foldConstFloatBinary(IR * ir, bool & change)
{
    TypeMgr * dm = getTypeMgr();
    ASSERT0(ir->isBinaryOp());
    ASSERT0(BIN_opnd0(ir)->is_const() && BIN_opnd0(ir)->is_fp() &&
            BIN_opnd1(ir)->is_const() && BIN_opnd1(ir)->is_fp());
    double v0 = CONST_fp_val(BIN_opnd0(ir));
    double v1 = CONST_fp_val(BIN_opnd1(ir));
    INT tylen = MAX(dm->get_bytesize(BIN_opnd0(ir)->getType()),
                    dm->get_bytesize(BIN_opnd1(ir)->getType()));
    DUMMYUSE(tylen);

    ASSERTN(tylen <= 8, ("TODO"));
    IR * oldir = ir;
    bool lchange = false;
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
        ir = buildImmFp(calcFloatVal(ir->getCode(), v0, v1), ir->getType());
        copyDbx(ir, oldir, this);
        lchange = true;
        break;
    default:
        ;
    } //end switch

    if (lchange) {
        freeIRTree(oldir);
        change = true;
    }
    return ir; //No need to update DU.
}


IR * Region::foldConst(IR * ir, bool & change)
{
    bool doit = false;
    for (INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != NULL) {
            IR * new_kid = foldConst(kid, change);
            if (new_kid != kid) {
                doit = true;
                ir->setKid(i, new_kid);
            }
        }
    }

    if (doit) {
        ir->setParentPointer();
    }

    TypeMgr * dm = getTypeMgr();
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        {
            IR * t0 = BIN_opnd0(ir);
            IR * t1 = BIN_opnd1(ir);
            ASSERT0(ir->isBinaryOp());
            ASSERTN(IR_MAX_KID_NUM(ir) == 2, ("binary op"));
            ASSERTN(t0 != NULL && t1 != NULL, ("binary op"));
            if ((t0->is_const() && t0->is_fp() &&
                 t1->is_const() && t1->is_fp()) &&
                 g_is_opt_float) {
                return foldConstFloatBinary(ir, change);
            } else if (t0->is_const() && t1->is_const() &&
                       t0->is_int() && t1->is_int()) {
                return foldConstIntBinary(ir, change);
            } //end if
        }
        break;
    case IR_BNOT:
    case IR_LNOT: //Boolean logical not e.g LNOT(0x0001) = 0x0000
    case IR_NEG:
        {
            //NEG(1.0) => INT(-1.0)
            ASSERTN(IR_MAX_KID_NUM(ir) == 1, ("unary op"));
            ASSERT0(UNA_opnd(ir) != NULL);
            if (UNA_opnd(ir)->is_const() &&
                UNA_opnd(ir)->is_fp() && g_is_opt_float) {
                ir = foldConstFloatUnary(ir, change);
                if (change) { return ir; }
            } else if (UNA_opnd(ir)->is_const() && UNA_opnd(ir)->is_int()) {
                ir = foldConstIntUnary(ir, change);
                if (change) { return ir; }
            } //end if
        }
        break;
    default: return ir;
    }

    //Logical expression equvialence substitution.
    switch (ir->getCode()) {
    case IR_LT:
        {
            IR * opnd1 = BIN_opnd1(ir);
            if (BIN_opnd0(ir)->is_unsigned() &&
                opnd1->is_const() &&
                opnd1->is_int() &&
                CONST_int_val(opnd1) == 0) {
                //LT(UNSIGNED, 0) always be false.
                if (getDUMgr() != NULL) {
                    getDUMgr()->removeUseOutFromDefset(ir);
                }
                IR * x = ir;
                ir = buildImmInt(0, ir->getType());
                copyDbx(ir, x, this);
                freeIRTree(x);
                change = true;
            }
        }
        break;
    case IR_GE:
        {

            IR * opnd1 = BIN_opnd1(ir);
            if (BIN_opnd0(ir)->is_unsigned() &&
                opnd1->is_const() &&
                opnd1->is_int() &&
                CONST_int_val(opnd1) == 0) {
                //GE(UNSIGNED, 0) always be true.
                IR * x = buildImmInt(1, ir->getType());
                copyDbx(x, ir, this);
                if (getDUMgr() != NULL) {
                    getDUMgr()->removeUseOutFromDefset(ir);
                }
                freeIRTree(ir);
                ir = x;
                change = true;
            }
        }
        break;
    case IR_NE:
        {
            //address of string always not be 0x0.
            //NE(LDA(SYM), 0) -> 1
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
                IR * x = buildImmInt(1, ir->getType());
                copyDbx(x, ir, this);
                freeIRTree(ir);
                ir = x;
                change = true;
            }
        }
        break;
    case IR_EQ:
        {
            //address of string always not be 0x0.
            //EQ(LDA(SYM), 0) -> 0
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
                IR * x = buildImmInt(0, ir->getType());
                copyDbx(x, ir, this);
                freeIRTree(ir);
                ir = x;
                change = true;
            }
        }
        break;
    case IR_ASR: //>>
    case IR_LSR: //>>
        {
            IR * opnd0 = BIN_opnd0(ir);
            IR * opnd1 = BIN_opnd1(ir);
            if (opnd0->is_const() &&
                opnd0->is_int() &&
                CONST_int_val(opnd0) == 0) {
                IR * newir = buildImmInt(0, ir->getType());
                copyDbx(newir, ir, this);
                freeIRTree(ir);
                ir = newir;
                change = true;
            } else if (opnd1->is_const() &&
                       opnd1->is_int() &&
                       CONST_int_val(opnd1) == 0) {
                IR * newir = opnd0;
                BIN_opnd0(ir) = NULL;
                freeIRTree(ir);
                ir = newir;
                change = true;
            }
        }
        break;
    case IR_LSL: //<<
        {
            IR * opnd0 = BIN_opnd0(ir);
            IR * opnd1 = BIN_opnd1(ir);
            if (opnd0->is_const() &&
                opnd0->is_int() &&
                CONST_int_val(opnd0) == 0) {
                IR * newir = buildImmInt(0, ir->getType());
                copyDbx(newir, ir, this);
                freeIRTree(ir);
                ir = newir;
                change = true;
            } else if (opnd1->is_const() && opnd1->is_int()) {
                if (CONST_int_val(opnd1) == 0) {
                    //x<<0 => x
                    IR * newir = opnd0;
                    BIN_opnd0(ir) = NULL;
                    freeIRTree(ir);
                    ir = newir;
                    change = true;
                } else if (opnd0->getTypeSize(dm) == 4 &&
                           CONST_int_val(opnd1) == 32) {
                    //x<<32 => 0, x is 32bit
                    IR * newir = buildImmInt(0, ir->getType());
                    copyDbx(newir, ir, this);
                    freeIRTree(ir);
                    ir = newir;
                    change = true;
                }
            }
        }
        break;
    default:;
    }
    return ir; //No need to update DU.
}


IR * Region::StrengthReduce(IN OUT IR * ir, IN OUT bool & change)
{
    return foldConst(ir, change);
}


//User must invoke 'setParentPointer' to maintain the validation of IR tree.
void Region::invertCondition(IR ** cond)
{
    switch ((*cond)->getCode()) {
    case IR_LAND:
    case IR_LOR:
        {
            IR * parent = IR_parent(*cond);
            IR * newir = buildLogicalNot(*cond);
            copyDbx(newir, *cond, this);
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

} //namespace xoc
