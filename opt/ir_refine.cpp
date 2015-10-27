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

namespace xoc {

//Algebraic identities.
IR * Region::refineIload1(IR * ir, bool & change)
{
    ASSERT0(ir->is_ild());
    /* Convert
        ILD,ofst1
         LDA
          LD,ofst2
    =>
        LD,ofst1+ofst2
    e.g: ((*(&q))->a).b => q.a.b
    or
    Convert
        ILD,ofst
         LDA
          ID
    =>
        LD,ofst
    e.g: (&q)->s => q.s */
    IR * lda = ILD_base(ir);
    ASSERT(lda->is_lda() && LDA_ofst(lda) == 0, ("not the case"));

    //ILD offset may not 0.
    INT ild_ofst = ILD_ofst(ir);

    ASSERT0(LDA_base(lda)->is_id());
    IR * ld = buildLoad(ID_info(LDA_base(lda)), IR_dt(ir));
    copyDbx(ld, LDA_base(lda), this);

    LD_ofst(ld) += ild_ofst;
    if (get_du_mgr() != NULL) {
        //newIR is IR_LD.
        //Consider the ir->get_offset() and copying MDSet info from 'ir'.
        if (ir->get_exact_ref() == NULL) {
            ld->set_ref_md(genMDforLoad(ld), this);
        } else {
            ld->copyRef(ir, this);
        }
        get_du_mgr()->changeUse(ld, ir, get_du_mgr()->getMiscBitSetMgr());
    }
    freeIRTree(ir);
    change = true;

    //Do not need set parent.
    return ld;
}


IR * Region::refineIload2(IR * ir, bool & change)
{
    ASSERT0(ir->is_ild());
    /* Convert
        ILD
         LDA,ofst
          ID
    =>
        LD,ofst */
    IR * rm = ir;
    IR * lda = ILD_base(ir);
    ir = LDA_base(lda);
    LDA_base(lda) = NULL;

    IR_DU_MGR * dumgr = get_du_mgr();

    if (ir->is_id()) {
        IR * ld = buildLoad(ID_info(ir), IR_dt(rm));
        copyDbx(ld, ir, this);
        if (dumgr != NULL) {
            ld->copyRef(rm, this);
            dumgr->changeUse(ld, rm, dumgr->getMiscBitSetMgr());
        }
        freeIRTree(ir);
        ir = ld;
    }

    ASSERT0(ir->is_ld());
    LD_ofst(ir) += LDA_ofst(lda);
    if (dumgr != NULL) {
        MD const* md = ir->get_exact_ref();
        ASSERT0(md != NULL);

        MD tmd(*md);
        MD_ofst(&tmd) += LDA_ofst(lda);
        MD const* entry = get_md_sys()->registerMD(tmd);
        ASSERT0(MD_id(entry) > 0);

        ir->set_ref_md(entry, this);
        dumgr->computeOverlapUseMDSet(ir, true);
    }
    freeIRTree(rm);
    change = true;
    //Do not need to set parent pointer.
    return ir;
}


IR * Region::refineIload(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_ild());
    ASSERT(IR_next(ir) == NULL && IR_prev(ir) == NULL, ("TODO"));
    IR * mem_addr = ILD_base(ir);
    if (mem_addr->is_lda() && LDA_ofst(mem_addr) == 0) {
        /* Convert
                ILD,ofst1
                 LDA
                  LD,ofst2
            =>
                LD,ofst1+ofst2
            e.g: ((*(&q))->a).b => q.a.b
        or
            Convert
                ILD,ofst
                 LDA
                  ID
            =>
                LD,ofst
            e.g: (&q)->s => q.s */
        return refineIload1(ir, change);
    } else if (mem_addr->is_lda() && LDA_ofst(mem_addr) != 0) {
        /* Convert
            ILD
             LDA,ofst
              ID
        =>
            LD,ofst */
        return refineIload2(ir, change);
    } else {
        ILD_base(ir) = refineIR(mem_addr, change, rc);
        if (change) {
            IR_parent(ILD_base(ir)) = ir;
        }
    }
    return ir;
}


//NOTE: the function may change IR stmt.
IR * Region::refineLda(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_lda());
    ASSERT(IR_next(ir) == NULL && IR_prev(ir) == NULL, ("TODO"));
    if (LDA_base(ir)->is_ild()) {
        /* Convert
            LDA
             ILD
              LD,ofst
        =>
            LD,ofst
        e.g1: &(*(a.p)) => a.p


        Convert
            LDA
             ILD,ofst2
              LD,ofst1
        =>
            ADD((LD,ofst1), ofst2)
        e.g2:&(s.a->b) => LD(s.a) + ofst(b) */
        IR * newir = ILD_base(LDA_base(ir));
        ILD_base(LDA_base(ir)) = NULL;
        if (ILD_ofst(LDA_base(ir)) != 0) {
            Type const* t = get_type_mgr()->getSimplexTypeEx(D_U32);
            newir = buildBinaryOpSimp(IR_ADD, IR_dt(ir), newir,
                                 buildImmInt(ILD_ofst(LDA_base(ir)), t));
            copyDbx(newir, ir, this);
        }
        //Do not need to set parent pointer.
        freeIRTree(ir);
        change = true;
        return newir; //No need for updating DU chain and MDS.
    } else {
        LDA_base(ir) = refineIR(LDA_base(ir), change, rc);
        IR_parent(LDA_base(ir)) = ir;
    }
    return ir;
}


IR * Region::refineIstore(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_ist());
    bool t = false;
    bool lchange = false;
    IST_base(ir) = refineIR(IST_base(ir), t, rc);
    change |= t;
    lchange |= t;
    t = false;
    IST_rhs(ir) = refineIR(IST_rhs(ir), t, rc);
    change |= lchange;
    lchange |= t;

    IR * lhs = IST_base(ir);
    IR * rhs = IST_rhs(ir);
    IR_DU_MGR * dumgr = get_du_mgr();
    if (lhs->is_lda() && LDA_base(lhs)->is_id()) {
        /* Convert :
        1. IST(LDA(ID))=X to ST(ID)=X
        2. IST(LDA(ID), ofst)=X to ST(ID, ofst)=X
        3. IST(LDA(ID,ofst))=X to ST(ID, ofst)=X
        4. IST(LDA(ID,ofst1), ofst2)=X to ST(ID, ofst1+ofst2)=X */
        IR * newir = buildStore(ID_info(LDA_base(lhs)),
                                 IR_dt(ir),
                                 LDA_ofst(lhs) + IST_ofst(ir),
                                 IST_rhs(ir));
        if (dumgr != NULL) {
            copyDbx(newir, ir, this);
            newir->copyRef(ir, this);

            bool maybe_exist_expired_du = false;
            if (newir->get_ref_md() == NULL) {
                newir->set_ref_md(genMDforStore(newir), this);
                newir->cleanRefMDSet();

                //Change IST to ST may result the du chain invalid.
                //There may be USEs that would not reference the MD that ST modified.
                //e.g: p = &a; p = &b;
                //ist(p, 10), ist may def a, b
                //After change to st(a, 10), st only define a, and is not define b any more.
                maybe_exist_expired_du = true;
            }

            dumgr->changeDef(newir, ir, dumgr->getMiscBitSetMgr());

            if (maybe_exist_expired_du) {
                dumgr->removeExpiredDUForStmt(newir);
            }
        }
        IST_rhs(ir) = NULL;
        freeIR(ir);
        ir = newir;
        change = true; //Keep the result type of ST unchanged.
        rhs = ST_rhs(ir); //No need for updating DU.
    }

    if (rhs->is_lda() && LDA_base(rhs)->is_ild()) {
        ASSERT(IR_next(rhs) == NULL && IR_prev(rhs) == NULL,
                ("expression cannot be linked to chain"));
        //Convert IST(LHS, LDA(ILD(var))) => IST(LHS, var)
        ASSERT0(!ILD_base(LDA_base(rhs))->is_id());
        IR * rm = rhs;
        if (ir->is_st()) {
            ST_rhs(ir) = ILD_base(LDA_base(rhs));
        } else {
            IST_rhs(ir) = ILD_base(LDA_base(rhs));
        }
        ILD_base(LDA_base(rhs)) = NULL;
        freeIRTree(rm);
        change = true;
    }

    rhs = ir->get_rhs();
    if (rhs->is_ild() && ILD_base(rhs)->is_lda()) {
        //IST(X)=ILD(LDA(ID)) => IST(X)=LD
        IR * rm = rhs;
        IR * newrhs = buildLoad(ID_info(LDA_base(ILD_base(rhs))), IR_dt(rm));
        ir->set_rhs(newrhs);
        copyDbx(newrhs, rhs, this);
        if (dumgr != NULL) {
            ASSERT0(LDA_base(ILD_base(rhs))->get_ref_mds() == NULL);
            newrhs->set_ref_md(genMDforLoad(newrhs), this);
            dumgr->copyDUSet(newrhs, rhs);
            dumgr->changeUse(newrhs, rhs, dumgr->getMiscBitSetMgr());
        }

        ASSERT(IR_next(rhs) == NULL && IR_prev(rhs) == NULL,
                ("expression cannot be linked to chain"));
        freeIRTree(rm);
        change = true;
        rhs = newrhs;
    }

    if (RC_insert_cvt(rc)) {
        ir->set_rhs(insertCvt(ir, rhs, change));
    }

    if (change) {
        ir->setParentPointer(false);
    }

    if (lchange && dumgr != NULL) {
        ASSERT0(!dumgr->removeExpiredDU(ir));
    }
    return ir;
}


#ifdef _DEBUG_
//Return true if CVT is redundant.
static inline bool is_redundant_cvt(IR * ir)
{
    if (ir->is_cvt()) {
        if (CVT_exp(ir)->is_cvt() || IR_dt(CVT_exp(ir)) == IR_dt(ir)) {
            return true;
        }
    }
    return false;
}
#endif


IR * Region::refineStore(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_st() || ir->is_stpr());

    bool lchange = false;

    IR * rhs = ir->is_st() ? ST_rhs(ir) : STPR_rhs(ir);

    if (RC_refine_stmt(rc) &&
        rhs->is_pr() &&
        ir->is_stpr() &&
        PR_no(rhs) == STPR_no(ir)) {

        //Remove pr1 = pr1.
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeIROutFromDUMgr(ir);
        }

        IRBB * bb = ir->get_bb();
        if (bb != NULL) {
            BB_irlist(bb).remove(ir);
            RC_stmt_removed(rc) = true;
        }

        freeIRTree(ir);
        change = true;
        return NULL;
    }

    rhs = refineIR(rhs, lchange, rc);
    ir->set_rhs(rhs);

    ASSERT0(!::is_redundant_cvt(rhs));
    if (RC_refine_stmt(rc)) {
        MD const* umd = rhs->get_exact_ref();
        if (umd != NULL && umd == ir->get_exact_ref()) {
            //Result and operand refered the same md.
            if (rhs->is_cvt()) {
                //CASE: pr(i64) = cvt(i64, pr(i32))
                //Do NOT remove 'cvt'.
                ;
            } else {
                change = true;
                if (get_du_mgr() != NULL) {
                    get_du_mgr()->removeIROutFromDUMgr(ir);
                }

                IRBB * bb = ir->get_bb();
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
        ir->set_rhs(insertCvt(ir, ir->get_rhs(), lchange));
    }

    change |= lchange;

    if (lchange) {
        ir->setParentPointer(false);
        IR_DU_MGR * dumgr = get_du_mgr();
        if (dumgr != NULL) {
            ASSERT0(!dumgr->removeExpiredDU(ir));
        }
    }

    return ir;
}


IR * Region::refineCall(IR * ir, bool & change, RefineCTX & rc)
{
    bool lchange = false;
    if (CALL_param_list(ir) != NULL) {
        IR * param = removehead(&CALL_param_list(ir));
        IR * newparamlst = NULL;
        IR * last = NULL;
        while (param != NULL) {
            IR * newp = refineIR(param, lchange, rc);
            add_next(&newparamlst, &last, newp);
            last = newp;
            param = removehead(&CALL_param_list(ir));
        }
        CALL_param_list(ir) = newparamlst;
    }

    if (lchange) {
        change = true;
        ir->setParentPointer(false);
    }

    if (lchange) {
        IR_DU_MGR * dumgr = get_du_mgr();
        if (dumgr != NULL) {
            ASSERT0(!dumgr->removeExpiredDU(ir));
        }
    }
    return ir;
}


IR * Region::refineIcall(IR * ir, bool & change, RefineCTX & rc)
{
    if (ICALL_callee(ir)->is_lda() && LDA_base(ICALL_callee(ir))->is_id()) {
        ASSERT0(0); //Obsolete code and removed.

        /* Convert ICALL(LDA(ID), PARAM) to ICALL(LD, PARAM)
        IR * rm = ICALL_callee(ir);
        ICALL_callee(ir) = buildLoad(ID_info(LDA_base(ICALL_callee(ir))),
                                   IR_dt(rm));
        ASSERT(IR_next(rm) == NULL &&
                IR_prev(rm) == NULL,
                ("expression cannot be linked to chain"));
        freeIR(rm);
        ir->setParentPointer(false);
        change = true; */
    }

    refineCall(ir, change, rc);

    return ir;
}


IR * Region::refineSwitch(IR * ir, bool & change, RefineCTX & rc)
{
    bool l = false;
    SWITCH_vexp(ir) = refineIR(SWITCH_vexp(ir), l, rc);
    if (l) {
        if (get_du_mgr() != NULL) {
            ASSERT0(!get_du_mgr()->removeExpiredDU(ir));
        }
        IR_parent(SWITCH_vexp(ir)) = ir;
        change = true;
    }

    l = false;
    //SWITCH_body(ir) = refineIRlist(SWITCH_body(ir), l, rc);
    change |= l;
    return ir;
}


IR * Region::refineBr(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_cond_br());
    bool l = false;
    BR_det(ir) = refineDet(BR_det(ir), l, rc);
    ir = refineBranch(ir);
    if (l) {
        if (get_du_mgr() != NULL) {
            ASSERT0(!get_du_mgr()->removeExpiredDU(ir));
        }
        change = true;
    }
    return ir;
}


IR * Region::refineReturn(IR * ir, bool & change, RefineCTX & rc)
{
    if (RET_exp(ir) == NULL) { return ir; }

    bool lchange = false;
    RET_exp(ir) = refineIR(RET_exp(ir), lchange, rc);

    if (lchange) {
        change = true;
        ir->setParentPointer(false);
        if (get_du_mgr() != NULL) {
            ASSERT0(!get_du_mgr()->removeExpiredDU(ir));
        }
    }
    return ir;
}


//IR already has built ssa info.
IR * Region::refinePhi(IR * ir, bool & change, RefineCTX & rc)
{
    //phi(1, 1, ...) => 1
    bool all_be_same_const = true;
    IR * opnd = PHI_opnd_list(ir);
    HOST_INT val = 0;
    if (opnd->is_const() && opnd->is_int(get_type_mgr())) {
        val = CONST_int_val(opnd);
        for (opnd = IR_next(opnd); opnd != NULL; opnd = IR_next(opnd)) {
            if (opnd->is_const() && opnd->is_int(get_type_mgr()) &&
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
        IR * use = get_ir(u);
        ASSERT0(use && use->is_pr());
        IR * lit = buildImmInt(val, IR_dt(use));
        ASSERT0(IR_parent(use));
        IR_parent(use)->replaceKid(use, lit,  false);
        freeIR(use);
    }

    ssainfo->cleanDU();

    change = true;

    if (RC_refine_stmt(rc)) {
        IRBB * bb = ir->get_bb();
        ASSERT0(bb);
        BB_irlist(bb).remove(ir);
        RC_stmt_removed(rc) = true;
        freeIRTree(ir);
    }
    return NULL;
}


/* Transform ir to IR_LNOT.
Return the transformed ir if changed, or the original.

CASE1:
    st:i32 $pr6
    cvt:i32
        select:i8
            ne:bool
                $pr6:i32
                intconst:i32 0
            intconst:i8 0 true_exp
            intconst:i8 1 false_exp
to
    st:i32 $pr6
    cvt:i32
        lnot:i8
           $pr6:i32

Other analogous cases:
    b=(a==0?1:0) => b=!a
*/
static inline IR * hoistSelectToLnot(IR * ir, Region * ru)
{
    IR * det = SELECT_det(ir);
    if (det->is_ne()) {
        IR * trueexp = SELECT_trueexp(ir);
        IR * falseexp = SELECT_falseexp(ir);
        TypeMgr * dm = ru->get_type_mgr();
        if (BIN_opnd1(det)->isConstIntValueEqualTo(0, dm) &&
            trueexp->isConstIntValueEqualTo(0, dm) &&
            falseexp->isConstIntValueEqualTo(1, dm)) {
            IR * lnot = ru->buildUnaryOp(IR_LNOT, IR_dt(ir), BIN_opnd0(det));
            BIN_opnd0(det) = NULL;
            return lnot;
        }
    }

    if (det->is_eq()) {
        IR * trueexp = SELECT_trueexp(ir);
        IR * falseexp = SELECT_falseexp(ir);
        TypeMgr * dm = ru->get_type_mgr();
        if (BIN_opnd1(det)->isConstIntValueEqualTo(0, dm) &&
            trueexp->isConstIntValueEqualTo(1, dm) &&
            falseexp->isConstIntValueEqualTo(0, dm)) {
            IR * lnot = ru->buildUnaryOp(IR_LNOT, IR_dt(ir), BIN_opnd0(det));
            BIN_opnd0(det) = NULL;
            return lnot;
        }
    }

    return ir;
}


IR * Region::refineSelect(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_select());
    SELECT_det(ir) = refineDet(SELECT_det(ir), change, rc);
    SELECT_trueexp(ir) = refineIRlist(SELECT_trueexp(ir), change, rc);
    SELECT_falseexp(ir) = refineIRlist(SELECT_falseexp(ir), change, rc);
    SELECT_det(ir) = foldConst(SELECT_det(ir), change);
    IR * det = SELECT_det(ir);

    IR * gen;
    if (det->is_const() && det->is_int(get_type_mgr())) {
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
    } else if (det->is_const() && det->is_fp(get_type_mgr())) {
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
        ir->setParentPointer(false);
    }
    return ir; //No need for updating DU.
}


IR * Region::refineNeg(IR * ir, bool & change)
{
    ASSERT0(ir->is_neg());
    bool lchange = false;
    ir = foldConst(ir, lchange);
    change |= lchange;
    if (!lchange && UNA_opnd0(ir)->is_neg()) {
        //-(-x) => x
        IR * tmp = UNA_opnd0(UNA_opnd0(ir));
        UNA_opnd0(UNA_opnd0(ir)) = NULL;
        freeIRTree(ir);
        change = true;
        return tmp;
    }
    return ir;
}


//Logic not: !(0001) = 0000
//Bitwise not: !(0001) = 1110
IR * Region::refineNot(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_lnot() || ir->is_bnot());
    UNA_opnd0(ir) = refineIR(UNA_opnd0(ir), change, rc);
    if (change) {
        IR_parent(UNA_opnd0(ir)) = ir;
    }

    if (ir->is_lnot()) {
        IR * op0 = UNA_opnd0(ir);
        bool lchange = false;
        switch (IR_code(op0)) {
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
            UNA_opnd0(ir) = NULL;
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
IR * Region::refineDiv(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_div());
    IR * op1 = BIN_opnd1(ir);
    IR * op0 = BIN_opnd0(ir);
    TypeMgr * dm = get_type_mgr();
    if (op1->is_const() && op1->is_fp(dm) &&
        g_is_opt_float && !op0->is_const()) {
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
        if ((isPowerOf2(abs((INT)(fp_imm))) || isPowerOf5(fp_imm))) {
            //X/n => X*(1.0/n)
            IR_code(ir) = IR_MUL;
            CONST_fp_val(op1) = ((HOST_FP)1.0) / fp_imm;
            change = true;
            return ir;
        }
    } else if (op1->is_const() &&
               op1->is_int(dm) &&
               isPowerOf2(CONST_int_val(op1)) &&
               RC_refine_div_const(rc)) {
        //X/2 => X>>1, arith shift right.
        if (op0->is_sint(dm)) {
            IR_code(ir) = IR_ASR;
        } else if (op0->is_uint(dm)) {
            IR_code(ir) = IR_LSR;
        } else {
            ASSERT0(0);
        }
        CONST_int_val(op1) = getPowerOf2(CONST_int_val(op1));
        change = true;
        return ir; //No need for updating DU.
    } else if (op0->is_ir_equal(op1, true)) {
        //X/X => 1.
        IR * tmp = ir;
        Type const* ty;
        TypeMgr * dm = get_type_mgr();
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = dm->getSimplexTypeEx(D_U32);
        } else {
            ty = IR_dt(op0);
        }

        if (dm->is_fp(ty)) {
            ir = buildImmFp(1.0f, ty);
        } else {
            ir = buildImmInt(1, ty);
        }

        //Cut du chain for opnd0, opnd1 and their def-stmt.
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeUseOutFromDefset(tmp);
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
    CK_USE(op0);
    CK_USE(op1);

    if (op1->is_const() &&
        op1->is_int(get_type_mgr()) && CONST_int_val(op1) == 1) {
        //mod X,1 => 0
        IR * tmp = ir;
        ir = dupIRTree(op1);
        CONST_int_val(ir) = 0;
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeUseOutFromDefset(tmp);
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

    CK_USE(op0);
    CK_USE(op1);

    if (op1->is_const() && op1->is_int(get_type_mgr()) && CONST_int_val(op1) == 1) {
        //rem X,1 => 0
        IR * tmp = ir;
        ir = dupIRTree(op1);
        CONST_int_val(ir) = 0;
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeUseOutFromDefset(tmp);
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
    if (op1->is_const() && op1->is_int(get_type_mgr()) && CONST_int_val(op1) == 0) {
        //add X,0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = NULL;
        ir = op0;
        freeIRTree(tmp);
        change = true;
        return ir; //No need for updating DU.
    }
    return ir;
}


IR * Region::refineMul(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_mul());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op1->is_const() &&
        ((op1->is_fp(get_type_mgr()) && CONST_fp_val(op1) == 2.0) ||
         (op1->is_int(get_type_mgr()) && CONST_int_val(op1) == 2))) {
        //mul X,2.0 => add.fp X,X
        //mul X,2 => add.int X,X
        IR_code(ir) = IR_ADD;
        freeIRTree(BIN_opnd1(ir));
        BIN_opnd1(ir) = dupIRTree(BIN_opnd0(ir));
        if (get_du_mgr() != NULL) {
            get_du_mgr()->copyIRTreeDU(BIN_opnd1(ir),
                                            BIN_opnd0(ir), true);
        }
        ir->setParentPointer(false);
        change = true;
        return ir; //No need for updating DU.
    } else if (op1->is_const() && op1->is_int(get_type_mgr())) {
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
            if (get_du_mgr() != NULL) {
                get_du_mgr()->removeUseOutFromDefset(ir);
            }
            IR * newir = op1;
            BIN_opnd1(ir) = NULL;
            freeIRTree(ir);
            change = true;
            return newir;
        } else if (isPowerOf2(CONST_int_val(op1)) &&
                   RC_refine_mul_const(rc)) {
            //mul X,4 => lsl X,2, logical shift left.
            CONST_int_val(op1) = getPowerOf2(CONST_int_val(op1));
            IR_code(ir) = IR_LSL;
            change = true;
            return ir; //No need for updating DU.
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
    if (op1->is_const() && op1->is_int(get_type_mgr()) && CONST_int_val(op1) == -1) {
        //BAND X,-1 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = NULL;
        ir = op0;
        freeIRTree(tmp);
        change = true;
        return ir; //No need for updating DU.
    }
    return ir;
}


IR * Region::refineBor(IR * ir, bool & change)
{
    ASSERT0(ir->is_bor());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op1->is_const() && op1->is_int(get_type_mgr()) && CONST_int_val(op1) == 0) {
        //BOR X,0 => X
        IR * tmp = ir;
        BIN_opnd0(ir) = NULL;
        ir = op0;
        freeIRTree(tmp);
        change = true;
        return ir; //No need for updating DU.
    }
    return ir;
}


IR * Region::refineLand(IR * ir, bool & change)
{
    ASSERT0(ir->is_land());
    IR * op0 = BIN_opnd0(ir);
    if (op0->is_const() && op0->is_int(get_type_mgr()) && CONST_int_val(op0) == 1) {
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
    if (op0->is_const() && op0->is_int(get_type_mgr()) && CONST_int_val(op0) == 1) {
        //1 || x => 1
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeUseOutFromDefset(ir);
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
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = ir;
        Type const* ty;
        TypeMgr * dm = get_type_mgr();
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = dm->getSimplexTypeEx(D_U32);
        } else {
            ty = IR_dt(op0);
        }
        if (dm->is_fp(ty)) {
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
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = ir;
        Type const* ty;
        TypeMgr * dm = get_type_mgr();
        if (op0->is_mc() || op0->is_str() || op0->is_ptr()) {
            ty = dm->getSimplexTypeEx(D_U32);
        } else {
            ty = IR_dt(op0);
        }
        ASSERT0(dm->is_sint(ty) || dm->is_uint(ty));
        ir = buildImmInt(0, ty);
        copyDbx(ir, tmp, this);
        freeIRTree(tmp);
        change = true;
        return ir; //No need for updating DU.
    }
    return ir;
}


IR * Region::refineEq(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_eq());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op0->isIRListEqual(op1) && RC_do_fold_const(rc)) {
        //eq X,X => 1
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = ir;
        ir = buildImmInt(1, get_type_mgr()->getSimplexTypeEx(D_B));
        copyDbx(ir, tmp, this);
        freeIRTree(tmp);
        change = true;
        //TODO: Inform its parent stmt IR to remove use
        //of the stmt out of du-chain.
        return ir;
    }
    return ir;
}


IR * Region::refineNe(IR * ir, bool & change, RefineCTX & rc)
{
    ASSERT0(ir->is_ne());

    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    ASSERT0(op0 != NULL && op1 != NULL);
    if (op0->isIRListEqual(op1) && RC_do_fold_const(rc)) {
        //ne X,X => 0
        if (get_du_mgr() != NULL) {
            get_du_mgr()->removeUseOutFromDefset(ir);
        }
        IR * tmp = ir;
        ir = buildImmInt(0, get_type_mgr()->getSimplexTypeEx(D_B));
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
    IR_TYPE opt1 = ir->get_code();
    IR_TYPE opt2 = op0->get_code();

    //If n1,n2 is int const. OP1((OP2 X,n1), n2) => OP2(X, OP1(n1,n2))
    //where OP1, OP2 must be identical precedence.
    TypeMgr * dm = get_type_mgr();
    if (op1->is_const() &&
        op1->is_int(dm) &&
        op0->is_associative() &&
        ir->is_associative() &&
        BIN_opnd1(op0)->is_const() &&
        BIN_opnd1(op0)->is_int(dm) &&
        ::getArithPrecedence(opt1) == ::getArithPrecedence(opt2)) {

        HOST_INT v = calcIntVal(opt1,
                                CONST_int_val(BIN_opnd1(op0)),
                                CONST_int_val(op1));
        DATA_TYPE dt =
            ir->is_ptr() ?
                dm->getPointerSizeDtype():
                ir->is_mc() ? dm->get_dtype(WORD_BITSIZE, true):
                                ir->get_dtype();
        IR * new_const = buildImmInt(v, get_type_mgr()->getSimplexTypeEx(dt));
        copyDbx(new_const, BIN_opnd0(ir), this);
        IR_parent(op0) = NULL;
        BIN_opnd0(ir) = NULL;
        freeIRTree(ir);
        freeIRTree(BIN_opnd1(op0));
        BIN_opnd1(op0) = new_const;
        change = true;
        op0->setParentPointer(false);
        return op0; //No need for updating DU.
    }
    return ir;
}


//Refine binary operations.
IR * Region::refineBinaryOp(IR * ir, bool & change, RefineCTX & rc)
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
    switch (ir->get_code()) {
    case IR_ADD:
    case IR_MUL:
    case IR_XOR:
    case IR_BAND:
    case IR_BOR:
    case IR_EQ:
    case IR_NE:
        {
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
        }
        break;
    case IR_SUB:
        {
            if (BIN_opnd1(ir)->is_const()) {
                ir = reassociation(ir, lchange);
                change |= lchange;
            } else {
                ir = refineSub(ir, change);
            }
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
    default:
        ASSERT0(0);
    }

    //Insert convert if need.
    if (ir->is_binary_op() && RC_insert_cvt(rc)) {
        BIN_opnd0(ir) = insertCvt(ir, BIN_opnd0(ir), change);
        BIN_opnd1(ir) = insertCvt(ir, BIN_opnd1(ir), change);
        if (change) { ir->setParentPointer(false); }
        insertCvtForBinaryOp(ir, change);
    } else if (change) {
        ir->setParentPointer(false);
    }
    return ir;
}


IR * Region::refineStoreArray(IR * ir, bool & change, RefineCTX & rc)
{
    IR * newir = refineArray(ir, change, rc);
    CK_USE(newir == ir);

    bool lchange = false;
    IR * newrhs = refineIR(STARR_rhs(ir), lchange, rc);
    if (lchange) {
        ir->set_rhs(newrhs);
        IR_parent(newrhs) = ir;
        change = lchange;
    }

    ASSERT0(!::is_redundant_cvt(newrhs));
    if (RC_refine_stmt(rc)) {
        MD const* umd = newrhs->get_exact_ref();
        if (umd != NULL && umd == ir->get_exact_ref()) {
            //Result and operand refered the same md.
            if (newrhs->is_cvt()) {
                //CASE: pr(i64) = cvt(i64, pr(i32))
                //Do NOT remove 'cvt'.
                ;
            } else {
                change = true;
                if (get_du_mgr() != NULL) {
                    get_du_mgr()->removeIROutFromDUMgr(ir);
                }

                IRBB * bb = ir->get_bb();
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


IR * Region::refineArray(IR * ir, bool & change, RefineCTX & rc)
{
    IR * newbase = refineIR(ARR_base(ir), change, rc);
    if (newbase != ARR_base(ir)) {
        ARR_base(ir) = newbase;
        IR_parent(newbase) = ir;
    }

    IR * newsublist = NULL;
    IR * last = NULL;
    IR * s = removehead(&ARR_sub_list(ir));

    for (; s != NULL;) {
        IR * newsub = refineIR(s, change, rc);
        if (newsub != s) {
            IR_parent(newsub) = ir;
        }
        add_next(&newsublist, &last, newsub);
        s = removehead(&ARR_sub_list(ir));
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
        /* I think the convert is incorrect. If var a is array,
        then LD(a,U32) means load 32bit element from a,
        e.g: load a[0]. So do not convert LD into LDA.

        //IR * rm = ir;
        //ir = buildLda(buildId(LD_info(ir)));
        //freeIR(rm);
        */
    }
    return ir;
}


IR * Region::refineCvt(IR * ir, bool & change, RefineCTX & rc)
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

    if (IR_dt(ir) == IR_dt(CVT_exp(ir))) {
        //cvt(i64, ld(i64)) => ld(i64)
        IR * tmp = CVT_exp(ir);
        IR_parent(tmp) = IR_parent(ir);
        CVT_exp(ir) = NULL;
        freeIRTree(ir);
        ir = tmp;
        change = true;
    }

    if (ir->is_cvt() && CVT_exp(ir)->is_const() &&
        ((ir->is_int(get_type_mgr()) && CVT_exp(ir)->is_int(get_type_mgr())) ||
         (ir->is_fp(get_type_mgr()) && CVT_exp(ir)->is_fp(get_type_mgr())))) {
        //cvt(i64, const) => const(i64)
        IR * tmp = CVT_exp(ir);
        IR_dt(tmp) = IR_dt(ir);
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

    SSAInfo * op0_ssainfo = op0->get_ssainfo();
    SSAInfo * op1_ssainfo = op1->get_ssainfo();

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

    Type const* ty = IR_dt(ir);

    //Reset SSA du for op0, op1.
    SSA_uses(op0_ssainfo).remove(op0);
    SSA_uses(op1_ssainfo).remove(op1);

    freeIRTree(ir);
    change = true;
    return buildImmInt(1, ty);
}


/* Perform peephole optimizations.
This function also responsible for normalizing IR and reassociation.
NOTE: This function do NOT generate new STMT. */
IR * Region::refineIR(IR * ir, bool & change, RefineCTX & rc)
{
    if (!g_do_refine) return ir;
    if (ir == NULL) return NULL;
    bool tmpc = false;
    switch (ir->get_code()) {
    case IR_CONST:
    case IR_ID:
        break;
    case IR_LD:
        ir = refineLoad(ir);
        break;
    case IR_STARRAY:
        ir = refineStoreArray(ir, tmpc, rc);
        break;
     case IR_ST:
    case IR_STPR:
        ir = refineStore(ir, tmpc, rc);
         break;
    case IR_ILD:
        ir = refineIload(ir, tmpc, rc);
        break;
    case IR_IST:
        ir = refineIstore(ir, tmpc, rc);
        break;
    case IR_LDA:
        ir = refineLda(ir, tmpc, rc);
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
        /* Do NOT do foldConst for conditional expr.
        e.g: If NE(1, 0) => 1, one should generate NE(1, 0) again,
        because of TRUEBR/FALSEBR do not accept IR_CONST. */
        {
            RefineCTX t(rc);
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
    default: ASSERT0(0);
    }

    if (tmpc && ir != NULL && ir->is_stmt()) {
        ir->setParentPointer(true);
    }

    change |= tmpc;
    return ir;
}


//Reshaping determinate expression.
//Only the last non-stmt expression can be reserved to perform determinating.
IR * Region::refineDet(IR * ir, bool & change, RefineCTX & rc)
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


/* Perform amendment for IRs that via primary
convertion in order to generate legal IR tree.
1. Mergering IR node like as :
    ST(LD(v)) => ST(ID(v))

    Generating icall instead of the deref of a function-pointer:
    DEREF(CALL) => ICALL

2. Delete non-statement node from statement list.
3. Checking OFST of IR.
4. Complementing det of control-flow node
        IF(pr100, TRUE_PART, FALSE_PART) =>
        IF(NE(pr100, 0), TRUE_PART, FALSE_PART)

'ir_list': list to refine.

NOTICE:
    While this function completed, IR's parent-pointer would be
    overrided, setParentPointer() should be invoked at all. */
IR * Region::refineIRlist(IR * ir_list, bool & change, RefineCTX & rc)
{
    bool lchange = true; //local flag
    while (lchange) {
        lchange = false;
        IR * new_list = NULL;
        IR * last = NULL;
        while (ir_list != NULL) {
            IR * ir = removehead(&ir_list);
            IR * newIR = refineIR(ir, lchange, rc);
            add_next(&new_list, &last, newIR);
        }
        change |= lchange;
        ir_list = new_list;
    }
    return ir_list;
}


bool Region::refineStmtList(IN OUT BBIRList & ir_list, RefineCTX & rc)
{
    if (!g_do_refine) return false;
    bool change = false;
    C<IR*> * next_ct;
    ir_list.get_head(&next_ct);
    C<IR*> * ct = next_ct;
    for (; ct != NULL; ct = next_ct) {
        IR * ir = C_val(ct);
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


bool Region::refineBBlist(IN OUT BBList * ir_bb_list, RefineCTX & rc)
{
    if (!g_do_refine) { return false; }
    START_TIMER("Refine IRBB list");
    bool change = false;
    C<IRBB*> * ct;
    for (ir_bb_list->get_head(&ct);
         ct != ir_bb_list->end(); ct = ir_bb_list->get_next(ct)) {
         IRBB * bb = ct->val();
        change |= refineStmtList(BB_irlist(bb), rc);
    }
    END_TIMER();
    return change;
}


void Region::insertCvtForBinaryOp(IR * ir, bool & change)
{
    ASSERT0(ir->is_binary_op());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    TypeMgr * dm = get_type_mgr();
    if (IR_dt(op0) == IR_dt(op1)) {
        if (op0->is_mc()) {
            ASSERT(TY_mc_size(op0->get_type()) ==
                    TY_mc_size(op1->get_type()),
                    ("invalid binop for two D_MC operands"));
        }
        return;
    }

    if (op0->is_ptr()) {
        if (op1->get_dtype_size(get_type_mgr()) > op0->get_dtype_size(get_type_mgr())) {
            ASSERT(get_type_mgr()->is_ptr_addend(IR_dt(op1)) && !op1->is_ptr(),
                    ("illegal pointer arith"));
            DATA_TYPE t = get_type_mgr()->getPointerSizeDtype();
            BIN_opnd1(ir) = buildCvt(op1, get_type_mgr()->getSimplexTypeEx(t));
            copyDbx(BIN_opnd1(ir), op1, this);
            ir->setParentPointer(false);
            change = true;
            return;
        }
        return;
    }

    ASSERT(!op1->is_ptr(), ("illegal binop for NoNpointer and Pointer"));

    //Both op0 and op1 are NOT pointer.
    if (op0->is_vec() || op1->is_vec()) {
        ASSERT0(op0->is_type_equal(op1));
        return;
    }

    //Both op0 and op1 are NOT vector type.
    Type const* type = dm->hoistDtypeForBinop(op0, op1);
    UINT dt_size = dm->get_bytesize(type);
    if (op0->get_dtype_size(dm) != dt_size) {
        BIN_opnd0(ir) = buildCvt(op0, type);
        copyDbx(BIN_opnd0(ir), op0, this);
        change = true;
        ir->setParentPointer(false);
    }

    if (op1->get_dtype_size(dm) != dt_size) {
        BIN_opnd1(ir) = buildCvt(op1, type);
        copyDbx(BIN_opnd1(ir), op1, this);
        change = true;
        ir->setParentPointer(false);
    }
}


//Insert CVT if need.
IR * Region::insertCvt(IR * parent, IR * kid, bool & change)
{
    switch (parent->get_code()) {
    case IR_CONST:
    case IR_PR:
    case IR_ID:
    case IR_BREAK:
    case IR_CONTINUE:
        ASSERT0(0);
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
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
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
            TypeMgr * dm = get_type_mgr();
            Type const* tgt_ty = IR_dt(parent);
            if (parent->is_vec() || kid->is_vec()) {
                //Can not do CVT between vector type and general type.
                ASSERT0(parent->is_type_equal(kid));
                //Do nothing.
                return kid;
            }
            UINT tgt_size = parent->get_dtype_size(dm);
            UINT src_size = kid->get_dtype_size(dm);
            if (tgt_size <= src_size) {
                //Do nothing.
                return kid;
            }
            if (kid->is_const() && kid->is_int(dm)) {
                ASSERT0(!kid->is_mc());
                if (tgt_ty->is_pointer()) {
                    IR_dt(kid) =
                        dm->getSimplexTypeEx(dm->getPointerSizeDtype());
                } else {
                    IR_dt(kid) = tgt_ty;
                }
                change = true;
                return kid;
            }
            IR * new_kid = buildCvt(kid, IR_dt(parent));
            copyDbx(new_kid, kid, this);
            change = true;
            return new_kid;
        }
        break;
    default: ASSERT0(0);
    }
    change = true;
    return kid;
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
        v1 = ((HOST_UINT)v0) >> v1;
        break;
    case IR_LSL:
        v1 = v0 << v1;
        break;
    default:
        ASSERT0(0);
    } //end switch
    return v1;
}


IR * Region::foldConstIntUnary(IR * ir, bool & change)
{
    ASSERT0(ir->is_unary_op());
    TypeMgr * dm = get_type_mgr();
    CK_USE(dm);

    ASSERT0(UNA_opnd0(ir)->is_const());
    HOST_INT v0 = CONST_int_val(UNA_opnd0(ir));
    if (ir->is_neg()) {
        ASSERT(dm->get_bytesize(IR_dt(UNA_opnd0(ir))) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = buildImmInt(-v0, IR_dt(ir));
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir;
    } else if (ir->is_lnot()) {
        ASSERT(dm->get_bytesize(IR_dt(UNA_opnd0(ir))) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = buildImmInt(!v0, IR_dt(ir));
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir;
    } else if (ir->is_bnot()) {
        ASSERT(dm->get_bytesize(IR_dt(UNA_opnd0(ir))) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = buildImmInt(~v0, IR_dt(ir));
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
    ASSERT0(ir->is_binary_op());
    TypeMgr * dm = get_type_mgr();
    ASSERT0(BIN_opnd0(ir)->is_const());
    HOST_INT v0 = CONST_int_val(BIN_opnd0(ir));

    ASSERT0(BIN_opnd1(ir)->is_const());
    HOST_INT v1 = CONST_int_val(BIN_opnd1(ir));
    INT tylen = MAX(dm->get_bytesize(IR_dt(BIN_opnd0(ir))),
                    dm->get_bytesize(IR_dt(BIN_opnd1(ir))));
    UNUSED(tylen);

    ASSERT(tylen <= 8, ("TODO"));
    IR * oldir = ir;
    bool lchange = false;
    switch (IR_code(ir)) {
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
            IR * x = NULL;
            if (ir->is_bool()) {
                x = buildImmInt(calcIntVal(ir->get_code(), v0, v1),
                                get_type_mgr()->getSimplexTypeEx(D_U32));
            } else if (ir->is_fp(get_type_mgr())) {
                //The result type of binary operation is
                //float point, inserting IR_CVT.
                Type const* ty =
                    get_type_mgr()->hoistDtypeForBinop(BIN_opnd0(ir), BIN_opnd1(ir));

                x = buildCvt(
                        buildImmInt(
                            calcIntVal(ir->get_code(), v0, v1), ty),
                        IR_dt(ir));
            } else {
                ASSERT0(ir->is_int(get_type_mgr()));
                x = buildImmInt(
                        calcIntVal(ir->get_code(), v0, v1), IR_dt(ir));
            }
            copyDbx(x, ir, this);
            ir = x;
            lchange = true;
        }
        break;
    default: ASSERT0(0);
    } //end switch
    if (lchange) {
        freeIRTree(oldir);
        change = true;
    }
    return ir; //No need for updating DU.
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
    ASSERT0(ir->is_unary_op());
    TypeMgr * dm = get_type_mgr();
    UNUSED(dm);

    if (ir->is_neg()) {
        ASSERT(dm->get_bytesize(IR_dt(UNA_opnd0(ir))) <= 8, ("TODO"));
        IR * oldir = ir;
        ir = buildImmFp(-CONST_fp_val(UNA_opnd0(ir)), IR_dt(ir));
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir; //No need for updating DU.
    } else if (ir->is_lnot()) {
        ASSERT(dm->get_bytesize(IR_dt(UNA_opnd0(ir))) <= 8, ("TODO"));
        IR * oldir = ir;
        HOST_FP t = CONST_fp_val(UNA_opnd0(ir));
        if (t == 0.0) {
            t = 1.0;
        } else {
            t = 0.0;
        }
        ir = buildImmFp(t, IR_dt(ir));
        copyDbx(ir, oldir, this);
        freeIRTree(oldir);
        change = true;
        return ir; //No need for updating DU.
    }
    return ir;
}


//Binary operation.
IR * Region::foldConstFloatBinary(IR * ir, bool & change)
{
    TypeMgr * dm = get_type_mgr();
    ASSERT0(ir->is_binary_op());
    ASSERT0(BIN_opnd0(ir)->is_const() && BIN_opnd0(ir)->is_fp(dm) &&
             BIN_opnd1(ir)->is_const() && BIN_opnd1(ir)->is_fp(dm));
    double v0 = CONST_fp_val(BIN_opnd0(ir));
    double v1 = CONST_fp_val(BIN_opnd1(ir));
    INT tylen = MAX(dm->get_bytesize(IR_dt(BIN_opnd0(ir))),
                    dm->get_bytesize(IR_dt(BIN_opnd1(ir))));
    UNUSED(tylen);

    ASSERT(tylen <= 8, ("TODO"));
    IR * oldir = ir;
    bool lchange = false;
    switch (ir->get_code()) {
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
        ir = buildImmFp(calcFloatVal(ir->get_code(), v0, v1),
                          IR_dt(ir));
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
    return ir; //No need for updating DU.
}


IR * Region::foldConst(IR * ir, bool & change)
{
    bool doit = false;
    for(INT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->get_kid(i);
        if (kid != NULL) {
            IR * new_kid = foldConst(kid, change);
            if (new_kid != kid) {
                doit = true;
                ir->set_kid(i, new_kid);
            }
        }
    }
    if (doit) {
        ir->setParentPointer();
    }

    TypeMgr * dm = get_type_mgr();
    switch (ir->get_code()) {
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
            ASSERT0(ir->is_binary_op());
            ASSERT(IR_MAX_KID_NUM(ir) == 2, ("binary op"));
            ASSERT(t0 != NULL && t1 != NULL, ("binary op"));
            if ((t0->is_const() && t0->is_fp(dm) &&
                 t1->is_const() && t1->is_fp(dm)) &&
                 g_is_opt_float) {
                return foldConstFloatBinary(ir, change);
            } else if (t0->is_const() && t1->is_const() &&
                       t0->is_int(dm) && t1->is_int(dm)) {
                return foldConstIntBinary(ir, change);
            } //end if
        }
        break;
    case IR_BNOT:
    case IR_LNOT: //Boolean logical not e.g LNOT(0x0001) = 0x0000
    case IR_NEG:
        {
            //NEG(1.0) => INT(-1.0)
            ASSERT(IR_MAX_KID_NUM(ir) == 1, ("unary op"));
            ASSERT0(UNA_opnd0(ir) != NULL);
            if (UNA_opnd0(ir)->is_const() &&
                UNA_opnd0(ir)->is_fp(dm) && g_is_opt_float) {
                ir = foldConstFloatUnary(ir, change);
                if (change) { return ir; }
            } else if (UNA_opnd0(ir)->is_const() && UNA_opnd0(ir)->is_int(dm)) {
                ir = foldConstIntUnary(ir, change);
                if (change) { return ir; }
            } //end if
        }
        break;
    default: return ir;
    }

    //Logical expression equvialence substitution.
    switch (ir->get_code()) {
    case IR_LT:
        {
            //LT(UNSIGNED, 0) always be false.
            IR * opnd1 = BIN_opnd1(ir);
            if (!BIN_opnd0(ir)->is_signed(dm) && opnd1->is_const() &&
                opnd1->is_int(dm) && CONST_int_val(opnd1) == 0) {
                if (get_du_mgr() != NULL) {
                    get_du_mgr()->removeUseOutFromDefset(ir);
                }
                IR * x = ir;
                ir = buildImmInt(0, IR_dt(ir));
                copyDbx(ir, x, this);
                freeIRTree(x);
            }
        }
        break;
    case IR_GE:
        {
            //GE(UNSIGNED, 0) always be true.
            IR * opnd1 = BIN_opnd1(ir);
            if (!BIN_opnd0(ir)->is_signed(dm) && opnd1->is_const() &&
                opnd1->is_int(dm) && CONST_int_val(opnd1) == 0) {
                IR * x = buildImmInt(1, IR_dt(ir));
                copyDbx(x, ir, this);
                if (get_du_mgr() != NULL) {
                    get_du_mgr()->removeUseOutFromDefset(ir);
                }
                freeIRTree(ir);
                ir = x;
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
                 LDA_base(opnd0)->is_str() &&
                 opnd1->is_const() && opnd1->is_int(dm) &&
                 CONST_int_val(opnd1) == 0)
                ||
                (opnd1->is_lda() &&
                 LDA_base(opnd1)->is_str() &&
                 opnd0->is_const() &&
                 opnd0->is_int(dm) &&
                 CONST_int_val(opnd0) == 0)) {
                IR * x = buildImmInt(1, IR_dt(ir));
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
                 LDA_base(opnd0)->is_str() &&
                 opnd1->is_const() &&
                 opnd1->is_int(dm) &&
                 CONST_int_val(opnd1) == 0)
                ||
                (opnd1->is_lda() &&
                 LDA_base(opnd1)->is_str() &&
                 opnd0->is_const() &&
                 opnd0->is_int(dm) &&
                 CONST_int_val(opnd0) == 0)) {
                IR * x = buildImmInt(0, IR_dt(ir));
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
                opnd0->is_int(dm) && CONST_int_val(opnd0) == 0) {
                IR * newir = buildImmInt(0, IR_dt(ir));
                copyDbx(newir, ir, this);
                freeIRTree(ir);
                ir = newir;
                change = true;
            } else if (opnd1->is_const() &&
                       opnd1->is_int(dm) &&
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
                opnd0->is_int(dm) &&
                CONST_int_val(opnd0) == 0) {
                IR * newir = buildImmInt(0, IR_dt(ir));
                copyDbx(newir, ir, this);
                freeIRTree(ir);
                ir = newir;
                change = true;
            } else if (opnd1->is_const() && opnd1->is_int(dm)) {
                if (CONST_int_val(opnd1) == 0) {
                    //x<<0 => x
                    IR * newir = opnd0;
                    BIN_opnd0(ir) = NULL;
                    freeIRTree(ir);
                    ir = newir;
                    change = true;
                } else if (opnd0->get_dtype_size(dm) == 4 &&
                           CONST_int_val(opnd1) == 32) {
                    //x<<32 => 0, x is 32bit
                    IR * newir = buildImmInt(0, IR_dt(ir));
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
    return ir; //No need for updating DU.
}


IR * Region::StrengthReduce(IN OUT IR * ir, IN OUT bool & change)
{
    return foldConst(ir, change);
}


//User must invoke 'setParentPointer' to maintain the validation of IR tree.
void Region::invertCondition(IR ** cond)
{
    switch ((*cond)->get_code()) {
    case IR_LAND:
    case IR_LOR:
        {
            IR * parent = IR_parent(*cond);
            IR * newir = buildLogicalOp(IR_LNOT, *cond, NULL);
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
        ASSERT(0, ("TODO"));
    }
}

} //namespace xoc
