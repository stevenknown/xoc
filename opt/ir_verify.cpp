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

static bool verifyKidMap(IR const* ir)
{
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (IRDesc::mustExist(ir->getCode(), i)) {
            ASSERTN(k, ("IRCODE '%s' miss kid%d", IRNAME(ir), i));
        }
        if (k != nullptr) {
            ASSERT0(IR_parent(k) == ir);
        }
    }
    return true;
}


bool verifyGeneral(IR const* ir, Region const* rg)
{
    verifyKidMap(ir);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0(d && d->verify(tm));
    if (d->is_vector()) {
        TMWORD ofst = ir->getOffset();
        if (ofst != 0) {
            ASSERT0((ofst % tm->getDTypeByteSize(d->getVectorElemDType()))
                    == 0);
        }
    }
    return true;
}


bool verifyNothing(IR const* ir, Region const* rg)
{
    return true;
}


bool verifyConst(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERTN(d->getDType() != D_UNDEF, ("size of load value cannot be zero"));
    if (!ir->is_sint() &&
        !ir->is_uint() &&
        !ir->is_fp() &&
        !ir->is_bool() &&
        !ir->is_mc() &&
        !ir->is_any() &&
        !ir->is_ptr() && //immediate can be pointer, e.g: int * p = 0;
        !ir->is_str()) {
        ASSERTN(0, ("unsupport immediate value DATA_TYPE:%d", d->getDType()));
    }
    return true;
}


bool verifyLD(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    //src of LD might be small or big compare with Reg.
    ASSERT0(LD_idinfo(ir));
    Var const* var = LD_idinfo(ir);
    DUMMYUSE(var);
    ASSERTN(VAR_prno(var) == PRNO_UNDEF,
            ("VAR_prno(var) of load must be PRNO_UNDEF"));
    ASSERT0(d);
    ASSERTN(d->getDType() != D_UNDEF, ("size of load value cannot be zero"));
    return true;
}


bool verifyST(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERTN(d->getDType()!= D_UNDEF, ("size of store value cannot be zero"));
    ASSERT0(ST_idinfo(ir));
    Var const* var = ST_idinfo(ir);
    DUMMYUSE(var);
    ASSERTN(VAR_prno(var) == PRNO_UNDEF,
            ("VAR_prno(var) of store must be PRNO_UNDEF"));
    ASSERT0(ST_rhs(ir));
    ASSERT0(ST_rhs(ir)->is_exp());
    ASSERT0(ST_rhs(ir)->is_single());
    if (d->is_vector()) {
        ASSERT0(d->getVectorElemDType() != D_UNDEF);
        ASSERT0(d->getVectorElemNum(tm) > 0);
        //CASE: RHS may be the same size with ST.
        //ASSERT0(tm->getDTypeByteSize(d->getVectorElemDType()) >=
        //        tm->getByteSize(ST_rhs(ir)->getType()));
    }
    return true;
}


bool verifySTPR(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERTN(ir->getDType() != D_UNDEF, ("size of store value cannot be zero"));
    ASSERT0(ir->getDType() != D_UNDEF);
    ASSERT0(STPR_no(ir) != PRNO_UNDEF);
    ASSERT0(STPR_rhs(ir));
    ASSERT0(STPR_rhs(ir)->is_exp());
    ASSERT0(STPR_rhs(ir)->is_single());
    return true;
}


bool verifyILD(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(ir->getDType() != D_UNDEF);
    ASSERT0(ILD_base(ir));
    if (!g_is_support_dynamic_type) {
        ASSERTN(ILD_base(ir)->isPtr(), ("base must be pointer"));
        if (!ILD_base(ir)->is_any()) {
            //ANY type's base pointer size is unknown.
            ASSERT0(tm->getPointerBaseByteSize(ILD_base(ir)->getType()) > 0);
        }
    }
    ASSERT0(ILD_base(ir)->is_exp());
    ASSERT0(ILD_base(ir)->is_single());
    return true;
}


bool verifyIST(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    if (!g_is_support_dynamic_type) {
        ASSERTN(IST_base(ir)->is_ptr(), ("base must be pointer"));
        ASSERT0(tm->getPointerBaseByteSize(IST_base(ir)->getType()) > 0);
    }
    ASSERT0(IST_rhs(ir));
    ASSERT0(IST_rhs(ir)->is_exp());
    ASSERTN(ir->getDType() != D_UNDEF, ("size of istore value cannot be zero"));
    ASSERT0(IST_base(ir)->is_single());
    ASSERT0(IST_rhs(ir)->is_single());
    return true;
}


bool verifySETELEM(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(SETELEM_base(ir) && SETELEM_val(ir) && SETELEM_ofst(ir));
    if (d->is_vector()) {
        ASSERT0(d->getVectorElemDType() != D_UNDEF);

        //Note if the value size less than elemsize, it will be hoist to
        //elemsize.
        ASSERT0(tm->getDTypeByteSize(d->getVectorElemDType()) >=
                tm->getByteSize(SETELEM_val(ir)->getType()));
    }
    ASSERT0(SETELEM_base(ir)->is_exp());
    ASSERT0(SETELEM_base(ir)->is_single());
    ASSERT0(SETELEM_val(ir)->is_exp());
    ASSERT0(SETELEM_val(ir)->is_single());
    ASSERT0(SETELEM_ofst(ir)->is_exp());
    ASSERT0(SETELEM_ofst(ir)->is_single());
    return true;
}


bool verifyGETELEM(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    IR const* base = GETELEM_base(ir);
    ASSERT0(base && GETELEM_ofst(ir));
    Type const* basedtd = base->getType();
    if (basedtd->is_vector()) {
        ASSERT0(basedtd->getVectorElemDType() != D_UNDEF);
        ASSERT0(tm->getDTypeByteSize(basedtd->getVectorElemDType()) >=
                tm->getByteSize(d));
    }
    return true;
}


bool verifyLDA(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0(LDA_idinfo(ir));
    ASSERT0_DUMMYUSE(d);
    ASSERTN(d->getDType() != D_UNDEF,
            ("size of load value cannot be zero"));
    ASSERT0(d->is_pointer());
    //Lda base can be general Var, label, const string.
    ASSERTN(!LDA_idinfo(ir)->is_fake(),
            ("LDA's base must be effect Var"));
    return true;
}


bool verifyCALL(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0(CALL_idinfo(ir));

    //result type of call is the type of return value if it exist.
    //The result type may be ANY.
    if (CALL_prno(ir) != PRNO_UNDEF) { ASSERT0_DUMMYUSE(d); }

    //Parameters should be expression.
    for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
        ASSERT0(p->is_exp());
    }

    //Dummy uses should be expression.
    for (IR * p = CALL_dummyuse(ir); p != nullptr; p = p->get_next()) {
        ASSERT0(p->is_exp());
    }
    return true;
}


bool verifyICALL(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    //result type of call is the type of return value if it exist.
    //Note return-value type may be ANY.

    //rtype of icall is the type of IR in return-value-list.
    ASSERT0(ICALL_callee(ir) && ICALL_callee(ir)->isPtr());
    ASSERT0(ICALL_callee(ir)->is_single());

    for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
        ASSERT0(p->is_exp());
    }
    return true;
}


bool verifyShift(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(BIN_opnd0(ir) && BIN_opnd0(ir)->is_exp() &&
            BIN_opnd1(ir) && BIN_opnd1(ir)->is_exp());

    //Check that shift operations only have integer type.
    //Note the shift operations may be operate on vector or tensor type.
    //e.g: lsl:vec (ld:vec x, 3:u32)
    //The lsl operation shift-left each elements in x by 3 bits.
    //ASSERT0(ir->is_int()); //the result may be integer and vector type.
    //the first operand may be integer and vector type.
    //ASSERT0(BIN_opnd0(ir)->is_int());
    ASSERT0(BIN_opnd1(ir)->is_int());

    ASSERT0(BIN_opnd0(ir)->is_single());
    ASSERT0(BIN_opnd1(ir)->is_single());
    return true;
}


bool verifyADD(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(BIN_opnd0(ir) && BIN_opnd0(ir)->is_exp() &&
            BIN_opnd1(ir) && BIN_opnd1(ir)->is_exp());
    ASSERT0(BIN_opnd0(ir)->is_single());
    ASSERT0(BIN_opnd1(ir)->is_single());

    //Opnd1 can not be pointer. e.g: &p-&q
    //CASE: We should allow ir situation.
    //If opnd1 is pointer, it means the arih regard the
    //the opnd1 as an addend to pointer opnd0.
    //ASSERT0(!BIN_opnd1(ir)->is_ptr());
    return true;
}


bool verifyCompare(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(ir->is_bool() || d->getVectorElemType(tm) == tm->getBool());
    ASSERT0(BIN_opnd0(ir) && BIN_opnd0(ir)->is_exp() &&
            BIN_opnd1(ir) && BIN_opnd1(ir)->is_exp());
    ASSERT0(((CBin*)ir)->getOpnd0()->is_single());
    ASSERT0(((CBin*)ir)->getOpnd1()->is_single());
    return true;
}


bool verifyBin(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(BIN_opnd0(ir) && BIN_opnd0(ir)->is_exp() &&
            BIN_opnd1(ir) && BIN_opnd1(ir)->is_exp());
    ASSERT0(BIN_opnd0(ir)->is_single());
    ASSERT0(BIN_opnd1(ir)->is_single());
    return true;
}


bool verifyLNOT(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(ir->is_bool());
    ASSERT0(UNA_opnd(ir) && UNA_opnd(ir)->is_exp());
    ASSERT0(UNA_opnd(ir)->is_single());
    return true;
}


bool verifyUna(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(UNA_opnd(ir) && UNA_opnd(ir)->is_exp());
    ASSERT0(UNA_opnd(ir)->is_single());
    return true;
}


bool verifyGOTO(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    ASSERT0(ir->getLabel());
    return true;
}


bool verifyIGOTO(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    ASSERTN(IGOTO_vexp(ir), ("igoto vexp can not be nullptr."));
    ASSERTN(IGOTO_case_list(ir), ("igoto case list can not be nullptr."));
    ASSERTN(IGOTO_vexp(ir)->is_single(),
            ("igoto vexp can NOT be in list."));
    return true;
}


bool verifyLoopCFS(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    ASSERT0(LOOP_det(ir) && LOOP_det(ir)->is_judge());
    ASSERT0(LOOP_det(ir)->is_single());
    if (LOOP_body(ir)) {
        ASSERT0(allBeStmt(LOOP_body(ir)));
    }
    return true;
}


bool verifyIF(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    ASSERT0(IF_det(ir) && IF_det(ir)->is_judge());
    ASSERT0(IF_det(ir)->is_single());
    if (IF_truebody(ir)) {
        ASSERT0(allBeStmt(IF_truebody(ir)));
    }
    if (IF_falsebody(ir)) {
        ASSERT0(allBeStmt(IF_falsebody(ir)));
    }
    return true;
}


bool verifySWITCH(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    ASSERTN(SWITCH_vexp(ir), ("switch vexp can not be nullptr."));
    ASSERT0(SWITCH_vexp(ir)->is_exp());

    //SWITCH case list can be nullptr.
    ASSERT0(SWITCH_vexp(ir)->is_single());
    if (SWITCH_body(ir)) {
        ASSERT0(allBeStmt(SWITCH_body(ir)));
    }
    return true;
}


bool verifyCASE(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    ASSERT0(CASE_lab(ir));
    ASSERT0(CASE_vexp(ir)->is_single());
    ASSERTN(CASE_vexp(ir)->is_const(),
            ("case value-expression must be const"));
    return true;
}


bool verifyArrayOp(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(ARR_base(ir)->isPtr());
    ASSERT0(ARR_elemtype(ir));
    if (ARR_ofst(ir) != 0 && !ARR_elemtype(ir)->is_any()) {
        UINT elem_data_size = tm->getByteSize(ARR_elemtype(ir));
        UINT result_data_size = tm->getByteSize(d);
        DUMMYUSE(elem_data_size | result_data_size);
        ASSERTN(result_data_size + ARR_ofst(ir) <= elem_data_size,
                ("result data size should be less than element data size"));
    }
    ASSERT0(ARR_base(ir)->is_single());
    ASSERTN(ARR_sub_list(ir), ("subscript expression can not be null"));
    ASSERT0(allBeExp(ARR_sub_list(ir)));
    if (ir->getCode() == IR_STARRAY) {
        ASSERT0(STARR_rhs(ir));
        ASSERT0(STARR_rhs(ir)->is_exp());
        ASSERT0(STARR_rhs(ir)->is_single());
    }
    return true;
}


bool verifyCVT(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(CVT_exp(ir) != nullptr && CVT_exp(ir)->is_exp());
    ASSERT0(CVT_exp(ir)->is_single());
    return true;
}


bool verifyPR(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(ir->getDType() != D_UNDEF);
    return true;
}


bool verifyBranch(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    ASSERT0(BR_lab(ir));
    ASSERT0(BR_det(ir) && BR_det(ir)->is_judge());
    ASSERT0(BR_det(ir)->is_single());
    return true;
}


bool verifyRETURN(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    if (RET_exp(ir) != nullptr) {
        ASSERT0(RET_exp(ir)->is_exp());
        ASSERT0(RET_exp(ir)->is_single());
    }
    return true;
}


bool verifySELECT(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    //true_exp's type might not equal to false_exp's.
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d && d->getDType() != D_UNDEF);
    ASSERT0(SELECT_det(ir) &&
            SELECT_det(ir)->is_bool() &&
            SELECT_det(ir)->is_single());

    ASSERT0(SELECT_trueexp(ir) &&
            SELECT_trueexp(ir)->is_exp() &&
            SELECT_trueexp(ir)->is_single());

    ASSERT0(SELECT_falseexp(ir) &&
            SELECT_falseexp(ir)->is_exp() &&
            SELECT_falseexp(ir)->is_single());
    return true;
}


bool verifyPHI(IR const* ir, Region const* rg)
{
    verifyGeneral(ir, rg);
    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);
    Type const* d = ir->getType();
    ASSERT0_DUMMYUSE(d);
    ASSERT0(d->getDType() != D_UNDEF);
    ASSERT0(ir->getDType() != D_UNDEF);
    ASSERT0(PHI_prno(ir) != PRNO_UNDEF);

    //PHI must have at least one opnd.
    ASSERT0(PHI_opnd_list(ir) != nullptr);
    //In order to convenient to cfg optimization, do not verify PHI here,
    //whereas more verifications to PHI should be placed to PRSSAMgr.
    //ASSERT0(verifyPhi(rg));
    return true;
}

} //namespace xoc
