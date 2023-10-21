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

//
//START LinearRep
//
HOST_INT LinearRep::getIntCoeff() const
{
    ASSERT0(hasIntCoeff());
    return CONST_int_val(coeff);
}


HOST_FP LinearRep::getFPCoeff() const
{
    ASSERT0(hasFPCoeff());
    return CONST_fp_val(coeff);
}


bool LinearRep::hasFPCoeff() const
{
    if (coeff == nullptr) { return false; }
    ASSERT0(coeff->is_const());
    return coeff->is_fp();
}


bool LinearRep::isCoeffEqualTo(HOST_INT v) const
{
    if (coeff == nullptr) { return false; }
    ASSERT0(coeff->is_const());
    if (!coeff->is_int()) { return false; }
    return CONST_int_val(coeff) == v;
}


void LinearRep::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    UINT const ind = 4;

    //Print coefficent.
    if (coeff != nullptr) {
        note(rg, "\nCOEFF:");
        rg->getLogMgr()->incIndent(ind);
        dumpIR(coeff, rg, nullptr, IR_DUMP_KID);
        rg->getLogMgr()->decIndent(ind);
    } else { note(rg, "\nNOCOEFF"); }

    //Print variable.
    if (var_exp != nullptr) {
        note(rg, "\n");
        prt(rg, "VAR:");
        rg->getLogMgr()->incIndent(ind);
        dumpIR(var_exp, rg, nullptr, IR_DUMP_KID);
        rg->getLogMgr()->decIndent(ind);
    } else { note(rg, "\nNOVAR"); }

    //Print addend.
    if (addend != nullptr) {
        ASSERT0(addend_sign != ADDEND_SIGN_UNDEF);
        note(rg, "\n%s ADDEND:", addend_sign == ADDEND_SIGN_POS ? "+" : "-");
        rg->getLogMgr()->incIndent(ind);
        dumpIR(addend, rg, nullptr, IR_DUMP_KID);
        rg->getLogMgr()->decIndent(ind);
    } else { note(rg, "\nNOADDEND"); }
}
//END LinearRep


class FindUniqueRef : public VisitIRTree {
    UINT m_ref_cnt;
    IR const* m_ref;
protected:
    virtual bool visitIR(IR const* ir)
    {
        if (ir == m_ref) {
            m_ref_cnt++;
        }
        if (m_ref_cnt > 1) {
            setTerminate();
            return false;
        }
        return true;
    }
public:
    FindUniqueRef(IR const* ir, IR const* ref)
    {
        m_ref_cnt = 0;
        m_ref = ref;
        visit(ir);
    }
    UINT getRefCount() const { return m_ref_cnt; }
};


//
//START LinearRepMgr
//
template <class RESTY, class OP0TY, class OP1TY>
RESTY calcImm(IR_CODE code, OP0TY a, OP1TY b)
{
    switch (code) {
    case IR_ADD: return (RESTY)a + (RESTY)b;
    case IR_SUB: return (RESTY)a - (RESTY)b;
    case IR_MUL: return (RESTY)a * (RESTY)b;
    default: ASSERTN(0, ("TODO"));
    }
    return 0;
}


LinearRepMgr::LinearRepMgr(Region * rg, OptCtx const& oc) : m_rg(rg), m_oc(oc)
{
    m_tm = rg->getTypeMgr();
    m_irmgr = rg->getIRMgr();
    m_refine = rg->getPassMgr() != nullptr ?
        (Refine*)rg->getPassMgr()->registerPass(PASS_REFINE) : nullptr;
}


void LinearRepMgr::clean()
{
    for (IR * ir = m_gened_ir_list.get_head(); ir != nullptr;
         ir = m_gened_ir_list.get_next()) {
        m_rg->freeIRTree(ir);
    }
    m_gened_ir_list.clean();
}


IR const* LinearRepMgr::buildConstBinOp(IR_CODE code, IR const* op0,
                                        IR const* op1)
{
    ASSERT0(op0->is_const() && op1->is_const());
    ASSERT0(op0->is_int() || op0->is_fp());
    ASSERT0(op1->is_int() || op1->is_fp());
    if (op0->is_int() && op1->is_int()) {
        //INT a = INT c + INT b
        Type const* resty = m_tm->hoistDTypeForBinOp(op0, op1);
        IR * res = m_irmgr->buildImmInt(
            calcImm<HOST_INT>(code, CONST_int_val(op0), CONST_int_val(op1)),
            resty);
        add(res);
        return res;
    }
    if (op0->is_fp() && op1->is_fp()) {
        //FP a = FP c + FP b
        Type const* resty = m_tm->hoistDTypeForBinOp(op0, op1);
        IR * res = m_irmgr->buildImmFP(
            calcImm<HOST_FP>(code, CONST_fp_val(op0), CONST_fp_val(op1)),
            resty);
        add(res);
        return res;
    }
    if (op0->is_int() && op1->is_fp()) {
        //FP a = INT c + FP b
        Type const* resty = m_tm->hoistDTypeForBinOp(op0, op1);
        IR * res = m_irmgr->buildImmFP(
            calcImm<HOST_FP>(code,
                (HOST_FP)CONST_int_val(op0), CONST_fp_val(op1)),
            resty);
        add(res);
        return res;
    }
    if (op0->is_fp() && op1->is_int()) {
        //FP a = FP c + INT b
        Type const* resty = m_tm->hoistDTypeForBinOp(op0, op1);
        IR * res = m_irmgr->buildImmFP(
            calcImm<HOST_FP>(code,
                CONST_fp_val(op0), (HOST_FP)CONST_int_val(op1)),
            resty);
        add(res);
        return res;
    }
    ASSERTN(0, ("TODO"));
    return nullptr;
}


IR const* LinearRepMgr::constructAddendByMul(LinearRep const& lr0,
                                             LinearRep const& lr1,
                                             OUT LRInferCtx & ctx)
{
    if (!lr0.hasAddend() || !lr1.hasAddend()) { return nullptr; }
    IR const* a0 = lr0.getAddend();
    IR const* a1 = lr1.getAddend();
    ASSERT0(a0 && a1);
    IR const* addend = buildBinOp(IR_MUL, a0, a1);
    ASSERT0(addend);
    if (isImmEqualTo(addend, 0)) { return nullptr; }
    ctx.is_constructed = true;
    return addend;
}


IR const* LinearRepMgr::constructAddend(IR_CODE code, LinearRep const& lr0,
                                        LinearRep const& lr1,
                                        OUT LRInferCtx & ctx)
{
    if (code == IR_MUL) {
        return constructAddendByMul(lr0, lr1, ctx);
    }
    ASSERT0(IR::isBinaryOp(code));
    if (!lr0.hasAddend()) { return lr1.getAddend(); }
    if (!lr1.hasAddend()) { return lr0.getAddend(); }
    //Both lr0 and lr1 has addend.
    IR const* a0 = lr0.getAddend();
    IR const* a1 = lr1.getAddend();
    ASSERT0(a0 && a1);
    IR const* addend = buildBinOp(code, a0, a1);
    ASSERT0(addend);
    if (isImmEqualTo(addend, 0)) { return nullptr; }
    ctx.is_constructed = true;
    return addend;
}


IR const* LinearRepMgr::constructCoeffByMul(LinearRep const& lr0,
                                            LinearRep const& lr1,
                                            OUT LRInferCtx & ctx)
{
    if (lr0.hasVar() && lr1.hasVar()) {
        ASSERT0(lr0.getVar() == lr1.getVar());
        //Non-linear representation.
        return nullptr;
    }
    LinearRep const* plr0 = &lr0;
    LinearRep const* plr1 = &lr1;
    if (plr1->hasVar()) {
        //Make sure lr0 has Var.
        xcom::swap(plr0, plr1);
    }
    //lr1 does not have Var.
    IR const* coeff0 = plr0->getCoeff();
    IR const* addend1 = plr1->getAddend();
    ASSERT0(addend1);
    if (coeff0 == nullptr) {
        coeff0 = m_irmgr->buildImmInt(1, addend1->getType());
        add(const_cast<IR*>(coeff0));
    }
    IR const* coeff = buildBinOp(IR_MUL, coeff0, addend1);
    ASSERT0(coeff);
    ctx.is_constructed = true;
    return coeff;
}


IR const* LinearRepMgr::buildBinOp(IR_CODE code, IR const* c0, IR const* c1)
{
    ASSERT0(c0 && c1 && IR::isBinaryOp(code));
    if (c0->is_const() && c1->is_const()) {
        return buildConstBinOp(code, c0, c1);
    }
    Type const* aty = nullptr;
    if (c0->isPtr()) {
        aty = c0->getType();
    } else if (c1->isPtr()) {
        aty = c1->getType();
    } else {
        aty = m_tm->hoistDTypeForBinOp(c0, c1);
    }
    IR * res = m_irmgr->buildBinaryOpSimp(code, aty,
        m_rg->dupIRTree(c0), m_rg->dupIRTree(c1));
    OptCtx tmpoc(m_rg);
    tmpoc.copy(m_oc);
    RefineCtx refinectx(&tmpoc);
    bool change;
    res = m_refine->refineIR(res, change, refinectx);
    add(res);
    return res;
}


IR const* LinearRepMgr::constructCoeff(IR_CODE code, LinearRep const& lr0,
                                       LinearRep const& lr1,
                                       OUT LRInferCtx & ctx)
{
    if (code == IR_MUL) {
        return constructCoeffByMul(lr0, lr1, ctx);
    }
    ASSERT0(IR::isBinaryOp(code));
    if (!lr0.hasCoeff()) { return lr1.getCoeff(); }
    if (!lr1.hasCoeff()) { return lr0.getCoeff(); }
    ASSERT0(lr0.hasVar() && lr1.hasVar() && lr0.getVar() == lr1.getVar());
    //Both lr0 and lr1 has coeff.
    IR const* c0 = lr0.getCoeff();
    IR const* c1 = lr1.getCoeff();
    IR const* coeff = buildBinOp(code, c0, c1);
    ASSERT0(coeff);
    ctx.is_constructed = true;
    return coeff;
}


bool LinearRepMgr::combinLinearRep(IR_CODE code, LinearRep const& lr0,
                                   LinearRep const& lr1,
                                   OUT LinearRep & reslr,
                                   OUT LRInferCtx & ctx)
{
    if (lr0.hasVar() && lr1.hasVar() && lr0.getVar() != lr1.getVar()) {
        //Different variable.
        return false;
    }
    if (lr0.isEmpty()) {
        reslr = lr1;
        return true;
    }
    if (lr1.isEmpty()) {
        reslr = lr0;
        return true;
    }
    ASSERT0(reslr.var_exp == nullptr);
    if (lr0.hasVar() && lr1.hasVar()) {
        //CASE: a*x+b combines c*x+d.
        //      a*x+b combines c*x.
        //      a*x combines c*x+d.
        //      a*x combines c*x.
        if (!isLinearCombine(code)) { return false; }
        IR const* coeff = constructCoeff(code, lr0, lr1, ctx);
        ASSERT0(coeff);
        ASSERT0(reslr.coeff == nullptr);
        reslr.coeff = coeff;
        IR const* addend = constructAddend(code, lr0, lr1, ctx);
        if (addend != nullptr) {
            ASSERT0(reslr.addend == nullptr);
            reslr.addend = addend;
            reslr.addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    if (lr0.hasVar() || lr1.hasVar()) {
        //Always linear-combine.
        //CASE: a*x+b combines d.
        //      a*x combines d.
        //      b combines c*x+d.
        //      b combines c*x.
        reslr.var_exp = lr0.hasVar() ? lr0.getVarExp() : lr1.getVarExp();
        IR const* coeff = constructCoeff(code, lr0, lr1, ctx);

        //Neither lr0 and lr1 may have coefficent.
        ASSERT0(reslr.coeff == nullptr);
        reslr.coeff = coeff;
        IR const* addend = constructAddend(code, lr0, lr1, ctx);

        //Neither lr0 and lr1 may have addend.
        if (addend != nullptr) {
            ASSERT0(reslr.addend == nullptr);
            reslr.addend = addend;
            reslr.addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    ASSERT0(!lr0.isEmpty() && !lr1.isEmpty());
    //Always linear-combine.
    //CASE: b combines d.
    IR const* addend = constructAddend(code, lr0, lr1, ctx);
    ASSERT0(addend);
    ASSERT0(reslr.addend == nullptr);
    reslr.addend = addend;
    reslr.addend_sign = ADDEND_SIGN_POS;
    return true;
}


bool LinearRepMgr::inferConst(IR const* ir, OUT LinearRep & reslr,
                              OUT LRInferCtx & ctx)
{
    if (reslr.addend == nullptr) {
        //Regard the IR as invariant if it is not Var.
        reslr.addend = ir;
        reslr.addend_sign = ADDEND_SIGN_POS;
        return true;
    }
    return false;
}


bool LinearRepMgr::inferLinearRepByDUChain(IR const* ir, Var const* var,
                                           OUT LinearRep & reslr,
                                           OUT LRInferCtx & ctx)
{
    ASSERT0(ir->is_exp());
    IR * def = xoc::findKillingDef(ir, m_rg);
    if (def == nullptr) { return false; }
    if (ctx.li != nullptr && !ctx.li->isInsideLoop(def->getBB()->id())) {
        //ir's def is out of scope.
        return false;
    }
    ASSERT0(def->hasRHS());
    return inferAndConstructLinearRep(def->getRHS(), var, reslr, ctx);
}


bool LinearRepMgr::inferPR(IR const* ir, Var const* var, OUT LinearRep & reslr,
                           OUT LRInferCtx & ctx)
{
    ASSERT0(var);
    MD const* ref = ir->getMustRef();
    ASSERTN(ref, ("miss MD"));
    if (ref->get_base() == var) {
        ASSERT0(reslr.var_exp == nullptr);
        reslr.var_exp = ir;
        return true;
    }
    if (ctx.is_transitive && inferLinearRepByDUChain(ir, var, reslr, ctx)) {
        return true;
    }
    if (reslr.addend == nullptr) {
        //Regard the IR as invariant if it is not Var.
        reslr.addend = ir;
        reslr.addend_sign = ADDEND_SIGN_POS;
        return true;
    }
    return false;
}


bool LinearRepMgr::inferLD(IR const* ir, Var const* var, OUT LinearRep & reslr,
                           OUT LRInferCtx & ctx)
{
    ASSERT0(var);
    if (ir->getIdinfo() == var) {
        ASSERT0(reslr.var_exp == nullptr);
        reslr.var_exp = ir;
        return true;
    }
    if (ctx.is_transitive && inferLinearRepByDUChain(ir, var, reslr, ctx)) {
        return true;
    }
    if (reslr.addend == nullptr) {
        //Regard the IR as invariant if it is not Var.
        reslr.addend = ir;
        reslr.addend_sign = ADDEND_SIGN_POS;
        return true;
    }
    return false;
}


bool LinearRepMgr::inferAndConstructLinearRep(IR const* ir, Var const* var,
                                              OUT LinearRep & lr,
                                              OUT LRInferCtx & ctx)
{
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB:
    case IR_MUL: {
        LinearRep op0lr;
        LinearRep op1lr;
        inferAndConstructLinearRep(BIN_opnd0(ir), var, op0lr, ctx);
        inferAndConstructLinearRep(BIN_opnd1(ir), var, op1lr, ctx);
        LinearRep reslr;
        return combinLinearRep(ir->getCode(), op0lr, op1lr, lr, ctx);
    }
    case IR_PR:
        return inferPR(ir, var, lr, ctx);
    case IR_LD:
        return inferLD(ir, var, lr, ctx);
    case IR_CONST:
         return inferConst(ir, lr, ctx);
    default: return false;
    }
}
//END LinearRepMgr

} //namespace xoc
