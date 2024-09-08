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
//START IVVal
//
void IVVal::dump(Region const* rg) const
{
    switch (getKind()) {
    case VAL_UNDEF:
        prt(rg, "UNDEF");
        return;
    case VAL_IS_VAR:
        prt(rg, "VAR:'%s'(MD%u)", getVar()->get_name()->getStr(),
            getMD()->id());
        return;
    case VAL_IS_INT:
        prt(rg, "INT:%d", getInt());
        return;
    case VAL_IS_FP:
        prt(rg, "FP:%f", getFP());
        return;
    case VAL_IS_EXP: {
        xcom::StrBuf tbuf(32);
        xoc::dumpIRToBuf(getExp(), rg, tbuf,
                         DumpFlag::combineIRID(IR_DUMP_DEF|IR_DUMP_KID));
        prt(rg, "EXP:%s", tbuf.getBuf());
        return;
    }
    case VAL_IS_CR:
        getCR()->dump(rg);
        return;
    default: UNREACHABLE();
    }
}


CHAR const* IVVal::dump(Region const* rg, UINT indent,
                        OUT xcom::StrBuf & buf) const
{
    class Dump : public xoc::DumpToBuf {
    public:
        Dump(Region const* rg, xcom::StrBuf & buf, UINT ind) :
            DumpToBuf(rg, buf, ind) {}
        IVVal const* ivval;
        virtual void dumpUserInfo() const override
        { ivval->dump(getRegion()); }
    };
    Dump d(rg, buf, indent);
    d.ivval = this;
    d.dump();
    return buf.getBuf();
}


void IVVal::clean(MOD ChainRecMgr & mgr)
{
    switch (getKind()) {
    case VAL_UNDEF:
    case VAL_IS_VAR:
    case VAL_IS_INT:
    case VAL_IS_FP:
        break;
    case VAL_IS_EXP:
        //CASE:do NOT free ir here, it may be placed in CFG.
        mgr.getRegion()->freeIRTree(const_cast<IR*>(IVVAL_exp(*this)));
        break;
    case VAL_IS_CR:
        IVVAL_cr(*this)->clean(mgr);
        break;
    default: UNREACHABLE();
    }
    clean();
}


bool IVVal::extractFrom(IR const* ir)
{
    ASSERT0(ir && !ir->is_undef());
    if (ir->is_const()) {
        if (ir->is_int()) {
            setToInt(CONST_int_val(ir), ir->getType());
            return true;
        }
        if (ir->is_ptr()) {
            setToInt(CONST_int_val(ir), ir->getType());
            return true;
        }
        if (ir->is_fp()) {
            setToFP(CONST_fp_val(ir), ir->getType());
            return true;
        }
        UNREACHABLE(); //TODO: support more kind of init-value.
        return false;
    }
    MD const* md = ir->getExactRef();
    if (md != nullptr) {
        setToVar(md, ir->getType());
        return true;
    }
    ASSERT0(ir->is_exp());
    setToExp(ir);
    return true;
}


//The function build IR_LD or IR_PR according to given Var in 'v'.
IR * ChainRecMgr::buildVarRef(IVVal const& v) const
{
    ASSERT0(v.is_var());
    if (v.getVar()->is_pr()) {
        return getIRMgr()->buildPRdedicated(
            v.getVar()->getPrno(), v.getDType());
    }
    return getIRMgr()->buildLoad(const_cast<Var*>(v.getVar()), v.getDType());
}


static bool doBinOpExp2CR(IR_CODE code, IR const* v0, ChainRec const& v1,
                          OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    switch (code) {
    case IR_ADD: {
        ChainRec * rescr = mgr.allocChainRec();
        mgr.doAdd(v1, v0, *rescr);
        res.setToCR(rescr, v0->getType());
        mgr.refine(res);
        return true;
    }
    case IR_SUB: {
        ChainRec * rescr = mgr.allocChainRec();
        mgr.doSub(v0, v1, *rescr);
        res.setToCR(rescr, v0->getType());
        mgr.refine(res);
        return true;
    }
    case IR_MUL: {
        ChainRec * rescr = mgr.allocChainRec();
        mgr.doMul(v1, v0, *rescr);
        res.setToCR(rescr, v0->getType());
        mgr.refine(res);
        return true;
    }
    default: ASSERTN(0, ("TODO"));
    }
    return false;
}


static bool doBinOpVar2Any(IR_CODE code, IVVal const& v0, IVVal const& v1,
                           OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(v0.is_var());
    Region * rg = mgr.getRegion();
    IR * op0 = mgr.buildVarRef(v0);
    IR * op1 = nullptr;
    switch (v1.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_VAR:
        op1 = mgr.buildVarRef(v1);
        break;
    case IVVal::VAL_IS_INT:
        op1 = rg->getIRMgr()->buildImmInt(v1.getInt(), v1.getDType());
        break;
    case IVVal::VAL_IS_FP:
        op1 = rg->getIRMgr()->buildImmFP(v1.getFP(), v1.getDType());
        break;
    case IVVal::VAL_IS_EXP:
        op1 = rg->dupIRTree(v1.getExp());
        break;
    case IVVal::VAL_IS_CR:
        mgr.record(op0);
        return doBinOpExp2CR(code, op0, *v1.getCR(), res, mgr);
    default: UNREACHABLE();
    }
    ASSERT0(IR::isBinaryOp(code));
    IR * tmpres = rg->getIRMgr()->buildBinaryOpSimp(code, v1.getDType(),
                                                    op0, op1);
    //Refinement just performed on new IR, and does not affect
    //original IR.
    PassMgr * pm = rg->getPassMgr();
    ASSERT0(pm);
    Refine * refine = (Refine*)pm->registerPass(PASS_REFINE);
    ASSERT0(refine);
    RefineCtx rc(const_cast<OptCtx*>(&mgr.getOptCtx()));
    RC_maintain_du(rc) = false;
    bool change;
    IR * resir = refine->refineIR(tmpres, change, rc);
    res.setToExp(resir);
    mgr.record(resir);
    mgr.refine(res);
    return true;
}


static bool doBinOpExp2Any(IR_CODE code, IVVal const& v0, IVVal const& v1,
                           OUT IVVal & res, ChainRecMgr & mgr)
{
    ASSERT0(v0.is_exp());
    Region * rg = mgr.getRegion();
    IR * op0 = rg->dupIRTree(v0.getExp());
    IR * op1 = nullptr;
    switch (v1.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_VAR:
        op1 = mgr.buildVarRef(v1);
        break;
    case IVVal::VAL_IS_INT:
        op1 = rg->getIRMgr()->buildImmInt(v1.getInt(), v1.getDType());
        break;
    case IVVal::VAL_IS_FP:
        op1 = rg->getIRMgr()->buildImmFP(v1.getFP(), v1.getDType());
        break;
    case IVVal::VAL_IS_EXP:
        op1 = rg->dupIRTree(v1.getExp());
        break;
    case IVVal::VAL_IS_CR:
        mgr.record(op0);
        return doBinOpExp2CR(code, op0, *v1.getCR(), res, mgr);
    default: UNREACHABLE();
    }
    ASSERT0(IR::isBinaryOp(code));
    IR * tmpres = rg->getIRMgr()->buildBinaryOpSimp(code, v1.getDType(),
                                                    op0, op1);
    //Refinement just performed on new IR, and does not affect
    //original IR.
    PassMgr * pm = rg->getPassMgr();
    ASSERT0(pm);
    Refine * refine = (Refine*)pm->registerPass(PASS_REFINE);
    ASSERT0(refine);
    RefineCtx rc(const_cast<OptCtx*>(&mgr.getOptCtx()));
    RC_maintain_du(rc) = false;
    bool change;
    IR * resir = refine->refineIR(tmpres, change, rc);
    res.setToExp(resir);
    mgr.record(resir);
    mgr.refine(res);
    return true;
}


static bool doBinOpFP2Int(IR_CODE code, IVVal const& v0, IVVal const& v1,
                          OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(v0.is_fp() && v1.is_int());
    ASSERT0(IR::isBinaryOp(code));
    Region * rg = mgr.getRegion();
    IR * op0 = rg->getIRMgr()->buildImmFP(v0.getFP(), v0.getDType());
    IR * op1 = rg->getIRMgr()->buildImmInt(v1.getInt(), v1.getDType());
    IR * tmpres = rg->getIRMgr()->buildBinaryOpSimp(
        code, v1.getDType(), op0, op1);
    //Refinement just performed on new IR, and does not affect
    //original IR.
    PassMgr * pm = rg->getPassMgr();
    ASSERT0(pm);
    Refine * refine = (Refine*)pm->registerPass(PASS_REFINE);
    ASSERT0(refine);
    RefineCtx rc(const_cast<OptCtx*>(&mgr.getOptCtx()));
    RC_maintain_du(rc) = false;
    bool change;
    IR * resir = refine->refineIR(tmpres, change, rc);
    res.setToExp(resir);
    mgr.record(resir);
    mgr.refine(res);
    return true;
}


static bool doBinOpInt2FP(IR_CODE code, IVVal const& v0, IVVal const& v1,
                          OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(IR::isBinaryOp(code));
    Region * rg = mgr.getRegion();
    PassMgr * pm = rg->getPassMgr();
    ASSERT0(pm);
    Refine * refine = (Refine*)pm->registerPass(PASS_REFINE);
    ASSERT0(refine);
    IR * op0 = rg->getIRMgr()->buildImmInt(v0.getInt(), v0.getDType());
    IR * op1 = rg->getIRMgr()->buildImmFP(v1.getFP(), v1.getDType());
    IR * tmpres = rg->getIRMgr()->buildBinaryOpSimp(
        code, v1.getDType(), op0, op1);
    bool change;
    //Refinement just performed on new IR, and does not affect
    //original IR.
    RefineCtx rc(const_cast<OptCtx*>(&mgr.getOptCtx()));
    RC_maintain_du(rc) = false;
    IR * resir = refine->refineIR(tmpres, change, rc);
    res.setToExp(resir);
    mgr.record(resir);
    mgr.refine(res);
    return true;
}


static bool doSubOrDivInt2Any(IR_CODE code, IVVal const& v0, IVVal const& v1,
                              OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(code == IR_SUB || code == IR_DIV);
    ASSERT0(v0.is_int());
    Region * rg = mgr.getRegion();
    IR * op0 = nullptr;
    IR * op1 = nullptr;
    switch (v1.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_INT:
        if (code == IR_SUB) {
            res.setToInt(v0.getInt() - v1.getInt(), v0.getDType());
        } else {
            ASSERTN(v1.getInt() != 0, ("can not DIV 0"));
            res.setToInt(v0.getInt() / v1.getInt(), v0.getDType());
        }
        return true;
    case IVVal::VAL_IS_FP:
        return doBinOpInt2FP(code, v0, v1, res, mgr);
    case IVVal::VAL_IS_VAR:
        op0 = rg->getIRMgr()->buildImmInt(v0.getInt(), v0.getDType());
        op1 = mgr.buildVarRef(v1);
        break;
    case IVVal::VAL_IS_EXP:
        op0 = rg->getIRMgr()->buildImmInt(v0.getInt(), v0.getDType());
        op1 = rg->dupIRTree(v1.getExp());
        break;
    case IVVal::VAL_IS_CR:
        if (code == IR_SUB) {
            ChainRec * rescr = mgr.allocChainRec();
            ASSERT0(res.is_undef());
            res.setToCR(rescr, v0.getDType());
            return mgr.doSub(v0, *v1.getCR(), *rescr);
        } else {
            UNREACHABLE(); //TODO
        }
        return true;
    default: UNREACHABLE();
    }
    IR * tmpres = rg->getIRMgr()->buildBinaryOpSimp(
        code, v1.getDType(), op0, op1);
    bool change;
    //Refinement just performed on new IR, and does not affect
    //original IR.
    PassMgr * pm = rg->getPassMgr();
    ASSERT0(pm);
    RefineCtx rc(const_cast<OptCtx*>(&mgr.getOptCtx()));
    RC_maintain_du(rc) = false;
    Refine * refine = (Refine*)pm->registerPass(PASS_REFINE);
    ASSERT0(refine);
    IR * resir = refine->refineIR(tmpres, change, rc);
    res.setToExp(resir);
    mgr.record(resir);
    mgr.refine(res);
    return true;
}


static bool doSubCR2Any(IVVal const& v0, IVVal const& v1,
                        OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(v0.is_cr());
    ChainRec * rescr = mgr.allocChainRec();
    switch (v1.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_INT:
    case IVVal::VAL_IS_FP:
    case IVVal::VAL_IS_VAR:
    case IVVal::VAL_IS_EXP: {
        bool succ = mgr.doSub(*v0.getCR(), v1, *rescr);
        if (!succ) { return false; }
        break;
    }
    case IVVal::VAL_IS_CR: {
        bool succ = mgr.doSub(*v0.getCR(), *v1.getCR(), *rescr);
        if (!succ) { return false; }
        break;
    }
    default: UNREACHABLE();
    }
    res.setToCR(rescr, rescr->getInit().getDType());
    mgr.refine(res);
    return true;
}


static bool doAddOrMulCR2CR(IR_CODE code, IVVal const& v0, IVVal const& v1,
                            OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(code == IR_ADD || code == IR_MUL);
    ASSERT0(v0.is_cr() && v1.is_cr());
    ASSERTN(v0.getCR()->getCode() == v1.getCR()->getCode(),
            ("v0 and v1 should be in same direction"));
    ChainRec * rescr = mgr.allocChainRec();
    bool succ = false;
    if (code == IR_ADD) {
        succ = mgr.doAdd(*v0.getCR(), *v1.getCR(), *rescr);
    } else {
        succ = mgr.doMul(*v0.getCR(), *v1.getCR(), *rescr);
    }
    if (!succ) { return false; }
    res.setToCR(rescr, rescr->getInit().getDType());
    return true;
}


static bool doAddOrMulInt2Any(IR_CODE code, IVVal const& v0, IVVal const& v1,
                              OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(ChainRec::isLinear(code) || code == IR_MUL);
    ASSERT0(v0.is_int());
    switch (v1.getKind()) {
    case IVVal::VAL_UNDEF:
        UNREACHABLE();
        return false;
    case IVVal::VAL_IS_INT:
        if (code == IR_ADD) {
            res.setToInt(v0.getInt() + v1.getInt(), v0.getDType());
        } else {
            res.setToInt(v0.getInt() * v1.getInt(), v0.getDType());
        }
        return true;
    case IVVal::VAL_IS_FP:
        return doBinOpInt2FP(code, v0, v1, res, mgr);
    case IVVal::VAL_IS_VAR:
    case IVVal::VAL_IS_EXP:
        if (code == IR_ADD) {
            return IVVal::doAdd(v1, v0, res, mgr);
        }
        return IVVal::doMul(v1, v0, res, mgr);
    case IVVal::VAL_IS_CR: {
        ChainRec * rescr = mgr.allocChainRec();
        bool succ = false;
        if (code == IR_ADD) {
            succ = mgr.doAdd(*v1.getCR(), v0, *rescr);
        } else {
            succ = mgr.doMul(*v1.getCR(), v0, *rescr);
        }
        if (!succ) { return false; }
        res.setToCR(rescr, rescr->getInit().getDType());
        return true;
    }
    default: UNREACHABLE();
    }
    return false;
}


static bool doSubOrDivFP2Any(IR_CODE code, IVVal const& v0, IVVal const& v1,
                             OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(code == IR_SUB || code == IR_DIV);
    ASSERT0(v0.is_fp());
    Region * rg = mgr.getRegion();
    IR * op0 = nullptr;
    IR * op1 = nullptr;
    switch (v1.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_INT:
        return doBinOpFP2Int(code, v0, v1, res, mgr);
    case IVVal::VAL_IS_FP:
        if (code == IR_SUB) {
            res.setToFP(v0.getFP() - v1.getFP(), v0.getDType());
        } else {
            ASSERTN(v1.getFP() != 0.0, ("can not DIV 0"));
            res.setToFP(v0.getFP() / v1.getFP(), v0.getDType());
        }
        return true;
    case IVVal::VAL_IS_VAR:
        op0 = rg->getIRMgr()->buildImmFP(v0.getFP(), v0.getDType());
        op1 = mgr.buildVarRef(v1);
        break;
    case IVVal::VAL_IS_EXP:
        op0 = rg->getIRMgr()->buildImmFP(v0.getFP(), v0.getDType());
        op1 = rg->dupIRTree(v1.getExp());
        break;
    default: UNREACHABLE();
    }
    IR * tmpres = rg->getIRMgr()->buildBinaryOpSimp(
        code, v1.getDType(), op0, op1);
    bool change;
    //Refinement just performed on new IR, and does not affect
    //original IR.
    PassMgr * pm = rg->getPassMgr();
    ASSERT0(pm);
    RefineCtx rc(const_cast<OptCtx*>(&mgr.getOptCtx()));
    RC_maintain_du(rc) = false;
    Refine * refine = (Refine*)pm->registerPass(PASS_REFINE);
    ASSERT0(refine);
    IR * resir = refine->refineIR(tmpres, change, rc);
    res.setToExp(resir);
    mgr.record(resir);
    mgr.refine(res);
    return true;
}


static bool doAddOrMulFP2Any(IR_CODE code, IVVal const& v0, IVVal const& v1,
                             OUT IVVal & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(code == IR_ADD || code == IR_MUL);
    ASSERT0(v0.is_fp());
    switch (v1.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_FP: {
        if (code == IR_ADD) {
            res.setToFP(v0.getFP() + v1.getFP(), v0.getDType());
        } else {
            res.setToFP(v0.getFP() * v1.getFP(), v0.getDType());
        }
        return true;
    }
    case IVVal::VAL_IS_INT:
    case IVVal::VAL_IS_VAR:
    case IVVal::VAL_IS_EXP:
        if (code == IR_ADD) { return IVVal::doAdd(v1, v0, res, mgr); }
        return IVVal::doMul(v1, v0, res, mgr);
    case IVVal::VAL_IS_CR: {
        ChainRec * rescr = mgr.allocChainRec();
        bool succ = false;
        if (code == IR_ADD) {
            succ = mgr.doAdd(*v1.getCR(), v0, *rescr);
        } else {
            succ = mgr.doMul(*v1.getCR(), v0, *rescr);
        }
        if (!succ) { return false; }
        res.setToCR(rescr, rescr->getInit().getDType());
        return true;
    }
    default: UNREACHABLE();
    }
    return false;
}


bool IVVal::doMul(IVVal const& v0, IVVal const& v1, OUT IVVal & res,
                  MOD ChainRecMgr & mgr)
{
    switch (v0.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_VAR:
        return doBinOpVar2Any(IR_MUL, v0, v1, res, mgr);
    case IVVal::VAL_IS_INT:
        return doAddOrMulInt2Any(IR_MUL, v0, v1, res, mgr);
    case IVVal::VAL_IS_FP:
        return doAddOrMulFP2Any(IR_MUL, v0, v1, res, mgr);
    case IVVal::VAL_IS_EXP:
        return doBinOpExp2Any(IR_MUL, v0, v1, res, mgr);
    case IVVal::VAL_IS_CR:
        if (!v1.is_cr()) { return doMul(v1, v0, res, mgr); }
        return doAddOrMulCR2CR(IR_MUL, v0, v1, res, mgr);
    default: UNREACHABLE();
    }
    return false;
}


bool IVVal::doAdd(IVVal const& v0, IVVal const& v1, OUT IVVal & res,
                  MOD ChainRecMgr & mgr)
{
    switch (v0.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_VAR:
        return doBinOpVar2Any(IR_ADD, v0, v1, res, mgr);
    case IVVal::VAL_IS_INT:
        return doAddOrMulInt2Any(IR_ADD, v0, v1, res, mgr);
    case IVVal::VAL_IS_FP:
        return doAddOrMulFP2Any(IR_ADD, v0, v1, res, mgr);
    case IVVal::VAL_IS_EXP:
        return doBinOpExp2Any(IR_ADD, v0, v1, res, mgr);
    case IVVal::VAL_IS_CR:
        if (!v1.is_cr()) { return doAdd(v1, v0, res, mgr); }
        return doAddOrMulCR2CR(IR_ADD, v0, v1, res, mgr);
    default: UNREACHABLE();
    }
    return false;
}


bool IVVal::doDiv(IVVal const& v0, IVVal const& v1, OUT IVVal & res,
                  MOD ChainRecMgr & mgr)
{
    switch (v0.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_VAR:
        return doBinOpVar2Any(IR_DIV, v0, v1, res, mgr);
    case IVVal::VAL_IS_INT:
        return doSubOrDivInt2Any(IR_DIV, v0, v1, res, mgr);
    case IVVal::VAL_IS_FP:
        return doSubOrDivFP2Any(IR_DIV, v0, v1, res, mgr);
    case IVVal::VAL_IS_EXP:
        return doBinOpExp2Any(IR_DIV, v0, v1, res, mgr);
    default: UNREACHABLE();
    }
    return false;
}


bool IVVal::doSub(IVVal const& v0, IVVal const& v1, OUT IVVal & res,
                  MOD ChainRecMgr & mgr)
{
    switch (v0.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return false;
    case IVVal::VAL_IS_VAR:
        return doBinOpVar2Any(IR_SUB, v0, v1, res, mgr);
    case IVVal::VAL_IS_INT:
        return doSubOrDivInt2Any(IR_SUB, v0, v1, res, mgr);
    case IVVal::VAL_IS_FP:
        return doSubOrDivFP2Any(IR_SUB, v0, v1, res, mgr);
    case IVVal::VAL_IS_EXP:
        return doBinOpExp2Any(IR_SUB, v0, v1, res, mgr);
    case IVVal::VAL_IS_CR:
        return doSubCR2Any(v0, v1, res, mgr);
    default: UNREACHABLE();
    }
    return false;
}


void IVVal::copyExclusive(IVVal const& src, MOD ChainRecMgr & mgr)
{
    switch (src.getKind()) {
    case IVVal::VAL_UNDEF: UNREACHABLE(); return;
    case IVVal::VAL_IS_VAR:
    case IVVal::VAL_IS_INT:
    case IVVal::VAL_IS_FP:
        *this = src;
        return;
    case IVVal::VAL_IS_EXP:
        *this = src;
        ASSERT0(src.getExp());
        setToExp(mgr.getRegion()->dupIRTreeList(src.getExp()));
        return;
    case IVVal::VAL_IS_CR: {
        *this = src;
        ChainRec * newcr = mgr.allocChainRec();
        ASSERT0(src.getCR());
        newcr->copyExclusive(*src.getCR(), mgr);
        IVVAL_cr(*this) = newcr;
        return;
    }
    default: UNREACHABLE();
    }
}


void IVVal::computeByLinRep(IVVal const& src, LinearRep const& lr,
                            MOD ChainRecMgr & mgr)
{
    clean();
    if (lr.hasCoeff()) {
        ASSERT0(lr.getCoeff());
        bool succ = extractFrom(lr.getCoeff());
        ASSERT0_DUMMYUSE(succ);
        IVVal::doMul(src, *this, *this, mgr);
    }
    if (!lr.hasAddend()) { return; }
    ASSERT0(lr.getAddend());
    IVVal addendval;
    bool succ = addendval.extractFrom(lr.getAddend());
    ASSERT0_DUMMYUSE(succ);
    if (lr.isPosAddend()) {
        IVVal::doAdd(src, addendval, *this, mgr);
        return;
    }
    if (lr.isNegAddend()) {
        IVVal::doSub(src, addendval, *this, mgr);
        return;
    }
    ASSERTN(0, ("TODO"));
}


void IVVal::setToCR(ChainRecMgr & mgr, Type const* ty)
{
    setToCR(mgr.allocChainRec(), ty);
}


bool IVVal::isEqual(IR const* v, IRMgr const* mgr) const
{
    if (!is_exp()) { return false; }
    return getExp()->isIRListEqual(v, mgr, true);
}


bool IVVal::isEqual(HOST_INT v) const
{
    switch (getKind()) {
    case VAL_IS_INT: return getInt() == v;
    case VAL_IS_FP: return TypeMgr::isEqual(HOST_FP(v), getFP());
    case VAL_IS_EXP: return getExp()->isConstIntValueEqualTo(v);
    default:;
    }
    return false;
}


bool IVVal::isEqual(HOST_FP v) const
{
    switch (getKind()) {
    case VAL_IS_FP: return TypeMgr::isEqual(v, getFP());
    case VAL_IS_INT: return TypeMgr::isEqual(v, HOST_FP(getInt()));
    case VAL_IS_EXP: return getExp()->isConstFPValueEqualTo(v);
    default:;
    }
    return false;
}


bool IVVal::isEqual(IVVal const& v, IRMgr const* mgr) const
{
    if (getKind() != v.getKind()) { return false; }
    switch (v.getKind()) {
    case IVVal::VAL_IS_VAR:
        return getMD() == v.getMD();
    case IVVal::VAL_IS_INT:
        return getInt() == v.getInt();
    case IVVal::VAL_IS_FP:
        return TypeMgr::isEqual(getFP(), v.getFP());
    case IVVal::VAL_IS_EXP:
        return getExp()->isIREqual(v.getExp(), mgr, true);
    case IVVal::VAL_IS_CR: {
        return getCR()->isEqual(*v.getCR(), mgr);
    }
    default: UNREACHABLE();
    }
    return false;
}
//END IVVal


//
//START ChainRec
//
void ChainRec::dump(Region const* rg) const
{
    note(rg, "\nChainRec(id:%u):", id());
    rg->getLogMgr()->incIndent(2);
    note(rg, "\nINIT:");
    const_cast<ChainRec*>(this)->getInit().dump(rg);
    note(rg, "\nCODE:%s", IRCNAME(getCode()));
    note(rg, "\nSTEP:");
    const_cast<ChainRec*>(this)->getStep().dump(rg);
    rg->getLogMgr()->decIndent(2);
}


void ChainRec::setCodeByIV(IV const* iv)
{
    if (iv->is_biv()) {
        BIV const* biv = (BIV const*)iv;
        if (biv->getIncDir() == IV::DIR_POS) { CR_code(this) = IR_ADD; return; }
        if (biv->getIncDir() == IV::DIR_NEG) { CR_code(this) = IR_SUB; return; }
        //Set CR code to ADD if BIV does not have any direction information.
        CR_code(this) = IR_ADD;
        return;
    }
    ASSERTN(iv->is_div(), ("iv is DIV"));
    DIV const* div = (DIV const*)iv;
    CR_code(this) = div->getChainRec()->getCode();
}


void ChainRec::setCodeByStep()
{
    IVVal const& step = getStep();
    if (step.is_int()) {
        if (step.getInt() >= 0) {
            CR_code(this) = IR_ADD;
        } else {
            CR_code(this) = IR_SUB;
        }
        return;
    }
    if (step.is_fp()) {
        if (step.getFP() >= 0.0) {
            CR_code(this) = IR_ADD;
        } else {
            CR_code(this) = IR_SUB;
        }
        return;
    }
    //Do not know whether chainrec is increase or decrease, default set to ADD.
    CR_code(this) = IR_ADD;
}


void ChainRec::extractFromLoopInvariant(IR const* ir, ChainRecMgr const& mgr)
{
    ASSERT0(ir->is_exp());
    CR_init(this).setToExp(ir);
    CR_step(this).setToInt(0, mgr.computeDefaultIntType(ir->getType()));
    CR_code(this) = IR_ADD;
    ASSERT0(isSanity());
}


void ChainRec::extractFrom(DIV const* iv)
{
    CR_init(this) = DIV_initv(iv);
    CR_step(this) = DIV_stepv(iv);
    setCodeByStep();
    ASSERT0(isSanity());
}


void ChainRec::extractFrom(BIV const* iv)
{
    //Set CR code to ADD if given BIV does not have direction information.
    CR_code(this) = iv->isInc() ? IR_ADD : iv->isDec() ? IR_SUB : IR_ADD;
    CR_init(this) = BIV_initv(iv);
    CR_step(this) = BIV_stepv(iv);
    ASSERT0(isSanity());
}


void ChainRec::clean(MOD ChainRecMgr & mgr)
{
    CR_init(this).clean(mgr);
    CR_step(this).clean(mgr);
    CR_code(this) = IR_UNDEF;
    //DO NOT CLEAN CRID.
}


void ChainRec::copyExclusive(ChainRec const& src, MOD ChainRecMgr & mgr)
{
    //DO NOT COPY CRID.
    CR_code(this) = CR_code(&src);
    CR_init(this).copyExclusive(src.getInit(), mgr);
    CR_step(this).copyExclusive(src.getStep(), mgr);
}


void ChainRec::extractFrom(IV const* iv)
{
    if (iv->is_biv()) { extractFrom((BIV const*)iv); return; }
    ASSERT0(iv->is_div());
    extractFrom((DIV const*)iv);
    ASSERT0(isSanity());
}


void ChainRec::dumpComputedValue(MOD ChainRecMgr & mgr) const
{
    Region const* rg = mgr.getRegion();
    IVValVec valvec;
    if (!mgr.computeValue(*this, 7, valvec)) {
        note(rg, "\nCAN NOT COMPUTE CHAIN-REC VALUE");
        return;
    }
    note(rg, "\nCOMPUTED CHAIN-REC VALUE:");
    xcom::StrBuf tmp(32);
    {
        DumpBufferSwitch buff(rg->getLogMgr());
        for (UINT i = 0; i < valvec.get_elem_count(); i++) {
            IVVal const& v = valvec.get(i);
            note(rg, "\n");
            v.dump(rg);
        }
        xcom::StrBuf * buf = rg->getLogMgr()->getBuffer();
        ASSERT0(buf);
        tmp.copy(*buf);
        rg->getLogMgr()->cleanBuffer();
    }
    rg->getLogMgr()->incIndent(2);
    note(rg, tmp.getBuf());
    mgr.getRegion()->getLogMgr()->decIndent(2);
}


bool ChainRec::isEqual(ChainRec const& src, IRMgr const* mgr) const
{
    if (getCode() != src.getCode()) { return false; }
    if (!getInit().isEqual(src.getInit(), mgr)) { return false; }
    if (!getStep().isEqual(src.getStep(), mgr)) { return false; }
    return true;
}


bool ChainRec::isEqual(ConstIVValList const& lst, IRMgr const* mgr) const
{
    ConstIVValListIter it;
    ChainRec const* s = this;
    UINT i = 0;
    for (lst.get_head(&it); i < lst.get_elem_count();
         it = lst.get_next(it), i++) {
        ASSERT0(it->val());
        if (!s->getInit().isEqual(*it->val(), mgr)) { return false; }
        if (!s->getStep().is_cr()) {
            break; //meet the last level CR.
        }
        s = s->getStep().getCR();
    }
    if (i != lst.get_elem_count() - 2) { return false; }
    it = lst.get_next(it);
    ASSERT0(it != lst.end());
    ASSERT0(it->val());
    return s->getStep().isEqual(*it->val(), mgr);
}


bool ChainRec::isEqual(IRMgr const* mgr, UINT valnum, ...) const
{
    va_list ptr;
    va_start(ptr, valnum);
    ConstIVValList vlst;
    for (UINT i = 0; i < valnum; i++) {
        IVVal const* val = va_arg(ptr, IVVal const*);
        vlst.append_tail(val);
    }
    va_end(ptr);
    return isEqual(vlst, mgr);
}


void ChainRec::computeByLinRep(LinearRep const& lr, ChainRec const& src,
                               MOD ChainRecMgr & mgr)
{
    ASSERT0(src.isSanity());
    IVVal coeff;
    if (lr.hasCoeff()) {
        ASSERT0(lr.getCoeff());
        bool succ = coeff.extractFrom(lr.getCoeff());
        ASSERT0_DUMMYUSE(succ);
    } else {
        coeff.setToInt(1, src.getInit().getDType());
    }
    bool succ = mgr.doMul(src, coeff, *this);
    ASSERT0_DUMMYUSE(succ);
    if (!lr.hasAddend()) {
        ASSERT0(isSanity());
        return;
    }
    ASSERT0(lr.getAddend());
    IVVal addendval;
    bool succ2 = addendval.extractFrom(lr.getAddend());
    ASSERT0_DUMMYUSE(succ2);
    if (lr.isPosAddend()) {
        bool succ = mgr.doAdd(*this, addendval, *this);
        ASSERT0_DUMMYUSE(succ);
        ASSERT0(isSanity());
        return;
    }
    if (lr.isNegAddend()) {
        bool succ = mgr.doSub(*this, addendval, *this);
        ASSERT0_DUMMYUSE(succ);
        ASSERT0(isSanity());
        return;
    }
    UNREACHABLE();
}
//END ChainRec


//
//START ChainRecMgr
//
ChainRecMgr::ChainRecMgr(Region * rg, OptCtx const* oc) : m_rg(rg), m_oc(oc)
{
    m_pool = smpoolCreate(sizeof(ChainRec) * 4, MEM_COMM);
    m_tm = rg->getTypeMgr();
    m_irmgr = rg->getIRMgr();
    m_cr_count = CRID_UNDEF;
}


ChainRecMgr::~ChainRecMgr()
{
    for (IR * ir = m_irlst.get_head();
         ir != nullptr; ir = m_irlst.get_next()) {
        if (ir->is_undef()) { continue; }
        m_rg->freeIRTree(ir);
    }
    smpoolDelete(m_pool);
}


ChainRec * ChainRecMgr::allocChainRec()
{
    ChainRec * cr = (ChainRec*)xmalloc(sizeof(ChainRec));
    CR_id(cr) = ++m_cr_count;
    return cr;
}
bool ChainRecMgr::doAdd(ChainRec const& cr0, ChainRec const& cr1,
                        OUT ChainRec & res)
{
    if (cr0.getCode() != IR_ADD || cr1.getCode() != IR_ADD) { return false; }
    if (!IVVal::doAdd(cr0.getInit(), cr1.getInit(), CR_init(&res), *this)) {
        return false;
    }
    if (!IVVal::doAdd(cr0.getStep(), cr1.getStep(), CR_step(&res), *this)) {
        return false;
    }
    CR_code(&res) = IR_ADD;
    refine(res);
    return true;
}


bool ChainRecMgr::doAdd(ChainRec const& cr1, IVVal const& val,
                        OUT ChainRec & res)
{
    if (!cr1.isLinear()) { return false; }
    if (!IVVal::doAdd(val, cr1.getInit(), CR_init(&res), *this)) {
        return false;
    }
    CR_step(&res) = cr1.getStep();
    CR_code(&res) = cr1.getCode();
    refine(res);
    return true;
}


bool ChainRecMgr::doSub(IVVal const& v, ChainRec const& cr1, OUT ChainRec & res)
{
    if (cr1.getCode() != IR_ADD) { return false; }
    if (!IVVal::doSub(v, cr1.getInit(), CR_init(&res), *this)) {
        return false;
    }
    CR_step(&res) = cr1.getStep();
    CR_code(&res) = cr1.getCode();
    refine(res);
    return true;
}


bool ChainRecMgr::doSub(IR const* ir, ChainRec const& cr1, OUT ChainRec & res)
{
    IR * neg = m_irmgr->buildImmInt(-1, m_tm->getHostIntType());
    bool succ = doMul(cr1, neg, res);
    if (!succ) { return false; }
    succ = doAdd(res, ir, res);
    m_rg->freeIRTree(neg);
    if (!succ) { return false; }
    refine(res);
    return true;
}


bool ChainRecMgr::doSub(ChainRec const& cr1, IVVal const& v, OUT ChainRec & res)
{
    if (cr1.getCode() != IR_ADD) { return false; }
    if (!IVVal::doSub(cr1.getInit(), v, CR_init(&res), *this)) {
        return false;
    }
    CR_step(&res) = cr1.getStep();
    CR_code(&res) = cr1.getCode();
    refine(res);
    return true;
}


bool ChainRecMgr::doSub(ChainRec const& cr0, ChainRec const& cr1,
                        OUT ChainRec & res)
{
    if (cr0.getCode() != IR_ADD || cr1.getCode() != IR_ADD) { return false; }
    if (!IVVal::doSub(cr0.getInit(), cr1.getInit(), CR_init(&res), *this)) {
        return false;
    }
    if (!IVVal::doSub(cr0.getStep(), cr1.getStep(), CR_step(&res), *this)) {
        return false;
    }
    CR_code(&res) = IR_ADD;
    refine(res);
    return true;
}


static bool doMulByCodeAdd(ChainRec const& cr0, ChainRec const& cr1,
                           OUT ChainRec & res, MOD ChainRecMgr & mgr)
{
    ASSERT0(cr0.getCode() == IR_ADD && cr1.getCode() == IR_ADD);
    //e.g:{x,+,y}*{a,+,b} ==> {x*a,+,{x,+,y}*b +y*{a,+,b}+y*b}
    IVVal const& x = cr0.getInit();
    IVVal const& y = cr0.getStep();
    IVVal const& a = cr1.getInit();
    IVVal const& b = cr1.getStep();
    IVVal res_init;
    if (!IVVal::doMul(x, a, res_init, mgr)) { return false; }
    ChainRec tmpcr1;
    if (b.is_cr()) {
        if (!mgr.doMul(cr0, *b.getCR(), tmpcr1)) { return false; }
    } else {
        if (!mgr.doMul(cr0, b, tmpcr1)) { return false; }
    }
    ChainRec tmpcr2;
    if (y.is_cr()) {
        if (!mgr.doMul(cr1, *y.getCR(), tmpcr2)) { return false; }
    } else {
        if (!mgr.doMul(cr1, y, tmpcr2)) { return false; }
    }
    IVVal tmpval3;
    if (!IVVal::doMul(y, b, tmpval3, mgr)) { return false; }
    if (!mgr.doAdd(tmpcr1, tmpcr2, tmpcr2)) { return false; }
    ChainRec * stepcr = mgr.allocChainRec();
    if (tmpval3.is_cr()) {
        if (!mgr.doAdd(tmpcr2, *tmpval3.getCR(), *stepcr)) { return false; }
    } else {
        if (!mgr.doAdd(tmpcr2, tmpval3, *stepcr)) { return false; }
    }
    ASSERT0(stepcr->getCode() != IR_UNDEF);
    CR_init(&res) = res_init;
    CR_code(&res) = IR_ADD;
    CR_step(&res).setToCR(stepcr, res_init.getDType());
    mgr.refine(res);
    return true;
}


static bool doMulByCodeMul(ChainRec const& cr0, ChainRec const& cr1,
                           OUT ChainRec & res, MOD ChainRecMgr & mgr)
{
    UNREACHABLE(); //TODO
    mgr.refine(res);
    return true;
}


bool ChainRecMgr::doMul(ChainRec const& cr0, ChainRec const& cr1,
                        OUT ChainRec & res)
{
    if (cr0.getCode() == IR_ADD && cr1.getCode() == IR_ADD) {
        return doMulByCodeAdd(cr0, cr1, res, *this);
    }
    if (cr0.getCode() == IR_MUL && cr1.getCode() == IR_MUL) {
        return doMulByCodeMul(cr0, cr1, res, *this);
    }
    return false;
}


bool ChainRecMgr::doMul(ChainRec const& cr1, IVVal const& val,
                        OUT ChainRec & res)
{
    if (!cr1.isLinear() && cr1.getCode() != IR_MUL) { return false; }
    if (!IVVal::doMul(val, cr1.getInit(), CR_init(&res), *this)) {
        return false;
    }
    if (cr1.isLinear()) {
        if (!IVVal::doMul(val, cr1.getStep(), CR_step(&res), *this)) {
            return false;
        }
    }
    CR_code(&res) = cr1.getCode();
    return true;
}


bool ChainRecMgr::computeInit(ChainRec const& cr, UINT num,
                              OUT IVValVec & valvec)
{
    if (cr.getInit().is_cr()) {
        return computeValue(*cr.getInit().getCR(), num, valvec);
    }
    valvec.set(0, cr.getInit());
    return true;
}


bool ChainRecMgr::computeStepByCR(ChainRec const& cr, UINT num,
                                  OUT IVValVec & valvec)
{
    ASSERT0(cr.getStep().is_cr());
    IVValVec tmpvalvec;
    if (!computeValue(*cr.getStep().getCR(), num - 1, tmpvalvec)) {
        return false;
    }
    IVVal v = valvec.get(0); //get init value
    ASSERT0(!v.is_cr());
    for (UINT i = 0; i < num - 1; i++) {
        IVVal const& s = tmpvalvec.get(i);
        ASSERT0(!s.is_cr());
        switch (cr.getCode()) {
        case IR_ADD:
            IVVal::doAdd(v, s, v, *this);
            break;
        case IR_SUB:
            IVVal::doSub(v, s, v, *this);
            break;
        case IR_MUL:
            IVVal::doMul(v, s, v, *this);
            break;
        case IR_DIV:
            IVVal::doDiv(v, s, v, *this);
            break;
        default: return false;
        }
        valvec.set(i + 1, v); //update step value.
    }
    return true;
}


bool ChainRecMgr::computeStep(ChainRec const& cr, UINT num,
                              OUT IVValVec & valvec)
{
    if (cr.getStep().is_cr()) {
        return computeStepByCR(cr, num, valvec);
    }
    IVVal v = valvec.get(0); //get init value
    for (UINT i = 1; i < num; i++) {
        switch (cr.getCode()) {
        case IR_ADD:
            IVVal::doAdd(v, cr.getStep(), v, *this);
            break;
        case IR_SUB:
            IVVal::doSub(v, cr.getStep(), v, *this);
            break;
        case IR_MUL:
            IVVal::doMul(v, cr.getStep(), v, *this);
            break;
        case IR_DIV:
            IVVal::doDiv(v, cr.getStep(), v, *this);
            break;
        default: return false;
        }
        valvec.set(i, v); //update step value.
    }
    return true;
}


bool ChainRecMgr::computeValue(ChainRec const& cr, UINT num,
                               OUT IVValVec & valvec)
{
    if (!computeInit(cr, num, valvec)) { return false; }
    return computeStep(cr, num, valvec);
}


bool ChainRecMgr::refine(MOD ChainRec & cr)
{
    bool change = refine(CR_init(&cr));
    change |= refine(CR_step(&cr));
    return change;
}


bool ChainRecMgr::refine(MOD ChainRec & cr, OUT IVVal & v)
{
    bool change = refine(cr);
    if (cr.isLinear() && cr.getStep().isEqual((HOST_INT)0)) {
        v.copyExclusive(cr.getInit(), *this);
        cr.clean(*this);
        change = true;
    } else if (cr.getCode() == IR_MUL && cr.getStep().isEqual((HOST_INT)1)) {
        v.copyExclusive(cr.getInit(), *this);
        cr.clean(*this);
        change = true;
    } else if (cr.getCode() == IR_MUL && cr.getInit().isEqual((HOST_INT)0)) {
        v.setToInt(0, cr.getInit().getDType());
        cr.clean(*this);
        change = true;
    }
    return change;
}


Type const* ChainRecMgr::computeDefaultIntType(Type const* ty) const
{
    if (ty->is_int() || ty->is_fp()) {
        return m_tm->getSIntTypeWithSameSize(ty);
    }
    return m_tm->getTargMachRegisterType();
}


bool ChainRecMgr::refine(MOD IVVal & v)
{
    switch (v.getKind()) {
    case IVVal::VAL_IS_EXP: {
        IR const* exp = v.getExp();
        ASSERT0(exp);
        if (!exp->isConstExp()) { return false; }
        Type const* ty = exp->getType();
        if (exp->is_cvt()) { exp = ((CCvt*)exp)->getLeafExp(); }
        if (!exp->is_const()) { return false; }
        if (exp->is_int() && ty->is_int()) {
            v.setToInt(CONST_int_val(exp), ty);
            return true;
        }
        if (exp->is_fp() && ty->is_fp()) {
            v.setToFP(CONST_fp_val(exp), ty);
            return true;
        }
        return false;
    }
    case IVVal::VAL_IS_CR: {
        ChainRec const* cr = v.getCR();
        ASSERT0(cr);
        if (cr->getInit().isEqual(HOST_INT(0))) {
            if (cr->getCode() == IR_MUL) {
                //{0,*,X} => 0
                v.setToInt(0, cr->getInit().getDType());
                return true;
            }
            if (cr->getCode() == IR_ADD &&
                cr->getStep().isEqual(HOST_INT(0))) {
                //{0,+,0} => 0
                //{0.0,+,0} => 0.0
                //{EXP(0),+,0} => EXP(0)
                v.copyExclusive(cr->getInit(), *this);
                return true;
            }
        }
        if (cr->getInit().isEqual(HOST_FP(0.0))) {
            if (cr->getCode() == IR_MUL) {
                //{0.0,*,X} => 0.0
                v.copyExclusive(cr->getInit(), *this);
                return true;
            }
            if (cr->getCode() == IR_ADD &&
                (cr->getStep().isEqual(HOST_FP(0.0)) ||
                 cr->getStep().isEqual(HOST_INT(0)))) {
                //{0.0,+,0} => 0.0
                v.copyExclusive(cr->getInit(), *this);
                return true;
            }
        }
        if (cr->getCode() == IR_ADD && cr->getStep().isEqual(HOST_INT(0))) {
            //{X,+,0} => X
            v.copyExclusive(cr->getInit(), *this);
            return true;
        }
        if (cr->getCode() == IR_MUL && cr->getStep().isEqual(HOST_INT(1))) {
            //{X,*,1} => X
            v.copyExclusive(cr->getInit(), *this);
            return true;
        }
        return false;
    }
    default: return false;
    }
    return false;
}
//END ChainRecMgr

} //namespace xoc
