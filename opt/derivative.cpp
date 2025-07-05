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

static inline bool needDumpParen(IR const* ir, CalcDerivative const* calc,
                                 CalcDxCtx const& dc)
{
    return !ir->is_ld() && !ir->is_pr() && !ir->is_const();
}


//
//START TensorInfoMgr
//
void TensorInfoMgr::init()
{
    if (m_tensorinfo_pool != nullptr) { return; }
    m_tensorinfo_pool = smpoolCreate(sizeof(TensorInfo) * 2, MEM_CONST_SIZE);
}


void TensorInfoMgr::destroy()
{
    if (m_tensorinfo_pool == nullptr) { return; }
    smpoolDelete(m_tensorinfo_pool);
    m_tensorinfo_pool = nullptr;
}


TensorInfo * TensorInfoMgr::allocTensorInfo()
{
    ASSERT0(m_tensorinfo_pool);
    TensorInfo * p = (TensorInfo*)smpoolMallocConstSize(
        sizeof(TensorInfo), m_tensorinfo_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(TensorInfo));
    p->init();
    return p;
}


TensorInfo * TensorInfoMgr::getTensorInfo(IR const* ir)
{
    ASSERT0(ir && TensorInfoMgr::hasTensorInfo(ir));
    if (ir->getAI() == nullptr) { return nullptr; }
    return (TensorInfo*)ir->getAI()->get(AI_TENSOR);
}


TensorInfo * TensorInfoMgr::genTensorInfo(MOD IR * ir)
{
    ASSERT0(ir && TensorInfoMgr::hasTensorInfo(ir));
    if (ir->getAI() == nullptr) {
        IR_ai(ir) = m_rg->allocAIContainer();
    }
    TensorInfo * ti = (TensorInfo*)ir->getAI()->get(AI_TENSOR);
    if (ti == nullptr) {
        ti = allocTensorInfo();
        IR_ai(ir)->init();
        IR_ai(ir)->set(ti, m_rg);
    }
    return ti;
}
//END TensorInfoMgr


//
//START CVarVec
//
void CVarVec::dump(Region const* rg) const
{
    note(rg, "\n-- DUMP CVarVec --");
    VarMgr const* vm = rg->getVarMgr();
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        Var const* v = get(i);
        if (v == nullptr) { continue; }
        v->dump(vm);
    }
}


bool CVarVec::verify() const
{
    //Check the uniqueness of each Var.
    xcom::TTab<Var const*> visited;
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        Var const* v = get(i);
        if (v == nullptr) { continue; }
        ASSERT0(!visited.find(v));
        visited.append(v);
    }
    return true;
}
//END CVarVec


//
//START Var2ValMap
//
void Var2ValMap::dump(Region const* rg) const
{
    note(rg, "\n- DUMP Var2ValMap -");
    rg->getLogMgr()->incIndent(2);
    Var2ValMapIter it;
    IR * d = nullptr;
    VarMgr const* vm = rg->getVarMgr();
    for (Var const* v = get_first(it, &d);
         v != nullptr; v = get_next(it, &d)) {
        note(rg, "\n--");
        note(rg, "\nVAR:");
        v->dump(vm);
        note(rg, "\nVAL:");
        rg->getLogMgr()->incIndent(2);
        ASSERT0(d);
        dumpIR(d, rg);
        rg->getLogMgr()->decIndent(2);
    }
    rg->getLogMgr()->decIndent(2);
}


void Var2ValMap::copyContent(Var2ValMap const& src, Region * rg)
{
    ASSERTN(get_elem_count() == 0, ("should free old elements"));
    Var2ValMapIter it;
    IR * d = nullptr;
    for (Var const* v = src.get_first(it, &d);
         v != nullptr; v = src.get_next(it, &d)) {
        ASSERT0(d);
        set(v, rg->dupIRTree(d));
    }
}


void Var2ValMap::freeContent(Region * rg)
{
    Var2ValMapIter it;
    IR * d = nullptr;
    for (get_first(it, &d); !it.end(); get_next(it, &d)) {
        ASSERT0(d);
        rg->freeIRTree(d);
    }
    clean();
}
//END Var2ValMap


//
//START IR2Der
//
void IR2Der::dump(Region const* rg) const
{
    note(rg, "\n- DUMP IR2Der -");
    rg->getLogMgr()->incIndent(2);
    IR2DerIter it;
    IR * d = nullptr;
    for (IR const* ir = get_first(it, &d);
         ir != nullptr; ir = get_next(it, &d)) {
        note(rg, "\n--");
        note(rg, "\nIR:");
        rg->getLogMgr()->incIndent(2);
        dumpIR(ir, rg);
        rg->getLogMgr()->decIndent(2);

        note(rg, "\nDER:");
        rg->getLogMgr()->incIndent(2);
        ASSERT0(d);
        dumpIR(d, rg);
        rg->getLogMgr()->decIndent(2);
    }
    rg->getLogMgr()->decIndent(2);
}


void IR2Der::freeContent(Region * rg)
{
    IR2DerIter it;
    IR * d = nullptr;
    for (IR const* ir = get_first(it, &d);
         ir != nullptr; ir = get_next(it, &d)) {
        //One IR may be recorded as Derivative of multiple IRs.
        rg->freeIRTree(d, false);
    }
    clean();
}
//END IR2Der


//
//START Var2DerVec
//
void Var2DerVec::dump(Region const* rg) const
{
    note(rg, "\n- DUMP Var2DerVec -");
    rg->getLogMgr()->incIndent(2);
    Var2DerMapIter it;
    VarMgr const* vm = rg->getVarMgr();
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        IR const* d = get(i);
        if (d == nullptr) { continue; }
        Var const* v = vm->get_var(i);
        ASSERT0(v);
        note(rg, "\n--");
        note(rg, "\nVAR:");
        v->dump(vm);
        note(rg, "\nDER:");
        rg->getLogMgr()->incIndent(2);
        ASSERT0(d);
        dumpIR(d, rg);
        rg->getLogMgr()->decIndent(2);
    }
    rg->getLogMgr()->decIndent(2);
}


void Var2DerVec::freeContent(Region * rg)
{
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        IR * ir = get(i);
        if (ir == nullptr) { continue; }
        rg->freeIRTree(ir);
    }
    clean();
}
//END Var2DerVec


//
//START CalcDxCtx
//
bool CalcDxCtx::verify() const
{
    ASSERT0(is_dense_mode() ?
            (void*)getVar2DerVec() : (void*)getVar2DerMap());
    ASSERT0(getIR2DerMap());
    return true;
}


void CalcDxCtx::dump(Region const* rg) const
{
    note(rg, "\n-- DUMP CalcDxCtx --");
    CalcDxCtx * pthis = const_cast<CalcDxCtx*>(this);
    if (pthis->getVar2DerMap() != nullptr &&
        pthis->getVar2DerMap()->get_elem_count() > 0) {
        rg->getLogMgr()->incIndent(2);
        pthis->getVar2DerMap()->dump(rg);
        rg->getLogMgr()->decIndent(2);
    }
    if (pthis->getIR2DerMap() != nullptr &&
        pthis->getIR2DerMap()->get_elem_count() > 0) {
        rg->getLogMgr()->incIndent(2);
        pthis->getIR2DerMap()->dump(rg);
        rg->getLogMgr()->decIndent(2);
    }
}
//END CalcDxCtx


//
//START CalcDerivative
//
CalcDerivative::CalcDerivative(Region * rg) :
    Pass(rg), m_act_mgr(rg), m_tensorinfo_mgr(rg)
{
    ASSERT0(rg != nullptr);
    m_irmgr = rg->getIRMgr();
    m_tm = rg->getTypeMgr();
    m_vm = rg->getVarMgr();
    m_refine = nullptr;
    m_allow_undefined_act = false;
    m_prefer_fp = true;
    m_fp_type = m_tm->getF64();
}


void CalcDerivative::dumpFormat(
    IR const* ir, bool c_style, CalcDxCtx const& dc) const
{
    xcom::StrBuf buf(128);
    dumpFormatBuf(ir, buf, c_style, dc);
    note(m_rg, "\n%s", buf.getBuf());
}


//In C++, local declared class should NOT be used in template parameters of a
//template class. Because the template class may be instanced outside the
//function and the local type in function is invisible.
class VFRefDx {
public:
    CalcDerivative const* cd;
    Var const* dx;
    bool find;
public:
    VFRefDx() { cd = nullptr; dx = nullptr; find = false; }

    //Return true to process the kid IR on tree.
    bool visitIR(IR const* ir, OUT bool & is_terminate)
    {
        Var const* v = cd->findDxVar(ir);
        if (v == dx) {
            is_terminate = true;
            find = true;
            return false;
        }
        return true;
    }
};


//Return true if exp-tree ir references dx-variable.
bool CalcDerivative::isRefDx(IR const* ir, Var const* dx) const
{
    ASSERT0(ir && dx);
    //Do NOT handle sibling IR of 'ir'.

    class IterTree : public VisitIRTree<VFRefDx> {
    public: IterTree(VFRefDx & vf) : VisitIRTree<VFRefDx>(vf) {}
    };
    VFRefDx vf;
    vf.dx = dx;
    vf.cd = this;
    IterTree it(vf);
    it.visit(ir);
    return vf.find;
}


void CalcDerivative::dumpForConst(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->is_const());
    if (ir->isInt()) {
        HOST_INT val = CONST_int_val(ir);
        if (val < 0) {
            buf.strcat("(%d)", val);
        } else {
            buf.strcat("%d", val);
        }
        return;
    }
    ASSERT0(ir->is_fp());
    if (c_style) {
        buf.strcat("%f", CONST_fp_val(ir));
    } else if (xcom::Float::isApproEq(CONST_fp_val(ir), xcom::Float::getE())) {
        buf.strcat("e");
    } else {
        buf.strcat("%f", CONST_fp_val(ir));
    }
    return;
}


void CalcDerivative::dumpTenType(TenType const* t, UINT indent) const
{
    ASSERT0(t);
    ASSERT0(m_rg->isLogMgrInit());
    StrBuf buf(32);
    t->dumpb(buf, indent);
    prt(m_rg, buf.getBuf());
}


void CalcDerivative::dumpTensorForIRTree(IR const* ir) const
{
    if (!m_rg->getLogMgr()->is_init()) { return; }
    dumpIR(ir, m_rg);
    m_rg->getLogMgr()->incIndent(2);
    UINT ind = m_rg->getLogMgr()->getIndent();
    ConstIRIter it;
    for (IR const* x = xoc::iterInitC(ir, it, false);
         x != nullptr; x = xoc::iterNextC(it, true)) {
        if (!x->is_tensor()) { continue; }
        note(m_rg, "\n--");
        dumpTensor(x, ind);
    }
    m_rg->getLogMgr()->decIndent(2);
}


void CalcDerivative::dumpTensor(IR const* ir, UINT indent) const
{
    if (!ir->is_tensor()) { return; }
    ASSERT0(m_rg->isLogMgrInit());
    if (ir->is_const()) {
        dumpIR(ir, m_rg, nullptr, IR_DUMP_DEF);
        dumpTenType((TenType const*)CONST_tensor_val(ir), indent);
        return;
    }
    TenType const* t = (TenType const*)CalcDerivative::getTensor(ir);
    dumpIR(ir, m_rg, nullptr, IR_DUMP_DEF);
    if (t == nullptr) {
        note(m_rg, "\nNO_TENSOR_INFO");
        return;
    }
    dumpTenType(t, indent);
}


void CalcDerivative::setTensor(MOD IR * ir, TenType * t)
{
    ASSERT0(ir && t);
    TensorInfo * ti = getTensorInfoMgr().genTensorInfo(ir);
    ASSERT0(ti);
    ASSERT0(ti->getTensor() == nullptr);
    ti->setTensor(t);
}


TenType * CalcDerivative::genTensor(MOD IR * ir)
{
    TensorInfo * ti = getTensorInfoMgr().genTensorInfo(ir);
    ASSERT0(ti);
    TenType * t = ti->getTensor();
    if (t == nullptr) {
        t = getTenMgr().allocTenAndRecord();
        ti->setTensor(t);
    }
    return t;
}


void CalcDerivative::dumpCppStyleForLog(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->is_log());
    xcom::StrBuf lbuf(32);

    //opnd0
    IR const* op0 = BIN_opnd0(ir);
    dumpFormatBuf(op0, lbuf, c_style, dc);
    buf.strcat("xcom::xlog(%s,", lbuf.getBuf());

    //opnd1
    lbuf.clean();
    IR const* op1 = BIN_opnd1(ir);
    dumpFormatBuf(op1, lbuf, c_style, dc);
    buf.strcat(" %s)", lbuf.getBuf());
}


void CalcDerivative::dumpFormatBufForBinFuncCall(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->isBinaryOp());
    //opnd0
    xcom::StrBuf lbuf0(32);
    IR const* op0 = BIN_opnd0(ir);
    dumpFormatBuf(op0, lbuf0, c_style, dc);

    //opnd1
    xcom::StrBuf lbuf1(32);
    IR const* op1 = BIN_opnd1(ir);
    dumpFormatBuf(op1, lbuf1, c_style, dc);

    buf.strcat("%s(%s, %s)", IR::getIRName(ir), lbuf0.getBuf(), lbuf1.getBuf());
}


void CalcDerivative::dumpFormatBufForUnaFuncCall(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->isUnaryOp());
    xcom::StrBuf lbuf(32);
    IR const* op = UNA_opnd(ir);
    dumpFormatBuf(op, lbuf, c_style, dc);
    buf.strcat("%s(%s)", IR::getIRName(ir), lbuf.getBuf());
}


void CalcDerivative::dumpCppStyleForNRoot(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->is_nroot());
    xcom::StrBuf lbuf(32);
    //opnd0
    IR const* op0 = BIN_opnd0(ir);
    dumpFormatBuf(op0, lbuf, c_style, dc);
    buf.strcat("xcom::xnroot(%s,", lbuf.getBuf());

    //opnd1
    lbuf.clean();
    IR const* op1 = BIN_opnd1(ir);
    dumpFormatBuf(op1, lbuf, c_style, dc);
    buf.strcat(" %s)", lbuf.getBuf());
}


CHAR const* CalcDerivative::getCppTypeCvtStr(Type const* ty) const
{
    switch (ty->getDType()) {
    case D_B: return "bool";
    case D_I8: return "signed char";
    case D_I16: return "short";
    case D_I32: return "int";
    case D_I64: return "long long";
    case D_U8: return "unsigned char";
    case D_U16: return "unsigned short";
    case D_U32: return "unsigned int";
    case D_U64: return "unsigned long long";
    case D_F32: return "float";
    case D_F64: return "double";
    case D_STR: return "char*";
    case D_PTR: return "void*";
    default: ASSERTN(0, ("TODO"));
    }
    return nullptr;
}


void CalcDerivative::dumpCppStyleForCvt(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->is_cvt());
    xcom::StrBuf lbuf(32);
    //opnd
    IR const* op = CVT_exp(ir);
    dumpFormatBuf(op, lbuf, c_style, dc);
    buf.strcat("(%s)(%s)", getCppTypeCvtStr(ir->getType()), lbuf.getBuf());
}


void CalcDerivative::dumpCppStyleForPow(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->is_pow());
    xcom::StrBuf lbuf(32);

    //opnd0
    IR const* op0 = BIN_opnd0(ir);
    dumpFormatBuf(op0, lbuf, c_style, dc);
    buf.strcat("pow(%s,", lbuf.getBuf());

    //opnd1
    lbuf.clean();
    IR const* op1 = BIN_opnd1(ir);
    dumpFormatBuf(op1, lbuf, c_style, dc);
    buf.strcat(" %s)", lbuf.getBuf());
}


void CalcDerivative::dumpAlgeStyleForLog(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->is_log());
    xcom::StrBuf lbuf(32);

    //opnd0
    IR const* op0 = BIN_opnd0(ir);
    dumpFormatBuf(op0, lbuf, c_style, dc);

    buf.strcat("log{");
    if (needDumpParen(op0, this, dc)) {
        buf.strcat("(");
    }
    buf.strcat(lbuf);
    if (needDumpParen(op0, this, dc)) {
        buf.strcat(")");
    }

    //opnd1
    lbuf.clean();
    IR const* op1 = BIN_opnd1(ir);
    dumpFormatBuf(op1, lbuf, c_style, dc);
    buf.strcat("(%s)}", lbuf.getBuf());
}


void CalcDerivative::dumpAlgeStyleForNRoot(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->isBinaryOp());
    xcom::StrBuf lbuf(32);

    //opnd0
    IR const* op0 = BIN_opnd0(ir);
    dumpFormatBuf(op0, lbuf, c_style, dc);
    if (needDumpParen(op0, this, dc)) {
        buf.strcat("(");
    }
    buf.strcat(lbuf);
    if (needDumpParen(op0, this, dc)) {
        buf.strcat(")");
    }
    buf.strcat("^(1/");

    //opnd1
    lbuf.clean();
    IR const* op1 = BIN_opnd1(ir);
    dumpFormatBuf(op1, lbuf, c_style, dc);
    if (needDumpParen(op1, this, dc)) {
        buf.strcat("(");
    }
    buf.strcat(lbuf);
    if (needDumpParen(op1, this, dc)) {
        buf.strcat(")");
    }
    buf.strcat(")");
}


void CalcDerivative::dumpAlgeStyleForUna(
    IR const* ir, CHAR const* op, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->isUnaryOp());
    ASSERT0(op);
    xcom::StrBuf lbuf(32);
    //opnd
    IR const* opnd = UNA_opnd(ir);
    dumpFormatBuf(opnd, lbuf, c_style, dc);
    buf.strcat(op);
    if (needDumpParen(opnd, this, dc)) {
        buf.strcat("(");
    }
    buf.strcat(lbuf);
    if (needDumpParen(opnd, this, dc)) {
        buf.strcat(")");
    }
}


void CalcDerivative::dumpAlgeStyleForBin(
    IR const* ir, CHAR const* op, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    ASSERT0(ir->isBinaryOp());
    ASSERT0(op);
    xcom::StrBuf lbuf(32);

    //opnd0
    IR const* op0 = BIN_opnd0(ir);
    dumpFormatBuf(op0, lbuf, c_style, dc);
    if (needDumpParen(op0, this, dc)) {
        buf.strcat("(");
    }
    buf.strcat(lbuf);
    if (needDumpParen(op0, this, dc)) {
        buf.strcat(")");
    }
    buf.strcat(op);

    //opnd1
    lbuf.clean();
    IR const* op1 = BIN_opnd1(ir);
    dumpFormatBuf(op1, lbuf, c_style, dc);
    if (needDumpParen(op1, this, dc)) {
        buf.strcat("(");
    }
    buf.strcat(lbuf);
    if (needDumpParen(op1, this, dc)) {
        buf.strcat(")");
    }
}


void CalcDerivative::dumpFormatBuf(
    IR const* ir, OUT xcom::StrBuf & buf, bool c_style,
    CalcDxCtx const& dc) const
{
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV: {
        CHAR const* op =
            ir->is_add() ? "+" :
            ir->is_sub() ? "-" :
            ir->is_mul() ? "*" :
            ir->is_div() ? "/" : nullptr;
        ASSERT0(op);
        dumpAlgeStyleForBin(ir, op, buf, c_style, dc);
        return;
    }
    case IR_NEG: {
        CHAR const* op =
            ir->is_neg() ? "-" : nullptr;
        ASSERT0(op);
        dumpAlgeStyleForUna(ir, op, buf, c_style, dc);
        return;
    }
    case IR_POW: {
        if (c_style) {
            dumpCppStyleForPow(ir, buf, c_style, dc);
            return;
        }
        dumpAlgeStyleForBin(ir, "^", buf, c_style, dc);
        return;
    }
    case IR_NROOT: {
        if (c_style) {
            dumpCppStyleForNRoot(ir, buf, c_style, dc);
            return;
        }
        dumpAlgeStyleForNRoot(ir, buf, c_style, dc);
        return;
    }
    case IR_LOG: {
        if (c_style) {
            dumpCppStyleForLog(ir, buf, c_style, dc);
            return;
        }
        dumpAlgeStyleForLog(ir, buf, c_style, dc);
        return;
    }
    case IR_CONST:
        dumpForConst(ir, buf, c_style, dc);
        return;
    case IR_LD:
        buf.strcat("%s", ir->getIdinfo()->get_name()->getStr());
        return;
    case IR_CVT:
        if (c_style) {
            dumpCppStyleForCvt(ir, buf, c_style, dc);
            return;
        }
        dumpFormatBuf(CVT_exp(ir), buf, c_style, dc);
        return;
    SWITCH_CASE_UNA_TRIGONOMETRIC:
        dumpFormatBufForUnaFuncCall(ir, buf, c_style, dc);
        return;
    SWITCH_CASE_EXT_EXP: return dumpFormatExtOp(ir, buf, c_style, dc);
    default: ASSERTN(0, ("TODO"));
    }
}


TenType * CalcDerivative::genImmTensor(Type const* ty)
{
    ASSERT0(ty);
    TensorType const* tensor_ty = (TensorType*)ty;
    ASSERTN(tensor_ty->getDim() == 2, ("only support 2D tensor for now"));
    TenType * t = getTenMgr().allocTenAndRecord(
        tensor_ty->getDegreeOfDim(0),
        tensor_ty->getDegreeOfDim(1));
    return (TenType*)t;
}


TenType * CalcDerivative::genImmTensorByIntScalar(HOST_INT v, Type const* ty)

{
    TenType * t = (TenType*)genImmTensor(ty);
    t->setAllElem((INT)v);
    return (TenType*)t;
}


IR * CalcDerivative::genFPImmByType(HOST_FP v, Type const* ty,
                                    CalcDxCtx const& dc)
{
    if (ty->isInt()) {
        return m_irmgr->buildImmInt((HOST_INT)v, ty);
    }
    if (ty->is_tensor()) {
        //CASE:Immediate with tensor type could be tensorized after entire
        //calculation. Thus we generate scalar immediate type here to make the
        //immediate more readable.
        //void * tensor_data = genImmTensorByIntScalar(v, ty);
        //return m_irmgr->buildImmTensor(tensor_data, ty);
    }
    Type const* fpty = ty->is_fp() ? ty : getFPType();
    return m_irmgr->buildImmFP(HOST_FP(v), fpty);
}


IR * CalcDerivative::genImmByType(HOST_INT v, Type const* ty,
                                  CalcDxCtx const& dc)
{
    if (ty->isInt()) {
        return m_irmgr->buildImmInt(v, ty);
    }
    if (ty->is_tensor()) {
        //CASE:Immediate with tensor type could be tensorized after entire
        //calculation. Thus we generate scalar immediate type here to make the
        //immediate more readable.
        //void * tensor_data = genImmTensorByIntScalar(v, ty);
        //return m_irmgr->buildImmTensor(tensor_data, ty);
    }
    if (m_prefer_fp) {
        Type const* fpty = ty->is_fp() ? ty : getFPType();
        return m_irmgr->buildImmFP(HOST_FP(v), fpty);
    }
    ASSERT0(ty->is_int());
    Type const* intty = m_tm->getSIntTypeWithSameSize(ty);
    return m_irmgr->buildImmInt(v, intty);
}


Var const* CalcDerivative::findDxVar(IR const* ir) const
{
    if (ir->is_pr()) { return m_rg->getVarByPRNO(ir->getPrno()); }
    if (ir->is_ld()) { return ir->getIdinfo(); }
    return nullptr;
}


IR * CalcDerivative::calcVarBaseLog(IR const* ir, bool & change, CalcDxCtx & dc)
{
    //VAR_BASE_LOG: log(x, a)' = -log_e(a)/(x*log_e(x)^2)
    ASSERT0(ir->is_log());
    Type const* ty = ir->getType();
    IR const* x = BIN_opnd0(ir); //base
    IR const* a = BIN_opnd1(ir); //argument
    ASSERT0(isRefDx(x, dc.getDxVar()));
    IR * e1 = m_irmgr->buildBinaryOpSimp(IR_LOG, ty,
        m_irmgr->buildImmFP(xcom::Float::getE(), m_tm->getF64()),
        m_rg->dupIRTree(a));
    IR * e2 = m_irmgr->buildUnaryOp(IR_NEG, ty, e1);
    IR * e3 = m_irmgr->buildBinaryOpSimp(IR_LOG, ty,
        m_irmgr->buildImmFP(xcom::Float::getE(), m_tm->getF64()),
        m_rg->dupIRTree(x));
    IR * e4 = m_irmgr->buildBinaryOpSimp(IR_POW, ty,
        e3, genImmByType(2, ty, dc));
    IR * e5 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty,
        m_rg->dupIRTree(x), e4);
    IR * e6 = m_irmgr->buildBinaryOpSimp(IR_DIV, ty, e2, e5);
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e6, change, dc);
    }

    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e7 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e6, gdx);
    return tryRefine(e7, change, dc);
}


IR * CalcDerivative::calcStpr(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir && ir->is_stpr());
    return calcIR(ir->getRHS(), change, dc);
}


IR * CalcDerivative::calcPR(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERTN(ir->is_pr() && ir->getSSAInfo(), ("Calc derivative need SSA"));
    if (dc.is_forward_mode()) {
        return calcIR(ir->getSSAInfo()->getDef(), change, dc);
    }
    Var const* prvar = m_rg->getVarByPRNO(ir->getPrno());
    ASSERT0(prvar && dc.getDxVar());
    if (prvar != dc.getDxVar()) {
        //Not indenpendent variable.
        //Note dc's Dx may be NULL.
        return genImmByType(0, ir->getType(), dc);
    }
    return genImmByType(1, ir->getType(), dc);
}


IR * CalcDerivative::calcAtan(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_atan());
    //atan(x)' = 1/sqrt(1+x^2)
    Type const* ty = ir->getType();
    IR const* x = UNA_opnd(ir);
    IR * e1 = m_irmgr->buildBinaryOpSimp(IR_DIV, ty,
        genImmByType(1, ty, dc),
        m_irmgr->buildBinaryOpSimp(IR_NROOT, ty,
            m_irmgr->buildBinaryOpSimp(IR_ADD, ty,
                genImmByType(1, ty, dc),
                m_irmgr->buildBinaryOpSimp(IR_POW, ty,
                    m_rg->dupIRTree(x),
                    genImmByType(2, ty, dc))),
            genImmByType(2, ty, dc)));
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e1, change, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e1, gdx);
    return tryRefine(e2, change, dc);
}


IR * CalcDerivative::calcExponent(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_exponent());
    //exponent(a, x)' = a^x*log_e(a)
    Type const* ty = ir->getType();
    IR const* a = BIN_opnd0(ir); //base
    IR const* x = BIN_opnd1(ir); //pow
    IR * e1 = m_irmgr->buildBinaryOpSimp(IR_LOG, ty,
        m_irmgr->buildImmFP(xcom::Float::getE(), m_tm->getF64()),
        m_rg->dupIRTree(a));
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_POW, ty,
        m_rg->dupIRTree(a), m_rg->dupIRTree(x));
    IR * e3 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e2, e1);
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e3, change, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e4 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e3, gdx);
    return tryRefine(e4, change, dc);
}


IR * CalcDerivative::calcConst(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_const());
    ASSERTN(!ir->is_tensor(), ("imm tensor should not be handled here"));
    //1' = 0
    return genImmByType(0, ir->getType(), dc);
}


IR * CalcDerivative::calcLD(IR const* ir, bool & change, MOD CalcDxCtx & dc)
{
    ASSERT0(ir->is_ld());
    //If current LD is not dx, return 0. else return x'.
    //x' = 1
    IR * der = nullptr;
    Var const* irvar = ir->getIdinfo();
    if (irvar != dc.getDxVar()) {
        //Not indenpendent variable.
        //Note dc's Dx may be NULL.
        der = genImmByType(0, ir->getType(), dc);
    } else {
        der = genImmByType(1, ir->getType(), dc);
    }
    return der;
}


IR * CalcDerivative::calcMul(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_mul());
    //(f(x)*g(x))' = f'(x)*g(x)+f(x)*g'(x)
    Type const* ty = ir->getType();
    IR const* fx = BIN_opnd0(ir);
    IR const* gx = BIN_opnd1(ir);
    IR * fdx = calcIR(fx, change, dc);
    IR * gdx = calcIR(gx, change, dc);
    IR * p0 = m_irmgr->buildBinaryOpSimp(
        IR_MUL, ty, fdx, m_rg->dupIRTree(gx));
    IR * p1 = m_irmgr->buildBinaryOpSimp(
        IR_MUL, ty, m_rg->dupIRTree(fx), gdx);
    IR * newir = m_irmgr->buildBinaryOpSimp(
        IR_ADD, ty, p0, p1);
    return tryRefine(newir, change, dc);
}


IR * CalcDerivative::calcAddSub(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_add() || ir->is_sub());
    //(f(x)+g(x))' = f'(x)+g'(x)
    IR * newir = m_rg->dupIR(ir);
    newir->setKid(0, calcIR(BIN_opnd0(ir), change, dc));
    newir->setKid(1, calcIR(BIN_opnd1(ir), change, dc));
    return tryRefine(newir, change, dc);
}


IR * CalcDerivative::calcDiv(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_div());
    //(f(x) / g(x))' = ( f'(x) * g(x) - f(x) * g'(x) ) / g(x)^2
    Type const* ty = ir->getType();
    IR const* fx = BIN_opnd0(ir);
    IR const* gx = BIN_opnd1(ir);
    IR * fdx = calcIR(fx, change, dc);
    IR * gdx = calcIR(gx, change, dc);
    IR * p0 = m_irmgr->buildBinaryOpSimp(
        IR_MUL, ty, fdx, m_rg->dupIRTree(gx));
    IR * p1 = m_irmgr->buildBinaryOpSimp(
        IR_MUL, ty, m_rg->dupIRTree(fx), gdx);
    IR * p2 = m_irmgr->buildBinaryOpSimp(
        IR_SUB, ty, p0, p1);
    IR * p3 = m_irmgr->buildBinaryOpSimp(
        IR_POW, ty, m_rg->dupIRTree(gx),
        genImmByType(2, ty, dc));
    IR * newir = m_irmgr->buildBinaryOpSimp(
        IR_DIV, ty, p2, p3);
    return tryRefine(newir, change, dc);
}


IR * CalcDerivative::calcPow(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_pow());
    //(x^y)' = y*x^(y-1)
    Type const* ty = ir->getType();
    IR const* base = BIN_opnd0(ir);
    IR const* pow = BIN_opnd1(ir);
    IR * newpow = m_irmgr->buildBinaryOpSimp(IR_SUB, ty,
        m_rg->dupIRTree(pow), genImmByType(1, ty, dc));
    IR * newir = m_rg->dupIR(ir);
    newir->setKid(0, m_rg->dupIRTree(base));
    newir->setKid(1, newpow);
    newir = m_irmgr->buildBinaryOpSimp(IR_MUL, ty,
        m_rg->dupIRTree(pow), newir);
    if (!isCompositeFunc(base, dc)) {
        return tryRefine(newir, change, dc);
    }

    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(base, change, dc);
    newir = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, newir, gdx);
    return tryRefine(newir, change, dc);
}


IR * CalcDerivative::calcNRoot(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_nroot());
    //nroot(x, n)' = 1 / (n * (nroot(x, n)) ^ (n-1) )
    Type const* ty = ir->getType();
    IR const* x = BIN_opnd0(ir);
    IR const* n = BIN_opnd1(ir);
    IR * e1 = m_irmgr->buildBinaryOpSimp(IR_NROOT, ty,
        m_rg->dupIRTree(x), m_rg->dupIRTree(n));
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_SUB, ty,
        m_rg->dupIRTree(n), genImmByType(1, ty, dc));
    IR * e3 = m_irmgr->buildBinaryOpSimp(IR_POW, ty, e1, e2);
    IR * e4 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty,
        m_rg->dupIRTree(n), e3);
    IR * e5 = m_irmgr->buildBinaryOpSimp(IR_DIV, ty,
        genImmByType(1, ty, dc), e4);
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e5, change, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e6 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e5, gdx);
    return tryRefine(e6, change, dc);
}


IR * CalcDerivative::calcLog(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_log());
    Type const* ty = ir->getType();
    IR const* a = BIN_opnd0(ir); //base
    IR const* x = BIN_opnd1(ir); //argument

    //NORMAL_LOG: log(a, x)' = 1 / (x*log_e(a))
    //VAR_BASE_LOG: log(x, a)' = -log(a)/(x*log(x)^2)
    //If independent variable is placed in argument, it belongs normal
    //logarithm case, or if it is placed in base, we usually call it
    //logarithm with a variable base. Thus here we need to determine
    //which case current IR belongs to.
    //Note if dx is NULL, whole Log expression is regarded as a constant.
    bool is_normal_log = dc.getDxVar() != nullptr && isRefDx(x, dc.getDxVar());
    bool is_var_base_log = dc.getDxVar() != nullptr &&
                           isRefDx(a, dc.getDxVar());
    if (!is_normal_log && !is_var_base_log) {
        //Regard current LOG as constant.
        return genImmByType(0, ty, dc);
    }
    if (is_normal_log && is_var_base_log) {
        //log_x(x) is meaningful only when x is 1. Otherwise the result
        //is undefined behaviours. Just return 0.
        getActMgr().dump(
            "%s:UNDEFINED ACT:calculate deriviate of log_x(x)",
            getPassName());
        ASSERT0(allowUndefAct());
        return genImmByType(0, ty, dc);
    }
    if (is_normal_log) {
        return calcNormalLog(ir, change, dc);
    }
    return calcVarBaseLog(ir, change, dc);
}


IR * CalcDerivative::calcNeg(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_neg());
    //-(x)' = -1
    Type const* ty = ir->getType();
    IR const* x = UNA_opnd(ir);
    if (x->is_const()) {
        return genImmByType(0, ty, dc);
    }
    if (!isCompositeFunc(x, dc)) {
        return genImmByType(-1, ty, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e1 = m_irmgr->buildUnaryOp(IR_NEG, ty, gdx);
    return tryRefine(e1, change, dc);
}


IR * CalcDerivative::calcSin(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_sin());
    //sin(x)' = cos(x)
    Type const* ty = ir->getType();
    IR const* x = UNA_opnd(ir);
    IR * e1 = m_irmgr->buildUnaryOp(IR_COS, ty, m_rg->dupIRTree(x));
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e1, change, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e1, gdx);
    return tryRefine(e2, change, dc);
}



IR * CalcDerivative::calcCos(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_cos());
    //cos(x)' = -sin(x)
    Type const* ty = ir->getType();
    IR const* x = UNA_opnd(ir);
    IR * e1 = m_irmgr->buildUnaryOp(
        IR_NEG, ty, m_irmgr->buildUnaryOp(IR_SIN, ty, m_rg->dupIRTree(x)));
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e1, change, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e1, gdx);
    return tryRefine(e2, change, dc);
}


IR * CalcDerivative::calcTan(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_tan());

    //tan(x)' = 1/(cos(x)^2)
    Type const* ty = ir->getType();
    IR const* x = UNA_opnd(ir);
    IR * e1 = m_irmgr->buildBinaryOpSimp(IR_DIV, ty,
        genImmByType(1, ty, dc),
        m_irmgr->buildBinaryOpSimp(IR_POW, ty,
            m_irmgr->buildUnaryOp(IR_COS, ty,
                m_rg->dupIRTree(x)),
            genImmByType(2, ty, dc)));
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e1, change, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e1, gdx);
    return tryRefine(e2, change, dc);
}


IR * CalcDerivative::calcAsin(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_asin());
    //asin(x)' = 1/sqrt(1-x^2)
    Type const* ty = ir->getType();
    IR const* x = UNA_opnd(ir);
    IR * e1 = m_irmgr->buildBinaryOpSimp(IR_DIV, ty,
        genImmByType(1, ty, dc),
        m_irmgr->buildBinaryOpSimp(IR_NROOT, ty,
            m_irmgr->buildBinaryOpSimp(IR_SUB, ty,
                genImmByType(1, ty, dc),
                m_irmgr->buildBinaryOpSimp(IR_POW, ty,
                    m_rg->dupIRTree(x),
                    genImmByType(2, ty, dc))),
            genImmByType(2, ty, dc)));
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e1, change, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e1, gdx);
    return tryRefine(e2, change, dc);
}


IR * CalcDerivative::calcAcos(IR const* ir, bool & change, CalcDxCtx & dc)
{
    ASSERT0(ir->is_acos());
    //acos(x)' = -1/sqrt(1-x^2)
    Type const* ty = ir->getType();
    IR const* x = UNA_opnd(ir);
    IR * e1 = m_irmgr->buildBinaryOpSimp(IR_DIV, ty,
        genImmByType(-1, ty, dc),
        m_irmgr->buildBinaryOpSimp(IR_NROOT, ty,
            m_irmgr->buildBinaryOpSimp(IR_SUB, ty,
                genImmByType(1, ty, dc),
                m_irmgr->buildBinaryOpSimp(IR_POW, ty,
                    m_rg->dupIRTree(x),
                    genImmByType(2, ty, dc))),
            genImmByType(2, ty, dc)));
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e1, change, dc);
    }
    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e1, gdx);
    return tryRefine(e2, change, dc);
}


IR * CalcDerivative::calcNormalLog(IR const* ir, bool & change, CalcDxCtx & dc)
{
    //NORMAL_LOG: log(a, x)' = 1 / (x*log_e(a))
    ASSERT0(ir->is_log());
    Type const* ty = ir->getType();
    IR const* a = BIN_opnd0(ir); //base
    IR const* x = BIN_opnd1(ir); //argument
    ASSERT0(isRefDx(x, dc.getDxVar()));
    IR * e1 = m_irmgr->buildBinaryOpSimp(IR_LOG, ty,
        m_irmgr->buildImmFP(xcom::Float::getE(), m_tm->getF64()),
        m_rg->dupIRTree(a));
    IR * e2 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty,
        m_rg->dupIRTree(x), e1);
    IR * e3 = m_irmgr->buildBinaryOpSimp(IR_DIV, ty,
        genImmByType(1, ty, dc), e2);
    if (!isCompositeFunc(x, dc)) {
        return tryRefine(e3, change, dc);
    }

    //f'(g(x)) = f'(g(x)) * g'(x)
    IR * gdx = calcIR(x, change, dc);
    IR * e4 = m_irmgr->buildBinaryOpSimp(IR_MUL, ty, e3, gdx);
    return tryRefine(e4, change, dc);
}


IR * CalcDerivative::calcIR(IR const* ir, bool & change, MOD CalcDxCtx & dc)
{
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB: return calcAddSub(ir, change, dc);
    case IR_MUL: return calcMul(ir, change, dc);
    case IR_DIV: return calcDiv(ir, change, dc);
    case IR_CONST: return calcConst(ir, change, dc);
    case IR_LD: return calcLD(ir, change, dc);
    case IR_POW: return calcPow(ir, change, dc);
    case IR_NROOT: return calcNRoot(ir, change, dc);
    case IR_LOG: return calcLog(ir, change, dc);
    case IR_EXPONENT: return calcExponent(ir, change, dc);
    case IR_NEG: return calcNeg(ir, change, dc);
    case IR_SIN: return calcSin(ir, change, dc);
    case IR_COS: return calcCos(ir, change, dc);
    case IR_TAN: return calcTan(ir, change, dc);
    case IR_ASIN: return calcAsin(ir, change, dc);
    case IR_ACOS: return calcAcos(ir, change, dc);
    case IR_ATAN: return calcAtan(ir, change, dc);
    case IR_STPR: return calcStpr(ir, change, dc);
    case IR_PR: return calcPR(ir, change, dc);
    SWITCH_CASE_EXT_EXP: return calcExtExp(ir, change, dc);
    default: ASSERTN(0, ("TODO"));
    }
    return nullptr;
}


IR * CalcDerivative::substituteVarInVar2ValMap(
    MOD IR * ir, Var2ValMap const& var2val, CalcDxCtx const& dc)
{
    class ReplaceEachLD : public ReplaceKidCompareFunc {
        Var2ValMap const& m_var2val;
    public:
        ReplaceEachLD(Var2ValMap const& var2val) : m_var2val(var2val){}

        //Return true if oldir has to be replaced, and record the anticepated
        //replacer in 'newir'.
        //newir: return as a result that recorded the anticipated IR to
        //replace.
        virtual bool is_replace(IR const* oldir, OUT IR ** newir) const
        {
            if (!oldir->is_ld()) { return false; }
            ASSERT0(newir);
            *newir = m_var2val.get(oldir->getIdinfo());
            if (*newir == nullptr) {
                //The Idinfo of oldir does not exist in Var2Value table.
                //Thus there is no need to replace oldir.
                return false;
            }
            return true;
        }
    };
    ReplaceEachLD cmpfunc(var2val);
    ir->replaceKid(true, cmpfunc, m_rg);
    bool lchange;
    return tryRefine(ir, lchange, dc);
}


IR * CalcDerivative::substituteVar(
    MOD IR * ir, Var const* var, IR const* exp, CalcDxCtx const& dc)
{
    class ReplaceKidWithSameIdinfo : public ReplaceKidCompareFunc {
        Var const* m_cmpvar;
    public:
        ReplaceKidWithSameIdinfo(Var const* var) { m_cmpvar = var; }

        //Return true if oldir has to be replaced by some newir.
        virtual bool is_replace(IR const* oldir, IR const*) const
        { return oldir->hasIdinfo() && oldir->getIdinfo() == m_cmpvar; }
    };
    ReplaceKidWithSameIdinfo cmpfunc(var);
    ir->replaceKid(exp, true, cmpfunc, m_rg);
    bool lchange;
    return tryRefine(ir, lchange, dc);
}


void CalcDerivative::calcUnaOpRev(
    IR const* ir, IR const* irder, bool & change, MOD CalcDxCtx & dc)
{
    ASSERT0(ir->isUnaryOp());
    IR const* op0 = UNA_opnd(ir);
    //Op0
    ASSERT0(op0->is_leaf());
    Var const* dxvar0 = findDxVar(op0);
    //If op0 is const, dxvar should be NULL.

    //Original dxvar of dc is not in use in reverse-mode.
    ASSERT0(dc.getDxVar() == nullptr);
    dc.setDxVar(dxvar0); //dxvar may be NULL.
    IR * op0der = calcIR(ir, change, dc);
    ASSERT0(op0der);
    dc.cleanDxVar();
    op0der = m_irmgr->buildBinaryOpSimp(IR_MUL, ir->getType(),
        m_rg->dupIRTree(irder), op0der);
    op0der = tryRefine(op0der, change, dc);
    dc.setIR2Der(op0, op0der);
    if (op0->is_ld()) {
        aggregateVarDer(op0->getIdinfo(), op0der, dc);
    }
    //Calc Kid
    calcIRRev(op0, op0der, change, dc);
}


void CalcDerivative::aggregateVarDer(
    Var const* var, IR const* der, MOD CalcDxCtx & dc)
{
    ASSERT0(der);
    //Since 'der' may be optimized and freed after the function returned,
    //duplicate it here.
    IR * newder = m_rg->dupIRTree(der);
    IR * oldder = dc.getDer(var);
    if (oldder != nullptr) {
        ASSERT0(!oldder->is_undef());
        newder = m_irmgr->buildBinaryOpSimp(IR_ADD, newder->getType(),
                                            newder, oldder);
    }
    bool lchange;
    newder = tryRefine(newder, lchange, dc);
    dc.setVar2Der(var, newder);
}


void CalcDerivative::calcBinOpRev(
    IR const* ir, IR const* irder, bool & change, MOD CalcDxCtx & dc)
{
    ASSERT0(ir->isBinaryOp());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    //Op0
    ASSERT0(op0->is_leaf());
    Var const* dxvar0 = findDxVar(op0);
    //If op0 is const, dxvar should be NULL.

    //Original dxvar of dc is is not in use in reverse-mode.
    ASSERT0(dc.getDxVar() == nullptr);
    dc.setDxVar(dxvar0); //dxvar may be NULL.
    IR * op0der = calcIR(ir, change, dc);
    ASSERT0(op0der);
    dc.cleanDxVar();
    op0der = m_irmgr->buildBinaryOpSimp(IR_MUL, ir->getType(),
        m_rg->dupIRTree(irder), op0der);
    op0der = tryRefine(op0der, change, dc);
    dc.setIR2Der(op0, op0der);
    if (op0->is_ld()) {
        aggregateVarDer(op0->getIdinfo(), op0der, dc);
    }

    //Op1
    ASSERT0(op1->is_leaf());
    Var const* dxvar1 = findDxVar(op1);
    //If op1 is const, dxvar should be NULL.

    //Original dxvar of dc is not in use in reverse-mode.
    ASSERT0(dc.getDxVar() == nullptr);
    dc.setDxVar(dxvar1); //dxvar may be NULL.
    IR * op1der = calcIR(ir, change, dc);
    ASSERT0(op1der);
    dc.cleanDxVar();
    op1der = m_irmgr->buildBinaryOpSimp(IR_MUL, ir->getType(),
        m_rg->dupIRTree(irder), op1der);
    op1der = tryRefine(op1der, change, dc);
    dc.setIR2Der(op1, op1der);
    if (op1->is_ld()) {
        aggregateVarDer(op1->getIdinfo(), op1der, dc);
    }
    //Calc Kid
    calcIRRev(op0, op0der, change, dc);
    calcIRRev(op1, op1der, change, dc);
}


void CalcDerivative::calcPRRev(
    IR const* ir, IR const* irder, bool & change, CalcDxCtx & dc)
{
    //PR is regarded as intermediate variable at reverse mode.
    //It is just used to store intermediate derivative and transfer
    //calculation to next operator.
    ASSERTN(ir->is_pr() && ir->getSSAInfo(), ("Calc derivative need SSA"));
    calcIRRev(ir->getSSAInfo()->getDef(), irder, change, dc);
}


void CalcDerivative::calcStprRev(
    IR const* ir, IR const* irder, bool & change, CalcDxCtx & dc)
{
    //Stpr is regarded as intermediate variable at reverse mode.
    //It is just used to store intermediate derivative and transfer
    //calculation to next operator.
    ASSERT0(ir->is_stpr());
    dc.setIR2Der(ir->getRHS(), m_rg->dupIRTree(irder));
    calcIRRev(ir->getRHS(), irder, change, dc);
}


void CalcDerivative::calcIRRev(
    IR const* ir, IR const* irder, bool & change, MOD CalcDxCtx & dc)
{
    switch (ir->getCode()) {
    case IR_CONST:
        return;
    case IR_ADD:
    case IR_SUB:
    case IR_LOG:
    case IR_MUL:
    case IR_POW:
        calcBinOpRev(ir, irder, change, dc); return;
    case IR_PR: calcPRRev(ir, irder, change, dc); return;
    case IR_LD: return;
    SWITCH_CASE_UNA:
    SWITCH_CASE_EXT_UNA:
        calcUnaOpRev(ir, irder, change, dc); return;
    case IR_STPR: calcStprRev(ir, irder, change, dc); return;
    default: calcExtExpRev(ir, irder, change, dc); return;
    }
    UNREACHABLE();
}


void CalcDerivative::calcIRRev(IR const* ir, bool & change, MOD CalcDxCtx & dc)
{
    ASSERT0(ir && (ir->is_stmt() || ir->is_exp()));
    ASSERT0(dc.verify());
    IR * irder = genImmByType(1, ir->getType(), dc);
    ASSERT0(CONST_tensor_val(irder));
    dc.setIR2Der(ir, irder);
    bool is_old_rev = dc.is_reverse_mode();
    dc.setReverseMode(true);
    calcIRRev(ir, irder, change, dc);
    dc.setReverseMode(is_old_rev);
}


bool CalcDerivative::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    m_act_mgr.dump();
    m_rg->getLogMgr()->decIndent(2);
    return true;
}


bool CalcDerivative::perform(OptCtx & oc)
{
    m_refine = (Refine*)m_rg->getPassMgr()->registerPass(PASS_REFINE);
    set_valid(true);
    return false;
}
//END CalcDerivative

} //namespace xoc
