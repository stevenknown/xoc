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

#define DESCEND_ORDER

//Describes the bitsize that represents the maximum numbers of IRs that
//a BB can include.
#define MAX_IR_BIT_RANGE_IN_BB 16

static void dumpFoldConst(
    IR_CODE code, IR const* ir1, IR const* ir2, IR const* res,
    AlgeReassociate const* reass)
{
    AlgeReassociate * pthis = const_cast<AlgeReassociate*>(reass);
    if (!pthis->getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf s1(32);
    xcom::StrBuf s2(32);
    xcom::StrBuf s3(32);
    pthis->getRegion()->getLogMgr()->incIndent(2);
    xoc::dumpIRToBuf(ir1, pthis->getRegion(), s1);
    xoc::dumpIRToBuf(ir2, pthis->getRegion(), s2);
    xoc::dumpIRToBuf(res, pthis->getRegion(), s3);
    pthis->getRegion()->getLogMgr()->decIndent(2);
    pthis->getActMgr().dumpAct("fold '%s' %s,%s \n  into%s",
       IR::getIRCodeName(code), s1.getBuf(), s2.getBuf(), s3.getBuf());
}


static void dumpReplaceExp(
    IR const* orgrhs, IR const* newrhs, AlgeReassociate const* reass)
{
    AlgeReassociate * pthis = const_cast<AlgeReassociate*>(reass);
    if (!pthis->getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf s1(32), s2(32);
    pthis->getRegion()->getLogMgr()->incIndent(2);
    xoc::dumpIRToBuf(orgrhs, pthis->getRegion(), s1);
    xoc::dumpIRToBuf(newrhs, pthis->getRegion(), s2);
    pthis->getRegion()->getLogMgr()->decIndent(2);
    pthis->getActMgr().dumpAct(
        "replace %s \n  with %s", s1.getBuf(), s2.getBuf());
}


static void dumpSimpStmt(IR const* ir, AlgeReassociate const* reass)
{
    ASSERT0(ir->is_stmt());
    AlgeReassociate * pthis = const_cast<AlgeReassociate*>(reass);
    if (!pthis->getRegion()->isLogMgrInit()) { return; }
    xcom::StrBuf s1(32);
    pthis->getRegion()->getLogMgr()->incIndent(2);
    xoc::dumpIRToBuf(ir, pthis->getRegion(), s1);
    pthis->getRegion()->getLogMgr()->decIndent(2);
    pthis->getActMgr().dumpAct(
        "simplify %s \n  to lowest height.", s1.getBuf());
}


static bool isConstExpTree(IR const* ir)
{
    ASSERT0(ir->is_exp());
    if (ir->isMemRef()) { return false; }
    if (ir->is_leaf()) {
        return ir->is_const() ? true : false;
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR const* kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        if (kid->is_const()) { continue; }
        if (!isConstExpTree(kid)) { return false; }
    }
    return true;
}


//
//START VRTGenMapped
//
ConstVarTab * VRTGenMapped::createMapped(IR const*)
{
    ASSERT0(m_vrmgr);
    return m_vrmgr->allocConstVarTab();
}
//END VRTGenMapped


//
//START LinOpVec
//
void LinOpVec::cleanLastFrom(VecIdx idx)
{
    for (VecIdx i = idx; i <= this->get_last_idx(); i++) {
        IR const* ir = this->get(i);
        ASSERT0(ir && !ir->is_undef());
        m_exist.remove(ir);
    }
    xcom::Vector<IR const*>::cleanFrom(idx);
}


void LinOpVec::clean()
{
    m_code = IR_UNDEF;
    xcom::Vector<IR const*>::clean();
    m_exist.clean();
}


void LinOpVec::dump(ReassCtx const& ctx) const
{
    Region const* rg = ctx.getRegion();
    if (!rg->isLogMgrInit()) { return; }
    DefFixedStrBuf buf;
    xoc::note(rg, "\n-- DUMP LinOpVec: CODE:%s --",
              xoc::dumpIRCodeName(getCode(), buf));
    rg->getLogMgr()->incIndent(2);
    for (VecIdx i = 0; i <(VecIdx)get_elem_count(); i++) {
        IR const* ir = get(i);
        ASSERT0(ir);
        xoc::dumpIR(ir, rg, nullptr, DumpFlag::combineIRID(IR_DUMP_DEF));
        RANK rank = ctx.getRank(ir);
        buf.clean();
        xoc::prt(rg, " rank:%s", ctx.dumpRank(rank, buf));
    }
    rg->getLogMgr()->decIndent(2);
}


bool LinOpVec::is_unique(IR const* ir) const
{
    for (VecIdx i = 0; i < (VecIdx)get_elem_count(); i++) {
        IR const* t = get(i);
        ASSERT0(t != ir);
    }
    return true;
}


bool LinOpVec::verify() const
{
    IR_CODE code = IR_UNDEF;
    for (VecIdx i = 0; i < (VecIdx)get_elem_count(); i++) {
        IR const* ir = get(i);
        ASSERT0(ir);
        if (i == 0) {
            code = ir->getCode();
            ASSERT0(m_code == code);
            continue;
        }
        if (ir->is_const()) { continue; }
        ASSERT0(m_code == ir->getCode());
        ASSERTN(code == ir->getCode(),
                ("ir in the vector should have same code"));
    }
    return true;
}
//END LinOpVec


//
//START VarRefMgr
//
VarRefMgr::VarRefMgr()
{
    m_ir2vrt.getGenMapped().setVarRefMgr(this);
}


VarRefMgr::~VarRefMgr()
{
    for (ConstVarTab * vrt = m_vrt_list.get_head();
         vrt != nullptr; vrt = m_vrt_list.get_next()) {
        delete vrt;
    }
}


ConstVarTab * VarRefMgr::allocConstVarTab()
{
    ConstVarTab * vrt = new ConstVarTab();
    m_vrt_list.append_tail(vrt);
    return vrt;
}


void VarRefMgr::set(IR const* ir, Var const* ref)
{
    ASSERT0(ir && ref);
    ConstVarTab * vrt = getAndGenVRT(ir);
    ASSERT0(vrt);
    vrt->append(ref);
}


ConstVarTab const* VarRefMgr::getVRT(IR const* ir) const
{
    bool find = false;
    return m_ir2vrt.get(ir, &find);
}


ConstVarTab * VarRefMgr::getAndGenVRT(IR const* ir)
{
    bool find = false;
    return m_ir2vrt.getAndGen(ir, &find);
}


bool VarRefMgr::find(IR const* ir, Var const* ref)
{
    ConstVarTab const* vrt = m_ir2vrt.get(ir);
    if (vrt == nullptr) { return false; }
    return vrt->find(ref);
}


void VarRefMgr::dump(Region const* rg) const
{
    xoc::note(rg, "\n-- DUMP IR2VARTAB --");
    IR2VRTabIter it;
    ConstVarTab * vrt;
    for (IR const* ir = m_ir2vrt.get_first(it, &vrt);
         ir != nullptr; ir = m_ir2vrt.get_next(it, &vrt)) {
        xoc::dumpIR(ir, rg);
        if (vrt == nullptr) { continue; }
        ConstVarTabIter it2;
        bool first = true;
        for (Var const* mapped = vrt->get_first(it2);
             mapped != nullptr; mapped = vrt->get_next(it2)) {
            xoc::note(rg, "%s", mapped->get_name()->getStr());
            if (!first) {
                xoc::note(rg, ",");
                first = false;
            }
        }
    }
}
//END VarRefMgr


//
//START ReassExp
//
void ReassExp::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    xoc::note(rg, "\n-- CODE:%s", IR::getIRCodeName(getCode()));
    xoc::note(rg, "\n-- OPND_NUM:%u", getOpndNum());
    rg->getLogMgr()->incIndent(2);
    UINT flag = IR_DUMP_COMBINE;
    REMOVE_FLAG(flag, IR_DUMP_SRC_LINE);
    for (UINT i = 0; i < getOpndNum(); i++) {
        IR const* ir = getOpnd(i);
        ASSERT0(ir);
        xoc::dumpIR(ir, rg, nullptr, DumpFlag::combineIRID(flag));
    }
    rg->getLogMgr()->decIndent(2);
}
//END ReassExp


//
//START ReassExpMgr
//
ReassExpMgr::ReassExpMgr()
{
    m_pool = xcom::smpoolCreate(sizeof(ReassExp) * 2, MEM_COMM);
}


ReassExpMgr::~ReassExpMgr()
{
    xcom::smpoolDelete(m_pool);
}


ReassExp * ReassExpMgr::allocReassExp(UINT opndnum)
{
    ASSERT0(opndnum > 0);
    ReassExp * e = (ReassExp*)xmalloc(sizeof(ReassExp));
    REASSEXP_opnd_buf(e) = (IR const**)xmalloc(sizeof(IR const*) * opndnum);
    REASSEXP_opnd_num(e) = opndnum;
    return e;
}


ReassExp * ReassExpMgr::genDirectReassExp(IR const* op)
{
    ASSERT0(op);
    ASSERT0(op->isDirectMemOp() || op->isPROp());
    ReassExp * e = allocReassExp(1);
    REASSEXP_code(e) = op->getCode();
    REASSEXP_opnd(e, 0) = op;
    return e;
}


ReassExp * ReassExpMgr::genConstReassExp(IR const* op)
{
    ASSERT0(op && op->isConstExp());
    ReassExp * e = allocReassExp(1);
    REASSEXP_code(e) = op->getCode();
    REASSEXP_opnd(e, 0) = op;
    return e;
}


ReassExp * ReassExpMgr::genBinReassExp(
    IR_CODE code, IR const* op0, IR const* op1)
{
    ASSERT0(code != IR_UNDEF);
    ASSERT0(IR::isBinaryOp(code));
    ASSERT0(op0 && op1);
    ReassExp * e = allocReassExp(2);
    REASSEXP_code(e) = code;
    REASSEXP_opnd(e, 0) = op0;
    REASSEXP_opnd(e, 1) = op1;
    return e;
}


ReassExp * ReassExpMgr::genBinReassExp(IR const* ir)
{
    ASSERT0(ir && ir->isBinaryOp());
    return genBinReassExp(ir->getCode(), BIN_opnd0(ir), BIN_opnd1(ir));
}
//END ReassExpMgr


//
//START ReassActMgr
//
void ReassActMgr::dumpAct(CHAR const* format, ...)
{
    if (!m_rg->isLogMgrInit()) { return; }
    va_list args;
    va_start(args, format);
    xcom::DefFixedStrBuf buf;
    buf.strcat("REASS:");
    buf.vstrcat(format, args);
    dump("%s", buf.getBuf());
    va_end(args);
}
//END ReassActMgr


class LinOpVecSort : public QuickSort<IR const*> {
    ReassCtx const& m_ctx;
protected:
    virtual IR const* _max(IR const* a, IR const* b) const
    {
        RANK a_rank = m_ctx.getRank(a);
        RANK b_rank = m_ctx.getRank(b);
        ASSERT0(a_rank != RANK_UNDEF);
        ASSERT0(b_rank != RANK_UNDEF);
        return a_rank > b_rank ? a : b;
    }

    virtual IR const* _min(IR const* a, IR const* b) const
    {
        RANK a_rank = m_ctx.getRank(a);
        RANK b_rank = m_ctx.getRank(b);
        ASSERT0(a_rank != RANK_UNDEF);
        ASSERT0(b_rank != RANK_UNDEF);
        return a_rank < b_rank ? a : b;
    }

    virtual bool GreatThan(IR const* a, IR const* b) const
    {
        RANK a_rank = m_ctx.getRank(a);
        RANK b_rank = m_ctx.getRank(b);
        ASSERT0(a_rank != RANK_UNDEF);
        ASSERT0(b_rank != RANK_UNDEF);

        //We expect to sort data in descend order.
        #ifdef DESCEND_ORDER
        return a_rank < b_rank;
        #else
        return a_rank > b_rank;
        #endif
    }

    virtual bool LessThan(IR const* a, IR const* b) const
    {
        RANK a_rank = m_ctx.getRank(a);
        RANK b_rank = m_ctx.getRank(b);
        ASSERT0(a_rank != RANK_UNDEF);
        ASSERT0(b_rank != RANK_UNDEF);

        //We expect to sort data in descend order.
        #ifdef DESCEND_ORDER
        return a_rank > b_rank;
        #else
        return a_rank < b_rank;
        #endif
    }
public:
    LinOpVecSort(ReassCtx & ctx) : m_ctx(ctx) {}
};


//
//START ReassCtx
//
//
ReassCtx::ReassCtx(OptCtx & oc, AlgeReassociate * algereass)
    : PassCtx(&oc, &algereass->getActMgr()), m_cur_rank(RANK_UNDEF)
{
    m_alge_reass = algereass;
    m_irmgr = oc.getRegion()->getIRMgr();
    m_ir_rank_range_bitsize = 0;
    m_need_recomp_gvn = false;
}


ReassCtx::~ReassCtx()
{
    IRTabIter it;
    Region * rg = getRegion();
    for (IR * ir = m_gened_ir.get_first(it);
         ir != nullptr; ir = m_gened_ir.get_next(it)) {
        rg->freeIRTree(ir);
    }
}


void ReassCtx::recordIR2Var(IR const* key, Var const* value)
{
    m_ir2var_lst.append_tail(IR2Var(key, value));
}


void ReassCtx::dumpBBListWithRankImpl(MOD IRDumpCtx<> & irdumpctx) const
{
    ASSERT0(m_rg);
    BBList const* bbl = m_rg->getBBList();
    ASSERT0(bbl);
    if (!m_rg->isLogMgrInit() || bbl->get_elem_count() == 0) { return; }
    BBListIter it;
    DefFixedStrBuf buf;
    note(m_rg, "\n\n-- DUMP BBLIST WITH RANK --");
    m_rg->getLogMgr()->incIndent(2);
    BBDumpCtxMgr<> dumpctx(&irdumpctx);
    for (IRBB const* bb = bbl->get_head(&it);
         bb != nullptr; bb = bbl->get_next(&it)) {
        RANK rank = getRank(bb);
        if (rank != RANK_UNDEF) {
            buf.clean();
            note(m_rg, "\nBB%u rank:%s", bb->id(), dumpRank(rank, buf));
        }
        bb->dump(m_rg, false, &dumpctx);
    }
    m_rg->getLogMgr()->decIndent(2);
}


UINT ReassCtx::getBBRankRangeBitSize() const
{
    return sizeof(RANK) * BITS_PER_BYTE - getIRRankRangeBitSize();
}


void ReassCtx::recordGenedIRTree(IR * ir)
{
    m_gened_ir.append(ir);
}


CHAR const* ReassCtx::dumpRank(RANK rank, OUT DefFixedStrBuf & buf) const
{
    UINT ir_rank_range_bitsize = getIRRankRangeBitSize();
    UINT end = sizeof(RANK) * BITS_PER_BYTE - 1;
    ULONGLONG ir_rank = xcom::extractBitRangeValue(
        (ULONGLONG)rank, 0, ir_rank_range_bitsize - 1);
    ULONGLONG bb_rank = xcom::extractBitRangeValue(
        (ULONGLONG)rank, ir_rank_range_bitsize, end);
    buf.strcat("%u(0x%x|0x%x)", rank, bb_rank, ir_rank);
    return buf.getBuf();
}


void ReassCtx::dumpBBListWithRank() const
{
    //The class dumps IR with user defined attributes.
    class DumpIRWithRank : public IRDumpCustomBaseFunc {
    public:
        ReassCtx const* ctx;
    public:
        virtual void dumpCustomAttr(
            OUT xcom::DefFixedStrBuf & buf, Region const* rg, IR const* ir,
            DumpFlag dumpflag) const override
        {
            ASSERT0(ctx);
            RANK rank = ctx->getRank(ir);
            if (rank == RANK_UNDEF) { return; }
            xcom::DefFixedStrBuf tbuf;
            buf.strcat(" rank:%s", ctx->dumpRank(rank, tbuf));
        }
    };
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID | IR_DUMP_SRC_LINE);
    DumpIRWithRank cf;

    //User defined attributes are VN info.
    cf.ctx = this;
    IRDumpCtx<> irdumpctx(4, f, nullptr, &cf);
    dumpBBListWithRankImpl(irdumpctx);
}


void ReassCtx::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(m_rg, "\n====-- DUMP ReassCtx --====");
    m_rg->getLogMgr()->incIndent(2);
    DefFixedStrBuf buf;
    note(m_rg, "\nCurRank:%s", dumpRank(getCurRank(), buf));
    const_cast<ReassCtx*>(this)->getLinOpVec().dump(*this);
    dumpBBListWithRank();
    m_rg->getLogMgr()->decIndent(2);
}


void ReassCtx::cleanBottomUp()
{
    m_cur_rank = RANK_UNDEF;
    m_lin_opvec.clean();

    //NOTE: The map of ir2rank should NOT be temporary informat.
    //It will be queried in each linearization.
    //m_ir2rank.clean();
}


bool ReassCtx::verify() const
{
    ReassCtx * pthis = const_cast<ReassCtx*>(this);
    LinOpVec & linopvec = pthis->getLinOpVec();

    //CASE: If the RHS is only constant, the evaluated reassexp
    //is valid but no optimization opportunity.
    //e.g: st:i32 'h' = intconst:i32 8;
    //ASSERT0(linopvec.getCode() != IR_UNDEF);
    for (UINT i = 0; i < linopvec.get_elem_count(); i++) {
        IR const* ir = linopvec.get(i);
        ASSERT0(ir && !ir->is_undef());
        RANK r = getRank(ir);
        ASSERT0(r != RANK_UNDEF);
    }
    return true;
}
//END ReassCtx


class AlgeIntlImpl {
public:
    static RANK computeRankViaBinOp(
        ReassExp const* reassexp, ReassCtx const& ctx);
    static RANK computeRankViaMultiOpnd(
        ReassExp const* reassexp, ReassCtx const& ctx);
    static RANK computeRankViaKid(IR const* ir, MOD ReassCtx & ctx);
    static RANK computeRankViaConst(IR const* ir, MOD ReassCtx & ctx);
    static RANK computeRankViaConstExpTree(IR const* ir, MOD ReassCtx & ctx);
    static RANK computeRankViaReassExp(
        ReassExp const* reassexp, MOD ReassCtx & ctx);

    static bool isTransferOpCode(IR_CODE code);

    //Return true if entire RHS are linearized.
    static bool linearRHS(MOD IR * rhs, MOD ReassCtx & ctx);

    //The function attempts to evaluate the Reass-Expression for given 'ir'.
    //Then the system will compute the rank according to the Reass-Expression.
    static bool evalAndReformExp(
        MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx);
    static bool tryEvalAndReformBinOp(
        MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx);
    static bool tryEvalAndReformConstOp(
        MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx);
    static void trySetCodeViaRHS(
        IR const* rhs, ReassExp const* reassexp, MOD LinOpVec & opvec,
        ReassCtx const& ctx);

    //Return true if ir can be reassociated and evaluated to a reass-exp.
    static bool tryEvalAndReformDirectOp(
        MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx);
    static bool tryReformDIV(
        MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx);
    static bool tryReformBinOp(
        MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx);

    //The function tries to perform commutative operations recursively to
    //the IR tree.
    //Return true if at least one of childs of the IR tree was changed.
    //ir: the root of the IR tree.
    static bool tryCommutativeBinOp(MOD IR * ir, MOD ReassCtx & ctx);
};


bool AlgeIntlImpl::tryReformDIV(
    MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx)
{
    ASSERT0(ir->is_div());
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    if (!op1->is_const()) { return false; }
    if (op1->is_fp() && !g_do_opt_float) { return false; }
    IR * newop1 = ctx.getIRMgr()->buildRecipOp(
        ctx.getRegion()->dupIRTree(op1), ir->getType());
    *evaled = ctx.getReassExpMgr().genBinReassExp(IR_MUL, op0, newop1);
    ctx.recordGenedIRTree(newop1);
    return true;
}


bool AlgeIntlImpl::tryCommutativeBinOp(MOD IR * ir, MOD ReassCtx & ctx)
{
    if (!ir->isBinaryOp()) { return false; }
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    tryCommutativeBinOp(op0, ctx);
    tryCommutativeBinOp(op1, ctx);
    if (!ir->is_commutative()) { return false; }
    if (!op0->is_commutative() && op1->is_commutative()) {
        //Make sure the commutative operand always be placed in the 0th
        //operand to facilitate the subsequently expression combination.
        ((CBin*)ir)->swapOpnd();
        return true;
    }
    return false;
}


bool AlgeIntlImpl::tryReformBinOp(
    MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx)
{
    ASSERT0(ir->isBinaryOp());
    if (ir->is_div()) {
        return tryReformDIV(ir, evaled, ctx);
    }
    //Still cannot reassociate.
    return false;
}


bool AlgeIntlImpl::tryEvalAndReformConstOp(
    MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx)
{
    ASSERT0(ir->isConstExp());
    AlgeReassociate * reass = ctx.getAlgeReass();
    if (reass->canBeCandConstOp(ir)) {
        *evaled = ctx.getReassExpMgr().genConstReassExp(ir);
        return true;
    }
    //The const-exp does not support reassociation.
    //And reform is useless.
    return false;
}


//Return true if ir can be reassociated and evaluated to a reass-exp.
bool AlgeIntlImpl::tryEvalAndReformDirectOp(
    MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx)
{
    //DirectMemOp always can be the candidate of evaluation.
    ASSERT0(ir->isDirectMemOp() || ir->isPROp());
    ASSERT0(evaled);
    *evaled = ctx.getReassExpMgr().genDirectReassExp(ir);
    return true;
}


//Return true if ir can be reassociated and evaluated to a reass-exp.
bool AlgeIntlImpl::tryEvalAndReformBinOp(
    MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx)
{
    ASSERT0(ir->isBinaryOp());
    AlgeIntlImpl::tryCommutativeBinOp(ir, ctx);
    if (ctx.getAlgeReass()->canBeCandBinOp(ir)) {
        *evaled = ctx.getReassExpMgr().genBinReassExp(ir);
        return true;
    }
    //The binary-op does not support reassociation, try reforming ir.
    if (!tryReformBinOp(ir, evaled, ctx)) { return false; }
    ASSERT0(evaled && *evaled);
    return true;
}


bool AlgeIntlImpl::evalAndReformExp(
    MOD IR * ir, ReassExp ** evaled, MOD ReassCtx & ctx)
{
    switch (ir->getCode()) {
    SWITCH_CASE_BIN:
        if (!AlgeIntlImpl::tryEvalAndReformBinOp(ir, evaled, ctx)) {
            //Terminate the propagation of linearizing.
            return false;
        }
        break;
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
        if (!AlgeIntlImpl::tryEvalAndReformDirectOp(ir, evaled, ctx)) {
            //Terminate the propagation of linearizing.
            return false;
        }
        break;
    case IR_CONST:
        if (!AlgeIntlImpl::tryEvalAndReformConstOp(ir, evaled, ctx)) {
            //Terminate the propagation of linearizing.
            return false;
        }
        break;
    default:
        //For now, only consider reassociate binary and const op.
        //Terminate the propagation of linearizing and try to reassociate
        //OPs that recorded in opvec.
        return false;
    }
    //For now, evalated-reass-information is ready for reassociation.
    ASSERT0(evaled && *evaled);
    AlgeReassociate * reass = ctx.getAlgeReass();
    if (!reass->isOpCodeConsistent(*evaled, ctx)) {
        //Current op is not consistent with other ops that already in opvec.
        //Terminate the propagation of linearizing and try to reassociate
        //OPs that recorded in opvec in ctx.
        return false;
    }
    return true;
}


RANK AlgeIntlImpl::computeRankViaBinOp(
    ReassExp const* reassexp, ReassCtx const& ctx)
{
    ASSERT0(reassexp);
    RANK rank = computeRankViaMultiOpnd(reassexp, ctx);
    rank = rank + 1;
    return rank;
}


RANK AlgeIntlImpl::computeRankViaMultiOpnd(
    ReassExp const* reassexp, ReassCtx const& ctx)
{
    ASSERT0(reassexp);
    RANK rank = RANK_UNDEF;
    for (UINT i = 0; i < reassexp->getOpndNum(); i++) {
        IR const* opnd = reassexp->getOpnd(i);
        if (opnd == nullptr) { continue; }
        rank = MAX(rank, ctx.getRank(opnd));
    }
    return rank;
}


RANK AlgeIntlImpl::computeRankViaKid(IR const* ir, MOD ReassCtx & ctx)
{
    ASSERT0(ir);
    RANK rank = ctx.getRank(ir);
    if (rank != RANK_UNDEF) { return rank; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR const* kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        rank = MAX(rank, ctx.getRank(kid));
    }
    ctx.setRank(ir, rank);
    return rank;
}


RANK AlgeIntlImpl::computeRankViaConstExpTree(IR const* ir, MOD ReassCtx & ctx)
{
    ASSERT0(ir && isConstExpTree(ir));
    RANK rank = ctx.getRank(ir);
    if (rank != RANK_UNDEF) { return rank; }
    rank = ctx.getAlgeReass()->getLowestRank();
    ASSERT0(rank != RANK_UNDEF);
    ctx.setRank(ir, rank);
    return rank;
}


RANK AlgeIntlImpl::computeRankViaConst(IR const* ir, MOD ReassCtx & ctx)
{
    ASSERT0(ir && ir->is_const());
    RANK rank = ctx.getRank(ir);
    if (rank != RANK_UNDEF) { return rank; }
    rank = ctx.getAlgeReass()->getLowestRank();
    ASSERT0(rank != RANK_UNDEF);
    ctx.setRank(ir, rank);
    return rank;
}


RANK AlgeIntlImpl::computeRankViaReassExp(
    ReassExp const* reassexp, MOD ReassCtx & ctx)
{
    switch (reassexp->getCode()) {
    SWITCH_CASE_BIN:
        return computeRankViaBinOp(reassexp, ctx);
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
        return computeRankViaMultiOpnd(reassexp, ctx);
    case IR_CONST:
        return computeRankViaConst(reassexp->getOpnd(0), ctx);
    default: UNREACHABLE();
    }
    return RANK_UNDEF;
}


bool AlgeIntlImpl::isTransferOpCode(IR_CODE code)
{
    return IR::isDirectMemOp(code) || code == IR_PR;
}


void AlgeIntlImpl::trySetCodeViaRHS(
    IR const* rhs, ReassExp const* reassexp, MOD LinOpVec & opvec,
    ReassCtx const& ctx)
{
    if (opvec.getCode() != IR_UNDEF) { return; }
    ASSERT0(rhs);
    if (isTransferOpCode(rhs->getCode())) { return; }
    if (rhs->is_const()) { return; }

    //Set the opcode for the root processing of an evaluated reass-exp.
    ASSERT0(reassexp->getCode() != IR_UNDEF);
    ASSERT0(ctx.getAlgeReass()->canBeReass(reassexp->getCode()));
    opvec.setCode(reassexp->getCode());
}


bool AlgeIntlImpl::linearRHS(MOD IR * rhs, MOD ReassCtx & ctx)
{
    ASSERT0(rhs);
    AlgeReassociate * reass = ctx.getAlgeReass();
    ReassExp * reassexp = nullptr;
    if (!evalAndReformExp(rhs, &reassexp, ctx)) {
        return false;
    }
    ASSERT0(reassexp);
    LinOpVec & opvec = ctx.getLinOpVec();
    trySetCodeViaRHS(rhs, reassexp, opvec, ctx);
    ASSERT0(reass->isOpCodeConsistent(reassexp, ctx));
    bool succ = reass->linearReassExp(reassexp, ctx);
    RANK rank = ctx.getRank(rhs);
    if (rank != RANK_UNDEF) { return succ; }
    rank = AlgeIntlImpl::computeRankViaReassExp(reassexp, ctx);
    if (rank == RANK_UNDEF) { return succ; }
    ctx.setRank(rhs, rank);
    return succ;
}


//
//START AlgeReassociate
//
void AlgeReassociate::reset()
{
    m_am.clean();
}


bool AlgeReassociate::initDepPass(MOD OptCtx & oc)
{
    PassTypeList optlist;
    optlist.append_tail(PASS_RPO);

    //Maintain DU chain for new generatged reassocicate expression need
    //DOM info.
    optlist.append_tail(PASS_DOM);
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, optlist);
    m_refine = (Refine*)m_rg->getPassMgr()->registerPass(PASS_REFINE);
    m_simp = (IRSimp*)m_rg->getPassMgr()->registerPass(PASS_IRSIMP);
    ASSERT0(m_refine && m_simp);
    return true;
}


bool AlgeReassociate::canBeReass(IR const* ir) const
{
    //NOTE:a-b-c can be linearized to {+ a (-b) (-c)}
    return ir->is_commutative() && ir->is_associative();
}


bool AlgeReassociate::canBeReass(IR_CODE c) const
{
    //NOTE:a-b-c can be linearized to {+ a (-b) (-c)}
    return IR::isCommutativeOp(c) && IR::isAssociativeOp(c);
}


bool AlgeReassociate::canBeCandBinOp(IR const* ir) const
{
    ASSERT0(ir->isBinaryOp());
    if (ir->is_judge()) {
        //Reassociate logical operation may incur short-cut
        //evaluation problem.
        return false;
    }
    if (!isTypeSafeToCommutate(ir)) { return false; }
    if (ir->is_div()) {
        if (g_do_alge_reassociate_aggressive) {
            //Reassociate divsion may casuse undefined behaviours.
            //a/b/c can not be reassociated to a/(b/c).
            return ir->isRecipOp();
        }
        return false;
    }
    return canBeReass(ir);
}


bool AlgeReassociate::canBeCandStmt(IR const* ir) const
{
    ASSERT0(ir->is_stmt());
    if (!ir->isStoreStmt()) { return false; }
    if (ir->isMayThrow(true)) { return false; }
    if (ir->isPartialStoreStmt()) { return false; }
    if (ir->isWritePartialPR()) { return false; }
    return true;
}


bool AlgeReassociate::dump() const
{
    if (!getRegion()->isLogMgrInit() || !g_dump_opt.isDumpAlgeReassociate()) {
        return true;
    }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_am.dump();
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


bool AlgeReassociate::linearConstExpTree(IR const* ir, MOD ReassCtx & ctx) const
{
    AlgeIntlImpl::computeRankViaConstExpTree(ir, ctx);
    ctx.getLinOpVec().append(ir);
    return true;
}


bool AlgeReassociate::linearConst(IR const* ir, MOD ReassCtx & ctx) const
{
    AlgeIntlImpl::computeRankViaConst(ir, ctx);
    ctx.getLinOpVec().append(ir);
    return true;
}


bool AlgeReassociate::linearConstOpForReassExp(
    ReassExp const* reassexp, MOD ReassCtx & ctx) const
{
    ASSERT0(IR_CONST == reassexp->getCode());
    IR const* op = reassexp->getOpnd(0);
    linearExp(op, ctx);

    //NOTE:The caller ensure that the binary operation should satisfy
    //the consistency of current linearization.
    return true;
}


bool AlgeReassociate::linearDirectOpForReassExp(
    ReassExp const* reassexp, MOD ReassCtx & ctx) const
{
    ASSERT0(IR::isDirectMemOp(reassexp->getCode()) ||
            reassexp->getCode() == IR_PR);
    ASSERT0(isOpCodeConsistent(reassexp, ctx));
    IR const* op = reassexp->getOpnd(0);
    linearExp(op, ctx);

    //NOTE:The caller ensure that the binary operation should satisfy
    //the consistency of current linearization.
    return true;
}


bool AlgeReassociate::linearBinOpForReassExp(
    ReassExp const* reassexp, MOD ReassCtx & ctx) const
{
    ASSERT0(IR::isBinaryOp(reassexp->getCode()));
    ASSERT0(isOpCodeConsistent(reassexp, ctx));
    IR const* op0 = reassexp->getOpnd(0);
    IR const* op1 = reassexp->getOpnd(1);
    linearExp(op0, ctx);
    linearExp(op1, ctx);

    //NOTE:The caller ensure that the binary operation should satisfy
    //the consistency of current linearization.
    return true;
}


inline static RANK makeBBRank(RANK rank, UINT ir_rank_range_bitsize)
{
    return rank << ir_rank_range_bitsize;
}


inline static RANK extractBBRank(RANK rank, UINT ir_rank_range_bitsize)
{
    return rank >> ir_rank_range_bitsize;
}


inline static RANK composeRank(RANK bbrank, RANK irrank)
{
    return bbrank | irrank;
}


static RANK combineBBRank(IR const* ir, RANK irrank, ReassCtx const& ctx)
{
    IRBB const* bb = nullptr;
    if (ir->is_stmt()) {
        bb = ir->getBB();
    } else {
        ASSERT0(ir->is_exp());
        bb = ir->getStmt()->getBB();
    }
    ASSERT0(bb);
    RANK bbrank = ctx.getRank(bb);
    UINT ir_rank_range_bitsize = ctx.getIRRankRangeBitSize();
    ASSERT0(!xcom::isExceedBitWidth(
        (ULONGLONG)extractBBRank(bbrank, ir_rank_range_bitsize),
        ctx.getBBRankRangeBitSize()));
    ASSERT0(!xcom::isExceedBitWidth(
        (ULONGLONG)irrank, ir_rank_range_bitsize));
    return composeRank(bbrank, irrank);
}


bool AlgeReassociate::hasSideEffect(IR const* ir) const
{
    return ir->hasSideEffect(true);
}


void AlgeReassociate::setAtLeastLowestRank(
    IR const* ir, MOD ReassCtx & ctx) const
{
    RANK rank = ctx.getRank(ir);
    if (rank != RANK_UNDEF) { return; }
    rank = getLowestRank();
    ctx.setRank(ir, rank);
}


void AlgeReassociate::setAtLeastCombineRank(
    IR const* ir, MOD ReassCtx & ctx) const
{
    RANK rank = ctx.getRank(ir);
    if (rank != RANK_UNDEF) { return; }
    rank = getLowestRank();
    rank = combineBBRank(ir, rank, ctx);
    ctx.setRank(ir, rank);
}


void AlgeReassociate::setRankByOther(
    IR const* ir, IR const* ref, MOD ReassCtx & ctx) const
{
    if (ctx.getRank(ir) != RANK_UNDEF) { return; }
    RANK rank = ctx.getRank(ref);
    if (rank == RANK_UNDEF) { return; }
    ctx.setRank(ir, rank);
}


bool AlgeReassociate::linearBinOp(IR const* ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir && ir->is_exp());
    ASSERT0(ir->isBinaryOp());
    if (!isOpCodeConsistent(ir->getCode(), ctx)) {
        //Record the op. It will be treated as an operand of the linearized-op.
        setAtLeastCombineRank(ir, ctx);
        ctx.getLinOpVec().append(ir);
        return false;
    }
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    linearExp(op0, ctx);
    linearExp(op1, ctx);

    //NOTE:The caller ensure that the binary operation should satisfy
    //the consistency of current linearization.
    return true;
}


bool AlgeReassociate::linearDirectMemExp(IR const* ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir && ir->is_exp());
    ASSERT0(ir->isMemOpnd() || ir->isReadPR());
    if (hasSideEffect(ir)) { return false; }
    IR * kdef = xoc::findKillingDef(ir, m_rg, ctx.getOptCtx());
    if (kdef == nullptr) {
        //Record the direct-mem-op. It will be treated as an operand of
        //the linearized-op.
        setAtLeastCombineRank(ir, ctx);
        ctx.getLinOpVec().append(ir);
        return false;
    }
    ASSERT0(kdef->hasResult());
    if (kdef->isPartialStoreStmt()) {
        //Record the direct-mem-op. It will be treated as an operand of
        //the linearized-op.
        setAtLeastCombineRank(ir, ctx);
        ctx.getLinOpVec().append(ir);
        return false;
    }
    bool succ = computeRankAndLinearExp(kdef, ctx);
    setRankByOther(ir, kdef, ctx);
    if (succ) {
        //There is no need to record the exp in lin-op-vec that is merely
        //used to propagate dependencies.
        return true;
    }
    //Record the direct-mem-op. It will be treated as an operand of
    //the linearized-op.
    ASSERT0(ctx.getRank(ir) != RANK_UNDEF);
    ctx.getLinOpVec().append(ir);
    return false;
}


bool AlgeReassociate::linearExp(IR const* ir, MOD ReassCtx & ctx) const
{
    switch (ir->getCode()) {
    case IR_CONST: return linearConst(ir, ctx);
    SWITCH_CASE_BIN:
        if (isConstExpTree(ir)) {
            return linearConstExpTree(ir, ctx);
        }
        return linearBinOp(ir, ctx);
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
        return linearDirectMemExp(ir, ctx);
    SWITCH_CASE_DEBUG:
    SWITCH_CASE_READ_ARRAY:
    case IR_LDA:
    SWITCH_CASE_UNA:
    SWITCH_CASE_INDIRECT_MEM_EXP:
    case IR_SELECT:
    case IR_DUMMYUSE:
    case IR_CASE:
    case IR_ID: {
        RANK rank = ctx.getRank(ir);
        if (rank == RANK_UNDEF) {
            rank = computeRankOfNonAlgeExp(ir);
            ctx.setRank(ir, rank);
        }
        return false;
    }
    default:
        ASSERT0(ir->isExtOp());
        return linearExtOp(ir, ctx);
    }
    return false;
}


//The function computes the rank for extended operations according to
//ReassExp information.
bool AlgeReassociate::linearExtOpForReassExp(
    ReassExp const* reassexp, MOD ReassCtx & ctx) const
{
    ASSERT0(IR::isExtOp(reassexp->getCode()));
    ASSERT0(isOpCodeConsistent(reassexp, ctx));
    for (UINT i = 0; i < reassexp->getOpndNum(); i++) {
        IR const* opnd = reassexp->getOpnd(i);
        if (opnd == nullptr) { continue; }
        linearExp(opnd, ctx);
    }
    //NOTE:The caller ensure that the extended operation should satisfy
    //the consistency of current linearization.
    return true;
}


bool AlgeReassociate::linearReassExp(
    ReassExp const* reassexp, MOD ReassCtx & ctx) const
{
    switch (reassexp->getCode()) {
    SWITCH_CASE_BIN: return linearBinOpForReassExp(reassexp, ctx);
    case IR_CONST: return linearConstOpForReassExp(reassexp, ctx);
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
        return linearDirectOpForReassExp(reassexp, ctx);
    default:
        ASSERT0(IR::isExtOp(reassexp->getCode()));
        return linearExtOpForReassExp(reassexp, ctx);
    }
    return false;
}


bool AlgeReassociate::isOpCodeConsistent(
    ReassExp const* reassexp, ReassCtx const& ctx) const
{
    return isOpCodeConsistent(reassexp->getCode(), ctx);
}


bool AlgeReassociate::isOpCodeConsistent(
    IR_CODE reasscode, ReassCtx const& ctx) const
{
    ASSERT0(reasscode != IR_UNDEF);
    if (reasscode == IR_CONST) {
        //CONST is always consistent to other op.
        return true;
    }
    if (reasscode == IR_PR || IR::isDirectMemOp(reasscode)) {
        //Direct operations are always consistent to other op.
        return true;
    }
    IR_CODE opcode = const_cast<ReassCtx&>(ctx).getLinOpVec().getCode();
    if (opcode == IR_UNDEF) {
        //ir is the first op, of cause is consistent.
        return true;
    }
    if (reasscode == opcode) {
        //ir is same with other op, of cause is consistent.
        return true;
    }
    if (reasscode == IR_MUL && opcode == IR_DIV) {
        //DIV is equivalent to multiplying it by the reciprocal.
        //e.g: a / b = a * (1/b), thus a / b * c = a * (1/b) * c.
        return true;
    }
    return opcode == IR_UNDEF || reasscode == opcode;
}


RANK AlgeReassociate::computeRankOfNonAlgeExp(IR const* ir) const
{
    return getLowestRank() + 1;
}


bool AlgeReassociate::linearExpViaStoreStmt(
    MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->isStoreStmt() || ir->isVirtualOp());
    bool succ = false;
    if (canBeCandStmt(ir)) {
        ASSERT0(!ir->isPartialStoreStmt());
        succ = AlgeIntlImpl::linearRHS(ir->getRHS(), ctx);
    }
    if (ctx.getRank(ir) == RANK_UNDEF) {
        RANK rank = ctx.getRank(ir->getRHS());
        if (rank == RANK_UNDEF) {
            rank = computeRankOfNonAlgeExp(ir->getRHS());
        }
        ASSERT0(rank != RANK_UNDEF);
        ctx.setRank(ir, rank);
    }
    return succ;
}


bool AlgeReassociate::foldConstLast(MOD ReassCtx & ctx) const
{
    LinOpVec & linopvec = ctx.getLinOpVec();
    IR_CODE opc = linopvec.getCode();
    switch (opc) {
    SWITCH_CASE_BIN:
        return foldConstLastTwoOp(ctx);
    default: ;//TODO:support more opcode.
    }
    return false;
}

bool AlgeReassociate::isSafeToFoldConstBinOp(
    IR_CODE opc, IR const* op0, IR const* op1) const
{
    if (!isSafeToFoldConstBinOpInCase1(opc, op0, op1)) { return false; }
    if (!isSafeToFoldConstBinOpInCase2(opc, op0, op1)) { return false; }
    return true;
}


bool AlgeReassociate::isSafeToFoldConstBinOpInCase2(
    IR_CODE opc, IR const* op0, IR const* op1) const
{
    //CASE:(1 / 20) * 40 => 40 / 20
    ASSERT0(opc != IR_UNDEF);
    ASSERT0(IR::isBinaryOp(opc));
    if (opc != IR_MUL) { return true; }
    if (!op0->is_div()) { return true; }
    if (!op0->is_int()) {
        //The refinement only handles integer.
        return false;
    }

    //(1 / z) * x => x / z
    IR * num_of_op0 = BIN_opnd0(op0); //imm 1
    if (!num_of_op0->is_int()) {
        //The refinement only handles integer.
        return false;
    }
    IR * den_of_op0 = BIN_opnd1(op0); //z
    if (!den_of_op0->is_int()) {
        //The refinement only handles integer.
        return false;
    }
    if (!op1->is_int()) { return false; }
    if (!IRMgr::isConstOne(num_of_op0)) { return false; }
    if (!op1->is_const() || !den_of_op0->is_const()) {
        //For conservative purpose, DIV may truncate integer value.
        //We only handles the compile time literal value.
        return false;
    }
    if ((CONST_int_val(op1) % CONST_int_val(den_of_op0)) != 0) {
        //For conservative purpose, integer DIV may truncate integer value.
        //We only consider the case in which op1 divides den_of_op0.
        //e.g:(1/20)*40 can be folded into 40/20, however (1/20)*30 cannot.
        return false;
    }
    return true;
}


bool AlgeReassociate::isSafeToFoldConstBinOpInCase1(
    IR_CODE opc, IR const* op0, IR const* op1) const
{
    //CASE: 40 * (1 / 20) => 40 / 20
    ASSERT0(opc != IR_UNDEF);
    ASSERT0(IR::isBinaryOp(opc));
    if (opc != IR_MUL) { return true; }
    if (!op1->is_div()) { return true; }
    if (!op1->is_int()) {
        //The refinement only handles integer.
        return false;
    }

    //x * (1 / z) => x / z
    IR * num_of_op1 = BIN_opnd0(op1); //imm 1
    if (!num_of_op1->is_int()) {
        //The refinement only handles integer.
        return false;
    }
    IR * den_of_op1 = BIN_opnd1(op1); //z
    if (!den_of_op1->is_int()) {
        //The refinement only handles integer.
        return false;
    }
    if (!op0->is_int()) { return false; }
    if (!IRMgr::isConstOne(num_of_op1)) { return false; }
    if (!op0->is_const() || !den_of_op1->is_const()) {
        //For conservative purpose, DIV may truncate integer value.
        //We only handles the compile time literal value.
        return false;
    }
    if ((CONST_int_val(op0) % CONST_int_val(den_of_op1)) != 0) {
        //For conservative purpose, integer DIV may truncate integer value.
        //We only consider the case in which op1 divides den_of_op0.
        //e.g:40*(1/20) can be folded into 40/20, however (1/20)*30 cannot.
        return false;
    }
    return true;
}


bool AlgeReassociate::foldConstLastTwoOp(MOD ReassCtx & ctx) const
{
    LinOpVec & linopvec = ctx.getLinOpVec();
    IR_CODE opc = linopvec.getCode();
    ASSERT0(opc != IR_UNDEF);
    ASSERT0(IR::isBinaryOp(opc));
    if (linopvec.get_elem_count() < 2) { return false; }
    ASSERT0(canBeReass(opc));
    VecIdx last1 = linopvec.get_last_idx();
    VecIdx last2 = linopvec.get_last_idx() - 1;
    ASSERT0(last1 != VEC_UNDEF);
    ASSERT0(last2 != VEC_UNDEF);
    IR const* last1_ir = linopvec.get(last1);
    IR const* last2_ir = linopvec.get(last2);
    if (!isConstExpTree(last1_ir) || !isConstExpTree(last2_ir)) {
        return false;
    }
    if (!isSafeToFoldConstBinOp(opc, last1_ir, last2_ir)) { return false; }
    Type const* opty = m_tm->hoistDTypeForBinOp(last1_ir, last2_ir);
    ASSERT0(opty);
    IR * newir = m_irmgr->buildBinaryOpSimp(opc,
       opty, m_rg->dupIRTree(last1_ir), m_rg->dupIRTree(last2_ir));
    ASSERT0(m_refine);
    RefineCtx rc(ctx.getOptCtx());
    bool change = false;
    newir = m_refine->refineExpression(newir, change, rc);
    ASSERT0(newir->is_const() && change);
    dumpFoldConst(opc, last1_ir, last2_ir, newir, this);

    //Compute the rank for newir.
    linearConst(newir, ctx);
    linopvec.cleanLastFrom(last2);
    linopvec.append(newir);
    return true;
}


//It is not worth to reassociate expressions.
IR * AlgeReassociate::reassociatedExp(
    OUT IRVec & reassopvec, ReassCtx const& ctx) const
{
    LinOpVec const& linopvec = const_cast<ReassCtx&>(ctx).getLinOpVec();
    if (linopvec.get_elem_count() == 0) { return nullptr; }
    IR_CODE opcode = linopvec.getCode();
    ASSERT0(opcode);
    ASSERT0(IR::isBinaryOp(opcode));
    IR * reass = nullptr;
    for (VecIdx i = linopvec.get_last_idx(); i != VEC_UNDEF; i--) {
        IR const* opnd = linopvec.get(i);
        ASSERT0(opnd);
        Type const* ty = opnd->getType();
        if (i == linopvec.get_last_idx()) {
            reass = m_rg->dupIRTree(opnd);
            reassopvec.set(i, reass);
            continue;
        }
        IR * newopnd = m_rg->dupIRTree(opnd);
        reassopvec.set(i, newopnd);
        ASSERT0(reass);
        reass = m_irmgr->buildBinaryOpSimp(opcode, ty, newopnd, reass);
    }
    return reass;
}


void AlgeReassociate::buildDUChainForReassExp(
    IRVec const& reassopvec, ReassCtx const& ctx) const
{
    LinOpVec const& linopvec = const_cast<ReassCtx&>(ctx).getLinOpVec();
    ASSERT0(linopvec.get_elem_count() == reassopvec.get_elem_count());
    for (VecIdx i = 0; i < (VecIdx)reassopvec.get_elem_count(); i++) {
        //IR const* orgop = linopvec.get(i);
        //ASSERT0(orgop);
        //if (!orgop->isMemOpnd()) { continue; }
        IR * reassop = reassopvec.get(i);
        if (!reassop->isMemOpnd()) {
            //reassop may be const.
            continue;
        }
        //xoc::addUse(reassop, orgop, m_rg);
        IR * reassop_stmt = reassop->getStmt();
        IRBB * startbb = reassop_stmt->getBB();
        IR * startir = startbb->getPrevIR(reassop_stmt);
        xoc::findAndSetLiveInDefForTree(
            reassop, startir, startbb, m_rg, *ctx.getOptCtx());
    }
}


static void simplifyStmtAndInsertToBB(
    MOD IR * ir, Region const* rg, ReassCtx const& ctx,
    AlgeReassociate const* reass)
{
    ASSERT0(ir->is_stmt());
    SimpCtx simpctx(ctx.getOptCtx());
    simpctx.setSimpLandLor();
    simpctx.setSimpLnot();
    simpctx.setSimpToLowestHeight();
    IRSimp * simppass = reass->getIRSimp();
    ASSERT0(simppass);
    dumpSimpStmt(ir, reass);
    IR * newir = simppass->simplifyStmt(ir, &simpctx);
    ASSERT0(newir);
    ASSERT0(simpctx.getStmtList() == nullptr);
    ASSERT0(!simpctx.needReconstructBBList());
    IRListIter it;
    IRBB * irbb = ir->getBB();
    BBIRList & irlst = irbb->getIRList();
    bool find = irlst.find(ir, &it);
    ASSERT0_DUMMYUSE(find);
    for (IR * x = xcom::removehead(&newir);
         x != nullptr; x = xcom::removehead(&newir)) {
        if (x == ir) { continue; }
        ASSERT0(x->is_stmt());
        irlst.insert_before(x, it);
    }
}


//The fuction dectects the following case:
//e.g:ir1: add (ld x, #1)
//    ir2: ild (ld x)
//  then the funtion returns true because they both reference ld x.
static bool doBothIRTreeRefSameMemRef(
    IR const* ir1, IR const* ir2, IRMgr const* mgr)
{
    ConstIRIter it1;
    ConstIRIter it2;
    for (IR const* x1 = xoc::iterInitC(ir1, it1);
         x1 != nullptr; x1 = xoc::iterNextC(it1)) {
        if (!x1->isMemRef()) { continue; }
        for (IR const* x2 = xoc::iterInitC(ir2, it2);
             x2 != nullptr; x2 = xoc::iterNextC(it2)) {
            if (!x2->isMemRef()) { continue; }
            if (x1->isMemRefEqual(x2, mgr)) {
                return true;
            }
        }
    }
    return false;
}


static Var const* getVarOfRef(IR const* ir, Region const* rg)
{
    ASSERT0(ir && rg);
    if (ir->isPROp()) {
        return rg->getVarByPRNO(ir->getPrno());
    }
    if (ir->isMemRef() && ir->hasIdinfo()) {
        return ir->getIdinfo();
    }
    return nullptr;
}


//Return true if ir is being processed repeatedly for the same MemRef.
static bool checkAndRecordIfAccessSameVarMoreThanTwice(
    IR const* ir, IR const* reass, MOD ReassCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->is_stmt() && ir->isStoreStmt() && ir->hasRHS());

    //CASE:
    //    $7=ld e + 4
    //    st e = $7
    //    $10=$7 + 20 #S1
    //  after REASS:
    //    $7=ld e + 4
    //    st e = $7
    //    $10=ld e + 20 #S1
    //  then after CP:
    //    $7=ld e + 4
    //    st e = $7
    //    $10= $7 + 20 #S1
    //When doing reass for #S1, we found 'e' referenced twice, that caused
    //the pass reprocess $7 again, and lead to a endless-loop compilation.
    ConstIRIter it;
    VarRefMgr & vrmgr = ctx.getAlgeReass()->getVarRefMgr();
    Region const* rg = ctx.getRegion();
    ConstVarTab const* irvrt = vrmgr.getAndGenVRT(ir);
    ASSERT0(irvrt);
    for (IR const* t = xoc::iterInitC(reass, it);
         t != nullptr; t = xoc::iterNextC(it)) {
        Var const* tv = getVarOfRef(t, rg);
        if (tv == nullptr) { continue; }
        if (irvrt->find(tv)) { return true; }

        //Record the #S1 that modifies '$10' to ref-var 'e' into the context.
        //The info will be recorded to pass-object until entire pass-object
        //destroy.
        ctx.recordIR2Var(ir, tv);
    }
    return false;
}


//Return true if there are stmt that might cause endless loop compilation.
static bool detectPossibleEndlessLoop(
    IR const* ir, IR const* reass, ReassCtx & ctx)
{
    ASSERT0(ir);
    ASSERT0(ir->is_stmt() && ir->isStoreStmt() && ir->hasRHS());
    IR const* orgrhs = ir->getRHS();
    if (doBothIRTreeRefSameMemRef(orgrhs, reass, ctx.getIRMgr())) {
        return true;
    }
    if (checkAndRecordIfAccessSameVarMoreThanTwice(ir, reass, ctx)) {
        return true;
    }
    return false;
}


//Return true if given 'ir' has been rewrote.
bool AlgeReassociate::replaceRHSWithReassExp(
    MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->is_stmt() && ir->isStoreStmt() && ir->hasRHS());
    IRVec reassopvec;
    IR * reass = reassociatedExp(reassopvec, ctx);
    if (reass == nullptr) { return false; }
    if (detectPossibleEndlessLoop(ir, reass, ctx)) {
        m_rg->freeIRTree(reass);
        return false;
    }
    //Maintain DU chain.
    IR * orgrhs = ir->getRHS();
    xoc::removeUseForTree(orgrhs, m_rg, *ctx.getOptCtx());
    dumpReplaceExp(orgrhs, reass, this);
    ctx.tryInvalidInfoBeforeFreeIR(orgrhs);
    m_rg->freeIRTree(orgrhs);
    ir->setRHS(reass);
    buildDUChainForReassExp(reassopvec, ctx);
    simplifyStmtAndInsertToBB(ir, m_rg, ctx, this);
    ctx.setRecompGVN(true);
    return true;
}


bool AlgeReassociate::reassociate(MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->is_stmt() && ir->isStoreStmt() && ir->hasRHS());
    ASSERT0(ctx.verify());
    LinOpVecSort sort(ctx);
    LinOpVec & linopvec = ctx.getLinOpVec();
    sort.sort(linopvec);
    bool changed = false;
    bool lchanged = false;
    do {
        lchanged = foldConstLast(ctx);
        if (!lchanged) { break; }
        changed |= lchanged;
    } while (lchanged);
    if (!changed) { return false; }
    return replaceRHSWithReassExp(ir, ctx);
}


bool AlgeReassociate::linearExpViaPhi(MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->is_phi());

    //Always compute the rank of PHI and ensure it is the maximum
    //topological order in the data-flow dependence graph.
    setAtLeastCombineRank(ir, ctx);

    //PHI operand can not be linearized.
    return false;
}


bool AlgeReassociate::computeRankAndLinearExp(
    MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_STPR:
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_EXT_WRITE_PR:
        return linearExpViaStoreStmt(ir, ctx);
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_CALL:
    case IR_ICALL:
    case IR_IGOTO:
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
    case IR_RETURN:
    case IR_GOTO:
    case IR_REGION:
        break;
    case IR_PHI:
        return linearExpViaPhi(ir, ctx);
    default:
        ASSERT0(ir->isExtOp());
        return linearExtOp(ir, ctx);
    }
    return false;
}


bool AlgeReassociate::linearExtOp(IR const* ir, MOD ReassCtx & ctx) const
{
    RANK rank = ctx.getRank(ir);
    if (rank == RANK_UNDEF) {
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR const* kid = ir->getKid(i);
            if (kid == nullptr) { continue; }
            linearExp(kid, ctx);
            RANK kidrank = ctx.getRank(kid);
            //NOTE: kid rank may be UNDEF.
            rank = MAX(kidrank, rank);
        }
        //Always compute the rank of IR and ensure it is the maximum topological
        //order in the data-flow dependence graph.
        rank = combineBBRank(ir, rank, ctx);
        ASSERT0(rank != RANK_UNDEF);
        ctx.setRank(ir, rank);
    }
    //There is no knowledge about the extended operation that wether it
    //can not be linearized.
    return false;
}


bool AlgeReassociate::computeRankAndReassForBB(
    IRBB const* bb, MOD ReassCtx & ctx) const
{
    bool changed = false;
    BBIRListIter it;
    BBIRList & irlst = const_cast<IRBB*>(bb)->getIRList();
    for (IR * ir = irlst.get_tail(&it);
         ir != nullptr; ir = irlst.get_prev(&it)) {
        ctx.cleanBottomUp();
        if (ir->is_phi()) { continue; }
        bool succ = computeRankAndLinearExp(ir, ctx);
        if (!succ) { continue; }
        changed |= reassociate(ir, ctx);
    }
    return changed;
}


static UINT computeMaxIRNumInBB(BBList const* bblst)
{
    UINT irnum = 0;
    BBListIter it;
    for (IRBB const* bb = bblst->get_head(&it);
         bb != nullptr; bb = bblst->get_next(&it)) {
        irnum = MAX(irnum, bb->getNumOfIR());
    }
    return irnum;
}


static void computeBBRank(
    Region const* rg, xcom::RPOVexList const* vexlst,
    UINT ir_rank_range_bitsize, MOD ReassCtx & ctx)
{
    ASSERT0(vexlst);
    xcom::RPOVexListIter it;
    RANK rank = ctx.getCurRank();
    for (vexlst->get_head(&it); it != vexlst->end();
         it = vexlst->get_next(it)) {
        IRBB const* bb = rg->getBB(it->val()->id());
        ASSERT0(bb);
        rank++;
        ctx.setRank(bb, makeBBRank(rank + 1, ir_rank_range_bitsize));
    }
}


bool AlgeReassociate::verifyRank(ReassCtx const& ctx) const
{
    UINT ir_rank_range_bitsize = ctx.getIRRankRangeBitSize();
    ASSERT0(ir_rank_range_bitsize < MAX_IR_BIT_RANGE_IN_BB);
    UINT bb_rank_range_bitsize =
        sizeof(RANK) * BITS_PER_BYTE - ir_rank_range_bitsize;
    UINT bbnum = m_rg->getBBList()->get_elem_count();
    ASSERT0(!xcom::isExceedBitWidth((ULONGLONG)bbnum, bb_rank_range_bitsize));
    return true;
}


void AlgeReassociate::recordHandledReassInfo(ReassCtx const& ctx)
{
    IR2VLst const& lst = ctx.getIR2VLst();
    VarRefMgr & vrmgr = getVarRefMgr();
    UINT i = 0;
    IR2VLstIter it;
    for (IR2Var ir2v = lst.get_head(&it); i < lst.get_elem_count();
         i++, ir2v = lst.get_next(&it)) {
        vrmgr.set(ir2v.key, ir2v.value);
    }
}


bool AlgeReassociate::doReass(MOD ReassCtx & ctx)
{
    bool changed = false;
    UINT irnum = computeMaxIRNumInBB(m_rg->getBBList());
    UINT ir_rank_range_bitsize = xcom::computeMaxBitSizeForValue(
        (ULONGLONG)(irnum + 1) * 2);
    ASSERT0(ir_rank_range_bitsize < MAX_IR_BIT_RANGE_IN_BB);
    ctx.setIRRankRangeBitSize(ir_rank_range_bitsize);
    ASSERT0L3(verifyRank(ctx));
    xcom::RPOVexList const* vexlst = m_cfg->getRPOVexList();
    ASSERT0(vexlst);
    computeBBRank(m_rg, vexlst, ir_rank_range_bitsize, ctx);
    xcom::RPOVexListIter it;
    for (vexlst->get_tail(&it); it != vexlst->end();
         it = vexlst->get_prev(it)) {
        IRBB const* bb = m_rg->getBB(it->val()->id());
        ASSERT0(bb);
        changed |= computeRankAndReassForBB(bb, ctx);
    }
    //Record the res-var to ref-var into pass-object to prevent handling
    //the same pattern over and over again.
    recordHandledReassInfo(ctx);
    return changed;
}


bool AlgeReassociate::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.isPassValid(PASS_MD_REF)) { return false; }

    //Initialize pass object since they might be destructed at any moment.
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    m_irmgr = m_rg->getIRMgr();
    if (!usePRSSADU() || !useMDSSADU()) {
        //AlgeReass prefers using SSA instead of classic DU.
        return false;
    }
    START_TIMER(t, getPassName());
    reset();
    initDepPass(oc);
    ReassCtx ctx(oc, this);
    bool change = doReass(ctx);
    if (!change) {
        END_TIMER(t, getPassName());
        return false;
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpAlgeReassociate()) {
        dump();
    }
    if (ctx.needRecompGVN()) {
        oc.setInvalidPass(PASS_GVN);
    }
    //DU chain and DU reference should be maintained.
    ASSERT0(xoc::verifyMDRef(m_rg, oc) && xoc::verifyClassicDUChain(m_rg, oc));
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    END_TIMER(t, getPassName());
    return true;
}
//END AlgeReassociate

} //namespace xoc
