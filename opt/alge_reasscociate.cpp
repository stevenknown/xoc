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
//START LinOpVec
//
void LinOpVec::dump(ReassCtx const& ctx) const
{
    Region const* rg = ctx.getRegion();
    DefFixedStrBuf buf;
    xoc::note(rg, "\n-- DUMP LinOpVec: CODE:%s --",
              xoc::dumpIRCodeName(getCode(), buf));
    for (VecIdx i = 0; i <(VecIdx)get_elem_count(); i++) {
        IR const* ir = get(i);
        ASSERT0(ir);
        xoc::dumpIR(ir, rg, nullptr, DumpFlag::combineIRID(IR_DUMP_DEF));
        RANK rank = ctx.getRank(ir);
        buf.clean();
        xoc::prt(rg, " rank:%s", ctx.dumpRank(rank, buf));
    }
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
//START ReassCtx
//
//
void ReassCtx::dumpBBListWithRankImpl(MOD IRDumpCtx<> & dumpctx) const
{
    ASSERT0(m_rg);
    BBList const* bbl = m_rg->getBBList();
    ASSERT0(bbl);
    if (!m_rg->isLogMgrInit() || bbl->get_elem_count() == 0) { return; }
    BBListIter it;
    DefFixedStrBuf buf;
    note(m_rg, "\n\n-- DUMP BBLIST WITH RANK --");
    for (IRBB const* bb = bbl->get_head(&it);
         bb != nullptr; bb = bbl->get_next(&it)) {
        RANK rank = getRank(bb);
        if (rank != RANK_UNDEF) {
            buf.clean();
            note(m_rg, "\nBB%u rank:%s", bb->id(), dumpRank(rank, buf));
        }
        bb->dump(m_rg, false, &dumpctx);
    }
}


UINT ReassCtx::getBBRankRangeBitSize() const
{
    return sizeof(RANK) * BITS_PER_BYTE - getIRRankRangeBitSize();
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
    class DumpIRWithRank : public IRDumpAttrBaseFunc {
    public:
        ReassCtx const* ctx;
    public:
        DumpIRWithRank() : ctx(nullptr) {}

        virtual void dumpAttr(
            OUT xcom::DefFixedStrBuf & buf, Region const* rg, IR const* ir,
            DumpFlag dumpflag) const override
        {
            ASSERT0(ctx);
            RANK rank = ctx->getRank(ir);
            if (rank == RANK_UNDEF) { return; }
            DefFixedStrBuf tbuf;
            buf.strcat(" rank:%s", ctx->dumpRank(rank, tbuf));
        }
    };
    DumpIRWithRank dumper;

    //Defin<F5>]5dump flags.
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID | IR_DUMP_SRC_LINE);

    //User defined attributes are VN info.
    dumper.ctx = this;

    //Define dump context.
    IRDumpCtx<> dumpctx(4, f, nullptr, &dumper);
    dumpBBListWithRankImpl(dumpctx);
}


void ReassCtx::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(m_rg, "\n==-- DUMP ReassCtx --==");
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
    m_ir2rank.clean();
}
//END ReassCtx


//
//START AlgeReasscociate
//
void AlgeReasscociate::reset()
{
    m_am.clean();
}


bool AlgeReasscociate::initDepPass(MOD OptCtx & oc)
{
    PassTypeList optlist;
    optlist.append_tail(PASS_RPO);
    if (g_do_refine) { optlist.append_tail(PASS_REFINE); }
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, optlist);
    m_refine = (Refine*)m_rg->getPassMgr()->queryPass(PASS_REFINE);
    return true;
}


bool AlgeReasscociate::canBeCandBinOp(IR const* ir) const
{
    ASSERT0(ir->isBinaryOp());
    if (ir->is_judge()) {
        //Reasscociate logical operation may incur short-cut
        //evaluation problem.
        return false;
    }
    if (!xoc::g_do_opt_float && ir->isFP()) { return false; }
    if (ir->is_div() || ir->is_mod()) {
        //a/b/c can not be reasscociated to a/(b/c)
        return false;
    }
    //a-b-c can be linearized to {+ a (-b) (-c)}
    return ir->is_commutative() && ir->is_associative();
}


bool AlgeReasscociate::canBeCandStmt(IR const* ir) const
{
    ASSERT0(ir->is_stmt());
    if (!ir->isStoreStmt()) { return false; }
    if (ir->isMayThrow(true)) { return false; }
    return true;
}


bool AlgeReasscociate::dump() const
{
    m_am.dump();
    return true;
}


RANK AlgeReasscociate::computeConst(IR const* ir, MOD ReassCtx & ctx) const
{
    RANK rank = getLowestRank();
    ctx.setRank(ir, rank);
    ctx.getLinOpVec().append(ir);
    return rank;
}


RANK AlgeReasscociate::computeBinOp(MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->isBinaryOp());
    ASSERT0(ctx.getRank(ir) == RANK_UNDEF);
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    RANK rank0 = computeRankForExp(op0, ctx);
    if (rank0 == RANK_UNDEF) { return RANK_UNDEF; }
    RANK rank1 = computeRankForExp(op1, ctx);
    if (rank1 == RANK_UNDEF) { return RANK_UNDEF; }
    RANK op_rank = MAX(rank0, rank1);
    //RANK bb_rank = ctx.getRank(ir->getStmt()->getBB());
    //op_rank = MIN(bb_rank, op_rank);
    ctx.setRank(ir, op_rank);
    if (!ir->is_commutative()) { return op_rank; }
    if (!isTypeSafeToCommutate(ir)) { return op_rank; }
    if (!op0->is_commutative() && op1->is_commutative()) {
        //Make sure the commutative operand always be placed in the 0th
        //operand to facilitate the subsequently expression combination.
        ((CBin*)ir)->swapOpnd();
        return op_rank;
    }
    return op_rank;
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
    ASSERT0(!xcom::isExceedBitWidth(
        (ULONGLONG)bbrank, ctx.getBBRankRangeBitSize()));
    ASSERT0(!xcom::isExceedBitWidth(
        (ULONGLONG)irrank, ctx.getIRRankRangeBitSize()));
    return irrank | bbrank;
}


RANK AlgeReasscociate::computeDirectMemExp(
    IR const* ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->isMemOpnd() || ir->isReadPR());
    IR * kdef = xoc::findKillingDef(ir, m_rg);
    if (kdef == nullptr) {
        if (xoc::isRegionLiveIn(ir, m_rg)) {
            RANK rank = getLowestRank();
            rank = combineBBRank(ir, rank, ctx);
            ctx.setRank(ir, rank);
            ctx.getLinOpVec().append(ir);
            return rank;
        }
        return RANK_UNDEF;
    }
    ASSERT0(kdef->hasResult());
    RANK rank = computeRankForStmt(kdef, ctx);
    if (rank == RANK_UNDEF) { return RANK_UNDEF; }
    ctx.setRank(ir, rank);
    return rank;
}


RANK AlgeReasscociate::computeRankForExp(MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    case IR_CONST:
        return computeConst(ir, ctx);
    SWITCH_CASE_BIN:
        return computeBinOp(ir, ctx);
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
        return computeDirectMemExp(ir, ctx);
    SWITCH_CASE_DEBUG:
    SWITCH_CASE_READ_ARRAY:
    case IR_LDA:
    SWITCH_CASE_UNA_REST:
    SWITCH_CASE_INDIRECT_MEM_EXP:
    case IR_SELECT:
    case IR_DUMMYUSE:
    case IR_CASE:
    case IR_ID:
        ASSERT0(0); //TODO
    default:
        ASSERT0(ir->isExtOp());
        return computeRankForExtOp(ir, ctx);
    }
    return RANK_UNDEF;
}


RANK AlgeReasscociate::computeStoreStmt(MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->isStoreStmt());
    if (!canBeCandStmt(ir)) { return RANK_UNDEF; }
    RANK rank = ctx.getRank(ir);
    if (rank != RANK_UNDEF) {
        return rank;
    }
    IR * rhs = ir->getRHS();
    if (!rhs->isBinaryOp() || !canBeCandBinOp(rhs)) {
        //Terminate the propagation of linearizing.
        ctx.getLinOpVec().append(rhs);
        return RANK_UNDEF;
    }
    ctx.getLinOpVec().setCode(rhs->getCode());
    RANK rhs_rank = computeRankForExp(rhs, ctx);
    if (rhs_rank == RANK_UNDEF) { return RANK_UNDEF; }
    ASSERT0(ctx.getRank(rhs) == rhs_rank);
    RANK ir_rank = rhs_rank + 1;
    ctx.setRank(ir, ir_rank);
    return ir_rank;
}


bool AlgeReasscociate::foldConstLastTwoOp(MOD ReassCtx & ctx) const
{
    bool changed = false;
    LinOpVec & linopvec = ctx.getLinOpVec();
    if (linopvec.get_elem_count() < 2) { return false; }
    VecIdx last1 = linopvec.get_last_idx();
    VecIdx last2 = linopvec.get_last_idx() - 1;
    ASSERT0(last1 != VEC_UNDEF);
    ASSERT0(last2 != VEC_UNDEF);
    IR const* last1_ir = linopvec.get(last1);
    IR const* last2_ir = linopvec.get(last2);
    if (!last1_ir->is_const() || !last2_ir->is_const()) { return false; }
    IR_CODE binop = linopvec.getCode();
    ASSERT0(binop != IR_UNDEF);
    Type const* binopty = last1_ir->getType();
    ASSERT0(binopty == last2_ir->getType());
    IR * newir = m_irmgr->buildBinaryOpSimp(binop,
       binopty, m_rg->dupIRTree(last1_ir), m_rg->dupIRTree(last2_ir));
    ASSERT0(m_refine);
    RefineCtx rc(&ctx.getOptCtx());
    bool change = false;
    newir = m_refine->foldConst(newir, change, rc);
    ASSERT0(newir->is_const() && change);

    //Compute the rank for newir.
    computeConst(newir, ctx);
    linopvec.cleanFrom(linopvec.get_last_idx() - 1);
    linopvec.set(last2, newir);
    return true;
}


//It is not worth to reasscociate expressions.
IR * AlgeReasscociate::reasscociatedExp(
    OUT IRVec & reassopvec, ReassCtx const& ctx) const
{
    LinOpVec const& linopvec = const_cast<ReassCtx&>(ctx).getLinOpVec();
    if (linopvec.get_elem_count() <= 2) { return nullptr; }
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


void AlgeReasscociate::buildDUChainForReassExp(
    IRVec const& reassopvec, ReassCtx const& ctx) const
{
    LinOpVec const& linopvec = const_cast<ReassCtx&>(ctx).getLinOpVec();
    ASSERT0(linopvec.get_elem_count() == reassopvec.get_elem_count());
    for (VecIdx i = 0; i < (VecIdx)reassopvec.get_elem_count(); i++) {
        IR const* orgop = linopvec.get(i);
        ASSERT0(orgop);
        if (!orgop->isMemOpnd()) { continue; }
        IR * reassop = reassopvec.get(i);
        ASSERT0(reassop && reassop->isMemOpnd());
        xoc::addUse(reassop, orgop, m_rg);
    }
}


//Return true if given 'ir' has been rewrote.
bool AlgeReasscociate::replaceRHSWithReassExp(
    MOD IR * ir, ReassCtx const& ctx) const
{
    IRVec reassopvec;
    IR * reass = reasscociatedExp(reassopvec, ctx);
    if (reass == nullptr) { return false; }
    IR * orgrhs = ir->getRHS();
    //Maintain DU chain.
    xoc::removeUseForTree(orgrhs, m_rg, ctx.getOptCtx());
    buildDUChainForReassExp(reassopvec, ctx);
    m_rg->freeIRTree(orgrhs);
    ir->setRHS(reass);
    return true;
}


bool AlgeReasscociate::optimizeLinOpVec(MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->is_stmt() && ir->isStoreStmt() && ir->hasRHS());
    LinOpVecSort sort(ctx);
    LinOpVec & linopvec = ctx.getLinOpVec();
    sort.sort(linopvec);
    bool changed = false;
    bool lchanged = false;
    do {
        lchanged = foldConstLastTwoOp(ctx);
        if (!lchanged) { break; }
        changed |= lchanged;
    } while (lchanged);
    if (!changed) { return false; }
    changed |= replaceRHSWithReassExp(ir, ctx);
    return changed;
}


RANK AlgeReasscociate::computePhi(MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->is_phi());
    RANK rank = ctx.getRank(ir->getBB());
    ctx.setRank(ir, rank);
    return rank;
}


RANK AlgeReasscociate::computeRankForStmt(MOD IR * ir, MOD ReassCtx & ctx) const
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_STPR:
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_INDIRECT_MEM_STMT:
        return computeStoreStmt(ir, ctx);
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
        return computePhi(ir, ctx);
    default:
        ASSERT0(ir->isExtOp());
        return computeRankForExtOp(ir, ctx);
    }
    return RANK_UNDEF;
}


bool AlgeReasscociate::computeRankAndReassForBB(
    IRBB const* bb, MOD ReassCtx & ctx) const
{
    bool changed = false;
    BBIRListIter it;
    BBIRList & irlst = const_cast<IRBB*>(bb)->getIRList();
    for (IR * ir = irlst.get_tail(&it);
         ir != nullptr; ir = irlst.get_prev(&it)) {
        ctx.cleanBottomUp();
        RANK stmtrank = computeRankForStmt(ir, ctx);
        if (stmtrank == RANK_UNDEF) { continue; }
        changed |= optimizeLinOpVec(ir, ctx);
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
    Region const* rg, RPOVexList const* vexlst, UINT ir_rank_range_bitsize,
    MOD ReassCtx & ctx)
{
    ASSERT0(vexlst);
    RPOVexListIter it;
    RANK rank = ctx.getCurRank();
    for (vexlst->get_head(&it); it != vexlst->end();
         it = vexlst->get_next(it)) {
        IRBB const* bb = rg->getBB(it->val()->id());
        ASSERT0(bb);
        rank++;
        ctx.setRank(bb, (rank + 1) << ir_rank_range_bitsize);
    }
}


bool AlgeReasscociate::verifyRank(ReassCtx const& ctx) const
{
    UINT ir_rank_range_bitsize = ctx.getIRRankRangeBitSize();
    ASSERT0(ir_rank_range_bitsize < MAX_IR_BIT_RANGE_IN_BB);
    UINT bb_rank_range_bitsize = sizeof(RANK) * BITS_PER_BYTE -
                                 ir_rank_range_bitsize;
    UINT bbnum = m_rg->getBBList()->get_elem_count();
    ASSERT0(!xcom::isExceedBitWidth((ULONGLONG)bbnum, bb_rank_range_bitsize));
    return true;
}


bool AlgeReasscociate::doReass(MOD OptCtx & oc)
{
    bool changed = false;
    ReassCtx ctx(oc, m_rg);
    UINT irnum = computeMaxIRNumInBB(m_rg->getBBList());
    UINT ir_rank_range_bitsize = xcom::computeMaxBitSizeForValue(
        (ULONGLONG)(irnum + 1));
    ASSERT0(ir_rank_range_bitsize < MAX_IR_BIT_RANGE_IN_BB);
    ctx.setIRRankRangeBitSize(ir_rank_range_bitsize);
    ASSERT0L3(verifyRank(ctx));
    RPOVexList const* vexlst = m_cfg->getRPOVexList();
    ASSERT0(vexlst);
    computeBBRank(m_rg, vexlst, ir_rank_range_bitsize, ctx);
    RPOVexListIter it;
    for (vexlst->get_tail(&it); it != vexlst->end();
         it = vexlst->get_prev(it)) {
        IRBB const* bb = m_rg->getBB(it->val()->id());
        ASSERT0(bb);
        changed |= computeRankAndReassForBB(bb, ctx);
    }
    return changed;
}


bool AlgeReasscociate::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    //Initialize pass object since they might be destructed at any moment.
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    m_irmgr = m_rg->getIRMgr();
    if (!usePRSSADU() || !useMDSSADU()) {
        //AlgeReass prefer using SSA instead of classic DU.
        return false;
    }
    START_TIMER(t, getPassName());
    reset();
    initDepPass(oc);
    bool change = doReass(oc);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpAlgeReasscociate()) {
        dump();
    }
    if (!change) {
        END_TIMER(t, getPassName());
        return false;
    }
    //DU chain and DU reference should be maintained.
    ASSERT0(xoc::verifyMDRef(m_rg, oc) && xoc::verifyMDDUChain(m_rg, oc));
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    END_TIMER(t, getPassName());
    return true;
}
//END AlgeReasscociate

} //namespace xoc
