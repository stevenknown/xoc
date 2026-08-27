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
    AlgeDistributive const* dist)
{
    AlgeDistributive * pthis = const_cast<AlgeDistributive*>(dist);
    if (!pthis->getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_ALGE_DISTRIBUTIVE))
    { return; }
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
    IR const* orgrhs, IR const* newrhs, AlgeDistributive const* dist)
{
    AlgeDistributive * pthis = const_cast<AlgeDistributive*>(dist);
    if (!pthis->getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_ALGE_DISTRIBUTIVE))
    { return; }
    xcom::StrBuf s1(32), s2(32);
    pthis->getRegion()->getLogMgr()->incIndent(2);
    xoc::dumpIRToBuf(orgrhs, pthis->getRegion(), s1);
    xoc::dumpIRToBuf(newrhs, pthis->getRegion(), s2);
    pthis->getRegion()->getLogMgr()->decIndent(2);
    pthis->getActMgr().dumpAct(
        "replace %s \n  with %s", s1.getBuf(), s2.getBuf());
}


static void dumpSimpStmt(IR const* ir, AlgeDistributive const* dist)
{
    ASSERT0(ir->is_stmt());
    AlgeDistributive * pthis = const_cast<AlgeDistributive*>(dist);
    if (!pthis->getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_ALGE_DISTRIBUTIVE))
    { return; }
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
//START DistActMgr
//
void DistActMgr::dumpAct(CHAR const* format, ...)
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpPass(PASS_ALGE_DISTRIBUTIVE))
    { return; }
    va_list args;
    va_start(args, format);
    xcom::DefFixedStrBuf buf;
    buf.strcat("DIST:");
    buf.vstrcat(format, args);
    dump("%s", buf.getBuf());
    va_end(args);
}
//END DistActMgr


//
//START DistCtx
//
//
DistCtx::DistCtx(OptCtx & oc, AlgeDistributive * dist)
    : PassCtx(&oc, &dist->getActMgr())
{
    m_dist = dist;
    m_irmgr = oc.getRegion()->getIRMgr();
    m_mdmgr = oc.getRegion()->getMDMgr();
    m_tm = oc.getRegion()->getTypeMgr();
    m_refine = (Refine*)m_rg->getPassMgr()->registerPass(PASS_REFINE);
    ASSERT0(m_refine);
}


DistCtx::~DistCtx()
{
}


void DistCtx::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(m_rg, "\n====-- DUMP DistCtx --====");
}
//END DistCtx


//
//START CoeffVarFactor
//
void CoeffVarFactor::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\nCOEFF EXP:");
    if (coeffexp != nullptr) {
        rg->getLogMgr()->incIndent(2);
        UINT flag = IR_DUMP_COMBINE;
        xoc::dumpIR(coeffexp, rg, nullptr, DumpFlag::combineIRID(flag));
        rg->getLogMgr()->decIndent(2);
    }

    note(rg, "\nVAR EXP:");
    if (varexp != nullptr) {
        rg->getLogMgr()->incIndent(2);
        UINT flag = IR_DUMP_COMBINE;
        xoc::dumpIR(varexp, rg, nullptr, DumpFlag::combineIRID(flag));
        rg->getLogMgr()->decIndent(2);
    }
}
//END CoeffVarFactor


//
//START LinearFactor
//
void LinearFactor::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n-- LINEAR FACTOR --");
    rg->getLogMgr()->incIndent(2);

    {
    note(rg, "\nLINEAR EXP:");
    rg->getLogMgr()->incIndent(2);
    if (linearexp != nullptr) {
        UINT flag = IR_DUMP_COMBINE;
        xoc::dumpIR(linearexp, rg, nullptr, DumpFlag::combineIRID(flag));
    }
    rg->getLogMgr()->decIndent(2);
    }

    cvf0.dump(rg);
    cvf1.dump(rg);

    rg->getLogMgr()->decIndent(2);
}
//END LinearFactor


//
//START AlgeDistributive
//
void AlgeDistributive::reset()
{
    m_am.clean();
}


bool AlgeDistributive::initDepPass(MOD OptCtx & oc)
{
    PassTypeList optlist;
    optlist.append_tail(PASS_RPO);

    //Maintain DU chain for new generatged distributive expression need
    //DOM info.
    optlist.append_tail(PASS_DOM);
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, optlist);
    m_refine = (Refine*)m_rg->getPassMgr()->registerPass(PASS_REFINE);
    m_simp = (IRSimp*)m_rg->getPassMgr()->registerPass(PASS_IRSIMP);
    ASSERT0(m_refine && m_simp);
    return true;
}


bool AlgeDistributive::canBeDist(IR const* ir) const
{
    //NOTE:a-b-c can be linearized to {+ a (-b) (-c)}
    return ir->is_commutative() && ir->is_associative();
}


bool AlgeDistributive::canBeDist(IR_CODE c) const
{
    //NOTE:a-b-c can be linearized to {+ a (-b) (-c)}
    return IR::isCommutativeOp(c) && IR::isAssociativeOp(c);
}


bool AlgeDistributive::canBeCandStmt(IR const* ir) const
{
    ASSERT0(ir->is_stmt());
    if (!ir->isStoreStmt()) { return false; }
    if (ir->isMayThrow(true)) { return false; }
    if (ir->isPartialStoreStmt()) { return false; }
    if (ir->isWritePartialPR()) { return false; }
    if (ir->isVirtualOp()) { return false; }
    return true;
}


bool AlgeDistributive::dump() const
{
    if (!getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_ALGE_DISTRIBUTIVE)) {
        return true;
    }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_am.dump();
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


bool AlgeDistributive::hasSideEffect(IR const* ir) const
{
    return ir->hasSideEffect(true);
}


//Return true if ir is monotonic operation.
static bool isLinearOp(IR const* ir)
{
    return ir->is_add() || ir->is_sub();
}


class AlgeDistIntlImpl {
public:
    static bool computeDistForBB(IRBB const* bb, MOD DistCtx & ctx);

    static bool extractCoeffVarViaDirectMemExp(
        IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx);
    static bool extractCoeffVarViaMul(
        IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx);
    static bool extractCoeffVarViaExp(
        IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx);
    static bool extractCoeffVarFactorViaStmt(
        IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx);
    static bool extractLinearFactorViaStoreStmt(
        IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx);
    static bool extractLinearFactorViaExp(
        IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx);
    static bool extractCoeffVarFactorViaStoreStmt(
        IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx);
    static bool extractLinearFactorViaStmt(
        IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx);
};


bool AlgeDistIntlImpl::extractCoeffVarViaDirectMemExp(
    IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx)
{
    ASSERT0(ir && ir->is_exp());
    ASSERT0(ir->isMemOpnd() || ir->isReadPR());
    if (ir->hasSideEffect(true)) { return false; }
    IR * kdef = xoc::findKillingDef(ir, ctx.getRegion(), ctx.getOptCtx());
    if (kdef == nullptr) {
        return false;
    }
    ASSERT0(kdef->hasResult());
    if (kdef->isPartialStoreStmt()) {
        return false;
    }
    return extractCoeffVarFactorViaStmt(kdef, cvf, ctx);
}


//Extract distributive operations from monotonic operations.
//Return true if the extraction is successful.
bool AlgeDistIntlImpl::extractCoeffVarViaMul(
    IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx)
{
    ASSERT0(ir->is_mul());
    return Refine::extractCoeffAndVar(
        const_cast<IR*>(ir), const_cast<IR*&>(cvf.coeffexp),
        const_cast<IR*&>(cvf.varexp));
}


bool AlgeDistIntlImpl::extractCoeffVarViaExp(
    IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx)
{
    switch (ir->getCode()) {
    case IR_MUL:
        return extractCoeffVarViaMul(ir, cvf, ctx);
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
        return extractCoeffVarViaDirectMemExp(ir, cvf, ctx);
    SWITCH_CASE_EXT_EXP:
        ASSERT0(ir->isExtOp());
        return ctx.getAlgeDist()->extractCoeffVarFactorViaExtOp(
            ir, cvf, ctx);
    default:;
    }
    return false;
}


//Extract distributive operations from monotonic operations.
//Return true if the extraction is successful.
static bool extractLinearOp(
    IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx)
{
    ASSERT0(isLinearOp(ir));
    ASSERTN(ir->isBinaryOp(), ("TODO:support more op."));
    IR const* op0 = BIN_opnd0(ir);
    IR const* op1 = BIN_opnd1(ir);
    CoeffVarFactor cvf0;
    CoeffVarFactor cvf1;
    if (!AlgeDistIntlImpl::extractCoeffVarViaExp(op0, cvf0, ctx)) {
        return false;
    }
    if (!AlgeDistIntlImpl::extractCoeffVarViaExp(op1, cvf1, ctx)) {
        return false;
    }
    lf.cvf0 = cvf0;
    lf.cvf1 = cvf1;
    lf.linearexp = ir;
    return true;
}


static bool extractLinearFactorViaDirectMemExp(
    IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx)
{
    ASSERT0(ir && ir->is_exp());
    ASSERT0(ir->isMemOpnd() || ir->isReadPR());
    if (ir->hasSideEffect(true)) { return false; }
    IR * kdef = xoc::findKillingDef(ir, ctx.getRegion(), ctx.getOptCtx());
    if (kdef == nullptr) {
        return false;
    }
    ASSERT0(kdef->hasResult());
    if (kdef->isPartialStoreStmt()) {
        return false;
    }
    return AlgeDistIntlImpl::extractLinearFactorViaStmt(kdef, lf, ctx);
}


//Return true if the extraction is successful.
bool AlgeDistIntlImpl::extractLinearFactorViaExp(
    IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx)
{
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB:
        return extractLinearOp(ir, lf, ctx);
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
        return extractLinearFactorViaDirectMemExp(ir, lf, ctx);
    SWITCH_CASE_EXT_EXP:
        ASSERT0(ir->isExtOp());
        return ctx.getAlgeDist()->extractLinearFactorViaExtOp(ir, lf, ctx);
    default:;
    }
    return false;
}


bool AlgeDistIntlImpl::extractCoeffVarFactorViaStoreStmt(
    IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx)
{
    ASSERT0(ir->isStoreStmt() || ir->isVirtualOp());
    if (!ctx.getAlgeDist()->canBeCandStmt(ir)) { return false; }
    ASSERT0(!ir->isPartialStoreStmt());
    return AlgeDistIntlImpl::extractCoeffVarViaExp(ir->getRHS(), cvf, ctx);
}


bool AlgeDistIntlImpl::extractLinearFactorViaStoreStmt(
    IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx)
{
    ASSERT0(ir->isStoreStmt() || ir->isVirtualOp());
    if (!ctx.getAlgeDist()->canBeCandStmt(ir)) { return false; }
    ASSERT0(!ir->isPartialStoreStmt());
    return extractLinearFactorViaExp(ir->getRHS(), lf, ctx);
}


bool AlgeDistributive::extractCoeffAndVar(
    IR const* ir, OUT IR ** coeff, OUT Var ** var, MOD DistCtx & ctx)
{
    ASSERT0(ir && coeff && var);
    Region * rg = ctx.getRegion();
    IRMgr * irmgr = ctx.getIRMgr();
    MDMgr * mdmgr = ctx.getMDMgr();
    if (ir->isDirectMemOp() || ir->isPROp()) {
        *coeff = irmgr->buildImmInt(1, ir->getType());
        MD const* md = mdmgr->genMDForDirectOp(ir);
        *var = md->get_base();
        return true;
    }
    if (ir->is_mul()) {
        IR const* op0 = BIN_opnd0(ir);
        IR const* op1 = BIN_opnd1(ir);
        if (op1->isDirectMemOp()) {
            //Guarrantee the first opnd is Var.
            xcom::swap(op0, op1);
        }
        if (!op0->isDirectMemOp()) {
            //CASE:we don't handle the fold-const stituation here.
            return false;
        }
        if (!isConstExpTree(op1)) { return false; }
        *coeff = rg->dupIRTree(op1);
        MD const* md = mdmgr->genMDForDirectOp(op0);
        *var = md->get_base();
        return true;
    }
    return false;
}


static void simplifyNewStmt(
    MOD IR * ir, Region const* rg, DistCtx const& ctx)
{
    ASSERT0(ir->is_stmt());
    Refine * refine = ctx.getRefine();
    RefineCtx rc(ctx.getOptCtx());
    bool change = false;
    ASSERT0(refine);
    IR * newir = refine->refineExpression(ir, change, rc);
    ASSERT0(newir == ir);
}


bool AlgeDistributive::distributive(
    MOD IR * ir, LinearFactor const& lf, MOD DistCtx & ctx) const
{
    ASSERT0(ir->is_stmt() && ir->isStoreStmt() && ir->hasRHS());
    ASSERT0(lf.cvf0.coeffexp && lf.cvf1.coeffexp);
    IR const* coeff0 = lf.cvf0.coeffexp;
    IR const* coeff1 = lf.cvf1.coeffexp;
    if (!coeff0->isConstExp()) { return false; }
    if (!coeff1->isConstExp()) { return false; }

    IR const* var0 = lf.cvf0.varexp;
    IR const* var1 = lf.cvf1.varexp;
    if (var0->hasSideEffect(true)) { return false; }
    if (var1->hasSideEffect(true)) { return false; }
    if (!var0->isIREqual(var1, ctx.getIRMgr(), true)) { return false; }

    Region * rg = ctx.getRegion();
    IR * new_coeff0 = rg->dupIRTree(coeff0);
    IR * new_coeff1 = rg->dupIRTree(coeff1);
    IR * new_var0 = rg->dupIRTree(var0);
    xoc::addUseForTree(new_coeff0, coeff0, rg);
    xoc::addUseForTree(new_coeff1, coeff1, rg);
    xoc::addUseForTree(new_var0, var0, rg);

    //Combine new expression.
    IR_CODE code = lf.linearexp->getCode();
    Type const* ty = lf.linearexp->getType();
    IR * newrhs = ctx.getIRMgr()->buildBinaryOpSimp(
        code, ty, new_coeff0, new_coeff1);
    newrhs = ctx.getIRMgr()->buildBinaryOpSimp(
        IR_MUL, ty, newrhs, new_var0);
   
    IR * rhs = ir->getRHS();
    xoc::removeUseForTree(rhs, rg, *ctx.getOptCtx());
    ctx.tryInvalidInfoBeforeFreeIR(rhs);
    dumpReplaceExp(rhs, newrhs, this);
    rg->freeIRTree(rhs);
    ir->setRHS(newrhs);
    simplifyNewStmt(ir, m_rg, ctx);
    return true;
}


bool AlgeDistIntlImpl::extractCoeffVarFactorViaStmt(
    IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_STPR:
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_EXT_WRITE_PR:
        return extractCoeffVarFactorViaStoreStmt(ir, cvf, ctx);
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_CALL:
    case IR_ICALL:
    case IR_IGOTO:
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
    case IR_RETURN:
    case IR_GOTO:
    case IR_REGION:
    case IR_PHI:
        return false;
    default:
        ASSERT0(ir->isExtOp());
        return ctx.getAlgeDist()->extractCoeffVarFactorViaExtOp(ir, cvf, ctx);
    }
    return false;
}


bool AlgeDistIntlImpl::extractLinearFactorViaStmt(
    IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_STPR:
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_EXT_WRITE_PR:
        return extractLinearFactorViaStoreStmt(ir, lf, ctx);
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_CALL:
    case IR_ICALL:
    case IR_IGOTO:
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
    case IR_RETURN:
    case IR_GOTO:
    case IR_REGION:
    case IR_PHI:
        return false;
    default:
        ASSERT0(ir->isExtOp());
        return ctx.getAlgeDist()->extractLinearFactorViaExtOp(ir, lf, ctx);
    }
    return false;
}


bool AlgeDistributive::extractCoeffVarFactorViaExtOp(
    IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx) const
{
    //There is no knowledge about the extended operation that wether it
    //can not be linearized.
    return false;
}


bool AlgeDistributive::extractLinearFactorViaExtOp(
    IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx) const
{
    //There is no knowledge about the extended operation that wether it
    //can not be linearized.
    return false;
}


bool AlgeDistIntlImpl::computeDistForBB(IRBB const* bb, MOD DistCtx & ctx)
{
    bool changed = false;
    BBIRListIter it;
    BBIRList & irlst = const_cast<IRBB*>(bb)->getIRList();
    for (IR * ir = irlst.get_tail(&it);
         ir != nullptr; ir = irlst.get_prev(&it)) {
        if (ir->is_phi()) { continue; }
        LinearFactor lf;
        bool succ = extractLinearFactorViaStmt(ir, lf, ctx);
        if (!succ) { continue; }
        changed |= ctx.getAlgeDist()->distributive(ir, lf, ctx);
    }
    return changed;
}


bool AlgeDistributive::doDist(MOD DistCtx & ctx)
{
    bool changed = false;
    xcom::RPOVexList const* vexlst = m_cfg->getRPOVexList();
    ASSERT0(vexlst);
    xcom::RPOVexListIter it;
    for (vexlst->get_tail(&it); it != vexlst->end();
         it = vexlst->get_prev(it)) {
        IRBB const* bb = m_rg->getBB(it->val()->id());
        ASSERT0(bb);
        changed |= AlgeDistIntlImpl::computeDistForBB(bb, ctx);
    }
    return changed;
}


bool AlgeDistributive::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.isPassValid(PASS_MD_REF)) { return false; }

    //Initialize pass object since they might be destructed at any moment.
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    m_irmgr = m_rg->getIRMgr();
    if (!usePRSSADU() || !useMDSSADU()) {
        //AlgeDist prefers using SSA instead of classic DU.
        return false;
    }
    START_TIMER(t, getPassName());
    dumpBeforePass();
    reset();
    initDepPass(oc);
    DistCtx ctx(oc, this);
    bool change = doDist(ctx);
    if (!change) {
        if (g_dump_opt.isDumpForTest()) { dump(); }
        END_TIMER(t, getPassName());
        return false;
    }
    dump();
    reset();
    //DU chain and DU reference should be maintained.
    ASSERT0(xoc::verifyMDRef(m_rg, oc) && xoc::verifyClassicDUChain(m_rg, oc));
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    END_TIMER(t, getPassName());
    return true;
}
//END AlgeDistributive

} //namespace xoc
