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

static void dumpAct(
    IR const* oldexp, IR const* genexp, IR const* newexp, LCSECtx const& ctx)
{
    ASSERT0(oldexp && genexp && newexp);
    Region const* rg = ctx.getRegion();
    ASSERT0(rg);
    ActMgr * am = ctx.getActMgr();
    if (am == nullptr || !rg->isLogMgrInit() || !g_dump_opt.isDumpLCSE()) {
        return;
    }
    am->dump("%s is CSE of %s and will be replaced by %s",
             DumpIRName().dump(oldexp), DumpIRName().dump(genexp),
             DumpIRName().dump(newexp));
}


//The function hoists the CSE of avail_pos to avail_cse.
//specified_exp: one of the kid of 'avail_pos' stmt.
//e.g: before:
//    =a+b //avail_pos
//    ...
//    =a+b //a+b is specified_exp
//after:
//    t=a+b //t is avail_cse, a+b is specified_exp
//    =t  //avail_pos
//    ...
//    =t //t is new_exp
//NOTE:the function maintains the DU chain of 'avail_cse_comp_stmt' and
//     original 'avail_pos'.
static IR * hoistCseAndGenAvailCseComp(
    IN IRBB * bb, ExprRep const* ie, IN IR * avail_pos, LCSECtx & ctx)
{
    ASSERT0(avail_pos && avail_pos->is_stmt());
    IR * avail_cse_in_pos = ctx.getAvailExpInPos(ie);
    ASSERT0(avail_cse_in_pos);
    ASSERT0(avail_cse_in_pos->is_exp() &&
            avail_cse_in_pos->getStmt() == avail_pos);
    Region * rg = ctx.getLCSE()->getRegion();
    IRListIter pos_holder = nullptr;
    bool f = BB_irlist(bb).find(avail_pos, &pos_holder);
    ASSERT0_DUMMYUSE(f);

    //Generate placeholder PR to replace orignial CSE in avail_pos.
    IR * placeholder_of_cse = rg->getIRMgr()->buildPR(
        avail_cse_in_pos->getType());
    ctx.getMDMgr()->allocRefForIRTree(placeholder_of_cse, false);

    //Determine whether to generate judgement expression.
    IR * newexp_in_avail_pos = placeholder_of_cse;
    if (avail_pos->hasJudgeDet() &&
        avail_cse_in_pos == avail_pos->getJudgeDet()) {
        newexp_in_avail_pos = rg->getIRMgr()->buildJudge(newexp_in_avail_pos);
    }
    bool succ = avail_pos->replaceKid(
        avail_cse_in_pos, newexp_in_avail_pos, true);
    ASSERT0(succ);

    //CASE: ist(p+n)=(p+n)
    //In the case, both BASE and RHS of IST are CSE.
    //Do not regenerate the CSE again if it has been generated in the
    //previous processing of the base of IST.
    ASSERT0(avail_cse_in_pos->getParent() == nullptr);
    IR * avail_cse_comp_stmt = rg->dupIsomoStmt(
        placeholder_of_cse, avail_cse_in_pos);

    //Insert avail_cse_comp into BB's IRList.
    BB_irlist(bb).insert_before(avail_cse_comp_stmt, pos_holder);

    //CASE:a=x+y //exp is x+y
    //After hoisting:
    //  $t=x+y //stpr $t is avail_cse
    //  a=$t
    //Build DU chain between stpr $t and pr $t.
    xoc::buildDUChain(
        avail_cse_comp_stmt, placeholder_of_cse, rg, *ctx.getOptCtx());

    //Even if CSE is not belong to avail_pos stmt, we still reserve the
    //avail_exp info of CSE to subsequent dumpping.
    //ctx.map_ie2avail_exp_in_pos.set(ie->id(), nullptr);

    dumpAct(avail_cse_in_pos, avail_cse_in_pos, placeholder_of_cse, ctx);

    //Return the statement that hold the CSE value.
    return avail_cse_comp_stmt;
}


static IR * hoistCseForSpecifiedExp(
    MOD IRBB * bb, ExprRep const* ie, IN IR * ir, MOD IR * specified_exp,
    MOD IR * avail_cse, MOD LCSECtx & ctx)
{
    ASSERT0(ir && ir->is_stmt());
    ASSERT0(specified_exp && specified_exp->is_exp() &&
            specified_exp->getStmt() == ir);
    ASSERT0(avail_cse);
    ASSERT0(avail_cse->is_stmt());
    Region * rg = ctx.getLCSE()->getRegion();

    //In the case, avail_cse has been generated already in previous
    //CSE hoisting process, namely, there are multiple CSE occ.
    //After replacing original expression with the result of avail_cse,
    //free the old expression.
    xoc::removeUseForTree(specified_exp, rg, *ctx.getOptCtx());
    ctx.tryInvalidInfoBeforeFreeIR(specified_exp);

    //Generate a new PR to replace given 'specified_exp'.
    IR * new_exp = rg->dupIsomoExpTree(avail_cse);

    //Determine whether to generate judgement expression.
    IR * placeholder_exp = new_exp;
    if (ir->hasJudgeDet() && specified_exp == ir->getJudgeDet()) {
        //NOTE:Here need to compose new_exp to judgement expression.
        placeholder_exp = rg->getIRMgr()->buildJudge(placeholder_exp);
    }
    ir->replaceKid(specified_exp, placeholder_exp, true);
    ASSERT0(specified_exp->getParent() == nullptr);
    dumpAct(specified_exp, ctx.getAvailExpInPos(ie), placeholder_exp, ctx);
    rg->freeIRTree(specified_exp);

    //CASE:a=x+y //exp is x+y
    //After hoisting:
    //  $t=x+y //stpr $t is avail_cse
    //  a=$t
    //Build DU chain between stpr $t and pr $t.
    xoc::buildDUChain(avail_cse, new_exp, rg, *ctx.getOptCtx());
    return new_exp;
}

//
//START LCSECtx
//
LCSECtx::LCSECtx(OptCtx & oc, ActMgr * am, LCSE * lcse)
    : PassCtx(&oc, am), m_lcse(lcse)
{
    m_mdmgr = getRegion()->getMDMgr();
}


LCSECtx::~LCSECtx()
{
    tmp.clean(m_lcse->getSBSMgr());
}


DefMiscBitSetMgr & LCSECtx::getSBSMgr()
{
    return m_lcse->getSBSMgr();
}


void LCSECtx::clean()
{
    map_ie2avail_pos.clean();
    map_ie2avail_cse_comp_stmt.clean();
    avail_ir_expr.clean();
}


void LCSECtx::dump() const
{
    ExprTab const* exprtab = m_lcse->getExprTab();
    Region const* rg = m_lcse->getRegion();

    xoc::note(rg, "\n-- LCSECtx --");
    rg->getLogMgr()->incIndent(2);
    //Dump Current Available ExprRep.
    xoc::note(rg, "\n---- ALL RECOGNIZED EXPRREP");
    rg->getLogMgr()->incIndent(2);
    for (BSIdx j = avail_ir_expr.get_first();
         j != BS_UNDEF; j = avail_ir_expr.get_next(j)) {
        ExprRep const* ie = exprtab->getExprRep(j);
        ASSERT0(ie);
        ie->dump(rg);
    }
    rg->getLogMgr()->decIndent(2);

    //Dump ExprRep to individual IR avail_pos expression.
    xoc::note(rg, "\n---- MAP EXPRREP TO AVAILABLE POS STMT");
    rg->getLogMgr()->incIndent(2);
    for (UINT i = 0; i < map_ie2avail_pos.get_elem_count(); i++) {
        IR const* mapped = map_ie2avail_pos.get(i);
        if (mapped == nullptr) { continue; }
        ExprRep const* ie = exprtab->getExprRep(i);
        ASSERT0(ie);
        ie->dump(rg);
        xoc::note(rg, "\n|== MAPPED TO ==>");
        xoc::dumpIR(mapped, rg);
    }
    rg->getLogMgr()->decIndent(2);

    //Dump ExprRep to individual IR avail_cse.
    xoc::note(rg, "\n---- MAP EXPRREP TO AVAILABLE HOISTED CSE STMT");
    rg->getLogMgr()->incIndent(2);
    for (UINT i = 0; i < map_ie2avail_cse_comp_stmt.get_elem_count(); i++) {
        IR const* mapped = map_ie2avail_cse_comp_stmt.get(i);
        if (mapped == nullptr) { continue; }
        ExprRep const* ie = exprtab->getExprRep(i);
        ASSERT0(ie);
        ie->dump(rg);
        xoc::note(rg, "\n|== MAPPED TO ==>");
        xoc::dumpIR(mapped, rg);
    }
    rg->getLogMgr()->decIndent(2);
    rg->getLogMgr()->decIndent(2);
}


void LCSECtx::invalidAvailPosAndComp(ExprRep const* ie)
{
    avail_ir_expr.diff(ie->id());
    map_ie2avail_pos.set(ie->id(), nullptr);
    map_ie2avail_exp_in_pos.set(ie->id(), nullptr);
    map_ie2avail_cse_comp_stmt.set(ie->id(), nullptr);
}
//END LCSECtx


//
//START LCSE
//
LCSE::LCSE(Region * rg) : Pass(rg), m_am(rg)
{
    ASSERT0(rg != nullptr);
    m_tm = rg->getTypeMgr();
    m_du = m_rg->getDUMgr();
    ASSERT0(m_du && m_tm);
    m_expr_tab = nullptr;
    m_enable_filter = true;
}


class LCSEIntlImpl {
public:
    //Return true if 'spec_exp' has been regarded as CSE candidate and hoisted
    //to a PR.
    //e.g:
    //    call(a+b, a+b);
    //  =>
    //    $p1 = a+b; #S1
    //    call($p1, $p1);
    //  return #S1 as new statement.
    //ir: normal stmt.
    //spec_exp: specified expression that expected to be hoisted.
    static bool processExpImpl(
        MOD IRBB * bb, MOD IR * ir, MOD IR * spec_exp, MOD LCSECtx & ctx)
    {
        bool change = false;
        LCSE * lcse = ctx.getLCSE();
        if (spec_exp == nullptr || !lcse->canBeCandidate(spec_exp)) {
            return change;
        }
        ExprRep * ie = lcse->getExprTab()->mapIR2ExprRep(spec_exp);
        if (ie == nullptr) {
            //e.g: a = 10, the ie of spec_exp is nullptr.
            return change;
        }
        ctx.avail_ir_expr.bunion(ie->id());
        IR * avail_pos = ctx.map_ie2avail_pos.get(ie->id());
        if (avail_pos == nullptr) {
            //Record position of IR stmt.
            ctx.recordAvailPos(ie, ir, spec_exp);
            return change;
        }
        if (!lcse->isAvailable(avail_pos, spec_exp)) { return change; }
        IR * avail_cse = ctx.map_ie2avail_cse_comp_stmt.get(ie->id());
        if (avail_cse == nullptr) {
            avail_cse = hoistCseAndGenAvailCseComp(bb, ie, avail_pos, ctx);
            ASSERT0(avail_cse);
            ctx.recordAvailCseComp(ie, avail_cse);
            change = true;
        }
        if (spec_exp == ctx.getAvailExpInPos(ie)) {
            //Has been hoisted in hoistCseAndGenAvailCseComp().
            return change;
        }
        hoistCseForSpecifiedExp(bb, ie, ir, spec_exp, avail_cse, ctx);
        change = true;
        return change;
    }
};


bool LCSE::canBeCandidate(IR * ir)
{
    ASSERT0(ir);
    if (!m_enable_filter) {
        return ir->isBinaryOp() ||
               ir->is_bnot() ||
               ir->is_lnot() ||
               ir->is_neg();
    }
    ASSERT0(ir->is_exp());
    if (ir->is_ild()) {
        //Avoid perform the opposite behavior to Copy-Propagation.
        //e.g:
        //    ST(P1, ILD(P2))
        //    ST(P3, ILD(P2))
        //    after LCSE, we get:
        //    ST(P1, ILD(P2))
        //    ST(P3, P1)
        //    But CP will progagate P1 because ILD is the copy-prop candidate.
        return false;
    }
    return ir->isBinaryOp() || ir->is_bnot() || ir->is_lnot() || ir->is_neg();
}


bool LCSE::isAvailableInSameBB(IR * def_stmt, IR * use_exp) const
{
    ASSERT0(def_stmt && use_exp);
    IR * use_stmt = use_exp->getStmt();
    ASSERT0(use_stmt);
    IRBB * bb = def_stmt->getBB();
    ASSERT0(bb);
    BBIRList & irlist = BB_irlist(bb);
    BBIRListIter irlistit = nullptr;
    irlist.find(def_stmt, &irlistit);
    ASSERT0(irlistit);
    for (; irlistit != nullptr; irlistit = irlist.get_next(irlistit)) {
        IR * stmt = irlistit->val();
        ASSERT0(stmt);
        if (stmt == use_stmt) { break; }
        if (xoc::isDependentForTree(stmt, use_exp, false, m_rg)) {
            return false;
        }
    }
    return true;
}


bool LCSE::isAvailable(IR * def_stmt, IR * use_exp) const
{
    ASSERT0(def_stmt && use_exp);
    IRBB * defbb = def_stmt->getBB();
    IRBB * usebb = use_exp->getStmt()->getBB();
    ASSERT0(defbb && usebb);
    if (defbb == usebb) { return isAvailableInSameBB(def_stmt, use_exp); }
    return true;
}


bool LCSE::processExpTreeOfReturnOp(
    MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    ASSERT0(ir->is_return());
    return LCSEIntlImpl::processExpImpl(bb, ir, RET_exp(ir), ctx);
}


bool LCSE::processExpTreeOfMultiCondBranchOp(
    MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    ASSERT0(ir->getValExp());
    return LCSEIntlImpl::processExpImpl(bb, ir, ir->getValExp(), ctx);
}


bool LCSE::processExpTreeOfCondBranchOp(
    MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    ASSERT0(ir->getJudgeDet());
    return LCSEIntlImpl::processExpImpl(bb, ir, ir->getJudgeDet(), ctx);
}


bool LCSE::processBase(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->getBase());
    return LCSEIntlImpl::processExpImpl(bb, ir, ir->getBase(), ctx);
}


bool LCSE::processRHS(
    MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->hasRHS());
    return LCSEIntlImpl::processExpImpl(bb, ir, ir->getRHS(), ctx);
}


bool LCSE::processExpTreeList(
    MOD IRBB * bb, MOD IR * ir, IR * explist, MOD LCSECtx & ctx)
{
    ASSERT0(ir->is_stmt());
    if (explist == nullptr) { return false; }
    bool change = false;
    //Iterative analyse cse, e.g:
    //  CALL(ADD(x,y), SUB(a,b), ADD(x,y), SUB(a,b))
    IR * next = nullptr;
    for (IR * p = explist; p != nullptr; p = next) {
        next = p->get_next();
        ASSERT0(p->getStmt() == ir);
        change |= LCSEIntlImpl::processExpImpl(bb, ir, p, ctx);
    }
    return change;
}


bool LCSE::processExpTreeOfCallStmt(
    MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    return processExpTreeList(bb, ir, CALL_arg_list(ir), ctx);
}


bool LCSE::processExpTreeOfDirectMemOp(
    MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->hasRHS());
    return LCSEIntlImpl::processExpImpl(bb, ir, ir->getRHS(), ctx);
}


bool LCSE::processExpTreeOfIndirectMemOp(
    MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    bool change = false;
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->hasRHS() && ir->getBase());
    change |= LCSEIntlImpl::processExpImpl(bb, ir, ir->getRHS(), ctx);
    change |= LCSEIntlImpl::processExpImpl(bb, ir, ir->getBase(), ctx);
    return change;
}


bool LCSE::processExpTreeOfWriteArray(
    MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    bool change = false;
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->hasRHS() && ir->getBase());
    change |= LCSEIntlImpl::processExpImpl(bb, ir, ir->getRHS(), ctx);
    change |= LCSEIntlImpl::processExpImpl(bb, ir, ir->getBase(), ctx);
    change |= processExpTreeList(bb, ir, ARR_sub_list(ir), ctx);
    return change;
}


bool LCSE::processExpTree(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR: return processExpTreeOfDirectMemOp(bb, ir, ctx);
    SWITCH_CASE_WRITE_ARRAY: return processExpTreeOfWriteArray(bb, ir, ctx);
    SWITCH_CASE_INDIRECT_MEM_STMT:
        return processExpTreeOfIndirectMemOp(bb, ir, ctx);
    SWITCH_CASE_CALL: return processExpTreeOfCallStmt(bb, ir, ctx);
    case IR_GOTO:
    SWITCH_CASE_CFS_OP:
    case IR_LABEL:
    case IR_REGION:
    case IR_PHI:
    case IR_GETELEM:
    case IR_SETELEM:
    case IR_CASE: return false;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        return processExpTreeOfCondBranchOp(bb, ir, ctx);
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    case IR_IGOTO: return processExpTreeOfMultiCondBranchOp(bb, ir, ctx);
    case IR_RETURN: return processExpTreeOfReturnOp(bb, ir, ctx);
    default: return processExpTreeOfExtOp(bb, ir, ctx);
    }
    return false; //unchanged.
}


static void processAvailExpr(
    IRBB const* bb, IR const* ir, ExprRep const* ie, MOD LCSECtx & ctx)
{
    ASSERT0(ie != nullptr);
    IREListIter it;
    Region const* rg = ctx.getRegion();

    //NOTE:We traverse each OCC of 'ie' to determine wether 'ir' may modify
    //one of them. However, only OCCs that is prior to 'ir' are meaningful
    //to the available-expression processing. For simplicity, we do not
    //distinguish whether OCC is prior to 'ir' or comes after 'ir'.
    //e.g: ie (a+b) has 3 OCCs, two of them are before 'ir', one of them is
    //after 'ir'.
    //  ...=a+b;
    //  ......;
    //  ...=a+b;
    //  'ir';
    //  ...=a+b;
    for (IR const* occ = EXPR_occ_list(ie).get_head(&it);
         occ != nullptr; occ = EXPR_occ_list(ie).get_next(&it)) {
        if (occ->is_undef()) {
            //FIX: Do not consider the OCC that has been freed by LCSE.
            continue;
        }
        IR const* occ_stmt = occ->getStmt();
        ASSERT0(occ_stmt != nullptr && occ_stmt->getBB());
        ASSERT0(ir->getBB() == bb);
        if (occ_stmt->getBB() != bb) {
            continue;
        }
        if (xoc::isDependentForTree(ir, occ, true, rg)) {
            //Current stmt 'ir' may modify the value of 'ie'.
            //Thus it is unavailable from now on, and clean all related info.
            ctx.invalidAvailPosAndComp(ie);
            return;
        }
    }
}


//There may have expressions be killed.
//Remove them out the avail_ir_expr.
void LCSE::processResult(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    ASSERT0(ir->hasResult());
    //Compute killed ir-expr.
    ExprTab * exprtab = ctx.getLCSE()->getExprTab();
    ASSERT0(exprtab);
    ExprRepVec & expr_rep_vec = exprtab->getExpVec();
    BSIdx nextj = BS_UNDEF;
    for (BSIdx j = ctx.avail_ir_expr.get_first(); j != BS_UNDEF; j = nextj) {
        nextj = ctx.avail_ir_expr.get_next(j);
        ExprRep const* ie = expr_rep_vec.get(j);

        //ie may be removed out of 'avail_expr'.
        processAvailExpr(bb, ir, ie, ctx);
    }
}


//Return true if common expression has been substituted.
bool LCSE::processStmt(MOD IRBB * bb, MOD IR * ir, MOD LCSECtx & ctx)
{
    bool change = false;
    if (ir->hasSideEffect(true) || ir->isDummyOp()) { return change; }
    change |= processExpTree(bb, ir, ctx);

    //Process the result of stmt.
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_CALL:
    case IR_STPR:
        processResult(bb, ir, ctx);
        return change;
    SWITCH_CASE_BRANCH_OP:
    SWITCH_CASE_CFS_OP:
    case IR_LABEL:
    case IR_CASE:
    case IR_REGION:
    case IR_GETELEM:
    case IR_SETELEM:
    case IR_PHI:
    case IR_RETURN: return change;
    default:
        processExtStmt(bb, ir, ctx);
        return change;
    }
    return change;
}


bool LCSE::dump() const
{
    if (!getRegion()->isLogMgrInit() || !g_dump_opt.isDumpLCSE()) {
        return true;
    }
    note(m_rg, "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    m_am.dump();
    m_rg->getLogMgr()->decIndent(2);
    return true;
}


//Return true if common expression has been substituted.
bool LCSE::processBB(MOD IRBB * bb, MOD LCSECtx & ctx)
{
    bool change = false;
    IRListIter ct = nullptr;
    for (BB_irlist(bb).get_head(&ct); ct != BB_irlist(bb).end();) {
        IR * ir = ct->val();
        ct = BB_irlist(bb).get_next(ct);
        change |= processStmt(bb, ir, ctx);
    }
    return change;
}


bool LCSE::processBBList(OptCtx & oc)
{
    bool change = false;
    BBList * bbl = m_rg->getBBList();
    BBListIter ctbb = nullptr;
    LCSECtx ctx(oc, &getActMgr(), this);
    for (bbl->get_head(&ctbb); ctbb != bbl->end(); ctbb = bbl->get_next(ctbb)) {
        IRBB * bb = ctbb->val();
        ASSERT0(bb);
        ctx.clean();
        change |= processBB(bb, ctx);
    }
    return change;
}


bool LCSE::initDepPass(OptCtx const& oc)
{
    if (!oc.is_ref_valid()) { return false; }

    //Check PR DU chain.
    PRSSAMgr * ssamgr = (PRSSAMgr*)(m_rg->getPassMgr()->queryPass(
        PASS_PRSSA_MGR));
    if (ssamgr != nullptr && ssamgr->is_valid()) {
        m_prssamgr = ssamgr;
    } else {
        m_prssamgr = nullptr;
    }
    if (!oc.is_pr_du_chain_valid() && m_prssamgr == nullptr) {
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    //Check NONPR DU chain.
    MDSSAMgr * mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->queryPass(
        PASS_MDSSA_MGR));
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        m_mdssamgr = mdssamgr;
    } else {
        m_mdssamgr = nullptr;
    }
    if (!oc.is_nonpr_du_chain_valid() && m_mdssamgr == nullptr) {
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    return true;
}


bool LCSE::perform(OptCtx & oc)
{
    if (m_rg->getBBList()->get_elem_count() == 0) { return false; }
    if (!initDepPass(oc)) { return false; }
    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_EXPR_TAB, PASS_UNDEF);
    m_expr_tab = (ExprTab*)m_rg->getPassMgr()->registerPass(PASS_EXPR_TAB);
    ASSERT0(m_expr_tab);
    bool change = processBBList(oc);
    ASSERT0(xoc::verifyIRandBB(m_rg));
    ASSERT0(xoc::verifyMDRef(m_rg, oc));
    ASSERT0(xoc::verifyClassicDUChain(m_rg, oc));
    ASSERT0(xoc::verifyClassicDUChain(m_rg, oc));
    ASSERT0(m_rg->getCFG()->verifyRPO(oc));
    ASSERT0(m_rg->getCFG()->verifyDomAndPdom(oc));
    ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    if (change) {
        dump();

        //Found CSE and processed CSE. And inform PassMgr to invalid the
        //related passes.
        oc.setInvalidPass(PASS_EXPR_TAB);
        oc.setInvalidPass(PASS_AA);
    }
    END_TIMER(t, getPassName());
    return change;
}
//END LCSE

} //namespace xoc
