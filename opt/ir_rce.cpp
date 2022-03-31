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

static void dumpInit(RCE const* pass)
{
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n\n==---- DUMP %s ----==", pass->getPassName());
    rg->getLogMgr()->incIndent(2);
}


static void dumpFini(RCE const* pass)
{
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    rg->getLogMgr()->decIndent(2);
}


static void dumpRemovedIR(IR const* ir, RCE const* pass)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\nREMOVE IR:");
    dumpIR(ir, rg, IR_DUMP_KID);
}


static void dumpRemovedEdge(IRBB const* from, IRBB const* to, RCE const* pass)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\nREMOVE EDGE: BB%d->BB%d", from->id(), to->id());
}


static void dumpChangedIR(IR const* oldir, IR const* newir, RCE const* pass)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\nCHANGE:");
    note(rg, "\nOLD:");
    dumpIR(oldir, rg, IR_DUMP_KID);
    note(rg, "\nNEW:");
    dumpIR(newir, rg, IR_DUMP_KID);
}


//If 'ir' is always true, set 'must_true', or if it is
//always false, set 'must_false'.
//Return true if this function is able to determine the result of 'ir',
//otherwise return false that it does know nothing about ir.
bool RCE::calcCondMustVal(IR const* ir, OUT bool & must_true,
                          OUT bool & must_false) const
{
    must_true = false;
    must_false = false;
    ASSERT0(ir->is_judge());
    switch (ir->getCode()) {
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_NE:
    case IR_EQ:
    case IR_LAND:
    case IR_LOR:
    case IR_LNOT: {
        if (ir->is_const()) {
            if (CONST_int_val(ir) == 1) {
                must_true = true;
            } else if (CONST_int_val(ir) == 0) {
                must_false = true;
            }
            return true;
        }
        if (m_gvn != nullptr && m_gvn->is_valid()) {
            return m_gvn->calcCondMustVal(ir, must_true, must_false);
        }
        break;
    }
    default: UNREACHABLE();
    }
    return false;
}


static bool regardAsMustTrue(IR const* ir)
{
    ASSERT0(ir->is_const());
    if (ir->is_int() || ir->is_ptr()) {
        if (CONST_int_val(ir) != 0) { return true; }
        return false;
    }
    if (ir->is_str() || ir->is_vec() || ir->is_mc()) { return true; }
    if (g_is_opt_float && ::fabs(CONST_fp_val(ir) - (HOST_FP)0.0) > EPSILON) {
        return true;
    }
    return false;
}


static bool regardAsMustFalse(IR const* ir)
{
    ASSERT0(ir->is_const());
    if (ir->is_int() || ir->is_ptr()) {
        if (CONST_int_val(ir) == 0) { return true; }
        return false;
    }
    if (ir->is_str() || ir->is_vec() || ir->is_mc()) { return false; }
    if (g_is_opt_float && ::fabs(CONST_fp_val(ir) - (HOST_FP)0.0) <= EPSILON) {
        return true;
    }
    return false;
}


//If 'ir' is always true, set 'must_true', or if it is
//always false, set 'must_false'.
//Return the changed ir.
IR * RCE::calcCondMustVal(IN IR * ir, OUT bool & must_true,
                          OUT bool & must_false, OUT bool & changed)
{
    must_true = false;
    must_false = false;
    ASSERT0(ir->is_judge());
    switch (ir->getCode()) {
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_NE:
    case IR_EQ:
    case IR_LAND:
    case IR_LOR:
    case IR_LNOT: {
        ir = m_refine->foldConst(ir, changed);
        if (changed && ir->is_const()) {
            if (regardAsMustTrue(ir)) {
                must_true = true;
            } else if (regardAsMustFalse(ir)) {
                must_false = true;
            }
            return ir;
        }

        if (m_gvn != nullptr && m_gvn->is_valid()) {
            if (changed) {
                bool vn_changed = false;
                m_gvn->computeVN(ir, vn_changed);
            }
            bool succ = m_gvn->calcCondMustVal(ir, must_true, must_false);
            if (succ) {
                changed = true;
                if (must_true) {
                    ASSERT0(!must_false);
                    Type const* type = ir->getType();
                    xoc::removeUseForTree(ir, m_rg);
                    m_rg->freeIRTree(ir);
                    return m_rg->buildImmInt(1, type);
                } else {
                    ASSERT0(must_false);
                    Type const* type = ir->getType();
                    xoc::removeUseForTree(ir, m_rg);
                    m_rg->freeIRTree(ir);
                    return m_rg->buildImmInt(0, type);
                }
            }
        }
        break;
    }
    default: UNREACHABLE();
    }
    return ir;
}


IR * RCE::processFalsebr(IR * ir, IR * new_det, bool must_true,
                         bool must_false, bool changed, MOD RCECtx & ctx)
{
    IR * old_det = BR_det(ir);
    BR_det(ir) = nullptr;
    if (must_true) {
        //FALSEBR(0x1), never jump.
        //Revise m_cfg. remove branch edge.
        IRBB * from = ir->getBB();
        IRBB * to = m_cfg->getTargetBB(from);
        ASSERT0(from != nullptr && to != nullptr);
        xoc::removeStmt(ir, m_rg);

        //Revise the PHI operand to target successor.
        dumpRemovedEdge(from, to, this);
        m_cfg->removeEdge(from, to);
        ctx.cfg_mod = true;
        //RPO is unchanged if only removing branch-edge.
        //DOM may changed.
        // BB1--->BB3------
        //  |      |       |
        //  |      v       v
        //   ----BB2----->BB4
        // If removing 3->4, idom of BB4 changed.
        ctx.oc->setDomValid(false);
        return nullptr;
    }
    if (must_false) {
        //FALSEBR(0x0), always jump.
        IRBB * from = ir->getBB();
        IRBB * to = m_cfg->getFallThroughBB(from);
        ASSERT0(from != nullptr && to != nullptr);

        IR * newbr = m_rg->buildGoto(BR_lab(ir));
        xoc::removeStmt(ir, m_rg);

        //Revise the PHI operand to fallthrough successor.
        //Revise m_cfg. remove fallthrough edge.
        dumpRemovedEdge(from, to, this);
        m_cfg->removeEdge(from, to);
        ctx.cfg_mod = true;
        //RPO is unchanged if only removing branch-edge.
        //DOM may changed.
        // BB1--->BB3------
        //  |      |       |
        //  |      v       v
        //   ----BB2----->BB4
        // If removing 3->4, idom of BB4 changed.
        ctx.oc->setDomValid(false);
        return newbr;
    }

    ASSERT0(BR_det(ir) == nullptr);
    if (changed) {
        if (!new_det->is_judge()) {
            new_det = m_rg->buildJudge(new_det);
        }
        BR_det(ir) = new_det;
        ir->setParent(new_det);
        dumpChangedIR(old_det, new_det, this);
        return ir;
    }

    //Resume original det.
    BR_det(ir) = new_det;
    return ir;
}


IR * RCE::processTruebr(IR * ir, IR * new_det, bool must_true,
                        bool must_false, bool changed, MOD RCECtx & ctx)
{
    IR * old_det = BR_det(ir);
    BR_det(ir) = nullptr;
    if (must_true) {
        //TRUEBR(0x1), always jump.
        IRBB * from = ir->getBB();
        IRBB * to = m_cfg->getFallThroughBB(from);
        ASSERT0(from != nullptr && to != nullptr);
        IR * newbr = m_rg->buildGoto(BR_lab(ir));
        xoc::removeStmt(ir, m_rg);

        //Revise the PHI operand to fallthrough successor.
        //Revise cfg. remove fallthrough edge.
        dumpRemovedEdge(from, to, this);
        m_cfg->removeEdge(from, to);
        ctx.cfg_mod = true;
        //RPO is unchanged if only removing branch-edge.
        //DOM may changed.
        // BB1--->BB3------
        //  |      |       |
        //  |      v       v
        //   ----BB2----->BB4
        // If removing 3->4, idom of BB4 changed.
        ctx.oc->setDomValid(false);
        return newbr;
    }
    if (must_false) {
        //TRUEBR(0x0), never jump.
        //Revise cfg. remove branch edge.
        IRBB * from = ir->getBB();
        IRBB * to = m_cfg->findBBbyLabel(BR_lab(ir));
        ASSERT0(from && to);
        xoc::removeStmt(ir, m_rg);

        //Revise the PHI operand to target successor.
        dumpRemovedEdge(from, to, this);
        m_cfg->removeEdge(from, to);
        ctx.cfg_mod = true;
        //RPO is unchanged if only removing branch-edge.
        //DOM may changed.
        // BB1--->BB3------
        //  |      |       |
        //  |      v       v
        //   ----BB2----->BB4
        // If removing 3->4, idom of BB4 changed.
        ctx.oc->setDomValid(false);
        return nullptr;
    }

    ASSERT0(BR_det(ir) == nullptr);
    if (changed) {
        if (!new_det->is_judge()) {
            new_det = m_rg->buildJudge(new_det);
        }
        BR_det(ir) = new_det;
        ir->setParent(new_det);
        dumpChangedIR(old_det, new_det, this);
        return ir;
    }

    //Resume original det.
    BR_det(ir) = new_det;
    return ir; 
}


IR * RCE::processBranch(IR * ir, MOD RCECtx & ctx)
{
    ASSERT0(ir->isConditionalBr());
    bool must_true, must_false, changed = false;
    IR * new_det = calcCondMustVal(BR_det(ir), must_true, must_false, changed);
    if (ir->is_truebr()) {
        return processTruebr(ir, new_det, must_true, must_false, changed, ctx);
    }
    ASSERT0(ir->is_falsebr()); 
    return processFalsebr(ir, new_det, must_true, must_false, changed, ctx);
}


//Perform dead store elmination: x = x;
IR * RCE::processStore(IR * ir)
{
    ASSERT0(ir->is_st());
    if (ST_rhs(ir)->getExactRef() == ir->getExactRef()) {
        dumpRemovedIR(ir, this);
        xoc::removeUseForTree(ir, m_rg);
        return nullptr;
    }
    return ir;
}


//Perform dead store elmination: x = x;
IR * RCE::processStorePR(IR * ir)
{
    ASSERT0(ir->is_stpr());
    if (STPR_rhs(ir)->getExactRef() == ir->getExactRef()) {
        dumpRemovedIR(ir, this);
        xoc::removeUseForTree(ir, m_rg);
        return nullptr;
    }
    return ir;
}


bool RCE::performSimplyRCEForBB(IRBB * bb, MOD RCECtx & ctx)
{
    bool change = false;
    BBIRList * ir_list = &BB_irlist(bb);
    IRListIter ct;
    IRListIter next_ct;
    for (ir_list->get_head(&next_ct), ct = next_ct;
         ct != nullptr; ct = next_ct) {
        IR * ir = ct->val();
        ir_list->get_next(&next_ct);
        if (ir->hasSideEffect(true)) { continue; }

        IR * newir = ir;
        switch (ir->getCode()) {
        case IR_TRUEBR:
        case IR_FALSEBR:
            newir = processBranch(ir, ctx);
            break;

        //This case has been dealt in ir_refine.
        //case IR_ST:
        //    newir = processStore(ir);
        //    break;
        default:;
        }
        if (newir == ir) { continue; }
        dumpChangedIR(ir, newir, this);
        ir_list->remove(ct);
        m_rg->freeIRTree(ir);
        if (newir != nullptr) {
            if (next_ct != nullptr) {
                ir_list->insert_before(newir, next_ct);
            } else {
                ir_list->append_tail(newir);
            }
        }
        change = true;
    }
    return change;
}


//e.g:
//1. if (a == a) { ... } , remove redundant comparation.
//2. b = b; remove redundant store.
bool RCE::performSimplyRCE(MOD RCECtx & ctx)
{
    BBList * bbl = m_rg->getBBList();
    bool change = false;
    BBListIter ct_bb;
    for (IRBB * bb = bbl->get_head(&ct_bb);
         bb != nullptr; bb = bbl->get_next(&ct_bb)) {
        change |= performSimplyRCEForBB(bb, ctx);
    }
    return change;
}


bool RCE::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }

    if (!oc.is_cfg_valid()) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    m_refine = (Refine*)m_rg->getPassMgr()->queryPass(PASS_REFINE);
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //DCE use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //DCE use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    START_TIMER(t, getPassName());
    if (m_gvn == nullptr || (!m_gvn->is_valid() && is_use_gvn())) {
        return false;
    }
    m_rg->getLogMgr()->startBuffer();
    dumpInit(this);
    RCECtx ctx(&oc);
    bool change = performSimplyRCE(ctx);
    if (ctx.cfg_mod) {
        ASSERT0(change);
        //DOM and RPO may changed, rebuild them when you need.
        change |= m_cfg->performMiscOpt(oc);
    }
    if (change) {
        //CFG changed, remove empty BB.
        //TODO: DO not recompute whole SSA/MDSSA. Instead, update
        //SSA/MDSSA info especially PHI operands incrementally.
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg));
        ASSERT0(m_cfg->verifyRPO(oc));
        ASSERT0(m_cfg->verifyDomAndPdom(oc));
    } else {
        m_rg->getLogMgr()->cleanBuffer();
    }
    dumpFini(this);
    m_rg->getLogMgr()->endBuffer();
    END_TIMER(t, getPassName());
    return change;
}

} //namespace xoc
