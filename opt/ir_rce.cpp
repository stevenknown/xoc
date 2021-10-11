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

bool RCE::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(getRegion(), "\n\n==---- DUMP RCE ----==\n");
    getRegion()->dump(false);
    return true;
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


//If 'ir' is always true, set 'must_true', or if it is
//always false, set 'must_false'.
//Return the changed ir.
IR * RCE::calcCondMustVal(IN IR * ir,
                          OUT bool & must_true,
                          OUT bool & must_false,
                          bool & changed)
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
        if (changed) {
            ASSERT0(ir->is_const() &&
                    (CONST_int_val(ir) == 0 || CONST_int_val(ir) == 1));
            if (CONST_int_val(ir) == 1) {
                must_true = true;
            } else if (CONST_int_val(ir) == 0) {
                must_false = true;
            }
            return ir;
        }

        if (m_gvn != nullptr && m_gvn->is_valid()) {
            bool succ = m_gvn->calcCondMustVal(ir, must_true, must_false);
            if (succ) {
                changed = true;
                if (must_true) {
                    ASSERT0(!must_false);
                    Type const* type = ir->getType();
                    removeUseForTree(ir, m_rg);
                    m_rg->freeIRTree(ir);
                    return m_rg->buildImmInt(1, type);
                } else {
                    ASSERT0(must_false);
                    Type const* type = ir->getType();
                    removeUseForTree(ir, m_rg);
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


IR * RCE::processBranch(IR * ir, MOD bool * cfg_mod)
{
    ASSERT0(ir->isConditionalBr());
    bool must_true, must_false, changed = false;
    IR * new_det = calcCondMustVal(BR_det(ir), must_true, must_false, changed);
    BR_det(ir) = nullptr;
    if (ir->is_truebr()) {
        if (must_true) {
            //TRUEBR(0x1), always jump.
            IRBB * from = ir->getBB();
            IRBB * to = m_cfg->getFallThroughBB(from);
            ASSERT0(from != nullptr && to != nullptr);
            IR * newbr = m_rg->buildGoto(BR_lab(ir));
            removeStmt(ir, m_rg);

            //Revise the PHI operand to fallthrough successor.
            //Revise cfg. remove fallthrough edge.
            m_cfg->removeEdge(from, to);
            *cfg_mod = true;
            return newbr;
        } else if (must_false) {
            //TRUEBR(0x0), never jump.
            //Revise cfg. remove branch edge.
            IRBB * from = ir->getBB();
            IRBB * to = m_cfg->findBBbyLabel(BR_lab(ir));
            ASSERT0(from && to);
            removeStmt(ir, m_rg);

            //Revise the PHI operand to target successor.
            m_cfg->removeEdge(from, to);
            *cfg_mod = true;
            return nullptr;
        }
    } else {
        if (must_true) {
            //FALSEBR(0x1), never jump.
            //Revise m_cfg. remove branch edge.
            IRBB * from = ir->getBB();
            IRBB * to = m_cfg->getTargetBB(from);
            ASSERT0(from != nullptr && to != nullptr);
            removeStmt(ir, m_rg);

            //Revise the PHI operand to target successor.
            m_cfg->removeEdge(from, to);
            *cfg_mod = true;
            return nullptr;
        } else if (must_false) {
            //FALSEBR(0x0), always jump.
            IRBB * from = ir->getBB();
            IRBB * to = m_cfg->getFallThroughBB(from);
            ASSERT0(from != nullptr && to != nullptr);

            IR * newbr = m_rg->buildGoto(BR_lab(ir));
            removeStmt(ir, m_rg);

            //Revise the PHI operand to fallthrough successor.
            //Revise m_cfg. remove fallthrough edge.
            m_cfg->removeEdge(from, to);
            *cfg_mod = true;
            return newbr;
        }
    }

    ASSERT0(!BR_det(ir));
    if (changed) {
        if (!new_det->is_judge()) {
            new_det = m_rg->buildJudge(new_det);
        }
        BR_det(ir) = new_det;
        ir->setParent(new_det);
    } else {
        //Resume original det.
        BR_det(ir) = new_det;
    }
    return ir;
}


//Perform dead store elmination: x = x;
IR * RCE::processStore(IR * ir)
{
    ASSERT0(ir->is_st());
    if (ST_rhs(ir)->getExactRef() == ir->getExactRef()) {
        removeUseForTree(ir, m_rg);
        return nullptr;
    }
    return ir;
}


//Perform dead store elmination: x = x;
IR * RCE::processStorePR(IR * ir)
{
    ASSERT0(ir->is_stpr());
    if (STPR_rhs(ir)->getExactRef() == ir->getExactRef()) {
        removeUseForTree(ir, m_rg);
        return nullptr;
    }
    return ir;
}


//e.g:
//1. if (a == a) { ... } , remove redundant comparation.
//2. b = b; remove redundant store.
bool RCE::performSimplyRCE(MOD bool * cfg_mod)
{
    BBList * bbl = m_rg->getBBList();
    bool change = false;
    BBListIter ct_bb;
    for (IRBB * bb = bbl->get_head(&ct_bb);
         bb != nullptr; bb = bbl->get_next(&ct_bb)) {
        BBIRList * ir_list = &BB_irlist(bb);
        IRListIter ct;
        IRListIter next_ct;
        for (ir_list->get_head(&next_ct), ct = next_ct;
             ct != nullptr; ct = next_ct) {
            IR * ir = ct->val();
            ir_list->get_next(&next_ct);
            if (ir->hasSideEffect()) { continue; }

            IR * newIR = ir;
            switch (ir->getCode()) {
            case IR_TRUEBR:
            case IR_FALSEBR:
                newIR = processBranch(ir, cfg_mod);
                break;

            //This case has been dealt in ir_refine.
            //case IR_ST:
            //    newIR = processStore(ir);
            //    break;
            default:;
            }

            if (newIR == ir) { continue; }
            ir_list->remove(ct);
            m_rg->freeIRTree(ir);
            if (newIR != nullptr) {
                if (next_ct != nullptr) {
                    ir_list->insert_before(newIR, next_ct);
                } else {
                    ir_list->append_tail(newIR);
                }
            }
            change = true;
        }
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
    m_mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_MD_SSA_MGR);
    m_prssamgr = (PRSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_PR_SSA_MGR);
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
    if (!m_gvn->is_valid() && is_use_gvn()) {
        m_gvn->perform(oc);
    }

    bool cfg_mod = false;
    bool change = performSimplyRCE(&cfg_mod);
    if (cfg_mod) {
        ASSERT0(change);
        m_cfg->performMiscOpt(oc);
    }
    if (change) {
        //CFG changed, remove empty BB.
        //TODO: DO not recompute whole SSA/MDSSA. Instead, update
        //SSA/MDSSA info especially PHI operands incrementally.
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg));
    }

    if (g_is_dump_after_pass && g_dump_opt.isDumpRCE()) {
        dump();
    }

    END_TIMER(t, getPassName());
    return change;
}

} //namespace xoc
