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
#include "prdf.h"
#include "prssainfo.h"
#include "ir_ssa.h"
#include "ir_gvn.h"
#include "ir_rce.h"

namespace xoc {

void IR_RCE::dump()
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n\n==---- DUMP IR_RCE ----==\n");

    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        //TODO:
    }
    fflush(g_tfile);
}


//If 'ir' is always true, set 'must_true', or if it is
//always false, set 'must_false'.
IR * IR_RCE::calcCondMustVal(
        IN IR * ir,
        OUT bool & must_true,
        OUT bool & must_false,
        bool & changed)
{
    must_true = false;
    must_false = false;
    ASSERT0(ir->is_judge());
    switch (ir->get_code()) {
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_NE:
    case IR_EQ:
    case IR_LAND:
    case IR_LOR:
    case IR_LNOT:
        {
            ir = m_ru->foldConst(ir, changed);
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

            if (m_gvn != NULL && m_gvn->is_valid()) {
                bool change = m_gvn->calcCondMustVal(ir, must_true, must_false);
                if (change) {
                    changed = true;
                    if (must_true) {
                        ASSERT0(!must_false);
                        Type const* type = ir->get_type();
                        ir->removeSSAUse();
                        m_ru->freeIRTree(ir);
                        ir = m_ru->buildImmInt(1, type);
                        return ir;
                    } else {
                        ASSERT0(must_false);
                        Type const* type = ir->get_type();
                        ir->removeSSAUse();
                        m_ru->freeIRTree(ir);
                        ir = m_ru->buildImmInt(0, type);
                        return ir;
                    }
                }
            }
        }
        break;
    default: UNREACH();
    }

    return ir;
}


IR * IR_RCE::processBranch(IR * ir, IN OUT bool & cfg_mod)
{
    ASSERT0(ir->isConditionalBr());

    bool must_true, must_false, changed = false;
    IR * new_det = calcCondMustVal(BR_det(ir), must_true, must_false, changed);
    BR_det(ir) = NULL;
    if (ir->is_truebr()) {
        if (must_true) {
            //TRUEBR(0x1), always jump.
            IRBB * from = ir->getBB();
            IRBB * to = m_cfg->getFallThroughBB(from);
            ASSERT0(from != NULL && to != NULL);
            IR * newbr = m_ru->buildGoto(BR_lab(ir));

            ir->removeSSAUse();

            //Revise the PHI operand to fallthrough successor.
            ir->getBB()->removeSuccessorDesignatePhiOpnd(m_cfg, to);

            m_ru->freeIRTree(ir);

            //Revise cfg. remove fallthrough edge.
            m_cfg->removeEdge(from, to);
            cfg_mod = true;
            return newbr;
        } else if (must_false) {
            //TRUEBR(0x0), never jump.
            //Revise cfg. remove branch edge.
            IRBB * from = ir->getBB();
            IRBB * to = m_cfg->findBBbyLabel(BR_lab(ir));
            ASSERT0(from && to);

            ir->removeSSAUse();

            //Revise the PHI operand to target successor.
            ir->getBB()->removeSuccessorDesignatePhiOpnd(m_cfg, to);

            m_ru->freeIRTree(ir);

            m_cfg->removeEdge(from, to);

            cfg_mod = true;
            return NULL;
        }
    } else {
        if (must_true) {
            //FALSEBR(0x1), never jump.
            //Revise m_cfg. remove branch edge.
            IRBB * from = ir->getBB();
            IRBB * to = m_cfg->getTargetBB(from);
            ASSERT0(from != NULL && to != NULL);

            ir->removeSSAUse();

            //Revise the PHI operand to target successor.
            ir->getBB()->removeSuccessorDesignatePhiOpnd(m_cfg, to);

            m_ru->freeIRTree(ir);

            m_cfg->removeEdge(from, to);

            cfg_mod = true;
            return NULL;
        } else if (must_false) {
            //FALSEBR(0x0), always jump.
            IRBB * from = ir->getBB();
            IRBB * to = m_cfg->getFallThroughBB(from);
            ASSERT0(from != NULL && to != NULL);

            IR * newbr = m_ru->buildGoto(BR_lab(ir));

            ir->removeSSAUse();

            //Revise the PHI operand to fallthrough successor.
            ir->getBB()->removeSuccessorDesignatePhiOpnd(m_cfg, to);

            m_ru->freeIRTree(ir);

            //Revise m_cfg. remove fallthrough edge.
            m_cfg->removeEdge(from, to);
            cfg_mod = true;
            return newbr;
        }
    }

    if (changed) {
        if (!new_det->is_judge()) {
            new_det = m_ru->buildJudge(new_det);
        }
        BR_det(ir) = new_det;
        ir->setParent(new_det);
    } else {
        BR_det(ir) = new_det;
    }

    return ir;
}


//Perform dead store elmination: x = x;
IR * IR_RCE::processStore(IR * ir)
{
    ASSERT0(ir->is_st());
    if (ST_rhs(ir)->getExactRef() == ir->getExactRef()) {
        ir->removeSSAUse();
        m_ru->freeIRTree(ir);
        return NULL;
    }
    return ir;
}


//Perform dead store elmination: x = x;
IR * IR_RCE::processStorePR(IR * ir)
{
    ASSERT0(ir->is_stpr());
    if (STPR_rhs(ir)->getExactRef() == ir->getExactRef()) {
        ir->removeSSAUse();
        m_ru->freeIRTree(ir);
        return NULL;
    }
    return ir;
}


//e.g:
//1. if (a == a) { ... } , remove redundant comparation.
//2. b = b; remove redundant store.
bool IR_RCE::performSimplyRCE(IN OUT bool & cfg_mod)
{
    BBList * bbl = m_ru->getBBList();
    bool change = false;
    C<IRBB*> * ct_bb;
    for (IRBB * bb = bbl->get_head(&ct_bb);
         bb != NULL; bb = bbl->get_next(&ct_bb)) {
        BBIRList * ir_list = &BB_irlist(bb);
        C<IR*> * ct, * next_ct;
        for (ir_list->get_head(&next_ct), ct = next_ct;
             ct != NULL; ct = next_ct) {
            IR * ir = ct->val();
            ir_list->get_next(&next_ct);
            IR * newIR = ir;
            switch (ir->get_code()) {
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

            if (newIR != ir) {
                ir_list->remove(ct);
                if (newIR != NULL) {
                    if (next_ct != NULL) {
                        ir_list->insert_before(newIR, next_ct);
                    } else {
                        ir_list->append_tail(newIR);
                    }
                }
                change = true;
            }
        }
    }
    return change;
}


bool IR_RCE::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_ru->checkValidAndRecompute(&oc, PASS_CFG,
        PASS_DU_REF, PASS_DU_CHAIN, PASS_UNDEF);

    if (!OC_is_du_chain_valid(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }

    if (!m_gvn->is_valid() && is_use_gvn()) {
        m_gvn->reperform(oc);
    }

    bool cfg_mod = false;
    bool change = performSimplyRCE(cfg_mod);
    if (cfg_mod) {
        //m_gvn->set_valid(false); //rce do not violate gvn for now.
        bool lchange;
        do {
            lchange = false;
            lchange |= m_cfg->removeUnreachBB();
            lchange |= m_cfg->removeEmptyBB(oc);
            lchange |= m_cfg->removeRedundantBranch();
            lchange |= m_cfg->removeTrampolinEdge();
        } while (lchange);

        m_cfg->computeExitList();

        //TODO: May be the change of CFG does not influence the
        //usage while we utilize du-chain and ir2mds.
        oc.set_flag_if_cfg_changed();

        OC_is_expr_tab_valid(oc) = false;
        OC_is_du_chain_valid(oc) = false;
        OC_is_ref_valid(oc) = false;
        OC_is_aa_valid(oc) = false;
        OC_is_expr_tab_valid(oc) = false;
        OC_is_reach_def_valid(oc) = false;
        OC_is_avail_reach_def_valid(oc) = false;

        OC_is_cfg_valid(oc) = true; //CFG has been maintained.
    }

    if (change) {
        ASSERT0(verifySSAInfo(m_ru));
    }

    END_TIMER(t, getPassName());
    return change;
}

} //namespace xoc
