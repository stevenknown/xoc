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
#include "ir_loop_cvt.h"

namespace xoc {

bool IR_LOOP_CVT::is_while_do(LI<IRBB> const* li, OUT IRBB ** gobackbb,
                              UINT * succ1, UINT * succ2)
{
    ASSERT0(gobackbb);
    IRBB * head = LI_loop_head(li);
    ASSERT0(head);

    *gobackbb = ::findSingleBackedgeStartBB(li, m_cfg);
    if (*gobackbb == NULL) {
        //loop may be too messy.
        return false;
    }

    if (BB_rpo(head) > BB_rpo(*gobackbb)) {
        //loop may already be do-while.
        return false;
    }

    IR * lastir = BB_last_ir(head);
    if (!lastir->isConditionalBr()) {
        return false;
    }

    bool f = findTwoSuccessorBBOfLoopHeader(li, m_cfg, succ1, succ2);
    if (!f) { return false; }

    if (li->is_inside_loop(*succ1) && li->is_inside_loop(*succ2)) {
        return false;
    }
    return true;
}


bool IR_LOOP_CVT::try_convert(LI<IRBB> * li, IRBB * gobackbb,
                              UINT succ1, UINT succ2)
{
    ASSERT0(gobackbb);

    IRBB * loopbody_start_bb;
    IRBB * epilog;
    if (li->is_inside_loop(succ1)) {
        ASSERT0(!li->is_inside_loop(succ2));
        loopbody_start_bb = m_cfg->getBB(succ1);
        epilog = m_cfg->getBB(succ2);
    } else {
        ASSERT0(li->is_inside_loop(succ2));
        ASSERT0(!li->is_inside_loop(succ1));
        loopbody_start_bb = m_cfg->getBB(succ2);
        epilog = m_cfg->getBB(succ1);
    }

    ASSERT0(loopbody_start_bb && epilog);
    IRBB * next = m_cfg->getFallThroughBB(gobackbb);
    if (next == NULL || next != epilog) {
        //No benefit to be get to convert this kind of loop.
        return false;
    }

    C<IR*> * irct;
    IR * lastir = BB_irlist(gobackbb).get_tail(&irct);
    ASSERT0(lastir->is_goto());

    IRBB * head = LI_loop_head(li);
    ASSERT0(head);

    //Copy ir in header to gobackbb.
    IR * last_cond_br = NULL;
    DUIter di = NULL;
    Vector<IR*> rmvec;
    for (IR * ir = BB_first_ir(head);
         ir != NULL; ir = BB_next_ir(head)) {
        IR * newir = m_ru->dupIRTree(ir);

        m_du->copyIRTreeDU(newir, ir, true);

        m_ii.clean();
        for (IR * x = iterRhsInit(ir, m_ii);
             x != NULL; x = iterRhsNext(m_ii)) {
            if (!x->isMemoryRef()) { continue; }

            UINT cnt = 0;
            if (x->isReadPR() && PR_ssainfo(x) != NULL) {
                IR * def = SSA_def(PR_ssainfo(x));
                if (def != NULL &&
                    li->is_inside_loop(BB_id(def->getBB()))) {
                    rmvec.set(cnt++, def);
                }
            } else {
                DUSet const* defset = x->readDUSet();
                if (defset == NULL) { continue; }

                for (INT d = defset->get_first(&di);
                     d >= 0; d = defset->get_next(d, &di)) {
                    IR * def = m_ru->getIR(d);

                    ASSERT0(def->getBB());
                    if (li->is_inside_loop(BB_id(def->getBB()))) {
                        rmvec.set(cnt++, def);
                    }
                }
            }

            if (cnt != 0) {
                for (UINT i = 0; i < cnt; i++) {
                    IR * d = rmvec.get(i);
                    m_du->removeDUChain(d, x);
                }
            }
        }

        BB_irlist(gobackbb).insert_before(newir, irct);
        if (newir->isConditionalBr()) {
            ASSERT0(ir == BB_last_ir(head));
            last_cond_br = newir;
            newir->invertIRType(m_ru);
        }
    }

    ASSERT0(last_cond_br);
    BB_irlist(gobackbb).remove(irct);
    m_ru->freeIR(lastir);
    m_cfg->removeEdge(gobackbb, head); //revise cfg.

    LabelInfo const* loopbody_start_lab =
            loopbody_start_bb->getLabelList().get_head();
    if (loopbody_start_lab == NULL) {
        loopbody_start_lab = ::allocInternalLabel(m_ru->get_pool());
        m_cfg->add_lab(loopbody_start_bb, loopbody_start_lab);
    }
    last_cond_br->setLabel(loopbody_start_lab);

    //Add back edge.
    m_cfg->addEdge(BB_id(gobackbb), BB_id(loopbody_start_bb));

    //Add fallthrough edge.
    m_cfg->addEdge(BB_id(gobackbb), BB_id(next));
    BB_is_fallthrough(next) = true;
    return true;
}


bool IR_LOOP_CVT::find_and_convert(List<LI<IRBB>*> & worklst)
{
    bool change = false;
    while (worklst.get_elem_count() > 0) {
        LI<IRBB> * x = worklst.remove_head();
        IRBB * gobackbb;
        UINT succ1;
        UINT succ2;
        if (is_while_do(x, &gobackbb, &succ1, &succ2)) {
            change |= try_convert(x, gobackbb, succ1, succ2);
        }

        x = LI_inner_list(x);
        while (x != NULL) {
            worklst.append_tail(x);
            x = LI_next(x);
        }
    }
    return change;
}


bool IR_LOOP_CVT::perform(OptCtx & oc)
{
    START_TIMER_AFTER();
    m_ru->checkValidAndRecompute(&oc, PASS_LOOP_INFO, PASS_RPO, PASS_UNDEF);

    LI<IRBB> * li = m_cfg->getLoopInfo();
    if (li == NULL) { return false; }

    List<LI<IRBB>*> worklst;
    while (li != NULL) {
        worklst.append_tail(li);
        li = LI_next(li);
    }

    bool change = find_and_convert(worklst);
    if (change) {
        //DU reference and du chain has maintained.
        ASSERT0(m_ru->verifyMDRef());
        ASSERT0(m_du->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NOPR_DU));

        //All these changed.
        OC_is_reach_def_valid(oc) = false;
        OC_is_avail_reach_def_valid(oc) = false;
        OC_is_live_expr_valid(oc) = false;

        oc.set_flag_if_cfg_changed();
        OC_is_cfg_valid(oc) = true; //Only cfg is avaiable.

        //TODO: make rpo, dom valid.
    }

    END_TIMER_AFTER(getPassName());
    return change;
}

} //namespace xoc
