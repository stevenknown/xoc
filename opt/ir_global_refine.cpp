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

IR * GlobalRefine::refineBr(IR * ir, bool & change, MOD OptCtx & oc)
{
    ASSERT0(ir && (ir->is_truebr() || ir->is_falsebr()));

    //TODO: After CFG contruction, transforming conditional branch operations
    //would disrupt the control flow, making it less suitable to implement it
    //in Refine.
    if (oc.isPassValid(PASS_CFG)) { return ir; }
    IR const* rhs = BR_det(ir);
    ASSERT0(rhs);
    if (!rhs->is_lt() && !rhs->is_ge()) { return ir; }
    IR const* opnd0 = BIN_opnd0(rhs);
    IR const* opnd1 = BIN_opnd1(rhs);
    ASSERT0(opnd0 && opnd1);
    if (!opnd0->isReadPR() || !opnd0->is_uint()) { return ir; }
    ASSERT0(opnd1->isReadPR() || opnd1->is_const());
    if (opnd1->isReadPR() || CONST_int_val(opnd1) != 0) { return ir; }

    //These two conditions will not be triggered.
    //  1. if (uxx < 0) { ...... }        //e.g. u8/u16/u32/u64
    //  2. if (!(uxx >= 0)) { ...... }    //e.g. u8/u16/u32/u64
    bool must_false = (ir->is_truebr() && rhs->is_lt()) ||
        (ir->is_falsebr() && rhs->is_ge());
    IRMgr * im = m_rg->getIRMgr();
    IR * ir_new = must_false ? nullptr : im->buildGoto(BR_lab(ir));
    if (ir_new != nullptr) { xoc::copyDbx(ir_new, ir, m_rg); }
    m_rg->freeIRTree(ir);
    change = true;
    return ir_new;
}


IR * GlobalRefine::refineIR(IR * ir, bool & change, MOD OptCtx & oc)
{
    ASSERT0(ir);
    if (!g_do_global_refine) { return ir; }
    if (ir == nullptr) { return nullptr; }
    if (ir->hasSideEffect(false) || ir->isDummyOp()) { return ir; }
    bool tmpc = false;
    switch (ir->getCode()) {
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        ir = refineBr(ir, tmpc, oc);
        break;
    default: break;
    }
    change |= tmpc;
    return ir;
}


IR * GlobalRefine::refineIRList(IR * ir_list, bool & change, MOD OptCtx & oc)
{
    bool lchange = true; //local flag
    while (lchange) {
        lchange = false;
        IR * new_list = nullptr;
        IR * last = nullptr;
        while (ir_list != nullptr) {
            IR * ir = xcom::removehead(&ir_list);
            IR * newIR = refineIR(ir, lchange, oc);
            xcom::add_next(&new_list, &last, newIR);
        }
        change |= lchange;
        ir_list = new_list;
    }
    return ir_list;
}


bool GlobalRefine::dump() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpGlobalRefine()) {
        return false;
    }
    note(m_rg, "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    if (m_rg->getIRList() != nullptr) {
        dumpIRList(m_rg->getIRList(), m_rg);
    } else if (m_rg->getBBList() != nullptr) {
        dumpBBList(m_rg->getBBList(), m_rg);
    }
    m_rg->getLogMgr()->decIndent(2);
    return true;
}


bool GlobalRefine::perform(OptCtx & oc)
{
    bool change = false;
    START_TIMER(t, "Do Primitive Refinement");
    if (m_rg->getIRList() != nullptr) {
        IR * irs = refineIRList(m_rg->getIRList(), change, oc);
        ASSERT0(xoc::verifyIRList(irs, nullptr, m_rg));
        m_rg->setIRList(irs);
    } else {
        change = refineBBList(m_rg->getBBList(), oc);
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
        ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
        ASSERT0(m_rg->getCFG()->verifyRPO(oc));
        ASSERT0(m_rg->getCFG()->verifyLoopInfo(oc));
        ASSERT0(m_rg->getCFG()->verifyDomAndPdom(oc));
    }
    END_TIMER(t, "Do Primitive Refinement");
    if (g_dump_opt.isDumpAfterPass()) { dump(); }
    if (change) { oc.setInvalidCFG(); oc.setInvalidIfCFGChanged(); }
    return change;
}
//END GlobalRefine

} //namespace xoc
