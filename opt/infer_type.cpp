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

//
//START InferType
//

void InferType::addDump(IR const* ir) const
{
    m_changed_irlist->append_tail(ir);
}


bool InferType::inferStmtMemAcc(IR * ir) const
{
    ASSERT0(ir->isMemoryRef() && ir->is_stmt());
    ASSERT0(ir->getRHS());
    if (ir->getRHS()->is_any()) { return false; }
    IR_dt(ir) = ir->getRHS()->getType();
    addDump(ir);
    return true;
}


bool InferType::inferExpMemAcc(IR * ir) const
{
    ASSERT0(ir->isMemoryOpnd() && ir->is_exp());
    if (ir->is_leaf()) { return false; }
    ASSERT0(ir->getBase());
    if (ir->getBase()->is_any()) { return false; }
    IR_dt(ir) = ir->getBase()->getType();
    addDump(ir);
    return true;
}


bool InferType::inferUnaOP(IR * ir) const
{
    ASSERT0(ir->isUnaryOp() && ir->is_any());
    IR * op = UNA_opnd(ir);
    if (op->is_any()) { return false; }
    IR_dt(ir) = op->getType();
    addDump(ir);
    return true;
}


bool InferType::inferSelect(IR * ir) const
{
    ASSERT0(ir->is_select() && ir->is_any());
    IR * texp = SELECT_trueexp(ir);
    IR * fexp = SELECT_falseexp(ir);
    if (texp->is_any() || fexp->is_any()) { return false; }

    Type const* rety = nullptr;
    if (texp->getTypeSize(m_tm) > fexp->getTypeSize(m_tm)) {
        //Choose the larger size type as the return-type.
        rety = texp->getType();
    } else if (texp->getTypeSize(m_tm) == fexp->getTypeSize(m_tm)) {
        if (texp->is_signed()) {
            //Choose the signed type as the return-type.
            rety = texp->getType();
        } else {
            rety = fexp->getType();
        }
    } else {
        rety = fexp->getType();
    }
    ASSERT0(rety);
    IR_dt(ir) = rety;
    addDump(ir);
    return true;
}


bool InferType::inferBinOP(IR * ir) const
{
    ASSERT0(ir->isBinaryOp() && ir->is_any());
    IR * op0 = BIN_opnd0(ir);
    IR * op1 = BIN_opnd1(ir);
    if (op0->is_any() || op1->is_any()) { return false; }

    Type const* rety = nullptr;
    if (op0->getTypeSize(m_tm) > op1->getTypeSize(m_tm)) {
        //Choose the larger size type as the return-type.
        rety = op0->getType();
    } else if (op0->getTypeSize(m_tm) == op1->getTypeSize(m_tm)) {
        if (op0->is_signed()) {
            //Choose the signed type as the return-type.
            rety = op0->getType();
        } else {
            rety = op1->getType();
        }
    } else {
        rety = op1->getType();
    }
    ASSERT0(rety);
    IR_dt(ir) = rety;
    addDump(ir);
    return true;
}


//The function attempts to infer ANY type recursively.
bool InferType::inferIR(IR * ir) const
{
    bool changed = false;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        changed |= inferIR(kid);
    }
    if (!ir->is_any()) { return changed; }

    switch (ir->getCode()) {
    case IR_SELECT:
        changed |= inferSelect(ir);
        return changed;
    SWITCH_CASE_BIN:
        changed |= inferBinOP(ir);
        return changed;
    SWITCH_CASE_UNA:
        changed |= inferUnaOP(ir);
        return changed;
    SWITCH_CASE_EXP_MEM_ACC:
        changed |= inferExpMemAcc(ir);
        return changed;
    SWITCH_CASE_STMT_MEM_ACC:
        changed |= inferStmtMemAcc(ir);
        return changed;
    default:;
    }
    return changed;
}


bool InferType::inferBBList(BBList const* bbl) const
{
    bool changed = false;
    BBListIter bbit;
    for (IRBB * bb = bbl->get_head(&bbit); bb != nullptr;
         bb = bbl->get_next(&bbit)) {
        BBIRList * irlist = bb->getIRList();
        BBIRListIter irit;
        for (IR * ir = irlist->get_head(&irit); ir != nullptr;
             ir = irlist->get_next(&irit)) {
            changed |= inferIR(ir);
        }
    }
    return changed;
}


bool InferType::inferIRList(IR * irl) const
{
    bool changed = false;
    for (IR * ir = irl; ir != nullptr; ir = ir->get_next()) {
        changed |= inferIR(ir);
    }
    return changed;
}


void InferType::dumpInit()
{
    ASSERT0(m_changed_irlist == nullptr);
    m_changed_irlist = new CIRList();
}


void InferType::dumpFini()
{
    ASSERT0(m_changed_irlist);
    delete m_changed_irlist;
    m_changed_irlist = nullptr;
}


bool InferType::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    ASSERT0(m_changed_irlist);
    for (IR const* ir = m_changed_irlist->get_head();
         ir != nullptr; ir = m_changed_irlist->get_next()) {
        dumpIR(ir, m_rg);
    }
    note(getRegion(), "\n");
    return true;
}


bool InferType::perform(OptCtx & oc)
{
    IR * irl = m_rg->getIRList();
    BBList * bbl = m_rg->getBBList();
    START_TIMER(t, getPassName());
    dumpInit();
    bool changed = false;
    if (bbl != nullptr && bbl->get_elem_count() != 0) {
        changed = inferBBList(bbl);
    } else if (irl != nullptr) {
        changed = inferIRList(irl);
    }
    if (!changed) {
        END_TIMER(t, getPassName());
        return false;
    }
    if (g_is_dump_after_pass && g_dump_opt.isDumpInferType()) {
        dump();
    }
    dumpFini();
    END_TIMER(t, getPassName());
    return true;
}
//END InferType

} //namespace xoc
