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

static Type const* meetType(Type const* t1, Type const* t2, TypeMgr * tm)
{
    //Upbound and Lowerbound is ANY type.
    if (t1->is_any()) {
        if (t2->is_any()) { return t2; }
        return t2;
    }
    if (t2->is_any()) {
        if (t1->is_any()) { return t1; }
        return t1;
    }
    UINT sz1 = tm->getBitSize(t1);
    UINT sz2 = tm->getBitSize(t2);
    return sz1 > sz2 ? t1 : t2;
}


//
//START InferType
//

void InferType::addDump(Var const* var) const
{
    if (m_changed_varlist != nullptr) {
        m_changed_varlist->append_tail(var);
    }
}


void InferType::addChanged(IR * ir)
{
    m_wl.append_tail(ir);
}


void InferType::addDump(IR const* ir) const
{
    if (m_changed_irlist != nullptr) {
        m_changed_irlist->append_tail(ir);
    }
}


//Infer variable's type.
bool InferType::inferVarTypeByIRCode(IR const* ir) const
{
    if (ir->is_any()) { return false; }
    Var * var = nullptr;
    switch (ir->getCode()) {
    SWITCH_CASE_CALL:
    case IR_PHI:
    case IR_STPR:
    SWITCH_CASE_READ_PR: {
        MD const* ref = ir->getRefMD();
        if (ref != nullptr) {
            var = ref->get_base();
            break;
        }
        var = m_rg->getVarByPRNO(ir->getPrno());
        break;
    }
    SWITCH_CASE_DIRECT_MEM_OP:
        var = ir->getIdinfo();
        break;
    default:;
    }
    if (var == nullptr || !var->is_any()) { return false; }
    Type const* newtype = meetType(var->getType(), ir->getType(), m_tm);
    if (newtype != var->getType()) {
        VAR_type(var) = newtype;
        addDump(var);
        return true;
    }
    return false;
}


bool InferType::inferStmtCall(IR * ir) const
{
    ASSERT0(ir->isCallStmt());
    return inferVarTypeByIRCode(ir);
}


bool InferType::inferStmtPhi(IR * ir) const
{
    ASSERT0(ir->is_phi());
    bool change = false;
    if (ir->is_any()) {
        Type const* newty = nullptr;
        for (IR const* opnd = PHI_opnd_list(ir);
             opnd != nullptr; opnd = opnd->get_next()) {
            if (opnd->is_any()) { break; }
            if (newty == nullptr) { newty = opnd->getType(); continue; }
            newty = meetType(newty, opnd->getType(), m_tm);
        }
        if (newty != nullptr && newty != ir->getType()) {
            IR_dt(ir) = newty;
            addDump(ir);
            const_cast<InferType*>(this)->addChanged(ir);
            change = true;
        }
    }
    change |= inferVarTypeByIRCode(ir);
    return change;
}


bool InferType::inferStmtMemAcc(IR * ir)
{
    ASSERT0(ir->isMemRef() && ir->is_stmt());
    ASSERT0(ir->getRHS());
    bool changed = false;
    IR * rhs = ir->getRHS();
    if (rhs != nullptr) {
        if (ir->getRHS()->is_any()) {
            if (ir->is_any()) { return false; }
            IR_dt(ir->getRHS()) = ir->getType();
            addDump(ir->getRHS());
            addChanged(ir->getRHS());
            changed = true;
        } else if (ir->is_any()) {
            IR_dt(ir) = ir->getRHS()->getType();
            changed = true;
            addDump(ir);
            addChanged(ir);
        }
    } else {
        //Virtual OP may not have RHS.
    }
    changed |= inferVarTypeByIRCode(ir);
    return true;
}


bool InferType::inferLeafExpMemAcc(IR * ir)
{
    ASSERT0(ir->isMemOpnd() && ir->is_exp() && ir->is_leaf());
    if (ir->is_any()) {
        MD const* ref = ir->getRefMD();
        if (ref != nullptr && !ref->get_base()->is_any()) {
            IR_dt(ir) = ref->get_base()->getType();
            addDump(ir);
            addChanged(ir);
            return true;
        }
        Var const* v;
        if (ir->is_pr() &&
            (v = m_rg->getVarByPRNO(PR_no(ir))) != nullptr &&
            !v->is_any()) {
            IR_dt(ir) = v->getType();
            addDump(ir);
            addChanged(ir);
            return true;
        }
        return false;
    }
    return inferVarTypeByIRCode(ir);
}


bool InferType::inferArray(IR * ir) const
{
    ASSERT0(ir->is_array());
    if (!ir->is_any()) { return false; }
    //TODO: infer the array operator type via array base type.
    return false;
}


bool InferType::inferIld(IR * ir)
{
    ASSERT0(ir->is_ild());
    if (!ir->is_any()) { return false; }

    IR const* base = ir->getBase();
    ASSERT0(base);
    if (!base->is_ptr()) { return false; }

    IR_dt(ir) = m_tm->getUIntType(m_tm->getPointerBaseByteSize(
        base->getType()));
    addDump(ir);
    addChanged(ir);
    return true;
}


bool InferType::inferExpMemAcc(IR * ir)
{
    ASSERT0(ir->isMemOpnd() && ir->is_exp());
    if (ir->is_leaf()) {
        return inferLeafExpMemAcc(ir);
    }
    if (ir->is_ild()) {
        return inferIld(ir);
    }
    if (ir->is_array()) {
        return inferArray(ir);
    }
    return false;
}


bool InferType::inferUnaOP(IR * ir)
{
    if (!ir->is_any()) { return false; }
    ASSERT0(ir->isUnaryOp());
    IR * op = UNA_opnd(ir);
    if (op->is_any()) { return false; }
    IR_dt(ir) = op->getType();
    addDump(ir);
    addChanged(ir);
    return true;
}


bool InferType::inferSelect(IR * ir)
{
    if (!ir->is_any()) { return false; }
    ASSERT0(ir->is_select());
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
    addChanged(ir);
    return true;
}


static Type const* inferPointerArith(IR const* ir, Type const* rety)
{
    switch (ir->getCode()) {
    case IR_ADD:
    case IR_SUB: {
        IR * op0 = BIN_opnd0(ir);
        IR * op1 = BIN_opnd1(ir);
        if (op0->is_ptr() && !op1->is_ptr()) {
            return op0->getType();
        }
        if (!op0->is_ptr() && op1->is_ptr()) {
            return op1->getType();
        }
        return rety;
    }
    default: break;
    }
    return rety;
}


bool InferType::inferBinOP(IR * ir)
{
    if (!ir->is_any()) { return false; }
    ASSERT0(ir->isBinaryOp());
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
    rety = inferPointerArith(ir, rety);
    ASSERT0(rety);
    ir->setType(rety);
    addDump(ir);
    addChanged(ir);
    return true;
}


//The function attempts to infer ANY type recursively.
bool InferType::inferIR(IR * ir)
{
    bool changed = false;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        changed |= inferIRList(kid);
    }
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
    SWITCH_CASE_EXP_MEM_OP:
        changed |= inferExpMemAcc(ir);
        return changed;
    SWITCH_CASE_STMT_MEM_OP:
        changed |= inferStmtMemAcc(ir);
        return changed;
    SWITCH_CASE_CALL:
        changed |= inferStmtCall(ir);
        return changed;
    case IR_PHI:
        changed |= inferStmtPhi(ir);
        return changed;
    default:;
    }
    return changed;
}


bool InferType::inferBBList(BBList const* bbl)
{
    bool changed = false;
    BBListIter bbit;
    for (IRBB * bb = bbl->get_head(&bbit); bb != nullptr;
         bb = bbl->get_next(&bbit)) {
        BBIRList & irlist = bb->getIRList();
        BBIRListIter irit;
        for (IR * ir = irlist.get_head(&irit); ir != nullptr;
             ir = irlist.get_next(&irit)) {
            changed |= inferIR(ir);
        }
    }
    return changed;
}


bool InferType::inferIRList(IR * irl)
{
    bool changed = false;
    for (IR * ir = irl; ir != nullptr; ir = ir->get_next()) {
        changed |= inferIR(ir);
    }
    return changed;
}


bool InferType::inferChangedList()
{
    bool changed = false;
    IRListIter it;
    for (IR * ir = m_wl.get_head(&it);
         ir != nullptr; ir = m_wl.get_next(&it)) {
        //No need to infer ir's sibling since it is added as single IR.
        changed |= inferIR(ir);
    }
    return changed;
}


void InferType::dumpInit()
{
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpInferType()) {
        ASSERT0(m_changed_irlist == nullptr);
        m_changed_irlist = new ConstIRList();
        ASSERT0(m_changed_varlist == nullptr);
        m_changed_varlist = new List<Var const*>();
    }
}


void InferType::dumpFini()
{
    if (m_changed_irlist != nullptr) {
        delete m_changed_irlist;
        m_changed_irlist = nullptr;
    }
    if (m_changed_varlist != nullptr) {
        delete m_changed_varlist;
        m_changed_varlist = nullptr;
    }
}


bool InferType::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    getRegion()->getLogMgr()->incIndent(2);
    ASSERT0(m_changed_irlist);
    if (m_changed_irlist->get_elem_count() > 0) {
        note(getRegion(), "\n==-- CHANGED IR --==");
        for (IR const* ir = m_changed_irlist->get_head();
             ir != nullptr; ir = m_changed_irlist->get_next()) {
            dumpIR(ir, m_rg);
        }
    }

    ASSERT0(m_changed_varlist);
    if (m_changed_varlist->get_elem_count() > 0) {
        note(getRegion(), "\n==-- CHANGED VAR --==");
        for (Var const* var = m_changed_varlist->get_head();
             var != nullptr; var = m_changed_varlist->get_next()) {
            var->dump(m_vm);
        }
    }
    note(getRegion(), "\n");
    bool res = Pass::dump();
    getRegion()->getLogMgr()->decIndent(2);
    return res;
}


bool InferType::perform(OptCtx & oc)
{
    IR * irl = m_rg->getIRList();
    BBList * bbl = m_rg->getBBList();
    START_TIMER(t, getPassName());
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    dumpInit();
    bool changed = false;
    do {
        m_wl.destroy();
        m_wl.init();
        //TODO: iteratively infering type via DU chain.
        if (bbl != nullptr && bbl->get_elem_count() != 0) {
            changed = inferBBList(bbl);
        }
        else if (irl != nullptr) {
            changed = inferIRList(irl);
        }
        changed |= inferChangedList();
    } while (m_wl.get_elem_count() != 0);
    if (!changed) {
        m_rg->getLogMgr()->cleanBuffer();
        dumpFini();
        END_TIMER(t, getPassName());
        return false;
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpInferType()) {
        dump();
    }
    dumpFini();
    END_TIMER(t, getPassName());
    return true;
}
//END InferType

} //namespace xoc
