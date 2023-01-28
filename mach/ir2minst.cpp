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
#include "machinc.h"

namespace mach {

IR2MInst::IR2MInst(Region * rg, MInstMgr * mgr)
{
    ASSERT0(rg && mgr);
    m_rg = rg;
    m_mimgr = mgr;
    m_tm = m_rg->getTypeMgr();
}


TMWORD IR2MInst::mapReg2TMCode(xgen::Reg r)
{
    return xgen::tmMapReg2TMWORD(r);
}


void IR2MInst::convertLabel(IR const* ir, OUT RecycMIList & mis)
{
    MInst * mi = m_mimgr->buildLabel();
    MI_lab(mi) = LAB_lab(ir);
    mis.append_tail(mi);
}


//Process unary operation.
void IR2MInst::convertUnaryOp(IR const* ir, OUT RecycMIList & mis,
                              MOD IMCtx * cont)
{
    ASSERTN(ir->isUnaryOp() && UNA_opnd(ir), ("missing operand"));
}


//Process binary operation.
void IR2MInst::convertBinaryOp(IR const* ir, OUT RecycMIList & mis,
                               MOD IMCtx * cont)
{
    ASSERTN(BIN_opnd0(ir) && BIN_opnd1(ir), ("missing operand"));
}


void IR2MInst::convertFalsebr(IR const* ir, OUT RecycMIList & mis,
                              MOD IMCtx * cont)
{
    ASSERT0(ir && ir->is_falsebr());
    IR * newir = m_rg->dupIRTree(ir);
    IR * br_det = BR_det(newir);
    ASSERT0(br_det->is_lt() || br_det->is_le() || br_det->is_gt() ||
            br_det->is_ge() || br_det->is_eq() || br_det->is_ne());
    IR_code(br_det) = IR::invertIRCode(br_det->getCode());
    IR_code(newir) = IR_TRUEBR;
    convertTruebr(newir, mis, cont);
    m_rg->freeIRTree(newir);
}


//Generate compare operations and return the comparation result registers.
//The output registers in IMCtx are ResultSR,
//TruePredicatedSR, FalsePredicatedSR.
//The ResultSR record the boolean value of comparison of relation operation.
//    e.g:
//        a - 1 > b + 2
//    =>
//        sr0 = a - 1
//        sr1 = b + 2
//        sr2 <- cmp.gt sr0, sr1
//        return sr2
//   e.g2:
//is_invert: true if generated inverted operation.
//  e.g: given a <= b, generate !(a > b)
void IR2MInst::convertRelationOp(IR const* ir, OUT RecycMIList & mis,
                                 MOD IMCtx * cont)
{
}


void IR2MInst::convertBBLabel(IRBB const* bb, OUT RecycMIList & mis)
{
    ASSERT0(bb);
    LabelInfoListIter it;
    for (bb->getLabelListConst().get_head(&it);
         it != bb->getLabelListConst().end();
         bb->getLabelListConst().get_next(&it)) {
        mis.append_tail(m_mimgr->buildLabel());
    }
}


void IR2MInst::convert(IR const* ir, OUT RecycMIList & mis, MOD IMCtx * cont)
{
    ASSERT0(ir && ir->verify(m_rg));
    RecycMIList tmis(getRecycMIListMgr());
dumpIR(ir, m_rg);//hack
    switch (ir->getCode()) {
    case IR_ST:
        convertStoreVar(ir, tmis, cont);
        break;
    case IR_STPR:
        convertStorePR(ir, tmis, cont);
        break;
    case IR_TRUEBR:
        convertTruebr(ir, tmis, cont);
        break;
    case IR_FALSEBR:
        convertFalsebr(ir, tmis, cont);
        break;
    case IR_RETURN:
        convertReturn(ir, tmis, cont);
        break;
    default: convertExtStmt(ir, tmis, cont);
    }
    mis.move_tail(tmis);
}


void IR2MInst::convertIRListToMIList(OUT RecycMIList & milst)
{
    IMCtx cont;
    for (IR * ir = m_rg->getIRList(); ir != nullptr; ir = ir->get_next()) {
        cont.clean();
        convert(ir, milst, &cont);
    }
}


void IR2MInst::convertIRBBListToMIList(OUT RecycMIList & milst)
{
    BBList * ir_bb_list = m_rg->getBBList();
    ASSERT0(ir_bb_list);
    IMCtx cont;
    BBListIter bbit;
m_rg->getRegionMgr()->getProgramRegion()->dump(true);//hack
    for (IRBB * bb = ir_bb_list->get_head(&bbit);
         bb != nullptr; bb = ir_bb_list->get_next(&bbit)) {
        convertBBLabel(bb, milst);
        IRListIter irit;
        for (bb->getIRList().get_head(&irit);
             irit != BB_irlist(bb).end();
             irit = bb->getIRList().get_next(irit)) {
            cont.clean();
            convert(irit->val(), milst, &cont);
        }
    }
}


//Translate IR in IRBB to a list of MInst
void IR2MInst::convertToMIList(OUT RecycMIList & milst)
{
    START_TIMER(t, "Convert IR to MInst");
    ASSERT0(m_rg);
    IR * ir_list = m_rg->getIRList();
    if (ir_list != nullptr) {
        ASSERT0(m_rg->getBBList() == nullptr ||
                m_rg->getBBList()->get_elem_count() == 0);
        convertIRListToMIList(milst);
        return;
    }
    convertIRBBListToMIList(milst);
    END_TIMER(t, "Convert IR to MInst");
}

} //namespace
