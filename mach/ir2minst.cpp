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
#include "../opt/comopt.h"

namespace mach {

IR2MInst::IR2MInst(Region * rg, MInstMgr * mgr, elf::ELFMgr * em)
{
    ASSERT0(rg && mgr);
    m_rg = rg;
    m_mimgr = mgr;
    m_tm = m_rg->getTypeMgr();
    m_em = em;
}


TMWORD IR2MInst::mapReg2TMCode(xgen::Reg r)
{
    return xgen::tmMapReg2TMWORD(r);
}


void IR2MInst::convertLabel(IR const* ir, OUT RecycMIList & mis,
                            MOD IMCtx * cont)
{
    MInst * mi = m_mimgr->buildLabel();
    ASSERT0(ir->getLabel());
    MI_lab(mi) = ir->getLabel();
    mis.append_tail(mi);
    IMCTX_label_num(cont)++;
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


void IR2MInst::convertBBLabel(IRBB const* bb, OUT RecycMIList & mis,
                              MOD IMCtx * cont)
{
    ASSERT0(bb);
    LabelInfoListIter it;
    for (LabelInfo const* li = bb->getLabelListConst().get_head(&it);
         it != bb->getLabelListConst().end();
         li = bb->getLabelListConst().get_next(&it)) {
        MInst * mi = m_mimgr->buildLabel();
        ASSERT0(mi && li);
        MI_lab(mi) = li;
        mis.append_tail(mi);
        IMCTX_label_num(cont)++;
    }
}


void IR2MInst::processHintOfAfterRet(OUT RecycMIList & mis, MOD IMCtx * cont)
{
    xoc::MCDwarfMgr * dm = m_rg->getRegionMgr()->getDwarfMgr();
    ASSERT0(dm);
    xcom::Vector<LabelInfo const*> * hint_ir_v =
        MCDWARFMGR_ret_after_hint_map_region(dm).get(m_rg);
    ASSERT0(hint_ir_v);
    for (UINT i = 0; i < hint_ir_v->get_elem_count(); i++) {
        MInst * mi = m_mimgr->buildLabel();
        LabelInfo const* li = (*hint_ir_v)[i];
        ASSERT0(mi && li);
        MI_lab(mi) = li;
        mis.append_tail(mi);
        IMCTX_label_num(cont)++;
    }
}


void IR2MInst::convert(IR const* ir, OUT RecycMIList & mis, MOD IMCtx * cont)
{
    ASSERT0(ir && ir->verify(m_rg));
    switch (ir->getCode()) {
    case IR_ST:
        convertStoreVar(ir, mis, cont);
        break;
    case IR_IST:
        convertIStoreVar(ir, mis, cont);
        break;
    case IR_STPR:
        convertStorePR(ir, mis, cont);
        break;
    case IR_TRUEBR:
        convertTruebr(ir, mis, cont);
        break;
    case IR_FALSEBR:
        convertFalsebr(ir, mis, cont);
        break;
    case IR_RETURN:
        convertReturn(ir, mis, cont);
        break;
    case IR_CALL:
        convertCall(ir, mis, cont);
        break;
    case IR_ICALL:
        convertICall(ir, mis, cont);
        break;
    case IR_GETELEM:
        convertExtract(ir, mis, cont);
        break;
    case IR_SETELEM:
        convertSetElem(ir, mis, cont);
        break;
    case IR_GOTO:
        convertGoto(ir, mis, cont);
        break;
    case IR_IGOTO:
        convertIgoto(ir, mis, cont);
        break;
    case IR_CFI_DEF_CFA:
        convertCFIDefCfa(ir, mis, cont);
        break;
    case IR_CFI_SAME_VALUE:
        convertCFISameValue(ir, mis, cont);
        break;
    case IR_CFI_OFFSET:
        convertCFIOffset(ir, mis, cont);
        break;
    case IR_CFI_RESTORE:
        convertCFIRestore(ir, mis, cont);
        break;
    case IR_CFI_DEF_CFA_OFFSET:
        convertCFICfaOffset(ir, mis, cont);
        break;
    case IR_LABEL:
        convertLabel(ir, mis, cont);
        break;
    default: convertExtStmt(ir, mis, cont);
    }
}


//Extract the constant value from 'val' that the size is conform to given
//field type.
TMWORD IR2MInst::extractImm(HOST_INT val, FIELD_TYPE ft)
{
    UINT start = 0;
    UINT end = getMIMgr()->getFieldSize(ft) - 1;
    return (TMWORD)xcom::extractBitRangeValue((ULONGLONG)val, start, end);
}


void IR2MInst::convertIRListToMIList(OUT RecycMIList & milst,
                                     MOD IMCtx * cont)
{
    for (IR * ir = m_rg->getIRList(); ir != nullptr; ir = ir->get_next()) {
        cont->clean();
        convert(ir, milst, cont);
    }
}


void IR2MInst::convertIRBBListToMIList(OUT RecycMIList & milst,
                                       MOD IMCtx * cont)
{
    BBList * ir_bb_list = m_rg->getBBList();
    ASSERT0(ir_bb_list);
    BBListIter bbit;

    for (IRBB * bb = ir_bb_list->get_head(&bbit);
         bb != nullptr; bb = ir_bb_list->get_next(&bbit)) {
        convertBBLabel(bb, milst, cont);
        IRListIter irit;
        for (bb->getIRList().get_head(&irit);
             irit != BB_irlist(bb).end();
             irit = bb->getIRList().get_next(irit)) {
            convert(irit->val(), milst, cont);
        }
    }

    //Insert some hints for the final milst.
    if (xoc::g_debug) {
        processHintOfAfterRet(milst, cont);
    }
}


//Translate IR in IRBB to a list of MInst
void IR2MInst::convertToMIList(OUT RecycMIList & milst, MOD IMCtx * cont)
{
    START_TIMER(t, "Convert IR to MInst");
    ASSERT0(m_rg);
    IR * ir_list = m_rg->getIRList();
    if (ir_list != nullptr) {
        ASSERT0(m_rg->getBBList() == nullptr ||
                m_rg->getBBList()->get_elem_count() == 0);
        convertIRListToMIList(milst, cont);
        return;
    }
    convertIRBBListToMIList(milst, cont);
    END_TIMER(t, "Convert IR to MInst");
}


void IR2MInst::convertCFIDefCfa(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont)
{
    ASSERT0(ir->is_cfi_def_cfa());
    ASSERT0(xoc::g_debug);
    IR const* rhs0 = CFI_CFA_KID(ir, 0);
    ASSERT0(rhs0->getCode() == IR_CONST);
    UINT reg_num = (UINT)((CConst*)rhs0)->getInt();

    IR const* rhs1 = CFI_CFA_KID(ir, 1);
    ASSERT0(rhs1->getCode() == IR_CONST);
    INT cfa_offset = (INT)((CConst*)rhs1)->getInt();

    MInst * mi = getMIMgr()->buildCFIDefCfa();
    ASSERT0(mi);
    MI_cfi_def_cfa_offset(mi) = cfa_offset;
    MI_cfi_def_cfa_register(mi) = reg_num;
    mis.append_tail(mi);
    mis.copyDbx(ir, getDbxMgr());
    IMCTX_cfi_num(cont)++;
}


void IR2MInst::convertCFISameValue(IR const* ir, OUT RecycMIList & mis,
                                   MOD IMCtx * cont)
{
    ASSERT0(ir->is_cfi_same_value());
    ASSERT0(xoc::g_debug);
    IR const* rhs0 = CFI_SAME_VALUE_kid(ir, 0);
    ASSERT0(rhs0->getCode() == IR_CONST);
    UINT reg_num = (UINT)((CConst*)rhs0)->getInt();
    MInst * mi = getMIMgr()->buildCFISameValue();
    ASSERT0(mi);
    MI_cfi_samevalue_register(mi) = reg_num;

    mis.append_tail(mi);
    mis.copyDbx(ir, getDbxMgr());
    IMCTX_cfi_num(cont)++;
}


void IR2MInst::convertCFIOffset(IR const* ir, OUT RecycMIList & mis,
                                MOD IMCtx * cont)
{
    ASSERT0(ir->is_cfi_offset());
    ASSERT0(xoc::g_debug);
    IR const* rhs0 = CFI_OFFSET_kid(ir, 0);
    ASSERT0(rhs0->getCode() == IR_CONST);
    UINT reg_num = (UINT)((CConst*)rhs0)->getInt();

    IR const* rhs1 = CFI_OFFSET_kid(ir, 1);
    ASSERT0(rhs1->getCode() == IR_CONST);
    INT offset = (INT)((CConst*)rhs1)->getInt();
    MInst * mi = getMIMgr()->buildCFIOffset();
    ASSERT0(mi);

    MI_cfi_offset_offset(mi) = offset;
    MI_cfi_offset_register(mi)  = reg_num;
    mis.append_tail(mi);
    mis.copyDbx(ir, getDbxMgr());
    IMCTX_cfi_num(cont)++;
}


void IR2MInst::convertCFIRestore(IR const* ir, OUT RecycMIList & mis,
                                 MOD IMCtx * cont)
{
    ASSERT0(ir->is_cfi_restore());
    ASSERT0(xoc::g_debug);
    IR const* rhs0 = CFI_RESTORE_kid(ir, 0);
    ASSERT0(rhs0->getCode() == IR_CONST);
    UINT reg_num = (UINT)((CConst*)rhs0)->getInt();

    MInst * mi = getMIMgr()->buildCFIRestore();
    ASSERT0(mi);
    MI_cfi_restore_register(mi) = reg_num;

    mis.append_tail(mi);
    mis.copyDbx(ir, getDbxMgr());
    IMCTX_cfi_num(cont)++;
}


void IR2MInst::convertCFICfaOffset(IR const* ir, OUT RecycMIList & mis,
                                   MOD IMCtx * cont)
{
    ASSERT0(ir->is_cfi_def_cfa_offset());
    ASSERT0(xoc::g_debug);
    IR const* rhs0 = CFI_DEF_CFA_OFFSET_KID(ir, 0);
    ASSERT0(rhs0->getCode() == IR_CONST);
    INT cfa_offset = (INT)((CConst*)rhs0)->getInt();

    MInst * mi = getMIMgr()->buildCFIDefCfaOffset();
    ASSERT0(mi);
    MI_cfi_def_cfa_offset_offset(mi) = cfa_offset;

    mis.append_tail(mi);
    mis.copyDbx(ir, getDbxMgr());
    IMCTX_cfi_num(cont)++;
}
} //namespace
