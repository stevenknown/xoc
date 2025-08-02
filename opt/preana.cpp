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

//
//START PreAnaBeforeOpt
//
PreAnaBeforeOpt::PreAnaBeforeOpt(Region * rg) : Pass(rg), m_rg(rg)
{
    ASSERT0(m_rg != nullptr);
    m_rm = m_rg->getRegionMgr();
    m_tm = m_rg->getTypeMgr();
    m_mdsys = m_rg->getMDSystem();
    m_vm = m_rg->getVarMgr();
}


bool PreAnaBeforeOpt::dump() const
{
    return true;
}


bool PreAnaBeforeOpt::scanBBList(BBList const* bblst)
{
    BBListIter bbit;
    bool changed = false;
    for (IRBB const* bb = bblst->get_head(&bbit);
         bb != nullptr; bb = bblst->get_next(&bbit)) {
        BBIRListIter irct;
        for (IR const* ir = const_cast<IRBB*>(bb)->getIRList().get_head(&irct);
             ir != nullptr;
             ir = const_cast<IRBB*>(bb)->getIRList().get_next(&irct)) {
            changed |= scanIRList(ir);
        }
    }
    return changed;
}


bool PreAnaBeforeOpt::scanCallStmt(IR const* ir)
{
    ASSERT0(ir->isCallStmt());
    bool changed = false;
    if (g_do_call_graph && !CALL_is_intrinsic(ir)) {
        ConstIRList * cl = m_rg->getCallList();
        ASSERT0(cl);
        cl->append_tail(ir);
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k == nullptr) { continue; }
        ASSERT0(IR_parent(k) == ir);
        changed |= scanIRList(k);
    }
    return changed;
}


bool PreAnaBeforeOpt::scanCase(IR const* ir)
{
    ASSERT0(ir && ir->is_case());
    IR const* stmt = ir->getStmt();
    ASSERT0(stmt);
    if (stmt->isBranch()) {
        //Case-IRs used in Branch Stmt do not need to set label attributes.
        return false;
    }

    //CASE: The label in Case-IR will be used as an address and should NOT
    //be removed even if the label does not appeared in BB label list.
    ASSERT0(stmt->isStoreStmt());
    LabelInfo * li = const_cast<LabelInfo*>(CASE_lab(ir));
    ASSERT0(li);
    LABELINFO_is_refed_by_ir(li) = true;
    return true;
}


bool PreAnaBeforeOpt::scanLDA(IR const* ir)
{
    ASSERT0(ir->is_lda());
    ASSERT0(LDA_idinfo(ir));
    Var * v = LDA_idinfo(ir);
    if (v->is_string()) {
        if (m_rm->getAndGenDedicateStrMD() != nullptr) {
            //Treats all string variables as the same one.
            return false;
        }
        Var * sv = m_vm->registerStringVar(
            nullptr, VAR_string(v), MEMORY_ALIGNMENT, SS_UNDEF);
        ASSERT0(sv);
        sv->setFlag(VAR_ADDR_TAKEN);
        return true;
    }
    if (v->is_label()) {
        return false; //nothing to do.
    }
    //General variable.
    IR const* parent = ir->getParent();
    ASSERT0(parent);
    if (parent->isArrayOp() && parent->isArrayBase(ir)) {
        ; //nothing to do.
    } else {
        //If LDA is the base of ARRAY, say (&a)[..], its
        //address does not need to mark as address taken.
        LDA_idinfo(ir)->setFlag(VAR_ADDR_TAKEN);
    }

    // ...=&x.a, address of 'x.a' is taken.
    MD md;
    MD_base(&md) = LDA_idinfo(ir); //correspond to Var
    MD_ofst(&md) = LDA_ofst(ir);
    MD_size(&md) = ir->getTypeSize(m_tm);
    MD_ty(&md) = MD_EXACT;
    m_mdsys->registerMD(md);
    return true;
}


bool PreAnaBeforeOpt::scanID(IR const* ir)
{
    ASSERT0(ir->is_id());
    //Array base must not be ID. It could be
    //LDA or computational expressions.
    //In C, array base address could be assgined to other variable.
    //Its address should be marked as taken.
    // And it's parent must be LDA.
    //e.g: Address of 'a' is taken.
    //    int a[10];
    //    int * p;
    //    p = a;
    ASSERT0(ID_info(ir));
    return false;
}


bool PreAnaBeforeOpt::scanReturn(IR const* ir)
{
    ASSERT0(ir->is_return());
    bool changed = false;
    if (g_do_call_graph) {
        ConstIRList * cl = m_rg->getReturnList();
        ASSERT0(cl);
        cl->append_tail(ir);
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k == nullptr) { continue; }
        ASSERT0(IR_parent(k) == ir);
        changed |= scanIRList(k);
    }
    return changed;
}


bool PreAnaBeforeOpt::scanExtOp(IR const* ir)
{
    ASSERT0(ir->isExtOp());
    //For the convenience of users, we do not assert here, and only collect
    //the information that will be used by common passes.
    //ASSERTN(0, ("Target Dependent Code"));
    return false;
}


bool PreAnaBeforeOpt::scanKid(IR const* ir)
{
    bool changed = false;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k == nullptr) { continue; }
        ASSERT0(IR_parent(k) == ir);
        changed |= scanIRList(k);
    }
    return changed;
}


//Prepare informations for analysis phase, such as record
//which variables have been taken address for both
//global and local variable.
bool PreAnaBeforeOpt::scanIRList(IR const* ir)
{
    bool changed = false;
    for (; ir != nullptr; ir = ir->get_next()) {
        switch (ir->getCode()) {
        SWITCH_CASE_DIRECT_MEM_STMT:
            changed |= scanIRList(ir->getRHS());
            break;
        SWITCH_CASE_CALL:
            changed |= scanCallStmt(ir);
            break;
        case IR_LDA:
            changed |= scanLDA(ir);
            break;
        case IR_ID:
            changed |= scanID(ir);
            break;
        SWITCH_CASE_LOOP_ITER_CFS_OP:
        SWITCH_CASE_DIRECT_MEM_EXP:
        case IR_CONST:
        case IR_GOTO:
        case IR_LABEL:
        SWITCH_CASE_READ_PR:
        case IR_PHI:
        case IR_REGION:
            //Nothing to do.
            break;
        case IR_RETURN:
            changed |= scanReturn(ir);
            break;
        case IR_CASE:
            changed |= scanCase(ir);
            break;
        default:
            if (ir->isExtOp()) {
                changed |= scanExtOp(ir);
            }
            changed |= scanKid(ir);
            break;
        }
    }
    return changed;
}


void PreAnaBeforeOpt::clean() const
{
    m_rg->getReturnList()->clean();
    m_rg->getCallList()->clean();
}


bool PreAnaBeforeOpt::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    bool changed = false;
    clean();
    if (m_rg->getIRList() != nullptr) {
        changed |= scanIRList(m_rg->getIRList());
    } else {
        ASSERT0(m_rg->getBBList());
        changed |= scanBBList(m_rg->getBBList());
    }
    END_TIMER(t, getPassName());
    return changed;
}
//END PreAnaBeforeOpt

} //namespace xoc
