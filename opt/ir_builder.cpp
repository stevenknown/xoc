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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START Region
//
//This function erases all informations of ir and
//append it into free_list for next allocation.
//If Attach Info exist, this function will erase it rather than deletion.
//If DU info exist, this function will retrieve it back
//to region for next allocation.
//Note that this function does NOT free ir's kids and siblings.
void Region::freeIR(IR * ir) const
{
    ASSERTN(ir && !ir->is_undef(), ("ir has been freed"));
    ASSERTN(ir->is_single(), ("chain list should be cut off"));
    if (getDUMgr() != nullptr) {
        ir->freeDUset(getDUMgr());
    }
    AIContainer * res_ai = IR_ai(ir);
    if (res_ai != nullptr) {
        //AICont will be reinitialized till next setting.
        res_ai->destroy();
    }
    DU * du = ir->cleanDU();
    if (du != nullptr) {
        DU_md(du) = nullptr;
        DU_mds(du) = nullptr;
        getAnalysisInstrument()->m_free_du_list.append_head(du);
    }
    getIRMgr()->freeIR(ir);

    //Zero clearing all data fields, except the AIContainer pointer.
    IR_ai(ir) = res_ai;
}


//Free ir and all its kids, except its sibling node.
//We can only utilizing the function to free the
//IR which allocated by 'allocIR'.
void Region::freeIRTree(IR * ir, bool is_check_undef) const
{
    if (ir == nullptr) { return; }
    if (!is_check_undef && ir->is_undef()) { return; }
    ASSERTN(!ir->is_undef(), ("ir has been freed"));
    ASSERTN(ir->is_single(), ("chain list should be cut off"));
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            freeIRTreeList(kid, is_check_undef);
        }
    }
    freeIR(ir);
}


//Free ir, and all its kids.
//We can only utilizing the function to free the IR which allocated
//by 'allocIR'.
void Region::freeIRTreeList(IRList & irs, bool is_check_undef) const
{
    IRListIter next;
    IRListIter ct;
    for (irs.get_head(&ct); ct != irs.end(); ct = next) {
        IR * ir = ct->val();
        next = irs.get_next(ct);
        ASSERTN(ir->is_single(),
                ("do not allow sibling node, need to simplify"));
        irs.remove(ir);
        freeIRTree(ir, is_check_undef);
    }
}


//Free ir, ir's sibling, and all its kids.
//We can only utilizing the function to free the IR
//which allocated by 'allocIR'.
//NOTICE: If ir's sibling is not nullptr, that means the IR is
//a high level type. IRBB consists of only middle/low level IR.
void Region::freeIRTreeList(IR * ir, bool is_check_undef) const
{
    if (ir == nullptr) { return; }
    IR * head = ir, * next = nullptr;
    while (ir != nullptr) {
        next = ir->get_next();
        xcom::remove(&head, ir);
        freeIRTree(ir, is_check_undef);
        ir = next;
    }
}


//Duplication all contents of 'src', includes AttachInfo, except DU info,
//SSA info, kids and siblings IR.
IR * Region::dupIR(IR const* src) const
{
    if (src == nullptr) { return nullptr; }
    IR_CODE irc = src->getCode();
    IR * res = getIRMgr()->allocIR(irc);
    ASSERTN(res != nullptr && src != nullptr, ("res/src is nullptr"));
    UINT res_id = IR_id(res);
    AIContainer * res_ai = IR_ai(res);
    UINT res_irc_sz = IR::getIRCodeSize(res);
    ::memcpy((void*)res, src, IRCSIZE(irc));
    IR_id(res) = res_id;
    IR_ai(res) = res_ai;
    IR::setIRCodeSize(res, res_irc_sz);
    IR_next(res) = IR_prev(res) = IR_parent(res) = nullptr;
    res->cleanDU(); //Do not copy DU info.
    res->cleanSSAInfo(); //Do not copy SSA info.
    res->copyAI(src, this);
    if (res->isMemRef()) {
        res->copyRef(src, this);
    }
    return res;
}


IR * Region::dupIsomoExpTree(IR const* ir)
{
    switch (ir->getCode()) {
    case IR_STARRAY:
        return CArray::dupIRTreeByStmt(ir, this);
    case IR_IST:
        return CILd::dupIRTreeByStmt(ir, this);
    case IR_ST:
        return CLd::dupIRTreeByStmt(ir, this);
    case IR_STPR:
        return CPr::dupIRTreeByRef(ir, this);
    case IR_PHI:
        return CPhi::dupIRTreeByRef(ir, this);
    case IR_ARRAY:
    case IR_ILD:
    case IR_LD:
    SWITCH_CASE_READ_PR:
        return dupIRTree(ir);
    default: UNREACHABLE(); //unsupport.
    }
    return nullptr;
}


IR * Region::dupIsomoStmt(IR const* ir, IR * rhs)
{
    IR * stmt = nullptr;
    switch (ir->getCode()) {
    case IR_STARRAY: {
        //Prepare base and subscript expression list.
        IR * newbase = dupIRTree(ir->getBase());
        IR * newsublist = dupIRTreeList(ARR_sub_list(ir));
        stmt = getIRMgr()->buildStoreArray(
            newbase, newsublist, ir->getType(), ARR_elemtype(ir),
            ((CArray*)ir)->getDimNum(), ARR_elem_num_buf(ir), rhs);
        stmt->setOffset(ir->getOffset());
        stmt->copyRef(ir, this);
        return stmt;
    }
    case IR_IST: {
        IR * newbase = dupIRTree(ir->getBase());
        stmt = getIRMgr()->buildIStore(newbase, rhs, ir->getOffset(),
                                       ir->getType());
        stmt->copyRef(ir, this);
        return stmt;
    }
    case IR_ST:
        stmt = getIRMgr()->buildStore(ST_idinfo(ir), ir->getType(),
                                      ir->getOffset(), rhs);
        stmt->copyRef(ir, this);
        return stmt;
    case IR_ARRAY: {
        stmt = CStArray::dupIRTreeByExp(ir, rhs, this);
        //Stmt's ref already copied.
        return stmt;
    }
    case IR_ILD:
        stmt = CISt::dupIRTreeByExp(ir, rhs, this);
        //Stmt's ref already copied.
        return stmt;
    case IR_LD:
        stmt = CSt::dupIRTreeByExp(ir, rhs, this);
        //Stmt's ref already copied.
        return stmt;
    case IR_PR:
        stmt = CStpr::dupIRTreeByExp(ir, rhs, this);
        return stmt;
    default: UNREACHABLE(); //Unsupport.
    }
    return nullptr;
}


//Duplicate 'ir' and its kids, but without ir's sibiling node.
//The duplication includes AI, except DU info, SSA info.
IR * Region::dupIRTree(IR const* ir) const
{
    if (ir == nullptr) { return nullptr; }
    IR * newir = dupIR(ir);
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            IR * newkid_list = dupIRTreeList(kid);
            newir->setKid(i, newkid_list);
        } else { ASSERT0(newir->getKid(i) == nullptr); }
    }
    return newir;
}


//Duplication 'ir' and kids, and its sibling, return list of new ir.
//Duplicate irs start from 'ir' to the end of list.
//The duplication includes AI, except DU info, SSA info.
IR * Region::dupIRTreeList(IR const* ir) const
{
    IR * new_list = nullptr;
    while (ir != nullptr) {
        IR * newir = dupIRTree(ir);
        xcom::add_next(&new_list, newir);
        ir = ir->get_next();
    }
    return new_list;
}
//END Region

} //namespace xoc
