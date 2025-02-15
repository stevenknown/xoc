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
//START CLd
//
IR * CLd::dupIRTreeByStmt(IR const* src, Region const* rg)
{
    ASSERT0(src->is_st());
    IR * ld = rg->getIRMgr()->buildLoad(ST_idinfo(src), src->getOffset(),
                                        src->getType());
    ld->copyRef(src, rg);
    return ld;
}
//END CLd


//
//START CPr
//
IR * CPr::dupIRTreeByRef(IR const* src, Region const* rg)
{
    ASSERT0(src->isPROp());
    IR * pr = rg->getIRMgr()->buildPRdedicated(src->getPrno(), src->getType());
    pr->copyRef(src, rg);
    return pr;
}
//END CPr


//
//START CILd
//
IR * CILd::dupIRTreeByStmt(IR const* src, Region const* rg)
{
    ASSERT0(src->is_ist());
    IR * ild = rg->getIRMgr()->buildILoad(
        rg->dupIRTree(IST_base(src)), IST_ofst(src), src->getType());
    ild->copyRef(src, rg);
    ILD_base(ild)->copyRefForTree(IST_base(src), rg);
    return ild;
}
//END CILd


//
//START CArray
//
IR * CArray::dupIRTreeByStmt(IR const* src, Region const* rg)
{
    ASSERT0(src->is_starray());
    IR * arr = rg->getIRMgr()->buildArray(
        rg->dupIRTree(ARR_base(src)), rg->dupIRTreeList(ARR_sub_list(src)),
        src->getType(), ARR_elemtype(src), ((CStArray*)src)->getDimNum(),
        ARR_elem_num_buf(src));
    arr->setOffset(src->getOffset());
    arr->copyRef(src, rg);
    ARR_base(arr)->copyRefForTree(ARR_base(src), rg);
    ARR_sub_list(arr)->copyRefForTree(ARR_sub_list(src), rg);
    return arr;
}


bool CArray::isIsomoArrayStructTo(IR const* src) const
{
    ASSERT0(isArrayOp() && src->isArrayOp());
    ASSERT0(getBase() && src->getBase());
    if (ARR_elem_num_buf(this) == nullptr ||
        ARR_elem_num_buf(src) == nullptr) {
        //We have no knowledge about array dimensions.
        return false;
    }
    TMWORD cur_dimnum = getDimNum();
    TMWORD src_dimnum = ((CArray*)src)->getDimNum();
    if (cur_dimnum != src_dimnum) { return false; }
    for (UINT i = 0; i < cur_dimnum; i++) {
        if (getElementNumOfDim(i) != ((CArray*)src)->getElementNumOfDim(i)) {
            return false;
        }
    }
    //There is no need to check each subscript expressions.
    return true;
}
//END CArray


//
//START CStArray
//
IR * CStArray::dupIRTreeByExp(IR const* src, IR * rhs, Region const* rg)
{
    ASSERT0(src->is_array());
    IR * starr = rg->getIRMgr()->buildStoreArray(
        rg->dupIRTree(ARR_base(src)), rg->dupIRTreeList(ARR_sub_list(src)),
        src->getType(), ARR_elemtype(src), ((CStArray*)src)->getDimNum(),
        ARR_elem_num_buf(src), rhs);
    starr->setOffset(src->getOffset());
    starr->copyRef(src, rg);
    ARR_base(starr)->copyRefForTree(ARR_base(src), rg);
    ARR_sub_list(starr)->copyRefForTree(ARR_sub_list(src), rg);
    return starr;
}
//END CStArray


//
//START CISt
//
IR * CISt::dupIRTreeByExp(IR const* src, IR * rhs, Region const* rg)
{
    ASSERT0(src->is_ild());
    IR * ist = rg->getIRMgr()->buildIStore(
        rg->dupIRTree(ILD_base(src)), rhs, src->getOffset(), src->getType());
    ist->copyRef(src, rg);
    IST_base(ist)->copyRefForTree(ILD_base(src), rg);
    return ist;
}
//END CISt


//
//START CSt
//
IR * CSt::dupIRTreeByExp(IR const* src, IR * rhs, Region const* rg)
{
    ASSERT0(src->is_ld());
    IR * st = rg->getIRMgr()->buildStore(
        LD_idinfo(src), src->getType(), src->getOffset(), rhs);
    st->copyRef(src, rg);
    return st;
}
//END CSt


//
//START CStpr
//
IR * CStpr::dupIRTreeByExp(IR const* src, IR * rhs, Region const* rg)
{
    ASSERT0(src->is_pr());
    IR * stpr = rg->getIRMgr()->buildStorePR(PR_no(src), src->getType(), rhs);
    stpr->copyRef(src, rg);
    return stpr;
}
//END CStpr


//The function only invoked at debug mode.
//Make sure LARGEST_IR_CODE is the largest ir.
bool checkMaxIRCode()
{
    //Change MAX_OFFSET_AT_FREE_TABLE if LARGEST_IR_CODE is not the largest.
    for (UINT i = IR_UNDEF; i < IR_CODE_NUM; i++) {
        ASSERT0(IRCSIZE(i) <= IRCSIZE(LARGEST_IR_CODE));
    }
    return true;
}


//The function only invoked at debug mode.
//Make sure IR_ICALL is the largest ir.
bool checkIRCodeBitSize()
{
    return IR_CODE_NUM <= 1 << IR_CODE_BIT_SIZE;
}


//
//START CIf
//
void CIf::addToTrueBody(UINT num, ...)
{
    va_list ptr;
    va_start(ptr, num);
    IR * ir = (IR*)va_arg(ptr, IR*);
    IR * last = nullptr;
    while (num > 0) {
        xcom::add_next(&IF_truebody(this), &last, ir);
        ir = (IR*)va_arg(ptr, IR*);
        ASSERT0(!ir->is_undef());
        num--;
    }
    va_end(ptr);
}


void CIf::addToFalseBody(UINT num, ...)
{
    va_list ptr;
    va_start(ptr, num);
    IR * ir = (IR*)va_arg(ptr, IR*);
    IR * last = nullptr;
    while (num > 0) {
        xcom::add_next(&IF_falsebody(this), &last, ir);
        ir = (IR*)va_arg(ptr, IR*);
        ASSERT0(!ir->is_undef());
        num--;
    }
    va_end(ptr);
}
//END CIf


//
//START CWhileDo
//
void CWhileDo::addToBody(UINT num, ...)
{
    va_list ptr;
    va_start(ptr, num);
    IR * ir = (IR*)va_arg(ptr, IR*);
    IR * last = nullptr;
    while (num > 0) {
        xcom::add_next(&LOOP_body(this), &last, ir);
        ir = (IR*)va_arg(ptr, IR*);
        ASSERT0(!ir->is_undef());
        num--;
    }
    va_end(ptr);
}
//END CWhileDo


//
//START CSwitch
//
void CSwitch::addToBody(UINT num, ...)
{
    va_list ptr;
    va_start(ptr, num);
    IR * ir = (IR*)va_arg(ptr, IR*);
    IR * last = nullptr;
    while (num > 0) {
        xcom::add_next(&SWITCH_body(this), &last, ir);
        ir = (IR*)va_arg(ptr, IR*);
        ASSERT0(!ir->is_undef());
        num--;
    }
    va_end(ptr);
}


//The function collects the LabelInfo for each branch-target.
//Note the default-label is collected too.
void CSwitch::collectLabel(OUT List<LabelInfo const*> & lst) const
{
    lst.append_tail(getDefLab());
    for (IR const* cs = getCaseList(); cs != nullptr; cs = cs->get_next()) {
        lst.append_tail(CASE_lab(cs));
    }
}
//END CSwitch


//
//START CIGoto
//
//The function collects the LabelInfo for each branch-target.
void CIGoto::collectLabel(OUT List<LabelInfo const*> & lst) const
{
    for (IR const* cs = getCaseList(); cs != nullptr; cs = cs->get_next()) {
        lst.append_tail(CASE_lab(cs));
    }
}
//END CIGoto


//
//START CCall
//
//Build dummyuse expression to represent potential memory objects that
//the Call referrenced.
//Note dummyuse may be a list of IR.
void CCall::addDummyUse(Region const* rg)
{
    IR * newdummyuse = rg->getIRMgr()->buildILoad(
        rg->getIRMgr()->buildImmAny(0), rg->getTypeMgr()->getAny());
    IR_parent(newdummyuse) = this;
    xcom::insertbefore(&CALL_dummyuse(this), CALL_dummyuse(this), newdummyuse);
}
//END CCall


//
//START CPhi
//
IR * CPhi::getOpnd(UINT idx) const
{
    UINT i = 0;
    IR * x = PHI_opnd_list(this);
    for (; x != nullptr && i < idx; x = x->get_next(), i++) {;}
    return x;
}


IR * CPhi::dupIRTreeByRef(IR const* src, Region const* rg)
{
    ASSERT0(src->isPROp());
    //Type cast if host compiler does NOT support C11.
    IR * phi = rg->getIRMgr()->buildPhi(
        src->getPrno(), src->getType(), (IR*)nullptr);
    phi->copyRef(src, rg);
    return phi;
}


void CPhi::insertOpndAt(UINT pos, IR * exp)
{
    IR * marker = PHI_opnd_list(this);
    IR * last = nullptr;
    UINT i = 0;
    for (; marker != nullptr && i <= pos; marker = marker->get_next(), i++) {
        last = marker;
    }
    if (marker != nullptr) {
        //Insert phi operand into operand-list before 'marker'.
        insertOpndBefore(marker, exp);
        return;
    }

    //Append a new phi operand to tail of operand-list.
    ASSERT0(i == pos);
    //'last' may be nullptr, because the operand list may be empty before
    //insertion. During several CFG edge removing and building,
    //there may appear single operand PHI.
    //If CFG optimization perform removing single edge then
    //adding a new edge, the PHI operand is empty when adding the new edge.
    xcom::add_next(&PHI_opnd_list(this), &last, exp);
    setParent(exp);
}
//END CPhi


//
//START CRegion
//
MDSet * CRegion::getMayDef() const
{
    return REGION_ru(this)->getMayDef();
}


bool CRegion::is_readonly() const
{
    ASSERT0(rg);
    return rg->is_readonly();
}


MDSet * CRegion::getMayUse() const
{
    return REGION_ru(this)->getMayUse();
}


MDSet * CRegion::genMayDef() const
{
    return REGION_ru(this)->genMayDef();
}


MDSet * CRegion::genMayUse() const
{
    return REGION_ru(this)->genMayUse();
}
//END CRegion

} //namespace xoc
