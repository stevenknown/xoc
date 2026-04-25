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

IRFieldAccTab::AccInfo CId::accinfo[CId::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_IDINFO, (void*)CId::accIdinfo),
};


//
//START CSetElem
//
static BYTE g_csetelem_vbuf[4] = {0x6};
IRKidMap const CSetElem::kid_map(g_csetelem_vbuf, sizeof(g_csetelem_vbuf));


IRFieldAccTab::AccInfo CSetElem::accinfo[CSetElem::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_SSAINFO, (void*)CSetElem::accSSAInfo),
    IRFieldAccTab::AccInfo(IR_ACC_PRNO, (void*)CSetElem::accPrno),
    IRFieldAccTab::AccInfo(IR_ACC_RESPR, (void*)CSetElem::accResultPR),
    IRFieldAccTab::AccInfo(IR_ACC_BASE, (void*)CSetElem::accBase),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CSetElem::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CSetElem::accBB),
};
//END CSetElem


IRFieldAccTab::AccInfo CGetElem::accinfo[CGetElem::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_SSAINFO, (void*)CGetElem::accSSAInfo),
    IRFieldAccTab::AccInfo(IR_ACC_PRNO, (void*)CGetElem::accPrno),
    IRFieldAccTab::AccInfo(IR_ACC_BASE, (void*)CGetElem::accBase),
    IRFieldAccTab::AccInfo(IR_ACC_RESPR, (void*)CGetElem::accResultPR),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CGetElem::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CGetElem::accBB),
};


//
//START CLd
//
IR * CLd::dupIRTreeByStmt(IR const* src, Region const* rg)
{
    ASSERT0(src->is_st());
    IR * ld = rg->getIRMgr()->buildLoad(
        ST_idinfo(src), src->getOffset(), src->getType());
    ld->copyRef(src, rg);
    return ld;
}


IRFieldAccTab::AccInfo CLd::accinfo[CLd::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_IDINFO, (void*)CLd::accIdinfo),
    IRFieldAccTab::AccInfo(IR_ACC_OFST, (void*)CLd::accOfst),
    IRFieldAccTab::AccInfo(IR_ACC_SS, (void*)CLd::accSS),
    IRFieldAccTab::AccInfo(IR_ACC_VOLATILE, (void*)CLd::accVolatile),
    IRFieldAccTab::AccInfo(IR_ACC_ALIGN, (void*)CLd::accAlign),
};
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


IRFieldAccTab::AccInfo CPr::accinfo[CPr::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_SSAINFO, (void*)CPr::accSSAInfo),
    IRFieldAccTab::AccInfo(IR_ACC_PRNO, (void*)CPr::accPrno),
};
//END CPr


IRFieldAccTab::AccInfo CTruebr::accinfo[CTruebr::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CTruebr::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CTruebr::accBB),
    IRFieldAccTab::AccInfo(IR_ACC_LABEL, (void*)CTruebr::accLab),
    IRFieldAccTab::AccInfo(IR_ACC_DET, (void*)CTruebr::accDet),
};


IRFieldAccTab::AccInfo CFalsebr::accinfo[CFalsebr::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CFalsebr::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CFalsebr::accBB),
    IRFieldAccTab::AccInfo(IR_ACC_LABEL, (void*)CFalsebr::accLab),
    IRFieldAccTab::AccInfo(IR_ACC_DET, (void*)CFalsebr::accDet),
};


IRFieldAccTab::AccInfo CRet::accinfo[CRet::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CRet::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CRet::accBB),
};


IRFieldAccTab::AccInfo CSelect::accinfo[CSelect::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CSelect::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_DET, (void*)CSelect::accDet),
};


IRFieldAccTab::AccInfo CAlloca::accinfo[CAlloca::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CAlloca::accKid),
};


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


IRFieldAccTab::AccInfo CILd::accinfo[CILd::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_OFST, (void*)CILd::accOfst),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CILd::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BASE, (void*)CILd::accBase),
    IRFieldAccTab::AccInfo(IR_ACC_SS, (void*)CILd::accSS),
    IRFieldAccTab::AccInfo(IR_ACC_VOLATILE, (void*)CILd::accVolatile),
    IRFieldAccTab::AccInfo(IR_ACC_ALIGN, (void*)CILd::accAlign),
};
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


IRFieldAccTab::AccInfo CStArray::accinfo[CStArray::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_RHS, (void*)CStArray::accRHS),
    IRFieldAccTab::AccInfo(IR_ACC_OFST, (void*)CStArray::accOfst),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CStArray::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CStArray::accBB),
    IRFieldAccTab::AccInfo(IR_ACC_BASE, (void*)CStArray::accBase),
    IRFieldAccTab::AccInfo(IR_ACC_SS, (void*)CStArray::accSS),
    IRFieldAccTab::AccInfo(IR_ACC_VOLATILE, (void*)CStArray::accVolatile),
    IRFieldAccTab::AccInfo(IR_ACC_ALIGN, (void*)CStArray::accAlign),
};


IRFieldAccTab::AccInfo CCvt::accinfo[CCvt::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CCvt::accKid),
};



//END CStArray


//
//START CISt
//
static BYTE g_cist_vbuf[2] = {0x3};
IRKidMap const CISt::kid_map(g_cist_vbuf, sizeof(g_cist_vbuf));

IR * CISt::dupIRTreeByExp(IR const* src, IR * rhs, Region const* rg)
{
    ASSERT0(src->is_ild());
    IR * ist = rg->getIRMgr()->buildIStore(
        rg->dupIRTree(ILD_base(src)), rhs, src->getOffset(), src->getType());
    ist->copyRef(src, rg);
    IST_base(ist)->copyRefForTree(ILD_base(src), rg);
    return ist;
}


IRFieldAccTab::AccInfo CISt::accinfo[CISt::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_RHS, (void*)CISt::accRHS),
    IRFieldAccTab::AccInfo(IR_ACC_OFST, (void*)CISt::accOfst),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CISt::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CISt::accBB),
    IRFieldAccTab::AccInfo(IR_ACC_BASE, (void*)CISt::accBase),
    IRFieldAccTab::AccInfo(IR_ACC_SS, (void*)CISt::accSS),
    IRFieldAccTab::AccInfo(IR_ACC_VOLATILE, (void*)CISt::accVolatile),
    IRFieldAccTab::AccInfo(IR_ACC_ALIGN, (void*)CISt::accAlign),
};
//END CISt


IRFieldAccTab::AccInfo CLda::accinfo[CLda::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_IDINFO, (void*)CLda::accIdinfo),
    IRFieldAccTab::AccInfo(IR_ACC_OFST, (void*)CLda::accOfst),
    IRFieldAccTab::AccInfo(IR_ACC_SS, (void*)CLda::accSS),
};


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


IRFieldAccTab::AccInfo CSt::accinfo[CSt::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_RHS, (void*)CSt::accRHS),
    IRFieldAccTab::AccInfo(IR_ACC_IDINFO, (void*)CSt::accIdinfo),
    IRFieldAccTab::AccInfo(IR_ACC_OFST, (void*)CSt::accOfst),
    IRFieldAccTab::AccInfo(IR_ACC_SS, (void*)CSt::accSS),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CSt::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CSt::accBB),
    IRFieldAccTab::AccInfo(IR_ACC_VOLATILE, (void*)CSt::accVolatile),
    IRFieldAccTab::AccInfo(IR_ACC_ALIGN, (void*)CSt::accAlign),
};
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


IRFieldAccTab::AccInfo CStpr::accinfo[CStpr::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_RHS, (void*)CStpr::accRHS),
    IRFieldAccTab::AccInfo(IR_ACC_SSAINFO, (void*)CStpr::accSSAInfo),
    IRFieldAccTab::AccInfo(IR_ACC_PRNO, (void*)CStpr::accPrno),
    IRFieldAccTab::AccInfo(IR_ACC_RESPR, (void*)CStpr::accResultPR),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CStpr::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CStpr::accBB),
};
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


IRFieldAccTab::AccInfo CIf::accinfo[CIf::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CIf::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_DET, (void*)CIf::accDet),
};
//END CIf


IRFieldAccTab::AccInfo CLab::accinfo[CLab::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_LABEL, (void*)CLab::accLab),
};


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


IRFieldAccTab::AccInfo CWhileDo::accinfo[CWhileDo::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CWhileDo::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_DET, (void*)CWhileDo::accDet),
};
//END CWhileDo


IRFieldAccTab::AccInfo CDoWhile::accinfo[CDoWhile::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CDoWhile::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_DET, (void*)CDoWhile::accDet),
};


IRFieldAccTab::AccInfo CDoLoop::accinfo[CDoLoop::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CDoLoop::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_DET, (void*)CDoLoop::accDet),
};


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


IRFieldAccTab::AccInfo CSwitch::accinfo[CSwitch::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CSwitch::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CSwitch::accBB),
    IRFieldAccTab::AccInfo(IR_ACC_LABEL, (void*)CSwitch::accLab),
    IRFieldAccTab::AccInfo(IR_ACC_COLLECT_LAB, (void*)CSwitch::accCollectLab),
    IRFieldAccTab::AccInfo(IR_ACC_VALEXP, (void*)CSwitch::accValExp),
    IRFieldAccTab::AccInfo(IR_ACC_CASE, (void*)CSwitch::accCase),
};
//END CSwitch


IRFieldAccTab::AccInfo CDummyUse::accinfo[CDummyUse::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CDummyUse::accKid),
};


IRFieldAccTab::AccInfo CCase::accinfo[CCase::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CCase::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_LABEL, (void*)CCase::accLab),
};


IRFieldAccTab::AccInfo CArray::accinfo[CArray::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_OFST, (void*)CArray::accOfst),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CArray::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BASE, (void*)CArray::accBase),
    IRFieldAccTab::AccInfo(IR_ACC_SS, (void*)CArray::accSS),
    IRFieldAccTab::AccInfo(IR_ACC_VOLATILE, (void*)CArray::accVolatile),
    IRFieldAccTab::AccInfo(IR_ACC_ALIGN, (void*)CArray::accAlign),
};


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


IRFieldAccTab::AccInfo CIGoto::accinfo[CIGoto::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CIGoto::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CIGoto::accBB),
    IRFieldAccTab::AccInfo(IR_ACC_COLLECT_LAB, (void*)CIGoto::accCollectLab),
    IRFieldAccTab::AccInfo(IR_ACC_VALEXP, (void*)CIGoto::accValExp),
    IRFieldAccTab::AccInfo(IR_ACC_CASE, (void*)CIGoto::accCase),
};
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


class VFCompareEQ {
public:
    bool find;
    IR const* compared_ir;
public:
    VFCompareEQ() { find = false; }
    bool visitIR(IR const* ir, OUT bool & is_terminate)
    {
        if (ir == compared_ir) {
            //Visiting will terminated immedately.
            find = true;
            is_terminate = true;
            return false;
        }
        //Keep visiting the kid and sibling.
        return true;
    }
};

bool CCall::isDummyUse(IR const* exp) const
{
    class IterTree : public VisitIRTree<VFCompareEQ> {
    public:
        IterTree(VFCompareEQ & vf) : VisitIRTree(vf) {}
    };
    ASSERT0(exp && exp->is_exp());
    VFCompareEQ vf;
    vf.compared_ir = exp;
    IterTree it(vf);
    it.visit(getDummyUse());
    return vf.find;
}


IRFieldAccTab::AccInfo CCall::accinfo[CCall::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_IDINFO, (void*)CCall::accIdinfo),
    IRFieldAccTab::AccInfo(IR_ACC_SSAINFO, (void*)CCall::accSSAInfo),
    IRFieldAccTab::AccInfo(IR_ACC_PRNO, (void*)CCall::accPrno),
    IRFieldAccTab::AccInfo(IR_ACC_SS, (void*)CCall::accSS),
    IRFieldAccTab::AccInfo(IR_ACC_RESPR, (void*)CCall::accResultPR),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CCall::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CCall::accBB),
};
//END CCall


IRFieldAccTab::AccInfo CICall::accinfo[CICall::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_SSAINFO, (void*)CICall::accSSAInfo),
    IRFieldAccTab::AccInfo(IR_ACC_PRNO, (void*)CICall::accPrno),
    IRFieldAccTab::AccInfo(IR_ACC_RESPR, (void*)CICall::accResultPR),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CICall::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CICall::accBB),
};


IRFieldAccTab::AccInfo CTer::accinfo[CTer::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CTer::accKid),
};


IRFieldAccTab::AccInfo CBin::accinfo[CBin::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CBin::accKid),
};


IRFieldAccTab::AccInfo CUna::accinfo[CUna::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CUna::accKid),
};


IRFieldAccTab::AccInfo CGoto::accinfo[CGoto::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CGoto::accBB),
    IRFieldAccTab::AccInfo(IR_ACC_LABEL, (void*)CGoto::accLab),
};


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


IR * CPhi::getOpndByPred(IRBB const* pred, IRCFG const* cfg) const
{
    ASSERT0(pred && cfg);
    ASSERT0(cfg->isVertex(pred->id()));
    ASSERT0(getBB());
    bool is_pred = false;
    UINT pred_pos = cfg->WhichPred(pred, getBB(), is_pred);
    ASSERT0(is_pred);
    return getOpnd(pred_pos);
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


IRFieldAccTab::AccInfo CPhi::accinfo[CPhi::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_SSAINFO, (void*)CPhi::accSSAInfo),
    IRFieldAccTab::AccInfo(IR_ACC_PRNO, (void*)CPhi::accPrno),
    IRFieldAccTab::AccInfo(IR_ACC_RESPR, (void*)CPhi::accResultPR),
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CPhi::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CPhi::accBB),
};
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


IRFieldAccTab::AccInfo CRegion::accinfo[CRegion::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CRegion::accBB),
};
//END CRegion


IRFieldAccTab::AccInfo CCFIDefCfa::accinfo[CCFIDefCfa::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CCFIDefCfa::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CCFIDefCfa::accBB),
};


IRFieldAccTab::AccInfo CCFISameValue::accinfo[CCFISameValue::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CCFISameValue::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CCFISameValue::accBB),
};


IRFieldAccTab::AccInfo CCFIOffset::accinfo[CCFIOffset::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CCFIOffset::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CCFIOffset::accBB),
};


IRFieldAccTab::AccInfo CCFIRestore::accinfo[CCFIRestore::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CCFIRestore::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CCFIRestore::accBB),
};


IRFieldAccTab::AccInfo CCFIDefCfaOffset::accinfo
    [CCFIDefCfaOffset::accinfo_num] = {
    IRFieldAccTab::AccInfo(IR_ACC_KID, (void*)CCFIDefCfaOffset::accKid),
    IRFieldAccTab::AccInfo(IR_ACC_BB, (void*)CCFIDefCfaOffset::accBB),
};

} //namespace xoc
