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
#include "prssainfo.h"
#include "ir_ssa.h"

namespace xoc {

#ifdef _DEBUG_
bool allBeStmt(IR * irlst)
{
    for (IR * ir = irlst; ir != nullptr; ir = ir->get_next()) {
        ASSERT0(ir->is_stmt());
    }
    return true;
}


bool allBeExp(IR * irlst)
{
    for (IR * ir = irlst; ir != nullptr; ir = ir->get_next()) {
        ASSERT0(ir->is_exp());
    }
    return true;
}


INT checkKidNumValid(IR const* ir, UINT n, CHAR const* filename, INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    return n;
}


INT checkKidNumValidUnary(IR const* ir, UINT n, CHAR const* filename,
                          INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->isUnaryOp());
    return n;
}


INT checkKidNumValidBinary(IR const* ir, UINT n, CHAR const* filename,
                           INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->isBinaryOp());
    return n;
}


INT checkKidNumValidBranch(IR const* ir, UINT n, CHAR const* filename,
                           INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->is_truebr() || ir->is_falsebr());
    return n;
}


INT checkKidNumValidLoop(IR const* ir, UINT n, CHAR const* filename, INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->is_whiledo() || ir->is_dowhile() || ir->is_doloop());
    return n;
}


INT checkKidNumValidCall(IR const* ir, UINT n, CHAR const* filename, INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->isCallStmt());
    return n;
}


INT checkKidNumValidArray(IR const* ir, UINT n, CHAR const* filename, INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->isArrayOp());
    return n;
}


INT checkKidNumIRCode(IR const* ir, UINT n, IR_CODE irc,
                      CHAR const* filename, INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->getCode() == irc);
    return n;
}


IR * checkIRC(IR * ir, IR_CODE irc)
{
    ASSERTN(ir->getCode() == irc, ("current ir is not '%s'", IRCNAME(irc)));
    return ir;
}


IR * checkIRCBranch(IR * ir)
{
    ASSERT0(ir->isConditionalBr());
    return ir;
}


IR * checkIRCCall(IR * ir)
{
    ASSERT0(ir->isCallStmt());
    return ir;
}


IR * checkIRCArray(IR * ir)
{
    ASSERT0(ir->is_array() || ir->is_starray());
    return ir;
}


IR * checkIRCOnlyCall(IR * ir)
{
    ASSERT0(ir->is_call());
    return ir;
}


IR * checkIRCOnlyICall(IR * ir)
{
    ASSERT0(ir->is_icall());
    return ir;
}


UINT checkArrayDimension(IR const* ir, UINT n)
{
    UINT i = 0;
    for (IR const* sub = ARR_sub_list(ir);
         sub != nullptr; sub = sub->get_next()) {
        i++;
    }
    ASSERT0(n < i);
    return n;
}


UINT checkStArrayDimension(IR const* ir, UINT n)
{
    UINT i = 0;
    for (IR const* sub = ARR_sub_list(ir);
         sub != nullptr; sub = sub->get_next()) {
        i++;
    }
    ASSERT0(n < i);
    return n;
}
#endif

//
//START IRDesc
//
bool IRDesc::mustExist(IR_CODE irc, UINT kididx)
{
    return HAVE_FLAG(IRDES_kid_map(g_ir_desc[irc]), 1 << kididx);
}
//END IRDesc


//The function clean the IR_parent for each elements in 'irlst'.
void cleanParentForIRList(IR * irlst)
{
    for (IR * t = irlst; t != nullptr; t = t->get_next()) {
        IR_parent(t) = nullptr;
    }
}


void setParentPointerForIRList(IR * ir_list)
{
    while (ir_list != nullptr) {
        ir_list->setParentPointer(true);
        ir_list = IR_next(ir_list);
    }
}


//
//START IR
//
size_t IR::count_mem() const
{
    size_t size = 0;
    ConstIRIter it;
    for (IR const* k = iterInitC(this, it);
         k != nullptr; k = iterNextC(it)) {
        size += IRCSIZE(k->getCode());
    }
    return size;
}


//Check that IR cannot take a UNDEF type.
bool IR::verify(Region const* rg) const
{
    IRVerifyFuncType verifyfunc = IRDES_verifyfunc(g_ir_desc[getCode()]);
    ASSERT0(verifyfunc);
    (*verifyfunc)(this, rg);
    return true;
}


//Calculate the accumulated offset value from the base of array.
//e.g: For given array long long p[10][20],
//the offset of p[i][j] can be computed by i*20 + j, and
//the offset of p[i] can be computed by i*20.
//If all the indice are constant value, calcuate the value, storing
//in 'ofst_val' and return True, otherwise return False that means the
//Offset can not be predicated.
bool IR::calcArrayOffset(TMWORD * ofst_val, TypeMgr * tm) const
{
    if (!isArrayOp() || ARR_elem_num_buf(this) == nullptr) { return false; }
    TMWORD aggr = 0;
    UINT dim = 0;
    for (IR const* s = ARR_sub_list(this); s != nullptr;
         s = s->get_next(), dim++) {
        if (!s->is_const()) { return false; }

        ASSERT0(!s->is_fp());
        if (CONST_int_val(s) < 0) { return false; }

        #ifdef _VC2010_
        #define MASK_32BIT 0xFFFFffff00000000lu
        #else
        #define MASK_32BIT 0xFFFFffff00000000llu
        #endif
        ASSERTN((((ULONGLONG)CONST_int_val(s)) &
                (ULONGLONG)(LONGLONG)MASK_32BIT) == 0,
                ("allow 32bit array offset."));

        ASSERT0(dim < ((CArray*)this)->getDimNum());
        //Array index start at 0.
        if (dim == 0) {
            //The dimension of dim0 may be zero.
            //e.g: struct { char di[]; } a; ... = a.di[0];
            aggr = (TMWORD)CONST_int_val(s);
        } else {
            ASSERT0(ARR_elem_num(this, dim) != 0);
            aggr += ARR_elem_num(this, dim - 1) * (TMWORD)CONST_int_val(s);
        }
    }

    aggr *= tm->getByteSize(ARR_elemtype(this));
    ASSERT0(ofst_val);
    *ofst_val = aggr;
    return true;
}


//Return true if ir-list are equivalent.
//is_cmp_kid: it is true if comparing kids as well.
bool IR::isIRListEqual(IR const* irs, bool is_cmp_kid) const
{
    if (this == irs) { return true; }
    IR const* pthis = this;
    while (irs != nullptr && pthis != nullptr) {
        if (!pthis->isIREqual(irs, is_cmp_kid)) {
            return false;
        }
        irs = irs->get_next();
        pthis = IR_next(pthis);
    }
    if ((irs != nullptr) ^ (pthis != nullptr)) {
        return false;
    }
    return true;
}


//Return true if given array has same dimension structure with current ir.
bool IR::isSameArrayStruct(IR const* ir) const
{
    ASSERT0(isArrayOp() && ir->isArrayOp());
    if (ARR_elemtype(this) != ARR_elemtype(ir) ||
        ARR_ofst(this) != ARR_ofst(ir) ||
        ((CArray*)this)->getDimNum() != ((CArray*)ir)->getDimNum()) {
        return false;
    }
    if ((ARR_elem_num_buf(this) == nullptr) ||
        (ARR_elem_num_buf(ir) == nullptr)) {
        //Array is any dimension.
        return false;
    }
    UINT dim = 0;
    for (IR const* s = ARR_sub_list(this); s != nullptr; s = s->get_next()) {
        if (((CArray*)this)->getElementNumOfDim(dim) !=
            ((CArray*)ir)->getElementNumOfDim(dim)) {
            return false;
        }
        dim++;
    }
    return true;
}


//Return true if IR tree is exactly congruent, or
//they are parity memory reference.
bool IR::isMemRefEqual(IR const* src) const
{
    ASSERTN(isMemRef() && src->isMemRef(), ("Not memory expression"));
    if (isIREqual(src, true)) { return true; }
    switch (getCode()) {
    SWITCH_CASE_DIRECT_MEM_OP:
        if (src->isDirectMemOp() &&
            getIdinfo() == src->getIdinfo() &&
            getOffset() == src->getOffset() &&
            getType() == src->getType()) {
            return true;
        }
        return false;
    SWITCH_CASE_INDIRECT_MEM_OP:
        if (src->isIndirectMemOp() &&
            getBase()->isIREqual(src->getBase(), true) &&
            getOffset() == src->getOffset() &&
            getType() == src->getType()) {
            return true;
        }
        return false;
    SWITCH_CASE_ARRAY_OP:
        if (src->isArrayOp() &&
            ARR_base(src)->isIREqual(ARR_base(this), true) &&
            ARR_ofst(src) == ARR_ofst(this) &&
            ARR_elemtype(src) == ARR_elemtype(this) &&
            getType() == src->getType()) {
            if ((ARR_sub_list(src) != nullptr) ^
                (ARR_sub_list(this) != nullptr)) {
                return false;
            }
            if (ARR_sub_list(src) != nullptr &&
                !ARR_sub_list(src)->isIRListEqual(ARR_sub_list(this))) {
                return false;
            }
            if ((ARR_elem_num_buf(src) != nullptr) ^
                (ARR_elem_num_buf(this) != nullptr)) {
                return false;
            }
            if (ARR_elem_num_buf(src) != nullptr) {
                ASSERT0(ARR_elem_num_buf(this));
                TMWORD dimnum = ((CArray*)this)->getDimNum();
                ASSERT0(((CArray*)src)->getDimNum() == dimnum);
                for (UINT i = 0; i < dimnum; i++) {
                    if (((CArray*)this)->getElementNumOfDim(i) !=
                        ((CArray*)src)->getElementNumOfDim(i)) {
                        return false;
                    }
                }
            }
            return true;
        }
        return false;
    SWITCH_CASE_MAY_PR_OP:
        if (src->isPROp()) {
            return getPrno() == src->getPrno();
        }
        return false;
    default: UNREACHABLE();
    }
    return false;
}


static bool isIRIsomorphic(IR const* ir, IR const* src,
                           bool is_cmp_kid, IsomoFlag const& flag);

static bool isIRListIsomorphic(IR const* ir1lst, IR const* ir2lst,
                               bool is_cmp_kid, IsomoFlag const& flag)
{
    IR const* ir1 = ir1lst;
    IR const* ir2 = ir2lst;
    for (; ir1 != nullptr && ir2 != nullptr;
         ir1 = ir1->get_next(), ir2 = ir2->get_next()) {
        if (!isIRIsomorphic(ir1, ir2, is_cmp_kid, flag)) {
            return false;
        }
    }
    if ((ir1 != nullptr) ^ (ir2 != nullptr)) {
        return false;
    }
    return true;
}


static bool isArrayIsomorphic(IR const* ir, IR const* src,
                              bool is_cmp_kid, IsomoFlag const& flag)
{
    if (!ir->isArrayOp()) { return false; }
    if (flag.have(ISOMO_CK_CODE) && ir->getCode() != src->getCode()) {
        //Not diff exp or stmt.
        return false;
    }
    if (flag.have(ISOMO_CK_TYPE) && ir->getType() != src->getType()) {
        return false;
    }
    if (ir->getOffset() != src->getOffset()) {
        return false;
    }
    if (ARR_elem_num_buf(src) == nullptr || ARR_elem_num_buf(ir) == nullptr) {
        //We have no knowledge about the array.
        return false;
    }
    TMWORD dimnum = ((CArray*)ir)->getDimNum();
    if (((CArray*)src)->getDimNum() != dimnum) { return false; }
    for (UINT i = 0; i < dimnum; i++) {
        if (((CArray*)ir)->getElementNumOfDim(i) !=
            ((CArray*)src)->getElementNumOfDim(i)) {
            return false;
        }
    }
    IR const* cursub = ARR_sub_list(ir);
    IR const* srcsub = ARR_sub_list(src);
    for (; cursub != nullptr; cursub = cursub->get_next(),
         srcsub = srcsub->get_next()) {
        ASSERT0(srcsub);
        if (!isIRIsomorphic(cursub, srcsub, is_cmp_kid, flag)) {
            return false;
        }
    }
    ASSERT0(ir->getBase() && src->getBase());
    return isIRIsomorphic(ir->getBase(), src->getBase(), is_cmp_kid, flag);
}


//flag: record the checking condition while compare two given ir expression
//      or stmt.
//      e.g: If ISOMO_CK_CODE is set, the comparison of IST and ILD will
//      return false.
static bool isIRIsomorphic(IR const* ir, IR const* src,
                           bool is_cmp_kid, IsomoFlag const& flag)
{
    if (ir == src) { return true; }
    switch (src->getCode()) {
    case IR_CONST: //Constant value: include integer, float, string.
        if (ir->getCode() != src->getCode()) { return false; }
        if (flag.have(ISOMO_CK_CONST_VAL) &&
            CONST_int_val(ir) != CONST_int_val(src)) {
            return false;
        }
        break;
    case IR_ID:
        if (ir->getCode() != src->getCode()) { return false; }
        if (ID_info(ir) != ID_info(src)) { return false; }
        break;
    SWITCH_CASE_DIRECT_MEM_OP:
        if (!ir->isDirectMemOp()) { return false; }
        if (flag.have(ISOMO_CK_CODE) && ir->getCode() != src->getCode()) {
            return false;
        }
        if (flag.have(ISOMO_CK_TYPE) && ir->getType() != src->getType()) {
            return false;
        }
        if (flag.have(ISOMO_CK_IDINFO) &&
            ir->getIdinfo() != src->getIdinfo()) {
            return false;
        }
        if (ir->getOffset() != src->getOffset()) {
            return false;
        }
        break;
    SWITCH_CASE_INDIRECT_MEM_OP:
        if (!ir->isIndirectMemOp()) { return false; }
        if (flag.have(ISOMO_CK_CODE) && ir->getCode() != src->getCode()) {
            return false;
        }
        if (flag.have(ISOMO_CK_TYPE) && ir->getType() != src->getType()) {
            return false;
        }
        if (ir->getOffset() != src->getOffset()) {
            return false;
        }
        ASSERT0(ir->getBase() && src->getBase());
        return isIRIsomorphic(ir->getBase(), src->getBase(), is_cmp_kid, flag);
    SWITCH_CASE_PR_OP:
        if (!ir->isPROp()) { return false; }
        if (flag.have(ISOMO_CK_CODE) && ir->getCode() != src->getCode()) {
            return false;
        }
        if (flag.have(ISOMO_CK_TYPE) && ir->getType() != src->getType()) {
            return false;
        }
        if (flag.have(ISOMO_CK_PRNO) && ir->getPrno() != src->getPrno()) {
            return false;
        }
        break;
    SWITCH_CASE_ARRAY_OP:
        if (!isArrayIsomorphic(ir, src, is_cmp_kid, flag)) { return false; }
        break;
    case IR_LDA:
        if (ir->getCode() != src->getCode()) { return false; }
        if (flag.have(ISOMO_CK_TYPE) && ir->getType() != src->getType()) {
            return false;
        }
        if (LDA_idinfo(ir) != LDA_idinfo(src) ||
            LDA_ofst(ir) != LDA_ofst(src)) {
            return false;
        }
        break;
    case IR_CALL:
        if (ir->getCode() != src->getCode()) { return false; }
        if (CALL_idinfo(ir) != CALL_idinfo(src)) { return false; }
        break;
    case IR_ICALL:
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        if (ir->getCode() != src->getCode()) { return false; }
        break;
    case IR_LABEL:
        if (ir->getCode() != src->getCode()) { return false; }
        if (LAB_lab(ir) != LAB_lab(src)) { return false; }
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SELECT:
        if (ir->getCode() != src->getCode()) { return false; }
        if (flag.have(ISOMO_CK_TYPE) && ir->getType() != src->getType()) {
            return false;
        }
        break;
    case IR_REGION:
        //One should implement comparation function for your own region.
        return false;
    case IR_GOTO:
    case IR_IGOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_SWITCH:
    case IR_CASE:
    case IR_RETURN:
    case IR_BREAK:
    case IR_CONTINUE:
        break;
    default: UNREACHABLE();
    }
    if (!is_cmp_kid) { return true; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir) && i < IR_MAX_KID_NUM(src); i++) {
        IR const* kid1 = ir->getKid(i);
        IR const* kid2 = src->getKid(i);
        if (src->isCallStmt() &&
            (kid1 == CALL_dummyuse(ir) ||
             kid2 == CALL_dummyuse(src))) {
            //Do NOT check the equality of dummyuses.
            continue;
        }
        if (!isIRListIsomorphic(kid1, kid2, is_cmp_kid, flag)) {
            return false;
        }
    }
    //Note there is no need to ask the number of ir and src must be same.
    //ASSERT0(i == IR_MAX_KID_NUM(ir) && i == IR_MAX_KID_NUM(src));
    return true;
}


//Return true if current ir tree is isomorphic to src.
//src: root of IR tree.
//is_cmp_kid: it is true if comparing kids as well.
//Note the function does not compare the siblings of 'src'.
bool IR::isIsomoTo(IR const* src, bool is_cmp_kid, IsomoFlag const& flag) const
{
    return isIRIsomorphic(this, src, is_cmp_kid, flag);
}


//Return true if current ir tree is equivalent to src.
//src: root of IR tree.
//is_cmp_kid: it is true if comparing kids as well.
//Note the function does not compare the siblings of 'src'.
bool IR::isIREqual(IR const* src, bool is_cmp_kid) const
{
    return isIRIsomorphic(this, src, is_cmp_kid, IsomoFlag(ISOMO_CK_ALL));
}


//Contructing IR forest.
//'recur': true to iterate kids.
void IR::setParentPointer(bool recur)
{
    for (INT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid = getKid(i);
        if (kid != nullptr) {
            while (kid != nullptr) {
                IR_parent(kid) = this;
                if (recur) {
                    kid->setParentPointer(recur);
                }
                kid = kid->get_next();
            }
        }
    }
}


//Find the first PR related to 'prno'.
//This function iterate IR tree nonrecursively.
IR * IR::getOpndPRList(PRNO prno) const
{
    IR * pr = nullptr;
    IR * p = const_cast<IR*>(this); //this is header of list.
    while (p != nullptr) {
        if ((pr = p->getOpndPR(prno)) != nullptr) {
            return pr;
        }
        p = p->get_next();
    }
    return nullptr;
}


//Find the first PR related to 'prno'.
//This function iterate IR tree nonrecursively.
//'it': iterator.
IR * IR::getOpndPR(PRNO prno, IRIter & it) const
{
    ASSERT0(is_stmt());
    it.clean();
    for (IR * k = xoc::iterInit(const_cast<IR*>(this), it);
         k != nullptr; k = xoc::iterNext(it)) {
        if (k->is_pr() && PR_no(k) == prno) {
            ASSERT0(is_rhs(k));
            return k;
        }
    }
    return nullptr;
}


bool IR::isRHSUseIsomoExp(IR const* exp) const
{
    ASSERT0(exp);
    if (is_exp() && isIsomoTo(exp)) {
        return true;
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid = getKid(i);
        if (kid != nullptr && kid->isRHSUseIsomoExp(exp)) {
            return true;
        }
    }
    return false;
}


//This function recursively iterate the IR tree to
//retrieve the PR whose PR_no is equal to given 'prno'.
//Otherwise return nullptr.
IR * IR::getOpndPR(PRNO prno) const
{
    if (isReadPR() && getPrno() == prno) {
        return const_cast<IR*>(this);
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid = getKid(i);
        if (kid != nullptr && kid->isReadPR() && kid->getPrno() == prno) {
            ASSERTN(!kid->isWritePR(), ("stmt should not be kid in IR tree"));
            return kid;
        }
        IR * f = kid->getOpndPR(prno);
        if (f != nullptr) { return f; }
    }
    return nullptr;
}


//This function recursively iterate the IR tree to
//retrieve the memory-ref IR whose MD is equal to given 'md'.
//Otherwise return nullptr.
IR * IR::getOpndMem(MD const* md) const
{
    if (isMemOpnd() && getRefMD() == md) {
        return const_cast<IR*>(this);
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid = getKid(i);
        if (kid != nullptr && kid->isMemRef() && kid->getRefMD() == md) {
            return kid;
        }
        IR * f = kid->getOpndMem(md);
        if (f != nullptr) { return f; }
    }
    return nullptr;
}


//Get the Stmt accroding to given prno.
//The stmt must write to PR as a result.
//This function can not be const because it will return itself.
IR * IR::getResultPR(PRNO prno)
{
    switch (getCode()) {
    SWITCH_CASE_WRITE_PR:
    SWITCH_CASE_CALL:
        return getPrno() == prno ? this : nullptr;
    SWITCH_CASE_WRITE_ARRAY:
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_BRANCH_OP:
    SWITCH_CASE_CFS_OP:
    SWITCH_CASE_LOOP_ITER_CFS_OP:
    case IR_LABEL:
    case IR_REGION:
        return nullptr;
    default: UNREACHABLE();
    }
    return nullptr;
}


//Copy MD that ir referenced accroding to 'mds'.
void IR::setRefMD(MD const* md, Region * rg)
{
    DU * du = getDU();
    if (du == nullptr) {
        if (md == nullptr) { return; }

        ASSERT0(rg);
        du = rg->allocDU();
        setDU(du);
    }
    DU_md(du) = md;
}


//Copy the set of MD that ir referenced accroding to 'mds'.
void IR::setRefMDSet(MDSet const* mds, Region * rg)
{
    ASSERT0(!is_region());
    DU * du = getDU();
    if (du == nullptr) {
        if (mds == nullptr) { return; }

        ASSERT0(rg);
        du = rg->allocDU();
        setDU(du);
    }
    DU_mds(du) = mds;
}


void IR::invertLand(Region * rg)
{
    ASSERT0(rg);
    //a&&b => !a || !b
    IR * newop0 = rg->getIRMgr()->buildLogicalNot(BIN_opnd0(this));
    IR * newop1 = rg->getIRMgr()->buildLogicalNot(BIN_opnd1(this));
    IR_code(this) = IR_LOR;
    BIN_opnd0(this) = newop0;
    BIN_opnd1(this) = newop1;
    IR_parent(newop0) = this;
    IR_parent(newop1) = this;
}


void IR::invertLor(Region * rg)
{
    ASSERT0(rg);
    //a||b => !a && !b
    IR * newop0 = rg->getIRMgr()->buildLogicalNot(BIN_opnd0(this));
    IR * newop1 = rg->getIRMgr()->buildLogicalNot(BIN_opnd1(this));
    IR_code(this) = IR_LAND;
    BIN_opnd0(this) = newop0;
    BIN_opnd1(this) = newop1;
    IR_parent(newop0) = this;
    IR_parent(newop1) = this;
}


//Clean all DU-Chain and Defined/Used-MD reference info.
void IR::freeDUset(DUMgr * dumgr)
{
    ASSERT0(dumgr);
    DU * du = getDU();
    if (du == nullptr || DU_duset(du) == nullptr) { return; }

    //Free DUSet back to DefSegMgr, or it will
    //complain and make an assertion.
    dumgr->getSBSMgr()->freeSBitSetCore(DU_duset(du));
    DU_duset(du) = nullptr;
}


static void removeSSAUseRecur(IR * ir)
{
    if (ir->is_stmt()) {
        SSAInfo * ssainfo = ir->getSSAInfo();
        if (ssainfo != nullptr) {
            ssainfo->cleanDU();
        }
    } else {
        SSAInfo * ssainfo = ir->isPROp() ? ir->getSSAInfo() : nullptr;
        if (ssainfo != nullptr) {
            SSA_uses(ssainfo).remove(ir);
        }
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->getKid(i); x != nullptr; x = x->get_next()) {
            if (x->is_pr()) {
                SSAInfo * ssainfo = PR_ssainfo(x);
                if (ssainfo != nullptr) {
                    SSA_uses(ssainfo).remove(x);
                }
            } else {
                ASSERT0(!x->isReadPR());
            }

            if (!x->is_leaf()) {
                removeSSAUseRecur(x);
            }
        }
    }
}


//Remove PRSSA Use-Def chain.
//e.g:pr1=...
//    ...=pr1 //S1
//If S1 deleted, pr1 should be removed from its SSA_uses.
void IR::removeSSAUse()
{
    removeSSAUseRecur(this);
}


//Copy memory reference only for current ir node.
//src: copy MD reference from 'src', it should be not same with current ir.
void IR::copyRef(IR const* src, Region * rg)
{
    ASSERT0(src && rg && this != src);
    ASSERTN(isMemRef() && src->isMemRef(), ("not memory reference"));
    ASSERT0(!src->is_undef());
    setRefMD(src->getRefMD(), rg);
    if (isReadPR() || isWritePR()) {
        //PR operation does not have MDSet reference.
        return;
    }
    //CALL stmt may have MDSet.
    setRefMDSet(src->getRefMDSet(), rg);
}


//Copy AttachInfo from 'src' to current ir, not include kid and sibling.
void IR::copyAI(IR const* src, Region * rg)
{
    if (src->getAI() == nullptr) { return; }
    if (!src->getAI()->is_init()) {
        //Destroy current AI to be consistent with src's AI status.
        if (IR_ai(this) != nullptr) {
            IR_ai(this)->destroy();
        }
        return;
    }
    if (IR_ai(this) == nullptr) {
        IR_ai(this) = rg->allocAIContainer();
    }
    //Note AI of current IR may not yet be initialized.
    ASSERT0(rg->getAttachInfoMgr());
    rg->getAttachInfoMgr()->copyAI(this, src);
}


//Dump IR tree's MD reference, where ir may be stmt or exp.
//indent: the addend to current indent of LogMgr.
void IR::dumpRef(Region * rg, UINT indent)
{
    if (!rg->isLogMgrInit() || is_const()) { return; }
    rg->getLogMgr()->incIndent(indent);
    dumpIR(this, rg, nullptr, false);

    //Dump mustref MD.
    MD const* md = getRefMD();
    MDSet const* mds = getRefMDSet();

    //MustDef
    bool prt_mustdef = false;
    if (md != nullptr) {
        note(rg, "\n%sMD%d", md->is_exact() ? "E" : "",  md->id());
        prt_mustdef = true;
    }

    if (mds != nullptr) {
        //MayDef
        if (!prt_mustdef) {
            note(rg, "\n"); //dump indent blank.
        }
        prt(rg, " : ");
        if (!isReadOnly()) {
            if (mds != nullptr && !mds->is_empty()) {
                mds->dump(rg->getMDSystem(), rg->getVarMgr());
            }
        }
    }

    if (isCallStmt()) {
        bool doit = false;
        CallGraph * callg = rg->getCallGraphPreferProgramRegion();
        if (callg != nullptr) {
            Region * callee = callg->getCalleeRegion(this, rg);
            if (callee != nullptr && callee->is_ref_valid()) {
                MDSet const* muse = callee->getMayUse();
                //May use
                prt(rg, " <-- ");
                if (muse != nullptr && !muse->is_empty()) {
                    muse->dump(callee->getMDSystem(), rg->getVarMgr());
                    doit = true;
                }
            }
        }
        if (!doit) {
            //MayUse MDSet.
            //Regard MayDef MDSet as MayUse.
            prt(rg, " <-- ");
            MDSet const* x = getRefMDSet();
            if (x != nullptr && !x->is_empty()) {
                x->dump(rg->getMDSystem(), rg->getVarMgr());
            }
        }
    }

    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        for (IR * k = getKid(i); k != nullptr; k = k->get_next()) {
            k->dumpRef(rg, 2);
        }
    }
    rg->getLogMgr()->decIndent(indent);
}


//Return true if current ir does not overlap to ir2.
//ir2: stmt or expression to be compared.
//Note this function is different to isNotOverlap(), it determines overlapping
//through MD and MDSet references.
bool IR::isNotOverlapByMDRef(IR const* ir2, Region const* rg) const
{
    MD const* must1 = getRefMD();
    MD const* must2 = ir2->getRefMD();
    MDSet const* may1 = getRefMDSet();
    MDSet const* may2 = ir2->getRefMDSet();
    if (must1 != nullptr && must2 != nullptr) {
        //CASE: the MustRef of ID may be GLOBAL_MEM or IMPORT_MEM.
        return !(must1 == must2 || must1->is_overlap(must2));
    }
    if (must1 != nullptr && may2 != nullptr && may2->is_contain(must1, rg)) {
        return false;
    }
    if (must2 != nullptr && may1 != nullptr && may1->is_contain(must2, rg)) {
        return false;
    }
    if (may1 != nullptr && may2 != nullptr &&
        (may1 == may2 || may1->is_intersect(*may2))) {
        return false;
    }
    return true;
}


//The function compare the memory object that 'this' and 'ir2' accessed,
//and return true if 'this' object is NOT overlapped with 'ir2',
//otherwise return false.
//ir2: stmt or expression to be compared.
//e.g: this and ir2 are overlapped:
//     'this' object: |--------|
//     'ir2'  object:        |----|
//e.g: this and ir2 are NOT overlapped:
//     'this' object: |------|
//     'ir2'  object:        |----|
//
//Note: The function will NOT consider the different pattern
// of 'this' and ir2.
// The function does not require RefMD information.
// The function just determine overlapping of given two IR according to
// their data-type and offset.
bool IR::isNotOverlap(IR const* ir2, Region const* rg) const
{
    ASSERT0(rg);
    IR const* ir1 = this;
    ASSERT0(ir1 && ir2);
    if ((!ir1->getType()->is_scalar() | !ir2->getType()->is_scalar()) ||
        !ir1->hasOffset() || !ir2->hasOffset()) {
        return false;
    }
    TMWORD offset1 = ir1->getOffset();
    TMWORD offset2 = ir2->getOffset();
    UINT size1 = rg->getTypeMgr()->getByteSize(ir1->getType());
    UINT size2 = rg->getTypeMgr()->getByteSize(ir2->getType());
    if ((offset1 + size1 <= offset2) || (offset2 + size2 <= offset1)) {
        return true;
    }
    return false;
}


//The function compare the memory object that 'this' and 'ir2' accessed,
//and return true if 'this' object is conver 'ir2',
//otherwise return false.
//ir2: stmt or expression to be compared.
//e.g: 'this' covers ir2:
//     'this' object: |------|
//     'ir2'  object:   |----|
//e.g: this is NOT cover ir2:
//     'this' object: |------|
//     'ir2'  object:        |----|
//
//Note: The function will NOT consider the different pattern
// of 'this' and ir2.
// The function does not require RefMD information.
// The function just determine overlapping of given two IR according to
// their data-type and offset.
bool IR::isCover(IR const* ir2, Region const* rg) const
{
    ASSERT0(rg);
    IR const* ir1 = this;
    ASSERT0(ir1 && ir2);
    TMWORD offset1 = ir1->getOffset();
    TMWORD offset2 = ir2->getOffset();
    UINT size1 = rg->getTypeMgr()->getByteSize(ir1->getType());
    UINT size2 = rg->getTypeMgr()->getByteSize(ir2->getType());
    if ((offset1 <= offset2) && (offset1 + size1 >= offset2 + size2)) {
        return true;
    }
    return false;
}


//Set prno, and update SSAInfo meanwhile.
void IR::setPrnoAndUpdateSSAInfo(PRNO prno)
{
    ASSERT0(prno != getPrno());
    if (getSSAInfo() != nullptr) {
        PRSSAMgr::removePRSSAOcc(this);
        setSSAInfo(nullptr);
    }
    setPrno(prno);
}


void IR::copyRefForTree(IR const* src, Region * rg)
{
    ASSERT0(src && isIREqual(src, true) && rg);
    ASSERT0(src != this);
    if (isMemRef()) {
        setRefMD(src->getRefMD(), rg);
        setRefMDSet(src->getRefMDSet(), rg);
    }

    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid = getKid(i);
        if (kid == nullptr) { continue; }
        IR * srckid = src->getKid(i);
        ASSERT0(srckid);
        for (; kid != nullptr;
             kid = IR_next(kid), srckid = IR_next(srckid)) {
            ASSERT0(srckid);
            kid->copyRefForTree(srckid, rg);
        }
    }
}


void IR::setRHS(IR * rhs)
{
    ASSERT0(hasRHS());
    ASSERT0(IRDES_accrhsfunc(g_ir_desc[getCode()]));
    (*IRDES_accrhsfunc(g_ir_desc[getCode()]))(this) = rhs;
    if (rhs != nullptr) {
        IR_parent(rhs) = this;
    }
}


bool IR::isDummyOp() const
{
    switch (getCode()) {
    SWITCH_CASE_EXT_VSTMT: return true;
    default: return is_dummy();
    }
    return false;
}


//This function recursively iterate the IR tree to
//retrieve whether the IR is may-throw.
//Record if ir might throw exception.
bool IR::isMayThrow(bool recur) const
{
    if (is_may_throw()) { return true; }
    if (!recur) { return false; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR const* tmp = getKid(i);
        if (tmp == nullptr) { continue; }
        if (tmp->is_may_throw()) {
            return true;
        }
        if (tmp->isMayThrow(true)) { return true; }
    }
    return false;
}


bool IR::hasSideEffect(bool recur) const
{
    if (IR_has_sideeffect(this)) { return true; }
    if (!recur) { return false; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR const* tmp = getKid(i);
        if (tmp == nullptr) { continue; }
        if (IR_has_sideeffect(tmp)) {
            return true;
        }
        if (tmp->hasSideEffect(true)) { return true; }
    }
    return false;
}


//This function recursively iterate the IR tree to
//retrieve whether the IR is no-movable.
bool IR::isNoMove(bool recur) const
{
    if (IR_no_move(this)) { return true; }
    if (!recur) { return false; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR const* tmp = getKid(i);
        if (tmp == nullptr) { continue; }
        if (IR_no_move(tmp)) {
            return true;
        }
        if (tmp->isNoMove(true)) { return true; }
    }
    return false;
}


//Check if 'exp' is child or grandchildren of current ir.
//Here we only compare equality of two IR pointer to determine and apply
//the DFS searching in tree.
bool IR::is_kids(IR const* exp) const
{
    if (exp == nullptr) { return false; }
    IR * tmp;
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        tmp = getKid(i);
        while (tmp != nullptr) {
            if (exp == tmp) {
                return true;
            }
            if (tmp->is_kids(exp)) {
                return true;
            }
            tmp = IR_next(tmp);
        } //end while
    } //end for
    return false;
}


//Return true if ir exactly modified 'md' or elements in MDSet 'mds'.
//md: given md, may be nullptr.
//mds: given MDSet, may be nullptr.
bool IR::isExactDef(MD const* md, MDSet const* mds) const
{
    ASSERT0(is_stmt());
    MD const* cur_ir_defined_md = getRefMD();
    if (cur_ir_defined_md != nullptr && cur_ir_defined_md->is_exact()) {
        if (md != nullptr &&
            (cur_ir_defined_md == md || cur_ir_defined_md->is_overlap(md))) {
            return true;
        }
        if (mds != nullptr && mds->is_contain_pure(cur_ir_defined_md->id())) {
            return true;
        }
    }
    //We can not determine whether current ir is
    //exactly modified md or mds.
    return false;
}


bool IR::isExactDef(MD const* md) const
{
    ASSERT0(is_stmt() && md);
    if (!md->is_exact()) { return false; }
    MD const* cur_ir_defined_md = getRefMD();
    if (cur_ir_defined_md != nullptr &&
        cur_ir_defined_md->is_exact() &&
        (cur_ir_defined_md == md || cur_ir_defined_md->is_overlap(md))) {
        return true;
    }
    return false;
}


//Return true if current ir is integer constant, and the number
//is equal to 'value'.
bool IR::isConstFPValueEqualTo(HOST_FP value) const
{
    if (!isConstExp()) { return false; }
    IR const* p = this;
    while (!p->is_const()) {
        ASSERTN(p->is_cvt(), ("const expression only include CVT and CONST."));
        p = CVT_exp(p);
        ASSERT0(p);
    }
    return p->is_fp() && TypeMgr::isEqual(CONST_fp_val(p), value);
}


//Return true if current ir is integer constant, and the number
//is equal to 'value'.
bool IR::isConstIntValueEqualTo(HOST_INT value) const
{
    if (!isConstExp()) { return false; }
    IR const* p = this;
    while (!p->is_const()) {
        ASSERTN(p->is_cvt(), ("const expression only include CVT and CONST."));
        p = CVT_exp(p);
        ASSERT0(p);
    }
    return p->is_int() && CONST_int_val(p) == value;
}


//Find and substitute 'newk' for 'oldk'.
//Return true if replaced the 'oldk'.
//'recur': set to true if function recusively perform
//replacement for 'oldk'.
bool IR::replaceKid(IR * oldk, IR * newk, bool recur)
{
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid = getKid(i);
        if (kid == nullptr) { continue; }
        for (IR * x = kid; x != nullptr; x = x->get_next()) {
            if (x == oldk) {
                xcom::replace(&kid, oldk, newk);
                if (IR_prev(newk) == nullptr) {
                    //oldk is the header, and update the kid i.
                    setKid(i, kid);
                } else {
                    IR_parent(newk) = IR_parent(oldk);
                }
                return true;
            }
            if (recur && x->replaceKid(oldk, newk, true)) {
                return true;
            }
        }
    }
    return false;
}


//Return true if ir is branch-op and has multiple jump targets.
bool IR::hasMultiTarget() const
{
    switch (getCode()) {
    case IR_SWITCH: {
        UINT numoftgt = 0;
        if (SWITCH_deflab(this) != nullptr) {
            numoftgt++;
        }
        if (getCaseList() != nullptr) { numoftgt++;}
        return numoftgt > 1;
    }
    case IR_IGOTO: {
        IR const* caselst = getCaseList();
        ASSERT0(caselst);
        if (caselst->get_next() != nullptr) { return true; }
        return false;
    }
    default:;
    }
    return false;
}


bool IR::isReadOnly() const
{
    switch (getCode()) {
    case IR_CALL: return CALL_is_readonly(this);
    case IR_ICALL: return ICALL_is_readonly(this);
    case IR_CVT: return CVT_exp(this)->isReadOnly();
    case IR_LD:
        if (LD_idinfo(this)->is_readonly() &&
            !LD_idinfo(this)->is_volatile()) {
            return true;
        }
        return false;
    default:;
    }
    return false;
}


bool IR::is_volatile() const
{
    //Describing if IR's address has been taken.
    if (is_id()) {
        Var * id_info = ID_info(this);
        ASSERT0(id_info != nullptr);
        return id_info->is_volatile();
    }
    return false;
}


IR * IR::getRHS() const
{
    ASSERT0(hasRHS());
    ASSERT0(IRDES_accrhsfunc(g_ir_desc[getCode()]));
    return (*IRDES_accrhsfunc(g_ir_desc[getCode()]))(const_cast<IR*>(this));
}


//Return true if ir is base expression of array operation.
bool IR::isArrayBase(IR const* ir) const
{
    ASSERT0(isArrayOp());
    return ((CArray*)this)->is_base(ir);
}


PRNO IR::getPrno() const
{
    ASSERT0(isPROp());
    ASSERT0(IRDES_accprnofunc(g_ir_desc[getCode()]));
    return (*IRDES_accprnofunc(g_ir_desc[getCode()]))(const_cast<IR*>(this));
}


void IR::cleanSSAInfo()
{
    if (IRDES_accssainfofunc(g_ir_desc[getCode()]) != nullptr) {
        //DO NOT ASSERT even if current IR has no related field for
        //conveninent purpose.
        setSSAInfo(nullptr);
    }
}


SSAInfo * IR::getSSAInfo() const
{
    ASSERT0(IRDES_accssainfofunc(g_ir_desc[getCode()]));
    return (*IRDES_accssainfofunc(g_ir_desc[getCode()]))(const_cast<IR*>(this));
}


IR * IR::getKid(UINT idx) const
{
    ASSERT0(IRDES_acckidfunc(g_ir_desc[getCode()]));
    return (*IRDES_acckidfunc(g_ir_desc[getCode()]))(
        const_cast<IR*>(this), idx);
}


IRBB * IR::getBB() const
{
    ASSERT0(IRDES_accbbfunc(g_ir_desc[getCode()]));
    return (*IRDES_accbbfunc(g_ir_desc[getCode()]))(const_cast<IR*>(this));
}


Var * IR::getIdinfo() const
{
    ASSERT0(hasIdinfo());
    ASSERT0(IRDES_accidinfofunc(g_ir_desc[getCode()]));
    return (*IRDES_accidinfofunc(g_ir_desc[getCode()]))(const_cast<IR*>(this));
}


void IR::setIdinfo(Var * idinfo)
{
    ASSERT0(hasIdinfo());
    ASSERT0(IRDES_accidinfofunc(g_ir_desc[getCode()]));
    (*IRDES_accidinfofunc(g_ir_desc[getCode()]))(this) = idinfo;
}


TMWORD IR::getOffset() const
{
    IRAccOfstFuncType func = IRDES_accofstfunc(g_ir_desc[getCode()]);
    //DO NOT ASSERT even if current IR has no offset for
    //conveninent purpose.
    return func != nullptr ? (*func)(const_cast<IR*>(this)) : 0;
}


IR * IR::getBase() const
{
    IRAccBaseFuncType func = IRDES_accbasefunc(g_ir_desc[getCode()]);
    //DO NOT ASSERT even if current IR has no related field for
    //conveninent purpose.
    return func != nullptr ? (*func)(const_cast<IR*>(this)) : nullptr;
}


void IR::setBase(IR * exp)
{
    ASSERT0(exp && exp->is_exp());
    IRAccBaseFuncType func = IRDES_accbasefunc(g_ir_desc[getCode()]);
    //DO NOT ASSERT even if current IR has no related field for
    //conveninent purpose.
    if (func != nullptr) {
        (*func)(const_cast<IR*>(this)) = exp;
        IR_parent(exp) = this;
    }
}


//Return label info if exist.
LabelInfo const* IR::getLabel() const
{
    IRAccLabFuncType func = IRDES_acclabfunc(g_ir_desc[getCode()]);
    //DO NOT ASSERT even if current IR has no related field for
    //conveninent purpose.
    return func != nullptr ? (*func)(const_cast<IR*>(this)) : nullptr;
}


UINT IR::getArrayElemDtSize(TypeMgr const* tm) const
{
    ASSERT0(is_array() || is_starray());
    return tm->getByteSize(ARR_elemtype(this));
}


bool IR::isConstExp() const
{
    if (is_const()) { return true; }
    if (is_cvt()) { return CVT_exp(this)->isConstExp(); }
    return false;
}


bool IR::isDirectArrayRef() const
{
    return isArrayOp() && ARR_base(this)->is_lda();
}


void IR::setBB(IRBB * bb)
{
    ASSERT0(IRDES_accbbfunc(g_ir_desc[getCode()]));
    (*IRDES_accbbfunc(g_ir_desc[getCode()]))(this) = bb;
}


void IR::setOffset(TMWORD ofst)
{
    ASSERT0(hasOffset());
    ASSERT0(IRDES_accofstfunc(g_ir_desc[getCode()]));
    (*IRDES_accofstfunc(g_ir_desc[getCode()]))(this) = ofst;
}


void IR::setSSAInfo(SSAInfo * ssa)
{
    ASSERT0(IRDES_accssainfofunc(g_ir_desc[getCode()]));
    (*IRDES_accssainfofunc(g_ir_desc[getCode()]))(this) = ssa;
}


void IR::setPrno(PRNO prno)
{
    ASSERT0(isPROp());
    ASSERT0(IRDES_accprnofunc(g_ir_desc[getCode()]));
    (*IRDES_accprnofunc(g_ir_desc[getCode()]))(this) = prno;
}


void IR::setStorageSpace(StorageSpace ss)
{
    ASSERT0(IRDES_accssfunc(g_ir_desc[getCode()]));
    (*IRDES_accssfunc(g_ir_desc[getCode()]))(this) = ss;
}


StorageSpace IR::getStorageSpace() const
{
    ASSERT0(IRDES_accssfunc(g_ir_desc[getCode()]));
    return (*IRDES_accssfunc(g_ir_desc[getCode()]))(const_cast<IR*>(this));
}


//Return label or nullptr.
void IR::setLabel(LabelInfo const* li)
{
    ASSERT0(IRDES_acclabfunc(g_ir_desc[getCode()]));
    (*IRDES_acclabfunc(g_ir_desc[getCode()]))(this) = li;
}


//Set the No.idx child to be 'kid', and update the IR_parent of kid.
void IR::setKid(UINT idx, IR * kid)
{
    ASSERT0(IRDES_acckidfunc(g_ir_desc[getCode()]));
    (*IRDES_acckidfunc(g_ir_desc[getCode()]))(this, idx) = kid;
    for (IR * k = kid; k != nullptr; k = IR_next(k)) {
        IR_parent(k) = this;
    }
}


//Return true if k is the kid node of current ir.
bool IR::is_lhs(IR const* k) const
{
    ASSERT0(is_stmt());
    if (isIndirectMemOp()) { return false; }
    return hasResult() ? k == this : false;
}


//Return true if current ir can be placed in BB.
bool IR::isStmtInBB() const
{
    if (is_switch() && SWITCH_body(this) != nullptr) { return false; }
    return IRDES_is_stmt_in_bb(g_ir_desc[getCode()]);
}


//Set ir DU to be nullptr, return the DU pointer.
DU * IR::cleanDU()
{
    switch (getCode()) {
    SWITCH_CASE_HAS_DU: {
        DU * du = DUPROP_du(this);
        DUPROP_du(this) = nullptr;
        return du;
    }
    default:;
    }
    return nullptr;
}


//Return stmt if it writes PR as result.
//This function can not be const because it will return itself.
IR * IR::getResultPR()
{
    ASSERT0(is_stmt());
    IRAccResultPRFuncType func = IRDES_accresultprfunc(g_ir_desc[getCode()]);
    //DO NOT ASSERT even if current IR has no related field for
    //conveninent purpose.
    return func != nullptr ? (*func)(this) : nullptr;
}


//Return true if ir is call and does have a return value.
bool IR::hasReturnValue() const
{
    ASSERT0(isCallStmt());
    return CALL_prno(this) != PRNO_UNDEF;
}


bool IR::isIntrinsicOp() const
{
    return isCallStmt() && CALL_is_intrinsic(this);
}


DU * IR::getDU() const
{
    return hasDU() ? DUPROP_du(this) : nullptr;
}


void IR::setDU(DU * du)
{
    ASSERT0(hasDU());
    DUPROP_du(this) = du;
}


//Return expression if stmt has CASE list.
IR * IR::getCaseList() const
{
    //Both stmt and exp.
    switch (getCode()) {
    case IR_SWITCH: return SWITCH_case_list(this);
    case IR_IGOTO: return IGOTO_case_list(this);
    default: UNREACHABLE();
    }
    return nullptr;
}


//The function collects the LabelInfo for each branch-target.
void IR::collectLabel(OUT List<LabelInfo const*> & lst) const
{
    switch (getCode()) {
    case IR_SWITCH: ((CSwitch*)this)->collectLabel(lst); break;
    case IR_IGOTO: ((CIGoto*)this)->collectLabel(lst); break;
    default: UNREACHABLE();
    }
}


IR * IR::getValExp() const
{
    switch (getCode()) {
    case IR_SWITCH: return SWITCH_vexp(this);
    case IR_IGOTO: return IGOTO_vexp(this);
    default: UNREACHABLE();
    }
    return nullptr;
}


void IR::setValExp(IR * exp)
{
    switch (getCode()) {
    case IR_SWITCH: SWITCH_vexp(this) = exp; break;
    case IR_IGOTO: IGOTO_vexp(this) = exp; break;
    default: UNREACHABLE();
    }
    IR_parent(exp) = this;
}


bool IR::hasAlign() const
{
    switch (getCode()) {
    case IR_LD:
    case IR_ST:
    case IR_ARRAY:
    case IR_STARRAY:
    case IR_ILD:
    case IR_IST:
        return true;
    default: return false;
    }
    return false;
}


UINT IR::getAlign() const
{
    switch (getCode()) {
    case IR_LD: return LD_align(this);
    case IR_ST: return ST_align(this);
    case IR_ARRAY: return ARR_align(this);
    case IR_STARRAY: return STARR_align(this);
    case IR_ILD: return ILD_align(this);
    case IR_IST: return IST_align(this);
    default: ASSERT0(0); //TODO
    }
    return false;
}


void IR::setAlign(UINT align_bytenum)
{
    switch (getCode()) {
    case IR_LD: LD_align(this) = align_bytenum; return;
    case IR_ST: ST_align(this) = align_bytenum; return;
    case IR_ARRAY: ARR_align(this) = align_bytenum; return;
    case IR_STARRAY: STARR_align(this) = align_bytenum; return;
    case IR_ILD: ILD_align(this) = align_bytenum; return;
    case IR_IST: IST_align(this) = align_bytenum; return;
    default: ASSERT0(0); //TODO
    }
}


bool IR::hasAlignedAttr() const
{
    switch (getCode()) {
    case IR_LD: return LD_is_aligned(this);
    case IR_ST: return ST_is_aligned(this);
    case IR_ARRAY: return ARR_is_aligned(this);
    case IR_STARRAY: return STARR_is_aligned(this);
    case IR_ILD: return ILD_is_aligned(this);
    case IR_IST: return IST_is_aligned(this);
    default: return false;
    }
    return false;
}


void IR::setAligned(bool is_aligned)
{
    switch (getCode()) {
    case IR_LD: LD_is_aligned(this) = is_aligned; return;
    case IR_ST: ST_is_aligned(this) = is_aligned; return;
    case IR_ARRAY: ARR_is_aligned(this) = is_aligned; return;
    case IR_STARRAY: STARR_is_aligned(this) = is_aligned; return;
    case IR_ILD: ILD_is_aligned(this) = is_aligned; return;
    case IR_IST: IST_is_aligned(this) = is_aligned; return;
    default: return;
    }
}


//Return determinate expression if any.
IR * IR::getJudgeDet() const
{
    ASSERT0(hasJudgeDet());
    ASSERT0(IRDES_accdetfunc(g_ir_desc[getCode()]));
    return (*IRDES_accdetfunc(g_ir_desc[getCode()]))(const_cast<IR*>(this));
}


void IR::setJudgeDet(IR * det)
{
    ASSERT0(det && det->is_exp());
    ASSERT0(hasJudgeDet());
    ASSERT0(IRDES_accdetfunc(g_ir_desc[getCode()]));
    (*IRDES_accdetfunc(g_ir_desc[getCode()]))(this) = det;
    IR_parent(det) = this;
}


IR_CODE IR::invertIRCode(IR_CODE src)
{
    switch(src) {
    case IR_LT: return IR_GE;
    case IR_LE: return IR_GT;
    case IR_GT: return IR_LE;
    case IR_GE: return IR_LT;
    case IR_EQ: return IR_NE;
    case IR_NE: return IR_EQ;
    default: ASSERTN(0, ("unsupport"));
    }
    return IR_UNDEF;
}


//This function invert the operation accroding to it semantics.
IR * IR::invertIRCode(IR * ir, Region * rg)
{
    switch (ir->getCode()) {
    case IR_LT: IR_code(ir) = IR_GE; break;
    case IR_LE: IR_code(ir) = IR_GT; break;
    case IR_GT: IR_code(ir) = IR_LE; break;
    case IR_GE: IR_code(ir) = IR_LT; break;
    case IR_EQ: IR_code(ir) = IR_NE; break;
    case IR_NE: IR_code(ir) = IR_EQ; break;
    case IR_TRUEBR: IR_code(ir) = IR_FALSEBR; break;
    case IR_FALSEBR: IR_code(ir) = IR_TRUEBR; break;
    case IR_LNOT: ir = UNA_opnd(ir); break;
    case IR_LOR:
        ir->invertLor(rg);
        break;
    case IR_LAND:
        ir->invertLand(rg);
        break;
    default: ASSERTN(0, ("unsupport"));
    }
    return ir;
}
//END IR

} //namespace xoc
