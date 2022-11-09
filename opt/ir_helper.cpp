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

static void verifyIR(IR * ir, BitSet * irh, Region const* rg)
{
    ASSERT0(irh != nullptr);
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k != nullptr) {
            ASSERTN(k->getParent() == ir, ("ir must be k's parent"));
            verifyIRList(k, irh, rg);
        }
    }

    //IR can not be used more than twice. Since there will cause
    //memory crash during freeIR().
    ASSERTN(!irh->is_contain(ir->id()), ("IR has been used again"));
    irh->bunion(ir->id());
    ir->verify(rg);
}


//Function to verify stmt info after IR simplified.
bool verifySimp(IR * ir_list, SimpCtx & simp)
{
    if (simp.isSimpCFG()) {
        for (IR * p = ir_list; p != nullptr; p = p->get_next()) {
            ASSERT0(p->is_stmt());
            ASSERT0(IR_parent(p) == nullptr);
        }
    }
    return true;
}


//Check for IR sanity and uniqueness.
bool verifyIRList(IR * ir, BitSet * irh, Region const* rg)
{
    BitSet * loc = nullptr;
    if (irh == nullptr) {
        loc = new BitSet();
        irh = loc;
    }
    while (ir != nullptr) {
        verifyIR(ir, irh, rg);
        ir = ir->get_next();
    }
    if (loc != nullptr) {
        delete loc;
    }
    return true;
}


//CASE:_$L9 is non-identifier char because of '$'.
bool isContainNonIdentifierChar(CHAR const* name)
{
    CHAR const* p = name;
    if (*p == 0) { return false; }
    if (!xcom::xisalpha(*p) && *p != '_') {
        //Check the first char.
        return true;
    }
    p++;
    for (; *p != 0; p++) {
        if (!xcom::xisalpha(*p) && *p != '_' && !xcom::xisdigit(*p)) {
            return true;
        }
    }
    return false;
}


//Iterative access ir tree. This funtion initialize the iterator.
//ir: the root ir of the tree.
//it: iterator. It should be clean already.
//iter_next: true if the function expect iterate the sibling IR.
//Readonly function.
IR const* iterInitC(IR const* ir, OUT ConstIRIter & it, bool iter_next)
{
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access ir tree.
//This function return the next IR node accroding to 'it'.
//it: iterator.
//iter_next: true if the function expect iterate the sibling IR.
//Readonly function.
IR const* iterNextC(MOD ConstIRIter & it, bool iter_next)
{
    IR const* ir = it.remove_head();
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//iter_next: true to iterate the next IR of ir in current iteration.
//The function is a readonly function.
//Use iterExpNextC to iter next IR.
//Note the function does not iterate inner stmt, e.g:stmts in body of IR_IF.
IR const* iterExpInitC(IR const* ir, OUT ConstIRIter & it)
{
    if (ir == nullptr) { return nullptr; }
    ASSERT0(ir->is_stmt());
    //Other stmt.
    IR const* firstkid = nullptr;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR const* kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        if (firstkid == nullptr) {
            firstkid = kid;
            continue;
        }
        it.append_tail(kid);
    }

    if (firstkid == nullptr) { return nullptr; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(firstkid); i++) {
        IR const* kid = firstkid->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (firstkid->get_next() != nullptr) {
        it.append_tail(firstkid->get_next());
    }
    return firstkid;
}


IR * iterInit(IN IR * ir, OUT IRIter & it, bool iter_next)
{
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


IR * iterNext(MOD IRIter & it, bool iter_next)
{
    IR * ir = it.remove_head();
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


IR * iterExpInit(IR const* ir, OUT IRIter & it)
{
    if (ir == nullptr) { return nullptr; }
    ASSERT0(ir->is_stmt());

    //Other stmt.
    IR * firstkid = nullptr;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        if (firstkid == nullptr) {
            firstkid = kid;
            continue;
        }
        it.append_tail(kid);
    }

    if (firstkid == nullptr) { return nullptr; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(firstkid); i++) {
        IR * kid = firstkid->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (firstkid->get_next() != nullptr) {
        it.append_tail(firstkid->get_next());
    }
    return firstkid;
}


IR * iterExpOfStmtInit(IR * ir, OUT IRIter & it)
{
    ASSERT0(ir->is_stmt());
    IR * firstkid = nullptr;
    switch (ir->getCode()) {
    case IR_IST:
        ASSERT0(IST_base(ir));
        firstkid = IST_base(ir);
        it.append_tail(firstkid);
        break;
    case IR_STARRAY:
        ASSERT0(STARR_base(ir));
        firstkid = STARR_base(ir);
        it.append_tail(firstkid);
        if (STARR_sub_list(ir) != nullptr) {
            it.append_tail(STARR_sub_list(ir));
        }
        break;
    default:;
    }
    return firstkid;
}


//Return the arthmetic precedence.
UINT getArithPrecedence(IR_CODE ty)
{
    UINT p = 0;
    switch (ty) {
    case IR_ARRAY:
        p = 1;
        break;
    case IR_NEG:
    case IR_BNOT:
    case IR_LNOT:
    case IR_ILD:
    case IR_LDA:
    case IR_CVT:
        p = 2;
        break;
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
        p = 4;
        break;
    case IR_ADD:
    case IR_SUB:
        p = 5;
        break;
    case IR_LSL:
    case IR_ASR:
    case IR_LSR:
        p = 6;
        break;
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
        p = 7;
        break;
    case IR_EQ:
    case IR_NE:
        p = 8;
        break;
    case IR_BAND:
        p = 9;
        break;
    case IR_XOR:
        p = 10;
        break;
    case IR_BOR:
        p = 11;
        break;
    case IR_LAND:
        p = 12;
        break;
    case IR_LOR:
        p = 13;
        break;
    case IR_SELECT:
        p = 14;
        break;
    SWITCH_CASE_EXT_EXP:
        p = 15;
        break;
    case IR_STPR:
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
    case IR_SETELEM:
    case IR_GETELEM:
    SWITCH_CASE_CALL:
    SWITCH_CASE_EXT_STMT:
        p = 16;
        break;
    default: ASSERTN(0, ("TODO"));
    }
    return p;
}


//
//START IRSet
//
void IRSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    DefSBitSet::dump(rg->getLogMgr()->getFileHandler());
}


bool IRSet::allElemBeExp(Region const* rg) const
{
    IRSetIter it;
    for (BSIdx i = get_first(&it); i != BS_UNDEF; i = get_next(i, &it)) {
        IR * e = rg->getIR(i);
        if (e == nullptr || !e->is_exp()) {
            return false;
        }
    }
    return true;
}
//END IRSet

} //namespace xoc
