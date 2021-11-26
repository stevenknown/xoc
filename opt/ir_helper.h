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
#ifndef __IR_HELPER_H__
#define __IR_HELPER_H__

namespace xoc {

typedef xcom::SEGIter * IRSetIter;

class IRSet : public DefSBitSet {
    COPY_CONSTRUCTOR(IRSet);
public:
    IRSet(DefSegMgr * sm) : DefSBitSet(sm) {}

    void append(IR const* v) { DefSBitSet::bunion(IR_id(v)); }

    bool find(IR const* v) const
    {
        ASSERT0(v);
        return DefSBitSet::is_contain(IR_id(v));
    }

    void remove(IR const* v)
    {
        ASSERT0(v);
        DefSBitSet::diff(IR_id(v));
    }
};


//Iterative access ir tree. This funtion initialize the iterator.
//'ir': the root ir of the tree.
//'it': iterator. It should be clean already.
//Readonly function.
inline IR const* iterInitC(IR const* ir, OUT ConstIRIter & it)
{
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access ir tree.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//Readonly function.
inline IR const* iterNextC(MOD ConstIRIter & it)
{
    IR const* ir = it.remove_head();
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it must be stmt.
//'it': iterator.
//This function is a readonly function.
//Use iterRhsNextC to iter next IR.
inline IR const* iterRhsInitC(IR const* ir, OUT ConstIRIter & it)
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

    //IR const* x = it.remove_head();
    //if (x == nullptr) { return nullptr; }

    if (firstkid == nullptr) { return nullptr; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(firstkid); i++) {
        IR const* kid = firstkid->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (IR_next(firstkid) != nullptr) {
        it.append_tail(IR_next(firstkid));
    }
    return firstkid;
}


//Iterative access the expression.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it must be expression.
//'it': iterator.
//Readonly function.
//Use iterRhsNextC to iter next IR.
inline IR const* iterExpInitC(IR const* ir, OUT ConstIRIter & it)
{
    if (ir == nullptr) { return nullptr; }
    ASSERT0(ir->is_exp());
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        it.append_tail(kid);
    }
    if (ir->get_next() != nullptr) {
        ASSERTN(!ir->get_next()->is_stmt(), ("ir can not be stmt list"));
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//Readonly function.
inline IR const* iterRhsNextC(MOD ConstIRIter & it)
{
    return iterNextC(it);
}


//Iterative access the ir tree that start with 'ir'.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it may be either stmt or expression.
//'it': iterator. It should be clean already.
//Note this function is NOT readonly, the returnd IR may be modified.
inline IR * iterInit(IN IR * ir, OUT IRIter & it)
{
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the ir tree.
//This funtion return the next IR node accroding to 'it'.
//'it': iterator.
//Note this function is NOT readonly, the returnd IR may be modified.
inline IR * iterNext(MOD IRIter & it)
{
    IR * ir = it.remove_head();
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it must be stmt.
//'it': iterator. It should be clean already.
//Use iterRhsNextC to iter next IR.
inline IR * iterRhsInit(IR * ir, OUT IRIter & it)
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
    if (IR_next(firstkid) != nullptr) {
        it.append_tail(IR_next(firstkid));
    }

    return firstkid;
}


//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//This is a readonly function.
inline IR * iterRhsNext(MOD IRIter & it)
{
    return iterNext(it);
}

bool allBeExp(IR * irlst);
bool allBeStmt(IR * irlst);

bool checkMaxIRType();
bool checkIRDesc();
bool checkRoundDesc();
CHAR const* compositeName(Sym const* n, xcom::StrBuf & buf);

void dumpConst(IR const* ir, Region const* rg);
void dumpIR(IR const* ir, Region const* rg, CHAR * attr = nullptr,
            UINT dumpflag = IR_DUMP_COMBINE);
void dumpIRListH(IR const* ir_list, Region const* rg, CHAR * attr = nullptr,
                 UINT dumpflag = IR_DUMP_COMBINE);
void dumpIRList(IR const* ir_list, Region const* rg, CHAR * attr = nullptr,
                UINT dumpflag = IR_DUMP_COMBINE);
void dumpIRList(IRList const& ir_list, Region const* rg);
void dumpLabelDecl(LabelInfo const* li, RegionMgr const* rm, bool for_gr);
void dumpLabelName(LabelInfo const* li, RegionMgr const* rm, bool for_gr);

UINT getArithPrecedence(IR_TYPE ty);

inline bool isCommutative(IR_TYPE irt)
{ return IRDES_is_commutative(g_ir_desc[irt]); }

inline bool isBinaryOp(IR_TYPE irt)
{ return IRDES_is_bin(g_ir_desc[irt]); }

inline bool isUnaryOp(IR_TYPE irt)
{ return IRDES_is_una(g_ir_desc[irt]); }

//CASE:_$L9 is non-identifier char because of '$'.
bool isContainNonIdentifierChar(CHAR const* name);

void setParentPointerForIRList(IR * ir_list);
bool verifyIRList(IR * ir, BitSet * irh, Region const* rg);
bool verifySimp(IR * ir, SimpCtx & simp);

} //namespace xoc
#endif
