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
    bool allElemBeExp(Region const* rg) const;

    void dump(Region const* rg) const;

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
//ir: the root ir of the tree.
//it: iterator. It should be clean already.
//iter_next: true to iterate the next IR of 'ir'.
//Readonly function.
IR const* iterInitC(IR const* ir, OUT ConstIRIter & it,
                    bool iter_next = true);

//Iterative access ir tree.
//This function return the next IR node accroding to 'it'.
//it: iterator.
//iter_next: true to iterate the next IR of ir in current iteration.
//Readonly function.
IR const* iterNextC(MOD ConstIRIter & it, bool iter_next = true);

//Iterative access the ir tree that start with 'ir'.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it may be either stmt or expression.
//it: iterator. It should be clean already.
//iter_next: true to iterate the next IR of 'ir'.
//Note this function is NOT readonly, the returnd IR may be modified.
IR * iterInit(IN IR * ir, OUT IRIter & it, bool iter_next = true);

//Iterative access the ir tree.
//This funtion return the next IR node accroding to 'it'.
//it: iterator.
//iter_next: true to iterate the next IR of ir in current iteration.
//Note this function is NOT readonly, the returnd IR may be modified.
IR * iterNext(MOD IRIter & it, bool iter_next = true);

//Iterative access the expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//iter_next: true to iterate the next IR of 'ir'.
//The function is a READONLY function.
//Use iterExpNextC to iter next IR.
//Note the function does not iterate inner stmt, e.g:stmts in body of IR_IF.
IR const* iterExpInitC(IR const* ir, OUT ConstIRIter & it);

//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'it'.
//it: iterator.
//iter_next: true to iterate the next IR of ir in current iteration.
//Readonly function.
inline IR const* iterExpNextC(MOD ConstIRIter & it, bool iter_next = true)
{
    return iterNextC(it, iter_next);
}

//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//iter_next: true to iterate the next IR of 'ir'.
//Use iterExpNext to iter next IR.
IR * iterExpInit(IR const* ir, OUT IRIter & it);

//Iterative access the right-hand-side expression of stmt.
//The function return the next IR node accroding to 'it'.
//it: iterator.
//iter_next: true to iterate the next IR of ir in current iteration.
//This is a readonly function.
inline IR * iterExpNext(MOD IRIter & it, bool iter_next = true)
{
    return iterNext(it, iter_next);
}

//Iterative access the expressions that is consist of LHS of stmt itself.
//The funtion initialize the iterator.
//ir: the stmt ir.
//it: iterator. It should be clean by caller.
//Use iterExpOfStmtNext to iter next IR.
//e.g: base expression consist of the access of IST, thus the iteration does
//not include the RHS of IST.
IR * iterExpOfStmtInit(IR * ir, OUT IRIter & it);

//Iterative access the expressions that is consist of LHS of stmt itself.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//Readonly function.
//e.g: base expression consist of the access of IST, thus the iteration does
//not include the RHS of IST.
inline IR * iterExpOfStmtNext(MOD IRIter & it)
{
    return iterNext(it, true);
}

bool allBeExp(IR * irlst);
bool allBeStmt(IR * irlst);

UINT getArithPrecedence(IR_CODE ty);

inline bool isCommutative(IR_CODE irt)
{ return IRDES_is_commutative(g_ir_desc[irt]); }

inline bool isBinaryOp(IR_CODE irt)
{ return IRDES_is_bin(g_ir_desc[irt]); }

inline bool isUnaryOp(IR_CODE irt)
{ return IRDES_is_una(g_ir_desc[irt]); }

//CASE:_$L9 is non-identifier char because of '$'.
bool isContainNonIdentifierChar(CHAR const* name);

void setParentPointerForIRList(IR * ir_list);

bool verifyIRList(IR * ir, BitSet * irh, Region const* rg);
bool verifySimp(IR * ir, SimpCtx & simp);

} //namespace xoc
#endif
