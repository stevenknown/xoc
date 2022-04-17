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

#define PR_TYPE_CHAR "$"

//Used by all IR.
#define IR_DUMP_DEF 0x0 //default options to dump ir
#define IR_DUMP_KID 0x1 //dump ir's kid
#define IR_DUMP_SRC_LINE 0x2 //dump source line if dbx info is valid.
#define IR_DUMP_ADDR 0x4 //dump host address of each IR
#define IR_DUMP_INNER_REGION 0x8 //dump inner region.
#define IR_DUMP_VAR_DECL 0x10 //dump variable declaration if exist that given
                              //by user.
#define IR_DUMP_NO_NEWLINE 0x20 //Do NOT dump newline
#define IR_DUMP_COMBINE (IR_DUMP_KID|IR_DUMP_SRC_LINE|IR_DUMP_VAR_DECL)

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
//'ir': the root ir of the tree.
//'it': iterator. It should be clean already.
//Readonly function.
IR const* iterInitC(IR const* ir, OUT ConstIRIter & it,
                    bool iter_next = true);

//Iterative access ir tree.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//Readonly function.
IR const* iterNextC(MOD ConstIRIter & it, bool iter_next = true);

//Iterative access the expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//The function is a readonly function.
//Use iterExpNextC to iter next IR.
IR const* iterExpInitC(IR const* ir, OUT ConstIRIter & it,
                       bool iter_next = true);

//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//Readonly function.
inline IR const* iterExpNextC(MOD ConstIRIter & it, bool iter_next = true)
{
    return iterNextC(it, iter_next);
}

//Iterative access the ir tree that start with 'ir'.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it may be either stmt or expression.
//'it': iterator. It should be clean already.
//Note this function is NOT readonly, the returnd IR may be modified.
IR * iterInit(IN IR * ir, OUT IRIter & it, bool iter_next = true);

//Iterative access the ir tree.
//This funtion return the next IR node accroding to 'it'.
//'it': iterator.
//Note this function is NOT readonly, the returnd IR may be modified.
IR * iterNext(MOD IRIter & it, bool iter_next = true);

//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//Use iterExpNextC to iter next IR.
IR * iterExpInit(IR * ir, OUT IRIter & it, bool iter_next = true);

//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//This is a readonly function.
inline IR * iterExpNext(MOD IRIter & it, bool iter_next = true)
{
    return iterNext(it, iter_next);
}

//Iterative access the expression kid of stmt itself.
//This funtion initialize the iterator.
//ir: the stmt ir.
//it: iterator. It should be clean by caller.
//Use iterExpOfStmtNext to iter next IR.
IR * iterExpOfStmtInit(IR * ir, OUT IRIter & it);

//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//Readonly function.
inline IR * iterExpOfStmtNext(MOD IRIter & it)
{
    return iterNext(it, true);
}

//Iterative access the expression kid of stmt itself.
//This funtion initialize the iterator.
//ir: the stmt ir.
//it: iterator. It should be clean by caller.
//Use iterExpOfStmtNextC to iter next IR.
IR const* iterExpOfStmtInitC(IR * ir, OUT ConstIRIter & it);

//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//Readonly function.
inline IR const* iterExpOfStmtNextC(MOD ConstIRIter & it)
{
    return iterNextC(it, true);
}

bool allBeExp(IR * irlst);
bool allBeStmt(IR * irlst);

bool checkMaxIRType();
bool checkIRDesc();
bool checkRoundDesc();

void dumpConst(IR const* ir, Region const* rg);

void dumpIR(IR const* ir, Region const* rg, CHAR * attr = nullptr,
            UINT dumpflag = IR_DUMP_COMBINE);
inline void dumpIR(IR const* ir, Region const* rg, UINT dumpflag)
{
    dumpIR(ir, rg, nullptr, dumpflag);
}
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
