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

IRDesc const g_ir_desc[] = {
    { IR_UNDEF, "undef", 0x0, 0, 0,
      0,},

    { IR_CONST, "const", CConst::kid_map, CConst::kid_num, sizeof(CConst),
      IRT_IS_LEAF,},

    { IR_ID, "id", CId::kid_map, CId::kid_num, sizeof(CId),
      IRT_HAS_IDINFO|IRT_IS_MEM_REF|IRT_IS_LEAF|IRT_IS_NON_PR_MEMREF|
      IRT_IS_MEM_OPND|IRT_HAS_DU },

    { IR_LD, "ld", CLd::kid_map, CLd::kid_num, sizeof(CLd),
      IRT_HAS_IDINFO|IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_LEAF|
      IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },

    { IR_ILD, "ild", CILd::kid_map, CILd::kid_num, sizeof(CILd),
      IRT_IS_UNA|IRT_IS_MEM_REF|IRT_IS_MEM_OPND|
      IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },

    { IR_PR, "pr", CPr::kid_map, CPr::kid_num, sizeof(CPr),
      IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_LEAF|IRT_HAS_DU },

    { IR_ARRAY, "array", CArray::kid_map, CArray::kid_num, sizeof(CArray),
      IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_NON_PR_MEMREF|
      IRT_HAS_DU|IRT_HAS_OFFSET },

    { IR_ST, "st", CSt::kid_map, CSt::kid_num, sizeof(CSt),
      IRT_HAS_IDINFO|IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },

    { IR_STPR, "stpr", CStpr::kid_map, CStpr::kid_num, sizeof(CStpr),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR },

    { IR_STARRAY, "starray", CStArray::kid_map, CStArray::kid_num,
      sizeof(CStArray),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },

    { IR_IST, "ist", CISt::kid_map, CISt::kid_num, sizeof(CISt),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },

    { IR_SETELEM, "setelem", CSetElem::kid_map, CSetElem::kid_num,
      sizeof(CSetElem),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR },

    { IR_GETELEM, "getelem", CGetElem::kid_map, CGetElem::kid_num,
      sizeof(CGetElem),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR },

    //CALL might not def PR if there is not return value.
    { IR_CALL, "call", CCall::kid_map, CCall::kid_num, sizeof(CCall),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_HAS_IDINFO|IRT_IS_STMT_IN_BB|IRT_HAS_DU },

    { IR_ICALL, "icall", CICall::kid_map, CICall::kid_num, sizeof(CICall),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU }, //ICALL might not def PR if
                                      //there is not return value.

    { IR_LDA, "lda", CLda::kid_map, CLda::kid_num, sizeof(CLda),
      IRT_HAS_IDINFO|IRT_IS_UNA|IRT_IS_LEAF|IRT_HAS_OFFSET },

    { IR_ADD, "add", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE },

    { IR_SUB, "sub", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE },

    { IR_MUL, "mul", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE },

    { IR_DIV, "div", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN },

    { IR_REM, "rem", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN },

    { IR_MOD, "mod", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN },

    { IR_LAND, "land", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_LOGICAL },

    { IR_LOR, "lor", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_LOGICAL },

    { IR_BAND, "band", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE },

    { IR_BOR, "bor", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE },

    { IR_XOR, "xor", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE },

    { IR_ASR, "asr", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN },

    { IR_LSR, "lsr", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN },

    { IR_LSL, "lsl", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN },

    { IR_LT, "lt", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION },

    { IR_LE, "le", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION },

    { IR_GT, "gt", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION },

    { IR_GE, "ge", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION },

    { IR_EQ, "eq", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE|IRT_IS_RELATION },

    { IR_NE, "ne", CBin::kid_map, CBin::kid_num, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE|IRT_IS_RELATION },

    { IR_BNOT, "bnot", CUna::kid_map, CUna::kid_num, sizeof(CUna),
      IRT_IS_UNA },

    { IR_LNOT, "lnot", CUna::kid_map, CUna::kid_num, sizeof(CUna),
      IRT_IS_UNA|IRT_IS_LOGICAL },

    { IR_NEG, "neg", CUna::kid_map, CUna::kid_num, sizeof(CUna),
      IRT_IS_UNA },

    { IR_CVT, "cvt", CCvt::kid_map, CCvt::kid_num, sizeof(CCvt),
      IRT_IS_UNA },

    { IR_GOTO, "goto", CGoto::kid_map, CGoto::kid_num, sizeof(CGoto),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },

    { IR_IGOTO, "igoto", CIGoto::kid_map, CIGoto::kid_num, sizeof(CIGoto),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },

    { IR_DO_WHILE, "dowhile", CDoWhile::kid_map, CDoWhile::kid_num,
      sizeof(CDoWhile), IRT_IS_STMT },

    { IR_WHILE_DO, "whiledo", CWhileDo::kid_map, CWhileDo::kid_num,
      sizeof(CWhileDo), IRT_IS_STMT },

    { IR_DO_LOOP, "doloop", CDoLoop::kid_map, CDoLoop::kid_num,
       sizeof(CDoLoop), IRT_IS_STMT },

    { IR_IF, "if", CIf::kid_map, CIf::kid_num, sizeof(CIf),
      IRT_IS_STMT },

    { IR_LABEL, "label", CLab::kid_map, CLab::kid_num, sizeof(CLab),
      IRT_IS_STMT },

    { IR_SWITCH, "switch", CSwitch::kid_map, CSwitch::kid_num, sizeof(CSwitch),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },

    { IR_CASE, "case", CCase::kid_map, CCase::kid_num, sizeof(CCase),
      0, },

    { IR_TRUEBR, "truebr", CTruebr::kid_map, CTruebr::kid_num, sizeof(CTruebr),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },

    { IR_FALSEBR, "falsebr", CFalsebr::kid_map, CFalsebr::kid_num,
      sizeof(CFalsebr), IRT_IS_STMT|IRT_IS_STMT_IN_BB },

    { IR_RETURN, "return", CRet::kid_map, CRet::kid_num, sizeof(CRet),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },

    { IR_SELECT, "select", CSelect::kid_map, CSelect::kid_num, sizeof(CSelect),
      0, },

    { IR_BREAK, "break", CBreak::kid_map, CBreak::kid_num, sizeof(CBreak),
      IRT_IS_STMT },

    { IR_CONTINUE, "continue", CContinue::kid_map, CContinue::kid_num,
      sizeof(CContinue), IRT_IS_STMT },

    { IR_PHI, "phi", CPhi::kid_map, CPhi::kid_num, sizeof(CPhi),
      IRT_IS_STMT|IRT_HAS_RESULT|IRT_IS_MEM_REF|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR },

    { IR_REGION, "region", CRegion::kid_map, CRegion::kid_num, sizeof(CRegion),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },

    { IR_TYPE_NUM, "LAST IR Code", 0x0, 0, 0, 0, },
};

RoundDesc const g_round_desc[] = {
    { ROUND_UNDEF, "undef" },
    { ROUND_DOWN, "down" },
    { ROUND_UP, "up" },
    { ROUND_TOWARDS_ZERO, "twards_zero" },
    { ROUND_AWAY_FROM_ZERO, "awayfromzero" },
    { ROUND_TO_NEAREST_INTEGER, "tonearestinteger" },
    { ROUND_HALF_UP, "halfup" },
    { ROUND_HALF_DOWN, "halfdown" },
    { ROUND_HALF_TOWARDS_ZERO, "halftowardszero" },
    { ROUND_HALF_AWAY_FROM_ZERO, "halfawayfromzero" },
    { ROUND_HALF_TO_EVEN, "halftoeven" },
    { ROUND_HALF_TO_ODD, "halftoodd" },
};

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


INT checkKidNumIRtype(IR const* ir, UINT n, IR_TYPE irty,
                      CHAR const* filename, INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->getCode() == irty);
    return n;
}


IR const* checkIRT(IR const* ir, IR_TYPE irt)
{
    ASSERTN(ir->getCode() == irt, ("current ir is not '%s'", IRTNAME(irt)));
    return ir;
}


IR const* checkIRTBranch(IR const* ir)
{
    ASSERT0(ir->isConditionalBr());
    return ir;
}


IR const* checkIRTCall(IR const* ir)
{
    ASSERT0(ir->isCallStmt());
    return ir;
}


IR const* checkIRTArray(IR const* ir)
{
    ASSERT0(ir->is_array() || ir->is_starray());
    return ir;
}


IR const* checkIRTOnlyCall(IR const* ir)
{
    ASSERT0(ir->is_call());
    return ir;
}


IR const* checkIRTOnlyICall(IR const* ir)
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
bool IRDesc::mustExist(IR_TYPE irtype, UINT kididx)
{
    return HAVE_FLAG(IRDES_kid_map(g_ir_desc[irtype]), 1 << kididx);
}
//END IRDesc


void setParentPointerForIRList(IR * ir_list)
{
    while (ir_list != nullptr) {
        ir_list->setParentPointer(true);
        ir_list = IR_next(ir_list);
    }
}


//Return the arthmetic precedence.
UINT getArithPrecedence(IR_TYPE ty)
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
    case IR_ST:
    case IR_STPR:
    case IR_STARRAY:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_IST:
    case IR_CALL:
    case IR_ICALL:
        p = 15;
        break;
    default: ASSERTN(0, ("TODO"));
    }
    return p;
}


bool CRegion::is_readonly() const
{
    ASSERT0(rg);
    return rg->is_readonly();
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
        size += IRTSIZE(k->getCode());
    }
    return size;
}


//Check that IR cannot take a UNDEF type.
bool IR::verify(Region const* rg) const
{
    verifyKids();

    TypeMgr const* tm = rg->getTypeMgr();
    ASSERT0(tm);
    DUMMYUSE(tm);

    Type const* d = getType();
    ASSERT0(d && d->verify(tm));
    if (d->is_vector()) {
        TMWORD ofst = getOffset();
        if (ofst != 0) {
            ASSERT0((ofst % tm->getDTypeByteSize(d->getVectorElemType())) == 0);
        }
    }

    switch (getCode()) {
    case IR_UNDEF: ASSERTN(0, ("should not be undef")); break;
    case IR_CONST:
        ASSERT0(d);
        ASSERTN(d->getDType() != D_UNDEF, ("size of load value cannot be zero"));
        if (!is_sint() &&
            !is_uint() &&
            !is_fp() &&
            !is_bool() &&
            !is_mc() &&
            !is_any() &&
            !is_ptr() && //immediate can be pointer, e.g: int * p = 0;
            !is_str()) {
            ASSERTN(0, ("unsupport immediate value DATA_TYPE:%d", getDType()));
        }
        break;
    case IR_ID: break;
    case IR_LD:
        //src of LD might be small or big compare with REG.
        ASSERT0(LD_idinfo(this));
        ASSERT0(d);
        ASSERTN(d->getDType() != D_UNDEF, ("size of load value cannot be zero"));
        break;
    case IR_ST:
        ASSERT0(d);
        ASSERTN(d->getDType()!= D_UNDEF, ("size of store value cannot be zero"));
        ASSERT0(ST_idinfo(this));
        ASSERT0(ST_rhs(this));
        ASSERT0(ST_rhs(this)->is_exp());
        ASSERT0(ST_rhs(this)->is_single());
        if (d->is_vector()) {
            ASSERT0(d->getVectorElemType() != D_UNDEF);
            ASSERT0(tm->getDTypeByteSize(d->getVectorElemType()) >=
                    tm->getByteSize(ST_rhs(this)->getType()));
        }
        break;
    case IR_STPR:
        ASSERT0(d);
        ASSERTN(getDType() != D_UNDEF, ("size of store value cannot be zero"));
        ASSERT0(getDType() != D_UNDEF);
        ASSERT0(STPR_no(this) > 0);
        ASSERT0(STPR_rhs(this));
        ASSERT0(STPR_rhs(this)->is_exp());
        ASSERT0(STPR_rhs(this)->is_single());
        break;
    case IR_ILD:
        ASSERT0(d);
        ASSERT0(getDType() != D_UNDEF);
        ASSERT0(ILD_base(this));
        if (!g_is_support_dynamic_type) {
            ASSERTN(ILD_base(this)->is_ptr(), ("base must be pointer"));
            ASSERT0(tm->getPointerBaseByteSize(ILD_base(this)->getType()) > 0);
        }
        ASSERT0(ILD_base(this)->is_exp());
        ASSERT0(ILD_base(this)->is_single());
        break;
    case IR_IST:
        ASSERT0(d);
        if (!g_is_support_dynamic_type) {
            ASSERTN(IST_base(this)->is_ptr(), ("base must be pointer"));
            ASSERT0(tm->getPointerBaseByteSize(IST_base(this)->getType()) > 0);
        }
        ASSERT0(IST_rhs(this));
        ASSERT0(IST_rhs(this)->is_exp());
        ASSERTN(getDType() != D_UNDEF, ("size of istore value cannot be zero"));
        ASSERT0(IST_base(this)->is_single());
        ASSERT0(IST_rhs(this)->is_single());
        break;
    case IR_SETELEM:
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        ASSERT0(SETELEM_base(this) && SETELEM_val(this) && SETELEM_ofst(this));
        if (d->is_vector()) {
            ASSERT0(d->getVectorElemType() != D_UNDEF);

            //Note if the value size less than elemsize, it will be hoist to
            //elemsize.
            ASSERT0(tm->getDTypeByteSize(d->getVectorElemType()) >=
                    tm->getByteSize(SETELEM_val(this)->getType()));
        }
        ASSERT0(SETELEM_base(this)->is_exp());
        ASSERT0(SETELEM_base(this)->is_single());
        ASSERT0(SETELEM_val(this)->is_exp());
        ASSERT0(SETELEM_val(this)->is_single());
        ASSERT0(SETELEM_ofst(this)->is_exp());
        ASSERT0(SETELEM_ofst(this)->is_single());
        break;
    case IR_GETELEM: {
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        IR const* base = GETELEM_base(this);
        ASSERT0(base && GETELEM_ofst(this));
        Type const* basedtd = base->getType();
        if (basedtd->is_vector()) {
            ASSERT0(basedtd->getVectorElemType() != D_UNDEF);
            ASSERT0(tm->getDTypeByteSize(basedtd->getVectorElemType()) >=
                    tm->getByteSize(d));
        }
        break;
    }
    case IR_LDA:
        ASSERT0(LDA_idinfo(this));
        ASSERT0(d);
        ASSERTN(d->getDType() != D_UNDEF, ("size of load value cannot be zero"));
        ASSERT0(d->is_pointer());
        //Lda base can be general Var, label, const string.
        ASSERTN(!VAR_is_fake(LDA_idinfo(this)), ("LDA's base must be effect Var"));
        break;
    case IR_CALL:
        ASSERT0(CALL_idinfo(this));

        //result type of call is the type of return value if it exist.
        //The result type may be ANY.
        if (CALL_prno(this) != PRNO_UNDEF) { ASSERT0(d); }

        //Parameters should be expression.
        for (IR * p = CALL_param_list(this); p != nullptr; p = p->get_next()) {
            ASSERT0(p->is_exp());
        }

        //Dummy uses should be expression.
        for (IR * p = CALL_dummyuse(this); p != nullptr; p = p->get_next()) {
            ASSERT0(p->is_exp());
        }
        break;
    case IR_ICALL:
        //result type of call is the type of return value if it exist.
        //Note return-value type may be ANY.

        //rtype of icall is the type of IR in return-value-list.
        ASSERT0(ICALL_callee(this) && ICALL_callee(this)->isPtr());
        ASSERT0(ICALL_callee(this)->is_single());

        for (IR * p = CALL_param_list(this); p != nullptr; p = p->get_next()) {
            ASSERT0(p->is_exp());
        }
        break;
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        ASSERT0(BIN_opnd0(this) && BIN_opnd0(this)->is_exp() &&
                BIN_opnd1(this) && BIN_opnd1(this)->is_exp());

        //Check that shift operations only have integer type.
        ASSERT0(is_int() && //the result must be integer type.
                BIN_opnd0(this)->is_int() &&
                BIN_opnd1(this)->is_int());

        ASSERT0(BIN_opnd0(this)->is_single());
        ASSERT0(BIN_opnd1(this)->is_single());
        break;
    case IR_ADD:
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        ASSERT0(BIN_opnd0(this) && BIN_opnd0(this)->is_exp() &&
                BIN_opnd1(this) && BIN_opnd1(this)->is_exp());
        ASSERT0(BIN_opnd0(this)->is_single());
        ASSERT0(BIN_opnd1(this)->is_single());

        //Opnd1 can not be pointer. e.g: &p-&q
        ASSERT0(!BIN_opnd1(this)->is_ptr());
        break;
    case IR_LAND:
    case IR_LOR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
        ASSERT0(d);
        ASSERT0(is_bool());
        ASSERT0(BIN_opnd0(this) && BIN_opnd0(this)->is_exp() &&
                BIN_opnd1(this) && BIN_opnd1(this)->is_exp());
        ASSERT0(((CBin*)this)->getOpnd0()->is_single());
        ASSERT0(((CBin*)this)->getOpnd1()->is_single());
        break;
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_XOR:
    case IR_BAND:
    case IR_BOR:
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        ASSERT0(BIN_opnd0(this) && BIN_opnd0(this)->is_exp() &&
                BIN_opnd1(this) && BIN_opnd1(this)->is_exp());
        ASSERT0(BIN_opnd0(this)->is_single());
        ASSERT0(BIN_opnd1(this)->is_single());
        break;
    case IR_LNOT:
        ASSERT0(d);
        ASSERT0(is_bool());
        ASSERT0(UNA_opnd(this) && UNA_opnd(this)->is_exp());
        ASSERT0(UNA_opnd(this)->is_single());
        break;
    case IR_BNOT:
    case IR_NEG:
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        ASSERT0(UNA_opnd(this) && UNA_opnd(this)->is_exp());
        ASSERT0(UNA_opnd(this)->is_single());
        break;
    case IR_GOTO:
        ASSERT0(GOTO_lab(this));
        break;
    case IR_IGOTO:
        ASSERTN(IGOTO_vexp(this), ("igoto vexp can not be nullptr."));
        ASSERTN(IGOTO_case_list(this), ("igoto case list can not be nullptr."));
        ASSERTN(IGOTO_vexp(this)->is_single(),
               ("igoto vexp can NOT be in list."));
        break;
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
        ASSERT0(LOOP_det(this) && LOOP_det(this)->is_judge());
        ASSERT0(LOOP_det(this)->is_single());
        if (LOOP_body(this)) {
            ASSERT0(allBeStmt(LOOP_body(this)));
        }
        break;
    case IR_IF:
        ASSERT0(IF_det(this) && IF_det(this)->is_judge());
        ASSERT0(IF_det(this)->is_single());
        if (IF_truebody(this)) {
            ASSERT0(allBeStmt(IF_truebody(this)));
        }
        if (IF_falsebody(this)) {
            ASSERT0(allBeStmt(IF_falsebody(this)));
        }
        //Fall through.
    case IR_LABEL:
        break;
    case IR_SWITCH:
        ASSERTN(SWITCH_vexp(this), ("switch vexp can not be nullptr."));
        ASSERT0(SWITCH_vexp(this)->is_exp());

        //SWITCH case list can be nullptr.
        ASSERT0(SWITCH_vexp(this)->is_single());
        if (SWITCH_body(this)) {
            ASSERT0(allBeStmt(SWITCH_body(this)));
        }
        break;
    case IR_CASE:
        ASSERT0(CASE_lab(this));
        ASSERT0(CASE_vexp(this)->is_single());
        ASSERTN(CASE_vexp(this)->is_const(),
               ("case value-expression must be const"));
        break;
    case IR_STARRAY:
    case IR_ARRAY:
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        ASSERT0(ARR_base(this)->isPtr());
        ASSERT0(ARR_elemtype(this));
        if (ARR_ofst(this) != 0 && !ARR_elemtype(this)->is_any()) {
            UINT elem_data_size = tm->getByteSize(ARR_elemtype(this));
            UINT result_data_size = tm->getByteSize(d);
            DUMMYUSE(elem_data_size | result_data_size);
            ASSERTN(result_data_size + ARR_ofst(this) <= elem_data_size,
                ("result data size should be less than element data size"));
        }
        ASSERT0(ARR_base(this)->is_single());
        ASSERTN(ARR_sub_list(this), ("subscript expression can not be null"));
        ASSERT0(allBeExp(ARR_sub_list(this)));
        if (getCode() == IR_STARRAY) {
            ASSERT0(STARR_rhs(this));
            ASSERT0(STARR_rhs(this)->is_exp());
            ASSERT0(STARR_rhs(this)->is_single());
        }
        break;
    case IR_CVT:
        ASSERT0(d);
        ASSERT0(CVT_exp(this) != nullptr && CVT_exp(this)->is_exp());
        ASSERT0(CVT_exp(this)->is_single());
        break;
    case IR_PR:
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        ASSERT0(getDType() != D_UNDEF);
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        ASSERT0(BR_lab(this));
        ASSERT0(BR_det(this) && BR_det(this)->is_judge());
        ASSERT0(BR_det(this)->is_single());
        break;
    case IR_RETURN:
        if (RET_exp(this) != nullptr) {
            ASSERT0(RET_exp(this)->is_exp());
            ASSERT0(RET_exp(this)->is_single());
        }
        break;
    case IR_SELECT:
        //true_exp's type might not equal to false_exp's.
        ASSERT0(d && d->getDType() != D_UNDEF);
        ASSERT0(SELECT_pred(this) &&
                SELECT_pred(this)->is_bool() &&
                SELECT_pred(this)->is_single());

        ASSERT0(SELECT_trueexp(this) &&
                SELECT_trueexp(this)->is_exp() &&
                SELECT_trueexp(this)->is_single());

        ASSERT0(SELECT_falseexp(this) &&
                SELECT_falseexp(this)->is_exp() &&
                SELECT_falseexp(this)->is_single());
        break;
    case IR_BREAK:
    case IR_CONTINUE:
        break;
    case IR_PHI:
        ASSERT0(d);
        ASSERT0(d->getDType() != D_UNDEF);
        ASSERT0(getDType() != D_UNDEF);
        ASSERT0(PHI_prno(this) > 0);

        //PHI must have at least one opnd.
        ASSERT0(PHI_opnd_list(this) != nullptr);
        //To convenient for cfg optimization, do not verify PHI here, and
        //more verification of PHI should be done at PRSSAMgr.
        //ASSERT0(verifyPhi(rg));
        break;
    case IR_REGION: break;
    default: UNREACHABLE();
    }
    return true;
}


bool IR::verifyKids() const
{
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * k = getKid(i);
        if (k != nullptr) {
            ASSERT0(IR_parent(k) == this);
        }
        if (!IRDesc::mustExist(getCode(), i)) {
            ASSERTN(k == nullptr,
                    ("IR_%s does not have No.%d kid", IRNAME(this), i));
        } else {
            //Here, ith kid cannot be nullptr.
            //CASE: Kind of node permit some of their kid to be nullptr.
            //For now include IR_IF, IR_RETURN, IR_DO_LOOP, etc. */
            if (k == nullptr) {
                switch (getCode()) {
                case IR_IF:
                case IR_DO_LOOP:
                case IR_WHILE_DO:
                case IR_SWITCH:
                case IR_DO_WHILE:
                    if (i == 0) {
                        ASSERTN(k != nullptr,
                                ("IR_%s miss kid%d", IRNAME(this), i));
                    }
                    break;
                case IR_ICALL:
                    if (i == 2) {
                        ASSERTN(k != nullptr,
                                ("IR_%s miss kid%d", IRNAME(this), i));
                    }
                    break;
                case IR_RETURN:
                case IR_CALL:
                    break;
                default:
                    ASSERTN(k != nullptr,
                            ("IR_%s miss kid%d", IRNAME(this), i));
                }
            }
        }
    }
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
    if (!is_array() && !is_starray()) { return false; }
    if (ARR_elem_num_buf(this) == nullptr) { return false; }

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
//'is_cmp_kid': it is true if comparing kids as well.
bool IR::isIRListEqual(IR const* irs, bool is_cmp_kid) const
{
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
    if ((ARR_elem_num_buf(this) == nullptr) || (ARR_elem_num_buf(ir) == nullptr)) {
        //Array is any dimension.
        return false;
    }
    UINT dim = 0;
    for (IR const* s = ARR_sub_list(this); s != nullptr; s = s->get_next()) {
        dim++;
        if (((CArray*)this)->getElementNumOfDim(dim) !=
            ((CArray*)ir)->getElementNumOfDim(dim)) {
            return false;
        }
    }
    return true;
}


//Return true if IR tree is exactly congruent, or
//they are parity memory reference.
bool IR::isMemRefEqual(IR const* src) const
{
    ASSERTN(isMemoryRef() && src->isMemoryRef(), ("Not memory expression"));
    if (isIREqual(src, true)) { return true; }

    switch (getCode()) {
    case IR_LD:
        if (src->is_st() &&
            LD_idinfo(this) == ST_idinfo(src) &&
            LD_ofst(this) == ST_ofst(src) &&
            getType() == src->getType()) {
            return true;
        }
        return false;
    case IR_ST:
        if (src->is_ld() &&
            LD_idinfo(src) == ST_idinfo(this) &&
            LD_ofst(src) == ST_ofst(this) &&
            getType() == src->getType()) {
            return true;
        }
        return false;
    case IR_ILD:
        if (src->is_ist() &&
            ILD_base(this)->isIREqual(IST_base(src), true) &&
            ILD_ofst(this) == IST_ofst(src) &&
            getType() == src->getType()) {
            return true;
        }
        return false;
    case IR_IST:
        if (src->is_ild() &&
            ILD_base(src)->isIREqual(IST_base(this), true) &&
            ILD_ofst(src) == IST_ofst(this) &&
            getType() == src->getType()) {
            return true;
        }
        return false;
    case IR_ARRAY:
        if (src->is_starray() &&
            ARR_base(src)->isIREqual(ARR_base(this), true) &&
            ARR_ofst(src) == ARR_ofst(this) &&
            ARR_elemtype(src) == ARR_elemtype(this) &&
            getType() == src->getType()) {
            if ((ARR_sub_list(src) != nullptr) ^ (ARR_sub_list(this) != nullptr)) {
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
    case IR_STARRAY:
        if (src->is_array() &&
            ARR_base(src)->isIREqual(ARR_base(this), true) &&
            ARR_ofst(src) == ARR_ofst(this) &&
            ARR_elemtype(src) == ARR_elemtype(this) &&
            getType() == src->getType()) {
            if ((ARR_sub_list(src) != nullptr) ^ (ARR_sub_list(this) != nullptr)) {
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
                TMWORD dimnum = ((CStArray*)this)->getDimNum();
                ASSERT0(((CArray*)src)->getDimNum() == dimnum);
                for (UINT i = 0; i < dimnum; i++) {
                    if (((CStArray*)this)->getElementNumOfDim(i) !=
                        ((CArray*)src)->getElementNumOfDim(i)) {
                        return false;
                    }
                }
            }
            return true;
        }
        return false;
    case IR_PR:
    case IR_STPR:
    case IR_CALL:
    case IR_ICALL:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_PHI:
        if (src->isPROp()) {
            return getPrno() == src->getPrno();
        }
        return false;
    default: UNREACHABLE();
    }
    return false;
}


static bool isIRIsomorphic(IR const* ir, IR const* src,
                           bool is_cmp_kid, bool identical)
{
    if (ir == src) { return true; }
    if (identical && ir->getCode() != src->getCode()) { return false; }
    switch (src->getCode()) {
    case IR_CONST: //Constant value: include integer, float, string.
        if (CONST_int_val(ir) != CONST_int_val(src)) { return false; }
        break;
    case IR_ID:
        if (ID_info(ir) != ID_info(src)) { return false; }
        break;
     case IR_ST:
    case IR_LD:
        if (!ir->is_st() && !ir->is_ld()) { return false; }
        if (ir->getIdinfo() != src->getIdinfo() ||
            ir->getOffset() != src->getOffset() ||
            ir->getType() != src->getType()) {
            return false;
        }
        break;
    case IR_IST:
    case IR_ILD:
        if (!ir->is_ist() && !ir->is_ild()) { return false; }
        if (ir->getOffset() != src->getOffset() ||
            ir->getType() != src->getType()) {
            return false;
        }
        break;
    case IR_STPR:
    case IR_PR:
        if (!ir->is_stpr() && !ir->is_pr()) { return false; }
        if (ir->getType() != src->getType() ||
            ir->getPrno() != src->getPrno()) {
            return false;
        }
        break;
    case IR_STARRAY:
    case IR_ARRAY:
        if (!ir->is_starray() && !ir->is_array()) { return false; }
        if (ARR_ofst(ir) != ARR_ofst(src) ||
            ir->getType() != src->getType()) {
            return false;
        }
        if ((ARR_elem_num_buf(src) != nullptr) ^
            (ARR_elem_num_buf(ir) != nullptr)) {
            return false;
        }
        if (ARR_elem_num_buf(src) != nullptr) {
            ASSERT0(ARR_elem_num_buf(ir));
            TMWORD dimnum = ((CArray*)ir)->getDimNum();
            if (((CArray*)src)->getDimNum() != dimnum) { return false; }
            for (UINT i = 0; i < dimnum; i++) {
                if (((CArray*)ir)->getElementNumOfDim(i) !=
                    ((CArray*)src)->getElementNumOfDim(i)) {
                    return false;
                }
            }
        }
        break;
    case IR_LDA:
        if (LDA_idinfo(ir) != LDA_idinfo(src) ||
            LDA_ofst(ir) != LDA_ofst(src) ||
            ir->getType() != src->getType()) {
            return false;
        }
        break;
    case IR_CALL:
        if (CALL_idinfo(ir) != CALL_idinfo(src)) { return false; }
        break;
    case IR_ICALL:
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        if (ir->getType() != src->getType()) { return false; }
        break;
    case IR_GOTO:
    case IR_IGOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
        break;
    case IR_LABEL:
        if (LAB_lab(ir) != LAB_lab(src)) { return false; }
        break;
    case IR_SWITCH:
    case IR_CASE:
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SELECT:
        if (ir->getType() != src->getType()) {
            return false;
        }
        break;
    case IR_RETURN:
        break;
    case IR_BREAK:
    case IR_CONTINUE:
        return true;
    case IR_PHI:
        return true;
    case IR_REGION:
        //One should implement comparation function for your own region.
        return false;
    default: UNREACHABLE();
    }

    if (!is_cmp_kid) { return true; }

    //Compare kids.
    for (INT i = 0;
         i < IR_MAX_KID_NUM(ir) && i < IR_MAX_KID_NUM(src); i++) {
        IR * kid1 = ir->getKid(i);
        IR * kid2 = src->getKid(i);

        if (src->isCallStmt() &&
            (kid1 == CALL_dummyuse(ir) ||
             kid2 == CALL_dummyuse(src))) {
            //Do NOT check the equality of dummyuses.
            continue;
        }

        if ((kid1 != nullptr) ^ (kid2 != nullptr)) {
            return false;
        }
        if (kid1 != nullptr && !kid1->isIRListEqual(kid2, is_cmp_kid)) {
            return false;
        }
    }
    return true;
}


//Return true if current ir tree is isomorphic to src.
//src: root of IR tree.
//is_cmp_kid: it is true if comparing kids as well.
//Note the function does not compare the siblings of 'src'.
bool IR::isIRIsomo(IR const* src, bool is_cmp_kid) const
{
    return isIRIsomorphic(this, src, is_cmp_kid, false);
}


//Return true if current ir tree is equivalent to src.
//src: root of IR tree.
//is_cmp_kid: it is true if comparing kids as well.
//Note the function does not compare the siblings of 'src'.
bool IR::isIREqual(IR const* src, bool is_cmp_kid) const
{
    return isIRIsomorphic(this, src, is_cmp_kid, true);
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
IR * IR::getOpndPRList(UINT prno) const
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
IR * IR::getOpndPR(UINT prno, IRIter & it) const
{
    ASSERT0(is_stmt());
    it.clean();
    for (IR * k = xoc::iterInit(const_cast<IR*>(this), it);
         k != nullptr; k = xoc::iterNext(it)) {
        if (k->is_pr() && PR_no(k) == prno && is_rhs(k)) {
            return k;
        }
    }
    return nullptr;
}


//This function recursively iterate the IR tree to
//retrieve the PR whose PR_no is equal to given 'prno'.
//Otherwise return nullptr.
IR * IR::getOpndPR(UINT prno) const
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
    if (isMemoryOpnd() && getRefMD() == md) {
        return const_cast<IR*>(this);
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid = getKid(i);
        if (kid != nullptr && kid->isMemoryRef() && kid->getRefMD() == md) {
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
IR * IR::getResultPR(UINT prno)
{
    switch (getCode()) {
    case IR_STPR:
        if (STPR_no(this) == prno) { return this; }
        return nullptr;
    case IR_SETELEM:
        if (SETELEM_prno(this) == prno) { return this; }
        return nullptr;
    case IR_GETELEM:
        if (GETELEM_prno(this) == prno) { return this; }
        return nullptr;
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
    case IR_GOTO:
    case IR_IGOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_LABEL:
    case IR_SWITCH:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_BREAK:
    case IR_CONTINUE:
    case IR_REGION:
        return nullptr;
    case IR_CALL:
    case IR_ICALL:
        if (CALL_prno(this) == prno) { return this; }
        return nullptr;
    case IR_PHI:
        if (PHI_prno(this) == prno) { return this; }
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
    IR * newop0 = rg->buildLogicalNot(BIN_opnd0(this));
    IR * newop1 = rg->buildLogicalNot(BIN_opnd1(this));
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
    IR * newop0 = rg->buildLogicalNot(BIN_opnd0(this));
    IR * newop1 = rg->buildLogicalNot(BIN_opnd1(this));
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


//This function only handle Call/ICall stmt, it find PR and remove
//them out of UseSet.
//Note this function does not maintain DU chain between call and its use.
void IR::removePRFromUseset(Region * rg)
{
    ASSERT0(isCallStmt() && rg);
    DUMgr * dumgr = rg->getDUMgr();
    DUSet * useset = getDUSet();
    if (useset == nullptr) { return; }

    DefMiscBitSetMgr * sbs_mgr = dumgr->getSBSMgr();
    DUSetIter di = nullptr;
    INT lnext = -1;
    for (INT i = useset->get_first(&di); i >= 0; i = lnext) {
        lnext = useset->get_next(i, &di);
        IR const* exp = rg->getIR(i);
        ASSERT0(exp->is_exp());
        if (!exp->isReadPR()) { continue; }

        ASSERT0(dumgr);
        useset->remove(i, *sbs_mgr);
    }
}


static void removeSSAUseRecur(IR * ir)
{
    if (ir->is_stmt()) {
        SSAInfo * ssainfo = ir->getSSAInfo();
        if (ssainfo != nullptr) {
            ssainfo->cleanDU();
        }
    } else {
        SSAInfo * ssainfo = ir->getSSAInfo();
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
    ASSERTN(isMemoryRef() && src->isMemoryRef(), ("not memory reference"));
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
                mds->dump(rg->getMDSystem());
            }
        }
    }

    if (isCallStmt()) {
        bool doit = false;
        CallGraph * callg = rg->getRegionMgr()->getCallGraph();
        if (callg != nullptr) {
            Region * callee = callg->mapCall2Region(this, rg);
            if (callee != nullptr && callee->is_ref_valid()) {
                MDSet const* muse = callee->getMayUse();
                //May use
                prt(rg, " <-- ");
                if (muse != nullptr && !muse->is_empty()) {
                    muse->dump(callee->getMDSystem());
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
                x->dump(rg->getMDSystem());
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


//Return true if current ir and ir2 represent different memory location,
//otherwise return false to tell caller we do not know more about these object.
//Note this function will consider data type that current ir or ir2
//referrenced.
bool IR::isDiffMemLoc(IR const* ir2) const
{
    IR const* ir1 = this;
    ASSERT0(ir1 && ir2);
    if (ir1 == ir2) { return false; }
    if ((ir1->is_st() || ir1->is_ld()) && (ir2->is_st() || ir2->is_ld())) {
        return ir1->isNotOverlapViaMDRef(ir2);

        //UINT tysz1 = ir1->getTypeSize(m_tm);
        //UINT tysz2 = ir2->getTypeSize(m_tm);
        //UINT ofst1 = ir1->getOffset();
        //UINT ofst2 = ir2->getOffset();
        //if ((((ofst1 + tysz1) <= ofst2) || ((ofst2 + tysz2) <= ofst1))) {
        //    return true;
        //}
        //return false;
    }
    if (ir1->isIndirectMemOp() && ir2->isIndirectMemOp()) {
        return ir1->isNotOverlapViaMDRef(ir2);
    }
    if (ir1->isArrayOp() && ir2->isArrayOp()) {
        IR const* base1 = ARR_base(ir1);
        IR const* base2 = ARR_base(ir2);
        if (base1->is_lda() &&
            base2->is_lda() &&
            base1->getIdinfo() != base2->getIdinfo()) {
            return true;
        }
        return ir1->isNotOverlapViaMDRef(ir2);
    }
    return ir1->isNotOverlapViaMDRef(ir2);
}


//Return true if current ir does not overlap to ir2.
//ir2: stmt or expression to be compared.
//Note this function is different to isNotOverlap(), it determines overlapping
//through MD and MDSet references.
bool IR::isNotOverlapViaMDRef(IR const* ir2) const
{
    MD const* must1 = getRefMD();
    MD const* must2 = ir2->getRefMD();
    MDSet const* may1 = getRefMDSet();
    MDSet const* may2 = ir2->getRefMDSet();
    if (must1 != nullptr && must2 != nullptr) {
        if (!must1->is_may() && !must2->is_may()) {
            //CASE: the MustRef of ID may be GLOBAL_MEM or IMPORT_MEM.
            return !(must1 == must2 || must1->is_overlap(must2));
        }
        //Need to do more judgement.
    }
    if (must1 != nullptr && may2 != nullptr &&
        may2->is_contain_only_taken_addr(must1)) {
        return false;
    }
    if (must2 != nullptr && may1 != nullptr &&
        may1->is_contain_only_taken_addr(must2)) {
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
    if (!ir1->getType()->is_scalar() | !ir2->getType()->is_scalar() ||
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
void IR::setPrnoConsiderSSAInfo(UINT prno)
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
    if (isMemoryRef()) {
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
    switch (getCode()) {
    case IR_ST:
        ST_rhs(this) = rhs;
        if (rhs != nullptr) {
            IR_parent(rhs) = this;
        }
        return;
    case IR_STPR:
        STPR_rhs(this) = rhs;
        if (rhs != nullptr) {
            IR_parent(rhs) = this;
        }
        return;
    case IR_STARRAY:
        STARR_rhs(this) = rhs;
        if (rhs != nullptr) {
            IR_parent(rhs) = this;
        }
        return;
    case IR_IST:
        IST_rhs(this) = rhs;
        if (rhs != nullptr) {
            IR_parent(rhs) = this;
        }
        return;
    default: ASSERTN(0, ("not store operation."));
    }
}


//This function recursively iterate the IR tree to
//retrieve whether the IR is may-throw.
//Record if ir might throw exception.
bool IR::isMayThrow(bool recur) const
{
    if (IR_may_throw(this)) { return true; }
    if (!recur) { return false; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR const* tmp = getKid(i);
        if (tmp == nullptr) { continue; }
        if (IR_may_throw(tmp)) {
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


//Return true if ir is branch op and has multiple jump target.
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
        if (VAR_is_readonly(LD_idinfo(this)) &&
            !VAR_is_volatile(LD_idinfo(this))) {
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
        return VAR_is_volatile(id_info);
    }
    return false;
}




//END IR


//
//START CLd
//
IR * CLd::dupIRTreeByStmt(IR const* src, Region * rg)
{
    ASSERT0(src->is_st());
    IR * ld = rg->buildLoad(ST_idinfo(src), src->getOffset(), src->getType());
    ld->copyRef(src, rg);
    return ld;
}
//END CLd


//
//START CPr
//
IR * CPr::dupIRTreeByRef(IR const* src, Region * rg)
{
    ASSERT0(src->isPROp());
    IR * pr = rg->buildPRdedicated(src->getPrno(), src->getType());
    pr->copyRef(src, rg);
    return pr;
}
//END CPr


//
//START CILd
//
IR * CILd::dupIRTreeByStmt(IR const* src, Region * rg)
{
    ASSERT0(src->is_ist());
    IR * ild = rg->buildILoad(rg->dupIRTree(IST_base(src)),
                              IST_ofst(src), src->getType());
    ild->copyRef(src, rg);
    ILD_base(ild)->copyRefForTree(IST_base(src), rg);
    return ild;
}
//END CILd


//
//START CArray
//
IR * CArray::dupIRTreeByStmt(IR const* src, Region * rg)
{
    ASSERT0(src->is_starray());
    IR * arr = rg->buildArray(rg->dupIRTree(ARR_base(src)),
                              rg->dupIRTreeList(ARR_sub_list(src)),
                              src->getType(), ARR_elemtype(src),
                              ((CStArray*)src)->getDimNum(),
                              ARR_elem_num_buf(src));
    arr->setOffset(src->getOffset());
    arr->copyRef(src, rg);
    ARR_base(arr)->copyRefForTree(ARR_base(src), rg);
    ARR_sub_list(arr)->copyRefForTree(ARR_sub_list(src), rg);
    return arr;
}
//END CArray


//
//START CStArray
//
IR * CStArray::dupIRTreeByExp(IR const* src, IR * rhs, Region * rg)
{
    ASSERT0(src->is_array());
    IR * starr = rg->buildStoreArray(rg->dupIRTree(ARR_base(src)),
                                     rg->dupIRTreeList(ARR_sub_list(src)),
                                     src->getType(), ARR_elemtype(src),
                                     ((CStArray*)src)->getDimNum(),
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
IR * CISt::dupIRTreeByExp(IR const* src, IR * rhs, Region * rg)
{
    ASSERT0(src->is_ild());
    IR * ist = rg->buildIStore(rg->dupIRTree(ILD_base(src)), rhs,
                               src->getOffset(), src->getType());
    ist->copyRef(src, rg);
    IST_base(ist)->copyRefForTree(ILD_base(src), rg);
    return ist;
}
//END CISt


//
//START CSt
//
IR * CSt::dupIRTreeByExp(IR const* src, IR * rhs, Region * rg)
{
    ASSERT0(src->is_ld());
    IR * st = rg->buildStore(LD_idinfo(src), src->getType(), src->getOffset(),
                              rhs);
    st->copyRef(src, rg);
    return st;
}
//END CSt


//
//START CStpr
//
IR * CStpr::dupIRTreeByExp(IR const* src, IR * rhs, Region * rg)
{
    ASSERT0(src->is_pr());
    IR * stpr = rg->buildStorePR(PR_no(src), src->getType(), rhs);
    stpr->copyRef(src, rg);
    return stpr;
}
//END CStpr


//The function only invoked at debug mode.
//Make sure IR_ICALL is the largest ir.
bool checkMaxIRType()
{
    //Change MAX_OFFSET_AT_FREE_TABLE if IR_ICALL is not the largest.
    for (UINT i = IR_UNDEF; i < IR_TYPE_NUM; i++) {
        ASSERT0(IRTSIZE(i) <= IRTSIZE(IR_ICALL));
    }
    return true;
}


//The function only invoked at debug mode.
bool checkIRDesc()
{
    UINT sz = (UINT)(1 << IR_TYPE_BIT_SIZE);
    ASSERTN(IR_TYPE_NUM <= sz, ("code field is too small"));
    DUMMYUSE(sz);
    for (UINT i = IR_UNDEF; i < IR_TYPE_NUM; i++) {
        ASSERT0(i == (UINT)IRDES_code(g_ir_desc[i]));
    }
    UINT descnum = sizeof(g_ir_desc) / sizeof(g_ir_desc[0]);
    CHECKN_DUMMYUSE(descnum - 1 == IR_TYPE_NUM, ("miss IRDesc declaration"));
    return true;
}


//The function only invoked at debug mode.
bool checkRoundDesc()
{
    for (UINT i = ROUND_UNDEF; i < ROUND_TYPE_NUM; i++) {
        ASSERT0(i == (UINT)ROUNDDESC_type(g_round_desc[i]));
    }
    UINT descnum = sizeof(g_round_desc) / sizeof(g_round_desc[0]);
    CHECKN_DUMMYUSE(descnum == ROUND_TYPE_NUM, ("miss RoundDesc declaration"));
    return true;
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
void CCall::addDummyUse(Region * rg)
{
    IR * newdummyuse = rg->buildILoad(rg->buildImmAny(0),
                                      rg->getTypeMgr()->getAny());
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


IR * CPhi::dupIRTreeByRef(IR const* src, Region * rg)
{
    ASSERT0(src->isPROp());
    //Type cast if host compiler does NOT support C11.
    IR * phi = rg->buildPhi(src->getPrno(), src->getType(), (IR*)nullptr);
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
