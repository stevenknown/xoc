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

//Use do-while to supress warning: value computed is not used
#define PADDR(ir) \
  do { int a = dump_addr ? prt(rg, " 0x%p", (ir)) : 0; DUMMYUSE(a); } while (0)

IRDesc const g_ir_desc[] = {
    { IR_UNDEF,    "undef",        0x0, 0, 0,
      0,},
    { IR_CONST,    "const",        0x0, 0, sizeof(CConst),
      IRT_IS_LEAF,},
    { IR_ID,       "id",           0x0, 0, sizeof(CId),
      IRT_HAS_IDINFO|IRT_IS_LEAF|IRT_IS_NON_PR_MEMREF|
      IRT_IS_MEM_OPND|IRT_HAS_DU },
    { IR_LD,       "ld",           0x0, 0, sizeof(CLd),
      IRT_HAS_IDINFO|IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_LEAF|
      IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },
    { IR_ILD,      "ild",          0x1, 1, sizeof(CILd),
      IRT_IS_UNA|IRT_IS_MEM_REF|IRT_IS_MEM_OPND|
      IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },
    { IR_PR,       "pr",           0x0, 0, sizeof(CPr),
      IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_LEAF|IRT_HAS_DU },
    { IR_ARRAY,    "array",        0x3, 2, sizeof(CArray),
      IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_NON_PR_MEMREF|
      IRT_HAS_DU|IRT_HAS_OFFSET },
    { IR_ST,       "st",           0x1, 1, sizeof(CSt),
      IRT_HAS_IDINFO|IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },
    { IR_STPR,     "stpr",         0x1, 1, sizeof(CStpr),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR },
    { IR_STARRAY,  "starray",      0x7, 3, sizeof(CStArray),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },
    { IR_IST,      "ist",          0x3, 2, sizeof(CISt),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET },
    { IR_SETELEM,  "setelem",      0x7, 3, sizeof(CSetElem),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR },
    { IR_GETELEM,  "getelem",      0x3, 2, sizeof(CGetElem),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR },
    { IR_CALL,     "call",         0x3, 2, sizeof(CCall),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_HAS_IDINFO|IRT_IS_STMT_IN_BB|IRT_HAS_DU }, //CALL might not def PR if
                                                     //there is not return
                                                     //value.
    { IR_ICALL,    "icall",        0x7, 3, sizeof(CICall),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU }, //ICALL might not def PR if
                                      //there is not return value.
    { IR_LDA,      "lda",          0x0, 0, sizeof(CLda),
      IRT_HAS_IDINFO|IRT_IS_UNA|IRT_IS_LEAF|IRT_HAS_OFFSET },
    { IR_ADD,      "add",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE },
    { IR_SUB,      "sub",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE },
    { IR_MUL,      "mul",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE },
    { IR_DIV,      "div",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN },
    { IR_REM,      "rem",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN },
    { IR_MOD,      "mod",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN },
    { IR_LAND,     "land",         0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_LOGICAL },
    { IR_LOR,      "lor",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_LOGICAL },
    { IR_BAND,     "band",         0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE },
    { IR_BOR,      "bor",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE },
    { IR_XOR,      "xor",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE },
    { IR_ASR,      "asr",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN },
    { IR_LSR,      "lsr",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN },
    { IR_LSL,      "lsl",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN },
    { IR_LT,       "lt",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION },
    { IR_LE,       "le",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION },
    { IR_GT,       "gt",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION },
    { IR_GE,       "ge",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION },
    { IR_EQ,       "eq",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE|IRT_IS_RELATION },
    { IR_NE,       "ne",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE|IRT_IS_RELATION },
    { IR_BNOT,     "bnot",         0x1, 1, sizeof(CUna),
      IRT_IS_UNA },
    { IR_LNOT,     "lnot",         0x1, 1, sizeof(CUna),
      IRT_IS_UNA|IRT_IS_LOGICAL },
    { IR_NEG,      "neg",          0x1, 1, sizeof(CUna),
      IRT_IS_UNA },
    { IR_CVT,      "cvt",          0x1, 1, sizeof(CCvt),
      IRT_IS_UNA },
    { IR_GOTO,     "goto",         0x0, 0, sizeof(CGoto),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },
    { IR_IGOTO,    "igoto",        0x3, 2, sizeof(CIGoto),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },
    { IR_DO_WHILE, "dowhile",      0x3, 2, sizeof(CDoWhile),
      IRT_IS_STMT },
    { IR_WHILE_DO, "whiledo",      0x3, 2, sizeof(CWhileDo),
      IRT_IS_STMT },
    { IR_DO_LOOP,  "doloop",       0x1F,5, sizeof(CDoLoop),
      IRT_IS_STMT },
    { IR_IF,       "if",           0x7, 3, sizeof(CIf),
      IRT_IS_STMT },
    { IR_LABEL,    "label",        0x0, 0, sizeof(CLab),
      IRT_IS_STMT },
    { IR_SWITCH,   "switch",       0x7, 3, sizeof(CSwitch),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },
    { IR_CASE,     "case",         0x1, 1, sizeof(CCase),
      0, },
    { IR_TRUEBR,   "truebr",       0x1, 1, sizeof(CTruebr),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },
    { IR_FALSEBR,  "falsebr",      0x1, 1, sizeof(CFalsebr),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },
    { IR_RETURN,   "return",       0x1, 1, sizeof(CRet),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB },
    { IR_SELECT,   "select",       0x7, 3, sizeof(CSelect),
      0, },
    { IR_BREAK,    "break",        0x0, 0, sizeof(CBreak),
      IRT_IS_STMT },
    { IR_CONTINUE, "continue",     0x0, 0, sizeof(CContinue),
      IRT_IS_STMT },
    { IR_PHI,      "phi",          0x1, 1, sizeof(CPhi),
      IRT_IS_STMT|IRT_HAS_RESULT|IRT_IS_MEM_REF|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR },
    { IR_REGION,   "region",       0x0, 0, sizeof(CRegion),
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


//Dump IR list with a logging header-notation.
//Dump both its kids and siblings.
void dumpIRListH(IR const* ir_list, Region const* rg, CHAR * attr,
                 UINT dumpflag)
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==---- DUMP IR List ----==");
    dumpIRList(ir_list, rg, attr, dumpflag);
}


//Dump IR, and both its kids and siblings.
void dumpIRList(IR const* ir_list, Region const* rg, CHAR * attr,
                UINT dumpflag)
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "");
    bool first_one = true;
    for (; ir_list != nullptr; ir_list = ir_list->get_next()) {
        if (first_one) {
            first_one = false;
            dumpIR(ir_list, rg, attr, dumpflag);
        } else {
            dumpIR(ir_list, rg, attr, dumpflag);
        }
    }
}


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


//Check for IR and IRBB sanity and uniqueness.
//Ensure that all IRs must be embedded into a basic block.
//Ensure that PHI must be the first stmt in basic block.
bool verifyIRandBB(BBList * bblst, Region const* rg)
{
    //IRAddressHash irh;
    BitSet irh;
    for (IRBB * bb = bblst->get_head();
         bb != nullptr; bb = bblst->get_next()) {
        bool should_not_phi = false;
        IRListIter irct;
        for (IR * ir = BB_irlist(bb).get_head(&irct);
             ir != nullptr; ir = BB_irlist(bb).get_next(&irct)) {
            ASSERT0(ir->is_single());
            ASSERT0(IR_parent(ir) == nullptr);
            ASSERT0(ir->getBB() == bb);

            if (!ir->is_phi()) {
                should_not_phi = true;
            }

            if (should_not_phi) {
                ASSERT0(!ir->is_phi());
            }

            verifyIRList(ir, &irh, rg);
            ASSERT0(ir->isStmtInBB());
        }
        bb->verify();
    }
    return true;
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


void dumpIRList(IRList const& ir_list, Region const* rg)
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==---- DUMP IR List ----==\n");
    ASSERT0(rg);
    IRListIter it;
    for (IR const* ir = ir_list.get_head(&it);
         ir != nullptr; ir = ir_list.get_next(&it)) {
        ASSERT0(ir->is_single());
        dumpIR(ir, rg);
    }
}


static void dump_lab_decl(LabelInfo const* li, RegionMgr const* rm)
{
    if (LABELINFO_type(li) == L_ILABEL) {
        prt(rm, "label " ILABEL_STR_FORMAT "", ILABEL_CONT(li));
    } else if (LABELINFO_type(li) == L_CLABEL) {
        prt(rm, "label " CLABEL_STR_FORMAT "", CLABEL_CONT(li));
    } else if (LABELINFO_type(li) == L_PRAGMA) {
        ASSERT0(LABELINFO_pragma(li));
        prt(rm, "pragma %s", SYM_name(LABELINFO_pragma(li)));
    } else { ASSERTN(0, ("unknown label type")); }

    bool first = true;
    if (LABELINFO_b1(li) != 0) {
        prt(rm, "(");
    }

    if (LABELINFO_is_try_start(li)) {
        if (!first) {
            prt(rm, ",");
        }
        first = false;
        prt(rm, "try_start ");
    }

    if (LABELINFO_is_try_end(li)) {
        if (!first) {
            prt(rm, ",");
        }
        first = false;
        prt(rm, "try_end ");
    }

    if (LABELINFO_is_catch_start(li)) {
        if (!first) {
            prt(rm, ",");
        }
        first = false;
        prt(rm, "catch_start ");
    }

    if (LABELINFO_is_terminate(li)) {
        if (!first) {
            prt(rm, ",");
        }
        first = false;
        prt(rm, "terminate ");
    }

    if (LABELINFO_b1(li) != 0) {
        prt(rm, ")");
    }
}


static void dump_label_ref(LabelInfo const* li, RegionMgr const* rm)
{
    if (LABELINFO_type(li) == L_ILABEL) {
        prt(rm, ILABEL_STR_FORMAT "", ILABEL_CONT(li));
    } else if (LABELINFO_type(li) == L_CLABEL) {
        prt(rm, CLABEL_STR_FORMAT "", CLABEL_CONT(li));
    } else {
        ASSERTN(0, ("unknown label type"));
    }
}


static void dumpAttachInfo(OUT CHAR * buf, IR const* ir)
{
    ASSERT0(ir && buf);
    AIContainer const* ai = ir->getAI();
    if (ai == nullptr) { return; }

    AICont const& cont = ai->read_cont();

    if (!cont.is_init()) { return; }

    strcat(buf, " attachinfo:");
    CHAR * p = buf + strlen(buf);
    bool not_first = false;
    for (UINT i = 0; i < cont.get_capacity(); i++) {
        BaseAttachInfo * ac = cont.get(i);
        if (ac == nullptr) { continue; }

        if (!not_first) {
            not_first = true;
        } else {
            sprintf(p, ",");
            p = p + strlen(p);
        }

        sprintf(p, "%s", ai->getAIName(ac->type));
        p = p + strlen(p);
    }
}


void dumpConst(IR const* ir, Region const* rg)
{
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    if (ir->is_sint()) {
        #if WORD_LENGTH_OF_HOST_MACHINE==32
        CHAR const* intfmt = "intconst:%s %d|0x%x";
        #elif WORD_LENGTH_OF_HOST_MACHINE==64
        CHAR const* intfmt = "intconst:%s %lld|0x%llx";
        #else
        #error "Need to support";
        #endif
        prt(rg, intfmt, xtm->dump_type(d, buf),
            CONST_int_val(ir), CONST_int_val(ir));
        return;
    }

    if (ir->is_uint()) {
        #if WORD_LENGTH_OF_HOST_MACHINE==32
        CHAR const* intfmt = "intconst:%s %u|0x%x";
        #elif WORD_LENGTH_OF_HOST_MACHINE==64
        CHAR const* intfmt = "intconst:%s %llu|0x%llx";
        #else
        #error "Need to support";
        #endif
        prt(rg, intfmt, xtm->dump_type(d, buf),
            CONST_int_val(ir), CONST_int_val(ir));
        return;
    }

    if (ir->is_fp()) {
        CHAR fpformat[128];
        ::snprintf(fpformat, 127, "fpconst:%%s %%.%df",
                   CONST_fp_mant(ir));
        prt(rg, fpformat, xtm->dump_type(d, buf), CONST_fp_val(ir));
        return;
    }

    if (ir->is_bool()) {
        prt(rg, "boolconst:%s %d", xtm->dump_type(d, buf),
            CONST_int_val(ir));
        return;
    }

    if (ir->is_str()) {
        UINT const tbuflen = 40;
        CHAR tbuf[tbuflen];
        tbuf[0] = 0;
        xstrcat(tbuf, tbuflen, "%s", SYM_name(CONST_str_val(ir)));

        //Remove \n to show string in one line.
        for (UINT i = 0; i < tbuflen && tbuf[i] != 0; i++) {
            if (tbuf[i] == '\n') { tbuf[i] = ' '; }
        }

        if (strlen(SYM_name(CONST_str_val(ir))) < tbuflen) {
            prt(rg, "strconst:%s \\\"%s\\\"",
                xtm->dump_type(d, buf), tbuf);
        } else {
            prt(rg, "strconst:%s \\\"%s...\\\"",
                xtm->dump_type(d, buf), tbuf);
        }
        return;
    }

    if (ir->is_mc()) {
        //Imm may be MC type.
        #if WORD_LENGTH_OF_HOST_MACHINE==32
        CHAR const* intfmt = "intconst:%s %u|0x%x";
        #elif WORD_LENGTH_OF_HOST_MACHINE==64
        CHAR const* intfmt = "intconst:%s %llu|0x%llx";
        #else
        #error "Need to support";
        #endif
        prt(rg, intfmt, xtm->dump_type(d, buf),
            CONST_int_val(ir), CONST_int_val(ir));
        return;
    }

    //Dump as HOST_INT type even if it is unrecognized,
    //leave the sanity check to verify().
    //Note the dump format may extend or truncate the real value.
    prt(rg, "intconst:%s %d|0x%x", xtm->dump_type(d, buf),
        CONST_int_val(ir),  CONST_int_val(ir));
}


//Dump IR and all of its kids.
//'attr': miscellaneous string which following 'ir'.
void dumpIR(IR const* ir, Region const* rg, IN CHAR * attr, UINT dumpflag)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_src_line = HAVE_FLAG(dumpflag, IR_DUMP_SRC_LINE);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    bool dump_inner_region = HAVE_FLAG(dumpflag, IR_DUMP_INNER_REGION);
    bool dump_var_decl = HAVE_FLAG(dumpflag, IR_DUMP_VAR_DECL);
    DUMMYUSE(dump_src_line);
    DUMMYUSE(dump_kid);
    DUMMYUSE(dump_addr);
    DUMMYUSE(dump_inner_region);
    DUMMYUSE(attr);
    DUMMYUSE(rg);
    DUMMYUSE(ir);
    TypeMgr * tm = rg->getTypeMgr();
    RegionMgr * rm = rg->getRegionMgr();
    LogMgr * lm = rg->getLogMgr();

    ASSERT0(tm);
    UINT dn = 4;
    if (!rg->isLogMgrInit() || ir == nullptr) { return; }

    //Attribution string do NOT exceed length of 128 chars.
    static CHAR attr_buf[128];
    if (attr == nullptr) {
        attr = attr_buf;
        *attr = 0;
    } else {
        sprintf(attr_buf, "%s", attr);
        attr = attr_buf;
    }

    CHAR * p = attr + strlen(attr);
    sprintf(p, " id:%d", ir->id());
    if (ir->isMayThrow()) {
        strcat(p, " throw");
    }
    if (ir->is_terminate()) {
        strcat(p, " terminate");
    }
    if (ir->is_atomic()) {
        strcat(p, " atom");
    }
    if (ir->is_rmw()) {
        strcat(p, " rmw");
    }
    if (ir->hasSideEffect()) {
        strcat(p, " sideeffect");
    }
    if (ir->isNoMove()) {
        strcat(p, " nomove");
    }
    if (ir->isReadOnly()) {
        strcat(p, " readonly");
    }
    if (ir->is_volatile()) {
        strcat(p, " volatile");
    }

    dumpAttachInfo(p, ir);

    //Record type info and var decl.
    StrBuf buf(64);
    StrBuf buf2(64);

    if (g_dbx_mgr != nullptr && dump_src_line) {
        DbxMgr::PrtCtx prtctx;
        prtctx.logmgr = lm;
        g_dbx_mgr->printSrcLine(ir, &prtctx);
    }

    Type const* d = nullptr;
    if (ir->getType() != nullptr) {
        d = ir->getType();
    }

    TypeMgr * xtm = const_cast<TypeMgr*>(tm);
    switch (ir->getCode()) {
    case IR_ST: {
        CHAR tt[40];
        tt[0] = 0;
        CHAR * name = xstrcat(tt, 40, "%s",
        SYM_name(ST_idinfo(ir)->get_name()));

        //Dump operator and variable name.
        note(rg, "\nst:%s", xtm->dump_type(d, buf));
        if (ST_ofst(ir) != 0) {
            prt(rg, ":offset(%d)", ST_ofst(ir));
        }
        prt(rg, " '%s'", name);

        //Dump declaration info if the frontend supplied.
        buf.clean();
        if (dump_var_decl && ST_idinfo(ir)->dumpVARDecl(buf) != nullptr) {
            prt(rg, " decl:%s", buf.buf);
        }

        PADDR(ir);
        prt(rg, "%s", attr);

        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(ST_rhs(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
        }
        break;
    }
    case IR_STPR:
        note(rg, "\nstpr $%d:%s", STPR_no(ir), xtm->dump_type(d, buf));
        PADDR(ir);
        prt(rg, "%s", attr);

        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(STPR_rhs(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_SETELEM:
        note(rg, "\nsetelem $%d:%s", SETELEM_prno(ir), xtm->dump_type(d, buf));
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(SETELEM_base(ir), rg, nullptr, dumpflag);
            dumpIRList(SETELEM_val(ir), rg, nullptr, dumpflag);
            dumpIRList(SETELEM_ofst(ir), rg, (CHAR*)" offset", dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_GETELEM:
        note(rg, "\ngetelem $%d:%s", GETELEM_prno(ir), xtm->dump_type(d, buf));
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(GETELEM_base(ir), rg, (CHAR*)" base", dumpflag);
            dumpIRList(GETELEM_ofst(ir), rg, (CHAR*)" offset", dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_STARRAY:
        if (ARR_ofst(ir) != 0) {
            note(rg, "\nstarray (%s:offset(%d), ety:%s)",
                 xtm->dump_type(d, buf),
                 ARR_ofst(ir),
                 xtm->dump_type(ARR_elemtype(ir), buf2));
        } else {
            note(rg, "\nstarray (%s, ety:%s)",
                 xtm->dump_type(d, buf),
                 xtm->dump_type(ARR_elemtype(ir), buf2));
        }

        PADDR(ir);
        prt(rg, "%s", attr);
        if (ARR_sub_list(ir) != nullptr && dump_kid) {
            //Dump elem number.
            lm->incIndent(dn);
            UINT dim = 0;
            if (ARR_elem_num_buf(ir) != nullptr) {
                note(rg, "\nelem_num[");
                for (IR const* sub = ARR_sub_list(ir); sub != nullptr;) {
                    prt(rg, "%d",
                        ((CArray*)ir)->getElementNumOfDim(dim));
                    sub = sub->get_next();
                    if (sub != nullptr) {
                        prt(rg, ",");
                    }
                    dim++;
                }
                prt(rg, "]");
            } else { note(rg, "\nelem_num[--]"); }

            //Dump sub exp list.
            dim = 0;
            for (IR const* sub = ARR_sub_list(ir);
                 sub != nullptr; sub = sub->get_next()) {
                CHAR tt[40];
                sprintf(tt, " dim%d", dim);
                dumpIR(sub, rg, (CHAR*)tt, dumpflag);
                dim++;
            }
            lm->decIndent(dn);
        }
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(ARR_base(ir), rg, (CHAR*)" array_base", dumpflag);
            dumpIRList(STARR_rhs(ir), rg, (CHAR*)" rhs", dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_IST:
        if (IST_ofst(ir) != 0) {
            note(rg, "\nist:%s:offset(%d)", xtm->dump_type(d, buf), IST_ofst(ir));
        } else {
            note(rg, "\nist:%s", xtm->dump_type(d, buf));
        }

        //Dump IR address.
        PADDR(ir);
        prt(rg, "%s", attr);

        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(IST_base(ir), rg, (CHAR*)" base", dumpflag);
            dumpIRList(IST_rhs(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_LD: {
        CHAR tt[40];
        tt[0] = 0;

        //Dump variable info.
        CHAR * name = xstrcat(tt, 40, "%s",
            SYM_name(LD_idinfo(ir)->get_name()));

        if (LD_ofst(ir) != 0) {
            note(rg, "\nld:%s:offset(%d) '%s'",
                 xtm->dump_type(d, buf), LD_ofst(ir), name);
        } else {
            note(rg, "\nld:%s '%s'", xtm->dump_type(d, buf), name);
        }

        //Dump declaration if frontend supplied.
        buf.clean();
        if (dump_var_decl && LD_idinfo(ir)->dumpVARDecl(buf) != nullptr) {
            prt(rg, " decl:%s", buf.buf);
        }

        //Dump IR address.
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    }
    case IR_ILD:
        if (ILD_ofst(ir) != 0) {
            note(rg, "\nild:%s:offset(%d)",
                 xtm->dump_type(d, buf), ILD_ofst(ir));
        } else {
            note(rg, "\nild:%s", xtm->dump_type(d, buf));
        }

        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(ILD_base(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_PR:
        note(rg, "\n$%d:%s", PR_no(ir), xtm->dump_type(d, buf));
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    case IR_ID: {
        CHAR tt[40];
        tt[0] = 0;

        //Dump ID name.
        CHAR * name =
            xstrcat(tt, 40, "%s", SYM_name(ID_info(ir)->get_name()));
        note(rg, "\nid:%s '%s'", xtm->dump_type(d, buf), name);

        buf.clean();
        if (dump_var_decl && ID_info(ir)->dumpVARDecl(buf) != nullptr) {
            prt(rg, " decl:%s", buf.buf);
        }

        //Dump IR address.
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    }
    case IR_CONST:
        note(rg, "\n");
        dumpConst(ir, rg);
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        note(rg, "\n%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
        if (ir->is_cvt() && CVT_round(ir) != ROUND_UNDEF) {
          prt(rg, ":round(%s)", ROUND_NAME(CVT_round(ir)));
        }
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k == nullptr) { continue; }
                dumpIRList(k, rg, nullptr, dumpflag);
            }
            lm->decIndent(dn);
        }
        break;
    case IR_IF:
        note(rg, "\nif");
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(IF_det(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\n{");
            lm->incIndent(dn);
            dumpIRList(IF_truebody(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
            note(rg, "\n}");

            if (IF_falsebody(ir)) {
                note(rg, "\nelse");
                note(rg, "\n{");
                lm->incIndent(dn);
                dumpIRList(IF_falsebody(ir), rg, nullptr, dumpflag);
                lm->decIndent(dn);
                note(rg, "\n}");
            }
        }
        break;
    case IR_DO_WHILE:
        note(rg, "\ndowhile");
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            note(rg, "\nbody:");
            lm->incIndent(dn);
            dumpIRList(LOOP_body(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\ndet:");
            lm->incIndent(dn);
            dumpIRList(LOOP_det(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\nend_dowhile");
        }
        break;
    case IR_WHILE_DO:
        note(rg, "\nwhiledo");
        PADDR(ir);
        prt(rg, "%s", attr);

        if (dump_kid) {
            note(rg, "\ndet:");
            lm->incIndent(dn);
            dumpIRList(LOOP_det(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\nbody:");

            lm->incIndent(dn);
            dumpIRList(LOOP_body(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\nend_whiledo");
        }
        break;
    case IR_DO_LOOP:
        note(rg, "\ndoloop");
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            note(rg, "\niv:");
            lm->incIndent(dn);
            dumpIRList(LOOP_iv(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\ninit:");
            lm->incIndent(dn);
            dumpIRList(LOOP_init(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\ndet:");
            lm->incIndent(dn);
            dumpIRList(LOOP_det(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\nstep:");
            lm->incIndent(dn);
            dumpIRList(LOOP_step(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\nbody:");
            lm->incIndent(dn);
            dumpIRList(LOOP_body(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\nend_doloop");
        }
        break;
    case IR_BREAK:
        note(rg, "\nbreak");
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    case IR_CONTINUE:
        note(rg, "\ncontinue");
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    case IR_RETURN:
        note(rg, "\nreturn");
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIR(RET_exp(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_GOTO:
        note(rg, "\ngoto ");
        dump_lab_decl(ir->getLabel(), rm);
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    case IR_IGOTO:
        note(rg, "\nigoto");
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(IGOTO_vexp(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            note(rg, "\ncase_list");
            lm->incIndent(dn);
            dumpIRList(IGOTO_case_list(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_LABEL: {
        LabelInfo const* li = LAB_lab(ir);
        if (LABELINFO_type(li) == L_ILABEL) {
            note(rg, "\nlabel " ILABEL_STR_FORMAT "",
                 ILABEL_CONT(LAB_lab(ir)));
        } else if (LABELINFO_type(li) == L_CLABEL) {
            note(rg, "\nlabel " CLABEL_STR_FORMAT "",
                 CLABEL_CONT(LAB_lab(ir)));
        } else if (LABELINFO_type(li) == L_PRAGMA) {
            ASSERT0(LABELINFO_pragma(LAB_lab(ir)));
            note(rg, "\npragma %s", SYM_name(LABELINFO_pragma(LAB_lab(ir))));
        } else { UNREACHABLE(); }

        PADDR(ir);

        if (LABELINFO_b1(li) != 0) {
            prt(rg, "(");
        }

        if (LABELINFO_is_try_start(li)) {
            prt(rg, "try_start ");
        }

        if (LABELINFO_is_try_end(li)) {
            prt(rg, "try_end ");
        }

        if (LABELINFO_is_catch_start(li)) {
            prt(rg, "catch_start ");
        }

        if (LABELINFO_is_terminate(li)) {
            prt(rg, "terminate ");
        }

        if (LABELINFO_b1(li) != 0) {
            prt(rg, ")");
        }

        prt(rg, "%s", attr);
        break;
    }
    case IR_SELECT: //formulized log_OR_exp?exp:cond_exp
        note(rg, "\nselect:%s", xtm->dump_type(d, buf));
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(SELECT_pred(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            lm->incIndent(dn);
            dumpIRList(SELECT_trueexp(ir), rg, (CHAR*)" true_exp", dumpflag);
            lm->decIndent(dn);

            lm->incIndent(dn);
            dumpIRList(SELECT_falseexp(ir), rg, (CHAR*)" false_exp", dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_LDA: { //Get address of a symbol
        CHAR tt[40];
        tt[0] = 0;

        //Dump variable info.
        CHAR * name = xstrcat(tt, 40, "%s",
            SYM_name(LDA_idinfo(ir)->get_name()));
        if (LDA_ofst(ir) != 0) {
            note(rg, "\nlda:%s:offset(%d) '%s'",
                 xtm->dump_type(d, buf), LDA_ofst(ir), name);
        } else {
            note(rg, "\nlda:%s '%s'", xtm->dump_type(d, buf), name);
        }

        //Dump declaration if frontend supplied.
        buf.clean();
        if (dump_var_decl && LDA_idinfo(ir)->dumpVARDecl(buf) != nullptr) {
            prt(rg, " decl:%s", buf.buf);
        }

        //Dump IR address.
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    }
    case IR_PHI:
        note(rg, "\n$%d:%s = phi", PHI_prno(ir), xtm->dump_type(d, buf));

        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            IR * opnd = PHI_opnd_list(ir);
            while (opnd != nullptr) {
                dumpIR(opnd, rg, nullptr, dumpflag);
                opnd = opnd->get_next();
            }
            lm->decIndent(dn);
        }
        break;
    case IR_SWITCH:
        note(rg, "\nswitch");
        if (SWITCH_deflab(ir) != nullptr) {
            prt(rg, ", deflab: ");
            dump_lab_decl(ir->getLabel(), rm);
        }
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(SWITCH_vexp(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);

            if (SWITCH_case_list(ir) != nullptr) {
                dumpIRList(SWITCH_case_list(ir), rg, nullptr, dumpflag);
            }

            if (SWITCH_body(ir) != nullptr) {
                note(rg, "\nbody:");
                lm->incIndent(dn);
                dumpIRList(SWITCH_body(ir), rg, nullptr, dumpflag);
                lm->decIndent(dn);
            }
            note(rg, "\nend_switch");
        }
        break;
    case IR_CASE:
        ASSERT0(CASE_vexp(ir));
        ASSERT0(CASE_lab(ir));
        note(rg, "\ncase");
        PADDR(ir);
        prt(rg, "%s", attr);

        lm->incIndent(dn);
        dumpIRList(CASE_vexp(ir), rg, nullptr, dumpflag);
        note(rg, "\n");
        dump_lab_decl(ir->getLabel(), rm);
        lm->decIndent(dn);
        break;
    case IR_ARRAY:
        if (ARR_ofst(ir) != 0) {
            note(rg, "\narray (%s:offset(%d), ety:%s)",
                 xtm->dump_type(d, buf),
                 ARR_ofst(ir),
                 xtm->dump_type(ARR_elemtype(ir), buf2));
        } else {
            note(rg, "\narray (%s, ety:%s)",
                 xtm->dump_type(d, buf),
                 xtm->dump_type(ARR_elemtype(ir), buf2));
        }

        PADDR(ir);
        prt(rg, "%s", attr);
        if (ARR_sub_list(ir) != nullptr && dump_kid) {
            //Dump element number if it exist.
            lm->incIndent(dn);

            if (ARR_elem_num_buf(ir) != nullptr) {
                UINT dim = 0;
                note(rg, "\nelemnum[");
                for (IR const* sub = ARR_sub_list(ir); sub != nullptr;) {
                    prt(rg, "%d", ARR_elem_num(ir, dim));
                    sub = sub->get_next();
                    if (sub != nullptr) {
                        prt(rg, ",");
                    }
                    dim++;
                }
                prt(rg, "]");
            } else { note(rg, "\nelemnum[--]"); }

            //Dump subscript expressions in each dimension.
            UINT dim = 0;
            for (IR const* sub = ARR_sub_list(ir);
                 sub != nullptr; sub = sub->get_next()) {
                CHAR tt[40];
                sprintf(tt, " dim%d", dim);
                dumpIR(sub, rg, (CHAR*)tt, dumpflag);
                dim++;
            }
            lm->decIndent(dn);
        }

        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(ARR_base(ir), rg, (CHAR*)" array_base", dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_CALL:
    case IR_ICALL: {
        if (ir->hasReturnValue()) {
            note(rg, "\n$%d:%s = ", CALL_prno(ir), xtm->dump_type(d, buf));
        } else {
            note(rg, "\n");
        }

        if (ir->is_call()) {
            CHAR tt[44];
            tt[0] = 0;
            CHAR const* string = SYM_name(CALL_idinfo(ir)->get_name());
            CHAR * name = xstrcat(tt, 40, "%s", string);
            if (strlen(string) > 40) {
                strcat(tt, "...");
            }
            prt(rg, "call '%s' ", name);
            buf.clean();
            if (dump_var_decl && CALL_idinfo(ir)->dumpVARDecl(buf) != nullptr) {
                prt(rg, "decl:%s", buf.buf);
            }
        } else {
            prt(rg, "icall ");
        }

        PADDR(ir);
        prt(rg, "%s", attr);

        if (dump_kid) {
            if (ir->is_icall()) {
                lm->incIndent(dn);
                dumpIR(ICALL_callee(ir), rg, (CHAR*)" callee", dumpflag);
                lm->decIndent(dn);
            }

            CHAR tmpbuf[30];
            UINT i = 0;

            //Dump parameter list.
            for (IR * p2 = CALL_param_list(ir);
                 p2 != nullptr; p2 = p2->get_next()) {
                sprintf(tmpbuf, " param%d", i);
                lm->incIndent(dn);
                dumpIR(p2, rg, tmpbuf, dumpflag);
                lm->decIndent(dn);
                i++;
            }

            //Dump dummy use.
            i = 0;
            for (IR * p2 = CALL_dummyuse(ir);
                 p2 != nullptr; p2 = p2->get_next()) {
                sprintf(tmpbuf, " dummy%d", i);
                lm->incIndent(dn);
                dumpIR(p2, rg, tmpbuf, dumpflag);
                lm->decIndent(dn);
                i++;
            }
        }
        break;
    }
    case IR_TRUEBR:
        note(rg, "\ntruebr ");
        dump_lab_decl(ir->getLabel(), rm);
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(BR_det(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_FALSEBR:
        note(rg, "\nfalsebr ");
        dump_lab_decl(ir->getLabel(), rm);
        PADDR(ir);
        prt(rg, "%s", attr);
        if (dump_kid) {
            lm->incIndent(dn);
            dumpIRList(BR_det(ir), rg, nullptr, dumpflag);
            lm->decIndent(dn);
        }
        break;
    case IR_REGION:
        note(rg, "\nregion");
        if (REGION_ru(ir)->getRegionVar() != nullptr) {
            Var * ruvar = REGION_ru(ir)->getRegionVar();
            CHAR tt[40];
            tt[0] = 0;

            //Dump variable info.
            xstrcat(tt, 40, "%s", SYM_name(ruvar->get_name()));
            prt(rg, " \'%s\',id:%d", tt, REGION_ru(ir)->id());
        }

        PADDR(ir); //Dump IR address.
        prt(rg, "%s", attr); //Dump attributes.

        if (dump_inner_region) {
            //Inner region.
            ASSERT0(REGION_ru(ir));
            lm->incIndent(dn);
            note(rg, "\nregion-info:");
            REGION_ru(ir)->dump(dump_inner_region);
            lm->decIndent(dn);
        }
        break;
    case IR_UNDEF:
        note(rg, "\nundef!");
        PADDR(ir);
        prt(rg, "%s", attr);
        break;
    default:
        ASSERTN(0, ("unknown IR type:%s", IRNAME(ir)));
        return ;
    }
}


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
///


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
        UINT ofst = getOffset();
        if (ofst != 0) {
            ASSERT0((ofst % tm->getDTypeByteSize(TY_vec_ety(d))) == 0);
        }
    }

    switch (getCode()) {
    case IR_UNDEF: ASSERTN(0, ("should not be undef")); break;
    case IR_CONST:
        ASSERT0(d);
        ASSERTN(TY_dtype(d) != D_UNDEF, ("size of load value cannot be zero"));
        if (!is_sint() &&
            !is_uint() &&
            !is_fp() &&
            !is_bool() &&
            !is_mc() &&
            !is_any() &&
            !is_str()) {
            ASSERTN(0, ("unsupport immediate value DATA_TYPE:%d", getDType()));
        }
        break;
    case IR_ID: break;
    case IR_LD:
        //src of LD might be small or big compare with REG.
        ASSERT0(LD_idinfo(this));
        ASSERT0(d);
        ASSERTN(TY_dtype(d) != D_UNDEF, ("size of load value cannot be zero"));
        break;
    case IR_ST:
        ASSERT0(d);
        ASSERTN(TY_dtype(d)!= D_UNDEF, ("size of store value cannot be zero"));
        ASSERT0(ST_idinfo(this));
        ASSERT0(ST_rhs(this));
        ASSERT0(ST_rhs(this)->is_exp());
        ASSERT0(ST_rhs(this)->is_single());
        if (d->is_vector()) {
            ASSERT0(TY_vec_ety(d) != D_UNDEF);
            ASSERT0(tm->getDTypeByteSize(TY_vec_ety(d)) >=
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
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(SETELEM_base(this) && SETELEM_val(this) && SETELEM_ofst(this));
        if (d->is_vector()) {
            ASSERT0(TY_vec_ety(d) != D_UNDEF);

            //Note if the value size less than elemsize, it will be hoist to
            //elemsize.
            ASSERT0(tm->getDTypeByteSize(TY_vec_ety(d)) >=
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
        ASSERT0(TY_dtype(d) != D_UNDEF);
        IR const* base = GETELEM_base(this);
        ASSERT0(base && GETELEM_ofst(this));
        Type const* basedtd = base->getType();
        if (basedtd->is_vector()) {
            ASSERT0(TY_vec_ety(basedtd) != D_UNDEF);
            ASSERT0(tm->getDTypeByteSize(TY_vec_ety(basedtd)) >=
                    tm->getByteSize(d));
        }
        break;
    }
    case IR_LDA:
        ASSERT0(LDA_idinfo(this));
        ASSERT0(d);
        ASSERTN(TY_dtype(d) != D_UNDEF, ("size of load value cannot be zero"));
        ASSERT0(d->is_pointer());
        //Lda base can be general Var, label, const string.
        ASSERTN(!VAR_is_fake(LDA_idinfo(this)), ("LDA's base must be effect Var"));
        break;
    case IR_CALL:
        ASSERT0(CALL_idinfo(this));

        //result type of call is the type of return value if it exist.
        //The result type may be VOID.
        if (CALL_prno(this) != 0) { ASSERT0(d); }

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
        //Note return-value type may be VOID.

        //rtype of icall is the type of IR in return-value-list.
        ASSERT0(ICALL_callee(this) && ICALL_callee(this)->is_ptr());
        ASSERT0(ICALL_callee(this)->is_single());

        for (IR * p = CALL_param_list(this); p != nullptr; p = p->get_next()) {
            ASSERT0(p->is_exp());
        }
        break;
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        ASSERT0(d);
        ASSERT0(TY_dtype(d) != D_UNDEF);
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
        ASSERT0(TY_dtype(d) != D_UNDEF);
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
        ASSERT0(TY_dtype(d) != D_UNDEF);
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
        ASSERT0(TY_dtype(d) != D_UNDEF);
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
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(ARR_base(this)->is_ptr() || ARR_base(this)->is_any());
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
        ASSERT0(TY_dtype(d) != D_UNDEF);
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
        ASSERT0(d && TY_dtype(d) != D_UNDEF);
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
        ASSERT0(TY_dtype(d) != D_UNDEF);
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
        if (src->isReadPR() || src->isWritePR() || src->isCallStmt()) {
            return getPrno() == src->getPrno();
        }
        return false;
    default: UNREACHABLE();
    }
    return false;
}


//Return true if current ir tree is equivalent to src.
//src: root of IR tree.
//is_cmp_kid: it is true if comparing kids as well.
//Note the function does not compare the siblings of 'src'.
bool IR::isIREqual(IR const* src, bool is_cmp_kid) const
{
    ASSERT0(src);
    if (getCode() != src->getCode()) { return false; }
    switch (src->getCode()) {
    case IR_CONST: //Constant value: include integer, float, string.
        if (CONST_int_val(this) != CONST_int_val(src)) { return false; }
        break;
    case IR_ID:
        if (ID_info(this) != ID_info(src)) { return false; }
        break;
    case IR_LD:
        if (LD_idinfo(this) != LD_idinfo(src) ||
            LD_ofst(this) != LD_ofst(src) ||
            getType() != src->getType()) {
            return false;
        }
        break;
    case IR_ILD:
        if (ILD_ofst(this) != ILD_ofst(src) || getType() != src->getType()) {
            return false;
        }
        break;
     case IR_ST:
        if (ST_idinfo(this) != ST_idinfo(src) ||
            ST_ofst(this) != ST_ofst(src) ||
            getType() != src->getType()) {
            return false;
        }
        break;
    case IR_STPR:
        if (getType() != src->getType() || STPR_no(this) != STPR_no(src)) {
            return false;
        }
        break;
    case IR_STARRAY:
        if (ARR_ofst(this) != ARR_ofst(src) || getType() != src->getType()) {
            return false;
        }
        if ((ARR_elem_num_buf(src) != nullptr) ^
            (ARR_elem_num_buf(this) != nullptr)) {
            return false;
        }
        if (ARR_elem_num_buf(src) != nullptr) {
            ASSERT0(ARR_elem_num_buf(this));
            TMWORD dimnum = ((CStArray*)this)->getDimNum();
            ASSERT0(((CStArray*)src)->getDimNum() == dimnum);
            for (UINT i = 0; i < dimnum; i++) {
                if (((CStArray*)this)->getElementNumOfDim(i) !=
                    ((CStArray*)src)->getElementNumOfDim(i)) {
                    return false;
                }
            }
        }
        break;
    case IR_IST:
        if (IST_ofst(this) != IST_ofst(src) || getType() != src->getType()) {
            return false;
        }
        break;
    case IR_LDA:
        if (LDA_idinfo(this) != LDA_idinfo(src) ||
            LDA_ofst(this) != LDA_ofst(src) ||
            getType() != src->getType()) {
            return false;
        }
        break;
    case IR_CALL:
        if (CALL_idinfo(this) != CALL_idinfo(src)) { return false; }
        break;
    case IR_ICALL:
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        if (getType() != src->getType()) { return false; }
        break;
    case IR_GOTO:
    case IR_IGOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
        break;
    case IR_LABEL:
        if (LAB_lab(this) != LAB_lab(src)) { return false; }
        break;
    case IR_SWITCH:
    case IR_CASE:
        break;
    case IR_ARRAY:
        if (ARR_ofst(this) != ARR_ofst(src) || getType() != src->getType()) {
            return false;
        }
        if ((ARR_elem_num_buf(src) != nullptr) ^
            (ARR_elem_num_buf(this) != nullptr)) {
            return false;
        }
        if (ARR_elem_num_buf(src) != nullptr) {
            ASSERT0(ARR_elem_num_buf(this));
            TMWORD dimnum = ((CArray*)this)->getDimNum();
            if (((CArray*)src)->getDimNum() != dimnum) {
                return false;
            }
            for (UINT i = 0; i < dimnum; i++) {
                if (((CArray*)this)->getElementNumOfDim(i) !=
                    ((CArray*)src)->getElementNumOfDim(i)) {
                    return false;
                }
            }
        }
        break;
    case IR_PR:
        if (getType() != src->getType() || PR_no(this) != PR_no(src)) {
            return false;
        }
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SELECT:
        if (getType() != src->getType()) {
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
    for (INT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid1 = getKid(i);
        IR * kid2 = src->getKid(i);

        if (src->isCallStmt() &&
            (kid1 == CALL_dummyuse(this) ||
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
    for (IR * k = iterInit(const_cast<IR*>(this), it);
         k != nullptr; k = iterNext(it)) {
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


//This function only handle Call/ICall stmt, it find PR and remove
//them out of UseSet.
//Note this function does not maintain DU chain between call and its use.
void IR::removePRFromUseset(DefMiscBitSetMgr & sbs_mgr, Region * rg)
{
    ASSERT0(isCallStmt() && rg);
    DUSet * useset = getDUSet();
    if (useset == nullptr) { return; }

    DUIter di = nullptr;
    INT lnext = -1;
    for (INT i = useset->get_first(&di); i >= 0; i = lnext) {
        lnext = useset->get_next(i, &di);
        IR const* exp = rg->getIR(i);
        ASSERT0(exp->is_exp());
        if (!exp->isReadPR()) { continue; }
        useset->remove(i, sbs_mgr);
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
//'src': copy MD reference from 'src', it may be different to current ir.
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
    setRefMDSet(src->getRefMDSet(), rg);
}


//Copy AttachInfo from 'src' to current ir, not include kid and sibling.
void IR::copyAI(IR const* src, Region * rg)
{
    if (src->getAI() == nullptr) { return; }
    if (IR_ai(this) == nullptr) {
        IR_ai(this) = rg->allocAIContainer();
    }
    IR_ai(this)->copy(src->getAI(), rg);
}


static bool hasProp(IR const* ir)
{
    return ir->isMayThrow() || ir->is_terminate() || ir->is_atomic() ||
           ir->is_rmw() || ir->hasSideEffect() || ir->isNoMove();
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
        note(rg, "\nMMD%d", MD_id(md));
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


static void dumpOffset(IR const* ir, RegionMgr const* rm)
{
    if (hasProp(ir) ||
        ir->getOffset() != 0 ||
        ((ir->is_array() || ir->is_starray()) &&
         ARR_elem_num_buf(ir) != nullptr)) {
        prt(rm, ":%d", ir->getOffset());
    }
}


static void dumpProp(IR const* ir, TypeMgr * tm, DumpGRCtx * ctx)
{
    bool first = true;
    RegionMgr const* rm = tm->getRegionMgr();
    if (ir->isMayThrow()) {
        if (!first) { prt(rm, ","); }
        else { prt(rm, ":("); }
        prt(rm, "throw(");
        if (ir->getAI() != nullptr) {
            EHLabelAttachInfo const* ehlab =
                (EHLabelAttachInfo const*)ir->getAI()->get(AI_EH_LABEL);
            if (ehlab != nullptr) {
                xcom::SList<LabelInfo*> const& labs = ehlab->read_labels();
                for (xcom::SC<LabelInfo*> * sc = labs.get_head();
                     sc != labs.end(); sc = labs.get_next(sc)) {
                    if (sc != labs.get_head()) {
                        prt(rm, ",");
                    }
                    dump_label_ref(sc->val(), rm);
                }
            }
        }
        prt(rm, ")");
        first = false;
    }
    if (ir->is_array() || ir->is_starray()) {
        ASSERT0(ARR_elemtype(ir));
        if (ARR_elemtype(ir) != ir->getType() ||
            ARR_elem_num_buf(ir) != nullptr) {
            if (!first) { prt(rm, ","); }
            else { prt(rm, ":("); }
            first = false;
        }
        bool prt_elemtype = false;
        if (ARR_elemtype(ir) != ir->getType()) {
            xcom::StrBuf ety(16);
            tm->dump_type(ARR_elemtype(ir), ety);
            prt(rm, "elemtype:%s", ety.buf);
            prt_elemtype = true;
        }
        if (ARR_elem_num_buf(ir) != nullptr) {
            if (prt_elemtype) {
                prt(rm, ",");
            }
            prt(rm, "dim");
            UINT dim = 0;
            prt(rm, "[");
            for (IR const* sub = ARR_sub_list(ir); sub != nullptr;) {
                prt(rm, "%d", ((CStArray*)ir)->getElementNumOfDim(dim));
                sub = sub->get_next();
                if (sub != nullptr) {
                    prt(rm, ",");
                }
                dim++;
            }
            prt(rm, "]");
        }
    }
    if (ir->is_terminate()) {
        if (!first) { prt(rm, ","); }
        else { prt(rm, ":("); }
        prt(rm, "terminate");
        first = false;
    }
    if (ir->is_atomic()) {
        if (!first) { prt(rm, ","); }
        else { prt(rm, ":("); }
        prt(rm, "atom");
        first = false;
    }
    if (ir->is_rmw()) {
        if (!first) { prt(rm, ","); }
        else { prt(rm, ":("); }
        prt(rm, "rmw");
        first = false;
    }
    if (ir->hasSideEffect()) {
        if (!first) { prt(rm, ","); }
        else { prt(rm, ":("); }
        prt(rm, "sideeffect");
        first = false;
    }
    if (ir->isNoMove()) {
        if (!first) { prt(rm, ","); }
        else { prt(rm, ":("); }
        prt(rm, "nomove");
        first = false;
    }
    if (ir->isCallStmt() && CALL_dummyuse(ir) != nullptr) {
        if (!first) { prt(rm, ","); }
        else { prt(rm, ":("); }
        prt(rm, "use(");
        rm->getLogMgr()->incIndent(DUMP_INDENT_NUM);
        for (IR * p = CALL_dummyuse(ir); p != nullptr; p = p->get_next()) {
            if (p != CALL_dummyuse(ir)) {
                prt(rm, ",");
            }
            dumpGR(p, tm, ctx);
        }
        prt(rm, ")");
        first = false;
        rm->getLogMgr()->decIndent(DUMP_INDENT_NUM);
    }
    if (!first) { prt(rm, ")"); }
}


CHAR const* compositeName(Sym const* n, xcom::StrBuf & buf)
{
    for (CHAR const* p = SYM_name(n); *p != 0; p++) {
        if (p == SYM_name(n)) {
            if (!xisalpha(*p) && *p != '_') {
                buf.sprint("@\"%s\"", SYM_name(n));
                return buf.buf;
            }
        } else {
            if (!xisalpha(*p) && *p != '_' && !xisdigit(*p)) {
                buf.sprint("@\"%s\"", SYM_name(n));
                return buf.buf;
            }
        }
    }
    return SYM_name(n);
}


static void dumpArrSubList(IR const* ir, UINT dn,
                           TypeMgr * tm, DumpGRCtx * ctx)
{
    if (ARR_sub_list(ir) == nullptr) { return; }
    RegionMgr * rm = tm->getRegionMgr();
    rm->getLogMgr()->incIndent(dn);
    prt(rm, "(");
    for (IR * s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
        if (s != ARR_sub_list(ir)) {
            prt(rm, ",");
        }
        dumpGR(s, tm, ctx);
    }
    prt(rm, ")");
    rm->getLogMgr()->decIndent(dn);
}


void dumpGR(IR const* ir, TypeMgr * tm, DumpGRCtx * ctx)
{
    UINT dn = DUMP_INDENT_NUM;
    RegionMgr * rm = tm->getRegionMgr();
    if (!rm->isLogMgrInit() || ir == nullptr) { return; }
    ASSERT0(tm);

    StrBuf buf(64);
    StrBuf buf2(64);
    Type const* d = ir->getType();
    LogMgr * lm = tm->getRegionMgr()->getLogMgr();
    switch (ir->getCode()) {
    case IR_ST:
        note(lm, "\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir, rm);
        dumpProp(ir, tm, ctx);
        buf.clean();
        prt(lm, " %s = ", compositeName(ST_idinfo(ir)->get_name(), buf));
        lm->incIndent(dn);
        dumpGR(ST_rhs(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_STPR:
        note(lm, "\n%s", IRNAME(ir));
        prt(lm, " $%d:%s", STPR_no(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        prt(lm, " = ");
        lm->incIndent(dn);
        dumpGR(STPR_rhs(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_SETELEM:
        note(lm, "\n%s", IRNAME(ir));
        prt(lm, " $%d:%s", SETELEM_prno(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        prt(lm, " = ");
        lm->incIndent(dn);
        dumpGR(SETELEM_base(ir), tm, ctx);
        prt(lm, ",");
        dumpGR(SETELEM_val(ir), tm, ctx);
        prt(lm, ",");
        dumpGR(SETELEM_ofst(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_GETELEM:
        note(lm, "\n%s", IRNAME(ir));
        prt(lm, " $%d:%s", GETELEM_prno(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        prt(lm, " = ");
        lm->incIndent(dn);
        dumpGR(GETELEM_base(ir), tm, ctx);
        prt(lm, ",");
        dumpGR(GETELEM_ofst(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_STARRAY:
        note(lm, "\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir, rm);
        dumpProp(ir, tm, ctx);
        prt(lm, " = ");

        lm->incIndent(dn);
        dumpGR(ARR_base(ir), tm, ctx);
        prt(lm, ", ");
        lm->decIndent(dn);

        dumpArrSubList(ir, dn, tm, ctx);
        prt(lm, ", ");

        lm->incIndent(dn);
        dumpGR(STARR_rhs(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_IST:
        note(lm, "\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir, rm);
        dumpProp(ir, tm, ctx);
        prt(lm, " = ");
        lm->incIndent(dn);
        dumpGRList(IST_base(ir), tm, ctx);
        prt(lm, ",");
        dumpGRList(IST_rhs(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_LD:
        note(lm, "\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir, rm);
        dumpProp(ir, tm, ctx);
        buf.clean();
        prt(lm, " %s", compositeName(LD_idinfo(ir)->get_name(), buf));
        break;
    case IR_ILD:
        note(lm, "\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir, rm);
        dumpProp(ir, tm, ctx);
        prt(lm, " ");
        lm->incIndent(dn);
        dumpGRList(ILD_base(ir), tm, ctx);
        lm->decIndent(dn);
        break;
    case IR_PR:
        note(lm, "\n$%d:%s", PR_no(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        break;
    case IR_ID:
        note(lm, "\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        buf.clean();
        prt(lm, " %s", compositeName(ID_info(ir)->get_name(), buf));
        break;
    case IR_CONST:
        if (ir->is_sint()) {
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            //Prefer print imm according to its type.
            CHAR const* intfmt = "%d:%s";

            //WORKAROUND:GR parser has bug in parsing lalme negative number.
            //CHAR const* intfmt = "%x:%s";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            //Prefer print imm according to its type.
            CHAR const* intfmt = "%lld:%s";

            //WORKAROUND:GR parser has bug in parsing lalme negative number.
            //CHAR const* intfmt = "0x%llx:%s";
            #else
            #error "Need to support";
            #endif
            prt(lm, intfmt, CONST_int_val(ir), tm->dump_type(d, buf));
        } else if (ir->is_uint()) {
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            CHAR const* intfmt = "%u:%s";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            CHAR const* intfmt = "%llu:%s";
            #else
            #error "Need to support";
            #endif
            prt(lm, intfmt, CONST_int_val(ir), tm->dump_type(d, buf));
        } else if (ir->is_fp()) {
            CHAR fpformat[128];
            ::snprintf(fpformat, 127, "%%.%df:%%s ", CONST_fp_mant(ir));
            prt(lm, fpformat, CONST_fp_val(ir), tm->dump_type(d, buf));
        } else if (ir->is_bool()) {
            prt(lm, "%d:%s", (UINT)CONST_int_val(ir), tm->dump_type(d, buf));
        } else if (ir->is_str()) {
            CHAR * tbuf = SYM_name(CONST_str_val(ir));
            //Remove \n to show string in one line.
            if (ctx != nullptr && ctx->dump_string_in_one_line) {
                size_t len = ::strlen(SYM_name(CONST_str_val(ir)));
                tbuf = (CHAR*)::malloc(len);
                tbuf[0] = 0;
                xstrcat(tbuf, len, "%s", SYM_name(CONST_str_val(ir)));
                for (UINT i = 0; i < len && tbuf[i] != 0; i++) {
                    if (tbuf[i] == '\n') { tbuf[i] = ' '; }
                }
            }
            prt(lm, "\"%s\"", tbuf);
            if (tbuf != SYM_name(CONST_str_val(ir))) {
                ::free(tbuf);
            }
        } else if (ir->is_mc()) {
            //Imm may be MC type.
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            CHAR const* intfmt = "%u:%s";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            CHAR const* intfmt = "%llu:%s";
            #else
            #error "Need to support";
            #endif
            prt(lm, intfmt, CONST_int_val(ir), tm->dump_type(d, buf));
        } else {
            //Dump as HOST_INT type even if it is unrecognized,
            //leave the sanity check to verify().
            //Note the dump format may extend or truncate the real value.
            //Imm may be MC type.
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            CHAR const* intfmt = "%u:%s";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            CHAR const* intfmt = "%llu:%s";
            #else
            #error "Need to support";
            #endif
            prt(lm, intfmt, CONST_int_val(ir), tm->dump_type(d, buf));
        }
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        note(lm, "\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        prt(lm, " ");
        lm->incIndent(dn);
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == nullptr) { continue; }
            if (i != 0) {
                prt(lm, ", ");
            }
            dumpGRList(k, tm, ctx);
        }
        lm->decIndent(dn);
        break;
    case IR_IF:
        note(lm, "\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(lm, " (");

        lm->incIndent(dn);
        dumpGRList(IF_det(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ")");

        note(lm, "\n{");
        lm->incIndent(dn);
        dumpGRList(IF_truebody(ir), tm, ctx);
        lm->decIndent(dn);
        note(lm, "\n}");

        if (IF_falsebody(ir)) {
            note(lm, "\nelse");
            note(lm, "\n{");
            lm->incIndent(dn);
            dumpGRList(IF_falsebody(ir), tm, ctx);
            lm->decIndent(dn);
            note(lm, "\n}");
        }
        prt(lm, ";");
        break;
    case IR_DO_WHILE:
        note(lm, "\ndo");
        dumpProp(ir, tm, ctx);
        prt(lm, " {");

        lm->incIndent(dn);
        dumpGRList(LOOP_body(ir), tm, ctx);
        lm->decIndent(dn);

        note(lm, "\n} while (");
        lm->incIndent(dn);
        dumpGRList(LOOP_det(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ");");
        break;
    case IR_WHILE_DO:
        note(lm, "\nwhile");
        dumpProp(ir, tm, ctx);
        prt(lm, " (");

        lm->incIndent(dn);
        dumpGRList(LOOP_det(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ") {");

        lm->incIndent(dn);
        dumpGRList(LOOP_body(ir), tm, ctx);
        lm->decIndent(dn);
        note(lm, "\n};");
        break;
    case IR_DO_LOOP:
        note(lm, "\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(lm, " (");

        lm->incIndent(dn);
        note(lm, "\n//iv");
        dumpGRList(LOOP_iv(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ",");

        lm->incIndent(dn);
        note(lm, "\n//init");
        dumpGRList(LOOP_init(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ",");

        lm->incIndent(dn);
        note(lm, "\n//det");
        dumpGRList(LOOP_det(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ",");

        lm->incIndent(dn);
        note(lm, "\n//step");
        dumpGRList(LOOP_step(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ")");

        note(lm, "\n {");
        lm->incIndent(dn);
        dumpGRList(LOOP_body(ir), tm, ctx);
        lm->decIndent(dn);

        note(lm, "\n};");
        break;
    case IR_BREAK:
    case IR_CONTINUE:
        note(lm, "\n%s;", IRNAME(ir));
        break;
    case IR_RETURN:
        note(lm, "\n%s", IRNAME(ir));
        prt(lm, " ");
        lm->incIndent(dn);
        dumpGR(RET_exp(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_GOTO:
        note(lm, "\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(lm, " ");
        dump_label_ref(ir->getLabel(), rm);
        prt(lm, ";");
        break;
    case IR_IGOTO:
        note(lm, "\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(lm, " (");

        lm->incIndent(dn);
        dumpGRList(IGOTO_vexp(ir), tm, ctx);
        prt(lm, ")");
        lm->decIndent(dn);

        lm->incIndent(dn);
        for (IR * c = IGOTO_case_list(ir); c != nullptr; c = c->get_next()) {
            dumpGR(c, tm, ctx);
            prt(lm, ", ");
        }
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_LABEL:
        {
            LabelInfo const* li = LAB_lab(ir);
            note(lm, "\n");
            dump_lab_decl(li, rm);
        }
        prt(lm, ";");
        break;
    case IR_SELECT: //formulized log_OR_exp?exp:cond_exp
        note(lm, "\nselect:%s", tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);

        lm->incIndent(dn);
        dumpGRList(SELECT_pred(ir), tm, ctx);
        prt(lm, ",");
        lm->decIndent(dn);

        lm->incIndent(dn);
        dumpGRList(SELECT_trueexp(ir), tm, ctx);
        prt(lm, ",");
        lm->decIndent(dn);

        lm->incIndent(dn);
        dumpGRList(SELECT_falseexp(ir), tm, ctx);
        lm->decIndent(dn);
        break;
    case IR_LDA:
        note(lm, "\n%s", IRNAME(ir));
        dumpOffset(ir, rm);
        dumpProp(ir, tm, ctx);
        buf.clean();
        prt(lm, " %s", compositeName(LDA_idinfo(ir)->get_name(), buf));
        break;
    case IR_PHI:
        note(lm, "\n%s $%d:%s = ", IRNAME(ir),
             PHI_prno(ir), tm->dump_type(d, buf));
        lm->incIndent(dn);
        ASSERT0(ctx->cfg && ir->getBB());
        {
            List<IRBB*> preds;
            ctx->cfg->get_preds(preds, ir->getBB());
            BBListIter bbct = nullptr;
            ASSERT0(preds.get_elem_count() ==
                    xcom::cnt_list(PHI_opnd_list(ir)));
            preds.get_head(&bbct);
            for (IR * opnd = PHI_opnd_list(ir);
                 opnd != nullptr;
                 opnd = opnd->get_next(), bbct = preds.get_next(bbct)) {
                if (opnd != PHI_opnd_list(ir)) {
                    prt(lm, ",");
                }
                xcom::C<LabelInfo const*> * lct;
                LabelInfo const* lab = bbct->val()->
                    getLabelListConst().get_head(&lct);
                if (lab == nullptr) {
                    //Add label because each opnd of PHI has to correspond to
                    //an unique label.
                    lab = ctx->cfg->getRegion()->genILabel();
                    ctx->cfg->addLabel(bbct->val(), lab);
                }
                prt(lm, "(");
                dumpGR(opnd, tm, ctx);
                prt(lm, ",");
                dump_label_ref(lab, rm);
                prt(lm, ")");
            }
        }
        lm->decIndent(dn);
        prt(lm, ";");
        break;
    case IR_SWITCH:
        note(lm, "\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(lm, " (");
        lm->incIndent(dn);
        dumpGRList(SWITCH_vexp(ir), tm, ctx);
        prt(lm, ") ");
        lm->decIndent(dn);
        if (SWITCH_deflab(ir) != nullptr) {
            note(lm, "\ndefault ");
            dump_label_ref(ir->getLabel(), rm);
            prt(lm, ", ");
        }

        for (IR * c = SWITCH_case_list(ir); c != nullptr; c = c->get_next()) {
            dumpGR(c, tm, ctx);
            prt(lm, ", ");
        }

        if (SWITCH_body(ir) != nullptr) {
            note(lm, "\n{ ");
            lm->incIndent(dn);
            dumpGRList(SWITCH_body(ir), tm, ctx);
            lm->decIndent(dn);
            note(lm, "\n}");
        }
        prt(lm, ";");
        break;
    case IR_CASE:
        ASSERT0(CASE_vexp(ir));
        ASSERT0(CASE_lab(ir));
        note(lm, "\ncase");
        dumpProp(ir, tm, ctx);
        prt(lm, " ");
        lm->incIndent(dn);
        dumpGRList(CASE_vexp(ir), tm, ctx);
        prt(lm, ", ");
        dump_label_ref(ir->getLabel(), rm);
        lm->decIndent(dn);
        break;
    case IR_ARRAY:
        note(lm, "\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir, rm);
        dumpProp(ir, tm, ctx);

        lm->incIndent(dn);
        dumpGR(ARR_base(ir), tm, ctx);
        prt(lm, ", ");
        lm->decIndent(dn);

        dumpArrSubList(ir, dn, tm, ctx);
        break;
    case IR_CALL:
    case IR_ICALL:
        note(lm, "\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(lm, " ");
        if (ir->hasReturnValue()) {
            prt(lm, "$%d:%s = ", CALL_prno(ir), tm->dump_type(d, buf));
        }
        if (ir->is_icall()) {
            lm->incIndent(dn);
            dumpGR(ICALL_callee(ir), tm, ctx);
            prt(lm, ", ");
            lm->decIndent(dn);
        } else {
            buf.clean();
            prt(lm, "%s", compositeName(CALL_idinfo(ir)->get_name(), buf));
        }
        prt(lm, "(");
        lm->incIndent(dn);
        for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
            if (p != CALL_param_list(ir)) {
                prt(lm, ",");
            }
            dumpGR(p, tm, ctx);
        }
        lm->decIndent(dn);
        prt(lm, ")");
        prt(lm, ";");
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        note(lm, "\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(lm, " (");
        lm->incIndent(dn);
        dumpGRList(BR_det(ir), tm, ctx);
        lm->decIndent(dn);
        prt(lm, "), ");
        dump_label_ref(ir->getLabel(), rm);
        prt(lm, ";");
        break;
    case IR_REGION:
        ASSERT0(REGION_ru(ir));
        if (ctx != nullptr && ctx->dump_inner_region) {
            //lm->incIndent(dn);
            ASSERT0(REGION_ru(ir));
            REGION_ru(ir)->dumpGR(ctx->dump_inner_region);
            //lm->decIndent(dn);
        } else {
            note(lm, "\nregion ");
            switch (REGION_ru(ir)->getRegionType()) {
            case REGION_PROGRAM: prt(lm, "program "); break;
            case REGION_BLACKBOX: prt(lm, "blackbox "); break;
            case REGION_FUNC: prt(lm, "func "); break;
            case REGION_INNER: prt(lm, "inner "); break;
            default: ASSERT0(0); //TODO
            }
            if (REGION_ru(ir)->getRegionVar() != nullptr) {
                prt(lm, "%s ",
                    SYM_name(REGION_ru(ir)->getRegionVar()->get_name()));
            }
        }
        prt(lm, ";");
        break;
    case IR_UNDEF:
        note(lm, "\nundef!");
        break;
    default:
        ASSERTN(0, ("unknown IR type:%s", IRNAME(ir)));
        return ;
    }
}


void dumpGRList(IR * irlist, TypeMgr * tm, DumpGRCtx * ctx)
{
    for (IR * ir = irlist; ir != nullptr; ir = ir->get_next()) {
        dumpGR(ir, tm, ctx);
    }
}


void dumpGRInBBList(List<IRBB*> * bblist, TypeMgr * tm, DumpGRCtx * ctx)
{
    ASSERT0(bblist);
    BBListIter bbct = nullptr;
    RegionMgr * rm = tm->getRegionMgr();
    for (bblist->get_head(&bbct);
         bbct != bblist->end(); bbct = bblist->get_next(bbct)) {
        IRBB * bb = bbct->val();
        xcom::C<LabelInfo const*> * labct;
        for (bb->getLabelListConst().get_head(&labct);
            labct != bb->getLabelListConst().end();
            labct = bb->getLabelListConst().get_next(labct)) {
            LabelInfo const* li = labct->val();
            ASSERT0(li);
            note(rm, "\n");
            dump_lab_decl(li, rm);
            prt(rm, ";");
        }

        IRListIter irct = nullptr;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            ASSERT0(ir);
            dumpGR(ir, tm, ctx);
        }
    }
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
    if (must1 != nullptr && must2 != nullptr && must1->is_overlap(must2)) {
        return false;
    }
    if (must1 != nullptr &&
        may2 != nullptr &&
        may2->is_contain_only_taken_addr(must1)) {
        return false;
    }
    if (must2 != nullptr &&
        may1 != nullptr &&
        may1->is_contain_only_taken_addr(must2)) {
        return false;
    }
    if (may1 != nullptr && may2 != nullptr && may1->is_intersect(*may2)) {
        return false;
    }
    return true;
}


bool IR::isNotOverlap(IR const* ir2, Region * rg) const
{
    ASSERT0(rg);
    IR const* ir1 = this;
    ASSERT0(ir1 && ir2);
    if (!ir1->getType()->is_scalar() ||
        !ir2->getType()->is_scalar() ||
        !ir1->hasOffset() ||
        !ir2->hasOffset()) {
        return false;
    }
    UINT offset1 = ir1->getOffset();
    UINT offset2 = ir2->getOffset();
    UINT size1 = rg->getTypeMgr()->getByteSize(ir1->getType());
    UINT size2 = rg->getTypeMgr()->getByteSize(ir2->getType());
    if ((offset1 + size1 <= offset2) || (offset2 + size2 <= offset1)) {
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
//END IR


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
//END CSwitch


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

} //namespace xoc
