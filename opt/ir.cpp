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

namespace xoc {

#define PADDR(ir) (dump_addr ? prt(" 0x%p", (ir)) : 0)

IRDesc const g_ir_desc[] = {
    { IR_UNDEF,    "undef",        0x0, 0, 0,
      0,},
    { IR_CONST,    "const",        0x0, 0, sizeof(CConst),
      IRT_IS_LEAF,},
    { IR_ID,       "id",           0x0, 0, sizeof(CId),
      IRT_HAS_IDINFO|IRT_IS_LEAF|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU,},
    { IR_LD,       "ld",           0x0, 0, sizeof(CLd),
      IRT_HAS_IDINFO|IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_LEAF|
      IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET,},
    { IR_ILD,      "ild",          0x1, 1, sizeof(CIld),
      IRT_IS_UNA|IRT_IS_MEM_REF|IRT_IS_MEM_OPND|
      IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET,  },
    { IR_PR,       "pr",           0x0, 0, sizeof(CPr),
      IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_LEAF|IRT_HAS_DU,},
    { IR_ARRAY,    "array",        0x3, 2, sizeof(CArray),
      IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_NON_PR_MEMREF|
      IRT_HAS_DU|IRT_HAS_OFFSET, },
    { IR_ST,       "st",           0x1, 1, sizeof(CSt),
      IRT_HAS_IDINFO|IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET, },
    { IR_STPR,     "stpr",         0x1, 1, sizeof(CStpr),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR, },
    { IR_STARRAY,  "starray",      0x7, 3, sizeof(CStArray),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET, },
    { IR_IST,      "ist",          0x3, 2, sizeof(CIst),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_IS_NON_PR_MEMREF|IRT_HAS_DU|IRT_HAS_OFFSET, },
    { IR_SETELEM,  "setelem",      0x7, 3, sizeof(CSetElem),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR, },
    { IR_GETELEM,  "getelem",      0x3, 2, sizeof(CGetElem),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR, },
    { IR_CALL,     "call",         0x3, 2, sizeof(CCall),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_HAS_IDINFO|IRT_IS_STMT_IN_BB|IRT_HAS_DU, },
    { IR_ICALL,    "icall",        0x7, 3, sizeof(CICall),
      IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU, },
    { IR_LDA,      "lda",          0x0, 0, sizeof(CLda),
      IRT_HAS_IDINFO|IRT_IS_UNA|IRT_IS_LEAF|IRT_HAS_OFFSET, },
    { IR_ADD,      "add",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE, },
    { IR_SUB,      "sub",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE, },
    { IR_MUL,      "mul",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE, },
    { IR_DIV,      "div",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN, },
    { IR_REM,      "rem",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN, },
    { IR_MOD,      "mod",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN, },
    { IR_LAND,     "land",         0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_LOGICAL, },
    { IR_LOR,      "lor",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_LOGICAL, },
    { IR_BAND,     "band",         0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE, },
    { IR_BOR,      "bor",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE, },
    { IR_XOR,      "xor",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE,},
    { IR_ASR,      "asr",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN, },
    { IR_LSR,      "lsr",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN, },
    { IR_LSL,      "lsl",          0x3, 2, sizeof(CBin),
      IRT_IS_BIN, },
    { IR_LT,       "lt",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION, },
    { IR_LE,       "le",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION, },
    { IR_GT,       "gt",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION, },
    { IR_GE,       "ge",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_RELATION, },
    { IR_EQ,       "eq",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE|IRT_IS_RELATION,},
    { IR_NE,       "ne",           0x3, 2, sizeof(CBin),
      IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE|IRT_IS_RELATION,},
    { IR_BNOT,     "bnot",         0x1, 1, sizeof(CUna),
      IRT_IS_UNA, },
    { IR_LNOT,     "lnot",         0x1, 1, sizeof(CUna),
      IRT_IS_UNA|IRT_IS_LOGICAL, },
    { IR_NEG,      "neg",          0x1, 1, sizeof(CUna),
      IRT_IS_UNA, },
    { IR_CVT,      "cvt",          0x1, 1, sizeof(CCvt),
      IRT_IS_UNA, },
    { IR_GOTO,     "goto",         0x0, 0, sizeof(CGoto),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB, },
    { IR_IGOTO,    "igoto",        0x3, 2, sizeof(CIGoto),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB, },
    { IR_DO_WHILE, "dowhile",      0x3, 2, sizeof(CDoWhile),
      IRT_IS_STMT, },
    { IR_WHILE_DO, "whiledo",      0x3, 2, sizeof(CWhileDo),
      IRT_IS_STMT, },
    { IR_DO_LOOP,  "doloop",       0x1F,5, sizeof(CDoLoop),
      IRT_IS_STMT, },
    { IR_IF,       "if",           0x7, 3, sizeof(CIf),
      IRT_IS_STMT, },
    { IR_LABEL,    "label",        0x0, 0, sizeof(CLab),
      IRT_IS_STMT, },
    { IR_SWITCH,   "switch",       0x7, 3, sizeof(CSwitch),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB, },
    { IR_CASE,     "case",         0x1, 1, sizeof(CCase),
      0, },
    { IR_TRUEBR,   "truebr",       0x1, 1, sizeof(CTruebr),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB, },
    { IR_FALSEBR,  "falsebr",      0x1, 1, sizeof(CFalsebr),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB, },
    { IR_RETURN,   "return",       0x1, 1, sizeof(CRet),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB, },
    { IR_SELECT,   "select",       0x7, 3, sizeof(CSelect),
      0, },
    { IR_BREAK,    "break",        0x0, 0, sizeof(CBreak),
      IRT_IS_STMT, },
    { IR_CONTINUE, "continue",     0x0, 0, sizeof(CContinue),
      IRT_IS_STMT, },
    { IR_PHI,      "phi",          0x1, 1, sizeof(CPhi),
      IRT_IS_STMT|IRT_HAS_RESULT|IRT_IS_MEM_REF|
      IRT_IS_STMT_IN_BB|IRT_HAS_DU|IRT_WRITE_PR|IRT_WRITE_WHOLE_PR, },
    { IR_REGION,   "region",       0x0, 0, sizeof(CRegion),
      IRT_IS_STMT|IRT_IS_STMT_IN_BB, },
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
    for (IR * ir = irlst; ir != NULL; ir = ir->get_next()) {
        ASSERT0(ir->is_stmt());
    }
    return true;
}


bool allBeExp(IR * irlst)
{
    for (IR * ir = irlst; ir != NULL; ir = ir->get_next()) {
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


IR const* checkIRTOnlyIcall(IR const* ir)
{
    ASSERT0(ir->is_icall());
    return ir;
}


UINT checkArrayDimension(IR const* ir, UINT n)
{
    UINT i = 0;
    for (IR const* sub = ARR_sub_list(ir); sub != NULL; sub = sub->get_next()) {
        i++;
    }
    ASSERT0(n < i);
    return n;
}


UINT checkStArrayDimension(IR const* ir, UINT n)
{
    UINT i = 0;
    for (IR const* sub = ARR_sub_list(ir); sub != NULL; sub = sub->get_next()) {
        i++;
    }
    ASSERT0(n < i);
    return n;
}
#endif


//Dump IR, and both its kids and siblings.
void dumpIRListH(IR * ir_list, Region * rg, CHAR * attr, UINT dumpflag)
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP IR List ----==");
    dumpIRList(ir_list, rg, attr, dumpflag);
}


//Dump IR, and both its kids and siblings.
void dumpIRList(IR * ir_list, Region * rg, CHAR * attr, UINT dumpflag)
{
    if (g_tfile == NULL) { return; }
    note("");
    bool first_one = true;
    for (; ir_list != NULL; ir_list = ir_list->get_next()) {
        if (first_one) {
            first_one = false;
            dumpIR(ir_list, rg, attr, dumpflag);
        } else {
            dumpIR(ir_list, rg, attr, dumpflag);
        }
    }
}


static void verifyIR(IR * ir, IRAddressHash * irh, Region const* rg)
{
    ASSERT0(irh != NULL);
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k != NULL) {
            ASSERTN(k->getParent() == ir, ("ir must be k's parent"));
            verifyIRList(k, irh, rg);
        }
    }

    //IR can not be used more than twice. Since there will cause
    //memory crash during freeIR().
    ASSERTN(!irh->find(ir), ("IR has been used again"));
    irh->append(ir);
    ir->verify(rg);
}


//Check for IR and IRBB sanity and uniqueness.
//Ensure that all IRs must be embedded into a basic block.
//Ensure that PHI must be the first stmt in basic block.
bool verifyIRandBB(BBList * bblst, Region const* rg)
{
    IRAddressHash irh;
    for (IRBB * bb = bblst->get_head();
         bb != NULL; bb = bblst->get_next()) {
        bool should_not_phi = false;

        xcom::C<IR*> * irct;
        for (IR * ir = BB_irlist(bb).get_head(&irct);
             ir != NULL; ir = BB_irlist(bb).get_next(&irct)) {
            ASSERT0(ir->is_single());
            ASSERT0(IR_parent(ir) == NULL);
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
        for (IR * p = ir_list; p != NULL; p = p->get_next()) {
            ASSERT0(p->is_stmt());
            ASSERT0(IR_parent(p) == NULL);
        }
    }
    return true;
}


//Check for IR sanity and uniqueness.
bool verifyIRList(IR * ir, IRAddressHash * irh, Region const* rg)
{
    IRAddressHash * loc = NULL;
    if (irh == NULL) {
        loc = new IRAddressHash();
        irh = loc;
    }
    while (ir != NULL) {
        verifyIR(ir, irh, rg);
        ir = ir->get_next();
    }
    if (loc != NULL) {
        delete loc;
    }
    return true;
}


void dumpIRList(IN List<IR*> & ir_list, Region * rg)
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP IR List ----==\n");
    ASSERT0(rg);
    for (IR * ir = ir_list.get_head();
         ir != NULL; ir = ir_list.get_next()) {
        ASSERT0(ir->is_single());
        dumpIR(ir, rg);
    }
    fflush(g_tfile);
}


void dumpIRList(IRList & ir_list, Region * rg)
{
    dumpIRList((List<IR*>&)ir_list, rg);
}


static void dump_lab_decl(LabelInfo const* li)
{
    if (LABEL_INFO_type(li) == L_ILABEL) {
        prt("label " ILABEL_STR_FORMAT "", ILABEL_CONT(li));
    } else if (LABEL_INFO_type(li) == L_CLABEL) {
        prt("label " CLABEL_STR_FORMAT "", CLABEL_CONT(li));
    } else if (LABEL_INFO_type(li) == L_PRAGMA) {
        ASSERT0(LABEL_INFO_pragma(li));
        prt("pragma %s", SYM_name(LABEL_INFO_pragma(li)));
    } else { ASSERTN(0, ("unknown label type")); }

    bool first = true;
    if (LABEL_INFO_b1(li) != 0) {
        prt("(");
    }

    if (LABEL_INFO_is_try_start(li)) {
        if (!first) {
            prt(",");
        }
        first = false;
        prt("try_start ");
    }

    if (LABEL_INFO_is_try_end(li)) {
        if (!first) {
            prt(",");
        }
        first = false;
        prt("try_end ");
    }

    if (LABEL_INFO_is_catch_start(li)) {
        if (!first) {
            prt(",");
        }
        first = false;
        prt("catch_start ");
    }

    if (LABEL_INFO_is_terminate(li)) {
        if (!first) {
            prt(",");
        }
        first = false;
        prt("terminate ");
    }

    if (LABEL_INFO_b1(li) != 0) {
        prt(")");
    }
}


static void dump_label_ref(LabelInfo const* li)
{
    if (LABEL_INFO_type(li) == L_ILABEL) {
        prt(ILABEL_STR_FORMAT "", ILABEL_CONT(li));
    } else if (LABEL_INFO_type(li) == L_CLABEL) {
        prt(CLABEL_STR_FORMAT "", CLABEL_CONT(li));
    } else {
        ASSERTN(0, ("unknown label type"));
    }
}


static void dump_ai(OUT CHAR * buf, IR const* ir)
{
    ASSERT0(ir && buf);
    AIContainer const* ai = ir->getAI();
    if (ai == NULL) { return; }

    AICont const& cont = ai->read_cont();

    if (!cont.is_init()) { return; }

    strcat(buf, " ai:");
    CHAR * p = buf + strlen(buf);
    bool not_first = false;
    for (UINT i = 0; i < cont.get_capacity(); i++) {
        BaseAttachInfo * ac = cont.get(i);
        if (ac == NULL) { continue; }

        if (!not_first) {
            not_first = true;
        } else {
            sprintf(p, ",");
            p = p + strlen(p);
        }

        sprintf(p, "%s", ai->get_ai_name(ac->type));
        p = p + strlen(p);
    }
}


//Dump IR and all of its kids.
//'attr': miscellaneous string which following 'ir'.
void dumpIR(IR const* ir, Region * rg, IN CHAR * attr, UINT dumpflag)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_src_line = HAVE_FLAG(dumpflag, IR_DUMP_SRC_LINE);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
	bool dump_inner_region = HAVE_FLAG(dumpflag, IR_DUMP_INNER_REGION);
    DUMMYUSE(dump_src_line);
    DUMMYUSE(dump_kid);
    DUMMYUSE(dump_addr);
	DUMMYUSE(dump_inner_region);
    DUMMYUSE(attr);
    DUMMYUSE(rg);
    DUMMYUSE(ir);
    TypeMgr * tm = rg->getTypeMgr();

    ASSERT0(tm);
    UINT dn = 4;
    if (g_tfile == NULL || ir == NULL) { return; }

    //Attribution string do NOT exceed length of 128 chars.
    static CHAR attr_buf[128];
    if (attr == NULL) {
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

    dump_ai(p, ir);

    //Record type info and var decl.
    StrBuf buf(64);
    StrBuf buf2(64);

    if (g_dbx_mgr != NULL && dump_src_line) {
        g_dbx_mgr->printSrcLine(ir);
    }

    Type const* d = NULL;
    if (ir->getType() != NULL) {
        d = ir->getType();
    }

    TypeMgr * xdm = const_cast<TypeMgr*>(tm);
    switch (ir->getCode()) {
    case IR_ST: {
        CHAR tt[40];
        tt[0] = 0;
        CHAR * name = xstrcat(tt, 40, "%s",
        SYM_name(ST_idinfo(ir)->get_name()));

        //Dump operator and variable name.
        note("\nst:%s", xdm->dump_type(d, buf));
        if (ST_ofst(ir) != 0) {
            prt(":offset(%d)", ST_ofst(ir));
        }
        prt(" '%s'", name);

        //Dump declaration info if the frontend supplied.
        buf.clean();
        if (ST_idinfo(ir)->dumpVARDecl(buf) != NULL) {
            prt(" decl:%s", buf.buf);
        }

        PADDR(ir);
        prt("%s", attr);

        if (dump_kid) {
            g_indent += dn;
            dumpIRList(ST_rhs(ir), rg, NULL, dumpflag);
            g_indent -= dn;
        }
        break;
    }
    case IR_STPR:
        note("\nstpr $%d:%s", STPR_no(ir), xdm->dump_type(d, buf));
        PADDR(ir);
        prt("%s", attr);

        if (dump_kid) {
            g_indent += dn;
            dumpIRList(STPR_rhs(ir), rg, NULL, dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_SETELEM:
        note("\nsetelem $%d:%s", SETELEM_prno(ir), xdm->dump_type(d, buf));
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(SETELEM_base(ir), rg, NULL, dumpflag);
            dumpIRList(SETELEM_val(ir), rg, NULL, dumpflag);
            dumpIRList(SETELEM_ofst(ir), rg, (CHAR*)" offset", dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_GETELEM:
        note("\ngetelem $%d:%s", GETELEM_prno(ir), xdm->dump_type(d, buf));
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(GETELEM_base(ir), rg, (CHAR*)" base", dumpflag);
            dumpIRList(GETELEM_ofst(ir), rg, (CHAR*)" offset", dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_STARRAY:
        if (ARR_ofst(ir) != 0) {
            note("\nstarray (%s:offset(%d), ety:%s)",
                 xdm->dump_type(d, buf),
                 ARR_ofst(ir),
                 xdm->dump_type(ARR_elemtype(ir), buf2));
        } else {
            note("\nstarray (%s, ety:%s)",
                 xdm->dump_type(d, buf),
                 xdm->dump_type(ARR_elemtype(ir), buf2));
        }

        PADDR(ir);
        prt("%s", attr);
        if (ARR_sub_list(ir) != NULL && dump_kid) {
            //Dump elem number.
            g_indent += dn;
            UINT dim = 0;
            if (ARR_elem_num_buf(ir) != NULL) {
                note("\nelem_num[");
                for (IR const* sub = ARR_sub_list(ir); sub != NULL;) {
                    prt("%d",
                        ((CArray*)ir)->getElementNumOfDim(dim));
                    sub = sub->get_next();
                    if (sub != NULL) {
                        prt(",");
                    }
                    dim++;
                }
                prt("]");
            } else { note("\nelem_num[--]"); }

            //Dump sub exp list.
            dim = 0;
            for (IR const* sub = ARR_sub_list(ir);
                 sub != NULL; sub = sub->get_next()) {
                CHAR tt[40];
                sprintf(tt, " dim%d", dim);
                dumpIR(sub, rg, (CHAR*)tt, dumpflag);
                dim++;
            }
            g_indent -= dn;
        }
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(ARR_base(ir), rg, (CHAR*)" array_base", dumpflag);
            dumpIRList(STARR_rhs(ir), rg, (CHAR*)" rhs", dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_IST:
        if (IST_ofst(ir) != 0) {
            note("\nist:%s:offset(%d)", xdm->dump_type(d, buf), IST_ofst(ir));
        } else {
            note("\nist:%s", xdm->dump_type(d, buf));
        }

        //Dump IR address.
        PADDR(ir);
        prt("%s", attr);

        if (dump_kid) {
            g_indent += dn;
            dumpIRList(IST_base(ir), rg, (CHAR*)" base", dumpflag);
            dumpIRList(IST_rhs(ir), rg, NULL, dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_LD: {
        CHAR tt[40];
        tt[0] = 0;

        //Dump variable info.
        CHAR * name = xstrcat(tt, 40, "%s",
            SYM_name(LD_idinfo(ir)->get_name()));

        if (LD_ofst(ir) != 0) {
            note("\nld:%s:offset(%d) '%s'",
                 xdm->dump_type(d, buf), LD_ofst(ir), name);
        } else {
            note("\nld:%s '%s'", xdm->dump_type(d, buf), name);
        }

        //Dump declaration if frontend supplied.
        buf.clean();
        if (LD_idinfo(ir)->dumpVARDecl(buf) != NULL) {
            prt(" decl:%s", buf.buf);
        }

        //Dump IR address.
        PADDR(ir);
        prt("%s", attr);
        break;
    }
    case IR_ILD:
        if (ILD_ofst(ir) != 0) {
            note("\nild:%s:offset(%d)", xdm->dump_type(d, buf), ILD_ofst(ir));
        } else {
            note("\nild:%s", xdm->dump_type(d, buf));
        }

        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(ILD_base(ir), rg, NULL, dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_PR:
        note("\n$%d:%s", PR_no(ir), xdm->dump_type(d, buf));
        PADDR(ir);
        prt("%s", attr);
        break;
    case IR_ID: {
        CHAR tt[40];
        tt[0] = 0;

        //Dump ID name.
        CHAR * name =
            xstrcat(tt, 40, "%s", SYM_name(ID_info(ir)->get_name()));
        note("\nid:%s '%s'", xdm->dump_type(d, buf), name);

        buf.clean();
        if (ID_info(ir)->dumpVARDecl(buf) != NULL) {
            prt(" decl:%s", buf.buf);
        }

        //Dump IR address.
        PADDR(ir);
        prt("%s", attr);
        break;
    }
    case IR_CONST:
        if (ir->is_sint()) {
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            CHAR const* intfmt = "\nintconst:%s %d|0x%x";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            CHAR const* intfmt = "\nintconst:%s %lld|0x%llx";
            #else
            #error "Need to support";
            #endif
            note(intfmt, xdm->dump_type(d, buf),
                 CONST_int_val(ir), CONST_int_val(ir));
        } else if (ir->is_uint()) {
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            CHAR const* intfmt = "\nintconst:%s %u|0x%x";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            CHAR const* intfmt = "\nintconst:%s %llu|0x%llx";
            #else
            #error "Need to support";
            #endif
            note(intfmt, xdm->dump_type(d, buf),
                 CONST_int_val(ir), CONST_int_val(ir));
        } else if (ir->is_fp()) {
            CHAR fpformat[128];
            ::snprintf(fpformat, 127, "\nfpconst:%%s %%.%df", CONST_fp_mant(ir));
            note(fpformat, xdm->dump_type(d, buf), CONST_fp_val(ir));
        } else if (ir->is_bool()) {
            note("\nboolconst:%s %d", xdm->dump_type(d, buf), CONST_int_val(ir));
        } else if (ir->is_str()) {
            UINT const tbuflen = 40;
            CHAR tbuf[tbuflen];
            tbuf[0] = 0;
            xstrcat(tbuf, tbuflen, "%s", SYM_name(CONST_str_val(ir)));

            //Remove \n to show string in one line.
            for (UINT i = 0; i < tbuflen && tbuf[i] != 0; i++) {
                if (tbuf[i] == '\n') { tbuf[i] = ' '; }
            }

            if (strlen(SYM_name(CONST_str_val(ir))) < tbuflen) {
                note("\nstrconst:%s \\\"%s\\\"", xdm->dump_type(d, buf), tbuf);
            } else {
                note("\nstrconst:%s \\\"%s...\\\"", xdm->dump_type(d, buf), tbuf);
            }
        } else if (ir->is_mc()) {
            //Imm may be MC type.
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            CHAR const* intfmt = "\nintconst:%s %u|0x%x";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            CHAR const* intfmt = "\nintconst:%s %llu|0x%llx";
            #else
            #error "Need to support";
            #endif
            note(intfmt, xdm->dump_type(d, buf),
                 CONST_int_val(ir), CONST_int_val(ir));
        } else {
            //Dump as HOST_INT type even if it is unrecognized,
            //leave the sanity check to verify().
            //Note the dump format may extend or truncate the real value.
            note("\nintconst:%s %d|0x%x", xdm->dump_type(d, buf),
                 CONST_int_val(ir),  CONST_int_val(ir));
        }
        PADDR(ir);
        prt("%s", attr);
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        note("\n%s:%s", IRNAME(ir), xdm->dump_type(d, buf));
        if (ir->is_cvt() && CVT_round(ir) != ROUND_UNDEF) {
          prt(":round(%s)", ROUND_NAME(CVT_round(ir)));
        }
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k == NULL) { continue; }
                dumpIRList(k, rg, NULL, dumpflag);
            } 
            g_indent -= dn;
        }
        break;
    case IR_IF:
        note("\nif");
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(IF_det(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\n{");
            g_indent += dn;
            dumpIRList(IF_truebody(ir), rg, NULL, dumpflag);
            g_indent -= dn;
            note("\n}");

            if (IF_falsebody(ir)) {
                note("\nelse");
                note("\n{");
                g_indent += dn;
                dumpIRList(IF_falsebody(ir), rg, NULL, dumpflag);
                g_indent -= dn;
                note("\n}");
            }
        }
        break;
    case IR_DO_WHILE:
        note("\ndowhile");
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            note("\nbody:");
            g_indent += dn;
            dumpIRList(LOOP_body(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\ndet:");
            g_indent += dn;
            dumpIRList(LOOP_det(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\nend_dowhile");
        }
        break;
    case IR_WHILE_DO:
        note("\nwhiledo");
        PADDR(ir);
        prt("%s", attr);

        if (dump_kid) {
            note("\ndet:");
            g_indent += dn;
            dumpIRList(LOOP_det(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\nbody:");

            g_indent += dn;
            dumpIRList(LOOP_body(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\nend_whiledo");
        }
        break;
    case IR_DO_LOOP:
        note("\ndoloop");
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            note("\niv:");
            g_indent += dn;
            dumpIRList(LOOP_iv(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\ninit:");
            g_indent += dn;
            dumpIRList(LOOP_init(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\ndet:");
            g_indent += dn;
            dumpIRList(LOOP_det(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\nstep:");
            g_indent += dn;
            dumpIRList(LOOP_step(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\nbody:");
            g_indent += dn;
            dumpIRList(LOOP_body(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\nend_doloop");
        }
        break;
    case IR_BREAK:
        note("\nbreak");
        PADDR(ir);
        prt("%s", attr);
        break;
    case IR_CONTINUE:
        note("\ncontinue");
        PADDR(ir);
        prt("%s", attr);
        break;
    case IR_RETURN:
        note("\nreturn");
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIR(RET_exp(ir), rg, NULL, dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_GOTO:
        note("\ngoto ");
        dump_lab_decl(ir->getLabel());
        PADDR(ir);
        prt("%s", attr);
        break;
    case IR_IGOTO:
        note("\nigoto");
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(IGOTO_vexp(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            note("\ncase_list");
            g_indent += dn;
            dumpIRList(IGOTO_case_list(ir), rg, NULL, dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_LABEL: {
        LabelInfo const* li = LAB_lab(ir);
        if (LABEL_INFO_type(li) == L_ILABEL) {
            note("\nlabel " ILABEL_STR_FORMAT "",
                 ILABEL_CONT(LAB_lab(ir)));
        } else if (LABEL_INFO_type(li) == L_CLABEL) {
            note("\nlabel " CLABEL_STR_FORMAT "",
                 CLABEL_CONT(LAB_lab(ir)));
        } else if (LABEL_INFO_type(li) == L_PRAGMA) {
            ASSERT0(LABEL_INFO_pragma(LAB_lab(ir)));
            note("\npragma %s", SYM_name(LABEL_INFO_pragma(LAB_lab(ir))));
        } else { UNREACHABLE(); }

        PADDR(ir);

        if (LABEL_INFO_b1(li) != 0) {
            prt("(");
        }

        if (LABEL_INFO_is_try_start(li)) {
            prt("try_start ");
        }

        if (LABEL_INFO_is_try_end(li)) {
            prt("try_end ");
        }

        if (LABEL_INFO_is_catch_start(li)) {
            prt("catch_start ");
        }

        if (LABEL_INFO_is_terminate(li)) {
            prt("terminate ");
        }

        if (LABEL_INFO_b1(li) != 0) {
            prt(")");
        }

        prt("%s", attr);
        break;
    }
    case IR_SELECT: //formulized log_OR_exp?exp:cond_exp
        note("\nselect:%s", xdm->dump_type(d, buf));

        PADDR(ir); prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(SELECT_pred(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            g_indent += dn;
            dumpIRList(SELECT_trueexp(ir), rg, (CHAR*)" true_exp", dumpflag);
            g_indent -= dn;

            g_indent += dn;
            dumpIRList(SELECT_falseexp(ir), rg, (CHAR*)" false_exp", dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_LDA: { //Get address of a symbol
        CHAR tt[40];
        tt[0] = 0;

        //Dump variable info.
        CHAR * name = xstrcat(tt, 40, "%s",
                            SYM_name(LDA_idinfo(ir)->get_name()));
        if (LDA_ofst(ir) != 0) {
            note("\nlda:%s:offset(%d) '%s'",
                 xdm->dump_type(d, buf), LDA_ofst(ir), name);
        } else {
            note("\nlda:%s '%s'", xdm->dump_type(d, buf), name);
        }

        //Dump declaration if frontend supplied.
        buf.clean();
        if (LDA_idinfo(ir)->dumpVARDecl(buf) != NULL) {
            prt(" decl:%s", buf.buf);
        }

        //Dump IR address.
        PADDR(ir);
        prt("%s", attr);
        break;
    }
    case IR_PHI:
        note("\n$%d:%s = phi", PHI_prno(ir), xdm->dump_type(d, buf));

        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            IR * opnd = PHI_opnd_list(ir);
            while (opnd != NULL) {
                dumpIR(opnd, rg, NULL, dumpflag);
                opnd = opnd->get_next();
            }
            g_indent -= dn;
        }
        break;
    case IR_SWITCH:
        note("\nswitch");
        if (SWITCH_deflab(ir) != NULL) {
            prt(", deflab: ");
            dump_lab_decl(ir->getLabel());
        }
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(SWITCH_vexp(ir), rg, NULL, dumpflag);
            g_indent -= dn;

            if (SWITCH_case_list(ir) != NULL) {
                dumpIRList(SWITCH_case_list(ir), rg, NULL, dumpflag);
            }

            if (SWITCH_body(ir) != NULL) {
                note("\nbody:");
                g_indent += dn;
                dumpIRList(SWITCH_body(ir), rg, NULL, dumpflag);
                g_indent -= dn;
            }
            note("\nend_switch");
        }
        break;
    case IR_CASE:
        ASSERT0(CASE_vexp(ir));
        ASSERT0(CASE_lab(ir));
        note("\ncase");
        PADDR(ir);
        prt("%s", attr);

        g_indent += dn;
        dumpIRList(CASE_vexp(ir), rg, NULL, dumpflag);
        note("\n");
        dump_lab_decl(ir->getLabel());
        g_indent -= dn;
        break;
    case IR_ARRAY:
        if (ARR_ofst(ir) != 0) {
            note("\narray (%s:offset(%d), ety:%s)",
                 xdm->dump_type(d, buf),
                 ARR_ofst(ir),
                 xdm->dump_type(ARR_elemtype(ir), buf2));
        } else {
            note("\narray (%s, ety:%s)",
                 xdm->dump_type(d, buf),
                 xdm->dump_type(ARR_elemtype(ir), buf2));
        }

        PADDR(ir);
        prt("%s", attr);
        if (ARR_sub_list(ir) != NULL && dump_kid) {
            //Dump element number if it exist.
            g_indent += dn;

            if (ARR_elem_num_buf(ir) != NULL) {
                UINT dim = 0;
                note("\nelemnum[");
                for (IR const* sub = ARR_sub_list(ir); sub != NULL;) {
                    prt("%d", ARR_elem_num(ir, dim));
                    sub = sub->get_next();
                    if (sub != NULL) {
                        prt(",");
                    }
                    dim++;
                }
                prt("]");
            } else { note("\nelemnum[--]"); }

            //Dump subscript expressions in each dimension.
            UINT dim = 0;
            for (IR const* sub = ARR_sub_list(ir);
                 sub != NULL; sub = sub->get_next()) {
                CHAR tt[40];
                sprintf(tt, " dim%d", dim);
                dumpIR(sub, rg, (CHAR*)tt, dumpflag);
                dim++;
            }
            g_indent -= dn;
        }

        if (dump_kid) {
            g_indent += dn;
            dumpIRList(ARR_base(ir), rg, (CHAR*)" array_base", dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_CALL:
    case IR_ICALL: {
        if (ir->hasReturnValue()) {
            note("\n$%d:%s = ", CALL_prno(ir), xdm->dump_type(d, buf));
        } else {
            note("\n");
        }

        if (ir->is_call()) {
            CHAR tt[44];
            tt[0] = 0;
            CHAR const* string = SYM_name(CALL_idinfo(ir)->get_name());
            CHAR * name = xstrcat(tt, 40, "%s", string);
            if (strlen(string) > 40) {
                strcat(tt, "...");
            }
            prt("call '%s' ", name);
            buf.clean();
            if (CALL_idinfo(ir)->dumpVARDecl(buf) != NULL) {
                prt("decl:%s", buf.buf);
            }
        } else {
            prt("icall ");
        }

        PADDR(ir);
        prt("%s", attr);

        if (dump_kid) {
            if (ir->is_icall()) {
                g_indent += dn;
                dumpIR(ICALL_callee(ir), rg, (CHAR*)" callee", dumpflag);
                g_indent -= dn;
            }

            CHAR tmpbuf[30];
            UINT i = 0;

            //Dump parameter list.
            for (IR * p2 = CALL_param_list(ir);
                 p2 != NULL; p2 = p2->get_next()) {
                sprintf(tmpbuf, " param%d", i);
                g_indent += dn;
                dumpIR(p2, rg, tmpbuf, dumpflag);
                g_indent -= dn;
                i++;
            }

            //Dump dummy use.
            i = 0;
            for (IR * p2 = CALL_dummyuse(ir);
                 p2 != NULL; p2 = p2->get_next()) {
                sprintf(tmpbuf, " dummy%d", i);
                g_indent += dn;
                dumpIR(p2, rg, tmpbuf, dumpflag);
                g_indent -= dn;
                i++;
            }
        }
        break;
    }
    case IR_TRUEBR:
        note("\ntruebr ");
        dump_lab_decl(ir->getLabel());
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(BR_det(ir), rg, NULL, dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_FALSEBR:
        note("\nfalsebr ");
        dump_lab_decl(ir->getLabel());
        PADDR(ir);
        prt("%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dumpIRList(BR_det(ir), rg, NULL, dumpflag);
            g_indent -= dn;
        }
        break;
    case IR_REGION:
        note("\nregion");
        if (REGION_ru(ir)->getRegionVar() != NULL) {
            VAR * ruvar = REGION_ru(ir)->getRegionVar();
            CHAR tt[40];
            tt[0] = 0;

            //Dump variable info.
            xstrcat(tt, 40, "%s", SYM_name(ruvar->get_name()));
            prt(" \'%s\',ruid:%d", tt,
                    REGION_id(REGION_ru(ir)));
        }

        PADDR(ir); //Dump IR address.
        prt("%s", attr); //Dump attributes.

        if (dump_inner_region) {
            //Inner region.
            ASSERT0(REGION_ru(ir));
            g_indent += dn;
            note("\nruinfo:");
            REGION_ru(ir)->dump(dump_inner_region);
            g_indent -= dn;
        }
        break;
    case IR_UNDEF:
        note("\nundef!");
        PADDR(ir);
        prt("%s", attr);
        break;
    default:
        ASSERTN(0, ("unknown IR type:%s", IRNAME(ir)));
        return ;
    }
    fflush(g_tfile);
}


void setParentPointerForIRList(IR * ir_list)
{
    while (ir_list != NULL) {
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
UINT IR::count_mem() const
{
    UINT size = 0;
    ConstIRIter ii;
    for (IR const* k = iterInitC(this, ii);
         k != NULL; k = iterNextC(ii)) {
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
    ASSERT0(d);

    if (d->is_pointer()) {
        ASSERT0(TY_ptr_base_size(d) != 0);
    } else if (d->is_mc()) {
        ASSERT0(TY_mc_size(d) != 0);
    } else {
        //IR_mc_size may be not zero.
        //e.g: struct {int x;}s;
        //    int w = s.x;
        //    Here we get w IR_LD(s, offset=0, mc_size=4)
        //ASSERT0(IR_mc_size(this) == 0);
    }

    if (d->is_vector()) {
        ASSERT0(TY_vec_ety(d) != D_UNDEF);

        ASSERTN(IS_SIMPLEX(TY_vec_ety(d)) || IS_PTR(TY_vec_ety(d)),
               ("illegal vector elem type"));

        ASSERT0(TY_vec_size(d) >= tm->get_dtype_bytesize(TY_vec_ety(d)) &&
                TY_vec_size(d) % tm->get_dtype_bytesize(TY_vec_ety(d)) == 0);

        UINT ofst = getOffset();
        if (ofst != 0) {
            ASSERT0((ofst % tm->get_dtype_bytesize(TY_vec_ety(d))) == 0);
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
            ASSERT0(tm->get_dtype_bytesize(TY_vec_ety(d)) >=
                    tm->get_bytesize(ST_rhs(this)->getType()));
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
        }
        ASSERT0(ILD_base(this)->is_exp());
        ASSERT0(ILD_base(this)->is_single());
        break;
    case IR_IST:
        ASSERT0(d);
        if (!g_is_support_dynamic_type) {
            ASSERTN(IST_base(this)->is_ptr(), ("base must be pointer"));
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
            ASSERT0(tm->get_dtype_bytesize(TY_vec_ety(d)) >=
                    tm->get_bytesize(SETELEM_val(this)->getType()));
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
            ASSERT0(tm->get_dtype_bytesize(TY_vec_ety(basedtd)) >=
                    tm->get_bytesize(d));
        }
        break;
    }
    case IR_LDA:
        ASSERT0(LDA_idinfo(this));
        ASSERT0(d);
        ASSERTN(TY_dtype(d) != D_UNDEF, ("size of load value cannot be zero"));
        ASSERT0(d->is_pointer());
        //Lda base can be general VAR, label, const string.
        ASSERTN(!VAR_is_fake(LDA_idinfo(this)), ("LDA's base must be effect VAR"));
        break;
    case IR_CALL:
        ASSERT0(CALL_idinfo(this));

        //result type of call is the type of return value if it exist.
        //The result type may be VOID.
        if (CALL_prno(this) != 0) { ASSERT0(d); }

        //Parameters should be expression.
        for (IR * p = CALL_param_list(this); p != NULL; p = p->get_next()) {
            ASSERT0(p->is_exp());
        }

        //Dummy uses should be expression.
        for (IR * p = CALL_dummyuse(this); p != NULL; p = p->get_next()) {
            ASSERT0(p->is_exp());
        }
        break;
    case IR_ICALL:
        //result type of call is the type of return value if it exist.
        //Note return-value type may be VOID.

        //rtype of icall is the type of IR in return-value-list.
        ASSERT0(ICALL_callee(this) && ICALL_callee(this)->is_ptr());
        ASSERT0(ICALL_callee(this)->is_single());

        for (IR * p = CALL_param_list(this); p != NULL; p = p->get_next()) {
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
        ASSERT0(BIN_opnd0(this)->is_single());
        ASSERT0(BIN_opnd1(this)->is_single());
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
        ASSERTN(IGOTO_vexp(this), ("igoto vexp can not be NULL."));
        ASSERTN(IGOTO_case_list(this), ("igoto case list can not be NULL."));
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
        ASSERTN(SWITCH_vexp(this), ("switch vexp can not be NULL."));
        ASSERT0(SWITCH_vexp(this)->is_exp());

        //SWITCH case list can be NULL.
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
        ASSERT0(ARR_base(this)->is_ptr() || ARR_base(this)->is_void());
        ASSERT0(ARR_elemtype(this));
        if (ARR_ofst(this) != 0 && !ARR_elemtype(this)->is_void()) {
            UINT elem_data_size = tm->get_bytesize(ARR_elemtype(this));
            UINT result_data_size = tm->get_bytesize(d);
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
        ASSERT0(CVT_exp(this) != NULL && CVT_exp(this)->is_exp());
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
        if (RET_exp(this) != NULL) {
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
        ASSERT0(PHI_opnd_list(this) != NULL);
        ASSERT0(verifyPhi(rg));
        break;
    case IR_REGION: break;
    default: UNREACHABLE();
    }
    return true;
}


//This function verify def/use information of PHI stmt.
//If vpinfo is available, the function also check VP_prno of phi operands.
//is_vpinfo_avail: set true if VP information is available.
bool IR::verifyPhi(Region const* rg) const
{
    ASSERT0(is_phi());
    List<IRBB*> preds;
    IR_CFG * cfg = rg->getCFG();
    IRBB * bb = getBB();
    ASSERT0(bb);
    cfg->get_preds(preds, bb);

    UINT num_pred = preds.get_elem_count();
    DUMMYUSE(num_pred);

    //Check the number of phi opnds.
    UINT num_opnd = 0;
    for (IR const* opnd = PHI_opnd_list(this);
         opnd != NULL; opnd = opnd->get_next()) {
        num_opnd++;
    }
    ASSERTN(num_opnd == num_pred, ("the num of opnd unmatch"));

    SSAInfo * ssainfo = getSSAInfo();
    ASSERT0(ssainfo);

    SSAUseIter vit = NULL;
    for (INT i = SSA_uses(ssainfo).get_first(&vit);
         vit != NULL; i = SSA_uses(ssainfo).get_next(i, &vit)) {
        IR const* use = const_cast<Region*>(rg)->getIR(i);

        if (!use->is_pr()) { continue; }

        ASSERTN(PR_no(use) == PHI_prno(this), ("prno is unmatch"));

        SSAInfo * use_ssainfo = PR_ssainfo(use);
        CHECK_DUMMYUSE(use_ssainfo);

        ASSERT0(SSA_def(use_ssainfo) == this);
    }
    return true;
}


bool IR::verifyKids() const
{
    ULONG kidbit = 1;
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++, kidbit <<= 1) {
        IR * k = getKid(i);
        if (k != NULL) {
            ASSERT0(IR_parent(k) == this);
        }
        if (!HAVE_FLAG(IRDES_kid_map(g_ir_desc[getCode()]), kidbit)) {
            ASSERTN(k == NULL,
                   ("IR_%s does not have No.%d kid", IRNAME(this), i));
        } else {
            //Here, ith kid cannot be NULL.
            //CASE: Kind of node permit some of their kid to be NULL.
            //For now include IR_IF, IR_RETURN, IR_DO_LOOP, etc. */
            if (k == NULL) {
                switch (getCode()) {
                case IR_IF:
                case IR_DO_LOOP:
                case IR_WHILE_DO:
                case IR_SWITCH:
                case IR_DO_WHILE:
                    if (i == 0) {
                        ASSERTN(k != NULL,
                                ("IR_%s miss kid%d", IRNAME(this), i));
                    }
                    break;
                case IR_ICALL:
                    if (i == 2) {
                        ASSERTN(k != NULL,
                                ("IR_%s miss kid%d", IRNAME(this), i));
                    }
                    break;
                case IR_RETURN:
                case IR_CALL:
                    break;
                default:
                    ASSERTN(k != NULL, ("IR_%s miss kid%d", IRNAME(this), i));
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
    if (ARR_elem_num_buf(this) == NULL) { return false; }

    TMWORD aggr = 0;
    UINT dim = 0;
    for (IR const* s = ARR_sub_list(this); s != NULL; s = s->get_next(), dim++) {
        if (!s->is_const()) { return false; }

        ASSERT0(!s->is_fp() && CONST_int_val(s) >= 0);

        #ifdef _VC2010_
        #define MASK_32BIT 0xFFFFffff00000000lu
        #else
        #define MASK_32BIT 0xFFFFffff00000000llu
        #endif
        ASSERTN((((ULONGLONG)CONST_int_val(s)) &
                (ULONGLONG)(LONGLONG)MASK_32BIT) == 0,
               ("allow 32bit array offset."));

        ASSERT0(dim < ((CArray*)this)->getDimNum());
        ASSERT0(ARR_elem_num(this, dim) != 0);
        //Array index start at 0.
        if (dim == 0) {
            aggr = (TMWORD)CONST_int_val(s);
        } else {
            aggr += ARR_elem_num(this, dim - 1) * (TMWORD)CONST_int_val(s);
        }
    }

    aggr *= tm->get_bytesize(ARR_elemtype(this));
    ASSERT0(ofst_val);
    *ofst_val = aggr;
    return true;
}


//Return true if ir-list are equivalent.
//'is_cmp_kid': it is true if comparing kids as well.
bool IR::isIRListEqual(IR const* irs, bool is_cmp_kid) const
{
    IR const* pthis = this;
    while (irs != NULL && pthis != NULL) {
        if (!pthis->isIREqual(irs, is_cmp_kid)) {
            return false;
        }
        irs = irs->get_next();
        pthis = IR_next(pthis);
    }
    if ((irs != NULL) ^ (pthis != NULL)) {
        return false;
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
            if ((ARR_sub_list(src) != NULL) ^ (ARR_sub_list(this) != NULL)) {
                return false;
            }
            if (ARR_sub_list(src) != NULL &&
                !ARR_sub_list(src)->isIRListEqual(ARR_sub_list(this))) {
                return false;
            }
            if ((ARR_elem_num_buf(src) != NULL) ^
                (ARR_elem_num_buf(this) != NULL)) {
                return false;
            }
            if (ARR_elem_num_buf(src) != NULL) {
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
            if ((ARR_sub_list(src) != NULL) ^ (ARR_sub_list(this) != NULL)) {
                return false;
            }
            if (ARR_sub_list(src) != NULL &&
                !ARR_sub_list(src)->isIRListEqual(ARR_sub_list(this))) {
                return false;
            }
            if ((ARR_elem_num_buf(src) != NULL) ^
                (ARR_elem_num_buf(this) != NULL)) {
                return false;
            }
            if (ARR_elem_num_buf(src) != NULL) {
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
//is_cmp_kid: it is true if comparing kids as well.
bool IR::isIREqual(IR const* src, bool is_cmp_kid) const
{
    ASSERT0(src);
    if (getCode() != src->getCode()) { return false; }
    switch (src->getCode()) {
    case IR_CONST: //Constant value: include integer, float, string.
        if (CONST_int_val(this) != CONST_int_val(src)) return false;
        break;
    case IR_ID:
        if (ID_info(this) != ID_info(src)) return false;
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
        if ((ARR_elem_num_buf(src) != NULL) ^
            (ARR_elem_num_buf(this) != NULL)) {
            return false;
        }
        if (ARR_elem_num_buf(src) != NULL) {
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
        if (IST_ofst(this) != IST_ofst(src) ||
            getType() != src->getType()) {
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
        if (CALL_idinfo(this) != CALL_idinfo(src)) return false;
        break;
    case IR_ICALL:
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        if (getType() != src->getType()) return false;
        break;
    case IR_GOTO:
    case IR_IGOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
        break;
    case IR_LABEL:
        if (LAB_lab(this) != LAB_lab(src)) return false;
        break;
    case IR_SWITCH:
    case IR_CASE:
        break;
    case IR_ARRAY:
        if (ARR_ofst(this) != ARR_ofst(src) || getType() != src->getType()) {
            return false;
        }
        if ((ARR_elem_num_buf(src) != NULL) ^
            (ARR_elem_num_buf(this) != NULL)) {
            return false;
        }
        if (ARR_elem_num_buf(src) != NULL) {
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

        if ((kid1 != NULL) ^ (kid2 != NULL)) {
            return false;
        }
        if (kid1 != NULL && !kid1->isIRListEqual(kid2, is_cmp_kid)) {
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
        if (kid != NULL) {
            while (kid != NULL) {
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
IR * IR::getOpndPRList(UINT prno)
{
    IR * pr = NULL;
    IR * p = this; //this is header of list.
    while (p != NULL) {
        if ((pr = p->getOpndPR(prno)) != NULL) {
            return pr;
        }
        p = p->get_next();
    }
    return NULL;
}


//Find the first PR related to 'prno'.
//This function iterate IR tree nonrecursively.
//'ii': iterator.
IR * IR::getOpndPR(UINT prno, IRIter & ii)
{
    ASSERT0(is_stmt());
    ii.clean();
    for (IR * k = iterInit(this, ii);
         k != NULL; k = iterNext(ii)) {
        if (k->is_pr() && PR_no(k) == prno && is_rhs(k)) {
            return k;
        }
    }
    return NULL;
}


//This function recursively iterate the IR tree to
//retrieve the PR whose PR_no is equal to given 'prno'.
//Otherwise return NULL.
IR * IR::getOpndPR(UINT prno)
{
    IR * pr = NULL;
    switch (getCode()) {
    case IR_CONST:
    case IR_ID:
    case IR_LD:
        return NULL;
    case IR_ST:
        return ST_rhs(this)->getOpndPR(prno);
    case IR_STPR:
        return STPR_rhs(this)->getOpndPR(prno);
    case IR_STARRAY:
        if ((pr = ARR_base(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        if ((pr = ARR_sub_list(this)->getOpndPRList(prno)) != NULL) {
            return pr;
        }
        return STARR_rhs(this)->getOpndPR(prno);
    case IR_SETELEM:
        if ((pr = SETELEM_base(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        if ((pr = SETELEM_ofst(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return SETELEM_val(this)->getOpndPR(prno);
    case IR_GETELEM:
        if ((pr = GETELEM_base(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        if ((pr = GETELEM_ofst(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return GETELEM_prno(this) == prno ? this : NULL;
    case IR_ILD:
        return ILD_base(this)->getOpndPR(prno);
    case IR_IST:
        if ((pr = IST_base(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return IST_rhs(this)->getOpndPR(prno);
    case IR_LDA: return NULL;
    case IR_CALL:
    case IR_ICALL:
        if ((pr = CALL_param_list(this)->getOpndPRList(prno)) != NULL) {
            return pr;
        }
        if (is_icall()) {
            return ICALL_callee(this)->getOpndPR(prno);
        }
        return NULL;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
       for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
            IR * k = getKid(i);
            if (k == NULL) { continue; }
            if ((pr = k->getOpndPR(prno)) != NULL) {
                return pr;
            }
       } 
       return NULL; 
    case IR_GOTO: return NULL;
    case IR_IGOTO:
        if ((pr = IGOTO_vexp(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return NULL;
    case IR_DO_WHILE:
    case IR_WHILE_DO:
        if ((pr = LOOP_det(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return LOOP_body(this)->getOpndPRList(prno);
    case IR_DO_LOOP:
        if ((pr = LOOP_det(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        if ((pr = LOOP_init(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        if ((pr = LOOP_step(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return LOOP_body(this)->getOpndPRList(prno);
    case IR_IF:
    case IR_LABEL: return NULL;
    case IR_SWITCH:
        if ((pr = SWITCH_vexp(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return SWITCH_body(this)->getOpndPRList(prno);
    case IR_CASE: return NULL;
    case IR_ARRAY:
        if ((pr = ARR_base(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        if ((pr = ARR_sub_list(this)->getOpndPRList(prno)) != NULL) {
            return pr;
        }
        return NULL;
    case IR_PR:
        return PR_no(this) == prno ? this : NULL;
    case IR_TRUEBR:
    case IR_FALSEBR:
        return BR_det(this)->getOpndPR(prno);
    case IR_SELECT:
        if ((pr = SELECT_pred(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        if ((pr = SELECT_trueexp(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return SELECT_falseexp(this)->getOpndPR(prno);
    case IR_RETURN:
        if ((pr = RET_exp(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return NULL;
    case IR_BREAK:
    case IR_CONTINUE:
        return NULL;
    case IR_PHI:
        if ((pr = PHI_opnd_list(this)->getOpndPRList(prno)) != NULL) {
            return pr;
        }
        return NULL;
    case IR_REGION: return NULL;
    default: UNREACHABLE();
    }
    return NULL;
}


//Get the Stmt accroding to given prno.
//The stmt must write to PR as a result.
IR * IR::getResultPR(UINT prno)
{
    switch (getCode()) {
    case IR_STPR:
        if (STPR_no(this) == prno) { return this; }
        return NULL;
    case IR_SETELEM:
        if (SETELEM_prno(this) == prno) { return this; }
        return NULL;
    case IR_GETELEM:
        if (GETELEM_prno(this) == prno) { return this; }
        return NULL;
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
        return NULL;
    case IR_CALL:
    case IR_ICALL:
        if (CALL_prno(this) == prno) { return this; }
        return NULL;
    case IR_PHI:
        if (PHI_prno(this) == prno) { return this; }
        return NULL;
    default: UNREACHABLE();
    }
    return NULL;
}


//Copy MD that ir referenced accroding to 'mds'.
void IR::setRefMD(MD const* md, Region * rg)
{
    DU * du = getDU();
    if (du == NULL) {
        if (md == NULL) { return; }

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
    if (du == NULL) {
        if (mds == NULL) { return; }

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


//This function only handle Call/Icall stmt, it find PR and remove
//them out of UseSet.
//Note this function does not maintain DU chain between call and its use.
void IR::removePROutFromUseset(DefMiscBitSetMgr & sbs_mgr, Region * rg)
{
    ASSERT0(isCallStmt() && rg);
    DUSet * useset = getDUSet();
    if (useset == NULL) { return; }

    DUIter di = NULL;
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
        if (ssainfo != NULL) {
            ssainfo->cleanDU();
        }
    } else {
        SSAInfo * ssainfo = ir->getSSAInfo();
        if (ssainfo != NULL) {
            SSA_uses(ssainfo).remove(ir);
        }
    }

    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->getKid(i); x != NULL; x = x->get_next()) {
            if (x->is_pr()) {
                SSAInfo * ssainfo = PR_ssainfo(x);
                if (ssainfo != NULL) {
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


//Remove SSA use-def chain.
//e.g: pr1=...
//    =pr1 //S1
//If S1 will be deleted, pr1 should be removed from its SSA_uses.
void IR::removeSSAUse()
{
    removeSSAUseRecur(this);
}


//Copy memory reference only for current ir node.
//'src': copy MD reference from 'src', it may be different to current ir.
void IR::copyRef(IR const* src, Region * rg)
{
    ASSERT0(src && rg && this != src);
    ASSERTN(isMemoryRef(), ("not memory reference"));
    ASSERT0(!src->is_undef());
    setRefMD(src->getRefMD(), rg);
    if (isReadPR() || isWritePR()) {;}
    else { setRefMDSet(src->getRefMDSet(), rg); }
}


static bool hasProp(IR const* ir)
{
    return ir->isMayThrow() ||
        ir->is_terminate() ||
        ir->is_atomic() ||
        ir->is_rmw() ||
        ir->hasSideEffect() ||
        ir->isNoMove();
}


//Dump IR tree's MD reference, where ir may be stmt or exp.
void IR::dumpRef(Region * r, UINT indent)
{
    if (this == NULL || g_tfile == NULL || is_const()) { return; }
    UINT oldindent = g_indent;
    g_indent = indent;
    dumpIR(this, r, NULL, false);

    //Dump mustref MD.
    MD const* md = getRefMD();
    MDSet const* mds = getRefMDSet();

    //MustDef
    bool prt_mustdef = false;
    if (md != NULL) {
        note("\nMMD%d", MD_id(md));
        prt_mustdef = true;
    }

    if (mds != NULL) {
        //MayDef
        if (!prt_mustdef) {
            note("\n"); //dump indent blank.
        }
        prt(" : ");
        if (mds != NULL && !mds->is_empty()) {
            mds->dump(r->getMDSystem());
        }
    }

    if (isCallStmt()) {
        bool doit = false;
        CallGraph * callg = r->getRegionMgr()->getCallGraph();
        if (callg != NULL) {
            Region * rg = callg->mapCall2Region(this, r);
            if (rg != NULL && REGION_is_mddu_valid(rg)) {
                MDSet const* muse = rg->getMayUse();
                //May use
                prt(" <-- ");
                if (muse != NULL && !muse->is_empty()) {
                    muse->dump(r->getMDSystem());
                    doit = true;
                }
            }
        }
        if (!doit) {
            //MayUse MDSet.
            //Regard MayDef MDSet as MayUse.
            prt(" <-- ");
            MDSet const* x = getRefMDSet();
            if (x != NULL && !x->is_empty()) {
                x->dump(r->getMDSystem());
            }
        }
    }

    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        for (IR * k = getKid(i); k != NULL; k = k->get_next()) {
            k->dumpRef(r, indent + 2);
        }
    }
    g_indent = oldindent;
    fflush(g_tfile);
}


static void dumpOffset(IR const* ir)
{
    if (hasProp(ir) ||
        ir->getOffset() != 0 ||
        ((ir->is_array() || ir->is_starray()) &&
         ARR_elem_num_buf(ir) != NULL)) {
        prt(":%d", ir->getOffset());
    }
}


static void dumpProp(IR const* ir, TypeMgr * tm, DumpGRCtx * ctx)
{
    bool first = true;
    if (ir->isMayThrow()) {
        if (!first) { prt(","); }
        else { prt(":("); }
        prt("throw(");
        if (ir->getAI() != NULL) {
            EHLabelAttachInfo const* ehlab =
                (EHLabelAttachInfo const*)ir->getAI()->get(AI_EH_LABEL);
            if (ehlab != NULL) {
                xcom::SList<LabelInfo*> const& labs = ehlab->read_labels();
                for (xcom::SC<LabelInfo*> * sc = labs.get_head();
                     sc != labs.end(); sc = labs.get_next(sc)) {
                    if (sc != labs.get_head()) {
                        prt(",");
                    }
                    dump_label_ref(sc->val());
                }
            }
        }
        prt(")");
        first = false;
    }
    if (ir->is_array() || ir->is_starray()) {
        ASSERT0(ARR_elemtype(ir));
        if (ARR_elemtype(ir) != ir->getType() ||
            ARR_elem_num_buf(ir) != NULL) {
            if (!first) { prt(","); }
            else { prt(":("); }
            first = false;
        }
        bool prt_elemtype = false;
        if (ARR_elemtype(ir) != ir->getType()) {
            xcom::StrBuf ety(16);
            tm->dump_type(ARR_elemtype(ir), ety);
            prt("elemtype:%s", ety.buf);
            prt_elemtype = true;
        }
        if (ARR_elem_num_buf(ir) != NULL) {
            if (prt_elemtype) {
                prt(",");
            }
            prt("dim");
            UINT dim = 0;
            prt("[");
            for (IR const* sub = ARR_sub_list(ir); sub != NULL;) {
                prt("%d", ((CStArray*)ir)->getElementNumOfDim(dim));
                sub = sub->get_next();
                if (sub != NULL) {
                    prt(",");
                }
                dim++;
            }
            prt("]");
        }
    }
    if (ir->is_terminate()) {
        if (!first) { prt(","); }
        else { prt(":("); }
        prt("terminate");
        first = false;
    }
    if (ir->is_atomic()) {
        if (!first) { prt(","); }
        else { prt(":("); }
        prt("atom");
        first = false;
    }
    if (ir->is_rmw()) {
        if (!first) { prt(","); }
        else { prt(":("); }
        prt("rmw");
        first = false;
    }
    if (ir->hasSideEffect()) {
        if (!first) { prt(","); }
        else { prt(":("); }
        prt("sideeffect");
        first = false;
    }
    if (ir->isNoMove()) {
        if (!first) { prt(","); }
        else { prt(":("); }
        prt("nomove");
        first = false;
    }
    if (ir->isCallStmt() && CALL_dummyuse(ir) != NULL) {
        if (!first) { prt(","); }
        else { prt(":("); }
        prt("use(");
        g_indent += DUMP_INDENT_NUM;
        for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
            if (p != CALL_dummyuse(ir)) {
                prt(",");
            }
            dumpGR(p, tm, ctx);
        }
        prt(")");
        first = false;
        g_indent -= DUMP_INDENT_NUM;
    }
    if (!first) { prt(")"); }
}


CHAR const* compositeName(SYM const* n, xcom::StrBuf & buf)
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


static void dumpArrSubList(IR const* ir, UINT dn, TypeMgr * tm, DumpGRCtx * ctx)
{
    if (ARR_sub_list(ir) == NULL) { return; }
    g_indent += dn;
    prt("(");
    for (IR * s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
        if (s != ARR_sub_list(ir)) {
            prt(",");
        }
        dumpGR(s, tm, ctx);
    }
    prt(")");
    g_indent -= dn;
}


void dumpGR(IR const* ir, TypeMgr * tm, DumpGRCtx * ctx)
{
    UINT dn = DUMP_INDENT_NUM;
    if (g_tfile == NULL || ir == NULL) { return; }
    ASSERT0(tm);

    StrBuf buf(64);
    StrBuf buf2(64);
    Type const* d = ir->getType();
    switch (ir->getCode()) {
    case IR_ST:
        note("\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, tm, ctx);
        buf.clean();
        prt(" %s = ", compositeName(ST_idinfo(ir)->get_name(), buf));
        g_indent += dn;
        dumpGR(ST_rhs(ir), tm, ctx);
        g_indent -= dn;
        prt(";");
        break;
    case IR_STPR:
        note("\n%s", IRNAME(ir));
        prt(" $%d:%s", STPR_no(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        prt(" = ");
        g_indent += dn;
        dumpGR(STPR_rhs(ir), tm, ctx);
        g_indent -= dn;
        prt(";");
        break;
    case IR_SETELEM:
        note("\n%s", IRNAME(ir));
        prt(" $%d:%s", SETELEM_prno(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        prt(" = ");
        g_indent += dn;
        dumpGR(SETELEM_base(ir), tm, ctx);
        prt(",");
        dumpGR(SETELEM_val(ir), tm, ctx);
        prt(",");
        dumpGR(SETELEM_ofst(ir), tm, ctx);
        g_indent -= dn;
        prt(";");
        break;
    case IR_GETELEM:
        note("\n%s", IRNAME(ir));
        prt(" $%d:%s", GETELEM_prno(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        prt(" = ");
        g_indent += dn;
        dumpGR(GETELEM_base(ir), tm, ctx);
        prt(",");
        dumpGR(GETELEM_ofst(ir), tm, ctx);
        g_indent -= dn;
        prt(";");
        break;
    case IR_STARRAY:
        note("\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, tm, ctx);
        prt(" = ");

        g_indent += dn;
        dumpGR(ARR_base(ir), tm, ctx);
        prt(", ");
        g_indent -= dn;

        dumpArrSubList(ir, dn, tm, ctx);
        prt(", ");

        g_indent += dn;
        dumpGR(STARR_rhs(ir), tm, ctx);
        g_indent -= dn;
        prt(";");
        break;
    case IR_IST:
        note("\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, tm, ctx);
        prt(" = ");
        g_indent += dn;
        dumpGRList(IST_base(ir), tm, ctx);
        prt(",");
        dumpGRList(IST_rhs(ir), tm, ctx);
        g_indent -= dn;
        prt(";");
        break;
    case IR_LD:
        note("\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, tm, ctx);
        buf.clean();
        prt(" %s", compositeName(LD_idinfo(ir)->get_name(), buf));
        break;
    case IR_ILD:
        note("\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, tm, ctx);
        prt(" ");
        g_indent += dn;
        dumpGRList(ILD_base(ir), tm, ctx);
        g_indent -= dn;
        break;
    case IR_PR:
        note("\n$%d:%s", PR_no(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        break;
    case IR_ID:
        note("\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        buf.clean();
        prt(" %s", compositeName(ID_info(ir)->get_name(), buf));
        break;
    case IR_CONST:
        if (ir->is_sint()) {
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            //CHAR const* intfmt = "%d:%s";
            CHAR const* intfmt = "%x:%s";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            //CHAR const* intfmt = "%lld:%s";
            CHAR const* intfmt = "0x%llx:%s";
            #else
            #error "Need to support";
            #endif
            prt(intfmt, CONST_int_val(ir), tm->dump_type(d, buf));
        } else if (ir->is_uint()) {
            #if WORD_LENGTH_OF_HOST_MACHINE==32
            CHAR const* intfmt = "%u:%s";
            #elif WORD_LENGTH_OF_HOST_MACHINE==64
            CHAR const* intfmt = "%llu:%s";
            #else
            #error "Need to support";
            #endif
            prt(intfmt, CONST_int_val(ir), tm->dump_type(d, buf));
        } else if (ir->is_fp()) {
            CHAR fpformat[128];
            ::snprintf(fpformat, 127, "%%.%df:%%s ", CONST_fp_mant(ir));
            prt(fpformat, CONST_fp_val(ir), tm->dump_type(d, buf));
        } else if (ir->is_bool()) {
            prt("%d:%s", (UINT)CONST_int_val(ir), tm->dump_type(d, buf));
        } else if (ir->is_str()) {
            CHAR * tbuf = SYM_name(CONST_str_val(ir));
            //Remove \n to show string in one line.
            if (ctx != NULL && ctx->dump_string_in_one_line) {
                size_t len = ::strlen(SYM_name(CONST_str_val(ir)));
                tbuf = (CHAR*)::malloc(len);
                tbuf[0] = 0;
                xstrcat(tbuf, len, "%s", SYM_name(CONST_str_val(ir)));
                for (UINT i = 0; i < len && tbuf[i] != 0; i++) {
                    if (tbuf[i] == '\n') { tbuf[i] = ' '; }
                }
            }
            prt("\"%s\"", tbuf);
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
            prt(intfmt, CONST_int_val(ir), tm->dump_type(d, buf));
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
            prt(intfmt, CONST_int_val(ir), tm->dump_type(d, buf));
        }
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        note("\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);
        prt(" ");
        g_indent += dn;
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == NULL) { continue; }
            if (i != 0) {
                prt(", ");
            }
            dumpGRList(k, tm, ctx);
        }
        g_indent -= dn;
        break;
    case IR_IF:
        note("\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(" (");

        g_indent += dn;
        dumpGRList(IF_det(ir), tm, ctx);
        g_indent -= dn;
        prt(")");

        note("\n{");
        g_indent += dn;
        dumpGRList(IF_truebody(ir), tm, ctx);
        g_indent -= dn;
        note("\n}");

        if (IF_falsebody(ir)) {
            note("\nelse");
            note("\n{");
            g_indent += dn;
            dumpGRList(IF_falsebody(ir), tm, ctx);
            g_indent -= dn;
            note("\n}");
        }
        prt(";");
        break;
    case IR_DO_WHILE:
        note("\ndo");
        dumpProp(ir, tm, ctx);
        prt(" {");

        g_indent += dn;
        dumpGRList(LOOP_body(ir), tm, ctx);
        g_indent -= dn;

        note("\n} while (");
        g_indent += dn;
        dumpGRList(LOOP_det(ir), tm, ctx);
        g_indent -= dn;
        prt(");");
        break;
    case IR_WHILE_DO:
        note("\nwhile");
        dumpProp(ir, tm, ctx);
        prt(" (");

        g_indent += dn;
        dumpGRList(LOOP_det(ir), tm, ctx);
        g_indent -= dn;
        prt(") {");

        g_indent += dn;
        dumpGRList(LOOP_body(ir), tm, ctx);
        g_indent -= dn;
        note("\n};");
        break;
    case IR_DO_LOOP:
        note("\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(" (");

        g_indent += dn;
        note("\n//iv");
        dumpGRList(LOOP_iv(ir), tm, ctx);
        g_indent -= dn;
        prt(",");

        g_indent += dn;
        note("\n//init");
        dumpGRList(LOOP_init(ir), tm, ctx);
        g_indent -= dn;
        prt(",");

        g_indent += dn;
        note("\n//det");
        dumpGRList(LOOP_det(ir), tm, ctx);
        g_indent -= dn;
        prt(",");

        g_indent += dn;
        note("\n//step");
        dumpGRList(LOOP_step(ir), tm, ctx);
        g_indent -= dn;
        prt(")");

        note("\n {");
        g_indent += dn;
        dumpGRList(LOOP_body(ir), tm, ctx);
        g_indent -= dn;

        note("\n};");
        break;
    case IR_BREAK:
    case IR_CONTINUE:
        note("\n%s;", IRNAME(ir));
        break;
    case IR_RETURN:
        note("\n%s", IRNAME(ir));
        prt(" ");
        g_indent += dn;
        dumpGR(RET_exp(ir), tm, ctx);
        g_indent -= dn;
        prt(";");
        break;
    case IR_GOTO:
        note("\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(" ");
        dump_label_ref(ir->getLabel());
        prt(";");
        break;
    case IR_IGOTO:
        note("\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(" (");

        g_indent += dn;
        dumpGRList(IGOTO_vexp(ir), tm, ctx);
        prt(")");
        g_indent -= dn;

        g_indent += dn;
        for (IR * c = IGOTO_case_list(ir); c != NULL; c = c->get_next()) {
            dumpGR(c, tm, ctx);
            prt(", ");
        }
        g_indent -= dn;
        prt(";");
        break;
    case IR_LABEL:
        {
            LabelInfo const* li = LAB_lab(ir);
            note("\n");
            dump_lab_decl(li);
        }
        prt(";");
        break;
    case IR_SELECT: //formulized log_OR_exp?exp:cond_exp
        note("\nselect:%s", tm->dump_type(d, buf));
        dumpProp(ir, tm, ctx);

        g_indent += dn;
        dumpGRList(SELECT_pred(ir), tm, ctx);
        prt(",");
        g_indent -= dn;

        g_indent += dn;
        dumpGRList(SELECT_trueexp(ir), tm, ctx);
        prt(",");
        g_indent -= dn;

        g_indent += dn;
        dumpGRList(SELECT_falseexp(ir), tm, ctx);
        g_indent -= dn;
        break;
    case IR_LDA:
        note("\n%s", IRNAME(ir));
        dumpOffset(ir);
        dumpProp(ir, tm, ctx);
        buf.clean();
        prt(" %s", compositeName(LDA_idinfo(ir)->get_name(), buf));
        break;
    case IR_PHI:
        note("\n%s $%d:%s = ", IRNAME(ir), PHI_prno(ir), tm->dump_type(d, buf));
        g_indent += dn;
        ASSERT0(ctx->cfg && ir->getBB());
        {
            List<IRBB*> preds;
            ctx->cfg->get_preds(preds, ir->getBB());
            xcom::C<IRBB*> * bbct = NULL;
            ASSERT0(preds.get_elem_count() == xcom::cnt_list(PHI_opnd_list(ir)));
            preds.get_head(&bbct);
            for (IR * opnd = PHI_opnd_list(ir);
                 opnd != NULL;
                 opnd = opnd->get_next(), bbct = preds.get_next(bbct)) {
                if (opnd != PHI_opnd_list(ir)) {
                    prt(",");
                }
                xcom::C<LabelInfo const*> * lct;
                LabelInfo const* lab = bbct->val()->
                    getLabelListConst().get_head(&lct);
                if (lab == NULL) {
                    lab = ctx->cfg->getRegion()->genIlabel();
                    bbct->val()->addLabel(lab);
                }
                prt("(");
                dumpGR(opnd, tm, ctx);
                prt(",");
                dump_label_ref(lab);
                prt(")");
            }
        }
        g_indent -= dn;
        prt(";");
        break;
    case IR_SWITCH:
        note("\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(" (");
        g_indent += dn;
        dumpGRList(SWITCH_vexp(ir), tm, ctx);
        prt(") ");
        g_indent -= dn;
        if (SWITCH_deflab(ir) != NULL) {
            note("\ndefault ");
            dump_label_ref(ir->getLabel());
            prt(", ");
        }

        for (IR * c = SWITCH_case_list(ir); c != NULL; c = c->get_next()) {
            dumpGR(c, tm, ctx);
            prt(", ");
        }

        if (SWITCH_body(ir) != NULL) {
            note("\n{ ");
            g_indent += dn;
            dumpGRList(SWITCH_body(ir), tm, ctx);
            g_indent -= dn;
            note("\n}");
        }
        prt(";");
        break;
    case IR_CASE:
        ASSERT0(CASE_vexp(ir));
        ASSERT0(CASE_lab(ir));
        note("\ncase");
        dumpProp(ir, tm, ctx);
        prt(" ");
        g_indent += dn;
        dumpGRList(CASE_vexp(ir), tm, ctx);
        prt(", ");
        dump_label_ref(ir->getLabel());
        g_indent -= dn;
        break;
    case IR_ARRAY:
        note("\n%s:%s", IRNAME(ir), tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, tm, ctx);

        g_indent += dn;
        dumpGR(ARR_base(ir), tm, ctx);
        prt(", ");
        g_indent -= dn;

        dumpArrSubList(ir, dn, tm, ctx);
        break;
    case IR_CALL:
    case IR_ICALL:
        note("\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(" ");
        if (ir->hasReturnValue()) {
            prt("$%d:%s = ", CALL_prno(ir), tm->dump_type(d, buf));
        }
        if (ir->is_icall()) {
            g_indent += dn;
            dumpGR(ICALL_callee(ir), tm, ctx);
            prt(", ");
            g_indent -= dn;
        } else {
            buf.clean();
            prt("%s", compositeName(CALL_idinfo(ir)->get_name(), buf));
        }
        prt("(");
        g_indent += dn;
        for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
            if (p != CALL_param_list(ir)) {
                prt(",");
            }
            dumpGR(p, tm, ctx);
        }
        g_indent -= dn;
        prt(")");
        prt(";");
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        note("\n%s", IRNAME(ir));
        dumpProp(ir, tm, ctx);
        prt(" (");
        g_indent += dn;
        dumpGRList(BR_det(ir), tm, ctx);
        g_indent -= dn;
        prt("), ");
        dump_label_ref(ir->getLabel());
        prt(";");
        break;
    case IR_REGION:
        ASSERT0(REGION_ru(ir));
        if (ctx != NULL && ctx->dump_inner_region) {
            //g_indent += dn;
            REGION_ru(ir)->dumpGR(ctx->dump_inner_region);
            //g_indent -= dn;
        } else {
            note("\nregion ");
            switch (REGION_type(REGION_ru(ir))) {
            case REGION_PROGRAM: prt("program "); break;
            case REGION_BLACKBOX: prt("blx "); break;
            case REGION_FUNC: prt("func "); break;
            case REGION_INNER: prt("inner "); break;
            default: ASSERT0(0); //TODO
            }
            if (REGION_ru(ir)->getRegionVar() != NULL) {
                prt("%s ", SYM_name(REGION_ru(ir)->getRegionVar()->get_name()));
            }
        }
        prt(";");
        break;
    case IR_UNDEF:
        note("\nundef!");
        break;
    default:
        ASSERTN(0, ("unknown IR type:%s", IRNAME(ir)));
        return ;
    }

    fflush(g_tfile);
}


void dumpGRList(IR * irlist, TypeMgr * tm, DumpGRCtx * ctx)
{
    for (IR * ir = irlist; ir != NULL; ir = ir->get_next()) {
        dumpGR(ir, tm, ctx);
    }
}


void dumpGRInBBList(List<IRBB*> * bblist, TypeMgr * tm, DumpGRCtx * ctx)
{
    ASSERT0(bblist);
    xcom::C<IRBB*> * bbct = NULL;
    for (bblist->get_head(&bbct);
         bbct != bblist->end(); bbct = bblist->get_next(bbct)) {
        IRBB * bb = bbct->val();
        xcom::C<LabelInfo const*> * labct;
        for (bb->getLabelListConst().get_head(&labct);
            labct != bb->getLabelListConst().end();
            labct = bb->getLabelListConst().get_next(labct)) {
            LabelInfo const* li = labct->val();
            ASSERT0(li);
            note("\n");
            dump_lab_decl(li);
            prt(";");
        }

        xcom::C<IR*> * irct = NULL;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            ASSERT0(ir);
            dumpGR(ir, tm, ctx);
        }
    }
}
//END IR


//Make sure IR_ICALL is the largest ir.
bool checkMaxIRType()
{
    //Change MAX_OFFSET_AT_FREE_TABLE if IR_ICALL is not the largest.
    for (UINT i = IR_UNDEF; i < IR_TYPE_NUM; i++) {
        ASSERT0(IRTSIZE(i) <= IRTSIZE(IR_ICALL));
    }
    return true;
}


bool checkIRDesc()
{
    UINT sz = (UINT)(1 << IR_TYPE_BIT_SIZE);
    ASSERTN(IR_TYPE_NUM <= sz, ("code field is too small"));
    DUMMYUSE(sz);
    for (UINT i = IR_UNDEF; i < IR_TYPE_NUM; i++) {
        ASSERT0(i == (UINT)IRDES_code(g_ir_desc[i]));
    }
    UINT descnum = sizeof(g_ir_desc) / sizeof(g_ir_desc[0]);
    ASSERTN(descnum - 1 == IR_TYPE_NUM, ("miss IRDesc declaration"));
    return true;
}

bool checkRoundDesc()
{
    for (UINT i = ROUND_UNDEF; i < ROUND_TYPE_NUM; i++) {
        ASSERT0(i == (UINT)ROUNDDESC_type(g_round_desc[i]));
    }
    UINT descnum = sizeof(g_round_desc) / sizeof(g_round_desc[0]);
    ASSERTN(descnum == ROUND_TYPE_NUM, ("miss RoundDesc declaration"));
    return true;
}

} //namespace xoc
