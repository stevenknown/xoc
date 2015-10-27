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

#if (WORD_LENGTH_OF_HOST_MACHINE == 32)
    #define PADDR(ir) (dump_addr ? fprintf(g_tfile, " 0x%lx",(ULONG)(ir)) : 0)
     //fprintf(g_tfile, " 0x%.8x",(ir)) : 0)
#elif (WORD_LENGTH_OF_HOST_MACHINE == 64)
    #define PADDR(ir) (dump_addr ? fprintf(g_tfile, " 0x%lx",(ULONG)(ir)) : 0)
    //fprintf(g_tfile, " 0x%.16x",(ir)) : 0)
#else
    #define PADDR(ir) (ASSERT0(0))
#endif

IRDesc const g_ir_desc[] = {
    {IR_UNDEF,      "undef",     0x0,    0,    0,                                              0                },
    {IR_CONST,      "const",     0x0,    0,    IRT_IS_LEAF,                                    sizeof(CConst)   },
    {IR_ID,         "id",        0x0,    0,    IRT_IS_LEAF,                                    sizeof(CId)      },
    {IR_LD,         "ld",        0x0,    0,    IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_LEAF,     sizeof(CLd)      },
    {IR_ILD,        "ild",       0x1,    1,    IRT_IS_UNA|IRT_IS_MEM_REF|IRT_IS_MEM_OPND,      sizeof(CIld)     },
    {IR_PR,         "pr",        0x0,    0,    IRT_IS_MEM_REF|IRT_IS_MEM_OPND|IRT_IS_LEAF,     sizeof(CPr)      },
    {IR_ARRAY,      "array",     0x3,    2,    IRT_IS_MEM_REF|IRT_IS_MEM_OPND,                 sizeof(CArray)   },
    {IR_ST,         "st",        0x1,    1,    IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT,      sizeof(CSt)      },
    {IR_STPR,       "stpr",      0x1,    1,    IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT,      sizeof(CStpr)    },
    {IR_STARRAY,    "starray",   0x7,    3,    IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT,      sizeof(CStArray) },
    {IR_IST,        "ist",       0x3,    2,    IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT,      sizeof(CIst)     },
    {IR_SETELEM,    "setepr",    0x3,    2,    IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT,      sizeof(CSetElem) },
    {IR_GETELEM,    "getepr",    0x3,    2,    IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT,      sizeof(CGetElem) },
    {IR_CALL,       "call",      0x1,    1,    IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT,      sizeof(CCall)    },
    {IR_ICALL,      "icall",     0x3,    2,    IRT_IS_STMT|IRT_IS_MEM_REF|IRT_HAS_RESULT,      sizeof(CICall)   },
    {IR_LDA,        "lda",       0x1,    1,    IRT_IS_UNA,                                     sizeof(CLda)     },
    {IR_ADD,        "add",       0x3,    2,    IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE, sizeof(CBin)   },
    {IR_SUB,        "sub",       0x3,    2,    IRT_IS_BIN|IRT_IS_ASSOCIATIVE,                    sizeof(CBin)   },
    {IR_MUL,        "mul",       0x3,    2,    IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE, sizeof(CBin)   },
    {IR_DIV,        "div",       0x3,    2,    IRT_IS_BIN,                                       sizeof(CBin)   },
    {IR_REM,        "rem",       0x3,    2,    IRT_IS_BIN,                                       sizeof(CBin)   },
    {IR_MOD,        "mod",       0x3,    2,    IRT_IS_BIN,                                       sizeof(CBin)   },
    {IR_LAND,       "land",      0x3,    2,    IRT_IS_BIN|IRT_IS_LOGICAL,                        sizeof(CBin)   },
    {IR_LOR,        "lor",       0x3,    2,    IRT_IS_BIN|IRT_IS_LOGICAL,                        sizeof(CBin)   },
    {IR_BAND,       "band",      0x3,    2,    IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE, sizeof(CBin)   },
    {IR_BOR,        "bor",       0x3,    2,    IRT_IS_BIN|IRT_IS_ASSOCIATIVE,                    sizeof(CBin)   },
    {IR_XOR,        "xor",       0x3,    2,    IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE, sizeof(CBin)   },
    {IR_ASR,        "asr",       0x3,    2,    IRT_IS_BIN,                                       sizeof(CBin)   },
    {IR_LSR,        "lsr",       0x3,    2,    IRT_IS_BIN,                                       sizeof(CBin)   },
    {IR_LSL,        "lsl",       0x3,    2,    IRT_IS_BIN,                                       sizeof(CBin)   },
    {IR_LT,         "lt",        0x3,    2,    IRT_IS_BIN|IRT_IS_RELATION,                       sizeof(CBin)   },
    {IR_LE,         "le",        0x3,    2,    IRT_IS_BIN|IRT_IS_RELATION,                       sizeof(CBin)   },
    {IR_GT,         "gt",        0x3,    2,    IRT_IS_BIN|IRT_IS_RELATION,                       sizeof(CBin)   },
    {IR_GE,         "ge",        0x3,    2,    IRT_IS_BIN|IRT_IS_RELATION,                       sizeof(CBin)   },
    {IR_EQ,         "eq",        0x3,    2,    IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE|IRT_IS_RELATION,  sizeof(CBin)        },
    {IR_NE,         "ne",        0x3,    2,    IRT_IS_BIN|IRT_IS_ASSOCIATIVE|IRT_IS_COMMUTATIVE|IRT_IS_RELATION,  sizeof(CBin)        },
    {IR_BNOT,       "bnot",      0x1,    1,    IRT_IS_UNA,                                     sizeof(CUna)     },
    {IR_LNOT,       "lnot",      0x1,    1,    IRT_IS_UNA|IRT_IS_LOGICAL,                      sizeof(CUna)     },
    {IR_NEG,        "neg",       0x1,    1,    IRT_IS_UNA,                                     sizeof(CUna)     },
    {IR_CVT,        "cvt",       0x1,    1,    IRT_IS_UNA,                                     sizeof(CCvt)     },
    {IR_GOTO,       "goto",      0x0,    0,    IRT_IS_STMT,                                    sizeof(CGoto)    },
    {IR_IGOTO,      "igoto",     0x3,    2,    IRT_IS_STMT,                                    sizeof(CIGoto)   },
    {IR_DO_WHILE,   "do_while",  0x3,    2,    IRT_IS_STMT,                                    sizeof(CDoWhile) },
    {IR_WHILE_DO,   "while_do",  0x3,    2,    IRT_IS_STMT,                                    sizeof(CWhileDo) },
    {IR_DO_LOOP,    "do_loop",   0xF,    4,    IRT_IS_STMT,                                    sizeof(CDoLoop)  },
    {IR_IF,         "if",        0x7,    3,    IRT_IS_STMT,                                    sizeof(CIf)      },
    {IR_LABEL,      "label",     0x0,    0,    IRT_IS_STMT,                                    sizeof(CLab)     },
    {IR_SWITCH,     "switch",    0x7,    3,    IRT_IS_STMT,                                    sizeof(CSwitch)  },
    {IR_CASE,       "case",      0x1,    1,    0,                                              sizeof(CCase)    },
    {IR_TRUEBR,     "truebr",    0x1,    1,    IRT_IS_STMT,                                    sizeof(CTruebr)  },
    {IR_FALSEBR,    "falsebr",   0x1,    1,    IRT_IS_STMT,                                    sizeof(CFalsebr) },
    {IR_RETURN,     "return",    0x1,    1,    IRT_IS_STMT,                                    sizeof(CRet)     },
    {IR_SELECT,     "select",    0x7,    3,    0,                                              sizeof(CSelect)  },
    {IR_BREAK,      "break",     0x0,    0,    IRT_IS_STMT,                                    sizeof(CBreak)   },
    {IR_CONTINUE,   "continue",  0x0,    0,    IRT_IS_STMT,                                    sizeof(CContinue)},
    {IR_PHI,        "phi",       0x1,    1,    IRT_IS_STMT|IRT_HAS_RESULT|IRT_IS_MEM_REF,      sizeof(CPhi)     },
    {IR_REGION,     "region",    0x0,    0,    IRT_IS_STMT,                                    sizeof(CRegion)  },
    {IR_TYPE_NUM,   "LAST IR Type", 0x0, 0,    0,                                              0                },
};


#ifdef _DEBUG_
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
    ASSERT0(ir->is_unary_op());
    return n;
}


INT checkKidNumValidBinary(IR const* ir, UINT n, CHAR const* filename,
                            INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->is_binary_op());
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
    ASSERT0(ir->is_calls_stmt());
    return n;
}


INT checkKidNumValidArray(IR const* ir, UINT n, CHAR const* filename, INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(ir->is_array_op());
    return n;
}


INT checkKidNumIRtype(IR const* ir, UINT n, IR_TYPE irty,
                    CHAR const* filename, INT line)
{
    UINT x = IR_MAX_KID_NUM(ir);
    ASSERTL(n < x, filename, line,
            ("%d is beyond maximum IR kids num %d", n, x));
    ASSERT0(IR_code(ir) == irty);
    return n;
}


IR const* checkIRT(IR const* ir, IR_TYPE irt)
{
    ASSERT(IR_code(ir) == irt, ("current ir is not '%s'", IRTNAME(irt)));
    return ir;
}


IR const* checkIRTBranch(IR const* ir)
{
    ASSERT0(ir->is_cond_br());
    return ir;
}


IR const* checkIRTCall(IR const* ir)
{
    ASSERT0(ir->is_calls_stmt());
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
    for (IR const* sub = ARR_sub_list(ir); sub != NULL; sub = IR_next(sub)) {
        i++;
    }
    ASSERT0(n < i);
    return n;
}


UINT checkStArrayDimension(IR const* ir, UINT n)
{
    UINT i = 0;
    for (IR const* sub = ARR_sub_list(ir); sub != NULL; sub = IR_next(sub)) {
        i++;
    }
    ASSERT0(n < i);
    return n;
}
#endif


//Dump ir list with a clause header.
void dump_irs_h(IR * ir_list , TypeMgr const* dm)
{
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n==---- DUMP IR List ----==\n");
    dump_irs(ir_list, dm, NULL);
}


//Dump IR, and both its kids and siblings.
void dump_irs(IR * ir_list, TypeMgr const* dm, CHAR * attr)
{
    if (g_tfile == NULL) { return; }
    bool first_one = true;
    while (ir_list != NULL) {
        if (first_one) {
            first_one = false;
            dump_ir(ir_list, dm, attr);
        } else {
            dump_ir(ir_list, dm);
        }
        ir_list = IR_next(ir_list);
    }
}


static void verifyIR(IR * ir, IRAddressHash * irh, Region const* ru)
{
    ASSERT0(irh != NULL);
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->get_kid(i);
        if (k != NULL) {
            ASSERT0(IR_parent(k) == ir);
            verify_irs(k, irh, ru);
        }
    }

    //IR can not be used more than twice. Since there will cause
    //memory crash during freeIR().
    ASSERT(!irh->find(ir), ("IR has been used again"));
    irh->append(ir);
    ir->verify(ru);
}


//Check for IR and IRBB sanity and uniqueness.
//Ensure that all IRs must be embedded into a basic block.
//Ensure that PHI must be the first stmt in basic block.
bool verifyIRandBB(BBList * bblst, Region const* ru)
{
    IRAddressHash irh;
    for (IRBB * bb = bblst->get_head();
         bb != NULL; bb = bblst->get_next()) {
        bool should_not_phi = false;

        C<IR*> * irct;
        for (IR * ir = BB_irlist(bb).get_head(&irct);
             ir != NULL; ir = BB_irlist(bb).get_next(&irct)) {
            ASSERT0(IR_next(ir) == NULL && IR_prev(ir) == NULL);
            ASSERT0(IR_parent(ir) == NULL);
            ASSERT0(ir->get_bb() == bb);

            if (!ir->is_phi()) {
                should_not_phi = true;
            }

            if (should_not_phi) {
                ASSERT0(!ir->is_phi());
            }

            verify_irs(ir, &irh, ru);
            ASSERT0(ir->isStmtInBB());
        }
        bb->verify();
    }
    return true;
}


//Check for IR is lowest form.
bool verifyLowestForm(BBList * ir_bb_list, Region * ru)
{
    CK_USE(ru);
    for (IRBB * bb = ir_bb_list->get_head();
         bb != NULL; bb = ir_bb_list->get_next()) {
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            ASSERT0(ru->is_lowest_heigh(ir));
        }
    }
    return true;
}


//Function to verify stmt info after IR simplified.
bool verify_simp(IR * ir_list, SimpCTX & simp)
{
    if (simp.is_simp_cfg()) {
        for (IR * p = ir_list; p != NULL; p = IR_next(p)) {
            ASSERT0(p->is_stmt());
            ASSERT0(IR_parent(p) == NULL);
        }
    }
    return true;
}


//Check for IR sanity and uniqueness.
bool verify_irs(IR * ir, IRAddressHash * irh, Region const* ru)
{
    IRAddressHash * loc = NULL;
    if (irh == NULL) {
        loc = new IRAddressHash();
        irh = loc;
    }
    while (ir != NULL) {
        verifyIR(ir, irh, ru);
        ir = IR_next(ir);
    }
    if (loc != NULL) {
        delete loc;
    }
    return true;
}


void dump_irs(IN List<IR*> & ir_list, TypeMgr const* dm)
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n==---- DUMP IR List ----==\n");
    ASSERT0(dm);
    for (IR * ir = ir_list.get_head();
         ir != NULL; ir = ir_list.get_next()) {
        ASSERT0(IR_next(ir) == NULL && IR_prev(ir) == NULL);
        dump_ir(ir, dm);
    }
    fflush(g_tfile);
}


void dump_irs(IRList & ir_list, TypeMgr const* dm)
{
    dump_irs((List<IR*>&)ir_list, dm);
}


static void ir_dump_lab(IR const* ir)
{
    LabelInfo const* li = ir->get_label();
    if (LABEL_INFO_type(li) == L_ILABEL) {
        fprintf(g_tfile, "ilabel(" ILABEL_STR_FORMAT ")", ILABEL_CONT(li));
    } else if (LABEL_INFO_type(li) == L_CLABEL) {
        fprintf(g_tfile, "clabel(" CLABEL_STR_FORMAT ")", CLABEL_CONT(li));
    } else {
        ASSERT(0, ("unknown label type"));
    }
}


static void dump_ai(OUT CHAR * buf, IR const* ir)
{
    ASSERT0(ir && buf);
    AttachInfo const* ai = ir->get_ai();
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
        }

        sprintf(p, "%s", ai->get_ai_name(ac->type));
        p = p + strlen(p);
    }
}


//Dump IR and all of its kids.
//'attr': miscellaneous string which following 'ir'.
void dump_ir(IR const* ir,
             TypeMgr const* dm,
             IN CHAR * attr,
             bool dump_kid,
             bool dump_src_line,
             bool dump_addr)
{
    UNUSED(dump_addr);
    UNUSED(dump_src_line);
    UNUSED(dump_kid);
    UNUSED(attr);
    UNUSED(dm);
    UNUSED(ir);

    ASSERT0(dm);
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
    sprintf(p, " id:%d", IR_id(ir));
    if (IR_may_throw(ir)) {
        strcat(p, " throw");
    }
    if (IR_is_termiate(ir)) {
        strcat(p, " terminate");
    }
    if (IR_is_atomic(ir)) {
        strcat(p, " atom");
    }
    if (IR_is_read_mod_write(ir)) {
        strcat(p, " rmw");
    }
    if (IR_has_sideeffect(ir)) {
        strcat(p, " sideeffect");
    }

    dump_ai(p, ir);

    //Record type info and var decl.
    static CHAR buf[MAX_BUF_LEN];
    static CHAR buf2[MAX_BUF_LEN];
    buf[0] = 0;
    buf2[0] = 0;
    if (g_dbg_mgr != NULL && dump_src_line) {
        g_dbg_mgr->printSrcLine(ir);
    }

    Type const* d = NULL;
    if (IR_dt(ir) != NULL) {
        d = ir->get_type();
    }

    TypeMgr * xdm = const_cast<TypeMgr*>(dm);
    switch (IR_code(ir)) {
    case IR_ST:
        {
            CHAR tt[40];
            tt[0] = 0;
            CHAR * name = xstrcat(tt, 40, "%s",
                                SYM_name(VAR_name(ST_idinfo(ir))));
            buf[0] = 0;

            //Dump operator and variable name.
            note("\nst:%s", xdm->dump_type(d, buf));
            if (ST_ofst(ir) != 0) {
                fprintf(g_tfile, ":offset(%d)", ST_ofst(ir));
            }
            fprintf(g_tfile, " '%s'", name);

            //Dump declaration info if the frontend supplied.
            buf[0] = 0;
            if (ST_idinfo(ir)->dumpVARDecl(buf, 40) != NULL) {
                fprintf(g_tfile, " decl:%s", buf);
            }

            PADDR(ir);
            fprintf(g_tfile, "%s", attr);

            if (dump_kid) {
                g_indent += dn;
                dump_irs(ST_rhs(ir), dm);
                g_indent -= dn;
            }
        }
        break;
    case IR_STPR:
        note("\nstpr $pr%d:%s", STPR_no(ir), xdm->dump_type(d, buf));
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);

        if (dump_kid) {
            g_indent += dn;
            dump_irs(STPR_rhs(ir), dm);
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
        fprintf(g_tfile, "%s", attr);
        if (ARR_sub_list(ir) != NULL && dump_kid) {
            CHAR tt[40];
            ASSERT0(ARR_elem_num_buf(ir));

            //Dump elem number.
            g_indent += dn;
            UINT dim = 0;
            note("\nenum[");
            for (IR const* sub = ARR_sub_list(ir); sub != NULL;) {
                fprintf(g_tfile, "%d", ARR_elem_num(ir, dim));
                sub = IR_next(sub);
                if (sub != NULL) {
                    fprintf(g_tfile, ",");
                }
                dim++;
            }
            fprintf(g_tfile, "]");

            //Dump sub exp list.
            dim = 0;
            for (IR const* sub = ARR_sub_list(ir);
                 sub != NULL; sub = IR_next(sub)) {
                sprintf(tt, " dim%d", dim);
                dump_ir(sub, dm, (CHAR*)tt);
                dim++;
            }
            g_indent -= dn;
        }
        if (dump_kid) {
            g_indent += dn;
            dump_irs(ARR_base(ir), dm, (CHAR*)" array_base");
            dump_irs(STARR_rhs(ir), dm, (CHAR*)" rhs");
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
        fprintf(g_tfile, "%s", attr);

        if (dump_kid) {
            g_indent += dn;
            dump_irs(IST_base(ir), dm, (CHAR*)" lhs");
            dump_irs(IST_rhs(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_LD:
        {
            CHAR tt[40];
            tt[0] = 0;

            //Dump variable info.
            CHAR * name = xstrcat(tt, 40, "%s",
                                SYM_name(VAR_name(LD_idinfo(ir))));
            buf[0] = 0;
            if (LD_ofst(ir) != 0) {
                note("\nld:%s:offset(%d) '%s'",
                     xdm->dump_type(d, buf), LD_ofst(ir), name);
            } else {
                note("\nld:%s '%s'", xdm->dump_type(d, buf), name);
            }

            //Dump declaration if frontend supplied.
            buf[0] = 0;
            if (LD_idinfo(ir)->dumpVARDecl(buf, 40) != NULL) {
                fprintf(g_tfile, " decl:%s", buf);
            }

            //Dump IR address.
            PADDR(ir);
            fprintf(g_tfile, "%s", attr);
        }
        break;
    case IR_ILD:
        if (ILD_ofst(ir) != 0) {
            note("\nild:%s:offset(%d)", xdm->dump_type(d, buf), ILD_ofst(ir));
        } else {
            note("\nild:%s", xdm->dump_type(d, buf));
        }

        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(ILD_base(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_PR:
        note("\n$pr%d:%s", PR_no(ir), xdm->dump_type(d, buf));
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        break;
    case IR_ID:
        {
            buf[0] = 0;
            CHAR tt[40];
            tt[0] = 0;

            //Dump ID name.
            CHAR * name = xstrcat(tt, 40, "%s", SYM_name(VAR_name(ID_info(ir))));
            note("\nid:%s '%s'", xdm->dump_type(d, buf), name);
            buf[0] = 0;
            if (ID_info(ir)->dumpVARDecl(buf, 40) != NULL) {
                fprintf(g_tfile, " decl:%s", buf);
            }

            //Dump IR address.
            PADDR(ir);
            fprintf(g_tfile, "%s", attr);
            break;
        }
    case IR_CONST:
        if (ir->is_sint(dm)) {
            note("\nintconst:%s %lld|0x%llx",
                 xdm->dump_type(d, buf),
                 CONST_int_val(ir),
                 CONST_int_val(ir));
        } else if (ir->is_uint(dm)) {
            note("\nintconst:%s %llu|0x%llx",
                 xdm->dump_type(d, buf),
                 CONST_int_val(ir),
                 CONST_int_val(ir));
        } else if (ir->is_fp(dm)) {
            note("\nfpconst:%s %f",
                 xdm->dump_type(d, buf),
                 CONST_fp_val(ir));
        } else if (ir->is_bool()) {
            note("\nboolconst:%s %d",
                 xdm->dump_type(d, buf),
                 (UINT)CONST_int_val(ir));
        } else if (ir->is_str()) {
            UINT const tbuflen = 40;
            CHAR tbuf[tbuflen];
            tbuf[0] = 0;
            xstrcat(tbuf, tbuflen, "%s", SYM_name(CONST_str_val(ir)));

            //Remove \n to show string in pretty format.
            for (UINT i = 0; i < tbuflen && tbuf[i] != 0; i++) {
                if (tbuf[i] == '\n') { tbuf[i] = ' '; }
            }

            buf[0] = 0;
            note("\nstrconst:%s \\\"%s....\\\"", xdm->dump_type(d, buf), tbuf);
        } else if (ir->is_mc()) {
            //ASSERT(0, ("immediate value cannot be MEM block"));
            note("\nintconst:%s %lld|0x%llx",
                 xdm->dump_type(d, buf),
                 CONST_int_val(ir),
                 CONST_int_val(ir));
        } else {
            //Dump const even if it is illegal type, leave the assertion
            //work to verify().
            note("\nintconst:%s %lld|0x%llx",
                 xdm->dump_type(d, buf),
                 CONST_int_val(ir),
                 CONST_int_val(ir));
            //ASSERT(0, ("unsupport immediate value DATA_TYPE:%d",
            //        ir->get_dtype(dm)));
        }
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        break;
    case IR_LOR: //logical or ||
    case IR_LAND: //logical and &&
    case IR_BOR: //inclusive or |
    case IR_BAND: //inclusive and &
    case IR_XOR: //exclusive or
    case IR_EQ: //==
    case IR_NE: //!=
    case IR_LT:
    case IR_GT:
    case IR_GE:
    case IR_LE:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_ADD:
    case IR_SUB:
    case IR_DIV:
    case IR_MUL:
    case IR_MOD:
    case IR_REM:
        note("\n%s:%s", IRNAME(ir), xdm->dump_type(d, buf));
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(BIN_opnd0(ir), dm);
            dump_irs(BIN_opnd1(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_IF:
        note("\nif");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(IF_det(ir), dm);
            g_indent -= dn;

            note("\n{");
            g_indent += dn;
            dump_irs(IF_truebody(ir), dm);
            g_indent -= dn;
            note("\n}");

            if (IF_falsebody(ir)) {
                note("\n else");
                note("\n{");
                g_indent += dn;
                dump_irs(IF_falsebody(ir), dm);
                g_indent -= dn;
                note("\n}");
            }
        }
        break;
    case IR_DO_WHILE:
        note("\ndo_while");
        note("\nbody");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);

        if (dump_kid) {
            g_indent += dn;
            dump_irs(LOOP_body(ir), dm);
            g_indent -= dn;

            note("\ndet");
            g_indent += dn;
            dump_irs(LOOP_det(ir), dm);
            g_indent -= dn;

            note("\nend_do_while");
        }
        break;
    case IR_WHILE_DO:
        note("\nwhile_do");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);

        if (dump_kid) {
            g_indent += dn;
            dump_irs(LOOP_det(ir), dm);
            g_indent -= dn;

            note("\nbody");

            g_indent += dn;
            dump_irs(LOOP_body(ir), dm);
            g_indent -= dn;

            note("\nend_while_do");
        }
        break;
    case IR_DO_LOOP:
        note("\ndo_loop");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);

        if (dump_kid) {
            g_indent += dn;
            note("\ninit");

            g_indent += dn;
            dump_irs(LOOP_init(ir), dm);
            g_indent -= dn;

            note("\ndet");

            g_indent += dn;
            dump_irs(LOOP_det(ir), dm);
            g_indent -= dn;

            note("\nstep");

            g_indent += dn;
            dump_irs(LOOP_step(ir), dm);
            g_indent -= dn;

            note("\ndo_loop_body");

            g_indent += dn;
            dump_irs(LOOP_body(ir), dm);
            g_indent -= dn;
            note("\nend_do_loop");
        }
        break;
    case IR_BREAK:
        note("\nbreak");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        break;
    case IR_CONTINUE:
        note("\ncontinue");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        break;
    case IR_RETURN:
        note("\nreturn");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_ir(RET_exp(ir), dm, NULL, dump_kid,
                    dump_src_line, dump_addr);
            g_indent -= dn;
        }
        break;
    case IR_GOTO:
        note("\ngoto ");
        ir_dump_lab(ir);
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        break;
    case IR_IGOTO:
        note("\nigoto");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(IGOTO_vexp(ir), dm);
            g_indent -= dn;

            note("\ncase_list");
            g_indent += dn;
            dump_irs(IGOTO_case_list(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_LABEL:
        {
            LabelInfo const* li = LAB_lab(ir);
            if (LABEL_INFO_type(li) == L_ILABEL) {
                note("\nilabel(" ILABEL_STR_FORMAT ")",
                     ILABEL_CONT(LAB_lab(ir)));
            } else if (LABEL_INFO_type(li) == L_CLABEL) {
                note("\nclabel(" CLABEL_STR_FORMAT ")",
                     CLABEL_CONT(LAB_lab(ir)));
            } else if (LABEL_INFO_type(li) == L_PRAGMA) {
                note("\npragms(%s)", SYM_name(LABEL_INFO_pragma(LAB_lab(ir))));
            } else { ASSERT0(0); }

            PADDR(ir);

            if (LABEL_INFO_b1(li) != 0) {
                fprintf(g_tfile, "(");
            }

            if (LABEL_INFO_is_try_start(li)) {
                fprintf(g_tfile, "try_start ");
            }

            if (LABEL_INFO_is_try_end(li)) {
                fprintf(g_tfile, "try_end ");
            }

            if (LABEL_INFO_is_catch_start(li)) {
                fprintf(g_tfile, "catch_start ");
            }

            if (LABEL_INFO_is_used(li)) {
                fprintf(g_tfile, "used ");
            }

            if (LABEL_INFO_b1(li) != 0) {
                fprintf(g_tfile, ")");
            }

            fprintf(g_tfile, "%s", attr);
        }
        break;
    case IR_SELECT: //formulized log_OR_exp?exp:cond_exp
        note("\nselect:%s", xdm->dump_type(d, buf));

        PADDR(ir); fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(SELECT_det(ir), dm);
            g_indent -= dn;

            g_indent += dn;
            dump_irs(SELECT_trueexp(ir), dm, (CHAR*)" true_exp");
            g_indent -= dn;

            g_indent += dn;
            dump_irs(SELECT_falseexp(ir), dm, (CHAR*)" false_exp");
            g_indent -= dn;
        }
        break;
    case IR_CVT: //type convertion
        note("\ncvt:%s", xdm->dump_type(d, buf));

        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(CVT_exp(ir), dm, (CHAR*)" cvtbase");
            g_indent -= dn;
        }
        break;
    case IR_LDA: //Get address of a symbol
        if (LDA_ofst(ir) != 0) {
            note("\nlda:%s:offset(%d)", xdm->dump_type(d, buf), LDA_ofst(ir));
        } else {
            note("\nlda:%s", xdm->dump_type(d, buf));
        }

        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(LDA_base(ir), dm);
            g_indent -= dn;
        }
        break;
      case IR_NEG: // -123
        note("\nneg:%s", xdm->dump_type(d, buf));
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(UNA_opnd0(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_PHI:
        note("\n$pr%d:%s = phi", PHI_prno(ir), xdm->dump_type(d, buf));

        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            IR * opnd = PHI_opnd_list(ir);
            while (opnd != NULL) {
                dump_ir(opnd, dm, NULL);
                opnd = IR_next(opnd);
            }
            g_indent -= dn;
        }
        break;
    case IR_BNOT: //bitwise not
        note("\nbnot:%s", xdm->dump_type(d, buf));

        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(UNA_opnd0(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_LNOT: //logical not
        note("\nlnot:%s", xdm->dump_type(d, buf));

        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(UNA_opnd0(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_SWITCH:
        note("\nswitch");
        if (SWITCH_deflab(ir) != NULL) {
            fprintf(g_tfile, ", deflab: ");
            ir_dump_lab(ir);
        }
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(SWITCH_vexp(ir), dm);
            g_indent -= dn;

            note("\ncase_list");
            g_indent += dn;
            dump_irs(SWITCH_case_list(ir), dm);
            g_indent -= dn;

            note("\nswitch_body");
            g_indent += dn;
            dump_irs(SWITCH_body(ir), dm);
            g_indent -= dn;
            note("\nend_switch");
        }
        break;
    case IR_CASE:
        {
            ASSERT0(CASE_vexp(ir));
            ASSERT0(CASE_lab(ir));
            note("\ncase %d, ", CONST_int_val(CASE_vexp(ir)));
            ir_dump_lab(ir);
            PADDR(ir);
            fprintf(g_tfile, "%s", attr);
        }
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
        fprintf(g_tfile, "%s", attr);
        if (ARR_sub_list(ir) != NULL && dump_kid) {
            CHAR tt[40];
            ASSERT0(ARR_elem_num_buf(ir));

            //Dump elem number.
            g_indent += dn;
            UINT dim = 0;
            note("\nenum[");
            for (IR const* sub = ARR_sub_list(ir); sub != NULL;) {
                fprintf(g_tfile, "%d", ARR_elem_num(ir, dim));
                sub = IR_next(sub);
                if (sub != NULL) {
                    fprintf(g_tfile, ",");
                }
                dim++;
            }
            fprintf(g_tfile, "]");

            //Dump sub exp list.
            dim = 0;
            for (IR const* sub = ARR_sub_list(ir);
                 sub != NULL; sub = IR_next(sub)) {
                sprintf(tt, " dim%d", dim);
                dump_ir(sub, dm, (CHAR*)tt);
                dim++;
            }
            g_indent -= dn;
        }
        if (dump_kid) {
            g_indent += dn;
            dump_irs(ARR_base(ir), dm, (CHAR*)" array_base");
            g_indent -= dn;
        }
        break;
    case IR_CALL:
    case IR_ICALL:
        {
            if (ir->hasReturnValue()) {
                note("\n$pr%d:%s = ", CALL_prno(ir), xdm->dump_type(d, buf));
            } else {
                note("\n");
            }

            if (ir->is_call()) {
                CHAR tt[44];
                tt[0] = 0;

                CHAR const* string = SYM_name(VAR_name(CALL_idinfo(ir)));

                CHAR * name = xstrcat(tt, 40, "%s", string);
                if (strlen(string) > 40) {
                    strcat(tt, "...");
                }

                fprintf(g_tfile, "call '%s' ", name);

                buf[0] = 0;
                if (CALL_idinfo(ir)->dumpVARDecl(buf, 40) != NULL) {
                    fprintf(g_tfile, "decl:%s", buf);
                }
            } else {
                fprintf(g_tfile, "icall ");
            }

            PADDR(ir);
            fprintf(g_tfile, "%s", attr);

            if (dump_kid) {
                if (ir->is_icall()) {
                    g_indent += dn;
                    dump_ir(ICALL_callee(ir), dm, (CHAR*)" callee",
                            dump_kid, dump_src_line, dump_addr);
                    g_indent -= dn;
                }

                //Dump parameter list.
                IR * tmp = CALL_param_list(ir);
                UINT i = 0;
                CHAR tmpbuf[30];
                while (tmp != NULL) {
                    sprintf(tmpbuf, " param%d", i);
                    g_indent += dn;
                    dump_ir(tmp, dm, tmpbuf, dump_kid,
                            dump_src_line, dump_addr);
                    g_indent -= dn;
                    i++;
                    tmp = IR_next(tmp);
                }
            }
            break;
        }
    case IR_TRUEBR:
        note("\ntruebr ");
        ir_dump_lab(ir);
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(BR_det(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_FALSEBR:
        note("\nfalsebr ");
        ir_dump_lab(ir);
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        if (dump_kid) {
            g_indent += dn;
            dump_irs(BR_det(ir), dm);
            g_indent -= dn;
        }
        break;
    case IR_REGION:
        note("\nregion");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        break;
    case IR_UNDEF:
        note("\nundef!");
        PADDR(ir);
        fprintf(g_tfile, "%s", attr);
        break;
    default:
        ASSERT(0, ("unknown IR type:%s", IRNAME(ir)));
        return ;
    } //end switch
    fflush(g_tfile);
} //end dump_ir


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
    default: ASSERT(0, ("TODO"));
    }
    return p;
}
///


bool CRegion::is_readonly() const
{
    ASSERT0(ru);
    return REGION_is_readonly(ru);
}


//
//START IR
//
//Check that IR cannot take a UNDEF type.
bool IR::verify(Region const* ru) const
{
    verifyKids();

    TypeMgr const* dm = ru->get_type_mgr();
    ASSERT0(dm);

    Type const* d = IR_dt(this);
    ASSERT0(d);

    if (d->is_pointer()) {
        ASSERT0(TY_ptr_base_size(d) != 0);
    } else if (d->is_mc()) {
        ASSERT0(TY_mc_size(d) != 0);
    } else {
        /*
        IR_mc_size may be not zero.
        e.g: struct {int x;}s;
            int a = s.x;
            Here we get a IR_LD(s, offset=0, mc_size=4)
        */
        //ASSERT0(IR_mc_size(this) == 0);
    }

    if (d->is_vector()) {
        ASSERT0(TY_vec_ety(d) != D_UNDEF);

        ASSERT(IS_SIMPLEX(TY_vec_ety(d)) || IS_PTR(TY_vec_ety(d)),
                ("illegal vector elem type"));

        ASSERT0(TY_vec_size(d) >= dm->get_dtype_bytesize(TY_vec_ety(d)) &&
                 TY_vec_size(d) % dm->get_dtype_bytesize(TY_vec_ety(d)) == 0);

        UINT ofst = get_offset();
        if (ofst != 0) {
            ASSERT0((ofst % dm->get_dtype_bytesize(TY_vec_ety(d))) == 0);
        }
    }

    switch (IR_code(this)) {
    case IR_UNDEF: ASSERT(0, ("should not be undef")); break;
    case IR_CONST:
        ASSERT0(d);
        ASSERT(TY_dtype(d) != D_UNDEF, ("size of load value cannot be zero"));
        if (!is_sint(dm) &&
            !is_uint(dm) &&
            !is_fp(dm) &&
            !is_bool() &&
            //!IR_dt(this)->is_mc() && //IR_CONST can not be MC.
            !is_str()) {
            ASSERT(0, ("unsupport immediate value DATA_TYPE:%d",
                    get_dtype()));
        }
        break;
    case IR_ID: break;
    case IR_LD:
        //src of LD might be small or big compare with REG.
        ASSERT0(LD_idinfo(this));
        ASSERT0(d);
        ASSERT(TY_dtype(d) != D_UNDEF, ("size of load value cannot be zero"));
        break;
    case IR_ST:
        ASSERT0(d);
        ASSERT(TY_dtype(d)!= D_UNDEF, ("size of store value cannot be zero"));
        ASSERT0(ST_idinfo(this));
        ASSERT0(ST_rhs(this)->is_exp());
        ASSERT0(IR_next(ST_rhs(this)) == NULL &&
                 IR_prev(ST_rhs(this)) == NULL);
        if (d->is_vector()) {
            ASSERT0(TY_vec_ety(d) != D_UNDEF);
            ASSERT0(dm->get_dtype_bytesize(TY_vec_ety(d)) >=
                     dm->get_bytesize(IR_dt(ST_rhs(this))));
        }
        break;
    case IR_STPR:
        ASSERT0(d);
        ASSERT(get_dtype() != D_UNDEF, ("size of store value cannot be zero"));
        ASSERT0(get_dtype() != D_UNDEF);
        ASSERT0(STPR_no(this) > 0);
        ASSERT0(STPR_rhs(this)->is_exp());
        ASSERT0(IR_next(STPR_rhs(this)) == NULL &&
                 IR_prev(STPR_rhs(this)) == NULL);
        break;
    case IR_STARRAY:
        //Stmt properties check.
        ASSERT0(d);
        ASSERT(get_dtype() != D_UNDEF, ("size of store value cannot be zero"));
        ASSERT0(get_dtype() != D_UNDEF);
        ASSERT0(STARR_rhs(this)->is_exp());
        ASSERT0(IR_next(STARR_rhs(this)) == NULL &&
                 IR_prev(STARR_rhs(this)) == NULL);

        //Array properties check.
        ASSERT0(ARR_base(this)->is_ptr());
        ASSERT0(ARR_elemtype(this) != 0);
        ASSERT0(ARR_elemtype(this));

        if (ARR_ofst(this) != 0) {
            ASSERT0(dm->get_bytesize(d) + ARR_ofst(this) <=
                     dm->get_bytesize(ARR_elemtype(this)));
        }

        ASSERT0(IR_next(ARR_base(this)) == NULL &&
                 IR_prev(ARR_base(this)) == NULL);

        ASSERT(ARR_sub_list(this),
                ("subscript expression can not be null"));
        break;
    case IR_ILD:
        ASSERT0(d);
        ASSERT0(get_dtype() != D_UNDEF);

        ASSERT0(ILD_base(this));

        ASSERT(ILD_base(this)->is_ptr(), ("base must be pointer"));

        ASSERT0(ILD_base(this)->is_exp());
        ASSERT0(IR_next(ILD_base(this)) == NULL &&
                 IR_prev(ILD_base(this)) == NULL);
        break;
    case IR_IST:
        ASSERT0(d);
        ASSERT(IST_base(this)->is_ptr(), ("base must be pointer"));
        ASSERT0(IST_rhs(this)->is_exp());
        ASSERT(get_dtype() != D_UNDEF, ("size of istore value cannot be zero"));
        ASSERT0(IR_next(IST_base(this)) == NULL &&
                 IR_prev(IST_base(this)) == NULL);
        ASSERT0(IR_next(IST_rhs(this)) == NULL &&
                 IR_prev(IST_rhs(this)) == NULL);
        break;
    case IR_SETELEM:
        ASSERT0(d);

        ASSERT0(SETELEM_rhs(this) && SETELEM_ofst(this));

        ASSERT0(d->is_mc() || d->is_vector());

        if (d->is_vector()) {
            ASSERT0(TY_vec_ety(d) != D_UNDEF);

            //Note if the rhssize less than elemsize, it will be hoist to
            //elemsize.
            ASSERT0(dm->get_dtype_bytesize(TY_vec_ety(d)) >=
                     dm->get_bytesize(IR_dt(ST_rhs(this))));
        }

        ASSERT0(SETELEM_rhs(this)->is_exp());

        ASSERT0(SETELEM_ofst(this)->is_exp());

        ASSERT0(TY_dtype(d) != D_UNDEF);

        ASSERT0(IR_next(SETELEM_ofst(this)) == NULL &&
                 IR_prev(SETELEM_ofst(this)) == NULL);

        ASSERT0(IR_next(IST_rhs(this)) == NULL &&
                 IR_prev(IST_rhs(this)) == NULL);
        break;
    case IR_GETELEM:
        {
            ASSERT0(d);
            ASSERT0(TY_dtype(d) != D_UNDEF);

            IR const* base = GETELEM_base(this);
            ASSERT0(base && GETELEM_ofst(this));
            Type const* basedtd = IR_dt(base);
            ASSERT0(basedtd->is_mc() || basedtd->is_vector());

            if (basedtd->is_vector()) {
                ASSERT0(TY_vec_ety(basedtd) != D_UNDEF);

                ASSERT0(dm->get_dtype_bytesize(TY_vec_ety(basedtd)) >=
                         dm->get_bytesize(d));
            }
        }
        break;
    case IR_LDA:
        ASSERT0(d);
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(LDA_base(this)->is_id() ||
                LDA_base(this)->is_str() ||
                LDA_base(this)->is_lab());

        ASSERT0(d->is_pointer());
        ASSERT0(IR_next(LDA_base(this)) == NULL &&
                IR_prev(LDA_base(this)) == NULL);
        break;
    case IR_CALL:
        ASSERT0(CALL_idinfo(this));

        //result type of call is the type of return value if it exist.
        if (d->is_void()) {
            ASSERT0(CALL_prno(this) == 0);
        } else {
            ASSERT0(CALL_prno(this) != 0);
        }

        //Parameters should be expression.
        for (IR * p = CALL_param_list(this); p != NULL; p = IR_next(p)) {
            ASSERT0(p->is_exp());
        }
        break;
    case IR_ICALL:
        //result type of call is the type of return value if it exist.
        if (d->is_void()) {
            ASSERT0(CALL_prno(this) == 0);
        } else {
            ASSERT0(CALL_prno(this) != 0);
        }

        //rtype of icall is the type of IR in return-value-list.
        ASSERT0(ICALL_callee(this)->is_ld() || ICALL_callee(this)->is_pr());

        ASSERT0(IR_next(ICALL_callee(this)) == NULL &&
                 IR_prev(ICALL_callee(this)) == NULL);

        for (IR * p = CALL_param_list(this); p != NULL; p = IR_next(p)) {
            ASSERT0(p->is_exp());
        }
        break;
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        ASSERT0(d);
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(BIN_opnd0(this) != NULL &&
                 BIN_opnd0(this)->is_exp() &&
                 BIN_opnd1(this) != NULL &&
                 BIN_opnd1(this)->is_exp());

        //Check that shift operations only have integer type.
        ASSERT0(BIN_opnd0(this)->is_int(dm) &&
                 BIN_opnd1(this)->is_int(dm));

        ASSERT0(IR_next(BIN_opnd0(this)) == NULL &&
                 IR_prev(BIN_opnd0(this)) == NULL);
        ASSERT0(IR_next(BIN_opnd1(this)) == NULL &&
                 IR_prev(BIN_opnd1(this)) == NULL);
        break;
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_XOR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_BAND:
    case IR_BOR:
        ASSERT0(d);
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(BIN_opnd0(this) != NULL &&
                 BIN_opnd0(this)->is_exp() &&
                 BIN_opnd1(this) != NULL &&
                 BIN_opnd1(this)->is_exp());
        ASSERT0(IR_next(BIN_opnd0(this)) == NULL &&
                 IR_prev(BIN_opnd0(this)) == NULL);
        ASSERT0(IR_next(BIN_opnd1(this)) == NULL &&
                 IR_prev(BIN_opnd1(this)) == NULL);
        break;
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
        ASSERT0(d);
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(UNA_opnd0(this) != NULL &&
                 UNA_opnd0(this)->is_exp());
        ASSERT0(IR_next(UNA_opnd0(this)) == NULL &&
                 IR_prev(UNA_opnd0(this)) == NULL);
        break;
    case IR_GOTO:
        ASSERT0(GOTO_lab(this));
        break;
    case IR_IGOTO:
        ASSERT(IGOTO_vexp(this), ("igoto vexp can not be NULL."));
        ASSERT(IGOTO_case_list(this), ("igoto case list can not be NULL."));
        ASSERT(IR_next(IGOTO_vexp(this)) == NULL &&
                IR_prev(IGOTO_vexp(this)) == NULL,
                ("igoto vexp can NOT be in list."));
        break;
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
        ASSERT0(IR_next(LOOP_det(this)) == NULL &&
                 IR_prev(LOOP_det(this)) == NULL);
        break;
    case IR_IF:
        ASSERT0(IR_next(IF_det(this)) == NULL &&
                 IR_prev(IF_det(this)) == NULL);
        //Fall through.
    case IR_LABEL:
        break;
    case IR_SWITCH:
        ASSERT(SWITCH_vexp(this), ("switch vexp can not be NULL."));

        //SWITCH case list can be NULL.
        ASSERT0(IR_next(SWITCH_vexp(this)) == NULL &&
                 IR_prev(SWITCH_vexp(this)) == NULL);
        break;
    case IR_CASE:
        ASSERT0(CASE_lab(this));
        ASSERT0(IR_next(CASE_vexp(this)) == NULL &&
                 IR_prev(CASE_vexp(this)) == NULL);
        break;
    case IR_ARRAY:
        {
            ASSERT0(d);
            ASSERT0(TY_dtype(d) != D_UNDEF);
            ASSERT0(ARR_base(this)->is_ptr());
            ASSERT0(ARR_elemtype(this) != 0);
            ASSERT0(ARR_elemtype(this));

            if (ARR_ofst(this) != 0) {
                ASSERT0(dm->get_bytesize(d) + ARR_ofst(this) <=
                         dm->get_bytesize(ARR_elemtype(this)));
            }

            ASSERT0(IR_next(ARR_base(this)) == NULL &&
                     IR_prev(ARR_base(this)) == NULL);

            ASSERT0(ARR_sub_list(this));
        }
        break;
    case IR_CVT:
        ASSERT0(d);
        ASSERT0(CVT_exp(this) != NULL &&
                 CVT_exp(this)->is_exp());
        ASSERT0(IR_next(CVT_exp(this)) == NULL &&
                 IR_prev(CVT_exp(this)) == NULL);
        break;
    case IR_PR:
        ASSERT0(d);
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(get_dtype() != D_UNDEF);
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        {
            ASSERT0(BR_lab(this));
            ASSERT0(BR_det(this) && BR_det(this)->is_judge());
            ASSERT0(IR_next(BR_det(this)) == NULL &&
                     IR_prev(BR_det(this)) == NULL);
        }
        break;
    case IR_RETURN:
        if (RET_exp(this) != NULL) {
            ASSERT0(RET_exp(this)->is_exp());
            ASSERT0(IR_next(RET_exp(this)) == NULL &&
                     IR_prev(RET_exp(this)) == NULL);
        }
        break;
    case IR_SELECT:
        //true_exp's type might not equal to false_exp's.
        ASSERT0(d);
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(SELECT_trueexp(this) && SELECT_trueexp(this)->is_exp());
        ASSERT0(SELECT_falseexp(this) && SELECT_falseexp(this)->is_exp());
        ASSERT0(SELECT_det(this) && SELECT_det(this)->is_judge());

        ASSERT0(IR_next(SELECT_trueexp(this)) == NULL &&
                 IR_prev(SELECT_trueexp(this)) == NULL);
        ASSERT0(IR_next(SELECT_falseexp(this)) == NULL &&
                 IR_prev(SELECT_falseexp(this)) == NULL);
        ASSERT0(IR_next(SELECT_det(this)) == NULL &&
                 IR_prev(SELECT_det(this)) == NULL);
        break;
    case IR_BREAK:
    case IR_CONTINUE:
        break;
    case IR_PHI:
        ASSERT0(d);
        ASSERT0(TY_dtype(d) != D_UNDEF);
        ASSERT0(get_dtype() != D_UNDEF);
        ASSERT0(PHI_prno(this) > 0);

        //PHI must have at least one opnd.
        ASSERT0(PHI_opnd_list(this) != NULL);
        ASSERT0(verifyPhi(ru));
        break;
    case IR_REGION:
        break;
    default: ASSERT0(0);
    }
    return true;
}


/* This function verify def/use information of PHI stmt.
If vpinfo is available, the function also check VP_prno of phi operands.
is_vpinfo_avail: set true if VP information is available. */
bool IR::verifyPhi(Region const* ru) const
{
    ASSERT0(is_phi());
    List<IRBB*> preds;
    IR_CFG * cfg = ru->get_cfg();
    IRBB * bb = get_bb();
    ASSERT0(bb);
    cfg->get_preds(preds, bb);

    UINT num_pred = preds.get_elem_count();
    UNUSED(num_pred);

    //Check the number of phi opnds.
    UINT num_opnd = 0;
    for (IR const* opnd = PHI_opnd_list(this);
         opnd != NULL; opnd = IR_next(opnd)) {
        num_opnd++;
    }
    ASSERT(num_opnd == num_pred, ("the num of opnd unmatch"));

    SSAInfo * ssainfo = get_ssainfo();
    ASSERT0(ssainfo);

    SSAUseIter vit = NULL;
    for (INT i = SSA_uses(ssainfo).get_first(&vit);
         vit != NULL; i = SSA_uses(ssainfo).get_next(i, &vit)) {
        IR const* use = const_cast<Region*>(ru)->get_ir(i);

        if (!use->is_pr()) { continue; }

        ASSERT(PR_no(use) == PHI_prno(this), ("prno is unmatch"));

        SSAInfo * use_ssainfo = PR_ssainfo(use);
        CK_USE(use_ssainfo);

        ASSERT0(SSA_def(use_ssainfo) == this);
    }
    return true;
}


bool IR::verifyKids() const
{
    ULONG kidbit = 1;
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++, kidbit <<= 1) {
        IR * k = get_kid(i);
        if (k != NULL) {
            ASSERT0(IR_parent(k) == this);
        }
        if (!HAVE_FLAG(IRDES_kid_map(g_ir_desc[IR_code(this)]), kidbit)) {
            ASSERT(k == NULL,
                    ("IR_%s does not have No.%d kid", IRNAME(this), i));
        } else {
            //Here, ith kid cannot be NULL.
            /* CASE: Kind of node permit some of their kid to be NULL.
            For now include IR_IF, IR_RETURN, IR_DO_LOOP, etc. */
            if (k == NULL) {
                switch (IR_code(this)) {
                case IR_IF:
                case IR_DO_LOOP:
                case IR_WHILE_DO:
                case IR_SWITCH:
                case IR_DO_WHILE:
                    if (i == 0) {
                        ASSERT(k != NULL,
                                ("IR_%s miss kid%d", IRNAME(this), i));
                    }
                    break;
                case IR_ICALL:
                    if (i == 2) {
                        ASSERT(k != NULL,
                                ("IR_%s miss kid%d", IRNAME(this), i));
                    }
                    break;
                case IR_RETURN:
                case IR_CALL:
                    break;
                default:
                    ASSERT(k != NULL, ("IR_%s miss kid%d", IRNAME(this), i));
                }
            }
        }
    }
    return true;
}


/* Calculate the accumulated offset value from the base of array.
e.g: For given array long long p[10][20],
the offset of p[i][j] can be computed by i*20 + j, and
the offset of p[i] can be computed by i*20.

If all the indice are constant value, calcuate the value, storing
in 'ofst_val' and return true, otherwise return false, means the
ofst can not be determined. */
bool IR::calcArrayOffset(TMWORD * ofst_val, TypeMgr * dm) const
{
    if (!is_array() && !is_starray()) { return false; }

    TMWORD aggr = 0;
    UINT dim = 0;
    for (IR const* s = ARR_sub_list(this); s != NULL; s = IR_next(s), dim++) {
        ASSERT0(ARR_elem_num_buf(this));
        if (!s->is_const()) { return false; }

        ASSERT0(!s->is_fp(dm) && CONST_int_val(s) >= 0);

        #ifdef _VC2010_
        #define MARK_32BIT 0xFFFFffff00000000lu
        #else
        #define MARK_32BIT 0xFFFFffff00000000llu
        #endif
        ASSERT((((ULONGLONG)CONST_int_val(s)) &
                 (ULONGLONG)(LONGLONG)MARK_32BIT) == 0,
                ("allow 32bit array offset."));

        ASSERT0(ARR_elem_num(this, dim) != 0);
        //Array index start at 0.
        if (dim == 0) {
            aggr = (TMWORD)CONST_int_val(s);
        } else {
            aggr += ARR_elem_num(this, dim - 1) * (TMWORD)CONST_int_val(s);
        }
    }

    aggr *= dm->get_bytesize(ARR_elemtype(this));
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
        if (!pthis->is_ir_equal(irs, is_cmp_kid)) {
            return false;
        }
        irs = IR_next(irs);
        pthis = IR_next(pthis);
    }
    if ((irs != NULL) ^ (pthis != NULL)) {
        return false;
    }
    return true;
}


//Return true if ir are equivalent.
//'is_cmp_kid': it is true if comparing kids as well.
bool IR::is_ir_equal(IR const* src, bool is_cmp_kid) const
{
    ASSERT0(src != NULL);

    //Compare opcode.
    if (IR_code(this) != IR_code(src)) { return false; }

    switch (IR_code(src)) {
    case IR_CONST: //Constant value: include integer, float, string.
        if (CONST_int_val(this) != CONST_int_val(src)) return false;
        break;
    case IR_ID:
        if (ID_info(this) != ID_info(src)) return false;
        break;
    case IR_LD:
        if (LD_idinfo(this) != LD_idinfo(src) ||
            LD_ofst(this) != LD_ofst(src) ||
            IR_dt(this) != IR_dt(src)) {
            return false;
        }
        break;
    case IR_ILD:
        if (ILD_ofst(this) != ILD_ofst(src) ||
            IR_dt(this) != IR_dt(src)) {
            return false;
        }
        break;
     case IR_ST:
        if (ST_idinfo(this) != ST_idinfo(src) ||
            ST_ofst(this) != ST_ofst(src) ||
            IR_dt(this) != IR_dt(src)) {
            return false;
        }
        break;
    case IR_STPR:
        if (IR_dt(this) != IR_dt(src) || STPR_no(this) != STPR_no(src)) {
            return false;
        }
        break;
    case IR_STARRAY:
        if (ARR_ofst(this) != ARR_ofst(src) || IR_dt(this) != IR_dt(src)) {
            return false;
        }
        break;
    case IR_IST:
        if (IST_ofst(this) != IST_ofst(src) ||
            IR_dt(this) != IR_dt(src)) {
            return false;
        }
        break;
    case IR_LDA:
        if (LDA_ofst(this) != LDA_ofst(src) ||
            IR_dt(this) != IR_dt(src)) {
            return false;
        }
        break;
    case IR_CALL:
        if (CALL_idinfo(this) != CALL_idinfo(src)) return false;
        break;
    case IR_ICALL:
        break;
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_CVT:
        if (IR_dt(this) != IR_dt(src)) return false;
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
        if (ARR_ofst(this) != ARR_ofst(src) || IR_dt(this) != IR_dt(src)) {
            return false;
        }
        break;
    case IR_PR:
        if (IR_dt(this) != IR_dt(src) || PR_no(this) != PR_no(src)) {
            return false;
        }
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SELECT:
        if (IR_dt(this) != IR_dt(src)) {
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
    default: ASSERT0(0);
    }

    //Compare kids.
    for (INT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        IR * kid1 = get_kid(i);
        IR * kid2 = src->get_kid(i);
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
        IR * kid = get_kid(i);
        if (kid != NULL) {
            while (kid != NULL) {
                IR_parent(kid) = this;
                if (recur) {
                    kid->setParentPointer(recur);
                }
                kid = IR_next(kid);
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
        p = IR_next(p);
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
    switch (IR_code(this)) {
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
        return SETELEM_rhs(this)->getOpndPR(prno);
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
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
        if ((pr = BIN_opnd0(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return BIN_opnd1(this)->getOpndPR(prno);
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
        return UNA_opnd0(this)->getOpndPR(prno);
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
        if ((pr = BIN_opnd0(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return BIN_opnd1(this)->getOpndPR(prno);
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
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        if ((pr = BIN_opnd0(this)->getOpndPR(prno)) != NULL) {
            return pr;
        }
        return BIN_opnd1(this)->getOpndPR(prno);
    case IR_CVT:
        return CVT_exp(this)->getOpndPR(prno);
    case IR_PR:
        return PR_no(this) == prno ? this : NULL;
    case IR_TRUEBR:
    case IR_FALSEBR:
        return BR_det(this)->getOpndPR(prno);
    case IR_SELECT:
        if ((pr = SELECT_det(this)->getOpndPR(prno)) != NULL) {
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
    default: ASSERT0(0);
    }
    return NULL;
}


//Get the Stmt accroding to given prno.
//The stmt must write to PR as a result.
IR * IR::getResultPR(UINT prno)
{
    switch (IR_code(this)) {
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
    case IR_CONST:
    case IR_ID:
    case IR_LD:
    case IR_ILD:
    case IR_LDA:
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_CASE:
    case IR_ARRAY:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_CVT:
    case IR_PR:
    case IR_SELECT:
    case IR_RETURN:    //Does not have a result PR.
        ASSERT0(0);
    case IR_PHI:
        if (PHI_prno(this) == prno) { return this; }
        return NULL;
    default: ASSERT0(0);
    }
    return NULL;
}


//Copy MD that ir referenced accroding to 'mds'.
void IR::set_ref_md(MD const* md, Region * ru)
{
    DU * du = get_du();
    if (du == NULL) {
        if (md == NULL) { return; }

        ASSERT0(ru);
        du = ru->allocDU();
        set_du(du);
    }
    DU_md(du) = md;
}


//Copy the set of MD that ir referenced accroding to 'mds'.
void IR::set_ref_mds(MDSet const* mds, Region * ru)
{
    DU * du = get_du();
    if (du == NULL) {
        if (mds == NULL) { return; }

        ASSERT0(ru);
        du = ru->allocDU();
        set_du(du);
    }
    DU_mds(du) = mds;
}


void IR::invertLand(Region * ru)
{
    ASSERT0(ru);
    //a&&b => !a || !b
    IR * newop0 = ru->buildLogicalNot(BIN_opnd0(this));
    IR * newop1 = ru->buildLogicalNot(BIN_opnd1(this));
    IR_code(this) = IR_LOR;

    BIN_opnd0(this) = newop0;
    BIN_opnd1(this) = newop1;
    IR_parent(newop0) = this;
    IR_parent(newop1) = this;
}


void IR::invertLor(Region * ru)
{
    ASSERT0(ru);
    //a||b => !a && !b
    IR * newop0 = ru->buildLogicalNot(BIN_opnd0(this));
    IR * newop1 = ru->buildLogicalNot(BIN_opnd1(this));
    IR_code(this) = IR_LAND;

    BIN_opnd0(this) = newop0;
    BIN_opnd1(this) = newop1;
    IR_parent(newop0) = this;
    IR_parent(newop1) = this;
}


static void removeSSAUseRecur(IR * ir)
{
    if (ir->is_stmt()) {
        SSAInfo * ssainfo = ir->get_ssainfo();
        if (ssainfo != NULL) {
            ssainfo->cleanDU();
        }
    }  else {
        SSAInfo * ssainfo = ir->get_ssainfo();
        if (ssainfo != NULL) {
            SSA_uses(ssainfo).remove(ir);
        }
    }

    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->get_kid(i); x != NULL; x = IR_next(x)) {
            if (x->is_pr()) {
                SSAInfo * ssainfo = x->get_ssainfo();
                if (ssainfo != NULL) {
                    SSA_uses(ssainfo).remove(x);
                }
            } else {
                ASSERT0(!x->is_read_pr());
            }

            if (!x->is_leaf()) {
                removeSSAUseRecur(x);
            }
        }
    }
}


/* Remove SSA use-def chain.
e.g: pr1=...
    =pr1 //S1
If S1 will be deleted, pr1 should be removed from its SSA_uses. */
void IR::removeSSAUse()
{
    removeSSAUseRecur(this);
}
//END IR

} //namespace xoc
