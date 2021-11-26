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

static bool hasProp(IR const* ir)
{
    return ir->isMayThrow() || ir->is_terminate() || ir->is_atomic() ||
           ir->is_rmw() || ir->hasSideEffect() || ir->isNoMove();
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
                    dumpLabelName(sc->val(), rm, true);
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
    if (isContainNonIdentifierChar(n->getStr())) {
        buf.sprint("\"%s\"", n->getStr());
        return buf.buf;
    }
    return n->getStr();
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
        dumpLabelName(ir->getLabel(), rm, true);
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
    case IR_LABEL: {
            LabelInfo const* li = LAB_lab(ir);
            note(lm, "\n");
            dumpLabelDecl(li, rm, true);
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
        ASSERT0(ctx && ctx->cfg && ir->getBB());
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
                dumpLabelName(lab, rm, true);
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
            dumpLabelName(ir->getLabel(), rm, true);
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
        dumpLabelName(ir->getLabel(), rm, true);
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
        dumpLabelName(ir->getLabel(), rm, true);
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
            dumpLabelDecl(li, rm, true);
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

} //namespace xoc
