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
    return ir->isMayThrow(false) || ir->is_terminate() || ir->is_atomic() ||
           ir->is_rmw() || ir->hasSideEffect(false) || ir->isNoMove(false);
}

GRDump::GRDump(Region const* rg) : m_rg(rg)
{
    ASSERT0(rg);
    m_tm = rg->getTypeMgr();
    m_cfg = rg->getCFG();
    m_lm = rg->getLogMgr();
    m_rm = rg->getRegionMgr();
}


void GRDump::dumpOffset(IR const* ir) const
{
    if (hasProp(ir) ||
        ir->getOffset() != 0 ||
        ((ir->is_array() || ir->is_starray()) &&
         ARR_elem_num_buf(ir) != nullptr)) {
        prt(m_lm, ":%d", ir->getOffset());
    }
}


void GRDump::dumpProp(IR const* ir, DumpGRCtx const* ctx) const
{
    bool first = true;
    if (ir->isMayThrow(false)) {
        if (!first) { prt(m_lm, ","); }
        else { prt(m_lm, ":("); }
        prt(m_lm, "throw(");
        if (ir->getAI() != nullptr) {
            EHLabelAttachInfo const* ehlab =
                (EHLabelAttachInfo const*)ir->getAI()->get(AI_EH_LABEL);
            if (ehlab != nullptr) {
                xcom::SList<LabelInfo*> const& labs = ehlab->read_labels();
                for (xcom::SC<LabelInfo*> * sc = labs.get_head();
                     sc != labs.end(); sc = labs.get_next(sc)) {
                    if (sc != labs.get_head()) {
                        prt(m_lm, ",");
                    }
                    dumpLabelName(sc->val(), m_rg->getRegionMgr(), true);
                }
            }
        }
        prt(m_lm, ")");
        first = false;
    }
    if (ir->is_array() || ir->is_starray()) {
        ASSERT0(ARR_elemtype(ir));
        if (ARR_elemtype(ir) != ir->getType() ||
            ARR_elem_num_buf(ir) != nullptr) {
            if (!first) { prt(m_lm, ","); }
            else { prt(m_lm, ":("); }
            first = false;
        }
        bool prt_elemtype = false;
        if (ARR_elemtype(ir) != ir->getType()) {
            xcom::StrBuf ety(16);
            m_tm->dump_type(ARR_elemtype(ir), ety);
            prt(m_lm, "elemtype:%s", ety.buf);
            prt_elemtype = true;
        }
        if (ARR_elem_num_buf(ir) != nullptr) {
            if (prt_elemtype) {
                prt(m_lm, ",");
            }
            prt(m_lm, "dim");
            UINT dim = 0;
            prt(m_lm, "[");
            for (IR const* sub = ARR_sub_list(ir); sub != nullptr;) {
                prt(m_lm, "%d", ((CStArray*)ir)->getElementNumOfDim(dim));
                sub = sub->get_next();
                if (sub != nullptr) {
                    prt(m_lm, ",");
                }
                dim++;
            }
            prt(m_lm, "]");
        }
    }
    if (ir->is_terminate()) {
        if (!first) { prt(m_lm, ","); }
        else { prt(m_lm, ":("); }
        prt(m_lm, "terminate");
        first = false;
    }
    if (ir->is_atomic()) {
        if (!first) { prt(m_lm, ","); }
        else { prt(m_lm, ":("); }
        prt(m_lm, "atom");
        first = false;
    }
    if (ir->is_rmw()) {
        if (!first) { prt(m_lm, ","); }
        else { prt(m_lm, ":("); }
        prt(m_lm, "rmw");
        first = false;
    }
    if (ir->hasSideEffect(false)) {
        if (!first) { prt(m_lm, ","); }
        else { prt(m_lm, ":("); }
        prt(m_lm, "sideeffect");
        first = false;
    }
    if (ir->isNoMove(false)) {
        if (!first) { prt(m_lm, ","); }
        else { prt(m_lm, ":("); }
        prt(m_lm, "nomove");
        first = false;
    }
    if (ir->isCallStmt() && CALL_dummyuse(ir) != nullptr) {
        if (!first) { prt(m_lm, ","); }
        else { prt(m_lm, ":("); }
        prt(m_lm, "use(");
        m_lm->incIndent(DUMP_INDENT_NUM);
        for (IR * p = CALL_dummyuse(ir); p != nullptr; p = p->get_next()) {
            if (p != CALL_dummyuse(ir)) {
                prt(m_lm, ",");
            }
            dumpIR(p, ctx);
        }
        prt(m_lm, ")");
        first = false;
        m_lm->decIndent(DUMP_INDENT_NUM);
    }
    if (!first) { prt(m_lm, ")"); }
}


CHAR const* GRDump::compositeName(Sym const* n, xcom::StrBuf & buf)
{
    if (xoc::isContainNonIdentifierChar(n->getStr())) {
        buf.sprint("\"%s\"", n->getStr());
        return buf.buf;
    }
    return n->getStr();
}


void GRDump::dumpArrSubList(IR const* ir, UINT dn, DumpGRCtx const* ctx) const
{
    if (ARR_sub_list(ir) == nullptr) { return; }
    m_lm->incIndent(dn);
    prt(m_lm, "(");
    for (IR * s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
        if (s != ARR_sub_list(ir)) {
            prt(m_lm, ",");
        }
        dumpIR(s, ctx);
    }
    prt(m_lm, ")");
    m_lm->decIndent(dn);
}


void GRDump::dumpConst(IR const* ir, DumpGRCtx const* ctx) const
{
    Type const* d = ir->getType();
    StrBuf buf(64);
    UINT dn = DUMP_INDENT_NUM;
    ASSERT0(ctx);
    if (ir->is_sint()) {
        //WORKAROUND:GR parser has bug in parsing large negative number.
        //CHAR const* intfmt = getHostIntFormat(true);
        //Prefer print imm according to its type.
        StrBuf fmt(16);
        fmt.strcat("%s:%%s", getHostIntFormat(false));
        m_lm->incIndent(dn);
        prt(m_lm, fmt.buf, CONST_int_val(ir), m_tm->dump_type(d, buf));
        m_lm->decIndent(dn);
        return;
    }

    if (ir->is_uint()) {
        //WORKAROUND:GR parser has bug in parsing large negative number.
        //CHAR const* intfmt = getHostIntFormat(true);
        //Prefer print imm according to its type.
        StrBuf fmt(16);
        fmt.strcat("%s:%%s", getHostUIntFormat(false));
        m_lm->incIndent(dn);
        prt(m_lm, fmt.buf, CONST_int_val(ir), m_tm->dump_type(d, buf));
        m_lm->decIndent(dn);
        return;
    }

    if (ir->is_fp()) {
        CHAR fpformat[128];
        ::snprintf(fpformat, 127, "%%.%df:%%s ", CONST_fp_mant(ir));
        m_lm->incIndent(dn);
        prt(m_lm, fpformat, CONST_fp_val(ir), m_tm->dump_type(d, buf));
        m_lm->decIndent(dn);
        return;
    }

    if (ir->is_bool()) {
        m_lm->incIndent(dn);
        prt(m_lm, "%d:%s", (UINT)CONST_int_val(ir), m_tm->dump_type(d, buf));
        m_lm->decIndent(dn);
        return;
    }

    if (ir->is_str()) {
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
        m_lm->incIndent(dn);
        prt(m_lm, "\"%s\"", tbuf);
        if (tbuf != SYM_name(CONST_str_val(ir))) {
            ::free(tbuf);
        }
        m_lm->decIndent(dn);
        return;
    }

    if (ir->is_mc()) {
        //WORKAROUND:GR parser has bug in parsing large negative number.
        //CHAR const* intfmt = getHostIntFormat(true);
        //Prefer print imm according to its type.
        StrBuf fmt(16);
        fmt.strcat("%s:%%s", getHostUIntFormat(false));
        m_lm->incIndent(dn);
        prt(m_lm, fmt.buf, CONST_int_val(ir), m_tm->dump_type(d, buf));
        m_lm->decIndent(dn);
        return;
    }

    //Dump as HOST_INT type even if it is unrecognized,
    //leave the sanity check to verify().
    //Note the dump format may extend or truncate the real value.
    //Imm may be MC type.
    StrBuf fmt(16);
    fmt.strcat("%s:%%s", getHostUIntFormat(false));
    m_lm->incIndent(dn);
    prt(m_lm, fmt.buf, CONST_int_val(ir), m_tm->dump_type(d, buf));
    m_lm->decIndent(dn);
}


void GRDump::dumpPhi(IR const* ir, DumpGRCtx const* ctx) const
{
    TypeMgr * tm = const_cast<TypeMgr*>(m_tm);
    Type const* d = ir->getType();
    StrBuf buf(64);
    note(m_lm, "\n%s %s%d:%s = ", IRNAME(ir), PR_TYPE_CHAR, PHI_prno(ir),
         tm->dump_type(d, buf));
    UINT dn = DUMP_INDENT_NUM;
    m_lm->incIndent(dn);
    ASSERT0(ctx && ir->getBB());

    List<IRBB*> preds;
    m_cfg->get_preds(preds, ir->getBB());
    BBListIter bbct = nullptr;
    ASSERT0(preds.get_elem_count() ==
            xcom::cnt_list(PHI_opnd_list(ir)));
    preds.get_head(&bbct);
    for (IR * opnd = PHI_opnd_list(ir); opnd != nullptr;
         opnd = opnd->get_next(), bbct = preds.get_next(bbct)) {
        if (opnd != PHI_opnd_list(ir)) {
            prt(m_lm, ",");
        }
        xcom::C<LabelInfo const*> * lct;
        LabelInfo const* lab = bbct->val()->getLabelListConst().
                               get_head(&lct);
        ASSERTN(lab, ("each opnd of PHI has to correspond to"
                      " an unique label."));
        prt(m_lm, "(");
        dumpIR(opnd, ctx);
        prt(m_lm, ",");
        dumpLabelName(lab, m_rm, true);
        prt(m_lm, ")");
    }

    m_lm->decIndent(dn);
    prt(m_lm, ";");
}


void GRDump::dumpIR(IR const* ir, DumpGRCtx const* ctx) const
{
    UINT dn = DUMP_INDENT_NUM;
    if (!m_rg->isLogMgrInit() || ir == nullptr) { return; }
    StrBuf buf(64);
    StrBuf buf2(64);
    Type const* d = ir->getType();
    switch (ir->getCode()) {
    case IR_ST:
        note(m_lm, "\n%s:%s", IRNAME(ir), m_tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, ctx);
        buf.clean();
        prt(m_lm, " %s = ", compositeName(ST_idinfo(ir)->get_name(), buf));
        m_lm->incIndent(dn);
        dumpIR(ST_rhs(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ";");
        break;
    case IR_STPR:
        note(m_lm, "\n%s", IRNAME(ir));
        prt(m_lm, " %s%d:%s", PR_TYPE_CHAR, STPR_no(ir),
            m_tm->dump_type(d, buf));
        dumpProp(ir, ctx);
        prt(m_lm, " = ");
        m_lm->incIndent(dn);
        dumpIR(STPR_rhs(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ";");
        break;
    case IR_SETELEM:
        note(m_lm, "\n%s", IRNAME(ir));
        prt(m_lm, " %s%d:%s", PR_TYPE_CHAR, SETELEM_prno(ir),
            m_tm->dump_type(d, buf));
        dumpProp(ir, ctx);
        prt(m_lm, " = ");
        m_lm->incIndent(dn);
        dumpIR(SETELEM_base(ir), ctx);
        prt(m_lm, ",");
        dumpIR(SETELEM_val(ir), ctx);
        prt(m_lm, ",");
        dumpIR(SETELEM_ofst(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ";");
        break;
    case IR_GETELEM:
        note(m_lm, "\n%s", IRNAME(ir));
        prt(m_lm, " %s%d:%s", PR_TYPE_CHAR, GETELEM_prno(ir),
            m_tm->dump_type(d, buf));
        dumpProp(ir, ctx);
        prt(m_lm, " = ");
        m_lm->incIndent(dn);
        dumpIR(GETELEM_base(ir), ctx);
        prt(m_lm, ",");
        dumpIR(GETELEM_ofst(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ";");
        break;
    case IR_STARRAY:
        note(m_lm, "\n%s:%s", IRNAME(ir), m_tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, ctx);
        prt(m_lm, " = ");

        m_lm->incIndent(dn);
        dumpIR(ARR_base(ir), ctx);
        prt(m_lm, ", ");
        m_lm->decIndent(dn);

        dumpArrSubList(ir, dn, ctx);
        prt(m_lm, ", ");

        m_lm->incIndent(dn);
        dumpIR(STARR_rhs(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ";");
        break;
    case IR_IST:
        note(m_lm, "\n%s:%s", IRNAME(ir), m_tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, ctx);
        prt(m_lm, " = ");
        m_lm->incIndent(dn);
        dumpIRList(IST_base(ir), ctx);
        prt(m_lm, ",");
        dumpIRList(IST_rhs(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ";");
        break;
    case IR_LD:
        note(m_lm, "\n%s:%s", IRNAME(ir), m_tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, ctx);
        buf.clean();
        prt(m_lm, " %s", compositeName(LD_idinfo(ir)->get_name(), buf));
        break;
    case IR_ILD:
        note(m_lm, "\n%s:%s", IRNAME(ir), m_tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, ctx);
        prt(m_lm, " ");
        m_lm->incIndent(dn);
        dumpIRList(ILD_base(ir), ctx);
        m_lm->decIndent(dn);
        break;
    SWITCH_CASE_READ_PR:
        note(m_lm, "\n$%d:%s", PR_no(ir), m_tm->dump_type(d, buf));
        dumpProp(ir, ctx);
        break;
    case IR_ID:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpProp(ir, ctx);
        buf.clean();
        prt(m_lm, " %s", compositeName(ID_info(ir)->get_name(), buf));
        break;
    case IR_CONST:
        dumpConst(ir, ctx);
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        note(m_lm, "\n%s:%s", IRNAME(ir), m_tm->dump_type(d, buf));
        dumpProp(ir, ctx);
        prt(m_lm, " ");
        m_lm->incIndent(dn);
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == nullptr) { continue; }
            if (i != 0) {
                prt(m_lm, ", ");
            }
            dumpIRList(k, ctx);
        }
        m_lm->decIndent(dn);
        break;
    case IR_IF:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpProp(ir, ctx);
        prt(m_lm, " (");

        m_lm->incIndent(dn);
        dumpIRList(IF_det(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ")");

        note(m_lm, "\n{");
        m_lm->incIndent(dn);
        dumpIRList(IF_truebody(ir), ctx);
        m_lm->decIndent(dn);
        note(m_lm, "\n}");

        if (IF_falsebody(ir)) {
            note(m_lm, "\nelse");
            note(m_lm, "\n{");
            m_lm->incIndent(dn);
            dumpIRList(IF_falsebody(ir), ctx);
            m_lm->decIndent(dn);
            note(m_lm, "\n}");
        }
        prt(m_lm, ";");
        break;
    case IR_DO_WHILE:
        note(m_lm, "\ndo");
        dumpProp(ir, ctx);
        prt(m_lm, " {");

        m_lm->incIndent(dn);
        dumpIRList(LOOP_body(ir), ctx);
        m_lm->decIndent(dn);

        note(m_lm, "\n} while (");
        m_lm->incIndent(dn);
        dumpIRList(LOOP_det(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ");");
        break;
    case IR_WHILE_DO:
        note(m_lm, "\nwhile");
        dumpProp(ir, ctx);
        prt(m_lm, " (");

        m_lm->incIndent(dn);
        dumpIRList(LOOP_det(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ") {");

        m_lm->incIndent(dn);
        dumpIRList(LOOP_body(ir), ctx);
        m_lm->decIndent(dn);
        note(m_lm, "\n};");
        break;
    case IR_DO_LOOP:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpProp(ir, ctx);
        prt(m_lm, " (");

        m_lm->incIndent(dn);
        note(m_lm, "\n//iv");
        dumpIRList(LOOP_iv(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ",");

        m_lm->incIndent(dn);
        note(m_lm, "\n//init");
        dumpIRList(LOOP_init(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ",");

        m_lm->incIndent(dn);
        note(m_lm, "\n//det");
        dumpIRList(LOOP_det(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ",");

        m_lm->incIndent(dn);
        note(m_lm, "\n//step");
        dumpIRList(LOOP_step(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ")");

        note(m_lm, "\n {");
        m_lm->incIndent(dn);
        dumpIRList(LOOP_body(ir), ctx);
        m_lm->decIndent(dn);

        note(m_lm, "\n};");
        break;
    SWITCH_CASE_LOOP_ITER_CFS_OP:
        note(m_lm, "\n%s;", IRNAME(ir));
        break;
    case IR_RETURN:
        note(m_lm, "\n%s", IRNAME(ir));
        prt(m_lm, " ");
        m_lm->incIndent(dn);
        dumpIR(RET_exp(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, ";");
        break;
    case IR_GOTO:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpProp(ir, ctx);
        prt(m_lm, " ");
        dumpLabelName(ir->getLabel(), m_rm, true);
        prt(m_lm, ";");
        break;
    case IR_IGOTO:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpProp(ir, ctx);
        prt(m_lm, " (");

        m_lm->incIndent(dn);
        dumpIRList(IGOTO_vexp(ir), ctx);
        prt(m_lm, ")");
        m_lm->decIndent(dn);

        m_lm->incIndent(dn);
        for (IR * c = IGOTO_case_list(ir); c != nullptr; c = c->get_next()) {
            dumpIR(c, ctx);
            prt(m_lm, ", ");
        }
        m_lm->decIndent(dn);
        prt(m_lm, ";");
        break;
    case IR_LABEL: {
            LabelInfo const* li = LAB_lab(ir);
            note(m_lm, "\n");
            xoc::dumpLabelDecl(li, m_rm, true);
        }
        prt(m_lm, ";");
        break;
    case IR_SELECT: //formulized log_OR_exp?exp:cond_exp
        note(m_lm, "\nselect:%s", m_tm->dump_type(d, buf));
        dumpProp(ir, ctx);

        m_lm->incIndent(dn);
        dumpIRList(SELECT_det(ir), ctx);
        prt(m_lm, ",");
        m_lm->decIndent(dn);

        m_lm->incIndent(dn);
        dumpIRList(SELECT_trueexp(ir), ctx);
        prt(m_lm, ",");
        m_lm->decIndent(dn);

        m_lm->incIndent(dn);
        dumpIRList(SELECT_falseexp(ir), ctx);
        m_lm->decIndent(dn);
        break;
    case IR_LDA:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpOffset(ir);
        dumpProp(ir, ctx);
        buf.clean();
        prt(m_lm, " %s", compositeName(LDA_idinfo(ir)->get_name(), buf));
        break;
    case IR_PHI:
        dumpPhi(ir, ctx);
        break;
    case IR_SWITCH:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpProp(ir, ctx);
        prt(m_lm, " (");
        m_lm->incIndent(dn);
        dumpIRList(SWITCH_vexp(ir), ctx);
        prt(m_lm, ") ");
        m_lm->decIndent(dn);
        if (SWITCH_deflab(ir) != nullptr) {
            note(m_lm, "\ndefault ");
            dumpLabelName(ir->getLabel(), m_rm, true);
            prt(m_lm, ", ");
        }

        for (IR * c = SWITCH_case_list(ir); c != nullptr; c = c->get_next()) {
            dumpIR(c, ctx);
            prt(m_lm, ", ");
        }

        if (SWITCH_body(ir) != nullptr) {
            note(m_lm, "\n{ ");
            m_lm->incIndent(dn);
            dumpIRList(SWITCH_body(ir), ctx);
            m_lm->decIndent(dn);
            note(m_lm, "\n}");
        }
        prt(m_lm, ";");
        break;
    case IR_CASE:
        ASSERT0(CASE_vexp(ir));
        ASSERT0(CASE_lab(ir));
        note(m_lm, "\ncase");
        dumpProp(ir, ctx);
        prt(m_lm, " ");
        m_lm->incIndent(dn);
        dumpIRList(CASE_vexp(ir), ctx);
        prt(m_lm, ", ");
        dumpLabelName(ir->getLabel(), m_rm, true);
        m_lm->decIndent(dn);
        break;
    case IR_ARRAY:
        note(m_lm, "\n%s:%s", IRNAME(ir), m_tm->dump_type(d, buf));
        dumpOffset(ir);
        dumpProp(ir, ctx);

        m_lm->incIndent(dn);
        dumpIR(ARR_base(ir), ctx);
        prt(m_lm, ", ");
        m_lm->decIndent(dn);

        dumpArrSubList(ir, dn, ctx);
        break;
    SWITCH_CASE_CALL:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpProp(ir, ctx);
        prt(m_lm, " ");
        if (ir->hasReturnValue()) {
            prt(m_lm, "%s%d:%s = ", PR_TYPE_CHAR, CALL_prno(ir),
                m_tm->dump_type(d, buf));
        }
        if (ir->is_icall()) {
            m_lm->incIndent(dn);
            dumpIR(ICALL_callee(ir), ctx);
            prt(m_lm, ", ");
            m_lm->decIndent(dn);
        } else {
            buf.clean();
            prt(m_lm, "%s", compositeName(CALL_idinfo(ir)->get_name(), buf));
        }
        prt(m_lm, "(");
        m_lm->incIndent(dn);
        for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
            if (p != CALL_param_list(ir)) {
                prt(m_lm, ",");
            }
            dumpIR(p, ctx);
        }
        m_lm->decIndent(dn);
        prt(m_lm, ")");
        prt(m_lm, ";");
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        note(m_lm, "\n%s", IRNAME(ir));
        dumpProp(ir, ctx);
        prt(m_lm, " (");
        m_lm->incIndent(dn);
        dumpIRList(BR_det(ir), ctx);
        m_lm->decIndent(dn);
        prt(m_lm, "), ");
        dumpLabelName(ir->getLabel(), m_rm, true);
        prt(m_lm, ";");
        break;
    case IR_REGION:
        ASSERT0(REGION_ru(ir));
        if (ctx != nullptr && ctx->dump_inner_region) {
            //m_lm->incIndent(dn);
            GRDump gd(REGION_ru(ir));
            gd.dumpRegion(ctx->dump_inner_region);
            //m_lm->decIndent(dn);
        } else {
            note(m_lm, "\nregion ");
            switch (REGION_ru(ir)->getRegionType()) {
            case REGION_PROGRAM: prt(m_lm, "program "); break;
            case REGION_BLACKBOX: prt(m_lm, "blackbox "); break;
            case REGION_FUNC: prt(m_lm, "func "); break;
            case REGION_INNER: prt(m_lm, "inner "); break;
            default: UNREACHABLE(); //TODO
            }
            if (REGION_ru(ir)->getRegionVar() != nullptr) {
                prt(m_lm, "%s ",
                    SYM_name(REGION_ru(ir)->getRegionVar()->get_name()));
            }
        }
        prt(m_lm, ";");
        break;
    case IR_UNDEF:
        note(m_lm, "\nundef!");
        break;
    default:
        ASSERTN(0, ("unknown IR code:%s", IRNAME(ir)));
        return ;
    }
}


void GRDump::dumpIRList(IR const* irlist, DumpGRCtx const* ctx) const
{
    for (IR const* ir = irlist; ir != nullptr; ir = ir->get_next()) {
        dumpIR(ir, ctx);
    }
}


void GRDump::dumpBBList(BBList const* bblist, DumpGRCtx const* ctx) const
{
    ASSERT0(bblist);
    if (m_cfg != nullptr) {
        //CFG may be unavailable.
        PRSSAMgr::genLabForInputBBOfPhiOpnd(m_cfg);
    }
    BBListIter bbct = nullptr;
    for (bblist->get_head(&bbct); bbct != bblist->end();
         bbct = bblist->get_next(bbct)) {
        IRBB * bb = bbct->val();
        xcom::C<LabelInfo const*> * labct;
        for (bb->getLabelListConst().get_head(&labct);
             labct != bb->getLabelListConst().end();
             labct = bb->getLabelListConst().get_next(labct)) {
            LabelInfo const* li = labct->val();
            ASSERT0(li);
            note(m_rg, "\n");
            xoc::dumpLabelDecl(li, m_rm, true);
            prt(m_rg, ";");
        }
        IRListIter irct = nullptr;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            ASSERT0(ir);
            dumpIR(ir, ctx);
        }
    }
}


void GRDump::dumpRegion(bool dump_inner_region) const
{
    note(m_rg, "\n//==---- DUMP Region '%s' ----==", m_rg->getRegionName());
    note(m_rg, "\nregion ");
    switch (m_rg->getRegionType()) {
    case REGION_PROGRAM: prt(m_rg, "program "); break;
    case REGION_BLACKBOX: prt(m_rg, "blackbox "); break;
    case REGION_FUNC: prt(m_rg, "func "); break;
    case REGION_INNER: prt(m_rg, "inner "); break;
    default: UNREACHABLE(); //TODO
    }
    if (m_rg->getRegionVar() != nullptr) {
        xcom::StrBuf buf(32);
        prt(m_rg, "%s ", GRDump::compositeName(
                         m_rg->getRegionVar()->get_name(), buf));
    }
    prt(m_rg, "(");
    m_rg->dumpParameter();
    prt(m_rg, ")");
    prt(m_rg, " {\n");
    m_lm->incIndent(DUMP_INDENT_NUM);
    m_rg->dumpVarTab();
    if (!m_rg->is_blackbox()) {
        DumpGRCtx ctx;
        ctx.dump_inner_region = dump_inner_region;
        ctx.cfg = m_cfg;
        if (m_rg->getIRList() != nullptr) {
            dumpIRList(m_rg->getIRList(), &ctx);
        } else {
            dumpBBList(m_rg->getBBList(), &ctx);
        }
    }
    m_lm->decIndent(DUMP_INDENT_NUM);
    note(m_rg, "\n}");
}




} //namespace xoc
