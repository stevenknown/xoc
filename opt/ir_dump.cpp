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

//Dump IR list with a logging header-notation.
//Dump both its kids and siblings.
void dumpIRListH(IR const* ir_list, Region const* rg, CHAR * attr,
                 DumpFlag dumpflag)
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==---- DUMP IR List ----==");
    dumpIRList(ir_list, rg, attr, dumpflag);
}


//Dump IR, and both its kids and siblings.
void dumpIRList(IR const* ir_list, Region const* rg, CHAR * attr,
                DumpFlag dumpflag)
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


static void dumpOffset(IR const* ir, Region const* rg)
{
    if (ir->getOffset() != 0) {
        prt(rg, ":offset(%d)", ir->getOffset());
    }
}


static void dumpIdinfo(IR const* ir, Region const* rg)
{
    if (!ir->hasIdinfo()) { return; }
    CHAR tt[40];
    tt[0] = 0;
    CHAR * name = xstrcat(tt, NUM_ARRELEM(tt), "%s",
                          ir->getIdinfo()->get_name()->getStr());
    prt(rg, " '%s'", name);
}


static void dumpAbstractIdinfo(IR const* ir, Region const* rg)
{
    CHAR tt[44];
    tt[0] = 0;
    CHAR const* string = ir->getIdinfo()->get_name()->getStr();
    CHAR * name = xstrcat(tt, NUM_ARRELEM(tt) - 4, "%s", string);
    if (::strlen(string) > (NUM_ARRELEM(tt) - 4)) {
        strcat(tt, "...");
    }
    prt(rg, " '%s'", name);
}


static void dumpAllKids(IR const* ir, Region const* rg, UINT dn,
                        DumpFlag dumpflag)
{
    LogMgr * lm = rg->getLogMgr();
    lm->incIndent(dn);
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k == nullptr) { continue; }
        dumpIRList(k, rg, nullptr, dumpflag);
    }
    lm->decIndent(dn);
}


static void dumpVarDecl(IR const* ir, Region const* rg)
{
    if (!ir->hasIdinfo()) { return; }
    ASSERT0(ir->getIdinfo());
    StrBuf buf(64);
    if (ir->getIdinfo()->dumpVARDecl(buf, rg->getVarMgr()) != nullptr) {
        prt(rg, " decl:%s", buf.buf);
    }
}


static void dumpLabelType(LabelInfo const* li, RegionMgr const* rm)
{
    if (li->is_ilabel() || li->is_clabel()) {
        prt(rm, "label ");
        return;
    }
    if (li->is_pragma()) {
        prt(rm, "pragma ");
        return;
    }
    ASSERTN(0, ("unknown label type"));

}


void dumpLabelName(LabelInfo const* li, RegionMgr const* rm, bool for_gr)
{
    if (li->is_ilabel()) {
        bool non_id = false;
        if (for_gr) {
            StrBuf buf(8);
            buf.strcat(ILABEL_STR_FORMAT, ILABEL_CONT(li));
            if (isContainNonIdentifierChar(buf.buf)) {
                //Some non-identifier character will be recognized as individual
                //token.
                //prt(rm, "\"" ILABEL_STR_FORMAT "\"", ILABEL_CONT(li));
                non_id = true;
            } else {
                //prt(rm, ILABEL_STR_FORMAT, ILABEL_CONT(li));
            }
        } else {
            //prt(rm, ILABEL_STR_FORMAT, ILABEL_CONT(li));
        }

        if (non_id) { prt(rm, "\""); }
        if (for_gr) {
            prt(rm, "%s", PREFIX_OF_ILABEL_IN_GR);
        }
        prt(rm, ILABEL_STR_FORMAT, ILABEL_CONT(li));
        if (non_id) { prt(rm, "\""); }
        return;
    }
    if (li->is_clabel()) {
        bool non_id = false;
        if (for_gr) {
            StrBuf buf(8);
            buf.strcat(CLABEL_STR_FORMAT, CLABEL_CONT(li));
            if (isContainNonIdentifierChar(buf.buf)) {
                //Some non-identifier character will be recognized as individual
                //token.
                //prt(rm, "\"" ILABEL_STR_FORMAT "\"", ILABEL_CONT(li));
                non_id = true;
            } else {
                //prt(rm, ILABEL_STR_FORMAT, ILABEL_CONT(li));
            }
        } else {
            //prt(rm, ILABEL_STR_FORMAT, ILABEL_CONT(li));
        }

        if (non_id) { prt(rm, "\""); }
        prt(rm, CLABEL_STR_FORMAT, CLABEL_CONT(li));
        if (non_id) { prt(rm, "\""); }
        return;
    }
    if (li->is_pragma()) {
        ASSERT0(li->getPragma());
        bool non_id = false;
        if (for_gr) {
            StrBuf buf(8);
            if (isContainNonIdentifierChar(li->getPragma()->getStr())) {
                //Some non-identifier character will be recognized as individual
                //token.
                non_id = true;
            }
        }
        if (non_id) { prt(rm, "\""); }
        prt(rm, "%s", li->getPragma()->getStr());
        if (non_id) { prt(rm, "\""); }
        return;
    }
    ASSERTN(0, ("unknown label type"));
}


void dumpLabelDecl(LabelInfo const* li, RegionMgr const* rm, bool for_gr)
{
    dumpLabelType(li, rm);
    dumpLabelName(li, rm, for_gr);
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


static void dumpAttr(OUT xcom::StrBuf & buf, IR const* ir)
{
    if (g_dump_opt.isDumpIRID()) {
        buf.strcat(" id:%d", ir->id());
    }
    if (ir->isMayThrow(false)) {
        buf.strcat(" throw");
    }
    if (ir->is_terminate()) {
        buf.strcat(" terminate");
    }
    if (ir->is_atomic()) {
        buf.strcat(" atom");
    }
    if (ir->is_rmw()) {
        buf.strcat(" rmw");
    }
    if (ir->hasSideEffect(false)) {
        buf.strcat(" sideeffect");
    }
    if (ir->isNoMove(false)) {
        buf.strcat(" nomove");
    }
    if (ir->isReadOnly()) {
        buf.strcat(" readonly");
    }
    if (ir->is_volatile()) {
        buf.strcat(" volatile");
    }
}


static void dumpAttachInfo(OUT xcom::StrBuf & buf, IR const* ir)
{
    ASSERT0(ir);
    AIContainer const* ai = ir->getAI();
    if (ai == nullptr) { return; }
    AICont const* cont = ai->getContainer();
    if (!cont->is_init()) { return; }
    buf.strcat(" attachinfo:");
    bool not_first = false;
    for (UINT i = 0; i < cont->get_capacity(); i++) {
        BaseAttachInfo const* ac = cont->get(i);
        if (ac == nullptr) { continue; }
        if (!not_first) {
            not_first = true;
        } else {
            buf.strcat(",");
        }
        buf.strcat("%s", ai->getAIName(ac->getType()));
    }
}


void dumpREGION(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_inner_region = ctx.dumpflag.have(IR_DUMP_INNER_REGION);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "%s", IRNAME(ir));
    if (REGION_ru(ir)->getRegionVar() != nullptr) {
        Var * var = REGION_ru(ir)->getRegionVar();
        CHAR tt[40];
        tt[0] = 0;

        //Dump variable info.
        xstrcat(tt, NUM_ARRELEM(tt), "%s", var->get_name()->getStr());
        prt(rg, " \'%s\',id:%d", tt, REGION_ru(ir)->id());
    }
    DUMPADDR(ir); //Dump IR address.
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr); //Dump attributes.
    if (dump_inner_region) {
        //Inner region.
        ASSERT0(REGION_ru(ir));
        lm->incIndent(ctx.dn);
        note(rg, "\nregion-info:");
        REGION_ru(ir)->dump(dump_inner_region);
        lm->decIndent(ctx.dn);
    }
}


void dumpARRAY(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();

    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    dumpOffset(ir, rg);
    buf.clean();
    prt(rg, " (elemtype:%s)", xtm->dump_type(ARR_elemtype(ir), buf));

    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (ARR_sub_list(ir) != nullptr && dump_kid) {
        //Dump element number if it exist.
        lm->incIndent(ctx.dn);

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
            dumpIR(sub, rg, (CHAR*)tt, ctx.dumpflag);
            dim++;
        }
        lm->decIndent(ctx.dn);
    }

    if (!dump_kid) { return; }

    lm->incIndent(ctx.dn);
    dumpIRList(ARR_base(ir), rg, (CHAR*)" array_base", ctx.dumpflag);
    lm->decIndent(ctx.dn);
}


void dumpCASE(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    LogMgr * lm = rg->getLogMgr();

    ASSERT0(CASE_vexp(ir));
    ASSERT0(CASE_lab(ir));
    note(rg, "case");
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);

    lm->incIndent(ctx.dn);
    dumpIRList(CASE_vexp(ir), rg, nullptr, ctx.dumpflag);
    note(rg, "\n");
    dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    lm->decIndent(ctx.dn);
}


void dumpSWITCH(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();

    note(rg, "switch");
    if (SWITCH_deflab(ir) != nullptr) {
        prt(rg, ", deflab: ");
        dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    }
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (!dump_kid) { return; }

    lm->incIndent(ctx.dn);
    dumpIRList(SWITCH_vexp(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    if (SWITCH_case_list(ir) != nullptr) {
        dumpIRList(SWITCH_case_list(ir), rg, nullptr, ctx.dumpflag);
    }

    if (SWITCH_body(ir) != nullptr) {
        note(rg, "\nbody:");
        lm->incIndent(ctx.dn);
        dumpIRList(SWITCH_body(ir), rg, nullptr, ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
    note(rg, "\nend_switch");
}


void dumpPHI(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
    note(rg, "%s %s%d:%s", IRNAME(ir), PR_TYPE_CHAR, PHI_prno(ir),
         xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (!dump_kid) { return; }

    prt(rg, " = ", ctx.attr);
    lm->incIndent(ctx.dn);
    IR * opnd = PHI_opnd_list(ir);
    while (opnd != nullptr) {
        dumpIR(opnd, rg, nullptr, ctx.dumpflag);
        opnd = opnd->get_next();
    }
    lm->decIndent(ctx.dn);
}


void dumpSELECT(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        LogMgr * lm = rg->getLogMgr();
        lm->incIndent(ctx.dn);
        dumpIRList(SELECT_det(ir), rg, nullptr, ctx.dumpflag);
        lm->decIndent(ctx.dn);

        lm->incIndent(ctx.dn);
        dumpIRList(SELECT_trueexp(ir), rg, (CHAR*)" true_exp", ctx.dumpflag);
        lm->decIndent(ctx.dn);

        lm->incIndent(ctx.dn);
        dumpIRList(SELECT_falseexp(ir), rg, (CHAR*)" false_exp", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}


void dumpLABEL(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    LabelInfo const* li = LAB_lab(ir);
    if (LABELINFO_type(li) == L_ILABEL) {
        note(rg, "label " ILABEL_STR_FORMAT "",
             ILABEL_CONT(LAB_lab(ir)));
    } else if (LABELINFO_type(li) == L_CLABEL) {
        note(rg, "label " CLABEL_STR_FORMAT "",
             CLABEL_CONT(LAB_lab(ir)));
    } else if (LABELINFO_type(li) == L_PRAGMA) {
        ASSERT0(LABELINFO_pragma(LAB_lab(ir)));
        note(rg, "pragma %s", SYM_name(LABELINFO_pragma(LAB_lab(ir))));
    } else { UNREACHABLE(); }

    DUMPADDR(ir); //dump runtime address on host machine.

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

    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
}


void dumpDOLOOP(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "doloop");
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (!dump_kid) { return; }

    note(rg, "\niv:");
    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_iv(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\ninit:");
    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_init(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\ndet:");
    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_det(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\nstep:");
    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_step(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\nbody:");
    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_body(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\nend_doloop");
}


void dumpWHILEDO(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "whiledo");
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (!dump_kid) { return; }

    note(rg, "\ndet:");
    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_det(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\nbody:");

    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_body(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\nend_whiledo");
}


void dumpDOWHILE(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "dowhile");
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (!dump_kid) { return; }

    note(rg, "\nbody:");
    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_body(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\ndet:");
    lm->incIndent(ctx.dn);
    dumpIRList(LOOP_det(ir), rg, nullptr, ctx.dumpflag);
    lm->decIndent(ctx.dn);

    note(rg, "\nend_dowhile");
}


void dumpIF(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "if");
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        lm->incIndent(ctx.dn);
        dumpIRList(IF_det(ir), rg, nullptr, ctx.dumpflag);
        lm->decIndent(ctx.dn);

        note(rg, "\n{");
        lm->incIndent(ctx.dn);
        dumpIRList(IF_truebody(ir), rg, nullptr, ctx.dumpflag);
        lm->decIndent(ctx.dn);
        note(rg, "\n}");

        if (IF_falsebody(ir)) {
            note(rg, "\nelse");
            note(rg, "\n{");
            lm->incIndent(ctx.dn);
            dumpIRList(IF_falsebody(ir), rg, nullptr, ctx.dumpflag);
            lm->decIndent(ctx.dn);
            note(rg, "\n}");
        }
    }
}


void dumpBinAndUna(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    if (ir->is_cvt() && CVT_round(ir) != ROUND_UNDEF) {
        prt(rg, ":round(%s)", ROUND_NAME(CVT_round(ir)));
    }
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx.dn, ctx.dumpflag);
    }
}


void dumpReadPR(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s%d:%s", PR_TYPE_CHAR, ir->getPrno(), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
}


void dumpGeneral(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    bool dump_var_decl = ctx.dumpflag.have(IR_DUMP_VAR_DECL);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    dumpOffset(ir, rg);
    dumpIdinfo(ir, rg);
    if (dump_var_decl) {
        dumpVarDecl(ir, rg);
    }
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx.dn, ctx.dumpflag);
    }
}


void dumpGeneralNoType(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    bool dump_var_decl = ctx.dumpflag.have(IR_DUMP_VAR_DECL);
    note(rg, "%s", IRNAME(ir));
    dumpOffset(ir, rg);
    dumpIdinfo(ir, rg);
    if (dump_var_decl) {
        dumpVarDecl(ir, rg);
    }
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx.dn, ctx.dumpflag);
    }
}

void dumpBranch(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    note(rg, "%s ", IRNAME(ir));
    ASSERT0(ir->getLabel());
    dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx.dn, ctx.dumpflag);
    }
}


void dumpUNDEF(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    note(rg, "%s!", IRNAME(ir));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
}


void dumpSTARRAY(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();

    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    dumpOffset(ir, rg);
    buf.clean();
    prt(rg, " (elemtype:%s)", xtm->dump_type(ARR_elemtype(ir), buf));

    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (ARR_sub_list(ir) != nullptr && dump_kid) {
        //Dump elem number.
        lm->incIndent(ctx.dn);
        UINT dim = 0;
        if (ARR_elem_num_buf(ir) != nullptr) {
            note(rg, "\nelem_num[");
            for (IR const* sub = ARR_sub_list(ir); sub != nullptr;) {
                prt(rg, "%d", ((CArray*)ir)->getElementNumOfDim(dim));
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
            dumpIR(sub, rg, (CHAR*)tt, ctx.dumpflag);
            dim++;
        }
        lm->decIndent(ctx.dn);
    }
    if (dump_kid) {
        lm->incIndent(ctx.dn);
        dumpIRList(ARR_base(ir), rg, (CHAR*)" array_base", ctx.dumpflag);
        dumpIRList(STARR_rhs(ir), rg, (CHAR*)" rhs", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}


void dumpWritePR(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s %s%d:%s", IRNAME(ir), PR_TYPE_CHAR, ir->getPrno(),
         xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx.dn, ctx.dumpflag);
    }
}


void dumpCallStmt(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    bool dump_var_decl = ctx.dumpflag.have(IR_DUMP_VAR_DECL);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
    if (ir->hasReturnValue()) {
        note(rg, "%s%d:%s = ", PR_TYPE_CHAR, CALL_prno(ir),
             xtm->dump_type(d, buf));
    } else {
        note(rg, ""); //just print indent.
    }
    prt(rg, "%s", IRNAME(ir));
    if (ir->is_call()) {
        dumpAbstractIdinfo(ir, rg);
        buf.clean();
        if (dump_var_decl) {
            dumpVarDecl(ir, rg);
        }
    }

    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, " %s", ctx.attr);
    if (!dump_kid) { return; }

    if (ir->is_icall()) {
        lm->incIndent(ctx.dn);
        dumpIR(ICALL_callee(ir), rg, (CHAR*)" callee", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }

    CHAR tmpbuf[30];
    UINT i = 0;

    //Dump parameter list.
    for (IR * p2 = CALL_param_list(ir);
         p2 != nullptr; p2 = p2->get_next()) {
        sprintf(tmpbuf, " param%d", i);
        lm->incIndent(ctx.dn);
        dumpIR(p2, rg, tmpbuf, ctx.dumpflag);
        lm->decIndent(ctx.dn);
        i++;
    }

    //Dump dummy use.
    i = 0;
    for (IR * p2 = CALL_dummyuse(ir);
         p2 != nullptr; p2 = p2->get_next()) {
        sprintf(tmpbuf, " dummy%d", i);
        lm->incIndent(ctx.dn);
        dumpIR(p2, rg, tmpbuf, ctx.dumpflag);
        lm->decIndent(ctx.dn);
        i++;
    }
}


void dumpRETURN(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    note(rg, "%s", IRNAME(ir));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx.dn, ctx.dumpflag);
    }
}


void dumpConst(IR const* ir, Region const* rg, IRDumpCtx & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    note(rg, "");
    dumpConstContent(ir, rg);
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
}


void dumpConstContent(IR const* ir, Region const* rg)
{
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    if (ir->is_sint()) {
        CHAR const* intfmt = getHostIntFormat(false);
        CHAR const* hexintfmt = getHostIntFormat(true);
        StrBuf fmt(16);
        fmt.strcat("intconst:%%s %s|0x%s", intfmt, hexintfmt);
        prt(rg, fmt.buf, xtm->dump_type(d, buf),
            CONST_int_val(ir), CONST_int_val(ir));
        return;
    }

    if (ir->is_uint()) {
        CHAR const* intfmt = getHostUIntFormat(false);
        CHAR const* hexintfmt = getHostUIntFormat(true);
        StrBuf fmt(16);
        fmt.strcat("intconst:%%s %s|0x%s", intfmt, hexintfmt);
        prt(rg, fmt.buf, xtm->dump_type(d, buf), CONST_int_val(ir),
            CONST_int_val(ir));
        return;
    }

    if (ir->is_fp()) {
        CHAR fpformat[128];
        ::snprintf(fpformat, 127, "fpconst:%%s %%.%df", CONST_fp_mant(ir));
        prt(rg, fpformat, xtm->dump_type(d, buf), CONST_fp_val(ir));
        return;
    }

    if (ir->is_bool()) {
        prt(rg, "boolconst:%s %d", xtm->dump_type(d, buf), CONST_int_val(ir));
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
        CHAR const* intfmt = getHostUIntFormat(false);
        CHAR const* hexintfmt = getHostUIntFormat(true);
        StrBuf fmt(16);
        fmt.strcat("intconst:%%s %s|0x%s", intfmt, hexintfmt);
        prt(rg, fmt.buf, xtm->dump_type(d, buf),
            CONST_int_val(ir), CONST_int_val(ir));
        return;
    }

    //Dump as HOST_INT type even if it is unrecognized,
    //leave the sanity check to verify().
    //Note the dump format may extend or truncate the real value.
    prt(rg, "intconst:%s %d|0x%x", xtm->dump_type(d, buf),
        CONST_int_val(ir),  CONST_int_val(ir));
}


CHAR const* dumpIRName(IR const* ir, MOD StrBuf & buf)
{
    buf.sprint("%s", IRNAME(ir));
    if (g_dump_opt.isDumpIRID()) {
        buf.strcat("(id:%u)", ir->id());
    }
    return buf.getBuf();
}


void dumpIRCombine(IR const* ir, Region const* rg)
{
    dumpIR(ir, rg, nullptr, DumpFlag(IR_DUMP_COMBINE));
}


//Dump IR and all of its kids.
//'attr': miscellaneous string which following 'ir'.
void dumpIR(IR const* ir, Region const* rg, CHAR const* attr, DumpFlag dumpflag)
{
    bool dump_src_line = dumpflag.have(IR_DUMP_SRC_LINE);
    bool dump_newline = !dumpflag.have(IR_DUMP_NO_NEWLINE);
    LogMgr * lm = rg->getLogMgr();
    UINT dn = 4;
    if (!rg->isLogMgrInit() || ir == nullptr) { return; }
    xcom::StrBuf lattr(32);
    if (attr != nullptr) {
        lattr.strcat(attr);
    }
    dumpAttr(lattr, ir);
    dumpAttachInfo(lattr, ir);

    //Record type info and var decl.
    if (g_dbx_mgr != nullptr && dump_src_line) {
        DbxMgr::PrtCtx prtctx;
        prtctx.logmgr = lm;
        g_dbx_mgr->printSrcLine(ir, &prtctx);
    }
    if (dump_newline) {
        //Dump newline before root ir.
        prt(rg, "\n");
    }
    IRDumpFuncType dumpfunc = IRDES_dumpfunc(g_ir_desc[ir->getCode()]);
    ASSERT0(dumpfunc);
    IRDumpCtx ctx;
    ctx.dn = dn;
    ctx.dumpflag = dumpflag;
    ctx.attr = lattr.getBuf();
    (*dumpfunc)(ir, rg, ctx);
}


void dumpIRToBuf(IR const* ir, Region const* rg, OUT StrBuf & outbuf,
                 DumpFlag dumpflag)
{
    ASSERT0(rg && ir);
    LogCtx tctx; //Define a tmp logctx.

    //Copy current LogCtx info except the dump buffer, because the tmp ctx
    //will open a new dump buffer to redirect the dumping.
    tctx.copyWithOutBuffer(rg->getLogMgr()->getCurrentCtx());

    //Push current LogCtx and enable tmp ctx as the present one.
    rg->getLogMgr()->push(tctx);

    //Open a new buffer for the tmp ctx.
    rg->getLogMgr()->startBuffer();

    //Dump IR info to dump buffer of the tmp ctx.
    dumpIR(ir, rg, nullptr, dumpflag);

    //Get the dump buffer.
    xcom::StrBuf const* tmplogbuf = rg->getLogMgr()->getBuffer();
    ASSERT0(tmplogbuf);

    //Append the string in dump buffer into 'outbuf'.
    outbuf.strcat(tmplogbuf->getBuf());

    //Close the dump buffer.
    rg->getLogMgr()->endBuffer(false);

    //Pop the tmp ctx and restore original LogCtx.
    rg->getLogMgr()->pop();
}

} //namespace xoc
