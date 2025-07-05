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

void dumpIRListH(IR const* ir_list, Region const* rg, CHAR const* attr,
                 DumpFlag dumpflag)
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==---- DUMP IR List ----==");
    dumpIRList(ir_list, rg, attr, dumpflag);
}


void dumpIRList(
    CHAR const* filename, IR const* ir_list, Region const* rg,
    bool dump_inner_region, IRDumpCtx<> * ctx)
{
    ASSERT0(filename);
    FileObj fo(filename, true, false);
    FILE * h = fo.getFileHandler();
    rg->getLogMgr()->push(h, filename);
    dumpIRList(ir_list, rg, dump_inner_region, ctx);
    rg->getLogMgr()->pop();
}


void dumpIRList(
    IR const* ir_list, Region const* rg, bool dump_inner_region,
    IRDumpCtx<> * ctx)
{
    ASSERT0(rg && ir_list);
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "");
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID | IR_DUMP_SRC_LINE |
        (dump_inner_region ? IR_DUMP_INNER_REGION : 0));
    for (; ir_list != nullptr; ir_list = ir_list->get_next()) {
        if (ctx != nullptr) {
            xoc::dumpIR(ir_list, rg, *ctx);
            continue;
        }
        xoc::dumpIR(ir_list, rg, nullptr, f);
    }
}


void dumpIRList(IR const* ir_list, Region const* rg, IRDumpCtx<> & ctx)
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "");
    for (; ir_list != nullptr; ir_list = ir_list->get_next()) {
        dumpIR(ir_list, rg, ctx);
    }
}


void dumpIRList(IR const* ir_list, Region const* rg, CHAR const* attr,
                DumpFlag dumpflag)
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "");
    bool first_one = true;
    for (; ir_list != nullptr; ir_list = ir_list->get_next()) {
        if (first_one) {
            first_one = false;
            dumpIR(ir_list, rg, attr, dumpflag);
            continue;
        }
        dumpIR(ir_list, rg, attr, dumpflag);
    }
}


void dumpIRList(ConstIRList const& ir_list, Region const* rg)
{
    dumpIRList((IRList const&)ir_list, rg);
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


void dumpStorageSpace(IR const* ir, Region const* rg)
{
    if (ir->hasStorageSpace() && ir->getStorageSpace() != SS_UNDEF) {
        prt(rg, ":storage_space(%s)",
            StorageSpaceDesc::getName(ir->getStorageSpace()));
    }
}


void dumpAlign(UINT align, Region const* rg)
{
    if (align != 0) {
        prt(rg, ":align(%u)", align);
    }
}


void dumpOffset(IR const* ir, Region const* rg)
{
    if (ir->getOffset() != 0) {
        prt(rg, ":offset(%d)", ir->getOffset());
    }
}


void dumpIdinfo(IR const* ir, Region const* rg)
{
    if (!ir->hasIdinfo()) { return; }
    xcom::FixedStrBuf<40> buf;
    buf.strcat("%s", ir->getIdinfo()->get_name()->getStr());
    prt(rg, " '%s'", buf.getBuf());
}


static void dumpAbstractIdinfo(IR const* ir, Region const* rg)
{
    CHAR const* string = ir->getIdinfo()->get_name()->getStr();
    UINT const sz = 40;
    xcom::FixedStrBuf<sz> buf;
    buf.strcat(sz - 4, "%s", string);
    if (::strlen(string) > (sz - 4)) {
        buf.strcat("...");
    }
    prt(rg, " '%s'", buf.getBuf());
}


void dumpAllKids(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    LogMgr * lm = rg->getLogMgr();
    UINT dn = ctx.dn;
    lm->incIndent(dn);
    IRDumpCtx<> lctx(ctx);
    lctx.attr = nullptr;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k == nullptr) { continue; }
        dumpIRList(k, rg, lctx);
    }
    lm->decIndent(dn);
}


void dumpVarDecl(IR const* ir, Region const* rg)
{
    if (!ir->hasIdinfo()) { return; }
    ASSERT0(ir->getIdinfo());
    xcom::DefFixedStrBuf buf;
    if (ir->getIdinfo()->dumpVARDecl(buf, rg->getVarMgr()) != nullptr) {
        prt(rg, " decl:%s", buf.getBuf());
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
            xcom::FixedStrBuf<8> buf;
            buf.strcat(ILABEL_STR_FORMAT, ILABEL_CONT(li));
            if (isContainNonIdentifierChar(buf.getBuf())) {
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
            xcom::FixedStrBuf<8> buf;
            buf.strcat(CLABEL_STR_FORMAT, CLABEL_CONT(li));
            if (isContainNonIdentifierChar(buf.getBuf())) {
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
            xcom::FixedStrBuf<8> buf;
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


template <class StrBufType>
static void dumpAttr(OUT StrBufType & buf, IR const* ir, DumpFlag dumpflag)
{
    if (g_dump_opt.isDumpIRID() && dumpflag.have(IR_DUMP_IRID)) {
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


template <class StrBufType>
static void dumpDbx(OUT StrBufType & buf, Region const* rg,
                    BaseAttachInfo const* ac)
{
    ASSERT0(rg);
    DbxMgr * dbx_mgr = rg->getDbxMgr();
    ASSERT0(dbx_mgr);
    DbxAttachInfo const* dbx_info = (DbxAttachInfo const*)ac;
    bool is_has_dbg_info = dbx_info->
        dbx.getLine(LANG_CPP, dbx_mgr) != DBX_UNDEF;
    if(!is_has_dbg_info) { return; }
    buf.strcat(" file: ");
    buf.strcat("%d", dbx_info->dbx.
        getFileIndex(LANG_CPP, dbx_mgr));
    buf.strcat(" row: ");
    buf.strcat("%d", dbx_info->dbx.
        getLine(LANG_CPP, dbx_mgr));
    buf.strcat(" col: ");
    buf.strcat("%d", dbx_info->dbx.
        getColOffset(LANG_CPP, dbx_mgr));
    buf.strcat(" flag: ");
    buf.strcat("%d", dbx_info->dbx.
        getFlag(LANG_CPP, dbx_mgr));
}


template <class StrBufType>
static void dumpAttachInfo(OUT StrBufType & buf, IR const* ir,
                           Region const* rg, DumpFlag dumpflag)
{
    ASSERT0(ir && rg);
    bool is_dump_all_dwarf_info = dumpflag.have(IR_DUMP_DWARF) &&
                                  rg->getDbxMgr() != nullptr;
    AIContainer const* ai = ir->getAI();
    if (ai == nullptr) { return; }
    AICont const* cont = ai->getContainer();
    if (!cont->is_init()) { return; }
    bool first = true;
    for (UINT i = 0; i < cont->get_capacity(); i++) {
        BaseAttachInfo const* ac = cont->get(i);
        if (ac == nullptr) { continue; }
        if (first) {
            buf.strcat(" attachinfo:");
            first = false;
        } else {
            buf.strcat(",");
        }
        buf.strcat("%s", ai->getAIName(ac->getType()));
        if (is_dump_all_dwarf_info && ac->getType() == AI_DBX) {
            dumpDbx(buf, rg, ac);
        }
    }
}


void dumpRegion(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_inner_region = ctx.dumpflag.have(IR_DUMP_INNER_REGION);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "%s", IRNAME(ir));
    if (REGION_ru(ir)->getRegionVar() != nullptr) {
        Var * var = REGION_ru(ir)->getRegionVar();
        //Dump variable info.
        xcom::FixedStrBuf<40> tt;
        tt.strcat(40, "%s", var->get_name()->getStr());
        prt(rg, " \'%s\',id:%d", tt.getBuf(), REGION_ru(ir)->id());
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


void dumpArray(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
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
        xcom::FixedStrBuf<40> tt;
        for (IR const* sub = ARR_sub_list(ir);
             sub != nullptr; sub = sub->get_next()) {
            tt.clean();
            tt.strcat(40, " dim%d", dim);
            dumpIR(sub, rg, tt.getBuf(), ctx.dumpflag);
            dim++;
        }
        lm->decIndent(ctx.dn);
    }
    if (!dump_kid) { return; }
    lm->incIndent(ctx.dn);
    dumpIRList(ARR_base(ir), rg, (CHAR const*)" array_base", ctx.dumpflag);
    lm->decIndent(ctx.dn);
}


void dumpAlloca(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    dumpAlign(ALLOCA_align(ir), rg);
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx);
    }
}


void dumpCase(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    LogMgr * lm = rg->getLogMgr();
    ASSERT0(CASE_vexp(ir));
    ASSERT0(CASE_lab(ir));
    note(rg, "%s", IRNAME(ir));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);

    lm->incIndent(ctx.dn);
    dumpIRList(CASE_vexp(ir), rg, nullptr, ctx.dumpflag);
    note(rg, "\n");
    dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    lm->decIndent(ctx.dn);
}


void dumpSWITCH(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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


void dumpPhi(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
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


void dumpSelect(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
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
        dumpIRList(SELECT_trueexp(ir), rg, (CHAR const*)" true_exp",
                   ctx.dumpflag);
        lm->decIndent(ctx.dn);

        lm->incIndent(ctx.dn);
        dumpIRList(SELECT_falseexp(ir), rg, (CHAR const*)" false_exp",
                   ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}


void dumpLabel(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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


void dumpDoLoop(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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


void dumpWhileDo(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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


void dumpDoWhile(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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


void dumpIf(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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


void dumpBinAndUna(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
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
        dumpAllKids(ir, rg, ctx);
    }
}


void dumpReadPR(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s%d:%s", PR_TYPE_CHAR, ir->getPrno(), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
}


void dumpGeneral(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    bool dump_var_decl = ctx.dumpflag.have(IR_DUMP_VAR_DECL);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    dumpOffset(ir, rg);
    dumpStorageSpace(ir, rg);
    dumpIdinfo(ir, rg);
    if (dump_var_decl) {
        dumpVarDecl(ir, rg);
    }
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx);
    }
}


void dumpGeneralNoType(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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
        dumpAllKids(ir, rg, ctx);
    }
}

void dumpBranch(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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
        dumpAllKids(ir, rg, ctx);
    }
}


void dumpUndef(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    note(rg, "%s!", IRNAME(ir));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
}


void dumpStArray(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
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
        xcom::FixedStrBuf<40> tt;
        for (IR const* sub = ARR_sub_list(ir);
             sub != nullptr; sub = sub->get_next()) {
            tt.clean();
            tt.strcat(40, " dim%d", dim);
            dumpIR(sub, rg, tt.getBuf(), ctx.dumpflag);
            dim++;
        }
        lm->decIndent(ctx.dn);
    }
    if (dump_kid) {
        lm->incIndent(ctx.dn);
        dumpIRList(ARR_base(ir), rg, (CHAR const*)" array_base", ctx.dumpflag);
        dumpIRList(STARR_rhs(ir), rg, (CHAR const*)" rhs", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}


void dumpWritePR(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s %s%d:%s", IRNAME(ir), PR_TYPE_CHAR, ir->getPrno(),
         xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx);
    }
}


void dumpCallStmt(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    bool dump_var_decl = ctx.dumpflag.have(IR_DUMP_VAR_DECL);
    xcom::FixedStrBuf<64> buf;
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

    xcom::FixedStrBuf<30> tmpbuf;
    UINT i = 0;

    //Dump argument list.
    for (IR * p2 = CALL_arg_list(ir); p2 != nullptr; p2 = p2->get_next()) {
        tmpbuf.sprint(" arg%d", i);
        IRDumpCtx<> lctx(ctx);
        lctx.attr = tmpbuf.getBuf();
        lm->incIndent(ctx.dn);
        dumpIR(p2, rg, lctx);
        //dumpIR(p2, rg, tmpbuf, ctx.dumpflag);
        lm->decIndent(ctx.dn);
        i++;
    }

    //Dump dummy use.
    i = 0;
    for (IR * p2 = CALL_dummyuse(ir);
         p2 != nullptr; p2 = p2->get_next()) {
        tmpbuf.sprint(" dummy%d", i);
        IRDumpCtx<> lctx(ctx);
        lctx.attr = tmpbuf.getBuf();
        lm->incIndent(ctx.dn);
        dumpIR(p2, rg, lctx);
        //dumpIR(p2, rg, tmpbuf, ctx.dumpflag);
        lm->decIndent(ctx.dn);
        i++;
    }
}


void dumpLda(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    bool dump_var_decl = ctx.dumpflag.have(IR_DUMP_VAR_DECL);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    dumpOffset(ir, rg);
    dumpStorageSpace(ir, rg);
    dumpIdinfo(ir, rg);
    if (dump_var_decl) {
        dumpVarDecl(ir, rg);
    }
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx);
    }
}


void dumpReturn(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    note(rg, "%s", IRNAME(ir));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        dumpAllKids(ir, rg, ctx);
    }
}


void dumpConst(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
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
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    if (ir->is_sint() || ir->is_uint()) {
        xoc::dumpHostInteger(CONST_int_val(ir), d, rg, xtm, ir->is_sint());
        return;
    }
    if (ir->is_fp()) {
        xcom::FixedStrBuf<64> fpformat;
        fpformat.sprint("fpconst:%%s %%.%df", CONST_fp_mant(ir));
        prt(rg, fpformat.getBuf(), xtm->dump_type(d, buf), CONST_fp_val(ir));
        return;
    }
    if (ir->is_bool()) {
        prt(rg, "boolconst:%s %d", xtm->dump_type(d, buf), CONST_int_val(ir));
        return;
    }
    if (ir->is_str()) {
        UINT const tbuflen = 40;
        xcom::FixedStrBuf<tbuflen> tt;
        tt.strcat(40, "%s", SYM_name(CONST_str_val(ir)));

        //Remove \n to show string in one line.
        CHAR * tbufptr = tt.getBuf();
        ASSERT0(tbufptr);
        for (UINT i = 0; i < tt.getBufLen() && tbufptr[i] != 0; i++) {
            if (tbufptr[i] == '\n') { tbufptr[i] = ' '; }
        }

        if (CONST_str_val(ir)->getLen() < tbuflen) {
            prt(rg, "strconst:%s \\\"%s\\\"",
                xtm->dump_type(d, buf), tt.getBufLen());
        } else {
            prt(rg, "strconst:%s \\\"%s...\\\"",
                xtm->dump_type(d, buf), tt.getBufLen());
        }
        return;
    }
    if (ir->is_mc()) {
        //Imm may be MC type.
        CHAR const* intfmt = getHostUIntFormat(false);
        CHAR const* hexintfmt = getHostUIntFormat(true);
        xcom::FixedStrBuf<16> fmt;
        fmt.strcat("intconst:%%s %s|0x%s", intfmt, hexintfmt);
        prt(rg, fmt.getBuf(), xtm->dump_type(d, buf),
            CONST_int_val(ir), CONST_int_val(ir));
        return;
    }

    //Dump as HOST_INT type even if it is unrecognized,
    //leave the sanity check to verify().
    //Note the dump format may extend or truncate the real value.
    prt(rg, "intconst:%s %d|0x%x", xtm->dump_type(d, buf),
        CONST_int_val(ir),  CONST_int_val(ir));
}


void dumpIRCodeName(IR_CODE code, Region const* rg)
{
    prt(rg, "%s", IR::getIRCodeName(code));
}


void dumpIRName(IR const* ir, Region const* rg)
{
    prt(rg, "%s", IRNAME(ir));
    if (g_dump_opt.isDumpIRID()) {
        prt(rg, "(id:%u)", ir->id());
    }
}


void dumpIRListCombine(IR const* ir, Region const* rg)
{
    dumpIRList(ir, rg, nullptr, DumpFlag(IR_DUMP_COMBINE));
}


void dumpIRCombine(IR const* ir, Region const* rg)
{
    dumpIR(ir, rg, nullptr, DumpFlag(IR_DUMP_COMBINE));
}


void dumpIR(IR const* ir, Region const* rg, CHAR const* attr, DumpFlag dumpflag)
{
    UINT dn = 4; //default indent number.
    IRDumpCtx<> ctx(dn, dumpflag, attr);
    dumpIR(ir, rg, ctx);
}


//Dump IR and all of its kids.
//'attr': miscellaneous string which following 'ir'.
void dumpIR(IR const* ir, Region const* rg, MOD IRDumpCtx<> & ctx)
{
    bool dump_src_line = ctx.dumpflag.have(IR_DUMP_SRC_LINE);
    bool dump_newline = !ctx.dumpflag.have(IR_DUMP_NO_NEWLINE);
    LogMgr * lm = rg->getLogMgr();
    if (!rg->isLogMgrInit() || ir == nullptr) { return; }
    xcom::DefFixedStrBuf lattr;
    if (ctx.attr != nullptr) {
        lattr.strcat("%s", ctx.attr);
    }
    dumpAttr(lattr, ir, ctx.dumpflag);
    if (ctx.dump_attr_func != nullptr) {
        ctx.dump_attr_func->dumpAttr(lattr, rg, ir, ctx.dumpflag);
    }
    dumpAttachInfo(lattr, ir, rg, ctx.dumpflag);

    //Record type info and var decl.
    if (rg->getDbxMgr() != nullptr && dump_src_line) {
        DbxMgr::PrtCtx prtctx(LANG_CPP);
        prtctx.logmgr = lm;
        rg->getDbxMgr()->printSrcLine(ir, &prtctx);
    }
    if (dump_newline) {
        //Dump newline before root ir.
        prt(rg, "\n");
    }
    IRDumpFuncType dumpfunc = IRDES_dumpfunc(ir->getCode());
    ASSERT0(dumpfunc);
    IRDumpCtx<> lctx(ctx);
    lctx.attr = lattr.getBuf();
    (*dumpfunc)(ir, rg, lctx);
}


CHAR const* dumpIRToBuf(IR const* ir, Region const* rg, OUT StrBuf & outbuf,
                        DumpFlag dumpflag)
{
    ASSERT0(rg && ir);
    class Dump : public xoc::DumpToBuf {
    public:
        IR const* ir;
        DumpFlag const& dumpflag;
        Dump(Region const* rg, DumpFlag const& df, xcom::StrBuf & buf) :
            DumpToBuf(rg, buf), dumpflag(df) {}
        virtual void dumpUserInfo() const override
        { xoc::dumpIR(ir, getRegion(), nullptr, dumpflag); }
    };
    if (!rg->isLogMgrInit()) { return nullptr; }
    Dump d(rg, dumpflag, outbuf);
    d.ir = ir;
    d.dump();
    return outbuf.getBuf();
}


void dumpCFIDefCfa(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        LogMgr * lm = rg->getLogMgr();
        lm->incIndent(ctx.dn);
        dumpIRList(CFI_CFA_KID(ir, 0), rg, (CHAR const*)" reg", ctx.dumpflag);
        lm->decIndent(ctx.dn);
        lm->incIndent(ctx.dn);
        dumpIRList(CFI_CFA_KID(ir, 1), rg, (CHAR const*)" num", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}


void dumpCFISameValue(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        LogMgr * lm = rg->getLogMgr();
        lm->incIndent(ctx.dn);
        dumpIRList(CFI_SAME_VALUE_kid(ir, 0), rg,
                   (CHAR const*)" num", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}


void dumpCFIOffset(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        LogMgr * lm = rg->getLogMgr();
        lm->incIndent(ctx.dn);
        dumpIRList(CFI_OFFSET_kid(ir, 0), rg,
                   (CHAR const*)" reg", ctx.dumpflag);
        lm->decIndent(ctx.dn);
        lm->incIndent(ctx.dn);
        dumpIRList(CFI_OFFSET_kid(ir, 1),
                   rg, (CHAR const*)" num", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}


void dumpCFIRestore(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        LogMgr * lm = rg->getLogMgr();
        lm->incIndent(ctx.dn);
        dumpIRList(CFI_RESTORE_kid(ir, 0), rg,
                   (CHAR const*)" num", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}


void dumpCFIDefCfaOffst(IR const* ir, Region const* rg, IRDumpCtx<> & ctx)
{
    bool dump_addr = ctx.dumpflag.have(IR_DUMP_ADDR);
    bool dump_kid = ctx.dumpflag.have(IR_DUMP_KID);
    xcom::FixedStrBuf<64> buf;
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    ASSERT0(ctx.attr);
    prt(rg, "%s", ctx.attr);
    if (dump_kid) {
        LogMgr * lm = rg->getLogMgr();
        lm->incIndent(ctx.dn);
        dumpIRList(CFI_DEF_CFA_OFFSET_KID(ir, 0),
                   rg, (CHAR const*)" num", ctx.dumpflag);
        lm->decIndent(ctx.dn);
    }
}

//
//START DumpToBuf
//
DumpToBuf::DumpToBuf(Region const* rg, xcom::StrBuf & outbuf, UINT indent) :
    m_indent(indent), m_rg(rg), m_outbuf(outbuf)
{
    ASSERT0(rg);
    m_lm = rg->getLogMgr();
    ASSERT0(m_lm);
}


CHAR const* DumpToBuf::dump() const
{
    if (!m_rg->isLogMgrInit()) { return nullptr; }
    LogCtx tctx; //Define a tmp logctx.

    //Copy current LogCtx info except the dump buffer, because the tmp ctx
    //will open a new dump buffer to redirect the dumping.
    tctx.copyWithOutBuffer(m_lm->getCurrentCtx());

    //Push current LogCtx and enable tmp ctx as the present one.
    m_lm->push(tctx);

    //Open a new buffer for the tmp ctx.
    m_lm->startBuffer();

    //Invoke user's interface to dump info to dump-buffer of the tmp ctx.
    m_lm->incIndent(m_indent);
    dumpUserInfo();
    m_lm->decIndent(m_indent);

    //Get the dump buffer.
    xcom::StrBuf const* tmplogbuf = m_lm->getBuffer();
    ASSERT0(tmplogbuf);

    //Append the string in dump buffer into 'm_outbuf'.
    m_outbuf.strcat("%s", tmplogbuf->getBuf());

    //Close the dump buffer.
    m_lm->endBuffer(false);

    //Pop the tmp ctx and restore original LogCtx.
    m_lm->pop();
    return m_outbuf.getBuf();
}
//END DumpToBuf

} //namespace xoc
