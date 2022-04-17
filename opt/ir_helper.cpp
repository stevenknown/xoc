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

//Use do-while to supress warning: value computed is not used
#define DUMPADDR(ir) \
  do { int x = dump_addr ? prt(rg, " 0x%p", (ir)) : 0; DUMMYUSE(x); } while (0)

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


//CASE:_$L9 is non-identifier char because of '$'.
bool isContainNonIdentifierChar(CHAR const* name)
{
    CHAR const* p = name;
    if (*p == 0) { return false; }
    if (!xcom::xisalpha(*p) && *p != '_') {
        //Check the first char.
        return true;
    }
    p++;
    for (; *p != 0; p++) {
        if (!xcom::xisalpha(*p) && *p != '_' && !xcom::xisdigit(*p)) {
            return true;
        }
    }
    return false;
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


static void dumpAttachInfo(OUT CHAR * buf, IR const* ir)
{
    ASSERT0(ir && buf);
    AIContainer const* ai = ir->getAI();
    if (ai == nullptr) { return; }

    AICont const* cont = ai->getContainer();

    if (!cont->is_init()) { return; }

    strcat(buf, " attachinfo:");
    CHAR * p = buf + strlen(buf);
    bool not_first = false;
    for (UINT i = 0; i < cont->get_capacity(); i++) {
        BaseAttachInfo const* ac = cont->get(i);
        if (ac == nullptr) { continue; }

        if (!not_first) {
            not_first = true;
        } else {
            sprintf(p, ",");
            p = p + strlen(p);
        }

        sprintf(p, "%s", ai->getAIName(ac->getType()));
        p = p + strlen(p);
    }
}


void dumpGETELEM(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                 CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
 
    note(rg, "getelem %s%d:%s", PR_TYPE_CHAR, GETELEM_prno(ir),
         xtm->dump_type(d, buf));
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

    lm->incIndent(dn);
    dumpIRList(GETELEM_base(ir), rg, (CHAR*)" base", dumpflag);
    dumpIRList(GETELEM_ofst(ir), rg, (CHAR*)" offset", dumpflag);
    lm->decIndent(dn);
}


void dumpSETELEM(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                 CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr(); 
    note(rg, "setelem %s%d:%s", PR_TYPE_CHAR, SETELEM_prno(ir),
         xtm->dump_type(d, buf));
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

    lm->incIndent(dn);
    dumpIRList(SETELEM_base(ir), rg, nullptr, dumpflag);
    dumpIRList(SETELEM_val(ir), rg, nullptr, dumpflag);
    dumpIRList(SETELEM_ofst(ir), rg, (CHAR*)" offset", dumpflag);
    lm->decIndent(dn);
}


void dumpBREAK(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
               CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    note(rg, "break");
    DUMPADDR(ir);
    prt(rg, "%s", attr);
}


void dumpCONTINUE(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                  CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    note(rg, "continue");
    DUMPADDR(ir);
    prt(rg, "%s", attr);
}


void dumpGOTO(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
              CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    note(rg, "goto ");
    dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    DUMPADDR(ir);
    prt(rg, "%s", attr); 
}


void dumpRETURN(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "return");
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

    lm->incIndent(dn);
    dumpIR(RET_exp(ir), rg, nullptr, dumpflag);
    lm->decIndent(dn);
}


void dumpREGION(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_inner_region = HAVE_FLAG(dumpflag, IR_DUMP_INNER_REGION);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "region");
    if (REGION_ru(ir)->getRegionVar() != nullptr) {
        Var * ruvar = REGION_ru(ir)->getRegionVar();
        CHAR tt[40];
        tt[0] = 0;

        //Dump variable info.
        xstrcat(tt, 40, "%s", SYM_name(ruvar->get_name()));
        prt(rg, " \'%s\',id:%d", tt, REGION_ru(ir)->id());
    }

    DUMPADDR(ir); //Dump IR address.
    prt(rg, "%s", attr); //Dump attributes.

    if (dump_inner_region) {
        //Inner region.
        ASSERT0(REGION_ru(ir));
        lm->incIndent(dn);
        note(rg, "\nregion-info:");
        REGION_ru(ir)->dump(dump_inner_region);
        lm->decIndent(dn);
    }
}


void dumpFALSEBR(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                 CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "falsebr ");
    dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

    lm->incIndent(dn);
    dumpIRList(BR_det(ir), rg, nullptr, dumpflag);
    lm->decIndent(dn);
}


void dumpTRUEBR(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "truebr ");
    dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

    lm->incIndent(dn);
    dumpIRList(BR_det(ir), rg, nullptr, dumpflag);
    lm->decIndent(dn);
}


void dumpARRAY(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
               CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
 
    StrBuf buf2(64);
    if (ARR_ofst(ir) != 0) {
        note(rg, "array (%s:offset(%d), ety:%s)",
             xtm->dump_type(d, buf),
             ARR_ofst(ir),
             xtm->dump_type(ARR_elemtype(ir), buf2));
    } else {
        note(rg, "array (%s, ety:%s)",
             xtm->dump_type(d, buf),
             xtm->dump_type(ARR_elemtype(ir), buf2));
    }

    DUMPADDR(ir);
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

    if (!dump_kid) { return; }

    lm->incIndent(dn);
    dumpIRList(ARR_base(ir), rg, (CHAR*)" array_base", dumpflag);
    lm->decIndent(dn);
}


void dumpCASE(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
              CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    LogMgr * lm = rg->getLogMgr();
 
    ASSERT0(CASE_vexp(ir));
    ASSERT0(CASE_lab(ir));
    note(rg, "case");
    DUMPADDR(ir);
    prt(rg, "%s", attr);

    lm->incIndent(dn);
    dumpIRList(CASE_vexp(ir), rg, nullptr, dumpflag);
    note(rg, "\n");
    dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    lm->decIndent(dn);
}


void dumpSWITCH(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();

    note(rg, "switch");
    if (SWITCH_deflab(ir) != nullptr) {
        prt(rg, ", deflab: ");
        dumpLabelDecl(ir->getLabel(), rg->getRegionMgr(), false);
    }
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

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


void dumpPHI(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
             CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
 
    note(rg, "%s %s%d:%s", IRNAME(ir), PR_TYPE_CHAR, PHI_prno(ir),
         xtm->dump_type(d, buf));

    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

    prt(rg, " = ", attr);
    lm->incIndent(dn);
    IR * opnd = PHI_opnd_list(ir);
    while (opnd != nullptr) {
        dumpIR(opnd, rg, nullptr, dumpflag);
        opnd = opnd->get_next();
    }
    lm->decIndent(dn);
}


void dumpLDA(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
             CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_var_decl = HAVE_FLAG(dumpflag, IR_DUMP_VAR_DECL);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    CHAR tt[40];
    tt[0] = 0;

    //Dump variable info.
    CHAR * name = xstrcat(tt, 40, "%s",
        SYM_name(LDA_idinfo(ir)->get_name()));
    if (LDA_ofst(ir) != 0) {
        note(rg, "lda:%s:offset(%d) '%s'",
             xtm->dump_type(d, buf), LDA_ofst(ir), name);
    } else {
        note(rg, "lda:%s '%s'", xtm->dump_type(d, buf), name);
    }

    //Dump declaration if frontend supplied.
    buf.clean();
    if (dump_var_decl && LDA_idinfo(ir)->dumpVARDecl(buf) != nullptr) {
        prt(rg, " decl:%s", buf.buf);
    }

    //Dump IR address.
    DUMPADDR(ir);
    prt(rg, "%s", attr);
}


void dumpSELECT(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
 
    note(rg, "select:%s", xtm->dump_type(d, buf));
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

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


void dumpLABEL(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
               CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
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

    prt(rg, "%s", attr);
}


void dumpIGOTO(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
               CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "igoto");
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

    lm->incIndent(dn);
    dumpIRList(IGOTO_vexp(ir), rg, nullptr, dumpflag);
    lm->decIndent(dn);

    note(rg, "\ncase_list");
    lm->incIndent(dn);
    dumpIRList(IGOTO_case_list(ir), rg, nullptr, dumpflag);
    lm->decIndent(dn);
}


void dumpDOLOOP(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "doloop");
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

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


void dumpWHILEDO(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                 CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "whiledo");
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

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


void dumpDOWHILE(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                 CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "dowhile");
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

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


void dumpIF(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
            CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    LogMgr * lm = rg->getLogMgr();
    note(rg, "if");
    DUMPADDR(ir);
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
}


void dumpBinAndUna(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                   CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
 
    note(rg, "%s:%s", IRNAME(ir), xtm->dump_type(d, buf));
    if (ir->is_cvt() && CVT_round(ir) != ROUND_UNDEF) {
      prt(rg, ":round(%s)", ROUND_NAME(CVT_round(ir)));
    }
    DUMPADDR(ir);
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
}


void dumpID(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
            CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_var_decl = HAVE_FLAG(dumpflag, IR_DUMP_VAR_DECL);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    CHAR tt[40];
    tt[0] = 0;

    //Dump ID name.
    CHAR * name = xstrcat(tt, 40, "%s", SYM_name(ID_info(ir)->get_name()));
    note(rg, "id:%s '%s'", xtm->dump_type(d, buf), name);

    buf.clean();
    if (dump_var_decl && ID_info(ir)->dumpVARDecl(buf) != nullptr) {
        prt(rg, " decl:%s", buf.buf);
    }

    //Dump IR address.
    DUMPADDR(ir);
    prt(rg, "%s", attr);
}


void dumpPR(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
            CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    note(rg, "%s%d:%s", PR_TYPE_CHAR, PR_no(ir), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    prt(rg, "%s", attr);
}


void dumpILD(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
             CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
 
    if (ILD_ofst(ir) != 0) {
        note(rg, "ild:%s:offset(%d)",
             xtm->dump_type(d, buf), ILD_ofst(ir));
    } else {
        note(rg, "ild:%s", xtm->dump_type(d, buf));
    }

    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (dump_kid) {
        lm->incIndent(dn);
        dumpIRList(ILD_base(ir), rg, nullptr, dumpflag);
        lm->decIndent(dn);
    }
}


void dumpLD(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
            CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_var_decl = HAVE_FLAG(dumpflag, IR_DUMP_VAR_DECL);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    CHAR tt[40];
    tt[0] = 0;

    //Dump variable info.
    CHAR * name = xstrcat(tt, 40, "%s", SYM_name(LD_idinfo(ir)->get_name()));
    if (LD_ofst(ir) != 0) {
        note(rg, "ld:%s:offset(%d) '%s'",
             xtm->dump_type(d, buf), LD_ofst(ir), name);
    } else {
        note(rg, "ld:%s '%s'", xtm->dump_type(d, buf), name);
    }

    //Dump declaration if frontend supplied.
    buf.clean();
    if (dump_var_decl && LD_idinfo(ir)->dumpVARDecl(buf) != nullptr) {
        prt(rg, " decl:%s", buf.buf);
    }

    //Dump IR address.
    DUMPADDR(ir);
    prt(rg, "%s", attr);
}


void dumpIST(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
             CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
 
    if (IST_ofst(ir) != 0) {
        note(rg, "ist:%s:offset(%d)", xtm->dump_type(d, buf), IST_ofst(ir));
    } else {
        note(rg, "ist:%s", xtm->dump_type(d, buf));
    }

    //Dump IR address.
    DUMPADDR(ir);
    prt(rg, "%s", attr);

    if (dump_kid) {
        lm->incIndent(dn);
        dumpIRList(IST_base(ir), rg, (CHAR*)" base", dumpflag);
        dumpIRList(IST_rhs(ir), rg, nullptr, dumpflag);
        lm->decIndent(dn);
    }
}


void dumpSTARRAY(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                 CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();

    StrBuf buf2(64); 
    if (ARR_ofst(ir) != 0) {
        note(rg, "starray (%s:offset(%d), ety:%s)",
             xtm->dump_type(d, buf),
             ARR_ofst(ir),
             xtm->dump_type(ARR_elemtype(ir), buf2));
    } else {
        note(rg, "starray (%s, ety:%s)",
             xtm->dump_type(d, buf),
             xtm->dump_type(ARR_elemtype(ir), buf2));
    }

    DUMPADDR(ir);
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
}


void dumpSTPR(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
              CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
 
    note(rg, "stpr %s%d:%s", PR_TYPE_CHAR, STPR_no(ir), xtm->dump_type(d, buf));
    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (dump_kid) {
        lm->incIndent(dn);
        dumpIRList(STPR_rhs(ir), rg, nullptr, dumpflag);
        lm->decIndent(dn);
    }
}


void dumpST(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
            CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    bool dump_var_decl = HAVE_FLAG(dumpflag, IR_DUMP_VAR_DECL);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
    CHAR tt[40];
    tt[0] = 0;
    CHAR * name = xstrcat(tt, 40, "%s",
    SYM_name(ST_idinfo(ir)->get_name()));

    //Dump operator and variable name.
    note(rg, "st:%s", xtm->dump_type(d, buf));
    if (ST_ofst(ir) != 0) {
        prt(rg, ":offset(%d)", ST_ofst(ir));
    }
    prt(rg, " '%s'", name);

    //Dump declaration info if the frontend supplied.
    buf.clean();
    if (dump_var_decl && ST_idinfo(ir)->dumpVARDecl(buf) != nullptr) {
        prt(rg, " decl:%s", buf.buf);
    }

    DUMPADDR(ir);
    prt(rg, "%s", attr);

    if (dump_kid) {
        lm->incIndent(dn);
        dumpIRList(ST_rhs(ir), rg, nullptr, dumpflag);
        lm->decIndent(dn);
    }
}


void dumpCallStmt(IR const* ir, Region const* rg, UINT dn, UINT dumpflag,
                  CHAR * attr)
{
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    bool dump_kid = HAVE_FLAG(dumpflag, IR_DUMP_KID);
    bool dump_var_decl = HAVE_FLAG(dumpflag, IR_DUMP_VAR_DECL);
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    LogMgr * lm = rg->getLogMgr();
    if (ir->hasReturnValue()) {
        note(rg, "%s%d:%s = ", PR_TYPE_CHAR, CALL_prno(ir),
             xtm->dump_type(d, buf));
    }
    if (ir->is_call()) {
        CHAR tt[44];
        tt[0] = 0;
        CHAR const* string = SYM_name(CALL_idinfo(ir)->get_name());
        CHAR * name = xstrcat(tt, 40, "%s", string);
        if (strlen(string) > 40) {
            strcat(tt, "...");
        }
        prt(rg, "%s '%s' ", IRNAME(ir), name);
        buf.clean();
        if (dump_var_decl && CALL_idinfo(ir)->dumpVARDecl(buf) != nullptr) {
            prt(rg, "decl:%s", buf.buf);
        }
    } else {
        prt(rg, "%s ", IRNAME(ir));
    }

    DUMPADDR(ir);
    prt(rg, "%s", attr);
    if (!dump_kid) { return; }

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


void dumpConst(IR const* ir, Region const* rg)
{
    StrBuf buf(64);
    TypeMgr const* xtm = rg->getTypeMgr();
    Type const* d = ir->getType();
    if (ir->is_sint()) {
        CHAR const* intfmt = getIntFormat(false);
        CHAR const* hexintfmt = getIntFormat(true);
        StrBuf fmt(16);
        fmt.strcat("intconst:%%s %s|0x%s", intfmt, hexintfmt);
        prt(rg, fmt.buf, xtm->dump_type(d, buf),
            CONST_int_val(ir), CONST_int_val(ir));
        return;
    }

    if (ir->is_uint()) {
        CHAR const* intfmt = getUIntFormat(false);
        CHAR const* hexintfmt = getUIntFormat(true);
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
        CHAR const* intfmt = getUIntFormat(false);
        CHAR const* hexintfmt = getUIntFormat(true);
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


//Dump IR and all of its kids.
//'attr': miscellaneous string which following 'ir'.
void dumpIR(IR const* ir, Region const* rg, IN CHAR * attr, UINT dumpflag)
{
    bool dump_src_line = HAVE_FLAG(dumpflag, IR_DUMP_SRC_LINE);
    bool dump_newline = !HAVE_FLAG(dumpflag, IR_DUMP_NO_NEWLINE);
    bool dump_addr = HAVE_FLAG(dumpflag, IR_DUMP_ADDR);
    TypeMgr * tm = rg->getTypeMgr();
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
    if (g_dump_opt.isDumpIRID()) {
        sprintf(p, " id:%d", ir->id());
    }
    if (ir->isMayThrow(false)) {
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
    if (ir->hasSideEffect(false)) {
        strcat(p, " sideeffect");
    }
    if (ir->isNoMove(false)) {
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
    if (dump_newline) {
        //Dump newline before root ir.
        prt(rg, "\n");
    }

    switch (ir->getCode()) {
    case IR_ST:
        dumpST(ir, rg, dn, dumpflag, attr);
        break;
    case IR_STPR:
        dumpSTPR(ir, rg, dn, dumpflag, attr);
        break;
    case IR_SETELEM:
        dumpSETELEM(ir, rg, dn, dumpflag, attr);
        break;
    case IR_GETELEM:
        dumpGETELEM(ir, rg, dn, dumpflag, attr);
        break;
    case IR_STARRAY:
        dumpSTARRAY(ir, rg, dn, dumpflag, attr);
        break;
    case IR_IST:
        dumpIST(ir, rg, dn, dumpflag, attr);
        break;
    case IR_LD:
        dumpLD(ir, rg, dn, dumpflag, attr);
        break;
    case IR_ILD:
        dumpILD(ir, rg, dn, dumpflag, attr);
        break;
    case IR_PR:
        dumpPR(ir, rg, dn, dumpflag, attr);
        break;
    case IR_ID:
        dumpID(ir, rg, dn, dumpflag, attr);
        break;
    case IR_CONST:
        note(rg, "");
        dumpConst(ir, rg);
        DUMPADDR(ir);
        prt(rg, "%s", attr);
        break;
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        dumpBinAndUna(ir, rg, dn, dumpflag, attr);
        break;
    case IR_IF:
        dumpIF(ir, rg, dn, dumpflag, attr);
        break;
    case IR_DO_WHILE:
        dumpDOWHILE(ir, rg, dn, dumpflag, attr);
        break;
    case IR_WHILE_DO:
        dumpWHILEDO(ir, rg, dn, dumpflag, attr);
        break;
    case IR_DO_LOOP:
        dumpDOLOOP(ir, rg, dn, dumpflag, attr);
        break;
    case IR_BREAK:
        dumpBREAK(ir, rg, dn, dumpflag, attr);
        break;
    case IR_CONTINUE:
        dumpCONTINUE(ir, rg, dn, dumpflag, attr);
        break;
    case IR_RETURN:
        dumpRETURN(ir, rg, dn, dumpflag, attr);
        break;
    case IR_GOTO:
        dumpGOTO(ir, rg, dn, dumpflag, attr);
        break;
    case IR_IGOTO:
        dumpIGOTO(ir, rg, dn, dumpflag, attr);
        break;
    case IR_LABEL:
        dumpLABEL(ir, rg, dn, dumpflag, attr);
        break;
    case IR_SELECT: //formulized log_OR_exp?exp:cond_exp
        dumpSELECT(ir, rg, dn, dumpflag, attr);
        break;
    case IR_LDA: //Get address of a symbol
        dumpLDA(ir, rg, dn, dumpflag, attr);
        break;
    case IR_PHI:
        //Dump PHI function. 
        dumpPHI(ir, rg, dn, dumpflag, attr);
        break;
    case IR_SWITCH:
        dumpSWITCH(ir, rg, dn, dumpflag, attr);
        break;
    case IR_CASE:
        dumpCASE(ir, rg, dn, dumpflag, attr);
        break;
    case IR_ARRAY:
        dumpARRAY(ir, rg, dn, dumpflag, attr);
       break;
    case IR_CALL:
    case IR_ICALL:
        dumpCallStmt(ir, rg, dn, dumpflag, attr);
        break;
    case IR_TRUEBR:
        dumpTRUEBR(ir, rg, dn, dumpflag, attr);
        break;
    case IR_FALSEBR:
        dumpFALSEBR(ir, rg, dn, dumpflag, attr);
        break;
    case IR_REGION:
        dumpREGION(ir, rg, dn, dumpflag, attr);
        break;
    case IR_UNDEF:
        note(rg, "undef!");
        DUMPADDR(ir);
        prt(rg, "%s", attr);
        break;
    default:
        ASSERTN(0, ("unknown IR type:%s", IRNAME(ir)));
        return ;
    }
}


//Iterative access ir tree. This funtion initialize the iterator.
//'ir': the root ir of the tree.
//'it': iterator. It should be clean already.
//Readonly function.
IR const* iterInitC(IR const* ir, OUT ConstIRIter & it, bool iter_next)
{
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access ir tree.
//This function return the next IR node accroding to 'it'.
//'it': iterator.
//Readonly function.
IR const* iterNextC(MOD ConstIRIter & it, bool iter_next)
{
    IR const* ir = it.remove_head();
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//The function is a readonly function.
//Use iterExpNextC to iter next IR.
IR const* iterExpInitC(IR const* ir, OUT ConstIRIter & it, bool iter_next)
{
    if (ir == nullptr) { return nullptr; }
    ASSERT0(ir->is_stmt());
    //Other stmt.
    IR const* firstkid = nullptr;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR const* kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        if (firstkid == nullptr) {
            firstkid = kid;
            continue;
        }
        it.append_tail(kid);
    }

    if (firstkid == nullptr) { return nullptr; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(firstkid); i++) {
        IR const* kid = firstkid->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && IR_next(firstkid) != nullptr) {
        it.append_tail(IR_next(firstkid));
    }
    return firstkid;
}


//Iterative access the ir tree that start with 'ir'.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it may be either stmt or expression.
//'it': iterator. It should be clean already.
//Note this function is NOT readonly, the returnd IR may be modified.
IR * iterInit(IN IR * ir, OUT IRIter & it, bool iter_next)
{
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}

//Iterative access the ir tree.
//This funtion return the next IR node accroding to 'it'.
//'it': iterator.
//Note this function is NOT readonly, the returnd IR may be modified.
IR * iterNext(MOD IRIter & it, bool iter_next)
{
    IR * ir = it.remove_head();
    if (ir == nullptr) { return nullptr; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && ir->get_next() != nullptr) {
        it.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//Use iterExpNextC to iter next IR.
IR * iterExpInit(IR * ir, OUT IRIter & it, bool iter_next)
{
    if (ir == nullptr) { return nullptr; }
    ASSERT0(ir->is_stmt());

    //Other stmt.
    IR * firstkid = nullptr;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        if (firstkid == nullptr) {
            firstkid = kid;
            continue;
        }
        it.append_tail(kid);
    }

    if (firstkid == nullptr) { return nullptr; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(firstkid); i++) {
        IR * kid = firstkid->getKid(i);
        if (kid != nullptr) {
            it.append_tail(kid);
        }
    }
    if (iter_next && IR_next(firstkid) != nullptr) {
        it.append_tail(IR_next(firstkid));
    }

    return firstkid;
}


//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//Use iterExpNextC to iter next IR.
IR * iterExpOfStmtInit(IR * ir, OUT IRIter & it)
{
    ASSERT0(ir->is_stmt());
    IR * firstkid = nullptr;
    switch (ir->getCode()) {
    case IR_IST:
        ASSERT0(IST_base(ir));
        firstkid = IST_base(ir);
        it.append_tail(firstkid);
        break;
    case IR_STARRAY:
        ASSERT0(STARR_base(ir));
        firstkid = STARR_base(ir);
        it.append_tail(firstkid);
        if (STARR_sub_list(ir) != nullptr) {
            it.append_tail(STARR_sub_list(ir));
        }
        break;
    default:;
    }
    return firstkid;
}


//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//ir: the root ir of the tree, it must be stmt.
//it: iterator. It should be clean already.
//Use iterExpNextC to iter next IR.
IR const* iterExpOfStmtInitC(IR * ir, OUT ConstIRIter & it)
{
    ASSERT0(ir->is_stmt());
    IR const* firstkid = nullptr;
    switch (ir->getCode()) {
    case IR_IST:
        ASSERT0(IST_base(ir));
        firstkid = IST_base(ir);
        it.append_tail(firstkid);
        break;
    case IR_STARRAY:
        ASSERT0(STARR_base(ir));
        firstkid = STARR_base(ir);
        it.append_tail(firstkid);
        if (STARR_sub_list(ir) != nullptr) {
            it.append_tail(STARR_sub_list(ir));
        }
        break;
    default:;
    }
    return firstkid;
}


//
//START IRSet
//
void IRSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    DefSBitSet::dump(rg->getLogMgr()->getFileHandler());
}


bool IRSet::allElemBeExp(Region const* rg) const
{
    IRSetIter it;
    for (BSIdx i = get_first(&it); i != BS_UNDEF; i = get_next(i, &it)) {
        IR * e = rg->getIR(i);
        if (e == nullptr || !e->is_exp()) {
            return false;
        }
    }
    return true;
}
//END IRSet

} //namespace xoc
