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
#include "prdf.h"
#include "prssainfo.h"
#include "ir_ssa.h"

namespace xoc {

//
//START IRBB
//
UINT IRBB::count_mem() const
{
    UINT count = sizeof(IRBB);
    count += ir_list.count_mem();
    count += lab_list.count_mem();
    return count;
}


//Could ir be looked as a last stmt in basic block?
bool IRBB::is_bb_down_boundary(IR * ir)
{
    ASSERT(ir->isStmtInBB() || ir->is_lab(), ("illegal stmt in bb"));
    switch (IR_code(ir)) {
    case IR_CALL:
    case IR_ICALL: //indirective call
        return ((CCall*)ir)->isMustBBbound();
    case IR_GOTO:
    case IR_IGOTO:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
        return true;
    case IR_SWITCH:
        ASSERT(SWITCH_body(ir) == NULL,
                ("Peel switch-body to enable switch in bb-list construction"));
        return true;
    default: break;
    }
    return false;
}


void IRBB::dump(Region * ru)
{
    if (g_tfile == NULL) { return; }

    g_indent = 0;

    fprintf(g_tfile, "\n----- BB%d ------", BB_id(this));
    if (get_lab_list().get_elem_count() > 0) {
        fprintf(g_tfile, "\nLABEL:");
        dumpBBLabel(get_lab_list(), g_tfile);
    }

    //Attributes
    fprintf(g_tfile, "\nATTR:");
    if (BB_is_entry(this)) {
        fprintf(g_tfile, "entry_bb ");
    }

    //if (BB_is_exit(this)) {
    //    fprintf(g_tfile, "exit_bb ");
    //}

    if (BB_is_fallthrough(this)) {
        fprintf(g_tfile, "fall_through ");
    }

    if (BB_is_target(this)) {
        fprintf(g_tfile, "branch_target ");
    }

    //IR list
    fprintf(g_tfile, "\nSTMT NUM:%d", getNumOfIR());
    g_indent += 3;
    TypeMgr * dm = ru->get_type_mgr();
    for (IR * ir = BB_first_ir(this);
        ir != NULL; ir = BB_irlist(this).get_next()) {
        ASSERT0(IR_next(ir) == NULL && IR_prev(ir) == NULL);
        ASSERT0(ir->get_bb() == this);
        dump_ir(ir, dm, NULL, true, true, false);
    }
    g_indent -= 3;
    fprintf(g_tfile, "\n");
    fflush(g_tfile);
}


//Check that all basic blocks should only end with terminator IR.
void IRBB::verify()
{
    UINT c = 0;
    C<IR*> * ct;
    for (IR * ir = BB_irlist(this).get_head(&ct);
         ir != NULL; ir = BB_irlist(this).get_next(&ct)) {
        ASSERT0(IR_next(ir) == NULL && IR_prev(ir) == NULL);
        ASSERT0(ir->get_bb() == this);
        switch (IR_code(ir)) {
        case IR_ST:
        case IR_STPR:
        case IR_STARRAY:
        case IR_IST:
        case IR_PHI:
        case IR_REGION:
        case IR_CALL:
        case IR_ICALL:
        case IR_GOTO:
        case IR_IGOTO:
        case IR_TRUEBR:
        case IR_FALSEBR:
        case IR_RETURN:
        case IR_SWITCH:
            break;
        default: ASSERT(0, ("BB does not supported this kind of IR."));
        }

        if (ir->is_calls_stmt() || ir->is_cond_br() ||
            ir->is_multicond_br() || ir->is_uncond_br()) {
            ASSERT(ir == BB_last_ir(this), ("invalid bb boundary."));
        }

        c++;
    }
    ASSERT0(c == getNumOfIR());
}


//Return true if one of bb's successor has a phi.
bool IRBB::successorHasPhi(CFG<IRBB, IR> * cfg)
{
    Vertex * vex = cfg->get_vertex(BB_id(this));
    ASSERT0(vex);
    for (EdgeC * out = VERTEX_out_list(vex);
         out != NULL; out = EC_next(out)) {
        Vertex * succ_vex = EDGE_to(EC_edge(out));
        IRBB * succ = cfg->get_bb(VERTEX_id(succ_vex));
        ASSERT0(succ);

        for (IR * ir = BB_first_ir(succ);
             ir != NULL; ir = BB_next_ir(succ)) {
            if (ir->is_phi()) { return true; }
        }
    }
    return false;
}


//Duplicate and add an operand that indicated by opnd_pos at phi stmt
//in one of bb's successors.
void IRBB::dupSuccessorPhiOpnd(CFG<IRBB, IR> * cfg, Region * ru, UINT opnd_pos)
{
    IR_CFG * ircfg = (IR_CFG*)cfg;
    Vertex * vex = ircfg->get_vertex(BB_id(this));
    ASSERT0(vex);
    for (EdgeC * out = VERTEX_out_list(vex);
         out != NULL; out = EC_next(out)) {
        Vertex * succ_vex = EDGE_to(EC_edge(out));
        IRBB * succ = ircfg->get_bb(VERTEX_id(succ_vex));
        ASSERT0(succ);

        for (IR * ir = BB_first_ir(succ);
             ir != NULL; ir = BB_next_ir(succ)) {
            if (!ir->is_phi()) { break; }

            ASSERT0(cnt_list(PHI_opnd_list(ir)) >= opnd_pos);

            IR * opnd;
            UINT lpos = opnd_pos;
            for (opnd = PHI_opnd_list(ir);
                 lpos != 0; opnd = IR_next(opnd)) {
                ASSERT0(opnd);
                lpos--;
            }

            IR * newopnd = ru->dupIRTree(opnd);
            if (opnd->is_read_pr()) {
                newopnd->copyRef(opnd, ru);
                ASSERT0(PR_ssainfo(opnd));
                PR_ssainfo(newopnd) = PR_ssainfo(opnd);
                SSA_uses(PR_ssainfo(newopnd)).append(newopnd);
            }

            ((CPhi*)ir)->addOpnd(newopnd);
        }
    }
}
//END IRBB



//Before removing bb, revising phi opnd if there are phis
//in one of bb's successors.
void IRBB::removeSuccessorPhiOpnd(CFG<IRBB, IR> * cfg)
{
    IR_CFG * ircfg = (IR_CFG*)cfg;
    Region * ru = ircfg->get_ru();
    Vertex * vex = ircfg->get_vertex(BB_id(this));
    ASSERT0(vex);
    for (EdgeC * out = VERTEX_out_list(vex);
         out != NULL; out = EC_next(out)) {
        Vertex * succ_vex = EDGE_to(EC_edge(out));
        IRBB * succ = ircfg->get_bb(VERTEX_id(succ_vex));
        ASSERT0(succ);

        UINT const pos = ircfg->WhichPred(this, succ);

        for (IR * ir = BB_first_ir(succ);
             ir != NULL; ir = BB_next_ir(succ)) {
            if (!ir->is_phi()) { break; }

            ASSERT0(cnt_list(PHI_opnd_list(ir)) ==
                     cnt_list(VERTEX_in_list(succ_vex)));

            IR * opnd;
            UINT lpos = pos;
            for (opnd = PHI_opnd_list(ir);
                 lpos != 0; opnd = IR_next(opnd)) {
                ASSERT0(opnd);
                lpos--;
            }

            opnd->removeSSAUse();
            ((CPhi*)ir)->removeOpnd(opnd);
            ru->freeIRTree(opnd);
        }
    }
}
//END IRBB

void dumpBBLabel(List<LabelInfo const*> & lablist, FILE * h)
{
    ASSERT0(h);
    C<LabelInfo const*> * ct;
    for (lablist.get_head(&ct); ct != lablist.end(); ct = lablist.get_next(ct)) {
        LabelInfo const* li = ct->val();
        switch (LABEL_INFO_type(li)) {
        case L_CLABEL:
            fprintf(h, CLABEL_STR_FORMAT, CLABEL_CONT(li));
            break;
        case L_ILABEL:
            fprintf(h, ILABEL_STR_FORMAT, ILABEL_CONT(li));
            break;
        case L_PRAGMA:
            fprintf(h, "%s", SYM_name(LABEL_INFO_pragma(li)));
            break;
        default: ASSERT0(0);
        }

        if (LABEL_INFO_is_try_start(li) ||
            LABEL_INFO_is_try_end(li) ||
            LABEL_INFO_is_catch_start(li)) {
            fprintf(g_tfile, "(");
            if (LABEL_INFO_is_try_start(li)) {
                fprintf(g_tfile, "try_start,");
            }
            if (LABEL_INFO_is_try_end(li)) {
                fprintf(g_tfile, "try_end,");
            }
            if (LABEL_INFO_is_catch_start(li)) {
                fprintf(g_tfile, "catch_start");
            }
            fprintf(g_tfile, ")");
        }
        fprintf(g_tfile, " ");
    }
}


void dumpBBList(BBList * bbl, Region * ru, CHAR const* name)
{
    ASSERT0(ru);
    FILE * h = NULL;
    FILE * org_g_tfile = g_tfile;
    if (name == NULL) {
        h = g_tfile;
    } else {
        unlink(name);
        h = fopen(name, "a+");
        ASSERT(h != NULL, ("can not dump."));
        g_tfile = h;
    }
    if (h != NULL && bbl->get_elem_count() != 0) {
        fprintf(h, "\n==---- DUMP '%s' BBList ----==",
                ru->get_ru_name());
        for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
            bb->dump(ru);
        }
    }
    fflush(h);
    if (h != org_g_tfile) {
        fclose(h);
    }
    g_tfile = org_g_tfile;
}

} //namespace xoc
