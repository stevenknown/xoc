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
//START BBIRList
//
//Insert ir prior to cond_br, uncond_br, call, return.
C<IR*> * BBIRList::append_tail_ex(IR * ir)
{
    if (ir == NULL) { return NULL; }

    C<IR*> * ct;
    for (List<IR*>::get_tail(&ct);
         ct != List<IR*>::end(); ct = List<IR*>::get_prev(ct)) {
        if (!m_bb->is_bb_down_boundary(ct->val())) {
            break;
        }
    }

    ASSERT0(m_bb);
    ir->set_bb(m_bb);
    if (ct == NULL) {
        //The only one stmt of BB is down boundary or bb is empty.
        return EList<IR*, IR2Holder>::append_head(ir);
    }
    return EList<IR*, IR2Holder>::insert_after(ir, ct);
}
//END BBIRList


//
//START IRBB
//
size_t IRBB::count_mem() const
{
    size_t count = sizeof(IRBB);
    count += ir_list.count_mem();
    count += lab_list.count_mem();
    return count;
}


//Could ir be looked as a last stmt in basic block?
bool IRBB::is_bb_down_boundary(IR * ir)
{
    ASSERT(ir->isStmtInBB() || ir->is_lab(), ("illegal stmt in bb"));
    switch (ir->get_code()) {
    case IR_CALL:
    case IR_ICALL: //indirective call
        return ((CCall*)ir)->isMustBBbound();
    case IR_GOTO:
    case IR_IGOTO:
        return true;
    case IR_SWITCH:
        ASSERT(SWITCH_body(ir) == NULL,
               ("Peel switch-body to enable switch in bb-list construction"));
        return true;
    case IR_TRUEBR:
    case IR_FALSEBR:
        return true;
    case IR_RETURN:
    case IR_REGION:
        return true;
    default: break;
    }
    return false;
}


void IRBB::dump(Region * ru, bool dump_inner_region)
{
    if (g_tfile == NULL) { return; }

    note("\n----- BB%d ------", BB_id(this));
    if (getLabelList().get_elem_count() > 0) {
        note("\nLABEL:");
        dumpBBLabel(getLabelList(), g_tfile);
    }

    //Attributes
    note("\nATTR:");
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
    note("\nSTMT NUM:%d", getNumOfIR());
    g_indent += 3;
    TypeMgr * dm = ru->get_type_mgr();
    for (IR * ir = BB_first_ir(this);
         ir != NULL; ir = BB_irlist(this).get_next()) {
        ASSERT0(ir->is_single() && ir->get_bb() == this);
        dump_ir(ir, dm, NULL, true, true, false, dump_inner_region);
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
        ASSERT0(ir->is_single());
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

        if (is_bb_down_boundary(ir)) {
            ASSERT(ir == BB_last_ir(this), ("invalid BB down boundary."));
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
                 lpos != 0; opnd = opnd->get_next()) {
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


//Before removing bb or change bb successor,
//you need remove the related PHI operand if BB successor has PHI stmt.
void IRBB::removeSuccessorDesignatePhiOpnd(CFG<IRBB, IR> * cfg, IRBB * succ)
{
    ASSERT0(cfg && succ);
    IR_CFG * ircfg = (IR_CFG*)cfg;
    Region * ru = ircfg->get_ru();
    UINT const pos = ircfg->WhichPred(this, succ);
    for (IR * ir = BB_first_ir(succ); ir != NULL; ir = BB_next_ir(succ)) {
        if (!ir->is_phi()) { break; }

        ASSERT0(cnt_list(PHI_opnd_list(ir)) == succ->getNumOfPred(cfg));

        IR * opnd;
        UINT lpos = pos;
        for (opnd = PHI_opnd_list(ir); lpos != 0; opnd = opnd->get_next()) {
            ASSERT0(opnd);
            lpos--;
        }

        if (opnd == NULL) {
            //PHI does not contain any operand.
            continue;
        }

        opnd->removeSSAUse();
        ((CPhi*)ir)->removeOpnd(opnd);
        ru->freeIRTree(opnd);
    }
}


//Before removing bb or change bb successor,
//you need remove the related PHI operand if BB successor has PHI stmt.
void IRBB::removeSuccessorPhiOpnd(CFG<IRBB, IR> * cfg)
{
    Vertex * vex = cfg->get_vertex(BB_id(this));
    ASSERT0(vex);
    for (EdgeC * out = VERTEX_out_list(vex); out != NULL; out = EC_next(out)) {
        IRBB * succ = ((IR_CFG*)cfg)->get_bb(VERTEX_id(EDGE_to(EC_edge(out))));
        ASSERT0(succ);
        removeSuccessorDesignatePhiOpnd(cfg, succ);
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
            ASSERT0(LABEL_INFO_pragma(li));
            fprintf(h, "%s", SYM_name(LABEL_INFO_pragma(li)));
            break;
        default: UNREACH();
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


void dumpBBList(BBList * bbl,
                Region * ru,
                CHAR const* name,
                bool dump_inner_region)
{
    ASSERT0(ru && bbl);
    FILE * h = NULL;
    FILE * org_g_tfile = g_tfile;
    if (name == NULL) {
        h = g_tfile;
    } else {
        UNLINK(name);
        h = fopen(name, "a+");
        ASSERT(h != NULL, ("can not dump."));
        g_tfile = h;
    }

    if (h != NULL && bbl->get_elem_count() != 0) {
        if (h == g_tfile) {
            note("\n==---- DUMP '%s' BBList ----==", ru->get_ru_name());
        } else {
            fprintf(h, "\n==---- DUMP '%s' BBList ----==", ru->get_ru_name());
        }

        for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
            bb->dump(ru, dump_inner_region);
        }
    }

    fflush(h);
    if (h != org_g_tfile) {
        fclose(h);
    }

    g_tfile = org_g_tfile;
}

} //namespace xoc
