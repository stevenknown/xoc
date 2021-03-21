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
#include "comopt.h"

namespace xoc {
//
//START BBIRList
//
//Insert ir prior to cond_br, uncond_br, call, return.
C<IR*> * BBIRList::append_tail_ex(IR * ir)
{
    if (ir == nullptr) { return nullptr; }

    IRListIter ct;
    for (List<IR*>::get_tail(&ct);
         ct != List<IR*>::end(); ct = List<IR*>::get_prev(ct)) {
        if (!IRBB::isLowerBoundary(ct->val())) {
            break;
        }
    }

    ASSERT0(m_bb);
    ir->setBB(m_bb);
    if (ct == nullptr) {
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
bool IRBB::isLowerBoundary(IR const* ir)
{
    ASSERTN(ir->isStmtInBB() || ir->is_lab(), ("illegal stmt in bb"));
    switch (ir->getCode()) {
    case IR_CALL:
    case IR_ICALL: //indirective call
        return ((CCall*)ir)->isMustBBbound();
    case IR_GOTO:
    case IR_IGOTO:
        return true;
    case IR_SWITCH:
        ASSERTN(SWITCH_body(ir) == nullptr,
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


//Return true if BB has a fall through successor.
//Note conditional branch always has fallthrough successor.
bool IRBB::is_fallthrough() const
{
    IR * last = const_cast<IRBB*>(this)->getLastIR();
    if (last == nullptr) { return true; }
    switch (last->getCode()) {
    case IR_CALL:
    case IR_ICALL:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SWITCH:
        return true;
    case IR_IGOTO:
    case IR_GOTO:
        //We have no knowledge about whether target BB of GOTO/IGOTO
        //will be followed subsequently on current BB.
        //Leave this problem to CFG builder, and the related
        //attribute should be set at that time.
        return false;
    case IR_RETURN:
        //Succeed stmt of 'ir' may be DEAD code
        //IR_BB_is_func_exit(cur_bb) = true;
        return true;
    case IR_REGION:
        return true;
    default: ;
    }
    return false;
}


void IRBB::dump(Region const* rg, bool dump_inner_region) const
{
    if (!rg->isLogMgrInit()) { return; }

    note(rg, "\n----- BB%d --- rpo:%d -----", id(), rpo());
    IRBB * pthis = const_cast<IRBB*>(this);
    if (pthis->getLabelList().get_elem_count() > 0) {
        note(rg, "\nLABEL:");
        dumpBBLabel(pthis->getLabelList(), rg);
    }

    //Attributes
    note(rg, "\nATTR:");
    if (is_entry()) {
        prt(rg, "entry_bb ");
    }

    //if (BB_is_exit(this)) {
    //    prt(rg, "exit_bb ");
    //}

    if (is_fallthrough()) {
        prt(rg, "fall_through ");
    }

    if (is_target()) {
        prt(rg, "branch_target ");
    }

    //IR list
    note(rg, "\nSTMT NUM:%d", getNumOfIR());
    rg->getLogMgr()->incIndent(3);
    for (IR * ir = BB_first_ir(pthis);
         ir != nullptr; ir = BB_irlist(pthis).get_next()) {
        ASSERT0(ir->is_single() && ir->getBB() == this);
        dumpIR(ir, rg, nullptr, IR_DUMP_KID | IR_DUMP_SRC_LINE |
               (dump_inner_region ? IR_DUMP_INNER_REGION : 0));
    }
    rg->getLogMgr()->decIndent(3);
    note(rg, "\n");
}


//Check that all basic blocks should only end with terminator IR.
void IRBB::verify()
{
    UINT c = 0;
    IRListIter ct;
    for (IR * ir = BB_irlist(this).get_head(&ct);
         ir != nullptr; ir = BB_irlist(this).get_next(&ct)) {
        ASSERT0(ir->is_single());
        ASSERT0(ir->getBB() == this);
        ASSERT0(ir->isStmtInBB());
        if (IRBB::isLowerBoundary(ir)) {
            ASSERTN(ir == BB_last_ir(this), ("invalid BB down boundary."));
        }
        c++;
    }
    ASSERT0(c == getNumOfIR());
}


//Return true if one of bb's successor has a phi.
bool IRBB::successorHasPhi(CFG<IRBB, IR> * cfg)
{
    xcom::Vertex * vex = cfg->getVertex(id());
    ASSERT0(vex);
    for (xcom::EdgeC * out = vex->getOutList();
         out != nullptr; out = out->get_next()) {
        xcom::Vertex * succ_vex = out->getTo();
        IRBB * succ = cfg->getBB(succ_vex->id());
        ASSERT0(succ);
        for (IR * ir = BB_first_ir(succ);
             ir != nullptr; ir = BB_next_ir(succ)) {
            if (ir->is_phi()) { return true; }
        }
    }
    return false;
}


//Duplicate and add an operand that indicated by opnd_pos at phi stmt
//in one of bb's successors.
void IRBB::dupSuccessorPhiOpnd(CFG<IRBB, IR> * cfg, Region * rg, UINT opnd_pos)
{
    IRCFG * ircfg = (IRCFG*)cfg;
    xcom::Vertex * vex = ircfg->getVertex(id());
    ASSERT0(vex);
    for (xcom::EdgeC * out = vex->getOutList();
         out != nullptr; out = out->get_next()) {
        xcom::Vertex * succ_vex = out->getTo();
        IRBB * succ = ircfg->getBB(succ_vex->id());
        ASSERT0(succ);

        for (IR * ir = BB_first_ir(succ);
             ir != nullptr; ir = BB_next_ir(succ)) {
            if (!ir->is_phi()) { break; }

            ASSERT0(xcom::cnt_list(PHI_opnd_list(ir)) >= opnd_pos);

            IR * opnd;
            UINT lpos = opnd_pos;
            for (opnd = PHI_opnd_list(ir);
                 lpos != 0; opnd = opnd->get_next()) {
                ASSERT0(opnd);
                lpos--;
            }

            IR * newopnd = rg->dupIRTree(opnd);
            if (opnd->isReadPR()) {
                //TO BE REMOVED, should have been copied in dupIR
                //newopnd->copyRef(opnd, rg);
                ASSERT0(PR_ssainfo(opnd));
                PR_ssainfo(newopnd) = PR_ssainfo(opnd);
                SSA_uses(PR_ssainfo(newopnd)).append(newopnd);
            }

            ((CPhi*)ir)->addOpnd(newopnd);
        }
    }
}


UINT IRBB::getNumOfPred(CFG<IRBB, IR> * cfg) const
{
    ASSERT0(cfg);
    xcom::Vertex const* vex = cfg->getVertex(id());
    ASSERT0(vex);
    UINT n = 0;
    for (xcom::EdgeC const* in = VERTEX_in_list(vex);
         in != nullptr; in = EC_next(in), n++);
    return n;
}


UINT IRBB::getNumOfSucc(CFG<IRBB, IR> * cfg) const
{
    ASSERT0(cfg);
    xcom::Vertex const* vex = cfg->getVertex(id());
    ASSERT0(vex);
    UINT n = 0;
    for (xcom::EdgeC const* out = VERTEX_out_list(vex);
         out != nullptr; out = EC_next(out), n++);
    return n;
}
//END IRBB


//Before removing BB or change BB successor,
//you need remove the related PHI operand if BB successor has PHI stmt.
void IRBB::removeSuccessorDesignatePhiOpnd(CFG<IRBB, IR> * cfg, IRBB * succ)
{
    IRCFG * ircfg = (IRCFG*)cfg;
    PRSSAMgr * prssamgr = (PRSSAMgr*)ircfg->getRegion()->getPassMgr()->
        queryPass(PASS_PR_SSA_MGR);
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        prssamgr->removeSuccessorDesignatePhiOpnd(this, succ);
    }

    MDSSAMgr * mdssamgr = (MDSSAMgr*)ircfg->getRegion()->getPassMgr()->
        queryPass(PASS_MD_SSA_MGR);
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->removeSuccessorDesignatePhiOpnd(this, succ);
    }
}


//After adding BB or change bb successor,
//you need add the related PHI operand if BB successor has PHI stmt.
void IRBB::addSuccessorDesignatePhiOpnd(CFG<IRBB, IR> * cfg, IRBB * succ)
{
    IRCFG * ircfg = (IRCFG*)cfg;
    PRSSAMgr * prssamgr = (PRSSAMgr*)ircfg->getRegion()->getPassMgr()->
        queryPass(PASS_PR_SSA_MGR);
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        prssamgr->addSuccessorDesignatePhiOpnd(this, succ);        
    }

    MDSSAMgr * mdssamgr = (MDSSAMgr*)ircfg->getRegion()->getPassMgr()->
        queryPass(PASS_MD_SSA_MGR);
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->addSuccessorDesignatePhiOpnd(this, succ);
    }
}


//Before removing current BB or change BB's successor,
//you need remove the related PHI operand if BB successor has PHI.
void IRBB::removeAllSuccessorsPhiOpnd(CFG<IRBB, IR> * cfg)
{
    xcom::Vertex * vex = cfg->getVertex(id());
    ASSERT0(vex);
    for (xcom::EdgeC * out = vex->getOutList();
         out != nullptr; out = EC_next(out)) {
        IRBB * succ = ((IRCFG*)cfg)->getBB(out->getToId());
        ASSERT0(succ);
        removeSuccessorDesignatePhiOpnd(cfg, succ);
    }
}
//END IRBB


void dumpBBLabel(List<LabelInfo const*> & lablist, Region const* rg)
{
    FILE * h = rg->getLogMgr()->getFileHandler();
    ASSERT0(h);
    xcom::C<LabelInfo const*> * ct;
    for (lablist.get_head(&ct); ct != lablist.end();
         ct = lablist.get_next(ct)) {
        LabelInfo const* li = ct->val();
        switch (LABELINFO_type(li)) {
        case L_CLABEL:
            fprintf(h, CLABEL_STR_FORMAT, CLABEL_CONT(li));
            break;
        case L_ILABEL:
            fprintf(h, ILABEL_STR_FORMAT, ILABEL_CONT(li));
            break;
        case L_PRAGMA:
            ASSERT0(LABELINFO_pragma(li));
            fprintf(h, "%s", SYM_name(LABELINFO_pragma(li)));
            break;
        default: UNREACHABLE();
        }

        if (LABELINFO_is_try_start(li) ||
            LABELINFO_is_try_end(li) ||
            LABELINFO_is_catch_start(li) ||
            LABELINFO_is_terminate(li)) {
            prt(rg, "(");
            if (LABELINFO_is_try_start(li)) {
                prt(rg, "try_start,");
            }
            if (LABELINFO_is_try_end(li)) {
                prt(rg, "try_end,");
            }
            if (LABELINFO_is_catch_start(li)) {
                prt(rg, "catch_start,");
            }
            if (LABELINFO_is_terminate(li)) {
                prt(rg, "terminate");
            }
            prt(rg, ")");
        }
        prt(rg, " ");
    }
}


void dumpBBList(BBList const* bbl,
                Region const* rg,
                bool dump_inner_region)
{
    ASSERT0(rg && bbl);
    if (!rg->isLogMgrInit()) { return; }
    if (bbl->get_elem_count() != 0) {
        note(rg, "\n==---- DUMP IRBBList '%s' ----==", rg->getRegionName());
        C<IRBB*> * ct = nullptr;
        for (IRBB * bb = bbl->get_head(&ct);
             bb != nullptr; bb = bbl->get_next(&ct)) {
            bb->dump(rg, dump_inner_region);
        }
    }
}

} //namespace xoc
