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

//
//START InvertBrTgt
//

void InvertBrTgt::addDump(IR const* ir) const
{
    if (m_changed_irlist != nullptr) {
        m_changed_irlist->append_tail(ir);
    }
}


void InvertBrTgt::dumpInit()
{
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpInvertBrTgt()) {
        ASSERT0(m_changed_irlist == nullptr);
        m_changed_irlist = new ConstIRList();
    }
}


void InvertBrTgt::dumpFini()
{
    if (m_changed_irlist != nullptr) {
        delete m_changed_irlist;
        m_changed_irlist = nullptr;
    }
}


bool InvertBrTgt::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    ASSERT0(m_changed_irlist);
    note(getRegion(), "\n==-- CHANGED IR --==");
    for (IR const* ir = m_changed_irlist->get_head();
         ir != nullptr; ir = m_changed_irlist->get_next()) {
        dumpIR(ir, m_rg);
    }
    note(getRegion(), "\n==-- CHANGED CFG --==\n");
    m_rg->getCFG()->dumpDOT(m_rg->getLogMgr()->getFileHandler(),
                            IRCFG::DUMP_DEF);
    note(getRegion(), "\n");
    return Pass::dump();
}


bool InvertBrTgt::invertLoop(InvertBrTgt * invt, IR * br, IRBB * br_tgt,
                             IR * jmp, IRBB * jmp_tgt)
{
    //Displacement
    if (br->is_truebr()) {
        IR_code(br) = IR_FALSEBR;
    } else {
        ASSERT0(br->is_falsebr());
        IR_code(br) = IR_TRUEBR;
    }
    LabelInfo const* br_lab = BR_lab(br);
    br->setLabel(GOTO_lab(jmp));
    jmp->setLabel(br_lab);
    invt->addDump(br);
    invt->addDump(jmp);

    IRCFG * cfg = invt->getRegion()->getCFG();
    void * vi1 = cfg->getEdge(br->getBB()->id(), br_tgt->id())->info();
    void * vi2 = cfg->getEdge(jmp->getBB()->id(), jmp_tgt->id())->info();
   
    CfgOptCtx ctx(*invt->getOptCtx()); 
    cfg->removeEdge(br->getBB(), br_tgt, ctx);
    cfg->removeEdge(jmp->getBB(), jmp_tgt, ctx);

    xcom::Edge * e1 = cfg->addEdge(br->getBB(), jmp_tgt, ctx);
    xcom::Edge * e2 = cfg->addEdge(jmp->getBB(), br_tgt, ctx);

    EDGE_info(e1) = vi2;
    EDGE_info(e2) = vi1;
    return true;
}


//Return true if there is loop changed.
static bool tryInvertLoop(InvertBrTgt * invt, IRCFG * cfg, LI<IRBB> const* li)
{
    IRBB * head = li->getLoopHead();
    ASSERT0(head && head->getVex());
    UINT backedge_pred = VERTEX_UNDEF;
    UINT backedge_num = 0;
    AdjVertexIter it;
    for (Vertex const* in = Graph::get_first_in_vertex(head->getVex(), it);
         in != nullptr; in = Graph::get_next_in_vertex(it)) {
        if (li->isInsideLoop(in->id())) {
            backedge_pred = in->id();
            backedge_num++;
        }
    }
    if (backedge_num > 1) { return false; }

    ASSERT0(backedge_pred != VERTEX_UNDEF);
    IRBB * backedge_start = cfg->getBB(backedge_pred);
    ASSERT0(!backedge_start->isExceptionHandler());
    IR * jmp = cfg->get_first_xr(const_cast<IRBB*>(backedge_start));
    if (jmp == nullptr) {
        //Uncanonical loop structure.
        //e.g:compile/preheader.c, backedge_start BB is empty.
        return false;
    }
    if (!jmp->is_goto()) { return false; }
    if (!head->hasLabel(GOTO_lab(jmp))) { return false; }

    BBListIter bbct;
    cfg->getBBList()->find(const_cast<IRBB*>(backedge_start), &bbct);
    IRBB * prev = cfg->getBBList()->get_prev(&bbct);
    if (prev == nullptr || prev->isExceptionHandler() ||
        !li->isInsideLoop(prev->id())) {
        return false;
    }

    IR * br = cfg->get_last_xr(prev);
    if (br == nullptr || !br->isConditionalBr() || br->hasSideEffect(true)) {
        return false;
    }

    IRBB * br_tgt = cfg->findBBbyLabel(BR_lab(br));
    if (li->isInsideLoop(br_tgt->id())) { return false; }

    return InvertBrTgt::invertLoop(invt, br, br_tgt, jmp, head);
}


//Tranform trampoline branch.
//Return true if there is loop changed.
//Note the pass is different from what IRCFG::removeTrampolinEdge() does.
//e.g:L1:
//    ...
//    truebr L2 | falsebr L2
//    goto L1 //redundant jump for taken loop iteration.
//    ...
//    L2:st = ...
//=>
//    L1:
//    ...
//    falsebr L1 | truebr L1
//    goto L2
//    ...
//    L2:st = ...
static bool iterLoopTree(InvertBrTgt * invt, IRCFG * cfg, LI<IRBB> const* li)
{
    if (li == nullptr) { return false; }
    bool changed = false;
    for (LI<IRBB> const* tli = li; tli != nullptr; tli = tli->get_next()) {
        changed |= tryInvertLoop(invt, cfg, tli);
        changed |= iterLoopTree(invt, cfg, tli->getInnerList());
    }
    return changed;
}


bool InvertBrTgt::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_oc = &oc;
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_LOOP_INFO,
                                               PASS_UNDEF);
    if (!oc.is_loopinfo_valid()) { return false; }
    dumpInit();
    bool changed = iterLoopTree(this, m_rg->getCFG(),
                                m_rg->getCFG()->getLoopInfo());
    if (!changed) {
        dumpFini();
        END_TIMER(t, getPassName());
        return false;
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpInvertBrTgt()) {
        dump();
    }
    oc.setInvalidIfCFGChanged();
    dumpFini();
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    END_TIMER(t, getPassName());
    return true;
}
//END InvertBrTgt

} //namespace xoc
