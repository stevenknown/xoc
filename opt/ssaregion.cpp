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
//START SSARegion
//
void SSARegion::add(PRNO prno, IR * start)
{
    IRIter it;
    for (IR * x = iterInit(start, it); x != nullptr; x = iterNext(it)) {
        if (x->isPROp() && x->getPrno() == prno) {
            add(x);
        }
    }
}


void SSARegion::add(IR * ir)
{
    ASSERT0(ir->isPROp());
    if (m_iridtab.find(ir->id())) { return; }
    m_iridtab.append(ir->id());
    m_irlist.append_tail(ir);
    if (ir->is_exp()) {
        m_bbset.bunion(ir->getStmt()->getBB()->id());
    } else {
        m_bbset.bunion(ir->getBB()->id());
    }
}


void SSARegion::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(m_rg, "\n==---- DUMP SSARegion ----==");
    ASSERT0(getRootBB());
    note(m_rg, "\nROOT:BB%d", getRootBB()->id());
    note(m_rg, "\nBBSET:");
    SSARegion * pthis = const_cast<SSARegion*>(this);
    pthis->getBBSet().dump(m_rg->getLogMgr()->getFileHandler());
    note(m_rg, "\nIRLIST:");
    m_rg->getLogMgr()->incIndent(2);
    for (IR const* ir = pthis->getIRList().get_head(); ir != nullptr;
         ir = pthis->getIRList().get_next()) {
        dumpIR(ir, m_rg, nullptr,
               DumpFlag::combineIRID(IR_DUMP_DEF|IR_DUMP_SRC_LINE));
    }
    m_rg->getLogMgr()->decIndent(2);
}


IRBB * SSARegion::findRootBB(IRBB * start)
{
    ASSERT0(start);
    while (!canBeRoot(start)) {
        xcom::Vertex const* treev = m_domtree.getVertex(start->id());
        ASSERT0(treev);
        xcom::Vertex const* idomv = m_domtree.getParent(treev);
        if (idomv != nullptr) {
            start = m_rg->getBB(idomv->id());
            ASSERT0(start);
            continue;
        }
        return nullptr;
    }
    return start;
}


bool SSARegion::isAllPredInRegion(IRBB const* bb) const
{
    xcom::Vertex const* bbv = bb->getVex();
    ASSERT0(bbv);
    xcom::AdjVertexIter itv;
    for (xcom::Vertex const* pred = Graph::get_first_in_vertex(bbv, itv);
         pred != nullptr; pred = Graph::get_next_in_vertex(itv)) {
        //If bb has PHI, its predecessor must have to be in SSA
        //region too, because the rename of operand of PHI start from its
        //predecessors.
        if (!isInRegion(pred->id())) { return false; }
    }
    return true;
}


bool SSARegion::canBeRoot(IRBB const* bb) const
{
    ASSERT0(bb);
    if (!bb->hasPRPhi()) { return true; }
    return isAllPredInRegion(bb);
}


void SSARegion::addAllBBUnderRoot()
{
    IRBB const* root = getRootBB();
    ASSERT0(root && root->getVex());
    xcom::GraphIterOut iterout(m_domtree, m_domtree.getVertex(root->id()));
    for (Vertex const* t = iterout.get_first();
         t != nullptr; t = iterout.get_next(t)) {
        add(t->id());
    }
}


void SSARegion::addPredBBTillRoot(IRBB const* start)
{
    IRBB const* root = getRootBB();
    ASSERT0(root && root->getVex());
    ASSERT0(m_cfg);
    xcom::GraphIterIn iterin(*m_cfg, start->getVex(), root->getVex());
    for (Vertex const* t = iterin.get_first();
         t != nullptr; t = iterin.get_next(t)) {
        if (isInRegion(t->id())) { continue; }
        ASSERTN(m_cfg->is_dom(root->id(), t->id()),
                ("root must dominate all other vertex in SSA region"));
        add(t->id());
    }
}


void SSARegion::inferAndAddRelatedBB()
{
    BBSetIter bbit;
    for (BSIdx i = getBBSet().get_first(&bbit);
         i != BS_UNDEF; i = getBBSet().get_next(i, &bbit)) {
        IRBB const* bb = m_rg->getBB(i);
        ASSERT0(bb);
        if (!bb->hasPRPhi()) { continue; }
        addPredBBTillRoot(bb);
    }
}


bool SSARegion::verifyRootDom() const
{
    SSARegion * pthis = const_cast<SSARegion*>(this);
    //Root must dominate all other BBs.
    IRBB const* root = getRootBB();
    ASSERT0(canBeRoot(root));
    ASSERT0(root && root->getVex());
    DomTree const& domtree = getDomTree();
    BBSetIter bbit;
    for (BSIdx i = pthis->getBBSet().get_first(&bbit);
         i != BS_UNDEF; i = pthis->getBBSet().get_next(i, &bbit)) {
        IRBB const* bb = m_rg->getBB(i);
        ASSERT0(bb);
        if (bb == root) { continue; }
        ASSERT0(domtree.is_dom(root->getVex(), bb->getVex()));
    }
    return true;
}


bool SSARegion::verify() const
{
    ASSERT0(getRootBB());
    ASSERT0(getDomTree().isVertex(getRootBB()->id()));
    ASSERTN(getOptCtx()->is_dom_valid(), ("DfMgr need domset info"));
    ASSERT0(verifyRootDom());
    return true;
}
//END SSARegion

} //namespace xoc
