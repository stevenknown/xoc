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
//START SortPredByBBId
//
//Only collect given BB's predecessor.
void SortPredByBBId::collectPhiOpnd2PredBB(IRBB const* bb)
{
    IRListIter it;
    for (IR * ir = const_cast<IRBB*>(bb)->getIRList().get_head(&it);
         ir != nullptr;
         ir = const_cast<IRBB*>(bb)->getIRList().get_next(&it)) {
        if (!ir->is_phi()) { return; }
        xcom::AdjVertexIter vit;
        Vertex const* in = m_cfg->get_first_in_vertex(bb->getVex(), vit);
        for (IR const* opnd = PHI_opnd_list(ir); opnd != nullptr;
             opnd = opnd->get_next(), in = m_cfg->get_next_in_vertex(vit)) {
            ASSERT0(in);
            m_phiopnd2bbid.set(opnd, in->id());
        }
        return; //only record info for the first PHI.
    }
}


//Collect all BBs in CFG.
void SortPredByBBId::collectPhiOpnd2PredBB()
{
    BBList const* bblst = m_cfg->getBBList();
    BBListIter bbit;
    for (IRBB * bb = bblst->get_head(&bbit);
         bbit != bblst->end(); bb = bblst->get_next(&bbit)) {
        collectPhiOpnd2PredBB(bb);
    }
}


//Sort the order of predecessor of given BB according to PHI operand layout.
void SortPredByBBId::sort(IRBB const* bb) const
{
    IR const* phi = const_cast<IRBB*>(bb)->getFirstIR();
    if (phi == nullptr || !phi->is_phi()) { return; }

    //Sort in-edge of bb to guarantee the order of them are same
    //with the phi-operands.
    xcom::Vertex * bbvex = bb->getVex();
    xcom::EdgeC * opnd_pred = bbvex->getInList();
    for (IR * opnd = PHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        UINT pred_bbid = m_phiopnd2bbid.get(opnd);
        ASSERT0(pred_bbid != BBID_UNDEF);
        IRBB * incoming_bb = m_cfg->getBB(pred_bbid);
        ASSERT0(incoming_bb);
        if (opnd_pred->getFromId() == incoming_bb->id()) {
            opnd_pred = opnd_pred->get_next();
            continue;
        }
        xcom::EdgeC * q;
        for (q = opnd_pred->get_next();
             q != nullptr; q = q->get_next()) {
            if (q->getFromId() == incoming_bb->id()) {
                break;
            }
        }
        ASSERTN(q, ("can not find expected in-edge for BB%d", bb->id()));
        xcom::swap(&VERTEX_in_list(bbvex), opnd_pred, q);
        opnd_pred = q->get_next();
    }
}


void SortPredByBBId::sort() const
{
    BBList const* bblst = m_cfg->getBBList();
    BBListIter bbit;
    for (IRBB * bb = bblst->get_head(&bbit);
         bbit != bblst->end(); bb = bblst->get_next(&bbit)) {
        sort(bb);
    }
}
//END SortPredByBBId


//
//START SortPredByLab
//
//Sort the order of predecessor of given BB according to PHI operand layout.
void SortPredByLab::sort(IRBB const* bb, IR2Lab const& ir2lab) const
{
    IR const* phi = const_cast<IRBB*>(bb)->getFirstIR();
    if (phi == nullptr || !phi->is_phi()) { return; }

    //Sort in-edge of bb to guarantee the order of them are same
    //with the phi-operands.
    xcom::Vertex * bbvex = bb->getVex();
    xcom::EdgeC * opnd_pred = bbvex->getInList();
    for (IR * opnd = PHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        LabelInfo const* opnd_label = ir2lab.get(opnd);
        IRBB * incoming_bb = m_cfg->findBBbyLabel(opnd_label);
        ASSERT0(incoming_bb);
        if (opnd_pred->getFromId() == incoming_bb->id()) {
            opnd_pred = opnd_pred->get_next();
            continue;
        }
        xcom::EdgeC * q;
        for (q = opnd_pred->get_next();
             q != nullptr; q = q->get_next()) {
            if (q->getFromId() == incoming_bb->id()) {
                break;
            }
        }
        ASSERTN(q, ("can not find expected in-edge for BB%d", bb->id()));
        xcom::swap(&VERTEX_in_list(bbvex), opnd_pred, q);
        opnd_pred = q->get_next();
    }
}


void SortPredByLab::sort(IR2Lab const& ir2lab) const
{
    BBList const* bblst = m_cfg->getBBList();
    BBListIter bbit;
    for (IRBB * bb = bblst->get_head(&bbit);
         bbit != bblst->end(); bb = bblst->get_next(&bbit)) {
        sort(bb, ir2lab);
    }
    ASSERT0(m_cfg->verifyPhiEdge(ir2lab));
}
//END SortPredByLab

} //namespace xoc
