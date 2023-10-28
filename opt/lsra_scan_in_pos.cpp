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
#include "targinfo_mgr.h"
#include "lifetime.h"
#include "lt_interf_graph.h"
#include "linear_scan.h"
#include "lsra_impl.h"
#include "lsra_scan_in_pos.h"
#include "lt_prio_mgr.h"
#include "lsra_scan_in_prio.h"

namespace xoc {

void ScanInPosOrder::collectUnhandledForDef(IR const* ir)
{
    IR * res = const_cast<IR*>(ir)->getResultPR();
    if (res == nullptr) { return; }
    PRNO prno = res->getPrno();
    LifeTime * lt = m_ra.getLT(prno);
    ASSERT0(lt);
    if (m_ra.hasReg(lt)) { return; }
    m_ra.addUnhandled(lt);
}


void ScanInPosOrder::collectUnhandledForUse(IR const* ir, ConstIRIter & irit)
{
    ASSERT0(ir);
    irit.clean();
    for (IR const* e = xoc::iterExpInitC(ir, irit); e != nullptr;
         e = xoc::iterExpNextC(irit)) {
        if (!e->isReadPR()) { continue; }
        PRNO prno = e->getPrno();
        ASSERT0(prno != PRNO_UNDEF);
        if (m_ra.hasReg(prno)) { continue; }
        LifeTime * lt = m_ra.getLT(prno);
        ASSERT0(lt);
        m_ra.addUnhandled(lt);
    }
}


void ScanInPosOrder::tryAssignRegForDefPos(Pos curpos, IR const* ir)
{
    ASSERT0(!m_ra.isSpillOp(ir) && !m_ra.isReloadOp(ir) && !m_ra.isMoveOp(ir));
    if (ir->isCallStmt()) {
        m_impl.splitCallerSavedLT(curpos, ir);
        m_impl.splitLinkLT(curpos, ir);
    }
    LifeTime * cand = m_impl.selectAssignDefCand(curpos, ir);
    if (cand == nullptr) {
        //No lifetime need to assign.
        return;
    }
    m_impl.tryAssignRegForIR(curpos, ir, cand);
}


void ScanInPosOrder::tryAssignRegForUsePos(Pos curpos, IR const* ir)
{
    ASSERT0(ir->is_stmt());
    ASSERT0(!m_ra.isSpillOp(ir) && !m_ra.isReloadOp(ir) && !m_ra.isMoveOp(ir));
    do {
        IR const* curir = nullptr;
        LifeTime * cand = m_impl.selectAssignUseCand(curpos, ir, &curir);
        if (cand == nullptr) {
            //No lifetime need to assign.
            return;
        }
        m_impl.tryAssignRegForIR(curpos, curir, cand);
    } while (true);
}


bool ScanInPosOrder::verifyResourceForDefPos(IR const* ir) const
{
    ASSERT0(ir && ir->is_stmt());
    IR const* res = const_cast<IR*>(ir)->getResultPR();
    if (res == nullptr) { return true; }
    PRNO prno = res->getPrno();
    ASSERT0(prno != PRNO_UNDEF);

    //Verify dedicated PR.
    LifeTime * lt = m_ra.getLT(prno);
    ASSERT0(lt);
    if (lt->is_dedicated()) {
        Reg antireg = m_ra.getDedicatedReg(prno);
        ASSERT0_DUMMYUSE(antireg != REG_UNDEF);
        ASSERT0(m_impl.getRegSetImpl().isAvailAllocable(antireg));
    }

    UINT need_newreg_num = 1; //there is only ONE result by default.
    ASSERT0_DUMMYUSE(need_newreg_num <=
        m_impl.getRegSetImpl().getAvailAllocable().get_elem_count());
    return true;
}


bool ScanInPosOrder::verifyResourceForUsePos(IR const* ir) const
{
    ASSERT0(ir && ir->is_stmt());
    UINT need_newreg_num = 0;
    ConstIRIter irit;
    xcom::TMap<PRNO, Reg> regmap;
    for (IR const* e = xoc::iterExpInitC(ir, irit); e != nullptr;
         e = xoc::iterExpNextC(irit)) {
        if (!e->isReadPR()) { continue; }
        PRNO prno = e->getPrno();
        ASSERT0(prno != PRNO_UNDEF);

        //Verify dedicated PR.
        LifeTime * lt = m_ra.getLT(prno);
        ASSERT0(lt);
        if (lt->is_dedicated()) {
            Reg antireg = m_ra.getDedicatedReg(prno);
            ASSERT0_DUMMYUSE(antireg != REG_UNDEF);
            ASSERT0(m_impl.getRegSetImpl().isAvailAllocable(antireg));
        }

        Reg r = m_ra.getReg(prno);
        if (r != REG_UNDEF) {
            //An IR tree may have multiple occurrences for same PR.
            regmap.setAlways(prno, r);
            continue;
        }
        need_newreg_num++;
    }
    UINT totalreg = need_newreg_num + regmap.get_elem_count();
    ASSERT0_DUMMYUSE(totalreg <=
        m_impl.getRegSetImpl().getAvailAllocable().get_elem_count());
    return true;
}


void ScanInPosOrder::scanRHS(IR * ir, Pos upos, ConstIRIter & irit)
{
    m_impl.transferInActive(upos);
    m_impl.transferActive(upos);
    collectUnhandledForUse(ir, irit);
    ASSERT0(verifyResourceForUsePos(ir));
    tryAssignRegForUsePos(upos, ir);
}


void ScanInPosOrder::scanLHS(IR * ir, Pos dpos)
{
    m_impl.transferInActive(dpos);
    m_impl.transferActive(dpos);
    collectUnhandledForDef(ir);
    ASSERT0(verifyResourceForDefPos(ir));
    tryAssignRegForDefPos(dpos, ir);
}


void ScanInPosOrder::scanIR(IR * ir, UpdatePos & up, ConstIRIter & irit)
{
    Pos dpos, upos;
    if (!up.updateAtIR(ir, dpos, upos)) {
        //No need to handle spill/reload. Their occ did not encoded with
        //a position and therefore not resided in any lifetime.
        return;
    }
    scanRHS(ir, upos, irit);
    scanLHS(ir, dpos);
    ASSERT0(m_ra.verify4List());
}


void ScanInPosOrder::scanIRList(BBIRList & irlst, UpdatePos & up,
                                ConstIRIter & irit)
{
    BBIRListIter bbirit;
    for (IR * ir = irlst.get_head(&bbirit);
         ir != nullptr; ir = irlst.get_next(&bbirit)) {
        scanIR(ir, up, irit);
    }
}


void ScanInPosOrder::scanBBList(BBList * bblst)
{
    ASSERT0(bblst);
    //Create entry position.
    UpdatePos up(m_ra);
    Pos dpos_start, upos_start;
    bool valid = up.updateAtRegionEntry(dpos_start, upos_start);
    ASSERT0_DUMMYUSE(valid);

    //Iterate BB list.
    BBListIter bbit;
    ConstIRIter irit;
    for (IRBB * bb = bblst->get_head(&bbit);
         bb != nullptr; bb = bblst->get_next(&bbit)) {
        //BB start position
        Pos dpos_start, upos_start;
        up.updateAtBBEntry(dpos_start, upos_start);
        BBIRList & irlst = bb->getIRList();
        scanIRList(irlst, up, irit);
        //BB end position
        Pos dpos_end, upos_end;
        up.updateAtBBExit(dpos_end, upos_end);
        m_impl.transferInActive(upos_end);
        m_impl.transferActive(upos_end);
    }
    Pos dpos_end, upos_end;
    bool valid2 = up.updateAtRegionExit(dpos_end, upos_end);
    ASSERT0_DUMMYUSE(valid2);
    m_impl.transferInActive(upos_end);
    m_impl.transferActive(upos_end);
}

} //namespace xoc
