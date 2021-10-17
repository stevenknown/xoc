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

bool RefineDUChain::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return true; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==", getPassName(),
         m_rg->getRegionName());
    if (m_is_use_gvn) {
        ASSERT0(m_du);
        m_du->dumpDUChainDetail();
    } else {
        ASSERT0(m_mdssamgr);
        m_mdssamgr->dumpDUChain();
    }
    return true;
}


//Return true if DU chain changed.
bool RefineDUChain::processBB(IRBB const* bb)
{
    ASSERT0(bb);
    C<IR*> * ct = nullptr;
    ConstIRIter ii;
    bool change = false;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != nullptr; ir = BB_irlist(bb).get_next(&ct)) {
        ii.clean();
        for (IR const* exp = iterRhsInitC(ir, ii);
             exp != nullptr; exp = iterRhsNextC(ii)) {
            if (!exp->is_ild()) { continue; }
            if (m_is_use_gvn) {
                change |= processExpressionViaGVN(exp);
                continue;
            }
            //Use MDSSA.
            change |= processExpressionViaMDSSA(exp);
        }
    }
    return change;
}


//Return true if indirect operation ir1 has same base expression with ir2.
//TODO: use gvn to utilize value flow.
bool RefineDUChain::hasSameBase(IR const* ir1, IR const* ir2)
{
    ASSERT0(ir1 && ir2);
    if (!ir1->isIndirectMemOp() || !ir2->isIndirectMemOp()) {
        //TODO: support more complex patterns.
        return false;
    }

    IR const* base1 = ir1->getBase();
    IR const* base2 = ir2->getBase();
    MD const* basemd1 = base1->getRefMD();
    MD const* basemd2 = base2->getRefMD();
    if (basemd1 == nullptr || basemd2 == nullptr) {
        //One of them is not memory object.
        //TODO: use gvn to utilize value flow.
        return false;
    }

    if (basemd1 != basemd2) {
        //TODO: the value comparison between different MD should through GVN.
        return false;
    }

    MDSSAInfo * basemdssa1 = m_mdssamgr->getMDSSAInfoIfAny(base1);
    if (basemdssa1 == nullptr) { return false; }
    MDSSAInfo * basemdssa2 = m_mdssamgr->getMDSSAInfoIfAny(base2);
    if (basemdssa2 == nullptr) { return false; }
    if (basemdssa1 == basemdssa2) {
        //MDSSAInfo is stored at AI, it may be different between IR.
        ASSERT0(basemdssa1->getVOpndForMD(basemd1->id(), m_mdssamgr));
        return true;
    }

    //Check whether the MD has different version.
    VMD const* basevmd1 = (VMD const*)basemdssa1->getVOpndForMD(basemd1->id(),
                                                                m_mdssamgr);
    if (basevmd1 == nullptr) { return false; }
    VMD const* basevmd2 = (VMD const*)basemdssa2->getVOpndForMD(basemd2->id(),
                                                                m_mdssamgr);
    if (basevmd2 == nullptr) { return false; }
    if (basevmd1 == basevmd2) { return true; }
    return false;
}


//Return true if DU chain changed.
bool RefineDUChain::processExpressionViaMDSSA(IR const* exp)
{
    ASSERT0(exp && exp->is_ild());
    MDSSAInfo * mdssainfo = m_mdssamgr->getMDSSAInfoIfAny(exp);
    if (mdssainfo == nullptr) { return false; }

    //Iterate each VOpnd.
    VOpndSetIter iter = nullptr;
    INT next_i = -1;
    bool change = false;
    for (INT i = mdssainfo->readVOpndSet()->get_first(&iter);
         i >= 0; i = next_i) {
        next_i = mdssainfo->readVOpndSet()->get_next(i, &iter);
        VMD const* t = (VMD const*)m_mdssamgr->getUseDefMgr()->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->getDef() == nullptr) {
            ASSERTN(t->version() == MDSSA_INIT_VERSION,
                    ("Only zero version MD has no DEF"));
            continue;
        }
        if (t->getDef()->is_phi()) {
            //TODO: Do PHI transferring to handle more cases.
            continue;
        }
        IR const* defstmt = t->getDef()->getOcc();
        ASSERT0(defstmt);
        if (!hasSameBase(exp, defstmt)) {
            continue;
        }

        if (exp->isNotOverlap(defstmt, m_rg)) {
            //Remove DU chain if we can guarantee ILD and its DEF stmt
            //are independent.
            m_mdssamgr->removeDUChain(defstmt, exp);

            //removeDUChain may remove VOpnd that indicated by 'next_i'.
            //We have to recompute next_i to avoid redundnant iteration.
            next_i = mdssainfo->readVOpndSet()->get_first(&iter);
            change = true;
        }
    }
    return change;
}


//Return true if DU chain changed.
bool RefineDUChain::processExpressionViaGVN(IR const* exp)
{
    ASSERT0(exp->is_exp());
    //Find the base expression that is not ILD.
    //e.g: given ILD(ILD(ILD(p))), the following loop will
    //reason out 'p'.
    UINT ild_star_level = 0;
    VN const* vn_of_ild_base = getVNOfIndirectOp(exp, &ild_star_level, m_gvn);
    if (vn_of_ild_base == nullptr) {
        //No need to analyze DEF set if there is no VN provided.
        return false;
    }

    DUSet const* defset = exp->getDUSet();
    if (defset == nullptr) { return false; }

    //Iterate each DEF stmt of 'exp'.
    bool change = false;
    DUSetIter di = nullptr;
    INT next_i = -1;
    for (INT i = defset->get_first(&di); i >= 0; i = next_i) {
        next_i = defset->get_next(i, &di);
        IR const* stmt = m_rg->getIR(i);
        ASSERT0(stmt->is_stmt());
        //Only deal with IR_IST.
        if (!stmt->is_ist()) { continue; }

        //Get VN of IST's base expression.
        UINT ist_star_level = 0;
        VN const* vn_of_ist_base = getVNOfIndirectOp(stmt, &ist_star_level,
                                                     m_gvn);
        if (vn_of_ist_base == nullptr) {
            //It is no need to analyze DEF stmt with have no VN.
            continue;
        }

        //VN is not match OR indirect level is not match
        if (vn_of_ist_base != vn_of_ild_base ||
            ild_star_level != ist_star_level) {
            continue;
        }
        m_du->removeDUChain(stmt, exp);
        change = true;
    }
    return change;
}


//Return true if DU chain changed.
bool RefineDUChain::process()
{
    BBList * bbl = m_rg->getBBList();
    C<IRBB*> * ctbb = nullptr;
    bool change = false;
    for (IRBB * bb = bbl->get_head(&ctbb);
         bb != nullptr; bb = bbl->get_next(&ctbb)) {
        change |= processBB(bb);
    }
    return change;
}


bool RefineDUChain::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_cfg_valid()) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //DCE use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //DCE use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    if (m_is_use_gvn) {
        m_gvn = (GVN const*)m_rg->getPassMgr()->queryPass(PASS_GVN);
        if (m_gvn == nullptr || !m_gvn->is_valid()) {
            return false;
        }
    } else {
        //Use MDSSA.
    }

    START_TIMER(t, getPassName());
    bool change = process();
    END_TIMER(t, getPassName());
    if (g_is_dump_after_pass && g_dump_opt.isDumpRefineDUChain()) {
        dump();
    }

    //This pass does not affect IR stmt/exp, but DU chain may be changed.
    return change;
}
//END PRE

} //namespace xoc
