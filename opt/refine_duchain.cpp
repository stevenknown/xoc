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
        m_rg->getLogMgr()->incIndent(2);
        m_du->dumpDUChainDetail();
        m_rg->getLogMgr()->decIndent(2);
    } else {
        ASSERT0(m_mdssamgr);
        m_rg->getLogMgr()->incIndent(2);
        m_mdssamgr->dumpDUChain();
        m_rg->getLogMgr()->decIndent(2);
    }
    return Pass::dump();
}


bool RefineDUChain::processArrayExp(IR const* exp)
{
    ASSERT0(exp->is_exp() && exp->isArrayOp());
    if (m_is_use_gvn) {
        return processArrayExpViaGVN(exp);
    }
    //Use MDSSA.
    return processExpViaMDSSA(exp);
}


bool RefineDUChain::processNormalExpByClassicDU(IR const* exp)
{
    if (!exp->isMemRefNonPR()) { return false; }
    MD const* mustuse = exp->getMustRef();
    if (mustuse == nullptr || !mustuse->is_local() ||
        mustuse->is_taken_addr()) {
        return false;
    }
    //CASE:If mustref did NOT be taken address, it should not dependent to
    //MD_LOCAL_MAY_ALIAS.
    DUSet const* defset = exp->getDUSet();
    if (defset == nullptr) { return false; }

    //Iterate each DEF stmt of 'exp'.
    bool change = false;
    DUSetIter di = nullptr;
    BSIdx next_i = BS_UNDEF;
    for (BSIdx i = defset->get_first(&di); i != BS_UNDEF; i = next_i) {
        next_i = defset->get_next(i, &di);
        IR const* stmt = m_rg->getIR(i);
        ASSERT0(stmt->is_stmt());
        MD const* mustdef = stmt->getMustRef();
        if (mustdef != nullptr) {
            ASSERTN(mustdef->is_overlap(mustuse), ("redundant DU"));
            continue;
        }
        MDSet const* maydef = stmt->getMayRef();
        if (maydef != nullptr && maydef->is_contain(mustuse, m_rg)) {
            continue;
        }
        m_du->removeDUChain(stmt, exp);
        change = true;
    }
    return change;
}


bool RefineDUChain::processNormalExpByMDSSA(IR const* exp)
{
    if (!exp->isMemRefNonPR()) { return false; }
    MD const* must = exp->getMustRef();
    if (must == nullptr || !must->is_local()) { return false; }
    //TODO:
    return false;
}


bool RefineDUChain::processNormalExp(IR const* exp)
{
    if (useMDSSADU()) {
        return processNormalExpByMDSSA(exp);
    }
    return processNormalExpByClassicDU(exp);
}


bool RefineDUChain::processIndirectExp(IR const* exp)
{
    ASSERT0(exp->is_exp() && exp->isIndirectMemOp());
    if (m_is_use_gvn) {
        return processIndirectExpViaGVN(exp);
    }
    return processExpViaMDSSA(exp);
}


//Return true if DU chain changed.
bool RefineDUChain::processBB(IRBB const* bb)
{
    ASSERT0(bb);
    BBIRListIter ct = nullptr;
    ConstIRIter ii;
    bool change = false;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != nullptr; ir = BB_irlist(bb).get_next(&ct)) {
        ii.clean();
        for (IR const* exp = iterExpInitC(ir, ii);
             exp != nullptr; exp = iterExpNextC(ii)) {
            if (exp->isIndirectMemOp()) {
                change |= processIndirectExp(exp);
                if (!change) {
                    change |= processNormalExp(exp);
                }
                continue;
            }
            if (exp->is_array()) {
                change |= processArrayExp(exp);
                if (!change) {
                    change |= processNormalExp(exp);
                }
                continue;
            }
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
bool RefineDUChain::processExpViaMDSSA(IR const* exp)
{
    ASSERT0(exp);
    MDSSAInfo * mdssainfo = m_mdssamgr->getMDSSAInfoIfAny(exp);
    if (mdssainfo == nullptr) { return false; }

    //Iterate each VOpnd.
    VOpndSetIter iter = nullptr;
    BSIdx next_i = BS_UNDEF;
    bool change = false;
    for (BSIdx i = mdssainfo->readVOpndSet().get_first(&iter);
         i != BS_UNDEF; i = next_i) {
        next_i = mdssainfo->readVOpndSet().get_next(i, &iter);
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
            next_i = mdssainfo->readVOpndSet().get_first(&iter);
            change = true;
        }
    }
    return change;
}


//Return true if DU chain changed.
bool RefineDUChain::processArrayExpViaGVN(IR const* exp)
{
    //TODO:remove DUChain if subexp's GVN is not the same.
    return false;
}


//Return true if DU chain changed.
bool RefineDUChain::processIndirectExpViaGVN(IR const* exp)
{
    ASSERT0(exp->is_exp() && exp->isIndirectMemOp());
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
    BSIdx next_i = BS_UNDEF;
    for (BSIdx i = defset->get_first(&di); i != BS_UNDEF; i = next_i) {
        next_i = defset->get_next(i, &di);
        IR const* stmt = m_rg->getIR(i);
        ASSERT0(stmt->is_stmt());
        //Only deal with IR_IST.
        if (!stmt->is_ist()) { continue; }
        if (m_tm->getByteSize(exp->getType()) != 1 ||
            m_tm->getByteSize(stmt->getType()) != 1) {
            //ist and ild may be overlapp.
            //e.g:refine_duchain2.c
            // ist(p): |----|
            // ild(q):  |----|
            //  even if p and q have different VN, the ist and ild are
            //  dependent.
            continue;
        }

        //Get VN of IST's base expression.
        UINT ist_star_level = 0;
        VN const* vn_of_ist_base = getVNOfIndirectOp(stmt, &ist_star_level,
                                                     m_gvn);
        if (vn_of_ist_base == nullptr) {
            //No need to analyze DEF stmt with have no VN.
            continue;
        }
        if (ild_star_level != ist_star_level) {
            //Indirect level is not match.
            continue;
        }
        if (vn_of_ist_base == vn_of_ild_base) {
            //VN is same, accessing same place.
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
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRefineDUChain()) {
        dump();
    }

    //This pass does not affect IR stmt/exp, but DU chain may be changed.
    return change;
}

} //namespace xoc
