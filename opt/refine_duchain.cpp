/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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


//This function try to require VN of base of ir.
//Return the VN if found, and the indirect operation level.
//e.g: given ILD(ILD(p)), return p and ist_star_level is 2.
//e.g2: given IST(ILD(q)), return q and ist_star_level is 2.
VN const* RefineDUChain::getVNOfIndirectOp(IR const* ir, UINT * indirect_level)
{
    ASSERT0(ir && (ir->is_ild() || ir->is_ist()));
    ASSERT0(m_gvn);
    IR const* base = ir;
    *indirect_level = 1;
    for (; base != NULL && !base->is_ild() && !base->is_ist();
         base = base->is_ild() ? ILD_base(base) : IST_base(base)) {
        *indirect_level++;
    }
    ASSERT0(base);

    //Get the VN of base expression.
    return const_cast<GVN*>(m_gvn)->mapIR2VN(base);
}


//Return true if DU chain changed.
bool RefineDUChain::processBB(IRBB const* bb)
{
    ASSERT0(bb);
    C<IR*> * ct = NULL;
    ConstIRIter ii;
    bool change = false;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != NULL; ir = BB_irlist(bb).get_next(&ct)) {
        ii.clean();
        for (IR const* exp = iterRhsInitC(ir, ii);
             exp != NULL; exp = iterRhsNextC(ii)) {
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


//Return true if DU chain changed.
bool RefineDUChain::processExpressionViaMDSSA(IR const* exp)
{
    ASSERT0(exp && exp->is_ild());
    MDSSAInfo * mdssainfo = m_mdssamgr->getMDSSAInfoIfAny(exp);
    if (mdssainfo == NULL) { return false; }

    //Iterate each VOpnd.
    VOpndSetIter iter = NULL;
    INT next_i = -1;
    bool change = false;
    for (INT i = mdssainfo->readVOpndSet()->get_first(&iter);
         i >= 0; i = next_i) {
        next_i = mdssainfo->readVOpndSet()->get_next(i, &iter);
        VMD const* t = (VMD const*)m_mdssamgr->getUseDefMgr()->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->getDef() == NULL) {
            ASSERTN(t->version() == 0, ("Only zero version MD has no DEF"));
            continue;
        }
        if (t->getDef()->is_phi()) {
            //TODO: Do PHI transferring to handle more cases.
            continue;
        }
        IR const* defstmt = t->getDef()->getOcc();
        ASSERT0(defstmt);
        if (exp->isNotOverlap(defstmt, m_rg)) {
            //Remove DU chain if we can guarantee ILD and its DEF stmt
            //are independent.
            //mdssainfo->getVOpndSet()->remove(t, *m_sbs_mgr);
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
    //find out 'p'.
    UINT ild_star_level = 0;
    VN const* vn_of_ild_base = getVNOfIndirectOp(exp, &ild_star_level);
    if (vn_of_ild_base == NULL) {
        //It is no need to analyze DEF set with have no VN.
        return false;
    }

    DUSet const* defset = exp->getDUSet();
    if (defset == NULL) { return false; }

    //Iterate each DEF stmt of 'exp'.
    bool change = false;
    DUIter di = NULL;
    INT next_i = -1;
    for (INT i = defset->get_first(&di); i >= 0; i = next_i) {
        next_i = defset->get_next(i, &di);
        IR const* stmt = m_rg->getIR(i);
        ASSERT0(stmt->is_stmt());
        //Only deal with IR_IST.
        if (!stmt->is_ist()) { continue; }

        //Get VN of IST's base expression.
        UINT ist_star_level = 0;
        VN const* vn_of_ist_base = getVNOfIndirectOp(stmt, &ist_star_level);
        if (vn_of_ist_base == NULL) {
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
    C<IRBB*> * ctbb = NULL;
    bool change = false;
    for (IRBB * bb = bbl->get_head(&ctbb);
         bb != NULL; bb = bbl->get_next(&ctbb)) {
        change |= processBB(bb);
    }
    return change;
}


bool RefineDUChain::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == NULL || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_cfg_valid()) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_MD_SSA_MGR);
    m_prssamgr = (PRSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_PR_SSA_MGR);
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
        if (m_gvn == NULL || !m_gvn->is_valid()) {
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
