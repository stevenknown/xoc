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

bool RefineDUChain::dump()
{
    if (g_tfile == NULL) { return true; }
    note("\n==---- DUMP REFINE_DUCHAIN ----==\n");
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
    return const_cast<IR_GVN*>(m_gvn)->mapIR2VN(base);
}


void RefineDUChain::processBB(IRBB const* bb)
{
    ASSERT0(bb);
    C<IR*> * ct = NULL;
    ConstIRIter ii;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != NULL; ir = BB_irlist(bb).get_next(&ct)) {
        ii.clean();    
        for (IR const* exp = iterRhsInitC(ir, ii);
             exp != NULL; exp = iterRhsNextC(ii)) {
            if (!exp->is_ild()) { continue; }
            if (m_is_use_gvn) {
                processExpressionViaGVN(exp);
                continue;
            }
            //Use MDSSA.
            processExpressionViaMDSSA(exp);
        }
    }
}


void RefineDUChain::processExpressionViaMDSSA(IR const* exp)
{
    ASSERT0(exp && exp->is_ild());
    MDSSAInfo * mdssainfo = m_mdssamgr->getMDSSAInfoIfAny(exp);
    if (mdssainfo == NULL) { return; }

    //Iterate each VOpnd.
    SEGIter * iter = NULL;
    INT next_i = -1;
    for (INT i = mdssainfo->readVOpndSet()->get_first(&iter);
         i >= 0; i = next_i) {
        next_i = mdssainfo->readVOpndSet()->get_next(i, &iter);
        VMD const* t = (VMD const*)m_mdssamgr->getUseDefMgr()->getVOpnd(i);
        ASSERT0(t && t->is_md());
        IR const* defstmt = t->getDef()->getOcc();
        ASSERT0(defstmt);
        if (exp->isNotOverLap(defstmt, m_rg)) {
            //Remove DU chain if we can guarantee ILD and its DEF stmt
            //are independent.
            //mdssainfo->getVOpndSet()->remove(t, *m_sbs_mgr);
            m_mdssamgr->removeDUChain(defstmt, exp);

            //removeDUChain may remove VOpnd that indicated by 'next_i'.
            //We have to recompute next_i to avoid redundnant iteration.
            next_i = mdssainfo->readVOpndSet()->get_first(&iter);
        }
    }
}


void RefineDUChain::processExpressionViaGVN(IR const* exp)
{
    ASSERT0(exp->is_exp());
    //Find the base expression that is not ILD.
    //e.g: given ILD(ILD(ILD(p))), the following loop will
    //find out 'p'.
    UINT ild_star_level = 0;
    VN const* vn_of_ild_base = getVNOfIndirectOp(exp, &ild_star_level);
    if (vn_of_ild_base == NULL) {
        //It is no need to analyze DEF set with have no VN.
        return;
    }
    
    DUSet const* defset = exp->getDUSet();
    if (defset == NULL) { return; }

    //Iterate each DEF stmt of 'exp'.            
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
    }          
}


void RefineDUChain::process()
{
    BBList * bbl = m_rg->getBBList();
    C<IRBB*> * ctbb = NULL;
    for (IRBB * bb = bbl->get_head(&ctbb);
         bb != NULL; bb = bbl->get_next(&ctbb)) {
        processBB(bb);
    }
}


bool RefineDUChain::perform(OptCtx & oc)
{    
    if (m_is_use_gvn) {
        if (!OC_is_du_chain_valid(oc)) {
            return false;
        }
        m_gvn = (IR_GVN const*)m_rg->getPassMgr()->queryPass(PASS_GVN);
        if (m_gvn == NULL || !m_gvn->is_valid()) {
            return false;
        }
    } else {
        //Use MDSSA.
        m_mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->
            queryPass(PASS_MD_SSA_MGR));
        if (m_mdssamgr == NULL || !m_mdssamgr->isMDSSAConstructed()) {
            return false;
        }
    }
    START_TIMER(t, getPassName());
    process();
    END_TIMER(t, getPassName());
    if (g_is_dump_after_pass && g_dump_opt.isDumpRefineDUChain()) {
        dump();
    }
    //This pass does not affect IR stmt/exp.
    return false;
}
//END IR_PRE

} //namespace xoc
