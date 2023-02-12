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

void OptCtx::setInvalidPass(PASS_TYPE pt)
{
    Pass * pass = m_rg->getPassMgr()->queryPass(pt);
    if (pass == nullptr) { return; }
    pass->set_valid(false);
}


//Return true if given pass is valid.
bool OptCtx::isPassValid(PASS_TYPE pt) const
{
    Pass * pass = m_rg->getPassMgr()->queryPass(pt);
    if (pass == nullptr) { return false; }
    return pass->is_valid();
}


void OptCtx::dumpPass() const
{
    PassTab const& passtab = m_rg->getPassMgr()->getPassTab();
    PassTabIter tabiter;
    Pass * p;
    for (passtab.get_first(tabiter, &p);
         p != nullptr; passtab.get_next(tabiter, &p)) {
        note(m_rg, "\nPASS:%s:%s", p->getPassName(),
             p->is_valid() ? "valid":"invalid");    
    }
}


void OptCtx::dumpFlag() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(m_rg, "\nFLAG:du_ref:%s", is_ref_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:prdu:%s", is_pr_du_chain_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:nonprdu:%s",
         is_nonpr_du_chain_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:live_expr:%s",
         is_live_expr_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:reach_def:%s",
         is_reach_def_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:avail_reach_def:%s",
         is_avail_reach_def_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:cfg:%s", is_cfg_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:scc:%s", is_scc_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:aa:%s", is_aa_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:expr_tab:%s", is_expr_tab_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:dom:%s", is_dom_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:pdom:%s", is_pdom_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:rpo:%s", is_rpo_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:loopinfo:%s", is_loopinfo_valid() ? "valid":"invalid");
    note(m_rg, "\nFLAG:callgraph:%s",
         is_callgraph_valid() ? "valid":"invalid");
}


void OptCtx::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    START_TIMER(t, "OptCtx: Dump");
    note(m_rg, "\n==---- DUMP %s '%s' ----==", "OptCtx",
         m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    dumpFlag(); 
    dumpPass();
    m_rg->getLogMgr()->decIndent(2);
    END_TIMER(t, "OptCtx: Dump");
}


static void setInvalidIfCFGChangedExceptImpl(OptCtx * oc,
                                             PassTypeList & optlist)
{
    ASSERTN(optlist.get_elem_count() < 1000,
            ("too many pass queried or miss ending placeholder"));
    if (optlist.get_elem_count() == 0) { return; }
    BitSet exclude;
    C<PASS_TYPE> * it;
    for (optlist.get_head(&it); it != nullptr; optlist.get_next(&it)) {
        PASS_TYPE opty = it->val();
        if (opty == PASS_UNDEF) { continue; }
        ASSERTN(opty < PASS_NUM,
                ("You should append PASS_UNDEF to pass list."));
        exclude.bunion(opty);
    }        
    if (!exclude.is_contain(PASS_RPO)) { oc->setInvalidRPO(); }
    if (!exclude.is_contain(PASS_LOOP_INFO)) { oc->setInvalidLoopInfo(); }
    if (!exclude.is_contain(PASS_DOM)) { oc->setInvalidDom(); }
    if (!exclude.is_contain(PASS_PDOM)) { oc->setInvalidPDom(); }
    if (!exclude.is_contain(PASS_CDG)) { oc->setInvalidCDG(); }
    if (!exclude.is_contain(PASS_SCC)) { oc->setInvalidSCC(); }
}


void OptCtx::setInvalidIfCFGChangedExcept(OptCtx * oc, ...)
{
    PassTypeList optlist;
    UINT num = 0;
    va_list ptr;
    va_start(ptr, oc);
    PASS_TYPE opty = (PASS_TYPE)va_arg(ptr, UINT);
    while (opty != PASS_UNDEF && num < 1000) {
        ASSERTN(opty < PASS_NUM,
                ("You should append PASS_UNDEF to pass list."));
        optlist.append_tail(opty);
        num++;
        opty = (PASS_TYPE)va_arg(ptr, UINT);
    }
    va_end(ptr);
    setInvalidIfCFGChangedExceptImpl(oc, optlist);
}

} //namespace xoc
