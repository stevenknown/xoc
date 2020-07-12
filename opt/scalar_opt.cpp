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

bool ScalarOpt::perform(OptCtx & oc)
{
    TTab<Pass*> opt_tab;
    List<Pass*> passlist;
    SimpCtx simp;
    if (g_do_gvn) { m_pass_mgr->registerPass(PASS_GVN); }

    if (g_do_pre) {
        //Do PRE individually.
        //Since it will incur the opposite effect with Copy-Propagation.
        Pass * pre = m_pass_mgr->registerPass(PASS_PRE);
        pre->perform(oc);
        ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    }

    if (g_do_dce) {
        DeadCodeElim * dce = (DeadCodeElim*)m_pass_mgr->registerPass(PASS_DCE);
        passlist.append_tail(dce);
        if (g_do_dce_aggressive) {
            dce->set_elim_cfs(true);
        }
    }

    bool in_ssa_form = false;
    PRSSAMgr * ssamgr = (PRSSAMgr*)m_pass_mgr->queryPass(PASS_PR_SSA_MGR);
    if (ssamgr != NULL && ssamgr->is_valid()) {
        in_ssa_form = true;
    }

    if (!in_ssa_form) {
        //RP can reduce the memory operations and
        //improve the effect of PR SSA, so perform
        //RP before SSA construction.
        //TODO: Do SSA renaming when after register promotion done.
        if (g_do_rp) {
            //First RP.
            passlist.append_tail(m_pass_mgr->registerPass(PASS_RP));
        }
    }

    if (g_do_cp) {
        CopyProp * pass = (CopyProp*)m_pass_mgr->registerPass(PASS_CP);
        pass->setPropagationKind(CP_PROP_SIMPLEX);
        passlist.append_tail(pass);
    }

    if (g_do_rp) {
        //Second RP.
        passlist.append_tail(m_pass_mgr->registerPass(PASS_RP));
    }

    if (g_do_gcse) {
        passlist.append_tail(m_pass_mgr->registerPass(PASS_GCSE));
    }

    if (g_do_lcse) {
        passlist.append_tail(m_pass_mgr->registerPass(PASS_LCSE));
    }

    if (g_do_rce) {
        passlist.append_tail(m_pass_mgr->registerPass(PASS_RCE));
    }

    if (g_do_dse) {
        passlist.append_tail(m_pass_mgr->registerPass(PASS_DSE));
    }

    if (g_do_licm) {
        passlist.append_tail(m_pass_mgr->registerPass(PASS_LICM));
    }

    if (g_do_ivr) {
        passlist.append_tail(m_pass_mgr->registerPass(PASS_IVR));
    }

    if (g_do_loop_convert) {
        passlist.append_tail(m_pass_mgr->registerPass(PASS_LOOP_CVT));
    }

    bool res = false;
    bool change;
    UINT count = 0;
    BBList * bbl = m_rg->getBBList();
    IRCFG * cfg = m_rg->getCFG();
    DUMMYUSE(cfg);
    Refine * refine = (Refine*)m_rg->getPassMgr()->registerPass(PASS_REFINE);
    do {
        change = false;
        for (Pass * pass = passlist.get_head();
             pass != NULL; pass = passlist.get_next()) {
            ASSERT0(verifyIRandBB(bbl, m_rg));
            bool doit = pass->perform(oc);
            if (doit) {
                change = true;
                ASSERT0(verifyIRandBB(bbl, m_rg));
                ASSERT0(cfg->verify());
            }
            RefineCtx rc;
            refine->refineBBlist(bbl, rc, oc);
            ASSERT0(m_rg->verifyRPO(oc));
        }
        count++;
        res |= change;
    } while (change && count < 20);
    ASSERT0(!change);

    if (g_do_lcse) {
        LCSE * lcse = (LCSE*)m_pass_mgr->registerPass(PASS_LCSE);
        lcse->set_enable_filter(false);
        res |= lcse->perform(oc);
    }

    if (g_do_rp) {
        RegPromot * r = (RegPromot*)m_pass_mgr->registerPass(PASS_RP);
        res |= r->perform(oc);
    }
    return res;
}

} //namespace xoc
