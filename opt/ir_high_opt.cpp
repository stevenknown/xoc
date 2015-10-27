/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

/* Perform high-level optimizaitions.
Basis step to do:
    1. Build control flow graph.
    2. Compute POINT-TO info.
    3. Compute DEF-USE info.
    4. Compute Lived Expression info.

Optimizations to be performed:
    1. Auto Parallel
    2. Loop interchange
    3. Loop reverese(may be a little helpful)
    4. Loop tiling
    5. Loop fusion
    6. Loop unrolling */
bool Region::HighProcess(OptCTX & oc)
{
    g_indent = 0;
    note("\n\n==== Region:%s HIGHEST LEVEL FARMAT ====\n\n", get_ru_name());

    SimpCTX simp;
    if (g_do_cfs_opt) {
        IR_CFS_OPT co(this);
        co.perform(simp);
        ASSERT0(verify_irs(get_ir_list(), NULL, this));
    }

    PassMgr * passmgr = initPassMgr();
    ASSERT0(passmgr);

    if (g_build_cfs) {
        SIMP_is_record_cfs(&simp) = true;
        CfsMgr * cfsmgr = (CfsMgr*)passmgr->registerPass(PASS_CFS_MGR);
        ASSERT0(cfsmgr);
        SIMP_cfs_mgr(&simp) = cfsmgr;
    }

    simp.set_simp_cf();
    set_ir_list(simplifyStmtList(get_ir_list(), &simp));
    ASSERT0(verify_simp(get_ir_list(), simp));
    ASSERT0(verify_irs(get_ir_list(), NULL, this));

    if (g_cst_bb_list) {
        constructIRBBlist();
        ASSERT0(verifyIRandBB(get_bb_list(), this));
        set_ir_list(NULL); //All IRs have been moved to each IRBB.
    }

    if (g_do_cfg) {
        ASSERT0(g_cst_bb_list);
        IR_CFG * cfg = (IR_CFG*)passmgr->registerPass(PASS_CFG);
        ASSERT0(cfg);
        cfg->initCfg(oc);
        if (g_do_loop_ana) {
            ASSERT0(g_do_cfg_dom);
            cfg->LoopAnalysis(oc);
        }
    }

    if (g_do_ssa) {
        //Note lowering IR now may be too early and will be
        //a hindrance to optmizations.
        //low_to_pr_mode(oc);
        IR_SSA_MGR * ssamgr = (IR_SSA_MGR*)passmgr->registerPass(PASS_SSA_MGR);
        ASSERT0(ssamgr);
        ssamgr->construction(oc);
    }

    if (g_do_aa) {
        ASSERT0(g_cst_bb_list && OC_is_cfg_valid(oc));
        IR_AA * aa = (IR_AA*)passmgr->registerPass(PASS_AA);
        ASSERT0(aa);
        aa->initAliasAnalysis();
        aa->perform(oc);
    }

    if (g_do_du_ana) {
        ASSERT0(g_cst_bb_list && OC_is_cfg_valid(oc) && OC_is_aa_valid(oc));
        IR_DU_MGR * dumgr = (IR_DU_MGR*)passmgr->registerPass(PASS_DU_MGR);
        ASSERT0(dumgr);

        UINT f = SOL_REACH_DEF|SOL_REF;
        //f |= SOL_AVAIL_REACH_DEF|SOL_AVAIL_EXPR|SOL_RU_REF;

        if (g_do_ivr) {
            f |= SOL_AVAIL_REACH_DEF|SOL_AVAIL_EXPR;
        }

        if (g_do_compute_available_exp) {
            f |= SOL_AVAIL_EXPR;
        }

        dumgr->perform(oc, f);
        dumgr->computeMDDUChain(oc);
    }

    if (g_do_expr_tab) {
        ASSERT0(g_cst_bb_list);
        IR_EXPR_TAB * exprtab =
            (IR_EXPR_TAB*)passmgr->registerPass(PASS_EXPR_TAB);
        ASSERT0(exprtab);
        exprtab->perform(oc);
    }

    if (g_do_cdg) {
        ASSERT0(g_cst_bb_list && OC_is_cfg_valid(oc));
        CDG * cdg = (CDG*)passmgr->registerPass(PASS_CDG);
        ASSERT0(cdg);
        cdg->build(oc, *get_cfg());
    }

    if (g_opt_level == NO_OPT) {
        return false;
    }

    /* Regenerate high level IR, and do high level optimizations.
    Now, I get one thing: We cannot or not very easy
    construct High Level Control IR,
    (IF,DO_LOOP,...) via analysing CFG.
        e.g:

            if (i > j) { //BB1
                ...
            } else {
                return 2; //S1
            }
        BB1 does not have a ipdom, so we can not find the indispensible 3 parts:
            True body, False body, and the Sibling node.

    Solution: We can scan IF stmt first, in order to mark
    start stmt and end stmt of IF.

    //AbsNode * an = REGION_analysis_instrument(this)->m_cfs_mgr->construct_abstract_cfs();
    //Polyhedra optimization.
    //IR_POLY * poly = newPoly();
    //if (poly->construct_poly(an)) {
    //    poly->perform_poly_trans();
    //}
    //delete poly;
    */
    return true;
}

} //namespace xoc
