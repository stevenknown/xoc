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

void Region::HighProcessImpl(OptCtx & oc)
{
    if (g_do_cfg) {
        ASSERT0(g_cst_bb_list);
        getPassMgr()->checkValidAndRecompute(&oc, PASS_CFG, PASS_UNDEF);
        //Remove empty bb when cfg rebuilted because
        //rebuilding cfg may generate redundant empty bb.
        //It disturbs the computation of entry and exit.
        getCFG()->removeEmptyBB(oc);

        //Compute exit bb while cfg rebuilt.
        getCFG()->computeExitList();
        ASSERT0(getCFG()->verify());

        getCFG()->performMiscOpt(oc);

        //Build DOM after CFG be optimized.
        getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);

        //Infer pointer arith need loopinfo.
        getPassMgr()->checkValidAndRecompute(&oc, PASS_LOOP_INFO, PASS_UNDEF);
    }

    if (g_do_pr_ssa) {
        ASSERT0(getPassMgr());
        PRSSAMgr * ssamgr = (PRSSAMgr*)getPassMgr()->registerPass(
            PASS_PR_SSA_MGR);
        ASSERT0(ssamgr);
        if (!ssamgr->is_valid()) {
            ssamgr->construction(oc);
            if (getDUMgr() != nullptr && !oc.is_du_chain_valid()) {
                //PRSSAMgr will destruct classic DU-chain.
                getDUMgr()->cleanDUSet();
                oc.setInvalidDUChain();
            }
        }
    }

    doBasicAnalysis(oc);

    if (g_do_expr_tab) {
        ASSERT0(g_cst_bb_list);
        getPassMgr()->checkValidAndRecompute(&oc, PASS_EXPR_TAB, PASS_UNDEF);
    }

    if (g_do_cdg) {
        ASSERT0(g_cst_bb_list);
        getPassMgr()->checkValidAndRecompute(&oc, PASS_CDG, PASS_UNDEF);
    }

    if (g_opt_level == OPT_LEVEL0) {
        return;
    }

    //Regenerate high level IR, and do high level optimizations.
    //Now, I get one thing: We cannot or not very easy
    //construct High Level Control IR,
    //(IF,DO_LOOP,...) via analysing CFG.
    //    e.g:
    //
    //        if (i > j) { //BB1
    //            ...
    //        } else {
    //            return 2; //S1
    //        }
    //    BB1 does not have a ipdom, so we can not find the indispensible 3 parts:
    //        True body, False body, and the Sibling node.
    //
    //Solution: We can scan IF stmt first, in order to mark
    //start stmt and end stmt of IF.
    //
    ////AbsNode * an = getAnalysisInstrument()->getCfsMgr()->
    ////    construct_abstract_cfs();
    ////Polyhedra optimization.
    ////IR_POLY * poly = newPoly();
    ////if (poly->construct_poly(an)) {
    ////    poly->perform_poly_trans();
    ////}
    ////delete poly;
}


//Perform high-level optimizaitions.
//Basis step to do:
//    1. Build control flow graph.
//    2. Compute POINT-TO info.
//    3. Compute DEF-USE info.
//    4. Compute Lived Expression info.
//
//Optimizations to be performed:
//    1. Auto Parallel
//    2. Loop interchange
//    3. Loop reverese(may be a little helpful)
//    4. Loop tiling
//    5. Loop fusion
//    6. Loop unrolling
bool Region::HighProcess(OptCtx & oc)
{
    note(this, "\n\n==== Region:%s HIGHEST LEVEL FARMAT ====\n\n",
         getRegionName());

    SimpCtx simp;
    if (g_do_cfs_opt) {
        CfsOpt co(this);
        co.perform(simp);
        ASSERT0(verifyIRList(getIRList(), nullptr, this));
    }

    if (g_build_cfs) {
        ASSERT0(getPassMgr());
        SIMP_is_record_cfs(&simp) = true;
        CfsMgr * cfsmgr = (CfsMgr*)getPassMgr()->registerPass(PASS_CFS_MGR);
        ASSERT0(cfsmgr);
        SIMP_cfs_mgr(&simp) = cfsmgr;
    }

    simp.setSimpCFS();
    setIRList(simplifyStmtList(getIRList(), &simp));
    ASSERT0(verifySimp(getIRList(), simp));
    ASSERT0(verifyIRList(getIRList(), nullptr, this));

    if (g_cst_bb_list) {
        constructBBList();
        ASSERT0(verifyIRandBB(getBBList(), this));
        setIRList(nullptr); //All IRs have been moved to each IRBB.
    }

    HighProcessImpl(oc);
    return true;
}

} //namespace xoc
