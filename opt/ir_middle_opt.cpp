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

void Region::lowerIRTreeToLowestHeight(OptCtx & oc)
{
    SimpCtx simp;
    if (g_is_lower_to_pr_mode) {
        simp.setSimpToPRmode();
    }

    if (g_do_pr_ssa) {
        //Note if this flag enable,
        //AA may generate imprecise result.
        //TODO: use SSA info to improve the precision of AA.
        simp.setSimpLandLor();
        simp.setSimpLnot();
        simp.setSimpCFS();
    }

    //Simplify IR tree if it is needed.
    simplifyBBlist(getBBList(), &simp);

    if (SIMP_need_recon_bblist(&simp)) {
        //New BB boundary IR generated, rebuilding CFG.
        if (reconstructBBList(oc)) {
            getCFG()->rebuild(oc);
        }
    }

    if (SIMP_changed(&simp)) {
        OC_is_aa_valid(oc) = false;
        OC_is_du_chain_valid(oc) = false;
        OC_is_reach_def_valid(oc) = false;
        OC_is_avail_reach_def_valid(oc) = false;
    }
}


//Simplification will maintain MD SSA, DU Ref information.
bool Region::performSimplify(OptCtx & oc)
{
    SimpCtx simp;
    simp.setSimpCFS();
    simp.setSimpArray();
    simp.setSimpSelect();
    simp.setSimpLandLor();
    simp.setSimpLnot();
    simp.setSimpILdISt();
    simp.setSimpToLowestHeight();
    if (g_is_lower_to_pr_mode) {
        simp.setSimpToPRmode();
    }
    if (g_opt_level != OPT_LEVEL0) {
        //O0 does not build DU ref.
        ASSERT0(verifyMDRef());
    }
    simplifyBBlist(getBBList(), &simp);
    if (g_do_cfg &&
        g_cst_bb_list &&
        SIMP_need_recon_bblist(&simp) &&
        reconstructBBList(oc)) {

        //Simplification may generate new memory operations.
        if (g_opt_level != OPT_LEVEL0) {
            //O0 does not build DU ref.
            ASSERT0(verifyMDRef());
        }

        //Before CFG building.
        getCFG()->removeEmptyBB(oc);

        getCFG()->rebuild(oc);

        ASSERT0(getCFG()->verify());

        getCFG()->performMiscOpt(oc);

        oc.set_flag_if_cfg_changed();
        //Each pass maintain CFG by default.
        OC_is_cfg_valid(oc) = true;
        if (g_do_cdg) {
            ASSERT0(getPassMgr());
            CDG * cdg = (CDG*)getPassMgr()->registerPass(PASS_CDG);
            cdg->rebuild(oc, *getCFG());
        }
    } else {
        ASSERT0((!g_do_md_du_analysis && !g_do_md_ssa) || verifyMDRef());
    }

    if (g_verify_level >= VERIFY_LEVEL_3 && OC_is_du_chain_valid(oc)) {
        ASSERT0(getDUMgr() == NULL ||
            getDUMgr()->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NONPR_DU));
    }

    if (g_is_dump_after_pass && g_dump_opt.isDumpALL()) {
        g_indent = 0;
        note("\n==---- DUMP AFTER SIMPLIFY IRBB LIST ----==");
        dumpBBList(getBBList(), this);
    }
    return true;
}


//Perform general optimizaitions.
//Basis step to do:
//    1. Build control flow.
//    2. Compute data flow dependence.
//    3. Compute live expression info.
//
//Optimizations to be performed:
//    1. GCSE
//    2. DCE
//    3. RVI(register variable recog)
//    4. IVR(induction variable elimination)
//    5. CP(constant propagation)
//    6. CP(copy propagation)
//    7. SCCP (Sparse Conditional Constant Propagation).
//    8. PRE (Partial Redundancy Elimination) with strength reduction.
//    9. Dominator-based optimizations such as copy propagation,
//        constant propagation and redundancy elimination using
//        value numbering.
//    10. Must-alias analysis, to convert pointer de-references
//        into regular variable references whenever possible.
//    11. Scalar Replacement of Aggregates, to convert structure
//        references into scalar references that can be optimized
//       using the standard scalar passes.
bool Region::MiddleProcess(OptCtx & oc)
{
    BBList * bbl = getBBList();
    if (bbl->get_elem_count() == 0) { return true; }

    if (g_verify_level >= VERIFY_LEVEL_3) {
        ASSERT0(getDUMgr() == NULL ||
            getDUMgr()->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NONPR_DU));
    }

    bool do_simplification = true;
    if (getPassMgr() != NULL &&
        (getPassMgr()->queryPass(PASS_PR_SSA_MGR) != NULL &&
         ((PRSSAMgr*)getPassMgr()->queryPass(PASS_PR_SSA_MGR))->
              isSSAConstructed())) {
        do_simplification = false;
    }

    if (do_simplification) {
        performSimplify(oc);
    }

    if (g_opt_level > OPT_LEVEL0) {
        PassMgr * passmgr = getPassMgr();
        ASSERT0(passmgr);
        passmgr->performScalarOpt(oc);
    }

    ASSERT0(verifyRPO(oc));
    if (g_do_refine) {
        RefineCtx rf;
        if (refineBBlist(bbl, rf)) {
            OC_is_expr_tab_valid(oc) = false;
            OC_is_live_expr_valid(oc) = false;
            OC_is_reach_def_valid(oc) = false;
            //DU chain is kept by refinement.
            ASSERT0(verifyIRandBB(bbl, this));
            if (g_verify_level >= VERIFY_LEVEL_3 && OC_is_du_chain_valid(oc)) {
                ASSERT0(getDUMgr() == NULL ||
                        getDUMgr()->verifyMDDUChain(
                            COMPUTE_PR_DU | COMPUTE_NONPR_DU));
            }
            return true;
        }        
    }
    ASSERT0(verifyIRandBB(bbl, this));    
    return true;
}

} //namespace xoc
