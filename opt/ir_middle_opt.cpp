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

void Region::lowerIRTreeToLowestHeight(OptCTX & oc)
{
    SimpCTX simp;
    if (g_is_lower_to_simplest) {
        simp.set_simp_cf();
        simp.set_simp_array();
        simp.set_simp_select();
        simp.set_simp_local_or_and();
        simp.set_simp_local_not();
        simp.set_simp_to_pr_mode();
    } else {
        simp.set_simp_to_lowest_heigh();
        SIMP_array(&simp) = false; //Keep array operation unchanged.
        SIMP_array_to_pr_mode(&simp) = true;
    }

    if (g_do_ssa) {
        //Note if this flag enable,
        //AA may generate imprecise result.
        //TODO: use SSA info to improve the precision of AA.
        simp.set_simp_to_pr_mode();
        simp.set_simp_select();
        simp.set_simp_local_or_and();
        simp.set_simp_local_not();
    }

    //Simplify IR tree if it is needed.
    simplifyBBlist(get_bb_list(), &simp);

    if (SIMP_need_recon_bblist(&simp)) {
        //New BB boundary IR generated, rebuilding CFG.
        if (reconstructBBlist(oc)) {
            get_cfg()->rebuild(oc);
            get_cfg()->removeEmptyBB(oc);
            get_cfg()->computeEntryAndExit(true, true);
        }
    }

    if (SIMP_changed(&simp)) {
        //We perfer flow sensitive analysis as default.
        get_aa()->set_flow_sensitive(true);
        get_aa()->perform(oc);

        //The primary actions must do are computing IR reference
        //and reach def.
        UINT action = SOL_REACH_DEF|SOL_REF;

        if (g_do_ivr) {
            //IVR needs available reach def.
            action |= SOL_AVAIL_REACH_DEF;
        }

        //DU mananger may use the context info supplied by AA.
        get_du_mgr()->perform(oc, action);

        //Compute the DU chain.
        get_du_mgr()->computeMDDUChain(oc);
    }
}


/*
Perform general optimizaitions.
Basis step to do:
    1. Build control flow.
    2. Compute data flow dependence.
    3. Compute live expression info.

Optimizations to be performed:
    1. GCSE
    2. DCE
    3. RVI(register variable recog)
    4. IVR(induction variable elimination)
    5. CP(constant propagation)
    6. CP(copy propagation)
    7. SCCP (Sparse Conditional Constant Propagation).
    8. PRE (Partial Redundancy Elimination) with strength reduction.
    9. Dominator-based optimizations such as copy propagation,
        constant propagation and redundancy elimination using
        value numbering.
    10. Must-alias analysis, to convert pointer de-references
        into regular variable references whenever possible.
    11. Scalar Replacement of Aggregates, to convert structure
        references into scalar references that can be optimized
        using the standard scalar passes.
*/
bool Region::MiddleProcess(OptCTX & oc)
{
    if (g_opt_level == NO_OPT) { return false; }
    g_indent = 0;

    PassMgr * passmgr = get_pass_mgr();
    ASSERT0(passmgr);

    //Perform scalar optimizations.
    passmgr->performScalarOpt(oc);
    return true;
}

} //namespace xoc
