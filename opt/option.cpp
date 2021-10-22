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

namespace xoc {

//Optimize float operation.
bool g_is_opt_float = true;

//Lower IR tree to PR mode.
bool g_is_lower_to_pr_mode = false;

//Lower IR tree to the lowest tree height.
bool g_is_lower_to_lowest_height = false;

//Enable XOC support dynamic type.
//That means the type of IR_ST may be ANY.
bool g_is_support_dynamic_type = false;

//If true to hoist short type to integer type.
bool g_is_hoist_type = false;

CHAR * g_func_or_bb_option = nullptr;

//Represent optimization level.
INT g_opt_level = OPT_LEVEL0;

//Construct BB list.
bool g_cst_bb_list = true;

//Build control flow graph.
bool g_do_cfg = true;

//Compute reverse-post-order.
bool g_do_rpo = true;

//Perform peephole optimizations.
bool g_do_refine = true;

//If true to insert IR_CVT by ir refinement.
bool g_do_refine_auto_insert_cvt = false;

//Perform loop analysis.
bool g_do_loop_ana = true;

//Perform cfg optimization: remove labels that no one referenced.
bool g_do_cfg_remove_redundant_label = true;

//Perform cfg optimization: remove empty BB.
bool g_do_cfg_remove_empty_bb = true;

//Perform cfg optimization: remove unreachable BB from entry.
bool g_do_cfg_remove_unreach_bb = true;

//Perform cfg optimization: remove redundant trampoline BB.
//e.g:
//    BB1: goto L1
//    BB2, L1: goto L2
//should be optimized and generate:
//    BB1: goto L2
bool g_do_cfg_remove_trampolin_bb = true;

//Perform cfg optimization: invert branch condition and
//remove redundant trampoline BB.
//e.g:
//    truebr L4 | false L4
//    goto L3
//    L4
//    ...
//    L3:
//    ...
bool g_do_cfg_invert_condition_and_remove_trampolin_bb = true;

//Perform cfg optimization: remove redundant branch.
//e.g:
//    BB1:
//    falsebr L0 //S1
//
//    BB2:
//    L0  //S2
//    ... //S3
//
//S1 is redundant branch.
bool g_do_cfg_remove_redundant_branch = true;

//Build dominator tree.
bool g_do_cfg_dom = true;

//Build post dominator tree.
bool g_do_cfg_pdom = true;

//Perform control flow structure optimizations.
bool g_do_cfs_opt = true;

//Build control dependence graph.
bool g_do_cdg = true;

//Build manager to reconstruct high level control flow structure IR.
//This option is always useful if you want to perform optimization on
//high level IR, such as IF, DO_LOOP, etc.
//Note that if the CFS auxiliary information established, the
//optimizations performed should not violate that.
bool g_build_cfs = false;

//Perform default alias analysis.
bool g_do_aa = true;

//Perform DU analysis for MD to build du chain.
bool g_do_md_du_analysis = true;

//Compute PR DU chain.
bool g_compute_pr_du_chain = false;

//Compute NONPR DU chain.
bool g_compute_nonpr_du_chain = false;

//Computem available expression during du analysis to
//build more precise du chain.
bool g_compute_available_exp = false;

//Computem imported MD which are defined and used in region.
bool g_compute_region_imported_defuse_md = false;

//Build expression table to record lexicographic equally IR expression.
bool g_do_expr_tab = true;

//Perform aggressive copy propagation.
bool g_do_cp_aggressive = false; //It may cost much compile time.

//Perform copy propagation.
bool g_do_cp = false;

//Perform dead code elimination.
bool g_do_dce = false;

//Perform aggressive dead code elimination.
bool g_do_dce_aggressive = false;

//Perform type inference.
bool g_infer_type = true;

//Perform linear function test replacement.
bool g_do_lftr = false;

//Perform dead store elimination.
bool g_do_dse = false;

//Perform global common subexpression elimination.
bool g_do_gcse = false;

//Perform interprocedual analysis and optimization.
bool g_do_ipa = false;

//Build Call Graph.
bool g_do_call_graph = false;

//If true to show compilation time.
bool g_show_time = false;

//Perform function inline.
bool g_do_inline = false;

//Record the limit to inline.
UINT g_inline_threshold = 10;

//Perform induction variable recognization.
bool g_do_ivr = false;

//Perform local common subexpression elimination.
bool g_do_lcse = false;

//Perform loop invariant code motion.
bool g_do_licm = false;

//Perform global value numbering.
bool g_do_gvn = true;

//Perform partial redundant elimination.
bool g_do_pre = false;

//Perform light weith redundant code elimination.
bool g_do_rce = false;

//Perform register promotion.
bool g_do_rp = false;

//Build PR SSA and perform optimization based on SSA.
bool g_do_pr_ssa = false;

//Build Memory SSA and perform optimization based on Memory SSA.
bool g_do_md_ssa = false;

//Record the maximum limit of the number of BB to perform optimizations.
UINT g_thres_opt_bb_num = 100000;

//Record the maximum limit of the number of
//PtPair to perform flow sensitive analysis.
UINT g_thres_ptpair_num = 10000;

//Record the maximum limit of the number of IR to perform optimizations.
//This is the threshold to do optimization.
UINT g_thres_opt_ir_num = 30000;

//Record the maximum limit of the number of IR in single to
//perform optimizations.
//This is the threshold to do optimization.
UINT g_thres_opt_ir_num_in_bb = 10000;

//Convert while-do to do-while loop.
bool g_do_loop_convert = false;

//Polyhedral Transformations.
bool g_do_poly_tran = false;

//Refine DefUse Chain.
bool g_do_refine_duchain = true;

//Perform versatile scalar optimizations.
bool g_do_scalar_opt = true;

//Set to true to retain the PassMgr even if Region processing finished.
bool g_retain_pass_mgr_for_region = false;

//This variable show the verification level that compiler will perform.
//More higher the level is, more verifications will be performed.
UINT g_verify_level = VERIFY_LEVEL_2;

//We always simplify parameters to lowest height to
//facilitate the query of point-to set.
//e.g: DUMgr is going to compute may point-to while
//ADD is pointer type. But only MD has point-to set.
//The query of point-to to ADD(id:6) is failed.
//So we need to store the add's value to a PR,
//and it will reserved the point-to set information.
//
//    call 'getNeighbour'
//       add (ptr<24>) param4 id:6
//           lda (ptr<96>) id:31
//               id (mc<96>, 'pix_a')
//           mul (u32) id:13
//               ld (i32 'i')
//               intconst 24|0x18 (u32) id:14
bool g_is_simplify_parameter = true;

//Dump after each pass.
bool g_is_dump_after_pass = true;

//Dump before each pass.
bool g_is_dump_before_pass = true;

//Set true to enable searching debug-info from expression bottom up
//to nearest stmt.
bool g_is_search_and_copy_dbx = true;

//Record dump options for each Pass.
DumpOpt g_dump_opt;

//Redirect output information to stdout to dump file if exist.
bool g_redirect_stdout_to_dump_file = false;

//Record the unique file handler for dump file.
//Note the order of access to this file will not be guaranteed
//in serial execution when there are multiple RegionMgrs doing compilation
//simultaneously.
FILE * g_unique_dumpfile = nullptr;

DumpOpt::DumpOpt()
{
    is_dump_all = false;
    is_dump_nothing = false;
    is_dump_aa = false;
    is_dump_dumgr = false;
    is_dump_mdset_hash = false;
    is_dump_cfg = false;
    is_dump_dom = false;
    is_dump_cp = false;
    is_dump_rp = false;
    is_dump_dce = false;
    is_dump_lftr = false;
    is_dump_gvn = false;
    is_dump_gcse = false;
    is_dump_ivr = false;
    is_dump_licm = false;
    is_dump_loopcvt = false;
    is_dump_simplification = false;
    is_dump_prssamgr = false;
    is_dump_mdssamgr = false;
    is_dump_cg = false;
    is_dump_ra = false;
    is_dump_memusage = false;
    is_dump_livenessmgr = false;
    is_dump_irparser = false;
}


bool DumpOpt::isDumpALL() const
{
    //is_dump_all and is_dump_nothing can not all be true.
    ASSERT0(!(is_dump_nothing & is_dump_all));
    return is_dump_all;
}


bool DumpOpt::isDumpNothing() const
{
    //is_dump_all and is_dump_nothing can not all be true.
    ASSERT0(!(is_dump_nothing & is_dump_all));
    return is_dump_nothing;
}


bool DumpOpt::isDumpAA() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_aa);
}


bool DumpOpt::isDumpDUMgr() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_dumgr);
}


bool DumpOpt::isDumpMDSetHash() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_mdset_hash);
}


bool DumpOpt::isDumpCFG() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_cfg);
}


bool DumpOpt::isDumpCFGOpt() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_cfgopt);
}


bool DumpOpt::isDumpDOM() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_dom);
}


bool DumpOpt::isDumpCP() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_cp);
}


bool DumpOpt::isDumpRP() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_rp);
}


bool DumpOpt::isDumpInferType() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_infertype);
}


bool DumpOpt::isDumpDCE() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_dce);
}


bool DumpOpt::isDumpRCE() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_rce);
}


bool DumpOpt::isDumpLFTR() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_lftr);
}


bool DumpOpt::isDumpLIS() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_lis);
}


bool DumpOpt::isDumpCDG() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_cdg);
}


bool DumpOpt::isDumpGVN() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_gvn);
}


bool DumpOpt::isDumpGCSE() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_gcse);
}


bool DumpOpt::isDumpIVR() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_ivr);
}


bool DumpOpt::isDumpLICM() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_licm);
}


bool DumpOpt::isDumpLoopCVT() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_loopcvt);
}


bool DumpOpt::isDumpSimp() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_simplification);
}


bool DumpOpt::isDumpPRSSAMgr() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_prssamgr);
}


bool DumpOpt::isDumpMDSSAMgr() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_mdssamgr);
}


bool DumpOpt::isDumpCG() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_cg);
}


bool DumpOpt::isDumpRA() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_ra);
}


bool DumpOpt::isDumpMemUsage() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_memusage);
}

bool DumpOpt::isDumpLivenessMgr() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_livenessmgr);
}


bool DumpOpt::isDumpIRParser() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_irparser);
}


bool DumpOpt::isDumpRefineDUChain() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_refine_duchain);
}

bool DumpOpt::isDumpRefine() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_refine);
}

bool DumpOpt::isDumpGSCC() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_gscc);
}

} //namespace xoc
