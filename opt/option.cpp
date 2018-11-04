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

//Enable XOC support dynamic type.
//That means the type of IR_ST may be VOID.
bool g_is_support_dynamic_type = false;

//If true to hoist short type to integer type.
bool g_is_hoist_type = false;

CHAR * g_func_or_bb_option = NULL;

//Represent optimization level.
INT g_opt_level = OPT_LEVEL0;

//Construct bb list.
bool g_cst_bb_list = true;

//Build control flow structure.
bool g_do_cfg = true;

//Compute reverse-post-order.
bool g_do_rpo = true;

//Perform peephole optimizations.
bool g_do_refine = true;

//If true to insert IR_CVT by ir refinement.
bool g_do_refine_auto_insert_cvt = false;

//Perform loop analysis.
bool g_do_loop_ana = true;

//Perform cfg optimization: remove empty bb.
bool g_do_cfg_remove_empty_bb = true;

//Perform cfg optimization: remove unreachable bb from entry.
bool g_do_cfg_remove_unreach_bb = true;

//Perform cfg optimization: remove redundant trampoline bb.
//e.g:
//    BB1: goto L1
//    BB2, L1: goto L2
//should be optimized and generate:
//    BB1: goto L2
bool g_do_cfg_remove_trampolin_bb = true;

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
bool g_do_md_du_ana = true;

//Compute DU chain.
bool g_compute_du_chain = false;

//Computem available expression during du analysis to
//build more precise du chain.
bool g_compute_available_exp = false;

//Computem imported MD which are defined and used in region.
bool g_compute_region_imported_defuse_md = false;

//Build expression table to record lexicographic equally IR expression.
bool g_do_expr_tab = true;

//Perform aggressive copy propagation.
bool g_do_cp_aggressive = true; //It may cost much compile time.

//Perform copy propagation.
bool g_do_cp = false;

//Perform dead code elimination.
bool g_do_dce = false;

//Perform aggressive dead code elimination.
bool g_do_dce_aggressive = true;

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

//If true to show memory usage for each Region.
bool g_show_memory_usage = true;

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

//Perform global register allocation.
bool g_do_gra = false;

//Perform partial redundant elimination.
bool g_do_pre = false;

//Perform redundant code elimination.
bool g_do_rce = false;

//Perform register promotion.
bool g_do_rp = false;

//Build PR SSA and perform optimization based on SSA.
bool g_do_pr_ssa = false;

//Build Memory SSA and perform optimization based on Memory SSA.
bool g_do_md_ssa = true;

//Record the maximum limit of the number of BB to perform optimizations.
UINT g_thres_opt_bb_num = 100000;

//Record the maximum limit of the number of
//PtPair to perform flow sensitive analysis.
UINT g_thres_flow_sensitive_aa = 10000;

//Record the maximum limit of the number of IR to perform optimizations.
//This is the threshold to do optimization.
UINT g_thres_opt_ir_num = 30000;

//Record the maximum limit of the number of IR in single to perform optimizations.
//This is the threshold to do optimization.
UINT g_thres_opt_ir_num_in_bb = 10000;

//Convert while-do to do-while loop.
bool g_do_loop_convert = false;

//Polyhedral Transformations.
bool g_do_poly_tran = false;

//Set to true to retain the PassMgr even if Region processing finished.
bool g_retain_pass_mgr_for_region = false;

//This variable show the verification level that compiler will perform.
//More higher the level is, more verifications will be performed.
UINT g_verify_level = VERIFY_LEVEL_2;

//We always simplify parameters to lowest height to
//facilitate the query of point-to set.
//e.g: IR_DU_MGR is going to compute may point-to while
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

//Dump MD Set Hash Table.
bool g_is_dump_mdset_hash = true;

//Dump MD DU chain built by DU Manager.
bool g_is_dump_du_chain = true;

//Dump Alias Analysis miscellaneous info.
bool g_is_dump_alias_analysis = true;

//Dump after each pass.
bool g_is_dump_after_pass = true;
} //namespace xoc
