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

//Lower IR tree to simplest form.
bool g_is_lower_to_simplest = false;

//If true to hoist short type to integer type.
bool g_is_hoist_type = false;

CHAR * g_func_or_bb_option = NULL;

//Represent optimization level.
INT g_opt_level = NO_OPT;

//Construct bb list.
bool g_cst_bb_list = true;

//Build control flow structure.
bool g_do_cfg = true;

//Compute reverse-post-order.
bool g_do_rpo = true;

//Perform peephole optimizations.
bool g_do_refine = true;

//If true to insert IR_CVT by ir refinement.
bool g_do_refine_auto_insert_cvt = true;

//Perform loop analysis.
bool g_do_loop_ana = true;

//Perform cfg optimization: remove empty bb.
bool g_do_cfg_remove_empty_bb = true;

//Perform cfg optimization: remove unreachable bb from entry.
bool g_do_cfg_remove_unreach_bb = true;

/* Perform cfg optimization: remove redundant trampoline bb.
e.g:
    BB1: goto L1
    BB2, L1: goto L2
should be optimized and generate:
    BB1: goto L2
*/
bool g_do_cfg_remove_trampolin_bb = true;

//Build dominator tree.
bool g_do_cfg_dom = true;

//Build post dominator tree.
bool g_do_cfg_pdom = true;

//Perform control flow structure optimizations.
bool g_do_cfs_opt = true;

//Build control dependence graph.
bool g_do_cdg = true;

/* Build manager to reconstruct high level control flow structure IR.
This option is always useful if you want to perform optimization on
high level IR, such as IF, DO_LOOP, etc.
Note that if the CFS auxiliary information established, the
optimizations performed should not violate that. */
bool g_build_cfs = true;

//Perform default alias analysis.
bool g_do_aa = true;

//Perform DU analysis to build du chain.
bool g_do_du_ana = true;

//Computem available expression during du analysis to
//build more precise du chain.
bool g_do_compute_available_exp = false;

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
bool g_show_comp_time = false;

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

//Build SSA form and perform optimization based on SSA.
bool g_do_ssa = false;

//Record the maximum limit of the number of BB to perform optimizations.
UINT g_thres_opt_bb_num = 10000;

//Record the maximum limit of the number of IR to perform optimizations.
//This is the threshold to do optimization.
UINT g_thres_opt_ir_num = 30000;

//Convert while-do to do-while loop.
bool g_do_loop_convert = false;

//Polyhedral Transformations.
bool g_do_poly_tran = false;

} //namespace xoc
