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

bool g_is_opt_float = true;
bool g_is_lower_to_pr_mode = false;
bool g_is_lower_to_lowest_height = true;
bool g_is_support_dynamic_type = false;
bool g_is_hoist_type = false;
CHAR * g_func_or_bb_option = nullptr;
INT g_opt_level = OPT_LEVEL0;
bool g_cst_bb_list = true;
bool g_enable_local_var_delegate = false;
bool g_do_cfg = true;
bool g_do_rpo = true;
bool g_do_refine = true;
bool g_do_refine_auto_insert_cvt = false;
bool g_do_loop_ana = true;
bool g_do_cfg_remove_redundant_label = true;
bool g_do_cfg_remove_empty_bb = true;
UINT g_cfg_remove_empty_bb_maxtimes_to_update_dominfo = 5000;
bool g_do_cfg_remove_unreach_bb = true;
bool g_do_cfg_remove_trampolin_bb = true;
bool g_do_cfg_remove_trampolin_branch = true;
bool g_do_cfg_remove_redundant_branch = true;
bool g_do_cfg_dom = true;
bool g_do_cfg_pdom = true;
bool g_do_cfs_opt = true;
bool g_do_cdg = true;
bool g_build_cfs = false;
bool g_do_aa = true;
bool g_do_md_du_analysis = true;
bool g_compute_pr_du_chain = false;
bool g_compute_nonpr_du_chain = false;
bool g_compute_available_exp = false;
bool g_compute_region_imported_defuse_md = false;
bool g_do_expr_tab = true;
bool g_do_cp_aggressive = false;
bool g_do_cp = false;
bool g_do_dce = false;
bool g_do_dce_aggressive = false;
bool g_infer_type = true;
bool g_do_vrp = false;
bool g_invert_branch_target = true;
bool g_do_lftr = false;
bool g_do_dse = false;
bool g_do_gcse = true;
bool g_do_ipa = false;
bool g_do_call_graph = false;
bool g_show_time = false;
bool g_do_inline = false;
UINT g_inline_threshold = 10;
bool g_do_ivr = false;
bool g_do_lcse = false;
bool g_do_licm = false;
bool g_do_licm_no_guard = false;
bool g_do_gvn = true;
bool g_do_pre = false;
bool g_do_rce = false;
bool g_do_vect = false;
bool g_do_rp = false;
bool g_do_prssa = false;
bool g_do_mdssa = false;
UINT g_thres_opt_bb_num = 100000;
UINT g_thres_ptpair_num = 10000;
UINT g_thres_opt_ir_num = 30000;
UINT g_thres_opt_ir_num_in_bb = 10000;
bool g_do_loop_convert = false;
bool g_do_poly_tran = false;
bool g_do_refine_duchain = true;
bool g_do_lsra = false;
bool g_do_pelog = false;
bool g_do_scalar_opt = true;
bool g_do_gp_adjustment = true;
bool g_do_relaxation = false;
bool g_retain_pass_mgr_for_region = true;
UINT g_verify_level = VERIFY_LEVEL_2;
bool g_is_simplify_parameter = true;
bool g_is_search_and_copy_dbx = true;
bool g_generate_var_for_pr = true;
DumpOpt g_dump_opt;
bool g_redirect_stdout_to_dump_file = false;
FILE * g_unique_dumpfile = nullptr;
CHAR const* g_unique_dumpfile_name = nullptr;

//
//START DumpOpt
//
DumpOpt::DumpOpt()
{
    is_dump_all = false;
    is_dump_nothing = false;
    //In most cases, dump-after-pass is sufficient.
    is_dump_before_pass = false;
    is_dump_after_pass = true;
    is_dump_aa = false;
    is_dump_dumgr = false;
    is_dump_mdset_hash = false;
    is_dump_cfg = false;
    is_dump_dom = false;
    is_dump_rpo = false;
    is_dump_cp = false;
    is_dump_rp = false;
    is_dump_dce = false;
    is_dump_vrp = false;
    is_dump_lftr = false;
    is_dump_vectorization = false;
    is_dump_gvn = false;
    is_dump_gcse = false;
    is_dump_ivr = false;
    is_dump_licm = false;
    is_dump_exprtab = false;
    is_dump_gcse = false;
    is_dump_loopcvt = false;
    is_dump_simplification = false;
    is_dump_prssamgr = false;
    is_dump_mdssamgr = false;
    is_dump_memusage = false;
    is_dump_livenessmgr = false;
    is_dump_irparser = false;
    is_dump_ir_id = false; //Do not dump IR's id by default.
}


bool DumpOpt::isDumpAll() const
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


bool DumpOpt::isDumpBeforePass() const
{
    return is_dump_before_pass;
}


bool DumpOpt::isDumpAfterPass() const
{
    return is_dump_after_pass;
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


bool DumpOpt::isDumpRPO() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_rpo);
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


bool DumpOpt::isDumpInvertBrTgt() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_invert_brtgt);
}


bool DumpOpt::isDumpVRP() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_vrp);
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


bool DumpOpt::isDumpVectorization() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_vectorization);
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


bool DumpOpt::isDumpExprTab() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_exprtab);
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


bool DumpOpt::isDumpLSRA() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_lsra);
}


bool DumpOpt::isDumpIRID() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_ir_id);
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


bool DumpOpt::isDumpPElog() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_pelog);
}


bool DumpOpt::isDumpGPAdjustment() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_gp_adjustment);
}

bool DumpOpt::isDumpRelaxation() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_relaxation);
}
//END DumpOpt


//
//START Option
//
void Option::dump(MOD LogMgr * lm)
{
    note(lm, "\n==---- DUMP All Options ----==");
    lm->incIndent(2);
    note(lm, "\ng_is_opt_float = %s", g_is_opt_float ? "true":"false");
    note(lm, "\ng_is_lower_to_pr_mode = %s",
         g_is_lower_to_pr_mode ? "true":"false");
    note(lm, "\ng_is_lower_to_lowest_height = %s",
         g_is_lower_to_lowest_height ? "true":"false");
    note(lm, "\ng_is_support_dynamic_type = %s",
         g_is_support_dynamic_type ? "true":"false");
    note(lm, "\ng_is_hoist_type = %s", g_is_hoist_type ? "true":"false");
    note(lm, "\ng_func_or_bb_option = %s",
         g_func_or_bb_option != nullptr ? g_func_or_bb_option:"");
    note(lm, "\ng_opt_level = %s",Option::getOptLevelName(g_opt_level));
    note(lm, "\ng_cst_bb_list = %s", g_cst_bb_list ? "true":"false");
    note(lm, "\ng_enable_local_var_delegate = %s",
         g_enable_local_var_delegate ? "true":"false");
    note(lm, "\ng_do_cfg = %s", g_do_cfg ? "true":"false");
    note(lm, "\ng_do_rpo = %s", g_do_rpo ? "true":"false");
    note(lm, "\ng_do_refine = %s", g_do_refine ? "true":"false");
    note(lm, "\ng_do_refine_auto_insert_cvt = %s",
         g_do_refine_auto_insert_cvt ? "true":"false");
    note(lm, "\ng_do_loop_ana = %s", g_do_loop_ana ? "true":"false");
    note(lm, "\ng_do_cfg_remove_redundant_label = %s",
         g_do_cfg_remove_redundant_label ? "true":"false");
    note(lm, "\ng_do_cfg_remove_empty_bb = %s",
         g_do_cfg_remove_empty_bb ? "true":"false");
    note(lm, "\ng_cfg_remove_empty_bb_maxtimes_to_update_dominfo = %u",
         g_cfg_remove_empty_bb_maxtimes_to_update_dominfo);
    note(lm, "\ng_do_cfg_remove_unreach_bb = %s",
         g_do_cfg_remove_unreach_bb ? "true":"false");
    note(lm, "\ng_do_cfg_remove_trampolin_bb = %s",
         g_do_cfg_remove_trampolin_bb ? "true":"false");
    note(lm, "\ng_do_cfg_remove_trampolin_branch = %s",
         g_do_cfg_remove_trampolin_branch ? "true":"false");
    note(lm, "\ng_do_cfg_remove_redundant_branch = %s",
         g_do_cfg_remove_redundant_branch ? "true":"false");
    note(lm, "\ng_do_cfg_dom = %s", g_do_cfg_dom ? "true":"false");
    note(lm, "\ng_do_cfg_pdom = %s", g_do_cfg_pdom ? "true":"false");
    note(lm, "\ng_do_cfs_opt = %s", g_do_cfs_opt ? "true":"false");
    note(lm, "\ng_do_cdg = %s", g_do_cdg ? "true":"false");
    note(lm, "\ng_build_cfs = %s", g_build_cfs ? "true":"false");
    note(lm, "\ng_do_aa = %s", g_do_aa ? "true":"false");
    note(lm, "\ng_do_md_du_analysis = %s",
         g_do_md_du_analysis ? "true":"false");
    note(lm, "\ng_compute_pr_du_chain = %s",
         g_compute_pr_du_chain ? "true":"false");
    note(lm, "\ng_compute_nonpr_du_chain = %s",
         g_compute_nonpr_du_chain ? "true":"false");
    note(lm, "\ng_compute_available_exp = %s",
         g_compute_available_exp ? "true":"false");
    note(lm, "\ng_compute_region_imported_defuse_md = %s",
         g_compute_region_imported_defuse_md ? "true":"false");
    note(lm, "\ng_do_expr_tab = %s", g_do_expr_tab ? "true":"false");
    note(lm, "\ng_do_cp_aggressive = %s", g_do_cp_aggressive ? "true":"false");
    note(lm, "\ng_do_cp = %s", g_do_cp ? "true":"false");
    note(lm, "\ng_do_dce = %s", g_do_dce ? "true":"false");
    note(lm, "\ng_do_dce_aggressive = %s",
         g_do_dce_aggressive ? "true":"false");
    note(lm, "\ng_infer_type = %s", g_infer_type ? "true":"false");
    note(lm, "\ng_do_vrp = %s", g_do_vrp ? "true":"false");
    note(lm, "\ng_invert_branch_target = %s",
         g_invert_branch_target ? "true":"false");
    note(lm, "\ng_do_lftr = %s", g_do_lftr ? "true":"false");
    note(lm, "\ng_do_dse = %s", g_do_dse ? "true":"false");
    note(lm, "\ng_do_gcse = %s", g_do_gcse ? "true":"false");
    note(lm, "\ng_do_ipa = %s", g_do_ipa ? "true":"false");
    note(lm, "\ng_do_call_graph = %s", g_do_call_graph ? "true":"false");
    note(lm, "\ng_show_time = %s", g_show_time ? "true":"false");
    note(lm, "\ng_do_inline = %s", g_do_inline ? "true":"false");
    note(lm, "\ng_inline_threshold = %u", g_inline_threshold);
    note(lm, "\ng_do_ivr = %s", g_do_ivr ? "true":"false");
    note(lm, "\ng_do_lcse = %s", g_do_lcse ? "true":"false");
    note(lm, "\ng_do_licm = %s", g_do_licm ? "true":"false");
    note(lm, "\ng_do_licm_no_guard = %s", g_do_licm_no_guard ? "true":"false");
    note(lm, "\ng_do_gvn = %s", g_do_gvn ? "true":"false");
    note(lm, "\ng_do_pre = %s", g_do_pre ? "true":"false");
    note(lm, "\ng_do_rce = %s", g_do_rce ? "true":"false");
    note(lm, "\ng_do_vect = %s", g_do_vect ? "true":"false");
    note(lm, "\ng_do_rp = %s", g_do_rp ? "true":"false");
    note(lm, "\ng_do_prssa = %s", g_do_prssa ? "true":"false");
    note(lm, "\ng_do_mdssa = %s", g_do_mdssa ? "true":"false");
    note(lm, "\ng_thres_opt_bb_num = %u", g_thres_opt_bb_num);
    note(lm, "\ng_thres_ptpair_num = %u", g_thres_ptpair_num);
    note(lm, "\ng_thres_opt_ir_num = %u", g_thres_opt_ir_num);
    note(lm, "\ng_thres_opt_ir_num_in_bb = %u", g_thres_opt_ir_num_in_bb);
    note(lm, "\ng_do_loop_convert = %s", g_do_loop_convert ? "true":"false");
    note(lm, "\ng_do_poly_tran = %s", g_do_poly_tran ? "true":"false");
    note(lm, "\ng_do_refine_duchain = %s",
         g_do_refine_duchain ? "true":"false");
    note(lm, "\ng_do_lsra = %s", g_do_lsra ? "true":"false");
    note(lm, "\ng_do_pelog = %s", g_do_pelog ? "true":"false");
    note(lm, "\ng_do_scalar_opt = %s", g_do_scalar_opt ? "true":"false");
    note(lm, "\ng_do_gp_adjustment = %s", g_do_gp_adjustment ? "true":"false");
    note(lm, "\ng_do_relaxation = %s", g_do_relaxation ? "true":"false");
    note(lm, "\ng_retain_pass_mgr_for_region = %s",
         g_retain_pass_mgr_for_region ? "true":"false");
    note(lm, "\ng_verify_level = %s", Option::getOptLevelName(g_verify_level));
    note(lm, "\ng_is_simplify_parameter = %s",
         g_is_simplify_parameter ? "true":"false");
    note(lm, "\ng_is_search_and_copy_dbx = %s",
         g_is_search_and_copy_dbx ? "true":"false");
    note(lm, "\ng_generate_var_for_pr = %s",
         g_generate_var_for_pr ? "true":"false");
    note(lm, "\ng_redirect_stdout_to_dump_file = %s",
         g_redirect_stdout_to_dump_file ? "true":"false");
    note(lm, "\ng_unique_dumpfile = 0x%x", g_unique_dumpfile);
    note(lm, "\ng_unique_dumpfile_name = %s",
         g_unique_dumpfile_name != nullptr ? g_unique_dumpfile_name : "");
    lm->decIndent(2);
}


void Option::dump(RegionMgr * rm)
{
    dump(rm->getLogMgr());
}


CHAR const* Option::getOptLevelName(UINT optlevel)
{
    switch (optlevel) {
    case OPT_LEVEL0: return "NO_OPT";
    case OPT_LEVEL1: return "OPT_LEVEL1";
    case OPT_LEVEL2: return "OPT_LEVEL2";
    case OPT_LEVEL3: return "OPT_LEVEL3";
    case SIZE_OPT: return "SIZE_OPT";
    default: UNREACHABLE();
    }
    return nullptr;
}


CHAR const* Option::getVerifyLevelName(UINT verifylevel)
{
    switch (verifylevel) {
    case VERIFY_LEVEL_1: return "VERIFY_LEVEL_1";
    case OPT_LEVEL2: return "VERIFY_LEVEL_2";
    case OPT_LEVEL3: return "VERIFY_LEVEL_3";
    default: UNREACHABLE();
    }
    return nullptr;
}
//END Option

} //namespace xoc
