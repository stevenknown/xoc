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
bool g_is_lower_to_lowest_height = false;
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
bool g_do_gcse = false;
bool g_do_ipa = false;
bool g_do_call_graph = false;
bool g_show_time = false;
bool g_do_inline = false;
UINT g_inline_threshold = 10;
bool g_do_ivr = false;
bool g_do_lcse = false;
bool g_do_licm = false;
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
bool g_do_scalar_opt = true;
bool g_retain_pass_mgr_for_region = true;
UINT g_verify_level = VERIFY_LEVEL_2;
bool g_is_simplify_parameter = true;
bool g_is_search_and_copy_dbx = true;
bool g_generate_var_for_pr = true;
DumpOpt g_dump_opt;
bool g_redirect_stdout_to_dump_file = false;
FILE * g_unique_dumpfile = nullptr;

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

} //namespace xoc
