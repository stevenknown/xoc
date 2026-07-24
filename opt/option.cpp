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

bool g_do_opt_float = false;
bool g_is_lower_to_pr_mode = false;
bool g_is_lower_to_lowest_height = true;
bool g_is_support_dynamic_type = false;
bool g_is_hoist_type = false;
CHAR * g_func_or_bb_option = nullptr;
INT g_opt_level = OPT_LEVEL0;
bool g_cst_bb_list = true;
bool g_enable_local_var_delegate = false;
bool g_assign_mdref_with_the_offset_for_prop = false;
bool g_do_cfg = true;
bool g_do_rpo = true;
bool g_do_refine = true;
bool g_do_global_refine = true;
bool g_do_refine_with_host_api = false;
bool g_insert_cvt = false;
bool g_calc_derivative = false;
bool g_do_loop_ana = true;
bool g_do_cfg_opt = true;
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
bool g_compute_pr_du_chain_by_prssa = true;
bool g_do_expr_tab = true;
bool g_do_cp_aggressive = false;
bool g_do_cp = true;
bool g_do_bcp = true;
bool g_do_dce = true;
bool g_do_dce_aggressive = false;
bool g_infer_type = true;
bool g_do_vrp = false;
bool g_do_invert_brtgt = true;
bool g_do_lftr = false;
bool g_do_dse = true;
bool g_do_gcse = true;
bool g_do_if_conversion = true;
bool g_do_ipa = false;
bool g_do_call_graph = false;
bool g_show_time = false;
bool g_do_inline = false;
UINT g_inline_threshold = 10;
bool g_do_ivr = false;
bool g_do_lcse = true;
bool g_do_licm = true;
bool g_do_licm_no_guard = false;
bool g_do_gvn = true;
bool g_do_pre = false;
bool g_do_rce = true;
bool g_do_vect = true;
bool g_do_multi_res_convert = true;
bool g_do_targinfo_handler = true;
bool g_do_alge_reassociate = true;
bool g_do_alge_reassociate_aggressive = true;
bool g_do_loop_dep_ana = false;
bool g_do_rp = false;
bool g_do_prssa = false;
bool g_do_mdssa = false;
bool g_reuse_ir = false;

//Set default value to false because some target machines use
//IR_ST as initialization of a Variable.
bool g_verify_ir_attr_readonly = false;
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
bool g_is_simplify_parameter = true;
bool g_is_simplify_array_ingredient = true;
bool g_is_search_and_copy_dbx = true;
bool g_generate_var_for_pr = true;
bool g_strictly_ensure_the_use_of_pointer = false;
DumpOption g_dump_opt;
PassOption g_pass_opt;
bool g_redirect_stdout_to_dump_file = false;
FILE * g_unique_dumpfile = nullptr;
CHAR const* g_unique_dumpfile_name = nullptr;
bool g_do_lsra_debug = false;
UINT g_debug_reg_mod = 0;
bool g_force_use_fp_as_sp = false;
bool g_stack_on_global = false;
bool g_recycle_local_id = false;
bool g_debug_gr = false;
bool g_debug = false;
bool g_debug_cpp = true;
bool g_debug_python = false;

StrTabOption g_include_region;
StrTabOption g_exclude_region;

static DumpOption::OptionDesc g_dump_opt_desc [] = {
  { PASS_UNDEF, "PASS_UNDEF", false, },
  { PASS_CFG, "PASS_CFG", false, },
  { PASS_AA, "PASS_AA", false, },
  { PASS_DU_MGR, "PASS_DU_MGR", false, },
  { PASS_CP, "PASS_CP", false, },
  { PASS_BCP, "PASS_BCP", false, },
  { PASS_CCP, "PASS_CCP", false, },
  { PASS_RCP, "PASS_RCP", false, },
  { PASS_RRCP, "PASS_RRCP", false, },
  { PASS_GCSE, "PASS_GCSE", false, },
  { PASS_LCSE, "PASS_LCSE", false, },
  { PASS_RP, "PASS_RP", false, },
  { PASS_PRE, "PASS_PRE", false, },
  { PASS_IVR, "PASS_IVR", false, },
  { PASS_SCEV, "PASS_SCEV", false, },
  { PASS_LICM, "PASS_LICM", false, },
  { PASS_DCE, "PASS_DCE", false, },
  { PASS_INFER_TYPE, "PASS_INFER_TYPE", false, },
  { PASS_INVERT_BRTGT, "PASS_INVERT_BRTGT", false, },
  { PASS_IF_CONVERSION, "PASS_IF_CONVERSION", false, },
  { PASS_LFTR, "PASS_LFTR", false, },
  { PASS_DSE, "PASS_DSE", false, },
  { PASS_RCE, "PASS_RCE", false, },
  { PASS_GVN, "PASS_GVN", false, },
  { PASS_DOM, "PASS_DOM", false, },
  { PASS_PDOM, "PASS_PDOM", false, },
  { PASS_MD_REF, "PASS_MD_REF", false, },
  { PASS_LIVE_EXPR, "PASS_LIVE_EXPR", false, },
  { PASS_SOLVESET_MGR, "PASS_SOLVESET_MGR", false, },
  { PASS_AVAIL_REACH_DEF, "PASS_AVAIL_REACH_DEF", false, },
  { PASS_REACH_DEF, "PASS_REACH_DEF", false, },
  { PASS_CLASSIC_DU_CHAIN, "PASS_CLASSIC_DU_CHAIN", false, },
  { PASS_EXPR_TAB, "PASS_EXPR_TAB", false, },
  { PASS_LOOP_INFO, "PASS_LOOP_INFO", false, },
  { PASS_CDG, "PASS_CDG", false, },
  { PASS_LOOP_CVT, "PASS_LOOP_CVT", false, },
  { PASS_RPO, "PASS_RPO", false, },
  { PASS_POLY, "PASS_POLY", false, },
  { PASS_LIVENESS_MGR, "PASS_LIVENESS_MGR", false, },
  { PASS_VRP, "PASS_VRP", false, },
  { PASS_PRSSA_MGR, "PASS_PRSSA_MGR", false, },
  { PASS_MDSSA_MGR, "PASS_MDSSA_MGR", false, },
  { PASS_REGSSA_MGR, "PASS_REGSSA_MGR", false, },
  { PASS_CFS_MGR, "PASS_CFS_MGR", false, },
  { PASS_POLY_TRAN, "PASS_POLY_TRAN", false, },
  { PASS_IPA, "PASS_IPA", false, },
  { PASS_INLINER, "PASS_INLINER", false, },
  { PASS_REFINE_DUCHAIN, "PASS_REFINE_DUCHAIN", false, },
  { PASS_SCALAR_OPT, "PASS_SCALAR_OPT", false, },
  { PASS_PRLIVENESS_MGR, "PASS_PRLIVENESS_MGR", false, },
  { PASS_MDLIVENESS_MGR, "PASS_MDLIVENESS_MGR", false, },
  { PASS_VMDLIVENESS_MGR, "PASS_VMDLIVENESS_MGR", false, },
  { PASS_MDSSALIVE_MGR, "PASS_MDSSALIVE_MGR", false, },
  { PASS_REFINE, "PASS_REFINE", false, },
  { PASS_GLOBAL_REFINE, "PASS_GLOBAL_REFINE", false, },
  { PASS_INSERT_CVT, "PASS_INSERT_CVT", false, },
  { PASS_VECT, "PASS_VECT", false, },
  { PASS_SCC, "PASS_SCC", false, },
  { PASS_IRSIMP, "PASS_IRSIMP", false, },
  { PASS_COMPRESS, "PASS_COMPRESS", false, },
  { PASS_REGALLOC_MGR, "PASS_REGALLOC_MGR", false, },
  { PASS_LINEAR_SCAN_RA, "PASS_LINEAR_SCAN_RA", false, },
  { PASS_IRMGR, "PASS_IRMGR", false, },
  { PASS_CALL_GRAPH, "PASS_CALL_GRAPH", false, },
  { PASS_MULTI_RES_CVT, "PASS_MULTI_RES_CVT", false, },
  { PASS_ALGE_REASSOCIATE, "PASS_ALGE_REASSOCIATE", false, },
  { PASS_TARGINFO_HANDLER, "PASS_TARGINFO_HANDLER", false, },
  { PASS_LOOP_DEP_ANA, "PASS_LOOP_DEP_ANA", false, },
  { PASS_PROLOGUE_EPILOGUE, "PASS_PROLOGUE_EPILOGUE", false, },
  { PASS_GP_ADJUSTMENT, "PASS_GP_ADJUSTMENT", false, },
  { PASS_BR_OPT, "PASS_BR_OPT", false, },
  { PASS_DYNAMIC_STACK, "PASS_DYNAMIC_STACK", false, },
  { PASS_IRRELOC, "PASS_IRRELOC", false, },
  { PASS_VARRELOC, "PASS_VARRELOC", false, },
  { PASS_ARGPASSER, "PASS_ARGPASSER", false, },
  { PASS_IGOTO_OPT, "PASS_IGOTO_OPT", false, },
  { PASS_INST_SCHED, "PASS_INST_SCHED", false, },
  { PASS_STACK_COLORING, "PASS_STACK_COLORING", false, },
  { PASS_SAVE_CALLEE, "PASS_SAVE_CALLEE", false, },
  #include "dump_opt_desc_ext.impl"
  { PASS_NUM, "PASS_NUM", false, },
};

//
//START StrTabOption
//
void StrTabOption::addString(CHAR const* str)
{
    if (str == nullptr) { return; }
    xcom::StrBuf tmp((UINT)::strlen(str) + 1);
    UINT charpos = 0;
    for (CHAR const* p = str; *p != 0; p++) {
        if (*p != m_split_char) {
            tmp.buf[charpos] = *p;
            charpos++;
            continue;
        }
        if (charpos == 0){
            //Empty string.
            continue;
        }
        tmp.buf[charpos] = 0;
        add(tmp.buf);
        charpos = 0;
    }
    if (charpos != 0) {
        tmp.buf[charpos] = 0;
        add(tmp.buf);
        charpos = 0;
    }
}


bool StrTabOption::find(CHAR const* str)
{
    SymTabIter it;
    for (Sym const* sym = get_first(it);
         sym != nullptr; sym = get_next(it)) {
        if (::strcmp(sym->getStr(), str) == 0) {
            return true;
        }
    }
    return false;
}


void StrTabOption::dump(MOD LogMgr * lm) const
{
    if (lm == nullptr || !lm->is_init()) { return; }
    SymTabIter it;
    lm->incIndent(2);
    for (Sym const* sym = get_first(it);
         sym != nullptr; sym = get_next(it)) {
        note(lm, "\n%s", sym->getStr());
    }
    lm->decIndent(2);
}


void StrTabOption::dump(RegionMgr * rm) const
{
    dump(rm->getLogMgr());
}
//END StrTabOption


//
//START DumpOption
//
DumpOption::DumpOption()
{
    m_pool = smpoolCreate(64, MEM_COMM);
    setDumpNothing();

    //No dump-options are disabled by default.
    is_dump_nothing = false;

    //In most cases, dump-after-pass is sufficient, thus enable it by default.
    is_dump_after_pass = true;
}


DumpOption::~DumpOption()
{
    ASSERT0(m_pool);
    smpoolDelete(m_pool);
    m_pool = nullptr;
}


void * DumpOption::xmalloc(UINT size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset((void*)p, 0, size);
    return p;
}


void DumpOption::setDumpNothing()
{
    for (PASS_TYPE pt = (PASS_TYPE)(PASS_UNDEF + 1);
         pt < PASS_NUM; pt = (PASS_TYPE)(pt + 1)) {
        OptionDesc * oi = getOptionDesc(pt);
        ASSERT0(oi);
        oi->is_dump = false;
    }
    is_dump_nothing = true;
    is_dump_all = false;
    is_dump_for_test = false;
    is_dump_before_pass = false;
    is_dump_after_pass = false;
    is_dump_mdref = false;
    is_dump_mdset_hash = false;
    is_dump_memusage = false;
    is_dump_irparser = false;
    is_dump_ir_id = false; //Do not dump IR's id by default.
    is_dump_to_buffer = false;
    is_dump_cfgopt = false;
    is_dump_linker = false;
}


void DumpOption::setDumpPass(PASS_TYPE passty, bool is_dump)
{
    DumpOption::OptionDesc * od = DumpOption::getOptionDesc(passty);
    ASSERT0(od);
    od->is_dump = is_dump;
}


void DumpOption::setDumpAll()
{
    for (PASS_TYPE pt = (PASS_TYPE)(PASS_UNDEF + 1);
         pt < PASS_NUM; pt = (PASS_TYPE)(pt + 1)) {
        OptionDesc * oi = getOptionDesc(pt);
        ASSERT0(oi);
        oi->is_dump = true;
    }
    is_dump_nothing = false;
    is_dump_all = true;

    //DumpAll will ask each passes dump complete and verbose information.
    //The information by DumpAll is far more than the requirement of
    //DumpForTest. Given the two options are usually alternative in
    //many passes, thus disable DumpForTest here if DumpAll is enabled.
    //However, user can enable DumpForTest explicitly at any time.
    is_dump_for_test = false;
    is_dump_before_pass = true;
    is_dump_mdref = true;
    is_dump_after_pass = true;
    is_dump_mdset_hash = true;
    is_dump_memusage = true;
    is_dump_irparser = true;
    is_dump_ir_id = true;
    is_dump_cfgopt = true;
    is_dump_linker = true;
}


bool DumpOption::isDumpAll() const
{
    //is_dump_all and is_dump_nothing can not all be true.
    ASSERT0(!(is_dump_nothing & is_dump_all));
    return is_dump_all;
}


bool DumpOption::isDumpForTest() const
{
    //is_dump_all and is_dump_nothing can not all be true.
    ASSERT0(!(is_dump_nothing & is_dump_all));
    return is_dump_for_test;
}



bool DumpOption::isDumpNothing() const
{
    //is_dump_all and is_dump_nothing can not all be true.
    ASSERT0(!(is_dump_nothing & is_dump_all));
    return is_dump_nothing;
}


bool DumpOption::isDumpBeforePass() const
{
    return is_dump_before_pass;
}


bool DumpOption::isDumpAfterPass() const
{
    return is_dump_after_pass;
}


bool DumpOption::isDumpMDSetHash() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_mdset_hash);
}


bool DumpOption::isDumpCFGOpt() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_cfgopt);
}


bool DumpOption::isDumpMDRef() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_mdref);
}


bool DumpOption::isDumpMemUsage() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_memusage);
}


bool DumpOption::isDumpIRParser() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_irparser);
}


bool DumpOption::isDumpIRID() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_ir_id);
}


bool DumpOption::isDumpLinker() const
{
    return is_dump_all || (!is_dump_nothing && is_dump_linker);
}


bool DumpOption::isDumpToBuffer() const
{
    return is_dump_to_buffer;
}


bool DumpOption::isDumpLSRAReorderMovInLatchBB() const
{
    return is_dump_all ||
        (!is_dump_nothing && is_dump_lsra_reorder_mov_in_latch_BB);
}


bool DumpOption::isDumpPass(PASS_TYPE pt) const
{
    if (is_dump_all) { return true; }
    if (is_dump_nothing) { return false; }
    OptionDesc const* od = getOptionDesc(pt);
    ASSERT0(od);
    return od->is_dump;
}


DumpOption::OptionDesc * DumpOption::getOptionDesc(PASS_TYPE pt)
{
    ASSERT0(pt < PASS_NUM);
    return &g_dump_opt_desc[pt];
}


void DumpOption::dump(RegionMgr const* rm) const
{
    ASSERT0(rm);
    if (!rm->isLogMgrInit()) { return; }
    note(rm, "\n==---- DUMP DumpOptions ----==");
    for (PASS_TYPE pt = (PASS_TYPE)(PASS_UNDEF + 1);
         pt < PASS_NUM; pt = (PASS_TYPE)(pt + 1)) {
        OptionDesc const* oi = getOptionDesc(pt);
        ASSERT0(oi);
        note(rm, "\n%s:", oi->getPassTypeName());
        prt(rm, "is_dump:%s", oi->is_dump ? "true" : "false");
    }
    note(rm, "\n");
}
//END DumpOption


//
//START Option
//
void Option::dump(MOD LogMgr * lm)
{
    note(lm, "\n==---- DUMP All Options ----==");
    lm->incIndent(2);
    note(lm, "\ng_do_opt_float = %s", g_do_opt_float ? "true":"false");
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
    note(lm, "\ng_do_global_refine = %s", g_do_global_refine ? "true":"false");
    note(lm, "\ng_do_refine_with_host_api = %s",
         g_do_refine_with_host_api ? "true":"false");
    note(lm, "\ng_insert_cvt = %s",
         g_insert_cvt ? "true":"false");
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
    note(lm, "\ng_compute_pr_du_chain_by_prssa = %s",
         g_compute_pr_du_chain_by_prssa ? "true":"false");
    note(lm, "\ng_do_expr_tab = %s", g_do_expr_tab ? "true":"false");
    note(lm, "\ng_do_cp_aggressive = %s", g_do_cp_aggressive ? "true":"false");
    note(lm, "\ng_do_cp = %s", g_do_cp ? "true":"false");
    note(lm, "\ng_do_bcp = %s", g_do_bcp ? "true":"false");
    note(lm, "\ng_do_dce = %s", g_do_dce ? "true":"false");
    note(lm, "\ng_do_dce_aggressive = %s",
         g_do_dce_aggressive ? "true":"false");
    note(lm, "\ng_infer_type = %s", g_infer_type ? "true":"false");
    note(lm, "\ng_do_vrp = %s", g_do_vrp ? "true":"false");
    note(lm, "\ng_do_invert_brtgt = %s",
         g_do_invert_brtgt ? "true":"false");
    note(lm, "\ng_do_lftr = %s", g_do_lftr ? "true":"false");
    note(lm, "\ng_do_if_conversion = %s", g_do_if_conversion ? "true":"false");
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
    note(lm, "\ng_do_multi_res_convert = %s",
         g_do_multi_res_convert ? "true":"false");
    note(lm, "\ng_do_targinfo_handler = %s",
         g_do_targinfo_handler ? "true":"false");
    note(lm, "\ng_do_alge_reassociate = %s",
         g_do_alge_reassociate ? "true":"false");
    note(lm, "\ng_do_alge_reassociate_aggressive = %s",
         g_do_alge_reassociate_aggressive ? "true":"false");
    note(lm, "\ng_do_loop_dep_ana = %s", g_do_loop_dep_ana ? "true":"false");
    note(lm, "\ng_do_rp = %s", g_do_rp ? "true":"false");
    note(lm, "\ng_do_prssa = %s", g_do_prssa ? "true":"false");
    note(lm, "\ng_do_mdssa = %s", g_do_mdssa ? "true":"false");
    note(lm, "\ng_reuse_ir = %s", g_reuse_ir ? "true":"false");
    note(lm, "\ng_verify_ir_attr_readonly = %s",
         g_verify_ir_attr_readonly ? "true":"false");
    note(lm, "\ng_thres_opt_bb_num = %u", g_thres_opt_bb_num);
    note(lm, "\ng_thres_ptpair_num = %u", g_thres_ptpair_num);
    note(lm, "\ng_thres_opt_ir_num = %u", g_thres_opt_ir_num);
    note(lm, "\ng_thres_opt_ir_num_in_bb = %u", g_thres_opt_ir_num_in_bb);
    note(lm, "\ng_do_loop_convert = %s", g_do_loop_convert ? "true":"false");
    note(lm, "\ng_do_poly_tran = %s", g_do_poly_tran ? "true":"false");
    note(lm, "\ng_do_refine_duchain = %s",
         g_do_refine_duchain ? "true":"false");
    note(lm, "\ng_do_lsra = %s", g_do_lsra ? "true":"false");
    note(lm, "\ng_recycle_local_id = %s", g_recycle_local_id ? "true":"false");
    note(lm, "\ng_do_scalar_opt = %s", g_do_scalar_opt ? "true":"false");
    note(lm, "\ng_retain_pass_mgr_for_region = %s",
         g_retain_pass_mgr_for_region ? "true":"false");
    note(lm, "\ng_is_simplify_parameter = %s",
         g_is_simplify_parameter ? "true":"false");
    note(lm, "\ng_is_simplify_array_ingredient = %s",
         g_is_simplify_array_ingredient ? "true":"false");
    note(lm, "\ng_is_search_and_copy_dbx = %s",
         g_is_search_and_copy_dbx ? "true":"false");
    note(lm, "\ng_generate_var_for_pr = %s",
         g_generate_var_for_pr ? "true":"false");
    note(lm, "\ng_strictly_ensure_the_use_of_pointer = %s",
         g_strictly_ensure_the_use_of_pointer ? "true":"false");
    note(lm, "\ng_redirect_stdout_to_dump_file = %s",
         g_redirect_stdout_to_dump_file ? "true":"false");
    note(lm, "\ng_unique_dumpfile = 0x%x", g_unique_dumpfile);
    note(lm, "\ng_unique_dumpfile_name = %s",
         g_unique_dumpfile_name != nullptr ? g_unique_dumpfile_name : "");
    note(lm, "\ng_include_option =");
    g_include_region.dump(lm);
    note(lm, "\ng_exclude_option =");
    g_exclude_region.dump(lm);
    note(lm, "\ng_do_lsra_debug = %s", g_do_lsra_debug ? "true":"false");
    note(lm, "\ng_debug_reg_mod = %u", g_debug_reg_mod);
    note(lm, "\ng_force_use_fp_as_stack_pointer = %s",
         g_force_use_fp_as_sp ? "true":"false");
    note(lm, "\ng_do_invert_brtgt = %s", g_do_invert_brtgt ? "true":"false");
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


//
//START PassOption
//
class PassSwitch {
public:
    bool * bool_switch;
};

//The Level1 pass only does refinement.
static PassSwitch g_pass_in_level1[] {
    { &xoc::g_do_prssa, },
    { &xoc::g_do_mdssa, },
    { &xoc::g_infer_type, },
};
static UINT g_pass_num_in_level1 =
    sizeof(g_pass_in_level1) / sizeof(g_pass_in_level1[0]);


static PassSwitch g_pass_in_level2[] {
    { &xoc::g_do_cp, },
    { &xoc::g_do_dce, },
    { &xoc::g_do_licm, },
    { &xoc::g_do_gcse, },
    { &xoc::g_do_rce, },
    { &xoc::g_do_rp, },
    { &xoc::g_do_prssa, },
    { &xoc::g_do_mdssa, },
    { &xoc::g_infer_type, },
};
static UINT g_pass_num_in_level2 =
    sizeof(g_pass_in_level2) / sizeof(g_pass_in_level2[0]);


static PassSwitch g_pass_in_level3[] {
    { &xoc::g_do_cp, },
    { &xoc::g_do_cp_aggressive, },
    { &xoc::g_do_bcp, },
    { &xoc::g_do_dce, },
    { &xoc::g_do_dce_aggressive, },
    { &xoc::g_do_licm, },
    { &xoc::g_do_gcse, },
    { &xoc::g_do_gvn, },
    { &xoc::g_do_alge_reassociate, },
    { &xoc::g_do_alge_reassociate_aggressive, },
    { &xoc::g_do_rce, },
    { &xoc::g_do_rp, },
    { &xoc::g_do_lftr, },
    { &xoc::g_do_prssa, },
    { &xoc::g_do_mdssa, },
    { &xoc::g_infer_type, },
    { &xoc::g_do_if_conversion, },
    { &xoc::g_do_vect, },
    { &xoc::g_do_cfg_remove_empty_bb, },
    { &xoc::g_do_cfg_remove_unreach_bb, },
    { &xoc::g_do_cfg_remove_trampolin_bb, },
    { &xoc::g_do_invert_brtgt, },
    { &xoc::g_do_cfg_remove_redundant_branch, },
    { &xoc::g_do_cfg_remove_trampolin_branch, },
    { &xoc::g_do_cfg_remove_redundant_label, },
};
static UINT g_pass_num_in_level3 =
    sizeof(g_pass_in_level3) / sizeof(g_pass_in_level3[0]);


static PassSwitch g_pass_in_level_size[] {
    { &xoc::g_do_dce, },
    { &xoc::g_do_dce_aggressive, },
    { &xoc::g_do_rce, },
    { &xoc::g_do_prssa, },
    { &xoc::g_do_mdssa, },
    { &xoc::g_infer_type, },
    { &xoc::g_do_cfg_remove_empty_bb, },
    { &xoc::g_do_cfg_remove_unreach_bb, },
    { &xoc::g_do_cfg_remove_trampolin_bb, },
    { &xoc::g_do_cfg_remove_redundant_branch, },
    { &xoc::g_do_cfg_remove_trampolin_branch, },
    { &xoc::g_do_cfg_remove_redundant_label, },
};
static UINT g_pass_num_in_level_size =
    sizeof(g_pass_in_level_size) / sizeof(g_pass_in_level_size[0]);


void PassOption::setPassInLevel3(bool enable)
{
    for (UINT i = 0; i < g_pass_num_in_level3; i++) {
        *g_pass_in_level3[i].bool_switch = enable;
    }
}


void PassOption::setPassInLevel2(bool enable)
{
    for (UINT i = 0; i < g_pass_num_in_level2; i++) {
        *g_pass_in_level2[i].bool_switch = enable;
    }
}


void PassOption::setPassInLevel1(bool enable)
{
    for (UINT i = 0; i < g_pass_num_in_level1; i++) {
        *g_pass_in_level1[i].bool_switch = enable;
    }
}


void PassOption::setPassInLevelSize(bool enable)
{
    for (UINT i = 0; i < g_pass_num_in_level_size; i++) {
        *g_pass_in_level_size[i].bool_switch = enable;
    }
}
//END PassOption


bool checkDumpOptionDesc()
{
    for (PASS_TYPE pt = (PASS_TYPE)(PASS_UNDEF + 1);
         pt < PASS_NUM; pt = (PASS_TYPE)(pt + 1)) {
        DumpOption::OptionDesc const* oi = DumpOption::getOptionDesc(pt);
        ASSERT0(oi);
        ASSERTN(oi->pt == pt, ("pass type unmatched, pass may be missed"));
    }
    return true;
}

} //namespace xoc
