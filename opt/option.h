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
#ifndef __OPTION_H__
#define __OPTION_H__

namespace xoc {

class PassMgr;

#define OPT_LEVEL0 0
#define OPT_LEVEL1 1
#define OPT_LEVEL2 2
#define OPT_LEVEL3 3
#define SIZE_OPT 4

#define VERIFY_LEVEL_1 1 //only perform basic verifications.
#define VERIFY_LEVEL_2 2 //do more aggressive check.
#define VERIFY_LEVEL_3 3 //do all verifications.

//Optimization Context
//This class record and propagate auxiliary information to optimizations.
//These options brief describe state of Passes following an optimization
//perform. When an optimization pass is run, it can change results of other
//pass. Set the valid/invalid option to inform that results were preserved by
//that optimization pass.es must be done explicitly.
#define OC_is_ref_valid(o) ((o).u1.s1.is_du_ref_valid)
#define OC_is_pr_du_chain_valid(o) ((o).u1.s1.is_pr_du_chain_valid)
#define OC_is_nonpr_du_chain_valid(o) ((o).u1.s1.is_nonpr_du_chain_valid)
#define OC_is_live_expr_valid(o) ((o).u1.s1.is_live_expr_valid)
#define OC_is_reach_def_valid(o) ((o).u1.s1.is_reach_def_valid)
#define OC_is_avail_reach_def_valid(o) ((o).u1.s1.is_avail_reach_def_valid)
#define OC_is_cfg_valid(o) ((o).u1.s1.is_cfg_valid)
#define OC_is_aa_valid(o) ((o).u1.s1.is_aa_result_valid)
#define OC_is_expr_tab_valid(o) ((o).u1.s1.is_ir_expr_tab)
#define OC_is_cdg_valid(o) ((o).u1.s1.is_cdg_valid)
#define OC_is_dom_valid(o) ((o).u1.s1.is_dom_valid)
#define OC_is_pdom_valid(o) ((o).u1.s1.is_pdom_valid)
#define OC_is_rpo_valid(o) ((o).u1.s1.is_rpo_valid)
#define OC_is_loopinfo_valid(o) ((o).u1.s1.is_loopinfo_valid)
#define OC_is_callg_valid(o) ((o).u1.s1.is_callg_valid)
#define OC_show_comp_time(o) ((o).u2.s1.show_compile_time)
class OptCtx {
public:
    union {
        UINT int1;
        struct {
            //Record MUST-DEF, MAY-DEF, MAY-USE MDSet for each IR STMT/EXP.
            UINT is_du_ref_valid:1;

            //Record DEF/USE IR stmt/exp for PR operation.
            UINT is_pr_du_chain_valid:1;
            //Record DEF/USE IR stmt/exp for NON-PR operation.
            UINT is_nonpr_du_chain_valid:1;
            UINT is_live_expr_valid:1;
            UINT is_reach_def_valid:1;
            UINT is_avail_reach_def_valid:1;
            UINT is_aa_result_valid:1; //POINT TO info is avaiable.
            UINT is_ir_expr_tab:1; //Liveness of ExpRep is avaliable.
            UINT is_cfg_valid:1; //CFG is avaliable.
            UINT is_cdg_valid:1; //CDG is avaliable.

            //Dominator Set, Immediate Dominator are avaliable.
            UINT is_dom_valid:1;

            //Post Dominator Set, Post Immediate Dominator are avaiable.
            UINT is_pdom_valid:1;

            UINT is_loopinfo_valid:1; //Loop info is avaiable.

            UINT is_callg_valid:1; //Call graph is available.

            UINT is_rpo_valid:1; //Rporder is available.
        } s1;
    } u1;

    union {
        UINT int1;
        struct {
            UINT show_compile_time:1; //Show compilation time.
        } s1;
    } u2;

public:
    OptCtx() { set_all_invalid(); u2.int1 = 0; }
    OptCtx const& operator = (OptCtx const&);

    void set_all_valid() { u1.int1 = (UINT)-1; }
    void set_all_invalid() { u1.int1 = 0; }

    //This function reset the flag if control flow changed.
    void set_flag_if_cfg_changed()
    {
        OC_is_cfg_valid(*this) = false;
        OC_is_cdg_valid(*this) = false;
        OC_is_dom_valid(*this) = false;
        OC_is_pdom_valid(*this) = false;
        OC_is_rpo_valid(*this) = false;
        OC_is_loopinfo_valid(*this) = false;
    }
};


class DumpOpt {
public:
    //Dump all information.
    //Note is_dump_all and is_dump_nothing can not all be true.
    bool is_dump_all;
    //Do not dump anything.
    //Note is_dump_all and is_dump_nothing can not all be true.
    bool is_dump_nothing;
    bool is_dump_aa; //Dump Alias Analysis information.
    bool is_dump_dumgr; //Dump MD Def-Use chain built by DU Manager.
    bool is_dump_duref; //Dump MD Def-Use reference built both
                        //by AA and DU Manager.
    bool is_dump_mdset_hash; //Dump MD Set Hash Table.
    bool is_dump_cfg; //Dump CFG.
    bool is_dump_dom; //Dump Dom/Pdom/Idom/Pidom.
    bool is_dump_cp; //Dump copy-propagation.
    bool is_dump_rp; //Dump register-promotion.
    bool is_dump_rce; //Dump light weight redundant-code-elimination.
    bool is_dump_dce; //Dump dead-code-elimination.
    bool is_dump_gvn; //Dump global-value-numbering.
    bool is_dump_gcse; //Dump global-common-subscript-expression.
    bool is_dump_ivr; //Dump induction variable recognization.
    bool is_dump_licm; //Dump loop-invariant-code-motion.
    bool is_dump_loopcvt; //Dump loop-convertion.
    bool is_dump_simplification; //Dump IR simplification.
    bool is_dump_prssamgr; //Dump PRSSAMgr.
    bool is_dump_mdssamgr; //Dump MDSSAMgr.
    bool is_dump_cg; //Dump CodeGeneration.
    bool is_dump_ra; //Dump register allocation.
    bool is_dump_memusage; //Dump memory usage.
    bool is_dump_livenessmgr; //Dump LivenessMgr.
    bool is_dump_refine_duchain; //Dump RefineDUChain.

public:
    DumpOpt();
    DumpOpt const& operator = (DumpOpt const&); //Disable operator =.

    bool isDumpALL() const;
    bool isDumpNothing() const;
    bool isDumpAA() const;
    bool isDumpDUMgr() const;
    bool isDumpMDSetHash() const;
    bool isDumpCFG() const;
    bool isDumpDOM() const;
    bool isDumpCP() const;
    bool isDumpRP() const;
    bool isDumpRCE() const;
    bool isDumpDCE() const;
    bool isDumpGVN() const;
    bool isDumpGCSE() const;
    bool isDumpIVR() const;
    bool isDumpLICM() const;
    bool isDumpLoopCVT() const;
    bool isDumpSimp() const;
    bool isDumpPRSSAMgr() const;
    bool isDumpMDSSAMgr() const;
    bool isDumpCG() const;
    bool isDumpRA() const;
    bool isDumpMemUsage() const;
    bool isDumpLivenessMgr() const;
    bool isDumpRefineDUChain() const;
};


//Declare the optimization.
typedef enum _PASS_TYPE {
    PASS_UNDEF = 0,
    PASS_CFG,
    PASS_AA,
    PASS_DU_MGR,
    PASS_CP,
    PASS_CCP,
    PASS_GCSE,
    PASS_LCSE,
    PASS_RP,
    PASS_PRE,
    PASS_IVR,
    PASS_SCEV,
    PASS_LICM,
    PASS_DCE,
    PASS_DSE,
    PASS_RCE,
    PASS_GVN,
    PASS_DOM,
    PASS_PDOM,
    PASS_DU_REF,
    PASS_LIVE_EXPR,
    PASS_AVAIL_REACH_DEF,
    PASS_REACH_DEF,
    PASS_DU_CHAIN,
    PASS_EXPR_TAB,
    PASS_LOOP_INFO,
    PASS_CDG,
    PASS_LOOP_CVT,
    PASS_RPO,
    PASS_POLY,
    PASS_LIVENESS_MGR,
    PASS_VRP,
    PASS_PR_SSA_MGR,
    PASS_MD_SSA_MGR,
    PASS_CFS_MGR,
    PASS_POLY_TRAN,
    PASS_MD_BUGPATTERN_MGR,
    PASS_IPA,
    PASS_INLINER,
    PASS_REFINE_DUCHAIN,
    PASS_SCALAR_OPT,
    PASS_MDLIVENESS_MGR,
    PASS_NUM,
} PASS_TYPE;

extern CHAR * g_func_or_bb_option;
extern INT g_opt_level; //Represent optimization level.
extern bool g_do_refine; //Perform peephole optimizations.
//If true to insert IR_CVT by ir refinement.
extern bool g_do_refine_auto_insert_cvt;
extern bool g_is_hoist_type; //Hoist data type from less than INT to INT.
extern bool g_do_ipa; //Perform interprocedual analysis and optimization.
extern bool g_do_call_graph; //Build call graph.
extern bool g_show_time; //If true to show compilation time.
extern bool g_do_inline; //Perform function inline.
extern UINT g_inline_threshold; //Record the limit to inline.
extern bool g_is_opt_float; //Optimize float point operation.
extern bool g_is_lower_to_pr_mode; //Lower IR to PR mode.

//Enable XOC support dynamic type.
//That means the type of IR_ST, IR_LD, IR_STPR, IR_PR may be VOID.
extern bool g_is_support_dynamic_type;
extern bool g_do_pr_ssa; //Do optimization in SSA.
extern bool g_do_md_ssa; //Do optimization in Memory SSA.
extern bool g_do_cfg; //Build control flow graph.
extern bool g_do_rpo; //Compute reverse-post-order.
extern bool g_do_loop_ana; //loop analysis.
//Perform cfg optimization: remove empty bb.
extern bool g_do_cfg_remove_empty_bb;
//Perform cfg optimization: remove unreachable bb from entry.
extern bool g_do_cfg_remove_unreach_bb;

//Perform cfg optimization: remove redundant trampoline bb.
//e.g:
//    BB1: goto L1
//    BB2, L1: goto L2
//should be optimized and generate:
//    BB1: goto L2
extern bool g_do_cfg_remove_trampolin_bb;

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
extern bool g_do_cfg_remove_redundant_branch;

//Perform cfg optimization: invert branch condition and
//remove redundant trampoline bb.
//e.g:
//    truebr L4 | false L4
//    goto L3
//    L4
//    ...
//    L3:
//    ...
extern bool g_do_cfg_invert_condition_and_remove_trampolin_bb;
extern bool g_do_cfg_dom; //Build dominator tree.
extern bool g_do_cfg_pdom; //Build post dominator tree.
extern bool g_do_cdg; //Build post dominator tree.
extern bool g_do_aa; //Perform default alias analysis.
//Perform DU analysis for MD to build du chain.
extern bool g_do_md_du_analysis;
//Compute PR DU chain in classic method. 
extern bool g_compute_pr_du_chain;
//Compute NONPR DU chain in classic method.
extern bool g_compute_nonpr_du_chain;
//Computem available expression during du analysis to
//build more precise du chain.
extern bool g_compute_available_exp;
//Computem imported MD which are defined and used in region.
extern bool g_compute_region_imported_defuse_md;
//Build expression table to record lexicographic equally IR expression.
extern bool g_do_expr_tab;
//Perform dead code elimination.
extern bool g_do_dce;

//Set true to eliminate control-flow-structures.
//Note this option may incur user unexpected result:
//e.g: If user is going to write a dead cyclic loop,
//    void non_return()
//    {
//        for (;;) {}
//    }
//Aggressive DCE will remove the above dead cycle.
extern bool g_do_dce_aggressive;
extern bool g_do_cp_aggressive; //It may cost much compile time.
extern bool g_do_cp; //Perform copy propagation.
extern bool g_do_rp; //Perform register promotion.
extern bool g_do_gcse; //Perform global common subexpression elimination.
extern bool g_do_lcse; //Perform local common subexpression elimination.
extern bool g_do_pre; //Perform partial redundant elimination.
extern bool g_do_rce; //light weight redundant code elimination
extern bool g_do_dse; //Perform dead store elimination.
extern bool g_do_licm; //Perform loop invariant code motion.
extern bool g_do_ivr; //Perform induction variable recognization.
extern bool g_do_gvn; //Perform global value numbering.
extern bool g_do_cfs_opt; //Perform control flow structure optimizations.

//Build manager to reconstruct high level control flow structure IR.
//This option is always useful if you want to perform optimization on
//high level IR, such as IF, DO_LOOP, etc.
//Note that if the CFS auxiliary information established, the
//optimizations performed should not violate that.
extern bool g_build_cfs;
extern bool g_cst_bb_list; //Construct BB list.

//Record the maximum limit of the number of IR to perform optimizations.
//This is the threshold to do optimization.
extern UINT g_thres_opt_ir_num;

//Record the maximum limit of the number of BB to perform optimizations.
extern UINT g_thres_opt_bb_num;

//Record the maximum limit of the number of IR in single to
//perform optimizations.
//This is the threshold to do optimization.
extern UINT g_thres_opt_ir_num_in_bb;

//Record the maximum limit of the number of
//PtPair to perform flow sensitive analysis.
extern UINT g_thres_ptpair_num;

extern bool g_do_loop_convert; //Convert while-do to do-while loop.
extern bool g_do_poly_tran; //Polyhedral Transformations.
extern bool g_do_refine_duchain; //Refine DefUse Chain.
extern bool g_do_scalar_opt; //Perform versatile scalar optimizations.

//Set to true to retain the PassMgr even if Region processing finished.
extern bool g_retain_pass_mgr_for_region;

//This variable show the verification level that compiler will perform.
//More higher the level is, more verifications will be performed.
extern UINT g_verify_level;

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
//Note user should definitely confirm that the point-to information
//of parameters of call can be left out if the flag set to false.
extern bool g_is_simplify_parameter;

//Dump after each pass.
extern bool g_is_dump_after_pass;

//Dump before each pass.
extern bool g_is_dump_before_pass;

//Set true to enable searching debug-info from expression bottom up
//to nearest stmt.
extern bool g_is_search_and_copy_dbx;

//Record dump options for each Pass.
extern DumpOpt g_dump_opt;

//Redirect output information to stdout to dump file if exist.
extern bool g_redirect_stdout_to_dump_file;
} //namespace xoc
#endif
