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
class LogMgr;
class RegionMgr;

#define OPT_LEVEL0 0
#define OPT_LEVEL1 1
#define OPT_LEVEL2 2
#define OPT_LEVEL3 3
#define SIZE_OPT 4

#define VERIFY_LEVEL_1 1 //only perform basic verifications.
#define VERIFY_LEVEL_2 2 //do more aggressive check.
#define VERIFY_LEVEL_3 3 //do all verifications.

class StrTabOption : public SymTab {
    COPY_CONSTRUCTOR(StrTabOption);
protected:
    CHAR m_split_char;
protected:
    void parse();
public:
    StrTabOption()
    {
        //By default, the split character is ','.
        m_split_char = ',';
    }
    void addString(CHAR const* str);
    void addString(xcom::StrBuf const& str) { addString(str.getBuf()); }

    void dump(MOD LogMgr * lm) const;
    void dump(RegionMgr * rm) const;

    //Return true if current options includes the given 'str'.
    bool find(CHAR const* str);

    //Return true if current options includes the given 'sym'.
    bool find(Sym const* sym) { return find(const_cast<Sym*>(sym)); }

    //Return true if current string table is empty.
    bool isEmpty() const { return get_elem_count() == 0; }

    //Set the split character when splitting the input string into a list of
    //name.
    void setSplitChar(CHAR c) { m_split_char = c; }
};


class DumpOption {
public:
    //Dump all information.
    //Note is_dump_all and is_dump_nothing can not all be true.
    bool is_dump_all;
    //Dump after pass.
    bool is_dump_after_pass;
    //Dump before pass.
    bool is_dump_before_pass;
    //Do not dump anything.
    //Note is_dump_all and is_dump_nothing can not all be true.
    bool is_dump_nothing;
    bool is_dump_aa; //Dump Alias Analysis information.
    bool is_dump_dumgr; //Dump MD Def-Use chain built by DU Manager.
    bool is_dump_duref; //Dump MD Def-Use reference built both
                        //by AA and DU Manager.
    bool is_dump_mdset_hash; //Dump MD Set Hash Table.
    bool is_dump_cfg; //Dump CFG.
    bool is_dump_cfgopt; //Dump CFG after CFG optimizations.
    bool is_dump_dom; //Dump Dom/Pdom/Idom/Pidom.
    bool is_dump_rpo; //Dump RPO.
    bool is_dump_cp; //Dump Copy Propagation.
    bool is_dump_rp; //Dump Register Promotion.
    bool is_dump_rce; //Dump light weight Redundant Code Elimination.
    bool is_dump_dce; //Dump Dead Code Elimination.
    bool is_dump_vrp; //Dump Value Range Propagation.
    bool is_dump_infertype; //Dump Infer Type.
    bool is_dump_invert_brtgt; //Dump Invert Branch Target.
    bool is_dump_lftr; //Dump Linear Function Test Replacement.
    bool is_dump_vectorization; //Dump IR Vectorization.
    bool is_dump_multi_res_convert; //Dump Multiple Result Convert.
    bool is_dump_loop_dep_ana; //Dump Loop Dependence Analysis.
    bool is_dump_gvn; //Dump Global Value Numbering.
    bool is_dump_gcse; //Dump Global Common Subexpression Elimination.
    bool is_dump_ivr; //Dump Induction Variable Recognization.
    bool is_dump_licm; //Dump Loop Invariant Code Motion.
    bool is_dump_exprtab; //Dump Expr Tab.
    bool is_dump_loopcvt; //Dump Loop Convertion.
    bool is_dump_simplification; //Dump IR simplification.
    bool is_dump_prssamgr; //Dump PRSSAMgr.
    bool is_dump_mdssamgr; //Dump MDSSAMgr.
    bool is_dump_memusage; //Dump memory usage.
    bool is_dump_livenessmgr; //Dump LivenessMgr.
    bool is_dump_irparser; //Dump IRParser.
    bool is_dump_refine_duchain; //Dump RefineDUChain.
    bool is_dump_refine; //Dump Refinement.
    bool is_dump_insert_cvt; //Dump InsertCvt.
    bool is_dump_calc_derivative; //Dump Derivative Cacluation.
    bool is_dump_gscc; //Dump GSCC.
    bool is_dump_cdg; //Dump Control Dependence Graph.
    bool is_dump_lsra; //Dump LinearScanRA
    bool is_dump_to_buffer; //Dump info to buffer
    bool is_dump_pelog; //Dump PrologueEpilogue
    //The option determines whether IR dumper dumps the IR's id when dumpIR()
    //invoked. It should be set to false when the dump information is used in
    //basedump file in testsuite, because the id may be different in different
    //compilation.
    bool is_dump_ir_id;
    bool is_dump_gp_adjustment; //Dump GlobalPointerAdjustment
    bool is_dump_br_opt; //Dump ir after BROpt.

    //Used to dump the reorder functionality result in the LSRA PASS to verify
    //the reorder result for the multiple MOV IRs if the reorder is required
    //due to the USE dependencies problem.
    bool is_dump_lsra_reorder_mov_in_latch_BB;
    bool is_dump_argpasser; //Dump ArgPasser.
    bool is_dump_irreloc; //Dump IRReloc.
public:
    DumpOption();
    DumpOption const& operator = (DumpOption const&); //Disable operator =.

    bool isDumpAll() const;
    bool isDumpNothing() const;
    bool isDumpBeforePass() const;
    bool isDumpAfterPass() const;
    bool isDumpAA() const;
    bool isDumpDUMgr() const;
    bool isDumpMDSetHash() const;
    bool isDumpCFG() const;
    bool isDumpCFGOpt() const;
    bool isDumpDOM() const;
    bool isDumpRPO() const;
    bool isDumpCP() const;
    bool isDumpRP() const;
    bool isDumpRCE() const;
    bool isDumpDCE() const;
    bool isDumpVRP() const;
    bool isDumpInferType() const;
    bool isDumpInvertBrTgt() const;
    bool isDumpLFTR() const;
    bool isDumpVectorization() const;
    bool isDumpMultiResConvert() const;
    bool isDumpLoopDepAna() const;
    bool isDumpGVN() const;
    bool isDumpGCSE() const;
    bool isDumpIVR() const;
    bool isDumpLICM() const;
    bool isDumpExprTab() const;
    bool isDumpLoopCVT() const;
    bool isDumpSimp() const;
    bool isDumpPRSSAMgr() const;
    bool isDumpMDSSAMgr() const;
    bool isDumpCG() const;
    bool isDumpRA() const;
    bool isDumpMemUsage() const;
    bool isDumpLivenessMgr() const;
    bool isDumpIRParser() const;
    bool isDumpRefineDUChain() const;
    bool isDumpRefine() const;
    bool isDumpInsertCvt() const;
    bool isDumpCalcDerivative() const;
    bool isDumpGSCC() const;
    bool isDumpCDG() const;
    bool isDumpLIS() const;
    bool isDumpIRID() const;
    bool isDumpLSRA() const;
    bool isDumpToBuffer() const;
    bool isDumpPElog() const;
    bool isDumpGPAdjustment() const;
    bool isDumpBROpt() const;
    bool isDumpLSRAReorderMovInLatchBB() const;
    bool isDumpArgPasser() const;
    bool isDumpIRReloc() const;
};

class Option {
public:
    Option() {}

    //The function dump all options information.
    static void dump(MOD LogMgr * lm);
    static void dump(RegionMgr * rm);

    //The function return the string name of given optimization level.
    static CHAR const* getOptLevelName(UINT optlevel);

    //The function return the string name of given verification level.
    static CHAR const* getVerifyLevelName(UINT verifylevel);
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
    PASS_INFER_TYPE,
    PASS_INVERT_BRTGT,
    PASS_LFTR,
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
    PASS_PRSSA_MGR,
    PASS_MDSSA_MGR,
    PASS_CFS_MGR,
    PASS_POLY_TRAN,
    PASS_MD_BUGPATTERN_MGR,
    PASS_IPA,
    PASS_INLINER,
    PASS_REFINE_DUCHAIN,
    PASS_SCALAR_OPT,
    PASS_MDLIVENESS_MGR,
    PASS_MDSSALIVE_MGR,
    PASS_REFINE,
    PASS_INSERT_CVT,
    PASS_CALC_DERIVATIVE,
    PASS_SCC,
    PASS_IRSIMP,
    PASS_LINEAR_SCAN_RA,
    PASS_IRMGR,
    PASS_CALL_GRAPH,
    PASS_VECT,
    PASS_MULTI_RES_CVT,
    PASS_LOOP_DEP_ANA,
    PASS_PROLOGUE_EPILOGUE,
    PASS_GP_ADJUSTMENT,
    PASS_BR_OPT,
    PASS_WORKAROUND,
    PASS_DYNAMIC_STACK,
    PASS_IRRELOC,
    PASS_ARGPASSER,
    PASS_IGOTO_OPT,
    PASS_NUM,
} PASS_TYPE;

extern CHAR * g_func_or_bb_option;

//Represent optimization level.
extern INT g_opt_level;

//Perform peephole optimizations.
extern bool g_do_refine;

//Perform more aggressive peephole optimizations.
//e.g: cos:f64 5.0:f64 will be refined to 0.9961947:f64 directly.
extern bool g_do_refine_with_host_api;

//If true to insert IR_CVT if necessary.
extern bool g_insert_cvt;

//If true to calculate derivative.
extern bool g_calc_derivative;

//If true to hoist short type to integer type.
//Hoist data type from less than INT to INT.
extern bool g_is_hoist_type;

//Perform interprocedual analysis and optimization.
extern bool g_do_ipa;

//Build call graph.
extern bool g_do_call_graph;

//If true to show compilation time.
extern bool g_show_time;

//Perform function inline.
extern bool g_do_inline;

//Record the limit to inline.
extern UINT g_inline_threshold;

//Optimize float point operation.
extern bool g_is_opt_float;

//Lower IR to PR mode.
//The simplification will guarrantee that all value in computation will be
//stored in PR. The behavior is just like a RISC machine.
//e.g: given st a = add ld b, ld c; will be simplied to
//  stpr $1 = ld a;
//  stpr $2 = ld b;
//  stpr $3 = add $1, $2;
//  st a = $3;
extern bool g_is_lower_to_pr_mode;

//Lower IR to the lowest tree height.
//Note the lowest height means tree height is more than 2.
//e.g: st a = add ld b, ld c; will not changed.
//e.g2: st a = add ld b, (add ld c, ld d); will be simplied to
// stpr $1 = add ld c, ld d;
// st a = add ld b, $1;
extern bool g_is_lower_to_lowest_height;

//Enable XOC support dynamic type.
//That means the type of IR_ST, IR_LD, IR_STPR, IR_PR may be ANY.
extern bool g_is_support_dynamic_type;

//Build PR SSA and perform optimization based on SSA.
extern bool g_do_prssa;

//Build Memory SSA and perform optimization based on Memory SSA.
extern bool g_do_mdssa;

//Build control flow graph.
extern bool g_do_cfg;

//Compute reverse-post-order.
extern bool g_do_rpo;

//Perform loop analysis.
extern bool g_do_loop_ana;

//Perform cfg optimization: remove labels that no one referenced.
extern bool g_do_cfg_remove_redundant_label;

//Perform cfg optimization: remove empty BB.
extern bool g_do_cfg_remove_empty_bb;

//Perform cfg optimization: the maximum times to update DomInfo when removing
//empty BB.
extern UINT g_cfg_remove_empty_bb_maxtimes_to_update_dominfo;

//Perform cfg optimization: remove unreachable BB from entry.
extern bool g_do_cfg_remove_unreach_bb;

//Perform cfg optimization: remove redundant trampoline BB.
//e.g:
//    BB1: goto L1
//    BB2, L1: goto L2
//should be optimized and generate:
//    BB1: goto L2
extern bool g_do_cfg_remove_trampolin_bb;

//Remove trampoline branch.
//Note the pass is different from what removeTrampolinEdge() does.
//e.g:L2:
//    truebr L4 | false L4
//    goto L3 //redundant jump
//    L4
//    st = ...
//    L3:
//    ...
//=>
//    L2:
//    falsebr L3 | truebr L3
//    EMPTY BB
//    L4:
//    st = ...
//    L3:
extern bool g_do_cfg_remove_trampolin_branch;

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

//Build dominator tree.
extern bool g_do_cfg_dom;

//Build post dominator tree.
extern bool g_do_cfg_pdom;

//Build control dependence graph.
extern bool g_do_cdg;

//Perform default alias analysis.
extern bool g_do_aa;

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

//Perform value range propagation.
extern bool g_do_vrp;

//Perform type inference.
extern bool g_infer_type;

//Perform cfg optimization: invert branch condition and target.
extern bool g_invert_branch_target;

//Set true to eliminate control-flow-structures.
//Note this option may incur user unexpected result:
//e.g: If user is going to write a dead cyclic loop,
//    void non_return()
//    {
//        for (;;) {}
//    }
//Aggressive DCE will remove the above dead cycle.
extern bool g_do_dce_aggressive;

//Perform linear function test replacement.
extern bool g_do_lftr;

//Perform aggressive copy propagation.
//It may cost much compile time.
extern bool g_do_cp_aggressive;

//Perform copy propagation.
extern bool g_do_cp;

//Perform register promotion.
extern bool g_do_rp;

//Perform global common subexpression elimination.
extern bool g_do_gcse;

//Perform local common subexpression elimination.
extern bool g_do_lcse;

//Perform partial redundant elimination.
extern bool g_do_pre;

//Perform light weith redundant code elimination.
extern bool g_do_rce;

//Perform auto vectorization.
extern bool g_do_vect;

//Perform multiple result convert.
extern bool g_do_multi_res_convert;

//Perform loop dependence analysis.
extern bool g_do_loop_dep_ana;

//Perform dead store elimination.
extern bool g_do_dse;

//Perform loop invariant code motion.
extern bool g_do_licm;

//Perform loop invariant code motion without inserting loop guard.
extern bool g_do_licm_no_guard;

//Perform induction variable recognization.
extern bool g_do_ivr;

//Perform global value numbering.
extern bool g_do_gvn;

//Perform control flow structure optimizations.
extern bool g_do_cfs_opt;

//Build manager to reconstruct high level control flow structure IR.
//This option is always useful if you want to perform optimization on
//high level IR, such as IF, DO_LOOP, etc.
//Note that if the CFS auxiliary information established, the
//optimizations performed should not violate that.
extern bool g_build_cfs;

//Construct BB list.
extern bool g_cst_bb_list;

//If the flag is true, MDSystem will add delegate of region local variable
//into overlapping MDSet for each MD.
//Note the flag is always stand for all region local variables, it's flaw
//is that if the flag appeared in overlapping MDSet of an IR, the DU chain
//that built by DUMgr or SSAMgr will be conservative.
extern bool g_enable_local_var_delegate;

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

//Convert while-do to do-while loop.
extern bool g_do_loop_convert;

//Polyhedral Transformations.
extern bool g_do_poly_tran;

//Refine DefUse Chain.
extern bool g_do_refine_duchain;

//Linear Scan Register Allocation.
extern bool g_do_lsra;

//Insert prologue and epilogue code.
extern bool g_do_pelog;

//Perform versatile scalar optimizations.
extern bool g_do_scalar_opt;

//Perform global pointer adjustment.
extern bool g_do_gp_adjustment;

//Perform relaxation.
extern bool g_do_relaxation;

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

//Set true to simplify array ingredients, such as, subscript expression, array
//base expression to lower height.
extern bool g_is_simplify_array_ingredient;

//Set true to enable searching debug-info from expression bottom up
//to nearest stmt.
extern bool g_is_search_and_copy_dbx;

//Set true to generate variable when building a PR.
//Usually, we assign variable to PR to indicate its identity in order to
//enable the pass that dependent on the analysis of MD.
//If user only consider PRNO in pass, Var and MD is dispensable.
extern bool g_generate_var_for_pr;

//Record dump options for each Pass.
extern DumpOption g_dump_opt;

//Redirect output information to stdout to dump file if exist.
extern bool g_redirect_stdout_to_dump_file;

//Record the unique file handler for dump file.
//Note the order of access to this file will not be guaranteed
//in serial execution when there are multiple RegionMgrs doing compilation
//simultaneously.
extern FILE * g_unique_dumpfile;
extern CHAR const* g_unique_dumpfile_name;

//Rocord a list of Region that should participate in optimization.
extern StrTabOption g_include_region;

//Rocord a list of Region that should NOT participate in optimization.
extern StrTabOption g_exclude_region;

//Stack is located in SPM by default, which is set to false.
//If it is located in HBM, it is set to true.
extern bool g_stack_on_global;

#ifdef FOR_TECO_IP
//Forced relaxation of cond-br to uncond-br.
extern bool g_do_relaxation2uncondbr;

//Forced relaxation of cond-br or uncond-br to jmp.
extern bool g_do_relaxation2jmp;

#endif
//Used to enable the debug mode for LSRA, and the g_debug_reg_num can be use
//to control the number of physical register under debug mode.
extern bool g_do_lsra_debug;
extern UINT g_debug_reg_num;
//Support alloca.
extern bool g_support_alloca;
//Enable fp as stack pointer.
extern bool g_force_use_fp_as_sp;
//Perform ir reloc.
extern bool g_do_ir_reloc;
//Enable arg passer.
extern bool g_do_arg_passer;
//Recycle local Var id and related MD id when destorying regions.
extern bool g_recycle_local_id;

//Enable debug.
extern bool g_debug;

//The front end is in debug_cpp mode.
extern bool g_debug_cpp;

//The front end is in debug_pcx mode.
extern bool g_debug_pcx;

//The front end is in debug_python mode.
extern bool g_debug_pyhton;

//Use the accurate algorithm to find the PRs with 2d lifetime hole, which is
//time-consuming but with higher performance of LSRA result.
extern bool g_use_accurate_2d_lifetime_hole_algo;
} //namespace xoc
#endif
