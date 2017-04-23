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

#define OPT_LEVEL0      0
#define OPT_LEVEL1      1
#define OPT_LEVEL2      2
#define OPT_LEVEL3      3
#define SIZE_OPT        4

#define VERIFY_LEVEL_1  1 //only perform basic verifications.
#define VERIFY_LEVEL_2  2 //do more aggressive check.
#define VERIFY_LEVEL_3  3 //do all verifications.

//Optimization Context
//This class record and propagate auxiliary information to optimizations.
#define OC_is_ref_valid(o)              ((o).u1.s1.is_du_ref_valid)
#define OC_is_du_chain_valid(o)         ((o).u1.s1.is_du_chain_valid)
#define OC_is_live_expr_valid(o)        ((o).u1.s1.is_live_expr_valid)
#define OC_is_reach_def_valid(o)        ((o).u1.s1.is_reach_def_valid)
#define OC_is_avail_reach_def_valid(o)  ((o).u1.s1.is_avail_reach_def_valid)
#define OC_is_cfg_valid(o)              ((o).u1.s1.is_cfg_valid)
#define OC_is_aa_valid(o)               ((o).u1.s1.is_aa_result_valid)
#define OC_is_expr_tab_valid(o)         ((o).u1.s1.is_ir_expr_tab)
#define OC_is_cdg_valid(o)              ((o).u1.s1.is_cdg_valid)
#define OC_is_dom_valid(o)              ((o).u1.s1.is_dom_valid)
#define OC_is_pdom_valid(o)             ((o).u1.s1.is_pdom_valid)
#define OC_is_rpo_valid(o)              ((o).u1.s1.is_rpo_valid)
#define OC_is_loopinfo_valid(o)         ((o).u1.s1.is_loopinfo_valid)
#define OC_is_callg_valid(o)            ((o).u1.s1.is_callg_valid)
#define OC_show_comp_time(o)            ((o).u2.s1.show_compile_time)
class OptCtx {
public:
    union {
        UINT int1;
        struct {
            //Record MUST-DEF, MAY-DEF, MAY-USE MDSet for each IR STMT/EXP.
            UINT is_du_ref_valid:1;

            UINT is_du_chain_valid:1; //Record DEF, USE IR List for IR STMT.
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
    OptCtx()
    {
        set_all_invalid();
        u2.int1 = 0;
    }

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

    inline bool is_all_intra_valid()
    {
        return OC_is_ref_valid(*this) &&
               OC_is_du_chain_valid(*this) &&
               OC_is_live_expr_valid(*this) &&
               OC_is_reach_def_valid(*this) &&
               OC_is_avail_reach_def_valid(*this) &&
               OC_is_aa_valid(*this) &&
               OC_is_expr_tab_valid(*this) &&
               OC_is_dom_valid(*this) &&
               OC_is_pdom_valid(*this) &&
               OC_is_cfg_valid(*this) &&
               OC_is_cdg_valid(*this) &&
               OC_is_rpo_valid(*this) &&
               OC_is_loopinfo_valid(*this);
    }
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
    PASS_PRDF,
    PASS_VRP,
    PASS_PR_SSA_MGR,
    PASS_MD_SSA_MGR,
    PASS_CFS_MGR,
    PASS_POLY_TRAN,
    PASS_MD_BUGPATTERN_MGR,
    PASS_IPA,
    PASS_INLINER,
    PASS_NUM,
} PASS_TYPE;

extern CHAR * g_func_or_bb_option;
extern INT g_opt_level;
extern bool g_do_gra;
extern bool g_do_refine;
extern bool g_do_refine_auto_insert_cvt;
extern bool g_is_hoist_type; //Hoist data type from less than INT to INT.
extern bool g_do_ipa;
extern bool g_do_call_graph; //Build call graph.
extern bool g_show_comp_time;
extern bool g_show_memory_usage; //Show the memory usage to dump file.
extern bool g_do_inline;
extern UINT g_inline_threshold;
extern bool g_is_opt_float; //Optimize float point operation.
extern bool g_is_lower_to_pr_mode; //Lower IR to PR mode.

//Enable XOC support dynamic type.
//That means the type of IR_ST, IR_LD, IR_STPR, IR_PR may be VOID.
extern bool g_is_support_dynamic_type;

extern bool g_do_pr_ssa; //Do optimization in SSA.
extern bool g_do_md_ssa; //Do optimization in Memory SSA.
extern bool g_do_cfg;
extern bool g_do_rpo;
extern bool g_do_loop_ana; //loop analysis.
extern bool g_do_cfg_remove_empty_bb;
extern bool g_do_cfg_remove_unreach_bb;
extern bool g_do_cfg_remove_trampolin_bb;
extern bool g_do_cfg_remove_redundant_branch;
extern bool g_do_cfg_dom;
extern bool g_do_cfg_pdom;
extern bool g_do_cdg;
extern bool g_do_aa;
extern bool g_do_md_du_ana;
extern bool g_compute_du_chain;
extern bool g_compute_available_exp;
extern bool g_compute_region_imported_defuse_md;
extern bool g_do_expr_tab;

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
extern bool g_do_cp;
extern bool g_do_rp;
extern bool g_do_gcse;
extern bool g_do_lcse;
extern bool g_do_pre;
extern bool g_do_rce;
extern bool g_do_dse;
extern bool g_do_licm;
extern bool g_do_ivr;
extern bool g_do_gvn;
extern bool g_do_cfs_opt;
extern bool g_build_cfs;
extern bool g_cst_bb_list; //Construct BB list.
extern UINT g_thres_opt_ir_num;
extern UINT g_thres_opt_bb_num;
extern bool g_do_loop_convert;
extern bool g_do_poly_tran;
extern bool g_retain_pass_mgr_for_region;
extern UINT g_verify_level;

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
//Note user should definitely confirm that the point-to information 
//of parameters of call can be left out if the flag set to false.
extern bool g_is_simplify_parameter;

//Dump MD Set Hash Table.
extern bool g_is_dump_mdset_hash;

//Dump MD DU chain built by DU Manager.
extern bool g_is_dump_du_chain;
} //namespace xoc
#endif
