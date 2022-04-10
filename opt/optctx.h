/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _OPT_CTX_H_
#define _OPT_CTX_H_

namespace xoc {

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
#define OC_is_scc_valid(o) ((o).u1.s1.is_scc_valid)
#define OC_is_aa_valid(o) ((o).u1.s1.is_aa_result_valid)
#define OC_is_expr_tab_valid(o) ((o).u1.s1.is_ir_expr_tab)
#define OC_is_cdg_valid(o) ((o).u1.s1.is_cdg_valid)
#define OC_is_dom_valid(o) ((o).u1.s1.is_dom_valid)
#define OC_is_pdom_valid(o) ((o).u1.s1.is_pdom_valid)
#define OC_is_rpo_valid(o) ((o).u1.s1.is_rpo_valid)
#define OC_is_loopinfo_valid(o) ((o).u1.s1.is_loopinfo_valid)
#define OC_is_callg_valid(o) ((o).u1.s1.is_callg_valid)
#define OC_do_merge_label(o) ((o).u1.s1.do_merge_label)
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
            UINT is_scc_valid:1; //SCC of CFG is avaliable.
            UINT is_cdg_valid:1; //CDG is avaliable.

            //Dominator Set, Immediate Dominator are avaliable.
            UINT is_dom_valid:1;

            //Post Dominator Set, Post Immediate Dominator are avaiable.
            UINT is_pdom_valid:1;

            UINT is_loopinfo_valid:1; //Loop info is avaiable.

            UINT is_callg_valid:1; //Call graph is available.

            UINT is_rpo_valid:1; //Rporder is available.

            //If it is true, CFG optimizer will attempt to merge label to
            //next BB if current BB is empty. Default is true.
            UINT do_merge_label:1;
        } s1;
    } u1;

public:
    OptCtx() { init(); }
    OptCtx const& operator = (OptCtx const&);

    bool do_merge_label() const { return OC_do_merge_label(*this); }

    void init() { setAllInvalid(); OC_do_merge_label(*this) = true; } 
    bool is_ref_valid() const { return OC_is_ref_valid(*this); }
    bool is_du_chain_valid() const
    { return is_pr_du_chain_valid() && is_nonpr_du_chain_valid(); }
    bool is_pr_du_chain_valid() const
    { return OC_is_pr_du_chain_valid(*this); }
    bool is_nonpr_du_chain_valid() const
    { return OC_is_nonpr_du_chain_valid(*this); }
    bool is_live_expr_valid() const { return OC_is_live_expr_valid(*this); }
    bool is_reach_def_valid() const { return OC_is_reach_def_valid(*this); }
    bool is_avail_reach_def_valid() const
    { return OC_is_avail_reach_def_valid(*this); }
    bool is_cfg_valid() const { return OC_is_cfg_valid(*this); }
    bool is_scc_valid() const { return OC_is_scc_valid(*this); }
    bool is_aa_valid() const { return OC_is_aa_valid(*this); }
    bool is_expr_tab_valid() const { return OC_is_expr_tab_valid(*this); }
    bool is_cdg_valid() const { return OC_is_cdg_valid(*this); }
    bool is_dom_valid() const { return OC_is_dom_valid(*this); }
    bool is_pdom_valid() const { return OC_is_pdom_valid(*this); }
    bool is_rpo_valid() const { return OC_is_rpo_valid(*this); }
    bool is_loopinfo_valid() const { return OC_is_loopinfo_valid(*this); }
    bool is_callg_valid() const { return OC_is_callg_valid(*this); }

    //The function make all flag valid.
    void setAllValid() { u1.int1 = (UINT)-1; }

    //The function make all flag invalid.
    void setAllInvalid() { u1.int1 = 0; }

    //The function will invalidate flags which may be affected when control
    //flow changed.
    void setInvalidIfCFGChanged()
    {
        //OC_is_cfg_valid(*this) = false; CFG should always be maintained.
        OC_is_scc_valid(*this) = false;
        OC_is_cdg_valid(*this) = false;
        OC_is_rpo_valid(*this) = false;
        OC_is_loopinfo_valid(*this) = false;
        OC_is_scc_valid(*this) = false;
        setDomValid(false);
    }

    //The function will invalidate flags which may be affected when dominator
    //changed.
    void setDomValid(bool valid)
    {
        OC_is_dom_valid(*this) = valid;
        OC_is_pdom_valid(*this) = valid;
    }

    //The function will invalidate flags which may be affected when data-flow
    //changed.
    void setInvalidIfDUMgrLiveChanged()
    {
        OC_is_expr_tab_valid(*this) = false;
        OC_is_live_expr_valid(*this) = false;
        OC_is_reach_def_valid(*this) = false;
        OC_is_avail_reach_def_valid(*this) = false;
    }

    //The function will invalidate flags which may be affected when DU chain
    //changed.
    void setInvalidClassicDUChain()
    {
        OC_is_pr_du_chain_valid(*this) = false;
        OC_is_nonpr_du_chain_valid(*this) = false;
    }
};

} //namespace xoc
#endif
