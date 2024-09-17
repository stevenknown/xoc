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

class Region;
typedef enum _PASS_TYPE PASS_TYPE; //forward declare PASS_TYPE

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
#define OC_is_dom_valid(o) ((o).u1.s1.is_dom_valid)
#define OC_is_pdom_valid(o) ((o).u1.s1.is_pdom_valid)
#define OC_is_rpo_valid(o) ((o).u1.s1.is_rpo_valid)
#define OC_is_loopinfo_valid(o) ((o).u1.s1.is_loopinfo_valid)
#define OC_do_merge_label(o) ((o).u1.s1.do_merge_label)
class OptCtx {
    Region * m_rg;
private:
    void dumpFlag() const;
    void dumpPass() const;
public:
    typedef UINT BitUnion;
    union {
        BitUnion int1;
        struct {
            //Record MUST-DEF, MAY-DEF, MAY-USE MDSet for each IR STMT/EXP.
            BitUnion is_du_ref_valid:1;

            //Record DEF/USE IR stmt/exp for PR operation.
            BitUnion is_pr_du_chain_valid:1;
            //Record DEF/USE IR stmt/exp for NON-PR operation.
            BitUnion is_nonpr_du_chain_valid:1;
            BitUnion is_live_expr_valid:1;
            BitUnion is_reach_def_valid:1;
            BitUnion is_avail_reach_def_valid:1;
            BitUnion is_aa_result_valid:1; //POINT TO info is avaiable.
            BitUnion is_cfg_valid:1; //CFG is avaliable.

            //Dominator Set, Immediate Dominator are avaliable.
            BitUnion is_dom_valid:1;

            //Post Dominator Set, Post Immediate Dominator are avaiable.
            BitUnion is_pdom_valid:1;

            BitUnion is_loopinfo_valid:1; //Loop info is avaiable.

            BitUnion is_rpo_valid:1; //Rporder is available.

            //If it is true, CFG optimizer will attempt to merge label to
            //next BB if current BB is empty. Default is true.
            BitUnion do_merge_label:1;
        } s1;
    } u1;
public:
    OptCtx(Region * rg) { init(rg); }

    void copy(OptCtx const& src) { *this = src; }
    void clean()
    {
        setInvalidAllFlags();
        //Expect label always can be merged.
        OC_do_merge_label(*this) = true;
    }

    bool do_merge_label() const { return OC_do_merge_label(*this); }
    void dump() const;

    Region * getRegion() const { return m_rg; }

    void init(Region * rg)
    {
        ASSERT0(rg);
        m_rg = rg;
        u1.int1 = 0;
        //Expect label always can be merged.
        OC_do_merge_label(*this) = true;
    }
    bool is_ref_valid() const { return OC_is_ref_valid(*this); }

    //Return true if classic PR DU chain is valid.
    bool is_pr_du_chain_valid() const
    { return OC_is_pr_du_chain_valid(*this); }

    //Return true if classic NonPR DU chain is valid.
    bool is_nonpr_du_chain_valid() const
    { return OC_is_nonpr_du_chain_valid(*this); }

    //Return true if live-expression is valid.
    bool is_live_expr_valid() const { return OC_is_live_expr_valid(*this); }

    //Return true if reach-definition is valid.
    bool is_reach_def_valid() const { return OC_is_reach_def_valid(*this); }

    //Return true if available-reach-definition is valid.
    bool is_avail_reach_def_valid() const
    { return OC_is_avail_reach_def_valid(*this); }

    //Return true if CFG is valid.
    bool is_cfg_valid() const { return OC_is_cfg_valid(*this); }

    //Return true if SCC information is valid.
    bool is_scc_valid() const { return isPassValid(PASS_SCC); }

    //Return true if AA information is valid.
    bool is_aa_valid() const { return OC_is_aa_valid(*this); }

    //Return true if Dominator and Immediate-Dominator are valid.
    bool is_dom_valid() const { return OC_is_dom_valid(*this); }

    //Return true if Post-Dominator and Immediate-Post-Dominator are valid.
    bool is_pdom_valid() const { return OC_is_pdom_valid(*this); }

    //Return true if RPO is valid.
    bool is_rpo_valid() const { return OC_is_rpo_valid(*this); }

    //Return true if LoopInfo is valid.
    bool is_loopinfo_valid() const { return OC_is_loopinfo_valid(*this); }

    //Return true if CallGraph is valid.
    bool is_callgraph_valid() const { return isPassValid(PASS_CALL_GRAPH); }

    //Return true if given pass is valid.
    bool isPassValid(PASS_TYPE pt) const;

    //The function will invalidate flags which may be affected when data-flow
    //changed.
    void setInvalidIfDUMgrLiveChanged()
    {
        setInvalidPass(PASS_EXPR_TAB);
        setInvalidPass(PASS_LIVE_EXPR);
        setInvalidPass(PASS_REACH_DEF);
        setInvalidPass(PASS_AVAIL_REACH_DEF);
    }

    //The function will invalidate flags which may be affected when control
    //flow changed.
    static void setInvalidIfCFGChangedExcept(OptCtx * oc, ...);
    void setInvalidIfCFGChanged()
    {
        //OC_is_cfg_valid(*this) = false; CFG should always be maintained.
        setInvalidRPO();
        setInvalidLoopInfo();
        setInvalidDom();
        setInvalidPDom();
        setInvalidCDG();
        setInvalidSCC();
    }

    //The function will invalidate flags which affected while DU chain
    //changed.
    void setInvalidClassicDUChain() { setInvalidPass(PASS_CLASSIC_DU_CHAIN); }
    void setInvalidDom() { setInvalidPass(PASS_DOM); }
    void setInvalidPDom() { setInvalidPass(PASS_PDOM); }
    void setInvalidPRDU() { OC_is_pr_du_chain_valid(*this) = false; }
    void setInvalidNonPRDU() { OC_is_nonpr_du_chain_valid(*this) = false; }
    void setInvalidRPO() { setInvalidPass(PASS_RPO); }
    void setInvalidLoopInfo() { setInvalidPass(PASS_LOOP_INFO); }
    void setInvalidCDG() { setInvalidPass(PASS_CDG); }
    void setInvalidSCC() { setInvalidPass(PASS_SCC); }
    void setInvalidMDSSA() { setInvalidPass(PASS_MDSSA_MGR); }
    void setInvalidPRSSA() { setInvalidPass(PASS_PRSSA_MGR); }
    void setInvalidLiveness() { setInvalidPass(PASS_LIVENESS_MGR); }
    void setInvalidPass(PASS_TYPE pt);

    //The function make all flag invalid.
    void setInvalidAllFlags();
    void setValidPass(PASS_TYPE pt);
};

} //namespace xoc
#endif
