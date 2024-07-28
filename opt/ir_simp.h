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
#ifndef _IR_SIMP_H_
#define _IR_SIMP_H_

namespace xoc {

class CfsMgr;

#define MAX_SIMP_WORD_LEN  1

#define SIMP_if(s) (s)->prop_top_down.s1.simp_if
#define SIMP_doloop(s) (s)->prop_top_down.s1.simp_do_loop
#define SIMP_dowhile(s) (s)->prop_top_down.s1.simp_do_while
#define SIMP_whiledo(s) (s)->prop_top_down.s1.simp_while_do
#define SIMP_switch(s) (s)->prop_top_down.s1.simp_switch
#define SIMP_select(s) (s)->prop_top_down.s1.simp_select
#define SIMP_array(s) (s)->prop_top_down.s1.simp_array
#define SIMP_break(s) (s)->prop_top_down.s1.simp_break
#define SIMP_continue(s) (s)->prop_top_down.s1.simp_continue
#define SIMP_lor_land(s) (s)->prop_top_down.s1.simp_logcial_or_and
#define SIMP_lnot(s) (s)->prop_top_down.s1.simp_logcial_not
#define SIMP_ild_ist(s) (s)->prop_top_down.s1.simp_ild_ist
#define SIMP_to_pr_mode(s) (s)->prop_top_down.s1.simp_to_pr_mode
#define SIMP_array_to_pr_mode(s) (s)->prop_top_down.s1.simp_array_to_pr_mode
#define SIMP_to_lowest_height(s) (s)->prop_top_down.s1.simp_to_lowest_height
#define SIMP_ret_array_val(s) (s)->prop_top_down.s1.simp_to_get_array_value
#define SIMP_cfs_only(s) (s)->prop_top_down.s1.simp_cfs_only
#define SIMP_is_record_cfs(s) (s)->prop_top_down.s1.is_record_cfs
#define SIMP_stmtlist(s) (s)->ir_stmt_list
#define SIMP_break_label(s) (s)->break_label
#define SIMP_continue_label(s) (s)->continue_label
#define SIMP_optctx(s) (s)->optctx
#define SIMP_changed(s) (s)->prop_bottom_up.s1.something_has_changed
#define SIMP_need_recon_bblist(s) \
    (s)->prop_bottom_up.s1.need_to_reconstruct_bb_list
#define SIMP_need_rebuild_pr_du_chain(s) \
    (s)->prop_bottom_up.s1.need_to_rebuild_pr_du_chain
#define SIMP_need_rebuild_nonpr_du_chain(s) \
    (s)->prop_bottom_up.s1.need_to_rebuild_nonpr_du_chain
#define SIMP_cfs_mgr(s) (s)->cfs_mgr

//The class represents the simplification behaviors.
//Each behavior has an option corresponding to it.
//Enable or disable options to control the related simplification.
class SimpCtx {
public:
    typedef UINT BitUnion;
    union {
        BitUnion flag_value;
        struct {
            //Propagate these flags top down to simplify IR.
            BitUnion simp_if:1; //simplify IF.
            BitUnion simp_do_loop:1; //simplify DO_LOOP.
            BitUnion simp_do_while:1; //simplify DO_WHILE.
            BitUnion simp_while_do:1; //simplify WHILE_DO.
            BitUnion simp_switch:1; //simplify SWITCH.
            BitUnion simp_select:1; //simplify SELECT.
            BitUnion simp_array:1; //simplify ARRAY.
            BitUnion simp_break:1; //simplify BREAK.
            BitUnion simp_continue:1; //simplify CONTINUE.
            BitUnion simp_logcial_or_and:1; //simplify LOR, LAND.
            BitUnion simp_logcial_not:1; //simplify LNOT.
            BitUnion simp_ild_ist:1; //simplify ILD and IST.

            //Propagate info top down.
            //Simplify IR tree to the tree with lowest height,
            //that means the tree height is not more than 2,
            //namely, non-leaf IR is no more than 1.
            //e.g: id = v2 + v3 is the lowest tree.
            //the IR format is:
            //  st(id, add(ld(v2), ld(v3)))
            //Here, add is non-leaf IR, its children can
            //not be non-leaf node anymore.
            BitUnion simp_to_lowest_height:1;

            //Propagate info top down.
            //Operand only can be PR.
            //e.g: st(id1, ild(ld(v2))), converted to:
            //        pr1=ld(v1)
            //        pr2=ld(v2)
            //        pr3=ild(P2)
            //        st(pr1, pr3)
            //And this IR tree is unpermittable: ADD(LD(ID1), P1)
            BitUnion simp_to_pr_mode:1;

            //Propagate info top down.
            //Store array value into individual PR, but keep array operation
            //unchanged.
            //e.g:
            //    add(array(a), array(b))
            //converted to:
            //    pr1 = array(a)
            //    pr2 = array(b)
            //    add(pr1, pr2)
            BitUnion simp_array_to_pr_mode:1;

            //Propagate info top down.
            //If it is true function return array's value, or else return
            //the array address.
            //The flag often used in RHS simplification.
            //
            //e.g: Given ... = a[i][j], we need to get the value of a[i][j].
            //If the flag is false, function return the address expression:
            //    &a + i*elem_size + j,
            //Or else return ILD(&a + i*elem_size + j).
            BitUnion simp_to_get_array_value:1;

            //Propagate info top down.
            //If it is true, simplfy Control-Flow-Struct stmt only.
            BitUnion simp_cfs_only:1;

            //Propagate info top down.
            //Record high level Control-Flow-Struct info.
            BitUnion is_record_cfs:1;
        } s1;
    } prop_top_down;

    union {
        BitUnion flag_value;
        struct {
            //Propagate info bottom up.
            //Record whether exp or stmt has changed.
            BitUnion something_has_changed:1;

            //Propagate info bottom up.
            //To inform Region to reconstruct bb list.
            //If this flag is true, DU info and
            //DU chain also need to be rebuilt.
            BitUnion need_to_reconstruct_bb_list:1;

            //Propagate info bottom up.
            //To inform Region to rebuild PR DU chain.
            //The DU chain is either classic DU chain or PRSSA.
            //If this flag is true, DU info and
            //DU chain also need to be rebuilt.
            BitUnion need_to_rebuild_pr_du_chain : 1;

            //Propagate info bottom up.
            //To inform Region to rebuild NonPR DU chain.
            //The DU chain is either classic DU chain or MDSSA.
            //If this flag is true, DU info and
            //DU chain also need to be rebuilt.
            BitUnion need_to_rebuild_nonpr_du_chain : 1;
        } s1;
    } prop_bottom_up;

    //Record IR stmts which generated bottom up.
    //When simplifing expression, the field records the
    //generated IR STMT list. Always used along with
    //'simp_to_lowest_heigh' and 'simp_to_pr_mode'.
    IR * ir_stmt_list;

    //Record pointer to CfsMgr object.
    CfsMgr * cfs_mgr;

    //Propagate info top down.
    //Record label info for context.
    LabelInfo const* break_label; //record the current LOOP/IF/SWITCH end label.
    LabelInfo const* continue_label; //record the current LOOP start label.
    OptCtx const* optctx; //record current OptCtx for region.
public:
    SimpCtx(OptCtx const* oc) { init(); SIMP_optctx(this) = oc; }
    SimpCtx(SimpCtx const& s)
    {
        clean();
        copyTopDownFlag(s); //only copy topdown information.
    }

    void init() { clean(); }

    //Append irs to current simplification context and
    //return back to up level.
    void appendStmt(SimpCtx & c)
    { xcom::add_next(&SIMP_stmtlist(this), SIMP_stmtlist(&c)); }

    //Append irs to current simplification context and
    //return back to up level.
    void appendStmt(IR * irs)
    { xcom::add_next(&SIMP_stmtlist(this), irs); }

    void clean()
    {
        prop_top_down.flag_value = 0;
        prop_bottom_up.flag_value = 0;
        SIMP_stmtlist(this) = nullptr;
        SIMP_cfs_mgr(this) = nullptr;
        SIMP_optctx(this) = nullptr;
        SIMP_break_label(this) = nullptr;
        SIMP_continue_label(this) = nullptr;
    }

    //Unify the actions which propagated top down
    //during processing IR tree.
    void copyTopDownFlag(SimpCtx const& c)
    {
        prop_top_down = c.prop_top_down;
        SIMP_cfs_mgr(this) = SIMP_cfs_mgr(&c);
        SIMP_cfs_mgr(this) = SIMP_cfs_mgr(&c);
        SIMP_break_label(this) = SIMP_break_label(&c);
        SIMP_continue_label(this) = SIMP_continue_label(&c);
        SIMP_optctx(this) = SIMP_optctx(&c);
        SIMP_stmtlist(this) = nullptr;
    }

    //Copy the actions which propagated bottom up
    //during processing IR tree.
    void copyBottomUpFlag(SimpCtx const& c)
    { prop_bottom_up.flag_value = c.prop_bottom_up.flag_value; }

    //Clean the actions which propagated bottom up
    //during processing IR tree.
    void cleanBottomUpFlag()
    {
        SIMP_changed(this) = false;
        SIMP_need_recon_bblist(this) = false;
        SIMP_need_rebuild_pr_du_chain(this) = false;
        SIMP_need_rebuild_nonpr_du_chain(this) = false;
    }

    //Return the stmt list that recorded in the context.
    IR * getStmtList() { return SIMP_stmtlist(this); }

    //Return the OptCtx that used in current region.
    OptCtx const* getOptCtx() const { return SIMP_optctx(this); }

    //Unify the actions which propagated bottom up
    //during processing IR tree.
    void unionBottomUpInfo(SimpCtx const& c)
    {
        SIMP_changed(this) |= SIMP_changed(&c);
        SIMP_need_recon_bblist(this) |= SIMP_need_recon_bblist(&c);
        SIMP_need_rebuild_pr_du_chain(this) |=
            SIMP_need_rebuild_pr_du_chain(&c);
        SIMP_need_rebuild_nonpr_du_chain(this) |=
            SIMP_need_rebuild_nonpr_du_chain(&c);
    }

    //Return true if BB list need to be reconstructed.
    bool needReconstructBBList() const { return SIMP_need_recon_bblist(this); }

    //Return true if SSA/Classic DU chain need to be rebuild.
    bool needRebuildDUChain() const
    { return needRebuildPRDUChain() || needRebuildNonPRDUChain(); }

    //Return true if PR SSA/Classic DU chain need to be rebuild.
    bool needRebuildPRDUChain() const
    { return SIMP_need_rebuild_pr_du_chain(this); }

    //Return true if NonPR SSA/Classic DU chain need to be rebuild.
    bool needRebuildNonPRDUChain() const
    { return SIMP_need_rebuild_nonpr_du_chain(this); }

    //Set action flags to simplify control flow structure.
    void setSimpCFS()
    {
        SIMP_if(this) = true;
        SIMP_doloop(this) = true;
        SIMP_dowhile(this) = true;
        SIMP_whiledo(this) = true;
        SIMP_switch(this) = true;
        SIMP_break(this) = true;
        SIMP_continue(this) = true;
    }

    //Return true if current simplifying policy
    //involved one of these actions.
    bool isSimpCFG() const
    {
        return SIMP_if(this) ||
               SIMP_doloop(this) ||
               SIMP_dowhile(this) ||
               SIMP_whiledo(this) ||
               SIMP_switch(this) ||
               SIMP_break(this) ||
               SIMP_continue(this);
    }

    //Return true if only simply CFS.
    bool isSimpCFSOnly() const { return SIMP_cfs_only(this); }

    //Simplify IR_ARRAY to linear address computational stmt/expression.
    void setSimpArray() { SIMP_array(this) = true; }

    //Simplify IR tree and reduce the tree height of IST/ILD to be lowest.
    //
    void setSimpILdISt() { SIMP_ild_ist(this) = true; }

    //Simplify IR_SELECT to IR_TRUBR/IR_FALSEBR operation.
    void setSimpSelect() { SIMP_select(this) = true; }

    //Simplify IR_LAND and IR_LOR operation.
    void setSimpLandLor() { SIMP_lor_land(this) = true; }

    //Simplify IR_LNOT operation.
    void setSimpLnot() { SIMP_lnot(this) = true; }

    //Simplify IR tree to be the tree with the lowest height.
    //e.g: The height of 'a + b' is 2, the lowest height,
    //whereas 'a + b + c' is not.
    //Note that if ARRAY/STARRAY/ILD/IST/SELECT are not demanded to be
    //simplied, regarding it as a whole node.
    //e.g: regard 'a[1][2] + b' to be the lowest height.
    void setSimpToLowestHeight()
    {
        ASSERTN(SIMP_lor_land(this) && SIMP_lnot(this),
               ("these operations should be lowered as well."));
        SIMP_to_lowest_height(this) = true;
    }

    //Reduce the tree heigh to lowest and load value to PR to
    //perform operation for IR_LD, IR_ILD, IR_ARRAY.
    //e.g: ist(a, (ld(b) + ld(c)))
    //will be simplified to :
    //    pr1 = a
    //    pr2 = b
    //    pr3 = c
    //    pr4 = pr2 + pr3
    //    ist(pr1, pr4)
    void setSimpToPRmode()
    {
        SIMP_to_pr_mode(this) = true;
        setSimpCFS();
        setSimpArray();
        setSimpILdISt();
        setSimpSelect();
        setSimpLandLor();
        setSimpLnot();
        setSimpToLowestHeight();
    }

    void dump(Region * rg)
    {
        ASSERT0(rg);
        note(rg, "\n==---- DUMP SimpCtx IR List ----==");
        dumpIRList(ir_stmt_list, rg, nullptr);
    }
};


//This pass is very important to multiple level IR compiler. It simplifies or
//transforms one level IR to another lower or equivalent level IR each time.
//The class provides default api and rules to simplify higher level IR to
//lower level IR in XOC framework. Also provides two kinds of simplification
//in general purpose, one is lowest height simplification, the other is PR-mode
//simplification.
//Lowest height means the height of IR tree in stmt will not more than 2.
//e.g: stpr $1 = add (ld a, sub (ld b, ld c));
//  The height of RHS of stpr is 3, after lowest-height simplification,
//  stmt list will be:
//     stpr $x = sub (ld b, ld c) //S2
//     stpr $1 = add (ld a, $x)  //S1
//  The height of RHS of S1 is 2.
//The PR-mode means all opcode only operate PR in each expression/stmt.
//e.g: stpr $1 = add (ld a, ld c);
//  after PR-mode simplification, stmt list will be:
//     stpr $x = ld a
//     stpr $y = ld b
//     stpr $1 = add ($x, $y)
class IRSimp : public Pass {
    COPY_CONSTRUCTOR(IRSimp);
private:
    TypeMgr * m_tm;
    IRMgr * m_irmgr;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
protected:
    //Return true if the tree height is not great than 2.
    //e.g: tree a + b is lowest height , but a + b + c is not.
    //Note that if ARRAY or ILD still not be lowered at the moment, regarding
    //it as a whole node. e.g: a[1][2] + b is the lowest height.
    bool isLowestHeight(IR const* ir, SimpCtx const* ctx) const;
    virtual bool isLowestHeightExp(IR const* ir, SimpCtx const* ctx) const;
    virtual bool isLowestHeightExtExp(IR const* ir) const
    {
        //Target Dependent Code.
        ASSERT0(ir->is_exp());
        return isLowest(ir);
    }
    //At lowest mode, the predicator, trueexp, falseexp must be leaf.
    //Note the lowest height means tree height is more than 2.
    //e.g: ... = add ld a, ld b; ADD is the lowest height.
    bool isLowestHeightSelect(IR const* ir) const;

    bool isLowest(IR const* ir) const;

    //At lowest mode, the array base, array subscript-expression must be leaf.
    bool isLowestHeightArrayOp(IR const* ir) const;

    //Check the call is used for special register or not.
    //Some target will define an intrinsic-call to operate target special
    //register, such as IO configure register. The interface is used to
    //determine whether current call-stmt is need to simplify.
    virtual bool isSpecialRegCall(IR const* ir) const
    {
        //Target Dependent Code
        return false;
    }

    //Return true if current simplification should maintain the DU chain as
    //much as possible, otherwise the function have to inform the PRSSAMgr,
    //MDSSAMgr, and Classic DUMgr to rebuild DU chain.
    bool needMaintainDUChain(SimpCtx const& ctx);

    IR * simplifyNormal(IR * ir, SimpCtx * ctx);

    //Simplfy RHS in prmode.
    //Convert st x = rhs --> stpr1 = rhs, st x = pr1.
    //Note the function only handle ir's RHS, return NULL if there is no
    //stmt generated.
    virtual IR * simplifyRHSInPRMode(IR * ir, SimpCtx * ctx);
    void simplifyStoreArrayRHS(IR * ir, OUT IR ** ret_list,
                               OUT IR ** last, SimpCtx * ctx);
    IR * simplifyStoreArrayAddr(IR * ir, OUT IR ** ret_list,
                                OUT IR ** last, SimpCtx * ctx);
    IR * simplifyStoreArrayLHS(IR * ir, OUT IR ** ret_list,
                               OUT IR ** last, SimpCtx * ctx);
    IR * simplifyArraySelf(IR * ir, IR * array_addr, SimpCtx * ctx);
    IR * simplifyArrayLowestHeight(IR * ir, IR * array_addr, SimpCtx * ctx);
    IR * simplifyArrayPRMode(IR * ir, IR * array_addr, SimpCtx * ctx);
    IR * simplifyArrayAddrID(IR * ir, IR * array_addr, SimpCtx * ctx);
    bool simplifyCallParamList(IR * ir, IR ** ret_list, IR ** last,
                               SimpCtx * ctx);
    IR * simplifyAllKidsExpression(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyCallPlaceholder(IR * ir, SimpCtx *)
    { ASSERTN(0, ("Target Dependent Code")); return ir; }
    virtual IR * simplifyExtStmt(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyExtExp(IR * ir, SimpCtx * ctx);
    virtual IR * simplifySpecialRegCall(IR * ir, SimpCtx * ctx)
    { ASSERTN(0, ("Target Dependent Code")); return ir; }
    IR * simplifyDummyUse(IR * ir, SimpCtx *) { return ir; }

    bool useMDSSADU() const;
    bool usePRSSADU() const;
public:
    explicit IRSimp(Region * rg) : Pass(rg)
    {
        m_tm = rg->getTypeMgr();
        m_irmgr = rg->getIRMgr();
        m_mdssamgr = nullptr;
        m_prssamgr = nullptr;
    }
    virtual ~IRSimp() {}

    virtual CHAR const* getPassName() const
    { return "IR Simplification"; }
    virtual PASS_TYPE getPassType() const { return PASS_IRSIMP; }
    IRMgr * getIRMgr() const { return m_irmgr; }

    //Series of helper functions to simplify
    //ir according to given specification.
    virtual IR * simplifyLoopIngredient(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyBranch(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyIfSelf(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyDoWhileSelf(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyWhileDoSelf(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyDoLoopSelf(IR * ir, SimpCtx * ctx);
    virtual IR * simplifySwitchSelf(IR * ir, SimpCtx * ctx);
    virtual void simplifySelectKids(IR * ir, SimpCtx * cont);
    virtual IR * simplifyDirectMemOp(IR * ir, SimpCtx * cont);
    virtual IR * simplifyIndirectStmt(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyIndirectExp(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyIndirectMemOp(IR * ir, SimpCtx * cont);
    virtual void simplifyCalleeExp(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyArrayIngredient(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyStoreArray(IR * ir, SimpCtx * ctx);
    virtual IR * simplifySetelem(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyGetelem(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyCall(IR * ir, SimpCtx * cont);
    virtual IR * simplifyIf(IR * ir, SimpCtx * cont);
    virtual IR * simplifyWhileDo(IR * ir, SimpCtx * cont);
    virtual IR * simplifyDoWhile (IR * ir, SimpCtx * cont);
    virtual IR * simplifyDoLoop(IR * ir, SimpCtx * cont);
    virtual IR * simplifyDet(IR * ir, SimpCtx * cont);
    virtual IR * simplifyJudgeDet(IR * ir, SimpCtx * cont);
    virtual IR * simplifySelect(IR * ir, SimpCtx * cont);
    virtual IR * simplifySwitch (IR * ir, SimpCtx * cont);
    virtual IR * simplifyReturn(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyCase(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyGoto(IR * ir, SimpCtx * ctx);
    virtual IR * simplifyIgoto(IR * ir, SimpCtx * cont);
    virtual IR * simplifyArrayAddrExp(IR * ir, SimpCtx * cont);
    virtual IR * simplifyArray(IR * ir, SimpCtx * cont);
    IR * simplifyExpressionList(IR * irlst, SimpCtx * cont);

    //Return new generated expression's value.
    //ir: ir may be in parameter list if its prev or next is not empty.
    virtual IR * simplifyExpression(IR * ir, SimpCtx * cont);
    virtual IR * simplifyStmt(IR * ir, SimpCtx * cont);
    virtual IR * simplifyStmtList(IR * ir, SimpCtx * cont);
    virtual void simplifyBB(IRBB * bb, SimpCtx * cont);
    virtual void simplifyBBlist(BBList * bbl, SimpCtx * cont);
    virtual void simplifyIRList(SimpCtx * cont);
    virtual IR * simplifyLogicalNot(IN IR * ir, SimpCtx * cont);
    virtual IR * simplifyLogicalOrAtFalsebr(IN IR * ir,
                                            LabelInfo const* tgt_label);
    virtual IR * simplifyLogicalOrAtTruebr(IN IR * ir,
                                           LabelInfo const* tgt_label);
    virtual IR * simplifyLogicalOr(IN IR * ir, SimpCtx * cont);
    virtual IR * simplifyLogicalAndAtTruebr(IN IR * ir,
                                            LabelInfo const* tgt_label);
    virtual IR * simplifyLogicalAndAtFalsebr(IN IR * ir,
                                             LabelInfo const* tgt_label);
    virtual IR * simplifyLogicalAnd(IN IR * ir, SimpCtx * cont);
    virtual IR * simplifyLogicalDet(IR * ir, SimpCtx * cont);

    //Simplify ir to PR mode.
    virtual IR * simplifyToPR(IR * ir, SimpCtx * ctx);

    virtual bool perform(OptCtx & oc) { DUMMYUSE(oc); return false; }
};

} //namespace xoc
#endif
