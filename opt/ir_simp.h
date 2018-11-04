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

#define SIMP_if(s)                  (s)->prop_top_down.simp_if
#define SIMP_doloop(s)              (s)->prop_top_down.simp_do_loop
#define SIMP_dowhile(s)             (s)->prop_top_down.simp_do_while
#define SIMP_whiledo(s)             (s)->prop_top_down.simp_while_do
#define SIMP_switch(s)              (s)->prop_top_down.simp_switch
#define SIMP_select(s)              (s)->prop_top_down.simp_select
#define SIMP_array(s)               (s)->prop_top_down.simp_array
#define SIMP_break(s)               (s)->prop_top_down.simp_break
#define SIMP_continue(s)            (s)->prop_top_down.simp_continue
#define SIMP_lor_land(s)            (s)->prop_top_down.simp_logcial_or_and
#define SIMP_lnot(s)                (s)->prop_top_down.simp_logcial_not
#define SIMP_ild_ist(s)             (s)->prop_top_down.simp_ild_ist
#define SIMP_to_pr_mode(s)          (s)->prop_top_down.simp_to_pr_mode
#define SIMP_array_to_pr_mode(s)    (s)->prop_top_down.simp_array_to_pr_mode
#define SIMP_to_lowest_height(s)    (s)->prop_top_down.simp_to_lowest_height
#define SIMP_ret_array_val(s)       (s)->prop_top_down.simp_to_get_array_value
#define SIMP_is_record_cfs(s)       (s)->prop_top_down.is_record_cfs
#define SIMP_stmtlist(s)            (s)->ir_stmt_list
#define SIMP_break_label(s)         (s)->break_label
#define SIMP_continue_label(s)      (s)->continue_label
#define SIMP_changed(s)             (s)->prop_bottom_up.something_has_changed
#define SIMP_need_recon_bblist(s)   (s)->prop_bottom_up.need_to_reconstruct_bb_list
#define SIMP_cfs_mgr(s)             (s)->cfs_mgr
class SimpCtx {
public:
    struct {
        //Propagate these flags top down to simplify IR.
        UINT simp_if:1; //simplify IF.
        UINT simp_do_loop:1; //simplify DO_LOOP.
        UINT simp_do_while:1; //simplify DO_WHILE.
        UINT simp_while_do:1; //simplify WHILE_DO.
        UINT simp_switch:1; //simplify SWITCH.
        UINT simp_select:1; //simplify SELECT.
        UINT simp_array:1; //simplify ARRAY.
        UINT simp_break:1; //simplify BREAK.
        UINT simp_continue:1; //simplify CONTINUE.
        UINT simp_logcial_or_and:1; //simplify LOR, LAND.
        UINT simp_logcial_not:1; //simplify LNOT.
        UINT simp_ild_ist:1; //simplify ILD and IST.

        //Propagate info top down.
        //Simplify IR tree to the tree with lowest height,
        //that means the tree height is not more than 2,
        //namely, non-leaf IR is no more than 1.
        //e.g: id = v2 + v3 is the lowest tree.
        //the IR format is:
        //  st(id, add(ld(v2), ld(v3)))
        //Here, add is non-leaf IR, its children can
        //not be non-leaf node anymore.
        UINT simp_to_lowest_height:1;

        //Propagate info top down.
        //Operand only can be PR.
        //e.g: st(id1, ild(ld(v2))), converted to:
        //        pr1=ld(v1)
        //        pr2=ld(v2)
        //        pr3=ild(P2)
        //        st(pr1, pr3)
        //And this IR tree is unpermittable: ADD(LD(ID1), P1)
        UINT simp_to_pr_mode:1;

        //Propagate info top down.
        //Store array value into individual PR, but keep array operation
        //unchanged.
        //e.g:
        //    add(array(a), array(b))
        //converted to:
        //    pr1 = array(a)
        //    pr2 = array(b)
        //    add(pr1, pr2)
        UINT simp_array_to_pr_mode:1;

        //Propagate info top down.
        //If it is true function return array's value, or else return
        //the array address.
        //The flag often used in RHS simplification.
        //
        //e.g: Given ... = a[i][j], we need to get the value of a[i][j].
        //If the flag is false, function return the address expression:
        //    &a + i*elem_size + j,
        //Or else return ILD(&a + i*elem_size + j).
        UINT simp_to_get_array_value:1;

        //Propagate info top down.
        //Record high level Control-Flow-Struct info.
        UINT is_record_cfs:1;
    } prop_top_down;

    struct {
        //Propagate info bottom up.
        //Record whether exp or stmt has changed.
        BYTE something_has_changed:1;

        //Propagate info bottom up.
        //To inform Region to reconstruct bb list.
        //If this flag is true, DU info and DU chain also need to be rebuilt.
        BYTE need_to_reconstruct_bb_list:1;
    } prop_bottom_up;

    //Record IR stmts which generated bottom up.
    //When simplifing expression, the field records the
    //generated IR STMT list. Always used along with
    //'simp_to_lowest_heigh' and 'simp_to_pr_mode'.
    IR * ir_stmt_list;

    CfsMgr * cfs_mgr;

    //--
    //Propagate info top down.
    //Record label info for context.
    LabelInfo const* break_label; //record the current LOOP/IF/SWITCH end label.
    LabelInfo const* continue_label; //record the current LOOP start label.
    //--

public:
    SimpCtx() { init(); }
    SimpCtx(SimpCtx const& s)
    {
        *this = s;
        SIMP_stmtlist(this) = NULL;
    }

    void copy(SimpCtx const& s)
    {
        *this = s;
        SIMP_stmtlist(this) = NULL;
    }

    void init() { ::memset(this, 0, sizeof(SimpCtx)); }

    //Append irs to current simplification context and
    //return back to up level.
    void appendStmt(SimpCtx & c)
    { add_next(&SIMP_stmtlist(this), SIMP_stmtlist(&c)); }

    //Append irs to current simplification context and
    //return back to up level.
    void appendStmt(IR * irs)
    { add_next(&SIMP_stmtlist(this), irs); }

    //Unify the actions which propagated top down
    //during processing IR tree.
    void copyTopdownFlag(SimpCtx & c)
    {
        prop_top_down = c.prop_top_down;
        cfs_mgr = c.cfs_mgr;
    }

    //Copy the actions which propagated bottom up
    //during processing IR tree.
    void copyBottomupFlag(SimpCtx & c) { prop_bottom_up = c.prop_bottom_up; }

    //Unify the actions which propagated bottom up
    //during processing IR tree.
    void unionBottomupFlag(SimpCtx & c)
    {
        SIMP_changed(this) |= SIMP_changed(&c);
        SIMP_need_recon_bblist(this) |= SIMP_need_recon_bblist(&c);
    }

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

    //Simplify IR_ARRAY to linear address computational stmt/expression.
    void setSimpArray() { SIMP_array(this) = true; }

    //Simplify IR tree and reduce the tree height of IST/ILD to be lowest.
    //
    void setSimpIldIst() { SIMP_ild_ist(this) = true; }

    //Simplify IR_SELECT to IR_TRUBR/IR_FALSEBR operation.
    void setSimpSelect() { SIMP_select(this) = true; }

    //Simplify IR_LAND and IR_LOR operation.
    void setSimpLandLor() { SIMP_lor_land(this) = true; }

    //Simplify IR_LNOT operation.
    void setSimpLnot() { SIMP_lnot(this) = true; }

    //Simplify IR tree to be the tree with lowest height.
    //e.g: The height of 'a + b' is 2, the lowest height,
    //whereas 'a + b + c' is not.
    //Note that if ARRAY/STARRAY/ILD/IST/SELECT are not demanded to be
    //lowered, regarding it as a whole node.
    //e.g: Treat a[1][2] + b to be the lowest height.
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
        setSimpIldIst();
        setSimpSelect();
        setSimpLandLor();
        setSimpLnot();
        setSimpToLowestHeight();
    }

	void dump(TypeMgr const* tm) {
		ASSERT0(tm);
		note("\n==---- DUMP SimpCtx IR List ----==");
		dumpIRList(ir_stmt_list, tm);
	}
};

} //namespace xoc
#endif
