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
#ifndef _IR_BB_H_
#define _IR_BB_H_

namespace xoc {

class IRBB;

//
//START BBIRList
//
//NOTE: Overload funtion when inserting or remving new IR.
class BBIRList : public EList<IR*, IR2Holder> {
    IRBB * m_bb;
public:
    BBIRList() { m_bb = NULL; }
    COPY_CONSTRUCTOR(BBIRList);

    inline C<IR*> * append_head(IR * ir)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(m_bb != NULL);
        ir->set_bb(m_bb);
        return EList<IR*, IR2Holder>::append_head(ir);
    }

    inline C<IR*> * append_tail(IR * ir)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(m_bb != NULL);
        ir->set_bb(m_bb);
        return EList<IR*, IR2Holder>::append_tail(ir);
    }

    //Insert ir prior to cond_br, uncond_br, call, return.
    C<IR*> * append_tail_ex(IR * ir)
    {
        if (ir == NULL) { return NULL; }
        C<IR*> * ct;
        for (IR * tir = List<IR*>::get_tail(&ct);
             tir != NULL; tir = List<IR*>::get_prev(&ct)) {
            if (!tir->is_uncond_br() &&
                !tir->is_cond_br() &&
                !tir->is_multicond_br() &&
                !tir->is_return() &&
                !tir->is_calls_stmt()) {
                break;
            }
        }

        ASSERT0(m_bb);
        ir->set_bb(m_bb);
        if (ct == NULL) {
            return EList<IR*, IR2Holder>::append_head(ir);
        } else {
            return EList<IR*, IR2Holder>::insert_after(ir, ct);
        }
    }

    //Count up memory size of BBIRList
    UINT count_mem() const
    {
        return sizeof(m_bb) + ((EList<IR*, IR2Holder>*)this)->count_mem();
    }

    //Insert 'ir' before 'marker'.
    inline C<IR*> * insert_before(IN IR * ir, IN IR * marker)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(marker != NULL);
        ASSERT0(m_bb != NULL);
        ir->set_bb(m_bb);
        return EList<IR*, IR2Holder>::insert_before(ir, marker);
    }

    //Insert 'ir' before 'marker'. marker will be modified.
    inline C<IR*> * insert_before(IN IR * ir, IN C<IR*> * marker)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(marker != NULL);
        ASSERT0(m_bb != NULL);
        ir->set_bb(m_bb);
        return EList<IR*, IR2Holder>::insert_before(ir, marker);
    }

    //Insert 'ir' after 'marker'.
    inline C<IR*> * insert_after(IR * ir, IR * marker)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(marker != NULL);
        ASSERT0(m_bb != NULL);
        ir->set_bb(m_bb);
        return EList<IR*, IR2Holder>::insert_after(ir, marker);
    }

    //Insert 'ir' after 'marker'.
    inline C<IR*> * insert_after(IR * ir, IN C<IR*> * marker)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(marker != NULL);
        ASSERT0(m_bb != NULL);
        ir->set_bb(m_bb);
        return EList<IR*, IR2Holder>::insert_after(ir, marker);
    }

    //Remove ir that hold by 'holder'.
    inline IR * remove(IN C<IR*> * holder)
    {
        if (holder == NULL) return NULL;
        C_val(holder)->set_bb(NULL);
        return EList<IR*, IR2Holder>::remove(holder);
    }

    //Remove ir.
    inline IR * remove(IN IR * ir)
    {
        if (ir == NULL) return NULL;
        ir->set_bb(NULL);
        return EList<IR*, IR2Holder>::remove(ir);
    }

    inline void set_bb(IRBB * bb) { m_bb = bb; }
};
//END BBIRList


//
//START IRBB
//
#define MAX_BB_KIDS_NUM     2

#define BB_rpo(b)               ((b)->rpo)
#define BB_id(b)                ((b)->id)
#define BB_irlist(b)            ((b)->ir_list)
#define BB_first_ir(b)          ((b)->ir_list.get_head())
#define BB_next_ir(b)           ((b)->ir_list.get_next())
#define BB_prev_ir(b)           ((b)->ir_list.get_prev())
#define BB_last_ir(b)           ((b)->ir_list.get_tail())
#define BB_is_entry(b)          ((b)->u1.s1.is_entry)
#define BB_is_exit(b)           ((b)->u1.s1.is_exit)
#define BB_is_fallthrough(b)    ((b)->u1.s1.is_fallthrough)
#define BB_is_target(b)         ((b)->u1.s1.is_target)
#define BB_is_catch_start(b)    ((b)->u1.s1.is_catch_start)
#define BB_is_try_start(b)      ((b)->u1.s1.is_try_start)
#define BB_is_try_end(b)        ((b)->u1.s1.is_try_end)
#define BB_is_unreach(b)        ((b)->u1.s1.is_unreachable)
class IRBB {
public:
    UINT id; //BB's id
    INT rpo; //reverse post order
    BBIRList ir_list; //IR list
    List<LabelInfo const*> lab_list; //Record labels attached on BB
    union {
        struct {
            BYTE is_entry:1; //bb is entry of the region.
            BYTE is_exit:1; //bb is exit of the region.
            BYTE is_fallthrough:1; //bb has a fall through successor.
            BYTE is_target:1; //bb is branch target.
            BYTE is_catch_start:1; //bb is entry of catch block.
            BYTE is_unreachable:1; //bb is unreachable.
            BYTE is_try_start:1; //bb is entry of try block.
            BYTE is_try_end:1; //bb is exit of try block.
        } s1;
        BYTE u1b1;
    } u1;

    IRBB()
    {
        ir_list.set_bb(this);
        id = 0;
        u1.u1b1 = 0;
        rpo = -1;
    }
    COPY_CONSTRUCTOR(IRBB);
    ~IRBB()
    {
        //If BB destructed in ~IRBBMgr(), then it is
        //dispensable to free them. Or the ir_list must be clean before
        //the deletion of BB.
        //for (IR * ir = ir_list.get_head(); ir != NULL; ir = ir_list.get_next()) {
        //    m_ru->freeIRTree(ir);
        //}
    }

    inline void addLabel(LabelInfo const* li)
    {
        ASSERT0(li != NULL);
        if (!get_lab_list().find(li)) {
            if (LABEL_INFO_is_catch_start(li)) {
                BB_is_catch_start(this) = true;
            }

            if (LABEL_INFO_is_try_start(li)) {
                BB_is_try_start(this) = true;
            }

            if (LABEL_INFO_is_try_end(li)) {
                BB_is_try_end(this) = true;
            }

            if (LABEL_INFO_is_unreachable(li)) {
                BB_is_unreach(this) = true;

            }
            get_lab_list().append_tail(li);
        }
    }

    UINT count_mem() const;

    void dump(Region * ru);
    void dupSuccessorPhiOpnd(CFG<IRBB, IR> * cfg, Region * ru, UINT opnd_pos);

    List<LabelInfo const*> & get_lab_list() { return lab_list; }
    List<LabelInfo const*> const& get_lab_list_c() const { return lab_list; }
    UINT getNumOfIR() const { return BB_irlist(this).get_elem_count(); }

    //For some aggressive optimized purposes, call node is not looked as
    //boundary of basic block.
    //So we must bottom-up go through whole bb to find call.
    inline bool is_bb_has_call() const
    {
        BBIRList * irlst = const_cast<BBIRList*>(&BB_irlist(this));
        for (IR * ir = irlst->get_tail();
             ir != NULL; ir = irlst->get_prev()) {
            if (ir->is_calls_stmt()) {
                return true;
            }
        }
        return false;
    }

    inline bool is_bb_has_return() const
    {
        BBIRList * irlst = const_cast<BBIRList*>(&BB_irlist(this));
        for (IR * ir = irlst->get_tail();
             ir != NULL; ir = irlst->get_prev()) {
            if (ir->is_return()) {
                return true;
            }
        }
        return false;
    }

    //Return true if BB is an entry BB of TRY block.
    inline bool is_try_start() const
    {
        bool r = BB_is_try_start(this);
     #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->get_lab_list().get_head();
             li != NULL; li = pthis->get_lab_list().get_next()) {
            if (LABEL_INFO_is_try_start(li)) {
                find = true;
                break;
            }
        }
        ASSERT0(r == find);
     #endif
        return r;
    }

    //Return true if BB is an exit BB of TRY block.
    inline bool is_try_end() const
    {
        bool r = BB_is_try_end(this);
 #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->get_lab_list().get_head();
             li != NULL; li = pthis->get_lab_list().get_next()) {
            if (LABEL_INFO_is_try_end(li)) {
                find = true;
                break;
            }
        }
        ASSERT0(r == find);
 #endif
        return r;
    }

    //Return true if BB is entry of CATCH block.
    inline bool is_exp_handling() const
    {
        bool r = BB_is_catch_start(this);
        #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->get_lab_list().get_head();
             li != NULL; li = pthis->get_lab_list().get_next()) {
            if (LABEL_INFO_is_catch_start(li)) {
                find = true;
                break;
            }
        }
        ASSERT0(r == find);
        #endif
        return r;
    }

    //Return true if BB is unreachable.
    inline bool is_unreachable() const
    {
        bool r = BB_is_unreach(this);
        #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->get_lab_list().get_head();
             li != NULL; li = pthis->get_lab_list().get_next()) {
            if (LABEL_INFO_is_unreachable(li)) {
                find = true;
                break;
            }
        }
        ASSERT0(r == find);
        #endif
        return r;
    }

    //Could ir be looked as a boundary stmt of basic block?
    inline bool is_bb_boundary(IR * ir)
    { return (is_bb_up_boundary(ir) || is_bb_down_boundary(ir)); }

    //Could ir be looked as a first stmt in basic block?
    inline bool is_bb_up_boundary(IR const* ir) const
    {
        ASSERT(ir->isStmtInBB() || ir->is_lab(), ("illegal stmt in bb"));
        return ir->is_lab();
    }
    bool is_bb_down_boundary(IR * ir);

    //Is bb containing such label carried by 'lir'.
    inline bool is_bb_has_label(LabelInfo const* lab)
    {
        for (LabelInfo const* li = get_lab_list().get_head();
             li != NULL; li = get_lab_list().get_next()) {
            if (isSameLabel(li, lab)) {
                return true;
            }
        }
        return false;
    }

    inline bool is_attach_dedicated_lab()
    {
        for (LabelInfo const* li = get_lab_list().get_head();
             li != NULL; li = get_lab_list().get_next()) {
            if (LABEL_INFO_is_catch_start(li) ||
                LABEL_INFO_is_try_start(li) ||
                LABEL_INFO_is_try_end(li) ||
                LABEL_INFO_is_pragma(li)) {
                return true;
            }
        }
        return false;
    }

    //Return true if ir1 dominate ir2 in current bb.
    //Function will modify the IR container of bb.
    //'is_strict': true if ir1 should not equal to ir2.
    inline bool is_dom(IR const* ir1, IR const* ir2, bool is_strict) const
    {
        ASSERT0(ir1->is_stmt() && ir2->is_stmt() &&
                 ir1->get_bb() == this && ir2->get_bb() == this);
        if (is_strict && ir1 == ir2) {
            return false;
        }

        C<IR*> * ctir;
        for (BB_irlist(this).get_head(&ctir);
             ctir != BB_irlist(this).end();
             ctir = BB_irlist(this).get_next(ctir)) {
            IR * ir = ctir->val();
            if (ir == ir1) {
                return true;
            }
            if (ir == ir2) {
                return false;
            }
        }
        return false;
    }

    bool mayThrowException() const
    {
        C<IR*> * ct;
        IR * x = BB_irlist(const_cast<IRBB*>(this)).get_tail(&ct);
        if (x != NULL && IR_may_throw(x)) {
            return true;
        }
        return false;
    }

    //Union all labels of 'src' to current bb.
    inline void unionLabels(IRBB * src)
    {
        for (LabelInfo const* li = src->get_lab_list().get_head();
             li != NULL; li = src->get_lab_list().get_next()) {
            if (!lab_list.find(li)) {
                lab_list.append_head(li);
                if (LABEL_INFO_is_catch_start(li)) {
                    BB_is_catch_start(this) = true;
                }
                if (LABEL_INFO_is_unreachable(li)) {
                    BB_is_unreach(this) = true;
                }
            }
        }
    }

    //Return true if one of bb's successor has a phi.
    bool successorHasPhi(CFG<IRBB, IR> * cfg);

    //Before removing bb, revising phi opnd if there are phis
    //in one of bb's successors.
    void removeSuccessorPhiOpnd(CFG<IRBB, IR> * cfg);

    void verify();
};
//END IRBB



//
//START IRBBMgr
//
class IRBBMgr {
protected:
    BBList m_bbs_list;
    UINT m_bb_count; //counter of IRBB.

public:
    IRBBMgr() { m_bb_count = 1; }
    COPY_CONSTRUCTOR(IRBBMgr);
    ~IRBBMgr()
    {
        for (IRBB * bb = m_bbs_list.get_head();
             bb != NULL; bb = m_bbs_list.get_next()) {
            delete bb;
        }
    }

    inline IRBB * newBB()
    {
        IRBB * bb = new IRBB();
        BB_id(bb) = m_bb_count++;
        m_bbs_list.append_tail(bb);
        return bb;
    }

    UINT count_mem()
    {
        UINT count = 0;
        for (IRBB * bb = m_bbs_list.get_head();
             bb != NULL; bb = m_bbs_list.get_next()) {
            count += bb->count_mem();
        }
        return count;
    }
};
//END IRBBMgr

//Exported Functions
void dumpBBLabel(List<LabelInfo const*> & lablist, FILE * h);
void dumpBBList(BBList * bbl, Region * ru, CHAR const* name = NULL);

} //namespace xoc
#endif
