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

    inline xcom::C<IR*> * append_head(IR * ir)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(m_bb != NULL);
        ir->setBB(m_bb);
        return EList<IR*, IR2Holder>::append_head(ir);
    }

    inline xcom::C<IR*> * append_tail(IR * ir)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(m_bb != NULL);
        ir->setBB(m_bb);
        return EList<IR*, IR2Holder>::append_tail(ir);
    }

    //Insert ir prior to cond_br, uncond_br, call, return.
    xcom::C<IR*> * append_tail_ex(IR * ir);

    //Count up memory size of BBIRList
    size_t count_mem() const
    {
        return (size_t)sizeof(m_bb) +
               ((EList<IR*, IR2Holder>*)this)->count_mem();
    }

    //Insert 'ir' before 'marker'.
    inline xcom::C<IR*> * insert_before(IN IR * ir, IN IR * marker)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(marker != NULL);
        ASSERT0(m_bb != NULL);
        ir->setBB(m_bb);
        return EList<IR*, IR2Holder>::insert_before(ir, marker);
    }

    //Insert 'ir' before 'marker'. marker will be modified.
    inline xcom::C<IR*> * insert_before(IN IR * ir, IN xcom::C<IR*> * marker)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(marker != NULL);
        ASSERT0(m_bb != NULL);
        ir->setBB(m_bb);
        return EList<IR*, IR2Holder>::insert_before(ir, marker);
    }

    //Insert 'ir' after 'marker'.
    inline xcom::C<IR*> * insert_after(IR * ir, IR * marker)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(marker != NULL);
        ASSERT0(m_bb != NULL);
        ir->setBB(m_bb);
        return EList<IR*, IR2Holder>::insert_after(ir, marker);
    }

    //Insert 'ir' after 'marker'.
    inline xcom::C<IR*> * insert_after(IR * ir, IN xcom::C<IR*> * marker)
    {
        if (ir == NULL) { return NULL; }
        ASSERT0(marker != NULL);
        ASSERT0(m_bb != NULL);
        ir->setBB(m_bb);
        return EList<IR*, IR2Holder>::insert_after(ir, marker);
    }

    //Remove ir that hold by 'holder'.
    inline IR * remove(IN xcom::C<IR*> * holder)
    {
        if (holder == NULL) return NULL;
        holder->val()->setBB(NULL);
        return EList<IR*, IR2Holder>::remove(holder);
    }

    //Remove ir.
    inline IR * remove(IN IR * ir)
    {
        if (ir == NULL) return NULL;
        ir->setBB(NULL);
        return EList<IR*, IR2Holder>::remove(ir);
    }

    void setBB(IRBB * bb) { m_bb = bb; }
};
//END BBIRList


//
//START IRBB
//
#define MAX_BB_KIDS_NUM     2

#define BB_rpo(b)               ((b)->m_rpo)
#define BB_id(b)                ((b)->m_id)
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
#define BB_is_terminate(b)      ((b)->u1.s1.is_terminate)
class IRBB {
public:
    UINT m_id; //BB's id
    INT m_rpo; //reverse post order
    BBIRList ir_list; //IR list
    List<LabelInfo const*> lab_list; //Record labels attached on BB
    union {
        struct {
            BYTE is_entry:1; //bb is entry of the region.
            BYTE is_exit:1; //bb is exit of the region.
            BYTE is_fallthrough:1; //bb has a fall through successor.
            BYTE is_target:1; //bb is branch target.
            BYTE is_catch_start:1; //bb is entry of catch block.
            BYTE is_terminate:1; //bb terminate the control flow.
            BYTE is_try_start:1; //bb is entry of try block.
            BYTE is_try_end:1; //bb is exit of try block.
        } s1;
        BYTE u1b1;
    } u1;

public:
    IRBB()
    {
        ir_list.setBB(this);
        m_id = 0;
        u1.u1b1 = 0;
        m_rpo = -1;
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

    inline void addLabel(LabelInfo const* li, bool at_head = false)
    {
        ASSERT0(li);
        if (getLabelList().find(li)) { return; }
        if (LABEL_INFO_is_catch_start(li)) {
            BB_is_catch_start(this) = true;
        }
        if (LABEL_INFO_is_try_start(li)) {
            BB_is_try_start(this) = true;
        }
        if (LABEL_INFO_is_try_end(li)) {
            BB_is_try_end(this) = true;
        }
        if (LABEL_INFO_is_terminate(li)) {
            BB_is_terminate(this) = true;
        }
        if (at_head) {
            getLabelList().append_head(li);
        } else {
            getLabelList().append_tail(li);
        }
    }

    size_t count_mem() const;

    //Clean attached label.
    void cleanLabelInfoList() { getLabelList().clean(); }

    void dump(Region * rg, bool dump_inner_region);
    void dupSuccessorPhiOpnd(CFG<IRBB, IR> * cfg, Region * rg, UINT opnd_pos);

    List<LabelInfo const*> & getLabelList() { return lab_list; }
    List<LabelInfo const*> const& getLabelListConst() const { return lab_list; }
    UINT getNumOfIR() const { return BB_irlist(this).get_elem_count(); }
    UINT getNumOfPred(CFG<IRBB, IR> * cfg) const
    {
        ASSERT0(cfg);
        xcom::Vertex const* vex = cfg->get_vertex(id());
        ASSERT0(vex);
        UINT n = 0;
        for (xcom::EdgeC const* in = VERTEX_in_list(vex);
             in != NULL; in = EC_next(in), n++);
        return n;
    }

    UINT getNumOfSucc(CFG<IRBB, IR> * cfg) const
    {
        ASSERT0(cfg);
        xcom::Vertex const* vex = cfg->get_vertex(BB_id(this));
        ASSERT0(vex);
        UINT n = 0;
        for (xcom::EdgeC const* out = VERTEX_out_list(vex);
             out != NULL; out = EC_next(out), n++);
        return n;
    }

    //Is bb containing such label carried by 'lir'.
    inline bool hasLabel(LabelInfo const* lab)
    {
        for (LabelInfo const* li = getLabelList().get_head();
             li != NULL; li = getLabelList().get_next()) {
            if (isSameLabel(li, lab)) {
                return true;
            }
        }
        return false;
    }

    //For some aggressive optimized purposes, call node is not looked as
    //boundary of basic block.
    //So we must bottom-up go through whole bb to find call.
    inline bool hasCall() const
    {
        BBIRList * irlst = const_cast<BBIRList*>(&BB_irlist(this));
        for (IR * ir = irlst->get_tail();
             ir != NULL; ir = irlst->get_prev()) {
            if (ir->isCallStmt()) {
                return true;
            }
        }
        return false;
    }

    inline bool hasReturn() const
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

    UINT id() const { return BB_id(this); }

    //Return true if BB is an entry BB of TRY block.
    inline bool isTryStart() const
    {
        bool r = BB_is_try_start(this);
     #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->getLabelList().get_head();
             li != NULL; li = pthis->getLabelList().get_next()) {
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
    inline bool isTryEnd() const
    {
        bool r = BB_is_try_end(this);
 #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->getLabelList().get_head();
             li != NULL; li = pthis->getLabelList().get_next()) {
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
    inline bool isExceptionHandler() const
    {
        bool r = BB_is_catch_start(this);
        #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->getLabelList().get_head();
             li != NULL; li = pthis->getLabelList().get_next()) {
            if (LABEL_INFO_is_catch_start(li)) {
                find = true;
                break;
            }
        }
        ASSERT0(r == find);
        #endif
        return r;
    }

    //Return true if BB is terminate.
    inline bool is_terminate() const
    {
        bool r = BB_is_terminate(this);
        #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->getLabelList().get_head();
             li != NULL; li = pthis->getLabelList().get_next()) {
            if (LABEL_INFO_is_terminate(li)) {
                find = true;
                break;
            }
        }
        ASSERT0(r == find);
        #endif
        return r;
    }

    //Could ir be looked as a boundary stmt of basic block?
    inline bool is_boundary(IR * ir)
    { return (isUpperBoundary(ir) || isDownBoundary(ir)); }

    //Could ir be looked as a first stmt in basic block?
    inline bool isUpperBoundary(IR const* ir) const
    {
        ASSERTN(ir->isStmtInBB() || ir->is_lab(), ("illegal stmt in bb"));
        return ir->is_lab();
    }
    bool isDownBoundary(IR * ir);

    inline bool isAttachDedicatedLabel()
    {
        for (LabelInfo const* li = getLabelList().get_head();
             li != NULL; li = getLabelList().get_next()) {
            if (LABEL_INFO_is_catch_start(li) ||
                LABEL_INFO_is_try_start(li) ||
                LABEL_INFO_is_try_end(li) ||
                LABEL_INFO_is_pragma(li)) {
                return true;
            }
        }
        return false;
    }

    inline bool isContainLabel(LabelInfo const* lab)
    {
        for (LabelInfo const* li = getLabelList().get_head();
             li != NULL; li = getLabelList().get_next()) {
            if (li == lab) {
                return true;
            }
        }
        return false;
    }

    //Return true if ir1 dominates ir2 in current bb.
    //Function will modify the IR container of bb.
    //'is_strict': true if ir1 should not equal to ir2.
    inline bool is_dom(IR const* ir1, IR const* ir2, bool is_strict) const
    {
        ASSERT0(ir1 && ir2 && ir1->is_stmt() && ir2->is_stmt() &&
            ir1->getBB() == this && ir2->getBB() == this);
        if (is_strict && ir1 == ir2) {
            return false;
        }

        xcom::C<IR*> * ctir;
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
        xcom::C<IR*> * ct;
        IR * x = BB_irlist(const_cast<IRBB*>(this)).get_tail(&ct);
        if (x != NULL && x->isMayThrow()) {
            return true;
        }
        return false;
    }

    //Add all Labels attached on src BB to current BB.
    inline void mergeLabeInfoList(IRBB * src)
    {
        for (LabelInfo const* li = src->getLabelList().get_tail();
             li != NULL; li = src->getLabelList().get_prev()) {
            addLabel(li, true);
        }
    }

    //Return true if one of bb's successor has a phi.
    bool successorHasPhi(CFG<IRBB, IR> * cfg);

    INT rpo() const { return BB_rpo(this); }

    //Before removing bb or change bb successor,
    //you need remove the related PHI operand if BB successor has PHI stmt.
    void removeSuccessorPhiOpnd(CFG<IRBB, IR> * cfg);

    //Before removing bb or change bb successor,
    //you need remove the related PHI operand if BB successor has PHI stmt.
    void removeSuccessorDesignatePhiOpnd(CFG<IRBB, IR> * cfg, IRBB * succ);

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

    inline IRBB * allocBB()
    {
        IRBB * bb = new IRBB();
        BB_id(bb) = m_bb_count++;
        m_bbs_list.append_tail(bb);
        return bb;
    }

    size_t count_mem()
    {
        size_t count = 0;
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
void dumpBBList(BBList * bbl,
                Region * rg,
                CHAR const* name = NULL,
                bool dump_inner_region = true);

} //namespace xoc
#endif
