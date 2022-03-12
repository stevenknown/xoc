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
template <class IRBB, class IR> class CFG;
typedef xcom::C<IR*> * IRListIter;
typedef IRListIter BBIRListIter;
typedef List<LabelInfo const*> LabelInfoList;
typedef C<LabelInfo const*> * LabelInfoListIter;
typedef TMap<IR const*, UINT> IROrder;
typedef TMap<LabelInfo const*, IRBB*> Lab2BB;

#define BBID_UNDEF VERTEX_UNDEF

//
//START BBIRList
//
//NOTE: Overload funtion when inserting or remving new IR.
class BBIRList : public EList<IR*, IR2Holder> {
    COPY_CONSTRUCTOR(BBIRList);
    IRBB * m_bb;

public:
    BBIRList() { m_bb = nullptr; }

    inline IRListIter append_head(IR * ir)
    {
        if (ir == nullptr) { return nullptr; }
        ASSERT0(m_bb != nullptr);
        ir->setBB(m_bb);
        return xcom::EList<IR*, IR2Holder>::append_head(ir);
    }

    inline IRListIter append_tail(IR * ir)
    {
        if (ir == nullptr) { return nullptr; }
        ASSERT0(m_bb != nullptr);
        ir->setBB(m_bb);
        return xcom::EList<IR*, IR2Holder>::append_tail(ir);
    }

    //Insert ir prior to cond_br, uncond_br, call, return.
    IRListIter append_tail_ex(IR * ir);
    //Insert ir after phi operations.
    IRListIter append_head_ex(IR * ir);

    //Count up memory size of BBIRList
    size_t count_mem() const
    {
        return (size_t)sizeof(m_bb) +
               ((xcom::EList<IR*, IR2Holder>*)this)->count_mem();
    }

    IR * getPrevIR(IR const* ir) const
    {
        ASSERT0(ir->is_stmt());
        IRListIter irit;
        find(const_cast<IR*>(ir), &irit);
        ASSERTN(irit, ("ir is not belong to current BB"));
        irit = get_prev(irit);
        return irit == nullptr ? nullptr : irit->val();
    }

    //Insert 'ir' before 'marker'.
    inline IRListIter insert_before(IN IR * ir, IR const* marker)
    {
        if (ir == nullptr) { return nullptr; }
        ASSERT0(marker != nullptr);
        ASSERT0(m_bb != nullptr);
        ir->setBB(m_bb);
        return xcom::EList<IR*, IR2Holder>::insert_before(
            ir, const_cast<IR*>(marker));
    }

    //Insert 'ir' before 'marker'. marker will be modified.
    inline IRListIter insert_before(IN IR * ir, IN IRListIter marker)
    {
        if (ir == nullptr) { return nullptr; }
        ASSERT0(marker != nullptr);
        ASSERT0(m_bb != nullptr);
        ir->setBB(m_bb);
        return xcom::EList<IR*, IR2Holder>::insert_before(ir, marker);
    }

    //Insert 'ir' after 'marker'.
    inline IRListIter insert_after(IR * ir, IR const* marker)
    {
        if (ir == nullptr) { return nullptr; }
        ASSERT0(marker != nullptr);
        ASSERT0(m_bb != nullptr);
        ir->setBB(m_bb);
        return xcom::EList<IR*, IR2Holder>::insert_after(
            ir, const_cast<IR*>(marker));
    }

    //Insert 'ir' after 'marker'.
    inline IRListIter insert_after(IR * ir, IN IRListIter marker)
    {
        if (ir == nullptr) { return nullptr; }
        ASSERT0(marker != nullptr);
        ASSERT0(m_bb != nullptr);
        ir->setBB(m_bb);
        return xcom::EList<IR*, IR2Holder>::insert_after(ir, marker);
    }

    //Remove ir that hold by 'holder'.
    inline IR * remove(IN IRListIter holder)
    {
        if (holder == nullptr) { return nullptr; }
        ASSERT0(holder->val());
        holder->val()->setBB(nullptr);
        return xcom::EList<IR*, IR2Holder>::remove(holder);
    }

    //Remove ir out of list.
    inline IR * remove(IN IR * ir)
    {
        if (ir == nullptr) { return nullptr; }
        ir->setBB(nullptr);
        return xcom::EList<IR*, IR2Holder>::remove(ir);
    }

    void setBB(IRBB * bb) { m_bb = bb; }
};
//END BBIRList


//
//START IRBB
//
#define MAX_BB_KIDS_NUM 2

#define BB_rpo(b) ((b)->m_rpo)
#define BB_id(b) ((b)->m_id)
#define BB_irlist(b) ((b)->ir_list)
#define BB_first_ir(b) ((b)->ir_list.get_head())
#define BB_next_ir(b) ((b)->ir_list.get_next())
#define BB_prev_ir(b) ((b)->ir_list.get_prev())
#define BB_last_ir(b) ((b)->ir_list.get_tail())
#define BB_is_entry(b) ((b)->u1.s1.is_entry)
#define BB_is_exit(b) ((b)->u1.s1.is_exit)
#define BB_is_target(b) ((b)->u1.s1.is_target)
#define BB_is_catch_start(b) ((b)->u1.s1.is_catch_start)
#define BB_is_try_start(b) ((b)->u1.s1.is_try_start)
#define BB_is_try_end(b) ((b)->u1.s1.is_try_end)
#define BB_is_terminate(b) ((b)->u1.s1.is_terminate)
class IRBB {
    COPY_CONSTRUCTOR(IRBB);
public:
    typedef BYTE BitUnion;
    union {
        struct {
            BitUnion is_entry:1; //bb is entry of the region.
            BitUnion is_exit:1; //bb is exit of the region.
            BitUnion is_target:1; //bb is branch target.
            BitUnion is_catch_start:1; //bb is entry of catch block.
            BitUnion is_terminate:1; //bb terminate the control flow.
            BitUnion is_try_start:1; //bb is entry of try block.
            BitUnion is_try_end:1; //bb is exit of try block.
        } s1;
        BitUnion u1b1;
    } u1;
    UINT m_id; //BB's id
    INT m_rpo; //reverse post order
    BBIRList ir_list; //IR list
    LabelInfoList lab_list; //Record labels attached on BB

public:
    IRBB()
    {
        ir_list.setBB(this);
        m_id = 0;
        u1.u1b1 = 0;
        m_rpo = RPO_UNDEF;
    }
    ~IRBB()
    {
        //BB will be destructed in ~IRBBMgr().
        //No need to free it. Or the ir_list must be clean before
        //the deletion of BB.
        //for (IR * ir = ir_list.get_head(); ir != nullptr; ir = ir_list.get_next()) {
        //    m_rg->freeIRTree(ir);
        //}
    }

    inline void addLabel(LabelInfo const* li, bool at_head = false)
    {
        ASSERT0(li);
        ASSERTN(!getLabelList().find(li),
                ("query CFG::lab2bb first to avoid duplicated insertion"));
        copyAttr(li);
        if (at_head) {
            getLabelList().append_head(li);
        } else {
            getLabelList().append_tail(li);
        }
    }

    size_t count_mem() const;
    void clean()
    {
        //Do not erase BB's id because it may be reallocated later.
        //IRBBMgr does not recycle BB's id.
        ir_list.clean();
        lab_list.clean();
        u1.u1b1 = 0;
        m_rpo = RPO_UNDEF;
    }

    //Clean attached label.
    void cleanLabelInfoList() { getLabelList().clean(); }
    void copyAttr(LabelInfo const* li)
    {
        ASSERT0(li);
        BB_is_catch_start(this) |= LABELINFO_is_catch_start(li);
        BB_is_try_start(this) |= LABELINFO_is_try_start(li);
        BB_is_try_end(this) |= LABELINFO_is_try_end(li);
        BB_is_terminate(this) |= LABELINFO_is_terminate(li);
    }

    void dumpLabelList(Region const* rg) const;
    void dumpAttr(Region const* rg) const;
    void dumpIRList(Region const* rg, bool dump_inner_region) const;
    void dump(Region const* rg, bool dump_inner_region) const;
    void dupSuccessorPhiOpnd(CFG<IRBB, IR> * cfg, Region * rg, UINT opnd_pos);

    LabelInfoList & getLabelList() { return lab_list; }
    LabelInfoList const& getLabelListConst() const
    { return lab_list; }
    UINT getNumOfIR() const { return BB_irlist(this).get_elem_count(); }
    UINT getNumOfPred(CFG<IRBB, IR> const* cfg) const;
    UINT getNumOfSucc(CFG<IRBB, IR> const* cfg) const;
    BBIRList & getIRList() { return BB_irlist(this); }
    IR * getFirstIR() { return BB_first_ir(this); }
    IR * getNextIR() { return BB_next_ir(this); }
    IR * getLastIR() { return BB_last_ir(this); }
    IR * getPrevIR(IR const* ir) const
    {
        ASSERT0(ir->is_stmt());
        return const_cast<IRBB*>(this)->getIRList().getPrevIR(ir);
    }

    bool hasMDPhi(CFG<IRBB, IR> const* cfg) const;

    //Is bb containing such label carried by 'lir'.
    inline bool hasLabel(LabelInfo const* lab) const
    {
        LabelInfoListIter it;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->getLabelList().get_head(&it);
             li != nullptr; li = pthis->getLabelList().get_next(&it)) {
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
             ir != nullptr; ir = irlst->get_prev()) {
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
             ir != nullptr; ir = irlst->get_prev()) {
            if (ir->is_return()) {
                return true;
            }
        }
        return false;
    }

    UINT id() const { return BB_id(this); }
    bool is_entry() const { return BB_is_entry(this); }
    bool is_exit() const { return BB_is_exit(this); }
    //Return true if BB has a fall through successor.
    //Note conditional branch always has fallthrough successor.
    bool is_fallthrough() const;
    bool is_target() const { return BB_is_target(this); }
    bool is_catch_start() const { return BB_is_catch_start(this); }
    bool is_try_start() const { return BB_is_try_start(this); }
    bool is_try_end() const { return BB_is_try_end(this); }

    //Return true if BB is an entry BB of TRY block.
    inline bool isTryStart() const
    {
        bool r = BB_is_try_start(this);
        #ifdef _DEBUG_
        bool find = false;
        IRBB * pthis = const_cast<IRBB*>(this);
        for (LabelInfo const* li = pthis->getLabelList().get_head();
             li != nullptr; li = pthis->getLabelList().get_next()) {
            if (LABELINFO_is_try_start(li)) {
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
             li != nullptr; li = pthis->getLabelList().get_next()) {
            if (LABELINFO_is_try_end(li)) {
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
             li != nullptr; li = pthis->getLabelList().get_next()) {
            if (LABELINFO_is_catch_start(li)) {
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
             li != nullptr; li = pthis->getLabelList().get_next()) {
            if (LABELINFO_is_terminate(li)) {
                find = true;
                break;
            }
        }
        ASSERT0(r == find);
        #endif
        return r;
    }

    //Could ir be looked as a boundary stmt of basic block?
    static bool isBoundary(IR * ir)
    { return isUpperBoundary(ir) || isLowerBoundary(ir); }

    //Could ir be looked as a first stmt in basic block?
    static bool isUpperBoundary(IR const* ir)
    {
        ASSERTN(ir->isStmtInBB() || ir->is_lab(), ("illegal stmt in bb"));
        return ir->is_lab();
    }
    static bool isLowerBoundary(IR const* ir);

    inline bool isAttachDedicatedLabel()
    {
        for (LabelInfo const* li = getLabelList().get_head();
             li != nullptr; li = getLabelList().get_next()) {
            if (li->is_catch_start() || li->is_try_start() ||
                li->is_try_end() || li->is_pragma()) {
                return true;
            }
        }
        return false;
    }

    inline bool isContainLabel(LabelInfo const* lab) const
    {
        for (LabelInfo const* li = const_cast<IRBB*>(this)->
                 getLabelList().get_head();
             li != nullptr;
             li = const_cast<IRBB*>(this)->getLabelList().get_next()) {
            if (li == lab) {
                return true;
            }
        }
        return false;
    }

    //Return true if current BB is the target of 'ir'.
    bool isTarget(IR const* ir) const
    { ASSERT0(ir->getLabel()); return isContainLabel(ir->getLabel()); }

    inline bool is_dom(IR const* ir1, IR const* ir2, IROrder const& order,
                       bool is_strict) const
    {
        ASSERT0(ir1 && ir2 && ir1->is_stmt() && ir2->is_stmt() &&
                ir1->getBB() == this && ir2->getBB() == this);
        if (is_strict && ir1 == ir2) {
            return false;
        }
        UINT ir1order = order.get(ir1);
        UINT ir2order = order.get(ir2);
        ASSERTN(ir1order != 0 || ir2order != 0,
                 ("Neither ir1 nor ir2 has an order set ")); 
        if (ir1order == 0) { return false; }
        if (ir2order == 0) { return true; }
        ASSERT0(ir1order != ir2order);
        return ir1order < ir2order;
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

        IRListIter ctir;
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
    //Return true if BB has no IR stmt.
    bool is_empty() const { return BB_irlist(this).get_elem_count() == 0; }

    bool mayThrowException() const
    {
        IRListIter ct;
        IR * x = BB_irlist(const_cast<IRBB*>(this)).get_tail(&ct);
        if (x != nullptr && x->isMayThrow(true)) {
            return true;
        }
        return false;
    }

    //Add all Labels attached on src BB to current BB.
    void mergeLabeInfoList(IRBB * src)
    {
        LabelInfoListIter it;
        for (src->getLabelList().get_head(&it);
             it != nullptr; it = src->getLabelList().get_next(it)) {
            copyAttr(it->val());
        }
        //Note src's labellist will be moved to current BB.
        getLabelList().move_head(src->getLabelList());
        ASSERT0(src->getLabelList().get_elem_count() == 0);
    }

    //Return true if one of bb's successor has a phi.
    bool successorHasPhi(CFG<IRBB, IR> * cfg);

    INT rpo() const { return BB_rpo(this); }

    bool verifyBranchLabel(Lab2BB const& lab2bb) const;
    bool verify(Region const* rg) const;
};
//END IRBB



//
//START IRBBMgr
//
class IRBBMgr {
    COPY_CONSTRUCTOR(IRBBMgr);
protected:
    BBList m_bbs_list;
    BBList m_free_list;
    UINT m_bb_count; //counter of IRBB.

public:
    IRBBMgr() { m_bb_count = BBID_UNDEF + 1; }
    ~IRBBMgr()
    {
        for (IRBB * bb = m_bbs_list.get_head();
             bb != nullptr; bb = m_bbs_list.get_next()) {
            delete bb;
        }
        //BB in free list will also be recorded in m_bbs_list.
    }

    inline IRBB * allocBB()
    {
        IRBB * bb = m_free_list.remove_head();
        if (bb == nullptr) {
            bb = new IRBB();
            BB_id(bb) = m_bb_count++;
            m_bbs_list.append_tail(bb);
        }
        return bb;
    }

    void freeBB(IRBB * bb)
    {
        ASSERT0(bb);
        ASSERTN(!m_free_list.find(bb), ("double free"));
        bb->clean();
        m_free_list.append_head(bb);
    }
    //Count memory usage for current object.
    size_t count_mem() const
    {
        size_t count = 0;
        BBListIter bbit;
        for (IRBB * bb = m_bbs_list.get_head(&bbit);
             bb != nullptr; bb = m_bbs_list.get_next(&bbit)) {
            count += bb->count_mem();
        }
        return count;
    }

    UINT getBBCount() const { return m_bb_count; }
};
//END IRBBMgr

//Exported Functions
void dumpBBLabel(LabelInfoList & lablist, Region const* rg);
void dumpBBList(BBList const* bbl, Region const* rg,
                bool dump_inner_region = true);
//filename: dump BB list into given filename.
void dumpBBList(CHAR const* filename, BBList const* bbl, Region const* rg,
                bool dump_inner_region = true);

bool verifyIRandBB(BBList * bbl, Region const* rg);

} //namespace xoc
#endif
