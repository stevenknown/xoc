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
#ifndef _LIFETIME_H_
#define _LIFETIME_H_

namespace xoc {

#define CROSS_CALL_NUM_THRESHOLD 2
#define REGION_START_POS 2

class LifeTime;
class LifeTimeMgr;
class LinearScanRA;
class LTConstraints;

typedef xcom::TMapIter<PRNO, Reg> PRNO2RegIter;
typedef xcom::TMap<PRNO, Reg> PRNO2Reg;
typedef xcom::TMap<Reg, PRNO> Reg2PRNO;
typedef xcom::DMap<PRNO, Reg, PRNO2Reg, Reg2PRNO> DualMapPRNO2Reg;
typedef xcom::DMapIter<PRNO, Reg, PRNO2Reg, Reg2PRNO> DualMapPRNO2RegIter;

typedef List<LifeTime*>::Iter LTListIter;
class LTList : public List<LifeTime*> {
public:
    void dump(Region const* rg) const;
};

class PreAssignedMgr : public PRNO2Reg {
public:
    //prno: indicates the PR that is pre-assigned.
    //antireg: the target-machine register that 'prno' anticipated.
    void add(PRNO prno, Reg antireg) { set(prno, antireg); }
    void dump(Region const* rg, TargInfoMgr const& timgr) const;
    bool isPreAssigned(PRNO prno) const { return find(prno); }
};

class DedicatedMgr : public DualMapPRNO2Reg {
public:
    //prno: indicates the PR that is dedicated.
    //antireg: the target-machine register that 'prno' anticipated.
    //Note that, the mapping between dedicated prnos and registers must be
    //one-to-one.
    void add(PRNO prno, Reg antireg) { set(prno, antireg); }
    void dump(Region const* rg, TargInfoMgr const& timgr) const;
    bool isDedicated(PRNO prno) const { return find(prno); }
};


class Occ {
public:
    bool m_is_def;
    Pos m_pos;
    IR * m_ir; //may be stmt or exp.
public:
    Occ() : m_is_def(false), m_pos(POS_UNDEF), m_ir(nullptr) {}
    Occ(Pos p) : m_is_def(false), m_pos(p), m_ir(nullptr) {}
    Occ(bool is_d, Pos p, IR * occ) :
        m_is_def(is_d), m_pos(p), m_ir(occ) {}

    bool is_def() const { return m_is_def; }
    Pos pos() const { return m_pos; }

    //Note the ir may be modified.
    IR * getIR() const { return m_ir; }
    IRBB * getBB() const { return getStmt()->getBB(); }
    IR * getStmt() const
    { return getIR()->is_stmt() ? getIR() : getIR()->getStmt(); }
};


typedef C<Occ> * OccListIter; //iterator

class OccList : public xcom::List<Occ> {
    COPY_CONSTRUCTOR(OccList);
public:
    OccList() {}
    void dump() const;
    void append_tail(Occ occ);
    void remove(OccListIter it);
};


typedef enum {
    LT_FLAG_UNDEF = 0,

    //Used to indicate the current lifetime has a DEF occurence or not in
    //the responding occ list, usually a lifetime has more one DEF occurence.
    //But if a lifetime does not has a DEF occurence, that means the IRs using
    //this lifetime do not care the content of the register, so this lifetime
    //doesn't need to be spilled to memory to store the original value
    LT_FLAG_HAS_DEF = 0x1,

    //Used to indicate this lifetime is pre-assigned or not.
    LT_FLAG_IS_PREASSIGNED = 0x2,

    //Used to indicate the current lifetime is spill only or not.
    //Usually, this flag is set only if there is no occurence after the split
    //position, it can only be set to the last child of a lifetime.
    LT_FLAG_SPILL_ONLY = 0x4,

    //Used to indicate the current lifetime has one def or not in its full
    //occurence list. This flag can be used to help to calculate the remat
    //information of lifetime, also it can be used to optimize the spill/reload
    //operation if a lifetime has only ONE def, redundant spill can be avoided
    //if the spill is placed right after the definition.
    //
    //e.g:
    // lifetime $1: <2-53>
    //    | ----------------------------------------------------
    //    | d              u                u  u               u
    // POS: 2  5          17               34 37  41          53
    //       ^              ^              ^    ^             ^
    //       |              |              |    |             |
    //    spill_1         split_1   reload_1  spill_2    reload_2
    // The lifetime is split at POS 18, the spill IR "spill_1" is inserted at
    // the POS 3 after the DEF at POS 2, a reload IR "reload_1" is inserted
    // at POS 33 before the next USE at POS 34, if it is split again at POS
    // 41, normally the spill IR "spill_2" should be inserted after POS 37,
    // but since it is defined only once, the second spill IR "spill_2" can
    // be saved, and reload directly with reload IR "reload_2" at POS 52
    // before next the USE at POS 53 because the value in the spill memory
    // is never changed.
    LT_FLAG_ONE_DEF_ONLY = 0x8,

    //Used to indicate the lifetime is rematerialized or not. If this flag
    //is set, the spill operation of this lifetime can be avoided when it
    //is split.
    LT_FLAG_IS_REMAT = 0x10,
} LT_ATTR_FLAG;


class LTAttrFlag : public UFlag {
public:
    LTAttrFlag(UINT v) : UFlag(v) {}
};

class LifeTime {
    COPY_CONSTRUCTOR(LifeTime);

    //Used to record the number of calls intersected with the whole lifetime
    //except holes, this is not an accurate number, if this number is greater
    //than the CROSS_CALL_NUM_THRESHOLD, it will not be updated.
    BYTE m_call_crossed_num;

    //Used to store the attributes of current lifetime.
    LTAttrFlag m_flag;

    PRNO m_prno;

    //Used to store the remat expression if lifetime can be rematerialized.
    IR const* m_remat_exp;

    //Here we use m_parent and m_ancestor to record the new lifetimes that
    //are created during the lifetime split.
    //For example: $1 are split two times, first $1 is split and generated a
    //decendent lifetime $10, and then $10 is split and generated a decendent
    //lifetime $15.
    //  The m_parent and m_ancestor are set per the table below:
    //       -------------------------------------
    //       |Lifetime   | m_parent | m_ancestor |
    //       |  $1       |   null   |    $1      |
    //       |  $10      |   $1     |    $1      |
    //       |  $15      |   $10    |    $1      |
    //       -------------------------------------
    // lifetime $1: <2-17><34-67>
    //    | ----------------                ----------------------------------
    //    |                u                d  u               u             u
    // POS: 2   5          17               34 37  41          53            67
    //
    // Lifetime overview after the first split for $1 is at Pos 30:
    // lifetime $1: <2-17>
    //    | ----------------
    //    |                u
    // POS: 2   5          17
    //   lifetime $10: <34-67>
    //    |                                 ----------------------------------
    //    |                                 d  u               u             u
    // POS: 2   5          17               34 37  41          53            67
    //
    // Lifetime overview after the second split for $10 is at Pos 41:
    // lifetime $1: <2-17>
    //    | ----------------
    //    |                u
    // POS: 2   5          17
    // lifetime $10: <34-37>
    //    |                                 ----
    //    |                                 d  u
    // POS: 2   5          17               34 37  41          53            67
    // lifetime $15: <53-67>
    //    |                                                    ---------------
    //    |                                                    u             u
    // POS: 2   5          17               34 37  41          53            67
    LifeTime const* m_parent;
    LifeTime const* m_ancestor;
    LTConstraints * m_lt_constraints;

    double m_priority;
    double m_spill_cost;
    RangeVec m_range_vec;

    //The paper proves that 95% of the lifetimes in the program have only one
    //definition, and this property can be leveraged to implement a variety of
    //optimizations. It is worthwhile to add an m_only_def_occ variable to the
    //lifetime class to record the only one definition.
    //Note that the m_only_def_occ will be UNDEF-OCC if current lifetime is not
    //one-def lifetime.
    Occ m_only_def_occ;
    OccList m_occ_list;

    //Used to store the descendant lifetime created during split.
    LTList m_child;
public:
    LifeTime(PRNO prno) : m_call_crossed_num(0), m_flag(0), m_prno(prno),
        m_remat_exp(nullptr), m_parent(nullptr), m_lt_constraints(nullptr)
    {
        m_ancestor = this;
        m_only_def_occ = Occ(POS_UNDEF);
    }
    Range addRange(Pos start, Pos end);
    Range addRange(Pos start) { return addRange(start, start); }
    void addOcc(Occ occ);
    void addChild(LifeTime * lt) { m_child.append_tail(lt); }

    //Return true if current lifetime canbe rematerialized.
    bool canBeRemat() const { return getRematExp() != nullptr; }

    //Clean the position from 'pos'(include pos).
    //e.g:lifetime range is <1-10><15-30>, given position is 16, the
    //    lifetime will be <1-10><15>.
    //Note given 'pos' must be in one of Ranges in current lifetime, otherwise
    //the function do nothing.
    void cleanRangeFrom(Pos pos);

    //The function will move Range and Occ of 'src' that starting from
    //position 'pos' (include pos) into current lifetime.
    void moveRangeVecFrom(LifeTime * src, Pos pos);
    void moveOccListFrom(LifeTime * src, Pos pos);

    //The function moves lifetime from 'src' to current lifetime from position
    //'pos'(include pos).
    //e.g:src is <1-3><10-20>, pos is 19, current lifetime will be <19-20>,
    //while src will be <1-3><10-18>.
    void moveFrom(LifeTime * src, Pos pos);

    void dump(Region const* rg) const;

    //Dump Reg2Lifetime info.
    void dumpReg2LifeTime(
        Region const* rg, LinearScanRA const* ra, Reg r) const;

    //Dump Reg2LifeTime info with specific pos.
    void dumpReg2LifeTimeWithPos(Region const* rg, LinearScanRA const* ra,
        Reg r, Pos start, Pos end, bool open_range) const;

    //Return true and occ if there is occ at given pos.
    bool findOcc(Pos pos, OUT OccListIter & it) const;
    bool findOccAfter(Pos pos, OUT OccListIter & it) const;
    bool findOccBefore(Pos pos, OUT OccListIter & it) const;

    //Return true and range if there is range include the given pos.
    bool findRange(Pos pos, OUT Range & r, OUT VecIdx & ridx,
                   OUT VecIdx * less = nullptr,
                   OUT VecIdx * great = nullptr) const;

    LifeTime const* getAncestor() const { return m_ancestor; }
    PRNO getAnctPrno() const { return m_ancestor->getPrno(); }
    LTAttrFlag getAttrFlag() const { return m_flag; }
    BYTE getCallCrossedNum() const { return m_call_crossed_num; }
    LTList const& getChild() { return m_child; }
    UINT getRangeNum() const
    { return const_cast<LifeTime*>(this)->getRangeVec().get_elem_count(); }
    Pos getFirstPos() const { return getFirstRange().start(); }
    Pos getLastPos() const
    { return const_cast<LifeTime*>(this)->getLastRange().end(); }
    Range getFirstRange() const
    { return const_cast<LifeTime*>(this)->getRangeVec().get(0); }
    Range getLastRange()
    {
        VecIdx i = m_range_vec.get_last_idx();
        if (i == VEC_UNDEF) {
            return Range(POS_UNDEF);
        }
        return m_range_vec.get(i);
    }
    VecIdx getLastRangeIdx() const { return m_range_vec.get_last_idx(); }
    OccList & getOccList() { return m_occ_list; }
    Occ getOnlyDefOcc() const
    { ASSERT0(isOneDefOnly()); return m_only_def_occ; }
    RangeVec & getRangeVec() { return m_range_vec; }
    Range const* getRangeVecBuf() const { return m_range_vec.get_vec(); }
    PRNO getPrno() const { return m_prno; }
    Range getRange(VecIdx idx) const { return m_range_vec.get(idx); }
    IR const* getRematExp() const { return m_remat_exp; }
    LifeTime const* getParent() const { return m_parent; }
    Type const* getFirstOccType() const
    {
        return const_cast<LifeTime*>(this)->getOccList().get_head().
               getIR()->getType();
    }

    Occ getFirstOcc() const
    { return const_cast<LifeTime*>(this)->getOccList().get_head(); }

    IR * getFirstOccStmt() const
    {
        IR * ir = const_cast<LifeTime*>(this)->getFirstOcc().getIR();
        ASSERT0(ir);
        return ir->is_stmt() ? ir : ir->getStmt();
    }
    UINT getOccNum() const
    { return const_cast<LifeTime*>(this)->getOccList().get_elem_count(); }

    double getPriority() const { return m_priority; }
    double getSpillCost() const { return m_spill_cost; }
    LTConstraints * getLTConstraints() const { return m_lt_constraints; }

    //Return the length of lifetime, which describes the number of program
    //points of lifetime.
    UINT getLength() const
    {
        ASSERT0(getLastPos() >= getFirstPos());
        return getLastPos() - getFirstPos() + 1;
    }

    void incCallCrossedNum(UINT num) { m_call_crossed_num += num; }

    void inheritAttrFlag(LifeTime const* lt)
    {
        m_flag.set(lt->getAttrFlag().getFlagSet());
        updateLTConstraintsForSplit(lt);

        //If the lifetime is split, the new lifetime can be rematerialized
        //too, so this remat expression should be inherited from the parent
        //lifetime.
        setRematExp(lt->getRematExp());
    }

    bool isAncestor() const { return this == m_ancestor; }

    //Return true if current lifetime contains the position.
    //e.g:contain the pos
    //    | ----   ---  -- |
    //         ^
    //         |
    //         pos
    //e.g2:not contain the pos
    //    | ----   ---  -- |
    //          ^
    //          |
    //          pos
    bool is_contain(Pos pos) const;

    //Return true if current lifetime covers the position, otherwise pos exceed
    //the lifetime.
    //e.g:cover the pos
    //    | ----   ---  -- |
    //         ^
    //         |
    //         pos
    //e.g2:not cover the pos
    //    | ----   ---  -- |
    //     ^              ^
    //     |              |
    //     pos            pos
    bool is_cover(Pos pos) const;

    //Return true if current lifetime intersects with lt.
    //e.g:intersect
    // current: |-----|     |-------|
    //      lt:      |---|
    //e.g:not intersect
    // current: |-----|     |-------|
    //      lt:        |---|         |--|
    bool is_intersect(LifeTime const* lt) const;
    bool is_intersect(RangeVec const& rv2) const;

    //Return true if 'ir' is an DEF occurrence of current lt.
    bool isDefOcc(IR const* ir) const
    {
        if (!ir->is_stmt()) { return false; }
        IR const* pr = const_cast<IR*>(ir)->getResultPR();
        return pr != nullptr && pr->getPrno() == getPrno() ? true : false;
    }

    //Return true if the occ has a DEF at least.
    bool isOccHasDef() const { return m_flag.have(LT_FLAG_HAS_DEF); }

    //Return true if current lifetime is pre-assigned.
    bool isPreAssigned() const { return m_flag.have(LT_FLAG_IS_PREASSIGNED); }

    //Return true if current lifetime is spill only or not.
    bool isSpillOnly() const { return m_flag.have(LT_FLAG_SPILL_ONLY); }

    //Return true if current lifetime has one define only.
    bool isOneDefOnly() const { return m_flag.have(LT_FLAG_ONE_DEF_ONLY); }

    //Return true if current lifetime is remateralized.
    bool isRematerialized() const { return m_flag.have(LT_FLAG_IS_REMAT); }

    //Return true if 'ir' is an USE occurrence of current lt.
    bool isUseOcc(IR const* ir) const
    {
        return ir->isReadPR() && ir->getPrno() == getPrno() ? true : false;
    }

    void removeOccFrom(OccListIter it);
    void removeOneDefOnly()
    {
        m_flag.remove(LT_FLAG_ONE_DEF_ONLY);
        m_only_def_occ = Occ(POS_UNDEF);
    }
    void removeRangeFrom(VecIdx idx);

    void setAncestor(LifeTime const* anc) { m_ancestor = anc; }

    void setOccHasDef() { m_flag.set(LT_FLAG_HAS_DEF); }
    void setOnlyDefOcc(Occ occ) { m_only_def_occ = occ; }

    //Set current lifetime to be pre-assigned, that means the lifetime must
    //assign pre-assigned register.
    void setPreAssigned() { m_flag.set(LT_FLAG_IS_PREASSIGNED); }

    void setRange(VecIdx idx, Range r);
    void setLastRange(Range r)
    {
        m_range_vec.get_last_idx() == VEC_UNDEF ?
            m_range_vec.append(r) :
            m_range_vec.set(m_range_vec.get_last_idx(), r);
    }
    void setParent(LifeTime const* parent) { m_parent = parent; }
    void setPriority(double pri) { m_priority = pri; }
    void setRematExp(IR const* exp) { m_remat_exp = exp; }
    void setSpillCost(double cost) { m_spill_cost = cost; }
    void setSpillOnly() { m_flag.set(LT_FLAG_SPILL_ONLY); }
    void setRematerialized() { m_flag.set(LT_FLAG_IS_REMAT); }
    void setOneDefOnly(Occ occ)
    {
        m_flag.set(LT_FLAG_ONE_DEF_ONLY);
        ASSERT0(this->getAncestor());
        const_cast<LifeTime*>(this->getAncestor())->setOnlyDefOcc(occ);
    }

    //Shrink the lifetime forward to the last occ position.
    void shrinkForwardToLastOccPos();
    void setLTConstraints(LTConstraints * lt_constraints)
    {
        ASSERT0(lt_constraints);
        m_lt_constraints = lt_constraints;
    }

    //This function handles the propagation and update of register allocation
    //constraints when a lifetime is split into two. The new
    //lifetime should inherit the original constraints, and the conflict
    //PRs need to be updated accordingly.
    void updateLTConstraintsForSplit(LifeTime const* old_lt);

    //This function updates the position at the tail of occ list.
    void updateTailOccPos(Pos pos)
    {
        Occ occ_old = getOccList().get_tail();
        occ_old.m_pos = pos;
        getOccList().remove_tail();
        addOcc(occ_old);
    }
    bool verify() const;
};

typedef Vector<Pos> Call2PosVec;
typedef xcom::TMap<PRNO, VecIdx> PRNO2CallID;

//This class is used to count the number of calls crossed with a lifetime
//during the lifetime construction phase. You can calculte the number of cross
//call of a lifetime after the lifetime is completely constructed, but this
//option will take extra time to get the cross call number.
//
//A vector will be used to store all the calls in lexicographical order, the
//content of this vector is the position of responding call IR. Some terms
//will be introduced based on this call vector:
//  call_id: The index of call vector can be used to indicate which call is.
//  baseline_call_id: The call_id before the the prevoius occurence of a
//                    lifetime, it should be recorded for every lifetime.
//                    It is stored as the value of the map PRNO2CallID.
//  current_call_id: The max call id in the call vector, which is used to
//                   describe the newest call ir so far during the traverse of
//                   the whole IRs, this is shared by the all lifetimes.
//  call_crossed_num: The number of calls which intersect with a whole lifetime
//                    excluding the live range holes.
//
//Since the complete lifetime normally are consisted by many live ranges,
//and these live ranges and the position of call are both incremental during
//the traverse of the wholes IRs in the lexicographical order, so the basic
//idea of this algorithm is to statistic the number of call of between the
//baseline_call_id and current_call_id for every lifetime. Please refer to
//the lifetime example below, there are total three calls in this function,
//but only two calls are the valid call_crossed_num for this lifetime.
//
//Lifetime example:
//   lifetime: <2-17><34-67>
//    | ----------------                ----------------------------------
//    |                u                d  u               u             u
// POS: 2   5          17 20            34 37  41          53            67
//          ^             ^                    ^
//          |             |                    |
//     call_id = 0   call_id = 1         call_id = 2
//
//Algorithm details:
//  For each IR of the IRBBlist:
//    1. Add the position of the call to the call vector if the IR is a call
//       statement.
//    2. For all the prnos (lifetime) which are used in the RHS:
//       2.1. If the call vector is empty or the call_crossed_num of lifetime
//            exceeds a predefined threshold, return.
//       2.2. If the position responding to the current_call_id is less than
//            the DEF position of current live range, return. This means the
//            the call position indicated by current call id doesn't
//            intersects with the current live range. [NOTE1]
//       2.3. Check the baseline_call_id of the lifetime is not recorded for
//            the lifetime:
//            2.3.1. If the baseline_call_id of the lifetime is not recorded,
//                   that means this lifetime is live from the entry of region.
//                   and all the number of calls in the call vector should be
//                   counted as the valid cross call number.
//              2.3.1.1. call_crossed_num = current_call_id + 1.
//              2.3.1.2. baseline_call_id = current_call_id.
//            2.3.2. If the baseline_call_id of the lifetime is recorded, just
//                   need to count the number calls between baseline_call_id
//                   and current_call_id as the valid cross call number.
//              2.3.2.1. If baseline_call_id is less than current_call_id
//                       call_crossed_num += current_call_id - baseline_call_id.
//                       baseline_call_id = current_call_id.
//              2.3.2.2. Return.
//    3. For the prnos (lifetime) which are defined in the LHS:
//       3.1. If the call vector is empty or the call_crossed_num of lifetime
//            exceeds a predefined threshold, return.
//       3.2. baseline_call_id = current_call_id.
//
//    [NOTE1]: For the above lifetime, when traverse at the position 37, the
//             current_call_id is 1 and start of current range is 34, the
//             position of call responding to current_call_id 1 is 20, which
//             is less than 34, so the call responding to the call_id 0 should
//             be excluded when count the call_crossed_num.
//
//We will list the detail steps to show how this algorithm works by using
//the above lifetime example:
//  1. At pos 2, the call vector Call2PosVec is empty, do nothing
//        Call2PosVec = []
//  2. At pos 5, a new call is met, step 2.1 is performed.
//     The related data will be changed as:
//       Call2PosVec = [5], current_call_id = 0,
//       baseline_call_id = not recorded
//  3. At pos 17, it is a use position, step 2.3.1 is performed.
//     The related data will be changed as:
//       Call2PosVec = [5], current_call_id = 0,
//       baseline_call_id = 0, call_crossed_num = 1
//  4. At pos 20, a new call is met, step 1 is performed.
//     The related data will be changed as:
//       Call2PosVec = [5, 20], current_call_id = 1,
//       baseline_call_id = 0, call_crossed_num = 1
//  5. At pos 34, it is a define position,  step 3.2 is performed.
//     The related data will be changed as:
//       Call2PosVec = [5, 20], current_call_id = 1,
//       baseline_call_id = 1, call_crossed_num = 1
//  6. At pos 37, it is a use position, step 2.2 is performed.
//     The related data will not be changed:
//       Call2PosVec = [5, 20], current_call_id = 1,
//       baseline_call_id = 1, call_crossed_num = 1
//  7. At pos 41, a new call is met, step 1 is performed.
//     The related data will be changed as:
//       Call2PosVec = [5, 20, 41], current_call_id = 2,
//       baseline_call_id = 1, call_crossed_num = 1
//  8. At pos 53, it is a use position, step 2.3.2.1 is performed.
//     The related data will be changed as:
//       Call2PosVec = [5, 20, 41], current_call_id = 2,
//       baseline_call_id = 2, call_crossed_num = 2
//  9. At pos 67, it is a use position, step 2.3.2.2 is performed.
//     The related data will not be changed:
//       Call2PosVec = [5, 20, 41], current_call_id = 2,
//       baseline_call_id = 2, call_crossed_num = 2
//
class CrossedCallCounter {
    COPY_CONSTRUCTOR(CrossedCallCounter);
    Call2PosVec m_call2pos;
    PRNO2CallID m_prno2callid;
public:
    CrossedCallCounter() {}
    void addNewCall(Pos call_pos) { m_call2pos.append(call_pos); }
    void updateForUse(LifeTime * lt, Pos def_pos);
    void updateForDef(LifeTime * lt);
};

typedef Vector<LifeTime*> PRNO2LT;


class RegSetImpl;
class RangeInfo;
typedef xcom::Vector<LifeTime*> PRNO2LT;
typedef xcom::Vector<RangeInfo*> RangeInfoVec;
typedef xcom::TMap<IR const*, Pos> IR2POS;


//
//START LifeTimeMgr
//
class LifeTimeMgr {
    friend class OccList;
    COPY_CONSTRUCTOR(LifeTimeMgr);
protected:
    bool m_use_expose; //true to compute exposed def/use for each BB.
    Region * m_rg;
    SMemPool * m_pool;

    //Record the max position of the region when generate the lifetime, and
    //the max position will be use tod calculte the distance between any
    //position inside the region and the end of the region.
    Pos m_max_pos;

    //Record the Lifetime that allocated in LSRA pass.
    LTList m_lt_list;

    //Record the Lifetime that used for Reg2LifeTime.
    LTList m_reg2lt_list;

    //Record the corresponded LifeTime info of PRNO.
    PRNO2LT m_prno2lt;

    //Record the corresponded LifeTime info of physical register.
    PRNO2LT m_reg2lt;

    xcom::Vector<Pos> m_bb_entry_pos;
    xcom::Vector<Pos> m_bb_exit_pos;
    #ifdef _DEBUG_
    xcom::TMap<IR const*, Pos> m_ir2pos;
    #endif
protected:
    //This func adds the position of caller register to the lifetime.
    virtual void addCallerLTPos(Pos pos, IR const* ir) {}

    LifeTime * allocLifeTime(PRNO prno);

    //Allocate the LifeTime for Reg2LifeTime.
    LifeTime * allocReg2LifeTime(PRNO prno);

    void computeLifeTimeBB(UpdatePos & up, IRBB const* bb,
                           PreAssignedMgr const& preassigned_mgr,
                           Pos livein_def, IRIter & irit,
                           MOD CrossedCallCounter & cross_call_counter);
    void destroy();
    void init(Region * rg);
    void setMaxPos(Pos pos) { ASSERT0(pos != POS_UNDEF); m_max_pos = pos; }
    void * xmalloc(size_t size);
    bool useExpose() const { return m_use_expose; }
public:
    LifeTimeMgr(Region * rg)
    { m_pool = nullptr; m_use_expose = false; init(rg); }
    virtual ~LifeTimeMgr() { destroy(); }

    void computeCrossedCallNum(UpdatePos & up, BBList const* bblst);
    void computeCrossedCallNumBB(UpdatePos & up, IRBB const* bb,
        IRIter & irit, MOD CrossedCallCounter & cross_call_counter);

    virtual void computeLifeTime(UpdatePos & up, BBList const* bblst,
                                 PreAssignedMgr const& preassigned_mgr);

    //If 'cur_range' is interected with the last range in the 'range_vec',
    //these two ranges will be merged into a range and stored into 'range_vec'.
    //'range_vec': RangeVec is used to contain Range.
    //'last_idx': the last index of the 'range_vec'.
    //'cur_range': range that will be stored into 'range_vec'.
    //e.g.: 'cur_range.start() is_interect range_vec.get(last_idx)',
    //      thus these two ranges can be merged into a range.
    //range_vec:   | [S    E]    |
    //cur_range:   |    [S    E] |
    //after_merge: | [S       E] |
    bool canMergeWithPreRange(RangeVec & range_vec,
                              UINT last_idx, Range & cur_range);

    void dumpAllLT(UpdatePos & up, BBList const* bblst,
                   bool dumpir = true) const;
    void dump() const;

    LifeTime * genLifeTime(PRNO prno);
    LifeTime * getLifeTime(PRNO prno) const { return m_prno2lt.get(prno); }
    LifeTime * getRegLifeTime(Reg reg) const { return m_reg2lt.get(reg); }
    LTList const& getLTList() const { return m_lt_list; }
    Pos getBBStartPos(UINT bbid) const { return m_bb_entry_pos.get(bbid); }
    Pos getBBEndPos(UINT bbid) const { return m_bb_exit_pos.get(bbid); }
    Pos getMaxPos() const { return m_max_pos; }
    PRNO2LT const& getPrno2LT() const { return m_prno2lt; }
    PRNO2LT const& getReg2LT() const { return m_reg2lt; }

    //Initialize Reg2LifeTimeInfo.
    void initReg2LifeTimeInfo();

    //Insert 'range_vec' into the lifetime of 'reg'.
    void mergeRegLifetimeWIthRange(Reg reg, RangeVec const& range_vec);

    //Merged two RangeVec 'rv_ori' and 'rv_new'.
    //Then the new RangeVec will be stored into 'rv_ori'
    void mergeLifeTime(RangeVec & rv_ori, RangeVec const& rv_new);

    //Merged the new lifetime 'lt_new' into the RangeVec of 'reg'.
    void mergeReg2LifeTime(Reg reg, MOD LifeTime * lt_new);

    //Clean the lifetime info before computation.
    void reset();
    void recomputeLifeTime(UpdatePos & up, BBList const* bblst,
                           PreAssignedMgr const& preassigned_mgr);
    void recordPos(IR const* ir, Pos pos);

    //Rename each occurrences of 'lt' to 'newprno'.
    void renameLifeTimeOcc(LifeTime const* lt, PRNO newprno);

    //Update the position at the BB entry.
    void updateBBEntryPos(IRBB const* bb, MOD UpdatePos & up,
                          MOD Pos & dpos_bb_start, MOD Pos & upos_bb_start);

    //Update the postion at the BB exit.
    void updateBBExitPos(IRBB const* bb, MOD UpdatePos & up,
                         MOD Pos & dpos_bb_end, MOD Pos & upos_bb_end);

    bool verifyPos(IR const* ir, Pos pos) const;
    bool verify() const;
};
//END LifeTimeMgr

//
//START LifeTime2DMgr
//
typedef Vector<IR*> PRNO2IR;
class LifeTime2DMgr : public LifeTimeMgr {
    COPY_CONSTRUCTOR(LifeTime2DMgr);
protected:
    LivenessMgr * m_live_mgr;
    LinearScanRA * m_lsra;

    //Maps from the register of caller to the responding lifetime.
    PRNO2LT m_caller2lt;
protected:
    //This function adds the position of caller register to the lifetime.
    //Since the call statement can clober the caller registers, the position
    //of call statement is added in the caller lifetime in order to indicate
    //this reg is unavailable at this position.
    //e.g:
    //  A call at position 30 is added to the lifetime of REG_1.
    //  LT:REG_1,range:<22-23><24-27><30><35-43>
    //   |                     ------  -   ----------
    //   |                     d    u  ^   d        u
    //                                 |
    //                                call
    virtual void addCallerLTPos(Pos pos, IR const* ir) override;

    LifeTime * getCallerLT(Reg r) const
    { ASSERT0(r != REG_UNDEF); return m_caller2lt.get(r); }

    //Merge the livein of BB info into the lifetime.
    void mergeLiveIn(IRBB const* bb, UpdatePos & up,
                     Pos dpos_bb_start);

    void initCallerLifeTime();

    //Merge the liveout of BB info into the lifetime.
    void mergeLiveOut(IRBB const* bb, UpdatePos & up, Pos livein_def,
                      Pos dpos_bb_end);

public:
    LifeTime2DMgr(Region * rg, LinearScanRA * ra) :
        LifeTimeMgr(rg), m_lsra(ra) {}

    virtual void computeLifeTime(UpdatePos & up, BBList const* bblst,
        PreAssignedMgr const& preassigned_mgr) override;
};
//END LifeTime2DMgr

} //namespace xoc
#endif
