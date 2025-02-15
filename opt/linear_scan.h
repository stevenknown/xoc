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
#ifndef _LINEAR_SCAN_H_
#define _LINEAR_SCAN_H_

namespace xoc {

class LifeTime;
class LifeTimeMgr;
class TargInfoMgr;
class ArgPasser;
class ApplyToRegion;

//
//START LexBackwardJump
//
#define BKJUMP_srcbb(b) ((b)->srcbb)
#define BKJUMP_dstbb(b) ((b)->dstbb)
class LexBackwardJump {
public:
    IRBB const* srcbb;
    IRBB const* dstbb;

    LexBackwardJump(IRBB const* src, IRBB const* dst) : srcbb(src), dstbb(dst)
    { }
    void init(IRBB const* src, IRBB const* dst)
    {
        ASSERT0(src && dst);
        srcbb = src;
        dstbb = dst;
    }
};
//END LexBackwardJump

//
//START Occurence
//
//Lightweight structure used in backward jump analysis.
#define OCC_first(r) ((r)->first)
#define OCC_last(r) ((r)->last)
class Occurence {
public:
    Pos first;
    Pos last;

    Occurence() : first(POS_UNDEF), last(POS_UNDEF) {}
};
//END Occurence

//The attribute flag responding to a POS.
typedef enum {
    //Undefined value.
    POS_ATTR_UNDEF            = 0x0,

    //Indicate this IR should not generate the final instruction.
    //If this attribute is set, the IR responding to this POS should be deleted
    //after the register assignment.
    POS_ATTR_NO_CODE_GEN      = 0x1,

    //Indicate the lifetime of PR should be extended to the next DEF from the
    //POS where POS_ATTR_LT_NO_TERM_AFTER is set.
    //e.g: This attribute is set at POS 17.
    //   Original lifetime: <2-17><34-67>
    //    | ----------------                ----------------------------------
    //    |                u                d      u           u             u
    //                     ^
    //                     |
    //                 POS = 17
    //
    //   Modified lifetime: <2-33><34-67>
    //    | ------------------------------------------------------------------
    //    |                u                d      u           u             u
    //
    //   For this example, by giving POS 17, the range of lifetime <2-17> is
    //   extended to the 33.
    POS_ATTR_LT_NO_TERM_AFTER = 0x2,

    //Indicate the lifetime of PR should be shrunk from the start of region to
    //to the POS where POS_ATTR_LT_SHRINK_BEFORE is set.
    //e.g: This attribute is set at POS 17.
    //   Original lifetime: <2-17><34-67>
    //    | ----------------                ----------------------------------
    //    |                u                d      u           u             u
    //                     ^
    //                     |
    //                 POS = 17
    //
    //   Modified lifetime: <17><34-67>
    //    |                -                ----------------------------------
    //    |                u                d      u           u             u
    //
    //   For this example, by giving POS 17, the range of lifetime <2-17> is
    //   shrunk from 2 to 17.
    POS_ATTR_LT_SHRINK_BEFORE   = 0x4,


    //Indicate the lifetime of PR should be extended to the end of BB from the
    //POS where POS_ATTR_LT_EXTEND_BB_END is set.
    //e.g: This attribute is set at POS 53, and the end of BB is at POS 67.
    //   Original lifetime: <2-17><34-53>
    //    | ----------------                --------------------
    //    |                u                d      u           u
    //                                                         ^
    //                                                         |
    //                                                     POS = 53
    //
    //   Modified lifetime: <2-17><34-67>
    //    | ----------------                ----------------------------------
    //    |                u                d      u           u             u
    //
    //   For this example, by giving POS 53, the range of lifetime <34-53> is
    //   extended to the end of BB 67.
    POS_ATTR_LT_EXTEND_BB_END   = 0x8,
} POS_ATTR_FLAG;

//This enum is used to describe the lexical sequence of BB for the fake-use IRs
//related to the same PR.
typedef enum {
    BB_SEQ_UNDEF = 0,

    //The fake-use IR in this BB is the first in lexical sequence.
    BB_SEQ_FIRST = 1,
} BB_SEQ;

//
//START PosAttr
//
class PosAttr : public UFlag {
    IRBB const* m_bb;
    IR const* m_ir;
    BB_SEQ m_seq;
public:
    PosAttr(UINT v, IRBB const* bb, IR const* ir) :
            UFlag(v), m_bb(bb), m_ir(ir) {}

    void init(UINT v, IRBB const* bb, IR const* ir)
    {
        set(v);
        setBB(bb);
        setIR(ir);
        m_seq = BB_SEQ_UNDEF;
    }

    IRBB const* getBB() const { return m_bb; }
    IR const* getIR() const { return m_ir; }
    BB_SEQ getSequence() const { return m_seq; }
    void setBB(IRBB const* bb) { m_bb = bb; }
    void setIR(IR const* ir) { m_ir = ir; }
    void setSequence(BB_SEQ seq) { m_seq = seq; }
};
//END PosAttr

//The insert mode used for Backward jump analysis. The fake-use IR should
//be added to the BB at the specified location per the INSERT mode.
typedef enum {
    INSERT_MODE_UNDEF = 0,

    //Indicate the fake-use IR should be appended at the head of BB.
    // eg.
    // LABEL: label_XX
    // ----- BB start ---
    //                                     <-------- fake IR
    // stpr $2:u32 id:xx
    //     xxxx
    //  goto label xxx id:xx
    // ----- BB end ---
    INSERT_MODE_HEAD = 1,

    //Indicate the fake-use IR should be appended at the end of BB but before
    //the branch IR.
    // eg.
    // LABEL: label_XX
    // ----- BB start ---
    // stpr $2:u32 id:xx
    //     xxxx
    //                                     <-------- fake IR
    //  goto label XXX id:xx
    // ----- BB end ---
    INSERT_MODE_TAIL = 2,

    //Indicate the fake-use IR should be inserted in the middle of BB, usually a
    //marker IR is provided to indicate the relative position inside BB.
    INSERT_MODE_MID = 3,
} INSERT_MODE;

//
//START BBPos
//
#define BBPOS_prno(r) ((r).m_prno)
#define BBPOS_bbid(r) ((r).m_bbid)
#define BBPOS_flag(r) ((r).m_flag)
class BBPos {
public:
    PRNO m_prno;
    UINT m_bbid;
    UINT m_flag;
public:
    BBPos(UINT v) : m_prno(v), m_bbid(0), m_flag(0) {}
    BBPos(PRNO pr, UINT id, UINT flag) : m_prno(pr), m_bbid(id), m_flag(flag)
    {}
};
//END BBPos

//
//START BBPosCmp
//
class BBPosCmp : public CompareKeyBase<BBPos> {
public:
    bool is_less(BBPos t1, BBPos t2) const
    {
        return (BBPOS_prno(t1) < BBPOS_prno(t2)) &&
               (BBPOS_bbid(t1) < BBPOS_bbid(t2)) &&
               (BBPOS_flag(t1) < BBPOS_flag(t2));
    }
    bool is_equ(BBPos t1, BBPos t2) const
    {
        return (BBPOS_prno(t1) == BBPOS_prno(t2)) &&
               (BBPOS_bbid(t1) == BBPOS_bbid(t2)) &&
               (BBPOS_flag(t1) == BBPOS_flag(t2));
    }
};
//END BBPosCmp

#define FAKEUSE_bb(r) ((r)->bb)
#define FAKEUSE_pos(r) ((r)->pos)
class FakeUse {
public:
    IRBB const* bb;
    Pos pos;
};

//
//START BackwardJumpAnalysisResMgr
//
//BackwardJumpAnalysisResMgr responsible to manager the resources that needed
//during the backward jump analysis.
class BackwardJumpAnalysisResMgr {
    COPY_CONSTRUCTOR(BackwardJumpAnalysisResMgr);
    SMemPool * m_pool;
public:
    BackwardJumpAnalysisResMgr();
    ~BackwardJumpAnalysisResMgr();

    //Generate a PosAttr object and append the pointer in the m_pos_attr_list.
    PosAttr * genPosAttr(UINT v, IRBB const* bb, IR const* ir);

    //Generate a Occurence object and append the pointer in the m_occ_list.
    LexBackwardJump * genLexBackwardJump(IRBB const* srcbb, IRBB const* dstbb);

    //Generate a Occurence object and append the pointer in the m_occ_list.
    Occurence * genOccurence()
    { return (Occurence*)xmalloc(sizeof(Occurence)); }

    FakeUse * genFakeUse()
    { return (FakeUse*)xmalloc(sizeof(FakeUse)); }

    void * xmalloc(size_t size);
};
//END BackwardJumpAnalysisResMgr

//
//START FakeVarMgr
//
class FakeVarMgr {
    COPY_CONSTRUCTOR(FakeVarMgr);
    Region * m_rg;
    Var * m_fake_scalar_var;
    Var * m_fake_vec_var;
public:
    FakeVarMgr(Region * rg)
    {
        m_rg = rg;
        m_fake_scalar_var = nullptr;
        m_fake_vec_var = nullptr;
    }
    ~FakeVarMgr() { }

    //This func generates a fake var per the type.
    Var * genFakeVar(Type const* ty);
};
//END FakeVarMgr

//Record the attribute of IR.
typedef xcom::TMap<BBPos, PosAttr const*, BBPosCmp> BBPos2Attr;
typedef xcom::TMapIter<BBPos, PosAttr const*> BBPos2AttrIter;

//Map from prno to the Occurence.
typedef xcom::TMap<PRNO, Occurence const*> Prno2OccMap;
typedef xcom::TMapIter<PRNO, Occurence const*> Prno2OccMapIter;

//Map from prno to the fake-use info.
typedef xcom::TMap<PRNO, FakeUse*> Prno2FakeUse;
typedef xcom::TMapIter<PRNO, FakeUse*> Prno2FakeUseIter;

//Used to store the PRLiveSet related to the backward jump BBs.
typedef List<PRLiveSet const*> PRLiveSetList;

//Used to store the backward jump edges.
typedef List<LexBackwardJump const*> BackwardEdgeList;

//
//START LexBackwardJumpAnalysis
//
//LexBackwardJumpAnalysis is used to check the CFG whether have the backward
//jump, and insert some fake-use IRs for some prno after analysis.
//eg. BB3-->BB2 is a backward jump:
//    BB1
//     |
//     |
//     V
//    BB2 <------
//     |        |
//     |        |
//     V        |
//    BB3 -------
//     |
//     |
//     V
//    BB4
class LexBackwardJumpAnalysis {
    COPY_CONSTRUCTOR(LexBackwardJumpAnalysis);
    friend class OccRecorderVF;
    Region * m_rg;
    BBPos2Attr * m_pos2attr;
    IRMgr * m_irmgr;
    BBList * m_bb_list;
    LivenessMgr * m_live_mgr;
    BackwardEdgeList m_backward_edges;
    Vector<Pos> m_bb_entry_pos;
    Vector<Pos> m_bb_exit_pos;
    Prno2OccMap m_prno2occ;
    FakeVarMgr * m_fake_var_mgr;
    LinearScanRA * m_lsra;
    Prno2FakeUse m_pr2fakeuse_head;
    Prno2FakeUse m_pr2fakeuse_tail;
    BackwardJumpAnalysisResMgr * m_resource_mgr;
public:
    LexBackwardJumpAnalysis(Region * rg, BBPos2Attr * pos2attr,
                            FakeVarMgr * fake_var_mgr, LinearScanRA * lsra);
    virtual ~LexBackwardJumpAnalysis();

    //The main function controls the analysis flow.
    bool analyze();

    //Release the resources used during the analysis.
    void destroy();

    void dump();

    //Init the resources for the analysis.
    void init();

    //Reset the resources for the analysis.
    void reset();
protected:
    //Add the backward jump to the set m_backward_edges.
    void addBackwardJump(IRBB const* srcbb, IRBB const* dstbb)
    {
        LexBackwardJump const* e = m_resource_mgr->genLexBackwardJump(srcbb,
                                                                      dstbb);
        m_backward_edges.append_tail(e);
    }

    //Collect all the backward jumps in the control flow graph.
    void collectBackwardJumps();

    void generateFakeUse();

    //Generate the occurence for pr in m_pr_live_set_list. The occurence is a
    //simple line/IR sequence ID.
    void generateOccurence();

    //Generate the occurence for BB.
    //pos: the BB start position.
    void generateOccurenceForBB(IRBB const* bb, MOD Pos & pos);

    //This func records the end/exit position of the input BB.
    Pos getBBEndPos(UINT bbid) const { return m_bb_exit_pos.get(bbid); }

    //This func records the start/entry position of the input BB.
    Pos getBBStartPos(UINT bbid) const { return m_bb_entry_pos.get(bbid); }

    void insertFakeUse();

    //This func inserts the fake-use IR at the head or tail of input BB.
    void insertFakeUse(IRBB const* bb, PRNO prno, INSERT_MODE mode);

    //This func inserts the fake-use IR at the head of input BB.
    void insertFakeUseAtBBEntry(IRBB const* bb, PRNO prno, BBPos const& pos);

    //This func inserts the fake-use IR at the tail of input BB.
    void insertFakeUseAtBBExit(IRBB const* bb, PRNO prno, BBPos const& pos);

    //Record the fake-use information.
    void recordFakeUse(PRNO prno, IRBB const* bb, INSERT_MODE mode);

    //This func records the occurence of the input prno at the specified pos.
    void recordOccurenceForPR(PRNO prno, Pos pos);
};
//END LexBackwardJumpAnalysis


//This enum is used to indicate the PR is DEF or USE in an IR.
typedef enum {
    //Initial state for a PR.
    PR_DU_UNDEF = 0,

    //PR is a DEF in an IR.
    PR_DU_DEFINED = 1,

    //PR is a USE in an IR.
    PR_DU_USED = 2,
} PR_DU_STATE;

//This enum is the check state of a PR in a BB which has multiple predecessors
//on CFG.
typedef enum {
    //Initial state for check state.
    PR_CHK_UNDEF = 0,

    //This means the PR_DU_STATE of a PR in all predecessors is all
    //PR_DU_DEFINED or PR_DU_USED.
    PR_CHK_GOOD = 1,

    //This means the PR_DU_STATE of a PR in some predecessors is PR_DU_DEFINED,
    //while it is PR_DU_USED in other predecessors.
    PR_CHK_CONFLICT = 2,
} PR_CHK_STATE;

//This union is used to describe the PR state, which includes the DU state and
//the check state.
typedef union {
    //The whole data combined with DU state and check state.
    UINT value;
    struct {
        //DEF and USE state of PR, and the value should be from PR_DU_STATE.
        UINT16 du;

        //The check state of PR, and the value should be from PR_CHK_STATE.
        UINT16 chk;
    } state;
} PRState;


//
//START RematCtx
//
class RematCtx {
public:
    //Record the expression that used in rematerialization.
    IR const* material_exp;
};
//END RematCtx


//
//START PRNOConstraintsTab
//
//This class is specifically used to store some PRs,
//which have certain relationships with each other.
class PRNOConstraintsTab : public xcom::TTab<PRNO> {
public:
    //Checks if the current object (this) and src_tab share two or
    //more common elements.
    //e.g: if `this` contains {1, 2, 3} and `src_tab` contains {2, 3, 4},
    //the function will return true because they share two elements (2 and 3).
    //TODO: Use this function with caution; it has low efficiency and is
    //recommended only for ASSERT checks.
    bool hasTwoOrMoreCommonElements(PRNOConstraintsTab const& src_tab) const;
};
//END PRNOConstraintsTab


typedef List<LifeTime*>::Iter LTSetIter;
typedef xcom::TMap<LifeTime*, xcom::C<LifeTime*>*> LT2Holder;
class LTSet : public xcom::EList<LifeTime*, LT2Holder> {
    COPY_CONSTRUCTOR(LTSet);
public:
    LTSet() {}
};

typedef xcom::TTabIter<LifeTime*> LTTabIter;
class LTTab : public xcom::TTab<LifeTime*> {
};

typedef TMap<PRNO, Var*> PRNO2Var;
typedef TMapIter<PRNO, Var*> PRNO2VarIter;


//
//START RegSetImpl
//
//This class represents the main manipulating interfaces to target dependent
//RegSet information.
class RegSetImpl {
    COPY_CONSTRUCTOR(RegSetImpl);
protected:
    LinearScanRA & m_ra;

    //Target scalar register.
    RegSet const* m_target_callee_scalar;
    RegSet const* m_target_caller_scalar;
    RegSet const* m_target_param_scalar;
    RegSet const* m_target_return_value_scalar;

    //Target vector register.
    RegSet const* m_target_callee_vector;
    RegSet const* m_target_caller_vector;
    RegSet const* m_target_param_vector;
    RegSet const* m_target_return_value_vector;

    //Target allocable register.
    RegSet const* m_target_allocable_scalar;
    RegSet const* m_target_allocable_vector;

    //Scalar register.
    RegSet m_avail_callee_scalar;
    RegSet m_avail_caller_scalar;
    RegSet m_avail_param_scalar;
    RegSet m_avail_return_value_scalar;

    //Vector register.
    RegSet m_avail_callee_vector;
    RegSet m_avail_caller_vector;
    RegSet m_avail_param_vector;
    RegSet m_avail_return_value_vector;

    //Record used register.
    RegSet m_used_callee;
    RegSet m_used_caller;

    //Allocable register.
    RegSet m_avail_allocable;
protected:
    //Some other modules will occupy additional registers, making these
    //registers unable to participate in normal register allocation. This
    //interface checks these modules and collects available registers.
    //Note that special handling can be done based on the architecture.
    virtual void collectOtherAvailableRegister()
    {
        //Collect FP register.
        //If the frame pointer register can be allocated and the dynamic stack
        //function and prologue&epilogue inserter function have not used this
        //register during the current compilation process, it can be used to
        //participate in scalar callee saved register allocation.
        //In addition, if debug mode is turned on, the frame pointer register
        //has a special role and cannot participate in allocation. Also, it can
        //not be used if debugging linear scan register allocation.
        if (isFPAllocable()) {
            Reg reg = getTIMgr().getFP();
            m_avail_callee_scalar.bunion(reg);
            m_avail_allocable.bunion(reg);
        }
    }

    void destroyDebugRegSet();
    void destroyRegSet();
    void initAvailRegSet();

    //Init the register set for debug purpose.
    //The function is always used by pass writer to manipulate the number and
    //the content of specific register set.
    //E.g: given a new linear-scan algorithm, the pass writer is going to test
    //the ability of the new algo. However, if the target machine has a lot of
    //physical registers, the pass writer expects to use smaller register set,
    //say 2 physical registers, to test the algo's ablity, pass writer can
    //overwrite the allocable-register-set through this inteface to
    //cut down the number of alloable-register-set.
    virtual void initDebugRegSet() { ASSERTN(0, ("Target Depedent Code")); }
public:
    RegSetImpl(LinearScanRA & ra);
    virtual ~RegSetImpl() { destroyRegSet(); }
    void dumpAvailRegSet() const;

    //Free register.
    void freeReg(xgen::Reg reg);
    //Free register.
    void freeReg(LifeTime const* lt);

    //Free register from alias register set.
    virtual void freeRegisterFromAliasSet(Reg r);
    //Free register from all caller alias register set.
    virtual void freeRegisterFromCallerAliasSet(Reg r);
    //Free register from all callee alias register set.
    virtual void freeRegisterFromCalleeAliasSet(Reg r);
    //Free register from all param alias register set.
    virtual void freeRegisterFromParamAliasSet(Reg r);
    //Free register from all return value alias register set.
    virtual void freeRegisterFromReturnValueAliasSet(Reg r);

    //Get allocable regsets.
    RegSet const& getAvailAllocable() const { return m_avail_allocable; }

    //Get the type of callee-save register. This function ensures the
    //correct register type is used when saving callee-saved registers.
    virtual Type const* getCalleeRegisterType(Reg r, TypeMgr * tm) const
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    TargInfoMgr & getTIMgr() const;

    //Get the total register numbers on the target.
    UINT const getTotalRegNum() const { return getTIMgr().getNumOfRegister(); }

    //Handles the case where both conflict and consistency sets are present.
    Reg handleConflictsAndConsistency(OUT RegSet & set,
                                      PRNOConstraintsTab const& conflict_prs,
                                      PRNOConstraintsTab const& consist_prs);

    //Handles the case where only conflict set is present.
    Reg handleOnlyConflicts(OUT RegSet & set,
                            PRNOConstraintsTab const& conflict_prs);

    //Handles the case where only consistency set is present.
    //TODO:
    //  Currently, the constraints of the consistency set are not
    //  considered during the register splitting process.
    //  Future implementation needs to support this.
    Reg handleOnlyConsistency(OUT RegSet & set,
                              PRNOConstraintsTab const& consist_prs);

    //Init all register sets.
    //Note the function invoked by constructor can not be virtual.
    void initRegSet();

    //True if reg is allocable.
    bool isAvailAllocable(Reg r) const
    { return m_avail_allocable.is_contain(r); }

    //True if input reg is callee register.
    bool isCallee(Reg r) const
    { return isCalleeScalar(r) || isCalleeVector(r); }

    //True if input register is callee saved scalar register. Note that the
    //frame pointer register can be used as callee saved register if this
    //register can be allocated and the dynamic stack function has not used it.
    //Note that special handling can be done based on the architecture.
    virtual bool isCalleeScalar(Reg r) const
    {
        bool is_callee_scalar = m_target_callee_scalar != nullptr &&
            m_target_callee_scalar->is_contain(r);
        //GCOVR_EXCL_START
        bool use_fp_as_callee_scalar = isFP(r) && isFPAllocable();
        //GCOVR_EXCL_STOP
        return is_callee_scalar || use_fp_as_callee_scalar;
    }

    //True if input reg is callee vector register.
    bool isCalleeVector(Reg r) const
    {
        return m_target_callee_vector != nullptr &&
            m_target_callee_vector->is_contain(r);
    }

    //True if input reg is caller register.
    bool isCaller(Reg r) const
    { return isCallerScalar(r) || isCallerVector(r); }

    //True if reg is a caller scalar register.
    virtual bool isCallerScalar(Reg r) const
    {
        return m_target_caller_scalar != nullptr &&
            m_target_caller_scalar->is_contain(r);
    }

    //True if reg is a caller vector register.
    bool isCallerVector(Reg r) const
    {
        return m_target_caller_vector != nullptr &&
            m_target_caller_vector->is_contain(r);
    }

    //Whether current register "r" is frame pointer register.
    bool isFP(Reg r) const { return r == getTIMgr().getFP(); }

    //Whether the frame pointer register is allocable. If the debug mode is not
    //turned on, and dynamic stack, prologue/epilogue inserter function not use
    //this register, it can be used for other purposes.
    bool isFPAllocable() const;

    //True if input reg is param register.
    bool isParam(Reg r) const
    { return isParamScalar(r) || isParamVector(r); }

    //True if input reg is scalar param register.
    virtual bool isParamScalar(Reg r) const
    {
        return m_target_param_scalar != nullptr &&
            m_target_param_scalar->is_contain(r);
    }

    //True if input reg is vector param register.
    bool isParamVector(Reg r) const
    {
        return m_target_param_vector != nullptr &&
            m_target_param_vector->is_contain(r);
    }

    //GCOVR_EXCL_START
    //Return true if Type matches the register type.
    virtual bool isRegTypeMatch(Type const* ty, Reg r) const
    { ASSERTN(0, ("Target Dependent Code")); return false; }
    //GCOVR_EXCL_STOP

    //True if input reg is return value register.
    bool isReturnValue(Reg r) const
    { return isReturnValueScalar(r) || isReturnValueVector(r); }

    //True if input reg is scalar return value register.
    virtual bool isReturnValueScalar(Reg r) const
    {
        return m_target_return_value_scalar != nullptr &&
            m_target_return_value_scalar->is_contain(r);
    }

    //True if input reg is vector return value register.
    bool isReturnValueVector(Reg r) const
    {
        return m_target_return_value_vector != nullptr &&
            m_target_return_value_vector->is_contain(r);
    }

    //Special registers represent registers used by dedicate
    //that do not exist in any regset.
    bool isSpecialReg(Reg r) const;

    //True if reg is a vector register.
    bool isVector(Reg r) const
    { return isCalleeVector(r) || isCallerVector(r); }

    //Get used caller regiser regset.
    RegSet getUsedCaller() const { return m_used_caller; }

    //Get used callee regiser regset.
    RegSet getUsedCallee() const { return m_used_callee; }

    //Pick a callee register, considering lifetime constraints.
    virtual Reg pickCallee(IR const* ir, LTConstraints const* lt_constraints);

    //Pick a caller register, considering lifetime constraints.
    virtual Reg pickCaller(IR const* ir, LTConstraints const* lt_constraints);

    //Pick reg in a given regset and return it.
    virtual Reg pickReg(RegSet & set);

    //Pick reg in a given regset.
    static void pickReg(RegSet & set, Reg r);

    //Pick reg by the incramental order in a given regset, usually the
    //arg passer and epilog/prolog pick the reg by this order.
    static Reg pickRegByIncrementalOrder(RegSet & set);

    //Select the registers that meet the conditions
    //based on the current set of lifetime constraints.
    //Currently, the restrictions are divided into two sets; in other words,
    //it picks a register while considering various
    //constraints related to conflicts and consistency.
    //The function's selection process involves three main steps:
    //If both conflict and consistency sets are present,
    //it uses `handleConflictsAndConsistency()`.
    //If only the conflict set is present, it calls `handleOnlyConflicts()`.
    //If only the consistency set is present,
    //it calls `handleOnlyConsistency()`.
    Reg pickRegWithConstraints(OUT RegSet & set,
                               LTConstraints const* lt_constraints);

    virtual void pickRegisterFromAliasSet(Reg r);
    //Pick reg from all caller alias register sets.
    virtual void pickRegisterFromCallerAliasSet(Reg r);
    //Pick reg from all callee alias register sets.
    virtual void pickRegisterFromCalleeAliasSet(Reg r);
    //Pick register for all param register set.
    virtual void pickRegisterFromParamAliasSet(Reg r);
    //Pick register for all return value register set.
    virtual void pickRegisterFromReturnValueAliasSet(Reg r);

    //Pick reg from all allocable register sets.
    void pickRegFromAllocable(Reg reg);

    //Record the allocation of callee.
    void recordUsedCallee(Reg r);
    //Record the allocation of caller.
    void recordUsedCaller(Reg r);

    //This function checks the registers listed in conflict_prs to see
    //if they have been allocated physical registers. If so, it removes
    //those registers from the provided set and stores the removed
    //registers in removed_regs_wrap.
    void removeConflictingReg(OUT RegSet & set,
                              PRNOConstraintsTab const& conflict_prs,
                              OUT RegSetWrap & removed_regs_wrap);
};
//END RegSetImpl


//
//START RoundRobinRegSetImpl
//
class RoundRobinRegSetImpl : public RegSetImpl {
    COPY_CONSTRUCTOR(RoundRobinRegSetImpl);
    BSIdx m_bsidx_marker;
public:
    RoundRobinRegSetImpl(LinearScanRA & ra) : RegSetImpl(ra)
    { m_bsidx_marker = BS_UNDEF; }
    virtual ~RoundRobinRegSetImpl() {}

    //Pick up a physical register from allocable register set by the roundrobin
    //way.
    Reg pickRegRoundRobin(RegSet & set);

    //Pick reg in a given regset and return it.
    virtual Reg pickReg(RegSet & set) override;
};
//END RoundRobinRegSetImpl


//
//START LTConstraintsStrategy
//
//This class represents a strategy for imposing constraints on lifetime.
//This is a base class that different architectural strategies
//may need to inherit from, as the requirements vary.
class LTConstraintsStrategy {
    COPY_CONSTRUCTOR(LTConstraintsStrategy);
protected:
    LinearScanRA & m_ra;
public:
    LTConstraintsStrategy(LinearScanRA & ra) : m_ra(ra) {}
    virtual ~LTConstraintsStrategy() {}

    //Set the constraints for the current IR's lifetime.
    //Note that the constraints for each IR vary depending on the architecture.
    virtual void applyConstraints(IR * ir)
    { ASSERTN(0, ("Target Dependent Code")); }
};
//END LTConstraintsStrategy


//
//START LTConstraints
//
//This class represents a virtual register that requires additional constraints
//during allocation, with the specific restrictions being dependent
//on the hardware architecture.
//The class maintains two key sets of constraints:
//1. m_conflicting_prs: This table contains PRs that
//    cannot be allocated simultaneously with the current PR. If a PR is
//    listed in this set, it indicates a conflict that must be avoided to
//    maintain correct operation of the program.
//
//2. m_consistent_prs: This table includes PRs that must always be allocated
//    to the same physical register as the current PR across all uses. This
//    ensures that the same register is used consistently in all relevant
//    scenarios, preventing errors due to inconsistent allocations.
//
//If there is a need to introduce additional or more complex constraints,
//this class can be inherited to create specialized constraint management
//for specific architectures or use cases. The LTConstraints class
//represents the manifestation of lifetime constraints. The specific
//implementation of how these constraints are generated will require
//extending the LTConstraintsStrategy class, which is also a base
//class that should be inherited and expanded according to the needs
//of different architectures.
typedef xcom::TTabIter<PRNO> PRNOConstraintsTabIter;
class LTConstraints {
    COPY_CONSTRUCTOR(LTConstraints);
public:
    LTConstraints() {}
    ~LTConstraints() {}

    //Add the PR to the conflict table.
    void addConflictPR(PRNO pr) { m_conflicting_prs.append(pr); }

    //Add the PR to the consist table.
    void addConsistPR(PRNO pr) { m_consistent_prs.append(pr); }

    PRNOConstraintsTab const& getConflictTab() const
    { return m_conflicting_prs; }
    PRNOConstraintsTab const& getConsistTab() const { return m_consistent_prs; }

    bool isConflictPR(PRNO pr) const { return m_conflicting_prs.find(pr); }
    bool isConsistPR(PRNO pr) const { return m_consistent_prs.find(pr); }

    //In the process of register allocation, a PR may be split,
    //which results in changes to the PR.
    //It is necessary to update the conflicting
    //registers promptly to maintain the integrity of the allocation.
    //This function updates the set of conflicting registers
    //by replacing the old PR with the newly renamed PR,
    //ensuring that all references to the old PR are updated.
    void updateConflictPR(PRNO renamed_pr, PRNO old_pr);
protected:
    //The current PR can NOT be the same as the physical registers
    //with PRs in the conflict PR tab.
    PRNOConstraintsTab m_conflicting_prs;

    //The current PR can be the same as the physical registers
    //with PRs in the consistent PR tab.
    PRNOConstraintsTab m_consistent_prs;
};
//END LTConstraints


typedef List<LTConstraints*> LTConstraintsList;
typedef List<LTConstraints*>::Iter LTConstraintsListIter;
//
//START LTConstraintsMgr
//
//This class serves as a base class for managing lifetime constraints.
//It is designed to be overridden for different architectures,
//allowing users to implement their own architecture-specific
//LTConstraintsMgr by inheriting from this class.
class LTConstraintsMgr {
    COPY_CONSTRUCTOR(LTConstraintsMgr);
protected:
    LTConstraintsList m_ltc_list;
protected:
    void destroy();
    void init();
public:
    LTConstraintsMgr() { init(); }
    virtual ~LTConstraintsMgr() { destroy(); }

    virtual LTConstraints * allocLTConstraints();

    //Clean the lifetime constraints info before computation.
    void reset() { destroy(); init(); }
};
//END LTConstraintsMgr


typedef xcom::TMap<Type const*, Var *> Ty2Var;

//The class represents the basic structure and interface of linear-scan register
//allocation.
class LinearScanRA : public Pass {
    COPY_CONSTRUCTOR(LinearScanRA);
protected:
    bool m_is_apply_to_region;
    //Used to control the FP can be allocable or not based on the user's input.
    bool m_is_fp_allocable_allowed;
    bool m_has_alloca; //Whether alloca opeartion exists.

    //True to indicate that the stack space may need to be realigned.
    bool m_may_need_to_realign_stack;
    LifeTimeMgr * m_lt_mgr;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
    BBList * m_bb_list;
    UINT m_func_level_var_count;
    LTSet m_unhandled;
    LTSet m_handled;
    LTSet m_active;
    LTSet m_inactive;
    Vector<Reg> m_prno2reg;
    IRTab m_spill_tab;
    IRTab m_reload_tab;
    IRTab m_remat_tab;
    IRTab m_move_tab;
    ConstIRTab m_fake_use_head_tab;
    ConstIRTab m_fake_use_tail_tab;
    DedicatedMgr m_dedicated_mgr;
    PRNO2Var m_prno2var;
    ActMgr m_act_mgr;
    Vector<UINT> m_bb_seqid;
    Ty2Var m_ty2var;
    LTConstraintsMgr * m_lt_constraints_mgr;
    LTConstraintsStrategy * m_lt_constraints_strategy;
protected:
    LifeTimeMgr * allocLifeTimeMgr(Region * rg)
    { ASSERT0(rg); return new LifeTimeMgr(rg); }

    virtual RegSetImpl * allocRegSetImpl() { return new RegSetImpl(*this); }

    //Allocates an instance of the lifetime constraints strategy.
    //Derived classes can override this virtual function to provide
    //their own implementation of the allocation process.
    virtual LTConstraintsStrategy * allocLTConstraintsStrategy()
    { return new LTConstraintsStrategy(*this); }

    //Allocates an instance of the lifetime constraints manager.
    //Derived classes can override this virtual function to provide
    //their own implementation of the allocation process.
    virtual LTConstraintsMgr * allocLTConstraintsMgr()
    { return new LTConstraintsMgr(); }
public:
    explicit LinearScanRA(Region * rg);
    virtual ~LinearScanRA();

    void addUnhandled(LifeTime * lt);
    void addActive(LifeTime * lt);
    void addInActive(LifeTime * lt);
    void addHandled(LifeTime * lt);
    void assignLexSeqIdForBB();

    virtual IR * buildRemat(PRNO prno, RematCtx const& rematctx,
                            Type const* ty);
    virtual IR * buildSpill(PRNO prno, Type const* ty);
    virtual IR * buildSpillByLoc(PRNO prno, Var * spill_loc, Type const* ty);
    virtual IR * buildReload(PRNO prno, Var * spill_loc, Type const* ty);

    //The function builds PRNO with given 'type', and records the relation
    //between PRNO and 'reg' in current pass. Unlike buildPrno(), the function
    //records the generated PRNO as a dedicated PRNO which binds a dedicated
    //physical register 'reg'.
    //When TargInfo enabled, some PR operations might correspond to dedicated
    //physical register. In order to preserve this information, passes such
    //as IR simplification, calls this function.
    PRNO buildPrnoDedicated(Type const* type, Reg reg);

    //The function builds PRNO with given 'type', and records the relation
    //between PRNO and 'reg' in current pass.
    //When TargInfo enabled, some PR operations might correspond to dedicated
    //physical register. In order to preserve this information, passes such
    //as IR simplification, calls this function.
    PRNO buildPrno(Type const* type, Reg reg);

    //This func is used to get the prno assigned to special register can be
    //avoid to be spilled to improve the performance on specific target.
    //Normally, if a prno is spilled into memory, that means the value in the
    //register assigned to the prno will be used again after the split
    //position, we have to reload the value from the memory before the USE
    //position. However, on some specific architecture, the value of the
    //register is automatically kept by the hardware, it can be used as a
    //special dedicated register.
    //For example: There is a ZERO register on some architecture, the value in
    //this register is always zero, it cannot be changed.
    virtual bool canSpillAvoid(PRNO prno) const
    {
        ASSERT0(prno != PRNO_UNDEF);
        return !getLT(prno)->isOccHasDef() || getReg(prno) == getSP();
    }
    //The function checks whether the register-allocation result should be
    //applied to current region or just an estimiation of register pressure.
    //NOTE: If the register-allocation behaviors applied to current region,
    //the spilling, reloading operations and latch-BB that generated by LSRA
    //will be inserted into region's BB list and CFG.
    void checkAndApplyToRegion(MOD ApplyToRegion & apply);

    //Determine whether the PASS apply all modifications of CFG and BB to
    //current region. User may invoke LSRA as performance estimating tools
    //to conduct optimizations, such as RP, GCSE, UNROLLING which may increase
    //register pressure.
    void checkAndPrepareApplyToRegion(OUT ApplyToRegion & apply);

    //This func is used to color the stack slot and reuse the stack slot then.
    void colorStackSlot()
    {
        START_TIMER(t, "colorStackSlot");
        //Do the stack slot reuse for the scalar var.
        reuseStackSlot(false);

        //Do the stack slot reuse for the vector var.
        reuseStackSlot(true);
        END_TIMER(t, "colorStackSlot");
    }

    //The function check whether 'lt' value is simple enough to rematerialize.
    //And return the information through rematctx.
    virtual bool checkLTCanBeRematerialized(MOD LifeTime * lt,
                                            OUT RematCtx & rematctx);
    virtual void collectDedicatedPR(BBList const* bblst,
                                    OUT DedicatedMgr & mgr);

    void dumpBBListWithReg() const;

    //This func shall generate the IRs to swap the data in two registers,
    //and implement the swap by memory as the temp memory location.
    //src_prno_with_r2: the src prno responding to the second register.
    //dst_prno_with_r1: the dst prno responding to the first register.
    //src_prno_with_r1: the src prno responding to the first register.
    //dst_prno_with_r2: the dst prno responding to the second register.
    //ty1: the data type of dst_prno_with_r1.
    //ty2: the data type of dst_prno_with_r2.
    //marker: the marker IR used to indicate where to insert the generated IRs.
    //bb: the BB where IRs will be inserted.
    //retun value: this func will return the last IR in the new generated IRs,
    //             it can be used as a new marker if user wants to get the tail
    //             of new IRs.
    //
    //There are three steps used to complete the swap operation:
    //  1. [mem]  <-- spill $src_prno_with_r1
    //  2. $dst_prno_with_r1 <-- mov $src_prno_with_r2
    //  3. $dst_prno_with_r2 <-- reload [mem]
    IR * doSwapByMem(PRNO src_prno_with_r2, PRNO dst_prno_with_r1,
       PRNO src_prno_with_r1, PRNO dst_prno_with_r2, Type const* ty1,
       Type const* ty2, IR const* marker, MOD IRBB * bb);

    //This func shall generate the IRs to swap the data in two registers,
    //and implement the swap by memory as the temp registers.
    //src_prno_with_r2: the src prno responding to the second register.
    //dst_prno_with_r1: the dst prno responding to the first register.
    //src_prno_with_r1: the src prno responding to the first register.
    //dst_prno_with_r2: the dst prno responding to the second register.
    //ty1: the data type of dst_prno_with_r1.
    //ty2: the data type of dst_prno_with_r2.
    //marker: the marker IR used to indicate where to insert the generated IRs.
    //bb: the BB where IRs will be inserted.
    //retun value: this func will return the last IR in the new generated IRs,
    //             it can be used as a new marker if user wants to get the tail
    //             of new IRs.
    //
    //There are three steps used to complete the swap operation:
    //  1. $temp  <-- mov $src_prno_with_r1
    //  2. $dst_prno_with_r1 <-- mov $src_prno_with_r2
    //  3. $dst_prno_with_r2 <-- mov $temp

    IR * doSwapByReg(PRNO src_prno_with_r2, PRNO dst_prno_with_r1,
       PRNO src_prno_with_r1, PRNO dst_prno_with_r2, Type const* ty1,
       Type const* ty2, IR const* marker, MOD IRBB * bb);

    void dumpPR2Reg(PRNO prno) const;
    void dumpPR2Reg() const;
    void dump4List() const;
    bool dump(bool dumpir = true) const;

    void freeReg(Reg reg);
    void freeReg(LifeTime const* lt);

    //Generate the remat information by traversing the lifetime list.
    void genRematInfo();
    void genRematForLT(MOD LifeTime * lt) const;

    //This func is used to find the ancestor prno if an original prno is split
    //more than one time and is renamed many times.
    PRNO getAnctPrno(PRNO prno) const;

    //Get the BB that will used to insert the spill IR of callee registers.
    //Normally, it is the entry BB of the CFG.
    virtual IRBB * getCalleeSpilledBB() const
    { return m_rg->getCFG()->getEntry(); }

    //Construct a name for Var that will lived in Region.
    CHAR const* genFuncLevelNewVarName(OUT xcom::StrBuf & name);
    Var * getSpillLoc(Type const* ty);
    Var * getSpillLoc(PRNO prno);
    Var * genSpillLoc(PRNO prno, Type const* ty);
    Var * genFuncLevelVar(Type const* type, UINT align);
    PRNO2Var const& getPrno2Var() const { return m_prno2var; }
    Reg getReg(PRNO prno) const;
    REGFILE getRegFile(Reg r) const;
    Reg getReg(LifeTime const* lt) const;
    LifeTime * getLT(PRNO prno) const;
    CHAR const* getRegName(Reg r) const;
    CHAR const* getRegFileName(REGFILE rf) const;
    LTSet & getUnhandled() { return m_unhandled; }
    LTSet & getActive() { return m_active; }
    LTSet & getInActive() { return m_inactive; }
    LTSet & getHandled() { return m_handled; }
    IRTab & getSpillTab() { return m_spill_tab; }
    IRTab & getReloadTab() { return m_reload_tab; }
    IRTab & getRematTab() { return m_remat_tab; }
    IRTab & getMoveTab() { return m_move_tab; }
    BBList * getBBList() const { return m_bb_list; }
    IRCFG * getCFG() const { return m_cfg; }
    TargInfoMgr & getTIMgr() const
    { return *(m_rg->getRegionMgr()->getTargInfoMgr()); }
    LifeTimeMgr & getLTMgr() { return *m_lt_mgr; }
    LTConstraintsMgr * getLTConstraintsMgr()
    { return m_lt_constraints_mgr; }
    Reg getDedicatedReg(LifeTime const* lt) const
    { return getDedicatedReg(lt->getPrno()); }
    Reg getDedicatedReg(PRNO prno) const { return m_dedicated_mgr.get(prno); }
    DedicatedMgr & getDedicatedMgr() { return m_dedicated_mgr; }
    ActMgr & getActMgr() { return m_act_mgr; }
    virtual CHAR const* getPassName() const
    { return "Linear Scan Register Allocation"; }
    PASS_TYPE getPassType() const { return PASS_LINEAR_SCAN_RA; }

    //This function mainly prepares the correct register type for a prno,
    //not the data type of lt.
    Type const* getVarTypeOfPRNO(PRNO prno) const;

    //This function returns the type when do the spill operation for a prno.
    //This function can be overidden by the derived class if the required
    //spill type is not the original type of the prno.
    //Prno: the input prno.
    virtual Type const* getSpillType(PRNO prno) const
    { return getVarTypeOfPRNO(prno); }

    //This function returns the actual type for the input type.
    //This function can be overidden by the derived class if the required
    //spill type is not the same as the original input type.
    //ty: the input type.
    virtual Type const* getSpillType(Type const* ty) const
    { ASSERT0(ty); return ty; }

    //Get target physical registers.
    //Get base pointer register.
    Reg getBP() const { return getTIMgr().getBP(); }

    //Get end caller saved scalar register.
    Reg getCallerScalarEnd() const { return getTIMgr().getCallerScalarEnd(); }

    //Get start caller saved scalar register.
    Reg getCallerScalarStart() const
    { return getTIMgr().getCallerScalarStart(); }

    //Get frame pointer register.
    Reg getFP() const { return getTIMgr().getFP(); }

    //Get global pointer register.
    Reg getGP() const { return getTIMgr().getGP(); }

    //Get start parameter scalar register.
    xgen::Reg getParamScalarStart() const
    { return getTIMgr().getParamScalarStart(); }

    //Get program counter register.
    Reg getPC() const { return getTIMgr().getPC(); }

    //Get returned address register.
    Reg getRA() const { return getTIMgr().getRA(); }

    //Get stack pointer register.
    Reg getSP() const { return getTIMgr().getSP(); }

    //Get target address register.
    Reg getTA() const { return getTIMgr().getTA(); }

    //Get program counter register.
    virtual xgen::Reg getTargetPC() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Used to get the special dedicated prno per the specific arch.
    virtual PRNO getSpecialDedicatedPrno(Type const* type, Reg reg) const
    { ASSERTN(0, ("Target Dependent Code")); return PRNO_UNDEF; }

    //The temporary register is a reserved register that used to save a
    //temporary value, which is usually used after the register allocation
    //and does not be assigned in the register allocation.
    Reg getTempReg(Type const* ty) const
    { return ty->is_vector() ? getTempVector() : getTempScalar(ty); }
    Reg getTempScalar(Type const* ty) const
    { return getTIMgr().getTempScalar(ty); }
    Reg getTempVector() const { return getTIMgr().getTempVector(); }

    //Get a temp memory location per the specified type.
    //ty: the input type.
    Var * getTempVar(Type const* ty)
    { ASSERT0(ty);  return getSpillLoc(ty); }

    //Get zero register.
    Reg getZero() const { return getTIMgr().getZero(); }

    bool hasReg(PRNO prno) const;
    bool hasReg(LifeTime const* lt) const;

    //This func shall generate the IRs to swap the data in two registers, if
    //there is a TMP register reserved on the specific architecture, the
    //register will be used as the temp space to finish the swap, or else,
    //memory location on stack will be adopted to help to complete the data
    //exchange.
    //src_prno_with_r2: the src prno responding to the second register.
    //dst_prno_with_r1: the dst prno responding to the first register.
    //src_prno_with_r1: the src prno responding to the first register.
    //dst_prno_with_r2: the dst prno responding to the second register.
    //ty1: the data type of dst_prno_with_r1.
    //ty2: the data type of dst_prno_with_r2.
    //marker: the marker IR used to indicate where to insert the generated IRs.
    //bb: the BB where IRs will be inserted.
    //retun value: this func will return the last IR in the new generated IRs,
    //             it can be used as a new marker if user wants to get the tail
    //             of new IRs.
    //e.g:
    //Original IRs with cyclic MOVs:
    //    $1(R0) <- $10(R1)
    //    $2(R1) <- $20(R2)
    //    $3(R2) <- $30(R0)
    //
    //These three IRs are abstracted with the concrete prnos with variable
    //names:
    //    $dst_prno_with_r0 <- $src_prno_with_r1
    //    $dst_prno_with_r1 <- $src_prno_with_r2
    //    $dst_prno_with_r2 <- $src_prno_with_r0
    //
    //There will be two groups of swap per this algorithm:
    //    Group 1: r = R0, r1 = R1, r2 = R2, and do swap for R1 and R2.
    //        temp <- $src_prno_with_r1               |  temp <- $10(R1)
    //        $dst_prno_with_r1 <- $src_prno_with_r2  |  $2(R1) <- $20(R2)
    //        $dst_prno_with_r2 <- temp               |  $3(R2) <- temp
    //    After the first group swap, R1 contains the final correct data,
    //    R2 contains the data of R1.
    //
    //    Group 2: r = R0, swap r1 = R2, r2 = R0, and do swap for R2 and R0.
    //        temp <- $src_prno_with_r2               |  temp <- $3(R2)
    //        $dst_prno_with_r1 <- $src_prno_with_r2  |  $3(R2) <- $30(R0)
    //        $dst_prno_with_r2 <- temp               |  $1(R0) <- temp
    virtual IR * insertIRToSwap(PRNO src_prno_with_r2, PRNO dst_prno_with_r1,
        PRNO src_prno_with_r1, PRNO dst_prno_with_r2, Type const* ty1,
        Type const* ty2, IR const* marker, MOD IRBB * bb);

    //Return true if register r1 alias to r2.
    virtual bool isAlias(Reg r1, Reg r2) const { return r1 == r2; }
    //Determine an edge is backward jump or not.
    bool isBackwardJump(UINT src_bbid, UINT dst_bbid) const
    { return m_bb_seqid[dst_bbid] <= m_bb_seqid[src_bbid]; }
    virtual bool isCalleePermitted(LifeTime const* lt) const;
    bool isDedicated(PRNO prno) const
    { return m_dedicated_mgr.isDedicated(prno); }

    //Check the Frame Pointer Register can be allocable or not.
    bool isFPAllocableAllowed() const { return m_is_fp_allocable_allowed; }

    //Return true if the register-allocation result should be applied to
    //current region's BB list and CFG.
    bool isApplyToRegion() const { return m_is_apply_to_region; }

    //Return true if a fake-use IR at the first BB of loop by lexicographical
    //order.
    //e.g: "[mem]<-fake_use $1" is a fake-use IR at the first BB of loop by
    //      lexicographical order.
    //       ----->BB1
    //      |      [mem]<-fake_use $1
    //      |      ...
    //      |      |
    //      |      V
    //      |      BB2
    //      |      |
    //      |      V
    //      |      BB3
    //      |      |
    //      |      V
    //      |      BB4
    //      |      |
    //      '------|
    //             V
    bool isFakeUseAtLexFirstBBInLoop(IR const* ir) const
    { return m_fake_use_head_tab.find(ir); }

    //Return true if a fake-use IR at the last BB of loop by lexicographical
    //order.
    //e.g: "[mem]<-fake_use $1" is a fake-use IR at the last BB of loop by
    //      lexicographical order.
    //       ----->BB1
    //      |      |
    //      |      V
    //      |      BB2
    //      |      |
    //      |      V
    //      |      BB3
    //      |      |
    //      |      V
    //      |      BB4
    //      |      ...
    //      |      [mem]<-fake_use $1
    //      |      |
    //       ------|
    //             V
    bool isFakeUseAtLexLastBBInLoop(IR const* ir) const
    { return m_fake_use_tail_tab.find(ir); }

    bool isInsertOp() const
    {
        LinearScanRA * pthis = const_cast<LinearScanRA*>(this);
        return pthis->getSpillTab().get_elem_count() != 0 ||
               pthis->getReloadTab().get_elem_count() != 0 ||
               pthis->getMoveTab().get_elem_count() != 0;
    }

    bool isMoveOp(IR const* ir) const
    { return m_move_tab.find(const_cast<IR*>(ir)); }

    bool isPrnoAlias(PRNO prno, PRNO alias) const;
    //Return true if ir is rematerializing operation.
    virtual bool isRematLikeOp(IR const* ir) const
    {
        if (!ir->is_stpr()) { return false; }
        if (!ir->getRHS()->is_lda() || !ir->getRHS()->is_const()) {
            return false;
        }
        return true;
    }
    bool isReloadOp(IR const* ir) const
    { return m_reload_tab.find(const_cast<IR*>(ir)); }

    bool isRematOp(IR const* ir) const
    { return m_remat_tab.find(const_cast<IR*>(ir)); }

    bool isSpillOp(IR const* ir) const
    { return m_spill_tab.find(const_cast<IR*>(ir)); }

    //This function returns true if the full width of the register will be
    //spilled into memory. Or else, returns false if the partial of register
    //will be spilled into memory. It can be overidden by the derived class
    //on the specific architecture.
    virtual bool isSpillFullReg() const { return false; }

    //This func is used to check the TMP register is available or not for the
    //input type.
    virtual bool isTmpRegAvailable(Type const* ty) const
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    //Initialize the allocation strategy for the lifetime constraint set.
    //Note that different architectures have varying strategies.
    void initConstraintsStrategy()
    {
        if (m_lt_constraints_strategy != nullptr) { return; }
        m_lt_constraints_strategy = allocLTConstraintsStrategy();
    }

    //Initializes the lifetime constraint management unit.
    //Note that different architectures have different constraints.
    void initLTConstraintsMgr()
    {
        if (m_lt_constraints_mgr != nullptr) { return; }
        m_lt_constraints_mgr = allocLTConstraintsMgr();
    }

    //Whether the stack space does not need to be realigned.
    bool noNeedToAlignStack() const { return !m_may_need_to_realign_stack; }

    //Whether the dynamic stack function has been performed and used frame
    //pointer register before.
    bool notHaveAlloca() const { return !m_has_alloca; }

    virtual bool perform(OptCtx & oc);
    virtual bool performLsraImpl(OptCtx & oc);

    //Used to record the special dedicated prno per the specific arch.
    virtual void recordSpecialDedicatedPrno(Type const* type, Reg reg,
                                            PRNO prno)
    { ASSERTN(0, ("Target Dependent Code")); return; }

    void recalculateSSA(OptCtx & oc) const;

    //Reset all resource before allocation.
    void reset();
    //Do the stack slot resue per the input var type to save the stack
    //space after the LSRA.
    //is_vector: the type of the stack slot. True means do the stack slot
    //           reuse for vector var; false means do the stack slot
    //           reuse for the scalar var.
    void reuseStackSlot(bool is_vector);

    void setApplyToRegion(bool doit) { m_is_apply_to_region = doit; }

    //Set attributes obtained from other passes.
    void setAttr();
    void setDedicatedReg(PRNO prno, Reg r) { m_dedicated_mgr.add(prno, r); }

    //Normally m_is_fp_allocable_allowed can be set to false only, because the
    //default value is true, it will be set to false if we find the fp is used
    //to implement the alloca on stack, debug option or special register, and
    //it is not expected to be allocable to other PRs in register allocation. In
    //other situations, it should be participated to the register allocation as
    //the regular registers.
    void setFPAllocableAllowed(bool allowed)
    { m_is_fp_allocable_allowed = allowed; }
    void setFakeUseAtLexFirstBBInLoop(IR * ir)
    { m_fake_use_head_tab.append(ir); }
    void setFakeUseAtLexLastBBInLoop(IR * ir)
    { m_fake_use_tail_tab.append(ir); }
    void setMove(IR * ir) { m_move_tab.append(ir); }
    void setReg(PRNO prno, Reg reg);
    void setReload(IR * ir) { m_reload_tab.append(ir); }
    void setRemat(IR * ir) { m_remat_tab.append(ir); }
    void setSpill(IR * ir) { m_spill_tab.append(ir); }

    //Set the constraint set for each IR.
    void scanIRAndSetConstraints();

    //After the lifetime calculation is completed, begin setting constraint
    //sets for each lifetime.
    void tryComputeConstraints();

    bool verify4List() const;
    bool verifyAfterRA() const;
};

} //namespace xoc
#endif
