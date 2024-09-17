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

//
//START BackwardEdge
//
#define BKEDGE_srcbb(b) ((b)->srcbb)
#define BKEDGE_dstbb(b) ((b)->dstbb)
class BackwardEdge {
public:
    IRBB const* srcbb;
    IRBB const* dstbb;

    BackwardEdge(IRBB const* src, IRBB const* dst) : srcbb(src), dstbb(dst) {}
    void init(IRBB const* src, IRBB const* dst)
    {
        ASSERT0(src && dst);
        srcbb = src;
        dstbb = dst;
    }
};
//END BackwardEdge

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
    BackwardEdge * genBackwardEdge(IRBB const* srcbb, IRBB const* dstbb);

    //Generate a Occurence object and append the pointer in the m_occ_list.
    Occurence * genOccurence();

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

//Used to store the PRLiveSet related to the backward jump BBs.
typedef List<PRLiveSet const*> PRLiveSetList;

//Used to store the backward jump edges.
typedef List<BackwardEdge const*> BackwardEdgeList;

//
//START BackwardJumpAnalysis
//
//BackwardJumpAnalysis is used to check the CFG whether have the backward
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
class BackwardJumpAnalysis {
    COPY_CONSTRUCTOR(BackwardJumpAnalysis);
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
    BackwardJumpAnalysisResMgr * m_resource_mgr;

public:
    BackwardJumpAnalysis(Region * rg, BBPos2Attr * pos2attr,
                         FakeVarMgr * fake_var_mgr, LinearScanRA * lsra);
    virtual ~BackwardJumpAnalysis();

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
        BackwardEdge const* e = m_resource_mgr->genBackwardEdge(srcbb, dstbb);
        m_backward_edges.append_tail(e);
    }

    //Check the condition for fake-use first, and then do the insertion.
    void checkAndInsertFakeUse();

    //Collect all the backward jumps in the control flow graph.
    void collectBackwardJumps();

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

    //This func inserts the fake-use IR at the head or tail of input BB.
    void insertFakeUse(IRBB const* bb, PRNO prno, INSERT_MODE mode);

    //This func inserts the fake-use IR at the head of input BB.
    void insertFakeUseAtBBEntry(IRBB const* bb, PRNO prno, BBPos const& pos);

    //This func inserts the fake-use IR at the tail of input BB.
    void insertFakeUseAtBBExit(IRBB const* bb, PRNO prno, BBPos const& pos);

    //This func records the occurence of the input prno at the specified pos.
    void recordOccurenceForPR(PRNO prno, Pos pos);
};
//END BackwardJumpAnalysis


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
    void initAvailRegSet();

    void destroyRegSet();
    void destroyDebugRegSet();
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

    TargInfoMgr & getTIMgr() const;

    //Get the total register numbers on the target.
    UINT const getTotalRegNum() const { return getTIMgr().getNumOfRegister(); }

    //Init all register sets.
    //Note the function invoked by constructor can not be virtual.
    void initRegSet();

    //True if reg is allocable.
    bool isAvailAllocable(Reg r) const
    { return m_avail_allocable.is_contain(r); }

    //True if input reg is callee register.
    bool isCallee(Reg r) const;

    //True if input reg is caller register.
    bool isCaller(Reg r) const;

    //True if input reg is param register.
    bool isParam(Reg r) const;

    //True if input reg is return value register.
    bool isReturnValue(Reg r) const;

    //Special registers represent registers used by dedicate
    //that do not exist in any regset.
    bool isSpecialReg(Reg r) const;

    //Get used caller regiser regset.
    RegSet getUsedCaller() const { return m_used_caller; }
    //Get used callee regiser regset.
    RegSet getUsedCallee() const { return m_used_callee; }
    //Pick callee register.
    virtual Reg pickCallee(IR const* ir);
    //Pick caller register.
    virtual Reg pickCaller(IR const* ir);

    //Pick reg in a given regset and return it.
    static Reg pickReg(RegSet & set);

    //Pick reg in a given regset.
    static void pickReg(RegSet & set, Reg r);

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
};

typedef xcom::TMap<Type const*, Var *> Ty2Var;

//The class represents the basic structure and interface of linear-scan register
//allocation.
class LinearScanRA : public Pass {
    COPY_CONSTRUCTOR(LinearScanRA);
protected:
    bool m_is_apply_to_region;
    //Used to control the FP can be allocable or not based on the user's input.
    bool m_is_fp_allocable_allowed;
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
    IRTab m_fake_use_tab;
    DedicatedMgr m_dedicated_mgr;
    PRNO2Var m_prno2var;
    ActMgr m_act_mgr;
    Vector<UINT> m_bb_seqid;
    xcom::TTab<PRNO> m_prno_with_2d_hole;
    Ty2Var m_ty2var;
protected:
    LifeTimeMgr * allocLifeTimeMgr(Region * rg)
    { ASSERT0(rg); return new LifeTimeMgr(rg); }

    virtual RegSetImpl * allocRegSetImpl() { return new RegSetImpl(*this); }

public:
    explicit LinearScanRA(Region * rg);
    virtual ~LinearScanRA();

    void addUnhandled(LifeTime * lt);
    void addActive(LifeTime * lt);
    void addInActive(LifeTime * lt);
    void addHandled(LifeTime * lt);
    void assignTopoIdForBB();

    virtual IR * buildRemat(PRNO prno, RematCtx const& rematctx,
                            Type const* ty);
    virtual IR * buildMove(PRNO from, PRNO to, Type const* fromty,
                           Type const* toty);
    virtual IR * buildSpill(PRNO prno, Type const* ty);
    virtual IR * buildSpillByLoc(PRNO prno, Var * spill_loc, Type const* ty);
    virtual IR * buildReload(PRNO prno, Var * spill_loc, Type const* ty);

    //Should be called before register allocation.
    PRNO buildPrnoDedicated(Type const* type, Reg reg);
    //Should be called after register allocation.
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
        return !getLT(prno)->isOccHasDef();
    }

    void addPrnoWith2dLTHole(PRNO prno) { m_prno_with_2d_hole.append(prno); }

    //The function check whether 'lt' value is simple enough to rematerialize.
    //And return the information through rematctx.
    virtual bool checkLTCanBeRematerialized(LifeTime const* lt,
                                            OUT RematCtx & rematctx);
    virtual void collectDedicatedPR(BBList const* bblst,
                                    OUT DedicatedMgr & mgr);

    void dumpPR2Reg(PRNO prno) const;
    void dumpPR2Reg() const;
    void dump4List() const;
    bool dump(bool dumpir = true) const;

    void freeReg(Reg reg);
    void freeReg(LifeTime const* lt);

    //This func is used to find the ancestor prno if an original prno is split
    //more than one time and is renamed many times.
    PRNO getAnctPrno(PRNO prno) const;

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
    Reg getDedicatedReg(LifeTime const* lt) const
    { return getDedicatedReg(lt->getPrno()); }
    Reg getDedicatedReg(PRNO prno) const { return m_dedicated_mgr.get(prno); }
    DedicatedMgr & getDedicatedMgr() { return m_dedicated_mgr; }
    ActMgr & getActMgr() { return m_act_mgr; }
    virtual CHAR const* getPassName() const
    { return "Linear Scan Register Allocation"; }
    PASS_TYPE getPassType() const { return PASS_LINEAR_SCAN_RA; }

    //This function mainly prepares the correct register type for spill,
    //not the data type of lt.
    Type const* getRegType(PRNO prno) const;

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
    Reg getTemp() const { return getTIMgr().getTemp(); }

    //Get zero register.
    Reg getZero() const { return getTIMgr().getZero(); }

    bool hasReg(PRNO prno) const;
    bool hasReg(LifeTime const* lt) const;

    //Return true if register r1 alias to r2.
    virtual bool isAlias(Reg r1, Reg r2) const { return r1 == r2; }
    //Determine an edge is backward jump or not.
    bool isBackwardJump(UINT src_bbid, UINT dst_bbid) const
    { return m_bb_seqid[dst_bbid] <= m_bb_seqid[src_bbid]; }
    virtual bool isCalleePermitted(LifeTime const* lt) const;
    bool isDedicated(PRNO prno) const
    { return m_dedicated_mgr.is_dedicated(prno); }

    //Check the Frame Pointer Register can be allocable or not.
    bool isFPAllocableAllowed() const { return m_is_fp_allocable_allowed; }

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

    bool isFakeUseOp(IR const* ir) const
    { return m_fake_use_tab.find(const_cast<IR*>(ir)); }

    //This func is used to check the TMP register is available or not for the
    //input type.
    virtual bool isTmpRegAvailable(Type const* ty) const
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    bool isUnsplitable(PRNO prno) const
    {
        //Currently the prnos with 2D lifetime holes are not allowed to
        //split during the LSRA, because it will lead to incorrect spill/reload
        //operation.
        return m_prno_with_2d_hole.find(prno);
    }


    virtual bool perform(OptCtx & oc);
    virtual bool performLsraImpl(OptCtx & oc);

    //Used to record the special dedicated prno per the specific arch.
    virtual void recordSpecialDedicatedPrno(Type const* type, Reg reg,
                                            PRNO prno)
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //Reset all resource before allocation.
    void reset();

    void setApplyToRegion(bool doit) { m_is_apply_to_region = doit; }
    void setDedicatedReg(PRNO prno, Reg r) { m_dedicated_mgr.add(prno, r); }

    //Normally m_is_fp_allocable_allowed can be set to false only, because the
    //default value is true, it will be set to false if we find the fp is used
    //to implement the alloca on stack, debug option or special register, and
    //it is not expected to be allocable to other PRs in register allocation. In
    //other situations, it should be participated to the register allocation as
    //the regular registers.
    void setFPAllocableAllowed(bool allowed)
    { m_is_fp_allocable_allowed = allowed; }
    void setFakeUse(IR * ir) { m_fake_use_tab.append(ir); }
    void setMove(IR * ir) { m_move_tab.append(ir); }
    void setReg(PRNO prno, Reg reg);
    void setReload(IR * ir) { m_reload_tab.append(ir); }
    void setRemat(IR * ir) { m_remat_tab.append(ir); }
    void setSpill(IR * ir) { m_spill_tab.append(ir); }

    void updateSSA(OptCtx & oc) const;

    bool verify4List() const;
    bool verifyAfterRA() const;
};

} //namespace xoc
#endif
