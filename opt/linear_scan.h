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

#define RA_NORMAL_DEBUG 100
#define RA_GROUP_DEBUG 101

namespace xoc {

class LifeTime;
class LifeTimeMgr;
class TargInfoMgr;
class ArgPasser;
class ApplyToRegion;
class PreferRegSetMgr;

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
    {}
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
#define OCC_ty(r) ((r)->ty)
class Occurence {
public:
    Pos first;
    Pos last;
    Type const* ty;

    Occurence() : first(POS_UNDEF), last(POS_UNDEF), ty(nullptr) {}
};
//END Occurence

//The attribute flag responding to a POS.
typedef enum {
    //Undefined value.
    POS_ATTR_UNDEF = 0x0,

    //Indicate this IR should not generate the final instruction.
    //If this attribute is set, the IR responding to this POS should be deleted
    //after the register assignment.
    POS_ATTR_NO_CODE_GEN = 0x1,
} POS_ATTR_FLAG;

//This enum is used to describe the lexical sequence of BB for the fake-use IRs
//related to the same PR.
typedef enum {
    BB_SEQ_UNDEF = 0,

    //The fake-use IR in this BB is the first in lexical sequence.
    BB_SEQ_FIRST,
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
    // LABEL: X
    // -- BB start --
    //                      <-- Append fake IR here.
    // add $2:u32
    // sub $3:u32
    // mul $4:u32
    // goto label L
    // -- BB end --
    INSERT_MODE_HEAD = 1,

    //Indicate the fake-use IR should be appended at the end of BB but before
    //the branch IR.
    // eg.
    // LABEL: X
    // -- BB start --
    // add $2:u32
    // sub $3:u32
    // mul $4:u32
    //                      <-- Append fake IR here.
    //  goto label L
    // -- BB end --
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
#define FAKEUSE_ty(r) ((r)->ty)
class FakeUse {
public:
    IRBB const* bb;
    Pos pos;
    Type const* ty;
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
//Start GenVarOfMap
//
class GenVarOfMap {
    COPY_CONSTRUCTOR(GenVarOfMap);
    Region * m_rg;
public:
    GenVarOfMap() {}
    ~GenVarOfMap() {}

    Var * createMapped(Type const* ty)
    {
        ASSERT0(ty);
        Var * v = nullptr;
        if (ty->is_vector()) {
            v = m_rg->getVarMgr()->registerVar(
                "#fake_vec_var", ty, 1, VAR_LOCAL | VAR_FAKE, SS_UNDEF);
            return v;
        }
        v = m_rg->getVarMgr()->registerVar(
            "#fake_scalar_var", ty, 1, VAR_LOCAL | VAR_FAKE, SS_UNDEF);
        return v;
    }

    void setRegion(Region * rg) { ASSERT0(rg); m_rg = rg; }
};
//End GenVarOfMap.


//
//START FakeVarMgr
//
class FakeVarMgr {
    COPY_CONSTRUCTOR(FakeVarMgr);
    typedef xcom::TMap<Type const*, Var*,
        CompareKeyBase<Type const*>, GenVarOfMap> Type2Var;
protected:
    Region * m_rg;
    Type2Var m_type2fakevar;
public:
    FakeVarMgr(Region * rg) : m_rg(rg)
    {
        GenVarOfMap & gm = m_type2fakevar.getGenMapped();
        gm.setRegion(rg);
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
protected:
    Region * m_rg;
    BBPos2Attr * m_pos2attr;
    IRMgr * m_irmgr;
    BBList * m_bb_list;
    LivenessMgr * m_live_mgr;
    RegAllocMgr * m_ramgr;
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
        LexBackwardJump const* e = m_resource_mgr->genLexBackwardJump(
            srcbb, dstbb);
        m_backward_edges.append_tail(e);
    }

    //Assign each BB a lexical sequence ID.
    void assignLexSeqIdForBB(OUT Vector<UINT> & bb_seqid);

    //Collect all the backward jumps in the control flow graph.
    void collectBackwardJumps(Vector<UINT> const& bb_seqid);

    //Return true if 'prno' has been pre-assigned a physical-register, and
    //we can neglect the affect of the special 'prno' during
    //FakeUse generation.
    bool canSkipPreAssignedReg(PRNO prno) const;

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
    void insertFakeUse(IRBB const* bb, PRNO prno, Type const* ty,
        INSERT_MODE mode);

    //This func inserts the fake-use IR at the head of input BB.
    void insertFakeUseAtBBEntry(IRBB const* bb, PRNO prno, Type const* ty,
        BBPos const& pos);

    //This func inserts the fake-use IR at the tail of input BB.
    void insertFakeUseAtBBExit(IRBB const* bb, PRNO prno, Type const* ty,
        BBPos const& pos);

    //Record the fake-use information.
    void recordFakeUse(PRNO prno, IRBB const* bb, Type const* ty,
        INSERT_MODE mode);

    //This func records the occurence of the input prno at the specified pos.
    void recordOccurenceForPR(PRNO prno, Pos pos, Type const* ty);
};
//END LexBackwardJumpAnalysis


//
//START FakeIRMgr
//
class FakeIRMgr {
    COPY_CONSTRUCTOR(FakeIRMgr);
    Region * m_rg;
    LinearScanRA * m_lsra;
    LexBackwardJumpAnalysis * m_back_jump_ana;
    FakeVarMgr m_fake_var_mgr;
    BBPos2Attr m_pos2attr;
public:
    FakeIRMgr(Region * rg, LinearScanRA * lsra) : m_rg(rg), m_lsra(lsra),
        m_fake_var_mgr(rg)
    {
        m_back_jump_ana = new LexBackwardJumpAnalysis(m_rg,
            &m_pos2attr, &m_fake_var_mgr, lsra);
    }
    ~FakeIRMgr()
    {
        if (m_back_jump_ana) { delete m_back_jump_ana; }
    }

    //Do the lexicographic backward jump analysis.
    void doBackwardJumpAnalysis() { m_back_jump_ana->analyze(); }

    //Removed the fake-use IR inserted by the backward jump analysis.
    void removeFakeUseIR(OptCtx & oc);
};
//END FakeIRMgr


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
    virtual void applyConstraints(IR *)
    { ASSERTN(0, ("Target Dependent Code")); }
};
//END LTConstraintsStrategy


typedef xcom::TTabIter<PRNO> PRNOConstraintsTabIter;

//Record the type of available register set selected by the
//prno during register allocation.
typedef xcom::TMap<PRNO, Type const*> TypeConstraintsTab;
typedef xcom::TMapIter<PRNO, Type const*> TypeConstraintsTabIter;


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
class LTConstraints {
    COPY_CONSTRUCTOR(LTConstraints);
public:
    LTConstraints() {}
    ~LTConstraints() { m_type_constrains_of_prs.destroy(); }

    //Add the PR to the conflict table.
    void addConflictPR(PRNO pr) { m_conflicting_prs.append(pr); }

    //Add the PR to the consist table.
    void addConsistPR(PRNO pr) { m_consistent_prs.append(pr); }

    //Add a type corresponding to PR.
    void addTypeConstraintsPR(PRNO pr, Type const* ty)
    {
        ASSERT0((pr != PRNO_UNDEF) && ty);
        m_type_constrains_of_prs.setAlways(pr, ty);
    }

    void clone(LTConstraints const& other);

    PRNOConstraintsTab const& getConflictTab() const
    { return m_conflicting_prs; }
    PRNOConstraintsTab const& getConsistentTab() const
    { return m_consistent_prs; }
    TypeConstraintsTab const& getTypeConstraintsTab() const
    { return m_type_constrains_of_prs; }
    Type const* getTypeWithPR(PRNO pr) const
    { ASSERT0(pr != PRNO_UNDEF); return m_type_constrains_of_prs.get(pr); }

    bool isConflictPR(PRNO pr) const { return m_conflicting_prs.find(pr); }
    bool isConsistPR(PRNO pr) const { return m_consistent_prs.find(pr); }

    //In the process of register allocation, a PR may be split,
    //which results in changes to the PR.
    //It is necessary to update the conflicting
    //registers promptly to maintain the integrity of the allocation.
    //This function renames the set of conflicting registers
    //by replacing the old PR with the newly renamed PR,
    //ensuring that all references to the old PR are updated.
    void renameConflictPR(PRNO renamed_pr, PRNO old_pr);

    //This function renames the set of consistent registers
    //by replacing the old PR with the newly renamed PR,
    //ensuring that all references to the old PR are updated.
    void renameConsistentPR(PRNO renamed_pr, PRNO old_pr);

    //This function renames the set of type consraints registers.
    void renameTypeConstraintsPR(PRNO renamed_pr, PRNO old_pr);
protected:
    //The current PR can NOT be the same as the physical registers
    //with PRs in the conflict PR tab.
    PRNOConstraintsTab m_conflicting_prs;

    //The current PR can be the same as the physical registers
    //with PRs in the consistent PR tab.
    PRNOConstraintsTab m_consistent_prs;

    //The current PR can only use specific types to select
    //available sets of physical registers.
    //By default, the available set of physical registers is selected
    //based on the type of IR, but in some cases, a more specific
    //type needs to be specified.
    //As shown below, the group multiplication coefficient of $4 is m1,
    //but the group multiplication coefficient of $6 is m8, and both $4 and $6
    //must be allocated from the register set with a group multiplication
    //coefficient of m8.
    //
    //  For lmul_ext_trunc operation:
    //    stpr $6:vec<i16m8*1024>
    //      cvt:vec<i16m8*1024>
    //        $4:vec<i16m1*128>
    TypeConstraintsTab m_type_constrains_of_prs;
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


//
//START RegLTBuilder
//
class RegLTBuilder {
    COPY_CONSTRUCTOR(RegLTBuilder);
    Region * m_rg;
    RegAllocMgr * m_ramgr;
    //This life time manger is used to store the register lifetime, which can
    //be used to get the top view usage of the all phisical registers,
    //which will be used in the spill reload promotion algorithm to find
    //whether there is a proper register to eliminate the spill/reload in
    //a certain live range.
    RegLifeTimeMgr m_reg_lt_mgr;
    bool canSkipMerge(LifeTime const* lt) const;

    //Merge the new lifetime 'lt_new' into the lifetime responding to the
    //physical-register assigned.
    //lt_new: the lifetime of psesudo-register that will be merged into the
    //        lifetime of physical-register.
    //
    //e.g:
    //  The lifetime of PR1 will be merged into the lifetime of REG1.
    //  Before Merge:
    //  LT:REG_1,range:<22-23><24-27><30><35-43>
    //   |                     ------  -   ----------
    //
    //  LT:PR_1,range:<46-49>
    //   |                     ------  -   ----------  ----
    //
    //  After Merge:
    //  LT:REG_1,range:<22-23><24-27><30><35-43><46-49>
    //   |                     ------  -   ----------  ----
    void mergeRegLifeTimeWithPRLT(MOD LifeTime * lt_new);
public:
    RegLTBuilder(Region * rg) : m_rg(rg), m_reg_lt_mgr(m_rg)
    {
        m_ramgr = (RegAllocMgr*)rg->getPassMgr()->registerPass(
            PASS_REGALLOC_MGR);
    }

    void dumpRegLTOverview() const;
    void dumpReg2LT(Pos start, Pos end, bool open_range = false) const;

    //Save the map of reg and it's corresponded lifetime.
    void generateRegLifeTime(OptCtx & oc);

    RegLifeTimeMgr & getRegLTMgr() { return m_reg_lt_mgr; }
};
//END RegLTBuilder

typedef xcom::TMap<Type const*, Var *> Ty2Var;


//
//START RegAlloc
//
//The class represents the basic structure and interface of register allocation.
class RegAlloc : public Pass {
    COPY_CONSTRUCTOR(RegAlloc);
protected:
    bool m_is_apply_to_region;
    RegAllocMgr * m_ramgr;
    TargInfoMgr const* m_timgr;
    ActMgr m_act_mgr;
public:
    RegAlloc(Region * rg) : Pass(rg), m_act_mgr(rg)
    {
        m_is_apply_to_region = false;
        m_ramgr = (RegAllocMgr*)rg->getPassMgr()->registerPass(
            PASS_REGALLOC_MGR);
        m_timgr = rg->getRegionMgr()->getTargInfoMgr();
        ASSERT0(m_timgr);
    }
    virtual ~RegAlloc() {}

    bool hasReg(LifeTime const* lt) const;
    ActMgr & getActMgr() { return m_act_mgr; }
    virtual CHAR const* getPassName() const { return "Register Allocation"; }
    RegAllocMgr * getRegAllocMgr() { return m_ramgr; }

    TargInfoMgr & getTIMgr() const
    { return *(m_rg->getRegionMgr()->getTargInfoMgr()); }

    //Return true if the register-allocation result should be applied to
    //current region's BB list and CFG.
    bool isApplyToRegion() const { return m_is_apply_to_region; }
    void setApplyToRegion(bool doit) { m_is_apply_to_region = doit; }
};
//END RegAlloc


//
//START LinearScanRA
//
//The class represents the basic structure and interface of linear-scan register
//allocation.
class LinearScanRA : public RegAlloc {
    COPY_CONSTRUCTOR(LinearScanRA);
    typedef xcom::Vector<LifeTime const*> Reg2LT;
protected:
    LifeTimeMgr * m_lt_mgr;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
    BBList * m_bb_list;
    LTConstraintsMgr * m_lt_constraints_mgr;
    PreferRegSetMgr * m_prefer_regset_mgr;
    LTConstraintsStrategy * m_lt_constraints_strategy;
    FakeIRMgr * m_fake_irmgr;
    UINT m_func_level_var_count;
    LTSet m_unhandled;
    LTSet m_handled;
    LTSet m_active;
    LTSet m_inactive;
    ConstIRTab m_fake_use_head_tab;
    ConstIRTab m_fake_use_tail_tab;

    //Map the physical register to the lifetime during the allocation process.
    Reg2LT m_reg2lt;

    Ty2Var m_ty2var;
protected:
    FakeIRMgr * allocFakeIRMgr() { return new FakeIRMgr(m_rg, this); }
    virtual RegSetImpl * allocRegSetImpl() { return new RegSetImpl(m_rg); }

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

    //Allocate and return a new PreferRegSetMgr instance. This is a base class,
    //and different architectures will need to inherit from it in the future.
    virtual PreferRegSetMgr * allocPreferRegSetMgr()
    { return new PreferRegSetMgr(m_rg); }
public:
    explicit LinearScanRA(Region * rg);
    virtual ~LinearScanRA();

    void addUnhandled(LifeTime * lt);
    void addActive(LifeTime * lt);
    void addInActive(LifeTime * lt);
    void addHandled(LifeTime * lt);

    //Get the lifetime which is assigned with the register during the
    //allocation process. The returned lifetime of the same physical register
    //may be different at the differnt positions in the region.
    LifeTime const* getLifeTime(Reg r) const
    { ASSERT0(r != REG_UNDEF); return m_reg2lt.get(r); }

    void setReg2LT(Reg r, LifeTime const* lt)
    { ASSERT0(r != REG_UNDEF); m_reg2lt.set(r, lt); }

    //Retun true if the lifetime of prno can overlap with other lifetime
    //assigned to the same physical register.
    bool canInterfereWithOtherLT(PRNO prno) const;

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

    //The function check whether 'lt' value is simple enough to rematerialize.
    //And return the information through rematctx.
    virtual bool checkLTCanBeRematerialized(
        MOD LifeTime * lt, OUT RematCtx & rematctx);

    void doBackwardJumpAnalysis()
    {
        ASSERT0(m_fake_irmgr);
        m_fake_irmgr->doBackwardJumpAnalysis();
    }

    void dumpDOTWithReg() const;
    void dumpDOTWithReg(CHAR const* name, UINT flag) const;
    void dump4List() const;
    bool dump(bool dumpir = true) const;
    void dumpBBListWithReg() const;
    void dumpReg2LT(Pos start, Pos end, bool open_range = false) const;
    void dumpPR2Reg(PRNO prno) const;
    void dumpPR2Reg() const;

    void destroyLocalUsage();
    void destroy();

    void freeReg(Reg reg);
    void freeReg(LifeTime const* lt);

    //Generate the remat information by traversing the lifetime list.
    void genRematInfo();
    void genRematForLT(MOD LifeTime * lt) const;

    //Get the BB that will used to insert the spill IR of callee registers.
    //Normally, it is the entry BB of the CFG.
    virtual IRBB * getCalleeSpilledBB() const
    { return m_rg->getCFG()->getEntry(); }
    LifeTime * getLT(PRNO prno) const;
    LTSet & getUnhandled() { return m_unhandled; }
    LTSet & getActive() { return m_active; }
    LTSet & getInActive() { return m_inactive; }
    LTSet & getHandled() { return m_handled; }
    BBList * getBBList() const { return m_bb_list; }
    IRCFG * getCFG() const { return m_cfg; }
    LifeTimeMgr & getLTMgr() { return *m_lt_mgr; }
    LTConstraintsMgr * getLTConstraintsMgr()
    { return m_lt_constraints_mgr; }

    RegSet * getPRPreferRegSet(PRNO prno) const
    {
        ASSERT0(m_prefer_regset_mgr);
        return m_prefer_regset_mgr->getPreferRegSet(prno);
    }

    void copyPreferRegSet(PRNO dst_prno, PRNO src_prno)
    {
        ASSERT0(dst_prno != PRNO_UNDEF && src_prno != PRNO_UNDEF);
        ASSERT0(m_prefer_regset_mgr);
        m_prefer_regset_mgr->copyPreferRegSet(dst_prno, src_prno);
    }

    virtual CHAR const* getPassName() const
    { return "Linear Scan Register Allocation"; }
    PASS_TYPE getPassType() const { return PASS_LINEAR_SCAN_RA; }
    void initLTConstraints(MOD LifeTime * lt);
    virtual bool isCalleePermitted(LifeTime const* lt) const;

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

    bool isPrnoAlias(PRNO prno, PRNO alias) const;

    //Check the lifetime is use the fake-use IR or not at the first BB in loop.
    bool isLTWithFakeUseAtLexFirstBBInLoop(LifeTime const* lt) const
    {
        ASSERT0(lt);
        IR * first = const_cast<LifeTime*>(lt)->getFirstOccStmt();
        return isFakeUseAtLexFirstBBInLoop(first);
    }

    void initFakeIRMgr()
    {
        if (m_fake_irmgr != nullptr) { return; }
        m_fake_irmgr = allocFakeIRMgr();
    }

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


    void initPreferRegSetMgr()
    {
        if (m_prefer_regset_mgr != nullptr) { return; }
        m_prefer_regset_mgr = allocPreferRegSetMgr();
    }

    void initLocalUsage();
    virtual bool perform(OptCtx & oc);
    virtual bool performLsraImpl(OptCtx & oc);
    void recalculateSSA(OptCtx & oc) const;

    //Removed the fake-use IR inserted before the LSRA.
    void removeFakeUseIR(OptCtx & oc)
    { m_fake_irmgr->removeFakeUseIR(oc); }

    //Reset all resource before allocation.
    void reset();

    void setFakeUseAtLexFirstBBInLoop(IR * ir)
    { m_fake_use_head_tab.append(ir); }
    void setFakeUseAtLexLastBBInLoop(IR * ir)
    { m_fake_use_tail_tab.append(ir); }

    //Set the constraint set for each IR.
    void scanIRAndSetConstraints();

    //After the lifetime calculation is completed, begin setting constraint
    //sets for each lifetime.
    void tryComputeConstraints();

    void trySetPreferRegSet();

    //This function tries to init the constraints info of 'newlt' based on
    //'lt', and then rename the prno of prs which are included in the
    //constraints info of 'newlt' derived from the parent lifetime 'lt'
    //during the lifetime split.
    //newlt: the new lifetime.
    //lt: the parent lifetime of 'newlt'.
    void tryInitAndRenamePRForLTConstraints(MOD LifeTime * newlt,
        LifeTime const* lt);
    void tryRenamePRForPRNOConstraints(MOD LifeTime * newlt,
        LifeTime const* lt);
    void tryRenamePRForTypeConstraints(MOD LifeTime * newlt,
        LifeTime const* lt);

    bool verify4List() const;
    bool verifyAfterRA() const;
    bool verifyLSRAByInterfGraph(OptCtx & oc) const;
};
//END LinearScanRA

//
//START LTInterfGraph
//
class LTInterfGraphLSRAChecker : public Graph {
    COPY_CONSTRUCTOR(LTInterfGraphLSRAChecker);
protected:
    Region * m_rg;
    RegAllocMgr * m_ramgr;
    LifeTime2DMgr & m_lt_mgr;
protected:
    bool canSkipCheck(LinearScanRA const* lsra, xcom::Edge const* e) const;
public:
    LTInterfGraphLSRAChecker(Region * rg, LifeTime2DMgr & mgr)
        : m_rg(rg), m_lt_mgr(mgr)
    {
        m_ramgr = (RegAllocMgr*)m_rg->getPassMgr()->registerPass(
            PASS_REGALLOC_MGR);
        set_direction(false);
        set_dense(false);
    }
    ~LTInterfGraphLSRAChecker() {}

    //Build interference graph.
    void build();

    bool check(LinearScanRA * lsra);

    void dumpGraph(CHAR const* name) const { dumpDOT(name); }
};
//END LTInterfGraph

void dumpBBListWithReg(Region const* rg);
void dumpDOTWithReg(
    Region const* rg, CHAR const* name = nullptr,
    UINT flag = IRCFG::DUMP_COMBINE);

} //namespace xoc
#endif
