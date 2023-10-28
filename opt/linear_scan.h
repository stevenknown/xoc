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

    //Indicate the lifetime of PR at POS should be extended to the next Def.
    //eg. This attribute is set at POS 31
    //   Original Lifetime:  <20-31><42-45><50-57>
    //        After adjust:  <20-41><42-45><50-57>
    POS_ATTR_LT_NO_TERM_AFTER = 0x2,

    //Indicate the lifetime of PR at POS should be started from current occ.
    //eg. This attribute is set at POS 31
    //   Original Lifetime:  <20-31><42-45><50-57>
    //        After adjust:  <31><42-45><50-57>
    POS_ATTR_LT_TERM_BEFORE   = 0x4,
} POS_ATTR_FLAG;

//
//START PosAttr
//
class PosAttr : public UFlag {
    IRBB const* m_bb;
    IR const* m_ir;
public:
    PosAttr(UINT v, IRBB const* bb, IR const* ir) :
            UFlag(v), m_bb(bb), m_ir(ir) {}

    void init(UINT v, IRBB const* bb, IR const* ir)
    {
        set(v);
        setBB(bb);
        setIR(ir);
    }

    IRBB const* getBB() const { return m_bb; }
    IR const* getIR() const { return m_ir; }
    void setBB(IRBB const* bb) { m_bb = bb; }
    void setIR(IR const* ir) { m_ir = ir; }
};
//END PosAttr

//The insert mode used for Backward jump analysis. The fake IR should
//be added to the BB at the specified location per the INSERT mode.
typedef enum {
    INSERT_MODE_UNDEF = 0,

    //Indicate the fake IR should be appended at the head of BB.
    // eg.
    // LABEL: label_XX
    // ----- BB start ---
    //                                     <-------- fake IR
    // stpr $2:u32 id:xx
    //     xxxx
    //  goto label xxx id:xx
    // ----- BB end ---
    INSERT_MODE_HEAD = 1,

    //Indicate the fake IR should be appended at the end of BB but before the
    //branch IR.
    // eg.
    // LABEL: label_XX
    // ----- BB start ---
    // stpr $2:u32 id:xx
    //     xxxx
    //                                     <-------- fake IR
    //  goto label XXX id:xx
    // ----- BB end ---
    INSERT_MODE_TAIL = 2
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

//Record the attribute of IR.
typedef xcom::TMap<BBPos, PosAttr const*, BBPosCmp> BBPos2Attr;
typedef xcom::TMapIter<BBPos, PosAttr const*> BBPos2AttrIter;

//ID for topological order.
typedef UINT TOPOID;

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
//jump, and insert some fake IRs for some prno after analysis.
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
    friend class OccRecorder;
    Region * m_rg;
    BBPos2Attr * m_pos2attr;
    IRMgr * m_irmgr;
    TypeMgr * m_tm;
    BBList * m_bb_list;
    Vector<TOPOID> m_bb_topoid;
    LivenessMgr * m_live_mgr;
    BackwardEdgeList m_backward_edges;
    Vector<Pos> m_bb_entry_pos;
    Vector<Pos> m_bb_exit_pos;
    Prno2OccMap m_prno2occ;
    PRLiveSetList m_pr_live_set_list;
    Var * m_fake_var;
    LinearScanRA * m_lsra;
    BackwardJumpAnalysisResMgr * m_resource_mgr;

public:
    BackwardJumpAnalysis(Region * rg, BBPos2Attr * pos2attr,
                         LinearScanRA * lsra);
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

    //Take the prno of live in for src_bbid and the live out for dst_bbid,
    //and put into m_pr_live_set_list.
    void addToPRLiveSetList(UINT src_bbid, UINT dst_bbid);

    //Give each BB a topological sequence ID.
    void assignTopoIdForBB();

    //Check the condition for fake use first, and then do the insertion.
    void checkAndInsertFakeUse();

    //Collect all the backward jumps in the control flow graph.
    void collectBackwardJumps();

    //Generate the occurence for pr in m_pr_live_set_list. The occurence is a
    //simple line/IR sequence ID.
    void generateOccurence();

    //Generate the occurence for BB.
    //pos: the BB start position.
    void generateOccurenceForBB(IRBB const* bb, MOD Pos & pos);

    //This func generates a fake var, which is shared for all fake use IRs.
    Var * genFakeVar();

    //This func records the end/exit position of the input BB.
    Pos getBBEndPos(UINT bbid) const { return m_bb_exit_pos.get(bbid); }

    //This func records the start/entry position of the input BB.
    Pos getBBStartPos(UINT bbid) const { return m_bb_entry_pos.get(bbid); }

    //Determine an edge is backward jump or not.
    bool isBackwardJump(UINT src_bbid, UINT dst_bbid) const
    { return m_bb_topoid[dst_bbid] <= m_bb_topoid[src_bbid]; }

    //Determine the prno is really need to be processed or not. During
    //the analysis, the prno in live-in or live-out of backward edge related
    //BBs are helpful, and the other prnos will be ignored.
    bool isPrInRelatedLiveSet(PRNO pr);

    //This func inserts the fake use IR at the head or tail of input BB.
    void insertFakeUse(IRBB const* bb, PRNO prno, INSERT_MODE mode);

    //This func inserts the fake use IR at the head of input BB.
    void insertFakeUseAtBBEntry(IRBB const* bb, PRNO prno, BBPos const& pos);

    //This func inserts the fake use IR at the tail of input BB.
    void insertFakeUseAtBBExit(IRBB const* bb, PRNO prno, BBPos const& pos);

    //This func records the occurence of the input prno at the specified pos.
    void recordOccurenceForPr(PRNO prno, Pos pos);

    //This func maps the position with the input attribute.
    void setPosAttr(BBPos const& pos, PosAttr const* attr);
};
//END BackwardJumpAnalysis


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
    //Init all register sets.
    //Note the function invoked by constructor can not be virtual.
    void initRegSet();
public:
    RegSetImpl(LinearScanRA & ra) : m_ra(ra) { initRegSet(); }
    virtual ~RegSetImpl() {}

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


    //True if reg is allocable.
    bool isAvailAllocable(Reg r) const
    { return m_avail_allocable.is_contain(r); }

    //True if input reg is callee register.
    bool isCallee(Reg r) const
    { return getTIMgr().isCallee(r) || getTIMgr().isVectorCallee(r); }
    //True if input reg is caller register.
    bool isCaller(Reg r) const
    { return getTIMgr().isCaller(r) || getTIMgr().isVectorCaller(r); }
    bool isParam(Reg r) const
    { return getTIMgr().isParam(r) || getTIMgr().isVectorParam(r); }
    bool isReturnValue(Reg r) const
    { return getTIMgr().isReturnValue(r) || getTIMgr().isVectorReturnValue(r); }
    //Special registers represent registers used by dedicate
    //that do not exist in any regset.
    bool isSpecialReg(Reg r) const;

    TargInfoMgr & getTIMgr() const;

    //Get allocable regsets.
    RegSet const& getAvailAllocable() const { return m_avail_allocable; }

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


//The class represents the basic structure and interface of linear-scan register
//allocation.
class LinearScanRA : public Pass {
    COPY_CONSTRUCTOR(LinearScanRA);
protected:
    bool m_is_apply_to_region;
    LifeTimeMgr * m_lt_mgr;
    TargInfoMgr * m_ti_mgr;
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
    DedicatedMgr m_dedicated_mgr;
    PRNO2Var m_prno2var;
    ActMgr m_act_mgr;

    //This list records the spill var at entry bb.
    //These spill vars will be stored at the bottom of the stack.
    //reloc_mgr would gets this list and computes offset for each var.
    //
    //The reason these variables are stored at the bottom of the stack
    //is that they are spilled before sp adjustment, and the bottom of
    //stack is fixed.
    //
    // $sp = $sp - stacksize    -- generate old sp.
    // spill $fp                -- spill var1 at entry bb.
    // spill $ra                -- spill var2 at entry bb.
    // spill callee save        -- spill var3 at entry bb.
    // ... ...
    // $fp = $sp + stacksize    -- store old $sp in $fp.
    // $sp = realign $sp.       -- generate new sp.
    // ... ...
    // ... ...
    // $sp = $fp - stacksize    -- restore old sp.
    // $sp = $fp + stacksize    -- restore sp.
    xcom::List<Var const*> m_spillvar_at_entrybb;
protected:
    virtual RegSetImpl * allocRegSetImpl() { return new RegSetImpl(*this); }
public:
    explicit LinearScanRA(Region * rg);
    virtual ~LinearScanRA();

    void addUnhandled(LifeTime * lt);
    void addActive(LifeTime * lt);
    void addInActive(LifeTime * lt);
    void addHandled(LifeTime * lt);

    virtual IR * buildRemat(PRNO prno, RematCtx const& rematctx,
                            Type const* ty);
    virtual IR * buildMove(PRNO from, PRNO to, Type const* fromty,
                           Type const* toty);
    virtual IR * buildSpill(PRNO prno, Type const* ty);
    virtual IR * buildReload(PRNO prno, Var * spill_loc, Type const* ty);

    //Should be called before register allocation.
    PRNO buildPrnoDedicated(Type const* type, Reg reg);
    //Should be called after register allocation.
    PRNO buildPrno(Type const* type, Reg reg);

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

    //Get the list of spill var in entrybb.
    xcom::List<Var const*> * getEntryBBSpillVarList()
    { return &m_spillvar_at_entrybb; }

    //Construct a name for Var that will lived in Region.
    CHAR const* genFuncLevelNewVarName(OUT xcom::StrBuf & name);
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
    TargInfoMgr & getTIMgr() { return *m_ti_mgr; }
    LifeTimeMgr & getLTMgr() { return *m_lt_mgr; }
    Reg getDedicatedReg(LifeTime const* lt) const
    { return getDedicatedReg(lt->getPrno()); }
    Reg getDedicatedReg(PRNO prno) const { return m_dedicated_mgr.get(prno); }
    DedicatedMgr & getDedicatedMgr() { return m_dedicated_mgr; }
    ActMgr & getActMgr() { return m_act_mgr; }
    virtual CHAR const* getPassName() const
    { return "Linear Scan Register Allocation"; }
    PASS_TYPE getPassType() const { return PASS_LINEAR_SCAN_RA; }

    virtual ArgPasser * getArgPasser() const {
        ASSERTN(0, ("Target Dependent Code"));
        return nullptr;
    }

    //Get target physical registers.
    virtual Reg getFP() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual Reg getBP() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual Reg getRA() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual Reg getSP() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual Reg getGP() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual Reg getTA() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual Reg getRegisterZero() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual Reg getScalarArgRegStart() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    bool hasReg(PRNO prno) const;
    bool hasReg(LifeTime const* lt) const;

    //Return true if register r1 alias to r2.
    virtual bool isAlias(Reg r1, Reg r2) const { return r1 == r2; }
    virtual bool isCalleePermitted(LifeTime const* lt) const;
    bool isDedicated(PRNO prno) const
    { return m_dedicated_mgr.is_dedicated(prno); }

    bool isInsertOp() const
    {
        LinearScanRA * pthis = const_cast<LinearScanRA*>(this);
        return pthis->getSpillTab().get_elem_count() != 0 ||
               pthis->getReloadTab().get_elem_count() != 0 ||
               pthis->getMoveTab().get_elem_count() != 0;
    }

    bool isMoveOp(IR const* ir) const;
    bool isPrnoAlias(PRNO prno, PRNO alias) const;
    //Return true if ir is rematerializing operation.
    virtual bool isRematLikeOp(IR const* ir) const;
    bool isReloadOp(IR const* ir) const;
    bool isRematOp(IR const* ir) const;
    bool isSpillOp(IR const* ir) const;


    virtual bool perform(OptCtx & oc);
    virtual bool performLsraImpl(OptCtx & oc);

    //Reset all resource before allocation.
    void reset();

    //Record the spill var in entry bb.
    void recordSpillVarAtEntryBB(Var * var)
    { m_spillvar_at_entrybb.append_tail(var); }

    void setApplyToRegion(bool doit) { m_is_apply_to_region = doit; }
    void setDedicatedReg(PRNO prno, Reg r) { m_dedicated_mgr.add(prno, r); }
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
