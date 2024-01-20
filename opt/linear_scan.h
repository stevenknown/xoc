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
    friend class OccRecorder;
    Region * m_rg;
    BBPos2Attr * m_pos2attr;
    IRMgr * m_irmgr;
    BBList * m_bb_list;
    Vector<TOPOID> m_bb_topoid;
    LivenessMgr * m_live_mgr;
    BackwardEdgeList m_backward_edges;
    Vector<Pos> m_bb_entry_pos;
    Vector<Pos> m_bb_exit_pos;
    Prno2OccMap m_prno2occ;
    PRLiveSetList m_pr_live_set_list;
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

    //Take the prno of live in for src_bbid and the live out for dst_bbid,
    //and put into m_pr_live_set_list.
    void addToPRLiveSetList(UINT src_bbid, UINT dst_bbid);

    //Give each BB a topological sequence ID.
    void assignTopoIdForBB();

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

    //Determine an edge is backward jump or not.
    bool isBackwardJump(UINT src_bbid, UINT dst_bbid) const
    { return m_bb_topoid[dst_bbid] <= m_bb_topoid[src_bbid]; }

    //Determine the prno is really need to be processed or not. During
    //the analysis, the prno in live-in or live-out of backward edge related
    //BBs are helpful, and the other prnos will be ignored.
    bool isPrInRelatedLiveSet(PRNO pr);

    //This func inserts the fake-use IR at the head or tail of input BB.
    void insertFakeUse(IRBB const* bb, PRNO prno, INSERT_MODE mode);

    //This func inserts the fake-use IR at the head of input BB.
    void insertFakeUseAtBBEntry(IRBB const* bb, PRNO prno, BBPos const& pos);

    //This func inserts the fake-use IR at the tail of input BB.
    void insertFakeUseAtBBExit(IRBB const* bb, PRNO prno, BBPos const& pos);

    //This func records the occurence of the input prno at the specified pos.
    void recordOccurenceForPr(PRNO prno, Pos pos);
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

typedef UINT BBID;
typedef List<PRNO> PRNOList;

//
//START PRInfo
//
//This class is used to record the pr information during the data flow analysis
//for the branch hole finder. There are two parts of data need to be recorded:
// 1. The state of PR.
// 2. The BBID where the DEF and USE state changed from.
class PRInfo {
    COPY_CONSTRUCTOR(PRInfo);

    //Record the state of PR started from the m_bbid.
    PRState m_state;

    //The BBID where the PR is DEF or USE.
    BBID m_bbid;

    //Point to an IR if the PR is defined in this IR, if current PR is a USE,
    //m_def_ir should be set to null. Normally the fake-use IR is inserted
    //before this IR.
    IR const* m_def_ir;
public:
    PRInfo()
    {
        m_state.value = 0;
        m_bbid = BBID_UNDEF;
        m_def_ir = nullptr;
    }

    void copyFrom(PRInfo const* other)
    {
        m_state.value = other->m_state.value;
        m_bbid = other->m_bbid;
        m_def_ir = other->m_def_ir;
    }

    void dump(Region * rg) const;

    UINT getBBID() const { return m_bbid; }

    PR_CHK_STATE getChkState() const
    { return (PR_CHK_STATE)m_state.state.chk; }

    PR_DU_STATE getDUState() const
    { return (PR_DU_STATE)m_state.state.du; }

    IR const* getDefIR() const { return m_def_ir; }

    UINT getState() const { return m_state.value; }

    void setBBID(BBID bbid) { m_bbid = bbid; }

    void setDefIR(IR const* ir) { m_def_ir = ir; }

    void updateChkState(PR_CHK_STATE chk_state)
    { m_state.state.chk = chk_state; }

    //This func updates the DU state per the input du_state.
    //du_state: the new DU state.
    //bbid: the bbid where the DU state comes from.
    void updateDUState(PR_DU_STATE du_state, BBID bbid = BBID_UNDEF);
};
//END PRInfo

//The data type mapping from the prno to the pr infor.
typedef TMap<PRNO, PRInfo*> PR2Info;
typedef TMapIter<PRNO, PRInfo*> PR2InfoIter;

//The data type mapping the branch start BB to PR2Info.
//Key: BBID of a branch start BB.
//value: PR2Info.
typedef TMap<BBID, PR2Info*> BB2PRHolder;
typedef TMapIter<BBID, PR2Info*> BB2PRHolderIter;


//
//START IRDUData
//
//This class is designed to record the DEF and USE info for each IR in all BBs,
//which is used in the preProcess function of BrHoleFinder to collect the DEF
//and USE info for the live-in and live-out PRs.
class IRDUData {
    COPY_CONSTRUCTOR(IRDUData);

    //Record the DEF PRs.
    PRNOList m_def_list;

    //Record the USE PRs.
    PRNOList m_use_list;
public:
    IRDUData() { init(); }
    ~IRDUData()
    {
        m_def_list.destroy();
        m_use_list.destroy();
    }

    void addDef(PRNO prno) { m_def_list.append_tail(prno); }
    void addUse(PRNO prno) { m_use_list.append_tail(prno); }

    void dump(Region * rg);

    PRNOList & getDefList() { return m_def_list; }
    PRNOList & getUseList() { return m_use_list; }

    void init()
    {
        m_def_list.init();
        m_use_list.init();
    }
};
//END IRDUData


//This structure describes the branch lifetime hole info that need to be
//processed by inserting the fake-use IR.
#define HOLEINFO_prno(h) ((h)->prno)
#define HOLEINFO_bbid(h) ((h)->bbid)
#define HOLEINFO_ir(h) ((h)->ir)
typedef struct {
    //The prno may have branch lifetime hole.
    PRNO prno;

    //The bbid for the hole resides in. Normally the DEF of the prno is in
    //this bbid.
    BBID bbid;

    //The IR for this DEF PR.
    IR const* ir;
} HoleInfo;


//
//START FinderResMgr
//
//This class is used to manage the resources used by BrHoleFinder.
class FinderResMgr {
    COPY_CONSTRUCTOR(FinderResMgr);
    typedef List<PR2Info*> PR2InfoList;
    typedef List<BB2PRHolder*> BB2PRHolderList;
    BB2PRHolderList m_bb_holder_list;
    PR2InfoList m_pr2info_list;
    SMemPool * m_pool;
public:
    FinderResMgr()
    {
        m_pool = nullptr;
        init();
    }
    ~FinderResMgr() { destroy(); }

    BB2PRHolder * allocBB2PRHolder();
    IRDUData * allocIRDUData();
    PR2Info * allocPR2Info();

    PRInfo * allocPRInfo()
    { return (PRInfo*)xmalloc(sizeof(PRInfo)); }

    PRState * allocStateResult()
    { return (PRState*)xmalloc(sizeof(PRState)); }

    void destroy();

    HoleInfo * genHoleInfo(PRNO prno, BBID bbid, IR const* ir);
    PosAttr * genPosAttr(UINT v, IRBB const* bb, IR const* ir);

    void init();
    void * xmalloc(size_t size);
};
//END FinderResMgr


//
//START BrHoleFinder
//
//This class shall find the potential branch lifetime holes for LSRA introduced
//by the branch of CFG, and then fix all the branch lifetime holes.
//
//Basically, the lifetime hole of a PR is generated by a DEF next to a USE.
//For the below example, the lifetime hole is: [18-33]
//  lifetime original: <2-17><34-51>
//   | ----------------                --------------------
//   |                u                d      u           u
//
//During the LSRA, there is no need to insert the spill and reload IR if the
//PR is split in the lifetime hole. However, it would lead to the incorrect
//result when lifetime hole is generated by the branches of CFG. Please see
//the following CFG example:
//
//                          BB1:
//                          $p <-- ... #S1
//                          |
//                          v
//                        BB2:
//                        $p < n #S2
//                       |     |
//                  _____|     |_____
//                  |               |
//                  v               v
//                BB3:              BB4:
//                ...               ...
//                ... <-- $p #S3    $p <-- ... #S5
//                $q <-- ... #S4    ...
//                ...               |
//                 |                |
//                 |________  ______|
//                         |  |
//                         v  v
//                         BB5:
//                         ...
//                         ... <-- $p #S6
//                         ...
//The linear order of BBs is: BB1 -> BB2 -> BB3 -> BB4 -> BB5
//The branch lifetime hole of '$p' looks like:
//    | BB1 -> BB2 -> BB3 -> BB4 -> BB5
//    | -----------------      ---------
//    |   d     u       u      d       u
//                          ^
//                          |
//                   branch lifetime hole
//    This hole is generated by the two parallel branches. If $p is split
//    after the USE of $p (#S3) in BB3, and the control flow goes through only
//    one of the two paths on CFG, the value of $p may be overwrote by other
//    PRs, so the user would not get the expected value.
//
//    For example, R1 is a physical register, and it is assigned to $p (#S3) in
//    BB3. There is a DEF of $q (#S4) in BB3 after the split position (in the
//    hole), so if there is no other available physical register resource, the
//    linear scan register allocator may assign R1 to $q without the insertion
//    of spill and reload. When the control flow goes through the left path on
//    CFG, we will get the value of $q if we use the $p (#S6) in BB5, this is
//    not correct, because the value in R1 was overwrote by $q (#S4).
//
//The algorithm will be implemented by three steps:
//  A. preProcess
//     This step will go through all the IRs in BB list to record the
//     DEF and USE info for all PRs, and here only the live-in or live-out
//     PRs will be cared.
//
//  B. check
//     This step will find all the branch lifetime holes based on the DEF
//     and USE info generated in step A. The finding process is performed
//     by the form of forward data flow analysis.
//
//  B.1. Data Flow Equation.
//
//     To implement the equation, a map holder is used as the data structure to
//     flow the data from top to down on the CFG, and each BB stores an IN map
//     holder and an OUT map holder.
//     Map holder:
//       key: BBID, stands for a branch start BB, and the out degree is greater
//                  than 1. In above CFG, only BB2 is a branch start BB.
//       value: Map, records the PR and its related information.
//              Key: PR number
//              value:           |-- PRState: State of PR.
//                      PRInfo---|-- DEF IR: Set only if the PR is a DEF in IR.
//                               |-- BBID: Where the PR is defined or used.

//     The data flow equation can be formulated as below:
//       1. BB[IN] = BB[OUT 1] U BB[OUT 2] U ... U BB[OUT N]
//       2. BB[OUT] = BB[IN] U GEN
//          where N is the number of predecessors.
//          BB[OUT N] is the OUT map holder of the Nth predecessor of
//                    current BB.
//          BB[IN] is the IN map holder of current BB.
//          BB[OUT] is the OUT map holder of current BB.
//
//     B.1.1. GEN
//         GEN map holder:
//         If a BB is in a branch, just set the BBID of branch start BB as the
//         key, and then go through all the IRs in the BB to generate PR2Info.
//         If a PR is used or defined in an IR, set the PR number as the key,
//         and then generate the PRInfo.
//
//         GEN PRInfo:
//          1. If the check state of PR is PR_CHK_CONFLICT, do nothing.
//          2. If current IR is the first occurence of the PR in current BB
//             after the branch start BB, set the DU state to PR_DU_DEFINED
//             if the occurence is a DEF, and to PR_DU_USED if the occurence
//             is a USE, also set the check state to PR_CHK_GOOD.
//
//         [NOTE1]: Only the first occurence of PR after the branch start BB is
//                  recorded. Because when all BBs of multiple siblings are
//                  ordered linearly, the type (DEF/USE) of the first occurence
//                  for a PR can determine whether there will be a lifetime
//                  hole cross the branch. For example, on above CFG, there is
//                  a branch lifetime hole for $p, however, if the type of #S5
//                  is changed from DEF to USE (""... <-- $p"), the first
//                  occurence of $p in two siblings BB3 and BB4 are both USE,
//                  there will be no lifetime hole no matter what the linear
//                  order (BB3->BB4 or BB4->bb3) is.
//
//        GEN example on BBID 4 in above CFG:
//          For the key BBID 2, which is a branch start BB, IR "$p <-- ..." #S5
//          is the first DEF occurence of $p in BBID 4.
//          so the GEN result will be updated as:
//          key: BBID 2 (branch start BB)
//          value: Key: $p
//                 value:          |-- PRState: PR_DU_DEFINED, PR_CHK_GOOD
//                        PRInfo---|-- DEF IR: '$p <-- ...' #S5
//                                 |-- BBID: 4
//
//     B.1.2. UNION Definition
//         Based on the above map holder data structure, when multiple edges
//         join at a BB on CFG, the union opertaion is used to merge all the
//         OUT map holders of all predecessors as the IN map holder for the BB,
//         it can be defined as below:
//         1. If the key of branch start BBID is different, copy and use
//            directly, that means the elements of the map will include more
//            branch start BBs.
//
//         2. If the key of branch start BBID is same, that means that they are
//            from the same branch start BB, we need to do the union operation
//            on the value PR2Info:
//            2.1. If the key of PR number is different, copy and use directly,
//                 that means the elements of map will include more PRs appeared
//                 in the same branch start BB.
//
//            2.1. If the key of PR number is same, that means the same PR are
//                 appeared in two different predecessors with the same branch
//                 start BB (the same ancestor), we need to do the further
//                 merge operation on the PRInfo of two predecessors, and the
//                 merged check state will be updated per the TABLE1 below:
//                ________________________________________________________
//                | rules  | predecessor_A | predecessor_B |    merged   |
//                |        |  check state  |  check state  | check state |
//                |------------------------------------------------------|
//                | rule 1 |  conflict     |  conflict     |  conflict   |
//                | rule 2 |  conflict     |  undef        |  conflict   |
//                | rule 3 |  conflict     |  good         |  conflict   |
//                | rule 4 |  good         |  conflict     |  conflict   |
//                | rule 5 |  undef        |  good         |  good       |
//                | rule 6 |  good         |  good         |  TABLE2     |
//                ________________________________________________________
//                                  TABLE1
//
//                For the rule 6 in TABLE1, the merged check state should be
//                updated per the TABLE2 below:
//                ___________________________________________________________
//                |    rules  | predecessor_A | predecessor_B |   merged    |
//                |           |   DU state    |    DU state   | check state |
//                |---------------------------------------------------------|
//                |  rule 6.1 |      DEF      |     USE       |  conflict   |
//                |  rule 6.2 |      USE      |     DEF       |  conflict   |
//                |  rule 6.3 |      USE      |     USE       |  good       |
//                |  rule 6.4 |      DEF      |     DEF       |  good       |
//                ___________________________________________________________
//                                  TABLE2
//
//            [NOTE2]: When the merged check state is conflicted, the defined
//                     BBID will be recorded as a marker to indicate where the
//                     fake-use IR need to be inserted.
//            The detailed union example can refer to B.2.5 in the next section.
//
//  B.2. Data flow process example on above CFG.
//     B.2.1. Process BB1
//         IN BB1:
//           NONE, because it is not in any branch.
//         OUT BB1:
//           NONE, because it is not in any branch.
//     B.2.2. Process BB2
//         IN BB2:
//           NONE, because it is not in any branch.
//         OUT BB2:
//           NONE, because it is not in any branch.
//     B.2.3. Process BB3
//         IN BB3: flowed down from BB2, same as OUT BB2.
//         OUT BB3:
//           key: BBID 2 (branch start BB)
//           value: Key: $p
//                  value:           |-- PRState: PR_DU_USED, PR_CHK_GOOD
//                          PRInfo---|-- DEF IR: null
//                                   |-- BBID: 3
//                  Key: $q
//                  value:           |-- PRState: PR_DU_DEFINED, PR_CHK_GOOD
//                          PRInfo---|-- DEF IR: '$q <-- ...' #S4
//                                   |-- BBID: 3
//     B.2.4. Process BB4
//         IN BB4: flowed down from BB2, same as OUT BB2.
//         OUT BB4:
//           key: BBID 2 (branch start BB)
//           value: Key: $p
//                  value:           |-- PRState: PR_DU_DEFINED, PR_CHK_GOOD
//                          PRInfo---|-- DEF IR: '$p <-- ...' #S5
//                                   |-- BBID: 4
//     B.2.5. Process BB5
//         The union operation is needed to do for the two precedecssors
//          (BB3 and BB4):
//         For the key start BBID 2:
//
//         For the key $p, both contained in BB3 and BB4, the PRInfo is not
//         same, and the check states are both PR_CHK_GOOD, so we need to
//         check further per rule 6 in above TABLE2. Because the two DU states
//         are not same, one is PR_DU_DEFINED (precedecssor BBID 4), and the
//         other one is PR_DU_USED (precedecssor BBID 3), so the result is
//         PR_CHK_CONFLICT according to the rule 6.1 or 6.2 in TABLE2 above.
//
//         For the key $q, only contained in BB3, so just copy it into the
//         final map directly.
//
//         The final union result will be updated as:
//         IN BB5:
//           key: BBID 2 (branch start BB)
//           value: Key: $p
//                  value:           |-- PRState: PR_DU_DEFINED, PR_CHK_CONFLICT
//                          PRInfo---|-- DEF IR: '$p <-- ...' #S5
//                                   |-- BBID: 4, see the [NOTE2] above.
//                  Key: $q
//                  value:           |-- PRState: PR_DU_DEFINED, PR_CHK_GOOD
//                          PRInfo---|-- DEF IR: '$q <-- ...' #S4
//                                   |-- BBID: 3
//         OUT BB5:
//           key: BBID 2 (branch start BB)
//           value: Key: $p
//                  value:           |-- PRState: PR_DU_DEFINED, PR_CHK_CONFLICT
//                          PRInfo---|-- DEF IR: '$p <-- ...' #S5
//                                   |-- BBID: 4
//                  Key: $q
//                  value:           |-- PRState: PR_DU_DEFINED, PR_CHK_GOOD
//                          PRInfo---|-- DEF IR: '$q <-- ...' #S4
//                                   |-- BBID: 3
//
//  C. postProcess
//     Definition for the fake-use IR: A statement that stores a PR to a fake
//     memory var, the format is: fake_var <-- $p
//
//     This step will fix the branch lifetime holes found in step B. The
//     fake-use IR for the specified PR can be inserted before the IR where
//     the PR is defined to eliminate the branch lifetime hole when do the
//     LSRA. The fake-use IR is set an attribute POS_ATTR_NO_CODE_GEN, which
//     means this IR is not involved in the final machine instruction
//     generation, and will be removed eventually during the attribute
//     processing phase.
//
//     In this CFG example, the conflicted PR $p is defined in #S5 of BB4,
//     so the fake-use IR #S7 will be inserted before #S5 in BB4.
//
//     After this step, the final CFG is as below:
//
//                          BB1:
//                          $p <-- ... #S1
//                          |
//                          v
//                        BB2:
//                        $p < n $S2
//                       |     |
//                  _____|     |______
//                  |                |
//                  |                v
//                  v               BB4:
//                BB3:              ...
//                ...               fake_var <-- $p #S7
//                ... <-- $p #S3    $p <-- ...      #S5
//                $q <-- ... #S4    ...
//                ...                |
//                 |                 |
//                 |________  ______|
//                         |  |
//                         v  v
//                         BB5:
//                         ...
//                         ... <-- $p #S6
//                         ...
//     The DU state of PR $p in BB3 and BB4 are changed to both PR_DU_USED
//     in the final CFG, and there is no chance to introduce a lifetime
//     hole when do the linear scan register allocation, this will ensure the
//     correctness of the register allocation result.
//
class BrHoleFinder {
    COPY_CONSTRUCTOR(BrHoleFinder);
    typedef TMap<IR const*, IRDUData const*> IR2DUData;
    typedef TMapIter<IR const*, IRDUData const*> IR2DUDataIter;

    Region * m_rg;
    BBList * m_bb_list;
    LivenessMgr * m_live_mgr;
    LinearScanRA * m_lsra;
    FinderResMgr m_resmgr;
    BBPos2Attr * m_pos2attr;
    FakeVarMgr * m_fake_var_mgr;
    IRMgr * m_irmgr;

    //The DU info for every IR generated in the preprocess.
    IR2DUData m_ir2du;

    //The data in for each BB during the data flow analysis.
    Vector<BB2PRHolder*> m_bb_state_in;

    //The data out for each BB during the data flow analysis.
    Vector<BB2PRHolder*> m_bb_state_out;

    //The hole info list used for future fix.
    List<HoleInfo const*> m_hole_list;
public:
    BrHoleFinder(Region * rg, BBPos2Attr * pos2attr,
                 FakeVarMgr * fake_var_mgr, LinearScanRA * lsra);
    ~BrHoleFinder() { destroy(); }

    void destroy();

    void dump() const;

    //Init the resources for the analysis.
    void init();

    void reset();

    void run();
protected:
    //Calculate the PR info in holder of specified BB.
    void calcHolderForBB(IRBB const* bb);

    //Calculate the PR info in holder of IR in specified BB.
    void calcHolderForIR(IR const* ir, MOD BB2PRHolder * holder,BBID cur_bbid);

    //Calculate the PR info in PR2Info in map holder of specified BB.
    void calcPR2Info(MOD PR2Info * pr2state, IRDUData const* ana,
                     BBID cur_bbid, IR const* ir);

    //Calculate the DEF PR info in PR2Info in map holder of specified BB.
    void calcPRDefState(MOD PR2Info * pr2state, IRDUData const* ana,
                        BBID cur_bbid, IR const* ir);

    //Calculate the USE PR info in PR2Info in map holder of specified BB.
    void calcPRUseState(MOD PR2Info * pr2state, IRDUData const* ana,
                        BBID cur_bbid);

    //This function finds the branch lifetime holes introduced by the branch.
    bool check();

    //Copy the data of BB2PRHolder from h2 to h1.
    void copyBBPrHolder(MOD BB2PRHolder * h1, BB2PRHolder const* h2);

    //Copy the data of PR2Info from src to dst.
    void copyPR2Info(MOD PR2Info * dst, PR2Info const* src);

    void dumpDataFlow() const;
    void dumpIRDUInfo() const;
    void dumpHolderState(BB2PRHolder const* holder) const;
    void dumpPR2Info(PR2Info const* pr2state) const;

    IRDUData const* getIRDUInfo(IR const* ir) const { return m_ir2du.get(ir); }

    BB2PRHolder * genBBPrHolderIn(BBID bbid);
    BB2PRHolder * genBBPrHolderOut(BBID bbid);

    BB2PRHolder * getBBPrHolderIn(BBID bbid) const
    { return m_bb_state_in.get(bbid); }
    BB2PRHolder * getBBPrHolderOut(BBID bbid) const
    { return m_bb_state_out.get(bbid); }

    PR2Info * genPr2InfoInHolder(MOD BB2PRHolder * h, BBID bbid);

    void genDefForIR(IR const* ir, MOD IRDUData * du_info);
    void genDUForIR(IR const* ir, MOD ConstIRIter & irit);
    void genUseForIR(IR const* ir, MOD IRDUData * du_info,
                     MOD ConstIRIter & irit);

    //Insert a fake-use IR for the prno per the pos and marker during the
    //branch lifetime hole fix phase.
    //bb: The BB to insert the fake-use IR
    //prno: the USE prno in fake-use IR.
    //pos: the combined position info for fake-use IR.
    //marker: fake-use IR should be inserted before this marker.
    void insertFakeUse(IRBB const* bb, PRNO prno, BBPos const& pos,
                       IR const* marker);

    //Merge the two PRInfo.
    void mergePrInfo(MOD PRInfo * pr_info1, PRInfo const* pr_info2, PRNO prno);

    //Fix the branch lifetime hole after check.
    void postProcess();

    //Do some analysis for IR before the check.
    bool preProcess();

    //Record the pr hole info for the future fix.
    void recordPrHoleInfo(HoleInfo const* hole)
    { m_hole_list.append_tail(hole); }

    void setIRDUInfo(IR const* ir, IRDUData const* du_info)
    { m_ir2du.set(ir, du_info); }
    void setBBPrHolderIn(BBID bbid, BB2PRHolder * h)
    { m_bb_state_in.set(bbid, h); }
    void setBBPrHolderOut(BBID bbid, BB2PRHolder * h)
    { m_bb_state_out.set(bbid, h); }

    //Do the union operation between two BB2PRHolder.
    void unionBBPrHolder(MOD BB2PRHolder * h1, BB2PRHolder const* h2);

    //Do the union operation between two PR2Info.
    void unionPr2State(MOD PR2Info * pr2state1, PR2Info const* pr2state2);
};
//END BrHoleFinder


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

    //This function mainly prepares the correct register type for spill,
    //not the data type of lt.
    Type const* getRegType(PRNO prno) const;

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
