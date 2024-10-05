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
#include "cominc.h"
#include "comopt.h"
#include "targinfo_mgr.h"
#include "lifetime.h"
#include "lt_interf_graph.h"
#include "linear_scan.h"
#include "lsra_impl.h"
#include "lsra_scan_in_pos.h"
#include "lt_prio_mgr.h"
#include "lsra_scan_in_prio.h"

namespace xoc {

static void dumpFakeUse(LinearScanRA * lsra, IR const* fake_ir,
                        IR const* marker, PRNO prno,
                        UINT bbid, bool head, CHAR const* context)
{
    CHAR const* preStr = "fake-use: insert fake-use IR id:";
    if (marker != nullptr) {
        lsra->getActMgr().dump(
            "%s %u of prno:%u before IR id:%u in bb:%u for %s", preStr,
            fake_ir->id(), prno, marker->id(), bbid, context);
        return;
    }
    lsra->getActMgr().dump(
        "%s %u of prno:%u at the %s of bb:%u for %s", preStr,
        fake_ir->id(), prno, head ? "head" : "tail", bbid, context);
}


//This func maps the position with the input attribute.
static void setPosAttr(BBPos const& pos, PosAttr const* attr,
                       MOD BBPos2Attr * pos2attr)
{
    ASSERT0(pos2attr && attr);
    //Check the duplicated insertion. At the moment, duplicated insertion for
    //same fake-use pr at the same BB position is meaningless.
    if (pos2attr->find(pos)) { return; }
    pos2attr->set(pos, attr);
}

//
//START PosAttrProc
//
class PosAttrProc {
    COPY_CONSTRUCTOR(PosAttrProc);
protected:
    Region * m_rg;
    BBPos2Attr const& m_pos2attr;
    LinearScanRA * m_lsra;
public:
    PosAttrProc(Region * rg, BBPos2Attr const& ir2attr, LinearScanRA * lsra) :
        m_rg(rg), m_pos2attr(ir2attr), m_lsra(lsra)
    { }
    virtual ~PosAttrProc() {}

    virtual void preProcess() { }

    //This func shall traverse all the attribute map, and process every
    //attribute. This func is a template for the attribute processor, and
    //the derived class should not override this method.
    void process();
protected:
    //Check the attributes is related to the current attributes processor.
    //This function must be implemented by the derived class.
    virtual bool checkRelatedAttrs(PosAttr const* attr) const = 0;

    //Process attr at the specified pos. This function must be implemented
    //by the derived class.
    virtual bool processAttr(BBPos const& pos, PosAttr const* attr) = 0;
};


void PosAttrProc::process()
{
    preProcess();

    BBPos2AttrIter it;
    PosAttr const* attr = nullptr;
    for (BBPos pos = m_pos2attr.get_first(it, &attr); !it.end();
         pos = m_pos2attr.get_next(it, &attr)) {
        if (!checkRelatedAttrs(attr)) { continue; }
        processAttr(pos, attr);
    }
}
//END PosAttrProc


//
//START PosAttrNoCodeGenProc
//
//This class processes the attribute with POS_ATTR_NO_CODE_GEN.
class PosAttrNoCodeGenProc : public PosAttrProc {
    COPY_CONSTRUCTOR(PosAttrNoCodeGenProc);
public:
    PosAttrNoCodeGenProc(Region * rg, BBPos2Attr const& ir2attr,
                         LinearScanRA * lsra) :
        PosAttrProc(rg, ir2attr, lsra)
    { }
    virtual ~PosAttrNoCodeGenProc() {}

protected:
    virtual bool checkRelatedAttrs(PosAttr const* attr) const
    {
        ASSERT0(attr);
        return attr->have(POS_ATTR_NO_CODE_GEN);
    }

    virtual bool processAttr(BBPos const& pos, PosAttr const* attr)
    {
        ASSERT0(attr);
        if (!processAttrNoCodeGen(pos, attr)) { return false; }
        return true;
    }

    bool processAttrNoCodeGen(BBPos const& pos, PosAttr const* attr);
};


bool PosAttrNoCodeGenProc::processAttrNoCodeGen(BBPos const& pos,
                                                PosAttr const* attr)
{
    ASSERT0(attr);
    if (!attr->have(POS_ATTR_NO_CODE_GEN)) { return true; }
    IR * tmp_ir = const_cast<IR*>(attr->getIR());

    //Check the fake-use IR responding at the pos.
    ASSERT0(tmp_ir && tmp_ir->is_st());
    ASSERT0(tmp_ir->getRHS() && tmp_ir->getRHS()->is_pr());
    ASSERT0(m_lsra->isPrnoAlias(tmp_ir->getRHS()->getPrno(), BBPOS_prno(pos)));
    IRBB const* bb = attr->getBB();
    ASSERT0(bb && BBPOS_bbid(pos) == bb->id());
    tmp_ir = BB_irlist(const_cast<IRBB*>(bb)).remove(tmp_ir);
    m_rg->freeIRTree(tmp_ir);
    return true;
}
//END PosAttrNoCodeGenProc


//
//START PosAttrLifeTimeProc
//
//This class processes the attribute with POS_ATTR_LT_NO_TERM_AFTER
//and POS_ATTR_LT_SHRINK_BEFORE.
class PosAttrLifeTimeProc : public PosAttrProc {
    COPY_CONSTRUCTOR(PosAttrLifeTimeProc);
    SMemPool * m_pool;
    //If there are multiple fake-use IRs related to the same PR, and these
    //IRs are included in different BBs, structure PosInfo is used to record
    //these infos about the position and the attribute.
    // e.g:
    //    In the below CFG, there are three fake-use IRs about $p.
    //
    //           BB1: ...
    //                 |
    //                 V
    //           BB2: fake_var <-- $p  #S1 (BB Entry pos: 20)
    //                ...
    //                 |
    //                 V
    //           BB3: fake_var <-- $p  #S2 (BB Entry pos: 30)
    //                ...
    //                 |
    //                 V
    //           BB4: fake_var <-- $p  #S3 (BB Entry pos: 40)
    //                ...
    //                 |
    //                 V
    //           BB5: ...
    //
    //    For this case:
    //        min_pos = 20
    //        attr_first points to the attribute of #S1.
    typedef struct {
        //attr_first will record the attribute of the fake-use IR contained
        //in the BB with the minimum BB entry position among these BBs inserted
        //with the fake-use IRs related to the same PR in lexcical sequence.
        PosAttr const* attr_first;

        //min_pos will record the minimum entry position of a BB among these BBs
        //inserted with the fake-use IRs related to the same PR in lexcical
        //sequence.
        Pos min_pos;
    } PosInfo;
    typedef xcom::TMap<PRNO, PosInfo*> PRNO2Pos;
    typedef xcom::TMapIter<PRNO, PosInfo*> PRNO2PosIter;
public:
    PosAttrLifeTimeProc(Region * rg, BBPos2Attr const& ir2attr,
                        LinearScanRA * lsra)
        : PosAttrProc(rg, ir2attr, lsra)
    {
        m_pool = smpoolCreate(64, MEM_COMM);
        ASSERT0(m_pool);
    }

    virtual ~PosAttrLifeTimeProc()
    {
        if (m_pool != nullptr) {
            smpoolDelete(m_pool);
            m_pool = nullptr;
        }
    }

protected:
    PosInfo * allocPosInfo()
    {
        PosInfo * info = (PosInfo*)xmalloc(sizeof(PosInfo));
        return info;
    }

    virtual bool checkRelatedAttrs(PosAttr const* attr) const;
    virtual void preProcess();
    virtual bool processAttr(BBPos const& pos, PosAttr const* attr);

    bool processAttrLTNoTerminatedAfter(BBPos const& pos, PosAttr const* attr);
    bool processAttrLTShrinkedBefore(BBPos const& pos, PosAttr const* attr);
    bool processAttrLTExtendBBEnd(BBPos const& pos, PosAttr const* attr);

    //Record the position info for attribute POS_ATTR_LT_SHRINK_BEFORE.
    void recordPosInfo(OUT PRNO2Pos & prno2pos, PRNO prno, Pos pos,
                       PosAttr const* attr);
    void * xmalloc(size_t size);
};


void * PosAttrLifeTimeProc::xmalloc(size_t size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset(p, 0, size);
    return p;
}


void PosAttrLifeTimeProc::recordPosInfo(OUT PRNO2Pos & prno2pos, PRNO prno,
                                        Pos pos, PosAttr const* attr)
{
    ASSERT0(attr && prno != PRNO_UNDEF && pos != POS_UNDEF);
    bool find = false;
    PosInfo * pos_info = nullptr;
    pos_info = prno2pos.get(prno, &find);
    if (!find) {
        pos_info = allocPosInfo();
        pos_info->attr_first = attr;
        pos_info->min_pos = pos;
        prno2pos.set(prno, pos_info);
        return;
    }
    if (pos_info->min_pos > pos) {
        pos_info->min_pos = pos;
        pos_info->attr_first = attr;
    }
}


//This function does the pre-process on the attribute POS_ATTR_LT_SHRINK_BEFORE.
//This attribute means the beginning of lifetime will be shrunk from the entry
//of region to the entry position of current BB, if the fake-use IRs related to
//the same PR are inserted in multiple BBs, the modified lifetime may be
//incorrect due to the improper processing order of these fake-use IRs.
//
//Because for each fake-use IR will do the shrinking process for each fake-use
//IR, so the last fake-use IR of the same PR will determine the final start
//position of lifetime of this PR. So it is no doubt that the final modified
//lifetime will be correct only if the fake-use IR with the minimum BB Entry pos
//is the last one to be processed, or else the modified lifetime cannot cover
//the other fake-use IRs related to the same PR.
//
//So the fake-use IR in first BB in lexical sequence (has the minimum BB Entry
//pos) need to be processed, while the otherfake-use IRs of the same PR in other
//BBs need to be ignored.
//
// e.g:
//   In the below CFG, there are three fake-use IRs about $p.
//
//          BB1: ...
//                |
//                V
//          BB2: fake_var <-- $p  #S1 (BB Entry pos: 20)
//               ...
//                |
//                V
//          BB3: fake_var <-- $p  #S2 (BB Entry pos: 30)
//               ...
//                |
//                V
//          BB4: fake_var <-- $p  #S3 (BB Entry pos: 40)
//               ...
//                |
//                V
//          BB5: ...
//               $p <-- ...       #S4 (pos: 50)
//               ...
//
//       Original lifetime for $p: <2-49><50-67>
//       | ------------------------------------------------------------------
//       |                   u         u         u         d                u
//                           ^         ^         ^         ^
//                           |         |         |         |
//                    pos_of_#S1  pos_of_#S2  pos_of_#S3  pos_of_#S4
//
//   Different processing orders of the three fake-use IRs will be showed in
//   the following three cases:
//
//     CASE1 (incorrect):
//       Processing order for these fake-use IRs:
//             #S1 --> #S2 --> #S3
//
//       Modified lifetime for $p: <40-49><50-67>
//       |                                       ----------------------------
//       |                   u         u         u         d                u
//                           ^         ^         ^         ^
//                           |         |         |         |
//                    pos_of_#S1  pos_of_#S2  pos_of_#S3  pos_of_#S4
//
//       The final start position of lifetime is 40, which doses not include the
//       position of #S1 (20) and #S2 (30).
//
//
//     CASE2 (incorrect):
//       Processing order for these fake-use IRs:
//             #S1 --> #S3 --> #S2
//
//       Modified lifetime for $p: <30-49><50-67>
//       |                             --------------------------------------
//       |                   u         u         u         d                u
//                           ^         ^         ^         ^
//                           |         |         |         |
//                    pos_of_#S1  pos_of_#S2  pos_of_#S3  pos_of_#S4
//
//       The final start position of lifetime is 30, which doses not include the
//       position of #S1 (20).
//
//
//     CASE3 (correct):
//       Processing order for these fake-use IRs:
//             #S2 --> #S3 --> #S1
//
//       Modified lifetime for $p: <20-49><50-67>
//       |                   ------------------------------------------------
//       |                   u         u         u         d                u
//                           ^         ^         ^         ^
//                           |         |         |         |
//                    pos_of_#S1  pos_of_#S2  pos_of_#S3  pos_of_#S4
//
//       The final start position of lifetime is 20, which can cover all the
//       position of fake-use IRs related to $p.
//
//
//   Conclusion: Only #S1 need to be processed, while #S2 and #S3 need to be
//               ignored.
//
void PosAttrLifeTimeProc::preProcess()
{
    //Use a temp map to store the pos info for PRs.
    PRNO2Pos prno2pos;

    BBPos2AttrIter it;
    PosAttr const* attr = nullptr;
    for (BBPos pos = m_pos2attr.get_first(it, &attr); !it.end();
         pos = m_pos2attr.get_next(it, &attr)) {
        //Only process the attribute POS_ATTR_LT_SHRINK_BEFORE.
        //For attribute POS_ATTR_LT_NO_TERM_AFTER is used to extend the first
        //range of lifetime to the next definition, if the range was extended
        //by any one of the fake-use IRs, the other fake-use IRs will make
        //no changes for the range, that is as expected.
        if (!attr->have(POS_ATTR_LT_SHRINK_BEFORE)) { continue; }
        IR * rhs = const_cast<IR*>(attr->getIR())->getRHS();
        ASSERT0(rhs && rhs->is_pr() && (rhs->getPrno() == BBPOS_prno(pos)));
        PRNO prno = rhs->getPrno();
        IRBB const* bb = attr->getBB();
        ASSERT0(BBPOS_bbid(pos) == bb->id());
        //Normally this position should be at the entry of BB.
        ASSERT0(BBPOS_flag(pos) == INSERT_MODE_HEAD);
        Pos bb_entry_pos = m_lsra->getLTMgr().getBBStartPos(bb->id());
        recordPosInfo(prno2pos, prno, bb_entry_pos, attr);
    }

    PRNO2PosIter iter;
    PosInfo * pos_info = nullptr;
    for (BBPos pos = prno2pos.get_first(iter, &pos_info); !iter.end();
         pos = prno2pos.get_next(iter, &pos_info)) {
        const_cast<PosAttr*>(pos_info->attr_first)->setSequence(BB_SEQ_FIRST);
    }
}


bool PosAttrLifeTimeProc::processAttr(BBPos const& pos, PosAttr const* attr)
{
    ASSERT0(attr);
    if (!processAttrLTNoTerminatedAfter(pos, attr)) { return false; }
    if (!processAttrLTShrinkedBefore(pos, attr)) { return false; }
    if (!processAttrLTExtendBBEnd(pos, attr)) { return false; }
    return true;
}


bool PosAttrLifeTimeProc::checkRelatedAttrs(PosAttr const* attr) const
{
    ASSERT0(attr);
    return attr->have(POS_ATTR_LT_NO_TERM_AFTER) ||
        attr->have(POS_ATTR_LT_SHRINK_BEFORE) ||
        attr->have(POS_ATTR_LT_EXTEND_BB_END);
}


bool PosAttrLifeTimeProc::processAttrLTNoTerminatedAfter(BBPos const& pos,
                                                         PosAttr const* attr)
{
    ASSERT0(attr);
    if (!attr->have(POS_ATTR_LT_NO_TERM_AFTER)) { return true; }
    IR * tmp_ir = const_cast<IR*>(attr->getIR());

    //Check the fake-use IR responding at the BBPos.
    ASSERT0(tmp_ir && tmp_ir->is_st());
    IR * rhs = tmp_ir->getRHS();
    ASSERT0(rhs && rhs->is_pr() && (rhs->getPrno() == BBPOS_prno(pos)));
    IRBB const* bb = attr->getBB();
    ASSERT0(bb && BBPOS_bbid(pos) == bb->id());
    //Normally this position should be at the head of BB.
    ASSERT0(BBPOS_flag(pos) == INSERT_MODE_HEAD);

    //Extend the current lifetime through the steps below:
    // 1. Find the accurate start pos of BB from lifetime manager.
    // 2. Find the live range including the start pos of BB in lifetime, because
    //    this pr is at the head of BB before the first real occurence, so the
    //    live range must be the first one.
    // 3. Find the next range, extend the current life range to the next DEF if
    //    there is a hole.
    // e.g:
    //   lifetime original: <2-17><34-67>
    //                bb_start_pos = 16
    //                    |
    //                    V
    //    | ----------------                ----------------------------------
    //    |                u                d      u           u             u
    //   lifetime modified: <2-33><34-67>
    //    | ------------------------------------------------------------------
    //    |                u                d      u           u             u
    LifeTime * lt = m_lsra->getLTMgr().getLifeTime(rhs->getPrno());
    Pos cur_bb_entry_pos = m_lsra->getLTMgr().getBBStartPos(bb->id());
    for (VecIdx i = 0; i <= lt->getRangeVec().get_last_idx(); i++) {
        Range r = lt->getRange(i);
        if (!r.is_contain(cur_bb_entry_pos)) { continue; }
        //Normally this should be the first range in the lifetime.
        ASSERT0(i == 0);
        VecIdx next = i + 1;
        if (next <= lt->getRangeVec().get_last_idx()) {
            Range next_r = lt->getRange(next);
            //There is a hole between current range and next range, extend
            //the lifetime to the next definition.
            if (next_r.start() > r.end() + 1) {
                RG_end(r) = next_r.start() - 1;
                lt->setRange(i, r);
            }
        }
        break;
    }
    return true;
}


bool PosAttrLifeTimeProc::processAttrLTShrinkedBefore(BBPos const& pos,
                                                      PosAttr const* attr)
{
    ASSERT0(attr);
    if (!attr->have(POS_ATTR_LT_SHRINK_BEFORE)) { return true; }
    IR * tmp_ir = const_cast<IR*>(attr->getIR());
    IR * rhs = tmp_ir->getRHS();
    ASSERT0(rhs && rhs->is_pr() && (rhs->getPrno() == BBPOS_prno(pos)));
    IRBB const* bb = attr->getBB();
    ASSERT0(BBPOS_bbid(pos) == bb->id());
    //Normally this position should be at the head of BB.
    ASSERT0(BBPOS_flag(pos) == INSERT_MODE_HEAD);

    //Only process the fake-use IR related to the same PR with the flag
    //BB_SEQ_FIRST.
    // For example:
    //    In the below CFG, there are three fake-use IRs about $p, #S1 is flaged
    //    with BB_SEQ_FIRST, and it is the only fake-use IR need to be processed
    //    for attribiute POS_ATTR_LT_SHRINK_BEFORE.
    //
    //           BB1: ...
    //                 |
    //                 V
    //           BB2: fake_var <-- $p  #S1 (Entry pos: 20)
    //                ...
    //                 |
    //                 V
    //           BB3: fake_var <-- $p  #S2 (Entry pos: 30)
    //                ...
    //                 |
    //                 V
    //           BB4: fake_var <-- $p  #S3 (Entry pos: 40)
    //                ...
    //                 |
    //                 V
    //           BB5: ...
    //
    //    lifetime for $p original: <2-49><50-67>
    //    | ------------------------------------------------------------------
    //    |                    u         u        u            d             u
    //
    //    The first range of lifetime will be terminated at the entry of BB2.
    //
    //    lifetime for $p modified: <20-49><50-67>
    //    |                    -----------------------------------------------
    //    |                    u         u        u            d             u
    if (attr->getSequence() != BB_SEQ_FIRST) { return true; }

    //Terminate the current lifetime before the BBPos through the steps below:
    // 1. Find the accurate position of BB entry from lifetime manager.
    // 2. Get the first live range in the lifetime, the lifetime is extended
    //    from the entry of function due to the fake-use PR.
    // 3. Update the start of the live range to the position of BB entry.
    //
    // e.g:
    //   lifetime original: <2-17><34-67>
    //               bb_start_pos = 16
    //                    |
    //                    V
    //    | ----------------                ----------------------------------
    //    |                u                d      u           u             u
    //                     ^
    //                     |
    //                   occ_pos = 17
    //
    //   lifetime modified: <16-17><34-67>
    //    |               --                ----------------------------------
    //    |                u                d      u           u             u
    LifeTime * lt = m_lsra->getLTMgr().getLifeTime(rhs->getPrno());
    Pos cur_bb_entry_pos = m_lsra->getLTMgr().getBBStartPos(bb->id());
    Range r = lt->getFirstRange();
    Pos const region_start = REGION_START_POS;

    //Shrink the current live range to the position of BB entry.
    if (RG_start(r) == region_start && r.is_contain(cur_bb_entry_pos)) {
        RG_start(r) = cur_bb_entry_pos;
        lt->setRange(0, r);
    }
    return true;
}


bool PosAttrLifeTimeProc::processAttrLTExtendBBEnd(BBPos const& pos,
                                                   PosAttr const* attr)
{
    ASSERT0(attr);
    if (!attr->have(POS_ATTR_LT_EXTEND_BB_END)) { return true; }
    IR * tmp_ir = const_cast<IR*>(attr->getIR());
    IRBB const* bb = attr->getBB();
    ASSERT0(bb && BBPOS_bbid(pos) == bb->id());

    //Normally this position should be at the tail of BB.
    ASSERT0(BBPOS_flag(pos) == INSERT_MODE_TAIL);

    //Extend the last live range to the end of BB by following steps:
    //1. Get the last live range.
    //2. If the end position of last range is after the BB end, then do nothing.
    //3. Update the the end position of last range to the BB end position.
    Pos bb_end_pos = m_lsra->getLTMgr().getBBEndPos(bb->id());
    IR * rhs = tmp_ir->getRHS();
    ASSERT0(rhs && rhs->is_pr() && (rhs->getPrno() == BBPOS_prno(pos)));
    LifeTime * lt = m_lsra->getLTMgr().getLifeTime(rhs->getPrno());
    Range r = lt->getLastRange();
    if (RG_end(r) >= bb_end_pos) { return true; }
    RG_end(r) = bb_end_pos;
    lt->setLastRange(r);
    return true;
}
//END PosAttrLifeTimeProc


class OccRecorderVF {
    BackwardJumpAnalysis * m_ana;
public:
    Pos pos;
public:
    OccRecorderVF(BackwardJumpAnalysis * ana) : m_ana(ana), pos(POS_UNDEF) {}
    bool visitIR(IR const* ir, OUT bool & is_term)
    {
        if (ir->is_pr() || ir->is_stpr()) {
            m_ana->recordOccurenceForPR(ir->getPrno(), pos);
        }
        return true;
    }
};


//
//START OccRecorder
//
class OccRecorder : public VisitIRTree<OccRecorderVF> {
    COPY_CONSTRUCTOR(OccRecorder);
public:
    OccRecorder(IR const* ir, OccRecorderVF & vf) : VisitIRTree(vf)
    {
        ASSERT0(ir);
        visit(ir);
    }
};
//END OccRecorder


//
//START BackwardJumpAnalysisResMgr
//
BackwardJumpAnalysisResMgr::BackwardJumpAnalysisResMgr()
{
    UINT const mem_pool_init_size = 64;
    m_pool = smpoolCreate(mem_pool_init_size, MEM_COMM);
    ASSERT0(m_pool);
}


BackwardJumpAnalysisResMgr::~BackwardJumpAnalysisResMgr()
{
    if (m_pool != nullptr) {
        smpoolDelete(m_pool);
        m_pool = nullptr;
    }
}


void * BackwardJumpAnalysisResMgr::xmalloc(size_t size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset(p, 0, size);
    return p;
}


PosAttr * BackwardJumpAnalysisResMgr::genPosAttr(UINT v, IRBB const* bb,
                                                 IR const* ir)
{
    ASSERT0(bb && ir);
    PosAttr * pos_attr = (PosAttr*)xmalloc(sizeof(PosAttr));
    pos_attr->init(v, bb, ir);
    return pos_attr;
}


BackwardEdge * BackwardJumpAnalysisResMgr::genBackwardEdge(IRBB const* srcbb,
                                                           IRBB const* dstbb)
{
    ASSERT0(srcbb && dstbb);
    BackwardEdge * e = (BackwardEdge*)xmalloc(sizeof(BackwardEdge));
    e->init(srcbb, dstbb);
    return e;
}
//END BackwardJumpAnalysisResMgr

//
//START FakeVarMgr
//
Var * FakeVarMgr::genFakeVar(Type const* ty)
{
    ASSERT0(ty);
    // The fake-use IR can use the same fake var as a flag.
    if (ty->is_vector()) {
        if (m_fake_vec_var != nullptr) { return m_fake_vec_var; }
        m_fake_vec_var = REGION_region_mgr(m_rg)->getVarMgr()->registerVar(
            "fake_vec_var", ty, 1, VAR_LOCAL | VAR_FAKE);
        return m_fake_vec_var;
    }
    if (m_fake_scalar_var == nullptr) {
        m_fake_scalar_var = REGION_region_mgr(m_rg)->getVarMgr()->registerVar(
            "fake_scalar_var", ty, 1, VAR_LOCAL | VAR_FAKE);
    }
    return m_fake_scalar_var;
}
//END FakeVarMgr

//
//START BackwardJumpAnalysis
//
BackwardJumpAnalysis::BackwardJumpAnalysis(Region * rg, BBPos2Attr * pos2attr,
                                           FakeVarMgr * fake_var_mgr,
                                           LinearScanRA * lsra)
{
    ASSERT0(rg && pos2attr && fake_var_mgr && lsra);
    m_rg = rg;
    m_pos2attr = pos2attr;
    m_bb_list = nullptr;
    m_irmgr = rg->getIRMgr();
    m_live_mgr = nullptr;
    m_fake_var_mgr = fake_var_mgr;
    m_resource_mgr = nullptr;
    m_lsra = lsra;
    init();
}


BackwardJumpAnalysis::~BackwardJumpAnalysis()
{
    destroy();
}


void BackwardJumpAnalysis::reset()
{
    destroy();
    init();
}


void BackwardJumpAnalysis::init()
{
    m_pos2attr->init();
    m_prno2occ.init();
    m_pr2fakeuse_head.init();
    m_pr2fakeuse_tail.init();
    m_backward_edges.init();
    if (m_resource_mgr == nullptr) {
        m_resource_mgr = new BackwardJumpAnalysisResMgr();
    }
}


void BackwardJumpAnalysis::destroy()
{
    m_pos2attr->destroy();
    m_backward_edges.destroy();
    m_prno2occ.clean();
    m_bb_entry_pos.clean();
    m_bb_exit_pos.clean();
    m_pr2fakeuse_head.clean();
    m_pr2fakeuse_tail.clean();
    if (m_resource_mgr != nullptr) {
        delete m_resource_mgr;
        m_resource_mgr = nullptr;
    }
}


void BackwardJumpAnalysis::recordOccurenceForPR(PRNO prno, Pos pos)
{
    //The occurence information of each prno contains the first position
    //and the last position only in the IRBB List. The last position need to be
    //updated if the previous record position is less than the new position.
    //Here is an example:
    // prno pos:|1   4   7  9  12 13|
    //          | ----   ---    --  |
    //
    // the final occ is: first = 1, last = 13
    ASSERT0(prno != PRNO_UNDEF);
    bool find = false;
    Occurence * occ = const_cast<Occurence*>(m_prno2occ.get(prno, &find));
    if (find) {
        ASSERT0(occ);
        ASSERT0(OCC_first(occ) != POS_UNDEF);
        OCC_last(occ) = MAX(OCC_last(occ), pos);
        m_prno2occ.setAlways(prno, occ);
        return;
    }

    occ = m_resource_mgr->genOccurence();
    ASSERT0(occ);
    OCC_first(occ) = pos;
    OCC_last(occ) = pos;
    m_prno2occ.set(prno, occ);
}


void BackwardJumpAnalysis::collectBackwardJumps()
{
    BBListIter bbit;
    for (IRBB const* bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        AdjVertexIter ito;
        for (Vertex const* o = Graph::get_first_out_vertex(bb->getVex(), ito);
             o != nullptr; o = Graph::get_next_out_vertex(ito)) {
            if (!m_lsra->isBackwardJump(bb->id(), o->id())) { continue; }
            addBackwardJump(bb, m_rg->getBB(o->id()));
        }
    }
}


void BackwardJumpAnalysis::generateOccurenceForBB(IRBB const* bb, MOD Pos & pos)
{
    ASSERT0(bb);
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();

    //If the irlist is empty, return directly.
    if (irlst.is_empty()) { return; }
    BBIRListIter bbirit;
    m_bb_entry_pos.set(bb->id(), pos + 1);
    OccRecorderVF vf(this);
    for (IR * ir = irlst.get_head(&bbirit); ir != nullptr;
         ir = irlst.get_next(&bbirit)) {
        vf.pos = ++pos;
        OccRecorder occurence_recorder(ir, vf);
    }
    m_bb_exit_pos.set(bb->id(), pos);
}


void BackwardJumpAnalysis::generateOccurence()
{
    BBListIter bbit;
    Pos pos = POS_UNDEF;
    for (IRBB * bb = m_bb_list->get_head(&bbit); bb != nullptr;
         bb = m_bb_list->get_next(&bbit)) {
        generateOccurenceForBB(bb, pos);
    }
}


bool BackwardJumpAnalysis::analyze()
{
    START_TIMER_FMT(t, ("Backward Jump Analysis"));
    m_live_mgr = (LivenessMgr*)m_rg->getPassMgr()->queryPass(PASS_LIVENESS_MGR);
    m_bb_list = m_rg->getBBList();
    if (m_bb_list == nullptr || m_bb_list->get_elem_count() == 0) {
        END_TIMER_FMT(t, ("Backward Jump Analysis"));
        return true;
    }

    //Step1: Collect the backward jump info in the CFG.
    collectBackwardJumps();

    //If there is no backward jump, nothing need to do.
    if (m_backward_edges.get_elem_count() == 0) {
        END_TIMER_FMT(t, ("Backward Jump Analysis"));
        return true;
    }

    //Step2: Generate the simple occurence info for the relative live PRs.
    generateOccurence();

    //Step3: Generate the fake-use IR info.
    generateFakeUse();

    //Step4: Insert the fake-use IR.
    insertFakeUse();
    END_TIMER_FMT(t, ("Backward Jump Analysis"));
    return true;
}


void BackwardJumpAnalysis::dump()
{
    note(m_rg, "\n==-- DUMP BackwardJumpAnalysis --==");

    Prno2OccMapIter it;
    Occurence const* occ = nullptr;
    note(m_rg, "\nPR occ interval:\n");
    for (PRNO pr = m_prno2occ.get_first(it, &occ); occ != nullptr;
         pr = m_prno2occ.get_next(it, &occ)) {
        ASSERT0(OCC_first(occ) != POS_UNDEF);
        note(m_rg, "PR %u, [%u, %u]\n", pr, OCC_first(occ), OCC_last(occ));
    }

    note(m_rg, "\nBackward jump BB interval:\n");
    for (BackwardEdge const* e = m_backward_edges.get_head();
         e != nullptr; e = m_backward_edges.get_next()) {
        UINT src_bb = BKEDGE_srcbb(e)->id();
        UINT dst_bb = BKEDGE_dstbb(e)->id();
        note(m_rg, "SRC bb: %u [%u, %u] ----> DST bb: %u [%u, %u]\n",
             src_bb, m_bb_entry_pos[src_bb], m_bb_exit_pos[src_bb],
             dst_bb, m_bb_entry_pos[dst_bb], m_bb_exit_pos[dst_bb]);
    }

    note(m_rg, "\nfake-use IR insert details:\n");
    BBPos2AttrIter iter;
    PosAttr const* attr = nullptr;
    for (BBPos pos = m_pos2attr->get_first(iter, &attr); !iter.end();
         pos = m_pos2attr->get_next(iter, &attr)) {
        ASSERT0(attr);
        UINT bbid = BBPOS_bbid(pos);
        UINT insert_flag = BBPOS_flag(pos);
        UINT prno = BBPOS_prno(pos);
        note(m_rg, "insert fake IR for prno %u in BB %u at %s\n", prno, bbid,
             (insert_flag == INSERT_MODE_TAIL) ? "tail" : "head");
    }
}


//This func inserts the fake-use IR at BB entry per the steps below:
//  1. Build a fake-use IR by write the pr into a fake var.
//  2. Insert the fake-use IR at the entry of BB.
//  3. Map the pos and the new fake-use IR with three attributes:
//        --- POS_ATTR_NO_CODE_GEN
//        --- POS_ATTR_LT_NO_TERM_AFTER
//        --- POS_ATTR_LT_SHRINK_BEFORE
void BackwardJumpAnalysis::insertFakeUseAtBBEntry(IRBB const* bb, PRNO prno,
                                                  BBPos const& pos)
{
    ASSERT0(bb);
    ASSERT0(prno != PRNO_UNDEF);
    Type const* ty = m_lsra->getRegType(prno);
    IR * pr_ir = m_irmgr->buildPRdedicated(prno, ty);
    IR * st = m_irmgr->buildStore(m_fake_var_mgr->genFakeVar(ty), ty, pr_ir);
    st->setBB(const_cast<IRBB*>(bb));
    const_cast<IRBB*>(bb)->getIRList().append_head(st);
    m_lsra->setFakeUseAtLexFirstBBInLoop(st);

    //POS_ATTR_LT_SHRINK_BEFORE is used to avoid the lifetime of PR is from the
    //entry of region because the first occurence of this PR is a USE.
    //e.g. the lifetime of a PR
    //   Original lifetime: <2-17><34-67>
    //    | ----------------                ----------------------------------
    //    |                u                d      u           u             u
    //
    //   After POS_ATTR_LT_SHRINK_BEFORE is processed:
    //
    //   Modified lifetime: <17><34-67>
    //    |                -                ----------------------------------
    //    |                u                d      u           u             u
    PosAttr * attr = m_resource_mgr->genPosAttr(POS_ATTR_NO_CODE_GEN |
        POS_ATTR_LT_NO_TERM_AFTER | POS_ATTR_LT_SHRINK_BEFORE, bb, st);
    setPosAttr(pos, attr, m_pos2attr);
    dumpFakeUse(m_lsra, st, nullptr, prno, bb->id(), true,
                "backward jump analysis");
}


//This func inserts the fake-use IR at BB exit per the steps below:
//  1. Build a fake-use IR by write the pr into a fake var.
//  2. Insert the fake-use IR at the tail of BB, but before the branch IR.
//  3. Map the pos and the new fake-use IR with attribute POS_ATTR_NO_CODE_GEN.
void BackwardJumpAnalysis::insertFakeUseAtBBExit(IRBB const* bb, PRNO prno,
                                                 BBPos const& pos)
{
    ASSERT0(bb);
    ASSERT0(prno != PRNO_UNDEF);
    IR const* tail = const_cast<IRBB*>(bb)->getIRList().get_tail();
    ASSERT0(tail);
    if (!tail->isBranch()) { return; }

    //Insert before the branch.
    Type const* ty = m_lsra->getRegType(prno);
    IR * pr_ir = m_irmgr->buildPRdedicated(prno, ty);
    IR * st = m_irmgr->buildStore(m_fake_var_mgr->genFakeVar(ty), ty, pr_ir);
    st->setBB(const_cast<IRBB*>(bb));
    const_cast<IRBB*>(bb)->getIRList().insert_before(st, tail);

    //Store the fake-use IR in the table, because the fake-use IR at the
    //last BB of loop by lexicographical order will not particaipated into
    //the register allocation, and this table will be used to identify such
    //kind of fake-use IR.
    m_lsra->setFakeUseAtLexLastBBInLoop(st);

    //Attribute POS_ATTR_LT_EXTEND_BB_END is set, because the multiple fake-use
    //IRs are inserted at the tail part of the same BB, which would lead to
    //this lifetime cannot live to the real ending position of BB.
    PosAttr * attr = m_resource_mgr->genPosAttr(
        POS_ATTR_NO_CODE_GEN | POS_ATTR_LT_EXTEND_BB_END, bb, st);
    setPosAttr(pos, attr, m_pos2attr);
    dumpFakeUse(m_lsra, st, nullptr, prno, bb->id(), false,
                "backward jump analysis");
}


void BackwardJumpAnalysis::insertFakeUse(IRBB const* bb, PRNO prno,
                                         INSERT_MODE mode)
{
    ASSERT0(bb);
    ASSERT0(mode != INSERT_MODE_UNDEF);
    ASSERT0(prno != PRNO_UNDEF);

    //Generate a customerized number to represent the position. This number
    //is unique for any pr and bbid with a sepcified INSERT_MODE, which
    //could avoid the duplicate insert for the same IR at a same BB.
    BBPos bbpos(prno, bb->id(), mode);

    //Check the duplicated insertion. At the moment, duplicated insertion for
    //same fake-use pr at the same BB position is meaningless.
    if (m_pos2attr->find(bbpos)) { return; }

    switch (mode) {
    case INSERT_MODE_HEAD: insertFakeUseAtBBEntry(bb, prno, bbpos); break;
    case INSERT_MODE_TAIL: insertFakeUseAtBBExit(bb, prno, bbpos); break;
    default: UNREACHABLE(); break;
    }
}


void BackwardJumpAnalysis::recordFakeUse(PRNO prno, IRBB const* bb,
                                         INSERT_MODE mode)
{
    ASSERT0(bb);
    ASSERT0(prno != PRNO_UNDEF);
    UINT bbid = bb->id();
    ASSERT0(bbid != BBID_UNDEF);

    if (mode == INSERT_MODE_HEAD) {
        bool find = false;
        FakeUse * fakeuse = m_pr2fakeuse_head.get(prno, &find);
        if (!find) {
            fakeuse = m_resource_mgr->genFakeUse();
            FAKEUSE_bb(fakeuse) = bb;
            FAKEUSE_pos(fakeuse) = m_bb_entry_pos[bbid];
            m_pr2fakeuse_head.set(prno, fakeuse);
            return;
        }
        if (m_bb_entry_pos[bbid] >= FAKEUSE_pos(fakeuse)) { return; }
        FAKEUSE_bb(fakeuse) = bb;
        FAKEUSE_pos(fakeuse) = m_bb_exit_pos[bbid];
        return;
    }

    if (mode == INSERT_MODE_TAIL) {
        bool find = false;
        FakeUse * fakeuse = m_pr2fakeuse_tail.get(prno, &find);
        if (!find) {
            fakeuse = m_resource_mgr->genFakeUse();
            FAKEUSE_bb(fakeuse) = bb;
            FAKEUSE_pos(fakeuse) = m_bb_exit_pos[bbid];
            m_pr2fakeuse_tail.set(prno, fakeuse);
            return;
        }
        if (m_bb_exit_pos[bbid] <= FAKEUSE_pos(fakeuse)) { return; }
        FAKEUSE_bb(fakeuse) = bb;
        FAKEUSE_pos(fakeuse) = m_bb_exit_pos[bbid];
        return;
    }
    UNREACHABLE();
}


void BackwardJumpAnalysis::generateFakeUse()
{
    for (BackwardEdge const* e = m_backward_edges.get_head();
         e != nullptr; e = m_backward_edges.get_next()) {
        IRBB const* src_bb = BKEDGE_srcbb(e);
        IRBB const* dst_bb = BKEDGE_dstbb(e);
        ASSERT0(src_bb && dst_bb);

        //For the source BB of the backward jump edge, prno of the live out
        //need to be processed. If the last occurence of the pr is before the
        //end of BB, the fake-use of pr will be inserted at the tail of source
        //BB.
        // e.g case: the last occurence of the p1 is before the end of BB4.
        //
        //                    BB0:int p1 =
        //                        |
        //                        v
        //      |----------->  BB1:p1 < XX
        //      |                |     |
        //      |           _____|     |
        //      |           |          |
        //      |           v          v
        //      |       BB2:p1 =  | BB3:ret
        //      |            |
        //      |            v
        //      |          BB4:...
        //      |              int p2 =
        //      |              ...
        //      |                <-------- insert 'fake_var = p1' here.
        //      |______________goto BB1
        PRLiveSet const* live_out = m_live_mgr->get_liveout(src_bb->id());
        ASSERT0(live_out);
        PRLiveSetIter * iter = nullptr;
        for (PRNO pr = (PRNO)live_out->get_first(&iter);
             pr != BS_UNDEF; pr = (PRNO)live_out->get_next(pr, &iter)) {
            if (m_lsra->isDedicated(pr) &&
                m_lsra->getDedicatedReg(pr) == m_lsra->getZero()) {
                continue;
            }
            Occurence const* occ = m_prno2occ.get(pr);
            ASSERT0(occ);
            //Since some new fake-use IRs will be inserted at the end of the
            //src BB, so the exit boundary of src BB should be included.
            if (OCC_last(occ) < m_bb_exit_pos[src_bb->id()]) {
                recordFakeUse(pr, src_bb, INSERT_MODE_TAIL);
            }
        }

        //For the destination BB of the backward jump edge, prno of the live in
        //need to be processed. If the first occurence of the pr is after the
        //end of BB, the fake-use IR of pr will be inserted at the head of
        //destination BB.
        // e.g case: first occurence of the p0 is after the entry of BB1.
        //
        //                    BB0:int p0
        //                        int p1 =
        //                        |
        //                        v
        //      |-----------> BB1:  <-------- insert 'fake_var = p0' here.
        //      |                 ...
        //      |                 int p2 =
        //      |                 p1 < XX
        //      |                 |     |
        //      |           ______|     |
        //      |           |           |
        //      |           v           v
        //      |        BB2:p1 =    | BB3:ret
        //      |            p0 = p1
        //      |            |
        //      |            v
        //      |          BB4:...
        //      |              int p3 = p0
        //      |              p1 =
        //      |______________goto BB1
        PRLiveSet const* live_in = m_live_mgr->get_livein(dst_bb->id());
        ASSERT0(live_in);
        iter = nullptr;
        for (PRNO pr = (PRNO)live_in->get_first(&iter);
             pr != BS_UNDEF; pr = (PRNO)live_in->get_next(pr, &iter)) {
            Occurence const* occ = m_prno2occ.get(pr);
            ASSERT0(occ);
            if (m_lsra->getDedicatedMgr().isDedicated(pr)) { continue; }
            if (OCC_first(occ) > m_bb_exit_pos[src_bb->id()]) {
                //Ignore the PR if the first occ of PR is after the jump/loop,
                //which means this PR is not involved in the scope of current
                //jump/loop.
                continue;
            }
            //Since some new fake-use IRs will be inserted at the start of the
            //dst BB, so the entry boundary of the dst BB should be included.
            if (OCC_first(occ) >= m_bb_entry_pos[dst_bb->id()]) {
                recordFakeUse(pr, dst_bb, INSERT_MODE_HEAD);
            }
        }
    }
}


void BackwardJumpAnalysis::insertFakeUse()
{
    Prno2FakeUseIter it;
    FakeUse * fake = nullptr;
    for (PRNO pr = m_pr2fakeuse_head.get_first(it, &fake); fake != nullptr;
         pr = m_pr2fakeuse_head.get_next(it, &fake)) {
        insertFakeUse(FAKEUSE_bb(fake), pr, INSERT_MODE_HEAD);
    }

    for (PRNO pr = m_pr2fakeuse_tail.get_first(it, &fake); fake != nullptr;
         pr = m_pr2fakeuse_tail.get_next(it, &fake)) {
        insertFakeUse(FAKEUSE_bb(fake), pr, INSERT_MODE_TAIL);
    }
}
//END BackwardJumpAnalysis

//
//START RegSetImpl
//
RegSetImpl::RegSetImpl(LinearScanRA & ra) : m_ra(ra)
{
    m_target_callee_scalar = nullptr;
    m_target_caller_scalar = nullptr;
    m_target_param_scalar = nullptr;
    m_target_return_value_scalar = nullptr;
    m_target_callee_vector = nullptr;
    m_target_caller_vector = nullptr;
    m_target_param_vector = nullptr;
    m_target_return_value_vector = nullptr;
    m_target_allocable_scalar = nullptr;
    m_target_allocable_vector = nullptr;
}


TargInfoMgr & RegSetImpl::getTIMgr() const
{
    return m_ra.getTIMgr();
}


void RegSetImpl::destroyDebugRegSet()
{
    if (m_target_callee_scalar != nullptr) {
        delete m_target_callee_scalar;
        m_target_callee_scalar = nullptr;
    }
    if (m_target_caller_scalar != nullptr) {
        delete m_target_caller_scalar;
        m_target_caller_scalar = nullptr;
    }
    if (m_target_param_scalar != nullptr) {
        delete m_target_param_scalar;
        m_target_param_scalar = nullptr;
    }
    if (m_target_return_value_scalar != nullptr) {
        delete m_target_return_value_scalar;
        m_target_param_scalar = nullptr;
    }

    if (m_target_callee_vector != nullptr) {
        delete m_target_callee_vector;
        m_target_callee_vector = nullptr;
    }
    if (m_target_caller_vector != nullptr) {
        delete m_target_caller_vector;
        m_target_caller_vector = nullptr;
    }
    if (m_target_param_vector != nullptr) {
        delete m_target_param_vector;
        m_target_param_vector = nullptr;
    }
    if (m_target_return_value_vector != nullptr) {
        delete m_target_return_value_vector;
        m_target_return_value_vector = nullptr;
    }

    if (m_target_allocable_scalar != nullptr) {
        delete m_target_allocable_scalar;
        m_target_allocable_scalar = nullptr;
    }
    if (m_target_allocable_vector != nullptr) {
        delete m_target_allocable_vector;
        m_target_allocable_vector = nullptr;
    }
}


void RegSetImpl::destroyRegSet()
{
    if (!g_do_lsra_debug) { return; }
    destroyDebugRegSet();
}


void RegSetImpl::initRegSet()
{
    if (g_do_lsra_debug) {
        initDebugRegSet();
    } else {
        m_target_callee_scalar = getTIMgr().getCalleeScalarRegSet();
        m_target_caller_scalar = getTIMgr().getCallerScalarRegSet();
        m_target_param_scalar = getTIMgr().getParamScalarRegSet();
        m_target_return_value_scalar = getTIMgr().getRetvalScalarRegSet();
        m_target_allocable_scalar = getTIMgr().getAllocableScalarRegSet();
        m_target_callee_vector = getTIMgr().getCalleeVectorRegSet();
        m_target_caller_vector = getTIMgr().getCallerVectorRegSet();
        m_target_param_vector = getTIMgr().getParamVectorRegSet();
        m_target_return_value_vector = getTIMgr().getRetvalVectorRegSet();
        m_target_allocable_vector = getTIMgr().getAllocableVectorRegSet();
    }
    initAvailRegSet();
}


void RegSetImpl::initAvailRegSet()
{
    if (m_target_callee_scalar != nullptr) {
        m_avail_callee_scalar.copy(*m_target_callee_scalar);
    }
    if (m_target_caller_scalar != nullptr) {
        m_avail_caller_scalar.copy(*m_target_caller_scalar);
    }
    if (m_target_param_scalar != nullptr) {
        m_avail_param_scalar.copy(*m_target_param_scalar);
    }
    if (m_target_return_value_scalar != nullptr) {
        m_avail_return_value_scalar.copy(*m_target_return_value_scalar);
    }
    if (m_target_allocable_scalar != nullptr) {
        m_avail_allocable.bunion(*m_target_allocable_scalar);
    }
    if (m_target_callee_vector!= nullptr) {
        m_avail_callee_vector.copy(*m_target_callee_vector);
    }
    if (m_target_caller_vector != nullptr) {
        m_avail_caller_vector.copy(*m_target_caller_vector);
    }
    if (m_target_param_vector != nullptr) {
        m_avail_param_vector.copy(*m_target_param_vector);
    }
    if (m_target_return_value_vector != nullptr) {
        m_avail_return_value_vector.copy(*m_target_return_value_vector);
    }
    if (m_target_allocable_vector != nullptr) {
        m_avail_allocable.bunion(*m_target_allocable_vector);
    }
}

//Pick up a physical register from allocable register set.
Reg RegSetImpl::pickReg(RegSet & set)
{
    BSIdx i = set.get_first();
    if (i == BS_UNDEF) {
        return REG_UNDEF;
    }
    set.diff(i);
    return (Reg)i;
}


void RegSetImpl::pickReg(RegSet & set, Reg r)
{
    if (set.is_contain(r)) {
        set.diff(r);
    }
}


void RegSetImpl::pickRegFromAllocable(Reg reg)
{
    ASSERT0(isAvailAllocable(reg));
    return pickRegisterFromAliasSet(reg);
}


void RegSetImpl::recordUsedCallee(Reg reg)
{
    ASSERT0(isAvailAllocable(reg));
    ASSERT0(isCallee(reg));
    m_used_callee.bunion(reg);
}


void RegSetImpl::recordUsedCaller(Reg reg)
{
    ASSERT0(isAvailAllocable(reg));
    ASSERT0(isCaller(reg));
    m_used_caller.bunion(reg);
}


Reg RegSetImpl::pickCallee(IR const* ir)
{
    Reg r = ir->is_vec() ? pickReg(m_avail_callee_vector)
        : pickReg(m_avail_callee_scalar);
    return r;
}


Reg RegSetImpl::pickCaller(IR const* ir)
{
    Reg r = ir->is_vec() ? pickReg(m_avail_caller_vector)
        : pickReg(m_avail_caller_scalar);
    return r;
}


void RegSetImpl::pickRegisterFromAliasSet(Reg r)
{
    ASSERT0(isAvailAllocable(r));
    if (isCallee(r)) {
        return pickRegisterFromCalleeAliasSet(r);
    }
    if (isCaller(r)) {
        return pickRegisterFromCallerAliasSet(r);
    }
    if (isParam(r)) {
        return pickRegisterFromParamAliasSet(r);
    }
    if (isReturnValue(r)) {
        return pickRegisterFromReturnValueAliasSet(r);
    }
    ASSERT0(isSpecialReg(r));
}


void RegSetImpl::pickRegisterFromParamAliasSet(Reg r)
{
    pickReg(m_avail_param_scalar, r);
    pickReg(m_avail_param_vector, r);
}


void RegSetImpl::pickRegisterFromReturnValueAliasSet(Reg r)
{
    pickReg(m_avail_return_value_scalar, r);
    pickReg(m_avail_return_value_vector, r);
}


void RegSetImpl::pickRegisterFromCallerAliasSet(Reg r)
{
    pickReg(m_avail_caller_scalar, r);
    pickReg(m_avail_caller_vector, r);
}


void RegSetImpl::pickRegisterFromCalleeAliasSet(Reg r)
{
    pickReg(m_avail_callee_scalar, r);
    pickReg(m_avail_callee_vector, r);
}


bool RegSetImpl::isSpecialReg(Reg r) const
{
    return r == m_ra.getFP() || r == m_ra.getBP() || r == m_ra.getRA() ||
           r == m_ra.getSP() || r == m_ra.getGP() || r == m_ra.getTA() ||
           r == m_ra.getZero();
}


void RegSetImpl::freeRegisterFromAliasSet(Reg r)
{
    ASSERT0(isAvailAllocable(r));
    if (isCallee(r)) {
        return freeRegisterFromCalleeAliasSet(r);
    }
    if (isCaller(r)) {
        return freeRegisterFromCallerAliasSet(r);
    }
    if (isParam(r)) {
        return freeRegisterFromParamAliasSet(r);
    }
    if (isReturnValue(r)) {
        return freeRegisterFromReturnValueAliasSet(r);
    }
    ASSERT0(isSpecialReg(r));
}


void RegSetImpl::freeRegisterFromCallerAliasSet(Reg r)
{
    if (m_target_caller_scalar && m_target_caller_scalar->is_contain(r)) {
        m_avail_caller_scalar.bunion((BSIdx)r);
    }
    if (m_target_caller_vector && m_target_caller_vector->is_contain(r)) {
        m_avail_caller_vector.bunion((BSIdx)r);
    }
}


void RegSetImpl::freeRegisterFromCalleeAliasSet(Reg r)
{
    if (m_target_callee_scalar && m_target_callee_scalar->is_contain(r)) {
        m_avail_callee_scalar.bunion((BSIdx)r);
    }
    if (m_target_callee_vector && m_target_callee_vector->is_contain(r)) {
        m_avail_callee_vector.bunion((BSIdx)r);
    }
}


void RegSetImpl::freeRegisterFromParamAliasSet(Reg r)
{
    if (m_target_param_scalar && m_target_param_scalar->is_contain(r)) {
        m_avail_param_scalar.bunion((BSIdx)r);
    }
    if (m_target_param_vector && m_target_param_vector->is_contain(r)) {
        m_avail_param_vector.bunion((BSIdx)r);
    }
}


void RegSetImpl::freeRegisterFromReturnValueAliasSet(Reg r)
{
    if (m_target_return_value_scalar &&
        m_target_return_value_scalar->is_contain(r)) {
        m_avail_return_value_scalar.bunion((BSIdx)r);
    }
    if (m_target_return_value_vector&&
        m_target_return_value_vector->is_contain(r)) {
        m_avail_return_value_vector.bunion((BSIdx)r);
    }
}


void RegSetImpl::freeReg(Reg reg)
{
    return freeRegisterFromAliasSet(reg);
}


void RegSetImpl::freeReg(LifeTime const* lt)
{
    Reg reg = m_ra.getReg(lt->getPrno());
    ASSERT0(reg != REG_UNDEF);
    freeReg(reg);
}


void RegSetImpl::dumpAvailRegSet() const
{
    note(m_ra.getRegion(), "\n==-- DUMP AvaiableRegisterSet  --==");
    StrBuf buf(32);
    m_avail_caller_scalar.dump(buf);
    note(m_ra.getRegion(), "\nAVAIL_CALLER:%s", buf.buf);

    buf.clean();
    m_avail_callee_scalar.dump(buf);
    note(m_ra.getRegion(), "\nAVAIL_CALLEE:%s", buf.buf);

    buf.clean();
    m_avail_param_scalar.dump(buf);
    note(m_ra.getRegion(), "\nAVAIL_PARAM:%s", buf.buf);

    buf.clean();
    m_avail_return_value_scalar.dump(buf);
    note(m_ra.getRegion(), "\nAVAIL_RETURN_VALUE:%s", buf.buf);

    buf.clean();
    m_avail_allocable.dump(buf);
    note(m_ra.getRegion(), "\nAVAIL_ALLOCABLE:%s", buf.buf);
}
//END RegSetImpl


//
//START LinearScanRA
//
LinearScanRA::LinearScanRA(Region * rg) : Pass(rg), m_act_mgr(rg)
{
    ASSERT0(rg != nullptr);
    m_cfg = nullptr;
    m_bb_list = nullptr;
    m_irmgr = rg->getIRMgr();
    m_lt_mgr = new LifeTimeMgr(rg);
    m_func_level_var_count = 0;
    m_is_apply_to_region = false;
    m_is_fp_allocable_allowed = true;
}


LinearScanRA::~LinearScanRA()
{
    delete m_lt_mgr;
}


bool LinearScanRA::isCalleePermitted(LifeTime const* lt) const
{
    //The first priority allocable register set is callee-saved. Callee
    //is the best choose if lt crossed function-call as well.
    //The second priority allocable register set is caller-saved.
    //Note if User has used caller-saved register before, it should be spilled
    //before enter a function-call, and reload the reigster if it is used again
    //after the function.
    return true;
}


//Reset all resource before allocation.
void LinearScanRA::reset()
{
    getTIMgr().reset();
    getLTMgr().reset();
    m_prno2reg.clean();
    m_unhandled.clean();
    m_handled.clean();
    m_active.clean();
    m_inactive.clean();
    m_spill_tab.clean();
    m_reload_tab.clean();
    m_remat_tab.clean();
    m_move_tab.clean();
    m_prno2var.clean();
    m_act_mgr.clean();
    m_bb_seqid.clean();
    m_prno_with_2d_hole.clean();
}


void LinearScanRA::assignTopoIdForBB()
{
    //Iterate BB list.
    BBListIter bbit;
    UINT id = 0;
    for (IRBB * bb = m_bb_list->get_head(&bbit); bb != nullptr;
         bb = m_bb_list->get_next(&bbit)) {
        m_bb_seqid.set(bb->id(), id++);
    }
}


Var * LinearScanRA::getSpillLoc(PRNO prno)
{
    prno = getAnctPrno(prno);
    ASSERT0(prno != PRNO_UNDEF);
    return m_prno2var.get(prno);
}


Var * LinearScanRA::genSpillLoc(PRNO prno, Type const* ty)
{
    prno = getAnctPrno(prno);
    ASSERT0(prno != PRNO_UNDEF);
    Var * v = getSpillLoc(prno);
    if (v == nullptr) {
        //The alignment of vector register is greater than STACK_ALIGNMENT.
        v = genFuncLevelVar(ty, MAX(
            m_rg->getTypeMgr()->getByteSize(ty), STACK_ALIGNMENT));
        VAR_storage_space(v) = SS_STACK;
        m_prno2var.set(prno, v);
    }
    return v;
}


Var * LinearScanRA::getSpillLoc(Type const* ty)
{
    ASSERT0(ty);
    bool find = false;
    Var * v = m_ty2var.get(ty, &find);
    if (find) { return v; }
    //The alignment of vector register is greater than STACK_ALIGNMENT.
    v = genFuncLevelVar(ty, MAX(
        m_rg->getTypeMgr()->getByteSize(ty), STACK_ALIGNMENT));
    VAR_storage_space(v) = SS_STACK;
    m_ty2var.set(ty, v);
    return v;
}


PRNO LinearScanRA::buildPrnoDedicated(Type const* type, Reg reg)
{
    ASSERT0(type);
    ASSERT0(reg != REG_UNDEF);
    PRNO prno = getSpecialDedicatedPrno(type, reg);
    if (prno != PRNO_UNDEF) { return prno; }

    prno = m_irmgr->buildPrno(type);
    setDedicatedReg(prno, reg);
    recordSpecialDedicatedPrno(type, reg, prno);
    return prno;
}


PRNO LinearScanRA::buildPrno(Type const* type, Reg reg)
{
    ASSERT0(type);
    ASSERT0(reg != REG_UNDEF);
    PRNO prno = getSpecialDedicatedPrno(type, reg);
    if (prno != PRNO_UNDEF) { return prno; }

    prno = m_irmgr->buildPrno(type);
    setReg(prno, reg);
    recordSpecialDedicatedPrno(type, reg, prno);
    return prno;
}


IR * LinearScanRA::buildSpillByLoc(PRNO prno, Var * spill_loc, Type const* ty)
{
    ASSERT0(spill_loc && ty);
    ASSERT0(prno != PRNO_UNDEF);
    IR * pr = m_irmgr->buildPRdedicated(prno, ty);
    m_rg->getMDMgr()->allocRef(pr);
    IR * stmt = m_irmgr->buildStore(spill_loc, pr);
    m_rg->getMDMgr()->allocRef(stmt);
    m_rg->addToVarTab(spill_loc);
    stmt->setAligned(true);
    return stmt;
}


IR * LinearScanRA::buildSpill(PRNO prno, Type const* ty)
{
    ASSERT0(ty);
    ASSERT0(prno != PRNO_UNDEF);
    Var * spill_loc = genSpillLoc(prno, ty);
    ASSERT0(spill_loc);
    return buildSpillByLoc(prno, spill_loc, ty);
}


IR * LinearScanRA::buildReload(PRNO prno, Var * spill_loc, Type const* ty)
{
    ASSERT0(spill_loc && ty);
    ASSERT0(prno != PRNO_UNDEF);
    IR * ld = m_irmgr->buildLoad(spill_loc, ty);
    m_rg->getMDMgr()->allocRef(ld);
    IR * stmt = m_irmgr->buildStorePR(prno, ty, ld);
    m_rg->getMDMgr()->allocRef(stmt);
    ld->setAligned(true);
    return stmt;
}


IR * LinearScanRA::buildRemat(PRNO prno, RematCtx const& rematctx,
                              Type const* ty)
{
    ASSERT0(rematctx.material_exp);
    IR * e = m_rg->dupIRTree(rematctx.material_exp);
    m_rg->getMDMgr()->allocRefForIRTree(e, true);
    IR * stmt = m_irmgr->buildStorePR(prno, ty, e);
    m_rg->getMDMgr()->allocRef(stmt);
    return stmt;
}


IR * LinearScanRA::buildMove(PRNO from, PRNO to, Type const* fromty,
                             Type const* toty)
{
    IR * pr = m_irmgr->buildPRdedicated(from, fromty);
    m_rg->getMDMgr()->allocRef(pr);
    IR * stmt = m_irmgr->buildStorePR(to, toty, pr);
    m_rg->getMDMgr()->allocRef(stmt);
    return stmt;
}


void LinearScanRA::setReg(PRNO prno, Reg reg)
{
    ASSERT0(prno != PRNO_UNDEF);
    m_prno2reg.set(prno, reg);
}


bool LinearScanRA::hasReg(LifeTime const* lt) const
{
    return hasReg(lt->getPrno());
}


bool LinearScanRA::hasReg(PRNO prno) const
{
    return getReg(prno) != REG_UNDEF;
}


Type const* LinearScanRA::getRegType(PRNO prno) const
{
    Var * var = m_rg->getVarByPRNO(prno);
    ASSERT0(var);
    TypeMgr * tm = m_rg->getTypeMgr();
    if (!VAR_type(var)->is_vector() && !VAR_type(var)->is_any()) {
        ASSERT0(var->getByteSize(tm) <=
                tm->getByteSize(tm->getTargMachRegisterType()));
    }

    return VAR_type(var)->is_vector() ? VAR_type(var)
        : tm->getTargMachRegisterType();
}


CHAR const* LinearScanRA::getRegFileName(REGFILE rf) const
{
    return const_cast<LinearScanRA*>(this)->getTIMgr().getRegFileName(rf);
}


CHAR const* LinearScanRA::getRegName(Reg r) const
{
    return const_cast<LinearScanRA*>(this)->getTIMgr().getRegName(r);
}


Reg LinearScanRA::getReg(PRNO prno) const
{
    ASSERT0(prno != PRNO_UNDEF);
    return m_prno2reg.get(prno);
}


Reg LinearScanRA::getReg(LifeTime const* lt) const
{
    return getReg(lt->getPrno());
}


REGFILE LinearScanRA::getRegFile(Reg r) const
{
    LinearScanRA * pthis = const_cast<LinearScanRA*>(this);
    return pthis->getTIMgr().getRegFile(r);
}


LifeTime * LinearScanRA::getLT(PRNO prno) const
{
    return const_cast<LinearScanRA*>(this)->getLTMgr().getLifeTime(prno);
}


//The function check the uniquenuess of four LT list that used in RA.
bool LinearScanRA::verify4List() const
{
    xcom::TTab<Reg> used;
    xcom::TTab<LifeTime const*> visit;
    LTListIter it;
    for (LifeTime const* lt = m_unhandled.get_head(&it);
         lt != nullptr; lt = m_unhandled.get_next(&it)) {
        ASSERT0(!visit.find(lt));
        visit.append(lt);
    }
    for (LifeTime const* lt = m_active.get_head(&it);
         lt != nullptr; lt = m_active.get_next(&it)) {
        ASSERT0(!visit.find(lt));
        visit.append(lt);
        Reg r = getReg(lt);
        if (r == REG_UNDEF) { continue; }
        ASSERTN(!used.find(r), ("lt overlapped that has same register"));
        used.append(r);
    }
    for (LifeTime const* lt = m_inactive.get_head(&it);
         lt != nullptr; lt = m_inactive.get_next(&it)) {
        ASSERT0(!visit.find(lt));
        visit.append(lt);
        Reg r = getReg(lt);
        if (r == REG_UNDEF) { continue; }
        ASSERTN(!used.find(r), ("lt overlapped that has same register"));
        used.append(r);
    }
    for (LifeTime const* lt = m_handled.get_head(&it);
         lt != nullptr; lt = m_handled.get_next(&it)) {
        ASSERT0(!visit.find(lt));
        visit.append(lt);
    }
    return true;
}


//The function check whether 'lt' value is simple enough to rematerialize.
//And return the information through rematctx.
bool LinearScanRA::checkLTCanBeRematerialized(LifeTime const* lt,
                                              OUT RematCtx & rematctx)
{
    //Target Dependent Code.
    return false;
}


bool LinearScanRA::verifyAfterRA() const
{
    ASSERT0(m_unhandled.get_elem_count() == 0);
    ASSERT0(m_active.get_elem_count() == 0);
    ASSERT0(m_inactive.get_elem_count() == 0);
    BBListIter bbit;
    TypeMgr * tm = m_rg->getTypeMgr();
    for (IRBB const* bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        BBIRListIter bbirit;
        BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
        for (IR * ir = irlst.get_head(&bbirit); ir != nullptr;
            ir = irlst.get_next(&bbirit)) {
            if (!isSpillOp(ir) && !isReloadOp(ir)) { continue; }

            //Check the reload and spill only.
            ASSERT0(ir->getRHS());
            Type const* lhs_ty = ir->getType();
            Type const* rhs_ty = ir->getRHS()->getType();
            ASSERT0(lhs_ty && rhs_ty);
            UINT lhs_size = tm->getDTypeByteSize(lhs_ty->getDType());
            UINT rhs_size = tm->getDTypeByteSize(rhs_ty->getDType());
            ASSERT0(lhs_size >= rhs_size);
            if (lhs_size < rhs_size) { return false; }
        }
    }
    return true;
}


void LinearScanRA::dump4List() const
{
    note(m_rg, "\n==-- DUMP 4LIST --==");
    note(m_rg, "\nUNHANDLED:");
    UINT ind = 1;
    m_rg->getLogMgr()->incIndent(ind);
    LTSetIter it;
    LinearScanRA * pthis = const_cast<LinearScanRA*>(this);
    for (pthis->getUnhandled().get_head(&it); it != nullptr;
         pthis->getUnhandled().get_next(&it)) {
        LifeTime * lt = it->val();
        dumpPR2Reg(lt->getPrno());
        lt->dump(m_rg);
    }
    m_rg->getLogMgr()->decIndent(ind);

    note(m_rg, "\nHANDLED:");
    m_rg->getLogMgr()->incIndent(ind);
    for (pthis->getHandled().get_head(&it); it != nullptr;
         pthis->getHandled().get_next(&it)) {
        LifeTime * lt = it->val();
        dumpPR2Reg(lt->getPrno());
        lt->dump(m_rg);
    }
    m_rg->getLogMgr()->decIndent(ind);

    note(m_rg, "\nACTIVE:");
    m_rg->getLogMgr()->incIndent(ind);
    for (pthis->getActive().get_head(&it); it != nullptr;
         pthis->getActive().get_next(&it)) {
        LifeTime * lt = it->val();
        dumpPR2Reg(lt->getPrno());
        lt->dump(m_rg);
    }
    m_rg->getLogMgr()->decIndent(ind);

    note(m_rg, "\nINACTIVE:");
    m_rg->getLogMgr()->incIndent(ind);
    for (pthis->getInActive().get_head(&it); it != nullptr;
         pthis->getInActive().get_next(&it)) {
        LifeTime * lt = it->val();
        dumpPR2Reg(lt->getPrno());
        lt->dump(m_rg);
    }
    m_rg->getLogMgr()->decIndent(ind);
}


void LinearScanRA::dumpBBListWithReg() const
{
    class DumpPRWithReg : public IRDumpAttrBaseFunc {
    public:
        LinearScanRA const* lsra;
    public:
        virtual void dumpAttr(
            OUT xcom::StrBuf & buf, Region const* rg, IR const* ir,
            DumpFlag dumpflag) const override
        {
            if (!ir->isPROp()) { return; }
            Reg r = lsra->getReg(ir->getPrno());
            if (r == REG_UNDEF) { return; }
            buf.strcat(" (%s)", lsra->getRegName(r));
        }
    };
    DumpPRWithReg df;
    df.lsra = this;
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID | IR_DUMP_SRC_LINE |
        (g_dump_opt.isDumpIRID() ? IR_DUMP_IRID : 0));
    IRDumpCtx<> ctx(4, f, nullptr, &df);
    ASSERT0(m_rg->getBBList());
    xoc::dumpBBList(m_rg->getBBList(), m_rg, false, &ctx);
}


void LinearScanRA::dumpPR2Reg(PRNO p) const
{
    Reg r = getReg(p);
    LinearScanRA * pthis = const_cast<LinearScanRA*>(this);
    REGFILE rf = pthis->getTIMgr().getRegFile(r);
    LifeTime const* lt = getLT(p);
    if (lt != nullptr) {
        ASSERT0(lt->getPrno() == p);
        note(m_rg, "\nLT:$%u:%s(%s)", lt->getPrno(), getRegName(r),
             pthis->getTIMgr().getRegFileName(rf));
        return;
    }
    //PRNO without allocated a lifetime. The prno may be appeared as a
    //temporate pseduo register, e.g:the PR that indicates the region
    //livein callee-saved physical register.
    note(m_rg, "\n--:$%u:%s(%s)", p, getRegName(r),
         pthis->getTIMgr().getRegFileName(rf));
}


void LinearScanRA::dumpPR2Reg() const
{
    note(m_rg, "\n==-- DUMP PR2Reg --==");
    m_rg->getLogMgr()->incIndent(2);
    for (PRNO p = PRNO_UNDEF + 1; p < m_prno2reg.get_elem_count(); p++) {
        dumpPR2Reg(p);
    }
    m_rg->getLogMgr()->decIndent(2);
}


bool LinearScanRA::dump(bool dumpir) const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    //m_rg->getLogMgr()->incIndent(2);
    //---------
    LinearScanRA * pthis = const_cast<LinearScanRA*>(this);
    pthis->getTIMgr().dump(m_rg);
    m_dedicated_mgr.dump(m_rg, pthis->getTIMgr());
    UpdatePos up(*this);
    pthis->getLTMgr().dumpAllLT(up, m_bb_list, dumpir);
    dumpPR2Reg();
    dumpBBListWithReg();
    dump4List();
    m_act_mgr.dump();
    //---------
    Pass::dump();
    //m_rg->getLogMgr()->decIndent(2);
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


void LinearScanRA::addUnhandled(LifeTime * lt)
{
    if (m_unhandled.find(lt)) { return; }
    ASSERT0(!hasReg(lt));
    m_unhandled.append_tail(lt);
}


void LinearScanRA::addActive(LifeTime * lt)
{
    if (m_active.find(lt)) { return; }
    m_active.append_tail(lt);
}


void LinearScanRA::addInActive(LifeTime * lt)
{
    if (m_inactive.find(lt)) { return; }
    m_inactive.append_tail(lt);
}


void LinearScanRA::addHandled(LifeTime * lt)
{
    if (m_handled.find(lt)) { return; }
    ASSERT0(hasReg(lt));
    m_handled.append_tail(lt);
}


Var * LinearScanRA::genFuncLevelVar(Type const* type, UINT align)
{
    xcom::StrBuf name(64);
    Var * v = m_rg->getVarMgr()->registerVar(
        genFuncLevelNewVarName(name), type, align, VAR_LOCAL);
    return v;
}


CHAR const* LinearScanRA::genFuncLevelNewVarName(OUT xcom::StrBuf & name)
{
    name.sprint("func_level_var_%u", ++m_func_level_var_count);
    return name.buf;
}


void LinearScanRA::updateSSA(OptCtx & oc) const
{
    bool rmprdu = false;
    bool rmnonprdu = false;
    //TODO:update SSA incrementally.
    MDSSAMgr * mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->queryPass(
        PASS_MDSSA_MGR);
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->destruction(oc);
        mdssamgr->construction(oc);
        oc.setInvalidNonPRDU();
        rmprdu = true;
    }
    PRSSAMgr * prssamgr = (PRSSAMgr*)m_rg->getPassMgr()->queryPass(
        PASS_PRSSA_MGR);
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        prssamgr->destruction(oc);
        prssamgr->construction(oc);
        oc.setInvalidPRDU();
        rmnonprdu = true;
    }
    xoc::removeClassicDUChain(m_rg, rmprdu, rmnonprdu);
}


void LinearScanRA::collectDedicatedPR(BBList const* bblst,
                                      OUT DedicatedMgr & mgr)
{
    //Target Depedent Code.
    //e.g: designate $3 have to be allocate physical register REG-5.
    //mgr.add((PRNO)3, (Reg)5);
    DUMMYUSE(bblst);
    DUMMYUSE(mgr);
}


//The class switch IRBBMgr of given region.
class UseNewIRBBMgr {
    IRBBMgr * m_org_bbmgr;
    IRBBMgr * m_new_bbmgr;
    Region * m_rg;
public:
    UseNewIRBBMgr(Region * rg, IRBBMgr * bbmgr)
    {
        m_rg = rg;
        m_org_bbmgr = bbmgr;
        m_new_bbmgr = new IRBBMgr(rg);
        m_rg->setBBMgr(m_new_bbmgr);
    }
    ~UseNewIRBBMgr()
    {
        ASSERT0(m_org_bbmgr && m_new_bbmgr);
        m_rg->setBBMgr(m_org_bbmgr);
        delete m_new_bbmgr;
    }
    IRBBMgr * getNewIRBBMgr() const { return m_new_bbmgr; }
    IRBBMgr * getOrgIRBBMgr() const { return m_org_bbmgr; }
};


//The class switch IRMgr of given region.
class UseNewIRMgr {
    IRMgr * m_org_mgr;
    IRMgr * m_new_mgr;
    Region * m_rg;
public:
    UseNewIRMgr(Region * rg, IRMgr * irmgr)
    {
        m_rg = rg;
        m_org_mgr = irmgr;
        ASSERT0(m_rg->getPassMgr());
        m_new_mgr = (IRMgr*)m_rg->getPassMgr()->allocPass(PASS_IRMGR);
        m_new_mgr->setIRCount(m_org_mgr->getIRCount());
        m_rg->setIRMgr(m_new_mgr);
    }
    ~UseNewIRMgr()
    {
        ASSERT0(m_org_mgr && m_new_mgr);
        m_rg->setIRMgr(m_org_mgr);
        m_rg->getPassMgr()->destroyPass(m_new_mgr);
    }
    IRMgr * getNewIRMgr() const { return m_new_mgr; }
    IRMgr * getOrgIRMgr() const { return m_org_mgr; }
};


//The class switch BBList of given region.
//Note the class will clone new BB via given IRBBMgr.
class UseNewBBList {
    BBList * m_org_bblst;
    BBList * m_new_bblst;
    IRBBMgr * m_org_bbmgr;
    Region * m_rg;
public:
    UseNewBBList(Region * rg, BBList * bblst, MOD IRBBMgr * bbmgr)
    {
        ASSERT0(bbmgr);
        m_rg = rg;
        m_org_bblst = bblst;
        m_org_bbmgr = bbmgr;
        m_new_bblst = new BBList();
        m_new_bblst->clone(*bblst, bbmgr, rg);
        m_rg->setBBList(m_new_bblst);
    }
    ~UseNewBBList()
    {
        m_rg->setBBList(m_org_bblst);
        BBListIter it;
        for (IRBB * bb = m_new_bblst->get_head(&it);
             bb != nullptr; bb = m_new_bblst->get_next(&it)) {
            m_org_bbmgr->destroyBB(bb);
        }
        delete m_new_bblst;
    }
    BBList * getNewBBList() const { return m_new_bblst; }
    BBList * getOrgBBList() const { return m_org_bblst; }
};


//The class switch IRCFG of given region.
class UseNewCFG {
    IRCFG * m_org_cfg;
    IRCFG * m_new_cfg;
    Region * m_rg;
public:
    UseNewCFG(Region * rg, IRCFG * cfg, BBList * bblst)
    {
        m_rg = rg;
        m_org_cfg = cfg;
        ASSERT0(m_rg->getPassMgr());
        //m_new_cfg = (IRCFG*)m_rg->getPassMgr()->allocPass(PASS_CFG);
        m_new_cfg = new IRCFG(*cfg, bblst, false, false);
        m_new_cfg->setBBVertex();
        m_rg->setCFG(m_new_cfg);
    }
    ~UseNewCFG()
    {
        ASSERT0(m_org_cfg && m_new_cfg);
        m_rg->setCFG(m_org_cfg);
        m_new_cfg->setBBList(nullptr);
        delete m_new_cfg;
    }
    IRCFG * getNewCFG() const { return m_new_cfg; }
    IRCFG * getOrgCFG() const { return m_org_cfg; }
};


class ApplyToRegion {
    COPY_CONSTRUCTOR(ApplyToRegion);
    xcom::Stack<UseNewIRMgr*> m_irmgr_stack;
    xcom::Stack<UseNewIRBBMgr*> m_bbmgr_stack;
    xcom::Stack<UseNewBBList*> m_bblist_stack;
    xcom::Stack<UseNewCFG*> m_cfg_stack;
    Region * m_rg;
public:
    ApplyToRegion(Region * rg) { m_rg = rg; }
    ~ApplyToRegion()
    {
        for (; m_irmgr_stack.get_top() != nullptr;) {
            m_irmgr_stack.pop();
            m_bbmgr_stack.pop();
            m_bblist_stack.pop();
            m_cfg_stack.pop();
        }
        ASSERT0(m_irmgr_stack.get_top() == nullptr);
        ASSERT0(m_bbmgr_stack.get_top() == nullptr);
        ASSERT0(m_bblist_stack.get_top() == nullptr);
        ASSERT0(m_cfg_stack.get_top() == nullptr);
    }
    void push()
    {
        //Push current IRMgr of region and adopt a new.
        UseNewIRMgr * usenewirmgr = new UseNewIRMgr(m_rg, m_rg->getIRMgr());
        m_irmgr_stack.push(usenewirmgr);

        //Push current IRBBMgr of region and adopt a new.
        UseNewIRBBMgr * usenewbbmgr = new UseNewIRBBMgr(
            m_rg, m_rg->getBBMgr());
        m_bbmgr_stack.push(usenewbbmgr);

        //Push current BBList of region and adopt a new.
        UseNewBBList * usenewbblst = new UseNewBBList(
            m_rg, m_rg->getBBList(), m_rg->getBBMgr());
        m_bblist_stack.push(usenewbblst);

        //Push current CFG of region and adopt a new.
        UseNewCFG * usenewcfg = new UseNewCFG(
            m_rg, m_rg->getCFG(), m_rg->getBBList());
        m_cfg_stack.push(usenewcfg);
    }
    void pop()
    {
        UseNewCFG * usecfg = m_cfg_stack.pop();
        if (usecfg != nullptr) { delete usecfg; }

        UseNewBBList * usebblst = m_bblist_stack.pop();
        if (usebblst != nullptr) { delete usebblst; }

        UseNewIRBBMgr * usebbmgr = m_bbmgr_stack.pop();
        if (usebbmgr != nullptr) { delete usebbmgr; }

        UseNewIRMgr * useirmgr = m_irmgr_stack.pop();
        if (useirmgr != nullptr) { delete useirmgr; }
    }
};


//This function implements the swap operation of two prnos through the temp
//register after the register allocation.
//  There are three steps used to complete the swap operation:
//  1. $temp  <-- mov $prno1
//  2. $prno1 <-- mov $prno2
//  3. $prno2 <-- mov $temp
IR * LinearScanRA::doSwapByReg(PRNO prno1, PRNO prno2, Type const* ty1,
    Type const* ty2, IR const* marker, MOD IRBB * bb)
{
    ASSERT0(bb && ty1 && ty2);
    ASSERT0(prno1 != PRNO_UNDEF && prno2 != PRNO_UNDEF);
    ASSERT0(prno1 != prno2);

    Type const* ty1_tmp = getSpillType(ty1);
    Type const* ty2_tmp = getSpillType(ty2);
    ASSERT0(ty1_tmp && ty2_tmp);

    //Move the data from the reg of prno1 to the reg of temp.
    PRNO tmpprno = buildPrno(ty1_tmp, ty1_tmp->is_vector() ?
        getTempVector() : getTempScalar());
    IR * stpr1 = m_irmgr->buildStorePR(tmpprno, ty1_tmp,
        m_irmgr->buildPRdedicated(prno1, ty1_tmp));

    //Move the data from the reg of prno2 to the reg of prno1.
    PRNO tmp_prno_2 = buildPrno(ty2_tmp, getReg(prno1));
    IR * stpr2 = m_irmgr->buildStorePR(tmp_prno_2, ty2_tmp,
        m_irmgr->buildPRdedicated(prno2, ty2_tmp));

    //Move the data from the reg of tmp to the reg of prno2.
    PRNO tmp_prno_3 = buildPrno(ty1_tmp, getReg(prno2));
    IR * stpr3 = m_irmgr->buildStorePR(tmp_prno_3, ty1_tmp,
        m_irmgr->buildPRdedicated(tmpprno, ty1_tmp));

    if (marker) {
        IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
        bb->getIRList().insert_after(stpr1, stmt);
    } else {
        bb->getIRList().append_head(stpr1);
    }

    bb->getIRList().insert_after(stpr2, stpr1);
    bb->getIRList().insert_after(stpr3, stpr2);

    setMove(stpr2);
    setMove(stpr1);
    setMove(stpr3);
    return stpr3;
}


//This function implements the swap operation of two prnos through the memory
//allocation after the register allocation.
//  There are three steps used to complete the swap operation:
//  1. [mem]  <-- spill $prno1
//  2. $prno1 <-- mov $prno2
//  3. $prno2 <-- reload [mem]
IR * LinearScanRA::doSwapByMem(PRNO prno1, PRNO prno2, Type const* ty1,
    Type const* ty2, IR const* marker, MOD IRBB * bb)
{
    ASSERT0(bb && ty1 && ty2);
    ASSERT0(prno1 != PRNO_UNDEF && prno2 != PRNO_UNDEF);
    ASSERT0(prno1 != prno2);

    Type const* ty1_tmp = getSpillType(ty1);
    Type const* ty2_tmp = getSpillType(ty2);
    ASSERT0(ty1_tmp && ty2_tmp);

    Var * spill_loc = getTempVar(ty1_tmp);
    ASSERT0(spill_loc);

    //Build the spill IR.
    PRNO tmpprno = buildPrno(ty1_tmp, getReg(prno1));
    IR * spill = buildSpillByLoc(tmpprno, spill_loc, ty1_tmp);
    if (marker) {
        IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
        bb->getIRList().insert_after(spill, stmt);
    } else {
        bb->getIRList().append_head(spill);
    }
    //This data move is always between two registers.
    tmpprno = buildPrno(ty2_tmp, getReg(prno1));
    IR * stpr = m_irmgr->buildStorePR(tmpprno, ty2_tmp,
        m_irmgr->buildPRdedicated(prno2, ty2_tmp));

    //Build the reload IR.
    tmpprno = buildPrno(ty1_tmp, getReg(prno2));
    IR * reload = buildReload(tmpprno, spill_loc, ty1_tmp);

    bb->getIRList().insert_after(stpr, spill);
    bb->getIRList().insert_after(reload, stpr);
    setSpill(spill);
    setMove(stpr);
    setReload(reload);
    return reload;
}


//This function implements the swap operation of two prnos after the register
//allocation.
//  There are two ways to finish this operation:
//     1. Temp register.
//     2. Temp memory location.
//  The principles of the swap operation are listed as below:
//     1. If the temp register for prno1 is available, do the swap operation
//        by the temp register.
//     2. If the temp register for prno2 is available, do the swap operation
//        by the temp register.
//     3. If the temp register for prno1 and prno2 are not available, do
//        swap operation by the temp memory location.
IR * LinearScanRA::insertIRToSwap(PRNO prno1, PRNO prno2, Type const* ty1,
    Type const* ty2, IR const* marker, MOD IRBB * bb)
{
    ASSERT0(bb && ty1 && ty2);
    ASSERT0(prno1 != PRNO_UNDEF && prno2 != PRNO_UNDEF);
    ASSERT0(prno1 != prno2);
    ASSERT0(getReg(prno1) != getReg(prno2));

    if (isTmpRegAvailable(ty1)) {
        return doSwapByReg(prno1, prno2, ty1, ty2, marker, bb);
    }

    if (isTmpRegAvailable(ty2)) {
        return doSwapByReg(prno2, prno1, ty2, ty1, marker, bb);
    }

    return doSwapByMem(prno1, prno2, ty1, ty2, marker, bb);
}


PRNO LinearScanRA::getAnctPrno(PRNO prno) const
{
    ASSERT0(prno != PRNO_UNDEF);
    LifeTime const* lt = m_lt_mgr->getLifeTime(prno);
    if (lt == nullptr ) { return prno; }
    return lt->getAnctPrno();
}


bool LinearScanRA::isPrnoAlias(PRNO prno1, PRNO prno2) const
{
    if (prno1 == prno2) { return true; }

    //Check the alias based on the ancestor of lifetime. Normally, the prno
    //of the ancestor is smaller, so we start from the bigger prno, and then
    //backtrace to its ancestors until the expected prno is finded or the
    //ancestor is not existed.
    PRNO big = MAX(prno1, prno2);
    PRNO small = MIN(prno1, prno2);
    LifeTime const* lt = m_lt_mgr->getLifeTime(big);
    //This assert is commented due to the below if statement.
    //ASSERT0(lt);
    if (lt == nullptr) {
        //If there is no lifetime related to the prno, which means this prno is
        //not participated the register allocation, it is assigned to dediacted
        //register, so it is not alias with any other register.
        return false;
    }
    while (lt->getParent() != nullptr) {
        ASSERT0(lt->getPrno() > lt->getParent()->getPrno());
        lt = lt->getParent();
        if (lt->getPrno() == small) { return true; }
    }
    return false;
}


bool LinearScanRA::performLsraImpl(OptCtx & oc)
{
    //The default linear-scan implementation.
    RegSetImpl * rsimpl = allocRegSetImpl();
    rsimpl->initRegSet();
    LSRAImpl impl(*this, *rsimpl);
    bool changed = impl.perform(oc);
    ASSERT0(verifyAfterRA());
    delete rsimpl;
    return changed;
}


//TODO: rematerialization and spill-store-elimination
bool LinearScanRA::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_DOM,
                                               PASS_LIVENESS_MGR, PASS_UNDEF);
    reset();
    m_cfg = m_rg->getCFG();
    m_bb_list = m_rg->getBBList();
    if (m_bb_list == nullptr || m_bb_list->get_elem_count() == 0) {
        return false;
    }

    //Determine whether the PASS apply all modifications of CFG and BB to
    //current region. User may invoke LSRA as performance estimating tools
    //to conduct optimizations, such as RP, GCSE, UNROLLING which may increase
    //register pressure.
    ApplyToRegion apply(m_rg);
    if (!m_is_apply_to_region) {
        //Stash current region information.
        apply.push();
        m_cfg = m_rg->getCFG();
        m_bb_list = m_rg->getBBList();
        ASSERT0(m_cfg->verify());
    }

    //Assign each BB a topologic sequence ID.
    assignTopoIdForBB();

    //Do the backward jump analysis based on the CFG and liveness information.
    BBPos2Attr pos2attr;
    FakeVarMgr fake_var_mgr(m_rg);
    BackwardJumpAnalysis back_jump_ana(m_rg, &pos2attr, &fake_var_mgr, this);
    back_jump_ana.analyze();

    //Enable the dump-buffer.
    //DumpBufferSwitch buff(m_rg->getLogMgr());
    UpdatePos up(*this);
    collectDedicatedPR(m_bb_list, m_dedicated_mgr);
    getLTMgr().computeLifeTime(up, m_bb_list, m_dedicated_mgr);

    //Process the lifetime related attributes before register assignment.
    PosAttrLifeTimeProc lt_proc(m_rg, pos2attr, this);
    lt_proc.process();

    LTPriorityMgr priomgr(m_cfg, getTIMgr());
    priomgr.computePriority(getLTMgr());
    bool changed = performLsraImpl(oc);

    //Remove the fake-use IR with no code gen attribute after register
    //assignment.
    PosAttrNoCodeGenProc no_code_gen_proc(m_rg, pos2attr, this);
    no_code_gen_proc.process();
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLSRA()) {
        dump(false);
    }
    if (!m_is_apply_to_region) {
        apply.pop();
        m_cfg = nullptr;
        m_bb_list = nullptr;
    }
    ASSERTN(getRegion()->getCFG()->verifyRPO(oc),
            ("make sure original RPO is legal"));
    ASSERTN(getRegion()->getCFG()->verifyDomAndPdom(oc),
            ("make sure original DOM/PDOM is legal"));
    if (!changed || !m_is_apply_to_region) {
        ASSERT0(m_rg->getBBMgr()->verify());
        END_TIMER(t, getPassName());
        return false;
    }
    updateSSA(oc);
    ASSERT0(m_rg->getBBMgr()->verify());
    END_TIMER(t, getPassName());
    return false;
}
//END LinearScanRA

} //namespace xoc

