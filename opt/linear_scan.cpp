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
    LexBackwardJumpAnalysis * m_ana;
public:
    Pos pos;
public:
    OccRecorderVF(LexBackwardJumpAnalysis * ana) : m_ana(ana), pos(POS_UNDEF) {}
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


LexBackwardJump * BackwardJumpAnalysisResMgr::genLexBackwardJump(
    IRBB const* srcbb, IRBB const* dstbb)
{
    ASSERT0(srcbb && dstbb);
    LexBackwardJump * e = (LexBackwardJump*)xmalloc(sizeof(LexBackwardJump));
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
//START LexBackwardJumpAnalysis
//
LexBackwardJumpAnalysis::LexBackwardJumpAnalysis(Region * rg,
    BBPos2Attr * pos2attr, FakeVarMgr * fake_var_mgr, LinearScanRA * lsra)
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


LexBackwardJumpAnalysis::~LexBackwardJumpAnalysis()
{
    destroy();
}


void LexBackwardJumpAnalysis::reset()
{
    destroy();
    init();
}


void LexBackwardJumpAnalysis::init()
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


void LexBackwardJumpAnalysis::destroy()
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


void LexBackwardJumpAnalysis::recordOccurenceForPR(PRNO prno, Pos pos)
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


void LexBackwardJumpAnalysis::collectBackwardJumps()
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


void LexBackwardJumpAnalysis::generateOccurenceForBB(IRBB const* bb,
                                                     MOD Pos & pos)
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


void LexBackwardJumpAnalysis::generateOccurence()
{
    BBListIter bbit;
    Pos pos = POS_UNDEF;
    for (IRBB * bb = m_bb_list->get_head(&bbit); bb != nullptr;
         bb = m_bb_list->get_next(&bbit)) {
        generateOccurenceForBB(bb, pos);
    }
}


bool LexBackwardJumpAnalysis::analyze()
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


void LexBackwardJumpAnalysis::dump()
{
    note(m_rg, "\n==-- DUMP LexBackwardJumpAnalysis --==");

    Prno2OccMapIter it;
    Occurence const* occ = nullptr;
    note(m_rg, "\nPR occ interval:\n");
    for (PRNO pr = m_prno2occ.get_first(it, &occ); occ != nullptr;
         pr = m_prno2occ.get_next(it, &occ)) {
        ASSERT0(OCC_first(occ) != POS_UNDEF);
        note(m_rg, "PR %u, [%u, %u]\n", pr, OCC_first(occ), OCC_last(occ));
    }

    note(m_rg, "\nBackward jump BB interval:\n");
    for (LexBackwardJump const* e = m_backward_edges.get_head();
         e != nullptr; e = m_backward_edges.get_next()) {
        UINT src_bb = BKJUMP_srcbb(e)->id();
        UINT dst_bb = BKJUMP_dstbb(e)->id();
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
void LexBackwardJumpAnalysis::insertFakeUseAtBBEntry(IRBB const* bb, PRNO prno,
                                                     BBPos const& pos)
{
    ASSERT0(bb);
    ASSERT0(prno != PRNO_UNDEF);
    Type const* ty = m_lsra->getVarTypeOfPRNO(prno);
    IR * pr_ir = m_irmgr->buildPRdedicated(prno, ty);
    IR * st = m_irmgr->buildStore(m_fake_var_mgr->genFakeVar(ty), ty, pr_ir);
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
void LexBackwardJumpAnalysis::insertFakeUseAtBBExit(IRBB const* bb, PRNO prno,
                                                    BBPos const& pos)
{
    ASSERT0(bb);
    ASSERT0(prno != PRNO_UNDEF);
    IR const* tail = const_cast<IRBB*>(bb)->getIRList().get_tail();
    ASSERT0(tail);
    if (!tail->isBranch()) { return; }

    //Insert before the branch.
    Type const* ty = m_lsra->getVarTypeOfPRNO(prno);
    IR * pr_ir = m_irmgr->buildPRdedicated(prno, ty);
    IR * st = m_irmgr->buildStore(m_fake_var_mgr->genFakeVar(ty), ty, pr_ir);
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


void LexBackwardJumpAnalysis::insertFakeUse(IRBB const* bb, PRNO prno,
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


void LexBackwardJumpAnalysis::recordFakeUse(PRNO prno, IRBB const* bb,
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


void LexBackwardJumpAnalysis::generateFakeUse()
{
    for (LexBackwardJump const* e = m_backward_edges.get_head();
         e != nullptr; e = m_backward_edges.get_next()) {
        IRBB const* src_bb = BKJUMP_srcbb(e);
        IRBB const* dst_bb = BKJUMP_dstbb(e);
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
            //Since some new fake-use IRs will be inserted at the start of the
            //dst BB, so the entry boundary of the dst BB should be included.
            if (OCC_first(occ) >= m_bb_entry_pos[dst_bb->id()]) {
                recordFakeUse(pr, dst_bb, INSERT_MODE_HEAD);
            }
        }
    }
}


void LexBackwardJumpAnalysis::insertFakeUse()
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
//END LexBackwardJumpAnalysis


//
//START PRNOConstraintsTab
//
bool PRNOConstraintsTab::hasTwoOrMoreCommonElements(
    PRNOConstraintsTab const& src_tab) const
{
    PRNOConstraintsTabIter it;
    UINT common_count = 0;
    for (PRNO pr = src_tab.get_first(it); pr != PRNO_UNDEF;
         pr = src_tab.get_next(it)) {
        if (!find(pr)) { continue; }
        common_count++;
        if (common_count >= 2) { return true; }
    }
    return false;
}
//END PRNOConstraintsTab


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

    //Try to collect available registers occupied by other modules.
    collectOtherAvailableRegister();
}


//Pick up a physical register from allocable register set by the incremental
//order.
Reg RegSetImpl::pickRegByIncrementalOrder(RegSet & set)
{
    BSIdx i = set.get_first();
    if (i == BS_UNDEF) {
        return REG_UNDEF;
    }
    set.diff(i);
    return (Reg)i;
}


//Pick up a physical register from allocable register set.
Reg RegSetImpl::pickReg(RegSet & set)
{
    return pickRegByIncrementalOrder(set);
}


void RegSetImpl::pickReg(RegSet & set, Reg r)
{
    if (set.is_contain(r)) {
        set.diff(r);
    }
}


Reg RegSetImpl::handleOnlyConsistency(OUT RegSet & set,
                                      PRNOConstraintsTab const& consist_prs)
{
    PRNOConstraintsTabIter it;

    //Check in consist_prs whether a register has already been allocated.
    //If a register has been allocated, return it directly.
    for (PRNO pr = consist_prs.get_first(it); pr != PRNO_UNDEF;
         pr = consist_prs.get_next(it)) {
        Reg r = m_ra.getReg(pr);
        if (r != REG_UNDEF) {
            return r;
        }
    }

    //Attempt to find an available register from the set.
    BSIdx available_reg = set.get_first();
    if (available_reg != BS_UNDEF) {
        set.diff(available_reg);
        return (Reg)available_reg;
    }
    return REG_UNDEF;
}


void RegSetImpl::removeConflictingReg(OUT RegSet & set,
                                      PRNOConstraintsTab const& conflict_prs,
                                      OUT RegSetWrap & removed_regs_wrap)
{

    PRNOConstraintsTabIter it;
    PRNO pr = conflict_prs.get_first(it);
    if (pr != PRNO_UNDEF) {
        removed_regs_wrap.alloc();
    }

    //Traverse conflict_prs and delete its registers from the set.
    //`set` represents the available (free) register set. It contains
    //registers that can be allocated. Suppose the conflicting PRs are
    //$2 and $3. This may lead to the following cases:
    //Case 1: If `set = {R2, R3, R4, R5, R6}`, and $2 is assigned to
    //register R2, and R2 is still in the free set. When allocating
    //a register for $3, we must remove R2 from the free set and select
    //one from the remaining set {R3, R4, R5, R6}. Additionally, we
    //record the removal of R2.
    //
    //Case 2: If $2 is assigned to register R2, but the free set is
    //`{R3, R4, R5, R6}` (meaning R2 is no longer in the free set), we
    //can freely select a register from the set without needing to record
    //any information.
    for (; pr != PRNO_UNDEF; pr = conflict_prs.get_next(it)) {
        Reg r = m_ra.getReg(pr);

        //The current PR has already been assigned a register,
        //and the assigned register is in the free set.
        if (r != REG_UNDEF && set.is_contain(r)) {
            set.diff(r);
            ASSERT0(removed_regs_wrap.getRegSet());
            removed_regs_wrap.getRegSet()->bunion(r);
        }
    }
}


Reg RegSetImpl::handleOnlyConflicts(OUT RegSet & set,
                                    PRNOConstraintsTab const& conflict_prs)
{
    RegSetWrap removed_regs_wrap;

    //Remove conflicting registers from the set to avoid conflicts.
    //Example 1: set = {r2, r3, r4}, removed_regs_wrap = {r2}.
    //Example 2: set = {r2}, removed_regs_wrap = {r2}.
    removeConflictingReg(set, conflict_prs, removed_regs_wrap);

    //Attempt to find an available register from the set after
    //removing conflict set.
    BSIdx available_reg = set.get_first();

    //Immediately restore previously removed conflicting registers
    //to ensure the set remains complete after conflict resolution.
    //In Example 1 and Example 2, r2 will be restored back to set.
    RegSet const* rs = removed_regs_wrap.getRegSet();
    if (rs != nullptr) { set.bunion(*rs); }

    if (available_reg != BS_UNDEF) {
        set.diff(available_reg);
        return (Reg)available_reg;
    }
    return REG_UNDEF;
}


Reg RegSetImpl::handleConflictsAndConsistency(
    OUT RegSet & set, PRNOConstraintsTab const& conflict_prs,
    PRNOConstraintsTab const& consist_prs)
{
    //Check for conflicts between conflict_prs and consist_prs.
    //conflict_prs contains elements like {pr0, pr1, pr2},
    //indicating that these registers cannot be allocated
    //to the same physical register in the future.
    //consist_prs contains elements like {pr1, pr2},
    //indicating that pr1 and pr2 must be allocated to the
    //same physical register. If there are common elements
    //between these two sets,
    //it represents a conflict and violates the intended semantics.
    ASSERT0(!conflict_prs.hasTwoOrMoreCommonElements(consist_prs));
    PRNOConstraintsTabIter it;

    //Check if any register has already been allocated in consist_prs
    //Return the allocated register directly.
    for (PRNO pr = consist_prs.get_first(it); pr != PRNO_UNDEF;
         pr = consist_prs.get_next(it)) {
        Reg r = m_ra.getReg(pr);
        if (r != REG_UNDEF) { return r; }
    }

    //If no registers were allocated in consist_prs, handle conflict set.
    return handleOnlyConflicts(set, conflict_prs);
}


Reg RegSetImpl::pickRegWithConstraints(OUT RegSet & set,
                                       LTConstraints const* lt_constraints)
{
    ASSERT0(lt_constraints);

    PRNOConstraintsTab const& conflict_prs = lt_constraints->getConflictTab();
    PRNOConstraintsTab const& consist_prs = lt_constraints->getConsistTab();

    //Check different conditions based on the state of
    //conflict_prs and consist_prs.
    //1. If both conflict_prs and consist_prs are not empty, handle that case.
    //2. If only conflict_prs is not empty, handle that case.
    //3. If only consist_prs is not empty, handle that case.
    //4. If both are empty, assert an error (indicating an unexpected state).
    if (!conflict_prs.is_empty() && !consist_prs.is_empty()) {
        return handleConflictsAndConsistency(set, conflict_prs, consist_prs);
    }

    if (!conflict_prs.is_empty()) {
        return handleOnlyConflicts(set, conflict_prs);
    }

    if (!consist_prs.is_empty()) {
        return handleOnlyConsistency(set, consist_prs);
    }

    ASSERT0(0); return REG_UNDEF;
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


Reg RegSetImpl::pickCallee(IR const* ir, LTConstraints const* lt_constraints)
{
    RegSet & set = ir->is_vec() ? m_avail_callee_vector :
                                  m_avail_callee_scalar;
    if (lt_constraints != nullptr) {
        return pickRegWithConstraints(set, lt_constraints);
    }
    return pickReg(set);
}


Reg RegSetImpl::pickCaller(IR const* ir, LTConstraints const* lt_constraints)
{
    RegSet & set = ir->is_vec() ? m_avail_caller_vector :
                                  m_avail_caller_scalar;
    if (lt_constraints != nullptr) {
        return pickRegWithConstraints(set, lt_constraints);
    }
    return pickReg(set);
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


bool RegSetImpl::isFPAllocable() const
{
    return !g_force_use_fp_as_sp && !xoc::g_debug && m_ra.notHaveAlloca() &&
        m_ra.noNeedToAlignStack() && m_ra.isFPAllocableAllowed();
}
//END RegSetImpl


//
//START RoundRobinRegSetImpl
//
//Pick up a physical register from allocable register set by the roundrobin
//way.
Reg RoundRobinRegSetImpl::pickRegRoundRobin(RegSet & set)
{
    BSIdx i = BS_UNDEF;
    if (m_bsidx_marker == BS_UNDEF) {
        i = m_bsidx_marker = set.get_first();
    } else if (m_bsidx_marker != BS_UNDEF) {
        i = set.get_next(m_bsidx_marker);
        if (i == BS_UNDEF || i > set.get_last()) { i = set.get_first(); }
        m_bsidx_marker = i;
    }

    if (i == BS_UNDEF) { return REG_UNDEF; }
    set.diff(i);
    return (Reg)i;
}


Reg RoundRobinRegSetImpl::pickReg(RegSet & set)
{
    return pickRegRoundRobin(set);
}
//END RoundRobinRegSetImpl


//START LTConstraintsMgr
void LTConstraintsMgr::init()
{
    m_ltc_list.init();
}


LTConstraints * LTConstraintsMgr::allocLTConstraints()
{
    LTConstraints * lt_constraints = new LTConstraints();
    ASSERT0(lt_constraints);
    m_ltc_list.append_tail(lt_constraints);
    return lt_constraints;
}


void LTConstraintsMgr::destroy()
{
    for (LTConstraints * ltc = m_ltc_list.get_head(); ltc!= nullptr;
         ltc = m_ltc_list.get_next()) {
        delete ltc;
    }
    m_ltc_list.destroy();
}
//END LTConstraintsMgr


//
//START LTConstraints
//
void LTConstraints::updateConflictPR(PRNO renamed_pr, PRNO old_pr)
{
    ASSERT0(m_conflicting_prs.find(old_pr));
    m_conflicting_prs.remove(old_pr);
    m_conflicting_prs.append(renamed_pr);
}
//END LTConstraints


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
    m_lt_constraints_strategy = nullptr;
    m_lt_constraints_mgr = nullptr;
}


LinearScanRA::~LinearScanRA()
{
    delete m_lt_mgr;
    if (m_lt_constraints_mgr != nullptr) {
        delete m_lt_constraints_mgr;
        m_lt_constraints_mgr = nullptr;
    }
    if (m_lt_constraints_strategy != nullptr) {
        delete m_lt_constraints_strategy;
        m_lt_constraints_strategy = nullptr;
    }
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
    if (getLTConstraintsMgr() != nullptr) {
        getLTConstraintsMgr()->reset();
    }
}


void LinearScanRA::assignLexSeqIdForBB()
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
    ASSERT0(m_irmgr);
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
    ASSERT0(rematctx.material_exp && ty);
    ASSERT0(prno != PRNO_UNDEF);
    IR * e = m_rg->dupIRTree(rematctx.material_exp);
    m_rg->getMDMgr()->allocRefForIRTree(e, true);
    IR * stmt = m_irmgr->buildStorePR(prno, ty, e);
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


Type const* LinearScanRA::getVarTypeOfPRNO(PRNO prno) const
{
    Var * var = m_rg->getVarByPRNO(prno);
    ASSERT0(var);
    TypeMgr * tm = m_rg->getTypeMgr();
    Type const* varty = var->getType();
    //[BUG FIX] If the type of var is ANY, size cannot be obtainer.
    if (varty->is_vector() || varty->is_fp() || varty->is_any()) {
        return varty;
    }
    Type const* tm_word_ty = tm->getTargMachRegisterType();
    UINT var_sz = var->getByteSize(tm);
    UINT tm_word_sz = tm->getByteSize(tm_word_ty);
    ASSERT0(varty->isInt());
    if (var_sz <= tm_word_sz) { return tm_word_ty; }
    return varty; //varty might be ANY.
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
bool LinearScanRA::checkLTCanBeRematerialized(MOD LifeTime * lt,
                                              OUT RematCtx & rematctx)
{
    //Target Dependent Code.
    ASSERT0(lt);
    if (!lt->canBeRemat()) { return false; }
    ASSERT0(lt->getRematExp());
    rematctx.material_exp = lt->getRematExp();
    lt->setRematerialized();
    return true;
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
            OUT xcom::DefFixedStrBuf & buf, Region const* rg, IR const* ir,
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
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID | IR_DUMP_SRC_LINE);
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
    UpdatePos up(this);
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


void LinearScanRA::recalculateSSA(OptCtx & oc) const
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


//This function implements the swap operation of two prnos through the temp
//register after the register allocation.
//  There are three steps used to complete the swap operation:
//  1. $temp  <-- mov $src_prno_with_r1
//  2. $dst_prno_with_r1 <-- mov $src_prno_with_r2
//  3. $dst_prno_with_r2 <-- mov $temp
IR * LinearScanRA::doSwapByReg(PRNO src_prno_with_r2, PRNO dst_prno_with_r1,
    PRNO src_prno_with_r1, PRNO dst_prno_with_r2, Type const* ty1,
    Type const* ty2, IR const* marker, MOD IRBB * bb)
{
    ASSERT0(bb && ty1 && ty2);
    ASSERT0(src_prno_with_r1 != PRNO_UNDEF && src_prno_with_r2 != PRNO_UNDEF);
    ASSERT0(dst_prno_with_r1 != PRNO_UNDEF && dst_prno_with_r2 != PRNO_UNDEF);
    ASSERT0(src_prno_with_r2 != src_prno_with_r1);
    ASSERT0(dst_prno_with_r1 != dst_prno_with_r2);
    ASSERT0(getReg(src_prno_with_r2) != getReg(src_prno_with_r1));

    Type const* ty1_tmp = getSpillType(ty1);
    Type const* ty2_tmp = getSpillType(ty2);
    ASSERT0(ty1_tmp && ty2_tmp);

    //Move the data from the reg of src_prno_with_r1 to the reg of temp.
    PRNO tmpprno = buildPrno(ty1_tmp, getTempReg(ty1_tmp));
    IR * stpr1 = m_irmgr->buildMove(tmpprno, src_prno_with_r1, ty1_tmp);

    //Move the data from the reg of src_prno_with_r2 to the reg of
    //dst_prno_with_r1.
    IR * stpr2 = m_irmgr->buildMove(dst_prno_with_r1, src_prno_with_r2,
                                    ty2_tmp);

    //Move the data from the reg of tmp to the reg of dst_prno_with_r2.
    IR * stpr3 = m_irmgr->buildMove(dst_prno_with_r2, tmpprno, ty1_tmp);

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
//  1. [mem]  <-- spill $src_prno_with_r1
//  2. $dst_prno_with_r1 <-- mov $src_prno_with_r2
//  3. $dst_prno_with_r2 <-- reload [mem]
IR * LinearScanRA::doSwapByMem(PRNO src_prno_with_r2, PRNO dst_prno_with_r1,
    PRNO src_prno_with_r1, PRNO dst_prno_with_r2, Type const* ty1,
    Type const* ty2, IR const* marker, MOD IRBB * bb)
{
    ASSERT0(bb && ty1 && ty2);
    ASSERT0(src_prno_with_r1 != PRNO_UNDEF && src_prno_with_r2 != PRNO_UNDEF);
    ASSERT0(dst_prno_with_r1 != PRNO_UNDEF && dst_prno_with_r2 != PRNO_UNDEF);
    ASSERT0(src_prno_with_r2 != src_prno_with_r1);
    ASSERT0(dst_prno_with_r1 != dst_prno_with_r2);
    ASSERT0(getReg(src_prno_with_r2) != getReg(src_prno_with_r1));

    Type const* ty1_tmp = getSpillType(ty1);
    Type const* ty2_tmp = getSpillType(ty2);
    ASSERT0(ty1_tmp && ty2_tmp);

    Var * spill_loc = getTempVar(ty1_tmp);
    ASSERT0(spill_loc);

    //Build the spill IR.
    IR * spill = buildSpillByLoc(src_prno_with_r1, spill_loc, ty1_tmp);
    if (marker) {
        IR const* stmt = marker->is_stmt() ? marker : marker->getStmt();
        bb->getIRList().insert_after(spill, stmt);
    } else {
        bb->getIRList().append_head(spill);
    }
    //This data move is always between two registers.
    IR * stpr = m_irmgr->buildMove(dst_prno_with_r1, src_prno_with_r2, ty2_tmp);

    //Build the reload IR.
    IR * reload = buildReload(dst_prno_with_r2, spill_loc, ty1_tmp);

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
//     1. If the temp register for dst_prno_with_r1 is available, do the swap
//        operation by the temp register.
//     2. If the temp register for dst_prno_with_r2 is available, do the swap
//        operation by the temp register.
//     3. If the temp register for dst_prno_with_r1 and dst_prno_with_r2 are
//        not available, do swap operation by the temp memory location.
IR * LinearScanRA::insertIRToSwap(PRNO src_prno_with_r2, PRNO dst_prno_with_r1,
    PRNO src_prno_with_r1, PRNO dst_prno_with_r2, Type const* ty1,
    Type const* ty2, IR const* marker, MOD IRBB * bb)
{
    ASSERT0(bb && ty1 && ty2);
    ASSERT0(src_prno_with_r1 != PRNO_UNDEF && src_prno_with_r2 != PRNO_UNDEF);
    ASSERT0(dst_prno_with_r1 != PRNO_UNDEF && dst_prno_with_r2 != PRNO_UNDEF);
    ASSERT0(src_prno_with_r1 != src_prno_with_r2);
    ASSERT0(dst_prno_with_r1 != dst_prno_with_r2);
    ASSERT0(getReg(src_prno_with_r2) != getReg(src_prno_with_r1));

    if (isTmpRegAvailable(ty1)) {
        return doSwapByReg(src_prno_with_r2, dst_prno_with_r1,
            src_prno_with_r1, src_prno_with_r1, ty1, ty2, marker, bb);
    }

    if (isTmpRegAvailable(ty2)) {
        return doSwapByReg(src_prno_with_r1, dst_prno_with_r2,
            src_prno_with_r2, dst_prno_with_r1, ty2, ty1, marker, bb);
    }

    return doSwapByMem(src_prno_with_r2, dst_prno_with_r1,
        src_prno_with_r1, dst_prno_with_r2, ty1, ty2, marker, bb);
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


void LinearScanRA::reuseStackSlot(bool is_vector)
{
    VarCheck::setVarCheckCondition(is_vector);
    LSRAVarLivenessMgr var_liveness_mgr(m_rg, *this);
    var_liveness_mgr.perform();

    VarLifeTimeMgr var_ltmgr(m_rg, *this, &var_liveness_mgr);
    var_ltmgr.computeLifeTime();
    var_ltmgr.computeAccuLifeTime();

    VarInterfGraph interf(m_rg, var_ltmgr);
    if (xoc::g_interference_graph_stack_slot_color) {
        interf.color();
    } else {
        interf.colorFast();
    }
    interf.rewrite();
}


void LinearScanRA::scanIRAndSetConstraints()
{
    if (m_lt_constraints_strategy == nullptr) {
        ASSERT0(m_lt_constraints_mgr == nullptr);
        return;
    }

    BBList * bb_list = this->getBBList();
    ASSERT0(bb_list);
    BBListIter bb_it;
    for (IRBB * bb = bb_list->get_head(&bb_it);
         bb != nullptr; bb = bb_list->get_next(&bb_it)) {
        BBIRList & ir_lst = bb->getIRList();
        BBIRListIter bb_ir_it;
        for (IR * ir = ir_lst.get_head(&bb_ir_it);
             ir != nullptr; ir = ir_lst.get_next(&bb_ir_it)) {
            //Note that different architectures have different strategy
            //implementations. For example, this architecture's addition
            //instruction requires that the source and destination registers
            //have different lifetime allocations,
            //while other architectures may not have this requirement.
            m_lt_constraints_strategy->applyConstraints(ir);
        }
    }
}


void LinearScanRA::tryComputeConstraints()
{
    //Before calculating the lifetime constraints, we need to
    //initialize both the lifetime constraint management unit
    //and the constraints strategy. After initialization, we can
    //proceed to set the constraint collection.
    //Note: initLTConstraintsMgr() and initConstraintsStrategy()
    //have different implementations depending on the architecture.
    //initLTConstraintsMgr() allocates memory for the lifetime
    //constraint specific to the architecture, while the
    //ConstraintsStrategy generates various constraint results
    //according to the architecture-specific strategies.
    //For example, the base class is LTConstraints. ARM may need
    //to create its own constraint collection, which can be done
    //with a class like ARMLTConstraints that inherits from
    //LTConstraints. Similarly, ARM can have its own
    //ARMLTConstraintsMgr, such as class ARMLTConstraintsMgr
    //that inherits from LTConstraintsMgr, with similar strategies
    //for method implementation.
    initLTConstraintsMgr();
    initConstraintsStrategy();
    scanIRAndSetConstraints();
}


void LinearScanRA::checkAndApplyToRegion(MOD ApplyToRegion & apply)
{
    if (isApplyToRegion()) { return; }
    //Stash pop current region information.
    apply.pop();
    m_cfg = m_rg->getCFG();
    m_bb_list = m_rg->getBBList();
    m_irmgr = m_rg->getIRMgr();
}


void LinearScanRA::checkAndPrepareApplyToRegion(OUT ApplyToRegion & apply)
{
    m_cfg = m_rg->getCFG();
    m_bb_list = m_rg->getBBList();
    m_irmgr = m_rg->getIRMgr();
    if (isApplyToRegion()) { return; }
    //Stash push current region information.
    apply.push();
    m_cfg = m_rg->getCFG();
    m_bb_list = m_rg->getBBList();
    m_irmgr = m_rg->getIRMgr();
    ASSERT0(m_cfg->verify());
}


void LinearScanRA::genRematForLT(MOD LifeTime * lt) const
{
    ASSERT0(lt);
    bool lt_has_only_one_def = lt->isOneDefOnly();
    OccList & occ_lst = const_cast<LifeTime*>(lt)->getOccList();
    OccListIter it = nullptr;
    for (Occ occ = occ_lst.get_head(&it); it != occ_lst.end();
         occ = occ_lst.get_next(&it)) {
        ASSERT0(occ.getIR());
        IR * occ_ir = occ.getIR();

        //Process the def occ only.
        if (!occ.is_def()) { continue; }

        //If there is a def IR which is not a remat-like Op, that means this
        //lifetime cannot be rematerialized.
        if (!isRematLikeOp(occ_ir)) {
            lt->setRematExp(nullptr);
            break;
        }
        if (lt->getRematExp() == nullptr) {
            lt->setRematExp(occ_ir->getRHS());
        }

        //If this lifetime has only one def, then exit the loop.
        if (lt_has_only_one_def) { break; }

        //If the two remat-like IRs are not same, clear the remat info stored
        //before.
        if (!lt->getRematExp()->isIREqual(occ_ir->getRHS(), m_rg->getIRMgr())) {
            lt->setRematExp(nullptr);
            break;
        }
    }
}


void LinearScanRA::genRematInfo()
{
    LTList const& ltlst = getLTMgr().getLTList();
    LTListIter it;
    for (LifeTime * lt = ltlst.get_head(&it); lt != nullptr;
         lt = ltlst.get_next(&it)) {
        genRematForLT(lt);
    }
}


void LinearScanRA::setAttr()
{
    DynamicStack * dynamic_stack = (DynamicStack*)m_rg->getPassMgr()->
        queryPass(PASS_DYNAMIC_STACK);
    ASSERT0(dynamic_stack);
    m_has_alloca = dynamic_stack->hasAlloca();
    m_may_need_to_realign_stack = dynamic_stack->mayRealignStack();
}


//TODO: rematerialization and spill-store-elimination
bool LinearScanRA::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_RPO, PASS_DOM, PASS_LIVENESS_MGR, PASS_UNDEF);
    reset();

    //Determine whether the PASS apply all modifications of CFG and BB to
    //current region. User may invoke LSRA as performance estimating tools
    //to conduct optimizations, such as RP, GCSE, UNROLLING which may increase
    //register pressure.
    ApplyToRegion apply(m_rg);
    checkAndPrepareApplyToRegion(apply);
    if (m_bb_list == nullptr || m_bb_list->get_elem_count() == 0) {
        return false;
    }
    setAttr();

    //Assign each BB a lexical sequence ID.
    assignLexSeqIdForBB();

    //Do the backward jump analysis based on the CFG and liveness information.
    BBPos2Attr pos2attr;
    FakeVarMgr fake_var_mgr(m_rg);
    LexBackwardJumpAnalysis back_jump_ana(m_rg, &pos2attr, &fake_var_mgr, this);
    back_jump_ana.analyze();

    UpdatePos up(this);
    collectDedicatedPR(m_bb_list, m_dedicated_mgr);
    getLTMgr().computeLifeTime(up, m_bb_list, m_dedicated_mgr);

    //After the lifetime calculation is completed, begin setting constraint
    //sets for each lifetime.
    tryComputeConstraints();

    //The remat info must be generated before the priority of the lifetime is
    //computed.
    genRematInfo();

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

    //Color the stack slot and reuse the stack slot.
    colorStackSlot();

    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLSRA()) {
        dump(false);
    }

    checkAndApplyToRegion(apply);
    ASSERTN(getRegion()->getCFG()->verifyRPO(oc),
            ("make sure original RPO is legal"));
    ASSERTN(getRegion()->getCFG()->verifyDomAndPdom(oc),
            ("make sure original DOM/PDOM is legal"));
    if (!changed || !isApplyToRegion()) {
        ASSERT0(m_rg->getBBMgr()->verify());
        END_TIMER(t, getPassName());
        return false;
    }
    recalculateSSA(oc);
    ASSERT0(m_rg->getBBMgr()->verify());
    END_TIMER(t, getPassName());
    return false;
}
//END LinearScanRA

} //namespace xoc

