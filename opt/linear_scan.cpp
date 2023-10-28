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

    //Check the fake use IR responding at the pos.
    ASSERT0(tmp_ir && tmp_ir->is_st());
    IR * rhs = tmp_ir->getRHS();
    ASSERT0(rhs && rhs->is_pr());
    ASSERT0(m_lsra->isPrnoAlias(rhs->getPrno(), BBPOS_prno(pos)));
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
//and POS_ATTR_LT_TERM_BEFORE.
class PosAttrLifeTimeProc : public PosAttrProc {
    COPY_CONSTRUCTOR(PosAttrLifeTimeProc);
public:
    PosAttrLifeTimeProc(Region * rg, BBPos2Attr const& ir2attr,
                        LinearScanRA * lsra)
        : PosAttrProc(rg, ir2attr, lsra)
    { }
    virtual ~PosAttrLifeTimeProc() {}

protected:
    virtual bool checkRelatedAttrs(PosAttr const* attr) const;
    virtual bool processAttr(BBPos const& pos, PosAttr const* attr);

    bool processAttrLTNoTerminatedAfter(BBPos const& pos, PosAttr const* attr);
    bool processAttrLTTerminatedBefore(BBPos const& pos, PosAttr const* attr);
};


bool PosAttrLifeTimeProc::processAttr(BBPos const& pos, PosAttr const* attr)
{
    ASSERT0(attr);
    if (!processAttrLTNoTerminatedAfter(pos, attr)) { return false; }
    if (!processAttrLTTerminatedBefore(pos, attr)) { return false; }
    return true;
}


bool PosAttrLifeTimeProc::checkRelatedAttrs(PosAttr const* attr) const
{
    ASSERT0(attr);
    return attr->have(POS_ATTR_LT_NO_TERM_AFTER | POS_ATTR_LT_TERM_BEFORE);
}


bool PosAttrLifeTimeProc::processAttrLTNoTerminatedAfter(BBPos const& pos,
                                                         PosAttr const* attr)
{
    ASSERT0(attr);
    if (!attr->have(POS_ATTR_LT_NO_TERM_AFTER)) { return true; }
    IR * tmp_ir = const_cast<IR*>(attr->getIR());

    //Check the fake use IR responding at the BBPos.
    ASSERT0(tmp_ir && tmp_ir->is_st());
    IR * rhs = tmp_ir->getRHS();
    ASSERT0(rhs && rhs->is_pr() && (rhs->getPrno() == BBPOS_prno(pos)));
    IRBB const* bb = attr->getBB();
    ASSERT0(bb && BBPOS_bbid(pos) == bb->id());
    //Normally this position should be at the head of BB.
    ASSERT0(BBPOS_flag(pos) == INSERT_MODE_HEAD);

    //Exetend the current lifetime through the steps below:
    // 1. Find the accurate start pos of BB from lifetime manager.
    // 2. Find the live range including the start pos of BB in lifetime, because
    //    this pr is at the head of BB, so the live range must exist.
    // 3. Find the next range, extend the current life range to the next def if
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
        VecIdx next = i + 1;
        if (next <= lt->getRangeVec().get_last_idx()) {
            Range next_r = lt->getRange(next);
            //There is a hole between current range and next range，extend
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


bool PosAttrLifeTimeProc::processAttrLTTerminatedBefore(BBPos const& pos,
                                                        PosAttr const* attr)
{
    ASSERT0(attr);
    if (!attr->have(POS_ATTR_LT_TERM_BEFORE)) { return true; }
    IR * tmp_ir = const_cast<IR*>(attr->getIR());
    IR * rhs = tmp_ir->getRHS();
    ASSERT0(rhs && rhs->is_pr() && (rhs->getPrno() == BBPOS_prno(pos)));
    IRBB const* bb = attr->getBB();
    ASSERT0(BBPOS_bbid(pos) == bb->id());
    //Normally this position should be at the head of BB.
    ASSERT0(BBPOS_flag(pos) == INSERT_MODE_HEAD);

    //Terminated the current lifetime before the BBPos through the steps below:
    // 1. Find the accurate start pos of BB from lifetime manager.
    // 2. Get the first live range in the lifetime, the lifetime is extend from
    //    the entry of function due to the fake use PR.
    // 3. Update the start of the live range as the accurate occurence position.
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
    //   lifetime modified: <17><34-67>
    //    |                -                ----------------------------------
    //    |                u                d      u           u             u
    LifeTime * lt = m_lsra->getLTMgr().getLifeTime(rhs->getPrno());
    Pos cur_bb_entry_pos = m_lsra->getLTMgr().getBBStartPos(bb->id());
    Range r = lt->getFirstRange();
    OccListIter it;
    bool succ = lt->findOccAfter(cur_bb_entry_pos, it);
    ASSERT0(succ);
    Pos occ_pos = it->val().pos();

    //Terminated the current life range from the current occurence.
    if (r.is_contain(cur_bb_entry_pos)) {
        RG_start(r) = occ_pos;
        lt->setRange(0, r);
    }
    return true;
}
//END PosAttrLifeTimeProc


//
//START OccRecorder
//
class OccRecorder : public VisitIRTree {
    COPY_CONSTRUCTOR(OccRecorder);
    BackwardJumpAnalysis * m_ana;
    Pos m_pos;
protected:
    virtual bool visitIR(IR const* ir)
    {
        if (ir->is_pr() || (ir->is_stpr())) {
            if (!m_ana->isPrInRelatedLiveSet(ir->getPrno())) { return true; }
            m_ana->recordOccurenceForPr(ir->getPrno(), m_pos);
        }
        return true;
    }
public:
    OccRecorder(IR const* ir, Pos pos, BackwardJumpAnalysis * ana) :
        m_ana(ana), m_pos(pos)
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


Occurence * BackwardJumpAnalysisResMgr::genOccurence()
{
    return (Occurence*)xmalloc(sizeof(Occurence));
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
//START BackwardJumpAnalysis
//
BackwardJumpAnalysis::BackwardJumpAnalysis(Region * rg, BBPos2Attr * pos2attr,
                                           LinearScanRA * lsra)
{
    ASSERT0(rg && pos2attr && lsra);
    m_rg = rg;
    m_pos2attr = pos2attr;
    m_bb_list = nullptr;
    m_irmgr = rg->getIRMgr();
    m_tm = rg->getTypeMgr();
    m_live_mgr = nullptr;
    m_fake_var = nullptr;
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
    m_pr_live_set_list.init();
    m_pos2attr->init();
    m_prno2occ.init();
    m_backward_edges.init();
    if (m_resource_mgr == nullptr) {
        m_resource_mgr = new BackwardJumpAnalysisResMgr();
    }
}


void BackwardJumpAnalysis::destroy()
{
    m_pr_live_set_list.destroy();
    m_pos2attr->destroy();
    m_backward_edges.destroy();
    m_prno2occ.clean();
    m_bb_topoid.clean();
    m_bb_entry_pos.clean();
    m_bb_exit_pos.clean();
    if (m_resource_mgr != nullptr) {
        delete m_resource_mgr;
        m_resource_mgr = nullptr;
    }
}


void BackwardJumpAnalysis::recordOccurenceForPr(PRNO prno, Pos pos)
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


void BackwardJumpAnalysis::addToPRLiveSetList(UINT src_bbid, UINT dst_bbid)
{
    PRLiveSet const* live_out = m_live_mgr->get_liveout(src_bbid);
    PRLiveSet const* live_in = m_live_mgr->get_livein(dst_bbid);
    ASSERT0(live_out && live_in);

    m_pr_live_set_list.append_tail(live_out);
    m_pr_live_set_list.append_tail(live_in);
}


bool BackwardJumpAnalysis::isPrInRelatedLiveSet(PRNO pr)
{
    ASSERT0(pr != PRNO_UNDEF);
    for (PRLiveSet const* set = m_pr_live_set_list.get_head(); set != nullptr;
         set = m_pr_live_set_list.get_next()) {
        if (set->is_contain(pr)) { return true; }
    }
    return false;
}


void BackwardJumpAnalysis::collectBackwardJumps()
{
    BBListIter bbit;
    for (IRBB const* bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        AdjVertexIter ito;
        for (Vertex const* o = Graph::get_first_out_vertex(bb->getVex(), ito);
             o != nullptr; o = Graph::get_next_out_vertex(ito)) {
            if (!isBackwardJump(bb->id(), o->id())) { continue; }
            addBackwardJump(bb, m_rg->getBB(o->id()));
            addToPRLiveSetList(bb->id(), o->id());
        }
    }
}


void BackwardJumpAnalysis::assignTopoIdForBB()
{
    //Iterate BB list.
    BBListIter bbit;
    TOPOID id = 0;
    for (IRBB * bb = m_bb_list->get_head(&bbit); bb != nullptr;
         bb = m_bb_list->get_next(&bbit)) {
        m_bb_topoid.set(bb->id(), id++);
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
    for (IR * ir = irlst.get_head(&bbirit); ir != nullptr;
         ir = irlst.get_next(&bbirit)) {
        OccRecorder occurence_recorder(ir, ++pos, this);
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
    START_TIMER_FMT(t, ("DUMP %s", "backward jump analysis"));
    m_live_mgr = (LivenessMgr*)m_rg->getPassMgr()->queryPass(PASS_LIVENESS_MGR);
    m_bb_list = m_rg->getBBList();
    if (m_bb_list == nullptr || m_bb_list->get_elem_count() == 0) {
        END_TIMER_FMT(t, ("DUMP %s", "backward jump analysis"));
        return true;
    }

    //Step1: Assign each BB a topologic sequence ID.
    assignTopoIdForBB();

    //Step2: Collect the backward jump info in the CFG.
    collectBackwardJumps();

    //If there is no backward jump, nothing need to do.
    if (m_backward_edges.get_elem_count() == 0) {
        END_TIMER_FMT(t, ("DUMP %s", "backward jump analysis"));
        return true;
    }

    //Step3: Generate the simple occurence info for the relative live PRs.
    generateOccurence();

    //Step4: Insert the fake use IR for the related PRs after checked.
    checkAndInsertFakeUse();
    END_TIMER_FMT(t, ("DUMP %s", "backward jump analysis"));
    return true;
}


void BackwardJumpAnalysis::dump()
{
    note(m_rg, "\n==-- DUMP %s --==", "BackwardJumpAnalysis");

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

    note(m_rg, "\nFake use IR insert details:\n");
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


Var * BackwardJumpAnalysis::genFakeVar()
{
    //The fake use IR can use the same fake var for a flag.
    if (m_fake_var == nullptr) {
        m_fake_var = REGION_region_mgr(m_rg)->getVarMgr()->registerVar(
            "fake_var", m_tm->getU64(), 1, VAR_LOCAL | VAR_FAKE);
    }
    return m_fake_var;
}


//This func inserts the fake use IR at BB entry per the steps below:
//  1. Build a fake use IR by write the pr into a fake var.
//  2. Insert the fake usr IR at the entry of BB
//  3. Map the pos and the new fake IR with three attributes:
//        --- POS_ATTR_NO_CODE_GEN
//        --- POS_ATTR_LT_NO_TERM_AFTER
//        --- POS_ATTR_LT_TERM_BEFORE
void BackwardJumpAnalysis::insertFakeUseAtBBEntry(IRBB const* bb, PRNO prno,
                                                  BBPos const& pos)
{
    ASSERT0(bb);
    ASSERT0(prno != PRNO_UNDEF);
    IR * pr_ir = m_irmgr->buildPRdedicated(prno, m_tm->getU64());
    IR * st = m_irmgr->buildStore(genFakeVar(), m_tm->getU64(), pr_ir);
    const_cast<IRBB*>(bb)->getIRList().append_head(st);
    PosAttr * attr = m_resource_mgr->genPosAttr(POS_ATTR_NO_CODE_GEN |
        POS_ATTR_LT_NO_TERM_AFTER | POS_ATTR_LT_TERM_BEFORE, bb, st);
    setPosAttr(pos, attr);
}


//This func inserts the fake use IR at BB exit per the steps below:
//  1. Build a fake use IR by write the pr into a fake var.
//  2. Insert the fake usr IR at the tail of BB, but before the branch IR.
//  3. Map the pos and the new fake IR with attribute POS_ATTR_NO_CODE_GEN.
void BackwardJumpAnalysis::insertFakeUseAtBBExit(IRBB const* bb, PRNO prno,
                                                 BBPos const& pos)
{
    ASSERT0(bb);
    ASSERT0(prno != PRNO_UNDEF);
    IR const* tail = const_cast<IRBB*>(bb)->getIRList().get_tail();
    ASSERT0(tail);
    if (!tail->isBranch()) { return; }

    //Insert before the branch.
    IR * pr_ir = m_irmgr->buildPRdedicated(prno, m_tm->getU64());
    IR * st = m_irmgr->buildStore(genFakeVar(), m_tm->getU64(), pr_ir);
    const_cast<IRBB*>(bb)->getIRList().insert_before(st, tail);
    PosAttr * attr = m_resource_mgr->genPosAttr(POS_ATTR_NO_CODE_GEN, bb, st);
    setPosAttr(pos, attr);
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
    //same fake use pr at the same BB position is meaningless.
    if (m_pos2attr->find(bbpos)) { return; }

    switch (mode) {
    case INSERT_MODE_HEAD: insertFakeUseAtBBEntry(bb, prno, bbpos); break;
    case INSERT_MODE_TAIL: insertFakeUseAtBBExit(bb, prno, bbpos); break;
    default: ASSERT0(0); break;
    }
}


void BackwardJumpAnalysis::checkAndInsertFakeUse()
{
    for (BackwardEdge const* e = m_backward_edges.get_head();
         e != nullptr; e = m_backward_edges.get_next()) {
        IRBB const* src_bb = BKEDGE_srcbb(e);
        IRBB const* dst_bb = BKEDGE_dstbb(e);
        ASSERT0(src_bb && src_bb);

        //For the source BB of the backward jump edge, prno of the live out
        //need to be processed. If the last occurence of the pr is before the
        //end of BB, the fake use of pr will be inserted at the tail of source
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
            Occurence const* occ = m_prno2occ.get(pr);
            ASSERT0(occ);
            if (OCC_last(occ) < m_bb_exit_pos[src_bb->id()]) {
                insertFakeUse(src_bb, pr, INSERT_MODE_TAIL);
            }
        }

        //For the destination BB of the backward jump edge, prno of the live in
        //need to be processed. If the first occurence of the pr is after the
        //end of BB, the fake use of pr will be inserted at the head of
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
            if (m_lsra->getDedicatedMgr().is_dedicated(pr)) { continue; }
            if (OCC_first(occ) > m_bb_exit_pos[src_bb->id()]) {
                //Ignore the PR if the first occ of PR is after the jump/loop,
                //which means this PR is not involved in the scope of current
                //jump/loop.
                continue;
            }
            if (OCC_first(occ) > m_bb_entry_pos[dst_bb->id()]) {
                insertFakeUse(dst_bb, pr, INSERT_MODE_HEAD);
            }
        }
    }
}


void BackwardJumpAnalysis::setPosAttr(BBPos const& pos, PosAttr const* attr)
{
    ASSERT0(m_pos2attr && attr);
    //Check the duplicated insertion. At the moment, duplicated insertion for
    //same fake use pr at the same BB position is meaningless.
    if (m_pos2attr->find(pos)) { return; }
    m_pos2attr->set(pos, attr);
}
//END BackwardJumpAnalysis


//
//START RegSetImpl
//
TargInfoMgr & RegSetImpl::getTIMgr() const
{
    return m_ra.getTIMgr();
}


void RegSetImpl::initRegSet()
{
    //#define DEBUG_LSRA
    #ifdef DEBUG_LSRA
    //User can customize the register set that used in register allocation.
    //e.g: given calling convention only has 1 regisetr allocable.
    m_avail_callee_scalar.bunion(SCALAR_CALLEE_SAVED_REG_START);
    m_avail_callee_scalar.bunion(SCALAR_CALLEE_SAVED_REG_START + 1);
    m_avail_caller_scalar.clean();
    m_avail_allocable.bunion(m_avail_callee_scalar);
    m_avail_allocable.bunion(m_avail_caller_scalar);
    m_avail_callee_vector.bunion(VECTOR_CALLEE_SAVED_REG_START);
    m_avail_callee_vector.bunion(VECTOR_CALLEE_SAVED_REG_START + 1);
    m_avail_caller_vector.clean();
    m_avail_allocable_vector.bunion(m_avail_callee_vector);
    m_avail_allocable_vector.bunion(m_avail_caller_vector);
    #else
    if (getTIMgr().getCallee() != nullptr) {
        m_avail_callee_scalar.copy(*getTIMgr().getCallee());
    }
    if (getTIMgr().getCaller() != nullptr) {
        m_avail_caller_scalar.copy(*getTIMgr().getCaller());
    }
    if (getTIMgr().getParam() != nullptr) {
        m_avail_param_scalar.copy(*getTIMgr().getParam());
    }
    if (getTIMgr().getReturnValue() != nullptr) {
        m_avail_return_value_scalar.copy(*getTIMgr().getReturnValue());
    }
    if (getTIMgr().getAllocable() != nullptr) {
        m_avail_allocable.bunion(*getTIMgr().getAllocable());
    }
    if (getTIMgr().getVectorCallee() != nullptr) {
        m_avail_callee_vector.copy(*getTIMgr().getVectorCallee());
    }
    if (getTIMgr().getVectorCaller() != nullptr) {
        m_avail_caller_vector.copy(*getTIMgr().getVectorCaller());
    }
    if (getTIMgr().getVectorParam() != nullptr) {
        m_avail_param_vector.copy(*getTIMgr().getVectorParam());
    }
    if (getTIMgr().getVectorReturnValue() != nullptr) {
        m_avail_return_value_vector.copy(*getTIMgr().getVectorReturnValue());
    }
    if (getTIMgr().getVectorAllocable() != nullptr) {
        m_avail_allocable.bunion(*getTIMgr().getVectorAllocable());
    }
    #endif
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
           r == m_ra.getRegisterZero();
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
    RegSet const* rs = getTIMgr().getCaller();
    if (rs != nullptr && rs->is_contain(r)) {
        m_avail_caller_scalar.bunion((BSIdx)r);
    }
    rs = getTIMgr().getVectorCaller();
    if (rs != nullptr && rs->is_contain(r)) {
        m_avail_caller_vector.bunion((BSIdx)r);
    }
}


void RegSetImpl::freeRegisterFromCalleeAliasSet(Reg r)
{
    RegSet const* rs = getTIMgr().getCallee();
    if (rs != nullptr && rs->is_contain(r)) {
        m_avail_callee_scalar.bunion((BSIdx)r);
    }
    rs = getTIMgr().getVectorCallee();
    if (rs != nullptr && rs->is_contain(r)) {
        m_avail_callee_vector.bunion((BSIdx)r);
    }
}


void RegSetImpl::freeRegisterFromParamAliasSet(Reg r)
{
    RegSet const* rs = getTIMgr().getParam();
    if (rs != nullptr && rs->is_contain(r)) {
        m_avail_param_scalar.bunion((BSIdx)r);
    }
    rs = getTIMgr().getVectorParam();
    if (rs != nullptr && rs->is_contain(r)) {
        m_avail_param_vector.bunion((BSIdx)r);
    }
}


void RegSetImpl::freeRegisterFromReturnValueAliasSet(Reg r)
{
    RegSet const* rs = getTIMgr().getReturnValue();
    if (rs != nullptr && rs->is_contain(r)) {
        m_avail_return_value_scalar.bunion((BSIdx)r);
    }
    rs = getTIMgr().getVectorReturnValue();
    if (rs != nullptr && rs->is_contain(r)) {
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
    note(m_ra.getRegion(), "\n==-- DUMP %s --==", "AvaiableRegisterSet");
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
    m_ti_mgr = new TargInfoMgr();
    m_func_level_var_count = 0;
    m_is_apply_to_region = false;
}


LinearScanRA::~LinearScanRA()
{
    delete m_lt_mgr;
    delete m_ti_mgr;
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
}


Var * LinearScanRA::getSpillLoc(PRNO prno)
{
    return m_prno2var.get(prno);
}


Var * LinearScanRA::genSpillLoc(PRNO prno, Type const* ty)
{
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


PRNO LinearScanRA::buildPrnoDedicated(Type const* type, Reg reg)
{
    PRNO prno = m_irmgr->buildPrno(type);
    setDedicatedReg(prno, reg);
    return prno;
}


PRNO LinearScanRA::buildPrno(Type const* type, Reg reg)
{
    PRNO prno = m_irmgr->buildPrno(type);
    setReg(prno, reg);
    return prno;
}


IR * LinearScanRA::buildSpill(PRNO prno, Type const* ty)
{
    Var * spill_loc = genSpillLoc(prno, ty);
    ASSERT0(spill_loc);
    IR * pr = m_irmgr->buildPRdedicated(prno, ty);
    m_rg->getMDMgr()->allocRef(pr);
    IR * stmt = m_irmgr->buildStore(spill_loc, pr);
    m_rg->getMDMgr()->allocRef(stmt);
    m_rg->addToVarTab(spill_loc);
    stmt->setAligned(true);
    return stmt;
}


IR * LinearScanRA::buildReload(PRNO prno, Var * spill_loc, Type const* ty)
{
    ASSERT0(spill_loc);
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
    return true;
}


void LinearScanRA::dump4List() const
{
    note(m_rg, "\n==-- DUMP %s --==", "4LIST");
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
    note(m_rg, "\n==-- DUMP %s --==", "PR2Reg");
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


bool LinearScanRA::isSpillOp(IR const* ir) const
{
    return m_spill_tab.find(const_cast<IR*>(ir));
}


bool LinearScanRA::isRematOp(IR const* ir) const
{
    return m_remat_tab.find(const_cast<IR*>(ir));
}


bool LinearScanRA::isReloadOp(IR const* ir) const
{
    return m_reload_tab.find(const_cast<IR*>(ir));
}


bool LinearScanRA::isMoveOp(IR const* ir) const
{
    return m_move_tab.find(const_cast<IR*>(ir));
}


bool LinearScanRA::isRematLikeOp(IR const* ir) const
{
    if (!ir->is_stpr()) { return false; }
    if (!ir->getRHS()->is_lda() || !ir->getRHS()->is_const()) { return false; }
    return true;
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
        m_new_bbmgr = new IRBBMgr();
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
    ASSERT0(lt);
    while (lt->getAncestor() != nullptr) {
        ASSERT0(lt->getPrno() > lt->getAncestor()->getPrno());
        lt = lt->getAncestor();
        if (lt->getPrno() == small) { return true; }
    }
    return false;
}


bool LinearScanRA::performLsraImpl(OptCtx & oc)
{
    //The default linear-scan implementation.
    RegSetImpl * rsimpl = allocRegSetImpl();
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

    //Do the backward jump analysis based on the CFG and liveness information.
    BBPos2Attr pos2attr;
    BackwardJumpAnalysis back_jump_ana(m_rg, &pos2attr, this);
    back_jump_ana.analyze();

    //Enable the dump-buffer.
    DumpBufferSwitch buff(m_rg->getLogMgr());
    UpdatePos up(*this);
    collectDedicatedPR(m_bb_list, m_dedicated_mgr);
    getLTMgr().computeLifeTime(up, m_bb_list, m_dedicated_mgr);

    //Process the lifetime related attributes before register assignment.
    PosAttrLifeTimeProc lt_proc(m_rg, pos2attr, this);
    lt_proc.process();

    LTPriorityMgr priomgr(m_cfg, getTIMgr());
    priomgr.computePriority(getLTMgr());
    bool changed = performLsraImpl(oc);

    //Remove the fake IR with no code gen attribute after register assignment.
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
