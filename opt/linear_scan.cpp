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
    if (!lsra->getRegion()->isLogMgrInit()) { return; }
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


void dumpBBListWithReg(Region const* rg)
{
    ASSERT0(rg && rg->getPassMgr());
    LinearScanRA const* lsra = (LinearScanRA const*)rg->getPassMgr()->
        queryPass(PASS_LINEAR_SCAN_RA);
    if (lsra == nullptr) { return; }
    lsra->dumpBBListWithReg();
}


void dumpDOTWithReg(Region const* rg, CHAR const* name, UINT flag)
{
    ASSERT0(rg && rg->getPassMgr());
    LinearScanRA const* lsra = (LinearScanRA const*)rg->getPassMgr()->
        queryPass(PASS_LINEAR_SCAN_RA);
    if (lsra == nullptr) { return; }
    lsra->dumpDOTWithReg(name, flag);
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


//Determine an edge is backward jump or not.
static bool isBackwardJump(
    Vector<UINT> const& bb_seqid, UINT src_bbid, UINT dst_bbid)
{
    return bb_seqid[dst_bbid] <= bb_seqid[src_bbid];
}


void LexBackwardJumpAnalysis::collectBackwardJumps(Vector<UINT> const& bb_seqid)
{
    BBListIter bbit;
    for (IRBB const* bb = m_bb_list->get_head(&bbit);
         bb != nullptr; bb = m_bb_list->get_next(&bbit)) {
        AdjVertexIter ito;
        for (Vertex const* o = Graph::get_first_out_vertex(bb->getVex(), ito);
             o != nullptr; o = Graph::get_next_out_vertex(ito)) {
            if (!isBackwardJump(bb_seqid, bb->id(), o->id())) {
                //Edge bb->o is not lex-backward-jump.
                continue;
            }
            addBackwardJump(bb, m_rg->getBB(o->id()));
        }
    }
}


void LexBackwardJumpAnalysis::generateOccurenceForBB(
    IRBB const* bb, MOD Pos & pos)
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


void LexBackwardJumpAnalysis::assignLexSeqIdForBB(OUT Vector<UINT> & bb_seqid)
{
    //Iterate BB list.
    BBListIter bbit;
    bb_seqid.set(m_bb_list->get_elem_count(), 0);
    UINT id = 0;
    for (IRBB * bb = m_bb_list->get_head(&bbit); bb != nullptr;
         bb = m_bb_list->get_next(&bbit)) {
        bb_seqid.set(bb->id(), id++);
    }
}


bool LexBackwardJumpAnalysis::analyze()
{
    START_TIMER_FMT(t, ("Backward Jump Analysis"));
    m_live_mgr = (LivenessMgr*)m_rg->getPassMgr()->
        queryPass(PASS_PRLIVENESS_MGR);
    m_bb_list = m_rg->getBBList();
    if (m_bb_list == nullptr || m_bb_list->get_elem_count() == 0) {
        END_TIMER_FMT(t, ("Backward Jump Analysis"));
        return true;
    }
    Vector<UINT> bb_seqid;
    assignLexSeqIdForBB(bb_seqid);

    //Step1: Collect the backward jump info in the CFG.
    collectBackwardJumps(bb_seqid);

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
void LexBackwardJumpAnalysis::insertFakeUseAtBBEntry(
    IRBB const* bb, PRNO prno, BBPos const& pos)
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
    PosAttr * attr = m_resource_mgr->genPosAttr(POS_ATTR_NO_CODE_GEN, bb, st);
    setPosAttr(pos, attr, m_pos2attr);
    dumpFakeUse(m_lsra, st, nullptr, prno, bb->id(), true,
                "backward jump analysis");
}


//This func inserts the fake-use IR at BB exit per the steps below:
//  1. Build a fake-use IR by write the pr into a fake var.
//  2. Insert the fake-use IR at the tail of BB, but before the branch IR.
//  3. Map the pos and the new fake-use IR with attribute POS_ATTR_NO_CODE_GEN.
void LexBackwardJumpAnalysis::insertFakeUseAtBBExit(
    IRBB const* bb, PRNO prno, BBPos const& pos)
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
    PosAttr * attr = m_resource_mgr->genPosAttr(POS_ATTR_NO_CODE_GEN, bb, st);
    setPosAttr(pos, attr, m_pos2attr);
    dumpFakeUse(m_lsra, st, nullptr, prno, bb->id(), false,
                "backward jump analysis");
}


void LexBackwardJumpAnalysis::insertFakeUse(
    IRBB const* bb, PRNO prno, INSERT_MODE mode)
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


void LexBackwardJumpAnalysis::recordFakeUse(
    PRNO prno, IRBB const* bb, INSERT_MODE mode)
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
            if (m_lsra->isPreAssigned(pr) &&
                (m_lsra->getPreAssignedReg(pr) == m_lsra->getZeroScalar() ||
                m_lsra->getPreAssignedReg(pr) == m_lsra->getZeroVector())) {
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
            if (m_lsra->getPreAssignedMgr().isPreAssigned(pr)) { continue; }
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


void RegSetImpl::pickOutAliasRegSet(Reg reg, OUT RegSet & alias_set)
{
    ASSERT0(reg != REG_UNDEF);
    ASSERTN(0, ("Target Dependent Code"));
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


Reg RegSetImpl::handleOnlyConsistency(
    OUT RegSet & set, PRNOConstraintsTab const& consist_prs)
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


void RegSetImpl::removeConflictingReg(
    OUT RegSet & set, PRNOConstraintsTab const& conflict_prs,
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


Reg RegSetImpl::handleOnlyConflicts(
    OUT RegSet & set, PRNOConstraintsTab const& conflict_prs)
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


Reg RegSetImpl::pickRegWithConstraints(
    OUT RegSet & set, LTConstraints const* lt_constraints)
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
    ASSERT0(m_ra.isPreAssignedReg(r));
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
    ASSERT0(m_ra.isPreAssignedReg(r));
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


bool RegSetImpl::isCalleeScalar(Reg r) const
{
    //True if input register is callee saved scalar register. Note that the
    //frame pointer register can be used as callee saved register if this
    //register can be allocated and the dynamic stack function has not used it.
    //Note that special handling can be done based on the architecture.
    bool is_callee_scalar = m_target_callee_scalar != nullptr &&
        m_target_callee_scalar->is_contain(r);
    //GCOVR_EXCL_START
    bool use_fp_as_callee_scalar = isFP(r) && m_ra.isFPAllocable();
    //GCOVR_EXCL_STOP
    return is_callee_scalar || use_fp_as_callee_scalar;
}


void RegSetImpl::collectOtherAvailableRegister()
{
    //Collect FP register.
    //If the frame pointer register can be allocated and the dynamic stack
    //function and prologue&epilogue inserter function have not used this
    //register during the current compilation process, it can be used to
    //participate in scalar callee saved register allocation.
    //In addition, if debug mode is turned on, the frame pointer register
    //has a special role and cannot participate in allocation. Also, it can
    //not be used if debugging linear scan register allocation.
    if (m_ra.isFPAllocable()) {
        Reg reg = getTIMgr().getFP();
        m_avail_callee_scalar.bunion(reg);
        m_avail_allocable.bunion(reg);
    }
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
//START FakeIRMgr
//
void FakeIRMgr::removeFakeUseIR()
{
    //Remove the fake-use IR with no code gen attribute after register
    //assignment.
    PosAttrNoCodeGenProc no_code_gen_proc(m_rg, m_pos2attr, m_lsra);
    no_code_gen_proc.process();
}
//END FakeIRMgr


//
//START LTInterfGraph
//
void LTInterfGraphLSRAChecker::build()
{
    erase();
    LTListIter it1;
    LTList const& ltlst = m_lt_mgr.getLTList();
    for (LifeTime * lt1 = ltlst.get_head(&it1);
         lt1 != nullptr; lt1 = ltlst.get_next(&it1)) {
        addVertex(lt1->getPrno());
        LTListIter it2;
        for (LifeTime * lt2 = ltlst.get_head(&it2);
             lt2 != nullptr; lt2 = ltlst.get_next(&it2)) {
            if (lt1->getPrno() == lt2->getPrno()) { continue; }
            if (lt1->is_intersect(lt2)) {
                addEdge(lt1->getPrno(), lt2->getPrno());
                continue;
            }
            addVertex(lt2->getPrno());
        }
    }
}


bool LTInterfGraphLSRAChecker::check(LinearScanRA * lsra)
{
    //Build the interference graph first.
    build();

    //Check the LSRA result by the following steps on the interference graph:
    // 1. Traverse each edge of the interference graph.
    // 2. If the regsiters assigned to the src node and dst node is the ZERO
    //    register, check the next edge.
    // 3. If the prno responding to the src or dst node is not participated
    //    into the LSRA (e.g: callee saved registers), or the lifetime has
    //    no def occurence, check the next edge.
    xcom::EdgeIter it;
    for (xcom::Edge * e = get_first_edge(it); e != nullptr;
         e = get_next_edge(it)) {
        ASSERT0(e->from()->id() != PRNO_UNDEF);
        ASSERT0(e->to()->id() != PRNO_UNDEF);
        ASSERT0(e->from() != e->to());
        if (lsra->getReg(e->from()->id()) == lsra->getZeroScalar() ||
            lsra->getReg(e->from()->id()) == lsra->getZeroVector() ||
            lsra->getReg(e->to()->id()) == lsra->getZeroScalar() ||
            lsra->getReg(e->to()->id()) == lsra->getZeroVector()) {
           //Implemented the step 2 above.
           continue;
        }
        if (!m_lt_mgr.getLifeTime(e->from()->id()) ||
            !m_lt_mgr.getLifeTime(e->from()->id())->isOccHasDef() ||
            !m_lt_mgr.getLifeTime(e->to()->id()) ||
            !m_lt_mgr.getLifeTime(e->to()->id())->isOccHasDef()) {
            //Implemented the step 3 above.
            continue;
        }
        ASSERT0(lsra->getReg(e->from()->id()) != lsra->getReg(e->to()->id()));
    }
    return true;
}
//END LTInterfGraph


//
//START IRGroup
//
void IRGroup::modifyIRVar(Var const* v)
{
    ASSERT0(v);
    Var * var = const_cast<Var*>(v);
    for (UINT i = 0; i < getIRCnt(); i++) {
        IR * ir = irvec.get(i);
        if (ir->is_st()) {
            ir->setIdinfo(var);
            continue;
        }
        ASSERT0(ir->is_stpr());
        ir->getRHS()->setIdinfo(var);
    }
}


void IRGroup::renameIRVar()
{
    ASSERT0(tmp_var);
    modifyIRVar(tmp_var);
}


void IRGroup::revertIRVar()
{
    ASSERT0(org_var);
    modifyIRVar(org_var);
}


void IRGroup::dump(Region * rg) const
{
    note(rg, "\n    IRGroup:IR num:%u,weight:%u,vid = %u,temp_vid:%u,reg:%u",
        getIRCnt(), weight, org_var->id(), tmp_var ? tmp_var->id() : 0, reg);
    note(rg, "[");
    for (VecIdx j = 0; j < (VecIdx)irvec.get_elem_count(); j++) {
        note(rg, "id:%u", irvec.get(j)->id());
        if (irvec.get(j)->is_st()) {
            note(rg, "(st)");
        } else if (irvec.get(j)->is_stpr()) {
            note(rg, "(ld)");
        } else {
            ASSERT0(0);
        }
        if (j != (VecIdx)irvec.get_elem_count() - 1) {
            note(rg, ", ");
        }
    }
    note(rg, "]");
}
//END IRGroup


//
//START VarGroups
//
void VarGroups::dump(Region * rg) const
{
    rg->getLogMgr()->incIndent(2);
    note(rg, "\n===Dump VarGroups===");
    note(rg, "\nGroup_number = %u, is_full:%d", getGroupNum(),
         isFullGroup());
    rg->getLogMgr()->decIndent(2);
    for (VecIdx i = 0; i < (VecIdx)getGroupNum(); i++) {
        note(rg, "\n  Sub Group id:%u", i);
        IRGroup * irgp = m_groups->get(i);
        irgp->dump(rg);
    }
}
//END VarGroups


//
//START SpillReloadPromote
//
SpillReloadPromote::SpillReloadPromote(Region * rg, LinearScanRA * ra,
    RegSetImpl & rsimpl) : m_rg(rg), m_lsra(ra), m_rsimpl(rsimpl),
    m_mdssamgr(nullptr), m_var_liveness_mgr(rg, *ra),
    m_var_ltmgr(rg, *ra, &m_var_liveness_mgr)
{
    ASSERT0(rg != nullptr);
    m_bb_list = m_rg->getBBList();
}


SpillReloadPromote::~SpillReloadPromote()
{
    //Free the resources allocated.
    for (VarGroups * vargps = m_vargroups_list.get_head(); vargps != nullptr;
         vargps = m_vargroups_list.get_next()) {
        delete vargps;
    }
    for (IRGroup * irgp = m_irgroup_list.get_head(); irgp != nullptr;
         irgp = m_irgroup_list.get_next()) {
        delete irgp;
    }
}


void SpillReloadPromote::dumpFullGroup() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(m_rg, "\n===Dump FULL GROUPS===");
    note(m_rg, "\nvar number = %u", m_full_groups.get_elem_count());
    Var2GroupsIter it;
    VarGroups * gps = nullptr;
    note(m_rg, "\nVar Groups Map:\n");
    for (Var const* v = m_full_groups.get_first(it, &gps); gps != nullptr;
         v = m_full_groups.get_next(it, &gps)) {
        note(m_rg, "\n==Groups for var:[%s,%u]==", v->get_name()->getStr(),
            v->id());
        gps->dump(m_rg);
    }
}


void SpillReloadPromote::dumpPartialGroup() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    Var2GroupsIter it;
    VarGroups * gps = nullptr;
    note(m_rg, "\n===Dump PART GROUPS===");
    note(m_rg, "\nvar number = %u", m_part_groups.get_elem_count());
    note(m_rg, "\nVar Groups Map:\n");
    for (Var const* v = m_part_groups.get_first(it, &gps); gps != nullptr;
         v = m_part_groups.get_next(it, &gps)) {
        note(m_rg, "\n==Groups for var:[%s,%u]==", v->get_name()->getStr(),
            v->id());
        gps->dump(m_rg);
    }
}


void SpillReloadPromote::dumpSingleGroup() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    Var2GroupsIter it;
    VarGroups * gps = nullptr;
    note(m_rg, "\n===Dump SINGLE GROUPS===");
    note(m_rg, "\nvar number = %u", m_single_groups.get_elem_count());
    note(m_rg, "\nVar Groups Map:\n");
    for (Var const* v = m_single_groups.get_first(it, &gps); gps != nullptr;
         v = m_single_groups.get_next(it, &gps)) {
        note(m_rg, "\n==Groups for var:[%s,%u]==", v->get_name()->getStr(),
            v->id());
        gps->dump(m_rg);
    }
}


void SpillReloadPromote::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    dumpFullGroup();
    dumpPartialGroup();
    dumpSingleGroup();
}


void SpillReloadPromote::genDUInfo(OptCtx & oc)
{
    ASSERT0(m_rg->getCFG()->verifyRPO(oc));
    ASSERT0(m_rg->getCFG()->verifyDomAndPdom(oc));
    ASSERT0(oc.is_rpo_valid());
    ASSERT0L3(m_rg->getCFG()->verifyLoopInfo(oc));
    ASSERT0(oc.is_loopinfo_valid());

    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_DOM,
        PASS_LOOP_INFO, PASS_UNDEF);

    //FIXME START
    //Need the previous PASS to revise the following PASS_AA, PASS_MD_REF
    //information.
    bool do_aa = g_do_aa;
    bool do_md_du_analysis = g_do_md_du_analysis;
    bool compute_pr_du_chain = g_compute_pr_du_chain;
    bool compute_nonpr_du_chain = g_compute_nonpr_du_chain;
    bool compute_pr_du_chain_by_prssa = g_compute_pr_du_chain_by_prssa;

    g_do_aa = true;
    g_do_md_du_analysis = true;
    g_compute_pr_du_chain = true;
    g_compute_nonpr_du_chain = false;
    g_compute_pr_du_chain_by_prssa = true;
    oc.setInvalidPass(PASS_PRLIVENESS_MGR);
    oc.setInvalidPass(PASS_AA);
    oc.setInvalidPass(PASS_MD_REF);
    m_rg->doAA(oc);
    m_rg->doMDRefAndClassicDU(oc);

    m_mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->registerPass(
        PASS_MDSSA_MGR);
    m_mdssamgr->destruction(oc);
    m_mdssamgr->construction(oc);
    ASSERT0(oc.isPassValid(PASS_MDSSA_MGR));
    g_do_aa = do_aa;
    g_do_md_du_analysis = do_md_du_analysis;
    g_compute_pr_du_chain = compute_pr_du_chain;
    g_compute_nonpr_du_chain = compute_nonpr_du_chain;
    g_compute_pr_du_chain_by_prssa = compute_pr_du_chain_by_prssa;
}


void SpillReloadPromote::doFullGroupSpillIR(IR const* ir)
{
    ASSERT0(ir);
    ASSERT0(m_lsra->isSpillOp(ir));
    Var const* var = ST_idinfo(ir);
    ASSERT0(var);

    VarGroups * var_groups = getAndGenVarFullGroups(var);
    IRGroup * irgp = getAndGenIRGroupFromFullGroup(var_groups, var);
    irgp->addIR(ir);
}


void SpillReloadPromote::doFullGroupReloadIR(IR const* ir)
{
    ASSERT0(ir);
    ASSERT0(m_lsra->isReloadOp(ir));
    Var const* var = LD_idinfo(ir->getRHS());
    ASSERT0(var);

    VarGroups * var_groups = getAndGenVarFullGroups(var);
    IRGroup * irgp = getAndGenIRGroupFromFullGroup(var_groups, var);
    irgp->addIR(ir);
}


void SpillReloadPromote::groupFully()
{
    for (IRBB * bb = m_bb_list->get_head(); bb != nullptr;
         bb = m_bb_list->get_next()) {
        for (IR * ir = BB_irlist(bb).get_head();
             ir != nullptr; ir = BB_irlist(bb).get_next()) {
            if (!m_lsra->isSpillOp(ir) && !m_lsra->isReloadOp(ir)) { continue; }
            if (m_lsra->isSpillOp(ir)) {
                doFullGroupSpillIR(ir);
                continue;
            }
            doFullGroupReloadIR(ir);
        }
    }
}


bool SpillReloadPromote::getRegToCoverRange(Range const& tgt_range,
    Type const* reg_ty, OUT Reg & best_reg, MOD xcom::TTab<Reg> & reg_tab)
{
    ASSERT0(reg_ty);
    //There may be more than one reg that meet the requirements. Thus this
    //function will find out all RangeInfo which contain resulted reg and
    //record to the 'reg_tab'.
    UINT min_dis = m_lsra->getRegLTMgr().getMaxPos();

    //Get the best reg according to specific Range. If all the ranges of the
    //lifetime of a reg aren't intersect with the 'tgt_range', it represents
    //this reg could be allocated to the 'tgt_range'. There may be more than
    //one reg that can allocated to this 'tgt_range' at the same time. Then
    //the distance between 'tgt_range' and other ranges in the lifetime of the
    //reg will be used as a decision maker. The shortest distance is regarded
    //as the best choice.
    //e.g.:
    // tgt_range    |            -----                     |
    // r1 range vec |  ---   ---       -    ---  --   ---  |
    // r2 range vec | -  --- -               ---- -----    |
    // r3 range vec | ---                            ----- |
    // And the best reg is 'r1'.

    //1.Collect all intervals of all regs.
    PRNO2LT const& reg2lt = m_lsra->getRegLTMgr().getReg2LT();
    for (Reg r = 0; r < reg2lt .get_elem_count(); r++) {
        LifeTime * lt = reg2lt.get(r);
        if (lt == nullptr || !m_rsimpl.isRegTypeMatch(reg_ty, r)) { continue; }

        //2.Find the interval that can hold the 'tgt_range' in the lifetime of
        //  the reg, and calculate the distance between 'tgt_range' and other
        //  ranges in the lifetime.
        for (UINT i = 0; i < lt->getRangeNum(); i++) {
            Range range = lt->getRange((VecIdx)i);
            //Case: 'tgt_range.end < range.start', the distance includes two
            //       parts as below.
            //  RangeVec:  |          [S E] [S E] |
            //  tgt_range: |   [S E]              |
            //               ^       ^
            //               |       |
            //             dis_1   dis_2
            if (i == 0 && (range.start() > tgt_range.end())) {
                UINT dis = range.start() - tgt_range.end();
                dis += tgt_range.start() - REGION_START_POS - 1;
                if (min_dis > dis) {
                    min_dis = dis;
                    best_reg = r;
                }
                reg_tab.append(r);
                break;
            }

            //Case: 'prev_range.end < start tgt_range end < range.start'.
            //       The distance includes two parts as below:
            //  prev_range:|[S E]                  |
            //  tgt_range: |        [S E]          |
            //  range:     |                 [S E] |
            //                    ^        ^
            //                    |        |
            //                  dis_1    dis_2
            if (i > 0) {
                Range prev_range = lt->getRange((VecIdx)(i - 1));
                if (prev_range.end() < tgt_range.start() &&
                    range.start() > tgt_range.end()) {
                    ASSERT0(prev_range.is_less(tgt_range));
                    UINT dis = tgt_range.start() - prev_range.end();
                    dis += range.start() - tgt_range.end();
                    if (min_dis > dis) {
                        min_dis = dis;
                        best_reg = r;
                    }
                    reg_tab.append(r);
                    break;
                }
            }

            //Case: 'range.end < tgt_range.start', the distance includes two
            //      parts as below:
            //  tgt_range: |        [S E]     |
            //  range    : | [S E]            |
            //                     ^        ^
            //                     |        |
            //                  dis_1    dis_2
            if (i == (UINT)lt->getLastRangeIdx() &&
                range.end() < tgt_range.start()) {
                UINT dis = tgt_range.start() - range.end();
                dis += m_lsra->getRegLTMgr().getMaxPos() - tgt_range.end();
                if (min_dis > dis) {
                    min_dis = dis;
                    best_reg = r;
                }
                reg_tab.append(r);
                break;
            }
        }
    }

    //3.There isn't any reg that can hold this 'tgt_range'.
    if (reg_tab.get_elem_count() == 0) { return false; }
    return true;
}


bool SpillReloadPromote::getBestRegToAccommodateRange(
    RangeVec const& rv, Type const* ty, OUT Reg & best_reg,
    MOD xcom::TTab<Reg> & reg_tab)
{
    ASSERT0(ty);
    ASSERT0(rv.get_elem_count() > 0);
    reg_tab.clean();

    //Construct a range to include the positions in 'rv'.
    UINT start = rv.get(0).start();
    UINT end = rv.get(rv.get_last_idx()).end();
    Range full_range(start, end);

    //First, try to find if there is any register can hold the full range of
    //the 'rv'.
    bool find = getRegToCoverRange(full_range, ty, best_reg, reg_tab);

    //Second, find the register can interleaves with the separate
    //range in the range vector, we will drop the best register found before,
    //because the 'hole' of resgiter is fully utilized, we have a better usage
    //of the register.
    //e.g: r2 is better than r1 to hold the target range vector.
    // target range vec|            -----       ---           |
    // r1 range vec    |  ---   ---                      ---  |
    // r2 range vec    | -  --- -          ---        -----   |
    PRNO2LT const& reg2lt = m_lsra->getRegLTMgr().getReg2LT();
    for (Reg r = 0; r < reg2lt.get_elem_count(); r++) {
        if (reg_tab.find(r)) { continue; }
        LifeTime * lt = reg2lt.get(r);
        if (lt == nullptr || !m_rsimpl.isRegTypeMatch(ty, r)) { continue; }
        if (!lt->is_intersect(rv)) {
            best_reg = r;
            return true;
        }
    }
    return find;
}


void SpillReloadPromote::promoteReloadOpByReg(IR * curir, Reg r, PRNO pr)
{
    ASSERT0(curir);
    ASSERT0(m_lsra->isReloadOp(curir));
    ASSERT0(r != REG_UNDEF);
    ASSERT0(pr != PRNO_UNDEF);
    PRNO dstpr = curir->getPrno();
    IR * stpr = m_rg->getIRMgr()->buildMove(dstpr, pr, curir->getType());
    m_lsra->setMove(stpr);
    IRBB * bb = curir->getBB();
    bb->getIRList().insert_before(stpr, curir);
    bb->getIRList().remove(curir);
    m_lsra->removeReloadOp(curir);
    m_rg->freeIRTree(curir);
}


void SpillReloadPromote::promoteSpillOpByReg(
    IR * curir, Reg r, PRNO pr, bool remove)
{
    ASSERT0(m_lsra->isSpillOp(curir));
    ASSERT0(r != REG_UNDEF);
    ASSERT0(pr != PRNO_UNDEF);
    PRNO srcpr = curir->getRHS()->getPrno();
    IR * stpr = m_rg->getIRMgr()->buildMove(pr, srcpr, curir->getType());
    m_lsra->setMove(stpr);
    IRBB * bb = curir->getBB();
    bb->getIRList().insert_before(stpr, curir);

    if (!remove) { return; }

    //If this spill is not shared with other reload IR, it should be removed.
    bb->getIRList().remove(curir);
    m_lsra->removeSpillOp(curir);
    m_rg->freeIRTree(curir);
}


UINT SpillReloadPromote::calcWeightForGroup(IRVec const* irvec) const
{
    ASSERT0(irvec);
    LI<IRBB> const* li = m_rg->getCFG()->getLoopInfo();
    UINT weight = 0;
    UINT const reload_wt = getTIMgr()->getLoadOnChipMemCycle();
    UINT const spill_wt = getTIMgr()->getStoreOnChipMemCycle();

    //Calc the weight of group based on the different wreight of spill/reload
    //IRs, and also consider the loop level.
    for (VecIdx i = 0; i < (VecIdx)irvec->get_elem_count(); i++) {
        IR const* ir = irvec->get(i);
        UINT nestlevel = 0;
        if (li != nullptr) {
            li->isInsideLoopTree(ir->getBB()->id(), nestlevel, true);
        }
        nestlevel++;
        if (m_lsra->isReloadOp(irvec->get(i))) {
            weight += reload_wt;// * nestlevel;
            continue;
        }
        ASSERT0(m_lsra->isSpillOp(irvec->get(i)));
        weight += spill_wt;// * nestlevel;
    }
    return weight;
}


class GroupSort : public xcom::BubbleSort<IRGroup*> {
    virtual bool GreatThan(IRGroup * A, IRGroup * B) const override
    { return IRGROUP_weight(A) < IRGROUP_weight(B); }
};


void SpillReloadPromote::sortFullGroup()
{
    Var2GroupsIter it;
    VarGroups * gps = nullptr;
    for (Var const* v = m_full_groups.get_first(it, &gps); gps != nullptr;
         v = m_full_groups.get_next(it, &gps)) {
        ASSERT0(v);
        for(UINT i = 0; i < gps->getGroupNum(); i++) {
            IRGroup * irgp = gps->getIRGroup(i);
            ASSERT0(irgp);
            IRGROUP_weight(irgp) = calcWeightForGroup(irgp->getIRVec());
            addToFullGroupVec(irgp);
        }
    }
    //Sort the group by the order of weight, because we will assign the
    //register for the group which has the highest weight..
    GroupSort group_sort;
    group_sort.sort(m_full_group_vec);
}


void SpillReloadPromote::sortPartialGroup()
{
    Var2GroupsIter it;
    VarGroups * gps = nullptr;
    for (Var const* v = m_part_groups.get_first(it, &gps); gps != nullptr;
         v = m_part_groups.get_next(it, &gps)) {
        ASSERT0(v);
        if (gps->isFullGroup()) { continue; }
        for(UINT i = 0; i < gps->getGroupNum(); i++) {
            IRGroup * irgp = gps->getIRGroup(i);
            ASSERT0(irgp);
            IRGROUP_weight(irgp) = calcWeightForGroup(irgp->getIRVec());
            addToValidPartialGroup(irgp);
        }
    }
    //Sort the group by the order of weight, because we will assign the
    //register for the group which has the highest weight..
    GroupSort group_sort;
    group_sort.sort(m_valid_partial_groups);
}


void SpillReloadPromote::sortSingleGroup(UINT id)
{
    m_valid_single_groups.clean();
    Var2GroupsIter it;
    VarGroups * gps = nullptr;
    for (Var const* v = m_single_groups.get_first(it, &gps); gps != nullptr;
         v = m_single_groups.get_next(it, &gps)) {
        ASSERT0(v);
        if (gps->isFullGroup()) { continue; }
        if (id >= gps->getGroupNum()) { continue; }
        IRGroup * irgp = gps->getIRGroup(id);
        ASSERT0(irgp);
        IRGROUP_weight(irgp) = calcWeightForGroup(irgp->getIRVec());
        addToValidSingleGroup(irgp);
    }
    //Sort the group by the order of weight, because we will assign the
    //register for the group which has the highest weight.
    GroupSort group_sort;
    group_sort.sort(m_valid_single_groups);
}


void SpillReloadPromote::promoteSpillReloadForGroup(
    IRGroup * gp, bool remove_spill)
{
    ASSERT0(gp);
    Reg r = IRGROUP_reg(gp);
    ASSERT0(r != REG_UNDEF);

    IRVec const* irvec = gp->getIRVec();
    ASSERT0(irvec);
    ASSERT0(irvec->get_elem_count() > 1);

    PRNO pr = m_lsra->buildPrnoAndSetReg(IRGROUP_orgvar(gp)->getType(), r);
    for (VecIdx i = 0; i < (VecIdx)irvec->get_elem_count(); i++) {
        IR * ir = irvec->get(i);
        ASSERT0(ir);
        ASSERT0(m_lsra->isReloadOp(ir) || m_lsra->isSpillOp(ir));
        if (m_lsra->isSpillOp(ir)) {
            promoteSpillOpByReg(ir, r, pr, remove_spill);
            continue;
        }
        promoteReloadOpByReg(ir, r, pr);
    }
}


void SpillReloadPromote::promoteSpillReloadPartialGroup()
{
    for (VecIdx i = 0; i < (VecIdx)m_valid_partial_groups.get_elem_count();
         i++) {
        IRGroup * gp = m_valid_partial_groups.get(i);
        ASSERT0(gp);
        if (IRGROUP_reg(gp) == REG_UNDEF) { continue; }
        promoteSpillReloadForGroup(gp, false);
    }
}


void SpillReloadPromote::promoteSpillReloadSingleGroup()
{
    Var2GroupsIter it;
    VarGroups * gps = nullptr;
    for (Var const* v = m_single_groups.get_first(it, &gps); gps != nullptr;
         v = m_single_groups.get_next(it, &gps)) {
        ASSERT0(v);
        //If this group is full group, that means it has been try to promote
        //in full group mode, so we don't need to try again.
        if (gps->isFullGroup()) { continue; }
        for (VecIdx i = 0; i < (VecIdx)gps->getGroupNum(); i++) {
            IRGroup * gp = gps->getIRGroup(i);
            ASSERT0(gp);
            if (IRGROUP_reg(gp) == REG_UNDEF) { continue; }

            //Promote for the groups which have been found a proper register
            //to do the promotion.
            promoteSpillReloadForGroup(gp, false);
        }
    }
}


void SpillReloadPromote::promoteSpillReloadFullGroup()
{
    for (VecIdx i = 0; i < (VecIdx)m_full_group_vec.get_elem_count(); i++) {
        IRGroup * gp = m_full_group_vec.get(i);
        ASSERT0(gp);
        IRVec const* irvec = gp->getIRVec();
        if (irvec->get_elem_count() == 1) {
            //If the group has only one IR, that means it is useless.
            addUnusedIR(irvec->get(0));
            continue;
        }
        if (IRGROUP_reg(gp) == REG_UNDEF) { continue; }
        promoteSpillReloadForGroup(gp, true);
    }
}


UINT SpillReloadPromote::getMaxSingleGroupNum()
{
    Var2GroupsIter it;
    VarGroups * gps = nullptr;
    UINT max = 0;
    for (Var const* v = m_single_groups.get_first(it, &gps); gps != nullptr;
         v = m_single_groups.get_next(it, &gps)) {
        ASSERT0(v);
        //Ignore the group of spill var if it is a full group.
        if (gps->isFullGroup()) { continue; }
        if (max < gps->getGroupNum()) { max = gps->getGroupNum(); }
     }
     return max;
}


Reg SpillReloadPromote::tryAssignRegForGroup(UINT vid, IRVec const* irvec,
    MOD xcom::TTab<Reg> & reg_tab)
{
    ASSERT0(irvec);
    VarLifeTime * vlt = getVarLTMgr().getLifeTime(vid);
    ASSERT0(vlt);
    //If the current group only has one IR, that means this IR is useless,
    //it should be removed, so it is not necessary to find a register for
    //this group.
    if (irvec->get_elem_count() == 1) { return REG_UNDEF; }

    //Check che current vlt is conflict with the var lifetime assigned with
    //this reg.
    RangeVec const& range_vec = vlt->getRangeVec();
    Reg best_reg = REG_UNDEF;
    bool find = getBestRegToAccommodateRange(range_vec,
        irvec->get(0)->getType(), best_reg, reg_tab);

    //If there is no suitable register, return.
    if (!find) { return REG_UNDEF; }

    //Record the regsiter if it is a callee saved.
    recordCallee(best_reg);

    //Add the live range of the current group to the lifetime of the register
    //to be promoted.
    m_lsra->getRegLTMgr().mergeRegLifetimeWIthRange(best_reg, range_vec);
    return best_reg;
}


void SpillReloadPromote::findRegForFullGroupVars()
{
    xcom::TTab<Reg> reg_tab;
    for (VecIdx i = 0; i < (VecIdx)m_full_group_vec.get_elem_count();
         i++) {
        IRGroup * gp = m_full_group_vec.get(i);
        ASSERT0(gp);
        Reg r = tryAssignRegForGroup(IRGROUP_orgvar(gp)->id(),
            gp->getIRVec(), reg_tab);
        if (r == REG_UNDEF) { continue; }
        IRGROUP_reg(gp) = r;
        m_vartab.append(IRGROUP_orgvar(gp));
    }
}


void SpillReloadPromote::findRegForPartialGroupVars()
{
    xcom::TTab<Reg> reg_tab;
    for (VecIdx i = 0; i < (VecIdx)m_valid_partial_groups.get_elem_count();
         i++) {
        IRGroup * gp = m_valid_partial_groups.get(i);
        ASSERT0(gp);
        Reg r = tryAssignRegForGroup(IRGROUP_tmpvar(gp)->id(),
            gp->getIRVec(), reg_tab);
        if (r == REG_UNDEF) { continue; }
        IRGROUP_reg(gp) = r;
    }
}


void SpillReloadPromote::findRegForSingleGroupVars()
{
    xcom::TTab<Reg> reg_tab;
    for (VecIdx i = 0; i < (VecIdx)m_valid_single_groups.get_elem_count();
         i++) {
        IRGroup * gp = m_valid_single_groups.get(i);
        ASSERT0(gp);
        Reg r = tryAssignRegForGroup(IRGROUP_tmpvar(gp)->id(),
            gp->getIRVec(), reg_tab);
        if (r == REG_UNDEF) { continue; }
        IRGROUP_reg(gp) = r;
    }
}


void SpillReloadPromote::genVar2DLifeTime(OptCtx & oc)
{
    m_var_liveness_mgr.perform(oc);
    getVarLTMgr().reset();
    getVarLTMgr().computeLifeTime();
}


IRGroup * SpillReloadPromote::genIRGroupForVar(
    MOD VarGroups * var_groups, Var const* var)
{
    ASSERT0(var_groups && var);

    //Get the current number of element as the new group id.
    UINT group_id = var_groups->getGroupNum();
    IRGroup * ir_group = allocIRGroup();
    IRGROUP_orgvar(ir_group) = var;
    var_groups->addGroup(group_id, ir_group);
    return ir_group;
}


void SpillReloadPromote::groupReloadInUseSet(
    IRSet const& useset, MOD IRGroup * ir_group)
{
    ASSERT0(ir_group);
    IRSetIter it;
    for (BSIdx i = useset.get_first(&it);
         i != BS_UNDEF; i = useset.get_next(i, &it)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use && !use->is_undef());

        //Continue if the use is PHI.
        if (use->is_id()) { continue; }

        IR const* ref_stmt = use->getStmt();
        ASSERT0(m_lsra->isReloadOp(ref_stmt));
        ir_group->addIR(ref_stmt);
    }
}


void SpillReloadPromote::groupIRPartially(IR const* ir, MOD IRSet & useset)
{
    ASSERT0(ir);
    ASSERT0(m_lsra->isSpillOp(ir));
    ASSERT0(m_mdssamgr && m_mdssamgr->is_valid());

    Var const* var = ST_idinfo(ir);
    ASSERT0(var);

    //If this var has already been promoted in the full group mode, so it is
    //not necessary to do the partial group promotion.
    if (isPromotedInFullGroup(var)) { return; }

    //Do NOT do collection crossing PHI.
    m_mdssamgr->collectUseSet(ir, COLLECT_IMM_USE, &useset);
    ASSERT0(useset.allElemBeExp(m_rg));

    //Generate the groups for  var.
    VarGroups * var_groups = getAndGenVarPartialGroups(var);

    //Check the define of spill var by this spill IR has wether has the
    //exclude use IR (reload IR) or not, that means this reload IR only
    //reloads the data from spill var after this spill IR. Because it
    //is normal that a reload IR may reload the multiple define of spill
    //var by different spill IRs due to the join of BBs in CFG.
    bool has_exclude_use = false;
    IRSetIter it;
    for (BSIdx i = useset.get_first(&it);
         i != BS_UNDEF; i = useset.get_next(i, &it)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use && !use->is_undef());
        //Continue if the use is PHI.
        if (!use->is_id()) {
            ASSERT0(m_lsra->isReloadOp(use->getStmt()));
            has_exclude_use = true;
            break;
        }
    }
    if (!has_exclude_use) {
        VARGROUPS_has_all(var_groups) = false;
        return;
    }

    //When goes here, that means this spill IR must be assigned a new
    //group id with its exclusive uses.
    IRGroup * ir_group = genIRGroupForVar(var_groups, var);
    ir_group->addIR(ir);

    //Group the use-exps.
    groupReloadInUseSet(useset, ir_group);
}


void SpillReloadPromote::groupPartially()
{
    DefMiscBitSetMgr sm;
    IRSet useset(sm.getSegMgr());
    for (IRBB * bb = m_bb_list->get_head(); bb != nullptr;
         bb = m_bb_list->get_next()) {
        for (IR * ir = BB_irlist(bb).get_head();
             ir != nullptr; ir = BB_irlist(bb).get_next()) {
            if (!m_lsra->isSpillOp(ir)) { continue; }
            useset.clean();
            groupIRPartially(ir, useset);
        }
    }
}


void SpillReloadPromote::genSingleVarGroupFromPartialIRGroup(
    IRGroup * irgp, Var const* v)
{
    ASSERT0(irgp && v);

    IRVec const* irvec = irgp->getIRVec();
    VarGroups * var_groups = getAndGenVarSingleGroups(v);
    IR * spill_ir = nullptr;
    for (VecIdx j = 0; j < (VecIdx)irgp->getIRCnt(); j++) {
        IR * cur_ir = irvec->get(j);
        if (m_lsra->isSpillOp(cur_ir)) {
            spill_ir = cur_ir;
            break;
        }
    }
    for (VecIdx j = 0; j < (VecIdx)irgp->getIRCnt(); j++) {
        IR * cur_ir = irvec->get(j);
        if (m_lsra->isSpillOp(cur_ir)) {
            continue;
        }
        IRGroup * ir_group = genIRGroupForVar(var_groups, v);
        ir_group->addIR(spill_ir);
        ir_group->addIR(cur_ir);
    }
}


void SpillReloadPromote::groupSingle()
{
    Var2GroupsIter it;
    VarGroups * gps = nullptr;

    //We get the single group of spill var based on the partial group result.
    //Because after the partial group mode, only the partial groups which were
    //not promoted with a proper register need to try in the last single group
    //mode.
    for (Var const* v = m_part_groups.get_first(it, &gps); gps != nullptr;
         v = m_part_groups.get_next(it, &gps)) {
        ASSERT0(v);
        //Ignore the group of var if it is a full group, because it has been
        //promoted in full group mode.
        if (gps->isFullGroup()) { continue; }
        for(UINT i = 0; i < gps->getGroupNum(); i++) {
            IRGroup * irgp = gps->getIRGroup(i);
            ASSERT0(irgp);
            if (irgp->getIRCnt() <= 2) {
                //If the IR number in a group is less than 2, that means the
                //spill or reload is not paired, it is useless, should be
                //removed. If it is equal to 2, that means it is same as the
                //single group, it has been tried on partial group mode.
                continue;
            }
            if (IRGROUP_reg(irgp) != REG_UNDEF) {
                //If it has been promoted in partial group mode, which means
                //a proper register has been assigned to this group, we can
                //ignore this group in single mode.
                continue;
             }

            //When goes here, we only process the partial group with IR count
            //greater than 2.
            //Gen the groups for the var.
            genSingleVarGroupFromPartialIRGroup(irgp, v);
        }
    }
}


void SpillReloadPromote::renameIRVar()
{
    xcom::Vector<IRGroup*> const* ir_gps = getIRGroups();
    ASSERT0(ir_gps);
    for (VecIdx i = 0; i < (VecIdx)ir_gps->get_elem_count(); i++) {
        IRGroup * irgp = ir_gps->get(i);
        Type const* ty = IRGROUP_orgvar(irgp)->getType();
        Var const* tmp = m_lsra->genFuncLevelVar(ty,
            m_rg->getTypeMgr()->getByteSize(ty));
        IRGROUP_tmpvar(irgp) = tmp;
        irgp->renameIRVar();
    }
}


void SpillReloadPromote::revertIRVar()
{
    xcom::Vector<IRGroup*> const* ir_gps = getIRGroups();
    ASSERT0(ir_gps);
    for (VecIdx i = 0; i < (VecIdx)ir_gps->get_elem_count(); i++) {
        IRGroup * irgp = ir_gps->get(i);
        irgp->revertIRVar();
    }
}


void SpillReloadPromote::destroyTmpVar()
{
    for (VecIdx i = 0; i < (VecIdx)m_valid_partial_groups.get_elem_count();
         i++) {
        IRGroup const* irgp = m_valid_partial_groups.get(i);
        Var * tmp = const_cast<Var*>(IRGROUP_tmpvar(irgp));
        ASSERT0(tmp);
        m_rg->getVarMgr()->destroyVar(tmp);
    }
}


void SpillReloadPromote::doPartialGroup(OptCtx & oc)
{
    setGroupMode(GROUP_MODE_PART);
    genDUInfo(oc);
    groupPartially();
    sortPartialGroup();
    renameIRVar();
    genVar2DLifeTime(oc);
    findRegForPartialGroupVars();
    revertIRVar();
}


void SpillReloadPromote::doSingleGroup(OptCtx & oc)
{
    setGroupMode(GROUP_MODE_SINGLE);
    groupSingle();
    for (UINT id = 0; id < getMaxSingleGroupNum(); id++) {
        sortSingleGroup(id);
        renameIRVar();
        genVar2DLifeTime(oc);
        findRegForSingleGroupVars();
        revertIRVar();
    }
}


void SpillReloadPromote::removeUnusedIR()
{
    for(VecIdx i = 0; i < (VecIdx)m_unused_irs.get_elem_count(); i++) {
        IR * ir = m_unused_irs.get(i);
        ASSERT0(ir);
        IRBB * bb = ir->getBB();
        bb->getIRList().remove(ir);
        if (m_lsra->isSpillOp(ir)) {
            m_lsra->removeSpillOp(ir);
        } else if (m_lsra->isReloadOp(ir)) {
            m_lsra->removeReloadOp(ir);
        } else {
            UNREACHABLE();
        }
        m_rg->freeIRTree(ir);
    }
}


void SpillReloadPromote::doFullGroup(OptCtx & oc)
{
    setGroupMode(GROUP_MODE_FULL);
    groupFully();
    sortFullGroup();
    genVar2DLifeTime(oc);
    findRegForFullGroupVars();
}


bool SpillReloadPromote::perform(OptCtx & oc)
{
    START_TIMER(t, "SpillReloadPromote");
    //Do the group first.
    doFullGroup(oc);
    doPartialGroup(oc);
    doSingleGroup(oc);

    //Do the promote after group.
    promoteSpillReloadFullGroup();
    promoteSpillReloadPartialGroup();
    promoteSpillReloadSingleGroup();

    //Remove the unsed spill or reload IRs.
    removeUnusedIR();
    destroyTmpVar();
    oc.setInvalidPRSSA();
    oc.setInvalidMDSSA();

    END_TIMER(t, "SpillReloadPromote");
    ASSERT0L3(m_rg->getCFG()->verifyLoopInfo(oc));
    ASSERT0(oc.is_loopinfo_valid());
    return m_unused_irs.get_elem_count() > 0;
}
//END SpillReloadPromote


//
//START LinearScanRA
//
LinearScanRA::LinearScanRA(Region * rg) : Pass(rg), m_act_mgr(rg)
{
    ASSERT0(rg != nullptr);
    m_cfg = nullptr;
    m_bb_list = nullptr;
    m_irmgr = rg->getIRMgr();
    m_func_level_var_count = 0;
    m_is_apply_to_region = false;
    m_is_fp_allocable_allowed = true;
    m_lt_mgr = nullptr;

    //Since some passes, such as ArgPass, might invoke the
    //APIs of LifeTimeMgr before LSRA's perform(), we initialize the
    //LifeTimeMgr ahead of time here.
    initLTMgr();

    ////////////////////////////////////////////////////////////////////////////
    // DO NOT ALLOCATE CLASS MEMBER HERE. INITIALIZE THEM AFTER ApplyToRegion.//
    ////////////////////////////////////////////////////////////////////////////
    m_lt_constraints_strategy = nullptr;
    m_lt_constraints_mgr = nullptr;
    m_fake_irmgr = nullptr;
    ////////////////////////////////////////////////////////////////////////////
    // DO NOT ALLOCATE CLASS MEMBER HERE. INITIALIZE THEM AFTER ApplyToRegion.//
    ////////////////////////////////////////////////////////////////////////////
}


LinearScanRA::~LinearScanRA()
{
    destroy();
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


void LinearScanRA::initLocalUsage()
{
    initFakeIRMgr();
}


void LinearScanRA::destroy()
{
    destroyLocalUsage();
    if (m_lt_mgr != nullptr) {
        delete m_lt_mgr;
        m_lt_mgr = nullptr;
    }
    if (m_reg_lt_mgr != nullptr) {
        delete m_reg_lt_mgr;
        m_reg_lt_mgr = nullptr;
    }
}


void LinearScanRA::destroyLocalUsage()
{
    //NOTE:Since other passes, such as ProEpiInserter, might invoke the
    //APIs of LifeTimeMgr, we retain the LifeTimeMgr untill the
    //pass object destroy.
    //if (m_lt_mgr != nullptr) {
    //    delete m_lt_mgr;
    //    m_lt_mgr = nullptr;
    //}
    if (m_fake_irmgr != nullptr) {
        delete m_fake_irmgr;
        m_fake_irmgr = nullptr;
    }
    if (m_lt_constraints_mgr != nullptr) {
        delete m_lt_constraints_mgr;
        m_lt_constraints_mgr = nullptr;
    }
    if (m_lt_constraints_strategy != nullptr) {
        delete m_lt_constraints_strategy;
        m_lt_constraints_strategy = nullptr;
    }
}


//Reset all resource before allocation.
void LinearScanRA::reset()
{
    getTIMgr().reset();
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
    if (getLTConstraintsMgr() != nullptr) {
        getLTConstraintsMgr()->reset();
    }
    if (m_lt_mgr != nullptr) {
        m_lt_mgr->reset();
    }
    if (m_reg_lt_mgr != nullptr) {
        m_reg_lt_mgr->reset();
    }

    //Set attributes obtained from other passes.
    DynamicStack * dynamic_stack = (DynamicStack*)m_rg->getPassMgr()->
        queryPass(PASS_DYNAMIC_STACK);
    ASSERT0(dynamic_stack);
    m_has_alloca = dynamic_stack->hasAlloca();
    m_may_need_to_realign_stack = dynamic_stack->mayRealignStack();
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

//Return physical register by given pre-assigned prno.
Reg LinearScanRA::getPreAssignedReg(PRNO prno) const
{
    Reg r = const_cast<LinearScanRA*>(this)->m_preassigned_mgr.get(prno);
    if (r != REG_UNDEF) {
        return r;
    }
    return const_cast<LinearScanRA*>(this)->m_dedicated_mgr.get(prno);
}


PRNO LinearScanRA::buildPrnoDedicated(Type const* type, Reg reg)
{
    ASSERT0(type);
    ASSERT0(reg != REG_UNDEF && isDedicatedReg(reg));
    PRNO prno = getDedicatedPRNO(reg);
    if (prno != PRNO_UNDEF) { return prno; }
    ASSERT0(m_irmgr);
    prno = m_irmgr->buildPrno(type);
    setPreAssignedReg(prno, reg);
    return prno;
}


PRNO LinearScanRA::buildPrnoPreAssigned(Type const* type, Reg reg)
{
    ASSERT0(type);
    ASSERT0(reg != REG_UNDEF);
    ASSERT0(m_irmgr);
    PRNO prno = m_irmgr->buildPrno(type);
    setPreAssignedReg(prno, reg);
    return prno;
}


PRNO LinearScanRA::buildPrnoAndSetReg(Type const* type, Reg reg)
{
    ASSERT0(type);
    ASSERT0(reg != REG_UNDEF);
    PRNO prno = isDedicatedReg(reg) ? buildPrnoDedicated(type, reg) :
        buildPrnoPreAssigned(type, reg);
    setReg(prno, reg);
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


void LinearScanRA::removeOpInPosGapRecord(IR const* ir)
{
    ASSERT0(isOpInPosGap(ir));
    if (isSpillOp(ir)) {
        removeSpillOp(const_cast<IR*>(ir));
        return;
    }
    if (isMoveOp(ir)) {
        removeMoveOp(const_cast<IR*>(ir));
        return;
    }
    if (isReloadOp(ir)) {
        removeReloadOp(const_cast<IR*>(ir));
        return;
    }
    if (isRematOp(ir)) {
        removeRematOp(const_cast<IR*>(ir));
        return;
    }
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
bool LinearScanRA::checkLTCanBeRematerialized(
    MOD LifeTime * lt, OUT RematCtx & rematctx)
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


void LinearScanRA::dumpDOTWithReg() const
{
    dumpDOTWithReg((CHAR const*)nullptr, IRCFG::DUMP_COMBINE);
}


void LinearScanRA::dumpDOTWithReg(CHAR const* name, UINT flag) const
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
    ASSERT0(m_cfg && m_cfg->is_valid());
    m_cfg->dumpDOT(name, flag, &ctx);
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
    m_preassigned_mgr.dump(m_rg, pthis->getTIMgr());
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
                                      OUT PreAssignedMgr & mgr)
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
    Reg tr = getTempReg(ty1_tmp);
    PRNO tmpprno = buildPrnoAndSetReg(ty1_tmp, tr);
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
    ASSERTN(m_lt_mgr, ("LifeTimeMgr is not initialized"));
    LifeTime const* lt = m_lt_mgr->getLifeTime(prno);
    if (lt == nullptr ) { return prno; }
    return lt->getAnctPrno();
}


bool LinearScanRA::isFPAllocable() const
{
    return !g_force_use_fp_as_sp && !xoc::g_debug && !hasAlloca() &&
        canPreservedFPInAlignStack() && isFPAllocableAllowed();
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


void LinearScanRA::generateRegLifeTime(OptCtx & oc)
{
    oc.setInvalidPass(PASS_PRLIVENESS_MGR);
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_RPO, PASS_DOM, PASS_PRLIVENESS_MGR, PASS_UNDEF);

    //Compute the lifetime firstly.
    VarUpdatePos up(this);
    getRegLTMgr().computeLifeTime(up, m_bb_list, m_preassigned_mgr);

    //Save the map between register and it's corresponded lifetime.
    LTListIter it;
    LTList const& lt_list = getRegLTMgr().getLTList();
    for (LifeTime * lt = lt_list.get_head(&it);
         lt != nullptr; lt = lt_list.get_next(&it)) {
        Reg reg = getReg(lt->getPrno());
        getRegLTMgr().mergeReg2LifeTime(reg, lt);
    }
}


void LinearScanRA::dumpRegLTOverview() const
{
    if (!getRegion()->isLogMgrInit()) { return; }

    xoc::note(m_rg, "\n==-- DUMP Reg2LifeTime in Region '%s' --==",
              m_rg->getRegionName());
    PRNO2LT const& reg2lt = m_reg_lt_mgr->getReg2LT();
    for (Reg r = 0; r < reg2lt.get_elem_count(); r++) {
        LifeTime * lt = reg2lt.get(r);
        if (lt == nullptr) { continue; }
        lt->dumpReg2LifeTime(m_rg, this, r);
    }
}


void LinearScanRA::dumpReg2LT(Pos start, Pos end, bool open_range) const
{
    PRNO2LT const& reg2lt = m_reg_lt_mgr->getReg2LT();
    for (Reg r = 0; r < reg2lt.get_elem_count(); r++) {
        LifeTime * lt = reg2lt.get(r);
        if (lt == nullptr) { continue; }
        lt->dumpReg2LifeTimeWithPos(m_rg, this, r, start, end, open_range);
    }
}


bool LinearScanRA::promoteSpillReload(OptCtx & oc, RegSetImpl & rsimpl)
{
    if (m_rg->getRegionName() == nullptr) { return false; }
    SpillReloadPromote spill_reload_promote(m_rg, this, rsimpl);
    return spill_reload_promote.perform(oc);
}


bool LinearScanRA::verifyLSRAByInterfGraph(OptCtx & oc) const
{
    START_TIMER(t, "verifyLSRAByInterfGraph");
    oc.setInvalidPass(PASS_PRLIVENESS_MGR);
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_RPO, PASS_DOM, PASS_PRLIVENESS_MGR, PASS_UNDEF);

    VarUpdatePos up(this);
    LifeTime2DMgr lt2d_mgr(m_rg, const_cast<LinearScanRA*>(this));
    lt2d_mgr.computeLifeTime(up, m_bb_list, m_preassigned_mgr);

    LTInterfGraphLSRAChecker graph(m_rg, lt2d_mgr);
    LinearScanRA * pthis = const_cast<LinearScanRA*>(this);
    ASSERT0(graph.check(pthis));
    END_TIMER(t, "verifyLSRAByInterfGraph");
    return true;
}


//TODO: rematerialization and spill-store-elimination
bool LinearScanRA::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO,
        PASS_DOM, PASS_PRLIVENESS_MGR, PASS_LOOP_INFO, PASS_UNDEF);

    //LSRA asks DynamicStack pass to determine whether the region has at least
    //one ALLOCA, thus the pass object is necesary. However the validation of
    //the pass is usually confirmed at Pass::perform() which does not only check
    //ALLOCA, but also supports the ALLOCA operations. Thus we expect that
    //DynamicAlloca has performed the detection of ALLOCA before entering
    //LSRA pass.
    m_rg->getPassMgr()->registerPass(PASS_DYNAMIC_STACK);
    reset();

    //Determine whether the PASS apply all modifications of CFG and BB to
    //current region. User may invoke LSRA as performance estimating tools
    //to conduct optimizations, such as RP, GCSE, UNROLLING which may increase
    //register pressure.
    ApplyToRegion apply(m_rg);
    checkAndPrepareApplyToRegion(apply);
    if (m_bb_list == nullptr || m_bb_list->get_elem_count() == 0) {
        set_valid(true);
        return false;
    }
    initLocalUsage();

    //Do the backward-jump analysis based on the CFG and liveness.
    doBackwardJumpAnalysis();

    UpdatePos up(this);
    collectDedicatedPR(m_bb_list, m_preassigned_mgr);
    ASSERT0(m_lt_mgr);
    getLTMgr().computeLifeTime(up, m_bb_list, m_preassigned_mgr);

    //After the lifetime calculation is completed, begin setting constraint
    //sets for each lifetime.
    tryComputeConstraints();

    //The remat info must be generated before the priority of the lifetime is
    //computed.
    genRematInfo();

    LTPriorityMgr priomgr(m_cfg, getTIMgr());
    priomgr.computePriority(getLTMgr());
    bool changed = performLsraImpl(oc);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLSRA()) {
        dump(false);
    }
    destroyLocalUsage();
    checkAndApplyToRegion(apply);
    ASSERTN(getRegion()->getCFG()->verifyRPO(oc),
            ("make sure original RPO is legal"));
    ASSERTN(getRegion()->getCFG()->verifyDomAndPdom(oc),
            ("make sure original DOM/PDOM is legal"));
    set_valid(true);
    if (!changed || !isApplyToRegion()) {
        ASSERT0(m_rg->getBBMgr()->verify());
        END_TIMER(t, getPassName());
        return false;
    }
    recalculateSSA(oc);
    oc.setInvalidLoopInfo();
    ASSERT0(m_rg->getBBMgr()->verify());
    END_TIMER(t, getPassName());
    return false;
}
//END LinearScanRA
} //namespace xoc
