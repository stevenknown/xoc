/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START BBIRList
//
//Insert ir after phi operations.
IRListIter BBIRList::append_head_ex(IR * ir)
{
    if (ir == nullptr) { return nullptr; }
    ASSERTN(!(xcom::EList<IR*, IR2Holder>::find(ir)), ("already in list"));
    IRListIter ct;
    for (List<IR*>::get_head(&ct);
         ct != List<IR*>::end(); ct = List<IR*>::get_next(ct)) {
        if (!ct->val()->is_phi()) {
            break;
        }
    }
    ASSERT0(m_bb);
    ir->setBB(m_bb);
    if (ct == nullptr) {
        //The only stmt of BB is phi or bb is empty.
        return EList<IR*, IR2Holder>::append_tail(ir);
    }
    return EList<IR*, IR2Holder>::insert_before(ir, ct);
}


//Insert ir prior to cond_br, uncond_br, call, return.
//Note the function will NOT set BB pointer of ir.
IRListIter BBIRList::append_tail_ex_without_set_bb(IR * ir)
{
    if (ir == nullptr) { return nullptr; }
    ASSERTN(!(xcom::EList<IR*, IR2Holder>::find(ir)), ("already in list"));
    IRListIter ct;
    List<IR*>::get_tail(&ct);
    if (ct != List<IR*>::end() && IRBB::isLowerBoundary(ct->val())) {
        //Skip over the last lower-bound IR.
        ct = List<IR*>::get_prev(ct);
    }
    if (ct == List<IR*>::end()) {
        //The only one stmt of BB is down boundary or bb is empty.
        return EList<IR*, IR2Holder>::append_head(ir);
    }
    return EList<IR*, IR2Holder>::insert_after(ir, ct);
}


//Insert ir prior to cond_br, uncond_br, call, return.
IRListIter BBIRList::append_tail_ex(IR * ir)
{
    if (ir == nullptr) { return nullptr; }
    IRListIter it = append_tail_ex_without_set_bb(ir);
    ASSERT0(m_bb);
    ir->setBB(m_bb);
    return it;
}


IR * BBIRList::extractRestIRIntoList(
    MOD BBIRListIter marker, bool include_marker)
{
    IR * restirs = nullptr;
    IR * last = nullptr;
    if (!include_marker) {
        this->get_next(&marker);
    }
    for (BBIRListIter next_ctir = marker;
         marker != nullptr; marker = next_ctir) {
        this->get_next(&next_ctir);

        //CASE:some IR in irlst may not have BB attribute,
        //e.g:Label.
        EList<IR*, IR2Holder>::remove(marker);
        xcom::add_next(&restirs, &last, marker->val());
    }
    return restirs;
}


void BBIRList::extractRestIRIntoList(
    MOD BBIRListIter marker, bool include_marker, OUT BBIRList & newlst)
{
    ASSERT0(marker);
    BBIRListIter curlstit;
    this->get_tail(&curlstit);
    ASSERT0(curlstit);
    for (BBIRListIter prevlstit = curlstit;
         curlstit != marker; curlstit = prevlstit) {
        this->get_prev(&prevlstit);
        //CASE:some IR in current IR list may not have BB attribute,
        //e.g:Label.
        EList<IR*, IR2Holder>::remove(curlstit);
        newlst.append_head(curlstit->val());
    }
    ASSERT0(curlstit == marker);
    if (include_marker) {
        EList<IR*, IR2Holder>::remove(curlstit);
        newlst.append_head(curlstit->val());
    }
}
//END BBIRList


//
//START BBList
//
IRBB * BBList::find(BBList const& lst, UINT id)
{
    BBListIter ct;
    for (IRBB * t = lst.get_head(&ct); t != nullptr; t = lst.get_next(&ct)) {
        if (t->id() == id) {
            return t;
        }
    }
    return nullptr;
}


//Return true if 'prev' is the previous BB of given BB in BBList.
//it: BBListIter of given BB.
bool BBList::isPrevBB(IRBB const* prev, IRBB const* next) const
{
    BBListIter nextit;
    xcom::List<IRBB*>::find(const_cast<IRBB*>(next), &nextit);
    ASSERT0(nextit);
    return isPrevBB(prev, nextit);
}


bool BBList::isPrevBB(IRBB const* prev, BBListIter nextit) const
{
    return prev == get_prev(&nextit);
}


void BBList::clone(BBList const& src, MOD IRBBMgr * bbmgr, Region const* rg)
{
    BBListIter srcit;
    for (IRBB const* srcbb = src.get_head(&srcit);
         srcbb != nullptr; srcbb = src.get_next(&srcit)) {
        IRBB * tgtbb = bbmgr->cloneBB(*srcbb, rg);
        append_tail(tgtbb);
    }
}


void BBList::copy(BBList const& src, Region const* rg)
{
    IRBBMgr * mgr = rg->getBBMgr();
    BBListIter srcit;
    for (IRBB const* srcbb = src.get_head(&srcit);
         srcbb != nullptr; srcbb = src.get_next(&srcit)) {
        IRBB * tgtbb = mgr->allocBB();
        tgtbb->copy(*srcbb, rg);
        append_tail(tgtbb);
    }
}
//END BBList


//
//START IRBB
//
size_t IRBB::count_mem() const
{
    size_t count = sizeof(IRBB);
    count += ir_list.count_mem();
    count += lab_list.count_mem();
    return count;
}


//Could ir be looked as a last stmt in basic block?
bool IRBB::isLowerBoundary(IR const* ir)
{
    ASSERTN(ir->isStmtInBB() || ir->is_lab(), ("illegal stmt in bb"));
    switch (ir->getCode()) {
    case IR_CALL:
    case IR_ICALL: //indirective call
        return ((CCall*)ir)->isMustBBbound();
    case IR_GOTO:
    case IR_IGOTO:
        return true;
    case IR_SWITCH:
        ASSERTN(SWITCH_body(ir) == nullptr,
                ("Peel switch-body to enable switch in bb-list construction"));
        return true;
    case IR_TRUEBR:
    case IR_FALSEBR:
        return true;
    case IR_RETURN:
    case IR_REGION:
        return true;
    default: break;
    }
    return false;
}


//Return true if BB has only fall through successor.
//Note the function does not take exception-edge into consider.
bool IRBB::is_only_fallthrough() const
{
    IR * last = const_cast<IRBB*>(this)->getLastIR();
    if (last == nullptr) { return true; }
    switch (last->getCode()) {
    case IR_CALL:
    case IR_ICALL:
        return true;
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SWITCH:
        return false;
    case IR_IGOTO:
    case IR_GOTO:
        //We have no knowledge about whether target BB of GOTO/IGOTO
        //will be followed subsequently on current BB.
        //Leave this problem to CFG builder, and the related
        //attribute should be set at that time.
        return false;
    case IR_RETURN:
        //Succeed stmt of 'ir' may be DEAD code
        //IR_BB_is_func_exit(cur_bb) = true;
        return true;
    case IR_REGION:
        return true;
    default: ASSERT0(!last->isBranch());
    }
    return true;
}


//Return true if BB has a fall through successor.
//Note conditional branch always has fallthrough successor.
bool IRBB::is_fallthrough() const
{
    IR * last = const_cast<IRBB*>(this)->getLastIR();
    if (last == nullptr) { return true; }
    switch (last->getCode()) {
    case IR_CALL:
    case IR_ICALL:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SWITCH:
        return true;
    case IR_IGOTO:
    case IR_GOTO:
        //We have no knowledge about whether target BB of GOTO/IGOTO
        //will be followed subsequently on current BB.
        //Leave this problem to CFG builder, and the related
        //attribute should be set at that time.
        return false;
    case IR_RETURN:
        //Succeed stmt of 'ir' may be DEAD code
        //IR_BB_is_func_exit(cur_bb) = true;
        return true;
    case IR_REGION:
        return true;
    default: ASSERT0(!last->isBranch());
    }
    return true;
}


void IRBB::dumpAttr(Region const* rg) const
{
    //Attributes
    note(rg, "\nATTR:");
    if (is_entry()) {
        prt(rg, "entry_bb ");
    }

    //if (BB_is_exit(this)) {
    //    prt(rg, "exit_bb ");
    //}

    if (is_fallthrough()) {
        prt(rg, "fall_through ");
    }

    if (is_target()) {
        prt(rg, "branch_target ");
    }
}


void IRBB::dumpIRList(Region const* rg, bool dump_inner_region,
                      MOD IRDumpCtx<> * ctx) const
{
    note(rg, "\nSTMT NUM:%d", getNumOfIR());
    rg->getLogMgr()->incIndent(3);
    IRBB * pthis = const_cast<IRBB*>(this);
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID|IR_DUMP_SRC_LINE|
        (dump_inner_region ? IR_DUMP_INNER_REGION : 0));
    for (IR * ir = BB_first_ir(pthis);
         ir != nullptr; ir = BB_irlist(pthis).get_next()) {
        ASSERT0(ir->is_single() || ir->is_undef());
        if (ctx != nullptr) {
            dumpIR(ir, rg, *ctx);
            continue;
        }
        dumpIR(ir, rg, nullptr, f);
    }
    rg->getLogMgr()->decIndent(3);
    note(rg, "\n");
}


void IRBB::dumpLabelList(Region const* rg) const
{
    IRBB * pthis = const_cast<IRBB*>(this);
    if (pthis->getLabelList().get_elem_count() > 0) {
        note(rg, "\nLABEL:");
        dumpBBLabel(pthis->getLabelList(), rg);
    }
}


void IRBB::dumpDigest(Region const* rg) const
{
    note(rg, "\n----- BB%u --- rpo:%d -----", id(),
         getVex() != nullptr ? rpo() : RPO_UNDEF);
    dumpLabelList(rg);
    dumpAttr(rg);
}


void IRBB::dump(Region const* rg, bool dump_inner_region,
                MOD IRDumpCtx<> * ctx) const
{
    if (!rg->isLogMgrInit()) { return; }
    dumpDigest(rg);
    dumpIRList(rg, dump_inner_region, ctx);
}


//Check that all basic blocks should only end with terminator IR.
bool IRBB::verify(Region const* rg) const
{
    UINT c = 0;
    IRListIter ct;
    BitSet irset;
    bool should_not_phi = false;
    BBIRList const& irlst = BB_irlist(this);
    for (IR * ir = irlst.get_head(&ct);
         ir != nullptr; ir = irlst.get_next(&ct)) {
        ASSERT0(ir->is_single());
        ASSERT0(ir->getBB() == this);
        ASSERT0(ir->getParent() == nullptr);
        ASSERT0(ir->isStmtInBB());
        ASSERTN(irlst.find(ir), ("element not in EList"));
        if (IRBB::isLowerBoundary(ir)) {
            ASSERTN(ir == const_cast<IRBB*>(this)->getLastIR(),
                    ("invalid BB down boundary."));
        }
        if (!ir->is_phi()) {
            should_not_phi = true;
        }
        if (should_not_phi) {
            ASSERT0(!ir->is_phi());
        }
        verifyIRList(ir, &irset, rg);
        c++;
    }
    ASSERT0(c == getNumOfIR());
    return true;
}


//Return true if one of bb's successor has a phi.
bool IRBB::hasPhiInSuccBB(CFG<IRBB, IR> const* cfg) const
{
    MDSSAMgr * mdssamgr = ((IRCFG*)cfg)->getRegion()->getMDSSAMgr();
    if (mdssamgr != nullptr && !mdssamgr->is_valid()) {
        mdssamgr = nullptr;
    }
    xcom::AdjVertexIter it;
    for (xcom::Vertex * succ_vex = Graph::get_first_out_vertex(getVex(), it);
         succ_vex != nullptr; succ_vex = Graph::get_next_out_vertex(it)) {
        IRBB * succ = cfg->getBB(succ_vex->id());
        ASSERT0(succ);
        if ((mdssamgr != nullptr && mdssamgr->hasPhi(succ)) ||
            PRSSAMgr::hasPhi(succ)) {
            return true;
        }
    }
    return false;
}


//Duplicate and add an operand that indicated by opnd_pos at phi stmt
//in one of bb's successors.
void IRBB::dupSuccessorPhiOpnd(CFG<IRBB, IR> * cfg, Region * rg, UINT opnd_pos)
{
    IRCFG * ircfg = (IRCFG*)cfg;
    xcom::AdjVertexIter it;
    for (xcom::Vertex * succ_vex = Graph::get_first_out_vertex(getVex(), it);
         succ_vex != nullptr; succ_vex = Graph::get_next_out_vertex(it)) {
        IRBB * succ = ircfg->getBB(succ_vex->id());
        ASSERT0(succ);
        BBIRListIter it;
        for (IR * ir = succ->getIRList().get_head(&it);
             ir != nullptr; ir = succ->getIRList().get_next(&it)) {
            if (!ir->is_phi()) { break; }
            ASSERT0(xcom::cnt_list(PHI_opnd_list(ir)) >= opnd_pos);
            IR * opnd;
            UINT lpos = opnd_pos;
            for (opnd = PHI_opnd_list(ir);
                 lpos != 0; opnd = opnd->get_next()) {
                ASSERT0(opnd);
                lpos--;
            }

            IR * newopnd = rg->dupIRTree(opnd);
            if (opnd->isReadPR()) {
                //TO BE REMOVED, should have been copied in dupIR
                //newopnd->copyRef(opnd, rg);
                ASSERT0(PR_ssainfo(opnd));
                PR_ssainfo(newopnd) = PR_ssainfo(opnd);
                SSA_uses(PR_ssainfo(newopnd)).append(newopnd);
            }

            ((CPhi*)ir)->addOpnd(newopnd);
        }
    }
}


bool IRBB::verifyBranchLabel(Lab2BB const& lab2bb) const
{
    //Verify branch target label.
    IR * last = const_cast<IRBB*>(this)->getLastIR();
    if (last == nullptr) { return true; }

    if (last->isConditionalBr()) {
        ASSERTN(lab2bb.get(BR_lab(last)),
                ("branch target cannot be nullptr"));
        return true;
    }

    if (last->isMultiConditionalBr()) {
        ASSERT0(last->is_switch());
        for (IR * c = SWITCH_case_list(last);
             c != nullptr; c = c->get_next()) {
            ASSERTN(lab2bb.get(CASE_lab(last)),
                    ("case branch target cannot be nullptr"));
        }
        if (SWITCH_deflab(last) != nullptr) {
            ASSERTN(lab2bb.get(SWITCH_deflab(last)),
                    ("default target cannot be nullptr"));
        }
        return true;
    }

    if (last->isUnconditionalBr()) {
        if (last->is_goto()) {
            ASSERTN(lab2bb.get(GOTO_lab(last)), ("use undefined label"));
        } else {
            ASSERT0(last->is_igoto());
            for (IR * caseexp = last->getCaseList(); caseexp != nullptr;
                caseexp = caseexp->get_next()) {
                ASSERTN(lab2bb.get(CASE_lab(caseexp)),
                        ("use undefined label"));
            }
        }
    }
    return true;
}


bool IRBB::hasPRPhi() const
{
    return PRSSAMgr::hasPhi(this);
}


bool IRBB::hasPhiWithAllSameOperand(CFG<IRBB, IR> const* cfg) const
{
    PRSSAMgr * prssamgr = ((IRCFG*)cfg)->getRegion()->getPRSSAMgr();
    if (prssamgr == nullptr || !prssamgr->is_valid()) {
        //The region is not in SSA mode.
        return false;
    }
    if (!prssamgr->hasPhiWithAllSameOperand(this)) { return false; }
    MDSSAMgr * mdssamgr = ((IRCFG*)cfg)->getRegion()->getMDSSAMgr();
    if (mdssamgr == nullptr || !mdssamgr->is_valid()) {
        //The region is not in MDSSA mode.
        return false;
    }
    return mdssamgr->hasPhiWithAllSameOperand(this);
}


bool IRBB::hasMDPhi(CFG<IRBB, IR> const* cfg) const
{
    MDSSAMgr * mgr = ((IRCFG*)cfg)->getRegion()->getMDSSAMgr();
    return mgr != nullptr && mgr->hasPhi(this);
}


bool IRBB::verifyTerminate() const
{
    bool find = false;
    IRBB * pthis = const_cast<IRBB*>(this);
    for (LabelInfo const* li = pthis->getLabelList().get_head();
         li != nullptr; li = pthis->getLabelList().get_next()) {
        if (LABELINFO_is_terminate(li)) {
            find = true;
            break;
        }
    }
    ASSERT0_DUMMYUSE(BB_is_terminate(this) == find);
    return true;
}


bool IRBB::verifyExpHandler() const
{
    bool find = false;
    IRBB * pthis = const_cast<IRBB*>(this);
    for (LabelInfo const* li = pthis->getLabelList().get_head();
         li != nullptr; li = pthis->getLabelList().get_next()) {
        if (LABELINFO_is_catch_start(li)) {
            find = true;
            break;
        }
    }
    ASSERT0_DUMMYUSE(BB_is_catch_start(this) == find);
    return true;
}


bool IRBB::verifyTryEnd() const
{
    bool find = false;
    IRBB * pthis = const_cast<IRBB*>(this);
    for (LabelInfo const* li = pthis->getLabelList().get_head();
         li != nullptr; li = pthis->getLabelList().get_next()) {
        if (LABELINFO_is_try_end(li)) {
            find = true;
            break;
        }
    }
    ASSERT0_DUMMYUSE(BB_is_try_end(this) == find);
    return true;
}


bool IRBB::verifyTryStart() const
{
    bool find = false;
    IRBB * pthis = const_cast<IRBB*>(this);
    for (LabelInfo const* li = pthis->getLabelList().get_head();
         li != nullptr; li = pthis->getLabelList().get_next()) {
        if (LABELINFO_is_try_start(li)) {
            find = true;
            break;
        }
    }
    ASSERT0_DUMMYUSE(BB_is_try_start(this) == find);
    return true;
}


void IRBB::copyIRList(IRBB const& src, Region const* rg)
{
    BBIRList & tgtirlst = getIRList();
    ASSERT0(tgtirlst.get_elem_count() == 0);
    BBIRList const& srcirlst = const_cast<IRBB&>(src).getIRList();
    BBIRListIter irit;
    for (IR const* srcir = srcirlst.get_head(&irit);
         srcir != nullptr; srcir = srcirlst.get_next(&irit)) {
        IR * tgtir = rg->dupIRTree(srcir);
        tgtir->setBB(this);
        tgtirlst.append_tail(tgtir);
    }
}


void IRBB::copyLabelInfoList(IRBB const& src, SMemPool * pool)
{
    getLabelList().copy(const_cast<IRBB&>(src).getLabelList());
}


//The function will copy all contents of 'src', include BBID and Vertex info.
//This is the difference that compare to IRBB::copy().
void IRBB::clone(IRBB const& src, Region const* rg)
{
    m_id = src.m_id;
    copy(src, rg);
}


void IRBB::copy(IRBB const& src, Region const* rg)
{
    ////////////////////////////////////////////////////////////////////////////
    //DO NOT COPY BB'S ID, BB ID IS UNIQUE IN CURRENT REGION.                 //
    ////////////////////////////////////////////////////////////////////////////
    u1 = src.u1;
    m_vertex = nullptr;
    copyLabelInfoList(src, rg->getCommPool());
    copyIRList(src, rg);
}


void IRBB::freeIRList(Region const* rg)
{
    BBIRListIter irit;
    for (IR * ir = BB_irlist(this).get_head(&irit);
         ir != nullptr; ir = BB_irlist(this).get_next(&irit)) {
        rg->freeIRTreeList(ir);
    }
    BB_irlist(this).clean();
}
//END IRBB


//
//START IRBBMgr
//
IRBBMgr::~IRBBMgr()
{
    //BBs which are in free list are also recorded in m_bb_vec,
    //thus no need to delete BB in m_free_list.
    for (VecIdx i = 0; i < (VecIdx)m_bb_vec.get_elem_count(); i++) {
        IRBB * bb = m_bb_vec.get(i);
        if (bb == nullptr) { continue; }
        bb->freeIRList(m_rg);
        delete bb;
    }
}


IRBB * IRBBMgr::cloneBB(IRBB const& src, Region const* rg)
{
    ASSERTN(m_bb_vec.get(src.id()) == nullptr,
            ("BB%d has been allocated", src.id()));
    IRBB * tgtbb = new IRBB();
    tgtbb->clone(src, rg);
    m_bb_vec.set(tgtbb->id(), tgtbb);
    m_bb_count = MAX(m_bb_count, (tgtbb->id() + 1));
    return tgtbb;
}


IRBB * IRBBMgr::allocBB()
{
    IRBB * bb = m_free_list.remove_head();
    if (bb == nullptr) {
        bb = new IRBB();
        BB_id(bb) = m_bb_count++;
        ASSERT0(m_bb_vec.get(bb->id()) == nullptr);
        m_bb_vec.set(bb->id(), bb);
    }
    return bb;
}


void IRBBMgr::destroyBB(IRBB * bb)
{
    ASSERT0(bb && bb->id() != BBID_UNDEF);
    m_bb_vec.set(bb->id(), nullptr);
    ASSERTN(!((xcom::List<IRBB*>&)m_free_list).find(bb),
            ("BB has been put in freelist"));
    bb->freeIRList(m_rg);
    bb->clean();
    delete bb;
}


void IRBBMgr::freeBB(IRBB * bb)
{
    ASSERT0(bb);
    ASSERTN(!((xcom::List<IRBB*>&)m_free_list).find(bb), ("double free"));
    bb->freeIRList(m_rg);
    bb->clean();
    m_free_list.append_head(bb);
}


//Count memory usage for current object.
size_t IRBBMgr::count_mem() const
{
    size_t count = 0;
    for (VecIdx i = 0; i < (VecIdx)m_bb_vec.get_elem_count(); i++) {
        IRBB const* bb = m_bb_vec.get(i);
        if (bb == nullptr) { continue; }
        count += bb->count_mem();
    }
    return count;
}


bool IRBBMgr::verify() const
{
    //Guarantee the BB id is unique.
    xcom::TTab<UINT> idtab;
    for (VecIdx i = 0; i < (VecIdx)m_bb_vec.get_elem_count(); i++) {
        IRBB const* bb = m_bb_vec.get(i);
        if (bb == nullptr) { continue; }
        ASSERT0(!idtab.find(bb->id()));
        idtab.append(bb->id());
    }
    return true;
}
//END IRBBMgr


void dumpBBLabel(LabelInfoList & lablist, Region const* rg)
{
    xcom::C<LabelInfo const*> * ct;
    for (lablist.get_head(&ct); ct != lablist.end();
         ct = lablist.get_next(ct)) {
        LabelInfo const* li = ct->val();
        switch (LABELINFO_type(li)) {
        case L_CLABEL:
            note(rg, CLABEL_STR_FORMAT, CLABEL_CONT(li));
            break;
        case L_ILABEL:
            note(rg, ILABEL_STR_FORMAT, ILABEL_CONT(li));
            break;
        case L_PRAGMA:
            ASSERT0(LABELINFO_pragma(li));
            note(rg, "%s", SYM_name(LABELINFO_pragma(li)));
            break;
        default: UNREACHABLE();
        }

        if (LABELINFO_is_try_start(li) ||
            LABELINFO_is_try_end(li) ||
            LABELINFO_is_catch_start(li) ||
            LABELINFO_is_terminate(li)) {
            prt(rg, "(");
            if (LABELINFO_is_try_start(li)) {
                prt(rg, "try_start,");
            }
            if (LABELINFO_is_try_end(li)) {
                prt(rg, "try_end,");
            }
            if (LABELINFO_is_catch_start(li)) {
                prt(rg, "catch_start,");
            }
            if (LABELINFO_is_terminate(li)) {
                prt(rg, "terminate");
            }
            prt(rg, ")");
        }
        prt(rg, " ");
    }
}


//filename: dump BB list into given filename.
void dumpBBList(CHAR const* filename, BBList const* bbl, Region const* rg,
                bool dump_inner_region, IRDumpCtx<> * ctx)
{
    ASSERT0(filename);
    FileObj fo(filename, true, false);
    FILE * h = fo.getFileHandler();
    rg->getLogMgr()->push(h, filename);
    dumpBBList(bbl, rg, dump_inner_region);
    rg->getLogMgr()->pop();
}


void dumpBBList(BBList const* bbl, Region const* rg, bool dump_inner_region,
                IRDumpCtx<> * ctx)
{
    ASSERT0(rg && bbl);
    if (!rg->isLogMgrInit() || bbl->get_elem_count() == 0) { return; }
    note(rg, "\n==---- DUMP IRBBList '%s' ----==", rg->getRegionName());
    xcom::C<IRBB*> * ct = nullptr;
    for (IRBB * bb = bbl->get_head(&ct);
         bb != nullptr; bb = bbl->get_next(&ct)) {
        bb->dump(rg, dump_inner_region, ctx);
    }
}


void dumpBBSet(BBSet const& bbs, Region const* rg, bool dump_inner_region,
               IRDumpCtx<> * ctx)
{
    ASSERT0(rg);
    if (!rg->isLogMgrInit() || bbs.get_elem_count() == 0) { return; }
    note(rg, "\n==---- DUMP IRBBSet '%s' ----==", rg->getRegionName());
    BBSetIter it;
    for (BSIdx i = bbs.get_first(&it);
         i != BS_UNDEF; i = bbs.get_next(i, &it)) {
        IRBB const* bb = rg->getBB(i);
        ASSERT0(bb);
        bb->dump(rg, dump_inner_region, ctx);
    }
}


//Check for IR and IRBB sanity and uniqueness.
//Ensure that all IRs must be embedded into a basic block.
//Ensure that PHI must be the first stmt in basic block.
bool verifyIRandBB(BBList * bbl, Region const* rg)
{
    Lab2BB lab2bb;
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        for (LabelInfo const* li = bb->getLabelList().get_head();
             li != nullptr; li = bb->getLabelList().get_next()) {
            lab2bb.set(li, bb);
        }
    }
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        bb->verify(rg);
        bb->verifyBranchLabel(lab2bb);
    }
    return true;
}

} //namespace xoc
