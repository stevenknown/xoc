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

static LoopDepInfoDesc g_loopdepinfo_desc [] = {
    { LOOP_DEP_UNDEF, "", },
    { LOOP_DEP_CARRIED, "loop-carried", },
    { LOOP_DEP_INDEPENDENT, "loop-independent", },
    { LOOP_DEP_REDUCE, "loop-reduce", },
};


CHAR const* LoopDepInfoDesc::getDepName(LOOP_DEP_KIND k)
{
    ASSERT0(k < LOOP_DEP_NUM);
    return g_loopdepinfo_desc[k].name;
}


//
//START LoopDepInfo
//
CHAR const* LoopDepInfo::getDepName() const
{
    return LoopDepInfoDesc::getDepName(m_kind);
}


CHAR const* LoopDepInfo::dumpTgt(OUT xcom::StrBuf & buf) const
{
    if (isTgtIR()) {
        ASSERT0(getTgtIR());
        return xoc::dumpIRName(getTgtIR(), buf);
    }
    ASSERT0(isTgtMDDef());
    ASSERT0(getTgtMDDef() && getTgtMDDef()->getResult());
    buf.strcat("mddef%u,", getTgtMDDef()->id());
    getTgtMDDef()->getResult()->dump(buf);
    return buf.buf;
}
//END LoopDepInfo


//
//START LDAInfoSet
//
void LDAInfoSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    LDAInfoSetIter lit;
    xcom::StrBuf tmp(16);
    for (LoopDepInfo * info = get_head(&lit);
         info != nullptr; info = get_next(&lit)) {
        ASSERT0(info->verify());
        note(rg, "\nLOOP_DEP:%s:%s<->",
             LoopDepInfoDesc::getDepName(LDI_kind(info)),
             DumpIRName().dump(LDI_src(info)));
        if (info->isTgtIR()) {
            prt(rg, "%s", DumpIRName().dump(LDI_tgt_ir(info)));
        } else {
            tmp.clean();
            ASSERT0(info->isTgtMDDef());
            prt(rg, "%s", info->dumpTgt(tmp));
        }
    }
}
//END LDAInfoSet


//
//START LDAActMgr
//
void LDAActMgr::dumpAct(CHAR const* format, ...) const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    LDAActMgr * pthis = const_cast<LDAActMgr*>(this);
    ASSERT0(format);
    va_list args;
    va_start(args, format);
    pthis->dump_args(format, args);
    va_end(args);
}


void LDAActMgr::dumpAct(IR const* ir, CHAR const* format, ...) const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    ASSERT0(ir);
    xcom::StrBuf tmpbuf(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        tmpbuf.vstrcat(format, args);
        va_end(args);
    }
    LDAActMgr * pthis = const_cast<LDAActMgr*>(this);
    ActHandler acth = pthis->dump("Found Loop Dependence:");
    if (format != nullptr) {
        acth.info->strcat("reason:%s", tmpbuf.getBuf());
    }
    getRegion()->getLogMgr()->incIndent(4);
    xcom::StrBuf irbuf(64);
    dumpIRToBuf(ir, getRegion(), irbuf);
    getRegion()->getLogMgr()->decIndent(4);
    acth.info->strcat(irbuf);
}


void LDAActMgr::dumpLinRepAct(IVLinearRep const& linrep,
                               CHAR const* format, ...) const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    ASSERTN(format, ("no action info"));
    LDAActMgr * pthis = const_cast<LDAActMgr*>(this);
    xcom::StrBuf tmpbuf(64);
    va_list args;
    va_start(args, format);
    tmpbuf.vstrcat(format, args);
    va_end(args);
    pthis->dump("LinearRepAct:%s", tmpbuf.buf);
}
//END LDAActMgr


//
//START LoopDepAna
//
//Return true if given two IRs in 'info' are indicates same memory location.
//The funtion uses Equal VN to determine whether these two IRs reference same
//memory base address.
//e.g: ist x VS ild y, return true if x's EVN is equal to y's EVN.
//Return true if these two IRs are reference identical memory location,
//otherwise tell caller 'I KNOW NOTHING ABOUT THAT' by returning false.
bool LoopDepAna::isSameMemLocViaEVN(LoopDepInfo const& info)
{
    ASSERT0(info.verify());
    if (info.getSrc()->isIndirectMemOp() &&
        info.isTgtIR() &&
        info.getTgtIR()->isIndirectMemOp() &&
        info.getSrc()->getOffset() == info.getTgtIR()->getOffset()) {
        return isSameMemLocIndirectOp(info);
    }
    if (info.getSrc()->isArrayOp() &&
        info.isTgtIR() &&
        info.getTgtIR()->isArrayOp() &&
        info.getSrc()->getOffset() == info.getTgtIR()->getOffset()) {
        return isSameMemLocArrayOp(info);
    }
    return false;
}


bool LoopDepAna::isSameMemLocArrayOp(LoopDepInfo const& info)
{
    if (info.isTgtMDDef()) { return false; }
    ASSERT0(info.isTgtIR());
    IR const* src = info.getSrc();
    IR const* tgt = info.getTgtIR();
    ASSERT0(src->getOffset() == tgt->getOffset());
    ASSERT0(src->isArrayOp() && tgt->isArrayOp());
    if (!((CArray*)src)->isIsomoArrayStructTo(tgt)) {
        return false; //We have no knowledge about the array.
    }
    IR const* srcbase = src->getBase();
    IR const* tgtbase = tgt->getBase();
    InferCtx ictx;
    VN const* srcvn = getInferEVN().inferExp(srcbase, ictx);
    VN const* tgtvn = getInferEVN().inferExp(tgtbase, ictx);
    if (srcvn == nullptr || srcvn != tgtvn) { return false; }

    //Check array subscript expression.
    IR const* srcsubexp = ARR_sub_list(src);
    IR const* tgtsubexp = ARR_sub_list(tgt);
    for (; srcsubexp != nullptr;
         srcsubexp = srcsubexp->get_next(),
         tgtsubexp = tgtsubexp->get_next()) {
        ASSERT0(tgtsubexp);
        ictx.clean();
        VN const* srcsubvn = getInferEVN().inferExp(srcsubexp, ictx);
        VN const* tgtsubvn = getInferEVN().inferExp(tgtsubexp, ictx);
        if (srcsubvn == nullptr || srcsubvn != tgtsubvn) { return false; }
    }
    return true;
}


bool LoopDepAna::isSameMemLocIndirectOp(LoopDepInfo const& info)
{
    if (info.isTgtMDDef()) { return false; }
    ASSERT0(info.getSrc()->isIndirectMemOp() &&
            info.isTgtIR() && info.getTgtIR());
    IR const* src = info.getSrc();
    IR const* tgt = info.getTgtIR();
    ASSERT0(src->getOffset() == tgt->getOffset());
    IR const* srcbase = src->getBase();
    IR const* tgtbase = tgt->getBase();
    InferCtx ictx;
    VN const* srcvn = getInferEVN().inferExp(srcbase, ictx);
    VN const* tgtvn = getInferEVN().inferExp(tgtbase, ictx);
    return srcvn == nullptr || srcvn == tgtvn;
}


void LoopDepAna::destroy()
{
    if (m_pool == nullptr) { return; }
    ASSERT0(m_infer_evn);
    delete m_infer_evn;
    m_infer_evn = nullptr;
    smpoolDelete(m_pool);
    m_pool = nullptr;
    m_act_mgr.clean();
}


void LoopDepAna::init(GVN * gvn)
{
    if (m_pool != nullptr) { return; }
    ASSERT0(getRegion());
    ASSERTN(gvn, ("LoopDepAna need GVN"));
    m_pool = smpoolCreate(sizeof(LFRInfo) * 4, MEM_COMM);
    m_cfg = getRegion()->getCFG();
    m_oc = nullptr;
    m_dumgr = nullptr;
    m_licm = nullptr;
    m_ivr = nullptr;
    m_gvn = gvn;
    m_prssamgr = nullptr;
    m_mdssamgr = nullptr;
    m_infer_evn = new InferEVN(gvn);
}


LoopDepInfo * LoopDepAna::allocLoopDepInfo()
{
    LoopDepInfo * p = (LoopDepInfo*)smpoolMalloc(sizeof(LoopDepInfo), m_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(LoopDepInfo));
    return p;
}


bool LoopDepAna::containLoopRedDep(LDAInfoSet const& set)
{
    LDAInfoSetIter it;
    for (LoopDepInfo const* info = set.get_head(&it);
         info != nullptr; info = set.get_next(&it)) {
        if (info->isLoopRed()) { return true; }
    }
    return false;
}


bool LoopDepAna::containLoopCarrDep(LDAInfoSet const& set)
{
    LDAInfoSetIter it;
    for (LoopDepInfo const* info = set.get_head(&it);
         info != nullptr; info = set.get_next(&it)) {
        if (info->isLoopCarr()) { return true; }
    }
    return false;
}


void LoopDepAna::analyzeLinearDep(
    IR const* ir, xcom::List<IR*> const& lst, OUT LDAInfoSet & set,
    MOD LDACtx & ctx)
{
    bool is_aggressive = true;
    ASSERT0(ir && ctx.getLI());
    xcom::List<IR*>::Iter it;
    for (IR * tgt = lst.get_head(&it);
         tgt != nullptr; tgt = lst.get_next(&it)) {
        ASSERT0(tgt->is_exp() || tgt->is_stmt());
        if (tgt == ir) { continue; }
        if (!xoc::isDependent(ir, tgt, is_aggressive, m_rg)) {
            continue;
        }
        if (xoc::isLoopIndependent(ir, tgt, is_aggressive, ctx.getLI(),
                                   m_rg, m_gvn)) {
            LoopDepInfo * ldainfo = allocLoopDepInfo();
            LDI_kind(ldainfo) = LOOP_DEP_INDEPENDENT;
            LDI_src(ldainfo) = ir;
            ldainfo->setTgtIR(tgt);
            continue;
        }
        LoopDepInfo * ldainfo = allocLoopDepInfo();
        LDI_kind(ldainfo) = LOOP_DEP_CARRIED;
        LDI_src(ldainfo) = ir;
        ldainfo->setTgtIR(tgt);
        set.append_tail(ldainfo);
    }
}


void LoopDepAna::analyzeRedDep(
    IR const* ir, xcom::List<IR*> const& lst, OUT LDAInfoSet & set,
    MOD LDACtx & ctx)
{
    if (!ir->is_exp()) { return; }
    if (!ir->isDirectMemOp() && !ir->isReadPR()) { return; }
    LoopDepInfo tmp;
    if (!xoc::hasLoopReduceDep(ir, m_rg, ctx.getLI(), tmp)) { return; }
    LoopDepInfo * ldainfo = allocLoopDepInfo();
    ldainfo->copy(tmp);
    set.append_tail(ldainfo);
}


void LoopDepAna::analyzeDep(
    IR const* ir, xcom::List<IR*> const& lst, OUT LDAInfoSet & set,
    MOD LDACtx & ctx)
{
    ctx.add(ir); //set ir that has analyzed.
    analyzeLinearDep(ir, lst, set, ctx);
    analyzeRedDep(ir, lst, set, ctx);
}


bool LoopDepAna::transLoopCarrToLoopIndep(
    IR const* ir, LDAInfoSet const& set)
{
    bool changed = false;
    LDAInfoSetIter lit;
    for (LoopDepInfo * info = set.get_head(&lit);
         info != nullptr; info = set.get_next(&lit)) {
        if (!info->isLoopCarr()) { continue; }
        if (!isSameMemLocViaEVN(*info)) { continue; }

        //Revise loop-carried to loop-independent to make loop dependence
        //more precise.
        ASSERT0(info->isTgtIR());
        getActMgr().dumpAct(ir,
            "%s and %s access same memory location, thus they have "
            "loop-independent dependence",
            DumpIRName().dump(info->getSrc()),
            DumpIRName().dump(info->getTgtIR()));
        LDI_kind(info) = LOOP_DEP_INDEPENDENT;
        changed = true;
    }
    return changed;
}


void LoopDepAna::analyzeDepForIRTree(
    IR const* ir, xcom::List<IR*> const& lst, OUT LDAInfoSet & set,
    LDACtx & ctx)
{
    ASSERT0(ir->is_exp() || ir->is_stmt());
    ConstIRIter it;
    for (IR const* x = xoc::iterInitC(ir, it, false);
         x != nullptr; x = xoc::iterNextC(it, true)) {
        if (!x->isMemRefNonPR()) { continue; }
        analyzeDep(x, lst, set, ctx);
    }
    transLoopCarrToLoopIndep(ir, set);
}


bool LoopDepAna::useLICM() const
{
    return m_licm != nullptr && m_licm->is_valid();
}


void LoopDepAna::dumpInferEVN() const
{
    if (m_infer_evn != nullptr) {
        m_infer_evn->dump();
    }
}


bool LoopDepAna::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    m_act_mgr.dump();
    dumpInferEVN();
    bool res = Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    return res;
}


void LoopDepAna::reset()
{
    GVN * gvn = m_gvn;
    destroy();
    init(gvn);
}


bool LoopDepAna::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    reset();
    //Initialize pass object since they might be destructed at any moment.
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    m_dumgr = m_rg->getDUMgr();
    m_oc = &oc;
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //Analysis uses either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //Analysis uses either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_LOOP_INFO,
                                               PASS_IVR, PASS_UNDEF);
    m_ivr = (IVR*)m_rg->getPassMgr()->queryPass(PASS_IVR);
    ASSERT0(m_ivr && m_ivr->is_valid());
    m_licm = (LICM*)m_rg->getPassMgr()->registerPass(PASS_LICM);
    if (!useLICM()) {
        //Note the pass still not force LICM to work yet.
        //The pass just utilize the analysis ability of LICM.
        return false;
    }
    m_gvn = (GVN*)m_rg->getPassMgr()->registerPass(PASS_GVN);
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_GVN, PASS_UNDEF);
    if (!useGVN()) {
        //Note the pass still not force GVN to work yet.
        //The pass just utilize the analysis ability of GVN.
        return false;
    }
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLoopDepAna()) {
        dump();
    }
    m_rg->getLogMgr()->cleanBuffer();
    END_TIMER(t, getPassName());
    return false;
}
//END LoopDepAna

} //namespace xoc
