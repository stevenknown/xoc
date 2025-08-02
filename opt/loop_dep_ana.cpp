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
    VMDFixedStrBuf fbuf;
    fbuf.bind(&buf);
    getTgtMDDef()->getResult()->dump(fbuf);
    fbuf.unbind();
    return buf.getBuf();
}
//END LoopDepInfo


//
//START LoopDepInfoSet
//
void LoopDepInfoSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    LoopDepInfoSetIter lit;
    xcom::StrBuf tmp(16);
    for (LoopDepInfo const* ldi = get_first(lit);
        !lit.end(); ldi = get_next(lit)) {
        ASSERT0(ldi);
        ASSERT0(ldi->verify());
        note(rg, "\nLOOP_DEP:%s:%s<->",
             LoopDepInfoDesc::getDepName(LDI_kind(ldi)),
             DumpIRName().dump(LDI_src(ldi)));
        if (ldi->isTgtIR()) {
            prt(rg, "%s", DumpIRName().dump(LDI_tgt_ir(ldi)));
        } else {
            tmp.clean();
            ASSERT0(ldi->isTgtMDDef());
            prt(rg, "%s", ldi->dumpTgt(tmp));
        }
    }
}


bool LoopDepInfoSet::isOnlyContainLoopIndep(IR const* src, IR const* tgt) const
{
    ASSERT0(src && tgt);
    if (get_elem_count() != 1) { return false; }
    LoopDepInfo const* ldi = get_first();
    return ldi->getSrc() == src && ldi->getTgtIR() == tgt &&
           ldi->isLoopIndep();
}


bool LoopDepInfoSet::isAtMostContainLoopIndep(
    IR const* src, IR const* tgt) const
{
    ASSERT0(src && tgt);
    if (get_elem_count() == 0) { return true; }
    LoopDepInfoSetIter it;
    for (LoopDepInfo const* ldi = get_first(it);
         !it.end(); ldi = get_next(it)) {
        if (ldi->getSrc() == src && ldi->getTgtIR() == tgt) {
            return ldi->isLoopIndep();
        }
    }
    return true; //No dependence between src and tgt.
}
//END LoopDepInfoSet


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
    xoc::dumpIRToBuf(ir, getRegion(), irbuf);
    getRegion()->getLogMgr()->decIndent(4);
    acth.info->strcat(irbuf);
}


void LDAActMgr::dumpLinRepAct(
    IVLinearRep const& linrep, CHAR const* format, ...) const
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
//START LoopDepCtx
//
LoopDepCtx::LoopDepCtx(Region const* rg, LI<IRBB> const* li) : m_am(rg)
{
    ASSERT0(li);
    m_li = li;
    m_pool = smpoolCreate(sizeof(LoopDepInfo) * 4, MEM_COMM);
    m_firtab_pool = smpoolCreate(
        FirstTab::getTNodeSize() * 2, MEM_CONST_SIZE);
    m_ir2ldi_pool = smpoolCreate(
        IR2LDITab::getTNodeSize() * 2, MEM_CONST_SIZE);
    m_mddef2ldi_pool = smpoolCreate(
        MDDef2LDITab::getTNodeSize() * 2, MEM_CONST_SIZE);
}


LoopDepCtx::~LoopDepCtx()
{
    smpoolDelete(m_pool);
    smpoolDelete(m_firtab_pool);
    smpoolDelete(m_ir2ldi_pool);
    smpoolDelete(m_mddef2ldi_pool);
}


LoopDepInfo * LoopDepCtx::allocLoopDepInfo()
{
    LoopDepInfo * p = (LoopDepInfo*)smpoolMalloc(sizeof(LoopDepInfo), m_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(LoopDepInfo));
    return p;
}


LoopDepCtx::FirstTab * LoopDepCtx::allocFirstTab()
{
    FirstTab * p = (FirstTab*)smpoolMalloc(sizeof(FirstTab), m_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(FirstTab));
    p->init(m_firtab_pool);
    return p;
}


LoopDepCtx::SecondTab * LoopDepCtx::allocSecondTab()
{
    SecondTab * p = (SecondTab*)smpoolMalloc(sizeof(SecondTab), m_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(SecondTab));
    return p;
}


LoopDepCtx::IR2LDITab * LoopDepCtx::allocIR2LDI()
{
    IR2LDITab * p = (IR2LDITab*)smpoolMalloc(sizeof(IR2LDITab), m_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(IR2LDITab));
    p->init(m_ir2ldi_pool);
    return p;
}


LoopDepCtx::MDDef2LDITab * LoopDepCtx::allocMDDef2LDI()
{
    MDDef2LDITab * p = (MDDef2LDITab*)smpoolMalloc(
        sizeof(MDDef2LDITab), m_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(MDDef2LDITab));
    p->init(m_mddef2ldi_pool);
    return p;
}


LoopDepInfo const* LoopDepCtx::appendLoopDepInfo(LoopDepInfo const& ldi)
{
    IR const* src = ldi.getSrc();
    ASSERT0(src);
    FirstTab * ft = m_ir2firsttab.get(src);
    if (ft == nullptr) {
        ft = allocFirstTab();
        m_ir2firsttab.set(src, ft);
    }
    ASSERT0(ldi.getKind() != LOOP_DEP_UNDEF);
    SecondTab * st = ft->get(ldi.getKind());
    if (st == nullptr) {
        st = allocSecondTab();
        ft->set(ldi.getKind(), st);
    }
    if (ldi.isTgtIR()) {
        if (st->ir2ldi == nullptr) {
            st->ir2ldi = allocIR2LDI();
        }
        IR const* tgtir = ldi.getTgtIR();
        ASSERT0(tgtir && !tgtir->is_undef());
        LoopDepInfo const* hashed = st->ir2ldi->get(tgtir);
        if (hashed == nullptr) {
            LoopDepInfo * t = allocLoopDepInfo();
            t->copy(ldi);
            st->ir2ldi->set(tgtir, t);
            hashed = t;
        }
        return hashed;
    }
    if (st->mddef2ldi == nullptr) {
        st->mddef2ldi = allocMDDef2LDI();
    }
    ASSERT0(ldi.isTgtMDDef());
    ASSERT0(ldi.getTgtMDDef());
    MDDef const* mddef = ldi.getTgtMDDef();
    ASSERT0(mddef && mddef->is_valid());
    LoopDepInfo const* hashed = st->mddef2ldi->get(mddef);
    if (hashed == nullptr) {
        LoopDepInfo * t = allocLoopDepInfo();
        t->copy(ldi);
        st->mddef2ldi->set(mddef, t);
        hashed = t;
    }
    return hashed;
}
//END LoopDepCtx


//
//START LoopDepAna
//
LoopDepAna::LoopDepAna(Region * rg, GVN * gvn) : Pass(rg)
{
    m_pool = nullptr;
    m_is_aggressive = true;
    init(gvn);
}


bool LoopDepAna::isSameMemLocViaEVN(
    LoopDepInfo const& info, OUT LoopDepCtx * ctx)
{
    ASSERT0(info.verify());
    if (info.getSrc()->isIndirectMemOp() &&
        info.isTgtIR() &&
        info.getTgtIR()->isIndirectMemOp() &&
        info.getSrc()->getOffset() == info.getTgtIR()->getOffset()) {
        return isSameMemLocIndirectOp(info, ctx);
    }
    if (info.getSrc()->isArrayOp() &&
        info.isTgtIR() &&
        info.getTgtIR()->isArrayOp() &&
        info.getSrc()->getOffset() == info.getTgtIR()->getOffset()) {
        return isSameMemLocArrayOp(info, ctx);
    }
    return false;
}


bool LoopDepAna::isSameMemLocArrayOp(
    LoopDepInfo const& info, OUT LoopDepCtx * ctx)
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


bool LoopDepAna::isSameMemLocIndirectOp(
    LoopDepInfo const& info, OUT LoopDepCtx * ctx)
{
    if (info.isTgtMDDef()) { return false; }
    ASSERT0(info.getSrc()->isIndirectMemOp() &&
            info.isTgtIR() && info.getTgtIR());
    IR const* src = info.getSrc();
    IR const* tgt = info.getTgtIR();
    ASSERT0(src->getOffset() == tgt->getOffset());
    IR const* srcbase = src->getBase();
    IR const* tgtbase = tgt->getBase();
    InferCtx ictx(ctx == nullptr ? nullptr : &ctx->getActMgr());
    VN const* srcvn = getInferEVN().inferExp(srcbase, ictx);
    VN const* tgtvn = getInferEVN().inferExp(tgtbase, ictx);
    return srcvn == nullptr || srcvn == tgtvn;
}


void LoopDepAna::destroy()
{
    if (m_pool == nullptr) { return; }
    m_infer_evn = nullptr;
    smpoolDelete(m_pool);
    m_pool = nullptr;
}


void LoopDepAna::init(GVN * gvn)
{
    if (m_pool != nullptr) { return; }
    ASSERT0(gvn);
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
    ASSERT0(gvn);
    m_infer_evn = gvn->getAndGenInferEVN();
    m_infer_evn->clean();
}


bool LoopDepAna::containLoopRedDep(LoopDepInfoSet const& set)
{
    LoopDepInfoSetIter it;
    for (LoopDepInfo const* ldi = set.get_first(it);
         !it.end(); ldi = set.get_next(it)) {
        ASSERT0(ldi);
        if (ldi->isLoopRed()) { return true; }
    }
    return false;
}


bool LoopDepAna::containLoopCarrDep(LoopDepInfoSet const& set)
{
    LoopDepInfoSetIter it;
    for (LoopDepInfo const* ldi = set.get_first(it);
         !it.end(); ldi = set.get_next(it)) {
        ASSERT0(ldi);
        if (ldi->isLoopCarr()) { return true; }
    }
    return false;
}


void LoopDepAna::analyzeLinearDep(
    IR const* ir, IR const* tgt, OUT LoopDepInfoSet & set, MOD LoopDepCtx & ctx)
{
    ASSERT0(ir && ctx.getLI());
    ASSERT0(tgt->is_exp() || tgt->is_stmt());
    if (tgt == ir) { return; }
    if (!xoc::isDependent(ir, tgt, is_aggressive(), m_rg)) { return; }
    if (xoc::isLoopIndependent(ir, tgt, is_aggressive(), ctx.getLI(),
                               m_rg, m_gvn)) {
        LoopDepInfo tmp;
        LDI_kind(&tmp) = LOOP_DEP_INDEPENDENT;
        LDI_src(&tmp) = ir;
        tmp.setTgtIR(tgt);
        LoopDepInfo const* ldi = ctx.appendLoopDepInfo(tmp);
        set.append(ldi);
        return;
    }
    LoopDepInfo tmp;
    LDI_kind(&tmp) = LOOP_DEP_CARRIED;
    LDI_src(&tmp) = ir;
    tmp.setTgtIR(tgt);
    LoopDepInfo const* ldi = ctx.appendLoopDepInfo(tmp);
    set.append(ldi);
}


void LoopDepAna::analyzeLinearDep(
    IR const* ir, xcom::List<IR*> const& lst, OUT LoopDepInfoSet & set,
    MOD LoopDepCtx & ctx)
{
    ASSERT0(ir && ctx.getLI());
    xcom::List<IR*>::Iter it;
    for (IR * tgt = lst.get_head(&it);
         tgt != nullptr; tgt = lst.get_next(&it)) {
        ASSERT0(tgt->is_exp() || tgt->is_stmt());
        analyzeLinearDep(ir, tgt, set, ctx);
    }
}


void LoopDepAna::analyzeRedDep(
    IR const* ir, OUT LoopDepInfoSet & set, MOD LoopDepCtx & ctx)
{
    if (!ir->is_exp()) { return; }
    if (!ir->isDirectMemOp() && !ir->isReadPR()) { return; }
    LoopDepInfo tmp;
    if (!xoc::hasLoopReduceDep(ir, m_rg, ctx.getLI(), tmp)) { return; }
    LoopDepInfo const* ldainfo = ctx.appendLoopDepInfo(tmp);
    set.append(ldainfo);
}


void LoopDepAna::analyzeDep(
    IR const* ir, xcom::List<IR*> const& lst, OUT LoopDepInfoSet & set,
    MOD LoopDepCtx & ctx)
{
    ctx.add(ir); //setting ir that is already analyzed.
    analyzeLinearDep(ir, lst, set, ctx);
    analyzeRedDep(ir, set, ctx);
}


void LoopDepAna::analyzeDep(
    IR const* ir, IR const* tgt, OUT LoopDepInfoSet & set, MOD LoopDepCtx & ctx)
{
    ctx.add(ir); //setting ir that is already analyzed.
    analyzeLinearDep(ir, tgt, set, ctx);
    analyzeRedDep(ir, set, ctx);
}


bool LoopDepAna::transLoopCarrToLoopIndep(
    IR const* ir, MOD LoopDepInfoSet & set, MOD LoopDepCtx & ctx)
{
    bool changed = false;
    LoopDepInfoSetIter it;
    List<LoopDepInfo const*> remove;
    List<LoopDepInfo const*> add;
    for (LoopDepInfo const* ldi = set.get_first(it);
         !it.end(); ldi = set.get_next(it)) {
        ASSERT0(ldi);
        if (!ldi->isLoopCarr()) { continue; }
        if (!isSameMemLocViaEVN(*ldi, &ctx)) { continue; }

        //Revise loop-carried to loop-independent to make loop dependence
        //more precise.
        ASSERT0(ldi->isTgtIR());
        ctx.getActMgr().dumpAct(ir,
            "%s and %s access same memory location, thus they have "
            "loop-independent dependence",
            DumpIRName().dump(ldi->getSrc()),
            DumpIRName().dump(ldi->getTgtIR()));
        LoopDepInfo t(*ldi);
        LDI_kind(&t) = LOOP_DEP_INDEPENDENT;
        LoopDepInfo const* newldi = ctx.appendLoopDepInfo(t);
        changed = true;
        remove.append_tail(ldi);
        add.append_tail(newldi);
    }
    for (LoopDepInfo const* l = remove.get_head();
         l != nullptr; l = remove.get_next()) {
        set.remove(l);
    }
    for (LoopDepInfo const* l = add.get_head();
         l != nullptr; l = add.get_next()) {
        set.append(l);
    }
    return changed;
}


void LoopDepAna::analyzeDepForIRTree(
    IR const* ir, IR const* tgt, OUT LoopDepInfoSet & set, MOD LoopDepCtx & ctx)
{
    ASSERT0(ir->is_exp() || ir->is_stmt());
    ASSERT0(tgt->is_exp() || tgt->is_stmt());
    ConstIRIter it;
    for (IR const* x = xoc::iterInitC(ir, it, false);
         x != nullptr; x = xoc::iterNextC(it, true)) {
        if (!x->isMemRefNonPR()) { continue; }
        analyzeDep(x, tgt, set, ctx);
    }
    transLoopCarrToLoopIndep(ir, set, ctx);
}


void LoopDepAna::analyzeDepAndRefineDep(
    IR const* ir, IR const* tgt, OUT LoopDepInfoSet & set, MOD LoopDepCtx & ctx)
{
    analyzeDep(ir, tgt, set, ctx);
    transLoopCarrToLoopIndep(ir, set, ctx);
}


void LoopDepAna::analyzeDepForIRTree(
    IR const* ir, xcom::List<IR*> const& lst, OUT LoopDepInfoSet & set,
    LoopDepCtx & ctx)
{
    ASSERT0(ir->is_exp() || ir->is_stmt());
    ConstIRIter it;
    for (IR const* x = xoc::iterInitC(ir, it, false);
         x != nullptr; x = xoc::iterNextC(it, true)) {
        if (!x->isMemRefNonPR()) { continue; }
        analyzeDep(x, lst, set, ctx);
    }
    transLoopCarrToLoopIndep(ir, set, ctx);
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


bool LoopDepAna::dump(LoopDepCtx const* ctx) const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    if (ctx != nullptr) {
        ctx->dump();
    }
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


bool LoopDepAna::initDepPass(MOD OptCtx & oc)
{
    if (!oc.is_ref_valid()) { return false; }

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
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_DOM, PASS_LOOP_INFO, PASS_IVR, PASS_UNDEF);
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
    return true;
}


bool LoopDepAna::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    reset();
    START_TIMER(t, getPassName());
    if (!initDepPass(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLoopDepAna()) {
        dump(nullptr);
    }
    m_rg->getLogMgr()->cleanBuffer();
    END_TIMER(t, getPassName());
    return false;
}
//END LoopDepAna

} //namespace xoc
