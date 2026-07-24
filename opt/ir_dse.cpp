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

class VisitFunc : public xcom::VisitTreeFuncBase {
    COPY_CONSTRUCTOR(VisitFunc);
    bool m_is_changed;
    DSECtx & m_ctx;
public:
    VisitFunc(MOD DSECtx & ctx) : m_ctx(ctx) { m_is_changed = false; }

    bool isChanged() const { return m_is_changed; }

    void visitWhenAllKidHaveBeenVisited(Vertex const*, Stack<Vertex const*> &)
    {}
    bool visitWhenFirstMeet(Vertex const* v, Stack<Vertex const*> &)
    {
        m_is_changed |= m_ctx.getDSE()->doBB(
            m_ctx.getCFG()->getBB(v->id()), m_ctx);
        return true;
    }
};


static void dumpReasonPreventDSE(
    DSECtx const& ctx, IR const* ir, CHAR const* format, ...)
{
    ActMgr * am = ctx.getActMgr();
    if (am == nullptr) { return; }
    if (!am->getRegion()->isLogMgrInit()) { return; }
    if (!g_dump_opt.isDumpPass(PASS_DSE)) { return; }
    xcom::StrBuf reasonbuf(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        reasonbuf.vstrcat(format, args);
        va_end(args);
    }
    xcom::StrBuf tmp(8);
    ActHandler ach = am->dump("%s prevents DSE",
        xoc::dumpIRName(ir, tmp));
    if (format != nullptr) {
        ASSERT0(ach.info);
        ach.info->strcat(", the reason is:%s", reasonbuf.getBuf());
    }
}


static void dumpReasonPreventDSE(
    DSECtx const& ctx, MDPhi const* phi, CHAR const* format, ...)
{
    ActMgr * am = ctx.getActMgr();
    if (am == nullptr) { return; }
    if (!am->getRegion()->isLogMgrInit()) { return; }
    if (!g_dump_opt.isDumpPass(PASS_DSE)) { return; }
    xcom::StrBuf reasonbuf(64);
    if (format != nullptr) {
        va_list args;
        va_start(args, format);
        reasonbuf.vstrcat(format, args);
        va_end(args);
    }
    xcom::FixedStrBuf<32> tmp;
    tmp.strcat("MDPhi%u:MD%uV%u",
        phi->id(), phi->getResult()->mdid(), phi->getResult()->version());
    ActHandler ach = am->dump("%s prevents DSE", tmp.getBuf());
    if (format != nullptr) {
        ASSERT0(ach.info);
        ach.info->strcat(", the reason is:%s", reasonbuf.getBuf());
    }
}


static void dumpRemovedIR(IR const* ir, DSECtx & ctx)
{
    if (ir == nullptr || !ctx.getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_DSE)) { return; }
    xcom::StrBuf buf(32);
    ctx.getRegion()->getLogMgr()->incIndent(2);
    xoc::dumpIRToBuf(
        ir, ctx.getRegion(), buf,
        DumpFlag::combineIRID(IR_DUMP_DEF|IR_DUMP_KID), 2);
    ctx.getRegion()->getLogMgr()->decIndent(2);
    ActMgr * am = ctx.getActMgr();
    am->dump("removed IR:%s", buf.getBuf());
}


//
//START DSECtx
//
DSECtx::DSECtx(DSE * dse, OptCtx & oc, ActMgr * am, xcom::DomTree const& dt)
    : PassCtx(&oc, am), domtree(dt)
{
    m_dse = dse;
    m_cfg = oc.getRegion()->getCFG();
    m_mdssamgr = dse->getMDSSAMgr();
}
//END DSECtx


//
//START VisitDomTree
//
class VisitDomTree : public xcom::VisitTree<VisitFunc> {
    COPY_CONSTRUCTOR(VisitDomTree);
public:
    VisitDomTree(IRBB * root, DomTree const& domtree, VisitFunc & vf)
        : VisitTree(domtree, root->id(), vf) {}
};
//END VisitDomTree


class DSEConstNextMDDefIter : public ConstNextMDDefIter {
protected:
    Region const* m_rg;
    MDDef const* m_root;
protected:
    virtual bool isStopAccess(MDDef const* mddef) override
    {
        if (mddef->getPrev() == m_root) {
            //Keep accessing the MDDef.
            return false;
        }
        //We only access the next-def of the 'm_root'.
        return true;
    }
public:
    DSEConstNextMDDefIter(
        MDSSAMgr const* mdssamgr, OptCtx const* oc,
        bool is_cross_phi, MDDef const* root)
            : ConstNextMDDefIter(mdssamgr, oc, is_cross_phi)
    {
        m_root = root;
        m_rg = oc->getRegion();
    }
};


//
//START DSE
//
DSE::DSE(Region * rg) : Pass(rg), m_am(rg)
{
    ASSERT0(rg != nullptr);
}


//Return true if ir1's value is killed by ir2.
//e.g:g = xxx; #S1
//    g = yyy; #S2
//where #S1 killed by #S2.
static bool isKilled(
    IR const* ir1, IR const* ir2, Region const* rg, OptCtx const* oc)
{
    if (!xoc::isDependent(ir1, ir2, true, rg)) {
        //ir1 and ir2 are unrelated.
        return false;
    }
    if (xoc::isCover(ir2, ir1, nullptr, oc)) {
        //The value defined by ir1 can NOT cross ir2.
        return true;
    }
    return false;
}


//Return true if we make sure that stmt does not have any USE, and its stored
//value will be overrided by Next-DEF stmt.
//Otherwise, return false to tell caller we have no knowledge about it.
//find_next_def: record whether there exists next-def for 'stmt'.
static bool canBeCoveredByNextDefByMDSSA(
    IR const* stmt, MDDef const* irmddef, DSECtx const& ctx,
    OUT bool & find_next_def)
{
    MDSSAMgr const* mgr = ctx.getMDSSAMgr();
    OptCtx const* oc = ctx.getOptCtx();
    Region const* rg = ctx.getRegion();
    DSEConstNextMDDefIter ndit(mgr, oc, false, irmddef);
    MDDef const* nd = ndit.get_first(irmddef);
    ASSERT0(nd == irmddef);
    find_next_def = false;
    for (nd = ndit.get_next(); nd != nullptr; nd = ndit.get_next()) {
        if (nd->is_phi()) {
            //We can not get exact result.
            dumpReasonPreventDSE(
                ctx, (MDPhi const*)nd,
                "cannot keep analyzing because the next-def is MDPhi%u.",
                nd->id());
            return false;
        }
        IR const* ndocc = nd->getOcc();
        if (!isKilled(stmt, ndocc, rg, oc)) {
            //We can not get exact result.
            xcom::StrBuf tmp(8);
            dumpReasonPreventDSE(
                ctx, ndocc,
                "cannot guarantee the next-def stmt covers %s",
                xoc::dumpIRName(stmt, tmp));
            return false;
        }
        find_next_def = true;
    }
    return true;
}


static bool canBeDeadStore(IR const* stmt, DSECtx const& ctx)
{
    if (!MDSSAMgr::hasMDSSAInfo(stmt)) {
        //We prefer to arbitrate the optimization through MDSSA info.
        return false;
    }
    MDSSAMgr const* mgr = ctx.getMDSSAMgr();
    OptCtx const* oc = ctx.getOptCtx();
    Region const* rg = ctx.getRegion();
    MDSSAInfo const* info = mgr->getMDSSAInfoIfAny(stmt);
    ASSERT0(info);
    VOpndSetIter it = nullptr;
    VOpndSet const& vopndset = info->readVOpndSet();
    UseDefMgr const* udmgr = const_cast<MDSSAMgr*>(mgr)->getUseDefMgr();
    for (BSIdx i = vopndset.get_first(&it); i != BS_UNDEF;
         i = vopndset.get_next(i, &it)) {
        VMD const* vmd = (VMD*)udmgr->getVOpnd(i);
        if (vmd == nullptr) {
            //CASE:licm_mdssa2.c
            //current i may have been removed by moveStmtToTgtBB() at last
            //round of the iteration.
            continue;
        }
        if (vmd->hasUse()) { return false; }
        MDDef const* vmddef = vmd->getDef();
        bool find_next_def = false;
        if (!canBeCoveredByNextDefByMDSSA(stmt, vmddef, ctx, find_next_def)) {
            return false;
        }
        if (!find_next_def) {
            //CASE:
            //  foo() {
            //    g=1;
            //  }
            //Althrough g=1 has no USE, DSE still not eliminate
            //it. Leave this work to DCE.
            return false;
        }
    }
    return true;
}


bool DSE::doStmt(
    IR * ir, MOD BBIRList & irlst, MOD BBIRListIter & it, DSECtx & ctx)
{
    ASSERT0(ir && ir->is_stmt());
    if (!canBeCandidate(ir)) { return false; }
    if (!canBeDeadStore(ir, ctx)) {
        //We know nothing about it.
        return false;
    }
    bool changed = true;
    dumpRemovedIR(ir, ctx);
    xoc::removeStmt(ir, getRegion(), *ctx.getOptCtx());
    ctx.tryInvalidInfoBeforeFreeIR(ir);

    //stmt is safe to free.
    getRegion()->freeIRTree(ir);

    //Remove stmt from BB.
    irlst.EList<IR*, IR2Holder>::remove(it);
    return changed;
}


bool DSE::useMDSSADU() const
{
    return m_mdssamgr != nullptr && m_mdssamgr->is_valid();
}


bool DSE::doBB(IRBB * bb, DSECtx & ctx)
{
    bool changed = false;
    BBIRList & irlst = bb->getIRList();
    BBIRListIter it;
    BBIRListIter nextit;
    for (irlst.get_head(&it); it != nullptr; it = nextit) {
        nextit = it;
        irlst.get_next(&nextit);
        IR * ir = it->val();
        changed |= doStmt(ir, irlst, it, ctx);
    }
    return changed;
}


bool DSE::doBBInDomTreeOrder(OptCtx & oc)
{
    IRCFG * cfg = getRegion()->getCFG();
    IRBB * entry = cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    bool changed = false;
    UINT count = 0;
    UINT const max_try = 50;
    xcom::DomTree domtree;
    cfg->genDomTree(domtree);
    DSECtx ctx(this, oc, &getActMgr(), domtree);
    do {
        VisitFunc vf(ctx);
        VisitDomTree visit(entry, domtree, vf);
        visit.visit();
        if (!vf.isChanged()) { break; }
        changed = true;
        ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
        count++;
    } while (count < max_try);
    ASSERT0(count < max_try);
    return changed;
}


bool DSE::dump() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpPass(PASS_DSE))
    { return true; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    const_cast<DSE*>(this)->getActMgr().dump();
    bool succ = Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    return succ;
}


bool DSE::initDepPass(MOD OptCtx & oc)
{
    ASSERTN(oc.is_cfg_valid(), ("DSE need CFG info"));
    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = m_rg->getMDSSAMgr();
    ASSERT0L3(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    if (!useMDSSADU()) {
        //DSE needs MDSSA.
        return false;
    }
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    return true;
}


//Perform Dead Store Elmination.
//A dead store is a store into a memory location which will later be
//overwritten by another store without any intervening loads.  In this
//case the earlier store can be deleted.
bool DSE::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    if (!initDepPass(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    dumpBeforePass();
    bool changed = doBBInDomTreeOrder(oc);
    END_TIMER(t, getPassName());
    if (!changed) {
        m_rg->getLogMgr()->cleanBuffer();
        return false;
    }
    dump();
    ASSERT0(xoc::verifyMDRef(m_rg, oc));
    ASSERT0(xoc::verifyClassicDUChain(m_rg, oc));
    ASSERT0(xoc::verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    return true;
}
//END DSE

} //namespace xoc
