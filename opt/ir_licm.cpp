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

static void dumpAnaCtx(LICM * licm, LICMAnaCtx const& ctx)
{
    Region const* rg = licm->getRegion();
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpPass(PASS_LICM)) { return; }
    class Dump : public xoc::DumpToBuf {
    public:
        LICMAnaCtx const* ctx;
        Dump(Region const* rg, xcom::StrBuf & buf) : DumpToBuf(rg, buf, 2) {}
        virtual void dumpUserInfo() const override { ctx->dump(); }
    };
    xcom::StrBuf buf(32);
    Dump d(rg, buf);
    d.ctx = &ctx;
    licm->getActMgr().dump("%s", d.dump());
}


static bool isImmRHS(IR const* exp, IR const* stmt)
{
    ASSERT0(exp->is_exp() && stmt->is_stmt());
    return exp == stmt->getRHS();
}


static void updateDomTreeByPreheader(
    LI<IRBB> const* li, IRBB const* preheader, DomTree & domtree,
    ActMgr & actmgr)
{
    ASSERT0(preheader);
    //Only insert preheader into domtree.
    domtree.insertParent(li->getLoopHead()->id(), preheader->id());
    actmgr.dump("maintain DomTree: set BB%u dominates BB%u",
                preheader->id(), li->getLoopHead()->id());
}


static void updateDomTreeByGuardRegion(
    LI<IRBB> const* li, InsertGuardHelper const& help,
    DomTree & domtree, ActMgr & actmgr)
{
    ASSERT0(help.getGuardEnd());
    domtree.insertParent(li->getLoopHead()->id(), help.getGuardEnd()->id());
    actmgr.dump("maintain DomTree: set BB%u dominates BB%u",
                help.getGuardEnd()->id(), li->getLoopHead()->id());

    domtree.insertParent(help.getGuardEnd()->id(),
                         help.getGuardStart()->id());
    actmgr.dump("maintain DomTree: set BB%u dominates BB%u",
                help.getGuardStart()->id(), help.getGuardEnd()->id());

    domtree.insertKid(help.getGuardStart()->id(), help.getGuardedBB()->id());
    actmgr.dump("maintain DomTree: set BB%u dominates BB%u",
                help.getGuardedBB()->id(), help.getGuardStart()->id());
}


static void dumpHoistedIR(IR const* ir, IRBB const* bb, LICM const* licm)
{
    ASSERT0(licm);
    if (ir == nullptr || !licm->getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_LICM)) { return; }
    xcom::StrBuf buf(32);
    licm->getRegion()->getLogMgr()->incIndent(2);
    xoc::dumpIRToBuf(ir, licm->getRegion(), buf,
                     DumpFlag::combineIRID(IR_DUMP_DEF));
    licm->getRegion()->getLogMgr()->decIndent(2);
    ActMgr & am = const_cast<LICM*>(licm)->getActMgr();
    am.dump("hoisted IR to BB%u:%s", bb->id(), buf.getBuf());
}


static void dumpMoveIRToPreheader(
    IR const* ir, IRBB const* prehead, LICM const* licm)
{
    if (ir == nullptr || !licm->getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_LICM)) { return; }
    xcom::StrBuf buf(32);
    xoc::dumpIRName(ir, buf);
    ActMgr & am = const_cast<LICM*>(licm)->getActMgr();
    am.dump("move %s to preheader BB%u", buf.getBuf(), prehead->id());
}


static void dumpMovedIRExecOnce(
    IR const* ir, IRBB const* from, IRBB const* to, LICM const* licm)
{
    if (ir == nullptr || !licm->getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_LICM)) {
        return;
    }
    xcom::StrBuf buf(32);
    ActMgr & am = const_cast<LICM*>(licm)->getActMgr();
    am.dump("%s executes at least once, thus it "
            "should be moved from BB%u to BB%u",
            dumpIRName(ir, buf), from->id(), to->id());
}


static void dumpMultiDef(LICM * licm, IR const* ir1, IR const* ir2)
{
    xcom::StrBuf buf1(32), buf2(32);
    licm->getActMgr().dump(
        "find multi-def of the same memory:%s and %s may be overlapped",
        xoc::dumpIRName(ir1, buf1),
        xoc::dumpIRName(ir2, buf2));
}


static void moveStmtToPreheader(
    MOD IR * stmt, MOD IRBB * tgtbb, OUT HoistCtx & ctx)
{
    ASSERT0(tgtbb);
    if (stmt->getBB() != nullptr) {
        //Some new generated stmt does not have set BB yet.
        //The stmt is used to hold the value of hoisted-expression.
        ASSERT0(tgtbb != stmt->getBB());
        stmt->getBB()->getIRList().remove(stmt);
        ctx.stmt_changed = true;
    }
    ctx.duset_changed = true;

    //CASE:We do not use append_tail_ex here, because the end IR of preheader
    //may be readonly CALL stmt, it is also the invariant stmt. e.g:licm.gr
    //Post process the preheader BB and split it into two fallthrough BBs and
    //put the CALL into the first BB, its sebsequent stmts into second BB.
    //tgtbb->getIRList().append_tail_ex(stmt);
    tgtbb->getIRList().append_tail(stmt);
    ctx.addHoistedStmt(stmt);
}


class LICMConstNextMDDefIter : public ConstNextMDDefIter {
protected:
    LICMAnaCtx const& m_anactx;
    IR const* m_stmt;
protected:
    virtual bool isStopAccess(MDDef const* mddef) override
    {
        IRBB const* bb = nullptr;
        if (mddef->is_phi()) { bb = ((MDPhi const*)mddef)->getBB(); }
        else { bb = mddef->getOcc()->getBB(); }
        ASSERT0(bb);
        if (!m_anactx.getLI()->isInsideLoop(bb->id())) {
            //There is no need to access outside MDDef.
            return true;
        }
        //IR const* mddefocc = mddef->getOcc();
        //ASSERT0(mddefocc);
        //if (xoc::isCover(mddefocc, m_stmt, nullptr, this->getOptCtx())) {
        //    //stmt's effect will not cross the MDDef, thus there is no need
        //    //to iterate more subsequent MDDefs.
        //    return true;
        //}
        //Keep accessing the MDDef.
        return false;
    }
public:
    LICMConstNextMDDefIter(
        LICMAnaCtx const& anactx, MDSSAMgr const* mdssamgr, OptCtx const* oc,
        bool is_cross_phi, IR const* stmt)
            : ConstNextMDDefIter(mdssamgr, oc, is_cross_phi), m_anactx(anactx)
        { m_stmt = stmt; }
};


//
//START HoistCtx
//
HoistCtx::HoistCtx(OptCtx & oc, DomTree * dt, LICM * licm, Stmt2UseMgr * s2u)
    : PassCtx(&oc, &licm->getActMgr())
{
    ASSERT0(dt && licm);
    ASSERT0(getRegion());
    m_prssamgr = getRegion()->getPRSSAMgr();
    m_mdssamgr = getRegion()->getMDSSAMgr();
    m_licm = licm;
    inserted_guard_bb = false;
    cfg_changed = false;
    duset_changed = false;
    stmt_changed = false;
    domtree = dt;
    m_cfg = getRegion()->getCFG();
    ASSERT0(s2u);
    m_s2u_mgr = s2u;
    ASSERT0(m_cfg && m_cfg->is_valid());
}


HoistCtx::HoistCtx(HoistCtx const& src) : PassCtx(src)
{
    inserted_guard_bb = false;
    cfg_changed = false;
    duset_changed = false;
    stmt_changed = false;
    copyTopDownInfo(src);
}


void HoistCtx::recordUseSet(IR const* ir)
{
    ASSERT0(ir->is_stmt());
    m_s2u_mgr->copyUseSet(ir);
}


void HoistCtx::dump() const
{
    Region const* rg = getRegion();
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpPass(PASS_LICM)) { return; }
    note(rg, "\n-- DUMP HoistCtx --", rg->getRegionName());
    rg->getLogMgr()->incIndent(2);
    note(rg, "\ninserted_guard_bb=%s", inserted_guard_bb ? "true":"false");
    note(rg, "\ncfg_changed=%s", cfg_changed ? "true":"false");
    note(rg, "\nduset_changed=%s", duset_changed ? "true":"false");
    note(rg, "\nstmt_changed=%s", stmt_changed ? "true":"false");
    xcom::StrBuf buf(32);
    note(rg, "\n-- Hoisted IR --");
    rg->getLogMgr()->incIndent(2);
    IRTabIter it;
    for (IR const* ir = m_hoisted_stmt.get_first(it);
         ir != nullptr; ir = m_hoisted_stmt.get_next(it)) {
        buf.clean();
        xoc::dumpIRToBuf(
            ir, rg, buf, DumpFlag::combineIRID(IR_DUMP_DEF|IR_DUMP_KID), 0);
        prt(rg, "%s", buf.getBuf());
    }
    rg->getLogMgr()->decIndent(2);

    note(rg, "\n-- Use OCC Of Hoisted Stmt In Loop Body --");
    rg->getLogMgr()->incIndent(2);
    Stmt2UseMgr::IR2IRSetIter ir2irsetit;
    IRSet * iset;
    Stmt2UseMgr const& s2umgr = getS2UMgr();
    xcom::StrBuf buf2(32);
    for (IR const* ir = s2umgr.getStmt2UseSet().get_first(ir2irsetit, &iset);
         ir != nullptr;
         ir = s2umgr.getStmt2UseSet().get_next(ir2irsetit, &iset)) {
        buf.clean();
        note(rg, "\nhoisted stmt:%s", xoc::dumpIRName(ir, buf));
        rg->getLogMgr()->incIndent(2);
        ASSERT0(iset);
        note(rg, "\n");
        bool first = true;
        IRSetIter isetit;
        for (BSIdx i = iset->get_first(&isetit);
             i != BS_UNDEF; i = iset->get_next(i, &isetit)) {
            if (first) { first = false; }
            else { prt(rg, ","); }
            IR const* useocc = rg->getIR(i);
            ASSERT0(useocc);
            buf2.clean();
            prt(rg, "%s", xoc::dumpIRName(useocc, buf2));
        }
        rg->getLogMgr()->decIndent(2);
    }
    rg->getLogMgr()->decIndent(2);

    //END
    rg->getLogMgr()->decIndent(2);
}
//END HoistCtx


//
//START MoveStmtBetweenBB
//
class MoveStmtBetweenBB {
    COPY_CONSTRUCTOR(MoveStmtBetweenBB);
protected:
    Region const* m_rg;
    OptCtx const* m_oc;
    ActMgr * m_am; //optional, if it is not NULL, invoke the ActMgr.
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
protected:
    void dumpMovedIR(IR const* ir, IRBB const* from, IRBB const* to) const;

    ActMgr * getActMgr() const { return m_am; }
    Region const* getRegion() const { return m_rg; }

    //Return true if any stmt that is related to 'exp' that is moved to 'tgtbb'.
    bool moveDefByDUChain(IR const* exp, IRBB * srcbb, IRBB * tgtbb) const;

    //Return true if any stmt that is related to 'exp' is moved to 'tgtbb'.
    bool moveDefByPRSSA(IR const* exp, IRBB * srcbb, IRBB * tgtbb) const;

    //Return true if any stmt that is related to 'exp' is moved to 'tgtbb'.
    bool moveDefByClassicDU(IR const* exp, IRBB * srcbb, IRBB * tgtbb) const;

    //Return true if any stmt that is related to 'exp' is moved to 'tgtbb'.
    bool moveDefByMDSSA(IR const* exp, IRBB * srcbb, IRBB * tgtbb) const;
    void moveStmtToTgtBB(
        BBIRListIter irit, MOD IRBB * srcbb, MOD IRBB * tgtbb) const;
    void moveStmtToTgtBB(
        IR * ir, MOD IRBB * srcbb, MOD IRBB * tgtbb) const;
    bool moveDependentStmtInBB(IR * ir, IRBB * srcbb, IRBB * tgtbb) const;

    bool needToMove(IR const* ir, IRBB const* srcbb) const
    {
        if (ir->is_phi()) { return false; }
        //Note if ir's BB changed, means it has been moved to target BB.
        return ir->getBB() == srcbb;
    }

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    MoveStmtBetweenBB(Region const* rg, OptCtx const* oc, ActMgr * am) :
        m_rg(rg), m_oc(oc), m_am(am)
    {
        m_mdssamgr = m_rg->getMDSSAMgr();
        m_prssamgr = m_rg->getPRSSAMgr();
    }
    void moveStmtListToTgtBB(
        xcom::List<BBIRListIter> const& itlst, MOD IRBB * srcbb,
        MOD IRBB * tgtbb) const;
};


//Return true if any stmt that is related to 'exp' is moved to 'tgtbb'.
bool MoveStmtBetweenBB::moveDefByMDSSA(
    IR const* exp, IRBB * srcbb, IRBB * tgtbb) const
{
    ASSERT0(exp->isMemRefNonPR());
    MDSSAInfo * info = m_mdssamgr->getMDSSAInfoIfAny(exp);
    ASSERTN(info, ("def stmt even not in MDSSA system"));
    VOpndSetIter it = nullptr;
    VOpndSet const& vopndset = info->readVOpndSet();
    UseDefMgr const* udmgr = m_mdssamgr->getUseDefMgr();
    BSIdx nexti;
    for (BSIdx i = vopndset.get_first(&it); i != BS_UNDEF; i = nexti) {
        nexti = vopndset.get_next(i, &it);
        VMD const* vmd = (VMD*)udmgr->getVOpnd(i);
        if (vmd == nullptr) {
            //CASE:licm_mdssa2.c
            //current i may have been removed by moveStmtToTgtBB() at last
            //round of the iteration.
            continue;
        }
        ASSERT0(vmd->is_md());
        MDDef const* def = vmd->getDef();
        if (def == nullptr) {
            //Region livein PR.
            continue;
        }
        ASSERT0(def->getBB());
        if (def->getBB() != srcbb) {
            //Outside src BB def.
            continue;
        }
        if (def->is_phi()) {
            //PHI is used to merge ocntrol-flow-dependence, can not be moved.
            continue;
        }
        IR * defocc = def->getOcc();
        if (!needToMove(defocc, srcbb)) { continue; }
        moveDependentStmtInBB(defocc, srcbb, tgtbb);
        moveStmtToTgtBB(defocc, srcbb, tgtbb);

        //Note after stmt moved, vopndset may be changed.
        if (!vopndset.is_contain(nexti)) {
            //CASE:rp3.c
            i = vopndset.get_first(&it);
        }
    }
    return false;
}


bool MoveStmtBetweenBB::moveDefByPRSSA(
    IR const* exp, IRBB * srcbb, IRBB * tgtbb) const
{
    ASSERT0(exp->isPROp());
    SSAInfo * info = exp->getSSAInfo();
    ASSERTN(info, ("miss PRSSAInfo"));
    //Check if SSA def is loop invariant.
    IR * def = SSA_def(info);
    if (def == nullptr) { return false; }
    if (!needToMove(def, srcbb)) { return false; }
    moveDependentStmtInBB(def, srcbb, tgtbb);
    moveStmtToTgtBB(def, srcbb, tgtbb);
    return true;
}


bool MoveStmtBetweenBB::moveDefByClassicDU(
    IR const* exp, IRBB * srcbb, IRBB * tgtbb) const
{
    ASSERTN(m_rg->getDUMgr(), ("valid DUChain need DUMgr"));
    DUSet const* defset = exp->readDUSet();
    if (defset == nullptr) { return true; }
    DUSetIter di = nullptr;
    BSIdx nexti;
    IRBB const* expbb = exp->getStmt()->getBB();
    ASSERT0(expbb);
    for (BSIdx i = defset->get_first(&di); i != BS_UNDEF; i = nexti) {
        nexti = defset->get_next(i, &di);
        IR * def = m_rg->getIR(i);
        ASSERT0(def);
        if (!needToMove(def, srcbb)) { continue; }
        moveDependentStmtInBB(def, srcbb, tgtbb);
        moveStmtToTgtBB(def, srcbb, tgtbb);
    }
    return true;
}


//Return true if any stmt that is related to 'exp' that is moved to 'tgtbb'.
bool MoveStmtBetweenBB::moveDefByDUChain(
    IR const* exp, IRBB * srcbb, IRBB * tgtbb) const
{
    ASSERT0(exp->is_exp() && exp->isMemOpnd());
    if (exp->isPROp() && usePRSSADU()) {
        return moveDefByPRSSA(exp, srcbb, tgtbb);
    }
    if (exp->isMemRefNonPR() && useMDSSADU()) {
        return moveDefByMDSSA(exp, srcbb, tgtbb);
    }
    ASSERT0(m_oc);
    if (m_oc->is_pr_du_chain_valid() || m_oc->is_nonpr_du_chain_valid()) {
        return moveDefByClassicDU(exp, srcbb, tgtbb);
    }
    return false;
}


bool MoveStmtBetweenBB::moveDependentStmtInBB(
    IR * ir, IRBB * srcbb, IRBB * tgtbb) const
{
    bool changed = false;
    ConstIRIter iriter;
    for (IR const* x = iterExpInitC(ir, iriter);
         x != nullptr; x = iterExpNextC(iriter, true)) {
        if (!x->isMemOpnd()) { continue; }
        changed |= moveDefByDUChain(x, srcbb, tgtbb);
    }
    return changed;
}


void MoveStmtBetweenBB::dumpMovedIR(
    IR const* ir, IRBB const* from, IRBB const* to) const
{
    if (ir == nullptr || !m_rg->isLogMgrInit() ||
        !g_dump_opt.isDumpPass(PASS_LICM)) {
        return;
    }
    ActMgr * am = getActMgr();
    if (am == nullptr) { return; }
    xcom::StrBuf buf(32);
    getRegion()->getLogMgr()->incIndent(2);
    xoc::dumpIRToBuf(ir, getRegion(), buf, DumpFlag::combineIRID(IR_DUMP_DEF));
    getRegion()->getLogMgr()->decIndent(2);
    am->dump("moved IR from BB%u to BB%u:%s",
             from->id(), to->id(), buf.getBuf());
}


void MoveStmtBetweenBB::moveStmtToTgtBB(
    IR * ir, MOD IRBB * srcbb, MOD IRBB * tgtbb) const
{
    srcbb->getIRList().remove(ir);
    tgtbb->getIRList().append_tail_ex(ir);
    dumpMovedIR(ir, srcbb, tgtbb);
}


void MoveStmtBetweenBB::moveStmtToTgtBB(
    BBIRListIter irit, MOD IRBB * srcbb, MOD IRBB * tgtbb) const
{
    IR * ir = irit->val();
    srcbb->getIRList().remove(irit);
    tgtbb->getIRList().append_tail_ex(ir);
    dumpMovedIR(ir, srcbb, tgtbb);
}


void MoveStmtBetweenBB::moveStmtListToTgtBB(
    xcom::List<BBIRListIter> const& itlst, MOD IRBB * srcbb,
    MOD IRBB * tgtbb) const
{
    xcom::List<BBIRListIter>::Iter lit;
    xcom::List<BBIRListIter>::Iter nextlit;
    for (itlst.get_head(&lit); lit != nullptr; lit = nextlit) {
        BBIRListIter irit = lit->val();
        nextlit = lit;
        itlst.get_next(&nextlit);
        IR * ir = irit->val();
        if (!needToMove(ir, srcbb)) { continue; }
        moveDependentStmtInBB(ir, srcbb, tgtbb);
        moveStmtToTgtBB(irit, srcbb, tgtbb);
    }
}
//END MoveStmtBetweenBB


//
//START LICMAnaCtx
//
LICMAnaCtx::LICMAnaCtx(
    Region const* rg, LI<IRBB> const* li, LICM * licm, OptCtx const* oc)
    : m_rg(rg), m_md2defcnt(rg, li)
{
    ASSERT0(licm && rg);
    m_li = li;
    m_licm = licm;
    m_cfg = rg->getCFG();
    ASSERT0(m_cfg && m_cfg->is_valid());
}


void LICMAnaCtx::dump() const
{
    Region const* rg = getRegion();
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpPass(PASS_LICM)) { return; }
    LI<IRBB> const* li = getLI();
    note(rg, "\n-- DUMP LICM Analysis Result:LOOP%u:'%s' --",
         li->id(), rg->getRegionName());
    rg->getLogMgr()->incIndent(2);
    rg->getCFG()->dumpLoopInfo();

    if (getInvExpTab().get_elem_count() > 0) {
        note(rg, "\n");
        prt(rg, "-- INVARIANT EXP (NUM=%d) -- :",
            getInvExpTab().get_elem_count());
        rg->getLogMgr()->incIndent(3);
        IRTabIter ti;
        for (IR * c = getInvExpTab().get_first(ti);
             c != nullptr; c = getInvExpTab().get_next(ti)) {
             xoc::dumpIR(c, rg, nullptr, DumpFlag::combineIRID(IR_DUMP_KID));
        }
        rg->getLogMgr()->decIndent(3);
    }

    if (getInvStmtList().get_elem_count() > 0) {
        note(rg, "\n");
        prt(rg, "-- INVARIANT STMT (NUM=%d) -- :",
            getInvStmtList().get_elem_count());
        rg->getLogMgr()->incIndent(3);
        InvStmtListIter it;
        for (IR * c = getInvStmtList().get_head(&it);
             c != nullptr; c = getInvStmtList().get_next(&it)) {
             xoc::dumpIR(c, rg);
        }
        rg->getLogMgr()->decIndent(3);
    }

    if (getConstCandTab().get_elem_count() > 0) {
        note(rg, "\n");
        prt(rg, "-- HOIST CAND (NUM=%d) -- :",
            getConstCandTab().get_elem_count());
        rg->getLogMgr()->incIndent(3);
        IRTabIter ti;
        for (IR * c = getConstCandTab().get_first(ti);
             c != nullptr; c = getConstCandTab().get_next(ti)) {
             xoc::dumpIR(c, rg, nullptr, DumpFlag::combineIRID(IR_DUMP_KID));
        }
        rg->getLogMgr()->decIndent(3);
    }

    if (!const_cast<LICMAnaCtx*>(this)->getMD2DefCnt().is_empty()) {
        note(rg, "\n");
        const_cast<LICMAnaCtx*>(this)->getMD2DefCnt().dump();
    }
    rg->getLogMgr()->decIndent(2);
}


bool LICMAnaCtx::verifyInvStmt(IR const* ir) const
{
    ConstIRIter it;
    for (IR const* x = iterExpInitC(ir, it);
         x != nullptr; x = iterExpNextC(it)) {
        if (x->isConstExp()) { continue; }

        //NOTE:Since dummyuse of call is regarded as MayDef and MayUse of a
        //call-stmt, thus if a call can be hoisted means its dummyuse also
        //should be hoisted.
        ASSERT0(isInvExp(x));
    }
    return true;
}


void LICMAnaCtx::addHoistCand(IR * ir)
{
    m_hoist_cand.append(ir);
}


void LICMAnaCtx::addHoistCand(IRList const& list)
{
    IRListIter it;
    for (IR * ir = list.get_head(&it); ir != nullptr;
         ir = list.get_next(&it)) {
        ASSERT0(!m_hoist_cand.find(ir));
        m_hoist_cand.append(ir);
    }
}
//END LICMAnaCtx


//
//START InsertPreheaderMgr
//
class InsertPreheaderMgr {
    COPY_CONSTRUCTOR(InsertPreheaderMgr);
    Region * m_rg;
    OptCtx * m_oc;
    LI<IRBB> const* m_li;
    IRBB * m_preheader;
    MDSSAMgr * m_mdssamgr;
    IRCFG * m_cfg;
    LICM * m_licm;
    RCE const* m_rce;
    ActMgr * m_am;
    LICMAnaCtx const& m_anactx;
    HoistCtx const& m_hoistctx;
    InsertGuardHelper m_gdhelp;
private:
    //rce: RCE object, may be null.
    void checkAndInsertGuardBB(IRTab const& irtab, OUT HoistCtx & ctx);

    //Return true if loop body is executed conditionally which is in charged of
    //the judgement stmt in loophead BB.
    //e.g:Return true for while-do loop, and false for do-while loop.
    bool isLoopExecConditional() const;

    //The function will remove the MDPhi that inserted by
    //insertPreheader().
    //NOTE: The preheader can only contain Phi at most.
    void removeInsertedMDPhi();

    //Try to evaluate the value of loop execution condition.
    //Returnt true if this function evaluated successfully,
    //otherwise return false.
    bool tryEvalLoopExecCondition(
        OUT bool & must_true, OUT bool & must_false, HoistCtx const& ctx) const;

    //The funtion should be invoked after exp hoisted.
    void updateMDSSADUForExpInPreHeader();
    void updateMDSSADUForExpInGuardStart();
    void updateMDSSADUForExpInBB(IRBB const* bb);
    void updateUseDefChainForExpInBB(IRBB const* bb);
    void updateDefUseAndDefDefChainForStmtInBB(IRBB const* bb);

    //The function revises the MDSSAInfo for stmt and its
    //NOTE:The necessary Phi and MDPhi should have been inserted into guarded BB
    //before call this function.
    void updateMDSSADUForUseOccOfHoistedStmtInLoopBody(HoistCtx const& ctx);

    //The funtion should be invoked after phi modified.
    void updateMDSSADUForLoopHeadPhi(HoistCtx const& ctx);
    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    void updateDomTree(DomTree & domtree);
public:
    InsertPreheaderMgr(
        LICM * licm, RCE const* rce, LICMAnaCtx const& anactx,
        HoistCtx const& hctx)
        : m_anactx(anactx), m_hoistctx(hctx),
          m_gdhelp(hctx.getRegion(), hctx.getOptCtx(), hctx.getActMgr())
    {
        m_rg = hctx.getRegion();
        m_oc = hctx.getOptCtx();
        m_preheader = nullptr;
        m_licm = licm;
        m_rce = rce;
        m_am = hctx.getActMgr();
        m_mdssamgr = m_rg->getMDSSAMgr();
        m_cfg = m_rg->getCFG();
        m_li = anactx.getLI();
    }

    LICM * getLICM() const { return m_licm; }
    IRBB * getPreheader() const { return m_preheader; }
    InsertGuardHelper const& getInsertGuardHelper() const { return m_gdhelp; }
    ActMgr * getActMgr() const { return m_am; }
    Region * getRegion() const { return m_rg; }
    LI<IRBB> const* getLI() const { return m_li; }

    bool needComplicatedGuard() const
    { return m_gdhelp.needComplicatedGuard(m_li); }

    void reviseSSADU(HoistCtx const& ctx);

    //The function will check whether stmts which is placed in 'guarded_bb'
    //should be moved to guard_start BB.
    //e.g: given loop struct falsebr (i < n) {...}, after hoisting and
    //inserting guard, the IR list will be:
    //  falsebr i < n, L1
    //  $1 = n //S1
    //  L1:
    //  falsebr (i < $1) //S2
    //  {...}
    //In above case, stmt S1 should always execute at least once. Otherwise,
    //stmt S2 will reference an undefined $1 when i >= n.
    void reviseStmtExecAtLeastOnce();

    void undoCFGChange(HoistCtx & ctx);

    //The function insert preheader before loop.
    //Return true if the function inserted a new BB as preheader, otherwise
    //regard an existing BB as preheader.
    bool perform(IRTab const& irtab, MOD HoistCtx & ctx);
};


//Return true if loop body is executed conditionally which is in charged of
//the judgement stmt in loophead BB.
//e.g:Return true for while-do loop, and false for do-while loop.
bool InsertPreheaderMgr::isLoopExecConditional() const
{
    IRBB const* head = m_li->getLoopHead();
    ASSERT0(head);
    IR const* last = const_cast<IRBB*>(head)->getLastIR();
    return last != nullptr && last->isConditionalBr();
}


//Try to evaluate the value of loop execution condition.
//Returnt true if this function evaluated successfully, otherwise return false.
bool InsertPreheaderMgr::tryEvalLoopExecCondition(
    OUT bool & must_true, OUT bool & must_false, HoistCtx const& ctx) const
{
    //rce: RCE object, may be null.
    if (m_rce == nullptr) { return false; }
    IRBB const* head = m_li->getLoopHead();
    ASSERT0(head);
    IR const* last = const_cast<IRBB*>(head)->getLastIR();
    ASSERT0(last && last->isConditionalBr());

    //Try to evaluate the value of judgement operation.
    RCECtx rcectx(*m_oc, m_am);
    return m_rce->calcCondMustVal(BR_det(last), must_true, must_false, rcectx);
}


void InsertPreheaderMgr::checkAndInsertGuardBB(
    IRTab const& irtab, OUT HoistCtx & ctx)
 {
    if (xoc::g_do_licm_no_guard || !isLoopExecConditional()) { return; }
    ASSERT0(m_licm);
    bool must_true, must_false;
    if (tryEvalLoopExecCondition(must_true, must_false, ctx) && must_true) {
        return; //guard BB is unnecessary
    }
    IRTabIter it;
    for (IR const* c = irtab.get_first(it);
         c != nullptr; c = irtab.get_next(it)) {
        ASSERT0(c->is_exp() || c->is_stmt());
        IR const* stmt = nullptr;
        if (c->is_exp()) {
            stmt = c->getStmt();
            if (!stmt->isStoreStmt()) { continue; }
            if (!isImmRHS(c, stmt)) { continue; }
        } else {
            stmt = c;
            if (!stmt->isStoreStmt()) { continue; }
        }
        if (!m_anactx.isInvStmt(stmt)) { continue; }
        if (!xoc::isStmtDomAllUseInsideLoop(stmt, m_li, m_rg, *m_oc)) {
            continue;
        }
        if (stmt->getBB() == m_li->getLoopHead()) { continue; }
        if (m_licm->hasInsertedGuardBB(m_li)) { continue; }

        //Guard BB is necessary.
        IRBB * guard = m_gdhelp.insertGuard(m_li, m_preheader);
        m_licm->getActMgr().dump("insert guard BB%u before preheader BB%u",
            guard->id(), m_preheader->id());

        //Move PRPHI and MDPHI from original preheader BB to guard BB.
        //NOTE:Since the PRPHI and MDPhi's DU chain has been maintained when
        //they are generated, and the moving doesn't change the DOM and DU
        //related properties. Thus there is no need to recompute them here
        //again. For the sake of conventional practic, LICM will verify
        //MDSSAInfo after InsertPreheaderMgr::reviseSSADU().
        xoc::movePhi(m_preheader, guard, m_rg);
        m_licm->getActMgr().dump(
            "move all IR_PHI and MDPhi from preheader BB%u to guard BB%u",
            m_preheader->id(), guard->id());
        m_licm->setLoopHasBeenGuarded(m_li);
        ctx.inserted_guard_bb = true;
        ctx.cfg_changed = true;
        return;
    }
}


//The function will remove the MDPhi that inserted by
//insertPreheader().
//NOTE: The preheader can only contain Phi at most.
void InsertPreheaderMgr::removeInsertedMDPhi()
{
    if (m_preheader == nullptr) { return; }
    MDPhiList const* philist = m_mdssamgr->getPhiList(m_preheader);
    if (philist == nullptr) { return; }
    MDSSAUpdateCtx ctx(*m_oc);
    m_mdssamgr->removePhiList(m_preheader, ctx);
}


void InsertPreheaderMgr::undoCFGChange(OUT HoistCtx & ctx)
{
    if (m_gdhelp.hasInsertedGuard()) {
        //Remove the preheader and guard just inserted.
        IRCfgOptCtx coctx(m_oc);
        ASSERT0(m_preheader == m_gdhelp.getGuardedBB());
        VexIdx preheadervid = m_preheader->id();
        m_gdhelp.removeGuardRegion(*ctx.domtree);
        m_preheader = nullptr; //gdhelper will delete the preheader BB.
        ASSERT0(!m_cfg->isVertex(preheadervid));

        //Useless preheader has been removed.
        ctx.cleanAfterLoop();
        ASSERT0(ctx.verifyDomTree());
        return;
    }
    if (m_preheader != nullptr && m_cfg->isEmptyBB(m_preheader)) {
        ASSERT0(m_preheader->getNumOfIR() == 0);

        //CASE:compiler/licm_undo2.c
        //Do NOT remove the inserted MDPhi here because the Phi might describe
        //a merge operation of multiple DEFs, and it is getting a little
        //complicated to undo the merge operation, moreover leaving the PHI
        //and the empty preheader BB will not much affect the subsequent
        //optimizations.
        //removeInsertedMDPhi();

        //Update DomTree before BB structure removed, because its id is freed.
        ctx.domtree->remove(m_preheader->id());

        //Remove the preheader just inserted.
        IRCfgOptCtx coctx(m_oc);
        RemoveEmptyBBCtx rmctx(coctx);
        rmctx.setForceRemove(true);
        bool res = m_cfg->removeSingleEmptyBB(m_preheader, rmctx);
        ASSERT0_DUMMYUSE(res);

        //removeEmptyBB only maintained these frequently used CFG info.
        OptCtx::setInvalidIfCFGChangedExcept(
            m_oc, PASS_RPO, PASS_DOM, PASS_LOOP_INFO, PASS_UNDEF);
    }
    ASSERT0(ctx.verifyDomTree());
    ctx.cleanAfterLoop();
}


void InsertPreheaderMgr::updateUseDefChainForExpInBB(IRBB const* bb)
{
    BBIRListIter it;
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    IR * prev = nullptr;
    RenameExp rne(m_mdssamgr, m_oc, nullptr);
    for (IR * ir = irlst.get_head(&it); ir != nullptr;
         prev = ir, ir = irlst.get_next(&it)) {
        rne.rename(ir, prev, bb);
    }
}


void InsertPreheaderMgr::updateDefUseAndDefDefChainForStmtInBB(IRBB const* bb)
{
    BBIRListIter it;
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    RecomputeDefDefAndDefUseChain recomp(
        *m_hoistctx.domtree, m_hoistctx.getMDSSAMgr(), *m_oc, m_am);
    for (IR * ir = irlst.get_head(&it);
         ir != nullptr; ir = irlst.get_next(&it)) {
        if (!MDSSAMgr::hasMDSSAInfo(ir)) { continue; }
        recomp.recompute(ir);
    }
}


void InsertPreheaderMgr::updateMDSSADUForExpInBB(IRBB const* bb)
{
    ASSERT0(useMDSSADU());
    ASSERT0(m_mdssamgr);
    ASSERTN(m_oc->is_dom_valid(), ("DOM info must be available"));
    ASSERT0(bb);
    //Note Phi operand in preheader should have been renamed
    //in InsertPhiHelper.
    updateUseDefChainForExpInBB(bb);
    updateDefUseAndDefDefChainForStmtInBB(bb);
}


void InsertPreheaderMgr::updateMDSSADUForExpInGuardStart()
{
    ASSERT0(m_gdhelp.getGuardStart());
    IRBB const* guardstart = m_gdhelp.getGuardStart();

    //NOTE:Since the PRPHI and MDPhi's DU chain has been maintained when
    //they are generated. Thus there is no need to recompute them here
    //again. For the sake of conventional practic, perform verification
    //against MDSSAInfo before the function return.
    updateMDSSADUForExpInBB(guardstart);
}


void InsertPreheaderMgr::updateMDSSADUForExpInPreHeader()
{
    //Note Phi operand in preheader should have been renamed
    //in InsertPhiHelper.
    updateMDSSADUForExpInBB(m_preheader);
}


static void collectAllStprInBB(IRBB const* bb, xcom::TTab<PRNO> & prnotab)
{
    //CASE:compile/rp0_2.c
    //This is an optimization senario to promote the precison of the result
    //of LICM, and reduce the number of PHIs that inserted in LOOPHEAD.
    //         GUARD_START:
    //         falsebr lt j, n, LOOPHEAD;
    //            |            |
    //            |            |
    //GURARD_BB:  V            |
    //stpr $6 = ld s; #S1      |
    //           |             |
    //           V             |
    //         LOOPHEAD:       V
    //         falsebr lt j, $7, LOOPEND;
    //            |
    //            V
    //         BODY:
    //         ist = $6 ...
    //Assuming bb is 'GUARD_BB', the stmt STPR in guarded BB, say #S1,
    //could be moved to guard-start BB, because even if the loop is never
    //execute, the STPR in guard-start BB still has no sideeffect on memory
    //since their results are wrote to PR.
    //Otherwise, a PHI that is related to $6 will have to be inserted in
    //LOOPHEAD.
    BBIRListIter it;
    ConstIRIter tit;
    for (IR const* t = const_cast<IRBB*>(bb)->getIRList().get_head(&it);
         t != nullptr;
         t = const_cast<IRBB*>(bb)->getIRList().get_next(&it)) {
        if (t->isWritePR()) {
            ASSERT0(!t->isCallStmt());
            prnotab.append(t->getPrno());
        }
    }
}


static void collectAllPRInLoopHead(
    IRBB const* loophead, xcom::TTab<PRNO> & prnotab)
{
    BBIRListIter it;
    ConstIRIter tit;
    for (IR const* t = const_cast<IRBB*>(loophead)->getIRList().get_head(&it);
         t != nullptr;
         t = const_cast<IRBB*>(loophead)->getIRList().get_next(&it)) {
        if (t->is_phi()) {
            //Do not consider PHI because it is not real USE of PR. PHI is
            //only used to converge PR value from different predecessors.
            //e.g:licm3.c
            //                 GUARD_START:
            //                 stpr $33 = ...
            //      ___________| |
            //     V             |
            //GUARDED:           |
            //stpr $34 = ... #S1 |
            // |                 |
            // |____             |
            //      |            |
            //      V            V
            //      __________________
            //      LOOPHEAD:
            //      phi $32 = $33, $34
            //      falsebr $9 != 0
            //Even if $34 is referenced by phi, its DEF #S1 should not be move
            //to GUARD_START BB.
            continue;
        }
        tit.clean();
        for (IR const* k = xoc::iterExpInitC(t, tit);
             k != nullptr; k = xoc::iterExpNextC(tit, true)) {
            if (!k->is_pr()) { continue; }
            prnotab.append(k->getPrno());
        }
    }
}


void InsertPreheaderMgr::reviseStmtExecAtLeastOnce()
{
    if (!getInsertGuardHelper().hasInsertedGuard()) { return; }
    IRBB * loophead = m_li->getLoopHead();
    ASSERT0(loophead);
    BBIRListIter it;
    IRBB * guarded_bb = m_gdhelp.getGuardedBB();
    IRBB * guard_start = m_gdhelp.getGuardStart();
    ASSERT0(guarded_bb && guard_start);

    //Record the IR that should be moved to GUARD_START_BB. These IRs have
    //already been placed in GUARDED_BB.
    //e.g:compile/licm_guard.c
    //     -----------------------------------
    //    | GUARD_START_BB:                   |
    //    | falsebr BB_LOOPHEAD gt ld pp, 0x0 |
    //     -----------------------------------
    //        |                  |
    //        V                  |
    //  -----------------------  |
    // | GUARDED_BB:           | |
    // | st q=ld xx         #S1| |
    // | stpr $3=ld q       #S2| |
    // | stpr $4=gt $3 0x0, #S3| |
    //  -----------------------  |
    //        |                  |
    //        V                  V
    //       --------------------------
    //      | LOOPHEAD:                |
    //      | falsebr L2 ne $4 0x0, #S4|
    //       --------------------------
    // #S4 uses $4 which defined by #S3. Meanwhile, #S3 uses $3 which defined
    // by #S2, and #S2 uses 'q' which defined by #S1. Thus #S1, #S2, #S3 all
    // three need to be moved to GUARD_START_BB, where 'xx' is invariant.
    xcom::List<BBIRListIter> shouldbemoved;
    xcom::TTab<PRNO> prnotab;

    //CASE: Once all stprs in 'guarded_bb' are choosed to be moved to
    //'guard_start', there is no need to scan loophead.
    //collectAllPRInLoopHead(loophead, prnotab);
    //Avoid warning caused by -Wunused-function.
    DUMMYUSE(collectAllPRInLoopHead);

    collectAllStprInBB(guarded_bb, prnotab);
    for (guarded_bb->getIRList().get_head(&it);
         it != nullptr; guarded_bb->getIRList().get_next(&it)) {
        IR * ir = it->val();
        if (!ir->isPROp()) { continue; }
        if (!prnotab.find(ir->getPrno())) { continue; }

        //loophead references ir.
        //The result PR will be referenced at least the first access of
        //loophead.
        dumpMovedIRExecOnce(ir, guarded_bb, guard_start, getLICM());
        shouldbemoved.append_tail(it);
    }
    MoveStmtBetweenBB mv2bb(m_rg, m_oc, &getLICM()->getActMgr());
    mv2bb.moveStmtListToTgtBB(shouldbemoved, guarded_bb, guard_start);
}


void InsertPreheaderMgr::updateMDSSADUForLoopHeadPhi(HoistCtx const& ctx)
{
    ASSERT0(useMDSSADU());
    //DOM info must be available.
    ASSERT0(m_oc->is_dom_valid());
    ASSERT0(m_mdssamgr);
    IRBB * loophead = m_li->getLoopHead();
    MDPhiList const* philist = m_mdssamgr->getPhiList(loophead);
    if (philist == nullptr) { return; }
    RecomputeDefDefAndDefUseChain recomp(
        *ctx.domtree, m_mdssamgr, *m_oc, getActMgr());
    recomp.recompute(philist);
    recomp.recomputeDefForPhiOpnd(philist);
}


void InsertPreheaderMgr::reviseSSADU(HoistCtx const& ctx)
{
    //NOTE:Since the PRPHI and MDPhi's DU chain has been maintained when
    //they are generated. Thus there is no need to recompute them here
    //again. For the sake of conventional practic, perform verification
    //against MDSSAInfo before the function return.
    if (m_gdhelp.hasInsertedGuard()) {
        m_gdhelp.insertPhiForGuardedBB(*ctx.domtree, ctx.getS2UMgr());
    }
    ASSERT0(m_oc->is_dom_valid());
    ASSERT0(ctx.verifyDomTree());
    if (m_preheader != nullptr && useMDSSADU()) {
        updateMDSSADUForExpInPreHeader();
        updateMDSSADUForLoopHeadPhi(ctx);
        updateMDSSADUForUseOccOfHoistedStmtInLoopBody(ctx);
    }
    if (m_gdhelp.hasInsertedGuard() && useMDSSADU()) {
        updateMDSSADUForExpInGuardStart();
    }
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, *m_oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, *m_oc));
}


void InsertPreheaderMgr::updateDomTree(DomTree & domtree)
{
    if (m_gdhelp.hasInsertedGuard()) {
        updateDomTreeByGuardRegion(
            m_li, m_gdhelp, domtree, m_licm->getActMgr());
        return;
    }
    if (m_preheader == nullptr) { return; }
    updateDomTreeByPreheader(m_li, m_preheader, domtree, m_licm->getActMgr());
}


bool InsertPreheaderMgr::perform(IRTab const& irtab, MOD HoistCtx & ctx)
{
    //Always insert a preheader to facilitate the insertion of guard-BB.
    m_preheader = nullptr;
    bool insert_prehead = xoc::insertPreheader(
        m_li, m_rg, &m_preheader, m_oc, true);
    ctx.cfg_changed |= insert_prehead;
    if (insert_prehead) {
        m_licm->getActMgr().dump("insert preheader BB%u of LOOP%u",
            m_preheader->id(), m_li->id());
    }
    ASSERT0(!insert_prehead || m_preheader);
    if (!m_oc->is_dom_valid()) {
        m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_DOM, PASS_UNDEF);
    }
    checkAndInsertGuardBB(irtab, ctx);
    updateDomTree(*ctx.domtree);
    ASSERT0(m_oc->is_dom_valid());
    ASSERT0(ctx.verifyDomTree());
    return insert_prehead;
}
//END InsertPreheaderMgr


static void postProcessIfChanged(HoistCtx const& ctx, OptCtx & oc)
{
    //CASE:compile/rp13.c, can not update RPO for some new BB.
    //ASSERT0(oc.is_rpo_valid());
    IRCFG * cfg = ctx.getCFG();
    ASSERT0(cfg->verifyRPO(oc));
    LICM const* licm = ctx.getLICM();
    if (ctx.cfg_changed) {
        //For conservative purpose, we hope to recompute RPO BB list
        //when it is needed.
        cfg->cleanRPOVexList();

        //LOOP, DOM are maintained, but CDG is not.
        ASSERT0(oc.is_dom_valid());
    }
    ASSERT0(cfg->verifyLoopInfo(oc));
    oc.setInvalidPass(PASS_EXPR_TAB);
    if (ctx.duset_changed) {
        oc.setInvalidPass(PASS_LIVE_EXPR);
        oc.setInvalidPass(PASS_AVAIL_REACH_DEF);
        oc.setInvalidPass(PASS_REACH_DEF);
        RCE * rce = ctx.getLICM()->getRCE();
        if (rce != nullptr && rce->is_use_gvn()) {
            rce->getGVN()->set_valid(false);
            //NOTE:If GVN is invalid, LoopDepAna is also invalid.
        }
    }
    cfg->performMiscOpt(oc);
    licm->dump();

    //DU chain and DU ref is maintained.
    ASSERT0(xoc::verifyMDRef(ctx.getRegion(), oc));
    ASSERT0(xoc::verifyClassicDUChain(ctx.getRegion(), oc));
    ASSERT0(cfg->verifyRPO(oc));
    ASSERT0(cfg->verifyDomAndPdom(oc));
    ASSERT0(!ctx.usePRSSADU() ||
            PRSSAMgr::verifyPRSSAInfo(ctx.getRegion(), oc));
    ASSERT0(!ctx.useMDSSADU() ||
            MDSSAMgr::verifyMDSSAInfo(ctx.getRegion(), oc));
}


static void postProcess(HoistCtx const& hoistctx, bool change, OptCtx & oc)
{
    if (change) {
        postProcessIfChanged(hoistctx, oc);
        return;
    }
    Region * rg = hoistctx.getRegion();
    rg->getLogMgr()->cleanBuffer();
    if (!hoistctx.cfg_changed) { return; }

    //CASE:compile/licm_undocfg2.c, needless Label has been added should be
    //remove here. Otherwise the needless label will mess with DCE to
    //return status changed which can not end up ScalarOpt in limit times.
    if (!g_do_cfg_remove_redundant_label) { return; }

    //Note the optimization does not change CFG.
    hoistctx.getCFG()->removeRedundantLabel();
}


class LICMIntl {
public:
    static bool tryHoistStmtOfExpCand(
        LICMAnaCtx const& anactx, OUT IR * cand_exp, OUT IRBB * prehead,
        OUT HoistCtx & ctx)
    {
        IR * cand_stmt = cand_exp->getStmt();
        if (cand_stmt->isStoreStmt() &&
            isImmRHS(cand_exp, cand_stmt) &&
            anactx.isInvStmt(cand_stmt) &&
            xoc::isStmtDomAllUseInsideLoop(
                cand_stmt, anactx.getLI(), ctx.getRegion(), *ctx.getOptCtx())) {
            if (ctx.getLICM()->tryHoistStmt(anactx, cand_stmt, prehead, ctx)) {
                dumpHoistedIR(cand_stmt, prehead, ctx.getLICM());
                return true;
            }
        }
        return false;
    }

    //Return true if BB or STMT changed.
    static bool tryHoistExpCandAndItsStmt(
        LICMAnaCtx const& anactx, OUT IR * cand_exp, OUT IRBB * prehead,
        OUT HoistCtx & ctx)
    {
        ASSERT0(cand_exp->is_exp());
        if (LICMIntl::tryHoistStmtOfExpCand(anactx, cand_exp, prehead, ctx)) {
            return true;
        }
        //Even if hoisting stmt is not success, we still try more for
        //hoisting exp.
        if (cand_exp->is_const()) {
            //CASE1: given
            //  n = 0x10; //S1
            //No need to build STPR in preheader.
            return false;
        }
        dumpHoistedIR(cand_exp, prehead, ctx.getLICM());
        Region * rg = ctx.getRegion();
        IRMgr * irmgr = rg->getIRMgr();
        MDMgr * mdmgr = rg->getMDMgr();

        //CASE2: given
        //  n = cand_exp; //S1
        //Generate new stmt S2, change S1 to S3:
        //  p1 = cand_exp; //S2
        //  n = p1; //S3
        //move S2 into prehead BB.
        IR * t = irmgr->buildPR(cand_exp->getType());
        IR * cand_stmt = cand_exp->getStmt();
        if (cand_stmt->hasJudgeDet() && cand_exp == cand_stmt->getJudgeDet()) {
            bool f = cand_stmt->replaceKid(
                cand_exp, irmgr->buildJudge(t), true);
            ASSERT0_DUMMYUSE(f);
        } else {
            bool f = cand_stmt->replaceKid(cand_exp, t, true);
            ASSERT0_DUMMYUSE(f);
        }
        mdmgr->allocRefForIRTree(t, false);

        //Build a hoisting stmt and put it to preheader BB.
        IR * stpr = rg->dupIsomoStmt(t, cand_exp);

        //Build DU chain between hoist stmt and its USE.
        xoc::buildDUChain(stpr, t, rg, *ctx.getOptCtx());
        moveStmtToPreheader(stpr, prehead, ctx);
        dumpMoveIRToPreheader(stpr, prehead, ctx.getLICM());
        ctx.getLICM()->updateMDSSADUForExpInLoopBody(cand_exp, ctx);
        return true;
    }

    //Return true if find loop invariant expression.
    //Note that finding loop invariant doesn't mean finding hoist candidate.
    //Note the function try to recognize the loop invariant expression and stmt.
    //So far, the function only regard whole RHS IR tree as loop invariant ONLY
    //if all kid IR trees in RHS are loop invariant.
    //TODO: recognize the partial IR tree that is loop invariant.
    static bool scanBB(
        IRBB const* bb, OUT bool * islegal, OUT LICMAnaCtx & anactx)
    {
        bool find = false;
        IRIter irit;
        LICM * licm = anactx.getLICM();
        BBIRListIter it;
        IRBB * pbb = const_cast<IRBB*>(bb);
        for (IR * ir = pbb->getIRList().get_head(&it);
             ir != nullptr; ir = pbb->getIRList().get_next(&it)) {
            if (!licm->isWorthHoist(ir)) { continue; }
            if ((ir->isCallStmt() && !ir->isReadOnly()) || ir->is_region()) {
                //TODO: support call/region.
                //Note PHI has been handled in isLoopInvariantInPRSSA().
                *islegal = false; //prevent loop hoisting.
                return false;
            }
            //stmt can be loop invariant only if whole RHS expressions
            //of the stmt are invariants.
            find |= licm->chooseStmt(ir, irit, anactx);
        }
        return find;
    }

    //Propagate invariant property to result.
    //Return true if some stmts are marked as invariant-stmt.
    //The function aim is to generate as more as invariants.
    //The function will modify m_invariant_stmt, record if the result of
    //stmt become loop invariant.
    //NOTE: Before call this function, user has to guarrantee entire RHS
    //tree of stmt in anactx.m_analysable_stmt_list are loop invariant-exp.
    static bool scanResult(OUT LICMAnaCtx & anactx)
    {
        //CASE:e.g:
        //  for (i=0; i<100; i++) {
        //      g=0;
        //      for (j=0; j<200; j++) {
        //          if (j!=x[i]) { err++; }
        //      }
        //  }
        //where both j=0 and g=0 are hoist candidates. However, only g=0 can
        //be set as an invariant-stmt. The function tracks every Var(MD) that
        //defined in the loop body. If a variable is defined only dependent on
        //other invariant-stmts, then the variable is loop-invariant.
        //Otherwise, if the variable is defined by a loop-induction-op, then
        //it cannot be loop-invariant.
        bool change = false;
        LICM * licm = anactx.getLICM();
        for (IR * stmt = anactx.getAnaStmtList().remove_head(); stmt != nullptr;
             stmt = anactx.getAnaStmtList().remove_head()) {
            ASSERT0(stmt->is_stmt());
            switch (stmt->getCode()) {
            SWITCH_CASE_DIRECT_MEM_STMT:
            SWITCH_CASE_WRITE_PR:
                change |= licm->scanDirectStmt(stmt, anactx);
                break;
            SWITCH_CASE_WRITE_ARRAY:
                change |= licm->scanArrayStmt(stmt, anactx);
                break;
            SWITCH_CASE_INDIRECT_MEM_STMT:
                change |= licm->scanInDirectStmt(stmt, anactx);
                break;
            SWITCH_CASE_CALL:
                change |= licm->scanCallStmt(stmt, anactx);
                break;
            default: ASSERTN(!stmt->isMemRef(), ("TODO"));
            }
        }
        return change;
    }

    //Scan expression to find invariant hoist candidate.
    //Note in order to reduce the complexity of LICM, the function only handle
    //the scenario that whole RHS of stmt is loop-invariant. For cases that
    //anticipating to scan and hoist one of kids in IR tree of RHS, will be
    //handled in Register Promotion.
    //islegal: set to true if loop is legal to perform invariant motion.
    //         otherwise set to false to prohibit code motion.
    //Return true if find loop invariant expression.
    static bool scanLoopBody(OUT bool * islegal, OUT LICMAnaCtx & anactx)
    {
        bool find = false;
        IRCFG const* cfg = anactx.getCFG();
        LI<IRBB> const* li = anactx.getLI();
        IRBB * head = li->getLoopHead();
        ASSERTN(head, ("loopinfo is invalid"));
        UINT headid = head->id();
        for (BSIdx i = li->getBodyBBSet()->get_first();
             i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
            if (i != (BSIdx)headid && !cfg->is_dom(headid, i)) {
                //The BB that doesn't affect loop body will be skipped.
                //Note loop head will take particapate in the computation.
                //The candidate BB must dominate all other loop body BBs.
                continue;
            }
            IRBB const* bb = cfg->getBB(i);
            ASSERT0(bb && bb->getVex());
            find |= scanBB(bb, islegal, anactx);
            if (!(*islegal)) {
                //Whole loop is unsuite to hoist.
                return false;
            }
        }
        return find;
    }
};


//
//START LICM
//
void LICM::clean()
{
    m_irs_mgr.clean();
    m_am.clean();
}


bool LICM::chooseExpList(
    IR * ir, OUT bool & all_exp_invariant, IRIter & irit,
    OUT LICMAnaCtx & anactx)
{
    bool find = false;
    for (IR * x = ir; x != nullptr; x = x->get_next()) {
        IRList * invlist = m_irs_mgr.alloc();
        ASSERT0(x->is_exp());
        bool invariant = false;
        find |= chooseExp(x, irit, &invariant, invlist, anactx);
        if (!invariant) { all_exp_invariant = false; }
        anactx.addHoistCand(*invlist);
        m_irs_mgr.free(invlist);
    }
    return find;
}


//ir: may be exp or stmt.
bool LICM::chooseKid(
    IR * ir, OUT bool & all_kid_invariant, IRIter & irit,
    OUT LICMAnaCtx & anactx)
{
    bool find = false;

    //Multi res list may be NULL.
    IR * multireslist = nullptr;
    if (ir->hasResList()) {
        multireslist = ir->getResList();
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == nullptr) { continue; }
        if (kid == multireslist) {
            //Skip the multi-res-list expression because they are just
            //placeholders.
            continue;
        }
        find |= chooseExpList(kid, all_kid_invariant, irit, anactx);
    }
    if (!all_kid_invariant) {
        return find;
    }
    if (ir->is_stmt() && !anactx.isInvStmt(ir)) {
        //Push stmt into list that to be analyzed.
        //Stmt is invariant only if both base and RHS are invariant.
        ASSERT0(!anactx.getAnaStmtList().find(ir));
        anactx.getAnaStmtList().append_tail(ir);
    }
    return find;
}


//Note if the function invoked, caller has to guarantee all expressions of 'ir'
//are loop invariant.
bool LICM::chooseCallStmt(IR * ir, IRIter & irit, OUT LICMAnaCtx & anactx)
{
    //Hoisting CALL out of loop should generate a guard as well to
    //guarantee CALL will not be exectued if the loop
    //will never execute.
    bool all_arg_invariant = true;
    bool find = chooseExpList(
        CALL_arg_list(ir), all_arg_invariant, irit, anactx);
    if (!all_arg_invariant || !ir->isReadOnly()) {
        //stmt can NOT be loop invariant because some exp is not invariant.
        return find;
    }
    //The callstmt is invariant-stmt.
    if (ir->hasIdinfo() && !isWorthHoist(ir->getIdinfo())) {
        //Do not hoist valueless call even if it is invariant.
        return find;
    }
    if (!anactx.isInvStmt(ir)) {
        //Push stmt into list that to be analyzed.
        //Stmt is invariant only if both base and RHS are invariant.
        ASSERT0(!anactx.getAnaStmtList().find(ir));
        anactx.getAnaStmtList().append_tail(ir);
        anactx.addHoistCand(ir);

        //CASE:Do not regard ir as invariant-stmt immediately here, the
        //subseqent function scanResult() will analyze and confirm that.
        //anactx.addInvStmt(ir);
    }
    return false;
}


bool LICM::isJudgeHoistCand(IR const* ir) const
{
    ASSERT0(ir->is_judge());
    switch (ir->getKidNum()) {
    case 1: {
        IR const* op = UNA_opnd(ir);
        return !(op->isReadPR() || op->is_const());
    }
    case 2: {
        IR const* op0 = BIN_opnd0(ir);
        IR const* op1 = BIN_opnd1(ir);
        bool op0_simp = op0->isReadPR() || op0->is_const();
        bool op1_simp = op1->isReadPR() || op1->is_const();
        return !(op0_simp && op1_simp);
    }
    default: ASSERTN(0, ("not judgement"));
    }
    return false;
}


//Note if the function invoked, caller has to guarantee all expressions of 'ir'
//are loop invariant.
bool LICM::chooseBranch(IR * ir, IRIter & irit, OUT LICMAnaCtx & anactx)
{
    bool exp_invariant;
    IRList * invlist = m_irs_mgr.alloc();
    bool find = chooseExp(
        BR_det(ir), irit, &exp_invariant, invlist, anactx);
    if (exp_invariant && !isJudgeHoistCand(BR_det(ir))) {
        //Do not hoist the simplest format judgement expression.
        //There is no profit for such a code hoisting.
        find = false;
    } else {
        anactx.addHoistCand(*invlist);
    }
    if (exp_invariant) {
        //CASE:Do not regard ir as invariant-stmt immediately here, the
        //subseqent function scanResult() will analyze and confirm that.
        //anactx.addInvStmt(ir);
    }
    ASSERT0(!ir->hasIdinfo());
    m_irs_mgr.free(invlist);
    //TODO:hoist branch if it is loop invariant.
    return find;
}


//Note if the function invoked, caller has to guarantee all expressions of 'ir'
//are loop invariant.
bool LICM::chooseSwitch(IR * ir, IRIter & irit, OUT LICMAnaCtx & anactx)
{
    bool exp_invariant;
    IRList * invlist = m_irs_mgr.alloc();
    bool find = chooseExp(
        SWITCH_vexp(ir), irit, &exp_invariant, invlist, anactx);
    anactx.addHoistCand(*invlist);
    if (exp_invariant) {
        //CASE:Do not regard ir as invariant-stmt immediately here, the
        //subseqent function scanResult() will analyze and confirm that.
        //anactx.addInvStmt(ir);
    }
    ASSERT0(!ir->hasIdinfo());
    m_irs_mgr.free(invlist);
    //TODO:hoist branch if it is loop invariant.
    return find;
}


bool LICM::canBeRegardAsInvExp(IR const* ir, LICMAnaCtx const& anactx) const
{
    ASSERT0(ir);
    switch (ir->getCode()) {
    case IR_CONST:
    case IR_LDA:
    case IR_CASE:
    case IR_ID:
        return true;
    default:
        return anactx.isInvExp(ir);
    }
    return false;
}


//Return true if exp in list is marked and collected into invariant-exp set.
bool LICM::canBeRegardAsInvExpList(
    IR const* explst, LICMAnaCtx const& anactx) const
{
    for (IR const* exp = explst; exp != nullptr; exp = exp->get_next()) {
        ASSERT0(exp->is_exp());
        if (!canBeRegardAsInvExp(exp, anactx)) {
            return false;
        }
    }
    return true;
}


bool LICM::chooseBin(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant, OUT IRList * invlist,
    OUT LICMAnaCtx & anactx)
{
    ASSERT0(!anactx.isInvExp(ir));
    bool find = false;
    bool op0_all_inv = false;
    IRList * invlist0 = m_irs_mgr.alloc();
    find |= chooseExp(BIN_opnd0(ir), irit, &op0_all_inv, invlist0, anactx);

    bool op1_all_inv = false;
    IRList * invlist1 = m_irs_mgr.alloc();
    find |= chooseExp(BIN_opnd1(ir), irit, &op1_all_inv, invlist1, anactx);

    if (op0_all_inv && op1_all_inv) {
        invlist->append_tail(ir);
        *all_exp_invariant = true;
        anactx.addInvExp(ir);
        find = true; //operation can be regarded as invariant.
    } else {
        anactx.addHoistCand(*invlist0);
        anactx.addHoistCand(*invlist1);
    }
    m_irs_mgr.free(invlist0);
    m_irs_mgr.free(invlist1);
    return find;
}


bool LICM::chooseUna(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant, OUT IRList * invlist,
    OUT LICMAnaCtx & anactx)
{
    ASSERT0(!anactx.isInvExp(ir));
    bool op0_all_inv = false;
    IRList * invlist0 = m_irs_mgr.alloc();
    bool find = chooseExp(
        UNA_opnd(ir), irit, &op0_all_inv, invlist0, anactx);
    if (op0_all_inv) {
        invlist->append_tail(ir);
        *all_exp_invariant = true;
        anactx.addInvExp(ir);
        find = true; //operation can be regarded as invariant.
    } else {
        anactx.addHoistCand(*invlist0);
    }
    m_irs_mgr.free(invlist0);
    return find;
}


bool LICM::chooseArray(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant, OUT IRList * invlist,
    OUT LICMAnaCtx & anactx)
{
    ASSERT0(!anactx.isInvExp(ir));
    List<IRList*> tmp;
    bool find = false;
    bool base_inv = false;
    IRList * invlist0 = m_irs_mgr.alloc();
    tmp.append_tail(invlist0);
    find |= chooseExp(ir->getBase(), irit, &base_inv, invlist0, anactx);

    bool all_sub_inv = true;
    for (IR * sub = ARR_sub_list(ir);
         sub != nullptr; sub = sub->get_next()) {
        bool sub_inv = false;
        IRList * invlist1 = m_irs_mgr.alloc();
        find |= chooseExp(sub, irit, &sub_inv, invlist1, anactx);
        tmp.append_tail(invlist1);
        if (!sub_inv) {
            all_sub_inv = false;
        }
    }

    if (base_inv && all_sub_inv) {
        invlist->append_tail(ir);
        *all_exp_invariant = true;
        anactx.addInvExp(ir);
        find = true; //operation can be regarded as invariant.
    } else {
        for (IRList * l = tmp.get_head(); l != nullptr;
             l = tmp.get_next()) {
            anactx.addHoistCand(*l);
        }
    }
    for (IRList * l = tmp.get_head(); l != nullptr;
         l = tmp.get_next()) {
        m_irs_mgr.free(l);
    }
    return find;
}


bool LICM::chooseILD(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant,
    OUT IRList * invlist, OUT LICMAnaCtx & anactx)
{
    ASSERT0(!anactx.isInvExp(ir));
    bool base_inv = false;
    IRList * invlist0 = m_irs_mgr.alloc();
    bool find = chooseExp(ir->getBase(), irit, &base_inv, invlist0, anactx);
    if (base_inv) {
        invlist->append_tail(ir);
        *all_exp_invariant = true;
        anactx.addInvExp(ir);
        find = true; //operation can be regarded as invariant.
    } else {
        anactx.addHoistCand(*invlist0);
    }
    m_irs_mgr.free(invlist0);
    return find;
}


bool LICM::chooseConst(
    IR * ir, OUT bool * all_exp_invariant, OUT LICMAnaCtx & anactx) const
{
    ASSERT0(!anactx.isInvExp(ir));
    anactx.addInvExp(ir);
    *all_exp_invariant = true;

    //ConstExp and LDA should not be regarded as loop hoist candidate,
    //however record it as loop invariant can help to infer more complex
    //invariant expression and stmt.
    //Do not add PR into invlist, because it may be viewed as hoist
    //candidate.
    //e.g: Because 0x100 is invariant, then IST(0x100)=0 is invariant.
    return true;
}


bool LICM::choosePR(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant,
    OUT IRList * invlist, OUT LICMAnaCtx & anactx) const
{
    ASSERT0(!anactx.isInvExp(ir));
    if (xoc::isLoopInvariant(ir, anactx.getLI(), m_rg,
                             &anactx.getInvStmtList(), true,
                             anactx.getOptCtx())) {
        *all_exp_invariant = true;
        anactx.addInvExp(ir);

        //PR should not be regarded as loop hoist candidate, however record
        //it as loop invariant can help to infer more complex invariant
        //expression and stmt.
        //e.g: IST($1)=0, if $1 is loop invariant, the IST($1) is invariant.
        //Do not add PR into invlist, because it may be viewed as hoist
        //candidate.
        //invlist->append_tail(ir);
        return true;
    }
    return false;
}


bool LICM::chooseLD(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant,
    OUT IRList * invlist, OUT LICMAnaCtx & anactx) const
{
    ASSERT0(!anactx.isInvExp(ir));
    ASSERT0(ir->hasIdinfo());
    if (!isWorthHoist(ir->getIdinfo())) {
        *all_exp_invariant = false;
        return false;
    }
    if (xoc::isLoopInvariant(
            ir, anactx.getLI(), m_rg, &anactx.getInvStmtList(), true,
            anactx.getOptCtx())) {
        *all_exp_invariant = true;
        anactx.addInvExp(ir);
        invlist->append_tail(ir);
        return true;
    }
    return false;
}


bool LICM::chooseExtExp(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant, OUT IRList * invlist,
    OUT LICMAnaCtx & anactx)
{
    ASSERT0(ir->is_exp());
    ASSERT0(!anactx.isInvExp(ir));
    if (ir->hasIdinfo() && !isWorthHoist(ir->getIdinfo())) {
        *all_exp_invariant = false;
        return false;
    }
    bool all_kid_invariant = true;
    bool find = chooseKid(ir, all_kid_invariant, irit, anactx);
    if (all_kid_invariant) {
        anactx.addHoistCand(ir);
    }
    return find;
}


bool LICM::chooseSELECT(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant, OUT IRList * invlist,
    OUT LICMAnaCtx & anactx)
{
    ASSERT0(!anactx.isInvExp(ir));
    bool find = false;
    //Trueexp
    bool op0_all_inv = false;
    IRList * invlist0 = m_irs_mgr.alloc();
    if (SELECT_trueexp(ir) != nullptr) {
        find |= chooseExp(
            SELECT_trueexp(ir), irit, &op0_all_inv, invlist0, anactx);
    }

    //Falseexp
    bool op1_all_inv = false;
    IRList * invlist1 = m_irs_mgr.alloc();
    if (SELECT_falseexp(ir) != nullptr) {
        find |= chooseExp(
            SELECT_falseexp(ir), irit, &op1_all_inv, invlist1, anactx);
    }

    //Predexp
    bool op2_all_inv = false;
    IRList * invlist2 = m_irs_mgr.alloc();
    find |= chooseSelectDet(
        SELECT_det(ir), irit, &op2_all_inv, invlist2, anactx);

    if (op0_all_inv && op1_all_inv && op2_all_inv) {
        invlist->append_tail(ir);
        *all_exp_invariant = true;
        anactx.addInvExp(ir);
        find = true; //operation can be regarded as invariant.
    } else {
        anactx.addHoistCand(*invlist0);
        anactx.addHoistCand(*invlist1);
        anactx.addHoistCand(*invlist2);
    }
    m_irs_mgr.free(invlist0);
    m_irs_mgr.free(invlist1);
    m_irs_mgr.free(invlist2);
    return find;
}


//Scan whole IR tree to find loop invariant expression
//and add it into invariant expression list.
//Return true if at least one invariant expression added into list.
//ir: the root IR.
//all_exp_invariant: true if all IR expressions start at 'ir' are
//                   loop invariant.
bool LICM::chooseExp(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant, OUT IRList * invlist,
    OUT LICMAnaCtx & anactx)
{
    ASSERT0(ir);
    ASSERT0(invlist);
    ASSERT0(ir->is_exp());
    *all_exp_invariant = false;
    if (anactx.isInvExp(ir)) {
        *all_exp_invariant = true;
        return false;
    }
    if (!isWorthHoist(ir)) {
        //CASE:compile/licm_volatile.c
        *all_exp_invariant = false;
        return false;
    }
    switch (ir->getCode()) {
    SWITCH_CASE_BIN:
        return chooseBin(ir, irit, all_exp_invariant, invlist, anactx);
    SWITCH_CASE_UNA:
        return chooseUna(ir, irit, all_exp_invariant, invlist, anactx);
    SWITCH_CASE_READ_ARRAY:
        return chooseArray(ir, irit, all_exp_invariant, invlist, anactx);
    SWITCH_CASE_INDIRECT_MEM_EXP:
        return chooseILD(ir, irit, all_exp_invariant, invlist, anactx);
    SWITCH_CASE_READ_PR:
        return choosePR(ir, irit, all_exp_invariant, invlist, anactx);
    SWITCH_CASE_DIRECT_MEM_EXP:
        return chooseLD(ir, irit, all_exp_invariant, invlist, anactx);
    case IR_CONST:
    case IR_LDA:
    case IR_CASE:
        return chooseConst(ir, all_exp_invariant, anactx);
    case IR_SELECT:
        return chooseSELECT(ir, irit, all_exp_invariant, invlist, anactx);
    default: return chooseExtExp(ir, irit, all_exp_invariant, invlist, anactx);
    }
    return false;
}


bool LICM::chooseAssign(IR * ir, IRIter & irit, OUT LICMAnaCtx & anactx)
{
    bool all_kid_invariant = true;
    bool find = chooseKid(ir, all_kid_invariant, irit, anactx);
    if (ir->hasIdinfo() && !isWorthHoist(ir->getIdinfo())) {
        return find;
    }
    if (all_kid_invariant) {
        anactx.addHoistCand(ir);
        //CASE:Do not regard ir as invariant-stmt immediately here, the
        //subseqent function scanResult() will analyze and confirm that.
        //anactx.addInvStmt(ir);
    }
    return find;
}


bool LICM::chooseExtStmt(IR * ir, IRIter & irit, OUT LICMAnaCtx & anactx)
{
    ASSERT0(ir->is_stmt() && ir->isExtOp());
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_WRITE_PR:
    SWITCH_CASE_EXT_DIRECT_MEM_VSTMT:
    SWITCH_CASE_EXT_INDIRECT_MEM_VSTMT:
        //Usually other PR operations is unrewarding to hoist.
        return chooseAssign(ir, irit, anactx);
    default:;
    }
    ASSERTN(0, ("Target Dependent Code"));
    //NOTE: LICM expects user who defined extended stmt could handle the
    //right behaviors when choosing canddidate to get the LICM benefits.
    return false;
}


//The function finds valuable stmt and expression and add it into invariant
//list.
//Note caller has to guarantee that entire RHS IR tree of ir is invariant.
//The function records the stmt in work list to next round analysis as well.
//Return true if a new invariant is added into list.
bool LICM::chooseStmt(IR * ir, IRIter & irit, OUT LICMAnaCtx & anactx)
{
    ASSERT0(ir->is_stmt());
    if (anactx.isInvStmt(ir)) {
        ASSERT0(anactx.verifyInvStmt(ir));
        //Not find new INV.
        return false;
    }
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    case IR_STPR:
    case IR_GETELEM:
    case IR_SETELEM:
        //Usually other PR operations is unrewarding to hoist.
        return chooseAssign(ir, irit, anactx);
    SWITCH_CASE_CALL:
        return chooseCallStmt(ir, irit, anactx);
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        return chooseBranch(ir, irit, anactx);
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
        return chooseSwitch(ir, irit, anactx);
    SWITCH_CASE_UNCONDITIONAL_BRANCH_OP:
        return false;
    case IR_PHI: return false;
    default: chooseExtStmt(ir, irit, anactx);
    }
    return false;
}


//Return true if some stmts are marked as invariant-stmt.
bool LICM::scanDirectStmt(IR * stmt, LICMAnaCtx & anactx)
{
    ASSERT0(stmt->isDirectMemOp() || stmt->isPROp());
    if (!stmt->hasRHS()) { return false; }
    if (stmt->getRHS() == nullptr) {
        //Virtual OP may not have RHS.
        //Return true to inform caller function 'stmt' can be treated
        //as invariant stmt.
        anactx.addInvStmt(stmt);
        return true;
    }
    ASSERT0(canBeRegardAsInvExp(stmt->getRHS(), anactx));
    if (anactx.isInvStmt(stmt)) { return false; }

    //NOTE:According to the definition of invariant-stmt, stmt can be
    //regarded as invariant-stmt. However, LICM has to consider the relation
    //between the stmt and other stmt in the Def-Def chain.
    //Here We just leave the work to tryHoistDependentStmt().
    anactx.addInvStmt(stmt);
    return true;
}


//Return true if some stmts are marked as invariant-stmt.
bool LICM::scanArrayStmt(IR * stmt, LICMAnaCtx & anactx)
{
    ASSERT0(stmt->isArrayOp());
    ASSERT0(canBeRegardAsInvExp(stmt->getRHS(), anactx));
    if (anactx.isInvStmt(stmt)) { return false; }
    if (stmt->getEffectRef() != nullptr && anactx.isUniqueDef(stmt)) {
        anactx.addInvStmt(stmt);
        return true;
    }
    if (!canBeRegardAsInvExp(ARR_base(stmt), anactx)) {
        return false;
    }
    for (IR * subexp = ARR_sub_list(stmt);
         subexp != nullptr; subexp = subexp->get_next()) {
        if (!canBeRegardAsInvExp(subexp, anactx)) {
            return false;
        }
    }
    //NOTE:According to the definition of invariant-stmt, stmt can be
    //regarded as invariant-stmt. However, LICM has to consider the relation
    //between the stmt and other stmt in the Def-Def chain.
    //Here We just leave the work to tryHoistDependentStmt().
    anactx.addInvStmt(stmt);
    return true;
}


//Return true if some stmts are marked as invariant-stmt.
bool LICM::scanInDirectStmt(IR * stmt, OUT LICMAnaCtx & anactx)
{
    ASSERT0(stmt->isIndirectMemOp());
    ASSERT0(canBeRegardAsInvExp(stmt->getRHS(), anactx));
    if (anactx.isInvStmt(stmt)) { return false; }
    if (stmt->getEffectRef() != nullptr && anactx.isUniqueDef(stmt)) {
        anactx.addInvStmt(stmt);
        return true;
    }
    if (!canBeRegardAsInvExp(stmt->getBase(), anactx)) {
        return false;
    }
    //NOTE:According to the definition of invariant-stmt, stmt can be
    //regarded as invariant-stmt. However, LICM has to consider the relation
    //between the stmt and other stmt in the Def-Def chain.
    //Here We just leave the work to tryHoistDependentStmt().
    anactx.addInvStmt(stmt);
    return true;
}


bool LICM::scanCallStmt(IR * stmt, OUT LICMAnaCtx & anactx)
{
    ASSERT0(canBeRegardAsInvExpList(CALL_arg_list(stmt), anactx));
    if (anactx.isInvStmt(stmt)) { return false; }
    if ((!stmt->hasReturnValue() || anactx.isUniqueDef(stmt)) &&
        stmt->isReadOnly()) {
        anactx.addInvStmt(stmt);
        return true;
    }
    return false;
}


bool LICM::analyszInvariantOp(OUT LICMAnaCtx & anactx)
{
    anactx.getMD2DefCnt().compute();
    bool change = true;
    //True if finding loop invariant exp or stmt.
    //Note that finding loop invariant does not mean finding hoist candidate.
    bool find = false;
    while (change) {
        bool islegal = true;
        change = LICMIntl::scanLoopBody(&islegal, anactx);
        if (!islegal) {
            find = false;
            break;
        }
        if (change) {
            find = true;
            //anactx.getAnaStmtList() will be empty when function return.
            LICMIntl::scanResult(anactx);

            //Before next round analysis, we must make sure all
            //stmts in this list is invariant or not.
            ASSERT0(anactx.getAnaStmtList().get_elem_count() == 0);
        }
    }
    if (!find) { anactx.getCandTab().clean(); }

    //Dump invariant info right here because they will be changed soon.
    dumpAnaCtx(this, anactx);
    return find;
}


//Return true if any stmt that is related to invariant stmt
//is moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
bool LICM::hoistDefByPRSSA(
    LICMAnaCtx const& anactx, IR const* exp, OUT IRBB * prehead,
    MOD HoistCtx & ctx)
{
    SSAInfo * info = exp->getSSAInfo();
    ASSERTN(info, ("miss PRSSAInfo"));
    //Check if SSA def is loop invariant.
    IR * def = SSA_def(info);
    if (def == nullptr) { return true; }
    if (def->is_phi()) { return false; }
    if (!anactx.getLI()->isInsideLoop(def->getBB()->id())) {
        return true;
    }
    if (!isWorthHoist(def) ||
        !tryHoistDefStmt(anactx, def, prehead, ctx)) {
        return false;
    }
    dumpHoistedIR(def, prehead, this);
    return true;
}


//Return true if any stmt that is related to invariant stmt
//is moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
bool LICM::hoistDefByClassicDU(
    LICMAnaCtx const& anactx, IR const* exp, OUT IRBB * prehead,
    MOD HoistCtx & ctx)
{
    ASSERTN(m_rg->getDUMgr(), ("valid DUChain need DUMgr"));
    DUSet const* defset = exp->readDUSet();
    if (defset == nullptr) { return true; }
    DUSetIter di = nullptr;
    BSIdx nexti;
    IRBB const* expbb = exp->getStmt()->getBB();
    ASSERT0(expbb);
    for (BSIdx i = defset->get_first(&di); i != BS_UNDEF; i = nexti) {
        nexti = defset->get_next(i, &di);
        IR * def = m_rg->getIR(i);
        ASSERT0(def);
        IRBB const* defbb = def->getBB();
        ASSERT0(defbb);
        if (defbb != expbb && m_cfg->is_dom(defbb->id(), expbb->id())) {
            ; //def's BB dominates exp.
        } else if (defbb == expbb && defbb->is_dom(def, exp->getStmt(), true)) {
            ; //def dominates exp.
        } else {
            //Can not hoist non-dominated DEF.
            //CASE:compile/cfg4.c
            return false;
        }
        if (!isWorthHoist(def) ||
            !tryHoistDefStmt(anactx, def, prehead, ctx)) {
            return false;
        }
        dumpHoistedIR(def, prehead, this);
    }
    return true;
}


//Return true if all DEF stmt/phi that is related to invariant stmt
//are moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
//Note some DEF that has been hoisted by this function is recorded in 'ctx'
//even not all of DEF hoisted totally.
bool LICM::hoistDefByMDSSA(
    LICMAnaCtx const& anactx, IR const* exp, OUT IRBB * prehead,
    MOD HoistCtx & ctx)
{
    ASSERT0(ctx.useMDSSADU());
    MDSSAInfo * info = ctx.getMDSSAMgr()->getMDSSAInfoIfAny(exp);
    ASSERTN(info, ("def stmt even not in MDSSA system"));
    VOpndSetIter it = nullptr;
    VOpndSet const& vopndset = info->readVOpndSet();
    UseDefMgr const* udmgr = ctx.getMDSSAMgr()->getUseDefMgr();
    BSIdx nexti;
    for (BSIdx i = vopndset.get_first(&it); i != BS_UNDEF; i = nexti) {
        nexti = vopndset.get_next(i, &it);
        VMD const* vmd = (VMD*)udmgr->getVOpnd(i);
        if (vmd == nullptr) {
            //CASE:licm_mdssa2.c
            //current i may have been removed by tryHoistDefStmt at last
            //round of the iteration.
            continue;
        }
        ASSERT0(vmd->is_md());
        MDDef const* def = vmd->getDef();
        if (def == nullptr) {
            //Region livein PR.
            continue;
        }
        ASSERT0(def->getBB());
        if (!anactx.getLI()->isInsideLoop(def->getBB()->id())) {
            //Outside loop def.
            continue;
        }
        if (def->is_phi()) {
            //CASE:Even if exp has been judged and viewed as
            //loop-invariant expression that ready to hoist, it may overlapped
            //with other STMT in the loop, e.g:licm_hoist_inexact2.c
            //Thus here still need to judge whether it has DU relation with
            //other STMTs inside loop. In SSA mode, PHI represents the DU
            //relation between 'exp' and other potential STMT.
            //However, even if 'exp' can not be hoisted because of DU, its kid
            //expressions have the opportunity to hoist.
            if (ctx.getMDSSAMgr()->isOverConservativeDUChain(def, exp)) {
                continue;
            }
            return false;
        }
        IR * stmt = def->getOcc();
        if (!isWorthHoist(stmt) ||
            !tryHoistDefStmt(anactx, stmt, prehead, ctx)) {
            return false;
        }
        //Note after stmt hoisted, vopndset may changed.
        if (!vopndset.is_contain(nexti)) {
            //CASE:rp3.c
            i = vopndset.get_first(&it);
        }
        dumpHoistedIR(stmt, prehead, this);
    }
    return true;
}


//Return true if any stmt that is related to invariant stmt
//is moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
bool LICM::hoistDefByDUChain(
    LICMAnaCtx const& anactx, IR const* exp, OUT IRBB * prehead,
    MOD HoistCtx & ctx)
{
    ASSERT0(exp->is_exp());
    if (!exp->isMemOpnd()) { return true; }
    if (exp->isPROp() && ctx.usePRSSADU()) {
        return hoistDefByPRSSA(anactx, exp, prehead, ctx);
    }
    if (exp->isMemRefNonPR() && ctx.useMDSSADU()) {
        return hoistDefByMDSSA(anactx, exp, prehead, ctx);
    }
    if (ctx.getOptCtx()->is_pr_du_chain_valid() ||
        ctx.getOptCtx()->is_nonpr_du_chain_valid()) {
        return hoistDefByClassicDU(anactx, exp, prehead, ctx);
    }
    return false;
}


static bool hasMultiDefInLoopByMDSSA(
    LICMAnaCtx const& anactx, IR const* stmt, HoistCtx const& ctx)
{
    ASSERT0(ctx.useMDSSADU());
    if (!MDSSAMgr::hasMDSSAInfo(stmt)) { return false; }
    MDSSAMgr const* mgr = ctx.getMDSSAMgr();
    OptCtx const* oc = ctx.getOptCtx();
    Region const* rg = ctx.getRegion();
    MDSSAInfo const* info = mgr->getMDSSAInfoIfAny(stmt);
    ASSERT0(info);
    VOpndSetIter it = nullptr;
    VOpndSet const& vopndset = info->readVOpndSet();
    UseDefMgr const* udmgr = const_cast<MDSSAMgr*>(mgr)->getUseDefMgr();
    MDSystem const* mdsys = mgr->getMDSystem();
    bool costly_analysis = true;
    for (BSIdx i = vopndset.get_first(&it); i != BS_UNDEF;
         i = vopndset.get_next(i, &it)) {
        VMD const* vmd = (VMD*)udmgr->getVOpnd(i);
        if (vmd == nullptr) {
            //CASE:licm_mdssa2.c
            //current i may have been removed by moveStmtToTgtBB() at last
            //round of the iteration.
            continue;
        }
        MDDef const* vmddef = vmd->getDef();
        LICMConstNextMDDefIter it(anactx, mgr, oc, true, stmt);
        MDDef const* nd = it.get_first(vmddef);
        ASSERT0(nd == vmddef);
        for (nd = it.get_next(); nd != nullptr; nd = it.get_next()) {
            if (nd->is_phi()) { continue; }
            IR const* ndocc = nd->getOcc();
            if (!xoc::isDependent(stmt, ndocc, costly_analysis, rg)) {
                //stmt and ndocc are unrelated.
                continue;
            }
            //stmt and ndocc may be overlapped.
            dumpMultiDef(ctx.getLICM(), stmt, ndocc);
            return true;
        }
    }
    return false;
}


static bool hasMultiDefInLoop(
    LICMAnaCtx const& anactx, IR const* stmt, HoistCtx const& ctx)
{
    if (stmt->getMustRef() != nullptr && anactx.isUniqueDef(stmt)) {
        return false;
    }
    if (ctx.useMDSSADU()) {
        return hasMultiDefInLoopByMDSSA(anactx, stmt, ctx);
    }
    //If there are multiple-defines to stmt's MD reference, the case
    //will be complicate.
    //e.g:
    //  LOOP{
    //    S1: g=1
    //    S2: ...=g
    //    S3: ...=g+i
    //    S4: g=2
    //    S5: i=i+1;
    //  }
    //Where S1 is invariant-stmt, however it can't be hoisted,
    //because S3 can't be hoisted for the reason of variable i.
    return true;
}


//Try hoisting the dependent stmt to 'stmt' firstly.
//Return true if all dependent stmts have been hoisted outside of loop.
bool LICM::tryHoistDependentStmt(
    LICMAnaCtx const& anactx, MOD IR * stmt, MOD IRBB * prehead,
    OUT HoistCtx & ctx)
{
    //cand is store-value and the result memory object is ID|PR.
    //NOTE: If we hoist entire stmt out from loop,
    //we should make sure the stmt will be execute at least once
    //or never. Conditional branch should be generated and encapsulate
    //the hoisted stmt to ensure that.
    //    while (a > 0) {
    //        a = 10;
    //        foo();
    //    }
    //    =>
    //    if (a > 0)  {
    //        a = 10;
    //    }
    //    while (a > 0) {
    //        foo();
    //    }
    ASSERT0(stmt->getBB());
    if (!isHoistStmt()) { return false; }
    ConstIRIter iriter;
    for (IR const* x = xoc::iterExpInitC(stmt, iriter);
         x != nullptr; x = xoc::iterExpNextC(iriter, true)) {
        if (!hoistDefByDUChain(anactx, x, prehead, ctx)) {
            //stmt can not be hoisted.
            return false;
        }
    }
    if (hasMultiDefInLoop(anactx, stmt, ctx)) {
        //For conventional practice, stmt will not be hoisted.
        return false;
    }
    if (ctx.useMDSSADU()) {
        return canHoistStmtByMDSSA(anactx, stmt, ctx);
    }
    return true; //stmt can be hoisted to preheader.
}


//Return true if stmt's MD ref can be covered by next-MDDef.
//Return false if we can't give out any conclusions.
static bool canBeCoverByNextDef(
    LICMAnaCtx const& anactx, IR const* stmt, HoistCtx const& ctx)
{
    ASSERT0(MDSSAMgr::hasMDSSAInfo(stmt));
    MDSSAMgr const* mgr = ctx.getMDSSAMgr();
    OptCtx const* oc = ctx.getOptCtx();
    Region const* rg = ctx.getRegion();
    MDSSAInfo const* info = mgr->getMDSSAInfoIfAny(stmt);
    ASSERT0(info);
    VOpndSetIter it = nullptr;
    VOpndSet const& vopndset = info->readVOpndSet();
    UseDefMgr const* udmgr = const_cast<MDSSAMgr*>(mgr)->getUseDefMgr();
    MDSystem const* mdsys = mgr->getMDSystem();
    bool costly_analysis = true;
    for (BSIdx i = vopndset.get_first(&it); i != BS_UNDEF;
         i = vopndset.get_next(i, &it)) {
        VMD const* vmd = (VMD*)udmgr->getVOpnd(i);
        if (vmd == nullptr) {
            //CASE:licm_mdssa2.c
            //current i may have been removed by moveStmtToTgtBB() at last
            //round of the iteration.
            continue;
        }
        MDDef const* vmddef = vmd->getDef();
        LICMConstNextMDDefIter it(anactx, mgr, oc, true, stmt);
        MDDef const* nd = it.get_first(vmddef);
        ASSERT0(nd == vmddef);
        for (nd = it.get_next(); nd != nullptr; nd = it.get_next()) {
            if (nd->is_phi()) {
                //We don't know the exact results.
                return false;
            }
            IR const* ndocc = nd->getOcc();
            if (!xoc::isDependent(stmt, ndocc, costly_analysis, rg)) {
                //stmt and ndocc are unrelated.
                continue;
            }
            if (xoc::isExactEqual(ndocc, stmt, nullptr, oc)) {
                //The value defined by stmt can NOT cross ndocc.
                continue;
            }
            //We don't know whether stmt can be covered.
            return false;
        }
    }
    return true;
}


bool LICM::canHoistStmtByMDSSA(
    LICMAnaCtx const& anactx, IR const* stmt, HoistCtx const& ctx) const
{
    ASSERT0(ctx.useMDSSADU());
    if (!MDSSAMgr::hasMDSSAInfo(stmt)) { return true; }
    bool cross_nonphi_def = false;
    ctx.getMDSSAMgr()->isCrossLoopHeadPhi(
        stmt, anactx.getLI(), cross_nonphi_def);
    if (cross_nonphi_def) {
        //Illegal hoisting that violate prev-def.
        return false;
    }
    //CASE:compile.gr/licm_dd.gr
    //If there exist multiples DEF of same MD in loop, for clarity,
    //we prefer to handle this case in DSE rather than LICM.
    //In DSE, the redundant DEFs will be eliminated firstly, thus LICM does not
    //need to account for the scenario.
    //if (!canBeCoverByNextDef(anactx, stmt, ctx)) {
    //    //The value defined by stmt might be used by the USE of its next DEF
    //    //in Def-Def chain.
    //    return false;
    //}
    DUMMYUSE(canBeCoverByNextDef); //avoid warning.

    //The hoist don't modify classic DU chain of 'cand_exp' and
    //'stmt'. Thus there is no need to revise classic DU chain.
    //But the live-expr, reach-def, avail-reach-def set
    //info of each BB changed.
    //However, MDSSA DU chain changed, maintain MDSSA DU chain if
    //doLoopTree is avaiable.
    return true; //stmt can be hoisted to preheader.
}


void LICM::updateMDSSADUForExpInLoopBody(MOD IR * exp, HoistCtx const& ctx)
{
    if (!ctx.useMDSSADU()) { return; }
    ASSERT0(exp && exp->is_exp());
    ASSERT0(ctx.getOptCtx()->is_dom_valid());
    IR * stmt = exp->getStmt();
    ASSERT0(stmt);
    ASSERT0(stmt->getBB());
    IR * startir = stmt->getBB()->getPrevIR(stmt);
    //If startir is NULL, that means 'stmt' is the first IR of BB.
    IRIter it;
    MDSSAStatus st;
    FindAndSetLiveInDef fs(ctx.getMDSSAMgr(), *ctx.getOptCtx());
    for (IR * x = xoc::iterInit(exp, it, false);
         x != nullptr; x = xoc::iterNext(it, true)) {
        if (!MDSSAMgr::hasMDSSAInfo(x)) { continue; }
        fs.findAndSet(x, startir, stmt->getBB(), st);
    }
    ASSERT0(st.is_succ());
}


//NOTE:The necessary Phi and MDPhi should have been inserted into guarded BB
//before call this function.
void InsertPreheaderMgr::updateMDSSADUForUseOccOfHoistedStmtInLoopBody(
    HoistCtx const& ctx)
{
    Region * rg = getRegion();
    Stmt2UseMgr::IR2IRSetIter it;
    IRSet * iset;
    Stmt2UseMgr const& s2umgr = ctx.getS2UMgr();
    RenameExp rne(ctx.getMDSSAMgr(), ctx.getOptCtx(), nullptr);
    for (IR const* ir = s2umgr.getStmt2UseSet().get_first(it, &iset);
         ir != nullptr; ir = s2umgr.getStmt2UseSet().get_next(it, &iset)) {
        ASSERT0(iset);
        ASSERT0(ir->is_stmt());
        if (!MDSSAMgr::hasMDSSAInfo(ir)) { continue; }
        if (getLI()->isInsideLoop(ir->getBB()->id())) {
            //ir hasn't been hoisted at all. Thus there is no need to recompute
            //its MDSSADU info.
            continue;
        }
        IRSetIter isetit;
        for (BSIdx i = iset->get_first(&isetit);
             i != BS_UNDEF; i = iset->get_next(i, &isetit)) {
            IR * useocc = rg->getIR(i);
            ASSERT0(useocc);
            if (!MDSSAMgr::hasMDSSAInfo(useocc)) {
                //If ir is CallStmt, its Use OCC may contain PR.
                continue;
            }
            IRBB const* useoccbb = xoc::MDSSAMgr::getExpBB(useocc);
            ASSERT0(useoccbb);
            if (!getLI()->isInsideLoop(useoccbb->id())) {
                //The Use OCC has been hoisted out of loop.
                continue;
            }
            //Since useocc's DEF has been hoisted out of the loop, renaming
            //begins at the first stmt of the BB containing useocc.
            rne.rename(useocc);
        }
    }
}


void LICM::cleanMDSSADUForStmt(MOD IR * stmt, HoistCtx const& ctx)
{
    if (MDSSAMgr::getMDSSAInfoIfAny(stmt) == nullptr) { return; }
    //There is no MDSSAInfo if 'stmt' is just generated.
    //Remove old MDSSAInfo, cutoff DefDef chain and DefUse chain before
    //reviseSSADU.
    MDSSAUpdateCtx updatectx(*const_cast<OptCtx*>(ctx.getOptCtx()));
    ctx.getMDSSAMgr()->removeStmtMDSSAInfo(stmt, updatectx);
}


void LICM::updateMDSSADUForStmtInLoopBody(MOD IR * stmt, HoistCtx const& ctx)
{
    if (!ctx.useMDSSADU()) { return; }
    ASSERT0(stmt && stmt->is_stmt());
    ASSERT0(ctx.getOptCtx()->is_dom_valid());
    if (MDSSAMgr::hasMDSSAInfo(stmt)) {
        //Firstly, right after stmt has been moved, update the stmt's MDSSAInfo
        //which include DefDef chain and DefUse chain.
        RecomputeDefDefAndDefUseChain recomp(
            *ctx.domtree, ctx.getMDSSAMgr(), *ctx.getOptCtx(), &getActMgr());
        recomp.recompute(stmt);
    }
    //Sencondly, update the DefUse chain of each expressions of stmt.
    //Note the update will be looking for the correct DEF for each exp of
    //stmt. And the looking process should start from the previous IR of
    //'stmt'.
    IR * startir = stmt->getBB()->getPrevIR(stmt);
    //If startir is NULL, that means 'stmt' is the first IR of BB.
    IRIter it;
    MDSSAStatus st;
    FindAndSetLiveInDef fs(ctx.getMDSSAMgr(), *ctx.getOptCtx());
    for (IR * x = xoc::iterExpInit(stmt, it);
         x != nullptr; x = xoc::iterExpNext(it, true)) {
        if (!MDSSAMgr::hasMDSSAInfo(x)) { continue; }
        fs.findAndSet(x, startir, stmt->getBB(), st);
    }
    ASSERT0(st.is_succ());
}


//Return true if stmt is successfully moved outside of loop.
bool LICM::tryHoistStmt(
    LICMAnaCtx const& anactx, MOD IR * stmt, MOD IRBB * prehead,
    OUT HoistCtx & ctx)
{
    if (!tryHoistDependentStmt(anactx, stmt, prehead, ctx)) {
        return false;
    }
    FindRedOpResult res = xoc::findRedOpInLoop(anactx.getLI(), stmt, m_rg);
    if (res != OP_IS_NOT_RED) {
        //stmt may be reduce-op or the function has no knowledge about it.
        //For conservative purpose, the function should NOT hoist stmt.
        return false;
    }
    //Record the UseSet of stmt before moving it to preheader BB, because
    //the InsertGuardHelper will try to insert Phi at loophead BB according to
    //this information.
    ctx.recordUseSet(stmt);
    moveStmtToPreheader(stmt, prehead, ctx);
    dumpMoveIRToPreheader(stmt, prehead, ctx.getLICM());

    //Clean MDSSADU info here to avoid confusing subsequent
    //RecomputeDefDefAndDefUseChain's recomputation.
    cleanMDSSADUForStmt(stmt, ctx);
    return true;
}


//Return true if any stmt is moved outside from loop.
bool LICM::tryHoistDefStmt(
    LICMAnaCtx const& anactx, MOD IR * def, MOD IRBB * prehead,
    MOD HoistCtx & ctx)
{
    ASSERT0(def->is_stmt());
    if (!anactx.getLI()->isInsideLoop(def->getBB()->id())) {
        //The DEF has already moved outside loop.
        return true;
    }
    if (!anactx.isInvStmt(def)) { return false; }
    if (!anactx.getLI()->atLeastExecOnce(def->getBB()->id(), m_cfg)) {
        //CASE:Do not hoist the stmt that might be executed conditionally.
        //  for (i) {
        //    if (i > 100) {
        //      g=0; #may not execute.
        //    }
        //  }
        return false;
    }
    return tryHoistStmt(anactx, def, prehead, ctx);
}


//Return true if gurard BB of LOOP 'li' has been inserted.
void LICM::setLoopHasBeenGuarded(LI<IRBB> const* li)
{
    m_insert_guard_bb.append(li);
}


//Return true if gurard BB of LOOP 'li' has been inserted.
bool LICM::hasInsertedGuardBB(LI<IRBB> const* li) const
{
    return m_insert_guard_bb.get(li) != nullptr;
}


//Try to move and check that each definitions of candidate has been
//already hoisted from loop.
//Return true if all DEF stmt of 'c' has been hoisted.
bool LICM::tryHoistAllDefStmt(
    LICMAnaCtx const& anactx, IR const* c, IRBB * prehead, MOD HoistCtx & ctx)
{
    ConstIRIter irit;
    for (IR const* x = iterInitC(c, irit); x != nullptr; x = iterNextC(irit)) {
        if (!hoistDefByDUChain(anactx, x, prehead, ctx)) {
            //x's DEF can not be hoisted.
            return false;
        }
    }
    return true;
}


bool LICM::hoistStmtCandInCandTab(
    MOD LICMAnaCtx & anactx, OUT IRBB * prehead,
    OUT xcom::Vector<IR*> & removed, IRTabIter & it, OUT HoistCtx & ctx)
{
    bool changed = false;
    it.clean();
    for (IR * c = anactx.getConstCandTab().get_first(it);
         c != nullptr; c = anactx.getConstCandTab().get_next(it)) {
        if (c->is_exp()) {
            //The function only process stmt hoisting.
            continue;
        }
        if (!isWorthHoist(c)) {
            removed.append(c);
            continue;
        }
        HoistCtx lctx(ctx);
        bool def_hoisted = tryHoistDefStmt(anactx, c, prehead, lctx);

        //Record the status if some DEF has been hoisted.
        changed |= lctx.stmt_changed;
        ctx.unionBottomUpInfo(lctx);
        if (!def_hoisted) {
            continue;
        }
        removed.append(c);
    }
    return changed;
}


//Return true if BB or STMT changed.
bool LICM::hoistStmtCand(
    MOD LICMAnaCtx & anactx, OUT IRBB * prehead, OUT HoistCtx & ctx)
{
    xcom::Vector<IR*> removed;
    IRTabIter it;
    bool changed = false;
    while (anactx.getConstCandTab().get_elem_count() > 0) {
        removed.clean();
        changed |= hoistStmtCandInCandTab(anactx, prehead, removed, it, ctx);
        if (removed.get_elem_count() == 0) {
            //No candicate saftified the hoisting condition.
            return changed;
        }
        for (UINT i = 0; i < removed.get_elem_count(); i++) {
            IR * c = removed.get(i);
            ASSERT0(c);
            anactx.getCandTab().remove(c);
        }
    }
    return changed;
}


void LICM::tryPickKidInvExp(
    LICMAnaCtx const& anactx, IR * c, OUT xcom::Vector<IR*> & tryagain) const
{
    switch (c->getCode()) {
    SWITCH_CASE_INDIRECT_MEM_OP: {
        IR * base = c->getBase();
        if (anactx.isInvExp(base) && !anactx.getConstCandTab().find(base)) {
            tryagain.append(base);
        }
        return;
    }
    SWITCH_CASE_ARRAY_OP: {
        //Add base.
        IR * base = c->getBase();
        if (anactx.isInvExp(base) && !anactx.getConstCandTab().find(base)) {
            tryagain.append(base);
        }

        //Add sub-exp.
        for (IR * t = ARR_sub_list(c); t != nullptr; t = t->get_next()) {
            if (anactx.isInvExp(t) && !anactx.getConstCandTab().find(t)) {
                tryagain.append(t);
            }
        }
        return;
    }
    default:;
    }
}


//The function not only hoists each candidates in given ConstCandTab, but also
//hoists all dependent stmts of these candidates.
bool LICM::hoistExpCandInCandTab(
    MOD LICMAnaCtx & anactx, OUT IRBB * prehead,
    OUT xcom::Vector<IR*> & tryagain, OUT xcom::Vector<IR*> & hoisted,
    IRTabIter & it, OUT HoistCtx & ctx)
{
    it.clean();
    bool changed = false;
    for (IR * c = anactx.getConstCandTab().get_first(it);
         c != nullptr; c = anactx.getConstCandTab().get_next(it)) {
        if (c->is_stmt()) {
            //The function only process expression hoisting.
            continue;
        }
        if (!isWorthHoist(c)) {
            hoisted.append(c);
            continue;
        }
        if (!anactx.getLI()->isInsideLoop(c->getStmt()->getBB()->id())) {
            //Candidate expression has been moved to preheader.
            //e.g:stpr $1 = add (ld gp, 0x1);  //S1
            //    st m = ild $1;  //S2
            //Both 'add' and 'ild' are cand-expression.
            //First, we choose moving S2 to preheader first.
            //Whereas according to the dependence
            //relation of $1, the DEF STMT of $1 will be moved to
            //preheader, namely, S1.
            //Thus when next iteration, we are going to moving cand-exp
            //'add', we find it has been moved to preheader.
            continue;
        }
        HoistCtx lctx(ctx);
        bool all_defs_hoisted = tryHoistAllDefStmt(anactx, c, prehead, lctx);
        //Record the status if some DEF has been hoisted.
        changed |= lctx.stmt_changed;
        ctx.unionBottomUpInfo(lctx);
        if (!all_defs_hoisted) {
            tryPickKidInvExp(anactx, c, tryagain);
            continue;
        }
        bool hoist_succ = LICMIntl::tryHoistExpCandAndItsStmt(
            anactx, c, prehead, ctx);
        changed |= hoist_succ;
        if (!hoist_succ) {
            tryPickKidInvExp(anactx, c, tryagain);
            continue;
        }
        hoisted.append(c);
    }
    return changed;
}


//Return true if BB or STMT changed.
bool LICM::hoistExpCand(
    MOD LICMAnaCtx & anactx, OUT IRBB * prehead, OUT HoistCtx & ctx)
{
    xcom::Vector<IR*> hoisted;
    xcom::Vector<IR*> tryagain;
    IRTabIter it;
    bool changed = false;
    while (anactx.getConstCandTab().get_elem_count() > 0) {
        hoisted.clean();

        //Record the new generated cand to try to hoist.
        tryagain.clean();
        changed |= hoistExpCandInCandTab(
            anactx, prehead, tryagain, hoisted, it, ctx);
        if (hoisted.get_elem_count() == 0) {
            //No candicate saftified the hoisting condition.
            return changed;
        }
        for (UINT i = 0; i < hoisted.get_elem_count(); i++) {
            IR * r = hoisted.get(i);
            ASSERT0(r);
            ASSERTN(r->is_exp(), ("the function only hoist expression"));
            anactx.getCandTab().remove(r);
        }
        for (UINT i = 0; i < tryagain.get_elem_count(); i++) {
            IR * t = tryagain.get(i);
            ASSERT0(t);
            ASSERTN(t->is_exp(),
                    ("the function only handle expression, even if try-again"));
            ASSERT0(!anactx.getCandTab().find(t));
            anactx.getCandTab().append(t);
        }
    }
    return changed;
}


//Hoist candidate IRs to preheader BB.
//This function will maintain RPO if new BB inserted.
//Return true if BB or STMT changed.
bool LICM::hoistCand(
    MOD LICMAnaCtx & anactx, OUT IRBB * prehead, OUT HoistCtx & ctx)
{
    bool changed = hoistExpCand(anactx, prehead, ctx);
    changed |= hoistStmtCand(anactx, prehead, ctx);
    return changed;
}


bool LICM::processLoop(LI<IRBB> * li, HoistCtx & ctx)
{
    ASSERT0(ctx.getOptCtx()->is_loopinfo_valid());

    //First of all, do analysis that collects all hoisting candidiates.
    LICMAnaCtx anactx(m_rg, li, this, ctx.getOptCtx());
    if (!analyszInvariantOp(anactx)) { return false; }
    if (anactx.getCandTab().get_elem_count() == 0) {
        //Note that finding loop invariant does not mean finding
        //hoist candidate.
        return false;
    }
    InsertPreheaderMgr insertmgr(this, m_rce, anactx, ctx);
    if (insertmgr.needComplicatedGuard()) { return false; }

    //Check whether the LOOP need a preheader BB. And inserting guard region
    //if current loop's trip-count is undetermined.
    ASSERT0(ctx.verifyDomTree());
    bool insert_prehead = insertmgr.perform(anactx.getConstCandTab(), ctx);
    IRBB * preheader = insertmgr.getPreheader();

    //Hoist candidates that may include expressions and stmts.
    bool succ_hoisted = hoistCand(anactx, preheader, ctx);
    bool changed = false;
    if (!succ_hoisted) {
        insertmgr.undoCFGChange(ctx);
        //Note PDom may not be maintained.
        return false;
    }
    ASSERT0(ctx.inserted_guard_bb ==
            insertmgr.getInsertGuardHelper().hasInsertedGuard());
    insertmgr.reviseStmtExecAtLeastOnce();
    insertmgr.reviseSSADU(ctx);
    changed |= insert_prehead;
    changed |= ctx.duset_changed;

    //Check if preheader still on CFG.
    bool splitnewbb = false;
    if (m_cfg->isVertex(preheader->id())) {
        //---- Maintain BB characters.
        //hoistCand may append stmt into BB which has down-boundary stmt.
        //That makes BB invalid. Split such invalid BB into two or more BBs.
        //CASE:licm.gr
        splitnewbb = m_cfg->splitBBIfNeeded(preheader, *ctx.getOptCtx());
        ctx.cfg_changed |= splitnewbb;
    }
    ctx.cleanAfterLoop();
    return changed;
}


bool LICM::doLoopTree(LI<IRBB> * li, HoistCtx & ctx)
{
    if (li == nullptr) { return false; }
    bool changed = false;
    for (LI<IRBB> * tli = li; tli != nullptr; tli = LI_next(tli)) {
        changed |= doLoopTree(LI_inner_list(tli), ctx);
        changed |= processLoop(tli, ctx);
    }
    return changed;
}


bool LICM::dump() const
{
    if (!m_rg->isLogMgrInit()) { return true; }
    if (!g_dump_opt.isDumpPass(PASS_LICM)) { return true; }
    xoc::note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    //Invariant Variable info has been dumpped during the transformation.
    m_am.dump();
    bool succ = Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    return succ;
}


bool LICM::initSSAMgr(HoistCtx const& ctx)
{
    if (!ctx.getOptCtx()->is_pr_du_chain_valid() && !ctx.usePRSSADU()) {
        //The pass use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        set_valid(false);
        return false;
    }
    if (!ctx.getOptCtx()->is_nonpr_du_chain_valid() && !ctx.useMDSSADU()) {
        //The pass use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        set_valid(false);
        return false;
    }
    return true;
}


bool LICM::initDepPass(MOD HoistCtx & ctx)
{
    if (!initSSAMgr(ctx)) { return false; }
    PassTypeList optlist;
    optlist.append_tail(PASS_DOM);
    optlist.append_tail(PASS_LOOP_INFO);
    if (is_aggressive()) {
        if (g_do_gvn) { optlist.append_tail(PASS_GVN); }
        if (g_do_rce) { optlist.append_tail(PASS_DOM); }
    }
    m_rg->getPassMgr()->checkValidAndRecompute(ctx.getOptCtx(), optlist);
    m_rce = (RCE*)m_rg->getPassMgr()->queryPass(PASS_RCE);
    if (m_rce != nullptr && m_rce->is_use_gvn()) {
        GVN * gvn = (GVN*)m_rg->getPassMgr()->queryPass(PASS_GVN);
        if (!gvn->is_valid()) {
            gvn->perform(*ctx.getOptCtx());
        }
    }
    return true;
}


bool LICM::perform(OptCtx & oc)
{
    ASSERT0(m_rg->getBBList());
    if (m_rg->getBBList()->is_empty()) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    START_TIMER(t, getPassName());
    dumpBeforePass();
    xcom::DomTree domtree;
    Stmt2UseMgr s2u(getRegion());
    HoistCtx ctx(oc, &domtree, this, &s2u);
    if (!initDepPass(ctx)) { END_TIMER(t, getPassName()); return false; }
    ctx.buildDomTree(m_cfg);
    ASSERT0L3(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    bool change = doLoopTree(m_cfg->getLoopInfo(), ctx);
    postProcess(ctx, change, oc);
    clean();
    set_valid(true);
    ASSERT0L3(IRMgr::verify(m_rg));
    ASSERT0L3(xoc::verifyClassicDUChain(m_rg, oc));
    ASSERT0L3(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0L3(m_rg->getCFG()->verify());
    ASSERT0L3(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0L3(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    ASSERT0L3(m_cfg->verifyRPO(oc));
    ASSERT0L3(m_cfg->verifyLoopInfo(oc));
    ASSERT0L3(m_cfg->verifyDomAndPdom(oc));
    END_TIMER(t, getPassName());
    return change;
}


bool LICM::chooseSelectDet(
    IR * ir, IRIter & irit, OUT bool * all_exp_invariant, OUT IRList * invlist,
    OUT LICMAnaCtx & anactx)
{
    return chooseExp(ir, irit, all_exp_invariant, invlist, anactx);
}
//END LICM

} //namespace xoc
