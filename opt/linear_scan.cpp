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
//START ActMgr
//
ActMgr::~ActMgr()
{
    for (xcom::StrBuf * buf = m_act_list.get_head();
         buf != nullptr; buf = m_act_list.get_next()) {
        delete buf;
    }
}


void ActMgr::dump(CHAR const* format, ...)
{
    if (!m_rg->isLogMgrInit()) { return; }
    StrBuf * buf = new StrBuf(64);
    buf->strcat("ACT%u:", m_cnt);
    m_cnt++;
    va_list args;
    va_start(args, format);
    buf->vstrcat(format, args);
    m_act_list.append_tail(buf);
    va_end(args);
}


void ActMgr::dumpAll() const
{
    if (m_act_list.get_elem_count() == 0) { return; }
    note(m_rg, "\n==-- DUMP ALL ACT --==");
    m_rg->getLogMgr()->incIndent(2);
    xcom::List<xcom::StrBuf*>::Iter it;
    for (xcom::StrBuf * buf = m_act_list.get_head(&it);
         buf != nullptr; buf = m_act_list.get_next(&it)) {
        note(m_rg, "\n%s", buf->buf);
    }
    m_rg->getLogMgr()->decIndent(2);
}
//END ActMgr


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


bool LinearScanRA::preferCallee(LifeTime const* lt) const
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
    m_dedicated_mgr.clean();
    m_prno2var.clean();
}


Var * LinearScanRA::getSpillLoc(PRNO prno)
{
    return m_prno2var.get(prno);
}


Var * LinearScanRA::genSpillLoc(PRNO prno, Type const* ty)
{
    Var * v = getSpillLoc(prno);
    if (v == nullptr) {
        v = genFuncLevelVar(ty, STACK_ALIGNMENT);
        m_prno2var.set(prno, v);
    }
    return v;
}


IR * LinearScanRA::buildSpill(PRNO prno, Type const* ty)
{
    Var * spill_loc = genSpillLoc(prno, ty);
    ASSERT0(spill_loc);
    IR * pr = m_irmgr->buildPRdedicated(prno, ty);
    m_rg->getMDMgr()->allocRef(pr);
    IR * stmt = m_irmgr->buildStore(spill_loc, pr);
    m_rg->getMDMgr()->allocRef(stmt);
    return stmt;
}


IR * LinearScanRA::buildReload(PRNO prno, Var * spill_loc, Type const* ty)
{
    ASSERT0(spill_loc);
    IR * ld = m_irmgr->buildLoad(spill_loc, ty);
    m_rg->getMDMgr()->allocRef(ld);
    IR * stmt = m_irmgr->buildStorePR(prno, ty, ld);
    m_rg->getMDMgr()->allocRef(stmt);
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
    m_act_mgr.dumpAll();
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
    //Enable the dump-buffer.
    DumpBufferSwitch buff(m_rg->getLogMgr());
    UpdatePos up(*this);
    collectDedicatedPR(m_bb_list, m_dedicated_mgr);
    getLTMgr().computeLifeTime(up, m_bb_list, m_dedicated_mgr);
    LTPriorityMgr priomgr(m_cfg, getTIMgr());
    priomgr.computePriority(getLTMgr());
    bool changed = false;
    {
        //The default linear-scan implementation.
        LSRAImpl impl(*this);
        changed = impl.perform(oc);
        ASSERT0(verifyAfterRA());
    }
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
