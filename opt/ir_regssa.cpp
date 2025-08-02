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

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

typedef xcom::TTab<RegPhi const*> PhiTab;

static CHAR const* g_parting_line_char = "----------------";
static CHAR const* g_msg_no_regssainfo = " NORegSSAINFO!!";
static CHAR const* g_msg_empty_vropndset = " EMPTY VOPNDSET";

static bool hasMayDefUntilCoverDefByPhi(
    RegPhi const* phi, RegDef const* coverregdef,
    xcom::TTab<RegDef const*> & visited, RegSSAMgr const* mgr);

class LocalRegSSADump {
public:
    static void dumpIRWithRegSSAInfo(
        RegSSAMgr const* mgr, Region const* rg, IR const* ir)
    {
        rg->getLogMgr()->incIndent(2);
        mgr->dumpIRWithRegSSAForStmt(ir);
        mgr->dumpIRWithRegSSAForExpTree(ir);
        rg->getLogMgr()->decIndent(2);
    }
};


//
//START RegSSAUpdateCtx
//
RegSSAUpdateCtx::RegSSAUpdateCtx(OptCtx & oc, ActMgr * am) : PassCtx(&oc, am)
{
    RegSSAUPDATECTX_update_duchain(this) = true;
    RegSSAUPDATECTX_removed_vropnd_list(this) = nullptr;
    m_dom_tree = nullptr;
}


void RegSSAUpdateCtx::dump(Region const* rg) const
{
    ASSERT0(rg);
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    note(rg, "\n==-- DUMP RegSSAUpdateCtx --==");
    note(rg, "\nNEED_UPDATE_DUCHAIN:%s",
         need_update_duchain() ? "true" : "false");
    IRList * lst = getRemovedVROpndIRList();
    if (lst == nullptr) { return; }
    note(rg, "\nREMOVED_VOPND_IRLIST:");
    rg->getLogMgr()->incIndent(2);
    IRListIter it;
    for (IR * ir = lst->get_head(&it);
         ir != nullptr; ir = lst->get_next(&it)) {
        xoc::dumpIR(ir, rg);
    }
    rg->getLogMgr()->decIndent(2);
}
//END RegSSAUpdateCtx


//
//START RegSSAStatus
//
CHAR const* RegSSAStatus::getStatusName(FlagSetIdx s) const
{
    switch (s) {
    case REGSSA_STATUS_DOM_IS_INVALID: return "dom is invalid";
    default: UNREACHABLE();
    }
    return nullptr;
}
//END RegSSAStatus


//
//START VRFindAndSetLiveInDef
//
void VRFindAndSetLiveInDef::findAndSet(
    MOD IR * exp, IR const* startir, IRBB const* startbb,
    OUT RegSSAStatus & st, RegSSAUpdateCtx const* ctx)
{
    ASSERT0(m_mgr);
    ASSERTN(getOptCtx().is_dom_valid(), ("DOM info must be available"));
    ASSERT0(startir == nullptr ||
            (startir->is_stmt() && startir->getBB() == startbb));
    ASSERT0(exp && exp->is_exp() && RegSSAMgr::hasExpRegSSAInfo(exp));
    RegSSAInfo * info = m_mgr->genRegSSAInfo(exp);
    ASSERT0(info);
    OptCtx const& oc = getOptCtx();
    List<VReg*> newvregs;
    Reg must = m_mgr->getReg(exp);
    if (must != REG_UNDEF) {
        VReg * newvreg = m_mgr->findDomLiveInDefFrom(
            must, startir, startbb, oc, st);
        if (newvreg != nullptr) {
            newvregs.append_tail(newvreg);
        } else {
            //Need append init-version VReg to represent the existence of Reg.
            newvregs.append_tail(m_mgr->genInitVersionVReg(must));
        }
        SRegSet const* may = m_mgr->getAliasRegSet(must);
        if (may != nullptr) {
            SRegSetIter it;
            for (BSIdx i = may->get_first(&it); i != BS_UNDEF;
                 i = may->get_next(i, &it)) {
                VReg * newvreg = m_mgr->findDomLiveInDefFrom(
                    i, startir, startbb, oc, st);
                if (newvreg != nullptr) {
                    newvregs.append_tail(newvreg);
                    continue;
                }
                //Need append init-version VReg to represent the
                //existence of Reg.
                newvregs.append_tail(m_mgr->genInitVersionVReg(i));
            }
        }
    }
    m_mgr->removeExpFromAllVROpnd(exp);
    info->cleanVROpndSet(m_mgr->getRegDUMgr());
    for (VReg * v = newvregs.get_head();
         v != nullptr; v = newvregs.get_next()) {
        if (v->version() != REGSSA_INIT_VERSION) {
            v->addUse(exp);
        }
        info->addVROpnd(v, m_mgr->getRegDUMgr());
    }
}


void VRFindAndSetLiveInDef::findAndSetForTree(
    IR * exp, IR const* startir, IRBB const* startbb, OUT RegSSAStatus & st,
    RegSSAUpdateCtx const* ctx)
{
    ASSERTN(getOptCtx().is_dom_valid(), ("DOM info must be available"));
    ASSERT0(exp->is_exp());
    IRIter it;
    for (IR * x = iterInit(exp, it,
            false); //Do NOT iter sibling of root IR of 'exp'
         x != nullptr; x = iterNext(it, true)) {
        if (!RegSSAMgr::hasExpRegSSAInfo(x)) { continue; }
        findAndSet(x, startir, startbb, st, ctx);
    }
}


void VRFindAndSetLiveInDef::findAndSet(
    IR * exp, VReg const* prevdef_res, bool prevdef_is_phi,
    IRBB const* prevdef_bb, IR const* prevdef_occ,
    OUT RegSSAStatus & st, RegSSAUpdateCtx const* ctx)
{
    ASSERT0(m_mgr);
    ASSERTN(getOptCtx().is_dom_valid(), ("DOM info must be available"));
    ASSERT0(exp->is_exp());
    RegSSAInfo * regssainfo = m_mgr->getRegSSAInfoIfAny(exp);

    //CASE: To avoid assertions that raised by verify() which is used
    //to guanrantee operand of RegPhi is not NULL, replace the removed
    //vropnd of operand with initial-version vropnd.
    ASSERT0(prevdef_res);
    Reg prevdefregid = prevdef_res->reg();
    ASSERT0(prevdefregid != REG_UNDEF);
    IR const* startir = nullptr;
    VReg * newlivein = nullptr;
    OptCtx const& oc = getOptCtx();
    ASSERTN(oc.is_dom_valid(), ("DOM info must be available"));
    if (prevdef_is_phi) {
        newlivein = m_mgr->findDomLiveInDefFromIDomOf(
            prevdef_bb, prevdefregid, oc, st);
    } else {
        startir = prevdef_bb->getPrevIR(prevdef_occ);
        newlivein = m_mgr->findDomLiveInDefFrom(
            prevdefregid, startir, prevdef_bb, oc, st);
    }
    if (newlivein == nullptr || newlivein == prevdef_res) {
        newlivein = m_mgr->genInitVersionVReg(prevdefregid);
        regssainfo->addVROpnd(newlivein, m_mgr->getRegDUMgr());
        return;
    }
    m_mgr->buildDUChain(newlivein->getDef(), exp);
}


void VRFindAndSetLiveInDef::findAndSetForLst(
    IRList const& lst, VReg const* prevdef_res, bool prevdef_is_phi,
    IRBB const* prevdef_bb, IR const* prevdef_occ,
    OUT RegSSAStatus & st, RegSSAUpdateCtx const* ctx)
{
    ASSERTN(getOptCtx().is_dom_valid(), ("DOM info must be available"));
    IRListIter it;
    for (IR * ir = lst.get_head(&it);
         ir != nullptr; ir = lst.get_next(&it)) {
        findAndSet(ir, prevdef_res, prevdef_is_phi, prevdef_bb, prevdef_occ,
                   st, ctx);
    }
}
//END VRFindAndSetLiveInDef


//
//START ConstRegDefIter
//
static void iterDefCHelperPhi(
    RegDef const* def, IR const* use, RegSSAMgr const* mgr,
    OUT ConstRegDefIter & it)
{
    //Iter phi's opnd
    ASSERT0(def->is_phi());
    for (IR const* opnd = REGPHI_opnd_list(def);
         opnd != nullptr; opnd = opnd->get_next()) {
        if (opnd->is_const()) {
            //CONST does not have VReg info.
            continue;
        }
        VReg const* vreg = ((RegPhi const*)def)->getOpndVReg(opnd,
            const_cast<RegSSAMgr*>(mgr)->getRegDUMgr());
       if (vreg == nullptr) {
            //Note VROpndSet of 'opnd' may be empty after some optimization.
            //It does not happen when RegSSA just constructed. The USE that
            //without real-DEF will have a virtual-DEF that version is 0.
            //During some increment-maintaining of RegSSA, VROpnd may be
            //removed, and VROpndSet become empty.
            //This means the current USE, 'opnd', does not have real-DEF stmt,
            //the value of 'opnd' always coming from parameter of global value.
            //The ID of PHI should not be removed, because it is be regarded
            //as a place-holder of PHI, and the place-holder indicates the
            //position of related predecessor of current BB of PHI in CFG.
            continue;
        }

        RegDef * vreg_tdef = vreg->getDef();
        if (vreg_tdef == nullptr || vreg_tdef == def ||
            it.is_visited(vreg_tdef)) {
            continue;
        }
        it.set_visited(vreg_tdef);
        it.append_tail(vreg_tdef);
    }
}


static void iterDefCHelper(
    RegDef const* def, IR const* use, RegSSAMgr const* mgr,
    OUT ConstRegDefIter & it)
{
    ASSERT0(def);
    if (def->is_phi()) {
        iterDefCHelperPhi(def, use, mgr, it);
        return;
    }
    ASSERT0(def->getOcc());
    if (use != nullptr && isKillingDef(def->getOcc(), use, nullptr)) {
        //Stop the iteration until encounter the killing DEF real stmt.
        return;
    }
    RegDef const* prev = def->getPrev();
    if (prev == nullptr || it.is_visited(prev)) { return; }
    it.set_visited(prev);
    it.append_tail(prev);
}


RegDef const* ConstRegDefIter::get_first(RegDef const* def)
{
    ASSERT0(def);
    set_visited(def);
    iterDefCHelper(def, nullptr, m_regssamgr, *this);
    return def;
}


RegDef const* ConstRegDefIter::get_next()
{
    RegDef const* def = remove_head();
    if (def == nullptr) { return nullptr; }
    iterDefCHelper(def, nullptr, m_regssamgr, *this);
    return def;
}


RegDef const* ConstRegDefIter::get_first_untill_killing_def(
    RegDef const* def, IR const* use)
{
    ASSERT0(def && use && use->is_exp());
    set_visited(def);
    iterDefCHelper(def, use, m_regssamgr, *this);
    return def;
}


RegDef const* ConstRegDefIter::get_next_untill_killing_def(IR const* use)
{
    ASSERT0(use && use->is_exp());
    RegDef const* def = remove_head();
    if (def == nullptr) { return nullptr; }
    iterDefCHelper(def, use, m_regssamgr, *this);
    return def;
}
//END ConstRegDefIter


//
//START ConstRegSSAUSEIRIter
//
ConstRegSSAUSEIRIter::ConstRegSSAUSEIRIter(RegSSAMgr const* regssamgr)
    : vropndset_iter(nullptr), current_pos_in_vropndset(BS_UNDEF),
      current_pos_in_useset(BS_UNDEF), current_useset(nullptr),
      m_regssamgr(regssamgr),
      m_dumgr(const_cast<RegSSAMgr*>(regssamgr)->getRegDUMgr())
{
    m_rg = regssamgr->getRegion();
}


IR const* ConstRegSSAUSEIRIter::get_first(IR const* def)
{
    RegSSAMgr * pthis = const_cast<RegSSAMgr*>(m_regssamgr);
    RegDUMgr * udmgr = const_cast<RegDUMgr*>(m_dumgr);
    RegSSAInfo * info = pthis->getRegSSAInfoIfAny(def);
    ASSERT0(info);
    vropndset_iter = nullptr;
    vropndset = info->getVROpndSet();
    //Find the first iter and position in VROpndSet.
    for (current_pos_in_vropndset = vropndset->get_first(
            &vropndset_iter);
         current_pos_in_vropndset != BS_UNDEF;
         current_pos_in_vropndset = vropndset->get_next(
            current_pos_in_vropndset, &vropndset_iter)) {
        VReg * vr = (VReg*)udmgr->getVROpnd(current_pos_in_vropndset);
        ASSERT0(vr && vr->is_reg());
        current_useset = vr->getUseSet();
        useset_iter.clean();
        //Find the first iter and position in UseSet.
        for (current_pos_in_useset = current_useset->get_first(useset_iter);
             !useset_iter.end();
             current_pos_in_useset = current_useset->get_next(useset_iter)) {
            IR * use = m_rg->getIR(current_pos_in_useset);
            ASSERT0(use && !use->isReadPR());
            return use;
        }
    }
    return nullptr;
}


IR const* ConstRegSSAUSEIRIter::get_next()
{
    RegDUMgr * dumgr = const_cast<RegDUMgr*>(m_dumgr);

    //Update iter and position in UseSet.
    for (; !useset_iter.end(); UNREACHABLE()) {
        //Find next USE.
        current_pos_in_useset = current_useset->get_next(useset_iter);
        if (useset_iter.end()) {
            //Prepare next VROpnd.
            current_pos_in_vropndset = vropndset->get_next(
                current_pos_in_vropndset, &vropndset_iter);
            //Step into next VROpnd.
            break;
        } else {
            IR * use = m_rg->getIR(current_pos_in_useset);
            ASSERT0(use && !use->isReadPR());
            return use;
        }
    }

    //Update iter and position in VROpndSet.
    for (; current_pos_in_vropndset != BS_UNDEF;
         current_pos_in_vropndset = vropndset->get_next(
             current_pos_in_vropndset, &vropndset_iter)) {
        VReg * vr = (VReg*)dumgr->getVROpnd(current_pos_in_vropndset);
        ASSERT0(vr && vr->is_reg());
        current_useset = vr->getUseSet();
        useset_iter.clean();
        //Find the first iter and position in UseSet.
        for (current_pos_in_useset = current_useset->get_first(
                 useset_iter);
             !useset_iter.end(); UNREACHABLE()) {
            IR * use = m_rg->getIR(current_pos_in_useset);
            ASSERT0(use && !use->isReadPR());
            return use;
        }
    }
    return nullptr;
}
//END ConstRegSSAUSEIRIter


//
//START VRCollectUse
//
bool VRCollectCtx::verify() const
{
    if (flag.have(COLLECT_OUTSIDE_LOOP_IMM_USE) ||
        flag.have(COLLECT_INSIDE_LOOP)) {
        ASSERTN(m_li, ("need LoopInfo"));
    }
    ASSERTN(!(flag.have(COLLECT_OUTSIDE_LOOP_IMM_USE) &
              flag.have(COLLECT_INSIDE_LOOP)),
            ("flags are conflict"));
    return true;
}
//END VRCollectCtx


//
//START VRCollectUse
//
VRCollectUse::VRCollectUse(
    RegSSAMgr const* mgr, RegSSAInfo const* info, VRCollectCtx const& ctx,
    OUT IRSet * set)
    : m_mgr(mgr), m_ctx(ctx)
{
    m_dumgr = const_cast<RegSSAMgr*>(mgr)->getRegDUMgr();
    ASSERT0(set);
    collectForRegSSAInfo(info, set);
}


VRCollectUse::VRCollectUse(
    RegSSAMgr const* mgr, VReg const* vreg, VRCollectCtx const& ctx,
    OUT IRSet * set)
    : m_mgr(mgr), m_ctx(ctx)
{
    m_dumgr = const_cast<RegSSAMgr*>(mgr)->getRegDUMgr();
    ASSERT0(set);
    collectForVROpnd(vreg, set);
}


void VRCollectUse::collectOutsideLoopImmUseForVROpnd(
    VROpnd const* vropnd, MOD RegDefVisitor & vis, OUT IRSet * set) const
{
    VReg::UseSetIter vit;
    ASSERT0(m_ctx.verify());
    LI<IRBB> const* li = m_ctx.getLI();
    ASSERT0(vropnd->is_reg());
    VReg * r = const_cast<VReg *>((VReg const*)vropnd);
    Region * rg = m_dumgr->getRegion();
    for (UINT i = r->getUseSet()->get_first(vit);
         !vit.end(); i = r->getUseSet()->get_next(vit)) {
        IR const* ir = rg->getIR(i);
        ASSERT0(ir && !ir->is_undef());
        if (ir->getCode() == IR_PHYREG) {
            RegPhi const* phi = ((CPhyReg*)ir)->getRegPhi();
            ASSERT0(phi);
            if (!li->isInsideLoop(phi->getBB()->id())) {
                //The function only collect the immediate USE outside loop.
                //So if the RegPhi is outside loop, just collects the ID as
                //immediate USE.
                set->bunion(i);
                continue;
            }
            if (vis.is_visited(phi->id())) { continue; }
            //Cross the RegPhi that is inside loop.
            vis.set_visited(phi->id());
            collectUseCrossPhi(phi, vis, set);
            continue;
        }
        ASSERT0(ir->is_exp() && ir->getStmt());
        if (li->isInsideLoop(ir->getStmt()->getBB()->id())) {
            continue;
        }
        //ir is the outside loop immediate USE.
        set->bunion(i);
    }
}


void VRCollectUse::collectUseForVROpnd(
    VROpnd const* vropnd, MOD RegDefVisitor & vis, OUT IRSet * set) const
{
    VReg::UseSetIter vit;
    ASSERT0(m_ctx.verify());
    if (m_ctx.flag.have(COLLECT_OUTSIDE_LOOP_IMM_USE)) {
        return collectOutsideLoopImmUseForVROpnd(vropnd, vis, set);
    }
    bool cross_phi = m_ctx.flag.have(COLLECT_CROSS_PHI);
    bool must_inside_loop = m_ctx.flag.have(COLLECT_INSIDE_LOOP);
    LI<IRBB> const* li = m_ctx.getLI();
    ASSERT0(vropnd->is_reg());
    VReg * vr = const_cast<VReg *>((VReg const*)vropnd);
    Region * rg = m_dumgr->getRegion();
    for (UINT i = vr->getUseSet()->get_first(vit);
         !vit.end(); i = vr->getUseSet()->get_next(vit)) {
        if (!cross_phi) {
            set->bunion(i);
            continue;
        }
        IR const* ir = rg->getIR(i);
        ASSERT0(ir && !ir->is_undef());
        if (ir->getCode() == IR_PHYREG) {
            RegPhi const* phi = ((CPhyReg*)ir)->getRegPhi();
            ASSERT0(phi);
            if (must_inside_loop && !li->isInsideLoop(phi->getBB()->id())) {
                continue;
            }
            if (vis.is_visited(phi->id())) { continue; }
            vis.set_visited(phi->id());
            collectUseCrossPhi(phi, vis, set);
            continue;
        }
        ASSERT0(ir->is_exp() && ir->getStmt());
        if (must_inside_loop &&
            !li->isInsideLoop(ir->getStmt()->getBB()->id())) {
            continue;
        }
        set->bunion(i);
    }
}


void VRCollectUse::collectUseCrossPhi(
    RegPhi const* phi, MOD RegDefVisitor & vis, OUT IRSet * set) const
{
    ASSERT0(phi && phi->is_phi());
    collectUseForVROpnd(phi->getResult(), vis, set);
}


void VRCollectUse::collectForVROpnd(VROpnd const* vropnd, OUT IRSet * set) const
{
    //DO NOT CLEAN SET
    ASSERT0(set && vropnd && vropnd->is_reg());
    RegDefVisitor vis;
    collectUseForVROpnd(vropnd, vis, set);
}


void VRCollectUse::collectForRegSSAInfo(
    RegSSAInfo const* info, OUT IRSet * set) const
{
    //DO NOT CLEAN SET
    ASSERT0(set && info);
    VROpndSetIter it = nullptr;
    VROpndSet const& vropndset = info->readVROpndSet();
    RegDefVisitor vis;
    for (BSIdx i = vropndset.get_first(&it);
         i != BS_UNDEF; i = vropndset.get_next(i, &it)) {
        VReg const* vr = (VReg*)m_dumgr->getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        vis.clean();
        collectUseForVROpnd(vr, vis, set);
    }
}
//END VRCollectUse


//
//START VRCollectDef
//
VRCollectDef::VRCollectDef(
    RegSSAMgr const* mgr, RegSSAInfo const* info, VRCollectCtx const& ctx,
    Reg ref, OUT IRSet * set)
    : m_mgr(mgr), m_info(info), m_ctx(ctx), m_ref(ref)
{
    m_dumgr = const_cast<RegSSAMgr*>(mgr)->getRegDUMgr();
    ASSERT0(set);
    collect(set);
}


void VRCollectDef::collect(OUT IRSet * set) const
{
    //DO NOT CLEAN 'set'.
    VROpndSetIter it = nullptr;
    bool cross_phi = m_ctx.flag.have(COLLECT_CROSS_PHI);
    bool must_inside_loop = m_ctx.flag.have(COLLECT_INSIDE_LOOP);
    LI<IRBB> const* li = m_ctx.getLI();
    if (must_inside_loop) { ASSERT0(li); }
    for (BSIdx i = m_info->readVROpndSet().get_first(&it);
         i != BS_UNDEF; i = m_info->readVROpndSet().get_next(i, &it)) {
        VROpnd const* t = m_dumgr->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        RegDef * tdef = ((VReg*)t)->getDef();
        if (tdef == nullptr) { continue; }
        if (must_inside_loop && !li->isInsideLoop(tdef->getBB()->id())) {
            continue;
        }
        if (tdef->is_phi() && cross_phi) {
            //TODO: iterate phi operands.
            collectDefThroughDefChain(tdef, set);
            continue;
        }
        IR const* defstmt = tdef->getOcc();
        ASSERT0(defstmt);
        if (defstmt->isCallStmt()) {
            //CASE:call()
            //     ...=USE
            //Call is the only stmt that need to process specially.
            //Because it is not killing-def.
            collectDefThroughDefChain(tdef, set);
            continue;
        }
        ASSERT0(RegSSAMgr::hasRegSSAInfo(defstmt));
        Reg mustdef = m_mgr->getReg(defstmt);
        if (m_ref != REG_UNDEF && mustdef != REG_UNDEF &&
            (mustdef == m_ref || m_mgr->isExactCover(mustdef, m_ref))) {
            //defstmt is killing definition of 'ref'.
            set->bunion(defstmt->id());
            continue;
        }
        if (m_ref != REG_UNDEF) {
            //TODO:
            //CASE1:DEF=...
            //      ...=USE
            //CASE2:...=
            //      ...=USE
            //Both cases need to collect all DEFs until
            //encounter the killing-def.
            collectDefThroughDefChain(tdef, set);
            continue;
        }

        //CASE1:...=
        //         =...
        //CASE2:DEF=...
        //         =...
        //Both cases need to collect all DEFs through def-chain.
        collectDefThroughDefChain(tdef, set);
    }
}


void VRCollectDef::collectDefThroughDefChain(
    RegDef const* def, OUT IRSet * set) const
{
    ASSERT0(def);
    ConstRegDefIter it(m_mgr);
    bool must_inside_loop = m_ctx.flag.have(COLLECT_INSIDE_LOOP);
    LI<IRBB> const* li = m_ctx.getLI();
    if (must_inside_loop) { ASSERT0(li); }
    for (RegDef const* d = it.get_first(def); d != nullptr; d = it.get_next()) {
        if (must_inside_loop && !li->isInsideLoop(d->getBB()->id())) {
            continue;
        }
        if (d->is_phi()) {
            //Nothing to do. The DEF of operand will be iterated at
            //iterDefCHelper().
            continue;
        }
        ASSERT0(d->getOcc());
        set->bunion(d->getOcc()->id());

        //TODO:for now, we have to walk alone with DEF chain to
        //mark almost all DEF to be effect. This may lead to
        //traverse the same DEF many times. Apply DP like algo to reduce
        //the traversal time.
    }
}
//END VRCollectDef


//
//START BBID2VRegLiveSet
//
void BBID2VRegLiveSet::dump(RegSSAMgr const* mgr) const
{
    ASSERT0(mgr);
    RegSSAMgr * pmgr = const_cast<RegSSAMgr*>(mgr);
    Region const* rg = mgr->getRegion();
    note(rg, "\n==-- DUMP BBID2VRegLiveSet --==");
    VRegLiveSet * liveset;
    xcom::TMapIter<UINT, VRegLiveSet*> it;
    for (UINT bbid = get_first(it, &liveset);
         bbid != BBID_UNDEF; bbid = get_next(it, &liveset)) {
        note(rg, "\nBB%u:", bbid);
        if (liveset == nullptr) {
            prt(rg, "--");
            continue;
        }
        VRegLiveSetIter its;
        bool first = true;
        for (BSIdx i = liveset->get_first(&its);
             i != BS_UNDEF; i = liveset->get_next(i, &its)) {
            VReg * t = (VReg*)pmgr->getRegDUMgr()->getVROpnd(i);
            ASSERT0(t && t->is_reg());
            if (!first) { note(rg, ","); }
            first = false;
            t->dump(rg, pmgr->getRegDUMgr());
        }
    }
}
//END BBID2VRegLiveSet


//
//START RegSSAConstructRenameVisit
//
class RegSSAConstructRenameVisitVF : public xcom::VisitTreeFuncBase {
    DefRegSet const& m_effect_regs;
    BB2DefRegSet & m_defed_regs_vec;
    Reg2VRegStack & m_reg2vregstk;
    RegSSAMgr * m_mgr;
    IRCFG * m_cfg;
    BB2VRegMap m_bb2vregmap;
public:
    RegSSAConstructRenameVisitVF(
        DefRegSet const& effect_regs, BB2DefRegSet & defed_regs_vec,
        Reg2VRegStack & reg2vregstk, RegSSAMgr * mgr)
            : m_effect_regs(effect_regs), m_defed_regs_vec(defed_regs_vec),
              m_reg2vregstk(reg2vregstk), m_mgr(mgr)
    {
        m_cfg = mgr->getCFG();
        m_bb2vregmap.setElemNum(m_cfg->getBBList()->get_elem_count());
    }

    void visitWhenAllKidHaveBeenVisited(
        xcom::Vertex const* v, xcom::Stack<Vertex const*> &)
    {
        //Do post-processing while all kids of BB has been processed.
        Reg2VReg * regid2vreg = m_bb2vregmap.get(v->id());
        ASSERT0(regid2vreg);
        xcom::DefSBitSet * defed_regs = m_defed_regs_vec.get(v->id());
        ASSERT0(defed_regs);
        DefSBitSetIter cur = nullptr;
        for (BSIdx i = defed_regs->get_first(&cur);
             i != BS_UNDEF; i = defed_regs->get_next(i, &cur)) {
            VRegStack * vs = m_reg2vregstk.get(i);
            ASSERT0(vs && vs->get_bottom());
            ASSERT0(vs->get_top()->reg() == (Reg)i);
            VReg * vreg = regid2vreg->get(i);
            while (vs->get_top() != vreg) {
                vs->pop();
            }
        }
        //vregmap is useless from now on.
        m_bb2vregmap.erase(v->id());
    }
    bool visitWhenFirstMeet(xcom::Vertex const* v, xcom::Stack<Vertex const*> &)
    {
        DefRegSet const* defed_regs = m_defed_regs_vec.get(v->id());
        ASSERT0(defed_regs);
        IRBB * bb = m_cfg->getBB(v->id());
        m_mgr->handleBBRename(bb, m_effect_regs, *defed_regs,
                              m_bb2vregmap, m_reg2vregstk);
        return true;
    }
};


class RegSSAConstructRenameVisit
    : public xcom::VisitTree<RegSSAConstructRenameVisitVF> {
    COPY_CONSTRUCTOR(RegSSAConstructRenameVisit);
public:
    RegSSAConstructRenameVisit(
        DomTree const& domtree, IRBB * root, RegSSAConstructRenameVisitVF & vf)
        : VisitTree(domtree, root->id(), vf) {}
};
//END RegSSAConstructRenameVisit


//
//START RenameDefVisit
//
class VRRenameDefVisitFunc : public xcom::VisitTreeFuncBase {
    VRRenameDef & m_rndef;
    IRCFG * m_cfg;
    DomTree const& m_dt;
public:
    VRRenameDefVisitFunc(VRRenameDef & rndef, IRCFG * cfg, DomTree const& dt)
        : m_rndef(rndef), m_cfg(cfg), m_dt(dt) {}

    void visitWhenAllKidHaveBeenVisited(
        xcom::Vertex const* v, MOD xcom::Stack<Vertex const*> &)
    { m_rndef.getBBID2VRegLiveSet().free(v->id()); }

    bool visitWhenFirstMeet(
        xcom::Vertex const* v, MOD xcom::Stack<Vertex const*> & stk)
    {
        //Init liveset for given vertex.
        VRegLiveSet * tliveset = m_rndef.getBBID2VRegLiveSet().get(v->id());
        if (tliveset == nullptr) {
            Vertex const* parent = m_dt.getParent(v);
            ASSERT0(parent);
            VRegLiveSet const* pset =
                m_rndef.getBBID2VRegLiveSet().get(parent->id());
            ASSERT0(pset);
            tliveset = m_rndef.getBBID2VRegLiveSet().genAndCopy(v->id(), *pset);
        } else if (tliveset->all_killed()) {
            stk.pop(); //no need to perform rename-def anymore.
            m_rndef.getBBID2VRegLiveSet().free(v->id());
            //All VRegs processed, no need to go to kid vertex.
            return false;
        }
        IRBB * vbb = m_cfg->getBB(v->id());
        ASSERT0(vbb);
        BBIRListIter irlistit;
        vbb->getIRList().get_head(&irlistit);
        m_rndef.renameUseInBBTillNextDef(v, vbb, true, irlistit, *tliveset);
        if (tliveset->all_killed()) {
            stk.pop(); //no need to perform rename-def anymore.
            m_rndef.getBBID2VRegLiveSet().free(v->id());
            //All VRegs processed, no need to go to kid vertex.
            return false;
        }
        return true;
    }
};


class RenameDefVisit : public xcom::VisitTree<VRRenameDefVisitFunc> {
public:
    RenameDefVisit(VexIdx rootid, DomTree const& domtree,
                   VRRenameDefVisitFunc & vf) : VisitTree(domtree, rootid, vf)
    {
        //Skip the root vertex on DomTree.
        setVisited(rootid);
    }
};
//END RenameDefVisit



//
//START VRRenameDef
//
VRRenameDef::VRRenameDef(
    DomTree const& dt, bool build_ddchain, RegSSAMgr * mgr, ActMgr * am)
    : m_domtree(dt), m_am(am)
{
    m_liveset = nullptr;
    m_mgr = mgr;
    m_cfg = m_mgr->getCFG();
    m_is_build_ddchain = build_ddchain;
    m_dumgr = m_mgr->getRegDUMgr();
    m_rg = mgr->getRegion();
}


void VRRenameDef::clean()
{
    BBID2VRegLiveSet & ls = getBBID2VRegLiveSet();
    VRegLiveSet * liveset = nullptr;
    BBID2VRegLiveSetIter it;
    for (UINT bbid = ls.get_first(it, &liveset);
         bbid != BBID_UNDEF; bbid = ls.get_next(it, &liveset)) {
        if (liveset == nullptr) { continue; }
        liveset->clean();
    }
}


void VRRenameDef::dumpRenameBB(IRBB const* bb)
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    ActMgr * am = getActMgr();
    if (am == nullptr) { return; }
    am->dump("VRRenameDef:renaming BB%u", bb->id());
}


void VRRenameDef::dumpRenameVReg(IR const* ir, VReg const* vreg)
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    ActMgr * am = getActMgr();
    if (am == nullptr) { return; }
    VRegFixedStrBuf buf1;
    VRegFixedStrBuf buf2;
    am->dump("VRRenameDef:renaming %s with %s",
             xoc::dumpIRName(ir, buf1), vreg->dump(m_mgr->getRA(), buf2));
}


void VRRenameDef::dumpInsertDDChain(IR const* ir, VReg const* vreg)
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    ActMgr * am = getActMgr();
    if (am == nullptr) { return; }
    VRegFixedStrBuf buf1;
    VRegFixedStrBuf buf2;
    am->dump("VRRenameDef:meet %s, the lifetime of %s is stopped here",
             xoc::dumpIRName(ir, buf2), vreg->dump(m_mgr->getRA(), buf1));
}


void VRRenameDef::dumpInsertDDChain(RegPhi const* phi, VReg const* vreg)
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    ActMgr * am = getActMgr();
    if (am == nullptr) { return; }
    VRegFixedStrBuf buf;
    am->dump("VRRenameDef:meet RegPhi%u, the lifetime of %s is stopped here",
             phi->id(), vreg->dump(m_mgr->getRA(), buf));
}


void VRRenameDef::dumpRenamePhi(RegPhi const* phi, UINT opnd_pos)
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    ActMgr * am = getActMgr();
    if (am == nullptr) { return; }
    am->dump("VRRenameDef:rename RegPhi%u with No.%u operand",
             phi->id(), opnd_pos);
}


void VRRenameDef::renamePhiOpnd(
    RegPhi const* phi, UINT opnd_idx, MOD VReg * vreg)
{
    ASSERT0(phi->is_phi());
    RegDUMgr * udmgr = m_mgr->getRegDUMgr();
    IR * opnd = phi->getOpnd(opnd_idx);
    ASSERT0(opnd);
    RegSSAInfo * info = m_mgr->getRegSSAInfoIfAny(opnd);
    if (info == nullptr || info->isEmptyVROpndSet()) {
        info = m_mgr->genRegSSAInfoAndSetDedicatedVersionVReg(
            opnd, REGSSA_INIT_VERSION);
    }
    info->renameOrAddSpecificUse(opnd, vreg, udmgr);
    ASSERT0(info->readVROpndSet().get_elem_count() == 1);
}


//stmtbb: the BB of inserted stmt
//newinfo: RegSSAInfo that intent to be swap-in.
bool VRRenameDef::renameVRegForDesignatedPhiOpnd(
    RegPhi * phi, UINT opnd_pos, MOD VRegLiveSet & liveset)
{
    dumpRenamePhi(phi, opnd_pos);
    RegDUMgr * udmgr = m_mgr->getRegDUMgr();
    Reg phireg = phi->getResult()->reg();
    VRegLiveSetIter it = nullptr;
    for (BSIdx i = liveset.get_first(&it); i != BS_UNDEF;
         i = liveset.get_next(i, &it)) {
        VReg * t = (VReg*)udmgr->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (t->reg() == phireg) {
            renamePhiOpnd(phi, opnd_pos, t);
            return true;
        }
    }
    return false;
}


//vreg: intent to be swap-in.
//irtree: may be stmt or exp.
//irit: for local used.
void VRRenameDef::renameVRegForIRTree(
    IR * irtree, VReg * vreg, MOD IRIter & irit, bool & no_exp_has_ssainfo)
{
    irit.clean();
    for (IR * e = iterExpInit(irtree, irit);
         e != nullptr; e = iterExpNext(irit, true)) {
        if (!RegSSAMgr::hasRegSSAInfo(e)) { continue; }
        no_exp_has_ssainfo = false;
        RegSSAInfo * einfo = m_mgr->getRegSSAInfoIfAny(e);
        if (einfo == nullptr || einfo->isEmptyVROpndSet()) {
            einfo = m_mgr->genRegSSAInfoAndSetDedicatedVersionVReg(
                e, REGSSA_INIT_VERSION);
        }
        bool changed = einfo->renameOrAddSpecificUse(
            e, vreg, m_mgr->getRegDUMgr());
        if (changed) { dumpRenameVReg(e, vreg); }
    }
}


//ir: may be stmt or exp
//irit: for local used.
void VRRenameDef::renameLivedVRegForIRTree(
    IR * ir, MOD IRIter & irit, VRegLiveSet const& liveset)
{
    VROpndSetIter it = nullptr;
    RegDUMgr * udmgr = m_mgr->getRegDUMgr();
    bool no_exp_has_ssainfo = true;
    for (BSIdx i = liveset.get_first(&it); i != BS_UNDEF;
         i = liveset.get_next(i, &it)) {
        VReg * t = (VReg*)udmgr->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        renameVRegForIRTree(ir, t, irit, no_exp_has_ssainfo);
        if (no_exp_has_ssainfo) {
            //Early quit the loop.
            return;
        }
    }
}


bool VRRenameDef::tryInsertDDChainForDesigatedVReg(
    RegPhi * phi, VReg * vreg, MOD VRegLiveSet & liveset)
{
    ASSERT0(phi->is_phi());
    VReg const* phires = phi->getResult();
    ASSERT0(phires && phires->is_reg());
    if (phires->reg() != vreg->reg()) { return false; }
    liveset.set_killed(vreg->id());
    dumpInsertDDChain(phi, vreg);
    m_mgr->insertDefBefore(phires, vreg);
    return true;
}


bool VRRenameDef::tryInsertDDChainForDesigatedVReg(
    IR * ir, VReg * vreg, bool before, MOD VRegLiveSet & liveset)
{
    ASSERT0(ir->is_stmt());
    dumpInsertDDChain(ir, vreg);
    RegSSAInfo * irinfo = m_mgr->getRegSSAInfoIfAny(ir);
    if (irinfo == nullptr || irinfo->isEmptyVROpndSet()) {
        //ir may be new generated stmt. There is not RegSSAInfo allocated yet.
        irinfo = m_mgr->genRegSSAInfoAndSetNewVesionVReg(ir);
    }
    VROpndSetIter vit = nullptr;
    RegDUMgr * udmgr = m_mgr->getRegDUMgr();
    Reg reg = vreg->reg();
    VROpndSet const& irdefset = irinfo->readVROpndSet();
    BSIdx nexti = BS_UNDEF;
    for (BSIdx i = irdefset.get_first(&vit); i != BS_UNDEF; i = nexti) {
        nexti = irdefset.get_next(i, &vit);
        VReg * irdef = (VReg*)udmgr->getVROpnd(i);
        ASSERT0(irdef && irdef->is_reg());
        if (irdef->reg() != reg) { continue; }

        //ir's RegDef killed vreg.
        ASSERTN(irdef->version() != vreg->version(),
                ("same version DEF appeared at multiple places"));
        liveset.set_killed(vreg->id());
        if (before) {
            m_mgr->insertDefBefore(vreg, irdef);
        } else {
            m_mgr->insertDefBefore(irdef, vreg);
        }
        return true;
    }
    return false;
}


bool VRRenameDef::tryInsertDDChainForPhi(
    RegPhi * phi, MOD VRegLiveSet & liveset)
{
    ASSERT0(phi->is_phi());
    VROpndSetIter it = nullptr;
    RegDUMgr * udmgr = m_mgr->getRegDUMgr();
    bool inserted = false;
    BSIdx nexti;
    for (BSIdx i = liveset.get_first(&it); i != BS_UNDEF; i = nexti) {
        nexti = liveset.get_next(i, &it); //i may be removed.
        VReg * t = (VReg*)udmgr->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (!liveset.is_live(i)) { continue; }
        inserted |= tryInsertDDChainForDesigatedVReg(phi, t, liveset);
    }
    return inserted;
}


bool VRRenameDef::tryInsertDDChainForStmt(
    IR * ir, bool before, MOD VRegLiveSet & liveset)
{
    ASSERT0(ir->is_stmt());
    VROpndSetIter it = nullptr;
    RegDUMgr * udmgr = m_mgr->getRegDUMgr();
    bool inserted = false;
    BSIdx nexti;
    for (BSIdx i = liveset.get_first(&it); i != BS_UNDEF; i = nexti) {
        nexti = liveset.get_next(i, &it); //i may be removed.
        VReg * t = (VReg*)udmgr->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (!liveset.is_live(i)) { continue; }
        inserted |= tryInsertDDChainForDesigatedVReg(ir, t, before, liveset);
    }
    return inserted;
}


void VRRenameDef::killLivedVReg(RegPhi const* phi, MOD VRegLiveSet & liveset)
{
    RegDUMgr * udmgr = m_mgr->getRegDUMgr();
    Reg phireg = phi->getResult()->reg();
    VROpndSetIter it = nullptr;
    BSIdx nexti = BS_UNDEF;
    for (BSIdx i = liveset.get_first(&it); i != BS_UNDEF; i = nexti) {
        nexti = liveset.get_next(i, &it);
        VReg * t = (VReg*)udmgr->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (t->reg() != phireg) { continue; }
        liveset.set_killed(t->id());
    }
}


//defvex: domtree vertex.
void VRRenameDef::iterSuccBBPhiListToRename(
    Vertex const* defvex, IRBB const* succ, UINT opnd_idx,
    MOD VRegLiveSet & liveset)
{
    ASSERT0(succ);
    dumpRenameBB(succ);
    RegPhiList * philist = m_mgr->getPhiList(succ->id());
    if (philist == nullptr) { return; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi);
        renameVRegForDesignatedPhiOpnd(phi, opnd_idx, liveset);
        if (liveset.all_killed()) {
            return;
        }
    }
}


//defvex: domtree vertex.
void VRRenameDef::iterSuccBB(Vertex const* defvex, MOD VRegLiveSet & liveset)
{
    Vertex const* cfgv = m_cfg->getVertex(defvex->id());
    ASSERT0(cfgv);
    AdjVertexIter it;
    for (Vertex const* succv = Graph::get_first_out_vertex(cfgv, it);
         succv != nullptr; succv = Graph::get_next_out_vertex(it)) {
        UINT opnd_idx = 0; //the index of corresponding predecessor.
        AdjVertexIter it2;
        bool find = false;

        //Note the function will count the number of predecessors of
        //each BB as the number of operand of PHI, even if some of them are
        //unreachable from region-entry, and will be removed by followed CFG
        //optimizations.
        for (Vertex const* predv = Graph::get_first_in_vertex(succv, it2);
             predv != nullptr;
             predv = Graph::get_next_in_vertex(it2), opnd_idx++) {
            if (predv->id() == cfgv->id()) {
                find = true;
                break;
            }
        }
        ASSERTN_DUMMYUSE(find, ("not found related pred"));

        //Replace opnd of PHI of 'succ' with lived SSA version.
        iterSuccBBPhiListToRename(defvex, m_cfg->getBB(succv->id()),
                                  opnd_idx, liveset);
    }
}


void VRRenameDef::iterBBPhiListToKillLivedVReg(
    IRBB const* bb, VRegLiveSet & liveset)
{
    ASSERT0(bb);
    RegPhiList * philist = m_mgr->getPhiList(bb->id());
    if (philist == nullptr) { return; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi);
        killLivedVReg(phi, liveset);
        if (liveset.all_killed()) {
            return;
        }
    }
}


void VRRenameDef::connectPhiTillPrevDef(
    IRBB const* bb, BBIRListIter & irlistit, MOD VRegLiveSet & liveset)
{
    ASSERT0(bb);
    RegPhiList * philist = m_mgr->getPhiList(bb->id());
    if (philist == nullptr) { return; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi);
        tryInsertDDChainForPhi(phi, liveset);
        if (liveset.all_killed()) {
            return;
        }
    }
}


void VRRenameDef::connectIRTillPrevDef(
    IRBB const* bb, BBIRListIter & irlistit, MOD VRegLiveSet & liveset)
{
    IRIter irit;
    BBIRList & irlist = const_cast<IRBB*>(bb)->getIRList();
    for (; irlistit != nullptr; irlistit = irlist.get_prev(irlistit)) {
        IR * stmt = irlistit->val();
        if (!RegSSAMgr::hasRegSSAInfo(stmt)) { continue; }
        tryInsertDDChainForStmt(stmt, false, liveset);
        if (liveset.all_killed()) {
            return;
        }
    }
}


void VRRenameDef::renameIRTillNextDef(
    IRBB const* bb, BBIRListIter & irlistit, MOD VRegLiveSet & liveset)
{
    dumpRenameBB(bb);
    IRIter irit;
    BBIRList & irlist = const_cast<IRBB*>(bb)->getIRList();
    for (; irlistit != nullptr; irlistit = irlist.get_next(irlistit)) {
        IR * stmt = irlistit->val();
        renameLivedVRegForIRTree(stmt, irit, liveset);
        if (!RegSSAMgr::hasRegSSAInfo(stmt)) { continue; }
        tryInsertDDChainForStmt(stmt, true, liveset);
        if (liveset.all_killed()) {
            return;
        }
    }
}


//stmtbbid: indicates the BB of inserted stmt
//v: the vertex on DomTree.
//bb: the BB that to be renamed
//dompred: indicates the predecessor of 'bb' in DomTree
//Note stmtbbid have to dominate 'bb'.
void VRRenameDef::connectDefInBBTillPrevDef(
    IRBB const* bb, BBIRListIter & irlistit, MOD VRegLiveSet & liveset)
{
    connectIRTillPrevDef(bb, irlistit, liveset);
    if (liveset.all_killed()) { return; }
    connectPhiTillPrevDef(bb, irlistit, liveset);
}


//stmtbbid: indicates the BB of inserted stmt
//v: the vertex on DomTree.
//bb: the BB that to be renamed
//dompred: indicates the predecessor of 'bb' in DomTree
//Note stmtbbid have to dominate 'bb'.
void VRRenameDef::renameUseInBBTillNextDef(
    Vertex const* defvex, IRBB const* bb, bool include_philist,
    BBIRListIter & irlistit, MOD VRegLiveSet & liveset)
{
    if (include_philist) {
        iterBBPhiListToKillLivedVReg(bb, liveset);
        if (liveset.all_killed()) { return; }
    }
    renameIRTillNextDef(bb, irlistit, liveset);
    if (liveset.all_killed()) { return; }
    iterSuccBB(defvex, liveset);
}


//defvex: the vertex on DomTree.
//start_ir: if it is nullptr, the renaming will start at the first IR in bb.
//          otherwise the renaming will start at the NEXT IR of start_ir.
void VRRenameDef::renameFollowUseIntraBBTillNextDef(
    Vertex const* defvex, MOD VRegLiveSet & stmtliveset,
    IRBB const* start_bb, IR const* start_ir)
{
    ASSERT0(start_bb);
    BBIRListIter irlistit = nullptr;
    BBIRList & irlist = const_cast<IRBB*>(start_bb)->getIRList();
    if (start_ir == nullptr) {
        irlist.get_head(&irlistit);
    } else {
        irlist.find(const_cast<IR*>(start_ir), &irlistit);
        ASSERT0(irlistit);
        irlistit = irlist.get_next(irlistit);
    }
    renameUseInBBTillNextDef(defvex, start_bb, false, irlistit, stmtliveset);
}


void VRRenameDef::connectDefInterBBTillPrevDef(
    Vertex const* defvex, MOD VRegLiveSet & stmtliveset, IRBB const* start_bb)
{
    for (Vertex const* p = m_domtree.getParent(defvex);
         p != nullptr; p = m_domtree.getParent(p)) {
        IRBB * bb = m_cfg->getBB(p->id());
        ASSERT0(bb);
        BBIRListIter irlistit;
        bb->getIRList().get_tail(&irlistit);
        connectDefInBBTillPrevDef(bb, irlistit, stmtliveset);
        if (stmtliveset.all_killed()) {
            return;
        }
    }
}


void VRRenameDef::renameFollowUseInterBBTillNextDef(
    Vertex const* defvex, MOD VRegLiveSet & stmtliveset, IRBB const* start_bb)
{
    VRRenameDefVisitFunc vf(*this, m_cfg, m_domtree);
    RenameDefVisit rn(defvex->id(), m_domtree, vf);
    rn.visit();
}


//defvex: the vertex on DomTree.
void VRRenameDef::rename(Vertex const* defvex, VRegLiveSet * defliveset,
                         IRBB const* start_bb, IR const* start_ir)
{
    renameFollowUseIntraBBTillNextDef(defvex, *defliveset, start_bb, start_ir);
    if (defliveset->all_killed()) {
        m_bbid2liveset.free(start_bb->id());
        return;
    }
    renameFollowUseInterBBTillNextDef(defvex, *defliveset, start_bb);
}


void VRRenameDef::connect(Vertex const* defvex, VRegLiveSet * defliveset,
                          IRBB const* start_bb, IR const* start_ir)
{
    ASSERT0(start_bb && start_ir);
    BBIRListIter irlistit = nullptr;
    BBIRList & irlist = const_cast<IRBB*>(start_bb)->getIRList();
    irlist.find(const_cast<IR*>(start_ir), &irlistit);
    ASSERT0(irlistit);
    irlistit = irlist.get_prev(irlistit);
    connectDefInBBTillPrevDef(start_bb, irlistit, *defliveset);
    if (defliveset->all_killed()) { return; }
    connectDefInterBBTillPrevDef(defvex, *defliveset, start_bb);
}


void VRRenameDef::processPhi(RegPhi const* newphi)
{
    ASSERT0(newphi);
    IRBB const* bb = newphi->getBB();
    Vertex const* defvex = m_domtree.getVertex(bb->id());
    ASSERT0(m_bbid2liveset.get(bb->id()) == nullptr);
    VRegLiveSet * defliveset = m_bbid2liveset.genAndCopy(
        bb->id(), newphi->getResult());
    rename(defvex, defliveset, bb, nullptr);
    //Phi does not have previous-def.
}


void VRRenameDef::processStmt(IR * newstmt)
{
    ASSERT0(newstmt && RegSSAMgr::hasRegSSAInfo(newstmt));
    IRBB const* bb = newstmt->getBB();
    ASSERT0(bb);
    RegSSAInfo const* info = m_mgr->getRegSSAInfoIfAny(newstmt);
    if (info == nullptr || info->isEmptyVROpndSet()) {
        info = m_mgr->genRegSSAInfoAndSetNewVesionVReg(newstmt);
    }
    if (info->readVROpndSet().get_elem_count() == 0) {
        //RegSSAInfo may be empty if CALL does not have MustRef and MayRef.
        return;
    }
    ASSERT0(m_bbid2liveset.get(bb->id()) == nullptr ||
            m_bbid2liveset.get(bb->id())->is_empty());
    VRegLiveSet * defliveset = m_bbid2liveset.genAndCopy(
        bb->id(), info->readVROpndSet());
    Vertex const* defvex = m_domtree.getVertex(bb->id());
    ASSERTN(defvex, ("miss vertex on domtree"));
    rename(defvex, defliveset, bb, newstmt);
    if (!m_is_build_ddchain) { return; }

    //Previous defliveset may has been freed.
    VRegLiveSet * newdefliveset = m_bbid2liveset.genAndCopy(
        bb->id(), info->readVROpndSet());
    connect(defvex, newdefliveset, bb, newstmt);
}


void VRRenameDef::rename(IR * newstmt)
{
    processStmt(newstmt);
}


void VRRenameDef::rename(RegPhi const* newphi)
{
    processPhi(newphi);
}
//END VRRenameDef


//
//START VRRecomputeDefDefAndDefUseChain
//
VRRecomputeDefDefAndDefUseChain::VRRecomputeDefDefAndDefUseChain(
    xcom::DomTree const& domtree, RegSSAMgr * mgr,
    OptCtx const& oc, ActMgr * am)
    : m_domtree(domtree), m_mgr(mgr), m_oc(oc), m_am(am)
{
    m_rg = m_mgr->getRegion();
    m_cfg = m_rg->getCFG();
}


void VRRecomputeDefDefAndDefUseChain::recompute(MOD IR * stmt)
{
    ASSERT0(stmt && stmt->is_stmt() && RegSSAMgr::hasRegSSAInfo(stmt));
    RegSSAUpdateCtx ctx(const_cast<OptCtx&>(m_oc));
    if (m_mgr->getRegSSAInfoIfAny(stmt) != nullptr) {
        //There is no RegSSAInfo if 'stmt' is just generated.
        //Remove old RegSSAInfo, cutoff DefDef chain and DefUse chain before
        //recomputation.
        m_mgr->removeStmtRegSSAInfo(stmt, ctx);
    }
    //Generate new RegSSAInfo according to stmt's memory reference.
    RegSSAInfo const* info = m_mgr->genRegSSAInfoAndSetNewVesionVReg(stmt);
    ASSERT0_DUMMYUSE(info);

    //RegSSAInfo may be empty if CALL does not have any MustRef and MayRef.
    //ASSERT0(info->readVROpndSet().get_elem_count() > 0);
    //Perform renaming according to the new versioned VReg.
    //Note the original USE of origin VReg may not be reached by this renaming
    //because 'stmt' may have been moved a BB that does not dominate origin
    //USE any more. And the origin USE's DefUse chain will be revised during
    //InsertPreheaderMgr::reviseSSADU().
    VRRenameDef rn(getDomTree(), true, m_mgr, getActMgr());
    rn.rename(stmt);
}


void VRRecomputeDefDefAndDefUseChain::recompute(xcom::List<IR*> const& irlist)
{
    xcom::List<IR*>::Iter it;
    for (IR * stmt = irlist.get_head(&it);
         stmt != nullptr; stmt = irlist.get_next(&it)) {
        ASSERT0(m_mgr->getRegSSAInfoIfAny(stmt));
        recompute(stmt);
    }
}


void VRRecomputeDefDefAndDefUseChain::recompute(RegPhi const* phi)
{
    ASSERT0(phi);
    VRRenameDef rn(getDomTree(), true, m_mgr, getActMgr());
    rn.rename(phi);
}


//In C++, local declared class should NOT be used in template parameters of a
//template class. Because the template class may be instanced outside the
//function and the local type in function is invisible.
class VFToRecomp {
    IR const* m_prev_stmt;
    IRBB const* m_start_bb;
    RegSSAMgr * m_mgr;
    OptCtx const& m_oc;
    RegSSAStatus & m_st;
public:
    VFToRecomp(IR const* prev, IRBB const* s, RegSSAMgr * m,
               OptCtx const& oc, MOD RegSSAStatus & st)
        : m_prev_stmt(prev), m_start_bb(s), m_mgr(m), m_oc(oc), m_st(st) {}
    bool visitIR(MOD IR * ir, OUT bool & is_term)
    {
        if (!ir->is_exp() || !RegSSAMgr::hasRegSSAInfo(ir)) { return true; }
        VRFindAndSetLiveInDef fs(m_mgr, m_oc);
        fs.findAndSet(ir, m_prev_stmt, m_start_bb, m_st);
        return true;
    }
};


//irit: for local used.
void VRRecomputeDefDefAndDefUseChain::recomputeDefForRHS(MOD IR * stmt)
{
    class IterTree : public VisitIRTree<VFToRecomp> {
    public:
        IterTree(VFToRecomp & vf) : VisitIRTree(vf) {}
    };
    ASSERT0(stmt && stmt->is_stmt() && stmt->getBB());
    IRBB * start_bb = stmt->getBB();
    IR * prev_stmt = start_bb->getPrevIR(stmt);
    RegSSAStatus st;
    VFToRecomp vf(prev_stmt, start_bb, m_mgr, m_oc, st);
    IterTree it(vf);
    it.visit(stmt);
}


void VRRecomputeDefDefAndDefUseChain::recomputeDefForPhiOpnd(RegPhi const* phi)
{
    UINT idx = 0;
    ASSERT0(phi->getBB());
    Vertex const* vex = phi->getBB()->getVex();
    ASSERT0(vex);
    AdjVertexIter itv;
    ASSERT0(m_oc.is_dom_valid());
    RegSSAStatus st;
    VRFindAndSetLiveInDef fs(m_mgr, m_oc);
    for (xcom::Vertex const* in = Graph::get_first_in_vertex(vex, itv);
         in != nullptr; in = Graph::get_next_in_vertex(itv), idx++) {
        IR * opnd = phi->getOpnd(idx);
        ASSERT0(opnd->is_leaf());
        fs.findAndSet(opnd, m_cfg->getBB(in->id()), st);
    }
}


void VRRecomputeDefDefAndDefUseChain::recomputeDefForPhiOpnd(
    RegPhiList const* philist)
{
    ASSERT0(philist);
    ASSERT0(m_oc.is_dom_valid());
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        recomputeDefForPhiOpnd(phi);
    }
}


void VRRecomputeDefDefAndDefUseChain::recompute(RegPhiList const* philist)
{
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        recompute(phi);
   }
}
//END VRRecomputeDefDefAndDefUseChain


//
//SATRT VRRenameExp
//
VRRenameExp::VRRenameExp(RegSSAMgr * mgr, OptCtx * oc, ActMgr * am)
    : m_mgr(mgr), m_am(am), m_oc(oc)
{
    m_rg = mgr->getRegion();
}


//In C++, local declared class should NOT be used in template parameters of a
//template class. Because the template class may be instanced outside the
//function and the local type in function is invisible.
class VFToRename {
public:
    bool visitIR(IR * ir, OUT bool & is_term)
    {
        if (!ir->is_exp() || !RegSSAMgr::hasExpRegSSAInfo(ir)) { return true; }
        VRFindAndSetLiveInDef fs(m_regssamgr, *m_oc);
        fs.findAndSet(ir, m_startir, m_startbb, *m_st);
        return true;
    }
public:
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until meet the
    //         CFG entry.
    //startbb: the BB that begin to do searching. It can NOT be NULL.
    IR const* m_startir;
    IRBB const* m_startbb;
    RegSSAMgr * m_regssamgr;
    OptCtx * m_oc;
    RegSSAStatus * m_st;
public:
    //startir: may be NULL.
    VFToRename(IR const* startir, IRBB const* startbb, RegSSAMgr * regssamgr,
               OptCtx * oc, RegSSAStatus * st)
    {
        ASSERT0(startbb && regssamgr && oc && st);
        m_startir = startir;
        m_startbb = startbb;
        m_regssamgr = regssamgr;
        m_oc = oc;
        m_st = st;
    }
};


void VRRenameExp::rename(MOD IR * root, IR const* startir, IRBB const* startbb)
{
    class IterTree : public VisitIRTree<VFToRename> {
    public:
        IterTree(VFToRename & vf) : VisitIRTree(vf) {}
    };
    ASSERT0(root && (root->is_stmt() || root->is_exp()));
    ASSERT0(startir == nullptr ||
            (startir->is_stmt() && startir->getBB() == startbb));
    RegSSAStatus st;
    VFToRename vf(startir, startbb, m_mgr, m_oc, &st);
    IterTree it(vf);
    it.visit(root);
}
//END VRRenameExp


//
//START ReconstructRegSSA
//
ReconstructRegSSAVF::ReconstructRegSSAVF(
    xcom::VexTab const& vextab, DomTree const& dt, xcom::Graph const* cfg,
    RegSSAMgr * mgr, OptCtx * oc, ActMgr * am)
        : m_vextab(vextab), m_cfg(cfg), m_regssamgr(mgr), m_oc(oc), m_am(am),
          m_dt(dt)
{
    ASSERT0(m_regssamgr);
    m_rg = m_regssamgr->getRegion();
}


void ReconstructRegSSAVF::renameBBPhiList(IRBB const* bb) const
{
    RegPhiList const* philist = m_regssamgr->getPhiList(bb);
    if (philist == nullptr) { return; }
    VRRenameDef rn(m_dt, false, m_regssamgr, getActMgr());
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi const* phi = it->val();
        ASSERT0(phi && phi->is_phi());
        rn.clean();
        rn.rename(phi);
    }
}


void ReconstructRegSSAVF::renameBBIRList(IRBB const* bb) const
{
    BBIRList & irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter irit;
    IR * prev = nullptr;
    VRRenameExp rne(m_regssamgr, m_oc, getActMgr());

    //CASE:DefDef chain is necessary after new stmt generated.
    //e.g:compile/vect18_5_2.c, compile.gr/vect18_5.low.gr
    //The new generated stmt should connect DefDef chain with the previous
    //stmt when it has been inserted into a BB.
    bool need_build_dd_chain = true;
    VRRenameDef rnd(m_dt, need_build_dd_chain, m_regssamgr, getActMgr());
    for (IR * ir = irlst.get_head(&irit);
         ir != nullptr; prev = ir, ir = irlst.get_next(&irit)) {
        ASSERT0(ir->is_stmt());
        if (RegSSAMgr::hasRegSSAInfo(ir)) {
            rnd.clean();
            rnd.rename(ir);
        }
        rne.rename(ir, prev, bb);
    }
}
//END ReconstructRegSSA


//
//START VRegLiveSet
//
void VRegLiveSet::dump(RegSSAMgr const* mgr) const
{
    note(mgr->getRegion(), "\nVRegLiveSet:");
    bool first = true;
    VROpndSetIter it;
    RegSSAMgr * pmgr = const_cast<RegSSAMgr*>(mgr);
    for (BSIdx i = get_first(&it);
         i != BS_UNDEF; i = get_next(i, &it)) {
        VReg * t = (VReg*)const_cast<RegSSAMgr*>(mgr)->
            getRegDUMgr()->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (!first) { prt(mgr->getRegion(), ","); }
        first = false;
        t->dump(mgr->getRegion(), pmgr->getRegDUMgr());
    }
    if (first) {
        //Set is empty.
        prt(mgr->getRegion(), "--");
    }
}
//END VRegLiveSet


//
//START VReg::UseSet
//
void VReg::UseSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    VReg::UseSetIter it;
    note(rg, "\nVReg::UseSet:");
    xcom::StrBuf tmp(8);
    for (UINT i = get_first(it); !it.end(); i = get_next(it)) {
        IR const* ir = rg->getIR(i);
        ASSERT0(ir);
        prt(rg, "<%s> ", dumpIRName(ir, tmp));
    }
}
//END VReg::UseSet


//
//START BB2DefRegSet
//
void BB2DefRegSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    note(rg, "\n==-- DUMP BB2DefRegSet --==");
    BBList * bbl = rg->getBBList();
    for (IRBB const* bb = bbl->get_head();
         bb != nullptr; bb = bbl->get_next()) {
        DefRegSet * defregs = get(bb->id());
        note(rg, "\nBB%u DefinedRegSet:", bb->id());
        if (defregs == nullptr) { continue; }
        defregs->dump(rg->getLogMgr()->getFileHandler());
    }
}
//END


//
//START Reg2VRegStack
//
void Reg2VRegStack::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    note(rg, "\n==-- DUMP Reg2VRegStack --==");
    for (VecIdx i = REG_UNDEF + 1; i <= get_last_idx(); i++) {
        VRegStack * s = get(i);
        note(rg, "\nReg%u:", i);
        if (s == nullptr) {
            continue;
        }
        for (VReg * vreg = s->get_bottom();
             vreg != nullptr; vreg = s->get_next()) {
            ASSERT0(vreg->reg() == (Reg)i);
            prt(rg, "v%u|", vreg->version());
        }
    }
}


void Reg2VRegStack::destroy()
{
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        VRegStack * s = get(i);
        if (s != nullptr) { delete s; }
    }
    Vector<VRegStack*>::destroy();
}


VRegStack * Reg2VRegStack::gen(Reg reg)
{
    VRegStack * stk = get(reg);
    if (stk == nullptr) {
        stk = new VRegStack();
        set(reg, stk);
    }
    return stk;
}


VReg * Reg2VRegStack::get_top(Reg reg) const
{
    VRegStack * stk = get(reg);
    if (stk == nullptr) { return nullptr; }
    return stk->get_top();
}


void Reg2VRegStack::push(Reg reg, VReg * vreg)
{
    ASSERT0(reg != REG_UNDEF && vreg && vreg->reg() == reg);
    VRegStack * stk = gen(reg);
    ASSERT0(stk);
    stk->push(vreg);
}
//END Reg2VRegStack


//
//START RegSSAMgr
//
size_t RegSSAMgr::count_mem() const
{
    size_t count = m_map_reg2stack.count_mem();
    count += m_max_version.count_mem();
    count += m_dumgr.count_mem();
    count += sizeof(*this);
    return count;
}


IRBB * RegSSAMgr::getExpBB(IR const* ir)
{
    return ir->getCode() == IR_PHYREG ?
        PHYREG_phi(ir)->getBB() : ir->getStmt()->getBB();
}


RegSSAMgr::RegSSAMgr(Region * rg)
    : Pass(rg), m_sbs_mgr(rg->getMiscBitSetMgr()),
    m_seg_mgr(rg->getMiscBitSetMgr()->getSegMgr()), m_dumgr(rg, this)
{
    cleanInConstructor();
    ASSERT0(rg);
    m_tm = rg->getTypeMgr();
    m_irmgr = rg->getIRMgr();
    ASSERT0(m_tm);
    ASSERT0(rg->getMiscBitSetMgr());
    ASSERT0(m_seg_mgr);
    m_cfg = rg->getCFG();
    ASSERTN(m_cfg, ("cfg is not available."));
    m_am = new ActMgr(m_rg);
    m_ra = nullptr;
    m_timgr = nullptr;
}


void RegSSAMgr::destroy(RegSSAUpdateCtx const* ctx)
{
    if (m_dumgr.m_regssainfo_pool == nullptr) { return; }

    //CAUTION: If you do not finish out-of-SSA prior to destory(),
    //the reference to IR's RegSSA info will lead to undefined behaviors.
    //ASSERTN(!is_valid(),
    //        ("Still in SSA mode, you should do out-of-SSA before destroy"));

    freePhiList(ctx);
    delete m_am;
    m_am = nullptr;
}


void RegSSAMgr::freeBBPhiList(IRBB * bb, RegSSAUpdateCtx const* ctx)
{
    RegPhiList * philist = getPhiList(bb->id());
    if (philist == nullptr) { return; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        if (ctx != nullptr) {
            ctx->tryInvalidInfoBeforeFreeIRList(phi->getOpndList());
        }
        m_rg->freeIRTreeList(phi->getOpndList());
        REGPHI_opnd_list(phi) = nullptr;
    }
}


void RegSSAMgr::freePhiList(RegSSAUpdateCtx const* ctx)
{
    for (IRBB * bb = m_rg->getBBList()->get_head();
         bb != nullptr; bb = m_rg->getBBList()->get_next()) {
        freeBBPhiList(bb, ctx);
    }
    m_dumgr.m_philist_vec.destroy();
    m_dumgr.m_philist_vec.init();
}


//The function destroy data structures that allocated during SSA
//construction, and these data structures are only useful in construction.
void RegSSAMgr::cleanLocalUsedData()
{
    //Do NOT clean max_version of each Reg, because some passes will generate
    //DEF stmt for individual Reg, which need new version of the Reg.
    //Reg's max verison is often used to update RegSSAInfo incrementally.
    //m_max_version.destroy();
    //m_max_version.init();
}


//This function dumps VReg structure and SSA DU info.
void RegSSAMgr::dumpAllVReg() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    Region const* rg = getRegion();
    note(rg, "\n\n==---- DUMP RegSSAMgr: ALL VReg '%s'----==",
         rg->getRegionName());
    VROpndVec * vec = const_cast<RegSSAMgr*>(this)->getRegDUMgr()->
        getVROpndVec();
    xcom::DefFixedStrBuf tmp;
    LinearScanRA const* ra = getRA();
    VRegFixedStrBuf buf;
    for (VecIdx i = 1; i <= vec->get_last_idx(); i++) {
        VReg * v = (VReg*)vec->get(i);
        if (v == nullptr) {
            //Some pass, e.g:ir_refinement, will remove ir and related VReg.
            continue;
        }
        buf.clean();
        note(rg, "\nVReg%u:%s: ", v->id(),
             VReg::dumpRegAndVer(buf, v->reg(), v->version(), ra));
        RegDef * regdef = v->getDef();
        //Print DEF.
        if (v->version() != REGSSA_INIT_VERSION) {
            //After renaming, Reg must have defstmt if its version is nonzero.
            ASSERT0(regdef);
        }
        if (regdef != nullptr) {
            if (regdef->is_phi()) {
                ASSERT0(regdef->getBB());
                prt(rg, "DEF:(phi,BB%u)", regdef->getBB()->id());
            } else {
                IR const* stmt = regdef->getOcc();
                ASSERT0(stmt);
                //The stmt may have been removed, and the VReg is obsoleted.
                //If the stmt removed, its UseSet should be empty.
                //ASSERT0(stmt->is_stmt() && !stmt->isWritePR());
                prt(rg, "DEF:(%s)", xoc::dumpIRName(stmt, tmp));
            }
        } else {
            prt(rg, "DEF:---");
        }

        //Print USEs.
        prt(rg, "\tUSE:");
        VReg::UseSetIter it;
        BSIdx nexti = 0;
        for (BSIdx j = v->getUseSet()->get_first(it); !it.end(); j = nexti) {
            nexti = v->getUseSet()->get_next(it);
            IR * use = m_rg->getIR(j);
            ASSERT0(use && !use->isReadPR());
            prt(rg, "(%s)", xoc::dumpIRName(use, tmp));
            if (nexti != BS_UNDEF) {
                prt(rg, ",");
            }
        }
    }
    note(rg, "\n");
}


//Before removing BB or change BB successor,
//you need remove the related PHI operand if BB successor has PHI.
void RegSSAMgr::removeSuccessorDesignatedPhiOpnd(
    IRBB const* succ, UINT pos, RegSSAUpdateCtx const& ctx)
{
    RegPhiList * philist = getPhiList(succ->id());
    if (philist == nullptr) { return; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        //CASE:CFG optimization may have already remove the predecessor of
        //'succ' before call the function.
        //ASSERT0(phi->getOpndNum() == succ->getNumOfPred());
        if (phi->getOpndList() == nullptr) {
            //RegPHI does not contain any operand.
            continue;
        }
        IR * opnd = phi->getOpnd(pos);
        removeRegSSAOccForTree(opnd, ctx);
        phi->removeOpnd(opnd);
        ctx.tryInvalidInfoBeforeFreeIR(opnd);
        m_rg->freeIRTree(opnd);
    }
}


bool RegSSAMgr::isAlias(IR const* ir, Reg reg2) const
{
    ASSERT0(ir->is_exp() || ir->is_stmt());
    Reg reg1 = getReg(ir);
    if (reg1 == REG_UNDEF) { return false; }
    return isAlias(reg1, reg2);
}


bool RegSSAMgr::isAlias(Reg reg1, Reg reg2) const
{
    return getTIMgr()->isAlias(reg1, reg2);
}


bool RegSSAMgr::isExactCover(Reg reg1, Reg reg2) const
{
    return getTIMgr()->isExactCover(reg1, reg2);
}


IR * RegSSAMgr::findUniqueDefInLoopForMustRef(
    IR const* exp, LI<IRBB> const* li, Region const* rg,
    OUT IRSet * set) const
{
    ASSERT0(exp && exp->is_exp() && hasExpRegSSAInfo(exp));
    Reg mustuse = getReg(exp);
    if (mustuse == REG_UNDEF) { return nullptr; }
    RegSSAInfo * regssainfo = getRegSSAInfoIfAny(exp);
    ASSERT0(regssainfo);
    xcom::DefMiscBitSetMgr sbsmgr;
    IRSet tmpset(sbsmgr.getSegMgr());
    if (set == nullptr) { set = &tmpset; }
    VRCollectCtx ctx(COLLECT_CROSS_PHI|COLLECT_INSIDE_LOOP);
    ctx.setLI(li);
    VRCollectDef cd(this, regssainfo, ctx, mustuse, set);
    if (set->get_elem_count() == 1) {
        IRSetIter it;
        return m_rg->getIR(set->get_first(&it));
    }
    return nullptr;
}


//The function try to find the unique RegDef for given def that is outside
//of the loop.
//Return the RegDef if found, otherwise nullptr.
RegDef const* RegSSAMgr::findUniqueOutsideLoopDef(
    RegDef const* phi, LI<IRBB> const* li) const
{
    ASSERT0(phi->is_phi());
    UINT num_outside_def = 0;
    RegDef const* ret = nullptr;
    for (IR const* opnd = REGPHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        VReg * opndvreg = ((RegPhi*)phi)->getOpndVReg(opnd,
            const_cast<RegSSAMgr*>(this)->getRegDUMgr());
        RegDef const* def = opndvreg->getDef();
        if (def == nullptr) { continue; }
        ASSERT0(def->getBB());
        if (li->isInsideLoop(def->getBB()->id())) { continue; }
        ret = def;
        num_outside_def++;
        if (num_outside_def > 1) { return nullptr; }
    }
    return ret;
}


//Find VReg from ir list and phi list.
VReg * RegSSAMgr::findLastMayDef(IRBB const* bb, Reg reg) const
{
    return findLastMayDefFrom(bb, BB_last_ir(const_cast<IRBB*>(bb)), reg);
}


VReg * RegSSAMgr::findVRegFromPhiList(IRBB const* bb, Reg reg) const
{
    RegPhiList * philist = getPhiList(bb->id());
    if (philist == nullptr) { return nullptr; }
    for (RegPhiListIter pit = philist->get_head();
         pit != philist->end(); pit = philist->get_next(pit)) {
        RegPhi * phi = pit->val();
        ASSERT0(phi && phi->is_phi());
        VReg * res = phi->getResult();
        ASSERT0(res);
        if (res->reg() == reg) { return res; }
    }
    return nullptr;
}


//Find VReg from ir list and phi list.
//start: the start position, if it is NULL, the function will scan the phi list.
VReg * RegSSAMgr::findLastMayDefFrom(
    IRBB const* bb, IR const* start, Reg reg) const
{
    if (start != nullptr) {
        BBIRListIter it = nullptr;
        BBIRList const& irlist = const_cast<IRBB*>(bb)->getIRList();
        irlist.find(const_cast<IR*>(start), &it);
        ASSERTN(it, ("IR%u is not belong to BB%u", start->id(), bb->id()));
        for (; it != nullptr; it = irlist.get_prev(it)) {
            IR const* ir = it->val();
            VReg * vreg;
            if (hasRegSSAInfo(ir) &&
                (vreg = findMayRef(ir, reg)) != nullptr) {
                return vreg;
            }
        }
    }
    return findVRegFromPhiList(bb, reg);
}


//The function does searching that begin at the IDom BB of marker.
//Note DOM info must be available.
VReg * RegSSAMgr::findDomLiveInDefFromIDomOf(
    IRBB const* marker, Reg reg, OptCtx const& oc,
    OUT RegSSAStatus & st) const
{
    UINT idom = ((DGraph*)m_cfg)->get_idom(marker->id());
    if (idom == VERTEX_UNDEF) { return nullptr; }
    ASSERT0(m_cfg->isVertex(idom));
    IRBB * bb = m_cfg->getBB(idom);
    ASSERT0(bb);
    return findDomLiveInDefFrom(reg, bb->getLastIR(), bb, oc, st);
}


VReg * RegSSAMgr::findLiveInDefFrom(
    Reg reg, IRBB const* bb, IR const* startir, IRBB const* startbb) const
{
    if (bb == startbb) {
        if (startir != nullptr) {
            return findLastMayDefFrom(bb, startir, reg);
        }
        return findVRegFromPhiList(bb, reg);
    }
    return findLastMayDef(bb, reg);
}


VReg * RegSSAMgr::findDomLiveInDefFrom(
    Reg reg, IR const* startir, IRBB const* startbb, OptCtx const& oc,
    OUT RegSSAStatus & st) const
{
    ASSERT0(startbb);

    //NOTE startir may be have already removed from startbb. For this case,
    //we have to trust that caller passed in right parameters.
    //ASSERT0(startir == nullptr ||
    //        const_cast<IRBB*>(startbb)->getIRList()->find(
    //        const_cast<IR*>(startir)));
    IRBB const* meetup = m_cfg->getEntry();
    ASSERT0(meetup);
    IRBB * idom = nullptr;
    for (IRBB const* t = startbb; t != nullptr; t = idom) {
        UINT idomidx = ((DGraph*)m_cfg)->get_idom(t->id());
        if (idomidx == VERTEX_UNDEF) {
            idom = nullptr;
        } else {
            ASSERTN(m_cfg->isVertex(idomidx), ("miss DomInfo"));
            idom = m_cfg->getBB(idomidx);
            ASSERT0(idom);
        }
        VReg * livein = findLiveInDefFrom(reg, t, startir, startbb);
        if (livein != nullptr) { return livein; }
        if (t == meetup) { continue; }
        if (!oc.is_dom_valid()) {
            //In the middle stage of optimization, e.g DCE, pass may transform
            //the CFG into a legal but insane CFG. In the case, the CFG
            //may be divided into serveral isolated parts. Thus there is no
            //livein path from entry to current BB.
            //Note if DOM info is not maintained, SSA update can not prove
            //to be correct. However, for now we keep doing the update to
            //maintain PHI's operand in order to tolerate subsequently
            //processing of CFG.
            st.set(REGSSA_STATUS_DOM_IS_INVALID);
            return nullptr;
        }
    }
    //DEF is Region-Livein-VReg of 'reg'.
    return nullptr;
}


void RegSSAMgr::addSuccessorDesignatedPhiOpnd(
    IRBB * bb, IRBB * succ, OptCtx const& oc, OUT RegSSAStatus & st)
{
    RegPhiList * philist = getPhiList(succ->id());
    if (philist == nullptr) { return; }
    bool is_pred;
    UINT const pos = m_cfg->WhichPred(bb, succ, is_pred);
    ASSERT0(is_pred);
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        RegSSAStatus lst;
        phi->insertOpndAt(this, pos, bb, oc, lst);
        st.bunion(lst);
        ASSERT0(phi->getOpndNum() == succ->getNumOfPred());
    }
}


void RegSSAMgr::dumpPhiList(RegPhiList const* philist) const
{
    if (philist == nullptr) { return; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi const* phi = it->val();
        ASSERT0(phi && phi->is_phi());
        note(getRegion(), "\n");
        phi->dump(m_rg, &m_dumgr);
    }
}


static void dumpRegSSAInfo(IR const* ir, RegSSAMgr const* mgr)
{
    Region const* rg = mgr->getRegion();
    RegSSAInfo * regssainfo = mgr->getRegSSAInfoIfAny(ir);
    RegDUMgr * dumgr = const_cast<RegSSAMgr*>(mgr)->getRegDUMgr();
    if (regssainfo == nullptr) {
        prt(rg, " %s", g_msg_no_regssainfo);
        return;
    }
    if (regssainfo->isEmptyVROpndSet()) {
        if (ir->isCallStmt()) {
            //CallStmt may be readonly.
            prt(rg, " %s", g_msg_empty_vropndset);
        } else {
            prt(rg, " %s", g_msg_no_regssainfo);
        }
        return;
    }
    VROpndSetIter iter = nullptr;
    for (BSIdx i = regssainfo->getVROpndSet()->get_first(&iter);
        i != BS_UNDEF; i = regssainfo->getVROpndSet()->get_next(i, &iter)) {
        note(rg, "\n--DEF:");
        VReg * vr = (VReg*)dumgr->getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (vr->getDef() != nullptr) {
            ASSERT0(vr->getDef()->getOcc() == ir);
        }
        vr->dump(rg, dumgr);
    }
}


void RegSSAMgr::dumpRegSSAInfoForStmt(
    OUT xcom::DefFixedStrBuf & buf, IR const* ir) const
{
    if (!ir->is_stmt() || !hasRegSSAInfo(ir)) { return; }
    class Dump : public xoc::DumpToBuf {
    public:
        IR const* ir;
        RegSSAMgr const* mgr;
    public:
        Dump(Region const* rg, xcom::StrBuf & buf, UINT indent)
            : DumpToBuf(rg, buf, indent) {}
        virtual void dumpUserInfo() const override
        { dumpRegSSAInfo(ir, mgr); }
    };
    xcom::StrBuf tmpbuf(32);
    Dump d(getRegion(), tmpbuf, DUMP_INDENT_NUM);
    d.ir = ir;
    d.mgr = this;
    d.dump();
    buf.strcat(tmpbuf.getBuf());
}


void RegSSAMgr::dumpIRWithRegSSAForStmt(IR const* ir) const
{
    if (!ir->is_stmt() || !hasRegSSAInfo(ir)) { return; }
    note(getRegion(), "\n----");
    dumpIR(ir, m_rg, nullptr, DumpFlag::combineIRID(IR_DUMP_DEF));
    ir->dumpRefOnly(m_rg);

    RegSSAInfo * regssainfo = getRegSSAInfoIfAny(ir);
    if (regssainfo == nullptr) {
        //Miss RegSSAInfo.
        note(getRegion(), "\n%s", g_msg_no_regssainfo);
        return;
    }
    if (regssainfo->isEmptyVROpndSet()) {
        if (ir->isCallStmt()) {
            //CallStmt may be readonly.
            note(getRegion(), "\n%s", g_msg_empty_vropndset);
        } else {
            note(getRegion(), "\n%s", g_msg_no_regssainfo);
        }
        return;
    }
    //Dump VRegSet.
    VROpndSetIter iter = nullptr;
    for (BSIdx i = regssainfo->getVROpndSet()->get_first(&iter);
        i != BS_UNDEF; i = regssainfo->getVROpndSet()->get_next(i, &iter)) {
        note(getRegion(), "\n--DEF:");
        VReg * vr = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (vr->getDef() != nullptr) {
            ASSERT0(vr->getDef()->getOcc() == ir);
        }
        vr->dump(m_rg, &m_dumgr);
    }
}


void RegSSAMgr::dumpRegSSAInfoForExp(
    OUT xcom::DefFixedStrBuf & buf, IR const* ir) const
{
    if (!ir->is_exp() || !hasExpRegSSAInfo(ir)) { return; }
    LinearScanRA const* ra = getRA();
    VROpndSetIter iter = nullptr;
    buf.strcat(" --USE:");
    bool first = true;
    RegSSAInfo * regssainfo = getRegSSAInfoIfAny(ir);
    if (regssainfo == nullptr || regssainfo->isEmptyVROpndSet()) {
        //Miss RegSSAInfo.
        buf.strcat(" %s", g_msg_no_regssainfo);
        return;
    }
    //Dump VRegSet.
    VRegFixedStrBuf tmpbuf;
    for (BSIdx i = regssainfo->getVROpndSet()->get_first(&iter);
         i != BS_UNDEF; i = regssainfo->getVROpndSet()->get_next(i, &iter)) {
        VReg const* vr = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (first) { first = false; }
        else { buf.strcat(","); }
        tmpbuf.clean();
        buf.strcat("VReg%u:%s", vr->id(),
            VReg::dumpRegAndVer(tmpbuf, vr->reg(), vr->version(), ra));
    }
}


void RegSSAMgr::dumpIRWithRegSSAForExpTree(IR const* ir) const
{
    if (ir->is_undef()) {
        //There might be some error occurred.
        //Do not assert and keep dumpping.
        return;
    }
    List<IR const*> lst;
    List<IR const*> opnd_lst;
    Region const* rg = getRegion();
    LinearScanRA const* ra = getRA();
    VRegFixedStrBuf buf;
    for (IR const* opnd = iterExpInitC(ir, lst);
         opnd != nullptr; opnd = iterExpNextC(lst)) {
        ASSERT0(opnd->is_exp());
        if (!hasExpRegSSAInfo(opnd)) {
            continue;
        }
        VROpndSetIter iter = nullptr;
        dumpIR(opnd, m_rg, nullptr, DumpFlag::combineIRID(IR_DUMP_DEF));
        opnd->dumpRefOnly(m_rg);

        note(rg, "\n--USE:");
        bool first = true;
        RegSSAInfo * regssainfo = getRegSSAInfoIfAny(opnd);
        if (regssainfo == nullptr || regssainfo->isEmptyVROpndSet()) {
            prt(rg, "%s", g_msg_no_regssainfo);
            continue;
        }

        for (BSIdx i = regssainfo->getVROpndSet()->get_first(&iter);
             i != BS_UNDEF;
             i = regssainfo->getVROpndSet()->get_next(i, &iter)) {
            VReg * vr = (VReg*)m_dumgr.getVROpnd(i);
            ASSERT0(vr && vr->is_reg());
            if (first) {
                first = false;
            } else {
                prt(rg, ",");
            }
            buf.clean();
            prt(rg, "VReg%u:%s", vr->id(),
                VReg::dumpRegAndVer(buf, vr->reg(), vr->version(), ra));
        }
    }
}


void RegSSAMgr::dumpIRWithRegSSA(IR const* ir, MOD IRDumpCtx<> * ctx) const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    ASSERT0(ir);
    xoc::dumpIR(ir, m_rg, *ctx);
    LocalRegSSADump::dumpIRWithRegSSAInfo(this, m_rg, ir);
}


//Dump IR tree and RegSSAInfo if any.
//ir: can be stmt or expression.
//flag: the flag to dump IR.
void RegSSAMgr::dumpIRWithRegSSA(IR const* ir, DumpFlag flag) const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    ASSERT0(ir);
    xoc::dumpIR(ir, m_rg, nullptr, flag);
    LocalRegSSADump::dumpIRWithRegSSAInfo(this, m_rg, ir);
}


void RegSSAMgr::dumpBBList() const
{
    note(getRegion(), "\n==---- DUMP BBLIST WITH RegSSA '%s' ----==",
         m_rg->getRegionName());
    getRegion()->getLogMgr()->incIndent(2);
    dumpVROpndRef();
    getRegion()->getLogMgr()->decIndent(2);
}


void RegSSAMgr::dumpVROpndRef() const
{
    Region * rg = getRegion();
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    note(rg, "\n==-- DUMP RegSSAMgr VROpndRef '%s' --==\n",
         rg->getRegionName());
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(rg, "\n--- BB%u ---", bb->id());
        dumpPhiList(getPhiList(bb->id()));
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            note(rg, "\n");
            dumpIRWithRegSSA(ir);
        }
    }
}


bool RegSSAMgr::dump() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) {
        return false;
    }
    START_TIMER(t, "RegSSA: Dump After Pass");
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    getRegion()->getLogMgr()->incIndent(2);
    ASSERT0(getTIMgr()->getRegDSystem());
    getTIMgr()->getRegDSystem()->dump();
    dumpVROpndRef();
    dumpDUChain();
    //dumpBBListWithRegSSAInfo();
    getRegion()->getLogMgr()->decIndent(2);
    END_TIMER(t, "RegSSA: Dump After Pass");
    return true;
}


//Find the VROpnd if 'ir' must OR may referenced 'reg'.
//Return the VReg if found.
VReg * RegSSAMgr::findMayRef(IR const* ir, Reg reg) const
{
    ASSERT0(ir->isMemRef());
    ASSERT0(ir->is_stmt() || ir->is_exp());
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(ir);
    ASSERTN(regssainfo, ("miss RegSSAInfo"));
    VROpndSetIter iter = nullptr;
    VROpndSet const& vropndset = regssainfo->readVROpndSet();
    for (BSIdx i = vropndset.get_first(&iter);
         i != BS_UNDEF; i = vropndset.get_next(i, &iter)) {
        VReg * t = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (t->reg() == reg) { return t; }
    }
    return nullptr;
}


//Find the MustDef of 'ir'.
RegDef * RegSSAMgr::findMustRegDef(IR const* ir) const
{
    ASSERT0(ir && hasRegSSAInfo(ir));
    Reg mustref = getReg(ir);
    if (mustref == REG_UNDEF) { return nullptr; }
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(ir);
    ASSERTN(regssainfo, ("miss RegSSAInfo"));
    VROpndSetIter iter = nullptr;
    for (BSIdx i = regssainfo->readVROpndSet().get_first(&iter);
         i != BS_UNDEF; i = regssainfo->readVROpndSet().get_next(i, &iter)) {
        VReg * t = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(t && t->is_reg());
        RegDef * tdef = t->getDef();
        if (tdef != nullptr && tdef->getResultReg() == mustref) {
            return tdef;
        }
    }
    return nullptr;
}


bool RegSSAMgr::hasDef(IR const* ir) const
{
    ASSERT0(ir->is_exp());
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(ir);
    ASSERTN(regssainfo, ("miss RegSSAInfo"));
    VROpndSetIter iter = nullptr;
    for (BSIdx i = regssainfo->readVROpndSet().get_first(&iter);
         i != BS_UNDEF; i = regssainfo->readVROpndSet().get_next(i, &iter)) {
        VReg * t = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(t && t->is_reg());
        RegDef * tdef = t->getDef();
        if (tdef != nullptr) { return true; }
    }
    return false;
}


static bool isRegionLiveInByRegDef(
    IR const* ir, RegDef const* def, xcom::TTab<RegDef const*> & visited,
    RegSSAMgr const* mgr)
{
    ASSERT0(ir && ir->is_exp() && RegSSAMgr::hasExpRegSSAInfo(ir));
    ASSERT0(def && !def->is_phi());
    if (visited.find(def)) { return true; }
    visited.append(def);
    IR const* defocc = def->getOcc();
    ASSERT0(defocc && defocc->is_stmt());
    Reg defocc_mustref = mgr->getReg(defocc);
    if (defocc_mustref == REG_UNDEF) {
        //CASE: the defocc May Def the Reg reference of 'ir'.
        //e.g: *p = 10; //The stmt may define Reg7.
        //     ... = g; //MustRef is Reg7.
        return false;
    }
    ASSERT0(ir != defocc);
    if (mgr->isDependent(ir, defocc)) {
        return false;
    }
    return true;
}


static bool isRegionLiveInByRegPhi(
    IR const* ir, RegPhi const* phi, xcom::TTab<RegDef const*> & visited,
    RegSSAMgr const* mgr)
{
    ASSERT0(ir && ir->is_exp() && RegSSAMgr::hasExpRegSSAInfo(ir));
    ASSERT0(phi && phi->is_phi());
    if (visited.find(phi)) { return true; }
    visited.append(phi);
    RegDUMgr const* udmgr = const_cast<RegSSAMgr*>(mgr)->getRegDUMgr();
    for (IR const* opnd = REGPHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        VReg * opndvreg = ((RegPhi*)phi)->getOpndVReg(opnd, udmgr);
        RegDef const* def = opndvreg->getDef();
        if (def == nullptr) { continue; }
        if (def->is_phi()) {
            if (!isRegionLiveInByRegPhi(ir, (RegPhi const*)def, visited, mgr)) {
                return false;
            }
            continue;
        }
        if (!isRegionLiveInByRegDef(ir, def, visited, mgr)) {
            return false;
        }
        continue;
    }
    return true;
}


bool RegSSAMgr::isRegionLiveIn(IR const* ir) const
{
    ASSERT0(ir && hasExpRegSSAInfo(ir) && ir->is_exp());
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(ir);
    ASSERTN(regssainfo, ("invalid RegSSA"));
    Reg mustref = getReg(ir);
    if (mustref == REG_UNDEF) { return regssainfo->isLiveInVROpndSet(this); }

    //Check the MustRef of 'ir' whether it is defined in all the paths that
    //begins at the region entry to current ir.
    VROpndSetIter it = nullptr;
    xcom::TTab<RegDef const*> visited;
    for (BSIdx i = regssainfo->readVROpndSet().get_first(&it);
        i != BS_UNDEF; i = regssainfo->readVROpndSet().get_next(i, &it)) {
        VReg const* vr = (VReg*)getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (vr->isLiveIn()) { continue; }
        RegDef const* regdef = vr->getDef();
        ASSERT0(regdef);
        if (regdef->is_phi()) {
            //CASE: Determine whether exist MayDef by crossing RegPhi.
            //e.g: t1 is region live-in.
            //  BB1:
            //  st d = ld t2; //d's Reg ref is {Reg11V1:Reg2V1}
            //  falsebr L1 ld i, 0;
            //   |   |
            //   |   V
            //   |  BB2:
            //   |  st e = #20; //e's Reg ref is {Reg12V1:Reg2V2}
            //   |  |
            //   V  V
            //  BB3:
            //  L1:
            //  RegPhi: Reg2V3 <- (Reg2V1 BB1), (Reg2V2 BB2)
            //  return ld t1; //t1's Reg ref is {Reg7V0:Reg2V3}
            if (!isRegionLiveInByRegPhi(
                    ir, (RegPhi const*)regdef, visited, this)) {
                return false;
            }
            continue;
        }
        if (!isRegionLiveInByRegDef(ir, regdef, visited, this)) {
            return false;
        }
        continue;
    }
    return true;
}


static bool hasMayDefUntilCoverDefByRegDef(
    RegDef const* regdef, RegDef const* coverregdef,
    xcom::TTab<RegDef const*> & visited, RegSSAMgr const* mgr)
{
    ASSERT0(regdef && !regdef->is_phi());
    ASSERT0(coverregdef);
    if (regdef == coverregdef) { return false; }
    if (visited.find(regdef)) { return false; }
    visited.append(regdef);
    IR const* coverregdef_occ = coverregdef->getOcc();
    ASSERT0(coverregdef_occ && RegSSAMgr::hasRegSSAInfo(coverregdef_occ));
    IR const* defocc = regdef->getOcc();
    ASSERT0(defocc);
    if (defocc == coverregdef_occ) { return false; }
    if (mgr->isDependent(coverregdef_occ, defocc)) {
        //An OCC may correspond to multiple VReg, namely RegDef.
        //e.g:st g = 0; VROpndSet is {Reg11V1, Reg2V3}
        return true;
    }
    //The MustRef is indepdent with 'coverregdef'.
    //NOTE: We have to traverse regdef's DefDef chain and keep conducting
    //the inspection.
    RegDef const* prevregdef = regdef->getPrev();
    if (prevregdef == nullptr) { return false; }
    if (prevregdef->is_phi()) {
        return hasMayDefUntilCoverDefByPhi(
            (RegPhi const*)prevregdef, coverregdef, visited, mgr);
    }
    return hasMayDefUntilCoverDefByRegDef(
        prevregdef, coverregdef, visited, mgr);
}


static bool hasMayDefUntilCoverDefByPhi(
    RegPhi const* phi, RegDef const* coverregdef,
    xcom::TTab<RegDef const*> & visited, RegSSAMgr const* mgr)
{
    ASSERT0(phi && phi->is_phi());
    if (visited.find(phi)) { return false; }
    visited.append(phi);
    RegDUMgr const* udmgr = const_cast<RegSSAMgr*>(mgr)->getRegDUMgr();
    for (IR const* opnd = REGPHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        VReg * opndvreg = ((RegPhi*)phi)->getOpndVReg(opnd, udmgr);
        RegDef const* regdef = opndvreg->getDef();
        if (regdef == nullptr) { continue; }
        if (regdef->is_phi()) {
            if (hasMayDefUntilCoverDefByPhi(
                    (RegPhi const*)regdef, coverregdef, visited, mgr)) {
                return true;
            }
            continue;
        }
        if (hasMayDefUntilCoverDefByRegDef(regdef, coverregdef, visited, mgr)) {
            return true;
        }
    }
    return false;
}


bool RegSSAMgr::canPhiReachRealRegDef(
    RegSSAInfo const* regssainfo, RegDef const* realdef) const
{
    ASSERT0(!realdef->is_phi());
    ASSERT0(regssainfo);
    VROpndSetIter it = nullptr;
    Reg realdef_reg = realdef->getResultReg();
    ASSERT0(realdef_reg != REG_UNDEF);
    xcom::TTab<RegDef const*> visited;
    visited.append(realdef);
    for (BSIdx i = regssainfo->readVROpndSet().get_first(&it);
         i != BS_UNDEF; i = regssainfo->readVROpndSet().get_next(i, &it)) {
        VReg const* t = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(t && t->is_reg());
        RegDef const* tregdef = t->getDef();
        if (tregdef == nullptr || tregdef == realdef) {
            //LiveIn Def or Meet realdef itself.
            continue;
        }
        if (!tregdef->is_phi()) {
            //Here, the tmpdef neither the nearest def nor the realdef.
            continue;
        }
        if (hasMayDefUntilCoverDefByPhi(
            (RegPhi const*)tregdef, realdef, visited, this)) {
            return false;
        }
    }
    return true;
}


//Return true if ir1's register may overlap to ir2's register.
//ir1: stmt or expression.
//ir2: stmt or expression.
bool RegSSAMgr::isDependent(IR const* ir1, IR const* ir2) const
{
    ASSERT0(hasRegSSAInfo(ir1) && hasRegSSAInfo(ir2));
    Reg reg1 = getReg(ir1);
    if (reg1 == REG_UNDEF) { return false; }
    Reg reg2 = getReg(ir2);
    if (reg2 == REG_UNDEF) { return false; }
    return isAlias(reg1, reg2);
}


RegDef const* RegSSAMgr::findNearestCoverDefThatCanReach(IR const* ir) const
{
    ASSERT0(ir && ir->is_exp() && hasExpRegSSAInfo(ir));
    Reg mustuse = getReg(ir);
    if (mustuse == REG_UNDEF) {
        //For those expressions who do not have MustRef, they do not have any
        //killing-def.
        return nullptr;
    }
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(ir);
    ASSERTN(regssainfo, ("RegSSAInfo is corrupt"));

    //There should not exist more than two cover-defs because they should
    //have been versioned.
    RegDef const* coverregdef = regssainfo->findCoverRegDef(this, mustuse);
    if (coverregdef == nullptr || coverregdef->is_phi()) {
        //If coverregdef is PHI, that means the value of ir is overlapped
        //with other stmts that more than one, and these stmts merged
        //by the PHI.
        //e.g: s.a = 1; #s.a is Reg19V1
        //     while (i) { s = ...; } #s is Reg19V3
        //     return s.a; //coverdef is PHI with Reg19V2
        return nullptr;
    }
    IR const* coverregdef_occ = coverregdef->getOcc();
    ASSERT0(coverregdef_occ);
    if (hasReg(coverregdef_occ)) {
        //For the conservative purpose, CoverDef that does not have MustRef
        //should not be killing-def of 'ir'.
        //CASE:compile.gr/no_classic_prdu/killingdef.gr
        return nullptr;
    }
    //The def is the potentail real-def of ir.
    RegDef const* nearest = findNearestDef(ir, false);
    if (!nearest->is_phi()) {
        if (coverregdef == nearest) {
            //The nearest DEF is the coverregdef.
            //e.g: s = ...;
            //     s.a = 10;
            //     ... = s.a;
            return coverregdef;
        }
        //There is at least one overlapped-def that is in the path from
        //'ir' to its cover-def.
        //e.g: s.a = 10;
        //     s = ...;
        //     ... = s.a;
        return nullptr;
    }
    if (canPhiReachRealRegDef(regssainfo, coverregdef)) {
        return coverregdef;
    }
    return nullptr;
}


//Find nearest virtual DEF in VROpndSet of 'ir'.
RegDef * RegSSAMgr::findNearestDef(
    IR const* ir, bool skip_independent_def) const
{
    ASSERT0(ir && ir->is_exp() && hasExpRegSSAInfo(ir));
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(ir);
    ASSERTN(regssainfo, ("miss RegSSAInfo"));
    VROpndSetIter iter = nullptr;
    INT lastrpo = RPO_UNDEF;
    RegDef * last = nullptr;

    //Iterate all VROpnd to find the nearest dependent RegDef.
    for (BSIdx i = regssainfo->readVROpndSet().get_first(&iter);
         i != BS_UNDEF; i = regssainfo->readVROpndSet().get_next(i, &iter)) {
        VReg * t = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(t && t->is_reg());
        RegDef * tdef = t->getDef();
        if (skip_independent_def && tdef != nullptr && !tdef->is_phi()) {
            IR const* occ = tdef->getOcc();
            ASSERTN(occ, ("non-phi RegDef must have OCC"));
            if (!isDependent(occ, ir)) {
                //occ is independent to 'ir'.
                continue;
            }
        }
        if (last == nullptr) {
            if (tdef != nullptr) {
                last = tdef;
                ASSERT0(tdef->getBB());
                lastrpo = last->getBB()->rpo();
                ASSERT0(lastrpo != RPO_UNDEF);
            } else {
                ASSERT0(t->isLiveIn());
                //Regard the virtual def at the entry of region,
                //it is the farmost def.
            }
            continue;
        }

        if (tdef == nullptr) { continue; }

        ASSERT0(tdef->getResult() && tdef->getResult()->is_reg());

        IRBB * tbb = tdef->getBB();
        ASSERT0(tbb);
        ASSERT0(tbb->rpo() != RPO_UNDEF);
        if (tbb->rpo() > lastrpo) {
            //tdef is near more than 'last', then update 'last'.
            last = tdef;
            //Update nearest BB's rpo.
            lastrpo = tbb->rpo();
            continue;
        }
        if (tbb != last->getBB()) {
            //last is near more than tdef, so nothing to do.
            continue;
        }

        //tdef' and 'last' are placed in same BB.
        if (tdef->is_phi()) {
            ; //last is near more than tdef, so nothing to do.
            if (tdef->getResult()->reg() == last->getResult()->reg()) {
                ASSERT0(tdef == last || !last->is_phi());
            }
            continue;
        }
        if (last->is_phi()) {
            if (tdef->getResult()->reg() == last->getResult()->reg()) {
                ASSERT0(tdef == last || !tdef->is_phi());
            }

            //tdef is near more than 'last', then update 'last'.
            last = tdef;
            ASSERTN(lastrpo == tbb->rpo(), ("lastrpo should be updated"));
            continue;
        }
        if (tbb->is_dom(last->getOcc(), tdef->getOcc(), true)) {
            //tdef is near more than 'last', then update 'last'.
            last = tdef;
        }
    }
    return last;
}


IR * RegSSAMgr::findKillingDefStmt(IR const* ir, bool aggressive) const
{
    RegDef const* regdef = nullptr;
    if (aggressive) {
        regdef = findNearestCoverDefThatCanReach(ir);
    } else {
        regdef = findKillingRegDef(ir);
    }
    if (regdef != nullptr && !regdef->is_phi()) {
        ASSERT0(regdef->getOcc());
        return regdef->getOcc();
    }
    return nullptr;
}


RegDef * RegSSAMgr::findKillingRegDef(IR const* ir) const
{
    ASSERT0(ir && ir->is_exp() && hasExpRegSSAInfo(ir));
    Reg opndreg = getReg(ir);
    if (opndreg == REG_UNDEF) {
        //TBD: For those exp who do not have MustRef, must they not
        //have killing-def?
        return nullptr;
    }
    RegDef * def = findNearestDef(ir, false);
    if (def == nullptr || def->is_phi()) { return nullptr; }
    ASSERT0(def->getOcc());
    return xoc::isKillingDef(def->getOcc(), ir, nullptr) ? def : nullptr;
}


static void dumpDef(
    RegDef const* def, Reg vropndreg, RegDUMgr const* mgr,
    Region * rg, xcom::BitSet & visited_def, MOD List<RegDef const*> & wl,
    MOD IRSet & visited, MOD bool & has_dump_something)
{
    if (def->is_phi()) {
        if (has_dump_something) {
            prt(rg, " ");
        }
        prt(rg, "(regphi%u)", def->id());
        has_dump_something = true;

        //Collect opnd of PHI to go forward to
        //retrieve corresponding DEFs.
        for (IR const* opnd = REGPHI_opnd_list(def);
             opnd != nullptr; opnd = opnd->get_next()) {
            if (opnd->is_const()) {
                //CONST does not have VReg info.
                continue;
            }

            VReg * opndvreg = ((RegPhi*)def)->getOpndVReg(opnd, mgr);

            //CASE:Do NOT assert VROpnd here.
            //  sometime VROpnd of ID will be NULL before reconstruction.
            //  st x_1 = ...     st x_2 = ...
            //      \           /
            //      x_3 = phi(x_1, x_2)
            //  If some pass removed 'st x_1', the PHI will be
            //       x_3 = phi(--, x_2)
            //  where the first operand is missed, and the illegal PHI will
            //  be recomputed until RegSSA is reconstructed.
            //  The situation will be checked during verifyPhi().
            //  Thus just omit the operand of PHI if it is NULL.
            //ASSERT0(opndvreg);

            if (opndvreg != nullptr && opndvreg->getDef() != nullptr &&
                !visited_def.is_contain(opndvreg->getDef()->id())) {
                //Keep walking previous DEF.
                wl.append_tail(opndvreg->getDef());
            }
        }
        return;
    }

    //def is normal IR stmt.
    ASSERT0(def->getOcc());
    if (!visited.find(def->getOcc())) {
        visited.append(def->getOcc());
        if (has_dump_something) {
            prt(rg, " ");
        }
        prt(rg, "(%s)", DumpIRName().dump(def->getOcc()));
        has_dump_something = true;
    }

    Reg defreg = mgr->getReg(def->getOcc());
    if (defreg != REG_UNDEF &&
        (defreg == vropndreg ||
         mgr->getRegSSAMgr()->isExactCover(defreg, vropndreg))) {
        //Stop iteration. def is killing may-def.
        return;
    }
    if (def->getPrev() != nullptr &&
        !visited_def.is_contain(def->getPrev()->id())) {
        //Keep walking previous DEF.
        wl.append_tail(def->getPrev());
    }
}


//The function dump all possible DEF of 'vropnd' by walking through the
//Def Chain.
void RegSSAMgr::dumpDefByWalkDefChain(
    List<RegDef const*> & wl, IRSet & visited, VReg const* vropnd) const
{
    if (vropnd->getDef() == nullptr) { return; }
    Reg vropndreg = vropnd->reg();
    ASSERT0(vropndreg != REG_UNDEF);
    wl.clean();
    wl.append_tail(vropnd->getDef());
    xcom::BitSet visited_def;
    bool has_dump_something = false;
    for (RegDef const* def = wl.remove_head();
         def != nullptr; def = wl.remove_head()) {
        visited_def.bunion(def->id());
        ASSERT0(def->getResult()->reg() == vropndreg);
        dumpDef(def, vropndreg, const_cast<RegSSAMgr*>(this)->getRegDUMgr(),
                m_rg, visited_def, wl, visited, has_dump_something);
    }
}


bool RegSSAMgr::hasPhiWithAllSameOperand(IRBB const* bb) const
{
    RegPhiList * philist = getPhiList(bb->id());
    if (philist == nullptr) { return true; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi);
        IR const* first_opnd = phi->getOpndList();
        for (IR const* opnd = first_opnd->get_next();
             opnd != nullptr; opnd = opnd->get_next()) {
            if (first_opnd->isIREqual(opnd, getIRMgr(), false)) { continue; }
            return false;
        }
    }
    return true;
}


//Return true if exist USE to 'ir'.
//This is a helper function to provid simple query, an example to
//show how to retrieval VROpnd and USE occurences as well.
//ir: stmt
bool RegSSAMgr::hasUse(IR const* ir) const
{
    ASSERT0(ir && ir->is_stmt());
    RegSSAInfo const* info = const_cast<RegSSAMgr*>(this)->
        getRegSSAInfoIfAny(ir);
    if (info == nullptr || info->isEmptyVROpndSet()) { return false; }
    VROpndSetIter iter = nullptr;
    for (BSIdx i = info->readVROpndSet().get_first(&iter);
         i != BS_UNDEF; i = info->readVROpndSet().get_next(i, &iter)) {
        VROpnd const* vr = getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (((VReg*)vr)->hasUse()) {
            return true;
        }
    }
    return false;
}


bool RegSSAMgr::constructDesignatedRegion(MOD SSARegion & ssarg)
{
    START_TIMER(t, "RegSSA: Construct Designated Region");
    ASSERT0(ssarg.verify());
    xcom::Vertex const* rootv = m_cfg->getVertex(ssarg.getRootBB()->id());
    ASSERT0(rootv);
    xcom::VexTab vextab;
    vextab.add(rootv);
    BBSet const& bbset = ssarg.getBBSet();
    BBSetIter it;
    for (BSIdx i = bbset.get_first(&it);
         i != BS_UNDEF; i = bbset.get_next(i, &it)) {
        vextab.append(i);
    }
    ReconstructRegSSAVF vf(vextab, ssarg.getDomTree(), m_cfg,
                           this, ssarg.getOptCtx(), ssarg.getActMgr());
    ReconstructRegSSA recon(ssarg.getDomTree(), rootv, vf);
    recon.reconstruct();
    ASSERT0(RegSSAMgr::verifyRegSSAInfo(m_rg, *ssarg.getOptCtx()));
    END_TIMER(t, "RegSSA: Construct Designated Region");
    return true;
}


//lst: for local used.
void RegSSAMgr::dumpExpDUChainIter(
    IR const* ir, MOD ConstIRIter & it, OUT bool * parting_line) const
{
    IRSet visited(getSBSMgr()->getSegMgr());
    xcom::List<RegDef const*> wl;
    it.clean();
    xcom::DefFixedStrBuf tmp;
    Region const* rg = getRegion();
    LinearScanRA const* ra = getRA();
    VRegFixedStrBuf buf;
    for (IR const* opnd = xoc::iterInitC(const_cast<IR*>(ir), it);
         opnd != nullptr; opnd = xoc::iterNextC(it)) {
        if (opnd->is_stmt() || !hasExpRegSSAInfo(opnd)) { continue; }
        VROpndSetIter iter = nullptr;
        if (!(*parting_line)) {
            note(rg, "\n%s", g_parting_line_char);
            (*parting_line) = true;
        }
        note(rg, "\n");
        prt(rg, "%s", xoc::dumpIRName(opnd, tmp));

        RegSSAInfo * regssainfo = getRegSSAInfoIfAny(opnd);
        if (regssainfo == nullptr || regssainfo->isEmptyVROpndSet()) {
            //NonPR-MemRef Expression must have RegSSAInfo.
            prt(rg, g_msg_no_regssainfo);
            continue;
        }
        RegDef * kdef = findKillingRegDef(opnd);
        if (kdef != nullptr) {
            prt(rg, " KDEF:%s", xoc::dumpIRName(kdef->getOcc(), tmp));
        }

        //Not found killing def, thus dump total define-chain.
        //Define-chain represents the may-def list.
        m_rg->getLogMgr()->incIndent(2);
        note(rg, "\nDEFSET:");
        visited.clean();
        m_rg->getLogMgr()->incIndent(2);
        for (BSIdx i = regssainfo->getVROpndSet()->get_first(&iter);
             i != BS_UNDEF;
             i = regssainfo->getVROpndSet()->get_next(i, &iter)) {
            VReg * vr = (VReg*)m_dumgr.getVROpnd(i);
            ASSERT0(vr && vr->is_reg());
            buf.clean();
            note(rg, "\nVReg%u:%s:", vr->id(),
                 VReg::dumpRegAndVer(buf, vr->reg(), vr->version(), ra));
            dumpDefByWalkDefChain(wl, visited, vr);
        }
        m_rg->getLogMgr()->decIndent(2);
        m_rg->getLogMgr()->decIndent(2);
    }
}


//Dump all USE.
static void dumpUseSet(VReg const* vreg, Region * rg)
{
    ASSERT0(vreg);
    VReg::UseSetIter vit;
    xcom::StrBuf tmp(8);
    for (BSIdx i = const_cast<VReg*>(vreg)->getUseSet()->get_first(vit);
         !vit.end(); i = const_cast<VReg*>(vreg)->getUseSet()->get_next(vit)) {
        IR const* use = rg->getIR(i);
        ASSERT0(use && RegSSAMgr::hasExpRegSSAInfo(use));
        prt(rg, "(%s) ", dumpIRName(use, tmp));
    }
}


void RegSSAMgr::dumpDUChainForStmt(IR const* ir, bool & parting_line) const
{
    ASSERT0(ir->is_stmt());
    ASSERT0(hasRegSSAInfo(ir));
    Region * rg = getRegion();
    if (!parting_line) {
        note(rg, "\n%s", g_parting_line_char);
        parting_line = true;
    }
    note(rg, "\n");
    prt(rg, "%s", DumpIRName().dump(ir));
    RegSSAMgr * pmgr = const_cast<RegSSAMgr*>(this);
    RegSSAInfo * regssainfo = pmgr->getRegSSAInfoIfAny(ir);
    if (regssainfo == nullptr) {
        prt(rg, g_msg_no_regssainfo);
        return;
    }
    if (regssainfo->isEmptyVROpndSet()) {
        if (ir->isCallStmt()) {
            //CallStmt may be readonly.
            prt(rg, g_msg_empty_vropndset);
        } else {
            prt(rg, g_msg_no_regssainfo);
        }
        return;
    }
    //Dump VROpnd and the USE List for each VROpnd.
    rg->getLogMgr()->incIndent(2);
    note(rg, "\nUSE:");
    rg->getLogMgr()->incIndent(2);
    VROpndSetIter iter = nullptr;
    VRegFixedStrBuf buf;
    LinearScanRA const* ra = getRA();
    for (BSIdx i = regssainfo->getVROpndSet()->get_first(&iter);
         i != BS_UNDEF; i = regssainfo->getVROpndSet()->get_next(i, &iter)) {
        VReg * vr = (VReg*)pmgr->getRegDUMgr()->getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (vr->getDef() != nullptr) {
            ASSERT0(vr->getDef()->getOcc() == ir);
        }
        buf.clean();
        note(rg, "\nVReg%u:%s:", vr->id(),
             VReg::dumpRegAndVer(buf, vr->reg(), vr->version(), ra));

        //Dump all USE.
        dumpUseSet(vr, rg);
    }
    rg->getLogMgr()->decIndent(2);
    rg->getLogMgr()->decIndent(2);
}


void RegSSAMgr::dumpDUChainForStmt(IR const* ir, MOD ConstIRIter & it) const
{
    ASSERT0(ir->is_stmt());
    dumpIR(ir, getRegion());
    getRegion()->getLogMgr()->incIndent(2);
    bool parting_line = false;
    //Handle stmt.
    if (hasRegSSAInfo(ir)) {
        dumpDUChainForStmt(ir, parting_line);
    }
    //Handle expression.
    dumpExpDUChainIter(ir, it, &parting_line);
    if (parting_line) {
        note(getRegion(), "\n%s", g_parting_line_char);
        note(getRegion(), "\n");
    }
    getRegion()->getLogMgr()->decIndent(2);
}


void RegSSAMgr::dumpDUChain() const
{
    Region * rg = getRegion();
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpRegSSAMgr()) { return; }
    note(rg, "\n==-- DUMP RegSSAMgr DU CHAIN '%s' --==\n", rg->getRegionName());
    BBList * bbl = rg->getBBList();
    ConstIRIter it;
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        bb->dumpDigest(rg);
        dumpPhiList(getPhiList(bb->id()));
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            dumpDUChainForStmt(ir, it);
        }
    }
}


void RegSSAMgr::setNewVesionVReg(IR * ir, OUT RegSSAInfo * regssainfo)
{
    ASSERT0(regssainfo && regssainfo == getRegSSAInfoIfAny(ir));

    //Generate new VReg according to Reg reference.
    Reg ref = getReg(ir);
    if (ref == REG_UNDEF) { return; }

    //ir may be CallStmt, thus its result is PR.
    Reg mustreg = ref;
    VReg * vreg = genNewVersionVReg(ref);
    ASSERT0(getSBSMgr());
    ASSERT0(vreg->getDef() == nullptr);
    VREG_def(vreg) = genRegDefStmt(ir, vreg);
    regssainfo->addVROpnd(vreg, getRegDUMgr());

    //Set version for alias register set.
    SRegSet const* refset = getAliasRegSet(ref);
    if (refset == nullptr) { return; }
    SRegSetIter it;
    for (BSIdx i = refset->get_first(&it);
         i != BS_UNDEF; i = refset->get_next(i, &it)) {
        Reg r = (Reg)i;
        if (r == mustreg) { continue; }
        ASSERTN(r != REG_UNDEF, ("PR should not in MaySet"));
        VReg * v = genNewVersionVReg(i);
        ASSERT0(getSBSMgr());
        if (ir->is_stmt()) {
            VREG_def(v) = genRegDefStmt(ir, v);
        }
        regssainfo->addVROpnd(v, getRegDUMgr());
    }
}


RegSSAInfo * RegSSAMgr::genRegSSAInfoAndSetNewVesionVReg(IR * ir)
{
    ASSERT0(ir && ir->is_stmt());
    //Note ir already has VROpndSet, keep it unchanged, then generate new one.
    RegSSAInfo * regssainfo = genRegSSAInfo(ir);
    regssainfo->cleanVROpndSet(getRegDUMgr());
    setNewVesionVReg(ir, regssainfo);
    return regssainfo;
}


void RegSSAMgr::setInitVersionVReg(
    IR const* ir, OUT RegSSAInfo * regssainfo)
{
    setDedicatedVersionVReg(ir, regssainfo, REGSSA_INIT_VERSION);
}


void RegSSAMgr::setInitVersionVReg(IR const* ir)
{
    ASSERT0(ir && hasRegSSAInfo(ir));
    RegSSAInfo * regssainfo = getRegSSAInfoIfAny(ir);
    ASSERT0(regssainfo);
    setDedicatedVersionVReg(ir, regssainfo, REGSSA_INIT_VERSION);
}


void RegSSAMgr::setInitVersionVReg(xcom::List<IR const*> const& lst)
{
    xcom::List<IR const*>::Iter it;
    for (IR const* ir = lst.get_head(&it);
         ir != nullptr; ir = lst.get_next(&it)) {
        setInitVersionVReg(ir);
    }
}


void RegSSAMgr::setDedicatedVersionVReg(
    IR const* ir, OUT RegSSAInfo * regssainfo, UINT version)
{
    ASSERT0(regssainfo && regssainfo == getRegSSAInfoIfAny(ir));
    Reg ref = getReg(ir);
    if (ref == REG_UNDEF) { return; }

    //ir may be Call stmt, its result is PR.
    VReg const* vreg = genVReg(ref, version);
    ASSERT0(getSBSMgr());
    regssainfo->addVROpnd(vreg, getRegDUMgr());

    //Set VReg for alias register set.
    SRegSet const* refset = getAliasRegSet(ref);
    if (refset == nullptr) { return; }
    SRegSetIter it;
    for (BSIdx i = refset->get_first(&it);
         i != BS_UNDEF; i = refset->get_next(i, &it)) {
        Reg reg = (Reg)i;
        ASSERTN(reg != REG_UNDEF, ("PR should not in MaySet"));
        VReg const* vreg = genVReg(i, version);
        ASSERT0(getSBSMgr());
        regssainfo->addVROpnd(vreg, getRegDUMgr());
    }
}


//Generate RegSSAInfo and generate VROpnd for referrenced Reg that both include
//MustRef Reg and MayRef Regs.
RegSSAInfo * RegSSAMgr::genRegSSAInfoAndSetDedicatedVersionVReg(
    IR * ir, UINT version)
{
    ASSERT0(ir);
    RegSSAInfo * regssainfo = genRegSSAInfo(ir);
    setDedicatedVersionVReg(ir, regssainfo, version);
    return regssainfo;
}


//Return true if all vropnds of 'def' can reach 'exp'.
bool RegSSAMgr::isMustDef(IR const* def, IR const* exp) const
{
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(def);
    return regssainfo != nullptr && regssainfo->isMustDef(
        const_cast<RegSSAMgr*>(this)->getRegDUMgr(), exp);
}


//Return true if def1 dominates def2.
bool RegSSAMgr::isDom(RegDef const* def1, RegDef const* def2) const
{
    ASSERT0(def1 != def2);
    IRBB const* bb1 = def1->getBB();
    IRBB const* bb2 = def2->getBB();
    if (bb1 != bb2) { return m_cfg->is_dom(bb1->id(), bb2->id()); }
    if (def1->is_phi()) {
        if (def2->is_phi()) {
            //PHIs that are in same BB do not dominate each other.
            return false;
        }
        return true;
    }
    if (def2->is_phi()) { return false; }
    ASSERT0(def1->getOcc() && def2->getOcc());
    return bb1->is_dom(def1->getOcc(), def2->getOcc(), true);
}


//Generate VReg for stmt and its kid expressions that reference memory.
void RegSSAMgr::initVReg(IN IR * ir, OUT DefRegSet & maydef)
{
    ASSERT0(ir->is_stmt());
    if (hasRegSSAInfo(ir)) {
        Reg ref = getReg(ir);
        if (ref != REG_UNDEF) { //MustRef of CallStmt may be PR.
            maydef.bunion(ref);
            SRegSet const* refset = getAliasRegSet(ref);
            if (refset != nullptr) {
                maydef.bunion((DefSBitSet&)*refset);
            }
            genRegSSAInfoAndSetDedicatedVersionVReg(ir, REGSSA_INIT_VERSION);
        }
    }
    m_iter.clean();
    for (IR * t = xoc::iterExpInit(ir, m_iter);
         t != nullptr; t = xoc::iterExpNext(m_iter)) {
        ASSERT0(t->is_exp());
        if (!hasExpRegSSAInfo(t)) { continue; }
        Reg ref = getReg(t);
        if (ref == REG_UNDEF) {
            //There is no phsical-register info, regssainfo is dispensable.
            continue;
        }
        genRegSSAInfoAndSetDedicatedVersionVReg(t, REGSSA_INIT_VERSION);
    }
}


void RegSSAMgr::collectUseSet(
    IR const* def, LI<IRBB> const* li, VRCollectFlag f, OUT IRSet * useset)
{
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(def);
    ASSERT0(regssainfo);
    VRCollectCtx ctx(f);
    ctx.setLI(li);
    VRCollectUse cu(this, regssainfo, ctx, useset);
}


void RegSSAMgr::collectUseSet(
    IR const* def, VRCollectFlag f, OUT IRSet * useset)
{
    RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(def);
    ASSERT0(regssainfo);
    VRCollectCtx ctx(f);
    VRCollectUse cu(this, regssainfo, ctx, useset);
}


void RegSSAMgr::collectUseReg(IR const* ir, OUT LiveInRegTab & livein_reg)
{
    ASSERT0(ir);
    Reg ref = getReg(ir);
    if (ref == REG_UNDEF) { return; }

    //ir may be Call stmt, its result is PR.
    livein_reg.append(ref);
    SRegSet const* refset = getAliasRegSet(ref);
    if (refset == nullptr) { return; }
    SRegSetIter it;
    for (BSIdx i = refset->get_first(&it);
         i != BS_UNDEF; i = refset->get_next(i, &it)) {
        Reg reg = (Reg)i;
        ASSERTN(reg != REG_UNDEF, ("PR should not in MayBeSet"));
        livein_reg.append(reg);
    }
}


void RegSSAMgr::computeLiveInReg(IRBB const* bb, OUT LiveInRegTab & livein_reg)
{
    livein_reg.clean();
    ConstIRIter irit;
    IRBB * pbb = const_cast<IRBB*>(bb);
    for (IR * ir = BB_last_ir(pbb); ir != nullptr; ir = BB_prev_ir(pbb)) {
        //Handle Def.
        Reg exact_def = REG_UNDEF;
        IR const* res = ir->getResultPR();
        if (res != nullptr && (exact_def = getReg(ir)) != REG_UNDEF) {
            LiveInRegTabIter it;
            for (Reg reg = livein_reg.get_first(it);
                 reg != REG_UNDEF; reg = livein_reg.get_next(it)) {
                if (!isExactCover(exact_def, reg)) { continue; }
                //ir kills the value of reg.
                livein_reg.remove(exact_def);
            }
        }
        //Handle Use.
        irit.clean();
        for (IR const* t = iterExpInitC(ir, irit);
             t != nullptr; t = iterExpNextC(irit)) {
            ASSERT0(t->is_exp());
            if (!hasExpRegSSAInfo(t)) { continue; }
            collectUseReg(t, livein_reg);
        }
    }
}


//maydef_reg: record Regs that defined in 'bb'.
void RegSSAMgr::collectDefinedRegAndInitVReg(
    IN IRBB * bb, OUT DefRegSet & maydef)
{
    BBIRListIter it;
    for (IR * ir = bb->getIRList().get_head(&it);
         ir != nullptr; ir = bb->getIRList().get_next(&it)) {
        initVReg(ir, maydef);
    }
}


//Insert a new PHI into bb according to given Reg.
//Note the operand of PHI will be initialized in initial-version.
RegPhi * RegSSAMgr::insertPhiWithNewVersion(
    Reg reg, IN IRBB * bb, UINT num_opnd)
{
    RegPhi * phi = insertPhi(reg, bb, num_opnd);
    VReg * newres = genNewVersionVReg(reg);
    REGDEF_result(phi) = newres;
    VREG_def(newres) = phi;
    return phi;
}


//Insert a new PHI into bb according to given Reg.
//Note the operand of PHI will be initialized in initial-version.
RegPhi * RegSSAMgr::insertPhi(Reg reg, IN IRBB * bb, UINT num_opnd)
{
    //Here each operand and result of phi set to same type.
    //They will be revised to correct type during renaming.
    RegPhi * phi = genRegPhi(reg, num_opnd, bb, genInitVersionVReg(reg));
    m_dumgr.genBBPhiList(bb->id())->append_head(phi);
    return phi;
}


//Insert phi for VReg.
//defbbs: record BBs which defined the VReg identified by 'reg'.
//visited: record visited BB id
void RegSSAMgr::placePhiForReg(
    Reg reg, List<IRBB*> const* defbbs, DfMgr const& dfm,
    xcom::BitSet & visited, List<IRBB*> & wl, BB2DefRegSet & defregs_vec)
{
    ASSERT0(defbbs && reg != REG_UNDEF);
    visited.clean();
    wl.clean();
    C<IRBB*> * bbit;
    for (IRBB * defbb = defbbs->get_head(&bbit);
         defbb != nullptr; defbb = defbbs->get_next(&bbit)) {
        wl.append_tail(defbb);
        //visited.bunion(defbb->id());
    }

    while (wl.get_elem_count() != 0) {
        IRBB * bb = wl.remove_head();

        //Each basic block in dfcs is in dominance frontier of 'bb'.
        xcom::BitSet const* dfcs = dfm.getDFControlSet(bb->id());
        if (dfcs == nullptr) { continue; }

        for (BSIdx i = dfcs->get_first(); i != BS_UNDEF;
             i = dfcs->get_next(i)) {
            if (visited.is_contain(i)) {
                //Already insert phi for 'reg' into BB i.
                //TODO:ensure the phi for same PR does NOT be
                //inserted multiple times.
                continue;
            }

            visited.bunion(i);

            IRBB * ibb = m_cfg->getBB(i);
            ASSERT0(ibb);

            //Redundant phi will be removed during refinePhi().
            insertPhi(reg, ibb);

            ASSERT0(defregs_vec.get(i));
            defregs_vec.get(i)->bunion(reg);

            wl.append_tail(ibb);
        }
    }
}


bool RegSSAMgr::doOpndHaveSameDef(
    RegPhi const* phi, OUT VReg ** common_def) const
{
    VReg * opnddefvreg = nullptr;
    bool same_def = true; //indicate all DEF of operands are the same stmt.
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VReg * v = phi->getOpndVReg(opnd, &m_dumgr);
        if (v == nullptr) {
            //VROpnd may have been removed from RegSSAMgr, thus the VROpnd that
            //corresponding to the ID is NULL.
            continue;
        }
        ASSERT0(v->is_reg());
        if (opnddefvreg == nullptr) {
            opnddefvreg = v;
            continue;
        }
        if (v != opnddefvreg && v != phi->getResult()) {
            same_def = false;
            break;
        }
    }
    ASSERT0(common_def);
    *common_def = opnddefvreg;
    return same_def;
}


//Return true if one of phi's operand have valid DEF, otherwise return false.
//CASE1: if all opnds's DEF are invalid, then phi is redundant.
//If opnd of phi do not have any valid DEF, then phi is redundant,
//otherwise phi can NOT be removed even if there are only one
//opnd have valid DEF.
//e.g1:Phi: Reg14V2 <- Reg14V1, Reg14V3
//     Reg14V1 is valid, Reg14V3 is invalid, then Phi can not be removed.
//e.g2:dce6.c, after DCE, there are two BBs. Both PHI prepended at BB
//     are redundant becase their opnd do not have DEF, and it is invalid.
//  --- BB7 ---
//  --- BB5 ---
//  Phi: Reg14V2 <- Reg14V0(id:60)(BB7)|UsedBy:
//  Phi: Reg13V2 <- Reg13V1(id:58)(BB7)|UsedBy:
//  Phi: Reg10V4 <- Reg10V3(id:56)(BB7)|UsedBy:
//  Phi: Reg9V4 <- Reg9V3(id:54)(BB7)|UsedBy:
//  Phi: Reg7V4 <- Reg7V3(id:52)(BB7)|UsedBy:
//  starray (i8, ety:i8) id:32 attachinfo:Dbx,RegSSA
//  return id:47 attachinfo:Dbx
bool RegSSAMgr::doOpndHaveValidDef(RegPhi const* phi) const
{
    if (phi->hasNoOpnd()) { return false; }
    //Indicate if there exist a valid DEF for operands
    bool has_valid_def = false;
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VReg * v = phi->getOpndVReg(opnd, &m_dumgr);
        if (v == nullptr) { continue; }
        ASSERT0(v->is_reg());
        if (v->getDef() != nullptr) {
            has_valid_def = true;
            break;
        }
    }
    return has_valid_def;
}


void RegSSAMgr::recordEffectReg(IRBB const* bb, OUT DefRegSet & effect_reg)
{
    LiveInRegTab livein_reg;
    computeLiveInReg(bb, livein_reg);
    LiveInRegTabIter iter;
    for (Reg reg = livein_reg.get_first(iter);
         reg != REG_UNDEF; reg = livein_reg.get_next(iter)) {
        effect_reg.bunion(reg);
    }
}


void RegSSAMgr::placePhi(
    DfMgr const& dfm, OUT DefRegSet & effect_reg, DefMiscBitSetMgr & bs_mgr,
    BB2DefRegSet & defined_reg_vec, List<IRBB*> & wl)
{
    START_TIMER(t, "RegSSA: Place phi");

    //Record BBs which modified each Reg.
    BBList * bblst = m_rg->getBBList();

    //All objects allocated and recorded in reg2defbb are used for
    //local purpose, and will be destoied before leaving this function.
    Vector<List<IRBB*>*> reg2defbb(bblst->get_elem_count());
    for (IRBB * bb = bblst->get_head(); bb != nullptr; bb = bblst->get_next()) {
        DefSBitSet * bs = bs_mgr.allocSBitSet();
        defined_reg_vec.set(bb->id(), bs);
        collectDefinedRegAndInitVReg(bb, *bs);
        if (m_is_semi_pruned) {
            recordEffectReg(bb, effect_reg);
        } else {
            //Record all modified Regs which will be versioned later.
            effect_reg.bunion(*bs);
        }

        //Record which BB defined these effect regs.
        DefSBitSetIter cur = nullptr;
        for (BSIdx i = bs->get_first(&cur); i != BS_UNDEF;
             i = bs->get_next(i, &cur)) {
            List<IRBB*> * bbs = reg2defbb.get(i);
            if (bbs == nullptr) {
                bbs = new List<IRBB*>();
                reg2defbb.set(i, bbs);
            }
            bbs->append_tail(bb);
        }
    }

    //Place phi for lived Reg.
    xcom::BitSet visited((bblst->get_elem_count() / BITS_PER_BYTE) + 1);
    DefRegSetIter cur = nullptr;
    for (BSIdx i = effect_reg.get_first(&cur);
         i != BS_UNDEF; i = effect_reg.get_next(i, &cur)) {
        //effect_reg includes Regs that have not been defined. These Regs's
        //defbbs is empty.
        List<IRBB*> const* defbbs = reg2defbb.get((Reg)i);
        if (defbbs != nullptr) {
            placePhiForReg((Reg)i, defbbs, dfm, visited, wl, defined_reg_vec);
        }
    }
    END_TIMER(t, "RegSSA: Place phi");

    //Free local used objects.
    for (VecIdx i = 0; i <= reg2defbb.get_last_idx(); i++) {
        List<IRBB*> * bbs = reg2defbb.get(i);
        if (bbs == nullptr) { continue; }
        delete bbs;
    }
}


//Note call stmt is a specical case in renaming because it regards MayDef
//as MayUse.
void RegSSAMgr::renameUse(IR * ir, Reg2VRegStack & reg2vregstk)
{
    ASSERT0(ir);
    ASSERT0(ir->is_exp());
    RegSSAInfo * regssainfo = genRegSSAInfo(ir);
    ASSERT0(regssainfo);
    VROpndSetIter iter;
    VROpndSet * set = regssainfo->getVROpndSet();
    VROpndSet removed;
    VROpndSet added;
    BSIdx next;
    for (BSIdx i = set->get_first(&iter); i != BS_UNDEF; i = next) {
        next = set->get_next(i, &iter);
        VReg * vr = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(vr && vr->is_reg() && vr->id() == (UINT)i);

        //Get the top-version on stack.
        VReg * topv = reg2vregstk.get_top(vr->reg());
        if (topv == nullptr) {
            //Reg does not have top-version, it has no def,
            //and may be parameter.
            continue;
        }

        //e.g: Reg1 = Reg2(VReg1)
        //    VReg1 will be renamed to VReg2, so VReg1 will not
        //    be there in current IR any more.

        //Set latest version of VReg be the USE of current opnd.
        if (topv->version() == REGSSA_INIT_VERSION) {
            //Do nothing.
            ASSERT0(vr == topv);
        } else if (vr != topv) {
            //vr may be ver0.
            //Current ir does not refer the old version VReg any more.
            ASSERT0(vr->version() == REGSSA_INIT_VERSION ||
                    vr->findUse(ir));
            ASSERT0(vr->version() == REGSSA_INIT_VERSION || vr->getDef());
            ASSERT0(!topv->findUse(ir));

            set->remove(vr, *getSBSMgr());
            added.append(topv, *getSBSMgr());
        }

        topv->addUse(ir);
    }

    set->bunion(added, *getSBSMgr());
    added.clean(*getSBSMgr());
}


RegPhi * RegSSAMgr::genRegPhi(Reg reg, IR * opnd_list, IRBB * bb, VReg * result)
{
    RegPhi * phi = m_dumgr.allocRegPhi(reg);

    //Allocate RegSSAInfo for given operands.
    for (IR * opnd = opnd_list; opnd != nullptr; opnd = opnd->get_next()) {
        ASSERT0(getReg(opnd) == reg);
        //Generate RegSSAInfo to ID.
        RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(opnd);
        ASSERT0_DUMMYUSE(regssainfo && !regssainfo->readVROpndSet().is_empty());
        ASSERT0(regssainfo->containSpecificRegOnly(reg, getRegDUMgr()));
        PHYREG_phi(opnd) = phi; //Record ID's host PHI.
    }
    REGPHI_opnd_list(phi) = opnd_list;
    REGPHI_bb(phi) = bb;
    REGDEF_result(phi) = result;

    //Do NOT set DEF of result here because result's version may be zero.
    //VREG_def(result) = phi;
    return phi;
}


RegPhi * RegSSAMgr::genRegPhi(Reg reg, UINT num_opnd, IRBB * bb, VReg * result)
{
    RegPhi * phi = m_dumgr.allocRegPhi(reg);
    m_dumgr.buildRegPhiOpnd(phi, reg, num_opnd);
    REGPHI_bb(phi) = bb;
    REGDEF_result(phi) = result;

    //Do NOT set DEF of result here because result's version may be zero.
    //VREG_def(result) = phi;
    return phi;
}


RegDef * RegSSAMgr::genRegDefStmt(IR * ir, VReg * result)
{
    ASSERT0(ir && ir->is_stmt());
    RegDef * regdef = m_dumgr.allocRegDefStmt();
    REGDEF_result(regdef) = result;
    REGDEF_is_phi(regdef) = false;
    REGDEFSTMT_occ(regdef) = ir;
    return regdef;
}


void RegSSAMgr::renameDef(IR * ir, IRBB * bb, Reg2VRegStack & reg2vregstk)
{
    ASSERT0(ir && ir->is_stmt());
    RegSSAInfo * regssainfo = genRegSSAInfo(ir);
    ASSERT0(regssainfo);
    VROpndSetIter iter;
    VROpndSet * set = regssainfo->getVROpndSet();
    VROpndSet added;
    BSIdx next;
    for (BSIdx i = set->get_first(&iter); i != BS_UNDEF; i = next) {
        next = set->get_next(i, &iter);
        VReg * vr = (VReg*)m_dumgr.getVROpnd(i);
        ASSERT0(vr && vr->is_reg() && vr->id() == (UINT)i);
        ASSERTN(vr->version() == REGSSA_INIT_VERSION,
                ("should be first meet"));

        //Update versioned Reg.
        VReg * newv = genNewVersionVReg(vr->reg());
        VReg * nearestv = reg2vregstk.get_top(vr->reg());
        reg2vregstk.push(vr->reg(), newv);

        RegDef * regdef = genRegDefStmt(ir, newv);
        if (nearestv != nullptr && nearestv->getDef() != nullptr) {
            addDefChain(nearestv->getDef(), regdef);
        }
        VREG_def(newv) = regdef;
        set->remove(vr, *getSBSMgr());
        added.append(newv, *getSBSMgr());
    }

    set->bunion(added, *getSBSMgr());
    added.clean(*getSBSMgr());
}


//Cut off the DU chain between 'def' and its predecessors.
void RegSSAMgr::cutoffDefChain(RegDef * def)
{
    RegDef * prev = def->getPrev();
    if (prev != nullptr) {
        ASSERT0(prev->getNextSet() && prev->getNextSet()->find(def));
        prev->getNextSet()->remove(def, *getSBSMgr());
    }
    REGDEF_prev(def) = nullptr;
}


//Return true if VRegs of stmt cross version when moving stmt outside of loop.
bool RegSSAMgr::isCrossLoopHeadPhi(
    IR const* stmt, LI<IRBB> const* li, OUT bool & cross_nonphi_def) const
{
    bool cross_loophead_phi = false;
    ASSERT0(stmt->is_stmt());
    RegSSAInfo * info = getRegSSAInfoIfAny(stmt);
    if (info == nullptr || info->isEmptyVROpndSet()) { return false; }

    //Check if one of VReg cross RegPhi in loophead.
    IRBB const* loophead = li->getLoopHead();
    ASSERT0(loophead);
    VROpndSet const& vregset = info->readVROpndSet();
    RegDUMgr const* udmgr = const_cast<RegSSAMgr*>(this)->getRegDUMgr();
    //stmt will be appended at the tail of tgtbb.
    VROpndSetIter vit = nullptr;
    for (BSIdx i = vregset.get_first(&vit);
         i != BS_UNDEF; i = vregset.get_next(i, &vit)) {
        VReg const* t = (VReg*)udmgr->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (t->getDef() == nullptr) { continue; }

        //Only consider prev-def of stmt.
        RegDef const* prev = t->getDef()->getPrev();
        if (prev == nullptr) { continue; }
        if (!li->isInsideLoop(prev->getBB()->id())) {
            //prev-def is not inside loop.
            continue;
        }
        if (!prev->is_phi()) {
            if (!isOverConservativeDUChain(stmt, prev->getOcc())) {
                //stmt may cross non-phi RegDef.
                cross_nonphi_def = true;
            }
            continue;
        }
        if (prev->getBB() == loophead) {
            cross_loophead_phi = true;
            continue;
        }
    }
    return cross_loophead_phi;
}


static bool verifyDDChainBeforeMerge(RegDef * def1, MOD RegDef * def2)
{
    //CASE1: def4 may be have been inserted between other DefDef chain.
    //      BB11:def1
    //       |
    //       v
    //    __BB14:def4__
    //   |             |
    //   v             v
    //  BB16:def2     BB20:def3
    //When inserting def4 at BB14, need to insert RegDef between def1->def2 and
    //def1->def3.
    if (def1->getPrev() == nullptr || def2->getPrev() == nullptr) {
        return true;
    }

    //CASE2:gcse.c.
    //The new inserted stmt may processed in previous updating,
    //and the related VReg has been inserted into some DD chain. In this
    //situation, caller expect to rebuild DD chain and DU chain for whole
    //DomTree. If it is that, just overwrite the DD chain according current
    //DD relation, while no need to check the relation between def1 and def2.
    RegDef * def2prev = def2->getPrev();
    for (RegDef * p = def1->getPrev(); p != nullptr; p = p->getPrev()) {
        if (p == def2prev) { return true; }
    }
    RegDef * def1prev = def1->getPrev();
    for (RegDef * p = def2->getPrev(); p != nullptr; p = p->getPrev()) {
        if (p == def1prev) { return true; }
    }

    //def1 and def2 are in different DomTree path. Merge the path is risky.
    return false;
}


void RegSSAMgr::insertDefBefore(RegDef * def1, MOD RegDef * def2)
{
    ASSERT0(def1 && def2);
    ASSERT0(isDom(def1, def2));
    RegDef * def2prev = def2->getPrev();
    if (def2prev == def1) {
        //def1 and def2 already be DefDef Chain.
        return;
    }
    ASSERT0(def2prev == nullptr ||
            (def2prev->getNextSet() && def2prev->getNextSet()->find(def2)));
    ASSERT0_DUMMYUSE(verifyDDChainBeforeMerge(def1, def2));
    //def1 and def2 are in same DomTree path.
    if (def2prev != nullptr) {
        def2prev->getNextSet()->remove(def2, *getSBSMgr());
        if (def1->getPrev() == nullptr) {
            def2prev->getNextSet()->append(def1, *getSBSMgr());
            REGDEF_prev(def1) = def2prev;
        }
        if (REGDEF_nextset(def1) == nullptr) {
            REGDEF_nextset(def1) = m_dumgr.allocRegDefSet();
        } else {
            ASSERT0(!REGDEF_nextset(def1)->hasAtLeastOneElemDom(def2, this));
        }
        def1->getNextSet()->append(def2, *getSBSMgr());
        REGDEF_prev(def2) = def1;
        return;
    }
    if (REGDEF_nextset(def1) == nullptr) {
        REGDEF_nextset(def1) = m_dumgr.allocRegDefSet();
    } else {
        ASSERT0(!REGDEF_nextset(def1)->hasAtLeastOneElemDom(def2, this));
    }
    def1->getNextSet()->append(def2, *getSBSMgr());
    REGDEF_prev(def2) = def1;
}


//Add relation to def1->def2 where def1 dominated def2.
void RegSSAMgr::addDefChain(RegDef * def1, RegDef * def2)
{
    ASSERT0(def1 && def2);
    ASSERTN(def2->getPrev() == nullptr,
            ("should cutoff outdated def-relation"));
    if (def1->getNextSet() == nullptr) {
        REGDEF_nextset(def1) = m_dumgr.allocRegDefSet();
    }
    def1->getNextSet()->append(def2, *getSBSMgr());
    REGDEF_prev(def2) = def1;
}


//Rename VReg from current version to the top-version on stack if it exist.
void RegSSAMgr::renamePhiResult(IN IRBB * bb, Reg2VRegStack & reg2vregstk)
{
    ASSERT0(bb);
    RegPhiList * philist = getPhiList(bb->id());
    if (philist == nullptr) { return; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi() && phi->getBB() == bb);

        //Rename phi result.
        VReg * vr = phi->getResult();
        ASSERT0(vr && vr->is_reg());

        //Update versioned Reg.
        VReg * newv = genNewVersionVReg(vr->reg());
        reg2vregstk.push(vr->reg(), newv);
        REGDEF_result(phi) = newv;
        cutoffDefChain(phi);
        VREG_def(newv) = phi;
    }
}


//Rename VReg from current version to the top-version on stack if it exist.
void RegSSAMgr::renameBB(IN IRBB * bb, Reg2VRegStack & reg2vregstk)
{
    renamePhiResult(bb, reg2vregstk);
    BBIRListIter it;
    for (IR * ir = bb->getIRList().get_head(&it);
         ir != nullptr; ir = bb->getIRList().get_next(&it)) {
        //Rename opnd, not include phi.
        //Walk through rhs expression IR tree to rename memory's VReg.
        m_iter.clean();
        for (IR * opnd = xoc::iterInit(ir, m_iter);
             opnd != nullptr; opnd = xoc::iterNext(m_iter)) {
            if (!hasExpRegSSAInfo(opnd)) {
                continue;
            }

            //In memory SSA, rename the Reg even if it is ineffect to
            //keep sound DU chain, e.g:
            //  int bar(int * p, int * q, int * m, int * n)
            //  {
            //    *p = *q + 20; *p define Reg2V1
            //    *m = *n - 64; *n use Reg2V1
            //    return 0;
            //  }
            renameUse(opnd, reg2vregstk);
        }
        if (!hasStmtRegSSAInfo(ir)) { continue; }

        //Rename result.
        renameDef(ir, bb, reg2vregstk);
    }
}


void RegSSAMgr::renamePhiOpndInSuccBB(IRBB * bb, Reg2VRegStack & reg2vregstk)
{
    ASSERT0(bb->getVex());
    for (EdgeC const* bbel = bb->getVex()->getOutList();
         bbel != nullptr; bbel = bbel->get_next()) {
        UINT opnd_idx = 0; //the index of corresponding predecessor.
        Vertex const* succv = bbel->getTo();
        EdgeC const* sel;
        for (sel = succv->getInList();
             sel != nullptr; sel = sel->get_next(), opnd_idx++) {
            if (sel->getFromId() == bb->id()) {
                break;
            }
        }
        ASSERTN(sel, ("not found related pred"));
        //Replace opnd of PHI of 'succ' with top SSA version.
        handlePhiInSuccBB(m_cfg->getBB(succv->id()), opnd_idx, reg2vregstk);
    }
}


//Replace opnd of PHI of 'succ' with top SSA version.
void RegSSAMgr::handlePhiInSuccBB(
    IRBB * succ, UINT opnd_idx, Reg2VRegStack & reg2vregstk)
{
    RegPhiList * philist = getPhiList(succ);
    if (philist == nullptr) { return; }

    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        IR * opnd = phi->getOpnd(opnd_idx);
        ASSERT0(opnd && opnd->getCode() == IR_PHYREG);
        ASSERT0(hasReg(opnd));

        VReg * topv = reg2vregstk.get_top(getReg(opnd));
        ASSERTN(topv, ("miss def-stmt to operand of phi"));

        RegSSAInfo * opnd_ssainfo = getRegSSAInfoIfAny(opnd);
        ASSERT0(opnd_ssainfo);
        opnd_ssainfo->cleanVROpndSet(getRegDUMgr());
        opnd_ssainfo->addVROpnd(topv, getRegDUMgr());
        topv->addUse(opnd);
    }
}


void RegSSAMgr::handleBBRename(
    IRBB * bb, DefRegSet const& effect_regs, DefRegSet const& defed_regs,
    MOD BB2VRegMap & bb2vregmap, Reg2VRegStack & reg2vregstk)
{
    ASSERT0(bb2vregmap.get(bb->id()) == nullptr);
    Reg2VReg * regid2vreg = bb2vregmap.gen(bb->id());
    DefRegSetIter it = nullptr;
    for (BSIdx reg = defed_regs.get_first(&it);
         reg != BS_UNDEF; reg = defed_regs.get_next(reg, &it)) {
        VReg * vreg = reg2vregstk.get_top(reg);
        ASSERT0(vreg || !effect_regs.is_contain(reg));
        if (vreg != nullptr) {
            regid2vreg->set(vreg->reg(), vreg);
        }
    }
    renameBB(bb, reg2vregstk);
    renamePhiOpndInSuccBB(bb, reg2vregstk);
}


void RegSSAMgr::initVRegStack(
    BB2DefRegSet const& bb2defregs, OUT Reg2VRegStack & reg2verstk)
{
    DefMiscBitSetMgr bs_mgr;
    DefRegSet effect_regs(bs_mgr.getSegMgr());
    for (VecIdx i = 0; i < (VecIdx)bb2defregs.get_elem_count(); i++) {
        xcom::DefSBitSet const* defregs = bb2defregs.get(i);
        if (defregs == nullptr) { continue; }
        effect_regs.bunion(*defregs);
    }
    initVRegStack(effect_regs, reg2verstk);
}


void RegSSAMgr::initVRegStack(
    DefRegSet const& defregs, OUT Reg2VRegStack & reg2verstk)
{
    DefRegSetIter it = nullptr;
    for (BSIdx i = defregs.get_first(&it);
         i != BS_UNDEF; i = defregs.get_next(i, &it)) {
        reg2verstk.push(i, genInitVersionVReg((Reg)i));
    }
}


//Rename variables.
void RegSSAMgr::rename(DefRegSet const& effect_regs, BB2DefRegSet & bb2defregs,
                       DomTree const& domtree, Reg2VRegStack & reg2vregstk)
{
    START_TIMER(t, "RegSSA: Rename");
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    initVRegStack(effect_regs, reg2vregstk);
    ASSERT0(m_cfg->getEntry());
    RegSSAConstructRenameVisitVF vf(effect_regs, bb2defregs, reg2vregstk, this);
    RegSSAConstructRenameVisit rn(domtree, m_cfg->getEntry(), vf);
    rn.visit();
    END_TIMER(t, "RegSSA: Rename");
}


void RegSSAMgr::cleanAllRegSSAInfo()
{
    getRegDUMgr()->cleanAllRegSSAInfo();
}


void RegSSAMgr::destructBBSSAInfo(IRBB * bb, RegSSAUpdateCtx const* ctx)
{
    freeBBPhiList(bb, ctx);
}


void RegSSAMgr::destructionInDomTreeOrder(
    IRBB * root, DomTree & domtree, RegSSAUpdateCtx const* ctx)
{
    xcom::Stack<IRBB*> stk;
    UINT n = m_rg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VRegMap bb2vregmap(n);
    IRBB * v = nullptr;
    stk.push(root);
    while ((v = stk.get_top()) != nullptr) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            destructBBSSAInfo(v, ctx);
        }
        xcom::Vertex const* bbv = domtree.getVertex(v->id());
        ASSERTN(bbv, ("dom tree is invalid."));
        xcom::EdgeC const* c = bbv->getOutList();
        bool all_visited = true;
        while (c != nullptr) {
            xcom::Vertex const* dom_succ = c->getTo();
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(dom_succ->id())) {
                ASSERT0(m_cfg->getBB(dom_succ->id()));
                all_visited = false;
                stk.push(m_cfg->getBB(dom_succ->id()));
                break;
            }
            c = c->get_next();
        }
        if (all_visited) {
            stk.pop();
            //Do post-processing while all kids of BB has been processed.
        }
    }
    cleanAllRegSSAInfo();
}


//Destruction of RegSSA.
//The function perform SSA destruction via scanning BB in preorder
//traverse dominator tree.
//Return true if inserting copy at the head of fallthrough BB
//of current BB's predessor.
void RegSSAMgr::destruction(DomTree & domtree, RegSSAUpdateCtx const* ctx)
{
    START_TIMER(t, "RegSSA: destruction in dom tree order");
    if (!is_valid()) { return; }
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    ASSERT0(m_cfg->getEntry());
    destructionInDomTreeOrder(m_cfg->getEntry(), domtree, ctx);
    set_valid(false);
    END_TIMER(t, "RegSSA: destruction in dom tree order");
}


bool RegSSAMgr::verifyDDChain() const
{
    START_TIMER(tverify, "RegSSA: Verify DefDef Chain");
    RegSSAMgr * pthis = const_cast<RegSSAMgr*>(this);
    RegDefVec const* regdefvec = pthis->getRegDUMgr()->getRegDefVec();
    for (VecIdx i = 0; i <= regdefvec->get_last_idx(); i++) {
        RegDef const* regdef = regdefvec->get(i);
        if (regdef == nullptr) { continue; }

        RegDef const* prev = regdef->getPrev();
        if (prev != nullptr) {
            ASSERT0(prev->getNextSet());
            ASSERT0(prev->getNextSet()->find(regdef));
        }
        if (regdef->is_phi() && ((RegPhi*)regdef)->hasNumOfOpndAtLeast(2)) {
            //Note RegPhi does not have previous DEF, because usually Phi has
            //multiple previous DEFs rather than single DEF.
            //CASE:compile/regssa_phi_prevdef.c
            //Sometime optimization may form CFG that cause PHI1
            //dominiates PHI2, then PHI1 will be PHI2's previous-DEF.
            //ASSERT0(prev == nullptr);
        }
        //CASE: Be careful that 'prev' should not belong to the NextSet of
        //regdef', otherwise the union operation of prev and regdef's succ DEF
        //will construct a cycle in DefDef chain, which is illegal.
        //e.g: for (i = 0; i < 10; i++) {;}, where i's Reg is Reg5.
        //  Reg5V2 <-- PHI(Reg5V--, Reg5V3)
        //  Reg5V3 <-- Reg5V2 + 1
        // If we regard Reg5V3 as the common-def, PHI is 'regdef', a cycle
        // will appeared.
        if (prev != nullptr && regdef->getNextSet() != nullptr) {
            ASSERTN(!regdef->getNextSet()->find(prev),
                    ("prev should NOT be the NEXT of regdef"));
        }
    }
    END_TIMER(tverify, "RegSSA: Verify DefDef Chain");
    return true;
}


bool RegSSAMgr::verifyPhiOpndList(RegPhi const* phi, UINT prednum) const
{
    RegSSAMgr * pthis = const_cast<RegSSAMgr*>(this);
    VReg * res = phi->getResult();
    ASSERT0_DUMMYUSE(res->is_reg());
    UINT opndnum = 0;
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        opndnum++;
        if (opnd->getCode() != IR_PHYREG) {
            ASSERT0(opnd->is_const() || opnd->is_lda());
            continue;
        }
        ASSERTN(PHYREG_phi(opnd) == phi, ("opnd is not an operand of phi"));

        //CASE1:Opnd may be ID, CONST or LDA.
        Reg opnd_reg = getReg(opnd);
        ASSERT0_DUMMYUSE(opnd_reg);
        ASSERTN(opnd_reg == res->reg(), ("reg of VReg is unmatched"));

        //CASE2:An individual ID can NOT represent multiple versioned Reg, thus
        //the VROpnd of ID must be unique.
        RegSSAInfo const* opnd_regssainfo = getRegSSAInfoIfAny(opnd);
        ASSERT0(opnd_regssainfo);
        UINT vropndnum = opnd_regssainfo->readVROpndSet().get_elem_count();
        ASSERT0_DUMMYUSE(vropndnum == 1);

        //CASE3:some pass, e.g:DCE, will remove RegPhi step by step, thus
        //do NOT invoke the function during the removing.
        VReg * opndvreg = ((RegPhi*)phi)->getOpndVReg(
            opnd, pthis->getRegDUMgr());
        ASSERTN_DUMMYUSE(opndvreg, ("miss VROpnd"));

        //CASE4:Version 0 does not have RegDef.
        //ASSERT0(VReg_version(opndvreg) > 0);
    }
    //CASE5:check the number of phi opnds.
    ASSERTN(opndnum == prednum,
            ("The number of phi operand must same with "
             "the number of BB predecessors."));
    return true;
}


//The function verify the operand and VReg info for RegPhi.
//NOTE: some pass, e.g:DCE, will remove RegPhi step by step, thus
//do NOT invoke the function during the removing.
bool RegSSAMgr::verifyPhi() const
{
    BBList * bblst = m_rg->getBBList();
    List<IRBB*> preds;
    for (IRBB * bb = bblst->get_head(); bb != nullptr;
         bb = bblst->get_next()) {
        m_cfg->get_preds(preds, bb);
        RegPhiList * philist = getPhiList(bb);
        if (philist == nullptr) { continue; }

        UINT prednum = bb->getNumOfPred();
        for (RegPhiListIter it = philist->get_head();
             it != philist->end(); it = philist->get_next(it)) {
            RegPhi * phi = it->val();
            ASSERT0(phi);
            verifyPhiOpndList(phi, prednum);
        }
    }
    return true;
}


void RegSSAMgr::collectDefinedReg(IRBB const* bb, OUT DefRegSet & maydef) const
{
    RegPhiList const* philist = getPhiList(bb);
    if (philist != nullptr) {
        for (RegPhiListIter it = philist->get_head();
             it != philist->end(); it = philist->get_next(it)) {
            ASSERT0(it->val());
            maydef.bunion(it->val()->getResult()->reg());
        }
    }
    BBIRListIter it;
    for (IR const* ir = const_cast<IRBB*>(bb)->getIRList().get_head(&it);
         ir != nullptr; ir = const_cast<IRBB*>(bb)->getIRList().get_next(&it)) {
        if (ir->isCallReadOnly() || !hasRegSSAInfo(ir)) {
            continue;
        }
        RegSSAInfo const* info = getRegSSAInfoIfAny(ir);
        if (info == nullptr || info->isEmptyVROpndSet()) { continue; }
        VROpndSetIter it;
        VROpndSet const& set = info->readVROpndSet();
        for (BSIdx i = set.get_first(&it); i != BS_UNDEF;
             i = set.get_next(i, &it)) {
            VReg const* vreg = (VReg*)getVROpnd(i);
            ASSERT0(vreg && vreg->is_reg());
            ASSERT0(vreg->version() != REGSSA_INIT_VERSION);
            ASSERT0(vreg->getDef());
            maydef.bunion(vreg->reg());
        }
    }
}


void RegSSAMgr::collectDefinedRegForBBList(
    MOD DefMiscBitSetMgr & bs_mgr, OUT BB2DefRegSet & bb2defregs) const
{
    BBList const* bblst = m_rg->getBBList();
    BBListIter it;
    for (IRBB const* bb = bblst->get_head(&it);
         bb != nullptr; bb = bblst->get_next(&it)) {
        DefRegSet * bs = bs_mgr.allocSBitSet();
        bb2defregs.set(bb->id(), bs);
        collectDefinedReg(bb, *bs);
    }
}


static bool verifyVerPhiInSuccBB(
    IRBB const* succ, UINT opnd_idx, Reg2VRegStack const& reg2verstk,
    RegSSAMgr const* mgr)
{
    RegPhiList * philist = mgr->getPhiList(succ);
    if (philist == nullptr) { return true; }
    RegSSAMgr * pmgr = const_cast<RegSSAMgr*>(mgr);
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi const* phi = it->val();
        ASSERT0(phi && phi->is_phi());
        IR * opnd = phi->getOpnd(opnd_idx);
        ASSERTN(opnd && (opnd->getCode() == IR_PHYREG),
                ("illegal phi operand"));
        VReg const* curvreg = ((RegPhi*)phi)->getOpndVReg(
            opnd, pmgr->getRegDUMgr());
        ASSERTN(curvreg, ("miss VROpnd"));
        VReg const* topvreg = reg2verstk.get_top(curvreg);
        if (curvreg == topvreg) { continue; }
        if (topvreg == nullptr && curvreg->version() == REGSSA_INIT_VERSION) {
            continue;
        }
        ASSERTN(0, ("use invalid VReg version"));
    }
    return true;
}


static bool verifyVerPhiResult(
    IRBB const* bb, MOD Reg2VRegStack & reg2verstk, RegSSAMgr const* mgr)
{
    ASSERT0(bb);
    RegPhiList const* philist = mgr->getPhiList(bb);
    if (philist == nullptr) { return true; }
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi() && phi->getBB() == bb);
        VReg * resvreg = phi->getResult();
        ASSERT0(resvreg && resvreg->is_reg());
        reg2verstk.push(resvreg);
    }
    return true;
}


static bool verifyVerUse(
    IR const* ir, Reg2VRegStack const& reg2verstk, RegSSAMgr const* mgr)
{
    ASSERT0(ir->is_exp());
    RegSSAInfo const* info = mgr->getRegSSAInfoIfAny(ir);
    ASSERT0(info);
    VROpndSetIter it;
    VROpndSet const& set = info->readVROpndSet();
    for (BSIdx i = set.get_first(&it); i != BS_UNDEF;
         i = set.get_next(i, &it)) {
        VReg const* vreg = (VReg*)mgr->getVROpnd(i);
        ASSERT0(vreg && vreg->is_reg() && vreg->id() == (UINT)i);
        if (vreg->version() == REGSSA_INIT_VERSION) { continue; }
        VReg const* expectvreg = reg2verstk.get_top(vreg);
        ASSERTN_DUMMYUSE(vreg == expectvreg, ("use invalid version"));
    }
    return true;
}


static bool verifyVerDef(
    IR const* ir, MOD Reg2VRegStack & reg2verstk, RegSSAMgr const* mgr)
{
    ASSERT0(ir->is_stmt());
    RegSSAInfo const* info = mgr->getRegSSAInfoIfAny(ir);
    ASSERT0(info);
    VROpndSetIter it;
    VROpndSet const& set = info->readVROpndSet();
    for (BSIdx i = set.get_first(&it); i != BS_UNDEF;
         i = set.get_next(i, &it)) {
        VReg * vreg = (VReg*)mgr->getVROpnd(i);
        ASSERT0(vreg && vreg->is_reg() && vreg->id() == (UINT)i);
        ASSERT0(vreg->version() != REGSSA_INIT_VERSION);

        //Verify DefDef chain.
        VReg const* domprev_vreg = reg2verstk.get_top(vreg->reg());
        if (domprev_vreg != nullptr) {
            RegDef const* exp_prev = domprev_vreg->getDef();
            RegDef const* cur_def = vreg->getDef();
            ASSERT0_DUMMYUSE(exp_prev && cur_def);
            ASSERTN(cur_def->getPrev() == exp_prev, ("illegal prev-def"));
            ASSERTN(exp_prev->isNext(cur_def), ("illegal next-def"));
        }
        reg2verstk.push(vreg);
    }
    return true;
}


static bool verifyPhiOpndInSuccBB(
    IRBB const* bb, Reg2VRegStack & reg2verstk, RegSSAMgr const* mgr)
{
    IRCFG * cfg = mgr->getRegion()->getCFG();
    ASSERT0(bb->getVex());
    for (EdgeC const* bbel = bb->getVex()->getOutList();
         bbel != nullptr; bbel = bbel->get_next()) {
        UINT opnd_idx = 0; //the index of corresponding predecessor.
        Vertex const* succv = bbel->getTo();
        EdgeC const* sel;
        for (sel = succv->getInList();
             sel != nullptr; sel = sel->get_next(), opnd_idx++) {
            if (sel->getFromId() == bb->id()) {
                break;
            }
        }
        ASSERTN(sel, ("not found related pred"));
        verifyVerPhiInSuccBB(
            cfg->getBB(succv->id()), opnd_idx, reg2verstk, mgr);
    }
    return true;
}


static bool verifyVerBB(
    IRBB const* bb, Reg2VRegStack & reg2verstk, RegSSAMgr const* mgr)
{
    verifyVerPhiResult(bb, reg2verstk, mgr);
    ConstIRIter irit;
    for (IR const* ir = BB_first_ir(const_cast<IRBB*>(bb));
         ir != nullptr; ir = BB_next_ir(const_cast<IRBB*>(bb))) {
        //Rename opnd, not include phi.
        //Walk through rhs expression IR tree to rename memory's VReg.
        irit.clean();
        for (IR const* opnd = xoc::iterInitC(ir, irit);
             opnd != nullptr; opnd = xoc::iterNextC(irit)) {
            if (!RegSSAMgr::hasExpRegSSAInfo(opnd)) { continue; }
            verifyVerUse(opnd, reg2verstk, mgr);
        }
        if (RegSSAMgr::hasRegSSAInfo(ir)) {
            //Rename result.
            verifyVerDef(ir, reg2verstk, mgr);
        }
    }
    verifyPhiOpndInSuccBB(bb, reg2verstk, mgr);
    return true;
}


//Record the top version before enter into BB.
static void recordTopVer(
    IRBB const* bb, DefRegSet const* defed_regs,
    Reg2VRegStack const& reg2verstk, MOD BB2VRegMap & bb2vreg)
{
    ASSERT0(bb2vreg.get(bb->id()) == nullptr);
    Reg2VReg * regid2vreg = bb2vreg.gen(bb->id());
    DefRegSetIter it = nullptr;
    for (BSIdx reg = defed_regs->get_first(&it);
         reg != BS_UNDEF; reg = defed_regs->get_next(reg, &it)) {
        VReg * vreg = reg2verstk.get_top((Reg)reg);
        if (vreg != nullptr) {
            regid2vreg->set(vreg->reg(), vreg);
        }
    }
}


static bool verifyVersionImpl(DomTree const& domtree, RegSSAMgr const* mgr)
{
    DefMiscBitSetMgr bs_mgr;
    BB2DefRegSet bb2defregs;
    mgr->collectDefinedRegForBBList(bs_mgr, bb2defregs);
    IRCFG const* cfg = mgr->getRegion()->getCFG();
    IRBB const* root = cfg->getEntry();
    ASSERT0(root);
    xcom::Stack<IRBB const*> stk;
    UINT n = mgr->getRegion()->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VRegMap bb2vreg(n);
    IRBB const* bb = nullptr;
    stk.push(root);
    Reg2VRegStack reg2verstk;

    //The initial-version of each Regs has been created already.
    //The call-site here can guarantee that no new initial-version of Reg
    //generated.
    //CASE:To speedup verify, there is no need to push init-version of each Reg
    //into stack. The init-version of IR is corresponding to a empty slot in
    //stack.
    //const_cast<RegSSAMgr*>(mgr)->initVRegStack(bb2defregs, reg2verstk);
    while ((bb = stk.get_top()) != nullptr) {
        if (!visited.is_contain(bb->id())) {
            visited.bunion(bb->id());
            DefRegSet const* regs = bb2defregs.get(bb->id());
            ASSERT0(regs);
            recordTopVer(bb, regs, reg2verstk, bb2vreg);
            verifyVerBB(bb, reg2verstk, mgr);
        }
        xcom::Vertex const* bbv = domtree.getVertex(bb->id());
        bool all_visited = true;
        for (xcom::EdgeC const* c = bbv->getOutList();
             c != nullptr; c = c->get_next()) {
            xcom::Vertex const* dom_succ = c->getTo();
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(dom_succ->id())) {
                ASSERT0(cfg->getBB(dom_succ->id()));
                all_visited = false;
                stk.push(cfg->getBB(dom_succ->id()));
                break;
            }
        }
        if (all_visited) {
            stk.pop();

            //Do post-processing while all kids of BB has been processed.
            Reg2VReg * regid2vreg = bb2vreg.get(bb->id());
            ASSERT0(regid2vreg);
            xcom::DefSBitSet const* defregs = bb2defregs.get(bb->id());
            ASSERT0(defregs);
            DefSBitSetIter it = nullptr;
            for (BSIdx i = defregs->get_first(&it);
                 i != BS_UNDEF; i = defregs->get_next(i, &it)) {
                VRegStack * verstk = reg2verstk.get(i);
                ASSERT0(verstk && verstk->get_bottom());
                VReg const* vreg = regid2vreg->get(i);
                while (verstk->get_top() != vreg) {
                    verstk->pop();
                }
            }

            //vregmap is useless from now on.
            bb2vreg.erase(bb->id());
        }
    }
    return true;
}


//Note the verification is relatively slow.
bool RegSSAMgr::verifyVersion(OptCtx const& oc) const
{
    //Extract dominate tree of CFG.
    START_TIMER(t, "RegSSA: verifyVersion");
    ASSERT0(oc.is_dom_valid());
    DomTree domtree;
    m_cfg->genDomTree(domtree);
    if (!verifyVersionImpl(domtree, this)) {
        return false;
    }
    END_TIMER(t, "RegSSA: verifyVersion");
    return true;
}


bool RegSSAMgr::verifyVReg(VReg const* vreg, BitSet * defset) const
{
    ASSERT0(vreg);
    if (!vreg->is_reg()) { return true; }
    RegDef * def = vreg->getDef();
    if (def == nullptr) {
        //ver0 used to indicate the Region live-in Reg.
        //It may be parameter or outer region Reg.
        ASSERTN(vreg->version() == REGSSA_INIT_VERSION,
                ("Nondef vp's version must be REGSSA_INIT_VERSION"));
    } else {
        ASSERTN(def->getResult() == vreg, ("def is not the DEF of v"));
        ASSERTN(vreg->version() != REGSSA_INIT_VERSION,
                ("version can not be REGSSA_INIT_VERSION"));
        if (defset != nullptr) {
            //Only the first verify after construction need to this costly
            //check.
            ASSERTN(!defset->is_contain(def->id()),
                    ("DEF for each reg+version must be unique."));
            defset->bunion(def->id());
        }
    }

    VReg * res = nullptr;
    if (def != nullptr) {
        res = def->getResult();
        ASSERT0(res);
        verifyDef(def, vreg);
    }

    if (res != nullptr) {
        verifyUseSet(res);
    }
    return true;
}


static bool verifyExistenceOfVReg(IR const* ir, RegSSAMgr const* mgr)
{
    ASSERT0(ir && RegSSAMgr::hasRegSSAInfo(ir));
    RegSSAInfo const* regssainfo = mgr->getRegSSAInfoIfAny(ir);
    ASSERT0(regssainfo && !regssainfo->isEmptyVROpndSet());
    Reg mustref = mgr->getReg(ir);
    if (mustref == REG_UNDEF) { return true; }

    VReg const* mustref_vreg = (VReg*)regssainfo->getVROpndForReg(
        mustref, mgr);
    ASSERTN(mustref_vreg, ("ir miss MustRef VReg"));
    if (ir->getCode() == IR_PHYREG) {
        //PHYREG does not have MayRef regset.
        return true;
    }
    //NOTE:ir's MayRef should include all elements in the AliasSet of mustref.
    SRegSet const* may = mgr->getAliasRegSet(mustref);
    if (may == nullptr) { return true; }
    SRegSetIter it;
    for (BSIdx i = may->get_first(&it);
         i != BS_UNDEF; i = may->get_next(i, &it)) {
        Reg reg = (Reg)i;
        ASSERT0(reg != REG_UNDEF);
        VReg const* mayref_vreg = (VReg*)regssainfo->getVROpndForReg(reg, mgr);
        ASSERTN(mayref_vreg, ("ir miss MayRef VReg"));
    }
    return true;
}


static void verifyIRVReg(
    IR const* ir, RegSSAMgr const* mgr, TTab<VReg const*> & visited)
{
    ConstIRIter irit;
    for (IR const* t = iterInitC(ir, irit);
         t != nullptr; t = iterNextC(irit)) {
        if (!RegSSAMgr::hasRegSSAInfo(t)) { continue; }
        Reg ref = mgr->getReg(t);
        if (ref == REG_UNDEF) { continue; }

        //Verify VReg.
        RegSSAInfo * ssainfo = mgr->getRegSSAInfoIfAny(t);
        ASSERT0(ssainfo && !ssainfo->isEmptyVROpndSet());
        VROpndSet const& vropndset = ssainfo->readVROpndSet();
        VROpndSetIter it = nullptr;
        for (BSIdx i = vropndset.get_first(&it);
             i != BS_UNDEF; i = vropndset.get_next(i, &it)) {
            VReg const* vreg = (VReg const*)mgr->getVROpnd(i);
            ASSERT0(vreg && vreg->is_reg());
            if (visited.find(vreg)) { continue; }
            mgr->verifyVReg(vreg);
            visited.append(vreg);
        }
        verifyExistenceOfVReg(t, mgr);
    }
}


static void verifyPhiListVReg(
    RegPhiList const* philist, RegSSAMgr const* mgr,
    TTab<VReg const*> & visited)
{
    if (philist == nullptr) { return; }
    //Record the result Reg idx of PHI to avoid multidefining same Reg by PHI.
    xcom::TTab<Reg> phi_res;
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi const* phi = it->val();
        ASSERT0(phi && phi->is_phi());
        VReg const* vreg = phi->getResult();

        //Check if there is multidefinition of same Reg by PHI.
        ASSERTN(!phi_res.find(vreg->reg()), ("multiple define same Reg"));
        phi_res.append(vreg->reg());

        if (!visited.find(vreg)) {
            visited.append(vreg);
            mgr->verifyVReg(vreg, nullptr);
        }
        for (IR * opnd = phi->getOpndList();
             opnd != nullptr; opnd = opnd->get_next()) {
            verifyIRVReg(opnd, mgr, visited);
        }
    }
}


bool RegSSAMgr::verifyRefedVReg() const
{
    TTab<VReg const*> visited;
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        verifyPhiListVReg(getPhiList(bb), this, visited);
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            verifyIRVReg(ir, this, visited);
        }
    }
    return true;
}


bool RegSSAMgr::verifyAllVReg() const
{
    START_TIMER(tverify, "RegSSA: Verify VReg After Pass");

    RegSSAMgr * pthis = const_cast<RegSSAMgr*>(this);
    //Check version for each VReg.
    xcom::BitSet defset;
    VROpndVec * vec = pthis->getRegDUMgr()->getVROpndVec();
    for (VecIdx i = 1; i <= vec->get_last_idx(); i++) {
        VReg * v = (VReg*)vec->get(i);
        if (v == nullptr) {
            //VReg may have been removed.
            continue;
        }
        verifyVReg(v, &defset);
    }
    END_TIMER(tverify, "RegSSA: Verify VReg After Pass");
    return true;
}


void RegSSAMgr::verifyDef(RegDef const* def, VReg const* vropnd) const
{
    ASSERT0(def && def->getResult());
    if (def->is_phi()) {
        //TODO: verify PHI.
        return;
    }
    IR const* stmt = def->getOcc();
    ASSERT0(stmt);
    if (stmt->is_undef()) {
        //The stmt may have been removed, and the VReg is obsoleted.
        //If the stmt removed, its UseSet should be empty, otherwise there is
        //an illegal missing-DEF error.
        ASSERT0(!vropnd->hasUse());
    } else {
        RegSSAInfo const* ssainfo = getRegSSAInfoIfAny(stmt);
        ASSERT0_DUMMYUSE(ssainfo);
        VReg const* res = def->getResult();
        ASSERT0_DUMMYUSE(res);
        ASSERT0(ssainfo->readVROpndSet().is_contain(res->id()));
    }
    bool findref = false;
    if (isAlias(stmt, vropnd->reg())) {
        findref = true;
    }
    //The number of VReg may be larger than the number of Reg.
    //Do NOT ASSERT if not found reference.
    //Some transformation, such as IR Refinement, may change
    //the RegSet contents. This might lead to the inaccurate and
    //redundant memory dependence. But the correctness of
    //dependence is garanteed.
    //e.g:
    //ist:*<4> id:18 //:Reg11, Reg12, Reg14, Reg15
    //    lda: *<4> 'r'
    //    ild: i32  //MReg13: Reg16
    //        ld: *<4> 'q' //MReg18
    //=> after IR combination: ist(lda) => st
    //st:*<4> 'r' //MReg12
    //    ild : i32 //MReg13 : Reg16
    //        ld : *<4> 'q'    //MReg18
    //ist changed to st. The reference RegSet changed to single Reg as well.
    //ASSERT0_DUMMYUSE(findref);
    DUMMYUSE(findref);
}


static bool verify_dominance(IRBB const* defbb, IR const* use, IRCFG * cfg)
{
    if (use->getCode() == IR_PHYREG) {
        //If stmt's USE is ID, the USE is placed in PHI, which may NOT be
        //dominated by stmt.
        return true;
    }
    //DEF should dominate all USEs.
    ASSERT0(defbb == RegSSAMgr::getExpBB(use) ||
            cfg->is_dom(defbb->id(), RegSSAMgr::getExpBB(use)->id()));
    return true;
}


//Check SSA uses.
void RegSSAMgr::verifyUseSet(VReg const* vropnd) const
{
    //Check if USE of vropnd references the correct Reg/RegSet.
    VReg::UseSetIter iter2;
    IRBB const* defbb = vropnd->getDef() != nullptr ?
        vropnd->getDef()->getBB() : nullptr;
    for (UINT j = const_cast<VReg*>(vropnd)->getUseSet()->get_first(iter2);
         !iter2.end();
         j = const_cast<VReg*>(vropnd)->getUseSet()->get_next(iter2)) {
        IR const* use = (IR*)m_rg->getIR(j);
        ASSERT0(hasExpRegSSAInfo(use));
        bool findref = false;
        if (isAlias(use, vropnd->reg())) {
            findref = true;
        }
        //The number of VReg may be larger than the number of Reg.
        //Do NOT ASSERT if not found reference Reg at USE point which
        //should correspond to vropnd->reg().
        //Some transformation, such as IR Refinement, may change
        //the USE's RegSet. This might lead to the inaccurate and
        //redundant RegSSA DU Chain. So the RegSSA DU Chain is conservative,
        //but the correctness of RegSSA dependence is garanteed.
        //e.g:
        //  vstpr //Ref{NULL:Reg11, Reg12, Reg14, Reg15)
        //      ld 'q'
        //=> After IR transformation: vstpr transformed to stpr:
        //  stpr //Ref{Reg12:NULL}
        //      ld 'q'
        //vstpr transformed to stpr. This reduce referenced RegSet
        //to a single Reg as well.
        //DUMMYUSE(findref);
        ASSERT0_DUMMYUSE(findref);

        //VROpndSet of each USE should contain vropnd.
        RegSSAInfo * regssainfo = getRegSSAInfoIfAny(use);
        ASSERT0_DUMMYUSE(regssainfo);
        ASSERT0(regssainfo->getVROpndSet()->find(vropnd));
        ASSERT0_DUMMYUSE(verify_dominance(defbb, use, m_cfg));
    }
}


void RegSSAMgr::verifyRegSSAInfoForIR(IR const* ir) const
{
    ASSERT0(ir);
    RegSSAInfo * regssainfo = getRegSSAInfoIfAny(ir);
    ASSERT0(regssainfo);
    VROpndSetIter iter = nullptr;
    VROpndSet * set = regssainfo->getVROpndSet();
    for (BSIdx i = set->get_first(&iter); i != BS_UNDEF;
         i = set->get_next(i, &iter)) {
        VReg * vr = (VReg*)m_dumgr.getVROpnd(i);
        ASSERTN(vr, ("vr may have been removed, "
                     "VROpndSet have not been updated in time"));
        ASSERT0(vr && vr->is_reg());
        RegDef * def = vr->getDef();
        if (ir->is_stmt()) {
            ASSERTN(vr->version() != REGSSA_INIT_VERSION,
                    ("not yet perform renaming"));
            ASSERTN(def && def->getOcc() == ir, ("IR stmt should have RegDef"));
            ASSERT0(def->is_valid());
            continue;
        }

        //ir is expression.
        if (def != nullptr) {
            ASSERT0(vr->findUse(ir));
        } else {
            //The DEF of vr is NULL, it should be initial version of Reg.
            ASSERT0(vr->version() == REGSSA_INIT_VERSION);
        }
        //VReg's Def and UseSet verification will processed in verifyVReg().
    }
}


bool RegSSAMgr::verifyRegSSAInfoUniqueness() const
{
    xcom::TMap<RegSSAInfo const*, IR const*> ir2regssainfo;
    for (VecIdx i = 0; i <= m_rg->getIRVec().get_last_idx(); i++) {
        IR const* ir = m_rg->getIR(i);
        if (ir == nullptr) { continue; }
        RegSSAInfo const* regssainfo = getRegSSAInfoIfAny(ir);
        if (regssainfo != nullptr) {
            ir2regssainfo.set(regssainfo, ir);
        }
    }
    return true;
}


bool RegSSAMgr::verifyDUChainAndOccForPhi(RegPhi const* phi) const
{
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        if (opnd->getCode() != IR_PHYREG) {
            ASSERT0(opnd->is_const() || opnd->is_lda());
            continue;
        }
        Reg opnd_reg = getReg(opnd);
        ASSERT0_DUMMYUSE(opnd_reg);
        ASSERTN(opnd_reg == phi->getResult()->reg(), ("Reg not matched"));
        verifyRegSSAInfoForIR(opnd);
    }
    return true;
}


bool RegSSAMgr::verifyDUChainAndOcc() const
{
    RegSSAMgr * pthis = const_cast<RegSSAMgr*>(this);
    //Check version for each VReg.
    BBList * bbl = m_rg->getBBList();
    BBListIter ct = nullptr;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        //Verify PHI list.
        RegPhiList * philist = pthis->getPhiList(bb);
        if (philist != nullptr) {
            for (RegPhiListIter it = philist->get_head();
                 it != philist->end(); it = philist->get_next(it)) {
                RegPhi const* phi = it->val();
                ASSERT0(phi && phi->is_phi() && phi->getResult());
                verifyDUChainAndOccForPhi(phi);
            }
        }
        IRListIter ctir = nullptr;
        for (BB_irlist(bb).get_head(&ctir);
             ctir != BB_irlist(bb).end();
             ctir = BB_irlist(bb).get_next(ctir)) {
            IR * ir = ctir->val();
            pthis->m_iter.clean();
            for (IR const* x = xoc::iterInit(ir, pthis->m_iter);
                 x != nullptr; x = xoc::iterNext(pthis->m_iter)) {
                if (!hasRegSSAInfo(x)) { continue; }
                verifyRegSSAInfoForIR(x);
            }
        }
    }
    return true;
}


//The verification check the DU info in SSA form.
//Current IR must be in SSA form.
bool RegSSAMgr::verify() const
{
    START_TIMER(tverify, "RegSSA: Verify After Pass");
    ASSERT0(verifyDDChain());

    //No need to verify all VRegs because after optimizations, there are some
    //expired VRegs that no one use them, and not maintained. Just verify VReg
    //that referenced by IR.
    //ASSERT0(verifyAllVReg());
    ASSERT0(verifyRefedVReg());
    ASSERT0(verifyDUChainAndOcc());
    ASSERT0(verifyRegSSAInfoUniqueness());
    END_TIMER(tverify, "RegSSA: Verify After Pass");
    return true;
}


void RegSSAMgr::changeVReg(VReg * oldvreg, VReg * newvreg)
{
    ASSERT0(oldvreg && newvreg && oldvreg->is_reg() && newvreg->is_reg());
    ASSERT0(oldvreg != newvreg);
    VReg::UseSet * oldus = oldvreg->getUseSet();
    VReg::UseSet * newus = newvreg->getUseSet();
    VReg::UseSetIter it;
    for (UINT j = oldus->get_first(it); !it.end(); j = oldus->get_next(it)) {
        IR * use = m_rg->getIR(j);
        RegSSAInfo * usessainfo = getRegSSAInfoIfAny(use);
        ASSERT0(usessainfo);
        usessainfo->removeVROpnd(oldvreg, getRegDUMgr());
        usessainfo->addVROpnd(newvreg, getRegDUMgr());
        newus->append(j);
    }
    oldvreg->cleanUseSet();
}


//DU chain operation.
//Change Def stmt from orginal RegDef to 'newdef'.
//oldvreg: original VReg.
//newdef: target RegDef.
//e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
void RegSSAMgr::changeDef(VReg * oldvreg, RegDef * newdef)
{
    ASSERT0(oldvreg && newdef);
    ASSERT0(oldvreg->getDef() != newdef);
    VREG_def(oldvreg) = newdef;
}


//DU chain operation.
//Change Def stmt from 'olddef' to 'newdef'.
//olddef: original stmt.
//newdef: target stmt.
//e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
void RegSSAMgr::changeDef(IR * olddef, IR * newdef)
{
    ASSERT0(olddef && newdef && olddef->is_stmt() && newdef->is_stmt());
    ASSERT0(olddef != newdef);
    ASSERT0(hasRegSSAInfo(olddef) && hasRegSSAInfo(newdef));
    RegSSAInfo * oldregssainfo = getRegSSAInfoIfAny(olddef);
    ASSERT0(oldregssainfo);
    VROpndSetIter it = nullptr;
    VROpndSet const& vropndset = oldregssainfo->readVROpndSet();
    for (BSIdx i = vropndset.get_first(&it);
         i != BS_UNDEF; i = vropndset.get_next(i, &it)) {
        VReg * t = (VReg*)getVROpnd(i);
        ASSERT0(t && t->is_reg());
        RegDef * def = t->getDef();
        ASSERT0(def);
        ASSERT0(t->version() != REGSSA_INIT_VERSION);
        ASSERT0(def->getOcc() == olddef);
        REGDEFSTMT_occ(def) = newdef;
    }

    RegSSAInfo * newregssainfo = getRegSSAInfoIfAny(newdef);
    if (newregssainfo == nullptr || newregssainfo->isEmptyVROpndSet()) {
        copyRegSSAInfo(newdef, olddef);
        return;
    }
    //newdef may have different VROpnd than olddef.
    //Thus appending VROpnd of old to newdef.
    newregssainfo->addUseSet(oldregssainfo, getRegDUMgr());
}


//Generate RegSSAInfo and generate VReg for referrenced Reg that both include
//The function will generate RegSSAInfo for 'exp' according to the refinfo.
//that defined inside li. The new info for 'exp' will be VReg that defined
//outside of li or the initial version of VReg.
void RegSSAMgr::genRegSSAInfoToOutsideLoopDef(
    IR * exp, RegSSAInfo const* refinfo, LI<IRBB> const* li)
{
    RegSSAInfo * info = genRegSSAInfo(exp);
    ASSERT0(info);
    VROpndSetIter vit = nullptr;
    List<VReg*> newvregs;
    for (BSIdx i = refinfo->readVROpndSet().get_first(&vit);
         i != BS_UNDEF; i = refinfo->readVROpndSet().get_next(i, &vit)) {
        VReg * vreg = (VReg*)getVROpnd(i);
        ASSERT0(vreg->is_reg());
        vreg->removeUse(exp);
        RegDef const* newdef = nullptr;
        if (vreg->getDef() == nullptr) {
            ASSERT0(vreg->version() == REGSSA_INIT_VERSION);
        } else if (vreg->getDef()->is_phi()) {
            newdef = findUniqueOutsideLoopDef(vreg->getDef(), li);
        } else {
            for (newdef = vreg->getDef(); newdef != nullptr &&
                 li->isInsideLoop(newdef->getBB()->id());
                 newdef = newdef->getPrev()) {
            }
        }
        if (newdef != nullptr) {
            newvregs.append_tail(newdef->getResult());
        } else {
            //Need add init-version VReg to represent the existence of Reg.
            newvregs.append_tail(genInitVersionVReg(vreg->reg()));
        }
    }
    info->cleanVROpndSet(getRegDUMgr());
    for (VReg * v = newvregs.get_head();
         v != nullptr; v = newvregs.get_next()) {
        if (v->version() != REGSSA_INIT_VERSION) {
            v->addUse(exp);
        }
        info->addVROpnd(v, getRegDUMgr());
    }
}


void RegSSAMgr::changeDefToOutsideLoopDefForTree(IR * exp, LI<IRBB> const* li)
{
    ASSERT0(exp->is_exp());
    IRIter it;
    for (IR * x = iterInit(exp, it); x != nullptr; x = iterNext(it)) {
        if (!hasExpRegSSAInfo(x)) { continue; }
        changeDefToOutsideLoopDef(x, li);
    }
}


void RegSSAMgr::changeDefToOutsideLoopDef(IR * exp, LI<IRBB> const* li)
{
    ASSERT0(exp->is_exp() && hasExpRegSSAInfo(exp));
    RegSSAInfo * info = getRegSSAInfoIfAny(exp);
    ASSERT0(info && !info->readVROpndSet().is_empty());
    genRegSSAInfoToOutsideLoopDef(exp, info, li);
}


//DU chain operation.
//Change Use expression from 'olduse' to 'newuse'.
//olduse: single source expression.
//newuse: single target expression.
//e.g: Change RegSSA DU chain DEF->olduse to DEF->newuse.
void RegSSAMgr::changeUse(IR * olduse, IR * newuse)
{
    ASSERT0(olduse && newuse && olduse->is_exp() && newuse->is_exp());
    ASSERTN(olduse != newuse, ("redundant operation"));
    ASSERT0(hasExpRegSSAInfo(olduse) && hasExpRegSSAInfo(newuse));
    RegSSAInfo * oldinfo = getRegSSAInfoIfAny(olduse);
    ASSERT0(oldinfo);
    RegSSAInfo * newinfo = copyAndAddRegSSAOcc(newuse, oldinfo);
    ASSERT0_DUMMYUSE(newinfo);
    removeExpFromAllVROpnd(olduse);
}


//DU chain operation.
//Change Use expression from 'olduse' to 'newuse'.
//olduse: source expression as tree root.
//newuse: target expression as tree root.
//e.g: Change RegSSA DU chain DEF->olduse to DEF->newuse.
void RegSSAMgr::changeUseForTree(
    IR * olduse, IR * newuse, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(olduse && newuse && olduse->is_exp() && newuse->is_exp());
    ASSERTN(olduse != newuse, ("redundant operation"));
    ASSERT0(hasExpRegSSAInfo(olduse) && hasExpRegSSAInfo(newuse));
    addRegSSAOccForTree(newuse, olduse);
    removeRegSSAOccForTree(olduse, ctx);
}


void RegSSAMgr::coalesceDUChain(IR const* src, IR const* tgt)
{
    ASSERT0(src && tgt);
    ASSERT0(src->is_stmt() && tgt->is_exp() && tgt->getStmt() == src);
    RegSSAInfo * src_regssainfo = getRegSSAInfoIfAny(src);
    RegSSAInfo * tgt_regssainfo = getRegSSAInfoIfAny(tgt);
    ASSERT0(src_regssainfo && tgt_regssainfo);
    VROpndSetIter iter1 = nullptr;
    for (BSIdx i = src_regssainfo->getVROpndSet()->get_first(&iter1);
         i != BS_UNDEF;
         i = src_regssainfo->getVROpndSet()->get_next(i, &iter1)) {
        VReg * src_vropnd = (VReg*)getRegDUMgr()->getVROpnd(i);
        ASSERT0(src_vropnd && src_vropnd->is_reg());

        //Find the Reg in tgt's vr-set which has same reg with src's
        //except the distinct version.
        //e.g: src has Reg6Vx, find Reg6Vy in tgt vr set.
        VROpndSetIter iter2 = nullptr;
        VReg * tgt_vropnd = nullptr;
        for (BSIdx j = tgt_regssainfo->getVROpndSet()->get_first(&iter2);
             j != BS_UNDEF;
             j = tgt_regssainfo->getVROpndSet()->get_next(j, &iter2)) {
            VReg * t = (VReg*)getRegDUMgr()->getVROpnd(j);
            ASSERT0(t && t->is_reg());
            if (t->reg() == src_vropnd->reg()) {
                ASSERT0(t != src_vropnd);
                tgt_vropnd = t;
                break;
            }
        }

        if (tgt_vropnd == nullptr) {
            //Not find related tgt VReg that has same Reg to src VReg.
            //Just skip it because there is no version Reg to coalesce.
            continue;
        }

        ASSERTN(tgt_vropnd->version() != src_vropnd->version(),
                ("DEF and USE reference same version Reg"));
        //Replace the USE of src to tgt.
        replaceVROpndForAllUse(tgt_vropnd, src_vropnd);
    }
    //Do NOT clean VROpndSet of src here because the subsequent removeStmt()
    //need VROpndSet information.
    //src_regssainfo->cleanVROpndSet(getRegDUMgr());
}


void RegSSAMgr::addUseToRegSSAInfo(IR const* use, RegSSAInfo * regssainfo)
{
    ASSERT0(regssainfo);
    regssainfo->addUse(use, getRegDUMgr());
}


void RegSSAMgr::addUseSetToRegSSAInfo(IRSet const& set, RegSSAInfo * regssainfo)
{
    ASSERT0(regssainfo);
    regssainfo->addUseSet(set, getRegDUMgr());
}


void RegSSAMgr::addUseSetToVReg(IRSet const& set, MOD VReg * vreg)
{
    ASSERT0(vreg && vreg->is_reg());
    vreg->addUseSet(set, m_rg);
}


RegSSAInfo * RegSSAMgr::copyAndAddRegSSAOcc(IR * ir, RegSSAInfo const* src)
{
    //User may retain IRTree in transformation, however the RegSSAInfo has been
    //removed. Thus for the sake of convenient, permit src to be empty here.
    //ASSERT0(!src->readVROpndSet().is_empty());
    RegSSAInfo * irregssainfo = genRegSSAInfo(ir);
    if (irregssainfo != src) {
        if (ir->getCode() == IR_PHYREG) {
            //IR_ID represents an individual versioned Reg, and each IR_ID only
            //can have one VROpnd. Thus clean the old VROpnd, add new VROpnd.
            Reg reg = getReg(ir);
            ASSERT0(reg);
            irregssainfo->copyBySpecificReg(*src, reg, getRegDUMgr());
        } else {
            //Set ir references all VROpnds as src has.
            //And ir will be USE of all VROpnds.
            irregssainfo->copy(*src, getRegDUMgr());
        }
    }
    //Set VROpnd references ir as occurrence.
    addUseToRegSSAInfo(ir, irregssainfo);
    return irregssainfo;
}


void RegSSAMgr::copyRegSSAInfo(IR * tgt, IR const* src)
{
    ASSERT0(hasRegSSAInfo(tgt));
    RegSSAInfo * tgtinfo = genRegSSAInfo(tgt);
    RegSSAInfo const* srcinfo = getRegSSAInfoIfAny(src);
    ASSERT0(srcinfo);
    tgtinfo->copy(*srcinfo, getRegDUMgr());
}


//The function copy RegSSAInfo from tree 'src' to tree tgt.
//Note src and tgt must be isomorphic.
void RegSSAMgr::copyRegSSAInfoForTree(IR * tgt, IR const* src)
{
    IRIter it;
    ConstIRIter it2;
    IR * x;
    IR const* y;
    ASSERT0(tgt->isIsomoTo(src, getIRMgr(), true));
    for (x = iterInit(tgt, it), y = iterInitC(src, it2);
         x != nullptr; x = iterNext(it), y = iterNextC(it2)) {
        if (!hasRegSSAInfo(x)) { continue; }
        copyRegSSAInfo(x, y);
    }
}


void RegSSAMgr::addStmtToRegSSAMgr(IR * ir, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(ctx.getDomTree());
    VRRecomputeDefDefAndDefUseChain recomp(
        *ctx.getDomTree(), this, *ctx.getOptCtx(), ctx.getActMgr());
    recomp.recompute(ir);
}


void RegSSAMgr::buildDUChain(RegDef const* def, IRList const& lst)
{
    ASSERT0(def);
    IRListIter it;
    for (IR * exp = lst.get_head(&it);
         exp != nullptr; exp = lst.get_next(&it)) {
        if (exp->is_undef()) {
            //CASE:During some passes, the optimizer will collect UseSet at
            //first, then performing the optimization, such as Phi-Elimination,
            //lead to some irs in UseSet freed after the elimination. However,
            //the UseSet is not updated at the same time. This caused IR_UNDEF
            //to be in UseSet.
            //e.g: given 'def' is Phi Reg13V2,
            //  RegPhi: Reg13V2 <-(id:29 Reg13V1 BB10), (id:30 Reg13V2 BB3)
            //  the UseSet includes id:30. After removePhiFromRegSSAMgr(def),
            //  id:30 which is in UseSet will be UNDEF.
            //For the sake of speed-up of compilation, we just skip IR_UNDEF
            //rather than asking caller remove them before invoking the
            //function.
            continue;
        }
        buildDUChain(def, exp);
    }
}


void RegSSAMgr::buildDUChain(RegDef const* def, IRSet const& set)
{
    ASSERT0(def);
    IRSetIter it = nullptr;
    for (BSIdx i = set.get_first(&it);
         i != BS_UNDEF; i = set.get_next(i, &it)) {
        IR * exp = m_rg->getIR(i);
        ASSERT0(exp);
        if (exp->is_undef()) {
            //CASE:During some passes, the optimizer will collect UseSet at
            //first, then performing the optimization, such as Phi-Elimination,
            //lead to some irs in UseSet freed after the elimination. However,
            //the UseSet is not updated at the same time. This caused IR_UNDEF
            //to be in UseSet.
            //e.g: given 'def' is Phi Reg13V2,
            //  RegPhi: Reg13V2 <-(id:29 Reg13V1 BB10), (id:30 Reg13V2 BB3)
            //  the UseSet includes id:30. After removePhiFromRegSSAMgr(def),
            //  id:30 which is in UseSet will be UNDEF.
            //For the sake of speed-up of compilation, we just skip IR_UNDEF
            //rather than asking caller remove them before invoking the
            //function.
            continue;
        }
        buildDUChain(def, exp);
    }
}


void RegSSAMgr::buildDUChain(RegDef const* def, MOD IR * exp)
{
    ASSERT0(def && exp);
    ASSERT0(exp->is_exp());
    ASSERTN(def->getResult(), ("does not have occurrence"));
    if (def->isUse(exp)) { return; }
    def->getResult()->addUse(exp);
    RegSSAInfo * info = genRegSSAInfo(exp);
    ASSERT0(info);
    if (exp->getCode() == IR_PHYREG) {
        ASSERTN(info->getVROpndNum() == 0 ||
                info->getUniqueVROpnd(this) == def->getResult(),
                ("IR_ID can not have more than one DEF"));
    }
    //Note the function does NOT check whether the remainder VROpnd in info
    //is conflict with 'def'. Users have to guarantee it by themself.
    info->addVROpnd(def->getResult(), getRegDUMgr());
}


//Add occurence to each VROpnd in regssainfo.
//ir: occurence to be added.
//ref: the reference that is isomorphic to 'ir'.
//     It is used to retrieve RegSSAInfo.
void RegSSAMgr::addRegSSAOccForTree(IR * ir, IR const* ref)
{
    ASSERT0(ir->isIREqual(ref, getIRMgr(), false));
    ASSERT0(ir->is_exp());
    RegSSAInfo * regssainfo = getRegSSAInfoIfAny(ref);
    if (regssainfo != nullptr) {
        copyAndAddRegSSAOcc(ir, regssainfo);
    } else {
        ASSERT0(!hasRegSSAInfo(ref));
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR const* refkid = ref->getKid(i);
        IR * x = ir->getKid(i);
        for (; x != nullptr; x = x->get_next(), refkid = refkid->get_next()) {
            ASSERTN(refkid, ("ir is not isomorphic to ref"));
            addRegSSAOccForTree(x, refkid);
        }
    }
}


void RegSSAMgr::removeRegSSAOccForTree(IR const* ir, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(ir);
    if (hasRegSSAInfo(ir)) {
        if (ir->is_stmt()) {
            removeStmtRegSSAInfo(ir, ctx);
        } else {
            removeExpFromAllVROpnd(ir);
        }
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->getKid(i); x != nullptr; x = x->get_next()) {
            removeRegSSAOccForTree(x, ctx);
        }
    }
}


void RegSSAMgr::removeDUChain(RegDef const* def, IR * exp)
{
    ASSERT0(exp->is_exp());
    def->getResult()->removeUse(exp);
    RegSSAInfo * info = genRegSSAInfo(exp);
    ASSERT0(info);

    //Note the function does NOT check whether the remainder VROpnd in info
    //is conflict with 'def'. Users have to guarantee it by themself.
    info->removeVROpnd(def->getResult(), getRegDUMgr());

    //Note when VROpnd removed, ir's RegSSAInfo VROpndSet might be empty, user
    //should set init-version VReg by setInitVersionVReg() to avoid the
    //RegSSAMgr complaint.
}


void RegSSAMgr::removeDUChain(IR const* stmt, IR const* exp)
{
    ASSERT0(stmt && exp && stmt->is_stmt() && exp->is_exp());
    RegSSAInfo * regssainfo = getRegSSAInfoIfAny(exp);
    if (regssainfo == nullptr) { return; }
    VROpndSetIter iter = nullptr;
    BSIdx next_i = BS_UNDEF;
    for (BSIdx i = regssainfo->getVROpndSet()->get_first(&iter);
         i != BS_UNDEF; i = next_i) {
        next_i = regssainfo->getVROpndSet()->get_next(i, &iter);
        VReg * vr = (VReg*)getRegDUMgr()->getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (vr->getDef() == nullptr) {
            ASSERTN(vr->version() == REGSSA_INIT_VERSION,
                    ("Only zero version Reg has no DEF"));
            continue;
        }
        if (vr->getDef()->getOcc() != stmt) { continue; }
        vr->removeUse(exp);
        regssainfo->removeVROpnd(vr, getRegDUMgr());
    }
}


void RegSSAMgr::replaceVROpndForAllUse(MOD VReg * to, MOD VReg * from)
{
    ASSERT0(to->is_reg() && from->is_reg());
    //Replace the USE of src to tgt.
    VReg::UseSetIter it;
    for (UINT k = from->getUseSet()->get_first(it);
         !it.end(); k = from->getUseSet()->get_next(it)) {
        IR const* use = (IR*)m_rg->getIR(k);
        RegSSAInfo * use_regssainfo = getRegSSAInfoIfAny(use);
        ASSERTN(use_regssainfo, ("use miss RegSSAInfo"));
        use_regssainfo->removeVROpnd(from, getRegDUMgr());
        use_regssainfo->addVROpnd(to, getRegDUMgr());
        to->addUse(use);
    }
    from->cleanUseSet();
}


void RegSSAMgr::removeVROpndForAllUse(
    MOD VReg * vropnd, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(vropnd && vropnd->is_reg());
    VReg::UseSet * useset = vropnd->getUseSet();
    VReg::UseSetIter it;
    RegSSAStatus st;
    for (BSIdx i = useset->get_first(it); !it.end(); i = useset->get_next(it)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use && hasExpRegSSAInfo(use));
        RegSSAInfo * regssainfo = getRegSSAInfoIfAny(use);
        ASSERT0(regssainfo);
        if (use->getCode() == IR_PHYREG) {
            //An individual ID can NOT represent multiple versioned Reg, thus
            //the VROpnd of ID must be unique.
            //regssainfo->cleanVROpndSet(getRegDUMgr());
            ASSERT0(regssainfo->readVROpndSet().get_elem_count() == 1);
        }
        regssainfo->removeVROpnd(vropnd, getRegDUMgr());

        //Record IR that need to complete the new-version or init-version VReg.
        ctx.tryRecordRemovedVROpndIR(use);
    }
    vropnd->cleanUseSet();
}


void RegSSAMgr::removePhiFromRegSSAMgr(
    RegPhi * phi, RegDef * prev, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(phi && phi->is_phi());
    for (IR * opnd = phi->getOpndList(); opnd != nullptr;
         opnd = opnd->get_next()) {
        //Update the RegSSAInfo for each phi opnd.
        removeRegSSAOccForTree(opnd, ctx);
    }
    VReg * phires = phi->getResult();

    //Note PhiResultVROpnd should be removed first of all because it
    //participates in DefDefChain.
    //NOTE:If a PHI removed, its USEs may not have single-def any more.
    //Thus user should consider how to process the DEF-VReg of USEs when the
    //PHI removed.
    RegSSAUpdateCtx lctx(ctx);
    IRList tmp;
    lctx.setRemovedVROpndIRList(&tmp);
    removeVROpndForAllUse(phires, lctx);
    removePhiFromDDChain(phi, prev, lctx);
    ctx.unionBottomUpInfo(lctx);
    ctx.tryInvalidInfoBeforeFreeIRList(phi->getOpndList());
    m_rg->freeIRTreeList(phi->getOpndList());
    REGPHI_opnd_list(phi) = nullptr;
    removeVReg(phires);
}


void RegSSAMgr::removePhiList(IRBB * bb, RegSSAUpdateCtx const& ctx)
{
    RegPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return; }
    RegPhiListIter next;
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = next) {
        next = philist->get_next(it);
        RegPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        removePhiFromRegSSAMgr(phi, nullptr, ctx);
        philist->remove_head();
    }
    //Do NOT free PhiList here even if it is empty because philist is
    //allocated from memory pool.
}


//Remove given IR expression from UseSet of each vropnd in RegSSAInfo.
//Note current RegSSAInfo is the SSA info of 'exp', the VROpndSet will be
//emtpy when exp is removed from all VROpnd's useset.
//exp: IR expression to be removed.
//NOTE: the function only process exp itself.
void RegSSAMgr::removeExpFromAllVROpnd(IR const* exp)
{
    ASSERT0(exp && exp->is_exp() && hasExpRegSSAInfo(exp));
    RegSSAInfo * expssainfo = getRegDUMgr()->getRegSSAInfo(exp);
    ASSERT0(expssainfo);
    VROpndSetIter it = nullptr;
    VROpndSet * vropndset = expssainfo->getVROpndSet();
    RegDUMgr * mgr = getRegDUMgr();
    for (BSIdx i = vropndset->get_first(&it);
         i != BS_UNDEF; i = vropndset->get_next(i, &it)) {
        VReg * vr = (VReg*)mgr->getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        vr->removeUse(exp);
    }
    expssainfo->cleanVROpndSet(mgr);
}


//Remove Use-Def chain.
//exp: the expression to be removed.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in RegSSAInfo.
//NOTE: the function only process exp itself.
void RegSSAMgr::removeUse(IR const* exp)
{
    ASSERT0(exp->is_exp());
    if (hasExpRegSSAInfo(exp)) {
        removeExpFromAllVROpnd(exp);
    }
}


//Remove all VReg in set from RegSSAMgr. The function will clean all information
//about these VRegs.
void RegSSAMgr::removeVRegInSet(VROpndSet const& set)
{
    VROpndSetIter it = nullptr;
    for (BSIdx i = set.get_first(&it); i != BS_UNDEF;
         i = set.get_next(i, &it)) {
        VReg * vreg = (VReg*)getRegDUMgr()->getVROpnd(i);
        ASSERT0(vreg && vreg->is_reg());
        //The function only remove VReg out of RegSSAMgr.
        removeVReg(vreg);
    }
}


void RegSSAMgr::removeStmtRegSSAInfo(IR const* stmt, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(stmt && stmt->is_stmt());
    RegSSAInfo * stmtregssainfo = getRegSSAInfoIfAny(stmt);
    ASSERT0(stmtregssainfo);
    VROpndSetIter iter = nullptr;
    BSIdx next_i = BS_UNDEF;
    IRList tmp;
    for (BSIdx i = stmtregssainfo->getVROpndSet()->get_first(&iter);
         i != BS_UNDEF; i = next_i) {
        next_i = stmtregssainfo->getVROpndSet()->get_next(i, &iter);
        VReg * vr = (VReg*)getRegDUMgr()->getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (vr->getDef() == nullptr) {
            ASSERTN(vr->version() == REGSSA_INIT_VERSION,
                    ("Only zero version Reg has no DEF"));
            continue;
        }
        ASSERT0(vr->getDef()->getOcc() == stmt);

        //Iterate all USEs and remove 'vr' from the USE's RegSSAInfo.
        RegSSAUpdateCtx lctx(ctx);
        tmp.clean();
        lctx.setRemovedVROpndIRList(&tmp);
        removeVROpndForAllUse(vr, lctx);

        //Iterate DefDef chain.
        removeDefFromDDChain(vr->getDef(), lctx);
        ctx.unionBottomUpInfo(lctx);

        //Remove 'vr' from current stmt.
        stmtregssainfo->removeVROpnd(vr, getRegDUMgr());

        //Clear DEF info of 'vr'
        VREG_def(vr) = nullptr;

        //Because all info has been eliminated, the vr is out
        //of date and can be removed from RegSSAMgr.
        removeVReg(vr);
    }
}


//Union successors in NextSet from 'from' to 'to'.
void RegSSAMgr::unionSuccessors(RegDef const* from, RegDef const* to)
{
    if (from->is_phi()) {
        if (to->getNextSet() == nullptr) {
            if (from->getNextSet() != nullptr) {
                //Note if RegDef indicates PHI, it does not have Previous DEF,
                //because PHI has multiple Previous DEFs rather than single DEF.
                REGDEF_nextset(to) = m_dumgr.allocRegDefSet();
                to->getNextSet()->bunion(*from->getNextSet(), *getSBSMgr());
            }
            return;
        }
        if (from->getNextSet() != nullptr) {
            //Note if RegDef indicates PHI, it does not have Previous DEF,
            //because PHI has multiple Previous DEFs rather than single DEF.
            to->getNextSet()->bunion(*from->getNextSet(), *getSBSMgr());
        }
        return;
    }
    if (to->getNextSet() == nullptr || from->getNextSet() == nullptr) {
        return;
    }
    to->getNextSet()->bunion(*from->getNextSet(), *getSBSMgr());
}


//Remove RegDef from DefDef chain.
//Note the function does not deal with RegSSAInfo of IR occurrence, and just
//process DefDef chain that built on RegDef.
//regdef: will be removed from DefDef chain, and be modified as well.
//prev: previous Def to regdef, and will be modified.
//e.g:D1->D2
//     |->D3
//     |  |->D5
//     |  |->D6
//     |->D4
//  where D1 is predecessor of D2, D3 and D4; D3 is predecssor of D5, D6.
//  After remove D3:
//e.g:D1->D2
//     |->D5
//     |->D6
//     |->D4
//  where D1 is predecessor of D2, D5, D6, D4.
void RegSSAMgr::removeDefFromDDChainHelper(RegDef * regdef, RegDef * prev)
{
    ASSERT0(regdef);
    if (regdef->getNextSet() == nullptr) {
        if (prev != nullptr) {
            if (prev->getNextSet() != nullptr) {
                //Cutoff def-def chain between 'regdef' to its predecessor.
                prev->getNextSet()->remove(regdef, *getSBSMgr());
            } else {
                //Note if regdef indicates PHI, it does not have Previous DEF,
                //because PHI has multiple Previous DEFs rather than single
                //DEF.
                ASSERT0(regdef->is_phi());
            }
        }
        REGDEF_prev(regdef) = nullptr;
        getRegDUMgr()->removeRegDef(regdef);
        return;
    }
    if (prev != nullptr) {
        //CASE: Be careful that 'prev' should not belong to the NextSet of
        //regdef', otherwise the union operation of prev and regdef's succ DEF
        //will construct a cycle in DefDef chain, which is illegal.
        //e.g: for (i = 0; i < 10; i++) {;}, where i's Reg is Reg5.
        //  Reg5V2 <-- PHI(Reg5V--, Reg5V3)
        //  Reg5V3 <-- Reg5V2 + 1
        // If we regard Reg5V3 as the common-def, PHI is 'regdef', a cycle
        // will appeared.
        ASSERTN(!regdef->getNextSet()->find(prev),
                ("prev is actually the NEXT of regdef"));

        //Union successors of 'regdef' to its predecessor's next-set.
        unionSuccessors(regdef, prev);
        if (prev->getNextSet() != nullptr) {
            prev->getNextSet()->remove(regdef, *getSBSMgr());
        }
    }

    //Update successor's predecesor.
    RegDefSetIter nit = nullptr;
    for (BSIdx w = regdef->getNextSet()->get_first(&nit);
         w != BS_UNDEF; w = regdef->getNextSet()->get_next(w, &nit)) {
        RegDef const* use = getRegDUMgr()->getRegDef(w);
        ASSERTN(use->getPrev() == regdef, ("insanity DD chain"));
        REGDEF_prev(use) = prev;
    }
    REGDEF_prev(regdef) = nullptr;
    regdef->cleanNextSet(getRegDUMgr());
    getRegDUMgr()->removeRegDef(regdef);
}


//Remove RegDef from DefDef chain.
//regdef: will be removed from DefDef chain, and be modified as well.
//e.g:D1->D2
//     |->D3
//     |  |->D5
//     |  |->D6
//     |->D4
//  where D1 is predecessor of D2, D3 and D4; D3 is predecssor of D5, D6.
//  After remove D3:
//e.g:D1->D2
//     |->D5
//     |->D6
//     |->D4
//  where D1 is predecessor of D2, D5, D6, D4.
void RegSSAMgr::removeDefFromDDChain(
    RegDef * regdef, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(regdef);
    removeRegDefFromDDChain(regdef, regdef->getPrev(), ctx);
}


void RegSSAMgr::removeRegDefFromDDChain(
    RegDef * regdef, RegDef * prev, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(regdef);
    VReg const* prevdef_res = regdef->getResult();
    bool prevdef_is_phi = regdef->is_phi();
    IRBB const* prevdef_bb = regdef->getBB();
    IR const* prevdef_occ = prevdef_is_phi ? nullptr : regdef->getOcc();
    removeDefFromDDChainHelper(regdef, prev);
    if (prev != nullptr) {
        ASSERTN(prev != regdef, ("a cycle in DD chain."));
        ASSERT0(ctx.getRemovedVROpndIRList());
        buildDUChain(prev, *ctx.getRemovedVROpndIRList());
        return;
    }
    IRList * lst = ctx.getRemovedVROpndIRList();
    if (lst == nullptr) { return; }
    if (!ctx.need_update_duchain()) {
        //No need to update live-in DEF because the DOM info might not be
        //avaiable.
        return;
    }

    //Note VROpndSet of 'vr' may be empty after the removing.
    //It does not happen when RegSSA just constructed. The USE that
    //without real-DEF will have a virtual-DEF that version is
    //INIT_VERSION.
    //During some increment maintaining of RegSSA, 'vr' may be
    //removed, just like what current function does.
    //This means the current USE, 'use', does not have real-DEF stmt,
    //the value of 'use' always coming from parameter of global value.
    VRFindAndSetLiveInDef fs(this, *ctx.getOptCtx());
    RegSSAStatus st;
    fs.findAndSetForLst(*lst, prevdef_res, prevdef_is_phi, prevdef_bb,
                        prevdef_occ, st, &ctx);
}


Reg RegSSAMgr::getReg(IR const* ir) const
{
    ASSERT0(ir && hasRegSSAInfo(ir));
    ASSERT0(getRA());
    if (ir->getCode() == IR_PHYREG) { return PHYREG_reg(ir); }
    return getRA()->getReg(ir->getPrno());
}


SRegSet const* RegSSAMgr::getAliasRegSet(Reg reg) const
{
    return getTIMgr()->getAliasRegSet(reg);
}


bool RegSSAMgr::isPhiKillingDef(RegPhi const* phi) const
{
    if (phi->getNextSet() == nullptr) { return true; }
    Reg phi_result_reg = phi->getResultReg();
    ASSERT0(phi_result_reg);
    RegDefSetIter nit = nullptr;
    RegSSAMgr * pthis = const_cast<RegSSAMgr*>(this);
    for (BSIdx i = phi->getNextSet()->get_first(&nit);
         i != BS_UNDEF; i = phi->getNextSet()->get_next(i, &nit)) {
        RegDef const* next_def = pthis->getRegDUMgr()->getRegDef(i);
        ASSERTN(next_def && next_def->getPrev() == phi,
                ("insanity DD chain"));
        Reg next_result_reg = next_def->getResultReg();
        if (!isExactCover(next_result_reg, phi_result_reg)) {
            //There are DEFs represented by phi and its operand may pass
            //through 'def'. These DEFs belong to the MayDef set of
            //followed USE.
            return false;
        }
    }
    return true;
}


//Remove PHI that without any USE.
//Return true if any PHI was removed, otherwise return false.
bool RegSSAMgr::removePhiNoUse(RegPhi * phi, OptCtx const& oc)
{
    ASSERT0(phi && phi->is_phi() && phi->getBB());
    VReg * vr = phi->getResult();
    ASSERT0(vr && vr->is_reg());
    ASSERT0(phi == vr->getDef());
    if (vr->hasUse()) { return false; }

    Reg phi_result_reg = vr->reg();
    ASSERT0(phi_result_reg != REG_UNDEF);
    if (!isPhiKillingDef(phi)) { return false; }

    //Note RegPhi does not have previous DEF, because usually Phi has
    //multiple previous DEFs rather than single DEF.
    //CASE:compile/regssa_phi_prevdef.c
    //Sometime optimization may form CFG that cause PHI1
    //dominiates PHI2, then PHI1 will be PHI2's previous-DEF.
    //ASSERT0(phi->getPrev() == nullptr);

    RegDef * phiprev = nullptr;
    if (phi->isDefRealStmt()) {
        RegSSAStatus st;
        VReg * prev = findDomLiveInDefFromIDomOf(
            phi->getBB(), phi->getResult()->reg(), oc, st);
        if (prev != nullptr) {
            phiprev = prev->getDef();
        }
    }
    RegSSAUpdateCtx ctx(const_cast<OptCtx&>(oc));
    removePhiFromRegSSAMgr(phi, phiprev, ctx);
    return true;
}


//Check each USE|DEF of ir, remove the expired one which is not reference
//the memory any more that ir defined.
//Return true if DU changed.
bool RegSSAMgr::removeExpiredDU(IR const* ir)
{
    //TODO: Do NOT attempt to remove if not found reference Reg at USE point
    //which should correspond to vropnd->reg().
    //Some transformation, such as IR Refinement, may change
    //the USE's RegSet. This might lead to the inaccurate and
    //redundant RegSSA DU Chain. So the RegSSA DU Chain is conservative,
    //but the correctness of RegSSA dependence is garanteed.
    //e.g:
    //  ist:*<4> id:18 //:Reg11, Reg12, Reg14, Reg15
    //    lda: *<4> 'r'
    //    ild: i32 //MReg13: Reg16
    //      ld: *<4> 'q' //MReg18
    //=> After IR combination: ist(lda) transformed to st
    //  st:*<4> 'r' //MReg12
    //    ild : i32 //MReg13 : Reg16
    //      ld : *<4> 'q'    //MReg18
    //ist transformed to st. This reduce referenced RegSet to a single Reg
    //as well.
    return false;
}


void RegSSAMgr::removePhiFromDDChain(
    RegPhi * phi, RegDef * prev, RegSSAUpdateCtx const& ctx)
{
    ASSERT0(phi && phi->is_phi());
    removeRegDefFromDDChain(phi, prev, ctx);
}


//This function perform SSA destruction via scanning BB in sequential order.
void RegSSAMgr::destruction(MOD OptCtx & oc)
{
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    UINT bbnum = bblst->get_elem_count();
    BBListIter bbct;
    RegSSAUpdateCtx ctx(oc);
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        ASSERT0(bbct->val());
        destructBBSSAInfo(bbct->val(), &ctx);
    }
    if (bbnum != bblst->get_elem_count()) {
        oc.setInvalidIfCFGChanged();
    }
    set_valid(false);
}


//wl: is an optional parameter to record BB which expected to deal with.
//    It is a work-list that is used to drive iterative collection and
//    elimination of redundant PHI elmination.
//Return true if phi removed.
bool RegSSAMgr::removePhiHasCommonDef(
    List<IRBB*> * wl, RegPhi * phi, OptCtx const& oc)
{
    ASSERT0(phi);
    VReg * common_def = nullptr;
    if (!doOpndHaveSameDef(phi, &common_def)) {
        return false;
    }
    ASSERT0(common_def);

    //commond_def may be livein-DEF.
    //e.g:Phi: Reg10V3 <- Reg10V0
    //  The only operand of PHI is livein Reg.
    for (IR * opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VReg * vr = phi->getOpndVReg(opnd, &m_dumgr);
        ASSERTN(vr,
            ("at least init-version VROpnd should be attached in the 'opnd'"
             "if original non-init-version VROpnd removed"));
        ASSERT0(vr->is_reg());
        if (wl != nullptr && vr->getDef() != nullptr) {
            ASSERT0(vr->getDef()->getBB());
            wl->append_tail(vr->getDef()->getBB());
        }
    }
    if (common_def != phi->getResult() && !common_def->isLiveIn()) {
        //Change DEF from PHI to common_def to elements in UseList.
        ASSERT0(common_def->is_reg());

        //CASE: Be careful that 'prev' should not belong to the NextSet of
        //regdef', otherwise the union operation of prev and regdef's succ DEF
        //will construct a cycle in Def-Def chain, which is illegal.
        //e.g: for (i = 0; i < 10; i++) {;}, where i's Reg is Reg5.
        //  Reg5V2 <-- PHI(Reg5V--, Reg5V3)
        //  Reg5V3 <-- Reg5V2 + 1 #S1
        //Assume above code told us that Reg5V3 is the common_def, whereas we
        //are going to remove PHI because its operands have a common_def. If
        //we set #S1 (the common_def) to be previous DEF in def-def chain of
        //Reg5, a def-def cycle will appeared, of course it is invalid.
        RegDef * prev = common_def->getDef();
        ASSERT0(prev);
        if (phi->isInNextSet(prev, getRegDUMgr())) {
            //Avoid making a def-def cycle.
            prev = nullptr;
        }
        RegSSAUpdateCtx ctx(const_cast<OptCtx&>(oc));
        removePhiFromRegSSAMgr(phi, prev, ctx);
        return true;
    }
    if (common_def->isLiveIn()) {
        //CASE:In the case, the DEF of each operands are the same live-in DEF.
        //Replace each USE of PHI with the live-in DEF.
        replaceVROpndForAllUse(common_def, phi->getResult());
    }
    RegSSAUpdateCtx ctx(const_cast<OptCtx&>(oc));
    removePhiFromRegSSAMgr(phi, nullptr, ctx);
    return true;
}


//wl: is an optional parameter to record BB which expected to deal with.
//    It is a work-list that is used to drive iterative collection and
//    elimination of redundant PHI elmination.
//Return true if phi removed.
bool RegSSAMgr::removePhiHasNoValidDef(
    List<IRBB*> * wl, RegPhi * phi, OptCtx const& oc)
{
    ASSERT0(phi);
    if (doOpndHaveValidDef(phi)) {
        return false;
    }
    for (IR * opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VReg * vr = phi->getOpndVReg(opnd, &m_dumgr);
        //if (vr == nullptr) {
        //    //VROpnd may be have been removed from RegSSAMgr, thus the VROpnd
        //    //that corresponding to current ID is NULL.
        //    continue;
        //}
        ASSERTN(vr, ("init-version should be placed if vr removed"));

        ASSERT0(vr->is_reg());
        if (wl != nullptr && vr->getDef() != nullptr) {
            ASSERT0(vr->getDef()->getBB());
            wl->append_tail(vr->getDef()->getBB());
        }
    }

    //Do NOT do collection crossing PHI.
    VRCollectCtx clctx(COLLECT_IMM_USE);
    IRSet useset(getSBSMgr()->getSegMgr());
    if (phi->getPrev() != nullptr) {
        VRCollectUse cu(this, phi->getResult(), clctx, &useset);
    }
    RegSSAUpdateCtx ctx(const_cast<OptCtx&>(oc));
    removePhiFromRegSSAMgr(phi, phi->getPrev(), ctx);
    if (phi->getPrev() != nullptr) {
        //Set UseSet of PHI to be the USE of previous DEF.
        buildDUChain(phi->getPrev(), useset);
    }
    return true;
}


//wl: work list for temporary used.
//Return true if any PHI was removed.
bool RegSSAMgr::prunePhiForBB(
    IRBB const* bb, List<IRBB*> * wl, OptCtx const& oc)
{
    ASSERT0(bb);
    RegPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return false; }
    bool remove = false;
    RegPhiListIter prev = nullptr;
    RegPhiListIter next = nullptr;
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = next) {
        next = philist->get_next(it);
        RegPhi * phi = it->val();
        ASSERT0(phi);
        if (removePhiHasCommonDef(wl, phi, oc)) {
            remove = true;
            philist->remove(prev, it);
            continue;
        }
        if (removePhiHasNoValidDef(wl, phi, oc)) {
            remove = true;
            philist->remove(prev, it);
            continue;
        }

        //Remove PHI that without any USE.
        //TBD: PHI that without USE could not removed in some case:
        //e.g:for (...) { S1<---will insert PHI of x
        //      x[i]=0;   S2
        //    }
        //    x[j]=...;   S3
        //  where x[j] is NOT killing-def of x[i].
        //  ----
        //  RegSSAMgr inserted PHI of x, it is the previous DEF of S3.
        //  If we remove PHI at S1, S2 will lost USE at opnd of PHI, thus
        //  will be removed finally. It is incorrect.
        if (removePhiNoUse(phi, oc)) {
            remove = true;
            philist->remove(prev, it);
            continue;
        }
        prev = it;
    }
    return remove;
}


//Remove redundant phi.
//Return true if any PHI was removed.
bool RegSSAMgr::removeRedundantPhi(OptCtx const& oc)
{
    List<IRBB*> wl;
    return prunePhi(wl, oc);
}


//Remove redundant phi.
//wl: work list for temporary used.
//Return true if any PHI was removed.
bool RegSSAMgr::prunePhi(List<IRBB*> & wl, OptCtx const& oc)
{
    START_TIMER(t, "RegSSA: Prune phi");
    BBList * bblst = m_rg->getBBList();
    BBListIter ct;

    wl.clean();
    for (bblst->get_head(&ct); ct != bblst->end(); ct = bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        wl.append_tail(bb);
    }

    bool remove = false;
    IRBB * bb = nullptr;
    while ((bb = wl.remove_head()) != nullptr) {
        remove |= prunePhiForBB(bb, &wl, oc);
    }
    END_TIMER(t, "RegSSA: Prune phi");
    return remove;
}


//Reinitialize Reg SSA manager.
void RegSSAMgr::reinit(OptCtx const& oc)
{
    RegSSAUpdateCtx ctx(const_cast<OptCtx&>(oc));
    destroy(&ctx);
    m_max_version.destroy();
    m_max_version.init();
    m_dumgr.reinit();
    init();
}


//The function will attempt to remove the USE that located in outside loop BB.
//Note the function will NOT cross RegPhi.
bool RegSSAMgr::tryRemoveOutsideLoopUse(RegDef * def, LI<IRBB> const* li)
{
    VReg * res = def->getResult();
    VReg::UseSetIter it;
    UINT next_i;
    bool removed = false;
    VReg::UseSet * uset = res->getUseSet();
    for (UINT i = uset->get_first(it); !it.end(); i = next_i) {
        next_i = uset->get_next(it);
        IR * u = m_rg->getIR(i);
        ASSERT0(u);
        if (u->getCode() == IR_PHYREG) {
            RegPhi const* phi = PHYREG_phi(u);
            if (li->isInsideLoop(phi->getBB()->id())) { continue; }
        } else {
            IRBB const* bb = u->getStmt()->getBB();
            ASSERT0(bb);
            if (li->isInsideLoop(bb->id())) { continue; }
        }
        removeDUChain(def, u);
        removed = true;
    }
    return removed;
}


bool RegSSAMgr::verifyRegSSAInfo(Region const* rg, OptCtx const& oc)
{
    RegSSAMgr * ssamgr = (RegSSAMgr*)(rg->getPassMgr()->
        queryPass(PASS_REGSSA_MGR));
    if (ssamgr == nullptr || !ssamgr->is_valid()) { return true; }
    ASSERT0(ssamgr->verify());
    ASSERT0(ssamgr->verifyPhi());
    if (oc.is_dom_valid()) {
        ASSERT0(ssamgr->verifyVersion(oc));
    }
    return true;
}


//Duplicate Phi operand that is at the given position, and insert after
//given position sequently.
//pos: given position
//num: the number of duplication.
//Note caller should guarrentee the number of operand is equal to the
//number predecessors of BB of Phi.
void RegSSAMgr::dupAndInsertPhiOpnd(IRBB const* bb, UINT pos, UINT num)
{
    ASSERT0(bb && num >= 1);
    RegPhiList * philist = getPhiList(bb);
    if (philist == nullptr || philist->get_elem_count() == 0) {
        return;
    }
    ASSERTN(0, ("TO BE CHECK"));
    for (RegPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        RegPhi * phi = it->val();
        ASSERT0(phi->getOpndNum() == bb->getNumOfPred());
        IR * opnd = phi->getOpnd(pos);
        ASSERTN(opnd, ("RegPHI does not contain such many operands."));
        RegSSAInfo * opndinfo = getRegSSAInfoIfAny(opnd);
        ASSERT0(opndinfo || opnd->getCode() != IR_PHYREG);
        for (UINT i = 0; i < num; i++) {
            IR * newopnd = m_rg->dupIR(opnd);
            phi->insertOpndAfter(opnd, newopnd);
            if (opndinfo != nullptr) {
                addUseToRegSSAInfo(newopnd, opndinfo);
            }
        }
    }
}


//Return true if stmt dominates use's stmt, otherwise return false.
bool RegSSAMgr::isStmtDomUseInsideLoop(
    IR const* stmt, IR const* use, LI<IRBB> const* li) const
{
    IRBB const* usestmtbb = nullptr;
    if (use->getCode() == IR_PHYREG) {
        RegPhi const* phi = PHYREG_phi(use);
        ASSERT0(phi && phi->is_phi());
        usestmtbb = phi->getBB();
    } else {
        ASSERT0(use->getStmt());
        usestmtbb = use->getStmt()->getBB();
    }
    ASSERT0(usestmtbb);

    if (!li->isInsideLoop(usestmtbb->id())) {
        //Only check dominiation info inside loop.
        return true;
    }

    IRBB const* defstmtbb = stmt->getBB();
    ASSERT0(defstmtbb);
    if (defstmtbb != usestmtbb &&
        m_cfg->is_dom(defstmtbb->id(), usestmtbb->id())) {
        return true;
    }
    if (defstmtbb == usestmtbb) {
        if (use->getCode() == IR_PHYREG) { //use's stmt is RegPhi
            //stmt can not dominate PHI because PHI is always
            //in the front of BB.
            return false;
        }
        IR const* ustmt = use->getStmt();
        ASSERT0(ustmt);
        return defstmtbb->is_dom(stmt, ustmt, true);
    }

    return false;
}


//Return true if ir dominates all its USE expressions which inside loop.
//In ssa mode, stmt's USE may be placed in operand list of PHI.
bool RegSSAMgr::isStmtDomAllUseInsideLoop(
    IR const* ir, LI<IRBB> const* li) const
{
    ASSERT0(ir && ir->getBB());
    ConstRegSSAUSEIRIter it(this);
    for (IR const* use = it.get_first(ir);
        use != nullptr; use = it.get_next()) {
        if (!isStmtDomUseInsideLoop(ir, use, li)) {
            return false;
        }
    }
    return true;
}


//Move RegPhi from 'from' to 'to'.
//This function often used in updating PHI when adding new dominater
//BB to 'to'.
void RegSSAMgr::movePhi(IRBB * from, IRBB * to)
{
    ASSERT0(from && to && from != to);
    RegPhiList * from_philist = getPhiList(from);
    if (from_philist == nullptr || from_philist->get_elem_count() == 0) {
        return;
    }
    RegPhiList * to_philist = m_dumgr.genBBPhiList(to->id());
    RegPhiListIter to_it = to_philist->get_head();
    for (RegPhiListIter from_it = from_philist->get_head();
         from_it != from_philist->end();
         from_it = from_philist->get_next(from_it)) {

        //Move RegPhi from 'from' to 'to'.
        RegPhi * phi = from_it->val();
        REGPHI_bb(phi) = to;
        if (to_it == nullptr) {
            //'to' BB does not have PHI list.
            to_it = to_philist->append_head(phi);
        } else {
            //Make sure phi's order in 'to' is same with 'from'.
            to_it = to_philist->insert_after(phi, to_it);
        }
    }
    from_philist->clean();
}


//Return true if the value of ir1 and ir2 are definitely same, otherwise
//return false to indicate unknown.
bool RegSSAMgr::hasSameValue(IR const* ir1, IR const* ir2) const
{
    ASSERT0(hasRegSSAInfo(ir1) && hasRegSSAInfo(ir2));
    RegSSAInfo const* info1 = getRegSSAInfoIfAny(ir1);
    RegSSAInfo const* info2 = getRegSSAInfoIfAny(ir2);
    ASSERT0(info1 && info2);
    return info1->isEqual(*info2);
}


//Return true if there is at least one USE 'vreg' has been placed in given
//IRBB 'bbid'.
//vreg: the function will iterate all its USE occurrences.
//it: for tmp used.
bool RegSSAMgr::isUseWithinBB(
    VReg const* vreg, MOD VReg::UseSetIter & it, UINT bbid) const
{
    ASSERT0(vreg && vreg->is_reg());
    it.clean();
    VReg::UseSet const* uset = const_cast<VReg*>(vreg)->getUseSet();
    for (UINT i = uset->get_first(it); !it.end(); i = uset->get_next(it)) {
        IR * u = m_rg->getIR(i);
        ASSERT0(u);
        if (u->getCode() == IR_PHYREG) {
            RegPhi const* phi = PHYREG_phi(u);
            if (phi->getBB()->id() == bbid) { return true; }
            continue;
        }
        IRBB const* bb = u->getStmt()->getBB();
        ASSERT0(bb);
        if (bb->id() == bbid) { return true; }
    }
    return false;
}


bool RegSSAMgr::isOverConservativeDUChain(IR const* ir1, IR const* ir2) const
{
    ASSERT0(ir1 && ir2 && ir1 != ir2);
    if (ir1->isNotOverlap(ir2, m_rg)) { return true; }
    Reg must1 = getReg(ir1);
    Reg must2 = getReg(ir2);
    if (must1 == REG_UNDEF || must2 == REG_UNDEF || must2 == must1) {
        //If MustDef is empty, for conservative purpose, 'defir' is
        //the MayDef of 'exp', thus we have to consider the
        //inexact DU relation.
        return false;
    }
    return !isAlias(must1, must2);
}


//Return true if 'def' is NOT the real-def of mustref, and can be ignored.
//phi: one of DEF of exp.
static bool canIgnorePhi(
    IR const* exp, RegPhi const* phi, RegSSAMgr const* mgr, MOD PhiTab & visit)
{
    ASSERT0(exp && exp->is_exp() && phi->is_phi());
    visit.append(phi);
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        RegSSAInfo const* info = mgr->getRegSSAInfoIfAny(opnd);
        ASSERT0(info);
        VReg * vreg = (VReg*)info->readVROpndSet().get_unique(mgr);
        ASSERT0(vreg && vreg->is_reg());
        RegDef const* def = vreg->getDef();
        if (def == nullptr) { continue; }
        if (def->is_phi()) {
            if (visit.get((RegPhi const*)def) != nullptr ||
                canIgnorePhi(exp, (RegPhi const*)def, mgr, visit)) {
                continue;
            }
            return false;
        }
        IR const* defir = def->getOcc();
        if (!mgr->isOverConservativeDUChain(defir, exp)) {
            return false;
        }
    }
    return true;
}


//Return true if the DU chain between 'def' and 'use' can be ignored during
//DU chain manipulation.
//def: one of DEF of exp.
//exp: expression.
bool RegSSAMgr::isOverConservativeDUChain(
    RegDef const* def, IR const* exp) const
{
    ASSERT0(exp->is_exp() && hasExpRegSSAInfo(exp));
    Reg mustref = getReg(exp);
    if (mustref == REG_UNDEF) {
        //If MustRef is empty, for conservative purpose, 'def' is the MayDef
        //of 'exp', thus we have to consider the inexact DU relation.
        return false;
    }
    if (def->is_phi()) {
        PhiTab phitab;
        return canIgnorePhi(exp, (RegPhi const*)def, this, phitab);
    }
    return isOverConservativeDUChain(def->getOcc(), exp);
}


void RegSSAMgr::initDepPass(OptCtx & oc)
{
    ASSERT0(m_rg);
    m_ra = (LinearScanRA*)m_rg->getPassMgr()->registerPass(PASS_LINEAR_SCAN_RA);
    m_timgr = &m_ra->getTIMgr();
}


void RegSSAMgr::construction(OptCtx & oc)
{
    START_TIMER(t0, "RegSSA: Construction");
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    ASSERT0(oc.is_ref_valid());
    ASSERT0(oc.is_dom_valid());
    reinit(oc);
    initDepPass(oc);
    //Extract dominate tree of CFG.
    START_TIMER(t1, "RegSSA: Extract Dom Tree");
    DomTree domtree;
    m_cfg->genDomTree(domtree);
    END_TIMER(t1, "RegSSA: Extract Dom Tree");
    if (!construction(domtree, oc)) {
        return;
    }
    set_valid(true);
    END_TIMER(t0, "RegSSA: Construction");
}


void RegSSAMgr::dumpBBListWithRegSSAInfo() const
{
    class DumpIRWithRegSSAInfo : public IRDumpCustomBaseFunc {
    public:
        RegSSAMgr const* regssamgr;
    public:
        virtual void dumpCustomAttr(
            OUT xcom::DefFixedStrBuf & buf, Region const* rg, IR const* ir,
            DumpFlag dumpflag) const override
        {
            if (ir->is_stmt()) {
                regssamgr->dumpRegSSAInfoForStmt(buf, ir);
                return;
            }
            if (ir->is_exp()) {
                regssamgr->dumpRegSSAInfoForExp(buf, ir);
                return;
            }
            //There might be some error occurred.
            //Do not assert and keep dumpping.
        }
    };
 
    class DumpBBWithRegSSAInfo : public BBDumpCtx<> {
    public:
        RegSSAMgr const* regssamgr;
    public:
        virtual void dumpProlog(
            Region const* rg, IRBB const* bb) const override
        {
            ASSERT0(bb);
            RegPhiList const* philist = regssamgr->getPhiList(bb);
            if (philist == nullptr) { return; }
            regssamgr->dumpPhiList(philist);
        }
        virtual void dumpEpilog(
            Region const* rg, IRBB const* bb) const override
        {}
    };
    DumpBBWithRegSSAInfo bbdumpctx;
    bbdumpctx.regssamgr = this;
    DumpFlag f = DumpFlag::combineIRID(IR_DUMP_KID | IR_DUMP_SRC_LINE);
    DumpIRWithRegSSAInfo cf;
    cf.regssamgr = this;
    IRDumpCtx<> irdumpctx(4, f, nullptr, &cf);
    BBDumpCtxMgr<> ctx(&irdumpctx, &bbdumpctx);
    ASSERT0(m_rg->getBBList());
    xoc::dumpBBList(m_rg->getBBList(), m_rg, false, &ctx);
}


bool RegSSAMgr::construction(DomTree & domtree, OptCtx & oc)
{
    ASSERT0(m_rg);
    START_TIMER(t1, "RegSSA: Build dominance frontier");
    DfMgr dfm;
    dfm.build((xcom::DGraph&)*m_cfg); //Build dominance frontier.
    END_TIMER(t1, "RegSSA: Build dominance frontier");
    if (dfm.hasHighDFDensityVertex((xcom::DGraph&)*m_cfg)) {
        return false;
    }
    List<IRBB*> wl;
    DefMiscBitSetMgr bs_mgr;
    DefRegSet effect_regs(bs_mgr.getSegMgr());
    BB2DefRegSet defed_regs;
    placePhi(dfm, effect_regs, bs_mgr, defed_regs, wl);

    //Perform renaming.
    Reg2VRegStack reg2vregstk;
    rename(effect_regs, defed_regs, domtree, reg2vregstk);

    //Note you can clean version stack after renaming.
    ASSERT0(verifyPhi());
    prunePhi(wl, oc);
    cleanLocalUsedData();
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRegSSAMgr()) {
        dump();
    }
    set_valid(true);
    ASSERT0(verify());
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(verifyPhi() && verifyAllVReg() && verifyVersion(oc));
    return true;
}
//END RegSSAMgr

} //namespace xoc
