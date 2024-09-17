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

static void recordInVex(Vertex const* v, VexTab & vt)
{
    vt.add(v);
    AdjVertexIter it;
    for (Vertex const* t = Graph::get_first_in_vertex(v, it);
         t != nullptr; t = Graph::get_next_in_vertex(it)) {
        vt.add(t);
    }
}


static void recordDomVexForEachElem(IRCFG const* cfg, MOD VexTab & vt)
{
    VexTab tmp;
    VexTabIter it;
    for (VexIdx t = vt.get_first(it);
         t != VERTEX_UNDEF; t = vt.get_next(it)) {
        DomSet const* ds = cfg->get_dom_set(t);
        if (ds == nullptr) {
            //Some vertex in 'vt' has been removed from CFG.
            continue;
        }
        for (BSIdx i = ds->get_first();
             i != BS_UNDEF ; i = ds->get_next(i)) {
            ASSERT0(i != VERTEX_UNDEF);
            Vertex const* v = cfg->getVertex(i);
            ASSERT0(v);
            tmp.add(v);
        }
    }
    VexTabIter it2;
    for (VexIdx t = tmp.get_first(it2);
         t != VERTEX_UNDEF; t = tmp.get_next(it2)) {
        vt.append(t);
    }
}


static void recordOutVex(Vertex const* v, VexTab & vt)
{
    vt.add(v);
    AdjVertexIter it;
    for (Vertex const* t = Graph::get_first_out_vertex(v, it);
         t != nullptr; t = Graph::get_next_out_vertex(it)) {
        vt.add(t);
    }
}


static Vertex const* findRoot(Vertex const* from, VexTab const& vt,
                              DGraph const* cfg)
{
    //CAUTION:LCA query is costly.
    DomTree ldt;
    cfg->genDomTree(ldt);
    ldt.computeHeight();
    xcom::BinLCA lca(&ldt);
    VexTabIter it;
    VexIdx highest = VERTEX_UNDEF;
    for (VexIdx t = vt.get_first(it);
         t != VERTEX_UNDEF; t = vt.get_next(it)) {
        VexIdx idom = lca.query(from->id(), t);
        ASSERT0(idom != VERTEX_UNDEF);
        if (highest == VERTEX_UNDEF) {
            highest = idom;
            continue;
        }
        if (ldt.hasLowerHeight(idom, highest)) {
            highest = idom;
            continue;
        }
    }
    ASSERT0(highest != VERTEX_UNDEF);
    Vertex const* root = cfg->getVertex(highest);
    ASSERT0(root);
    return root;
}


static void postProcessReached(Vertex const* from, Vertex const* to,
                               MOD RCE * rce, MOD RCECtx & rcectx)
{
    VexTab modset;
    Vertex const* root = nullptr;
    UINT iter_times = 0;
    rce->getCFG()->reviseDomInfoAfterAddOrRemoveEdge(
        from, to, &modset, root, iter_times);
    ASSERT0(root);
    if (root == from) {
        recordOutVex(from, modset);
        recordInVex(to, modset);
        recordOutVex(to, modset);
    } else if (root == to) {
        recordOutVex(to, modset);
        recordInVex(from, modset);
        recordOutVex(from, modset);
    } else {
        recordInVex(to, modset);
        recordOutVex(to, modset);
        recordInVex(from, modset);
        recordOutVex(from, modset);
    }
    recordDomVexForEachElem(rce->getCFG(), modset);
    CfgOptCtx cfgoptctx(rcectx.oc, &rce->getActMgr());
    rce->getCFG()->reviseMDSSA(modset, root, cfgoptctx);

    //RPO is unchanged if only removing branch-edge.
    //DOM may changed.
    // BB1--->BB3------
    //  |      |       |
    //  |      v       v
    //   ----BB2----->BB4
    // If removing BB3->BB4, idom of BB4 changed.
    // DOM has been maintained by removeEdge().
    OptCtx::setInvalidIfCFGChangedExcept(
        &rcectx.oc, PASS_RPO, PASS_DOM, PASS_UNDEF);
    rcectx.cfg_changed = true;
    ASSERT0(rce->getCFG()->verifyDomAndPdom(rcectx.oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(rce->getRegion(), rcectx.oc));
}


static Vertex const* findMinimalRoot(RCE const* rce)
{
    //TODO:find minimal root vertex rather than CFG entry.
    //VexTab border;
    //Vertex const* root = findRoot(from, border, rce->getCFG());
    //ASSERT0(root);
    DUMMYUSE(findRoot); //Avoid gcc complaint.

    //WORDAROUND:Use cfg's entry as root.
    return rce->getCFG()->getEntry()->getVex();
}


static void postProcessUnreached(Vertex const* from, Vertex const* to,
                                 MOD RCE * rce, MOD RCECtx & rcectx)
{
    //Removing edge will cause unreachable code to appear.
    //CASE:rce2.c, licm_hoist.c.
    CfgOptCtx ctx(rcectx.oc);

    //Unreachable-code will confuse DomInfo computation.
    CFGOPTCTX_need_update_dominfo(&ctx) = false;
    RemoveUnreachBBCtx rmunrchctx;
    bool removed = rce->getCFG()->removeUnreachBB(ctx, &rmunrchctx);
    ASSERT0_DUMMYUSE(removed);

    //CFG related info is incorrect if unreachable code removed.
    //e.g:rce_updatedom2.c
    rcectx.oc.setInvalidIfCFGChanged();

    //DomInfo is necessary for subsequently SSA updation.
    VexTab modset;
    UINT iter_times = 0;
    Vertex const* root = findMinimalRoot(rce);

    //Recompute DOM need RPO.
    rce->getRegion()->getPassMgr()->checkValidAndRecompute(
        &rcectx.oc, PASS_RPO, PASS_UNDEF);

    //Recompute the DOM for subgraph.
    rce->getCFG()->recomputeDomInfoForSubGraph(root, &modset, iter_times);
    rcectx.oc.setValidPass(PASS_DOM);

    //Update the vertex's DomInfo as a result.
    modset.add(rmunrchctx.getVexTab());
    recordDomVexForEachElem(rce->getCFG(), modset);

    //Fixup SSA info according DOM.
    CfgOptCtx cfgoptctx(rcectx.oc, &rce->getActMgr());
    rce->getCFG()->reviseMDSSA(modset, root, cfgoptctx);

    //Update status of CFG of RCECtx.
    rcectx.cfg_changed = true;

    //Do some verification at last.
    ASSERT0(rce->getCFG()->verifyDomAndPdom(rcectx.oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(rce->getRegion(), rcectx.oc));
}


static void postProcessAfterRemoveEdge(
    Vertex const* from, Vertex const* to, MOD RCE * rce, MOD RCECtx & rcectx)
{
    ASSERT0(from->getInDegree() > 0);
    ASSERT0(rce->getCFG()->getEntry());
    Vertex const* entry = rce->getCFG()->getEntry()->getVex();
    ASSERT0(entry);
    bool try_failed = false;
    bool reach = Graph::isReachIn(to, entry, (UINT)-1, try_failed);
    ASSERTN(!try_failed, ("tried too much"));
    if (reach) {
        postProcessReached(from, to, rce, rcectx);
        return;
    }
    postProcessUnreached(from, to, rce, rcectx);
}


static void dumpInit(RCE const* pass)
{
    if (!g_dump_opt.isDumpAfterPass() || !g_dump_opt.isDumpRCE()) {
        return;
    }
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n\n==---- DUMP %s ----==", pass->getPassName());
    rg->getLogMgr()->incIndent(2);
}


static void dumpFini(RCE const* pass)
{
    if (!g_dump_opt.isDumpAfterPass() || !g_dump_opt.isDumpRCE()) {
        return;
    }
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    rg->getLogMgr()->decIndent(2);
}


static void dumpRemovedIR(IR const* ir, RCE const* rce)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region * rg = rce->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\nREMOVE IR:");
    xoc::dumpIR(ir, rg, DumpFlag::combineIRID(IR_DUMP_KID));
}


static void dumpRemovedEdge(IRBB const* from, IRBB const* to, RCE const* pass)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\nREMOVE EDGE: BB%d->BB%d", from->id(), to->id());
}


static void dumpChangedIR(IR const* oldir, IR const* newir, RCE const* pass)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region * rg = pass->getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\nCHANGE:");
    rg->getLogMgr()->incIndent(2);
    note(rg, "\nOLD:");
    xoc::dumpIR(oldir, rg, DumpFlag::combineIRID(IR_DUMP_KID));
    note(rg, "\nNEW:");
    if (newir != nullptr) {
        xoc::dumpIR(newir, rg, DumpFlag::combineIRID(IR_DUMP_KID));
    } else {
        note(rg, "\nNULL");
    }
    rg->getLogMgr()->decIndent(2);
}


//If 'ir' is always true, set 'must_true', or if it is
//always false, set 'must_false'.
//Return true if this function is able to determine the result of 'ir',
//otherwise return false that it does know nothing about ir.
bool RCE::calcCondMustVal(IR const* ir, OUT bool & must_true,
                          OUT bool & must_false, OptCtx const& oc) const
{
    must_true = false;
    must_false = false;
    ASSERT0(ir->is_judge());
    switch (ir->getCode()) {
    SWITCH_CASE_COMPARE:
    SWITCH_CASE_LOGIC: {
        if (ir->is_const()) {
            if (CONST_int_val(ir) == 1) {
                must_true = true;
            } else if (CONST_int_val(ir) == 0) {
                must_false = true;
            }
            return true;
        }
        if (m_gvn != nullptr && m_gvn->is_valid()) {
            return m_gvn->calcCondMustVal(ir, must_true, must_false);
        }
        break;
    }
    default: UNREACHABLE();
    }
    return false;
}


static bool regardAsMustTrue(IR const* ir)
{
    ASSERT0(ir->is_const());
    if (ir->is_int() || ir->is_ptr()) {
        if (CONST_int_val(ir) != 0) { return true; }
        return false;
    }
    if (ir->is_str() || ir->is_vec() || ir->is_mc()) { return true; }
    if (g_is_opt_float && ::fabs(CONST_fp_val(ir) - (HOST_FP)0.0) > EPSILON) {
        return true;
    }
    return false;
}


static bool regardAsMustFalse(IR const* ir)
{
    ASSERT0(ir->is_const());
    if (ir->is_int() || ir->is_ptr()) {
        if (CONST_int_val(ir) == 0) { return true; }
        return false;
    }
    if (ir->is_str() || ir->is_vec() || ir->is_mc()) { return false; }
    if (g_is_opt_float && ::fabs(CONST_fp_val(ir) - (HOST_FP)0.0) <= EPSILON) {
        return true;
    }
    return false;
}


//If 'ir' is always true, set 'must_true', or if it is
//always false, set 'must_false'.
//Return the changed ir.
IR * RCE::calcCondMustVal(
    MOD IR * ir, OUT bool & must_true, OUT bool & must_false,
    OUT bool & changed, MOD OptCtx & oc)
{
    must_true = false;
    must_false = false;
    ASSERT0(ir->is_judge());
    switch (ir->getCode()) {
    SWITCH_CASE_COMPARE:
    SWITCH_CASE_LOGIC: {
        RefineCtx rc(&oc);
        ir = m_refine->foldConst(ir, changed, rc);
        if (changed && ir->is_const()) {
            if (regardAsMustTrue(ir)) {
                must_true = true;
            } else if (regardAsMustFalse(ir)) {
                must_false = true;
            }
            return ir;
        }
        if (m_gvn == nullptr || !m_gvn->is_valid()) {
            //GVN is inavailable.
            return ir;
        }
        if (changed) {
            bool vn_changed = false;
            m_gvn->computeVN(ir, vn_changed);
        }
        bool succ = m_gvn->calcCondMustVal(ir, must_true, must_false);
        if (!succ) { return ir; }
        changed = true;
        if (must_true) {
            ASSERT0(!must_false);
            Type const* type = ir->getType();
            xoc::removeUseForTree(ir, m_rg, oc);
            m_rg->freeIRTree(ir);
            return m_irmgr->buildImmInt(1, type);
        }
        ASSERT0(must_false);
        Type const* type = ir->getType();
        xoc::removeUseForTree(ir, m_rg, oc);
        m_rg->freeIRTree(ir);
        return m_irmgr->buildImmInt(0, type);
    }
    default: UNREACHABLE();
    }
    return ir;
}


IR * RCE::processFalsebr(IR * ir, IR * new_det, bool must_true,
                         bool must_false, bool changed, MOD RCECtx & ctx)
{
    IR * old_det = BR_det(ir);
    BR_det(ir) = nullptr;
    if (must_true) {
        //FALSEBR(0x1), never jump.
        //Revise m_cfg. remove branch edge.
        IRBB * from = ir->getBB();
        IRBB * to = m_cfg->getTargetBB(from);
        ASSERT0(from != nullptr && to != nullptr);
        xoc::removeStmt(ir, m_rg, ctx.oc);

        //Revise the PHI operand to target successor.
        dumpRemovedEdge(from, to, this);
        CfgOptCtx coctx(ctx.oc);
        m_cfg->removeEdge(from, to, coctx);
        postProcessAfterRemoveEdge(from->getVex(), to->getVex(), this, ctx);
        //Do NOT free old ir here, leave it to the caller.
        return nullptr;
    }
    if (must_false) {
        //FALSEBR(0x0), always jump.
        IRBB * from = ir->getBB();
        IRBB * to = m_cfg->getFallThroughBB(from);
        ASSERT0(from != nullptr && to != nullptr);

        IR * newbr = m_irmgr->buildGoto(BR_lab(ir));
        xoc::removeStmt(ir, m_rg, ctx.oc);

        //Revise the PHI operand to fallthrough successor.
        //Revise m_cfg. remove fallthrough edge.
        dumpRemovedEdge(from, to, this);
        CfgOptCtx coctx(ctx.oc);
        CFGOPTCTX_need_update_dominfo(&coctx) = false;
        m_cfg->removeEdge(from, to, coctx);
        postProcessAfterRemoveEdge(from->getVex(), to->getVex(), this, ctx);
        return newbr;
    }
    ASSERT0(BR_det(ir) == nullptr);
    if (changed) {
        if (!new_det->is_judge()) {
            new_det = m_irmgr->buildJudge(new_det);
        }
        BR_det(ir) = new_det;
        ir->setParent(new_det);
        dumpChangedIR(old_det, new_det, this);
        return ir;
    }

    //Resume original det.
    BR_det(ir) = new_det;
    return ir;
}


IR * RCE::processTruebr(IR * ir, IR * new_det, bool must_true,
                        bool must_false, bool changed, MOD RCECtx & ctx)
{
    IR * old_det = BR_det(ir);
    BR_det(ir) = nullptr;
    if (must_true) {
        //TRUEBR(0x1), always jump.
        IRBB * from = ir->getBB();
        IRBB * to = m_cfg->getFallThroughBB(from);
        ASSERT0(from != nullptr && to != nullptr);
        IR * newbr = m_irmgr->buildGoto(BR_lab(ir));
        xoc::removeStmt(ir, m_rg, ctx.oc);
        //Revise the PHI operand to fallthrough successor.
        //Revise CFG. Remove fallthrough edge.
        dumpRemovedEdge(from, to, this);
        CfgOptCtx coctx(ctx.oc);
        m_cfg->removeEdge(from, to, coctx);
        postProcessAfterRemoveEdge(from->getVex(), to->getVex(), this, ctx);
        return newbr;
    }
    if (must_false) {
        //TRUEBR(0x0), never jump.
        //Revise CFG. Remove branch edge.
        IRBB * from = ir->getBB();
        IRBB * to = m_cfg->findBBbyLabel(BR_lab(ir));
        ASSERT0(from && to);
        xoc::removeStmt(ir, m_rg, ctx.oc);

        //Revise the PHI operand to target successor.
        dumpRemovedEdge(from, to, this);
        CfgOptCtx coctx(ctx.oc);
        m_cfg->removeEdge(from, to, coctx);
        postProcessAfterRemoveEdge(from->getVex(), to->getVex(), this, ctx);
        return nullptr;
    }
    ASSERT0(BR_det(ir) == nullptr);
    if (changed) {
        if (!new_det->is_judge()) {
            new_det = m_irmgr->buildJudge(new_det);
        }
        BR_det(ir) = new_det;
        ir->setParent(new_det);
        dumpChangedIR(old_det, new_det, this);
        return ir;
    }

    //Resume original det.
    BR_det(ir) = new_det;
    return ir;
}


IR * RCE::processBranch(IR * ir, MOD RCECtx & ctx)
{
    ASSERT0(ir->isConditionalBr());
    bool must_true, must_false, changed = false;
    IR * new_det = calcCondMustVal(BR_det(ir), must_true, must_false,
                                   changed, ctx.oc);
    if (ir->is_truebr()) {
        return processTruebr(ir, new_det, must_true, must_false, changed, ctx);
    }
    ASSERT0(ir->is_falsebr());
    return processFalsebr(ir, new_det, must_true, must_false, changed, ctx);
}


//Perform dead store elmination: x = x;
IR * RCE::processStore(IR * ir, RCECtx const& ctx)
{
    ASSERT0(ir->is_st());
    if (ST_rhs(ir)->getExactRef() == ir->getExactRef()) {
        dumpRemovedIR(ir, this);
        xoc::removeUseForTree(ir, m_rg, ctx.oc);
        return nullptr;
    }
    return ir;
}


//Perform dead store elmination: x = x;
IR * RCE::processStorePR(IR * ir, RCECtx const& ctx)
{
    ASSERT0(ir->is_stpr());
    if (STPR_rhs(ir)->getExactRef() == ir->getExactRef()) {
        dumpRemovedIR(ir, this);
        xoc::removeUseForTree(ir, m_rg, ctx.oc);
        return nullptr;
    }
    return ir;
}


bool RCE::performSimplyRCEForBB(IRBB * bb, MOD RCECtx & ctx)
{
    bool change = false;
    BBIRList * ir_list = &BB_irlist(bb);
    IRListIter ct;
    IRListIter next_ct;
    for (ir_list->get_head(&next_ct), ct = next_ct;
         ct != nullptr; ct = next_ct) {
        IR * ir = ct->val();
        ir_list->get_next(&next_ct);
        if (ir->hasSideEffect(true) || ir->isDummyOp()) { continue; }
        IR * newir = ir;
        switch (ir->getCode()) {
        SWITCH_CASE_CONDITIONAL_BRANCH_OP:
            newir = processBranch(ir, ctx);
            break;

        //This case has been dealt in ir_refine.
        //SWITCH_CASE_DIRECT_MEM_STMT:
        //    newir = processStore(ir);
        //    break;
        default:;
        }
        if (newir == ir) { continue; }
        dumpChangedIR(ir, newir, this);
        ir_list->remove(ct);
        m_rg->freeIRTree(ir);
        if (newir != nullptr) {
            if (next_ct != nullptr) {
                ir_list->insert_before(newir, next_ct);
            } else {
                ir_list->append_tail(newir);
            }
        }
        change = true;
    }
    return change;
}


//e.g:
//1. if (a == a) { ... } , remove redundant comparation.
//2. b = b; remove redundant store.
bool RCE::performSimplyRCE(MOD RCECtx & ctx)
{
    BBList * bbl = m_rg->getBBList();
    bool change = false;
    BBListIter ct_bb;
    for (IRBB * bb = bbl->get_head(&ct_bb);
         bb != nullptr; bb = bbl->get_next(&ct_bb)) {
        change |= performSimplyRCEForBB(bb, ctx);
    }
    return change;
}


bool RCE::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_cfg_valid()) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    m_refine = (Refine*)m_rg->getPassMgr()->queryPass(PASS_REFINE);
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //DCE use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //DCE use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    START_TIMER(t, getPassName());
    if (is_use_gvn() && m_gvn == nullptr) {
        //GVN is not ready.
        return false;
    }
    //Incremental update DOM need RPO.
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_RPO, PASS_GVN, PASS_UNDEF);
    ASSERT0(m_gvn->is_valid());
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    dumpInit(this);
    RCECtx ctx(oc);
    bool change = performSimplyRCE(ctx);
    if (ctx.cfg_changed) {
        ASSERT0(change);
        //CASE:rce_cfgopt.c
        //Remove unreachable code firstly before recomputing RPO and DOM.
        CfgOptCtx ctx(oc);
        //Unreachable-code will confuse DomInfo computation.
        CFGOPTCTX_need_update_dominfo(&ctx) = false;
        bool res = m_cfg->removeUnreachBB(ctx, nullptr);
        if (res) {
            //CFG related info is incorrect if unreachable code removed.
            //e.g:rce_updatedom2.c
            oc.setInvalidIfCFGChanged();
        }
        //DomInfo is needed when updating SSA info.
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);

        //TBD:Sometimes, CFG is complicated, maintain Dom and SSA is costly.
        //Thus you should apply a cost-model to drive the updation of Dom and
        //SSA. For now, we still choose to update Dom and SSA as much as
        //possible, whereas the compilation time can be unbearable.
        //CASE:Do NOT update Dom Info and SSA Info, because abnormal insane
        //CFG will confuse the Dom and SSA algorithm. When a edge removed and
        //there may have unreachable BB appeared, which MDSSAMgr does not known
        //these abnormal predecessors.
        //e.g:alias.loop.c
        CFGOPTCTX_need_update_dominfo(&ctx) = true;
        change |= m_cfg->performMiscOpt(ctx);
    }
    if (change) {
        //CFG changed, remove empty BB.
        //TODO:DO not recompute whole SSA/MDSSA. Instead, update SSA/MDSSA
        //info especially PHI operands incrementally.
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
        ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
        ASSERT0(m_cfg->verifyRPO(oc));
        ASSERT0(m_cfg->verifyLoopInfo(oc));
        ASSERT0(m_cfg->verifyDomAndPdom(oc));
    } else {
        m_rg->getLogMgr()->cleanBuffer();
    }
    dumpFini(this);
    END_TIMER(t, getPassName());
    return change;
}

} //namespace xoc
