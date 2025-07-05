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


static Vertex const* findRoot(
    Vertex const* from, VexTab const& vt, DGraph const* cfg)
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


static void postProcessReached(
    Vertex const* from, Vertex const* to, MOD RCE * rce, MOD RCECtx & rcectx)
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
    IRCfgOptCtx cfgoptctx(rcectx.getOptCtx(), rcectx.getActMgr());
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
        rcectx.getOptCtx(), PASS_RPO, PASS_DOM, PASS_UNDEF);
    rcectx.cfg_changed = true;
    ASSERT0(rce->getCFG()->verifyDomAndPdom(*rcectx.getOptCtx()));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(rce->getRegion(), *rcectx.getOptCtx()));
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


static void postProcessUnreached(
    Vertex const* from, Vertex const* to, MOD RCE * rce, MOD RCECtx & rcectx)
{
    //Removing edge will cause unreachable code to appear.
    //CASE:rce2.c, licm_hoist.c.
    IRCfgOptCtx ctx(rcectx.getOptCtx());

    //Unreachable-code will confuse DomInfo computation.
    CFGOPTCTX_need_update_dominfo(&ctx) = false;
    RemoveUnreachBBCtx rmunrchctx;
    bool removed = rce->getCFG()->removeUnreachBB(ctx, &rmunrchctx);
    ASSERT0_DUMMYUSE(removed);

    //CFG related info is incorrect if unreachable code removed.
    //e.g:rce_updatedom2.c
    rcectx.getOptCtx()->setInvalidIfCFGChanged();

    //DomInfo is necessary for subsequently SSA update.
    VexTab modset;
    UINT iter_times = 0;
    Vertex const* root = findMinimalRoot(rce);

    //Recompute DOM need RPO.
    rce->getRegion()->getPassMgr()->checkValidAndRecompute(
        rcectx.getOptCtx(), PASS_RPO, PASS_UNDEF);

    //Recompute the DOM for subgraph.
    rce->getCFG()->recomputeDomInfoForSubGraph(root, &modset, iter_times);
    rcectx.getOptCtx()->setValidPass(PASS_DOM);

    //Update the vertex's DomInfo as a result.
    modset.add(rmunrchctx.getVexTab());
    recordDomVexForEachElem(rce->getCFG(), modset);

    //Fixup SSA info according DOM.
    IRCfgOptCtx cfgoptctx(rcectx.getOptCtx(), &rce->getActMgr());
    rce->getCFG()->reviseMDSSA(modset, root, cfgoptctx);

    //Update status of CFG of RCECtx.
    rcectx.cfg_changed = true;

    //Do some verification at last.
    ASSERT0(rce->getCFG()->verifyDomAndPdom(*rcectx.getOptCtx()));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(rce->getRegion(), *rcectx.getOptCtx()));
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


static void dumpRemovedIR(IR const* ir, RCECtx const& ctx)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region * rg = ctx.getRegion();
    if (!rg->isLogMgrInit()) { return; }
    ActMgr * am = ctx.getActMgr();
    if (am == nullptr) { return; }
    xcom::StrBuf buf(32);
    xcom::StrBuf buf2(32);
    buf.strcat("\nREMOVE IR:%s",
        xoc::dumpIRToBuf(ir, rg, buf2, DumpFlag::combineIRID(IR_DUMP_KID)));
    am->dump("%s", buf.getBuf());
}


static void dumpRemovedEdge(IRBB const* from, IRBB const* to, RCECtx const& ctx)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region * rg = ctx.getRegion();
    if (!rg->isLogMgrInit()) { return; }
    ActMgr * am = ctx.getActMgr();
    if (am == nullptr) { return; }
    am->dump("\nREMOVE EDGE: BB%d->BB%d", from->id(), to->id());
}


static void dumpChangedIR(IR const* oldir, IR const* newir, RCECtx const& ctx)
{
    if (!g_dump_opt.isDumpRCE()) { return; }
    Region const* rg = ctx.getRegion();
    ActMgr * am = ctx.getActMgr();
    ASSERT0(rg);
    if (am == nullptr || !rg->isLogMgrInit() || !g_dump_opt.isDumpBCP()) {
        return;
    }
    class Dump : public xoc::DumpToBuf {
    public:
        xcom::StrBuf buf;
        IR const* oldir;
        IR const* newir;
    public:
        Dump(Region const* rg) : DumpToBuf(rg, buf, 2), buf(32) {}
        virtual void dumpUserInfo() const override
        {
            Region const* rg = getRegion();

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
    };
    Dump d(rg);
    d.oldir = oldir;
    d.newir = newir;
    am->dump("%s", d.dump());
}


static IR * replaceBranch(
    IR * ir, MOD BBIRList * ir_list, MOD IRListIter & ct, RCE * rce,
    MOD RCECtx & rcectx)
{
    Region * rg = rcectx.getRegion();
    IRCFG * cfg = rg->getCFG();
    IRMgr * irmgr = rg->getIRMgr();
    IRBB * from = ir->getBB();
    IRBB * to = cfg->getFallThroughBB(from);
    ASSERT0(from != nullptr && to != nullptr);
    IR * newbr = irmgr->buildGoto(BR_lab(ir));
    xoc::removeStmt(ir, rg, *rcectx.getOptCtx());
    copyDbx(newbr, ir, rg);

    //Revise the PHI operand to fallthrough successor.
    //Revise cfg. remove fallthrough edge.
    dumpRemovedEdge(from, to, rcectx);
    dumpChangedIR(ir, newbr, rcectx);

    //Remove optimization related info.
    rcectx.tryInvalidInfoBeforeFreeIR(ir);

    //Free old ir here, do NOT leave it to the caller.
    //Add newir.
    //Now, stmt is safe to free.
    ir_list->insert_before(newbr, ct);
    ir_list->remove(ct);
    rg->freeIRTree(ir);

    //Do NOT free old ir here, leave it to the caller.
    //Reivse CFG.
    IRCfgOptCtx coctx(rcectx.getOptCtx());
    CFGOPTCTX_need_update_dominfo(&coctx) = false;
    cfg->removeEdge(from, to, coctx);
    postProcessAfterRemoveEdge(from->getVex(), to->getVex(), rce, rcectx);
    return newbr;
}


static IR * removeBranch(
    IR * ir, MOD BBIRList * ir_list, MOD IRListIter & ct, RCE * rce,
    MOD RCECtx & rcectx)
{
    Region * rg = rcectx.getRegion();
    IRCFG * cfg = rg->getCFG();
    IRBB * from = ir->getBB();
    IRBB * to = cfg->getTargetBB(from);
    ASSERT0(from != nullptr && to != nullptr);
    xoc::removeStmt(ir, rcectx.getRegion(), *rcectx.getOptCtx());

    //Revise the PHI operand to target successor.
    dumpRemovedEdge(from, to, rcectx);
    dumpChangedIR(ir, nullptr, rcectx);

    //Remove optimization related info.
    rcectx.tryInvalidInfoBeforeFreeIR(ir);

    //Free old ir here, do NOT leave it to the caller.
    //Now, stmt is safe to free.
    ir_list->remove(ct);
    rg->freeIRTree(ir);

    //Reivse CFG.
    IRCfgOptCtx coctx(rcectx.getOptCtx());
    cfg->removeEdge(from, to, coctx);
    postProcessAfterRemoveEdge(from->getVex(), to->getVex(), rce, rcectx);
    return nullptr;
}


//
//START RCECtx
//
RCECtx::RCECtx(OptCtx & t, ActMgr * am) : PassCtx(&t, am), cfg_changed(false)
{
}
//END RCECtx


//
//START RCE
//
//If 'ir' is always true, set 'must_true', or if it is
//always false, set 'must_false'.
//Return true if this function is able to determine the result of 'ir',
//otherwise return false that it does know nothing about ir.
bool RCE::calcCondMustVal(
    IR const* ir, OUT bool & must_true, OUT bool & must_false,
    RCECtx const& ctx) const
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
        if (is_use_gvn()) {
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
    if (g_do_opt_float && ::fabs(CONST_fp_val(ir) - (HOST_FP)0.0) > EPSILON) {
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
    if (g_do_opt_float && ::fabs(CONST_fp_val(ir) - (HOST_FP)0.0) <= EPSILON) {
        return true;
    }
    return false;
}


//If 'ir' is always true, set 'must_true', or if it is
//always false, set 'must_false'.
//Return the changed ir.
IR * RCE::calcCondMustVal(
    MOD IR * ir, OUT bool & must_true, OUT bool & must_false,
    OUT bool & changed, MOD RCECtx & rcectx)
{
    must_true = false;
    must_false = false;
    ASSERT0(ir->is_judge());
    switch (ir->getCode()) {
    SWITCH_CASE_COMPARE:
    SWITCH_CASE_LOGIC: {
        RefineCtx rc(rcectx.getOptCtx());
        ir = m_refine->foldConst(ir, changed, rc);
        if (changed && ir->is_const()) {
            if (regardAsMustTrue(ir)) {
                must_true = true;
            } else if (regardAsMustFalse(ir)) {
                must_false = true;
            }
            return ir;
        }
        if (!is_use_gvn()) {
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
            xoc::removeUseForTree(ir, m_rg, *rcectx.getOptCtx());
            rcectx.tryInvalidInfoBeforeFreeIR(ir);
            m_rg->freeIRTree(ir);
            return m_irmgr->buildImmInt(1, type);
        }
        ASSERT0(must_false);
        Type const* type = ir->getType();
        xoc::removeUseForTree(ir, m_rg, *rcectx.getOptCtx());
        rcectx.tryInvalidInfoBeforeFreeIR(ir);
        m_rg->freeIRTree(ir);
        return m_irmgr->buildImmInt(0, type);
    }
    default: UNREACHABLE();
    }
    return ir;
}


IR * RCE::processFalsebrWithMustVal(
    IR * ir, bool must_true, bool must_false, MOD BBIRList * ir_list,
    MOD IRListIter & ct, MOD RCECtx & ctx)
{
    if (must_true) {
        //FALSEBR(0x1), never jump.
        //Remove FALSEBR, Revise CFG, Remove branch edge.
        return removeBranch(ir, ir_list, ct, this, ctx);
    }
    ASSERT0(must_false);
    //FALSEBR(0x0), always jump.
    return replaceBranch(ir, ir_list, ct, this, ctx);
}


static IR * inferAndReformDetCase1(
    IR * ir, OUT bool & changed, MOD BBIRList * ir_list, MOD IRListIter & ct,
    MOD RCECtx & ctx, MOD RCE * rce)
{
    IR * det = BR_det(ir);
    //Reform '((a == a) | (b & c)) == true' into true.
    //Reform '((a == a) || (b & c)) == true' into true.
    if (!det->is_bor() && !det->is_lor()) { return ir; }
    IR * op0 = BIN_opnd0(det);
    IR * op1 = BIN_opnd1(det);
    if (!op0->is_eq() && op1->is_eq()) {
        xcom::swap(op0, op1);
    }
    if (!op0->is_eq()) { return ir; }
    IR * op0_of_eq = BIN_opnd0(op0);
    IR * op1_of_eq = BIN_opnd1(op0);
    if (!op0_of_eq->isIREqual(op1_of_eq, ctx.getRegion()->getIRMgr(), true)) {
        return ir;
    }
    changed = true;
    if (ir->is_truebr()) {
        return rce->processTruebrWithMustVal(
            ir, true, false, ir_list, ct, ctx);
    }
    ASSERT0(ir->is_falsebr());
    return rce->processFalsebrWithMustVal(ir, true, false, ir_list, ct, ctx);
}


static IR * inferAndReformDetCase2ImplFP(
    bool comp_greater, IR * det, OUT bool & changed, MOD RCECtx & ctx,
    MOD RCE * rce)
{
    IR * op0 = BIN_opnd0(det);
    IR * op1 = BIN_opnd1(det);
    IR * op1_of_op0 = BIN_opnd1(op0); //1
    IR * op1_of_op1 = BIN_opnd1(op1); //5
    ASSERT0(op1_of_op0->is_fp() && op1_of_op1->is_fp());
    if (!g_do_opt_float) { return det; }
    if (CONST_fp_val(op1_of_op0) > CONST_fp_val(op1_of_op1) ||
        xcom::Float::isApproEq(
            CONST_fp_val(op1_of_op0), CONST_fp_val(op1_of_op1))) {
        if (comp_greater) {
            //Retain greater operand.
            BIN_opnd0(det) = nullptr;
            ctx.tryInvalidInfoBeforeFreeIR(det);
            ctx.getRegion()->freeIRTree(det);
            changed = true;
            return op0;
        }
        //Retain lesser operand.
        BIN_opnd1(det) = nullptr;
        ctx.tryInvalidInfoBeforeFreeIR(det);
        ctx.getRegion()->freeIRTree(det);
        changed = true;
        return op1;
    }
    if (CONST_fp_val(op1_of_op0) < CONST_fp_val(op1_of_op1)) {
        if (comp_greater) {
            //Retain greater operand.
            BIN_opnd1(det) = nullptr;
            changed = true;
            ctx.tryInvalidInfoBeforeFreeIR(det);
            ctx.getRegion()->freeIRTree(det);
            return op1;
        }
        //Retain lesser operand.
        BIN_opnd0(det) = nullptr;
        ctx.tryInvalidInfoBeforeFreeIR(det);
        ctx.getRegion()->freeIRTree(det);
        changed = true;
        return op0;
    }
    return det;
}


static IR * inferAndReformDetCase2ImplINT(
    bool comp_greater, IR * det, OUT bool & changed, MOD RCECtx & ctx,
    MOD RCE * rce)
{
    IR * op0 = BIN_opnd0(det);
    IR * op1 = BIN_opnd1(det);
    IR * op1_of_op0 = BIN_opnd1(op0); //1
    IR * op1_of_op1 = BIN_opnd1(op1); //5
    ASSERT0(op1_of_op0->is_int() && op1_of_op1->is_int());
    if (CONST_int_val(op1_of_op0) >= CONST_int_val(op1_of_op1)) {
        if (comp_greater) {
            //Retain greater operand.
            BIN_opnd0(det) = nullptr;
            ctx.tryInvalidInfoBeforeFreeIR(det);
            ctx.getRegion()->freeIRTree(det);
            changed = true;
            return op0;
        }
        //Retain lesser operand.
        BIN_opnd1(det) = nullptr;
        ctx.tryInvalidInfoBeforeFreeIR(det);
        ctx.getRegion()->freeIRTree(det);
        changed = true;
        return op1;
    }
    if (CONST_int_val(op1_of_op0) <= CONST_int_val(op1_of_op1)) {
        if (comp_greater) {
            //Retain greater operand.
            BIN_opnd1(det) = nullptr;
            ctx.tryInvalidInfoBeforeFreeIR(det);
            ctx.getRegion()->freeIRTree(det);
            changed = true;
            return op1;
        }
        //Retain lesser operand.
        BIN_opnd0(det) = nullptr;
        ctx.tryInvalidInfoBeforeFreeIR(det);
        ctx.getRegion()->freeIRTree(det);
        changed = true;
        return op0;
    }
    return det;
}


static IR * inferAndReformDetCase2Impl(
    IR * det, OUT bool & changed, MOD RCECtx & ctx, MOD RCE * rce)
{
    bool changed0 = false, changed1 = false;
    if (det->isBinaryOp()) {
        BIN_opnd0(det) = inferAndReformDetCase2Impl(
            BIN_opnd0(det), changed0, ctx, rce);
        BIN_opnd1(det) = inferAndReformDetCase2Impl(
            BIN_opnd1(det), changed1, ctx, rce);
    } else if (det->isUnaryOp()) {
        UNA_opnd(det) = inferAndReformDetCase2Impl(
            UNA_opnd(det), changed0, ctx, rce);
    }
    if (changed0 || changed1) {
        det->setParentPointer(false);
    }
    changed |= changed0;
    changed |= changed1;
    //Reform '(a > 1) & (a > 5)' into (a > 5)
    //Reform '(a > 1) && (a > 5)' into (a > 5)
    if (!det->is_band() && !det->is_land()) { return det; }
    IR * op0 = BIN_opnd0(det);
    IR * op1 = BIN_opnd1(det);
    bool comp_greater = true;
    if ((op0->is_gt() && op1->is_gt()) || (op0->is_ge() && op1->is_ge())) {
        comp_greater = true;
    } else if ((op0->is_lt() && op1->is_lt()) ||
               (op0->is_le() && op1->is_le())) {
        comp_greater = false;
    } else {
        return det;
    }
    IR * op0_of_op0 = BIN_opnd0(op0); //a
    IR * op1_of_op0 = BIN_opnd1(op0); //1
    IR * op0_of_op1 = BIN_opnd0(op1); //a
    IR * op1_of_op1 = BIN_opnd1(op1); //5
    if (!op0_of_op0->isIREqual(op0_of_op1, ctx.getRegion()->getIRMgr(), true)) {
        return det;
    }
    if (!op1_of_op0->is_const() || !op1_of_op1->is_const()) { return det; }
    if (op1_of_op0->is_fp() && op1_of_op1->is_fp()) {
        return inferAndReformDetCase2ImplFP(
            comp_greater, det, changed, ctx, rce);
    }
    if (op1_of_op0->is_int() && op1_of_op1->is_int()) {
        return inferAndReformDetCase2ImplINT(
            comp_greater, det, changed, ctx, rce);
    }
    return det;
}


static IR * inferAndReformDetCase2(
    IR * ir, OUT bool & changed, MOD RCECtx & ctx, MOD RCE * rce)
{
    ASSERT0(ir->isBranch());
    BR_det(ir) = inferAndReformDetCase2Impl(BR_det(ir), changed, ctx, rce);
    if (changed) {
        ir->setParentPointer(false);
    }
    return ir;
}


static IR * inferAndReformDetCase3(
    IR * ir, OUT bool & changed, MOD BBIRList * ir_list, MOD IRListIter & ct,
    MOD RCECtx & ctx, MOD RCE * rce)
{
    IR * det = BR_det(ir);
    if (!det->is_eq()) { return ir; }
    IR * op0_of_eq = BIN_opnd0(det);
    IR * op1_of_eq = BIN_opnd1(det);
    if (!op0_of_eq->isIREqual(op1_of_eq, ctx.getRegion()->getIRMgr(), true)) {
        return ir;
    }
    if (ir->is_truebr()) {
        return rce->processTruebrWithMustVal(ir, true, false, ir_list, ct, ctx);
    }
    ASSERT0(ir->is_falsebr());
    return rce->processFalsebrWithMustVal(ir, true, false, ir_list, ct, ctx);
}


IR * RCE::inferAndReformDet(
    IR * ir, MOD BBIRList * ir_list, MOD IRListIter & ct, MOD RCECtx & ctx)
{
    bool changed = false;
    ir = inferAndReformDetCase1(ir, changed, ir_list, ct, ctx, this);
    if (changed) { return ir; }
    ASSERT0(ir);
    ir = inferAndReformDetCase2(ir, changed, ctx, this);
    if (changed) { return ir; }
    ir = inferAndReformDetCase3(ir, changed, ir_list, ct, ctx, this);
    return ir;
}


IR * RCE::reformToJudgeDet(
    IR * ir, IR * new_det, bool changed, MOD RCECtx & ctx)
{
    IR * old_det = BR_det(ir);
    BR_det(ir) = nullptr;
    if (changed) {
        if (!new_det->is_judge()) {
            new_det = m_irmgr->buildJudge(new_det);
        }
        BR_det(ir) = new_det;
        ir->setParent(new_det);
        dumpChangedIR(old_det, new_det, ctx);
        return ir;
    }

    //Resume original det.
    BR_det(ir) = new_det;
    return ir;
}


IR * RCE::processTruebrWithMustVal(
    IR * ir, bool must_true, bool must_false, MOD BBIRList * ir_list,
    MOD IRListIter & ct, MOD RCECtx & ctx)
{
    if (must_true) {
        //TRUEBR(0x1), always jump.
        return replaceBranch(ir, ir_list, ct, this, ctx);
    }
    ASSERT0(must_false);
    //TRUEBR(0x0), never jump.
    //Remove TRUEBR, Revise CFG. Remove branch edge.
    return removeBranch(ir, ir_list, ct, this, ctx);
}


IR * RCE::processBranch(
    IR * ir, MOD BBIRList * ir_list, MOD IRListIter & ct, MOD RCECtx & ctx)
{
    ASSERT0(ir->isConditionalBr());
    bool must_true, must_false, changed = false;
    IR * new_det = calcCondMustVal(
        BR_det(ir), must_true, must_false, changed, ctx);
    if (new_det != BR_det(ir)) {
        if (!new_det->is_judge()) {
            new_det = m_irmgr->buildJudge(new_det);
            changed = true;
        }
        BR_det(ir) = new_det;
        ir->setParentPointer(false);
    }
    if (ir->is_truebr()) {
        if (must_true || must_false) {
            return processTruebrWithMustVal(
                ir, must_true, must_false, ir_list, ct, ctx);
        }
    } else {
        ASSERT0(ir->is_falsebr());
        if (must_true || must_false) {
            return processFalsebrWithMustVal(
                ir, must_true, must_false, ir_list, ct, ctx);
        }
    }
    ir = reformToJudgeDet(ir, new_det, changed, ctx);
    return inferAndReformDet(ir, ir_list, ct, ctx);
}


//Perform dead store elmination: x = x;
IR * RCE::processStoreStmt(IR * ir, RCECtx const& ctx)
{
    ASSERT0(ir->isStoreStmt());
    if (ir->hasSideEffect(true)) { return ir; }
    ASSERT0(ir->hasRHS());
    IR const* rhs = ir->getRHS();
    if (rhs->getExactRef() == ir->getExactRef()) {
        //Do dead store elmination: x=x, *x=*x, x[1]=x[1], etc;
        dumpRemovedIR(ir, ctx);
        xoc::removeUseForTree(ir, m_rg, *ctx.getOptCtx());
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
            newir = processBranch(ir, ir_list, ct, ctx);
            break;

        //This case has been dealt in IR-Refinement.
        //SWITCH_CASE_DIRECT_MEM_STMT:
        //    newir = processStoreStmt(ir);
        //    break;
        default:;
        }
        if (newir == ir) { continue; }
        ASSERTN(ir->is_undef(), ("old ir should have been removed."));
        ASSERT0(newir == nullptr || ir_list->find(newir));
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


bool RCE::dump() const
{
    if (!g_dump_opt.isDumpAfterPass() || !g_dump_opt.isDumpRCE()) {
        return true;
    }
    Region * rg = getRegion();
    if (!rg->isLogMgrInit()) { return true; }
    note(rg, "\n\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    rg->getLogMgr()->incIndent(2);
    m_am.dump();
    rg->getLogMgr()->decIndent(2);
    return true;
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
        &oc, PASS_RPO, PASS_DOM, PASS_GVN, PASS_UNDEF);
    ASSERT0(m_gvn->is_valid());
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    RCECtx ctx(oc, &getActMgr());
    bool change = performSimplyRCE(ctx);
    if (ctx.cfg_changed) {
        ASSERT0(change);
        //CASE:rce_cfgopt.c
        //Remove unreachable code firstly before recomputing RPO and DOM.
        IRCfgOptCtx ctx(&oc);
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
        //Thus you should apply a cost-model to drive the update of Dom and
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
        dump();
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
    END_TIMER(t, getPassName());
    return change;
}
//END RCE

} //namespace xoc
