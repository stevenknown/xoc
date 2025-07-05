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

static void dumpAct(IR const* oldexp, IR const* newexp, BCPCtx const& ctx)
{
    Region const* rg = ctx.getRegion();
    ActMgr * am = ctx.getActMgr();
    ASSERT0(rg);
    if (am == nullptr || !rg->isLogMgrInit() || !g_dump_opt.isDumpBCP()) {
        return;
    }
    class Dump : public xoc::DumpToBuf {
    public:
        xcom::StrBuf buf;
        IR const* oldexp;
        IR const* newexp;
    public:
        Dump(Region const* rg) : DumpToBuf(rg, buf, 2), buf(32) {}
        virtual void dumpUserInfo() const override
        {
            Region const* rg = getRegion();
            rg->getLogMgr()->incIndent(2);
            xoc::dumpIRCombine(oldexp, rg);
            rg->getLogMgr()->decIndent(2);
            note(rg, "\nWILL BE REPLACED BY");
            rg->getLogMgr()->incIndent(2);
            xoc::dumpIRCombine(newexp, rg);
            rg->getLogMgr()->decIndent(2);
        }
    };
    Dump d(rg);
    d.oldexp = oldexp;
    d.newexp = newexp;
    am->dump("%s", d.dump());
}


//
//START BCPCtx
//
BCPCtx::BCPCtx(OptCtx * oc, ActMgr * am) : PassCtx(oc, am)
{
    m_cdg = (CDG*)m_oc->getRegion()->getPassMgr()->queryPass(PASS_CDG);
    m_cfg = m_oc->getRegion()->getCFG();
    m_irmgr = m_rg->getIRMgr();
}


void BCPCtx::tryInvalidPassInfoBeforeFreeIR(IR const*) const
{
    //CASE:BCP will modify the VN of branch-condition, recompute VN to active
    //more aggressive optimizations.
    getOptCtx()->setInvalidPass(PASS_GVN);
}
//END BCPCtx


//
//START BrCondProp
//
BrCondProp::BrCondProp(Region * rg) : Pass(rg), m_am(rg)
{
}


BrCondProp::~BrCondProp()
{
}


bool BrCondProp::dump(BCPCtx const& ctx) const
{
    if (!m_rg->isLogMgrInit()) { return true; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==\n",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    if (ctx.getActMgr() != nullptr) {
        ctx.getActMgr()->dump();
    }
    Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    return true;
}


bool BrCondProp::initSSAMgr(OptCtx const& oc)
{
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //BCP use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //BCP use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    return true;
}


typedef xcom::TMapIter<VN const*, IRList*> VN2IRListIter;
class VN2IRList : public xcom::TMap<VN const*, IRList*> {
    COPY_CONSTRUCTOR(VN2IRList);
    xcom::List<IRList*> m_irlst;
public:
    VN2IRList() {}
    ~VN2IRList()
    {
        for (IRList * irl = m_irlst.get_head();
             irl != nullptr; irl = m_irlst.get_next()) {
            delete irl;
        }
    }
    IRList * allocIRList()
    {
        IRList * irl = new IRList();
        m_irlst.append_tail(irl);
        return irl;
    }
    void add(VN const* vn, MOD IR * br)
    {
        ASSERT0(vn && br && br->isBranch());
        IRList * irl = get(vn);
        if (irl == nullptr) {
            irl = allocIRList();
            set(vn, irl);
        }
        irl->append_tail(br);
    }
    void dump(Region const* rg) const
    {
        if (get_elem_count() == 0) { return; }
        ASSERT0(rg);
        VN2IRListIter it;
        IRList * irlst = nullptr;
        xcom::StrBuf buf(32);
        for (VN const* vn = get_first(it, &irlst);
             vn != nullptr; vn = get_next(it, &irlst)) {
            vn->dump(rg, buf);
            note(rg, "\n%s:", buf.getBuf());
            if (irlst == nullptr) {
                prt(rg, "--");
                continue;
            }
            bool first = true;
            for (IR const* ir = irlst->get_head();
                 ir != nullptr; ir = irlst->get_next()) {
                if (!first) { note(rg, ","); }
                first = false;
                xoc::dumpIRName(ir, rg);
            }
        }
    }
};


static bool isReachable(IRBB const* start, IRBB const* end)
{
    bool try_failed;
    bool reach = Graph::isReachIn(
        start->getVex(), end->getVex(), GRAPH_REACHIN_MAX_TRY_LIMIT,
        try_failed);
    ASSERTN(!try_failed, ("abnormal result"));
    return reach;
}


static bool isReachable(
    IRBB const* edgefrom, IRBB const* edgeto, IRBB const* target)
{
    xcom::VexTab terminate_vex_tab;
    terminate_vex_tab.append(edgefrom->getVex()->id());
    bool try_failed;
    bool reach = Graph::isReachIn(
        target->getVex(), edgeto->getVex(), GRAPH_REACHIN_MAX_TRY_LIMIT,
        try_failed, &terminate_vex_tab);
    ASSERTN(!try_failed, ("abnormal result"));
    return reach;
}


static void replaceDet(
    MOD IR * ir, bool must_true, bool must_false, BCPCtx const& ctx)
{
    ASSERT0(ir && ir->isBranch());
    ASSERT0(must_true ^ must_false);
    IR * old_det = BR_det(ir);
    ASSERT0(old_det->isBinaryOp());

    //Use the data type of opnd0 as the type of new DET.
    Type const* type = BIN_opnd0(old_det)->getType();
    xoc::removeUseForTree(old_det, ctx.getRegion(), *ctx.getOptCtx());
    ctx.tryInvalidInfoBeforeFreeIR(ir);
    IR * new_det = nullptr;
    IRMgr * irmgr = ctx.getIRMgr();
    ASSERT0(irmgr);
    if (must_true) {
        new_det = irmgr->buildImmInt(1, type);
    } else {
        ASSERT0(must_false);
        new_det = irmgr->buildImmInt(0, type);
    }
    new_det = irmgr->buildJudge(new_det);
    dumpAct(old_det, new_det, ctx);
    ir->replaceKid(old_det, new_det, false);
    ctx.tryInvalidInfoBeforeFreeIR(old_det);
    ctx.tryInvalidPassInfoBeforeFreeIR(old_det);
    ctx.getRegion()->freeIRTree(old_det);
}


//Return true if BCP occurred and branch condition changed.
static bool doBCPImpl(
    IR const* ir1, MOD IR * ir2, IRBB const* ir1brtgt,
    IRBB const* ir1fallthrough, BCPCtx const& ctx)
{
    bool must_true = false;
    bool must_false = false;
    IRBB const* ir1bb = ir1->getBB();
    IRBB const* ir2bb = ir2->getBB();
    if (!isReachable(ir1bb, ir2bb)) {
        //Can NOT propagate ir1's condition.
        return false;
    }

    //CASE:ir1:falsebr(ne $x, 1)
    //     ir2:falsebr(eq $y, 1)
    //Both ne and eq have the same VN.
    if (ir1->is_falsebr()) {
        if (!isReachable(ir1bb, ir1fallthrough, ir2bb)) {
            must_false = true;
            replaceDet(ir2, must_true, must_false, ctx);
            return true;
        }
        return false;
    }
    if (ir1->is_truebr()) {
        if (!isReachable(ir1bb, ir1brtgt, ir2bb)) {
            must_true = true;
            replaceDet(ir2, must_true, must_false, ctx);
            return true;
        }
        return false;
    }
    UNREACHABLE();
    return false;
}


static bool doBCP(VN2IRList const& vn2irlst, BCPCtx const& ctx)
{
    Region const* rg = ctx.getRegion();
    ASSERT0(rg);
    IRCFG const* cfg = ctx.getCFG();
    IRMgr const* irmgr = ctx.getIRMgr();
    ASSERT0(irmgr);
    IRList * irlst = nullptr;
    VN2IRListIter it;
    bool changed = false;
    for (VN const* vn = vn2irlst.get_first(it, &irlst);
         vn != nullptr; vn = vn2irlst.get_next(it, &irlst)) {
        if (irlst == nullptr || irlst->get_elem_count() <= 1) { continue; }
        IRListIter irit1;
        for (IR const* ir1 = irlst->get_head(&irit1);
             ir1 != nullptr; ir1 = irlst->get_next(&irit1)) {
            ASSERT0(ir1->is_stmt() && ir1->isBranch());
            IRBB const* ir1bb = ir1->getBB();
            ASSERT0(ir1bb);
            IRBB const* ir1brtgt = cfg->getTargetBB(ir1);

            //TODO:use cfg->getFallThroughBB(ir1bb_ct);
            IRBB const* ir1fallthrough = cfg->getFallThroughBB(ir1bb);
            IRListIter irit2 = irit1;
            for (IR * ir2 = irlst->get_next(&irit2);
                 ir2 != nullptr; ir2 = irlst->get_next(&irit2)) {
                ASSERT0(ir2->is_stmt() && ir2->isBranch());
                if (!ir1->isIREqual(ir2, irmgr, false)) { continue; }
                changed |= doBCPImpl(ir1, ir2, ir1brtgt, ir1fallthrough, ctx);
            }
        }
    }
    return changed;
}


bool BrCondProp::doProp(MOD BCPCtx & ctx)
{
    BBList const* bbl = ctx.getRegion()->getBBList();
    BBListIter it;
    InferCtx ictx;
    VN2IRList vn2irlst;
    GVN * gvn = ctx.getGVN();
    ASSERT0(gvn && gvn->is_valid());
    InferEVN * evn = gvn->getAndGenInferEVN();
    ASSERT0(evn);
    for (IRBB * bb = bbl->get_head(&it);
         bb != nullptr; bb = bbl->get_next(&it)) {
        IR * lastir = BB_last_ir(bb);
        if (lastir == nullptr || !lastir->isConditionalBr()) { continue; }
        IR const* det = BR_det(lastir);
        ASSERT0(det);
        ictx.clean();
        VN const* vn = evn->inferExp(det, ictx);
        if (vn != nullptr) {
            vn2irlst.add(vn, lastir);
        }
    }
    return doBCP(vn2irlst, ctx);
}


bool BrCondProp::initDepPass(OptCtx & oc)
{
    if (!initSSAMgr(oc)) { return false; }
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_LOOP_INFO, PASS_CDG, PASS_DOM, PASS_GVN, PASS_UNDEF);
    return true;
}


bool BrCondProp::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    START_TIMER(t, getPassName());
    if (!initDepPass(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }
    if (g_dump_opt.isDumpBeforePass()) {
        dumpBeforePass();
    }
    ASSERTN(oc.isPassValid(PASS_GVN),
            ("GVN provide more accurate result of value"
             " flow analysis and is necesary."));
    BCPCtx bcpctx(&oc, &getActMgr());
    bool change = doProp(bcpctx);
    if (!change) {
        END_TIMER(t, getPassName());
        return false;
    }
    dump(bcpctx);

    //DU chain and DU reference should be maintained.
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(verifyClassicDUChain(m_rg, oc));
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    IRCFG const* cfg = m_rg->getCFG();
    ASSERT0(cfg && cfg->is_valid());
    ASSERT0(cfg->verifyRPO(oc));
    ASSERT0(cfg->verifyLoopInfo(oc));
    ASSERT0(cfg->verifyDomAndPdom(oc));
    END_TIMER(t, getPassName());
    return true;
}
//END BrCondProp

} //namespace xoc
