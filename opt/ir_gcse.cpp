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

static bool isDom(IR const* exp_stmt, IR const* gen_stmt, GCSECtx const& ctx)
{
    IRBB const* gen_bb = gen_stmt->getBB();
    IRBB const* exp_bb = exp_stmt->getBB();
    if (ctx.getCFG()->hasEHEdge()) {
        if (gen_bb == exp_bb) {
            return gen_bb->is_dom(gen_stmt, exp_stmt, true);
        }
        ASSERT0(ctx.getTG());
        return ctx.getTG()->is_dom(gen_bb->id(), exp_bb->id());
    }
    if (gen_bb == exp_bb) {
        return gen_bb->is_dom(gen_stmt, exp_stmt, true);
    }
    return ctx.getCFG()->is_dom(gen_bb->id(), exp_bb->id());
}

static void dumpAct(
    IR const* oldexp, IR const* genexp, IR const* newexp, GCSECtx const& ctx)
{
    Region const* rg = ctx.getRegion();
    ASSERT0(rg);
    ActMgr * am = ctx.getActMgr();
    if (am == nullptr || !rg->isLogMgrInit() || !g_dump_opt.isDumpGCSE()) {
        return;
    }
    am->dump("%s is CSE of %s and will be replaced by %s",
             DumpIRName().dump(oldexp), DumpIRName().dump(genexp),
             DumpIRName().dump(newexp));
}


class VFSideEff {
public:
    bool has_sideeffect;
public:
    VFSideEff() : has_sideeffect(false) {}
    bool visitIR(IR const* ir, OUT bool & is_terminate)
    {
        if (ir->isMayThrow(false) || ir->hasSideEffect(false) ||
            ir->isNoMove(false)) {
            has_sideeffect = true;
            return false;
        }
        return true;
    }
};

class IterSideEffect : public VisitIRTree<VFSideEff> {
public:
    IterSideEffect(IR const* ir, VFSideEff & vf)
        : VisitIRTree(vf) { visit(ir); }
};

class PropVNVisitFunc : public xcom::VisitTreeFuncBase {
    COPY_CONSTRUCTOR(PropVNVisitFunc);
    bool m_is_changed;
    IRCFG * m_cfg;
    GCSE * m_gcse;
    GCSECtx const& m_ctx;
public:
    PropVNVisitFunc(IRCFG * cfg, GCSE * gcse, GCSECtx const& ctx)
        : m_cfg(cfg), m_gcse(gcse), m_ctx(ctx)
    { m_is_changed = false; }

    bool isChanged() const { return m_is_changed; }

    void visitWhenAllKidHaveBeenVisited(
        Vertex const* v, Stack<Vertex const*> &)
    {}
    bool visitWhenFirstMeet(Vertex const* v, Stack<Vertex const*> &)
    {
        m_is_changed |= m_gcse->doPropVN(m_cfg->getBB(v->id()), m_ctx);
        return true;
    }
};


//
//START GCSECtx
//
GCSECtx::GCSECtx(OptCtx & oc, xcom::DomTree const& domtree, ActMgr * am,
                 GCSE * gcse)
    : PassCtx(&oc, am), m_gcse(gcse)
{
    Region * rg = oc.getRegion();
    ASSERT0(rg);
    m_cfg = rg->getCFG();
    m_tg = nullptr;
    if (!m_cfg->hasEHEdge()) { return; }

    //Initialize Temp CFG and pick out exception-handling-edge.
    m_tg = allocTG(rg);
    m_tg->clone(*m_cfg, false, false);
    m_tg->pickEH();
    IRBB * entry = m_cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    xcom::Vertex * root = domtree.getVertex(entry->id());
    m_tg->removeUnreachNode(entry->id());
    m_tg->computeDomAndIdom();
    m_tg->computePdomAndIpdom(root);
}


GCSECtx::~GCSECtx()
{
    if (m_tg != nullptr) {
        delete m_tg;
        m_tg = nullptr;
    }
}


TG * GCSECtx::allocTG(Region * rg)
{
    return new TG(rg);
}


void GCSECtx::tryInvalidInfoBeforeFreeIR(IR const* ir) const
{
    ASSERT0(m_gcse);
    ASSERT0(m_gcse->getInferEVN());
    m_gcse->getInferEVN()->cleanVNIRTree(ir);
    PassCtx::tryInvalidInfoBeforeFreeIR(ir);
}
//END GCSECtx

//
//START PropVNVisit
//
class PropVNVisit : public xcom::VisitTree<PropVNVisitFunc> {
    COPY_CONSTRUCTOR(PropVNVisit);
public:
    PropVNVisit(IRBB * root, DomTree const& domtree, PropVNVisitFunc & vf)
        : VisitTree(domtree, root->id(), vf) {}
};
//END PropVNVisit


class PropExpVisitFunc : public xcom::VisitTreeFuncBase {
    COPY_CONSTRUCTOR(PropExpVisitFunc);
    bool m_is_changed;
    IRCFG * m_cfg;
    GCSE * m_gcse;
    GCSECtx const& m_ctx;
    List<IR*> m_livexp;
public:
    PropExpVisitFunc(IRCFG * cfg, GCSE * gcse, GCSECtx const& ctx)
        : m_cfg(cfg), m_gcse(gcse), m_ctx(ctx)
    { m_is_changed = false; }

    bool isChanged() const { return m_is_changed; }

    void visitWhenAllKidHaveBeenVisited(Vertex const*, Stack<Vertex const*> &)
    {}
    bool visitWhenFirstMeet(Vertex const* v, Stack<Vertex const*> &)
    {
        m_is_changed |= m_gcse->doPropExp(
            m_cfg->getBB(v->id()), m_livexp, m_ctx);
        return true;
    }
};


//
//START PropExpVisit
//
class PropExpVisit : public xcom::VisitTree<PropExpVisitFunc> {
    COPY_CONSTRUCTOR(PropExpVisit);
public:
    PropExpVisit(IRBB * root, DomTree const& domtree, PropExpVisitFunc & vf)
        : VisitTree(domtree, root->id(), vf) {}
};
//END PropExpVisit


//
//START GCSE
//
GCSE::GCSE(Region * rg, GVN * gvn) : Pass(rg), m_am(rg)
{
    ASSERT0(rg);
    ASSERT0(gvn);
    m_cfg = rg->getCFG();
    m_dumgr = rg->getDUMgr();
    m_aa = rg->getAA();
    ASSERT0(m_dumgr && m_aa && m_cfg);
    m_expr_tab = nullptr;
    m_tm = rg->getTypeMgr();
    m_is_in_ssa_form = false;
    m_prssamgr = nullptr;
    m_mdssamgr = nullptr;
    m_gvn = gvn;
    m_infer_evn = m_gvn->getAndGenInferEVN();
    ASSERT0(m_infer_evn);
}


GCSE::~GCSE()
{
}


bool GCSE::doPropVNInDomTreeOrder(
    xcom::DomTree const& domtree, GCSECtx const& ctx)
{
    IRBB * entry = m_cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    PropVNVisitFunc vf(m_cfg, this, ctx);
    PropVNVisit prop(entry, domtree, vf);
    prop.visit();
    return vf.isChanged();
}


void GCSE::copyVN(IR const* newir, IR const* oldir)
{
    //Assign the identical vn to newrhs.
    if (m_gvn == nullptr) { return; }
    VN const* vn = m_gvn->getVN(oldir);
    //Note vn may be NULL.
    m_gvn->setVN(newir, vn);
}


bool GCSE::elimCSE(IR * use, IR * use_stmt, IR const* gen, GCSECtx const& ctx)
{
    ASSERT0(use != gen);
    ASSERT0(use_stmt->isKids(use));
    ASSERT0(use->is_exp() && gen->is_exp() && use_stmt->is_stmt());

    //gen_pr hold the CSE value come from gen-stmt.
    //We eliminate the redundant computation via replace use by gen_pr.
    IR * gen_pr = m_exp2pr.get(gen);
    ASSERT0(gen_pr && gen_pr->is_pr());
    IR * new_pr = m_rg->dupIRTree(gen_pr);
    dumpAct(use, gen, new_pr, ctx);
    bool f = use_stmt->replaceKid(use, new_pr, true);
    ASSERT0_DUMMYUSE(f);

    //Assign MD to new_pr.
    m_rg->getMDMgr()->allocMDForPROp(new_pr);

    //Set identical VN to new_pr with CSE.
    copyVN(new_pr, gen);

    //Add DU chain from gen_pr's stmt to new_pr.
    IR * gen_stmt = gen->getStmt();
    ASSERT0(gen_stmt->isPREqual(gen_pr));
    xoc::buildDUChain(gen_stmt, new_pr, m_rg, *ctx.getOptCtx());
    xoc::removeUseForTree(use, m_rg, *ctx.getOptCtx());
    ctx.tryInvalidInfoBeforeFreeIR(use);
    m_rg->freeIRTree(use);
    return true;
}


bool GCSE::elimCseOfBranch(
    IR * use, IR * use_stmt, IN IR * gen, GCSECtx const& ctx)
{
    ASSERT0(use_stmt->isKids(use));
    ASSERT0(use->is_exp() && gen->is_exp() && use_stmt->is_stmt());

    //gen_pr hold the CSE value come from gen-stmt.
    //We eliminate the redundant computation via replace use by gen_pr.
    IR * gen_pr = m_exp2pr.get(gen);
    ASSERT0(gen_pr);
    IR * new_pr = m_rg->dupIRTree(gen_pr);
    dumpAct(use, gen, new_pr, ctx);

    //Det of branch stmt have to be judgement operation.
    ASSERT0(use == BR_det(use_stmt));
    IR * newdet = m_rg->getIRMgr()->buildJudge(new_pr);
    bool f = use_stmt->replaceKid(use, newdet, true);
    ASSERT0_DUMMYUSE(f);
    IR_may_throw(use_stmt) = false;

    //Assign MD to new_pr.
    m_rg->getMDMgr()->allocMDForPROp(new_pr);

    //Set identical VN to new_pr with CSE.
    copyVN(new_pr, gen);

    //Add DU chain.
    IR * gen_stmt = gen->getStmt();
    ASSERT0(gen_stmt->isPREqual(gen_pr));
    xoc::buildDUChain(gen_stmt, new_pr, m_rg, *ctx.getOptCtx());
    xoc::removeUseForTree(use, m_rg, *ctx.getOptCtx());
    ctx.tryInvalidInfoBeforeFreeIR(use);
    m_rg->freeIRTree(use);
    return true;
}


bool GCSE::elimCseOfAssignment(
    IR * use, IR * use_stmt, IR * gen, GCSECtx const& ctx)
{
    if (use_stmt->is_stpr() && use_stmt->getRHS() == use) {
        //use-stmt becomes a move.
        IR_may_throw(use_stmt) = false;
        copyVN(use_stmt, gen);
    }
    return elimCSE(use, use_stmt, gen, ctx);
}


//Generate delegate-PR of CSE at generation point.
IR * GCSE::genDelegatePR(IR const* gen, IR const* gen_stmt)
{
    //TBD:Do we have to use operand type as the delegate-PR's type if
    //gen_stmt is Branch Operation.
    //if (gen_stmt->is_truebr() || gen_stmt->is_falsebr()) {
    //    //Expect opnd1's type is same with opnd0.
    //    return m_rg->getIRMgr()->buildPR(BIN_opnd0(gen)->getType());
    //}
    return m_rg->getIRMgr()->buildPR(gen->getType());
}


void GCSE::processCseGen(
    MOD IR * gen, MOD IR * gen_stmt, bool & change, GCSECtx const& ctx)
{
    ASSERT0(gen->is_exp() && gen_stmt->is_stmt());
    //Move STORE_VAL to temp PR.
    //e.g: a = 10, expression of store_val is nullptr.
    IRBB * bb = gen_stmt->getBB();
    ASSERT0(bb);
    IR * dele_pr = m_exp2pr.get(gen);
    if (dele_pr != nullptr) { return; }
    dele_pr = genDelegatePR(gen, gen_stmt);

    //Set mapping between delegate-PR and 'gen' expression.
    m_exp2pr.set(gen, dele_pr);
    m_rg->getMDMgr()->allocMDForPROp(dele_pr);

    //Relpace GEN that is in original gen-stmt with DelegatePR.
    IR * newkid = dele_pr;
    if (gen_stmt->isConditionalBr() && gen == BR_det(gen_stmt)) {
        //Det of branch stmt have to be judgement expression.
        newkid = m_rg->getIRMgr()->buildJudge(dele_pr);
        copyDbx(newkid, dele_pr, m_rg);
    }
    bool v = gen_stmt->replaceKid(gen, newkid, false);
    ASSERT0_DUMMYUSE(v);

    //Generate STPR operation to store GEN to delegate-PR.
    //For now, GEN is dangling IR exp.
    ASSERT0(gen->getParent() == nullptr && gen->is_single());
    IR * new_stpr = m_rg->getIRMgr()->buildStorePR(
        dele_pr->getPrno(), dele_pr->getType(), gen);
    m_rg->getMDMgr()->allocMDForPROp(new_stpr);
    copyVN(new_stpr, gen);
    copyDbx(new_stpr, gen_stmt, m_rg);

    //The 'find()' is fast because it is implemented with hash.
    IRListIter holder = nullptr;
    bool f = BB_irlist(bb).find(gen_stmt, &holder);
    ASSERT0_DUMMYUSE(f);
    ASSERT0_DUMMYUSE(holder);
    BB_irlist(bb).insert_before(new_stpr, holder);

    //Keep original DU chain unchanged, build DU chain for new stmt.
    ASSERT0(dele_pr->is_pr());
    xoc::buildDUChain(new_stpr, dele_pr, m_rg, *ctx.getOptCtx());
    IR_may_throw(gen_stmt) = false;
    change = true;
}


bool GCSE::isCseCandidate(IR const* ir) const
{
    ASSERT0(ir && ir->is_exp());
    switch (ir->getCode()) {
    SWITCH_CASE_BIN:
    case IR_SELECT:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_ILD:

    //We expect to extract the CVT if it is the CSE.
    //CASE:compiler.gr/cvt_cse.gr
    case IR_CVT:
        ASSERT0(!ir->isDummyOp());
        return true;
    default: break;
    }
    return false;
}


bool GCSE::elim(IR * use, IR * use_stmt, IR * gen,
                IR * gen_stmt, GCSECtx const& ctx)
{
    //exp is CSE.
    //e.g: ...=a+b <--generate CSE
    //     ...
    //     ...=a+b <--use CSE
    bool change = false;
    processCseGen(gen, gen_stmt, change, ctx);
    switch (use_stmt->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_ARRAY_OP:
    SWITCH_CASE_WRITE_PR:
        change |= elimCseOfAssignment(use, use_stmt, gen, ctx);
        return change;
    SWITCH_CASE_CALL:
        change |= elimCSE(use, use_stmt, gen, ctx);
        return change;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        change |= elimCseOfBranch(use, use_stmt, gen, ctx);
        return change;
    case IR_RETURN:
        change |= elimCSE(use, use_stmt, gen, ctx);
        return change;
    default: ASSERTN(0, ("TODO:need to support"));
    }
    return change;
}


bool GCSE::findAndElim(IR * exp, IR * gen, GCSECtx const& ctx)
{
    ASSERT0(exp && gen);
    ASSERT0(exp != gen);
    IR * exp_stmt = exp->getStmt();
    IR * gen_stmt = gen->getStmt();
    ASSERT0(exp_stmt->getBB() && gen_stmt->getBB());
    if (!isDom(exp_stmt, gen_stmt, ctx)) { return false; }
    if ((gen->is_cvt() || exp->is_cvt()) && !canElimCVT(exp, gen)) {
        return false;
    }
    return elim(exp, exp_stmt, gen, gen_stmt, ctx);
}


bool GCSE::processCSE(MOD IR * exp, IN List<IR*> & livexp, GCSECtx const& ctx)
{
    IR * expstmt = exp->getStmt();
    ExprRep * irie = m_expr_tab->mapIR2ExprRep(exp);
    ASSERT0(irie && expstmt->getBB());
    IRListIter ct;
    bool change = false;
    for (IR * gen = livexp.get_head(&ct);
         gen != nullptr; gen = livexp.get_next(&ct)) {
        ExprRep * xie = m_expr_tab->mapIR2ExprRep(gen);
        ASSERT0(xie);
        if (irie != xie) { continue; }
        IR * gen_stmt = gen->getStmt();
        ASSERT0(gen_stmt->getBB());
        UINT iid = expstmt->getBB()->id();
        UINT xid = gen_stmt->getBB()->id();
        if (!m_cfg->gen_dom_set(iid)->is_contain(xid)) {
            continue;
        }
        return elim(exp, expstmt, gen, gen_stmt, ctx);
    }
    return change;
}


bool GCSE::handleCandidateByExprRep(IR * exp, GCSECtx const& ctx)
{
    ExprRep * e = m_expr_tab->mapIR2ExprRep(exp);
    if (e == nullptr) { return false; }
    IREListIter it;
    bool changed = false;
    IREListIter nextit;
    for (e->getOccList().get_head(&it); it != nullptr; it = nextit) {
        nextit = it;
        e->getOccList().get_next(&nextit);
        IR * occ = it->val();
        if (occ == exp) { continue; }
        if (occ->is_undef()) {
            //Occ has been removed during some optimizations.
            //For the sake of speepding-up compilation, we do not update
            //occ-list of ExprRep at every manipulations of IR expression.
            //Just skip the UNDEF IR here.
            continue;
        }
        IR const* occ_stmt = occ->getStmt();
        IR const* exp_stmt = exp->getStmt();
        ASSERT0(occ_stmt && occ_stmt->is_stmt());
        ASSERT0(exp_stmt && exp_stmt->is_stmt());
        if (!isDom(occ_stmt, exp_stmt, ctx)) { continue; }
        if (!xoc::hasSameUniqueMustDefForTree(exp, occ, m_rg)) { continue; }
        changed |= findAndElim(occ, exp, ctx);
        e->getOccList().remove(it);
    }
    return changed;
}


bool GCSE::handleCandidate(IR * exp, IRBB * bb, GCSECtx const& ctx)
{
    InferCtx ictx;
    ASSERT0(m_infer_evn);
    VN const* evn = m_infer_evn->inferExp(exp, ictx);
    if (evn != nullptr) {
        IR * gen = m_evn2exp.get(evn);
        if (gen != nullptr) {
            //Clean exp info out of VN2IR tab.
            VN const* vn = m_gvn->getVN(exp);
            if (vn != nullptr) {
                m_vn2exp.clean(vn);
            }

            //Found CSE and replaced it with pr.
            return findAndElim(exp, gen, ctx);
        }
        m_evn2exp.set(evn, exp);
    }
    VN const* vn = m_gvn->getVN(exp);
    if (vn != nullptr) {
        IR * gen = m_vn2exp.get(vn);
        if (gen != nullptr) {
            //Clean exp info out of VN2IR tab.
            m_evn2exp.clean(evn);

            //Found CSE and replaced it with pr.
            return findAndElim(exp, gen, ctx);
        }
        m_vn2exp.set(vn, exp);
    }
    return handleCandidateByExprRep(exp, ctx);
}


//Determine if det-exp of truebr/falsebr ought to be CSE.
bool GCSE::shouldBeCSE(IR const* det) const
{
    ASSERT0(det->is_judge());

    //If the det if simply enough, CSE is dispensable.
    if (!IR_parent(det)->is_truebr() && !IR_parent(det)->is_falsebr()) {
        return true;
    }
    if (!det->is_relation()) {
        //det is complex operation.
        return true;
    }
    IR const* op0 = BIN_opnd0(det);
    IR const* op1 = BIN_opnd1(det);
    if (!op0->is_pr() && !op0->is_const()) {
        return true;
    }

    if (!op1->is_pr() && !op1->is_const()) {
        return true;
    }

    return false;
}


bool GCSE::doPropExpInDomTreeOrder(
    xcom::DomTree const& domtree, GCSECtx const& ctx)
{
    IRBB * entry = m_cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    PropExpVisitFunc vf(m_cfg, this, ctx);
    PropExpVisit prop(entry, domtree, vf);
    prop.visit();
    return vf.isChanged();
}


bool GCSE::hasSideEffect(IR const* ir) const
{
    VFSideEff vf;
    IterSideEffect it(ir, vf);
    return vf.has_sideeffect;
}


bool GCSE::doPropVNDirectStmt(IR * ir, GCSECtx const& ctx)
{
    if (hasSideEffect(ir)) { return false; }
    IR * rhs = ir->getRHS();
    if (rhs == nullptr) {
        //Virtual OP may not have RHS.
        return false;
    }
    //Find CSE and replace it with properly PR.
    if (isCseCandidate(rhs)) {
        return handleCandidate(rhs, ir->getBB(), ctx);
    }
    return false;
}


bool GCSE::doPropVNIndirectStmt(IR * ir, GCSECtx const& ctx)
{
    if (hasSideEffect(ir)) { return false; }
    IR * rhs = ir->getRHS();
    if (rhs == nullptr) {
        //Virtual OP may not have RHS.
        return false;
    }
    bool change = false;

    //Find CSE and replace it with properly PR.
    IR * base = ir->getBase();
    if (isCseCandidate(base)) {
        change |= handleCandidate(base, ir->getBB(), ctx);
    }
    if (isCseCandidate(rhs)) {
        change |= handleCandidate(rhs, ir->getBB(), ctx);
    }
    return change;
}


bool GCSE::doPropVNCallStmt(IR * ir, GCSECtx const& ctx)
{
    if (hasSideEffect(ir)) { return false; }
    IR * p = CALL_arg_list(ir);
    IR * next = nullptr;
    m_newst_lst.clean();
    bool change = false;
    IRBB * bb = ir->getBB();
    while (p != nullptr) {
        next = p->get_next();
        if (isCseCandidate(p)) {
            change |= handleCandidate(p, bb, ctx);
        }
        p = next;
    }
    return change;
}


bool GCSE::doPropVNBrStmt(IR * ir, GCSECtx const& ctx)
{
    if (hasSideEffect(ir)) { return false; }

    //Find CSE and replace it with properly pr.
    ASSERT0(BR_det(ir));
    if (isCseCandidate(BR_det(ir)) && shouldBeCSE(BR_det(ir))) {
        return handleCandidate(BR_det(ir), ir->getBB(), ctx);
    }
    return false;
}


bool GCSE::doPropVNRetStmt(IR * ir, GCSECtx const& ctx)
{
    if (hasSideEffect(ir)) { return false; }
    if (RET_exp(ir) != nullptr && isCseCandidate(RET_exp(ir))) {
        return handleCandidate(RET_exp(ir), ir->getBB(), ctx);
    }
    return false;
}


bool GCSE::doPropVNStmt(IR * ir, GCSECtx const& ctx)
{
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR:
        return doPropVNDirectStmt(ir, ctx);
    SWITCH_CASE_INDIRECT_MEM_STMT:
        return doPropVNIndirectStmt(ir, ctx);
    SWITCH_CASE_CALL:
        return doPropVNCallStmt(ir, ctx);
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        return doPropVNBrStmt(ir, ctx);
    case IR_RETURN:
        return doPropVNRetStmt(ir, ctx);
    default: break;
    }
    return false;
}


//Do propagation according to value numbering.
bool GCSE::doPropVN(IRBB * bb, GCSECtx const& ctx)
{
    bool change = false;
    IRListIter it;
    BBIRList & lst = bb->getIRList();
    for (IR * ir = lst.get_head(&it); ir != nullptr; ir = lst.get_next(&it)) {
        change |= doPropVNStmt(ir, ctx);
    }
    return change;
}


bool GCSE::doPropReturn(IR * ir, MOD List<IR*> & livexp, GCSECtx const& ctx)
{
    ASSERT0(ir->is_return());
    IR * retexp = RET_exp(ir);
    if (retexp == nullptr || !isCseCandidate(retexp) || !shouldBeCSE(retexp)) {
        return false;
    }
    if (processCSE(retexp, livexp, ctx)) {
        //Found CSE and replaced CSE with PR successfully.
        return true;
    }
    //Generate new CSE.
    livexp.append_tail(retexp);
    return false;
}


bool GCSE::doPropBranch(IR * ir, MOD List<IR*> & livexp, GCSECtx const& ctx)
{
    ASSERT0(ir->isBranch());
    if (!isCseCandidate(BR_det(ir)) || !shouldBeCSE(BR_det(ir))) {
        return false;
    }
    if (processCSE(BR_det(ir), livexp, ctx)) {
        //Replaced CSE with PR successfully.
        return true;
    }
    //Generate new CSE.
    livexp.append_tail(BR_det(ir));
    return false;
}


bool GCSE::doPropCall(IR * ir, MOD List<IR*> & livexp, GCSECtx const& ctx)
{
    bool change = false;
    IR * arg = CALL_arg_list(ir);
    IR * next = nullptr;
    while (arg != nullptr) {
        next = arg->get_next();
        if (isCseCandidate(arg)) {
            if (processCSE(arg, livexp, ctx)) {
                //Has found CSE and replaced CSE with pr.
                change = true;
            } else {
                //Generate new CSE.
                livexp.append_tail(arg);
            }
        }
        arg = next;
    }
    return change;
}


bool GCSE::doPropAssign(IR * ir, MOD List<IR*> & livexp, GCSECtx const& ctx)
{
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->isDirectMemOp() || ir->isIndirectMemOp() ||
            ir->is_stpr() || ir->isCallStmt());
    IR * rhs = ir->getRHS();

    //Find CSE and replace it with properly PR.
    if (rhs != nullptr && isCseCandidate(rhs)) {
        if (processCSE(rhs, livexp, ctx)) {
            //Replaced CSE with PR successfully.
            return true;
        }
        //Generate new CSE.
        livexp.append_tail(rhs);
    }
    return false; //unchange
}


void GCSE::removeMayKill(IR * ir, MOD List<IR*> & livexp)
{
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->isDirectMemOp() || ir->isIndirectMemOp() ||
            ir->is_stpr() || ir->isCallStmt());
    MDSet tmp;
    MDSet const* maydef = ir->getMayRef();
    CollectMayUseRecur co(m_rg);
    if (maydef != nullptr && !maydef->is_empty()) {
        IRListIter ct2;
        IRListIter next;
        for (livexp.get_head(&ct2), next = ct2;
             ct2 != nullptr; ct2 = next) {
            livexp.get_next(&next);
            IR * x2 = ct2->val();
            tmp.clean(m_misc_bs_mgr);
            co.collect(x2, true, m_misc_bs_mgr, tmp);
            if (maydef->is_intersect(tmp)) {
                livexp.remove(ct2);
            }
        }
    }
    MD const* mustdef = ir->getMustRef();
    if (mustdef != nullptr) {
        IRListIter ct2;
        IRListIter next;
        for (livexp.get_head(&ct2), next = ct2;
             ct2 != nullptr; ct2 = next) {
            livexp.get_next(&next);
            IR * x2 = ct2->val();
            tmp.clean(m_misc_bs_mgr);
            co.collect(x2, true, m_misc_bs_mgr, tmp);
            if (tmp.is_overlap(mustdef, m_rg)) {
                livexp.remove(ct2);
            }
        }
    }
    tmp.clean(m_misc_bs_mgr);
}


bool GCSE::canElimCVT(IR const* exp, IR const* gen) const
{
    ASSERT0(exp && gen);
    if (!exp->is_cvt() || !gen->is_cvt()) {
        //CASE:pr29201.c, given following two IR expressions.
        //Since $59's value has been inferred out by GVN and is immdediate 1,
        //then we can infer out that the result of 'ne' is 0, namely, the VN1.
        //Similarly, we can infer out the result of 'cvt' is also VN1.
        //However, we can not elimiate CVT by NE.
        //  ne:bool id:103 (VN1,INT:intconst:i64 0|0x0) attachinfo:Dbx
        //    $59:bool id:78 (VN1,INT:intconst:i64 0|0x0) attachinfo:Dbx
        //    boolconst:bool 0 id:102 (VN1,INT:intconst:i64 0|0x0)
        //  ----
        //  cvt:*<1> id:10 (VN1,INT:intconst:i64 0|0x0)
        //    intconst:i32 0x0 id:9 (VN1,INT:intconst:i64 0|0x0)
        return false;
    }
    Type const* exp_type = exp->getType();
    Type const* gen_type = gen->getType();
    Type const* exp_src_type = CVT_exp(exp)->getType();
    Type const* gen_src_type = CVT_exp(gen)->getType();
    if (exp_type != gen_type) {
        //Result type must be same.
        return false;
    }
    if (exp->isFP()) {
        ASSERT0(gen->isFP());
        if (CVT_round(exp) != CVT_round(gen)) {
            //Float type's rounding mode must be same.
            return false;
        }
        if (exp_src_type != gen_src_type) {
            //CASE:exec.gr/gcse_cvt.gr
            //  $9:f32 = cvt:f32 2147483648:u32;
            //  $10:f32 = cvt:f32 2147483648:i32;
            //source type must be same.
            return false;
        }
        return true;
    }
    if (!exp_type->isInt() || !gen_type->isInt()) { return false; }
    if (exp_src_type == gen_src_type) { return true; }
    if (m_tm->getByteSize(exp_src_type) > m_tm->getByteSize(exp_type) &&
        m_tm->getByteSize(gen_src_type) > m_tm->getByteSize(gen_type)) {
        //The type-convertion that truncate data type always unchange value.
        return true;
    }
    bool exp_cvt_has_same_sign =
        !(exp_type->is_signed() ^ exp_src_type->is_signed());
    bool gen_cvt_has_same_sign =
        !(gen_type->is_signed() ^ gen_src_type->is_signed());
    if (!exp_cvt_has_same_sign || !gen_cvt_has_same_sign) {
        //CASE:cvt(i32, u8) can not be CSE of cvt(i32, u32).
        return false;
    }
    return true;
}


bool GCSE::doPropStmt(IR * ir, List<IR*> & livexp, GCSECtx const& ctx)
{
    bool change = false;
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    case IR_STPR:
        change |= doPropAssign(ir, livexp, ctx);
        break;
    SWITCH_CASE_CALL:
        change |= doPropCall(ir, livexp, ctx);
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        change |= doPropBranch(ir, livexp, ctx);
        break;
    case IR_RETURN:
        change |= doPropReturn(ir, livexp, ctx);
        break;
    default: break;
    }
    //Remove may-killed live-expr.
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_CALL:
    case IR_STPR: {
        removeMayKill(ir, livexp);
        break;
    }
    default:;
    }
    return change;
}


//Do propagation according to lexciographic equivalence.
bool GCSE::doPropExp(IRBB * bb, List<IR*> & livexp, GCSECtx const& ctx)
{
    livexp.clean();
    DefDBitSetCore * x = m_dumgr->getSolveSetMgr()->getAvailExprIn(bb->id());
    DefSBitSetIter st = nullptr;
    if (x != nullptr) {
        for (BSIdx i = x->get_first(&st);
             i != BS_UNDEF; i = x->get_next(i, &st)) {
            IR * y = m_rg->getIR(i);
            if (y->is_undef() || y->is_pr()) { continue; }
            ASSERT0(y && y->is_exp());
            livexp.append_tail(y);
        }
    }
    bool change = false;
    IRListIter ct;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != nullptr; ir = BB_irlist(bb).get_next(&ct)) {
        change |= doPropStmt(ir, livexp, ctx);
    }
    return change;
}


void GCSE::dumpEVN() const
{
    if (!getRegion()->isLogMgrInit()) { return; }
    note(getRegion(), "\n==---- DUMP InferEVN '%s' ----==",
         getRegion()->getRegionName());
    getRegion()->getLogMgr()->incIndent(2);
    ASSERT0(m_infer_evn);
    m_infer_evn->dumpBBListWithEVN();
    getRegion()->getLogMgr()->decIndent(2);
}


static bool dumpForTest(GCSE const* gcse)
{
    if (gcse->getActMgr().getActNum() == 0) { return true; }
    Region const* rg = gcse->getRegion();
    ASSERT0(rg->isLogMgrInit());
    note(rg, "\n==---- DUMP %s '%s' ----==",
         gcse->getPassName(), rg->getRegionName());
    rg->getLogMgr()->incIndent(2);
    gcse->getActMgr().dump();
    rg->getLogMgr()->decIndent(2);
    return true;
}


bool GCSE::dump() const
{
    if (!getRegion()->isLogMgrInit() || !g_dump_opt.isDumpGCSE()) {
        return true;
    }
    if (!g_dump_opt.isDumpAfterPass()) { return true; }
    if (g_dump_opt.isDumpForTest()) {
        return dumpForTest(this);
    }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    getRegion()->getLogMgr()->incIndent(2);
    m_am.dump();
    bool succ = Pass::dump();
    getRegion()->getLogMgr()->decIndent(2);
    return succ;
}


void GCSE::reset()
{
    m_vn2exp.clean();
    m_evn2exp.clean();
    m_exp2pr.clean();
    ASSERT0(m_infer_evn);
    m_infer_evn->clean();
    m_am.clean();
}


bool GCSE::initDepPass(MOD OptCtx & oc)
{
    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //The pass use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        set_valid(false);
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //The pass use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        set_valid(false);
        return false;
    }
    if (m_gvn != nullptr) {
        m_rg->getPassMgr()->checkValidAndRecompute(
            &oc, PASS_DOM, PASS_PDOM, PASS_GVN, PASS_UNDEF);
        ASSERT0(m_gvn->is_valid());
        //GVN provide more accurate result of value flow analysis than
        //expression analysis.
    }
    oc.setInvalidPass(PASS_EXPR_TAB);
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_DOM, PASS_PDOM, PASS_EXPR_TAB, PASS_UNDEF);
    m_expr_tab = (ExprTab*)m_rg->getPassMgr()->queryPass(PASS_EXPR_TAB);
    ASSERT0(m_expr_tab && m_expr_tab->is_valid());
    return true;
}


bool GCSE::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    START_TIMER(t, getPassName());
    if (!initDepPass(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }
    reset();
    bool change = false;
    IRBB * entry = m_cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    xcom::DomTree domtree;
    m_cfg->genDomTree(domtree);
    GCSECtx ctx(oc, domtree, &m_am, this);
    if (m_gvn != nullptr) {
        ASSERT0(getInferEVN());
        change = doPropVNInDomTreeOrder(domtree, ctx);
    } else {
        change = doPropExpInDomTreeOrder(domtree, ctx);
    }
    END_TIMER(t, getPassName());
    if (change) {
        dump();
        //no new expr generated, only new pr.
        oc.setInvalidIfDUMgrLiveChanged();

        //DU reference and du chain has maintained.
        ASSERT0(m_dumgr->verifyMDRef());
        ASSERT0(verifyClassicDUChain(m_rg, oc));
        ASSERT0(m_cfg->verifyRPO(oc));
        ASSERT0(m_cfg->verifyDomAndPdom(oc));
        ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
        ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg, oc));

        //For now, gvn has updated correctly.
        oc.setInvalidPass(PASS_EXPR_TAB);
    }
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    return change;
}
//END GCSE

} //namespace xoc
