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

class VF {
public:
    bool has_sideeffect;
public:
    VF() : has_sideeffect(false) {}
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


class IterSideEffect : public VisitIRTree<VF> {
public:
    IterSideEffect(IR const* ir, VF & vf) : VisitIRTree(vf) { visit(ir); }
};


class PropVNVisitFunc : public xcom::VisitTreeFuncBase {
    COPY_CONSTRUCTOR(PropVNVisitFunc);
    bool m_is_changed;
    IRCFG * m_cfg;
    GCSE * m_gcse;
public:
    PropVNVisitFunc(IRCFG * cfg, GCSE * gcse) : m_cfg(cfg), m_gcse(gcse)
    { m_is_changed = false; }

    bool isChanged() const { return m_is_changed; }

    void visitWhenAllKidHaveBeenVisited(
        Vertex const* v, Stack<Vertex const*> &)
    {}
    bool visitWhenFirstMeet(Vertex const* v, Stack<Vertex const*> &)
    {
        m_is_changed |= m_gcse->doPropVN(m_cfg->getBB(v->id()));
        return true;
    }
};


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
    List<IR*> m_livexp;
public:
    PropExpVisitFunc(IRCFG * cfg, GCSE * gcse) : m_cfg(cfg), m_gcse(gcse)
    { m_is_changed = false; }

    bool isChanged() const { return m_is_changed; }

    void visitWhenAllKidHaveBeenVisited(Vertex const*, Stack<Vertex const*> &)
    {}
    bool visitWhenFirstMeet(Vertex const* v, Stack<Vertex const*> &)
    {
        m_is_changed |= m_gcse->doPropExp(m_cfg->getBB(v->id()), m_livexp);
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
bool GCSE::doPropVNInDomTreeOrder(xcom::DomTree const& domtree)
{
    IRBB * entry = m_cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    PropVNVisitFunc vf(m_cfg, this);
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


void GCSE::dumpAct(IR const* oldexp, IR const* newexp)
{
    if (!getRegion()->isLogMgrInit() || !g_dump_opt.isDumpGCSE()) { return; }
    m_am.dump("%s is CSE and will be replaced by %s",
              DumpIRName().dump(oldexp), DumpIRName().dump(newexp));
}


void GCSE::elimCse(IR * use, IR * use_stmt, IR * gen)
{
    ASSERT0(use_stmt->isKids(use));
    ASSERT0(use->is_exp() && gen->is_exp() && use_stmt->is_stmt());

    //gen_pr hold the CSE value come from gen-stmt.
    //We eliminate the redundant computation via replace use by gen_pr.
    IR * gen_pr = m_exp2pr.get(gen);
    ASSERT0(gen_pr && gen_pr->is_pr());
    IR * new_pr = m_rg->dupIRTree(gen_pr);
    dumpAct(use, new_pr);
    bool f = use_stmt->replaceKid(use, new_pr, true);
    ASSERT0_DUMMYUSE(f);

    //Assign MD to new_pr.
    m_rg->getMDMgr()->allocMDForPROp(new_pr);

    //Set identical VN to new_pr with CSE.
    copyVN(new_pr, gen);

    //Add DU chain from gen_pr's stmt to new_pr.
    IR * gen_stmt = gen->getStmt();
    ASSERT0(gen_stmt->isPREqual(gen_pr));
    xoc::buildDUChain(gen_stmt, new_pr, m_rg, *getOptCtx());
    xoc::removeUseForTree(use, m_rg, *m_oc);
    m_rg->freeIRTree(use);
}


void GCSE::elimCseOfBranch(IR * use, IR * use_stmt, IN IR * gen)
{
    ASSERT0(use_stmt->isKids(use));
    ASSERT0(use->is_exp() && gen->is_exp() && use_stmt->is_stmt());

    //gen_pr hold the CSE value come from gen-stmt.
    //We eliminate the redundant computation via replace use by gen_pr.
    IR * gen_pr = m_exp2pr.get(gen);
    ASSERT0(gen_pr);
    IR * new_pr = m_rg->dupIRTree(gen_pr);
    dumpAct(use, new_pr);

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
    xoc::buildDUChain(gen_stmt, new_pr, m_rg, *getOptCtx());
    xoc::removeUseForTree(use, m_rg, *m_oc);
    m_rg->freeIRTree(use);
}


void GCSE::elimCseOfAssignment(IR * use, IR * use_stmt, IR * gen)
{
    if (use_stmt->is_stpr() && use_stmt->getRHS() == use) {
        //use-stmt becomes a move.
        IR_may_throw(use_stmt) = false;
        copyVN(use_stmt, gen);
    }
    elimCse(use, use_stmt, gen);
}


//Process the expression in CSE generation.
//This function do replacement via gvn info.
//e.g: ...=a+b <--generate CSE
//     ...
//     ...=a+b <--use CSE
//gen: generated cse.
void GCSE::processCseGen(IN IR * gen, IR * gen_stmt, bool & change)
{
    ASSERT0(gen->is_exp() && gen_stmt->is_stmt());
    //Move STORE_VAL to temp PR.
    //e.g: a = 10, expression of store_val is nullptr.
    IRBB * bb = gen_stmt->getBB();
    ASSERT0(bb);
    IR * tmp_pr = m_exp2pr.get(gen);
    if (tmp_pr != nullptr) { return; }

    //First process cse generation point.
    if (gen_stmt->is_truebr() || gen_stmt->is_falsebr()) {
        //Expect opnd1's type is same with opnd0.
        tmp_pr = m_rg->getIRMgr()->buildPR(BIN_opnd0(gen)->getType());
    } else {
        tmp_pr = m_rg->getIRMgr()->buildPR(gen->getType());
    }
    m_exp2pr.set(gen, tmp_pr);
    m_rg->getMDMgr()->allocMDForPROp(tmp_pr);
    IR * new_stpr = m_rg->getIRMgr()->buildStorePR(PR_no(tmp_pr),
                                                   IR_dt(tmp_pr), gen);
    m_rg->getMDMgr()->allocMDForPROp(new_stpr);
    copyVN(new_stpr, gen);
    copyDbx(new_stpr, gen_stmt, m_rg);

    //The 'find()' is fast because it is implemented with hash.
    IRListIter holder = nullptr;
    bool f = BB_irlist(bb).find(gen_stmt, &holder);
    ASSERT0_DUMMYUSE(f);
    ASSERT0_DUMMYUSE(holder);
    BB_irlist(bb).insert_before(new_stpr, holder);

    IR * newkid = tmp_pr;
    if (gen_stmt->isConditionalBr() && gen == BR_det(gen_stmt)) {
        //Det of branch stmt have to be judgement expression.
        newkid = m_rg->getIRMgr()->buildJudge(tmp_pr);
        copyDbx(newkid, tmp_pr, m_rg);
    }

    bool v = gen_stmt->replaceKid(gen, newkid, false);
    ASSERT0_DUMMYUSE(v);

    //Keep original DU unchange, add DU chain for new stmt.
    ASSERT0(tmp_pr->is_pr());
    xoc::buildDUChain(new_stpr, tmp_pr, m_rg, *getOptCtx());
    IR_may_throw(gen_stmt) = false;
    change = true;
}


bool GCSE::isCseCandidate(IR * ir)
{
    ASSERT0(ir);
    switch (ir->getCode()) {
    SWITCH_CASE_BIN:
    case IR_SELECT:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_ILD:
        ASSERT0(!ir->isDummyOp());
        return true;
    default: break;
    }
    return false;
}


bool GCSE::elim(IR * use, IR * use_stmt, IR * gen, IR * gen_stmt)
{
    //exp is CSE.
    //e.g: ...=a+b <--generate CSE
    //     ...
    //     ...=a+b <--use CSE
    bool change = false;
    processCseGen(gen, gen_stmt, change);
    switch (use_stmt->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_ARRAY_OP:
    SWITCH_CASE_WRITE_PR:
        elimCseOfAssignment(use, use_stmt, gen);
        change = true;
        return change;
    SWITCH_CASE_CALL:
        elimCse(use, use_stmt, gen);
        change = true;
        return change;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        elimCseOfBranch(use, use_stmt, gen);
        change = true;
        return change;
    case IR_RETURN:
        elimCse(use, use_stmt, gen);
        change = true;
        return change;
    default: ASSERTN(0, ("TODO:need to support"));
    }
    return change;
}


bool GCSE::isDom(IR const* exp_stmt, IR const* gen_stmt) const
{
    IRBB const* gen_bb = gen_stmt->getBB();
    IRBB const* exp_bb = exp_stmt->getBB();
    if (m_cfg->hasEHEdge()) {
        ASSERT0(m_tg);
        if (gen_bb == exp_bb) {
            return gen_bb->is_dom(gen_stmt, exp_stmt, true);
        }
        return m_tg->is_dom(gen_bb->id(), exp_bb->id());
    }
    if (gen_bb == exp_bb) {
        return gen_bb->is_dom(gen_stmt, exp_stmt, true);
    }
    return m_cfg->is_dom(gen_bb->id(), exp_bb->id());
}


bool GCSE::findAndElim(IR * exp, IR * gen)
{
    ASSERT0(exp && gen);
    ASSERT0(exp != gen);
    IR * exp_stmt = exp->getStmt();
    IR * gen_stmt = gen->getStmt();
    ASSERT0(exp_stmt->getBB() && gen_stmt->getBB());
    if (!isDom(exp_stmt, gen_stmt)) { return false; }
    return elim(exp, exp_stmt, gen, gen_stmt);
}


//If find 'exp' is CSE, replace it with related pr.
//NOTE: exp should be freed.
bool GCSE::processCse(IN IR * exp, IN List<IR*> & livexp)
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
        return elim(exp, expstmt, gen, gen_stmt);
    }
    return change;
}


bool GCSE::handleCandidateByExprRep(IR * exp)
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
        if (!isDom(occ_stmt, exp_stmt)) { continue; }
        if (!xoc::hasSameUniqueMustDefForTree(exp, occ, m_rg)) { continue; }
        changed |= findAndElim(occ, exp);
        e->getOccList().remove(it);
    }
    return changed;
}


bool GCSE::handleCandidate(IR * exp, IRBB * bb)
{
    VN const* vn = m_gvn->getVN(exp);
    if (vn != nullptr) {
        IR * gen = m_vn2exp.get(vn);
        if (gen != nullptr) {
            //Found CSE and replaced it with pr.
            return findAndElim(exp, gen);
        }
        m_vn2exp.set(vn, exp);
    }
    return handleCandidateByExprRep(exp);
}


//Determine if det-exp of truebr/falsebr ought to be cse.
bool GCSE::shouldBeCse(IR * det)
{
    ASSERT0(det->is_judge());

    //If the det if simply enough, cse is dispensable.
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


bool GCSE::doPropExpInDomTreeOrder(xcom::DomTree const& domtree)
{
    IRBB * entry = m_cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    PropExpVisitFunc vf(m_cfg, this);
    PropExpVisit prop(entry, domtree, vf);
    prop.visit();
    return vf.isChanged();
}


bool GCSE::hasSideEffect(IR const* ir) const
{
    VF vf;
    IterSideEffect it(ir, vf);
    return vf.has_sideeffect;
}


//Do propagation according to value numbering.
bool GCSE::doPropVN(IRBB * bb)
{
    bool change = false;
    IRListIter ct;
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != nullptr; ir = BB_irlist(bb).get_next(&ct)) {
        switch (ir->getCode()) {
        SWITCH_CASE_DIRECT_MEM_STMT:
        SWITCH_CASE_INDIRECT_MEM_STMT:
        case IR_STPR: {
            if (hasSideEffect(ir)) { break; }
            IR * rhs = ir->getRHS();
            if (rhs == nullptr) {
                //Virtual OP may not have RHS.
                break;
            }
            //Find CSE and replace it with properly PR.
            if (isCseCandidate(rhs)) {
                change |= handleCandidate(rhs, bb);
            }
            break;
        }
        SWITCH_CASE_CALL: {
            if (hasSideEffect(ir)) { break; }
            IR * p = CALL_param_list(ir);
            IR * next = nullptr;
            bool lchange = false;
            m_newst_lst.clean();
            while (p != nullptr) {
                next = p->get_next();
                if (isCseCandidate(p)) {
                    change |= handleCandidate(p, bb);
                }
                p = next;
            }
            change |= lchange;
            break;
        }
        SWITCH_CASE_CONDITIONAL_BRANCH_OP:
            if (hasSideEffect(ir)) { break; }
            //Find CSE and replace it with properly pr.
            ASSERT0(BR_det(ir));
            if (isCseCandidate(BR_det(ir)) && shouldBeCse(BR_det(ir))) {
                change |= handleCandidate(BR_det(ir), bb);
            }
            break;
        case IR_RETURN:
            if (hasSideEffect(ir)) { break; }
            if (RET_exp(ir) != nullptr && isCseCandidate(RET_exp(ir))) {
                change |= handleCandidate(RET_exp(ir), bb);
            }
            break;
        default: break;
        }
    }
    return change;
}


bool GCSE::doPropReturn(IR * ir, MOD List<IR*> & livexp)
{
    ASSERT0(ir->is_return());
    if (RET_exp(ir) != nullptr && isCseCandidate(RET_exp(ir)) &&
        shouldBeCse(RET_exp(ir))) {
        if (processCse(RET_exp(ir), livexp)) {
            //Found CSE and replaced CSE with PR successfully.
            return true;
        }
        //Generate new CSE.
        livexp.append_tail(RET_exp(ir));
    }
    return false;
}


bool GCSE::doPropBranch(IR * ir, MOD List<IR*> & livexp)
{
    ASSERT0(ir->isBranch());
    if (isCseCandidate(BR_det(ir)) && shouldBeCse(BR_det(ir))) {
        if (processCse(BR_det(ir), livexp)) {
            //Replaced CSE with PR successfully.
            return true;
        }
        //Generate new CSE.
        livexp.append_tail(BR_det(ir));
    }
    return false;
}


bool GCSE::doPropCall(IR * ir, MOD List<IR*> & livexp)
{
    bool change = false;
    IR * param = CALL_param_list(ir);
    IR * next = nullptr;
    while (param != nullptr) {
        next = param->get_next();
        if (isCseCandidate(param)) {
            if (processCse(param, livexp)) {
                //Has found cse and replaced cse with pr.
                change = true;
            } else {
                //Generate new cse.
                livexp.append_tail(param);
            }
        }
        param = next;
    }
    return change;
}


bool GCSE::doPropAssign(IR * ir, MOD List<IR*> & livexp)
{
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->is_stmt());
    ASSERT0(ir->isDirectMemOp() || ir->isIndirectMemOp() ||
            ir->is_stpr() || ir->isCallStmt());
    IR * rhs = ir->getRHS();
    //Find CSE and replace it with properly PR.
    if (rhs != nullptr && isCseCandidate(rhs)) {
        if (processCse(rhs, livexp)) {
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
    if (maydef != nullptr && !maydef->is_empty()) {
        IRListIter ct2;
        IRListIter next;
        for (livexp.get_head(&ct2), next = ct2;
             ct2 != nullptr; ct2 = next) {
            livexp.get_next(&next);
            IR * x2 = ct2->val();
            tmp.clean(m_misc_bs_mgr);
            DUMgr::collectMayUseRecursive(
                x2, m_rg, true, m_misc_bs_mgr, tmp);
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
            DUMgr::collectMayUseRecursive(x2, m_rg, true,
                                          m_misc_bs_mgr, tmp);
            if (tmp.is_overlap(mustdef, m_rg)) {
                livexp.remove(ct2);
            }
        }
    }
    tmp.clean(m_misc_bs_mgr);
}


bool GCSE::doPropStmt(IR * ir, List<IR*> & livexp)
{
    bool change = false;
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    case IR_STPR:
        change |= doPropAssign(ir, livexp);
        break;
    SWITCH_CASE_CALL:
        change |= doPropCall(ir, livexp);
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        change |= doPropBranch(ir, livexp);
        break;
    case IR_RETURN:
        change |= doPropReturn(ir, livexp);
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
bool GCSE::doPropExp(IRBB * bb, List<IR*> & livexp)
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
        change |= doPropStmt(ir, livexp);
    }
    return change;
}


bool GCSE::dump() const
{
    if (!getRegion()->isLogMgrInit() || !g_dump_opt.isDumpGCSE()) {
        return true;
    }
    if (!g_dump_opt.isDumpAfterPass()) { return true; }
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
    m_exp2pr.clean();
}


bool GCSE::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
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
    m_oc = &oc;
    START_TIMER(t, getPassName());
    if (m_gvn != nullptr) {
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_PDOM,
                                                   PASS_GVN, PASS_UNDEF);
        ASSERT0(m_gvn->is_valid());
        //GVN provide more accurate result of value flow analysis than
        //expression analysis.
    }
    oc.setInvalidPass(PASS_EXPR_TAB);
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_PDOM,
                                               PASS_EXPR_TAB, PASS_UNDEF);
    m_expr_tab = (ExprTab*)m_rg->getPassMgr()->queryPass(PASS_EXPR_TAB);
    ASSERT0(m_expr_tab && m_expr_tab->is_valid());
    reset();
    bool change = false;
    IRBB * entry = m_cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    xcom::DomTree domtree;
    m_cfg->genDomTree(domtree);
    xcom::Vertex * root = domtree.getVertex(entry->id());
    if (m_cfg->hasEHEdge()) {
        //Initialize Temp CFG and pick out exception-edge.
        m_tg = new TG(m_rg);
        m_tg->clone(*m_cfg, false, false);
        m_tg->pick_eh();
        m_tg->removeUnreachNode(entry->id());
        m_tg->computeDomAndIdom();
        m_tg->computePdomAndIpdom(root);
    }
    if (m_gvn != nullptr) {
        change = doPropVNInDomTreeOrder(domtree);
    } else {
        change = doPropExpInDomTreeOrder(domtree);
    }
    END_TIMER(t, getPassName());
    dump();
    if (change) {
        //no new expr generated, only new pr.
        oc.setInvalidIfDUMgrLiveChanged();

        //DU reference and du chain has maintained.
        ASSERT0(m_dumgr->verifyMDRef());
        ASSERT0(verifyMDDUChain(m_rg, oc));
        ASSERT0(m_cfg->verifyRPO(oc));
        ASSERT0(m_cfg->verifyDomAndPdom(oc));
        ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
        ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
        //For now, gvn has updated correctly.
        oc.setInvalidPass(PASS_EXPR_TAB);
    }
    if (m_cfg->hasEHEdge()) {
        ASSERT0(m_tg);
        delete m_tg;
        m_tg = nullptr;
    }
    ASSERT0(m_tg == nullptr);
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    return change;
}
//END GCSE

} //namespace xoc
