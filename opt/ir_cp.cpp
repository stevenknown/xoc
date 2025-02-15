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

static void dumpAct(
    CopyProp * cp, IR const* def_stmt, IR const* prop_value,
    IR const* use)
{
    Region const* rg = cp->getRegion();
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpLICM()) { return; }
    class Dump : public xoc::DumpToBuf {
    public:
        CopyProp const* cp;
        IR const* def_stmt;
        IR const* prop_value;
        IR const* use;
        MDSSAMgr * mdssamgr;
        Dump(Region const* rg, xcom::StrBuf & buf) : DumpToBuf(rg, buf, 2)
        { mdssamgr = rg->getMDSSAMgr(); }
        virtual void dumpUserInfo() const override
        {
            Region const* rg = getRegion();
            note(getRegion(),
                 "\nPROPAGATING CANDIDATE: %s THAT LOCATED IN STMT:",
                 DumpIRName().dump(prop_value));
            rg->getLogMgr()->incIndent(2);
            xoc::dumpIR(def_stmt, rg, nullptr,
                        DumpFlag::combineIRID(IR_DUMP_KID|IR_DUMP_VAR_DECL));
            rg->getLogMgr()->decIndent(2);

            note(getRegion(), "\nWILL REPLACE %s THAT LOCATED IN STMT:",
                 DumpIRName().dump(use));
            rg->getLogMgr()->incIndent(2);
            if (use->is_id()) {
                ASSERT0(mdssamgr);
                MDSSAInfo * mdssainfo = mdssamgr->getUseDefMgr()->
                    getMDSSAInfo(use);
                ASSERT0_DUMMYUSE(mdssainfo);
                ASSERT0(ID_phi(use));
                note(getRegion(), "\n");
                ID_phi(use)->dump(rg, mdssamgr->getUseDefMgr());
            } else {
                ASSERT0(use->getStmt());
                xoc::dumpIR(use->getStmt(), rg, nullptr,
                    DumpFlag::combineIRID(IR_DUMP_KID|IR_DUMP_VAR_DECL));
            }
            rg->getLogMgr()->decIndent(2);
        }
    };
    xcom::StrBuf buf(32);
    Dump d(rg, buf);
    d.cp = cp;
    d.def_stmt = def_stmt;
    d.prop_value = prop_value;
    d.use = use;
    cp->getActMgr().dump("%s", d.dump());
}


class PropVisitFunc : public xcom::VisitTreeFuncBase {
    COPY_CONSTRUCTOR(PropVisitFunc);
    bool m_is_changed;
    IRCFG * m_cfg;
    CopyProp * m_cp;
    IRSet m_useset; //for local used
public:
    PropVisitFunc(IRCFG * cfg, CopyProp * cp, DefSegMgr * segmgr)
        : m_cfg(cfg), m_cp(cp), m_useset(segmgr)
    { m_is_changed = false; }

    bool isChanged() const { return m_is_changed; }

    void visitWhenAllKidHaveBeenVisited(Vertex const*, Stack<Vertex const*> &)
    {}
    bool visitWhenFirstMeet(Vertex const* v, Stack<Vertex const*> &)
    {
        m_is_changed |= m_cp->doPropBB(m_cfg->getBB(v->id()), m_useset);
        return true;
    }
};


//
//START PropVisit
//
class PropVisit : public xcom::VisitTree<PropVisitFunc> {
    COPY_CONSTRUCTOR(PropVisit);
public:
    PropVisit(IRBB * root, DomTree const& domtree, PropVisitFunc & vf)
        : VisitTree(domtree, root->id(), vf) {}
};
//END PropVisit


//
//START CopyProp
//
CopyProp::CopyProp(Region * rg) :
    Pass(rg), m_prop_kind(CP_PROP_UNDEF), m_am(rg)
{
    m_md_sys = rg->getMDSystem();
    m_dumgr = rg->getDUMgr();
    m_cfg = rg->getCFG();
    m_md_set_mgr = rg->getMDSetMgr();
    m_tm = rg->getTypeMgr();
    m_gvn = nullptr;
    m_oc = nullptr;
    m_mdssamgr = nullptr;
    m_prssamgr = nullptr;
    ASSERT0(m_cfg && m_dumgr && m_md_sys && m_tm && m_md_set_mgr);
    UINT flag = CP_PROP_CONST|CP_PROP_PR|CP_PROP_NONPR;

    //CASE:By default, we do not intend to add CP_PROP_TO_PHI_OPND to be
    //propagation-kind. The reason is propagating Constant Value to phi operand
    //will produce two or more empty predecessors BBs, which will confuse DCE
    //functions when removing an ineffect Branch Stmt(such as TrueBr). And this
    //will lead to incorrect removing of PHI operand. Finally, causing
    //incorrect behaviors during PRSSA destruction.
    //e.g:exec/cse.c, with cmdline -O3 -only-rce -cp_aggr -gvn -cfgopt
    //-dce_aggr
    //Before propagation:
    //  BB13
    //  falsebr _$L7 ne $3, 0
    //   | |______________
    //   V                |
    //  BB16:             |
    //  stpr $4 = 0;      |
    //  goto _$L8;        |
    //   |                |
    //   V                |
    //  BB18:_$L7         |
    //  stpr $5 = -1;     |
    //   |  ______________|
    //   | |
    //   V V
    //  BB19:_$L8
    //  phi $6 = ($4, BB16), ($5, BB18)
    //  return $6;
    //
    //After propagation, RCE pass and removing all STPR that without any USE,
    //the BB list will be:
    //  BB13:
    //  falsebr _$L8 ne $3, 0; #S1
    //   | |______________
    //   V                |
    //  BB18: //empty BB  |
    //   |  ______________|
    //   | |
    //   V V
    //  BB19:_$L8
    //  phi $6 = (0, BB13), (-1, BB18);
    //  return $6;
    //During DCE pass, it should remove #S1, edge BB13->BB19, BB18->BB19, and
    //PHI simultaneously. However, for now, DCE can not perform the expected
    //removing behaviour. For the sake of avoiding propagate constant to PHI
    //operand could prevent DCE's incorrect optimization, and leave the job of
    //removing FALSEBR to RCE pass.
    //flag |= CP_PROP_CONST_TO_PHI_OPND;
    m_prop_kind.set(flag);
}


//Return true if ir's type is consistent with 'cand_exp'.
bool CopyProp::checkTypeConsistency(IR const* ir, IR const* cand_exp) const
{
    Type const* t1 = ir->getType();
    Type const* t2 = cand_exp->getType();

    //Do copy-prog even if data type is ANY.
    if (t1 == t2) { return true; }

    if (t1->is_scalar() && t2->is_scalar()) {
        if (t1->is_signed() ^ t2->is_signed()) {
            //Sign must be consistent.
            return false;
        }
        if (m_tm->getByteSize(t1) < m_tm->getByteSize(t2)) {
            //ir size must be equal or great than cand.
            return false;
        }
        return true;
    }
    if (t1->isPointer() && t2->isPointer()) {
        return true;
    }
    if (t1->is_any() || t2->is_any()) {
        //ANY is not consistent with other type.
        return false;
    }
    return m_tm->getByteSize(t1) >= m_tm->getByteSize(t2);
}


//The function try to maintain VN when copy occurred.
void CopyProp::copyVN(IR const* from, IR const* to) const
{
    if (useGVN()) { m_gvn->copyVN(from, to); }
}


//Check and replace 'exp' with 'cand_exp' if they are
//equal, and update DU info. If 'cand_exp' is NOT leaf,
//that will create redundant computation, and
//depends on later Redundancy Elimination to reverse back.
//exp: expression that will be replaced.
//cand_exp: substitute cand_exp for exp.
//  e.g: cand_exp is *p, cand_exp_md is MD3
//      *p(MD3) = 10 //p point to MD3
//      ...
//      g = *q(MD3) //q point to MD3
void CopyProp::replaceExp(MOD IR * exp, IR const* cand_exp, MOD CPCtx & ctx)
{
    ASSERT0(exp && exp->is_exp() && cand_exp);
    ASSERT0(exp->getExactRef() || allowInexactMD() ||
            exp->is_id() || //exp is operand of MD PHI
            exp->is_pr());  //exp is operand of PR PHI
    if (!checkTypeConsistency(exp, cand_exp)) { return; }

    //The memory that 'exp' pointed to is same to 'cand_exp' because
    //cand_exp has been garanteed that will not change in propagation
    //interval.
    //IR * parent = exp->getParent();
    //if (parent->is_ild()) {
    //    CPC_need_recomp_aa(ctx) = true;
    //} else if (parent->is_ist() && exp == IST_base(parent)) {
    //    if (!cand_exp->is_ld() &&
    //        !cand_exp->is_pr() &&
    //        !cand_exp->is_lda()) {
    //        return;
    //    }
    //    CPC_need_recomp_aa(ctx) = true;
    //}
    ASSERT0(cand_exp->getStmt());
    IR * newir = m_rg->dupIRTree(cand_exp);
    xoc::addUseForTree(newir, cand_exp, m_rg);
    xoc::removeUseForTree(exp, m_rg, *getOptCtx());
    CPC_change(ctx) = true;
    if (newir->mustBePointerType()) { ASSERT0(newir->is_ptr()); }
    else if (newir->mustBeBoolType()) { ASSERT0(newir->is_bool()); }
    else { newir->setType(exp->getType()); }

    if (exp->is_id()) {
        ASSERT0(ID_phi(exp));
        ID_phi(exp)->replaceOpnd(exp, newir);
    } else {
        bool doit = exp->getParent()->replaceKid(exp, newir, false);
        ASSERT0(doit);
        DUMMYUSE(doit);
    }
    copyVN(cand_exp, newir);
    m_rg->freeIRTree(exp);

    //Fixup PRSSA|MDSSA DUChain.
    //NOTE classic DU chain does not need to find the live-in DEF.
    IR * stmt = newir->getStmt();
    IRBB * stmtbb = stmt->getBB();
    xoc::findAndSetLiveInDef(newir, stmtbb->getPrevIR(stmt), stmtbb,
                             m_rg, *getOptCtx());
}


bool CopyProp::isCopyOP(IR * ir) const
{
    if (ir->isDummyOp()) { return false; }
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    case IR_STPR:
        if (ir->is_stpr() && !is_aggressive() && !isLowCostExp(ir->getRHS())) {
            //CASE:Under unaggressive mode, propagating LD/ILD through PR may
            //degrade performance, e.g:
            //  pr1 = LD(x)
            //  while (..) {
            //     = pr1
            //  }
            //====> after propagate LD(x)
            //  pr1 = LD(x)
            //  while (..) {
            //     = LD(x)
            //  }
            //This might lead perform degradation.
            return false;
        }
        return canBeCandidate(ir->getRHS());
    case IR_PHI:
        if (xcom::cnt_list(PHI_opnd_list(ir)) == 1) {
            return true;
        }
    default:;
    }
    return false;
}


bool CopyProp::existMayDefTillBB(
    IR const* exp, IRBB const* start, IRBB const* meetup) const
{
    xcom::GraphIterIn iterin(*m_cfg, start->getVex());
    for (Vertex const* t = iterin.get_first();
         t != nullptr; t = iterin.get_next(t)) {
        if (t == meetup->getVex()) { continue; }
        IRListIter irit = nullptr;
        IRBB * tbb = m_cfg->getBB(t->id());
        ASSERT0(tbb);
        for (IR const* tir = tbb->getIRList().get_head(&irit);
             tir != nullptr; tir = tbb->getIRList().get_next(&irit)) {
            if (xoc::isDependentForTree(tir, exp, is_aggressive(), m_rg)) {
                return true;
            }
        }
    }
    return false;
}


//def_stmt: the stmt of 'prop_value'.
//prop_value: the expression to be propagated.
//nextit: the iter of the next IR of 'def_stmt'.
bool CopyProp::existMayDefTillEndOfCurBB(
    IR const* def_stmt, IR const* prop_value, IRListIter const& curit) const
{
    ASSERT0(curit);
    ASSERT0(curit->val());
    ASSERT0(curit->val()->getBB() == def_stmt->getBB());
    IRBB * defbb = def_stmt->getBB();
    IRListIter it = curit;
    for (IR const* ir = it->val(); ir != nullptr;
         ir = defbb->getIRList().get_next(&it)) {
        if (xoc::isDependentForTree(ir, prop_value, is_aggressive(), m_rg)) {
            return true;
        }
    }
    return false;
}


//Both def_stmt and use_stmt are in same BB.
//curit: the IR list iter of 'def_stmt'.
bool CopyProp::isAvailableInSameBB(
    IR const* def_stmt, IR const* use_stmt, IR const* prop_value,
    IRListIter const& curit) const
{
    IRBB * defbb = def_stmt->getBB();
    IR * ir = curit->val();
    for (IRListIter it = curit; ir != use_stmt && ir != nullptr;
         ir = BB_irlist(defbb).get_next(&it)) {
        if (xoc::isDependentForTree(ir, prop_value, is_aggressive(), m_rg)) {
            return false;
        }
    }
    if (ir == use_stmt) {
        return true;
    }
    ASSERTN(defbb == use_stmt->getBB(),
            ("def_stmt should be in same bb with use_stmt"));
    return false;
}


bool CopyProp::canBeCandidate(IR const* ir) const
{
    ASSERT0(ir->is_exp());
    if (ir->is_lda() || isLDACVT(ir) || ir->isConstExp()) {
        return m_prop_kind.have(CP_PROP_CONST);
    }
    if (ir->isReadPR() || isPRCVT(ir)) {
        return m_prop_kind.have(CP_PROP_PR);
    }
    switch (ir->getCode()) {
    SWITCH_CASE_UNA:
        return m_prop_kind.have(CP_PROP_UNARY);
    default:;
    }
    if (ir->isMemRefNonPR() && m_prop_kind.have(CP_PROP_NONPR)) {
        //TODO:In aggressive mode, we anticipate propagating RHS
        //expression even if it is inexact.
        //e.g: s is MC type.
        //    s = *p
        //    *q = s
        //  *p can be propagated.
        if (ir->getExactRef() == nullptr && !allowInexactMD()) {
           return false;
        }
        return true;
    }
    return false;
}


//Return true if 'prop_value' does not be modified till meeting 'use_stmt'.
//e.g:xx = prop_value //def_stmt
//    ..
//    ..
//    use_bb:
//    yy = xx  //use_stmt|use_phi
//
//def_stmt: ir stmt.
//curit: the IR list iter of 'def_stmt'.
//prop_value: expression that will be propagated.
//Note either use_phi or use_stmt is nullptr.
bool CopyProp::isAvailable(IR const* def_stmt, IR const* prop_value,
                           IR const* repexp, IRListIter const& curit) const
{
    IR * use_stmt = nullptr;
    MDPhi * use_phi = nullptr;
    IRBB * usebb = nullptr;
    if (repexp->is_id()) {
        ASSERT0(ID_phi(repexp));
        usebb = ID_phi(repexp)->getBB();
        use_phi = ID_phi(repexp);
        ASSERT0(use_phi && use_phi->is_phi());
    } else {
        use_stmt = repexp->getStmt();
        ASSERT0(use_stmt->is_stmt() && use_stmt->getBB());
        usebb = use_stmt->getBB();
        use_phi = nullptr;
    }
    ASSERT0(usebb);
    ASSERT0(use_stmt || use_phi);
    if (def_stmt == use_stmt) { return false; }

    if (isSimpCVT(prop_value)) {
        prop_value = getSimpCVTValue(prop_value);
        ASSERT0(prop_value);
    }
    if (prop_value->is_const() || prop_value->is_lda()) { return true; }

    //Need to check overlapped MDSet.
    //e.g: Suppose occ is '*p + *q', {p->a}, {q->b}.
    //occ can NOT get reach to 'def_ir' if one of p, q, a, b
    //be modified during the path.
    IRBB * defbb = def_stmt->getBB();
    if (defbb != usebb && !m_cfg->is_dom(defbb->id(), usebb->id())) {
        return false;
    }

    if (defbb == usebb) {
        //Both def_ir and use_ir are in same BB.
        return isAvailableInSameBB(def_stmt, use_stmt, prop_value, curit);
    }
    if (use_phi != nullptr || use_stmt->is_phi()) {
        //Propagate value to phi operand.
        //Nothing to do.
    }
    if (existMayDefTillEndOfCurBB(def_stmt, prop_value, curit)) {
        return false;
    }
    if (existMayDefTillBB(prop_value, usebb, defbb)) {
        return false;
    }
    return true;
}


//Return true if ir is CVT with cvt-exp that always include low-cost
//expression. These low-cost always profitable and may bring up new
//optimization opportunity.
bool CopyProp::isLowCostCVT(IR const* ir) const
{
    if (!ir->is_cvt()) { return false; }
    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
            continue;
        }
        switch (ir->getCode()) {
        case IR_CONST:
        SWITCH_CASE_READ_PR:
        case IR_LDA:
            return true;
        default: return false;
        }
    }
    UNREACHABLE();
    return false;
}


//Return true if CVT with simply cvt-exp that can be regard as
//copy-propagate candidate.
bool CopyProp::isLDACVT(IR const* ir) const
{
    ASSERT0(ir);
    if (!ir->is_cvt()) { return false; }
    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
            continue;
        }
        return ir->is_lda();
    }
    UNREACHABLE();
    return false;
}


//Return true if CVT with simply cvt-exp that can be regard as
//copy-propagate candidate.
bool CopyProp::isPRCVT(IR const* ir) const
{
    ASSERT0(ir);
    if (!ir->is_cvt()) { return false; }
    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
            continue;
        }
        return ir->isReadPR();
    }
    UNREACHABLE();
    return false;
}


//Return true if CVT with simply cvt-exp that can be regard as
//copy-propagate candidate.
bool CopyProp::isSimpCVT(IR const* ir) const
{
    ASSERT0(ir);
    if (!ir->is_cvt()) { return false; }
    IR const* orgcvt = ir;
    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
            continue;
        }
        switch (ir->getCode()) {
        SWITCH_CASE_READ_PR:
        SWITCH_CASE_DIRECT_MEM_EXP:
        case IR_CONST:
        case IR_LDA:
            if (orgcvt->is_fp() && ir->is_fp() &&
                orgcvt->getType() != ir->getType()) {
                //Different float point type conversion can not be skipped.
                //CASE: $1:f64 = cvt:fp64 ($2:fp32)
                //      ... = $1:64 #USE
                //Where $2:f32 can NOT be propagated to $1 because of type.
                return false;
            }
            return true;
        default: return false;
        }
    }
    UNREACHABLE();
    return false;
}


//CVT with simply cvt-exp is copy-propagate candidate.
IR const* CopyProp::getSimpCVTValue(IR const* ir) const
{
    if (!ir->is_cvt()) { return nullptr; }
    IR const* orgcvt = ir;
    for (;;) {
        if (ir->is_cvt()) {
            ir = CVT_exp(ir);
            continue;
        }
        switch (ir->getCode()) {
        SWITCH_CASE_DIRECT_MEM_EXP:
        SWITCH_CASE_READ_PR:
        case IR_CONST:
        case IR_LDA:
            if (orgcvt->is_fp() && ir->is_fp() &&
                orgcvt->getType() != ir->getType()) {
                //Different float point type conversion can not be skipped.
                //CASE: $1:f64 = cvt:fp64 ($2:fp32)
                //      ... = $1:64 #USE
                //Where $2:f32 can NOT be propagated to $1 because of type.
                UNREACHABLE();
            }
            return ir;
        default: return nullptr;
        }
    }
    UNREACHABLE();
    return nullptr;
}


//Get the value expression that to be propagated.
IR * CopyProp::getPropagatedValue(IR * stmt)
{
    if (stmt->hasRHS()) { return stmt->getRHS(); }
    switch (stmt->getCode()) {
    case IR_PHI: return PHI_opnd_list(stmt);
    default:;
    }
    UNREACHABLE();
    return nullptr;
}


bool CopyProp::doPropForMDPhi(IR const* prop_value, MOD IR * use)
{
    CPCtx lchange;
    replaceExp(use, prop_value, lchange);
    return CPC_change(lchange);
}


bool CopyProp::doPropForNormalStmt(
    IRListIter curit, IRListIter * nextit, IR const* prop_value, MOD IR * use,
    IRBB * def_bb)
{
    IR * use_stmt = use->getStmt();
    ASSERT0(use_stmt && use_stmt->is_stmt() && use_stmt->getBB());
    IRBB * use_bb = use_stmt->getBB();
    CPCtx lchange;
    IR * old_use_stmt = use_stmt;
    replaceExp(use, prop_value, lchange);
    ASSERTN(use_stmt->is_stmt(), ("ensure use_stmt still legal"));
    if (!CPC_change(lchange)) { return false; }

    //Indicate whether use_stmt is the next stmt of def_stmt.
    bool is_next = false;
    if (*nextit != nullptr && use_stmt == (*nextit)->val()) {
        is_next = true;
    }

    if (use_stmt == old_use_stmt) { return true; }

    //old_use_stmt has been removed and new stmt generated.
    ASSERTN(old_use_stmt->is_undef(), ("the old one should be freed"));
    IRListIter irct = nullptr;
    BB_irlist(use_bb).find(old_use_stmt, &irct);
    ASSERTN(irct, ("oldstmt still stayed in BB"));
    BB_irlist(use_bb).insert_before(use_stmt, irct);
    BB_irlist(use_bb).remove(irct);
    if (is_next) {
        //Update nextit to reflect the change of old_use_stmt.
        //e.g: st x = lda y;
        //     ist(x) = 0; #old_use_stmt
        // ====>
        //     st x = lda y;
        //     st y = 0; #use_stmt(new generated)
        *nextit = curit;
        BB_irlist(def_bb).get_next(nextit);
    }
    return true;
}


IR const* CopyProp::pickUpCandExp(
    IR const* prop_value, IR const* repexp, IR const* def_stmt,
    IRListIter const& curit, bool prssadu, bool mdssadu) const
{
    if (repexp->is_id()) {
        if (!prop_value->is_const()) {
            //Do NOT propagate non-const value to operand of MD PHI.
            return nullptr;
        }
        //TODO: For now, we will not propagate CONST value to PHI for the
        //time being. Because IR_DCE will remove the
        //'def_stmt' if there is no USE of it. And we should insert an
        //assignment of the CONST value during stripping-PHI of MDSSA.
        //e.g:i1 = 1; //S1
        //    LOOP:
        //    i3 = phi(i1, i2); //S2
        //    i2 = i3 + 1;
        //    truebr LOOP;
        //  After IR_CP, 1 has been propagated to S2:
        //    i1 = 1;
        //    LOOP:
        //    i3 = phi(1, i2);
        //    i2 = i3 + 1;
        //    truebr LOOP;
        //  And after IR_DCE, S1 is removed:
        //    LOOP:
        //    i3 = phi(1, i2);
        //    i2 = i3 + 1;
        //    truebr LOOP;
        // phi stripping should deal with the situation.
        ASSERT0(ID_phi(repexp) && ID_phi(repexp)->is_phi());
        return nullptr;
    }
    if (repexp->getExactRef() == nullptr && !repexp->isPROp()) {
        //repexp is inexact.
        if (!allowInexactMD()) {
            //Do NOT progate value to inexact memory reference, except PR.
            return nullptr;
        }
        if (!xoc::isKillingDef(def_stmt, repexp, m_gvn)) {
            return nullptr;
        }
    } else if (!xoc::isKillingDef(def_stmt, repexp, m_gvn)) {
        return nullptr;
    }
    if (!isAvailable(def_stmt, prop_value, repexp, curit)) {
        //The value that will be propagated can
        //not be killed during 'def_stmt' and 'use_stmt'.
        //e.g:
        //    g = a; //S1
        //    if (...) {
        //        a = ...; //S3
        //    }
        //    ... = g; //S2
        //g can not be propagted since a is killed by S3.
        return nullptr;
    }
    if (!prssadu && !mdssadu &&
        !m_dumgr->isExactAndUniqueDef(def_stmt, repexp)) {
        //Only single definition is allowed.
        //e.g:
        //    g = 20; //S3
        //    if (...) {
        //        g = 10; //S1
        //    }
        //    ... = g; //S2
        //g can not be propagated since there are
        //more than one definitions are able to get to S2.
        return nullptr;
    }
    if (prop_value->is_cvt()) {
        return tryDiscardCVT(prop_value);
    }
    return prop_value;
}


//Check if the CVT can be discarded and the cvt-expression will be regarded
//as the recommended propagate value.
//prop_value: indicates the value that will be propagated, must be CVT.
//Note that user can implement target dependent interface to enable
//more policies.
IR const* CopyProp::tryDiscardCVT(IR const* prop_value) const
{
    ASSERT0(prop_value->is_cvt());
    IR const* leaf = ((CCvt*)prop_value)->getLeafExp();
    if (leaf->is_lda() || leaf->is_const()) {
        //CASE: If the different type of LDA and CONST progagated into
        //unsuitable IR tree, the combination of IR tree may complain and
        //report assertion. For now, we do not progagate these cases.
        return prop_value;
    }
    if ((prop_value->is_int() && leaf->is_fp()) ||
        (prop_value->is_fp() && leaf->is_int())) {
        //TBD:Can we safely discard the float<->integer conversion?
        //For now, we do NOT disgard CVT for conservative purpose.
        return prop_value;
    }
    if (prop_value->is_fp() && leaf->is_fp() &&
        prop_value->getType() != leaf->getType()) {
        //CASE: Different float point type conversion can not be skipped.
        //e.g: $1:f64 = cvt:fp64 ($2:fp32)
        //     ... = $1:64 #USE
        //Where $2:f32 can NOT be propagated to $1 because of type.
        return prop_value;
    }
    UINT tgt_size = prop_value->getTypeSize(m_tm);
    UINT src_size = leaf->getTypeSize(m_tm);
    if (tgt_size > src_size) {
        //CASE: If leaf is PR or NonPR memory reference, we should not skip the
        //CVT if tgt_size is bigger than leaf's Var size.
        //e.g:exec/array_alias2.c
        //  st:i64 b = cvt:i64( $8:i32 )
        //  printf("%llu", b:i64); #S1
        //where $8's Var size is i32.
        //After CP propagates $8 to #S1, the code will be:
        //  printf("%llu", $8:i64); #S2
        //However, $8's Var declaration's type is i32, the PR operation in #S2
        //will generate incorrect i64 memory read.
        return prop_value;
    }
    //The CVT can be skipped.
    return leaf;
}


bool CopyProp::computeUseSet(IR const* def_stmt, OUT IRSet & useset,
                             OUT bool & prssadu, OUT bool & mdssadu)
{
    ASSERT0(def_stmt->hasResult());
    prssadu = false;
    mdssadu = false;
    useset.clean();
    if (def_stmt->isPROp()) {
        if (usePRSSADU()) {
            SSAInfo * ssainfo = def_stmt->getSSAInfo();
            ASSERT0(ssainfo);
            //Record use_stmt in another vector to facilitate this function
            //if it is not in use-list any more after copy-propagation.
            SSAUseIter it;
            for (BSIdx u = SSA_uses(ssainfo).get_first(&it);
                 u != BS_UNDEF; u = SSA_uses(ssainfo).get_next(u, &it)) {
                IR const* use = m_rg->getIR(u);
                ASSERT0(use);
                useset.bunion(use->id());
            }
            prssadu = true;
            return true;
        }
        goto TRY_CLASSIC_DU;
    }
    ASSERT0(def_stmt->isMemRefNonPR());
    if (useMDSSADU()) {
        if (def_stmt->getExactRef() == nullptr && !allowInexactMD()) {
            //Do NOT progate value through inexact memory reference,
            //except PR.
            return false;
        }
        //Do NOT do collection crossing PHI.
        m_mdssamgr->collectUseSet(def_stmt, COLLECT_IMM_USE, &useset);
        mdssadu = true;
        return true;
    }
TRY_CLASSIC_DU:
    DUSet const* duset = def_stmt->readDUSet();
    if (duset != nullptr && !duset->is_empty()) {
        if (def_stmt->getExactRef() == nullptr && !allowInexactMD()) {
            //Do NOT progate value through inexact memory reference,
            //except PR.
            return false;
        }
        //Record use_stmt in another Set to facilitate this function
        //if it is not in use-list any more after copy-propagation.
        useset.copy((DefSBitSetCore&)*duset);
        return true;
    }
    return false;
}


//curit: the iter to current IR.
//nextit: the iter to next IR in 'bb'. It may be changed.
bool CopyProp::doPropUseSet(
    IRSet const& useset, IR const* def_stmt, IR const* prop_value,
    IRListIter curit, IRListIter * nextit, bool prssadu, bool mdssadu)
{
    bool change = false;
    IRSetIter it;
    ASSERT0(useset.allElemBeExp(m_rg));
    for (BSIdx i = useset.get_first(&it);
         i != BS_UNDEF; i = useset.get_next(i, &it)) {
        IR * use = m_rg->getIR(i); //the expression that will be replaced.
        ASSERT0(use);
        if (use->is_undef()) {
            //Whole IR tree has been replaced.
            continue;
        }
        IR const* new_prop_value = pickUpCandExp(
            prop_value, use, def_stmt, curit, prssadu, mdssadu);
        if (new_prop_value == nullptr) { continue; }
        IR const* nearest_def = xoc::findNearestDomDef(use, m_rg);
        if (nearest_def != def_stmt) { continue; }
        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpCP()) {
            dumpAct(this, def_stmt, new_prop_value, use);
        }
        if (!allowPropConstToPhiOpnd() && use->getStmt()->is_phi() &&
            prop_value->isConstExp()) {
            continue;
        }
        if (use->getStmt()->is_phi() && !isLegalToPhiOpnd(new_prop_value)) {
            continue;
        }
        if (use->is_id()) {
            change |= doPropForMDPhi(new_prop_value, use);
        } else {
            change |= doPropForNormalStmt(curit, nextit, new_prop_value,
                                          use, def_stmt->getBB());
        }
    } //end for each USE in SET
    return change;
}


//useset: for local used.
bool CopyProp::doPropStmt(IR * cpop, MOD IRSet & useset,
                          IRListIter curit, IRListIter * nextit)
{
    ASSERT0(cpop->is_stmt() && cpop->hasResult());
    if (cpop->getExactRef() == nullptr && !cpop->isWritePR() &&
        !allowInexactMD()) {
        //Do NOT progate value through inexact memory operation, except PR.
        //TODO:In aggressive mode, we anticipate propagating value through
        //inexact DEF.
        //e.g:a[i][j] = x
        //    ....... = a[i][j]
        //  x can be propagated.
        return false;
    }
    IR const* prop_value = getPropagatedValue(cpop);
    if (prop_value == nullptr || !canBeCandidate(prop_value)) {
        return false;
    }
    bool prssadu;
    bool mdssadu;
    if (!computeUseSet(cpop, useset, prssadu, mdssadu)) {
        return false;
    }
    return doPropUseSet(useset, cpop, prop_value,
                        curit, nextit, prssadu, mdssadu);
}


//useset: for local used.
bool CopyProp::doPropBB(IN IRBB * bb, MOD IRSet & useset)
{
    bool change = false;
    IRListIter curit;
    IRListIter nextit;
    for (BB_irlist(bb).get_head(&curit),
         nextit = curit; curit != nullptr; curit = nextit) {
        IR * stmt = curit->val();
        BB_irlist(bb).get_next(&nextit);
        if (!isCopyOP(stmt)) { continue; }
        change |= doPropStmt(stmt, useset, curit, &nextit);
    }
    return change;
}


static void refinement(Region * rg, OptCtx & oc)
{
    if (!g_do_refine) { return; }
    RefineCtx rf(&oc);
    Refine * refine = (Refine*)rg->getPassMgr()->registerPass(PASS_REFINE);
    refine->refineBBlist(rg->getBBList(), rf);
}


bool CopyProp::doPropBBInDomTreeOrder()
{
    IRBB * entry = m_cfg->getEntry();
    ASSERTN(entry && BB_is_entry(entry), ("Not find CFG entry"));
    bool changed = false;
    UINT count = 0;
    UINT const max_try = 50;
    xcom::DomTree domtree;
    m_cfg->genDomTree(domtree);
    do {
        PropVisitFunc vf(m_cfg, this, getSegMgr());
        PropVisit visit(entry, domtree, vf);
        visit.visit();
        if (!vf.isChanged()) { break; }
        changed = true;
        refinement(m_rg, *getOptCtx());
        ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg, *getOptCtx()));
        ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg, *getOptCtx()));
        ASSERT0(verifyMDDUChain(m_rg, *getOptCtx()));
        count++;
    } while (count < max_try);
    ASSERT0(count < max_try);
    return changed;
}


bool CopyProp::dump() const
{
    if (!m_rg->isLogMgrInit() || !g_dump_opt.isDumpCP()) { return false; }
    if (!g_dump_opt.isDumpAfterPass()) { return true; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    const_cast<CopyProp*>(this)->getActMgr().dump();
    bool succ = Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    return succ;
}


bool CopyProp::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    ASSERTN(oc.is_cfg_valid(), ("CopyProp need CFG info"));
    bool is_org_pr_du_chain_valid = oc.is_pr_du_chain_valid();
    bool is_org_nonpr_du_chain_valid = oc.is_nonpr_du_chain_valid();
    DUMMYUSE(is_org_pr_du_chain_valid);
    DUMMYUSE(is_org_nonpr_du_chain_valid);
    if (!oc.is_ref_valid()) { return false; }
    if (is_aggressive() && g_do_gvn) {
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_GVN, PASS_UNDEF);
    }
    m_oc = &oc;
    m_gvn = (GVN*)m_rg->getPassMgr()->queryPass(PASS_GVN);
    if (m_gvn != nullptr && !m_gvn->is_valid()) {
        if (allowInexactMD()) {
            //Aggressive CP need GVN.
            m_gvn->perform(oc);
        } else {
            return false;
        }
    }
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
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
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    bool changed = doPropBBInDomTreeOrder();
    END_TIMER(t, getPassName());
    if (!changed) {
        m_rg->getLogMgr()->cleanBuffer();
        return false;
    }
    dump();
    oc.setInvalidPass(PASS_EXPR_TAB);
    oc.setInvalidPass(PASS_AA);
    oc.setValidPass(PASS_MD_REF);
    ASSERT0(m_dumgr->verifyMDRef());
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    return true;
}
//END CopyProp

} //namespace xoc
