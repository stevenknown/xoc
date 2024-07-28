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

//Find the unique IV at IR tree.
class FindUniqueIVRef : public VisitIRTree {
    UINT m_ivref_cnt;
    IR const* m_last_ivref;
    IVR const* m_ivr;
    IV const* m_iv;
    LI<IRBB> const* m_li;
protected:
    virtual bool visitIR(IR const* ir) override
    {
        IV const* iv = nullptr;
        if (!m_ivr->isIV(m_li, ir, &iv)) { return true; }
        m_ivref_cnt++;
        m_last_ivref = ir;
        m_iv = iv;
        return true;
    }
public:
    FindUniqueIVRef(IR const* ir, IVR const* ivr, LI<IRBB> const* li)
    {
        ASSERT0(ivr);
        m_ivref_cnt = 0;
        m_last_ivref = nullptr;
        m_iv = nullptr;
        m_ivr = ivr;
        m_li = li;
        visit(ir);
    }
    IV const* getIV() const { return m_iv; }
    IR const* getUniqueIVRef() const
    { return m_ivref_cnt == 1 ? m_last_ivref : nullptr; }
};


//Return true if the IR tree only contain loop invariant except IV.
class CoeffIsInv : public VisitIRTree {
    bool m_is_inv;
    IR const* m_ivref;
    IVR const* m_ivr;
    Region const* m_rg;
    LI<IRBB> const* m_li;
    InvStmtList const* m_invstmtlst;
protected:
    virtual bool visitIR(IR const* ir) override
    {
        if (ir == m_ivref || !ir->isMemRef()) { return true; }
        if (!xoc::isLoopInvariant(ir, m_li, m_rg, m_invstmtlst, false)) {
            setTerminate();
            m_is_inv = false;
            return false;
        }
        return true;
    }
public:
    CoeffIsInv(IR const* root, IR const* ivref, IVR const* ivr,
               LI<IRBB> const* li, InvStmtList const* invstmtlst)
    {
        ASSERT0(ivref && ivr && li);
        m_ivref = ivref;
        m_ivr = ivr;
        m_li = li;
        m_rg = ivr->getRegion();
        m_is_inv = true;
        m_invstmtlst = invstmtlst;
        visit(root);
    }
    bool isInv() const { return m_is_inv; }
};


//
//START FindBIVByChainRec
//
static bool hasSameVar(IR const* ir1, IR const* ir2)
{
    ASSERT0(ir1 && ir2);
    MD const* md1 = ir1->getMustRef();
    MD const* md2 = ir2->getMustRef();
    return md1 != nullptr && md2 != nullptr &&
           md1->get_base() == md2->get_base();
}


class FindBIVByChainRec {
    COPY_CONSTRUCTOR(FindBIVByChainRec);
protected:
    MDSSAMgr const* m_mdssamgr;
    PRSSAMgr const* m_prssamgr;
    IRCFG const* m_cfg;
    TypeMgr * m_tm;
    Region * m_rg;
    LI<IRBB> const* m_li;
    IVR * m_ivr;
    ChainRecMgr * m_crmgr;
    IVRCtx const& m_ivrctx;
protected:
    //The class describes terminating information for computation.
    class TermInfo {
        COPY_CONSTRUCTOR_ASSIGN(TermInfo);
        //Propagate info bottom-up.
        bool m_is_meet_iv;

        //Propagate info top-down.
        bool m_is_term_mdphi;

        //Propagate info top-down and bottom-up.
        //Record the reduction stmt of IV.
        //Note reduction stmt indicates the DEF occrrence of IV in loop body.
        IR const* m_reduction_stmt;

        //Propagate info top-down and bottom-up.
        //Record the USE occurrence of IV.
        IR const* m_reduction_exp;

        //Propagate info top-down.
        union {
            void const* m_meta;
            MDPhi const* m_mdphi;
            CPhi const* m_prphi;
        } u;
    public:
        TermInfo(bool is_term_mdphi, void * termphi)
            : m_is_meet_iv(false), m_is_term_mdphi(is_term_mdphi),
              m_reduction_stmt(nullptr), m_reduction_exp(nullptr)
        { u.m_meta = termphi; }
        TermInfo(TermInfo const& src)
        { ::memset((void*)this, 0, sizeof(TermInfo)); copyTopDown(src); }

        //The function copy to-down informations of 'src' into current one.
        void copyTopDown(TermInfo const& src)
        {
            m_is_term_mdphi = src.m_is_term_mdphi;
            u.m_meta = src.u.m_meta;
            m_reduction_stmt = src.m_reduction_stmt;
            m_reduction_exp = src.m_reduction_exp;
        }

        MDPhi const* getTermMDPhi() const { return u.m_mdphi; }
        CPhi const* getTermPRPhi() const { return u.m_prphi; }
        IR const* getRedStmt() const { return m_reduction_stmt; }
        IR const* getRedExp() const { return m_reduction_exp; }

        bool isTermMDPhi() const { return m_is_term_mdphi; }
        bool isTermPRPhi() const { return !m_is_term_mdphi; }
        bool isMeetIV() const { return m_is_meet_iv; }

        void setMeetIV() { m_is_meet_iv = true; }
        void setRedStmt(IR const* s) { m_reduction_stmt = s; }
        void setRedExp(IR const* e) { m_reduction_exp = e; }

        //The function unify bottom-up informations of 'src' into current one.
        void unifyBottomUp(TermInfo const& src)
        {
            m_is_meet_iv |= src.m_is_meet_iv;
            ASSERT0(m_reduction_stmt == nullptr ||
                    src.m_reduction_stmt == nullptr ||
                    m_reduction_stmt == src.m_reduction_stmt);
            if (m_reduction_stmt == nullptr) {
                m_reduction_stmt = src.m_reduction_stmt;
            }
            ASSERT0(m_reduction_exp == nullptr ||
                    src.m_reduction_exp == nullptr ||
                    m_reduction_exp == src.m_reduction_exp);
            if (m_reduction_exp == nullptr) {
                m_reduction_exp = src.m_reduction_exp;
            }
        }
    };
protected:
    bool computeStepByMemOp(IR const* ir, OUT ChainRec & cr);
    bool computeStepByConst(IR const* ir, OUT ChainRec & cr);
    bool computeCR(IR const* ivocc, IR const* ir,
                   MOD TermInfo & ti, OUT ChainRec & cr);
    bool computeStepByIRTree(IR const* ivocc, IR const* ir,
                             MOD TermInfo & ti, OUT ChainRec & cr);
    bool combineStepByBinOp(IR_CODE code, ChainRec const& cr0,
                            ChainRec const& cr1, OUT ChainRec & rescr);
    bool combineTermInfoByBinOp(IR_CODE code, TermInfo const& ti0,
                                TermInfo const& ti1,
                                OUT TermInfo & resti) const;

    bool extractRedOpOpnd(MDPhi const* phi,
                          OUT IR const** init, OUT IR const** step);
    bool extractRedOpOpnd(IR const* ir,
                          OUT IR const** init, OUT IR const** step);

    bool findBIVImpl(IR const* init, IR const* step, MOD TermInfo & ti,
                     OUT ChainRec & cr, OUT IR const** initstmt);

    void genBIV(TermInfo const& ti, ChainRec const& cr, IR const* init) const;

    //Return true if collected information is sanitary enough to
    //generate a BIV.
    bool isSanityBIV(TermInfo const& ti) const;
    bool isTermCond(IR const* ir, MOD TermInfo & ti,
                    OUT bool & is_valid_cr);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    FindBIVByChainRec(Region * rg, MOD IVR * ivr, LI<IRBB> const* li,
                      IVRCtx const& ctx) : m_ivrctx(ctx)
    {
        m_mdssamgr = rg->getMDSSAMgr();
        m_prssamgr = rg->getPRSSAMgr();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_rg = rg;
        m_li = li;
        m_ivr = ivr;
        m_crmgr = &m_ivr->getChainRecMgr();
    }
    void findByMDSSA();
    void findByPRSSA();
    void find();
};


bool FindBIVByChainRec::extractRedOpOpnd(MDPhi const* phi, OUT IR const** init,
                                         OUT IR const** step)
{
    ASSERT0(phi->is_phi());
    ASSERT0(init && step);
    ASSERTN(phi->getOpndNum() == 2, ("phi operand more than 2"));
    IRBB const* pred0 = m_cfg->getNthPred(phi->getBB(), 0);
    IRBB const* pred1 = m_cfg->getNthPred(phi->getBB(), 1);
    IR const* op0 = phi->getOpnd(0);
    IR const* op1 = phi->getOpnd(1);
    if (!m_li->isInsideLoop(pred0->id()) && m_li->isInsideLoop(pred1->id())) {
        *init = op0;
        *step = op1;
        return true;
    }
    if (m_li->isInsideLoop(pred0->id()) && !m_li->isInsideLoop(pred1->id())) {
        *init = op1;
        *step = op0;
        return true;
    }
    return false;
}


bool FindBIVByChainRec::extractRedOpOpnd(IR const* ir, OUT IR const** init,
                                         OUT IR const** step)
{
    ASSERT0(ir->is_phi());
    ASSERT0(init && step);
    CPhi const* phi = (CPhi const*)ir;
    ASSERT0(phi->getOpndNum() == 2);
    IRBB const* pred0 = m_cfg->getNthPred(phi->getBB(), 0);
    IRBB const* pred1 = m_cfg->getNthPred(phi->getBB(), 1);
    IR const* op0 = phi->getOpnd(0);
    IR const* op1 = phi->getOpnd(1);
    if (!m_li->isInsideLoop(pred0->id()) && m_li->isInsideLoop(pred1->id())) {
        *init = op0;
        *step = op1;
        return true;
    }
    if (m_li->isInsideLoop(pred0->id()) && !m_li->isInsideLoop(pred1->id())) {
        *init = op1;
        *step = op0;
        return true;
    }
    return false;
}


bool FindBIVByChainRec::computeStepByMemOp(IR const* ir, OUT ChainRec & cr)
{
    ASSERT0(ir->is_exp() && ir->getMustRef());
    if (!cr.getStep().is_undef()) {
        //Step has already been assigned value.
        return false;
    }
    CR_step(&cr).setToVar(ir->getMustRef(), ir->getType());
    return true;
}


bool FindBIVByChainRec::computeStepByConst(IR const* ir, OUT ChainRec & cr)
{
    ASSERT0(ir->isConstExp() || ir->is_lda());
    if (!cr.getStep().is_undef()) {
        //Step has been assign value.
        return false;
    }
    return CR_step(&cr).extractFrom(ir);
}


bool FindBIVByChainRec::isTermCond(IR const* ir, MOD TermInfo & ti,
                                   OUT bool & is_valid_cr)
{
    ASSERT0(ir->isMemRef() && ir->is_exp());
    is_valid_cr = true;
    if (ir->isMemRefNonPR() && ti.isTermMDPhi()) {
        MDDef const* mddef = m_mdssamgr->findMustMDDef(ir);
        if (mddef == nullptr) {
            is_valid_cr = false;
            //Not DefUse SCC.
            return false;
        }
        if (!m_li->isInsideLoop(mddef->getBB()->id())) {
            is_valid_cr = false;
            //Out of loop scope.
            return false;
        }
        return mddef == ti.getTermMDPhi();
    }
    if (ir->isPROp() && ti.isTermPRPhi()) {
        ASSERT0(ir->getSSAInfo());
        IR const* def = ir->getSSAInfo()->getDef();
        if (def == nullptr) {
            is_valid_cr = false;
            //Not DefUse SCC.
            return false;
        }
        if (!m_li->isInsideLoop(def->getBB()->id())) {
            is_valid_cr = false;
            //Out of loop scope.
            return false;
        }
        return def == ti.getTermPRPhi();
    }
    return false;
}


bool FindBIVByChainRec::combineTermInfoByBinOp(IR_CODE code,
    TermInfo const& ti0, TermInfo const& ti1, OUT TermInfo & resti) const
{
    if (ti0.isMeetIV() & ti1.isMeetIV()) {
        //If both of ti0 and ti1 meet IV, means the coefficent of IV
        //may not be one. IV may not be linear.
        //e.g:k=iv+iv;
        return false;
    }
    resti.unifyBottomUp(ti0);
    resti.unifyBottomUp(ti1);
    return true;
}


bool FindBIVByChainRec::combineStepByBinOp(IR_CODE code, ChainRec const& cr0,
                                           ChainRec const& cr1,
                                           OUT ChainRec & rescr)
{
    switch (code) {
    case IR_ADD:
        IVVal::doAdd(cr0.getStep(), cr1.getStep(), CR_step(&rescr), *m_crmgr);
        return true;
    case IR_SUB:
        IVVal::doSub(cr0.getStep(), cr1.getStep(), CR_step(&rescr), *m_crmgr);
        return true;
    default: UNREACHABLE();
    }
    return false;
}


bool FindBIVByChainRec::computeStepByIRTree(IR const* ivocc, IR const* ir,
                                            MOD TermInfo & ti,
                                            OUT ChainRec & cr)
{
    switch (ir->getCode()) {
    case IR_CVT:
        return computeStepByIRTree(ivocc,
            ((CCvt*)const_cast<IR*>(ir))->getLeafExp(), ti, cr);
    case IR_CONST:
    case IR_LDA:
        return computeStepByConst(ir, cr);
    case IR_ADD:
    case IR_SUB: {
        ChainRec cr0;
        TermInfo ti0(ti);
        bool succ0 = computeStepByIRTree(ivocc, BIN_opnd0(ir), ti0, cr0);
        if (!succ0) { return false; }
        ChainRec cr1;
        TermInfo ti1(ti);
        bool succ1 = computeStepByIRTree(ivocc, BIN_opnd1(ir), ti1, cr1);
        if (!succ1) { return false; }
        if (!combineTermInfoByBinOp(ir->getCode(), ti0, ti1, ti)) {
            return false;
        }
        if (!combineStepByBinOp(ir->getCode(), cr0, cr1, cr)) { return false; }
        return true;
    }
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_INDIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR:
        if (ir->getMustRef() == nullptr) { return false; }
        return computeCR(ivocc, ir, ti, cr);
    default: return false;
    }
    return false;
}


bool FindBIVByChainRec::isSanityBIV(TermInfo const& ti) const
{
    return ti.getRedExp() != nullptr && ti.getRedStmt() != nullptr;
}


bool FindBIVByChainRec::computeCR(IR const* ivocc, IR const* ir,
                                  MOD TermInfo & ti, OUT ChainRec & cr)
{
    ASSERT0(ivocc->isMemRef() && ivocc->is_exp());
    ASSERT0(ir->isMemRef() && ir->is_exp());
    bool is_valid_cr = true;
    if (isTermCond(ir, ti, is_valid_cr)) {
        //Set step to be 0.
        CR_step(&cr).setToInt(0,
            m_crmgr->computeDefaultIntType(ir->getType()));
        ti.setMeetIV(); //inform caller IV found.
        ASSERT0(ti.getRedExp() == nullptr);
        ti.setRedExp(ir);

        //CASE:loop may do not have a reduce-operation, that means step value
        //is zero, but the situation satisfied the terminate rules applied
        //by current class.
        //e.g: given loop
        //  MDPhi3: MD23V2 <- MD23V1, MD23V2;
        //  ...use MD23V2 ...;
        //  goto loophead;
        //ASSERTN(ti.getRedStmt(), ("miss reduction-stmt"));
        return true;
    }
    if (!is_valid_cr) { return false; }
    if (xoc::isLoopInvariant(ir, m_li, m_rg, nullptr, true)) {
        if (ir->is_id()) {
            //There is no other real DEF to ir's MD. And ID should NOT be
            //regard as IV.
            return false;
        }
        //If ivocc is equal to 'ir', that means there are
        //multiple DEFs of ivocc in the loop, whereas ir should NOT be loop
        //invariant.
        ASSERT0(!hasSameVar(ivocc, ir));
        //ir is symbol constant.
        return computeStepByMemOp(ir, cr);
    }
    IR const* def = xoc::findKillingDef(ir, m_rg);
    if (def == nullptr) {
        //If ivocc is equal to 'ir', that means there are
        //multiple DEFs of ivocc in the loop. Otherwise, ir may have
        //diverse MayDefs.
        return false;
    }
    if (!m_li->isInsideLoop(def->getBB()->id())) {
        //ir is symbol constant.
        ASSERT0(!hasSameVar(ivocc, ir));
        return computeStepByMemOp(ir, cr);
    }
    ASSERT0(def);
    if (hasSameVar(ivocc, def)) {
        if (ti.getRedStmt() != nullptr) {
            //ivocc should not be IV, because its occurrence
            //has been defined multiple times in loop.
            return false;
        }
        ti.setRedStmt(def);
    }
    if (!def->hasRHS() || def->getRHS() == nullptr) {
        //Virtual OP may not have RHS.
        //Unknown DefUse path.
        return false;
    }
    return computeStepByIRTree(ivocc, def->getRHS(), ti, cr);
}


bool FindBIVByChainRec::findBIVImpl(IR const* init, IR const* step,
                                    MOD TermInfo & ti, OUT ChainRec & cr,
                                    OUT IR const** initstmt)
{
    ASSERT0(init && step);
    IVVal val;
    if (init->isMemOpnd()) {
        *initstmt = xoc::findKillingDef(init, m_rg);
        if (*initstmt == nullptr) { return false; }
        if (!m_ivr->computeInitVal(*initstmt, val)) { return false; }
    } else {
        ASSERT0(init->isConstExp() || init->is_lda());
        *initstmt = nullptr;
        val.extractFrom(init);
    }
    if (!step->isMemOpnd()) { return false; }
    CR_init(&cr) = val;
    bool is_cr = computeCR(step, step, ti, cr);
    cr.setCodeByStep();
    return is_cr;
}


void FindBIVByChainRec::genBIV(TermInfo const& ti, ChainRec const& cr,
                               IR const* initstmt) const
{
    BIV * biv = m_ivr->allocBIV();
    IV_li(biv) = m_li;
    ASSERT0(ti.getRedStmt());
    IV_reduction_stmt(biv) = ti.getRedStmt();

    //CASE: Reduction Expression may be not exist.
    //e.g:compile/ivr_noredexp.c, and here is a simplified code snippet:
    //  stpr $8 = ld gdd; #S1
    //  label L1;
    //  phi $13 = ($12,L3),($14,L4);
    //  phi $9 = ($8,L3),($10,L4); #S2
    //  falsebr (le $13, $7), L2;
    //  label L4;
    //  stpr $10 = 20.000000:f64; #S3
    //  goto L1;
    //  label L2;
    //  st gdd = $9;
    //In the example, $10 is the BIV, #S1, #S2 and #S3 together form a BIV use
    //case. Among them, #S1 represents the init-stmt, #S2 represents the
    //jion-point of init-stmt and step-stmt, #S3 represents the step-stmt.
    //#S3 is recognized as Reduction Stmt. However, the RHS of #S3 is CONST
    //operation, this will lead that the inference of Reduction Expression
    //will be terminated when the inferring-function meets the RHS CONST.
    //Thus Reduction Expression is NULL in this case.
    //Note even if Reduction Expression of $10 may be NULL, the ChainRec of
    //$10 is available.
    //ASSERT0(ti.getRedExp());
    IV_reduction_exp(biv) = ti.getRedExp();
    BIV_stepv(biv) = const_cast<ChainRec&>(cr).getStep();
    ASSERT0(cr.getCode() == IR_ADD || cr.getCode() == IR_SUB);
    BIV_initv(biv) = const_cast<ChainRec&>(cr).getInit();

    //CASE: the initv may be embedded in PHI's operand, thus there is no
    //DEF stmt corresponded to the value. e.g:ivr_init.gr
    //ASSERT0(initstmt && initstmt->is_stmt());
    BIV_init_stmt(biv) = initstmt;
    m_ivr->recordBIV(biv);
}


void FindBIVByChainRec::findByPRSSA()
{
    ASSERT0(usePRSSADU());
    IRBB * head = m_li->getLoopHead();
    ASSERT0(head);
    if (m_cfg->getPredsNum(head) != 2) { return; }
    BBIRListIter it;
    for (IR const* phi = head->getIRList().get_head(&it);
         phi != nullptr; phi = head->getIRList().get_next(&it)) {
        if (!phi->is_phi()) { break; }
        IR const* init = nullptr;
        IR const* step = nullptr;
        if (!extractRedOpOpnd(phi, &init, &step)) { continue; }
        TermInfo ti(false, (void*)phi);
        ChainRec cr;
        IR const* initstmt = nullptr;
        if (findBIVImpl(init, step, ti, cr, &initstmt)) {
            genBIV(ti, cr, initstmt);
        }
    }
}


void FindBIVByChainRec::findByMDSSA()
{
    ASSERT0(useMDSSADU());
    IRBB const* head = m_li->getLoopHead();
    ASSERT0(head);
    if (m_cfg->getPredsNum(head) > 2) {
        //Can not predicate which predecessor is the real initial value.
        return;
    }
    MDPhiList * philist = m_mdssamgr->getPhiList(head->id());
    if (philist == nullptr) { return; }
    for (MDPhiListIter pit = philist->get_head();
         pit != philist->end(); pit = philist->get_next(pit)) {
        MDPhi const* phi = pit->val();
        ASSERT0(phi && phi->is_phi());

        //Extract operand from PHI.
        IR const* init = nullptr;
        IR const* step = nullptr;
        if (!extractRedOpOpnd(phi, &init, &step)) { continue; }

        //Find BIV recursively.
        TermInfo ti(true, (void*)phi);
        ChainRec cr;
        IR const* initstmt = nullptr;
        if (findBIVImpl(init, step, ti, cr, &initstmt) && isSanityBIV(ti)) {
            genBIV(ti, cr, initstmt);
        }
    }
}


void FindBIVByChainRec::find()
{
    findByPRSSA();
    findByMDSSA();
}
//END FindBIVByChainRec


//
//START FindBIVByRedOp
//
//The class represents the computation of reduction operation.
class FindBIVByRedOp {
    COPY_CONSTRUCTOR(FindBIVByRedOp);
protected:
    //True if IVR pass only find BIV and DIV for exact MD.
    //Note if IR_ST, IR_LD, IR_PR, IR_STPR are ANY, the MD is inexact.
    BYTE m_is_only_handle_exact_md:1;
    MDSSAMgr const* m_mdssamgr;
    PRSSAMgr const* m_prssamgr;
    IRCFG const* m_cfg;
    Region * m_rg;
    LI<IRBB> const* m_li;
    IVR * m_ivr;
    ChainRecMgr * m_crmgr;
    IVRCtx const& m_ivrctx;
protected:
    //Extract BIV info from linear-representation.
    //Return true if the function extracted correct BIV information, and
    //record the BIV into 'biv', otherwise return false.
    bool extractBIV(IR const* def, IVLinearRep const& lr,
                    LI<IRBB> const* li, OUT BIV ** biv,
                    IVRCtx const& ctx);

    //Find initial value of IV through reduction-operation, if found the
    //value return true, otherwise return false.
    //ir: the reduce-exp.
    //val: used to record the initial value.
    //initstmt: record the initializing stmt.
    bool findInitValByRedOp(IR const* ir, LI<IRBB> const* li,
                            OUT IVVal & val, OUT IR const** initstmt,
                            IVRCtx const& ctx) const;
    bool findInitValByRedOp(LI<IRBB> const* li, OUT BIV * iv,
                            IVRCtx const& ctx) const;
    IR const* findInitStmtByPRSSA(IR const* redexp,
                                  LI<IRBB> const* li) const;
    IR const* findInitStmtByMDSSA(IR const* redexp,
                                  LI<IRBB> const* li) const;
    IR const* findInitStmtByClassicDU(IR const* redexp,
                                      LI<IRBB> const* li,
                                      MD const* ivoccmd) const;

    static bool isMDEqual(MD const* md, IR const* ir)
    { return md == ir->getRefMD(); }

    //Return true if ir is expression that represent the multiple of IV.
    bool isMultipleOfMD(LI<IRBB> const* li, IR const* ir,
                        MD const* selfmd, OUT IVLinearRep * linrep) const;

    //Return true if ir is linear-representation of MD.
    //e.g: if i is var, a*i is the linear-represetation of i.
    //li: given the LoopInfo.
    //ir: IR expression that to be analyzed.
    //selfmd: indicates the MD of self-modified variable.
    //linrep: record and output linear-representation if exist.
    bool isLinearRepOfMD(LI<IRBB> const* li, IR const* ir,
                         MD const* selfmd, OUT IVLinearRep * linrep) const;

    //Return true if ir indicates a memory reference that corresponds to
    //one of operands of 'phi', whereas ir is located in the BB which is
    //predecessor of BB of phi.
    bool isRelateToMDPhiOpnd(IR const* ir, MDPhi const* phi) const;

    //Return true if ir indicates a memory reference that corresponds to
    //one of operands of 'phi', whereas ir is located in the BB which is
    //predecessor of BB of phi.
    bool isRelateToPhiOpnd(IR const* ir, IR const* phi) const;

    //Return true if ir is reduction OP in MDSSA mode.
    bool isReductionOpByMDSSA(IR const* ir, LI<IRBB> const* li,
                              OUT IVLinearRep * lr) const;

    //Return true if ir is reduction OP in PRSSA mode.
    bool isReductionOpByPRSSA(IR const* ir, LI<IRBB> const* li,
                              OUT IVLinearRep * lr) const;

    //Return true if ir is reduction-operation.
    //NOTE: 1. the function will use PRSSA/MDSSA/Classic-DU to do analysis.
    //      2. the function does NOT check whether 'ir' is on the necessary
    //      path in the loop.
    //lr: record the linear-representation of 'ir' if exist.
    //set: record the DEF stmt set of reduction variable.
    bool isReductionOp(IR const* ir, LI<IRBB> const* li,
                       OUT IVLinearRep * lr) const;
    bool isOnNecessaryPath(UINT bbid, UINT back_start_bbid) const;

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    FindBIVByRedOp(Region * rg, MOD IVR * ivr, LI<IRBB> const* li,
                 IVRCtx const& ctx, bool only_handle_exact_md) :
        m_is_only_handle_exact_md(only_handle_exact_md), m_ivrctx(ctx)
    {
        //SSA may be disabled.
        m_mdssamgr = rg->getMDSSAMgr();
        m_prssamgr = rg->getPRSSAMgr();
        m_cfg = rg->getCFG();
        m_rg = rg;
        m_li = li;
        m_ivr = ivr;
        m_crmgr = &m_ivr->getChainRecMgr();
    }
    //Find Basic IV.
    void find();
};


IR const* FindBIVByRedOp::findInitStmtByMDSSA(IR const* redexp,
                                              LI<IRBB> const* li) const
{
    ASSERT0(redexp->isMemRefNonPR());
    MDDef const* def = m_mdssamgr->findMustMDDef(redexp);
    if (def == nullptr) { return nullptr; }
    ASSERTN(def->is_phi() && def->getBB() == li->getLoopHead(),
            ("def must be reduce-op"));
    UINT pos = 0;
    IR const* opnd = nullptr;
    for (opnd = MDPHI_opnd_list(def);
         opnd != nullptr; opnd = opnd->get_next(), pos++) {
        IRBB const* pred = m_cfg->getNthPred(def->getBB(), pos);
        if (!li->isInsideLoop(pred->id())) {
            break;
        }
    }
    ASSERT0(opnd);
    if (opnd->isConstExp() || opnd->is_lda()) { return opnd; }
    ASSERTN(opnd->is_id(), ("phi opnd must be PR"));
    //Note if opnd's def is NULL, means it is region livein PR.
    MDDef const* opnddef = m_mdssamgr->findKillingMDDef(opnd);
    if (opnddef == nullptr || opnddef->is_phi()) { return nullptr; }
    return opnddef->getOcc();
}


IR const* FindBIVByRedOp::findInitStmtByClassicDU(IR const* redexp,
                                                  LI<IRBB> const* li,
                                                  MD const* ivoccmd) const
{
    //For tmp used, it must be clean before return.
    //IRSet defset(const_cast<IVR*>(m_ivr)->getSegMgr());
    //xoc::collectDefSet(redexp, m_rg, &defset);
    //CASE: avoid collecting DefSet whenever possible.
    //IR const* domdef = xoc::findNearestDomDef(redexp, defset, m_rg);
    IR const* domdef = xoc::findNearestDomDef(redexp, m_rg);
    if (domdef == nullptr) { return nullptr; }
    MD const* emd = nullptr;
    if (m_is_only_handle_exact_md) {
        emd = domdef->getExactRef();
    } else {
        emd = domdef->getMustRef();
    }
    if (emd == nullptr || emd != ivoccmd) {
        return nullptr;
    }
    IRBB * dbb = domdef->getBB();
    if (dbb == li->getLoopHead() || !li->isInsideLoop(dbb->id())) {
        return domdef;
    }
    return nullptr;
}


IR const* FindBIVByRedOp::findInitStmtByPRSSA(IR const* redexp,
                                              LI<IRBB> const* li) const
{
    ASSERT0(redexp->isPROp());
    ASSERTN(redexp->getSSAInfo(), ("miss SSAInfo"));
    IR const* def = redexp->getSSAInfo()->getDef();
    if (def == nullptr) { return nullptr; }
    ASSERTN(def->is_phi() && def->getBB() == li->getLoopHead(),
            ("def must be reduce-op"));
    UINT pos = 0;
    IR const* opnd = nullptr;
    for (opnd = PHI_opnd_list(def);
         opnd != nullptr; opnd = opnd->get_next(), pos++) {
        IRBB const* pred = m_cfg->getNthPred(def->getBB(), pos);
        if (!li->isInsideLoop(pred->id())) {
            break;
        }
    }
    ASSERT0(opnd);
    if (opnd->isConstExp() || opnd->is_lda()) { return opnd; }
    ASSERTN(opnd->isPROp(), ("phi opnd must be PR"));
    ASSERTN(opnd->getSSAInfo(), ("miss SSAInfo"));
    //Note if opnd's def is NULL, means it is region livein PR.
    return opnd->getSSAInfo()->getDef();
}


bool FindBIVByRedOp::findInitValByRedOp(LI<IRBB> const* li, OUT BIV * iv,
                                        IVRCtx const& ctx) const
{
    IR const* redexp = iv->getRedExp();
    ASSERT0(redexp);
    ASSERT0(redexp->isMemRef() && redexp->getMustRef());
    ASSERT0(iv && iv->getRedStmt()->is_stmt());
    return findInitValByRedOp(redexp, li, BIV_initv(iv),
                              &BIV_init_stmt(iv), ctx);
}


//Find initialze value of IV, if found the value return true, otherwise
//return false.
//iv: used to record the initial value.
//initstmt: record the initializing stmt.
bool FindBIVByRedOp::findInitValByRedOp(IR const* ir, LI<IRBB> const* li,
                                        OUT IVVal & val,
                                        OUT IR const** initstmt,
                                        IVRCtx const& ctx) const
{
    IR const* def = nullptr;
    ASSERT0(ir->isMemRef());
    if (ir->isPROp() && usePRSSADU()) {
        def = findInitStmtByPRSSA(ir, li);
    } else if (useMDSSADU()) {
        def = findInitStmtByMDSSA(ir, li);
    } else {
        ASSERT0(ir->getMustRef());
        def = findInitStmtByClassicDU(ir, li, ir->getMustRef());
    }
    if (def != nullptr) {
        ASSERT0(initstmt);
        *initstmt = def;
        return m_ivr->computeInitVal(def, val);
    }
    ctx.dumpAct(
        "FIND_BIV_INIT_VAL:IR %s is BIV, however can not find "
        "its initial value",
        DumpIRName().dump(ir));
    return false;
}


bool FindBIVByRedOp::isReductionOpByMDSSA(IR const* ir, LI<IRBB> const* li,
                                          OUT IVLinearRep * lr) const
{
    ASSERT0(ir->is_stmt() && ir->isMemRefNonPR());
    ASSERT0(useMDSSADU());
    IR const* rhs = ir->getRHS();
    if (rhs == nullptr) { return false; }
    if (!m_ivr->isReductionOpCode(rhs->getCode())) { return false; }
    ASSERTN(rhs->isBinaryOp(), ("TODO"));
    IR const* op0 = BIN_opnd0(rhs);
    IR const* op1 = BIN_opnd1(rhs);
    IRBB const* head = li->getLoopHead();
    ASSERT0(head);
    MDDef const* kdef0 = nullptr;
    if (op0->isMemRefNonPR()) { kdef0 = m_mdssamgr->findMustMDDef(op0); }
    if (kdef0 != nullptr && kdef0->is_phi() && kdef0->getBB() == head &&
        isRelateToMDPhiOpnd(ir, (MDPhi const*)kdef0) &&
        m_ivr->canBeAddend(li, op1)) {
        if (lr != nullptr) {
            lr->coeff = nullptr;
            lr->var_exp = op0;
            lr->addend = op1;
            lr->addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    MDDef const* kdef1 = nullptr;
    if (op1->isMemRefNonPR()) { kdef1 = m_mdssamgr->findMustMDDef(op1); }
    if (kdef1 != nullptr && kdef1->is_phi() && kdef1->getBB() == head &&
        isRelateToMDPhiOpnd(ir, (MDPhi const*)kdef1) &&
        m_ivr->canBeAddend(li, op0)) {
        if (lr != nullptr) {
            lr->coeff = nullptr;
            lr->var_exp = op1;
            lr->addend = op0;
            lr->addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    return false;
}


bool FindBIVByRedOp::extractBIV(IR const* def, IVLinearRep const& lr,
                                LI<IRBB> const* li, OUT BIV ** biv,
                                IVRCtx const& ctx)
{
    ASSERT0(def->is_stmt());
    if (!lr.isValidAddendSign()) { return false; }
    if (!lr.hasVar()) { return false; }
    MD const* bivref = def->getMustRef();
    if (m_is_only_handle_exact_md &&
        (!bivref->is_exact() || lr.getVarExp()->getExactRef() == nullptr)) {
        ctx.dumpAct(
            "FIND_BIV:%s in LOOP%u does not have exact MD",
            DumpIRName().dump(def), m_li->id());
        return false;
    }
    IR const* addend = lr.addend;
    if (addend->is_int()) {
        ASSERT0(m_ivr->canBeAddend(li, addend));
    } else if (g_is_support_dynamic_type && addend->is_const()) {
        //TODO: support dynamic const type as the addend of ADD/SUB.
        ctx.dumpAct("FIND_BIV:Addend %s is ANY-type",
                    DumpIRName().dump(addend));
        return false;
    } else {
        return false;
    }
    ASSERT0(biv);
    *biv = m_ivr->allocBIV();
    IV_li(*biv) = m_li;
    IV_reduction_stmt(*biv) = const_cast<IR*>(def);
    IV_reduction_exp(*biv) = lr.getVarExp();
    BIV_stepv(*biv).extractFrom(addend);
    m_crmgr->refine(BIV_stepv(*biv));

    //Find and infer the initial value.
    //Note IV may not have an initial value, e.g:parameter.
    return findInitValByRedOp(m_li, *biv, m_ivrctx);
}


bool FindBIVByRedOp::isMultipleOfMD(LI<IRBB> const* li, IR const* ir,
                                    MD const* selfmd,
                                    OUT IVLinearRep * linrep) const
{
    if (ir->is_mul()) {
        //IVLinearRep of IV:a*i.
        if (isMDEqual(selfmd, BIN_opnd0(ir)) &&
            m_ivr->canBeCoeff(li, BIN_opnd1(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd1(ir);
                linrep->var_exp = BIN_opnd0(ir);
            }
            return true;
        }
        if (isMDEqual(selfmd, BIN_opnd1(ir)) &&
            m_ivr->canBeCoeff(li, BIN_opnd0(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd0(ir);
                linrep->var_exp = BIN_opnd1(ir);
            }
            return true;
        }
    }
    if (isMDEqual(selfmd, ir)) {
        //1*i is linear-rep of i.
        if (linrep != nullptr) {
            linrep->var_exp = ir;
        }
        return true;
    }
    return false;
}


bool FindBIVByRedOp::isLinearRepOfMD(LI<IRBB> const* li, IR const* ir,
                                     MD const* selfmd,
                                     OUT IVLinearRep * linrep) const
{
    ASSERT0(ir->is_exp());
    if (!ir->is_add() && !ir->is_sub()) {
        //May be linear-rep that form is: a*i.
        return isMultipleOfMD(li, ir, selfmd, linrep);
    }
    //May be linear-rep that form is: a*i+b or b+a*i.
    if (isMultipleOfMD(li, BIN_opnd0(ir), selfmd, linrep) &&
        m_ivr->canBeAddend(li, BIN_opnd1(ir))) {
        if (linrep != nullptr) {
            linrep->addend = BIN_opnd1(ir);
            linrep->addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    if (isMultipleOfMD(li, BIN_opnd1(ir), selfmd, linrep) &&
        m_ivr->canBeAddend(li, BIN_opnd0(ir))) {
        if (linrep != nullptr) {
            linrep->addend = BIN_opnd0(ir);
            linrep->addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    return false;
}


bool FindBIVByRedOp::isRelateToMDPhiOpnd(IR const* ir, MDPhi const* phi) const
{
    ASSERT0(ir && phi && ir->is_stmt() && phi->is_phi());
    ASSERT0(ir->getMustRef());
    bool is_pred;
    VexIdx idx = ((Graph*)m_cfg)->WhichPred(
        ir->getBB()->id(), phi->getBB()->getVex(), is_pred);
    if (!is_pred) { return false; }
    IR * phiopnd = phi->getOpnd(idx);
    ASSERT0(phiopnd);
    return phiopnd->getMustRef() == ir->getMustRef();
}


bool FindBIVByRedOp::isRelateToPhiOpnd(IR const* ir, IR const* phi) const
{
    ASSERT0(ir && phi && ir->is_stmt() && phi->is_phi());
    ASSERT0(ir->getPrno() != PRNO_UNDEF);
    bool is_pred;
    VexIdx idx = ((Graph*)m_cfg)->WhichPred(
        ir->getBB()->id(), phi->getBB()->getVex(), is_pred);
    if (!is_pred) { return false; }
    IR * phiopnd = ((CPhi*)phi)->getOpnd(idx);
    ASSERT0(phiopnd);
    return phiopnd->getPrno() == ir->getPrno();
}


bool FindBIVByRedOp::isReductionOpByPRSSA(IR const* ir, LI<IRBB> const* li,
                                          OUT IVLinearRep * lr) const
{
    ASSERT0(ir->is_stpr());
    ASSERT0(usePRSSADU());
    IR const* rhs = ir->getRHS();
    if (rhs == nullptr) { return false; }
    if (!m_ivr->isReductionOpCode(rhs->getCode())) { return false; }
    IR const* op0 = BIN_opnd0(rhs);
    IR const* op1 = BIN_opnd1(rhs);
    IRBB const* head = li->getLoopHead();
    ASSERT0(head);
    IR const* kdef0 = nullptr;
    if (op0->isMemRef()) { kdef0 = xoc::findKillingDef(op0, m_rg); }
    if (kdef0 != nullptr && kdef0->is_phi() && kdef0->getBB() == head &&
        isRelateToPhiOpnd(ir, kdef0) &&
        m_ivr->canBeAddend(li, op1)) {
        if (lr != nullptr) {
            lr->coeff = nullptr;
            lr->var_exp = op0;
            lr->addend = op1;
            lr->addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    IR const* kdef1 = nullptr;
    if (op1->isMemRef()) { kdef1 = xoc::findKillingDef(op1, m_rg); }
    if (kdef1 != nullptr && kdef1->is_phi() && kdef1->getBB() == head &&
        isRelateToPhiOpnd(ir, kdef1) &&
        m_ivr->canBeAddend(li, op0)) {
        if (lr != nullptr) {
            lr->coeff = nullptr;
            lr->var_exp = op1;
            lr->addend = op0;
            lr->addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    return false;
}


bool FindBIVByRedOp::isReductionOp(IR const* ir, LI<IRBB> const* li,
                                   OUT IVLinearRep * lr) const
{
    ASSERT0(ir->isMemRef());
    MD const* mustref = ir->getRefMD();
    if (mustref == nullptr ||
        (m_is_only_handle_exact_md && !mustref->is_exact())) {
        return false;
    }

    //Extract linear-representation of variable.
    if (ir->getRHS() == nullptr) {
        //Virtual OP may not have RHS.
        return false;
    }
    IVLinearRep linrep;
    if (ir->isPROp() && usePRSSADU()) {
        //Prefer judgement by SSA info.
        if (!isReductionOpByPRSSA(ir, li, &linrep)) {
            return false;
        }
    } else if (ir->isMemRefNonPR() && useMDSSADU()) {
        //Prefer judgement by SSA info.
        if (!isReductionOpByMDSSA(ir, li, &linrep)) {
            return false;
        }
    } else if (isLinearRepOfMD(li, ir->getRHS(), mustref, &linrep)) {
        //Judgement by classic DU.
        if (!xoc::hasUniqueDefInLoopForMustRef(linrep.getVarExp(), m_rg, li)) {
            //ir can NOT be defined more than twice inside loop.
            return false;
        }
    } else {
        return false;
    }
    //The coefficient and addend should be loop-invariant.
    ASSERT0(linrep.is_valid());
    if (!linrep.hasAddend() || linrep.getAddend()->is_const() ||
        xoc::isLoopInvariant(linrep.getAddend(), li, m_rg, nullptr, true)) {
        ; //nothing to do
    } else { return false; }

    if (!linrep.hasCoeff() || linrep.getCoeff()->is_const() ||
        xoc::isLoopInvariant(linrep.getCoeff(), li, m_rg, nullptr, true)) {
        ; //nothing to do
    } else { return false; }

    if (lr != nullptr) {
        lr->copy(linrep);
    }
    return true;
}


bool FindBIVByRedOp::isOnNecessaryPath(UINT bbid, UINT back_start_bbid) const
{
    //bb should dominate backedge start BB.
    //TODO: consider the convergence of IV reduction.
    //e.g:  LoopHeadBB
    //       |      |
    //       V      V
    //   i=i+1      i=i+1
    //   |__         ___|
    //      |        |
    //      V        V
    //      goto LoopHeadBB //backedge start BB
    return bbid == back_start_bbid || m_cfg->is_dom(bbid, back_start_bbid);
}


void FindBIVByRedOp::find()
{
    IRBB const* back_start_bb = xoc::findBackEdgeStartBB(m_li, m_cfg);
    if (back_start_bb == nullptr) {
        //TODO: support more sophisiticated CFG pattern.
        m_ivrctx.dumpAct(
            "FIND_BIV:LOOP%u does not have back-edge BB", m_li->id());
        return;
    }
    xcom::StrBuf tmp(8);
    for (BSIdx i = m_li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = m_li->getBodyBBSet()->get_next(i)) {
        //if ((UINT)i == headi) { continue; }
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && bb->getVex());
        if (!isOnNecessaryPath(i, back_start_bb->id())) {
            m_ivrctx.dumpAct("FIND_BIV:BB%u is not in necessary path", i);
            continue;
        }
        BBIRListIter it;
        for (IR * ir = bb->getIRList().get_head(&it);
             ir != nullptr; ir = bb->getIRList().get_next(&it)) {
            if (ir->isCallStmt()) {
                //TODO: callstmt may be intrinsic operation.
                continue;
            }
            if (!ir->hasRHS() || ir->isVirtualOp()) {
                //Do not handle virtual-op in reduction.
                continue;
            }
            //ir is the candidate.
            IVLinearRep lr;
            if (!isReductionOp(ir, m_li, &lr)) {
                m_ivrctx.dumpAct(
                    "FIND_BIV:%s in LOOP%u is not reduction-op",
                    dumpIRName(ir, tmp), m_li->id());
                continue;
            }
            BIV * biv = nullptr;
            if (!extractBIV(ir, lr, m_li, &biv, m_ivrctx)) {
                continue;
            }
            m_ivr->recordBIV(biv);
        }
    }
}
//END FindBIVByRedOp


//
//START FindDIV
//
class FindDIV {
    COPY_CONSTRUCTOR(FindDIV);
protected:
    MDSSAMgr const* m_mdssamgr;
    PRSSAMgr const* m_prssamgr;
    IRCFG const* m_cfg;
    Region * m_rg;
    LI<IRBB> const* m_li;
    IVR * m_ivr;
    IVRCtx const& m_ivrctx;
    ChainRecMgr * m_crmgr;
protected:
    //The function computes new init-value and step-value by substitute iv's
    //init and step for Variable in 'lr'.
    void computeCRByLinRep(IVLinearRep const& lr, OUT ChainRec & rescr,
                           OptCtx const& oc);

    bool findByCRRecur(IR const* ir, OUT ChainRec & cr);
    bool findByCR(IR const* ir);
    bool findByLinRep(IR const* ir);
    void findByStmt(IR * ir, ComputeMD2DefCnt const& md2defcnt,
                    OUT IRSet & set);

    OptCtx const* getOptCtx() const { return m_ivrctx.getOptCtx(); }

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    FindDIV(Region * rg, MOD IVR * ivr, LI<IRBB> const* li,
            IVRCtx const& ctx) : m_ivrctx(ctx)
    {
        m_mdssamgr = rg->getMDSSAMgr();
        m_prssamgr = rg->getPRSSAMgr();
        m_cfg = rg->getCFG();
        m_rg = rg;
        m_li = li;
        m_ivr = ivr;
        m_crmgr = &m_ivr->getChainRecMgr();
    }
    void find();
};


bool FindDIV::findByCRRecur(IR const* ir, OUT ChainRec & cr)
{
    ASSERT0(ir && ir->is_exp());
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_EXP:
    SWITCH_CASE_INDIRECT_MEM_EXP:
    SWITCH_CASE_READ_PR: {
        if (ir->getMustRef() == nullptr) { return false; }
        IV const* iv = nullptr;
        if (m_ivr->isIV(ir, &iv)) {
            cr.extractFrom(iv);
            return true;
        }
        IR const* def = xoc::findKillingDef(ir, m_rg);
        if (def == nullptr || def->is_phi() || def->isCallStmt()) {
            //TODO:call may be intrinsic stmt that can be reduction stmt.
            return false;
        }
        if (!m_li->isInsideLoop(def->getBB()->id())) {
            if (xoc::isLoopInvariant(ir, m_li, m_rg, nullptr, true)) {
                cr.extractFromLoopInvariant(ir, *m_crmgr);
                m_crmgr->refine(cr);
                return true;
            }
            return false;
        }
        if (def->getRHS() == nullptr) {
            //Virtual OP may not have RHS.
            return false;
        }
        return findByCRRecur(def->getRHS(), cr);
    }
    case IR_ADD:
    case IR_SUB:
    case IR_MUL: {
        ChainRec cr0;
        ChainRec cr1;
        if (!findByCRRecur(BIN_opnd0(ir), cr0)) { return false; }
        if (!findByCRRecur(BIN_opnd1(ir), cr1)) { return false; }
        if (ir->getCode() == IR_ADD) {
            return m_crmgr->doAdd(cr0, cr1, cr);
        } else if (ir->getCode() == IR_MUL) {
            return m_crmgr->doMul(cr0, cr1, cr);
        }
        ASSERT0(ir->getCode() == IR_SUB);
        return m_crmgr->doSub(cr0, cr1, cr);
    }
    case IR_CONST:
    case IR_LDA:
        cr.extractFromLoopInvariant(ir, *m_crmgr);
        m_crmgr->refine(cr);
        return true;
    default:
        //TODO: Support more DIV reduction operation.
        //ASSERTN(0, ("TODO"));
        return false;
    }
    return false;
}


bool FindDIV::findByCR(IR const* ir)
{
    ASSERT0(ir);
    ChainRec cr;
    xcom::StrBuf tmp(8);
    if (!findByCRRecur(ir, cr)) {
        m_ivrctx.dumpAct(
            "FIND_DIV:%s in LOOP%u is not chain-rec of any IV",
            dumpIRName(ir, tmp), m_li->id());
        return false;
    }
    ChainRec * pcr = m_ivr->allocChainRec();
    *pcr = cr;

    //ir is DIV's RHS.
    m_ivr->recordDIV(m_li, ir->getStmt(), pcr, *getOptCtx());
    return true;
}


void FindDIV::computeCRByLinRep(IVLinearRep const& lr, OUT ChainRec & rescr,
                                OptCtx const& oc)
{
    IV const* iv = lr.getIV();
    ASSERT0(iv);
    ChainRec tmp;
    ChainRec const* orgcr = nullptr;
    if (iv->is_biv()) {
        BIV const* biv = (BIV const*)iv;
        tmp.extractFrom(biv);
        orgcr = &tmp;
    } else {
        ASSERT0(iv->is_div());
        DIV const* div = (DIV const*)iv;
        ChainRec const* divcr = div->getChainRec();
        ASSERT0(divcr);
        orgcr = divcr;
    }
    rescr.computeByLinRep(lr, *orgcr, *m_crmgr);
    rescr.setCodeByIV(lr.getIV());
    m_crmgr->refine(rescr);
}


bool FindDIV::findByLinRep(IR const* ir)
{
    ASSERT0(ir && ir->is_exp());
    IVLinearRep lr;
    xcom::StrBuf tmp(8);
    if (!m_ivr->isLinearRepOfIV(m_li, ir, &lr)) {
        m_ivrctx.dumpAct(
            "FIND_DIV:%s in LOOP%u is not linear-rep of any IV",
            dumpIRName(ir, tmp), m_li->id());
        return false;
    }
    ASSERT0(lr.is_valid());
    ChainRec * cr = m_ivr->allocChainRec();
    computeCRByLinRep(lr, *cr, *getOptCtx());
    //ir is DIV's RHS.
    m_ivr->recordDIV(m_li, ir->getStmt(), cr, *getOptCtx());
    return true;
}


void FindDIV::findByStmt(IR * ir, ComputeMD2DefCnt const& md2defcnt,
                         OUT IRSet & set)
{
    xcom::StrBuf tmp(8);
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    SWITCH_CASE_INDIRECT_MEM_STMT:
    SWITCH_CASE_WRITE_ARRAY:
    case IR_STPR: {
        MD const* ref = ir->getExactRef();
        if (ref == nullptr || m_ivr->isBIV(m_li, ir, nullptr)) {
            return;
        }
        if (!md2defcnt.isUniqueDef(ir)) {
            m_ivrctx.dumpAct(
                "FIND_DIV:%s in LOOP%u does not have unique DEF",
                dumpIRName(ir, tmp), m_li->id());
            return;
        }
        IR * rhs = ir->getRHS();
        if (rhs == nullptr) {
            //Virtual OP may not have RHS.
            return;
        }
        if (findByLinRep(rhs)) { return; }
        if (findByCR(rhs)) { return; }
        return;
    }
    default:;
    }
}


void FindDIV::find()
{
    IVR::BIVList const* bivlst = m_ivr->getBIVList(m_li);
    if (bivlst == nullptr || bivlst->get_elem_count() == 0) { return; }
    IRSet set(m_ivr->getSegMgr()); //for tmp use
    ComputeMD2DefCnt md2defcnt(m_rg, m_li);
    md2defcnt.compute();
    for (BSIdx i = m_li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = m_li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && bb->getVex());
        IRListIter it;
        for (IR * ir = bb->getIRList().get_head(&it);
             ir != nullptr; ir = bb->getIRList().get_next(&it)) {
            findByStmt(ir, md2defcnt, set);
        }
    }
}
//END FindDIV


//
//START IVRCtx
//
void IVRCtx::dumpAct(CHAR const* format, ...) const
{
    if (m_act_mgr == nullptr) { return; }
    va_list args;
    va_start(args, format);
    m_act_mgr->dump_args(format, args);
    va_end(args);
}
//END IVRCtx


//
//START BIV
//
IR * BIV::genInitExp(IRMgr * irmgr) const
{
    ASSERT0(getInitValType());
    switch (getInitVal().getKind()) {
    case IVVal::VAL_IS_INT:
        return irmgr->buildImmInt(getInitValInt(), getInitValType());
    case IVVal::VAL_IS_FP:
        return irmgr->buildImmFP(getInitValFP(), getInitValType());
    case IVVal::VAL_IS_VAR:
        ASSERT0(getInitValMD() != nullptr);
        return irmgr->buildLoad(getInitValMD()->get_base(), getInitValType());
    default:
        ASSERTN(0, ("need to generate IR if needed"));
        return nullptr;
    }
    return nullptr;
}


IR * BIV::genStepExp(IRMgr * irmgr) const
{
    ASSERT0(getInitValType());
    Type const* ty = getInitValType();
    ASSERT0(isInc() || isDec());
    IR_CODE irc = isInc() ? IR_ADD : IR_SUB;
    return irmgr->buildBinaryOp(irc, ty,
        irmgr->buildLoad(getExpOccMD()->get_base(), ty),
        irmgr->buildImmInt(getStepValInt(), ty));
}


IR * BIV::genBoundExp(IVBoundInfo const& boundinfo, IVR const* ivr,
                      IRMgr * irmgr, Region * rg) const
{
    IR const* ivref = nullptr;
    IR const* bexp = nullptr;
    bool is_closed_range = false;
    ivr->extractIVBoundExpFromStmt(this, boundinfo.getBound(), &ivref, &bexp,
                                   &is_closed_range);
    ASSERT0(ivref && bexp);
    if (isInc()) {
        IR_CODE irc;
        if (is_closed_range) {
            irc = IR_LE;
        } else {
            irc = IR_LT;
        }
        return irmgr->buildCmp(irc, rg->dupIRTree(ivref), rg->dupIRTree(bexp));
    }
    ASSERT0(isDec());
    IR_CODE irc = is_closed_range ? IR_GE : IR_GT;
    return irmgr->buildCmp(irc, rg->dupIRTree(ivref), rg->dupIRTree(bexp));
}


void BIV::dump(Region const* rg) const
{
    BIV * iv = const_cast<BIV*>(this);
    ASSERT0(iv->getRedStmt());
    note(rg, "\nBIV(STMTOCC:MD%d,'%s')",
         iv->getStmtOccMD()->id(), iv->getStmtOccVarName());

    //See FindBIVByChainRec::genBIV() for details.
    //ASSERT0(iv->getRedExp());
    if (iv->getRedExp() != nullptr) {
        prt(rg, "(EXPOCC:MD%d,'%s')",
            iv->getExpOccMD()->id(), iv->getExpOccVarName());
    } else {
        prt(rg, "(EXPOCC:--)");
    }
    rg->getLogMgr()->incIndent(2);

    //Dump initval.
    note(rg, "\nINIT-VAL:");
    if (iv->hasInitVal()) {
        rg->getLogMgr()->incIndent(2);
        iv->getInitVal().dump(rg);
        rg->getLogMgr()->decIndent(2);
    }

    //Dump monotone direction.
    note(rg, "\nSTEP-VAL:");
    rg->getLogMgr()->incIndent(2);
    iv->getStepVal().dump(rg);
    rg->getLogMgr()->decIndent(2);

    //Dump BIV's reduction-stmt.
    note(rg, "\nREDUCTION-STMT:");
    ASSERT0(iv->getRedStmt());
    rg->getLogMgr()->incIndent(2);
    xoc::dumpIR(iv->getRedStmt(), rg, nullptr,
                DumpFlag::combineIRID(IR_DUMP_KID));
    rg->getLogMgr()->decIndent(2);

    //Dump BIV's occ-exp.
    note(rg, "\nREDUCTION-EXP:");
    //See FindBIVByChainRec::genBIV() for details.
    //ASSERT0(iv->getRedExp());
    if (iv->getRedExp() != nullptr) {
        rg->getLogMgr()->incIndent(2);
        xoc::dumpIR(iv->getRedExp(), rg, nullptr,
                    DumpFlag::combineIRID(IR_DUMP_KID));
        rg->getLogMgr()->decIndent(2);
    } else {
        prt(rg, "--");
    }

    //Dump BIV's init-stmt.
    if (iv->getInitStmt() != nullptr) {
        note(rg, "\nINIT-STMT:");
        rg->getLogMgr()->incIndent(2);
        xoc::dumpIR(iv->getInitStmt(), rg, nullptr,
                    DumpFlag::combineIRID(IR_DUMP_KID));
        rg->getLogMgr()->decIndent(2);
    }
    rg->getLogMgr()->decIndent(2);
}
//END BIV


//
//START DIV
//
void DIV::dump(Region const* rg) const
{
    DIV * iv = const_cast<DIV*>(this);
    ASSERT0(iv && iv->getRedStmt());
    ASSERT0(iv->getRedStmt());
    note(rg, "\nDIV(STMTOCC:MD%u,'%s')",
         iv->getStmtOccMD()->id(), iv->getStmtOccVarName());
    if (iv->getRedExp() != nullptr) {
        //Reduction exp may be NULL to DIV.
        prt(rg, "(EXPOCC:MD%u,'%s')",
            iv->getExpOccMD()->id(), iv->getExpOccVarName());
    }

    //Dump div occurrence.
    rg->getLogMgr()->incIndent(2);

    note(rg, "\nINIT-VAL:");
    rg->getLogMgr()->incIndent(2);
    iv->getInitVal().dump(rg);
    rg->getLogMgr()->decIndent(2);

    note(rg, "\nSTEP-VAL:");
    rg->getLogMgr()->incIndent(2);
    iv->getStepVal().dump(rg);
    rg->getLogMgr()->decIndent(2);

    note(rg, "\nREDUCTION-STMT:");
    rg->getLogMgr()->incIndent(2);
    xoc::dumpIR(iv->getRedStmt(), rg, nullptr,
                DumpFlag::combineIRID(IR_DUMP_KID));
    rg->getLogMgr()->decIndent(2);

    //Dump linear-representation by other BIV or DIV.
    note(rg, "\nLINEAR-REP:");
    rg->getLogMgr()->incIndent(2);
    iv->getChainRec()->dump(rg);
    rg->getLogMgr()->decIndent(2);

    rg->getLogMgr()->decIndent(2);
}


//Return the IV's increasing direction.
IV::INCDIR DIV::getIncDir() const
{
    ChainRec const* cr = getChainRec();
    ASSERT0(cr);
    if (cr->getCode() == IR_ADD || cr->getCode() == IR_MUL) {
        IVVal const& v = cr->getStep();
        if (v.is_int()) {
            return v.getInt() >= 0 ? IV::DIR_POS : IV::DIR_NEG;
        }
        if (v.is_fp()) {
            return v.getFP() >= 0.0 ? IV::DIR_POS : IV::DIR_NEG;
        }
        return IV::DIR_UNDEF; //do not know value in variable.
    }
    if (cr->getCode() == IR_SUB) {
        IVVal const& v = cr->getStep();
        if (v.is_int()) {
            ASSERT0(v.getInt() < 0);
            return IV::DIR_NEG;
        }
        if (v.is_fp()) {
            ASSERT0(v.getFP() < 0);
            return IV::DIR_NEG;
        }
        return IV::DIR_UNDEF; //do not know value in variable.
    }
    return IV::DIR_UNDEF; //do not know value in variable.
}
//END DIV


//
//START IVLinearRep
//
void IVLinearRep::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    UINT const ind = 2;
    if (getIV() != nullptr) {
        note(rg, "\n");
        prt(rg, "IV:");
        rg->getLogMgr()->incIndent(ind);
        getIV()->dump(rg);
        rg->getLogMgr()->decIndent(ind);
    } else { note(rg, "\nNOADDEND"); }
    LinearRep::dump(rg);
}
//END IVLinearRep


//
//START IVBoundInfo
//
void IVBoundInfo::dump(Region const* rg) const
{
    ASSERT0(rg);
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP IVBoundInfo --==");
    rg->getLogMgr()->incIndent(2);
    BIV const* iv = IVBI_iv(*this);
    iv->dump(rg);
    xcom::StrBuf lbuf(32);
    xcom::StrBuf buf(32);
    if (IVBI_is_tc_imm(*this)) {
        xoc::dumpHostInt(IVBI_tc_imm(*this), false, false, lbuf);
        buf.strcat("\nTRIPCOUNT IS IMM:%s", lbuf.buf);

        lbuf.clean();
        xoc::dumpHostInt(IVBI_tc_init_val_imm(*this), false, false, lbuf);
        buf.strcat("\nINIT_VAL IS IMM:%s", lbuf.buf);

        lbuf.clean();
        xoc::dumpHostInt(IVBI_tc_end_val_imm(*this), false, false, lbuf);
        buf.strcat("\nEND_VAL IS IMM:%s", lbuf.buf);
    } else {
        IR const* exp = IVBI_tc_exp(*this);
        ASSERT0(exp && exp->is_exp());
        buf.strcat("\nTRIPCOUNT IS EXP:");
        {
            DumpBufferSwitch buff(rg->getLogMgr());
            xoc::dumpIR(exp, rg);
            ASSERT0(rg->getLogMgr()->getBuffer());
            buf.strcat(rg->getLogMgr()->getBuffer()->getBuf());
            rg->getLogMgr()->cleanBuffer();
        }
    }
    lbuf.clean();
    xoc::dumpHostInt((HOST_INT)iv->getStepValInt(), false, false, lbuf);
    buf.strcat("\nSTEP IS IMM:%s", lbuf.buf);
    note(rg, buf.buf);
    rg->getLogMgr()->decIndent(2);
}
//END IVBoundInfo


//
//START IV
//
bool IV::isRefIV(MD const* ref) const
{
    ASSERT0(ref);
    //Exp occ may be NULL if IV is DIV.
    return ref == getStmtOccMD() || ref == getExpOccMD();
}


bool IV::isRefIV(IR const* ir) const
{
    MD const* irref = ir->getMustRef();
    if (irref == nullptr) { return false; }
    ASSERT0(getStmtOccMD() && getExpOccMD());
    return isRefIV(irref);
}


void IV::dump(Region const* rg) const
{
    if (is_biv()) { ((BIV const*)this)->dump(rg); return; }
    ASSERT0(is_div());
    ((DIV const*)this)->dump(rg);
}


CHAR const* IV::dump(VarMgr const* vm, OUT xcom::StrBuf & buf) const
{
    Var const* stmtvar = getStmtOccVar();
    Var const* expvar = getExpOccVar();
    ASSERT0(stmtvar && expvar);
    xcom::StrBuf tmp(32);
    buf.strcat("stmtocc:(%s), ", stmtvar->dump(tmp, vm));
    tmp.clean();
    buf.strcat("expocc:(%s)", expvar->dump(tmp, vm));
    return buf.buf;
}
//END IV


//
//START IVR
//
static bool isIVrecur(IVR const* ivr, LI<IRBB> const* li, IR const* ir,
                      IV const** iv)
{
    for (LI<IRBB> const* tli = li; tli != nullptr; tli = tli->get_next()) {
        if (isIVrecur(ivr, tli->getInnerList(), ir, iv)) { return true; }
        if (ivr->isIV(tli, ir, iv)) { return true; }
    }
    return false;
}


IVR::IVR(Region * rg) : Pass(rg), m_crmgr(rg, nullptr)
{
    ASSERT0(rg != nullptr);
    m_mdsys = rg->getMDSystem();
    m_du = rg->getDUMgr();
    m_irmgr = rg->getIRMgr();
    m_cfg = rg->getCFG();
    m_tm = rg->getTypeMgr();
    m_pool = smpoolCreate(sizeof(IV) * 4, MEM_COMM);
    m_sc_pool = smpoolCreate(sizeof(xcom::SC<IV*>) * 4, MEM_CONST_SIZE);
    m_is_only_handle_exact_md = true;
    m_is_aggressive = false;
    m_prssamgr = nullptr;
    m_mdssamgr = nullptr;
    m_gvn = nullptr;
    m_sbs_mgr = new DefMiscBitSetMgr();
    m_act_mgr = new ActMgr(rg);
}


//iv: IV info that will be modified
bool IVR::computeInitVal(IR const* ir, OUT IVVal & val) const
{
    IR const* v = nullptr;
    if (ir->isConstExp() || ir->is_lda()) {
        v = ir;
    } else if (ir->hasRHS()) {
        v = ir->getRHS();
        if (v == nullptr) {
            //Virtual OP may not have RHS.
            return false;
        }
    } else { return false; }
    ASSERT0(v);
    if (v->is_cvt()) {
        v = ((CCvt*)v)->getLeafExp();
    }
    return val.extractFrom(v);
}


void IVR::recordDIV(LI<IRBB> const* li, IR const* red, ChainRec const* cr,
                    OptCtx const& oc)
{
    ASSERT0(li && red && red->is_stmt());
    ASSERT0(cr && cr->getCode() != IR_UNDEF);
    DIV * x = allocDIV();
    IV_li(x) = li;
    IV_reduction_stmt(x) = red;
    DIV_chain_rec(x) = cr;
    DIV_initv(x) = cr->getInit();
    DIV_stepv(x) = cr->getStep();
    ASSERT0(x->isSanity());

    //Same occurrence may correspond to multiple LoopInfo.
    //e.g: c4.c
    //  while () {
    //    while () {
    //      ...
    //      i++;
    //    }
    //  }
    DIVList * ivlst = m_li2divlst.get(li->id());
    if (ivlst == nullptr) {
        ivlst = (DIVList*)xmalloc(sizeof(DIVList));
        ivlst->init(m_sc_pool);
        m_li2divlst.set(li->id(), ivlst);
    }
    ivlst->append_head(x);
}


//biv: MD reference of BIV
//li: LoopInfo related to 'biv'.
//red: the reduction stmt of IV.
//defset: the DEF set of RHS occurrence in 'red'.
//step: record the step constant of BIV.
//is_increment: record the monotonicity of IV.
void IVR::recordBIV(BIV * biv)
{
    ASSERT0(biv->isSanity());
    //Same occurrence may correspond to multiple LoopInfo.
    //e.g: c4.c
    //  while () {
    //    while () {
    //      ...
    //      i++;
    //    }
    //  }
    BIVList * ivlst = m_li2bivlst.get(biv->getLI()->id());
    if (ivlst == nullptr) {
        ivlst = (BIVList*)xmalloc(sizeof(BIVList));
        ivlst->init(m_sc_pool);
        m_li2bivlst.set(biv->getLI()->id(), ivlst);
    }
    ivlst->append_head(biv);
}


bool IVR::isBIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const
{
    MD const* ref = ir->getRefMD();
    if (ref == nullptr || !ref->is_exact()) { return false; }

    BIVList const* bivlst = m_li2bivlst.get(li->id());
    if (bivlst == nullptr) { return false; }

    for (BIVListIter it = bivlst->get_head();
         it != bivlst->end(); it = bivlst->get_next(it)) {
        BIV const* tiv = it->val();
        ASSERT0(tiv);
        if (tiv->isRefIV(ref)) {
            if (iv != nullptr) {
                *iv = tiv;
            }
            return true;
        }
    }
    return false;
}


bool IVR::isDIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const
{
    MD const* ref = ir->getRefMD();
    if (ref == nullptr || !ref->is_exact()) { return false; }

    DIVList * divlst = m_li2divlst.get(li->id());
    if (divlst == nullptr) { return false; }

    for (DIVListIter it = divlst->get_head();
         it != divlst->end(); it = divlst->get_next(it)) {
        DIV const* tiv = it->val();
        ASSERT0(tiv);
        if (tiv->isRefIV(ref)) {
            if (iv != nullptr) {
                *iv = tiv;
            }
            return true;
        }
    }
    return false;
}


bool IVR::isIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const
{
    if (isBIV(li, ir, iv)) { return true; }
    return isDIV(li, ir, iv);
}


bool IVR::isIV(IR const* ir, OUT IV const** iv) const
{
    MD const* ref = ir->getRefMD();
    if (ref != nullptr && ref->is_exact()) {
        return isIVrecur(this, m_cfg->getLoopInfo(), ir, iv);
    }
    return false;
}


//Return true if ir is coefficent of linear-representation.
bool IVR::canBeCoeff(LI<IRBB> const* li, IR const* ir) const
{
    return xoc::isLoopInvariant(ir, li, m_rg, nullptr, true);
}


//Return true if ir is addend of linear-representation.
bool IVR::canBeAddend(LI<IRBB> const* li, IR const* ir) const
{
    return xoc::isLoopInvariant(ir, li, m_rg, nullptr, true);
}


bool IVR::isRelaxLinearRepOfIV(LI<IRBB> const* li, IR const* ir,
                               InvStmtList const* invstmtlst, OptCtx const* oc,
                               OUT IVLinearRep * linrep,
                               MOD LinearRepMgr & lrmgr) const
{
    BIVList const* bivlst = getBIVList(li);
    if (bivlst == nullptr) { return false; }
    for (BIVListIter it = bivlst->get_head();
         it != bivlst->end(); it = bivlst->get_next(it)) {
        BIV const* tiv = it->val();
        ASSERT0(tiv);
        LRInferCtx ctx;
        ctx.is_transitive = true;
        ctx.li = li;
        ASSERTN(tiv->getInitIVVar(), ("miss IV var"));
        if (lrmgr.inferAndConstructLinearRep(ir, tiv->getInitIVVar(),
                                             *linrep, ctx)) {
            IVLR_iv(linrep) = tiv;
            return true;
        }
    }
    return false;
}


bool IVR::isMultipleOfIV(LI<IRBB> const* li, IR const* ir,
                         OUT IVLinearRep * linrep) const
{
    ASSERT0(ir->is_exp());
    IV const* iv = nullptr;
    if (isIV(li, ir, &iv)) {
        if (linrep != nullptr) {
            linrep->coeff = m_irmgr->buildImmInt(1,
                m_crmgr.computeDefaultIntType(ir->getType()));
            linrep->var_exp = ir;
            IVLR_iv(linrep) = iv;
        }
        return true;
    }
    if (ir->is_mul()) {
        if (isIV(li, BIN_opnd0(ir), &iv) &&
            canBeCoeff(li, BIN_opnd1(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd1(ir);
                linrep->var_exp = BIN_opnd0(ir);
                IVLR_iv(linrep) = iv;
            }
            return true;
        }
        if (isIV(li, BIN_opnd1(ir), &iv) &&
            canBeCoeff(li, BIN_opnd0(ir))) {
            if (linrep != nullptr) {
                linrep->coeff = BIN_opnd0(ir);
                linrep->var_exp = BIN_opnd1(ir);
                IVLR_iv(linrep) = iv;
            }
            return true;
        }
    }
    return false;
}


bool IVR::isLinearRepOfIV(LI<IRBB> const* li, IR const* ir,
                          OUT IVLinearRep * linrep) const
{
    ASSERT0(ir->is_exp());
    if (!ir->is_add() && !ir->is_sub()) {
        //May be linear-rep that form is: a*i.
        return isMultipleOfIV(li, ir, linrep);
    }
    if (isMultipleOfIV(li, BIN_opnd0(ir), linrep) &&
        canBeAddend(li, BIN_opnd1(ir))) {
        //May be linear-rep that form is: a*i+b.
        if (linrep != nullptr) {
            ASSERT0(linrep->getIV());
            linrep->addend = BIN_opnd1(ir);
            linrep->addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    if (isMultipleOfIV(li, BIN_opnd1(ir), linrep) &&
        canBeAddend(li, BIN_opnd0(ir))) {
        //May be linear-rep that form is: b+a*i.
        if (linrep != nullptr) {
            ASSERT0(linrep->getIV());
            linrep->addend = BIN_opnd0(ir);
            linrep->addend_sign = ADDEND_SIGN_POS;
        }
        return true;
    }
    return false;
}


void IVR::dump_recur(LI<IRBB> const* li, UINT indent) const
{
    Region const* rg = getRegion();
    INT orgindent = rg->getLogMgr()->getIndent();
    while (li != nullptr) {
        note(getRegion(), "\n\n==-- LOOP INFO --==\n");
        //Dump loopinfo.
        //for (UINT i = 0; i < indent; i++) { prt(getRegion(), " "); }
        note(getRegion(), "\nLI%d:BB%d", li->id(), li->getLoopHead()->id());
        prt(getRegion(), ",BODY:");
        for (BSIdx i = li->getBodyBBSet()->get_first();
             i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
            prt(getRegion(), "%d,", i);
        }

        BIVList const* bivlst = m_li2bivlst.get(li->id());
        if (bivlst != nullptr) {
            for (BIVListIter it = bivlst->get_head();
                 it != bivlst->end(); it = bivlst->get_next(it)) {
                BIV const* iv = it->val();
                ASSERT0(iv);
                iv->dump(getRegion());
            }
        }

        DIVList * divlst = m_li2divlst.get(li->id());
        if (divlst != nullptr) {
            for (DIVListIter it = divlst->get_head();
                 it != divlst->end(); it = divlst->get_next(it)) {
                DIV const* iv = it->val();
                iv->dump(getRegion());
            }
        }

        dump_recur(LI_inner_list(li), indent + 2);
        li = LI_next(li);
    }
    rg->getLogMgr()->setIndent(orgindent);
}


//Dump IVR info for loop.
bool IVR::dump() const
{
    if (!m_rg->isLogMgrInit()) { return true; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    dump_recur(m_cfg->getLoopInfo(), 0);
    return Pass::dump();
}


void IVR::clean()
{
    for (VecIdx i = 0; i <= m_li2bivlst.get_last_idx(); i++) {
        BIVList * ivlst = m_li2bivlst.get(i);
        if (ivlst == nullptr) { continue; }
        ivlst->clean();
    }
    for (VecIdx i = 0; i <= m_li2divlst.get_last_idx(); i++) {
        DIVList * ivlst = m_li2divlst.get(i);
        if (ivlst == nullptr) { continue; }
        ivlst->clean();
    }
    getActMgr()->clean();
}


IR const* IVR::findBIVBoundStmt(LI<IRBB> const* li, OUT BIV const** biv,
                                IVRCtx const& ivrctx) const
{
    ASSERT0(li);
    BIVList const* bivlst = getBIVList(li);
    if (bivlst == nullptr) {
        ivrctx.dumpAct("no BIV found");
        //There is no biv.
        return nullptr;
    }
    //Loop head can not change the control flow of loop.
    //The loop head must be inside the loop body.
    xcom::List<UINT> endlst;
    li->findAllLoopEndBB(m_cfg, endlst);
    if (endlst.get_elem_count() > 1) {
        ivrctx.dumpAct("there are multiple exit BBs in LoopInfo %u", li->id());
        //Multiple exit BB.
        return nullptr;
    }
    UINT bbid = endlst.get_head();
    ASSERT0(bbid != BBID_UNDEF);
    IRBB * bb = m_cfg->getBB(bbid);
    ASSERT0(bb);
    IR * bstmt = bb->getLastIR();
    ASSERTN(bstmt && bstmt->isConditionalBr(), ("weird exit BB"));
    for (BIVListIter it = bivlst->get_head();
         it != bivlst->end(); it = bivlst->get_next(it)) {
        BIV const* iv = it->val();
        ASSERT0(iv);
        if (isBIVBoundStmt(iv, li, bstmt)) {
            if (biv != nullptr) { *biv = iv; }
            return bstmt;
        }
    }
    if (biv != nullptr) { *biv = nullptr; }
    xcom::StrBuf tmp(8);
    ivrctx.dumpAct("%s in BB%u is not end-bound-stmt for any IV",
                   dumpIRName(bstmt, tmp), bbid);
    return nullptr;
}


bool IVR::extractIVBoundExpFromStmt(IV const* iv, IR const* stmt,
                                    OUT IR const** ivref,
                                    OUT IR const** bexp,
                                    OUT bool * is_closed_range) const
{
    ASSERT0(iv && stmt && stmt->is_stmt());
    if (!stmt->isConditionalBr()) { return false; }
    IR const* compare_exp = BR_det(stmt);
    if (!compare_exp->is_relation()) { return false; }
    switch (compare_exp->getCode()) {
    case IR_LE:
    case IR_GE:
        *is_closed_range = true;
        break;
    default:
        *is_closed_range = false;
    }
    if (!extractIVBoundExp(iv, compare_exp, ivref, bexp)) {
        return false;
    }
    ASSERT0(ivref && bexp);
    return true;
}


bool IVR::extractIVBoundExp(IV const* iv, IR const* compare_exp,
                            OUT IR const** ivref,
                            OUT IR const** bexp) const
{
    ASSERT0(compare_exp && compare_exp->is_relation());
    IR const* opnd0 = BIN_opnd0(compare_exp);
    IR const* opnd1 = BIN_opnd1(compare_exp);
    ASSERT0(ivref && bexp);
    *ivref = nullptr;
    *bexp = nullptr;
    if (opnd0->isMemOpnd()) {
        MD const* mustref = opnd0->getMustRef();
        if (mustref == nullptr) { return false; }
        if (iv->isRefIV(mustref)) {
            *ivref = opnd0;
            *bexp = opnd1;
            return true;
        }
    }
    if (opnd1->isMemOpnd()) {
        MD const* mustref = opnd1->getMustRef();
        if (mustref == nullptr) { return false; }
        if (iv->isRefIV(mustref)) {
            *ivref = opnd1;
            *bexp = opnd0;
            return true;
        }
    }
    return false;
}


bool IVR::isBIVBoundExp(BIV const* biv, IR const* compare_exp,
                        IR const* ivref) const
{
    ASSERT0(biv && compare_exp->is_exp() && ivref->is_exp() &&
            ivref->getParent() == compare_exp);
    if (!compare_exp->is_relation()) { return false; }
    IR const* opnd0 = BIN_opnd0(compare_exp);
    IR const* opnd1 = BIN_opnd1(compare_exp);
    if (biv->isInc()) {
        //Basic IV is in incremental order.
        if (ivref == opnd0 &&
           (compare_exp->is_lt() || compare_exp->is_le() ||
            compare_exp->is_ne())) {
            //compare_exp may be: iv<UB, iv<=UB, or iv!=UB.
            return true;
        }
        if (ivref == opnd1 &&
           (compare_exp->is_gt() || compare_exp->is_ge() ||
            compare_exp->is_ne())) {
            //compare_exp may be: UB>iv, UB>=iv, or UB!=iv.
            return true;
        }
        return false;
    }
    //Basic IV is in decremental order.
    if (ivref == opnd0 &&
        (compare_exp->is_gt() || compare_exp->is_ge() ||
         compare_exp->is_ne())) {
         //compare_exp may be: iv>UB, iv>=UB, or iv!=UB.
         return true;
    }
    if (ivref == opnd1 &&
        (compare_exp->is_lt() || compare_exp->is_le() ||
         compare_exp->is_ne())) {
         //compare_exp may be: UB<iv, UB<=iv, or UB!=iv.
         return true;
    }
    return false;
}


bool IVR::isBIVBoundStmt(BIV const* biv, LI<IRBB> const* li,
                         IR const* stmt) const
{
    IR const* ivref = nullptr;
    IR const* bexp = nullptr;
    bool is_closed_range = false;
    if (!extractIVBoundExpFromStmt(biv, stmt, &ivref, &bexp,
                                   &is_closed_range)) {
        return false;
    }
    ASSERT0(ivref && bexp);
    DUMMYUSE(is_closed_range);
    ASSERT0(stmt->isConditionalBr());
    IR const* compare_exp = BR_det(stmt);
    if (stmt->is_falsebr() &&
        xoc::isBranchTargetOutSideLoop(li, m_cfg, stmt) &&
        isBIVBoundExp(biv, compare_exp, ivref)) {
        return true;
    }
    if (stmt->is_truebr() &&
        xoc::isBranchTargetOutSideLoop(li, m_cfg, stmt)) {
        IR * newir = IR::invertIRCode(const_cast<IR*>(compare_exp), m_rg);
        ASSERT0_DUMMYUSE(newir == compare_exp);
        bool res = isBIVBoundExp(biv, compare_exp, ivref);
        IR::invertIRCode(const_cast<IR*>(compare_exp), m_rg);
        return res;
    }
    return false;
}


bool IVR::computeConstValOfExp(IR const* exp, OUT HOST_INT & val) const
{
    ASSERT0(exp && exp->is_exp());
    if (exp->isConstInt()) {
        //Bound expression is constant.
        val = CONST_int_val(exp);
        return true;
    }
    if (useGVN()) {
        //Determine whether the expression has constant value.
        ASSERT0(m_gvn);
        VN const* vn = m_gvn->getVN(exp);
        if (vn == nullptr) {
            //We know nothing about value of exp.
            return false;
        }
        if (vn->getType() != VN_INT) {
            return false;
        }
        val = VN_int_val(vn);
        return true;
    }
    return false;
}


bool IVR::computeConstInitValOfBIV(BIV const* biv, OUT HOST_INT & val) const
{
    if (biv->isInitVar() && useGVN()) {
        ASSERT0(m_gvn);
        IR const* initval = biv->getInitStmt();
        ASSERT0(initval && initval->hasRHS() && initval->getRHS());
        VN const* vn = m_gvn->getVN(initval->getRHS());
        if (vn == nullptr) {
            //We know nothing about initial value of IV.
            return false;
        }
        if (vn->getType() != VN_INT) {
            return false;
        }
        val = VN_int_val(vn);
        return true;
    }
    if (biv->isInitConstInt()) {
        val = biv->getInitValInt();
        return true;
    }
    return false;
}


void IVR::setAggressive(bool doit)
{
    m_is_aggressive = doit;
    m_gvn = (GVN*)m_rg->getPassMgr()->registerPass(PASS_GVN);
}


bool IVR::computeIVBound(LI<IRBB> const* li, OUT IVBoundInfo & bi,
                         MOD IVRCtx & ivrctx) const
{
    if (computeConstIVBound(li, bi, ivrctx)) { return true; }
    if (computeExpIVBound(li, bi, ivrctx)) { return true; }
    return false;
}


IR * IVR::genTripCountExp(BIV const* biv, IR const* initexp,
                          IR const* boundexp, HOST_INT step,
                          MOD IVRCtx & ivrctx) const
{
    IR const* iv_initval = nullptr;
    IR const* iv_endval = nullptr;
    if (biv->isInc()) {
        iv_initval = initexp;
        iv_endval = boundexp;
    } else {
        ASSERT0(biv->isDec());
        iv_initval = boundexp;
        iv_endval = initexp;
    }
    DUMMYUSE(iv_initval);
    Type const* ty = iv_endval->getType();
    IR * tripcount_exp = m_irmgr->buildBinaryOp(IR_DIV, ty,
        m_irmgr->buildBinaryOp(IR_SUB, ty,
                               m_rg->dupIRTree(boundexp),
                               m_rg->dupIRTree(initexp)),
        m_irmgr->buildImmInt(step, ty));
    Refine * refine = (Refine*)m_rg->getPassMgr()->queryPass(PASS_REFINE);
    if (refine != nullptr) {
        //Perform peephole optimization to ir.
        //Return updated ir if optimization performed.
        RefineCtx rc(ivrctx.getOptCtx());
        bool change;
        tripcount_exp = refine->refineIRUntilUnchange(tripcount_exp,
                                                      change, rc);
    }
    return tripcount_exp;
}


bool IVR::computeExpIVBound(LI<IRBB> const* li, OUT IVBoundInfo & bi,
                            MOD IVRCtx & ivrctx) const
{
    BIV const* biv = nullptr;
    IR const* ubstmt = findBIVBoundStmt(li, &biv, ivrctx);
    if (ubstmt == nullptr) {
        //The shape of 'li' is denormal, we almost can not figure the result
        //out in a high probability.
        return false;
    }
    ASSERT0(ubstmt->is_stmt());
    ASSERT0(biv);
    HOST_INT step = (HOST_INT)biv->getStepValInt();
    if (step == 0) {
        //Step is 0, may cause infinit loop.
        return false;
    }
    ASSERT0(biv);
    if (!biv->hasInitVal()) { return false; }

    //Compute the finish value of IV.
    IR const* ivref = nullptr;
    IR const* bexp = nullptr;
    bool is_closed_range = false;
    extractIVBoundExpFromStmt(biv, ubstmt, &ivref, &bexp, &is_closed_range);
    ASSERT0(ivref && bexp);
    IR const* initexp = biv->isInitExp() ?
        biv->getInitExp() : biv->genInitExp(m_irmgr);
    ASSERT0(initexp);
    IR * tripcount_exp = genTripCountExp(biv, initexp, bexp, step, ivrctx);
    ASSERT0(tripcount_exp);
    IVBI_is_tc_imm(bi) = false;
    IVBI_tc_exp(bi) = tripcount_exp;
    IVBI_iv(bi) = biv;
    IVBI_iv_end_bound_stmt(bi) = ubstmt;
    IVBI_is_end_bound_closed(bi) = is_closed_range;
    return true;
}


bool IVR::computeConstIVBound(LI<IRBB> const* li, OUT IVBoundInfo & bi,
                              MOD IVRCtx & ivrctx) const
{
    BIV const* biv = nullptr;
    IR const* ubstmt = findBIVBoundStmt(li, &biv, ivrctx);
    if (ubstmt == nullptr) {
        //The shape of 'li' is denormal, we almost can not figure the result
        //out in a high probability.
        return false;
    }
    ASSERT0(biv);
    if (!biv->hasInitVal()) { return false; }

    //Compute the initial value of IV.
    HOST_INT iv_initval = 0;
    if (!computeConstInitValOfBIV(biv, iv_initval)) { return false; }

    //Compute the finish value of IV.
    IR const* ivref = nullptr;
    IR const* bexp = nullptr;
    bool is_closed_range = false;
    extractIVBoundExpFromStmt(biv, ubstmt, &ivref, &bexp, &is_closed_range);
    ASSERT0(ivref && bexp);
    HOST_INT iv_endval = 0;
    //Whether the end bound is constant value.
    if (!computeConstValOfExp(bexp, iv_endval)) { return false; }
    HOST_INT step = (HOST_INT)biv->getStepValInt();
    if (step == 0) {
        //Step is 0, may cause infinit loop.
        return false;
    }
    HOST_INT trip_count;
    if (biv->isInc()) {
        trip_count = (iv_endval - iv_initval) / step;
    } else {
        ASSERT0(biv->isDec());
        trip_count = (iv_initval - iv_endval) / step;
    }
    if (trip_count < 0) { return false; }
    IVBI_is_tc_imm(bi) = true;
    IVBI_tc_imm(bi) = trip_count;
    IVBI_iv(bi) = biv;
    IVBI_tc_init_val_imm(bi) = iv_initval;
    IVBI_tc_end_val_imm(bi) = iv_endval;
    IVBI_iv_end_bound_stmt(bi) = ubstmt;
    IVBI_is_end_bound_closed(bi) = is_closed_range;
    return true;
}


bool IVR::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }

    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    m_crmgr.setOptCtx(&oc);

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
    clean();
    m_rg->getPassMgr()->checkValidAndRecompute(
        &oc, PASS_DU_REF, PASS_DOM, PASS_LOOP_INFO, PASS_RPO, PASS_UNDEF);
    m_du = (DUMgr*)m_rg->getPassMgr()->queryPass(PASS_DU_MGR);
    if (is_aggressive() && !useGVN()) {
        m_gvn = (GVN*)m_rg->getPassMgr()->registerPass(PASS_GVN);
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_GVN, PASS_UNDEF);
        ASSERT0(useGVN());
    }
    LI<IRBB> const* li = m_cfg->getLoopInfo();
    if (li == nullptr) {
        END_TIMER(t, getPassName());
        set_valid(true);
        return false;
    }
    CLoopInfoIter it;
    IVRCtx ctx(&oc, getActMgr());
    for (LI<IRBB> const* tli = iterInitLoopInfoC(li, it);
         tli != nullptr; tli = iterNextLoopInfoC(it)) {
        bool try_classic_prdu = true;
        bool try_classic_nonprdu = true;
        if (usePRSSADU()) {
            FindBIVByChainRec f(m_rg, this, tli, ctx);
            f.findByPRSSA();
            try_classic_prdu = false;
        }
        if (useMDSSADU()) {
            FindBIVByChainRec f(m_rg, this, tli, ctx);
            f.findByMDSSA();
            try_classic_nonprdu = false;
        }
        if (try_classic_prdu || try_classic_nonprdu) {
            FindBIVByRedOp f(m_rg, this, tli, ctx, m_is_only_handle_exact_md);
            f.find();
        }
        FindDIV fdiv(m_rg, this, tli, ctx);
        fdiv.find();
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpIVR()) {
        getActMgr()->dump();
        dump();
    }
    set_valid(true);
    END_TIMER(t, getPassName());
    return false;
}
//END IVR

} //namespace xoc
