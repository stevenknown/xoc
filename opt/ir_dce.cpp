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

static void dumpRemovedIneffectIR(
    IR const* ir, IRBB const* bb, DeadCodeElim const* dce)
{
    ASSERT0(dce);
    if (ir == nullptr || !dce->getRegion()->isLogMgrInit() ||
        !g_dump_opt.isDumpDCE()) { return; }
    xcom::StrBuf buf(32);
    dce->getRegion()->getLogMgr()->incIndent(2);
    xoc::dumpIRToBuf(ir, dce->getRegion(), buf,
                     DumpFlag::combineIRID(IR_DUMP_DEF|IR_DUMP_KID));
    dce->getRegion()->getLogMgr()->decIndent(2);
    ActMgr & am = const_cast<DeadCodeElim*>(dce)->getActMgr();
    am.dump("removed ineffect IR:%s", buf.getBuf());
}


//
//START DCECtx
//
void DCECtx::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit() || !g_dump_opt.isDumpDCE()) {
        return;
    }
    note(rg, "\n==-- DUMP DCECtx --==");
    note(rg, "\nEFFECT BB:");
    StmtSetIter it;
    for (BSIdx i = m_effect_bb.get_first(&it);
         i != BS_UNDEF; i = m_effect_bb.get_next(i, &it)) {
        IRBB const* bb = rg->getBBMgr()->getBB(i);
        ASSERT0(bb);
        prt(rg, "%u ", bb->id());
    }
    note(rg, "\nEFFECT STMT:");
    rg->getLogMgr()->incIndent(2);
    StmtSetIter it2;
    for (BSIdx i = m_effect_stmt.get_first(&it2);
         i != BS_UNDEF; i = m_effect_stmt.get_next(i, &it2)) {
        IR const* ir = rg->getIR(i);
        ASSERT0(ir);
        note(rg, "\n%s ", DumpIRName().dump(ir));
    }
    rg->getLogMgr()->decIndent(2);
    note(rg, "\nEXCLUDE STMT:");
    rg->getLogMgr()->incIndent(2);
    StmtSetIter it3;
    for (BSIdx i = m_exclude_stmt.get_first(&it3);
         i != BS_UNDEF; i = m_exclude_stmt.get_next(i, &it3)) {
        IR const* ir = rg->getIR(i);
        ASSERT0(ir);
        note(rg, "\n%s ", DumpIRName().dump(ir));
    }
    rg->getLogMgr()->decIndent(2);
}
//END DCECtx


//
//START DeadCodeElim
//
void DeadCodeElim::setEffectStmt(
    IR const* stmt, bool set_bb_effect, MOD ConstIRList * act_ir_lst,
    DCECtx & dcectx)
{
    ASSERT0(!dcectx.isExcluded(stmt));
    ASSERT0(stmt && stmt->is_stmt());
    act_ir_lst->append_tail(stmt);
    dcectx.addEffectStmt(stmt);
    if (set_bb_effect) {
        ASSERT0(stmt->getBB());
        dcectx.addEffectBB(stmt->getBB());
    }
}


bool DeadCodeElim::dumpBeforePass() const
{
    if (!getRegion()->isLogMgrInit() || !g_dump_opt.isDumpDCE()) {
        return false;
    }
    START_TIMER_FMT(t, ("DUMP BEFORE %s", getPassName()));
    note(getRegion(), "\n==---- DUMP BEFORE %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    getRegion()->getLogMgr()->incIndent(2);
    dumpBBList(m_rg->getBBList(), m_rg);
    if (usePRSSADU()) {
        m_prssamgr->dumpBrief();
    }
    if (useMDSSADU()) {
        m_mdssamgr->dump();
    }
    getRegion()->getLogMgr()->decIndent(2);
    END_TIMER_FMT(t, ("DUMP BEFORE %s", getPassName()));
    return true;
}


void DeadCodeElim::dump(DCECtx const& dcectx) const
{
    if (!getRegion()->isLogMgrInit() || !g_dump_opt.isDumpDCE()) { return; }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    const_cast<DeadCodeElim*>(this)->getActMgr().dump();
    dcectx.dump(m_rg);
    Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
}


//CFG optimization may invalid CDG.
void DeadCodeElim::checkValidAndRecomputeCDG(DCECtx const& dcectx)
{
    if (m_is_elim_cfs) {
        ASSERT0(dcectx.getOptCtx());
        m_rg->getPassMgr()->checkValidAndRecompute(
            dcectx.getOptCtx(), PASS_PDOM, PASS_CDG, PASS_RPO, PASS_UNDEF);
        m_cdg = (CDG*)m_rg->getPassMgr()->queryPass(PASS_CDG);
        ASSERT0(m_cdg && m_cdg->is_valid());
        return;
    }
    m_cdg = nullptr;
}


bool DeadCodeElim::checkEffectStmt(IR const* ir)
{
    ASSERT0(ir->is_stmt());
    class VF {
    public:
        bool checkStmt(IR const* ir, OUT bool & is_terminate)
        {
            ASSERT0(ir->is_stmt());
            ASSERT0(ir->isMemRef());
            MD const* mustref = ir->getMustRef();
            if (mustref != nullptr) {
                if (dce->is_effect_write(mustref->get_base())) {
                    is_terminate = true;
                    is_effect = true;
                    return false;
                }
                //No need to check MayRef if MustRef is not NULL.
                //Keep walking trough the IR tree.
                return true;
            }
            MDSet const* mayref = ir->getMayRef();
            if (mayref == nullptr) { return true; }
            MDSetIter iter;
            for (BSIdx i = mayref->get_first(&iter);
                 i != BS_UNDEF; i = mayref->get_next(i, &iter)) {
                MD * md = mdsys->getMD(i);
                ASSERT0(md);
                if (dce->is_effect_write(md->get_base())) {
                    is_terminate = true;
                    is_effect = true;
                    return false;
                }
            }
            //Keep walking trough the IR tree.
            return true;
        }
        bool checkExp(IR const* ir, OUT bool & is_terminate)
        {
            ASSERT0(ir->is_exp());
            ASSERT0(ir->isMemRef());
            //Check if expression using volatile variable.
            //e.g: volatile int g = 0;
            //    while(g); # The stmt has effect.
            MD const* mustref = ir->getMustRef();
            if (mustref != nullptr) {
                if (dce->is_effect_read(mustref->get_base())) {
                    is_terminate = true;
                    is_effect = true;
                    return false;
                }
                //No need to check MayRef if MustRef is not NULL.
                //Keep walking trough the IR tree.
                return true;
            }
            MDSet const* mayref = ir->getMayRef();
            if (mayref == nullptr) { return true; }
            MDSetIter iter;
            for (BSIdx i = mayref->get_first(&iter);
                 i != BS_UNDEF; i = mayref->get_next(i, &iter)) {
                MD * md = mdsys->getMD(i);
                ASSERT0(md != nullptr);
                if (dce->is_effect_read(md->get_base())) {
                    is_terminate = true;
                    is_effect = true;
                    return false;
                }
            }
            //Keep walking trough the IR tree.
            return true;
        }
        bool visitIR(IR const* ir, OUT bool & is_terminate)
        {
            if (dce->hasSideEffect(ir)) {
                is_terminate = true;
                is_effect = true;
                return false;
            }
            if (!ir->isMemRef()) {
                //Keep walking trough the IR tree.
                return true;
            }
            if (!ir->isMemRefNonPR()) {
                //Keep walking trough the IR tree.
                return true;
            }
            if (!dce->isCheckMDRef()) {
                //NonPR operation will always be treated as effect.
                is_terminate = true;
                is_effect = true;
                return false;
            }
            if (ir->is_stmt()) { return checkStmt(ir, is_terminate); }
            return checkExp(ir, is_terminate);
        }
    public:
        bool is_effect;
        MDSystem const* mdsys;
        DeadCodeElim const* dce;
    public:
        VF() { is_effect = false; dce = nullptr; mdsys = nullptr; }
    };
    class IterTree : public VisitIRTree<VF> {
    public: IterTree(VF & vf) : VisitIRTree<VF>(vf) {}
    };
    VF vf;
    vf.dce = this;
    vf.mdsys = getMDSystem();
    IterTree it(vf);
    it.visit(ir);
    return vf.is_effect;
}


//Return true if ir is effect.
bool DeadCodeElim::checkCall(IR const* ir) const
{
    ASSERT0(ir->isCallStmt());
    return !ir->isReadOnly() || IR_has_sideeffect(ir) || IR_no_move(ir);
}


void DeadCodeElim::markEffectIRForRegion(
    IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx)
{
    ASSERT0(ir->is_region());
    ASSERT0(!dcectx.isExcluded(ir));
    //The redundant of Region should be processed in IPA
    //rather than DCE, thus regard Region always effect.
    setEffectStmt(ir, true, &work_list, dcectx);
}


void DeadCodeElim::markEffectIRForReturn(
    IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx)
{
    ASSERT0(ir->is_return());
    ASSERT0(!dcectx.isExcluded(ir));
    //Do NOT set exit-bb to be effect.
    //That will generate redundant control-flow dependence.
    //CASE:
    //    IF (...)
    //        ...
    //    ENDIF
    //    RETURN //EXIT BB
    //IF clause stmt is redundant code.
    setEffectStmt(ir, true, &work_list, dcectx);
}


void DeadCodeElim::markEffectIRForCall(
    IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx)
{
    ASSERT0(ir->isCallStmt());
    ASSERT0(!dcectx.isExcluded(ir));
    if (checkCall(ir)) {
        setEffectStmt(ir, true, &work_list, dcectx);
    }
}


void DeadCodeElim::markEffectIRForBranch(
    IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx)
{
    ASSERT0(ir->isBranch());
    ASSERT0(!dcectx.isExcluded(ir));
    if (!m_is_elim_cfs) {
        setEffectStmt(ir, true, &work_list, dcectx);
    }
}


void DeadCodeElim::markEffectIRForStmt(
    IR const* ir, MOD ConstIRList & work_list, MOD DCECtx & dcectx)
{
    if (dcectx.isExcluded(ir)) { return; }
    switch (ir->getCode()) {
    case IR_REGION:
        markEffectIRForRegion(ir, work_list, dcectx);
        return;
    case IR_RETURN:
        markEffectIRForReturn(ir, work_list, dcectx);
        return;
    SWITCH_CASE_CALL:
        markEffectIRForCall(ir, work_list, dcectx);
        return;
    SWITCH_CASE_BRANCH_OP:
        markEffectIRForBranch(ir, work_list, dcectx);
        return;
    default:
        if (!checkEffectStmt(ir)) { return; }
        setEffectStmt(ir, true, &work_list, dcectx);
    }
}


void DeadCodeElim::markEffectIRForBBIRList(
    IRBB const* bb, MOD ConstIRList & work_list, MOD DCECtx & dcectx)
{
    BBIRListIter irit;
    IRBB * pbb = const_cast<IRBB*>(bb);
    for (IR const* ir = pbb->getIRList().get_head(&irit);
         ir != nullptr; ir = pbb->getIRList().get_next(&irit)) {
        markEffectIRForStmt(ir, work_list, dcectx);
    }
}


//Mark effect IRs.
void DeadCodeElim::markEffectIR(MOD ConstIRList & work_list,
                                MOD DCECtx & dcectx)
{
    List<IRBB*> * bbl = m_rg->getBBList();
    BBListIter ct;
    for (IRBB * bb = bbl->get_head(&ct);
         bb != nullptr; bb = bbl->get_next(&ct)) {
        markEffectIRForBBIRList(bb, work_list, dcectx);
    }
}


//Return true if there are effect BBs that controlled by ir's BB.
//ir: stmt.
bool DeadCodeElim::find_effect_kid_condbr(IR const* ir,
                                          MOD DCECtx & dcectx) const
{
    ASSERT0(m_cfg);
    IRBB const* bb = ir->getBB();
    ASSERT0(bb);
    ASSERT0(ir->isConditionalBr() || ir->isMultiConditionalBr());
    //Mark the sucsessor on CFG and related stmts to be effect if needed.
    for (xcom::EdgeC const* ec = bb->getVex()->getOutList();
         ec != nullptr; ec = ec->get_next()) {
        IRBB * succ = m_cfg->getBB(ec->getToId());
        ASSERT0(succ != nullptr);
        if (!m_cfg->isControlPred(bb, succ)) { continue; }
        for (IR * r = BB_irlist(succ).get_head();
             r != nullptr; r = BB_irlist(succ).get_next()) {
            if (dcectx.isEffectStmt(r)) {
                return true;
            }
        }
    }
    return false;
}


//Return true if there are effect BBs that controlled by ir's BB.
//ir: stmt.
bool DeadCodeElim::find_effect_kid_uncondbr(IR const* ir,
                                            MOD DCECtx & dcectx) const
{
    ASSERT0(m_cfg);
    IRBB const* bb = ir->getBB();
    ASSERT0(bb);
    ASSERT0(ir->isUnconditionalBr());
    //Check if unconditional branch stmt is effect.
    //Note you can not mark unconditional branch directly. However it is
    //effect only when at least one effect path goes through the stmt.
    //CASE: If S1 or S2 is effect, bb is effect.
    //       p1    p2
    //     /   |  /  |
    //    V    V V   V
    //   S1    bb    S2
    for (xcom::EdgeC const* ecp = bb->getVex()->getInList();
         ecp != nullptr; ecp = ecp->get_next()) {
        //Check if predecessing control-BB have effect stmt.
        IRBB const* pred = m_cfg->getBB(ecp->getFromId());
        if (!m_cfg->isControlPred(pred, bb)) { continue; }

        bool control_bb_has_effect_stmt = false;
        if (dcectx.isEffectBB(pred)) {
            control_bb_has_effect_stmt = true;
        } else {
            BBIRListIter irit;
            for (IR * r = BB_irlist(pred).get_head(&irit);
                 r != nullptr; r = BB_irlist(pred).get_next(&irit)) {
                if (dcectx.isEffectStmt(r)) {
                    control_bb_has_effect_stmt = true;
                    break;
                }
            }
        }
        if (!control_bb_has_effect_stmt) { continue; }

        //Check the path-relation between the successor of 'pred' and 'bb'.
        for (xcom::EdgeC const* ecs = pred->getVex()->getOutList();
             ecs != nullptr; ecs = ecs->get_next()) {
            IRBB * succ = m_cfg->getBB(ecs->getToId());
            ASSERTN(succ, ("BB%d does not belong to CFG", ecs->getToId()));
            if (!m_cfg->isControlPred(bb, succ)) { continue; }

            for (IR * r = BB_irlist(succ).get_head();
                 r != nullptr; r = BB_irlist(succ).get_next()) {
                if (dcectx.isEffectStmt(r)) {
                    return true;
                }
            }
        }
    }

    //Check if unconditional branch stmt is effect.
    //CASE: If S1 is effect, bb is effect.
    //    bb
    //    |
    //    V
    //    S1
    for (xcom::EdgeC const* ec = bb->getVex()->getOutList();
         ec != nullptr; ec = ec->get_next()) {
        IRBB const* succ = m_cfg->getBB(ec->getToId());
        ASSERT0(succ);
        if (dcectx.isEffectBB(succ)) { return true; }
        if (succ == bb) { continue; }

        BBIRListIter irit;
        for (IR const* r = BB_irlist(succ).get_head(&irit);
             r != nullptr; r = BB_irlist(succ).get_next(&irit)) {
            if (dcectx.isEffectStmt(r)) {
                return true;
            }
        }
    }
    return false;
}


//Return true if there are effect BBs that controlled by ir's BB.
//ir: stmt.
bool DeadCodeElim::find_effect_kid(IR const* ir, MOD DCECtx & dcectx) const
{
    if (ir->isConditionalBr() || ir->isMultiConditionalBr()) {
        return find_effect_kid_condbr(ir, dcectx);
    }
    if (ir->isUnconditionalBr()) {
        return find_effect_kid_uncondbr(ir, dcectx);
    }
    UNREACHABLE();
    return false;
}


bool DeadCodeElim::markControlPredAndStmt(
    IRBB const* bb, OUT ConstIRList & act_ir_lst, MOD DCECtx & dcectx)
{
    ASSERT0(m_cdg);
    UINT bbid = bb->id();
    bool change = false;
    Vertex const* cdgv = m_cdg->getVertex(bbid);
    ASSERTN(cdgv, ("CDG is unmatch to CFG"));
    for (xcom::EdgeC const* ec = cdgv->getInList();
         ec != nullptr; ec = ec->get_next()) {
        IRBB const* pred = m_cfg->getBB(ec->getFromId());
        ASSERT0(pred);
        if (!dcectx.isEffectBB(pred)) {
            dcectx.addEffectBB(pred);
            change = true;
        }

        //Mark the last IR of controlling BB to be effect.
        IR * last_ir = const_cast<IRBB*>(pred)->getLastIR(); //last IR of BB.
        if (last_ir == nullptr || !last_ir->isBranch()) {
            //CASE: the last ir of controlling-predecessor may NOT be branch in
            //some denormal CFG. e.g:
            //  int g;
            //  int n;
            //  int foo(int b)
            //  {
            //      int i;
            //      switch (n) {
            //      case 1: b = 10; break;
            //      case 3: b = 13; break;
            //      }
            //      n = 10;
            //      L1:
            //      i = 20;
            //      L2:
            //      g = b;
            //      goto L1;
            //      return g + n;
            //  }
            continue;
        }
        if (dcectx.isExcluded(last_ir)) {
            continue;
        }
        if (!dcectx.isEffectStmt(last_ir)) {
            setEffectStmt(last_ir, false, &act_ir_lst, dcectx);
            change = true;
        }
    }
    return change;
}


//The function marks possible predecessor in CFG to be effect BB, e.g back-edge.
bool DeadCodeElim::markCFGPred(IRBB const* bb, MOD DCECtx & dcectx)
{
    bool change = false;
    ASSERT0(dcectx.getOptCtx()->is_rpo_valid());
    ASSERT0(bb->rpo() > RPO_UNDEF);
    for (xcom::EdgeC const* ec = bb->getVex()->getInList();
         ec != nullptr; ec = ec->get_next()) {
        UINT predid = ec->getFromId();
        if (dcectx.isEffectBB(predid)) { continue; }

        IRBB const* pred = m_cfg->getBB(predid);
        ASSERT0(pred->rpo() > RPO_UNDEF);
        if (pred->rpo() > (INT)bb->rpo()) {
            //Predecessor is lexicographical back of bb.
            dcectx.addEffectBB(pred);
            change = true;
        }
    }
    return change;
}


bool DeadCodeElim::tryMarkUnconditionalBranch(
    IRBB const* bb, MOD ConstIRList & act_ir_lst, MOD DCECtx & dcectx)
{
    IR * last_ir = const_cast<IRBB*>(bb)->getLastIR(); //last IR of BB.
    if (last_ir == nullptr) { return false; }
    if (!last_ir->isUnconditionalBr()) { return false; }
    if (dcectx.isExcluded(last_ir)) { return false; }
    if (dcectx.isEffectStmt(last_ir)) { return false; }
    if (find_effect_kid(last_ir, dcectx)) {
        //TBD: Does effect_kid of last_ir have to be considered?
        setEffectStmt(last_ir, true, &act_ir_lst, dcectx);
        return true;
    }
    return false;
}


bool DeadCodeElim::tryMarkBranch(
    IRBB const* bb, OUT ConstIRList & act_ir_lst, MOD DCECtx & dcectx)
{
    //If last ir of effect BB is branch operation, try to mark effect stmts
    //that controlled by current bb.
    IR * last_ir = const_cast<IRBB*>(bb)->getLastIR(); //last IR of BB.
    if (last_ir == nullptr) { return false; }
    if (!last_ir->isBranch()) { return false; }
    if (dcectx.isExcluded(last_ir)) { return false; }
    if (dcectx.isEffectStmt(last_ir)) { return false; }
    if (find_effect_kid(last_ir, dcectx)) {
        //IR_SWTICH might have multiple successors BB.
        setEffectStmt(last_ir, false, &act_ir_lst, dcectx);
        return true;
    }
    if (!getCFG()->isValidToKeepSSAIfRemoveBB(bb)) {
        //Branch stmt and BB can NOT be removed becasue that will make SSA
        //structure invalid.
        setEffectStmt(last_ir, false, &act_ir_lst, dcectx);
        return true;
    }
    return false;
}


//Try to set controlling BB of 'bb' to be effective.
//bb: given effect BB.
//act_ir_lst: collect stmts that become effect marked by this function.
bool DeadCodeElim::setControlDepBBToBeEffect(
    IRBB const* bb, OUT ConstIRList & act_ir_lst, MOD DCECtx & dcectx)
{
    bool change = markControlPredAndStmt(bb, act_ir_lst, dcectx);
    change |= markCFGPred(bb, dcectx);

    //If last ir of effect BB is branch operation, try to mark effect stmts
    //that controlled by current bb.
    change |= tryMarkBranch(bb, act_ir_lst, dcectx);
    return change;
}


bool DeadCodeElim::preserveControlDep(
    MOD ConstIRList & act_ir_lst, MOD DCECtx & dcectx)
{
    ASSERT0(m_cfg);
    bool change = false;
    List<IRBB*> lst_2;
    BBList * bbl = m_rg->getBBList();
    BBListIter ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        if (dcectx.isEffectBB(bb)) {
            change |= setControlDepBBToBeEffect(bb, act_ir_lst, dcectx);
        }

        //CASE:test_pre1()
        //  BB1:goto L1
        //  BB3:L1: ...
        //Note BB3 is ineffective, but 'goto' can not be removed!
        change |= tryMarkUnconditionalBranch(bb, act_ir_lst, dcectx);
    }
    return change;
}


bool DeadCodeElim::collectByPRSSA(IR const* x, MOD ConstIRList * pwlst2,
                                  MOD DCECtx & dcectx)
{
    ASSERT0(x->isReadPR() && PR_ssainfo(x) && usePRSSADU());
    IR const* d = PR_ssainfo(x)->getDef();
    if (d == nullptr) { return false; }
    ASSERT0(d->is_stmt());
    ASSERT0(d->isWritePR() || d->isCallHasRetVal());
    if (dcectx.isExcluded(d)) { return false; }
    if (dcectx.isEffectStmt(d)) { return false; }
    setEffectStmt(d, true, pwlst2, dcectx);
    return true;
}


bool DeadCodeElim::collectAllDefThroughDefChain(
    MDDef const* tdef, IR const* use, OUT ConstIRList * pwlst2,
    MOD DCECtx & dcectx)
{
    bool change = false;
    ASSERT0(tdef);
    ConstMDDefIter ii;
    for (MDDef const* def = m_mdssamgr->iterDefInitCTillKillingDef(
             tdef, use, ii);
         def != nullptr;
         def = m_mdssamgr->iterDefNextCTillKillingDef(use, ii)) {
        if (def->is_phi()) {
            //Merged DEF will be iterated.
            continue;
        }
        IR const* stmt = def->getOcc();
        ASSERT0(stmt);

        //TODO:for now, we have to walk alone with DEF chain to
        //mark almost all DEF to be effect. This may lead to
        //traverse the same DEF many times. Apply DP like algo to reduce
        //the traversal time.
        if (dcectx.isExcluded(stmt)) {
            continue;
        }
        if (dcectx.isEffectStmt(stmt)) {
            continue; //Check all previous DEF in debug mode.
        }
        change = true;
        setEffectStmt(stmt, true, pwlst2, dcectx);
    }
    return change;
}


bool DeadCodeElim::collectByMDSSA(IR const* x, MOD ConstIRList * pwlst2,
                                  MOD DCECtx & dcectx)
{
    ASSERT0(x->isMemRefNonPR() && useMDSSADU());
    ASSERT0(x->is_exp());
    MDSSAInfo * mdssainfo = m_mdssamgr->getMDSSAInfoIfAny(x);
    if (mdssainfo == nullptr) {
        return false;
    }
    bool change = false;
    VOpndSetIter iter = nullptr;
    MD const* mustuse = x->getRefMD();
    for (BSIdx i = mdssainfo->readVOpndSet().get_first(&iter);
         i != BS_UNDEF; i = mdssainfo->readVOpndSet().get_next(i, &iter)) {
        VOpnd const* t = m_mdssamgr->getVOpnd(i);
        if (t == nullptr) {
            //VOpnd may have been removed.
            continue;
        }
        ASSERT0(t->is_md());
        MDDef * tdef = ((VMD*)t)->getDef();
        if (tdef == nullptr) { continue; }
        if (tdef->is_phi()) {
            //TODO: iter phi.
            change |= collectAllDefThroughDefChain(tdef, x, pwlst2, dcectx);
            continue;
        }

        IR const* defstmt = tdef->getOcc();
        ASSERT0(defstmt);
        if (defstmt->isCallStmt()) {
            //CASE:call()
            //        =USE
            //Call is the only stmt that need to process specially.
            //Because it always is not dominated killing-def.
            change |= collectAllDefThroughDefChain(tdef, x, pwlst2, dcectx);
            continue;
        }

        MD const* mustdef = defstmt->getRefMD();
        if (mustuse != nullptr &&
            mustdef != nullptr &&
            mustuse->is_exact() &&
            mustdef->is_exact()) {
            if (mustdef == mustuse || mustdef->is_overlap(mustuse)) {
                if (dcectx.isExcluded(defstmt)) { continue; }
                if (dcectx.isEffectStmt(defstmt)) { continue; }
                setEffectStmt(defstmt, true, pwlst2, dcectx);
                change = true;
            }
            //Do NOT set 'defstmt' to be effect because
            //the Def and Use are independent.
            //e.g:arr[1]=10;
            //    return arr[2];
            continue;
        }

        if (mustuse != nullptr) {
            //TODO:
            //CASE1:DEF=
            //         =USE
            //CASE2:...=
            //         =USE
            //Both cases need to collect all DEFs until meet
            //the dominated killing-def.
            change |= collectAllDefThroughDefChain(tdef, x, pwlst2, dcectx);
            continue;
        }

        //CASE1:...=
        //         =...
        //CASE2:DEF=
        //         =...
        //Both cases need to collect all DEFs through def-chain.
        change |= collectAllDefThroughDefChain(tdef, x, pwlst2, dcectx);
    }
    return change;
}


bool DeadCodeElim::collectByDUSet(IR const* x, MOD ConstIRList * pwlst2,
                                  MOD DCECtx & dcectx)
{
    ASSERT0(x->is_exp());
    DUSet const* defs = x->readDUSet();
    if (defs == nullptr) { return false; }
    DUSetIter di = nullptr;
    bool change = false;
    for (BSIdx i = defs->get_first(&di);
         i != BS_UNDEF; i = defs->get_next(i, &di)) {
        IR const* d = m_rg->getIR(i);
        ASSERT0(d->is_stmt());
        if (dcectx.isExcluded(d)) { continue; }
        if (dcectx.isEffectStmt(d)) { continue; }
        change = true;
        setEffectStmt(d, true, pwlst2, dcectx);
    }
    return change;
}


//Try to revise DomInfo, SSA, etc before removing 'stmt'.
//Return true if CFG, Dom and SSA are all maintained.
static bool tryReviseAnaInfo(IR const* stmt, DeadCodeElim const* dce,
                             DCECtx const& dcectx)
{
    switch (stmt->getCode()) {
    SWITCH_CASE_BRANCH_OP:
        //TODO:In some cases, such as reducable graph, Dom info and SSA info
        //may be maintained incrementally.
        break;
    default: UNREACHABLE();
    }
    //For now, we can not revise the DomInfo because the controlflow
    //modification is too compilcate to maintain.
    //e.g:BB1->BB2->BB3
    //     |         ^
    //     \________/
    //BB1 is the idom of BB3, after removing BB1->BB3, BB2 is add to the BB3's
    //DomSet, and being the idom of BB3 as well.
    //Note the invalidation to DomInfo and MDSSA cause costly recomputation of
    //all the necessary data-structure and information which will increase
    //dramatically the compilation time.
    dcectx.getOptCtx()->setInvalidIfCFGChanged();
    dcectx.getOptCtx()->setInvalidMDSSA();
    return false;
}


static bool removeIneffectIRImpl(
    DeadCodeElim const* dce, DCECtx const& dcectx,
    IRBB * bb, BBListIter const& bbit, OUT bool & remove_branch_stmt)
{
    IRListIter ctir = nullptr;
    IRListIter next = nullptr;
    bool tobecheck = false;
    Region * rg = dce->getRegion();
    IRCFG * cfg = dce->getCFG();
    for (BB_irlist(bb).get_head(&ctir), next = ctir;
         ctir != nullptr; ctir = next) {
        IR * stmt = ctir->val();
        BB_irlist(bb).get_next(&next);
        if (dcectx.isEffectStmt(stmt)) { continue; }

        //Revise DU chains before removing IR stmt.
        //Note in SSA mode, user should not just remove the SSA def,
        //while user has to consider the SSA_uses and make sure they are
        //all removable. Utilizing SSA related API to maintain DU chain.
        dumpRemovedIneffectIR(stmt, bb, dce);
        xoc::removeStmt(stmt, rg, *dcectx.getOptCtx());
        if (stmt->isConditionalBr() ||
            stmt->isUnconditionalBr() ||
            stmt->isMultiConditionalBr()) {
            remove_branch_stmt = true;
            if (!tryReviseAnaInfo(stmt, dce, dcectx)) {
                //No need to verify PHI here, because SSA mode will be rebuilt
                //after this function if revising analysis info failed.
            }
            CfgOptCtx ctx(*dcectx.getOptCtx());
            cfg->changeToBeFallthroughBB(bb, bbit, ctx);
        }
        //Now, stmt is safe to free.
        rg->freeIRTree(stmt);

        //Remove stmt from BB.
        BB_irlist(bb).EList<IR*, IR2Holder>::remove(ctir);
        tobecheck = true;
    }
    return tobecheck;
}


bool DeadCodeElim::removeIneffectIR(
    DCECtx const& dcectx, OUT bool & remove_branch_stmt)
{
    BBListIter bbit = nullptr;
    List<IRBB*> * bbl = m_rg->getBBList();
    bool change = false;
    for (IRBB * bb = bbl->get_head(&bbit);
         bb != nullptr; bb = bbl->get_next(&bbit)) {
        change |= removeIneffectIRImpl(
            this, dcectx, bb, bbit, remove_branch_stmt);
        if (!useMDSSADU() || !dcectx.getOptCtx()->is_dom_valid()) {
            //Optimize MDSSA operations, such as PHI, need valid DomInfo.
            continue;
        }
        if (m_is_reserve_phi) {
            //TBD:Do we need to remove MDPhi which witout any USE?
            //I think keeping PHI there could maintain DefDef-Chain, e.g:
            //md1v1, md1v2, md1v3, md1v4 are in different BBs. In the case
            //MDPhi is acted as a disjoint dominator that point to diverge
            //data flow to md1v3 and md1v4.
            //e.g:md1v1<-...
            //      |
            //      V
            //    md1v2<-PHI
            //    |        |
            //    V        V
            // md1v3<-...  md1v4<-...
            continue;
        }
        change |= m_mdssamgr->removeRedundantPhi(bb, *dcectx.getOptCtx());
    }
    return change;
}


bool DeadCodeElim::collectByDU(
    IR const* x, MOD ConstIRList * pwlst2, MOD DCECtx & dcectx,
    bool usemdssa, bool useprssa)
{
    ASSERT0(x->is_exp());
    if (x->isReadPR() && useprssa) {
        return collectByPRSSA(x, pwlst2, dcectx);
    }
    if (x->isMemRefNonPR() && usemdssa) {
        return collectByMDSSA(x, pwlst2, dcectx);
    }
    return collectByDUSet(x, pwlst2, dcectx);
}


//Iterative record effect IRs, according to DU chain,
//and preserving the control flow dependence.
void DeadCodeElim::iterCollect(MOD ConstIRList & efflist, MOD DCECtx & dcectx)
{
    ConstIRList efflist2;
    ConstIRList * pwlst1 = &efflist;
    ConstIRList * pwlst2 = &efflist2;
    bool change = true;
    bool usemdssa = useMDSSADU();
    bool useprssa = usePRSSADU();
    List<IRBB*> succs;
    UINT const maxtry = 0xFFFF;
    UINT count = 0;
    ConstIRIter citer;
    while (change && count < maxtry) {
        change = false;
        for (IR const* ir = pwlst1->get_head();
             ir != nullptr; ir = pwlst1->get_next()) {
            citer.clean();
            for (IR const* x = iterExpInitC(ir, citer);
                 x != nullptr; x = iterExpNextC(citer)) {
                if (!x->isMemOpnd()) { continue; }
                change |= collectByDU(x, pwlst2, dcectx, usemdssa, useprssa);
           }
        }
        if (m_is_elim_cfs) {
            change |= preserveControlDep(*pwlst2, dcectx);
        }
        pwlst1->clean();
        xcom::swap(pwlst1, pwlst2);
        count++;
    }
    ASSERT0(count < maxtry);
}


bool DeadCodeElim::removeRedundantPhi(MOD OptCtx & oc)
{
    if (!oc.is_dom_valid()) {
        //Remove Phi need DOM info available.
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    }
    bool change = false;
    if (usePRSSADU()) {
        change |= m_prssamgr->refinePhi(oc);
    }
    if (useMDSSADU()) {
        change |= m_mdssamgr->removeRedundantPhi(oc);
    }
    return change;
}


void DeadCodeElim::addEffectIRAndBB(
    ConstIRList const& efflist, OUT DCECtx & dcectx) const
{
    ConstIRListIter it;
    for (IR const* ir = efflist.get_head(&it);
         ir != nullptr; ir = efflist.get_next(&it)) {
        ASSERT0(ir->is_stmt());
        ASSERTN(!dcectx.isExcluded(ir), ("effect ir should not be in list"));
        dcectx.addEffectStmt(ir);
        dcectx.addEffectBB(ir->getBB());
    }
}


bool DeadCodeElim::performByEffectIRList(
    ConstIRList & efflist, OUT DCECtx & dcectx, OUT bool remove_branch_stmt)
{
    addEffectIRAndBB(efflist, dcectx);
    return iterCollectAndElim(efflist, dcectx, remove_branch_stmt);
}


bool DeadCodeElim::iterCollectAndElim(
    ConstIRList & efflist, OUT DCECtx & dcectx, OUT bool & remove_branch_stmt)
{
    checkValidAndRecomputeCDG(dcectx);
    iterCollect(efflist, dcectx);

    //Remove ineffect IRs.
    //dcectx.setRemoveCFS(false);
    bool removed = removeIneffectIR(dcectx, remove_branch_stmt);

    //Remove dissociated vertex.
    OptCtx * oc = dcectx.getOptCtx();
    CfgOptCtx coctx(*oc);
    if (useMDSSADU()) {
        //CFG opt will attempt to maintain MDSSA information and update DOM
        //info as much as possible.
        CFGOPTCTX_need_update_dominfo(&coctx) = true;
    }
    if (m_cfg->performMiscOpt(coctx)) {
        //Note DOM info will changed without maintaining.
        //CFG has been changed, thus remove empty BB to produce more
        //optimization opportunities.
        //TODO: DO not recompute whole SSA/MDSSA. Instead, update
        //SSA|MDSSA info especially PHI operands incrementally.
        removed = true;
    }
    if (!oc->is_dom_valid()) {
        //If DomInfo is invalid, MDSSA info is also not maintained.
        oc->setInvalidMDSSA();
        m_rg->getPassMgr()->checkValidAndRecompute(oc, PASS_MDSSA_MGR,
            PASS_DOM, PASS_LOOP_INFO, PASS_CDG, PASS_UNDEF);
    }
    removed |= removeRedundantPhi(*oc);
    ASSERT0(m_dumgr->verifyMDRef());
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, *oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, *oc));
    return removed;
}


bool DeadCodeElim::elimImpl(OptCtx & oc, OUT DCECtx & dcectx,
                            OUT bool & remove_branch_stmt)
{
    bool change = false;
    bool removed = false;
    UINT count = 0;
    ConstIRList effstmtlist;
    UINT const max_iter = 0xFFFF;
    remove_branch_stmt = false;
    do {
        dcectx.reinit();
        removed = false;
        count++;
        effstmtlist.clean();
        markEffectIR(effstmtlist, dcectx);
        removed = iterCollectAndElim(effstmtlist, dcectx, remove_branch_stmt);
        change |= removed;
    } while (removed && count < max_iter);
    ASSERT0(!removed);
    return change;
}


bool DeadCodeElim::initSSAMgr(OptCtx const& oc)
{
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
    return true;
}


//An aggressive algo will be used if CDG is avaliable.
bool DeadCodeElim::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    START_TIMER(t, getPassName());
    if (!initSSAMgr(oc)) { return false; }
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    if (g_dump_opt.isDumpBeforePass()) {
        dumpBeforePass();
    }
    bool remove_branch_stmt = false;
    DCECtx dcectx(&oc, *getSBSMgr().getSegMgr());
    bool change = elimImpl(oc, dcectx, remove_branch_stmt);
    if (!change) {
        m_rg->getLogMgr()->cleanBuffer();
        END_TIMER(t, getPassName());
        return false;
    }
    dump(dcectx);

    //DU chain and DU reference should be maintained.
    ASSERT0(m_dumgr->verifyMDRef());
    if (remove_branch_stmt) {
        //Branch stmt will effect control-flow-data-structure.
        oc.setInvalidIfCFGChanged();
    }
    oc.setInvalidIfDUMgrLiveChanged();
    ASSERT0(verifyMDDUChain(m_rg, oc));
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    ASSERT0(m_cfg->verifyDom());
    END_TIMER(t, getPassName());
    return true;
}
//END DeadCodeElim

} //namespace xoc
