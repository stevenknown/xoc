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

class IterSideEffect : public VisitIRTree {
    bool m_has_sideeffect;
protected:
    virtual bool visitIR(IR const* ir)
    {
        if (ir->isMayThrow(false) || ir->hasSideEffect(false) ||
            ir->isNoMove(false)) {
            m_has_sideeffect = true;
            return false;
        }
        return true;
    }
public:
    IterSideEffect(IR const* ir) : m_has_sideeffect(false) { visit(ir); }
    bool hasSideEffect() const { return m_has_sideeffect; }
};

//
//START DCECtx
//
void DCECtx::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP DCECtx --==");
    note(rg, "\nEFFECT BB:");
    for (BSIdx i = m_effect_bb.get_first();
         i != BS_UNDEF; i = m_effect_bb.get_next(i)) {
        IRBB const* bb = rg->getBBMgr()->getBB(i);
        ASSERT0(bb);
        prt(rg, "%u ", bb->id());
    }
    note(rg, "\nEFFECT STMT:");
    rg->getLogMgr()->incIndent(2);
    for (BSIdx i = m_effect_stmt.get_first();
         i != BS_UNDEF; i = m_effect_stmt.get_next(i)) {
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
    IR const* stmt, bool set_bb_effect, MOD List<IR const*> * act_ir_lst,
    DCECtx & dcectx)
{
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
    if (!getRegion()->isLogMgrInit()) { return false; }
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


bool DeadCodeElim::dump(DCECtx const& dcectx) const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    dcectx.dump(m_rg);
    dumpBBList(m_rg->getBBList(), m_rg);
    if (usePRSSADU()) {
        m_prssamgr->dumpBrief();
    }
    if (useMDSSADU()) {
        m_mdssamgr->dump();
    }
    Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


//CFG optimization may invalid CDG.
void DeadCodeElim::checkValidAndRecomputeCDG()
{
    if (m_is_elim_cfs) {
        m_rg->getPassMgr()->checkValidAndRecompute(m_oc, PASS_PDOM, PASS_CDG,
                                                   PASS_RPO, PASS_UNDEF);
        m_cdg = (CDG*)m_rg->getPassMgr()->queryPass(PASS_CDG);
        ASSERT0(m_cdg && m_cdg->is_valid());
        return;
    }
    m_cdg = nullptr;
}


bool DeadCodeElim::checkEffectStmt(IR const* ir)
{
    IterSideEffect it(ir);
    if (it.hasSideEffect()) {
        //TBD:For now, DCE permits remove redundant dummy operation.
        return true;
    }
    if (!m_is_use_md_du && ir->isMemRefNonPR()) {
        return true;
    }
    MD const* mustdef = ir->getRefMD();
    if (mustdef != nullptr) {
        if (is_effect_write(mustdef->get_base())) {
            return true;
        }
    } else {
        MDSet const* maydefs = ir->getRefMDSet();
        if (maydefs != nullptr) {
            MDSetIter iter;
            for (BSIdx i = maydefs->get_first(&iter);
                 i != BS_UNDEF; i = maydefs->get_next(i, &iter)) {
                MD * md = m_md_sys->getMD(i);
                ASSERT0(md);
                if (is_effect_write(md->get_base())) {
                    return true;
                }
            }
        }
    }
    ConstIRIter citer;
    for (IR const* x = iterExpInitC(ir, citer);
         x != nullptr; x = iterExpNextC(citer)) {
        if (!m_is_use_md_du && x->isMemRefNonPR()) {
            return true;
        }
        if (!x->isMemRef()) { continue; }

        //Check if using volatile variable.
        //e.g: volatile int g = 0;
        //    while(g); # The stmt has effect.
        MD const* md = x->getRefMD();
        if (md != nullptr) {
            if (is_effect_read(md->get_base())) {
                return true;
            }
            continue;
        }
        MDSet const* mds = x->getRefMDSet();
        if (mds == nullptr) { continue; }
        MDSetIter iter;
        for (BSIdx i = mds->get_first(&iter);
             i != BS_UNDEF; i = mds->get_next(i, &iter)) {
            MD * md2 = m_md_sys->getMD(i);
            ASSERT0(md2 != nullptr);
            if (is_effect_read(md2->get_base())) {
                return true;
            }
        }
    }
    return false;
}


//Return true if ir is effect.
bool DeadCodeElim::checkCall(IR const* ir) const
{
    ASSERT0(ir->isCallStmt());
    return !ir->isReadOnly() || IR_has_sideeffect(ir) || IR_no_move(ir);
}


//Mark effect IRs.
void DeadCodeElim::mark_effect_ir(MOD List<IR const*> & work_list,
                                  MOD DCECtx & dcectx)
{
    List<IRBB*> * bbl = m_rg->getBBList();
    BBListIter ct;
    for (IRBB * bb = bbl->get_head(&ct);
         bb != nullptr; bb = bbl->get_next(&ct)) {
        BBIRListIter irit;
        for (IR const* ir = bb->getIRList().get_head(&irit);
             ir != nullptr; ir = bb->getIRList().get_next(&irit)) {
            switch (ir->getCode()) {
            case IR_REGION:
                //The redundant of Region should be processed in IPA
                //rather than DCE, thus regard Region always effect.
            case IR_RETURN:
                //Do NOT set exit-bb to be effect.
                //That will generate redundant control-flow dependence.
                //CASE:
                //    IF (...)
                //        ...
                //    ENDIF
                //    RETURN //EXIT BB
                //IF clause stmt is redundant code.
                setEffectStmt(ir, true, &work_list, dcectx);
                break;
            SWITCH_CASE_CALL:
                if (checkCall(ir)) {
                    setEffectStmt(ir, true, &work_list, dcectx);
                }
                break;
            SWITCH_CASE_BRANCH_OP:
                if (!m_is_elim_cfs) {
                    setEffectStmt(ir, true, &work_list, dcectx);
                }
                break;
            default:
                if (checkEffectStmt(ir)) {
                    setEffectStmt(ir, true, &work_list, dcectx);
                }
            }
        }
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


bool DeadCodeElim::markControlPredAndStmt(IRBB const* bb,
                                          OUT List<IR const*> & act_ir_lst,
                                          MOD DCECtx & dcectx)
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


bool DeadCodeElim::tryMarkUnconditionalBranch(IRBB const* bb,
                                              MOD List<IR const*> & act_ir_lst,
                                              MOD DCECtx & dcectx)
{
    IR * last_ir = const_cast<IRBB*>(bb)->getLastIR(); //last IR of BB.
    if (last_ir != nullptr &&
        last_ir->isUnconditionalBr() &&
        !dcectx.isEffectStmt(last_ir) &&
        find_effect_kid(last_ir, dcectx)) {
        //TBD: Does effect_kid of last_ir have to be considered?
        setEffectStmt(last_ir, true, &act_ir_lst, dcectx);
        return true;
    }
    return false;
}


bool DeadCodeElim::tryMarkBranch(IRBB const* bb,
                                 OUT List<IR const*> & act_ir_lst,
                                 MOD DCECtx & dcectx)
{
    //If last ir of effect BB is branch operation, try to mark effect stmts
    //that controlled by current bb.
    IR * last_ir = const_cast<IRBB*>(bb)->getLastIR(); //last IR of BB.
    if (last_ir != nullptr && last_ir->isBranch() &&
        !dcectx.isEffectStmt(last_ir)) {
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
    }
    return false;
}


//Try to set controlling BB of 'bb' to be effective.
//bb: given effect BB.
//act_ir_lst: collect stmts that become effect marked by this function.
bool DeadCodeElim::setControlDepBBToBeEffect(IRBB const* bb,
                                             OUT List<IR const*> & act_ir_lst,
                                             MOD DCECtx & dcectx)
{
    bool change = markControlPredAndStmt(bb, act_ir_lst, dcectx);
    change |= markCFGPred(bb, dcectx);

    //If last ir of effect BB is branch operation, try to mark effect stmts
    //that controlled by current bb.
    change |= tryMarkBranch(bb, act_ir_lst, dcectx);
    return change;
}


bool DeadCodeElim::preserve_cd(MOD List<IR const*> & act_ir_lst,
                               MOD DCECtx & dcectx)
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


bool DeadCodeElim::collectByPRSSA(IR const* x, MOD List<IR const*> * pwlst2,
                                  MOD DCECtx & dcectx)
{
    ASSERT0(x->isReadPR() && PR_ssainfo(x) && usePRSSADU());
    IR const* d = PR_ssainfo(x)->getDef();
    if (d == nullptr) { return false; }
    ASSERT0(d->is_stmt());
    ASSERT0(d->isWritePR() || d->isCallHasRetVal());
    if (dcectx.isEffectStmt(d)) { return false; }
    setEffectStmt(d, true, pwlst2, dcectx);
    return true;
}


bool DeadCodeElim::collectAllDefThroughDefChain(
    MDDef const* tdef, IR const* use, OUT xcom::List<IR const*> * pwlst2,
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

        if (dcectx.isEffectStmt(stmt)) {
            continue; //Check all previous DEF in debug mode.
        }
        change = true;
        setEffectStmt(stmt, true, pwlst2, dcectx);
    }
    return change;
}


bool DeadCodeElim::collectByMDSSA(IR const* x, MOD List<IR const*> * pwlst2,
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


bool DeadCodeElim::collectByDUSet(IR const* x, MOD List<IR const*> * pwlst2,
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
        if (!dcectx.isEffectStmt(d)) {
            change = true;
            setEffectStmt(d, true, pwlst2, dcectx);
        }
    }
    return change;
}


//Try to revise DomInfo before removing 'stmt'.
static void tryReviseDomInfoForUncondBr(IR const* stmt, DeadCodeElim const* dce,
                                        DCECtx const& dcectx)
{
    ASSERTN(stmt->is_goto(), ("TODO"));
    dcectx.getOptCtx()->setInvalidIfCFGChanged();
    dcectx.getOptCtx()->setInvalidMDSSA();
}


//Try to revise DomInfo, SSA, etc before removing 'stmt'.
static void tryReviseAnalysisInfo(IR const* stmt, DeadCodeElim const* dce,
                                  DCECtx const& dcectx)
{
    switch (stmt->getCode()) {
    case IR_GOTO:
        tryReviseDomInfoForUncondBr(stmt, dce, dcectx);
        return;
    case IR_FALSEBR:
    case IR_TRUEBR:
    default: break;
    }
    //We can not revise the DomInfo because the controlflow modification is
    //too compilcate.
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
}


static bool removeIneffectIRImpl(DeadCodeElim const* dce, DCECtx const& dcectx,
                                 IRBB * bb, BBListIter const& bbit,
                                 OUT bool & remove_branch_stmt)
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

        //Revise DU chains.
        //Could not just remove the SSA def, you should consider
        //the SSA_uses and make sure they are all removable.
        //Use SSA related API.
        xoc::removeStmt(stmt, rg, *dcectx.getOptCtx());
        if (stmt->isConditionalBr() ||
            stmt->isUnconditionalBr() ||
            stmt->isMultiConditionalBr()) {
            remove_branch_stmt = true;
            tryReviseAnalysisInfo(stmt, dce, dcectx);
            //No need verify PHI here, because SSA will be rebuilt
            //after this function.
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
            //TBD:Do we need to remove MDPHI which witout any USE?
            //I think keeping PHI there could maintain Def-Chain, e.g:
            //md1v1, md1v2, md1v3, md1v4 are in different BBs. Here PHI
            //is acted as a disjoint dominator point to diverge data flow
            //to md1v3 and md1v4.
            //e.g:md1v1<-...
            //      |
            //      V
            //    md1v2<-PHI
            //    |        |
            //    V        V
            // md1v3<-...  md1v4<-...
            continue;
        }
        if (m_mdssamgr->removeRedundantPhi(bb, *dcectx.getOptCtx())) {
            change = true;
        }
    }
    return change;
}


bool DeadCodeElim::collectByDU(
    IR const* x, MOD List<IR const*> * pwlst2, MOD DCECtx & dcectx,
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
void DeadCodeElim::iter_collect(MOD List<IR const*> & work_list,
                                MOD DCECtx & dcectx)
{
    List<IR const*> work_list2;
    List<IR const*> * pwlst1 = &work_list;
    List<IR const*> * pwlst2 = &work_list2;
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
            change |= preserve_cd(*pwlst2, dcectx);
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


bool DeadCodeElim::elimImpl(OptCtx & oc, OUT DCECtx & dcectx,
                            OUT bool & remove_branch_stmt)
{
    bool change = false;
    bool removed = false;
    UINT count = 0;
    List<IR const*> work_list;
    UINT const max_iter = 0xFFFF;
    remove_branch_stmt = false;
    do {
        dcectx.reinit(m_rg);
        checkValidAndRecomputeCDG();
        removed = false;
        count++;
        //Mark effect IRs.
        work_list.clean();
        mark_effect_ir(work_list, dcectx);
        iter_collect(work_list, dcectx);

        //Remove ineffect IRs.
        //dcectx.setRemoveCFS(false);
        removed = removeIneffectIR(dcectx, remove_branch_stmt);

        //Remove dissociated vertex.
        CfgOptCtx coctx(oc);
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
        if (!oc.is_dom_valid()) {
            //If DomInfo is invalid, MDSSA info is also not maintained.
            oc.setInvalidMDSSA();
            m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_MDSSA_MGR,
                PASS_DOM, PASS_LOOP_INFO, PASS_CDG, PASS_UNDEF);
        }
        removed |= removeRedundantPhi(oc);
        ASSERT0(m_dumgr->verifyMDRef());
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg, oc));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
        change |= removed;
    } while (removed && count < max_iter);
    ASSERT0(!removed);
    return change;
}


//An aggressive algo will be used if CDG is avaliable.
bool DeadCodeElim::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    m_oc = &oc;
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
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    DumpBufferSwitch buff(m_rg->getLogMgr());
    if (!g_dump_opt.isDumpToBuffer()) { buff.close(); }
    if (g_dump_opt.isDumpBeforePass() && g_dump_opt.isDumpDCE()) {
        dumpBeforePass();
    }
    bool remove_branch_stmt = false;
    DCECtx dcectx(&oc);
    bool change = elimImpl(oc, dcectx, remove_branch_stmt);
    if (!change) {
        m_rg->getLogMgr()->cleanBuffer();
        END_TIMER(t, getPassName());
        return false;
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpDCE()) {
        dump(dcectx);
    }

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
