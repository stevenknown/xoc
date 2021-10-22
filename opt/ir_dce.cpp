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

//
//START DeadCodeElim
//
void DeadCodeElim::setEffectStmt(IR const* stmt,
                                 MOD xcom::BitSet * is_bb_effect,
                                 MOD List<IR const*> * act_ir_lst)
{
    ASSERT0(stmt && stmt->is_stmt());
    act_ir_lst->append_tail(stmt);
    m_is_stmt_effect.bunion(stmt->id());
    ASSERT0(stmt->getBB());
    if (is_bb_effect != nullptr) {
        is_bb_effect->bunion(stmt->getBB()->id());
    }
}


void DeadCodeElim::setEffectMDDef(MDDef const* mddef,
                                  MOD xcom::BitSet * is_bb_effect)
{
    ASSERT0(mddef);
    m_is_mddef_effect.append(mddef);
    ASSERT0(mddef->getBB());
    if (is_bb_effect != nullptr) {
        is_bb_effect->bunion(mddef->getBB()->id());
    }
}


bool DeadCodeElim::dump() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    note(getRegion(), "\n==-- Ineffect BB --==");
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(getRegion(), "\n--0- BB%d", bb->id());
        if (!m_is_bb_effect.is_contain(bb->id())) {
            prt(getRegion(), "\t\tineffect BB!");
        }
    }
    note(getRegion(), "\n");
    dumpBBList(m_rg->getBBList(), m_rg);
    if (usePRSSADU()) {
        m_prssamgr->dump();
    }
    if (useMDSSADU()) {
        m_mdssamgr->dump();
    }
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


//Return true if ir can be optimized.
bool DeadCodeElim::check_stmt(IR const* ir)
{
    if (ir->isMayThrow() || ir->hasSideEffect() || ir->isNoMove()) {
        return true;
    }

    if (!m_is_use_md_du && ir->isMemoryRefNonPR()) {
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
            for (INT i = maydefs->get_first(&iter);
                 i >= 0; i = maydefs->get_next(i, &iter)) {
                MD * md = m_md_sys->getMD(i);
                ASSERT0(md);
                if (is_effect_write(md->get_base())) {
                    return true;
                }
            }
        }
    }

    m_citer.clean();
    for (IR const* x = iterRhsInitC(ir, m_citer);
         x != nullptr; x = iterRhsNextC(m_citer)) {
        if (!m_is_use_md_du && x->isMemoryRefNonPR()) {
            return true;
        }

        if (!x->isMemoryRef()) { continue; }

        //Check if using volatile variable.
        //e.g: volatile int g = 0;
        //    while(g); # The stmt has effect.
        MD const* md = x->getRefMD();
        if (md != nullptr) {
            if (is_effect_read(md->get_base())) {
                return true;
            }
        } else {
            MDSet const* mds = x->getRefMDSet();
            if (mds != nullptr) {
                MDSetIter iter;
                for (INT i = mds->get_first(&iter);
                     i != -1; i = mds->get_next(i, &iter)) {
                    MD * md2 = m_md_sys->getMD(i);
                    ASSERT0(md2 != nullptr);
                    if (is_effect_read(md2->get_base())) {
                        return true;
                    }
                }
            }
        }
    }
    return false;
}


//Return true if ir is effect.
bool DeadCodeElim::check_call(IR const* ir) const
{
    ASSERT0(ir->isCallStmt());
    return !ir->isReadOnly() || IR_has_sideeffect(ir) || IR_no_move(ir);
}


//Mark effect IRs.
void DeadCodeElim::mark_effect_ir(MOD List<IR const*> & work_list)
{
    List<IRBB*> * bbl = m_rg->getBBList();
    BBListIter ct;
    for (IRBB * bb = bbl->get_head(&ct);
         bb != nullptr; bb = bbl->get_next(&ct)) {
        for (IR const* ir = BB_first_ir(bb);
             ir != nullptr; ir = BB_next_ir(bb)) {
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
                setEffectStmt(ir, &m_is_bb_effect, &work_list);
                break;
            case IR_CALL:
            case IR_ICALL:
                if (check_call(ir)) {
                    setEffectStmt(ir, &m_is_bb_effect, &work_list);
                }
                break;
            case IR_TRUEBR:
            case IR_FALSEBR:
            case IR_GOTO:
            case IR_IGOTO:
                if (!m_is_elim_cfs) {
                    setEffectStmt(ir, &m_is_bb_effect, &work_list);
                }
                break;
            default:
                if (check_stmt(ir)) {
                    setEffectStmt(ir, &m_is_bb_effect, &work_list);
                }
            }
        }
    }
}


//Return true if there are effect BBs that controlled by ir's BB.
//ir: stmt.
bool DeadCodeElim::find_effect_kid_condbr(IR const* ir) const
{
    ASSERT0(m_cfg);
    IRBB const* bb = ir->getBB();
    ASSERT0(bb);
    ASSERT0(ir->isConditionalBr() || ir->isMultiConditionalBr());
    //Mark the sucsessor on CFG and related stmts to be effect if needed.
    for (xcom::EdgeC const* ec = m_cfg->getVertex(bb->id())->getOutList();
         ec != nullptr; ec = ec->get_next()) {
        IRBB * succ = m_cfg->getBB(ec->getToId());
        ASSERT0(succ != nullptr);
        if (!m_cfg->isControlPred(bb, succ)) { continue; }

        for (IR * r = BB_irlist(succ).get_head();
             r != nullptr; r = BB_irlist(succ).get_next()) {
            if (m_is_stmt_effect.is_contain(r->id())) {
                return true;
            }
        }
    }
    return false;
}


//Return true if there are effect BBs that controlled by ir's BB.
//ir: stmt.
bool DeadCodeElim::find_effect_kid_uncondbr(IR const* ir) const
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
    for (xcom::EdgeC const* ecp = m_cfg->getVertex(bb->id())->getInList();
         ecp != nullptr; ecp = ecp->get_next()) {
        //Check if predecessing control-BB have effect stmt.
        IRBB const* pred = m_cfg->getBB(ecp->getFromId());
        if (!m_cfg->isControlPred(pred, bb)) { continue; }

        bool control_bb_has_effect_stmt = false;
        if (m_is_bb_effect.is_contain(pred->id())) {
            control_bb_has_effect_stmt = true;
        } else {
            BBIRListIter irit;
            for (IR * r = BB_irlist(pred).get_head(&irit);
                 r != nullptr; r = BB_irlist(pred).get_next(&irit)) {
                if (m_is_stmt_effect.is_contain(r->id())) {
                    control_bb_has_effect_stmt = true;
                    break;
                }
            }
        }
        if (!control_bb_has_effect_stmt) { continue; }

        //Check the path-relation between the successor of 'pred' and 'bb'.
        for (xcom::EdgeC const* ecs = m_cfg->getVertex(pred->id())->
                getOutList();
             ecs != nullptr; ecs = ecs->get_next()) {
            IRBB * succ = m_cfg->getBB(ecs->getToId());
            ASSERTN(succ, ("BB%d does not belong to CFG", ecs->getToId()));
            if (!m_cfg->isControlPred(bb, succ)) { continue; }

            for (IR * r = BB_irlist(succ).get_head();
                 r != nullptr; r = BB_irlist(succ).get_next()) {
                if (m_is_stmt_effect.is_contain(r->id())) {
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
    for (xcom::EdgeC const* ec = m_cfg->getVertex(bb->id())->getOutList();
         ec != nullptr; ec = ec->get_next()) {
        IRBB const* succ = m_cfg->getBB(ec->getToId());
        ASSERT0(succ);
        if (m_is_bb_effect.is_contain(succ->id())) { return true; }
        
        BBIRListIter irit;
        for (IR const* r = BB_irlist(succ).get_head(&irit);
             r != nullptr; r = BB_irlist(succ).get_next(&irit)) {
            if (m_is_stmt_effect.is_contain(r->id())) {
                return true;
            }
        }
    }
    return false;
}


//Return true if there are effect BBs that controlled by ir's BB.
//ir: stmt.
bool DeadCodeElim::find_effect_kid(IR const* ir) const
{
    if (ir->isConditionalBr() || ir->isMultiConditionalBr()) {
        return find_effect_kid_condbr(ir);
    }
    if (ir->isUnconditionalBr()) {
        return find_effect_kid_uncondbr(ir);
    }
    UNREACHABLE();
    return false;
}


bool DeadCodeElim::markControlPredAndStmt(IRBB const* bb,
                                          OUT List<IR const*> & act_ir_lst)
{
    ASSERT0(m_cdg);
    UINT bbid = bb->id();
    bool change = false;
    for (xcom::EdgeC const* ec = m_cdg->getVertex(bbid)->getInList();
         ec != nullptr; ec = ec->get_next()) {
        IRBB const* pred = m_cfg->getBB(ec->getFromId());
        ASSERT0(pred);
        if (!m_is_bb_effect.is_contain(pred->id())) {
            setEffectBB(pred);
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
        if (!m_is_stmt_effect.is_contain(last_ir->id())) {
            setEffectStmt(last_ir, nullptr, &act_ir_lst);
            change = true;
        }
    }
    return change;
}


//The function marks possible predecessor in CFG to be effect BB, e.g back-edge.
bool DeadCodeElim::markCFGPred(IRBB const* bb)
{
    bool change = false;
    UINT bbid = bb->id();
    ASSERT0(bb->rpo() > RPO_UNDEF);
    for (xcom::EdgeC const* ec = m_cfg->getVertex(bbid)->getInList();
         ec != nullptr; ec = ec->get_next()) {
        UINT predid = ec->getFromId();
        if (m_is_bb_effect.is_contain(predid)) { continue; }

        IRBB const* pred = m_cfg->getBB(predid);
        ASSERT0(pred->rpo() > RPO_UNDEF);
        if (pred->rpo() > (INT)bb->rpo()) {
            //Predecessor is lexicographical back of bb.
            setEffectBB(pred);
            change = true;
        }
    }
    return change;
}


bool DeadCodeElim::tryMarkUnconditionalBranch(IRBB const* bb,
                                              MOD List<IR const*> & act_ir_lst)
{
    IR * last_ir = const_cast<IRBB*>(bb)->getLastIR(); //last IR of BB.
    if (last_ir != nullptr && last_ir->isUnconditionalBr() &&
        !m_is_stmt_effect.is_contain(last_ir->id()) &&
        find_effect_kid(last_ir)) {
        //TO BE COMFIRED: Does effect_kid of last_ir have to be considered?
        setEffectStmt(last_ir, &m_is_bb_effect, &act_ir_lst);
        return true;
    }
    return false;
}


bool DeadCodeElim::tryMarkBranch(IRBB const* bb,
                                 OUT List<IR const*> & act_ir_lst)
{
    //If last ir of effect BB is branch operation, try to mark effect stmts
    //that controlled by current bb.
    IR * last_ir = const_cast<IRBB*>(bb)->getLastIR(); //last IR of BB.
    if (last_ir != nullptr && last_ir->isBranch() &&
        !m_is_stmt_effect.is_contain(last_ir->id()) &&
        find_effect_kid(last_ir)) {
        //IR_SWTICH might have multiple successors BB.
        setEffectStmt(last_ir, nullptr, &act_ir_lst);
        return true;
    }
    return false;
}


//Try to set controlling BB of 'bb' to be effective.
//bb: given effect BB.
//act_ir_lst: collect stmts that become effect marked by this function.
bool DeadCodeElim::setControlDepBBToBeEffect(IRBB const* bb,
                                             OUT List<IR const*> & act_ir_lst)
{
    bool change = markControlPredAndStmt(bb, act_ir_lst);
    change |= markCFGPred(bb);

    //If last ir of effect BB is branch operation, try to mark effect stmts
    //that controlled by current bb.
    change |= tryMarkBranch(bb, act_ir_lst);
    return change;
}


bool DeadCodeElim::preserve_cd(MOD List<IR const*> & act_ir_lst)
{
    ASSERT0(m_cfg);
    bool change = false;
    List<IRBB*> lst_2;
    BBList * bbl = m_rg->getBBList();
    BBListIter ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        if (m_is_bb_effect.is_contain(bb->id())) {
            change |= setControlDepBBToBeEffect(bb, act_ir_lst);
        }

        //CASE:test_pre1()
        //  BB1:goto L1
        //  BB3:L1: ...
        //Note BB3 is ineffective, but 'goto' can not be removed!
        change |= tryMarkUnconditionalBranch(bb, act_ir_lst);
    }
    return change;
}


bool DeadCodeElim::collectByPRSSA(IR const* x, MOD List<IR const*> * pwlst2)
{
    ASSERT0(x->isReadPR() && PR_ssainfo(x) && usePRSSADU());
    IR const* d = PR_ssainfo(x)->getDef();
    if (d == nullptr) { return false; }
    ASSERT0(d->is_stmt());
    ASSERT0(d->isWritePR() || d->isCallHasRetVal());
    if (m_is_stmt_effect.is_contain(d->id())) { return false; }
    setEffectStmt(d, &m_is_bb_effect, pwlst2);
    return true;
}


bool DeadCodeElim::collectAllDefThroughDefChain(
    MDDef const* tdef, IR const* use, OUT xcom::List<IR const*> * pwlst2)
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

        if (m_is_stmt_effect.is_contain(stmt->id())) {
            continue; //Check all previous DEF in debug mode.
        }
        change = true;
        setEffectStmt(stmt, &m_is_bb_effect, pwlst2);
    }
    return change;
}


bool DeadCodeElim::collectByMDSSA(IR const* x, MOD List<IR const*> * pwlst2)
{
    ASSERT0(x->isMemoryRefNonPR() && useMDSSADU());
    ASSERT0(x->is_exp());
    MDSSAInfo * mdssainfo = m_mdssamgr->getMDSSAInfoIfAny(x);
    if (mdssainfo == nullptr ||
        mdssainfo->readVOpndSet() == nullptr ||
        mdssainfo->readVOpndSet()->is_empty()) {
        return false;
    }
    bool change = false;
    VOpndSetIter iter = nullptr;
    MD const* mustuse = x->getRefMD();

    for (INT i = mdssainfo->readVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->readVOpndSet()->get_next(i, &iter)) {
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
            change |= collectAllDefThroughDefChain(tdef, x, pwlst2);
            continue;
        }

        IR const* defstmt = tdef->getOcc();
        ASSERT0(defstmt);
        if (defstmt->isCallStmt()) {
            //CASE:call()
            //        =USE
            //Call is the only stmt that need to process specially.
            //Because it always is not dominated killing-def.
            change |= collectAllDefThroughDefChain(tdef, x, pwlst2);
            continue;
        }

        MD const* mustdef = defstmt->getRefMD();
        if (mustuse != nullptr &&
            mustdef != nullptr &&
            mustuse->is_exact() &&
            mustdef->is_exact()) {
            if (mustdef == mustuse || mustdef->is_overlap(mustuse)) {
                if (m_is_stmt_effect.is_contain(defstmt->id())) { continue; }
                setEffectStmt(defstmt, &m_is_bb_effect, pwlst2);
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
            //CASE2:???=
            //         =USE
            //Both cases need to collect all DEFs until
            //the dominated killing-def.
            change |= collectAllDefThroughDefChain(tdef, x, pwlst2);
            continue;
        }

        //CASE1:???=
        //         =???
        //CASE2:DEF=
        //         =???
        //Both cases need to collect all DEFs through def-chain.
        change |= collectAllDefThroughDefChain(tdef, x, pwlst2);
    }
    return change;
}


bool DeadCodeElim::collectByDUSet(IR const* x, MOD List<IR const*> * pwlst2)
{
    ASSERT0(x->is_exp());
    DUSet const* defs = x->readDUSet();
    if (defs == nullptr) { return false; }
    DUSetIter di = nullptr;
    bool change = false;
    for (INT i = defs->get_first(&di); i >= 0; i = defs->get_next(i, &di)) {
        IR const* d = m_rg->getIR(i);
        ASSERT0(d->is_stmt());
        if (!m_is_stmt_effect.is_contain(d->id())) {
            change = true;
            setEffectStmt(d, &m_is_bb_effect, pwlst2);
        }
    }
    return change;
}


bool DeadCodeElim::removeIneffectIR(OUT bool & remove_branch_stmt)
{
    BBListIter ctbb = nullptr;
    List<IRBB*> bblst;
    List<IRBB*> * bbl = m_rg->getBBList();
    List<C<IRBB*>*> ctlst;
    bool change = false;
    for (IRBB * bb = bbl->get_head(&ctbb);
         bb != nullptr; bb = bbl->get_next(&ctbb)) {
        IRListIter ctir = nullptr;
        IRListIter next = nullptr;
        bool tobecheck = false;
        for (BB_irlist(bb).get_head(&ctir), next = ctir;
             ctir != nullptr; ctir = next) {
            IR * stmt = ctir->val();
            BB_irlist(bb).get_next(&next);
            if (!m_is_stmt_effect.is_contain(stmt->id())) {
                //Could not just remove the SSA def, you should consider
                //the SSA_uses and make sure they are all removable.
                //Use SSA related API.
                //ASSERT0(stmt->getSSAInfo() == nullptr);

                //Revise DU chains.
                //TODO: If SSA form is available, it doesn't need to maintain
                //DU chain of PR in DU manager counterpart.
                xoc::removeStmt(stmt, m_rg);
                if (stmt->isConditionalBr() ||
                    stmt->isUnconditionalBr() ||
                    stmt->isMultiConditionalBr()) {
                    remove_branch_stmt = true;
                    reviseSuccForFallthroughBB(bb, ctbb, bbl);
                    //No need verify PHI here, because SSA will rebuild
                    //after this function.
                    //ASSERT0(m_mdssamgr->verifyPhi(true));
                }

                //Now, stmt is safe to free.
                m_rg->freeIRTree(stmt);

                //Remove stmt from BB.
                BB_irlist(bb).remove(ctir);

                change = true;
                tobecheck = true;
            }
        }

        if (useMDSSADU()) {
            //TBD:Do we need to remove MDPHI which witout any USE?
            //I think keeping PHI there could maintain Def-Chain, e.g:
            //md1v1, md1v2, md1v3, md1v4 are in different BB, PHI is acted as
            //a disjoint dominate point to diverge data flow to md1v3
            //and md1v4.
            //        md1v1<-
            //          |
            //          V
            //        md1v2<-PHI
            //       /         |
            //       V         V
            //  md1v3<-        md1v4<-
            if (!m_is_reserve_phi && m_mdssamgr->removeRedundantPhi(bb)) {
                change = true;
                tobecheck = true;
            }
        }

        if (tobecheck) {
            bblst.append_tail(bb);
            ctlst.append_tail(ctbb);
        }
    }
    return change;
}


//Iterative record effect IRs, according to DU chain,
//and preserving the control flow dependence.
void DeadCodeElim::iter_collect(MOD List<IR const*> & work_list)
{
    List<IR const*> work_list2;
    List<IR const*> * pwlst1 = &work_list;
    List<IR const*> * pwlst2 = &work_list2;
    bool change = true;
    bool usemdssa = useMDSSADU();
    List<IRBB*> succs;
    UINT const maxtry = 0xFFFF;
    UINT count = 0;
    while (change && count < maxtry) {
        change = false;
        for (IR const* ir = pwlst1->get_head();
             ir != nullptr; ir = pwlst1->get_next()) {
            m_citer.clean();

            for (IR const* x = iterRhsInitC(ir, m_citer);
                 x != nullptr; x = iterRhsNextC(m_citer)) {
                if (!x->isMemoryOpnd()) { continue; }
                if (x->isReadPR() && PR_ssainfo(x) != nullptr) {
                    change |= collectByPRSSA(x, pwlst2);
                    continue;
                }
                if (usemdssa && m_mdssamgr->getMDSSAInfoIfAny(x) != nullptr) {
                    change |= collectByMDSSA(x, pwlst2);
                    continue;
                }
                change |= collectByDUSet(x, pwlst2);
            }
        }

        //dumpIRList((IREList&)*pwlst2);
        if (m_is_elim_cfs) {
            change |= preserve_cd(*pwlst2);
        }

        //dumpIRList((IREList&)*pwlst2);
        pwlst1->clean();
        List<IR const*> * tmp = pwlst1;
        pwlst1 = pwlst2;
        pwlst2 = tmp;
        count++;
    }
    ASSERT0(count < maxtry);
}


//Fix control flow if BB is empty.
//It will be illegal if empty BB has non-taken branch.
void DeadCodeElim::fix_control_flow(List<IRBB*> & bblst,
                                    List<C<IRBB*>*> & ctlst)
{
    ASSERT0(m_cdg);
    BBList * bbl = m_rg->getBBList();
    BBListIter ct = ctlst.get_head();
    BBListIter bbct;
    for (bblst.get_head(&bbct); bbct != bblst.end();
         bbct = bblst.get_next(bbct), ct = ctlst.get_next()) {
        IRBB * bb = bbct->val();
        ASSERT0(ct && bb);
        if (BB_irlist(bb).get_elem_count() != 0) { continue; }

        xcom::EdgeC * vout = m_cfg->getVertex(bb->id())->getOutList();
        if (vout == nullptr || xcom::cnt_list(vout) <= 1) { continue; }

        BBListIter next_ct = ct;
        bbl->get_next(&next_ct);
        IRBB * next_bb = nullptr;
        if (next_ct != nullptr) {
            next_bb = next_ct->val();
        }

        while (vout != nullptr) {
            xcom::Edge * e = EC_edge(vout);
            if (EDGE_info(e) != nullptr &&
                CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e))) {
                vout = EC_next(vout);
                continue;
            }

            xcom::Vertex * s = EDGE_to(e);
            if (s->id() == bb->id() ||
                (next_bb != nullptr && s->id() == next_bb->id())) {
                vout = EC_next(vout);
                continue;
            }

            if (!m_cdg->is_control(bb->id(), s->id())) {
                //See dce.c:lexrun(), bb5 control bb6, but not control bb8.
                //if bb5 is empty, insert goto to bb8.
                IRBB * tgt = m_cfg->getBB(s->id());
                ASSERT0(tgt);

                //Find a normal label as target.
                LabelInfo const* li;
                for (li = tgt->getLabelList().get_head();
                     li != nullptr; li = tgt->getLabelList().get_next()) {
                    if (LABELINFO_is_catch_start(li) ||
                        LABELINFO_is_try_start(li) ||
                        LABELINFO_is_try_end(li) ||
                        LABELINFO_is_pragma(li)) {
                        continue;
                    }
                    break;
                }
                ASSERT0(li);

                IR * g = m_rg->buildGoto(li);
                BB_irlist(bb).append_tail(g);
                bool change = true;
                xcom::Vertex * bbv = m_cfg->getVertex(bb->id());
                while (change) {
                    xcom::EdgeC * ec = VERTEX_out_list(bbv);
                    change = false;
                    while (ec != nullptr) {
                        if (EC_edge(ec) != e) {
                            //May be remove multi edges.
                            ((xcom::Graph*)m_cfg)->removeEdgeBetween(
                                ec->getFrom(), ec->getTo());
                            change = true;
                            break;
                        }
                        ec = EC_next(ec);
                    }
                }
                break;
            } else {
                ASSERT0(BB_irlist(m_cfg->getBB(s->id())).
                        get_elem_count() == 0);
            }
            vout = EC_next(vout);
        }
    }
}


//Fix control flow edge if BB will become fallthrough BB.
//Remove out edges of bb except the fallthrough edge.
//Note it is illegal if empty BB has non-taken branch.
void DeadCodeElim::reviseSuccForFallthroughBB(IRBB * bb, BBListIter bbct,
                                              BBList * bbl) const
{
    ASSERT0(bb && bbct);
    xcom::EdgeC * ec = m_cfg->getVertex(bb->id())->getOutList();
    if (ec == nullptr) { return; }

    ASSERT0(!bb->is_empty());
    BBListIter next_ct = bbct;
    bbl->get_next(&next_ct);
    IRBB * next_bb = nullptr;
    if (next_ct != nullptr) {
        next_bb = next_ct->val();
    }

    bool has_fallthrough = false;
    xcom::EdgeC * next_ec = nullptr;
    for (; ec != nullptr; ec = next_ec) {
        next_ec = ec->get_next();

        xcom::Edge * e = ec->getEdge();
        if (e->info() != nullptr && CFGEI_is_eh((CFGEdgeInfo*)e->info())) {
            continue;
        }

        IRBB * succ = m_cfg->getBB(e->to()->id());
        ASSERT0(succ);
        if (succ == next_bb) {
            //Do not remove the fall-throught edge.
            has_fallthrough = true;
            ec = ec->get_next();
            continue;
        }

        //Note IRCFG::removeEdge() will invoke removeSuccessorDesignatePhiOpnd()
        //to update PHI, whereas in-edge of succ changed, and operands of phi
        //maintained as well.
        m_cfg->removeEdge(bb, succ);
    }

    //Add edge between bb and next_bb if bb is empty.
    //e.g:BB2->BB3,BB2->BB5,BB3->BB2
    //    Add edge BB3->BB5 if BB3 is empty.
    //     ____
    //     |   |
    //     V   |
    //  __BB2  |
    //  |  |   |
    //  |  V   |
    //  | BB3  |
    //  |  |___|
    //  |
    //  |->BB5
    if (next_bb != nullptr && !has_fallthrough) {
        //Note the function will add operands of PHI if 'next_bb' has PHI.
        m_cfg->addEdge(bb, next_bb);
    }
}


void DeadCodeElim::reinit()
{
    UINT irnum = m_rg->getIRVec()->get_elem_count() / BITS_PER_BYTE + 1;
    if (m_is_stmt_effect.get_byte_size() <= irnum) {
        m_is_stmt_effect.alloc(irnum + 1);
    }
    m_is_stmt_effect.clean();
    m_is_mddef_effect.clean();

    UINT bbnum = m_rg->getBBList()->get_elem_count() / BITS_PER_BYTE + 1;
    if (m_is_bb_effect.get_byte_size() <= bbnum) {
        m_is_bb_effect.alloc(bbnum + 1);
    }
    m_is_bb_effect.clean();
}


bool DeadCodeElim::removeRedundantPhi()
{
    bool change = false;
    if (usePRSSADU()) {
        change |= m_prssamgr->refinePhi();
    }
    if (useMDSSADU()) {
        change |= m_mdssamgr->removeRedundantPhi();
    }
    return change;
}


//An aggressive algo will be used if cdg is avaliable.
bool DeadCodeElim::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }

    if (!oc.is_ref_valid()) { return false; }
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
    if (m_is_elim_cfs) {
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_PDOM, PASS_CDG,
                                                   PASS_UNDEF);
        ASSERT0(oc.is_cdg_valid());
        m_cdg = (CDG*)m_rg->getPassMgr()->queryPass(PASS_CDG);
        ASSERT0(m_cdg);
    } else {
        m_cdg = nullptr;
    }
    reinit();

    bool change = false;
    bool removed = true;
    UINT count = 0;
    List<IR const*> work_list;
    UINT const max_iter = 0xFFFF;
    bool remove_branch_stmt = false;
    while (removed && count < max_iter) {
        removed = false;
        count++;

        //Mark effect IRs.
        work_list.clean();
        mark_effect_ir(work_list);
        iter_collect(work_list);

        //Remove ineffect IRs.
        removed = removeIneffectIR(remove_branch_stmt);
        if (!removed) { break; }

        //Reinit intra-used data structure.
        change = true;
        m_is_stmt_effect.clean();
        m_is_mddef_effect.clean();
        m_is_bb_effect.clean();
        if (m_cfg->performMiscOpt(oc)) {
            //CFG has been changed, thus remove empty BB to produce more
            //optimization opportunities.
            //TODO: DO not recompute whole SSA/MDSSA. Instead, update
            //SSA/MDSSA info especially PHI operands incrementally.
        }
        removed |= removeRedundantPhi();
        ASSERT0(m_rg->verifyMDRef());
        ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
        ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg));
        //fix_control_flow(bblst, ctlst);
    }
    ASSERT0(!removed);

    if (!change) {
        END_TIMER(t, getPassName());
        return false;
    }

    if (g_is_dump_after_pass && g_dump_opt.isDumpDCE()) {
        dump();
    }

    //DU chain and DU reference should be maintained.
    ASSERT0(m_rg->verifyMDRef() && verifyMDDUChain(m_rg));
    if (remove_branch_stmt) {
        //Branch stmt will effect control-flow-data-structure.
        oc.setInvalidIfCFGChanged();
    }
    oc.setInvalidIfDUMgrLiveChanged();
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg));
    END_TIMER(t, getPassName());
    return true;
}
//END DeadCodeElim

} //namespace xoc
