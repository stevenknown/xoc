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
                                 IN OUT xcom::BitSet * is_bb_effect,
                                 IN OUT List<IR const*> * act_ir_lst)
{
    ASSERT0(stmt && stmt->is_stmt());
    act_ir_lst->append_tail(stmt);
    m_is_stmt_effect.bunion(stmt->id());
    ASSERT0(stmt->getBB());
    if (is_bb_effect != NULL) {
        is_bb_effect->bunion(stmt->getBB()->id());
    }
}


void DeadCodeElim::dump()
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP DeadCodeElim ----==");
    note("\n==-- Ineffect BB --==");
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        note("\n--0- BB%d", BB_id(bb));
        if (!m_is_bb_effect.is_contain(BB_id(bb))) {
            prt("\t\tineffect BB!");
        }
    }
    note("\n");
    dumpBBList(m_rg->getBBList(), m_rg);
    if (m_prssamgr != NULL && m_prssamgr->isSSAConstructed()) {
        m_prssamgr->dump();
    }
    if (m_mdssamgr != NULL && m_mdssamgr->isMDSSAConstructed()) {
        m_mdssamgr->dump();
    }
    fflush(g_tfile);
}


//Return true if ir can be optimized.
bool DeadCodeElim::check_stmt(IR const* ir)
{
    if (ir->isMayThrow() || ir->hasSideEffect() || ir->isNoMove()) {
        return true;
    }

    if (!m_is_use_md_du && ir->isMemoryRefNotOperatePR()) {
        return true;
    }

    MD const* mustdef = ir->getRefMD();
    if (mustdef != NULL) {
        if (is_effect_write(mustdef->get_base())) {
            return true;
        }
    } else {
        MDSet const* maydefs = ir->getRefMDSet();
        if (maydefs != NULL) {
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
         x != NULL; x = iterRhsNextC(m_citer)) {
        if (!m_is_use_md_du && x->isMemoryRefNotOperatePR()) {
            return true;
        }

        if (!x->isMemoryRef()) { continue; }

        //Check if using volatile variable.
        //e.g: volatile int g = 0;
        //    while(g); # The stmt has effect.
        MD const* md = x->getRefMD();
        if (md != NULL) {
            if (is_effect_read(md->get_base())) {
                return true;
            }
        } else {
            MDSet const* mds = x->getRefMDSet();
            if (mds != NULL) {
                MDSetIter iter;
                for (INT i = mds->get_first(&iter);
                     i != -1; i = mds->get_next(i, &iter)) {
                    MD * md2 = m_md_sys->getMD(i);
                    ASSERT0(md2 != NULL);
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
    return !ir->isReadOnlyCall() || IR_has_sideeffect(ir) || IR_no_move(ir);
}


//Mark effect IRs.
void DeadCodeElim::mark_effect_ir(IN OUT List<IR const*> & work_list)
{
    List<IRBB*> * bbl = m_rg->getBBList();
    BBListIter ct;
    for (IRBB * bb = bbl->get_head(&ct);
         bb != NULL; bb = bbl->get_next(&ct)) {
        for (IR const* ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            switch (ir->getCode()) {
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


bool DeadCodeElim::find_effect_kid(IRBB const* bb, IR const* ir) const
{
    ASSERT0(m_cfg && m_cdg);
    ASSERT0(ir->getBB() == bb);
    if (ir->isConditionalBr() || ir->isMultiConditionalBr()) {
        xcom::EdgeC const* ec = VERTEX_out_list(m_cdg->getVertex(BB_id(bb)));
        while (ec != NULL) {
            IRBB * succ = m_cfg->getBB(ec->getToId());
            ASSERT0(succ != NULL);
            for (IR * r = BB_irlist(succ).get_head();
                 r != NULL; r = BB_irlist(succ).get_next()) {
                if (m_is_stmt_effect.is_contain(IR_id(r))) {
                    return true;
                }
            }
            ec = EC_next(ec);
        }
    } else if (ir->isUnconditionalBr()) {
        xcom::EdgeC const* ecp = VERTEX_in_list(m_cdg->getVertex(BB_id(bb)));
        while (ecp != NULL) {
            INT cd_pred = ecp->getFromId();
            xcom::EdgeC const* ecs = VERTEX_out_list(m_cdg->getVertex(cd_pred));
            while (ecs != NULL) {
                INT cd_succ = ecs->getToId();
                IRBB * succ = m_cfg->getBB(cd_succ);
                ASSERTN(succ, ("BB%d does not on CFG", cd_succ));
                for (IR * r = BB_irlist(succ).get_head();
                     r != NULL; r = BB_irlist(succ).get_next()) {
                    if (m_is_stmt_effect.is_contain(IR_id(r))) {
                        return true;
                    }
                }
                ecs = EC_next(ecs);
            }
            ecp = EC_next(ecp);
        }
    } else {
        UNREACHABLE();
    }
    return false;
}


//Set control-dep bb to be effective.
bool DeadCodeElim::setControlDepBBToBeEffect(
    IRBB const* bb,
    IN OUT List<IR const*> & act_ir_lst)
{
    bool change = false;
    UINT bbid = BB_id(bb);
    ASSERT0(m_cdg->getVertex(bbid));
    for (xcom::EdgeC const* ec = VERTEX_in_list(m_cdg->getVertex(bbid));
         ec != NULL; ec = EC_next(ec)) {
        INT cd_pred = ec->getFromId();
        if (!m_is_bb_effect.is_contain(cd_pred)) {
            m_is_bb_effect.bunion(cd_pred);
            change = true;
        }
    }

    ASSERT0(m_cfg->getVertex(bbid));
    xcom::EdgeC const* ec = VERTEX_in_list(m_cfg->getVertex(bbid));
    if (xcom::cnt_list(ec) >= 2) {
        ASSERT0(BB_rpo(bb) >= 0);
        UINT bbto = BB_rpo(bb);
        while (ec != NULL) {
            IRBB * pred = m_cfg->getBB(ec->getFromId());
            ASSERT0(pred);
            if (BB_rpo(pred) > (INT)bbto &&
                !m_is_bb_effect.is_contain(BB_id(pred))) {
                m_is_bb_effect.bunion(BB_id(pred));
                change = true;
            }
            ec = EC_next(ec);
        }
    }

    if (BB_irlist(bb).get_elem_count() == 0) { return change; }

    IR * ir = BB_last_ir(const_cast<IRBB*>(bb)); //last IR of BB.
    ASSERT0(ir != NULL);
    if ((ir->isConditionalBr() || ir->isMultiConditionalBr()) &&
        !m_is_stmt_effect.is_contain(ir->id())) {
        //IR_SWTICH might have multiple succ-BB.
        if (find_effect_kid(bb, ir)) {
            setEffectStmt(ir, NULL, &act_ir_lst);
            change = true;
        }
    }
    return change;
}


bool DeadCodeElim::preserve_cd(IN OUT List<IR const*> & act_ir_lst)
{
    ASSERT0(m_cfg && m_cdg);
    bool change = false;
    List<IRBB*> lst_2;
    BBList * bbl = m_rg->getBBList();
    BBListIter ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        if (m_is_bb_effect.is_contain(BB_id(bb))) {
            change |= setControlDepBBToBeEffect(bb, act_ir_lst);
        }

        //CASE: test_pre1()
        //    GOTO 0xa4f634 id:23
        //        CLABEL (name:L1) 0xa4f5e4 id:22 branch-target
        //    in BB3
        //    BB3 is ineffective, but GOTO can not be removed!
        if (BB_irlist(bb).get_elem_count() == 0) { continue; }

        IR * ir = BB_last_ir(bb); //last IR of BB.
        ASSERT0(ir);
        if (ir->isUnconditionalBr() && !m_is_stmt_effect.is_contain(ir->id())) {
            if (find_effect_kid(bb, ir)) {
                setEffectStmt(ir, &m_is_bb_effect, &act_ir_lst);
                change = true;
            }
        }
    }
    return change;
}


bool DeadCodeElim::collectByPRSSA(IR const* x, IN OUT List<IR const*> * pwlst2)
{
    ASSERT0(x->isReadPR() && PR_ssainfo(x));
    IR const* d = PR_ssainfo(x)->get_def();
    if (d == NULL) { return false; }
    ASSERT0(d->is_stmt());
    ASSERT0(d->isWritePR() || d->isCallHasRetVal());
    if (m_is_stmt_effect.is_contain(d->id())) { return false; }
    setEffectStmt(d, &m_is_bb_effect, pwlst2); 
    return true;
}


bool DeadCodeElim::collectAllDefThroughDefChain(
   MDDef const* tdef,
   IN OUT xcom::List<IR const*> * pwlst2)
{
    bool change = false;
    ASSERT0(tdef);
    ConstMDDefIter ii;    
    for (MDDef const* def = m_mdssamgr->iterDefInitC(tdef, ii);
         def != NULL; def = m_mdssamgr->iterDefNextC(ii)) {
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


bool DeadCodeElim::collectByMDSSA(IR const* x, IN OUT List<IR const*> * pwlst2)
{
    ASSERT0(x->isMemoryRefNotOperatePR() && m_mdssamgr);
    ASSERT0(x->is_exp()); 
    MDSSAInfo * mdssainfo = m_mdssamgr->getMDSSAInfoIfAny(x);
    if (mdssainfo == NULL ||
        mdssainfo->readVOpndSet() == NULL ||
        mdssainfo->readVOpndSet()->is_empty()) {
        return false;
    }
    bool change = false;
    VOpndSetIter iter = NULL;
    MD const* mustuse = x->getRefMD();
    for (INT i = mdssainfo->readVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->readVOpndSet()->get_next(i, &iter)) {
        VOpnd const* t = m_mdssamgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        MDDef * tdef = ((VMD*)t)->getDef();
        if (tdef == NULL) { continue; }
        if (tdef->is_phi()) {
            //TODO: iter phi.
            change |= collectAllDefThroughDefChain(tdef, pwlst2);
            continue;
        }

        IR const* defstmt = tdef->getOcc();
        ASSERT0(defstmt);
        if (defstmt->isCallStmt()) {
            //CASE:call()
            //        =USE            
            //Call is the only stmt that need to process specially.
            //Because it always is not dominated killing-def.
            change |= collectAllDefThroughDefChain(tdef, pwlst2);
            continue;
        }

        MD const* mustdef = defstmt->getRefMD();
        if (mustuse != NULL &&
            mustdef != NULL &&
            mustuse->is_exact() &&
            mustdef->is_exact()) {
            if (mustdef == mustuse || mustdef->is_exact_cover(mustuse)) {
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

        if (mustuse != NULL) {
            //TODO:
            //CASE1:DEF=
            //         =USE            
            //CASE2:???=
            //         =USE
            //Both cases need to collect all DEFs until
            //the dominated killing-def.
            change |= collectAllDefThroughDefChain(tdef, pwlst2);
            continue;
        }

        //CASE1:???=
        //         =???
        //CASE2:DEF=
        //         =???
        //Both cases need to collect all DEFs through def-chain.
        change |= collectAllDefThroughDefChain(tdef, pwlst2);        
    }
    return change;
}


bool DeadCodeElim::collectByDU(IR const* x, IN OUT List<IR const*> * pwlst2)
{
    ASSERT0(x->is_exp());
    DUSet const* defs = x->readDUSet();
    if (defs == NULL) { return false; }
    DUIter di = NULL;
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


bool DeadCodeElim::remove_ineffect_ir() const
{
    BBListIter ctbb = NULL;
    List<IRBB*> bblst;
    List<IRBB*> * bbl = m_rg->getBBList();
    List<C<IRBB*>*> ctlst;
    bool change = false;    
    for (IRBB * bb = bbl->get_head(&ctbb);
         bb != NULL; bb = bbl->get_next(&ctbb)) {
        IRListIter ctir = NULL;
        IRListIter next = NULL;
        bool tobecheck = false;
        for (BB_irlist(bb).get_head(&ctir), next = ctir;
             ctir != NULL; ctir = next) {
            IR * stmt = ctir->val();
            BB_irlist(bb).get_next(&next);
            if (!m_is_stmt_effect.is_contain(IR_id(stmt))) {
                //Revise PRSSA info if PR is in SSA form.
                stmt->removeSSAUse();

                //Could not just remove the SSA def,
                //you should consider the SSA_uses
                //and make sure they are all removable.
                //Use SSA form related api.
                //ASSERT0(stmt->getSSAInfo() == NULL);

                //Revise DU chains.
                //TODO: If ssa form is available, it doesn't need to maintain
                //DU chain of PR in DU manager counterpart.
                m_du->removeIROutFromDUMgr(stmt);

                if (stmt->isConditionalBr() ||
                    stmt->isUnconditionalBr() ||
                    stmt->isMultiConditionalBr()) {
                    revise_successor(bb, ctbb, bbl);
                }

                if (m_mdssamgr != NULL) {                    
                    m_mdssamgr->removeMDSSAUse(stmt);
                }

                BB_irlist(bb).remove(ctir);
                m_rg->freeIRTree(stmt);
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
void DeadCodeElim::iter_collect(IN OUT List<IR const*> & work_list)
{
    List<IR const*> work_list2;
    List<IR const*> * pwlst1 = &work_list;
    List<IR const*> * pwlst2 = &work_list2;
    bool change = true;
    List<IRBB*> succs;
    while (change) {
        change = false;
        for (IR const* ir = pwlst1->get_head();
             ir != NULL; ir = pwlst1->get_next()) {
            m_citer.clean();
            for (IR const* x = iterRhsInitC(ir, m_citer);
                 x != NULL; x = iterRhsNextC(m_citer)) {
                if (!x->isMemoryOpnd()) { continue; }
                if (x->isReadPR() && PR_ssainfo(x) != NULL) {
                    change |= collectByPRSSA(x, pwlst2);
                    continue;
                }
                if (m_mdssamgr->getMDSSAInfoIfAny(x) != NULL) {
                    change |= collectByMDSSA(x, pwlst2);
                    continue;
                }
                change |= collectByDU(x, pwlst2);
            }
        }

        //dumpIRList((IRList&)*pwlst2);
        if (m_is_elim_cfs) {
            change |= preserve_cd(*pwlst2);
        }

        //dumpIRList((IRList&)*pwlst2);
        pwlst1->clean();
        List<IR const*> * tmp = pwlst1;
        pwlst1 = pwlst2;
        pwlst2 = tmp;
    } //end while
}


//Fix control flow if BB is empty.
//It will be illegal if empty BB has non-taken branch.
void DeadCodeElim::fix_control_flow(List<IRBB*> & bblst,
                                    List<C<IRBB*>*> & ctlst)
{
    BBList * bbl = m_rg->getBBList();
    BBListIter ct = ctlst.get_head();
    BBListIter bbct;
    for (bblst.get_head(&bbct); bbct != bblst.end();
         bbct = bblst.get_next(bbct), ct = ctlst.get_next()) {
        IRBB * bb = bbct->val();
        ASSERT0(ct && bb);
        if (BB_irlist(bb).get_elem_count() != 0) { continue; }

        xcom::EdgeC * vout = VERTEX_out_list(m_cfg->getVertex(BB_id(bb)));
        if (vout == NULL || cnt_list(vout) <= 1) { continue; }

        BBListIter next_ct = ct;
        bbl->get_next(&next_ct);
        IRBB * next_bb = NULL;
        if (next_ct != NULL) {
            next_bb = next_ct->val();
        }

        while (vout != NULL) {
            xcom::Edge * e = EC_edge(vout);
            if (EDGE_info(e) != NULL && CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e))) {
                vout = EC_next(vout);
                continue;
            }

            xcom::Vertex * s = EDGE_to(e);
            if (s->id() == BB_id(bb) ||
                (next_bb != NULL && s->id() == BB_id(next_bb))) {
                vout = EC_next(vout);
                continue;
            }

            if (!m_cdg->is_cd(BB_id(bb), s->id())) {
                //See dce.c:lexrun(), bb5 control bb6, but not control bb8.
                //if bb5 is empty, insert goto to bb8.
                IRBB * tgt = m_cfg->getBB(s->id());
                ASSERT0(tgt);

                //Find a normal label as target.
                LabelInfo const* li;
                for (li = tgt->getLabelList().get_head();
                     li != NULL; li = tgt->getLabelList().get_next()) {
                    if (LABEL_INFO_is_catch_start(li) ||
                        LABEL_INFO_is_try_start(li) ||
                        LABEL_INFO_is_try_end(li) ||
                        LABEL_INFO_is_pragma(li)) {
                        continue;
                    }
                    break;
                }
                ASSERT0(li);

                IR * g = m_rg->buildGoto(li);
                BB_irlist(bb).append_tail(g);
                bool change = true;
                xcom::Vertex * bbv = m_cfg->getVertex(BB_id(bb));
                while (change) {
                    xcom::EdgeC * ec = VERTEX_out_list(bbv);
                    change = false;
                    while (ec != NULL) {
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


//Fix control flow if BB is empty.
//It is illegal if empty BB has non-taken branch.
void DeadCodeElim::revise_successor(IRBB * bb,
                                    BBListIter bbct,
                                    BBList * bbl) const
{
    ASSERT0(bb && bbct);
    xcom::EdgeC * ec = VERTEX_out_list(m_cfg->getVertex(BB_id(bb)));
    if (ec == NULL) { return; }

    BBListIter next_ct = bbct;
    bbl->get_next(&next_ct);
    IRBB * next_bb = NULL;
    if (next_ct != NULL) {
        next_bb = next_ct->val();
    }

    while (ec != NULL) {
        xcom::Edge * e = EC_edge(ec);
        if (EDGE_info(e) != NULL && CFGEI_is_eh((CFGEdgeInfo*)EDGE_info(e))) {
            ec = EC_next(ec);
            continue;
        }

        IRBB * succ_bb = m_cfg->getBB(e->to()->id());
        ASSERT0(succ_bb);

        if (succ_bb != next_bb) {
            bb->removeSuccessorPhiOpnd(m_cfg);
        }

        xcom::EdgeC * next_ec = EC_next(ec);
        ((xcom::Graph*)m_cfg)->removeEdge(e);
        ec = next_ec;
    }

    if (next_bb != NULL) {
        m_cfg->addEdge(BB_id(bb), BB_id(next_bb));
    }
}


void DeadCodeElim::reinit()
{
    UINT irnum = m_rg->getIRVec()->get_elem_count() / BITS_PER_BYTE + 1;
    if (m_is_stmt_effect.get_byte_size() <= irnum) {
        m_is_stmt_effect.alloc(irnum + 1);
    }

    UINT bbnum = m_rg->getBBList()->get_elem_count() / BITS_PER_BYTE + 1;
    if (m_is_bb_effect.get_byte_size() <= bbnum) {
        m_is_bb_effect.alloc(bbnum + 1);
    }
}


//An aggressive algo will be used if cdg is avaliable.
bool DeadCodeElim::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == NULL || bbl->get_elem_count() == 0) { return false; }
    if (!OC_is_ref_valid(oc)) { return false; }
    //Update object pointer every time.
    m_mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_MD_SSA_MGR);
    PRSSAMgr * m_prssamgr = (PRSSAMgr*)m_rg->getPassMgr()->queryPass(
        PASS_PR_SSA_MGR);
    if ((!OC_is_pr_du_chain_valid(oc) &&
         (m_prssamgr == NULL || !m_prssamgr->isSSAConstructed()))) {
        //DCE use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if ((!OC_is_nonpr_du_chain_valid(oc) &&
         (m_mdssamgr == NULL || !m_mdssamgr->isMDSSAConstructed()))) {
        //DCE use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    START_TIMER(t, getPassName());
    if (m_is_elim_cfs) {
        m_rg->checkValidAndRecompute(&oc, PASS_CDG, PASS_PDOM,
            PASS_CDG, PASS_UNDEF);
        m_cdg = (CDG*)m_rg->getPassMgr()->registerPass(PASS_CDG);
    } else {
        m_rg->checkValidAndRecompute(&oc, PASS_PDOM, PASS_UNDEF);
        m_cdg = NULL;
    }
   
    bool change = false;
    bool removed = false;
    reinit(); 
    do {
        removed = false;
        //Mark effect IRs.
        List<IR const*> work_list;
        mark_effect_ir(work_list);
        iter_collect(work_list);
        removed = remove_ineffect_ir();
        if (removed) {
            change = true;
            m_is_stmt_effect.clean();
            m_is_bb_effect.clean();

            //AA, DU chain and DU reference are maintained.
            ASSERT0(m_rg->verifyMDRef() &&
                m_du->verifyMDDUChain(DUOPT_COMPUTE_PR_DU | DUOPT_COMPUTE_NONPR_DU));
            if (m_prssamgr != NULL && m_prssamgr->isSSAConstructed()) {
                ASSERT0(verifySSAInfo(m_rg));
            }
            if (m_mdssamgr != NULL && m_mdssamgr->isMDSSAConstructed()) {
                ASSERT0(verifyMDSSAInfo(m_rg));
            }
            //Remove empty BB.
            if (m_cfg->performMiscOpt(oc)) {
                if (m_mdssamgr != NULL && m_mdssamgr->isMDSSAConstructed()) {
                    m_mdssamgr->prunePhi();
                }
                if (m_prssamgr != NULL && m_prssamgr->isSSAConstructed()) {
                    m_prssamgr->refinePhi();
                }
            }
            ASSERT0(verifyMDSSAInfo(m_rg));
        }
        //fix_control_flow(bblst, ctlst);
    } while (removed);

    if (g_is_dump_after_pass && g_dump_opt.isDumpDCE()) {
        dump();
    }

    if (!change) {
        END_TIMER(t, getPassName());
        return false;
    }

    //AA, DU chain and DU reference are maintained.
    ASSERT0(m_rg->verifyMDRef() &&
            m_du->verifyMDDUChain(DUOPT_COMPUTE_PR_DU | DUOPT_COMPUTE_NONPR_DU));
    OC_is_expr_tab_valid(oc) = false;
    OC_is_live_expr_valid(oc) = false;
    OC_is_reach_def_valid(oc) = false;
    OC_is_avail_reach_def_valid(oc) = false;
    if (m_prssamgr != NULL && m_prssamgr->isSSAConstructed()) {
        ASSERT0(verifySSAInfo(m_rg));
    }
    if (m_mdssamgr != NULL && m_mdssamgr->isMDSSAConstructed()) {
        ASSERT0(verifyMDSSAInfo(m_rg));
    }
    END_TIMER(t, getPassName());
    return true;
}
//END DeadCodeElim

} //namespace xoc
