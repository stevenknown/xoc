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

//doReplacement may append stmt into BB which has down-boundary stmt.
//That makes BB invalid. Split such invalid BB into two or more BBs.
bool LFTR::splitBBIfNeeded(IRBB * bb)
{
    IRListIter it;
    for (bb->getIRList()->get_head(&it); it != nullptr;) {
        IRListIter cur = it;
        bb->getIRList()->get_next(&it);
        if (IRBB::isLowerBoundary(cur->val()) && it != nullptr) {
            m_cfg->splitBB(bb, cur);
            return true;
        }
    }
    return false;
}


void LFTR::analyzeBB(LI<IRBB> * li, IRBB * bb)
{
    for (IR * ir = bb->getFirstIR(); ir != nullptr; ir = bb->getNextIR()) {
        if (ir->isNoMove() || ir->is_phi()) {
            continue;
        }

        //Check whether all RHS are loop invariants.
        m_iriter.clean();
        for (IR * x = iterInit(ir, m_iriter);
            x != nullptr; x = iterNext(m_iriter)) {
            if (!x->is_mul() || x->hasSideEffect() || x->isNoMove()) {
                continue;
            }

            IV const* iv = nullptr;
            IR const* op0 = BIN_opnd0(x);
            IR const* op1 = BIN_opnd1(x);
            if (op0->is_ld() && m_ivr->isIV(op0, &iv)) {
                ;
            } else {
                continue;
            }

            if ((op1->is_ld() &&
                xoc::isLoopInvariant(op1, li, m_rg, nullptr, false)) ||
                op1->is_const()) {
                ;
            } else {
                continue;
            }

            ASSERT0(op0->getRefMD());
            ASSERT0(iv);
            if (m_ivmd2ivinfo.get(op0->getRefMD()) == nullptr) {
                m_ivmd2ivinfo.set(op0->getRefMD(), iv);
            } else {
                ASSERT0(m_ivmd2ivinfo.get(op0->getRefMD()) == iv);
            }
            m_cand_list.append_tail(x);
        }
    }
}


void LFTR::analysis(LI<IRBB> * li)
{
    IRBB * head = li->getLoopHead();
    UINT headid = head->id();
    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        if (i != (INT)headid && !m_cfg->is_dom(headid, i)) {
            //Loop head should anticipate into analysis as well.
            //The candidate BB must dominate all other loop body BBs.
            continue;
        }

        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->getVertex(i));
        analyzeBB(li, bb);
    }
}


void LFTR::clean()
{
    m_cand_list.clean();
    m_cand_occ2info.clean();
    m_ivmd2ivinfo.clean();
}


//Return true if code changed.
bool LFTR::doLoopTree(LI<IRBB> * li, OUT bool & du_set_info_changed,
                      OUT bool & insert_bb, OptCtx & oc)
{
    if (li == nullptr) { return false; }
    bool changed = false;
    for (LI<IRBB> * tli = li; tli != nullptr; tli = tli->get_next()) {
        changed |= doLoopTree(tli->getInnerList(), du_set_info_changed,
                              insert_bb, oc);
        clean();
        analysis(tli);
        if (m_cand_list.get_elem_count() == 0) { continue; }

        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLFTR()) {
            dump(tli);
        }

        IRBB * preheader = nullptr;
        if (xoc::insertPreheader(tli, m_rg, &preheader)) {
            //Recompute DOM related info.
            oc.setDomValid(false);
            OC_is_cdg_valid(oc) = false;
            OC_is_rpo_valid(oc) = false;
            m_cfg->computeDomAndIdom(oc);
            insert_bb = true;
        }

        bool inserted3 = false;
        bool replaced = doReplacement(preheader, tli, inserted3);
        if (!replaced && !inserted3) {
            //Nothing changed.
            continue;
        }

        du_set_info_changed |= replaced;
        changed |= du_set_info_changed;
        insert_bb |= inserted3;

        bool inserted2 = splitBBIfNeeded(preheader);
        ASSERTN(!inserted2, ("Does this happen?"));
        insert_bb |= inserted2;
        if (inserted2) {
            //Recompute DOM related info.
            oc.setDomValid(false);
            OC_is_cdg_valid(oc) = false;
            OC_is_rpo_valid(oc) = false;
            m_cfg->computeDomAndIdom(oc);
        }
    }
    return changed;
}


IR * LFTR::genNewIVRef(LFRInfo const* info)
{
    IR * newiv = nullptr;
    ASSERT0(info->new_div->isPROp());
    Type const* ty = info->new_div->getType();
    if (info->new_div->is_stmt()) {
        ASSERTN(info->new_div->is_stpr() || info->new_div->is_phi(), ("TODO"));
        newiv = m_rg->buildPRdedicated(info->new_div->getPrno(), ty);
        m_rg->allocRefForPR(newiv);
        if (usePRSSADU()) {
            m_prssamgr->buildDUChain(info->new_div, newiv);
        } else {
            //Add classic PRDU.
            m_dumgr->buildDUChain(info->new_div, newiv);
        }
    } else {
        ASSERT0(info->new_div->is_pr());
        newiv = m_rg->dupIRTree(info->new_div);
        if (usePRSSADU()) {
            SSAInfo * ssainfo = PR_ssainfo(info->new_div);
            ASSERT0(ssainfo);
            m_prssamgr->buildDUChain(ssainfo->getDef(), newiv);
        } else {
            //Add classic PRDU.
            m_dumgr->addUse(newiv, info->new_div);
        }
    }
    ASSERT0(newiv->is_pr());
    return newiv;
}


void LFTR::getLinearRepOfIV(IR const* lf_exp, IV const** iv, IR const** coeff)
{
    ASSERT0(iv && coeff);
    ASSERT0(lf_exp->is_mul() && !lf_exp->hasSideEffect() &&
            !lf_exp->isNoMove());
    IR const* op0 = BIN_opnd0(lf_exp);
    ASSERT0(op0->is_ld() && op0->getRefMD());
    *iv = m_ivmd2ivinfo.get(op0->getRefMD());
    ASSERT0(*iv);
    *coeff = BIN_opnd1(lf_exp);
    ASSERT0((*coeff)->is_const());
}


void LFTR::pickupProperCandidate(OUT List<LFRInfo*> & lfrinfo_list)
{
    List<IR*> init_val_list;
    for (IR * cand = m_cand_list.get_head();
         cand != nullptr; cand = m_cand_list.get_next()) {
        //TODO: support computing step of DIV.
        IV const* ivinfo = nullptr;
        IR const* coeff = nullptr;
        getLinearRepOfIV(cand, &ivinfo, &coeff);
        ASSERT0(ivinfo && coeff);
        ASSERTN(coeff->is_int(), ("TODO:only support integer currently"));

        if (!ivinfo->is_biv()) {
            //TODO: support DIV and compute the step value of DIV.
            continue;
        }

        //Find if there exist same init-val computation expression.
        IR const* equal_exp = nullptr;
        for (IR const* e = init_val_list.get_head();
             e != nullptr; e = init_val_list.get_next()) {
            if (cand->isIREqual(e, true)) {
                equal_exp = e;
                break;
            }
        }
        if (equal_exp != nullptr) {
            LFRInfo * info = m_cand_occ2info.get(equal_exp);
            ASSERT0(info);
            m_cand_occ2info.set(cand, info);
            continue;
        }

        LFRInfo * info = genLFRInfo(cand);
        ASSERT0(!info->is_init());
        info->lf_exp = cand;
        info->ivinfo = ivinfo;
        info->ivcoeff = coeff;
        lfrinfo_list.append_tail(info);
    }
}


bool LFTR::insertComputingInitValCode(IRBB * preheader,
                                      List<LFRInfo*> const& lfrinfo_list)
{
    C<LFRInfo*> * it;
    bool changed = false;
    for (LFRInfo * info = lfrinfo_list.get_head(&it);
         info != nullptr; info = lfrinfo_list.get_next(&it)) {
        //Build stmt to compute init-val at preheader.
        IR * newrhs = m_rg->dupIRTree(info->lf_exp);
        IR * comp_init_val = m_rg->buildStorePR(info->lf_exp->getType(),
                                                newrhs);
        m_rg->allocRefForPR(comp_init_val);
        xoc::addUseForTree(newrhs, info->lf_exp, m_rg);
        preheader->getIRList()->append_tail_ex(comp_init_val);

        //Record initialization of IV.
        info->init_stmt = comp_init_val;
        info->new_div = comp_init_val;
        changed = true;
    }
    return changed;
}


void LFTR::buildClassicDUForRed(IR * init, IR * red)
{
    ASSERT0(red->is_stpr() && init->is_stpr());
    if (m_dumgr == nullptr) { return; }

    //Get RHS OCC of IV in reduction.
    IR * iv_occ = red->getRHS()->getOpndMem(red->getRefMD());
    ASSERT0(iv_occ && iv_occ->is_pr() && iv_occ->getPrno() == red->getPrno());

    m_dumgr->buildDUChain(red, iv_occ);
    m_dumgr->buildDUChain(init, iv_occ);
}


//Insert PHI at head BB of loop for given reduction operation.
//init: initial value stmt.
//red: reduction stmt that is inserted at loop.
//Return PHI stmt that is inerted.
IR * LFTR::insertPhiForRed(LI<IRBB> const* li, IR * init, IR * red)
{
    ASSERT0(li && red->is_stpr() && init->is_stpr());
    IRBB * head = li->getLoopHead();
    ASSERT0(head);
    IR const* defarr[2];
    IR * opndarr[2];
    ASSERTN(m_cfg->getPredsNum(head) == 2,
            ("TODO: support loop head with more preds"));

    //Init-value BB do not have to be immeidate-predecessor of head, however
    //head BB should be reachable from init-value BB.
    UINT pos_of_redbb = m_cfg->WhichPred(red->getBB(), head);
    UINT pos_of_initbb = pos_of_redbb == 0 ? 1 : 0;
    defarr[pos_of_redbb] = red;
    defarr[pos_of_initbb] = init;

    //Insert phi.
    //e.g:PR_1 = ...
    //    PR_l = PR_1, ...
    //after insertion:
    //    PR_1 = ...
    //    PR_1 = PHI(PR_1, PR_1)
    //    PR_1 = PR_1, ...
    IR * opnd_list = nullptr;
    for (UINT i = 0; i < 2; i++) {
        UINT prno = defarr[i]->getPrno();
        ASSERT0(prno != PRNO_UNDEF);
        IR * opnd = m_rg->buildPRdedicated(prno, defarr[i]->getType());
        m_rg->allocRefForPR(opnd);
        xcom::add_next(&opnd_list, opnd);
        opndarr[i] = opnd;
    }
    IR * phi = m_rg->buildPhi(red->getPrno(), red->getType(), opnd_list);
    head->getIRList()->append_head(phi);

    //Renaming.
    //e.g:PR_1 = ...
    //    PR_1 = PHI(PR_1, PR_1)
    //    PR_1 = PR_1, ...
    //after renaming:
    //    PR_init = ...
    //    PR_phi = PHI(PR_init, PR_red)
    //    PR_red = PR_phi, ...
    phi->setPrno(m_rg->buildPrno(phi->getType()));
    m_rg->allocRefForPR(phi);

    //Get RHS OCC of IV in reduction.
    IR * iv_occ = red->getRHS()->getOpndMem(red->getRefMD());
    ASSERT0(iv_occ && iv_occ->is_pr() && iv_occ->getPrno() == red->getPrno());

    //Generate SSAInfo & Build DU chain.
    iv_occ->setPrnoConsiderSSAInfo(phi->getPrno());
    m_rg->allocRefForPR(iv_occ);
    //PR_ssainfo(iv_occ) = PHI_ssainfo(phi) =
    //    m_prssamgr->allocSSAInfo(phi->getPrno());
    m_prssamgr->buildDUChain(phi, iv_occ);

    red->setPrnoConsiderSSAInfo(m_rg->buildPrno(red->getType()));
    m_rg->allocRefForPR(red);

    IR * opnd = opndarr[m_cfg->WhichPred(red->getBB(), head)];
    opnd->setPrnoConsiderSSAInfo(red->getPrno());
    m_rg->allocRefForPR(opnd);
    //PR_ssainfo(opnd) = STPR_ssainfo(red) =
    //    m_prssamgr->allocSSAInfo(red->getPrno());
    m_prssamgr->buildDUChain(red, opnd);

    m_prssamgr->buildDUChain(init, opndarr[pos_of_initbb]);
    return phi;
}


//Insert reduction code into proper BB.
//reduction code is form as: newiv = newiv + coeff*step.
bool LFTR::insertReductionCode(List<LFRInfo*> & lfrinfo_list)
{
    bool changed = false;
    for (LFRInfo * info = lfrinfo_list.get_head();
         info != nullptr; info = lfrinfo_list.get_next()) {
        IR * newiv = genNewIVRef(info);
        ASSERT0(newiv->is_pr());
        ASSERTN(info->ivinfo->is_biv(), ("TODO: support DIV"));
        BIV const* ivinfo = info->getBIVInfo();
        HOST_INT newiv_step = ivinfo->getStep() * info->getIVCoeff();
        IR_TYPE op = IR_UNDEF;
        if (ivinfo->isInc()) {
            ASSERT0(newiv_step > 0);

            //The sign of Opcode has been substitute into step-constant.
            op = IR_ADD;
        } else {
            ASSERT0(newiv_step < 0);
            //The sign of Opcode has been substituted into step-constant.
            op = IR_ADD;
        }

        //Build stmt to do reduction in loop body.
        Type const* ty = info->new_div->getType();
        IR * red = m_rg->buildStorePR(newiv->getPrno(), ty,
                                      m_rg->buildBinaryOp(op, ty,
                                          newiv,
                                          m_rg->buildImmInt(newiv_step, ty)));
        m_rg->allocRefForPR(red);
        IRBB * redbb = ivinfo->getRedStmt()->getBB();
        ASSERT0(redbb);
        redbb->getIRList()->insert_before(red, ivinfo->getRedStmt());
        if (usePRSSADU()) {
            IR * phi = insertPhiForRed(ivinfo->getLI(), info->init_stmt, red);
            ASSERT0(phi);
            info->new_div = phi; //Change initial-stmt phi.
        } else {
            //Keep initial-stmt unchanged.
            //Classic PRDU.
            //Build DU chain for IV in reduction operation.
            buildClassicDUForRed(info->init_stmt, red);
        }
        changed = true;
    }
    return changed;
}


//Replace original candidate occurrence with new IV.
bool LFTR::replaceCandByIV()
{
    LFRInfoMapIter it;
    LFRInfo * info;
    bool changed = false;
    for (IR const* cand = m_cand_occ2info.get_first(it, &info);
         info != nullptr; cand = m_cand_occ2info.get_next(it, &info)) {
        ASSERT0(cand == info->lf_exp);
        ASSERT0(info->lf_exp->is_exp());
        info->lf_exp->getStmt()->replaceKid(info->lf_exp, genNewIVRef(info),
                                            true);
        xoc::removeUseForTree(info->lf_exp, m_rg);
        changed = true;
    }
    return changed;
}


//doReplacement candidate IRs to preheader BB.
//This function will maintain RPO if new BB inserted.
//Return true if BB or STMT changed.
bool LFTR::doReplacement(OUT IRBB * preheader, OUT LI<IRBB> * li,
                         OUT bool & insert_bb)
{
    List<LFRInfo*> lfrinfo_list;
    pickupProperCandidate(lfrinfo_list);
    bool changed = insertComputingInitValCode(preheader, lfrinfo_list);
    changed |= insertReductionCode(lfrinfo_list);
    changed |= replaceCandByIV();
    return changed;
}


bool LFTR::dump(LI<IRBB> const* li) const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());

    note(getRegion(), "\n==-- LINEAR FUNC CAND LIST of LI%d --==", li->id());
    IRListIter it;
    for (IR const* x = m_cand_list.get_head(&it); x != nullptr;
         x = m_cand_list.get_next(&it)) {
        dumpIR(x, m_rg);
    }
    return true;
}


bool LFTR::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }

    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    m_dumgr = m_rg->getDUMgr();

    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //LFTR use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //LFTR use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_LOOP_INFO,
                                               PASS_IVR, PASS_UNDEF);
    m_ivr = (IVR*)m_rg->getPassMgr()->queryPass(PASS_IVR);
    ASSERT0(m_ivr && m_ivr->is_valid());

    bool du_set_info_changed = false;
    bool insert_bb = false;

    clean();
    bool change = doLoopTree(m_cfg->getLoopInfo(), du_set_info_changed,
                             insert_bb, oc);
    if (!change) {
        END_TIMER(t, getPassName());
        return false;
    }

    //This pass does not devastate IVR information. However, new IV might be
    //inserted.
    //DU chain and DU reference should be maintained.
    ASSERT0(m_rg->verifyMDRef() && verifyMDDUChain(m_rg));
    oc.setInvalidIfDUMgrLiveChanged();
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg));
    END_TIMER(t, getPassName());
    return true;
}

} //namespace xoc
