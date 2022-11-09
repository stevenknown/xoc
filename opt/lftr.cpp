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

//
//START LFAnaCtx
//
LFAnaCtx::LFAnaCtx(LI<IRBB> const* li)
{
    m_li = li;
    m_pool = smpoolCreate(sizeof(LFRInfo) * 4, MEM_COMM);
}


LFRInfo * LFAnaCtx::allocLFRInfo()
{
    return (LFRInfo*)xmalloc(sizeof(LFRInfo));
}


void LFAnaCtx::setMapMDAndIV(MD const* ivmd, IV const* iv)
{
    if (m_ivmd2ivinfo.get(ivmd) == nullptr) {
        m_ivmd2ivinfo.set(ivmd, iv);
    } else {
        ASSERT0(m_ivmd2ivinfo.get(ivmd) == iv);
    }
}


LFRInfo * LFAnaCtx::genLFRInfo(IR const* lf_exp)
{
    LFRInfo * info = m_cand_occ2info.get(lf_exp);
    if (info == nullptr) {
        info = allocLFRInfo();
        setMapExpToLFRInfo(lf_exp, info);
    }
    return info;
}


void LFAnaCtx::dump(Region * rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- LINEAR FUNC CAND LIST of LI%d --==", m_li->id());
    IRListIter it;
    for (IR const* x = getCandList().get_head(&it); x != nullptr;
         x = getCandList().get_next(&it)) {
        dumpIR(x, rg);
    }
}
//END LFAnaCtx


void LFTR::analyzeBB(LI<IRBB> const* li, IRBB const* bb, MOD LFAnaCtx & ctx)
{
    BBIRListIter it;
    for (IR * ir = const_cast<IRBB*>(bb)->getIRList().get_head(&it);
         ir != nullptr;
         ir = const_cast<IRBB*>(bb)->getIRList().get_next(&it)) {
        if (ir->isNoMove(true) || ir->is_phi() || ir->isDummyOp()) {
            continue;
        }

        //Check whether all RHS are loop invariants.
        m_iriter.clean();
        for (IR * x = xoc::iterInit(ir, m_iriter);
             x != nullptr; x = xoc::iterNext(m_iriter)) {
            if (!x->is_mul() || x->hasSideEffect(true) || x->isNoMove(true) ||
                x->isDummyOp()) {
                continue;
            }

            IV const* iv = nullptr;
            IR const* op0 = BIN_opnd0(x);
            IR const* op1 = BIN_opnd1(x);
            if (op0->is_ld() && m_ivr->isIV(li, op0, &iv)) {
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

            ASSERT0(op0->getMustRef());
            ASSERT0(iv && iv->getLI() == li);
            ctx.setMapMDAndIV(op0->getMustRef(), iv);
            ctx.addCandLF(x);
        }
    }
}


void LFTR::analysis(LI<IRBB> * li, MOD LFAnaCtx & ctx)
{
    IRBB * head = li->getLoopHead();
    UINT headid = head->id();
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        if (i != (BSIdx)headid && !m_cfg->is_dom(headid, i)) {
            //Loop head should anticipate into analysis as well.
            //The candidate BB must dominate all other loop body BBs.
            continue;
        }

        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && bb->getVex());
        analyzeBB(li, bb, ctx);
    }
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
        LFAnaCtx ctx(tli);
        analysis(tli, ctx);
        if (ctx.getCandList().get_elem_count() == 0) { continue; }

        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpLFTR()) {
            dump(tli, ctx);
        }

        IRBB * preheader = nullptr;
        if (xoc::insertPreheader(tli, m_rg, &preheader, &oc, false)) {
            m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM,
                                                       PASS_LOOP_INFO,
                                                       PASS_UNDEF);
            insert_bb = true;
        }

        bool li_insert_bb = false;
        bool replaced = doReplacement(preheader, tli, li_insert_bb, ctx);
        if (!replaced && !li_insert_bb) {
            //Nothing changed.
            continue;
        }

        du_set_info_changed |= replaced;
        changed |= du_set_info_changed;
        insert_bb |= li_insert_bb;

        //doReplacement may append stmt into BB which has down-boundary stmt.
        //That makes BB invalid. Split such invalid BB into two or more BBs.
        bool splitted = m_cfg->splitBBIfNeeded(preheader, oc);
        ASSERTN(!splitted, ("Does this happen?"));
        //if (splitted) {
        //    //TODO:maintain CFG related info.
        //    //oc.setInvalidIfCFGChanged();
        //    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM,
        //                                               PASS_UNDEF);
        //}
        //insert_bb |= splitted;
        
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
        newiv = m_irmgr->buildPRdedicated(info->new_div->getPrno(), ty);
        m_rg->getMDMgr()->allocRef(newiv);
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
            m_dumgr->addUseForTree(newiv, info->new_div);
        }
    }
    ASSERT0(newiv->is_pr());
    return newiv;
}


void LFTR::getLinearRepOfIV(LFAnaCtx const& ctx, IR const* lf_exp,
                            IV const** iv, IR const** coeff)
{
    ASSERT0(iv && coeff);
    ASSERT0(lf_exp->is_mul() && !lf_exp->hasSideEffect(true) &&
            !lf_exp->isNoMove(true) && !lf_exp->isDummyOp());
    IR const* op0 = BIN_opnd0(lf_exp);
    ASSERT0(op0->is_ld() && op0->getMustRef());
    *iv = ctx.getIVInfo(op0->getMustRef());
    ASSERT0(*iv);
    *coeff = BIN_opnd1(lf_exp);
}


void LFTR::pickupProperCandidate(OUT List<LFRInfo*> & lfrinfo_list,
                                 MOD LFAnaCtx & ctx)
{
    List<IR*> init_val_list;
    IRListIter it;
    for (IR * cand = ctx.getCandList().get_head(&it);
         cand != nullptr; cand = ctx.getCandList().get_next(&it)) {
        //TODO: support computing step of DIV.
        IV const* ivinfo = nullptr;
        IR const* coeff = nullptr;
        getLinearRepOfIV(ctx, cand, &ivinfo, &coeff);
        ASSERT0(ivinfo && coeff);
        ASSERTN(coeff->is_int(), ("TODO:only support integer type now"));
        if (!coeff->is_const()) {
            //TODO:support variadic coeff.
            continue;
        }
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
            LFRInfo * info = ctx.getLFRInfo(equal_exp);
            ASSERT0(info);
            ctx.setMapExpToLFRInfo(cand, info);
            continue;
        }

        LFRInfo * info = ctx.genLFRInfo(cand);
        ASSERT0(!info->is_init());
        info->init(cand, ivinfo, coeff);
        lfrinfo_list.append_tail(info);
    }
}


void LFTR::addDUChainForRHSOfInitDef(IR * newrhs, IR const* oldrhs,
                                     LI<IRBB> const* li)
{
    if (!useMDSSADU()) { return; }
    ASSERT0(newrhs->isIREqual(oldrhs, true));
    IRIter itnew;
    ConstIRIter itold;
    IR * x;
    IR const* y;
    for (x = iterInit(newrhs, itnew), y = iterInitC(oldrhs, itold);
         x != nullptr; x = iterNext(itnew), y = iterNextC(itold)) {
        if (x->isMemRefNonPR()) {
            ASSERT0(y && y->isMemRefNonPR());
            MDSSAInfo const* info = m_mdssamgr->getMDSSAInfoIfAny(y);
            ASSERT0(info);
            m_mdssamgr->genMDSSAInfoToOutsideLoopDef(x, info, li);
        }
    }
}


bool LFTR::insertComputingInitValCode(IRBB * preheader,
                                      List<LFRInfo*> const& lfrinfo_list,
                                      LI<IRBB> const* li)
{
    C<LFRInfo*> * it;
    bool changed = false;
    for (LFRInfo * info = lfrinfo_list.get_head(&it);
         info != nullptr; info = lfrinfo_list.get_next(&it)) {
        ASSERTN(info->ivinfo->getLI() == li,
                ("IV is not belong to LI%d", li->id()));
        //Build stmt to compute init-val at preheader.
        IR * newrhs = m_rg->dupIRTree(info->lf_exp);
        IR * comp_init_val = m_irmgr->buildStorePR(info->lf_exp->getType(),
                                                newrhs);
        m_rg->getMDMgr()->allocRef(comp_init_val);
        addDUChainForRHSOfInitDef(newrhs, info->lf_exp, li);
        preheader->getIRList().append_tail_ex(comp_init_val);

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
    IR * iv_occ = red->getRHS()->getOpndMem(red->getMustRef());
    ASSERT0(iv_occ && iv_occ->is_pr() && iv_occ->getPrno() == red->getPrno());

    m_dumgr->buildDUChain(red, iv_occ);
    m_dumgr->buildDUChain(init, iv_occ);
}


static void findInitAndRedPos(Vertex const* headvex, IR const* init,
                              IR const* red, UINT & pos_of_initbb,
                              UINT & pos_of_redbb)
{
    bool try_failed;
    //Because init-value BB may not be the immeidate-predecessor of head,
    //we need to find which predecessor to heavex that init-value livein.
    if (Graph::isReachIn(headvex, init->getBB()->getVex(), 100, try_failed)) {
        ASSERT0(Graph::isReachIn(headvex->getNthInVertex(1),
                                 red->getBB()->getVex(), 100, try_failed));
        pos_of_initbb = 0;
        pos_of_redbb = 1;
        return;
    }
    ASSERT0(Graph::isReachIn(headvex, red->getBB()->getVex(), 100,
            try_failed));
    pos_of_initbb = 1;
    pos_of_redbb = 0;
}


//Generate SSAInfo & Build DU chain.
static void createPRSSADU(Region * rg, PRSSAMgr * prssamgr, IR * phi,
                          IR * iv_occ, IR * red, IR * init,
                          IR * opnd_relate_to_red, IR * opnd_relate_to_init)
{
    iv_occ->setPrnoAndUpdateSSAInfo(phi->getPrno());

    //Build DU chain between phi and iv.
    prssamgr->buildDUChain(phi, iv_occ);

    //Create new PR.
    red->setPrnoAndUpdateSSAInfo(rg->getIRMgr()->buildPrno(red->getType()));

    //Creat new PR for phi operand.
    opnd_relate_to_red->setPrnoAndUpdateSSAInfo(red->getPrno());

    //Build DU chain between red/init and phi operand.
    prssamgr->buildDUChain(red, opnd_relate_to_red);
    prssamgr->buildDUChain(init, opnd_relate_to_init);
}


//Insert PHI at head BB of loop for given reduction operation.
//init: initial value stmt.
//red: reduction stmt that is inserted at loop.
//Return the inserted PHI.
IR * LFTR::insertPhiForRed(LI<IRBB> const* li, IR * init, IR * red)
{
    ASSERT0(li && red->is_stpr() && init->is_stpr());
    IRBB * head = li->getLoopHead();
    ASSERT0(head);
    IR const* defarr[2];
    IR * opndarr[2];
    ASSERTN(m_cfg->getPredsNum(head) == 2,
            ("TODO: support loop head with more predecessors"));

    Vertex const* headvex = head->getVex();
    ASSERT0(headvex);
    UINT pos_of_initbb;
    UINT pos_of_redbb;
    findInitAndRedPos(headvex, init, red, pos_of_initbb, pos_of_redbb);

    //Note redbb may be head itself.
    //CASE:BB1->BB2->BB3->BB5
    //           ^___/
    //  BB2 is either redbb and head.
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
        PRNO prno = defarr[i]->getPrno();
        ASSERT0(prno != PRNO_UNDEF);
        IR * opnd = m_irmgr->buildPRdedicated(prno,
                                                       defarr[i]->getType());
        m_rg->getMDMgr()->allocRef(opnd);
        xcom::add_next(&opnd_list, opnd);
        opndarr[i] = opnd;
    }
    IR * phi = m_irmgr->buildPhi(red->getPrno(), red->getType(),
                                          opnd_list);
    head->getIRList().append_head(phi);

    //Renaming.
    //e.g:PR_1 = ...
    //    PR_1 = PHI(PR_1, PR_1)
    //    PR_1 = PR_1, ...
    //after renaming:
    //    PR_init = ...
    //    PR_phi = PHI(PR_init, PR_red)
    //    PR_red = PR_phi, ...
    phi->setPrno(m_irmgr->buildPrno(phi->getType()));

    //Get RHS OCC of IV in reduction.
    IR * iv_occ = red->getRHS()->getOpndMem(red->getMustRef());
    ASSERT0(iv_occ && iv_occ->is_pr() && iv_occ->getPrno() == red->getPrno());

    IR * opnd_relate_to_red = opndarr[pos_of_redbb];
    IR * opnd_relate_to_init = opndarr[pos_of_initbb];
    createPRSSADU(m_rg, m_prssamgr, phi, iv_occ, red, init, opnd_relate_to_red,
                  opnd_relate_to_init);
    m_rg->getMDMgr()->allocRef(phi);
    m_rg->getMDMgr()->allocRef(iv_occ);
    m_rg->getMDMgr()->allocRef(red);
    m_rg->getMDMgr()->allocRef(opnd_relate_to_red);
    m_rg->getMDMgr()->allocRef(opnd_relate_to_init);
    return phi;
}


//Insert reduction code into proper BB.
//reduction code is form as: newiv = newiv + coeff*step.
bool LFTR::insertReductionCode(List<LFRInfo*> const& lfrinfo_list)
{
    bool changed = false;
    List<LFRInfo*>::Iter it;
    for (LFRInfo * info = lfrinfo_list.get_head(&it);
         info != nullptr; info = lfrinfo_list.get_next(&it)) {
        IR * newiv = genNewIVRef(info);
        ASSERT0(newiv->is_pr());
        ASSERTN(info->ivinfo->is_biv(), ("TODO: support DIV"));
        BIV const* ivinfo = info->getBIVInfo();
        HOST_INT newiv_step = ivinfo->getStep() * info->getIVCoeff();
        IR_CODE op = IR_UNDEF;
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
        IR * red = m_irmgr->buildStorePR(newiv->getPrno(), ty,
            m_irmgr->buildBinaryOp(op, ty, newiv,
                                   m_irmgr->buildImmInt(newiv_step, ty)));
        m_rg->getMDMgr()->allocRef(red);
        IRBB * redbb = ivinfo->getRedStmt()->getBB();
        ASSERT0(redbb);
        redbb->getIRList().insert_before(red, ivinfo->getRedStmt());
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
bool LFTR::replaceCandByIV(LFAnaCtx const& ctx)
{
    LFRInfoMapIter it;
    LFRInfo * info;
    bool changed = false;
    for (IR const* cand = ctx.getLFRInfoMap().get_first(it, &info);
         info != nullptr; cand = ctx.getLFRInfoMap().get_next(it, &info)) {
        ASSERT0(cand == info->lf_exp);
        ASSERT0(info->lf_exp->is_exp());
        info->lf_exp->getStmt()->replaceKid(info->lf_exp, genNewIVRef(info),
                                            true);
        xoc::removeUseForTree(info->lf_exp, m_rg, *getOptCtx());
        changed = true;
    }
    return changed;
}


//doReplacement candidate IRs to preheader BB.
//This function will maintain RPO if new BB inserted.
//Return true if BB or STMT changed.
bool LFTR::doReplacement(OUT IRBB * preheader, OUT LI<IRBB> * li,
                         OUT bool & insert_bb, MOD LFAnaCtx & ctx)
{
    List<LFRInfo*> lfrinfo_list;
    pickupProperCandidate(lfrinfo_list, ctx);
    bool changed = insertComputingInitValCode(preheader, lfrinfo_list, li);
    changed |= insertReductionCode(lfrinfo_list);
    changed |= replaceCandByIV(ctx);
    return changed;
}


bool LFTR::dump(LI<IRBB> const* li, LFAnaCtx const& ctx) const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    ctx.dump(getRegion());
    return Pass::dump();
}


bool LFTR::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }

    if (!oc.is_ref_valid()) { return false; }
    m_mdssamgr = m_rg->getMDSSAMgr();
    m_prssamgr = m_rg->getPRSSAMgr();
    m_dumgr = m_rg->getDUMgr();
    m_irmgr = m_rg->getIRMgr();

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

    DumpBufferSwitch buff(m_rg->getLogMgr());
    bool change = doLoopTree(m_cfg->getLoopInfo(), du_set_info_changed,
                             insert_bb, oc);
    if (!change) {
        m_rg->getLogMgr()->cleanBuffer();
        END_TIMER(t, getPassName());
        return false;
    }
    //This pass does not devastate IVR information. However, new IV might be
    //inserted.
    //DU chain and DU reference should be maintained.
    ASSERT0(m_dumgr->verifyMDRef() && verifyMDDUChain(m_rg, oc));
    oc.setInvalidIfDUMgrLiveChanged();
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(m_rg));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(m_rg, oc));
    END_TIMER(t, getPassName());
    return true;
}

} //namespace xoc
