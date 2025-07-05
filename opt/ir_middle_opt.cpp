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

void Region::lowerIRTreeToLowestHeight(OptCtx & oc)
{
    SimpCtx simp(&oc);
    if (g_is_lower_to_pr_mode) {
        simp.setSimpToPRmode();
    }
    if (g_do_prssa) {
        //Note if this flag enable,
        //AA may generate imprecise result.
        //TODO: use SSA info to improve the precision of AA.
        simp.setSimpLandLor();
        simp.setSimpLnot();
        simp.setSimpCFS();
    }
    //Simplify IR tree if it is needed.
    getIRSimp()->simplifyBBlist(getBBList(), &simp);
    if (SIMP_need_recon_bblist(&simp)) {
        //New BB boundary IR generated, rebuilding CFG.
        if (reconstructBBList(oc)) {
            getCFG()->rebuild(oc);
        }
    }
    if (!SIMP_changed(&simp)) { return; }
    oc.setInvalidPass(PASS_AA);
    oc.setInvalidPass(PASS_CLASSIC_DU_CHAIN);
    oc.setInvalidPass(PASS_REACH_DEF);
    oc.setInvalidPass(PASS_AVAIL_REACH_DEF);
}


void Region::postSimplify(MOD SimpCtx & simp, MOD OptCtx & oc)
{
    if (!g_do_cfg || !g_cst_bb_list || !simp.needReconstructBBList()) {
        return;
    }
    bool changed = reconstructBBList(oc);
    if (!changed) {
        ASSERT0((!g_do_md_du_analysis && !g_do_mdssa) ||
                getDUMgr()->verifyMDRef());
        return;
    }
    oc.setInvalidPass(PASS_CFG);
    oc.setInvalidIfCFGChanged();

    //Simplification may generate new memory operations.
    if (g_opt_level != OPT_LEVEL0) {
        //O0 does not build DU ref.
        ASSERT0(getDUMgr() && getDUMgr()->verifyMDRef());
    }
    bool need_rebuild_mdssa = false;
    bool need_rebuild_prssa = false;
    MDSSAMgr * mdssamgr = (MDSSAMgr*)getPassMgr()->queryPass(
        PASS_MDSSA_MGR);
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        need_rebuild_mdssa = true;
        mdssamgr->destruction(oc);
    }
    PRSSAMgr * prssamgr = (PRSSAMgr*)getPassMgr()->queryPass(
        PASS_PRSSA_MGR);
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        need_rebuild_prssa = true;
        prssamgr->destruction(oc);
    }

    //Before CFG rebuilding.
    IRCfgOptCtx ctx(&oc);
    RemoveEmptyBBCtx rmctx(ctx);
    getCFG()->removeEmptyBB(rmctx);
    getCFG()->rebuild(oc);
    ASSERT0(getCFG()->verify());
    if (need_rebuild_mdssa) {
        mdssamgr->construction(oc);
        oc.setInvalidIfMDSSAReconstructed();

        //CASE:Since MDSSA does not affect the real IR stmt and exp.
        //Here we prefer to do not invalid classic NonPRDU.
        //oc.setInvalidNonPRDU();
        SIMP_need_rebuild_nonpr_du_chain(&simp) = false;
    }
    if (need_rebuild_prssa) {
        prssamgr->reconstruction(oc);
        oc.setInvalidIfPRSSAReconstructed();

        //If SSA is enabled, disable classic DU Chain.
        //Since we do not maintain both them as some passes.
        //e.g:In RCE, remove PHI's operand will not update the
        //operand DEF's DUSet.
        //CASE:compiler.gr/alias.loop.gr
        oc.setInvalidPRDU();
        SIMP_need_rebuild_pr_du_chain(&simp) = false;
    }
    if (g_invert_branch_target) {
        getPassMgr()->registerPass(PASS_INVERT_BRTGT)->perform(oc);
    }
    getCFG()->performMiscOpt(oc);
}


bool Region::performSimplifyImpl(MOD SimpCtx & simp, OptCtx & oc)
{
    if (g_is_lower_to_lowest_height) {
        simp.setSimpToLowestHeight();
    }
    if (g_is_lower_to_pr_mode) {
        simp.setSimpToPRmode();
    }
    if (g_opt_level != OPT_LEVEL0) {
        //O0 does not build DU ref.
        ASSERT0(getDUMgr() && getDUMgr()->verifyMDRef());
    }
    getIRSimp()->simplifyBBlist(getBBList(), &simp);
    postSimplify(simp, oc);
    if (simp.needRebuildDUChain()) {
        DUMgr * dumgr = getDUMgr();
        if (dumgr != nullptr) {
            //NOTE rebuild classic DU-Chain is costly.
            //Especially compilation speed is considerable.
            dumgr->checkAndComputeClassicDUChain(oc);
        }
        bool rmprdu = false;
        bool rmnonprdu = false;
        if (simp.needRebuildNonPRDUChain()) {
            MDSSAMgr * mdssamgr = (MDSSAMgr*)getPassMgr()->queryPass(
                PASS_MDSSA_MGR);
            if (mdssamgr != nullptr) {
                mdssamgr->reconstruction(oc);
                oc.setInvalidIfMDSSAReconstructed();
                oc.setInvalidNonPRDU();
                rmnonprdu = true;
            }
        }
        if (simp.needRebuildPRDUChain()) {
            PRSSAMgr * prssamgr = (PRSSAMgr*)getPassMgr()->queryPass(
                PASS_PRSSA_MGR);
            if (prssamgr != nullptr) {
                prssamgr->reconstruction(oc);
                oc.setInvalidIfPRSSAReconstructed();
                oc.setInvalidPRDU();
                rmprdu = true;
            }
        }
        xoc::removeClassicDUChain(this, rmprdu, rmnonprdu);
    }
    ASSERT0L3(verifyClassicDUChain(this, oc));
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(this, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(this, oc));
    return true;
}


bool Region::performSimplifyArrayIngredient(OptCtx & oc)
{
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(this, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(this, oc));
    SimpCtx simp(&oc);

    //Only simplify the expression of array ingredient to lowest height to
    //faciitate LICM, RP and VECT.
    SIMP_to_lowest_height(&simp) = true;
    bool change = performSimplifyImpl(simp, oc);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpAll()) {
        note(this, "\n==---- DUMP AFTER SIMPLIFY ARRAY INGREDIENT ----==");
        dumpBBList();
    }
    return change;
}


bool Region::performSimplify(OptCtx & oc)
{
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(this, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(this, oc));
    SimpCtx simp(&oc);
    simp.setSimpCFS();
    simp.setSimpArray();
    simp.setSimpSelect();
    simp.setSimpLandLor();
    simp.setSimpLnot();
    simp.setSimpILdISt();
    bool change = performSimplifyImpl(simp, oc);
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpAll()) {
        note(this, "\n==---- DUMP AFTER SIMPLIFY IRBB LIST ----==");
        getLogMgr()->incIndent(2);
        dumpBBList();
        getLogMgr()->decIndent(2);
    }
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(this, oc));
    ASSERT0(MDSSAMgr::verifyMDSSAInfo(this, oc));
    return change;
}


bool Region::doPRSSA(OptCtx & oc)
{
    if (!g_do_prssa) { return false; }
    ASSERT0(getPassMgr());
    PRSSAMgr * prssamgr = (PRSSAMgr*)getPassMgr()->registerPass(
        PASS_PRSSA_MGR);
    ASSERT0(prssamgr);
    if (!prssamgr->is_valid()) {
        prssamgr->reconstruction(oc);
        oc.setInvalidIfPRSSAReconstructed();
    }
    //If SSA is enabled, disable classic DU Chain.
    //Since we do not maintain both them as some passes.
    //e.g:In RCE, remove PHI's operand will not update the
    //operand DEF's DUSet.
    //CASE:compiler.gr/alias.loop.gr
    oc.setInvalidPRDU();
    ASSERT0L3(PRSSAMgr::verifyPRSSAInfo(this, oc));
    return true;
}


bool Region::doMDSSA(OptCtx & oc)
{
    if (!g_do_mdssa) { return false; }
    ASSERT0(getPassMgr());
    MDSSAMgr * mdssamgr = (MDSSAMgr*)getPassMgr()->registerPass(
        PASS_MDSSA_MGR);
    ASSERT0(mdssamgr);
    if (!mdssamgr->is_valid()) {
        mdssamgr->construction(oc);
        oc.setInvalidIfMDSSAReconstructed();
    }
    //If SSA is enabled, disable classic DU Chain.
    //Since we do not maintain both them as some passes.
    //e.g:In RCE, remove PHI's operand will not update the
    //operand DEF's DUSet.
    //CASE:compiler.gr/alias.loop.gr
    oc.setInvalidNonPRDU();
    ASSERT0L3(MDSSAMgr::verifyMDSSAInfo(this, oc));
    return true;
}


bool Region::doRefineDU(OptCtx & oc)
{
    if (!g_do_refine_duchain) { return false; }
    ASSERT0(getPassMgr());
    RefineDUChain * refinedu = (RefineDUChain*)getPassMgr()->
        registerPass(PASS_REFINE_DUCHAIN);
    if (g_compute_pr_du_chain && g_compute_nonpr_du_chain) {
        refinedu->setUseGvn(true);
        GVN * gvn = (GVN*)getPassMgr()->registerPass(PASS_GVN);
        gvn->perform(oc);
    }
    return refinedu->perform(oc);
}


bool Region::doOnlyClassicNonPRDU(OptCtx & oc)
{
    bool org_compute_pr_du_chain = g_compute_pr_du_chain;
    bool org_compute_nonpr_du_chain = g_compute_nonpr_du_chain;
    g_compute_pr_du_chain = false;
    g_compute_nonpr_du_chain = true;
    ASSERT0L3(xoc::verifyMDRef(this, oc));
    DUMgr * dumgr = (DUMgr*)getPassMgr()->registerPass(PASS_DU_MGR);
    ASSERT0(dumgr);
    dumgr->checkAndComputeClassicDUChain(oc);
    ASSERT0L3(verifyClassicDUChain(this, oc));
    g_compute_pr_du_chain = org_compute_pr_du_chain;
    g_compute_nonpr_du_chain = org_compute_nonpr_du_chain;
    return true;
}


bool Region::doOnlyClassicPRDU(OptCtx & oc)
{
    bool org_compute_pr_du_chain = g_compute_pr_du_chain;
    bool org_compute_nonpr_du_chain = g_compute_nonpr_du_chain;
    g_compute_pr_du_chain = true;
    g_compute_nonpr_du_chain = false;
    ASSERT0L3(xoc::verifyMDRef(this, oc));
    DUMgr * dumgr = (DUMgr*)getPassMgr()->registerPass(PASS_DU_MGR);
    ASSERT0(dumgr);
    dumgr->checkAndComputeClassicDUChain(oc);
    ASSERT0L3(verifyClassicDUChain(this, oc));
    g_compute_pr_du_chain = org_compute_pr_du_chain;
    g_compute_nonpr_du_chain = org_compute_nonpr_du_chain;
    return true;
}


bool Region::doSimplyCPByClassicDU(OptCtx & oc)
{
    if (!g_compute_pr_du_chain_by_prssa) { return false; }
    PRSSAMgr * prssa = getPRSSAMgr();
    if (prssa != nullptr && prssa->is_valid()) {
        //Do NOT violate PRSSA.
        return false;
    }
    MDSSAMgr * mdssa = getMDSSAMgr();
    if (mdssa == nullptr || !mdssa->is_valid()) {
        //MDSSA is necessary.
        return false;
    }
    //Use classic DU to remove redundant copy.
    doOnlyClassicPRDU(oc);
    CopyProp * cp = (CopyProp*)getPassMgr()->registerPass(PASS_CP);
    ASSERT0(cp);
    cp->setPropagationKind(CP_PROP_CONST|CP_PROP_PR);
    bool changed = cp->perform(oc);
    if (!changed) { return false; }

    ASSERT0(getCFG());
    if (getCFG()->getLoopInfo() != nullptr) {
        oc.setInvalidLoopInfo(); //[TODO]: Incremental Maintenance.
    }

    //[TODO]: Open DeadCodeElim.
    //DeadCodeElim * dce = (DeadCodeElim*)getPassMgr()->registerPass(PASS_DCE);
    //ASSERT0(dce);
    //dce->setAggressive(false);
    //changed |= dce->perform(oc);
    return changed;
}


static void assignDummyUseForCallInIRList(IR * irlst, Region const* rg)
{
    if (irlst == nullptr) { return; }
    IRMgr * irmgr = rg->getIRMgr();
    ASSERT0(irmgr);
    for (IR * ir = irlst; ir != nullptr; ir = ir->get_next()) {
        if (ir->isCallStmt() && !CALL_is_intrinsic(ir)) {
            irmgr->genDummyuseForCallStmt(ir);
        }
    }
}


static void assignDummyUseForCallInBBList(BBList const* bblst, Region const* rg)
{
    if (bblst == nullptr) { return; }
    IRMgr * irmgr = rg->getIRMgr();
    ASSERT0(irmgr);
    BBListIter bbct;
    for (bblst->get_head(&bbct);
         bbct != nullptr; bbct = bblst->get_next(bbct)) {
        IRBB * bb = bbct->val();

        //For now, CallStmt is always BB lower boundary.
        IR * lastir = BB_last_ir(bb);
        if (lastir == nullptr || !lastir->isCallStmt() ||
            CALL_is_intrinsic(lastir) || lastir->isReadOnly()) {
            continue;
        }
        irmgr->genDummyuseForCallStmt(lastir);
    }
}


void Region::assignDummyUseForCall()
{
    if (getIRList() != nullptr) {
        assignDummyUseForCallInIRList(getIRList(), this);
        return;
    }
    BBList const* bblst = getBBList();
    assignDummyUseForCallInBBList(bblst, this);
}


void Region::doAssignDirectRefMD(OptCtx & oc)
{
    if (!oc.isPassValid(PASS_MD_REF)) {
        getMDMgr()->assignMD();
    }
}


bool Region::doAA(OptCtx & oc)
{
    if (!g_do_aa || oc.isPassValid(PASS_AA)) { return false; }
    ASSERT0(getPassMgr());
    ASSERT0(g_cst_bb_list && oc.isPassValid(PASS_CFG));
    doAssignDirectRefMD(oc);
    assignDummyUseForCall();
    getPassMgr()->checkValidAndRecompute(
        &oc, PASS_DOM, PASS_LOOP_INFO, PASS_AA, PASS_UNDEF);
    return true;
}


bool Region::doMDRefAndClassicPRDU(OptCtx & oc)
{
    if (!g_compute_pr_du_chain) { return false; }
    ASSERT0(getPassMgr());
    DUMgr * dumgr = (DUMgr*)getPassMgr()->registerPass(PASS_DU_MGR);
    ASSERT0(dumgr);
    //Do not ask checkAndRecompute options, because user may only require
    //PR DU chain. So applying ClassicPRDU analysis if needed.
    DUOptFlag flag(DUOPT_COMPUTE_PR_REF | DUOPT_COMPUTE_NONPR_REF);
    if (g_compute_pr_du_chain_by_prssa) {
        //No Need to compute REACH_DEF here because user ask computing PRDU
        //chain through PRSSA. And the computation of REACH_DEF is costly.
        //Thus call computePRDUChainByPRSSA() to computes PRDU chain in
        //checkAndComputeClassicDUChain() after the function returning.
        //CASE:Some target expect to compute PRDU by REACH_DEF and
        //PRSSA both.
        ;
    } else {
        flag.set(DUOPT_SOL_REACH_DEF | DUOPT_COMPUTE_PR_DU);
    }
    getDUMgr()->perform(oc, flag);
    if (flag.have(DUOPT_COMPUTE_PR_DU)) {
        ASSERT0(oc.is_reach_def_valid());
        getDUMgr()->computeMDDUChain(
            oc, false, DUOptFlag(DUOPT_COMPUTE_PR_DU));
    } else {
        //At least compute PRDU by one way.
        ASSERT0(g_compute_pr_du_chain_by_prssa);
        getDUMgr()->computePRDUChainByPRSSA(oc, true);
    }
    return true;
}


//The function performs AA aggressively.
bool Region::doAggressiveAA(OptCtx & oc)
{
    if (!g_do_aa || oc.isPassValid(PASS_AA)) { return false; }
    ASSERT0(getPassMgr());
    ASSERT0(getCFG()->is_valid());

    //Do fold-const first.
    getPassMgr()->checkValidAndRecompute(&oc, PASS_REFINE, PASS_UNDEF);
    AliasAnalysis * aa = (AliasAnalysis*)getPassMgr()->registerPass(PASS_AA);
    ASSERT0(aa);

    //Recompute and set MD reference to avoid AA's complaint.
    //Compute AA to build coarse-grained DU chain.
    doAssignDirectRefMD(oc);
    if (!aa->is_init() || !aa->is_valid()) {
        aa->initAliasAnalysis();
    }
    aa->set_flow_sensitive(false);
    aa->perform(oc);
    ASSERT0(aa->is_valid());

    //Compute PR's DefUse chain to improve AA precison.
    if (!doPRSSA(oc) && !doMDRefAndClassicPRDU(oc)) {
        return false;
    }
    //Recompute and set MD reference to avoid AA's complaint.
    getMDMgr()->assignMD();

    //Estimate the threshold before performing AA.
    UINT numir = 0;
    for (IRBB * bb = getBBList()->get_head();
         bb != nullptr; bb = getBBList()->get_next()) {
        numir += bb->getNumOfIR();
    }
    aa->set_flow_sensitive(numir < xoc::g_thres_opt_ir_num);
    aa->perform(oc);
    ASSERT0(aa->is_valid());
    return true;
}


bool Region::doMDRef(OptCtx & oc)
{
    ASSERT0(getPassMgr());
    DUMgr * dumgr = (DUMgr*)getPassMgr()->registerPass(PASS_DU_MGR);
    ASSERT0(dumgr);
    DUOptFlag f(DUOPT_COMPUTE_PR_REF | DUOPT_COMPUTE_NONPR_REF);
    if (g_compute_region_imported_defuse_md) {
        f.set(DUOPT_SOL_REGION_REF);
    }
    bool changed = dumgr->perform(oc, f);
    ASSERT0(oc.isPassValid(PASS_MD_REF));
    ASSERT0(xoc::verifyMDRef(this, oc));
    ASSERT0(changed);
    return changed;
}


bool Region::doMDRefAndClassicDU(OptCtx & oc)
{
    ASSERT0(getPassMgr());
    bool changed = doMDRef(oc);
    DUMgr * dumgr = (DUMgr*)getPassMgr()->registerPass(PASS_DU_MGR);
    ASSERT0(dumgr);
    changed |= dumgr->checkAndComputeClassicDUChain(oc);
    return changed;
}


bool Region::doDUAna(OptCtx & oc)
{
    if (!g_do_md_du_analysis) { return false; }
    ASSERT0(g_cst_bb_list && oc.is_cfg_valid() && oc.is_aa_valid());
    doMDRefAndClassicDU(oc);

    //NOTE:MD reference may changed and inform pass-mgr to rebuild MDSSA.
    oc.setInvalidPass(PASS_MDSSA_MGR);
    bool changed_prssa = doPRSSA(oc);
    bool changed_mdssa = doMDSSA(oc);
    bool remove_prdu = changed_prssa ? true : false;
    bool remove_nonprdu = changed_mdssa ? true : false;
    xoc::removeClassicDUChain(oc.getRegion(), remove_prdu, remove_nonprdu);
    return doRefineDU(oc);
}


bool Region::doBasicAnalysis(OptCtx & oc)
{
    bool changed = doAA(oc);
    changed |= doDUAna(oc);
    return changed;
}


//Perform general optimizaitions.
//Basis step to do:
//    1. Build control flow.
//    2. Compute data flow dependence.
//    3. Compute live expression info.
//
//Optimizations to be performed:
//    1. GCSE
//    2. DCE
//    3. RVI(register variable recog)
//    4. IVR(induction variable elimination)
//    5. CP(constant propagation)
//    6. CP(copy propagation)
//    7. SCCP (Sparse Conditional Constant Propagation).
//    8. PRE (Partial Redundancy Elimination) with strength reduction.
//    9. Dominator-based optimizations such as copy propagation,
//       constant propagation and redundancy elimination using
//       value numbering.
//    10. Must-alias analysis, to convert pointer de-references
//        into regular variable references whenever possible.
//    11. Scalar Replacement of Aggregates, to convert structure
//        references into scalar references that can be optimized
//       using the standard scalar passes.
bool Region::MiddleProcess(OptCtx & oc)
{
    BBList * bbl = getBBList();
    if (bbl->get_elem_count() == 0) { return true; }
    ASSERT0L3(verifyClassicDUChain(this, oc));
    if (g_opt_level > OPT_LEVEL0) {
        //Do analysis before simplification.
        doBasicAnalysis(oc);
    }
    if (g_is_lower_to_lowest_height || g_is_lower_to_pr_mode) {
        performSimplify(oc);
    } else if (g_is_simplify_array_ingredient) {
        performSimplifyArrayIngredient(oc);
    }
    if (g_opt_level > OPT_LEVEL0) {
        getPassMgr()->registerPass(PASS_SCALAR_OPT)->perform(oc);
        if (g_invert_branch_target) {
            getPassMgr()->registerPass(PASS_INVERT_BRTGT)->perform(oc);
        }
    }
    ASSERT0(getCFG() && getCFG()->verifyRPO(oc));
    if (g_do_refine) {
        RefineCtx rf(&oc);
        Refine * refine = (Refine*)getPassMgr()->registerPass(PASS_REFINE);
        if (refine->refineBBlist(bbl, rf)) {
            ASSERT0L3(verifyClassicDUChain(this, oc));
            return true;
        }
        return false;
    }
    ASSERT0(verifyIRandBB(bbl, this));
    return true;
}

} //namespace xoc
