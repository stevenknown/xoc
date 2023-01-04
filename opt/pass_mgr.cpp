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

PassMgr::PassMgr(Region * rg)
{
    ASSERT0(rg);
    m_rg = rg;
    m_rumgr = rg->getRegionMgr();
    m_tm = rg->getTypeMgr();
    ASSERT0(m_tm);
}


//Destory dedicated pass.
void PassMgr::destroyPass(Pass * pass)
{
    ASSERT0(pass);
    PASS_TYPE passtype = pass->getPassType();
    ASSERT0(passtype != PASS_UNDEF);
    m_registered_pass.remove(passtype);
    delete pass;
}


void PassMgr::destroyPass(PASS_TYPE passtype)
{
    Pass * pass = queryPass(passtype);
    if (pass == nullptr) { return; }
    destroyPass(pass);
}



void PassMgr::destroyAllRegisteredPass()
{
    PassTabIter tabiter;
    Pass * p;
    Pass * irmgr = nullptr;
    for (m_registered_pass.get_first(tabiter, &p);
         p != nullptr; m_registered_pass.get_next(tabiter, &p)) {
        if (p->getPassType() == PASS_IRMGR) {
            //Because some passes dependent on IRMgr, destroy it at last.
            irmgr = p;
            continue;
        }
        delete p;
    }
    if (irmgr != nullptr) {
        delete irmgr;
    }
}


void PassMgr::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    START_TIMER(t, "PassMgr");
    note(m_rg, "\n==---- DUMP %s '%s' ----==", "PassMgr",
         m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    PassTabIter tabiter;
    Pass * p;
    for (m_registered_pass.get_first(tabiter, &p);
         p != nullptr; m_registered_pass.get_next(tabiter, &p)) {
        note(m_rg, "\nPASS:%s", p->getPassName());
    }
    m_rg->getLogMgr()->decIndent(2);
    END_TIMER(t, "PassMgr");
}


Pass * PassMgr::allocCopyProp()
{
    return new CopyProp(m_rg);
}


Pass * PassMgr::allocGCSE()
{
    return new GCSE(m_rg, (GVN*)registerPass(PASS_GVN));
}


Pass * PassMgr::allocLCSE()
{
    return new LCSE(m_rg);
}


Pass * PassMgr::allocRP()
{
    return new RegPromot(m_rg);
}


Pass * PassMgr::allocPRE()
{
    //return new PRE(m_rg);
    return nullptr;
}


Pass * PassMgr::allocIVR()
{
    return new IVR(m_rg);
}


Pass * PassMgr::allocLICM()
{
    return new LICM(m_rg);
}


Pass * PassMgr::allocDCE()
{
    return new DeadCodeElim(m_rg);
}


Pass * PassMgr::allocLFTR()
{
    return new LFTR(m_rg);
}


Pass * PassMgr::allocInferType()
{
    return new InferType(m_rg);
}


Pass * PassMgr::allocInvertBrTgt()
{
    return new InvertBrTgt(m_rg);
}


Pass * PassMgr::allocVRP()
{
    #ifdef FOR_IP
    return new VRP(m_rg);
    #else
    return nullptr;
    #endif
}


Pass * PassMgr::allocDSE()
{
    #ifdef FOR_IP
    return new DSE(m_rg);
    #else
    return nullptr;
    #endif
}


Pass * PassMgr::allocRCE()
{
    return new RCE(m_rg, (GVN*)registerPass(PASS_GVN));
}


Pass * PassMgr::allocGVN()
{
    return new GVN(m_rg);
}


Pass * PassMgr::allocLoopCvt()
{
    return new LoopCvt(m_rg);
}


Pass * PassMgr::allocPRSSAMgr()
{
    return new PRSSAMgr(m_rg);
}


Pass * PassMgr::allocMDSSAMgr()
{
    return new MDSSAMgr(m_rg);
}


Pass * PassMgr::allocCDG()
{
    return new CDG(m_rg);
}


Pass * PassMgr::allocGSCC()
{
    return new GSCC(m_rg);
}


Pass * PassMgr::allocIRSimp()
{
    return new IRSimp(m_rg);
}


Pass * PassMgr::allocIRMgr()
{
    return new IRMgrExt(m_rg);
}


Pass * PassMgr::allocLinearScanRA()
{
    #ifdef FOR_IP
    return new LinearScanRA(m_rg);
    #else
    return nullptr;
    #endif
}


Pass * PassMgr::allocCCP()
{
    //return new CondConstProp(m_rg, (PRSSAMgr*)registerPass(PASS_PRSSA_MGR));
    return nullptr;
}


Pass * PassMgr::allocExprTab()
{
    return new ExprTab(m_rg);
}


Pass * PassMgr::allocCfsMgr()
{
    return new CfsMgr(m_rg);
}


Pass * PassMgr::allocIPA()
{
    return new IPA(m_rg);
}


Pass * PassMgr::allocInliner()
{
    return new Inliner(m_rg);
}


Pass * PassMgr::allocAA()
{
    return new AliasAnalysis(m_rg);
}


Pass * PassMgr::allocDUMgr()
{
    return new DUMgr(m_rg);
}


Pass * PassMgr::allocCFG()
{
    BBList * bbl = m_rg->getBBList();
    UINT n = MAX(8, xcom::getNearestPowerOf2(bbl->get_elem_count()));
    return new IRCFG(C_SEME, bbl, m_rg, n);
}


Pass * PassMgr::allocRefineDUChain()
{
    return new RefineDUChain(m_rg);
}


Pass * PassMgr::allocScalarOpt()
{
    return new ScalarOpt(m_rg);
}


Pass * PassMgr::allocMDSSALiveMgr()
{
    return new MDSSALiveMgr(m_rg);
}


Pass * PassMgr::allocLivenessMgr()
{
    return new LivenessMgr(m_rg);
}


Pass * PassMgr::allocMDLivenessMgr()
{
    return new MDLivenessMgr(m_rg);
}


Pass * PassMgr::allocRefine()
{
    return new Refine(m_rg);
}


Pass * PassMgr::registerPass(PASS_TYPE opty)
{
    Pass * pass = queryPass(opty);
    if (pass != nullptr) { return pass; }

    switch (opty) {
    case PASS_CFG:
        pass = allocCFG();
        break;
    case PASS_AA:
        pass = allocAA();
        break;
    case PASS_DU_MGR:
        pass = allocDUMgr();
        break;
    case PASS_CP:
        pass = allocCopyProp();
        break;
    case PASS_GCSE:
        pass = allocGCSE();
        break;
    case PASS_LCSE:
        pass = allocLCSE();
        break;
    case PASS_RP:
        pass = allocRP();
        break;
    case PASS_PRE:
        pass = allocPRE();
        break;
    case PASS_IVR:
        pass = allocIVR();
        break;
    case PASS_LICM:
        pass = allocLICM();
        break;
    case PASS_DCE:
        pass = allocDCE();
        break;
    case PASS_DSE:
        pass = allocDSE();
        break;
    case PASS_RCE:
        pass = allocRCE();
        break;
    case PASS_GVN:
        pass = allocGVN();
        break;
    case PASS_LOOP_CVT:
        pass = allocLoopCvt();
        break;
    case PASS_PRSSA_MGR:
        pass = allocPRSSAMgr();
        break;
    case PASS_MDSSA_MGR:
        pass = allocMDSSAMgr();
        break;
    case PASS_CCP:
        pass = allocCCP();
        break;
    case PASS_CDG:
        pass = allocCDG();
        break;
    case PASS_EXPR_TAB:
        pass = allocExprTab();
        break;
    case PASS_CFS_MGR:
        pass = allocCfsMgr();
        break;
    case PASS_IPA:
        pass = allocIPA();
        break;
    case PASS_INLINER:
        pass = allocInliner();
        break;
    case PASS_REFINE_DUCHAIN:
        pass = allocRefineDUChain();
        break;
    case PASS_SCALAR_OPT:
        pass = allocScalarOpt();
        break;
    case PASS_MDLIVENESS_MGR:
        pass = allocMDLivenessMgr();
        break;
    case PASS_LFTR:
        pass = allocLFTR();
        break;
    case PASS_INFER_TYPE:
        pass = allocInferType();
        break;
    case PASS_INVERT_BRTGT:
        pass = allocInvertBrTgt();
        break;
    case PASS_VRP:
        pass = allocVRP();
        break;
    case PASS_SCC:
        pass = allocGSCC();
        break;
    case PASS_REFINE:
        pass = allocRefine();
        break;
    case PASS_MDSSALIVE_MGR:
        pass = allocMDSSALiveMgr();
        break;
    case PASS_IRSIMP:
        pass = allocIRSimp();
        break;
    case PASS_IRMGR:
        pass = allocIRMgr();
        break;
    case PASS_LINEAR_SCAN_RA:
        pass = allocLinearScanRA();
        break;
    case PASS_LIVENESS_MGR:
        pass = allocLivenessMgr();
        break;
    default: ASSERTN(0, ("Unsupport Pass."));
    }

    ASSERT0(opty != PASS_UNDEF && pass);
    m_registered_pass.set(opty, pass);
    return pass;
}


//This function check validation of options in oc, perform
//recomputation if it is invalid.
//...: the options/passes that anticipated to recompute.
void PassMgr::checkValidAndRecompute(OptCtx * oc, ...)
{
    PassTypeList optlist;
    UINT num = 0;
    va_list ptr;
    va_start(ptr, oc);
    PASS_TYPE opty = (PASS_TYPE)va_arg(ptr, UINT);
    while (opty != PASS_UNDEF && num < 1000) {
        ASSERTN(opty < PASS_NUM,
                ("You should append PASS_UNDEF to pass list."));
        optlist.append_tail(opty);
        num++;
        opty = (PASS_TYPE)va_arg(ptr, UINT);
    }
    va_end(ptr);
    checkValidAndRecompute(oc, optlist);
}


void PassMgr::checkAndRecomputeDUChain(OptCtx * oc, DUMgr * dumgr,
                                       BitSet const& opts)
{
    if (oc->is_pr_du_chain_valid() && oc->is_nonpr_du_chain_valid()) {
        return;
    }
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return; }
    if (dumgr == nullptr) {
        dumgr = (DUMgr*)registerPass(PASS_DU_MGR);
    }

    PassTypeList optlist;
    if (!oc->is_ref_valid()) {
        optlist.append_tail(PASS_RPO);
        optlist.append_tail(PASS_DOM);
        optlist.append_tail(PASS_DU_REF);
    }
    if (!oc->is_reach_def_valid()) {
        optlist.append_tail(PASS_RPO);
        optlist.append_tail(PASS_DOM);
        optlist.append_tail(PASS_DU_REF);
        optlist.append_tail(PASS_REACH_DEF);
    }
    if (optlist.get_elem_count() != 0) {
        m_rg->getPassMgr()->checkValidAndRecompute(oc, optlist);
    }

    DUOptFlag flag(DUOPT_UNDEF);
    if (!oc->is_nonpr_du_chain_valid()) {
        flag.set(DUOPT_COMPUTE_NONPR_DU);
    }
    if (!oc->is_pr_du_chain_valid()) {
        flag.set(DUOPT_COMPUTE_PR_DU);
    }

    //TBD: compute classic DU without considering PRSSA.
    ////If PRs have already been in SSA form, compute
    ////DU chain doesn't make any sense.
    //PRSSAMgr * ssamgr = (PRSSAMgr*)queryPass(PASS_PRSSA_MGR);
    //if ((ssamgr == nullptr || !ssamgr->is_valid()) &&
    //    !oc->is_pr_du_chain_valid()) {
    //    flag |= DUOPT_COMPUTE_PR_DU;
    //}
    if (flag.do_nothing()) {
        //Nothing need to compute.
        return;
    }
    if (opts.is_contain(PASS_REACH_DEF)) {
        dumgr->computeMDDUChain(*oc, true, flag);
    } else {
        dumgr->computeMDDUChain(*oc, false, flag);
    }
}


void PassMgr::checkAndRecomputeAAandDU(OptCtx * oc, IRCFG * cfg,
                                       AliasAnalysis *& aa, DUMgr *& dumgr,
                                       BitSet const& opts)
{
    BBList * bbl = m_rg->getBBList();
    DUOptFlag f(DUOPT_UNDEF);
    if (opts.is_contain(PASS_DU_REF) && !oc->is_ref_valid()) {
        f.set(DUOPT_COMPUTE_PR_REF|DUOPT_COMPUTE_NONPR_REF);
    }
    if (opts.is_contain(PASS_LIVE_EXPR) && !oc->is_live_expr_valid()) {
        f.set(DUOPT_SOL_AVAIL_EXPR);
    }
    if (opts.is_contain(PASS_AVAIL_REACH_DEF) &&
        !oc->is_avail_reach_def_valid()) {
        f.set(DUOPT_SOL_AVAIL_REACH_DEF);
    }
    if (opts.is_contain(PASS_REACH_DEF) && !oc->is_reach_def_valid()) {
        f.set(DUOPT_SOL_REACH_DEF);
    }
    if (opts.is_contain(PASS_DU_CHAIN) &&
        (!oc->is_pr_du_chain_valid() || !oc->is_nonpr_du_chain_valid()) &&
        !oc->is_reach_def_valid()) {
        f.set(DUOPT_SOL_REACH_DEF);
    }
    if (opts.is_contain(PASS_AA) && !oc->is_aa_valid() &&
        bbl != nullptr && bbl->get_elem_count() != 0) {
        ASSERTN(cfg && oc->is_cfg_valid(),
                ("You should make CFG available first."));
        if (aa == nullptr) {
            aa = (AliasAnalysis*)registerPass(PASS_AA);
            if (!aa->is_init()) {
                aa->initAliasAnalysis();
            }
        }
        UINT numir = 0;
        UINT max_numir_in_bb = 0;
        for (IRBB * bb = bbl->get_head();
            bb != nullptr; bb = bbl->get_next()) {
            numir += bb->getNumOfIR();
            max_numir_in_bb = MAX(max_numir_in_bb, bb->getNumOfIR());
        }
        if (numir > g_thres_opt_ir_num ||
            max_numir_in_bb > g_thres_opt_ir_num_in_bb) {
            aa->set_flow_sensitive(false);
        }
        //NOTE: assignMD(false) must be called before AA.
        aa->perform(*oc);
    }
    if (!f.do_nothing() && bbl != nullptr && bbl->get_elem_count() != 0) {
        if (dumgr == nullptr) {
            dumgr = (DUMgr*)registerPass(PASS_DU_MGR);
        }
        if (opts.is_contain(PASS_DU_REF)) {
            f.set(DUOPT_COMPUTE_NONPR_DU|DUOPT_COMPUTE_PR_DU);
        }
        dumgr->perform(*oc, f);
        if (f.have(DUOPT_COMPUTE_PR_REF) || f.have(DUOPT_COMPUTE_NONPR_REF)) {
            ASSERT0(dumgr->verifyMDRef());
        }
        if (f.have(DUOPT_SOL_AVAIL_EXPR)) {
            ASSERT0(dumgr->verifyLiveinExp());
        }
    }
}


void PassMgr::checkValidAndRecompute(OptCtx * oc, PassTypeList & optlist)
{
    ASSERTN(optlist.get_elem_count() < 1000,
            ("too many pass queried or miss ending placeholder"));
    if (optlist.get_elem_count() == 0) { return; }

    BitSet opts;
    C<PASS_TYPE> * it;
    for (optlist.get_head(&it); it != nullptr; optlist.get_next(&it)) {
        PASS_TYPE opty = it->val();
        if (opty == PASS_UNDEF) { continue; }
        ASSERTN(opty < PASS_NUM,
                ("You should append PASS_UNDEF to pass list."));
        opts.bunion(opty);
    }
    if (opts.is_contain(PASS_DOM) || opts.is_contain(PASS_PDOM)) {
        //Incremental DOM info update need both DOM and PDOM available.
        opts.bunion(PASS_DOM);
        opts.bunion(PASS_PDOM);
    }

    IRCFG * cfg = (IRCFG*)queryPass(PASS_CFG);
    AliasAnalysis * aa = nullptr;
    DUMgr * dumgr = nullptr;

    for (optlist.get_head(&it); it != optlist.end();
         it = optlist.get_next(it)) {
        PASS_TYPE pt = it->val();
        switch (pt) {
        case PASS_CFG:
            if (!oc->is_cfg_valid()) {
                if (cfg == nullptr) {
                    //CFG is not constructed.
                    cfg = (IRCFG*)registerPass(PASS_CFG);
                    cfg->initCFG(*oc);
                } else {
                    //CAUTION: validation of CFG should maintained by user.
                    cfg->rebuild(*oc);
                }
            }
            break;
        case PASS_CDG: {
            CDG * cdg = (CDG*)registerPass(PASS_CDG);
            ASSERT0(cdg); //cdg is not enable.
            if (!cdg->is_valid()) {
                ASSERTN(cfg && oc->is_cfg_valid(),
                        ("You should make CFG available first."));
                cdg->perform(*oc);
            } else {
                ASSERT0(cdg->verify());
            }
            break;
        }
        case PASS_DOM:
        case PASS_PDOM:
            if (!oc->is_dom_valid()) {
                ASSERTN(cfg && oc->is_cfg_valid(),
                        ("You should make CFG available first."));
                cfg->computeDomAndIdom(*oc);
            }
            if (!oc->is_pdom_valid()) {
                ASSERTN(cfg && oc->is_cfg_valid(),
                        ("You should make CFG available first."));
                cfg->computePdomAndIpdom(*oc);
            }
            break;
        case PASS_EXPR_TAB:
            if (!oc->is_expr_tab_valid() &&
                m_rg->getBBList() != nullptr &&
                m_rg->getBBList()->get_elem_count() != 0) {
                ExprTab * exprtab = (ExprTab*)registerPass(PASS_EXPR_TAB);
                ASSERT0(exprtab);
                exprtab->reperform(*oc);
            }
            break;
        case PASS_LOOP_INFO:
            if (!oc->is_loopinfo_valid()) {
                ASSERTN(cfg && oc->is_cfg_valid(),
                        ("You should make CFG available first."));
                cfg->LoopAnalysis(*oc);
            } else {
                cfg->verifyLoopInfo(*oc);
            }
            break;
        case PASS_RPO:
            ASSERTN(cfg && oc->is_cfg_valid(),
                    ("You should make CFG available first."));
            if (!oc->is_rpo_valid() || cfg->getRPOVexList() == nullptr) {
                cfg->computeRPO(*oc);
            } else {
                ASSERT0(cfg->verifyRPO(*oc));
            }
            break;
        case PASS_SCC:
            if (!oc->is_scc_valid()) {
                ASSERTN(cfg && oc->is_cfg_valid(),
                        ("You should make CFG available first."));
                GSCC * gscc = (GSCC*)registerPass(PASS_SCC);
                ASSERT0(gscc); //scc is not enable.
                gscc->perform(*oc);
            }
            break;
        case PASS_AA:
        case PASS_DU_REF:
        case PASS_LIVE_EXPR:
        case PASS_REACH_DEF:
        case PASS_AVAIL_REACH_DEF:
            checkAndRecomputeAAandDU(oc, cfg, aa, dumgr, opts);
            break;
        case PASS_DU_CHAIN:
            checkAndRecomputeDUChain(oc, dumgr, opts);
            break;
        default: {
            Pass * pass = registerPass(pt);
            if (pass != nullptr || !pass->is_valid()) {
                pass->perform(*oc);
            }
        }
        } //end switch
    } //end for
}

} //namespace xoc
