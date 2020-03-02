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
    m_pool = smpoolCreate(sizeof(TimeInfo) * 4, MEM_COMM);
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
    m_registered_graph_based_pass.remove(passtype);
    delete pass;
}


void PassMgr::destroyPass(PASS_TYPE passtype)
{
    Pass * pass = queryPass(passtype);
    if (pass == NULL) { return; }
    destroyPass(pass);
}



void PassMgr::destroyAllPass()
{
    TMapIter<PASS_TYPE, Pass*> tabiter;
    Pass * p;
    for (m_registered_pass.get_first(tabiter, &p);
         p != NULL; m_registered_pass.get_next(tabiter, &p)) {
        delete p;
    }

    xcom::Graph * opt2;
    TMapIter<PASS_TYPE, xcom::Graph*> tabiter2;
    for (m_registered_graph_based_pass.get_first(tabiter2, &opt2);
         opt2 != NULL;
         m_registered_graph_based_pass.get_next(tabiter2, &opt2)) {
        delete opt2;
    }
}


Pass * PassMgr::allocCopyProp()
{
    Pass * pass = new CopyProp(m_rg);
    SimpCtx * simp = (SimpCtx*)xmalloc(sizeof(SimpCtx));
    simp->init();
    pass->set_simp_cont(simp);
    return pass;
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
    return new RegPromot(m_rg, (GVN*)registerPass(PASS_GVN));
}


Pass * PassMgr::allocPRE()
{
    //return new PRE(m_rg);
    return NULL;
}


Pass * PassMgr::allocIVR()
{
    //return new IVR(m_rg);
    return NULL;
}


Pass * PassMgr::allocLICM()
{
    return new LICM(m_rg);
}


Pass * PassMgr::allocDCE()
{
    return new DeadCodeElim(m_rg);
}


Pass * PassMgr::allocDSE()
{
    //return new DSE(m_rg);
    return NULL;
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


xcom::Graph * PassMgr::allocCDG()
{
    return new CDG(m_rg);
}


Pass * PassMgr::allocCCP()
{
    //return new CondConstProp(m_rg, (PRSSAMgr*)registerPass(PASS_PR_SSA_MGR));
    return NULL;
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
    return new IRCFG(C_SEME, bbl, m_rg, n, n);
}


Pass * PassMgr::allocRefineDUChain()
{
    return new RefineDUChain(m_rg);
}


xcom::Graph * PassMgr::registerGraphBasedPass(PASS_TYPE opty)
{
    xcom::Graph * pass = NULL;
    switch (opty) {
    case PASS_CDG:
        pass = allocCDG();
        break;
    default: ASSERTN(0, ("Unsupport Optimization."));
    }

    ASSERT0(opty != PASS_UNDEF && pass);
    m_registered_graph_based_pass.set(opty, pass);
    return pass;
}


Pass * PassMgr::registerPass(PASS_TYPE opty)
{
    Pass * pass = queryPass(opty);
    if (pass != NULL) { return pass; }

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
    case PASS_PR_SSA_MGR:
        pass = allocPRSSAMgr();
        break;
    case PASS_MD_SSA_MGR:
        pass = allocMDSSAMgr();
        break;
    case PASS_CCP:
        pass = allocCCP();
        break;
    case PASS_CDG:
        return (Pass*)registerGraphBasedPass(opty);
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
    default: ASSERTN(0, ("Unsupport Optimization."));
    }

    ASSERT0(opty != PASS_UNDEF && pass);
    m_registered_pass.set(opty, pass);
    return pass;
}


void PassMgr::performScalarOpt(OptCtx & oc)
{
    TTab<Pass*> opt_tab;
    List<Pass*> passlist;
    SimpCtx simp;
    if (g_do_gvn) { registerPass(PASS_GVN); }

    if (g_do_pre) {
        //Do PRE individually.
        //Since it will incur the opposite effect with Copy-Propagation.
        Pass * pre = registerPass(PASS_PRE);
        pre->perform(oc);
        ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    }

    if (g_do_dce) {
        DeadCodeElim * dce = (DeadCodeElim*)registerPass(PASS_DCE);
        passlist.append_tail(dce);
        if (g_do_dce_aggressive) {
            dce->set_elim_cfs(true);
        }
    }

    bool in_ssa_form = false;
    PRSSAMgr * ssamgr = (PRSSAMgr*)(m_rg->getPassMgr()->
        queryPass(PASS_PR_SSA_MGR));
    if (ssamgr != NULL && ssamgr->isSSAConstructed()) {
        in_ssa_form = true;
    }

    if (!in_ssa_form) {
        //RP can reduce the memory operations and
        //improve the effect of PR SSA, so perform
        //RP before SSA construction.
        //TODO: Do SSA renaming when after register promotion done.
        if (g_do_rp) {
            //First RP.
            passlist.append_tail(registerPass(PASS_RP));
        }
    }

    if (g_do_cp) {
        CopyProp * pass = (CopyProp*)registerPass(PASS_CP);
        pass->setPropagationKind(CP_PROP_SIMPLEX);
        passlist.append_tail(pass);
    }

    if (g_do_rp) {
        //Second RP.
        passlist.append_tail(registerPass(PASS_RP));
    }

    if (g_do_gcse) {
        passlist.append_tail(registerPass(PASS_GCSE));
    }

    if (g_do_lcse) {
        passlist.append_tail(registerPass(PASS_LCSE));
    }

    if (g_do_rce) {
        passlist.append_tail(registerPass(PASS_RCE));
    }

    if (g_do_dse) {
        passlist.append_tail(registerPass(PASS_DSE));
    }

    if (g_do_licm) {
        passlist.append_tail(registerPass(PASS_LICM));
    }

    if (g_do_ivr) {
        passlist.append_tail(registerPass(PASS_IVR));
    }

    if (g_do_loop_convert) {
        passlist.append_tail(registerPass(PASS_LOOP_CVT));
    }

    bool change;
    UINT count = 0;
    BBList * bbl = m_rg->getBBList();
    IRCFG * cfg = m_rg->getCFG();
    DUMMYUSE(cfg);
    do {
        change = false;
        for (Pass * pass = passlist.get_head();
             pass != NULL; pass = passlist.get_next()) {
            CHAR const* passname = pass->getPassName();
            ASSERT0(verifyIRandBB(bbl, m_rg));
            ULONGLONG t = xcom::getusec();
            bool doit = pass->perform(oc);
            appendTimeInfo(passname, xcom::getusec() - t);
            if (doit) {
                change = true;
                ASSERT0(verifyIRandBB(bbl, m_rg));
                ASSERT0(cfg->verify());
            }
            RefineCtx rc;
            m_rg->refineBBlist(bbl, rc);
            ASSERT0(m_rg->verifyRPO(oc));
        }
        count++;
    } while (change && count < 20);
    ASSERT0(!change);

    if (g_do_lcse) {
        LCSE * lcse = (LCSE*)registerPass(PASS_LCSE);
        lcse->set_enable_filter(false);
        ULONGLONG t = xcom::getusec();
        lcse->perform(oc);
        t = xcom::getusec() - t;
        appendTimeInfo(lcse->getPassName(), t);
    }

    if (g_do_rp) {
        RegPromot * r = (RegPromot*)registerPass(PASS_RP);
        ULONGLONG t = xcom::getusec();
        r->perform(oc);
        appendTimeInfo(r->getPassName(), xcom::getusec() - t);
    }
}

} //namespace xoc
