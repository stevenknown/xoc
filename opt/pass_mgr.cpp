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
    PassTabIter tabiter;
    Pass * p;
    for (m_registered_pass.get_first(tabiter, &p);
         p != NULL; m_registered_pass.get_next(tabiter, &p)) {
        delete p;
    }

    xcom::Graph * opt2;
    GraphPassTabIter tabiter2;
    for (m_registered_graph_based_pass.get_first(tabiter2, &opt2);
         opt2 != NULL;
         m_registered_graph_based_pass.get_next(tabiter2, &opt2)) {
        delete opt2;
    }
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


Pass * PassMgr::allocScalarOpt()
{
    return new ScalarOpt(m_rg);
}


Pass * PassMgr::allocMDLivenessMgr()
{
    return new MDLivenessMgr(m_rg);
}


Pass * PassMgr::allocRefine()
{
    return new Refine(m_rg);
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
    case PASS_SCALAR_OPT:
        pass = allocScalarOpt();
        break;
    case PASS_MDLIVENESS_MGR:
        pass = allocMDLivenessMgr();
        break;
    case PASS_REFINE:
        pass = allocRefine();
        break;
    default: ASSERTN(0, ("Unsupport Optimization."));
    }

    ASSERT0(opty != PASS_UNDEF && pass);
    m_registered_pass.set(opty, pass);
    return pass;
}

} //namespace xoc
