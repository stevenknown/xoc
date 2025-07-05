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
//START UseNewIRBBMgr
//
UseNewIRBBMgr::UseNewIRBBMgr(Region const* rg, IRBBMgr * bbmgr)
{
    m_rg = rg;
    m_org_bbmgr = bbmgr;
    m_new_bbmgr = new IRBBMgr(rg);
    m_rg->setBBMgr(m_new_bbmgr);
}


UseNewIRBBMgr::~UseNewIRBBMgr()
{
    ASSERT0(m_org_bbmgr && m_new_bbmgr);
    m_rg->setBBMgr(m_org_bbmgr);
    delete m_new_bbmgr;
}
//END UseNewIRBBMgr


//
//START UseNewIRMgr
//
UseNewIRMgr::UseNewIRMgr(Region const* rg, IRMgr * irmgr)
{
    m_rg = rg;
    m_org_mgr = irmgr;
    ASSERT0(m_rg->getPassMgr());
    m_new_mgr = (IRMgr*)m_rg->getPassMgr()->allocPass(PASS_IRMGR);
    m_new_mgr->setIRCount(m_org_mgr->getIRCount());
    m_rg->setIRMgr(m_new_mgr);
}


UseNewIRMgr::~UseNewIRMgr()
{
    ASSERT0(m_org_mgr && m_new_mgr);
    m_rg->setIRMgr(m_org_mgr);
    m_rg->getPassMgr()->destroyPass(m_new_mgr);
}
//END UseNewIRMgr


//
//START UseNewBBList
//
UseNewBBList::UseNewBBList(Region const* rg, BBList * bblst, MOD IRBBMgr * newbbmgr)
{
    ASSERT0(newbbmgr);
    m_rg = rg;
    m_org_bblst = bblst;
    m_new_bbmgr = newbbmgr;
    m_new_bblst = new BBList();
    m_new_bblst->clone(*bblst, m_new_bbmgr, rg);
    m_rg->setBBList(m_new_bblst);
}
UseNewBBList::~UseNewBBList()
{
    BBListIter it;
    for (IRBB * bb = m_new_bblst->get_head(&it);
         bb != nullptr; bb = m_new_bblst->get_next(&it)) {
        m_new_bbmgr->destroyBB(bb);
    }
    delete m_new_bblst;
    m_rg->setBBList(m_org_bblst);
}
//END UseNewBBList


//
//START UseNewCFG
//
UseNewCFG::UseNewCFG(Region const* rg, IRCFG * cfg, BBList * newbblst)
{
    ASSERT0(newbblst);
    m_rg = rg;
    m_org_cfg = cfg;
    ASSERT0(m_rg->getPassMgr());
    //m_new_cfg = (IRCFG*)m_rg->getPassMgr()->allocPass(PASS_CFG);
    m_new_cfg = new IRCFG(*cfg, newbblst, false, false);
    m_new_cfg->setBBVertex();
    m_rg->setCFG(m_new_cfg);
}


UseNewCFG::~UseNewCFG()
{
    ASSERT0(m_org_cfg && m_new_cfg);
    m_rg->setCFG(m_org_cfg);
    m_new_cfg->setBBList(nullptr);
    delete m_new_cfg;
}
//END UseNewCFG


//
//START ApplyToRegion
//
void ApplyToRegion::push()
{
    //Push current IRMgr of region and adopt a new.
    UseNewIRMgr * usenewirmgr = new UseNewIRMgr(m_rg, m_rg->getIRMgr());
    ASSERT0(usenewirmgr->getNew() == m_rg->getIRMgr());
    m_irmgr_stack.push(usenewirmgr);

    //Push current IRBBMgr of region and adopt a new.
    UseNewIRBBMgr * usenewbbmgr = new UseNewIRBBMgr(
        m_rg, m_rg->getBBMgr());
    ASSERT0(usenewbbmgr->getNew() == m_rg->getBBMgr());
    m_bbmgr_stack.push(usenewbbmgr);

    //Push current BBList of region and adopt a new.
    UseNewBBList * usenewbblst = new UseNewBBList(
        m_rg, m_rg->getBBList(), usenewbbmgr->getNew());
    ASSERT0(usenewbblst->getNew() == m_rg->getBBList());
    m_bblist_stack.push(usenewbblst);

    //Push current CFG of region and adopt a new.
    UseNewCFG * usenewcfg = new UseNewCFG(
        m_rg, m_rg->getCFG(), usenewbblst->getNew());
    ASSERT0(usenewcfg->getNew() == m_rg->getCFG());
    m_cfg_stack.push(usenewcfg);
}


void ApplyToRegion::pop()
{
    UseNewCFG * usecfg = m_cfg_stack.pop();
    if (usecfg != nullptr) {
        ASSERT0(usecfg->getNew() == m_rg->getCFG());
        delete usecfg;
    }

    UseNewBBList * usebblst = m_bblist_stack.pop();
    if (usebblst != nullptr) {
        ASSERT0(usebblst->getNew() == m_rg->getBBList());
        delete usebblst;
    }

    UseNewIRBBMgr * usebbmgr = m_bbmgr_stack.pop();
    if (usebbmgr != nullptr) {
        ASSERT0(usebbmgr->getNew() == m_rg->getBBMgr());
        delete usebbmgr;
    }

    UseNewIRMgr * useirmgr = m_irmgr_stack.pop();
    if (useirmgr != nullptr) {
        ASSERT0(useirmgr->getNew() == m_rg->getIRMgr());
        delete useirmgr;
    }

    //Region dependent data structures have been updated to the last.
}
//END ApplyToRegion

} //namespace xoc
