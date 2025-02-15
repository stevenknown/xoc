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
#ifndef _APPLY_TO_REGION_H_
#define _APPLY_TO_REGION_H_

namespace xoc {

//The class switch IRBBMgr of given region.
class UseNewIRBBMgr {
    IRBBMgr * m_org_bbmgr;
    IRBBMgr * m_new_bbmgr;
    Region const* m_rg;
public:
    UseNewIRBBMgr(Region const* rg, IRBBMgr * bbmgr)
    {
        m_rg = rg;
        m_org_bbmgr = bbmgr;
        m_new_bbmgr = new IRBBMgr(rg);
        m_rg->setBBMgr(m_new_bbmgr);
    }
    ~UseNewIRBBMgr()
    {
        ASSERT0(m_org_bbmgr && m_new_bbmgr);
        m_rg->setBBMgr(m_org_bbmgr);
        delete m_new_bbmgr;
    }
    IRBBMgr * getNew() const { return m_new_bbmgr; }
    IRBBMgr * getOrg() const { return m_org_bbmgr; }
};


//The class switch IRMgr of given region.
class UseNewIRMgr {
    IRMgr * m_org_mgr;
    IRMgr * m_new_mgr;
    Region const* m_rg;
public:
    UseNewIRMgr(Region const* rg, IRMgr * irmgr)
    {
        m_rg = rg;
        m_org_mgr = irmgr;
        ASSERT0(m_rg->getPassMgr());
        m_new_mgr = (IRMgr*)m_rg->getPassMgr()->allocPass(PASS_IRMGR);
        m_new_mgr->setIRCount(m_org_mgr->getIRCount());
        m_rg->setIRMgr(m_new_mgr);
    }
    ~UseNewIRMgr()
    {
        ASSERT0(m_org_mgr && m_new_mgr);
        m_rg->setIRMgr(m_org_mgr);
        m_rg->getPassMgr()->destroyPass(m_new_mgr);
    }
    IRMgr * getNew() const { return m_new_mgr; }
    IRMgr * getOrg() const { return m_org_mgr; }
};


//The class switch BBList of given region.
//Note the class will clone new BB via given IRBBMgr.
class UseNewBBList {
    BBList * m_org_bblst;
    BBList * m_new_bblst;
    IRBBMgr * m_new_bbmgr; //record user generated new IRBBMgr.
    Region const* m_rg;
public:
    UseNewBBList(Region const* rg, BBList * bblst, MOD IRBBMgr * newbbmgr)
    {
        ASSERT0(newbbmgr);
        m_rg = rg;
        m_org_bblst = bblst;
        m_new_bbmgr = newbbmgr;
        m_new_bblst = new BBList();
        m_new_bblst->clone(*bblst, m_new_bbmgr, rg);
        m_rg->setBBList(m_new_bblst);
    }
    ~UseNewBBList()
    {
        BBListIter it;
        for (IRBB * bb = m_new_bblst->get_head(&it);
             bb != nullptr; bb = m_new_bblst->get_next(&it)) {
            m_new_bbmgr->destroyBB(bb);
        }
        delete m_new_bblst;
        m_rg->setBBList(m_org_bblst);
    }
    BBList * getNew() const { return m_new_bblst; }
    BBList * getOrg() const { return m_org_bblst; }
};


//The class switch IRCFG of given region.
class UseNewCFG {
    IRCFG * m_org_cfg;
    IRCFG * m_new_cfg;
    Region const* m_rg;
public:
    UseNewCFG(Region const* rg, IRCFG * cfg, BBList * newbblst)
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
    ~UseNewCFG()
    {
        ASSERT0(m_org_cfg && m_new_cfg);
        m_rg->setCFG(m_org_cfg);
        m_new_cfg->setBBList(nullptr);
        delete m_new_cfg;
    }
    IRCFG * getNew() const { return m_new_cfg; }
    IRCFG * getOrg() const { return m_org_cfg; }
};


//
//START ApplyToRegion
//
class ApplyToRegion {
    COPY_CONSTRUCTOR(ApplyToRegion);
protected:
    typedef xcom::Stack<UseNewIRMgr*> IRMgrStack;
    typedef xcom::Stack<UseNewIRMgr*>::Iter IRMgrStackIter;
    typedef xcom::Stack<UseNewIRBBMgr*> IRBBMgrStack;
    typedef xcom::Stack<UseNewIRBBMgr*>::Iter IRBBMgrStackIter;
    typedef xcom::Stack<UseNewBBList*> BBListStack;
    typedef xcom::Stack<UseNewBBList*>::Iter BBListStackIter;
    typedef xcom::Stack<UseNewCFG*> CFGStack;
    typedef xcom::Stack<UseNewCFG*>::Iter CFGStackIter;
public:
    class ApplyDataStruct {
    public:
        IRMgr const* irmgr;
        IRBBMgr const* bbmgr;
        BBList const* bblist;
        IRCFG const* cfg;
    public:
        ApplyDataStruct() { clean(); }
        void clean() { ::memset(this, 0, sizeof(ApplyDataStruct));  }
    };
protected:
    Region const* m_rg;
    IRMgrStack m_irmgr_stack;
    IRBBMgrStack m_bbmgr_stack;
    BBListStack m_bblist_stack;
    CFGStack m_cfg_stack;
public:
    ApplyToRegion(Region const* rg) { m_rg = rg; }
    ~ApplyToRegion()
    {
        UINT n = m_irmgr_stack.get_elem_count();
        ASSERT0(n == m_bbmgr_stack.get_elem_count());
        ASSERT0(n == m_bblist_stack.get_elem_count());
        ASSERT0(n == m_cfg_stack.get_elem_count());
        for (; m_irmgr_stack.get_top() != nullptr;) {
            pop();
        }
        ASSERT0(m_irmgr_stack.get_top() == nullptr);
        ASSERT0(m_bbmgr_stack.get_top() == nullptr);
        ASSERT0(m_bblist_stack.get_top() == nullptr);
        ASSERT0(m_cfg_stack.get_top() == nullptr);
    }

    //Get the nth data structures of Region in stack.
    void getTopNthDataStruct(UINT n, OUT ApplyDataStruct & ds) const
    {
        UINT cnt = m_irmgr_stack.get_elem_count();
        ASSERT0(cnt == m_bbmgr_stack.get_elem_count());
        ASSERT0(cnt == m_bblist_stack.get_elem_count());
        ASSERT0(cnt == m_cfg_stack.get_elem_count());
        ds.clean();
        if (n >= m_irmgr_stack.get_elem_count()) { return; }
        {
            IRMgrStackIter it;
            UseNewIRMgr const* u = m_irmgr_stack.get_top_nth(n, &it);
            ASSERT0(u);
            ds.irmgr = u->getOrg();
        }
        {
            IRBBMgrStackIter it;
            UseNewIRBBMgr const* u = m_bbmgr_stack.get_top_nth(n, &it);
            ASSERT0(u);
            ds.bbmgr = u->getOrg();
        }
        {
            BBListStackIter it;
            UseNewBBList const* u = m_bblist_stack.get_top_nth(n, &it);
            ASSERT0(u);
            ds.bblist = u->getOrg();
        }
        {
            CFGStackIter it;
            UseNewCFG const* u = m_cfg_stack.get_top_nth(n, &it);
            ASSERT0(u);
            ds.cfg = u->getOrg();
        }
    }

    //The function applies new Region dependent data structures, and
    //push last data structures into a stack.
    //For now, the data structures include IRMgr, IRBBMgr, BBList, and IRCFG.
    void push()
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

    //The function destorys current Region dependent data structures that have
    //been pushed on the top of stack, and restore the last pushed Region
    //dependent data structures.
    //For now, the data structures include IRMgr, IRBBMgr, BBList, and IRCFG.
    void pop()
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
};
//END ApplyToRegion

} //namespace xoc
#endif
