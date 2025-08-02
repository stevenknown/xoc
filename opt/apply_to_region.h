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

class Region;
class IRBBMgr;

//The class switch IRBBMgr of given region.
class UseNewIRBBMgr {
    IRBBMgr * m_org_bbmgr;
    IRBBMgr * m_new_bbmgr;
    Region const* m_rg;
public:
    UseNewIRBBMgr(Region const* rg, IRBBMgr * bbmgr);
    ~UseNewIRBBMgr();
    IRBBMgr * getNew() const { return m_new_bbmgr; }
    IRBBMgr * getOrg() const { return m_org_bbmgr; }
};


//The class switch IRMgr of given region.
class UseNewIRMgr {
    IRMgr * m_org_mgr;
    IRMgr * m_new_mgr;
    Region const* m_rg;
public:
    UseNewIRMgr(Region const* rg, IRMgr * irmgr);
    ~UseNewIRMgr();
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
    UseNewBBList(Region const* rg, BBList * bblst, MOD IRBBMgr * newbbmgr);
    ~UseNewBBList();
    BBList * getNew() const { return m_new_bblst; }
    BBList * getOrg() const { return m_org_bblst; }
};


//The class switch IRCFG of given region.
class UseNewCFG {
    IRCFG * m_org_cfg;
    IRCFG * m_new_cfg;
    Region const* m_rg;
public:
    UseNewCFG(Region const* rg, IRCFG * cfg, BBList * newbblst);
    ~UseNewCFG();
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
    void push();

    //The function destorys current Region dependent data structures that have
    //been pushed on the top of stack, and restore the last pushed Region
    //dependent data structures.
    //For now, the data structures include IRMgr, IRBBMgr, BBList, and IRCFG.
    void pop();
};
//END ApplyToRegion

} //namespace xoc
#endif
