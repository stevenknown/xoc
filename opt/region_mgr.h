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
#ifndef __REGION_MGR_H__
#define __REGION_MGR_H__

namespace xoc {

#define REGION_ID_UNDEF 0
#define LABEL_ID_UNDEF 0

typedef enum {
    REGION_UNDEF = 0,

    //Region is black box with
    //Black box is single entry, single exit.
    REGION_BLACKBOX,

    //Sub region is the region which contains a list of IRs,
    //Sub region must be single entry, multiple exits.
    REGION_INNER,

    //Region is exception region.
    //Exception is single entry, multiple exits.
    REGION_EH,

    //Region is function unit
    //Function unit is single entry, multiple exits.
    REGION_FUNC,

    //Region is whole program spectrum.
    //Program region is single entry, multiple exits.
    REGION_PROGRAM,
} REGION_TYPE;

class Region;
class CallGraph;
class IPA;
class TargInfo;

//
//START RegionMgr
//
//Region Manager is the top level manager.
#define RM_label_count(r) ((r)->m_label_count)
class RegionMgr {
    COPY_CONSTRUCTOR(RegionMgr);
    friend class Region;
protected:
#ifdef _DEBUG_
    UINT m_num_allocated;
#endif
    Vector<Region*> m_id2ru;
    Vector<OptCtx*> m_id2optctx;
    xcom::BitSetMgr m_bs_mgr;
    xcom::DefMiscBitSetMgr m_sbs_mgr;
    SymTab m_sym_tab;
    TypeMgr m_type_mgr;
    VarMgr * m_var_mgr;
    MD const* m_str_md;
    MDSystem * m_md_sys;
    CallGraph * m_call_graph;
    UINT m_ru_count;
    List<UINT> m_free_ru_id;
    UINT m_label_count;
    bool m_is_regard_str_as_same_md;
    TargInfo * m_targinfo;
    SMemPool * m_pool;
    LogMgr * m_logmgr;

protected:
    void estimateEV(OUT UINT & num_call, OUT UINT & num_ru,
                    bool scan_call, bool scan_inner_region);

    void * xmalloc(UINT size);

public:
    RegionMgr();
    virtual ~RegionMgr();

    Sym const* addToSymbolTab(CHAR const* s) { return m_sym_tab.add(s); }

    //This function will establish a map between region and its id.
    void addToRegionTab(Region * rg);

    //Allocate Region.
    virtual Region * allocRegion(REGION_TYPE rt);

    //Allocate VarMgr.
    virtual VarMgr * allocVarMgr();

    //Allocate TargInfo.
    virtual TargInfo * allocTargInfo();

    //Allocate CallGraph.
    virtual CallGraph * allocCallGraph(UINT edgenum, UINT vexnum);

    //Allocate IPA module.
    IPA * allocIPA(Region * program);

    //Destroy specific region by given id.
    void deleteRegion(Region * rg, bool collect_id = true);

    void dumpRelationGraph(CHAR const* name);

    //Dump regions recorded via addToRegionTab().
    void dump(bool dump_inner_region);

    xcom::BitSetMgr * getBitSetMgr() { return &m_bs_mgr; }
    xcom::DefMiscBitSetMgr * get_sbs_mgr() { return &m_sbs_mgr; }
    virtual Region * getRegion(UINT id) { return m_id2ru.get(id); }
    UINT getNumOfRegion() const { return (UINT)(m_id2ru.get_last_idx() + 1); }
    VarMgr * getVarMgr() { return m_var_mgr; }
    MD const* genDedicateStrMD();
    MDSystem * getMDSystem() { return m_md_sys; }
    SymTab * getSymTab() { return &m_sym_tab; }
    TypeMgr * getTypeMgr() { return &m_type_mgr; }
    CallGraph * getCallGraph() const { return m_call_graph; }
    VarMgr * getVarMgr() const { return m_var_mgr; }
    TargInfo * getTargInfo() const { return m_targinfo; }
    LogMgr * getLogMgr() const { return m_logmgr; }
    OptCtx * getAndGenOptCtx(UINT id);

    //Register exact MD for each global variable.
    //Note you should call this function as early as possible, e.g, before
    //process all regions. Because that will assign smaller MD id to global
    //variable.
    void registerGlobalMD();

    //Initialize VarMgr structure and MD system.
    //It is the first thing you should do after you declared a RegionMgr.
    inline void initVarMgr()
    {
        ASSERTN(m_var_mgr == nullptr, ("VarMgr already initialized"));
        m_var_mgr = allocVarMgr();
        ASSERT0(m_var_mgr);

        ASSERTN(m_md_sys == nullptr, ("MDSystem already initialized"));
        m_md_sys = new MDSystem(m_var_mgr);
        ASSERT0(m_md_sys);
    }

    //Initialize TargInfo.
    //It is the first thing one should do after declaring RegionMgr.
    void initTargInfo()
    {
        ASSERTN(m_targinfo == nullptr, ("TargInfo already initialized"));
        m_targinfo = allocTargInfo();
        ASSERT0(m_targinfo);
    }
    bool isLogMgrInit() const
    { return const_cast<RegionMgr*>(this)->getLogMgr()->is_init(); }

    //Scan call site and build call graph.
    void buildCallGraph(OptCtx & oc, bool scan_call, bool scan_inner_region);

    Region * newRegion(REGION_TYPE rt);

    //Note m_targinfo will be destructed when RegionMgr is destructed.
    //m_targinfo should be set to nullptr if given TargInfo object is not
    //allocated by allocTargInfo() of current RegionMgr.
    void setTargInfo(TargInfo * ti)
    {
        ASSERTN(m_targinfo == nullptr, ("TargInfo has already been set"));
        m_targinfo = ti;
    }

    //Process region in the form of function type.
    virtual bool processFuncRegion(IN Region * func, OptCtx * oc);

    //Process top-level region unit.
    //Top level region unit should be program unit.
    virtual bool processProgramRegion(IN Region * program, OptCtx * oc);

    bool verifyPreDefinedInfo();
};
//END RegionMgr

} //namespace xoc
#endif
