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
class IPA;
class TargInfo;
class TargInfoMgr;
class MCDwarfMgr;
//
//START RegionMgr
//
//Region Manager is the top level manager.
#define RM_label_count(r) ((r)->m_label_count)
class RegionMgr {
public:
    typedef xcom::Vector<Region*> RegionTab;
private:
    COPY_CONSTRUCTOR(RegionMgr);
    friend class Region;
protected:
    bool m_is_regard_str_as_same_md;
#ifdef _DEBUG_
    UINT m_num_allocated;
#endif
    UINT m_rg_count; //record the number of region that has been allocated.
    UINT m_label_count; //record the number of label that has been allocated.
    VarMgr * m_var_mgr;
    VarLabelRelationMgr * m_var_label_relation_mgr;
    MD const* m_str_md;
    MDSystem * m_md_sys;
    TargInfo * m_targinfo;
    TargInfoMgr * m_targinfo_mgr;
    xcom::SMemPool * m_pool;
    LogMgr * m_logmgr;
    //For debug the context management of Dwarf.
    MCDwarfMgr * m_dm;
    Region * m_program;
    RegionTab m_id2rg;
    SymTab m_sym_tab;
    TypeMgr m_type_mgr;
    xcom::Vector<OptCtx*> m_id2optctx;
    xcom::BitSetMgr m_bs_mgr;
    xcom::DefMiscBitSetMgr m_sbs_mgr;

    //ID is an important resource that should be recycled.
    //The region ID will be recycled if a region destroy.
    //The data structure records the ID of region that can be
    //reused to assign to a new region.
    xcom::List<UINT> m_free_rg_id;
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

    //Allocate DwarfMgr.
    virtual MCDwarfMgr * allocDwarfMgr();

    //Allocate VarLabelRelationMgr.
    virtual VarLabelRelationMgr * allocVarLabelRelationMgr();

    //Allocate TargInfo.
    virtual TargInfo * allocTargInfo();

    //Allocate TargInfoMgr.
    virtual TargInfoMgr * allocTargInfoMgr();

    //Allocate IPA module.
    IPA * allocIPA(Region * program);

    //Check switch-case entry for each IR.
    virtual bool checkIRSwitchCaseInterface(IR_CODE c) const;
    bool checkIRSwitchCaseEntry() const;

    //Destroy specific region by given id.
    void deleteRegion(Region * rg, bool collect_id = true);

    void dumpRelationGraph(CHAR const* name);

    //Dump regions recorded via addToRegionTab().
    void dump(bool dump_inner_region);

    xcom::BitSetMgr * getBitSetMgr() { return &m_bs_mgr; }
    xcom::DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }
    virtual Region * getRegion(UINT id) { return m_id2rg.get(id); }
    UINT getNumOfRegion() const { return m_id2rg.get_elem_count(); }
    RegionTab * getRegionTab() { return &m_id2rg; }
    VarMgr * getVarMgr() { return m_var_mgr; }

    //The function generates a dedicated MD to represent string md.
    //Note the function regards all string variables as the same unbound MD.
    //e.g: android/external/tagsoup/src/org/ccil/cowan/tagsoup/HTMLSchema.java
    //There is a function allocates 3000+ string variable.
    //Each string has been taken address.
    //That will inflate may_point_to_set too much.
    //In this situation, AA can be conservatively regard all string variables
    //as same unbounded MD.
    MD const* genDedicateStrMD();
    MDSystem * getMDSystem() { return m_md_sys; }
    SymTab * getSymTab() { return &m_sym_tab; }
    TypeMgr * getTypeMgr() { return &m_type_mgr; }
    VarMgr * getVarMgr() const { return m_var_mgr; }
    VarLabelRelationMgr * getVarLabelRelationMgr() const
    { return m_var_label_relation_mgr; }
    TargInfo * getTargInfo() const { return m_targinfo; }
    TargInfoMgr * getTargInfoMgr() const { return m_targinfo_mgr; }
    LogMgr * getLogMgr() const { return m_logmgr; }
    OptCtx * getAndGenOptCtx(Region * rg);
    virtual Region * getProgramRegion() const { return m_program; }

    //Register exact MD for each global variable.
    //Note you should call this function as early as possible, e.g, before
    //process all regions. Because that will assign smaller MD id to global
    //variable.
    void registerGlobalMD();

    //Initialize the flags of specific IR.
    virtual void initIRDescFlagSet();

    //Initialize VarMgr structure and MD system.
    //It is the first thing you should do after you declared a RegionMgr.
    void initVarMgr()
    {
        ASSERTN(m_var_mgr == nullptr, ("VarMgr already initialized"));
        m_var_mgr = allocVarMgr();
        ASSERT0(m_var_mgr);

        ASSERTN(m_md_sys == nullptr, ("MDSystem already initialized"));
        m_md_sys = new MDSystem(m_var_mgr);
        ASSERT0(m_md_sys);
    }

    //Initialize DwarfMgr structure
    //It is the first thing you should do after you declared a RegionMgr.
    void initDwarfMgr()
    {
        ASSERTN(m_dm == nullptr, ("VarMgr already initialized"));
        m_dm = allocDwarfMgr();
        ASSERT0(m_dm);
    }


    //Initialize VarLabelRelationMgr structure.
    //You should do after you declared a RegionMgr if variable and label need
    //to be establish relationships.
    void initVarLabelRelationMgr()
    {
        ASSERTN(m_var_label_relation_mgr == nullptr,
                ("VarLabelRelationMgr already initialized"));
        m_var_label_relation_mgr = allocVarLabelRelationMgr();
        ASSERT0(m_var_label_relation_mgr);
    }

    //Initialize TargInfo.
    //It is the first thing one should do after declaring RegionMgr.
    void initTargInfo()
    {
        ASSERTN(m_targinfo == nullptr, ("TargInfo already initialized"));
        m_targinfo = allocTargInfo();
        ASSERT0(m_targinfo);
        ASSERT0(verifyPreDefinedInfo());
    }

    //Initialize TargInfoMgr.
    void initTargInfoMgr()
    {
        ASSERTN(m_targinfo_mgr == nullptr,
                ("TargInfoMgr already initialized"));
        m_targinfo_mgr = allocTargInfoMgr();
        ASSERT0(m_targinfo_mgr);
    }

    bool isLogMgrInit() const
    { return const_cast<RegionMgr*>(this)->getLogMgr()->is_init(); }

    //Return true if all compilation process to regard all
    //string variables as a same unbound MD.
    //e.g: android/external/tagsoup/src/org/ccil/cowan/tagsoup/HTMLSchema.java
    //There is a function allocates 3000+ string variable.
    //Each string has been taken address.
    //That will inflate may_point_to_set too much.
    //In this situation, AA can be conservatively regard all string variables
    //as same unbounded MD.
    bool isRegardAllStringAsSameMD() const
    { return m_is_regard_str_as_same_md; }

    Region * newRegion(REGION_TYPE rt);

    //Process region in the form of function type.
    virtual bool processFuncRegion(IN Region * func, OptCtx * oc);

    //Process top-level region unit.
    //Top level region unit should be program unit.
    virtual bool processProgramRegion(IN Region * program, OptCtx * oc);

    void setProgramRegion(Region * rg) { m_program = rg; }

    //The function will demand all compilation process to regard all
    //string variables as a same unbound MD.
    //e.g: android/external/tagsoup/src/org/ccil/cowan/tagsoup/HTMLSchema.java
    //There is a function that allocated 3000+ string variables.
    //Each string has been taken address.
    //That will inflate may_point_to_set too much.
    //In this situation, AA can be conservatively regard all string variables
    //as same unbounded MD.
    void setRegardAllStringAsSameMD(bool doit)
    { m_is_regard_str_as_same_md = doit; }

    bool verifyPreDefinedInfo();

    xoc::MCDwarfMgr * getDwarfMgr() { return m_dm; }
};
//END RegionMgr

} //namespace xoc
#endif
