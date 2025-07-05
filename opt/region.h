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
#ifndef __REGION_H__
#define __REGION_H__

namespace xoc {
class IPA;
class CfsMgr;
class DUMgr;
class AliasAnalysis;
class ExprTab;
class MDSSAMgr;
class PRSSAMgr;
class Pass;
class PassMgr;
class IRSimp;
class CDG;
class CallGraph;

//Region MD referrence info.
#define REF_INFO_maydef(ri) ((ri)->may_def_mds)
#define REF_INFO_mayuse(ri) ((ri)->may_use_mds)
class RefInfo {
    COPY_CONSTRUCTOR(RefInfo);
public:
    MDSet * may_def_mds; //Record the MD set for Region usage
    MDSet * may_use_mds; //Record the MD set for Region usage
public:
    //Count memory usage for current object.
    size_t count_mem()
    {
        size_t c = sizeof(RefInfo);
        return c;
    }
};

typedef xcom::List<xoc::Var const*> ConstVarList;
typedef xcom::List<xoc::Var const*>::Iter ConstVarListIter;
class ConstParamVarList : public ConstVarList {
protected:
    bool m_is_in_decl_order;
public:
    ConstParamVarList() { m_is_in_decl_order = false; }

    bool isInDeclOrder() const { return m_is_in_decl_order; }

    void setInDeclOrder(bool in_decl_order)
    { m_is_in_decl_order = in_decl_order; }
};

//
//START Region
//
//Record unique id in RegionMgr.
#define REGION_id(r) ((r)->m_id)
#define REGION_type(r) ((r)->m_rg_type)
#define REGION_parent(r) ((r)->m_parent)
#define REGION_region_mgr(r) ((r)->m_region_mgr)

//Set to true if Region is expected to be inlined.
#define REGION_is_expect_inline(r) ((r)->m_u2.s1.is_expect_inline)

//Set to true if Region can be inlined.
#define REGION_is_inlinable(r) ((r)->m_u2.s1.is_inlinable)

//True value means MustDef, MayDef, MayUse are available.
#define REGION_is_ref_valid(r) ((r)->m_u2.s1.is_ref_valid)

//True if region does not modify any memory and live-in variables which
//include Var and PR.
//This property is very useful if region is a blackbox.
//And readonly region will alleviate the burden of optimizor.
#define REGION_is_readonly(r) ((r)->m_u2.s1.is_readonly)

//Record memory reference for region.
#define REGION_refinfo(r) ((r)->m_ref_info)

//This class is base class to describe Region.
//A program organized in a set of region, each region has it own IR stmt
//list, local Var table, and many kinds of analysis and transformation
//modules.
//
//The kind of region can be blackbox, subregion, exception handling,
//function unit, and program unit.
//* blackbox
//  The region is a black box that attached customized data.
//  Black box is a single entry, single exit region.
//
//* subregion
//  This kind of region serves as a unit of compilation. Subregion
//  can be nested one inside another, which contains a list of IR.
//  Subregion is single entry, multiple exits region. The different
//  between subregion and function unit is subregion has neither
//  epilog and prolog part of function, nor the landing part of
//  exception handling. In general, subregions should be compiled
//  inside-out. An additional use of this node is to specify a region
//  to be parallelized.
//
//* exception handling
//  It is region that represent exception handler and try block.
//
//* function unit
//  The region represent normal function unit.
//  Function unit may be multiple entries, and multiple exits.
//  Most of optimizations apply on function unit. In general, the
//  compilation of region unit include following phases:
//    * Preprocessing IR and BB list to specify which variable is taken
//      address.
//    * High level process to perform miscellaneous high
//      level IR transformations according to syntactics analysis.
//      This process is also responsible to simpify high level IR
//      down to low level, and construct basic blocks.
//    * Both high level and middle level will perform these passes,
//      * Build control flow graph.
//      * Flow sensitive or insensitive alias analysis.
//      * Def-Use analysis.
//      * Control dependence analysis.
//    * Middle level optimizations are all based on low level IR,
//      such as copy propagation, dead code elimination, global value
//      numbering, and global common subexpression elimination.
//
//* program unit
//  The region represent all program unit. A program unit contains a
//  number of function units.
//  Program unit is single entry and multiple exits region.
//  XOC apply interprocedural analysis and optimization and inlining
//  on program unit.
class Region {
    friend class RegionMgr;
    friend class IRMgr;
    COPY_CONSTRUCTOR(Region);
protected:
    //Record the binary data for black box region.
    #define REGION_blackbox_data(r) ((r)->m_u1.m_blx_data)
    //Record analysis data structure for code region.
    #define REGION_analysis_instrument(r) ((r)->m_u1.m_ana_ins)
    union {
        AnalysisInstrument * m_ana_ins; //Analysis instrument.
        void * m_blx_data; //Black box data.
    } m_u1;
    SMemPool * m_pool;

    //Record vars defined in current region, include the subregion defined
    //variables. All LOCAL vars in the tab will be destroyed at region
    //destruction.
    VarTab m_rg_var_tab;
protected:
    //Allocate PassMgr
    virtual PassMgr * allocPassMgr();

    //Allocate dbxMgr
    virtual DbxMgr * allocDbxMgr();

    //Allocate AttachInfoMgr
    virtual AttachInfoMgr * allocAttachInfoMgr();

    //Clear the formal parameter list.
    void cleanFormalParamList()
    {
        delete m_formal_param_list;
        m_formal_param_list = nullptr;
    }

    void genEntryBB();
    AnalysisInstrument * getAnalysisInstrument() const
    {
        ASSERT0(hasAnaInstrument());
        return REGION_analysis_instrument(this);
    }

    void HighProcessImpl(OptCtx & oc);

    void scanCallListImpl(OUT UINT & num_inner_region, IR * irlst,
                          OUT List<IR const*> * call_list,
                          OUT List<IR const*> * ret_list,
                          bool scan_inner_region);

    virtual void postSimplify(MOD SimpCtx & simp, MOD OptCtx & oc);
    bool processIRList(OptCtx & oc);
    bool processBBList(OptCtx & oc);
    bool partitionRegion();
    bool performSimplifyImpl(MOD SimpCtx & simp, OptCtx & oc);

    //The function only simplies array ingredient operations.
    //Simplification will maintain CFG, PRSSA, MDSSA, and DU Ref information,
    //excepts the classic DU-Chain.
    bool performSimplifyArrayIngredient(OptCtx & oc);

    //The function perform normal simplifications, include CFG,
    //array operations etc.
    //Simplification will maintain CFG, PRSSA, MDSSA, and DU Ref information,
    //excepts the classic DU-Chain.
    virtual bool performSimplify(OptCtx & oc);
    bool processRegionIRInIRList(OptCtx & oc, IR const* ir);
    bool processRegionIRInIRList(OptCtx & oc);
    bool processRegionIRInBBList(OptCtx & oc);
public:
    REGION_TYPE m_rg_type; //region type.
    UINT m_id; //region unique id.
    Var * m_var; //record Var if RU has.
    Region * m_parent; //record parent region.
    RegionMgr * m_region_mgr; //Region manager.
    RefInfo * m_ref_info; //record USE/DEF MD info to current region.
    ConstParamVarList * m_formal_param_list; //record formal parameter.
    union {
        struct {
            BYTE is_expect_inline:1; //see above macro declaration.
            BYTE is_ref_valid:1;  //see above macro declaration.

            //True if region does not modify any live-in variables
            //which include Var and PR. We say the region is readonly.
            BYTE is_readonly:1;

            //True if region can be inlined. Default is false for
            //conservative purpose.
            BYTE is_inlinable:1;
        } s1;
        BYTE s1b1;
    } m_u2;
public:
    explicit Region(REGION_TYPE rt, RegionMgr * rm)
    { m_pool = nullptr; init(rt, rm); }
    virtual ~Region() { destroy(); }

    //Add var which used inside current or inner Region.
    //Once the region destructing, all local vars are deleted.
    void addToVarTab(Var * v) { m_rg_var_tab.append(v); }

    //Add irs into IR list of current region.
    void addToIRList(IR * irs)
    { xcom::add_next(&ANA_INS_ir_list(getAnalysisInstrument()), irs); }

    //Allocate DU reference that describes memory reference to IR.
    inline DU * allocDU() const
    {
        DU * du = ANA_INS_free_du_list(getAnalysisInstrument()).remove_head();
        if (du == nullptr) {
            du = (DU*)xcom::smpoolMallocConstSize(sizeof(DU),
                ANA_INS_du_pool(getAnalysisInstrument()));
            ::memset((void*)du, 0, sizeof(DU));
        }
        return du;
    }

    //Allocate basic block.
    IRBB * allocBB() { return getBBMgr()->allocBB(); }

    //Allocate AIContainer that describes attach info to IR.
    AIContainer * allocAIContainer() const
    {
        ASSERT0(getAnalysisInstrument()->getAttachInfoMgr());
        return getAnalysisInstrument()->getAttachInfoMgr()->allocAIContainer();
    }

    //Note the returned ByteBuf does not need to free by user.
    ByteBuf * allocByteBuf(UINT bytesize)
    {
        ByteBuf * buf = (ByteBuf*)xmalloc(sizeof(ByteBuf));
        BYTEBUF_size(buf) = bytesize;
        BYTEBUF_buffer(buf) = (BYTE*)xmalloc(bytesize);
        return buf;
    }

    //The function assigns a DummyUse to each non-intrisinc call-stmt in BB
    //or IR list.
    //Note DummyUse is usually used to build explicit DefUse chain.
    //e.g:given call foo(p); after assigning DummyUse, the function call will
    //become call:(use(ld x, ld y), p), where ld x, ld y are MayRef of
    //the function foo.
    void assignDummyUseForCall();

    //Construct IR list from BB list.
    //clean_ir_list: clean bb's ir list if it is true.
    IR * constructIRlist(bool clean_ir_list);

    //Construct BB list from IR list.
    //1. Split list of IRs into basic-block list.
    //2. Set BB propeties. e.g: entry-bb, exit-bb.
    void constructBBList();

    //Count memory usage for current object.
    size_t count_mem() const;

    //The funtion enables reallocating resource without by destructing region
    //object. Note the function should work together with init() to complete
    //the reallocation of resource.
    virtual void destroy();
    void destroyPassMgr();
    void destroyAttachInfoMgr();
    void destroyIRBBMgr();

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doPRSSA(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doMDSSA(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doRefineDU(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doMDRef(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doMDRefAndClassicDU(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doMDRefAndClassicPRDU(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doOnlyClassicNonPRDU(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doOnlyClassicPRDU(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doDUAna(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doAA(OptCtx & oc);

    //The function assigns MD to each direct memory-ref operations that include
    //PR operation and NonPR Direct-Mem-Ref operations.
    void doAssignDirectRefMD(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doSimplyCPByClassicDU(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doAggressiveAA(OptCtx & oc);

    //The funtion returns true means IR, BBList, MD ref,
    //DefUse chain or CFG etc informations changed, user should consider
    //whether other optimizations should be reperform again.
    //Otherwise return false means there is nothing changed.
    bool doBasicAnalysis(OptCtx & oc);

    //Duplication all contents of 'src', includes AttachInfo, except DU info,
    //SSA info, kids and siblings IR.
    IR * dupIR(IR const* ir) const;

    //Duplicate 'ir' and its kids, but without ir's sibiling node.
    //The duplication includes AI, except DU info, SSA info.
    IR * dupIRTree(IR const* ir) const;

    //Duplication 'ir' and kids, and its sibling, return list of new ir.
    //Duplicate irs start from 'ir' to the end of list.
    //The duplication includes AI, except DU info, SSA info.
    IR * dupIRTreeList(IR const* ir) const;

    //Duplication 'ir' and kids, but without ir's sibling node.
    //The function will generate the isomophic IR expression if given ir is
    //stmt.
    //ir: root of IR tree.
    IR * dupIsomoExpTree(IR const* ir);

    //Duplication 'ir' and kids, but without ir's sibling node.
    //The function will generate the isomophic IR stmt if given ir is
    //expression.
    //ir: root of IR tree.
    IR * dupIsomoStmt(IR const* ir, IR * rhs);

    //Dump IRBB list into given file.
    //filename: dump BB list into given filename.
    //dump_inner_region: true to dump inner-region.
    void dumpBBList(CHAR const* filename, bool dump_inner_region = true) const;

    //Dump IRBB list into log file.
    //dump_inner_region: true to dump inner-region.
    void dumpBBList(bool dump_inner_region = true) const;

    //Dump IR list into given file.
    //filename: dump IR list into given filename.
    void dumpIRList(CHAR const* filename, bool dump_inner_region = true) const;

    //Dump IR list into log file.
    void dumpIRList(UINT dumpflag = IR_DUMP_COMBINE) const;

    //Dump all irs and ordering by IR_id.
    void dumpAllIR() const { getIRMgr()->dump(); }

    //Dump each Var in current region's Var table.
    void dumpVARInRegion() const;

    //Dump all MDs that referenced by current region.
    void dumpRegionMayRef() const;

    //Dump all MD that related to Var.
    void dumpVarMD(Var * v, UINT indent) const;

    //Dump IR and memory usage.
    void dumpMemUsage() const;

    //Dump formal parameter list.
    void dumpParameter() const;
    void dumpVarTab() const;

    //Dump GR through LogMgr.
    //dump_inner_region: true to dump inner-region.
    void dumpGR(bool dump_inner_region = true) const;

    //Dump Region's IR BB list.
    //DUMP ALL BBList DEF/USE/OVERLAP_DEF/OVERLAP_USE.
    void dumpRef(UINT indent = 4) const;
    void dumpBBRef(IN IRBB * bb, UINT indent) const;
    void dump(bool dump_inner_region) const;

    //This function erases all informations of ir and
    //append it into free_list for next allocation.
    //If Attach Info exist, this function will erase it rather than deletion.
    //If DU info exist, this function will retrieve it back
    //to region for next allocation.
    //Note that this function does NOT free ir's kids and siblings.
    void freeIR(IR * ir) const;

    //Free ir and all its kids, except its sibling node.
    //We can only utilizing the function to free the
    //IR which allocated by 'allocIR'.
    //is_check_undef: If it is true, the function will assert when meeting
    //                an IR_UNDEF, that means some IRs has been freed already.
    void freeIRTree(IR * ir, bool is_check_undef = true) const;

    //Free ir, ir's sibling, and all its kids.
    //We can only utilizing the function to free the IR
    //which allocated by 'allocIR'.
    //NOTICE: If ir's sibling is not nullptr, that means the IR is
    //a high level type. IRBB consists of only middle/low level IR.
    //is_check_undef: If it is true, the function will assert when meeting
    //                an IR_UNDEF, that means some IRs has been freed already.
    void freeIRTreeList(IR * ir, bool is_check_undef = true) const;

    //Free ir, and all its kids.
    //We can only utilizing the function to free the IR which allocated
    //by 'allocIR'.
    //is_check_undef: If it is true, the function will assert when meeting
    //                an IR_UNDEF, that means some IRs has been freed already.
    void freeIRTreeList(IRList & irs, bool is_check_undef = true) const;

    //Free IRBB list.
    //We can only utilizing the function to free the IRBB
    //which allocated by 'allocBB'.
    //NOTICE: bb will not be destroyed, it is just recycled.
    void freeIRBBList(BBList & bbl);

    //Find the formal parameter list. If it has already been recorded, return it
    //directly. Otherwise, iterate the Var table of the current region to find
    //all Vars that are formal parameters.
    //in_decl_order: if it is true, this function sorts the formal parameters
    //in left-to-right order based on their declaration.
    //e.g: foo(a, b, c), varlst will be {a, b, c}.
    ConstVarList const* findAndRecordFormalParamList(bool in_decl_order);

    //This function iterate Var table of current region to
    //find all Var which are formal parameter.
    //in_decl_order: if it is true, this function will sort the formal
    //parameters in the Left to Right order according to their declaration.
    //e.g: foo(a, b, c), varlst will be {a, b, c}.
    //NOTE: the function will iterate all Vars in the region. It is very
    //costly and SHOULD BE USED CAREFULLY.
    void findFormalParam(OUT List<Var const*> & varlst, bool in_decl_order);

    //This function find the formal parameter variable at the specified position
    //from the parameter list. If the parameter list is nullptr, it first calls
    //findAndRecordFormalParamList to initialize the list.
    Var const* findFormalParam(UINT position);

    //This function find Var via iterating Var table of current region.
    Var * findVarViaSymbol(Sym const* sym) const;

    REGION_TYPE getRegionType() const { return REGION_type(this); }

    //Get the data in which blackbox region contained.
    void * getBlackBoxData() const
    { ASSERT0(is_blackbox()); return REGION_blackbox_data(this); }

    MDSystem * getMDSystem() const { return getRegionMgr()->getMDSystem(); }
    TypeMgr * getTypeMgr() const { return getRegionMgr()->getTypeMgr(); }
    MDMgr * getMDMgr() { return &ANA_INS_md_mgr(getAnalysisInstrument()); }

    //Get TargInfo that describes current target machine.
    TargInfo * getTargInfo() const
    { ASSERT0(getRegionMgr()); return getRegionMgr()->getTargInfo(); }

    //Get common memory pool of current region.
    SMemPool * getCommPool() const { return m_pool; }

    //Get the formal parameter list.
    //NOTICE: The formal parameter list may be nullptr.
    ConstVarList const* getFormalParamList() const
    { return m_formal_param_list; }

    //Get memory pool to allocate Single-List-Container.
    SMemPool * getSCPool() const
    { return ANA_INS_sc_labelinfo_pool(getAnalysisInstrument()); }

    //Get the maximum PRNO.
    UINT getPRCount() const
    { return ANA_INS_pr_count(getAnalysisInstrument()); }

    //Get the variable which represent current region.
    Var * getRegionVar() const { return m_var; }

    //Get the RegionMgr that current region belongs to.
    RegionMgr * getRegionMgr() const { return REGION_region_mgr(this); }

    //Get IR list if any.
    IR * getIRList() const { return ANA_INS_ir_list(getAnalysisInstrument()); }

    //Get the VarMgr related to current RegionMgr.
    VarMgr * getVarMgr() const
    { ASSERT0(getRegionMgr()); return getRegionMgr()->getVarMgr(); }

    //Get a variable table that includes all variables
    //declared in current region.
    VarTab * getVarTab() { return &m_rg_var_tab; }

    //Get BitSetMgr of current region.
    xcom::BitSetMgr * getBitSetMgr() const
    { return &ANA_INS_bs_mgr(getAnalysisInstrument()); }

    //Get Sparse|Dense BitSetMgr of current region.
    xcom::DefMiscBitSetMgr * getMiscBitSetMgr() const
    { return &ANA_INS_sbs_mgr(getAnalysisInstrument()); }

    //Get MDSetMgr of current region.
    MDSetMgr * getMDSetMgr() const
    { return &ANA_INS_mds_mgr(getAnalysisInstrument()); }

    //Get IRBB list if any.
    BBList * getBBList() const
    { return ANA_INS_ir_bb_list(getAnalysisInstrument()); }

    //Get IRBBMgr of current region.
    IRBBMgr * getBBMgr() const
    { return ANA_INS_ir_bb_mgr(getAnalysisInstrument()); }

    //Get the BB by given bbid.
    IRBB * getBB(UINT bbid) const { return getBBMgr()->getBB(bbid); }

    //Get MDSetHash.
    MDSetHash * getMDSetHash() const
    { return &ANA_INS_mds_hash(getAnalysisInstrument()); }

    //Return IR pointer via the unique IR_id.
    IR * getIR(UINT irid) const
    { return ANA_INS_ir_vec(getAnalysisInstrument()).get(irid); }

    //Return the vector that record all allocated IRs.
    xcom::Vector<IR*> & getIRVec() const
    { return ANA_INS_ir_vec(getAnalysisInstrument()); }

    //Return IRMgr.
    IRMgr * getIRMgr() const { return ANA_INS_ir_mgr(getAnalysisInstrument()); }

    //Return PassMgr.
    PassMgr * getPassMgr() const
    { return ANA_INS_pass_mgr(getAnalysisInstrument()); }

    //Return DbxMgr.
    DbxMgr * getDbxMgr() const
    { return ANA_INS_dbx_mgr(getAnalysisInstrument()); }

    //Return AttachInfoMgr.
    AttachInfoMgr * getAttachInfoMgr() const
    { return ANA_INS_ai_mgr(getAnalysisInstrument()); }

    //Return IRSimp.
    IRSimp * getIRSimp() const
    {
        return getPassMgr() != nullptr ?
               (IRSimp*)getPassMgr()->registerPass(PASS_IRSIMP) : nullptr;
    }

    //Return CDG.
    CDG * getCDG() const
    {
        return getPassMgr() != nullptr ?
               (CDG*)getPassMgr()->queryPass(PASS_CDG) : nullptr;
    }

    //Return IRCFG.
    IRCFG * getCFG() const
    {
        return getPassMgr() != nullptr ?
               (IRCFG*)getPassMgr()->queryPass(PASS_CFG) : nullptr;
    }

    //Return CallGraph.
    CallGraph * getCallGraph() const
    {
        return getPassMgr() != nullptr ?
               (CallGraph*)getPassMgr()->queryPass(PASS_CALL_GRAPH) : nullptr;
    }

    //If program region exist, return the CallGraph of it, otherwise return
    //NULL.
    CallGraph * getProgramRegionCallGraph() const;

    //The function try to get program region CallGraph. If there is no
    //Callgraph, return current region CallGraph.
    CallGraph * getCallGraphPreferProgramRegion() const;

    //Get Alias Analysis.
    AliasAnalysis * getAA() const
    {
        return getPassMgr() != nullptr ?
               (AliasAnalysis*)getPassMgr()->queryPass(PASS_AA) : nullptr;
    }

    //Return DU info manager.
    DUMgr * getDUMgr() const
    {
        return getPassMgr() != nullptr ?
               (DUMgr*)getPassMgr()->queryPass(PASS_DU_MGR) : nullptr;
    }

    //Return MDSSA manager.
    MDSSAMgr * getMDSSAMgr() const
    {
        return getPassMgr() != nullptr ?
               (MDSSAMgr*)getPassMgr()->queryPass(PASS_MDSSA_MGR) : nullptr;
    }

    //Return PRSSA manager.
    PRSSAMgr * getPRSSAMgr() const
    {
        return getPassMgr() != nullptr ?
               (PRSSAMgr*)getPassMgr()->queryPass(PASS_PRSSA_MGR) : nullptr;
    }

    Region * getParent() const { return REGION_parent(this); }
    CHAR const* getRegionName() const;
    Region * getFuncRegion();

    //Allocate and return all CALLs in the region.
    inline ConstIRList * getCallList()
    {
        if (ANA_INS_call_list(getAnalysisInstrument()) == nullptr) {
            ANA_INS_call_list(getAnalysisInstrument()) = new ConstIRList();
        }
        return ANA_INS_call_list(getAnalysisInstrument());
    }

    //Allocate and return a list of IR_RETURN in current Region.
    inline ConstIRList * getReturnList()
    {
        if (ANA_INS_return_list(getAnalysisInstrument()) == nullptr) {
            ANA_INS_return_list(getAnalysisInstrument()) = new ConstIRList();
        }
        return ANA_INS_return_list(getAnalysisInstrument());
    }

    //Get the MayDef MDSet of Region.
    MDSet * getMayDef() const
    { return m_ref_info != nullptr ? REF_INFO_maydef(m_ref_info) : nullptr; }

    //Generate the MayDef MDSet of Region.
    MDSet * genMayDef()
    {
        if (m_ref_info == nullptr) { return nullptr; }
        REF_INFO_maydef(m_ref_info) = getMDSetMgr()->alloc();
        return REF_INFO_maydef(m_ref_info);
    }

    //Get the MayUse MDSet of Region.
    MDSet * getMayUse() const
    { return m_ref_info != nullptr ? REF_INFO_mayuse(m_ref_info) : nullptr; }

    //Generate the MayUse MDSet of Region.
    MDSet * genMayUse()
    {
        if (m_ref_info == nullptr) { return nullptr; }
        REF_INFO_mayuse(m_ref_info) = getMDSetMgr()->alloc();
        return REF_INFO_mayuse(m_ref_info);
    }

    //Get the top parent level region.
    Region * getTopRegion()
    {
        Region * rg = this;
        while (rg->getParent() != nullptr) {
            rg = rg->getParent();
        }
        return rg;
    }

    //Allocate a internal LabelInfo that is not declared by compiler user.
    LabelInfo * genILabel()
    {
        LabelInfo * li = genILabel(RM_label_count(getRegionMgr()));
        RM_label_count(getRegionMgr())++;
        return li;
    }

    //Allocate a LabelInfo according to given 'labid'.
    LabelInfo * genILabel(UINT labid)
    {
        ASSERT0(labid <= RM_label_count(getRegionMgr()));
        LabelInfo * li = allocInternalLabel(getCommPool());
        LABELINFO_num(li) = labid;
        return li;
    }

    LabelInfo * genPragmaLabel(CHAR const* lab)
    { return genPragmaLabel(getRegionMgr()->addToSymbolTab(lab));}
    LabelInfo * genPragmaLabel(Sym const* labsym)
    {
        ASSERT0(labsym);
        return allocPragmaLabel(labsym, getCommPool());
    }

    LabelInfo * genCustomLabel(CHAR const* lab)
    { return genCustomLabel(getRegionMgr()->addToSymbolTab(lab));}
    LabelInfo * genCustomLabel(Sym const* labsym)
    {
        ASSERT0(labsym);
        return xoc::allocCustomerLabel(labsym, getCommPool());
    }

    //Allocate Var for PR.
    Var * genVarForPR(PRNO prno, Type const* type);

    //Map from prno to related Var.
    Var * getVarByPRNO(PRNO prno) const
    { return ANA_INS_prno2var(getAnalysisInstrument()).get((VecIdx)prno); }

    //Return the type for array index, the default is WORD length of target
    //machine.
    inline Type const* getTargetMachineArrayIndexType()
    {
        return getTypeMgr()->getSimplexTypeEx(
            getTypeMgr()->getAlignedDType(
            WORD_LENGTH_OF_TARGET_MACHINE, false));
    }

    //Use HOST_INT type describes the value.
    //The value can not exceed IR type's value range.
    HOST_INT getIntegerInDataTypeValueRange(IR * ir) const;
    HOST_INT getMaxInteger(UINT bitsize, bool is_signed) const;
    HOST_INT getMinInteger(UINT bitsize, bool is_signed) const;

    //Get the VarMgr related to current RegionMgr.
    LogMgr * getLogMgr() const
    { ASSERT0(getRegionMgr()); return getRegionMgr()->getLogMgr(); }

    //Perform high level optmizations.
    //Return true if processing finish successful, otherwise return false.
    virtual bool HighProcess(OptCtx & oc);

    //Return true if current region has Analysis Instrument.
    bool hasAnaInstrument() const
    { return is_function() || is_program() || is_inner() || is_eh(); }

    UINT id() const { return REGION_id(this); }

    //Initialze Region.
    //The funtion enables reallocating resource without by destructing region
    //object. Note the function should work together with destroy() to complete
    //the reallocation of resource.
    void init(REGION_TYPE rt, RegionMgr * rm);

    void initRefInfo()
    {
        if (m_ref_info != nullptr) { return; }
        m_ref_info = (RefInfo*)xmalloc(sizeof(RefInfo));
    }

    //Allocate and initialize pass manager.
    PassMgr * initPassMgr();

    //Allocate and initialize dbx manager.
    DbxMgr * initDbxMgr();

    //Allocate and initialize IR manager.
    IRMgr * initIRMgr();

    //Allocate and initialize IRBB manager.
    IRBBMgr * initIRBBMgr();

    //Allocate and initialize attachinfo manager.
    AttachInfoMgr * initAttachInfoMgr();
    bool isSafeToOptimize(IR const* ir);

    //Return true if ir belongs to current region.
    bool isRegionIR(IR const* ir) const;

    //Return true if Var belongs to current region.
    bool isRegionVAR(Var const* var) const;

    //Return true if Region name is equivalent to 'n'.
    //This function is helper function to faciltate user identify Region.
    bool isRegionName(CHAR const* n) const
    {
        ASSERT0(n);
        return getRegionName() != nullptr && ::strcmp(getRegionName(), n) == 0;
    }

    //Return true if Region type is undefined.
    bool is_undef() const { return getRegionType() == REGION_UNDEF; }

    //Return true if Region indicates function.
    //Function contains complete prolog and epilog,
    //analysis instructment as well.
    bool is_function() const { return getRegionType() == REGION_FUNC; }

    //Return true if Region indicates a blackbox.
    //Note blackbox Region does NOT contain analysis instrument.
    bool is_blackbox() const { return getRegionType() == REGION_BLACKBOX; }

    //Return true if Region indicates whole program.
    bool is_program() const { return getRegionType() == REGION_PROGRAM; }

    //Return true if current Region is an inner region that embeded other one.
    bool is_inner() const { return getRegionType() == REGION_INNER; }

    //Return true if Region is exception handler.
    bool is_eh() const { return getRegionType() == REGION_EH; }

    //Return true if Region only contain normal read MD, except volatile read.
    bool is_readonly() const { return REGION_is_readonly(this); }

    //Return true if Region's MD reference has been computed and is avaiable.
    bool is_ref_valid() const { return REGION_is_ref_valid(this); }

    bool isLogMgrInit() const { return getRegionMgr()->isLogMgrInit(); }

    //Perform middle level IR optimizations which are implemented
    //according to control flow info and data flow info.
    //Return true if processing finish successful, otherwise return false.
    virtual bool MiddleProcess(OptCtx & oc);

    //Construct BB list by destructing CFG.
    bool reconstructBBList(OptCtx & oc);

    //This function iterates over the Var table of the current region to
    //find all Vars that are formal parameters and records them in the formal
    //parameter list.
    //in_decl_order: if it is true, this function sorts the formal parameters
    //in left-to-right order based on their declaration.
    //e.g: foo(a, b, c), varlst will be {a, b, c}.
    ConstVarList const* refindAndRecordFormalParamList(bool in_decl_order);

    //Reinitialize current region.
    void reinit()
    {
        REGION_TYPE rt = getRegionType();
        RegionMgr * rm = getRegionMgr();
        destroy();
        init(rt, rm);
    }

    //Split IR list into list of basic blocks.
    //irs: a list of ir.
    //bbl: a list of bb.
    //ctbb: marker current bb container.
    //Note if CFG is invalid, it will not be updated.
    BBListIter splitIRlistIntoBB(IN IR * irs, OUT BBList * bbl,
                                 BBListIter ctbb, OptCtx const& oc);

    //Assign variable to given PR.
    void setMapPRNO2Var(PRNO prno, Var * pr_var)
    { ANA_INS_prno2var(getAnalysisInstrument()).set((VecIdx)prno, pr_var); }

    //Set the counter of PR.
    //Note 'cnt' will be assigned to next new PR, so it should have not be
    //referenced.
    void setPRCount(UINT cnt)
    { ANA_INS_pr_count(getAnalysisInstrument()) = cnt; }
    void setRegionVar(Var * v) { m_var = v; }
    void setIRList(IR * irs) const
    { ANA_INS_ir_list(getAnalysisInstrument()) = irs; }
    void setBlackBoxData(void * d) { REGION_blackbox_data(this) = d; }
    void setCommPool(SMemPool * pool) { m_pool = pool; }
    void setIRMgr(IRMgr * mgr) const
    { ANA_INS_ir_mgr(getAnalysisInstrument()) = mgr; }
    void setBBMgr(IRBBMgr * mgr) const
    { ANA_INS_ir_bb_mgr(getAnalysisInstrument()) = mgr; }
    void setBBList(BBList * bblst) const
    { ANA_INS_ir_bb_list(getAnalysisInstrument()) = bblst; }
    void setCFG(IRCFG * newcfg) const;

    //Collect information of CALL and RETURN in current region.
    //num_inner_region: count the number of inner regions.
    void scanCallAndReturnList(OUT UINT & num_inner_region,
                               OUT List<IR const*> * call_list,
                               OUT List<IR const*> * ret_list,
                               bool scan_inner_region);
    //Collect information of CALL and RETURN in current region.
    void scanCallAndReturnList(OUT UINT & num_inner_region,
                               bool scan_inner_region)
    {
        getCallList()->clean();
        getReturnList()->clean(); //Scan RETURN as well.
        scanCallAndReturnList(num_inner_region, getCallList(),
                              getReturnList(), scan_inner_region);
    }

    //Do simplification that lowering IR tree to lowest tree height.
    //The lowering should conform to the restriction declared in 'oc'.
    void lowerIRTreeToLowestHeight(OptCtx & oc);

    //This function is main entry to process current region.
    //Return true if the processing is successful.
    virtual bool process(OptCtx * oc);
    bool processRegionIR(IR const* ir);

    //This function is the entry to process inner region.
    //Return true if the processing is successful.
    virtual bool processInnerRegion(OptCtx * oc);

    //The funtion will re-construct IRBBMgr and re-initialize the related
    //BB list information.
    void reInitIRBBMgr()
    {
        destroyIRBBMgr();
        initIRBBMgr();
    }

    //The funtion will re-construct IRMgr.
    IRMgr * reInitIRMgr();

    //The function collect information that IPA may used.
    //Check and rescan call-list of region if something changed.
    void updateCallAndReturnList(bool scan_inner_region);

    //Ensure that each IR in ir_list must be allocated in crrent region.
    bool verifyIROwnership();

    //Allocate memory from region pool.
    void * xmalloc(UINT size);
    IR * xmallocIR(UINT size);
};
//END Region

} //namespace xoc
#endif
