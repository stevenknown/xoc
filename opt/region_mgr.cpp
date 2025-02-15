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
//START RegionMgr
//
RegionMgr::RegionMgr() : m_type_mgr(this)
{
    #ifdef _DEBUG_
    m_num_allocated = 0;
    #endif
    m_rg_count = REGION_ID_UNDEF + 1;
    m_label_count = LABEL_ID_UNDEF + 1;
    m_var_mgr = nullptr;
    m_var_label_relation_mgr = nullptr;
    m_md_sys = nullptr;

    //Set to false by default to get more opportunities to optimizations.
    m_is_regard_str_as_same_md = false;
    m_str_md = nullptr;
    m_targinfo = nullptr;
    m_program = nullptr;
    m_dm = nullptr;
    m_pool = smpoolCreate(64, MEM_COMM);
    m_logmgr = new LogMgr();
    m_targinfo_mgr = nullptr;
    initIRDescFlagSet();
}


RegionMgr::~RegionMgr()
{
    for (VecIdx id = 0; id <= m_id2rg.get_last_idx(); id++) {
        Region * rg = m_id2rg.get(id);
        if (rg == nullptr) { continue; }
        deleteRegion(rg, false);
    }
    m_id2rg.clean();

    #ifdef _DEBUG_
    ASSERTN(m_num_allocated == 0, ("there is still region leave out"));
    #endif

    if (m_md_sys != nullptr) {
        delete m_md_sys;
        m_md_sys = nullptr;
    }
    if (m_var_mgr != nullptr) {
        delete m_var_mgr;
        m_var_mgr = nullptr;
    }
    if (m_var_label_relation_mgr != nullptr) {
        delete m_var_label_relation_mgr;
        m_var_label_relation_mgr = nullptr;
    }
    m_id2optctx.clean();
    smpoolDelete(m_pool);
    m_pool = nullptr;
    delete m_logmgr;
    m_logmgr = nullptr;
    if(m_dm != nullptr) {
        delete m_dm;
        m_dm = nullptr;
    }

    if (m_targinfo != nullptr) {
        delete m_targinfo;
        m_targinfo = nullptr;
    }
    #ifdef REF_TARGMACH_INFO
    if (m_targinfo_mgr != nullptr) {
        //Note if user enable and reference TargInfoMgr, the macro
        //REF_TARGMACH_INFO has to be opend. And TargInfoMgr will reference
        //xgen's data structure.
        delete m_targinfo_mgr;
        m_targinfo_mgr = nullptr;
    }
    #endif
}


void RegionMgr::initIRDescFlagSet()
{
    //NOTE: If new IR flag value is greater than the bit range that
    //IRDescFlagSeg can express, user should extend the
    //IRDescFlagSeg and IRDescFlagSegNum value and set the big flag value here,
    //then invoke the function right after RegionMgr created.
    //e.g: Assume we are going to add a new flag IRC_NEW_FEAT which value
    //is 117 to IR_VST, the code is:
    //IRDES_attr(g_ir_desc[IR_VST]).set(IRC_NEW_FEAT);
}


void * RegionMgr::xmalloc(UINT size)
{
    ASSERTN(m_pool != nullptr, ("pool not initialized"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset((void*)p, 0, size);
    return p;
}


OptCtx * RegionMgr::allocOptCtx()
{
    return (OptCtx*)xmalloc(sizeof(OptCtx));
}


OptCtx * RegionMgr::getAndGenOptCtx(Region * rg)
{
    OptCtx * oc = m_id2optctx.get(rg->id());
    if (oc == nullptr) {
        oc = allocOptCtx();
        oc->init(rg);
        m_id2optctx.set(rg->id(), oc);
    }
    return oc;
}


MD const* RegionMgr::genDedicateStrMD()
{
    if (!m_is_regard_str_as_same_md) { return nullptr; }

    //Regard all string variables as same unbound MD.
    if (m_str_md == nullptr) {
        Sym const* s = addToSymbolTab("DedicatedVarBeRegardedAsString");
        Var * sv = getVarMgr()->registerStringVar(DEDICATED_STRING_VAR_NAME, s,
                                                  MEMORY_ALIGNMENT);
        sv->setFlag((VAR_FLAG)(VAR_IS_UNALLOCABLE|VAR_ADDR_TAKEN));
        MD md;
        MD_base(&md) = sv;
        MD_ty(&md) = MD_UNBOUND;
        ASSERT0(MD_base(&md)->is_string());
        MD const* e = m_md_sys->registerMD(md);
        ASSERT0(MD_id(e) > 0);
        m_str_md = e;
    }
    return m_str_md;
}


//Register exact MD for each global variable.
//Note you should call this function as early as possible, e.g, before process
//all regions. Because that will assign smaller MD id to global variable.
void RegionMgr::registerGlobalMD()
{
    //Only top region can do initialize MD for global variable.
    ASSERT0(m_var_mgr);
    VarVec * varvec = m_var_mgr->getVarVec();
    for (VecIdx i = 0; i <= varvec->get_last_idx(); i++) {
        Var * v = varvec->get(i);
        if (v == nullptr || v->is_local()) { continue; }
        //Note Var is regarded as VAR_GLOBAL by default if VAR_LOCAL not set.

        //User sometime intentionally declare non-allocable
        //global variable to custmized usage.
        //ASSERT0(!v->is_unallocable());

        if (v->is_string() && genDedicateStrMD() != nullptr) {
            //Treat all string variables as the same one.
            continue;
        }

        //We allocate MDTab for Var which is func-decl or fake as well.
        //Since some Passes such as AA may need fake Var to do analysis.
        MD md;
        MD_base(&md) = v;
        MD_ofst(&md) = 0;
        MD_size(&md) = v->is_any() ? 0 : v->getByteSize(getTypeMgr());
        if (v->is_fake() || v->is_func() || v->is_any()) {
            MD_ty(&md) = MD_UNBOUND;
        } else {
            MD_ty(&md) = MD_EXACT;
        }
        m_md_sys->registerMD(md);
    }
}


VarMgr * RegionMgr::allocVarMgr()
{
    return new VarMgr(this);
}


MCDwarfMgr * RegionMgr::allocDwarfMgr()
{
    return new xoc::MCDwarfMgr();
}


VarLabelRelationMgr * RegionMgr::allocVarLabelRelationMgr()
{
    return new VarLabelRelationMgr();
}


TargInfo * RegionMgr::allocTargInfo()
{
    return nullptr;
}


TargInfoMgr * RegionMgr::allocTargInfoMgr()
{
    return nullptr;
}


Region * RegionMgr::allocRegion(REGION_TYPE rt)
{
    return new Region(rt, this);
}


void RegionMgr::initTargInfoMgr()
{
    ASSERTN(m_targinfo_mgr == nullptr,
            ("TargInfoMgr already initialized"));
    #ifdef REF_TARGMACH_INFO
    m_targinfo_mgr = allocTargInfoMgr();
    ASSERT0(m_targinfo_mgr);
    m_targinfo_mgr->init();
    #endif
}


Region * RegionMgr::newRegion(REGION_TYPE rt)
{
    #ifdef _DEBUG_
    m_num_allocated++;
    #endif

    Region * rg = allocRegion(rt);
    UINT free_id = m_free_rg_id.remove_head();
    if (free_id == REGION_ID_UNDEF) {
        REGION_id(rg) = m_rg_count++;
    } else {
        REGION_id(rg) = free_id;
    }
    return rg;
}


//Record new region and delete it when RegionMgr destroy.
void RegionMgr::addToRegionTab(Region * rg)
{
    ASSERTN(rg->id() > 0, ("should generate new region via newRegion()"));
    ASSERT0(getRegion(rg->id()) == nullptr);
    ASSERT0(rg->id() < m_rg_count);
    UINT pad = xcom::getNearestPowerOf2(rg->id());
    if (m_id2rg.get_elem_count() < pad) {
        m_id2rg.set(pad, nullptr);
    }
    m_id2rg.set(rg->id(), rg);
}


bool RegionMgr::verifyPreDefinedInfo()
{
    checkIRCodeBitSize();
    checkMaxIRCode();
    checkIRDesc();
    checkRoundDesc();
    checkIRSwitchCaseEntry();
    ASSERT0(WORD_LENGTH_OF_TARGET_MACHINE <=
            sizeof(TMWORD) * HOST_BIT_PER_BYTE);
    ASSERT0(sizeof(TMWORD) <= sizeof(HOST_UINT));
    ASSERT0(BIT_PER_BYTE == HOST_BIT_PER_BYTE);
    ASSERT0(sizeof(INT8) * HOST_BIT_PER_BYTE == 8);
    ASSERT0(sizeof(UINT8) * HOST_BIT_PER_BYTE == 8);
    ASSERT0(sizeof(INT16) * HOST_BIT_PER_BYTE == 16);
    ASSERT0(sizeof(UINT16) * HOST_BIT_PER_BYTE == 16);
    ASSERT0(sizeof(INT32) * HOST_BIT_PER_BYTE == 32);
    ASSERT0(sizeof(UINT32) * HOST_BIT_PER_BYTE == 32);
    ASSERT0(sizeof(INT64) * HOST_BIT_PER_BYTE == 64);
    ASSERT0(sizeof(UINT64) * HOST_BIT_PER_BYTE == 64);
    #ifdef INT128 //Host type support 128bit signed integer.
    ASSERT0(sizeof(INT128) * HOST_BIT_PER_BYTE == 128);
    #endif
    #ifdef UINT128 //Host type support 128bit unsigned integer.
    ASSERT0(sizeof(UINT128) * HOST_BIT_PER_BYTE == 128);
    #endif

    ASSERT0(!IS_UNSIGN_TY(INT8));
    ASSERT0(IS_UNSIGN_TY(UINT8));
    ASSERT0(!IS_UNSIGN_TY(INT16));
    ASSERT0(IS_UNSIGN_TY(UINT16));
    ASSERT0(!IS_UNSIGN_TY(INT32));
    ASSERT0(IS_UNSIGN_TY(UINT32));
    ASSERT0(!IS_UNSIGN_TY(INT64));
    ASSERT0(IS_UNSIGN_TY(UINT64));
    #ifdef INT128 //Host type support 128bit signed integer.
    ASSERT0(!IS_UNSIGN_TY(INT128));
    #endif
    #ifdef UINT128 //Host type support 128bit unsigned integer.
    ASSERT0(IS_UNSIGN_TY(UINT128));
    #endif

    //Host LONGLONG should not less than HOST_INT,
    //otherwise the integer might be truncated wrongfully.
    ASSERT0(sizeof(HOST_INT) <= sizeof(LONGLONG));
    ASSERT0(sizeof(HOST_INT) == sizeof(HOST_UINT));
    ASSERT0(sizeof(HOST_UINT) >= sizeof(UINT32));

    ASSERT0(WORD_LENGTH_OF_HOST_MACHINE ==
            (sizeof(HOST_UINT) * HOST_BIT_PER_BYTE));

    ASSERT0(sizeof(CHAR) == sizeof(UCHAR) &&
            sizeof(SHORT) == sizeof(USHORT) &&
            sizeof(INT) == sizeof(UINT) &&
            sizeof(LONG) == sizeof(ULONG) &&
            sizeof(LONGLONG) == sizeof(ULONGLONG));

    ASSERT0(sizeof(CHAR) <= sizeof(SHORT) &&
            sizeof(SHORT) <= sizeof(INT) &&
            sizeof(INT) <= sizeof(LONG) &&
            sizeof(LONG) <= sizeof(LONGLONG));

    ASSERT0(BYTE_PER_CHAR < BYTE_PER_SHORT &&
            BYTE_PER_SHORT < BYTE_PER_INT &&
            BYTE_PER_INT <= BYTE_PER_LONG &&
            BYTE_PER_LONG <= BYTE_PER_LONGLONG &&
            BYTE_PER_FLOAT < BYTE_PER_DOUBLE &&
            BYTE_PER_INT <= BYTE_PER_POINTER);

    ASSERT0(BYTE_PER_CHAR < sizeof(ULONGLONG) &&
            BYTE_PER_SHORT < sizeof(ULONGLONG) &&
            BYTE_PER_INT <= sizeof(ULONGLONG) &&
            BYTE_PER_LONG <= sizeof(ULONGLONG) &&
            BYTE_PER_FLOAT <= sizeof(ULONGLONG) &&
            BYTE_PER_DOUBLE <= sizeof(ULONGLONG) &&
            BYTE_PER_POINTER <= sizeof(ULONGLONG) &&
            GENERAL_REGISTER_SIZE <= sizeof(ULONGLONG));

    ASSERT0(BYTE_PER_CHAR <= sizeof(HOST_INT) &&
            BYTE_PER_CHAR <= sizeof(HOST_UINT) &&
            BYTE_PER_CHAR <= sizeof(HOST_FP));

    ASSERT0(BYTE_PER_SHORT <= sizeof(HOST_INT) &&
            BYTE_PER_SHORT <= sizeof(HOST_UINT) &&
            BYTE_PER_SHORT <= sizeof(HOST_FP));

    ASSERT0(BYTE_PER_INT <= sizeof(HOST_INT) &&
            BYTE_PER_INT <= sizeof(HOST_UINT) &&
            BYTE_PER_INT <= sizeof(HOST_FP));

    ASSERT0(IR_CODE_NUM <= ((1<<IR_CODE_BIT_SIZE) - 1));
    return true;
}


void RegionMgr::dumpRelationGraph(CHAR const* name)
{
    if (getNumOfRegion() == 0) { return; }
    if (name == nullptr) {
        name = "graph_region_relation_graph.vcg";
    }
    UNLINK(name);
    xcom::Graph g;
    for (UINT id = 0; id < getNumOfRegion(); id++) {
        Region * rg = getRegion(id);
        if (rg == nullptr || rg->getParent() == nullptr) { continue; }
        g.addEdge(rg->getParent()->id(), rg->id());
    }
    g.dumpVCG(name);
}


//Dump regions recorded via addToRegionTab().
void RegionMgr::dump(bool dump_inner_region)
{
    if (!isLogMgrInit() || getNumOfRegion() == 0) { return; }

    note(this, "\n==---- DUMP ALL Registered Region ----==");
    for (UINT id = 0; id < getNumOfRegion(); id++) {
        Region * rg = getRegion(id);
        if (rg == nullptr) { continue; }
        rg->dump(dump_inner_region);
    }
}


//This function destroy region, and free the region id
//to next region alloction.
void RegionMgr::deleteRegion(Region * rg, bool collect_id)
{
    START_TIMER_FMT(t, ("Delete Region%d", rg->id()));
    ASSERT0(rg);
    UINT id = rg->id();
    ASSERTN(getRegion(id), ("not registered region"));
    delete rg;

    if (collect_id && id != REGION_ID_UNDEF) {
        m_id2rg.set(id, nullptr);
        m_free_rg_id.append_head(id);
    }

    #ifdef _DEBUG_
    ASSERT0(m_num_allocated != 0);
    m_num_allocated--;
    #endif

    END_TIMER_FMT(t, ("Delete Region"));
}


//Process function level region.
bool RegionMgr::processFuncRegion(Region * func, OptCtx * oc)
{
    ASSERTN(!func->is_blackbox(),
            ("can not generate code for blackbox region"));
    return func->process(oc);
}


//Process top-level region.
//Top level region should be program.
bool RegionMgr::processProgramRegion(Region * program, OptCtx * oc)
{
    ASSERT0(program && program->is_program());
    return program->process(oc);
}
//END RegionMgr

} //namespace xoc
