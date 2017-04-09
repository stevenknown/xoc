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
RegionMgr::~RegionMgr()
{
    for (INT id = 0; id <= m_id2ru.get_last_idx(); id++) {
        Region * ru = m_id2ru.get(id);
        if (ru == NULL) { continue; }
        deleteRegion(ru, false);
    }

    #ifdef _DEBUG_
    ASSERT(m_num_allocated == 0, ("there is still region leave out"));
    #endif

    if (m_md_sys != NULL) {
        delete m_md_sys;
        m_md_sys = NULL;
    }

    if (m_var_mgr != NULL) {
        delete m_var_mgr;
        m_var_mgr = NULL;
    }

    if (m_call_graph != NULL) {
        delete m_call_graph;
        m_call_graph = NULL;
    }
}


//Try to generate a MD to represent dedicated string md.
//In case, e.g: android/external/tagsoup/src/org/ccil/cowan/tagsoup/HTMLSchema.java
//There is a function allocates 3000+ string variable.
//Each string has been taken address.
//That will inflate may_point_to_set too much.
//In this situation, AA can be conservatively regard all string variables
//as same unbounded MD.
MD const* RegionMgr::genDedicateStrMD()
{
    if (!m_is_regard_str_as_same_md) { return NULL; }

    //Regard all string variables as same unbound MD.
    if (m_str_md == NULL) {
        SYM * s = addToSymbolTab("DedicatedVarBeRegardedAsString");
        VAR * sv = getVarMgr()->registerStringVar("DedicatedStringVar", s, 1);
        VAR_allocable(sv) = false;
        VAR_is_addr_taken(sv) = true;
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
    VarVec * varvec = m_var_mgr->get_var_vec();
    for (INT i = 0; i <= varvec->get_last_idx(); i++) {
        VAR * v = varvec->get(i);
        if (v == NULL || VAR_is_local(v)) { continue; }

        //User sometime intentionally declare non-allocable
        //global variable to custmized usage.
        //ASSERT0(VAR_allocable(v));

        if (v->is_string() && genDedicateStrMD() != NULL) {
            continue;
        }

        //We allocate MDTab for VAR which is func-decl or fake as well.
        //Since some Passes such as AA may need fake VAR to do analysis.
        MD md;
        MD_base(&md) = v;
        MD_ofst(&md) = 0;
        MD_size(&md) = v->getByteSize(getTypeMgr());
        if (VAR_is_fake(v) || VAR_is_func_decl(v)) {
            MD_ty(&md) = MD_UNBOUND;
        } else {
            MD_ty(&md) = MD_EXACT;
        }
        m_md_sys->registerMD(md);
    }
}

CallGraph * RegionMgr::allocCallGraph(UINT edgenum, UINT vexnum)
{
    return new CallGraph(edgenum, vexnum, this);
}


VarMgr * RegionMgr::allocVarMgr()
{
    return new VarMgr(this);
}


Region * RegionMgr::allocRegion(REGION_TYPE rt)
{
    return new Region(rt, this);
}


Region * RegionMgr::newRegion(REGION_TYPE rt)
{
    #ifdef _DEBUG_
    m_num_allocated++;
    #endif

    Region * ru = allocRegion(rt);
    UINT free_id = m_free_ru_id.remove_head();
    if (free_id == 0) {
        REGION_id(ru) = m_ru_count++;
    } else {
        REGION_id(ru) = free_id;
    }

    return ru;
}


//Record new region and delete it when RegionMgr destroy.
void RegionMgr::addToRegionTab(Region * ru)
{
    ASSERT(REGION_id(ru) > 0, ("should generate new region via newRegion()"));
    ASSERT0(get_region(REGION_id(ru)) == NULL);
    ASSERT0(REGION_id(ru) < m_ru_count);
    m_id2ru.set(REGION_id(ru), ru);
}


bool RegionMgr::verifyPreDefinedInfo()
{
    ASSERT0(WORD_LENGTH_OF_TARGET_MACHINE ==
            sizeof(TMWORD) * HOST_BIT_PER_BYTE);
    ASSERT0(BIT_PER_BYTE == HOST_BIT_PER_BYTE);

    ASSERT0(sizeof(INT8) * HOST_BIT_PER_BYTE == 8);
    ASSERT0(sizeof(UINT8) * HOST_BIT_PER_BYTE == 8);
    ASSERT0(sizeof(INT16) * HOST_BIT_PER_BYTE == 16);
    ASSERT0(sizeof(UINT16) * HOST_BIT_PER_BYTE == 16);
    ASSERT0(sizeof(INT32) * HOST_BIT_PER_BYTE == 32);
    ASSERT0(sizeof(UINT32) * HOST_BIT_PER_BYTE == 32);
    ASSERT0(sizeof(INT64) * HOST_BIT_PER_BYTE == 64);
    ASSERT0(sizeof(UINT64) * HOST_BIT_PER_BYTE == 64);
    #ifdef INT128
    ASSERT0(sizeof(INT128) * HOST_BIT_PER_BYTE == 128);
    #endif
    #ifdef UINT128
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
    #ifdef INT128
    ASSERT0(!IS_UNSIGN_TY(INT128));
    #endif
    #ifdef UINT128
    ASSERT0(IS_UNSIGN_TY(UINT128));
    #endif

    //Host LONGLONG should not less than HOST_INT,
    //otherwise the integer might be truncated wrongfully.
    ASSERT0(sizeof(HOST_INT) <= sizeof(LONGLONG));
    ASSERT0(sizeof(HOST_INT) == sizeof(HOST_UINT));

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

    return true;
}


void RegionMgr::dumpRelationGraph(CHAR const* name)
{
    if (getNumOfRegion() == 0) { return; }
    if (name == NULL) {
        name = "graph_region_relation_graph.vcg";
    }    
    UNLINK(name);
    xcom::Graph g;
    for (UINT id = 0; id < getNumOfRegion(); id++) {
        Region * ru = get_region(id);
        if (ru == NULL || ru->getParent() == NULL) { continue; }
        g.addEdge(ru->getParent()->id(), ru->id());
    }
    g.dump_vcg(name);
}


//Dump regions recorded via addToRegionTab().
void RegionMgr::dump(bool dump_inner_region)
{
    if (g_tfile == NULL || getNumOfRegion() == 0) { return; }

    g_indent = 0;
    note("\n==---- DUMP ALL Registered Region ----==");
    for (UINT id = 0; id < getNumOfRegion(); id++) {
        Region * ru = get_region(id);
        if (ru == NULL) { continue; }
        ru->dump(dump_inner_region);
    }
    fflush(g_tfile);
}


//This function destroy region, and free the region id
//to next region alloction.
void RegionMgr::deleteRegion(Region * ru, bool collect_id)
{
    ASSERT0(ru);
    UINT id = REGION_id(ru);
    ASSERT(get_region(id), ("not registered region"));
    delete ru;

    if (collect_id && id != 0) {
        m_id2ru.set(id, NULL);
        m_free_ru_id.append_head(id);
    }

    #ifdef _DEBUG_
    ASSERT0(m_num_allocated != 0);
    m_num_allocated--;
    #endif
}


void RegionMgr::estimateEV(
        OUT UINT & num_call,
        OUT UINT & num_ru,
        bool scan_call,
        bool scan_inner_region)
{
    for (UINT i = 0; i < getNumOfRegion(); i++) {
        Region * ru = get_region(i);
        if (ru == NULL) { continue; }

        num_ru++;

        ASSERT0(ru->is_function() || ru->is_program());
        if (scan_call) {
            ru->scanCallAndReturnList(num_ru, scan_inner_region);
        }

        List<IR const*> * call_list = ru->getCallList();
        if (call_list != NULL) {
            num_call += call_list->get_elem_count();
        }
    }
    num_ru = MAX(4, xcom::getNearestPowerOf2(num_ru));
    num_call = MAX(4, xcom::getNearestPowerOf2(num_call));
}


//Scan call site and build call graph.
//Return true if building graph successfully, otherwise return false.
void RegionMgr::buildCallGraph(
        OptCtx & oc,
        bool scan_call,
        bool scan_inner_region)
{
    //Generate call-list and return-list.
    UINT vn = 0, en = 0;
    estimateEV(en, vn, scan_call, scan_inner_region);

    if (m_call_graph == NULL) {
        //Construct call graph.
        m_call_graph = allocCallGraph(vn, en);
    }
    ASSERT0(m_call_graph);

    OC_is_callg_valid(oc) = m_call_graph->build(this);
}


//Process function level region.
bool RegionMgr::processFuncRegion(Region * func, OptCtx * oc)
{
    ASSERT0(func->is_function());
    g_indent = 0;
    bool s = func->process(oc);
    tfree();
    return s;
}


//Process top-level region.
//Top level region should be program.
bool RegionMgr::processProgramRegion(Region * program, OptCtx * oc)
{
    ASSERT0(program && program->is_program());
    g_indent = 0;
    bool s = program->process(oc);
    tfree();
    return s;
}
//END RegionMgr

} //namespace xoc
