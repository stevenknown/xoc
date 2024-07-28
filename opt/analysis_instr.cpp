/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

#ifdef _DEBUG_
static bool verifyVar(Region * rg, VarMgr * vm, Var * v)
{
    ASSERT0_DUMMYUSE(v);
    ASSERT0_DUMMYUSE(vm);
    if (rg->is_function() || rg->is_eh() ||
        rg->getRegionType() == REGION_INNER) {
        //If var is global but unallocable, it often be
        //used as placeholder or auxilary var.

        //For these kind of regions, there are only local variable or
        //unablable global variable is legal.
        ASSERT0(v->is_local() || v->is_unallocable() || v->is_func() ||
                v->is_decl() || v->is_region());
    } else if (rg->is_program()) {
        //Theoretically, only global variable is legal in program region.
        //However even if the program region there may be local
        //variables, e.g: PR, a kind of local variable.
        //ASSERT0(v->is_global());
    } else {
        ASSERTN(0, ("unsupport variable type."));
    }
    return true;
}
#endif


//Free md's id and local-var's id back to MDSystem and VarMgr.
//The index of MD and Var is important resource if there
//are a lot of REGIONs in RegionMgr.
//Note this function does NOT process GLOBAL variable.
static void destroyVARandMD(Region * rg)
{
    VarMgr * varmgr = rg->getVarMgr();
    MDSystem * mdsys = rg->getMDSystem();
    VarTabIter c;
    ConstMDIter iter;
    VarTab * vartab = rg->getVarTab();
    ASSERT0(vartab);
    for (Var * v = vartab->get_first(c); v != nullptr;
         v = vartab->get_next(c)) {
        ASSERT0(verifyVar(rg, varmgr, v));
        mdsys->removeMDforVAR(v, iter);
        varmgr->destroyVar(v);
    }
}


//
//START AnalysisInstrument
//
AnalysisInstrument::AnalysisInstrument(Region * rg) :
    m_rg(rg),
    m_md_mgr(rg),
    m_mds_mgr(rg, &m_sbs_mgr),
    m_mds_hash_allocator(&m_sbs_mgr),
    m_mds_hash(&m_mds_hash_allocator)
{
    m_rg = rg;
    m_call_list = nullptr;
    m_return_list = nullptr;
    m_ir_list = nullptr;
    m_pass_mgr = nullptr;
    m_attachinfo_mgr = nullptr;
    m_ir_mgr = nullptr;
    m_ir_bb_mgr = nullptr;
    m_dbx_mgr = nullptr;
    //Counter of IR_PR, and do not use '0' as prno.
    m_pr_count = PRNO_UNDEF + 1;
    m_du_pool = smpoolCreate(sizeof(DU) * 4, MEM_CONST_SIZE);
    m_sc_labelinfo_pool = smpoolCreate(sizeof(xcom::SC<LabelInfo*>) * 4,
                                       MEM_CONST_SIZE);
    m_ir_bb_list = new BBList();
}


AnalysisInstrument::~AnalysisInstrument()
{
    #ifdef DEBUG_SEG
    //Just dump the seg info if you really need to see.
    //DefSegMgr * segmgr = m_sbs_mgr.getSegMgr();
    //dumpSegMgr(segmgr, getRegion()->getLogMgr()->getFileHandler());
    #endif

    //Destroy pass manager.
    if (m_pass_mgr != nullptr) {
        delete m_pass_mgr;
        m_pass_mgr = nullptr;
    }

    //Destroy attachinfo manager.
    if (m_attachinfo_mgr != nullptr) {
        delete m_attachinfo_mgr;
        m_attachinfo_mgr = nullptr;
    }

    if (m_dbx_mgr != nullptr) {
        delete m_dbx_mgr;
        m_dbx_mgr = nullptr;
    }

    //Free local Var id and related MD id back to VarMgr and MDSystem if the
    //option is set. This is quite time-consuming if the numbers of Regions
    //and Vars are large.
    if (g_recycle_local_id) {
        destroyVARandMD(m_rg);
    } else {
        m_rg->getVarTab()->clean();
    }

    //Destroy reference info.
    RefInfo * refinfo = REGION_refinfo(m_rg);
    if (refinfo != nullptr) {
        if (REF_INFO_mayuse(refinfo) != nullptr) {
            REF_INFO_mayuse(refinfo)->clean(m_sbs_mgr);
            m_mds_mgr.free(REF_INFO_mayuse(refinfo));
        }
        if (REF_INFO_maydef(refinfo) != nullptr) {
            REF_INFO_maydef(refinfo)->clean(m_sbs_mgr);
            m_mds_mgr.free(REF_INFO_maydef(refinfo));
        }

        //REGION_refinfo allocated in pool.
        REGION_refinfo(m_rg) = nullptr;
    }

    //Destory CALL list.
    if (m_call_list != nullptr) {
        delete m_call_list;
        m_call_list = nullptr;
    }

    //Destory RETURN list.
    if (m_return_list != nullptr) {
        delete m_return_list;
        m_return_list = nullptr;
    }

    ////////////////////////////////////////////////////////////////////////////
    //DO NOT DESTROY MEMBER WHICH ALLOCATED IN POOL AFTER THIS LINE           //
    ////////////////////////////////////////////////////////////////////////////

    //Destroy all DUSet which allocated in the du_pool.
    smpoolDelete(m_du_pool);
    m_du_pool = nullptr;

    //Destroy the pool of container of SC of LabelInfo.
    smpoolDelete(m_sc_labelinfo_pool);
    m_sc_labelinfo_pool = nullptr;

    //Destroy the BBList that allocated by 'this'.
    delete m_ir_bb_list;

    //Just set to NULL because these members are not managed by 'this'.
    m_ir_list = nullptr;
    m_ir_mgr = nullptr;
    m_ir_bb_mgr = nullptr;
}


size_t AnalysisInstrument::count_mem() const
{
    size_t count = sizeof(*this);
    if (m_call_list != nullptr) {
        count += m_call_list->count_mem();
    }
    count += smpoolGetPoolSize(m_du_pool);
    count += smpoolGetPoolSize(m_sc_labelinfo_pool);
    count += m_prno2var.count_mem();
    count += m_bs_mgr.count_mem();
    count += m_sbs_mgr.count_mem();
    count += m_mds_mgr.count_mem();
    count += m_mds_hash.count_mem();
    count += m_ir_bb_list != nullptr ? m_ir_bb_list->count_mem(): 0;
    count += m_ir_mgr != nullptr ? m_ir_mgr->count_mem() : 0;
    count += m_ir_bb_mgr != nullptr ? m_ir_bb_mgr->count_mem() : 0;
    //m_free_du_list has been counted in m_du_pool.
    return count;
}
//END AnalysisInstrument

} //namespace xoc
