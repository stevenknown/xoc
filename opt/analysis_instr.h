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
#ifndef __ANALYSIS_INSTR_H__
#define __ANALYSIS_INSTR_H__

namespace xoc {

//Analysis Instrument.
#define ANA_INS_pr_count(a) ((a)->m_pr_count)
#define ANA_INS_du_pool(a) ((a)->m_du_pool)
#define ANA_INS_sc_labelinfo_pool(a) ((a)->m_sc_labelinfo_pool)
#define ANA_INS_ir_list(a) ((a)->m_ir_list)
#define ANA_INS_ir_mgr(a) ((a)->m_ir_mgr)
#define ANA_INS_call_list(a) ((a)->m_call_list)
#define ANA_INS_return_list(a) ((a)->m_return_list)
#define ANA_INS_ir_free_tab(a) ((a)->m_free_tab)
#define ANA_INS_prno2var(a) ((a)->m_prno2var)
#define ANA_INS_ir_vec(a) ((a)->m_ir_mgr->getIRVec())
#define ANA_INS_bs_mgr(a) ((a)->m_bs_mgr)
#define ANA_INS_sbs_mgr(a) ((a)->m_sbs_mgr)
#define ANA_INS_mds_mgr(a) ((a)->m_mds_mgr)
#define ANA_INS_md_mgr(a) ((a)->m_md_mgr)
#define ANA_INS_mds_hash_allocator(a) ((a)->m_mds_hash_allocator)
#define ANA_INS_mds_hash(a) ((a)->m_mds_hash)
#define ANA_INS_free_du_list(a) ((a)->m_free_du_list)
#define ANA_INS_ir_bb_mgr(a) ((a)->m_ir_bb_mgr)
#define ANA_INS_ir_bb_list(a) ((a)->m_ir_bb_list)

//Record Data structure for IR analysis and transformation.
#define ANA_INS_pass_mgr(a) ((a)->m_pass_mgr)
#define ANA_INS_ai_mgr(a) ((a)->m_attachinfo_mgr)
class AnalysisInstrument {
    friend class Region;
    friend class IRMgr;
    COPY_CONSTRUCTOR(AnalysisInstrument);
protected:
    UINT m_pr_count; //counter of IR_PR.
    Region * m_rg;
    SMemPool * m_du_pool;
    SMemPool * m_sc_labelinfo_pool;
    //Indicate a list of IR.
    IR * m_ir_list;
    List<IR const*> * m_call_list; //record CALL/ICALL in region.
    List<IR const*> * m_return_list; //record RETURN in region.
    PassMgr * m_pass_mgr; //PASS manager.
    AttachInfoMgr * m_attachinfo_mgr; //AttachInfo manager.
    IRMgr * m_ir_mgr;

    //Mapping prno to related Var. prno is dense integer.
    xcom::Vector<Var*> m_prno2var;
    xcom::BitSetMgr m_bs_mgr;
    xcom::DefMiscBitSetMgr m_sbs_mgr;
    MDMgr m_md_mgr;
    MDSetMgr m_mds_mgr;
    MDSetHashAllocator m_mds_hash_allocator;
    MDSetHash m_mds_hash;
    List<DU*> m_free_du_list;
    IRBBMgr m_ir_bb_mgr; //Allocate the basic block.
    BBList m_ir_bb_list; //record a list of basic blocks.
protected:
    //Count memory usage for current object.
    size_t count_mem() const;
    PassMgr * getPassMgr() const { return ANA_INS_pass_mgr(this); }
    IRMgr * getIRMgr() const { return ANA_INS_ir_mgr(this); }
    AttachInfoMgr * getAttachInfoMgr() const { return ANA_INS_ai_mgr(this); }
public:
    explicit AnalysisInstrument(Region * rg);
    ~AnalysisInstrument();
};

} //namespace xoc
#endif
