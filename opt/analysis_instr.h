/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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

//Make sure IR_ICALL is the largest ir.
#define MAX_OFFSET_AT_FREE_TABLE (sizeof(CICall) - sizeof(IR))

//Analysis Instrument.
//Record Data structure for IR analysis and transformation.
#define ANA_INS_pass_mgr(a) ((a)->m_pass_mgr)
class AnalysisInstrument {
    COPY_CONSTRUCTOR(AnalysisInstrument);
public:
    UINT m_pr_count; //counter of IR_PR.
    Region * m_rg;
    SMemPool * m_du_pool;
    SMemPool * m_sc_labelinfo_pool;
    //Indicate a list of IR.
    IR * m_ir_list;
    List<IR const*> * m_call_list; //record CALL/ICALL in region.
    List<IR const*> * m_return_list; //record RETURN in region.
    PassMgr * m_pass_mgr; //PASS manager.
    IR * m_free_tab[MAX_OFFSET_AT_FREE_TABLE + 1];
    Vector<Var*> m_prno2var; //map prno to related Var. prno is dense integer.
    Vector<IR*> m_ir_vector; //record IR which have allocated. ir id is dense
    xcom::BitSetMgr m_bs_mgr;
    xcom::DefMiscBitSetMgr m_sbs_mgr;
    MDSetMgr m_mds_mgr;
    MDSetHashAllocator m_mds_hash_allocator;
    MDSetHash m_mds_hash;
    List<DU*> m_free_du_list;
    IRBBMgr m_ir_bb_mgr; //Allocate the basic block.
    BBList m_ir_bb_list; //record a list of basic blocks.

    #ifdef _DEBUG_
    xcom::BitSet m_has_been_freed_irs;
    #endif

public:
    explicit AnalysisInstrument(Region * rg);
    ~AnalysisInstrument();
    //Count memory usage for current object.
    size_t count_mem() const;
};

} //namespace xoc
#endif
