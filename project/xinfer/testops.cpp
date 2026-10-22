/*@
Copyright (c) 2026-present, Su Zhenyu steven.known@gmail.com

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
#include "ops/ops.h"
#include "xinfer_cominc.h"

namespace xinfer {

TestOps::TestOps(xoc::RegionMgr * rm, MOD SafeTensorsMgr & stmgr)
    : m_stmgr(stmgr)
{
    m_rm = rm;
}


TestOps::~TestOps()
{
}


void TestOps::dump() const
{
    if (!m_rm->isLogMgrInit()) { return; }
    note(m_rm, "\n==-- DUMP TestOps --==");
    xoc::LogMgr * lm = m_rm->getLogMgr();
    lm->incIndent(2);
    //START
    //END
    lm->decIndent(2);
}


static void test_embedding_lookup(TestOps & testops)
{
    SafeTensor const* st = testops.getSafeTensorsMgr().getSafeTensor(
        "transformer.wte.weight");
    if (st == nullptr) { return; }
    RTTensor wte;
    LLMInfer::convertSafeTensorToRTTensor(wte, *st);
    int64_t n_emb = RTTENSOR_nemb(&wte);
    float * emb_out = (float*)::malloc(n_emb*sizeof(float));
    embedding_lookup(emb_out, &wte, 0);
    if (xoc::g_dump_opt.isDumpAll()) {
        for(UINT i = 0; i < n_emb; i++) {
            note(testops.getRegionMgr(), "\nemb[%d] = %f", i, emb_out[i]);
        }
    }
    ::free(emb_out);
}


bool TestOps::perform()
{
    test_embedding_lookup(*this);
    return true;
}

} //namespace xinfer
