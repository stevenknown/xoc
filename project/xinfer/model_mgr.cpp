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
#include "xinfer_cominc.h"

namespace xinfer {

ModelAttr g_model_attr[] = {
    { MODEL_UNDEF, "", },
    { MODEL_DISTILGPT2, "distilgpt2", },
    { MODEL_GPT2, "gpt2", },
    { MODEL_QWEN2D5_1D5B, "qwen2.5_1.5B", },
};
UINT g_model_attr_num = sizeof(g_model_attr) / sizeof(g_model_attr[0]);

//
//START ModelMgr
//
ModelMgr::ModelMgr()
{
    m_infer = nullptr;
}


ModelMgr::~ModelMgr()
{
    destroy();
}


void ModelMgr::destroy()
{
    if (m_infer != nullptr) {
        delete m_infer;
        m_infer = nullptr;
    }
}


void ModelMgr::dump() const
{
}


LLMInfer * ModelMgr::allocModel(
    ModelType mt, xoc::RegionMgr * rm, SafeTensorsMgr const& stmgr,
    Config const& config, JNodeMgr const& jm,
    MOD Tokenizer & tokenizer, MOD xoc::ESymTab & sym_tab)
{
    ASSERT0(mt != MODEL_UNDEF);
    switch (mt) {
    case MODEL_DISTILGPT2:
        ASSERT0(m_infer == nullptr);
        m_infer = new DistilGPT2LLMInfer(
            rm, stmgr, config, jm, tokenizer, sym_tab);
        return m_infer;
    case MODEL_GPT2:
        m_infer = new GPT2LLMInfer(rm, stmgr, config, jm, tokenizer, sym_tab);
        return m_infer;
    case MODEL_QWEN2D5_1D5B:
        m_infer = new QWEN2D5LLMInfer(
            rm, stmgr, config, jm, tokenizer, sym_tab);
        return m_infer;
    default: ASSERT0(0); //TODO:
    }
    return nullptr;
}
//END Driver

} //namespace xinfer
