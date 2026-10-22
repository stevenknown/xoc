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

//
//START GPT2LLMInfer
//
GPT2LLMInfer::GPT2LLMInfer(
    xoc::RegionMgr * rm, SafeTensorsMgr const& stmgr,
    Config const& config, JNodeMgr const& jm, Tokenizer & tokenizer,
    xoc::ESymTab & sym_tab)
    : LLMInfer(rm, stmgr, config, jm, tokenizer, sym_tab)
{
}


GPT2LLMInfer::~GPT2LLMInfer()
{
}


//Init RTTensor.
XI_STATUS GPT2LLMInfer::initRTCtxTensors(
    OUT RTCtx & ctx, RTMemMgr & mgr, MOD RTArg & arg)
{
    XI_STATUS status = LLMInfer::initRTCtxTensors(ctx, mgr, arg);
    if (status != XI_SUCC) { return status; }

    SafeTensor const* st;
    st = getSafeTensor(get_wpe_weight_name());
    if (st == nullptr) {
        dumpAct(getLogMgr(), "\nerror:miss word-position-embedding");
        return XI_ERR;
    }
    ctx.wpe = mgr.allocRTTensor();
    LLMInfer::convertSafeTensorToRTTensor(*ctx.wpe, *st);

    st = getSafeTensor(get_ln_f_bias_name());
    if (st == nullptr) {
        dumpAct(getLogMgr(), "\nerror:miss ln_f.bias");
        return XI_ERR;
    }
    ctx.ln_f_b = mgr.allocRTTensor();
    LLMInfer::convertSafeTensorToRTTensor(*ctx.ln_f_b, *st);

    return XI_SUCC;
}


XI_STATUS GPT2LLMInfer::initRTCtxTensorsLayer(
    OUT RTCtx & ctx, MOD RTMemMgr & mgr)
{
    XI_STATUS status = LLMInfer::initRTCtxTensorsLayer(ctx, mgr);
    return status;
}


void GPT2LLMInfer::initRTArg(RTArg & arg)
{
    arg.n_embd = m_config.extractNum(addToSymbolTab("n_embd"));
    arg.n_layer = m_config.extractNum(addToSymbolTab("n_layer"));
    arg.n_head = m_config.extractNum(addToSymbolTab("n_head"));
    arg.n_positions = m_config.extractNum(addToSymbolTab("n_positions"));
    arg.layernorm_epsilon = m_config.extractNum(
        addToSymbolTab("layer_norm_epsilon"));
    arg.intermediate_size = 4 * arg.n_embd;
    arg.eos_token_id = m_config.extractNum(addToSymbolTab("eos_token_id"));
    arg.vocab_size = m_config.extractNum(addToSymbolTab("vocab_size"));

    ASSERT0(arg.n_head != 0);
    UINT head_dim = arg.n_embd / arg.n_head;
    UINT max_ctx = arg.n_positions;

    arg.tensor_dtype = RTTENSOR_DTYPE_F32;
    UINT elemsz = getRTTensorDTypeByteSize(arg.tensor_dtype);
    ASSERT0(elemsz != 0);
    UINT64 sz = arg.n_embd * elemsz;

    arg.hidden.grow(sz);
    arg.tmp1.grow(sz);
    arg.tmp2.grow(sz);
    arg.tmp_qkv_and_fc.grow(4 * sz);
    ASSERT0(arg.vocab_size > 0 && arg.vocab_size < MAX_VOCAB_SIZE);
    arg.logits.grow(arg.vocab_size * elemsz);

    //KVcache:each layer:[max_ctx, head_dim].
    //In MHA, each head own its dedicated kcache and vcache.
    UINT64 kv_one_layer = elemsz * max_ctx * arg.n_head * head_dim;
    UINT64 kvsize = arg.n_layer * kv_one_layer;

    arg.kcache.grow(kvsize);
    arg.vcache.grow(kvsize);
    arg.current_pos = 0;
    arg.newline_token_id = getTokenizer().encode('\n');
    if (arg.newline_token_id == (TOKID)INVALID_TOKEN) {
        dumpAct(getLogMgr(), "\nerror:encode prompt failed");
        return;
    }
}
//END GPT2LLMInfer

} //namespace xinfer
