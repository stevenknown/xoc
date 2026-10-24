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
//START QWEN2D5LLMInfer
//
QWEN2D5LLMInfer::QWEN2D5LLMInfer(
    xoc::RegionMgr * rm, SafeTensorsMgr const& stmgr,
    Config const& config, JNodeMgr const& jm, Tokenizer & tokenizer,
    xoc::ESymTab & sym_tab)
    : LLMInfer(rm, stmgr, config, jm, tokenizer, sym_tab)
{
    m_max_seq_len = config.extractNum(
        addToSymbolTab("max_position_embeddings"));
}


QWEN2D5LLMInfer::~QWEN2D5LLMInfer()
{
}


XI_STATUS QWEN2D5LLMInfer::initRTCtxTensors(
    OUT RTCtx & ctx, RTMemMgr & mgr, MOD RTArg & arg)
{
    XI_STATUS status = LLMInfer::initRTCtxTensors(ctx, mgr, arg);
    if (status != XI_SUCC) { return status; }
    return XI_SUCC;
}


bool QWEN2D5LLMInfer::verify(RTCtx const& ctx, RTArg const& arg)
{
    return true;
}


XI_STATUS QWEN2D5LLMInfer::initRTCtxTensorsLayer(
    OUT RTCtx & ctx, MOD RTMemMgr & mgr)
{
    ctx.ln_1_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.ln_2_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.q_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.q_proj_bias = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.k_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.k_proj_bias = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.v_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.v_proj_bias = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.o_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.mlp_gate_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.mlp_up_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.mlp_down_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);

    //Iter each layer.
    xcom::StrBuf buf(32); //is used to form tensor's name.
    for (unsigned int layer = 0; layer < ctx.n_layer; layer++) {
        RTTensor * rtt;
        XI_STATUS status;

        //norm weight
        buf.sprint(get_h_ln_1_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.ln_1_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        //norm weight
        buf.sprint(get_h_ln_2_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.ln_2_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        //attention q|k|v proj weight
        buf.sprint(get_q_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.q_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_k_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.k_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_v_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.v_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        //attention q|k|v proj bias.
        buf.sprint(get_q_proj_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.q_proj_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_k_proj_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.k_proj_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_v_proj_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.v_proj_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        //attention output proj weight
        buf.sprint(get_o_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.o_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        //mlp fc weight
        buf.sprint(get_layers_mlp_gate_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.mlp_gate_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_layers_mlp_up_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.mlp_up_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        //mlp proj weight
        buf.sprint(get_h_mlp_down_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.mlp_down_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }
    }
    return XI_SUCC;
}


void QWEN2D5LLMInfer::initRTArg(RTArg & arg)
{
    arg.rmsnorm_epsilon = m_config.extractNum(
        addToSymbolTab("rms_norm_eps"));
    arg.n_embd = m_config.extractNum(addToSymbolTab("hidden_size"));
    arg.n_layer = m_config.extractNum(addToSymbolTab("num_hidden_layers"));
    arg.n_head = m_config.extractNum(addToSymbolTab("num_attention_heads"));
    arg.n_kvhead = m_config.extractNum(addToSymbolTab("num_key_value_heads"));
    arg.rope_theta = m_config.extractNum(addToSymbolTab("rope_theta"));
    arg.eos_token_id = m_config.extractNum(addToSymbolTab("eos_token_id"));
    arg.vocab_size = m_config.extractNum(addToSymbolTab("vocab_size"));
    arg.intermediate_size = m_config.extractNum(
        addToSymbolTab("intermediate_size"));
    arg.n_positions = m_config.extractNum(
        addToSymbolTab("max_position_embeddings"));
    xoc::Sym const* dtype_str = m_config.extractStr(
        addToSymbolTab("torch_dtype"));
    ASSERT0(dtype_str);
    if (::strcmp(dtype_str->getStr(), "bfloat16") == 0) {
        arg.tensor_dtype = RTTENSOR_DTYPE_BF16;
    } else { ASSERT0(0); } //TODO:
    UINT elemsz = getRTTensorDTypeByteSize(arg.tensor_dtype);
    ASSERT0(elemsz != 0);

    ASSERT0(arg.n_head != 0);
    UINT head_dim = arg.n_embd / arg.n_head;
    UINT max_ctx = arg.n_positions;
    UINT hiddensz = arg.n_embd * elemsz;
    arg.hidden.grow(hiddensz);

    //Used to store the output of W1, W2 of MLP.
    arg.tmp1.grow(arg.intermediate_size * elemsz);
    arg.tmp2.grow(arg.intermediate_size * elemsz);
    arg.tmp3.grow(arg.intermediate_size * elemsz);

    //mlp_swigu needs two tmp buffers(gate,up) whose bytesize has to be the
    //twice value of arg.intermediate_size*elemsz.
    UINT qkvfcsz = MAX(4 * hiddensz, 2 * arg.intermediate_size * elemsz);
    arg.tmp_qkv_and_fc.grow(qkvfcsz);
    ASSERT0(arg.vocab_size > 0 && arg.vocab_size < MAX_VOCAB_SIZE);
    arg.logits.grow(arg.vocab_size * elemsz);

    //KVcache:each layer:[max_ctx, head_dim].
    //In MHA, each head own its dedicated kcache and vcache.
    UINT64 kv_one_layer = max_ctx * arg.n_head * head_dim * elemsz;
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
//END QWEN2D5LLMInfer

} //namespace xinfer
