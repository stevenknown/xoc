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
#ifndef _QWEN2_5_LLM_INFER_H_
#define _QWEN2_5_LLM_INFER_H_

namespace xinfer {

class QWEN2D5LLMInfer : public LLMInfer {
    COPY_CONSTRUCTOR(QWEN2D5LLMInfer);
protected:
    UINT m_max_seq_len;
protected:
    virtual XI_STATUS initRTCtxTensors(
        OUT RTCtx & ctx, RTMemMgr & mgr, MOD RTArg & arg) override;
    virtual void initRTArg(RTArg & arg) override;
    virtual XI_STATUS initRTCtxTensorsLayer(
        OUT RTCtx & ctx, MOD RTMemMgr & mgr) override;
public:
    QWEN2D5LLMInfer(
        xoc::RegionMgr * rm, SafeTensorsMgr const& stmgr,
        Config const& config, JNodeMgr const& jm, Tokenizer & tokenizer,
        xoc::ESymTab & sym_tab);
    virtual ~QWEN2D5LLMInfer();

    UINT getMaxSeqLen() const { return m_max_seq_len; }

    //Attention comon.
    virtual CHAR const* get_wte_weight_name() const override
    { return "model.embed_tokens.weight"; }

    virtual CHAR const* get_ln_f_weight_name() const override
    { return "model.norm.weight"; }

    virtual CHAR const* get_h_ln_1_weight_format() const override
    { return "model.layers.%d.input_layernorm.weight"; }

    virtual CHAR const* get_h_ln_2_weight_format() const override
    { return "model.layers.%d.post_attention_layernorm.weight"; }

    virtual CHAR const* get_o_proj_weight_format() const override
    { return "model.layers.%d.self_attn.o_proj.weight"; }

    virtual CHAR const* get_o_proj_bias_format() const override
    { return "model.layers.%d.self_attn.o_proj.bias"; }

    virtual CHAR const* get_h_mlp_down_proj_weight_format() const override
    { return "model.layers.%d.mlp.down_proj.weight"; }

    virtual CHAR const* get_h_mlp_down_proj_bias_format() const override
    { return "model.layers.%d.mlp.down_proj.bias"; }

    //QWEN specific ---------------------------------------------
    virtual CHAR const* get_q_proj_weight_format() const
    { return "model.layers.%d.self_attn.q_proj.weight"; }

    virtual CHAR const* get_q_proj_bias_format() const
    { return "model.layers.%d.self_attn.q_proj.bias"; }

    virtual CHAR const* get_k_proj_weight_format() const
    { return "model.layers.%d.self_attn.k_proj.weight"; }

    virtual CHAR const* get_k_proj_bias_format() const
    { return "model.layers.%d.self_attn.k_proj.bias"; }

    virtual CHAR const* get_v_proj_weight_format() const
    { return "model.layers.%d.self_attn.v_proj.weight"; }

    virtual CHAR const* get_v_proj_bias_format() const
    { return "model.layers.%d.self_attn.v_proj.bias"; }

    virtual CHAR const* get_layers_mlp_gate_proj_weight_format() const
    { return "model.layers.%d.mlp.gate_proj.weight"; }

    virtual CHAR const* get_layers_mlp_up_proj_weight_format() const
    { return "model.layers.%d.mlp.up_proj.weight"; }

    virtual bool verify(RTCtx const& ctx, RTArg const& arg);
};

} //namespace xinfer

#endif
