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
#ifndef _DISTILGPT2_LLM_INFER_H_
#define _DISTILGPT2_LLM_INFER_H_

namespace xinfer {

class DistilGPT2LLMInfer : public GPT2LLMInfer {
    COPY_CONSTRUCTOR(DistilGPT2LLMInfer);
public:
    DistilGPT2LLMInfer(
        xoc::RegionMgr * rm, SafeTensorsMgr const& stmgr,
        Config const& config, JNodeMgr const& jm, Tokenizer & tokenizer,
        xoc::ESymTab & sym_tab);
    virtual ~DistilGPT2LLMInfer();

    //word-token-embedding
    virtual CHAR const* get_wte_weight_name() const override
    { return "transformer.wte.weight"; }

    //word-position-embedding
    virtual CHAR const* get_wpe_weight_name() const override
    { return "transformer.wpe.weight"; }

    //finial layernorm.
    virtual CHAR const* get_ln_f_weight_name() const override
    { return "transformer.ln_f.weight"; }

    virtual CHAR const* get_ln_f_bias_name() const override
    { return "transformer.ln_f.bias"; }

    //block layernorm1.
    virtual CHAR const* get_h_ln_1_weight_format() const override
    { return "transformer.h.%d.ln_1.weight"; }

    virtual CHAR const* get_h_ln_1_bias_format() const override
    { return "transformer.h.%d.ln_1.bias"; }

    //block layernorm2.
    virtual CHAR const* get_h_ln_2_weight_format() const override
    { return "transformer.h.%d.ln_2.weight"; }

    virtual CHAR const* get_h_ln_2_bias_format() const override
    { return "transformer.h.%d.ln_2.bias"; }

    //combined qkv attention projection.
    virtual CHAR const* get_h_c_attn_weight_format() const override
    { return "transformer.h.%d.attn.c_attn.weight"; }

    virtual CHAR const* get_h_c_attn_bias_format() const override
    { return "transformer.h.%d.attn.c_attn.bias"; }

    //combined fully-connected.
    virtual CHAR const* get_h_mlp_c_fc_weight_format() const override
    { return "transformer.h.%d.mlp.c_fc.weight"; }

    virtual CHAR const* get_h_mlp_c_fc_bias_format() const override
    { return "transformer.h.%d.mlp.c_fc.bias"; }

    //combined output projection.
    virtual CHAR const* get_o_proj_weight_format() const override
    { return "transformer.h.%d.attn.c_proj.weight"; }

    virtual CHAR const* get_o_proj_bias_format() const override
    { return "transformer.h.%d.attn.c_proj.bias"; }

    //combined output fully-connected.
    virtual CHAR const* get_h_mlp_down_proj_weight_format() const override
    { return "transformer.h.%d.mlp.c_proj.weight"; }

    virtual CHAR const* get_h_mlp_down_proj_bias_format() const override
    { return "transformer.h.%d.mlp.c_proj.bias"; }
};

} //namespace xinfer

#endif
