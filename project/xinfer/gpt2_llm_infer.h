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
#ifndef _GPT2_LLM_INFER_H_
#define _GPT2_LLM_INFER_H_

namespace xinfer {

class GPT2LLMInfer : public LLMInfer {
    COPY_CONSTRUCTOR(GPT2LLMInfer);
protected:
    XI_STATUS initRTCtxTensorsLayer(OUT RTCtx & ctx, MOD RTMemMgr & mgr);
    virtual XI_STATUS initRTCtxTensors(
        OUT RTCtx & ctx, RTMemMgr & mgr, MOD RTArg & arg) override;
    virtual void initRTArg(RTArg & arg) override;
public:
    GPT2LLMInfer(
        xoc::RegionMgr * rm, SafeTensorsMgr const& stmgr,
        Config const& config, JNodeMgr const& jm, Tokenizer & tokenizer,
        xoc::ESymTab & sym_tab);
    virtual ~GPT2LLMInfer();

    virtual CHAR const* get_wte_weight_name() const override
    { return "wte.weight"; }

    virtual CHAR const* get_wpe_weight_name() const override
    { return "wpe.weight"; }

    virtual CHAR const* get_ln_f_weight_name() const override
    { return "ln_f.weight"; }

    virtual CHAR const* get_ln_f_bias_name() const override
    { return "ln_f.bias"; }

    virtual CHAR const* get_h_ln_1_weight_format() const override
    { return "h.%d.ln_1.weight"; }

    virtual CHAR const* get_h_ln_1_bias_format() const override
    { return "h.%d.ln_1.bias"; }

    virtual CHAR const* get_h_ln_2_weight_format() const override
    { return "h.%d.ln_2.weight"; }

    virtual CHAR const* get_h_ln_2_bias_format() const override
    { return "h.%d.ln_2.bias"; }

    virtual CHAR const* get_h_c_attn_weight_format() const override
    { return "h.%d.attn.c_attn.weight"; }

    virtual CHAR const* get_h_c_attn_bias_format() const override
    { return "h.%d.attn.c_attn.bias"; }

    virtual CHAR const* get_h_mlp_c_fc_weight_format() const override
    { return "h.%d.mlp.c_fc.weight"; }

    virtual CHAR const* get_h_mlp_c_fc_bias_format() const override
    { return "h.%d.mlp.c_fc.bias"; }

    virtual CHAR const* get_o_proj_weight_format() const override
    { return "h.%d.attn.c_proj.weight"; }

    virtual CHAR const* get_o_proj_bias_format() const override
    { return "h.%d.attn.c_proj.bias"; }

    virtual CHAR const* get_h_mlp_down_proj_weight_format() const override
    { return "h.%d.mlp.c_proj.weight"; }

    virtual CHAR const* get_h_mlp_down_proj_bias_format() const override
    { return "h.%d.mlp.c_proj.bias"; }
};

} //namespace xinfer

#endif
