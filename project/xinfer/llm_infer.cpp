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
#include <cstdlib> // srand(), rand()
#include <ctime> // time()

namespace xinfer {

//
//START LLMInfer
//
LLMInfer::LLMInfer(
    xoc::RegionMgr * rm, SafeTensorsMgr const& stmgr, Config const& config,
    JNodeMgr const& jm, Tokenizer & tokenizer, xoc::ESymTab & sym_tab)
    : m_stmgr(stmgr), m_config(config), m_jm(jm), m_tokenizer(tokenizer),
      m_sym_tab(sym_tab)
{
    m_rm = rm;
}


LLMInfer::~LLMInfer()
{
}


xoc::Sym const* LLMInfer::addToSymbolTab(CHAR const* s)
{
    return m_sym_tab.add(s);
}


SafeTensor const* LLMInfer::getSafeTensor(CHAR const* name)
{
    xoc::Sym const* namesym = addToSymbolTab(name);
    return m_stmgr.getSafeTensor(namesym);
}


SafeTensor const* LLMInfer::getSafeTensor(xoc::Sym const* namesym) const
{
    return m_stmgr.getSafeTensor(namesym);
}


void LLMInfer::initRTArg(RTArg & arg)
{
    ASSERT0(0);//Target Dependent Code
    arg.current_pos = 0;
    arg.newline_token_id = getTokenizer().encode('\n');
    if (arg.newline_token_id == (TOKID)INVALID_TOKEN) {
        dumpAct(getLogMgr(), "\nerror:encode prompt failed");
        return;
    }
}


void LLMInfer::dump() const
{
    if (!m_rm->isLogMgrInit()) { return; }
    if (!g_dump_llminfer) { return; }
    note(m_rm, "\n==-- DUMP LLMInfer --==");
    xoc::LogMgr * lm = m_rm->getLogMgr();
    lm->incIndent(2);
    //START
    //END
    lm->decIndent(2);
}


static RTTENSOR_DTYPE convertDTypeToRTTENSORDt(xoc::DATA_TYPE dt)
{
    switch (dt) {
    case xoc::D_F32: return RTTENSOR_DTYPE_F32;
    case xoc::D_F16: return RTTENSOR_DTYPE_F16;
    case xoc::D_BF16: return RTTENSOR_DTYPE_BF16;
    case xoc::D_I8: return RTTENSOR_DTYPE_I8;
    case xoc::D_U8: return RTTENSOR_DTYPE_U8;
    case xoc::D_I16: return RTTENSOR_DTYPE_I16;
    case xoc::D_I32: return RTTENSOR_DTYPE_I32;
    case xoc::D_I64: return RTTENSOR_DTYPE_I64;
    case xoc::D_B: return RTTENSOR_DTYPE_BOOL;
    case xoc::D_F8E4M3: return RTTENSOR_DTYPE_F8_E4M3;
    case xoc::D_F8E5M2: return RTTENSOR_DTYPE_F8_E5M2;
    default: ASSERT0(0);
    }
    return RTTENSOR_DTYPE_UNDEF;
}


void LLMInfer::convertSafeTensorToRTTensor(
    OUT RTTensor & rt, SafeTensor const& st)
{
    rt.data = (void*)st.getDataBuf();

    //Convert dtype.
    rt.dtype = convertDTypeToRTTENSORDt(st.getDType());

    //Convert shape.
    rt.ndim = st.getShapeDimNum();
    rt.elem_num = st.getTotalElemNum();
    for (UINT i = 0; i < (UINT)rt.ndim; i++) {
        rt.shape[i] = st.getShapeDim(i);
    }
}


XI_STATUS LLMInfer::initRTTensor(RTTensor * rtt, CHAR const* name)
{
    SafeTensor const* st = getSafeTensor(name);
    if (st == nullptr) {
        dumpAct(getLogMgr(), "\nerror:cannot find %s in LLMInfer", name);
        return XI_ERR;
    }
    LLMInfer::convertSafeTensorToRTTensor(*rtt, *st);
    return XI_SUCC;
}


XI_STATUS LLMInfer::initRTCtxTensorsLayer(
    OUT RTCtx & ctx, MOD RTMemMgr & mgr)
{
    ctx.ln_1_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.ln_1_bias = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.ln_2_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.ln_2_bias = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.c_attn_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.c_attn_bias = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.o_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.o_proj_bias = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.mlp_c_fc_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.mlp_c_fc_bias = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.mlp_down_proj_weight = mgr.allocRTTensorArray(ctx.n_layer);
    ctx.mlp_down_proj_bias = mgr.allocRTTensorArray(ctx.n_layer);

    //Iter each layer.
    xcom::StrBuf buf(32); //is used to form tensor's name.
    for (unsigned int layer = 0; layer < ctx.n_layer; layer++) {
        RTTensor * rtt;
        XI_STATUS status;

        buf.sprint(get_h_ln_1_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.ln_1_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_ln_1_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.ln_1_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_ln_2_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.ln_2_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_ln_2_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.ln_2_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_c_attn_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.c_attn_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_c_attn_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.c_attn_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_mlp_c_fc_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.mlp_c_fc_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_mlp_c_fc_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.mlp_c_fc_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_o_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.o_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_o_proj_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.o_proj_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_mlp_down_proj_weight_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.mlp_down_proj_weight[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }

        buf.sprint(get_h_mlp_down_proj_bias_format(), layer);
        rtt = mgr.allocRTTensor();
        ctx.mlp_down_proj_bias[layer] = rtt;
        status = initRTTensor(rtt, buf.getBuf());
        if (status != XI_SUCC) { return status; }
    }
    return XI_SUCC;
}


static void initRTDump(LLMInfer & infer, RTCtx & ctx)
{
    g_rtctx = nullptr;
    #ifdef _DEBUG_
    if (xinfer::g_dump_rt_norm) {
        g_rtctx = &ctx;
        g_rtctx->dump_l2norm = true;
    }
    if (xinfer::g_dump_rt) {
        g_rtctx = &ctx;
        g_rtctx->dump = true;
    }
    if (xinfer::g_dump_rt_vec) {
        g_rtctx = &ctx;
        g_rtctx->dump_vec = true;
    }
    #endif
    #ifdef _DUMP_BY_LOGMGR_
    if (g_rtctx != nullptr) {
        g_rtctx->lm = infer.getLogMgr();
    }
    #endif
}


static void initRTCtxBuffer(OUT RTCtx & ctx, RTArg & arg)
{
    //Init buffer pointer.
    ctx.hidden = arg.getHiddenBuf();
    ctx.hidden_len = arg.getHiddenBufLen();

    ctx.tmp1 = arg.getTmp1Buf();
    ctx.tmp1_len = arg.getTmp1BufLen();

    ctx.tmp2 = arg.getTmp2Buf();
    ctx.tmp2_len = arg.getTmp2BufLen();

    ctx.tmp3 = arg.getTmp3Buf();
    ctx.tmp3_len = arg.getTmp3BufLen();

    ctx.tmp_qkv = arg.getTmpQKVBuf();
    ctx.tmp_qkv_len = arg.getTmpQKVBufLen();

    ctx.tmp_fc = arg.getTmpFCBuf();
    ctx.tmp_fc_len = arg.getTmpFCBufLen();

    ctx.tmp_attn_out = arg.getTmpAttnOutBuf();
    ctx.tmp_attn_out_len = arg.getTmpAttnOutBufLen();

    ctx.logits = arg.getLogitsBuf();
    ctx.logits_len = arg.getLogitsBufLen();

    ctx.kcache = arg.getKCacheBuf();
    ctx.kcache_len = arg.getKCacheBufLen();

    ctx.vcache = arg.getVCacheBuf();
    ctx.vcache_len = arg.getVCacheBufLen();
}


XI_STATUS LLMInfer::initRTCtxTensors(
    OUT RTCtx & ctx, RTMemMgr & mgr, MOD RTArg & arg)
{
    //Init RTTensor.
    SafeTensor const* st;

    st = getSafeTensor(get_wte_weight_name());
    if (st == nullptr) {
        dumpAct(getLogMgr(), "\nerror:miss word-token-embedding");
        return XI_ERR;
    }
    ctx.wte = mgr.allocRTTensor();
    LLMInfer::convertSafeTensorToRTTensor(*ctx.wte, *st);

    st = getSafeTensor(get_ln_f_weight_name());
    if (st == nullptr) {
        dumpAct(getLogMgr(), "\nerror:miss ln_f.weight");
        return XI_ERR;
    }
    ctx.ln_f_w = mgr.allocRTTensor();
    LLMInfer::convertSafeTensorToRTTensor(*ctx.ln_f_w, *st);

    return initRTCtxTensorsLayer(ctx, mgr);
}


void LLMInfer::initRTRand()
{
    ::srand((unsigned)time(0));
}


XI_STATUS LLMInfer::initRTCtx(
    OUT RTCtx & ctx, RTMemMgr & mgr, MOD RTArg & arg)
{
    ::memset(&ctx, 0, sizeof(RTCtx));
    initRTDump(*this, ctx);
    ctx.n_layer = arg.n_layer;
    ctx.n_embd = arg.n_embd;
    ctx.n_head = arg.n_head;
    ctx.n_kvhead = arg.n_kvhead;
    ctx.max_pos = arg.n_positions;
    ctx.rope_theta = (float)arg.rope_theta;
    ctx.layernorm_epsilon = (float)arg.layernorm_epsilon;
    ctx.rmsnorm_epsilon = (float)arg.rmsnorm_epsilon;
    ctx.intermediate_size = arg.intermediate_size;
    ctx.softmax_epsilon = g_softmax_epsilon;
    ctx.tensor_dtype = arg.tensor_dtype;

    ctx.temperature = (float)xinfer::g_temperature;
    ctx.n_topk = xinfer::g_topk;
    ASSERT0(arg.vocab_size > 0 && arg.vocab_size < MAX_VOCAB_SIZE);
    ctx.vocab_size = arg.vocab_size;
    initRTCtxBuffer(ctx, arg);
    initRTRand();
    XI_STATUS status = initRTCtxTensors(ctx, mgr, arg);
    return status;
}


XI_STATUS LLMInfer::autoRegression(
    TOKID token, int64_t pos, MOD RTCtx & ctx, MOD RTArg & arg)
{
    START_TIMER(t, "autoRegression");

    //The pos cannot exceed the maximum sequence length.
    UINT maxlen = MIN(ctx.max_pos, xinfer::g_max_seq_len);
    TOKID eos_token_id = arg.eos_token_id;
    ASSERT0(eos_token_id != (TOKID)INVALID_TOKEN);
    xcom::StrBuf output(256);
    ASSERT0(dumpAct(getLogMgr(), "\nSTART autoRegression"));
    TOKID prev_token = INVALID_TOKEN;
    while (pos < maxlen) {
        START_TIMER_FMT(t2, ("%uth regression", pos));
        TOKID next_token = decode(token,  pos, &ctx);
        arg.addGenedToken(next_token);
        ASSERTN(all_unlocked(&ctx), ("\nruntime resource not yet unlocked"));
        if (next_token == (TOKID)INVALID_TOKEN) {
            dumpAct(getLogMgr(),
                "\nafter decode, the next_token is -1, where pos=%llu", pos);
            break;
        }
        if (next_token == prev_token && next_token == arg.newline_token_id) {
            dumpAct(getLogMgr(),
                "\nafter decode, the prev_token and next_token are both "
                "newline, where pos=%llu", pos);
            break;
        }
        ASSERT0(dumpToken(*this, next_token, pos));
        if (next_token == eos_token_id) {
            dumpAct(getLogMgr(),
                "\nafter decode, the next_token is eos, "
                "where pos=%llu", pos);
            break;
        }
        prev_token = token;
        token = next_token;
        pos++;
        END_TIMER_FMT(t2, ("%uth regression", pos));
    }
    END_TIMER(t, "autoRegression");
    return XI_SUCC;
}


XI_STATUS LLMInfer::generate(
    CHAR const* prompt, MOD RTArg & arg, MOD RTCtx & ctx)
{
    START_TIMER(t, "generate");
    ASSERT0(all_unlocked(&ctx));
    TokenArrayMgr tokarrmgr;
    TokenArray * tokarr = getTokenizer().encode(
        prompt, MAX_VOCAB_SIZE, tokarrmgr);
    if (tokarr == nullptr) {
        dumpAct(getLogMgr(), "\nerror:encode prompt failed");
        END_TIMER(t, "generate");
        return XI_ERR;
    }
    int64_t first_next_token;
    int64_t pos = prefill(&first_next_token, tokarr, &ctx);
    ASSERT0(pos >= 0);
    TOKID token = first_next_token;
    ASSERT0(token != (TOKID)INVALID_TOKEN);
    ASSERT0(dumpTokenArr(*this, tokarr));
    arg.addPromptToken(tokarr);
    XI_STATUS status = autoRegression(token, pos, ctx, arg);
    ASSERT0(all_unlocked(&ctx));
    END_TIMER(t, "generate");
    return status;
}


bool LLMInfer::verify(RTCtx const& ctx, RTArg const& arg)
{
    ASSERT0(ctx.wpe);
    UINT maxseqlen = RTTENSOR_max_seq_len(ctx.wpe);
    ASSERT0(maxseqlen == arg.getMaxSeqLen());
    return true;
}


bool LLMInfer::perform()
{
    RTArg arg;
    initRTArg(arg);
    arg.dump(*this, m_rm->getLogMgr());
    if (xinfer::g_prompt == nullptr) { return true; }

    RTMemMgr mgr;
    RTCtx ctx;
    XI_STATUS status = initRTCtx(ctx, mgr, arg);
    if (status != XI_SUCC) {
        return false;
    }
    ASSERT0(verify(ctx, arg));
    dumpRTCtx(&ctx, getRegionMgr());

    if (generate(xinfer::g_prompt, arg, ctx) != XI_SUCC) {
        return false;
    }
    arg.outputGenedTokens(*this, m_rm->getLogMgr());
    dump();
    return true;
}
//END LLMInfer

} //namespace xinfer
