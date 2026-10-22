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

bool dumpAct(xoc::RegionMgr const* rm, CHAR const* format, ...)
{
    if (rm == nullptr || format == nullptr) { return true; }
    if (!rm->isLogMgrInit()) { return true; }
    if (!g_dump_act) { return true; }
    xoc::LogMgr * lm = rm->getLogMgr();
    va_list args;
    va_start(args, format);
    xoc::note_arg(lm, format, args);
    va_end(args);
    return true;
}


bool dumpAct(xoc::LogMgr * lm, CHAR const* format, ...)
{
    if (lm == nullptr || !lm->is_init()) { return true; }
    if (!g_dump_act) { return true; }
    va_list args;
    va_start(args, format);
    xoc::note_arg(lm, format, args);
    va_end(args);
    return true;
}


bool dumpTokenVectorToUtf8Buffer(
    OUT StrBuf & output, LLMInfer & infer, TokenVec const& tokvec)
{
    xcom::StrBuf tmp(64);
    for (UINT i = 0; i < tokvec.get_elem_count(); i++) {
        TOKID tid = tokvec.get(i);
        infer.getTokenizer().decode(tid, tmp, false);
        output.strcat(tmp);
    } 
    return true;
}


bool dumpToken(LLMInfer & infer, TOKID token, UINT count)
{
    if (!infer.getRegionMgr()->isLogMgrInit()) { return true; }
    xcom::StrBuf output(64);
    infer.getTokenizer().decode(token, output, false);
    LogMgr * lm = infer.getLogMgr();
    note(lm, "\nTOKENID:%u,count=%u,%s:", token, count, output.getBuf());
    prt(lm, "[");
    for (BYTE const* p = (BYTE const*)output.getBuf(); *p != 0; p++) {
        if (p != (BYTE const*)output.getBuf()) {
            prt(lm, ",");
        }
        prt(lm, "0x%02X", *p);
    }
    prt(lm, "]");
    return true;
}


bool dumpTokenArr(LLMInfer & infer, TokenArray const* tokarr)
{
    if (!infer.getRegionMgr()->isLogMgrInit()) { return true; }
    note(infer.getRegionMgr(), "\nDUMP TOKARR:");
    infer.getLogMgr()->incIndent(2);
    for (UINT i = 0; i < tokarr->array_elem_num; i++) {
        TOKID tkid = tokarr->array[i];
        dumpToken(infer, tkid, i);
    }
    infer.getLogMgr()->decIndent(2);
    return true;
}


static void dumpRTTensorData(
    RTTensor const* rtt, xoc::LogMgr * lm, INT dumpnum)
{
    note(lm, "\n{");
    UINT elemnum =
        dumpnum < 0 ? rtt->elem_num : MIN((INT)rtt->elem_num, dumpnum);
    switch (rtt->dtype) {
    case RTTENSOR_DTYPE_F32: {
        float const* data = (float const*)rtt->data;
        for (UINT i = 0; i < elemnum; i++) {
            if (i != 0) { prt(lm, " "); }
            prt(lm, "%0.4f", data[i]);
        }
        break;
    }
    case RTTENSOR_DTYPE_F16: {
        UINT16 const* data = (UINT16 const*)rtt->data;
        for (UINT i = 0; i < elemnum; i++) {
            if (i != 0) { prt(lm, " "); }
            float fp32 = xcom::half2float(data[i]);
            prt(lm, "%0.4f", fp32);
        }
        break;
    }
    case RTTENSOR_DTYPE_BF16: {
        UINT16 const* data = (UINT16 const*)rtt->data;
        for (UINT i = 0; i < elemnum; i++) {
            if (i != 0) { prt(lm, " "); }
            float fp32 = xcom::bf162float(data[i]);
            prt(lm, "%0.4f", fp32);
        }
        break;
    }
    case RTTENSOR_DTYPE_I8:
    case RTTENSOR_DTYPE_U8: {
        UINT8 const* data = (UINT8 const*)rtt->data;
        for (UINT i = 0; i < elemnum; i++) {
            if (i != 0) { prt(lm, " "); }
            prt(lm, "0x%x", data[i]);
        }
        break;
    }
    case RTTENSOR_DTYPE_I16: {
        UINT16 const* data = (UINT16 const*)rtt->data;
        for (UINT i = 0; i < elemnum; i++) {
            if (i != 0) { prt(lm, " "); }
            prt(lm, "0x%x", data[i]);
        }
        break;
    }
    case RTTENSOR_DTYPE_I32: {
        UINT32 const* data = (UINT32 const*)rtt->data;
        for (UINT i = 0; i < elemnum; i++) {
            if (i != 0) { prt(lm, " "); }
            prt(lm, "0x%x", data[i]);
        }
        break;
    }
    case RTTENSOR_DTYPE_I64: {
        UINT64 const* data = (UINT64 const*)rtt->data;
        for (UINT i = 0; i < elemnum; i++) {
            if (i != 0) { prt(lm, " "); }
            prt(lm, "0x%x", data[i]);
        }
        break;
    }
    default: ASSERT0(0); //TODO:
    }
    if (elemnum < rtt->elem_num) {
        prt(lm, " ...}");
    } else {
        prt(lm, "}");
    }
}


void dumpRTTensor(RTTensor const* rtt, xoc::RegionMgr const* rm)
{
    if (rtt == nullptr) { return; }
    if (!rm->isLogMgrInit()) { return; }
    xoc::LogMgr * lm = rm->getLogMgr();
    note(lm, "\n-- RTTensor --");
    CHAR const* str = nullptr;
    switch (rtt->dtype) {
    case RTTENSOR_DTYPE_UNDEF:   str = "RTTENSOR_DTYPE_UNDEF"; break;
    case RTTENSOR_DTYPE_F32:     str = "RTTENSOR_DTYPE_F32"; break;
    case RTTENSOR_DTYPE_F16:     str = "RTTENSOR_DTYPE_F16"; break;
    case RTTENSOR_DTYPE_BF16:    str = "RTTENSOR_DTYPE_BF16"; break;
    case RTTENSOR_DTYPE_I8:      str = "RTTENSOR_DTYPE_I8"; break;
    case RTTENSOR_DTYPE_U8:      str = "RTTENSOR_DTYPE_U8"; break;
    case RTTENSOR_DTYPE_I16:     str = "RTTENSOR_DTYPE_I16"; break;
    case RTTENSOR_DTYPE_I32:     str = "RTTENSOR_DTYPE_I32"; break;
    case RTTENSOR_DTYPE_I64:     str = "RTTENSOR_DTYPE_I64"; break;
    case RTTENSOR_DTYPE_BOOL:    str = "RTTENSOR_DTYPE_BOOL"; break;
    case RTTENSOR_DTYPE_F8_E4M3: str = "RTTENSOR_DTYPE_F8_E4M3"; break;
    case RTTENSOR_DTYPE_F8_E5M2: str = "RTTENSOR_DTYPE_F8_E5M2"; break;
    default: ASSERT0(0);
    }
    note(lm, "\ndtype=%s", str);
    note(lm, "\nndim=%d", rtt->ndim);
    note(lm, "\nelem_num=%d", rtt->elem_num);
    note(lm, "\nshape=[");
    for (int i = 0; i < MAX_TENSOR_DIM_NUM; i++) {
        if (i != 0) { prt(lm, ","); }
        prt(lm, "%d", rtt->shape[i]);
    }
    prt(lm, "]");

    note(lm, "\ndata=0x%x,first %d elements:",
         rtt->data, g_max_tensor_data_dump_num);
    lm->incIndent(2);
    dumpRTTensorData(rtt, lm, g_max_tensor_data_dump_num);
    lm->decIndent(2);
}


void dumpRTCtxDebug(RTCtx const* ctx, xoc::RegionMgr const* rm)
{
    #ifdef _DEBUG_
    note(rm, "\ndump_l2norm=%d", ctx->dump_l2norm);
    note(rm, "\ndump_vec=%d", ctx->dump_vec);
    note(rm, "\ndump=%d", ctx->dump);
    note(rm, "\nlock_tmp1=%d", ctx->lock_tmp1);
    note(rm, "\nlock_tmp2=%d", ctx->lock_tmp2);
    note(rm, "\nlock_tmp_qkv=%d", ctx->lock_tmp_qkv);
    note(rm, "\nlock_tmp_fc=%d", ctx->lock_tmp_fc);
    note(rm, "\nlock_tmp_attn_out=%d", ctx->lock_tmp_attn_out);
    note(rm, "\nlock_hidden=%d", ctx->lock_hidden);
    note(rm, "\nlock_logits=%d", ctx->lock_logits);
    note(rm, "\nlock_kcache=%d", ctx->lock_kcache);
    note(rm, "\nlock_vcache=%d", ctx->lock_vcache);
    #endif
    #ifdef _DUMP_BY_LOGMGR_
    note(rm, "\nlm=0x%x", ctx->lm);
    #endif
}


void dumpRTCtx(RTCtx const* ctx, xoc::RegionMgr const* rm)
{
    if (!rm->isLogMgrInit()) { return; }
    if (!g_dump_rtctx) { return; }
    note(rm, "\n-- RTCtx --");
    if (ctx == nullptr) { return; }
    rm->getLogMgr()->incIndent(2);
    note(rm, "\nn_layer=%d", ctx->n_layer);
    note(rm, "\nn_embd=%d", ctx->n_embd);
    note(rm, "\nn_head=%d", ctx->n_head);
    note(rm, "\nn_kvhead=%d", ctx->n_kvhead);
    note(rm, "\nn_topk=%d", ctx->n_topk);
    note(rm, "\nvocab_size=%d", ctx->vocab_size);
    note(rm, "\nmax_pos=%d", ctx->max_pos);
    note(rm, "\nrope_theta=%f", ctx->rope_theta);
    note(rm, "\nlayer_norm_epsilon=%f", ctx->layernorm_epsilon);
    note(rm, "\nrms_norm_epsilon=%f", ctx->rmsnorm_epsilon);
    note(rm, "\ntemperature=%f", ctx->temperature);

    note(rm, "\ntmp1=0x%x", ctx->tmp1);
    note(rm, "\ntmp1_len=%llu", ctx->tmp1_len);

    note(rm, "\ntmp2=0x%x", ctx->tmp2);
    note(rm, "\ntmp2_len=%llu", ctx->tmp2_len);

    note(rm, "\ntmp_qkv=0x%x", ctx->tmp_qkv);
    note(rm, "\ntmp_qkv_len=%llu", ctx->tmp_qkv_len);

    note(rm, "\ntmp_fc=0x%x", ctx->tmp_fc);
    note(rm, "\ntmp_fc_len=%llu", ctx->tmp_fc_len);

    note(rm, "\ntmp_attn_out=0x%x", ctx->tmp_attn_out);
    note(rm, "\ntmp_attn_out_len=%llu", ctx->tmp_attn_out_len);

    note(rm, "\nhidden=0x%x", ctx->hidden);
    note(rm, "\nhidden_len=%llu", ctx->hidden_len);

    note(rm, "\nlogits=0x%x", ctx->logits);
    note(rm, "\nlogits_len=%llu", ctx->logits_len);

    note(rm, "\nkcache=0x%x", ctx->kcache);
    note(rm, "\nkcache_len=%llu", ctx->kcache_len);

    note(rm, "\nvcache=0x%x", ctx->vcache);
    note(rm, "\nvcache_len=%llu", ctx->vcache_len);

    note(rm, "\nwte=0x%x", ctx->wte);
    rm->getLogMgr()->incIndent(2);
    dumpRTTensor(ctx->wte, rm);
    rm->getLogMgr()->decIndent(2);

    note(rm, "\nwpe=0x%x", ctx->wpe);
    rm->getLogMgr()->incIndent(2);
    dumpRTTensor(ctx->wpe, rm);
    rm->getLogMgr()->decIndent(2);

    note(rm, "\nln_f_w=0x%x", ctx->ln_f_w);
    rm->getLogMgr()->incIndent(2);
    dumpRTTensor(ctx->ln_f_w, rm);
    rm->getLogMgr()->decIndent(2);

    note(rm, "\nln_f_b=0x%x", ctx->ln_f_b);
    rm->getLogMgr()->incIndent(2);
    dumpRTTensor(ctx->ln_f_b, rm);
    rm->getLogMgr()->decIndent(2);

    note(rm, "\nln_1_weight=0x%x", ctx->ln_1_weight);
    if (ctx->ln_1_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->ln_1_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nln_1_bias=0x%x", ctx->ln_1_bias);
    if (ctx->ln_1_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->ln_1_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nln_2_weight=0x%x", ctx->ln_2_weight);
    if (ctx->ln_2_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->ln_2_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nln_2_bias=0x%x", ctx->ln_2_bias);
    if (ctx->ln_2_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->ln_2_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nattn_c_attn_weight=0x%x", ctx->c_attn_weight);
    if (ctx->c_attn_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->c_attn_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nattn_c_attn_bias=0x%x", ctx->c_attn_bias);
    if (ctx->c_attn_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->c_attn_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\no_proj_weight=0x%x", ctx->o_proj_weight);
    if (ctx->o_proj_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->o_proj_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\no_proj_bias=0x%x", ctx->o_proj_bias);
    if (ctx->o_proj_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->o_proj_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nmlp_c_fc_weight=0x%x", ctx->mlp_c_fc_weight);
    if (ctx->mlp_c_fc_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->mlp_c_fc_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nmlp_c_fc_bias=0x%x", ctx->mlp_c_fc_bias);
    if (ctx->mlp_c_fc_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->mlp_c_fc_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nmlp_down_proj_weight=0x%x", ctx->mlp_down_proj_weight);
    if (ctx->mlp_down_proj_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->mlp_down_proj_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nmlp_down_proj_bias=0x%x", ctx->mlp_down_proj_bias);
    if (ctx->mlp_down_proj_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->mlp_down_proj_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    //QWEN specific
    note(rm, "\nq_proj_weight=0x%x", ctx->q_proj_weight);
    if (ctx->q_proj_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->q_proj_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nq_proj_bias=0x%x", ctx->q_proj_bias);
    if (ctx->q_proj_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->q_proj_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nk_proj_weight=0x%x", ctx->k_proj_weight);
    if (ctx->k_proj_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->k_proj_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }


    note(rm, "\nk_proj_bias=0x%x", ctx->k_proj_bias);
    if (ctx->k_proj_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->k_proj_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nv_proj_weight=0x%x", ctx->v_proj_weight);
    if (ctx->v_proj_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->v_proj_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nv_proj_bias=0x%x", ctx->v_proj_bias);
    if (ctx->v_proj_bias != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->v_proj_bias[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nmlp_gate_proj_weight=0x%x", ctx->mlp_gate_proj_weight);
    if (ctx->mlp_gate_proj_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->mlp_gate_proj_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    note(rm, "\nmlp_up_proj_weight=0x%x", ctx->mlp_up_proj_weight);
    if (ctx->mlp_up_proj_weight != nullptr) {
        rm->getLogMgr()->incIndent(2);
        for (unsigned int i = 0; i < ctx->n_layer; i++) {
            note(rm, "\nlayer:%d", i);
            dumpRTTensor(ctx->mlp_up_proj_weight[i], rm);
            note(rm, "\n");
        }
        rm->getLogMgr()->decIndent(2);
    }

    dumpRTCtxDebug(ctx, rm);
    rm->getLogMgr()->decIndent(2);
}

} //namespace xinfer
