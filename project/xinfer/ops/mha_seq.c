#include "ops.h"

void scaled_dot_attn_pos_bf16(
    uint16_t * out, uint16_t const*restrict q,
    uint16_t const*restrict k_cache, uint16_t const*restrict v_cache,
    int64_t pos, unsigned int n_head, unsigned int head_dim,
    float eps, float * tmp_attn_score_and_prob)
{
    XASSERT(out);
    XASSERT(q);
    XASSERT(k_cache);
    XASSERT(v_cache);
    XASSERT(pos >= 0);
    XASSERT(n_head > 0);
    XASSERT(head_dim > 0);
    int64_t seqlen = pos + 1; //Effect number of token:0-pos
    unsigned int n_embd =  n_head * head_dim;

    //Iterate each attention-head.
    float scale = 1.0f / sqrtf((float)head_dim);
    for (unsigned int h = 0; h < n_head; h++) {
        //qhead buffer diagram:
        //  -----------------------------
        //  |qhead0|........|qheadN|
        //
        //  kcache-----------------------
        //                  |pos0_kheadN|
        //                  |pos1_kheadN|
        //                  |pos2_kheadN|
        //  vcache-----------------------
        //                  |pos0_vheadN|
        //                  |pos1_vheadN|
        //                  |pos2_vheadN|
        //Attention score of qheadN in pos0 is qheadN*pos0_kheadN.
        //in pos1 is qheadN*pos1_kheadN, and in pos2 is qheadN*pos2_kheadN.
        unsigned int off_in_embd = h * head_dim;

        //q_head:pointer of query of current head.
        uint16_t const* q_head = q + off_in_embd;

        //Compute the score of each history positions in sequence.
        //Formula: score = Q*K^T/scale.
        //NOTE:k_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, need stride version.
        //score buffer diagram:
        //  score[pos0]   |qheadN * pos0_kheadN|
        //  score[pos1] = |qheadN * pos1_kheadN|
        //  score[pos2]   |qheadN * pos2_kheadN|
        for (int64_t t = 0; t < seqlen; t++) {
            uint16_t const* k_t_head = k_cache + t * n_embd + off_in_embd;
            float dot = 0.0f;
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot +
                    bf16_to_float(q_head[d]) * bf16_to_float(k_t_head[d]);
            }
            tmp_attn_score_and_prob[t] = dot * scale;
        }
        //Compute the probability according to scores.
        softmax_inplace_f32(tmp_attn_score_and_prob, seqlen, eps);

        //Compute the outputN of the headN by probabilities*vheadN.
        //The method is accumulating vheadN in each posX into corresponding
        //output buffer.
        //Output buffer diagram:
        //  tmpoutputN[pos0]   |probability_pos0 * vheadN|
        //  tmpoutputN[pos1] = |probability_pos1 * vheadN|
        //  tmpoutputN[pos2]   |probability_pos2 * vheadN|
        //then:
        //  outputN = tmpoutputN[pos0] + tmpoutputN[pos1] + tmpoutputN[pos2].
        //
        //NOTE:v_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, expects stride version.
        uint16_t * out_head = out + off_in_embd;
        memset(out_head, 0, head_dim * sizeof(uint16_t));
        for (int64_t t = 0; t < seqlen; t++) {
            uint16_t const* v_t_head = v_cache + t * n_embd + off_in_embd;
            float s = tmp_attn_score_and_prob[t];
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                float f32val = bf16_to_float(out_head[d]);
                f32val = f32val + s * bf16_to_float(v_t_head[d]);
                out_head[d] = float_to_bf16(f32val);
            }
        }
    }
}


void scaled_dot_attn_pos_f16(
    uint16_t * out, uint16_t const*restrict q,
    uint16_t const*restrict k_cache, uint16_t const*restrict v_cache,
    int64_t pos, unsigned int n_head, unsigned int head_dim,
    float eps, float * tmp_attn_score_and_prob)
{
    XASSERT(out);
    XASSERT(q);
    XASSERT(k_cache);
    XASSERT(v_cache);
    XASSERT(pos >= 0);
    XASSERT(n_head > 0);
    XASSERT(head_dim > 0);
    int64_t seqlen = pos + 1; //Effect number of token:0-pos
    unsigned int n_embd =  n_head * head_dim;

    //Iterate each attention-head.
    float scale = 1.0f / sqrtf((float)head_dim);
    for (unsigned int h = 0; h < n_head; h++) {
        //qhead buffer diagram:
        //  -----------------------------
        //  |qhead0|........|qheadN|
        //
        //  kcache-----------------------
        //                  |pos0_kheadN|
        //                  |pos1_kheadN|
        //                  |pos2_kheadN|
        //  vcache-----------------------
        //                  |pos0_vheadN|
        //                  |pos1_vheadN|
        //                  |pos2_vheadN|
        //Attention score of qheadN in pos0 is qheadN*pos0_kheadN.
        //in pos1 is qheadN*pos1_kheadN, and in pos2 is qheadN*pos2_kheadN.
        unsigned int off_in_embd = h * head_dim;

        //q_head:pointer of query of current head.
        uint16_t const* q_head = q + off_in_embd;

        //Compute the score of each history positions in sequence.
        //Formula: score = Q*K^T/scale.
        //NOTE:k_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, need stride version.
        //score buffer diagram:
        //  score[pos0]   |qheadN * pos0_kheadN|
        //  score[pos1] = |qheadN * pos1_kheadN|
        //  score[pos2]   |qheadN * pos2_kheadN|
        for (int64_t t = 0; t < seqlen; t++) {
            uint16_t const* k_t_head = k_cache + t * n_embd + off_in_embd;
            float dot = 0.0f;
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot +
                    fp16_to_float(q_head[d]) * fp16_to_float(k_t_head[d]);
            }
            tmp_attn_score_and_prob[t] = dot * scale;
        }
        //Compute the probability according to scores.
        softmax_inplace_f32(tmp_attn_score_and_prob, seqlen, eps);

        //Compute the outputN of the headN by probabilities*vheadN.
        //The method is accumulating vheadN in each posX into corresponding
        //output buffer.
        //Output buffer diagram:
        //  tmpoutputN[pos0]   |probability_pos0 * vheadN|
        //  tmpoutputN[pos1] = |probability_pos1 * vheadN|
        //  tmpoutputN[pos2]   |probability_pos2 * vheadN|
        //then:
        //  outputN = tmpoutputN[pos0] + tmpoutputN[pos1] + tmpoutputN[pos2].
        //
        //NOTE:v_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, expects stride version.
        uint16_t * out_head = out + off_in_embd;
        memset(out_head, 0, head_dim * sizeof(uint16_t));
        for (int64_t t = 0; t < seqlen; t++) {
            uint16_t const* v_t_head = v_cache + t * n_embd + off_in_embd;
            float s = tmp_attn_score_and_prob[t];
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                float f32val = fp16_to_float(out_head[d]);
                f32val = f32val + s * fp16_to_float(v_t_head[d]);
                out_head[d] = float_to_fp16(f32val);
            }
        }
    }
}


void scaled_dot_attn_pos_f32(
    float * out, float const*restrict q,
    float const*restrict k_cache, float const*restrict v_cache,
    int64_t pos, unsigned int n_head, unsigned int head_dim,
    float eps, float * tmp_attn_score_and_prob)
{
    XASSERT(out);
    XASSERT(q);
    XASSERT(k_cache);
    XASSERT(v_cache);
    XASSERT(pos >= 0);
    XASSERT(n_head > 0);
    XASSERT(head_dim > 0);
    int64_t seqlen = pos + 1; //Effect number of token:0-pos
    unsigned int n_embd =  n_head * head_dim;

    //Iterate each attention-head.
    float scale = 1.0f / sqrtf((float)head_dim);
    for (unsigned int h = 0; h < n_head; h++) {
        //qhead buffer diagram:
        //  -----------------------------
        //  |qhead0|........|qheadN|
        //
        //  kcache-----------------------
        //                  |pos0_kheadN|
        //                  |pos1_kheadN|
        //                  |pos2_kheadN|
        //  vcache-----------------------
        //                  |pos0_vheadN|
        //                  |pos1_vheadN|
        //                  |pos2_vheadN|
        //Attention score of qheadN in pos0 is qheadN*pos0_kheadN.
        //in pos1 is qheadN*pos1_kheadN, and in pos2 is qheadN*pos2_kheadN.
        unsigned int off_in_embd = h * head_dim;

        //q_head:pointer of query of current head.
        float const* q_head = q + off_in_embd;

        //Compute the score of each history positions in sequence.
        //Formula: score = Q*K^T/scale.
        //NOTE:k_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, need stride version.
        //score buffer diagram:
        //  score[pos0]   |qheadN * pos0_kheadN|
        //  score[pos1] = |qheadN * pos1_kheadN|
        //  score[pos2]   |qheadN * pos2_kheadN|
        for (int64_t t = 0; t < seqlen; t++) {
            float const* k_t_head = k_cache + t * n_embd + off_in_embd;
            float dot = 0.0f;
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot + q_head[d] * k_t_head[d];
            }
            tmp_attn_score_and_prob[t] = dot * scale;
        }
        //Compute the probability according to scores.
        softmax_inplace_f32(tmp_attn_score_and_prob, seqlen, eps);

        //Compute the outputN of the headN by probabilities*vheadN.
        //The method is accumulating vheadN in each posX into corresponding
        //output buffer.
        //Output buffer diagram:
        //  tmpoutputN[pos0]   |probability_pos0 * vheadN|
        //  tmpoutputN[pos1] = |probability_pos1 * vheadN|
        //  tmpoutputN[pos2]   |probability_pos2 * vheadN|
        //then:
        //  outputN = tmpoutputN[pos0] + tmpoutputN[pos1] + tmpoutputN[pos2].
        //
        //NOTE:v_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, expects stride version.
        float * out_head = out + off_in_embd;
        memset(out_head, 0, head_dim * sizeof(float));
        for (int64_t t = 0; t < seqlen; t++) {
            float const* v_t_head = v_cache + t * n_embd + off_in_embd;
            float s = tmp_attn_score_and_prob[t];
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                out_head[d] = out_head[d] + s * v_t_head[d];
            }
        }
    }
}


//tmp_seq_qkv
//  pos0: | Q[768] | K[768] | V[768] |
//  pos1: | Q[768] | K[768] | V[768] |
//  pos2: | Q[768] | K[768] | V[768] |
//
//  kcache---------------------------
//  pos0:|khead0|khead1| ... |kheadN|
//  pos1:|khead0|khead1| ... |kheadN|
//  pos2:|khead0|khead1| ... |kheadN|
//
//  vcache---------------------------
//  pos0:|vhead0|vhead1| ... |vheadN|
//  pos1:|vhead0|vhead1| ... |vheadN|
//  pos2:|vhead0|vhead1| ... |vheadN|
void scaled_dot_attn_seq(
    BYTE * out, BYTE const*restrict tmp_seq_qkv,
    void const*restrict k_cache, void const*restrict v_cache,
    int64_t startpos, int64_t endpos,
    unsigned int n_head, unsigned int n_kvhead,
    unsigned int head_dim, float eps, unsigned int elemsz,
    RTTENSOR_DTYPE dtype)
{
    XASSERT(out);
    XASSERT(tmp_seq_qkv);
    XASSERT(k_cache);
    XASSERT(v_cache);
    XASSERT(n_head > 0);
    XASSERT(head_dim > 0);
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    unsigned int kvdim = 2 * n_kvhead * head_dim;
    unsigned int qdim = n_head * head_dim;
    unsigned int qkvdim = qdim + kvdim;
    int64_t seqlen = endpos - startpos + 1;

    //Each head requires temporary storage for attn_score(probability)[seqlen]
    //NOTE:Do not malloc if the seqlen is not very large, use small
    //stack allocated buffers.
    //Under decode phase, seqlen is at most max_pos. Stack is unsuitable
    //for overly large max_pos values.
    float * tmp_attn_score_and_prob; //the buffer for tmp score and probability.
    float tmp[MAX_ELEMNUM_ON_STACK];
    unsigned int const score_type_sz = sizeof(float);
    if (seqlen > MAX_ELEMNUM_ON_STACK) {
        tmp_attn_score_and_prob = malloc(seqlen * score_type_sz);
        XASSERT(dump("malloc %u bytes memory,seqlen",
            seqlen * score_type_sz));
    } else { tmp_attn_score_and_prob = tmp; }

    //Requiring a number of operations that equal to the factorial of
    //seqlen.
    unsigned int qkvsz = qkvdim * elemsz;
    unsigned int qsz = qdim * elemsz;
    switch (dtype) {
    case RTTENSOR_DTYPE_F32: {
        for (int64_t pos = 0; pos < seqlen; pos++) {
            BYTE * q = tmp_seq_qkv + pos * qkvsz;
            BYTE * subout = out + pos * qsz;
            scaled_dot_attn_pos_f32(
                (float*)subout, (float const*)q, k_cache, v_cache,
                startpos + pos, n_head, head_dim, eps,
                tmp_attn_score_and_prob);
            XASSERT(dump_vec(subout, qdim, dtype,
                "scaled_dot_attn_pos_f32 output,pos%u", startpos + pos));
        }
        break;
    }
    case RTTENSOR_DTYPE_F16:
        for (int64_t pos = 0; pos < seqlen; pos++) {
            BYTE * q = tmp_seq_qkv + pos * qkvsz;
            BYTE * subout = out + pos * qsz;
            scaled_dot_attn_pos_f16(
                (uint16_t*)subout, (uint16_t const*)q, k_cache, v_cache,
                startpos + pos, n_head, head_dim, eps, tmp_attn_score_and_prob);
            XASSERT(dump_vec(subout, qdim, dtype,
                "scaled_dot_attn_pos_f16 output,pos%u", startpos + pos));
        }
        break;
    case RTTENSOR_DTYPE_BF16:
        for (int64_t pos = 0; pos < seqlen; pos++) {
            BYTE * q = tmp_seq_qkv + pos * qkvsz;
            BYTE * subout = out + pos * qsz;
            scaled_dot_attn_pos_bf16(
                (uint16_t*)subout, (uint16_t const*)q, k_cache, v_cache,
                startpos + pos, n_head, head_dim, eps, tmp_attn_score_and_prob);
            XASSERT(dump_vec(subout, qdim, dtype,
                "scaled_dot_attn_pos_bf16 output,pos%u", startpos + pos));
        }
        break;
    default:
        XASSERT(0); //Unsupport
        break;
    }
    if (seqlen > MAX_ELEMNUM_ON_STACK) {
        free(tmp_attn_score_and_prob);
        XASSERT(dump("free %u bytes memory", seqlen * sizeof(float)));
    }
}


void mha_seq(
    void *restrict out, void const*restrict in,
    RTTensor *restrict c_attn_w, RTTensor *restrict c_attn_b,
    RTTensor *restrict c_proj_w, RTTensor *restrict c_proj_b,
    unsigned int layer, int64_t startpos, int64_t endpos,
    RTCtx *restrict ctx)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(c_attn_w);
    XASSERT(c_attn_b);
    XASSERT(c_proj_w);
    XASSERT(c_proj_b);
    XASSERT(ctx);

    BYTE * tmp_seq_qkv = lock_tmp_qkv(ctx);
    XASSERT(tmp_seq_qkv);
    XASSERT(ctx->n_embd > 0);
    XASSERT(ctx->n_head > 0);
    XASSERT(ctx->n_head * ctx->head_dim == ctx->n_embd);

    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    int64_t seqlen = endpos - startpos + 1;
    XASSERT(seqlen < ctx->max_pos);

    XASSERT(endpos <= ctx->max_pos);
    unsigned int qkv_dim = 3 * ctx->n_embd;

    XASSERT(c_attn_w->data);
    XASSERT(c_attn_w->ndim == 2);
    XASSERT(c_attn_w->elem_num == (int64_t)ctx->n_embd * qkv_dim);
    XASSERT(c_attn_w->shape[0] == ctx->n_embd);
    XASSERT(c_attn_w->shape[1] == qkv_dim);

    XASSERT(c_attn_b->data);
    XASSERT(c_attn_b->ndim == 1);
    XASSERT(c_attn_b->shape[0] == qkv_dim);
    XASSERT(c_attn_b->elem_num == qkv_dim);

    XASSERT(c_proj_w->data);
    XASSERT(c_proj_w->elem_num == ctx->n_embd * ctx->n_embd);
    XASSERT(c_proj_w->ndim == 2);
    XASSERT(c_proj_w->shape[0] == ctx->n_embd);
    XASSERT(c_proj_w->shape[1] == ctx->n_embd);

    XASSERT(c_proj_b->data);
    XASSERT(c_proj_b->ndim == 1);
    XASSERT(c_proj_b->shape[0] == ctx->n_embd);
    XASSERT(c_proj_b->elem_num == ctx->n_embd);

    //in is [seqlen*n_embd], c_attn_w is [n_embd,3*n_embd],
    //c_attn_b is [1,3*n_embd].
    //QKV projection: tmp_seq_qkv[seqlen, 3*n_embd] = in[seqlen*n_embd]*W+bias.
    //Assum n_embd=768, then tmp_seq_qkv is:
    //  pos0: | Q[768] | K[768] | V[768] |
    //  pos1: | Q[768] | K[768] | V[768] |
    //  pos2: | Q[768] | K[768] | V[768] |
    unsigned int n_embd = ctx->n_embd;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    //XASSERT(dump_vec(in, seqlen * n_embd, dtype, "in:"));
    matmulBias(tmp_seq_qkv, in, seqlen, n_embd, c_attn_w, c_attn_b);

    XASSERT(dumpQKV(tmp_seq_qkv, seqlen, startpos, ctx->n_head,
        ctx->n_kvhead, ctx->head_dim, dtype));

    //Point to layer of pos.
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    XASSERT(elemsz != 0);
    storeKVCacheFromQKV(tmp_seq_qkv, layer, elemsz, startpos, endpos, ctx);

    //Write KV cache at position pos.
    //KV cache layout:[max_pos*n_embd]
    XASSERT(n_embd == ctx->n_head * ctx->head_dim);
    int64_t elem_num_per_layer_kv = ctx->max_pos * n_embd;
    BYTE * kcache = lock_kcache(ctx);
    BYTE * vcache = lock_vcache(ctx);
    BYTE * layer_kcache_start = kcache + layer * elem_num_per_layer_kv * elemsz;
    BYTE * layer_vcache_start = vcache + layer * elem_num_per_layer_kv * elemsz;

    //tmp_attn_out, which is [seqlen*n_embd], shares the same buffer with 'k+v'.
    //BYTE * tmp_attn_out = k;
    //XASSERT(2*ctx->n_kvhead >= ctx->n_head);
    BYTE * tmp_attn_out = malloc(seqlen * n_embd * elemsz);
    XASSERT(dump("malloc %u bytes memory,tmp_attn_out:%u*%u",
        seqlen * n_embd * elemsz, seqlen, n_embd));
    scaled_dot_attn_seq(
        tmp_attn_out, tmp_seq_qkv, layer_kcache_start, layer_vcache_start,
        startpos, endpos, ctx->n_head, ctx->n_kvhead, ctx->head_dim,
        ctx->layernorm_epsilon, elemsz, dtype);

    //output projection
    //tmp_attn_out is [seqlen*n_embd], out is [seqlen*n_embd].
    //[seqlen*n_embd] = [seqlen*n_embd] x [n_embd*n_embd] + [1*n_embd].

    //XASSERT(dump_mat(tmp_attn_out, seqlen, n_embd, dtype,
    //    "before matmulBias,tmp_attn_out,pos0~pos%u", seqlen - 1));
    matmulBias(out, tmp_attn_out, seqlen, n_embd, c_proj_w, c_proj_b);
    XASSERT(dump_vec(out, seqlen * n_embd, dtype,
        "mha_seq output projection,pos%u~pos%u", startpos, endpos));

    free(tmp_attn_out);
    XASSERT(dump("free %u bytes memory", seqlen * n_embd * elemsz));

    unlock_tmp_qkv(ctx);
    unlock_kcache(ctx);
    unlock_vcache(ctx);
}
