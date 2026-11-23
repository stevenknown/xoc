#include "ops.h"

//RoPE for one head, in place. Rotate each adjacent pair (2i, 2i+1):
//Treat adjacent pair (x[2t], x[2t+1]) as complex number, apply rotation
//angle theta_t
//  theta_i = pos / rope_theta^(2i / head_dim)
//  out[2i] = x[2i] * cos - x[2i+1] * sin
//  out[2i+1] = x[2i] * sin + x[2i+1] * cos
//TODO:compute trigonometric function via a lookup-table.
static void rope_rotate_inplace_f32(
    float * in, int64_t startpos, int64_t endpos, unsigned int n_head,
    unsigned int pos_dim, unsigned int head_dim, float rope_theta)
{
    float * subin = in;
    for (int64_t pos = startpos; pos <= endpos; pos++, subin += pos_dim) {
        float * x = subin;
        for (unsigned int hidx = 0; hidx < n_head; hidx++, x += head_dim) {
            XASSERT((head_dim % 2) == 0);
            for (unsigned int i = 0; i < head_dim; i += 2) {
                float theta = (float)pos
                    / powf(rope_theta, (float)i / (float)head_dim);
                float cos_t = cosf(theta);
                float sin_t = sinf(theta);
                float x0 = x[i];
                float x1 = x[i + 1];
                x[i] = x0 * cos_t - x1 * sin_t;
                x[i + 1] = x0 * sin_t + x1 * cos_t;
            }
        }
    }
}


static void rope_rotate_inplace_f16(
    uint16_t * in, int64_t startpos, int64_t endpos, unsigned int n_head,
    unsigned int pos_dim, unsigned int head_dim, float rope_theta)
{
    uint16_t * subin = in;
    for (int64_t pos = startpos; pos <= endpos; pos++, subin += pos_dim) {
        uint16_t * x = subin;
        for (unsigned int hidx = 0; hidx < n_head; hidx++, x += head_dim) {
            XASSERT((head_dim % 2) == 0);
            for (unsigned int i = 0; i < head_dim; i += 2) {
                float theta = (float)pos
                    / powf(rope_theta, (float)i / (float)head_dim);
                float cos_t = cosf(theta);
                float sin_t = sinf(theta);
                float x0 = fp16_to_float(x[i]);
                float x1 = fp16_to_float(x[i + 1]);
                float v0 = x0 * cos_t - x1 * sin_t;
                float v1 = x0 * sin_t + x1 * cos_t;
                x[i] = float_to_fp16(v0);
                x[i + 1] = float_to_fp16(v1);
            }
        }
    }
}


static void rope_rotate_inplace_bf16(
    uint16_t * in, int64_t startpos, int64_t endpos, unsigned int n_head,
    unsigned int pos_dim, unsigned int head_dim, float rope_theta)
{
    uint16_t * subin = in;
    for (int64_t pos = startpos; pos <= endpos; pos++, subin += pos_dim) {
        uint16_t * x = subin;
        for (unsigned int hidx = 0; hidx < n_head; hidx++, x += head_dim) {
            XASSERT((head_dim % 2) == 0);
            for (unsigned int i = 0; i < head_dim; i += 2) {
                float theta = (float)pos
                    / powf(rope_theta, (float)i / (float)head_dim);
                float cos_t = cosf(theta);
                float sin_t = sinf(theta);
                float x0 = bf16_to_float(x[i]);
                float x1 = bf16_to_float(x[i + 1]);
                float v0 = x0 * cos_t - x1 * sin_t;
                float v1 = x0 * sin_t + x1 * cos_t;
                x[i] = float_to_bf16(v0);
                x[i + 1] = float_to_bf16(v1);
            }
        }
    }
}


static void rope_rotate_inplace(
    void * in, int64_t startpos, int64_t endpos, unsigned int n_head,
    unsigned int pos_dim, unsigned int head_dim, float rope_theta,
    RTTENSOR_DTYPE dtype)
{
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        rope_rotate_inplace_f32(in, startpos, endpos, n_head, pos_dim,
            head_dim, rope_theta);
        return;
    case RTTENSOR_DTYPE_F16:
        rope_rotate_inplace_f16(in, startpos, endpos, n_head, pos_dim,
            head_dim, rope_theta);
        return;
    case RTTENSOR_DTYPE_BF16:
        rope_rotate_inplace_bf16(in, startpos, endpos, n_head, pos_dim,
            head_dim, rope_theta);
        return;
    default: XASSERT(0); //TODO:
    }
}


void gqa_bf16(
    uint16_t *restrict o_attn,
    uint16_t const*restrict tmpq, uint16_t const*restrict layer_kcache,
    uint16_t const*restrict layer_vcache,
    unsigned int n_qhead, unsigned int n_kvhead,
    unsigned int head_dim, unsigned int kvdim, int64_t pos, float eps,
    RTCtx *restrict ctx)
{
    XASSERT(dump("start gqa_bf16,pos%u", pos));
    float * tmplogits = lock_tmp3(ctx);
    XASSERT(ctx->tmp3_len >= (pos+1)*sizeof(float));
    unsigned int elemsz = sizeof(uint16_t);

    //GQA attention over keys 0..pos(causal)

    //Compute the Q-head number that is related to KV-head.
    //Multiple Q-heads are related to one KV-head.
    //e.g:
    //| qheadidx0 | qheadidx1 | qheadidx2 | qheadidx3 | qheadidx4 | qheadidx5 |
    //| kvhead0   | kvhead1   |
    //Thus
    //kvhead0 is related to (qheadidx0, qheadidx1, qheadidx2).
    //kvhead1 is related to (qheadidx3, qheadidx4, qheadidx5).

    unsigned int group_size = n_qhead / n_kvhead;
    XASSERT(n_qhead % n_kvhead == 0);
    float scale = 1.0f / sqrtf((float)head_dim);
    for (unsigned int qhidx = 0; qhidx < n_qhead; qhidx++) {
        uint16_t const* subqhead = tmpq + qhidx * head_dim;

        //Given q-head idx, compute the kv-head idx.
        //e.g:qhidx is 3, the related kvheadidx is 3/3 == 1.
        unsigned int kvheadidx = qhidx / group_size;

        //tmplogits:score[p] = qhead*khead[p]*scale, p = 0..pos
        float max_s = g_float_neg_infinity;
        unsigned int kvhead_ofst = kvheadidx * head_dim;
        for (int64_t p = 0; p <= pos; p++) {
            uint16_t const* layer_kcache_pos = layer_kcache + p * kvdim;

            //khead points to the k-head at pos-N of specific layer-M
            uint16_t const* khead = layer_kcache_pos + kvhead_ofst;

            //Compute dot-product of qhead and khead.
            float dot = 0.0f;
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot +
                    bf16_to_float(subqhead[d]) * bf16_to_float(khead[d]);
            }
            float s = dot * scale;
            tmplogits[p] = s;
            if (s > max_s) {
                //Find max value for numerical stability.
                //Compute max here to make full use of data-locality.
                max_s = s;
            }
        }
        //Do softmax over [0,pos].
        float sum = 0.0f;
        for (int64_t p = 0; p <= pos; p++) {
            float val = tmplogits[p] - max_s;
            tmplogits[p] = expf(val);
            sum = sum + tmplogits[p];
        }
        XASSERT(sum >= 0.0f);
        sum = fmaxf(sum, eps);

        //Divide sum, get probability.
        float recip_sum = 1.0f / sum;

        //weighted sum of v.
        uint16_t * o_subattn = o_attn + qhidx * head_dim;
        memset(o_subattn, 0, head_dim * elemsz);
        for (int64_t p = 0; p <= pos; p++) {
            uint16_t const* layer_vcache_pos = layer_vcache + p * kvdim;

            //vhead points to the v-head at pos-N of specific layer-M
            uint16_t const* vhead = layer_vcache_pos + kvhead_ofst;
            float w = tmplogits[p] * recip_sum;

            //vectorizable.
            //Compute the weighted sum of v.
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                float val = bf16_to_float(o_subattn[d]) +
                    w * bf16_to_float(vhead[d]);
                o_subattn[d] = float_to_bf16(val);
            }
        }
    }
    XASSERT(dump_vec(o_attn, n_qhead * head_dim, ctx->tensor_dtype,
        "gqa_bf16 output,pos%u", pos));
    unlock_tmp3(ctx);
}


void gqa_f16(
    uint16_t *restrict o_attn,
    uint16_t const*restrict tmpq, uint16_t const*restrict layer_kcache,
    uint16_t const*restrict layer_vcache,
    unsigned int n_qhead, unsigned int n_kvhead,
    unsigned int head_dim, unsigned int kvdim, int64_t pos, float eps,
    RTCtx *restrict ctx)
{
    XASSERT(dump("start gqa_f16,pos%u", pos));
    float * tmplogits = lock_tmp3(ctx);
    unsigned int elemsz = sizeof(uint16_t);
    XASSERT(ctx->tmp3_len >= (pos+1)*elemsz);

    //GQA attention over keys 0..pos (causal)

    //Compute the Q-head number that is related to KV-head.
    //Multiple Q-heads are related to one KV-head.
    //e.g:
    //| qheadidx0 | qheadidx1 | qheadidx2 | qheadidx3 | qheadidx4 | qheadidx5 |
    //| kvhead0   | kvhead1   |
    //Thus
    //kvhead0 is related to (qheadidx0, qheadidx1, qheadidx2).
    //kvhead1 is related to (qheadidx3, qheadidx4, qheadidx5).

    unsigned int group_size = n_qhead / n_kvhead;
    XASSERT(n_qhead % n_kvhead == 0);
    float scale = 1.0f / sqrtf((float)head_dim);
    for (unsigned int qhidx = 0; qhidx < n_qhead; qhidx++) {
        uint16_t const* subqhead = tmpq + qhidx * head_dim;

        //Given q-head idx, compute the kv-head idx.
        //e.g:qhidx is 3, the related kvheadidx is 3/3 == 1.
        unsigned int kvheadidx = qhidx / group_size;

        //tmplogits s[p] = qhidx . kh[p] * scale, p = 0..pos
        float max_s = g_float_neg_infinity;
        unsigned int kvhead_ofst = kvheadidx * head_dim;
        for (int64_t p = 0; p <= pos; p++) {
            uint16_t const* layer_kcache_pos = layer_kcache + p * kvdim;

            //khead points to the k-head at pos-N of specific layer-M
            uint16_t const* khead = layer_kcache_pos + kvhead_ofst;

            //Compute dot-product of qhead and khead.
            float dot = 0.0f;
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot +
                    fp16_to_float(subqhead[d]) * fp16_to_float(khead[d]);
            }
            float s = dot * scale;
            tmplogits[p] = s;
            if (s > max_s) {
                //Find max value for numerical stability.
                //Compute max here to make full use of data-locality.
                max_s = s;
            }
        }
        //Do softmax over [0,pos].
        float sum = 0.0f;
        for (int64_t p = 0; p <= pos; p++) {
            float val = tmplogits[p] - max_s;
            tmplogits[p] = expf(val);
            sum = sum + tmplogits[p];
        }
        XASSERT(sum >= 0.0f);
        sum = fmaxf(sum, eps);

        //Divide sum, get probability.
        float recip_sum = 1.0f / sum;

        //weighted sum of v.
        uint16_t * o_subattn = o_attn + qhidx * head_dim;
        memset(o_subattn, 0, head_dim * elemsz);
        for (int64_t p = 0; p <= pos; p++) {
            uint16_t const* layer_vcache_pos = layer_vcache + p * kvdim;

            //vhead points to the v-head at pos-N of specific layer-M
            uint16_t const* vhead = layer_vcache_pos + kvhead_ofst;
            float w = tmplogits[p] * recip_sum;

            //vectorizable.
            //Compute the weighted sum of v.
            for (unsigned int d = 0; d < head_dim; d++) {
                float val = fp16_to_float(o_subattn[d]) +
                    w * fp16_to_float(vhead[d]);
                o_subattn[d] = float_to_fp16(val);
            }
        }
    }
    XASSERT(dump_vec(o_attn, n_qhead * head_dim, ctx->tensor_dtype,
        "gqa_f16 output,pos%u", pos));
    unlock_tmp3(ctx);
}


//tmpq: query buffer, shape=[1*n_embd]=[n_qhead * head_dim].
//  Temporary buffer, holds projected query after q_proj,
//  already in‑place transformed by RoPE.
//  Data becomes invalid and NOT stored into KV‑cache.
//kcache: kcache start at some layer.
//vcache: vcache start at some layer.
//n_qhead: number of qhead.
//n_kvhead: number of kvhead.
//kvdim: the total dim(number of element) after concatenating all kvhead.
void gqa_f32(
    float *restrict o_attn,
    float const*restrict tmpq, float const*restrict layer_kcache,
    float const*restrict layer_vcache,
    unsigned int n_qhead, unsigned int n_kvhead,
    unsigned int head_dim, unsigned int kvdim, int64_t pos, float eps,
    RTCtx *restrict ctx)
{
    XASSERT(dump("start gqa_f32,pos%u", pos));
    float * tmplogits = lock_tmp3(ctx);
    XASSERT(ctx->tmp3_len >= (pos+1)*sizeof(float));

    //GQA attention over keys 0..pos (causal)

    //Compute the Q-head number that is related to KV-head.
    //Multiple Q-heads are related to one KV-head.
    //e.g:
    //| qheadidx0 | qheadidx1 | qheadidx2 | qheadidx3 | qheadidx4 | qheadidx5 |
    //| kvhead0   | kvhead1   |
    //Thus
    //kvhead0 is related to (qheadidx0, qheadidx1, qheadidx2).
    //kvhead1 is related to (qheadidx3, qheadidx4, qheadidx5).

    unsigned int group_size = n_qhead / n_kvhead;
    XASSERT(n_qhead % n_kvhead == 0);
    float scale = 1.0f / sqrtf((float)head_dim);
    for (unsigned int qhidx = 0; qhidx < n_qhead; qhidx++) {
        float const* subqhead = tmpq + qhidx * head_dim;

        //Given q-head idx, compute the kv-head idx.
        //e.g:qhidx is 3, the related kvheadidx is 3/3 == 1.
        unsigned int kvheadidx = qhidx / group_size;

        //tmplogits s[p] = qhidx . kh[p] * scale, p = 0..pos
        float max_s = g_float_neg_infinity;
        unsigned int kvhead_ofst = kvheadidx * head_dim;
        for (int64_t p = 0; p <= pos; p++) {
            float const* layer_kcache_pos = layer_kcache + p * kvdim;

            //khead points to the k-head at pos-N of specific layer-M
            float const* khead = layer_kcache_pos + kvhead_ofst;

            //Compute dot-product of qhead and khead.
            float dot = 0.0f;
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot + subqhead[d] * khead[d];
            }
            float s = dot * scale;
            tmplogits[p] = s;
            if (s > max_s) {
                //Find max value for numerical stability.
                //Compute max here to make full use of data-locality.
                max_s = s;
            }
        }
        //Do softmax over [0,pos].
        float sum = 0.0f;
        for (int64_t p = 0; p <= pos; p++) {
            float val = tmplogits[p] - max_s;
            tmplogits[p] = expf(val);
            sum = sum + tmplogits[p];
        }
        XASSERT(sum >= 0.0f);
        sum = fmaxf(sum, eps);

        //Divide sum, get probability.
        float recip_sum = 1.0f / sum;

        //weighted sum of v.
        float * o_subattn = o_attn + qhidx * head_dim;
        memset(o_subattn, 0, head_dim * sizeof(float));
        for (int64_t p = 0; p <= pos; p++) {
            float const* layer_vcache_pos = layer_vcache + p * kvdim;

            //vhead points to the v-head at pos-N of specific layer-M
            float const* vhead = layer_vcache_pos + kvhead_ofst;
            float w = tmplogits[p] * recip_sum;

            //vectorizable.
            //Compute the weighted sum of v.
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                o_subattn[d] = o_subattn[d] + w * vhead[d];
            }
        }
    }
    XASSERT(dump_vec(o_attn, n_qhead * head_dim, ctx->tensor_dtype,
        "gqa_f32 output,pos%u", pos));
    unlock_tmp3(ctx);
}


void gqa(
    void *restrict o_attn,
    void const*restrict tmpq, void const*restrict layer_kcache,
    void const*restrict layer_vcache,
    unsigned int n_qhead, unsigned int n_kvhead,
    unsigned int head_dim, unsigned int kvdim,
    int64_t startpos, int64_t endpos, unsigned int elemsz,
    float eps, RTCtx *restrict ctx)
{
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    XASSERT(endpos <= ctx->max_pos);
    switch (ctx->tensor_dtype) {
    case RTTENSOR_DTYPE_F32: {
        BYTE * subtmpq = tmpq;
        BYTE * suboattn = o_attn;
        unsigned int qdimsz = n_qhead * head_dim * elemsz;
        for (int64_t pos = startpos; pos <= endpos;
             pos++, subtmpq += qdimsz, suboattn += qdimsz) {
            gqa_f32(
                (float*)suboattn, (float const*)subtmpq,
                layer_kcache, layer_vcache,
                n_qhead, n_kvhead, head_dim, kvdim, pos, eps, ctx);
        }
        return;
    }
    case RTTENSOR_DTYPE_F16: {
        BYTE * subtmpq = tmpq;
        BYTE * suboattn = o_attn;
        unsigned int qdimsz = n_qhead * head_dim * elemsz;
        for (int64_t pos = startpos; pos <= endpos;
             pos++, subtmpq += qdimsz, suboattn += qdimsz) {
            gqa_f16(
                (uint16_t*)suboattn, (uint16_t const*)subtmpq,
                layer_kcache, layer_vcache,
                n_qhead, n_kvhead, head_dim, kvdim, pos, eps, ctx);
        }
        return;
    }
    case RTTENSOR_DTYPE_BF16: {
        BYTE * subtmpq = tmpq;
        BYTE * suboattn = o_attn;
        unsigned int qdimsz = n_qhead * head_dim * elemsz;
        for (int64_t pos = startpos; pos <= endpos;
             pos++, subtmpq += qdimsz, suboattn += qdimsz) {
            gqa_bf16(
                (uint16_t*)suboattn, (uint16_t const*)subtmpq,
                layer_kcache, layer_vcache,
                n_qhead, n_kvhead, head_dim, kvdim, pos, eps, ctx);
        }
        return;
    }
    default:
        XASSERT(0); //Unsupport
        return;
    }
}


//MHA with RoPE and GQA.
//out:[seqlen*n_embd] attention output AFTER o_proj(ready for residual add).
//in:[seqlen*n_embd] normalized input hidden state.
//ctx:context, kcache|vcache|scratch buffers.
//startpos,endpos:token position, 0-based. KV cache written at 'pos'.
//layer: transformer layer index (which weights to use).
void mha_rope(
    void *restrict out, void const*restrict in,
    RTTensor *restrict q_proj_w, RTTensor *restrict q_proj_b,
    RTTensor *restrict k_proj_w, RTTensor *restrict k_proj_b,
    RTTensor *restrict v_proj_w, RTTensor *restrict v_proj_b,
    RTTensor *restrict o_proj_w,
    unsigned int layer, int64_t startpos, int64_t endpos, unsigned int n_embd,
    unsigned int n_head, RTCtx *restrict ctx)
{
    unsigned int n_qhead = ctx->n_head;
    unsigned int n_kvhead = ctx->n_kvhead;
    XASSERT(n_kvhead <= n_qhead);
    XASSERT(n_embd % n_head == 0);
    XASSERT(n_qhead % n_kvhead == 0);
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    int64_t seqlen = endpos - startpos + 1;

    //The head_dim of qhead has to be the same with kvhead.
    unsigned int head_dim = ctx->n_embd / ctx->n_head; //128
    float rope_theta = ctx->rope_theta;

    //The total dim after concatenating all qhead.
    XASSERT(n_qhead * head_dim == n_embd);
    unsigned int qdim = n_embd; //equals to n_embd

    //The total dim after concatenating all kvhead.
    unsigned int kvdim = n_kvhead * head_dim;
    XASSERT(kvdim <= qdim);

    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);

    //tmp_qkv start
    //== k (kvdim=1024 float) ====================================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //== v (kvdim=1024 float) ====================================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //== o_attn (n_embd=8192 float)===============================
    //| |qhead0[128] |qhead1[128] |.................|qhead63[128]||
    //== q (n_embd=8192 float)====================================
    //| |qhead0[128] |qhead1[128] |.................|qhead63[128]||
    //============================================================

    //tmp_seq_qkv start
    //== k (kvdim=1024 float) pos0 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //== k (kvdim=1024 float) pos1 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //== k (kvdim=1024 float) pos2 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //...
    //== k (kvdim=1024 float) posN ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |

    //== v (kvdim=1024 float) pos0 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //== v (kvdim=1024 float) pos1 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //== v (kvdim=1024 float) pos2 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //...
    //== v (kvdim=1024 float) posN ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |

    //== q (kvdim=1024 float) pos0 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //== q (kvdim=1024 float) pos1 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //== q (kvdim=1024 float) pos2 ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |
    //...
    //== q (kvdim=1024 float) posN ===============================
    //| |kvhead0[128]|kvhead1[128]|...|kvhead7[128]|              |

    //== o_attn (n_embd=8192 float)===============================
    //| |qhead0[128] |qhead1[128] |.................|qhead63[128]||

    BYTE * tmp_seq_qkv = lock_tmp_qkv(ctx);
    BYTE * q = tmp_seq_qkv;
    BYTE * k = q + seqlen * qdim * elemsz;
    BYTE * v = k + seqlen * kvdim * elemsz;
    BYTE * o_attn = v + seqlen * kvdim * elemsz;
    XASSERT(ctx->tmp_qkv_len >= seqlen * (qdim+2*kvdim+n_embd) * elemsz);

    //Scratch buffers used:
    //ctx->tmp1:o_proj, [1 * n_embd]
    //ctx->tmp_seq_qkv:
    //  q,      [n_qhead * head_dim]
    //  k,      [n_kvhead * head_dim]
    //  v,      [n_kvhead * head_dim]
    //  o_attn, [n_qhead * head_dim]
    //ctx->tmp_fc:scores [pos+1]

    //QKV projection (out = in @ W^T)
    XASSERT(ctx->q_proj_weight[layer]->shape[0] == qdim);
    XASSERT(ctx->q_proj_weight[layer]->shape[1] == n_embd);

    //q=[seqlen*n_embd]x[qdim*n_embd]
    //in=[seqlen*n_embd], q_proj_w=[qdim*n_embd], q_proj_b=[1*n_embd].
    matmulTransBias(q, in, seqlen, n_embd,
        ctx->q_proj_weight[layer], ctx->q_proj_bias[layer]);
    XASSERT(dump_mat(q, seqlen, n_embd, dtype,
        "q projection,output,pos%u~pos%u", startpos, endpos));

    //k=[seqlen*n_embd]x[kvdim*n_embd]
    //v=[seqlen*n_embd]x[kvdim*n_embd]
    //k_proj_w=[kvdim*n_embd], k_proj_b=[1*n_embd].
    //v_proj_w=[kvdim*n_embd], v_proj_b=[1*n_embd].
    //in=[seqlen*n_embd].
    XASSERT(ctx->k_proj_weight[layer]->shape[0] == kvdim);
    XASSERT(ctx->k_proj_weight[layer]->shape[1] == n_embd);
    XASSERT(ctx->v_proj_weight[layer]->shape[0] == kvdim);
    XASSERT(ctx->v_proj_weight[layer]->shape[1] == n_embd);
    matmulTransBias(k, in, seqlen, n_embd,
        ctx->k_proj_weight[layer], ctx->k_proj_bias[layer]);
    XASSERT(dump_mat(k, seqlen, n_embd, dtype,
        "k projection,output,pos%u~pos%u", startpos, endpos));

    matmulTransBias(v, in, seqlen, n_embd,
        ctx->v_proj_weight[layer], ctx->v_proj_bias[layer]);
    XASSERT(dump_mat(v, seqlen, n_embd, dtype,
        "v projection,output,pos%u~pos%u", startpos, endpos));

    rope_rotate_inplace(q, startpos, endpos, n_head, qdim, head_dim,
        rope_theta, dtype);
    XASSERT(dump_l2norm(q, seqlen, qdim, dtype,
        "q rotate,output,pos%u~pos%u", startpos, endpos));
    XASSERT(dump_mat(q, seqlen, qdim, dtype,
        "q rotate,output,pos%u~pos%u", startpos, endpos));

    rope_rotate_inplace(k, startpos, endpos, n_kvhead, kvdim, head_dim,
        rope_theta, dtype);
    XASSERT(dump_l2norm(k, seqlen, kvdim, dtype,
        "q rotate,output,pos%u~pos%u", startpos, endpos));
    XASSERT(dump_mat(k, seqlen, kvdim, dtype,
        "v rotate,output,pos%u~pos%u", startpos, endpos));

    //Write KV cache at position pos.
    storeKCacheAndVCache(k, v, layer, elemsz, startpos, endpos, ctx);
    XASSERT(dumpKVCacheLayer(layer, startpos, endpos, ctx));

    BYTE * kcache = lock_kcache(ctx);
    BYTE * vcache = lock_vcache(ctx);

    //Each token occupies kvdim = n_kvhead * head_dim floats.
    int64_t elem_num_per_layer_kv = ctx->max_pos * kvdim;
    BYTE * layer_k_cache = kcache + layer * elem_num_per_layer_kv * elemsz;
    BYTE * layer_v_cache = vcache + layer * elem_num_per_layer_kv * elemsz;

    //o_attn[seqlen*n_embd]=gqa(q[seqlen*n_embd])
    gqa(o_attn, q, layer_k_cache, layer_v_cache,
        n_qhead, n_kvhead, head_dim, kvdim, startpos, endpos, elemsz,
        ctx->softmax_epsilon, ctx);
    XASSERT(dump_l2norm(o_attn, seqlen, n_embd, dtype,
        "gqa output,pos%u~pos%u", startpos, endpos));

    //Output projection:out[seqlen*n_embd] = o_attn*o_proj^T
    //o_attn=[seqlen*n_embd], o_proj_weight=[n_embd*n_embd].
    XASSERT(ctx->o_proj_weight[layer]->shape[0] == n_embd);
    XASSERT(ctx->o_proj_weight[layer]->shape[1] == qdim);
    matmulTrans(out, o_attn, seqlen, n_embd, ctx->o_proj_weight[layer]);
    XASSERT(dump_mat(out, seqlen, n_embd, dtype,
        "mha_rope,output,pos%u~pos%u", startpos, endpos));

    unlock_tmp_qkv(ctx);
    unlock_kcache(ctx);
    unlock_vcache(ctx);
}
