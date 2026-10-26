#include "ops.h"

void scaled_dot_attn_decode_bf16(
    uint16_t * out, uint16_t const*restrict q,
    uint16_t const*restrict k_cache, uint16_t const*restrict v_cache,
    int64_t pos, unsigned int n_head, unsigned int head_dim, int64_t max_pos,
    float eps)
{
    XASSERT(out);
    XASSERT(q);
    XASSERT(k_cache);
    XASSERT(v_cache);
    XASSERT(pos >= 0);
    XASSERT(pos < max_pos);
    XASSERT(n_head > 0);
    XASSERT(head_dim > 0);
    float scale = 1.0f / sqrtf((float)head_dim);
    int64_t seq_len = pos + 1; //Effect number of token:0-pos

    //Each head requires temporary storage for attn_score [seq_len]
    //NOTE:Do not malloc if the seq_len is not very large, use small
    //stack allocated buffers.
    //Under decode phase, seq_len is at most max_pos. Stack is unsuitable
    //for overly large max_pos values.
    float * attn_score;
    float tmp[MAX_ELEMNUM_ON_STACK];
    unsigned int elemsz = sizeof(uint16_t);

    //Element size of the hoisted precision type, which is only used inside
    //computation.
    unsigned int const intrasz = sizeof(float);
    if (seq_len > MAX_ELEMNUM_ON_STACK) {
        attn_score = malloc(seq_len * intrasz);
        XASSERT(dump("malloc %u bytes memory,seq_len", seq_len * intrasz));
    } else { attn_score = tmp; }
    unsigned int n_embd =  n_head * head_dim;

    //Iterate each attention-head.
    for (unsigned int h = 0; h < n_head; h++) {
        unsigned int off_in_embd = h * head_dim;

        //q_head:pointer of query of current head.
        uint16_t const* q_head = q + off_in_embd;

        //Q*K^T/scale, compute the score of each history positions in sequence.
        //NOTE:k_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, need stride version.
        for (int64_t t = 0; t < seq_len; t++) {
            uint16_t const* k_t_head = k_cache + t * n_embd + off_in_embd;
            float dot = 0.0f;
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot +
                    bf16_to_float(q_head[d]) * bf16_to_float(k_t_head[d]);
            }
            attn_score[t] = dot * scale;
        }
        softmax_inplace_f32(attn_score, seq_len, eps);

        //attn_score*V and get the output of the head.
        //NOTE:v_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul,  eed stride version.
        uint16_t * out_head = out + off_in_embd;
        memset(out_head, 0, head_dim * elemsz);
        for (int64_t t = 0; t < seq_len; t++) {
            uint16_t const* v_t_head = v_cache + t * n_embd + off_in_embd;
            float s = attn_score[t];
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                float f32val = bf16_to_float(out_head[d]);
                f32val = f32val + s * bf16_to_float(v_t_head[d]);
                out_head[d] = float_to_bf16(f32val);
            }
        }
    }
    if (seq_len > MAX_ELEMNUM_ON_STACK) {
        free(attn_score);
        XASSERT(dump("free %u bytes memory", seq_len * intrasz));
    }
}


void scaled_dot_attn_decode_f16(
    uint16_t * out, uint16_t const*restrict q,
    uint16_t const*restrict k_cache, uint16_t const*restrict v_cache,
    int64_t pos, unsigned int n_head, unsigned int head_dim, int64_t max_pos,
    float eps)
{
    XASSERT(out);
    XASSERT(q);
    XASSERT(k_cache);
    XASSERT(v_cache);
    XASSERT(pos >= 0);
    XASSERT(pos < max_pos);
    XASSERT(n_head > 0);
    XASSERT(head_dim > 0);
    float scale = 1.0f / sqrtf((float)head_dim);
    int64_t seq_len = pos + 1; //Effect number of token:0-pos

    //Each head requires temporary storage for attn_score [seq_len]
    //NOTE:Do not malloc if the seq_len is not very large, use small
    //stack allocated buffers.
    //Under decode phase, seq_len is at most max_pos. Stack is unsuitable
    //for overly large max_pos values.
    float * attn_score;
    float tmp[MAX_ELEMNUM_ON_STACK];
    unsigned int elemsz = sizeof(uint16_t);

    //Element size of the hoisted precision type, which is only used inside
    //computation.
    unsigned int const intrasz = sizeof(float);
    if (seq_len > MAX_ELEMNUM_ON_STACK) {
        attn_score = malloc(seq_len * intrasz);
        XASSERT(dump("malloc %u bytes memory,seq_len", seq_len * intrasz));
    } else { attn_score = tmp; }
    unsigned int n_embd =  n_head * head_dim;

    //Iterate each attention-head.
    for (unsigned int h = 0; h < n_head; h++) {
        unsigned int off_in_embd = h * head_dim;

        //q_head:pointer of query of current head.
        uint16_t const* q_head = q + off_in_embd;

        //Q*K^T/scale, compute the score of each history positions in sequence.
        //NOTE:k_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, need stride version.
        for (int64_t t = 0; t < seq_len; t++) {
            uint16_t const* k_t_head = k_cache + t * n_embd + off_in_embd;
            float dot = 0.0f;
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot +
                    fp16_to_float(q_head[d]) * fp16_to_float(k_t_head[d]);
            }
            attn_score[t] = dot * scale;
        }
        softmax_inplace_f32(attn_score, seq_len, eps);

        //attn_score*V and get the output of the head.
        //NOTE:v_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul,  eed stride version.
        uint16_t * out_head = out + off_in_embd;
        memset(out_head, 0, head_dim * elemsz);
        for (int64_t t = 0; t < seq_len; t++) {
            uint16_t const* v_t_head = v_cache + t * n_embd + off_in_embd;
            float s = attn_score[t];
            for (unsigned int d = 0; d < head_dim; d++) {
                float f32val = fp16_to_float(out_head[d]);
                f32val = f32val + s * fp16_to_float(v_t_head[d]);
                out_head[d] = float_to_fp16(f32val);
            }
        }
    }
    if (seq_len > MAX_ELEMNUM_ON_STACK) {
        free(attn_score);
        XASSERT(dump("free %u bytes memory", seq_len * intrasz));
    }
}


void scaled_dot_attn_decode_f32(
    float * out, float const*restrict q,
    float const*restrict k_cache, float const*restrict v_cache,
    int64_t pos, unsigned int n_head, unsigned int head_dim, int64_t max_pos,
    float eps)
{
    XASSERT(out);
    XASSERT(q);
    XASSERT(k_cache);
    XASSERT(v_cache);
    XASSERT(pos >= 0);
    XASSERT(pos < max_pos);
    XASSERT(n_head > 0);
    XASSERT(head_dim > 0);
    float scale = 1.0f / sqrtf((float)head_dim);
    int64_t seq_len = pos + 1; //Effect number of token:0-pos

    //Each head requires temporary storage for attn_score [seq_len]
    //NOTE:Do not malloc if the seq_len is not very large, use small
    //stack allocated buffers.
    //Under decode phase, seq_len is at most max_pos. Stack is unsuitable
    //for overly large max_pos values.
    float * attn_score;
    float tmp[MAX_ELEMNUM_ON_STACK];
    if (seq_len > MAX_ELEMNUM_ON_STACK) {
        attn_score = malloc(seq_len * sizeof(float));
        XASSERT(dump("malloc %u bytes memory,seq_len",
            seq_len * sizeof(float)));
    } else { attn_score = tmp; }
    unsigned int n_embd =  n_head * head_dim;

    //Iterate each attention-head.
    for (unsigned int h = 0; h < n_head; h++) {
        unsigned int off_in_embd = h * head_dim;

        //q_head:pointer of query of current head.
        float const* q_head = q + off_in_embd;

        //Q*K^T/scale, compute the score of each history positions in sequence.
        //NOTE:k_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul, need stride version.
        for (int64_t t = 0; t < seq_len; t++) {
            float const* k_t_head = k_cache + t * n_embd + off_in_embd;
            float dot = 0.0f;
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                dot = dot + q_head[d] * k_t_head[d];
            }
            attn_score[t] = dot * scale;
        }
        softmax_inplace_f32(attn_score, seq_len, eps);

        //attn_score*V and get the output of the head.
        //NOTE:v_t_head layout is not contiguous in memory, thus cannot
        //use dense matmul,  eed stride version.
        float * out_head = out + off_in_embd;
        memset(out_head, 0, head_dim * sizeof(float));
        for (int64_t t = 0; t < seq_len; t++) {
            float const* v_t_head = v_cache + t * n_embd + off_in_embd;
            float s = attn_score[t];
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                out_head[d] = out_head[d] + s * v_t_head[d];
            }
        }
    }
    if (seq_len > MAX_ELEMNUM_ON_STACK) {
        free(attn_score);
        XASSERT(dump("free %u bytes memory", seq_len * sizeof(float)));
    }
}


void scaled_dot_attn_decode(
    void * out, void const*restrict q,
    void const*restrict k_cache, void const*restrict v_cache,
    int64_t pos, unsigned int n_head, unsigned int head_dim, int64_t max_pos,
    float eps, RTTENSOR_DTYPE dtype)
{
    XASSERT(out);
    XASSERT(q);
    XASSERT(k_cache);
    XASSERT(v_cache);
    XASSERT(pos >= 0);
    XASSERT(pos < max_pos);
    XASSERT(n_head > 0);
    XASSERT(head_dim > 0);
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        scaled_dot_attn_decode_f32(
            out, q, k_cache, v_cache, pos, n_head, head_dim, max_pos, eps);
        return;
    case RTTENSOR_DTYPE_F16:
        scaled_dot_attn_decode_f16(
            out, q, k_cache, v_cache, pos, n_head, head_dim, max_pos, eps);
        return;
    case RTTENSOR_DTYPE_BF16:
        scaled_dot_attn_decode_bf16(
            out, q, k_cache, v_cache, pos, n_head, head_dim, max_pos, eps);
        return;
    default:
        XASSERT(0); //Unsupport
        return;
    }
}


void mha(void *restrict out, void const*restrict in,
         RTTensor *restrict c_attn_w, RTTensor *restrict c_attn_b,
         RTTensor *restrict c_proj_w, RTTensor *restrict c_proj_b,
         unsigned int layer, int64_t pos, unsigned int n_embd,
         unsigned int n_head, RTCtx *restrict ctx)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(c_attn_w);
    XASSERT(c_attn_b);
    XASSERT(c_proj_w);
    XASSERT(c_proj_b);
    XASSERT(ctx);

    BYTE * tmp_qkv = lock_tmp_qkv(ctx);
    BYTE * tmp_attn_out = lock_tmp_attn_out(ctx);
    XASSERT(tmp_qkv && tmp_attn_out);
    XASSERT(n_embd > 0);
    XASSERT(n_head > 0);
    XASSERT((n_embd % n_head) == 0);

    unsigned int head_dim = n_embd / n_head;
    unsigned int qkv_dim = 3 * n_embd;

    XASSERT(pos >= 0);
    XASSERT(pos < ctx->max_pos);

    XASSERT(c_attn_w->data);
    XASSERT(c_attn_w->ndim == 2);
    XASSERT(c_attn_w->elem_num == (int64_t)n_embd * qkv_dim);
    XASSERT(c_attn_w->shape[0] == n_embd);
    XASSERT(c_attn_w->shape[1] == qkv_dim);

    XASSERT(c_attn_b->data);
    XASSERT(c_attn_b->ndim == 1);
    XASSERT(c_attn_b->shape[0] == qkv_dim);
    XASSERT(c_attn_b->elem_num == qkv_dim);

    XASSERT(c_proj_w->data);
    XASSERT(c_proj_w->elem_num == n_embd * n_embd);
    XASSERT(c_proj_w->ndim == 2);
    XASSERT(c_proj_w->shape[0] == n_embd);
    XASSERT(c_proj_w->shape[1] == n_embd);

    XASSERT(c_proj_b->data);
    XASSERT(c_proj_b->ndim == 1);
    XASSERT(c_proj_b->shape[0] == n_embd);
    XASSERT(c_proj_b->elem_num == n_embd);

    //in is [1*n_embd], c_attn_w is [n_embd*qkv_dim], c_attn_b is [1*qkv_dim].
    //QKV projection: in[1*n_embd]*W+bias ---> tmp_qkv[qkv_dim].
    matmulBias(tmp_qkv, in, c_attn_w, c_attn_b);
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    XASSERT(elemsz != 0);

    //Split q,k,v.
    BYTE const* q = tmp_qkv;
    BYTE const* k = tmp_qkv + n_embd * elemsz;
    BYTE const* v = tmp_qkv + 2 * n_embd * elemsz;

    //Point to layer of pos.
    XASSERT(n_embd == n_head * head_dim);

    //Write KV cache at position pos.
    //KV cache layout:[max_pos*n_embd]
    int64_t elem_num_per_layer_kv = ctx->max_pos * n_embd;
    BYTE * kcache = lock_kcache(ctx);
    BYTE * vcache = lock_vcache(ctx);
    BYTE * layer_k_cache = kcache + layer * elem_num_per_layer_kv * elemsz;
    BYTE * layer_v_cache = vcache + layer * elem_num_per_layer_kv * elemsz;

    //Store k,v of layer to cache.
    int64_t pos_offset = pos * n_embd * elemsz;
    memcpy(layer_k_cache + pos_offset, k, n_embd * elemsz);
    memcpy(layer_v_cache + pos_offset, v, n_embd * elemsz);

    scaled_dot_attn_decode(
        tmp_attn_out, q, layer_k_cache, layer_v_cache,
        pos, n_head, head_dim, ctx->max_pos, ctx->layernorm_epsilon, dtype);

    //output projection
    //tmp_attn_out is [1*n_embd], out is [1*n_embd].
    //[1*n_embd] = [1*n_embd] x [n_embd*n_embd] + [1*n_embd].
    matmulBias(out, tmp_attn_out, c_proj_w, c_proj_b);
    unlock_tmp_qkv(ctx);
    unlock_tmp_attn_out(ctx);
    unlock_kcache(ctx);
    unlock_vcache(ctx);
}


//RoPE for one head, in place. Rotate each adjacent pair (2i, 2i+1):
//Treat adjacent pair (x[2t], x[2t+1]) as complex number, apply rotation
//angle theta_t
//  theta_i = pos / rope_theta^(2i / head_dim)
//  out[2i] = x[2i] * cos - x[2i+1] * sin
//  out[2i+1] = x[2i] * sin + x[2i+1] * cos
//TODO:compute trigonometric function via a lookup-table.
static void rope_rotate_inplace_f32(
    float * x, int64_t pos, unsigned int head_dim, float rope_theta)
{
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


static void rope_rotate_inplace_f16(
    uint16_t * x, int64_t pos, unsigned int head_dim, float rope_theta)
{
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


static void rope_rotate_inplace_bf16(
    uint16_t * x, int64_t pos, unsigned int head_dim, float rope_theta)
{
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


static void rope_rotate_inplace(
    void * x, int64_t pos, unsigned int head_dim, float rope_theta,
    RTTENSOR_DTYPE dtype)
{
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        rope_rotate_inplace_f32(x, pos, head_dim, rope_theta);
        return;
    case RTTENSOR_DTYPE_F16:
        rope_rotate_inplace_f16(x, pos, head_dim, rope_theta);
        return;
    case RTTENSOR_DTYPE_BF16:
        rope_rotate_inplace_bf16(x, pos, head_dim, rope_theta);
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
    float * tmplogits = lock_tmp3(ctx);
    XASSERT(ctx->tmp3_len >= (pos+1)*sizeof(float));
    unsigned int elemsz = sizeof(uint16_t);

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
        uint16_t const* tmp_qhead_buf = tmpq + qhidx * head_dim;

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
                    bf16_to_float(tmp_qhead_buf[d]) * bf16_to_float(khead[d]);
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
        uint16_t * o_vec = o_attn + qhidx * head_dim;
        memset(o_vec, 0, head_dim * elemsz);
        for (int64_t p = 0; p <= pos; p++) {
            uint16_t const* layer_vcache_pos = layer_vcache + p * kvdim;

            //vhead points to the v-head at pos-N of specific layer-M
            uint16_t const* vhead = layer_vcache_pos + kvhead_ofst;
            float w = tmplogits[p] * recip_sum;

            //vectorizable.
            //Compute the weighted sum of v.
            for (unsigned int d = 0; d < head_dim; d++) {
                float val = bf16_to_float(o_vec[d]) +
                    w * bf16_to_float(vhead[d]);
                o_vec[d] = float_to_bf16(val);
            }
        }
    }
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
        uint16_t const* tmp_qhead_buf = tmpq + qhidx * head_dim;

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
                    fp16_to_float(tmp_qhead_buf[d]) * fp16_to_float(khead[d]);
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
        uint16_t * o_vec = o_attn + qhidx * head_dim;
        memset(o_vec, 0, head_dim * elemsz);
        for (int64_t p = 0; p <= pos; p++) {
            uint16_t const* layer_vcache_pos = layer_vcache + p * kvdim;

            //vhead points to the v-head at pos-N of specific layer-M
            uint16_t const* vhead = layer_vcache_pos + kvhead_ofst;
            float w = tmplogits[p] * recip_sum;

            //vectorizable.
            //Compute the weighted sum of v.
            for (unsigned int d = 0; d < head_dim; d++) {
                float val = fp16_to_float(o_vec[d]) +
                    w * fp16_to_float(vhead[d]);
                o_vec[d] = float_to_fp16(val);
            }
        }
    }
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
        float const* tmp_qhead_buf = tmpq + qhidx * head_dim;

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
                dot = dot + tmp_qhead_buf[d] * khead[d];
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
        float * o_vec = o_attn + qhidx * head_dim;
        memset(o_vec, 0, head_dim * sizeof(float));
        for (int64_t p = 0; p <= pos; p++) {
            float const* layer_vcache_pos = layer_vcache + p * kvdim;

            //vhead points to the v-head at pos-N of specific layer-M
            float const* vhead = layer_vcache_pos + kvhead_ofst;
            float w = tmplogits[p] * recip_sum;

            //vectorizable.
            //Compute the weighted sum of v.
            HINT_VECT
            for (unsigned int d = 0; d < head_dim; d++) {
                o_vec[d] = o_vec[d] + w * vhead[d];
            }
        }
    }
    unlock_tmp3(ctx);
}


void gqa(
    void *restrict o_attn,
    void const*restrict tmpq, void const*restrict layer_kcache,
    void const*restrict layer_vcache,
    unsigned int n_qhead, unsigned int n_kvhead,
    unsigned int head_dim, unsigned int kvdim, int64_t pos, float eps,
    RTCtx *restrict ctx)
{
    switch (ctx->tensor_dtype) {
    case RTTENSOR_DTYPE_F32:
        gqa_f32(
            o_attn, tmpq, layer_kcache, layer_vcache, n_qhead, n_kvhead,
            head_dim, kvdim, pos, eps, ctx);
        return;
    case RTTENSOR_DTYPE_F16:
        gqa_f16(
            o_attn, tmpq, layer_kcache, layer_vcache, n_qhead, n_kvhead,
            head_dim, kvdim, pos, eps, ctx);
        return;
    case RTTENSOR_DTYPE_BF16:
        gqa_bf16(
            o_attn, tmpq, layer_kcache, layer_vcache, n_qhead, n_kvhead,
            head_dim, kvdim, pos, eps, ctx);
        return;
    default:
        XASSERT(0); //Unsupport
        return;
    }
}


//MHA with RoPE and GQA.
//out:[n_embd] attention output AFTER o_proj(ready for residual add).
//in:[n_embd] normalized input hidden state.
//ctx:context, kcache|vcache|scratch buffers.
//pos:current token position, 0-based. KV cache written at 'pos'.
//layer: transformer layer index (which weights to use).
void mha_rope(
    void *restrict out, void const*restrict in,
    RTTensor *restrict q_proj_w, RTTensor *restrict q_proj_b,
    RTTensor *restrict k_proj_w, RTTensor *restrict k_proj_b,
    RTTensor *restrict v_proj_w, RTTensor *restrict v_proj_b,
    RTTensor *restrict o_proj_w,
    unsigned int layer, int64_t pos, unsigned int n_embd,
    unsigned int n_head, RTCtx *restrict ctx)
{
    unsigned int n_qhead = ctx->n_head;
    unsigned int n_kvhead = ctx->n_kvhead;
    XASSERT(n_kvhead <= n_qhead);
    XASSERT(n_embd % n_head == 0);
    XASSERT(n_qhead % n_kvhead == 0);

    //The head_dim of qhead has to be the same with kvhead.
    unsigned int head_dim = ctx->n_embd / ctx->n_head; //128
    float rope_theta = ctx->rope_theta;
    XASSERT(n_qhead * head_dim == n_embd);

    //The total dim after concatenating all qhead.
    unsigned int qdim = n_embd; //equals to n_embd

    //The total dim after concatenating all kvhead.
    unsigned int kvdim = n_kvhead * head_dim;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);

    XASSERT(kvdim <= qdim);

    //tmp_qkv start
    //------------------------------------------------------------
    //| k (kvdim=1024 float)                                      |
    //| ----------------------------- ... --------------          |
    //| |kvhead0[128]|kvhead1[128]| ...  |kvhead7[128]|           |
    //| ----------------------------- ... --------------          |
    //|-----------------------------------------------------------
    //| v (kvdim=1024 float)                                      |
    //| ----------------------------- ... --------------          |
    //| |kvhead0[128]|kvhead1[128]| ...  |kvhead7[128]|           |
    //| ----------------------------- ... --------------          |
    //|-----------------------------------------------------------
    //| o_attn (n_embd=8192 float)                                |
    //| ----------------------------- ... --------------          |
    //| |qhead0[128] |qhead1[128] | ...  |qhead63[128]|           |
    //| ----------------------------- ... --------------          |
    //|-----------------------------------------------------------
    //| q (n_embd=8192 float)                                     |
    //| ----------------------------- ... --------------          |
    //| |qhead0[128] |qhead1[128] | ...  |qhead63[128]|           |
    //| ----------------------------- ... --------------          |
    //------------------------------------------------------------

    BYTE * tmp_qkv = lock_tmp_qkv(ctx);
    BYTE * k = tmp_qkv;
    BYTE * v = tmp_qkv + kvdim * elemsz;

    //q occupies n_embd elements.
    BYTE * q = tmp_qkv + 2 * kvdim * elemsz;

    //o_attn occupies n_embd elements.
    BYTE * o_attn = tmp_qkv + (2 * kvdim + n_embd) * elemsz;

    //Scratch buffers used:
    //ctx->tmp1   :o_proj, [1 * n_embd]
    //ctx->tmp_qkv:k,      [n_kvhead * head_dim]
    //            +v,      [n_kvhead * head_dim]
    //            +o_attn, [n_qhead * head_dim]
    //            +q,      [n_qhead * head_dim]
    //ctx->tmp_fc :scores [pos+1]

    //QKV projection (out = in @ W^T)
    //q=[1*n_embd]x[qdim*n_embd]
    XASSERT(ctx->q_proj_weight[layer]->shape[0] == qdim);
    XASSERT(ctx->q_proj_weight[layer]->shape[1] == n_embd);
    matmulTransBias(q, in, ctx->q_proj_weight[layer], ctx->q_proj_bias[layer]);

    //k=[1*n_embd]x[kvdim*n_embd]
    XASSERT(ctx->k_proj_weight[layer]->shape[0] == kvdim);
    XASSERT(ctx->k_proj_weight[layer]->shape[1] == n_embd);
    matmulTransBias(k, in, ctx->k_proj_weight[layer], ctx->k_proj_bias[layer]);

    //v=[1*n_embd]x[kvdim*n_embd]
    matmulTransBias(v, in, ctx->v_proj_weight[layer], ctx->v_proj_bias[layer]);

    //RoPE on q and k (NOT v)
    for (unsigned int qhidx = 0; qhidx < n_qhead; qhidx++) {
        rope_rotate_inplace(
            q + qhidx * head_dim * elemsz, pos, head_dim, rope_theta, dtype);
    }
    for (unsigned int kvh = 0; kvh < n_kvhead; kvh++) {
        rope_rotate_inplace(
            k + kvh * head_dim * elemsz, pos, head_dim, rope_theta, dtype);
    }

    //Write KV cache at position pos.
    //KV cache layout:[max_pos, n_kvhead*head_dim]=[max_pos*kvdim]
    int64_t elem_num_per_layer_kv = ctx->max_pos * kvdim;
    BYTE * kcache = lock_kcache(ctx);
    BYTE * vcache = lock_vcache(ctx);

    //-------------------------------------------------------------------
    //layer_k_cache base(ONLY K data, shape[max_pos, n_kvhead, head_dim])
    //that n_kvhead=8,head_dim=128
    //------------------------------------
    //pos0
    //k_head0[0..127] ... k_head7[0..127]
    //------------------------------------
    //pos1
    //k_head0[0..127] ... k_head7[0..127]
    //------------------------------------
    //...
    //------------------------------------
    //pos new token
    //k_head0[0..127] ... k_head7[0..127]
    //------------------------------------

    //-------------------------------------------------------------------
    //layer_v_cache base(ONLY V data, shape[max_pos, n_kvhead, head_dim])
    //------------------------------------
    //pos0
    //v_head0[0..127] ... v_head7[0..127]
    //------------------------------------
    //pos1
    //v_head0[0..127] ... v_head7[0..127]
    //------------------------------------
    //...
    //------------------------------------
    //pos new token
    //v_head0[0..127] ... v_head7[0..127]
    //------------------------------------
    //Each token occupies kvdim = n_kvhead * head_dim floats.
    BYTE * layer_k_cache = kcache + layer * elem_num_per_layer_kv * elemsz;
    BYTE * layer_v_cache = vcache + layer * elem_num_per_layer_kv * elemsz;
    BYTE * kc = layer_k_cache + pos * kvdim * elemsz;
    BYTE * vc = layer_v_cache + pos * kvdim * elemsz;
    memcpy(kc, k, kvdim * elemsz);
    memcpy(vc, v, kvdim * elemsz);

    //o_attn[1*n_embd]=gqa(q[1*n_embd])
    gqa(o_attn, q, layer_k_cache, layer_v_cache,
        n_qhead, n_kvhead, head_dim, kvdim, pos, ctx->softmax_epsilon, ctx);
    XASSERT(dump_l2norm(o_attn, n_embd, dtype, "gqa output"));

    //Output projection:out[1*n_embd] = o_attn*o_proj^T
    //o_proj_weight is [n_embd*n_embd]
    XASSERT(ctx->o_proj_weight[layer]->shape[0] == n_embd);
    XASSERT(ctx->o_proj_weight[layer]->shape[1] == qdim);
    matmulTrans(out, o_attn, ctx->o_proj_weight[layer]);

    unlock_tmp_qkv(ctx);
    unlock_kcache(ctx);
    unlock_vcache(ctx);
}
