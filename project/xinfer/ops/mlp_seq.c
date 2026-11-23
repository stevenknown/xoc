#include "ops.h"

void mlp_seq(
    void * out, void const*restrict in,
    RTTensor const*restrict c_fc_w, RTTensor const*restrict c_fc_b,
    RTTensor const*restrict c_proj_w, RTTensor const*restrict c_proj_b,
    int64_t seqlen, RTCtx * ctx)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(c_fc_w);
    XASSERT(c_fc_b);
    XASSERT(c_proj_w);
    XASSERT(c_proj_b);
    XASSERT(ctx);

    //Model embedding dimension size. Inner hidden dim = 4 * n_embd.
    unsigned int n_embd = ctx->n_embd;
    XASSERT(n_embd > 0);
    unsigned int inner_dim = ctx->intermediate_size;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);

    //tmp_fc is used to store 4*n_embd. It shares memory with MHA's tmp_qkv.
    void * tmp_fc = lock_tmp_fc(ctx);
    XASSERT(tmp_fc);
    XASSERT(ctx->tmp_fc_len >= seqlen * inner_dim * elemsz);
    XASSERT(c_fc_w->data);
    XASSERT(c_fc_b->data);
    XASSERT(c_proj_w->data);
    XASSERT(c_proj_b->data);
    XASSERT(c_fc_w->elem_num == n_embd * inner_dim);
    XASSERT(c_fc_b->elem_num == inner_dim);
    XASSERT(c_proj_w->elem_num == inner_dim * n_embd);
    XASSERT(c_proj_b->elem_num == n_embd);

    //FC increases a dimension.
    //in[n_embd] to tmp_fc[4*n_embd].
    XASSERT(c_fc_w->ndim == 2);
    XASSERT(c_fc_w->shape[0] == n_embd);
    XASSERT(c_fc_w->shape[1] == inner_dim);

    XASSERT(c_fc_b->ndim == 1);
    XASSERT(c_fc_b->shape[0] == inner_dim);

    //tmp_fc is [seqlen*inner_dim].
    //in is [seqlen*n_embd].
    //[seqlen*inner_dim] = [seqlen*n_embd] x [n_embd*inner_dim] + [1*inner_dim]
    matmulBias(tmp_fc, in, seqlen, n_embd, c_fc_w, c_fc_b);
    XASSERT(dump_l2norm(tmp_fc, seqlen, inner_dim, dtype,
        "after increase dim by FC to %u*%u", seqlen, inner_dim));

    //tmp_fc is activated by GELU in-place.
    gelu(tmp_fc, seqlen * inner_dim, dtype);
    XASSERT(dump_l2norm(tmp_fc, seqlen, inner_dim, dtype,
        "gelu active output:%u*%u", seqlen, inner_dim));

    //Decrease (project) a dimension.
    //tmp_fc[4*n_embd] to out[n_embd].
    XASSERT(c_proj_w->ndim == 2);
    XASSERT(c_proj_w->shape[0] == inner_dim);
    XASSERT(c_proj_w->shape[1] == n_embd);
    XASSERT(c_proj_b->ndim == 1);
    XASSERT(c_proj_b->shape[0] == n_embd);

    //tmp_fc is [seqlen*inner_dim], w_proj is [inner_dim*n_embd].
    //out is [seqlen*n_embd], b_proj is [1*n_embd].
    //[seqlen*n_embd] = [seqlen*inner_dim] x [inner_dim*n_embd] + [1*n_embd]
    matmulBias(out, tmp_fc, seqlen, inner_dim, c_proj_w, c_proj_b);
    XASSERT(dump_l2norm(out, seqlen, n_embd, dtype,
        "after decrease dim by FC to %u*%u", seqlen, n_embd));
    unlock_tmp_fc(ctx);
}
