#include "ops.h"

void mlp(void * out, void const*restrict in,
         RTTensor const*restrict c_fc_w, RTTensor const*restrict c_fc_b,
         RTTensor const*restrict c_proj_w, RTTensor const*restrict c_proj_b,
         RTCtx * ctx)
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
    XASSERT(ctx->tmp_fc_len >= inner_dim * elemsz);
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

    //tmp_fc is [1*inner_dim].
    //in is [1*n_embd]
    //[1*inner_dim] = [1*n_embd] x [n_embd*inner_dim] + [1*inner_dim]
    matmulBias(tmp_fc, in, c_fc_w, c_fc_b);
    XASSERT(dump_l2norm(tmp_fc, inner_dim, dtype,
            "after increase dim by FC to 1*%u", inner_dim));

    //tmp_fc is activated by GELU in-place.
    gelu(tmp_fc, inner_dim, dtype);
    XASSERT(dump_l2norm(tmp_fc, inner_dim, dtype, "gelu active output"));

    //Decrease (project) a dimension.
    //tmp_fc[4*n_embd] to out[n_embd].
    XASSERT(c_proj_w->ndim == 2);
    XASSERT(c_proj_w->shape[0] == inner_dim);
    XASSERT(c_proj_w->shape[1] == n_embd);

    XASSERT(c_proj_b->ndim == 1);
    XASSERT(c_proj_b->shape[0] == n_embd);

    //tmp_fc is [1*inner_dim], w_proj is [inner_dim*n_embd].
    //out is [1*n_embd], b_proj is [1*n_embd].
    //[1*n_embd] = [1*inner_dim] x [inner_dim*n_embd] + [1*n_embd]
    matmulBias(out, tmp_fc, c_proj_w, c_proj_b);
    XASSERT(dump_l2norm(out, n_embd, dtype,
            "after decrease dim by FC to 1*%u", n_embd));
    unlock_tmp_fc(ctx);
}


void mlp_swigu(
    void * out, void const*restrict in,
    RTTensor const*restrict gate_proj_w, RTTensor const*restrict up_proj_w,
    RTTensor const*restrict down_proj_w, RTCtx * ctx)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(gate_proj_w);
    XASSERT(up_proj_w);
    XASSERT(down_proj_w);
    XASSERT(ctx);
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);

    //Model embedding dimension size. Inner hidden dim = 4 * n_embd.
    unsigned int n_embd = ctx->n_embd;
    XASSERT(n_embd > 0);
    unsigned int inner_dim = ctx->intermediate_size;

    //tmp_fc overlaps with tmp_qkv.
    BYTE * tmp_buffer_start = lock_tmp_fc(ctx);
    XASSERT(tmp_buffer_start);
    XASSERT(ctx->tmp_fc_len >= 2 * inner_dim * elemsz);
    BYTE * tmp_gate = tmp_buffer_start;
    BYTE * tmp_up = tmp_buffer_start + inner_dim * elemsz;
    XASSERT(gate_proj_w->data);
    XASSERT(up_proj_w->data);
    XASSERT(down_proj_w->data);

    //gate = in*gate_proj_w^T
    //[1*inner_dim]=[1*n_embd]*[inner_dim*n_embd]^T
    XASSERT(gate_proj_w->ndim == 2);
    XASSERT(gate_proj_w->shape[0] == inner_dim);
    XASSERT(gate_proj_w->shape[1] == n_embd);
    XASSERT(gate_proj_w->elem_num == inner_dim * n_embd);
    matmulTrans(tmp_gate, in, gate_proj_w);
    XASSERT(dump_l2norm(tmp_gate, inner_dim, dtype,
            "after gate proj to 1*%u", inner_dim));

    //up = in*up_proj_w^T
    //[1*inner_dim]=[1*n_embd]*[inner_dim*n_embd]^T
    XASSERT(up_proj_w->ndim == 2);
    XASSERT(up_proj_w->shape[0] == inner_dim);
    XASSERT(up_proj_w->shape[1] == n_embd);
    XASSERT(up_proj_w->elem_num == inner_dim * n_embd);
    matmulTrans(tmp_up, in, up_proj_w);
    XASSERT(dump_l2norm(tmp_up, inner_dim, dtype,
            "after up proj to 1*%u", inner_dim));

    //Apply silu activation to gate: silu(x)=x*sigmoid(x)
    silu(tmp_gate, inner_dim, dtype);
    XASSERT(dump_l2norm(tmp_gate, inner_dim, dtype, "silu active output"));

    //tmp_gate = element‑wise product tmp_gate,tmp_up.
    dotInplace(tmp_gate, tmp_gate, tmp_up, inner_dim, dtype);
    XASSERT(dump_l2norm(tmp_gate, inner_dim, dtype, "dot gate*up output"));

    //down: out = tmp_gate*down_proj_w^T
    //[1*n_embd]=[1*inner_dim]*[n_embd*inner_dim]^T
    XASSERT(down_proj_w->ndim == 2);
    XASSERT(down_proj_w->shape[0] == n_embd);
    XASSERT(down_proj_w->shape[1] == inner_dim);
    XASSERT(down_proj_w->elem_num == n_embd * inner_dim);
    matmulTrans(out, tmp_gate, down_proj_w);
    XASSERT(dump_l2norm(out, n_embd, dtype, "after down proj to 1*%u", n_embd));

    unlock_tmp_fc(ctx);
}
