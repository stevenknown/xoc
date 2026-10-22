#include "ops.h"

void transformer_blocks_rope(int64_t pos, RTCtx * ctx)
{
    void * hidden = lock_hidden(ctx);
    void * tmp1 = lock_tmp1(ctx);
    void * tmp2 = lock_tmp2(ctx);
    unsigned int n_layer = ctx->n_layer;
    unsigned int n_embd = ctx->n_embd;
    unsigned int n_head = ctx->n_head;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    XASSERT(ctx->hidden_len >= n_embd * elemsz);

    //Iter each transformer block.
    for (unsigned int layer = 0; layer < n_layer; layer++) {
        XASSERT(dump_l2norm(hidden, n_embd, dtype,
                "start transformer-block layer%d,pos%d", layer, pos));
        XASSERT(ctx->ln_1_weight);
        RTTensor * ln1_w = ctx->ln_1_weight[layer];
        RTTensor * ln2_w = ctx->ln_2_weight[layer];
        RTTensor * q_proj_w = ctx->q_proj_weight[layer];
        RTTensor * q_proj_b = ctx->q_proj_bias[layer];
        RTTensor * k_proj_w = ctx->k_proj_weight[layer];
        RTTensor * k_proj_b = ctx->k_proj_bias[layer];
        RTTensor * v_proj_w = ctx->v_proj_weight[layer];
        RTTensor * v_proj_b = ctx->v_proj_bias[layer];
        RTTensor * o_proj_w = ctx->o_proj_weight[layer];
        RTTensor * mlp_gate_proj_w = ctx->mlp_gate_proj_weight[layer];
        RTTensor * mlp_up_proj_w = ctx->mlp_up_proj_weight[layer];
        RTTensor * mlp_down_proj_w = ctx->mlp_down_proj_weight[layer];

        //First layer norm: input hidden, output tmp1.
        XASSERT(ctx->tmp1_len >= n_embd * elemsz);
        rmsnorm(tmp1, hidden, ln1_w, NULL, 1, n_embd, ctx->rmsnorm_epsilon);
        XASSERT(dump_l2norm(tmp1, n_embd, dtype,
                "rmsnorm_1 output,pos%d", pos));

        //Multihead attention, output tmp2.
        //MHA input tmp1, output tmp2, and update kcache/vcache
        //tmp1 is [1*n_embd], tmp2 is [1*n_embd].
        XASSERT(ctx->tmp2_len >= n_embd * elemsz);
        mha_rope(
            tmp2, tmp1,
            q_proj_w, q_proj_b, k_proj_w, k_proj_b, v_proj_w, v_proj_b,
            o_proj_w, layer, pos, n_embd, n_head, ctx);
        XASSERT(dump_l2norm(tmp2, n_embd, dtype, "mha_rope output,pos%d", pos));

        //Residual connect: hidden = hidden + tmp2
        //hidden is [1*n_embd], tmp2 is [1*n_embd].
        addInplace(hidden, tmp2, hidden, n_embd, dtype);
        XASSERT(dump_l2norm(hidden, n_embd, dtype,
                "residual output,pos%d", pos));

        //Second layer norm.
        //tmp1 is [1*n_embd], hidden is [1*n_embd].
        rmsnorm(tmp1, hidden, ln2_w, NULL, 1, n_embd, ctx->rmsnorm_epsilon);
        XASSERT(dump_l2norm(tmp1, n_embd, dtype,
                "rmsnorm_2 output,pos%d", pos));

        //FFN:MLP, input tmp1, output tmp2
        //tmp2 is [1*n_embd], tmp1 is [1*n_embd].
        mlp_swigu(
            tmp2, tmp1, mlp_gate_proj_w, mlp_up_proj_w,
            mlp_down_proj_w, ctx);
        XASSERT(dump_l2norm(tmp2, n_embd, dtype,
                "mlp_swigu output,pos%d", pos));

        //Residual connection: hidden = hidden + tmp2
        //hidden is [1*n_embd], tmp2 is [1*n_embd].
        addInplace(hidden, tmp2, hidden, n_embd, dtype);
        XASSERT(dump_l2norm(hidden, n_embd, dtype,
                "residual output,pos%d", pos));
    }

    //Final layer norm
    rmsnormInplace(
        hidden, ctx->ln_f_w, ctx->ln_f_b, 1, ctx->n_embd, ctx->rmsnorm_epsilon);
    XASSERT(dump_l2norm(hidden, n_embd, dtype,
            "last rmsnorm output,pos%d", pos));
    XASSERT(dump("end transformer-block,pos%d", pos));
    unlock_hidden(ctx);
    unlock_tmp1(ctx);
    unlock_tmp2(ctx);
}
