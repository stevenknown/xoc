#include "ops.h"

void transformer_blocks_gpt2(int64_t pos, RTCtx * ctx)
{
    float * hidden = lock_hidden(ctx);
    float * tmp1 = lock_tmp1(ctx);
    float * tmp2 = lock_tmp2(ctx);
    unsigned int n_layer = ctx->n_layer;
    unsigned int n_embd = ctx->n_embd;
    unsigned int n_head = ctx->n_head;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;

    //Iter each transformer block.
    for (unsigned int layer = 0; layer < n_layer; layer++) {
        XASSERT(dump_l2norm(hidden, n_embd, dtype,
                "start transformer-block layer%d,pos%d", layer, pos));
        RTTensor * ln1_w = ctx->ln_1_weight[layer];
        RTTensor * ln1_b = ctx->ln_1_bias[layer];
        RTTensor * ln2_w = ctx->ln_2_weight[layer];
        RTTensor * ln2_b = ctx->ln_2_bias[layer];
        RTTensor * c_attn_w = ctx->c_attn_weight[layer];
        RTTensor * c_attn_b = ctx->c_attn_bias[layer];
        RTTensor * o_proj_w = ctx->o_proj_weight[layer];
        RTTensor * o_proj_b = ctx->o_proj_bias[layer];
        RTTensor * mlp_c_fc_w = ctx->mlp_c_fc_weight[layer];
        RTTensor * mlp_c_fc_b = ctx->mlp_c_fc_bias[layer];
        RTTensor * mlp_down_proj_w = ctx->mlp_down_proj_weight[layer];
        RTTensor * mlp_down_proj_b = ctx->mlp_down_proj_bias[layer];

        //First layer norm: input hidden, output tmp1.
        layernorm(tmp1, hidden, ln1_w, ln1_b, n_embd, ctx->layernorm_epsilon);
        XASSERT(dump_l2norm(tmp1, n_embd, dtype, "ln_1 output,pos%d", pos));

        //Multihead attention, output tmp2.
        //MHA input tmp1, output tmp2, and update kcache/vcache
        //tmp1 is [1*n_embd], tmp2 is [1*n_embd].
        mha(tmp2, tmp1, c_attn_w, c_attn_b, o_proj_w, o_proj_b,
            layer, pos, n_embd, n_head, ctx);
        XASSERT(dump_l2norm(tmp2, n_embd, dtype, "MHA output,pos%d", pos));

        //Residual connect: hidden = hidden + tmp2
        //hidden is [1*n_embd], tmp2 is [1*n_embd].
        addF32Inplace(hidden, tmp2, hidden, n_embd);
        XASSERT(dump_l2norm(hidden, n_embd, dtype,
                "residual output,pos%d", pos));

        //Second layer norm.
        //tmp1 is [1*n_embd], hidden is [1*n_embd].
        layernorm(tmp1, hidden, ln2_w, ln2_b, n_embd, ctx->layernorm_epsilon);
        XASSERT(dump_l2norm(tmp1, n_embd, dtype, "ln_2 output,pos%d", pos));

        //FFN:MLP, input tmp1, output tmp2
        //tmp2 is [1*n_embd], tmp1 is [1*n_embd].
        mlp(tmp2, tmp1, mlp_c_fc_w, mlp_c_fc_b,
            mlp_down_proj_w, mlp_down_proj_b, ctx);
        XASSERT(dump_l2norm(tmp2, n_embd, dtype, "MLP output,pos%d", pos));

        //Residual connection: hidden = hidden + tmp2
        //hidden is [1*n_embd], tmp2 is [1*n_embd].
        addF32Inplace(hidden, tmp2, hidden, n_embd);
        XASSERT(dump_l2norm(hidden, n_embd, dtype,
                "residual output,pos%d", pos));
    }

    //Final layer norm
    layernormInplace(
        hidden, ctx->ln_f_w, ctx->ln_f_b, ctx->n_embd,
        ctx->layernorm_epsilon);
    XASSERT(dump_l2norm(hidden, n_embd, dtype, "last ln output,pos%d", pos));
    XASSERT(dump("end transformer-block,pos%d", pos));
    unlock_hidden(ctx);
    unlock_tmp1(ctx);
    unlock_tmp2(ctx);
}
