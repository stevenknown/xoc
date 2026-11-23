#include "ops.h"

void transformer_blocks_gpt2_seq(int64_t startpos, int64_t endpos, RTCtx * ctx)
{
    void * hidden = lock_hidden(ctx);
    void * tmp1 = lock_tmp1(ctx);
    void * tmp2 = lock_tmp2(ctx);
    unsigned int n_layer = ctx->n_layer;
    unsigned int n_embd = ctx->n_embd;
    unsigned int n_head = ctx->n_head;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    int64_t seqlen = endpos - startpos + 1;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    unsigned int elemnum = seqlen * n_embd;
    XASSERT(ctx->hidden_len >= elemnum*elemsz);
    XASSERT(ctx->tmp1_len >= elemnum*elemsz);
    XASSERT(ctx->tmp2_len >= elemnum*elemsz);

    //Iter each transformer block.
    for (unsigned int layer = 0; layer < n_layer; layer++) {
        XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
            "start transformer-block layer%u,pos%u~pos%u",
            layer, startpos, endpos));
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

        //hidden buffer stores the hidden state for all tokens.
        //First layernorm: input hidden, output tmp1.
        //hidden is [seqlen*n_embd].
        //tmp1 is [seqlen*n_embd], ln1_w is [1*n_embd], ln1_b is [1*n_embd].
        layernorm(tmp1, hidden, ln1_w, ln1_b, elemnum, ctx->layernorm_epsilon);
        XASSERT(dump_l2norm(tmp1, seqlen, n_embd, dtype,
            "ln_1 output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //Multihead attention, output tmp2.
        //MHA input tmp1, output tmp2, and update kcache/vcache
        //tmp1 is [seqlen*n_embd], tmp2 is [seqlen*n_embd].
        //c_attn_w is [n_embd, 3*n_embd], c_proj_w is [n_embd, n_embd].
        mha_seq(tmp2, tmp1, c_attn_w, c_attn_b, o_proj_w, o_proj_b,
                layer, startpos, endpos, ctx);
        XASSERT(dump_l2norm(tmp2, seqlen, n_embd, dtype,
            "MHA output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //Residual connect: hidden = hidden + tmp2
        //hidden is [seqlen*n_embd], tmp2 is [seqlen*n_embd].
        addF32Inplace(hidden, tmp2, hidden, elemnum);
        XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
            "residual output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //Second layernorm.
        //tmp1 is [seqlen*n_embd], hidden is [seqlen*n_embd].
        layernorm(tmp1, hidden, ln2_w, ln2_b, elemnum, ctx->layernorm_epsilon);
        XASSERT(dump_l2norm(tmp1, seqlen, n_embd, dtype,
            "ln_2 output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //FFN:MLP, input tmp1, output tmp2
        //tmp2 is [seqlen*n_embd], tmp1 is [seqlen*n_embd].
        mlp_seq(tmp2, tmp1, mlp_c_fc_w, mlp_c_fc_b,
                mlp_down_proj_w, mlp_down_proj_b, seqlen, ctx);
        XASSERT(dump_l2norm(tmp2, seqlen, n_embd, dtype,
            "MLP output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //Residual connection: hidden = hidden + tmp2
        //hidden is [seqlen*n_embd], tmp2 is [seqlen*n_embd].
        addF32Inplace(hidden, tmp2, hidden, elemnum);
        XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
            "residual output,layer%u,pos%u~pos%u", layer, startpos, endpos));
    }

    //Final layernorm
    layernormInplace(
        hidden, ctx->ln_f_w, ctx->ln_f_b, elemnum, ctx->layernorm_epsilon);
    XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
        "last ln output,pos%u~pos%u", startpos, endpos));
    XASSERT(dump("end transformer-block,pos%u~pos%u", startpos, endpos));
    unlock_hidden(ctx);
    unlock_tmp1(ctx);
    unlock_tmp2(ctx);
}
