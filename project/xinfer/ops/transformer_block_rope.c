#include "ops.h"

void transformer_blocks_rope_seq(int64_t startpos, int64_t endpos, RTCtx * ctx)
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
    XASSERT(ctx->tmp3_len >= elemnum*elemsz);

    //Iter each transformer block.
    for (unsigned int layer = 0; layer < n_layer; layer++) {
        XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
            "start transformer-block,input hidden,layer%u,pos%u~pos%u",
            layer, startpos, endpos));
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

        //First rmsnorm: input hidden, output tmp1.
        XASSERT(ctx->tmp1_len >= elemnum * elemsz);
        //hidden is [seqlen*n_embd].
        //tmp1 is [seqlen*n_embd], ln1_w is [1*n_embd], ln1_b is [1*n_embd].
        rmsnorm(tmp1, hidden, ln1_w, NULL, seqlen, n_embd,
                ctx->rmsnorm_epsilon);
        XASSERT(dump_l2norm(tmp1, seqlen, n_embd, dtype,
            "rmsnorm_1 output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //Multihead attention, output tmp2.
        //MHA input tmp1, output tmp2, and update kcache/vcache
        //tmp1 is [seqlen*n_embd], tmp2 is [seqlen*n_embd].
        XASSERT(ctx->tmp2_len >= elemnum * elemsz);
        mha_rope(
            tmp2, tmp1,
            q_proj_w, q_proj_b, k_proj_w, k_proj_b, v_proj_w, v_proj_b,
            o_proj_w, layer, startpos, endpos, n_embd, n_head, ctx);
        XASSERT(dump_l2norm(tmp2, seqlen, n_embd, dtype,
            "MHA_ROPE output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //Residual connect: hidden = hidden + tmp2
        //hidden is [seqlen*n_embd], tmp2 is [seqlen*n_embd].
        addInplace(hidden, tmp2, hidden, elemnum, dtype);
        XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
            "residual output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //Second rmsnorm.
        //tmp1 is [seqlen*n_embd], hidden is [seqlen*n_embd].
        rmsnorm(tmp1, hidden, ln2_w, NULL, seqlen, n_embd,
                ctx->rmsnorm_epsilon);
        XASSERT(dump_l2norm(tmp1, seqlen, n_embd, dtype,
            "rmsnorm_2 output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //FFN:MLP, input tmp1, output tmp2
        //tmp2 is [seqlen*n_embd], tmp1 is [seqlen*n_embd].
        mlp_swigu(
            tmp2, tmp1, mlp_gate_proj_w, mlp_up_proj_w,
            mlp_down_proj_w, seqlen, ctx);
        XASSERT(dump_l2norm(tmp2, seqlen, n_embd, dtype,
            "MLP_SWIGU output,layer%u,pos%u~pos%u", layer, startpos, endpos));

        //Residual connection: hidden = hidden + tmp2
        //hidden is [seqlen*n_embd], tmp2 is [seqlen*n_embd].
        addInplace(hidden, tmp2, hidden, elemnum, dtype);
        XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
            "residual output,layer%u,pos%u~pos%u", layer, startpos, endpos));
        XASSERT(dump_mat(hidden, seqlen, n_embd, dtype,
            "sequenced residual output,layer%u,pos%u~pos%u",
            layer, startpos, endpos));
        XASSERT(dump("end transformer-block,layer%u,pos%u~pos%u",
            layer, startpos, endpos));
    }
    //Final rmsnorm
    //hidden is [seqlen*n_embd], tmp2 is [seqlen*n_embd].
    rmsnormInplace(
        hidden, ctx->ln_f_w, ctx->ln_f_b, seqlen, n_embd,
        ctx->rmsnorm_epsilon);
    XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
        "last rmsnorm output,pos%u~pos%u", startpos, endpos));
    XASSERT(dump("end transformer-block-rope for all layer,pos%u~pos%u",
        startpos, endpos));
    unlock_hidden(ctx);
    unlock_tmp1(ctx);
    unlock_tmp2(ctx);
}
