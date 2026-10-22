#include "ops.h"

void forward_token(int64_t token_id, int64_t pos, RTCtx * ctx)
{
    XASSERT(ctx);
    XASSERT(pos >= 0 && pos < ctx->max_pos);
    int64_t vocab_size = ctx->vocab_size;
    unsigned int n_embd = ctx->n_embd;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    void * hidden = lock_hidden(ctx);
    RTTensor * wte = ctx->wte;
    RTTensor * wpe = ctx->wpe;

    //hidden = token_embedding + position_embedding
    embedding_lookup(hidden, wte, token_id);
    XASSERT(dump_l2norm(hidden, n_embd, dtype, "lookup wte output,pos%d", pos));

    if (wpe != NULL) {
        //GPT2 model.
        void * tmp1 = lock_tmp1(ctx);
        XASSERT(wpe->shape[0] == ctx->max_pos);
        XASSERT(wpe->shape[1] == n_embd);
        XASSERT(ctx->tmp1_len == wpe->shape[1] * elemsz);
        embedding_lookup(tmp1, wpe, pos);

        XASSERT(dump_l2norm(
            tmp1, n_embd, dtype, "lookup wpe output,pos%d", pos));
        XASSERT(ctx->hidden_len == ctx->tmp1_len);
        XASSERT(ctx->hidden_len == n_embd * elemsz);
        addInplace(hidden, tmp1, hidden, n_embd, dtype);
        XASSERT(dump_l2norm(hidden, n_embd, dtype, "addF32 output,pos%d", pos));

        unlock_tmp1(ctx);
        unlock_hidden(ctx);
        transformer_blocks_gpt2(pos, ctx);
        return;
    }
    //LLAMA model.
    unlock_hidden(ctx);
    transformer_blocks_rope(pos, ctx);
}


int64_t decode(int64_t token_id, int64_t pos, RTCtx * ctx)
{
    XASSERT(ctx);
    XASSERT(pos >= 0 && pos < ctx->max_pos);
    forward_token(token_id, pos, ctx);

    //LM Head projects the ctx->hidden that produced by transformer to
    //vocab space,
    //and get the token's logits.
    //LM Head shares weight with wte.
    //hidden is [1*n_embd], out_logits is [1*vocab_size], wte
    //is [vocab_size*n_embd].
    //[1*vocab_size] = [1*n_embd] x [vocab_size*n_embd]^T;
    int64_t vocab_size = ctx->vocab_size;
    unsigned int n_embd = ctx->n_embd;
    RTTensor * wte = ctx->wte;
    XASSERT(wte->ndim == 2);
    XASSERT(wte->shape[0] == vocab_size);
    XASSERT(wte->shape[1] == n_embd);

    //All prompt tokens are processed, KV-Cache stores all K/V for prompt
    //sequence.
    //Take hidden state from the last position, compute logits and sample
    //the first next_token.
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    BYTE * last_hidden = lock_hidden(ctx);
    BYTE * logits = lock_logits(ctx);
    XASSERT(ctx->logits_len == vocab_size * elemsz);
    int64_t next_token = compute_logits_and_sample(
        logits, last_hidden, wte, n_embd, vocab_size, ctx->temperature,
        ctx->n_topk, ctx->layernorm_epsilon, ctx->tensor_dtype);
    XASSERT(dump("sample_logits output,tokenid %d", next_token));
    unlock_hidden(ctx);
    unlock_logits(ctx);
    return next_token;
}
