#include "ops.h"

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
    XASSERT(ctx->logits_len == vocab_size * elemsz);
    int64_t next_token = compute_logits_and_sample(
        last_hidden, wte, n_embd, vocab_size, ctx->temperature,
        ctx->n_topk, ctx->layernorm_epsilon, ctx->tensor_dtype, ctx);
    XASSERT(dump("decode,after sample_logits output,tokenid %d", next_token));
    unlock_hidden(ctx);
    return next_token;
}
