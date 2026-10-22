#include "ops.h"

int64_t prefill(
    int64_t * first_next_token, TokenArray const* tokarr, RTCtx *restrict ctx)
{
    //prefill
    // |--loop over each prompt token
    //   |--fetch token embedding wte and position embedding wpe
    //   |--transformer_blocks_gpt2() run all n_layer transformer layers
    //     |--layernorm -> mha(writes KV-Cache) ->
    //        residual add -> layernorm -> mlp -> residual add
    int64_t vocab_size = ctx->vocab_size;
    int n_embd = ctx->n_embd;
    RTTensor * wte = ctx->wte;
    int64_t * arr = tokarr->array;
    int64_t seq_len = tokarr->array_elem_num;
    XASSERT(seq_len <= ctx->max_pos);

    //Iterate over every token in tokarr and run forward computation
    //step by step.
    for (int64_t pos = 0; pos < seq_len; pos++) {
        int64_t tid = arr[pos];
        //pos is the position of wpe and kvcache.
        //Forward computing, write K,V into KVCache.
        //*first_next_token = decode(tid, pos, ctx);
        forward_token(tid, pos, ctx);
    }
    //All prompt tokens are processed, KV-Cache stores all K/V for prompt
    //sequence.
    //Take hidden state from the last position, compute logits and sample
    //the first next_token.
    void * last_hidden = lock_hidden(ctx);
    void * logits = lock_logits(ctx);
    XASSERT(ctx->logits_len ==
            vocab_size * getRTTensorDTypeByteSize(ctx->tensor_dtype));
    *first_next_token = compute_logits_and_sample(
        logits, last_hidden, wte, n_embd, vocab_size, ctx->temperature,
        ctx->n_topk, ctx->layernorm_epsilon, ctx->tensor_dtype);
    XASSERT(dump("sample_logits output,tokenid %d", *first_next_token));
    unlock_hidden(ctx);
    unlock_logits(ctx);
    if (*first_next_token < 0 || *first_next_token >= vocab_size) {
        XASSERT(dump("sample token invalid, tokenid:%d", *first_next_token));
        return -1;
    }

    //Starting position for decode phase equals prompt sequence length.
    int64_t decode_pos = seq_len;
    return decode_pos;
}
