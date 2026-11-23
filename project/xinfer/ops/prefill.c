#include "ops.h"

static int64_t compute_next_token(
    int64_t * first_next_token, int64_t startpos, int64_t endpos,
    RTCtx *restrict ctx)
{
    //All prompt tokens are processed, KV-Cache stores all K/V for prompt
    //sequence.
    //Take hidden state from the last position, compute logits and sample
    //the first next_token.
    BYTE * hidden = lock_hidden(ctx);
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    int64_t seqlen = endpos - startpos + 1;
    unsigned int elemsz = getRTTensorDTypeByteSize(ctx->tensor_dtype);
    XASSERT(ctx->hidden_len >= seqlen*ctx->n_embd*elemsz);
    XASSERT(dump_mat(hidden, seqlen, ctx->n_embd, ctx->tensor_dtype,
        "after forward_token_seq output,hidden,pos%u~pos%u", startpos, endpos));

    //Point to the last hidden.
    int64_t vocab_size = ctx->vocab_size;
    BYTE * last_hidden = hidden + (seqlen - 1) * ctx->n_embd * elemsz;
    *first_next_token = compute_logits_and_sample(
        last_hidden, ctx->wte, ctx->n_embd, vocab_size,
        ctx->temperature, ctx->n_topk, ctx->layernorm_epsilon,
        ctx->tensor_dtype, ctx);
    XASSERT(dump("sample_logits output,tokenid %d", *first_next_token));
    unlock_hidden(ctx);
    if (*first_next_token < 0 || *first_next_token >= vocab_size) {
        XASSERT(dump("sample token invalid, tokenid:%d", *first_next_token));
        return -1;
    }

    //Starting position for decode phase equals prompt sequence length.
    return endpos + 1;
}


int64_t prefill(
    int64_t * first_next_token, TokenArray const* tokarr, RTCtx *restrict ctx)
{
    XASSERT(dump("start prefill for tokenarray"));
    int64_t * arr = tokarr->array;
    int64_t seqlen = tokarr->array_elem_num;
    XASSERT(seqlen <= ctx->max_pos);

    //Iterate over every token in tokarr and run forward computation
    //step by step.
    for (int64_t pos = 0; pos < seqlen; pos++) {
        int64_t tid = arr[pos];
        //pos is the position of wpe and kvcache.
        //Forward computing, write K,V into KVCache.
        //*first_next_token = decode(tid, pos, ctx);
        forward_token(tid, pos, ctx);
    }
    int64_t last_pos = seqlen - 1;
    int64_t next_tok_pos = compute_next_token(
        first_next_token, last_pos, last_pos, ctx);
    XASSERT(dump("after prefill,next tokenid:%u,pos%u",
        *first_next_token, next_tok_pos));
    return next_tok_pos;
}


int64_t prefill_seq(
    int64_t * first_next_token, TokenArray const* tokarr, RTCtx *restrict ctx)
{
    XASSERT(dump("start prefill_seq for tokenarray"));
    int64_t seqlen = tokarr->array_elem_num;
    XASSERT(seqlen <= ctx->max_pos);

    //Process tokens in tokarr all at once.
    forward_token_seq(tokarr, seqlen, ctx);
    int64_t last_pos = seqlen - 1;
    int64_t next_tok_pos = compute_next_token(
        first_next_token, 0, last_pos, ctx);
    XASSERT(dump("after prefill_seq,next tokenid:%u,pos%u",
        *first_next_token, next_tok_pos));
    return next_tok_pos;
}
