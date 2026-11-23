#include "ops.h"

void add_pos_embedding_range(
    void * hidden, int64_t startpos, int64_t endpos, RTCtx * ctx)
{
    RTTensor * wpe = ctx->wpe;
    XASSERT(wpe);
    XASSERT(startpos <= endpos);
    XASSERT(startpos >= 0);
    unsigned int n_embd = ctx->n_embd;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    BYTE * tmp1 = wpe->data;
    tmp1 = tmp1 + startpos * n_embd * elemsz;
    int64_t seqlen = endpos - startpos + 1;
    unsigned int elemnum = n_embd * seqlen;
    XASSERT(wpe->shape[0] == ctx->max_pos);
    XASSERT(wpe->shape[1] == n_embd);
    XASSERT(dump_l2norm(tmp1, seqlen, n_embd, dtype,
        "lookup wpe output,pos%u~pos%u", startpos, endpos));
    XASSERT(ctx->hidden_len >= elemnum * elemsz);
    addInplace(hidden, tmp1, hidden, elemnum, dtype);
    XASSERT(dump_l2norm(hidden, seqlen, n_embd, dtype,
        "Add pos-embedding to token-embedding,pos%d~pos%d",
        startpos, endpos));
}


void forward_token_seq(
    TokenArray const*restrict tokarr, int64_t seqlen, RTCtx *restrict ctx)
{
    XASSERT(ctx);
    int64_t vocab_size = ctx->vocab_size;
    unsigned int n_embd = ctx->n_embd;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    void * hidden = lock_hidden(ctx);
    RTTensor * wte = ctx->wte;
    RTTensor * wpe = ctx->wpe;
    XASSERT(ctx->hidden_len >= n_embd*elemsz*seqlen);

    //NOTE:hidden = token_embedding + position_embedding
    embedding_lookup_seq(hidden, wte, tokarr, seqlen);
    XASSERT(dump_l2norm(
        hidden, seqlen, n_embd, dtype, "lookup wte output,pos0~pos%d",
        seqlen - 1));
    XASSERT(dump_mat(hidden, seqlen, n_embd, dtype,
        "input hidden,pos0~pos%u", seqlen - 1));
    if (ctx->pos_encoding_type == POS_WPE) {
        add_pos_embedding_range(hidden, 0, seqlen - 1, ctx);
    }
    unlock_hidden(ctx);
    transformer_blocks_seq(0, seqlen - 1, ctx);
}


void forward_token(int64_t token_id, int64_t pos, RTCtx * ctx)
{
    XASSERT(ctx);
    XASSERT(pos >= 0 && pos < ctx->max_pos);
    int64_t vocab_size = ctx->vocab_size;
    unsigned int n_embd = ctx->n_embd;
    RTTENSOR_DTYPE dtype = ctx->tensor_dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    void * hidden = lock_hidden(ctx);
    XASSERT(ctx->hidden_len >= n_embd*elemsz);
    RTTensor * wte = ctx->wte;
    RTTensor * wpe = ctx->wpe;

    //NOTE:hidden = token_embedding + position_embedding
    embedding_lookup(hidden, wte, token_id);
    XASSERT(dump_l2norm(hidden, 1, n_embd, dtype,
        "lookup wte output,pos%u", pos));
    XASSERT(dump_vec(hidden, n_embd, dtype, "input hidden,pos%u", pos));
    if (ctx->pos_encoding_type == POS_WPE) {
        add_pos_embedding_range(hidden, pos, pos, ctx);
    }
    unlock_hidden(ctx);
    transformer_blocks_seq(pos, pos, ctx);
}
