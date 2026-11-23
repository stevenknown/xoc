#include "ops.h"

void embedding_lookup(
    void *restrict out, RTTensor const*restrict emb_weight, int64_t token_id)
{
    XASSERT(emb_weight && out);
    RTTENSOR_DTYPE dtype = emb_weight->dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);

    //Embedding table must be two-dimensional.
    XASSERT(emb_weight->ndim == 2);

    //Validate token index within valid vocabulary range.
    int64_t vocab_size = emb_weight->shape[0];
    int64_t n_emb = emb_weight->shape[1];
    XASSERT(token_id >= 0 && token_id < vocab_size);
    BYTE const* weight_base = (BYTE const*)emb_weight->data;
    BYTE const* src = weight_base + token_id * n_emb * elemsz;
    memcpy(out, src, n_emb * elemsz);
}


void embedding_lookup_seq(
    void *restrict out, RTTensor const*restrict emb_weight,
    TokenArray const*restrict tokarr, int64_t seqlen)
{
    XASSERT(emb_weight && out);
    XASSERT(seqlen <= tokarr->array_elem_num);

    //Embedding table must be two-dimensional.
    XASSERT(emb_weight->ndim == 2);

    //Validate token index within valid vocabulary range.
    int64_t vocab_size = emb_weight->shape[0];
    int64_t n_emb = emb_weight->shape[1];
    RTTENSOR_DTYPE dtype = emb_weight->dtype;
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    BYTE const* weight_base = (BYTE const*)emb_weight->data;

    //Iterate over every token in tokarr.
    unsigned int oneembsz = n_emb * elemsz; //bytesize of one line embd.
    int64_t * arr = tokarr->array;
    for (int64_t pos = 0; pos < seqlen; pos++) {
        int64_t tid = arr[pos];
        XASSERT(tid >= 0 && tid < vocab_size);
        BYTE const* src = weight_base + tid * oneembsz;
        memcpy(out + pos * oneembsz, src, oneembsz);
    }
}
