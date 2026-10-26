#include "ops.h"

static void embedding_lookup_f16(
    uint16_t * out, RTTensor const* emb_weight, int64_t token_id)
{
    XASSERT(emb_weight && out);

    //Embedding table must be two-dimensional.
    XASSERT(emb_weight->ndim == 2);

    //Validate token index within valid vocabulary range.
    int64_t vocab_size = emb_weight->shape[0];
    XASSERT(token_id >= 0 && token_id < vocab_size);
    int64_t n_emb = emb_weight->shape[1];
    XASSERT(emb_weight->dtype == RTTENSOR_DTYPE_F16);

    //Calculate byte offset of target embedding row.
    size_t row_byte_offset = (size_t)token_id * n_emb * sizeof(uint16_t);
    uint8_t const* weight_base = (const uint8_t*)emb_weight->data;
    uint8_t const* row_ptr = weight_base + row_byte_offset;
    uint16_t const* src = (const uint16_t*)row_ptr;
    memcpy(out, src, n_emb * sizeof(uint16_t));
}


static void embedding_lookup_bf16(
    float * out, RTTensor const* emb_weight, int64_t token_id)
{
    XASSERT(emb_weight && out);

    //Embedding table must be two-dimensional.
    XASSERT(emb_weight->ndim == 2);

    //Validate token index within valid vocabulary range.
    int64_t vocab_size = emb_weight->shape[0];
    XASSERT(token_id >= 0 && token_id < vocab_size);
    int64_t n_emb = emb_weight->shape[1];
    XASSERT(emb_weight->dtype == RTTENSOR_DTYPE_BF16);

    //Calculate byte offset of target embedding row.
    size_t row_byte_offset = (size_t)token_id * n_emb * sizeof(uint16_t);
    uint8_t const* weight_base = (const uint8_t*)emb_weight->data;
    uint8_t const* row_ptr = weight_base + row_byte_offset;
    uint16_t const* src = (const uint16_t*)row_ptr;
    memcpy(out, src, n_emb * sizeof(uint16_t));
}


static void embedding_lookup_f32(
    float *restrict out, RTTensor const*restrict emb_weight, int64_t token_id)
{
    XASSERT(emb_weight && out);

    //Embedding table must be two-dimensional.
    XASSERT(emb_weight->ndim == 2);

    //Validate token index within valid vocabulary range.
    int64_t vocab_size = emb_weight->shape[0];
    int64_t n_emb = emb_weight->shape[1];
    XASSERT(token_id >= 0 && token_id < vocab_size);
    XASSERT(emb_weight->dtype == RTTENSOR_DTYPE_F32);
    float const* weight_base = (float const*)emb_weight->data;
    float const* src = weight_base + token_id * n_emb;
    memcpy(out, src, n_emb * sizeof(float));
}


void embedding_lookup(void * out, RTTensor const* emb_weight, int64_t token_id)
{
    XASSERT(emb_weight && out);
    switch (emb_weight->dtype) {
    case RTTENSOR_DTYPE_F32:
        embedding_lookup_f32(out, emb_weight, token_id);
        return;
    case RTTENSOR_DTYPE_F16:
        embedding_lookup_f16(out, emb_weight, token_id);
        return;
    case RTTENSOR_DTYPE_BF16:
        embedding_lookup_bf16(out, emb_weight, token_id);
        return;
    default: XASSERT(0); //unsupported
    }
}
