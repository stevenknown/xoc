#include "ops.h"

int64_t compute_logits_and_sample_bf16(
    uint16_t * logits, uint16_t const* hidden, RTTensor const* wte,
    int64_t n_embd, int64_t vocab_size, float temperature, unsigned int n_topk,
    float eps)
{
    //Compute logits by LM Head.
    //LM Head shares weight with wte.
    //hidden is [1*n_embd], logits is [1*vocab_size], wte
    //is [vocab_size*n_embd].
    //[1*vocab_size] = in[1*n_embd]x[vocab_size*n_embd]^T;
    XASSERT(wte->ndim == 2);
    XASSERT(wte->shape[0] == vocab_size);
    XASSERT(wte->shape[1] == n_embd);
    matmulTransBF16(logits, hidden, 1, n_embd, wte);

    //Apply temperature.
    //Temperature scaling: divide logits by T before softmax.
    //T>1:more diverse output;0<T<1: more deterministic output.
    //Must apply before topk and softmax.
    if (temperature > 0.0f && temperature != 1.0f) {
        float recip = 1.0f / temperature;
        for (int64_t i = 0; i < vocab_size; i++) {
            float val = bf16_to_float(logits[i]);
            val = val * recip;
            logits[i] = float_to_bf16(val);
        }
    }
    RTTENSOR_DTYPE dtype = wte->dtype;
    XASSERT(dump_topn(logits, vocab_size, dtype, n_topk, "computed logits"));

    //Softmax convert logits to probability.
    softmax_inplace_bf16(logits, vocab_size, eps);

    int64_t result_token = INVALID_TOKEN;
    if (n_topk <= 1) {
        //Greedy argmax sampling.
        result_token = argmaxBF16(logits, vocab_size);
        XASSERT(dump("argmax to get token:%lld", result_token));
    } else {
        //Select next tokenid by topk.
        topkBF16(logits, vocab_size, n_topk, eps);
        XASSERT(dump_topn(logits, vocab_size, dtype, n_topk, "topk output"));

        //Random sample next token.
        result_token = sample_from_probs_bf16(logits, vocab_size, eps);
        XASSERT(dump("sample probability to get token:%lld", result_token));
    }
    return result_token;
}


int64_t compute_logits_and_sample_f16(
    uint16_t * logits, uint16_t const* hidden, RTTensor const* wte,
    int64_t n_embd, int64_t vocab_size, float temperature, unsigned int n_topk,
    float eps)
{
    //Compute logits by LM Head.
    //LM Head shares weight with wte.
    //hidden is [1*n_embd], logits is [1*vocab_size], wte
    //is [vocab_size*n_embd].
    //[1*vocab_size] = [1*n_embd]x[vocab_size*n_embd]^T;
    XASSERT(wte->ndim == 2);
    XASSERT(wte->shape[0] == vocab_size);
    XASSERT(wte->shape[1] == n_embd);
    matmulTransF16(logits, hidden, 1, n_embd, wte);

    //Apply temperature.
    //Temperature scaling: divide logits by T before softmax.
    //T>1:more diverse output;0<T<1: more deterministic output.
    //Must apply before topk and softmax.
    if (temperature > 0.0f && temperature != 1.0f) {
        float recip = 1.0f / temperature;
        for (int64_t i = 0; i < vocab_size; i++) {
            float val = fp16_to_float(logits[i]);
            val = val * recip;
            logits[i] = float_to_fp16(val);
        }
    }
    RTTENSOR_DTYPE dtype = wte->dtype;
    XASSERT(dump_topn(logits, vocab_size, dtype, n_topk, "computed logits"));

    //Softmax convert logits to probability.
    softmax_inplace_f16(logits, vocab_size, eps);

    int64_t result_token = INVALID_TOKEN;
    if (n_topk <= 1) {
        //Greedy argmax sampling.
        result_token = argmaxF16(logits, vocab_size);
        XASSERT(dump("argmax to get token:%lld", result_token));
    } else {
        //Select next tokenid by topk.
        topkF16(logits, vocab_size, n_topk, eps);
        XASSERT(dump_topn(logits, vocab_size, dtype, n_topk, "topk output"));

        //Random sample next token.
        result_token = sample_from_probs_f16(logits, vocab_size, eps);
        XASSERT(dump("sample probability to get token:%lld", result_token));
    }
    return result_token;
}


//logits:[1*vocab_size], the tmp buffer.
//hidden:[1*n_embd], transformer output hidden vector
int64_t compute_logits_and_sample_f32(
    float * logits, float const* hidden,
    RTTensor const* wte, int64_t n_embd, int64_t vocab_size,
    float temperature, unsigned int n_topk, float eps)
{
    //Compute logits by LM Head.
    //LM Head shares weight with wte.
    //hidden is [1*n_embd], logits is [1*vocab_size], wte
    //is [vocab_size*n_embd].
    //[1*vocab_size] = [1*n_embd]x[vocab_size*n_embd]^T;
    XASSERT(wte->ndim == 2);
    XASSERT(wte->shape[0] == vocab_size);
    XASSERT(wte->shape[1] == n_embd);
    matmulTrans(logits, hidden, 1, n_embd, wte);
    XASSERT(dump_vec(logits, vocab_size, RTTENSOR_DTYPE_F32,
        "compute logits wte,output"));
    XASSERT(dump_l2norm(
        logits, 1, vocab_size, RTTENSOR_DTYPE_F32,
        "compute logits wte,output"));

    //Apply temperature.
    //Temperature scaling: divide logits by T before softmax.
    //T>1:more diverse output;0<T<1: more deterministic output.
    //Must apply before topk and softmax.
    if (temperature > 0.0f && temperature != 1.0f) {
        float recip = 1.0f / temperature;
        for (int64_t i = 0; i < vocab_size; i++) {
            logits[i] *= recip;
        }
    }
    RTTENSOR_DTYPE dtype = wte->dtype;
    XASSERT(dump_topn(logits, vocab_size, dtype, n_topk,
        "computed logits"));

    //Softmax convert logits to probability.
    //[1*vocab_size] = [1*vocab_size];
    softmax_inplace_f32(logits, vocab_size, eps);
    XASSERT(dump_vec(logits, vocab_size, RTTENSOR_DTYPE_F32,
        "softmax,output"));

    int64_t result_token = INVALID_TOKEN;
    if (n_topk <= 1) {
        //Greedy argmax sampling.
        result_token = argmaxF32(logits, vocab_size);
        XASSERT(dump("argmax to get token:%lld", result_token));
    } else {
        //Select next tokenid by topk.
        //[1*vocab_size] = [1*vocab_size];
        topkF32(logits, vocab_size, n_topk, eps);
        XASSERT(dump_topn(logits, vocab_size, dtype, n_topk,
            "topk output"));

        //Random sample next token.
        //[1*vocab_size] = [1*vocab_size];
        result_token = sample_from_probs_f32(logits, vocab_size, eps);
        XASSERT(dump("sample probability to get token:%lld", result_token));
    }
    XASSERT(dump_vec(logits, vocab_size, RTTENSOR_DTYPE_F32,
        "result token:%u,computed logits,output", result_token));
    return result_token;
}


int64_t compute_logits_and_sample(
    void const* hidden, RTTensor const* wte,
    int64_t n_embd, int64_t vocab_size, float temperature, unsigned int n_topk,
    float eps, RTTENSOR_DTYPE dtype, RTCtx const* ctx)
{
    //logits:[1*vocab_size], output logits.
    void * logits = lock_logits(ctx);
    XASSERT(ctx->logits_len == vocab_size * getRTTensorDTypeByteSize(dtype));
    int64_t next_token = RTTENSOR_DTYPE_UNDEF;
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        next_token = compute_logits_and_sample_f32(
            logits, hidden, wte, n_embd, vocab_size,
            temperature, n_topk, eps);
        break;
    case RTTENSOR_DTYPE_F16:
        next_token = compute_logits_and_sample_f16(
            logits, hidden, wte, n_embd, vocab_size,
            temperature, n_topk, eps);
        break;
    case RTTENSOR_DTYPE_BF16:
        next_token = compute_logits_and_sample_bf16(
            logits, hidden, wte, n_embd, vocab_size,
            temperature, n_topk, eps);
        break;
    default: XASSERT(0);
    }
    unlock_logits(ctx);
    return next_token;
}
