#include "ops.h"

float findmaxvalueF32(float const* logits, int64_t elemnum)
{
    float max_val = logits[0];
    HINT_VECT
    for (int64_t i = 1; i < elemnum; i++) {
        if (logits[i] > max_val) {
            max_val = logits[i];
        }
    }
    return max_val;
}


float findmaxvalueF16(uint16_t const* logits, int64_t elemnum)
{
    float max_val = fp16_to_float(logits[0]);
    for (int64_t i = 1; i < elemnum; i++) {
        float val = fp16_to_float(logits[i]);
        if (val > max_val) {
            max_val = val;
        }
    }
    return max_val;
}


float findmaxvalueBF16(uint16_t const* logits, int64_t elemnum)
{
    float max_val = bf16_to_float(logits[0]);
    for (int64_t i = 1; i < elemnum; i++) {
        float val = bf16_to_float(logits[i]);
        if (val > max_val) {
            max_val = val;
        }
    }
    return max_val;
}


void softmax_inplace_f16(uint16_t * logits, int64_t elemnum, float eps)
{
    XASSERT(elemnum > 0 && logits);

    //Find max value for numerical stability.
    float max_val = findmaxvalueF16(logits, elemnum);

    //Numberically stable version of softmax.
    //softmax(X(i))=exp(X(i)-max_val) / sigma_all_i(exp(X(i)-max_val))

    //exp(X(i)-max_val), and accumulate the sum of all expf.
    float sum = 0.0f;
    for (int64_t i = 0; i < elemnum; i++) {
        float val = fp16_to_float(logits[i]) - max_val;
        val = expf(val);
        logits[i] = float_to_fp16(val); //expf(x):float type e^x
        sum = sum + val;
    }
    XASSERT(sum >= 0.0f);
    sum = fmaxf(sum, eps);

    //Divide sum, get probability.
    float recip_sum = 1.0f / sum;
    for (int64_t i = 0; i < elemnum; i++) {
        float val = fp16_to_float(logits[i]);
        val = val * recip_sum;
        logits[i] = float_to_fp16(val);
    }
}


void softmax_inplace_bf16(uint16_t * logits, int64_t elemnum, float eps)
{
    XASSERT(elemnum > 0 && logits);

    //Find max value for numerical stability.
    float max_val = findmaxvalueBF16(logits, elemnum);

    //Numberically stable version of softmax.
    //softmax(X(i))=exp(X(i)-max_val) / sigma_all_i(exp(X(i)-max_val))

    //exp(X(i)-max_val), and accumulate the sum of all expf.
    float sum = 0.0f;
    for (int64_t i = 0; i < elemnum; i++) {
        float val = bf16_to_float(logits[i]) - max_val;
        val = expf(val);
        logits[i] = float_to_bf16(val); //expf(x):float type e^x
        sum = sum + val;
    }
    XASSERT(sum >= 0.0f);
    sum = fmaxf(sum, eps);

    //Divide sum, get probability.
    float recip_sum = 1.0f / sum;
    for (int64_t i = 0; i < elemnum; i++) {
        float val = bf16_to_float(logits[i]);
        val = val * recip_sum;
        logits[i] = float_to_bf16(val);
    }
}


void softmax_inplace_f32(float * logits, int64_t elemnum, float eps)
{
    XASSERT(elemnum > 0 && logits);

    //Find max value for numerical stability.
    float max_val = findmaxvalueF32(logits, elemnum);

    //Numberically stable version of softmax.
    //softmax(X(i))=exp(X(i)-max_val) / sigma_all_i(exp(X(i)-max_val))

    //exp(X(i)-max_val), and accumulate the sum of all expf.
    float sum = 0.0f;
    for (int64_t i = 0; i < elemnum; i++) {
        float val = logits[i] - max_val;
        logits[i] = expf(val); //expf(x):float type e^x
        sum = sum + logits[i];
    }
    XASSERT(sum >= 0.0f);
    sum = fmaxf(sum, eps);

    //Divide sum, get probability.
    float recip_sum = 1.0f / sum;
    HINT_VECT
    for (int64_t i = 0; i < elemnum; i++) {
        logits[i] = logits[i] * recip_sum;
    }
}


void softmax_inplace(
    void * logits, int64_t elemnum, float eps, RTTENSOR_DTYPE dtype)
{
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        softmax_inplace_f32(logits, elemnum, eps);
        return;
    case RTTENSOR_DTYPE_F16:
        softmax_inplace_f16(logits, elemnum, eps);
        return;
    case RTTENSOR_DTYPE_BF16:
        softmax_inplace_bf16(logits, elemnum, eps);
        return;
    default:
        XASSERT(0);
        return;
    }
}
