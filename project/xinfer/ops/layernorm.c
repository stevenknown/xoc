#include "ops.h"

void layernormF32Inplace(
    float * in_out, float const*restrict w,
    float const*restrict b, unsigned int dim, float eps)
{
    //compute mean.
    float mean = 0.0f;
    for (unsigned int i = 0; i < dim; i++) {
        mean += in_out[i];
    }
    mean /= (float)dim;

    //compute variance.
    float var = 0.0f;
    for (unsigned int i = 0; i < dim; i++) {
        float diff = in_out[i] - mean;
        var += diff * diff;
    }
    var /= (float)dim;

    //perform scale and shift.
    float recip_std = 1.0f / sqrtf(var + eps);
    for (unsigned int i = 0; i < dim; i++) {
        float gamma = w[i];
        float beta = b[i];
        float val = (in_out[i] - mean) * recip_std; //use MUL rather than DIV.
        in_out[i] = val * gamma + beta;
    }
}


void layernormF32(
    float * out, float const*restrict in, float const*restrict w,
    float const*restrict b, unsigned int dim, float eps)
{
    //compute mean.
    float mean = 0.0f;
    for (unsigned int i = 0; i < dim; i++) {
        mean += in[i];
    }
    mean /= (float)dim;

    //Compute variance.
    float var = 0.0f;
    for (unsigned int i = 0; i < dim; i++) {
        float diff = in[i] - mean;
        var += diff * diff;
    }
    var /= (float)dim;

    //Perform scale and shift.
    float recip_std = 1.0f / sqrtf(var + eps);
    for (unsigned int i = 0; i < dim; i++) {
        float gamma = w[i];
        float beta = b[i];
        float val = (in[i] - mean) * recip_std; //use MUL rather than DIV.
        out[i] = val * gamma + beta;
    }
}


void layernormF16(
    float * out, float const*restrict in, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int dim, float eps)
{
    //Compute mean.
    float mean = 0.0f;
    for (unsigned int i = 0; i < dim; i++) {
        mean += in[i];
    }
    mean /= (float)dim;

    //Compute variance.
    float var = 0.0f;
    for (unsigned int i = 0; i < dim; i++) {
        float diff = in[i] - mean;
        var += diff * diff;
    }
    var /= (float)dim;

    //Perform scale and shift.
    float recip_std = 1.0f / sqrtf(var + eps);
    for (unsigned int i = 0; i < dim; i++) {
        float gamma = fp16_to_float(w[i]);
        float beta = fp16_to_float(b[i]);
        float val = (in[i] - mean) * recip_std; //use MUL rather than DIV.
        out[i] = val * gamma + beta;
    }
}


void layernormBF16(
    float * out, float const*restrict in, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int dim, float eps)
{
    //Compute mean.
    float mean = 0.0f;
    for (unsigned int i = 0; i < dim; i++) {
        mean += in[i];
    }
    mean /= (float)dim;

    //Compute variance.
    float var = 0.0f;
    for (unsigned int i = 0; i < dim; i++) {
        float diff = in[i] - mean;
        var += diff * diff;
    }
    var /= (float)dim;

    //Perform scale and shift.
    float recip_std = 1.0f / sqrtf(var + eps);
    for (unsigned int i = 0; i < dim; i++) {
        float gamma = bf16_to_float(w[i]);
        float beta = bf16_to_float(b[i]);
        float val = (in[i] - mean) * recip_std; //use MUL rather than DIV.
        out[i] = val * gamma + beta;
    }
}


void layernormInplace(
    float * in_out, RTTensor const*restrict w,
    RTTensor const*restrict b, unsigned int dim, float eps)
{
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F32);
        layernormF32Inplace(
            in_out, (float const*)w->data, (float const*)b->data, dim, eps);
        return;
    case RTTENSOR_DTYPE_F16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F16);
        XASSERT(0); //TODO:
        //layernormF16Inplace(
        //    out, in, (uint16_t const*)w->data, (uint16_t const*)b->data,
        //    dim, eps);
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_BF16);
        XASSERT(0); //TODO:
        //layernormBF16Inplace(
        //    out, in, (uint16_t const*)w->data, (uint16_t const*)b->data,
        //    dim, eps);
        return;
    default:
        XASSERT(0);
        return;
    }
}


void layernorm(
    float * out, float const*restrict in, RTTensor const*restrict w,
    RTTensor const*restrict b, unsigned int dim, float eps)
{
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F32);
        layernormF32(
            out, in, (float const*)w->data, (float const*)b->data, dim, eps);
        return;
    case RTTENSOR_DTYPE_F16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F16);
        layernormF16(
            out, in, (uint16_t const*)w->data, (uint16_t const*)b->data,
            dim, eps);
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_BF16);
        layernormBF16(
            out, in, (uint16_t const*)w->data, (uint16_t const*)b->data,
            dim, eps);
        return;
    default:
        XASSERT(0);
        return;
    }
}
