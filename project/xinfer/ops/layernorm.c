#include "ops.h"

void layernormF32Inplace(
    float * in_out, float const*restrict w,
    float const*restrict b, unsigned int indim, unsigned int wdim, float eps)
{
    XASSERT(indim % wdim == 0);
    XASSERT(wdim != 0);
    XASSERT(indim != 0);
    for (unsigned int ind = 0; ind < indim; ind += wdim) {
        float * subin_out = in_out + ind;

        //compute mean.
        float mean = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            mean += subin_out[i];
        }
        mean /= (float)wdim;

        //compute variance.
        float var = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            float diff = subin_out[i] - mean;
            var += diff * diff;
        }
        var /= (float)wdim;

        //perform scale and shift.
        float recip_std = 1.0f / sqrtf(var + eps);
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            float gamma = w[i];
            float beta = b[i];

            //use MUL rather than DIV.
            float val = (subin_out[i] - mean) * recip_std;
            subin_out[i] = val * gamma + beta;
        }
    }
}


void layernormF16Inplace(
    uint16_t * in_out, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int indim, unsigned int wdim, float eps)
{
    XASSERT(0); //TODO:
}


void layernormBF16Inplace(
    uint16_t * in_out, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int indim, unsigned int wdim, float eps)
{
    XASSERT(0); //TODO:
}


void layernormF32(
    float * out, float const*restrict in, float const*restrict w,
    float const*restrict b, unsigned int indim, unsigned int wdim, float eps)
{
    XASSERT(indim % wdim == 0);
    XASSERT(wdim != 0);
    XASSERT(indim != 0);
    for (unsigned int ind = 0; ind < indim; ind += wdim) {
        float const* subin = in + ind;
        float * subout = out + ind;

        //Compute mean.
        float mean = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            mean += subin[i];
        }
        mean /= (float)wdim;

        //Compute variance.
        float var = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            float diff = subin[i] - mean;
            var += diff * diff;
        }
        var /= (float)wdim;

        //Perform scale and shift.
        float recip_std = 1.0f / sqrtf(var + eps);
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            float gamma = w[i];
            float beta = b[i];

            //use MUL rather than DIV.
            float val = (subin[i] - mean) * recip_std;
            subout[i] = val * gamma + beta;
        }
    }
}


void layernormF16(
    uint16_t * out,  uint16_t const*restrict in, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int indim, unsigned int wdim,
    float eps)
{
    XASSERT(indim % wdim == 0);
    XASSERT(wdim != 0);
    XASSERT(indim != 0);
    for (unsigned int ind = 0; ind < indim; ind += wdim) {
        uint16_t const* subin = in + ind;
        uint16_t * subout = out + ind;

        //Compute mean.
        float mean = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            mean += fp16_to_float(subin[i]);
        }
        mean /= (float)wdim;

        //Compute variance.
        float var = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            float diff = fp16_to_float(subin[i]) - mean;
            var += diff * diff;
        }
        var /= (float)wdim;

        //Perform scale and shift.
        float recip_std = 1.0f / sqrtf(var + eps);
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            float gamma = fp16_to_float(w[i]);
            float beta = fp16_to_float(b[i]);
            float val = fp16_to_float(subin[i]);

            //use MUL rather than DIV.
            val = (val - mean) * recip_std;
            val = val * gamma + beta;
            subout[i] = float_to_fp16(val);
        }
    }
}


void layernormBF16(
    uint16_t * out, uint16_t const*restrict in, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int indim, unsigned int wdim,
    float eps)
{
    XASSERT(indim % wdim == 0);
    XASSERT(wdim != 0);
    XASSERT(indim != 0);
    for (unsigned int ind = 0; ind < indim; ind += wdim) {
        uint16_t const* subin = in + ind;
        uint16_t * subout = out + ind;

        //Compute mean.
        float mean = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            mean += bf16_to_float(subin[i]);
        }
        mean /= (float)wdim;

        //Compute variance.
        float var = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < wdim; i++) {
            float diff = bf16_to_float(subin[i]) - mean;
            var += diff * diff;
        }
        var /= (float)wdim;

        //Perform scale and shift.
        float recip_std = 1.0f / sqrtf(var + eps);
        for (unsigned int i = 0; i < wdim; i++) {
            float gamma = bf16_to_float(w[i]);
            float beta = bf16_to_float(b[i]);
            float val = bf16_to_float(subin[i]);
            val = (val - mean) * recip_std; //use MUL rather than DIV.
            val = val * gamma + beta;
            out[i] = float_to_bf16(val);
        }
    }
}


void layernormInplace(
    void * in_out, RTTensor const*restrict w,
    RTTensor const*restrict b, unsigned int dim, float eps)
{
    XASSERT(w->ndim == 1);
    XASSERT(b->ndim == 1);
    unsigned int wdim = w->shape[0];
    XASSERT(b->shape[0] == wdim);
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F32);
        layernormF32Inplace(
            in_out, (float const*)w->data, (float const*)b->data,
            dim, wdim, eps);
        return;
    case RTTENSOR_DTYPE_F16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F16);
        layernormF16Inplace(
            (uint16_t*)in_out, (uint16_t const*)w->data,
            (uint16_t const*)b->data, dim, wdim, eps);
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_BF16);
        layernormBF16Inplace(
            (uint16_t*)in_out, (uint16_t const*)w->data,
            (uint16_t const*)b->data, dim, wdim, eps);
        return;
    default:
        XASSERT(0);
        return;
    }
}


void layernorm(
    void * out, void const*restrict in, RTTensor const*restrict w,
    RTTensor const*restrict b, unsigned int dim, float eps)
{
    XASSERT(w->ndim == 1);
    XASSERT(b->ndim == 1);
    unsigned int wdim = w->shape[0];
    XASSERT(b->shape[0] == wdim);
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F32);
        layernormF32(
            out, in, (float const*)w->data, (float const*)b->data,
            dim, wdim, eps);
        return;
    case RTTENSOR_DTYPE_F16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F16);
        layernormF16(
            out, in, (uint16_t const*)w->data, (uint16_t const*)b->data,
            dim, wdim, eps);
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_BF16);
        layernormBF16(
            out, in, (uint16_t const*)w->data, (uint16_t const*)b->data,
            dim, wdim, eps);
        return;
    default:
        XASSERT(0);
        return;
    }
}
