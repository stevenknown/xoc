#include "ops.h"

void dotF32Inplace(float * out, float const* a, float const* b, unsigned int n)
{
    for (unsigned int i = 0; i < n; i++) {
        out[i] = a[i] * b[i];
    }
}


void dotF16Inplace(
    uint16_t * out, uint16_t const* a, uint16_t const* b, unsigned int n)
{
    for (unsigned int i = 0; i < n; i++) {
        float fa = fp16_to_float(a[i]);
        float fb = fp16_to_float(b[i]);
        float fc = fa * fb;
        out[i] = float_to_fp16(fc);
    }
}


void dotBF16Inplace(
    uint16_t * out, uint16_t const* a, uint16_t const* b, unsigned int n)
{
    for (unsigned int i = 0; i < n; i++) {
        float fa = bf16_to_float(a[i]);
        float fb = bf16_to_float(b[i]);
        float fc = fa * fb;
        out[i] = float_to_bf16(fc);
    }
}


void dotInplace(
    void * out, void const* a, void const* b, unsigned int n,
    RTTENSOR_DTYPE dtype)
{
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        dotF32Inplace(out, a, b, n);
        return;
    case RTTENSOR_DTYPE_F16:
        dotF16Inplace(out, a, b, n);
        return;
    case RTTENSOR_DTYPE_BF16:
        dotBF16Inplace(out, a, b, n);
        return;
    default:
        XASSERT(0);
        return;
    }
}
