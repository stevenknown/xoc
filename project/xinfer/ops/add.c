#include "ops.h"

//Element-wise in-place vector add: out[i] = a[i] + b[i]
//a:input float vector A
//b:input float vector B
//out:output float vector, can alias with a or b (inplace supported)
//n:number of elements to process
//All buffers are float32, no type conversion.
void addF32Inplace(float * out, float const* a, float const* b, unsigned int n)
{
    for (unsigned int i = 0; i < n; i++) {
        out[i] = a[i] + b[i];
    }
}


void addF16Inplace(
    uint16_t * out, uint16_t const* a, uint16_t const* b, unsigned int n)
{
    for (unsigned int i = 0; i < n; i++) {
        float ta = fp16_to_float(a[i]);
        float tb = fp16_to_float(b[i]);
        float res = ta + tb;
        out[i] = float_to_fp16(res);
    }
}


void addBF16Inplace(
    uint16_t * out, uint16_t const* a, uint16_t const* b, unsigned int n)
{
    for (unsigned int i = 0; i < n; i++) {
        float ta = bf16_to_float(a[i]);
        float tb = bf16_to_float(b[i]);
        float res = ta + tb;
        out[i] = float_to_bf16(res);
    }
}


void addInplace(
    void * out, void const* a, void const* b, unsigned int n,
    RTTENSOR_DTYPE dtype)
{
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        addF32Inplace((float*)out, (float const*)a, (float const*)b, n);
        return;
    case RTTENSOR_DTYPE_F16:
        addF16Inplace(
            (uint16_t*)out, (uint16_t const*)a, (uint16_t const*)b, n);
        return;
    case RTTENSOR_DTYPE_BF16:
        addBF16Inplace(
            (uint16_t*)out, (uint16_t const*)a, (uint16_t const*)b, n);
        return;
    default: XASSERT(0); return;
    }
}
