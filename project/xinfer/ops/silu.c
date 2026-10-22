#include "ops.h"

//SiLU activation, in-place.
//inout: input/output buffer
//len:number of elements
void siluF32(float *restrict inout, unsigned int len)
{
    XASSERT(inout);
    for (unsigned int i = 0; i < len; i++) {
        float x = inout[i];
        float sig = 1.0f / (1.0f + expf(-x));
        inout[i] = x * sig;
    }
}


void siluF16(uint16_t *restrict inout, unsigned int len)
{
    XASSERT(inout);
    for (unsigned int i = 0; i < len; i++) {
        float x = fp16_to_float(inout[i]);
        float sig = 1.0f / (1.0f + expf(-x));
        x = x * sig;
        inout[i] = float_to_fp16(x);
    }
}


void siluBF16(uint16_t *restrict inout, unsigned int len)
{
    XASSERT(inout);
    for (unsigned int i = 0; i < len; i++) {
        float x = bf16_to_float(inout[i]);
        float sig = 1.0f / (1.0f + expf(-x));
        x = x * sig;
        inout[i] = float_to_bf16(x);
    }
}


void silu(void *restrict inout, unsigned int len, RTTENSOR_DTYPE dtype)
{
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        siluF32(inout, len);
        return;
    case RTTENSOR_DTYPE_F16:
        siluF16(inout, len);
        return;
    case RTTENSOR_DTYPE_BF16:
        siluBF16(inout, len);
        return;
    default:
        XASSERT(0);
        return;
    }
}


//Fast sigmoid approx:1/(1+exp(-x)) is appro= 0.5*(1+tanhf(0.5*x)).
void siluF32Appro(float *restrict inout, unsigned int len)
{
    XASSERT(inout);
    for (unsigned int i = 0; i < len; i++) {
        float x = inout[i];
        float s = 0.5f * (1.0f + tanhf(0.5f * x));
        inout[i] = x * s;
    }
}
