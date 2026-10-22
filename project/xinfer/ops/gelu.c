#include "ops.h"

void geluF32(float * buf, unsigned int len)
{
    XASSERT(buf);
    XASSERT(len > 0);
    float c1 = 0.7978845608f;
    float c2 = 0.044715f;
    for (unsigned int i = 0; i < len; i++) {
        float x = buf[i];
        float x3 = x * x * x;
        float inner = c1 * (x + c2 * x3);
        float tanh_val = tanhf(inner);
        buf[i] = x * 0.5f * (1.0f + tanh_val);
    }
}


void geluF16(uint16_t * buf, unsigned int len)
{
    XASSERT(buf);
    XASSERT(len > 0);
    float c1 = 0.7978845608f;
    float c2 = 0.044715f;
    for (unsigned int i = 0; i < len; i++) {
        float x = fp16_to_float(buf[i]);
        float x3 = x * x * x;
        float inner = c1 * (x + c2 * x3);
        float tanh_val = tanhf(inner);
        x = x * 0.5f * (1.0f + tanh_val);
        buf[i] = float_to_fp16(x);
    }
}


void geluBF16(uint16_t * buf, unsigned int len)
{
    XASSERT(buf);
    XASSERT(len > 0);
    float c1 = 0.7978845608f;
    float c2 = 0.044715f;
    for (unsigned int i = 0; i < len; i++) {
        float x = bf16_to_float(buf[i]);
        float x3 = x * x * x;
        float inner = c1 * (x + c2 * x3);
        float tanh_val = tanhf(inner);
        x = x * 0.5f * (1.0f + tanh_val);
        buf[i] = float_to_bf16(x);
    }
}


void gelu(void * buf, unsigned int len, RTTENSOR_DTYPE dtype)
{
    XASSERT(buf);
    XASSERT(len > 0);
    switch (dtype) {
    case RTTENSOR_DTYPE_F32:
        geluF32(buf, len);
        return;
    case RTTENSOR_DTYPE_F16:
        geluF16(buf, len);
        return;
    case RTTENSOR_DTYPE_BF16:
        geluBF16(buf, len);
        return;
    default:
        XASSERT(0); //Unsupport
        return; //unsupported dtype
    }
}
