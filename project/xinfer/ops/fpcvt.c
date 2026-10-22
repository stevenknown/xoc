#include "ops.h"

float fp16_to_float(uint16_t val)
{
    uint32_t sign = (val >> 15U) & 0x1U;
    uint32_t exponent = (val >> 10U) & 0x1FU;
    uint32_t mantissa = val & 0x3FFU;
    uint32_t f32_bits;
    if (exponent == 0U) {
        if (mantissa == 0U) {
            //zero value
            f32_bits = (sign << 31U);
        } else {
            //denormal fp16, normalize to f32 normal form
            int shift = 0;
            uint32_t m = mantissa;
            while ((m & 0x400U) == 0U) {
                m <<= 1U;
                shift++;
            }
            mantissa = (m & 0x3FFU) << 13U;
            exponent = 1U - shift + 127U - 15U;
            f32_bits = (sign << 31U) | (exponent << 23U) | mantissa;
        }
    } else if (exponent == 0x1FU) {
        //inf/nan
        f32_bits = (sign << 31U) | (0xFFU << 23U) | (mantissa << 13U);
    } else {
        //normal fp16 value
        exponent = exponent - 15U + 127U;
        mantissa = mantissa << 13U;
        f32_bits = (sign << 31U) | (exponent << 23U) | mantissa;
    }
    float res;
    uint32_t * p = (uint32_t*)&res;
    *p = f32_bits;
    return res;
}


float bf16_to_float(uint16_t b16)
{
    float res;
    uint32_t * fbits = (uint32_t*)&res;
    *fbits = ((uint32_t)b16) << 16U;
    return res;
}


float tofloat(void const* val, RTTENSOR_DTYPE dtype)
{
    switch (dtype) {
    case RTTENSOR_DTYPE_F32: return *(float const*)val;
    case RTTENSOR_DTYPE_F16: return fp16_to_float(*(uint16_t const*)val);
    case RTTENSOR_DTYPE_BF16: return bf16_to_float(*(uint16_t const*)val);
    default: XASSERT(0); //TODO:
    }
    return 0;
}


uint16_t float_to_bf16(float f)
{
    uint32_t u32 = *(uint32_t const*)&f;
    return (uint16_t)(u32 >> 16U);
}


uint16_t float_to_fp16(float f)
{
    uint32_t uintv = *(uint32_t const*)&f;
    uint32_t sign = (uintv >>31U) &1U;
    uint32_t exp = (uintv >>23U) &0xFFU;
    uint32_t mant = uintv &0x7FFFFFU;
    uint16_t half;
    int32_t new_exp = (int32_t)exp - 127 + 15;
    if(exp == 0xFFU) {
        //inf|nan
        half = (uint16_t)((sign <<15U) | 0x7C00U | (mant>>13U));
    } else if (new_exp >= 0x1FU) {
        //overflow to inf
        half = (uint16_t)((sign <<15U) | 0x7C00U);
    } else if (new_exp <=0) {
        //subnormal|underflow
        mant |= 0x800000U;
        mant = mant >> (1 - new_exp);
        half = (uint16_t)((sign <<15U) | (mant>>13U));
    } else {
        //normal, simple truncate
        half = (uint16_t)((sign <<15U) | (((uint32_t)new_exp)<<10U) |
               (mant>>13U));
    }
    return half;
}
