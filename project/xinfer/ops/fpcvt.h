#ifdef __cplusplus
extern "C" {
#endif

//Convert 16bit float16 (IEEE754 halfprecision) to 32bit float
//val:raw uint16_t bit pattern of fp16
//return:corresponding 32bit float value
inline float fp16_to_float(uint16_t val) ATTRIBUTE_USE ATTRIBUTE_INLINE;

//Convert bf16 uint16_t bit pattern to 32-bit float
//b16:input bf16 value stored as uint16_t
//Return converted float32 result
inline float bf16_to_float(uint16_t b16) ATTRIBUTE_USE ATTRIBUTE_INLINE;

//Convert VOID type value to 32-bit float
//val:input, a pointer that points to a value whose dtype is specified
//    by 'dtype'.
//Return converted float32 result
inline float tofloat(
    void const* val, RTTENSOR_DTYPE dtype) ATTRIBUTE_USE ATTRIBUTE_INLINE;

//Convert float to uint16_t bf16.
//Return converted bf16 result
inline uint16_t float_to_bf16(float f) ATTRIBUTE_USE ATTRIBUTE_INLINE;

//Convert float to uint16_t f16.
//Return converted f16 result
inline uint16_t float_to_fp16(float f) ATTRIBUTE_USE ATTRIBUTE_INLINE;
#ifdef __cplusplus
}
#endif
