#ifdef __cplusplus
extern "C" {
#endif

//GELU activate inplace, overwrite input buffer.
//GPT2 approximate gelu:
//  x * 0.5f * (1.0f + tanh(0.7978845608f * (x + 0.044715f * x*x*x)))
//buf:input&output buffer
//len:number of float elements
void geluF32(float * buf, unsigned int len);
void gelu(void * buf, unsigned int len, RTTENSOR_DTYPE dtype);

#ifdef __cplusplus
}
#endif
