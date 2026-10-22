#ifdef __cplusplus
extern "C" {
#endif

void addF32Inplace(float* out, float const* a, float const* b, unsigned int n);

void addInplace(
    void * out, void const* a, void const* b, unsigned int n,
    RTTENSOR_DTYPE dtype);

#ifdef __cplusplus
}
#endif
