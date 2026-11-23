#ifdef __cplusplus
extern "C" {
#endif

//SiLU activation, in-place.
//inout: input/output buffer
//elemnum_in_group:number of elements
void silu(
    void * inout, unsigned int groupnum,
    unsigned int elemnum_in_group,  RTTENSOR_DTYPE dtype);

//Fast sigmoid approx:1/(1+exp(-x)) is appro= 0.5*(1+tanhf(0.5*x)).
void siluF32Appro(float * inout, unsigned int len);

#ifdef __cplusplus
}
#endif
