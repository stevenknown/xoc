#ifdef __cplusplus
extern "C" {
#endif

//LayerNorm implementation In-place, compute out =
//  (in - mean) / sqrt(var + eps) * weight + bias
//in:input float32 buffer, length = dim
//out:output float32 buffer, length = dim
//w:gamma(scale) tensor, support dtype:
//  RTTENSOR_DTYPE_F32 / RTTENSOR_DTYPE_F16 / RTTENSOR_DTYPE_BF16,
//  must be 1D shape [dim]
//b:beta(bias) tensor, support dtype:
//  RTTENSOR_DTYPE_F32 / RTTENSOR_DTYPE_F16 / RTTENSOR_DTYPE_BF16,
//  must be 1D shape [dim]
//dim:feature dimension size
//eps:epsilon for numerical stability, typical value 1e-5f or 1e-6f
//
//Input 'in' and output 'out' are always float32.
//Weight w / bias b can be f32 / f16 / bf16, will be converted to
//float32 on the fly.
void layernorm(
    float * out, float const* in, RTTensor const* w, RTTensor const* b,
    unsigned int dim, float eps);

void layernormInplace(
    float * in_out, RTTensor const* w, RTTensor const* b,
    unsigned int dim, float eps);

void layernormF32(
    float * out, float const* in, float const* w,
    float const* b, unsigned int dim, float eps);

void layernormF32Inplace(
    float * in_out, float const* w,
    float const* b, unsigned int dim, float eps);

void layernormF16(
    float * out, float const* in, uint16_t const* w,
    uint16_t const* b, unsigned int dim, float eps);

void layernormBF16(
    float * out, float const* in, uint16_t const* w,
    uint16_t const* b, unsigned int dim, float eps);

#ifdef __cplusplus
}
#endif
