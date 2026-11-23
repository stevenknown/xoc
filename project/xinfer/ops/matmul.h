#ifdef __cplusplus
extern "C" {
#endif

void matmulTransBF16(
    uint16_t * out, uint16_t const* in, unsigned int inrow, unsigned int incol,
    RTTensor const* w);
void matmulTransF16(
    uint16_t * out, uint16_t const* in, unsigned int inrow, unsigned int incol,
    RTTensor const* w);

//Vector-matrix multiply plus bias, batch=1 (single token decode)
//out:output vector [out_dim]
//    out = b + in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
//in:input vector [in_dim]
//w:weight matrix [wrow*wcol], row-major
//b:bias [1*wrow], can NOT be NULL.
//wcol: number of columns of weight matrix, the in_dim of input vector dim.
//wrow: number of rows of weight matrix, the out_dim of output vector dim.
//NOTE:out shall not overlap with in and w buffer.
//NOTE:Weight can only be F32.
void matmulTransF32Bias(
    float * out, float const* in, unsigned int inrow, unsigned int incol,
    float const* weight, float const* b, unsigned int wrow, unsigned int wcol);

void matmulF32Bias(
    float * out, float const* in, unsigned int inrow, unsigned int incol,
    float const* weight, float const* b, unsigned int wrow, unsigned int wcol);

//Vector-matrix multiply plus bias, batch=1 (single token decode)
//out:output vector [out_dim]
//    out = b + in * W, namely, [1*wcol]+[1*wrow]x[wrow*wcol]-->[1*wcol]
//in:input vector [in_dim]
//w:weight matrix [wrow*wcol], row-major
//b:bias [1*wcol], can NOT be NULL.
//wcol: number of columns of weight matrix, the in_dim of input vector dim.
//wrow: number of rows of weight matrix, the out_dim of output vector dim.
//NOTE:out shall not overlap with in and w buffer.
//NOTE:Weight can only be F32.
void matmulBias(
    void * out, void const* in, unsigned int inrow, unsigned int incol,
    RTTensor const* weight, RTTensor const* b);

//Vector-matrix multiply: out[1*wrow] =
//  in[1*wcol] * (weight[wrow*wcol]^T)
//out:output float32 buffer, size = wrow, store token logits
//    out = in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
//in:input float32 vector, size = wcol
//w:weight tensor, shape=[wrow, wcol],
//  support only dtype F32
//wcol:in feature dimension (number of columns of weight matrix weight)
//wrow:vocabulary size (number of rows of weight matrix weight)
//Return 0 for success, negative for error.
//
//in/out are always float32.
//NOTE:Weight weight can only be F32.
int matmulTransF32(
    float * out, float const* in, unsigned int inrow, unsigned int incol,
    RTTensor const* w);

//Vector-matrix multiply: out[1*wrow] =
//  in[1*wcol] * (weight[wrow*wcol]^T)
//out:output float32 buffer, size = wrow, store token logits
//    out = in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
//in:input float32 vector, size = wcol
//weight:weight tensor, shape=[wrow, wcol],
//    support only dtype F32
//wcol:in feature dimension (number of columns of weight matrix weight)
//wrow:vocabulary size (number of rows of weight matrix weight)
//Return 0 for success, negative for error.
//
//i  out are always float32.
//NOTE:Weight weight can only be F32.
int matmulTransF32Ptr(
    float * out, float const* in, unsigned int inrow, unsigned int incol,
    float const* weight, unsigned int wrow, unsigned int wcol);

//Vector-matrix multiply: out[1*vocab_size] =
//  in[1*n_embd] * (weight[vocab_size*n_embd]^T)
//out:output float32 buffer, size = vocab_size, store token logits
//    out = in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
//in:input float32 vector, size = n_embd
//weight:weight tensor, shape=[vocab_size, n_embd],
//    support dtype:F32/F16/BF16
//    wcol:in feature dimension (number of columns of weight matrix)
//    wrow:vocabulary size (number of rows of weight matrix)
//in/out are always float32.
//Weight weight can be f32/fp16/bf16, convert element to float32 on-the-fly.
void matmulTrans(
    void * out, void const* in, unsigned int inrow, unsigned int incol,
    RTTensor const* weight);
void matmulTransBias(
    void * out, void const* in, unsigned int inrow, unsigned int incol,
    RTTensor const* weight, RTTensor const* bias);

#ifdef __cplusplus
}
#endif
