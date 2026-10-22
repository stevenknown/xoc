#ifdef __cplusplus
extern "C" {
#endif

void matmulTransBF16(uint16_t * out, uint16_t const* in, RTTensor const* w);
void matmulTransF16(uint16_t * out, uint16_t const* in, RTTensor const* w);

//Vector-matrix multiply plus bias, batch=1 (single token decode)
//out:output vector [out_dim]
//    out = b + in * W^T, namely, [1*n_col]x[n_row*n_col]-->[1*n_row]
//in:input vector [in_dim]
//w:weight matrix [n_row*n_col], row-major
//b:bias [1*n_row], can NOT be NULL.
//n_col: number of columns of weight matrix, the in_dim of input vector dim.
//n_row: number of rows of weight matrix, the out_dim of output vector dim.
//NOTE:out shall not overlap with in and w buffer.
//NOTE:Weight can only be F32.
void matmulTransF32Bias(
    float * out, float const* in, float const* weight,
    float const* b, unsigned int n_row, unsigned int n_col);

//Vector-matrix multiply plus bias, batch=1 (single token decode)
//out:output vector [out_dim]
//    out = b + in * W, namely, [1*n_col]+[1*n_row]x[n_row*n_col]-->[1*n_col]
//in:input vector [in_dim]
//w:weight matrix [n_row*n_col], row-major
//b:bias [1*n_col], can NOT be NULL.
//n_col: number of columns of weight matrix, the in_dim of input vector dim.
//n_row: number of rows of weight matrix, the out_dim of output vector dim.
//NOTE:out shall not overlap with in and w buffer.
//NOTE:Weight can only be F32.
void matmulF32Bias(
    float * out, float const* in, float const* weight,
    float const* b, unsigned int n_row, unsigned int n_col);

void matmulBias(
    void * out, void const* in, RTTensor const* weight, RTTensor const* b);

//Vector-matrix multiply: out[1*n_row] =
//  in[1*n_col] * (weight[n_row*n_col]^T)
//out:output float32 buffer, size = n_row, store token logits
//    out = in * W^T, namely, [1*n_col]x[n_row*n_col]-->[1*n_row]
//in:input float32 vector, size = n_col
//w:weight tensor, shape=[n_row, n_col],
//  support only dtype F32
//n_col:in feature dimension (number of columns of weight matrix weight)
//n_row:vocabulary size (number of rows of weight matrix weight)
//Return 0 for success, negative for error.
//
//in/out are always float32.
//NOTE:Weight weight can only be F32.
int matmulTransF32(float * out, float const* in, RTTensor const* w);

//Vector-matrix multiply: out[1*n_row] =
//  in[1*n_col] * (weight[n_row*n_col]^T)
//out:output float32 buffer, size = n_row, store token logits
//    out = in * W^T, namely, [1*n_col]x[n_row*n_col]-->[1*n_row]
//in:input float32 vector, size = n_col
//weight:weight tensor, shape=[n_row, n_col],
//    support only dtype F32
//n_col:in feature dimension (number of columns of weight matrix weight)
//n_row:vocabulary size (number of rows of weight matrix weight)
//Return 0 for success, negative for error.
//
//i  out are always float32.
//NOTE:Weight weight can only be F32.
int matmulTransF32Ptr(
    float * out, float const* in,
    float const* weight, unsigned int n_row, unsigned int n_col);

//Vector-matrix multiply: out[1*vocab_size] =
//  in[1*n_embd] * (weight[vocab_size*n_embd]^T)
//out:output float32 buffer, size = vocab_size, store token logits
//    out = in * W^T, namely, [1*n_col]x[n_row*n_col]-->[1*n_row]
//in:input float32 vector, size = n_embd
//weight:weight tensor, shape=[vocab_size, n_embd],
//    support dtype:F32/F16/BF16
//    n_col:in feature dimension (number of columns of weight matrix)
//    n_row:vocabulary size (number of rows of weight matrix)
//in/out are always float32.
//Weight weight can be f32/fp16/bf16, convert element to float32 on-the-fly.
void matmulTrans(
    void * out, void const* in, RTTensor const* weight);
void matmulTransBias(
    void * out, void const* in, RTTensor const* weight, RTTensor const* bias);

#ifdef __cplusplus
}
#endif
