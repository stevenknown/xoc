#include "ops.h"

void matmulTransF32Bias(
    float *restrict out, float const*restrict in, float const*restrict weight,
    float const*restrict b, unsigned int n_row, unsigned int n_col)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(n_col > 0);
    XASSERT(n_row > 0);

    //out = b + in * W^T, namely, [1*n_col]x[n_row*n_col]-->[1*n_row]
    //NOTE:Usually, the number of rows are far greater than the number
    //of columns, such as [50527*768].
    //w's layout:[n_row*n_col], rowmajor matrix.
    for (unsigned int i = 0; i < n_row; i++) {
        float sum = b[i]; //start with bias.

        //Array version.
        float const* row_of_w = weight + i * n_col;
        for (unsigned int j = 0; j < n_col; j++) {
            //in * W^T
            sum = sum + in[j] * row_of_w[j];
        }
        out[i] = sum;
    }
}


void matmulTransF16Bias(
    uint16_t *restrict out, uint16_t const*restrict in,
    uint16_t const*restrict weight, uint16_t const*restrict b,
    unsigned int n_row, unsigned int n_col)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(n_col > 0);
    XASSERT(n_row > 0);

    //out = b + in * W^T, namely, [1*n_col]x[n_row*n_col]-->[1*n_row]
    //NOTE:Usually, the number of rows are far greater than the number
    //of columns, such as [50527*768].
    //w's layout:[n_row*n_col], rowmajor matrix.
    for (unsigned int i = 0; i < n_row; i++) {
        float sum = fp16_to_float(b[i]); //start with bias.

        //Array version.
        uint16_t const* row_of_w = weight + i * n_col;
        for (unsigned int j = 0; j < n_col; j++) {
            //in * W^T
            float wval = fp16_to_float(row_of_w[j]);
            sum = sum + fp16_to_float(in[j]) * wval;
        }
        out[i] = float_to_fp16(sum);
    }
}


void matmulTransBF16Bias(
    uint16_t *restrict out, uint16_t const*restrict in,
    uint16_t const*restrict weight, uint16_t const*restrict b,
    unsigned int n_row, unsigned int n_col)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(n_col > 0);
    XASSERT(n_row > 0);

    //out = b + in * W^T, namely, [1*n_col]x[n_row*n_col]-->[1*n_row]
    //NOTE:Usually, the number of rows are far greater than the number
    //of columns, such as [50527*768].
    //w's layout:[n_row*n_col], rowmajor matrix.
    for (unsigned int i = 0; i < n_row; i++) {
        float sum = bf16_to_float(b[i]); //start with bias.

        //Array version.
        uint16_t const* row_of_w = weight + i * n_col;
        for (unsigned int j = 0; j < n_col; j++) {
            //in * W^T
            float wval = bf16_to_float(row_of_w[j]);
            sum = sum + bf16_to_float(in[j]) * wval;
        }
        out[i] = float_to_bf16(sum);
    }
}


void matmulF32Bias(
    float *restrict out, float const*restrict in, float const*restrict weight,
    float const*restrict b, unsigned int n_row, unsigned int n_col)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(n_col > 0);
    XASSERT(n_row > 0);

    //out = b + in * W, namely, [1*n_row]x[n_row*n_col]-->[1*n_col]
    //w's layout:[n_row*n_col], row-major matrix.
    for (unsigned int i = 0; i < n_col; i++) {
        float sum = b[i]; //start with bias.

        //Array version.
        for (unsigned int j = 0; j < n_row; j++) {
            //in * W
            //NOTE:Since weight is stored in row-major order in memory, the
            //elements that accessed via weight[j * n_col * i] are not
            //contiguous.
            sum = sum + in[j] * weight[j * n_col + i];
        }
        out[i] = sum;
    }
}


void matmulBias(
    void *restrict out, void const*restrict in, RTTensor const*restrict w,
    RTTensor const*restrict b)
{
    XASSERT(out && in && w && b);
    XASSERT(w->ndim == 2);
    XASSERT(w->data);
    unsigned int nrow = w->shape[0];
    unsigned int ncol = w->shape[1];
    XASSERT(b->ndim == 1);
    XASSERT(b->shape[0] == ncol);
    XASSERT(b->data);
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32: {
        XASSERT(b->dtype == RTTENSOR_DTYPE_F32);
        return matmulF32Bias(
            (float*)out, (float const*)in,
            (float const*)w->data, (float const*)b->data, nrow, ncol);
    }
    case RTTENSOR_DTYPE_F16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F16);
        XASSERT(0); //TODO:
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_BF16);
        XASSERT(0); //TODO:
        return;
    default:
        XASSERT(0); //TODO:
    }
}


int matmulTransF32Ptr(
    float *restrict out, float const*restrict in,
    float const*restrict weight, unsigned int n_row, unsigned int n_col)
{
    XASSERT(out && in && weight);

    //out = in * W^T, namely, [1*n_col]x[n_row*n_col]-->[1*n_row]
    //Iterate each token (each row in weight matrix).
    for (unsigned int i = 0; i < n_row; i++) {
        float sum = 0.0f;

        //Element offset for row i: i * n_col.
        //Byte offset for row i: i * n_col * sizeof(float).
        unsigned int row_base = i * n_col;

        //Array version.
        float const* row_of_w = weight + row_base;
        for (unsigned int j = 0; j < n_col; j++) {
            sum = sum + in[j] * row_of_w[j];
        }

        //Pointer version, may be helpful to scalar optimization but
        //may prevent auto-vect.
        //for (unsigned int j = 0; j < n_col; j++) {
        //    unsigned int elem_off = row_base + j;
        //    float weight_val = *(weight + elem_off);
        //    sum = sum + in[j] * weight_val;
        //}

        out[i] = sum;
    }
    return 0;
}


int matmulTransF32(
    float*restrict out, const float*restrict in, RTTensor const*restrict w)
{
    XASSERT(out && in && w);
    XASSERT(w->ndim == 2);
    XASSERT(w->dtype == RTTENSOR_DTYPE_F32);
    unsigned int n_row = w->shape[0];
    unsigned int n_col = w->shape[1];
    return matmulTransF32Ptr(out, in, (float const*)w->data, n_row, n_col);
}


void matmulTransBias(
    void *restrict out, void const*restrict in, RTTensor const*restrict w,
    RTTensor const*restrict b)
{
    XASSERT(out && in && w && b);
    XASSERT(w->ndim == 2);
    XASSERT(w->data);
    unsigned int nrow = w->shape[0];
    unsigned int ncol = w->shape[1];
    XASSERT(b->ndim == 1);
    XASSERT(b->shape[0] == nrow);
    XASSERT(b->data);
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F32);
        matmulTransF32Bias(
            (float*)out, (float const*)in,
            (float const*)w->data, (float const*)b->data, nrow, ncol);
        return;
    case RTTENSOR_DTYPE_F16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F16);
        matmulTransF16Bias(
            (uint16_t*)out, (uint16_t const*)in,
            (uint16_t const*)w->data, (uint16_t const*)b->data,
            nrow, ncol);
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_BF16);
        matmulTransBF16Bias(
            (uint16_t*)out, (uint16_t const*)in,
            (uint16_t const*)w->data, (uint16_t const*)b->data,
            nrow, ncol);
        return;
    default: XASSERT(0); //TODO:
    }
}


void matmulTransF16(
    uint16_t *restrict out, uint16_t const*restrict in,
    RTTensor const*restrict w)
{
    XASSERT(out && in && w);
    XASSERT(w->ndim == 2);
    XASSERT(w->dtype == RTTENSOR_DTYPE_F16);
    unsigned int elemsz = sizeof(uint16_t);
    XASSERT(w->dtype == RTTENSOR_DTYPE_F16);
    uint8_t const* ptr = (const uint8_t*)w->data;
    unsigned int n_row = w->shape[0];
    unsigned int n_col = w->shape[1];

    //Iterate each token (each row in w matrix).
    for (unsigned int j = 0; j < n_row; j++) {
        float sum = 0.0f;
        //byte offset for row j: j * n_col * elemsz
        unsigned int row_base = j * n_col * elemsz;
        for (unsigned int k = 0; k < n_col; k++) {
            unsigned int elem_off = row_base + k * elemsz;
            float w_val = fp16_to_float(*((uint16_t const*)(ptr + elem_off)));
            sum = sum + fp16_to_float(in[k]) * w_val;
        }
        out[j] = float_to_fp16(sum);
    }
}


void matmulTransBF16(
    uint16_t *restrict out, uint16_t const*restrict in,
    RTTensor const*restrict w)
{
    XASSERT(out && in && w);
    XASSERT(w->ndim == 2);
    unsigned int elemsz = sizeof(uint16_t);
    XASSERT(w->dtype == RTTENSOR_DTYPE_BF16);
    uint8_t const* ptr = (const uint8_t*)w->data;
    unsigned int n_row = w->shape[0];
    unsigned int n_col = w->shape[1];

    //Iterate each token (each row in w matrix).
    for (unsigned int j = 0; j < n_row; j++) {
        float sum = 0.0f;
        //byte offset for row j: j * n_col * elemsz
        unsigned int row_base = j * n_col * elemsz;
        for (unsigned int k = 0; k < n_col; k++) {
            unsigned int elem_off = row_base + k * elemsz;
            float w_val = bf16_to_float(*((uint16_t const*)(ptr + elem_off)));
            sum = sum + bf16_to_float(in[k]) * w_val;
        }
        out[j] = float_to_bf16(sum);
    }
}


void matmulTrans(
    void *restrict out, void const*restrict in, RTTensor const*restrict w)
{
    XASSERT(out && in && w);
    XASSERT(w->ndim == 2);
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        matmulTransF32(out, in, w);
        return;
    case RTTENSOR_DTYPE_F16:
        matmulTransF16(out, in, w);
        return;
    case RTTENSOR_DTYPE_BF16:
        matmulTransBF16(out, in, w);
        return;
    default:
        XASSERT(0); //Unsupport
        return; //unsupported dtype
    }
}
