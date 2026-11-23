#include "ops.h"

void matmulTransF32Bias(
    float *restrict out, float const*restrict in,
    unsigned int inrow, unsigned int incol,
    float const*restrict weight, float const*restrict b,
    unsigned int wrow, unsigned int wcol)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(wcol > 0);
    XASSERT(wrow > 0);
    XASSERT(incol == wcol);
    unsigned int in_elemnum = inrow * incol;
    for (unsigned int ind = 0, outd = 0;
         ind < in_elemnum; ind += incol, outd += wrow) {
        float const* subin = in + ind;
        float * subout = out + outd;

        //out = b + in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
        //NOTE:Usually, the number of rows are far greater than the number
        //of columns, such as [50527*768].
        //w's layout:[wrow*wcol], rowmajor matrix.
        for (unsigned int i = 0; i < wrow; i++) {
            float sum = b[i]; //start with bias.

            //Array version.
            float const* row_of_w = weight + i * wcol;
            HINT_VECT
            for (unsigned int j = 0; j < wcol; j++) {
                //in * W^T
                sum = sum + subin[j] * row_of_w[j];
            }
            subout[i] = sum;
        }
    }
}


void matmulTransF16Bias(
    uint16_t *restrict out, uint16_t const*restrict in,
    unsigned int inrow, unsigned int incol,
    uint16_t const*restrict weight, uint16_t const*restrict b,
    unsigned int wrow, unsigned int wcol)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(wcol > 0);
    XASSERT(wrow > 0);
    XASSERT(incol == wcol);
    unsigned int in_elemnum = inrow * incol;
    for (unsigned int ind = 0, outd = 0;
         ind < in_elemnum; ind += incol, outd += wrow) {
        uint16_t const* subin = in + ind;
        uint16_t * subout = out + outd;

        //out = b + in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
        //NOTE:Usually, the number of rows are far greater than the number
        //of columns, such as [50527*768].
        //w's layout:[wrow*wcol], rowmajor matrix.
        for (unsigned int i = 0; i < wrow; i++) {
            float sum = fp16_to_float(b[i]); //start with bias.

            uint16_t const* row_of_w = weight + i * wcol;
            //NOTE:float-conversion might prevent auto-vect.
            HINT_VECT
            for (unsigned int j = 0; j < wcol; j++) {
                //in * W^T
                float wval = fp16_to_float(row_of_w[j]);
                sum = sum + fp16_to_float(subin[j]) * wval;
            }
            subout[i] = float_to_fp16(sum);
        }
    }
}


void matmulTransBF16Bias(
    uint16_t *restrict out, uint16_t const*restrict in,
    unsigned int inrow, unsigned int incol,
    uint16_t const*restrict weight, uint16_t const*restrict b,
    unsigned int wrow, unsigned int wcol)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(wcol > 0);
    XASSERT(wrow > 0);
    XASSERT(incol == wcol);
    unsigned int in_elemnum = inrow * incol;
    for (unsigned int ind = 0, outd = 0;
         ind < in_elemnum; ind += incol, outd += wrow) {
        uint16_t const* subin = in + ind;
        uint16_t * subout = out + outd;

        //out = b + in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
        //NOTE:Usually, the number of rows are far greater than the number
        //of columns, such as [50527*768].
        //w's layout:[wrow*wcol], rowmajor matrix.
        for (unsigned int i = 0; i < wrow; i++) {
            float sum = bf16_to_float(b[i]); //start with bias.

            //Array version.
            uint16_t const* row_of_w = weight + i * wcol;
            HINT_VECT
            for (unsigned int j = 0; j < wcol; j++) {
                //in * W^T
                float wval = bf16_to_float(row_of_w[j]);
                sum = sum + bf16_to_float(subin[j]) * wval;
            }
            subout[i] = float_to_bf16(sum);
        }
    }
}


static void matmulF32BiasClassic(
    float *restrict out, float const*restrict in,
    unsigned int inrow, unsigned int incol, float const*restrict weight,
    float const*restrict b, unsigned int wrow, unsigned int wcol)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(incol > 0);
    XASSERT(inrow > 0);
    XASSERT(wcol > 0);
    XASSERT(wrow > 0);
    XASSERT(incol == wrow);
    for (unsigned int ind = 0, outd = 0;
         ind < inrow; ind += incol, outd += wcol) {
        float const* subin = in + ind;
        float * subout = out + outd;

        //subout = b + subin * W, namely, [1*wrow]x[wrow*wcol]-->[1*wcol]
        //w's layout:[wrow*wcol], row-major matrix.
        for (unsigned int i = 0; i < wcol; i++) {
            float sum = b[i]; //start with bias.

            //Array version.
            HINT_VECT
            for (unsigned int j = 0; j < wrow; j++) {
                //in * W
                //NOTE:Since weight is stored in row-major order in memory,
                //the elements that accessed via weight[j*wcol + i] are not
                //contiguous. Non-one-stride might prevent auto-vect.
                sum = sum + subin[j] * weight[j * wcol + i];
            }
            subout[i] = sum;
        }
    }
}


static void matmulF32BiasConsiderMemContiguous(
    float *restrict out, float const*restrict in,
    unsigned int inrow, unsigned int incol, float const*restrict weight,
    float const*restrict b, unsigned int wrow, unsigned int wcol)
{
    XASSERT(out);
    XASSERT(in);
    XASSERT(weight);
    XASSERT(b);
    XASSERT(incol > 0);
    XASSERT(inrow > 0);
    XASSERT(wcol > 0);
    XASSERT(wrow > 0);
    XASSERT(incol == wrow);
    unsigned int elemsz = sizeof(float);
    unsigned int out_elemnum = inrow * wcol;
    for (unsigned int outd = 0; outd < out_elemnum; outd += wcol) {
        float * subout = out + outd;
        memcpy(subout, b, wcol * elemsz);
    }
    unsigned int in_elemnum = inrow * incol;
    for (unsigned int ind = 0, outd = 0;
         ind < in_elemnum; ind += incol, outd += wcol) {
        float const* subin = in + ind;
        float * subout = out + outd;
        for (unsigned i = 0; i < incol; i++) {
            float subinval = subin[i];
            float const* subweight = weight + i * wcol;
            HINT_VECT
            for (unsigned j = 0; j < wcol; j++) {
                subout[j] += subinval * subweight[j];
            }
        }
    }
}


void matmulF32Bias(
    float *restrict out, float const*restrict in,
    unsigned int inrow, unsigned int incol, float const*restrict weight,
    float const*restrict b, unsigned int wrow, unsigned int wcol)
{
    bool const expect_vect = true;
    if (expect_vect) { //should be optimized by RedundantBranchElim.
        matmulF32BiasConsiderMemContiguous(
            out, in, inrow, incol, weight, b, wrow, wcol);
        return;
    }
    matmulF32BiasClassic(out, in, inrow, incol, weight, b, wrow, wcol);
}


void matmulBias(
    void *restrict out, void const*restrict in,
    unsigned int inrow, unsigned int incol, RTTensor const*restrict w,
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
            (float*)out, (float const*)in, inrow, incol,
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
    unsigned int inrow, unsigned int incol,
    float const*restrict weight, unsigned int wrow, unsigned int wcol)
{
    XASSERT(out && in && weight);
    XASSERT(incol == wcol);
    float const* subin = in;
    float * subout = out;
    for (unsigned ind = 0; ind < inrow; ind++, subin += incol, subout += wrow) {
        //out = in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
        //Iterate each token (each row in weight matrix).
        for (unsigned int i = 0; i < wrow; i++) {
            float sum = 0.0f;
   
            //Element offset for row i: i * wcol.
            //Byte offset for row i: i * wcol * sizeof(float).
            unsigned int row_base = i * wcol;
    
            //Array version.
            float const* row_of_w = weight + row_base;
            HINT_VECT
            for (unsigned int j = 0; j < wcol; j++) {
                sum = sum + subin[j] * row_of_w[j];
            }

            //Pointer version, may be helpful to scalar optimization but
            //may prevent auto-vect.
            //for (unsigned int j = 0; j < wcol; j++) {
            //    unsigned int elem_off = row_base + j;
            //    float weight_val = *(weight + elem_off);
            //    sum = sum + in[j] * weight_val;
            //}

            subout[i] = sum;
        }
    }
    return 0;
}


int matmulTransF32(
    float*restrict out, const float*restrict in,
    unsigned int inrow, unsigned int incol, RTTensor const*restrict w)
{
    XASSERT(out && in && w);
    XASSERT(w->ndim == 2);
    XASSERT(w->dtype == RTTENSOR_DTYPE_F32);
    unsigned int wrow = w->shape[0];
    unsigned int wcol = w->shape[1];
    return matmulTransF32Ptr(
        out, in, inrow, incol, (float const*)w->data, wrow, wcol);
}


void matmulTransBias(
    void *restrict out, void const*restrict in,
    unsigned int inrow, unsigned int incol,
    RTTensor const*restrict w, RTTensor const*restrict b)
{
    XASSERT(out && in && w && b);
    XASSERT(w->ndim == 2);
    XASSERT(w->data);
    unsigned int wrow = w->shape[0];
    unsigned int wcol = w->shape[1];
    XASSERT(b->ndim == 1);
    XASSERT(b->shape[0] == wrow);
    XASSERT(b->data);
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F32);
        matmulTransF32Bias(
            (float*)out, (float const*)in, inrow, incol,
            (float const*)w->data, (float const*)b->data, wrow, wcol);
        return;
    case RTTENSOR_DTYPE_F16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_F16);
        matmulTransF16Bias(
            (uint16_t*)out, (uint16_t const*)in, inrow, incol,
            (uint16_t const*)w->data, (uint16_t const*)b->data,
            wrow, wcol);
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b->dtype == RTTENSOR_DTYPE_BF16);
        matmulTransBF16Bias(
            (uint16_t*)out, (uint16_t const*)in, inrow, incol,
            (uint16_t const*)w->data, (uint16_t const*)b->data,
            wrow, wcol);
        return;
    default: XASSERT(0); //TODO:
    }
}


void matmulTransF16(
    uint16_t *restrict out, uint16_t const*restrict in,
    unsigned int inrow, unsigned int incol, RTTensor const*restrict w)
{
    XASSERT(out && in && w);
    XASSERT(w->ndim == 2);
    XASSERT(w->dtype == RTTENSOR_DTYPE_F16);
    unsigned int elemsz = sizeof(uint16_t);
    uint16_t const* weight = (uint16_t const*)w->data;
    unsigned int wrow = w->shape[0];
    unsigned int wcol = w->shape[1];
    XASSERT(incol == wcol);
    uint16_t const* subin = in;
    uint16_t * subout = out;
    for (unsigned ind = 0; ind < inrow; ind++, subin += incol, subout += wrow) {
        //out = in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
        //Iterate each token (each row in w matrix).
        for (unsigned int i = 0; i < wrow; i++) {
            float sum = 0.0f;

            //Element offset for row i: i * wcol.
            //Byte offset for row i: i * wcol * elemsz
            unsigned int row_base = i * wcol;

            //NOTE:function-call might prevent auto-vect.
            uint16_t const* weight_row = weight + row_base;
            HINT_VECT
            for (unsigned int j = 0; j < wcol; j++) {
                float w_val = fp16_to_float(weight_row[j]);
                sum = sum + fp16_to_float(subin[j]) * w_val;
            }
            subout[i] = float_to_fp16(sum);
        }
    }
}


void matmulTransBF16(
    uint16_t *restrict out, uint16_t const*restrict in,
    unsigned int inrow, unsigned int incol,
    RTTensor const*restrict w)
{
    XASSERT(out && in && w);
    XASSERT(w->ndim == 2);
    XASSERT(w->dtype == RTTENSOR_DTYPE_BF16);
    unsigned int elemsz = sizeof(uint16_t);
    uint16_t const* weight = (uint16_t const*)w->data;
    unsigned int wrow = w->shape[0];
    unsigned int wcol = w->shape[1];
    XASSERT(incol == wcol);
    uint16_t const* subin = in;
    uint16_t * subout = out;
    for (unsigned ind = 0; ind < inrow; ind++, subin += incol, subout += wrow) {
        //out = in * W^T, namely, [1*wcol]x[wrow*wcol]-->[1*wrow]
        //Iterate each token (each row in w matrix).
        for (unsigned int i = 0; i < wrow; i++) {
            float sum = 0.0f;

            //Element offset for row i: i * wcol.
            //Byte offset for row i: i * wcol * elemsz
            unsigned int row_base = i * wcol;

            //NOTE:function-call might prevent auto-vect.
            uint16_t const* weight_row = weight + row_base;
            HINT_VECT
            for (unsigned int j = 0; j < wcol; j++) {
                float w_val = bf16_to_float(weight_row[j]);
                sum = sum + bf16_to_float(subin[j]) * w_val;
            }
            subout[i] = float_to_bf16(sum);
        }
    }
}


void matmulTrans(
    void *restrict out, void const*restrict in,
    unsigned int inrow, unsigned int incol, RTTensor const*restrict w)
{
    XASSERT(out && in && w);
    XASSERT(w->ndim == 2);
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        matmulTransF32(out, in, inrow, incol, w);
        return;
    case RTTENSOR_DTYPE_F16:
        matmulTransF16(out, in, inrow, incol, w);
        return;
    case RTTENSOR_DTYPE_BF16:
        matmulTransBF16(out, in, inrow, incol, w);
        return;
    default:
        XASSERT(0); //Unsupport
        return; //unsupported dtype
    }
}
