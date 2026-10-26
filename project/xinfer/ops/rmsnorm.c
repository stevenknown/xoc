#include "ops.h"

void rmsnormF32(
    float * out, float const*restrict in, float const*restrict w,
    float const*restrict b, unsigned int num_group, unsigned int dim,
    float eps)
{
    for (unsigned int k = 0; k < num_group; k++) {
        float const* inb = in + k * dim;
        float * outb = out + k * dim;

        //sum of squares.
        float sum = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < dim; i++) {
            float val = inb[i];
            sum = sum + val * val;
        }

        //rms and reciprocal sqrt.
        float rms = sum / dim;
        rms = rms + eps;
        float recip_rms = 1.0f / sqrtf(rms);

        //scale x and multiply weight.
        if (b != NULL) {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                outb[i] = inb[i] * recip_rms * w[i] + b[i];
            }
        } else {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                outb[i] = inb[i] * recip_rms * w[i];
            }
        }
    }
}


void rmsnormF32Inplace(
    float * in_out, float const*restrict w,
    float const*restrict b, unsigned int num_group, unsigned int dim,
    float eps)
{
    for (unsigned int k = 0; k < num_group; k++) {
        float * inoutb = in_out + k * dim;

        //sum of squares.
        float sum = 0.0f;
        for (unsigned int i = 0; i < dim; i++) {
            float val = inoutb[i];
            sum = sum + val * val;
        }

        //rms and reciprocal sqrt.
        float rms = sum / dim;
        rms = rms + eps;
        float recip_rms = 1.0f / sqrtf(rms);

        //scale x and multiply weight.
        if (b != NULL) {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                inoutb[i] = inoutb[i] * recip_rms * w[i] + b[i];
            }
        } else {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                inoutb[i] = inoutb[i] * recip_rms * w[i];
            }
        }
    }
}


void rmsnormF16(
    uint16_t * out, uint16_t const*restrict in, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int num_group, unsigned int dim,
    float eps)
{
    for (unsigned int k = 0; k < num_group; k++) {
        uint16_t const* inb = in + k * dim;
        uint16_t * outb = out + k * dim;

        //sum‑of‑squares
        float sum_sq = 0.0f;
        for (unsigned int i = 0; i < dim; i++) {
            float val = fp16_to_float(inb[i]);
            sum_sq += val * val;
        }

        float rms = sum_sq / dim;
        rms += eps;
        float recip_rms = 1.0f / sqrtf(rms);

        //compute in fp32, convert back to bf16 for store
        if (b != NULL) {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                float x = fp16_to_float(inb[i]);
                float gw = fp16_to_float(w[i]);
                float bb = fp16_to_float(b[i]);
                float res = x * recip_rms * gw + bb;
                outb[i] = float_to_fp16(res);
            }
        } else {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                float x = bf16_to_float(inb[i]);
                float gw = bf16_to_float(w[i]);
                float res = x * recip_rms * gw;
                outb[i] = float_to_fp16(res);
            }
        }
    }
}


void rmsnormF16Inplace(
    uint16_t * in_out, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int num_group, unsigned int dim,
    float eps)
{
    for (unsigned int k = 0; k < num_group; k++) {
        uint16_t * inoutb = in_out + k * dim;

        //sum‑of‑squares
        float sum = 0.0f;
        for (unsigned int i = 0; i < dim; i++) {
            float val = fp16_to_float(inoutb[i]);
            sum += val * val;
        }

        float rms = sum / dim;
        rms += eps;
        float recip_rms = 1.0f / sqrtf(rms);

        //compute in fp32, convert back to bf16 for store
        if (b != NULL) {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                float x = fp16_to_float(inoutb[i]);
                float gw = fp16_to_float(w[i]);
                float bb = fp16_to_float(b[i]);
                float res = x * recip_rms * gw + bb;
                inoutb[i] = float_to_fp16(res);
            }
        } else {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                float x = bf16_to_float(inoutb[i]);
                float gw = bf16_to_float(w[i]);
                float res = x * recip_rms * gw;
                inoutb[i] = float_to_fp16(res);
            }
        }
    }
}


void rmsnormBF16Inplace(
    uint16_t * in_out, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int num_group, unsigned int dim,
    float eps)
{
    for (unsigned int k = 0; k < num_group; k++) {
        uint16_t * inoutb = in_out + k * dim;

        //sum‑of‑squares
        float sum = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < dim; i++) {
            float val = bf16_to_float(inoutb[i]);
            sum += val * val;
        }

        float rms = sum / dim;
        rms += eps;
        float recip_rms = 1.0f / sqrtf(rms);

        //compute in fp32, convert back to bf16 for store
        if (b != NULL) {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                float x = bf16_to_float(inoutb[i]);
                float gw = bf16_to_float(w[i]);
                float bb = bf16_to_float(b[i]);
                float res = x * recip_rms * gw + bb;
                inoutb[i] = float_to_bf16(res);
            }
        } else {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                float x = bf16_to_float(inoutb[i]);
                float gw = bf16_to_float(w[i]);
                float res = x * recip_rms * gw;
                inoutb[i] = float_to_bf16(res);
            }
        }
    }
}


void rmsnormBF16(
    uint16_t * out, uint16_t const*restrict in, uint16_t const*restrict w,
    uint16_t const*restrict b, unsigned int num_group, unsigned int dim,
    float eps)
{
    for (unsigned int k = 0; k < num_group; k++) {
        uint16_t const* inb = in + k * dim;
        uint16_t * outb = out + k * dim;

        //sum‑of‑squares
        float sum_sq = 0.0f;
        HINT_VECT
        for (unsigned int i = 0; i < dim; i++) {
            float val = bf16_to_float(inb[i]);
            sum_sq += val * val;
        }

        float rms = sum_sq / dim;
        rms += eps;
        float recip_rms = 1.0f / sqrtf(rms);

        //compute in fp32, convert back to bf16 for store
        if (b != NULL) {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                float x = bf16_to_float(inb[i]);
                float gw = bf16_to_float(w[i]);
                float bb = bf16_to_float(b[i]);
                float res = x * recip_rms * gw + bb;
                outb[i] = float_to_bf16(res);
            }
        } else {
            HINT_VECT
            for (unsigned int i = 0; i < dim; i++) {
                float x = bf16_to_float(inb[i]);
                float gw = bf16_to_float(w[i]);
                float res = x * recip_rms * gw;
                outb[i] = float_to_bf16(res);
            }
        }
    }
}


void rmsnormInplace(
    void * in_out, RTTensor const*restrict w,
    RTTensor const*restrict b, unsigned int num_group, unsigned int dim,
    float eps)
{
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        XASSERT(b != NULL ? b->dtype == RTTENSOR_DTYPE_F32 : 1);
        rmsnormF32Inplace(
            (float*)in_out, (float const*)w->data,
            b != NULL ? (float const*)b->data : NULL,
            num_group, dim, eps);
        return;
    case RTTENSOR_DTYPE_F16:
        XASSERT(b != NULL ? b->dtype == RTTENSOR_DTYPE_F16 : 1);
        rmsnormF16Inplace(
            (uint16_t*)in_out, (uint16_t const*)w->data,
            b != NULL ? (uint16_t const*)b->data : NULL,
            num_group, dim, eps);
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b != NULL ? b->dtype == RTTENSOR_DTYPE_BF16 : 1);
        rmsnormBF16Inplace(
            (uint16_t*)in_out, (uint16_t const*)w->data,
            b != NULL ? (uint16_t const*)b->data : NULL,
            num_group, dim, eps);
        return;
    default:
        XASSERT(0);
        return;
    }
}


void rmsnorm(
    void * out, void const*restrict in, RTTensor const*restrict w,
    RTTensor const*restrict b, unsigned int num_group, unsigned int dim,
    float eps)
{
    switch (w->dtype) {
    case RTTENSOR_DTYPE_F32:
        XASSERT(b != NULL ? b->dtype == RTTENSOR_DTYPE_F32 : 1);
        rmsnormF32(
            (float*)out, (float const*)in, (float const*)w->data,
            b != NULL ? (float const*)b->data : NULL,
            num_group, dim, eps);
        return;
    case RTTENSOR_DTYPE_F16:
        XASSERT(b != NULL ? b->dtype == RTTENSOR_DTYPE_F16 : 1);
        rmsnormF16(
            (uint16_t*)out, (uint16_t const*)in, (uint16_t const*)w->data,
            b != NULL ? (uint16_t const*)b->data : NULL,
            num_group, dim, eps);
        return;
    case RTTENSOR_DTYPE_BF16:
        XASSERT(b != NULL ? b->dtype == RTTENSOR_DTYPE_BF16 : 1);
        rmsnormBF16(
            (uint16_t*)out, (uint16_t const*)in, (uint16_t const*)w->data,
            b != NULL ? (uint16_t const*)b->data : NULL,
            num_group, dim, eps);
        return;
    default:
        XASSERT(0);
        return;
    }
}
