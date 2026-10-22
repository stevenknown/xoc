#include "ops.h"

int64_t argmaxF32(float const* probs, int64_t probslen)
{
    int64_t best_idx = 0;
    float best_val = probs[0];
    for (int64_t i = 1; i < probslen; i++) {
        if (probs[i] > best_val) {
            best_val = probs[i];
            best_idx = i;
        }
    }
    return best_idx;
}


int64_t argmaxF16(uint16_t const* probs, int64_t probslen)
{
    int64_t best_idx = 0;
    float best_val = fp16_to_float(probs[0]);
    for (int64_t i = 1; i < probslen; i++) {
        float val = fp16_to_float(probs[i]);
        if (val > best_val) {
            best_val = val;
            best_idx = i;
        }
    }
    return best_idx;
}


int64_t argmaxBF16(uint16_t const* probs, int64_t probslen)
{
    int64_t best_idx = 0;
    float best_val = bf16_to_float(probs[0]);
    for (int64_t i = 1; i < probslen; i++) {
        float val = bf16_to_float(probs[i]);
        if (val > best_val) {
            best_val = val;
            best_idx = i;
        }
    }
    return best_idx;
}
