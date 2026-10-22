#include "ops.h"

float l2normF32(float const* in, int64_t num)
{
    float norm = 0;
    for (int64_t i = 0; i < num; i++) {
        norm += in[i] * in[i];
    }
    return sqrtf(norm);
}
