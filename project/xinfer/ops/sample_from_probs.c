#include "ops.h"

#include <stdint.h>
#include <stdbool.h>
#include <math.h>

//pcg32 random.
static uint64_t pcg_state = 0x853c49e6748fea9bULL;
static const uint64_t pcg_inc = 0xda3e39cb94b95bdbULL;

static uint32_t pcg32_random()
{
    uint64_t oldstate = pcg_state;
    pcg_state = oldstate * 6364136223846793005ULL + pcg_inc;
    uint32_t xorshifted = (uint32_t)(((oldstate >> 18u) ^ oldstate) >> 27u);
    uint32_t rot = oldstate >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}


//Seed pcg random generator, call once at program startup
//seed:seed value
void pcg32_seed(uint64_t seed)
{
    pcg_state = seed + pcg_inc;
    pcg32_random();
}


//Generate double in [0.0 , 1.0).
static double pcg32_double()
{
    uint32_t r = pcg32_random();
    return (double)r / (double)UINT32_MAX;
}


int64_t sample_from_probs_f32(float const* probs, int64_t vocab_size, float eps)
{
    XASSERT(probs && vocab_size > 0);
    double total_sum = 0.0;

    //Firstly, calculate the total sum of valid probabilities.
    for (int64_t i = 0; i < vocab_size; i++) {
        float p = probs[i];
        if (p > 0.0f) {
            total_sum = total_sum + (double)p;
        }
    }
    if (total_sum < eps) { //1e-12
        //All probabilities are almost zero, no valid token.
        return INVALID_TOKEN;
    }
    //Random value in [0 , total_sum]
    double r = pcg32_double() * total_sum;
    double cum_sum = 0.0;
    for (int64_t i = 0; i < vocab_size; i++) {
        cum_sum += (double)probs[i];
        if (r <= cum_sum) {
            return i;
        }
    }
    //Floating point edge case fallback
    return INVALID_TOKEN;
}


int64_t sample_from_probs_f16(
    uint16_t const* probs, int64_t vocab_size, float eps)
{
    XASSERT(probs && vocab_size > 0);
    double total_sum = 0.0;

    //Firstly, calculate the total sum of valid probabilities.
    for (int64_t i = 0; i < vocab_size; i++) {
        float p = fp16_to_float(probs[i]);
        if (p > 0.0f) {
            total_sum = total_sum + (double)p;
        }
    }
    if (total_sum < eps) { //1e-12
        //All probabilities are almost zero, no valid token.
        return INVALID_TOKEN;
    }
    //Random value in [0 , total_sum]
    double r = pcg32_double() * total_sum;
    double cum_sum = 0.0;
    for (int64_t i = 0; i < vocab_size; i++) {
        float val = fp16_to_float(probs[i]);
        cum_sum += (double)val;
        if (r <= cum_sum) {
            return i;
        }
    }
    //Floating point edge case fallback
    return INVALID_TOKEN;
}


int64_t sample_from_probs_bf16(
    uint16_t const* probs, int64_t vocab_size, float eps)
{
    XASSERT(probs && vocab_size > 0);
    double total_sum = 0.0;

    //Firstly, calculate the total sum of valid probabilities.
    for (int64_t i = 0; i < vocab_size; i++) {
        float p = bf16_to_float(probs[i]);
        if (p > 0.0f) {
            total_sum = total_sum + (double)p;
        }
    }
    if (total_sum < eps) { //1e-12
        //All probabilities are almost zero, no valid token.
        return INVALID_TOKEN;
    }
    //Random value in [0 , total_sum]
    double r = pcg32_double() * total_sum;
    double cum_sum = 0.0;
    for (int64_t i = 0; i < vocab_size; i++) {
        float val = bf16_to_float(probs[i]);
        cum_sum += (double)val;
        if (r <= cum_sum) {
            return i;
        }
    }
    //Floating point edge case fallback
    return INVALID_TOKEN;
}

