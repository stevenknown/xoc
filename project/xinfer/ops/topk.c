#include "ops.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>

typedef struct {
    float prob; //probability
    int64_t idx; //token index
} ProbIndex;

//Qsort compare function:descending sort (big prob first).
static int probindex_cmp(const void* a, const void* b)
{
    ProbIndex const* pa = (ProbIndex const*)a;
    ProbIndex const* pb = (ProbIndex const*)b;
    if (pa->prob > pb->prob) { return -1; }
    if (pa->prob < pb->prob) { return 1; }
    return 0;
}


void topkF16(uint16_t * probs, int64_t vocab_size, int64_t k, float eps)
{
    XASSERT(probs && vocab_size > 0 && k > 0);
    if (k > vocab_size) {
        k = vocab_size;
    }

    int64_t l = sizeof(ProbIndex) * (size_t)vocab_size;
    ProbIndex * pi = (ProbIndex*)malloc(l);
    XASSERT(dump("malloc %u bytes memory,ProbIndex", l));
    XASSERT(pi);

    //Fill probindex pairs.
    for (int64_t i = 0; i < vocab_size; i++) {
        pi[i].prob = fp16_to_float(probs[i]);
        pi[i].idx  = i;
    }

    //Qsort descending by probability.
    qsort(pi, vocab_size, sizeof(ProbIndex), probindex_cmp);

    //Clear all probability to zero.
    //Vectorizable version.
    //for (int64_t i = 0; i < vocab_size; i++) { probs[i] = 0.0f; }
    //FastAPI version.
    memset(probs, 0, sizeof(uint16_t) * vocab_size);

    //Keep topk, accumulate sum.
    float sum = 0.0f;
    for (unsigned int i = 0; i < k; i++) {
        float val = pi[i].prob;
        probs[pi[i].idx] = float_to_fp16(val);
        sum = sum + val;
    }
    //Renormalize.
    if (sum > eps) {
        for (unsigned int i = 0; i < k; i++) {
            float val = fp16_to_float(probs[pi[i].idx]);
            val = val / sum;
            probs[pi[i].idx] = float_to_fp16(val);
        }
    }
    free(pi);
    XASSERT(dump("free %u bytes memory", l));
}


void topkBF16(uint16_t * probs, int64_t vocab_size, int64_t k, float eps)
{
    XASSERT(probs && vocab_size > 0 && k > 0);
    if (k > vocab_size) {
        k = vocab_size;
    }

    int64_t l = sizeof(ProbIndex) * (size_t)vocab_size;
    ProbIndex * pi = (ProbIndex*)malloc(l);
    XASSERT(dump("malloc %u bytes memory,ProbIndex", l));
    XASSERT(pi);

    //Fill probindex pairs.
    for (int64_t i = 0; i < vocab_size; i++) {
        pi[i].prob = bf16_to_float(probs[i]);
        pi[i].idx  = i;
    }

    //Qsort descending by probability.
    qsort(pi, vocab_size, sizeof(ProbIndex), probindex_cmp);

    //Clear all probability to zero.
    //Vectorizable version.
    //for (int64_t i = 0; i < vocab_size; i++) { probs[i] = 0.0f; }
    //FastAPI version.
    memset(probs, 0, sizeof(uint16_t) * vocab_size);

    //Keep topk, accumulate sum.
    float sum = 0.0f;
    for (unsigned int i = 0; i < k; i++) {
        float val = pi[i].prob;
        probs[pi[i].idx] = float_to_bf16(val);
        sum = sum + val;
    }
    //Renormalize.
    if (sum > eps) {
        for (unsigned int i = 0; i < k; i++) {
            float val = bf16_to_float(probs[pi[i].idx]);
            val = val / sum;
            probs[pi[i].idx] = float_to_bf16(val);
        }
    }
    free(pi);
    XASSERT(dump("free %u bytes memory", l));
}


void topkF32(float * probs, int64_t vocab_size, int64_t k, float eps)
{
    XASSERT(probs && vocab_size > 0 && k > 0);
    if (k > vocab_size) {
        k = vocab_size;
    }

    int64_t l = sizeof(ProbIndex) * (size_t)vocab_size;
    ProbIndex * pi = (ProbIndex*)malloc(l);
    XASSERT(dump("malloc %u bytes memory,ProbIndex", l));
    XASSERT(pi);

    //Fill probindex pairs.
    for (int64_t i = 0; i < vocab_size; i++) {
        pi[i].prob = probs[i];
        pi[i].idx  = i;
    }

    //Qsort descending by probability.
    qsort(pi, vocab_size, sizeof(ProbIndex), probindex_cmp);

    //Clear all probability to zero.
    //Vectorizable version.
    //for (int64_t i = 0; i < vocab_size; i++) { probs[i] = 0.0f; }
    //FastAPI version.
    memset(probs, 0, sizeof(float) * vocab_size);

    //Keep topk, accumulate sum.
    float sum = 0.0f;
    for (unsigned int i = 0; i < k; i++) {
        probs[pi[i].idx] = pi[i].prob;
        sum += pi[i].prob;
    }
    //Renormalize.
    if (sum > eps) {
        for (unsigned int i = 0; i < k; i++) {
            probs[pi[i].idx] /= sum;
        }
    }
    free(pi);
    XASSERT(dump("free %u bytes memory", l));
}
