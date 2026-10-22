#include "ops.h"

RTCtx * g_rtctx = NULL;

bool dump(char const* format, ...)
{
    if (g_rtctx == NULL) { return true; }
    #ifdef _DEBUG_
    if (!g_rtctx->dump) { return true; }
    #endif
    XASSERT(format);

    #ifdef _DUMP_BY_LOGMGR_
    {
    va_list args;
    va_start(args, format);
    dumpNormInfo(g_rtctx->lm, format, args);
    va_end(args);
    return true;
    }
    #endif

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
    return true;
}


bool dump_f32vec(float const* h, unsigned int n, char const* format, ...)
{
    if (g_rtctx == NULL) { return true; }
    #ifdef _DEBUG_
    if (!g_rtctx->dump_vec) { return true; }
    #endif

    #ifdef _DUMP_BY_LOGMGR_
    {
    va_list args;
    va_start(args, format);
    dumpF32Vec(g_rtctx->lm, h, n, format, args);
    va_end(args);
    return true;
    }
    #endif

    if (format != NULL) {
        va_list args;
        va_start(args, format);
        vfprintf(stderr, format, args);
        va_end(args);
        fprintf(stderr, ",");
    }
    for (unsigned int i = 0; i < n; i++) {
        if (i != 0) { fprintf(stderr, ","); }
        fprintf(stderr, "h[%u]=%f", i, h[i]);
    }
    fprintf(stderr, "\n");
    return true;
}


bool dump_l2norm(
    void const* h, unsigned int num, RTTENSOR_DTYPE dtype,
    char const* format, ...)
{
    if (g_rtctx == NULL) { return true; }
    #ifdef _DEBUG_
    if (!g_rtctx->dump_l2norm) { return true; }
    #endif

    #ifdef _DUMP_BY_LOGMGR_
    {
    va_list args;
    va_start(args, format);
    dumpL2Norm(g_rtctx->lm, h, num, dtype, format, args);
    va_end(args);
    return true;
    }
    #endif

    if (format != NULL) {
        va_list args;
        va_start(args, format);
        vfprintf(stderr, format, args);
        va_end(args);
        fprintf(stderr, ",");
    }
    float norm = 0; //The L2 norm.
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    XASSERT(elemsz != 0);
    for (unsigned int i = 0; i < num; i++) {
        float v = tofloat(((BYTE*)h) + i * elemsz, dtype);
        norm += v*v;
    }
    norm = sqrtf(norm);
    fprintf(stderr, "l2norm=%.4f", norm);
    unsigned upper = num > 4 ? 4 : num;
    for (unsigned int i = 0; i < upper; i++) {
        fprintf(stderr, ",h[%u]=%.4f",
            i, tofloat(((BYTE*)h) + i * elemsz, dtype));
    }
    fprintf(stderr, "\n");
    return true;
}


bool dump_topn(
    void const* val, int64_t valnum, RTTENSOR_DTYPE dtype, unsigned int n,
    char const* format, ...)
{
    if (g_rtctx == NULL) { return true; }
    int64_t * top_ids = (int64_t*)malloc(n * sizeof(int64_t));
    XASSERT(dump("malloc %u bytes memory, top_idx", n * sizeof(int64_t)));
    memset(top_ids, 0, n * sizeof(int64_t));
    float * top_vals = (float*)malloc(n * sizeof(float));
    XASSERT(dump("malloc %u bytes memory, top_vals", n * sizeof(int64_t)));
    memset(top_vals, 0, n * sizeof(float));
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    XASSERT(elemsz != 0);
    top_vals[0] = tofloat(val, dtype);
    top_ids[0] = 0;
    for (int64_t i = 1; i < valnum; i++) {
        //Update elemens of top_vals.
        for (unsigned int t = 0; t < n; t++) {
            float fv = tofloat(((BYTE*)val) + i * elemsz, dtype);
            if (fv <= top_vals[t]) { continue; }

            //Find the larger elements.
            //Move val in top_vals backward until No.t.
            for (unsigned int s = n - 1; s > t; s--) {
                top_vals[s] = top_vals[s-1];
                top_ids[s] = top_ids[s-1];
            }
            //Update the t-th largest element of top_vals.
            top_vals[t] = fv;
            top_ids[t] = i;
            break;
        }
    }

    #ifdef _DUMP_BY_LOGMGR_
    {
    va_list args;
    va_start(args, format);
    dumpTopN(g_rtctx->lm, top_ids, top_vals, n, format, args);
    va_end(args);
    }
    #else
    fprintf(stderr, "\nDUMP TOP%u:", n);
    if (format != NULL) {
        fprintf(stderr, ",");
        va_list args;
        va_start(args, format);
        vfprintf(stderr, format, args);
        va_end(args);
    }
    fprintf(stderr, ":");
    for (unsigned int t = 0; t < n; t++) {
        fprintf(stderr, "\n  top%u:id=%d,logit=%.4f",
                t, top_ids[t], top_vals[t]);
    }
    fprintf(stderr, "\n");
    #endif

    free(top_ids);
    XASSERT(dump("free %u bytes memory", n * sizeof(int64_t)));

    free(top_vals);
    XASSERT(dump("free %u bytes memory", n * sizeof(int64_t)));
    return true;
}
