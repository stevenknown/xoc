#include "ops.h"

RTCtx * g_rtctx = NULL;

bool prt(char const* format, ...)
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
    prtNormInfo(g_rtctx->lm, format, args);
    va_end(args);
    return true;
    }
    #endif

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    return true;
}


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


bool dump_mat(
    void const* vec, unsigned int nrow, unsigned int ncol,
    RTTENSOR_DTYPE dtype, char const* format, ...)
{
    if (g_rtctx == NULL) { return true; }
    #ifdef _DEBUG_
    if (!g_rtctx->dump_mat) { return true; }
    #endif

    #ifdef _DUMP_BY_LOGMGR_
    {
    va_list args;
    va_start(args, format);
    dumpMat(g_rtctx->lm, vec, nrow, ncol, dtype, format, args);
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
    switch (dtype) {
    case RTTENSOR_DTYPE_F32: {
        float const* h = (float const*)vec;
        for (unsigned int i = 0; i < nrow; i++) {
            fprintf(stderr, "\n");
            for (unsigned int j = 0; j < ncol; j++) {
                if (j != 0) { fprintf(stderr, ","); }
                fprintf(stderr, "h[%u]=%f", i * ncol + j, h[i * ncol + j]);
            }
        }
        break;
    }
    case RTTENSOR_DTYPE_F16: {
        uint16_t const* h = (uint16_t const*)vec;
        for (unsigned int i = 0; i < nrow; i++) {
            fprintf(stderr, "\n");
            for (unsigned int j = 0; j < ncol; j++) {
                if (j != 0) { fprintf(stderr, ","); }
                fprintf(stderr, "h[%u]=%f",
                    i * ncol + j, fp16_to_float(h[i * ncol + j]));
            }
        }
        break;
    }
    case RTTENSOR_DTYPE_BF16: {
        uint16_t const* h = (uint16_t const*)vec;
        for (unsigned int i = 0; i < nrow; i++) {
            fprintf(stderr, "\n");
            for (unsigned int j = 0; j < ncol; j++) {
                if (j != 0) { fprintf(stderr, ","); }
                fprintf(stderr, "h[%u]=%f",
                    i * ncol + j, bf16_to_float(h[i * ncol + j]));
            }
        }
        break;
    }
    default: XASSERT(0);
    }
    fprintf(stderr, "\n");
    return true;
}


bool dump_vec(void const* vec, unsigned int elemnum, RTTENSOR_DTYPE dtype,
    char const* format, ...)
{
    if (g_rtctx == NULL) { return true; }
    #ifdef _DEBUG_
    if (!g_rtctx->dump_vec) { return true; }
    #endif

    #ifdef _DUMP_BY_LOGMGR_
    {
    va_list args;
    va_start(args, format);
    dumpVec(g_rtctx->lm, vec, elemnum, dtype, format, args);
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
    switch (dtype) {
    case RTTENSOR_DTYPE_F32: {
        float const* h = (float const*)vec;
        for (unsigned int i = 0; i < elemnum; i++) {
            if (i != 0) { fprintf(stderr, ","); }
            fprintf(stderr, "h[%u]=%f", i, h[i]);
        }
        break;
    }
    case RTTENSOR_DTYPE_F16: {
        uint16_t const* h = (uint16_t const*)vec;
        for (unsigned int i = 0; i < elemnum; i++) {
            if (i != 0) { fprintf(stderr, ","); }
            fprintf(stderr, "h[%u]=%f", i, fp16_to_float(h[i]));
        }
        break;
    }
    case RTTENSOR_DTYPE_BF16: {
        uint16_t const* h = (uint16_t const*)vec;
        for (unsigned int i = 0; i < elemnum; i++) {
            if (i != 0) { fprintf(stderr, ","); }
            fprintf(stderr, "h[%u]=%f", i, bf16_to_float(h[i]));
        }
        break;
    }
    default: XASSERT(0);
    }
    fprintf(stderr, "\n");
    return true;
}


bool dump_l2norm(
    void const* h, unsigned int groupnum, unsigned int elemnum_in_group,
    RTTENSOR_DTYPE dtype, char const* format, ...)
{
    if (g_rtctx == NULL) { return true; }
    #ifdef _DEBUG_
    if (!g_rtctx->dump_l2norm) { return true; }
    #endif

    #ifdef _DUMP_BY_LOGMGR_
    {
    va_list args;
    va_start(args, format);
    dumpL2Norm(g_rtctx->lm, h, groupnum, elemnum_in_group, dtype, format, args);
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
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    XASSERT(elemsz != 0);
    for (unsigned int x = 0; x < groupnum;
         x++, h = ((BYTE*)h) + elemnum_in_group * elemsz) {
        float norm = 0; //The L2 norm.
        for (unsigned int i = 0; i < elemnum_in_group; i++) {
            float v = tofloat(((BYTE*)h) + i * elemsz, dtype);
            norm += v*v;
        }
        norm = sqrtf(norm);
        fprintf(stderr, "l2norm=%.4f", norm);
        unsigned upper = elemnum_in_group > 4 ? 4 : elemnum_in_group;
        for (unsigned int i = 0; i < upper; i++) {
            fprintf(stderr, ",h[%u]=%.4f",
                i, tofloat(((BYTE*)h) + i * elemsz, dtype));
        }
        fprintf(stderr, "\n");
    }
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
        fprintf(stderr, "\n  top%u:id=%ld,logit=%.4f",
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


bool dumpQKV(
    BYTE const* qkv, unsigned int qkvgroupnum, int64_t startpos,
    unsigned int n_head, unsigned int n_kvhead, unsigned int head_dim,
    RTTENSOR_DTYPE dtype)
{
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    XASSERT(elemsz != 0);
    XASSERT(startpos >= 0);
    unsigned int headdimsz = head_dim * elemsz;
    unsigned int qkvsz = (n_head + 2 * n_kvhead) * headdimsz;
    for (int64_t i = 0; i < qkvgroupnum; i++, startpos++) {
        //Split q,k,v.
        //Get QKV in token sequences.
        BYTE const* q = qkv + i * qkvsz;
        BYTE const* k = q + n_head * headdimsz;
        BYTE const* v = k + n_kvhead * headdimsz;
        XASSERT(dump_l2norm(
            q, 1, n_head, dtype, "QKV projection q,pos%d", startpos));
        XASSERT(dump_l2norm(
            k, 1, n_kvhead, dtype, "QKV projection k,pos%d", startpos));
        XASSERT(dump_l2norm(
            v, 1, n_kvhead, dtype, "QKV projection v,pos%d", startpos));
    }
    return true;
}
