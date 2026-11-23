#ifndef _OPS_DUMP_H_
#define _OPS_DUMP_H_

#ifdef __cplusplus
extern "C" {
#endif

bool dump_l2norm(
    void const* h, unsigned int groupnum,
    unsigned int elemnum_in_group, RTTENSOR_DTYPE dtype,
    char const* format, ...);
bool dump_f32vec(float const* h, unsigned int n, char const* format, ...);
bool dump_vec(void const* vec, unsigned int elemnum, RTTENSOR_DTYPE dtype,
    char const* format, ...);
bool dump_mat(
    void const* vec, unsigned int nrow, unsigned int ncol,
    RTTENSOR_DTYPE dtype, char const* format, ...);
bool dump(char const* format, ...);
bool dumpL2Norm(
    void * lm, void const* h, unsigned int groupnum,
    unsigned int elemnum_in_group, RTTENSOR_DTYPE dtype,
    char const* format, va_list args);
bool dumpF32Vec(
    void * lm, float const* h, unsigned int n, char const* format,
    va_list args);
bool dumpVec(
    void * lm, void const* h, unsigned int n, RTTENSOR_DTYPE dtype,
    char const* format, va_list args);
bool dumpMat(
    void * lm, void const* h, unsigned int nrow, unsigned int ncol,
    RTTENSOR_DTYPE dtype, char const* format, va_list args);
bool dumpNormInfo(void * lm, char const* format, va_list args);
bool dumpNormInfoVar(void * lm, char const* format, ...);
bool dump_topn(
    void const* val, int64_t valnum, RTTENSOR_DTYPE dtype, unsigned int n,
    char const* format, ...);
bool dumpTopN(
    void * lm, int64_t const* top_ids, float const* top_vals, unsigned int n,
    char const* format, va_list args);
bool dumpQKV(
    BYTE const* qkv, unsigned int qkvgroupnum, int64_t startpos,
    unsigned int n_head, unsigned int n_kvhead, unsigned int head_dim,
    RTTENSOR_DTYPE dtype);

bool prtNormInfo(void * lm, char const* format, va_list args);
bool prt(char const* format, ...);

typedef struct tagRTCtx RTCtx;
extern RTCtx * g_rtctx;

#ifdef __cplusplus
}
#endif

#endif
