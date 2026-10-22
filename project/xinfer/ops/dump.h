#ifdef __cplusplus
extern "C" {
#endif

bool dump_l2norm(
    void const* h, unsigned int num, RTTENSOR_DTYPE dtype,
    char const* format, ...);
bool dump_f32vec(float const* h, unsigned int n, char const* format, ...);
bool dump(char const* format, ...);
bool dumpL2Norm(
    void * lm, void const* h, unsigned int num, RTTENSOR_DTYPE dtype,
    char const* format, va_list args);
bool dumpF32Vec(
    void * lm, float const* h, unsigned int n, char const* format,
    va_list args);
bool dumpNormInfo(void * lm, char const* format, va_list args);
bool dumpNormInfoVar(void * lm, char const* format, ...);
bool dump_topn(
    void const* val, int64_t valnum, RTTENSOR_DTYPE dtype, unsigned int n,
    char const* format, ...);
bool dumpTopN(
    void * lm, int64_t const* top_ids, float const* top_vals, unsigned int n,
    char const* format, va_list args);

typedef struct tagRTCtx RTCtx;
extern RTCtx * g_rtctx;

#ifdef __cplusplus
}
#endif
