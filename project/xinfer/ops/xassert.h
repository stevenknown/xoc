#ifndef XASSERT_H
#define XASSERT_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _DEBUG_
#define XASSERT(cond) \
    do { \
        if (!(cond)) { \
            fprintf(stderr, "XASSERT FAILED: %s, file:%s, line:%u\n", \
                    #cond, __FILE__, __LINE__); \
            abort(); \
        } \
    } while(0)
#else
    #define XASSERT(cond)
#endif

#ifdef __cplusplus
}
#endif

#endif
