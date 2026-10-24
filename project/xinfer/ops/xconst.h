#ifdef __cplusplus
extern "C" {
#endif

#define INVALID_TOKEN (-1)
#define MAX_ELEMNUM_ON_STACK 64

//Default epsilon is 1e-5f
#define DEF_EPS (0.000000000001)

//Limits of integral types.
#ifndef INT8_MIN
#define INT8_MIN (-128)
#endif
#ifndef INT16_MIN
#define INT16_MIN (-32767-1)
#endif
#ifndef INT32_MIN
#define INT32_MIN (-2147483647-1)
#endif
#ifndef INT8_MAX
#define INT8_MAX (127)
#endif
#ifndef INT16_MAX
#define INT16_MAX (32767)
#endif
#ifndef INT32_MAX
#define INT32_MAX (2147483647)
#endif
#ifndef UINT8_MAX
#define UINT8_MAX (255U)
#endif
#ifndef UINT16_MAX
#define UINT16_MAX (65535U)
#endif
#ifndef UINT32_MAX
#define UINT32_MAX (4294967295U)
#endif

#ifdef _DEBUG_
  //DummyUsed
  #define ATTRIBUTE_USE
  #define ATTRIBUTE_INLINE
#else 
  #ifdef _USE_GCC_
    #define ATTRIBUTE_USE __attribute__((used))
    #define ATTRIBUTE_INLINE __attribute__((always_inline))
  #else
    #define ATTRIBUTE_USE
    #define ATTRIBUTE_INLINE
  #endif
#endif

#ifdef __cplusplus
}
#endif
