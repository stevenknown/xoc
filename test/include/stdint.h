#ifndef _STDINT_H_
#define _STDINT_H_

//#include <limits.h>

/* Exact-width integer types.  */
typedef signed char        int8_t;
typedef unsigned char       uint8_t;

typedef signed short       int16_t;
typedef unsigned short      uint16_t;

typedef signed int          int32_t;
typedef unsigned int        uint32_t;

typedef signed long long   int64_t;
typedef unsigned long long uint64_t;

typedef signed int intptr_t;

/* Minimum and maximum values for these types.  */
#define INT8_MIN  (-128)
#define INT8_MAX   (127)
#define UINT8_MAX  (255)

#define INT16_MIN (-32767-1)
#define INT16_MAX  (32767)
#define UINT16_MAX (65535)

#define INT32_MIN (-2147483647-1)
#define INT32_MAX  (2147483647)
#define UINT32_MAX (4294967295U)

#define __INT64_C(v)  (v ## LL)

#define INT64_MIN (-__INT64_C(9223372036854775807)-1)
#define INT64_MAX  (__INT64_C(9223372036854775807))
#define UINT64_MAX (__UINT64_C(18446744073709551615))

///* Limits of integral types.  */
//#if defined(__INT_MAX__) && defined(__LONG_MAX__)
//  #define INTMAX_MIN (-__LONG_MAX__ - 1)
//  #define INTMAX_MAX  __LONG_MAX__
//  #define UINTMAX_MAX __LONG_MAX__
//#elif defined(__INT64_MAX__)
//  #define INTMAX_MIN (-__INT64_MAX__ - 1)
//  #define INTMAX_MAX  __INT64_MAX__
//  #define UINTMAX_MAX __UINT64_MAX
//#else
//  #error "No integral type is large enough to be INTMAX"
//#endif

/* Limits of the common real types.  */
//#define FLOAT_MIN      (1.17549435e-38F)
//#define FLOAT_MAX      (3.40282347e+38F)
//#define DOUBLE_MIN     (2.22507287e-308)
//#define DOUBLE_MAX     (1.79769e+308)
//#define LONG_DOUBLE_MIN (3.36210314311209350e-4932L)
//#define LONG_DOUBLE_MAX (1.18973149535723176502e+4932L)

/* Limits of other real types.  */
//#define LDBL_MIN      (3.36210314311209350e-4932L)
//#define LDBL_MAX      (1.18973149535723176502e+4932L)

/* Least significant bit, etc.  */
#define INT8_C(x)   x ## LL
#define INT16_C(x)  x ## LL
#define INT32_C(x)  x ## LL
#define INT64_C(x)  x ## LL

#define UINT8_C(x)  x ## U
#define UINT16_C(x) x ## U
#define UINT32_C(x) x ## U
#define UINT64_C(x) x ## UL

/* Compile-time checks for minimum-width integer types.  */
//_Static_assert(sizeof(int8_t) == 1, "int8_t is not 8 bits");
//_Static_assert(sizeof(uint8_t) == 1, "uint8_t is not 8 bits");
//_Static_assert(sizeof(int16_t) == 2, "int16_t is not 16 bits");
//_Static_assert(sizeof(uint16_t) == 2, "uint16_t is not 16 bits");
//_Static_assert(sizeof(int32_t) == 4, "int32_t is not 32 bits");
//_Static_assert(sizeof(uint32_t) == 4, "uint32_t is not 32 bits");
//_Static_assert(sizeof(int64_t) == 8, "int64_t is not 64 bits");
//_Static_assert(sizeof(uint64_t) == 8, "uint64_t is not 64 bits");

#endif /* stdint.h */
