/*
 * limits.h - Integer type limits
 * 
 * Conforms to: C89, C99, C11, C17
 * 
 * This file defines macros that specify the minimum and maximum values
 * for integer types on different architectures (32-bit and 64-bit).
 * 
 * Copyright (c) 2026. All rights reserved.
 */

#ifndef _LIMITS_H
#define _LIMITS_H

/* ==========================================================================
 * Architecture Detection
 * ========================================================================== */

/* Detect 64-bit architecture */
#if defined(__x86_64__) || defined(__x86_64) || defined(__amd64__) || \
    defined(__amd64) || defined(__aarch64__) || defined(_M_X64) || \
    defined(_M_AMD64) || defined(__LP64__) || defined(_LP64)
    #define __ARCH_64BIT  1
    #define __ARCH_32BIT  0

/* Detect 32-bit architecture */
#elif defined(__i386__) || defined(__i386) || defined(i386) || \
      defined(_M_IX86) || defined(__ARM_ARCH_7__) || \
      defined(__ARM_ARCH_7A__) || defined(_M_ARM) || defined(__arm__)
    #define __ARCH_64BIT  0
    #define __ARCH_32BIT  1

/* Default to 32-bit if architecture cannot be determined */
#else
    #define __ARCH_64BIT  0
    #define __ARCH_32BIT  1
#endif

/* ==========================================================================
 * Basic Character Limits
 * ========================================================================== */

/* Number of bits in a char (at least 8) */
#define CHAR_BIT        8

/* char type range (depends on whether char is signed or unsigned) */
/* Default: char is signed */
#ifndef __CHAR_UNSIGNED__
    #define CHAR_MIN    (-128)
    #define CHAR_MAX    127
#else
    #define CHAR_MIN    0
    #define CHAR_MAX    255
#endif

/* signed char type range */
#define SCHAR_MIN       (-128)
#define SCHAR_MAX       127

/* unsigned char type range */
#define UCHAR_MAX       255

/* ==========================================================================
 * Short Integer Limits (at least 16 bits)
 * ========================================================================== */

/* short type range */
#define SHRT_MIN        (-32767 - 1)
#define SHRT_MAX        32767

/* unsigned short type range */
#define USHRT_MAX       65535

/* ==========================================================================
 * Integer Limits (architecture-dependent)
 * ========================================================================== */

#if __ARCH_64BIT
    /* 64-bit architecture: int is typically 32 bits */
    #define INT_MIN         (-2147483647 - 1)
    #define INT_MAX         2147483647
    #define UINT_MAX        4294967295U
#else
    /* 32-bit architecture: int is 32 bits */
    #define INT_MIN         (-2147483647 - 1)
    #define INT_MAX         2147483647
    #define UINT_MAX        4294967295U
#endif

/* ==========================================================================
 * Long Integer Limits (architecture-dependent)
 * ========================================================================== */

#if __ARCH_64BIT
    /* 64-bit architecture (LP64 model): long is 64 bits */
    #define LONG_MIN        (-9223372036854775807L - 1)
    #define LONG_MAX        9223372036854775807L
    #define ULONG_MAX       18446744073709551615UL
#else
    /* 32-bit architecture: long is 32 bits */
    #define LONG_MIN        (-2147483647L - 1)
    #define LONG_MAX        2147483647L
    #define ULONG_MAX       4294967295UL
#endif

/* ==========================================================================
 * Long Long Integer Limits (at least 64 bits, C99 and later)
 * ========================================================================== */

#define LLONG_MIN       (-9223372036854775807LL - 1)
#define LLONG_MAX       9223372036854775807LL
#define ULLONG_MAX      18446744073709551615ULL

/* ==========================================================================
 * Wide Character Limits (C99 and later)
 * ========================================================================== */

#if __ARCH_64BIT
    #define WCHAR_MIN   (-2147483647 - 1)
    #define WCHAR_MAX   2147483647
#else
    #define WCHAR_MIN   (-2147483647 - 1)
    #define WCHAR_MAX   2147483647
#endif

#define WINT_MIN        0U
#define WINT_MAX        4294967295U

/* ==========================================================================
 * Fixed-Width Integer Limits (C99 stdint.h related)
 * 
 * Note: These are typically defined in stdint.h, but some implementations
 * also provide them here for convenience.
 * ========================================================================== */

/* int8_t type limits */
#define INT8_MIN        (-128)
#define INT8_MAX        127
#define UINT8_MAX       255

/* int16_t type limits */
#define INT16_MIN       (-32767 - 1)
#define INT16_MAX       32767
#define UINT16_MAX      65535

/* int32_t type limits */
#define INT32_MIN       (-2147483647 - 1)
#define INT32_MAX       2147483647
#define UINT32_MAX      4294967295U

/* int64_t type limits */
#define INT64_MIN       (-9223372036854775807LL - 1)
#define INT64_MAX       9223372036854775807LL
#define UINT64_MAX      18446744073709551615ULL

/* ==========================================================================
 * Pointer-Related Limits (architecture-dependent)
 * ========================================================================== */

#if __ARCH_64BIT
    /* 64-bit architecture: pointers are 64 bits */
    #define INTPTR_MIN      (-9223372036854775807L - 1)
    #define INTPTR_MAX      9223372036854775807L
    #define UINTPTR_MAX     18446744073709551615UL
    
    #define PTRDIFF_MIN     (-9223372036854775807L - 1)
    #define PTRDIFF_MAX     9223372036854775807L
    
    #define SIZE_MAX        18446744073709551615UL
#else
    /* 32-bit architecture: pointers are 32 bits */
    #define INTPTR_MIN      (-2147483647 - 1)
    #define INTPTR_MAX      2147483647
    #define UINTPTR_MAX     4294967295U
    
    #define PTRDIFF_MIN     (-2147483647 - 1)
    #define PTRDIFF_MAX     2147483647
    
    #define SIZE_MAX        4294967295U
#endif

/* ==========================================================================
 * Signal Atomic Type Limits
 * ========================================================================== */

#define SIG_ATOMIC_MIN  (-2147483647 - 1)
#define SIG_ATOMIC_MAX  2147483647

/* ==========================================================================
 * Standard Compliance Macros
 * ========================================================================== */

/* C standard version detection */
#ifdef __STDC__
    #ifndef __STDC_VERSION__
        #define __STDC_VERSION__  201710L  /* C17 default */
    #endif
#endif

/* Long long support indicator (C99 and later) */
#ifndef __NO_LONG_LONG
    #define __LONG_LONG_MAX__  9223372036854775807LL
#endif

/* ==========================================================================
 * Helper Macros
 * ========================================================================== */

/* Macro to calculate signed type maximum value */
#define __SIGNED_MAX(type)  ((type)(((((type)1 << (sizeof(type) * CHAR_BIT - 2)) - 1) * 2) + 1))

/* Macro to calculate signed type minimum value */
#define __SIGNED_MIN(type)  ((type)(-__SIGNED_MAX(type) - 1))

/* Macro to calculate unsigned type maximum value */
#define __UNSIGNED_MAX(type) ((type)(~(type)0))

/* ==========================================================================
 * POSIX Extensions (optional)
 * ========================================================================== */

#if defined(_POSIX_SOURCE) || defined(__STDC__)

    #define PATH_MAX        4096
    #define NAME_MAX        255
    #define LINE_MAX        2048
#endif

/* ==========================================================================
 * Architecture Information Macros (for debugging/reference)
 * ========================================================================== */

#ifdef __ARCH_64BIT
    #if __ARCH_64BIT == 1
        #define __ARCH_INFO "64-bit"
    #else
        #define __ARCH_INFO "32-bit"
    #endif
#endif

/* ==========================================================================
 * End Include Guard
 * ========================================================================== */

#endif /* _LIMITS_H */
