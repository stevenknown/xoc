/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Su Zhenyu nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#ifndef _L_TYPE_
#define _L_TYPE_

#ifdef _ON_WINDOWS_
    #ifdef _VC6_
    #include "windows.h"
    #include "errors.h"
    #endif

    //The enumerate has no associated handler in a switch statement.
    #pragma warning(disable: 4061)

    //Conditional expression is constant.
    #pragma warning(disable: 4127)

    //unreferenced inline function has been removed.
    #pragma warning(disable: 4514)

    //A number of bytes padding added after data member.
    #pragma warning(disable: 4820)

    //Nonstandard extension used : zero-sized array in template SRDescGroup
    #pragma warning(disable: 4200)

    #include "malloc.h"
    #define ALLOCA    _alloca
    #define SNPRINTF _snprintf
    #define VSNPRINTF _vsnprintf
    #define RESTRICT __restrict

    //Use POSIX name "unlink" instead of ISO C++ conformat name "_unlink".
    #define UNLINK   _unlink
#else
    //Default is linux version
    #include "unistd.h" //for UNLINK declaration
    #define ALLOCA    alloca
    #define SNPRINTF snprintf
    #define VSNPRINTF vsnprintf
    #define RESTRICT __restrict__
    #define UNLINK   unlink
#endif

#include "stdlib.h"
#include "stdarg.h"
#include "stdio.h"
#include "string.h"
#include "memory.h"

//Primitive types may have been defined, override them.
#undef STATUS
#undef BYTE
#undef CHAR
#undef UCHAR
#undef SHORT
#undef USHORT
#undef INT
#undef UINT
#undef LONG
#undef ULONG
#undef LONGLONG
#undef ULONGLONG
#undef BOOL

typedef int STATUS;
typedef unsigned char BYTE;
typedef char CHAR;
typedef unsigned char UCHAR;
typedef short SHORT;
typedef unsigned short USHORT;
typedef int INT;
typedef unsigned int UINT;
typedef unsigned int BOOL;
typedef long LONG;
typedef unsigned long ULONG;

#ifdef _ON_WINDOWS_
    typedef __int64 LONGLONG;
    typedef unsigned __int64 ULONGLONG;
#else
    typedef long long LONGLONG;
    typedef unsigned long long ULONGLONG;
#endif

#ifndef va_copy
#define va_copy(d, s) ((d) = (s))
#endif

//Avoid using the predefined ASSERTN.
#undef ASSERTN
#undef ASSERTL
#undef ASSERT0
#undef ASSERTL0

// This macro is needed to prevent the clang static analyzer from generating
// false-positive reports in ASSERTN() macros.
#ifdef __clang__
#define CLANG_ANALYZER_NORETURN __attribute__((analyzer_noreturn))
#else
#define CLANG_ANALYZER_NORETURN
#endif

#ifdef _DEBUG_
    #ifdef __cplusplus
        #define EXTERN_C extern "C"
    #else
        #define EXTERN_C extern
    #endif
    #include "stdio.h"
    EXTERN_C INT m518087(CHAR const* info, ...) CLANG_ANALYZER_NORETURN;
    EXTERN_C INT m022138(CHAR const* filename, INT line) CLANG_ANALYZER_NORETURN;

    #define ASSERTN(a, b)  \
        ((a) ? 0 : (m022138(__FILE__, __LINE__), m518087 b))
    #define ASSERTL(a, filename, line, b)  \
        ((a) ? 0 : (m022138(filename, line), m518087 b))
    #define ASSERT0(a)  ((a) ? 0 : (m022138(__FILE__, __LINE__), m518087 ("")))
    #define ASSERTL0(a, filename, line)  \
        ((a) ? 0 : (m022138(filename, line), m518087 ("")))
#else
    #define ASSERTN(a, b)
    #define ASSERTL(a, filename, line, b)
    #define ASSERT0(a)
    #define ASSERTL0(a, filename, line)
#endif

#undef UNREACHABLE
#define UNREACHABLE()  ASSERTN(0, ("Unreachable."))

#undef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))

#undef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))

#undef DIFF
#define DIFF(a,b) (xabs((a)-(b)))

#undef ODD
#define ODD(v) ((v)%2!=0?1:0)

#undef EVEN
#define EVEN(v) (!ODD(v))

#undef SET_FLAG
#define SET_FLAG(flag, v) ((flag) |= (v))

#undef HAVE_FLAG
#define HAVE_FLAG(flag, v) (((flag) & (v)) == (v))

#undef ONLY_HAVE_FLAG
#define ONLY_HAVE_FLAG(flag, v) (!((flag)&(~(v))))

#undef REMOVE_FLAG
#define REMOVE_FLAG(flag, v) ((flag) = ((flag) & (~(v))))

#undef OFFSET_OF
//Offset of field 'f' of struct type 'st'.
#define OFFSET_OF(st, f) ((size_t)&((st*)0)->f)

#undef GET_LOW_32BIT
#define GET_LOW_32BIT(l) ((l)&0xffffFFFF)

#undef GET_HIGH_32BIT
#define GET_HIGH_32BIT(l) (((l)>>32)&0xffffFFFF)

//True if type is unsigned.
#define IS_UNSIGN_TY(type) ((type)(((type)0) - 1) > 0)

#undef IN
#define IN  //indicate input argument

#undef OUT
#define OUT //indicate output argument

//Misc Dumps/Dumpf of Vector<T>
#define D_BOOL           1
#define D_INT            2
#endif