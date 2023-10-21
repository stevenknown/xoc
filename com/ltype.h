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

//This file include system library header files and defined general macros
//that used in project.

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

#include "limits.h"
#include "stdlib.h"
#include "stdarg.h"
#include "stdio.h"
#include "string.h"
#include "memory.h"

namespace xcom {

#ifdef _SUPPORT_C11_
//Predefine C11 keyword if host compiler does not support C11.
#define nullptr NULL
#endif

//Primitive types may have been defined, override them.
#undef STATUS
#undef BYTE
#undef CHAR
#undef UCHAR
#undef SHORT
#undef USHORT
#undef INT
#undef INT8
#undef INT16
#undef INT32
#undef INT64
#undef INT128
#undef UINT
#undef UINT8
#undef UINT16
#undef UINT32
#undef UINT64
#undef UINT128
#undef LONG
#undef ULONG
#undef LONGLONG
#undef ULONGLONG
#undef BOOL
#undef UINT64
#undef UINT32

typedef signed int STATUS;
typedef unsigned char BYTE;
typedef char CHAR;
typedef unsigned char UCHAR;
typedef signed short SHORT;
typedef unsigned short USHORT;
typedef signed int INT;
typedef unsigned int UINT;
typedef signed long LONG;
typedef unsigned long ULONG;
typedef signed char INT8;
typedef signed short INT16;
typedef signed int INT32;
typedef signed long long INT64;
typedef signed long long INT128;
typedef unsigned char UINT8;
typedef unsigned short UINT16;
typedef unsigned int UINT32;
typedef unsigned long long UINT64;
typedef unsigned long long UINT128;

#ifdef _ON_WINDOWS_
    typedef __int64 LONGLONG;
    typedef unsigned __int64 ULONGLONG;
#else
    typedef long long LONGLONG;
    typedef unsigned long long ULONGLONG;
#endif

#ifndef MAX_HOST_INT_VALUE
#define MAX_HOST_INT_VALUE INT_MAX
#endif
#ifndef MIN_HOST_INT_VALUE
#define MIN_HOST_INT_VALUE INT_MIN
#endif

} //namespace xcom

#endif
