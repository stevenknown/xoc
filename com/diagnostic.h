/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com
All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __DIAGNOSTIC_H__
#define __DIAGNOSTIC_H__

//This file defined assertion and checking api that used in xcom namespace.

namespace xcom {

////////////////////////////////////////////////////////////////////
//Following macros defined the interface that used to diagnose the//
//validation of program in debug mode.                            //
////////////////////////////////////////////////////////////////////

//ASSERT
//Avoid using the predefined ASSERTN.
#undef ASSERTN
#undef ASSERTL
#undef ASSERT0
#undef ASSERTL0

//Defined for clang analyzer.
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
    EXTERN_C INT m522138(CHAR const* filename, INT line) CLANG_ANALYZER_NORETURN;

    #define ASSERTN(a, b) \
        ((a) ? (void)0 : (void)(m522138(__FILE__, __LINE__), m518087 b))
    #define ASSERTL(a, filename, line, b) \
        ((a) ? (void)0 : (void)(m522138(filename, line), m518087 b))
    #define ASSERT0(a) \
        ((a) ? (void)0 : (void)(m522138(__FILE__, __LINE__), m518087 ("")))
    #define ASSERTL0(a, filename, line) \
        ((a) ? (void)0 : (void)(m522138(filename, line), m518087 ("")))
#else
    #define ASSERTN(a, b)
    #define ASSERTL(a, filename, line, b)
    #define ASSERT0(a)
    #define ASSERTL0(a, filename, line)
#endif

#undef UNREACHABLE
#define UNREACHABLE()  ASSERTN(0, ("Unreachable."))

////////////////////////////////////////////////////////////////////////////
//Following macros defined the interface that may be used in release mode.//
////////////////////////////////////////////////////////////////////////////

//CHECK
//Used to avoid warning: unreferenced variable if set
//-Werror=unused-variable.
template <typename T> int dummy_use(T const&) { return 0; }
#define DUMMYUSE(v) xcom::dummy_use(v)

#ifdef _DEBUG_ //DEBUG MODE
    //Do assert at debug mode, and do dummyuse at release mode.
    #define CHECK0_DUMMYUSE(a) ASSERT0(a)
    #define CHECKN_DUMMYUSE(a, b) ASSERTN(a, b)
#else //RELEASE MODE
    //Do assert at debug mode, and do dummyuse at release mode.
    #define CHECK0_DUMMYUSE(a) DUMMYUSE(a)
    #define CHECKN_DUMMYUSE(a, b) DUMMYUSE(a)
#endif

} //namespace xcom

#endif //END __DIAGNOSTIC_H__
