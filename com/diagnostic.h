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
//AVOID USING THE PREDEFINED ASSERTN.
#undef ASSERTN
#undef ASSERTLN
#undef ASSERT0
#undef ASSERTL0
#undef ASSERT0L1
#undef ASSERT0L2
#undef ASSERT0L3
#undef ASSERTNL0
#undef ASSERTNL1
#undef ASSERTNL2
#undef ASSERTNL3

//These macros define the assertion level. The assertion functions only
//take effect when the level of assertion function is greater than user
//defined ASSERT_LEVEL.
//e.g: cmdline is cc -DASSERT_LEVEL=3, then only ASSERT0L3 and ASSERTNL3
//take effect.
#define ASSERT_LEVEL0 0
#define ASSERT_LEVEL1 1
#define ASSERT_LEVEL2 2
#define ASSERT_LEVEL3 3
#define ASSERT_LEVEL_MAX ASSERT_LEVEL3

//Defined for clang analyzer.
// This macro is needed to prevent the clang static analyzer from generating
// false-positive reports in ASSERTN() macros.
#ifdef __clang__
#define CLANG_ANALYZER_NORETURN __attribute__((analyzer_noreturn))
#else
#define CLANG_ANALYZER_NORETURN
#endif

//These marcos define the assertion functions under DEBUG mode.
#ifdef _DEBUG_
    #ifdef __cplusplus
        #define EXTERN_C extern "C"
    #else
        #define EXTERN_C extern
    #endif
    #include "stdio.h"
    EXTERN_C INT m518087(CHAR const* info, ...) CLANG_ANALYZER_NORETURN;
    EXTERN_C INT m522138(CHAR const* filename, INT line)
        CLANG_ANALYZER_NORETURN;

    #define ASSERTN(a, b) \
        ((a) ? (void)0 : (void)(m522138(__FILE__, __LINE__), m518087 b))
    #define ASSERTLN(a, filename, line, b) \
        ((a) ? (void)0 : (void)(m522138(filename, line), m518087 b))
    #define ASSERT0(a) \
        ((a) ? (void)0 : (void)(m522138(__FILE__, __LINE__), m518087("")))
    #define ASSERTL0(a, filename, line) \
        ((a) ? (void)0 : (void)(m522138(filename, line), m518087("")))

    //These macros define assertion functions that will take effect according
    //to whether user define ASSERT_LEVEL in cmdline.
    //If ASSERT_LEVEL is not defined, these macros take effect by default.
    //Otherwise, the assertion functions only take effect according
    //to the constant value of ASSERT_LEVEL.
    //If ASSERT_LEVEL is defined with a literal, such as ASSERT_LEVEL=2, only
    //the assertion functions that assert-level greater than 2 take effect.
    //e.g1: If user's cmdline is 'cc -D_DEBUG_ -DASSERT_LEVEL=2', then only
    //ASSERT0L2,  ASSERTNL2, ASSERT0L3 and ASSERTNL3 take effect.
    //e.g2: If user's cmdline is 'cc -D_DEBUG_ -DASSERT_LEVEL=-1', then none of
    //assertion functions take effect.
    #ifdef ASSERT_LEVEL
        #if ASSERT_LEVEL >= ASSERT_LEVEL0
            #define ASSERT0L0(a) ASSERT0(a)
            #define ASSERTNL0(a, b) ASSERTN(a, b)
        #else
            #define ASSERT0L0(a)
            #define ASSERTNL0(a, b)
        #endif

        #if ASSERT_LEVEL >= ASSERT_LEVEL1
            #define ASSERT0L1(a) ASSERT0(a)
            #define ASSERTNL1(a, b) ASSERTN(a, b)
        #else
            #define ASSERT0L1(a)
            #define ASSERTNL1(a, b)
        #endif

        #if ASSERT_LEVEL >= ASSERT_LEVEL2
            #define ASSERT0L2(a) ASSERT0(a)
            #define ASSERTNL2(a, b) ASSERTN(a, b)
        #else
            #define ASSERT0L2(a)
            #define ASSERTNL2(a, b)
        #endif

        #if ASSERT_LEVEL >= ASSERT_LEVEL3
            #define ASSERT0L3(a) ASSERT0(a)
            #define ASSERTNL3(a, b) ASSERTN(a, b)
        #else
            #define ASSERT0L3(a)
            #define ASSERTNL3(a, b)
        #endif
    #else
        #define ASSERT0L0(a) ASSERT0(a)
        #define ASSERTNL0(a, b) ASSERTN(a, b)
        #define ASSERT0L1(a) ASSERT0(a)
        #define ASSERTNL1(a, b) ASSERTN(a, b)
        #define ASSERT0L2(a) ASSERT0(a)
        #define ASSERTNL2(a, b) ASSERTN(a, b)
        #define ASSERT0L3(a) ASSERT0(a)
        #define ASSERTNL3(a, b) ASSERTN(a, b)
    #endif
#else
    #define ASSERTN(a, b)
    #define ASSERTLN(a, filename, line, b)
    #define ASSERT0(a)
    #define ASSERTL0(a, filename, line)
    #define ASSERT0L0(a)
    #define ASSERTNL0(a, b)
    #define ASSERT0L1(a)
    #define ASSERTNL1(a, b)
    #define ASSERT0L2(a)
    #define ASSERTNL2(a, b)
    #define ASSERT0L3(a)
    #define ASSERTNL3(a, b)
#endif

#undef UNREACHABLE
#define UNREACHABLE()  ASSERTN(0, ("Unreachable."))

////////////////////////////////////////////////////////////////////////////////
//FOLLOWING MACROS DEFINED THE INTERFACE THAT MAY BE USED IN RELEASE MODE.    //
////////////////////////////////////////////////////////////////////////////////

//CHECK
//Used to avoid warning: unreferenced variable if set
//-Werror=unused-variable.
template <typename T> int dummy_use(T const&) { return 0; }
#define DUMMYUSE(v) xcom::dummy_use(v)
#define DUMMYUSE_LABEL(label) if(0) { goto label; }

#ifdef _DEBUG_ //DEBUG MODE
    //Do assert at debug mode, and do dummyuse at release mode.
    #define ASSERT0_DUMMYUSE(a) ASSERT0(a)
    #define ASSERTN_DUMMYUSE(a, b) ASSERTN(a, b)
#else //RELEASE MODE
    //Do assert at debug mode, and do dummyuse at release mode.
    #define ASSERT0_DUMMYUSE(a) DUMMYUSE(a)
    #define ASSERTN_DUMMYUSE(a, b) DUMMYUSE(a)
#endif

} //namespace xcom

#endif //END __DIAGNOSTIC_H__
