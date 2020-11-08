/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com
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
#ifndef __COMM_MACRO_H__
#define __COMM_MACRO_H__

//This file defined macros that used in xcom namespace.

namespace xcom {

//This macro declare copy constructor for class.
#define COPY_CONSTRUCTOR(class_name)   \
    class_name(class_name const&);     \
    class_name const& operator = (class_name const&)

//Used to avoid warning: unreferenced variable if set
//-Werror=unused-variable.
template <typename T> int dummy_use(T const&) { return 0; }
#define DUMMYUSE(v) xcom::dummy_use(v)

#ifdef _DEBUG_
//Do assert at debug mode, and do dummyuse at release mode.
#define CHECK_DUMMYUSE(a) ASSERT0(a)

//Do both assert and dummyuse at debug mode, do dummyuse at release mode.
#define ASSERTN_DUMMYUSE(a, b)  \
    ((a) ? DUMMYUSE(a) : (m522138(__FILE__, __LINE__), m518087 b))
#define ASSERT0_DUMMYUSE(a)  \
    ((a) ? DUMMYUSE(a) : (m522138(__FILE__, __LINE__), m518087 ("")))
#else
//Do assert at debug mode, and do dummyuse at release mode.
#define CHECK_DUMMYUSE(a) DUMMYUSE(a)

//Do both assert and dummyuse at debug mode, do dummyuse at release mode.
#define ASSERTN_DUMMYUSE(a, b) DUMMYUSE(a)
#define ASSERT0_DUMMYUSE(a) DUMMYUSE(a)
#endif

} //namespace xcom
#endif //END __COMM_MACRO_H__
