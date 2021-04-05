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

#ifndef va_copy
#define va_copy(d, s) ((d) = (s))
#endif

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

//This macro declare copy constructor for class.
#define COPY_CONSTRUCTOR(class_name) \
    class_name(class_name const&);   \
    class_name const& operator = (class_name const&)

} //namespace xcom

#endif //END __COMM_MACRO_H__
