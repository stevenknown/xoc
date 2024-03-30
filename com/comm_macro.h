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

#undef GET_LOW_12BIT
#define GET_LOW_12BIT(l) ((l)&(0xfff))

#undef GET_LOW_16BIT
#define GET_LOW_16BIT(l) ((l)&(0xffff))

#undef GET_SIGN_LOW_16BIT
#define GET_SIGN_LOW_16BIT(l) (((l&0xffff)^0x8000)-0x8000)

#undef GET_HIGH_16BIT
#define GET_HIGH_16BIT(l) (((l)>>16)&0xffff)

#undef GET_SIGN_HIGH_16BIT
#define GET_SIGN_HIGH_16BIT(l) (((l&0xffffffff)^0x80000000)-0x80000000)

#undef GET_LOW_18BIT
#define GET_LOW_18BIT(l) ((l)&(0x3ffff))

#undef GET_LOW_22BIT
#define GET_LOW_22BIT(l) ((l)&(0x3fffff))

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

#undef MOD
#define MOD //indicate modified argument

//This macro declare copy constructor for class.
#define COPY_CONSTRUCTOR(class_name) \
    class_name(class_name const&);   \
    class_name const& operator = (class_name const&)

#define COPY_CONSTRUCTOR_ASSIGN(class_name) \
    class_name const& operator = (class_name const&)

#define BITS_PER_BYTE 8  //Bit number of Byte.

//A macro that gets the negative value of a positive integer.
//For example, i is the representation of 888 of 64-bit in memory, and
//NEG_INT(i) will return the representation of -888 of 64-bit in memory.
//888:  0000000000000000000000000000000000000000000000000000001101111000
//~888: 1111111111111111111111111111111111111111111111111111110010000111
//-888: 1111111111111111111111111111111111111111111111111111110010001000
#define NEG_INT(i) (~(i) + 0x1ULL)

//A macro that convert a double value to a half value. This macro will extract
//sign, exponent and fraction in double, then combine them into a value and
//shift it to get the first 16-bit element.
//For example:
//the double representation of -27.15625 is:
//1 10000000011 1011001010000000000000000000000000000000000000000000
// (1) extract sign and first elem of exponent to get:
//1 10000000000 0000000000000000000000000000000000000000000000000000
// (2) extract the last four elements of exponent to get:
//0 00000000011 0000000000000000000000000000000000000000000000000000
// (3) extract the first ten elements of fraction to get:
//0 00000000000 1011001010000000000000000000000000000000000000000000
// (4) do or operation and shift the result to 64-bit representation:
//00000000000000000000000000000000000000000000000001 10011 1011001010
//the half representation of 123.456 is:
//1       10011 1011001010
//sign: 1 1
#define DOUBLE2HALF(i) ((i & 0xC000000000000000) | \
                       ((i & 0xF0000000000000) << 6) | \
                       ((i & 0xFFC0000000000) << 6)) >> 48

typedef UINT BSIdx;
#define BS_UNDEF ((BSIdx)-1) //The maximum unsigned integer

} //namespace xcom

#endif //END __COMM_MACRO_H__
