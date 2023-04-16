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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __LITERAL_H__
#define __LITERAL_H__

namespace xoc {

#define HOST_FP_DEFAULT_DUMP_MANTISSA 32

class Type;

//The function dump Host Machine Integer number into given buffer.
//val: integer value that can be represented with HOST_INT.
//is_siged: expect val to be signed integer number.
//is_hex: expect val to be hexadecimal number.
//buf: output string buffer.
void dumpHostInt(HOST_INT val, bool is_signed, bool is_hex,
                 OUT StrBuf & buf);

//The function dump Host Machine Integer number into given buffer.
//val: integer value that can be represented with HOST_INT.
//ty: the data-type of 'val'.
//buf: output string buffer.
void dumpHostInt(HOST_INT intval, Type const* ty, Region const* rg,
                 OUT StrBuf & buf);

//The function dump Host Machine Float Point number into given buffer.
//val: float-pointe number that can be represented with HOST_INT.
//buf: output string buffer.
//mantissa: the number of float mantissa in output buffer.
//          e.g:given 1.123456, if mantissa is 3, the output is 1.123.
void dumpHostFP(HOST_FP val, OUT StrBuf & buf,
                BYTE mantissa = DEFAULT_MANTISSA_NUM);

//The function dump Host Machine Float Point number into given buffer.
//val: float-pointe number that can be represented with HOST_INT.
//ty: the data-type of 'val'.
//mantissa: the number of float mantissa in output buffer.
//          e.g:given 1.123456, if mantissa is 3, the output is 1.123.
//buf: output string buffer.
void dumpHostFP(HOST_FP val, Type const* ty, BYTE mantissa,
                Region const* rg, OUT StrBuf & buf);

} //namespace xoc
#endif
