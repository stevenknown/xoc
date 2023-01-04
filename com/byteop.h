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
#ifndef __BYTEOP_H__
#define __BYTEOP_H__

namespace xcom {

#if BITS_PER_BYTE == 8
#define DIVBPB(a) ((a) >> 3)
#define MULBPB(a) ((a) << 3)
#define MODBPB(a) ((a) & 7)
#else
#define DIVBPB(a) ((a) / BITS_PER_BYTE)
#define MULBPB(a) ((a) * BITS_PER_BYTE)
#define MODBPB(a) ((a) % BITS_PER_BYTE)
#endif

//Count of bits in all the one byte numbers.
extern BYTE const g_bit_count[256];

//Mapping from 8 bit unsigned integers to the index of the first one bit.
extern BYTE const g_first_one[256];

//Mapping from 8 bit unsigned integers to the index of the last one bit.
extern BYTE const g_last_one[256];

class ByteOp {
public:
    //Return the bit index of first '1', start from '0'.
    //Return BS_UNDEF if there is no '1' in given byte buffer.
    static BSIdx get_first_idx(BYTE const* ptr, UINT bytesize);

    //Return the next bit index of '1' according to given 'idx'.
    //Return BS_UNDEF if there is no other '1' in given buffer.
    static BSIdx get_next_idx(BYTE const* ptr, UINT bytesize, BSIdx idx);

    //Return the number of '1' in given buffer.
    static UINT get_elem_count(BYTE const* ptr, UINT bytesize);
};

} //namespace xcom
#endif //END __BYTEOP_H__
