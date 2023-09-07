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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

void ByteBuf::setVal(UINT byteofst, BYTE const* val, UINT valbytesize)
{
    ASSERT0(valbytesize > 0);
    ASSERT0(BITS_PER_BYTE == 8);
    ASSERT0(byteofst + valbytesize <= BYTEBUF_size(this));
    switch (valbytesize) {
    case 1:
        *(m_byte_buffer + byteofst) = *val;
        return;
    case 2:
        *(UINT16*)(m_byte_buffer + byteofst) = *(UINT16*)val;
        return;
    case 3:
        *(UINT16*)(m_byte_buffer + byteofst) = *(UINT16*)val;
        byteofst += sizeof(UINT16);
        *(m_byte_buffer + byteofst) = *val;
        return;
    case 4:
        *(UINT32*)(m_byte_buffer + byteofst) = *(UINT32*)val;
        return;
    case 5:
        *(UINT32*)(m_byte_buffer + byteofst) = *(UINT32*)val;
        byteofst += sizeof(UINT32);
        *(m_byte_buffer + byteofst) = *val;
        return;
    case 6:
        *(UINT32*)(m_byte_buffer + byteofst) = *(UINT32*)val;
        byteofst += sizeof(UINT32);
        *(UINT16*)(m_byte_buffer + byteofst) = *(UINT16*)val;
        return;
    case 7:
        *(UINT32*)(m_byte_buffer + byteofst) = *(UINT32*)val;
        byteofst += sizeof(UINT32);
        *(UINT16*)(m_byte_buffer + byteofst) = *(UINT16*)val;
        byteofst += sizeof(UINT16);
        *(m_byte_buffer + byteofst) = *val;
        return;
    case 8:
        *(UINT64*)(m_byte_buffer + byteofst) = *(UINT64*)val;
    default:
        ::memcpy(m_byte_buffer + byteofst, val, valbytesize);
    }
}


void ByteBuf::dump(OUT StrBuf & strbuf, BYTE const* buf, UINT len)
{
    strbuf.strcat("\n");
    for (UINT i = 0; i < len; i++) {
        strbuf.strcat("0x%02x,", buf[i]);
    }
}


void ByteBuf::dump(OUT FileObj & fo, BYTE const* buf, UINT len)
{
    fo.prt("\n");
    for (UINT i = 0; i < len; i++) {
        fo.prt("0x%02x,", buf[i]);
    }
}


void ByteBuf::dump(OUT StrBuf & strbuf) const
{
    dump(strbuf, m_byte_buffer, m_byte_size);
}


void ByteBuf::dump(FileObj & fo) const
{
    dump(fo, m_byte_buffer, m_byte_size);
}

} //namespace xoc
