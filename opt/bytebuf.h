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
#ifndef _BYTEBUF_H_
#define _BYTEBUF_H_

namespace xoc {

#define BYTEBUF_size(b) ((b)->m_byte_size)
#define BYTEBUF_buffer(b) ((b)->m_byte_buffer)
class ByteBuf {
    COPY_CONSTRUCTOR(ByteBuf);
public:
    UINT m_byte_size;
    BYTE * m_byte_buffer;
public:
    static void dump(OUT FileObj & fo, BYTE const* buf, UINT len);
    void dump(OUT FileObj & fo) const;

    BYTE * getBuffer() const { return m_byte_buffer; }
    UINT getSize() const { return m_byte_size; }

    //Set given value into current byte-buffer.
    //byteofst: the byte offset from current buffer. New value will be set
    //          start from it.
    //val: the buffer that record new value to be set.
    //valbytesize: the bytesize of new value.
    void setVal(UINT byteofst, BYTE const* val, UINT valbytesize);
};

} //namespace xoc
#endif
