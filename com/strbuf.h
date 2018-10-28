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
#ifndef _STRBUF_H_
#define _STRBUF_H_

namespace xcom {

class StrBuf {
public:
    CHAR * buf;
    UINT buflen;

public:
    StrBuf(UINT initsize)
    {
        ASSERT0(initsize > 0);
        buflen = initsize;
        buf = (CHAR*)::malloc(initsize);
        buf[0] = 0;
    }
    COPY_CONSTRUCTOR(StrBuf);
    ~StrBuf()
    {
        if (buf != NULL) {
            ::free(buf);
        }
    }

    void clean()
    {
        ASSERT0(buf);
        buf[0] = 0;
    }
    void sprint(CHAR const* format, ...);
    void vsprint(CHAR const* format, va_list args);
    void strcat(CHAR const* format, ...);
    void strcat(UINT l, CHAR const* format, va_list args);
    size_t strlen() const { return ::strlen(buf); }
    void nstrcat(UINT size, CHAR const* format, ...);
    void vstrcat(CHAR const* format, va_list args);
};

} //namespace xcom
#endif
