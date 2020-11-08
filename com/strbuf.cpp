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
#ifdef _ON_WINDOWS_
#include <time.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

#include "xcominc.h"

namespace xcom {

void StrBuf::strcat(UINT l, CHAR const* format, va_list args)
{
    size_t sl = ::strlen(buf);
    if (buflen - sl <= l) {
        CHAR * oldbuf = buf;
        buflen += l + 1;
        buf = (CHAR*)malloc(buflen);
        ::memcpy(buf, oldbuf, sl);
        free(oldbuf);
    }
    UINT k = VSNPRINTF(buf + sl, l + 1, format, args);
    ASSERT0(k == l);
    DUMMYUSE(k);
}


void StrBuf::strcat(CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    va_list org_args;
    va_copy(org_args, args);
    UINT l = VSNPRINTF(nullptr, 0, format, args);
    strcat(l, format, org_args);
    va_end(args);
    va_end(org_args);
}


void StrBuf::vstrcat(CHAR const* format, va_list args)
{
    va_list org_args;
    va_copy(org_args, args);
    UINT l = VSNPRINTF(nullptr, 0, format, args);
    strcat(l, format, org_args);
    va_end(org_args);
}


void StrBuf::sprint(CHAR const* format, ...)
{
    clean();
    va_list args;
    va_start(args, format);
    va_list org_args;
    va_copy(org_args, args);
    UINT l = VSNPRINTF(nullptr, 0, format, args);
    strcat(l, format, org_args);
    va_end(args);
    va_end(org_args);
}


//This function print string according to 'format'.
//args: a list of argument store in stack.
void StrBuf::vsprint(CHAR const* format, va_list args)
{
    clean();
    vstrcat(format, args);
}


//The functions snprintf() and vsnprintf() do not write more than size
//bytes (including the terminating null byte ('\0')).
//size: the maximum possible byte size of string.
void StrBuf::nstrcat(UINT size, CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    va_list org_args;
    va_copy(org_args, args);
    UINT l = VSNPRINTF(nullptr, 0, format, args);
    if (l > size) { l = size; }
    strcat(l, format, org_args);
    va_end(args);
    va_end(org_args);
}

}//namespace xcom
