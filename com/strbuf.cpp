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
#ifdef _ON_WINDOWS_
#include <time.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

#include "xcominc.h"

namespace xcom {

void StrBuf::vstrcat(UINT bytesize, CHAR const* format, va_list args)
{
    if (bytesize == 0) {
        //'format' is emtpy string: "".
        return;
    }
    size_t sl = ::strlen(buf);
    if (buflen - sl <= bytesize) {
        CHAR * oldbuf = buf;
        buflen += bytesize + 1;
        buf = (CHAR*)::malloc(buflen);
        ::memcpy(buf, oldbuf, sl);
        ::free(oldbuf);
    }
    //NOTE: the function will put number of 'bytesize' characters into buf,
    //and append a '\0' at the end. Thus the buf+sl+bytesize+1 must less or
    //equal then the buflen. Or else VSNPRINTF might return -1 in some C lib.
    UINT k = VSNPRINTF(buf + sl, bytesize, format, args);

    //NOTE:VSNPRINTF return the byte size of the formatted string.
    //CASE:Some C library returns -1 if 'bytesize' less than the
    //characters byte length of formatted string.
    if (k >= bytesize || k == (UINT)-1) {
        //If k is equal to bytesize, that means the buffer is smaller than
        //formatted string, set the last charactor of buffer to '0'.
        bytesize = bytesize - 1;
    }
    buf[sl + bytesize] = 0;
    DUMMYUSE(k);
}


LONG StrBuf::findSubStr(CHAR const* substring)
{
    return xstrstr(getBuf(), substring);
}


void StrBuf::strcat(CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    va_list org_args;
    va_copy(org_args, args);
    UINT l = VSNPRINTF(nullptr, 0, format, args);
    l++; //Enlarge buffer to hold the end '0'.
    vstrcat(l, format, org_args);
    va_end(args);
    va_end(org_args);
}


void StrBuf::strcat(StrBuf const& another)
{
    strcat("%s", another.getBuf());
}


void StrBuf::vstrcat(CHAR const* format, va_list args)
{
    va_list org_args;
    va_copy(org_args, args);
    UINT l = VSNPRINTF(nullptr, 0, format, args);
    l++; //Enlarge buffer to hold the end '0'.
    vstrcat(l, format, org_args);
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
    l++; //Enlarge buffer to hold the end '0'.
    vstrcat(l, format, org_args);
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


void StrBuf::sprint(UINT bytesize, CHAR const* format, ...)
{
    clean();
    va_list args;
    va_start(args, format);
    va_list org_args;
    va_copy(org_args, args);
    vstrcat(bytesize, format, org_args);
    va_end(args);
    va_end(org_args);
}


void StrBuf::strcat(UINT bytesize, CHAR const* format, ...)
{
    ASSERT0(bytesize > 0);
    va_list args;
    va_start(args, format);
    va_list org_args;
    va_copy(org_args, args);

    //Should reserve one byte for storing the terminating null byte ('\0').
    vstrcat(bytesize, format, org_args);
    va_end(args);
    va_end(org_args);
}


void StrBuf::toByteHex(OUT BYTE * outputbuf, UINT bufl)
{
    xcom::charToByteHex(buf, outputbuf, bufl);
}

}//namespace xcom
