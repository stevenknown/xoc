/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include <windows.h>
#include <string>
#include <iostream>
#include "../com/xcominc.h"

static LARGE_INTEGER li = {0}, li2 = {0};

void tstart()
{
    QueryPerformanceFrequency(&li);
    __int64 freq = li.QuadPart;
    QueryPerformanceCounter(&li);
}


char * format_elapsed(double d, char * buf, UINT buflen)
{
    if( d < 0.00000001 ) {
        // show in ps with 4 digits
        SNPRINTF(buf, buflen, "%0.4f ps", d * 1000000000000.0);
    } else if( d < 0.00001 ) {
        // show in ns
        SNPRINTF(buf, buflen, "%0.0f ns", d * 1000000000.0);
    } else if( d < 0.001 ) {
        // show in us
        SNPRINTF(buf, buflen, "%0.0f us", d * 1000000.0);
    } else if( d < 0.1 ) {
        // show in ms
        SNPRINTF(buf, buflen, "%0.0f ms", d * 1000.0);
    } else if( d <= 60.0 ) {
        // show in seconds
        SNPRINTF(buf, buflen, "%0.2f s", d);
    } else if( d < 3600.0 ) {
        // show in min:sec
        SNPRINTF(buf, buflen, "%01.0f:%02.2f", floor(d/60.0), fmod(d,60.0));
    } else {
        // show in h:min:sec
        SNPRINTF(buf, buflen, "%01.0f:%02.0f:%02.2f",
                 floor(d/3600.0), floor(fmod(d,3600.0)/60.0), fmod(d,60.0));
    }
    return buf;
}



#include <time.h>
//Return time, in ps, ns, ms, or h:min:sec.
//buf: recommend to larger than 256 bytes at least.
char * tend(char * buf, UINT buflen)
{
    clock_t start = clock();
    //Do something
    clock_t end = clock();
    float seconds = (float)(end - start) / CLOCKS_PER_SEC;

    long long freq = 4294967296;
    QueryPerformanceCounter(&li2);
    __int64 ticks = li2.QuadPart-li.QuadPart;    
    buf[0] = '\0';
    format_elapsed((double)ticks/(double)freq, buf, buflen);
    return buf;
}
