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
#include "../com/xcominc.h"
#include "commoninc.h"
#include "errno.h"

namespace xoc {

#define ERR_BUF_LEN 1024

//Print \l as the Carriage Return.
//bool g_prt_carriage_return_for_dot = false;
static SMemPool * g_pool_tmp_used = NULL;
static CHAR g_indent_chars = ' ';

//Return true if val is 32bit integer.
bool isInteger32bit(HOST_UINT val)
{
    ASSERT0_DUMMYUSE(sizeof(HOST_UINT) >= sizeof(UINT32));
    return (((UINT32)val) & (UINT32)0x0000FFFF) != val;
}


//Return true if val is 64bit integer.
bool isInteger64bit(UINT64 val)
{
    return (val & (UINT64)0xFFFFFFFF) != val;
}


void interwarn(CHAR const* format, ...)
{
    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vsprint(format, arg);
    prt2C("\n!!!INTERNAL WARNING:%s\n\n", buf.buf);
    va_end(arg);
}


//Print message to console.
void prt2C(CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    #ifdef FOR_DEX
    __android_log_vprint(ANDROID_LOG_ERROR, LOG_TAG, format, args);
    #else
    if (g_redirect_stdout_to_dump_file && g_unique_dumpfile != NULL) {
        vfprintf(g_unique_dumpfile, format, args);
        fflush(g_unique_dumpfile);
    } else {
        vfprintf(stdout, format, args);
        fflush(stdout);
    }
    #endif
    va_end(args);
}


SMemPool * get_tmp_pool()
{
    return g_pool_tmp_used;
}


//Malloc memory for tmp used.
void * tlloc(LONG size)
{
    if (size < 0 || size == 0) { return NULL; }
    if (g_pool_tmp_used == NULL) {
        g_pool_tmp_used = smpoolCreate(8, MEM_COMM);
    }
    void * p = smpoolMalloc(size, g_pool_tmp_used);
    if (p == NULL) { return NULL; }
    ::memset(p, 0, size);
    return p;
}


void tfree()
{
    if (g_pool_tmp_used != NULL) {
        smpoolDelete(g_pool_tmp_used);
        g_pool_tmp_used = NULL;
    }
}

} //namespace xoc
