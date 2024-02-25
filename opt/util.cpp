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
#include "cominc.h"

namespace xoc {

//Print \l as the Carriage Return.
//bool g_prt_carriage_return_for_dot = false;

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
    //#ifdef FOR_DEX
    //__android_log_vprint(ANDROID_LOG_ERROR, LOG_TAG, format, args);
    //#else
    if (g_redirect_stdout_to_dump_file && g_unique_dumpfile != nullptr) {
        vfprintf(g_unique_dumpfile, format, args);
        fflush(g_unique_dumpfile);
    } else {
        vfprintf(stdout, format, args);
        fflush(stdout);
    }
    //#endif
    va_end(args);
}


CHAR const* getHostIntFormat(bool hex)
{
    if (sizeof(HOST_INT) * HOST_BIT_PER_BYTE == 32) {
        return hex ? "%x" : "%d";
    }
    if (sizeof(HOST_INT) * HOST_BIT_PER_BYTE == 64) {
        return hex ? "%llx" : "%lld";
    }
    UNREACHABLE();
    return nullptr;
}


CHAR const* getHostUIntFormat(bool hex)
{
    if (sizeof(HOST_UINT) * HOST_BIT_PER_BYTE == 32) {
        return hex ? "%x" : "%u";
    }
    if (sizeof(HOST_UINT) * HOST_BIT_PER_BYTE == 64) {
        return hex ? "%llx" : "%llu";
    }
    UNREACHABLE();
    return nullptr;
}


CHAR const* getHostFPFormat()
{
    return "%f";
}


void dumpHostFP(HOST_FP fpval, Type const* ty, Region const* rg,
                TypeMgr const* tm)
{
    CHAR const* fpfmt = getHostFPFormat();
    CHAR fpformat[128];
    ::snprintf(fpformat, 127, "fpconst:%%s %s", fpfmt);
    xcom::StrBuf buf(16);
    prt(rg, fpformat, tm->dump_type(ty, buf), fpval);
}


void dumpHostInteger(HOST_UINT intval, Type const* ty, Region const* rg,
                     TypeMgr const* tm, bool is_sign)
{
    CHAR const* intfmt = nullptr;
    CHAR const* hexintfmt = nullptr;
    if (is_sign) {
        intfmt = getHostIntFormat(false);
        hexintfmt = getHostIntFormat(true);
    } else {
        intfmt = getHostUIntFormat(false);
        hexintfmt = getHostUIntFormat(true);
    }
    xcom::StrBuf fmt(64);
    xcom::StrBuf buf(64);
    fmt.strcat("intconst:%%s %s|0x%s", intfmt, hexintfmt);
    prt(rg, fmt.buf, tm->dump_type(ty, buf), intval, intval);
}

} //namespace xoc
