/*@
Copyright (c) 2026-present, Su Zhenyu steven.known@gmail.com

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
#include "xinfer_cominc.h"

#ifdef __cplusplus
extern "C" {
#endif

static UINT g_count = 0;

bool dumpF32Vec(
    void * lm, float const* h, unsigned int n, char const* format,
    va_list args)
{
    LogMgr * lmp = (LogMgr*)lm;
    if (lmp == nullptr || !lmp->is_init()) { return true; }
    xoc::note(lmp, "\n[%u]", g_count);
    g_count++;
    if (format != NULL) {
        xoc::note_arg(lmp, format, args);
        xoc::prt(lmp, ",");
    }
    for (unsigned int i = 0; i < n; i++) {
        if (i != 0) { xoc::prt(lmp, ","); }
        xoc::prt(lmp, "h[%u]=%f", i, h[i]);
    }
    return true;
}


bool dumpL2Norm(
    void * lm, void const* h, unsigned int num, RTTENSOR_DTYPE dtype,
    char const* format, va_list args)
{
    LogMgr * lmp = (LogMgr*)lm;
    if (lmp == nullptr || !lmp->is_init()) { return true; }
    xoc::note(lmp, "\n[%u]", g_count);
    g_count++;
    if (format != NULL) {
        xoc::note_arg(lmp, format, args);
        xoc::prt(lmp, ",");
    }
    float norm = 0; //The L2 norm.
    unsigned int elemsz = getRTTensorDTypeByteSize(dtype);
    ASSERT0(elemsz != 0);
    for (unsigned int i = 0; i < num; i++) {
        float v = tofloat(((unsigned char*)h) + i * elemsz, dtype);
        norm += v*v;
    }
    norm = sqrtf(norm);
    xoc::prt(lmp, "l2norm=%f", norm);
    unsigned upper = num > 4 ? 4 : num;
    for (unsigned int i = 0; i < upper; i++) {
        xoc::prt(lmp, ",h[%u]=%f",
            i, tofloat(((unsigned char*)h) + i * elemsz, dtype));
    }
    return true;
}


bool dumpNormInfoVar(void * lm, char const* format, ...)
{
    va_list args;
    va_start(args, format);
    dumpNormInfo(lm, format, args);
    va_end(args);
    return true;
}


bool dumpNormInfo(void * lm, char const* format, va_list args)
{
    LogMgr * lmp = (LogMgr*)lm;
    if (lmp == nullptr || !lmp->is_init()) { return true; }
    xoc::note(lmp, "\n[%u]", g_count);
    g_count++;
    ASSERT0(format);
    xoc::note_arg(lmp, format, args);
    return true;
}


bool dumpTopN(
    void * lm, int64_t const* top_ids, float const* top_vals, unsigned int n,
    char const* format, va_list args)
{
    LogMgr * lmp = (LogMgr*)lm;
    if (lmp == nullptr || !lmp->is_init()) { return true; }
    xoc::note(lmp, "\n[%u]DUMP TOP%u", g_count, n);
    g_count++;
    if (format != NULL) {
        xoc::prt(lmp, ",");
        xoc::prt_arg(lmp, format, args);
    }
    xoc::prt(lmp, ":");
    for (unsigned int t = 0; t < n; t++) {
        xoc::note(lmp, "\n  top%u:tokid=%d,val=%f", t, top_ids[t], top_vals[t]);
    }
    return true;
}

#ifdef __cplusplus
}
#endif


