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
#include "cominc.h"

namespace xoc {

void dumpHostFP(HOST_FP val, Type const* ty, BYTE mantissa, Region const* rg,
                OUT StrBuf & buf)
{
    CHAR fpformat[128];
    ::snprintf(fpformat, 127, "fpconst:%%s %%.%df", mantissa);
    xcom::StrBuf lbuf(16);
    buf.sprint(fpformat, rg->getTypeMgr()->dump_type(ty, lbuf), val);
}


void dumpHostInt(HOST_INT val, Type const* ty, Region const* rg,
                 OUT StrBuf & buf)
{
    CHAR const* intfmt = getHostIntFormat(false);
    CHAR const* hexintfmt = getHostIntFormat(true);
    StrBuf fmt(16);
    fmt.sprint("intconst:%%s %s|0x%s", intfmt, hexintfmt);
    xcom::StrBuf lbuf(16);
    buf.sprint(fmt.buf, rg->getTypeMgr()->dump_type(ty, lbuf), val, val);
}


void dumpHostFP(HOST_FP val, OUT StrBuf & buf, BYTE mantissa)
{
    ASSERT0(mantissa < DEFAULT_MANTISSA_NUM);
    CHAR fmt[128];
    ::snprintf(fmt, 127, "%%.%df", mantissa);
    buf.strcat(fmt, val);
}


void dumpHostInt(HOST_INT val, bool is_signed, bool is_hex, OUT StrBuf & buf)
{
    if (is_hex) {
        CHAR const* fmt = getHostUIntFormat(true);
        buf.strcat(fmt, val);
        return;
    }
    if (is_signed) {
        CHAR const* fmt = getHostIntFormat(false);
        buf.strcat(fmt, val);
        return;
    }
    CHAR const* fmt = getHostUIntFormat(false);
    buf.strcat(fmt, val);
}

} //namespace xoc
