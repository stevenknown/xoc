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
#include "xcominc.h"

namespace xcom {

//Initialize global variable.
PRECISION_TYPE Float::g_e = PRECISION_TYPE_E;

bool Float::isApproEq(PRECISION_TYPE a, PRECISION_TYPE b)
{
    if ((a > 0 && b < 0) || (a < 0 && b > 0)) {
        return false;
    }
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    if((a == PRECISION_TYPE(0) && b < INFINITESIMAL) ||
       (b == PRECISION_TYPE(0) && a < INFINITESIMAL)) {
        return true;
    }
    if (a > b) {
        return (a - b) < INFINITESIMAL;
    }
    return (b - a) < INFINITESIMAL;
}


//Zerolization.
Float zerolinz(Float const& a)
{
    PRECISION_TYPE v = a.m_f;
    if (v < 0) {
        v = -v;
    }
    if (v <= INFINITESIMAL) {
        return Float(0);
    }
    return a;
}


PRECISION_TYPE integralize(PRECISION_TYPE const& a)
{
    PRECISION_TYPE av = a;
    bool is_neg = false;
    if (av < 0) {
        is_neg = true;
        av = -av;
    }
    LONGLONG iv = (LONGLONG)av;
    PRECISION_TYPE ifv = PRECISION_TYPE(iv);
    //1.000001 is approximate to 1.000000.
    if ((av - ifv) < INFINITESIMAL) {
        return is_neg ? -ifv : ifv;
    }

    //1.999999 is approximate to 2.000000.
    ifv = PRECISION_TYPE(iv + 1);
    if ((ifv - av) < INFINITESIMAL) {
        return is_neg ? -ifv : ifv;
    }
    //if (av < 0.0001) {
    //    return PRECISION_TYPE(0);
    //}
    return a;
}


CHAR const* Float::dump(StrBuf & buf) const
{
    buf.sprint("%f", m_f);
    return buf.buf;
}


bool Float::is_int() const
{
    PRECISION_TYPE av = m_f;
    if (av < 0) {
        av = -av;
    }
    LONGLONG iv = (LONGLONG)av;
    PRECISION_TYPE ifv = PRECISION_TYPE(iv);
    //1.000001 is approximate to 1.000000.
    if ((av - ifv) < INFINITESIMAL) {
        return true;
    }

    //1.999999 is approximate to 2.000000.
    ifv = PRECISION_TYPE(iv + 1);
    if ((ifv - av) < INFINITESIMAL) {
        return true;
    }
    return false;
}


void Float::dump() const
{
    StrBuf buf(16);
    dump(buf);
    fprintf(stdout, "%s", buf.buf);
}

} //namespace xcom
