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

static inline LONGLONG gcdf(LONGLONG x, LONGLONG y)
{
    LONGLONG t;
    if (x < 0) { x = -x; }
    if (y < 0) { y = -y; }
    if (x > y) {
        t = x;
        x = y;
        y = t;
    }
    while (x) {
        t = x;
        x = y % x;
        y = t;
    }
    return y;
}


//Reduction of 64bit longlong integer.
void reduce_ll(LONGLONG & num, LONGLONG & den)
{
    if (num == 0) {
        den = 1;
        return;
    }
    LONGLONG gcd = gcdf(num, den);
    if (gcd == 1) {
        if (den < 0) {
            den = -den;
            num = -num;
        }
        return;
    }
    num = num / gcd;
    den = den / gcd;
    if (den < 0) {
        den = -den;
        num = -num;
    }
}


//Calculate the approximate rational number.
void appro(LONGLONG & num, LONGLONG & den)
{
    float v = float(num) / float(den);
    if (v < 100.0) {
        v = v * 1000000;
        num = INT(v);
        den = 1000000;
    } else if (v < 1000.0) {
        v = v * 100000;
        num = INT(v);
        den = 100000;
    } else if (v < 100000.0) {
        v = v * 10000;
        num = INT(v);
        den = 10000;
    } else if (v < 1000000.0) {
        v = v * 1000;
        num = INT(v);
        den = 1000;
    } else if (v < 10000000.0) {
        v = v * 100;
        num = INT(v);
        den = 100;
    } else if (v < 100000000.0) {
        v = v * 10;
        num = INT(v);
        den = 10;
    } else if (v < 2147483647.0) {
        num = INT(v);
        den = 1;
    } else {
        ASSERTN(0, ("overflow the range of integer, 0x7fffFFFF."));
        num = 0;
        den = 1;
    }
    reduce_ll(num, den);
}

} //namespace xcom
