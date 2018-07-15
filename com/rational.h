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
#ifndef __RATIONAL_H__
#define __RATIONAL_H__

namespace xcom {

#define FRAC_TYPE INT
class Rational {
    friend bool operator != (Rational const& a, Rational const& b);
    friend bool operator == (Rational const& a, Rational const& b);
    friend bool operator < (Rational const& a, Rational const& b);
    friend bool operator <= (Rational const& a, Rational const& b);
    friend bool operator > (Rational const& a, Rational const& b);
    friend bool operator >= (Rational const& a, Rational const& b);
    friend Rational operator * (Rational const& a, Rational const& b);
    friend Rational operator / (Rational const& a, Rational const& b);
    friend Rational operator + (Rational const& a, Rational const& b);
    friend Rational operator - (Rational const& a, Rational const& b);
    friend Rational operator - (Rational const& a);

    FRAC_TYPE m_num;
    FRAC_TYPE m_den;

    FRAC_TYPE _gcd(FRAC_TYPE x, FRAC_TYPE y);
public:
    Rational();
    Rational(Rational const& r);
    Rational(INT num, INT den = 1);
    Rational & operator = (Rational const& a);
    INT typecast2int() { return m_num / m_den; }
    bool is_int() { return m_den == 1; }
    void reduce();
    Rational rabs();
    FRAC_TYPE num() const {return m_num;}
    FRAC_TYPE& num() {return m_num;}
    FRAC_TYPE den() const {return m_den;}
    FRAC_TYPE& den() {return m_den;}
    CHAR const* format(StrBuf & buf);
};


#ifndef INT_MAX
#define INT_MAX        0x7fffFFFF
#endif


//Exported Functions
inline bool operator == (Rational const& a, Rational const& b)
{ return (a.m_num == b.m_num && a.m_den == b.m_den); }
inline bool operator != (Rational const& a, Rational const& b)
{ return (a.m_num != b.m_num || a.m_den != b.m_den); }
bool operator == (Rational const& a, Rational const& b);
bool operator != (Rational const& a, Rational const& b);
bool operator < (Rational const& a, Rational const& b);
bool operator <= (Rational const& a, Rational const& b);
bool operator > (Rational const& a, Rational const& b);
bool operator >= (Rational const& a, Rational const& b);
Rational operator * (Rational const& a, Rational const& b);
Rational operator / (Rational const& a, Rational const& b);
Rational operator + (Rational const& a, Rational const& b);
Rational operator - (Rational const& a, Rational const& b);
Rational operator - (Rational const& a);
//Subtraction
inline Rational operator - (Rational const& a, Rational const& b)
{ return a + (-b); }

//Minus
inline Rational operator - (Rational const& a)
{
    Rational b = a;
    b.m_num = -b.m_num;
    return b;
}

} //namespace xcom
#endif
