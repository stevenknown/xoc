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
#ifndef __BIRATIONAL_H__
#define __BIRATIONAL_H__

namespace xcom {

class BIRational {
    friend bool operator != (BIRational const& a, BIRational const& b);
    friend bool operator == (BIRational const& a, BIRational const& b);
    friend bool operator < (BIRational const& a, BIRational const& b);
    friend bool operator <= (BIRational const& a, BIRational const& b);
    friend bool operator > (BIRational const& a, BIRational const& b);
    friend bool operator >= (BIRational const& a, BIRational const& b);
    friend BIRational operator * (BIRational const& a, BIRational const& b);
    friend BIRational operator / (BIRational const& a, BIRational const& b);
    friend BIRational operator + (BIRational const& a, BIRational const& b);
    friend BIRational operator - (BIRational const& a, BIRational const& b);
    friend BIRational operator - (BIRational const& a);

private:
    BigInt m_num;
    BigInt m_den;

public:
    BIRational() { m_num = BigInt(1, 0); m_den = BigInt(1, 1); }
    BIRational(BIRational const& src)
    {
        //Sometimes, src need not to be initialized always.
        //ASSERTN(src.m_den != 0, ("denominator is 0!"));
        copy(src);
    }
    BIRational(BigIntElemType num, BigIntElemType den = 1) { set(num, den); }
    BIRational(BigInt num, BigInt den) { set(num, den); }
    BIRational & operator = (BIRational const& src)
    {
        copy(src);
        return *this;
    }

    void copy(BIRational const& src)
    {
        ASSERTN(src.m_den != BigInt(1, 0), ("denominator is 0!"));
        m_num.copy(src.m_num);
        m_den.copy(src.m_den);
    }

    bool is_int() { return m_den == BigInt(1,1); }
    BIRational rabs();
    BigInt num() const { return m_num; }
    BigInt& num() { return m_num; }
    BigInt den() const { return m_den; }
    BigInt& den() { return m_den; }
    void dump(char const* name);

    void set(BigIntElemType num, BigIntElemType den)
    {
        ASSERTN(den != BigInt(1, 0), ("denominator can not be 0"));
        m_num.initElem(1, num);
        m_den.initElem(1, den);
    }
    void set(BigInt const& num, BigInt const& den)
    {
        ASSERTN(den != BigInt(1, 0), ("denominator can not be 0"));
        m_num.copy(num);
        m_den.copy(den);
    }
};

//Exported Functions
inline bool operator == (BIRational const& a, BIRational const& b)
{ return (a.m_num == b.m_num && a.m_den == b.m_den); }
inline bool operator != (BIRational const& a, BIRational const& b)
{ return (a.m_num != b.m_num || a.m_den != b.m_den); }
bool operator == (BIRational const& a, BIRational const& b);
bool operator != (BIRational const& a, BIRational const& b);
bool operator < (BIRational const& a, BIRational const& b);
bool operator <= (BIRational const& a, BIRational const& b);
BIRational operator * (BIRational const& a, BIRational const& b);
BIRational operator / (BIRational const& a, BIRational const& b);
BIRational operator + (BIRational const& a, BIRational const& b);
BIRational operator - (BIRational const& a, BIRational const& b);
BIRational operator - (BIRational const& a);
//Subtraction
inline BIRational operator - (BIRational const& a, BIRational const& b)
{ return a + (-b); }

//Minus
inline BIRational operator - (BIRational const& a)
{
    BIRational b = a;
    b.m_num.neg();
    return b;
}

inline bool operator > (BIRational const& a, BIRational const& b)
{
    return !(a <= b);
}

inline bool operator >= (BIRational const& a, BIRational const& b)
{
    return !(a < b);
}

} //namespace xcom
#endif
