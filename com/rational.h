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

typedef INT DefRationalFType;

#ifndef FTYPE_INT_MAX
#define FTYPE_INT_MAX ((DefRationalFType)0x7fffFFFF)
#endif

#define REDUCE_NUM_DEN

//Forward declaration.
template <typename FType> class TRational;

template <typename FType>
bool operator != (const TRational<FType>& a, const TRational<FType>& b);

template <typename FType>
bool operator == (const TRational<FType>& a, const TRational<FType>& b);

template <typename FType>
bool operator < (const TRational<FType>& a, const TRational<FType>& b);

template <typename FType>
bool operator <= (TRational<FType> const& a, TRational<FType> const& b);

template <typename FType>
bool operator > (TRational<FType> const& a, TRational<FType> const& b);

template <typename FType>
bool operator >= (TRational<FType> const& a, TRational<FType> const& b);

template <typename FType>
TRational<FType> operator * (
    TRational<FType> const& a, TRational<FType> const& b);

template <typename FType>
TRational<FType> operator / (
    TRational<FType> const& a, TRational<FType> const& b);

template <typename FType>
TRational<FType> operator + (
    TRational<FType> const& a, TRational<FType> const& b);

template <typename FType>
TRational<FType> operator - (
    TRational<FType> const& a, TRational<FType> const& b);

template <typename FType>
TRational<FType> operator - (TRational<FType> const& a);

//Templated Rational Type.
//FType, fraction type, that defines the fraction type of numerator and
//denominator. Usually FType must be Signed Integer type, such as INT.
template <class FType>
class TRational {
public:
    friend bool operator != <FType> (TRational const& a, TRational const& b);
    friend bool operator == <FType> (TRational const& a, TRational const& b);
    friend bool operator < <FType> (TRational const& a, TRational const& b);
    friend bool operator < <FType> (TRational const& a, TRational const& b);
    friend bool operator <= <FType> (TRational const& a, TRational const& b);
    friend bool operator > <FType> (TRational const& a, TRational const& b);
    friend bool operator >= <FType> (TRational const& a, TRational const& b);
    friend TRational<FType> operator * <FType> (
        TRational const& a, TRational const& b);
    friend TRational operator / <FType> (
        TRational const& a, TRational const& b);
    friend TRational operator + <FType> (
        TRational const& a, TRational const& b);
    friend TRational operator - <FType> (
        TRational const& a, TRational const& b);
    friend TRational operator - <FType> (TRational const& a);
protected:
    FType m_num;
    FType m_den;
protected:
    FType _gcd(FType x, FType y);
public:
    TRational();
    TRational(FType num, FType den = 1);

    TRational abs();

    FType den() const { return m_den; }
    FType & den() { return m_den; }
    CHAR const* dump(StrBuf & buf) const;
    void dump() const;

    FType num() const { return m_num; }
    FType & num() { return m_num; }

    bool is_int() { return m_den == 1; }

    void reduce();

    FType typecast2int() { return m_num / m_den; }

    bool verify() const;
};

//
//START TRational
//
template <class FType>
TRational<FType>::TRational() : m_num(0), m_den(1)
{}

template <class FType>
TRational<FType>::TRational(FType num, FType den)
{
    ASSERTN(den != 0, ("denominator is 0!"));
    m_num = num, m_den = den;
}

template <class FType>
bool TRational<FType>::verify() const
{
    ASSERTN(m_den != 0, ("denominator is 0!"));
    return true;
}

template <class FType>
void TRational<FType>::reduce()
{
    if (m_num == 0) {
        m_den = 1;
        return;
    }
    FType gcd = _gcd(m_num, m_den);
    if (gcd == 1) {
        if (m_den < 0) {
            m_den = -m_den;
            m_num = -m_num;
        }
        return;
    }
    m_num = m_num / gcd;
    m_den = m_den / gcd;
    if (m_den < 0) {
        m_den = -m_den;
        m_num = -m_num;
    }
    return;
}

template <class FType>
TRational<FType> TRational<FType>::abs()
{
    ASSERTN(m_den != 0, ("denominator is 0!"));
    TRational<FType> b(*this);
    if (b.m_num < 0) {
        b.m_num = -b.m_num;
    }
    if (b.m_den < 0) {
        b.m_den = -b.m_den;
    }
    return b;
}

template <class FType>
FType TRational<FType>::_gcd(FType x, FType y)
{
    FType t;
    if (x < 0) { x = -x; }
    if (y < 0) { y = -y; }
    if ( x > y ) {
        t = x, x = y, y = t;
    }
    while (x != 0) {
        t = x;
        x = y % x;
        y = t;
    }
    return y;
}

template <class FType>
CHAR const* TRational<FType>::dump(StrBuf & buf) const
{
    if (m_den == 1) {
        buf.sprint("%d", m_num);
    } else {
        buf.sprint("%d/%d", m_num, m_den);
    }
    return buf.buf;
}

template <class FType>
void TRational<FType>::dump() const
{
    StrBuf buf(16);
    dump(buf);
    fprintf(stdout, "%s", buf.buf);
}
//END TRational

void reduce_ll(LONGLONG & num, LONGLONG & den);
void appro(LONGLONG & num, LONGLONG & den);

//Exported Functions
template <class FType>
bool operator == (TRational<FType> const& a, TRational<FType> const& b)
{
    return (a.m_num == b.m_num && a.m_den == b.m_den);
}

template <class FType>
bool operator != (TRational<FType> const& a, TRational<FType> const& b)
{
    return (a.m_num != b.m_num || a.m_den != b.m_den);
}

template <class FType>
bool operator < (TRational<FType> const& a, TRational<FType> const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    if ((LONGLONG)a.m_num * (LONGLONG)b.m_den <
        (LONGLONG)a.m_den * (LONGLONG)b.m_num) {
        return true;
    }
    return false;
}

template <class FType>
bool operator <= (TRational<FType> const& a, TRational<FType> const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    if (((LONGLONG)(a.m_num) * (LONGLONG)(b.m_den)) <=
        ((LONGLONG)(a.m_den) * (LONGLONG)(b.m_num))) {
        return true;
    }
    return false;
}

template <class FType>
TRational<FType> operator * (
    TRational<FType> const& a, TRational<FType> const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    LONGLONG rnum = (LONGLONG)(a.m_num) * (LONGLONG)(b.m_num);
    TRational<FType> rat;
    if (rnum == 0) {
        rat.m_num = 0;
        rat.m_den = 1;
        return rat;
    }
    LONGLONG rden = (LONGLONG)(a.m_den) * (LONGLONG)(b.m_den);
    ASSERTN(rden != 0, ("den is zero"));
    if (rnum == rden) { rat.m_num = 1; rat.m_den = 1; return rat; }
    if (rnum == -rden) { rat.m_num = -1; rat.m_den = 1; return rat; }
    if ((rnum < 0 && rden < 0) || rden < 0) { rnum = -rnum; rden = -rden; }
    #ifdef REDUCE_NUM_DEN
    reduce_ll(rnum, rden);
    #endif
    ASSERT0(rden > 0);
    LONGLONG trnum = (LONGLONG)::abs((FType)rnum);
    if ((trnum >= (LONGLONG)(FTYPE_INT_MAX>>2)) ||
        (rden >= (LONGLONG)(FTYPE_INT_MAX>>2))) {
        reduce_ll(trnum, rden);
        if ((trnum >= (LONGLONG)(FTYPE_INT_MAX)) ||
            (rden >= (LONGLONG)(FTYPE_INT_MAX))) {
            appro(trnum, rden);
            ASSERT0((trnum < (LONGLONG)(FTYPE_INT_MAX)) &&
                     (rden < (LONGLONG)(FTYPE_INT_MAX)));
        }
    }

    //Enforce conversion from 'int64' to 'int32',
    //even if it possible loss of data.
    rat.m_num = (FType)(rnum < 0 ? -trnum : trnum);
    rat.m_den = (FType)rden;
    return rat;
}

template <class FType>
bool operator > (TRational<FType> const& a, TRational<FType> const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    if (((LONGLONG)(a.m_num) * (LONGLONG)(b.m_den)) >
        ((LONGLONG)(a.m_den) * (LONGLONG)(b.m_num))) {
        return true;
    }
    return false;
}

template <class FType>
bool operator >= (TRational<FType> const& a, TRational<FType> const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    if (((LONGLONG)(a.m_num) * (LONGLONG)(b.m_den)) >=
        ((LONGLONG)(a.m_den) * (LONGLONG)(b.m_num))) {
        return true;
    }
    return false;
}

template <class FType>
TRational<FType> operator / (
    TRational<FType> const& a, TRational<FType> const& b)
{
    FType anum = a.m_num;
    FType aden = a.m_den;
    FType bnum = b.m_num;
    FType bden = b.m_den;

    ASSERTN(aden != 0 && bden != 0, ("denominator is 0"));
    ASSERTN(bnum != 0, ("'a' divided by 0"));

    TRational<FType> rat;
    if (anum == 0) { rat.m_num = 0; rat.m_den = 1; return rat; }
    if (anum == aden) {
        if (bnum < 0) {
            rat.m_num = -bden;
            rat.m_den = -bnum;
        } else {
            rat.m_num = bden;
            rat.m_den = bnum;
        }
        return rat;
    }

    LONGLONG ratnum = (LONGLONG)(anum) * (LONGLONG)(bden);
    LONGLONG ratden = (LONGLONG)(aden) * (LONGLONG)(bnum);
    if (ratnum == ratden) { rat.m_num = 1; rat.m_den = 1; return rat; }
    if (ratnum == -ratden) { rat.m_num = -1; rat.m_den = 1; return rat; }
    if ((ratnum < 0 && ratden < 0) || ratden < 0) {
        ratnum = -ratnum; ratden = -ratden;
    }
    #ifdef REDUCE_NUM_DEN
    reduce_ll(ratnum, ratden);
    #endif
    ASSERT0(ratden > 0);
    LONGLONG trnum = (LONGLONG)::abs((FType)ratnum);
    if ((trnum >= (LONGLONG)(FTYPE_INT_MAX >> 2)) ||
        (ratden >= (LONGLONG)(FTYPE_INT_MAX >> 2))) {
        reduce_ll(trnum, ratden);
        if ((trnum >= (LONGLONG)(FTYPE_INT_MAX)) ||
            (ratden >= (LONGLONG)(FTYPE_INT_MAX))) {
            appro(trnum, ratden);
            ASSERT0((trnum < (LONGLONG)(FTYPE_INT_MAX)) &&
                    (ratden < (LONGLONG)(FTYPE_INT_MAX)));
        }
    }
    rat.m_num = (FType)(ratnum < 0 ? -trnum : trnum);
    rat.m_den = (FType)ratden;
    return rat;
}

template <class FType>
TRational<FType> operator + (
    TRational<FType> const& a, TRational<FType> const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    TRational<FType> rat;
    LONGLONG rnum = (LONGLONG)(a.m_num) * (LONGLONG)(b.m_den) +
                    (LONGLONG)(a.m_den) * (LONGLONG)(b.m_num);
    if (rnum == 0) {
        rat.m_num = 0;
        rat.m_den = 1;
        return rat;
    }
    LONGLONG rden = (LONGLONG)(a.m_den) * (LONGLONG)(b.m_den);
    ASSERTN(rden != 0, ("den is 0"));
    if (rnum == rden) { rat.m_num = 1; rat.m_den = 1; return rat; }
    if (rnum == -rden) { rat.m_num = -1; rat.m_den = 1; return rat; }
    if ((rnum < 0 && rden < 0) || rden < 0) { rnum = -rnum; rden = -rden; }
    #ifdef REDUCE_NUM_DEN
    reduce_ll(rnum, rden);
    #endif
    ASSERT0(rden > 0);
    LONGLONG trnum = (LONGLONG)::abs((FType)rnum);
    if ((trnum >= (LONGLONG)(FTYPE_INT_MAX>>2)) ||
        (rden >= (LONGLONG)(FTYPE_INT_MAX>>2))) {
        reduce_ll(trnum, rden);
        if ((trnum >= (LONGLONG)(FTYPE_INT_MAX)) ||
            (rden >= (LONGLONG)(FTYPE_INT_MAX))) {
            appro(trnum, rden);
            ASSERT0((trnum < (LONGLONG)(FTYPE_INT_MAX)) &&
                    (rden < (LONGLONG)(FTYPE_INT_MAX)));
        }
    }
    rat.m_num = (FType)(rnum < 0 ? -trnum : trnum);
    rat.m_den = (FType)rden;
    return rat;
}

//Subtraction operation
template <class FType>
TRational<FType> operator - (
    TRational<FType> const& a, TRational<FType> const& b)
{ return a + (-b); }

//Minus operation
template <class FType>
TRational<FType> operator - (TRational<FType> const& a)
{
    TRational<FType> b = a;
    b.m_num = -b.m_num;
    return b;
}

//Defined the default Rational type with INT Fraction type.
typedef TRational<DefRationalFType> Rational;

} //namespace xcom
#endif
