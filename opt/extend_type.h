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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _EXTEND_TYPE_H_
#define _EXTEND_TYPE_H_

namespace xoc {

class UINT128;

class INT128 {
    friend class UINT128;
    //This class premits copy-constructing.
protected:
    INT64 m_low;
    INT64 m_high;
public:
    INT128() { m_low = 0; m_high = 0; }
    INT128(UINT64 v) { m_low = v; m_high = 0; }
    INT128(UINT128 const& v);

    CHAR const* dump(OUT xcom::DefFixedStrBuf & buf) const;
};

class UINT128 {
    friend class INT128;
    //This class premits copy-constructing.
protected:
    UINT64 m_low;
    UINT64 m_high;
public:
    UINT128() { m_low = 0; m_high = 0; }
    UINT128(UINT64 v) { m_low = v; m_high = 0; }
    UINT128(INT128 const& v) { m_low = v.m_low; m_high = v.m_high; }

    CHAR const* dump(OUT xcom::DefFixedStrBuf & buf) const;

    friend bool operator == (UINT128 const& a, UINT128 const& b);
    friend bool operator != (UINT128 const& a, UINT128 const& b);
    friend bool operator < (UINT128 const& a, UINT128 const& b);
    friend bool operator <= (UINT128 const& a, UINT128 const& b);
    friend bool operator > (UINT128 const& a, UINT128 const& b);
    friend bool operator >= (UINT128 const& a, UINT128 const& b);
    friend UINT128 operator * (UINT128 const& a, UINT128 const& b);
    friend UINT128 operator / (UINT128 const& a, UINT128 const& b);
    friend UINT128 operator % (UINT128 const& a, UINT128 const& b);
    friend UINT128 operator + (UINT128 const& a, UINT128 const& b);
    friend UINT128 operator - (UINT128 const& a, UINT128 const& b);
    friend UINT128 operator - (UINT128 a);
};

//Exported Functions
inline bool operator == (UINT128 const& a, UINT128 const& b)
{ return (a.m_low == b.m_low) & (a.m_high == b.m_high); }
inline bool operator != (UINT128 const& a, UINT128 const& b)
{ return !(a == b); }
inline bool operator < (UINT128 const& a, UINT128 const& b)
{ return (a.m_high < b.m_high) | (a.m_low < b.m_low); }
inline bool operator <= (UINT128 const& a, UINT128 const& b)
{ return (a < b) | (a == b); }
inline bool operator > (UINT128 const& a, UINT128 const& b)
{ return (a.m_high > b.m_high) | (a.m_low > b.m_low); }
inline bool operator >= (UINT128 const& a, UINT128 const& b)
{ return (a > b) | (a == b); }
inline UINT128 operator * (UINT128 const& a, UINT128 const& b)
{
    UINT128 c;
    ASSERT0(0);
    return c;
}
inline UINT128 operator / (UINT128 const& a, UINT128 const& b)
{
    UINT128 c;
    ASSERT0(0);
    return c;
}
inline UINT128 operator % (UINT128 const& a, UINT128 const& b)
{
    UINT128 c;
    ASSERT0(0);
    return c;
}
inline UINT128 operator + (UINT128 const& a, UINT128 const& b)
{
    UINT128 c;
    c.m_low = a.m_low + b.m_low;
    UINT64 carray = c.m_low < a.m_low ? 1 : 0;
    c.m_high = carray + a.m_high + b.m_high;
    return c;
}
inline UINT128 operator - (UINT128 const& a, UINT128 const& b)
{
    UINT128 c;
    UINT64 borrow;
    if (a.m_low < b.m_low) {
        borrow = 1;
        UINT64 max = xcom::computeUnsignedMaxValue<UINT64>();
        c.m_low = a.m_low + max - b.m_low + 1;
    } else {
        borrow = 0;
        c.m_low = a.m_low - b.m_low;
    }
    if (a.m_high < b.m_high + borrow) {
        UINT64 max = xcom::computeUnsignedMaxValue<UINT64>();
        c.m_high = a.m_high + max - b.m_high - borrow + 1;
    } else {
        c.m_high = a.m_high - b.m_high - borrow;
    }
    return c;
}

//Minus operation
inline UINT128 operator - (UINT128 a)
{
    UINT128 c;
    c = ((UINT128)0) - a;
    return c;
}

} //namespace xoc

#endif
