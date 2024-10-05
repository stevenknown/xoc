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
#ifndef __FLTY_H__
#define __FLTY_H__

namespace xcom {

#define PRECISION_TYPE double
#define PRECISION_TYPE_E 2.718281828459045

class Float {
public:
    typedef PRECISION_TYPE PType;
public:
    friend Float zerolinz(Float const& a);
    friend bool operator == (Float const& a, Float const& b);
    friend bool operator == (Float const& a, Float::PType b);
    friend bool operator == (Float::PType a, Float const& b);
    friend bool operator != (Float const& a, Float const& b);
    friend bool operator < (Float const& a, Float const& b);
    friend bool operator <= (Float const& a, Float const& b);
    friend bool operator > (Float const& a, Float const& b);
    friend bool operator >= (Float const& a, Float const& b);
    friend Float operator * (Float const& a, Float const& b);
    friend Float operator / (Float const& a, Float const& b);
    friend Float operator + (Float const& a, Float const& b);
    friend Float operator - (Float const& a, Float const& b);
    friend Float operator - (Float a);
protected:
    PRECISION_TYPE m_f;
public:
    static PRECISION_TYPE g_e;
public:
    Float() { m_f = PRECISION_TYPE(0); }
    Float(PRECISION_TYPE f) { m_f = f; }

    CHAR const* dump(StrBuf & buf) const;
    void dump() const;

    //Return the base of natural logarithm: e.
    static inline PRECISION_TYPE getE() { return Float::g_e; }

    //The function return the unique address of 'e'.
    //Since the float-number is an approximate representation of real-number,
    //thus there is a margin of error. In order to avoid the error, user can
    //compare the unique address of static member 'e' instead of comparing
    //the appoximate value.
    static inline PRECISION_TYPE const* getEAddr() { return &g_e; }
    static inline PRECISION_TYPE getEpsilon()
    {
        //The precision of 'double' is too high to
        //some operation of those value that approaching infinitesimal.
        //e.g: when the value is 0.00000000000000066613381477509392,
        //it should approximately equal to 0 in actually.
        //#define INFINITESIMAL 0.00000000000000001
        #define INFINITESIMAL 0.000001
        return INFINITESIMAL;
    }

    //The function attempt to find the nearest integer of given float number.
    //e.g:given 1.000001, return 1.000000.
    //    given 1.999999, return 2.000000.
    static PRECISION_TYPE integralize(PRECISION_TYPE const& a);
    bool is_int() const;

    //Return true if given float-number 'a' is approximately equal to current
    //float-number.
    bool isApproEq(Float const& a) const
    { return Float::isApproEq(m_f, a.m_f); }

    //Return true if given float-number 'a' is approximately equal to 'b'.
    static bool isApproEq(PRECISION_TYPE a, PRECISION_TYPE b);

    Float & operator = (Float const& a) { m_f = a.m_f; return *this; }

    //Define the implict type converting operation.
    //e.g:convert Float to double.
    //    Float a(1.1); double b = (double)a;
    operator PRECISION_TYPE() const { return m_f; }

    void reduce() {}

    //Calculate the floor boundary.
    INT typecast2int() const { return (INT)m_f; }

    PRECISION_TYPE val() const { return m_f; }
    PRECISION_TYPE & val() { return m_f; }

    static Float zerolinz(Float const& a);
};

//Exported Functions
inline bool operator == (Float const& a, Float const& b)
{ return Float::isApproEq(a.m_f, b.m_f); }
inline bool operator == (Float const& a, Float::PType b)
{ return Float::isApproEq(a.m_f, b); }
inline bool operator == (Float::PType a, Float const& b)
{ return Float::isApproEq(b.m_f, a); }
bool operator != (Float const& a, Float const& b);
inline bool operator != (Float const& a, Float const& b) { return !(a == b); }
inline bool operator < (Float const& a, Float const& b)
{ return a.m_f < b.m_f; }
inline bool operator <= (Float const& a, Float const& b)
{ return (a.m_f < b.m_f) || (a == b); }
inline bool operator > (Float const& a, Float const& b)
{ return a.m_f > b.m_f; }
inline bool operator >= (Float const& a, Float const& b)
{ return (a.m_f > b.m_f) || (a == b); }
inline Float operator * (Float const& a, Float const& b)
{
    //return Float(integralize(integralize(a.m_f) * integralize(b.m_f)));
    return Float(a.m_f * b.m_f);
}
inline Float operator / (Float const& a, Float const& b)
{
    //return Float(integralize(integralize(a.m_f) / integralize(b.m_f)));
    return Float(a.m_f / b.m_f);
}
inline Float operator + (Float const& a, Float const& b)
{
    //return Float(integralize(integralize(a.m_f) + integralize(b.m_f)));
    return Float(a.m_f + b.m_f);
}

//Subtration operation
inline Float operator - (Float const& a, Float const& b)
{
    //return Float(integralize(integralize(a.m_f) - integralize(b.m_f)));
    return Float(a.m_f - b.m_f);
}

//Minus operation
inline Float operator - (Float a)
{
    a.m_f = -(a.m_f);
    return a;
}

} //namespace xcom
#endif
