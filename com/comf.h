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
#ifndef _COMF_H_
#define _COMF_H_

namespace xcom {

//This macro declare copy constructor for class.
#define COPY_CONSTRUCTOR(class_name)   \
    class_name(class_name const&);     \
    class_name const& operator = (class_name const&)

//Used to avoid warning: unreferenced variable if set
//-Werror=unused-variable.
//#define UNUSED(v) (v)
template <typename T> int dummy_use(T const&) { return 0; }
#define UNUSED(v) xcom::dummy_use(v)

#ifdef _DEBUG_
#define CK_USE(a)    ASSERT0(a)
#define ASSERT_UNUSED(a, b)  \
    ((a) ? UNUSED(0) : (m022138(__FILE__, __LINE__), m518087 b))
#define ASSERT0_UNUSED(a)  \
    ((a) ? UNUSED(0) : (m022138(__FILE__, __LINE__), m518087 ("")))
#else
#define CK_USE(a)    UNUSED(a)
#define ASSERT_UNUSED(a, b)
#define ASSERT0_UNUSED(a)
#endif


template <class T, UINT GrowSize> class Vector;

//Arrangement
//P(n,m)=n*(n-1)*...*(n-m+1)=n!/(n-m)!
UINT arra(UINT n, UINT m); //Arrangement

//Convert floating point string into binary words.
void af2i(IN CHAR * f, OUT BYTE * buf, INT buflen, bool is_double);

//Return the position in array if find v.
//array: sorted in incremental order.
//n: elements size of array.
//v: search v in array.
bool binsearch(INT array[], UINT n, INT v, IN OUT UINT * ppos);

//Combination
//C(n,m)=(n*(n-1)*...*(n-m+1))/m! = n!/m!(n-m)!
//Simplify:C(n,m)=(n*(n-1)*(m+1))/(n-m)!
UINT combin(UINT n, UINT m); //Combination

//Ceil rounding alignment.
//e.g  v=17 , align=4 , the result is 20.
LONGLONG ceil_align(LONGLONG v, LONGLONG align);

//Caculate the number of bits which longer enough to represent given constant.
UINT computeConstBitLen(ULONGLONG v);

//Dumpf() for Vector<TY>.
void dumpf_svec(void * vec, UINT ty, CHAR const* name, bool is_del);

//Dumps() for Vector<TY>.
void dumps_svec(void * vec, UINT ty);

//Extended Euclid Method.
//    ax + by = ay' + b(x' -floor(a/b)*y') = gcd(a,b) = gcd(b, a%b)
INT exgcd(INT a, INT b, OUT INT & x, OUT INT & y);

//Factorial of n, namely, requiring n!.
UINT fact(UINT n);

//Searchs for sub-string.
INT findstr(CHAR * src, CHAR * s);

//Extract the right most sub-string which separated by 'separator' from string.
//e.g: Given string is a\b\c, separator is '\', return c;
CHAR const* extractRightMostSubString(CHAR const* string, CHAR separator);

//Extract the left most sub-string which separated by 'separator' from string.
//e.g: Given string is a\b\c, separator is '\', return a;
void extractLeftMostSubString(CHAR * tgt, CHAR const* string, CHAR separator);
//Great common divisor for number of values.
INT gcdm(UINT num, ...);

//Great common divisor for values stored in vector.
INT gcdm(UINT num, Vector<INT, 8> const& a);

//Compute the nearest power of 2 that not less than v.
inline UINT getNearestPowerOf2(UINT v)
{
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v++;
    return v;
}

//Compute the nearest power of 2 that not less than v.
inline ULONGLONG getNearestPowerOf2(ULONGLONG v)
{
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v |= v >> 32;
    v++;
    return v;
}

//Compute the number of 1.
UINT getLookupPopCount(ULONGLONG v);

//Compute the number of 1.
UINT getSparsePopCount(ULONGLONG v);

//Compute the power of 2, return the result.
//Note v must be power of 2.
UINT getPowerOf2(ULONGLONG v);

//Extract file suffix.
//e.g: Given a.foo, return foo.
CHAR * getfilesuffix(CHAR const* n, OUT CHAR * buf, UINT bufl);

//Extract file path.
//e.g: Given /xx/yy/zz.file, return /xx/yy
CHAR * getfilepath(CHAR const* n, OUT CHAR * buf, UINT bufl);

//Extract file name.
//e.g: Given /xx/yy/zz.foo, return zz.
CHAR * getfilename(CHAR const* n, OUT CHAR * buf, UINT bufl);

//Get current micro-second.
ULONGLONG getusec();
LONG getclockstart();
float getclockend(LONG start);

//Get the index of the first '1' start at most right side.
//e.g: given m=0x8, the first '1' index is 3.
INT getFirstOneAtRightSide(INT m);

inline UINT hash32bit(UINT n)
{
    n = (n+0x7ed55d16) + (n<<12);
    n = (n^0xc761c23c) ^ (n>>19);
    n = (n+0x165667b1) + (n<<5);
    n = (n+0xd3a2646c) ^ (n<<9);
    n = (n+0xfd7046c5) + (n<<3);
    n = (n^0xb55a4f09) ^ (n>>16);
    return n;
}

//Judge if 'f' is integer conform to IEEE754 spec.
bool is_integer(float f);

//Judge if 'd' is integer conform to IEEE754 spec.
bool is_integerd(double d);

//inline is necessary to avoid multiple define.
inline bool isPowerOf2(ULONGLONG x) { return (x != 0 && (x & (x-1)) == 0); }
bool isPowerOf5(double f);

//Prime Factorization.
//e.g: 435234 = 251 * 17 * 17 * 3 * 2.
void prim(INT m, OUT INT * buf);

//Reverse a DWORD by lexicalgraph.
//e.g:if 'd' is 0x12345678, return 0x78563412.
LONG revlong(LONG d);

//Reverse the string.
UCHAR * reverseString(UCHAR * v);
CHAR * upper(CHAR * n);
CHAR * lower(CHAR * n);
INT sgcd(INT x, INT y);
INT slcm(INT x, INT y);

//Shift a string to right side or left side.
//ofst: great than zero means shifting string to right side,
//   and the displacement is abs(ofst); negative
//   means shifting string to left.
void strshift(CHAR * src, INT ofst);

CHAR * xstrcat(CHAR * buf, size_t bufl, CHAR const* info, ...);
UINT xstrlen(CHAR const* p);

//Compare the first 'n' char of two string.
//Return true if equal.
bool xstrcmp(CHAR const* p1, CHAR const* p2, INT n);

//Format string and record in buf.
//'buf': output buffer record string.
//'stack_start': point to the first args.
CHAR * xsprintf(IN OUT CHAR * buf, UINT buflen, CHAR const* format, ...);

//Convert a string into long integer.
//e.g: cl = '1','2','3','4','5'
//return 12345.
//'is_oct': if true, nptr is octal digits.
LONGLONG xatoll(CHAR const* nptr, bool is_oct);

//Convert char value into binary.
//e.g: char p = ' '; p is blank.
INT xctoi(CHAR const* cl);

//Convert long to string.
UCHAR * xltoa(LONG v, OUT UCHAR * buf);
INT xceiling(INT a, INT b);
INT xfloor(INT a, INT b);
LONG xstrstr(CHAR const* src, CHAR const* par, INT i);

//Split string by given separetor, and return the number of substring.
//str: input string.
//ret: record each substring which separated by sep.
//sep: separator.
//Note caller is responsible for the free of each string memory in ret.
UINT xsplit(CHAR const* str, OUT Vector<CHAR*, 8> & ret, CHAR const* sep);
void xstrcpy(CHAR * tgt, CHAR const* src, size_t size);
inline bool xisspace(CHAR c) { return c == ' ' || c == '\t'; }
inline bool xisdigit(CHAR c) { return c >= '0' && c <= '9'; }
inline bool xisdigithex(CHAR d)
{
    if (xisdigit(d)) return true;
    else if ((d >= 'a' && d <= 'f') || (d >= 'A' && d <= 'F')) return true;
    return false;
}
inline bool xisalpha(CHAR c)
{ return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }
LONGLONG xabs(LONGLONG a);


//Exported Data Structures
class ASCII {
public:
    UCHAR val;
    CHAR ch;
};
extern ASCII g_asc1[];

} //namespace xcom
#endif
