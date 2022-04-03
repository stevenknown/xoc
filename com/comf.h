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
#ifndef _COMMON_FUNCTION_H_
#define _COMMON_FUNCTION_H_

namespace xcom {

//Forward Declaration
template <class T> class Vector;

//Compute the maximum unsigned integer value that type is ValueType.
//e.g: given bitwidth is 3, return 7 as result.
//Note ValueType should be unsigned integer type.
template <class ValueType>
inline ValueType computeUnsignedMaxValue(UINT bitwidth)
{
    return ((((ValueType)1) << bitwidth) - 1);
}

//Arrangement
//P(n,m)=n*(n-1)*...*(n-m+1)=n!/(n-m)!
UINT arra(UINT n, UINT m); //Arrangement

//Convert floating point string into binary words.
void af2i(IN CHAR * f, OUT BYTE * buf, INT buflen, bool is_double);

//Return the position in array if find v.
//array: sorted in incremental order.
//n: elements size of array.
//v: search v in array.
bool binsearch(INT array[], UINT n, INT v, MOD UINT * ppos);

//Combination
//C(n,m)=(n*(n-1)*...*(n-m+1))/m! = n!/m!(n-m)!
//Simplify:C(n,m)=(n*(n-1)*(m+1))/(n-m)!
UINT combin(UINT n, UINT m); //Combination

//Ceil rounding alignment.
//e.g  v=17 , align=4 , the result is 20.
LONGLONG ceil_align(LONGLONG v, LONGLONG align);

//Compute the number of bits which biger enough to represent given value.
//value: the input value that to be check.
//e.g: given 00101, it needs at least 3 bits to hold the binary value 101.
//     and function return 3.
UINT computeMaxBitSizeForValue(ULONGLONG v);

//Extended Euclid Method.
//    ax + by = ay' + b(x' -floor(a/b)*y') = gcd(a,b) = gcd(b, a%b)
INT exgcd(INT a, INT b, OUT INT & x, OUT INT & y);

//Extract the right most sub-string which separated by 'separator' from string.
//e.g: Given string is a\b\c, separator is '\', return c;
CHAR const* extractRightMostSubString(CHAR const* string, CHAR separator);

//Extract the left most sub-string which separated by 'separator' from string.
//e.g: Given string is a\b\c, separator is '\', return a;
void extractLeftMostSubString(CHAR * tgt, CHAR const* string, CHAR separator);

//Factorial of n, namely, requiring n!.
UINT fact(UINT n);

//Great common divisor for number of values.
INT gcdm(UINT num, ...);

//Great common divisor for values stored in vector.
INT gcdm(UINT num, Vector<INT> const& a);

//Compute the nearest power of 2 that not less than v.
//e.g: given v is 60, return 64.
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
//e.g: given v is 60, return 64.
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
//e.g: given 0xa, return 2.
UINT getLookupPopCount(ULONGLONG v);

//Compute the number of 1.
//e.g: given 0xa, return 2.
UINT getSparsePopCount(ULONGLONG v);

//Compute the power of 2, return the result.
//Note v must be power of 2.
//e.g: given v is 64, return 16.
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

//Calculate an integer hash value according to 'n'.
inline UINT32 hash32bit(UINT32 n)
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
bool isIntegerF(float f);

//Judge if 'd' is integer conform to IEEE754 spec.
bool isIntegerD(double d);

//Return true if 'f' represents a finite floating-point value.
//f: is conform to IEEE754 spec.
bool isFiniteD(double f);

//Return true if 'f' represents a finite floating-point value.
//f: is conform to IEEE754 spec.
bool isFiniteF(float f);

//inline is necessary to avoid multiple define.
inline bool isPowerOf2(ULONGLONG x) { return (x != 0 && (x & (x-1)) == 0); }
bool isPowerOf5(double f);

//Prime Factorization.
//e.g: 435234 = 251 * 17 * 17 * 3 * 2.
void prim(INT m, OUT INT * buf);

//Reverse a LONG type integer by lexicalgraph.
//e.g: if 'd' is 0x12345678, return 0x78563412.
LONG revlong(LONG d);

//Reverse the string.
UCHAR * reverseString(UCHAR * v);

//Replace letters in 'n' to capital letter.
CHAR * upper(CHAR * n);

//Replace letters in 'n' to lowercase letter.
CHAR * lower(CHAR * n);

//Calculate the Great Common Divisor of x and y.
INT sgcd(INT x, INT y);

//Calculate the Least Common Multiple of x and y.
INT slcm(INT x, INT y);

//Shift a string to right side or left side.
//ofst: great than zero means shifting string to right side,
//   and the displacement is abs(ofst); negative
//   means shifting string to left.
void strshift(CHAR * src, INT ofst);

//The function produce output string according to 'info' as described below,
//and appends the output the 'buf'.
//Return a pointer to the resulting string 'buf'.
//buf: output string buffer.
//bufl: byte size of 'buf'.
//info: formatting string.
CHAR * xstrcat(CHAR * buf, size_t bufl, CHAR const* info, ...);

//Calculates the byte size of string 'p', excluding the terminating byte '\0'.
UINT xstrlen(CHAR const* p);

//Compare the first 'n' characters of two string.
//Return true if equal.
bool xstrcmp(CHAR const* p1, CHAR const* p2, INT n);

//Format string and record in buf.
//'buf': output buffer record string.
//'stack_start': point to the first args.
CHAR * xsprintf(MOD CHAR * buf, UINT buflen, CHAR const* format, ...);

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

//Cacluates the smallest integral value that is not less than
//the division of a, b.
//e.g: xceiling(3,2) is 2.
INT xceiling(INT a, INT b);

//Cacluates the largest integral value that is not greater than
//the division of a, b.
//e.g: xceiling(3,2) is 1.
INT xfloor(INT a, INT b);

//Find partial string, return the subscript-index if substring found,
//otherwise return -1.
//src: input string.
//par: partial string.
LONG xstrstr(CHAR const* src, CHAR const* par);

//Split string by given separetor, and return the number of substring.
//str: input string.
//ret: record each substring which separated by sep.
//sep: separator.
//Note caller is responsible for the free of each string memory in ret.
UINT xsplit(CHAR const* str, OUT Vector<CHAR*> & ret, CHAR const* sep);

//The function copies byte size of 'size' of the string pointed to by 'src'
//to destination 'tgt', including the '\0'.
void xstrcpy(CHAR * tgt, CHAR const* src, size_t size);

//Return true if 'c' is blank space or TAB character.
inline bool xisspace(CHAR c) { return c == ' ' || c == '\t'; }

//Return true if 'c' is decimal.
inline bool xisdigit(CHAR c) { return c >= '0' && c <= '9'; }

//Return true if 'c' is hex decimal.
inline bool xisdigithex(CHAR d)
{
    if (xisdigit(d)) { return true; }
    else if ((d >= 'a' && d <= 'f') || (d >= 'A' && d <= 'F')) { return true; }
    return false;
}

//Return true if 'c' is letter.
inline bool xisalpha(CHAR c)
{ return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }

//Return abs value of 'a'.
LONGLONG xabs(LONGLONG a);

//Exported Data Structures
class ASCII {
public:
    UCHAR val;
    CHAR ch;
};
extern ASCII g_asc1[];

} //namespace xcom

#endif  //_COMMON_FUNCTION_H_
