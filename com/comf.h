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

class StrBuf;
class StrBufVec;

//Forward Declaration
template <class T> class Vector;

//Compute the maximum unsigned integer value that type is ValueType.
//e.g: given bitwidth is 3, return 7 as result.
//Note ValueType should be unsigned integer type.
template <class ValueType>
inline ValueType computeUnsignedMaxValue(UINT bitwidth)
{ return ((((ValueType)1) << bitwidth) - 1); }

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

//The function converts string content into set of hex and store in the 'buf'.
//Note the content in given buf must be string format of hex, that is the
//string can only contain "abcdefABCDEF0123456789".
//string: a string that represents bytes, where a byte consists of two hex.
//        e.g:ADBC5E demostrates three bytes, 0xAD, 0xBC, and 0x5E.
void charToByteHex(CHAR const* string, OUT BYTE * buf, UINT buflen);

//The function converts a character into hex.
//Note the content in given buf must be string format of hex, that is 'c'
//can only be "abcdefABCDEF0123456789".
BYTE charToHex(CHAR c);

//Return the number of leading zero.
//pos: record the bit position of the leading zero.
//e.g:given a is 0b00110, where the left size is the highest bit.
//the function return 2, indicates there are two leading zeros counting from
//high to low side about given number.
UINT countLeadingZero(UINT64 a);
UINT countLeadingZero(UINT32 a);

//Return true if found leading one.
//pos: record the bit position of the leading one.
//e.g:given a is 0b11110, where the left size is the highest bit.
//the function return 4, indicates there are four leading one counting from
//high to low side about given number.
UINT countLeadingOne(UINT64 a);
UINT countLeadingOne(UINT32 a);

//Count the trailing zeros of val.
//e.g: given 0b1011000, the number of trailing zeros is 3.
UINT countTrailingZero(UINT64 val);
UINT countTrailingZero(UINT32 val);

//Extended Euclid Method.
//    ax + by = ay' + b(x' -floor(a/b)*y') = gcd(a,b) = gcd(b, a%b)
INT exgcd(INT a, INT b, OUT INT & x, OUT INT & y);

//Extract the right most sub-string which separated by 'separator' from string.
//e.g: Given string is a\b\c, separator is '\', return c;
CHAR const* extractRightMostSubString(CHAR const* string, CHAR separator);

//Extract the left most sub-string which separated by 'separator' from string.
//e.g: Given string is a\b\c, separator is '\', return a;
void extractLeftMostSubString(CHAR * tgt, CHAR const* string, CHAR separator);

//Extract the bit value from 'val' by given range that between 'start' bit
//position and 'end' bit position.
//The bit position start at 0.
//e.g: Given start is 2, end is 4, val is 0b110101111, return 0b010.
ULONGLONG extractBitRangeValue(ULONGLONG val, UINT start, UINT end);

//Extract the valid value according to the bitwidth.
//e.g: given value is 7 and bitwidth is 2, return 3 as result.
//Note ValueType should be unsigned integer type.
template <class ValueType>
inline ValueType extractValidValueViaBitWidth(
    ValueType value, UINT bitwidth)
{
    return (value & computeUnsignedMaxValue<ValueType>(bitwidth));
}

//Factorial of n, namely, requiring n!.
UINT fact(UINT n);

//convert float to ESP64(64-bit extended single precision) format.
UINT64 float2ESP64(UINT64 val);

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
CHAR * getFileSuffix(CHAR const* n, OUT CHAR * buf, UINT bufl);

//Extract file path.
//e.g: Given /xx/yy/zz.file, return /xx/yy
CHAR * getFilePath(CHAR const* n, OUT CHAR * buf, UINT bufl);

//Extract file name.
//e.g: Given /xx/yy/zz.foo, return zz.
CHAR * getFileName(CHAR const* n, OUT CHAR * buf, UINT bufl);

//Get current micro-second.
ULONGLONG getusec();
LONG getclockstart();
float getclockend(LONG start);

//Get the index of the first '1' start at most right side.
//e.g: given m=0x8, the first '1' index is 3.
INT getFirstOneAtRightSide(INT m);

//Get the sign bit of given bitsize.
//                      |
//                      V
//8bit integer:         0 00000000
//                        |<-7 ->|
//16bit integer:        0 000000000000000
//                        |<-   15    ->|
//32bit integer:        0 0000000000000000000000000000000
//                        |<-            31           ->|
//64bit integer:        0 0000000000....00000000000000000
//                        |<-            63           ->|
//128bit integer:       0 0000000000....00000000000000000
//                        |<-            127          ->|
//16bit floating point: 0 00000 0000000000 *f16*
//                        | 5 | |<- 10 ->|
//                      0 00000000 0000000 *bf16*
//                        |<- 8->| |<-7->|
//32bit floating point: 0 00000000 00000000000000000000000
//                        |<- 8->| |<-       23        ->|
//64bit floating point: 0 00000000000 0000000...0000000000
//                        |<-  11 ->| |<-     52       ->|
//128bit floating point:0 0000...0000 0000000...0000000000
//                        |<-  15 ->| |<-     112      ->|
UINT getSignBit(UINT bitsize);

//Get low 16-bit value of signal 32-bit value.
//For example:
//val:     0b 0100010......1000 0111 0101 1011
//                         &
//0xffff:  0b 0000000......1111 1111 1111 1111
//                         =
//         0b 0000000......1000 0111 0101 1011
//                         ^
//0x8000:  0b 0000000......1000 0000 0000 0000
//                         =
//         0b 0000000......0000 0111 0101 1011
//                         -
//0x8000:  0b 0000000......1000 0000 0000 0000
//                         =
//         0b 1111111......1000 0111 0101 1011
INT getSign32BitLow16BitVal(INT32 val);

//Get low 16-bit value of signal 64-bit value.
//The principle is similar to getSign32BitLow16BitVal().
INT getSign64BitLow16BitVal(INT64 val);

//Get low 32-bit value of signal 64-bit value.
//The principle is similar to getSign32BitLow16BitVal().
INT getSign64BitLow32BitVal(INT64 val);

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

//convert half to EHP64(64-bit extended half-precision) format.
UINT64 half2EHP64(UINT64 val);

//Return true if val exceed the range that can be described with 'bitsize'.
bool isExceedBitWidth(ULONGLONG val, UINT bitwidth);

//Return true if 'c' is an extended character.
//
//For example:
//  The special character "À" is expressed in unicode form as: \u00C0.
//  According to the following rules:
//
//  ---------------------------------------------------------------
//  |  Unicode hexadecimal  |            UTF-8 binary             |
//  |-------------------------------------------------------------|
//  | 0000 0000 ~ 0000 007F | 0xxxxxxx                            |
//  | 0000 0080 ~ 0000 07FF | 110xxxxx 10xxxxxx                   |
//  | 0000 0800 ~ 0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx          |
//  | 0001 0000 ~ 0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx |
//  ---------------------------------------------------------------
//
//  The special character "À" is expressed in UTF-8 form as: "0xC3 0x80".
inline bool xisextchar(CHAR c) { return (c & 0x80) != 0; }

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
inline bool isPowerOf2(ULONGLONG x) { return (x != 0) & ((x & (x-1)) == 0); }
bool isPowerOf5(double f);

//Prime Factorization.
//e.g: 435234 = 251 * 17 * 17 * 3 * 2.
void prim(INT m, OUT INT * buf);

//Reverse a LONG type integer by lexicalgraph.
//e.g: if 'd' is 0x12345678, return 0x78563412.
LONG revlong(LONG d);

//Reverse the string.
CHAR * reverseString(CHAR * v);

//The function rotates string in buf.
//e.g: given string "xyzmn", n is 2, after rotation,
//the buf will be "zmnxy".
//buf: record the string.
//buflen: byte length of buf.
//n: rotate times.
CHAR * rotateString(MOD CHAR * str, UINT n);

//Replace letters in 'n' to capital letter.
CHAR * upper(CHAR * n);
inline CHAR upper(CHAR n)
{ return ((n >= 'a') & (n <= 'z')) ? (CHAR)(n - 32) : n; }

//Replace letters in 'n' to lowercase letter.
CHAR * lower(CHAR * n);
inline CHAR lower(CHAR n)
{ return ((n >= 'A') & (n <= 'Z')) ? (CHAR)(n + 32) : n; }

//Replace the suffix of the file name(or path name) using given suffix name.
//For example:
//org_file_name: "xxx.c" or "/tmp/xxx.cpp"
//new_suffix: ".o"
//new_file_name: "xxx.o"
void replaceFileNameSuffix(CHAR const* org_file_name, CHAR const* new_suffix,
                           OUT StrBuf & new_file_name);

//Calculate the Great Common Divisor of x and y.
INT sgcd(INT x, INT y);

//Calculate the Least Common Multiple of x and y.
INT slcm(INT x, INT y);

//Get 0~15(part1), 16~31(part2), 32~47(part3), 48~63(part4) bits value of
//signal 64-bit value.
//This function uses getSign64BitLow16BitVal(), getSign64BitLow32BitVal(),
//and shift operation to iteratively fetch each value.
void splitSign64BitToFourParts(INT64 val, OUT INT & part1, OUT INT & part2,
                               OUT INT & part3, OUT INT & part4);

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
//e.g: return true if p1 is "aaa", p2 is "aaab".
bool xstrcmp(CHAR const*RESTRICT p1, CHAR const*RESTRICT p2, INT n);

//Format string and record in buf.
//'buf': output buffer record string.
//'stack_start': point to the first args.
CHAR * xsprintf(MOD CHAR * buf, UINT buflen, CHAR const* format, ...);

//Convert a string into long integer.
//e.g: cl = '1','2','3','4','5'
//return 12345.
//'is_oct': if true, nptr is octal digits.
LONGLONG xatoll(CHAR const* nptr, bool is_oct = false);

//Convert char value into binary.
//e.g: char p = ' '; p is blank.
INT xctoi(CHAR const* cl);

//Convert long to string.
CHAR * xltoa(LONG v, OUT CHAR * buf);

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

//Split string by given separator, and return the number of substring.
//str: input string.
//ret: record each substring which separated by sep.
//sep: separator.
//Note caller is responsible for the free of each string memory in ret.
//e.g:given str "a/b/c", sep is '/', ret will store three StrBuf, "a", "b"
//and "c".
UINT xsplit(CHAR const* str, CHAR const* sep, OUT StrBufVec & ret);

//The function copies 'bytesize' of the string pointed by 'src'
//to destination StrBuf 'tgt', including the '\0'.
//The function also return buffer pointer of 'tgt'.
CHAR const* xstrcpy(CHAR const* src, size_t bytesize, OUT StrBuf & tgt);

//Return true if 'c' is blank space or TAB character.
inline bool xisspace(CHAR c) { return (c == ' ') | (c == '\t'); }

//Return true if char 'c' is decimal.
inline bool xisdigit(CHAR c) { return (c >= '0') & (c <= '9'); }

//Return true if string 'str' is decimal.
inline bool xisdigit(CHAR const* str)
{
    for (; *str != 0; str++) {
        if (!xisdigit(*str)) {
            return false;
        }
    }
    return true;
}

//Return true if 'c' is hex decimal.
inline bool xisdigitbin(CHAR c) { return (c == '0') | (c == '1'); }

//Return true if 'c' is hex decimal.
inline bool xisdigithex(CHAR c)
{
    if (xisdigit(c)) { return true; }
    if ((upper(c) >= 'A') & (upper(c) <= 'F')) { return true; }
    return false;
}

//Return true if 'c' is letter.
inline bool xisalpha(CHAR c) { return (upper(c) >= 'A') & (upper(c) <= 'Z'); }

//Return abs value of 'a'.
inline LONGLONG xabs(LONGLONG a) { return a >= LONGLONG(0) ? a : -a; }
inline float xfabs(float a) { return (a < float(0.0)) ? -a : a; }
inline double xfabs(double a) { return (a < double(0.0)) ? -a : a; }

//Return sqrt value of 'num'.
float xsqrt(float num);
double xsqrt(double num);

//Return true if the imm is valid for the limited bitsize.
bool isValidImmForBitsize(UINT bitsize, UINT64 imm);

//Exported Data Structures
class ASCII {
public:
    UCHAR val;
    CHAR ch;
};
extern ASCII g_asc1[];

} //namespace xcom

#endif  //_COMMON_FUNCTION_H_
