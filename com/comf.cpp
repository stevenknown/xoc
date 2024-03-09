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
#ifdef _ON_WINDOWS_
#include <time.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

#include "math.h"
#include "xcominc.h"

#ifndef BITS_PER_BYTE
#define BITS_PER_BYTE 8
#endif

namespace xcom {

static CHAR g_hexdigits[] = "0123456789ABCDEF";

ASCII g_asc1[] = {
    {27, '.'},
    {32, ' '},
    {33, '!'},
    {34, '"'},
    {35, '#'},
    {36, '$'},
    {37, '%'},
    {38, '&'},
    {39, '\''},
    {40, '('},
    {41, ')'},
    {42, '*'},
    {43, '+'},
    {44, ','},
    {45, '-'},
    {46, '.'},
    {47, '/'},
    {48, '0'},
    {49, '1'},
    {50, '2'},
    {51, '3'},
    {52, '4'},
    {53, '5'},
    {54, '6'},
    {55, '7'},
    {56, '8'},
    {57, '9'},
    {58, ':'},
    {59, ';'},
    {60, '<'},
    {61, '='},
    {62, '>'},
    {63, '?'},
    {64, '@'},
    {65, 'A'},
    {66, 'B'},
    {67, 'C'},
    {68, 'D'},
    {69, 'E'},
    {70, 'F'},
    {71, 'G'},
    {72, 'H'},
    {73, 'I'},
    {74, 'J'},
    {75, 'K'},
    {76, 'L'},
    {77, 'M'},
    {78, 'N'},
    {79, 'O'},
    {80, 'P'},
    {81, 'Q'},
    {82, 'R'},
    {83, 'S'},
    {84, 'T'},
    {85, 'U'},
    {86, 'V'},
    {87, 'W'},
    {88, 'X'},
    {89, 'Y'},
    {90, 'Z'},
    {91, '['},
    {92, '\\'},
    {93, ']'},
    {94, '^'},
    {95, '_'},
    {96, '`'},
    {97, 'a'},
    {98, 'b'},
    {99, 'c'},
    {100, 'd'},
    {101, 'e'},
    {102, 'f'},
    {103, 'g'},
    {104, 'h'},
    {105, 'i'},
    {106, 'j'},
    {107, 'k'},
    {108, 'l'},
    {109, 'm'},
    {110, 'n'},
    {111, 'o'},
    {112, 'p'},
    {113, 'q'},
    {114, 'r'},
    {115, 's'},
    {116, 't'},
    {117, 'u'},
    {118, 'v'},
    {119, 'w'},
    {120, 'x'},
    {121, 'y'},
    {122, 'z'},
    {123, '{'},
    {124, '|'},
    {125, '}'},
    {126, '~'},
};


//Compute the number of bits which biger enough to represent given value.
//value: the input value that to be check.
//e.g: given 00101, it needs at least 3 bits to hold the binary value 101.
//     and function return 3.
UINT computeMaxBitSizeForValue(ULONGLONG value)
{
    if (value == 0) { return 1; }
    UINT bitwidth = sizeof(value) * BITS_PER_BYTE;
    UINT half_bitwidth = bitwidth / 2;
    for (; value < computeUnsignedMaxValue<ULONGLONG>(half_bitwidth); ) {
        bitwidth /= 2;
        half_bitwidth = bitwidth / 2;
    }
    for (INT i = bitwidth - 1; i >= 0; i--) {
        if (HAVE_FLAG(value, (((ULONGLONG)1) << i))) {
            return i + 1;
        }
    }
    UNREACHABLE();
    return 0;
}


UINT countLeadingOne(UINT64 a)
{
    UINT32 hi = a >> (sizeof(UINT32) * BITS_PER_BYTE);
    if (hi != (UINT32)-1) { return countLeadingOne(hi); }
    return (sizeof(UINT32) * BITS_PER_BYTE) +
           countLeadingOne((UINT32)a);
}


UINT countLeadingOne(UINT32 a)
{
    UINT32 marker = (UINT32)(1 << (sizeof(UINT32) * BITS_PER_BYTE - 1));
    UINT i = 0;
    for (; i < sizeof(UINT32) * BITS_PER_BYTE; i++) {
        if ((marker & a) == 0) { break; }
        marker = marker >> 1;
    }
    return i;
}


UINT countLeadingZero(UINT64 a)
{
    UINT32 hi = a >> (sizeof(UINT32) * BITS_PER_BYTE);
    if (hi != 0) { return countLeadingZero(hi); }
    return (sizeof(UINT32) * BITS_PER_BYTE) +
           countLeadingZero((UINT32)a);
}


UINT countLeadingZero(UINT32 a)
{
    UINT32 marker = (UINT32)(1 << (sizeof(UINT32) * BITS_PER_BYTE - 1));
    UINT i = 0;
    for (; i < sizeof(UINT32) * BITS_PER_BYTE; i++) {
        if ((marker & a) == marker) { break; }
        marker = marker >> 1;
    }
    return i;
}


UINT countTrailingZero(UINT64 val)
{
    if (val & 0x1) { return 0; }
    UINT64 trail = 0;
    UINT64 size = sizeof(UINT64) * BITS_PER_BYTE;
    for (UINT64 i = 0; i < size; i++) {
        if ((val >> i) & 1) {
            break;
        }
        trail++;
    }
    return (UINT)trail;
}


UINT countTrailingZero(UINT32 val)
{
    if (val & 0x1) { return 0; }
    UINT32 trail = 0;
    UINT32 size = sizeof(UINT32) * BITS_PER_BYTE;
    for (UINT32 i = 0; i < size; i++) {
        if ((val >> i) & 1) {
            break;
        }
        trail++;
    }
    return (UINT)trail;
}


//Append string to buf.
//This function will guarantee that the length of string does
//not exceed bufl.
CHAR * xstrcat(CHAR * buf, size_t bufl, CHAR const* info, ...)
{
    size_t l = ::strlen(buf);
    if (l >= bufl) { return buf; }

    size_t x = bufl - l;

    va_list ptr;
    va_start(ptr, info);

    CHAR * lbuf = buf + l;
    vsnprintf(lbuf, x, info, ptr);
    buf[bufl - 1] = 0;
    va_end(ptr);
    return buf;
}


//Round off to minus infinity
INT xfloor(INT a, INT b)
{
    ASSERTN(b != 0, ("div zero"));
    if (a % b == 0) {
        //This part could not be combined with third one.
        return (a / b);
    } else if ((a < 0 && b < 0) || (a > 0 && b > 0)) {
        return (a / b);
    } else {
        return ((a - b) / b);
    }
}


//Round up to plus infinity.
INT xceiling(INT a, INT b)
{
    ASSERTN(b != 0, ("div zero"));
    if (a % b == 0) {
        //(a+b-1)/b will be errorneous
        //CASE:ceiling(-4, 2)
        return a / b;
    } else if ((a < 0 && b < 0) || (a > 0 && b > 0)) {
        return (a + b) / b;
    } else {
        //(a+b-1)/b will be errorneous
        //CASE:ceiling(5,-2)
        return a / b;
    }
}


//Calculate the Great Common Divisor of x and y.
INT sgcd(INT x, INT y)
{
    INT t;
    if (x < 0) {
        x = -x;
    }
    if (y < 0) {
        y = -y;
    }
    if ( x > y ){
        t = x, x = y, y = t;
    }
    while (x) {
        t = x;
        x = y % x;
        y = t;
    }
    return y;
}


//Calculate the Least Common Multiple of x and y.
INT slcm(INT x, INT y)
{
    return x * y / sgcd(x,y);
}


//Great common divisor for values stored in vector.
INT gcdm(UINT num, Vector<INT> const& values)
{
    if (num == 0) { return 0; }
    INT n1 = values[0];
    UINT i = 1;
    while (i < num) {
        INT n2 = values[i];
        n1 = sgcd(n1, n2);
        if (n1 == 1) {
            return 1;
        }
        i++;
    }
    return n1;
}


//Great common divisor for number of values.
INT gcdm(UINT num, ...)
{
    if (num == 0) {
        return 0;
    }
    INT *ptr = (INT*) (((BYTE*)(&num)) + sizeof(UINT));
    INT n1 = *ptr++;

    UINT i = 0;
    while (i < num) {
        INT n2 = *ptr;
        n1 = sgcd(n1, n2);
        if (n1 == 1) {
            return 1;
        }
        i++;
        ptr++; //stack growing down
    }
    return n1;
}


static INT _exgcd(INT a, INT b, OUT INT & x, OUT INT & y)
{
    if (b == 0) {
        x = 1;
        y = 0;
        return a;
    }
    INT x1, y1;
    INT gcd = _exgcd(b, a%b, x1, y1);
    x = y1;
    y = x1 - a/b*y1;
    return gcd;
}


//Extended Euclid Method.
//    ax + by = ay' + b(x' -floor(a/b)*y') = gcd(a,b) = gcd(b, a%b)
INT exgcd(INT a, INT b, OUT INT & x, OUT INT & y)
{
    INT gcd = _exgcd(a, b, x, y);
    if (gcd < 0) {
        gcd = -gcd;
        x = -x;
        y = -y;
    }
    return gcd;
}


//Factorial of n, namely, requiring n!.
UINT fact(UINT n)
{
    if (n == 0) { return 1; }
    UINT res = n;
    while (n >= 3) {
        n--;
        res = res * n;
    }
    return res;
}



//float:
//x    xxxxxxxx xxxxxxxxxxxxxxxxxxx
//sign exponent    mantissa
//31   30 ~ 23     22 ~ 0
//ESP64:
//x    1000xxxxxxx xxxxxxxxxxxxxxxxxxxxxxx00000000000000000000000000000
//x    0111xxxxxxx xxxxxxxxxxxxxxxxxxxxxxx00000000000000000000000000000
UINT64 float2ESP64(UINT64 val)
{
    if (val == 0) { return 0; }
    //sign
    UINT64 esp64_val = (val & 0x80000000) << 32;
    //exponent
    esp64_val |= ((val & 0x40000000) > 0) ?
        (UINT64)0x4000000000000000ULL : (UINT64)0x3800000000000000ULL;
    //mantissa
    esp64_val |= (val & 0x3FFFFFFF) << 29;
    return esp64_val;
}


//half:
//x    xxxxx       xxxxxxxxxx
//sign exponent    mantissa
//15   14 ~ 10     9 ~ 0
//EHP64:
//x    1000000xxxx xxxxxxxxxx000000000000000000000000000000000000000000
//x    0111111xxxx xxxxxxxxxx000000000000000000000000000000000000000000
UINT64 half2EHP64(UINT64 val)
{
    if (val == 0) { return 0; }
    //sign
    UINT64 ehp64_val = (val & 0x8000) << 48;
    //exponent
    ehp64_val |= ((val & 0x4000) > 0) ?
        (UINT64)0x4000000000000000ULL : (UINT64)0x3F00000000000000ULL;
    //mantissa
    ehp64_val |= (val & 0x3FFF) << 42;
    return ehp64_val;
}


//Arrangement
//P(n,m)=n*(n-1)*...*(n-m+1)=n!/(n-m)!
UINT arra(UINT n, UINT m)
{
    ASSERTN(n != 0 && m != 0 && n >= m, ("illegal param"));
    UINT l = n - m + 1, i = n, res = 1;
    while (i >= l) {
        res = res * i;
        i--;
    }
    return res;
}


//Combination
//C(n,m)=(n*(n-1)*...*(n-m+1))/m! = n!/m!(n-m)!
//Simplify:C(n,m)=(n*(n-1)*(m+1))/(n-m)!
UINT combin(UINT n, UINT m)
{
    ASSERTN(n != 0 && m != 0 && n >= m, ("illegal param"));
    if (n == m) {
        return 1;
    }
    UINT l = m + 1, i = n, res = 1;
    while (i >= l) {
        res = res * i;
        i--;
    }
    res = res / fact(n-m);
    return res;
}


//Convert char value into binary.
//e.g: char p = ' '; p is blank.
INT xctoi(CHAR const* cl)
{
    #ifndef BYTE_PER_INT
        #define BYTE_PER_INT 4
    #endif

    #ifndef BIT_PER_BYTE
        #define BIT_PER_BYTE 8
    #endif

    if (cl == nullptr || xstrcmp(cl, "", 1)) { return 0; }
    INT l = (INT)::strlen(cl);
    if (l > BYTE_PER_INT) {
        ASSERTN(0, ("too many characters in integer"));
        return 0;
    }
    INT i = 0;
    INT r = 0;
    while (i < l) {
        r |= cl[i] << (i * BIT_PER_BYTE);
        i++;
    }
    return r;
}


static LONGLONG hextoll(CHAR const* nptr)
{
    LONGLONG res = 0;
    UCHAR c = *nptr;
    while (xisdigithex(c)) {
        if ((c >= '0') & (c <= '9')) {
            res = 16 * res + c - '0';
            c = *++nptr;
            continue;
        }
        c = upper(c);
        if ((c >= 'A') & (c <= 'F')) {
            res = 16 * res + c - 'A' + 10;
            c = *++nptr;
            continue;
        }
        return 0; //Just return 0 to do error recovery.
    }
    return res;
}


static LONGLONG bintoll(CHAR const* nptr)
{
    LONGLONG res = 0;
    UCHAR c = *nptr;
    while (xisdigitbin(c)) {
        res = 2 * res + c - '0';
        c = *++nptr;
    }
    //LONGLONG res = 0;
    //UCHAR c = *nptr;
    //if (!xisdigitbin(c)) {
    //    return 0; //Just return 0 to do error recovery.
    //}
    //LONGLONG powof2 = 1;
    //res = c - '0';
    //c = *++nptr;
    //while (xisdigitbin(c)) {
    //    res = powof2 * (c - '0') + res;
    //    c = *++nptr;
    //}
    return res;
}


//Convert a string into long integer.
//e.g: cl = '1','2','3','4','5'
//return 12345.
//'is_oct': if true, nptr is octal digits.
LONGLONG xatoll(CHAR const* nptr, bool is_oct)
{
    if (nptr == nullptr) { return 0; }
    while (*nptr == ' ' || *nptr == '\t') {
        nptr++;
    }
    CHAR sign = *nptr;
    if (sign == '-' || sign == '+') {
        nptr++;
    }
    LONGLONG res = 0;
    if (nptr[0] == '0' && upper(nptr[1]) == 'X') { //hex
        nptr += 2;
        res = hextoll(nptr);
    } else if (nptr[0] == '0' && upper(nptr[1]) == 'B') { //binary
        nptr += 2;
        res = bintoll(nptr);
    } else if (is_oct) { //octal
        while (*nptr >= '0' && *nptr <= '7') {
            res = 8 * res + (*nptr - '0');
            nptr++;
        }
    } else { //decimal
        while (*nptr >= '0' && *nptr <= '9') {
            res = 10 * res + (*nptr - '0');
            nptr++;
        }
    }
    if (sign == '-') {
        return -res;
    }
    return res;
}


//Find partial string, return the subscript-index if substring found,
//otherwise return -1.
//src: input string.
//par: partial string.
LONG xstrstr(CHAR const* src, CHAR const* par)
{
    for (CHAR const* seg = src; *seg != 0;) {
        size_t span = 0;
        CHAR const* p = par;
        CHAR const* s = seg;
        for (; *p != 0 && *s != 0 && *s == *p; p++, s++) {
            if (span == 0 && *s == par[0]) {
                span = s - seg;
            }
        }
        if (*p == 0) { return (INT)(seg - src); }
        //Now, s is pointing to the last different char.
        s++;
        if (span == 0 && *s == par[0]) {
            span = s - seg;
        }
        if (span != 0) {
            seg += span;
        } else {
            seg = s;
        }
    }
    return -1;
}


//Split string by given separator, and return the number of substring.
//str: input string.
//ret: record each substring which separated by sep.
//sep: separator.
//Note caller is responsible for the free of each string memory in ret.
UINT xsplit(CHAR const* str, CHAR const* sep, OUT StrBufVec & ret)
{
    ASSERT0(str);
    ASSERTN(::strlen(sep) == 1, ("separator must be single character"));
    CHAR const* start = str;
    CHAR const* end = str;
    UINT num = 0;
    UINT len = 0;
    for (; *end != 0;) {
        if (*end != *sep) { len++; end++; continue; }
        StrBuf * substrbuf = ret.genStrBuf(num);
        substrbuf->growBuf(len + 1);
        ::memcpy(substrbuf->buf, start, len);
        substrbuf->buf[len] = 0;
        num++;
        len = 0;
        end++;
        start = end;
    }
    StrBuf * substrbuf2 = ret.genStrBuf(num);
    substrbuf2->growBuf(len + 1);
    ::memcpy(substrbuf2->buf, start, len);
    substrbuf2->buf[len] = 0;
    num++;
    end++;
    start = end;
    return num;
}


CHAR const* xstrcpy(CHAR const* src, size_t bytesize, OUT StrBuf & tgt)
{
    size_t l = MIN(::strlen(src), bytesize);
    tgt.growBuf((UINT)(l + 1)); //include '\0'.
    ::memcpy(tgt.buf, src, l);
    tgt.buf[l] = 0;
    return tgt.getBuf();
}


CHAR * rotateString(MOD CHAR * str, UINT n)
{
    xcom::rotate_buffer(str, ::strlen(str), n);
    return str;
}


//Reverse the string.
CHAR * reverseString(CHAR * v)
{
    xcom::reverse_buffer(v, ::strlen(v));
    return v;
}


//Convert long to string.
CHAR * xltoa(LONG v, OUT CHAR * buf)
{
    UCHAR const p [] = {'0','1','2','3','4','5','6','7','8','9'};
    bool sign = false;
    if (v < 0) { v = -v; sign = true; }
    CHAR * str = buf;

    LONG rem = 0;
    while (v != 0) {
        rem = v % 10;
        v /= 10;
        *str++ = p[rem];
    }
    if (sign) {
        *str++ = '-';
    }
    *str = 0; //end of string
    reverseString(buf);
    return buf;
}


static void _prim(INT m, INT n, OUT INT * buf, UINT i)
{
    if (m > n) {
        while (m % n != 0) { n++; }
        m = m / n; //Factorize 'm' to two composite-number.
        buf[i++] = n;
        _prim(m, n, buf, i);
    }
}


//Prime Factorization.
//e.g:435234 = 251 * 17 * 17 * 3 * 2
void prim(INT m, OUT INT * buf)
{
    ASSERT0(buf);
    bool sign = false;
    buf[0] = 0;
    if (m < 0) {
        sign = true;
        m = -m;
    }

    _prim(m, 2, buf, 0);

    if (sign) {
        buf[0] = -buf[0];
    }
}


//Reverse a LONG type integer by lexicalgraph.
//e.g: if 'd' is 0x12345678, return 0x78563412.
LONG revlong(LONG d)
{
    CHAR * c = (CHAR*)&d, m;
    m = c[0], c[0] = c[3], c[3] = m;
    m = c[1], c[1] = c[2], c[2] = m;
    return d;
}


//Convert floating point string into binary words.
void af2i(IN CHAR * f, OUT BYTE * buf, UINT buflen, bool is_double)
{
    DUMMYUSE(is_double);
    DUMMYUSE(buflen);
    DUMMYUSE(buf);
    DUMMYUSE(f);
    ASSERT0(f && buf);
    ASSERTN(0, ("TODO"));
}


//Compute the power of 2, return the result.
//Note v must be power of 2.
//e.g: given v is 64, return 16.
UINT getPowerOf2(ULONGLONG v)
{
    ASSERT0(v > 0);
    ASSERT0(!(v & (v - 1)));
    UINT n = 0;
    while ((v & 1) == 0) {
        v = v >> 1;
        n++;
    }
    return n;
}


//Compute the number of 1.
UINT getSparsePopCount(ULONGLONG v)
{
    int n = 0;
    while (v != 0) {
        v = v & (v - 1);
        n++;
    }
    return n;
}


//Compute the number of 1.
UINT getLookupPopCount(ULONGLONG v)
{
    BYTE * p = (BYTE*)&v;
    return g_bit_count[p[0]] + g_bit_count[p[1]] +
           g_bit_count[p[2]] + g_bit_count[p[3]] +
           g_bit_count[p[4]] + g_bit_count[p[5]] +
           g_bit_count[p[6]] + g_bit_count[p[7]];
}


//Ceil rounding alignment.
//e.g  v=17 , align=4 , the result is 20.
LONGLONG ceil_align(LONGLONG v, LONGLONG align)
{
    if (align == 0 || align == 1) { return v; }
    if ((v % align) != 0) {
        v = (v / align + 1) * align;
    }
    return v;
}


//Extract file path.
//e.g: Given /xx/yy/zz.file, return /xx/yy
CHAR * getFilePath(CHAR const* n, OUT CHAR * buf, UINT bufl)
{
    if (n == nullptr) { return nullptr; }

    ASSERT0(buf);
    INT l = (INT)::strlen(n);
    INT i = l;
    while (n[i] != '\\' && n[i] != '/' && i >= 0) {
        i--;
    }

    if (i < 0) {
        return nullptr;
    }

    DUMMYUSE(bufl);
    ASSERT0(i < (INT)bufl);
    ::memcpy(buf, n, i);
    buf[i] = 0;
    return buf;
}


//Shift a string to right side or left side.
//ofst: great than zero means shifting string to right side,
//   and the displacement is abs(ofst); negative
//   means shifting string to left.
void strshift(MOD CHAR * string, INT ofst)
{
    INT len = (INT)::strlen(string), i;
    if (string == nullptr) { return; }

    if (ofst >= 0) { //shift to right
        if (ofst >= len) {
            string[0] = 0;
        } else {
            string[len - ofst] = 0;
        }
    } else if (ofst < 0) { //shift to left
        if ((-ofst) >= len) {
            string[0] = 0;
        } else {
            INT diff = len + ofst;
            ofst = -ofst;
            i = 0;
            while (i < diff ) {
                string[i] = string[ofst + i];
                i++;
            }
            string[i] = 0;
        }
    }
}


//Extract file name.
//e.g: Given /xx/yy/zz.foo, return zz.
CHAR * getFileName(CHAR const* path, OUT CHAR * buf, UINT bufl)
{
    DUMMYUSE(bufl);
    if (path == nullptr) { return nullptr; }
    INT l = (INT)::strlen(path);
    INT i = l;
    INT dotpos = -1;
    while (path[i] != '\\' && path[i] != '/' && i >= 0) {
        if (path[i] == '.') {
            dotpos = i;
        }
        i--;
    }
    i++;
    ASSERT0(i < (INT)bufl);
    UINT start;
    UINT end;
    if (i < 0) { start = 0; }
    else { start = i; }

    if (dotpos < 0) { end = l; }
    else { end = dotpos; }

    UINT len = end - start;
    if (len > 0) {
        ::memcpy(buf, path + start, len);
    }
    buf[len] = 0;
    return buf;
}


//Extract file suffix.
//e.g: Given a.foo, return foo.
CHAR * getFileSuffix(CHAR const* n, OUT CHAR * buf, UINT bufl)
{
    if (n == nullptr) { return nullptr; }

    INT l = (INT)::strlen(n);
    INT i = l;
    while (n[i] != '.' && i >= 0) {
        i--;
    }
    DUMMYUSE(bufl);
    if (i < 0) { return nullptr; }
    ASSERT0((UINT)(l - i -1) < bufl);
    ::memcpy(buf, n + i + 1, l - i -1);
    buf[l - i -1] = 0;
    return buf;
}


//Extract the right most sub-string which separated by 'separator' from string.
//e.g: Given string is a\b\c, separator is '\', return c;
CHAR const* extractRightMostSubString(CHAR const* string, CHAR separator)
{
    size_t l = ::strlen(string);
    CHAR const* p = string + l;
    for (; l != 0; l--, p--) {
        if (*p == separator) {
            return p + 1;
        }
    }

    //Not found method name.
    return p;
}


ULONGLONG extractBitRangeValue(ULONGLONG val, UINT start, UINT end)
{
    ASSERT0(start < sizeof(ULONGLONG) * BITS_PER_BYTE);
    ASSERT0(end < sizeof(ULONGLONG) * BITS_PER_BYTE);
    ASSERT0(start <= end);
    UINT lastbit = sizeof(ULONGLONG) * BITS_PER_BYTE - 1;
    UINT size = end - start;
    val = val << (lastbit - end);
    val = val >> (lastbit - size);
    return val;
}


//Extract the left most sub-string which separated by 'separator' from string.
void extractLeftMostSubString(CHAR * tgt, CHAR const* string, CHAR separator)
{
    CHAR const* p = string;
    for (CHAR c = *p; c != 0; p++, c = *p) {
        if (c == separator) {
            break;
        }
    }

    size_t l = p - string;
    ::memcpy(tgt, string, l);
    tgt[l] = 0;
}


UINT xstrlen(CHAR const* p)
{
    UINT len = 0;
    while (*p != '\0') {
        if (*p == '\\' && *(p+1) == 'n') {
            p += 2;
        } else {
            p++;
        }
        len++;
    }
    return len;
}


bool xstrcmp(CHAR const*RESTRICT p1, CHAR const*RESTRICT p2, INT n)
{
    //Note it does not have to judge whether current char is terminate char.
    while (n-- > 0 && *p1++ == *p2++) {}
    return n < 0;
}


CHAR * upper(CHAR * n)
{
    if (n == nullptr) { return nullptr; }
    LONG l = (LONG)::strlen(n);
    l--;
    while (l >= 0) {
        n[l] = upper(n[l]);
        l--;
    }
    return n;
}


CHAR * lower(CHAR * n)
{
    if (n == nullptr) { return nullptr; }
    LONG l = (LONG)::strlen(n);
    l--;
    while (l >= 0) {
        n[l] = lower(n[l]);
        l--;
    }
    return n;
}


//Get the index of the first '1' start at most right side.
//e.g: given m=0x8, the first '1' index is 3.
INT getFirstOneAtRightSide(INT m)
{
    static const INT dbitpos[32] = {
      0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
      31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
    };
    return dbitpos[((UINT)((m & -m) * 0x077CB531U)) >> 27];
}


bool isExceedBitWidth(ULONGLONG val, UINT bitwidth)
{
    ASSERTN(bitwidth <= sizeof(ULONGLONG) * BITS_PER_BYTE, ("TODO"));
    if (bitwidth == sizeof(ULONGLONG) * BITS_PER_BYTE) { return true; }
    return val > ((((ULONGLONG)1) << bitwidth) - 1);
}


//Judge if 'f' is integer conform to IEEE754 spec.
bool isIntegerF(float f)
{
    //0000 0000 0111 1111 1111 1111 1111 1111 //mantissa
    //0111 1111 1000 0000 0000 0000 0000 0000 //exponential
    float * p = &f;
    INT i = *(INT*)p;
    INT m = i & 0x007FFFFF; //mantissa
    INT n = ((i & 0x7F800000) >> 23) - 127; //number of exponential
    INT j = getFirstOneAtRightSide(m);
    return 23 - j <= n;
}


//Judge if 'd' is integer conform to IEEE754 spec.
bool isIntegerD(double d)
{
    //0000 0000 0000 1111 1111 1111 1111 1111 1111 1111 1111
    //1111 1111 1111 1111 1111 //mantissa
    double * p = &d;
    LONGLONG i = *(LONGLONG*)p;
    LONGLONG m = i & 0x000FFFFFffffffffull; //mantissa
    //number of exponential
    INT n = (INT)((i & 0x7FF0000000000000ull) >> 52) - 1023;
    INT j = 0;
    while (j < 52) {
        if ((m & 0x1) == 1) { break; }
        m = m >> 1;
        j++;
    }
    return 52 - j <= n;
}


//Return true if 'f' represents a finite floating-point value.
//f: is conform to IEEE754 spec.
bool isFiniteD(double f)
{
    //mantissa
    //0000 0000 0000
    //1111 1111 1111 1111 1111 1111 1111 1111 1111 1111 1111 1111 1111

    //exponential
    //11111111111111
    //0000000000000000000000000000000000000000000000000000000000000000
    double * p = &f;
    LONGLONG i = *(LONGLONG*)p;
    LONGLONG mask = 0x7FF0000000000000ull; //mask indicates bits of infinite.
    return (i & mask) != mask;
}


//Return true if 'f' represents a finite floating-point value.
//f: is conform to IEEE754 spec.
bool isFiniteF(float f)
{
    //0000 0000 0111 1111 1111 1111 1111 1111 //mantissa
    //0111 1111 1000 0000 0000 0000 0000 0000 //exponential
    float * p = &f;
    INT i = *(INT*)p;
    INT mask = 0x7F800000; //mask indicates bits of infinite.
    return (i & mask) != mask;
}


bool isPowerOf5(double f)
{
    ASSERT0(f >= 0.0);
    if (f == 0.0 || !::isFiniteD(f)) { return false; }
    while (f >= 5.0) {
        f = f / 5.0;
        if (f == 1.0) {
            return true;
        }
    }
    return false;
}


//Return the position in array if find v.
//array: sorted in incremental order.
//n: elements size of array.
//v: search v in array.
bool binsearch(INT array[], UINT n, INT v, MOD UINT * ppos)
{
    if (n == 0) { return false; }
    if (n == 1 && array[0] != v) { return false; }

    //n >= 2
    UINT lo = 0;
    UINT hi = n - 1;
    while (lo <= hi) {
        UINT pos = lo + (hi - lo) / 2;
        if (v < array[pos]) {
            hi = pos - 1;
        } else if (v > array[pos]) {
            lo = pos + 1;
        } else {
            *ppos = pos;
            return true;
        }
    }
    return false;
}


LONG getclockstart()
{
    return clock();
}


float getclockend(LONG start)
{
    return (float)(clock() - start) / CLOCKS_PER_SEC;
}


//Get current micro-second.
ULONGLONG getusec()
{
#ifdef _ON_WINDOWS_
    time_t timer;
    timer = time(&timer);

    //Time Format as: Local time is: Fri Jan 17 16:26:27 2013
    //struct tm * tblock = localtime(&timer);
    //printf("Local time is: %s", asctime(tblock));
    return (ULONGLONG)timer;
#else
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv, &tz);
    return (((ULONGLONG)tv.tv_sec) * 1000000LL + tv.tv_usec) / 1000000LL;
#endif
}


static inline bool prtchar(CHAR * buf, UINT buflen, UINT * pbufpos, CHAR c)
{
    if ((*pbufpos) >= buflen) {
        return false;
    } else {
        buf[*pbufpos] = c;
        (*pbufpos)++;
    }
    return true;
}


static bool prt_ulong(CHAR * buf, UINT buflen, UINT * pbufpos, ULONG v)
{
    if (v == 0) {
        if (!prtchar(buf, buflen, pbufpos, '0')) {
            return false;
        }
        return true;
    }

    //Count up the number of digits.
    //e.g: 123, digits=3
    INT digits = 0;
    ULONG v2 = v;
    while (v2 != 0) {
        v2 /= 10;
        digits++;
    }

    //write out by descent order
    (*pbufpos) += digits;
    for (INT digno = 1; digno <= digits; digno++){
        CHAR ch = g_hexdigits[v % 10];
        if ((*pbufpos) - digno < buflen) {
            buf[(*pbufpos) - digno] = ch;
        } else {
            return false; //buffer overflow.
        }
        v /= 10;
    }
    return true;
}


static bool prt_int(CHAR * buf, UINT buflen, UINT * pbufpos, INT v)
{
    if (v < 0) {
        if (!prtchar(buf, buflen, pbufpos, '-')) {
            return false;
        }
        v = -v;
    }
    return prt_ulong(buf, buflen, pbufpos, v);
}


static bool prt_ulong_hex(CHAR * buf, UINT buflen, UINT * pbufpos,
                          ULONG v, INT format_size)
{
    ULONG mask = 0xF0000000;
    INT shift = 28;
    if (format_size > 0 && format_size < 8) {
        mask >>= 4*(8-format_size);
        shift -= 4*(8-format_size);
    } else {
        format_size = 8;
    }

    for (INT digno = 0; digno < format_size; digno++) {
        UINT digit = (UINT)(v & mask) >> shift;
        if (!prtchar(buf, buflen, pbufpos, g_hexdigits[digit])) {
            return false;
        }
        mask >>= 4;
        shift -= 4;
    }
    return true;
}


static bool prt_ansi_str(CHAR * buf, UINT buflen, UINT * pbufpos,
                         CHAR * s, INT format_size)
{
    CHAR ch;
    bool sized = (format_size > 0);
    if (s == nullptr) { return true; }
    while ((ch = *s++) != 0) {
        if (!prtchar(buf, buflen, pbufpos, ch)) {
            return false;
        }
        if (sized && (--format_size == 0)) {
            break;
        }
    }
    return true;
}


static bool prt_wide_str(CHAR * buf, UINT buflen, UINT * pbufpos,
                         wchar_t * ws, INT format_size)
{
    wchar_t ch;
    bool sized = (format_size > 0);
    while ((ch = *ws++) != 0) {
        if (!prtchar(buf, buflen, pbufpos, (CHAR)ch)) {
            return false;
        }
        if (sized && (--format_size == 0)) {
            break;
        }
    }
    return true;
}


static bool is_exist_mark(CHAR const* format)
{
    CHAR const* p = format;
    UINT i = 0;
    while (p[i] != 0) {
        switch (p[i]) {
        case 'c': //ANSI char
        case 'd': //Integer
        case 'i': //Integer
        case 'u': //ULONG as decimal
        case 'x': //ULONG as hex
        case 's': //ANSI string
        case 'S': //Wide string
            return true;
        }
        i++;
    }
    return false;
}


//Parse string that starts with '%'.
static bool percent(CHAR * buf, UINT buflen, MOD UINT * bufpos,
                    MOD CHAR const** format, va_list stack_start)
{
    //The info related to %, e.g:'%d', can not longer than 255.
    CHAR sbuf[255];
    sbuf[0] = 0;
    UINT tpos = 0;
    UINT format_size = 0;
    CHAR const* format_pos = *format;
    CHAR ch = *++format_pos; //The first char followed '%'.
    format_pos++;
    bool is_mark_positive = false;
    bool is_align_left = false;
    bool is_stuf_zero_left = false;
    bool is_nega = false;

    if (ch == '%') {
        //put double '%' , if we encounter '%%...'
        if (!prtchar(buf, buflen, bufpos, ch)) { goto OVER; }
        if (!prtchar(buf, buflen, bufpos, ch)) { goto OVER; }
        ch = *format_pos++;
        goto FIN;
    }

    //+/-/0/[0-9]
    if (ch == '+') {
        if (!is_exist_mark(format_pos)) {
            if (!prtchar(buf, buflen, bufpos, '%')) { goto OVER; }
            goto FIN;
        }
        is_mark_positive = true;  // add '+'
        ch = *format_pos++;
        goto DIGIT0;
    } else if (ch == '-') {
        if (!is_exist_mark(format_pos)) {
            if (!prtchar(buf, buflen, bufpos, '%')) { goto OVER; }
            goto FIN;
        }
        is_align_left = true; //print digit left-align
        ch = *format_pos++;
        goto DIGIT0;
    } else if (xisdigit(ch)) {
        if (!is_exist_mark(format_pos)) {
            if (!prtchar(buf, buflen, bufpos, '%')) { goto OVER; }
            goto FIN;
        }
        goto DIGIT0;
    } else {
        goto LETTER;
    }

DIGIT0:
    while (ch == '0') {
        if (!is_align_left) {
            //e.g print '1234' such as '+00001234'
            is_stuf_zero_left = true;
        }
        ch = *format_pos++;
    }

    if (ch >= '1' && ch <= '9') {
        //Digial.
        format_size = 0;

        //e.g: %20d, format_size is 20.
        do {
            format_size = (format_size * 10) + ch - '0';
            if ((ch = *format_pos++) == 0) {
                break;
            }
        } while (ch >= '1' && ch <= '9');
    }

LETTER:
    tpos = 0;
    //Switch on marker character
    switch (ch) {
    case 'c': //ANSI CHAR
        {
            //CHAR is promoted to INT when passed through '...'.
            //So you should pass 'INT' not 'CHAR' to 'va_arg'.
            //If this code is reached, the program will abort.
            CHAR c = (CHAR)va_arg(stack_start, INT);
            if (!prtchar(buf, buflen, bufpos, c)) { goto OVER; }
        }
        break;
    case 'd'://    Integer
    case 'i'://    Integer
        {
            INT i = va_arg(stack_start, INT);
            if (i < 0) { is_nega = true; }
            if (!prt_int(sbuf, buflen, &tpos, i)) { goto OVER; }
        }
        break;
    case 'u'://    ULONG as decimal
        {
            ULONG uv = va_arg(stack_start, ULONG);
            if (!prt_ulong(sbuf, buflen, &tpos, uv)) { goto OVER; }
        }
        break;
     case 'x':// ULONG as hex
        {
            ULONG uv = va_arg(stack_start, ULONG);
            if (!prt_ulong_hex(sbuf, buflen, &tpos, uv, format_size)) {
                goto OVER;
            }
        }
        break;
    case 's'://    ANSI string
        {
            CHAR * s = va_arg(stack_start, CHAR*);
            if (!prt_ansi_str(sbuf, buflen, &tpos, s, format_size)) {
                goto OVER;
            }
        }
        break;
    case 'S'://    Wide string
        {
            wchar_t * s = va_arg(stack_start, wchar_t*);
            if (!prt_wide_str(sbuf, buflen, &tpos, s, format_size)) {
                goto OVER;
            }
        }
        break;
    case 'f':
        {} //TODO
    default:
        goto FIN;
    }

    if (is_mark_positive) {
        if (is_nega) {
            if (!prtchar(buf, buflen, bufpos, '-')) { goto OVER; }

            //remove '-'
            strshift(sbuf, -1);
            tpos -= 1;
        } else {
            if (!prtchar(buf, buflen, bufpos, '+')) { goto OVER; }
        }
    }
    if (is_align_left) {
        if (tpos + *bufpos >= buflen) { goto OVER; }
        if (tpos < format_size) {
            ::memcpy(buf + *bufpos, sbuf, tpos);
            *bufpos += tpos;
            for (UINT i = 0; i < format_size - tpos; i++) {
                if (!prtchar(buf, buflen, bufpos, ' ')) {
                    goto OVER;
                }
            }
        }else{
            ::memcpy(buf + *bufpos, sbuf, tpos);
            *bufpos += tpos;
        }
        goto FIN;
    }
    if (is_stuf_zero_left) {
        if (tpos + *bufpos >= buflen) goto OVER;
        if (tpos < format_size) {
            for(UINT i = 0; i < format_size - tpos; i++) {
                if (!prtchar(buf, buflen, bufpos, '0')) {
                    goto OVER;
                }
            }
            ::memcpy(buf + *bufpos, sbuf, tpos);
            *bufpos += tpos;
        } else {
            ::memcpy(buf + *bufpos, sbuf, tpos);
            *bufpos += tpos;
        }
        goto FIN;
    }
    if (format_size > 0) {
        if (tpos < format_size) {
            for (UINT i = 0; i < format_size - tpos; i++) {
                if (!prtchar(buf, buflen, bufpos, ' ')) {
                    goto OVER;
                }
            }
            ::memcpy(buf + *bufpos, sbuf, tpos);
            *bufpos += tpos;
        } else {
            ::memcpy(buf + *bufpos, sbuf, tpos);
            *bufpos += tpos;
        }
        goto FIN;
    }
    ::memcpy(buf + *bufpos, sbuf, tpos);
    *bufpos += tpos;
FIN:
    *format = format_pos;
    return true;
OVER: //We got some problems, and going to the xsprintf.
    *format = format_pos;
    return false;
}


static bool back_slash(CHAR * buf, UINT buflen, MOD UINT * bufpos,
                       MOD CHAR const** format)
{
    CHAR const* pos = *format;
    CHAR ch = *pos++;

    // '\n' '\t' '\b'
    if (ch == '\\') {
        if (!prtchar(buf, buflen, bufpos, ch)) { goto OVER; }
        ch = *pos++;
    } else {
        switch (ch) {
        case 'n':
            ch = *pos++;
            if (!prtchar(buf, buflen, bufpos, '\n')) { goto OVER; }
            break;
        case 't':
            ch = *pos++;
            if (!prtchar(buf, buflen, bufpos, '\t')) { goto OVER; }
            break;
        case 'b':
            ch = *pos++;
            if (!prtchar(buf, buflen, bufpos, ' ')) { goto OVER; }
            break;
        }
    }
    *format = pos;
    return true;
OVER: //Get some problems.
    *format = pos;
    return false; //going to exit xsprintf.
}


//Format string and record in buf.
//buf: output buffer that record string.
//buflen: length of output buffer.
CHAR * xsprintf(MOD CHAR * buf, UINT buflen, CHAR const* format, ...)
{
    UINT bufpos = 0;
    CHAR ch = *format;
    va_list stack_start;
    va_start(stack_start, format);
    //Walk through each characters.
    while (ch != '\0') {
        switch(ch){
        case '\\':
            if (!back_slash(buf, buflen, &bufpos, &format)) {
                goto OVER;
            }
            break;
        case '%':
            if (!percent(buf, buflen, &bufpos, &format, stack_start)) {
                goto OVER;
            }
            break;
        default:
            //General usage character.
            if (!prtchar(buf, buflen, &bufpos, ch)) {
                goto OVER;
            }
            format++;
        }
        ch = *format;
    }
OVER:
    // nullptr terminate string
    prtchar(buf, buflen, &bufpos, '\0');

    // Ensure string terminated
    buf[buflen - 1] = '\0';
    va_end(stack_start);
    return buf;
}


static BYTE g_char2hex[] = {
    0, //'0'
    1, //'1'
    2, //'2'
    3, //'3'
    4, //'4'
    5, //'5'
    6, //'6'
    7, //'7'
    8, //'8'
    9, //'9'
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    10, //'A'
    11, //'B'
    12, //'C'
    13, //'D'
    14, //'E'
    15, //'F'
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    10, //'a'
    11, //'b'
    12, //'c'
    13, //'d'
    14, //'e'
    15, //'f'
};


//The function converts a character into hex.
//Note the content in given buf must be string format of hex, that is 'c'
//can only be "abcdefABCDEF0123456789".
BYTE charToHex(CHAR c)
{
    ASSERT0(c >= '0' && c <= 'f');
    return g_char2hex[c - '0'];
}


//The function converts string content into set of hex and store in the 'buf'.
//Note the content in given buf must be string format of hex, that is the
//string can only contain "abcdefABCDEF0123456789".
//string: a string that represents bytes, where a byte consists of two hex.
//        e.g:ADBC5E demostrates three bytes, 0xAD, 0xBC, and 0x5E.
void charToByteHex(CHAR const* string, OUT BYTE * buf, UINT buflen)
{
    size_t stringlen = ::strlen(string);
    ASSERTN(stringlen/2 <= buflen,
            ("buffer is too small to hold hex, expect %u bytes", stringlen/2));
    for (UINT i = 0; i < stringlen - 1; i += 2) {
        BYTE high = charToHex(string[i]);
        BYTE low = charToHex(string[i + 1]);
        buf[i/2] = (BYTE)(low | (high << 4));
    }
}


INT getSign32BitLow16BitVal(INT32 val)
{
    return ((val & 0xffff) ^ 0x8000) - 0x8000;
}


INT getSign64BitLow16BitVal(INT64 val)
{
    return ((val & 0xffff) ^ 0x8000) - 0x8000;
}


INT getSign64BitLow32BitVal(INT64 val)
{
    return ((val & 0xffffffff) ^ 0x80000000) - 0x80000000;
}


void splitSign64BitToFourParts(INT64 val, OUT INT & part1, OUT INT & part2,
                               OUT INT & part3, OUT INT & part4)
{
    //Get 0~15 bits number.
    part1 = getSign64BitLow16BitVal(val);

    val -= part1;

    //Get 0~31 bits number.
    part2 = getSign64BitLow32BitVal(val);

    //Get 32~63 bits number.
    val = (val - part2) >> 32;

    //Get 15~31 bits number.
    part2 >>= 16;

    //Get 32~47 bits number.
    part3 = getSign64BitLow16BitVal(val);

    val -= part3;

    //Get 47~63 bits number.
    part4 = getSign64BitLow32BitVal(val);
    part4 >>= 16;
}


UINT getSignBit(UINT bitsize)
{
    //Note that bitsize should be greater than or equal to 8 and be a
    //multiple of 8.
    ASSERTN(bitsize >= 8, ("Type bitsize is too small.\n"));
    ASSERTN(bitsize % 8 == 0, ("Type bitsize must be multiple of 8.\n"));

    return bitsize - 1;
}


void replaceFileNameSuffix(CHAR const* org_file_name, CHAR const* new_suffix,
                           OUT StrBuf & new_file_name)
{
    //Get prefix of given file name (or file path name).
    //For example: "yyy.c" -> "yyy" or "/tmp/xxx.cpp" -> "xxx"
    CHAR * prefix = (CHAR*)::malloc(::strlen(org_file_name));
    prefix = getFileName(org_file_name, prefix, (UINT)::strlen(org_file_name));
    ASSERT0(::strlen(prefix) != 0);

    //Concat prefix with suffix to get new file name.
    //For example: prefix: "xxx", suffix: ".o", new file name: "xxx.o".
    new_file_name.strcat(prefix);
    new_file_name.strcat(new_suffix);

    ::free(prefix);
}


bool isValidImmForBitsize(UINT bitsize, UINT64 imm)
{
    UINT64 mask = (((UINT64)1) << bitsize) - 1;
    return (mask & imm) == imm;
}


float xsqrt(float num)
{
    if (num <= float(0.0)) {
        return float(0.0);
    }
    float epsilon = float(0.00001);
    float guess = num;
    while (xfabs(guess * guess - num) >= epsilon) {
        guess = (guess + num / guess) / float(2.0);
    }
    return guess;
}


double xsqrt(double num)
{
    double x = num;
    double y = (x + num / x) / double(2.0);
    double epsilon = double(0.000000001);
    while (xfabs(y - x) > epsilon) {
        x = y;
        y = (x + num / x) / double(2.0);
    }
    return y;
}

} //namespace xcom
