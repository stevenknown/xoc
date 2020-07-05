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

#include "ltype.h"
#include "comf.h"
#include "stdio.h"
#include "string.h"
#include "smempool.h"
#include "sstl.h"
#include "bs.h"

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


//Caculate the number of bits which longer enough to represent given constant.
UINT computeConstBitLen(ULONGLONG v)
{
#ifdef _VC6_
    if (!(v & 0xffffffffffffff00)) { return 8; }
    if (!(v & 0xffffffffffff0000)) { return 16; }
    if (!(v & 0xffffffff00000000)) { return 32; }
#else
    if (!(v & 0xffffffffffffff00ull)) { return 8; }
    if (!(v & 0xffffffffffff0000ull)) { return 16; }
    if (!(v & 0xffffffff00000000ull)) { return 32; }
#endif
    return 64;
}


//Append string to buf.
//This function will guarantee that the length of string does
//not exceed bufl.
CHAR * xstrcat(CHAR * buf, size_t bufl, CHAR const* info, ...)
{
    //CHAR * ptr = (CHAR*)&info;
    //ptr += sizeof(CHAR*);
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


//Great common divisor.
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


//Least common multiple.
INT slcm(INT x, INT y)
{
    return x * y / sgcd(x,y);
}


//Great common divisor for values stored in vector.
INT gcdm(UINT num, Vector<INT, 8> const& values)
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

    if (cl == NULL || strcmp(cl, "") == 0) { return 0; }
    INT l = (INT)strlen(cl);
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


//Convert a string into long integer.
//e.g: cl = '1','2','3','4','5'
//return 12345.
//'is_oct': if true, nptr is octal digits.
LONGLONG xatoll(CHAR const* nptr, bool is_oct)
{
    if (nptr == NULL) { return 0; }
    while (*nptr == ' ' || *nptr == '\t') {
        nptr++;
    }
    CHAR sign = *nptr;
    if (sign == '-' || sign == '+') {
        nptr++;
    }
    LONGLONG res = 0;
    if (nptr[0] == '0' && (nptr[1] == 'x' || nptr[1] == 'X')) { //hex
        nptr += 2;
        UCHAR c = *nptr;
        while ((c >= 'a' && c <= 'f') ||
               (c >= 'A' && c <= 'F') ||
               (c >= '0' && c <= '9')) {
            if (c >= '0' && c <= '9') {
                res = 16 * res + c - '0';
            } else if (c >= 'A' && c <= 'F') {
                res = 16 * res + c - 'A' + 10;
            } else if (c >= 'a' && c <= 'f') {
                res = 16 * res + c - 'a' + 10;
            } else {
                return 0;
            }
            c = *++nptr;
        }
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


LONGLONG xabs(LONGLONG a)
{
    return a >= 0 ? a : -a;
}


//Find partial string, return the subscript-index if substring found,
//otherwise return -1.
//
//'src': input string.
//'par': partial string.
//'i': find the ith partial string. And 'i' get started with 0.
//    If one only want to find the first partial string, i equals to 0.
LONG xstrstr(CHAR const* src, CHAR const* par, INT i)
{
    CHAR const* s = src;
    while (*s != 0) {
        CHAR const* p = par;
        CHAR const* q = s;
        while (*p != 0 && *p == *q) {
            p++;
            q++;
        }
        if (*p == 0) {
            if (i == 0) {
                return (LONG)(s - src);
            }
            i--;
        }
        s++;
    }
    return -1;
}


//Split string by given separetor, and return the number of substring.
//str: input string.
//ret: record each substring which separated by sep.
//sep: separator.
//Note caller is responsible for the free of each string memory in ret.
UINT xsplit(CHAR const* str, OUT Vector<CHAR*> & ret, CHAR const* sep)
{
    ASSERT0(str);
    ASSERTN(strlen(sep) == 1, ("separator must be single character"));
    CHAR const* start = str;
    CHAR const* end = str;

    UINT num = 0;
    UINT len = 0;
    for (; *end != 0;) {
        if (*end != *sep) { len++; end++; continue; }

        CHAR * substr = (CHAR*)::malloc(len + 1);
        ::memcpy(substr, start, len);
        substr[len] = 0;
        ret.set(num, substr);
        num++;
        len = 0;
        end++;
        start = end;
    }

    CHAR * substr = (CHAR*)::malloc(len + 1);
    ::memcpy(substr, start, len);
    substr[len] = 0;
    ret.set(num, substr);
    num++;
    len = 0;
    end++;
    start = end;

    return num;
}


void xstrcpy(CHAR * tgt, CHAR const* src, size_t size)
{
    size_t l = strlen(src);
    if (l >= size) {
        l = size - 1;
    }
    ::memcpy(tgt, src, l);
    tgt[l] = 0;
}


//Reverse the string.
UCHAR * reverseString(UCHAR * v)
{
    INT end = (INT)strlen((CHAR*)v) - 1;
    INT start = 0;
    while (start <= end - 1) {
        BYTE b = v[start];
        v[start] = v[end];
        v[end] = b;
        start++;
        end--;
    }
    return v;
}


//Convert long to string.
UCHAR * xltoa(LONG v, OUT UCHAR * buf)
{
    UCHAR const p [] = {'0','1','2','3','4','5','6','7','8','9'};
    bool sign = false;
    if (v < 0) { v = -v; sign = true; }
    UCHAR * str = buf;

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


//Dumpf() for Vector<TY>.
void dumpf_svec(void * vec, UINT ty, CHAR const* name, bool is_del)
{
    if (name == NULL) {
        name = "matrix.tmp";
    }

    if (is_del) {
        UNLINK(name);
    }

    static INT g_count = 0;
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    fprintf(h, "\nSVECOTR dump id:%d\n", g_count++);

    ///
    switch (ty) {
    case D_BOOL: {
        Vector<bool> *p = (Vector<bool> *)vec;
        for (INT i = 0; i <= p->get_last_idx(); i++) {
            fprintf(h, "%d", (INT)p->get(i));
            if (i != p->get_last_idx()) {
                fprintf(h, ", ");
            }
        }
        break;
    }
    case D_INT: {
        Vector<INT> * p = (Vector<INT> *)vec;
        for (INT i = 0; i <= p->get_last_idx(); i++) {
            fprintf(h, "%d", (INT)p->get(i));
            if (i != p->get_last_idx()) {
                fprintf(h, ", ");
            }
        }
        break;
    }
    default: ASSERTN(0, ("illegal ty"));
    }
    fprintf(h, "\n");
    fclose(h);
}


//Dumps() for Vector<TY>.
void dumps_svec(void * vec, UINT ty)
{
    printf("\n");
    switch (ty) {
    case D_BOOL: {
        Vector<bool> *p = (Vector<bool> *)vec;
        for (INT i = 0; i <= p->get_last_idx(); i++) {
            printf("%d", (INT)p->get(i));
            if (i != p->get_last_idx()) {
                printf(", ");
            }
        }
        break;
    }
    case D_INT: {
        Vector<INT> *p = (Vector<INT> *)vec;
        for (INT i = 0; i <= p->get_last_idx(); i++) {
            printf("%d", (INT)p->get(i));
            if (i != p->get_last_idx()) {
                printf(", ");
            }
        }
        break;
    }
    default: { ASSERTN(0, ("illegal ty")); }
    } //end switch
    printf("\n");
}


//Reverse a DWORD by lexicalgraph.
//e.g:if 'd' is 0x12345678, return 0x78563412
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


//Searchs for sub-string.
INT findstr(CHAR * src, CHAR * s)
{
    if (src == NULL || s == NULL) { return 0; }

    // can't have empty or NULL 'old'
    INT srclen = -1;
    CHAR * startp = src, * p, * q;
    INT l = 0;
    INT i = 0;
    srclen = (INT)strlen(src);
    p = startp;
    while (p[i] != 0) {
        if (p[i] == s[0]) {
            q = &p[i];
            l = 0;
            while (l < srclen && q[l] != 0) {
                if (q[l] != s[l]) { break; }
                l++;
            }

            if (s[l] == 0) {//match seacrching
                goto FIND;
            }
        }
        i++;
    }
    return 0;

FIND:
    return 1;
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
CHAR * getfilepath(CHAR const* n, OUT CHAR * buf, UINT bufl)
{
    if (n == NULL) { return NULL; }

    ASSERT0(buf);
    INT l = (INT)strlen(n);
    INT i = l;
    while (n[i] != '\\' && n[i] != '/' && i >= 0) {
        i--;
    }

    if (i < 0) {
        return NULL;
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
void strshift(IN OUT CHAR * string, INT ofst)
{
    INT len = (INT)strlen(string), i;
    if (string == NULL) { return; }

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
CHAR * getfilename(CHAR const* path, OUT CHAR * buf, UINT bufl)
{
    DUMMYUSE(bufl);
    if (path == NULL) { return NULL; }
    INT l = (INT)strlen(path);
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
CHAR * getfilesuffix(CHAR const* n, OUT CHAR * buf, UINT bufl)
{
    if (n == NULL) { return NULL; }

    INT l = (INT)strlen(n);
    INT i = l;
    while (n[i] != '.' && i >= 0) {
        i--;
    }
    DUMMYUSE(bufl);
    if (i < 0) { return NULL; }
    ASSERT0((UINT)(l - i -1) < bufl);
    ::memcpy(buf, n + i + 1, l - i -1);
    buf[l - i -1] = 0;
    return buf;
}


//Extract the right most sub-string which separated by 'separator' from string.
//e.g: Given string is a\b\c, separator is '\', return c;
CHAR const* extractRightMostSubString(CHAR const* string, CHAR separator)
{
    size_t l = strlen(string);
    CHAR const* p = string + l;
    for (; l != 0; l--, p--) {
        if (*p == separator) {
            return p + 1;
        }
    }

    //Not found method name.
    return p;
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
    while (*p != 0) {
        if (*p == '\\' && *(p+1) == 'n') {
            p += 2;
        } else {
            p++;
        }
        len++;
    }
    return len;
}


//Compare the first 'n' char of two string.
//Return true if equal.
bool xstrcmp(CHAR const* p1, CHAR const* p2, INT n)
{
    while (n-- > 0 && *p1++ == *p2++) { }
    if (n >= 0) { return true; }
    return false;
}


CHAR * upper(CHAR * n)
{
    if (n == NULL) { return NULL; }
    LONG l = (LONG)strlen(n);
    l--;
    while (l >= 0) {
        if (n[l] >= 'a' && n[l] <= 'z') {
            n[l] = n[l] - 32;
        }
        l--;
    }
    return n;
}


CHAR * lower(CHAR * n)
{
    if (n == NULL) { return NULL; }
    LONG l = (LONG)strlen(n);
    l--;
    while (l >= 0) {
        if (n[l] >= 'A' && n[l] <= 'Z') {
            n[l] = n[l] + 32;
        }
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


//Judge if 'f' is integer conform to IEEE754 spec.
bool isIntegerF(float f)
{
    //0000 0000 0111 1111 1111 1111 1111 1111 //mantissa
    //0111 1111 1000 0000 0000 0000 0000 0000 //exp
    float * p = &f;
    INT i = *(INT*)p;
    INT m = i & 0x007FFFFF; //mantissa
    INT n = ((i & 0x7F800000) >> 23) - 127; //number of exp
    INT j = getFirstOneAtRightSide(m);
    return 23 - j <= n;
}


//Judge if 'd' is integer conform to IEEE754 spec.
bool isIntegerD(double d)
{
    //0000 0000 0000 1111 1111 1111 1111 1111 1111 1111 1111 1111 1111 1111 1111 1111 //mantissa
    double * p = &d;
    LONGLONG i = *(LONGLONG*)p;
    LONGLONG m = i & 0x000FFFFFffffffffull; //mantissa
    INT n = (INT)((i & 0xFFF0000000000000ull) >> 52) - 1023; //number of exp
    INT j = 0;
    while (j < 52) {
        if ((m & 0x1) == 1) { break; }
        m = m >> 1;
        j++;
    }
    return 52 - j <= n;
}


bool isPowerOf5(double f)
{
    ASSERT0(f >= 0.0);
    if (f == 0.0) { return false; }
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
bool binsearch(INT array[], UINT n, INT v, IN OUT UINT * ppos)
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
    if (s == NULL) { return true; }
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
static bool percent(CHAR * buf,
                    UINT buflen,
                    IN OUT UINT * bufpos,
                    IN OUT CHAR const** format,
                    va_list stack_start)
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


static bool back_slash(CHAR * buf,
                       UINT buflen,
                       IN OUT UINT * bufpos,
                       IN OUT CHAR const** format)
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
CHAR * xsprintf(IN OUT CHAR * buf, UINT buflen, CHAR const* format, ...)
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
    // NULL terminate string
    prtchar(buf, buflen, &bufpos, '\0');

    // Ensure string terminated
    buf[buflen - 1] = '\0';
    va_end(stack_start);
    return buf;
}

} //namespace xcom
