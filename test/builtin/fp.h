/* score-fp-bit.h for Sunplus S+CORE processor
   Copyright (C) 2005 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#ifndef FP_BIT_H_INCLUDED
#define FP_BIT_H_INCLUDED

/*--------------------------------------------------------
  These C source files are porting from gcc fp-bit.c
  by shengguo(mail: shengguo@sunnorth.com.cn).
  They are rewritted for better performance on S+core.
  Rounding mode is round_nearest_even.
  --------------------------------------------------------*/

/* The typedefs for S+core.  */
typedef signed char INT8;
typedef signed short INT16;
typedef signed int INT32;
typedef signed long long INT64;
typedef signed long long INT128;

typedef unsigned char UINT8;
typedef unsigned short UINT16;
typedef unsigned int UINT32;
typedef unsigned long long UINT64;
typedef unsigned long long UINT128;

/*--------------------------------------------------------
  float(SFmode) 32bits:
   0-22 bit : fraction part
  23-30 bit : exponent part
     31 bit : sign bit
  --------------------------------------------------------
  double(DFmode) 64bits:
   0-51 bit : fraction part
  52-62 bit : exponent part
     63 bit : sign bit
  --------------------------------------------------------
  float/double value kinds
  inf      : exponent is 0xff/0x7ff, fraction is 0.
  nan      : exponent is 0xff/0x7ff, fraction is not 0.
  zero     : exponent is 0, fraction is 0.
  denormal : exponent is 0, fraction is not 0.
  normal   : except above kinds.
  --------------------------------------------------------*/

#if defined(FLOAT)
#       define NGARDS       7L
#       define GARDROUND    0x3f
#       define GARDMASK     0x7fUL
#       define GARDMSB      0x40
#       define EXPBITS      8
#       define EXPBIAS      127
#       define FRACBITS     23
#       define EXPMAX       (0xff)
#       define FRAC_NBITS   32
#       define FRACHIGH     0x80000000UL
#       define FRACHIGH2    0xc0000000UL
#       define NORMAL_EXPMIN (-(EXPBIAS)+1)
#       define IMPLICIT_1 (1UL<<(FRACBITS+NGARDS))
#       define IMPLICIT_2 (1UL<<(FRACBITS+1+NGARDS))
#       define SHIFT_RIGHT_JAMMING(z, a, cnt)  \
  {  \
    if (a << ((-(cnt)) & 31))  \
      {  \
        z = (a >> (cnt)) | 1;  \
      }  \
    else  \
      {  \
        z = a >> (cnt);  \
      }  \
  }
#else
#       define NGARDS       8L
#       define GARDROUND    0x7f
#       define GARDMASK     0xffULL
#       define GARDMSB      0x80
#       define EXPBITS      11
#       define EXPBIAS      1023
#       define FRACBITS     52
#       define EXPMAX       (0x7ff)
#       define FRAC_NBITS   64
#       define FRACHIGH     0x8000000000000000ULL
#       define FRACHIGH2    0xc000000000000000ULL
#       define NORMAL_EXPMIN (-(EXPBIAS)+1)
#       define IMPLICIT_1 (1ULL<<(FRACBITS+NGARDS))
#       define IMPLICIT_2 (1ULL<<(FRACBITS+1+NGARDS))
#       define SHIFT_RIGHT_JAMMING(z, a, cnt)  \
  {  \
    if (a << ((-(cnt)) & 63))  \
      {  \
        z = (a >> (cnt)) | 1;  \
      }  \
    else  \
      {  \
        z = a >> (cnt);  \
      }  \
  }
#endif

#define SF_QUIET_NAN    0x100000L
#define SF_DEFAULT_NAN  (0x7f800000UL | SF_QUIET_NAN)
#define DF_QUIET_NAN    0x8000000000000LL
#define DF_DEFAULT_NAN  (0x7ff0000000000000ULL | DF_QUIET_NAN)

/* F_D_BITOFF is the number of bits offset between the MSB of the mantissa
   of a float and of a double. Assumes there are only two float types.
   (double::FRAC_BITS+double::NGARDS-(float::FRAC_BITS-float::NGARDS))
 */
#define F_D_BITOFF (52+8-(23+7))

#define ROUND_NEAREST_EVEN(fraction, exp, sign) \
    if (exp > EXPBIAS)  \
      {  \
        exp = EXPMAX;  \
        fraction = 0;  \
      }  \
    else if (exp < NORMAL_EXPMIN)  \
      {  \
        register INT32 shift;  \
        shift = NORMAL_EXPMIN - exp;  \
        exp = 0;  \
        if (shift > FRAC_NBITS - NGARDS)  \
          {  \
            fraction = 0;  \
          }  \
        else  \
          {  \
            SHIFT_RIGHT_JAMMING (fraction, fraction, shift);  \
            if ((fraction & GARDMASK) == GARDMSB)  \
              {  \
                if ((fraction & (1 << NGARDS)))  \
                fraction += GARDROUND + 1;  \
              }  \
            else  \
              {  \
                fraction += GARDROUND;  \
              }  \
            if (fraction >= IMPLICIT_1)  \
              {  \
                exp += 1;  \
              }  \
            fraction >>= NGARDS;  \
          }  \
      }  \
    else   \
      {  \
        exp += EXPBIAS;  \
        if ((fraction & GARDMASK) == GARDMSB)  \
          {  \
            if (fraction & (1 << NGARDS))  \
              {  \
                fraction += GARDROUND + 1;  \
              }  \
          }  \
        else  \
          {  \
            fraction += GARDROUND;  \
          }  \
        if (fraction >= IMPLICIT_2)  \
          {  \
            fraction >>= 1;  \
            exp += 1;  \
          }  \
        fraction >>= NGARDS;  \
      }

/* function declarations.  */
/*
Calculating the negative number of single precision floating number.

'a': 32bit packing form of floating number.
*/
extern UINT32 __negsf2(UINT32 a);

/*
Calculating the negative number of double precision floating number.

'a': 64bit packing form of floating number.
*/
extern UINT64 __negdf2(UINT64 a);

/*
Single precision floating point addition.

'a', 'b': 32bit packing form of floating number.
*/
extern UINT32 __addsf3(UINT32 a, UINT32 b);

/*
Single precision floating point subtraction.

'a', 'b': 32bit packing form of floating number.
*/
extern UINT32 __subsf3(UINT32 a, UINT32 b);

/*
Double precision floating point addition.

'a', 'b': 64bit packing form of floating number.
*/
extern UINT64 __adddf3(UINT64 a, UINT64 b);

/*
Double precision floating point subtraction.

'a', 'b': 64bit packing form of floating number.
*/
extern UINT64 __subdf3(UINT64 a, UINT64 b);

/*
Single precision floating point multiplication.

'a', 'b': 32bit packing form of floating number.
*/
extern UINT32 __mulsf3(UINT32 a, UINT32 b);

/*
Double precision floating point multiplication.

'a', 'b': 64bit packing form of floating number.
*/
extern UINT64 __muldf3(UINT64 a, UINT64 b);

/*
Single precision division.

'a', 'b': 32bit packing form of floating number.
*/
extern UINT32 __divsf3(UINT32 a, UINT32 b);

/*
Single precision division.

'a', 'b': 32bit packing form of floating number.
*/
extern UINT64 __divdf3(UINT64 a, UINT64 b);

/*
Extending floating point from single to double precision.

'a': 32bit packing form of floating number.
*/
extern UINT64 __extendsfdf2(UINT32 a);

/*
Truncation floating point from double to single precision.

'a': 64bit packing form of floating number.
*/
extern UINT32 __truncdfsf2(UINT64 a);

/*
Convert double to signed int, rounding toward zero.

'a': 64bit packing form of floating number.
*/
extern UINT32 __fixdfsi(UINT64 a);

/*
Convert float to signed int, rounding toward zero.

'a': 32bit packing form of floating number.
*/
extern UINT32 __fixsfsi(UINT32 a);

/*
Convert signed int to double.

'a': 32bit integer number.
*/
extern UINT64 __floatsidf(UINT32 a);

/*
Convert signed int to float.

'a': 32bit integer number.
*/
extern UINT32 __floatsisf(UINT32 a);

/*
Convert unsigned int to double.

'a': 32bit integer number.
*/
extern UINT64 __floatunsidf(UINT32 a);

/*
Convert unsigned int to float.

'a': 32bit integer number.
*/
extern UINT32 __floatunsisf(UINT32 a);

/*
These functions return a value less than zero if neither
argument is NaN, and a is strictly less than b.
*/
extern INT32 __ltsf2(UINT32 a, UINT32 b);
extern INT32 __ltdf2(UINT64 a, UINT64 b);

/*
These functions return a value less than or equal to zero
if neither argument is NaN, and a is less than or equal to b.
*/
extern INT32 __lesf2(UINT32 a, UINT32 b);
extern INT32 __ledf2(UINT64 a, UINT64 b);

/*
These functions return a nonzero value if either argument is NaN, or if
a and b are unequal.
*/
extern INT32 __nesf2(UINT32 a, UINT32 b);
extern INT32 __nedf2(UINT64 a, UINT64 b);

/*
These functions return a value greater than zero if neither argument
is NaN, and a is strictly greater than b.
*/
extern INT32 __gtsf2(UINT32 a, UINT32 b);
extern INT32 __gtdf2(UINT64 a, UINT64 b);

/*
These functions return a value greater than or equal to zero if
neither argument is NaN, and a is greater than or equal to b.
*/
extern INT32 __gesf2(UINT32 a, UINT32 b);
extern INT32 __gedf2(UINT64 a, UINT64 b);

/*
These functions return a nonzero value if either a or b is NaN, otherwise 0.
*/
extern INT32 __unorddf2(UINT64 a, UINT64 b);
extern INT32 __unordsf2(UINT32 a, UINT32 b);

/*
These functions return zero if neither argument is NaN, and a and b are equal.
*/
extern INT32 __eqdf2(UINT64 a, UINT64 b);
extern INT32 __eqsf2(UINT32 a, UINT32 b);

/* From libgcc2.c. (-Shengguo) */
typedef signed int SItype;
typedef unsigned int USItype;
typedef signed long long DItype;
typedef unsigned long long UDItype;
typedef float SFtype;
typedef double DFtype;

/*
These functions convert 'a' to an unsigned int, rounding toward zero.
*/
extern USItype __fixunssfsi (SFtype a);
extern USItype __fixunsdfsi (DFtype a);

/*
These functions convert u, a signed long long, to floating point.
*/
extern SFtype __floatdisf (DItype u);
extern DFtype __floatdidf (DItype u);

/*
These functions convert u, an unsigned long, to floating point.
*/
extern SFtype __floatundisf (UDItype u);
extern DFtype __floatundidf (UDItype u);

/*
These functions convert 'a' to a signed long, rounding toward zero.
*/
extern DItype __fixsfdi (SFtype a);
extern DItype __fixdfdi (DFtype a);

/*
These functions convert 'a' to an unsigned long, rounding toward zero.
Negative values all become zero.
*/
extern DItype __fixunssfdi (SFtype a);
extern DItype __fixunsdfdi (DFtype a);

//Unsupported functions
extern INT32 __cmpdf2(UINT64 a, UINT64 b);
extern INT32 __cmpsf2(UINT32 a, UINT32 b);
extern INT32 __compare_df2 (UINT64 a, UINT64 b, INT32 retval_for_nan);
extern INT32 __compare_sf2 (UINT32 a, UINT32 b, INT32 retval_for_nan);
#endif /* FP_BIT_H_INCLUDED */
