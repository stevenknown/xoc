/* score-fp-bit.c for Sunplus S+CORE processor
   Copyright (C) 2005 Free Software Foundation, Inc.
   Contributed by Sunnorth.

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

#include "fp.h"

extern unsigned int clz32 (unsigned int);

#define __builtin_clz(x) clz32(x)

#if defined(L_addsub_sf)
UINT32
__subsf3 (UINT32 a, UINT32 b)
{
    b ^= (1UL << (FRACBITS + EXPBITS));
    return __addsf3(a, b);
}

UINT32
__addsf3 (UINT32 a, UINT32 b)
{
    register UINT32 frac_a, frac_b;
    register INT32  exp_a, exp_b;
    register UINT32 sign_a, sign_b;
    register INT32  expDiff;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1UL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    frac_b = b & ((1UL << FRACBITS) - 1);
    exp_b = (b >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_b = b >> (FRACBITS + EXPBITS);

    /* STEP 2: Shift fraction to make exp_a == exp_b.
               After this step, exp_b has no use any more,
               exp_a saves the exponent of final result.  */
    if (exp_a > exp_b)
      {
        if (exp_a == EXPMAX)
          {
            if (frac_a != 0)
              {
                return (a | SF_QUIET_NAN);
              }
            return a;
          }

        expDiff = exp_a - exp_b;
        if (expDiff >= (FRACBITS + 3))
          {
            return a;
          }

        if (exp_b == 0)
          {
            frac_b <<= (NGARDS + 1);
          }
        else
          {
            frac_b = (frac_b | (1UL << FRACBITS)) << NGARDS;
          }
        frac_a = (frac_a | (1UL << FRACBITS)) << NGARDS;
        SHIFT_RIGHT_JAMMING (frac_b, frac_b, expDiff);
      }
    else if (exp_a < exp_b)
      {
        if (exp_b == EXPMAX)
          {
            if (frac_b != 0)
              {
                return (b | SF_QUIET_NAN);
              }
            return b;
          }

        expDiff = exp_b - exp_a;
        if (expDiff >= (FRACBITS + 3))
          {
            return b;
          }

        if (exp_a == 0)
          {
            frac_a <<= (NGARDS + 1);
          }
        else
          {
            frac_a = (frac_a | (1UL << FRACBITS)) << NGARDS;
          }
        frac_b = (frac_b | (1UL << FRACBITS)) << NGARDS;
        SHIFT_RIGHT_JAMMING (frac_a, frac_a, expDiff);
        exp_a = exp_b;
      }
    else
      {
        if (exp_a == EXPMAX)
          {
            if (frac_a != 0)
              {
                return (a | SF_QUIET_NAN);
              }
            if (frac_b != 0)
              {
                return (b | SF_QUIET_NAN);
              }
            if (sign_a != sign_b)
              {
                return SF_DEFAULT_NAN;
              }
            return a;
          }
        if (exp_a == 0)
          {
            if (sign_a == sign_b)
              {
                return ((sign_a << (FRACBITS + EXPBITS)) | (frac_a + frac_b));
              }
            if (frac_a > frac_b)
              {
                return ((sign_a << (FRACBITS + EXPBITS)) | (frac_a - frac_b));
              }
            else if (frac_a < frac_b)
              {
                return ((sign_b << (FRACBITS + EXPBITS)) | (frac_b - frac_a));
              }
            return 0;
          }
        else
          {
            frac_a = (frac_a | (1UL << FRACBITS)) << NGARDS;
            frac_b = (frac_b | (1UL << FRACBITS)) << NGARDS;
          }
      }
    exp_a -= EXPBIAS;

    /* STEP 3: Do fraction part add/sub.
               frac_a saves the add/sub result.
               exp_a saves the exponent of result.
               sign_a saves the sign bit of result.  */
    if (sign_a == sign_b)
      {
        /* Here a, b have same sign, so we do add.  */
        frac_a = frac_a + frac_b;

        if (frac_a >= IMPLICIT_2)
          {
            frac_a >>= 1;
            exp_a++;
          }
      }
    else
      {
        if (frac_a > frac_b)
          {
            frac_a = frac_a - frac_b;
          }
        else if (frac_a < frac_b)
          {
            sign_a = sign_b;
            frac_a = frac_b - frac_a;
          }
        else
          {
            return 0;
          }
        while (frac_a < IMPLICIT_1)
          {
            frac_a <<= 1;
            exp_a--;
          }
      }

    /* STEP 4: Round the result. Rounding mode is round_nearest_even.  */
    ROUND_NEAREST_EVEN (frac_a, exp_a, sign_a);

    /* STEP 5: Pack the result and return.  */
    return ((sign_a << (FRACBITS + EXPBITS)) | (exp_a << FRACBITS) | (frac_a & ((1UL << FRACBITS) - 1)));
}
#endif

#if defined(L_addsub_df)
UINT64
__subdf3 (UINT64 a, UINT64 b)
{
    b ^= (1ULL << (FRACBITS + EXPBITS));
    return __adddf3(a, b);
}

UINT64
__adddf3 (UINT64 a, UINT64 b)
{
    register UINT64 frac_a, frac_b;
    register INT32  exp_a, exp_b;
    register UINT32 sign_a, sign_b;
    register INT32  expDiff;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1ULL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    frac_b = b & ((1ULL << FRACBITS) - 1);
    exp_b = (b >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_b = b >> (FRACBITS + EXPBITS);

    /* STEP 2: Shift fraction to make exp_a == exp_b.
               After this step, exp_b has no use any more,
               exp_a saves the exponent of final result.  */
    if (exp_a > exp_b)
      {
        if (exp_a == EXPMAX)
          {
            if (frac_a != 0)
              {
                return (a | DF_QUIET_NAN);
              }
            return a;
          }

        expDiff = exp_a - exp_b;
        if (expDiff >= (FRACBITS + 3))
          {
            return a;
          }

        if (exp_b == 0)
          {
            frac_b <<= (NGARDS + 1);
          }
        else
          {
            frac_b = (frac_b | (1ULL << FRACBITS)) << NGARDS;
          }
        frac_a = (frac_a | (1ULL << FRACBITS)) << NGARDS;
        SHIFT_RIGHT_JAMMING (frac_b, frac_b, expDiff);
      }
    else if (exp_a < exp_b)
      {
        if (exp_b == EXPMAX)
          {
            if (frac_b != 0)
              {
                return (b | DF_QUIET_NAN);
              }
            return b;
          }

        expDiff = exp_b - exp_a;
        if (expDiff >= (FRACBITS + 3))
          {
            return b;
          }

        if (exp_a == 0)
          {
            frac_a <<= (NGARDS + 1);
          }
        else
          {
            frac_a = (frac_a | (1ULL << FRACBITS)) << NGARDS;
          }
        frac_b = (frac_b | (1ULL << FRACBITS)) << NGARDS;
        SHIFT_RIGHT_JAMMING (frac_a, frac_a, expDiff);
        exp_a = exp_b;
      }
    else
      {
        if (exp_a == EXPMAX)
          {
            if (frac_a != 0)
              {
                return (a | DF_QUIET_NAN);
              }
            if (frac_b != 0)
              {
                return (b | DF_QUIET_NAN);
              }
            if (sign_a != sign_b)
              {
                return DF_DEFAULT_NAN;
              }
            return a;
          }
        if (exp_a == 0)
          {
            if (sign_a == sign_b)
              {
                return (((UINT64)sign_a << (FRACBITS + EXPBITS)) | (frac_a + frac_b));
              }
            if (frac_a > frac_b)
              {
                return (((UINT64)sign_a << (FRACBITS + EXPBITS)) | (frac_a - frac_b));
              }
            else if (frac_a < frac_b)
              {
                return (((UINT64)sign_b << (FRACBITS + EXPBITS)) | (frac_b - frac_a));
              }
            return 0;
          }
        else
          {
            frac_a = (frac_a | (1ULL << FRACBITS)) << NGARDS;
            frac_b = (frac_b | (1ULL << FRACBITS)) << NGARDS;
          }
      }
    exp_a -= EXPBIAS;

    /* STEP 3: Do fraction part add/sub.
               frac_a saves the add/sub result.
               exp_a saves the exponent of result.
               sign_a saves the sign bit of result.  */
    if (sign_a == sign_b)
      {
        /* Here a, b have same sign, so we do add.  */
        frac_a = frac_a + frac_b;

        if (frac_a >= IMPLICIT_2)
          {
            frac_a >>= 1;
            exp_a++;
          }
      }
    else
      {
        if (frac_a > frac_b)
          {
            frac_a = frac_a - frac_b;
          }
        else if (frac_a < frac_b)
          {
            sign_a = sign_b;
            frac_a = frac_b - frac_a;
          }
        else
          {
            return 0;
          }
        while (frac_a < IMPLICIT_1)
          {
            frac_a <<= 1;
            exp_a--;
          }
      }

    /* STEP 4: Round the result. Rounding mode is round_nearest_even.  */
    ROUND_NEAREST_EVEN (frac_a, exp_a, sign_a);

    /* STEP 5: Pack the result and return.  */
    return (((UINT64)sign_a << (FRACBITS + EXPBITS)) | ((UINT64)(UINT32)exp_a << FRACBITS) | (frac_a & ((1ULL << FRACBITS) - 1)));
}
#endif

#if defined(L_mul_sf)
UINT32
__mulsf3 (UINT32 a, UINT32 b)
{
    register UINT32 frac_a, frac_b;
    register INT32  exp_a, exp_b;
    register UINT32 sign_a, sign_b;
    register UINT32 low;
    register UINT32 shift;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1UL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    frac_b = b & ((1UL << FRACBITS) - 1);
    exp_b = (b >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_b = b >> (FRACBITS + EXPBITS);

    /* STEP 2: Handle NaN, INF.  */
    if (exp_a == EXPMAX)
      {
        if (frac_a != 0)
          {
            return ((a ^ (sign_b<<(FRACBITS + EXPBITS))) | SF_QUIET_NAN);
          }
        if (exp_b == EXPMAX && frac_b != 0)
          {
            return ((b ^ (sign_a<<(FRACBITS + EXPBITS))) | SF_QUIET_NAN);
          }
        if (exp_b == 0 && frac_b == 0)
          {
            return SF_DEFAULT_NAN;
          }
        return (a ^ (sign_b<<(FRACBITS + EXPBITS)));
      }
    if (exp_b == EXPMAX)
      {
        if (frac_b != 0)
          {
            return ((b ^ (sign_a<<(FRACBITS + EXPBITS))) | SF_QUIET_NAN);
          }
        if (exp_a == 0 && frac_a == 0)
          {
            return SF_DEFAULT_NAN;
          }
        return (b ^ (sign_a<<(FRACBITS + EXPBITS)));
      }

    /* STEP 3: Handle 0, denormal.  */
    if (exp_a == 0)
      {
        if (frac_a == 0)
          {
            return (a ^ (sign_b<<(FRACBITS + EXPBITS)));
          }

        shift = __builtin_clz (frac_a) + FRACBITS - 31;
        frac_a <<= shift;
        exp_a = exp_a + 1 - shift;
      }
    if (exp_b == 0)
      {
        if (frac_b == 0)
          {
            return (b ^ (sign_a<<(FRACBITS + EXPBITS)));
          }

        shift = __builtin_clz (frac_b) + FRACBITS - 31;
        frac_b <<= shift;
        exp_b = exp_b + 1 - shift;
      }

    /* STEP 4: Do fraction part mul.
               frac_a saves the mul result.
               exp_a saves the exponent of result.
               sign_a saves the sign bit of result.  */
    frac_a = (frac_a | (1UL << FRACBITS)) << 8;
    frac_b = (frac_b | (1UL << FRACBITS)) << 8;

    exp_a = exp_a - EXPBIAS + exp_b - EXPBIAS + FRAC_NBITS - (FRACBITS + 16 - NGARDS);
    sign_a = sign_a ^ sign_b;
    {
        register UINT64 answer = (UINT64)frac_a * (UINT64)frac_b;
        frac_a = answer >> 32;
        low = (UINT32)answer;
    }

#if (NGARDS >= 8)
#error "NGARDS must be less than 8"
#endif
    while (frac_a >= IMPLICIT_2)
      {
        exp_a++;
        low = low | (frac_a & 1);
        frac_a >>= 1;
      }
    if (((frac_a & (GARDMASK | (1 << NGARDS))) == GARDMSB) && low)
      {
        frac_a += GARDROUND + 1;
      }

    /* STEP 5: Round the result. Rounding mode is round_nearest_even.  */
    ROUND_NEAREST_EVEN (frac_a, exp_a, sign_a);

    /* STEP 6: Pack the result and return.  */
    return ((sign_a << (FRACBITS + EXPBITS)) | (exp_a << FRACBITS) | (frac_a & ((1UL << FRACBITS) - 1)));
}
#endif

#if defined(L_mul_df)
UINT64
__muldf3 (UINT64 a, UINT64 b)
{
    register UINT64 frac_a, frac_b;
    register INT32  exp_a, exp_b;
    register UINT32 sign_a, sign_b;
    register UINT64 high, low;
    register UINT32 shift;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1ULL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    frac_b = b & ((1ULL << FRACBITS) - 1);
    exp_b = (b >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_b = b >> (FRACBITS + EXPBITS);

    /* STEP 2: Handle NaN, INF.  */
    if (exp_a == EXPMAX)
      {
        if (frac_a != 0)
          {
            return ((a ^ ((UINT64)sign_b<<(FRACBITS + EXPBITS))) | DF_QUIET_NAN);
          }
        if (exp_b == EXPMAX && frac_b != 0)
          {
            return ((b ^ ((UINT64)sign_a<<(FRACBITS + EXPBITS))) | DF_QUIET_NAN);
          }
        if (exp_b == 0 && frac_b == 0)
          {
            return DF_DEFAULT_NAN;
          }
        return (a ^ ((UINT64)sign_b<<(FRACBITS + EXPBITS)));
      }
    if (exp_b == EXPMAX)
      {
        if (frac_b != 0)
          {
            return ((b ^ ((UINT64)sign_a<<(FRACBITS + EXPBITS))) | DF_QUIET_NAN);
          }
        if (exp_a == 0 && frac_a == 0)
          {
            return DF_DEFAULT_NAN;
          }
        return (b ^ ((UINT64)sign_a<<(FRACBITS + EXPBITS)));
      }

    /* STEP 3: Handle 0, denormal.  */
    if (exp_a == 0)
      {
        if (frac_a == 0)
          {
            return (a ^ ((UINT64)sign_b<<(FRACBITS + EXPBITS)));
          }

        if ((UINT32)(frac_a >> 32) == 0)
          {
            shift = 32 + __builtin_clz (frac_a) + FRACBITS - 63;
          }
        else
          {
            shift = __builtin_clz (frac_a >> 32) + FRACBITS - 63;
          }
        frac_a <<= shift;
        exp_a = exp_a + 1 - shift;
      }
    if (exp_b == 0)
      {
        if (frac_b == 0)
          {
            return (b ^ ((UINT64)sign_a<<(FRACBITS + EXPBITS)));
          }

        if ((UINT32)(frac_b >> 32) == 0)
          {
            shift = 32 + __builtin_clz (frac_b) + FRACBITS - 63;
          }
        else
          {
            shift = __builtin_clz (frac_b >> 32) + FRACBITS - 63;
          }
        frac_b <<= shift;
        exp_b = exp_b + 1 - shift;
      }

    /* STEP 4: Do fraction part mul.
               frac_a saves the mul result.
               exp_a saves the exponent of result.
               sign_a saves the sign bit of result.  */
    frac_a = (frac_a | (1ULL << FRACBITS)) << 10;
    frac_b = (frac_b | (1ULL << FRACBITS)) << 10;

    exp_a = exp_a - EXPBIAS + exp_b - EXPBIAS + FRAC_NBITS - (FRACBITS + 20 - NGARDS);
    sign_a = sign_a ^ sign_b;
    {
        register UINT64 ps_hh__;
        register UINT32 nl = frac_a;
        register UINT32 nh = frac_a >> 32;
        register UINT32 ml = frac_b;
        register UINT32 mh = frac_b >> 32;
        register UINT64 pp_ll = (UINT64)ml * (UINT64)nl;
        register UINT64 pp_hl = (UINT64)mh * (UINT64)nl;
        register UINT64 pp_lh = (UINT64)ml * (UINT64)nh;
        register UINT64 pp_hh = (UINT64)mh * (UINT64)nh;

        ps_hh__ = pp_hl + pp_lh;
        high = (ps_hh__ >> 32) + pp_hh;
        if (ps_hh__ < pp_hl)
          high += 1ULL << 32;
        pp_hl = ((UINT64)(UINT32)ps_hh__) << 32;
        low = pp_ll + pp_hl;
        if (low < pp_ll)
          high++;
    }

#if (NGARDS >= 9)
#error "NGARDS must be less than 9"
#endif
    frac_a = high;
    while (frac_a >= IMPLICIT_2)
      {
        exp_a++;
        low = low | (frac_a & 1);
        frac_a >>= 1;
      }
    if (((frac_a & (GARDMASK | (1 << NGARDS))) == GARDMSB) && low)
      {
        frac_a += GARDROUND + 1;
      }

    /* STEP 5: Round the result. Rounding mode is round_nearest_even.  */
    ROUND_NEAREST_EVEN (frac_a, exp_a, sign_a);

    /* STEP 6: Pack the result and return.  */
    return (((UINT64)sign_a << (FRACBITS + EXPBITS)) | ((UINT64)(UINT32)exp_a << FRACBITS) | (frac_a & ((1ULL << FRACBITS) - 1)));
}
#endif

#if defined(L_div_sf)
UINT32
__divsf3 (UINT32 a, UINT32 b)
{
    register UINT32 frac_a, frac_b;
    register INT32  exp_a, exp_b;
    register UINT32 sign_a, sign_b;
    register UINT32 shift;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1UL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    frac_b = b & ((1UL << FRACBITS) - 1);
    exp_b = (b >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_b = b >> (FRACBITS + EXPBITS);

    /* STEP 2: Handle NaN, INF.  */
    if (exp_a == EXPMAX)
      {
        if (frac_a != 0)
          {
            return (a | SF_QUIET_NAN);
          }
        if (exp_b == EXPMAX)
          {
            if (frac_b != 0)
              {
                return (b | SF_QUIET_NAN);
              }
            else
              {
                return SF_DEFAULT_NAN;
              }
          }
        return (a ^ (sign_b<<(FRACBITS + EXPBITS)));
      }
    if (exp_b == EXPMAX)
      {
        if (frac_b != 0)
          {
            return (b | SF_QUIET_NAN);
          }
        return ((sign_a^sign_b)<<(FRACBITS + EXPBITS));
      }

    /* STEP 3: Handle 0, denormal.  */
    if (exp_a == 0)
      {
        if (frac_a == 0)
          {
            if (exp_b == 0 && frac_b == 0)
              {
                return SF_DEFAULT_NAN;
              }
            return ((sign_a^sign_b)<<(FRACBITS + EXPBITS));
          }

        shift = __builtin_clz (frac_a) + FRACBITS - 31;
        frac_a <<= shift;
        exp_a = exp_a + 1 - shift;
      }
    if (exp_b == 0)
      {
        if (frac_b == 0)
          {
            return (((sign_a^sign_b)<<(FRACBITS + EXPBITS)) | (EXPMAX << FRACBITS));
          }

        shift = __builtin_clz (frac_b) + FRACBITS - 31;
        frac_b <<= shift;
        exp_b = exp_b + 1 - shift;
      }

    /* STEP 4: Now we do fraction part division.
               frac_a saves the div result.
               exp_a saves the exponent of result.
               sign_a saves the sign bit of result.  */
    {
        /* Porting from SoftFloat, get better performance.  */
        register UINT32 b0;
        register UINT32 rem0;
        register UINT64 rem, term;
        register UINT32 quotient;

        frac_a = (frac_a | (1UL << FRACBITS)) << 7;
        frac_b = (frac_b | (1UL << FRACBITS)) << 8;
        exp_a = exp_a - exp_b - 1;
        sign_a = sign_a ^ sign_b;

        if ( frac_b <= (frac_a + frac_a) )
          {
            frac_a >>= 1;
            exp_a++;
          }
        b0 = frac_b >> 16;
        quotient = (frac_a / b0) << 16;
        term = (UINT64)frac_b * (UINT64)quotient;
        rem = ((UINT64)frac_a << 32) - term;
        while ( ((INT64)rem) < 0 )
          {
            quotient -= 0x10000;
            rem = rem + ((UINT64)frac_b << 16);
          }
        rem0 = rem >> 16;
        quotient |= ((b0 << 16) <= rem0) ? 0xFFFF : rem0 / b0;

        if ((quotient & 0x3f) <= 2)
          {
            term = (UINT64)frac_b * (UINT64)quotient;
            rem = ((UINT64)frac_a << 32) - term;
            while ( ((INT64)rem) < 0 )
              {
                quotient--;
                rem = rem + frac_b;
              }
            quotient |= ((UINT32)rem != 0);
          }
#if (NGARDS == 7)
    frac_a = quotient;
#elif (NGARDS < 7)
    SHIFT_RIGHT_JAMMING (frac_a, quotient, (7 - NGARDS));
#elif (NGARDS > 7)
    frac_a = (quotient << (NGARDS - 7));
#else
#error "NGARDS undefined!"
#endif
    }

    /* STEP 5: Round the result. Rounding mode is round_nearest_even.  */
    ROUND_NEAREST_EVEN (frac_a, exp_a, sign_a);

    /* STEP 6: Pack the result and return.  */
    return ((sign_a << (FRACBITS + EXPBITS)) | (exp_a << FRACBITS) | (frac_a & ((1UL << FRACBITS) - 1)));
}
#endif

#if defined(L_div_df)
/* Porting from SoftFloat, get better performance.  */
static inline void
add64(
     UINT32 a0, UINT32 a1, UINT32 b0, UINT32 b1, UINT32 *z0Ptr, UINT32 *z1Ptr )
{
    UINT32 z1;

    z1 = a1 + b1;
    *z1Ptr = z1;
    *z0Ptr = a0 + b0 + ( z1 < a1 );
}

static inline void
sub64(
     UINT32 a0, UINT32 a1, UINT32 b0, UINT32 b1, UINT32 *z0Ptr, UINT32 *z1Ptr )
{

    *z1Ptr = a1 - b1;
    *z0Ptr = a0 - b0 - ( a1 < b1 );
}

static inline void
add96(
     UINT32 a0,
     UINT32 a1,
     UINT32 a2,
     UINT32 b0,
     UINT32 b1,
     UINT32 b2,
     UINT32 *z0Ptr,
     UINT32 *z1Ptr,
     UINT32 *z2Ptr
 )
{
    UINT32 z0, z1, z2;
    UINT32 carry0, carry1;

    z2 = a2 + b2;
    carry1 = ( z2 < a2 );
    z1 = a1 + b1;
    carry0 = ( z1 < a1 );
    z0 = a0 + b0;
    z1 += carry1;
    z0 += ( z1 < carry1 );
    z0 += carry0;
    *z2Ptr = z2;
    *z1Ptr = z1;
    *z0Ptr = z0;
}

static inline void
sub96(
     UINT32 a0,
     UINT32 a1,
     UINT32 a2,
     UINT32 b0,
     UINT32 b1,
     UINT32 b2,
     UINT32 *z0Ptr,
     UINT32 *z1Ptr,
     UINT32 *z2Ptr
 )
{
    UINT32 z0, z1, z2;
    UINT32 borrow0, borrow1;

    z2 = a2 - b2;
    borrow1 = ( a2 < b2 );
    z1 = a1 - b1;
    borrow0 = ( a1 < b1 );
    z0 = a0 - b0;
    z0 -= ( z1 < borrow1 );
    z1 -= borrow1;
    z0 -= borrow0;
    *z2Ptr = z2;
    *z1Ptr = z1;
    *z0Ptr = z0;
}

static inline void
mul32To64 (UINT32 a, UINT32 b, UINT32 *z0Ptr, UINT32 *z1Ptr)
{
    UINT64 temp;
    temp = (UINT64)a * (UINT64)b;
    *z0Ptr = temp >> 32;
    *z1Ptr = temp;
}

static inline void
mul64By32To96(
     UINT32 a0,
     UINT32 a1,
     UINT32 b,
     UINT32 *z0Ptr,
     UINT32 *z1Ptr,
     UINT32 *z2Ptr
 )
{
    UINT32 z0, z1, z2, more1;

    mul32To64 (a1, b, &z1, &z2);
    mul32To64 (a0, b, &z0, &more1);
    add64 (z0, more1, 0, z1, &z0, &z1);
    *z2Ptr = z2;
    *z1Ptr = z1;
    *z0Ptr = z0;
}

static UINT32
estimateDiv64To32 (UINT32 a0, UINT32 a1, UINT32 b)
{
    UINT32 b0, b1;
    UINT32 rem0, rem1, term0, term1;
    UINT32 z;

    if ( b <= a0 ) return 0xFFFFFFFF;
    b0 = b>>16;
    z = ( b0<<16 <= a0 ) ? 0xFFFF0000 : ( a0 / b0 )<<16;
    mul32To64 (b, z, &term0, &term1);
    sub64 (a0, a1, term0, term1, &rem0, &rem1);
    while ( ( (INT32) rem0 ) < 0 ) {
        z -= 0x10000;
        b1 = b<<16;
        add64 (rem0, rem1, b0, b1, &rem0, &rem1);
    }
    rem0 = ( rem0<<16 ) | ( rem1>>16 );
    z |= ( b0<<16 <= rem0 ) ? 0xFFFF : rem0 / b0;
    return z;
}

UINT64
__divdf3 (UINT64 a, UINT64 b)
{
    register UINT64 frac_a, frac_b;
    register INT32  exp_a, exp_b;
    register UINT32 sign_a, sign_b;
    register UINT32 shift;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1ULL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    frac_b = b & ((1ULL << FRACBITS) - 1);
    exp_b = (b >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_b = b >> (FRACBITS + EXPBITS);

    /* STEP 2: Handle NaN, INF.  */
    if (exp_a == EXPMAX)
      {
        if (frac_a != 0)
          {
            return (a | DF_QUIET_NAN);
          }
        if (exp_b == EXPMAX)
          {
            if (frac_b != 0)
              {
                return (b | DF_QUIET_NAN);
              }
            else
              {
                return DF_DEFAULT_NAN;
              }
          }
        return (a ^ ((UINT64)sign_b<<(FRACBITS + EXPBITS)));
      }
    if (exp_b == EXPMAX)
      {
        if (frac_b != 0)
          {
            return (b | DF_QUIET_NAN);
          }
        return ((UINT64)(sign_a^sign_b)<<(FRACBITS + EXPBITS));
      }

    /* STEP 3: Handle 0, denormal.  */
    if (exp_a == 0)
      {
        if (frac_a == 0)
          {
            if (exp_b == 0 && frac_b == 0)
              {
                return DF_DEFAULT_NAN;
              }
            return ((UINT64)(sign_a^sign_b)<<(FRACBITS + EXPBITS));
          }

        if ((UINT32)(frac_a >> 32) == 0)
          {
            shift = 32 + __builtin_clz (frac_a) + FRACBITS - 63;
          }
        else
          {
            shift = __builtin_clz (frac_a >> 32) + FRACBITS - 63;
          }
        frac_a <<= shift;
        exp_a = exp_a + 1 - shift;
      }
    if (exp_b == 0)
      {
        if (frac_b == 0)
          {
            return (((UINT64)(sign_a^sign_b)<<(FRACBITS + EXPBITS)) | ((UINT64)EXPMAX << FRACBITS));
          }

        if ((UINT32)(frac_b >> 32) == 0)
          {
            shift = 32 + __builtin_clz (frac_b) + FRACBITS - 63;
          }
        else
          {
            shift = __builtin_clz (frac_b >> 32) + FRACBITS - 63;
          }
        frac_b <<= shift;
        exp_b = exp_b + 1 - shift;
      }

    /* STEP 4: Now we do fraction part division.
               frac_a saves the div result.
               exp_a saves the exponent of result.
               sign_a saves the sign bit of result.  */
    {
        UINT32 aSig0, aSig1, bSig0, bSig1, zSig0, zSig1;
        UINT32 rem0, rem1, rem2, rem3, term0, term1, term2, term3;

        frac_a = (frac_a | (1ULL << FRACBITS)) << 11;
        frac_b = (frac_b | (1ULL << FRACBITS)) << 11;
        exp_a = exp_a - exp_b - 1;
        sign_a = sign_a ^ sign_b;

        if (frac_b < frac_a)
          {
            frac_a >>= 1;
            exp_a++;
          }
        aSig0 = frac_a >> 32;
        aSig1 = frac_a;
        bSig0 = frac_b >> 32;
        bSig1 = frac_b;

        zSig0 = estimateDiv64To32 (aSig0, aSig1, bSig0);
        mul64By32To96 (bSig0, bSig1, zSig0, &term0, &term1, &term2);
        sub96 (aSig0, aSig1, 0, term0, term1, term2, &rem0, &rem1, &rem2);
        while ( (INT32) rem0 < 0 ) {
            --zSig0;
            add96 (rem0, rem1, rem2, 0, bSig0, bSig1, &rem0, &rem1, &rem2);
        }
        zSig1 = estimateDiv64To32 (rem1, rem2, bSig0);
        if ( ( zSig1 & 0x3FF ) <= 4 ) {
            mul64By32To96 (bSig0, bSig1, zSig1, &term1, &term2, &term3);
            sub96 (rem1, rem2, 0, term1, term2, term3, &rem1, &rem2, &rem3);
            while ( (INT32) rem1 < 0 ) {
                --zSig1;
                add96 (rem1, rem2, rem3, 0, bSig0, bSig1, &rem1, &rem2, &rem3);
            }
            zSig1 |= ( ( rem1 | rem2 | rem3 ) != 0 );
        }
        frac_a = (((UINT64)zSig0 << 32) | zSig1);
        SHIFT_RIGHT_JAMMING (frac_a, frac_a, 11 - NGARDS);
    }

    /* STEP 5: Round the result. Rounding mode is round_nearest_even.  */
    ROUND_NEAREST_EVEN (frac_a, exp_a, sign_a);

    /* STEP 6: Pack the result and return.  */
    return (((UINT64)sign_a << (FRACBITS + EXPBITS)) | ((UINT64)(UINT32)exp_a << FRACBITS) | (frac_a & ((1ULL << FRACBITS) - 1)));
}
#endif

#if defined(L_negate_sf)
UINT32
__negsf2 (UINT32 a)
{
    a ^= (1UL << 31);

    if ((a & 0x7fffffffUL) <= 0x7f800000UL)
      {
        /* Here a is not NAN.  */
        return a;
      }

    /* Here a is NAN.  */
    return (a | SF_QUIET_NAN);
}
#endif

#if defined(L_negate_df)
UINT64
__negdf2 (UINT64 a)
{
    a ^= (1ULL << 63);
    if ((a & 0x7fffffffffffffffULL) <= 0x7ff0000000000000ULL)
      {
        /* Here a is not NAN.  */
        return a;
      }

    /* Here a is NAN.  */
    return (a | DF_QUIET_NAN);
}
#endif

#if defined(L_si_to_sf)
UINT32
__floatsisf (UINT32 a)
{
    register UINT32 frac_a;
    register INT32  exp_a;
    register UINT32 sign_a;
    register UINT32 ld_zero;

    /* STEP 1: Handle special cases.  */
    if (a == 0)
      {
        return 0;
      }
    if (a == 0x80000000UL)
      {
        return (0xcf000000UL);
      }

    /* STEP 2: Turn 32-bit integer into float.  */
    sign_a = a & 0x80000000UL;
    if (sign_a)
      {
        frac_a = -a;
      }
    else
      {
        frac_a = a;
      }

    ld_zero = __builtin_clz (frac_a);
    exp_a = EXPBIAS + 30 - ld_zero;
    if (ld_zero >= 8)
      {
        frac_a <<= (ld_zero - 8);
      }
    else
      {
        frac_a <<= (ld_zero - 1);

        /* Round the result. Rounding mode is round_nearest_even.  */
        if ((frac_a & 0xffUL) == 0xc0)
          {
            frac_a += 0x40;
          }
        else
          {
            frac_a += 0x3f;
          }
        if (frac_a >= (1UL << 31))
          {
            frac_a >>= 1;
            exp_a += 1;
          }
        frac_a >>= 7;
      }

    /* STEP 3: Pack the result and return.  */
    return (sign_a | ((exp_a << 23) + frac_a));
}
#endif

#if defined(L_si_to_df)
UINT64
__floatsidf (UINT32 a)
{
    register UINT64 frac_a;
    register INT32  exp_a;
    register UINT32 sign_a;
    register UINT32 ld_zero;

    /* STEP 1: Handle special cases.  */
    if (a == 0)
      {
        return 0;
      }
    if (a == 0x80000000UL)
      {
        return (0xc1e0000000000000ULL);
      }

    /* STEP 2: Turn 32-bit integer into double.  */
    sign_a = a & 0x80000000UL;
    if (sign_a)
      {
        a = -a;
      }
    ld_zero = __builtin_clz (a);
    exp_a = EXPBIAS + 30 - ld_zero;
    frac_a = (UINT64)a << (FRACBITS - 31 + ld_zero);

    /* STEP 3: Pack the result and return.  */
    return (((UINT64)sign_a << 32) | (((UINT64)(UINT32)exp_a << FRACBITS) + frac_a));
}
#endif

#if defined(L_sf_to_si)
UINT32
__fixsfsi (UINT32 a)
{
    register UINT32 frac_a;
    register INT32  exp_a;
    register UINT32 sign_a;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1UL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    /* STEP 2: return integer result. rounding toward zero.  */
    if (exp_a > (30 + EXPBIAS))
      {
        if (exp_a == EXPMAX && frac_a != 0)
          {
            return 0;
          }
        return (sign_a + 0x7fffffffUL);
      }

    if (exp_a < (0 + EXPBIAS))
      {
        return 0;
      }

    frac_a = (frac_a | (1UL << FRACBITS)) << NGARDS;
    frac_a = frac_a >> (FRACBITS + NGARDS + EXPBIAS - exp_a);
    if (sign_a != 0)
      {
        frac_a = -frac_a;
      }
    return frac_a;
}
#endif

#if defined(L_df_to_si)
UINT32
__fixdfsi (UINT64 a)
{
    register UINT64 frac_a;
    register INT32  exp_a;
    register UINT32 sign_a;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1ULL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    /* STEP 2: return integer result. rounding toward zero.  */
    if (exp_a > (30 + EXPBIAS))
      {
        if (exp_a == EXPMAX && frac_a != 0)
          {
            return 0;
          }
        return (sign_a + 0x7fffffffUL);
      }

    if (exp_a < (0 + EXPBIAS))
      {
        return 0;
      }

    frac_a = (frac_a | (1ULL << FRACBITS)) << NGARDS;
    frac_a = frac_a >> (FRACBITS + NGARDS + EXPBIAS - exp_a);
    if (sign_a != 0)
      {
        frac_a = -frac_a;
      }
    return frac_a;
}
#endif

#if defined(L_sf_to_df)
UINT64
__extendsfdf2 (UINT32 a)
{
    register UINT32 frac_a;
    register INT32  exp_a;
    register UINT32 sign_a;
    register UINT32 shift;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1UL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    /* STEP 2: Handle NaN, INF.  */
    if (exp_a == EXPMAX)
      {
        if (frac_a != 0)
          {
            return (((UINT64)sign_a << 63) | (0x7ffULL << 52) | ((UINT64)frac_a << F_D_BITOFF) | DF_QUIET_NAN);
          }
        return (((UINT64)sign_a << 63) | ((UINT64)0x7ff << 52));
      }

    /* STEP 3: Handle 0, denormal.  */
    if (exp_a == 0)
      {
        if (frac_a == 0)
          {
            return ((UINT64)sign_a << (52 + 11));
          }

        shift = __builtin_clz (frac_a) + FRACBITS - 31;
        frac_a <<= shift;
        exp_a = exp_a + 1 - shift;
        frac_a ^= (1UL << FRACBITS);
      }

    /* STEP 4: pack the result and return.  */
    exp_a += (1023 - EXPBIAS);
    return (((UINT64)sign_a << 63) | ((UINT64)(UINT32)exp_a << 52) | ((UINT64)frac_a << (52 - FRACBITS)));
}
#endif

#if defined(L_df_to_sf)
UINT32
__truncdfsf2 (UINT64 a)
{
    register UINT64 frac_a;
    register INT32  exp_a;
    register UINT32 sign_a;
    register UINT32 frac;

    /* STEP 1: Get fraction, exponent, sign from arguments.  */
    frac_a = a & ((1ULL << FRACBITS) - 1);
    exp_a = (a >> FRACBITS) & ((1UL << EXPBITS) - 1);
    sign_a = a >> (FRACBITS + EXPBITS);

    /* STEP 2: Handle NaN.  */
    if (exp_a == EXPMAX && frac_a != 0)
      {
        SHIFT_RIGHT_JAMMING (frac, frac_a, F_D_BITOFF);
        return ((sign_a << (23 + 8)) | (0xff << 23) | frac | SF_QUIET_NAN);
      }

    /* STEP 3: Handle large/small numbers.  */
    if (exp_a > (127 + EXPBIAS))
      {
        return ((sign_a << (23 + 8)) | (0xff << 23));
      }
    if (exp_a < (-151 + EXPBIAS))
      {
        return (sign_a << (23 + 8));
      }

    /* STEP 4: Do fraction part trunc.
               frac saves the fraction of result.
               exp_a saves the exponent of result.
               sign_a saves the sign bit of result.  */
    frac_a |= (1ULL << FRACBITS);
    if (exp_a < (-126 + EXPBIAS))
      {
        register INT32 shift;
        shift = (FRACBITS - 30) + (-126 + EXPBIAS) - exp_a;
        SHIFT_RIGHT_JAMMING (frac, frac_a, shift);
        exp_a = 0;

        if ((frac & 0xff) == 0xc0)
          frac += 0x40;
        else
          frac += 0x3f;

        if (frac >= (1UL << 30))
          exp_a += 1;
      }
    else
      {
        SHIFT_RIGHT_JAMMING (frac, frac_a, (FRACBITS - 30));
        exp_a = exp_a - EXPBIAS + 127;

        if ((frac & 0xff) == 0xc0)
          frac += 0x40;
        else
          frac += 0x3f;

        if (frac >= (1UL << 31))
          {
            frac >>= 1;
            exp_a += 1;
          }
      }
    frac >>= 7;

    /* STEP 5: pack the result and return.  */
    return ((sign_a << (23 + 8)) | (exp_a << 23) | (frac & ((1UL << 23) - 1)));
}
#endif

#if defined(L_unord_sf)
INT32
__unordsf2 (UINT32 a, UINT32 b)
{
    a <<= 1;
    if (a > 0xff000000UL)
      {
        return 1;
      }
    b <<= 1;
    if (b > 0xff000000UL)
      {
        return 1;
      }
    return 0;
}
#endif

#if defined(L_unord_df)
INT32
__unorddf2 (UINT64 a, UINT64 b)
{
    a <<= 1;
    if (a > 0xffe0000000000000ULL)
      {
        return 1;
      }
    b <<= 1;
    if (b > 0xffe0000000000000ULL)
      {
        return 1;
      }
    return 0;
}
#endif

#if defined(L_fpcmp_parts_sf)
INT32
__compare_sf2 (UINT32 a, UINT32 b, INT32 retval_for_nan)
{
    register UINT32 sign_a, sign_b;

    /* STEP 1: Get sign from arguments.
               Clear sign bit from arguments for later using.  */
    sign_a = a >> 31;
    sign_b = b >> 31;
    a <<= 1;
    b <<= 1;

    /* STEP 2: Handle NaN.  */
    if (a > (0xffUL << 24) || b > (0xffUL << 24))
      return retval_for_nan;

    /* STEP 3: Handle 0.  */
    if (a == 0)
      sign_a = 0;

    if (b == 0)
      sign_b = 0;

    /* STEP 4: Now we compare them.  */
    if (sign_a != sign_b)
      return (sign_b - sign_a);

    if (a > b)
      {
        if (sign_a)
          return -1;
        else
          return 1;
      }
    else if (a < b)
      {
        if (sign_a)
          return 1;
        else
          return -1;
      }

    return 0;
}
#endif

#if defined(L_compare_sf)
INT32
__cmpsf2 (UINT32 a, UINT32 b)
{
    return __compare_sf2 (a, b, 1);
}
#endif

#if defined(L_lt_sf)
INT32
__ltsf2 (UINT32 a, UINT32 b)
{
    //return __compare_sf2 (a, b, 1);
    return __compare_sf2 (a, b, 1) < 0;
}
#endif

#if defined(L_le_sf)
INT32
__lesf2 (UINT32 a, UINT32 b)
{
    //return __compare_sf2 (a, b, 1);
    return __compare_sf2 (a, b, 1) <= 0;
}
#endif

#if defined(L_eq_sf)
INT32
__eqsf2 (UINT32 a, UINT32 b)
{
    //return __compare_sf2 (a, b, 1);
    return __compare_sf2 (a, b, 1) == 0;
}
#endif

#if defined(L_ne_sf)
INT32
__nesf2 (UINT32 a, UINT32 b)
{
    //return __compare_sf2 (a, b, 1);
    return __compare_sf2 (a, b, 1) != 0;
}
#endif

#if defined(L_gt_sf)
INT32
__gtsf2 (UINT32 a, UINT32 b)
{
    //return __compare_sf2 (a, b, -1);
    return __compare_sf2 (a, b, -1) > 0;
}
#endif

#if defined(L_ge_sf)
INT32
__gesf2 (UINT32 a, UINT32 b)
{
    //return __compare_sf2 (a, b, -1);
    return __compare_sf2 (a, b, -1) >= 0;
}
#endif

#if defined(L_fpcmp_parts_df)
INT32
__compare_df2 (UINT64 a, UINT64 b, INT32 retval_for_nan)
{
    register UINT32 sign_a, sign_b;

    /* STEP 1: Get sign from arguments.
               Clear sign bit from arguments for later using.  */
    sign_a = a >> 63;
    sign_b = b >> 63;
    a <<= 1;
    b <<= 1;

    /* STEP 2: Handle NAN.  */
    if (a > (0x7ffULL << 53) || b > (0x7ffULL << 53))
      return retval_for_nan;

    /* STEP 3: Handle Zero.  */
    if (a == 0)
      sign_a = 0;

    if (b == 0)
      sign_b = 0;

    /* STEP 4: Now we compare them.  */
    if (sign_a != sign_b)
      return (sign_b - sign_a);

    if (a > b)
      {
        if (sign_a)
          return -1;
        else
          return 1;
      }
    else if (a < b)
      {
        if (sign_a)
          return 1;
        else
          return -1;
      }

    return 0;
}
#endif

#if defined(L_compare_df)
INT32
__cmpdf2 (UINT64 a, UINT64 b)
{
    return __compare_df2 (a, b, 1);
}
#endif

#if defined(L_lt_df)
INT32
__ltdf2 (UINT64 a, UINT64 b)
{
    //return __compare_df2 (a, b, 1);
    return __compare_df2 (a, b, 1) < 0;
}
#endif

#if defined(L_le_df)
INT32
__ledf2 (UINT64 a, UINT64 b)
{
    //return __compare_df2 (a, b, 1);
    return __compare_df2 (a, b, 1) <= 0;
}
#endif

#if defined(L_eq_df)
INT32
__eqdf2 (UINT64 a, UINT64 b)
{
    //return __compare_df2 (a, b, 1);
    return __compare_df2 (a, b, 1) == 0;
}
#endif

#if defined(L_ne_df)
INT32
__nedf2 (UINT64 a, UINT64 b)
{
    //return __compare_df2 (a, b, 1);
    return __compare_df2 (a, b, 1) != 0;
}
#endif

#if defined(L_gt_df)
INT32
__gtdf2 (UINT64 a, UINT64 b)
{
    //return __compare_df2 (a, b, -1);
    return __compare_df2 (a, b, -1) > 0;
}
#endif

#if defined(L_ge_df)
INT32
__gedf2 (UINT64 a, UINT64 b)
{
    //return __compare_df2 (a, b, -1);
    return __compare_df2 (a, b, -1) >= 0;
}
#endif

#if defined(L_usi_to_sf)
UINT32
__floatunsisf (UINT32 a)
{
    register UINT32 frac_a;
    register INT32  exp_a;
    register UINT32 ld_zero;

    /* STEP 1: Handle special cases.  */
    if (a == 0)
      {
        return 0;
      }

    /* STEP 2: Turn 32-bit unsigned integer into float.  */
    ld_zero = __builtin_clz (a);
    exp_a = EXPBIAS + 30 - ld_zero;
    if (ld_zero >= 8)
      {
        frac_a = a << (ld_zero - 8);
      }
    else
      {
        frac_a = a << ld_zero;
        frac_a = (frac_a & 1) | (frac_a >> 1);

        /* Round the result. Rounding mode is round_nearest_even.  */
        if ((frac_a & 0xffUL) == 0xc0)
          {
            frac_a += 0x40;
          }
        else
          {
            frac_a += 0x3f;
          }
        if (frac_a >= (1UL << 31))
          {
            frac_a >>= 1;
            exp_a += 1;
          }
        frac_a >>= 7;
      }

    /* STEP 3: Pack the result and return.  */
    return ((exp_a << FRACBITS) + frac_a);
}
#endif

#if defined(L_usi_to_df)
UINT64
__floatunsidf (UINT32 a)
{
    register UINT64 frac_a;
    register INT32  exp_a;
    register UINT32 ld_zero;

    /* STEP 1: Handle special cases.  */
    if (a == 0)
      {
        return 0;
      }

    /* STEP 2: Turn 32-bit integer into double.  */
    ld_zero = __builtin_clz (a);
    exp_a = EXPBIAS + 30 - ld_zero;
    frac_a = (UINT64)a << (FRACBITS - 31 + ld_zero);

    /* STEP 3: Pack the result and return.  */
    return (((UINT64)(UINT32)exp_a << FRACBITS) + frac_a);
}
#endif



/* From libgcc2.c. (-Shengguo) */
#if defined(L_fixunssfsi)
USItype
__fixunssfsi (SFtype a)
{
  if (a >= - (SFtype) (- ((SItype)(((USItype)1 << ((4 * 8) - 1)) - 1)) - 1))
    return (SItype) (a + (- ((SItype)(((USItype)1 << ((4 * 8) - 1)) - 1)) - 1)) - (- ((SItype)(((USItype)1 << ((4 * 8) - 1)) - 1)) - 1);
  return (SItype) a;
}
#endif

#if defined(L_fixunsdfsi)
USItype
__fixunsdfsi (DFtype a)
{
  if (a >= - (DFtype) (- ((SItype)(((USItype)1 << ((4 * 8) - 1)) - 1)) - 1))
    return (SItype) (a + (- ((SItype)(((USItype)1 << ((4 * 8) - 1)) - 1)) - 1)) - (- ((SItype)(((USItype)1 << ((4 * 8) - 1)) - 1)) - 1);
  return (SItype) a;
}
#endif

#if defined(L_floatdisf)
SFtype
__floatdisf (DItype u)
{
  DFtype f;
  if (! (- ((DItype) 1 << 53) < u
  && u < ((DItype) 1 << 53)))
    {
      if ((UDItype) u & (((UDItype) 1 << (((4 * 8) * 2) - 53)) - 1))
 {
   u &= ~ (((UDItype) 1 << (((4 * 8) * 2) - 53)) - 1);
   u |= ((UDItype) 1 << (((4 * 8) * 2) - 53));
 }
    }

  f = (SItype) (u >> (4 * 8));
  f *= 0x1p32f;
  f += (USItype)u;
  return (SFtype) f;
}
#endif

#if defined(L_floatundisf)
SFtype
__floatundisf (UDItype u)
{
  DFtype f;
  if (u >= ((UDItype) 1 << 53))
    {
      if ((UDItype) u & (((UDItype) 1 << (((4 * 8) * 2) - 53)) - 1))
 {
   u &= ~ (((UDItype) 1 << (((4 * 8) * 2) - 53)) - 1);
   u |= ((UDItype) 1 << (((4 * 8) * 2) - 53));
 }
    }

  f = (USItype) (u >> (4 * 8));
  f *= 0x1p32f;
  f += (USItype)u;
  return (SFtype) f;
}
#endif

#if defined(L_floatdidf)
DFtype
__floatdidf (DItype u)
{
  DFtype f = (SItype) (u >> (4 * 8));
  f *= 0x1p32f;
  f += (USItype)u;
  return f;
}
#endif

#if defined(L_floatundidf)
DFtype
__floatundidf (UDItype u)
{
  DFtype f = (USItype) (u >> (4 * 8));
  f *= 0x1p32f;
  f += (USItype)u;
  return f;
}
#endif

#if defined(L_fixsfdi)
DItype
__fixsfdi (SFtype a)
{
  if (a < 0)
    return - __fixunssfdi (-a);
  return __fixunssfdi (a);
}
#endif

#if defined(L_fixunssfdi)
DItype
__fixunssfdi (SFtype a)
{
  const DFtype dfa = a;
  const USItype hi = dfa / 0x1p32f;
  const USItype lo = dfa - (DFtype) hi * 0x1p32f;
  return ((UDItype) hi << (4 * 8)) | lo;
}
#endif

#if defined(L_fixdfdi)
DItype
__fixdfdi (DFtype a)
{
  if (a < 0)
    return - __fixunsdfdi (-a);
  return __fixunsdfdi (a);
}
#endif

#if defined(L_fixunsdfdi)
DItype
__fixunsdfdi (DFtype a)
{
  const USItype hi = a / 0x1p32f;
  const USItype lo = a - (DFtype) hi * 0x1p32f;
  return ((UDItype) hi << (4 * 8)) | lo;
}
#endif
