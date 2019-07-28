/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com
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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "xcominc.h"

namespace xcom {

#define REDUCE
#define abs(x) ((x) >= 0 ? (x) : -(x))

BIRational BIRational::rabs()
{
    ASSERTN(m_den != BigInt(1, 0), ("denominator is 0!"));
    BIRational b(*this);
    if (b.m_num < BigInt(1, 0)) {
        b.m_num.neg();
    }
    if (b.m_den < BigInt(1, 0)) {
        b.m_den.neg();
    }
    return b;
}


void BIRational::_gcd(BigInt const& a, BigInt const& b, BigInt & gcd)
{
    BigInt t;
    BigInt x = a;
    BigInt y = b;
    if (x < 0) {
        x.neg();
    }
    if (y < 0) {
        y.neg();
    }
    if (x > y) {
        t = x;
        x = y;
        y = t;
    }
    BigInt quo, rem;
    while (x != 0) {
        t = x;

        biDivRem(y, x, quo, rem);
        x = rem;
        
        //x = y % x;
        y = t;
    }
    gcd = y;
}


void BIRational::reduce()
{
    if (m_num == BigInt(1, 0)) {
        m_den.setEqualTo(1);
        return;
    }
    BigInt gcd;
    _gcd(m_num, m_den, gcd);
    if (gcd == BigInt(1, 1)) {
        if (m_den.is_neg()) {
            m_den.neg();
            m_num.neg();
        }
        return;
    }
    BigInt quo, rem;
    biDivRem(m_num, gcd, quo, rem);
    bool is_num_neg = m_num.is_neg();
    m_num = quo;
    biDivRem(m_den, gcd, quo, rem);
    bool is_den_neg = m_den.is_neg();
    m_den = quo;
    if (is_num_neg ^ is_den_neg) {
        if (!is_num_neg) {
            m_num.neg();
        }
        if (is_den_neg) {
            m_den.neg();
        }
    } else {
        if (is_num_neg) {
            m_num.neg();
        }
        if (is_den_neg) {
            m_den.neg();
        }
    }
}


void BIRational::dump() const
{
    dump(stdout);
}


void BIRational::dump(FILE * h) const
{
    ASSERT0(h);
    if (m_den == 1) {
        //Dump numerator.
        m_num.dump(h, false, false);
        fprintf(h, " / 1");
    } else {
        //Dump numerator.
        m_num.dump(h, false, false);
        fprintf(h, " / ");
        //Dump denominator
        m_den.dump(h, false, false);
    }
    fflush(h);
}


void BIRational::dump(char const* name) const
{
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    dump(h);
    fclose(h);
}


bool operator < (BIRational const& a, BIRational const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    BigInt res1, res2;
    bisMul(a.m_num, b.m_den, res1);
    bisMul(a.m_den, b.m_num, res2);
    return res1 < res2;
}


bool operator <= (BIRational const& a, BIRational const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    BigInt res1, res2;
    bisMul(a.m_num, b.m_den, res1);
    bisMul(a.m_den, b.m_num, res2);
    return res1 <= res2;
}


BIRational operator * (BIRational const& a, BIRational const& b)
{
    BigInt rnum;
    bisMul(a.m_num, b.m_num, rnum);
    BIRational rat;
    if (rnum == BigInt(1, 0)) {
        rat.m_num.setEqualTo(0);
        rat.m_den.setEqualTo(1);
        return rat;
    }
    BigInt rden;
    bisMul(a.m_den, b.m_den, rden);
    ASSERTN(rden != BigInt(1, 0), ("den is zero"));
    if (rnum == rden) {
        rat.m_num.initElem(1, 1);
        rat.m_den.initElem(1, 1);
        return rat;
    }

    BigInt rden2(rden);
    rden2.neg();
    if (rnum == rden2) {
        rat.m_num.initElem(1, -1);
        rat.m_den.initElem(1, 1);
        return rat;
    }

    if ((rnum < 0 && rden < 0) || rden < 0) {
        rnum.neg();
        rden.neg();
    }
    rat.m_num = rnum;
    rat.m_den = rden;
    return rat;
}


BIRational operator / (BIRational const& a, BIRational const& b)
{
    BigInt anum = a.m_num;
    BigInt aden = a.m_den;
    BigInt bnum = b.m_num;
    BigInt bden = b.m_den;
    ASSERTN(aden != 0 && bden != 0, ("denominator is 0"));
    ASSERTN(bnum != 0, ("'a' divided by 0"));
    BIRational rat;
    if (anum == 0) {
        rat.m_num.initElem(1, 0);
        rat.m_den.initElem(1, 1);
        return rat;
    }
    if (anum == aden) {
        rat.m_num = bden;
        rat.m_den = bnum;
        if (bnum < 0) {
            rat.m_num.neg();
            rat.m_den.neg();
        }
        return rat;
    }

    BigInt ratnum;
    BigInt ratden;
    bisMul(anum, bden, ratnum);
    bisMul(aden, bnum, ratden);
    if (ratnum == ratden) {
        rat.m_num.initElem(1, 1);
        rat.m_den.initElem(1, 1);
        return rat;
    }

    BigInt ratden2(ratden);
    ratden2.neg();
    if (ratnum == ratden2) {
        rat.m_num.initElem(1, -1);
        rat.m_den.initElem(1, 1);
        return rat;
    }
    if ((ratnum < 0 && ratden < 0) || ratden < 0) {
        ratnum.neg();
        ratden.neg();
    }
    ASSERT0(ratden > 0);
    rat.m_num = ratnum;
    rat.m_den = ratden;
    return rat;
}


BIRational operator + (BIRational const& a, BIRational const& b)
{
    ASSERTN(a.m_den != 0 && b.m_den != 0, ("denominator is 0!"));
    BIRational rat;
    BigInt rden, rnum;
    bisMul(a.m_num, b.m_den, rden); //rden used for tmp result.
    bisMul(a.m_den, b.m_num, rnum); //rnum used for tmp result.
    bisAdd(rden, rnum, rnum);
    if (rnum == 0) {
        rat.m_num.initElem(1, 0);
        rat.m_den.initElem(1, 1);
        return rat;
    }

    bisMul(a.m_den, b.m_den, rden);

    ASSERTN(rden != 0, ("den is 0"));
    if (rnum == rden) {
        rat.m_num.initElem(1, 1);
        rat.m_den.initElem(1, 1);
        return rat;
    }

    BigInt rden2(rden);
    rden2.neg();
    if (rnum == rden2) {
        rat.m_num.initElem(1, -1);
        rat.m_den.initElem(1, 1);
        return rat;
    }
    if ((rnum < 0 && rden < 0) || rden < 0) {
        rnum.neg();
        rden.neg();
    }
    ASSERT0(rden > 0);
    rat.m_num = rnum;
    rat.m_den = rden;
    return rat;
}

} //namespace xcom
