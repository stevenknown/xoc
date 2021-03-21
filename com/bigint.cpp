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

#define GET_HIGH_PART(val) \
    ((BigIntElemType)(val >> SHIFT_BIT_SIZE_OF_HIGH_PART))

#define GET_LOW_PART(val) ((BigIntElemType)(val))

#define MAKE_SUPER_ELEM(low, high) (((SuperElemType)low) | \
    (((SuperElemType)high) << SHIFT_BIT_SIZE_OF_HIGH_PART))

namespace xcom {

//
//START BigInt
//
BigInt::BigInt(UINT elemnum, ...)
{
    ASSERT0(elemnum > 0);
    va_list ptr;
    va_start(ptr, elemnum);
    UINT i = 0;
    BigIntElemType last = 0;
    set(elemnum - 1, 0); //pre-allocate buffer.
    for (; i < elemnum; i++) {
        last = va_arg(ptr, BigIntElemType);
        set(i, last);
    }
    //The last element is regarded as significant position
    //for big integer.
    setSigPos(elemnum - 1);
    va_end(ptr);
}


void BigInt::setEqualTo(BigIntElemType elem)
{
    set(0, elem);
    setSigPos(0);
}


//Set a list of integer that are expected
//value for each element.
//NOTE make sure the last element is 0x0.
void BigInt::initElem(UINT elemnum, ...)
{
    ASSERT0(elemnum > 0);
    va_list ptr;
    va_start(ptr, elemnum);
    UINT i = 0;
    clean();
    BigIntElemType last = 0;
    set(elemnum - 1, 0); //pre-allocate buffer.
    for (; i < elemnum; i++) {
        last = va_arg(ptr, BigIntElemType);
        set(i, last);
    }
    //The last element is regarded as significant position
    //for big integer.
    setSigPos(elemnum - 1);
    va_end(ptr);
}


bool BigInt::isAllElemEqual(BigIntElemType elem) const
{
    for (INT i = 0; i <= getSigPos(); i++) {
        if ((*this)[i] != elem) {
            return false;
        }
    }
    return true;
}


//is_seg_hex: true if dump each segments in Hex format.
void BigInt::dump(CHAR const* name, bool is_seg_hex) const
{
    #define BS_NAME "bigint.dump"
    if (name == nullptr) {
        name = BS_NAME;
    }
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    dump(h, true, is_seg_hex);
    fclose(h);
}


//Dump to stdout.
void BigInt::dump(bool is_seg_hex) const
{
    dump(stdout, false, is_seg_hex);
}


//is_seg_hex: true if dump each segments in Hex format.
//with_newline: true to dump a '\n' before content.
void BigInt::dump(FILE * h, bool with_newline, bool is_seg_hex) const
{
    ASSERTN(h, ("require file handler"));
    if (getSigPos() >= 0 && with_newline) {
        fprintf(h, "\n");
    }
    bool reverse_order = false;
    INT last_pos = get_last_idx();
    for (INT i = 0; i <= last_pos; i++) {
        BigIntElemType val = get(i);
        if (reverse_order) {
            //Extract the lower 4-bit data from value.
            #define BIT_PER_BYTE 8
            #define LOWER_4_BIT_SHIFT_SIZE 4
            #define LOWER_4_BIT_MASK 0xF
            UINT len = sizeof(BigIntElemType) /
                sizeof(BYTE) * (BIT_PER_BYTE / LOWER_4_BIT_SHIFT_SIZE);
            UINT cnt = 0;
            if (i == m_sig_pos) {
                fprintf(h, "[");
            }
            for (UINT j = 0; j < len; j++) {
                fprintf(h, "%01x", (UINT)(val & LOWER_4_BIT_MASK));
                cnt++;
                val >>= LOWER_4_BIT_SHIFT_SIZE;
            }
            if (i == m_sig_pos) {
                fprintf(h, "]");
            }
            if (cnt == len && i != last_pos) {
                fprintf(h, " ");
                cnt = 0;
            }
            //fprintf(h, "%01x ", val);
        } else {
            if (i == m_sig_pos) {
                fprintf(h, "[");
            }
            if (is_seg_hex) {
                fprintf(h, "%08x", val);
            } else {
                fprintf(h, "%d", val);
            }
            if (i == m_sig_pos) {
                fprintf(h, "]");
            }
            if (i != last_pos) {
                fprintf(h, " ");
            }
        }
    }

    fflush(h);
}


//Input a list of integer that are expected value for each element in BigInt.
//valnum: the number of value.
//...: value list.
bool BigInt::verify(UINT elemnum, ...)
{
    ASSERT0(elemnum <= (UINT)(getSigPos() + 1));
    va_list ptr;
    va_start(ptr, elemnum);
    UINT i = 0;
    for (; i < elemnum; i++) {
        BigIntElemType expected_val = va_arg(ptr, BigIntElemType);
        BigIntElemType val = get(i);
        CHECK0_DUMMYUSE(val == expected_val);
    }
    va_end(ptr);
    return true;
}
//END BigInt


//
//START BigIntMgr
//
void BigIntMgr::clean()
{
    for (INT i = 0; i <= m_bi_vec.get_last_idx(); i++) {
        BigInt * bi = m_bi_vec[i];
        ASSERT0(bi);
        delete bi;
    }
    m_bi_vec.clean();
}


BigInt * BigIntMgr::create()
{
    BigInt * p = new BigInt();
    m_bi_vec.append(p);
    return p;
}


BigInt * BigIntMgr::copy(BigInt const& src)
{
    BigInt * p = create();
    p->copy(src);
    return p;
}
//END BigIntMgr


//
//Binary Operations
//
bool operator != (BigInt const& a, BigInt const& b)
{
    return !(a == b);
}


bool operator == (BigInt const& a, BigInt const& b)
{
    BigInt const* longer = nullptr;
    BigInt const* shorter = nullptr;
    if (b.getSigPos() > a.getSigPos()) {
        //b's length > a's length
        longer = &b;
        shorter = &a;
    } else {
        //b's length <= a's length
        longer = &a;
        shorter = &b;
    }

    //The element number of shorter BigInt with no significant position.
    INT shorter_elem_num = shorter->getSigPos();
    INT i;
    for (i = 0; i <= shorter_elem_num; i++) {
        if (shorter->get(i) != longer->get(i)) {
            return false;
        }
    }
    INT longer_elem_num = longer->getSigPos();
    if (i <= longer_elem_num) {
        BigIntElemType rest_elem = IS_NEG(longer->getSig()) ?
            (BigIntElemType)(-1) : (BigIntElemType)(0);
        for (; i <= longer_elem_num; i++) {
            if (longer->get(i) == rest_elem) {
                continue;
            }
            return false;
        }
    }
    return true;
}


bool operator < (BigInt const& a, BigInt const& b)
{
    bool is_a_neg = a.is_neg();
    bool is_b_neg = b.is_neg();
    if (!is_a_neg && !is_a_neg) {
        if (a.getSigPos() < b.getSigPos()) { return true; }
        if (a.getSigPos() > b.getSigPos()) { return false; }
        for (INT i = 0; i <= a.getSigPos(); i++) {
            if (a[i] >= b[i]) {
                return false;
            }
        }
        return true;
    }
    if (is_a_neg && !is_b_neg) {
        return true;
    }
    if (!is_a_neg && is_b_neg) {
        return false;
    }

    //Both a, b are negative.
    BigInt const* shorter;
    BigInt const* longer;
    if (a.getSigPos() < b.getSigPos()) {
        shorter = &a;
        longer = &b;
    } else {
        shorter = &b;
        longer = &a;
    }
    for (INT i = 0; i <= shorter->getSigPos(); i++) {
        if ((*shorter)[i] < (*longer)[i]) {
            return true;
        }
    }
    return true;
}


bool operator <= (BigInt const& a, BigInt const& b)
{
    //if (a.getSigPos() > b.getSigPos()) { return false; }
    //if (a.getSigPos() < b.getSigPos()) { return true; }
    //for (INT i = 0; i <= a.getSigPos(); i++) {
    //    if (a[i] > b[i]) {
    //        return false;
    //    }
    //}
    //return true;

    bool is_a_neg = a.is_neg();
    bool is_b_neg = b.is_neg();
    if (!is_a_neg && !is_a_neg) {
        //Both a, b are positive.
        if (a.getSigPos() > b.getSigPos()) { return false; }
        if (a.getSigPos() < b.getSigPos()) { return true; }
        for (INT i = 0; i <= a.getSigPos(); i++) {
            if (a[i] > b[i]) {
                return false;
            }
        }
        return true;
    }
    if (is_a_neg && !is_b_neg) {
        return true;
    }
    if (!is_a_neg && is_b_neg) {
        return false;
    }

    //Both a, b are negative.
    BigInt const* shorter;
    BigInt const* longer;
    if (a.getSigPos() < b.getSigPos()) {
        shorter = &a;
        longer = &b;
    } else {
        shorter = &b;
        longer = &a;
    }
    for (INT i = 0; i <= shorter->getSigPos(); i++) {
        if ((*shorter)[i] < (*longer)[i]) {
            return true;
        }
    }
    if (a.getSigPos() == b.getSigPos()) {
        return true;
    }
    return false;
}


bool operator > (BigInt const& a, BigInt const& b)
{
    return !(a <= b);
}


bool operator >= (BigInt const& a, BigInt const& b)
{
    return !(a < b);
}


bool operator == (BigInt const& a, BigIntElemType v)
{
    BigInt vv;
    vv.set_vec(&v, 1);
    vv.setSigPos(0);
    bool res = a == vv;
    vv.set_vec(nullptr, 0);
    return res;

    //INT i = 0;
    //if (i <= a.getSigPos()) {
    //    //Compare the first elem.
    //    if (a[i] != v) {
    //        return false;
    //    }
    //}
    //for (i = 1; i <= a.getSigPos(); i++) {
    //    if (a[i] != 0) {
    //        return false;
    //    }
    //}
    //return true;
}


bool operator == (BigIntElemType v, BigInt const& a)
{
    return a == v;
}


bool operator != (BigInt const& a, BigIntElemType v)
{
    return !(a == v);
}


bool operator != (BigIntElemType v, BigInt const& a)
{
    return !(a == v);
}


bool operator < (BigInt const& a, BigIntElemType v)
{
    BigInt vv;
    vv.set_vec(&v, 1);
    vv.setSigPos(0);
    bool res = a < vv;
    vv.set_vec(nullptr, 0);
    return res;

    //for (INT i = 0; i <= a.getSigPos(); i++) {
    //    if (a[i] >= v) {
    //        return false;
    //    }
    //}
    //return true;
}


bool operator < (BigIntElemType v, BigInt const& a)
{
    return a > v;
}


bool operator <= (BigIntElemType v, BigInt const& a)
{
    return a >= v;
}


bool operator > (BigIntElemType v, BigInt const& a)
{
    return a < v;
}


bool operator >= (BigIntElemType v, BigInt const& a)
{
    return a <= v;
}


bool operator <= (BigInt const& a, BigIntElemType v)
{
    BigInt vv;
    vv.set_vec(&v, 1);
    vv.setSigPos(0);
    bool res = a <= vv;
    vv.set_vec(nullptr, 0);
    return res;

    //for (INT i = 0; i <= a.getSigPos(); i++) {
    //    if (a[i] > v) {
    //        return false;
    //    }
    //}
    //return true;
}


bool operator > (BigInt const& a, BigIntElemType v)
{
    return !(a <= v);
}


bool operator >= (BigInt const& a, BigIntElemType v)
{
    return !(a < v);
}


//BigInt Signed Addition.
//e.g: res = a + b;
BigInt& bisAdd(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res)
{
    ASSERT0(sizeof(SuperElemType) >= 2 * sizeof(BigIntElemType));
    ASSERTN(b.getSigPos() >= 0, ("Miss significant elem"));
    ASSERTN(a.getSigPos() >= 0, ("Miss significant elem"));
    BigInt const* longer = nullptr;
    BigInt const* shorter = nullptr;
    if (b.getSigPos() > a.getSigPos()) {
        //b's length > a's length
        longer = &b;
        shorter = &a;
    } else {
        //b's length <= a's length
        longer = &a;
        shorter = &b;
    }

    BigIntElemType carry_part = 0;
    INT i;

    //The element number of shorter BigInt with no significant position.
    INT shorter_elem_num = shorter->getSigPos();
    for (i = 0; i <= shorter_elem_num; i++) {
        SuperElemType v1 = (SuperElemType)shorter->get(i);
        SuperElemType v2 = (SuperElemType)longer->get(i);
        SuperElemType v3 = v1 + v2 + (SuperElemType)carry_part;
        carry_part = GET_HIGH_PART(v3);
        carry_part = IS_NEG(carry_part) ? 0 : carry_part;
        BigIntElemType res_part = GET_LOW_PART(v3);
        res.set(i, res_part);
    }
    INT longer_elem_num = longer->getSigPos();
    for (; i <= longer_elem_num; i++) {
        SuperElemType v2 = (SuperElemType)longer->get(i);
        SuperElemType v3 = v2 + (SuperElemType)carry_part;
        carry_part = GET_HIGH_PART(v3);
        carry_part = IS_NEG(carry_part) ? 0 : carry_part;
        BigIntElemType res_part = GET_LOW_PART(v3);
        res.set(i, res_part);
    }
    if (carry_part != 0) {
        res.set(i, carry_part);

        //Set significant position to result.
        res.setSigPos(i);
    } else {
        ASSERT0((i - 1) >= 0);
        //Set significant position to result.
        res.setSigPos(i - 1);
    }
    return res;
}


//BigInt Unsigned Addition.
//e.g: res = a + b;
//Note unsigned addition support in-situ operation.
//e.g: res = res + b is legal.
BigInt& biuAdd(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res)
{
    ASSERT0(sizeof(SuperUElemType) >= 2 * sizeof(BigIntUElemType));
    ASSERTN(b.getSigPos() >= 0, ("Miss significant elem"));
    ASSERTN(a.getSigPos() >= 0, ("Miss significant elem"));
    BigInt const* longer = nullptr;
    BigInt const* shorter = nullptr;
    if (b.getSigPos() > a.getSigPos()) {
        //b's length > a's length
        longer = &b;
        shorter = &a;
    } else {
        //b's length <= a's length
        longer = &a;
        shorter = &b;
    }

    BigIntUElemType carry_part = 0;
    INT i;

    //The element number of shorter BigInt with no significant position.
    INT shorter_elem_num = shorter->getSigPos();
    for (i = 0; i <= shorter_elem_num; i++) {
        SuperUElemType v1 = (SuperUElemType)(BigIntUElemType)shorter->get(i);
        SuperUElemType v2 = (SuperUElemType)(BigIntUElemType)longer->get(i);
        SuperUElemType v3 = v1 + v2 + (SuperUElemType)carry_part;
        //carry_part = GET_HIGH_PART(v3) == 0 ? 0 : 1;
        carry_part = GET_HIGH_PART(v3);
        carry_part = IS_NEG(carry_part) ? 0 : carry_part;
        BigIntUElemType res_part = GET_LOW_PART(v3);
        res.set(i, res_part);
    }
    INT longer_elem_num = longer->getSigPos();
    for (; i <= longer_elem_num; i++) {
        SuperUElemType v2 = (SuperUElemType)(BigIntUElemType)longer->get(i);
        SuperUElemType v3 = v2 + (SuperUElemType)(BigIntUElemType)carry_part;
        //carry_part = GET_HIGH_PART(v3) == 0 ? 0 : 1;
        carry_part = GET_HIGH_PART(v3);
        carry_part = IS_NEG(carry_part) ? 0 : carry_part;
        BigIntUElemType res_part = GET_LOW_PART(v3);
        res.set(i, res_part);
    }
    if (carry_part != 0) {
        res.set(i, carry_part);

        //Set significant position to result.
        res.setSigPos(i);
    } else {
        ASSERT0((i - 1) >= 0);
        //Set significant position to result.
        res.setSigPos(i - 1);
    }
    return res;
}


//BigInt substraction.
//e.g: res = a - b;
//Note substraction does not support in-situ operation.
//e.g: res = res - b is not legal.
BigInt& biSub(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res)
{
    ASSERT0(sizeof(SuperElemType) >= 2 * sizeof(BigIntElemType));
    ASSERTN(b.getSigPos() >= 0, ("Miss significant elem"));
    ASSERTN(a.getSigPos() >= 0, ("Miss significant elem"));
    INT longer_pos = a.getSigPos() > b.getSigPos() ?
        a.getSigPos() : b.getSigPos();
    INT i;
    for (i = 0; i <= b.getSigPos(); i++) {
        res.set(i, ~b[i]);
    }
    for (; i <= longer_pos; i++) {
        res.set(i, ~((BigIntElemType)0));
    }
    ASSERT0((i - 1) >= 0);
    res.setSigPos(i - 1); //Set significant.
    BigInt addend(1, 0x1);
    biuAdd(res, addend, res);
    biuAdd(a, res, res);
    //Neglect the carray part if exist.
    res.setSigPos(i - 1); //Set significant.
    return res;
}


//BigInt Unsigned Mutiplification.
//e.g: res = a * b;
BigInt& biuMul(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res)
{
    ASSERT0(sizeof(SuperUElemType) >= 2 * sizeof(BigIntUElemType));
    ASSERTN(b.getSigPos() >= 0, ("Miss significant elem"));
    ASSERTN(a.getSigPos() >= 0, ("Miss significant elem"));
    ASSERTN(b.getSigPos() >= 0, ("Miss significant elem"));
    UINT shift_cnt = 0;
    BigInt tmpres;
    res.setEqualTo(0);
    for (INT j = 0; j <= b.getSigPos(); j++) {
        tmpres.clean();
        tmpres.setEqualTo(0);
        if (shift_cnt != 0) {
            //Initialize shift-part of temporary result of multiple.
            //e.g: 21 * 34, first multiple result is 63, the second
            //result is 840, shift_cnt for second result is 1, which
            //means shifting 84 to leftside to be 840.
            UINT k = 0;
            for (; k < shift_cnt; k++) {
                tmpres.set(k, 0);
            }
            tmpres.setSigPos(k);
        }
        BigIntUElemType carry_part = 0;
        INT i = 0;
        for (; i <= a.getSigPos(); i++) {
            SuperUElemType v1 = (SuperUElemType)(BigIntUElemType)a.get(i);
            SuperUElemType v2 = (SuperUElemType)(BigIntUElemType)b.get(j);
            SuperUElemType v3 = (v1 * v2) + (SuperUElemType)carry_part;
            carry_part = GET_HIGH_PART(v3);
            BigIntUElemType res_part = GET_LOW_PART(v3);
            tmpres.set(i + shift_cnt, res_part);
        }
        if (carry_part != 0) {
            tmpres.set(i + shift_cnt, carry_part);

            //Set significant position to result.
            tmpres.setSigPos(i + shift_cnt);
        } else {
            ASSERT0((i + shift_cnt - 1) >= 0);
            //Set significant position to result.
            tmpres.setSigPos(i + shift_cnt - 1);
        }
        biuAdd(res, tmpres, res);
        shift_cnt++;
    }
    return res;
}


//BigInt Signed Mutiplification.
//e.g: res = a * b;
BigInt& bisMul(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res)
{
    BigInt const* pa = &a;
    BigInt const* pb = &b;
    BigInt ta;
    BigInt tb;
    bool is_a_neg = false;
    bool is_b_neg = false;
    //Change value into absolute values.
    if (IS_NEG(a.getSig())) {
        ta.copy(a);
        ta.neg();
        pa = &ta;
        is_a_neg = true;
    }
    if (IS_NEG(b.getSig())) {
        tb.copy(b);
        tb.neg();
        pb = &tb;
        is_b_neg = true;
    }
    biuMul(*pa, *pb, res);
    if (is_a_neg ^ is_b_neg) {
        if (!res.isAllElemEqual(0)) {
            res.neg();
        }
    }
    return res;
}


//BigInt Signed Division.
//Return quotient and remainder of a divided by b.
//The quotient and remainder respect a = quo * b + rem.
void biDivRem(IN BigInt const& a, IN BigInt const& b,
              IN OUT BigInt & quo, IN OUT BigInt & rem)

{
    bool is_a_neg = a.is_neg();
    bool is_b_neg = b.is_neg();
    if (!is_a_neg && !is_b_neg) {
        biuDivRem(a, b, quo, rem);
        return;
    }

    BigInt abs_a;
    BigInt abs_b;
    BigInt * pa = const_cast<BigInt*>(&a);
    BigInt * pb = const_cast<BigInt*>(&b);
    if (is_a_neg) {
        abs_a = a;
        abs_a.abs();
        pa = &abs_a;
    }
    if (is_b_neg) {
        abs_b = b;
        abs_b.abs();
        pb = &abs_b;
    }
    biuDivRem(*pa, *pb, quo, rem);
    if (is_a_neg ^ is_b_neg) {
        quo.neg();
    }
}


//BigInt Signed Division.
//Return quotient and remainder of a divided by b.
//The quotient and remainder respect a = quo * b + rem.
void biuDivRem(IN BigInt const& a, IN BigInt const& b,
               IN OUT BigInt & quo, IN OUT BigInt & rem)
{

    if (a < b) {
        quo.setEqualTo(0);
        rem = a;
        return;
    }
    BigInt copy_a = a;
    BigInt * pa = &copy_a;
    BigInt const* pb = &b;
    BigInt * pc = &rem;
    BigIntUElemType small_quo = 0;
    //#define SMALL_QUO_MAX  REMOVE_FLAG((ULONGLONG)(-1) & ((ULONGLONG)0xF));
    #define SMALL_QUO_MAX  (((BigIntUElemType)(-1) >> 2) << 2)
    bool is_use_small_quo = true;
    BigInt const addend(1, 0x1);
    while (true) {
        biSub(*pa, *pb, *pc);
        small_quo++;
        if (*pc < *pb) {
            if (pc != &rem) {
                rem = *pc;
            }
            if (is_use_small_quo) {
                quo.setEqualTo(small_quo);
            }
            break;
        }
        BigInt * t = pa;
        pa = pc;
        pc = t;
        pc->setEqualTo(0);
        if (is_use_small_quo) {
            if (small_quo > SMALL_QUO_MAX) {
                quo.setEqualTo(small_quo);
                is_use_small_quo = false;
            }
        } else {
            biuAdd(quo, addend, quo);
        }
    }
}


//Compute negative value on current bigint.
void BigInt::neg()
{
    ASSERT0(sizeof(SuperElemType) >= 2 * sizeof(BigIntElemType));
    ASSERTN(getSigPos() >= 0, ("Miss significant elem"));
    if (isAllElemEqual(0)) {
        return;
    }
    for (INT i = 0; i <= getSigPos(); i++) {
        set(i, ~(*this)[i]);
    }
    BigInt addend(1, 0x1);
    biuAdd(*this, addend, *this);
}


//Compute absolute value on current bigint.
void BigInt::abs()
{
    if (IS_NEG(getSig())) {
        neg();
    }
}

} //namespace xcom
