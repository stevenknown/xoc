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
#ifndef __BIG_INT_H__
#define __BIG_INT_H__

//Big Integer
//The value described by Big Integer is just signed.

namespace xcom {

class BigInt;
class BigIntMgr;
typedef INT BigIntElemType;
typedef LONGLONG SuperElemType;
typedef UINT BigIntUElemType;
typedef ULONGLONG SuperUElemType;
#define SHIFT_BIT_SIZE_OF_HIGH_PART 32
#define IS_NEG(val) \
  (((BigIntUElemType)val) & \
  (BigIntUElemType)(1 << (SHIFT_BIT_SIZE_OF_HIGH_PART - 1)))

//Export Functions
bool operator != (BigInt const& a, BigInt const& b);
bool operator == (BigInt const& a, BigInt const& b);
bool operator < (BigInt const& a, BigInt const& b);
bool operator <= (BigInt const& a, BigInt const& b);
bool operator > (BigInt const& a, BigInt const& b);
bool operator >= (BigInt const& a, BigInt const& b);

bool operator != (BigInt const& a, BigIntElemType v);
bool operator == (BigInt const& a, BigIntElemType v);
bool operator < (BigInt const& a, BigIntElemType v);
bool operator <= (BigInt const& a, BigIntElemType v);
bool operator > (BigInt const& a, BigIntElemType v);
bool operator >= (BigInt const& a, BigIntElemType v);

bool operator != (BigIntElemType v, BigInt const& a);
bool operator == (BigIntElemType v, BigInt const& a);
bool operator < (BigIntElemType v, BigInt const& a);
bool operator <= (BigIntElemType v, BigInt const& a);
bool operator > (BigIntElemType v, BigInt const& a);
bool operator >= (BigIntElemType v, BigInt const& a);

BigInt& biuAdd(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res);
BigInt& bisAdd(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res);
BigInt& biSub(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res);
BigInt& bisMul(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res);
BigInt& biuMul(IN BigInt const& a, IN BigInt const& b, IN OUT BigInt & res);

class BigInt : public xcom::Vector<BigIntElemType> {
    friend bool operator != (BigInt const& a, BigInt const& b);
    friend bool operator == (BigInt const& a, BigInt const& b);
    friend bool operator < (BigInt const& a, BigInt const& b);
    friend bool operator <= (BigInt const& a, BigInt const& b);
    friend bool operator > (BigInt const& a, BigInt const& b);
    friend bool operator >= (BigInt const& a, BigInt const& b);
    friend bool operator != (BigInt const& a, BigIntElemType v);
    friend bool operator == (BigInt const& a, BigIntElemType v);
    friend bool operator < (BigInt const& a, BigIntElemType v);
    friend bool operator <= (BigInt const& a, BigIntElemType v);
    friend bool operator > (BigInt const& a, BigIntElemType v);
    friend bool operator >= (BigInt const& a, BigIntElemType v);
    friend bool operator != (BigIntElemType v, BigInt const& a);
    friend bool operator == (BigIntElemType v, BigInt const& a);
    friend bool operator < (BigIntElemType v, BigInt const& a);
    friend bool operator <= (BigIntElemType v, BigInt const& a);
    friend bool operator > (BigIntElemType v, BigInt const& a);
    friend bool operator >= (BigIntElemType v, BigInt const& a);

    friend BigInt& biuAdd(IN BigInt const& a,
                          IN BigInt const& b,
                          IN OUT BigInt & res);
    friend BigInt& bisAdd(IN BigInt const& a,
                          IN BigInt const& b,
                          IN OUT BigInt & res);
    friend BigInt& biSub(IN BigInt const& a,
                         IN BigInt const& b,
                         IN OUT BigInt & res);
    friend BigInt& bisMul(IN BigInt const& a,
                          IN BigInt const& b,
                          IN OUT BigInt & res);
    friend BigInt& biuMul(IN BigInt const& a,
                          IN BigInt const& b,
                          IN OUT BigInt & res);
    

private:
    INT m_sig_pos; //Record significant position.

protected:
    void setSig(INT pos) { m_sig_pos = pos; }

public:
    BigInt() { m_sig_pos = -1; }
    BigInt(BigInt const& bi) { copy(bi); }
    BigInt(UINT elemnum, ...);
    ~BigInt() {}

    //Copy from 'src' to this object.
    void copy(BigInt const& src)
    {
        ((xcom::Vector<BigIntElemType>*)this)->copy(
            (xcom::Vector<BigIntElemType> const&)src);
        m_sig_pos = src.m_sig_pos;
    }
    void clean() { ((xcom::Vector<BigIntElemType>*)this)->clean(); }

    void neg();
    void abs(); //Change value into absolute values.

    //Dump big-integer to specific file with 'name'.
    //is_seg_hex: true if dump each segments in Hex format.
    void dump(CHAR const* name, bool is_seg_hex) const;
    void dump(FILE * h, bool with_newline, bool is_seg_hex) const;
    void dump() const;

    //Return significant number position.
    //Return -1 if BigInt has no initial value.
    INT getSigPos() const { return m_sig_pos; }

    //Return significant number.    
    BigIntElemType getSig() const
    {
        ASSERTN(getSigPos() >= 0, ("no initial value"));
        return get((UINT)getSigPos());
    }

    bool isAllElemEqual(BigIntElemType elem) const;
    bool is_neg() const { return IS_NEG(getSig()) != 0; }

    //Support concatenation assignment, such as: a=b=c.
    BigInt const& operator = (BigInt const& src) { copy(src); return *this; }

    //Set a list of integer that are expected value for each element.
    //Note this function makes sure the last element is significant number.
    //elemnum: the number of elements to be set.
    //...: list of value for each element.
    bool initElem(UINT elemnum, ...);

    //Compare element value of bigint with a list of expected integer.
    //valnum: the number of values.
    bool verify(UINT valnum, ...);
};


//This class is responsible to allocate and destroy BigInt memory.
class BigIntMgr {
protected:
    xcom::Vector<BigInt*> m_bi_vec;

public:
    BigIntMgr() {}
    ~BigIntMgr() { clean(); }
    BigInt * create();
    BigInt * copy(BigInt const& src);
    void clean();
};

} //namespace xcom
#endif //END __BIG_INT_H__
