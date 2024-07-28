/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com
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
#ifndef __XMAT_H__
#define __XMAT_H__

namespace xcom {

class IMat;
class IMatMgr;
class RMat;
class RMatMgr;
class BIRMat;
class FMat;
class FMatMgr;

///Rational
typedef MatWrap<Rational> RMatWrap;

class RMat : public Matrix<Rational> {
protected:
    friend class IMat;
    friend class BIRMat;
protected:
    virtual void copyTBuf(Rational * dst, Rational const* src,
                          UINT elemnum) override
    { ::memcpy(dst, src, elemnum * sizeof(Rational)); }
public:
    RMat() {}
    RMat(UINT) {} //Used by template call of T(0) in Vector<Mat>
    RMat(RMat const& m) { copy(m); }

    //DO NOT explicitly initialize the base-class copy-constructor.
    RMat(IMat const& m) { init(m); }
    RMat(UINT row, UINT col) : Matrix<Rational>(row, col) {}
    ~RMat() { destroy(); }

    void adjust() override {}

    void copy(RMat const& r);
    void copy(IMat const& r);
    void clean() { zero(); }
    UINT comden(UINT row, UINT col); //Common denominator

    void destroy();
    void dumpf(CHAR const* name = MATRIX_DUMP_FILE_NAME,
               bool is_del = false) const override;
    void dumps() const override;
    void dumpfh(FILE * h) const;
    void ds(RMat const& c);

    void getr(UINT row, UINT col, Rational::FType * numer,
              Rational::FType * denom);
    Rational getr(UINT row, UINT col);

    void init();
    void init(UINT row, UINT col);
    void init(RMat const& m);
    void init(IMat const& m);
    bool is_imat(UINT * row = nullptr, UINT * col = nullptr);
    void intlize(INT row = -1); //Converting rational element to integer.

    RMat & operator = (RMat const& m);

    Rational reduce(UINT row, UINT col);
    void reduce();

    void sete(UINT num, ...);
    void setr(UINT row, UINT col, Rational::FType numer,
              Rational::FType denom = 1);
    void setr(UINT row, UINT col, Rational rat);
    void substit(RMat const& exp, UINT v, bool is_eq = true,
                 INT cst_col = CST_COL_UNDEF);
};


class RMatMgr : public MatMgr<Rational> {
public:
    virtual Matrix<Rational> * allocMat() override
    { ASSERT0(incMatCnt()); return new RMat(); }
    virtual Matrix<Rational> * allocMat(UINT row, UINT col) override
    { ASSERT0(incMatCnt()); return new RMat(row, col); }
};


///Integer
typedef MatWrap<INT> IMatWrap;

class IMat : public Matrix<INT> {
    friend class RMat;
    friend class BIRMat;
    void _verify_hnf(IMat &h) const;
protected:
    virtual void copyTBuf(INT * dst, INT const* src, UINT elemnum) override
    { ::memcpy(dst, src, elemnum * sizeof(INT)); }
public:
    IMat() {}
    IMat(INT) {} //Used by template call of T(0) in Vector<Mat>.
    IMat(UINT row, UINT col) : Matrix<INT>(row, col) {}
    ~IMat() {}

    void adjust() override {}

    //Find convex hull of a set of points.
    void cvexhull(OUT IMat &hull);
    void copy(RMat const& r);
    void clean() { fzero(); }

    void dumps() const override;
    INT det(MOD RMatMgr & mgr) const;
    void dumpf(CHAR const* name = MATRIX_DUMP_FILE_NAME,
               bool is_del = false) const override;

    //Generate unimodular matrix to eliminate element.
    void genElimMat(UINT row, UINT col, OUT IMat &elim);

    //Reduce matrix by GCD operation.
    void gcd();

    //Hermite Normal Form decomposition.
    void hnf(OUT IMat &h, OUT IMat &u, MOD IMatMgr & mgr) const;

    //Invering of Integer Matrix will be transformed
    //to Rational Matrix, and one exception will be thrown
    //if there are some element's denomiator is not '1'.
    bool inv(OUT IMat &e, RMatMgr & mgr) const;

    IMat & operator = (IMat const& m);
};


class IMatMgr : public MatMgr<INT> {
public:
    virtual Matrix<INT> * allocMat() override
    { ASSERT0(incMatCnt()); return new IMat(); }
    virtual Matrix<INT> * allocMat(UINT row, UINT col) override
    { ASSERT0(incMatCnt()); return new IMat(row, col); }
};


///Float
#define DEFAULT_SD            6
#define USE_FAST_BUT_LOW_PRECISION_SQRT

typedef MatWrap<Float> FMatWrap;

class FMat : public Matrix<Float> {
protected:
    virtual void copyTBuf(Float * dst, Float const* src, UINT elemnum) override
    { ::memcpy(dst, src, elemnum * sizeof(Float)); }
public:
    FMat() {}
    FMat(INT) {} //Used by template call of T(0) in Vector<Mat>
    FMat(UINT row, UINT col) : Matrix<Float>(row, col) {}
    ~FMat() {}

    void adjust() override;

    void dumps() const override;
    void dumpf(CHAR const* name = MATRIX_DUMP_FILE_NAME,
               bool is_del = false) const override;
    void dumpfh(FILE * h) const;

    //Get the significant digit description string.
    CHAR const* getSigDigitDesc() const;

    bool is_imat(UINT * row, UINT * col);

    FMat& operator = (FMat const& m);

    void reduce() {}
    Float reduce(UINT row, UINT col) { return get(row, col); }

    Float sqrtElem(Float a) override;
    void sete(UINT num, ...);
    void setie(UINT num, ...);
    void setSigDigitDesc(UINT sd); //Redefine the significant digit.
    void substit(IN FMat const& exp, UINT v, bool is_eq, INT cst_col);
};


class FMatMgr : public MatMgr<Float> {
public:
    virtual Matrix<Float> * allocMat() override
    { ASSERT0(incMatCnt()); return new FMat(); }
    virtual Matrix<Float> * allocMat(UINT row, UINT col) override
    { ASSERT0(incMatCnt()); return new FMat(row, col); }
};


///Boolean

typedef MatWrap<bool> BMatWrap;

class BMat : public Matrix<bool> {
protected:
    virtual void copyTBuf(bool * dst, bool const* src, UINT elemnum) override
    { ::memcpy(dst, src, elemnum * sizeof(bool)); }
public:
    BMat() {}
    BMat(INT) {} //Used by template call of T(0) in Vector<Mat>
    BMat(UINT row, UINT col) : Matrix<bool>(row, col) {}
    ~BMat() {}

    void adjust() override {}

    void dumps() const override;
    void dumpf(CHAR const* name = MATRIX_DUMP_FILE_NAME,
               bool is_del = false) const override;
    void dumpfh(FILE * h) const;

    //Set entry value one by one, 'num' indicate entry number.
    void sete(UINT num, ...);

    BMat & operator = (BMat const& m);
};


class BMatMgr : public MatMgr<bool> {
public:
    virtual Matrix<bool> * allocMat() override
    { ASSERT0(incMatCnt()); return new BMat(); }
    virtual Matrix<bool> * allocMat(UINT row, UINT col) override
    { ASSERT0(incMatCnt()); return new BMat(row, col); }
};

} //namespace xcom
#endif
