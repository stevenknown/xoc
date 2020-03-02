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
#ifndef __XMAT_H__
#define __XMAT_H__

namespace xcom {

class INTMat;
class RMat;
class BIRMat;
class FloatMat;

///Rational
RMat operator * (RMat const& a, RMat const& b);
RMat operator + (RMat const& a, RMat const& b);
RMat operator - (RMat const& a, RMat const& b);

class RMat : public Matrix<Rational> {
protected:
    friend RMat operator * (RMat const& a, RMat const& b);
    friend RMat operator + (RMat const& a, RMat const& b);
    friend RMat operator - (RMat const& a, RMat const& b);
    friend class INTMat;
    friend class BIRMat;
    bool m_is_init;

    void _init_hook();
public:
    RMat();
    RMat(FRAC_TYPE v); //used by template call of T(0) in Vector<Mat>
    RMat(RMat const& m);
    RMat(INTMat const& m);
    RMat(UINT row, UINT col);
    ~RMat();
    void init();
    void init(UINT row, UINT col);
    void init(RMat const& m);
    void init(INTMat const& m);
    void destroy();
    bool is_init() const { return m_is_init; }
    bool is_imat(UINT * row = NULL, UINT * col = NULL);
    void sete(UINT num,...);
    void setr(UINT row, UINT col, FRAC_TYPE numer, FRAC_TYPE denom = 1);
    void setr(UINT row, UINT col, Rational rat);
    void getr(UINT row, UINT col, FRAC_TYPE * numer, FRAC_TYPE * denom);
    Rational getr(UINT row, UINT col);
    bool inv(RMat & e) const;
    void ds(RMat const& c);
    void copy(RMat const& r);
    void copy(INTMat const& r);
    void clean() { zero(); }
    UINT comden(UINT row, UINT col); //Common denominator
    void substit(RMat const& exp,
                 UINT v,
                 bool is_eq = true,
                 INT rhs_idx = -1);
    void intlize(INT row = -1); //Converting rational element to integer.
    RMat & operator = (RMat const& m);
    Rational reduce(UINT row, UINT col);
    void reduce();
};


///Integer
INTMat operator * (INTMat const& a, INTMat const& b);
INTMat operator + (INTMat const& a, INTMat const& b);
INTMat operator - (INTMat const& a, INTMat const& b);

class INTMat : public Matrix<INT> {
    friend class RMat;
    friend class BIRMat;
    bool m_is_init;
    void _verify_hnf(INTMat &h);
    friend INTMat operator * (INTMat const& a, INTMat const& b);
    friend INTMat operator + (INTMat const& a, INTMat const& b);
    friend INTMat operator - (INTMat const& a, INTMat const& b);
public:
    INTMat();

    //Used by template call of T(0) in Vector<Mat>.
    INTMat(INT v);
    INTMat(UINT row, UINT col);
    ~INTMat();
    //Find convex hull of a set of points.
    void cvexhull(OUT INTMat &hull);
    INTMat & operator = (INTMat const& m);
    void copy(RMat const& r);
    void clean() { fzero(); }

    //Invering of Integer Matrix will be transformed
    //to Rational Matrix, and one exception will be thrown
    //if there are some element's denomiator is not '1'.
    bool inv(OUT INTMat &e) const;
    void init();
    bool is_init() const { return m_is_init; }

    void destroy();
    INT det() const;

    //Set entry value one by one, 'num' indicate entry number.
    void sete(UINT num,...);

    //Generate unimodular matrix to eliminate element.
    void genElimMat(UINT row, UINT col, OUT INTMat &elim);

    //Hermite Normal Form decomposition.
    void hnf(OUT INTMat &h, OUT INTMat &u);

    //Reduce matrix by GCD operation.
    void gcd();

    void dumpf(CHAR const* name = MATRIX_DUMP_FILE_NAME,
               bool is_del = false) const;
    void dumps() const;
};


///Float
#define DEFAULT_SD            6
#define USE_FAST_BUT_LOW_PRECISION_SQRT
FloatMat operator * (FloatMat const& a, FloatMat const& b);
FloatMat operator - (FloatMat const& a, FloatMat const& b);
class FloatMat : public Matrix<Float> {
    bool m_is_init;
    CHAR * m_sd_str; //Descripte significant digit string.
public:
    FloatMat();
    FloatMat(INT v); //used by template call of T(0) in Vector<Mat>
    FloatMat(UINT row, UINT col);
    ~FloatMat();
    void init();
    void destroy();
    bool is_init() const { return m_is_init; }
    void sete(UINT num,...);
    void setie(UINT num, ...);
    FloatMat& operator = (FloatMat const& m);
    void setSigDigitDesc(UINT sd); //Redefine the significant digit.
    void substit(IN FloatMat const& exp, UINT v, bool is_eq, INT rhs_idx);
    bool is_imat(UINT * row, UINT * col);

    //Get the significant digit description string.
    CHAR const* getSigDigitDesc() const;
    Float reduce(UINT row, UINT col) { return get(row, col); }
    void reduce() {}
};


///Boolean
class BMat : public Matrix<bool> {
    bool m_is_init;
public:
    BMat();
    BMat(INT v); //used by template call of T(0) in Vector<Mat>
    BMat(UINT row, UINT col);
    ~BMat();
    void init();
    void destroy();
    bool is_init() const { return m_is_init; }

    //Set entry value one by one, 'num' indicate entry number.
    void sete(UINT num, ...);
    BMat & operator = (BMat const& m);
};

} //namespace xcom
#endif
