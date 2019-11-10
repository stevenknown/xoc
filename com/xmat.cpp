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
#include "math.h"
#include "xcominc.h"

namespace xcom {

Matrix<Rational> operator * (Matrix<Rational> const& a,
                             Matrix<Rational> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size > 0 && a.m_col_size > 0, ("invalid matrix"));
    ASSERTN(b.m_row_size > 0 && b.m_col_size > 0, ("invalid matrix"));
    ASSERTN(a.m_col_size == b.m_row_size, ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<Rational> c(a.m_row_size, b.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < b.m_col_size; j++) {
            Rational tmp = 0;
            for (UINT k = 0; k < a.m_col_size; k++) {
                tmp = tmp + a.get(i,k) * b.get(k,j);
            }
            c.set(i,j,tmp);
        }
    }
    return c;
}


Matrix<Rational> operator + (Matrix<Rational> const& a,
                             Matrix<Rational> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size == b.m_row_size && a.m_col_size == b.m_col_size,
            ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<Rational> c(a.m_row_size, a.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < a.m_col_size; j++) {
            c.set(i,j,a.get(i,j) + b.get(i,j));
        }
    }
    return c;
}


Matrix<Rational> operator - (Matrix<Rational> const& a,
                             Matrix<Rational> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size == b.m_row_size && a.m_col_size == b.m_col_size,
            ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<Rational> c(a.m_row_size, a.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < a.m_col_size; j++) {
            c.set(i,j,a.get(i,j) - b.get(i,j));
        }
    }
    return c;
}


Matrix<INT> operator * (Matrix<INT> const& a, Matrix<INT> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size > 0 && a.m_col_size > 0, ("invalid matrix"));
    ASSERTN(b.m_row_size > 0 && b.m_col_size > 0, ("invalid matrix"));
    ASSERTN(a.m_col_size == b.m_row_size, ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<INT> c(a.m_row_size, b.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < b.m_col_size; j++) {
            INT tmp = 0;
            for (UINT k = 0; k < a.m_col_size; k++) {
                tmp = tmp + a.get(i,k) * b.get(k,j);
            }
            c.set(i,j,tmp);
        }
    }
    return c;
}


Matrix<INT> operator + (Matrix<INT> const& a, Matrix<INT> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size == b.m_row_size && a.m_col_size == b.m_col_size,
            ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<INT> c(a.m_row_size, a.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < a.m_col_size; j++) {
            c.set(i,j,a.get(i,j) + b.get(i,j));
        }
    }
    return c;
}


Matrix<INT> operator - (Matrix<INT> const& a, Matrix<INT> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size == b.m_row_size && a.m_col_size == b.m_col_size,
            ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<INT> c(a.m_row_size, a.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < a.m_col_size; j++) {
            c.set(i,j,a.get(i,j) - b.get(i,j));
        }
    }
    return c;
}


Matrix<PRECISION_TYPE> operator * (Matrix<PRECISION_TYPE> const& a,
                                   Matrix<PRECISION_TYPE> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size > 0 && a.m_col_size > 0, ("invalid matrix"));
    ASSERTN(b.m_row_size > 0 && b.m_col_size > 0, ("invalid matrix"));
    ASSERTN(a.m_col_size == b.m_row_size, ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<PRECISION_TYPE> c(a.m_row_size, b.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < b.m_col_size; j++) {
            PRECISION_TYPE tmp = 0;
            for (UINT k = 0; k < a.m_col_size; k++) {
                tmp = tmp + a.get(i,k) * b.get(k,j);
            }
            c.set(i,j,tmp);
        }
    }
    return c;
}


Matrix<PRECISION_TYPE> operator + (Matrix<PRECISION_TYPE> const& a,
                                   Matrix<PRECISION_TYPE> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size == b.m_row_size && a.m_col_size == b.m_col_size,
            ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<PRECISION_TYPE> c(a.m_row_size, a.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < a.m_col_size; j++) {
            c.set(i,j,a.get(i,j) + b.get(i,j));
        }
    }
    return c;
}


Matrix<PRECISION_TYPE> operator - (Matrix<PRECISION_TYPE> const& a,
                                   Matrix<PRECISION_TYPE> const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size == b.m_row_size && a.m_col_size == b.m_col_size,
            ("invalid matrix type of mul"));
    ASSERTN(a.HOOK_INIT == b.HOOK_INIT && a.HOOK_EQUAL == b.HOOK_EQUAL,
            ("strange matrix member function"));
    Matrix<PRECISION_TYPE> c(a.m_row_size, a.m_col_size, &a.m_inhr);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < a.m_col_size; j++) {
            c.set(i,j,a.get(i,j) - b.get(i,j));
        }
    }
    return c;
}


//
//START Rational Matrix
//
static void init_den(Matrix<Rational> * pbasis)
{
    Rational v(0);
    for (UINT i = 0; i < pbasis->getRowSize(); i++) {
        for (UINT j = 0; j < pbasis->getColSize(); j++) {
            pbasis->set(i, j, v);
        }
    }
}


//Rational square root.
static Rational rat_sqrt(Rational)
{
    ASSERTN(0, ("NYI"));
    return 0;
}


static void rmat_dumpf_by_handle(void const* pbasis, FILE * h)
{
    ASSERTN(h, ("dump file handle is NULL"));
    RMat * pthis = (RMat*)pbasis;
    fprintf(h, "\nMATRIX(%dx%d)\n", pthis->getRowSize(), pthis->getColSize());
    for (UINT i = 0; i < pthis->getRowSize(); i++) {
        for (UINT j = 0; j < pthis->getColSize(); j++) {
            Rational rat = pthis->get(i, j);
            CHAR const* blank = "      ";
            if (rat.den() == 1) {
                fprintf(h, "%5d%s", (INT)rat.num(), blank);
            } else if (rat.den() == -1) {
                fprintf(h, "%5d%s", -((INT)rat.num()), blank);
            } else if (rat.num() == 0) {
                if (rat.den() == 1) {
                    fprintf(h, "%5d %s", 0, blank);
                } else {
                    fprintf(h, "%5d/%-5d", (INT)rat.num(), (INT)rat.den());
                }
            } else {
                fprintf(h, "%5d/%-5d", (INT)rat.num(), (INT)rat.den());
            }
        }
        fprintf(h, "\n");
    }
    fprintf(h, "\n");
}


static void rmat_dumpf(void const* pbasis, CHAR const* name, bool is_del)
{
    ASSERT0(name);
    if (is_del) {
        UNLINK(name);
    }

    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    rmat_dumpf_by_handle(pbasis, h);
    fclose(h);
}


#define PRT_COMMA ","
static void rmat_dumps(void const* pbasis)
{
    printf("\n");
    RMat * pthis = (RMat*)pbasis;
    for (UINT i = 0; i < pthis->getRowSize(); i++) {
        for (UINT j = 0; j < pthis->getColSize(); j++) {
            Rational rat = pthis->get(i, j);
            CHAR const* blank = "      ";
            if (rat.den() == 1) {
                printf("%5d%s%s", (INT)rat.num(), PRT_COMMA, blank);
            } else if (rat.den() == -1) {
                printf("%5d%s%s", -((INT)rat.num()), PRT_COMMA, blank);
            } else if (rat.num() == 0) {
                if (rat.den() == 1) {
                    printf("%5d %s%s", 0, PRT_COMMA, blank);
                } else {
                    printf("%5d/%-5d%s",
                           (INT)rat.num(), (INT)rat.den(), PRT_COMMA);
                }
            } else {
                printf("%5d/%-5d%s", (INT)rat.num(), (INT)rat.den(), PRT_COMMA);
            }
        }
        printf("\n");
    }
    printf("\n");
}


RMat::RMat()
{
    m_is_init = false;
    init();
}


//used by template call of T(0) in Vector<Mat>
RMat::RMat(FRAC_TYPE)
{
    RMat();
}


RMat::RMat(RMat const& m) : Matrix<Rational>(m)
{
    m_is_init = true;
    _init_hook();
}


//DO NOT explicitly initialize the base-class copy-constructor.
RMat::RMat(INTMat const& m)
{
    m_is_init = false;
    init(m);
}


RMat::RMat(UINT row, UINT col) : Matrix<Rational>(row, col)
{
    m_is_init = true;
    _init_hook();
    init_den(this);
}


RMat::~RMat()
{
    destroy();
}


void RMat::_init_hook()
{
    INHR i;
    i.hi = init_den;
    i.hs = rat_sqrt;
    i.hds = rmat_dumps;
    i.hdf = rmat_dumpf;
    i.hdfh = rmat_dumpf_by_handle;
    set_hook(&i);
}


void RMat::init()
{
    if (m_is_init) { return; }
    ((Matrix<Rational>*)this)->init(); //Do initialization of base class here.
    m_is_init = true;
    _init_hook();
    init_den(this);
}


void RMat::init(UINT row, UINT col)
{
    if (m_is_init) { return; }
    ((Matrix<Rational>*)this)->init(row, col);
    m_is_init = true;
    _init_hook();
    init_den(this);
}


void RMat::init(RMat const& m)
{
    if (m_is_init) { return; }
    ((Matrix<Rational>*)this)->init();
    m_is_init = true;
    copy(m);
    _init_hook();
}


void RMat::init(INTMat const& m)
{
    if (m_is_init) { return; }
    ((Matrix<Rational>*)this)->init();
    m_is_init = true;
    copy(m);
    _init_hook();
}


void RMat::destroy()
{
    if (!m_is_init) { return; }
    Matrix<Rational>::destroy();
    m_is_init = false;
}


void RMat::getr(UINT row, UINT col, FRAC_TYPE * numer, FRAC_TYPE * denom)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    Rational rat = Matrix<Rational>::get(row, col);
    *numer = rat.num();
    *denom = rat.den();
}


Rational RMat::getr(UINT row, UINT col)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    return Matrix<Rational>::get(row, col);
}


void RMat::sete(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= m_col_size*m_row_size, ("set out of boundary."));
    if (num <= 0) {
        return;
    }
    va_list ptr;
    va_start(ptr, num);
    UINT i = 0;
    UINT row = 0, col = 0;
    while (i < num) {
        UINT numer = (UINT)va_arg(ptr, UINT);
        set(row, col++, numer);
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
        i++;
    }
    va_end(ptr);
}


//Set value to numerator and denomiator.
void RMat::setr(UINT row, UINT col, FRAC_TYPE numer, FRAC_TYPE denom)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(denom != 0, ("denominator is 0!"));
    Rational rat;
    rat.num() = numer;
    rat.den() = denom;
    Matrix<Rational>::set(row, col, rat);
}


void RMat::setr(UINT row, UINT col, Rational rat)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(rat.den() != 0, ("denominator is 0!"));
    Matrix<Rational>::set(row, col, rat);
}


RMat & RMat::operator = (RMat const& m)
{
    ((Matrix<Rational>*)this)->copy(*((Matrix<Rational>*)&m));
    return *this;
}


void RMat::copy(RMat const& r)
{
    ASSERTN(m_is_init && r.m_is_init, ("not yet initialize."));
    Matrix<Rational>::copy(*((Matrix<Rational>*)&r));
}


//Copy elements in Integer Matrix.
void RMat::copy(INTMat const& m)
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    if (this == (RMat*)&m) { return; }
    if (m_mat) {
        ::free(m_mat);
    }
    m_row_size = m.m_row_size;
    m_col_size = m.m_col_size;
    m_mat = (Rational*)::malloc(m_row_size * m_col_size * sizeof(Rational));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            setr(i, j, m.get(i, j), 1);
        }
    }
}


//Return true if matrix is nonsingular, otherwise return false.
bool RMat::inv(OUT RMat & e) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    bool is_nonsingular = Matrix<Rational>::inv(e);
    //for (UINT i = 0; i < m_row_size; i++) {
    //    for (UINT j = 0; j < m_col_size; j++) {
    //        set(i, j, reduce(get(i,j)));
    //    }
    //}
    return is_nonsingular;
}


//Reduce matrix elements to a common denominator.
//Return the common denominator.
//'row': Row to reduce
//'col': The starting column to reduce.
UINT RMat::comden(UINT row, UINT col)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size, ("out of boundary"));
    bool is_int = true; //All elem is integer.
    INT lcm  = 1;
    for (UINT j = col; j < m_col_size; j++) {
            Rational v = get(row, j);
            ASSERTN(v.den() > 0,
                ("should be converted positive in rational file"));
            if (v.num() == 0) {
                continue;
            }
            if (v.den() != 1) {
                lcm = xcom::slcm(lcm, v.den());
                is_int = false;
            }
    }
    if (!is_int) {
        for (UINT j = col; j < m_col_size; j++) {
            Rational v = get(row, j);
            if (v.den() != lcm) {
                INT num = v.num() * (lcm / v.den());
                ASSERTN(num <= 0x7FFFffff, ("integer overflow"));
                setr(row, j, num, lcm);
            }
        }
    }
    return lcm;
}


//Substituting variable 'v' with expression 'exp'.
//'exp': system of equations
//'v': varible id, the column number.
//'is_eq': set to true indicate that each rows of matrix represents
//    an equations, or the false value only represent a right-hand of
//    a linear function,
//    e.g: if 'is_eq' is false, matrix is a vector, [1, 7, -2, -10], that
//    represents the right-hand of
//        f(x) = x1 + 7x2 - 2x3 - 10,
//    and if 'is_eq' is true, matrix repesented an equation,
//        x1 + 7x2 - 2x3  = -10
void RMat::substit(RMat const& exp, UINT v, bool is_eq, INT rhs_idx)
{
    ASSERTN(m_is_init && exp.m_is_init, ("not yet initialize."));
    ASSERTN(m_col_size == exp.m_col_size && v < m_col_size &&
            exp.is_rowvec(), ("unmatch matrix"));

    if (!is_eq) {
        ASSERT0(rhs_idx >= 1 && rhs_idx < (INT)m_col_size);
        mulOfColumns(rhs_idx, m_col_size - 1, -1);
    }
    for (UINT i = 0; i < m_row_size; i++) {
        if (get(i, v) != 0) {
            RMat tmp = exp;
            if (tmp.get(0, v) == 0) {
                continue;
            }
            if (get(i, v) != tmp.get(0, v)) {
                tmp.mul(-get(i, v)/tmp.get(0, v));
            } else {
                tmp.mul(-1);
            }
            addRowToRow(tmp, 0, i);
        }
    }
    if (!is_eq) {
        mulOfColumns(rhs_idx, m_col_size - 1, -1);
    }
}


//elements all be integer
bool RMat::is_imat(UINT * row, UINT * col)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            if (get(i, j).den() != 1) {
                if (row) *row = i;
                if (col) *col = j;
                return false;
            }
        }
    }
    return true;
}


Rational RMat::reduce(UINT row, UINT col)
{
    Rational v = get(row, col);
    v.reduce();
    set(row, col, v);
    return v;
}


void RMat::reduce()
{
    ASSERTN(m_is_init, ("not yet initialize."));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            Rational v = get(i, j);
            v.reduce();
            set(i, j, v);
        }
    }
}


RMat operator * (RMat const& a, RMat const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    RMat c;
    Matrix<Rational> * cp = (Matrix<Rational>*)&c;
    Matrix<Rational> const* ap = (Matrix<Rational> const*)&a;
    Matrix<Rational> const* bp = (Matrix<Rational> const*)&b;
    *cp = *ap * *bp;
    return c;
}


RMat operator + (RMat const& a, RMat const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    RMat c;
    Matrix<Rational> *cp = (Matrix<Rational>*)&c;
    Matrix<Rational> *ap = (Matrix<Rational>*)&a;
    Matrix<Rational> *bp = (Matrix<Rational>*)&b;
    *cp = *ap + *bp;
    return c;
}


RMat operator - (RMat const& a, RMat const& b)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    RMat c;
    Matrix<Rational> *cp = (Matrix<Rational>*)&c;
    Matrix<Rational> *ap = (Matrix<Rational>*)&a;
    Matrix<Rational> *bp = (Matrix<Rational>*)&b;
    *cp = *ap - *bp;
    return c;
}


//Dark Shadow elimination, inequlities form as: Ax <= c,
//and region of x(unknowns) for which gap between upper
//and lower bounds of x is guaranteed to be greater than
//or equals 1.
//e.g: L <= x <= U , to ( L + 1) <= U.
//'c': constant vector.
void RMat::ds(RMat const&)
{
    ASSERTN(m_is_init, ("not yet initialize."));
}


//Converting rational element to integer for row vector.
//'row': number of row to integral
//NOTICE: This function uses row convention.
void RMat::intlize(INT row)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < (INT)m_row_size, ("out of range"));
    if (row != -1) {
        if (comden(row, 0) != 1) {
            for (UINT j = 0; j < m_col_size; j++) {
                setr(row, j, get(row, j).num(), 1);
            }
        }
    } else {
        for (UINT i = 0; i < m_row_size; i++) {
            if (comden(i, 0) != 1) {
                for (UINT j = 0; j < m_col_size; j++) {
                    setr(i, j, get(i, j).num(), 1);
                }
            }
        }
    }
}



//
//Integer Matrix
//
INTMat::INTMat()
{
    m_is_init = false;
    init();
}


//used by template call of T(0) in Vector<Mat>
INTMat::INTMat(INT)
{
    INTMat();
}


INTMat::INTMat(UINT row, UINT col)
{
    m_is_init = false;
    init();
    growRowAndCol(row, col);
}


INTMat::~INTMat()
{
    destroy();
}


void INTMat::init()
{
    if(m_is_init) return;
    ((Matrix<INT>*)this)->init();
    m_is_init = true;
}


void INTMat::destroy()
{
    if(!m_is_init) return;
    m_is_init = false;
}


//Pamaters after 'num' must be integer.
void INTMat::sete(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= m_col_size*m_row_size, ("set out of boundary."));
    if (num <= 0) {
        return;
    }

//#define VARIADIC_PARAMETER_ACCESS
#ifdef VARIADIC_PARAMETER_ACCESS
    //The following algorithm to access variadic parameter may not
    //compatible with current stack layout and stack alignment,
    //says the code work well on Linux with gcc4.8, but
    //it does not work on Windows with VS2015. Therefore
    //a better advise is to use va_arg().
    //INT * ptr = (INT*) (((BYTE*)(&num)) + sizeof(UINT));
    //UINT i = 0;
    //UINT row = 0, col = 0;
    //while (i < num) {
    //    INT numer = *ptr;
    //    set(row, col++, numer);
    //    if (col >= m_col_size) {
    //        row++;
    //        col = 0;
    //    }
    //    i++;
    //    ptr++; //stack growing down
    //}
#else
    va_list ptr;
    va_start(ptr, num);
    UINT row = 0, col = 0;    
    for (UINT i = 0; i < num; i++) {
        INT numer = va_arg(ptr, UINT);
        set(row, col++, numer);
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
    }
    va_end(ptr);
#endif
}


INTMat & INTMat::operator = (INTMat const& m)
{
    ((Matrix<INT>*)this)->copy(*((Matrix<INT>*)&m));
    return *this;
}


//Copy elements in RMat. Convert value type from rational to integral.
void INTMat::copy(RMat const& r)
{
    ASSERTN(m_is_init && r.m_is_init,
            ("not yet initialize."));
    if (this == (INTMat*)&r) return;
    if (m_mat) {
        ::free(m_mat);
    }
    m_row_size = r.m_row_size;
    m_col_size = r.m_col_size;
    m_mat = (INT*)::malloc(m_row_size * m_col_size * sizeof(INT));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            ASSERTN(r.get(i, j).den() == 1, ("illegal rmat"));
            set(i, j, r.get(i, j).num());
        }
    }
}


//Invering of Integer Matrix will be transformed to Rational
//Matrix, and one exception will be thrown if there are some
//element's denomiator is not '1'.
bool INTMat::inv(OUT INTMat & e) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    RMat tmp, v;
    tmp.copy(*this);
    bool is_nonsingular = tmp.inv(tmp);
    e.reinit(m_row_size, m_col_size);
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            Rational v2 = tmp.get(i, j);
            ASSERTN(v2.den() == 1, ("Should converts INTMat to RMat firstly"));
            e.set(i, j, v2.num());
        }
    }
    return is_nonsingular;
}


//Determinant of Integer Matrix will be transformed to Rational
//Matrix, and one exception will be thrown if there are some
//element's denomiator is not '1'.
INT INTMat::det() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    RMat tmp;
    tmp.copy(*this);
    Rational v = tmp.det();
    ASSERTN(v.den() == 1, ("Should converts INTMat to RMat firstly"));
    return v.num();
}


//Generate unimodular matrix to elimnate element.
void INTMat::genElimMat(UINT row, UINT col, OUT INTMat & elim)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    INT aii = get(row, row), aij = get(row, col), x, y;
    INT gcd = exgcd(aii, aij, x, y);
    ASSERTN(gcd == aii*x + aij*y, ("illegal computation"));

    //Construct unimodular to eliminate aij.
    //Satisfied: det(uni) = x * ((x*aii+y*aij)/x*gcd) = 1
    elim.reinit(m_col_size, m_col_size, &m_inhr);
    elim.eye(1);
    elim.set(row, row, x);
    elim.set(col, row, y);
    elim.set(row, col, -aij/gcd);
    elim.set(col, col, aii/gcd);
}


void INTMat::_verify_hnf(INTMat & h)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    for (UINT i = 0; i < MIN(h.m_row_size,h.m_col_size); i++) {
        //1. Eliminate element from i+1 to n to single zero.
        UINT j;
        INT v = h.get(i, i);
        DUMMYUSE(v);

        for (j = 0; j < i; j++) {
            ASSERTN(h.get(i,j) >= 0, ("negtive element"));
            ASSERTN(h.get(i,j) < v, ("large than diagnal element"));
        }
        for (j = i + 1; j < h.m_col_size; j++) {
            ASSERTN(h.get(i,j) == 0, ("should be low triangular"));
        }
    }//end for
}


//Hermite Normal Form decomposition.
//Given a m*n matrix A, there exists an n*n unimodular matrix U and
//an m*n lowtriangular matrix H such that:
//    A = H*inv(U)
//Hermite Normal Form:
//  One possible set of conditions (corresponding to the col
//  convention and making H lower triangular) is given by
//    1. h(ij)=0 for j>i,
//    2. h(ii)>0 for all i, and
//    3. h(ij)<=0 and |h(ij)|<|h(ii)| for j<i
//
//Return true if succ.
//
//h: hermite matrix, it is a lower triangular matrix, row convention.
//u: unimodular matrix, so 'this' and h and u satisfied:
//    h = 'this' * u and
//    'this' = h * inv(u)

//NOTICE:
//  1. 'this' uses row convention.
//  2. 'this' may be singular.
void INTMat::hnf(OUT INTMat & h, OUT INTMat & u)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    u.reinit(m_col_size, m_col_size);
    u.eye(1); //unimodular matrix
    h = *this;

    //Eliminiate non-zero upper diagonal elements.
    for (INT i = 0; i < (INT)MIN(h.m_row_size,h.m_col_size); i++) {
        //1. Eliminate element from i+1 to n to zero.
        INT j;
        for (j = i + 1; j < (INT)h.m_col_size; j++) {
            if (h.get(i, j) != 0) {
                INTMat elim;
                h.genElimMat(i, j, elim);

                //Compounding unimodular postmultiply matrix
                //for each constituent transformation.
                u = u * elim;

                //HNF(notes as H): H=A*U
                h = h * elim;
            }//end if
        }//end for

        //2. Make diagonal element positive.
        if (h.get(i, i) < 0) {
            INTMat neg(h.m_row_size, h.m_col_size);
            neg.eye(1);
            neg.set(i, i, -1);
            h = h * neg;
            u = u * neg;
        }

        //3. Before performing the following operation, the
        //diagonal element must be positive!
        //Make elements below diagonal in row from 0 to i-1 are non-negative.
        //e.g: If aij is neative, and if abs(aij) <= abs(aii),
        //set aij = aij + aii or else set aij = aij + (d+1)*aii,
        //where d is abs(aij/aii).
        for (j = 0; j < i; j++) {
            if (h.get(i, j) < 0) {
                INT v;
                if (abs(h.get(i,j)) <= abs(h.get(i,i))) {
                    v = 1;
                } else {
                    v = abs(h.get(i,j) / h.get(i,i)) + 1;
                }
                INTMat elim(m_col_size, m_col_size); //'this' may be m*n
                elim.eye(1);
                elim.set(i, j, v);
                h = h * elim;
                u = u * elim;
            }
        }

        //4. Reduce below diagonal elements which their value larger
        //than diagonal element in row.
        //Make sure 'elim' is triangular matrix to ensure |det(elim)|=1
        //
        //e.g: If a(i,j) > a(i,i)
        //        d  = a(i,j)/a(i,i)
        //        a(i,j) = a(i,j) + (-d)*a(i,i)
        //Generate 'elim' such as, at row convention.
        //     [1 0 0]
        //     [0 1 0]
        //     [-d 0 1]
        // to reduce element a(i,j) less than a(i,i).
        for (j = 0; j < i; j++) {
            if (h.get(i, j) >= h.get(i, i)) {
                INT d = h.get(i, j) / h.get(i, i); //Get
                INTMat elim(m_col_size, m_col_size);
                elim.eye(1);
                elim.set(i, j, -d);
                h = h * elim;
                u = u * elim;
            }
        }
    } //end for
    _verify_hnf(h);
}


//Reduce matrix by GCD operation.
void INTMat::gcd()
{
    if (getColSize() == 1) return;
    for (UINT i = 0; i < getRowSize(); i++) {
        UINT min = (UINT)-1;
        UINT j;
        bool allzero = true;
        for (j = 0; j < getColSize(); j++) {
            UINT x = abs(get(i, j));
            if (x != 0) {
                min = MIN(min, x);
                allzero = false;
            }
        }
        if (min == 1 || min == 0 || allzero) {
            continue;
        }
        UINT mingcd = min;
        for (j = 0; j < getColSize(); j++) {
            UINT q = abs(get(i, j));
            if (q != 0 && q != mingcd) {
                mingcd = sgcd(mingcd, q);
                if (mingcd == 1) {
                    break;
                }
            }
        }
        if (mingcd == 1) {
            continue;
        }
        for (j = 0; j < getColSize(); j++) {
            set(i, j, get(i, j)/(INT)mingcd);
        }
    }
}


//Find maximum convex hull of a set of 2-dimension points.(Graham scan)
//'c': coordinates of a set of points.
//'idx': 1*n matrix, indices of coordinates of convex hull.
//Note 'this' is a n*2 matrix that each row indicate one coordinate as (x,y).
void INTMat::cvexhull(OUT INTMat & hull)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_col_size == 2, ("need n*2 matrix"));
    INT p0_x = 0, p0_y = 0;
    UINT p0_idx = 0;
    for (UINT i = 0; i < m_row_size; i++) {
        if (i == 0) {
            p0_x = get(i, 0);
            p0_y = get(i, 1);
            p0_idx = i;
        } else if (get(i, 1) < p0_y) { //Get minimum coordinate in y axis.
            p0_x = get(i, 0);
            p0_y = get(i, 1);
            p0_idx = i;
        } else if (get(i, 1) == p0_y) {
            if (get(i, 0) < p0_x) { //Get most left point in x axis.
                p0_x = get(i, 0);
                p0_y = get(i, 1);
                p0_idx = i;
            }
        }
    }

    //Sort points with increasing polar angle, insertion sort.
    List<INT> order;
    for (UINT i = 0; i < m_row_size; i++) {
        if (i == p0_idx) {
            continue;
        }
        bool inserted = false;
        INT next_idx = -1;
        INT idx;
        for (idx = order.get_head(); idx != 0; idx = next_idx) {
            next_idx = order.get_next();
            INT p1_x = get(i, 0) - p0_x;
            INT p1_y = get(i, 1) - p0_y;
            INT p2_x = get(idx - 1, 0) - p0_x;
            INT p2_y = get(idx - 1, 1) - p0_y;
            INT cross =  p1_x * p2_y -  p2_x * p1_y;
            if (cross > 0) {
                //'i' is in deasil order of 'idx',
                //indicate angle of 'i-p0' less than 'idx-p0'.
                order.insert_before(i + 1, idx);
                inserted = true;
                break;
            } else if (cross == 0) {
                //collinear, keep the point most far away from 'p0'
                if (p1_x != 0) {
                    if (abs(p1_x) > abs(p2_x)) { //remove p2.
                        order.insert_before(i + 1, idx);
                        order.remove(idx);
                        inserted = true;
                    } else { //keep p2.
                        inserted = true;
                    }
                } else { //line p1,p2 is perpendicular to x-axis
                    if (abs(p1_y) > abs(p2_y)) { //remove p2.
                        order.insert_before(i + 1, idx);
                        order.remove(idx);
                        inserted = true;
                    } else { //keep p2.
                        inserted = true;
                    }
                }
                if (inserted) {
                    break;
                }
            } //end else ...
        } //end for

        if (!inserted) {
            ASSERTN(idx == 0, ("illegal list"));
            order.append_tail(i + 1); //The first index in list starting at '1'.
        }
    } //end for

    Stack<INT> s;
    s.push(p0_idx + 1);
    s.push(order.get_head_nth(0));
    s.push(order.get_head_nth(1));

    //Processing node in order list.
    for (INT idx = order.get_head_nth(2); idx != 0;
         idx = order.get_next()) {
        INT cross = 0;

        //If vector TOP(1)->idx is not turn left corresponding to
        //TOP(1)->TOP(0), pop the element TOP(0) from stack.
        for (;;) {
            INT p0_idx2 = s.get_top_nth(1);
            INT p1_idx = s.get_top_nth(0);
            p0_x = get(p0_idx2 - 1, 0);
            p0_y = get(p0_idx2 - 1, 1);

            INT p1_x = get(p1_idx - 1, 0) - p0_x;
            INT p1_y = get(p1_idx - 1, 1) - p0_y;
            INT p2_x = get(idx - 1, 0) - p0_x;
            INT p2_y = get(idx - 1, 1) - p0_y;
            cross =  p1_x * p2_y -  p2_x * p1_y;
            if (cross < 0) {
                s.pop();
            } else {
                break;
            }
        }

        ASSERTN(cross > 0, ("exception!"));
        s.push(idx); //point 'idx - 1' turn left.
    }

    //Record index of vertex of hull.
    hull.reinit(1, s.get_elem_count());
    for (INT j = s.get_elem_count() - 1; j >=0; j--) {
        INT idx = s.pop();
        hull.set(0, j,  idx-1);
    }
}


void INTMat::dumps() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    printf("\n");
    for (UINT i = 0; i < m_row_size; i++) {
        printf("\t");
        for (UINT j = 0; j < m_col_size; j++) {
            CHAR const* blank = "           ";
            printf("%5d%s", get(i,j), blank);
        }
        printf("\n");
    }
    printf("\n");
}


void INTMat::dumpf(CHAR const* name, bool is_del) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERT0(name);
    if (is_del) {
        UNLINK(name);
    }
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    fprintf(h, "\nMATRIX(%dx%d)\n", this->getRowSize(), this->getColSize());
    for (UINT i = 0; i < m_row_size; i++) {
        fprintf(h, "\t");
        for (UINT j = 0; j < m_col_size; j++) {
            CHAR const* blank = "           ";
            fprintf(h, "%5d%s", get(i,j), blank);
        }
        fprintf(h, "\n");
    }
    fprintf(h, "\n");
    fclose(h);
}


INTMat operator * (INTMat const& a, INTMat const& b)
{
    INTMat c;
    Matrix<INT> * cp = (Matrix<INT>*)&c;
    Matrix<INT> * ap = (Matrix<INT>*)&a;
    Matrix<INT> * bp = (Matrix<INT>*)&b;
    *cp = *ap * *bp;
    return c;
}


INTMat operator + (INTMat const& a, INTMat const& b)
{
    INTMat c;
    Matrix<INT> * cp = (Matrix<INT>*)&c;
    Matrix<INT> * ap = (Matrix<INT>*)&a;
    Matrix<INT> * bp = (Matrix<INT>*)&b;
    *cp = *ap + *bp;
    return c;
}


INTMat operator - (INTMat const& a, INTMat const& b)
{
    INTMat c;
    Matrix<INT> *cp = (Matrix<INT>*)&c;
    Matrix<INT> *ap = (Matrix<INT>*)&a;
    Matrix<INT> *bp = (Matrix<INT>*)&b;
    *cp = *ap - *bp;
    return c;
}



//
//START Float Matrix, default precision is double.
//
static CHAR const* g_sd_str = "%f";
static void val_adjust(Matrix<Float> * pbasis)
{
    StrBuf buf(64);
    for (UINT i = 0; i < pbasis->getRowSize(); i++) {
        for (UINT j = 0; j < pbasis->getColSize(); j++) {
            buf.sprint(g_sd_str, pbasis->get(i,j).f());
            pbasis->set(i, j, atof(buf.buf));
        }
    }
}


static Float val_sqrt(Float a)
{
    Float v = ::sqrt(a.f());
    return v;
}


static void flt_dumpf_by_handle(void const* pbasis, FILE * h)
{
    ASSERTN(h, ("file handle is NULL"));
    FloatMat * pthis = (FloatMat*)pbasis;
    fprintf(h, "\nMATRIX(%dx%d)\n", pthis->getRowSize(), pthis->getColSize());
    for (UINT i = 0; i < pthis->getRowSize(); i++) {
        fprintf(h, "\t");
        for (UINT j = 0; j < pthis->getColSize(); j++) {
            CHAR const* blank = " ";
            fprintf(h, "%10f%s", pthis->get(i,j).f(), blank);
        }
        fprintf(h, "\n");
    }
    fprintf(h, "\n");
}


static void flt_dumpf(void const* pbasis, CHAR const* name, bool is_del)
{
    ASSERT0(name);
    if (is_del) {
        UNLINK(name);
    }
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    flt_dumpf_by_handle(pbasis, h);
    fclose(h);
}


//Print as real number even though T is integer.
static void flt_dumps(void const* pbasis)
{
    FloatMat * pthis = (FloatMat*)pbasis;
    printf("\n");
    for (UINT i = 0; i < pthis->getRowSize(); i++) {
        printf("\t");
        for (UINT j = 0; j < pthis->getColSize(); j++) {
            CHAR const* blank = "           ";
            printf("%5f%s", pthis->get(i,j).f(), blank);
        }
        printf("\n");
    }
    printf("\n");
}


#ifdef USE_FAST_BUT_LOW_PRECISION_SQRT
static float fast_sqrt_float(float n)
{
    float const f = 1.5f;
    float x = n * 0.5f;
    float y = n;

    //WARNING: dereferencing type-punned pointer will break
    //strict-aliasing rules [-Wstrict-aliasing]
    float * py = &y;
    LONG i = *(LONG*)py;

    i  = 0x5f3759df - (i >> 1);

    //WARNING: dereferencing type-punned pointer will break
    //strict-aliasing rules [-Wstrict-aliasing]
    LONG * pi = &i;
    y  = *(float*)pi;

    //May be need more iters for higher precision?
    y  = y * (f - ( x * y * y ));
    y  = y * (f - ( x * y * y ));
    y  = y * (f - ( x * y * y ));
    return n * y;
}


static Float fast_sqrt(Float n)
{
    return Float(fast_sqrt_float((float)n.f()));
}
#endif


void FloatMat::setSigDigitDesc(UINT sd)
{
    if (HOOK_ADJUST == NULL) {
        INHR i;
        i.he = NULL;
        i.ha = val_adjust;
        i.hs = val_sqrt;
        i.hds = flt_dumps;
        i.hdf = flt_dumpf;
        i.hdfh = flt_dumpf_by_handle;
        set_hook(&i);
    }
    switch (sd) {
    case 0:    g_sd_str = "%.0f"; break;
    case 1:    g_sd_str = "%.1f"; break;
    case 2:    g_sd_str = "%.2f"; break;
    case 3:    g_sd_str = "%.3f"; break;
    case 4:    g_sd_str = "%.4f"; break;
    case 5:    g_sd_str = "%.5f"; break;
    case 6:    g_sd_str = "%.6f"; break;
    default:
        ASSERTN(0, ("unsupport significant digit!"));
    }
}


CHAR const* FloatMat::getSigDigitDesc() const
{
    return g_sd_str;
}


FloatMat::FloatMat()
{
    m_is_init = false;
    init();

}


//used by template call of T(0) in Vector<Mat>
FloatMat::FloatMat(INT)
{
    FloatMat();
}


FloatMat::FloatMat(UINT row, UINT col)
{
    m_is_init = false;
    init();
    growRowAndCol(row, col);
}


FloatMat::~FloatMat()
{
    destroy();
}


void FloatMat::init()
{
    if (m_is_init) { return; }

    ((Matrix<PRECISION_TYPE>*)this)->init();

    INHR i;
    i.he = NULL;
    i.ha = val_adjust;

    #ifdef USE_FAST_BUT_LOW_PRECISION_SQRT
    i.hs = fast_sqrt; //an alternative
    #else
    i.hs = val_sqrt;
    #endif

    i.hds = flt_dumps;
    i.hdf = flt_dumpf;
    i.hdfh = flt_dumpf_by_handle;
    set_hook(&i);
    m_is_init = true;
}


void FloatMat::destroy()
{
    if(!m_is_init) return;
    m_is_init = false;
}


//Set value of elements one by one.
//'num': indicate the number of variant parameters.
//NOTICE:
// Pamaters after 'num' must be float/double.
// e.g: sete(NUM, 2.0, 3.0...)
void FloatMat::sete(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= m_col_size*m_row_size, ("set out of boundary."));
    if (num <= 0) {
        return;
    }
    UINT i = 0;
    UINT row = 0, col = 0;
    BYTE * ptr =(BYTE*) (((BYTE*)(&num)) + sizeof(num));
    while (i < num) {
        PRECISION_TYPE numer = (PRECISION_TYPE)*(double*)ptr;
        Matrix<Float>::set(row, col++, Float(numer));
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
        i++;

        //C++ default regard type of the parameter as double.
        ptr+=sizeof(double); //stack growing down
    }
}


//Set value of elements one by one.
//'num': indicate the number of variant parameters.
//NOTICE:
//  Pamaters after 'num' must be integer.
//  e.g: setie(NUM, 2, 3...)
void FloatMat::setie(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= m_col_size*m_row_size, ("set out of boundary."));
    if (num <= 0) {
        return;
    }
    UINT i = 0;
    UINT row = 0, col = 0;
    BYTE * ptr =(BYTE*) (((BYTE*)(&num)) + sizeof(num));
    while (i < num) {
        INT numer = *(INT*)ptr;
        Matrix<Float>::set(row, col++, Float(numer));
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
        i++;

        //C++ default regard type of the parameter as double.
        ptr += sizeof(INT); //stack growing down
    }
}


FloatMat& FloatMat::operator = (FloatMat const& m)
{
    ((Matrix<PRECISION_TYPE>*)this)->copy(*((Matrix<PRECISION_TYPE>*)&m));
    return *this;
}


void FloatMat::substit(IN FloatMat const& exp, UINT v, bool is_eq, INT rhs_idx)
{
    ASSERTN(m_is_init && exp.m_is_init,
                            ("not yet initialize."));
    ASSERTN(m_col_size == exp.m_col_size && v < m_col_size &&
        exp.is_rowvec(), ("unmatch matrix"));

    if (!is_eq) {
        ASSERT0(rhs_idx >= 1 && rhs_idx < (INT)m_col_size);
        mulOfColumns(rhs_idx, m_col_size - 1, -1);
    }
    for (UINT i = 0; i < m_row_size; i++) {
        if (get(i, v) != 0) {
            FloatMat tmp = exp;
            if (tmp.get(0, v) == 0) {
                continue;
            }
            if (get(i, v) != tmp.get(0, v)) {
                tmp.mul(-get(i, v) / tmp.get(0, v));
            } else {
                tmp.mul(-1);
            }
            addRowToRow(tmp, 0, i);
        }
    }
    if (!is_eq) {
        mulOfColumns(rhs_idx, m_col_size - 1, -1);
    }
}


//All elements are integer
bool FloatMat::is_imat(UINT * row, UINT * col)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            if (!get(i, j).is_int()) {
                if (row) *row = i;
                if (col) *col = j;
                return false;
            }
        }
    }
    return true;
}


FloatMat operator * (FloatMat const& a, FloatMat const& b)
{
    FloatMat c;
    Matrix<PRECISION_TYPE> * cp = (Matrix<PRECISION_TYPE>*)&c;
    Matrix<PRECISION_TYPE> * ap = (Matrix<PRECISION_TYPE>*)&a;
    Matrix<PRECISION_TYPE> * bp = (Matrix<PRECISION_TYPE>*)&b;
    *cp = *ap * *bp;
    return c;
}


FloatMat operator - (FloatMat const& a, FloatMat const& b)
{
    FloatMat c;
    Matrix<PRECISION_TYPE> * cp = (Matrix<PRECISION_TYPE>*)&c;
    Matrix<PRECISION_TYPE> * ap = (Matrix<PRECISION_TYPE>*)&a;
    Matrix<PRECISION_TYPE> * bp = (Matrix<PRECISION_TYPE>*)&b;
    *cp = *ap - *bp;
    return c;
}



//
//Boolean Matrix
//
static void bool_dumpf_by_handle(void const* pbasis, FILE * h)
{
    ASSERTN(h != NULL, ("file handle is NULL"));
    BMat * pthis = (BMat*)pbasis;
    fprintf(h, "\nMATRIX(%dx%d)\n", pthis->getRowSize(), pthis->getColSize());
    for (UINT i = 0; i < pthis->getRowSize(); i++) {
        fprintf(h, "\t");
        for (UINT j = 0; j < pthis->getColSize(); j++) {
            CHAR const* blank = "           ";
            fprintf(h, "%10d%s", pthis->get(i,j), blank);
        }
        fprintf(h, "\n");
    }
    fprintf(h, "\n");
}


static void bool_dumpf(void const* pbasis, CHAR const* name, bool is_del)
{
    ASSERT0(name);
    if (is_del) {
        UNLINK(name);
    }
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));
    bool_dumpf_by_handle(pbasis, h);
    fclose(h);
}


//Print as real number even though T is integer.
static void bool_dumps(void const* pbasis)
{
    BMat * pthis = (BMat*)pbasis;
    printf("\n");
    for (UINT i = 0; i < pthis->getRowSize(); i++) {
        printf("\t");
        for (UINT j = 0; j < pthis->getColSize(); j++) {
            CHAR const* blank = "           ";
            printf("%5d%s", pthis->get(i,j), blank);
        }
        printf("\n");
    }
    printf("\n");
}


BMat::BMat()
{
    m_is_init = false;
    init();
}


//used by template call of T(0) in Vector<Mat>
BMat::BMat(INT)
{
    BMat();
}


BMat::BMat(UINT row, UINT col)
{
    m_is_init = false;
    init();
    growRowAndCol(row, col);
}


BMat::~BMat()
{
    destroy();
}


void BMat::init()
{
    if (m_is_init) return;
    ((Matrix<bool>*)this)->init();
    INHR i;
    i.hds = bool_dumps;
    i.hdf = bool_dumpf;
    i.hdfh = bool_dumpf_by_handle;
    set_hook(&i);
    m_is_init = true;
}


void BMat::destroy()
{
    if(!m_is_init) return;
    m_is_init = false;
}


//Assignment value of matrix element
//e.g: sete(3, true, true, false)
void BMat::sete(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= m_col_size*m_row_size, ("set out of boundary."));
    if (num <= 0) {
        return;
    }
    UINT i = 0;
    UINT row = 0, col = 0;
    bool * ptr = (bool*)(((BYTE*)(&num)) + sizeof(num));
    while (i < num) {
        set(row, col++, *ptr);
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
        i++;

        //Default regard type of the parameter as double.
        ptr++; //stack growing down
    }
}


BMat& BMat::operator = (BMat const& m)
{
    ((Matrix<bool>*)this)->copy(*((Matrix<bool>*)&m));
    return *this;
}

} //namespace xcom
