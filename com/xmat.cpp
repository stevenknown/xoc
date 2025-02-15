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
#include "math.h"
#include "xcominc.h"

namespace xcom {

static bool g_is_dump_comma = true;


#ifdef USE_FAST_BUT_LOW_PRECISION_SQRT
static Float fast_sqrt(Float const& n)
{
    return Float(xsqrtNonIter((float)n.val()));
}
#endif

//
//START Rational Matrix
//
void RMat::dumpfh(FILE * h) const
{
    ASSERTN(h, ("dump file handle is nullptr"));
    fprintf(h, "\nMATRIX(%dx%d)\n", getRowSize(), getColSize());
    StrBuf strbuf(32);
    UINT elemprtlen = 7;
    for (UINT i = 0; i < getRowSize(); i++) {
        for (UINT j = 0; j < getColSize(); j++) {
            Rational rat = get(i, j);
            strbuf.clean();
            if (rat.den() == 1) {
                strbuf.strcat("%d", (INT)rat.num());
            } else if (rat.den() == -1) {
                strbuf.strcat("%5d", -((INT)rat.num()));
            } else if (rat.num() == 0) {
                if (rat.den() == 1) {
                    strbuf.strcat("0");
                } else {
                    strbuf.strcat("%5d/%-5d", (INT)rat.num(), (INT)rat.den());
                }
            } else {
                strbuf.strcat("%5d/%-5d", (INT)rat.num(), (INT)rat.den());
            }
            if (g_is_dump_comma) {
                strbuf.strcat(",");
            }
            padTo(strbuf, elemprtlen);
            fprintf(h, "%s", strbuf.buf);
        }
        fprintf(h, "\n");
    }
    fprintf(h, "\n");
}


void RMat::dumpf(CHAR const* name, bool is_del) const
{
    ASSERT0(name);
    FileObj fo(name, is_del);
    dumpfh(fo.getFileHandler());
}


void RMat::dumps() const
{
    printf("\n");
    dumpfh(stdout);
    printf("\n");
}


void RMat::init()
{
    if (m_is_init) { return; }
    Matrix<Rational>::init();
}


void RMat::init(UINT row, UINT col)
{
    if (m_is_init) { return; }
    Matrix<Rational>::init(row, col);
}


void RMat::init(RMat const& m)
{
    if (m_is_init) { return; }
    Matrix<Rational>::init();
    copy(m);
}


void RMat::init(IMat const& m)
{
    if (m_is_init) { return; }
    Matrix<Rational>::init();
    copy(m);
}


void RMat::destroy()
{
    if (!m_is_init) { return; }
    Matrix<Rational>::destroy();
}


void RMat::getr(UINT row, UINT col, Rational::FType * numer,
                Rational::FType * denom)
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
    ASSERTN(num <= getSize(), ("out of range"));
    if (num == 0) { return; }
    UINT row = 0, col = 0;
    va_list ptr;
    va_start(ptr, num);
    for (UINT i = 0; i < num; i++) {
        EType numer = va_arg(ptr, Rational::FType);
        set(row, col++, numer);
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
    }
    va_end(ptr);
}


//Set value to numerator and denomiator.
void RMat::setr(UINT row, UINT col,
                Rational::FType numer, Rational::FType denom)
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
    Matrix<Rational>::copy(*((Matrix<Rational>*)&m));
    return *this;
}


void RMat::copy(RMat const& m)
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    Matrix<Rational>::copy(*((Matrix<Rational>*)&m));
}


//Copy elements in Integer Matrix.
void RMat::copy(IMat const& m)
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    if (this == (RMat*)&m) { return; }
    if (m.getSize() == 0) {
        deleteAllElem();
        return;
    }
    freeMatBuf();
    m_row_size = m.m_row_size;
    m_col_size = m.m_col_size;
    allocMatBuf(m_row_size * m_col_size);
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            setr(i, j, m.get(i, j), 1);
        }
    }
}


//Reduce matrix elements to a common denominator.
//Return the common denominator.
//row: Row to reduce
//col: The starting column to reduce.
UINT RMat::comden(UINT row, UINT col)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size, ("out of boundary"));
    bool is_int = true; //All elem is integer.
    INT lcm = 1;
    for (UINT j = col; j < m_col_size; j++) {
        Rational v = get(row, j);
        ASSERTN(v.den() > 0,
                ("should be converted positive in rational file"));
        if (v.num() == 0) { continue; }
        if (v.den() == 1) { continue; }
        lcm = xcom::slcm(lcm, v.den());
        is_int = false;
    }
    if (is_int) { return lcm; }
    for (UINT j = col; j < m_col_size; j++) {
        Rational v = get(row, j);
        if (v.den() == lcm) { continue; }
        INT num = v.num() * (lcm / v.den());
        ASSERTN(num <= computeUnsignedMaxValue<INT>(sizeof(INT)),
                ("integer overflow"));
        setr(row, j, num, lcm);
    }
    return lcm;
}


//Substitute variable 'v' with expression 'exp'.
//exp: system of equations
//v: varible id, the column number.
//is_eq: set to true indicate that each rows of matrix represents
//    an equations, or the false value only represent a right-hand of
//    a linear function,
//    e.g: if 'is_eq' is false, matrix is a vector, [1, 7, -2, -10], that
//    represents the right-hand of
//        f(x) = x1 + 7x2 - 2x3 - 10,
//    and if 'is_eq' is true, matrix repesented an equation,
//        x1 + 7x2 - 2x3  = -10
void RMat::substit(RMat const& exp, UINT v, bool is_eq, INT cst_col)
{
    ASSERTN(m_is_init && exp.m_is_init, ("not yet initialize."));
    ASSERTN(m_col_size == exp.m_col_size && v < m_col_size &&
            exp.is_rowvec(), ("unmatch matrix"));

    if (!is_eq) {
        ASSERT0(cst_col >= 1 && cst_col < (INT)m_col_size);
        mulOfColumns(cst_col, m_col_size - 1, -1);
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
        mulOfColumns(cst_col, m_col_size - 1, -1);
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


//Dark Shadow elimination, inequlities form as: Ax <= c,
//and region of x(unknowns) for which gap between upper
//and lower bounds of x is guaranteed to be greater than
//or equals 1.
//e.g: L <= x <= U , to ( L + 1) <= U.
//c: constant vector.
void RMat::ds(RMat const&)
{
    ASSERTN(m_is_init, ("not yet initialize."));
}


//Converting rational element to integer for row vector.
//row: number of row to integral
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
IMat & IMat::operator = (IMat const& m)
{
    Matrix<INT>::copy(*((Matrix<INT>*)&m));
    return *this;
}


//Copy elements in RMat. Convert value type from rational to integral.
void IMat::copy(RMat const& r)
{
    ASSERTN(m_is_init && r.m_is_init, ("not yet initialize."));
    if (this == (IMat*)&r) { return; }
    if (r.getSize() == 0) {
        deleteAllElem();
        return;
    }
    freeMatBuf();
    m_row_size = r.m_row_size;
    m_col_size = r.m_col_size;
    allocMatBuf(m_row_size * m_col_size);
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
bool IMat::inv(OUT IMat & e, RMatMgr & mgr) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    RMat tmp, v;
    tmp.copy(*this);
    bool is_nonsingular = tmp.inv(tmp, mgr);
    e.reinit(m_row_size, m_col_size);
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            Rational v2 = tmp.get(i, j);
            ASSERTN(v2.den() == 1, ("Should converts IMat to RMat firstly"));
            e.set(i, j, v2.num());
        }
    }
    return is_nonsingular;
}


//Determinant of Integer Matrix will be transformed to Rational
//Matrix, and one exception will be thrown if there are some
//element's denomiator is not '1'.
INT IMat::det(MOD RMatMgr & mgr) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    RMat tmp;
    tmp.copy(*this);
    Rational v = tmp.det(mgr);
    ASSERTN(v.den() == 1, ("Should converts IMat to RMat firstly"));
    return v.num();
}


//Generate unimodular matrix to elimnate element.
void IMat::genElimMat(UINT row, UINT col, OUT IMat & elim)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    INT aii = get(row, row), aij = get(row, col), x, y;
    INT gcd = exgcd(aii, aij, x, y);
    ASSERTN(gcd == aii*x + aij*y, ("illegal computation"));

    //Construct unimodular to eliminate aij.
    //Satisfied: det(uni) = x * ((x*aii+y*aij)/x*gcd) = 1
    elim.reinit(m_col_size, m_col_size);
    elim.initIden(1);
    elim.set(row, row, x);
    elim.set(col, row, y);
    elim.set(row, col, -aij/gcd);
    elim.set(col, col, aii/gcd);
}


void IMat::_verify_hnf(IMat & h) const
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
//h: hermite matrix, it is a lower triangular matrix, row convention.
//u: unimodular matrix, so 'this' and h and u satisfied:
//    h = 'this' * u and
//    'this' = h * inv(u)

//NOTICE:
//  1. 'this' uses row convention.
//  2. 'this' may be singular.
void IMat::hnf(OUT IMat & h, OUT IMat & u, MOD IMatMgr & mgr) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    u.reinit(m_col_size, m_col_size);
    u.initIden(1); //unimodular matrix
    h = *this;

    //Eliminiate non-zero upper diagonal elements.
    for (INT i = 0; i < (INT)MIN(h.m_row_size,h.m_col_size); i++) {
        //1. Eliminate element from i+1 to n to zero.
        INT j;
        for (j = i + 1; j < (INT)h.m_col_size; j++) {
            if (h.get(i, j) != 0) {
                IMat elim;
                h.genElimMat(i, j, elim);

                //Compounding unimodular postmultiply matrix
                //for each constituent transformation.
                IMat::mul(u, elim, u, mgr);

                //HNF(notes as H): H=A*U
                IMat::mul(h, elim, h, mgr);
            }
        }

        //2. Make diagonal element positive.
        if (h.get(i, i) < 0) {
            IMat neg(h.m_row_size, h.m_col_size);
            neg.initIden(1);
            neg.set(i, i, -1);
            IMat::mul(h, neg, h, mgr);
            IMat::mul(u, neg, u, mgr);
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
                IMat elim(m_col_size, m_col_size); //'this' may be m*n
                elim.initIden(1);
                elim.set(i, j, v);
                IMat::mul(h, elim, h, mgr);
                IMat::mul(u, elim, u, mgr);
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
                IMat elim(m_col_size, m_col_size);
                elim.initIden(1);
                elim.set(i, j, -d);
                IMat::mul(h, elim, h, mgr);
                IMat::mul(u, elim, u, mgr);
            }
        }
    }
    _verify_hnf(h);
}


//Reduce matrix by GCD operation.
void IMat::gcd()
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
//: coordinates of a set of points.
//idx: 1*n matrix, indices of coordinates of convex hull.
//Note 'this' is a n*2 matrix that each row indicate one coordinate as (x,y).
void IMat::cvexhull(OUT IMat & hull)
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


void IMat::dumps() const
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


void IMat::dumpf(CHAR const* name, bool is_del) const
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


//
//START Float Matrix, default precision is double.
//
static CHAR const* g_sd_str = "%f";
void FMat::adjust()
{
    StrBuf buf(64);
    for (UINT i = 0; i < getRowSize(); i++) {
        for (UINT j = 0; j < getColSize(); j++) {
            buf.sprint(g_sd_str, get(i,j).val());
            set(i, j, ::atof(buf.buf));
        }
    }
}


Float FMat::sqrtElem(Float a)
{
    #ifdef USE_FAST_BUT_LOW_PRECISION_SQRT
    return fast_sqrt(a);
    #endif
    return ::sqrt(a.val());
}


void FMat::dumpfh(FILE * h) const
{
    ASSERTN(h, ("file handle is nullptr"));
    fprintf(h, "\nMATRIX(%dx%d)\n", getRowSize(), getColSize());
    for (UINT i = 0; i < getRowSize(); i++) {
        fprintf(h, "\t");
        for (UINT j = 0; j < getColSize(); j++) {
            CHAR const* blank = " ";
            fprintf(h, "%10f%s", get(i,j).val(), blank);
        }
        fprintf(h, "\n");
    }
    fprintf(h, "\n");
}


void FMat::dumpf(CHAR const* name, bool is_del) const
{
    FileObj fo(name, is_del);
    dumpfh(fo.getFileHandler());
}


//Print as real number even though T is integer.
void FMat::dumps() const
{
    printf("\n");
    for (UINT i = 0; i < getRowSize(); i++) {
        printf("\t");
        for (UINT j = 0; j < getColSize(); j++) {
            CHAR const* blank = "           ";
            printf("%5f%s", get(i,j).val(), blank);
        }
        printf("\n");
    }
    printf("\n");
}


void FMat::setSigDigitDesc(UINT sd)
{
    switch (sd) {
    case 0: g_sd_str = "%.0f"; break;
    case 1: g_sd_str = "%.1f"; break;
    case 2: g_sd_str = "%.2f"; break;
    case 3: g_sd_str = "%.3f"; break;
    case 4: g_sd_str = "%.4f"; break;
    case 5: g_sd_str = "%.5f"; break;
    case 6: g_sd_str = "%.6f"; break;
    default: ASSERTN(0, ("unsupport significant digit!"));
    }
}


CHAR const* FMat::getSigDigitDesc() const
{
    return g_sd_str;
}


//Set value of elements one by one.
//num: indicate the number of variant parameters.
//NOTICE:
// Arguments after 'num' must be float/double.
// e.g: sete(NUM, 2.0, 3.0...)
void FMat::sete(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= getSize(), ("out of boundary."));
    if (num == 0) { return; }
    UINT row = 0, col = 0;
    va_list ptr;
    va_start(ptr, num);
    for (UINT i = 0; i < num; i++) {
        //In C++ specification, FP immediate's type is regarded as 'double'.
        Matrix<Float>::set(row, col++, Float(va_arg(ptr, EType)));
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
    }
    va_end(ptr);
}


//Set value of elements one by one.
//num: indicate the number of variant parameters.
//NOTICE:
//  Pamaters after 'num' must be integer.
//  e.g: setie(NUM, 2, 3...)
void FMat::setie(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= getSize(), ("out of boundary."));
    if (num == 0) { return; }
    UINT i = 0;
    UINT row = 0, col = 0;
    BYTE * ptr = (BYTE*)(((BYTE*)(&num)) + sizeof(num));
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


FMat& FMat::operator = (FMat const& m)
{
    Matrix<Float>::copy(*((Matrix<Float>*)&m));
    return *this;
}


void FMat::substit(IN FMat const& exp, UINT v, bool is_eq, INT cst_col)
{
    ASSERTN(m_is_init && exp.m_is_init, ("not yet initialize."));
    ASSERTN(m_col_size == exp.m_col_size && v < m_col_size &&
            exp.is_rowvec(), ("unmatch matrix"));
    if (!is_eq) {
        ASSERT0(cst_col >= 1 && cst_col < (INT)m_col_size);
        mulOfColumns(cst_col, m_col_size - 1, -1);
    }
    for (UINT i = 0; i < m_row_size; i++) {
        if (get(i, v) != Float(0)) {
            FMat tmp = exp;
            if (tmp.get(0, v) == Float(0)) { continue; }
            if (get(i, v) != tmp.get(0, v)) {
                tmp.mul(-get(i, v) / tmp.get(0, v));
            } else {
                tmp.mul(-1);
            }
            addRowToRow(tmp, 0, i);
        }
    }
    if (!is_eq) {
        mulOfColumns(cst_col, m_col_size - 1, -1);
    }
}


//All elements are integer
bool FMat::is_imat(UINT * row, UINT * col)
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


//
//Boolean Matrix
//
void BMat::dumpfh(FILE * h) const
{
    ASSERTN(h, ("file handle is nullptr"));
    fprintf(h, "\nMATRIX(%dx%d)\n", getRowSize(), getColSize());
    for (UINT i = 0; i < getRowSize(); i++) {
        fprintf(h, "\t");
        for (UINT j = 0; j < getColSize(); j++) {
            CHAR const* blank = "           ";
            fprintf(h, "%10d%s", get(i,j), blank);
        }
        fprintf(h, "\n");
    }
    fprintf(h, "\n");
}


void BMat::dumpf(CHAR const* name, bool is_del) const
{
    FileObj fo(name, is_del);
    dumpfh(fo.getFileHandler());
}


void BMat::dumps() const
{
    printf("\n");
    for (UINT i = 0; i < getRowSize(); i++) {
        printf("\t");
        for (UINT j = 0; j < getColSize(); j++) {
            CHAR const* blank = "           ";
            printf("%5d%s", get(i,j), blank);
        }
        printf("\n");
    }
    printf("\n");
}


//Assignment value of matrix element
//e.g: sete(3, true, true, false)
void BMat::sete(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= getSize(), ("out of boundary."));
    if (num == 0) { return; }
    UINT row = 0, col = 0;
    va_list ptr;
    va_start(ptr, num);
    for (UINT i = 0; i < num; i++) {
        //Note 'bool' is promoted to 'int' when passed through '...'.
        BOOL numer = va_arg(ptr, INT) == 0 ? false : true;
        set(row, col++, numer);
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
    }
    va_end(ptr);
}


BMat& BMat::operator = (BMat const& m)
{
    Matrix<BOOL>::copy(*((Matrix<BOOL>*)&m));
    return *this;
}

} //namespace xcom
