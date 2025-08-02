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
#ifndef __MATT_H__
#define __MATT_H__

#define MATRIX_DUMP_FILE_NAME "dumpmatrix.tmp"

namespace xcom {

//The file defines classes to operate on matrix template.

template <class T> class MatMgr;
template <class T> class Matrix;
template <class T> class MatWrap;

#define CST_COL_UNDEF -1

//
//START Matrix
//
//NOTE:
//We abstract these objects which have same operations
//in '+' '-' '*' '/' '<' '<=' '>' '>=' '!=' '==' 'minus'.
//These objects may be integer, real number, complex number, vector,
//matrix, etc.

#define MAX_MAT_RANK 512
#define MAX_QR_ITER_NUM 512
#define NORM_1 1 //1 norm
#define NORM_2 2 //2 norm
#define NORM_F 3 //Frobenius norm
#define NORM_INF 4 //Infinite norm
//#define QR_ITER_WITH_SHIFT //low precision

//
//START MatWrap
//
template <class T> class MatWrap {
protected:
    Matrix<T> * m_mat;
    MatMgr<T> & m_mgr;
public:
    MatWrap(MatMgr<T> & mgr);
    MatWrap(Matrix<T> const& src, MatMgr<T> & mgr);
    MatWrap(UINT row, UINT col, MatMgr<T> & mgr);
    ~MatWrap();

    //Return the reference to matrix.
    Matrix<T> & m() { return *m_mat; }
    Matrix<T> & m() const { return *m_mat; }

    //Return the pointer to matrix.
    Matrix<T> * mp() const { return m_mat; }
};
//END MatWrap


//Matrix use hooks to diverge customized function. The strategy allows Matrix
//object could be allocated in mempool.
template <class T> class Matrix {
#ifdef _DEBUG_
public:
    UINT m_id; //only used for debug mode.
    UINT id() const { return m_id; }
    bool setId(UINT i) { m_id = i; return true; }
#endif
public:
    typedef T EType;
    //Matrix allows copy-constructor.
protected:
    //Allocate matrix element buffer.
    void allocMatBuf(UINT elemnum)
    { ASSERT0(m_mat == nullptr && elemnum > 0); m_mat = allocTBuf(elemnum); }

    //Allocate a buffer of type T that contains 'elemnum' elements.
    //Note T may have constructor, thus we invoke the constructor through
    //'new' for each element in the buffer.
    T * allocTBuf(UINT elemnum) { return new T[elemnum]; }

    //The interface is used to copy a batch of elements from buffer 'src' to
    //the buffer 'dst'.
    //Overload the function if there is a specific initialization for type T.
    virtual void copyTBuf(T * dst, T const* src, UINT elemnum)
    {
        //T may have constructor, thus invoke the copy-constructor one by
        //one by default. Note it will be slowly.
        for (UINT i = 0; i < elemnum; i++) {
            dst[i] = src[i];
        }
    }

    //The interface is used to clean (NOT free) given matrix element buffer.
    //Overload the function if there is a specific initialization for type T.
    virtual void cleanTBuf(T * dst, UINT elemnum)
    {
        //T may have constructor, thus invoke the copy-constructor one by
        //one by default. Note it will be slowly.
        for (UINT i = 0; i < elemnum; i++) {
            dst[i] = 0;
        }
    }

    void dumpInd(StrBuf & buf, UINT ind) const
    { for (UINT i = 0; i < ind; i++) { buf.strcat(" "); } }

    //The interface is used to compare two given matrix element, and it always
    //be used in any interior element equality comparison.
    //Overload the function if there is a specific initialization for type T.
    virtual bool equalElem(T const& a, T const& b) const { return a == b; }

    //Free a buffer of type T.
    //Note T may have destructor, thus we invoke the destructor through
    //'delete' for each element in the buffer.
    void freeTBuf(T * buf) { ASSERT0(buf); delete [] buf; }
    void freeMatBuf()
    {
        if (m_mat == nullptr) { return; }
        freeTBuf(m_mat);
        m_mat = nullptr;
    }

    //The function constructs matrix element buffer via given T buffer.
    //tbuf: input T buffer.
    //tbufelemnum: the number of element in 'tbuf'.
    void initMatBufWith(T const tbuf[], UINT tbufelemnum)
    {
        ASSERTN(getSize() == 0 && m_mat == nullptr, ("matrix should be empty"));
        allocMatBuf(tbufelemnum);
        copyTBuf(m_mat, tbuf, tbufelemnum);
    }

    //The function is used to pad blank when doing matrix dump.
    void padTo(MOD StrBuf & buf, UINT len) const;

    void reallocMatBufWith(T const* buf, UINT elemnum)
    {
        if (elemnum == 0) { return; }
        ASSERT0(is_init());
        //Use different buffer copy strategy than reallocMatBuf().
        UINT sz = getSize();
        if (sz >= elemnum) {
            copyTBuf(m_mat, buf, elemnum);
            return;
        }
        freeMatBuf();
        allocMatBuf(elemnum);
        copyTBuf(m_mat, buf, elemnum);
    }
    void reallocMatBuf(UINT elemnum)
    {
        if (elemnum == 0) { return; }
        ASSERT0(is_init());
        UINT sz = getSize();
        if (sz >= elemnum) {
            return;
        }
        T * tmp_mat = allocTBuf(elemnum);
        if (sz != 0) {
            ASSERT0(m_mat);
            copyTBuf(tmp_mat, m_mat, sz);
            freeMatBuf();
        }
        m_mat = tmp_mat;
    }

    //Interior function.
    //The function is used to compute the sqrt for given element.
    //Here only apply an instantiate that simpliest call C/C++ library.
    //If it is inappropriate, overloading it as your need.
    virtual T sqrtElem(T)
    { ASSERTN(0, ("Target Dependent Code")); return T(0); }
public:
    bool m_is_init;
    UINT m_row_size; //record row size
    UINT m_col_size; //record column size
    T * m_mat; //record element in matrix
public:
    //Laplace extension.
    T FullPermutation(UINT n);

    //Helper function of fully permutation.
    //v: current digital that need chooses one slot in buf.
    //posbuf: slot buf.
    //posbufnum: number of slot.
    //n: natural number.
    void FullPermutationRecur(UINT v, UINT * posbuf,
                              UINT posbufnum, UINT n, T & det);
    UINT ReverseOrderNumber(UINT * numbuf, UINT numlen);
public:
    Matrix()
    {
        m_is_init = false;
        init();
    }
    Matrix(Matrix<T> const& m)
    {
        m_is_init = false;
        init();
        copy(m);
    }
    Matrix(UINT row, UINT col)
    {
        m_is_init = false;
        init(row, col);
    }
    virtual ~Matrix() { destroy(); }

    T abs(T v) const { return v < T(0) ? v = -(v) : v; }

    //Calculate Algebraic Complement of element A(row,col).
    void algc(UINT row, UINT col, Matrix<T> & m);

    //Calculate classical Adjoint Matrix.
    bool adj(Matrix<T> & m, MatMgr<T> & mgr);

    //Matrix Addition.
    //Note the function supports in-place operation.
    //e.g:res = a + b;
    static Matrix<T> & add(Matrix<T> const& a, Matrix<T> const& b,
                           OUT Matrix<T> & res);

    //Add row 'from' to row 'to', then row 'to' be modified
    void addRowToRow(UINT from, UINT to);

    //Add one row of 'm' to one row of 'this'. Row 'to' changed.
    void addRowToRow(Matrix<T> const& m, UINT mfrom, UINT to);

    //Add one column to another column. Column 'to' changed.
    void addColumnToColumn(UINT from, UINT to);

    //Add one column of 'm' to one column of 'this'. Column 'to' changed.
    void addColumnToColumn(Matrix<T> const& m, UINT mfrom, UINT to);

    //The function performs error analysis and elimination.
    //Adjust significant digit and revising value.
    //Inherit class should override the function if it has numberical error.
    virtual void adjust() { ASSERTN(0, ("Target Dependent Code")); }

    //Calculate Basis of rows vector.
    //'b': use row convention. Each row indicate one basis
    //NOTE: 'this' uses row convention.
    void basis(OUT Matrix<T> & b, MOD MatMgr<T> & mgr);

    void copy(Matrix<T> const& m);
    void copy(MatWrap<T> const& w) { copy(w.m()); }

    //Clean row.
    void cleanRow(UINT row) { setRow(row, T(0)); }

    //Clean column.
    void cleanCol(UINT col) { setCol(col, T(0)); }

    //Count memory usage for current object.
    size_t count_mem() const;

    //Calculate condition number for Frobenius Norm.
    //k(A) = norm(A) * norm(inv(A))
    //Return true if 'this' is nonsingular matrix.
    bool cond(T & c, MatMgr<T> & mgr, UINT p = NORM_INF);

    //Compute vector product.
    //Vector Product(cross product).
    //Given two vector [a1, b1, c1], [a2, b2, c2], the vector product is:
    //  [b1*c2-c1*b2, c1*a2-a1*c2, a1*b2-b1*a2].
    //v: v must be row/col vector with three dimensions.
    //u: cross product.
    void cross(MOD Matrix<T> & v, OUT Matrix<T> & u);

    //The function will destroy all elements in matrix.
    void deleteAllElem()
    {
        if (!m_is_init) { return; }
        m_row_size = 0;
        m_col_size = 0;
        freeMatBuf();
    }

    //The function will destroy whole matrix object.
    void destroy()
    {
        if (!m_is_init) { return; }
        deleteAllElem();
        m_is_init = false;
    }

    //Calculate determinant result, only consider square matrix.
    //Fast method:
    //  det(A) = (-1)^r * det(U),r is the number of times of interchanging.
    //  U is upper triangular matrix.
    T det(MOD MatMgr<T> & mgr) const;
    void deleteRow(UINT row)
    {
        ASSERTN(m_is_init, ("not yet initialize."));
        deleteRow(row, row);
    }
    void deleteRow(UINT from, UINT to);
    void deleteCol(UINT col)
    {
        ASSERTN(m_is_init, ("not yet initialize."));
        deleteCol(col, col);
    }
    void deleteCol(UINT from, UINT to);

    //Compute Dot Product.
    //v: v must be a row/col vector.
    T dot(Matrix<T> const& v) const;

    //Compute the dot production.
    //srow: starting row
    //scol: starting column
    //erow: ending row
    //ecol: ending column
    //v: value matrix, must be a row/col vector.
    //NOTE: 'srow, scol, erow, ecol'  must express one vector.
    T dot(UINT srow, UINT scol, UINT erow,
          UINT ecol, Matrix<T> const& v) const;

    //Compute Dot Product between two matrix.
    //srow: starting row
    //scol:starting column
    //erow:end row
    //ecol:end column.
    //v: v must be a row/col vector.
    //NOTE 'srow, scol, erow, ecol' must express one vector.
    T dot(UINT srow, UINT scol, UINT erow, UINT ecol, Matrix<T> const& v,
          UINT vsrow, UINT vscol, UINT verow, UINT vecol) const;

    //Compute Dot Product between referred 'row' and 'v'.
    T dotrow(UINT row, Matrix<T> const& v) const;

    //Compute Dot Product between referred 'col' and 'v'.
    T dotcol(UINT col, Matrix<T> const& v) const;

    //Diagonalize matrix, form as A = P*D*inv(P), P is eigen space of eahc
    //eigen value.
    //D is diagonal matrix that diagonal entry is eigen value.
    //Return true if it is diagonalizable, otherwise return false.
    //p: eigen space matrix, each column correspond to related eigen value.
    //   It uses col convention..
    //d: eigen value matrix. It was diagonally.
    //NOTE: matrix use row convention.
    bool diag(OUT Matrix<T> & p, OUT Matrix<T> & d, MOD MatMgr<T> & mgr);

    //Print matrix element to file.
    virtual void dumpf(FILE * h) const;
    virtual void dumpf(CHAR const* name = MATRIX_DUMP_FILE_NAME,
                       bool is_del = false) const;

    //Dump matrix element to buffer.
    virtual void dumpb(OUT StrBuf & buf, UINT indent) const;

    //Print matrix element on screen.
    virtual void dumps() const;
    virtual void dumpT2S(T const&) const
    { ASSERTN(0, ("Target Dependent Code")); }

    //The function is used to dump 't' into 'buf'.
    //Note 'buf' can be cleaned entirely.
    virtual void dumpT2Buf(T const&, OUT StrBuf &) const
    { ASSERTN(0, ("Target Dependent Code")); }

    //Computation of eigenvalues.
    //Eigenvalues and eigenvectors.
    //eigv: is a vector containing the eigenvalues of a square matrix.
    //NOTE: matrix must be row-vector matrix.
    void eig(OUT Matrix<T> & eigv, MOD MatMgr<T> & mgr);

    //Computation of eigenvalues and eigenvectors.
    //eigv: is a matrix containing the eigenvalues of a square matrix.
    //      It is diagonal matrix
    //eigx: is a full matrix whose columns are the corresponding
    //      eigenvectors so that A*eigX = eigX*eigV.
    //      It uses col convention.
    //NOTE: matrix must be row-vector matrix.
    void eig(OUT Matrix<T> & eigv, OUT Matrix<T> & eigx, MOD MatMgr<T> & mgr);

    //Reduce matrix to row-echelon normal form, that the pivot diagonal element
    //to 1, and other elements in column are zero.
    void eche(MOD MatMgr<T> & mgr);

    //Full rank decomposition
    //f: m*r full rank matrix
    //g: r*n full rank matrix
    //Let A is m*n matrix, rank is r, and A,f,g satisfied A=f*g.
    void frd(OUT Matrix<T> & f, OUT Matrix<T> & g, MOD MatMgr<T> & mgr);

    //Return the number of rows.
    UINT getRowSize() const { return m_row_size; }

    //Return the number of columns.
    UINT getColSize() const { return m_col_size; }

    //Return the index of last row.
    UINT getLastRow() const { ASSERT0(getSize() > 0); return m_row_size - 1; }

    //Return the index of last column.
    UINT getLastCol() const { ASSERT0(getSize() > 0); return m_col_size - 1; }

    //The function return the element buffer.
    T * getElemBuf() const { return m_mat; }

    //Get element of matrix.
    //Simulating function like as 'v = m_mat[row][col]'
    //Because we allocated memory continuously, so we cannot access element
    //of 'm_mat'by 'm_mat[row][col]' directly.
    T & get(UINT row, UINT col) const
    {
        ASSERTN(m_is_init, ("not yet initialize."));
        ASSERTN(row < m_row_size && col < m_col_size,
                ("exception occur in get()"));
        return *(m_mat + (m_col_size * row + col));
    }

    //Get element address of matrix.
    //Simulating function like as 'v = m_mat[row][col]'
    //Because we allocated memory continuously, so we cannot access element
    //of 'm_mat'by 'm_mat[row][col]' directly.
    T * getElemAddr(UINT row, UINT col)
    {
        ASSERTN(m_is_init, ("not yet initialize."));
        ASSERTN(row < m_row_size && col < m_col_size,
                ("exception occur in get()"));
        return m_mat + (m_col_size * row + col);
    }

    //Growing row of matrix.
    //size: rows to be grown.
    //Note if matrix is empty, growing column vector contains 'size' rows.
    void growRow(UINT size, T const& v = T(0));

    //Growing one row.
    //row: row which should copy to m_mat, and 'num' descripted how
    //     many columns the row is.
    //If number of elememnt in 'row' less than m_col_size, set the
    //remain entries to zero.
    void growRow(T const row[], UINT rowelemnum);
    void growRow(Vector<T> const& row, UINT rowelemnum)
    {
        ASSERTN(row.m_vec, ("vector is empty"));
        growRow(row.m_vec, rowelemnum);
    }

    //Add a row, a must be vector.
    void growRow(Matrix<T> const& a);

    //Add row from 'from' to 'to' of a.
    void growRow(Matrix<T> const& a, UINT from, UINT to);

    //Growing column of matrix.
    //size: columns to be grown.
    //Note if matrix is empty, growing column vector contains a
    //number of 'size' of columns.
    void growCol(UINT size, T const& v = T(0));

    //Growing one col.
    //col: which should copy to m_mat, and 'colelemnum' descripted
    //how many row the column is.
    void growCol(T const col[], UINT colelemnum);
    void growCol(Vector<T> const& col, UINT colelemnum)
    {
        ASSERTN(m_is_init, ("not yet initialize."));
        ASSERTN(col.m_vec, ("vector is empty"));
        growCol(col.m_vec, colelemnum);
    }

    //Add a column, a must be vector.
    void growCol(Matrix<T> const& a);

    //Append columns from 'from' to 'to' of a.
    void growCol(Matrix<T> const& a, UINT from, UINT to);
    void growRowAndCol(UINT row_size, UINT col_size, T const& v = T(0));

    //Set 'v' to row vector by diagonal entry.
    void getdiag(OUT Matrix<T> & v);

    //Get the number of elements.
    UINT getSize() const { return m_row_size * m_col_size; }

    //Compute {1}-inverse matrix.
    //Return true if matrix is nonsingular, otherwise return false.
    //
    //Matrix {1}-inverse is a special case of a general type of pseudoinverse
    //known as g-inverse.
    //That is satisfied:
    //    1.AXA = A
    bool ginv(OUT Matrix<T> & e, MatMgr<T> & mgr);

    bool is_init() const { return m_is_init; }

    //The function initializes matrix elements with zero by default.
    //Note if the function is invoked in constructor, and the inherited class
    //implements the inherited version, then user has to invoke the inherited
    //version after the constructor.
    virtual void initElem() { ::memset(m_mat, 0, sizeof(T) * getSize()); }
    void init()
    {
        if (m_is_init) { return; }
        init(0, 0);
    }
    void init(UINT row, UINT col)
    {
        if (m_is_init) { return; }
        m_row_size = 0;
        m_col_size = 0;
        m_mat = nullptr;
        m_is_init = true;
        growRowAndCol(row, col);
    }

    //Interchange column.
    void interchCol(UINT col1, UINT col2);

    //Interchange row.
    void interchRow(UINT row1, UINT row2);

    //Return true if matrix is a quad one.
    bool is_quad() const;

    //Return true if matrix is I matrix.
    bool is_unitary() const;

    //Return true if matrix is symmetry.
    bool is_symm() const;

    //Return true if matrix is orthogonal.
    //Note current matrix uses row convention
    bool is_orth(MOD MatMgr<T> & mgr) const;

    //Return true if matrix is nonsingular.
    bool is_nonsig(MatMgr<T> & mgr) const;

    //Return true if matrix and m are homogeneous.
    bool is_homo(Matrix<T> const& m) const;
    bool is_rowvec() const; //Return true if matrix is row vector.
    bool is_colvec() const; //Return true if matrix is column vector.

    //Return true if row entry equals 'v'.
    bool is_rowequ(UINT row, T const& v) const;
    bool is_rowequ(UINT row, Matrix<T> const& m, UINT mrow) const;

    //Return true if column entry equals 'v'.
    bool is_colequ(UINT col, T const& v) const;
    bool is_colequ(UINT col, Matrix<T> const& m, UINT mcol) const;
    bool is_vec() const; //Return true if matrix is vector.
    bool isLowTriangular() const;
    bool isAntiLowTriangular() const;
    bool isUpTriangular() const;
    bool isAntiUpTriangular() const;

    //Inserting one empty row before 'ridx'.
    void insertRowBefore(UINT ridx, MOD MatMgr<T> & mgr, T const& v = T(0));

    //Inserting empty rows before 'ridx'.
    //ridx: row index
    //rnum: number of rows
    void insertRowsBefore(UINT ridx, UINT rnum, MOD MatMgr<T> & mgr,
                          T const& v = T(0));

    //Insert rows before 'ridx' which rows copy from matrix 'm'
    //from 'from' to 'to'.
    //ridx: row index
    void insertRowsBefore(UINT ridx, Matrix<T> const& m,
                          UINT mfrom, UINT mto, MOD MatMgr<T> & mgr);

    //Inserting one empty column before 'cidx'.
    //cidx: column index
    void insertColumnBefore(UINT cidx, MOD MatMgr<T> & mgr, T const& v = T(0));

    //Insert the number 'cnum' of empty columns before 'cidx'.
    //cidx: column index
    //cnum: number of columns
    void insertColumnsBefore(UINT cidx, UINT cnum, MOD MatMgr<T> & mgr,
                             T const& v = T(0));

    //Inserting columns before 'cidx' which columns copy from matrix 'm'
    //from 'from' to 'to'.
    //cidx: column index
    void insertColumnsBefore(UINT cidx, Matrix<T> const& m,
                             UINT mfrom, UINT mto, MOD MatMgr<T> & mgr);
    //T integral(FUNCTY s, UINT vidx, T lowb, T upperb);};

    //Get inner matrix at starting point (sr,sl) in matrix, and
    //end point (er,el).
    void inner(OUT Matrix<T> & in, UINT sr, UINT sl, UINT er, UINT el) const;

    //Get inner row of matrix
    void innerRow(OUT Matrix<T> & in, UINT from, UINT to) const;

    //Get inner row of matrix
    void innerColumn(OUT Matrix<T> & in, UINT from, UINT to) const;

    //Initialize to identical matrix with pivot entry 'v'
    void initIden(T const& v);

    //Compute inverse of matrix.
    //Return true if matrix is nonsingular, otherwise return false.
    //'e': inverted matrix.
    //Note matrix must be use row convention.
    bool inv(OUT Matrix<T> & e, MatMgr<T> & mgr) const;

    //Compute right-inverse of row full rank matrix.
    //'x': right-inverse
    //That is satisfied:
    //    AX = I
    bool rinv(OUT Matrix<T> & e, MatMgr<T> & mgr);

    //Compute left-inverse of column full rank matrix.
    //'x': left-inverse
    //That is satisfied:
    //    XA = I
    bool linv(OUT Matrix<T> & e, MatMgr<T> & mgr);

    //Compute {1,2}-inverse matrix.
    //Return true if matrix is nonsingular, otherwise return false.
    //
    //'x': reflexive inverse
    //
    //{1,2}-inverse is also named reflexive g-inverse,
    //a special case of a g-inverse.
    //That is satisifed:
    //    1.AXA = A
    //    2.XAX = X
    void refinv(OUT Matrix<T> & e, MatMgr<T> & mgr);

    //Compute {1,3}-inverse matrix.
    //
    //'x': least-norm inverse
    //
    //{1,3}-inverse is also named least-norm inverse,
    //a special case of a g-inverse.
    //That is satisifed:
    //    1.AXA = A
    //    3.(XA)^H = XA
    void minv(OUT Matrix<T> & x, MatMgr<T> & mgr); //least-norm inverse

    //Compute {1,4}-inverse matrix.
    //x: least-square inverse
    //{1,4}-inverse is also named least-square inverse, a special case
    //of a g-inverse.
    //That is satisifed:
    //    1.AXA = A
    //    4.(AX)^H = AX
    void lstinv(OUT Matrix<T> & x, MatMgr<T> & mgr);

    //LU Decomposition.
    //Non-pivot triangular decomposition
    //Compute L,U in terms of the formula: A = LU,
    //A=|A  ||DDD|
    //  |AB || EE|
    //  |ABC||  F|
    //where L:Lower triangular matrix, U: upper triangular matrix.
    //NOTE: The precision of 'T' may has serious effect.
    bool lu(Matrix<T> & l, Matrix<T> & u);

    //Compute Moore-Penrose inverse matrix.
    //Return true if matrix is nonsingular, otherwise return false.
    //x: pseudo-inverse
    //The Moore-Penrose is the most commonly encountered pseudoinverse
    //that is also known as {1,2,3,4}-inverse, or pseudo inverse.
    //That is satisifed:
    //    1.AXA = A
    //    2.XAX = X
    //    3.(AX)^H = AX
    //    4.(XA)^H = XA
    void mpinv(OUT Matrix<T> & e, MatMgr<T> & mgr);

    //Compute Modular of vector.
    //'this' must be vector
    T mod();

    //Compute Modular of row vector.
    T modrow(UINT row);

    //Compute Modular of column vector.
    T modcol(UINT col);

    //Method of least squares
    //Methods of  least squares to get approximate solution.
    //x: result of system equations
    //   X = x1*v1 + x2*v2 + ...    xn*vn + c,  X is n*n if result has no
    //   constant vector,
    //   or X is n*(n+1), the column n+1 is constant vector
    //
    //   The (b - A*x) is the least square error.
    //   If A is invertable, A*x equals 'b'.
    //b: constant vector.
    //NOTE: A is row convention, and each column indicate  the variable.
    void mls(OUT Matrix<T> & x, Matrix<T> const& b, MatMgr<T> & mgr);

     //Scalar Multiplification.
     //Multiply each elements by 'v'.
    void mul(T const& v);

    //Matrix Multiplification.
    //Note the function does NOT support in-place operation, res can not be
    //same memory with 'a' or 'b'.
    //e.g:res = a * b;
    static Matrix<T> & mul(Matrix<T> const& a, Matrix<T> const& b,
                           OUT Matrix<T> & res);

    //Matrix Multiplification.
    //The function supports in-place operation.
    //The function might allocate a temporary matrix to store the result if
    //given 'res' is the same memory with 'a' or 'b'.
    //Record the result in 'res' matrix.
    //e.g:res = a * b;
    static Matrix<T> & mul(MOD Matrix<T> & a, MOD Matrix<T> & b,
                           OUT Matrix<T> & res, MOD MatMgr<T> & mgr);

    //Scalar multiplification for each element of row
    void mulOfRow(UINT row, T const& v);

    //Multiply times v for each element of row from 'from' to 'to'.
    void mulOfRows(UINT from, UINT to, T const& v);

    //Scalar multiplification for each element of col.
    void mulOfColumn(UINT col, T const& v);

    //Multiply times v for each element of column from 'from' to 'to'.
    void mulOfColumns(UINT from, UINT to, T v);

    //Mutiply times v to element of row 'from', and add such element to
    //row 'to'.
    //v times row 'from', then add row to 'to'.
    void mulAndAddRow(UINT from, T const& v, UINT to);
    void mulAndAddCol(UINT from, T const& v, UINT to);

    void neg();

    //Compute negaitve value of elements from row 'from' to 'to'.
    void negOfRows(UINT from, UINT to);

    //Compute negaitve value of elements from column 'from' to 'to'.
    void negOfColumns(UINT from, UINT to);

    //Calculate null space(kern), each column of 'ns' indicate every variable.
    void nullspace(Matrix<T> & ns, MOD MatMgr<T> & mgr);

    //Normalize each row vector.
    //NOTE: 'this' uses row convention.
    void nml();

    //Calculate matrix/vector Euclidean norm.
    //Calculate matrix/vector norm.
    //p: can be 1, 2, Frobenius, Infinite norm.
    //NOTE: matrix uses row convention.
    T norm(UINT p, MatMgr<T> & mgr);

    //Allow operation as 'x=y=z'
    Matrix<T> & operator = (Matrix<T> const& m)
    {
        ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
        copy(m);
        return *this;
    }

    //Return true if matrix is equivalent to m.
    bool operator == (Matrix<T> const& m) const;

    //Return true if matrix is not equivalent to m.
    bool operator != (Matrix<T> const& m) const { return !(*this == m); }

    //Calculate orthonormal basis
    //Orthogonalization and Normalization by Gram-Schmidt method.
    //z: use row convention.
    //NOTE: matrix is a row vector.
    void orthn(Matrix<T> & z, MOD MatMgr<T> & mgr);

    //Calculate orthogonal basis.
    //Orthogonalization by Gram-Schmidt method.
    //z: Each row of vectors are orthogonalized mutually.
    //   It uses row convention.
    //NOTE:
    //  'this' use row convention.
    //  Each rows of vectors should be independent mutually.
    //  Or else zero vector was generated.
    void orth(Matrix<T> & z, MOD MatMgr<T> & mgr);

    //Pivot orthogonal decomposition
    //Permute QR Decomposition.
    //Function produces a permutation matrix p, an upper triangular
    //matrix R of the same dimension as A and a unitary matrix Q so that
    //A*P = Q*R.
    //A=|ABC||o  ||AAA|
    //  |ABC|| o ||BBB|
    //  |ABC||  o||CCC|
    //The column permutation p is chosen so that abs(diag(R)) is decreasing.
    //NOTE: The precision of 'T' may has serious effect.
    bool pqr(OUT Matrix<T> & p, OUT Matrix<T> & q, OUT Matrix<T> & r);

    //Compute projection of vector 'v' in row space of 'this'.
    //p: projection of v
    //v: vector to project .
    //NOITCE:
    //  1. each row of 'this' must be orthogonal base of one space.
    //  2. 'this' uses row convention.
    void proj(OUT Matrix<T> & proj, Matrix<T> const& p, MOD MatMgr<T> & mgr);

    //Complement to nonsingular.
    //Complement remainding rows for partial matrix to generate nonsingular
    //matrix.
    //To obtain an invertible square matrix, we extend 'this' to a basis by
    //simply adding linearly independent unit vectors at the bottom of 'this'.
    //Each of which spawns one missing dimension.
    //e.g: Given matrix is [1,1], the output matrix is [1,1][0,1]
    //NOTE: The partial rows of matrix must be independent.
    void padToNonsingular(MOD MatMgr<T> & mgr);

    //Pading to quad matrix, and rank = MAX(row, col)
    //The function pad row and col to generate quad matrix.
    void padToQuad(T const& v = T(0));

    //Pading matrix to specific shape with given padding value 'v'.
    //up: the row size to pad on up-side of current matrix.
    //down: the row size to pad on down-side of current matrix.
    //left: the column size to pad on left-side of current matrix.
    //right: the column size to pad on right-side of current matrix.
    //v: the value that expected to pad.
    //e.g: given matrix [a b c d], pad value is 0, and both up, down, left,
    //right are 1. After padding, the original matrix will be surrounded
    //by a layer of 0,
    //  0 0 0 0
    //  0 a b 0
    //  0 c d 0
    //  0 0 0 0
    void padTo(MOD MatMgr<T> & mgr, UINT up = 1, UINT down = 1,
               UINT left = 1, UINT right = 1, T const& v = T(0));

    //Pivot Triangular Decomposition.
    //Compute P,L,U in terms of the formula: PA = LU,
    //where P:Permute MatrixL, L:Lower triangular matrix,
    //U: upper triangular matrix.
    //NOTE: The precision of 'T' may has serious effect.
    bool plu(Matrix<T> & p, Matrix<T> & l, Matrix<T> & u, MatMgr<T> & mgr);

    //Orthogonal decomposition
    //QR Decomposition.
    //Function produces an upper triangular matrix R of the same
    //dimension as row of A and a unitary matrix Q(orthonomal basis)
    //so that A = Q*R.
    //A=|ABC||ooo|
    //  |ABC|| oo|
    //  |ABC||  o|
    //Space of row vector of A that can be descripte by number of basis,
    //also could be represented by number of orthogonal basis with
    //relatived coefficient.
    //And R consists of these coefficients.
    //
    //calc_basis: If true is means to first calculate basis of 'this',
    //            and then compute the QR.
    //
    //NOTE:
    //  1.'this' uses row convention, so the formual need to transform.
    //    Orignal theroem: Given A uses col convention., and A=QR, so
    //    trans(Q)A = trans(Q)(QR) = R, here R is upper-triangular
    //    matrix. Now is, given A use row convention, there are two
    //    methods to get the Q,R, the first is:
    //      replace A in original formual with trans(A).
    //      so
    //      trans(trans(Q)*A) = trans(A)*Q = trans(Q*R)*Q =
    //      trans(R)*trans(Q)*Q = trans(R)
    //
    //    and the second is:
    //      Because A use row convention,  Q is also use row convention.
    //      To get upper triangular R,
    //      we need projecting each axis of vectors in A, and the project
    //      coefficent of each axis is the row of Q, so we get the
    //      formaluar:
    //          R = Q * trans(A), Q is orthogonal, row convention,
    //          and each row indicate project coefficent.
    //      A uses column convention,  each column means vector of space A.
    //      Then the output 'q','r' is colQ, and colR, and in actually,
    //        colQ*colR uses col convention.
    //
    //  2.'this' matrix should be row linearly independent as well,
    //    otherwise 'calc_basis' must set to 'true', then basis of A be
    //    computed, and the out put Q*R equals basis(A).
    //
    //  3.The precision of 'T' may has serious effect.
    void qr(Matrix<T> & q, Matrix<T> & r, MOD MatMgr<T> & mgr,
            bool calc_basis = false);

    //Compute rank of 'this', and the corresponded linear indepent vectors
    //store  as row convention.
    //basis: output the matrix that is transformed in
    //       upper trapezoid matix as a result of elementary row transformation.
    //is_unitary: True indicates the routine will reduce the pivot
    //            element 'pv' to 1, and eliminating all other same column
    //            elements to zero, otherwise do nothing of those operations.
    //NOTE:
    //  In actually the estimation of rank is not a simply problem.
    //  See details in "Numerical Linear Algebra and Optimization" ,
    //  Philip E. Gill.
    UINT rank(MatMgr<T> & mgr, Matrix<T> * basis = nullptr,
              bool is_unitarize = false) const;

    //The function will reinitialize the matrix by given row and col size.
    //Note all origin elements will be destroyed.
    void reinit(UINT row, UINT col)
    {
        if (m_row_size == row && m_col_size == col) { return; }
        destroy();
        init(row, col);
    }

    //The function swaps element buffer and data structure to 'src'.
    //The behavior is similar to: tmp = *this; *this = src; src = tmp.
    void swap(MOD Matrix<T> & src);

    //Initialize diagonal entry to 'v'.
    void setdiag(Vector<T> const& v, UINT vlen);

    //Initialize diagonal entry of vector v.
    //NOTE: 'v' is vector.
    void setdiag(Matrix<T> const& v);

    //Set element of matrix.
    //Simulating function like as 'm_mat[row][col] = v'
    //Because we allocated memory continuously, so we cannot access element
    //of 'm_mat' by 'm_mat[row][col]' directly.
    void set(UINT row, UINT col, T const& v)
    {
        ASSERTN(m_is_init, ("not yet initialize."));
        ASSERTN(row < m_row_size && col < m_col_size,
                ("exception occur in set()"));
        *(m_mat + (m_col_size * row + col)) = v;
    }

    //Matrix Subtraction.
    //Note the function supports in-place operation.
    //e.g:res = a - b;
    static Matrix<T> & sub(Matrix<T> const& a, Matrix<T> const& b,
                           OUT Matrix<T> & res);

    //Solve system of equaions, 'this' is coeff matirx
    //Solving system of linear equations.
    //Return true if the solution is unique, othwise return false, and x is
    //generating solution.
    //
    //x: result of system equations
    //   1.Unique solution: [v1, v2, ... vn],  and 'x' is column vector.
    //   2.Generating solutin:
    //      x1*[a1,...,an] + x2*[b1,...,bn] + .. + xn*[k1,..,kn] + [C1,..,Cn]
    //      x1,x2,...,xn are unconstrained variables.
    //
    //     'x' is n*(n+1) matrix, last column is augmented vector.
    //
    //b: augmented vector. At present constant part only supports
    //   the constant value.
    //
    //NOTE:
    //  1.A is invertable, so is LU decomposable as well, the result is
    //    unique. To get approximatest solution, call mls().
    //  2.A is row convention, and each column indicate the variable.
    //  3.If 'this' is singular, compute the general solution utilize
    //    nullspace().
    bool sse(OUT Matrix<T> & x, Matrix<T> const& b, MatMgr<T> & mgr);

    //Set element of matrix.
    //If element (row,col) out side of current range, grow the matrix to size
    //of 'row' and 'col' at least. Then set that element to 'v'.
    void setg(UINT row, UINT col, T const& v);

    //Set element of matrix one by one.
    //num: indicate the number of elements in the variadic parameters list.
    virtual void setPartialElem(UINT num, ...);

    //Set all elements to be value 'v'
    void setAllElem(T const& v);

    //Set all 'row' row elements of matrix.
    //newval: row need to set new value.
    void setRow(UINT row, T newval[], UINT num);

    //Set all 'row' row elements with the element in matrix 'v'
    //row: row need to set.
    //v: v must be vector.
    void setRow(UINT row, Matrix<T> const& v);

    //Set an entire row by given value 'v' with type T.
    //row: row need to set new value.
    void setRow(UINT row, T const& v);

    //Set a list of row by coping row from given matrix 'm'.
    void setRows(UINT from, UINT to, Matrix<T> const& m, UINT mfrom);

    //Set a list rows by given value 'v' with type T.
    void setRows(UINT from, UINT to, T const& v);

    //Set a list rows and a list columns by given value 'v' with type T.
    void setRowsAndCols(UINT rowfrom, UINT rowto, UINT colfrom, UINT colto,
                        T const& v);

    //Set 'col' column elements.
    //col: the column to set new value.
    //newcol: a vector that record the new value.
    //num: the number of element in newcol.
    void setCol(UINT col, T newcol[], UINT num);

    //Set 'col' column elements.
    //col: the column to set new value.
    //newcol: a vector that record the new value. It can be row-vector or
    //        column-vector.
    void setCol(UINT col, Matrix<T> const& newval);

    //Set 'col' column elements to be 'v'.
    //col: the column to set new value.
    //v: the new value to set.
    void setCol(UINT col, T const& v);

    //Set column elements from 'from' column to 'to' column to be elements
    //of the column 'mfrom' in 'm'.
    void setCols(UINT from, UINT to, Matrix<T> const& m, UINT mfrom);

    //Set column elements from 'from' column to 'to' column to be value 'v'.
    void setCols(UINT from, UINT to, T const& v);

    //Calculate spectral radius.
    //Spectral radius.
    //Specrad = max|lamda(i)|, i=1~N-1
    //NOTE: matrix must be row-vector matrix.
    T sprad(MOD MatMgr<T> & mgr);

    //Strange Value decomposition
    //A = u*s*eigx, 'this' is m*n matrix.
    //A=|ABC||o ||DD|
    //  |ABC|| o||EE|
    //  |ABC|
    //u: is m*m orthonormal matrix, col vector form arrangement.
    //s: m*n stanger value matrix, diagonal matrix.
    //eigx: n*n  orthonormal matrix,  row vector form arrangement.
    bool svd(OUT Matrix<T> & u, OUT Matrix<T> & s, OUT Matrix<T> & eigx,
             MOD MatMgr<T> & mgr);

    //Compute trace.
    T tr();

    //The function perform matrix transpose.
    void trans();

    void zero(); //Set each elements to zero with type T.
    void fzero(); //Fast zeroization by invoking ::memset((void*)0).
};


template <class T>
size_t Matrix<T>::count_mem() const
{
    size_t count = sizeof(*this);
    count += getSize() * sizeof(T);
    return count;
}


template <class T>
void Matrix<T>::growRow(UINT size, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (size == 0) { return; }

    UINT oldrows = 0;
    if (m_row_size == 0) {
        //If matrix is empty, growing 'size' rows, but only one column.
        ASSERTN(m_col_size == 0 && m_mat == nullptr,
                ("matrix should be empty"));
        allocMatBuf(size);
        m_col_size = 1;
        m_row_size = size;
        goto INIT;
    }
    {
        reallocMatBuf(m_col_size * (m_row_size + size));
        oldrows = m_row_size;
        m_row_size += size;
    }
INIT:
    //Initializing elements.
    setRowsAndCols(oldrows, m_row_size - 1, 0, m_col_size - 1, v);
}


template <class T>
void Matrix<T>::growRow(const T row[], UINT rowelemnum)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERT0(row != nullptr);
    if (rowelemnum == 0) { return; }
    if (m_row_size == 0) {
        initMatBufWith(row, rowelemnum);
        m_row_size = 1;
        m_col_size = rowelemnum;
        return;
    }
    ASSERTN(rowelemnum <= m_col_size,("unmatch column sizes"));
    T * tmp_mat = allocTBuf(m_col_size * (m_row_size + 1));
    copyTBuf(tmp_mat, m_mat, m_col_size * m_row_size);
    copyTBuf(tmp_mat + m_col_size * m_row_size, row, rowelemnum);
    if (rowelemnum < m_col_size) {
        cleanTBuf(tmp_mat + (m_col_size * m_row_size + rowelemnum),
                  (m_col_size - rowelemnum));
    }
    freeMatBuf();
    m_mat = tmp_mat;
    m_row_size++;
}


template <class T>
void Matrix<T>::growRow(Matrix<T> const& a)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(a.is_vec(), ("a must be vector"));
    if (m_row_size > 0) {
        ASSERTN(a.getSize() == m_col_size, ("invalid vector"));
    }
    growRow(a.m_mat, a.getSize());
}


template <class T>
void Matrix<T>::growRow(Matrix<T> const& a, UINT from, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(from < a.m_row_size && to < a.m_row_size && from <= to,
            ("not yet initialize."));
    if (m_row_size == 0) {
        //If matrix is empty, growing 'size' rows, but only one column.
        ASSERTN(!m_col_size && m_mat == nullptr, ("matrix should be empty"));
        m_col_size = a.m_col_size;
        m_row_size = to - from + 1;
        allocMatBuf(m_row_size * m_col_size);
        copyTBuf(m_mat, a.m_mat + from * a.m_col_size, m_row_size * m_col_size);
        return;
    }
    ASSERTN(a.m_col_size == m_col_size, ("unmatch matrix"));
    UINT row_size = to - from + 1;
    T * tmp_mat = allocTBuf((m_row_size + row_size) * m_col_size);
    ASSERT0(tmp_mat);
    UINT orig_size = m_col_size * m_row_size;
    copyTBuf(tmp_mat, m_mat, orig_size);
    copyTBuf(tmp_mat + orig_size, a.m_mat + from * a.m_col_size,
             row_size * m_col_size);
    freeMatBuf();
    m_mat = tmp_mat;
    m_row_size += row_size;
}


template <class T>
void Matrix<T>::growCol(UINT size, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (size == 0) { return; }
    if (m_col_size == 0) {
        //Matrix is empty. If matrix is empty, growing 'size' columns,
        //but only one row.
        ASSERT0(getSize() == 0 && m_mat == nullptr);
        allocMatBuf(size);
        m_col_size = size;
        m_row_size = 1;

        //Initialize each elements in row.
        setRowsAndCols(0, 0, 0, m_col_size - 1, v);
        return;
    }
    growRowAndCol(0, size, v);
}


template <class T>
void Matrix<T>::growCol(Matrix<T> const& a)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(a.is_vec(), ("a must be vector"));
    if (m_col_size > 0) {
        ASSERTN(a.getSize() == m_row_size, ("invalid vector"));
    }
    growCol(a.m_mat, a.getSize());
}


template <class T>
void Matrix<T>::growCol(Matrix<T> const& a, UINT from, UINT to)
{
    ASSERTN(m_is_init && a.m_is_init, ("not yet initialize."));
    ASSERTN(from < a.m_col_size && to < a.m_col_size && from <= to,
            ("not yet initialize."));
    if (m_col_size == 0) {
        //Matrix is empty, and m_row_size also be 0.
        ASSERT0(getSize() == 0 && m_mat == nullptr);
        m_row_size = a.m_row_size;
        m_col_size = to - from + 1;
        allocMatBuf(m_row_size * m_col_size);
        for (UINT i = 0 ; i < m_row_size; i++) {
            copyTBuf(m_mat + i * m_col_size, a.m_mat + i * a.m_col_size + from,
                     m_col_size);
        }
        return;
    }
    ASSERTN(a.m_row_size == m_row_size, ("unmatch matrix"));
    UINT col_size = to - from + 1;
    T * tmp_mat = allocTBuf(m_row_size * (m_col_size + col_size));
    ASSERT0(tmp_mat);
    for (UINT i = 0 ; i < m_row_size; i++) {
        copyTBuf(tmp_mat + i * (m_col_size + col_size),
                 m_mat + i * m_col_size,
                 m_col_size);
        copyTBuf(tmp_mat + i * (m_col_size+col_size) + m_col_size,
                 a.m_mat + i * a.m_col_size + from,
                 col_size);
    }
    freeMatBuf();
    m_mat = tmp_mat;
    m_col_size += col_size;
}


template <class T>
void Matrix<T>::growCol(const T col[], UINT colelemnum)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERT0(col != nullptr);
    if (m_col_size == 0) {
        //Matrix is empty, and m_row_size also be 0.
        ASSERT0(getSize() == 0 && m_mat == nullptr);
        allocMatBuf(colelemnum);
        copyTBuf(m_mat, col, colelemnum);
        m_row_size = colelemnum;
        m_col_size = 1;
        return;
    }
    ASSERTN(colelemnum <= m_row_size,("unmatch column sizes"));
    T * tmp_mat = allocTBuf(m_row_size * (m_col_size + 1));
    ASSERT0(tmp_mat);
    for (UINT i = 0 ; i < m_row_size; i++) {
        copyTBuf(tmp_mat + i * (m_col_size + 1), m_mat + i * m_col_size,
                 m_col_size);
        if (i >= colelemnum) {
            *(tmp_mat + i*(m_col_size+1) + m_col_size) = 0;
        } else {
            *(tmp_mat + i*(m_col_size+1) + m_col_size) = col[i];
        }
    }
    freeMatBuf();
    m_mat = tmp_mat;
    m_col_size++;
}


template <class T>
void Matrix<T>::growRowAndCol(UINT row_size, UINT col_size, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (row_size == 0 && col_size == 0) { return; }
    if (col_size == 0) {
        //Only grow row.
        growRow(row_size, v);
        return;
    }
    //The complexity of grow column is same as grow both, thus
    //growCol() will invoke current function.
    if (m_col_size == 0) {
        //Matrix is empty, m_row_size also be 0.
        ASSERT0(getSize() == 0 && m_mat == nullptr);
        allocMatBuf(row_size * col_size);
        m_row_size = row_size;
        m_col_size = col_size;
        initElem();
        return;
    }
    T * tmp_mat = allocTBuf((m_row_size + row_size) * (m_col_size + col_size));
    ASSERT0(tmp_mat);
    for (UINT i = 0 ; i < m_row_size; i++) {
        copyTBuf(tmp_mat + i * (m_col_size + col_size),
                 m_mat + i * m_col_size, m_col_size);
    }
    freeMatBuf();
    m_mat = tmp_mat;
    UINT oldrows = m_row_size;
    UINT oldcols = m_col_size;
    m_row_size += row_size;
    m_col_size += col_size;

    //Initializing elements.
    setRowsAndCols(0, oldrows - 1, oldcols, m_col_size - 1, v);
    ASSERT0(col_size != 0);
    if (row_size != 0) {
        setRowsAndCols(oldrows, m_row_size - 1, 0, m_col_size - 1, v);
    }
}


template <class T>
void Matrix<T>::deleteRow(UINT from, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(from < m_row_size && to < m_row_size && from <= to,
            ("out of boundary"));
    if (m_row_size == 0) { return; }
    T * tmp_mat = allocTBuf(m_col_size * (m_row_size - (to - from + 1)));
    copyTBuf(tmp_mat, m_mat, m_col_size * from);
    copyTBuf(tmp_mat + m_col_size * from,
             m_mat + m_col_size * (to + 1),
             m_col_size * (m_row_size - to - 1));
    freeMatBuf();
    m_mat = tmp_mat;
    m_row_size -= to - from + 1;
}


template <class T>
void Matrix<T>::deleteCol(UINT from, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(from < m_col_size && to < m_col_size && from <= to,
            ("out of boundary"));
    if (m_col_size == 0) { return; }
    UINT col_size = m_col_size - (to - from + 1);
    T * tmp_mat = allocTBuf(col_size * m_row_size);
    for (UINT i = 0 ; i < m_row_size; i++) {
        copyTBuf(tmp_mat + i * col_size, m_mat + i * m_col_size, from);
        copyTBuf(tmp_mat + i * col_size + from,
                 m_mat + (i * m_col_size + to + 1),
                 (m_col_size - to - 1));
    }
    freeMatBuf();
    m_mat = tmp_mat;
    m_col_size = col_size;
}


template <class T>
void Matrix<T>::padToQuad(T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (m_row_size == m_col_size) { return; }
    if (m_row_size < m_col_size) {
        growRow(m_col_size - m_row_size);
        return;
    }
    growCol(m_row_size - m_col_size, v);
}


template <class T>
void Matrix<T>::padTo(MOD MatMgr<T> & mgr, UINT up, UINT down,
                      UINT left, UINT right, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(getSize() > 0, ("do not pad empty matrix."));
    if (up == 0 && left == 0) {
        growRowAndCol(down, right, v);
        return;
    }
    if (up != 0) {
        insertRowsBefore(0, up, mgr, v);
    }
    if (left != 0) {
        insertColumnsBefore(0, left, mgr, v);
    }
    growRowAndCol(down, right, v);
}


template <class T>
void Matrix<T>::setg(UINT row, UINT col, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (m_row_size <= row && m_col_size > col) {
        //grow row
        growRow(row + 1 - m_row_size, v);
    } else if (m_row_size > row && m_col_size <= col) {
        //grow col
        growCol(col + 1 - m_col_size, v);
    } else if (m_row_size <= row && m_col_size <= col) {
        //grow both
        growRowAndCol(row + 1 - m_row_size, col + 1 - m_col_size, v);
    }
    set(row, col, v);
}


template <class T>
void Matrix<T>::setRow(UINT row, T newval[], UINT num)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size && num == m_col_size,
            ("exception occur in set()"));
    copyTBuf(m_mat + row * m_col_size, newval, m_col_size);
}


template <class T>
void Matrix<T>::setRow(UINT row, Matrix<T> const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size, ("exception occur in set()"));
    ASSERTN(v.is_vec(), ("v must be vector"));
    ASSERTN((m_col_size == v.getSize()), ("invalid vector"));
    if (v.is_rowvec()) {
        ASSERTN(v.m_col_size == m_col_size,("unmatch vector"));
        copyTBuf(m_mat + row * m_col_size, v.m_mat, m_col_size);
        return;
    }
    if (v.is_colvec()) {
        ASSERTN(v.m_row_size == m_col_size,("unmatch vector"));
        for (UINT i = 0; i < m_col_size; i++) {
            set(row, i, v.get(i, 0));
        }
        return;
    }
    UNREACHABLE();
}


template <class T>
void Matrix<T>::setRows(UINT from, UINT to, Matrix<T> const& m, UINT mfrom)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m.m_col_size == m_col_size && from <= to && to < m_row_size &&
            mfrom + to - from < m.m_row_size, ("illegal matrix"));
    for (UINT i = from; i <= to; i++, mfrom++) {
        for (UINT j = 0; j < getColSize(); j++) {
            set(i, j, m.get(mfrom, j));
        }
    }
}


template <class T>
void Matrix<T>::setRows(UINT from, UINT to, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(from <= to && to < m_row_size, ("illegal matrix"));
    for (UINT i = from; i <= to; i++) {
        setRow(i, v);
    }
}


template <class T>
void Matrix<T>::setRowsAndCols(
    UINT rowfrom, UINT rowto, UINT colfrom, UINT colto, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(rowfrom <= rowto && rowto < m_row_size, ("illegal matrix"));
    ASSERTN(colfrom <= colto && colto < m_col_size, ("illegal matrix"));
    for (UINT i = rowfrom; i <= rowto; i++) {
        for (UINT j = colfrom; j <= colto; j++) {
            set(i, j, v);
        }
    }
}


template <class T>
void Matrix<T>::setRow(UINT row, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size, ("exception occur in set()"));
    for (UINT i = 0; i < m_col_size; i++) {
        set(row, i, v);
    }
}


template <class T>
void Matrix<T>::setCol(UINT col, T newcol[], UINT num)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(col < m_col_size && num == m_row_size,
            ("exception occur in set()"));
    for (UINT i = 0 ; i < m_row_size ; i++) {
        set(i, col, newcol[i]);
    }
}


template <class T>
void Matrix<T>::setCol(UINT col, Matrix<T> const& newval)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(col < m_col_size, ("exception occur in set()"));
    ASSERTN(newval.is_vec(), ("v must be vector"));
    ASSERTN(newval.getSize() == m_row_size, ("invalid vector"));
    if (newval.is_rowvec()) {
        ASSERTN(newval.m_col_size == m_row_size,("unmatch vector"));
        for (UINT i = 0; i < m_row_size; i++) {
            set(i, col, newval.get(0, i));
        }
        return;
    }
    if (newval.is_colvec()) {
        ASSERTN(newval.m_row_size == m_row_size,("unmatch vector"));
        for (UINT i = 0; i < m_row_size; i++) {
            set(i, col, newval.get(i, 0));
        }
        return;
    }
    ASSERTN(0, ("invalid operation"));
}


template <class T>
void Matrix<T>::setCols(UINT from, UINT to, Matrix<T> const& m, UINT mfrom)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m.m_row_size == m_row_size &&
            from <= to && to < m_col_size && mfrom + to - from < m.m_col_size,
            ("illegal matrix"));
    for (UINT i = 0; i < getRowSize(); i++) {
        UINT k = 0;
        for (UINT j = from; j <= to; j++, k++) {
            set(i, j, m.get(i, mfrom + k));
        }
    }
}


template <class T>
void Matrix<T>::setCols(UINT from, UINT to, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(from <= to && to < m_col_size, ("illegal matrix"));
    for (UINT j = from; j <= to; j++) {
        setCol(j, v);
    }
}


template <class T>
void Matrix<T>::setCol(UINT col, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(col < m_col_size, ("exception occur in set()"));
    for (UINT i = 0; i < m_row_size; i++) {
        set(i, col, v);
    }
}


template <class T>
void Matrix<T>::interchRow(UINT row1, UINT row2)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row1 < m_row_size && row2 < m_row_size,
            ("exception occur in interchRow()"));
    if (row1 == row2) { return; }
    for (UINT i = 0 ; i < m_col_size ; i++) {
        T tmp = get(row1, i);
        set(row1, i, get(row2, i)); //ACTION: m_mat[row1][i] = m_mat[row2][i];
        set(row2, i, tmp); //ACTION: m_mat[row2][i] = tmp;
    }
}


template <class T>
void Matrix<T>::interchCol(UINT col1, UINT col2)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(col1 < m_row_size && col2 < m_row_size,
            ("exception occur in interchCol()"));
    if (col1 == col2) { return; }
    for (UINT i = 0 ; i < m_row_size ; i++) {
        T tmp = get(i, col1);
        set(i,col1, get(i, col2)); //ACTION: m_mat[i][col1] = m_mat[i][col2];
        set(i, col2, tmp); //ACTION: m_mat[i][col2] = tmp;
    }
}


template <class T>
void Matrix<T>::trans()
{
    ASSERTN(m_is_init, ("not yet initialize."));
    UINT i;
    if (m_row_size == 0) { return; }
    ASSERTN(m_mat,("exception occur tranpose()"));
    T * tmp_mat = allocTBuf(m_row_size * m_col_size);
    for (i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            //ACTION: tmp_mat[j][i] = m_mat[i][j];
            *(tmp_mat + j*m_row_size + i) = get(i, j);
        }
    }
    freeMatBuf();
    m_mat = tmp_mat;
    i = m_row_size;
    m_row_size = m_col_size;
    m_col_size = i;
}


template <class T>
void Matrix<T>::copy(Matrix<T> const& m)
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    if (this == &m) { return; }
    if (m.getSize() == 0) {
        deleteAllElem();
        return;
    }
    //ACTION:m(a,b), m(x,y): a*b == x*y
    reallocMatBufWith(m.m_mat, m.getSize());
    m_row_size = m.m_row_size;
    m_col_size = m.m_col_size;
}


template <class T>
bool Matrix<T>::is_nonsig(MatMgr<T> & mgr) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (!is_quad()) { return false; }
    if (equalElem(det(mgr), T(0))) { return false; }
    return true;
}


template <class T>
bool Matrix<T>::is_quad() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0,
            ("exception occur in is_quad()"));
    return m_row_size == m_col_size;
}


template <class T>
bool Matrix<T>::is_unitary() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (!is_quad()) { return false; }
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            if (i == j) {
                if (get(i, j) != 1) {
                    return false;
                }
                continue;
            }
            if (!equalElem(get(i, j), T(0))) {
                return false;
            }
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::is_symm() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (!is_quad()) { return false; }
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            if (i == j) { continue; }
            if (!equalElem(get(i, j), get(j,i))) {
                return false;
            }
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::is_orth(MOD MatMgr<T> & mgr) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    MatWrap<T> t1(*this, mgr); Matrix<T> & tmp = t1.m();
    tmp.trans();
    mul(*const_cast<Matrix<T>*>(this), tmp, tmp, mgr);
    for (UINT i = 0; i < tmp.m_row_size; i++) {
        for (UINT j = 0; j < tmp.m_col_size; j++) {
            if (i != j && !equalElem(tmp.get(i, j), T(0))) {
                return false;
            }
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::operator == (Matrix<T> const& m) const
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    if (this == &m) { return true; }
    if (!is_homo(m)) { return false; }
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            if (get(i, j) != m.get(i, j)) {
                //May be need to change precision.
                return false;
            }
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::is_homo(Matrix<T> const& m) const
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    return m_row_size == m.m_row_size && m_col_size == m.m_col_size;
}


template <class T>
void Matrix<T>::neg()
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0, ("invalid matrix"));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            //Note the negative operator '-' of T should be overloading.
            set(i, j, -get(i, j));
        }
    }
}


template <class T>
void Matrix<T>::negOfRows(UINT from, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && to < m_row_size && from <= to,
            ("invalid matrix or parameter"));
    for (UINT i = from; i <= to; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            //Note the negative operator '-' of T should be overloading.
            set(i, j, -get(i, j));
        }
    }
}


template <class T>
void Matrix<T>::negOfColumns(UINT from, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && to < m_col_size && from <= to,
            ("invalid matrix or parameter"));
    for (UINT j = from; j <= to; j++) {
        for (UINT i = 0; i < m_row_size; i++) {
            //Note the negative operator '-' of T should be overloading.
            set(i, j, -get(i, j));
        }
    }
}


template <class T>
void Matrix<T>::mul(T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0, ("invalid matrix"));
    if (equalElem(v, T(0))) {
        zero();
        return;
    }
    if (v == T(1)) { return; }
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            set(i, j, (get(i, j) * v));
        }
    }
    return;
}


template <class T>
void Matrix<T>::mulOfRow(UINT row, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && row < m_row_size,
            ("invalid matrix or parameter"));
    if (v == T(1)) { return; }
    if (equalElem(v, T(0))) {
        setRow(row, T(0));
        return;
    }
    for (UINT j = 0; j < m_col_size; j++) {
        set(row, j, (get(row, j) * v));
    }
}


template <class T>
void Matrix<T>::mulOfRows(UINT from, UINT to, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && to < m_row_size && from <= to,
            ("invalid matrix or parameter"));
    if (v == 1) { return; }
    if (equalElem(v, T(0))) {
        setRows(from, to, T(0));
        return;
    }
    for (UINT i = from; i <= to; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            set(i, j, (get(i, j) * v));
        }
    }
}


template <class T>
void Matrix<T>::mulOfColumn(UINT col, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && col < m_col_size,
            ("invalid matrix or parameter"));
    if (v == T(1)) { return; }
    if (equalElem(v, T(0))) {
        setCol(col, T(0));
        return;
    }
    for (UINT i = 0; i < m_row_size; i++) {
        set(i, col, (get(i, col) * v));
    }
}


template <class T>
void Matrix<T>:: mulOfColumns(UINT from, UINT to, T v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && to < m_col_size && from <= to,
            ("invalid matrix or parameter"));
    if (v == T(1)) { return; }
    if (equalElem(v, T(0))) {
        setCols(from, to, T(0));
        return;
    }
    for (UINT j = from; j <= to; j++) {
        for (UINT i = 0; i < m_row_size; i++) {
            set(i, j, (get(i, j) * v));
        }
    }
}


template <class T>
void Matrix<T>::addRowToRow(UINT from, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && from < m_row_size &&
            to < m_row_size, ("invalid matrix or parameter"));
    for (UINT j = 0; j < m_col_size; j++) {
        set(to, j, (get(from, j) + get(to, j)));
    }
}


template <class T>
void Matrix<T>::addRowToRow(Matrix<T> const& m, UINT mfrom, UINT to)
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && mfrom < m.m_row_size &&
            to < m_row_size, ("invalid matrix or parameter"));
    ASSERTN(m_col_size == m.m_col_size, ("unmatch row size"));
    for (UINT j = 0; j < m_col_size; j++) {
        set(to, j, (m.get(mfrom, j) + get(to, j)));
    }
}


template <class T>
void Matrix<T>::addColumnToColumn(UINT from, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && from < m_col_size &&
            to < m_col_size, ("invalid matrix or parameter"));
    for (UINT i = 0; i < m_row_size; i++) {
        set(i, to, (get(i, from) + get(i, to)));
    }
}


template <class T>
void Matrix<T>::addColumnToColumn(Matrix<T> const& m, UINT mfrom, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && mfrom < m.m_col_size &&
            to < m_col_size, ("invalid matrix or parameter"));
    ASSERTN(m_row_size == m.m_row_size, ("unmatch col size"));
    for (UINT i = 0; i < m_row_size; i++) {
        set(i, to, (m.get(i, mfrom) + get(i, to)));
    }
}


template <class T>
void Matrix<T>::mulAndAddRow(UINT from, T const& v, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && from < m_row_size &&
            to < m_row_size, ("invalid matrix or parameter"));
    for (UINT j = 0; j < m_col_size; j++) {
        set(to, j, ((get(from, j) * v) + get(to, j)));
    }
}


template <class T>
void Matrix<T>::mulAndAddCol(UINT from, T const& v, UINT to)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0 && from < m_col_size &&
            to < m_col_size, ("invalid matrix or parameter"));
    for (UINT i = 0; i < m_row_size; i++) {
        set(i, to, ((get(i, from) * v) + get(i, to)));
    }
}


template <class T>
void Matrix<T>::setAllElem(T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    for (UINT j = 0; j < m_row_size; j++) {
        for (UINT i = 0; i < m_col_size; i++) {
            set(j, i, v);
        }
    }
}


template <class T>
UINT Matrix<T>::ReverseOrderNumber(UINT * numbuf, UINT numlen)
{
    UINT ron = 0;
    for (UINT i = 0; i < numlen - 1; i++) {
        for (UINT j = i+1; j < numlen; j++) {
            if (numbuf[i] > numbuf[j]) {
                ron++;
            }
        }
    }
    return ron;
}


template <class T>
void Matrix<T>::FullPermutationRecur(UINT v, UINT * posbuf, UINT posbufnum,
                                     UINT n, T & det)
{
    for (UINT i = 0; i < posbufnum; i++) {
        if (posbuf[i] != 0) { continue; }
        //slot of position 'i' in 'posbuf' is avaible.
        posbuf[i] = v;
        if (v == n) {
            //ONLY for debugging
            //printf("\n full_perm_order: ");
            //for (INT w = 0; w < posbufnum; w++) {
            //    printf("%d,",posbuf[w]);
            //}
            //printf("\telem: ");
            //for (INT w = 0; w < posbufnum; w++) {
            //    T gg = get(w, (posbuf[w]-1));
            //    printf("%d,",gg.num());
            //}
            //printf(" : ron=%d\n",
            //       ReverseOrderNumber(posbuf, posbufnum));
            T t;
            t = 1;
            for (UINT j = 0; j < posbufnum; j++) {
                ASSERTN(posbuf[j]>=1, ("Invalid subscript"));
                t = t * get(j, (posbuf[j]-1));
            }
            UINT ron = ReverseOrderNumber(posbuf, posbufnum);
            if ((ron % 2) != 0) {
                T minus;
                minus = -1;
                t = t * minus;
            }
            det = det + t;
            posbuf[i] = 0;
            return;
        }
        FullPermutationRecur(v + 1, posbuf, posbufnum, n, det);
        posbuf[i] = 0;
    }
}


template <class T>
T Matrix<T>::FullPermutation(UINT n)
{
    ASSERTN(n > 1, ("Invalid number"));
    UINT * posbuf = new UINT[n];
    T det = T(0);
    ::memset((void*)posbuf, 0, sizeof(UINT) * n);
    for (UINT i = 0; i < n; i++) {
        posbuf[i] = 1;
        FullPermutationRecur(2, posbuf, n, n, det);
        posbuf[i] = 0;
    }
    delete [] posbuf;
    return det;
}


template <class T>
T Matrix<T>::det(MOD MatMgr<T> & mgr) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size >= 1 && m_col_size >= 1, ("strange matrix"));
    if (!is_quad()) {
        return T(0);
    }
    T detv;
    switch (m_row_size) {
    case 1:
        return get(0,0);
    case 2:
        detv = get(0,0) * get(1,1) - get(0,1) * get(1,0);
        return detv;
    case 3:
        if (isUpTriangular() || isLowTriangular()) {
            detv = get(0,0)*get(1,1)*get(2,2);
        } else if (isAntiUpTriangular() || isAntiLowTriangular()) {
            T sign = ((m_row_size * (m_row_size - 1))/2) % 2 == 0 ?
                     T(1) : T(-1);
            detv = get(2,0) * get(1,1) * get(0,2) * sign;
        } else {
            detv = get(0,0) * get(1,1) * get(2,2) +
                   get(1,0) * get(2,1) * get(0,2) +
                   get(0,1) * get(1,2) * get(2,0) -
                   get(0,2) * get(1,1) * get(2,0) -
                   get(0,1) * get(1,0) * get(2,2) -
                   get(2,1) * get(1,2) * get(0,0);
        }
        return detv;
    default: break;
    }
    if (isUpTriangular() || isLowTriangular()) {
        detv = 1;
        for (UINT i = 0; i < m_row_size; i++) {
            detv = detv * get(i, i);
        }
        return detv;
    }
    if (isAntiUpTriangular() || isAntiLowTriangular()) {
        detv = 1;
        for (UINT i = 0; i < m_row_size; i++) {
            detv = detv * get(i, m_row_size - 1 - i);
        }
        return detv;
    }
    //#define DET_M1 //M1 is slow slightly
    #ifdef DET_M1
    detv = FullPermutation(m_row_size);
    #else
    MatWrap<T> t1(*this, mgr); Matrix<T> & a = t1.m();
    UINT r = 0, j;
    for (j = 0; j < a.m_col_size; j++) {
        //Finding the perfect pivot entry or permuting of
        //non-zero pivot entry.
        INT swap_row = -1;
        T swap_entry;
        for (UINT k = j; k < a.m_row_size; k++) {
            T tmp = a.get(k,j);
            if (equalElem(tmp, T(0))) {
                //zero entry
                continue;
            }
            if (swap_row == -1) {
                //The first non-zero entry is the candidate,
                swap_row = k;
                swap_entry = tmp;
                if (swap_entry == T(1)) {
                    break;
                }
            } else if (tmp == T(1)) {
                //but we are glad to find the row with
                //unitary entry as the pivot,
                swap_row = k;
                break;
            } else if (abs(swap_entry) < abs(tmp)) {
                //or always find the maximum entry as pivot entry.
                swap_row = k;
                swap_entry = tmp;
            }
        }
        if (swap_row == -1) {
            //Not find nonzero element.
            //There are more than 2 of zeros in one column.
            //And matrix is singular.
            return T(0);
        }
        if (swap_row != (INT)j) {
            //Ensure diagonal nonzero.
            a.interchRow(swap_row, j);
            r++;
        }

        //Reduce other column entries below pivot entry to zero.
        for (UINT i = j+1; i < a.m_row_size; i++) {
            if (!equalElem(a.get(i, j), T(0))) {
                T tmp = - (a.get(i, j) / a.get(j, j));
                a.mulAndAddRow(j, tmp, i);
            }
        }
    }
    detv = 1;
    for (j = 0; j < a.m_col_size; j++) {
        detv = detv * a.get(j,j);
    }
    if (ODD(r)) {
        detv = -(detv);
    }
    #endif
    return detv;
}


template <class T>
bool Matrix<T>::inv(OUT Matrix<T> & e, MatMgr<T> & mgr) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size >= 1 && m_col_size >= 1, ("strange matrix"));
    if (!is_quad()) {
        e.deleteAllElem();
        return false;
    }

    //Use tmp matrix to avoid the function changes current
    //matrix's original value.
    MatWrap<T> t1(*this, mgr); Matrix<T> & p = t1.m();

    //Calc INV by specific row size.
    switch (m_row_size) {
    case 1:
        //Initialize result matrix.
        if (this == &e) { e.zero(); }
        else {
            //unitary matrix
            e.reinit(m_row_size, m_col_size);
        }
        ASSERT0(!equalElem(p.get(0, 0), T(0)));
        e.set(0, 0, T(1) / p.get(0,0));
        return true; //non-singular
    case 2: {
        //Initialize result matrix.
        if (this == &e) { e.zero(); }
        else {
            //unitary matrix
            e.reinit(m_row_size, m_col_size);
        }

        //1/(ad-bc)(d -b)
        //         (-c a)
        T k = p.get(0,0) * p.get(1,1) - p.get(0,1) * p.get(1,0);
        if (equalElem(k, T(0))) {
            e.deleteAllElem();
            return false; //singular
        }
        k = T(1) / k;
        e.set(0, 0, p.get(1,1));
        e.set(1, 1, p.get(0,0));
        e.set(0, 1, T(-1) * p.get(0,1));
        e.set(1, 0, T(-1) * p.get(1,0));
        e.mul(k);
        return true; //non-singular
    }
    default:
        e.reinit(m_row_size, m_col_size);
        break;
    }
    e.initIden(1);
    bool is_nonsingular = false;
    for (UINT j = 0; j < p.m_col_size; j++) {
        //Finding the perfect pivot entry or permuting of
        //non-zero pivot entry.
        INT swap_row = -1;
        T swap_entry;
        for (UINT k = j; k < p.m_row_size; k++) {
            T tmp = p.get(k,j);
            if (equalElem(tmp, T(0))) { //zero entry
                continue;
            }
            if (swap_row == -1) {
                //The first non-zero entry is the candidate,
                swap_row = k;
                swap_entry = tmp;
                if (swap_entry == T(1)) {
                    break;
                }
            } else if (tmp == T(1)) {
                //But we are glad to find the row with
                //unitary entry as the pivot.
                swap_row = k;
                break;
            } else if (abs(swap_entry) < abs(tmp)) {
                //or always find the maximum entry as pivot entry.
                swap_row = k;
                swap_entry = tmp;
            }
        }
        if (swap_row == -1) {
            is_nonsingular = false;
            e.deleteAllElem();
            goto FIN;
        }
        if (swap_row != (INT)j) {
            p.interchRow(swap_row, j);
            e.interchRow(swap_row, j);
        }

        //Reduce diagonal entry to unitary
        if (p.get(j,j) != T(1)) {
            T tmp = T(1) / p.get(j,j);
            p.mulOfRow(j, tmp);
            e.mulOfRow(j, tmp);
        }

        //Reduce other column entries below pivot entry to zero.
        for (UINT i = 0; i < p.m_row_size; i++) {
            if (i != j) {
                if (!equalElem(p.get(i, j), T(0))) {
                    T tmp = T(-1) * p.get(i, j);
                    p.mulAndAddRow(j, tmp, i);
                    e.mulAndAddRow(j, tmp, i);
                }
            }
        }
    }
    is_nonsingular = true;
FIN:
    return is_nonsingular;
}


template <class T>
void Matrix<T>::frd(OUT Matrix<T> & f, OUT Matrix<T> & g, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 1 && m_col_size > 1, ("strange matrix"));
    MatWrap<T> t1(mgr); Matrix<T> & tmp = t1.m();
    UINT rankr = rank(mgr, &tmp, true);
    g.growRow(tmp, 0, rankr - 1);
    UINT rank_row = 0;
    for (UINT j = 0; j < tmp.m_col_size; j++) {
        if (!equalElem(tmp.get(rank_row, j), T(1))) { continue; }
        f.growCol(*this, j, j);
        rank_row++;
        if (rank_row == rankr) {
            break;
        }
    }
}


template <class T>
bool Matrix<T>::linv(OUT Matrix<T> & x, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 1 && m_col_size > 1, ("strange matrix"));
    ASSERTN(m_row_size >= m_col_size, ("unexpected matrix"));
    x.deleteAllElem();
    MatWrap<T> t1(*this, mgr); Matrix<T> & transm = t1.m();
    MatWrap<T> t2(mgr); Matrix<T> & prod = t2.m();
    transm.trans();
    mul(transm, *this, prod);
    if (!prod.inv(prod, mgr)) { return false; }
    mul(prod, transm, x);
    return true;
}


template <class T>
bool Matrix<T>::rinv(OUT Matrix<T> & x, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 1 && m_col_size > 1, ("strange matrix"));
    ASSERTN(m_row_size <= m_col_size, ("unexpected matrix"));
    x.deleteAllElem();
    MatWrap<T> t1(*this, mgr); Matrix<T> & transm = t1.m();
    MatWrap<T> t2(mgr); Matrix<T> & prod = t2.m();
    transm.trans();
    mul(*this, transm, prod);
    if (!prod.inv(prod, mgr)) {
        return false;
    }
    mul(transm, prod, x);
    return true;
}


template <class T>
void Matrix<T>::mpinv(OUT Matrix<T> & x, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    refinv(x, mgr);
}


template <class T>
void Matrix<T>::lstinv(OUT Matrix<T> & x, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    refinv(x, mgr);
}


template <class T>
void Matrix<T>::minv(OUT Matrix<T> & x, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    refinv(x, mgr);
}


template <class T>
void Matrix<T>::refinv(OUT Matrix<T> & x, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 1 && m_col_size > 1, ("strange matrix"));
    MatWrap<T> f(0, 0, mgr);
    MatWrap<T> g(0, 0, mgr);
    MatWrap<T> fl(0, 0, mgr);
    MatWrap<T> gr(0, 0, mgr);
    frd(f.m(), g.m(), mgr);
    f.m().linv(fl.m(), mgr);
    g.m().rinv(gr.m(), mgr);
    mul(gr.m(), fl.m(), x);
}


template <class T>
bool Matrix<T>::ginv(OUT Matrix<T> & x, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 1 && m_col_size > 1, ("strange matrix"));
    if (rank(mgr) == MIN(m_row_size, m_col_size)) {
        if (m_row_size == m_col_size) {
            //Nonsingular matrix inverse
            return inv(x, mgr);
        }

        //Computing Moore-Penrose inverse.
        MatWrap<T> t1(*this, mgr); Matrix<T> & trans_m = t1.m();
        trans_m.trans();
        Matrix<T> * A = nullptr,* At = nullptr;
        if (m_row_size < m_col_size) {
            At = this;
            A = &trans_m;
        } else {
            A = this;
            At = &trans_m;
        }

        //Ginv = ((At*A)^-1)*At
        MatWrap<T> t2(mgr); Matrix<T> & quad = t2.m();
        mul(*At, *A, quad);
        bool res = quad.inv(quad, mgr);
        DUMMYUSE(res);
        ASSERTN(res, ("quad should be invertible!"));
        mul(quad, *At, x);
        if (m_row_size < m_col_size) {
            x.trans();
        }

        //hack test
        MatWrap<T> t3(mgr); Matrix<T> & t = t3.m();
        mul(*this, x, t, mgr);
        mul(t, *this, t, mgr);
        //
        return false;
    }

    //Computing matrix {1}-inverse
    //Generate P,Q, that makes PAQ=J, where J is [I 0],
    //                                           [0 0]
    //and Ag is Q[I X]P.
    //           [Y Z]
    MatWrap<T> t1(m_row_size, m_row_size, mgr); Matrix<T> & P = t1.m();
    MatWrap<T> t2(m_col_size, m_col_size, mgr); Matrix<T> & Q = t2.m();
    MatWrap<T> t3(*this, mgr); Matrix<T> & tmpthis = t3.m();
    P.initIden(1);
    Q.initIden(1);
    UINT rankr = 0, row, col;

    //Generating nonsingular matrix P, that obtained by performing the
    //same operations which to reduce 'A' to the matrix with (rank*rank)
    //identity sub-matrix on left-corner on the identity matrix.

    //Generating P by row elementary operations.
    for (row = 0, col = 0;
        row < tmpthis.m_row_size && col < tmpthis.m_col_size;
        row++, col++) {
        //Finding the perfect pivot entry or permuting of non-zero pivot entry.
        INT swap_row = -1;
        T swap_entry;

        //Find  column with non-zero entry. 'w' meas 'which'
        for (UINT w = col; w < tmpthis.m_col_size; w++) {
            for (UINT k = row; k < tmpthis.m_row_size; k++) {
                T tmp = tmpthis.get(k, w);
                if (equalElem(tmp, T(0))) {
                    continue;
                }
                if (swap_row == -1) {
                    //The first non-zero entry is the candidate
                    swap_row = k;
                    swap_entry = tmp;
                    if (swap_entry == 1) {
                        break;
                    }
                } else if (tmp == 1) {
                    //We are preferable to interchange the row
                    //with unitary entry as the pivot
                    swap_row = k;

                    swap_entry = tmp;
                    break;
                } else if (abs(swap_entry) < abs(tmp)) {
                    //Find the maximum entry as pivot entry.
                    swap_row = k;
                    swap_entry = tmp;
                }
            }
            if (swap_row == -1) {
                continue; //Try next column, trapezoid matrix
            }
            if (swap_row != (INT)row) {
                tmpthis.interchRow(swap_row, row);
                P.interchRow(swap_row, row);
            }
            col = w;
            break;
        }
        if (swap_row == -1) {
            goto FIN; //All elements of row 'row' are zero.
        }

        //Reducing pivot entry (row, col) to unitary.
        if (tmpthis.get(row, col) != 1) {
            //Notice, quotient may be zero if den less than 1.
            T tmp = 1 / (tmpthis.get(row, col));
            tmpthis.mulOfRow(row, tmp);
            P.mulOfRow(row, tmp);
        }

        //Reducing other column-wise entries except for the pivot entry
        //to zero.
        UINT strow = 0;
        for (UINT i = strow; i < tmpthis.m_row_size; i++) {
            if (i != row) {
                //For reducing the computation cost.
                if (!equalElem(tmpthis.get(i, col), T(0))) {
                    //T tmp = -1 * ptr->get(i, col);
                    T tmp = -(tmpthis.get(i, col)) /
                            tmpthis.get(row, col);
                    tmpthis.mulAndAddRow(row, tmp, i);
                    P.mulAndAddRow(row, tmp, i);
                }
            }
        }
        rankr++;
        tmpthis.adjust();
    } //end for (INT row = 0...
FIN:
    ///hack TEST
    //Matrix<T> t = P**this;
    //

    //Generating Q by column elementary operations
    for (row = 0; row < rankr; row++) {
        if (!equalElem(tmpthis.get(row, row), T(1))) {
            //It is necessary to interchange second column with third.
            //CASE: 1 0 0 *
            //      0 0 1 *
            //    =>
            //      1 0 0 *
            //      0 1 0 *
            bool find = false;
            for (UINT col = row + 1; col < tmpthis.m_col_size; col++) {
                if (equalElem(tmpthis.get(row, col), T(1))) {
                    tmpthis.interchCol(row, col);
                    Q.interchCol(row, col);
                    find = true;
                    break;
                }
            }
            ASSERTN(find, ("exception occurring!"));
        }
        for (UINT col = row + 1; col < tmpthis.m_col_size; col++) {
            if (!equalElem(tmpthis.get(row, col), T(0))) {
                T tmp = -1 * tmpthis.get(row, col);
                tmpthis.mulAndAddCol(row, tmp, col);
                Q.mulAndAddCol(row, tmp, col);
            }
        }
    }

    ///hack TEST
    //t = P**this*Q;
    ///

    MatWrap<T> t4(rankr, rankr, mgr); Matrix<T> & I = t4.m();
    I.initIden(1);
    I.growRowAndCol(Q.m_col_size - rankr, P.m_row_size - rankr);
    mul(Q, I, Q, mgr);
    mul(Q, P, x, mgr); //x = Q*I*P;

    ///hack TEST
    //t = *this * x * *this;
    ///
    return false;
}


template <class T>
void Matrix<T>::swap(MOD Matrix<T> & src)
{
    bool cur_m_is_init = m_is_init;
    UINT cur_m_row_size = m_row_size;
    UINT cur_m_col_size = m_col_size;
    T * cur_m_mat = m_mat;
    m_is_init = src.m_is_init;
    m_row_size = src.m_row_size;
    m_col_size = src.m_col_size;
    m_mat = src.m_mat;
    src.m_is_init = cur_m_is_init;
    src.m_row_size = cur_m_row_size;
    src.m_col_size = cur_m_col_size;
    src.m_mat = cur_m_mat;
}


template <class T>
void Matrix<T>::setdiag(Vector<T> const& v, UINT vlen)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(),("must be square"));
    ASSERTN(vlen == m_row_size,("unmatch vector"));
    for (UINT i = 0; i < m_row_size; i++) {
        set(i, i, v[i]);
    }
}


//Set 'v' to row vector by diagonal entry.
template <class T>
void Matrix<T>::getdiag(OUT Matrix<T> & v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0, ("matrix is empty."));
    UINT b = MIN(m_row_size, m_col_size);
    v.reinit(1, b);
    for (UINT i = 0; i < b; i++) {
        v.set(0, i, get(i, i));
    }
}


template <class T>
void Matrix<T>::setdiag(Matrix<T> const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(), ("must be square"));
    ASSERTN(v.is_vec(), ("must be vector"));
    ASSERTN(v.getSize() == m_row_size, ("out of boundary"));
    if (v.is_rowvec()) {
        for (UINT i = 0; i < m_row_size; i++) {
            set(i, i, v.get(0,i));
        }
        return;
    }
    for (UINT i = 0; i < m_row_size; i++) {
        set(i, i, v.get(i, 0));
    }
}


template <class T>
void Matrix<T>::initIden(T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(),("identity matrix must be square"));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            if (i == j) {
                set(i, j, v);
                continue;
            }
            set(i, j, 0);
        }
    }
}


template <class T>
void Matrix<T>::zero()
{
    ASSERTN(m_is_init, ("not yet initialize."));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            set(i, j, T(0));
        }
    }
}


template <class T>
void Matrix<T>::fzero()
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (m_mat != nullptr) {
        cleanTBuf(m_mat, m_row_size * m_col_size);
    }
}


template <class T>
bool Matrix<T>::isLowTriangular() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(),("must be square"));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = i + 1; j < m_col_size; j++) {
            if (!equalElem(get(i, j), T(0))) {
                return false;
            }
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::isAntiUpTriangular() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(),("must be square"));
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size - 1 - i; j++) {
            if (!equalElem(get(i, j), T(0))) {
                return false;
            }
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::is_rowvec() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    return m_row_size == 1;
}


template <class T>
bool Matrix<T>::is_colvec() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    return m_col_size == 1;
}


template <class T>
bool Matrix<T>::is_vec() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    return is_rowvec() || is_colvec();
}


template <class T>
bool Matrix<T>::is_rowequ(UINT row, T const& v) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size, ("out of boundary"));
    for (UINT i = 0;  i < m_col_size; i++) {
        if (!equalElem(get(row, i), v)) {
            return false;
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::is_rowequ(UINT row, Matrix<T> const& m, UINT mrow) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size && mrow < m.m_row_size &&
            m_col_size == m.m_col_size, ("out of boundary"));
    for (UINT i = 0;  i < m_col_size; i++) {
        if (!equalElem(get(row, i), m.get(mrow, i))) {
            return false;
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::is_colequ(UINT col, T const& v) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(col < m_col_size, ("out of boundary"));
    for (UINT i = 0;  i < m_row_size; i++) {
        if (!equalElem(get(i, col), v)) {
            return false;
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::is_colequ(UINT col, Matrix<T> const& m, UINT mcol) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(col < m_col_size && mcol < m.m_col_size &&
            m_row_size == m.m_row_size, ("out of boundary"));
    for (UINT i = 0;  i < m_row_size; i++) {
        if (!equalElem(get(i, col), m.get(i, mcol))) {
            return false;
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::isUpTriangular() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(),("must be square"));
    for (UINT j = 0; j < m_col_size; j++) {
        for (UINT i = j + 1; i < m_row_size; i++) {
            if (!equalElem(get(i, j), T(0))) {
                return false;
            }
        }
    }
    return true;
}


template <class T>
bool Matrix<T>::isAntiLowTriangular() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(),("must be square"));
    for (UINT j = 0; j < m_col_size; j++) {
        for (UINT i = m_row_size - 1; i > m_row_size - 1 - j; i--) {
            if (!equalElem(get(i, j), T(0))) {
                return false;
            }
        }
    }
    return true;
}


template <class T>
void Matrix<T>::eche(MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    MatWrap<T> tmp(mgr);
    rank(mgr, &tmp.m());
    copy(tmp);
}


template <class T>
void Matrix<T>::basis(OUT Matrix<T> & b, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    UINT r = rank(mgr, &b, false);
    if (r < b.m_row_size) { //b is not full rank.
        MatWrap<T> tmp(mgr);
        b.inner(tmp.m(), 0, 0, r-1, b.m_col_size-1);
        b.copy(tmp);
    }
}


template <class T>
void Matrix<T>::algc(UINT row, UINT col, OUT Matrix<T> & m)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size && col < m_col_size, ("not an entry"));
    m.reinit(m_row_size - 1, m_col_size - 1);
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            if (i == row || j == col) {
                continue;
            }
            UINT ni = i, nj = j;
            if (ni > row) {
                ni--;
            }
            if (nj > col) {
                nj--;
            }
            m.set(ni, nj, get(i, j));
        }
    }
}


template <class T>
bool Matrix<T>::adj(OUT Matrix<T> & m, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(), ("adj for rectangle matrix"));
//#define BY_DEFINITION //Use the your favorite method.
#ifdef BY_DEFINITION
    m.reinit(m_row_size, m_col_size);
    switch (m_row_size) {
    case 2:
        m.set(0, 0, get(1,1));
        m.set(0, 1, -get(0,1));
        m.set(1, 0, -get(1,0));
        m.set(1, 1, get(0,0));
        break;
    default: {
        MatWrap<T> tmp(mgr);
        for (UINT i = 0; i < m_row_size; i++) {
            for (UINT j = 0; j < m_col_size; j++) {
                algc(i, j, tmp.m());
                T c = ODD(i + j) ? -1 : 1;
                m.set(i, j, c * tmp.m().det(mgr));
            }
        }
        m.trans();
    }
    }
#else //BY INVERSING Matrix
    T dv = this->det(mgr);
    if (equalElem(dv, T(0))) {
        return false;
    }
    inv(m, mgr);
    m.mul(dv);
#endif
    return true;
}


template <class T>
void Matrix<T>::nullspace(OUT Matrix<T> & ns, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ns.reinit(m_col_size, m_col_size);
    MatWrap<T> t1(mgr); Matrix<T> & tmp = t1.m();

    //Reduce matrix to echelon form.
    //This operation usually generates the general form solution of Ax=0.
    //Since augmented vector be always zero, we ignored last column during
    //each step.
    rank(mgr, &tmp, true);

    //Format the generating solution to combination of vectors, and
    //using free(unconstrained) variables as the weight of vector.
    //e.g:
    // The combination of vectors:
    // [x1,x2,x3,x4] = x1*[0,0,0,0] + x2*[2,1,0,0] +
    //                 x3*[0,0,0,0] + x4*[1,0,-2,1]
    ns.initIden(1);
    //INT unum = tmp.m_col_size; //Number of variable.
    for (UINT nsrow = 0, row = 0; row < tmp.m_row_size; row++) {
        //Find non-zero leading entry.
        UINT col;
        bool find_non_zero = false;
        for (col = row; col < tmp.m_col_size; col++) {
            if (!equalElem(tmp.get(row, col), T(0))) {
                nsrow = col;
                find_non_zero = true;
                break;
            }
        }
        if (!find_non_zero) {
            break;
        }
        ns.set(nsrow, col, 0);
        for (UINT k = col + 1; k < tmp.m_col_size; k++) {
            ns.set(nsrow, k, -(tmp.get(row, k)));
        }
    }
}


template <class T>
T Matrix<T>::tr()
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(), ("must be quad"));
    T trv = T(0); //allocate on stack
    for (UINT i = 0; i < m_row_size; i++) {
        trv = trv + get(i, i);
    }
    return trv;
}



template <class T>
UINT Matrix<T>::rank(MOD MatMgr<T> & mgr, OUT Matrix<T> * basis,
                     bool is_unitarize) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 0 && m_col_size > 0, ("strange matrix"));
    UINT rankv = 0;
    Matrix<T> * ptr = nullptr;
    Matrix<INT> * rowpos = nullptr; //record change process of rows.
    if (basis != nullptr) {
        ptr = basis;
    } else {
        ptr = mgr.allocMat(m_row_size, m_col_size);
        is_unitarize = true;
    }
    ASSERT0(ptr != this);
    ptr->copy(*this);
    if (!is_unitarize) {
        rowpos = mgr.allocIntMat(ptr->getRowSize(), 1);
        for (UINT i = 0; i < rowpos->getRowSize(); i++) {
            rowpos->set(i, 0, i);
        }
    }
    for (UINT row = 0, col = 0;
        row < ptr->m_row_size && col < ptr->m_col_size;
        row++, col++) {
        //Finding the perfect pivot entry or permuting
        //of non-zero pivot entry.
        INT swap_row = -1;
        T swap_elem = T(0);

        //Find  column with non-zero entry. 'w' meas 'which'
        for (UINT w = col; w < ptr->m_col_size; w++) {
            for (UINT k = row; k < ptr->m_row_size; k++) {
                T tmp = ptr->get(k, w);
                if (equalElem(tmp, T(0))) {
                    continue;
                }
                if (swap_row == -1) {
                    //The first non-zero element is the candidate.
                    swap_row = k;
                    swap_elem = tmp;
                    if (swap_elem == T(1)) {
                        break;
                    }
                } else if (tmp == T(1)) {
                    //We prefered regarding the row with
                    //unitary entry as the pivot.
                    swap_row = k;
                    swap_elem = tmp;
                    break;
                } else if (abs(swap_elem) < abs(tmp)) {
                    //Find the maximum entry as pivot entry.
                    swap_row = k;
                    swap_elem= tmp;
                }
            }
            if (swap_row == -1) {
                continue; //Try next column, trapezoid matrix
            }
            if (swap_row != (INT)row) {
                ptr->interchRow(swap_row, row);
                if (rowpos != nullptr) {
                    rowpos->interchRow(swap_row, row);
                }
            }
            col = w;
            break;
        }
        if (swap_row == -1) {
            goto FIN; //All elements of 'row' are zero.
        }

        //Reducing pivot entry (row, col) to unitary.
        if (is_unitarize && ptr->get(row, col) != T(1)) {
            //Notice, quotient may be zero if den less than 1.
            T tmp = T(1) / (ptr->get(row, col));
            ptr->mulOfRow(row, tmp);
        }

        //Eliminate all other elements at current column to zero, except
        //for the pivot element.
        UINT start_row = 0; //elminate all elment of column
        if (!is_unitarize) {
            start_row = row + 1;
        }
        for (UINT i = start_row; i < ptr->m_row_size; i++) {
            if (i != row) {
                if (!equalElem(ptr->get(i, col), T(0))) {
                    //T tmp = -1 * ptr->get(i, col);
                    T tmp = -(ptr->get(i, col)) / ptr->get(row, col);
                    ptr->mulAndAddRow(row, tmp, i);
                }
            }
        }
        rankv++;
        ptr->adjust();
    } //end for (INT row = 0...
FIN:
    if (rowpos != nullptr) {
        ASSERT0(ptr == basis);
        ASSERT0(rankv <= rowpos->getRowSize());
        if (rankv < this->getRowSize()) {
            ptr->reinit(rankv, ptr->getColSize());
            for (UINT i = 0; i < rankv; i++) {
                ptr->setRows(i, i, *this, rowpos->get(i, 0));
            }
        }
        mgr.freeIntMat(rowpos);
    }
    if (ptr != basis) {
        mgr.freeMat(ptr);
    }
    return rankv;
}


template <class T>
void Matrix<T>::insertRowBefore(UINT ridx, MOD MatMgr<T> & mgr, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(ridx < m_row_size && m_row_size > 0, ("out of bound"));
    MatWrap<T> t1(mgr); Matrix<T> & tmp = t1.m();
    if (ridx == 0) {
        tmp.growCol(m_col_size, v);
        tmp.growRow(*this, 0, m_row_size - 1);
        *this = tmp;
        return;
    }
    inner(tmp, 0, 0, ridx-1, m_col_size - 1);
    tmp.growRow(1, v);
    tmp.growRow(*this, ridx, m_row_size - 1);
    *this = tmp;
}


template <class T>
void Matrix<T>::insertRowsBefore(
    UINT ridx, UINT rnum, MOD MatMgr<T> & mgr, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(ridx < m_row_size && m_row_size > 0, ("out of bound"));
    MatWrap<T> t1(mgr); Matrix<T> & tmp = t1.m();
    if (rnum == 0) {
        return;
    }
    if (ridx == 0) {
        tmp.growCol(m_col_size, v);
        tmp.growRow(rnum - 1, v);
        tmp.growRow(*this, 0, m_row_size - 1);
        *this = tmp;
        return;
    }
    inner(tmp, 0, 0, ridx-1, m_col_size - 1);
    tmp.growRow(rnum, v);
    tmp.growRow(*this, ridx, m_row_size - 1);
    *this = tmp;
}


template <class T>
void Matrix<T>::insertRowsBefore(
    UINT ridx, Matrix<T> const& m, UINT mfrom, UINT mto, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    ASSERTN(ridx < m_row_size && m_row_size > 0 &&
            mfrom < m.m_row_size && mto < m.m_row_size && mfrom <= mto,
            ("out of bound"));
    MatWrap<T> t1(mgr); Matrix<T> & tmp = t1.m();
    if (ridx == 0) {
        tmp.growCol(m_col_size);
        tmp.growRow(m, mfrom, mto);
        tmp.growRow(*this, 0, m_row_size - 1);
        tmp.deleteRow(0);
        *this = tmp;
        return;
    }
    inner(tmp, 0, 0, ridx - 1, m_col_size - 1);
    tmp.growRow(m, mfrom, mto);
    tmp.growRow(*this, ridx, m_row_size - 1);
    *this = tmp;
}


template <class T>
void Matrix<T>::insertColumnBefore(UINT cidx, MOD MatMgr<T> & mgr, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(cidx < m_col_size && m_col_size > 0, ("out of bound"));
    MatWrap<T> t1(mgr); Matrix<T> & tmp = t1.m();
    if (cidx == 0) {
        tmp.growRow(m_row_size, v);
        tmp.growCol(*this, 0, m_col_size - 1);
        copy(tmp);
        return;
    }
    inner(tmp, 0, 0, m_row_size - 1, cidx - 1);
    tmp.growCol(1, v);
    tmp.growCol(*this, cidx, m_col_size - 1);
    copy(tmp);
}


template <class T>
void Matrix<T>::insertColumnsBefore(
    UINT cidx, UINT cnum, MOD MatMgr<T> & mgr, T const& v)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(cidx < m_col_size && m_col_size > 0, ("out of bound"));
    MatWrap<T> t1(mgr); Matrix<T> & tmp = t1.m();
    if (cnum == 0) {
        return;
    }
    if (cidx == 0) {
        tmp.growRow(m_row_size, v);
        tmp.growCol(cnum - 1, v);
        tmp.growCol(*this, 0, m_col_size - 1);
        *this = tmp;
        return;
    }
    inner(tmp, 0, 0, m_row_size - 1, cidx - 1);
    tmp.growCol(cnum, v);
    tmp.growCol(*this, cidx, m_col_size - 1);
    *this = tmp;
}


template <class T>
void Matrix<T>::insertColumnsBefore(
    UINT cidx, Matrix<T> const& m, UINT mfrom, UINT mto, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init && m.m_is_init, ("not yet initialize."));
    ASSERTN(cidx < m_col_size && m_col_size > 0 &&
            mfrom < m.m_col_size && mto < m.m_col_size &&
            mfrom <= mto,    ("out of bound"));
    MatWrap<T> t1(mgr); Matrix<T> & tmp = t1.m();
    if (cidx == 0) {
        tmp.growRow(m_row_size);
        tmp.growCol(m, mfrom, mto);
        tmp.growCol(*this, 0, m_col_size - 1);
        tmp.deleteCol(0);
        *this = tmp;
        return;
    }
    inner(tmp, 0, 0, m_row_size - 1, cidx - 1);
    tmp.growCol(m, mfrom, mto);
    tmp.growCol(*this, cidx, m_col_size - 1);
    *this = tmp;
}


template <class T>
void Matrix<T>::inner(OUT Matrix<T> & in, UINT sr, UINT sl,
                      UINT er, UINT el) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(sr <= er && sl <= el && er < m_row_size && el < m_col_size,
            ("illegal info"));
    UINT row = er - sr + 1;
    UINT col = el - sl + 1;
    in.reinit(row, col);
    for (UINT i = sr; i <= er; i++) {
        for (UINT j = sl; j <= el; j++) {
            in.set(i - sr, j - sl, get(i, j));
        }
    }
}


//Get inner rows.
template <class T>
void Matrix<T>::innerRow(OUT Matrix<T> & in, UINT from, UINT to) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(from <= to && to < m_row_size, ("illegal parameter"));
    inner(in, from, 0, to, m_col_size - 1);
}


//Get inner columns.
template <class T>
void Matrix<T>::innerColumn(OUT Matrix<T> & in, UINT from, UINT to) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(from <= to && to < m_col_size, ("illegal parameter"));
    inner(in, 0, from, m_row_size - 1, to);
}


template <class T>
bool Matrix<T>::plu(OUT Matrix<T> & p, OUT Matrix<T> & l, OUT Matrix<T> & u,
                    MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 1 && m_col_size > 1, ("strange matrix"));
    bool succ = false;
    MatWrap<T> t1(mgr);
    Matrix<T> & permd = t1.m(); //Permuting this matrix by 'p'.
    UINT i;
    p.reinit(m_row_size, m_row_size); //Permute Matrix
    p.initIden(1);
    u = *this;
    for (UINT row = 0, col = 0; row < u.m_row_size && col < u.m_col_size;
         row++, col++) {
        //Finding the perfect pivot entry or permuting of non-zero pivot entry.
        INT swap_row = -1;
        T swap_entry;

        //Find column with non-zero entry. 'w' meas 'which'
        for (UINT w = col; w < u.m_col_size; w++) {
            for (UINT k = row; k < u.m_row_size; k++) {
                T tmp = u.get(k, w);
                if (equalElem(tmp, T(0))) {
                    continue;
                }
                if (swap_row == -1) {
                    swap_row = k; //The first non-zero entry is the candidate
                    swap_entry = tmp;
                    if (swap_entry == T(1)) {
                        break;
                    }
                } else if (abs(swap_entry) < abs(tmp)) {
                    //or always find the maximum entry as pivot entry.
                    swap_row = k;
                    swap_entry = tmp;
                }
            }
            if (swap_row == -1) {
                continue; //try next column, trapezoid matrix
            }
            if (swap_row != (INT)row) {
                u.interchRow(swap_row, row);
                p.interchRow(swap_row, row); //Record permutate matrix
            }
            col = w; //Record current non-zero column
            break;
        } //end foreach column
        if (swap_row == -1) {
            //All elements of 'row' are zero.
            goto FIN;
        }

        //Reduce other column entries which below pivot-entry to be zero.
        T topent = u.get(row, col);
        ASSERTN(!equalElem(topent, T(0)), ("zero entry"));
        for (i = row + 1; i < u.m_row_size; i++) {
            //To reduce the computation cost.
            if (!equalElem(u.get(i, col), T(0))) {
                T tmp = T(-1) * u.get(i, col) / topent;
                u.mulAndAddRow(row, tmp, i);
            }
        }
    }
    mul(p, *this, permd); //Permute current matrix by matrix 'p'.
    succ = permd.lu(l, u);
    if (succ) {
        //By using 'double' precison, the equation 'permd == l * u'
        //may be failed, because of accumulate error of precision.
        //But the result is correct.
        //ASSERTN(permd == l * u, ("unequal for PA=L*U after decomposition"));
    }
FIN:
    return succ;
}


template <class T>
bool Matrix<T>::lu(OUT Matrix<T> & l, OUT Matrix<T> & u)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size > 1 && m_col_size > 1, ("strange matrix"));
    bool succ = false;
    UINT i;
    u = *this;
    l.reinit(m_row_size, m_row_size);
    l.initIden(1);
    for (UINT row = 0, col = 0;
         row < u.m_row_size && col < u.m_col_size;
         row++, col++) {
        //Finding the perfect pivot entry or permuting
        //of non-zero pivot entry.
        INT swap_row = -1;
        T swap_entry;

        //Find  column with non-zero entry. 'w' meas 'which'
        for (UINT w = col; w < u.m_col_size; w++) {
            for (UINT k = row; k < u.m_row_size; k++) {
                T tmp = u.get(k, w);
                if (equalElem(tmp, T(0))) {
                    continue;
                }
                if (swap_row == -1) {
                    swap_row = k; //The first non-zero entry
                                  //is the candidate.
                    swap_entry = tmp;
                    break;
                }
            }
            if (swap_row == -1) {
                continue; //try next column, trapezoid matrix
            }
            if (swap_row != (INT)row) {
                //Can not reduce to up triangular form.
                //Theroem:
                //There is no LU decomposition can be find if A
                //has a singular sub matrix in upper-left corner.
                goto FIN;
            }
            col = w; //Record current non-zero column
            break;
        }
        if (swap_row == -1) {
            goto FIN; //All elements of row 'row' be zero.
        }

        for (i = row; i < l.m_row_size; i++) {
            l.set(i, row, u.get(i, col));
        }
        //Reduce other column entries below pivot entry to zero.
        T topent = u.get(row, col);
        ASSERTN(!equalElem(topent, T(0)), ("zero entry"));
        for (i = row + 1; i < u.m_row_size; i++) {
            if (!equalElem(u.get(i, col), T(0))) {
                T tmp = T(-1) * u.get(i, col) / topent;
                u.mulAndAddRow(row, tmp, i);
            }
        }//end for
    }//end for (INT row = 0...

    for (i = 0; i < l.m_row_size; i++) {
        //Reduce pivot entry to unitary, dividing  sub-entry by pivot entry.
        T tmp = T(1) / l.get(i, i);
        l.mulOfColumn(i, tmp);
    }
    succ = true;
FIN:
    return succ;
}


template <class T>
void Matrix<T>::qr(OUT Matrix<T> & q, OUT Matrix<T> & r,
                   MOD MatMgr<T> & mgr, bool calc_basis)
{
    ASSERTN(m_is_init, ("not yet initialize."));

    //Linear dependent to some of row vectors.
    if (calc_basis && rank(mgr) < m_row_size - 1) {
        MatWrap<T> bs(mgr);
        basis(bs.m(), mgr);
        bs.m().qr(q, r, mgr);
        return;
    }
    orth(q, mgr); //Computing orthogonal basis.
    q.nml(); //Normalizing orthogonal basis.
//#define QR_M2
#ifdef QR_M2
    //Method2, see NOTE.
    MatWrap<T> t1(*this, mgr); Matrix<T> & A = t1.m();
    A.trans();
    mul(q, A, r);

    //q is orthogonal basis set, and q should use col convention.
    //and trans(A) equals Q*R.
    q.trans();
#else
    //Method 1, see NOTE.
    //q is orthogonal basis set, and q should use col convention.
    //and trans(A) equals Q*R.
    q.trans();
    mul(*this, q, r);
    r.trans();
#endif
}


template <class T>
void Matrix<T>::nml()
{
    ASSERTN(m_is_init, ("not yet initialize."));
    for (UINT i = 0; i < m_row_size; i++) {
        T modv = modrow(i);
        if (equalElem(modv, T(0))) {
            continue;
        }
        mulOfRow(i, T(1) / modv);
    }
}


template <class T>
bool Matrix<T>::pqr(Matrix<T> & p, Matrix<T> & q, Matrix<T> & r)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    bool succ = false;
    ASSERTN(0, ("TODO"));
    return succ;
}


template <class T>
T Matrix<T>::dotrow(UINT row, Matrix<T> const& v) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(v.is_vec() && v.getSize() == m_col_size, ("not a vector."));
    return dot(row, 0, row, m_col_size - 1, v);
}


template <class T>
T Matrix<T>::dotcol(UINT col, Matrix<T> const& v) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(v.is_vec() && v.getSize() == m_row_size, ("not a vector."));
    return dot(0, col, m_row_size - 1, col, v);
}


template <class T>
T Matrix<T>::dot(Matrix<T> const& v) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(getSize() == v.getSize(), ("not a vector."));
    ASSERTN(v.m_row_size == 1 || v.m_col_size == 1,
            ("not a vector, only one row or one col"));
    ASSERTN(m_row_size ==1 || m_col_size == 1,
            ("not a vector, only one row or one col"));
    T dotv = 0;
    if (m_row_size == 1 && v.m_row_size == 1) {
        //'this' is row vector, 'v' is row vector
        for (UINT i = 0; i < m_col_size; i++) {
            dotv = dotv + (get(0, i) * v.get(0,i));
        }
        return dotv;
    }
    if (m_row_size == 1 && v.m_col_size == 1) {
        //'this' is row vector, 'v' is col vector
        for (UINT i = 0; i < m_col_size; i++) {
            dotv = dotv + (get(0, i) * v.get(i, 0));
        }
        return dotv;
    }
    if (m_col_size == 1 && v.m_col_size == 1) {
        //'this' is col vector, 'v' is col vector
        for (UINT i = 0; i < m_row_size; i++) {
            dotv = dotv + (get(i, 0) * v.get(i, 0));
        }
        return dotv;
    }
    if (m_col_size == 1 && v.m_row_size == 1) {
        //'this' is col vector, 'v' is row vector
        for (UINT i = 0; i < m_row_size; i++) {
            dotv = dotv + (get(i, 0) * v.get(0, i));
        }
        return dotv;
    }
    ASSERTN(0, ("illegal operation"));
    return dotv;
}


template <class T>
T Matrix<T>::dot(UINT srow, UINT scol, UINT erow, UINT ecol,
                 Matrix<T> const& v) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(srow <= erow && scol <= ecol &&
            (erow - srow + 1) * (ecol - scol + 1) == v.getSize(),
            ("not a vector."));
    ASSERTN(srow < m_row_size  && scol < m_col_size &&
            erow < m_row_size  && ecol < m_col_size, ("out of boundary."));
    ASSERTN(v.m_row_size ==1 || v.m_col_size == 1,
            ("not a vector, only one row or one col"));
    ASSERTN(srow == erow || scol == ecol,
            ("not a vector, only one row or one col"));
    T dotv = 0;
    if (srow == erow && v.m_row_size == 1) {
        //'this' is row vector, 'v' is row vector
        for (UINT i = scol; i <= ecol; i++) {
            dotv = dotv + (get(srow, i) * v.get(0, i));
        }
        return dotv;
    }
    if (scol == ecol && v.m_row_size == 1) {
        //'this' is col vector, 'v' is row vector
        for (UINT i = srow; i <= erow; i++) {
            dotv = dotv + (get(i, scol) * v.get(0, i));
        }
        return dotv;
    }
    if (scol == ecol && v.m_col_size == 1) {
        //'this' is col vector, 'v' is col vector
        for (UINT i = srow; i <= erow; i++) {
            dotv = dotv + (get(i, scol) * v.get(i, 0));
        }
        return dotv;
    }
    if (srow == erow && v.m_col_size == 1) {
        //'this' is row vector, 'v' is col vector
        for (UINT i = scol; i <= ecol; i++) {
            dotv = dotv + (get(srow, i) * v.get(i, 0));
        }
        return dotv;
    }
    ASSERTN(0,("illegal operation"));
    return dotv;
}


template <class T>
T Matrix<T>::dot(UINT srow, UINT scol, UINT erow, UINT ecol, Matrix<T> const& v,
                 UINT vsrow, UINT vscol, UINT verow, UINT vecol) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(srow <= erow &&
            scol <= ecol &&
            vsrow <= verow &&
            vscol <= vecol &&
            (erow - srow + 1) * (ecol - scol + 1) ==
            (verow - vsrow + 1) * (vecol - vscol + 1),
            ("not a vector."));
    ASSERTN(srow < m_row_size  && scol < m_col_size &&
            vsrow < v.m_row_size  && vscol < v.m_col_size &&
            erow < m_row_size  && ecol < m_col_size &&
            verow < v.m_row_size  && vecol < v.m_col_size,
            ("out of boundary."));
    ASSERTN(srow == erow || scol == ecol,
            ("not a vector, only one row or one col"));
    ASSERTN(vsrow == verow || vscol == vecol,
            ("not a vector, only one row or one col"));
    T dotv = 0;
    //'this' is row vector, 'v' is row vector
    if (srow == erow && vsrow == verow) {
        for (UINT i = scol; i <= ecol; i++) {
            dotv = dotv + (get(srow, i) * v.get(vsrow,i));
        }
        return dotv;
    }
    if (srow == erow && vscol == vecol) {
        //'this' is row vector, 'v' is col vector
        for (UINT i = scol; i <= ecol; i++) {
            dotv = dotv + (get(srow, i) * v.get(i, vscol));
        }
        return dotv;
    }
    if (scol == ecol && vsrow == verow) {
        //'this' is col vector, 'v' is row vector
        for (UINT i = srow; i <= erow; i++) {
            dotv = dotv + (get(i, scol) * v.get(vsrow, i));
        }
        return dotv;
    }
    if (scol == ecol && vscol == vecol) {
        //'this' is col vector, 'v' is col vector
        for (UINT i = srow; i <= erow; i++) {
            dotv = dotv + (get(i, scol) * v.get(i, vscol));
        }
        return dotv;
    }
    ASSERTN(0, ("illegal operation"));
    return dotv;
}


template <class T>
void Matrix<T>::cross(MOD Matrix<T> & v, OUT Matrix<T> & u)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(&u != this, ("output matrix is same as current one"));
    ASSERTN(m_row_size == 1 || m_col_size == 1,
            ("not a vector, only one row or one col"));
    if (&v == this) {
        u.reinit(1, MAX(v.getColSize(), v.getRowSize()));
        u.zero();
        return;
    }
    if (m_row_size == 1) {
        //'this' is row vector
        if (v.m_row_size == 1) {
            //'v' is row vector
            v.trans();
            cross(v, u);
            v.trans();
        } else {
            //'v' is col vector
            ASSERTN(m_col_size == 3 && v.getRowSize() == 3,
                    ("only valid for 3-dimension vector"));
            u.reinit(1, v.getRowSize());
            u.set(0, 0, get(0, 1) * v.get(2, 0) - get(0, 2) * v.get(1, 0));
            u.set(0, 1, get(0, 2) * v.get(0, 0) - get(0, 0) * v.get(2, 0));
            u.set(0, 2, get(0, 0) * v.get(1, 0) - get(0, 1) * v.get(0, 0));
        }
        return;
    }
    //'this' is col vector
    trans();
    cross(v, u);
    trans();
}


template <class T>
void Matrix<T>::orthn(OUT Matrix<T> & z, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    MatWrap<T> t1(mgr); Matrix<T> & b1 = t1.m();
    z.reinit(m_row_size, m_col_size);
    innerRow(b1, 0, 0); //first row vector.
    T dotv = b1.dot(b1);
    b1.mulOfRow(0, sqrtElem(T(1) / dotv)); //normalize first dim.
    z.setRow(0, b1);
    MatWrap<T> t2(mgr); Matrix<T> & allzi = t2.m();
    MatWrap<T> t3(mgr); Matrix<T> & zi = t3.m();
    MatWrap<T> t4(mgr); Matrix<T> & xi = t4.m();
    for (UINT row = 1; row < m_row_size; row++) {
        //Orthogonalizing: <Xn,Zi>*Zi
        allzi.reinit(1, z.m_col_size);
        for (INT i = row - 1; i >= 0; i--) { //Here index 'i' must be signed
            z.innerRow(zi, i, i); //(k-1)th ~ 0th basis
            T dv = dot(row, 0, row, m_col_size - 1, zi);
            zi.mul(dv);
            add(allzi, zi, allzi);
        }
        innerRow(xi, row, row); //kth vector.

        //Normalization: Zi = Zi/|Zi|
        sub(xi, allzi, xi);
        xi.mul(T(1) / sqrtElem(xi.dot(xi)));
        z.setRow(row, xi);
    }
}


template <class T>
void Matrix<T>::orth(OUT Matrix<T> & z, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    MatWrap<T> b1(mgr);
    z.reinit(m_row_size, m_col_size);
    innerRow(b1.m(), 0, 0); //first row vector
    z.setRow(0, b1.m());
    MatWrap<T> t1(mgr); Matrix<T> & allzi = t1.m();
    MatWrap<T> t2(mgr); Matrix<T> & zi = t2.m();
    MatWrap<T> t3(mgr); Matrix<T> & xi = t3.m();
    for (UINT row = 1; row < m_row_size; row++) {
        //Only orthogonalize: (<Xn,Zi>/<Zi,Zi>)*Zi
        allzi.reinit(1, z.m_col_size);
        for (INT i = row - 1; i >= 0; i--) { //'i' must be signed
            z.innerRow(zi, i, i); //(k-1)th~0th basis
            T dv_zz = zi.dot(zi);

            //CASE: If two rows of elements are equal, such as,
            //  [a,b,c]
            //  [1,1,1]
            //  [1,1,1]
            //  'dv_zz' will be zero!
            ASSERTN(dv_zz != T(0),
                ("dot() is zero, row '%d' migth be all zero", i));
            T dv_xz = dot(row, 0, row, m_col_size-1, zi) / dv_zz;
            zi.mul(dv_xz);
            add(allzi, zi, allzi);
        }
        innerRow(xi, row, row); //kth vector.

        //Store the component vector as the kth orthogonal
        //axis of row convention space.
        sub(xi, allzi, xi);
        z.setRow(row, xi);
    }
}


template <class T>
void Matrix<T>::mls(OUT Matrix<T> & x, Matrix<T> const& b, MatMgr<T> & mgr)
{
    //Method of least squares: trans(A)*A*x = trans(A)*b
    MatWrap<T> tw1(*this, mgr); Matrix<T> & A = tw1.m();
    MatWrap<T> tw2(mgr); Matrix<T> & t1 = tw2.m();
    MatWrap<T> tw3(mgr); Matrix<T> & t2 = tw3.m();
    A.trans();
    mul(A, b, t1); //trans(A)*b, then t1 will be vector
    mul(A, *this, t2);//trans(A)*A
    t2.growCol(t1); //t1 will be augment coefficient vector

    //For speed up, we do invoke nullspace() instead of sse(). It's just a
    //trick.
    t2.nullspace(x, mgr);

    //Contain the number of (m_row_size - 1) variables.
    x.deleteRow(x.m_row_size - 1);
}


template <class T>
bool Matrix<T>::sse(OUT Matrix<T> & x, Matrix<T> const& b, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size == b.m_row_size, ("unmatch format"));

    //TODO: Constant part should permit the support of
    //constant symbol as well.
    ASSERTN(b.is_colvec(), ("b must be col vector"));

    //Theorem: PA = LU, Ly = Pb, Ux = y
    if (det(mgr) != T(0)) {
        MatWrap<T> t1(mgr); Matrix<T> & u = t1.m();
        MatWrap<T> t2(mgr); Matrix<T> & l = t2.m();
        MatWrap<T> t3(mgr); Matrix<T> & p = t3.m();
        bool s = plu(p, l, u, mgr);
        ASSERT0_DUMMYUSE(s); //illegal solution if s is false.
        u.inv(u, mgr);
        l.inv(l, mgr);

        //x = u * l * p * b;
        mul(u, l, u, mgr);
        mul(u, p, u, mgr);
        mul(u, const_cast<Matrix<T>&>(b), x, mgr);
        return true;
    }

    //det() is zero, there are two case:
    //1.no solution 2.infinitely many solutions

    //Computing general solution.
    MatWrap<T> tmp(*this, mgr);

    //Here we use the function nullspace() to reduce and format one
    //generating solution.
    //However, function nullspace() regard the last column of 'tmp' as
    //one variable when we invoke growCol(b).
    //Consequently, to get the correct solution from output of nullspace(), the
    //sign of the last column must be negative value of original.
    tmp.m().growCol(b);
    tmp.m().nullspace(x, mgr);

    //There are number of (m_row_size - 1) variables.
    x.deleteRow(x.m_row_size - 1);
    x.mulOfColumn(x.getColSize() - 1, -1);
    return false;
}


template <class T>
T Matrix<T>::mod()
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_vec(), ("not a vector, only one row or one col"));
    T modv = T(0); //allocated on stack
    if (is_rowvec()) { //'this' is row vector
        for (UINT i = 0; i < m_col_size; i++) {
            T v = get(0, i);
            modv = modv + (v * v);
        }
    } else if (is_colvec()) { //'this' is col vector
        for (UINT i = 0; i < m_row_size; i++) {
            T v = get(i, 0);
            modv = modv + (v * v);
        }
    } else {
        ASSERTN(0, ("illegal operation"));
    }
    modv = sqrtElem(modv);
    return modv;
}


template <class T>
T Matrix<T>::modrow(UINT row)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(row < m_row_size, ("out of boundary"));
    T modv = T(0);
    for (UINT i = 0; i < m_col_size; i++) {
        T & v = get(row, i);
        modv = modv + (v * v);
    }
    modv = sqrtElem(modv);
    return modv;
}


template <class T>
T Matrix<T>::modcol(UINT col)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(col < m_col_size, ("out of boundary"));
    T modv = T(0);
    for (UINT i = 0; i < m_row_size; i++) {
        T & v = get(i, col);
        modv = modv + (v * v);
    }
    modv = sqrtElem(modv);
    return modv;
}


template <class T>
void Matrix<T>::eig(OUT Matrix<T> & eigv, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(), ("A must be square."));
    MatWrap<T> t1(*this, mgr); Matrix<T> & v = t1.m();
    MatWrap<T> t2(mgr); Matrix<T> & q = t2.m();
    MatWrap<T> t3(mgr); Matrix<T> & r = t3.m();
    UINT count = 0;
    while (!v.isLowTriangular() && count < MAX_QR_ITER_NUM) {
        #ifdef QR_ITER_WITH_SHIFT
        MatWrap<T> t4(m_row_size, m_col_size, mgr); Matrix<T> & i = t4.m();
        i.initIden(v.get(v.m_row_size -1 , v.m_col_size - 1));
        v = v - i;
        v.qr(q,r);
        v = r*q;
        v.trans(); //v must use row convention.
        v = v + i;
        #else
        v.qr(q, r, mgr);
        mul(r, q, v);
        v.trans(); //v must use row convention.
        #endif
        count++;
    }
    ASSERTN(v.is_quad() && v.is_homo(*this), ("A must be square."));

    //Sort diagonal eigvalue in decreasing order
    for (UINT i = 0; i < v.m_col_size - 1; i++) {
        T vi = v.get(i, i);
        for (UINT j = i + 1; j < v.m_col_size; j++) {
            T vj = v.get(j, j);
            if (vi < vj) { //swap elements
                T tmp = vi;
                v.set(i, i, vj);
                v.set(j, j, tmp);
            }
        }
    }
    eigv.reinit(1, m_col_size);
    for (UINT i = 0; i < m_row_size; i++) {
        eigv.set(0, i, v.get(i, i));
    }
}


template <class T>
void Matrix<T>::eig(OUT Matrix<T> & eigv, OUT Matrix<T> & eigx,
                    MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(), ("A must be square."));
    MatWrap<T> t1(mgr); Matrix<T> & v = t1.m();
    eig(v, mgr);
    v.adjust();
    eigv.reinit(v.getSize(), v.getSize());
    eigv.setdiag(v);
    eigx.reinit(0, 0);
    List<T> vlst;
    MatWrap<T> t2(mgr); Matrix<T> & x = t2.m();
    MatWrap<T> t3(mgr); Matrix<T> & nul = t3.m();
    for (UINT i = 0; i <  eigv.m_row_size; i++) {
        T lam = eigv.get(i, i);
        if (vlst.find(lam)) {
            continue;
        }
        vlst.append_tail(lam);

        //Computing nulA of ('this'-lam*I)
        x.reinit(m_row_size, m_col_size);
        x.initIden(lam); //lam*I
        sub(*this, x, x);
        x.nullspace(nul, mgr); //null space descripted in col convention.
        //Extract the non-zero col basis from null space of (A-lam*I)
        for (UINT j = 0; j < nul.m_col_size; j++) {
            bool zero_col = true;
            for (UINT w = 0; w < nul.m_row_size; w++) {
                if (!equalElem(nul.get(w, j), T(0))) {
                    zero_col = false;
                    break;
                }
            }
            if (zero_col) { continue; }
            Matrix<T> tmpcol;
            nul.innerColumn(tmpcol, j, j);
            eigx.growCol(tmpcol);
        }
    }
}


template <class T>
void Matrix<T>::proj(OUT Matrix<T> & p, Matrix<T> const& v, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_orth(mgr), ("need orthogonal"));
    ASSERTN(v.is_vec() && v.getSize() == m_col_size, ("must be vector"));
    p.reinit(1, m_col_size);
    MatWrap<T> t1(mgr); Matrix<T> & row = t1.m();
    for (UINT i = 0; i < m_row_size; i++) {
        innerRow(row, i, i);
        T dv = v.dot(row);
        if (equalElem(dv, T(0))) {
            continue;
        }
        dv = dv / row.dot(row);
        row.mul(dv);
        add(p, row, p);
    }
}


template <class T>
void Matrix<T>::setPartialElem(UINT num, ...)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(num <= getSize(), ("out of boundary."));
    if (num == 0) { return; }
    UINT row = 0, col = 0;
    va_list ptr;
    va_start(ptr, num);
    using CvtedPODType = typename TryCvtIfTypeIsNotPOD<
        IsTrivialType<T>::is_trivial, T>::type;
    for (UINT i = 0; i < num; i++) {
        set(row, col++, CvtedPODType(va_arg(ptr, CvtedPODType)));
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
    }
    va_end(ptr);

    //#define DEFAULT_VARIADIC_PARAMETER_ACCESS
    #ifdef DEFAULT_VARIADIC_PARAMETER_ACCESS
    //The following algorithm to access variadic parameter may not
    //compatible with current stack layout and stack alignment,
    //says the code work well on Linux with gcc4.8, but
    //it does not work on Windows with VS2015. Therefore
    //a better advise is to use va_arg().
    T * ptrt = (T*) (((BYTE*)(&num)) + sizeof(T));
    for (UINT i = 0; i < num; i++, ptrt++) {
        set(row, col++, T(*ptrt));
        if (col >= m_col_size) {
            row++;
            col = 0;
        }
    }
    #endif
}


template <class T>
bool Matrix<T>::svd(OUT Matrix<T> & u, OUT Matrix<T> & s, OUT Matrix<T> & eigx,
                    MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (m_row_size < m_col_size) {
        //Assuming that A is 2*3, then B=trans(A)*A is  3*3 matrix.
        //When we computing SVD of trans(A) instead of A, and B is A*trans(A),
        //the 2*2 matrix.
        //It is simpler to compute the EIGX,EIGV than those of A.
        //Because A=USV, so trans(A) = trans(V)*trans(S)*trans(U).
        MatWrap<T> transA(*this, mgr);
        transA.m().trans();
        bool res = transA.m().svd(u, s, eigx, mgr);
        eigx.trans();
        u.trans();
        s.trans();
        eigx.swap(u);
        return res;
    }
    MatWrap<T> t1(*this, mgr); Matrix<T> & a = t1.m();
    a.trans();
    mul(a, *this, a, mgr);
    a.adjust();
    MatWrap<T> t2(mgr); Matrix<T> & eigv = t2.m();
    MatWrap<T> t3(mgr); Matrix<T> & tmpeigx = t3.m();
    a.eig(eigv, tmpeigx, mgr); //tmpeigx uses col convention.

    //Eigvalues already be in decreasing order.
    tmpeigx.trans();
    tmpeigx.orthn(eigx, mgr); //eigx use row convention.
    eigx.adjust();
    if (eigx.m_row_size != this->m_col_size) {
        return false;
    }
    if (!eigx.is_quad()){ //eigx must be n*n
        return false;
    }

    //element of 's' must be initialized with 0
    s.reinit(m_row_size, m_col_size);

    //Computing stranger value s.
    //s = sqrtElem (eigen value);
    UINT scount = 0, i; //count of stranger value
    for (i = 0; i < MIN(s.m_row_size, eigv.m_row_size); i++) {
        T sv = eigv.get(i, i);
        if (equalElem(sv, T(0))) {
            break;
        }
        scount++;
        s.set(i, i, sqrtElem(sv));
    }

    //Computing Av1 and normalized it.
    MatWrap<T> t5(mgr); Matrix<T> & v = t5.m();
    MatWrap<T> t6(mgr); Matrix<T> & subU = t6.m(); //subU uses col convention.
    MatWrap<T> t7(mgr); Matrix<T> & tmp = t7.m();
    for (i = 0; i < scount; i++) {
        eigx.innerRow(v, i, i); //Get the 1th dimension axis.
        v.trans(); //v1 must be col vector
        mul(*this,  v, tmp); //tmp is col vector

        //tmp is 1th orthonormal basis of A, and also the first col vector
        //of U. delTArra1 is modular of Av1.
        tmp.mul(T(1) / s.get(i, i));

        subU.growRow(tmp);
    }

    //To construct U, we need the quad-matrix to present R^n space,
    //and the first rth axis composed 'subU'.
    //So we compute the NulA of u1, namely, the result
    //of 'RowA(subU) * x = 0'.
    if (scount < m_row_size) {
        MatWrap<T> t8(mgr); Matrix<T> & nulofu1 = t7.m();
        subU.nullspace(nulofu1, mgr); //nulofu1 uses col convention.

        //Compose U, with row convention arrangement.
        for (i = 0; i < nulofu1.m_col_size; i++) {
            if (nulofu1.is_colequ(i, 0)) { continue; }
            nulofu1.innerColumn(tmp, i, i);
            subU.growRow(tmp);
        }
        ASSERTN(subU.is_quad(), ("too many basis"));
    }

    //And then subU has orthogonalized u2, u3, ..., we need farther
    //orthogonalize u2, u3,... each other.
    subU.orthn(u, mgr); //subU use row convention, u use row convention.
    u.trans();
    return true;
}


template <class T>
bool Matrix<T>::diag(OUT Matrix<T> & p, OUT Matrix<T> & d, MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(is_quad(), ("only quad matirx"));
    eig(d, p, mgr); //d is diagonal, p uses col convention.
    if (!p.is_quad()) {
        //There were not enough eigen vectors to construct the basis of R^n.
        return false;
    }
    return true;
}


template <class T>
T Matrix<T>::norm(UINT p, MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    if (p == NORM_F) {
        T n = T(0);
        for (UINT i = 0; i < m_row_size; i++) {
            for (UINT j = 0; j < m_col_size; j++) {
                n = n + (get(i, j) * get(i, j));
            }
        }
        return sqrtElem(n);
    }
    if (p == NORM_1) {
        T n = T(0);
        if (is_vec()) { //vector norm
            for (UINT i = 0; i < m_row_size; i++) {
                for (UINT j = 0; j < m_col_size; j++) {
                    n = n + abs(get(i, j));
                }
            }
        } else { //matirx norm, row norm
            for (UINT i = 0; i < m_row_size; i++) {
                T tmp = 0;
                for (UINT j = 0; j < m_col_size; j++) {
                    tmp = tmp + abs(get(i, j));
                }
                n = MAX(n, tmp);
            }
        }
        return n;
    }
    if (p == NORM_2) {
        if (is_vec()) { //vector norm
            T n = T(0);
            for (UINT i = 0; i < m_row_size; i++) {
                for (UINT j = 0; j < m_col_size; j++) {
                    n = n + (get(i, j) * get(i, j));
                }
            }
            return sqrtElem(n);
        }

        //matrix norm
        MatWrap<T> tw1(*this, mgr); Matrix<T> & t1 = tw1.m();
        MatWrap<T> tw2(mgr); Matrix<T> & t2 = tw2.m();
        t1.trans();
        mul(t1, *this, t2);
        T res = sqrtElem(t2.sprad(mgr));
        return res;
    }
    if (p == NORM_INF) {
        T n = T(0);
        if (is_vec()) { //vector norm
            for (UINT i = 0; i < m_row_size; i++) {
                for (UINT j = 0; j < m_col_size; j++) {
                    n = MAX(n, abs(get(i, j)));
                }
            }
        } else {
            //matrix norm, column norm
            for (UINT j = 0; j < m_col_size; j++) {
                T tmp = 0;
                for (UINT i = 0; i < m_row_size; i++) {
                    tmp = tmp + abs(get(i, j));
                }
                n = MAX(n, tmp);
            }
        }
        return n;
    }
    ASSERTN(0, ("TODO:unknown norm type"));
    return 0;
}


template <class T>
bool Matrix<T>::cond(T & c, MatMgr<T> & mgr, UINT p)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    MatWrap<T> m(mgr);
    if (!inv(m.m(), mgr)) {
        return false;
    }
    c = norm(p, mgr) * m.m().norm(p, mgr);
    return true;
}


template <class T>
T Matrix<T>::sprad(MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    MatWrap<T> t1(mgr); Matrix<T> & v = t1.m();
    eig(v, mgr);
    T maxeigv = T(0); //allocated on stack
    for (UINT j = 0; j < m_col_size; j++) {
        if (j == 0) {
            maxeigv = abs(v.get(0, j));
        } else {
            maxeigv = MAX(maxeigv, abs(v.get(0, j)));
        }
    }
    return maxeigv;
}


template <class T>
void Matrix<T>::padToNonsingular(MOD MatMgr<T> & mgr)
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(m_row_size != m_col_size && m_row_size != 0 && m_col_size != 0,
            ("already nonsin"));
    MatWrap<T> t1(*this, mgr); Matrix<T> & tmp = t1.m();
    bool tran = false;
    if (tmp.m_col_size < tmp.m_row_size) {
        tmp.trans();
        tran = true;
    }
    UINT rows = tmp.m_row_size;
    DUMMYUSE(rows);
    tmp.eche(mgr);
    ASSERTN(rows == tmp.m_row_size, ("rows vector are non independent"));
    for (UINT i = 0; i < tmp.m_row_size; i++) {
        for (UINT j = 0; j < tmp.m_col_size; j++) {
            if (tmp.get(i, j) != 0) {
                //Set followed element to zero.
                //e.g: row is [0,2,4,-1], then set last two be zero [0,2,0,0]
                for (UINT k = j+1; k < tmp.m_col_size; k++) {
                    tmp.set(i, k, 0);
                }
                break;
            }
        }
    }
    MatWrap<T> t2(mgr); Matrix<T> & newrows = t2.m();
    for (UINT j = 0; j < tmp.m_col_size; j++) {
        bool allzero = true;
        for (UINT i = 0; i < tmp.m_row_size; i++) {
            if (tmp.get(i, j) != 0) {
                allzero = false;
                break;
            }
        }
        if (allzero) {
            if (newrows.m_row_size == 0) {
                newrows.growCol(tmp.m_col_size);
            } else {
                newrows.growRow(1);
            }
            newrows.setRow(newrows.m_row_size - 1, T(0));
            newrows.set(newrows.m_row_size - 1, j, 1);
        }
    }
    ASSERTN(newrows.m_row_size == abs(m_row_size - m_col_size),
             ("The partial rows of matrix must be independent."));
    if (tran) {
        newrows.trans();
        growCol(newrows, 0, newrows.m_col_size - 1);
        return;
    }
    growRow(newrows, 0, newrows.m_row_size - 1);
}


template <class T>
void Matrix<T>::dumpb(OUT StrBuf & buf, UINT indent) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    buf.strcat("\n");
    dumpInd(buf, indent);
    #ifdef _DEBUG_
    buf.strcat("MAT id:%u", id());
    #endif
    StrBuf strbuf(32);
    UINT elemprtlen = 7;
    buf.strcat("\n");
    for (UINT i = 0; i < m_row_size; i++) {
        dumpInd(buf, indent);
        for (UINT j = 0; j < m_col_size; j++) {
            dumpT2Buf(get(i, j), buf);
            padTo(buf, elemprtlen);
            //buf.strcat("%s", strbuf.buf);
        }
        buf.strcat("\n");
    }
}


template <class T>
void Matrix<T>::dumpf(FILE * h) const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERTN(h, ("In dumpf(), file handle is nullptr!!!"));
    FileObj fo(h);
    #ifdef _DEBUG_
    fo.prt("\nMAT id:%u\n", id());
    #endif
    StrBuf strbuf(32);
    UINT elemprtlen = 7;
    for (UINT i = 0; i < m_row_size; i++) {
        for (UINT j = 0; j < m_col_size; j++) {
            dumpT2Buf(get(i, j), strbuf);
            padTo(strbuf, elemprtlen);
            fo.prt("%s", strbuf.buf);
        }
        fo.prt("\n");
    }
    fo.prt("\n");
}


template <class T>
void Matrix<T>::padTo(MOD StrBuf & buf, UINT len) const
{
    for (INT i = 0; i < (INT)(len - (UINT)buf.strlen()); i++) {
        buf.strcat(" ");
    }
}


template <class T>
void Matrix<T>::dumpf(CHAR const* name, bool is_del) const
{
    //Default version regards T as 'int'.
    ASSERTN(m_is_init, ("not yet initialize."));
    ASSERT0(name);
    FileObj fo(name, is_del);
    dumpf(fo.getFileHandler());
}


template <class T>
void Matrix<T>::dumps() const
{
    ASSERTN(m_is_init, ("not yet initialize."));
    printf("\n");
    for (UINT i = 0; i < m_row_size; i++) {
        printf("\t");
        for (UINT j = 0; j < m_col_size; j++) {
            CHAR const* blank = "           ";
            T v = get(i, j);
            dumpT2S(v);
            printf("%s", blank);
        }
        printf("\n");
    }
    printf("\n");
}
//END Matrix

//Multiplification: res = a * b
template <class T>
Matrix<T> & Matrix<T>::mul(MOD Matrix<T> & a, MOD Matrix<T> & b,
                           OUT Matrix<T> & res, MOD MatMgr<T> & mgr)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size > 0 && a.m_col_size > 0, ("invalid matrix"));
    ASSERTN(b.m_row_size > 0 && b.m_col_size > 0, ("invalid matrix"));
    ASSERTN(a.m_col_size == b.m_row_size, ("invalid matrix type of mul"));
    Matrix<T> * pres = &res;
    if (pres == &a || pres == &b) {
        pres = mgr.allocMat(a.m_row_size, b.m_col_size);
    } else {
        pres->reinit(a.m_row_size, b.m_col_size);
    }
    mul(a, b, *pres);
    if (&res == &a) { a.copy(*pres); mgr.freeMat(pres); return res; }
    if (&res == &b) { b.copy(*pres); mgr.freeMat(pres); }
    return res;
}


//Multiplification: res = a * b
template <class T>
Matrix<T> & Matrix<T>::mul(Matrix<T> const& a, Matrix<T> const& b,
                           OUT Matrix<T> & res)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size > 0 && a.m_col_size > 0, ("invalid matrix"));
    ASSERTN(b.m_row_size > 0 && b.m_col_size > 0, ("invalid matrix"));
    ASSERTN(a.m_col_size == b.m_row_size, ("invalid matrix type of mul"));
    ASSERTN(&a != &res && &b != &res, ("not support in-place operation"));
    res.reinit(a.m_row_size, b.m_col_size);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < b.m_col_size; j++) {
            T tmp = 0;
            for (UINT k = 0; k < a.m_col_size; k++) {
                tmp = tmp + a.get(i, k) * b.get(k, j);
            }
            res.set(i, j, tmp);
        }
    }
    return res;
}


//Addition: res = a + b
//Note the function supports in-place operation.
template <class T>
Matrix<T> & Matrix<T>::add(Matrix<T> const& a, Matrix<T> const& b,
                           OUT Matrix<T> & res)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size == b.m_row_size && a.m_col_size == b.m_col_size,
            ("invalid matrix type of add"));
    res.reinit(a.m_row_size, a.m_col_size);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < a.m_col_size; j++) {
            res.set(i, j, a.get(i, j) + b.get(i, j));
        }
    }
    return res;
}


//Subtraction: res = a - b
//Note the function supports in-place operation.
template <class T>
Matrix<T> & Matrix<T>::sub(Matrix<T> const& a, Matrix<T> const& b,
                           OUT Matrix<T> & res)
{
    ASSERTN(a.m_is_init && b.m_is_init, ("not yet initialize."));
    ASSERTN(a.m_row_size == b.m_row_size && a.m_col_size == b.m_col_size,
            ("invalid matrix type of sub"));
    res.reinit(a.m_row_size, a.m_col_size);
    for (UINT i = 0; i < a.m_row_size; i++) {
        for (UINT j = 0; j < a.m_col_size; j++) {
            res.set(i, j, a.get(i, j) - b.get(i, j));
        }
    }
    return res;
}


//
//START MatMgr
//
template <class T> class MatMgr {
    COPY_CONSTRUCTOR(MatMgr);
protected:
    #ifdef _DEBUG_
protected:
    UINT m_matcnt; //only used for debug mode.
    UINT m_imatcnt; //only used for debug mode.
protected:
    bool incMatCnt() { m_matcnt++; return true; }
    bool decMatCnt() { m_matcnt--; return true; }
    bool incIMatCnt() { m_imatcnt++; return true; }
    bool decIMatCnt() { m_imatcnt--; return true; }
    #endif
public:
    MatMgr()
    {
       #ifdef _DEBUG_
       m_matcnt = 0;
       m_imatcnt = 0;
       #endif
    }
    virtual ~MatMgr()
    {
        #ifdef _DEBUG_
        ASSERTN(m_matcnt == 0,
                ("there are matrices that still have not been freed"));
        ASSERTN(m_imatcnt == 0,
                ("there are matrices that still have not been freed"));
        #endif
    }

    virtual Matrix<T> * allocMat()
    {
        ASSERT0(incMatCnt());
        Matrix<T> * m = new Matrix<T>();
        ASSERT0(m->setId(m_matcnt));
        return m;
    }
    virtual Matrix<T> * allocMat(UINT row, UINT col)
    {
        ASSERT0(incMatCnt());
        Matrix<T> * m = new Matrix<T>(row, col);
        ASSERT0(m->setId(m_matcnt));
        return m;
    }
    virtual Matrix<INT> * allocIntMat()
    {
        ASSERT0(incIMatCnt());
        Matrix<INT> * m = new Matrix<INT>();
        ASSERT0(m->setId(m_imatcnt));
        return m;
    }
    virtual Matrix<INT> * allocIntMat(UINT row, UINT col)
    {
        ASSERT0(incIMatCnt());
        Matrix<INT> * m = new Matrix<INT>(row, col);
        ASSERT0(m->setId(m_imatcnt));
        return m;
    }

    virtual Matrix<T> * copyMat(Matrix<T> const& src)
    {
        Matrix<T> * newmat = allocMat();
        newmat->copy(src);
        return newmat;
    }

    void freeMat(Matrix<T> & mat) { freeMat(&mat); }
    void freeMat(Matrix<T> * mat) { ASSERT0(decMatCnt()); delete mat; }
    void freeIntMat(Matrix<INT> * mat)
    { ASSERT0(decIMatCnt()); delete mat; }

    //The function will free given list Matrix object.
    //Note the Matrix object should be pointer type.
    void freeMat(UINT num, ...)
    {
        if (num == 0) { return; }
        va_list ptr;
        va_start(ptr, num);
        UINT i = 0;
        while (i < num) {
            Matrix<T> * mat = (Matrix<T>*)va_arg(ptr, Matrix<T>*);
            freeMat(mat);
            i++;
        }
        va_end(ptr);
    }
};
//END MatMgr


//
//START MatWrap
//
template <class T>
MatWrap<T>::MatWrap(MatMgr<T> & mgr) : m_mgr(mgr)
{
    m_mat = mgr.allocMat();
}


template <class T>
MatWrap<T>::MatWrap(Matrix<T> const& src, MatMgr<T> & mgr) : m_mgr(mgr)
{
    m_mat = mgr.copyMat(src);
}


template <class T>
MatWrap<T>::MatWrap(UINT row, UINT col, MatMgr<T> & mgr) : m_mgr(mgr)
{
    m_mat = mgr.allocMat(row, col);
}


template <class T>
MatWrap<T>::~MatWrap()
{
    m_mgr.freeMat(m_mat);
}

//Matrix<T> & m() { return *m_mat; }
//END MatWrap

} //namespace xcom

#endif
