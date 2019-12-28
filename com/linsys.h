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

#ifndef __LIN_SYS_H_
#define __LIN_SYS_H_

namespace xcom {

//
//START ZPoly
//
//e.g: Integer Polyhedron Z=Q¡ÉL
//    domain  Q = {i,j|0<=i<=5, 0<=3j<=20}
//    lattice L = {i,j|2i+1, 3j+5|i,j¡ÊZ}
class ZPoly {
    RMat m_domain;
    INTMat m_lattice; //an affine, invertible matrix.
public:

};
//END ZPoly


//
//START Lineq
//
//System of Linear Inequalities and Equalities.
#define CST_UNK 1 //unknown result of comparation of const term
#define CST_LT 2 //less than
#define CST_GT 3 //great than
#define CST_EQ 4 //equal to

class Lineq {    
    bool m_is_init;

    //Index of right-hand-side, also the column index of constant coefficient
    //vector, start from zero, named by Mathematical Programming System.
    //If 'rhs_idx' does not equal to 'm_col_size - 1', it means,
    //each column from 'rhs_idx + 1' to 'm_col_size -1' represent one
    //constant symbol.
    //e.g: x+y <= 10 + M + N, where M, N represent constant symbols respectively.
    //Default value is -1, indicate last column is constant vector.
    INT m_rhs_idx;

    //Record coeff of inequality: Ax <= b+C(x), where C(x) is function of
    //symbolic constant.
    RMat * m_coeff;
    bool m_is_dump;

    INT compareConstIterm(RMat const& m,
                          UINT rhs_idx,
                          INT idx_of_eqt1,
                          Rational v);
    INT compareConstIterm(RMat const& m,
                          UINT rhs_idx,
                          INT idx_of_eqt1,
                          INT idx_of_eqt2);
    INT selectLeadingColumn(INTMat const& coeff,
                            Vector<bool> const& is_noneg,
                            UINT rhs_part);
    void combine(OUT INTMat & res,
                 INTMat const& coeff,
                 UINT nc,
                 UINT pc,
                 UINT lc,
                 UINT pos);
    void combineRays(OUT INTMat & res,
                     IN OUT INTMat & coeff,
                     UINT r1,
                     UINT r2,
                     UINT lc,
                     UINT pos);
    bool omit(INTMat const& coeff,
              UINT ncv,
              UINT pcv,
              UINT rhs_part,
              Vector<UINT> const& combined,
              Vector<UINT> const& noneg);
    void removeRedRow(IN OUT INTMat & cs,
                      INTMat const& org_cone,
                      UINT rhs_part);
public:
    Lineq(RMat * m, INT rhs_idx = -1);
    ~Lineq();
    void init(RMat * m, INT rhs_idx = -1);
    void destroy();

    void appendEquation(RMat const& eq);

    //Dumps variable, forms as
    //  ak*xk <= const + F(x) + a0x0 + a1x1 + ... + a(k-1)x(k-1) +
    //           a(k+1)x(k+1) + ... + anxn.
    void dumps_var_bound(UINT u);

    //Set index of const column and coeff matrix.
    void setParam(RMat * m, INT rhs_idx = -1);
    void set_dump(bool is_dump) { m_is_dump = is_dump; }
    bool reduce(IN OUT RMat & m, UINT rhs_idx, bool is_intersect);
    void ConvexHullUnionAndIntersect(OUT RMat & res,
                                     IN List<RMat*> & chulls,
                                     UINT rhs_idx,
                                     bool is_intersect);

    //Fourier-Motzkin elimination
    bool fme(UINT const u, OUT RMat & res, bool const darkshadow = false);
    bool is_consistent();
    bool has_solution(RMat const& leq,
                      RMat const& eq,
                      RMat const& vc,
                      UINT rhs_idx,
                      bool is_int_sol,
                      bool is_unique_sol);
    void initVarConstraint(Vector<INT> const& sign,
                           IN OUT RMat & vc,
                           UINT rhs_idx);
    void substituteAndExpand(IN OUT RMat & coeff,
                             UINT rhs_idx,
                             RMat const& p,
                             UINT sub_var);
    //Represent variable, forms as
    //    ak*xk <= const + F(x) + a0x0 + a1x1 + ... + a(k-1)x(k-1) +
    //             a(k+1)x(k+1) + ... + anxn.
    void formatBound(UINT u, OUT RMat & ineqt_of_u);
    bool calcBound(IN OUT List<RMat*> & limits);

    void move2cstsym(IN RMat & ieq,
                     UINT rhs_idx,
                     UINT first_var,
                     UINT last_var,
                     OUT UINT * first_sym_idx,
                     OUT UINT * last_sym_idx);
    void move2var(IN RMat & ieq,
                  UINT rhs_idx,
                  UINT first_sym,
                  UINT last_sym,
                  OUT UINT * first_var_idx,
                  OUT UINT * last_var_idx);
    void removeIdenRow(IN OUT RMat & m);

    //Polyhedra operation
    bool convertConstraint2Ray(OUT INTMat & gmat,
                               INTMat const& cs,
                               UINT rhs_idx,
                               UINT raylimit = 1000);
    bool convertRay2Constraint(INTMat const& gmat,
                               OUT INTMat & cs,
                               UINT cslimit = 100);
    void PolyDiff(OUT RMat & res, IN RMat & a, IN RMat & b, UINT rhs_idx);
    void PolyImage(OUT RMat & res, IN RMat & a, UINT rhs_idx);
    void EhartPoly(OUT RMat & res, IN RMat & a, UINT rhs_idx);

};

} //namespace xcom
#endif
