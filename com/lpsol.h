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
#ifndef __LPSOLF_H_
#define __LPSOLF_H_

namespace xcom {

//The file includes the following main components:
//implementation of the linear programming solver.
//implementation of the exact simplex based
//on rational arithmetic, and the approximate method based
//on double/float arithmetic.
//implementation of the 0-1 integer programming solver.
//implementation of the mixed integer programming solver.

#define IS_INEQ(a, b) ((a) != (b))
#define IS_EQ(a, b) ((a) == (b))
#define IS_GE(a, b) ((a) >= (b))
#define IS_LE(a, b) ((a) <= (b))
#define INVALID_EQNUM -1
#define SIX_DUMP_NAME "dumpmatrix.tmp"

class PivotPair {
public:
    INT flag;
    INT nvidx; //pivoting candidate, nonbasic variable index.
    INT bvidx; //pivoting candidate, basic variable index.
    PivotPair(INT nv, INT bv)
    {
        init(nv, bv);
    }

    void init(INT nv, INT bv)
    {
        ASSERTN(nv >= 0 && bv >= 0, ("illegal index"));
        nvidx = nv;
        bvidx = bv;
        flag = 1;
    }
};


class PivotPairTab {
public:
    BYTE m_is_init:1;
    BMat m_pair; //Pairs of nv,bv.

    PivotPairTab(INT varnum)
    {
        m_is_init = false;
        init(varnum);
    }

    ~PivotPairTab()
    {
        destroy();
    }

    void init(INT varnum)
    {
        if (m_is_init) return;
        m_pair.reinit((UINT)varnum, (UINT)varnum);
        m_is_init = true;
    }

    void destroy()
    {
        if (!m_is_init) return;
        m_pair.destroy();
        m_is_init = false;
    }

    //Generate a pair of non-basic variable and basic variable.
    //Rows indicate NV, cols indicate BV.
    void genPair(INT nvidx, INT bvidx)
    {
        ASSERT0(nvidx != bvidx);
        m_pair.set((UINT)nvidx, (UINT)bvidx, true);
    }

    //Rows indicate NV, cols indicate BV.
    bool is_handle(INT nvidx, INT bvidx) const
    {
        ASSERT0(nvidx != bvidx);
        return m_pair.get((UINT)nvidx, (UINT)bvidx);
    }

    //Rows indicate NV, cols indicate BV.
    void disableNV(UINT idx)
    {
        ASSERT0(idx < m_pair.getRowSize());
        for (UINT j = 0; j < m_pair.getColSize(); j++) {
            if (j == idx) { continue; }
            m_pair.set(idx, j, 1);
        } //end for
    }

    //Rows indicate NV, cols indicate BV.
    bool canBeNVCandidate(UINT idx) const
    {
        ASSERT0(idx < m_pair.getRowSize());
        for (UINT j = 0; j < m_pair.getColSize(); j++) {
            if (j == idx) {
                continue;
            }
            if (!m_pair.get(idx, j)) {
                //At least one chance!
                return true;
            }
        } //end for
        return false;
    }

    //Rows indicate NV, cols indicate BV.
    bool canBeBVCandidate(UINT idx) const
    {
        ASSERT0(idx < m_pair.getColSize());
        for (UINT i = 0; i < m_pair.getRowSize(); i++) {
            if (i == idx) {
                continue;
            }
            if (!m_pair.get(i, idx)) {
                //At least one chance!
                return true;
            }
        } //end for
        return false;
    }
};


template <class T> class Element {
};


template <class Mat> class PVParam {
public:
    Mat * eq;
    Mat * tgtf;
    Vector<bool> * nvset;
    Vector<bool> * bvset;
    Vector<INT> * bv2eqmap;
    Vector<INT> * eq2bvmap;
    PivotPairTab * ppt;
    INT rhs_idx;

    PVParam(Mat * peq,
            Mat * ptgtf,
            Vector<bool> * pnvset,
            Vector<bool> * pbvset,
            Vector<INT> * pbv2eqmap,
            Vector<INT> * peq2bvmap,
            PivotPairTab * pppt,
            INT prhs_idx)
    {
        eq = peq;
        tgtf = ptgtf;
        nvset = pnvset;
        bvset = pbvset;
        bv2eqmap = pbv2eqmap;
        eq2bvmap = peq2bvmap;
        ppt = pppt;
        rhs_idx = prhs_idx;
    }
};



//
//START SIX
//
//Definitive Simplex Method.
#define SIX_SUCC 0
#define SIX_UNBOUND 1
#define SIX_NO_PRI_FEASIBLE_SOL 2
#define SIX_OPTIMAL_IS_INFEASIBLE 3
#define SIX_TIME_OUT 4

inline CHAR const* getStatusName(STATUS st)
{
    switch (st) {
    case SIX_SUCC: return "success";
    case SIX_UNBOUND: return "unbound";
    case SIX_NO_PRI_FEASIBLE_SOL: return "no_prime_feasible_solution";
    case SIX_OPTIMAL_IS_INFEASIBLE: return "optimal_is_infeasible";
    case SIX_TIME_OUT: return "time_out";
    default: UNREACHABLE();
    }
    return nullptr;
}


template <class Mat, class T> class SIX : public Element<T> {
    BYTE m_is_dump:1;
    BYTE m_is_init:1; //To make sure functions are idempotent.
    INT m_rhs_idx;
    UINT m_indent;
    UINT m_max_iter;
    PivotPairTab * m_ppt;

    INT findPivotBV(UINT pivot_nv, MOD PVParam<Mat> & pp);
    INT findPivotNVandBVPair(OUT INT & nvidx,
                             OUT INT & bvidx,
                             MOD PVParam<Mat> & pp);

    bool isConstItermFeasible(Mat & newleq, INT rhs_idx) const;

    void newPPT(INT rhs_idx);
    INT normalize(OUT Mat & newleq,
                  OUT Mat & newvc,
                  OUT INTMat & vcmap,
                  OUT Mat & newtgtf,
                  Mat const& vc,
                  Mat const& eq,
                  Mat const& leq,
                  Mat const& tgtf);

    void pivot(UINT nv, UINT bv, MOD PVParam<Mat> & pp);

    INT stage1(OUT Mat & newleq,
               OUT Mat & newvc,
               OUT Mat & newtgtf,
               OUT Vector<bool> & nvset,
               OUT Vector<bool> & bvset,
               Vector<INT> & bv2eqmap,
               Vector<INT> & eq2bvmap,
               MOD INT & new_rhs_idx);
    UINT solveSlackForm(MOD Mat & tgtf,
                        MOD Mat & eqc,
                        MOD Mat & vc,
                        OUT T & maxv,
                        OUT Mat & sol,
                        MOD Vector<bool> & nvset,
                        MOD Vector<bool> & bvset,
                        MOD Vector<INT> & bv2eqmap,
                        MOD Vector<INT> & eq2bvmap,
                        INT rhs_idx);
    void slack(MOD Mat & tgtf,
               MOD Mat & leq,
               MOD Mat & vc,
               MOD INT & rhs_idx);

    void checkAndInitConst(Mat const& leq,
                           Mat const& eq,
                           Mat const& tgtf,
                           Mat const& vc,
                           INT rhs_idx);
public:
    SIX(UINT indent = 0, UINT max_iter = 0xFFFFffff, bool is_dump = false)
    {
        m_is_init = false;
        m_rhs_idx = -1;
        m_ppt = nullptr;
        init();
        m_is_dump = is_dump;
        setParam(indent, max_iter);
    }
    ~SIX() { destroy(); }
    void init()
    {
        if (m_is_init) { return; }
        m_indent = 0;
        m_max_iter = 0xFFFFffff;
        m_is_init = true;
    }
    void destroy()
    {
        if (!m_is_init) { return; }
        m_rhs_idx = -1;
        m_is_init = false;
        if (m_ppt != nullptr) {
            delete m_ppt;
        }
    }

    void setParam(UINT indent, UINT max_iter)
    {
        m_indent = indent;
        m_max_iter = max_iter;
    }

    bool verifyEmptyVariableConstrain(Mat const& tgtf,
                                      Mat const& vc,
                                      Mat const& eq,
                                      Mat const& leq,
                                      INT rhs_idx);
    UINT minm(OUT T & minv,
              OUT Mat & res,
              Mat const& tgtf,
              Mat const& vc,
              Mat const& eq,
              Mat const& leq,
              INT rhs_idx = -1); //Linear minmum solution
    UINT maxm(OUT T & maxv,
              OUT Mat & res,
              Mat const& tgtf,
              Mat const& vc,
              Mat const& eq,
              Mat const& leq,
              INT rhs_idx = -1); //Linear maximum solution
    UINT calcDualMaxm(OUT T & dual_maxv,
                      OUT Mat & dual_slack_sol,
                      OUT Mat & dual_tgtf,
                      OUT INT & dual_rhs_idx,
                      OUT INT & dual_num_nv,
                      Mat const& tgtf,
                      IN Mat & vc,
                      Mat const& leq,
                      INT rhs_idx);
    void calcFinalSolution(OUT Mat & sol,
                           OUT T & v,
                           MOD Mat & slack_sol,
                           IN INTMat & vcmap,
                           Mat const& orignal_tgtf,
                           INT rhs_idx);
    void convertEq2Ineq(OUT Mat & leq, Mat const& eq);
    bool calcSolution(MOD Mat & sol,
                      Vector<bool> const& has_val,
                      Mat const& eqc, INT rhs_idx);
    bool constructBasicFeasibleSolution(MOD Mat & leq,
                                        MOD Mat & tgtf,
                                        MOD Mat & vc,
                                        MOD Vector<bool> & nvset,
                                        MOD Vector<bool> & bvset,
                                        MOD Vector<INT> & bv2eqmap,
                                        MOD Vector<INT> & eq2bvmap,
                                        MOD INT & rhs_idx);

    FILE * dump_open_file() const;
    bool dump_prt_indent(FILE * h) const;
    bool dump_str(CHAR const* format, ...) const;

    bool is_feasible(Mat const& sol,
                     IN Mat & lc,
                     bool is_eqc,
                     Mat const& vc,
                     INT rhs_idx) const;
    void reviseTargetFunc(MOD Mat & tgtf,
                          Mat const& eq,
                          Mat const& leq,
                          INT rhs_idx);

    UINT TwoStageMethod(MOD Mat & newleq,
                        MOD Mat & newvc,
                        MOD Mat & newtgtf,
                        MOD Mat & slack_sol,
                        MOD T & maxv,
                        MOD Vector<bool> & nvset,
                        MOD Vector<bool> & bvset,
                        MOD Vector<INT> & bv2eqmap,
                        MOD Vector<INT> & eq2bvmap,
                        MOD INT & new_rhs_idx);
};


//Generate new pivot-pair-tab.
template <class Mat, class T>
void SIX<Mat, T>::newPPT(INT var_num)
{
    ASSERT0(var_num > 0);
    if (m_ppt != nullptr) {
        m_ppt->destroy();
        m_ppt->init(var_num);
        return;
    }
    m_ppt = new PivotPairTab(var_num);
}


template <class Mat, class T>
FILE * SIX<Mat, T>::dump_open_file() const
{
    if (!m_is_dump) { return nullptr; }
    FILE * h = fopen(SIX_DUMP_NAME, "a+");
    ASSERTN(h, ("%s create failed!!!", SIX_DUMP_NAME));
    return h;
}


template <class Mat, class T>
bool SIX<Mat, T>::dump_prt_indent(FILE * h) const
{
    ASSERT0(h);
    for (UINT i = 0; i < m_indent; i++) {
        fprintf(h, "  ");
    }
    return true;
}


//Print string with indent chars.
template <class Mat, class T>
bool SIX<Mat, T>::dump_str(CHAR const* format, ...) const
{
    if (format == nullptr) { return true; }
    FILE * h = dump_open_file();
    if (h == nullptr) { return true; }

    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vstrcat(format, arg);

    //Print leading \n.
    size_t i = 0;
    while (i < buf.strlen()) {
        if (buf.buf[i] == '\n') {
            fprintf(h, "\n");
        } else {
            break;
        }
        i++;
    }
    dump_prt_indent(h);
    if (i == buf.strlen()) {
        fclose(h);
        va_end(arg);
        return true;
    }
    fprintf(h, "%s", buf.buf + i);
    fclose(h);
    va_end(arg);
    return true;
}


//Calculate solution by given variable's value.
//Return true if solution is unique.
//sol: record the solution computed.
//has_val: represents which variable was depicted in 'sol'.
//eqc: equality constraints
//vc: variable constraints
//rhs_idx: column index indicates the constant-column.
template <class Mat, class T>
bool SIX<Mat, T>::calcSolution(MOD Mat & sol,
                               Vector<bool> const& has_val,
                               Mat const& eqc,
                               INT rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    ASSERTN(((INT)has_val.get_elem_count()) == rhs_idx &&
            sol.getColSize() == eqc.getColSize(), ("illegal info"));
    Mat tmpeqc = eqc;
    //Calc the number of variables which has value.
    UINT hasvalnum = 0;
    for (UINT i = 0; i < (UINT)rhs_idx; i++) {
        if (has_val.get(i)) {
            hasvalnum++;
        }
    }

    if (hasvalnum == (UINT)rhs_idx) {
        //'sol' already be solution
        return true;
    }

    //Compute the number of the variables which consist of each equations.
    //If there are more than one variable in the equation, the solution
    //of the group equations will NOT be unique.
    Vector<UINT> eq2var; //When equation was processed, record the
                         //variable index that will be solved.
    for (UINT i = 0; i < tmpeqc.getRowSize(); i++) {
        UINT nonzerocoeff = 0;
        for (UINT j = 0; j < (UINT)rhs_idx; j++) {
            if (has_val.get(j)) {
                continue;
            }
            if (IS_INEQ(tmpeqc.reduce(i, j), 0)) {
                nonzerocoeff++;

                //Suppose each equality only have one variable require
                //to solve. And the first that is.
                eq2var.set(i, j);
            }
        }
        if (nonzerocoeff > 1) { //magitude may be zero
            return false;
        }
    }

    //Exam OK! Compute the solution.
    for (UINT i = 0; i < (UINT)rhs_idx; i++) {
        if (!has_val.get(i)) {
            sol.set(0, i, 0); //do some cleaning
            continue;
        }
        T val = sol.get(0, i);
        tmpeqc.mulOfColumn(i, val);
    }

    //If variable has been computed, set relevant vector position to be true.
    Vector<bool> comped;
    for (UINT i = 0; i < tmpeqc.getRowSize(); i++) {
        T temval = 0, varcoeff = 0;
        INT varidx = eq2var.get(i);
        ASSERTN(!comped.get(varidx), ("already has computed"));
        comped.set(varidx, true);
        for (UINT j = 0; j < (UINT)rhs_idx; j++) {
            if ((UINT)varidx == j) {
                varcoeff = tmpeqc.get(i, j);
                continue;
            }
            temval = temval + tmpeqc.get(i, j);
        }
        temval = tmpeqc.get(i, rhs_idx) - temval; //ax1 = C - bx2 - cx3
        varcoeff.reduce();
        if (IS_INEQ(varcoeff, 1)) {
            temval = temval/varcoeff; //x1 = (C - bx2 - cx3) / a
        }
        sol.set(0, varidx, temval);
    }
    return true;
}


//Calculate the value for each basic variable and find the
//minmum one to be the candidate that will be nonbasic variable.
//
//NOTICE:
//    When we are going to find the tightest constraint for
//    variable 'pivot_nv', assumed the other nonbasic variables
//    to be zero, in other word that the tightest value of
//    'pivot_nv' equals:
//        const-term / pivot_nv's coeff.
//
//    In actually, since each equality corresponding to one
//    specifical basic variable, the value of basic variable is
//    also the slack range of 'pivot_nv'.
template <class Mat, class T>
INT SIX<Mat, T>::findPivotBV(UINT pivot_nv, MOD PVParam<Mat> & pp)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    Mat const& eqc = *pp.eq;
    Vector<INT> const& eq2bvmap = *pp.eq2bvmap;
    Vector<bool> const& nvset = *pp.nvset;

    DUMMYUSE(nvset);

    PivotPairTab & ppt = *pp.ppt;
    INT rhs_idx = pp.rhs_idx;

    ASSERT0(nvset.get(pivot_nv));
    T minbval;
    INT eqidx = -1;
    UINT i;
    bool all_unbound = true;
    bool first = true;
    for (i = 0; i < eqc.getRowSize(); i++) {
        T pivot_nv_val = eqc.get(i, pivot_nv);

        //Positive coeff is the one we are looking for.
        if (IS_LE(pivot_nv_val, 0)) {
            //xi is unbound!
            //e.g:Given x1 is basic variable, x2 is non-basic variable, and
            //  the equality as:
            //    #i: x1 = 28 + x2 + 0 + 0 (our form is: x1 - x2 - 0 - 0 = 28)
            //  No matter how we increase the value of x2,
            //  x1 is always positive, and also satified x1's
            //  limits( x1 >= 0 ) at the same time.
            //  We said x1 is unconstrained to x2, x1 is unbound.
            //  So this equality(constraint) '#i' could not limit x2.
            //  Similarly for situation which x2 is zero.
            continue;
        }

        if (ppt.is_handle(pivot_nv, eq2bvmap.get(i))) {
            //Pivoting pair had already examed.
            //Try next to avoid cycle.
            continue;
        }

        //Hint:nvnum + bvnum == rhs_idx
        if (!ppt.canBeBVCandidate(eq2bvmap.get(i))) {
            continue;
        }

        //Find candidate!
        all_unbound = false;

        T v = eqc.get(i, rhs_idx) / pivot_nv_val;
        if (first) {
            minbval = v;
            eqidx = i;
            first = false;
        } else if (minbval > v) {
            minbval = v;
            eqidx = i;
        }
    }

    //None of basic variable that could form a bound for 'pivot_nv'.
    //Whereas contrasting the classical algorithm, we are going to relax
    //the choice condition, that is finding a basic variable as the
    //tightest bound of 'pivot_nv', even if coefficient of 'pivot_nv' is not
    //positive(in our presentation).
    //
    //Well, the new policy we applied is looking for the equality with the
    //minimal value of 'constant_coeff/pivot_nv's_coeff'.
    //And relative basic variable of that equality could be what we found.
    if (all_unbound) {
        eqidx = -1; //Find again! good luck!
        bool first2 = true;
        for (i = 0; i < eqc.getRowSize(); i++) {
            if (ppt.is_handle(pivot_nv, eq2bvmap.get(i))) {
                //Pivoting pair had already examed.
                //Try next to avoid cycle.
                continue;
            }

            //Hint:nvnum + bvnum == rhs_idx
            if (!ppt.canBeBVCandidate(eq2bvmap.get(i))) {
                continue;
            }

            T den = eqc.get(i, pivot_nv);
            if (den == 0) {
                continue;
            }

            T v = eqc.get(i, rhs_idx) / den;
            if (first2) {
                minbval = v;
                eqidx = i;
                first2 = false;
            } else if (minbval > v) {
                minbval = v;
                eqidx = i;
            }
        }
        if (eqidx == -1) {
            //All pair of (pivot_nv_idx, bv_idx) had processed.
            //Retry another nv to aviod cycle.
            return -1;
        }
    }

    //We found a basic varible in condition!
    //And it will be the tightest bound of 'pivot_nv'.
    return eq2bvmap.get(eqidx);
}


//Finding the feasible pivoting pair of (nv, bv).
//NOTICE:
//  In order to avoid the degeneracy case, record the pivoting
//  pair at a time.
template <class Mat, class T>
INT SIX<Mat, T>::findPivotNVandBVPair(OUT INT & nvidx,
                                      OUT INT & bvidx,
                                      MOD PVParam<Mat> & pp)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    Mat const& tgtf = *pp.tgtf;
    MOD Vector<bool> & nvset = *pp.nvset;
    MOD Vector<bool> & bvset = *pp.bvset;
    DUMMYUSE(nvset);
    DUMMYUSE(bvset);
    PivotPairTab & ppt = *pp.ppt;
    INT rhs_idx = pp.rhs_idx;

    //If all variables with positive coefficient are examed,
    //try negative ones.
    bool find = false;

    //In the case of all the coeff of 'tgtf' is nonpositive, the NV with
    //negtive coeff might be recommended.
    bool try_neg_coeff_var = false;
    bool try_zero_coeff_var = false;
AGAIN:
    for (UINT i = 0; i < (UINT)rhs_idx; i++) {
        T coeff = tgtf.get(0, i);
        if (bvset.get(i)) {
            ASSERTN(coeff == 0, ("the coeff of 'bv' must be zero."));
            continue;
        }

        ASSERT0(nvset.get(i));

        //Selecting the nv in term of 'pair table'.
        //Here n+m == rhs_idx
        if (!ppt.canBeNVCandidate(i)) {
            //Given a number of n of nonbasic variables and a
            //number of m of basic variables, there will be at
            //most (n+m-1) times to try for each nonbasic variables.
            continue;
        }

        if (coeff > 0) {
            if (try_neg_coeff_var) {
                continue;
            }

            //Calculate the value for each basic value and find the
            //minmum one as pivoting basic candidate.
            bvidx = findPivotBV(i, pp);
            if (bvidx == -1) {
                //Can not find any basic variable to swap out.
                //Try another non-basic    variable.
                //start_idx_of_v = pivot_nv_idx + 1;
                continue;
            }
            nvidx = i;
            goto FIN;
        } else if (coeff == 0) {
            if (!try_zero_coeff_var) {
                continue;
            }
            bvidx = findPivotBV(i, pp);
            if (bvidx == -1) {
                continue;
            }
            nvidx = i;
            goto FIN;
        } else { //Tring variable with negative coeff.
            if (!try_neg_coeff_var) {
                continue;
            }
            bvidx = findPivotBV(i, pp);
            if (bvidx == -1) {
                continue;
            }
            nvidx = i;
            goto FIN;
        }
    } //end for

    if (!find) {
        if (!try_zero_coeff_var) {
            try_zero_coeff_var = true;
            goto AGAIN;
#ifdef ALLOW_RELAX_CURRENT_SOL
        } else if (!try_neg_coeff_var) {
            //All of the variables with positive coefficient were tried.
            //Attempt the negative one to next. But this will cause the
            //relaxation the optimal solution!
            //Or the scenario should be solved during initialize-simplex.
            //I wonder if it is a good idea.
            try_neg_coeff_var = true;
            goto AGAIN;
#endif
        } else {
            return false;
        }
    }
    ASSERTN(0, ("arrive in here?"));
FIN:
    return true;
}


//Return true if 'sol' is feasible, otherwise return false.
//
//'sol': solution to exam.
//'lc': linear constraint.
//'is_eqc': True indicate 'lc' is equality constraint,
//    and False indicate lc is inequality constraint.
//'vc': variable constraint
template <class Mat, class T>
bool SIX<Mat, T>::is_feasible(Mat const& sol,
                              IN Mat & lc,
                              bool is_eqc,
                              Mat const& vc,
                              INT rhs_idx) const
{
    ASSERTN(m_is_init, ("not yet initialize"));
    ASSERTN(sol.getColSize() == lc.getColSize() &&
            lc.getColSize() == vc.getColSize(),
            ("illegal info"));
    //Check variable constraint
    for (UINT i = 0; i < (UINT)rhs_idx; i++) {
        if (vc.get(i, i) * sol.get(0, i) > vc.get(i, rhs_idx)) {
            return false;
        }
    }

    //Check equality constraint
    for (UINT i = 0; i < lc.getRowSize(); i++) {
        T sum = T(0);
        for (UINT j = 0; j < (UINT)rhs_idx; j++) {
            sum = sum + lc.get(i, j) * sol.get(0, j);
        }
        if (is_eqc) {
            sum.reduce();
            if (IS_INEQ(sum, lc.reduce(i, rhs_idx))) {
                return false;
            }
        } else {
            if (sum > lc.get(i, rhs_idx)) {
                return false;
            }
        }
    }
    return true;
}


//Construct a basic feasible solution.
//
//Return true if we have found the slack form of a feasible solution,
//otherwise there is no feasible solution of original problem.
//
//NOTICE:
//    Sometimes we could not find out the basic feasible solution.
//    Therefore we attempt to reconstruct original problem L into a
//    auxiliary one L'. The solution of L' is equivalent to L.
//    In this funtion, we introduced a assistant variable x0, that
//    constraint is x0≡0, and the target function is L'={x0|max(-x0)}.
//    And if L' has a maximum feasible solution: max(-x0)=0, x0=0,
//    the slack form exists a feasible solution and L also has a solution.
template <class Mat, class T>
bool SIX<Mat, T>::constructBasicFeasibleSolution(MOD Mat & leq,
                                                 MOD Mat & tgtf,
                                                 MOD Mat & vc,
                                                 MOD Vector<bool> & nvset,
                                                 MOD Vector<bool> & bvset,
                                                 MOD Vector<INT> & bv2eqmap,
                                                 MOD Vector<INT> & eq2bvmap,
                                                 MOD INT & rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    ASSERTN(rhs_idx > 0, ("illegal info"));

    //Construct auxiliary linear programming L' by introducing one
    //nonnegative variable 'xa', and the target function was transformed as:
    //    max(x) = ax1 + bx2 + ... + zxn + (-xa), where xa >= 0,
    //    x1,x2,...,xn,xa >= 0
    Mat origtgtf = tgtf;
    INT orig_rhs_idx = rhs_idx;

    //Append columns for auxiliary variable 'xa'
    leq.insertColumnBefore(rhs_idx);
    leq.setCol(rhs_idx, -1);
    tgtf.insertColumnBefore(rhs_idx);
    tgtf.zero();
    tgtf.setCol(rhs_idx, -1);
    vc.insertColumnBefore(rhs_idx);
    vc.growRow(1);
    vc.set(rhs_idx, rhs_idx, -1);
    vc.set(rhs_idx, rhs_idx + 1, 0);

    UINT xa = rhs_idx; //variable index of 'xa'
    rhs_idx++;
    UINT bv_start_idx = rhs_idx;

    //Slack transformation of L'.
    slack(tgtf, leq, vc, rhs_idx);

    //Initializing basic-variable set and nonbasic-variable set.
    //Establishing the mapping between variables and relevant equalities.
    for (UINT i = 0; i < bv_start_idx; i++) {
        bv2eqmap.set(i, INVALID_EQNUM);
        nvset.set(i, true); //Initializing nonbasic variable set.
    }
    UINT j = 0; //Index that of mapping of bv and eq starting at 0.
    for (UINT i = bv_start_idx; i < (UINT)rhs_idx; i++, j++) {
        bvset.set(i, true);
        nvset.set(i, false);
        eq2bvmap.set(j, i);
        bv2eqmap.set(i, j);
    }

    //Find the minimum constant term, and select the basic-variable
    //to be the swap-out one.
    UINT pivot_bv_idx = 0;
    T minb;
    for (UINT i = 0; i < leq.getRowSize(); i++) {
        if (i == 0) {
            minb = leq.get(i, rhs_idx);
            pivot_bv_idx = eq2bvmap.get(i);
        } else if (minb > leq.get(i, rhs_idx)) {
            minb = leq.get(i, rhs_idx);
            pivot_bv_idx = eq2bvmap.get(i);
        }
    }

    PVParam<Mat> pp(&leq, &tgtf, &nvset, &bvset,
                    &bv2eqmap, &eq2bvmap, nullptr, rhs_idx);
    pivot(xa, pivot_bv_idx, pp);

    Mat sol;
    T maxv;
    if (SIX_SUCC != solveSlackForm(tgtf, leq, vc, maxv, sol,
                                   nvset, bvset, bv2eqmap, eq2bvmap, rhs_idx)) {
        return false;
    }

    //Maximum solution of L' must be zero, or else L has no basic
    //feasible solution.
    maxv.reduce();
    if (IS_INEQ(maxv, 0)) {
        return false;
    }

    if (bvset.get(xa)) {
        //CASE: 'xa' might be basic variable!
        //In the sake of that each BV is relevant to an unique equation,
        //perform pivoting to pivot xa to a nv.
        VecIdx cand_nv;
        INT eqnum = bv2eqmap.get(xa);
        ASSERTN(eqnum >= 0, ("index must nonnegative"));
        for (cand_nv = 0; cand_nv <= nvset.get_last_idx(); cand_nv++) {
            if (nvset.get(cand_nv) &&
                IS_INEQ(leq.reduce(eqnum, cand_nv), 0)) {
                break;
            }
        }
        ASSERTN(cand_nv <= nvset.get_last_idx(),
                ("No candidate nv can be used in pivoting"));
        pivot(cand_nv, xa, pp);
        ASSERT0(!bvset.get(xa));
    }

    //Updating original target function by substituting xa with 0.
    UINT inccols = tgtf.getColSize() - origtgtf.getColSize();
    tgtf = origtgtf;
    tgtf.insertColumnsBefore(orig_rhs_idx, inccols);
    for (UINT i = 0; i < (UINT)rhs_idx; i++) {
        if (IS_INEQ(tgtf.reduce(0, i), 0) && bvset.get(i)) {
            Mat exp;
            leq.innerRow(exp, bv2eqmap.get(i), bv2eqmap.get(i));
            tgtf.substit(exp, i, false, rhs_idx);
        }
    }

    //Remove xa relevant information.
    tgtf.deleteCol(xa);
    leq.deleteCol(xa);
    vc.deleteCol(xa);
    vc.deleteRow(xa);

    //Update the bvset, nvset, and correspondence between bv and eqnum.
    j = 0;
    Vector<bool> tnvset, tbvset; //local use.
    Vector<INT> tbv2eqmap;
    for (VecIdx i = 0; i <= nvset.get_last_idx(); i++) {
        if (i != (VecIdx)xa) {
            tnvset.set(j, nvset.get(i));
            tbvset.set(j, bvset.get(i));
            tbv2eqmap.set(j, bv2eqmap.get(i));
            j++;
        }
    }
    nvset.copy(tnvset);
    bvset.copy(tbvset);
    bv2eqmap.copy(tbv2eqmap);

    //Update the correspondence between eqnum and bv.
    for (VecIdx i = 0; i <= eq2bvmap.get_last_idx(); i++) {
        UINT bv = eq2bvmap.get(i);
        if (bv > xa) {
            eq2bvmap.set(i, bv - 1);
        }
    }

    //Update column of constant term.
    rhs_idx--;
    return true;
}


//Solve slack-form that given by 'eqc'.
//Return the maximal value and solution, otherwise return unbound.
//tgtf: target function
//eqc: slack form
//vc: variable constraint
//maxv: maximum value
//sol: result solution
//NOTICE:
//    The slack form can be illustrated as:
//        10x + 11y + 60z ↙ min
//        -0.1x + (-1.2y) + (-3.3z) + s1 = -50
//        -73x  + (-96y)  + (-253z) + s2 = -3200
//        -9.6x + (-7y)   + (-19z)  + s3 = -1000
//    and variable constraint:
//        x,y,z,s1,s2,s3 >= 0
template <class Mat, class T>
UINT SIX<Mat, T>::solveSlackForm(MOD Mat & tgtf,
                                 MOD Mat & eqc,
                                 MOD Mat & vc,
                                 OUT T & maxv,
                                 OUT Mat & sol,
                                 MOD Vector<bool> & nvset,
                                 MOD Vector<bool> & bvset,
                                 MOD Vector<INT> & bv2eqmap,
                                 MOD Vector<INT> & eq2bvmap,
                                 INT rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    newPPT(rhs_idx);

    //The initial-solution of target function is always be zero,
    //we get the solution by setting each of non-basic variables to be zero.
    //e.g:max(x) = a*x1 + b*x2,
    //    Set x1, x2 to be zero, then the max(x) is a*0 + b*0 = 0.
    maxv = T(0);
    sol.reinit(1, tgtf.getColSize());

    UINT cnt = 0; //The number of iterations
    PVParam<Mat> pp(&eqc, &tgtf, &nvset, &bvset, &bv2eqmap, &eq2bvmap,
                    m_ppt, rhs_idx);
    while (cnt < m_max_iter) {
        cnt++;

        //Choose the nonbasic variable that coefficient is
        //positive to be the pivoting candidate.
        INT pivot_nv_idx = -1;
        bool all_coeffs_nonpositive = true;
        for (UINT i = 0; i < (UINT)rhs_idx; i++) {
            if (!nvset.get(i)) {
                //May be the verification is unnecessary.
                ASSERTN(IS_EQ(tgtf.reduce(0, i), 0),
                        ("bv must be zero in target function"));
                tgtf.set(0, i, 0);
                continue;
            }
            if (tgtf.get(0, i) > 0) {
                all_coeffs_nonpositive = false;
                if (m_ppt->canBeNVCandidate(i)) {
                    pivot_nv_idx = i;
                    break;
                }
            }
        }

        INT pivot_bv_idx = -1;
        if (pivot_nv_idx == -1) {
            //Each coeffs of target function variables either be zero or
            //negaitive! It looks like that we could not get more larger
            //value of target function by increase value of one
            //of its variables.
            //So it has to perform feasibility verification.
            //Then, we attempt to calculate the optimial solution,
            //whereas target function represented by Non-Basic Variable all
            //through.
            //So one simply method to get value of basic variable is
            //to set non-basic variable all be zero. Then the basic variables
            //could consequently be worked out.
            //The following code would do that operation accroding
            //to information in 'bvset'.
            //There are two possibilities, the first is that the optimal
            //solution is avaiable, the else indicates the solution is
            //infeasible.
            if (all_coeffs_nonpositive) {
                sol.zero(); //Set all non-basic variables to zero.
                //Each basic variables BVi is equal to corresponding Ci.
                //e.g: Given slack form is:
                //    BVi = Ci + f1(NVi)
                //    BVj = Cj + f2(NVi)
                //then BVi, BVj are equal to Ci, Cj respectively.
                #ifdef _DEBUG_
                //Also do some verification which is slow
                //and need more calculation.
                ASSERTN(calcSolution(sol, nvset, eqc, rhs_idx),
                        ("unexpected result"));
                #else
                for (VecIdx i = 0; i <= bvset.get_last_idx(); i++) {
                    if (bvset.get(i)) {
                        INT eqnum = bv2eqmap.get(i);
                        ASSERT0(eqnum >= 0);
                        sol.set(0, i, eqc.get(eqnum, rhs_idx));
                    }
                }
                #endif
                if (is_feasible(sol, eqc, true, vc, rhs_idx)) {
                    //Calculating maximum value.
                    //TODO: for now, SIX only support linear
                    //expression with single column constant term so far.
                    //e.g: maxv = ax1 + bx2 + Const,
                    //The followed form should be supported,
                    //  maxv = ax1 + bx2 + Const + ConstSym(x).
                    maxv = tgtf.get(0, rhs_idx);
                    ASSERT0(dump_str("\nPIVOTING status=%s,number_of_iter=%u",
                                     getStatusName(SIX_SUCC), cnt));
                    return SIX_SUCC;
                } else {
                    //ASSERTN(0,
                    //    ("solution is either feasibly optimal or unbound!"));
                    ASSERT0(dump_str("\nPIVOTING status=%s,number_of_iter=%u",
                                     getStatusName(SIX_OPTIMAL_IS_INFEASIBLE),
                                     cnt));
                    return SIX_OPTIMAL_IS_INFEASIBLE;
                }
            }

            //Select a bv in terms of the giving nv.
            //Note that the candidate 'pivot_bv_idx' is
            //relevant with the tightest constraint of
            //'pivot_nv_idx', and intrducing the tightest
            //constraint into target function to get arrive
            //to the extremal point(max|min).
            //If we could not find a constraint so as to
            //reduce the limitation of nv, the slack form is unbound.
            if (!findPivotNVandBVPair(pivot_nv_idx, pivot_bv_idx, pp)) {
                ASSERT0(dump_str("\nPIVOTING status=%s,number_of_iter=%u",
                                 getStatusName(SIX_UNBOUND), cnt));
                //Note if SIX solver return SIX_UNBOUND, there are two
                //means, one is the solution is not unique, the
                //other is there is no solution.
                //We can not differentiate these two situations.
                //Return false for conservative purpose.
                //e.g: for {i > 0}, there is a unbound result, and infinit
                //solution could satified the system.
                //For {i=0,j=0,i+1<=j}, there is also a unbound result,
                //but there is no solution for <i,j> to satisfied the system.
                return SIX_UNBOUND;
            }
        } else {
            //Calculate the value for each basic value and find the
            //minmum one as pivoting basic candidate.
            pivot_bv_idx = findPivotBV(pivot_nv_idx, pp);
            if (pivot_bv_idx == -1) {
                //Can not find any basic variable to swap out.
                //Try another non-basic variable.
                m_ppt->disableNV(pivot_nv_idx);
                ASSERT0(dump_str("\nNOT FIND BV bv=%d,nv=%d,number_of_iter=%u\n",
                                 pivot_bv_idx, pivot_nv_idx, cnt));
                continue;
            }
        }

        ASSERT0(pivot_nv_idx != -1 && pivot_bv_idx != -1 &&
                !m_ppt->is_handle(pivot_nv_idx, pivot_bv_idx));
        m_ppt->genPair(pivot_nv_idx, pivot_bv_idx);

        //'pivot_nv_idx' refers to 'swap-in' variable, as well as the variables
        //inferred by column index.
        //'pivot_bv_idx' refers to 'swap-out' variable, as well as the variables
        //inferred by row index.
        //Since our algorithme is definitely, there may be exist a cyclic when
        //pivoting a pair of 'swap-in', 'swap-out' variables.
        //    e.g: Assuming that at time t1, the swap-in is xi, swap-out is xj,
        //    and at time t2, the swap-in may become xj, swap-out  xi.
        //        This is one cycle!
        //So the important work is to find mutually exclusive pivoting variables
        //pairs.
        ASSERT0(dump_str("\nPIVOTING bv=%d,nv=%d,number_of_iter=%u\n",
                         pivot_bv_idx, pivot_nv_idx, cnt));
        pivot(pivot_nv_idx, pivot_bv_idx, pp);

        #ifdef _DEBUG_
        UINT count = 0;
        for (VecIdx i = 0; i <= bvset.get_last_idx();i++) {
            ASSERTN(bvset.get(i) != nvset.get(i), ("illegal pivoting"));
            if (bv2eqmap.get(i) != INVALID_EQNUM) {
                count++;
                ASSERTN((VecIdx)eq2bvmap.get(bv2eqmap.get(i)) == i,
                        ("unmatch!"));
            }
        }
        ASSERTN((UINT)eq2bvmap.get_elem_count() ==
                eqc.getRowSize(), ("illegal pivot"));
        ASSERTN(count == eqc.getRowSize(), ("illegal pivot"));
        #endif
    }
    ASSERT0(dump_str("\nPIVOTING status=%s,number_of_iter=%u",
                     getStatusName(SIX_TIME_OUT), cnt));

    return SIX_TIME_OUT;
}


//Convert equations to equivalent inequalities, and remove
//equations by algebraic substitution.
template <class Mat, class T>
void SIX<Mat, T>::convertEq2Ineq(OUT Mat & leq, Mat const& eq)
{
    //Substitute equalitions into inequality system.
    if (eq.size() == 0) { return; }
    Vector<bool> eq_removed;
    UINT eq_count = eq.getRowSize();
    if (leq.size() > 0) {
        for (INT j = 0; j < m_rhs_idx; j++) {
            UINT num_nonzero = 0;
            UINT pos_nonzero = 0;
            for (UINT i = 0; i < eq.getRowSize(); i++) {
                if (eq_removed.get(i)) {
                    continue;
                }
                if (eq.get(i, j) != 0) {
                    num_nonzero++;
                    pos_nonzero = i;
                }
            }
            if (num_nonzero != 1) { continue; }
            eq_removed.set(pos_nonzero, true);
            eq_count--;

            //Substitute variable with linear polynomials.
            //e.g: Given -2i+N<=0, -i+4j-3N=1
            //substitute i with 4j-3N-1, will get:-2(4j-3N-1)+N<=0,
            //then simplied to -8j+7N+2<=0, i reduced.
            Mat tp;
            for (UINT m = 0; m < leq.getRowSize(); m++) {
                T v = leq.get(m, j);
                if (v == T(0)) {
                    continue;
                }
                eq.innerRow(tp, pos_nonzero, pos_nonzero);
                T tpv = tp.get(0, j);
                if (tpv != T(1) && tpv != T(0)) {
                    tp.mulOfRow(0, T(1)/tpv);
                }
                tp.mulOfRow(0, v);
                leq.set(m, j, T(0));
                //Convert the sign of element by taking negative
                //from column 'rhs_idx' to last column of 'eq',
                //since these elements will be added to RHS of
                //inequalities 'leq'.
                //e.g: Given i=2j-1+N, convert -1+N to 1-N.
                for (UINT k = m_rhs_idx; k < tp.getColSize(); k++) {
                    tp.set(0, k, -tp.get(0, k));
                }
                leq.addRowToRow(tp, 0, m);
            }
        } //end for
    } //end if

    if (eq_count > 0) {
        UINT c = leq.getRowSize();
        if (leq.size() == 0) {
            leq.reinit(eq_count * 2, eq.getColSize());
        } else {
            leq.growRow(eq_count*2);
        }
        for (UINT i = 0; i < eq.getRowSize(); i++) {
            if (eq_removed.get(i)) {
                continue;
            }
            leq.setRows(c, c, eq, i);
            leq.mulOfRow(c, -1);
            leq.setRows(c+1, c+1, eq, i);
            c+=2;
        }
    }
}


//Normalize linear programming.
//  1. Converting equality constraint into inequality constraint.
//  2. To ensure variables have positive bound.
//Return the starting column index of constant term.
//NOTE:
//  Some new auxillary/dummy variables may be generated.
//  Caller must ensure that 'leq' does not contain constraints of '>='.
template <class Mat, class T>
INT SIX<Mat, T>::normalize(OUT Mat & newleq,
                           OUT Mat & newvc,
                           OUT INTMat & vcmap,
                           OUT Mat & newtgtf,
                           Mat const& vc,
                           Mat const& eq,
                           Mat const& leq,
                           Mat const& tgtf)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    UINT col_last_var = m_rhs_idx - 1;
    UINT vars = col_last_var + 1;
    Mat tmpleq(leq);
    convertEq2Ineq(tmpleq, eq);

    //Ensuring variable has nonnegative constraint.
    //e.g: Given 'vc' as follows:
    //    v0  v1  v2  C
    //   -1   0   0   0
    //    0   0   0   0
    //    0   0  -1   0
    //it means that given constraints are v0 >= 0 , v2 >= 0,
    //but there is no knownledge about v1ㄛ thus v1 might be
    //negative!
    //Thus here introduce new auxillary/dummy variables v1' and v1'',
    //which satified a new equation: v1 = v1' - v1'', where v1' >= 0,
    //v1'' >= 0.
    UINT leqgrowcols = 0, tgtfgrowcols = 0;
    UINT vcgrowrows = 0, vcgrowcols = 0;
    for (UINT col = 0; col <= col_last_var; col++) {
        bool has_constrain = false;
        for (UINT row = 0; row < vc.getRowSize(); row++) {
            if (vc.get(row, col) != 0) {
                //There are constraints for variable 'i'.
                has_constrain = true;
            }
            if (vc.get(row, col) > 0) {
                //Variable should only have one nonnegative constraint.
                //e.g: xi <= N, so it was necessary to generate leq
                //constraints.
                ASSERTN(0, ("constraint for No.%d variable is negative. "
                            "Please set the variable unbound or insert v<=0 "
                            "into inequalities before invoke SIX.", col));
            }
        }
        if (!has_constrain) {
            //No constraints for variable 'i', thus
            //introduce auxillary variable.
            //Replaces v with v' and insert a new column indicating the v''.
            vcgrowrows++;
            vcgrowcols++;
            leqgrowcols++;
            tgtfgrowcols++;
        }
    }

    //Extract the variable-constraints which coefficient is not zero.
    vc.inner(newvc, 0, 0, vc.getRowSize() - 1, col_last_var);
    if (vcgrowcols > 0) {
        newvc.growCol(vcgrowcols); //for new dummy variables.
    }

    //So we require the number of 'vcgrowrows' auxillary/dummy variables.
    newvc.growCol(vc, col_last_var + 1, vc.getColSize() - 1);
    if (vcgrowrows > 0) {
        newvc.growRow(vcgrowrows); //for new auxillary variables
    }

    //Inserting auxillary/dummy variables.
    newleq = tmpleq;
    newleq.insertColumnsBefore(m_rhs_idx, leqgrowcols);

    //Growing target function for substituting an original unbound
    //variable with new auxillary/dummy variables.
    newtgtf = tgtf;
    newtgtf.insertColumnsBefore(m_rhs_idx, tgtfgrowcols);

    //Constructing an injective mapping between original variables and
    //auxillary/dummy variables.
    vcmap.reinit(0, 0);
    INTMat m(1, 3);
    Mat n;
    for (UINT i = 0; i < vars; i++) {
        if (!vc.is_colequ(i, 0)) { continue; }

        //No constraint of variable i in 'vc'.
        //Aforementioned formual wrote: v = v' - v'', thus 'newvc'
        //replaces v with v' and insert a new column indicating the v''.
        //Therefore colum 'i' and 'col_last_var + 1' portray
        //variable v' and v'', and their coefficient are both -1.
        //e.g: constraint of v' is: -v' <= 0, whereas of v'' is -v'' <= 0.
        newvc.set(i, i, -1);
        newvc.set(col_last_var + 1, col_last_var + 1, -1);

        //Construct an injective mapping to describe the relationship
        //bewteen v, v' and v''.
        //The mapping is consist of: v, v', v''.
        m.sete(3, i, i, col_last_var + 1);
        vcmap.growRow(m);

        //Inserting new dummy variables in inequality matrix.
        //Note n will be reinited.
        tmpleq.innerColumn(n, i, i);
        newleq.setCol(col_last_var + 1, n);
        newleq.mulOfColumn(col_last_var + 1, -1);

        //Inserting new dummy variables in target-function.
        tgtf.innerColumn(n, i, i);
        newtgtf.setCol(col_last_var + 1, n);
        newtgtf.mulOfColumn(col_last_var + 1, -1);
        col_last_var++;
    }
    return col_last_var + 1;
}


//Construct slack-form by inserting slack variable for each of inequlities.
//'tgtf': target function
//'leq': slack form
//'vc': variable constraints
//NOTICE:
//  For any constraint: f <= c, we can represent it with
//  the else two constraints as: f + s = c, s >= 0, where s is
//  slack variable.
template <class Mat, class T>
void SIX<Mat, T>::slack(MOD Mat & tgtf,
                        MOD Mat & leq,
                        MOD Mat & vc,
                        MOD INT & rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    UINT orig_leqs = leq.getRowSize();
    leq.insertColumnsBefore(rhs_idx, orig_leqs);
    vc.insertColumnsBefore(rhs_idx, orig_leqs);
    vc.growRow(orig_leqs);

    //coefficient of slack variables in 'tgtf' are 0 when initialized.
    tgtf.insertColumnsBefore(rhs_idx, orig_leqs);

    //Set diagonal elements to -1.
    //The each rows of 'vc' portrays one of the variables constraint.
    //e.g:  -x1  0     0   <= 0
    //      0    -x2   0   <= 0
    //      0    0     -x3 <= 0
    for (UINT i = 0; i < orig_leqs; i++) {
        leq.set(i, rhs_idx + i, 1);
        vc.set(rhs_idx + i, rhs_idx + i, -1);
    }

    //Amendenting the starting column index of constant part
    rhs_idx += orig_leqs;
}


//Pivot row-tableaux of normalized-form of basis-variable.
//
//'eq': equalities in normalized form which contained slack variables.
//'nv': index of non-basis variable that swap-in.
//'bv': index of basis variable that swap-out.
//'tgtf': target function.
//'nvset': a set of non-basic variables.
//'bvset': a set of basic variables.
//'bv2eqmap': mapping from index of basic-variable to the
//    relevant equation.
//'nv2eqmap': mapping from index of nonbasic-variable to the
//    relevant equation.
//'rhs_idx': index of starting column of constant part.
//
//NOTICE:
//    The system of equations must be represented in normalized form.
//    A pivoting of a m*n tableaux requires one variable swapping,
//    one division, m*n-1 multiplication, m*n-m-n+1 addition and n-1
//    sign change.
template <class Mat, class T>
void SIX<Mat, T>::pivot(UINT nv, UINT bv, MOD PVParam<Mat> & pp)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    Mat & eq = *pp.eq;
    Mat & tgtf = *pp.tgtf;
    Vector<bool> & nvset = *pp.nvset;
    Vector<bool> & bvset = *pp.bvset;
    Vector<INT> & bv2eqmap = *pp.bv2eqmap;
    Vector<INT> & eq2bvmap = *pp.eq2bvmap;
    INT rhs_idx = pp.rhs_idx;

    //The index of equations starting at 0.
    INT eqnum = bv2eqmap.get(bv);
    ASSERTN(eqnum >= 0, ("index must nonnegative"));
    ASSERT0(IS_INEQ(eq.reduce(eqnum, nv), 0));
    eq.mulOfRow(eqnum, 1/(eq.get(eqnum, nv)));

    Mat nvexp;
    eq.innerRow(nvexp, eqnum, eqnum);

    //Coeff of 'nv' of 'eq' is '1'.
    //Substitute each nv for all equations with
    //a expression (ax1 + bx2 + cx3) such as:
    //  nv = ax1 + bx2 +cx3
    for (UINT i = 0; i < eq.getRowSize(); i++) {
        if (i == (UINT)eqnum) {
            continue;
        }
        T coeff_of_nv = -eq.get(i, nv);
        for (UINT j = 0; j < eq.getColSize(); j++) {
            T v = coeff_of_nv * nvexp.get(0, j);
            eq.set(i, j, eq.get(i, j) + v);
        }
    }

    //Substitute variables 'nv' in target function with its expression
    //such as:
    //f(x) = w*nv + u*x4 =>
    //f(x) = w*(a*x1 + b*x2 + c*x3) + u*x4
    nvexp.mul(-1);
    for (UINT i = rhs_idx; i < nvexp.getColSize(); i++) {
        nvexp.set(0, i, -nvexp.get(0, i));
    }
    nvexp.mul(tgtf.get(0, nv));
    tgtf.addRowToRow(nvexp, 0, 0);

    //Switch the role of 'nv' and 'bv', and update relevant information.
    nvset.set(nv, false);
    nvset.set(bv, true);
    bvset.set(nv, true);
    bvset.set(bv, false);
    eq2bvmap.set(bv2eqmap.get(bv), nv);
    bv2eqmap.set(nv, bv2eqmap.get(bv));
    bv2eqmap.set(bv, INVALID_EQNUM);
}


//Verify the legality of input data structures and
//initialize the constant-term column.
template <class Mat, class T>
void SIX<Mat, T>::checkAndInitConst(Mat const& leq,
                                    Mat const& eq,
                                    Mat const& tgtf,
                                    Mat const& vc,
                                    INT rhs_idx)
{
    DUMMYUSE(vc);
    DUMMYUSE(tgtf);
    DUMMYUSE(rhs_idx);
    ASSERTN(rhs_idx == -1 ||
            rhs_idx == (INT)leq.getColSize() -1, ("unsupport"));
    INT max_cols = -1;
    if (eq.size() != 0 && leq.size() != 0) {
        ASSERTN(eq.getColSize() == leq.getColSize(), ("unmatch variables"));
        max_cols = eq.getColSize();
    } else if (eq.size() != 0) {
        max_cols = eq.getColSize();
    } else if (leq.size() != 0) {
        max_cols = leq.getColSize();
    } else {
        ASSERTN(0, ("no constraints"));
    }
    ASSERTN(rhs_idx == -1 || rhs_idx == (INT)leq.getColSize() -1,
            ("unsupport"));

    if (m_rhs_idx == -1) {
        m_rhs_idx = max_cols - 1; //Only one const-term.
    } else {
        ASSERTN(m_rhs_idx < (INT)max_cols && m_rhs_idx >= 1,
                ("out of boundary"));
    }

    UINT num_cols_of_const_term = max_cols - m_rhs_idx;

    DUMMYUSE(num_cols_of_const_term);
    ASSERTN(num_cols_of_const_term == 1,
            ("No yet support const term with multi-columns."));
    ASSERTN(tgtf.is_vec() && tgtf.getColSize() == (UINT)max_cols,
            ("multi target functions"));
    ASSERTN(vc.getRowSize() == (UINT)m_rhs_idx &&
            vc.getColSize() == (UINT)max_cols,
            ("unmatch variables constraints"));
}


//Calculate the dual maximization solution to solve original minimum problem.
//Assuming the followed set in order to clarify the method,
//    Ns is the set of NonBasic Var with n elements,
//    Bs is the set of Basic Var with m elements,
//    X1 is the set of real var with n elements,
//    X2 is the set of auxillary var with m elements.
//Also assuming original target function is of the form:
//        min() = ﹉Gi*Yi, i = (0,1,...,m),
//note that each auxillary var of maxm-problem is corresponding to
//the real var of original minm-problem.
//
//The derivation of the original solution conforms to the formula that if
//the final form of the dual target function was shown as:
//        max() = maxv + ﹉Cj*Xj, j﹋Ns, Cj≒0
//then the each elements of original solution is corresponding to
//the relevant coefficient of element which is of X2, namely,
//        Yi = -(C'i), where C'i﹋X2, i = (0,1,...,m).
//
//'leq': must be formed of : A而y≒c而
//'tgtf': original target function, b而y
//'vc': variable constraint of y.
//'rhs_idx': the starting column of const-term of 'leq'.
//
//NOTICE: The duality theory: Ax≒b, max(cx) is the dual of A而y≡c而, min(b而y)
template <class Mat, class T>
UINT SIX<Mat, T>::calcDualMaxm(OUT T & dual_maxv,
                               OUT Mat & dual_slack_sol,
                               OUT Mat & dual_tgtf,
                               OUT INT & dual_rhs_idx,
                               OUT INT & dual_num_nv,
                               Mat const& tgtf,
                               MOD Mat &, //vc
                               Mat const& leq,
                               INT rhs_idx)
{
    INT num_of_const_col = leq.getColSize() - rhs_idx;
    ASSERT0(num_of_const_col >= 1);

    //The constraint of minimum problem is -A而y≒-c而, y≡0.
    //Generate the constraint of corresponding max-problem.
    Mat A, b, c;
    leq.innerColumn(A, 0, rhs_idx - 1);
    A.trans();
    Mat dual_leq(A);
    dual_leq.growCol(num_of_const_col);
    dual_leq.mul(-1); //gen [A, 0]

    //gen [A, b]
    for (INT i = 0; i < rhs_idx; i++) {
        dual_leq.set(i, dual_leq.getColSize() - num_of_const_col,
                    tgtf.get(0, i));
    }

    //Generate [c], the coeff of target function of max-problem.
    leq.innerColumn(dual_tgtf, rhs_idx, leq.getColSize() - num_of_const_col);
    dual_tgtf.trans();
    dual_tgtf.growCol(num_of_const_col);
    dual_tgtf.mul(-1);

    //Column of const-term
    dual_rhs_idx = dual_tgtf.getColSize() - num_of_const_col;
    dual_num_nv = dual_rhs_idx;

    //constraints of dual variable
    Mat dual_vc;
    dual_vc.growRowAndCol(dual_rhs_idx , dual_rhs_idx );
    dual_vc.eye(-1); //dual variable must be nonnegative one.
    dual_vc.growCol(num_of_const_col);

    /////////////////////////////////
    //Starting solve the dual problem
    /////////////////////////////////
    //dual bv2eqmap, eq2bvmap
    Vector<INT> dual_bv2eqmap, dual_eq2bvmap;
    Vector<bool> dual_nvset; //Nonbasis variable set
    Vector<bool> dual_bvset; //Basis variable set
    UINT status = TwoStageMethod(dual_leq,
                                 dual_vc,
                                 dual_tgtf,
                                 dual_slack_sol,
                                 dual_maxv,
                                 dual_nvset,
                                 dual_bvset,
                                 dual_bv2eqmap,
                                 dual_eq2bvmap,
                                 dual_rhs_idx);
    if (status == SIX_SUCC) {
        //Do not forget the const-term.
        for (UINT i = rhs_idx; i < tgtf.getColSize(); i++) {
            dual_maxv = dual_maxv + tgtf.get(0, i);
        }
    }
    return status;
}


#define SOLVE_DUAL_PROBLEM
#ifdef SOLVE_DUAL_PROBLEM
//Solve the minmum problem of linear prgramming.
template <class Mat, class T>
UINT SIX<Mat, T>::minm(OUT T & minv,
                       OUT Mat & sol,
                       Mat const& tgtf,
                       Mat const& vc,
                       Mat const& eq,
                       Mat const& leq,
                       INT rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    m_rhs_idx = rhs_idx;
    checkAndInitConst(leq, eq, tgtf, vc, rhs_idx);
    ASSERT0(m_rhs_idx > 0);

    Mat nd_leq, nd_vc, nd_tgtf; //normalized 'leq', 'vc', 'tgtf'.
    //record variable's mapping if that exist unconstrained variable.
    INTMat orig_vcmap;

    //First, normalize the minimum problem,
    //and record the column of const-term after normalizing.
    INT nd_rhs_idx = normalize(nd_leq, nd_vc, orig_vcmap,
                               nd_tgtf, vc, eq, leq, tgtf);
    ASSERTN(nd_rhs_idx > 0, ("at least one variable"));

    //Calclate dual problem solution.
    Mat dual_slack_sol, dual_tgtf;
    INT dual_rhs_idx = -1;
    INT dual_num_nv = -1; //record the number of dual-nv
    //Record the maximum value when two-stage iteration finished.
    T dual_maxv = 0;
    minv = 0;
    UINT status = calcDualMaxm(dual_maxv,
                               dual_slack_sol,
                               dual_tgtf,
                               dual_rhs_idx,
                               dual_num_nv,
                               nd_tgtf,
                               nd_vc,
                               nd_leq,
                               nd_rhs_idx);
    if (status == SIX_SUCC) {
        INT num_of_const_term = dual_tgtf.getColSize() - dual_rhs_idx;
        //yi = -Coeff(n+i), i﹋Bs
        UINT num_of_orig_var = //also be the number of dual-bv.
            (dual_tgtf.getColSize() - num_of_const_term) - dual_num_nv;

        //Basic variables of dual solution represent
        //the nonbasic variables of original solution.
        //e.g:If the dual-slack-solution is:
        //    {x0, x1, x2, ... x(n), y0, y1, ... ym, C}
        //the original-solution should be the form that:
        //    {y0, y1, ... ym, C}
        Mat orig_tmp_slack_sol(1, num_of_orig_var + num_of_const_term);
        for (UINT k = 0; k < num_of_orig_var; k++) {
            orig_tmp_slack_sol.set(0, k, -dual_tgtf.get(0, dual_num_nv + k));
        }

        //minimum value
        T v;
        calcFinalSolution(sol, v,
                          orig_tmp_slack_sol,
                          orig_vcmap,
                          tgtf,
                          nd_rhs_idx);
        v.reduce();
        dual_maxv.reduce();
        sol.reduce();
        //v might be not equal to 'dual_maxv' when T is float point
        //because precision error.
        //e.g:min = -x1-5x2
        //    -x1 + x2 <= 2
        //    5x1 + 6x2 <= 30
        //    x1 <= 4
        //    x1, x2 >= 0
        //    x1, x2 is integer
        // Solution is:
        //    min is -16, x1=1, x2=3, C=1
        //    v is -16.0, but dual_max is -17.0.
        //ASSERT0(IS_EQ(v, dual_maxv));
        minv = v;
    }
    return status;
}
#else

//Solve minmum problem of linear prgramming naively.
template <class Mat, class T>
UINT SIX<Mat, T>::minm(OUT T & minv,
                       OUT Mat & sol,
                       Mat const& tgtf,
                       Mat const& vc,
                       Mat const& eq,
                       Mat const& leq,
                       INT rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    Mat neg_tgtf = tgtf;
    neg_tgtf.mul(-1);
    T maxv;
    UINT st = maxm(maxv, sol, neg_tgtf, vc, eq, leq);
    if (st == SIX_SUCC) {
        minv = -maxv;
    }
    return st;
}
#endif


template <class Mat, class T>
bool SIX<Mat, T>::isConstItermFeasible(Mat & newleq, INT rhs_idx) const
{
    ASSERTN(newleq.getColSize() - rhs_idx == 1,
            ("multiple const-term is unsupport"));
    for (UINT i = 0; i < newleq.getRowSize(); i++) {
        if (newleq.get(i, rhs_idx) < 0) {
            return false;
        }
    }
    return true;
}


//Establish the mapping of variable and relevant inequalities, and try to
//find the first feasible solution.
//Return ST_SUCC if find a feasible solution, otherwise
//return SIX_NO_PRI_FEASIBLE_SOL.
//
//'nvset': Nonbasis variable set
//'bvset': Basis variable set
//'bv2eqmap, eq2bvmap': mapping from index of basis-variable
//    to the index of relevant equality.
//'new_rhs_idx': column of constant term when the stage was finished.
template <class Mat, class T>
INT SIX<Mat, T>::stage1(OUT Mat & newleq,
                        OUT Mat & newvc,
                        OUT Mat & newtgtf,
                        OUT Vector<bool> & nvset,
                        OUT Vector<bool> & bvset,
                        Vector<INT> & bv2eqmap,
                        Vector<INT> & eq2bvmap,
                        MOD INT & new_rhs_idx)
{
    bool has_pos_coeff_var = false;
    for (UINT i = 0; i < (UINT)new_rhs_idx; i++) {
        if (newtgtf.get(0, i) > 0) {
            has_pos_coeff_var = true;
            break;
        }
    }

    //First of all, we are going to find basic feasible solution.
    if (!has_pos_coeff_var || !isConstItermFeasible(newleq, new_rhs_idx)) {
    //if (!has_pos_coeff_var) {
        //All these coefficient of variables in target function are
        //negative or zero! Thus the possible optimum solution can be
        //gained by set all variable zero.
        //Nonetheless, that solution might be infeasible.
        //Hence, the examination for feasibility is indispensable.
        if (!constructBasicFeasibleSolution(newleq,
                                            newtgtf,
                                            newvc,
                                            nvset,
                                            bvset,
                                            bv2eqmap,
                                            eq2bvmap,
                                            new_rhs_idx)) {
            ASSERT0(dump_str("\nSTAGE1 status=%s",
                             getStatusName(SIX_NO_PRI_FEASIBLE_SOL)));
            return SIX_NO_PRI_FEASIBLE_SOL;
        }
        ASSERT0(dump_str("\nSTAGE1 status=found_basic_feasible_solution"));
        return SIX_SUCC;
    }

    ASSERT0(dump_str("\nSTAGE1 status=slack_transformation"));
    UINT bv_start_idx = new_rhs_idx;

    //A slack transformation.
    slack(newtgtf, newleq, newvc, new_rhs_idx);

    //Initializing basic-variable and non-basic-variable set.
    //Mapping from nonbasic-variable to its relevant referrenced
    //equalities.
    for (UINT i = 0; i < bv_start_idx; i++) {
        bv2eqmap.set(i, INVALID_EQNUM);
        nvset.set(i, true);
    }

    UINT j = 0;
    for (UINT i = bv_start_idx; i < (UINT)new_rhs_idx; i++, j++) {
        bvset.set(i, true);
        nvset.set(i, false);
        eq2bvmap.set(j, i);
        bv2eqmap.set(i, j);
    }
    return SIX_SUCC;
}


//Calculate the target-function's value, and derive
//original variable's value from the set of {Nonbasic var} ﹍ {Basic var}.
//'rhs_idx': column of const-term of 'slack_sol'.
template <class Mat, class T>
void SIX<Mat, T>::calcFinalSolution(OUT Mat & sol,
                                    OUT T & v,
                                    MOD Mat & slack_sol,
                                    IN INTMat & vcmap,
                                    Mat const& orignal_tgtf,
                                    INT rhs_idx)
{
    DUMMYUSE(rhs_idx);

    //After normalizing done, new dummy nonbasic variables might be generated,
    //recalculate the value to recover the real variables.
    //e.g:v1 = v1'- v1'', v1 is original variable.
    if (vcmap.getRowSize() > 0) {
        for (UINT i = 0; i < vcmap.getRowSize(); i++) {
            UINT real_nv_idx = vcmap.get(i, 0);
            UINT dummy_nv_idx1 = vcmap.get(i, 1);
            UINT dummy_nv_idx2 = vcmap.get(i, 2);
            ASSERTN(real_nv_idx < (UINT)rhs_idx &&
                    dummy_nv_idx1 < (UINT)rhs_idx &&
                    dummy_nv_idx2 < (UINT)rhs_idx, ("illegal vcmap"));
            slack_sol.set(0, real_nv_idx,
                          slack_sol.get(0, dummy_nv_idx1) -
                          slack_sol.get(0, dummy_nv_idx2));
        }
    }

    //Recording the real variable's value.
    sol.reinit(1, orignal_tgtf.getColSize());
    for (UINT i = 0; i < (UINT)m_rhs_idx; i++) {
        sol.set(0, i, slack_sol.get(0, i));
    }

    //Coeff of const-term should be 1.
    for (UINT k = m_rhs_idx; k < (UINT)sol.getColSize(); k++) {
        sol.set(0, k, 1);
    }

    //Calculating the target function's value.
    v = 0;
    UINT num_of_const_term = sol.getColSize() - m_rhs_idx;
    DUMMYUSE(num_of_const_term);
    ASSERTN(num_of_const_term == 1,
            ("No yet support const term with multi-columns."));
    for (UINT j = 0; j < (UINT)sol.getColSize(); j++) {
        v = v + sol.get(0, j) * orignal_tgtf.get(0, j);
    }
}


//Two-stages simplex method.
//maxv: maximum value.
//slack_sol: since the final slack form is involved with some
//           auxiliary variables and slack variables, the final solution
//           resolved recorded temporarily with 'slack_sol'.
//nvset: indicate if variable is non-basic.
//bvset: indicate if variable is basic.
template <class Mat, class T>
UINT SIX<Mat, T>::TwoStageMethod(MOD Mat & newleq,
                                 MOD Mat & newvc,
                                 MOD Mat & newtgtf,
                                 MOD Mat & slack_sol,
                                 MOD T & maxv,
                                 MOD Vector<bool> & nvset,
                                 MOD Vector<bool> & bvset,
                                 MOD Vector<INT> & bv2eqmap,
                                 MOD Vector<INT> & eq2bvmap,
                                 MOD INT & new_rhs_idx)
{
    UINT status = stage1(newleq, newvc, newtgtf, nvset, bvset,
                         bv2eqmap, eq2bvmap, new_rhs_idx);
    if (status != SIX_SUCC) {
        return status;
    }
    ASSERT0(isConstItermFeasible(newleq, new_rhs_idx));
    return solveSlackForm(newtgtf, newleq,
                          newvc, maxv, slack_sol, nvset, bvset,
                          bv2eqmap, eq2bvmap, new_rhs_idx);
}


//Check and alarm if exists column that all of its element are 0 and 'tgtf'
//expected the variable's result.
//If it is the case, the variable that 0-column corresponds to
//has no any constraint, then 'tgtf' could have infinite solution,
//says, the system is unbound.
//Return true if system is unbound.
//e.g:
//    There are 3 variable i,j,k, but the second variable j's
//    boundary is empty. And target function is MAX:i+2j+k
//         i  j  k  CSt
//        -1  0  0  0
//         1  0  0  99
//         0  0  1  99
//        -1  0  0  0
//    As aforementioned, the sysmtem is unbound because j
//    can be any integer.
template <class Mat, class T>
bool SIX<Mat, T>::verifyEmptyVariableConstrain(Mat const& tgtf,
                                               Mat const&, //variable constraint
                                               Mat const& eq,
                                               Mat const& leq,
                                               INT rhs_idx)
{
    Vector<bool> is_nonzero;
    for (INT j = 0; j < rhs_idx; j++) {
        if (leq.getColSize() > 0 && !leq.is_colequ(j, 0)) {
            is_nonzero.set(j, true);
        }
        if (eq.getColSize() > 0 && !eq.is_colequ(j, 0)) {
            is_nonzero.set(j, true);
        }
    }
    for (INT j = 0; j < rhs_idx; j++) {
        if (!is_nonzero.get(j) &&
            tgtf.get(0, j) != 0) {
            ASSERTN(0, ("%dth variable is unbounded, that will "
                        "incur the linear system to be unbound.\n", j));
        }
    }
    return false;
}


//Compute linear maximum solution.
//Return the status, include:
//  SIX_SUCC: Get maximum solution.
//  SIX_UNBOUND: Target function is unbound.
//maxv: maximum value.
//tgtf: target function.
//sol: optimum feasible solution.
//vc: variable constraints, one variable one row.
//eq: equalites which the solution should subject to .
//    e.g: i-j=10, eq is [1,-1,10], where the last column is rhs_idx.
//leq: inequalites which the solution should subject to .
//    e.g: i-j<=10, leq is [1,-1,10], where the last column is rhs_idx.
//rhs_idx: index number of constant column.
//    e.g: Given tgtf as [a*x1, b*x2, c*x3, 100], then rhs_idx is 3.
//
//NOTICE:
//  The columns size of 'sol', 'tgtf', 'vc', 'eq', 'leq' must be same.
template <class Mat, class T>
UINT SIX<Mat, T>::maxm(OUT T & maxv,
                       OUT Mat & sol,
                       Mat const& tgtf,
                       Mat const& vc,
                       Mat const& eq,
                       Mat const& leq,
                       INT rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    m_rhs_idx = rhs_idx;
    checkAndInitConst(leq, eq, tgtf, vc, rhs_idx);
    if (verifyEmptyVariableConstrain(tgtf, vc, eq, leq, m_rhs_idx)) {
        return SIX_UNBOUND;
    }

    //Mapping from index of basis-variable to the index of relevant equality.
    Mat newleq, newvc, newtgtf;
    INTMat vcmap;
    INT new_rhs_idx = normalize(newleq, newvc,
                                vcmap, newtgtf, vc, eq, leq, tgtf);
    ASSERTN(new_rhs_idx > 0, ("at least one variable"));

    //Record the maximum value when two-stage iteration finished.
    T maxv_of_two_stage_iter = 0;
    Mat slack_sol;
    Vector<bool> nvset; //Nonbasis variable set
    Vector<bool> bvset; //Basis variable set
    Vector<INT> bv2eqmap, eq2bvmap;
    UINT status = TwoStageMethod(newleq, newvc, newtgtf, slack_sol,
                                 maxv_of_two_stage_iter, nvset,
                                 bvset, bv2eqmap, eq2bvmap, new_rhs_idx);
    maxv = 0;
    if (status == SIX_SUCC) {
        calcFinalSolution(sol, maxv, slack_sol, vcmap, tgtf, new_rhs_idx);
        maxv_of_two_stage_iter.reduce();
        sol.reduce();
        maxv.reduce();
        //v might be not equal to 'dual_maxv' when T is float point
        //because precision error.
        //e.g:min = -x1-5x2
        //    -x1 + x2 <= 2
        //    5x1 + 6x2 <= 30
        //    x1 <= 4
        //    x1, x2 >= 0
        //    x1, x2 is integer
        // Solution is:
        //    min is -16, x1=1, x2=3, C=1
        //    v is -16.0, but dual_max is -17.0.
        //ASSERTN(IS_EQ(maxv_of_two_stage_iter, maxv), ("should be equal"));//hack
    }
    return status;
}


//Check if one column of eq/leq that all elements be 0 and 'tgtf'
//expects the result of the variable. If it is the case, the variable
//has not any constraints, and 'tgtf' might have infinite solutions,
//that is the system is unbound.
//e.g:
//  There are 3 variable i,j,k, but the second variable j's
//  boundary is empty. And target function is MAX:i+2j+k
//       i  j  k    CSt
//      -1  0  0 <= 0
//       1  0  0 <= 99
//       0  0  1 <= 99
//      -1  0  0 <= 0
//  As aforementioned, the sysmtem is unbound because j can
//  be any integer.
//  Set the coefficient of variable in 'tgtf' to be 0 if no
//  constraints related with it.
template <class Mat, class T>
void SIX<Mat, T>::reviseTargetFunc(MOD Mat & tgtf,
                                   Mat const& eq,
                                   Mat const& leq,
                                   INT rhs_idx)
{
    Vector<bool> is_nonzero;
    for (INT j = 0; j < rhs_idx; j++) {
        if (leq.getColSize() > 0 && !leq.is_colequ(j, 0)) {
            is_nonzero.set(j, true);
        }
        if (eq.getColSize() > 0 && !eq.is_colequ(j, 0)) {
            is_nonzero.set(j, true);
        }
    }
    for (INT j = 0; j < rhs_idx; j++) {
        if (!is_nonzero.get(j)) {
            tgtf.set(0, j, 0);
        }
    }
}
//END SIX



//
//START MIP, Mix Integer Programming
//
#define IP_SUCC 0
#define IP_UNBOUND 1
#define IP_NO_PRI_FEASIBLE_SOL 2
#define IP_NO_BETTER_THAN_BEST_SOL 3

template <class Mat, class T> class MIP : public Element<T> {
    BYTE m_is_init:1; //To make sure functions are idempotent.
    BYTE m_is_dump:1;
    Mat m_cur_best_sol;
    T m_cur_best_v;
    BMat * m_allow_rational_indicator;
    INT m_rhs_idx;
    UINT m_indent;
    UINT m_times;
    UINT RecusivePart(OUT T & v,
                      OUT Mat & sol,
                      Mat const& tgtf,
                      Mat const& vc,
                      Mat const& eq,
                      Mat const& leq,
                      INT rhs_idx,
                      bool is_max,
                      bool is_bin,
                      IN INTMat & fork_count);
public:
    MIP(bool is_dump = false)
    {
        m_is_init = false;
        m_rhs_idx = -1;
        m_is_dump = is_dump;
        init();
    }
    virtual ~MIP() { destroy(); }

    void init()
    {
        if (m_is_init) { return; }
        m_cur_best_sol.init();
        m_cur_best_v = 0;
        m_allow_rational_indicator = nullptr;
        m_indent = 0;
        m_times = 0;
        m_is_init = true;
    }

    void destroy()
    {
        if (!m_is_init) { return; }
        m_rhs_idx = -1;
        m_cur_best_sol.destroy();
        m_cur_best_v = 0;
        m_allow_rational_indicator = nullptr;
        m_is_init = false;
    }

    void checkAndInitConst(Mat const& leq,
                           Mat const& eq,
                           Mat const& tgtf,
                           Mat const& vc,
                           INT rhs_idx);
    virtual bool is_satisfying(OUT UINT & row,
                               OUT UINT & col,
                               IN Mat & sol,
                               bool is_bin);
    virtual UINT minm(OUT T & minv,
                      OUT Mat & res,
                      Mat const& tgtf,
                      Mat const& vc,
                      Mat const& eq,
                      Mat const& leq,
                      bool is_bin = false,
                      IN BMat * rational_indicator = nullptr,
                      INT rhs_idx = -1); //Linear minmum solution
    virtual UINT maxm(OUT T & maxv,
                      OUT Mat & res,
                      Mat const& tgtf,
                      Mat const& vc,
                      Mat const& eq,
                      Mat const& leq,
                      bool is_bin = false,
                      IN BMat * rational_indicator = nullptr,
                      INT rhs_idx = -1); //Linear maximum solution
    void reviseTargetFunc(MOD Mat & tgtf,
                          Mat const& eq,
                          Mat const& leq,
                          INT rhs_idx);
    virtual FILE * dump_open_file() const;
    virtual bool dump_prt_indent(FILE * h) const;
    virtual bool dump_start_six(Mat const& tgtf,
                                Mat const& vc,
                                Mat const& eq,
                                Mat const& leq) const;
    virtual bool dump_end_six(UINT status, T v, Mat & sol, bool is_maxm) const;
    virtual bool dump_is_satisfying() const;
    virtual bool dump_floor_branch(INT floor) const;
    virtual bool dump_ceiling_branch(INT ceil) const;
};


//This function create file handler for dumpping.
//Start dump functions
template <class Mat, class T>
FILE * MIP<Mat, T>::dump_open_file() const
{
    if (!m_is_dump) { return nullptr; }
    FILE * h = ::fopen(SIX_DUMP_NAME, "a+");
    ASSERTN(h, ("%s create failed!!!", SIX_DUMP_NAME));
    return h;
}


template <class Mat, class T>
bool MIP<Mat, T>::dump_prt_indent(FILE * h) const
{
    for (UINT i = 0; i < m_indent; i++) {
        fprintf(h, "  ");
    }
    return true;
}


template <class Mat, class T>
bool MIP<Mat, T>::dump_start_six(Mat const&, //target function
                                 Mat const&, //variable constraint.
                                 Mat const&, //equations.
                                 Mat const&) const //inequalities.
{
    FILE * h = dump_open_file();
    if (h == nullptr) { return true; }
    if (m_times == 0) {
        fprintf(h, "\n");
    }
    dump_prt_indent(h);
    fprintf(h, "TIME=%u...\n", m_times);
    dump_prt_indent(h);
    fprintf(h, "BEGIN SIX...\n");
    fclose(h);
    return true;
}


template <class Mat, class T>
bool MIP<Mat, T>::dump_end_six(UINT status, T v, Mat & sol, bool is_maxm) const
{
    FILE * h = dump_open_file();
    if (h == nullptr) { return true; }
    StrBuf buf(32);
    dump_prt_indent(h);
    fprintf(h, "\nEND SIX,status=%s,%s_value=%s,solution={",
            getStatusName(status),
            is_maxm ? "max" : "min",
            v.format(buf));
    for (UINT i = 0; i < sol.getColSize(); i++) {
        fprintf(h, "%s,", sol.get(0, i).format(buf));
    }
    fprintf(h, "}\n");
    fclose(h);
    return true;
}


template <class Mat, class T>
bool MIP<Mat, T>::dump_is_satisfying() const
{
    FILE * h = dump_open_file();
    if (h == nullptr) { return true; }
    dump_prt_indent(h);
    fprintf(h, "SOL Satisfying!!! return IP_SUCC.\n");
    fclose(h);
    return true;
}


template <class Mat, class T>
bool MIP<Mat, T>::dump_floor_branch(INT floor) const
{
    FILE * h = dump_open_file();
    if (h == nullptr) { return true; }
    fprintf(h, "\n");
    dump_prt_indent(h);
    fprintf(h, "ENTER floor(left) branch, floor_value=%d\n", floor);
    fclose(h);
    return true;
}


template <class Mat, class T>
bool MIP<Mat, T>::dump_ceiling_branch(INT ceil) const
{
    FILE * h = dump_open_file();
    if (h == nullptr) { return true; }
    fprintf(h, "\n");
    dump_prt_indent(h);
    fprintf(h, "ENTER ceiling(right) branch, ceiling_value=%d\n", ceil);
    fclose(h);
    return true;
}


//Verify the legality of input data structures and initialize
//the constant-term column.
template <class Mat, class T>
void MIP<Mat, T>::checkAndInitConst(Mat const& leq,
                                    Mat const& eq,
                                    Mat const& tgtf,
                                    Mat const& vc,
                                    INT rhs_idx)
{
    DUMMYUSE(rhs_idx);
    DUMMYUSE(tgtf);
    ASSERTN(rhs_idx == -1 || rhs_idx == (INT)leq.getColSize() -1,
            ("Parameter is not yet support right now."));
    INT max_cols = -1;
    if (eq.size() != 0 && leq.size() != 0) {
        ASSERTN(eq.getColSize() == leq.getColSize(),
                ("unmatch variables"));
        max_cols = eq.getColSize();
    } else if (eq.size() != 0) {
        max_cols = eq.getColSize();
    } else if (leq.size() != 0) {
        max_cols = leq.getColSize();
    } else {
        ASSERTN(0, ("no constraints"));
    }
    ASSERTN(rhs_idx == -1 || rhs_idx == (INT)leq.getColSize()-1,
            ("unsupport"));

    if (m_rhs_idx == -1) {
        m_rhs_idx = max_cols -1; //Only one const-term.
    } else {
        ASSERTN(m_rhs_idx < (INT)max_cols && m_rhs_idx >= 1,
                ("out of boundary"));
    }

    UINT num_cols_of_const_term = max_cols - m_rhs_idx;

    DUMMYUSE(num_cols_of_const_term);
    ASSERTN(num_cols_of_const_term == 1,
            ("No yet support const term with multi-columns."));

    ASSERTN(tgtf.is_vec() &&
            tgtf.getColSize() == (UINT)max_cols,
            ("multi target functions"));

    ASSERTN(vc.getRowSize() == (UINT)m_rhs_idx &&
            vc.getColSize() == (UINT)max_cols,
            ("unmatch variables constraints"));

    if (m_allow_rational_indicator != nullptr) {
        ASSERTN(m_allow_rational_indicator->getColSize() == (UINT)max_cols,
                ("unmatch variable"));
    }

    //For now, we only permit Variable constraint 'vc' of each
    //variable to be 0, namely only i>=0 is allowed. One should
    //establish constraints i'>=0, and i=i'+3 if i>=3 is required.
    //Similar, if i<=0, add constraints i=x-y, x>=0, y>=0, x<=y.
    if (!vc.is_colequ(m_rhs_idx, 0)) {
        ASSERTN(num_cols_of_const_term == 1,
                ("no yet support const term with multi-columns."));
        ASSERTN(0, ("variable constraint can only be i>=0"));
    }
    for (INT i = 0; i < m_rhs_idx; i++) {
        if (vc.get(i, i) > 0) {
            ASSERTN(0, ("coeff of variable must be -1, e.g: -i<=0"));
        }
    }
}


//Return true if 'sol' is satisfying.
template <class Mat, class T>
bool MIP<Mat, T>::is_satisfying(OUT UINT & row,
                                OUT UINT & col,
                                IN Mat & sol,
                                bool is_bin)
{
    if (m_allow_rational_indicator != nullptr) {
        ASSERT0(m_allow_rational_indicator->getRowSize() ==
                sol.getRowSize() &&
                m_allow_rational_indicator->getColSize() ==
                sol.getColSize());
        for (UINT i = 0; i < sol.getRowSize(); i++) {
            for (UINT j = 0; j < sol.getColSize(); j++) {
                T v = sol.reduce(i, j);
                if (!m_allow_rational_indicator->get(i, j)) {
                    if (!v.is_int()) {
                        //Not an integer.
                        row = i;
                        col = j;
                        return false;
                    }
                    if (is_bin && IS_INEQ(v, T(0)) && IS_INEQ(v, T(1))) {
                        //Not an 0-1 integer.
                        row = i;
                        col = j;
                        return false;
                    }
                } //end if
            } //end for
        } //end for
        return true;
    } else if (is_bin) {
        for (UINT i = 0; i < sol.getRowSize(); i++) {
            for (UINT j = 0; j < sol.getColSize(); j++) {
                T v = sol.reduce(i, j);
                if (IS_INEQ(v, T(0)) && IS_INEQ(v, T(1))) {
                    row = i;
                    col = j;
                    return false;
                }
            }
        }
        return true;
    }
    return sol.is_imat(&row, &col);
}


template <class Mat, class T>
void MIP<Mat, T>::reviseTargetFunc(MOD Mat & tgtf,
                                   Mat const& eq,
                                   Mat const& leq,
                                   INT rhs_idx)
{
    SIX<Mat, T> six(0, 0xFFFFFFFF, m_is_dump);
    six.reviseTargetFunc(tgtf, eq, leq, rhs_idx);
}


//Recursive subroutine.
//'is_max': true refers to solve the maximum problem
//'is_bin': true refers to solve the binary(0-1) programming
template <class Mat, class T>
UINT MIP<Mat, T>::RecusivePart(OUT T & v,
                               OUT Mat & sol,
                               Mat const& tgtf,
                               Mat const& vc,
                               Mat const& eq,
                               Mat const& leq,
                               INT rhs_idx,
                               bool is_max,
                               bool is_bin,
                               IN INTMat & fork_count)
{
    STATUS status;
    SIX<Mat, T> six(m_indent, 10000, m_is_dump);
    ASSERT0(dump_start_six(tgtf, vc, eq, leq));
    m_times++;
    if (is_max) {
        status = six.maxm(v, sol, tgtf, vc, eq, leq, rhs_idx);
        ASSERT0(dump_end_six(status, v, sol,true));
    } else {
        status = six.minm(v, sol, tgtf, vc, eq, leq, rhs_idx);
        ASSERT0(dump_end_six(status, v, sol, false));
    }
    if (SIX_SUCC != status) {
        switch (status) {
        case SIX_UNBOUND:
            status = IP_UNBOUND;
            break;
        case SIX_NO_PRI_FEASIBLE_SOL:
            status = IP_NO_PRI_FEASIBLE_SOL;
            break;
        case SIX_OPTIMAL_IS_INFEASIBLE:
            status = IP_NO_PRI_FEASIBLE_SOL;
            break;
        case SIX_TIME_OUT:
        default: UNREACHABLE();
        }
        return status;
    }

    //Query integer solution by cutting convex with hyper-plane.
    UINT row, col;
    if (is_satisfying(row, col, sol, is_bin)) {
        ASSERT0(dump_is_satisfying());
        return IP_SUCC;
    }
    if (m_cur_best_sol.size() != 0) {
        //Find a solution, do check if it involved rational value.
        if (is_max) {
            if (IS_LE(v, m_cur_best_v)) {
                return IP_NO_BETTER_THAN_BEST_SOL;
            }
        } else {
            if (IS_GE(v, m_cur_best_v)) {
                return IP_NO_BETTER_THAN_BEST_SOL;
            }
        }
    }
    if (fork_count.get(0, col) >= 1) {
        //e.g: Avoid the cyclic recusive computing.
        //1th, the fork point is 0,
        //        221/3, 63, 0, 3, 72,    65
        //2th, the fork point is 5,
        //        73,    63, 0, 3, 214/3, 65
        //3th, the fork point is 0,
        //        221/3, 63, 0, 3, 72,    65
        //...
        return IP_NO_PRI_FEASIBLE_SOL;
    }
    fork_count.set(0, col, fork_count.get(0, col)+1);

    INT sol_floor = 0;
    INT sol_ceil = 0;
    Mat new_constraint(1, tgtf.getColSize());

    //Solving the floor part.
    Mat tleq = leq;
    Mat teq = eq;
    if (is_bin) {
        //Append equations
        sol_floor = 0;
        sol_ceil = 1;
        new_constraint.set(0, col, 1);
        new_constraint.set(0, m_rhs_idx, sol_floor);
        teq.growRow(new_constraint, 0, new_constraint.getRowSize() - 1);
    } else {
        //Append LT constraints
        ASSERTN(row == 0, ("only support unique solution"));
        sol_floor = sol.get(row, col).typecast2int();
        sol_ceil = sol_floor + 1;
        new_constraint.set(0, col, 1);
        new_constraint.set(0, m_rhs_idx, sol_floor);
        tleq.growRow(new_constraint, 0, new_constraint.getRowSize() - 1);
    }

    Mat tmp_sol;
    T tmpv;
    m_indent++;
    ASSERT0(dump_floor_branch(sol_floor));
    status = RecusivePart(v, sol, tgtf, vc, teq,
                          tleq, rhs_idx, is_max,
                          is_bin, fork_count);
    if (IP_SUCC == status) {
        tmp_sol = sol;
        tmpv = v;
        if (is_max) {
            if (m_cur_best_sol.size() == 0 || m_cur_best_v < v) {
                m_cur_best_sol = sol;
                m_cur_best_v = v;
            }
        } else {
            if (m_cur_best_sol.size() == 0 || m_cur_best_v > v) {
                m_cur_best_sol = sol;
                m_cur_best_v = v;
            }
        }
    }

    //Solving the ceil part.
    tleq = leq;
    teq = eq;
    if (is_bin) {
        //Append equations
        new_constraint.zero();
        new_constraint.set(0, col, 1);
        new_constraint.set(0, m_rhs_idx, sol_ceil);
        teq.growRow(new_constraint, 0, new_constraint.getRowSize() - 1);
    } else {
        //Append LT constraints
        new_constraint.zero();
        new_constraint.set(0, col, -1);
        new_constraint.set(0, m_rhs_idx, -sol_ceil);
        tleq.growRow(new_constraint, 0, new_constraint.getRowSize() - 1);
    }
    ASSERT0(dump_ceiling_branch(sol_ceil));

    status = RecusivePart(v, sol, tgtf, vc, teq,
                          tleq, rhs_idx, is_max,
                          is_bin, fork_count);
    if (IP_SUCC == status) {
        if (is_max) {
            //Maximum problem
            if (tmp_sol.size() != 0) {
                if (tmpv > v) {
                    v = tmpv;
                    sol = tmp_sol;
                }
            }
            if (m_cur_best_sol.size() == 0 || m_cur_best_v < v) {
                m_cur_best_sol = sol;
                m_cur_best_v = v;
            }
        } else {
            //Minimum problem
            if (tmp_sol.size() != 0) {
                if (tmpv < v) {
                    v = tmpv;
                    sol = tmp_sol;
                }
            }
            if (m_cur_best_sol.size() == 0 || m_cur_best_v > v) {
                m_cur_best_sol = sol;
                m_cur_best_v = v;
            }
        }
        m_indent--;
        return IP_SUCC;
    } else if (tmp_sol.size() != 0) {
        v = tmpv;
        sol = tmp_sol;
        if (is_max) {
            if (m_cur_best_sol.size() == 0 || m_cur_best_v < v) {
                m_cur_best_sol = sol;
                m_cur_best_v = v;
            }
        } else {
            if (m_cur_best_sol.size() == 0 || m_cur_best_v > v) {
                m_cur_best_sol = sol;
                m_cur_best_v = v;
            }
        }
        m_indent--;
        return IP_SUCC;
    }
    m_indent--;
    return status;
}


//Compute maximum solution of mixed integer programming.
//
//Return the result.
//    SUCC: Get maximum solution.
//    UNBOUND: Target function is unbound.
//'sol': optimum feasible solution.
//'vc': variable constraints, one variable one row.
//'eq': equalites which the solution should subject to .
//'leq': inequalites which the solution should subject to .
//'is_bin': true refers to solve the binary(0-1) programming,
//    else to solve integer programming.
//'rational_indicator': if it is not nullptr, TRUE element means
//    the solution permits to be rational.
//    e.g: If rational_indicator(0, j) is TRUE, the element j
//    of solution could be rational.
//'rhs_idx': index number of constant column.
//    e.g: Given tgtf as [ax1, bx2, cx3, 100], then rhs_idx is 3.
//
//NOTICE:
//    The columns size of 'sol', 'tgtf', 'vc', 'eq', 'leq' must be same.
template <class Mat, class T>
UINT MIP<Mat, T>::maxm(OUT T & maxv,
                       OUT Mat & sol,
                       Mat const& tgtf,
                       Mat const& vc,
                       Mat const& eq,
                       Mat const& leq,
                       bool is_bin,
                       IN BMat * rational_indicator,
                       INT rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    m_allow_rational_indicator = rational_indicator;
    checkAndInitConst(leq, eq, tgtf, vc, rhs_idx);
    m_cur_best_sol.deleteAllElem();
    m_cur_best_v = 0;
    m_times = 0;
    m_indent = 0;
    INTMat fork_count(1, tgtf.getColSize());
    return RecusivePart(maxv, sol, tgtf, vc, eq, leq,
                        rhs_idx, true, is_bin, fork_count);
}


//Solve the minimum solution of mixed integer programming.
//
//Return the result.
//    SUCC: Get minimum solution.
//    UNBOUND: Target function is unbound.
//'sol': optimum feasible solution.
//'vc': variable constraints, one variable one row.
//'eq': equalites which the solution should subject to .
//'leq': inequalites which the solution should subject to .
//'is_bin': true refers to solve the binary(0-1) programming,
//        else to solve integer programming.
//'rational_indicator': if it is not nullptr, TRUE element means
//        the solution permits to be rational.
//    e.g: If rational_indicator(0, j) is TRUE, the element j
//        of solution could be rational.
//'rhs_idx': index number of constant column.
//    e.g: Given tgtf as [ax1, bx2, cx3, 100], then rhs_idx is 3.
//
//NOTICE:
//    The columns size of 'sol', 'tgtf', 'vc', 'eq', 'leq' must be same.
template <class Mat, class T>
UINT MIP<Mat, T>::minm(OUT T & minv,
                       OUT Mat & sol,
                       Mat const& tgtf,
                       Mat const& vc,
                       Mat const& eq,
                       Mat const& leq,
                       bool is_bin,
                       IN BMat * rational_indicator,
                       INT rhs_idx)
{
    ASSERTN(m_is_init, ("not yet initialize"));
    m_allow_rational_indicator = rational_indicator;
    checkAndInitConst(leq, eq, tgtf, vc, rhs_idx);
    m_cur_best_sol.deleteAllElem();
    m_cur_best_v = 0;
    m_times = 0;
    m_indent = 0;
    INTMat fork_count(1, tgtf.getColSize());
    return RecusivePart(minv, sol, tgtf, vc, eq, leq,
                        rhs_idx, false, is_bin, fork_count);
}

} //namespace xcom
#endif
