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
#include "ltype.h"
#include "comf.h"
#include "strbuf.h"
#include "smempool.h"
#include "rational.h"
#include "flty.h"
#include "sstl.h"
#include "matt.h"
#include "bs.h"
#include "sbs.h"
#include "sgraph.h"
#include "xmat.h"
#include "linsys.h"
#include "lpsol.h"

namespace xcom {

//Map from  unknowns of system of inequality to their max/min vaule.
typedef Vector<Vector<INT>*> VECOFVECINT; //int vec of vec
class X2V_MAP {
    VECOFVECINT m_x2pos; //record positive coeff of variable,
                        //index of the vector indicate the index of eqaution.
    VECOFVECINT m_x2neg; //record negative coeff of variable,
                        //index of the vector indicate the index of eqaution.
    bool m_is_init;
    SMemPool * m_pool;

    void * xmalloc(INT size)
    {
        ASSERTN(m_is_init == true, ("List not yet initialized."));
        void * p = smpoolMalloc(size, m_pool);
        if (!p) return NULL;
        ::memset(p,0,size);
        return p;
    }

public:
    X2V_MAP()
    {
        m_is_init = false;
        init();
    }

    ~X2V_MAP()
    {
        destroy();
    }

    void init()
    {
        if (m_is_init) return;
        m_x2pos.init();
        m_x2neg.init();
        m_pool = smpoolCreate(64, MEM_COMM);
        m_is_init = true;
    }

    void destroy()
    {
        if (!m_is_init) return;
        INT i;
        for (i = 0; i <= m_x2pos.get_last_idx(); i++) {
            Vector<INT> *vec = m_x2pos.get(i);
            if (vec) {
                vec->destroy();
            }
        }
        for (i = 0; i <= m_x2neg.get_last_idx(); i++) {
            Vector<INT> * vec = m_x2neg.get(i);
            if (vec) {
                vec->destroy();
            }
        }
        m_x2pos.destroy();
        m_x2neg.destroy();
        smpoolDelete(m_pool);
        m_is_init = false;
    }

    //Map the equation/inequality to the variable that the
    //equation only has single variable, and the coefficient of variable if 'coeff'.
    void map(UINT idx_of_var, UINT idx_of_equation, Rational coeff)
    {
        if (coeff > 0) {
            //Get equatino index vec.
            Vector<INT> * vec = m_x2pos.get(idx_of_var);
            if (vec == NULL) {
                vec = (Vector<INT>*)xmalloc(sizeof(Vector<INT>));
                vec->init();
                m_x2pos.set(idx_of_var, vec);
            }
            vec->set(vec->get_last_idx() + 1, idx_of_equation);
        } else if (coeff < 0) {
            Vector<INT> * vec = m_x2neg.get(idx_of_var);
            if (vec == NULL) {
                vec = (Vector<INT>*)xmalloc(sizeof(Vector<INT>));
                vec->init();
                m_x2neg.set(idx_of_var, vec);
            }
            vec->set(vec->get_last_idx() + 1, idx_of_equation);
        }
    }

    Vector<INT> * get_pos_of_var(UINT var)
    {
        ASSERTN(m_is_init == true, ("not yet initialized."));
        return m_x2pos.get(var);
    }

    Vector<INT> * get_neg_of_var(UINT var)
    {
        ASSERTN(m_is_init == true, ("not yet initialized."));
        return m_x2neg.get(var);
    }
};



///
///START Lineq
///
Lineq::Lineq(RMat * m, INT rhs_idx)
{
    m_is_init = false;
    m_rhs_idx = -1;
    m_coeff = NULL;
    init(m, rhs_idx);
}


Lineq::~Lineq()
{
      destroy();
}


void Lineq::init(RMat * m, INT rhs_idx)
{
    if (m_is_init) return;
    if (m != NULL) {
        setParam(m, rhs_idx);
    } else {
        m_coeff = NULL;
        m_rhs_idx = -1;
    }
    m_is_init = true;
}


void Lineq::destroy()
{
    if (!m_is_init) return;
    m_coeff = NULL;
    m_rhs_idx = -1;
    m_is_init = false;
}


//Set coeff matrix and index of start column of constant term.
void Lineq::setParam(RMat * m, INT rhs_idx)
{
    ASSERTN(m != NULL && m->getColSize() > 0, ("coeff mat is empty"));
    m_coeff = m;
    if (rhs_idx == -1) {
        m_rhs_idx = m->getColSize() -1;
        return;
    }
    ASSERTN(rhs_idx < (INT)m->getColSize() && rhs_idx >= 1,
            ("out of bound"));
    m_rhs_idx = rhs_idx;
}


//Comparing constant term of inequality and a constant value.
//
//'m': system of inequalities
//'idx_of_eq': index of inequality
INT Lineq::compareConstIterm(
        RMat const& m,
        UINT rhs_idx,
        INT idx_of_eq,
        Rational v)
{
    ASSERTN(m_is_init == true, ("not yet initialize."));
    ASSERTN(rhs_idx < m.getColSize(), ("illegal param"));
    bool has_cs1 = false; //has symbolic constant of eqt1
    for (UINT i = rhs_idx + 1; i < m.getColSize(); i++) {
        if (m.get(idx_of_eq, i) != 0) {
            has_cs1 = true;
            break;
        }
    }
    if (!has_cs1) {
        Rational c1 = m.get(idx_of_eq, rhs_idx);
        if (c1 == v) {
            return CST_EQ;
        } else if (c1 < v) {
            return CST_LT;
        } else {
            return CST_GT;
        }
    }

    return CST_UNK;
}


//Comparing constant term of ineqt1 and ineqt2.
INT Lineq::compareConstIterm(RMat const& m, UINT rhs_idx,
                           INT idx_of_eqt1, INT idx_of_eqt2)
{
    ASSERTN(m_is_init == true, ("not yet initialize."));
    ASSERTN(rhs_idx < m.getColSize(), ("illegal param"));
    bool has_cs1 = false; //has symbolic constant of eqt1
    bool has_cs2 = false; //has symbolic constant of eqt2
    bool has_same_cs = true; //eq1 and eqt2 have same symbolic constant

    //Check constant symbols
    for (UINT i = rhs_idx + 1; i < m.getColSize(); i++) {
        if (m.get(idx_of_eqt1, i) != 0) {
            has_cs1 = true;
        }
        if (m.get(idx_of_eqt2, i) != 0) {
            has_cs2 = true;
        }
        if (m.get(idx_of_eqt1, i) != m.get(idx_of_eqt2, i)) {
            //Symbol's coeff of two inequalities are not the same.
            has_same_cs = false;
            break;
        }
    }

    //no symbolic constant or same sym-constants
    if ((!has_cs1 && !has_cs2) || has_same_cs) {
        Rational c1 = m.get(idx_of_eqt1, rhs_idx);
        Rational c2 = m.get(idx_of_eqt2, rhs_idx);
        if (c1 == c2) {
            return CST_EQ;
        } else if (c1 < c2) {
            return CST_LT;
        } else {
            return CST_GT;
        }
    }

    return CST_UNK;
}


//Unify a list of convex hulls into a single convex hull, or
//the intersection of these hulls.
//e.g: Given two 1-dimension polytopes: 10 <= x <=100, 20 <= y <= 200
//    Shape of resulting polyhedron by unifying is 10 <= z <= 200.
//
//'chlst': list of convex hulls which must be with the same dimension.
void Lineq::ConvexHullUnionAndIntersect(
        OUT RMat & res,
        IN List<RMat*> & chlst,
        UINT rhs_idx,
        bool is_intersect)
{
    if (chlst.get_elem_count() == 0) {
        res.deleteAllElem();
        return;
    }
    RMat * p = chlst.get_head();
    UINT first_cols = p->getColSize();
    res.reinit(1, first_cols);
    //output

    //Allocating buf to hold output-boundary${SRCPATH}
    RMat * v = new RMat[rhs_idx];
    List<RMat*> bd; //record boundary to each iv.
    for (UINT i = 0; i < rhs_idx; i++) {
        bd.append_tail(&v[i]);
    }

    //Scanning each convex hull to compute boundary.
    Lineq lin(NULL);
    for (p = chlst.get_head(); p != NULL; p = chlst.get_next()) {
        ASSERTN(first_cols == p->getColSize(),
                ("matrix is not in homotype"));
        //FM-elim
        lin.setParam(p, rhs_idx);
        if (!lin.calcBound(bd)) { //elem of 'bd' would be modified.
            ASSERTN(0, ("inequalities in 'bd' are inconsistent!"));
        }
        UINT j = 0;
        for (RMat * t = bd.get_head(); t; t = bd.get_next(), j++) {
            if (t->size() != 0) {
                res.growRow(*t, 0, t->getRowSize() - 1);
            }
        }
    }
    delete [] v;

    //ONLY for verification.
    lin.setParam(&res, rhs_idx);

    //Union or intersect these hulls.
    if (!lin.reduce(res, rhs_idx, is_intersect)) {
        if (is_intersect) {
            //Convex hull is empty.
            res.deleteAllElem();
        } else {
            ASSERTN(0, ("union operation is illegal!"));
        }
    }
}


//Reducing for tightest or most-relaxed bound of each variable,
//and check for consistency. Return true if system is consistent,
//otherwise return false.
//
//'m': system of inequalities, and will be rewritten with new system.
//'rhs_idx': index of column to indicate the first constant column
//'is_intersect': if it is set to true, the reduction will perform
//    intersection of bound of variable. Otherwise perform
//    the union operation.
//    e.g: Given: x < 100 and x < 200
//         Result of intersection is x < 100, whereas union is x < 200.
//NOTE:
//    Following operations will be performed, and
//    assume 'is_intersect' is true:
//    1. Check simple bounds for inconsistencies.
//        e.g: 0 <= 100 or 0 <= 0
//    2. Delete redundant inequalities, keep only the tighter bound.
//        e.g: x <= 10 , x <= 20. the former will be kept.
//    3. Check for inconsistent bound.
//        e.g: x <= 9, x >= 10, there is no solution!
bool Lineq::reduce(IN OUT RMat & m, UINT rhs_idx, bool is_intersect)
{
    ASSERTN(m_is_init == true, ("not yet initialize."));
    ASSERTN(m.size() != 0 && rhs_idx < m.getColSize(), ("illegal param"));
    removeIdenRow(m);

    //Map the index of eqution to corresponding variable.
    //See following code for detailed usage.
    X2V_MAP x2v;

    //Record if some inequalities should be removed.
    Vector<bool> removed;
    bool consistency = true;
    bool someone_removed = false;
    UINT idx_of_var;

    //Walk through inequations to construct the mapping.
    //Perform reduction/relaxtion for inequlities which
    //only involve single variable.
    //e.g: x <= 100, valid
    //     x + y <= 100, invalid
    for (UINT i = 0; i < m.getRowSize(); i++) {
        INT vars = 0;
        INT single_var_idx = -1;
        removed.set(i, false); //initializing 'removed' vector.

        //Walk through columns of variable, except for the constant columns.
        for (UINT j = 0; j < (UINT)rhs_idx; j++) {
            if (m.get(i, j) != 0) {
                vars++;
                single_var_idx = j;
            }
        }

        //Checking for consistency for the inequality
        //that have no any variable.
        //e.g: 0 < 1
        if (vars == 0) {
            //It is inconsistent, such as: 0 < -100, and it indicates
            //there is no solution of original system.
            //Whereas one situation should be note that if there are
            //constant-symbols in inequality, we could not determined the
            //value of constant term.
            //e.g: 0 <= -100 + M + N. Is it consistent? What are the value of
            //M and N?
            INT s = compareConstIterm(m, rhs_idx, i, (Rational)0);
            switch (s) {
            case CST_LT: //0 <= -100
                consistency = false;
                goto FIN;
            case CST_EQ: //0 <= 0
            case CST_GT: //0 <= 1
                someone_removed = true;
                removed.set(i, true);
                break;
            case CST_UNK: //0 <= -100 + M
                //Do NOT remove this constrain.
                //TODO: Check the domain of constant symbol.
                break;
            default: UNREACHABLE();;
            }
        } else if (vars == 1) {
            //Record number of inequality if it only has single variable.
            x2v.map(single_var_idx, i, m.get(i, single_var_idx));
        }
    }

    //Compute the tightest/relaxed bound of inequalities
    //which only involve single variable.
    //Processing positive coefficent relationship, e.g: x <= N, x <= M.
    for (idx_of_var = 0; idx_of_var < (UINT)rhs_idx; idx_of_var++) {
        Vector<INT> * poscoeff_eqt = x2v.get_pos_of_var(idx_of_var);
        if (poscoeff_eqt != NULL) {
            //Reduction in terms of intersection/union operation on boundary.
            for (INT k1 = 0; k1 < poscoeff_eqt->get_last_idx(); k1++) {
                UINT idx_of_ineqt1 = poscoeff_eqt->get(k1);
                if (removed.get(idx_of_ineqt1) == true) {
                    continue;
                }
                Rational coeff = m.get(idx_of_ineqt1, idx_of_var);
                ASSERTN(coeff > (Rational)0, ("unmatch info"));

                //Reduce coeff of variable to 1.
                if (coeff != 1) {
                    m.mulOfRow(idx_of_ineqt1, 1/coeff);
                }

                bool ineq1_removed = false;
                for (INT k2 = k1 + 1;
                     k2 <= poscoeff_eqt->get_last_idx(); k2++) {
                    UINT idx_of_ineqt2 = poscoeff_eqt->get(k2);
                    if (removed.get(idx_of_ineqt2) == true) {
                        continue;
                    }
                    coeff = m.get(idx_of_ineqt2, idx_of_var);
                    ASSERTN(coeff > 0, ("unmatch info"));

                    //Reduce coeff of variable to 1.
                    if (coeff != 1) {
                        m.mulOfRow(idx_of_ineqt2, 1/coeff);
                    }

                    INT cres = compareConstIterm(m, rhs_idx,
                                            idx_of_ineqt1, idx_of_ineqt2);
                    if (is_intersect) {
                        //Find minimal coeff
                        //e.g:1. x <= 100
                        //    2. x <= 200
                        //The second inequlity will be marked REMOVE.
                        if (cres == CST_LT || cres == CST_EQ) {
                            removed.set(idx_of_ineqt2, true);
                            someone_removed = true;
                        } else if (cres == CST_GT) {
                            removed.set(idx_of_ineqt1, true);
                            someone_removed = true;
                            ineq1_removed = true;
                        }
                    } else {
                        //Find maximal coeff
                        //e.g:1. x <= 100
                        //    2. x <= 200
                        //The first inequlity will be marked REMOVE.
                        if (cres == CST_LT || cres == CST_EQ) {
                            removed.set(idx_of_ineqt1, true);
                            someone_removed = true;
                            ineq1_removed = true;
                        } else if (cres == CST_GT) {
                            removed.set(idx_of_ineqt2, true);
                            someone_removed = true;
                        }
                    }
                    if (ineq1_removed) {
                        //Try next ineq represented by 'k1'
                        break;
                    }
                }
            }
        }
        //dumps_svec((void*)&removed, D_BOOL);

        //Compute the tightest/relaxed bound of inequalities
        //which only involved single variable.
        //Processing negitive coefficent relationship, e.g: -x <= W, -x <= V.
        Vector<INT> * negcoeff_eqt = x2v.get_neg_of_var(idx_of_var);
        if (negcoeff_eqt != NULL) {
            for (INT k1 = 0; k1 < negcoeff_eqt->get_last_idx(); k1++) {
                UINT idx_of_eqt1 = negcoeff_eqt->get(k1);
                if (removed.get(idx_of_eqt1) == true) {
                    continue;
                }

                //Reduce coeff to 1
                Rational coeff  = m.get(idx_of_eqt1, idx_of_var);
                ASSERTN(coeff < 0, ("unmatch info"));
                coeff = -coeff;
                if (coeff != 1) {
                    m.mulOfRow(idx_of_eqt1, 1/coeff);
                }
                bool ineq1_removed = false;
                for (INT k2 = k1 + 1; k2 <= negcoeff_eqt->get_last_idx(); k2++) {
                    UINT idx_of_eqt2 = negcoeff_eqt->get(k2);
                    if (removed.get(idx_of_eqt2) == true) {
                        continue;
                    }

                    //Reduce coeff to 1
                    coeff  = m.get(idx_of_eqt2, idx_of_var);
                    ASSERTN(coeff < 0, ("unmatch info"));
                    coeff = -coeff;
                    if (coeff != 1) {
                        m.mulOfRow(idx_of_eqt2, 1/coeff);
                    }

                    if (is_intersect) {
                        //Find maximum coeff.
                        //We also compare the minimal value, because
                        //that we represent 'x >= a' as '-x <= -a'
                        //e.g:1. x >= 100
                        //    2. x >= 200
                        //First inequlity marked REMOVE.
                        INT cres = compareConstIterm(m, rhs_idx,
                            idx_of_eqt1, idx_of_eqt2);
                        if (cres == CST_LT || cres == CST_EQ) {
                            removed.set(idx_of_eqt2, true);
                            someone_removed = true;
                        } else if (cres == CST_GT) {
                            removed.set(idx_of_eqt1, true);
                            someone_removed = true;
                            ineq1_removed = true;
                        }
                    } else {
                        //Find minimum coeff.
                        //e.g:1. x >= 100
                        //    2. x >= 200
                        //Second inequlity was marked REMOVE.
                        INT cres = compareConstIterm(m, rhs_idx,
                            idx_of_eqt1, idx_of_eqt2);
                        if (cres == CST_LT || cres == CST_EQ) {
                            removed.set(idx_of_eqt1, true);
                            someone_removed = true;
                            ineq1_removed = true;
                        } else if (cres == CST_GT) {
                            removed.set(idx_of_eqt2, true);
                            someone_removed = true;
                        }
                    }
                    if (ineq1_removed) {
                        break;
                    }
                }
            }
        }
        //dumps_svec((void*)&removed, D_BOOL);

        //Verification for legitimate intersection of lower and upper boundary.
        //e.g: x <= 9 , x >= 10 is inconsistency.
        if (is_intersect && poscoeff_eqt != NULL && negcoeff_eqt != NULL) {
            for (INT i = 0; i <= poscoeff_eqt->get_last_idx(); i++) {
                INT pi = poscoeff_eqt->get(i);
                Rational coeff = m.get(pi, idx_of_var);
                if (coeff != 1) { //Reduce coefficent of variable to 1.
                    m.mulOfRow(pi, 1/coeff);
                }
                for (INT j = 0; j <= negcoeff_eqt->get_last_idx(); j++) {
                    INT ni = negcoeff_eqt->get(j);
                    coeff = m.get(ni, idx_of_var);
                    coeff = -coeff;
                    if (coeff != 1) {
                        m.mulOfRow(ni, -1/coeff);
                    } else {
                        m.mulOfRow(ni, -1);
                    }
                    INT cres = compareConstIterm(m, rhs_idx, pi, ni);
                    m.mulOfRow(ni, -1);
                    if (cres == CST_LT) {
                        //Low bound is larger than upper bound!
                        consistency = false;
                        goto FIN;
                    }
                }
            }
        }
    } //end for each variable

    //Remove redundant inequalities.
    if (someone_removed) {
        UINT i, count = 0;
        for (i = 0; i < m.getRowSize(); i++) {
            if (!removed.get(i)) {
                count++;
            }
        }
        RMat tmpres(count, m.getColSize());
        count = 0;
        for (i = 0; i < m.getRowSize(); i++) {
            if (!removed.get(i)) {
                tmpres.setRows(count, count, m, i);
                count++;
            }
        }
        m = tmpres;
    }

FIN:
    return consistency;
}


//Fourier-Motzkin elimination, inequlities form as: Ax <= c
//Return false if there is inconsistency in ineqlities.
//The last column is constant vector.
//
//'u': index of variable, index start from '0'.
//'res': new generated system of ineqalities without variable 'u'.
//'darkshadow': if set to true, tigthening the boundary.
//
//NOTICE:
//1. 'this' uses row convention,  and each col indicate one variable.
//    If variable 'u' is eliminated, all elements of column
//    'u' of 'this' are zero.
//2. Implementation
//    To eliminate variable 'u', each inequality should be
//    normalized in coefficient, and generate new inequality
//    from each pair (upper and lower of variable 'u').
//    e.g:  given system of inequlity,
//        -3x-4y <= -16
//        4x-7y<=20
//        4x+7y<=56
//        -2x+3y<=9
//    after normalizing,
//        1.-x-4/3y <= -16/3
//        2.x-7/4y<=5
//        3.x+7/4y<=14
//        4.-x+3/2y<=9/2
//    generate new inequality from pair (1,2), (1,3), (4,2), (4,3).
bool Lineq::fme(UINT const u, OUT RMat & res, bool const darkshadow)
{
    ASSERTN(m_is_init == true, ("not yet initialize."));
    ASSERTN(m_coeff != NULL && m_coeff != &res, ("illegal parameter of fme"));
    ASSERTN(m_rhs_idx != -1, ("not yet initialize."));
    if (m_coeff->size() == 0) {
        res.deleteAllElem();
        return true;
    }

    //Record the number of inequalities which
    //coefficient of variable 'u' is positive.
    //NOTE: memory will be freed at end of function.
    UINT * pos = (UINT*)::malloc(sizeof(UINT) * m_coeff->getRowSize());
    UINT poscount = 0;

    //Record the number of inequalities which
    //coefficient of variable 'u' is negative.
    //NOTE: memory will be freed at end of function.
    UINT * negc = (UINT*)::malloc(sizeof(UINT) * m_coeff->getRowSize());
    UINT negcount = 0;
    if (m_rhs_idx == -1) {
        m_rhs_idx = m_coeff->getColSize() -1;
    }
    ASSERTN(u < (UINT)m_rhs_idx, ("not a variable"));
    RMat tmp = *m_coeff;
    res.reinit(0, 0);
    bool consistency = true;

    //Perform two of primary operations at first.
    for (UINT i = 0; i < m_coeff->getRowSize(); i++) {
        //1.Check simple bounds for consistencies.
        //e.g: 0 <= -100
        bool have_vars = false;
        for (UINT j = 0; j < (UINT)m_rhs_idx; j++) {
            if (m_coeff->get(i, j) != 0) {
                have_vars = true;
                break;
            }
        }
        if (!have_vars &&
            compareConstIterm(*m_coeff, m_rhs_idx, i, (Rational)0) == CST_LT) {
            //CASE: 0 <= f(x), then f(x) can not less than zero.
            consistency = false;
            goto FIN;
        }

        //1.Record index of inequalities which coefficient
        //of variable 'u' is nonzero for following steps.
        //Positive coefficient indicates that the
        //inequality represeting u < f(x), negtive
        //coefficient indicates that the inequality
        //represeting -u < f(x).
        Rational coeff = m_coeff->get(i, u);
        if (coeff != 0) {
            if (coeff > 0) {
                pos[poscount] = i;
                poscount++;
                if (coeff != 1) {
                    tmp.mulOfRow(i, 1/coeff);
                }
            } else {
                negc[negcount] = i;
                negcount++;
                if (coeff != -1) {
                    tmp.mulOfRow(i, 1/(-coeff));
                }
                if (darkshadow) {
                    //-u < f(x) => -u < f(x) - 1
                    tmp.set(i, m_rhs_idx, tmp.get(i, m_rhs_idx) - 1);
                }
            }
        } else {
            //ith inequlity does not have variable 'u',
            //append to 'res' directly.
            RMat ineqt;
            tmp.innerRow(ineqt, i, i);
            res.growRow(ineqt);
        }
    } //end for each row

    //Generate new inequality to eliminate variable 'u'.
    //There may be some redundant equations.
    if (poscount + negcount == 1) {
        //Only one ineqt about of 'u' that could not be eliminated.
        UINT pi;
        if (poscount == 1) {
            pi = pos[0];
        } else {
            pi = negc[0];
        }
        RMat row;
        tmp.innerRow(row, pi, pi);
        res.growRow(row);
    } else if (poscount + negcount > 1) {
        //More than 1 ineqt about of 'u'
        UINT rowstart;
        if (res.size() == 0) {
            res.reinit(poscount * negcount, tmp.getColSize());
            rowstart = 0;
        } else {
            rowstart = res.getRowSize();
            res.growRow(poscount * negcount);
        }
        for (UINT i = 0; i < poscount; i++) {
            UINT pi = pos[i]; //index of positive coeff
            for (UINT j = 0; j < negcount; j++) {
                UINT ni = negc[j]; //index of negative coeff
                res.setRows(rowstart, rowstart, tmp, ni);
                res.addRowToRow(tmp, pi, rowstart);
                rowstart++;
            }
        }
    }

    if (res.getRowSize() > 0) {
        consistency = reduce(res, m_rhs_idx, true);
    }

FIN:
    ::free(pos);
    ::free(negc);
    return consistency;
}


//Return true if there are no contradictory
//constrains of the system of inequlities.
bool Lineq::is_consistent()
{
    ASSERTN(m_is_init == true, ("not yet initialize."));
    ASSERTN(m_coeff && m_rhs_idx != -1, ("not yet initialize."));
    bool consistency = true;;
    //Reduce variables one by one.
    RMat * orig_coeff = m_coeff;
    INT orig_rhs_idx = m_rhs_idx;
    RMat coeff = *orig_coeff;
    setParam(&coeff, m_rhs_idx);
    for (UINT i = 0; i < (UINT)m_rhs_idx; i++) {
        RMat res;
        if (!fme(i, res)) {
            consistency = false;
            goto FIN;
        }
        coeff = res;
    }
FIN:
    setParam(orig_coeff, orig_rhs_idx);
    return consistency;
}


void Lineq::initVarConstraint(
        Vector<INT> const* sign,
        IN OUT RMat & vc,
        UINT rhs_idx)
{
    UINT nvar = rhs_idx;
    vc.reinit(nvar, nvar + 1);
    vc.setCol(rhs_idx, Rational(0));
    for (UINT i = 0; i < nvar; i++) {
        if ((sign != NULL && sign->get(i) >= 0) ||
            (sign == NULL)) {
            vc.set(i, i, -1);
        } else {
            //variable is unbound.
        }
    }
}


//Return true if there is at least one rational/integer
//solution of the system of inequlities.
//Call setParam() to set coefficient and rhs_idx.
//
//'coeff': coefficient matrix to constrains.
//'vc': variable constrains.
//'is_int_sol': true if the solution must be integral.
//'is_unique_sol': true if there is unique solution. */
bool Lineq::has_solution(RMat const& leq,
                         RMat const& eq,
                         IN OUT RMat & vc,
                         UINT rhs_idx,
                         bool is_int_sol,
                         bool is_unique_sol)
{
    //TODO: Use Farkas Lemma: Ax≒b has solution <=> find y,
    //satisfied y*b≡0, y≡0, A而*y而=0.
    //RMat ns;
    //coeff = eq+leq;
    //coeff.dumpf();
    //coeff.nullspace(ns); //solving A而*y而=0 via computing
                           //Null Space of 'coeff'.
    //ns.dumpf();
    if (leq.size() == 0 && eq.size() == 0) {
        return false;
    }
    ASSERT0(eq.size() == 0 || leq.size() == 0 ||
            leq.getColSize() == eq.getColSize());

    //Prepare data for SIX/MIP solver.
    INT num_of_var = rhs_idx;
    RMat tgtf(1, leq.getColSize());
    for (INT i = 0; i < num_of_var; i++) {
        tgtf.set(0, i, 1);
    }
    ASSERT0(vc.getRowSize() == (UINT)num_of_var &&
            vc.getColSize() == (UINT)num_of_var + 1/*CSt*/);
    RMat res;
    if (is_int_sol) {
        MIP<RMat, Rational> mip;
        mip.reviseTargetFunc(tgtf, eq, leq, num_of_var);
        Rational v;
        UINT st;
        if ((st = mip.maxm(v, res, tgtf, vc, eq, leq,
                           false, NULL, rhs_idx)) == IP_SUCC) {
            //printf("maxv is %d/%d\n", v.num(), v.den());
            //printf("solution is:\n"); res.dumpf();
            return true;
        }
        if (!is_unique_sol && st == IP_UNBOUND) {
            return true;
        }
        if ((st = mip.minm(v, res, tgtf, vc, eq, leq,
                           false, NULL, rhs_idx)) == IP_SUCC) {
            //printf("minv is %d/%d\n", v.num(), v.den());
            //printf("solution is:\n"); res.dumpf();
            return true;
        }
        if (!is_unique_sol && st == IP_UNBOUND) {
            return true;
        }
        return false;
    } else {
        SIX<RMat, Rational> six;
        six.reviseTargetFunc(tgtf, eq, leq, num_of_var);
        Rational v;
        UINT st;
        if ((st = six.maxm(v, res, tgtf, vc, eq, leq, rhs_idx)) == SIX_SUCC) {
            //printf("maxv is %d/%d\n", v.num(), v.den());
            //printf("solution is:\n"); res.dumpf();
            return true;
        }
        if (!is_unique_sol && st == SIX_UNBOUND) {
            return true;
        }
        if ((st=six.minm(v, res, tgtf, vc, eq, leq, rhs_idx)) == SIX_SUCC) {
            //printf("minv is %d/%d\n", v.num(), v.den());
            //printf("solution is:\n"); res.dumpf();
            return true;
        }
        if (!is_unique_sol && st == SIX_UNBOUND) {
            return true;
        }
        return false;
    }
}


//Appends equations into system.
//e.g:
//    Given a system of inequalities, e.g:
//        -1*i + 1*j  +0*k <= 1
//        2*i +0*j -4*k <= -3
//    and the input equation is
//        -1*i + 2*j - k = 10
//    then the new inequalites would be
//        -1*i + 1*j  +0*k <= 1
//        2*i +0*j -4*k <= -3
//        -1*i + 2*j - k <= 10
//        1*i - 2*j + k <= -10
//'eq': the equations to be appended.
void Lineq::appendEquation(RMat const& eq)
{
    ASSERTN(m_is_init == true, ("not yet initialize."));
    if (eq.size() == 0) return;
    ASSERTN(eq.getColSize() == m_coeff->getColSize(), ("unmatch"));
    if (eq.getRowSize() == 0) return;
    RMat tmp = eq;
    //if (tmp.getColSize() < m_coeff->getColSize()) {
    //    tmp.growCol(m_coeff->getColSize() - eq.getColSize());
    //}
    m_coeff->growRow(tmp, 0, tmp.getRowSize() - 1);
    tmp.mul(-1);
    m_coeff->growRow(tmp, 0, tmp.getRowSize() - 1);
}


//Format representation of limits of variable 'u'.
//The result form is
//    'ak*xk <= const + F(c) + a0x0 + a1x1 + ... + a(k-1)x(k-1)
//             + a(k+1)x(k+1) + ... + anxn',
//    where F(c) expresses the function of symbol.
//e.g: Given inequality: x + y <= 100 + F(c), output is
//    x <= 100  + F(c) + (-y)
//
//'u': index of variable that getting start with zero.
//'ineqt_of_u': bound of variable.
void Lineq::formatBound(UINT u, OUT RMat & ineqt_of_u)
{
    ASSERTN(m_is_init == true, ("not yet initialize."));
    ASSERTN(m_coeff &&
           m_coeff->getRowSize() > 0 &&
           m_coeff->getColSize() > 0, ("matrix is empty"));
    ASSERTN((INT)u < m_rhs_idx, ("not a variable"));

    ineqt_of_u.reinit(0,0);

    //Get all of inequalities which has variable 'u'.
    for (UINT i = 0; i < m_coeff->getRowSize(); i++) {
        if (m_coeff->get(i, u) != 0) {
            RMat m;
            m_coeff->innerRow(m, i, i);
            ineqt_of_u.growRow(m);
        }
    }

    if (ineqt_of_u.getRowSize() == 0) {
        //No constrains for u.
        return;
    }

    //Reduce coeff of 'u' to 1 or -1, and shift each other
    //variables from left of inequality to right.
    if (m_rhs_idx != 1) { //Only one variable, column 0 indicate the variable.
        UINT pos = ineqt_of_u.getColSize();
        ineqt_of_u.growCol(m_rhs_idx - 1);
        for (UINT i = 0; i < ineqt_of_u.getRowSize(); i++) {

            //Init element to 0
            for (UINT i2 = pos; i2 < ineqt_of_u.getColSize(); i2++) {
                ineqt_of_u.setr(i, i2, 0, 1);
            }

            //Reduce coeff of 'u' to 1
            Rational coeff = ineqt_of_u.get(i, u);
            if (coeff < 0) {
                coeff = -coeff;
            }
            if (coeff != 1) {
                ineqt_of_u.mulOfRow(i, 1/coeff);
            }

            //Shift other variables from left to righ of '<='.
            UINT k = pos;
            for (UINT j = 0; j < (UINT)m_rhs_idx; j++) {
                if (j != u) {
                    ineqt_of_u.set(i, k, -ineqt_of_u.get(i, j));
                    k++;
                }
            }//end for
        }//end for
        if ((INT)u != (m_rhs_idx - 1)) {
            ineqt_of_u.deleteCol(u + 1, m_rhs_idx - 1);
        }
        if (u > 0) { //u is not first variable
            ineqt_of_u.deleteCol(0, u - 1);
        }
    } else {
        for (UINT i = 0; i < ineqt_of_u.getRowSize(); i++) {
            Rational coeff = ineqt_of_u.get(i, u);
            if (coeff < 0) {
                coeff = -coeff;
            }
            if (coeff != 1) {
                ineqt_of_u.mulOfRow(i, 1/coeff);
            }
        }
    }

    //For conveniency of code genereation, perform reduction to a common low
    //denominator.
    //e.g: x1 <= 3/25 * y1 + 2/3 * y2.
    //produce:
    //        x1 <= 9/75 * y1 + 50/75 * y2.
    //So we can generate code as
    //        for (... x1 <= floor((9*y1+50*y2)/75) ...)
    for (UINT i = 0; i < ineqt_of_u.getRowSize(); i++) {
        ineqt_of_u.comden(i, 1); //start from 2nd column.
    }
}


//Compute each variable's boundary by system of inequlities.
//Return true if all of variables boundary are available, otherwise return false.
//    e.g: Given inequalities:
//            1 <= i1 <= 4
//            5-i1 <= i2 <= 12-i1
//        the boundaries of i1, i2 are
//            1 <= i1 <= 4
//            1 <= i2 <= 11.
//
//'limits':  a list records the new bound for each of variables.
//    The head element refers to the first(outermost) variable.
//
//NOTICE:
//    Column describes the variable.
bool Lineq::calcBound(IN OUT List<RMat*> & limits)
{
    ASSERTN(m_is_init == true, ("not yet initialized"));
    ASSERTN(m_coeff != NULL && limits.get_elem_count() == (UINT)m_rhs_idx,
           ("unmatch coeff matrix info"));

    //Eliminating variable one by one, and inner to outer.
    INT i,j;
    RMat res;
    RMat work; //Recovery for next elimination.
    Lineq lineq(NULL);
    for (j = 0; j < m_rhs_idx; j++) {
        work = *m_coeff; //Recovery for next elimination.
        lineq.setParam(&work, m_rhs_idx);
        for (i = m_rhs_idx - 1; i >= 0; i--) {
            if (i == j) {
                continue;
            }
            if (!lineq.fme(i, res, false)) {
                ASSERTN(0, ("system inconsistency!"));
                return false;
            }
            work = res; //very important! Update information of Lineq system.
        }

        //Computing the remaining alone variable.
        RMat * newb = limits.get_head_nth(j);
        ASSERTN(newb, ("illegal!!"));
        *newb = work;
    }
    return true;
}


//Move variable to righ-hand-side of inequality.
//e.g: Move j to RHS:
//        -2*i + j + 3*k <= 10 will be:
//        -2*i + 3*k <= 10 - j
//    and move k to RHS:
//        -2*i + 3*k <= 10 - j ->
//        -2*i <= 10 - j - 3*k
//    Similarly, one can move multiple variables at a time.
//
//'ieq':     equalities/inequalities to be transformed.
//'rhs_idx': rhs_idx of ieq.
//'first_var': the first variable index to be moved.
//'last_var': the last variable index to be moved.
//'first_sym_idx': the index of the first symbol at RHS.
//'last_sym_idx': the index of the last symbol at RHS. */
void Lineq::move2cstsym(IN OUT RMat & ieq,
                        UINT rhs_idx,
                        UINT first_var,
                        UINT last_var,
                        OUT UINT * first_sym_idx,
                        OUT UINT * last_sym_idx)
{
    ASSERTN(m_is_init == true, ("not yet initialized"));
    ASSERTN(rhs_idx < ieq.getColSize() && last_var < rhs_idx &&
            first_var <= last_var,
            ("illegal info"));
    RMat colm;
    ieq.innerColumn(colm, first_var, last_var);
    colm.mul(-1);
    ieq.growCol(colm, 0, colm.getColSize() - 1);
    ieq.deleteCol(first_var, last_var);
    if (first_sym_idx) {
        *first_sym_idx = ieq.getColSize() - rhs_idx - 1;
    }
    if (last_sym_idx) {
        *last_sym_idx = (ieq.getColSize() - rhs_idx - 1) +
                        (last_var - first_var);
    }
}


//Substitute variable 'sub_var' with linear polynomials.
//e.g: Given -2i+N<=0, substitute i with 4j-3N-1 (i=4j-3N-1), we get:
//    -2(4j-3N-1)+N, and simplied to -8j+7N+2<=0.
//
//'p': each row indicates polynomial
//'sub_var': index of variable to substitute.
void Lineq::substituteAndExpand(
        IN OUT RMat & coeff,
        UINT rhs_idx,
        RMat const& p,
        UINT sub_var)
{
    DUMMYUSE(rhs_idx);
    ASSERT0(coeff.getColSize() == p.getColSize() && sub_var < rhs_idx);
    RMat tp;
    for (UINT i = 0; i < p.getRowSize(); i++) {
        if (p.get(i, sub_var) == Rational(0)) {
            continue;
        }
        for (UINT j = 0; j < coeff.getRowSize(); j++) {
            Rational v = coeff.get(j, sub_var);
            if (v == Rational(0)) {
                continue;
            }
            p.innerRow(tp, i, i);
            Rational v1 = tp.get(0, sub_var);
            if (v1 != 1) {
                tp.mulOfRow(0, 1/v1);
            }
            tp.mulOfRow(0, v);
            coeff.set(j, sub_var, Rational(0));

            //Convert the sign of element by negtive operation from 'rhs_idx'
            //to last one, beause these elements will be added to RHS of
            //inequalities.
            //e.g: Given i=2j-1+N, convert -1+N to 1-N.
            for (UINT k = m_rhs_idx; k < tp.getColSize(); k++) {
                tp.set(0, k, -tp.get(0, k));
            }
            coeff.addRowToRow(tp, 0, j);
        }
    }
}


//Move constant symbols to left-hand-side(LHS) of inequality.
//It is similar to 'move2rhs'.
//
//'rhs_idx': the start column of constant part of inequality.
//'first_sym': column index of first constant symbol, starting with rhs_idx+1.
//'last_sym': column index of last constant symbol, starting with rhs_idx+1.
//
//NOTICE:
//    The column 'rhs_idx' does not belong to constant symbols.
//    e.g: i < 10 + j + k, the first symbol is j, then 'first_sym' is 0.
void Lineq::move2var(IN OUT RMat & ieq,
                     UINT rhs_idx,
                     UINT first_sym,
                     UINT last_sym,
                     OUT UINT * first_var_idx,
                     OUT UINT * last_var_idx)
{
    ASSERTN(m_is_init == true, ("not yet initialized"));
    ASSERTN(last_sym < ieq.getColSize() &&
            first_sym > rhs_idx &&
            first_sym <= last_sym,
            ("illegal info"));
    RMat colm;
    ieq.innerColumn(colm, first_sym, last_sym);
    ieq.deleteCol(first_sym, last_sym);
    colm.mul(-1);
    ieq.insertColumnsBefore(rhs_idx, colm, 0, colm.getColSize() - 1);
    if (first_var_idx) {
        *first_var_idx = rhs_idx;
    }
    if (last_var_idx) {
        *last_var_idx = rhs_idx + (last_sym - first_sym);
    }
}


//Remove idendtical row.
//e.g:
//    1, 2, 3
//    3, 4, 5
//    1, 2, 3
//    The third row will be removed.
void Lineq::removeIdenRow(IN OUT RMat & m)
{
    Vector<Rational> sum;
    Vector<bool> removed;
    UINT i;
    for (i = 0; i < m.getRowSize(); i++) {
        Rational s = 0;
        for (UINT j = 0; j < m.getColSize(); j++) {
            s = s + m.get(i,j);
        }
        sum.set(i, s);
    }
    for (i = 0; i < m.getRowSize(); i++) {
        if (removed.get(i)) {
            continue;
        }
        for (UINT j = i + 1; j < m.getRowSize(); j++) {
            if (sum.get(i) != sum.get(j)) {
                continue;
            }
            if (m.is_rowequ(i, m, j)) {
                removed.set(j, true);
            }
        }
    }

    RMat tmp;
    INT end_of_row = -1;
    INT start_of_row = -1;
    bool doit = false;
    for (i = 0; i < m.getRowSize(); i++) {
        if (!removed.get(i)) {
            if (start_of_row == -1) {
                start_of_row = i;
                end_of_row = i;
            } else {
                end_of_row++;
            }
            if (i < m.getRowSize() - 1) {
                continue;
            }
        }
        doit = true;
        if (start_of_row == -1) {
            continue;
        }
        ASSERT0(end_of_row >= start_of_row);
        RMat n;
        m.innerRow(n, start_of_row, end_of_row);
        if (tmp.getRowSize() == 0) {
            tmp.copy(n);
        } else {
            tmp.growRow(n, 0, n.getRowSize() - 1);
        }
        start_of_row = -1;
    }
    if (doit) {
        m.copy(tmp);
    }
}


//Dump bound of variable 'u'.
//Forms as
//    'ak*xk <= const + F(x) + a0x0 + a1x1 + ... + a(k-1)x(k-1) +
//                a(k+1)x(k+1) + ... + anxn'
//e.g: Given inequality: x + y <= 100 + F(x), output is
//    x <= 100  + F(x) + (-y)
//
//'u': index of variable that getting start with zero.
void Lineq::dumps_var_bound(UINT u)
{
    RMat ineqt_of_u;
    formatBound(u, ineqt_of_u);
}


//Ehrhart Polynomial.
void Lineq::EhartPoly(OUT RMat & res, IN RMat & a, UINT rhs_idx)
{
    DUMMYUSE(rhs_idx);
    DUMMYUSE(a);
    DUMMYUSE(res);
    UNREACHABLE();
}


INT Lineq::selectLeadingColumn(
        INTMat const& coeff,
        Vector<bool> const& is_noneg,
        UINT rhs_part)
{
    //The selecting criteria include static/dynamic ordering,
    //they are MaxIndex, MinIndex, MinCutoff, MaxCutoff, MixCutoff,
    //LexMin(lexicographic min ordering), LexMax, and RandomRow.
    //
    //In general, LexMin is the best choice which is in fact chosen
    //people always used.
    //If you know that the input is already sorted in the order you
    //like, use MinIndex or MaxIndex. If the input contains many
    //redundant rows (say more than 80% redundant), you might want to
    //try MaxCutoff which might result in much faster termination.
    for (UINT j = rhs_part; j < coeff.getColSize(); j++) {
        if (is_noneg.get(j)) {
            continue;
        }
        for (UINT i = 0; i < coeff.getRowSize(); i++) {
            if (coeff.get(i,j) < 0) {
                return j;
            }
        }
    }
    return -1;
}


//Combine constrains.
//'res': output constraints, and must be initialized by caller.
//'r1, r2': row position of negative coeff
//'lc': leading column
//'pos': row position for current modification of 'res'
//
//      0 0 0 1 0 -1  4  2 2
//      1 0 0 0 3  2 -4 -1 6
//    =>
//      1 0 0 2 3  0  4  3 10
void Lineq::combine(OUT INTMat & res,
                    INTMat const& coeff,
                    UINT r1,
                    UINT r2,
                    UINT lc,
                    UINT pos)
{
    if (r1 == r2) return;
    INT l1 = coeff.get(r1, lc);
    INT l2 = coeff.get(r2, lc);
    if (l1 < 0) l1 = -l1;
    if (l2 < 0) l2 = -l2;
    INT l = slcm(l1, l2);
    INT m1 = 1;
    INT m2 = 1;
    if (l1 != l) m1 = l/l1;
    if (l2 != l) m2 = l/l2;
    for (UINT i = 0; i < coeff.getColSize(); i++) {
        res.set(pos, i, coeff.get(r1, i)*m1 + coeff.get(r2, i)*m2);
    }
}


//For each pair, after examing all the columns of the left hand part and the
//non-negative columns of its right hand side, we check whether the columns
//include those whose intersections with the row of the pair are zeros.
//If there are no such columns, or if there is still at least one row which
//intersects all such columns in zeros, we omit the pair.
//
//'combined': record rows whose coefficient is positive and has been combined.
//'noneg': record nonegative columns.
bool Lineq::omit(INTMat const& coeff,
                 UINT ncv,
                 UINT pcv,
                 UINT rhs_part,
                 Vector<UINT> const& combined,
                 Vector<UINT> const& noneg)
{
    Vector<UINT> sczero; //record idx of single common zero columns.
    UINT sczero_count = 0;

    //Exam left and right sides.
    INT i;
    for (i = 0; i < (INT)rhs_part; i++) {
        if (coeff.get(ncv, i) == 0 && coeff.get(pcv, i) == 0) {
            sczero.set(sczero_count++, i);
        }
    }
    for (i = 0; i <= noneg.get_last_idx(); i++) {
        UINT c = noneg.get(i);
        if (coeff.get(ncv, c) == 0 && coeff.get(pcv, c) == 0) {
            sczero.set(sczero_count++, c);
        }
    }
    if (sczero_count == 0) {
        return true;
    }

    //Check with rules.
    for (INT j = 0; j <= combined.get_last_idx(); j++) {
        UINT crow = combined.get(j);
        bool all_zero = true; //intersecting all columns in zeros.
        for (INT k = 0; k <= sczero.get_last_idx(); k++) {
            if (coeff.get(crow, sczero.get(k)) != 0) {
                all_zero = false;
                break;
            }
        }
        if (all_zero) {
            return true;
        }
    }
    return false;
}


//Return true if system has non-negative solutions, otherwise return false.
//Generate all vertices of an H-polyhedron.
//The function use homogeneous representation of affine spaces.
//
//'cs': representation matrix
//'gmat': generating matrix, both ray and vertex in the matrix.
//   e.g: If the res is following matrix,
//       1   0   1   0
//       0   1   1   0
//       1   1   1   0
//       1   1   1   1
//       1   1   2   1
//       2   1   2   1
//       1   1   2   0
//       1   1   2   2
//   then
//       1   0   1   | 0   <- ray [1 0 1]
//       0   1   1   | 0   <- affine ray [0 1 1]
//       1   1   1   | 0   <- ray [1 1 1]
//       1   1   1   | 1   <- vertex [1 1 1]
//       1   1   2   | 1   <- vertex
//       2   1   2   | 1   <- vertex
//       1   1   2   | 0   <- ray
//       1   1   2   | 2   <- vertex [0.5 0.5 1]
//   The last column is affine part.
//   If affine part of each vector is 0, it is a ray(line),
//   e.g:[1 2 0]    in the homogeneous linear space correspond to the infinite
//   direction ray [1 2] in the affine space.
//
//   If affine part of each vector is 1, the vector is vertex in affine-space,
//   e.g: the vector (ray) [1 2 1] in the homogeneous linear space correspond
//   to the vector (vertex) [1 2] in the affine space.
//
//   Otherwise the vector correspond to a vertex in scaled affine space.
//   e.g: the vector (ray) [1 2 2] in the homogeneous linear space correspond
//   to the vector (vertex) [0.5 1] in the affine space.
//
//'raylimit': is the maximum allowed ray.
bool Lineq::convertConstraint2Ray(
        OUT INTMat & gmat,
        INTMat const& cs,
        UINT rhs_idx,
        UINT raylimit)
{
    if (cs.size() == 0) return false;
    INTMat coeff;
    if (!cs.is_colequ(rhs_idx, 0)) { //Inhomogeneous system.
        cs.innerColumn(coeff, 0, rhs_idx);

        //Trans the form as ﹉ai*xi>=0
        coeff.negOfColumns(0, coeff.getColSize() - 2);
    } else {
        cs.innerColumn(coeff, 0, rhs_idx - 1);
        coeff.neg();
    }

    //coeff.reinit(4,3);
    //coeff.sete(4*3,
    //        0, 1, -1,
    //        -1, 0, 6,
    //        0, -1, 7,
    //        1, 0, -2);
    //
    //
    //coeff.sete(4*3,
    //        1, 0, -1,
    //        -1, 0, 3,
    //        0, -1, 5,
    //        0, 1, -2);

    coeff.dumpf();
    coeff.trans();
    coeff.insertColumnsBefore(0, coeff.getRowSize());
    UINT rhs_part = coeff.getRowSize();
    for (UINT i = 0; i < coeff.getRowSize(); i++) {
        coeff.set(i, i, 1);
    }
    Vector<UINT> row_of_pos_coeff, row_of_neg_coeff;
    Vector<bool> rm; //record idx of rows which marked removable.
    Vector<UINT> noneg; //record idx of columns which are non-negative column.
    Vector<bool> is_noneg; //record idx of columns which
                            //are non-negative column.
    Vector<UINT> combined; //record idx of rows with positive-coeff
                            //which has been combined.
    UINT combined_count = 0;
    UINT noneg_count = 0;
    INTMat res, tmp;
    bool first = true;
    int xx = 0;
    for (;;) {
        coeff.dumpf();
        INT lc = selectLeadingColumn(coeff, is_noneg, rhs_part);

        //FOR TEST, should be removed.
        //if (xx == 0) lc=5;
        //else if (xx == 1) lc=7;
        //else if (xx == 3) lc=8;
        //if (xx == 3) {
        //    int a = 0;
        //}

        xx++;

        //TEST

        if (lc < 0) {
            break;
        }
        INT rem_rows = (INT)coeff.getRowSize();
        rm.clean();
        row_of_pos_coeff.clean();
        row_of_neg_coeff.clean();
        INT m = 0, n = 0;
        UINT i = 0;
        for (i = 0; i < coeff.getRowSize(); i++) {
            if (coeff.get(i, lc) < 0) {
                row_of_neg_coeff[m++] = i;
            } else if (coeff.get(i, lc) > 0) {
                row_of_pos_coeff[n++] = i;
            }
        }
        if (n == 0) {
            //In particular, if the right hand side contains a column, all the
            //elements of which are negative, we say the given system has
            //no-zero non-negative solutions.
            return false;
        }
        INT p = n*m;
        if (p > 0) {
            if (p > (INT)res.getRowSize()) {
                res.reinit(p, coeff.getColSize());
            }
        } else { ASSERTN(0, ("Must have negative coeff")); }
        i = 0;
        INT omit_count = 0;
        for (INT nc = 0; nc <= row_of_neg_coeff.get_last_idx(); nc++) {
            //idx of rows with negative-coeff.
            UINT ncv = row_of_neg_coeff[nc];
            combined.clean();
            combined_count = 0;
            for (INT pc = 0; pc <= row_of_pos_coeff.get_last_idx(); pc++) {
                UINT pcv = row_of_pos_coeff[pc];
                if (first || !omit(coeff, ncv, pcv,
                                    rhs_part, combined, noneg)) {
                    combine(res, coeff, ncv, pcv, lc, i++);
                    combined[combined_count++] = pcv;
                } else {
                    omit_count++;
                }
            }
            rm[ncv] = true; //removed rows
            rem_rows--; //remained rows
            res.dumpf(0,0);
        }
        first = false;
        noneg[noneg_count++] = lc;
        is_noneg[lc] = true;

        //Conjuntion of two parts.
        ASSERT0(rem_rows + p - omit_count >= 0);
        tmp.reinit(rem_rows + p - omit_count, coeff.getColSize());
        INT row = 0;
        for (UINT h = 0; h < coeff.getRowSize(); h++) {
            if (!rm[h]) {
                tmp.setRows(row, row, coeff, h);
                row++;
            }
        }
        ASSERT0(row <= row + p - 1 - omit_count);
        tmp.setRows(row, row + p - 1 - omit_count, res, 0);
        coeff.copy(tmp);
        if (coeff.getRowSize() > raylimit) {
            return false;
        }
    } //end while

    bool is_homo = cs.is_colequ(rhs_idx, 0);
    if (!is_homo) {
        //The last variable of extremal vector is the coefficient of
        //constant part. Thus if the value of all the extremal vectors is
        //zero, the original system 'cs' has no non-negative solutions.
        if (coeff.is_colequ(rhs_part - 1, 0)) {
            //The polyhedron is empty.
            return false;
        }
    }
    coeff.innerColumn(gmat, 0, rhs_part - 1);
    gmat.gcd();
    return true;
}


//Generate a new row via combining row 'r1' and 'r2' that
//eliminating column 'lc' to be zero.
//
//'res': output constraints, and must be initialized by caller.
//'r1, r2': row position of negative coeff
//'lc': leading column
//'pos': row position for current modification of 'res'
//
//  0 0 0 1 0 -1  4  2 2
//  1 0 0 0 3  2 -4 -1 6
//=>
//  1 0 0 2 3  0  4  3 10
void Lineq::combineRays(
        OUT INTMat & res,
        IN OUT INTMat & coeff,
        UINT r1,
        UINT r2,
        UINT lc,
        UINT pos)
{
    if (r1 == r2) return;
    INT l1 = coeff.get(r1, lc);
    INT l2 = coeff.get(r2, lc);
    if (l1 < 0) l1 = -l1;
    if (l2 < 0) l2 = -l2;
    INT l = slcm(l1, l2);
    INT m1 = 1;
    INT m2 = 1;
    if (l1 != l) m1 = l/l1;
    if (l2 != l) m2 = l/l2;
    for (UINT i = 0; i < coeff.getColSize(); i++) {
        res.set(pos, i, coeff.get(r1, i)*m1 + coeff.get(r2, i)*m2);
    }
    if (coeff.get(r1, 0) == 1 || coeff.get(r2, 0) == 1) {
        //A combination of a ray and a line is a ray.
        res.set(pos, 0, 1);
    } else {
        //A combination of two line is another line.
        res.set(pos, 0, 0);
    }
}


void Lineq::removeRedRow(
            IN OUT INTMat & cs,
            INTMat const& org_cone,
            UINT rhs_part)
{
    UINT cs_rows = cs.getRowSize();
    UINT cs_cols = rhs_part;
    UINT o_rows = org_cone.getRowSize();
    UINT o_cols = org_cone.getColSize();
    Vector<bool> rm;
    UINT rm_count = 0;
    for (UINT i = 1; i < cs_rows - 1; i++) {
        if (rm.get(i)) {
            continue;
        }
        for (UINT j = i + 1; j < cs_rows; j++) {
            if (rm.get(j)) {
                continue;
            }
            bool find = false;
            for (UINT k = 0; k < o_rows; k++) {
                if ((cs.dot(i, 0, i, cs_cols - 1, org_cone,
                            k, 0, k, o_cols - 1) == 0 &&
                     cs.dot(j, 0, j, cs_cols - 1, org_cone,
                             k, 0, k, o_cols - 1) > 0) ||
                    (cs.dot(i, 0, i, cs_cols - 1, org_cone,
                            k, 0, k, o_cols - 1) > 0 &&
                     cs.dot(j, 0, j, cs_cols - 1, org_cone,
                             k, 0, k, o_cols - 1) == 0)) {
                    //Satisfy theorem 1(redundant set).
                    find = true;
                    break;
                }
            }
            if (!find) {
                rm.set(j, true);
                rm_count ++;
            }
        }
    }
    if (rm.get_last_idx() >= 0) {
        INTMat res(cs_rows - rm_count, cs.getColSize());
        UINT k = 0;
        for (UINT i = 0; i < cs_rows; i++) {
            if (!rm.get(i)) {
                res.setRows(k, k, cs, i);
                k++;
            }
        }
        cs.copy(res);
    }
}


//Generate constraints according to given matrix of rays.
//'gmat': general matrix, and must be formed as: Ax+b>=0
//    e.g: Given matrix
//         1 -1 0 // x-y>=0
//        -1  0 1 //-x+1>=0
//      for system with x,y varible:
//         x-y>=0
//        -x+1>=0
//'cslimit': is the maximum allowed constraints.
bool Lineq::convertRay2Constraint(
        INTMat const& gmat,
        OUT INTMat & cs,
        UINT cslimit)
{
    DUMMYUSE(cslimit);
    if (gmat.size() == 0) {
        cs.deleteAllElem();
        return false;
    }

    INTMat org_cone(gmat);
    UINT rhs_part;
    {
        //Initializing cs=[R|A而]
        UINT const nrow = gmat.getColSize() +
            1; //add a column for bidirectional coordinate 'u'
        UINT const ncol = nrow + gmat.getRowSize() +
            1; //add a row for homogeneous coordinate '竹'
        rhs_part = nrow;
        cs.reinit(nrow, ncol);
        UINT i;
        for (i = 0; i < nrow; i++) {
            cs.set(i, i, 1);
        }
        cs.dumpf();

        //Set homogeneous coordinate.
        cs.set(cs.getRowSize() - 1, cs.getColSize() - 1, 1);
        cs.dumpf();

        //Set bidirectional coordinate.
        for (i = rhs_part; i < ncol; i++) {
            cs.set(0, i, 1);
        }
        cs.dumpf();

        //Set the transpose data of 'gmat'.
        for (i = 0; i < gmat.getRowSize(); i++) {
            for (UINT j = 0; j < gmat.getColSize(); j++) {
                cs.set(1 + j, rhs_part + i, gmat.get(i, j));
            }
        }
        cs.dumpf();

        org_cone.growRow(1);
        org_cone.insertColumnBefore(0);
        org_cone.setCol(0, 1);
        org_cone.set(org_cone.getRowSize() - 1,
                     org_cone.getColSize() - 1, 1);
        org_cone.dumpf();
    }

    {
        //The succession of transformation computes a
        //fundamental set of rays on 'cs'.
        //At each step, a hyperplane or constrain is selected.
        Vector<bool> is_noneg; //record idx of columns
                               //which are non-negative column.
        Vector<UINT> row_of_pos_coeff, row_of_neg_coeff;
        Vector<UINT> combined; //record idx of rows with
                               //positive-coeff which has been combined.
        INTMat res; //hold generated combination rows.
        UINT combined_count = 0;

        for (;;) {
            cs.dumpf();
            INT lc = selectLeadingColumn(cs, is_noneg, rhs_part);
            if (lc < 0) {
                break;
            }

            //Counts positive & negative coeffs.
            row_of_pos_coeff.clean();
            row_of_neg_coeff.clean();
            INT m = 0, n = 0;
            for (UINT i = 0; i < cs.getRowSize(); i++) {
                if (cs.get(i, lc) < 0) {
                    row_of_neg_coeff[m++] = i;
                } else if (cs.get(i, lc) > 0) {
                    row_of_pos_coeff[n++] = i;
                }
            }
            if (n == 0) {
                //In particular, if the right hand side contains
                //a column, all the elements of which are negative,
                //we say the given system has no-zero non-negative
                //solutions.
                return false;
            }
            INT p = n*m;
            if (p > 0) {
                if (p > (INT)res.getRowSize()) {
                    res.reinit(p, cs.getColSize());
                }
            } else {
                ASSERTN(0, ("Must have negative coeff"));
            }

            //Compute the combination of pair of rays.
            UINT i = 0;
            for (INT nc = 0; nc <= row_of_neg_coeff.get_last_idx(); nc++) {
                //idx of rows with negative-coeff.
                UINT ncv = row_of_neg_coeff[nc];
                combined.clean();
                combined_count = 0;
                INT pc;
                for (pc = 0; pc <= row_of_pos_coeff.get_last_idx(); pc++) {
                    //idx of rows with positive-coeff.
                    UINT pcv = row_of_pos_coeff[pc];
                    combineRays(res, cs, ncv, pcv, lc, i++);
                    res.dumpf();
                    cs.dumpf();
                    combined[combined_count++] = pcv;
                }

                //Modify conserved rays.
                if (cs.get(ncv, 0) == 0) {
                    cs.set(ncv, 0, 1);
                }
                for (pc = 0; pc <= row_of_pos_coeff.get_last_idx(); pc++) {
                    //idx of rows with positive-coeff.
                    UINT pcv = row_of_pos_coeff[pc];
                    if (cs.get(pcv, 0) == 0) {
                        cs.set(pcv, 0, 1);
                    }
                }
                for (UINT i2 = 1; i2 < cs.getColSize(); i2++) {
                    cs.set(ncv, i2, -cs.get(ncv, 2));
                }
                //
            }

            //Add something here...
            is_noneg[lc] = true;
            cs.growRow(res, 0, res.getRowSize() - 1);
            cs.dumpf();
            removeRedRow(cs, org_cone, rhs_part);
        }
        cs.deleteCol(rhs_part, cs.getColSize() - 1);
    }
    cs.gcd();
    return true;
}


//Difference operation on polyhedra 'a' and 'b'.
//'res': return the difference of the two polyhedra 'a' and 'b'
//'rhs_idx': constant column index.
//Let P1 is polyhedron of 'a',
//    L1 is lattice of 'a',
//    P2 is polyhedron of 'b',
//    L2 is lattice of 'b',
//    P1' = DomImage(P1, L1)
//    P2' = DomImage(P2, L2)
//    a = L1 ﹎ P1'
//    b = L2 ﹎ P2'
//Then a - b = (L1 ﹎ (P1'-P2')) ﹍ ((L1-L2) ﹎ (P1' ﹎ P2')).
void Lineq::PolyDiff(OUT RMat & res, IN RMat & a, IN RMat & b, UINT rhs_idx)
{
    DUMMYUSE(rhs_idx);
    if (a.size() == 0) { res.deleteAllElem(); return; }
    if (b.size() == 0) { res.copy(a); return; }
    ASSERT0(a.is_homo(b));

}


void Lineq::PolyImage(OUT RMat & res, IN RMat & a, UINT rhs_idx)
{
    DUMMYUSE(res);
    DUMMYUSE(a);
    DUMMYUSE(rhs_idx);
    UNREACHABLE();
}
//END Lineq

} //namespace xcom
