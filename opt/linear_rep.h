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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _LINEAR_REP_H_
#define _LINEAR_REP_H_

namespace xoc {

//The context to infer linear-represetation.
class LRInferCtx {
public:
    //True if the linear-rep is a new generated.
    //It means newe IR expression is allocated, e.g:coeff expression.
    //Propagate info bottom up.
    bool is_constructed;

    //True if the inference of variable will cross DU chain.
    //Propagate info top down.
    bool is_transitive;

    //Record the linear-rep inference scope, it is a LoopInfo.
    //Propagate info top down.
    LI<IRBB> const* li;
public:
    LRInferCtx() { ::memset((void*)this, 0, sizeof(LRInferCtx)); }
};

typedef enum tagAddendSign {
    ADDEND_SIGN_UNDEF = 0,
    ADDEND_SIGN_POS,
    ADDEND_SIGN_NEG,
} AddendSign;

//This class represents linear representation of variable that
//formulated as: a*i+b, where a is coeff, i is variable, b is addend.
class LinearRep {
public:
public:
    IR const* coeff; //record the expression of coefficient
    IR const* addend; //record the expression of addend
    AddendSign addend_sign; //the sign of addend
    IR const* var_exp; //record the expression of variable
public:
    LinearRep() { clean(); }

    void clean() { ::memset((void*)this, 0, sizeof(LinearRep)); }
    void copy(LinearRep const& src) { *this = src; }
    void dump(Region const* rg) const;

    //Return the addend of linear expression.
    IR const* getAddend() const { return addend; }

    //Return the variable of linear expression.
    Var const* getVar() const
    {
        ASSERT0(hasVar() && var_exp->hasIdinfo());
        return var_exp->getIdinfo();
    }

    //Return the variable expression of linear expression.
    //Variable expression is the IR that describes Var, e.g:IR_LD|IR_ST|IR_ID.
    IR const* getVarExp() const { return var_exp; }

    //Return the integer coefficient.
    HOST_INT getIntCoeff() const;

    //Return the float-point coefficient.
    HOST_FP getFPCoeff() const;

    //Return the coefficient IR expression.
    IR const* getCoeff() const { return coeff; }

    //Return true if coefficient is available.
    bool hasCoeff() const { return coeff != nullptr; }

    //Return true if addend is available.
    bool hasAddend() const { return addend != nullptr; }

    //Return true if coefficient is constant.
    bool hasConstCoeff() const
    { return coeff == nullptr ? false : coeff->is_const(); }

    //Return true if coeff is integer immediate.
    bool hasIntCoeff() const
    {
        if (coeff == nullptr) { return false; }
        ASSERT0(coeff->is_const());
        return coeff->is_int();
    }

    //Return true if coeff is float-point immediate.
    bool hasFPCoeff() const;

    //Return true if current object represents linear-expression at least
    //has a variable that formed as: a*var.
    bool hasVar() const { return var_exp != nullptr; }

    //Return true if coeff is integer immediate, and the value is equal to 'v'.
    bool isCoeffEqualTo(HOST_INT v) const;

    //Return true if the sign of addend is available.
    bool isValidAddendSign() const
    {
        return addend_sign == ADDEND_SIGN_POS ||
               addend_sign == ADDEND_SIGN_NEG;
    }

    //Return true if current linear-rep addend's sign is positive.
    bool isPosAddend() const { return addend_sign == ADDEND_SIGN_POS; }

    //Return true if current linear-rep addend's sign is negative.
    bool isNegAddend() const { return addend_sign == ADDEND_SIGN_NEG; }

    //Return true if current linear-rep is inavailable.
    bool isEmpty() const { return !hasVar() && !hasAddend(); }
};


class LinearRepMgr {
    COPY_CONSTRUCTOR(LinearRepMgr);
    TypeMgr * m_tm;
    Region * m_rg;
    IRMgr * m_irmgr;
    OptCtx const& m_oc;
    Refine * m_refine;
    List<IR*> m_gened_ir_list;
protected:
    void add(IR * ir) { m_gened_ir_list.append_tail(ir); }

    IR const* buildConstBinOp(IR_CODE code, IR const* op0, IR const* op1);
    IR const* buildBinOp(IR_CODE code, IR const* c0, IR const* c1);

    void clean();
    IR const* constructCoeffByMul(LinearRep const& lr0,
                                  LinearRep const& lr1,
                                  OUT LRInferCtx & ctx);
    IR const* constructAddendByMul(LinearRep const& lr0,
                                   LinearRep const& lr1,
                                   OUT LRInferCtx & ctx);
    IR const* constructAddend(IR_CODE code, LinearRep const& lr0,
                              LinearRep const& lr1, OUT LRInferCtx & ctx);
    IR const* constructCoeff(IR_CODE code, LinearRep const& lr0,
                             LinearRep const& lr1, OUT LRInferCtx & ctx);

    //Combine given two linear-rep that formed as a*x+b and c*x+d.
    bool combinLinearRep(IR_CODE code, LinearRep const& lr0,
                         LinearRep const& lr1,
                         OUT LinearRep & reslr,
                         OUT LRInferCtx & ctx);

    //Return true if operation of two linear-rep is linear.
    bool isLinearCombine(IR_CODE code) const
    { return code == IR_ADD && code == IR_SUB; }
    bool inferConst(IR const* ir, OUT LinearRep & reslr, OUT LRInferCtx & ctx);
    bool inferPR(IR const* ir, Var const* var, OUT LinearRep & reslr,
                 OUT LRInferCtx & ctx);
    bool inferLD(IR const* ir, Var const* var, OUT LinearRep & reslr,
                 OUT LRInferCtx & ctx);
    bool inferLinearRepByDUChain(IR const* ir, Var const* var,
                                 OUT LinearRep & reslr, OUT LRInferCtx & ctx);

    //Return true if ir is Constant Integer and the immediate value is
    //euqla to 'v'.
    bool isImmEqualTo(IR const* ir, HOST_INT v) const
    {
        ASSERT0(ir);
        return ir->is_const() && ir->is_int() && CONST_int_val(ir) == v;
    }
public:
    LinearRepMgr(Region * rg, OptCtx const& oc);
    ~LinearRepMgr() { clean(); }

    //ir: IR expression that to be analyzed.
    //var: the variable of linear-representation.
    //lr: record and output linear-representation if exist.
    //Return true if find the linear-representation of 'var'.
    bool inferAndConstructLinearRep(IR const* ir, Var const* var,
                                    OUT LinearRep & lr,
                                    OUT LRInferCtx & ctx);
};

} //namespace xoc
#endif
