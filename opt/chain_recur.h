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
#ifndef _CHAIN_RECUR_H_
#define _CHAIN_RECUR_H_

namespace xoc {

class IV;
class BIV;
class DIV;
class ChainRec;
class ChainRecMgr;
class IVVal;

typedef xcom::List<IVVal const*> ConstIVValList;
typedef xcom::List<IVVal const*>::Iter ConstIVValListIter;

//This class represents value of an IV.
//The value may be constant and expression.
#define IVVAL_int(v) ((v).u.m_int)
#define IVVAL_fp(v) ((v).u.m_fp)
#define IVVAL_md(v) ((v).u.m_md)
#define IVVAL_exp(v) ((v).u.m_exp)
#define IVVAL_cr(v) ((v).u.m_cr)
#define IVVAL_data_type(v) ((v).m_data_type)
#define IVVAL_kind(v) ((v).m_kind)
class IVVal {
    //This class premits copy-constructing.
public:
    typedef enum tagKind {
        VAL_UNDEF = 0,
        VAL_IS_VAR, //value is variable.
        VAL_IS_INT, //value is integer.
        VAL_IS_FP, //value is float-point.
        VAL_IS_EXP, //value is IR expression.
        VAL_IS_CR, //value is chain-rec.
    } Kind;
public:
    BYTE m_kind:3; //record the kind of value.
    Type const* m_data_type; //record the data-type of value.
    union {
        HOST_INT m_int; //record value if it is integer.
        HOST_FP m_fp; //record value if it is float.
        //Record value if it is variable. Note IV value must be exact MD.
        MD const* m_md;
        IR const* m_exp; //record value if it is IR expression.
        ChainRec * m_cr; //record value if it is chain-rec.
    } u;
public:
    IVVal() { clean(); }
    IVVal(HOST_INT v, Type const* ty) { setToInt(v, ty); }
    IVVal(HOST_FP v, Type const* ty) { setToFP(v, ty); }
    IVVal(MD const* v, Type const* ty) { setToVar(v, ty); }
    IVVal(IR const* v) { setToExp(v); }
    IVVal(ChainRec * v, Type const* ty) { setToCR(v, ty); }

    void clean() { ::memset((void*)this, 0, sizeof(IVVal)); }
    void clean(MOD ChainRecMgr & mgr);

    //The function guarrantees whole object (include subfield objects)
    //are exclusive.
    //e.g: if src is CR, the function will allocate a new CR object and copy
    //data structure recursively from src's CR object.
    void copyExclusive(IVVal const& src, MOD ChainRecMgr & mgr);

    //The function computes value by substituing src for variable in 'lr'.
    void computeByLinRep(IVVal const& src, LinearRep const& lr,
                         MOD ChainRecMgr & mgr);

    //Perform value arithmetics.
    //Return true if the function calculate a correct result value.
    //Note the function supports in-place operation, 'res' can be identical
    //to v0 or v1.
    static bool doAdd(IVVal const& v0, IVVal const& v1, OUT IVVal & res,
                      MOD ChainRecMgr & mgr);
    static bool doSub(IVVal const& v0, IVVal const& v1, OUT IVVal & res,
                      MOD ChainRecMgr & mgr);
    static bool doMul(IVVal const& v0, IVVal const& v1, OUT IVVal & res,
                      MOD ChainRecMgr & mgr);
    static bool doDiv(IVVal const& v0, IVVal const& v1, OUT IVVal & res,
                      MOD ChainRecMgr & mgr);
    void dump(Region const* rg) const;

    //The function extract value from given 'ir'.
    //Return true if extract value successfully.
    bool extractFrom(IR const* ir);

    //Return kind of value.
    Kind getKind() const { return (Kind)IVVAL_kind(*this); }

    //Return data type of value.
    Type const* getDType() const { return IVVAL_data_type(*this); }

    //Return Var if value is MD.
    Var const* getVar() const { return getMD()->get_base(); }

    //Return MD if value indicates MD.
    MD const* getMD() const
    {
        ASSERT0(is_var() && IVVAL_md((*this)));
        return IVVAL_md(*this);
    }

    //Return IR if value indicates expression.
    IR const* getExp() const
    {
        ASSERT0(is_exp() && IVVAL_exp((*this)));
        return IVVAL_exp(*this);
    }

    //Return integer if value indicates integer.
    HOST_INT getInt() const
    {
        ASSERT0(is_int());
        return IVVAL_int(*this);
    }

    //Return float-point if value indicates a float-point.
    HOST_FP getFP() const
    {
        ASSERT0(is_fp());
        return IVVAL_fp(*this);
    }

    //Return the binary content in memory of float-point
    //if value indicates a float-point.
    HOST_UINT getFPBin() const
    {
        ASSERT0(is_fp());
        return *((HOST_UINT*)&IVVAL_fp(*this));
    }

    //Return float-point if value indicates a float-point.
    ChainRec const* getCR() const
    {
        ASSERT0(is_cr());
        return IVVAL_cr(*this);
    }

    //Return the kind of value.
    bool is_undef() const { return IVVAL_kind(*this) == VAL_UNDEF; }
    bool is_int() const { return IVVAL_kind(*this) == VAL_IS_INT; }
    bool is_fp() const { return IVVAL_kind(*this) == VAL_IS_FP; }
    bool is_var() const { return IVVAL_kind(*this) == VAL_IS_VAR; }
    bool is_exp() const { return IVVAL_kind(*this) == VAL_IS_EXP; }
    bool is_cr() const { return IVVAL_kind(*this) == VAL_IS_CR; }

    //Return true if value is equal to v.
    bool isEqual(HOST_INT v) const;
    bool isEqual(HOST_FP v) const;
    bool isEqual(IVVal const& v) const;
    bool isEqual(IR const* v) const;

    //Set current value to be integer.
    void setToInt(HOST_INT val, Type const* ty)
    {
        ASSERTN(ty && (ty->is_int() || ty->isPointer()),
                ("given type can not be view as integer"));
        IVVAL_kind(*this) = VAL_IS_INT;
        IVVAL_int(*this) = val;
        IVVAL_data_type(*this) = ty;
    }

    //Set current value to be float-point.
    void setToFP(HOST_FP val, Type const* ty)
    {
        ASSERT0(ty && ty->is_fp());
        IVVAL_kind(*this) = VAL_IS_FP;
        IVVAL_fp(*this) = val;
        IVVAL_data_type(*this) = ty;
    }

    //Set current value to be Var.
    //exp: optional, record the IR occurrence that referenced 'md'.
    void setToVar(MD const* md, Type const* ty)
    {
        ASSERT0(md && ty);
        IVVAL_md(*this) = md;
        IVVAL_data_type(*this) = ty;
        IVVAL_kind(*this) = IVVal::VAL_IS_VAR;
    }

    //Set current value to be IR expression.
    //exp: optional, record the IR occurrence that referenced 'md'.
    void setToExp(IR const* exp)
    {
        ASSERT0(exp);
        IVVAL_exp(*this) = exp;
        IVVAL_data_type(*this) = exp->getType();
        IVVAL_kind(*this) = IVVal::VAL_IS_EXP;
    }

    //Set current value to be chain-rec.
    //cr: record the chain-rec.
    void setToCR(ChainRec * cr, Type const* ty)
    {
        ASSERT0(cr && ty);
        IVVAL_cr(*this) = cr;
        IVVAL_data_type(*this) = ty;
        IVVAL_kind(*this) = IVVal::VAL_IS_CR;
    }
    void setToCR(ChainRecMgr & mgr, Type const* ty);
};


typedef xcom::Vector<IVVal> IVValVec;

#define CR_code(cr) ((cr)->m_code)
#define CR_init(cr) ((cr)->m_init)
#define CR_step(cr) ((cr)->m_step)
class ChainRec {
    //The class allows copy-construction.
public:
    IR_CODE m_code;
    IVVal m_init;
    IVVal m_step;
public:
    ChainRec() : m_code(IR_UNDEF) {}
    ChainRec(IVVal const& init, IVVal const& step)
    { CR_init(this) = init; CR_step(this) = step; CR_code(this) = IR_ADD; }
    ChainRec(IVVal const& init, IVVal const& step, IR_CODE code)
    { CR_init(this) = init; CR_step(this) = step; CR_code(this) = code; }

    void clean(MOD ChainRecMgr & mgr);
    void copyExclusive(ChainRec const& src, MOD ChainRecMgr & mgr);
    void computeByLinRep(LinearRep const& lr, ChainRec const& src,
                         MOD ChainRecMgr & mgr);

    void dump(Region const* rg) const;
    void dumpComputedValue(MOD ChainRecMgr & mgr) const;

    //The function extract chain-rec from given 'iv'.
    //Return true if extract chain-rec successfully.
    void extractFrom(IV const* iv);
    void extractFrom(DIV const* iv);
    void extractFrom(BIV const* iv);

    //The function extract chain-rec from given 'ir'.
    //Return true if extract chain-rec successfully.
    //Regard 'ir' as loop invariant.
    void extractFromLoopInvariant(IR const* ir, ChainRecMgr const& mgr);

    IVVal const& getStep() const { return m_step; }
    IVVal const& getInit() const { return m_init; }
    IR_CODE getCode() const { return m_code; }

    //Return true if current chain-rec is equal to src.
    bool isEqual(ChainRec const& src) const;

    //Return true if current chain-rec form is equal to given IV value list.
    //e.g: given CR is {1,+,3}, function call of
    //     isEqual(2, &IVVal(HOST_INT(1)), &IVVal(HOST_INT(3)));
    //     will return true.
    bool isEqual(UINT num, ...) const;
    bool isEqual(ConstIVValList const& lst) const;

    //Return true if chainrec is either linear increasing or linear decreasing.
    bool isLinear() const { return getCode() == IR_ADD || getCode() == IR_SUB; }
    static bool isLinear(IR_CODE c) { return c == IR_ADD || c == IR_SUB; }
    bool isSanity() const
    {
        return !getInit().is_undef() && !getStep().is_undef() &&
               getCode() != IR_UNDEF;
    }

    //The function set code according to the increasing direction of IV.
    void setCodeByStep();

    //The function set code by analyzing step value.
    void setCodeByIV(IV const* iv);
};


class ChainRecMgr {
    COPY_CONSTRUCTOR(ChainRecMgr);
protected:
    SMemPool * m_pool;
    Region * m_rg;
    TypeMgr * m_tm;
    IRMgr * m_irmgr;
    OptCtx const* m_oc;
    xcom::List<IR*> m_irlst;
protected:
    bool computeStep(ChainRec const& cr, UINT num, OUT IVValVec & valvec);
    bool computeStepByCR(ChainRec const& cr, UINT num, OUT IVValVec & valvec);
    bool computeInit(ChainRec const& cr, UINT num, OUT IVValVec & valvec);

    void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, size);
        return p;
    }
public:
    //oc: optional, can be NULL.
    ChainRecMgr(Region * rg, OptCtx const* oc);
    ~ChainRecMgr();

    ChainRec * allocChainRec() { return (ChainRec*)xmalloc(sizeof(ChainRec)); }

    //The function compute the first 'num' value that represented by 'cr'.
    //Return true if the computation is successful, otherwise false which
    //meaning 'cr' can not be computed.
    //num: the number of value that user is going to compute.
    //     e.g:given chain-rec {1,+,3}, if num is 5, the evalated value are
    //     {1,4,7,10,13}
    //valvec: store the computed value into the vector as result.
    bool computeValue(ChainRec const& cr, UINT num, OUT IVValVec & valvec);

    //The function try to compute integer type for CR's step value by
    //default. Note the function will always return integer type.
    Type const* computeDefaultIntType(Type const* ty) const;

    //Add given two chain-recs, store result in 'res'.
    //Return true if the function calculate a correct result chain-rec.
    //Note the function supports in-place operation, 'res' can be identical
    //to cr0 or cr1.
    bool doAdd(ChainRec const& cr0, ChainRec const& cr1, OUT ChainRec & res);

    //Add given two chain-recs, store result in 'res'.
    //The function adds a loop invariant symbol 'ir' and cr1, and store the
    //result chain-rec to 'res'.
    //ir: user have to guarrantee it is a loop invariant variable.
    //e.g:x*{a,+,b} ==> {x*a,+,b}
    //Note the function supports in-place operation, 'res' can be identical
    //to cr0 or cr1.
    bool doAdd(ChainRec const& cr1, IVVal const& val, OUT ChainRec & res);

    //Note the function supports in-place operation.
    bool doAdd(ChainRec const& cr1, IR const* ir, OUT ChainRec & res)
    {
        IVVal cr2_init;
        bool succ = cr2_init.extractFrom(ir);
        if (!succ) { return false; }
        return doAdd(cr1, cr2_init, res);
    }

    //Note the function supports in-place operation.
    bool doAdd(ChainRec const& cr1, HOST_INT v, OUT ChainRec & res)
    { return doAdd(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    //Note the function supports in-place operation.
    bool doAdd(ChainRec const& cr1, HOST_FP v, OUT ChainRec & res)
    { return doAdd(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    //Note the function supports in-place operation.
    bool doAdd(ChainRec const& cr1, MD const* v, OUT ChainRec & res)
    { return doAdd(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    //Substract given two chain-recs, store result in 'res'.
    //Note the function supports in-place operation, 'res' can be identical
    //to cr0 or cr1.
    bool doSub(ChainRec const& cr0, ChainRec const& cr1, OUT ChainRec & res);

    //Note the function supports in-place operation.
    bool doSub(ChainRec const& cr1, IVVal const& v, OUT ChainRec & res);

    //Note the function supports in-place operation.
    bool doSub(IVVal const& v, ChainRec const& cr1, OUT ChainRec & res);

    //Substract given two chain-recs, store result in 'res'.
    //The function substracts a loop invariant symbol 'ir' and cr1, and
    //store the result chain-rec to 'res'.
    //ir: user have to guarrantee it is a loop invariant variable.
    //e.g:x*{a,-,b} ==> {x*a,-,b}
    //Note the function supports in-place operation, 'res' can be identical
    //to cr0 or cr1.
    bool doSub(ChainRec const& cr1, IR const* ir, OUT ChainRec & res)
    {
        IVVal cr2_init;
        bool succ = cr2_init.extractFrom(ir);
        if (!succ) { return false; }
        return doSub(cr1, cr2_init, res);
    }

    //Note the function supports in-place operation.
    bool doSub(IR const* ir, ChainRec const& cr1, OUT ChainRec & res);

    //Note the function supports in-place operation.
    bool doSub(ChainRec const& cr1, HOST_INT v, OUT ChainRec & res)
    { return doSub(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    //Note the function supports in-place operation.
    bool doSub(HOST_INT v, ChainRec const& cr1, OUT ChainRec & res)
    {
        bool succ = doMul(cr1, HOST_INT(-1), res);
        if (!succ) { return false; }
        return doAdd(res, IVVal(v, cr1.getInit().getDType()), res);
    }

    //Note the function supports in-place operation.
    bool doSub(ChainRec const& cr1, HOST_FP v, OUT ChainRec & res)
    { return doSub(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    //Note the function supports in-place operation.
    bool doSub(HOST_FP v, ChainRec const& cr1, OUT ChainRec & res)
    {
        bool succ = doMul(cr1, HOST_FP(-1.0), res);
        if (!succ) { return false; }
        return doAdd(res, IVVal(v, cr1.getInit().getDType()), res);
    }

    //Note the function supports in-place operation.
    bool doSub(ChainRec const& cr1, MD const* v, OUT ChainRec & res)
    { return doSub(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    //Note the function supports in-place operation.
    bool doSub(MD const* v, ChainRec const& cr1, OUT ChainRec & res)
    {
        bool succ = doMul(cr1, HOST_INT(-1), res);
        if (!succ) { return false; }
        return doAdd(res, IVVal(v, cr1.getInit().getDType()), res);
    }

    //Multiple given two chain-recs, store result in 'res'.
    //e.g:{x,+,y}*{a,+,b} ==> {x*a,+,{x*b+y*a+y*b,+,2*y*b}}
    //Note the function supports in-place operation, 'res' can be identical
    //to cr0 or cr1.
    bool doMul(ChainRec const& cr0, ChainRec const& cr1, OUT ChainRec & res);

    //Multiple given two chain-recs, store result in 'res'.
    //The function multiple a loop invariant symbol 'ir' and cr1, and store
    //the result chain-rec to 'res'.
    //ir: user have to guarrantee it is a loop invariant variable.
    //e.g:x*{a,+,b} ==> {x*a,+,x*b}
    //    x*{a,-,b} ==> {x*a,-,x*b}
    //Note the function supports in-place operation, 'res' can be identical
    //to cr0 or cr1.
    bool doMul(ChainRec const& cr1, IVVal const& val, OUT ChainRec & res);

    //Note the function supports in-place operation.
    bool doMul(ChainRec const& cr1, IR const* ir, OUT ChainRec & res)
    {
        if (cr1.getCode() != IR_ADD) { return false; }
        IVVal coeff;
        bool succ = coeff.extractFrom(ir);
        if (!succ) { return false; }
        return doMul(cr1, coeff, res);
    }

    //Note the function supports in-place operation.
    bool doMul(ChainRec const& cr1, HOST_INT v, OUT ChainRec & res)
    { return doMul(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    //Note the function supports in-place operation.
    bool doMul(ChainRec const& cr1, HOST_FP v, OUT ChainRec & res)
    { return doMul(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    //Note the function supports in-place operation.
    bool doMul(ChainRec const& cr1, MD const* v, OUT ChainRec & res)
    { return doMul(cr1, IVVal(v, cr1.getInit().getDType()), res); }

    OptCtx const& getOptCtx() const { ASSERT0(m_oc); return *m_oc; }
    Region * getRegion() const { return m_rg; }
    IRMgr * getIRMgr() const { return m_irmgr; }

    //The function records all allocated ir through the mgr and free them
    //at the destruction.
    void record(IR * ir) { ASSERT0(ir); m_irlst.append_tail(ir); }

    //The function will apply CR rewrite rules to refine given CR.
    //Return true if refinement is successful.
    //v: record the returned IVVal if cr can be rewritten to a single IVVal.
    //   Note the 'cr' is rewritten to IVVal, its code will be IR_UNDEF, and
    //   both init-val and step-val will be clean.
    bool refine(MOD ChainRec & cr, OUT IVVal & v);

    //The function will apply CR rewrite rules to refine given CR.
    //Return true if refinement is successful.
    //Note the function will NOT refine cr even if it can be rewritten to IVVal.
    bool refine(MOD ChainRec & cr);

    //The function will refine given IVVal.
    //Return true if refinement is successful.
    bool refine(MOD IVVal & v);

    void setOptCtx(OptCtx const* oc) { m_oc = oc; }
};

} //namespace xoc
#endif
