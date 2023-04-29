/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#ifndef _IR_IVR_H_
#define _IR_IVR_H_

namespace xoc {

class PRSSAMgr;
class MDSSAMgr;
class IVBoundInfo;

#define IV_INIT_VAL_UNDEF 0
#define IV_INIT_VAL_IS_VAR 1
#define IV_INIT_VAL_IS_INT 2
#define IV_INIT_VAL_IS_FP 3

//The class represent Induction Variable information.
//Note: IV may have multiple upper-bounds,
//e.g:
//  for (i=0;;i++) {
//    if (f(i) == 10) { goto L1; }
//    if (f(i) == 20) { goto L2; }
//  }
//  L1: ...
//  L2: ...
#define IV_is_biv(d) ((d)->m_is_biv)
#define IV_li(d) ((d)->m_li)
#define IV_reduction_stmt(d) ((d)->m_reduction_stmt)
class IV {
public:
    BYTE m_is_biv:1; //true if iv is BIV.
    LI<IRBB> const* m_li;

    //Record the reduction stmt of IV. Note reduction stmt indicates the
    //occrrence of IV in loop body as well.
    //Reduction is the unique stmt that defined IV in loop body.
    IR * m_reduction_stmt;
public:
    IV() { memset(this, 0, sizeof(IV)); }
    //Return the LoopInfo that IV located in.
    LI<IRBB> const* getLI() const { return IV_li(this); }

    //Return the MD of IV occcurrence.
    MD const* getOccMD() const { return getRedStmt()->getRefMD(); }

    //Return the reduction stmt of IV. Note reduction stmt indicates the
    //occrrence of IV in loop body as well.
    //Reduction is the unique stmt that defined IV in loop body.
    IR const* getRedStmt() const { return IV_reduction_stmt(this); }

    //Return the RHS of reduction stmt of IV.
    IR const* getRhsOccOfRedStmt() const;

    //Return true if current IV is basic IV.
    bool is_biv() const { return m_is_biv; }

    //Return true if IV is increment, otherwise return false.
    bool isInc() const;
};


//This class represents basic IV.
#define BIV_step(d) (((BIV*)d)->u2.step_int)
#define BIV_init_stmt(d) (((BIV*)d)->m_init_val_stmt)
#define BIV_initv_int(d) (((BIV*)d)->u1.init_val_int)
#define BIV_initv_fp(d) (((BIV*)d)->u1.init_val_fp)
#define BIV_initv_md(d) (((BIV*)d)->u1.init_val_md)
#define BIV_initv_data_type(d) (((BIV*)d)->m_init_val_data_type)
#define BIV_initv_kind(d) (((BIV*)d)->m_init_val_kind)
#define BIV_is_inc(d) (((BIV*)d)->m_is_inc)

typedef LONGLONG BIVIntType;
typedef HOST_FP BIVFpType;

//This class represents attributes of Basic IV.
//TODO: enable BIV-step supporting FP or VAR type.
class BIV : public IV {
    COPY_CONSTRUCTOR(BIV);
public:
    BYTE m_is_inc:1; //true if iv is increment, or false means iv is decrement.
    BYTE m_init_val_kind:2; //initial value may be integer, float or variable.
    Type const* m_init_val_data_type; //record the Type of init value.

    //The unique stmt that initialize the IV outside the loop.
    IR const* m_init_val_stmt;
    union {
        BIVIntType * init_val_int; //integer initial value.
        BIVFpType * init_val_fp; //float initial value.
        MD const* init_val_md; //initial value is variable.
    } u1;
    union {
        BIVIntType step_int; //step during each iteration, may be negative.
        BIVFpType step_fp; //step is float.
        MD const* step_md; //step is variable.
    } u2;
public:
    BIV() { ::memset(this, 0, sizeof(BIV)); }

    void dump(Region const* rg) const;

    bool hasInitVal() const { return BIV_initv_int(this) != nullptr; }

    //Return true if initial value is const.
    bool isInitConst() const { return isInitConstInt() || isInitConstFP(); }

    bool isInitConstInt() const
    { return BIV_initv_kind(this) == IV_INIT_VAL_IS_INT; }

    bool isInitConstFP() const
    { return BIV_initv_kind(this) == IV_INIT_VAL_IS_FP; }

    bool isInitVar() const
    { return BIV_initv_kind(this) == IV_INIT_VAL_IS_VAR; }

    //Return true if current IV is monotone increasing.
    bool is_inc() const { return BIV_is_inc(this); }

    //Return the data type of initial value.
    Type const* getInitValType() const { return BIV_initv_data_type(this); }

    //Get the stmt that iniailize the BIV.
    IR const* getInitStmt() const { return BIV_init_stmt(this); }

    //Get the expression that represents the initial value of the BIV.
    IR const* getInitExp() const
    {
        ASSERT0(getInitStmt() && getInitStmt()->getRHS());
        return getInitStmt()->getRHS();
    }

    //Get the integer initial value if IV is integer type.
    BIVIntType * getInitValInt() const
    {
        ASSERT0(isInitConstInt());
        return BIV_initv_int(this);
    }

    //Get the float-point initial value if IV is float-point type.
    BIVFpType * getInitValFP() const
    {
        ASSERT0(isInitConstFP());
        return BIV_initv_fp(this);
    }

    //Get the memory descriptor if IV is variable.
    MD const* getInitValMD() const
    {
        ASSERT0(isInitVar());
        return BIV_initv_md(this);
    }

    //Get the IV step integer value.
    //Note the float-point type step value should able to be converted to
    //integer. That is IVR does not allow, such as: f+=1.5f, step computation.
    BIVIntType getStep() const { return BIV_step(this); }

    //Generate the IR expression of initial value of BIV.
    IR * genInitExp(IRMgr * irmgr) const;

    //Generate the IR expression of step value of BIV.
    IR * genStepExp(IRMgr * irmgr) const;

    //Generate the IR expression to compare the BIV and end bound.
    IR * genBoundExp(IVBoundInfo const& boundinfo, IVR const* ivr,
                     IRMgr * irmgr, Region * rg) const;
};


//This class represents linear representation of variable that
//formulated as: a*i+b, where a is coeff, i is variable, b is addend.
class LinearRep {
public:
    IR const* coeff; //coefficient
    IR const* addend; //addend
    IR_CODE addend_sign; //the sign of addend
    IR const* var; //variable
    IV const* iv; //record BIV/DIV info
public:
    LinearRep() { memset(this, 0, sizeof(LinearRep)); }

    void copy(LinearRep const& src) { *this = src; }
    void dump(Region const* rg) const;

    //Return true if coeff is integer immediate, and the value is equal to 'v'.
    bool isCoeffEqualTo(HOST_INT v) const;
    bool is_valid() const { return var != nullptr; }
};


#define DIV_linrep(d) (((DIV*)d)->m_linrep)

//This class represents derived IV.
//Derived IV is linear represented by BIV or other DIV.
class DIV : public IV {
public:
    LinearRep * m_linrep; //linear-representation of variable
public:
    DIV() { memset(this, 0, sizeof(DIV)); }

    LinearRep * getLinRep() const { return DIV_linrep(this); }
    void dump(Region const* rg) const;
};


//The class represents the loop bound information.
//Loop bound is always used to describe the specific IV iterating process.
//Normally, a loop bound includes initial-value of IV, end-value of IV, the
//step of IV, and the increasing/decreasing direction of IV.
//Note the init-value and end-value may either be constant or IR expression.
#define IVBI_is_tc_imm(bi) ((bi).m_is_tc_imm)
#define IVBI_tc_imm(bi) ((bi).u1.m_tc_imm)
#define IVBI_tc_exp(bi) ((bi).u1.m_tc_exp)
#define IVBI_tc_init_val_imm(bi) ((bi).m_tc_init_val_imm)
#define IVBI_tc_end_val_imm(bi) ((bi).m_tc_end_val_imm)
#define IVBI_iv(bi) ((bi).m_biv)
#define IVBI_iv_end_bound_stmt(bi) ((bi).m_biv_end_bound_stmt)
#define IVBI_is_end_bound_closed(bi) ((bi).m_is_biv_end_bound_closed)
class IVBoundInfo {
public:
    //True indicates that trip-count is immediate, otherwise trip-count is
    //IR expression.
    bool m_is_tc_imm;

    //True to indicate the value of IV include the
    //maximum/minimum value of 'bexp'.
    //e.g:i <= N, is_closed_range is true.
    //    i <  N, is_closed_range is false.
    bool m_is_biv_end_bound_closed;
    union {
        HOST_INT m_tc_imm;
        IR const* m_tc_exp;
    } u1;
    HOST_INT m_tc_init_val_imm;
    HOST_INT m_tc_end_val_imm;

    //Record the basic IV that related to the bound.
    BIV const* m_biv;

    //Record the end bound stmt of BIV.
    IR const* m_biv_end_bound_stmt;
public:
    IVBoundInfo() { ::memset(this, 0, sizeof(IVBoundInfo)); }
    void dump(Region const* rg) const;
    IR const* getBound() const { return IVBI_iv_end_bound_stmt(*this); }
    HOST_INT getTCImm() const
    {
        ASSERT0(IVBI_is_tc_imm(*this));
        return IVBI_tc_imm(*this);
    }
    IR const* getTCExp() const
    {
        ASSERT0(!IVBI_is_tc_imm(*this));
        return IVBI_tc_exp(*this);
    }
    BIV const* getIV() const { return IVBI_iv(*this); }

    //Return true if trip-count is immediate.
    bool isTCImm() const { return IVBI_is_tc_imm(*this); }
    bool isEndBoundClosed() const { return IVBI_is_end_bound_closed(*this); }
};


class IVRCtx {
    COPY_CONSTRUCTOR(IVRCtx);
public:
    OptCtx * m_oc;
public:
    IVRCtx(OptCtx * oc) { m_oc = oc; }
    OptCtx * getOptCtx() const { return m_oc; }
};


//Induction Variable Recognization.
//Note: IV may have multiple upper-bounds,
//e.g:
//  for (i=0;;i++) {
//    if (f(i) == 10) { goto L1; }
//    if (f(i) == 20) { goto L2; }
//  }
//  L1: ...
//  L2: ...
class IVR : public Pass {
    COPY_CONSTRUCTOR(IVR);
protected:
    typedef TTab<UINT> IDTab;
    typedef TTabIter<UINT> IDTabIter;
    typedef SList<BIV*> BIVList;
    typedef SC<BIV*> * BIVListIter;
    typedef SList<DIV const*> DIVList;
    typedef SC<DIV const*> * DIVListIter;

    //True if IVR pass only find BIV and DIV for exact MD.
    //Note if IR_ST, IR_LD, IR_PR, IR_STPR are ANY, the MD is inexact.
    BYTE m_is_only_handle_exact_md:1;

    //True if only strictly match the monotonic code pattern: i=i+1.
    BYTE m_is_strictly_match_pattern:1;

    //True if user expect that compute IV through more complicated algo and
    //information, e.g: GVN.
    BYTE m_is_aggressive:1;

    //True if IV information is available.
    MDSystem * m_mdsys;
    TypeMgr * m_tm;
    DUMgr * m_du;
    IRMgr * m_irmgr;
    IRCFG * m_cfg;
    SMemPool * m_pool;
    SMemPool * m_sc_pool;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    GVN * m_gvn;
    Vector<BIVList*> m_li2bivlst;
    Vector<DIVList*> m_li2divlst;
    DefMiscBitSetMgr m_sbs_mgr;
protected:
    LinearRep * allocLinearRep()
    { return (LinearRep*)xmalloc(sizeof(LinearRep)); }
    BIV * allocBIV()
    {
        BIV * iv = (BIV*)xmalloc(sizeof(BIV));
        IV_is_biv(iv) = true;
        return iv;
    }
    DIV * allocDIV()
    {
        DIV * iv = (DIV*)xmalloc(sizeof(DIV));
        IV_is_biv(iv) = false;
        DIV_linrep(iv) = allocLinearRep();
        return iv;
    }

    //iv: IV info that will be modified
    bool computeInitVal(IR const* ir, OUT BIV * iv);
    bool computeConstInitValOfBIV(BIV const* biv, OUT HOST_INT & val) const;
    bool computeConstValOfExp(IR const* exp, OUT HOST_INT & val) const;

    void dump_recur(LI<IRBB> const* li, UINT indent) const;

    //Extract BIV info from linear-representation.
    bool extractBIV(IR const* def, LinearRep const& lr, IRSet const& defset,
                    LI<IRBB> const* li, OUT BIV ** biv);

    //Find the loop monotone increasing or decreasing bound stmt and relvant IV.
    //Return the mono-bound stmt and the IV. Otherwise return nullptr if
    //find nothing.
    IR const* findBIVBoundStmt(LI<IRBB> const* li, OUT BIV const** biv) const;

    void findBIV(LI<IRBB> const* li, IDTab & tmp);
    void findDIV(LI<IRBB> const* li, BIVList const& bivlst);
    virtual void findDIVByStmt(IR * ir, LI<IRBB> const* li,
                               BIVList const& bivlst, OUT IRSet & set);

    bool hasMultiDefInLoop(IR const* ir, LI<IRBB> const* li,
                           OUT IRSet * set) const;

    //Find initialze value of IV, if found return true,
    //otherwise return false.
    bool findInitVal(IRSet const& defset, OUT BIV * iv);
    IR * findMatchedOcc(MD const* biv, IR * start);
    //sdlst: record list of MD that has single DEF stmt.
    bool findSingleDefStmt(OUT List<MD*> & sdlst,  IRBB const* loophead,
                           IDTab const& modified_mds) const;

    DefSegMgr * getSegMgr() { return getSBSMgr()->getSegMgr(); }
    DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }

    //Return true if compare_exp is the BIV upper-bound expression and ivref
    //is the BIV reference.
    //compare_exp: the expression that indicates the upper-bound of IV.
    //ivref: IV reference in 'compare_exp'. It must be kid of 'compare_exp'.
    virtual bool isBIVBoundExp(BIV const* biv, IR const* compare_exp,
                               IR const* ivref) const;
    virtual bool isBIVBoundStmt(BIV const* biv, LI<IRBB> const* li,
                                IR const* stmt) const;
    bool isSelfModByDUSet(IR const* ir) const;
    bool isSelfModByPRSSA(IR const* ir) const;
    bool isSelfModByMDSSA(IR const* ir) const;

    //Return true if ir is addend of linear-representation.
    bool isAddend(LI<IRBB> const* li, IR const* ir) const;

    //Return true if ir is coefficent of linear-representation.
    bool isCoeff(LI<IRBB> const* li, IR const* ir) const;

    //Return true if ir is reduction-operation.
    //Note the function will use classic-DU/PRSSA/MDSSA to do analysis.
    //lr: record the linear-representation of 'ir' if exist.
    bool isReductionOp(IR const* ir, LI<IRBB> const* li,
                       OUT LinearRep * lr, OUT IRSet * set) const;

    void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }
    bool scanExp(IR const* ir, LI<IRBB> const* li, IDTab const& ivmds);
    void recordBIV(BIV * biv);
    void recordDIV(LI<IRBB> const* li, IR * red, LinearRep * linrep);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
    bool useGVN() const
    { return m_gvn != nullptr && m_gvn->is_valid(); }
public:
    explicit IVR(Region * rg) : Pass(rg)
    {
        ASSERT0(rg != nullptr);
        m_mdsys = rg->getMDSystem();
        m_du = rg->getDUMgr();
        m_irmgr = rg->getIRMgr();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_pool = smpoolCreate(sizeof(IV) * 4, MEM_COMM);
        m_sc_pool = smpoolCreate(sizeof(xcom::SC<IV*>) * 4, MEM_CONST_SIZE);
        m_is_only_handle_exact_md = true;
        m_is_strictly_match_pattern = false;
        m_is_aggressive = false;
        m_prssamgr = nullptr;
        m_mdssamgr = nullptr;
        m_gvn = nullptr;
    }
    virtual ~IVR()
    {
        smpoolDelete(m_pool);
        smpoolDelete(m_sc_pool);
    }

    void clean();

    //The function try to evaluate the constant trip-count for given 'li'.
    //Return true if the function reason out the constant trip-count.
    bool computeConstIVBound(LI<IRBB> const* li, OUT IVBoundInfo & tc) const;

    //The function try to evaluate the trip-count expression for given 'li'.
    //Return true if the function reason out the trip-count expression.
    bool computeExpIVBound(LI<IRBB> const* li, OUT IVBoundInfo & bi,
                           MOD IVRCtx & ivrctx) const;

    //The function try to evaluate the constant or expression trip-count for
    //given 'li'.
    //Return true if the function reason out one of constant or expression
    //trip-count.
    bool computeIVBound(LI<IRBB> const* li, OUT IVBoundInfo & tc,
                        MOD IVRCtx & ivrctx) const;

    bool dump() const;

    //Extract IV reference and Bound expression from mono-bound expression.
    bool extractIVBoundExp(IV const* biv, IR const* compare_exp,
                           OUT IR const** ivref,
                           OUT IR const** bexp) const;

    //Extract IV reference and Bound expression from mono-bound expression.
    //is_closed_range: true to indicate the value of IV include the
    //                 maximum/minimum value of 'bexp'.
    //                 e.g:i <= N, is_closed_range is true.
    //                     i <  N, is_closed_range is false.
    bool extractIVBoundExpFromStmt(IV const* iv, IR const* stmt,
                                   OUT IR const** ivref,
                                   OUT IR const** bexp,
                                   OUT bool * is_closed_range) const;

    //Given loop id, return the BIV list.
    BIVList const* getBIVList(UINT loopid) const
    { return m_li2bivlst.get(loopid); }

    //Given li, return the BIV list.
    BIVList const* getBIVList(LI<IRBB> const* li) const
    { return getBIVList(li->id()); }

    virtual CHAR const* getPassName() const
    { return "Induction Variable Recogization"; }
    PASS_TYPE getPassType() const { return PASS_IVR; }

    //Generate the expression that indicates trip-count.
    IR * genTripCountExp(BIV const* biv, IR const* initexp,
                         IR const* boundexp, HOST_INT step,
                         MOD IVRCtx & ivrctx) const;

    //Return true if ir is linear-representation of BIV.
    //li: loop info.
    //ir: the linear-rep candidate.
    //linrep: the output result that record the linear-rep info if 'ir' is.
    //e.g: if i is IV, a*i is the linear-represetation of i.
    bool isLinearRepOfIV(LI<IRBB> const* li, IR const* ir,
                         OUT LinearRep * linrep) const;

    //Return true if ir is linear-representation.
    //linrep: record the coeff, iv, and addend of linear-representation
    //        if exist.
    bool isLinearRep(LI<IRBB> const* li, IR const* ir,
                     OUT LinearRep * linrep) const;

    //Return true if ir is linear-representation of BIV.
    //e.g: if i is IV, a*i is the linear-represetation of i.
    //li: given the LoopInfo.
    //ir: IR expression that to be analyzed.
    //selfmd: indicates the MD of self-modified variable.
    //linrep: record and output linear-representation if exist.
    bool isLinearRepOfMD(LI<IRBB> const* li, IR const* ir, MD const* selfmd,
                         OUT LinearRep * linrep) const;

    //Return true if ir indicates IV reference.
    bool isIV(IR const* ir, OUT IV const** iv) const;

    //Return true if ir indicates IV reference in given loop 'li'.
    bool isIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const;

    //Return true if ir indicates BIV reference in given loop 'li'.
    bool isBIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const;

    //Return true if ir indicates DIV reference in given loop 'li'.
    bool isDIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const;

    //Return true if ir is self-modified, e.g: x = op(x).
    bool isSelfMod(IR const* ir) const;
    bool is_aggressive() const { return m_is_aggressive; }

    void setOnlyHandleExactMD(bool doit) { m_is_only_handle_exact_md = doit; }

    //Inform the pass to perform optimization aggressively.
    void setAggressive(bool doit);

    //Set 'strictly' to true if only strictly match the monotonic code
    //pattern: i=i+1.
    void setStrictlyMatchPattern(bool strictly)
    { m_is_strictly_match_pattern = strictly; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
