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
class LinearRep;
class IV;
class IVR;
class ChainRec;

//This class represents linear representation of BIV or DIV that
//formulated as: a*iv+b, where a is coeff, iv is variable, b is addend.
#define IVLR_iv(lr) ((lr)->m_iv)
class IVLinearRep : public LinearRep {
public:
    IV const* m_iv; //record BIV|DIV info
public:
    IVLinearRep() { clean(); }

    void clean() { LinearRep::clean(); m_iv = nullptr; }
    void copy(IVLinearRep const& src)
    { LinearRep::copy(src); m_iv = src.m_iv; }

    void dump(Region const* rg) const;

    IV const* getIV() const { return m_iv; }

    bool is_valid() const
    {
        //IV's linear-rep should have a variable.
        return getVarExp() != nullptr;
    }
};


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
#define IV_reduction_exp(d) ((d)->m_reduction_exp)
#define IV_initv(d) ((d)->m_init_val)
#define IV_stepv(d) ((d)->m_step_val)
class IV {
public:
    //The data type defined the increasing direction of current IV.
    typedef enum tagINCDIR {
        DIR_UNDEF = 0, //Do NOT know how to increase.
        DIR_POS, //Increasing positive.
        DIR_NEG, //Increasing negative.
    } INCDIR;
public:
    BYTE m_is_biv:1; //true if iv is BIV.
    LI<IRBB> const* m_li;

    //Record the reduction stmt of IV. Note reduction stmt indicates the
    //occrrence of IV in loop body as well.
    //Reduction is the unique stmt that defined IV in loop body.
    IR const* m_reduction_stmt;

    //Record the RHS expression of reduction stmt.
    //In SSA mode, the RHS expression of reduction stmt may not equal to LHS.
    //Note reduction exp may be NULL if current IV is DIV.
    IR const* m_reduction_exp;
    IVVal m_init_val; //record the initial value of BIV.
    IVVal m_step_val; //record the step value of BIV.
public:
    IV() { memset((void*)this, 0, sizeof(IV)); }

    void dump(Region const* rg) const;
    CHAR const* dump(VarMgr const* vm, OUT xcom::StrBuf & buf) const;

    //Return the LoopInfo that IV located in.
    LI<IRBB> const* getLI() const { return IV_li(this); }

    //Get the MD of IR stmt that represents the IV in reduction operation.
    MD const* getStmtOccMD() const
    {
        ASSERT0(getRedStmt());
        return getRedStmt()->getRefMD();
    }

    //Get the Var of IR stmt that represents the IV in reduction operation.
    Var const* getStmtOccVar() const
    {
        ASSERT0(getStmtOccMD());
        return getStmtOccMD()->get_base();
    }

    //Get the MD of IR expression that represents the IV in reduction operation.
    MD const* getExpOccMD() const
    {
        //Reduction exp may be NULL if current IV is DIV.
        return getRedExp() != nullptr ? getRedExp()->getRefMD() : nullptr;
    }

    //Get the Var of IR expression that represents the IV in
    //reduction operation.
    Var const* getExpOccVar() const
    {
        ASSERT0(getExpOccMD());
        return getExpOccMD()->get_base();
    }

    //Return the string name of stmt IV variable.
    CHAR const* getStmtOccVarName() const
    {
        ASSERT0(getStmtOccMD());
        return getStmtOccMD()->get_base()->get_name()->getStr();
    }

    //Return the string name of expression IV variable.
    CHAR const* getExpOccVarName() const
    {
        ASSERT0(getExpOccMD());
        return getExpOccMD()->get_base()->get_name()->getStr();
    }

    //Return the reduction stmt of IV. Note reduction stmt indicates the
    //occrrence of IV in loop body.
    //Reduction is the unique stmt that defined IV in loop body.
    IR const* getRedStmt() const { return IV_reduction_stmt(this); }

    //Return the computation expression of reduction operation.
    //Note the reduce-exp may be not the RHS of reduce-stmt.
    IR const* getRedExp() const { return IV_reduction_exp(this); }
    IVVal const& getInitVal() const { return IV_initv(this); }
    IVVal const& getStepVal() const { return IV_stepv(this); }

    //Get the IV step integer value.
    //Note the float-point type step value should able to be converted to
    //integer, otherwise, step computation, such as: i+=1.5f, is not
    //supported by IVR.
    HOST_INT getStepValInt() const
    {
        ASSERT0(isStepValInt());
        return getStepVal().getInt();
    }

    //Return true if IV found initial-value.
    bool hasInitVal() const { return !IV_initv(this).is_undef(); }

    //Return true if IV found step-value.
    bool hasStepVal() const { return !IV_stepv(this).is_undef(); }

    //Return true if current IV is basic IV.
    bool is_biv() const { return m_is_biv; }

    //Return true if current IV is derived IV.
    bool is_div() const { return !is_biv(); }

    //Return true if 'ir' represent the reference of current IV.
    bool isRefIV(IR const* ir) const;

    //Return true if 'ref' represent the reference of current IV.
    bool isRefIV(MD const* ref) const;

    //Return true if step value is integer.
    bool isStepValInt() const { return getStepVal().is_int(); }
    bool isStepValFP() const { return getStepVal().is_fp(); }
    bool isStepValVar() const { return getStepVal().is_var(); }
    bool isStepValExp() const { return getStepVal().is_exp(); }
    bool isStepValCR() const { return getStepVal().is_cr(); }
};


//This class represents Basic Induction Variable.
//TODO: enable BIV-step supporting FP or VAR type.
#define BIV_stepv(d) IV_stepv(d)
#define BIV_initv(d) IV_initv(d)
#define BIV_init_stmt(d) (((BIV*)d)->m_init_val_stmt)
class BIV : public IV {
public:
    //The unique stmt that initialize the IV outside the loop.
    IR const* m_init_val_stmt;
public:
    BIV() { ::memset((void*)this, 0, sizeof(BIV)); }

    void dump(Region const* rg) const;

    //Return true if initial value is const.
    bool isInitConst() const { return isInitConstInt() || isInitConstFP(); }

    bool isInitConstInt() const
    { return getInitVal().getKind() == IVVal::VAL_IS_INT; }

    bool isInitConstFP() const
    { return getInitVal().getKind() == IVVal::VAL_IS_FP; }

    bool isInitVar() const
    { return getInitVal().getKind() == IVVal::VAL_IS_VAR; }

    bool isInitExp() const
    { return getInitVal().getKind() == IVVal::VAL_IS_EXP; }

    //Return true if ir represents a reference to current BIV.
    bool isRefBIV(IR const* ir) const { return isRefIV(ir); }

    //Return true if md represents a reference to current BIV.
    bool isRefBIV(MD const* md) const { return isRefIV(md); }

    //Return true if IV is increasing positive.
    //Otherwise the function know nothing about the direction.
    bool isInc() const { return getIncDir() == IV::DIR_POS; }

    //Return true if IV is increasing negative.
    //Otherwise the function know nothing about the direction.
    bool isDec() const { return getIncDir() == IV::DIR_NEG; }

    //Return true if BIV is sanitary.
    bool isSanity() const
    { return hasInitVal() && hasStepVal() && getLI() != nullptr; }

    //Return the IV's increasing direction.
    //Return UNDEF if there is no direction information.
    IV::INCDIR getIncDir() const
    {
        return isStepValInt() ? //ONLY integer can judge direction.
            getStepValInt() >= 0 ? IV::DIR_POS : IV::DIR_NEG
            : IV::DIR_UNDEF;
    }

    //Return the data type of initial value.
    Type const* getInitValType() const { return getInitVal().getDType(); }

    //Get the stmt that iniailize the BIV.
    IR const* getInitStmt() const { return BIV_init_stmt(this); }

    //Get the IV variable that occurred in init-stmt.
    Var const* getInitIVVar() const
    {
        if (getInitStmt() == nullptr) { return nullptr; }
        MD const* md = getInitStmt()->getExactRef();
        ASSERT0(md);
        return md->get_base();
    }

    //Get the expression that represents the initial value of the BIV.
    //Note not all BIV has an initial stmt, namely initial expression.
    //e.g: $1 = phi(0, $2), the initial value has embedded in PHI.
    IR const* getInitExp() const
    {
        ASSERT0(getInitStmt() && getInitStmt()->getRHS());
        return getInitStmt()->getRHS();
    }

    //Get the integer initial value if IV is integer type.
    HOST_INT getInitValInt() const
    {
        ASSERT0(isInitConstInt());
        return getInitVal().getInt();
    }

    //Get the float-point initial value if IV is float-point type.
    HOST_FP getInitValFP() const
    {
        ASSERT0(isInitConstFP());
        return getInitVal().getFP();
    }

    //Get the memory descriptor if IV is variable.
    MD const* getInitValMD() const
    {
        ASSERT0(isInitVar());
        return getInitVal().getMD();
    }

    //Generate the IR expression of initial value of BIV.
    IR * genInitExp(IRMgr * irmgr) const;

    //Generate the IR expression of step value of BIV.
    IR * genStepExp(IRMgr * irmgr) const;

    //Generate the IR expression to compare the BIV and end bound.
    IR * genBoundExp(IVBoundInfo const& boundinfo, IVR const* ivr,
                     IRMgr * irmgr, Region * rg) const;
};


//This class represents Derived Induction Variable.
//Derived IV is linear represented by BIV or other DIV.
#define DIV_chain_rec(d) (((DIV*)d)->m_chain_rec)
#define DIV_initv(d) IV_initv(d)
#define DIV_stepv(d) IV_stepv(d)
class DIV : public IV {
public:
    //Record chain-rec.
    //e.g: current DIV is x, it is computed by x=i+3, then the chain-rec is
    //computed by i, where i is BIV.
    //Note the initial value and step value will extract from the chainrec.
    ChainRec const* m_chain_rec;
public:
    DIV() { memset((void*)this, 0, sizeof(DIV)); }
    void dump(Region const* rg) const;

    //Return the IV's increasing direction.
    IV::INCDIR getIncDir() const;

    ChainRec const* getChainRec() const { return DIV_chain_rec(this); }

    //Return true if ir represents a reference to current DIV.
    bool isRefDIV(IR const* ir) const { return isRefIV(ir); }

    //Return true if IV is increasing positive.
    //Otherwise the function know nothing about the direction.
    bool isInc() const { return getIncDir() == IV::DIR_POS; }

    //Return true if IV is increasing negative.
    //Otherwise the function know nothing about the direction.
    bool isDec() const { return getIncDir() == IV::DIR_NEG; }

    //Return true if DIV is sanitary.
    bool isSanity() const
    { return hasInitVal() && hasStepVal() && getLI() != nullptr; }
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
    IVBoundInfo() { ::memset((void*)this, 0, sizeof(IVBoundInfo)); }
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
    BIV const* getBIV() const { return IVBI_iv(*this); }

    //Return true if trip-count is immediate.
    bool isTCImm() const { return IVBI_is_tc_imm(*this); }
    bool isEndBoundClosed() const { return IVBI_is_end_bound_closed(*this); }
};


class IVRCtx {
    COPY_CONSTRUCTOR(IVRCtx);
public:
    OptCtx * m_oc;
    ActMgr * m_act_mgr;
public:
    IVRCtx(OptCtx * oc, ActMgr * am = nullptr) { m_oc = oc; m_act_mgr = am; }

    void dumpAct(CHAR const* format, ...) const;

    OptCtx * getOptCtx() const { return m_oc; }
    ActMgr * getActMgr() const { return m_act_mgr; }
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
    friend class FindBIVByChainRec;
    friend class FindBIVByRedOp;
    friend class FindDIV;
protected:
    typedef SList<BIV*> BIVList;
    typedef SC<BIV*> * BIVListIter;
    typedef SList<DIV const*> DIVList;
    typedef SC<DIV const*> * DIVListIter;

    //True if IVR pass only find BIV and DIV for exact MD.
    //Note if IR_ST, IR_LD, IR_PR, IR_STPR are ANY, the MD is inexact.
    BYTE m_is_only_handle_exact_md:1;

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
    DefMiscBitSetMgr * m_sbs_mgr;
    GVN * m_gvn;
    ActMgr * m_act_mgr;
    Vector<BIVList*> m_li2bivlst;
    Vector<DIVList*> m_li2divlst;
    ChainRecMgr m_crmgr;
protected:
    BIV * allocBIV()
    {
        BIV * iv = (BIV*)xmalloc(sizeof(BIV));
        IV_is_biv(iv) = true;
        return iv;
    }
    DIV * allocDIV() { return (DIV*)xmalloc(sizeof(DIV)); }
    ChainRec * allocChainRec() { return m_crmgr.allocChainRec(); }

    //The function analyze 'ir' and inference the value that can be used to
    //describe IV.
    //iv: IV info that will be modified
    bool computeInitVal(IR const* ir, OUT IVVal & iv) const;
    bool computeConstInitValOfBIV(BIV const* biv, OUT HOST_INT & val) const;
    bool computeConstValOfExp(IR const* exp, OUT HOST_INT & val) const;

    //Return true if ir is addend of linear-representation.
    bool canBeAddend(LI<IRBB> const* li, IR const* ir) const;

    //Return true if ir is coefficent of linear-representation.
    bool canBeCoeff(LI<IRBB> const* li, IR const* ir) const;

    void dump_recur(LI<IRBB> const* li, UINT indent) const;

    //Find the loop monotone increasing or decreasing bound stmt and relvant IV.
    //Return the mono-bound stmt and the IV. Otherwise return nullptr if
    //find nothing.
    IR const* findBIVBoundStmt(LI<IRBB> const* li, OUT BIV const** biv,
                               IVRCtx const& ivrctx) const;

    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_sbs_mgr; }
    xcom::DefSegMgr * getSegMgr() const { return getSBSMgr()->getSegMgr(); }
    ChainRecMgr & getChainRecMgr() { return m_crmgr; }

    //Return true if compare_exp is the BIV upper-bound expression and ivref
    //is the BIV reference.
    //compare_exp: the expression that indicates the upper-bound of IV.
    //ivref: IV reference in 'compare_exp'. It must be kid of 'compare_exp'.
    virtual bool isBIVBoundExp(BIV const* biv, IR const* compare_exp,
                               IR const* ivref) const;
    virtual bool isBIVBoundStmt(BIV const* biv, LI<IRBB> const* li,
                                IR const* stmt) const;

    //Return true if code can be reduction-op code.
    virtual bool isReductionOpCode(IR_CODE code) const
    { return code == IR_ADD || code == IR_SUB; }

    void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, size);
        return p;
    }
    void recordBIV(BIV * biv);
    void recordDIV(LI<IRBB> const* li, IR const* red, ChainRec const* cr,
                   OptCtx const& oc);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
    bool useGVN() const
    { return m_gvn != nullptr && m_gvn->is_valid(); }
public:
    explicit IVR(Region * rg);
    virtual ~IVR()
    {
        smpoolDelete(m_pool);
        smpoolDelete(m_sc_pool);
        delete m_sbs_mgr;
    }

    void clean();

    //The function try to evaluate the constant trip-count for given 'li'.
    //Return true if the function reason out the constant trip-count.
    bool computeConstIVBound(LI<IRBB> const* li, OUT IVBoundInfo & tc,
                             MOD IVRCtx & ivrctx) const;

    //The function try to evaluate the trip-count expression for given 'li'.
    //Return true if the function reason out the trip-count expression.
    bool computeExpIVBound(LI<IRBB> const* li, OUT IVBoundInfo & bi,
                           MOD IVRCtx & ivrctx) const;

    //The function try to evaluate the constant or expression trip-count for
    //given 'li'.
    //Return true if the function reasons out one of constant or expression
    //trip-count.
    bool computeIVBound(LI<IRBB> const* li, OUT IVBoundInfo & tc,
                        MOD IVRCtx & ivrctx) const;

    bool dump() const;

    //Extract IV reference and Bound expression from mono-bound expression.
    bool extractIVBoundExp(IV const* biv, IR const* compare_exp,
                           OUT IR const** ivref, OUT IR const** bexp) const;

    //Extract IV reference and Bound expression from mono-bound expression.
    //is_closed_range:
    //  true to indicate the value of IV include the
    //  maximum/minimum value of 'bexp'.
    //  e.g:i <= N, is_closed_range is true.
    //      i <  N, is_closed_range is false.
    bool extractIVBoundExpFromStmt(IV const* iv, IR const* stmt,
                                   OUT IR const** ivref,
                                   OUT IR const** bexp,
                                   OUT bool * is_closed_range) const;

    //Given loop id, return the BIV list.
    BIVList const* getBIVList(UINT loopid) const
    { return m_li2bivlst.get(loopid); }

    //Given loop 'li', return the BIV list.
    BIVList const* getBIVList(LI<IRBB> const* li) const
    { return getBIVList(li->id()); }

    virtual CHAR const* getPassName() const
    { return "Induction Variable Recogization"; }
    PASS_TYPE getPassType() const { return PASS_IVR; }
    ActMgr * getActMgr() const { return m_act_mgr; }

    //Generate the expression that represents 'biv' trip-count.
    IR * genTripCountExp(BIV const* biv, IR const* initexp,
                         IR const* boundexp, HOST_INT step,
                         MOD IVRCtx & ivrctx) const;

    //Return true if ir is expression that represent the multiple of IV.
    //e.g: iv or n*iv
    //li: loop info.
    //ir: the linear-rep candidate.
    //linrep: the output result that record the linear-rep info if 'ir' is.
    //e.g: if i is IV, a*i is the linear-represetation of i.
    bool isMultipleOfIV(LI<IRBB> const* li, IR const* ir,
                        OUT IVLinearRep * linrep) const;

    //Return true if ir is relaxed linear-representation about IV.
    //The function try to find the standard linear-expression such as: a*i+b,
    //moreover it permits extra loop invariant factor in the expression,
    //such as (i + c)*8 + a - 7, where c, a are loop invariant expression.
    //invstmtlst: optional, record the analysis result of LICM that indicates
    //            whether a stmt is invariant stmt.
    //linrep: record the linear-representation that found.
    //lrmgr: used to allocate IR expression for linear-representation.
    bool isRelaxLinearRepOfIV(LI<IRBB> const* li, IR const* ir,
                              InvStmtList const* invstmtlst, OptCtx const* oc,
                              OUT IVLinearRep * linrep,
                              MOD LinearRepMgr & lrmgr) const;

    //Return true if ir is linear-representation about IV.
    //The function find the expression such as: a*i+b, where a is at least 1,
    //b can be zero.
    //linrep: optional, if not nullptr, it records the coeff, iv, and addend
    //        of linear-representation if exist.
    bool isLinearRepOfIV(LI<IRBB> const* li, IR const* ir,
                         OUT IVLinearRep * linrep) const;

    //Return true if ir is expression that represent the multiple of IV.
    bool isMultipleOfMD(LI<IRBB> const* li, IR const* ir, MD const* selfmd,
                        OUT IVLinearRep * linrep) const;

    //Return true if ir indicates IV reference.
    //iv: record related IV information if ir is IV.
    bool isIV(IR const* ir, OUT IV const** iv) const;

    //Return true if ir indicates IV reference in given loop 'li'.
    bool isIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const;

    //Return true if ir indicates BIV reference in given loop 'li'.
    bool isBIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const;

    //Return true if ir indicates DIV reference in given loop 'li'.
    bool isDIV(LI<IRBB> const* li, IR const* ir, OUT IV const** iv) const;

    bool is_aggressive() const { return m_is_aggressive; }

    void setOnlyHandleExactMD(bool doit) { m_is_only_handle_exact_md = doit; }

    //Inform the pass to perform optimization aggressively.
    void setAggressive(bool doit);

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
