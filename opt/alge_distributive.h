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
#ifndef _ALGE_DISTRIBUTIVE_
#define _ALGE_DISTRIBUTIVE_

namespace xoc {

class DistCtx;
class AlgeDistributive;

class DistActMgr : public ActMgr {
    COPY_CONSTRUCTOR(DistActMgr);
public:
    DistActMgr(Region const* rg) : ActMgr(rg) {}

    //Dump misc action that related to given ir.
    //format: the reason.
    void dumpAct(CHAR const* format, ...);
};

//The class represents Coefficent and Variable of MUL operation.
class CoeffVarFactor {
    //THE CLASS PERMITS COPY-CONSTRUCTOR.
public:
    IR const* coeffexp;
    IR const* varexp;
public:
    void dump(Region const* rg) const;
};

class LinearFactor {
    //THE CLASS PERMITS COPY-CONSTRUCTOR.
public:
    IR const* linearexp;
    CoeffVarFactor cvf0; //the coeff and var of opnd0.
    CoeffVarFactor cvf1; //the coeff and var of opnd1.
public:
    void dump(Region const* rg) const;
};

class DistCtx : public PassCtx {
    //THE CLASS ALLOWS COPY-CONSTRUCTOR.
    friend class DistIntlImpl;
protected:
    AlgeDistributive * m_dist;
    IRMgr * m_irmgr;
    MDMgr * m_mdmgr;
    TypeMgr * m_tm; //may register new type.
    Refine * m_refine;
public:
    DistCtx(OptCtx & oc, AlgeDistributive * algedist);
    ~DistCtx();

    void dump() const;

    AlgeDistributive * getAlgeDist() const { return m_dist; }
    IRMgr * getIRMgr() const { return m_irmgr; }
    MDMgr * getMDMgr() const { return m_mdmgr; }
    TypeMgr * getTypeMgr() const { return m_tm; }
    Refine * getRefine() const { return m_refine; }
};


//This class represents algebraic distributive.
class AlgeDistributive : public Pass {
    COPY_CONSTRUCTOR(AlgeDistributive);
    friend class AlgeDistIntlImpl;
    friend class DistIntlImpl;
protected:
    bool m_is_aggressive;
    IRCFG * m_cfg;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    IRMgr * m_irmgr;
    Refine * m_refine;
    IRSimp * m_simp;
    DistActMgr m_am;

    //The table is used to prevent endless-loop compilation that casued by
    //associating same MemRef repeatedly when the pass performed
    //multiple times.
    VarRefMgr m_vr_mgr;
protected:
    bool canBeDist(IR const* ir) const;
    bool canBeDist(IR_CODE c) const;
    bool canBeCandStmt(IR const* ir) const;
    bool canBeCandConstOp(IR const* ir) const
    {
        ASSERT0(ir->isConstExp());
        return true; //always regard const-exp as candidate.
    }
    //The rank of operand can be used to determine the layout order of operand.
    //The operand with a higher rank will be processed preferentially.
    bool doDist(MOD DistCtx & ctx);

    bool extractCoeffVarFactorViaExtOp(
        IR const* ir, OUT CoeffVarFactor & cvf, MOD DistCtx & ctx) const;
    bool extractLinearFactorViaExtOp(
        IR const* ir, OUT LinearFactor & lf, MOD DistCtx & ctx) const;
    static bool extractCoeffAndVar(
        IR const* ir, OUT IR ** coeff, OUT Var ** var, MOD DistCtx & ctx);

    bool hasSideEffect(IR const* ir) const;

    bool initDepPass(MOD OptCtx & oc);

    //Perform optimization to operations that recorded in the opvec.
    //Return true if operations in vector have been optimized.
    bool distributive(
        MOD IR * ir, LinearFactor const& lf, MOD DistCtx & ctx) const;

    void reset();

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    explicit AlgeDistributive(Region * rg) : Pass(rg), m_am(rg)
    {
        ASSERT0(rg != nullptr);
        m_cfg = rg->getCFG();
        m_irmgr = nullptr;
        m_prssamgr = nullptr;
        m_mdssamgr = nullptr;
        m_refine = nullptr;
        m_simp = nullptr;
        m_is_aggressive = false;
    }
    virtual ~AlgeDistributive() {}

    //Export Functions.
    //The function simplifies expression by Combining-Like-Terms.
    //e.g:given ir in linopvec of ctx are: i, 100*i, and the opc is IR_ADD.
    //the function will generate 101*i;
    //Return true if the combination succeeded, and the ops that are recorded
    //in LinOpVec of 'ctx' are changed also.
    static bool combineLikeTerm(MOD DistCtx & ctx);

    void dumpAllAct() const { m_am.dump(); }
    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "Alge Distributive"; }
    DistActMgr & getActMgr() { return m_am; }
    PASS_TYPE getPassType() const { return PASS_ALGE_DISTRIBUTIVE; }
    IRSimp * getIRSimp() const { return m_simp; }
    Refine * getRefine() const { return m_refine; }

    //Return true if user ask to perform aggressive optimization that without
    //consideration of compilation time and memory.
    bool is_aggressive() const { return m_is_aggressive; }

    virtual bool perform(OptCtx & oc);

    //Set to true if user ask to perform aggressive optimization that without
    //consideration of compilation time and memory.
    void setAggressive(bool doit) { m_is_aggressive = doit; }
};

} //namespace xoc

#endif
