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
#ifndef _IR_REFINE_H_
#define _IR_REFINE_H_

namespace xoc {

class Region;
class ActMgr;

//Refining context variable.
//Set the following options true or false to enable or disable the refinement.
#define RC_refine_div_const(r) ((r).u1.s1.refine_div_const)
#define RC_refine_mul_const(r) ((r).u1.s1.refine_mul_const)
#define RC_do_fold_const(r) ((r).u1.s1.do_fold_const)
#define RC_hoist_to_lnot(r) ((r).u1.s1.hoist_to_lnot)
#define RC_refine_stmt(r) ((r).u1.s1.refine_stmt)
#define RC_stmt_removed(r) ((r).u1.s1.stmt_has_been_removed)
#define RC_maintain_du(r) ((r).u1.s1.maintain_du)
#define RC_optctx(r) ((r).m_oc)
class RefineCtx {
    //The class allows copy-constructor.
public:
    typedef UINT BitUnion;
    union {
        struct {
            //Pass info top-down. True to do following refinement.
            //e.g: int a; a/2 => a>>1
            BitUnion refine_div_const:1;

            //Pass info top-down. True to do following refinement.
            //e.g: int a; a*2 => a<<1
            //     int b; b*2 => b+b
            BitUnion refine_mul_const:1;

            //Pass info top-down. True to do following refinement.
            BitUnion refine_stmt:1;

            //Pass info top-down. True to do following refinement.
            //e.g: int a; a=2+3 => a=5
            BitUnion do_fold_const:1;

            //Pass info top-down. True to transform comparison stmt to lnot
            //e.g: transform $1!=0?0:1 to lnot($1),
            //where lnot indicates logical-not.
            BitUnion hoist_to_lnot:1;

            //Pass info top-down. True to update DefUse chain
            //when memory operation generated or modified.
            BitUnion maintain_du:1;

            //Collect information bottom-up to inform caller function
            //that current stmt has been removed from the BB.
            //This flag may prevent the illegal removal when refinement
            //return back from IR expression processing.
            BitUnion stmt_has_been_removed:1;
        } s1;
        BitUnion i1;
    } u1;
    OptCtx * m_oc; //some operations may change flags in OptCtx.
    ActMgr * m_act_mgr;
    PRSSAMgr const* m_prssamgr;
    MDSSAMgr const* m_mdssamgr;
public:
    RefineCtx(MOD OptCtx * oc)
    {
        RC_refine_div_const(*this) = true;
        RC_refine_mul_const(*this) = false;
        RC_refine_stmt(*this) = true;
        RC_do_fold_const(*this) = true;
        RC_maintain_du(*this) = true;
        RC_stmt_removed(*this) = false;
        RC_hoist_to_lnot(*this) = true;
        m_oc = oc;
        m_act_mgr = nullptr;
        ASSERT0(m_oc->getRegion());
        m_prssamgr = m_oc->getRegion()->getPRSSAMgr();
        m_mdssamgr = m_oc->getRegion()->getMDSSAMgr();
    }
    ~RefineCtx() { m_act_mgr = nullptr; }
    RefineCtx const& operator = (RefineCtx const&);

    //Clean the actions which propagated bottom up during refinement.
    void cleanBottomUpFlag() { RC_stmt_removed(*this) = false; }

    void dump() const;

    //Return the OptCtx.
    //Note some operations may change flags in OptCtx.
    OptCtx * getOptCtx() const { return RC_optctx(*this); }
    Region * getRegion() const { return getOptCtx()->getRegion(); }
    ActMgr * getActMgr() const { return m_act_mgr; }

    bool fold_const() const { return RC_do_fold_const(*this); }

    bool hoist_to_lnot() const { return RC_hoist_to_lnot(*this); }

    bool refine_stmt() const { return RC_refine_stmt(*this); }
    bool refine_div_const() const { return RC_refine_div_const(*this); }
    bool refind_mul_const() const { return RC_refine_mul_const(*this); }

    //Set flag to disable following optimizations.
    void setUnOptFlag()
    {
        RC_refine_div_const(*this) = false;
        RC_refine_mul_const(*this) = false;
        RC_refine_stmt(*this) = false;
        RC_do_fold_const(*this) = false;
        RC_stmt_removed(*this) = false;
        RC_hoist_to_lnot(*this) = false;
    }
    void setActMgr(ActMgr * am) { m_act_mgr = am; }

    //Return true if refinement need to maintain DefUse chain.
    bool maintainDU() const { return RC_maintain_du(*this); }

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
};


//This class perform peephole optimizations.
class Refine : public Pass {
    COPY_CONSTRUCTOR(Refine);
protected:
    TypeMgr * m_tm;
    IRMgr * m_irmgr;
protected:
    //The function try to choose proper data-type for judgement operation.
    //The data type is used to fold-const with specific judge operations.
    virtual Type const* chooseValueIntTypeOfJudgeOp(IR const* exp) const;

    //The function prefer to compute ir's const value by float point type.
    IR * foldConstFloatUnary(IR * ir, bool & change);

    //The function prefer to compute ir's const value by float point type.
    IR * foldConstFloatBinary(IR * ir, bool & change);
    IR * foldConstFloatBinaryForPow(IR * ir, bool & change);

    //The function prefer to compute ir's const value by integer point type.
    IR * foldConstIntUnary(IR * ir, bool & change);
    IR * foldConstAllKids(IR * ir, bool & change, RefineCtx & rc);

    //The function prefer to compute ir's const value by integer point type.
    IR * foldConstIntBinary(IR * ir, bool & change, RefineCtx & rc);
    IR * foldConstCompareAndShift(IR * ir, bool & change, RefineCtx & rc);
    virtual IR * foldConstExtExp(IR * ir, bool & change, RefineCtx & rc)
    {
        //Target Dependent Code.
        return ir;
    }
    virtual IR * foldConstUnary(IR * ir, bool & change, RefineCtx &);
    virtual IR * foldConstBinary(IR * ir, bool & change, MOD RefineCtx & rc);

    IRMgr * getIRMgr() const { return m_irmgr; }

    //Peephole optimizations.
    IR * refineSetelem(IR * ir, bool & change, RefineCtx & rc);
    IR * refineGetelem(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBand(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBor(IR * ir, bool & change, RefineCtx & rc);
    virtual IR * refineCvt(IR * ir, bool & change, RefineCtx & rc);
    IR * refineLand(IR * ir, bool & change, RefineCtx & rc);
    IR * refineLor(IR * ir, bool & change, RefineCtx & rc);
    IR * refineXor(IR * ir, bool & change, RefineCtx & rc);
    IR * refineAdd(IR * ir, bool & change, RefineCtx & rc);
    IR * refineSub(IR * ir, bool & change, RefineCtx & rc);
    IR * refineMul(IR * ir, bool & change, RefineCtx & rc);
    IR * refineRem(IR * ir, bool & change, RefineCtx & rc);
    IR * refineDiv(IR * ir, bool & change, RefineCtx & rc);
    IR * refineNe(IR * ir, bool & change, RefineCtx & rc);
    IR * refineEq(IR * ir, bool & change, RefineCtx & rc);
    IR * refineMod(IR * ir, bool & change, RefineCtx & rc);
    IR * refinePow(IR * ir, bool & change, RefineCtx & rc);
    IR * refineNRoot(IR * ir, bool & change, RefineCtx & rc);
    IR * refineLog(IR * ir, bool & change, RefineCtx & rc);
    IR * refineExponent(IR * ir, bool & change, RefineCtx & rc);
    IR * refineCall(IR * ir, bool & change, RefineCtx & rc);
    IR * refineICall(IR * ir, bool & change, RefineCtx & rc);
    IR * refineSwitch(IR * ir, bool & change, RefineCtx & rc);
    IR * refineReturn(IR * ir, bool & change, RefineCtx & rc);
    IR * refinePhi(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBr(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBranch(IR * ir, bool & change);
    IR * refineArray(IR * ir, bool & change, RefineCtx & rc);
    IR * refineAbs(IR * ir, bool & change, RefineCtx & rc);
    IR * refineNeg(IR * ir, bool & change, RefineCtx & rc);
    IR * refineNot(IR * ir, bool & change, RefineCtx & rc);
    IR * refineAsr(IR * ir, bool & change);
    IR * refineLsl(IR * ir, bool & change);
    IR * refineLsr(IR * ir, bool & change);
    virtual IR * refineTrigonometric(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBinaryOp(IR * ir, bool & change, RefineCtx & rc);
    IR * refineLoad(IR * ir);
    IR * refineILoad1(IR * ir, bool & change, RefineCtx & rc);
    IR * refineILoad2(IR * ir, bool & change, RefineCtx & rc);
    IR * refineILoad3(IR * ir, bool & change, RefineCtx & rc);
    IR * refineILoad(IR * ir, bool & change, RefineCtx & rc);
    IR * refineDetViaSSADU(IR * ir, bool & change, RefineCtx const& rc);
    IR * refineDet(IR * ir_list, bool & change, RefineCtx & rc);
    IR * refineDirectStore(IR * ir, bool & change, RefineCtx & rc);
    IR * refineStoreArray(IR * ir, bool & change, RefineCtx & rc);
    IR * refineIStore1(IR * ir, bool & change, RefineCtx & rc);
    IR * refineIStore(IR * ir, bool & change, RefineCtx & rc);
    IR * refineIgoto(IR * ir, bool & change, RefineCtx & rc);
    IR * refineIf(IR * ir, bool & change, RefineCtx & rc);
    IR * refineDoLoop(IR * ir, bool & change, RefineCtx & rc);
    IR * refineNormalLoop(IR * ir, bool & change, RefineCtx & rc);
    bool refineStmtList(MOD BBIRList & ir_list, MOD RefineCtx & rc);

    //The function only iteratively perform refinement for kids of given ir.
    //It does not perform refinement between 'ir' and its kids.
    IR * refineAllKids(IR * ir, bool & change, RefineCtx & rc);
    IR * reassociationCase1(IR * ir, bool & change);
    IR * reassociation(IR * ir, bool & change);
    IR * refineCompare(IR * ir, bool & change, RefineCtx & rc);
    virtual IR * refineExtOp(IR * ir, bool & change, RefineCtx & rc)
    {
        switch (ir->getCode()) {
        SWITCH_CASE_EXT_STMT:
        SWITCH_CASE_EXT_EXP:
            //Target Dependent Code.
            return ir;
        default: UNREACHABLE();
        }
        return nullptr;
    }
    IR * refineIRImpl(IR * ir, bool & change, MOD RefineCtx & rc);

    IR * StrengthReduce(MOD IR * ir, MOD bool & change, RefineCtx & rc);

    //The function will attempt to recompute the MD reference for given 'ir'.
    //Note the computation require that DUMgr has been ready.
    void recomputeMayRef(IR * ir);
public:
    explicit Refine(Region * rg);
    virtual ~Refine() {}

    //Calculate the value according to given binary code and type.
    HOST_INT calcBinIntVal(IR const* ir, HOST_INT v0, HOST_INT v1);

    //The function computes the copmile-time constant according to target
    //machine signed integer type.
    virtual HOST_INT calcBinSIntVal(IR_CODE code, Type const* ty,
                                    HOST_INT v0, HOST_INT v1);
    virtual HOST_UINT calcBinUIntVal(IR_CODE code, Type const* ty,
                                     HOST_UINT v0, HOST_UINT v1);

    virtual bool dump() const;

    //Perform const-folding.
    //Return updated ir if optimization performed.
    IR * foldConst(IR * ir, bool & change, RefineCtx & rc);

    virtual CHAR const* getPassName() const { return "IR Refinement"; }
    virtual PASS_TYPE getPassType() const { return PASS_REFINE; }

    //Invert condition for relation operation.
    static void invertCondition(IR ** cond, Region * rg);

    //Return true if given code may cause a runtime exception by
    //given v0 and v1.
    //The function is always used for avoiding different behaviours under -O3
    //when performing optimizations.
    bool mayCauseHardWareException(IR_CODE code, HOST_INT v0, HOST_INT v1);

    //Perform peephole optimization to ir_list.
    //Return updated ir_list if optimization performed.
    IR * refineIRList(IR * ir_list, bool & change, MOD RefineCtx & rc);

    //Perform peephole optimization to ir.
    //Return updated ir if optimization performed.
    virtual IR * refineIR(IR * ir, bool & change, MOD RefineCtx & rc);

    //Perform peephole optimization to ir over and over again until the result
    //ir do not change any more.
    //Return updated ir if optimization performed.
    IR * refineIRUntilUnchange(IR * ir, bool & change, MOD RefineCtx & rc);

    //Perform peephole optimization to BB list.
    //BB list will be updated if optimization performed.
    //Return true if BB list changed.
    bool refineBBlist(MOD BBList * ir_bb_list, MOD RefineCtx & rc);

    //Refine select for different architectures.
    virtual IR * refineSelect(IR * ir, bool & change, RefineCtx & rc);

    bool perform(OptCtx & oc, MOD RefineCtx & rc);
    virtual bool perform(OptCtx & oc) override;
};

} //namespace xoc
#endif
