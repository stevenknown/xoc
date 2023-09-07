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

//Refining context variable.
//Set the following options true or false to enable or disable the refinement.
#define RC_refine_div_const(r) ((r).u1.s1.refine_div_const)
#define RC_refine_mul_const(r) ((r).u1.s1.refine_mul_const)
#define RC_do_fold_const(r) ((r).u1.s1.do_fold_const)
#define RC_hoist_to_lnot(r) ((r).u1.s1.hoist_to_lnot)
#define RC_insert_cvt(r) ((r).u1.s1.insertCvt)
#define RC_refine_stmt(r) ((r).u1.s1.refine_stmt)
#define RC_stmt_removed(r) ((r).u1.s1.stmt_has_been_removed)
#define RC_update_mdref(r) ((r).u1.s1.update_mdref)
#define RC_optctx(r) ((r).m_oc)
class RefineCtx {
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

            //Pass info top-down.
            //If the flag is true, the process will insert IR_CVT
            //if kid's type size is smaller then parent's type size.
            //e.g: parent:I32 = kid:I8 will be
            //     parent:I32 = cvt:I32 (kid:I8)
            BitUnion insertCvt:1;

            //Pass info top-down. True to transform comparison stmt to lnot
            //e.g: transform $1!=0?0:1 to lnot($1),
            //where lnot indicates logical-not.
            BitUnion hoist_to_lnot:1;

            //Pass info top-down. True to compute and update MD reference
            //when memory operation generated or modified.
            BitUnion update_mdref:1;

            //Collect information bottom-up to inform caller function
            //that current stmt has been removed from the BB.
            //This flag may prevent the illegal removal when refinement
            //return back from IR expression processing.
            BitUnion stmt_has_been_removed:1;
        } s1;
        BitUnion i1;
    } u1;
    OptCtx * m_oc; //some operations may change flags in OptCtx.
public:
    RefineCtx(OptCtx * oc)
    {
        RC_refine_div_const(*this) = true;
        RC_refine_mul_const(*this) = false;
        RC_refine_stmt(*this) = true;
        RC_do_fold_const(*this) = true;
        RC_update_mdref(*this) = true;
        if (g_do_refine_auto_insert_cvt) {
            RC_insert_cvt(*this) = true;
        } else {
            RC_insert_cvt(*this) = false;
        }
        RC_stmt_removed(*this) = false;
        RC_hoist_to_lnot(*this) = true;
        m_oc = oc;
    }
    RefineCtx const& operator = (RefineCtx const&);

    //Clean the actions which propagated bottom up during refinement.
    void cleanBottomUpFlag() { RC_stmt_removed(*this) = false; }

    //Return the OptCtx.
    OptCtx * getOptCtx() const { return RC_optctx(*this); }

    bool fold_const() const { return RC_do_fold_const(*this); }

    bool hoist_to_lnot() const { return RC_hoist_to_lnot(*this); }

    bool insert_cvt() const { return RC_insert_cvt(*this); }

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

        if (g_do_refine_auto_insert_cvt) {
            RC_insert_cvt(*this) = true;
        } else {
            RC_insert_cvt(*this) = false;
        }

        RC_stmt_removed(*this) = false;
        RC_hoist_to_lnot(*this) = false;
    }

    bool update_mdref() const { return RC_update_mdref(*this); }
};


//This class perform peephole optimizations.
class Refine : public Pass {
    COPY_CONSTRUCTOR(Refine);

    IR * foldConstFloatUnary(IR * ir, bool & change);
    IR * foldConstFloatBinary(IR * ir, bool & change);
    IR * foldConstIntUnary(IR * ir, bool & change);
    IR * foldConstIntBinary(IR * ir, bool & change);

    //Check and insert data type CVT if it is necessary.
    IR * insertCvt(IR * parent, IR * kid, bool & change);
    void insertCvtForBinaryOp(IR * ir, bool & change);
    //Insert CVT for float if necessary.
    virtual IR * insertCvtForFloat(IR * parent, IR * kid, bool & change);

    //Peephole optimizations.
    IR * refineSetelem(IR * ir, bool & change, RefineCtx & rc);
    IR * refineGetelem(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBand(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBor(IR * ir, bool & change, RefineCtx & rc);
    IR * refineCvt(IR * ir, bool & change, RefineCtx & rc);
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
    IR * refineCall(IR * ir, bool & change, RefineCtx & rc);
    IR * refineICall(IR * ir, bool & change, RefineCtx & rc);
    IR * refineSwitch(IR * ir, bool & change, RefineCtx & rc);
    IR * refineReturn(IR * ir, bool & change, RefineCtx & rc);
    IR * refinePhi(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBr(IR * ir, bool & change, RefineCtx & rc);
    IR * refineBranch(IR * ir);
    IR * refineArray(IR * ir, bool & change, RefineCtx & rc);
    IR * refineNeg(IR * ir, bool & change, RefineCtx & rc);
    IR * refineNot(IR * ir, bool & change, RefineCtx & rc);
    IR * refineAsr(IR * ir, bool & change);
    IR * refineLsl(IR * ir, bool & change);
    IR * refineLsr(IR * ir, bool & change);
    IR * refineBinaryOp(IR * ir, bool & change, RefineCtx & rc);
    IR * refineLoad(IR * ir);
    IR * refineILoad1(IR * ir, bool & change, RefineCtx & rc);
    IR * refineILoad2(IR * ir, bool & change, RefineCtx & rc);
    IR * refineILoad3(IR * ir, bool & change, RefineCtx & rc);
    IR * refineILoad(IR * ir, bool & change, RefineCtx & rc);
    IR * refineDetViaSSAdu(IR * ir, bool & change);
    IR * refineDet(IR * ir_list, bool & change, RefineCtx & rc);
    IR * refineStore(IR * ir, bool & change, RefineCtx & rc);
    IR * refineStoreArray(IR * ir, bool & change, RefineCtx & rc);
    IR * refineIStore1(IR * ir, bool & change, RefineCtx & rc);
    IR * refineIStore(IR * ir, bool & change, RefineCtx & rc);
    bool refineStmtList(MOD BBIRList & ir_list, MOD RefineCtx & rc);
    IR * reassociation(IR * ir, bool & change);
    virtual IR * refineExtOp(IR * ir, bool & change, RefineCtx & rc);

    IR * StrengthReduce(MOD IR * ir, MOD bool & change, RefineCtx & rc);

    //The function will attempt to recompute the MD reference for given 'ir'.
    //Note the computation require that DUMgr has been ready.
    void recomputeMayRef(IR * ir);
protected:
    TypeMgr * m_tm;
public:
    explicit Refine(Region * rg);
    virtual ~Refine() {}

    virtual bool dump() const;

    //Perform const-folding.
    //Return updated ir if optimization performed.
    IR * foldConst(IR * ir, bool & change, RefineCtx const& rc);

    virtual CHAR const* getPassName() const { return "IR Refinement"; }
    virtual PASS_TYPE getPassType() const { return PASS_REFINE; }

    //Invert condition for relation operation.
    static void invertCondition(IR ** cond, Region * rg);

    //Perform peephole optimization to ir_list.
    //Return updated ir_list if optimization performed.
    IR * refineIRlist(IR * ir_list, bool & change, RefineCtx & rc);

    //Perform peephole optimization to ir.
    //Return updated ir if optimization performed.
    IR * refineIR(IR * ir, bool & change, RefineCtx & rc);

    //Perform peephole optimization to ir over and over again until the result
    //ir does not change any more.
    //Return updated ir if optimization performed.
    IR * refineIRUntilUnchange(IR * ir, bool & change, RefineCtx & rc);

    //Perform peephole optimization to BB list.
    //BB list will be updated if optimization performed.
    //Return true if BB list changed.
    bool refineBBlist(MOD BBList * ir_bb_list, MOD RefineCtx & rc);

    //Refine select for different architectures.
    virtual IR * refineSelect(IR * ir, bool & change, RefineCtx & rc);

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
