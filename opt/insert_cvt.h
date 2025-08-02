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

author: Su Zhenyu
@*/
#ifndef _IR_INSERT_CVT_H_
#define _IR_INSERT_CVT_H_

namespace xoc {

class Region;

//Insertion context variable.
//Set the following options true or false to enable or disable the convertion.
#define INSCVTCTX_stmt_removed(r) ((r).u1.s1.stmt_has_been_removed)
#define INSCVTCTX_optctx(r) ((r).m_oc)
class InsertCvtCtx {
public:
    typedef UINT BitUnion;
    union {
        struct {
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
    InsertCvtCtx(OptCtx * oc)
    {
        INSCVTCTX_stmt_removed(*this) = false;
        INSCVTCTX_optctx(*this) = oc;
    }
    InsertCvtCtx const& operator = (InsertCvtCtx const&);

    //Clean the actions which propagated bottom up during convertment.
    void cleanBottomUpFlag() { INSCVTCTX_stmt_removed(*this) = false; }

    //Return the OptCtx.
    OptCtx * getOptCtx() const { return INSCVTCTX_optctx(*this); }

    //Return true if stmt removed during the convertion.
    bool isStmtRemoved() const { return INSCVTCTX_stmt_removed(*this); }

    //Set flag to disable following optimizations.
    void setUnOptFlag() { INSCVTCTX_stmt_removed(*this) = false; }
};


//The class inserts type-convertion.
//First of all, the pass checks the IR relations between types:
//int32->int64, int32<->f32, int32<->f64, int64<->f32, int64<->f64, and
//insert CVT if necessary.
class InsertCvt : public Pass {
    COPY_CONSTRUCTOR(InsertCvt);
protected:
    TypeMgr * m_tm;
protected:
    virtual bool checkTypeConsistency(IR const* parent, IR const* kid) const
    { return parent->getType() == kid->getType(); }

    //The function only iteratively perform CVT insertion for kids of given ir.
    //It does not perform CVT insertion between 'ir' and its kids.
    IR * convertAllKids(IR * ir, bool & change, InsertCvtCtx & rc);

    //Try to convert constant expression.
    //Return updated ir if type converted.
    IR * convertConstExp(IR * ir, bool & change, InsertCvtCtx const& rc);
    IR * convertSetelem(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertGetelem(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertCall(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertICall(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertSwitch(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertReturn(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertBr(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertArray(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertNeg(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertNot(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertUnaryOp(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertBinaryOp(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertILoad(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertDet(IR * ir_list, bool & change, InsertCvtCtx & rc);
    IR * convertDirectStore(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertStoreArray(IR * ir, bool & change, InsertCvtCtx & rc);
    IR * convertIStore(IR * ir, bool & change, InsertCvtCtx & rc);
    bool convertStmtList(MOD BBIRList & ir_list, MOD InsertCvtCtx & rc);
    virtual IR * convertExtOp(IR * ir, bool & change, InsertCvtCtx & rc);

    //InsertCvt select for different architectures.
    virtual IR * convertSelect(IR * ir, bool & change, InsertCvtCtx & rc);

    //The function prefer to compute ir's const value by integer point type.
    IR * convertConstIntBinary(IR * ir, bool & change);

    //Check and insert data type CVT if it is necessary.
    //Insert CVT between 'parent' and 'kid' if need, otherwise return kid.
    IR * insertCvtImpl(IR * parent, IR * kid, bool & change);
    void insertCvtForBinaryOp(IR * ir, bool & change);
    void insertCvtForBinaryOpByPtrType(IR * ir, bool & change);
    void insertCvtForOpnd0OfBinaryOp(
        IR * ir, IR * op0, Type const* hoisted_type, bool & change);
    void insertCvtForOpnd1OfBinaryOp(
        IR * ir, IR * op1, Type const* hoisted_type, bool & change);

    //Insert CVT for float scalar if necessary. It will process three cases by
    //'insertCvtForFloatScalarCase1()' and 'insertCvtForFloatScalarCase2()'.
    IR * insertCvtForFloatScalar(IR * parent, IR * kid, bool & change);

    //If the parent is float but the kid is non-float or has inconsistent
    //precision, the CVT must be inserted.
    IR * insertCvtForFloatScalarCase1(IR * parent, IR * kid, bool & change);

    //If the kid is float but the parent is non-float or has inconsistent
    //precision, the CVT must be inserted.
    IR * insertCvtForFloatScalarCase2(IR * parent, IR * kid, bool & change);

    //Parent is floating-point vector and kid is floating-point vector and
    //their types are inconsistent.
    IR * insertCvtForFloatVector(IR * parent, IR * kid, bool & change);

    //Insert datatypecvt when setelem val src type different from dst type.
    IR * insertCvtForSetelemVal(IR * ir, IR * val);

    //Insert CVT for float if necessary. It will process three cases by
    //'insertCvtForFloatScalar()' and 'insertCvtForFloatVector()'.
    virtual IR * tryInsertCvtForFloat(IR * parent, IR * kid, bool & change);

    //Insert CVT for the operation that one of its kid is not vector type.
    //e.g:For a target support spreading a scalar to vector, the function does
    //not insert CVT between scalar and vector type.
    //NOTE: the function should check the legal size for both vector and
    //non-vector type.
    virtual IR * tryInsertCvtIfOneOpndIsVector(IR * parent, IR * kid) const
    {
        ASSERT0(parent && kid);
        //Target Dependent Code.
        ASSERT0(0);
        UINT tgt_size = parent->getTypeSize(m_tm);
        UINT src_size = kid->getTypeSize(m_tm);
        //parent operation permits current kid's type.
        //Do not do hoisting for vector type.
        ASSERTN_DUMMYUSE(tgt_size >= src_size, ("size is overflowed"));
        return kid;
    }
public:
    explicit InsertCvt(Region * rg);
    virtual ~InsertCvt() {}

    //Perform peephole optimization to ir_list.
    //Return updated ir_list if optimization performed.
    IR * convertIRlist(IR * ir_list, bool & change, MOD InsertCvtCtx & rc);

    //Perform peephole optimization to ir.
    //Return updated ir if optimization performed.
    virtual IR * convertIR(IR * ir, bool & change, MOD InsertCvtCtx & rc);

    //Perform peephole optimization to ir over and over again until the result
    //ir do not change any more.
    //Return updated ir if optimization performed.
    IR * convertIRUntilUnchange(IR * ir, bool & change, MOD InsertCvtCtx & rc);

    //Perform peephole optimization to BB list.
    //BB list will be updated if optimization performed.
    //Return true if BB list changed.
    bool convertBBlist(MOD BBList * ir_bb_list, MOD InsertCvtCtx & rc);

    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "InsertCvt"; }
    virtual PASS_TYPE getPassType() const { return PASS_INSERT_CVT; }

    bool perform(OptCtx & oc, MOD InsertCvtCtx & rc);
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
