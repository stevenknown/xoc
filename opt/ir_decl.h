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
#ifndef _IR_DECL_H_
#define _IR_DECL_H_

namespace xoc {

//Record float point.
#define CONST_fp_val(ir) (((CConst*)CK_IRC(ir, IR_CONST))->u1.s1.fp_const_value)

//Record the number of mantissa of float-point number.
#define CONST_fp_mant(ir) (((CConst*)CK_IRC(ir, IR_CONST))->u1.s1.fp_mantissa)

//Record integer.
#define CONST_int_val(ir) (((CConst*)CK_IRC(ir, IR_CONST))->u1.int_const_value)

//Record string.
#define CONST_str_val(ir) (((CConst*)CK_IRC(ir, IR_CONST))->u1.str_value)

//Record anonymous value.
#define CONST_anony_val(ir) \
    (((CConst*)CK_IRC(ir, IR_CONST))->u1.anonymous_value)
class CConst : public IR {
    COPY_CONSTRUCTOR(CConst);
public:
    union {
        //record string-const if current ir is string type.
        Sym const* str_value;

        //record integer value using a length of HOST_INT memory.
        HOST_INT int_const_value;

        //record float point value if current ir is float type.
        struct {
            HOST_FP fp_const_value;

            //record the number of mantissa of float-point number.
            BYTE fp_mantissa;
        } s1;

        //record customized value.
        void * anonymous_value;
    } u1;
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0x0;
public:
    HOST_FP getFP() const { return CONST_fp_val(this); }
    BYTE getMantissa() const { return CONST_fp_mant(this); }
    HOST_INT getInt() const { return CONST_int_val(this); }
    Sym const* getStr() const { return CONST_str_val(this); }
    void * getAnonymousVal() const { return CONST_anony_val(this); }
};

//Record Var property.
class VarProp {
    COPY_CONSTRUCTOR(VarProp);
public:
    //Record Var if ir is IR_LD|IR_ID.
    Var * id_info;
};


//Record DU property.
#define DUPROP_du(ir) (((DuProp*)ir)->du)
class DuProp : public IR {
    COPY_CONSTRUCTOR(DuProp);
public:
    DU * du;
};


//ID need DU info, some Passes requires it, e.g. GVN.
//Note IR_ID should NOT participate in GVN analysis because it does not
//represent a real operation.
#define ID_info(ir) (((CId*)CK_IRC(ir, IR_ID))->id_info)
#define ID_du(ir) (((CId*)CK_IRC(ir, IR_ID))->du)
#define ID_phi(ir) (((CId*)CK_IRC(ir, IR_ID))->phi)
class CId : public DuProp, public VarProp {
    COPY_CONSTRUCTOR(CId);
public:
    MDPhi * phi; //record the MD PHI dummy stmt if ID is operand of MD PHI.
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0x0;
public:
    MDPhi * getMDPhi() const { return ID_phi(this); }
    static inline Var *& accIdinfo(IR * ir) { return ID_info(ir); }
};


class OffsetProp {
    COPY_CONSTRUCTOR(OffsetProp);
public:
    //Record accessing field. result-type-idx should be D_MC.
    //ir is used by IR_LD|IR_ST|IR_ILD|IR_IST|IR_LDA|IR_ARRAY
    //
    //Usage:
    //    LD<ofst:3>('x')                 => pr=*(&x + 3)
    //    ILD<ofst:3>(LD('x'))            => pr=*(x + 3)
    //    ST<ofst:3>('x', IMM:0x100)      => *(&x + 3)=0x100
    //    IST<ofst:3>(LD('x'), IMM:0x100) => *(x + 3)=0x100
    //    LDA<ofst:3>('x')                => pr = &x + 3
    //    ARRAY<ofst:3>(LDA('x'), OFST:5) => *(&x[5] + 3) = pr or
    //                                       pr = *(&x[5] + 3)
    TMWORD field_offset;
};


//This class represents memory load operation.
//LD_ofst descibe the byte offset that is the addend to variable base address.
//
//usage: ld(i32, ofst:10, s) with LD_ofst = 10 means:
//    Assum a pointer p, it point to the address of variable s.
//    The ld operation loads i32 value from the address (p + 10)
#define LD_ofst(ir) (((CLd*)CK_IRC(ir, IR_LD))->field_offset)
#define LD_idinfo(ir) (((CLd*)CK_IRC(ir, IR_LD))->id_info)
#define LD_align(ir) (((CLd*)CK_IRC(ir, IR_LD))->align)
#define LD_is_aligned(ir) (((CLd*)CK_IRC(ir, IR_LD))->is_aligned)
#define LD_du(ir) (((CLd*)CK_IRC(ir, IR_LD))->du)

//Return the storage space if any.
#define LD_storage_space(ir) VAR_storage_space(LD_idinfo(ir))
class CLd : public DuProp, public VarProp, public OffsetProp {
    COPY_CONSTRUCTOR(CLd);
public:
    static BYTE const kid_map = 0;
    static BYTE const kid_num = 0;
    UINT align;
    bool is_aligned;
public:
    static inline Var *& accIdinfo(IR * ir) { return LD_idinfo(ir); }
    static inline TMWORD & accOfst(IR * ir) { return LD_ofst(ir); }
    static inline StorageSpace & accSS(IR * ir) { return LD_storage_space(ir); }
    static IR * dupIRTreeByStmt(IR const* src, Region * rg);
};


//This class represents indirect memory load operation.
//ILD_ofst descibe the byte offset that is the addend to address.
//If ILD_ofst is not 0, the base memory address must add the offset.
//
//usage: ild p, where p is ILD_base, it must be pointer.
//    1. res = ild (p), if ILD_ofst is 0.
//    2. res = ild (p + ILD_ofst) if ILD_ofst is not 0.
#define ILD_ofst(ir) (((CILd*)CK_IRC(ir, IR_ILD))->field_offset)
#define ILD_du(ir) (((CILd*)CK_IRC(ir, IR_ILD))->du)
#define ILD_base(ir) ILD_kid(ir, 0)
#define ILD_kid(ir, idx) CK_KID(ir, CILd, IR_ILD, idx)
#define ILD_storage_space(ir) (((CILd*)CK_IRC(ir, IR_ILD))->storage_space)
#define ILD_align(ir) (((CILd*)CK_IRC(ir, IR_ILD))->alignment)
#define ILD_is_aligned(ir) (((CILd*)CK_IRC(ir, IR_ILD))->is_aligned)
class CILd : public DuProp, public OffsetProp {
    COPY_CONSTRUCTOR(CILd);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    UINT alignment;
    bool is_aligned;
    StorageSpace storage_space;
    IR * opnd[kid_num];
public:
    static inline TMWORD & accOfst(IR * ir) { return ILD_ofst(ir); }
    static inline IR *& accKid(IR * ir, UINT idx) { return ILD_kid(ir, idx); }
    static inline IR *& accBase(IR * ir) { return ILD_base(ir); }
    static inline StorageSpace & accSS(IR * ir)
    { return ILD_storage_space(ir); }

    static IR * dupIRTreeByStmt(IR const* src, Region * rg);

    IR * getKid(UINT idx) const { return ILD_kid(this, idx); }
    IR * getBase() const { return ILD_base(this); }
};


//This class represents properties of stmt.
class StmtProp {
    COPY_CONSTRUCTOR(StmtProp);
public:
    IRBB * bb;
};


//This class represents memory store operation.
//ST_ofst descibe the byte offset that is the addend to address.
//ST_idinfo describe the memory variable.
//If ST_ofst is not 0, the base memory address must add the offset.
//
//usage: st(lhs, rhs), p = &lhs, where p is the memory address of lhs.
//    1. [p] = rhs, if ST_ofst is 0.
//    2. [p + ST_ofst] = rhs if ST_ofst is not 0.
#define ST_bb(ir) (((CSt*)CK_IRC(ir, IR_ST))->bb)
#define ST_idinfo(ir) (((CSt*)CK_IRC(ir, IR_ST))->id_info)
#define ST_align(ir) (((CSt*)CK_IRC(ir, IR_ST))->alignment)
#define ST_is_aligned(ir) (((CSt*)CK_IRC(ir, IR_ST))->is_aligned)
#define ST_ofst(ir) (((CSt*)CK_IRC(ir, IR_ST))->field_offset)

//Return the storage space if any.
#define ST_storage_space(ir) VAR_storage_space(ST_idinfo(ir))
#define ST_du(ir) (((CSt*)CK_IRC(ir, IR_ST))->du)
#define ST_rhs(ir) ST_kid(ir, 0)
#define ST_kid(ir, idx) (((CSt*)ir)->opnd[CK_KID_IRC(ir, IR_ST, idx)])
class CSt : public CLd, public StmtProp {
    COPY_CONSTRUCTOR(CSt);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    UINT alignment;
    bool is_aligned;
    IR * opnd[kid_num];
public:
    static inline IR *& accRHS(IR * ir) { return ST_rhs(ir); }
    static inline Var *& accIdinfo(IR * ir) { return ST_idinfo(ir); }
    static inline TMWORD & accOfst(IR * ir) { return ST_ofst(ir); }
    static inline StorageSpace & accSS(IR * ir) { return ST_storage_space(ir); }
    static inline IR *& accKid(IR * ir, UINT idx) { return ST_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return ST_bb(ir); }

    static IR * dupIRTreeByExp(IR const* src, IR * rhs, Region * rg);

    IR * getKid(UINT idx) const { return ST_kid(this, idx); }
    IR * getRHS() const { return ST_rhs(this); }
};


//This class represents temporary memory store operation.
//The temporary memory named pseudo register.
//usage: stpr(prno:1, val), will store val to PR1.
#define STPR_bb(ir) (((CStpr*)CK_IRC(ir, IR_STPR))->bb)
#define STPR_no(ir) (((CStpr*)CK_IRC(ir, IR_STPR))->prno)
#define STPR_ssainfo(ir) (((CStpr*)CK_IRC(ir, IR_STPR))->ssainfo)
#define STPR_du(ir) (((CStpr*)CK_IRC(ir, IR_STPR))->du)
#define STPR_rhs(ir) STPR_kid(ir, 0)
#define STPR_kid(ir, idx) (((CStpr*)ir)->opnd[CK_KID_IRC(ir, IR_STPR, idx)])
class CStpr : public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CStpr);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    PRNO prno; //PR number.
    SSAInfo * ssainfo; //Present ssa def and use set.
    IR * opnd[kid_num];
public:
    static inline IR *& accRHS(IR * ir) { return STPR_rhs(ir); }
    static inline SSAInfo *& accSSAInfo(IR * ir) { return STPR_ssainfo(ir); }
    static inline PRNO & accPrno(IR * ir) { return STPR_no(ir); }
    static inline IR * accResultPR(IR * ir) { return ir; }
    static inline IR *& accKid(IR * ir, UINT idx) { return STPR_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return STPR_bb(ir); }

    IR * getKid(UINT idx) const { return STPR_kid(this, idx); }
    IR * getRHS() const { return STPR_rhs(this); }
    static IR * dupIRTreeByExp(IR const* src, IR * rhs, Region * rg);
};


//This class represents an operation that store value to be part of the section
//of 'base'.
//NOTE: Type of result PR should same with base.
//SETELEM_ofst descibe the byte offset to the start address of result PR.
//The the number of bytes of result PR must be an integer multiple of
//the number of bytes of SETELEM_val if the result data type is vector.
//
//usage: stpr $ofst = 4;
//       setelem $2:vec<4*i32> = $3:vec<4*i32>, $1:i32, $ofst.
//  The result PR is $2, the base value is $3, $1 is the input value, and $ofst
//  record the byte offset which is 4.
//  The code stores $1 that is part of $3 to be second element
//  of $2. In this case, the second element's offset in $2 is 4 bytes.
//
//This operation will store value to the memory which offset to the
//memory chunk or vector's base address.
#define SETELEM_bb(ir) (((CSetElem*)CK_IRC(ir, IR_SETELEM))->bb)
#define SETELEM_prno(ir) (((CSetElem*)CK_IRC(ir, IR_SETELEM))->prno)
#define SETELEM_ssainfo(ir) (((CSetElem*)CK_IRC(ir, IR_SETELEM))->ssainfo)
#define SETELEM_du(ir) (((CSetElem*)CK_IRC(ir, IR_SETELEM))->du)
#define SETELEM_base(ir) SETELEM_kid(ir, 0)
#define SETELEM_val(ir) SETELEM_kid(ir, 1)
#define SETELEM_ofst(ir) SETELEM_kid(ir, 2)
#define SETELEM_kid(ir, idx) \
    (((CSetElem*)ir)->opnd[CK_KID_IRC(ir, IR_SETELEM, idx)])
class CSetElem : public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CSetElem);
public:
    static BYTE const kid_map = 0x7;
    static BYTE const kid_num = 3;
    PRNO prno; //PR number.
    SSAInfo * ssainfo; //Present ssa def and use set.
    IR * opnd[kid_num];
public:
    static inline SSAInfo *& accSSAInfo(IR * ir) { return SETELEM_ssainfo(ir); }
    static inline PRNO & accPrno(IR * ir) { return SETELEM_prno(ir); }
    static inline IR * accResultPR(IR * ir) { return ir; }
    static inline IR *& accKid(IR * ir, UINT idx)
    { return SETELEM_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return SETELEM_bb(ir); }

    IR * getKid(UINT idx) const { return SETELEM_kid(this, idx); }
    IR * getBase() const { return SETELEM_base(this); }
    IR * getVal() const { return SETELEM_val(this); }
};


//This class represents an operation that get an element from a base memory
//location and store the element to a PR.
//
//GETELEM_ofst descibe the byte offset to the start address of GETELEM_base.
//The byte offset must be an integer multiple of the byte size of element
//in GETELEM_base.

//The the number of byte of GETELEM_base must be
//an integer multiple of the number of byte of result PR if base is vector.
//
//usage: getelem $1:i32 $2:vec<4*i32>, 4.
//    The base object is a PR $2, which is a vector.
//    The example get the second element of $2, then store it to $1.
#define GETELEM_bb(ir) (((CGetElem*)CK_IRC(ir, IR_GETELEM))->bb)
#define GETELEM_prno(ir) (((CGetElem*)CK_IRC(ir, IR_GETELEM))->prno)
#define GETELEM_ssainfo(ir) (((CGetElem*)CK_IRC(ir, IR_GETELEM))->ssainfo)
#define GETELEM_du(ir) (((CGetElem*)CK_IRC(ir, IR_GETELEM))->du)
#define GETELEM_base(ir) GETELEM_kid(ir, 0)
#define GETELEM_ofst(ir) GETELEM_kid(ir, 1)
#define GETELEM_kid(ir, idx) \
    (((CGetElem*)ir)->opnd[CK_KID_IRC(ir, IR_GETELEM, idx)])
class CGetElem : public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CGetElem);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    PRNO prno; //PR number.
    //versioned presentation or ssa def and use list in ssa mode.
    //Note this field only avaiable if SSA information is maintained.
    SSAInfo * ssainfo;
    IR * opnd[kid_num];
public:
    static inline SSAInfo *& accSSAInfo(IR * ir) { return GETELEM_ssainfo(ir); }
    static inline PRNO & accPrno(IR * ir) { return GETELEM_prno(ir); }
    static inline IR * accResultPR(IR * ir) { return ir; }
    static inline IR *& accKid(IR * ir, UINT idx)
    { return GETELEM_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return GETELEM_bb(ir); }

    IR * getKid(UINT idx) const { return GETELEM_kid(this, idx); }
    IR * getBase() const { return GETELEM_base(this); }
};


//This class represents indirect memory store operation.
//IST_ofst descibe the byte offset that is the addend to address.
//
//If IST_ofst is not 0, the base memory address must add the offset.
//
//usage: ist = ld p, rhs, where the value of p is the base memory address
//to be stored. The followed code exhibits the behaivor of such usage.
//    1. [p] = rhs, if IST_ofst is 0.
//    2. [p + IST_ofst] = rhs, if IST_ofst is not 0.
#define IST_bb(ir) (((CISt*)CK_IRC(ir, IR_IST))->bb)
#define IST_ofst(ir) (((CISt*)CK_IRC(ir, IR_IST))->field_offset)
#define IST_du(ir) (((CISt*)CK_IRC(ir, IR_IST))->du)
#define IST_base(ir) IST_kid(ir, 0)
#define IST_rhs(ir) IST_kid(ir, 1)
#define IST_storage_space(ir) (((CISt*)CK_IRC(ir, IR_IST))->storage_space)
#define IST_align(ir) (((CISt*)CK_IRC(ir, IR_IST))->alignment)
#define IST_is_aligned(ir) (((CISt*)CK_IRC(ir, IR_IST))->is_aligned)
#define IST_kid(ir, idx) (((CISt*)ir)->opnd[CK_KID_IRC(ir, IR_IST, idx)])
class CISt : public DuProp, public OffsetProp, public StmtProp {
    COPY_CONSTRUCTOR(CISt);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    UINT alignment;
    bool is_aligned;
    StorageSpace storage_space;
    IR * opnd[kid_num];
public:
    static inline IR *& accRHS(IR * ir) { return IST_rhs(ir); }
    static inline TMWORD & accOfst(IR * ir) { return IST_ofst(ir); }
    static inline IR *& accKid(IR * ir, UINT idx) { return IST_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return IST_bb(ir); }
    static inline IR *& accBase(IR * ir) { return IST_base(ir); }
    static inline StorageSpace & accSS(IR * ir)
    { return IST_storage_space(ir); }

    static IR * dupIRTreeByExp(IR const* src, IR * rhs, Region * rg);

    IR * getKid(UINT idx) const { return IST_kid(this, idx); }
    IR * getRHS() const { return IST_rhs(this); }
    IR * getBase() const { return IST_base(this); }
};


//This class represents the operation to load memory variable address.
//The base of LDA may be ID variable, LABEL variable, STRING variable.
//NOTE: LDA_ofst describe the byte offset that is the addend to the address.
//usage: lda(s) with LDA_ofst = 10 means:
//    pointer p = lda(s)
//    p = p + 10
//    return p
#define LDA_ofst(ir) (((CLda*)CK_IRC(ir, IR_LDA))->field_offset)
#define LDA_idinfo(ir) (((CLda*)CK_IRC(ir, IR_LDA))->id_info)
#define LDA_storage_space(ir) VAR_storage_space(LDA_idinfo(ir))
class CLda : public IR, public VarProp, public OffsetProp {
    COPY_CONSTRUCTOR(CLda);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0;
public:
    static inline Var *& accIdinfo(IR * ir) { return LDA_idinfo(ir); }
    static inline TMWORD & accOfst(IR * ir) { return LDA_ofst(ir); }
    static inline StorageSpace & accSS(IR * ir)
    { return LDA_storage_space(ir); }
};


//This class uses bits to describe attributes.
//Represents a direct function call.
//NOTE: 'opnd' must be the last member.
//Record the BB that CALL stmt placed.
#define CALL_bb(ir) CK_FLD_KIND(ir, CCall, CK_IRC_CALL, bb)

//Represents the identifier info fo the function call.
#define CALL_idinfo(ir) CK_FLD_KIND(ir, CCall, CK_IRC_ONLY_CALL, id_info)

//Return the result PRNO if any.
//#define CALL_prno(ir) (((CCall*)CK_IRC_CALL(ir))->prno)
#define CALL_prno(ir) CK_FLD_KIND(ir, CCall, CK_IRC_CALL, prno)

//Return the storage space if any.
#define CALL_storage_space(ir) VAR_storage_space(CALL_idinfo(ir))

//Access the SSA info of result PR.
#define CALL_ssainfo(ir) CK_FLD_KIND(ir, CCall, CK_IRC_CALL, prssainfo)

//True if this call is intrinsic operation.
#define CALL_is_intrinsic(ir) \
    CK_FLD_KIND(ir, CCall, CK_IRC_CALL,  m_is_intrinsic)

//Record intrinsic operator if CALL_is_intrinsic is true.
#define CALL_intrinsic_op(ir) CK_FLD_KIND(ir, CCall, CK_IRC_CALL, intrinsic_op)

//Call does not necessarily to be BB boundary.
#define CALL_is_not_bb_bound(ir) \
    CK_FLD_KIND(ir, CCall, CK_IRC_CALL, m_is_not_bb_bound)

//True if this call does not modify any memory.
#define CALL_is_readonly(ir) (CALL_idinfo(ir)->is_readonly())

//True if call allocated memory from heap. It always describe functions
//like malloc() or 'new' operator.
#define CALL_is_alloc_heap(ir) \
    CK_FLD_KIND(ir, CCall, CK_IRC_CALL, m_is_alloc_heap)

//Record MD DU information.
#define CALL_du(ir) CK_FLD_KIND(ir, CCall, CK_IRC_CALL, du)

//Parameter list of call.
#define CALL_param_list(ir) CALL_kid(ir, 0)

//Record dummy referenced IR.
#define CALL_dummyuse(ir) CALL_kid(ir, 1)
#define CALL_kid(ir, idx) CK_CALL_KID(ir, CCall, idx)
class CCall : public DuProp, public VarProp, public StmtProp {
    COPY_CONSTRUCTOR(CCall);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 2;
    //True if current call is intrinsic call.
    BYTE m_is_intrinsic:1;

    //True if this call do allocate memory from heap. It always the function
    //like malloc or new.
    BYTE m_is_alloc_heap:1;

    //True if this call does not necessarily to be basic block boundary.
    //By default, call stmt must be down boundary of basic block, but if
    //the flag is true, the call is always be defined by customer for
    //special purpose, e.g, intrinsic call or customized operation.
    BYTE m_is_not_bb_bound:1;

    //Record the intrinsic operation.
    UINT intrinsic_op;

    PRNO prno; //Result PR number if any.

    SSAInfo * prssainfo; //indicates PR ssa def and use set.

    ////////////////////////////////////////////////////////////////////////////
    //NOTE: 'opnd' must be the last member.                                   //
    ////////////////////////////////////////////////////////////////////////////
    IR * opnd[kid_num];
public:
    //Build dummyuse expression to represent potential memory objects that
    //the Call referrenced.
    //Note dummyuse may be a list of IR.
    void addDummyUse(Region * rg);
    static inline Var *& accIdinfo(IR * ir) { return CALL_idinfo(ir); }
    static inline SSAInfo *& accSSAInfo(IR * ir) { return CALL_ssainfo(ir); }
    static inline PRNO & accPrno(IR * ir) { return CALL_prno(ir); }
    static inline StorageSpace & accSS(IR * ir)
    { return CALL_storage_space(ir); }
    static inline IR * accResultPR(IR * ir)
    { return ir->hasReturnValue() ? ir : nullptr; }
    static inline IR *& accKid(IR * ir, UINT idx) { return CALL_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return CALL_bb(ir); }

    IR * getKid(UINT idx) const { return CALL_kid(this, idx); }
    IR * getParamList() const { return CALL_param_list(this); }
    IR * getDummyUse() const { return CALL_dummyuse(this); }
    CHAR const* getCalleeNameString() const
    { return SYM_name(CALL_idinfo(this)->get_name()); }
    //Get the intrinsic operation code.
    UINT getIntrinsicOp()
    {
        ASSERT0(CALL_is_intrinsic(this));
        return CALL_intrinsic_op(this);
    }

    //Return true if current stmt has dummyuse.
    bool hasDummyUse() const { return CALL_dummyuse(this) != nullptr; }

    bool is_intrinsic() const { return CALL_is_intrinsic(this); }
    bool is_readonly() const { return CALL_is_readonly(this); }
    bool isMustBBbound()
    {
        #ifdef _DEBUG_
        if (CALL_is_not_bb_bound(this)) {
            ASSERTN(CALL_is_intrinsic(this),
                    ("normal call stmt must be BB boundary"));
        }
        #endif
        return !CALL_is_not_bb_bound(this);
    }
};



//Represents an indirect function call.
//This class uses macro operations of CCall.
//Expression to compute the target function address.
//NOTE: 'opnd_pad' must be the first member.

//Indicate the callee function pointer.
#define ICALL_callee(ir) (*(((CICall*)ir)->opnd + CK_KID_IRC(ir, IR_ICALL, 2)))

//True if current call is readonly.
#define ICALL_is_readonly(ir) (((CICall*)CK_IRC_ONLY_ICALL(ir))->m_is_readonly)
#define ICALL_kid(ir, idx) (((CICall*)ir)->opnd[CK_KID_IRC(ir, IR_ICALL, idx)])
class CICall : public CCall {
    COPY_CONSTRUCTOR(CICall);
public:
    static BYTE const kid_map = 0x4; //callee must exist
    static BYTE const pad_kid_num = 1;
    static BYTE const kid_num = CCall::kid_num + pad_kid_num;
    //NOTE: 'opnd_pad' must be the first member.
    IR * opnd_pad[pad_kid_num];

    //True if current call is readonly.
    BYTE m_is_readonly:1;
public:
    static inline SSAInfo *& accSSAInfo(IR * ir) { return CALL_ssainfo(ir); }
    static inline PRNO & accPrno(IR * ir) { return CALL_prno(ir); }
    static inline IR * accResultPR(IR * ir)
    { return ir->hasReturnValue() ? ir : nullptr; }
    static inline IR *& accKid(IR * ir, UINT idx) { return ICALL_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return CALL_bb(ir); }

    IR * getKid(UINT idx) const { return ICALL_kid(this, idx); }
    IR * getCallee() const { return ICALL_callee(this); }

    bool is_readonly() const { return ICALL_is_readonly(this); }
};


//Binary Operation, includes ADD, SUB, MUL, DIV, REM, MOD,
//LAND, LOR, BAND, BOR, XOR, LT, LE, GT, GE, EQ, NE, ASR, LSR, LSL.
#define BIN_opnd0(ir) BIN_kid(ir, 0)
#define BIN_opnd1(ir) BIN_kid(ir, 1)
#define BIN_kid(ir, idx) (((CBin*)ir)->opnd[CK_KID_BIN(ir, idx)])
class CBin : public IR {
    COPY_CONSTRUCTOR(CBin);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return BIN_kid(ir, idx); }

    IR * getKid(UINT idx) const { return BIN_kid(this, idx); }
    IR * getOpnd0() const { return BIN_opnd0(this); }
    IR * getOpnd1() const { return BIN_opnd1(this); }
};


//Unary Operation, includes NEG, BNOT, LNOT.
#define UNA_opnd(ir) UNA_kid(ir, 0)
#define UNA_kid(ir, idx) (((CUna*)ir)->opnd[CK_KID_UNA(ir, idx)])
class CUna : public IR {
    COPY_CONSTRUCTOR(CUna);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return UNA_kid(ir, idx); }
    IR * getKid(UINT idx) const { return UNA_kid(this, idx); }
    IR * getOpnd() const { return UNA_opnd(this); }
};


//This class represents goto operation, unconditional jump to target label.
#define GOTO_bb(ir) (((CGoto*)CK_IRC(ir, IR_GOTO))->bb)
#define GOTO_lab(ir) (((CGoto*)CK_IRC(ir, IR_GOTO))->jump_target_lab)
class CGoto : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CGoto);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0;
    LabelInfo const* jump_target_lab;
public:
    static inline IRBB *& accBB(IR * ir) { return GOTO_bb(ir); }
    static inline LabelInfo const*& accLab(IR * ir) { return GOTO_lab(ir); }
    LabelInfo const* getLab() const { return GOTO_lab(this); }
};


//This class represents indirect goto operation,
//the control flow will unconditional jump to one target label of a list of
//label which determined by value-exp.
//usage: igoto (value-exp) case_list.
#define IGOTO_bb(ir) (((CIGoto*)CK_IRC(ir, IR_IGOTO))->bb)

//Value expression.
#define IGOTO_vexp(ir) IGOTO_kid(ir, 0)

//Record a list pairs of <case-value, jump label>.
#define IGOTO_case_list(ir) IGOTO_kid(ir, 1)

#define IGOTO_kid(ir, idx) (((CIGoto*)ir)->opnd[CK_KID_IRC(ir, IR_IGOTO, idx)])
class CIGoto : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CIGoto);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return IGOTO_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return IGOTO_bb(ir); }

    //The function collects the LabelInfo for each branch-target.
    void collectLabel(OUT List<LabelInfo const*> & lst) const;
    IR * getCaseList() const { return IGOTO_case_list(this); }
};


//High level control loop operation.
//usage:
//    while (det) {
//      body
//    }
//NOTE:
//    * The member layout should be same as do_while.
//    * 'opnd' must be the last member of CWhileDo.
//Determinate expression. It can NOT be nullptr.
#define LOOP_det(ir) LOOP_kid(ir, 0)

//Record stmt list in loop body of IF. It can be nullptr.
#define LOOP_body(ir) LOOP_kid(ir, 1)
#define LOOP_kid(ir, idx) (((CWhileDo*)ir)->opnd[CK_KID_LOOP(ir, idx)])
class CWhileDo : public IR {
    COPY_CONSTRUCTOR(CWhileDo);
public:
    static BYTE const kid_map = 0x1; //det must exist
    static BYTE const kid_num = 2;
    ////////////////////////////////////////////////////////////////////////////
    //NOTE: 'opnd' must be the last member of CWhileDo.                       //
    ////////////////////////////////////////////////////////////////////////////
    IR * opnd[kid_num];
public:
    //num: the number of IR added.
    void addToBody(UINT num, ...);
    static inline IR *& accKid(IR * ir, UINT idx) { return LOOP_kid(ir, idx); }
    static inline IR *& accDet(IR * ir) { return LOOP_det(ir); }

    IR * getBody() const { return LOOP_body(this); }
};


//High level control loop operation.
//usage:
//    do {
//      body
//    } while (det)
class CDoWhile : public CWhileDo {
    COPY_CONSTRUCTOR(CDoWhile);
public:
    static BYTE const kid_map = 0x1; //det must exist
    static BYTE const kid_num = 2;
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return LOOP_kid(ir, idx); }
    static inline IR *& accDet(IR * ir) { return LOOP_det(ir); }
};


//High level control loop operation.
//This structure represents a kind of loop with
//plainly definition of INIT(low bound), DET(HIGH bound),
//LOOP-BODY and STEP(Increment or Dcrement) of induction variable.
//e.g1:
//    do
//      ivr: id i
//      init: 0
//      det: i <= 10
//      step: i+1
//      body {stmt_list}
//    enddo
//e.g2:
//    do
//      ivr: $1
//      init: 0
//      det: $1 <= 10
//      step: $1+1
//      body {stmt_list}
//    enddo
//This class uses LOOP_det access its determinate expression,
//and LOOP_body access loop body.
//NOTE: 'opnd_pad' must be the first member of CDoLoop.

//Record the induction variable.
//There is only one basic induction variable for do-loop.
#define LOOP_iv(ir) (*(((CDoLoop*)ir)->opnd + CK_KID_IRC(ir, IR_DO_LOOP, 2)))

//Record the expression that initialize induction variable.
#define LOOP_init(ir) (*(((CDoLoop*)ir)->opnd + CK_KID_IRC(ir, IR_DO_LOOP, 3)))

//Record the expression that update induction variable.
#define LOOP_step(ir) (*(((CDoLoop*)ir)->opnd + CK_KID_IRC(ir, IR_DO_LOOP, 4)))
#define DOLOOP_kid(ir, idx) \
    (((CDoLoop*)ir)->opnd[CK_KID_IRC(ir, IR_DO_LOOP, idx)])
class CDoLoop : public CWhileDo {
    COPY_CONSTRUCTOR(CDoLoop);
public:
    static BYTE const kid_map = 0x1; //det must exist
    static BYTE const pad_kid_num = 3;
    static BYTE const kid_num = CWhileDo::kid_num + pad_kid_num;
    //NOTE: 'opnd_pad' must be the first member of CDoLoop.
    IR * opnd_pad[pad_kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return DOLOOP_kid(ir, idx); }
    static inline IR *& accDet(IR * ir) { return LOOP_det(ir); }

    IR * getIV() const { return LOOP_iv(this); }
    IR * getInit() const { return LOOP_init(this); }
    IR * getStep() const { return LOOP_step(this); }
};


//This class represents high level control IF operation.
//usage:
//    if (det)
//      truebody
//      falsebody
//    endif
//Determinate expression. It can NOT be nullptr.
#define IF_det(ir) IF_kid(ir, 0)

//Record stmt list in true body of IF. It can be nullptr.
#define IF_truebody(ir) IF_kid(ir, 1)

//Record stmt list in false body of IF. It can be nullptr.
#define IF_falsebody(ir) IF_kid(ir, 2)
#define IF_kid(ir, idx) (((CIf*)ir)->opnd[CK_KID_IRC(ir, IR_IF, idx)])
class CIf : public IR {
    COPY_CONSTRUCTOR(CIf);
public:
    static BYTE const kid_map = 0x1; //det must exist
    static BYTE const kid_num = 3;
    IR * opnd[kid_num];
public:
    //num: the number of IR added.
    void addToTrueBody(UINT num, ...);
    //num: the number of IR added.
    void addToFalseBody(UINT num, ...);
    static inline IR *& accKid(IR * ir, UINT idx) { return IF_kid(ir, idx); }
    static inline IR *& accDet(IR * ir) { return IF_det(ir); }

    IR * getKid(UINT idx) const { return IF_kid(this, idx); }
    IR * getDet() const { return IF_det(this); }
    IR * getTrueBody() const { return IF_truebody(this); }
    IR * getFalseBody() const { return IF_falsebody(this); }
};


//This class represents internal and customer defined label.
#define LAB_lab(ir) (((CLab*)CK_IRC(ir, IR_LABEL))->label_info)
class CLab : public IR {
    COPY_CONSTRUCTOR(CLab);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0;
    LabelInfo const* label_info;
public:
    static inline LabelInfo const*& accLab(IR * ir) { return LAB_lab(ir); }
    LabelInfo const* getLab() const { return LAB_lab(this); }
};


//This class represents high and middle level control flow switch operation.
//usage:
//    switch (value-exp)
//    case_list
//    body
//    endswitch
#define SWITCH_bb(ir) (((CSwitch*)CK_IRC(ir, IR_SWITCH))->bb)

//Default label.
//This is a label repesent the default jump target of IR_SWITCH.
//The label is optional.
//If there are not any cases matched, the control flow will jump to
//the default label.
#define SWITCH_deflab(ir) (((CSwitch*)CK_IRC(ir, IR_SWITCH))->default_label)

//Value expression.
#define SWITCH_vexp(ir) SWITCH_kid(ir, 0)

//Switch body.
#define SWITCH_body(ir) SWITCH_kid(ir, 1)

//Record a list pair of <case-value, jump label>.
#define SWITCH_case_list(ir) SWITCH_kid(ir, 2)

#define SWITCH_kid(ir, idx)  \
    (((CSwitch*)ir)->opnd[CK_KID_IRC(ir, IR_SWITCH, idx)])
class CSwitch : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CSwitch);
public:
    static BYTE const kid_map = 0x1; //value expression must exist
    static BYTE const kid_num = 3;
    IR * opnd[kid_num];
    LabelInfo const* default_label;
public:
    //num: the number of IR added.
    void addToBody(UINT num, ...);
    static inline IR *& accKid(IR * ir, UINT idx)
    { return SWITCH_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return SWITCH_bb(ir); }
    static inline LabelInfo const*& accLab(IR * ir)
    { return SWITCH_deflab(ir); }

    //The function collects the LabelInfo for each branch-target.
    //Note the default-label is collected too.
    void collectLabel(OUT List<LabelInfo const*> & lst) const;

    LabelInfo const* getDefLab() const { return SWITCH_deflab(this); }
    IR * getKid(UINT idx) const { return SWITCH_kid(this, idx); }
    IR * getValExp() const { return SWITCH_vexp(this); }
    IR * getBody() const { return SWITCH_body(this); }
    IR * getCaseList() const { return SWITCH_case_list(this); }
};


//This class represents the case value expression and its jump target label.
//NOTE: this class is used only by SWITCH and IGOTO.
#define CASE_lab(ir) (((CCase*)CK_IRC(ir, IR_CASE))->jump_target_label)

//Value expression.
#define CASE_vexp(ir) CASE_kid(ir, 0)
#define CASE_kid(ir, idx) (((CCase*)ir)->opnd[CK_KID_IRC(ir, IR_CASE, idx)])
class CCase : public IR {
    COPY_CONSTRUCTOR(CCase);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    IR * opnd[kid_num]; //case-value
    LabelInfo const* jump_target_label; //jump lable for case.
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return CASE_kid(ir, idx); }
    static inline LabelInfo const*& accLab(IR * ir) { return CASE_lab(ir); }

    LabelInfo const* getLab() const { return CASE_lab(this); }
    IR * getValExp() const { return CASE_vexp(this); }
    IR * getKid(UINT idx) const { return CASE_kid(this, idx); }
};


//This class represents array operation.
//Base of array can be LDA, or other computational expression.
//This operation do not perform any array bound diagnosis.
//If array base is LDA, it denotes that the array's base is a variable with
//array type,
//e.g: char p[N]; (&p)[i] = ...
//
//If array base is computational expression, it denotes that the array's
//base is pointer, and the pointer points to an array.
//e.g: char * p; (p+1)[i] = ...
//
//ARR_elem_ty represents the type of array element. Moreover, element may be
//array as well.
#define ARR_elemtype(ir) (((CArray*)CK_IRC_ARR(ir))->elemtype)
#define ARR_ofst(ir) (((CArray*)CK_IRC_ARR(ir))->field_offset)
#define ARR_du(ir) (((CArray*)CK_IRC_ARR(ir))->du)
#define ARR_align(ir) (((CArray*)CK_IRC_ARR(ir))->alignment)
#define ARR_is_aligned(ir) (((CArray*)CK_IRC_ARR(ir))->is_aligned)

//Get the number of element in each dimension.
//ARR_elem_num represents the number of array element in current dimension.
//Note the lowest dimension, which iterates most fastly, is at the 0th position
//in 'ARR_elem_num_buf'.
//e.g: Given array D_I32 A[10][20], the 0th dimension is the lowest dimension,
//it has 20 elements, each element has type D_I32;
//the 1th dimension has 10 elements, each element has type [D_I32*20], and
//the ARR_elem_num_buf will be [10, 20], the lowest dimension at the position 0
//in the buffer.
//Note that if the ARR_elem_num of a dimension is 0, that means we can not
//determine the number of element at the dimension.
#define ARR_elem_num(ir, dim) \
    (((CArray*)CK_IRC_ARR(ir))->elem_num[CK_ARRAY_DIM(ir, dim)])
#define ARR_elem_num_buf(ir) (((CArray*)CK_IRC_ARR(ir))->elem_num)

//Array subscript expression list.
//If we have an array accessing, such as A[i][j], the ARR_sub_list will be
//ld(j)->ld(i), the first expression represents the accessing of the lowest
//dimension. By given the example, if ARR_elem_num is [10, 20].
//the final accessing address will be lda(A) + (20 * i + j) * sizeof(D_I32).
#define ARR_sub_list(ir) ARR_kid(ir, 1)

//Array base expression.
#define ARR_base(ir) ARR_kid(ir, 0)

//Return the storage space if any.
#define ARR_storage_space(ir) (((CArray*)CK_IRC_ARR(ir))->storage_space)

#define ARR_kid(ir, idx) (((CArray*)ir)->opnd[CK_KID_ARR(ir, idx)])
class CArray : public DuProp, public OffsetProp {
    COPY_CONSTRUCTOR(CArray);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    UINT alignment;

    bool is_aligned;

    //Note that if ARR_ofst is not zero, the IR_dt may not equal to
    //ARR_elemtype. IR_dt describe the data-type of ARRAY operation + ARR_ofst.
    //ARR_elemtype describe array element type.
    //
    //e.g: struct {int a, b; } s[100];
    //     ... = s[2].b;
    //
    //data-type of array operation is D_I32, because ARR_ofst is 4,
    //that means we are taking the value of second field of struct, meanwhile
    //data-type of array element is D_MC, size is 8, (struct {int a, b;}).
    Type const* elemtype; //record data-type of array element.

    //Record the number of array element for each dimension.
    //Note that the elem_num buffer can NOT be modified
    //after it is created.
    TMWORD const* elem_num;

    //Record the storage space if any.
    StorageSpace storage_space;

    ////////////////////////////////////////////////////////////////////////////
    //NOTE: 'opnd' must be the last member of CArray.                         //
    ////////////////////////////////////////////////////////////////////////////
    IR * opnd[kid_num];
public:
    static inline TMWORD & accOfst(IR * ir) { return ARR_ofst(ir); }
    static inline IR *& accKid(IR * ir, UINT idx) { return ARR_kid(ir, idx); }
    static inline IR *& accBase(IR * ir) { return ARR_base(ir); }
    static inline StorageSpace & accSS(IR * ir)
    { return ARR_storage_space(ir); }

    static IR * dupIRTreeByStmt(IR const* src, Region * rg);

    //Return the number of dimensions.
    UINT getDimNum() const
    {
        ASSERT0(isArrayOp());
        return xcom::cnt_list(ARR_sub_list(this));
    }

    //Return the number of element of the lowest dimension.
    TMWORD getElementNumOfLowestDim() const { return getElementNumOfDim(0); }

    //Return the number of element of the hightest dimension.
    TMWORD getElementNumOfHighestDim() const
    {
        ASSERT0(getDimNum() > 0);
        return getElementNumOfDim(getDimNum() - 1);
    }

    //Return the subscript expression of the lowest dimension.
    IR * getSubExpOfLowestDim() const { return getSubList(); }

    //Return the subscript expression of the highest dimension.
    IR * getSubExpOfHighestDim() const { return xcom::get_last(getSubList()); }

    //Return the number of element in given dimension.
    TMWORD getElementNumOfDim(UINT dimension) const
    {
        ASSERT0(ARR_elem_num_buf(this));
        return ARR_elem_num(this, dimension);
    }
    TMWORD const* getElemNumBuf() const { return ARR_elem_num_buf(this); }
    Type const* getElemType() const { return ARR_elemtype(this); }
    IR * getKid(UINT idx) const { return ARR_kid(this, idx); }
    IR * getBase() const { return ARR_base(this); }
    IR * getSubList() const { return ARR_sub_list(this); }

    //Return true if exp is array base.
    bool is_base(IR const* exp) const { return exp == ARR_base(this); }

    //Return true if exp is array subscript expression list.
    bool isInSubList(IR const* exp) const
    {
        for (IR const* s = ARR_sub_list(this);
             s != nullptr; s = s->get_next()) {
            if (s == exp || s->is_kids(exp)) { return true; }
        }
        return false;
    }
};


//This class represents the operation storing value to array.
//The most operations and properties are same as CArray.
//
//Base of array can be LDA, or other computational expression.
//This operation do not perform any array bound diagnosis.
//
//If array base is IR_LDA, it denotes that the array's base is variable with
//array type,
//    e.g: char p[N]; (&p)[i] = ...
//
//If array base is computational expression, it denotes that the array's
//base is pointer, and the pointer point to an array.
//    e.g: char * p; (p+1)[i] = ...
//
//'elem_ty' represents the type of array element.
//Moreover, element may be also an array as well.
//
//'elem_num' represents the number of array element in given dimension.
//
#define STARR_bb(ir) (((CStArray*)CK_IRC(ir, IR_STARRAY))->stmtprop.bb)
#define STARR_rhs(ir) \
    (*(((CStArray*)ir)->opnd_pad + CK_KID_IRC(ir, IR_STARRAY, 0)))
#define STARR_base(ir) ARR_base(ir)
#define STARR_sub_list(ir) ARR_sub_list(ir)
#define STARR_elem_type(ir) ARR_elem_type(ir)
#define STARR_ofst(ir) ARR_ofst(ir)
#define STARR_align(ir) ARR_align(ir)
#define STARR_is_aligned(ir) ARR_is_aligned(ir)
#define STARR_du(ir) ARR_du(ir)
#define STARR_storage_space(ir) ARR_storage_space(ir)
#define STARR_elem_num(ir, dim) ARR_elem_num(ir, dim)
#define STARR_elem_num_buf(ir) ARR_elem_num_buf(ir)
class CStArray : public CArray {
    COPY_CONSTRUCTOR(CStArray);
public:
    static BYTE const kid_map = 0x7;
    static BYTE const pad_kid_num = 1;
    static BYTE const kid_num = CArray::kid_num + pad_kid_num;
    //NOTE: 'opnd_pad' must be the first member of CStArray.
    IR * opnd_pad[pad_kid_num];

    //DO NOT PLACE MEMBER BEFORE opnd_pad
    StmtProp stmtprop;
public:
    static inline IR *& accRHS(IR * ir) { return STARR_rhs(ir); }
    static inline TMWORD & accOfst(IR * ir) { return ARR_ofst(ir); }
    static inline IR *& accKid(IR * ir, UINT idx) { return ARR_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return STARR_bb(ir); }
    static inline IR *& accBase(IR * ir) { return ARR_base(ir); }
    static inline StorageSpace & accSS(IR * ir)
    { return STARR_storage_space(ir); }

    static IR * dupIRTreeByExp(IR const* src, IR * rhs, Region * rg);
};


//This class represents data-type convertion.
//Record the expression to be converted.
#define CVT_exp(ir) (UNA_opnd(ir))
#define CVT_leaf_exp(ir) (((CCvt*)ir)->getLeafExp())
#define CVT_kid(ir, idx) (UNA_kid(ir, idx))
#define CVT_round(ir) (((CCvt*)ir)->round)
class CCvt : public CUna {
    COPY_CONSTRUCTOR(CCvt);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    ROUND_TYPE round;
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return CVT_kid(ir, idx); }

    //Get the leaf expression.
    //e.g: cvt:i32(cvt:u8(x)), this function will return x;
    IR * getLeafExp()
    {
        ASSERT0(getCode() == IR_CVT);
        IR * v;
        for (v = this; v->getCode() == IR_CVT; v = CVT_exp(v)) {;}
        ASSERT0(v);
        return v;
    }
    IR * getExp() const { return CVT_exp(this); }
    IR * getKid(UINT idx) const { return CVT_kid(this, idx); }
    ROUND_TYPE getRoundType() const { return CVT_round(this); }
};


//This class represents temporary memory location which named pseudo register.
//It can be used to indicate the Region live-in register. In this case,
//a PR may not have a definition.
//NOTE:
//    1.PR can not be taken address.
//    2.PR is always allocate on stack.
#define PR_no(ir) (((CPr*)CK_IRC(ir, IR_PR))->prno)
#define PR_ssainfo(ir) (((CPr*)CK_IRC(ir, IR_PR))->ssainfo)
#define PR_du(ir) (((CPr*)CK_IRC(ir, IR_PR))->du)
class CPr : public DuProp {
    COPY_CONSTRUCTOR(CPr);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0;
    PRNO prno; //PR number.

    //versioned presentation or ssa def and use list in ssa mode.
    //Note this field only avaiable if SSA information is maintained.
    SSAInfo * ssainfo;
public:
    static inline SSAInfo *& accSSAInfo(IR * ir) { return PR_ssainfo(ir); }
    static inline PRNO & accPrno(IR * ir) { return PR_no(ir); }

    //src:can be stmt or expression.
    static IR * dupIRTreeByRef(IR const* src, Region * rg);
};


//This class represents true branch operation.
//Branch if determinant express is true, otherwise control flow does not change.

//NOTE: the lay out of truebr should same as falsebr.
#define BR_bb(ir) (((CTruebr*)CK_IRC_BR(ir))->bb)
#define BR_lab(ir) (((CTruebr*)CK_IRC_BR(ir))->jump_target_lab)

//Determinate expression. It can NOT be nullptr.
#define BR_det(ir) BR_kid(ir, 0)
#define BR_kid(ir, idx) (((CTruebr*)ir)->opnd[CK_KID_BR(ir, idx)])
class CTruebr : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CTruebr);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    IR * opnd[kid_num];
    LabelInfo const* jump_target_lab; //jump target label.
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return BR_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return BR_bb(ir); }
    static inline LabelInfo const*& accLab(IR * ir) { return BR_lab(ir); }
    static inline IR *& accDet(IR * ir) { return BR_det(ir); }

    LabelInfo const* getLab() const { return BR_lab(this); }
    IR * getKid(UINT idx) const { return BR_kid(this, idx); }
    IR * getDet() const { return BR_det(this); }
};


//This class represents false branch operation.
//Branch if determinant express is false,
//otherwise control flow does not change.
//Also use BR_det, BR_lab access.
//NOTE: the lay out of truebr should same as falsebr.
class CFalsebr : public CTruebr {
    COPY_CONSTRUCTOR(CFalsebr);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return BR_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return BR_bb(ir); }
    static inline LabelInfo const*& accLab(IR * ir) { return BR_lab(ir); }
    static inline IR *& accDet(IR * ir) { return BR_det(ir); }
};


//This class represents function return operation.
//Return value expressions.
//usage: return a;  a is return-value expression.
#define RET_bb(ir) (((CRet*)CK_IRC(ir, IR_RETURN))->bb)
#define RET_exp(ir) RET_kid(ir, 0)
#define RET_kid(ir, idx) (((CRet*)ir)->opnd[CK_KID_IRC(ir, IR_RETURN, idx)])
class CRet : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CRet);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 1;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return RET_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return RET_bb(ir); }

    IR * getKid(UINT idx) const { return RET_kid(this, idx); }
    IR * getExp() const { return RET_exp(this); }
};


//This class represents conditional select operation.
//usage: res = select(a > b), (10), (20)
//    means:
//    if (a > b) res = 10;
//    else res = 20;
//  where a > b is predicator expression.
//This operation compute the value accroding to the result of
//predicator expression, if the result value is true, return
//SELECT_trueexp, otherwise return SELECT_falseexp.

//Predicator expression.
#define SELECT_det(ir) SELECT_kid(ir, 0)

//True part
#define SELECT_trueexp(ir) SELECT_kid(ir, 1)

//False part
#define SELECT_falseexp(ir) SELECT_kid(ir, 2)
#define SELECT_kid(ir, idx) \
    (((CSelect*)ir)->opnd[CK_KID_IRC(ir, IR_SELECT, idx)])
class CSelect : public IR {
    COPY_CONSTRUCTOR(CSelect);
public:
    static BYTE const kid_map = 0x7;
    static BYTE const kid_num = 3;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return SELECT_kid(ir, idx); }
    static inline IR *& accDet(IR * ir) { return SELECT_det(ir); }

    IR * getKid(UINT idx) const { return SELECT_kid(this, idx); }
    IR * getPred() const { return SELECT_det(this); }
    IR * getTrueExp() const { return SELECT_trueexp(this); }
    IR * getFalseExp() const { return SELECT_falseexp(this); }
};


//This class represents high level control structure, that
//terminate current loop execution immediately without any
//other operations.
//This operation is used by do-loop, do-while, while-do.
class CBreak : public IR {
    COPY_CONSTRUCTOR(CBreak);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0;
};


//This class represents high level control structure, that
//re-execute current loop immediately without any
//other operations.
//This operation is used by do-loop, do-while, while-do.
class CContinue : public IR {
    COPY_CONSTRUCTOR(CContinue);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0;
};


//This class represents phi operation.
#define PHI_bb(ir) (((CPhi*)CK_IRC(ir, IR_PHI))->bb)
#define PHI_prno(ir) (((CPhi*)CK_IRC(ir, IR_PHI))->prno)
#define PHI_ssainfo(ir) (((CPhi*)CK_IRC(ir, IR_PHI))->ssainfo)
#define PHI_opnd_list(ir) PHI_kid(ir, 0)
#define PHI_kid(ir, idx) (((CPhi*)ir)->opnd[CK_KID_IRC(ir, IR_PHI, idx)])
class CPhi : public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CPhi);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    PRNO prno; //PR number.
    SSAInfo * ssainfo; //Present ssa def and use set.
    IR * opnd[kid_num];
public:
    static inline SSAInfo *& accSSAInfo(IR * ir) { return PHI_ssainfo(ir); }
    static inline PRNO & accPrno(IR * ir) { return PHI_prno(ir); }
    static inline IR * accResultPR(IR * ir) { return ir; }
    static inline IR *& accKid(IR * ir, UINT idx) { return PHI_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return PHI_bb(ir); }

    void removeOpnd(IR * ir)
    {
        ASSERT0(xcom::in_list(PHI_opnd_list(this), ir));
        xcom::remove(&PHI_opnd_list(this), ir);
    }

    //Add opnd to the tail of operand list.
    //The opnd must correspond to the last predecessor
    //of BB that current phi located in.
    void addOpnd(IR * ir)
    {
        ASSERT0(!xcom::in_list(PHI_opnd_list(this), ir));
        xcom::add_next(&PHI_opnd_list(this), ir);
        IR_parent(ir) = this;
    }

    //src:can be stmt or expression.
    static IR * dupIRTreeByRef(IR const* src, Region * rg);

    IR * getKid(UINT idx) const { return PHI_kid(this, idx); }
    //Get the No.idx operand.
    IR * getOpnd(UINT idx) const;
    IR * getOpndList() const { return PHI_opnd_list(this); }
    UINT getOpndNum() const { return xcom::cnt_list(PHI_opnd_list(this)); }

    void insertOpndBefore(IR * marker, IR * exp)
    {
        ASSERT0(xcom::in_list(PHI_opnd_list(this), marker));
        ASSERT0(!xcom::in_list(PHI_opnd_list(this), exp));
        xcom::insertbefore(&PHI_opnd_list(this), marker, exp);
        IR_parent(exp) = this;
    }
    void insertOpndAfter(IR * marker, IR * exp)
    {
        ASSERT0(marker && exp);
        ASSERT0(xcom::in_list(PHI_opnd_list(this), marker));
        ASSERT0(!xcom::in_list(PHI_opnd_list(this), exp));
        xcom::insertafter(&marker, exp);
        IR_parent(exp) = this;
    }
    void insertOpndAt(UINT pos, IR * exp);
};


//This class represents region operation.
//NOTE: If region is in BB, it must be single entry, single exit, since
//it might be reduced from reducible graph.
#define REGION_bb(ir) (((CRegion*)CK_IRC(ir, IR_REGION))->bb)
#define REGION_ru(ir) (((CRegion*)CK_IRC(ir, IR_REGION))->rg)
class CRegion : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CRegion);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0;
    Region * rg;
public:
    static inline IRBB *& accBB(IR * ir) { return REGION_bb(ir); }

    //True if region is readonly.
    //This property is very useful if region is a blackbox.
    //And readonly region will alleviate the burden of optimizor.
    bool is_readonly() const;

    MDSet * getMayDef() const;
    MDSet * getMayUse() const;
    MDSet * genMayDef() const;
    MDSet * genMayUse() const;
    Region * getRegion() const { return REGION_ru(this); }
};

bool checkIRCodeBitSize();
bool checkMaxIRCode();

} //namespace xoc

#endif
