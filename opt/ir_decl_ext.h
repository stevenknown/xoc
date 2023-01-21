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
#ifndef _IR_DECL_EXT_H_
#define _IR_DECL_EXT_H_

namespace xoc {

#define PRED_PR_IDX 0

#define VSTPR_bb(ir) (((CVirStpr*)CK_IRC(ir, IR_VSTPR))->bb)
#define VSTPR_no(ir) (((CVirStpr*)CK_IRC(ir, IR_VSTPR))->prno)
#define VSTPR_ssainfo(ir) (((CVirStpr*)CK_IRC(ir, IR_VSTPR))->ssainfo)
#define VSTPR_du(ir) (((CVirStpr*)CK_IRC(ir, IR_VSTPR))->du)
#define VSTPR_kid(ir, idx) \
    (((CVirStpr*)ir)->opnd[CK_KID_IRC(ir, IR_VSTPR, idx)])
//Represents the RHS of defined PR, thus it must be PR, which indicates that
//RHS defined LHS. If LHS and RHS's physical registers are different, it will
//certainly be converted to MOVE operation, e.g:stpr<-pr.
#define VSTPR_rhs(ir) VSTPR_kid(ir, 0)

//Represents a dummy USE of PR that is used to maintain DU chain.
#define VSTPR_dummyuse(ir) VSTPR_kid(ir, 1)
class CVirStpr : public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CVirStpr);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 2;
    PRNO prno;
    SSAInfo * ssainfo;
    IR * opnd[kid_num];
public:
    static inline IR *& accRHS(IR * ir) { return VSTPR_rhs(ir); }
    static inline SSAInfo *& accSSAInfo(IR * ir) { return VSTPR_ssainfo(ir); }
    static inline PRNO & accPrno(IR * ir) { return VSTPR_no(ir); }
    static inline IR * accResultPR(IR * ir) { return ir; }
    static inline IR *& accKid(IR * ir, UINT idx) { return VSTPR_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return VSTPR_bb(ir); }
};


#define VST_bb(ir) (((CVirSt*)CK_IRC(ir, IR_VST))->bb)
#define VST_idinfo(ir) (((CVirSt*)CK_IRC(ir, IR_VST))->id_info)
#define VST_ofst(ir) (((CVirSt*)CK_IRC(ir, IR_VST))->field_offset)
#define VST_du(ir) (((CVirSt*)CK_IRC(ir, IR_VST))->du)
#define VST_rhs(ir) VST_kid(ir, 0)
#define VST_dummyuse(ir) VST_kid(ir, 1)
#define VST_kid(ir, idx) (((CVirSt*)ir)->opnd[CK_KID_IRC(ir, IR_VST, idx)])
class CVirSt : public CLd, public StmtProp {
    COPY_CONSTRUCTOR(CVirSt);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 2;
    IR * opnd[kid_num];
public:
    static inline IR *& accRHS(IR * ir) { return VST_rhs(ir); }
    static inline Var *& accIdinfo(IR * ir) { return VST_idinfo(ir); }
    static inline TMWORD & accOfst(IR * ir) { return VST_ofst(ir); }
    static inline IR *& accKid(IR * ir, UINT idx) { return VST_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return VST_bb(ir); }
};


//This class represents an indirect memory operation that is used to describe
//multiple memory locations. Some target machine instruction will write
//multiple memory simultaneously, e.g:picture compress operation.
#define VIST_bb(ir) (((CVirISt*)CK_IRC(ir, IR_VIST))->bb)
#define VIST_ofst(ir) (((CVirISt*)CK_IRC(ir, IR_VIST))->field_offset)
#define VIST_du(ir) (((CVirISt*)CK_IRC(ir, IR_VIST))->du)
#define VIST_base(ir) VIST_kid(ir, 0)
#define VIST_rhs(ir) VIST_kid(ir, 1)
#define VIST_dummyuse(ir) VIST_kid(ir, 2)
#define VIST_kid(ir, idx) (((CVirISt*)ir)->opnd[CK_KID_IRC(ir, IR_VIST, idx)])
class CVirISt : public DuProp, public OffsetProp, public StmtProp {
    COPY_CONSTRUCTOR(CVirISt);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 3;
    IR * opnd[kid_num];
public:
    static inline IR *& accRHS(IR * ir) { return VIST_rhs(ir); }
    static inline TMWORD & accOfst(IR * ir) { return VIST_ofst(ir); }
    static inline IR *& accKid(IR * ir, UINT idx) { return VIST_kid(ir, idx); }
    static inline IRBB *& accBB(IR * ir) { return VIST_bb(ir); }
    static inline IR *& accBase(IR * ir) { return VIST_base(ir); }
};


//This class represents broadcast operation that is used to dispatch value in
//'src' to multiple results in 'res_list'.
#define BROADCAST_src(ir) BROADCAST_kid(ir, 0)
#define BROADCAST_alter_res_desc_list(ir) BROADCAST_kid(ir, 1)
#define BROADCAST_kid(ir, idx) \
    (((CBroadCast*)ir)->opnd[CK_KID_IRC(ir, IR_BROADCAST, idx)])
class CBroadCast : public IR {
    COPY_CONSTRUCTOR(CBroadCast);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return BROADCAST_kid(ir, idx); }
};


//This class represents conjunction operation.
//This operation gather byte value of opnd0, opnd1 from specific byte position,
//and combine two part of value into result with double size of data type
//of opnd0.
#define CONJ_pos(ir) CONJ_kid(ir, 0)

//Operand0
#define CONJ_opnd0(ir) CONJ_kid(ir, 1)

//Operand1
#define CONJ_opnd1(ir) CONJ_kid(ir, 2)
#define CONJ_kid(ir, idx) (((CConj*)ir)->opnd[CK_KID_IRC(ir, IR_CONJ, idx)])
class CConj : public IR {
    COPY_CONSTRUCTOR(CConj);
public:
    static BYTE const kid_map = 0x7;
    static BYTE const kid_num = 3;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx) { return CONJ_kid(ir, idx); }

    IR * getKid(UINT idx) const { return CONJ_kid(this, idx); }
    IR * getPos() const { return CONJ_pos(this); }
};


//This class represents insertion operation.
#define INSERT_pos(ir) INSERT_kid(ir, 0)

//Operand0
#define INSERT_opnd0(ir) INSERT_kid(ir, 1)

//Operand1
#define INSERT_opnd1(ir) INSERT_kid(ir, 2)
#define INSERT_kid(ir, idx) \
    (((CInsert*)ir)->opnd[CK_KID_IRC(ir, IR_INSERT, idx)])
class CInsert : public IR {
    COPY_CONSTRUCTOR(CInsert);
public:
    static BYTE const kid_map = 0x7;
    static BYTE const kid_num = 3;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return INSERT_kid(ir, idx); }

    IR * getKid(UINT idx) const { return INSERT_kid(this, idx); }
    IR * getPos() const { return INSERT_pos(this); }
};


//This class represents extraction operation.
#define EXTRACT_pos(ir) EXTRACT_kid(ir, 0)

//Operand
#define EXTRACT_opnd(ir) EXTRACT_kid(ir, 1)

#define EXTRACT_kid(ir, idx) \
    (((CExtract*)ir)->opnd[CK_KID_IRC(ir, IR_EXTRACT, idx)])
class CExtract : public IR {
    COPY_CONSTRUCTOR(CExtract);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return EXTRACT_kid(ir, idx); }

    IR * getKid(UINT idx) const { return EXTRACT_kid(this, idx); }
    IR * getPos() const { return EXTRACT_pos(this); }
};


//This class represents matrix multiplication operation.
//e.g: matrixmul.type in, weight, row_in, col_in, row_weight, col_weight, accum;
//Input Data
#define MATMUL_in(ir) MATMUL_kid(ir, 0)

//Weight Data
#define MATMUL_weight(ir) MATMUL_kid(ir, 1)

//The row of input data.
#define MATMUL_row_in(ir) MATMUL_kid(ir, 2)

//The column of input data.
#define MATMUL_col_in(ir) MATMUL_kid(ir, 3)

//The row of weight data.
#define MATMUL_row_weight(ir) MATMUL_kid(ir, 4)

//The column of weight data.
#define MATMUL_col_weight(ir) MATMUL_kid(ir, 5)

//The accumulate flag.
//If it is true, the matrix-mul operation will accumulate the result
//into original data, says, res += in * weight, otherwise res = in * weight.
#define MATMUL_accum(ir) MATMUL_kid(ir, 6)

#define MATMUL_kid(ir, idx) \
    (((CMatMul*)ir)->opnd[CK_KID_IRC(ir, IR_MATMUL, idx)])
class CMatMul : public IR {
    COPY_CONSTRUCTOR(CMatMul);
public:
    static BYTE const kid_map = 0x7F;
    static BYTE const kid_num = 7;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return MATMUL_kid(ir, idx); }

    IR * getKid(UINT idx) const { return MATMUL_kid(this, idx); }
};


//This class represents broadcast operation.
#define BRDCAST_src(ir) MATMUL_kid(ir, 0)
#define BRDCAST_mode(ir) (((CBrdCast*)CK_IRC(ir, IR_BRDCAST))->mode)
#define BRDCAST_is_get(ir) (((CBrdCast*)CK_IRC(ir, IR_BRDCAST))->is_get_op)
#define BRDCAST_kid(ir, idx) \
    (((CBrdCast*)ir)->opnd[CK_KID_IRC(ir, IR_BRDCAST, idx)])
class CBrdCast : public IR {
    COPY_CONSTRUCTOR(CBrdCast);
public:
    typedef enum { THREADGROUP, ROW, COL, P2P } MODE;
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    bool is_get_op;
    MODE mode;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return BRDCAST_kid(ir, idx); }

    IR * getKid(UINT idx) const { return BRDCAST_kid(this, idx); }
};


//This class represents memcpy operation.
#define MEMCPY_src(ir) MEMCPY_kid(ir, 0)
#define MEMCPY_len(ir) MEMCPY_kid(ir, 1)
#define MEMCPY_kid(ir, idx) \
    (((CMemcpy*)ir)->opnd[CK_KID_IRC(ir, IR_MEMCPY, idx)])
#define MEMCPY_src_storage_space(ir) \
    (((CMemcpy*)CK_IRC(ir, IR_MEMCPY))->src_storage_space)
class CMemcpy : public IR {
    COPY_CONSTRUCTOR(CMemcpy);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    StorageSpace src_storage_space;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return MEMCPY_kid(ir, idx); }
    static inline StorageSpace & accSS(IR * ir)
    { return MEMCPY_src_storage_space(ir); }

    IR * getKid(UINT idx) const { return MEMCPY_kid(this, idx); }
    IR * getSrc() const { return MEMCPY_src(this); }
    IR * getLen() const { return MEMCPY_len(this); }
};

} //namespace xoc
#endif
