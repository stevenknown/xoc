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

} //namespace xoc

#include "targ_decl_ext.h"

#endif
