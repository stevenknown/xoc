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

//This class represents properties of stmt that may have multiple results.
class MultiResProp {
    COPY_CONSTRUCTOR(MultiResProp);
public:
    IR * res_list;
};

#define VSTPR_bb(ir) (((CVStpr*)CK_IRC(ir, IR_VSTPR))->bb)
#define VSTPR_no(ir) (((CVStpr*)CK_IRC(ir, IR_VSTPR))->prno)
#define VSTPR_ssainfo(ir) (((CVStpr*)CK_IRC(ir, IR_VSTPR))->ssainfo)
#define VSTPR_du(ir) (((CVStpr*)CK_IRC(ir, IR_VSTPR))->du)
#define VSTPR_kid(ir, idx) \
    (((CVStpr*)ir)->opnd[CK_KID_IRC(ir, IR_VSTPR, idx)])
//Represents the RHS of defined PR, thus it must be PR, which indicates that
//RHS defined LHS. If LHS and RHS's physical registers are different, it will
//certainly be converted to MOVE operation, e.g:stpr<-pr.
#define VSTPR_rhs(ir) VSTPR_kid(ir, 0)

//Represents a dummy USE of PR that is used to maintain DU chain.
#define VSTPR_dummyuse(ir) VSTPR_kid(ir, 1)
class CVStpr : public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CVStpr);
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


#define VST_bb(ir) (((CVSt*)CK_IRC(ir, IR_VST))->bb)
#define VST_idinfo(ir) (((CVSt*)CK_IRC(ir, IR_VST))->id_info)
#define VST_ofst(ir) (((CVSt*)CK_IRC(ir, IR_VST))->field_offset)
#define VST_du(ir) (((CVSt*)CK_IRC(ir, IR_VST))->du)
#define VST_rhs(ir) VST_kid(ir, 0)
#define VST_dummyuse(ir) VST_kid(ir, 1)
#define VST_kid(ir, idx) (((CVSt*)ir)->opnd[CK_KID_IRC(ir, IR_VST, idx)])
class CVSt : public CLd, public StmtProp {
    COPY_CONSTRUCTOR(CVSt);
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
#define VIST_bb(ir) (((CVISt*)CK_IRC(ir, IR_VIST))->bb)
#define VIST_ofst(ir) (((CVISt*)CK_IRC(ir, IR_VIST))->field_offset)
#define VIST_du(ir) (((CVISt*)CK_IRC(ir, IR_VIST))->du)
#define VIST_base(ir) VIST_kid(ir, 0)
#define VIST_rhs(ir) VIST_kid(ir, 1)
#define VIST_dummyuse(ir) VIST_kid(ir, 2)
#define VIST_kid(ir, idx) (((CVISt*)ir)->opnd[CK_KID_IRC(ir, IR_VIST, idx)])
class CVISt : public DuProp, public OffsetProp, public StmtProp {
    COPY_CONSTRUCTOR(CVISt);
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
#define BROADCAST_res_list(ir) \
    (((CBroadCast*)CK_IRC(ir, IR_BROADCAST))->res_list)
#define BROADCAST_kid(ir, idx) \
    (((CBroadCast*)ir)->opnd[CK_KID_IRC(ir, IR_BROADCAST, idx)])
class CBroadCast : public IR, public MultiResProp {
    COPY_CONSTRUCTOR(CBroadCast);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 1;
    IR * opnd[kid_num];
    IR * res_list;
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return BROADCAST_kid(ir, idx); }
    static inline IR *& accResList(IR * ir)
    { return BROADCAST_res_list(ir); }
};


//This class represents atomic inc operation of fetch and add on memory. IR:
//
//  stpr $res:i64
//    atominc:i64
//      ild:i64 memory
//        $src:*<1>
//      ild:i64 multi-res
//        $src:*<1>
//Or:
//
//  stpr $res:i64
//    atominc:i64
//      ld:i64:storage_space(global) 'global_var' memory
//      ld:i64:storage_space(global) 'global_var' multi-res
//
//Note that this operation will change memory of "global_var" or pointed by
//"$src", and "res" both.

//Operated memory.
#define ATOMINC_memory(ir) ATOMINC_kid(ir, 0)

//The num to add (absent on T1 so it's initialized to nullptr).
#define ATOMINC_addend(ir) ATOMINC_kid(ir, 1)

//The multiple result list.
#define ATOMINC_multires(ir) (((CAtomInc*)CK_IRC(ir, IR_ATOMINC))->res_list)

#define ATOMINC_kid(ir, idx)\
    (((CAtomInc*)ir)->opnd[CK_KID_IRC(ir, IR_ATOMINC, idx)])

class CAtomInc : public IR, public MultiResProp {
    COPY_CONSTRUCTOR(CAtomInc);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 2;
    IR * opnd[kid_num];
    IR * res_list;
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return ATOMINC_kid(ir, idx); }
    static inline IR *& accResList(IR * ir)
    { return ATOMINC_multires(ir); }
};


//This class represents atomic cas operation of compare and swap on memory. IR:
//
//  stpr $res:i32
//    cas:i32
//      ild:i32 memory
//        $src:*<1>
//      $oldval:i32 oldval
//      $newval:i32 newval
//      ild:i32 multi-res
//        $src:*<1>
//Or:
//
//  stpr $res:i64
//    atomcas:i64
//      ld:i64:storage_space(global) 'global_var' memory
//      $oldval:i64 oldval
//      $newval:i64 newval
//      ld:i64:storage_space(global) 'global_var' multi-res
//
//Note that this operation will change memory of "global_var" or pointed by
//"$src", and "res" both.
//
//Operated memory.
#define ATOMCAS_memory(ir) ATOMCAS_kid(ir, 0)

//Compared with the value of target operand.
#define ATOMCAS_oldval(ir) ATOMCAS_kid(ir, 1)

//Indicate the newval if changed success.
#define ATOMCAS_newval(ir) ATOMCAS_kid(ir, 2)

//Atomic compare and swap operation may occupy additional resources on
//different architectures, such as registers, memory, etc. This attribute needs
//to be explicitly specified to support other modules in allocating resources.
#define ATOMCAS_occupy(ir) ATOMCAS_kid(ir, 3)

//Multiple result modified by this operation.
#define ATOMCAS_multires(ir) (((CAtomCas*)CK_IRC(ir, IR_ATOMCAS))->res_list)

#define ATOMCAS_kid(ir, idx)\
    (((CAtomCas*)ir)->opnd[CK_KID_IRC(ir, IR_ATOMCAS, idx)])
class CAtomCas : public IR, public MultiResProp {
    COPY_CONSTRUCTOR(CAtomCas);
public:
    static BYTE const kid_map = 0x7;
    static BYTE const kid_num = 4;
    IR * opnd[kid_num];
    IR * res_list;
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return ATOMCAS_kid(ir, idx); }
    static inline IR *& accResList(IR * ir)
    { return ATOMCAS_multires(ir); }
};

} //namespace xoc

//Do NOT place extended declarations header files within xoc namespace.
//User should guarantee extended IR are declared within xoc namespace.
#include "targ_decl_ext.h"

#endif
