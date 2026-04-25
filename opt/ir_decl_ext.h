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
    static UINT const accinfo_num = 6;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];
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
    static UINT const accinfo_num = 5;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];
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
    static UINT const accinfo_num = 5;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];
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
#define BROADCAST_src(ir)      BROADCAST_kid(ir, 0)
#define BROADCAST_res_list(ir) BROADCAST_kid(ir, 1)
#define BROADCAST_kid(ir, idx) \
    (((CBroadCast*)ir)->opnd[CK_KID_IRC(ir, IR_BROADCAST, idx)])
class CBroadCast : public IR, public MultiResProp {
    COPY_CONSTRUCTOR(CBroadCast);
public:
    //Result list is permitted to be NULL if the broadcast-IR lies on the
    //child of dummyuse of virtual operations.
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 2;
    static UINT const accinfo_num = 2;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return BROADCAST_kid(ir, idx); }
    static inline IR *& accResList(IR * ir)
    { return BROADCAST_res_list(ir); }

    IR const* getResList() const { return BROADCAST_res_list(this); }
    bool isResList(IR const* exp) const;
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
#define ATOMINC_memory(ir)   ATOMINC_kid(ir, 0)

//The num to add (absent on T1 so it's initialized to nullptr).
#define ATOMINC_addend(ir)   ATOMINC_kid(ir, 1)

//The multiple result list.
#define ATOMINC_multires(ir) ATOMINC_kid(ir, 2)

#define ATOMINC_kid(ir, idx)\
    (((CAtomInc*)ir)->opnd[CK_KID_IRC(ir, IR_ATOMINC, idx)])

class CAtomInc : public IR, public MultiResProp {
    COPY_CONSTRUCTOR(CAtomInc);
public:
    static BYTE const kid_map = 0x1;
    static BYTE const kid_num = 3;
    static UINT const accinfo_num = 2;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return ATOMINC_kid(ir, idx); }
    static inline IR *& accResList(IR * ir)
    { return ATOMINC_multires(ir); }
    IR const* getResList() const { return ATOMINC_multires(this); }
    bool isResList(IR const* exp) const;
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
#define ATOMCAS_memory(ir)   ATOMCAS_kid(ir, 0)

//Compared with the value of target operand.
#define ATOMCAS_oldval(ir)   ATOMCAS_kid(ir, 1)

//Indicate the newval if changed success.
#define ATOMCAS_newval(ir)   ATOMCAS_kid(ir, 2)

//Atomic compare and swap operation may occupy additional resources on
//different architectures, such as registers, memory, etc. This attribute needs
//to be explicitly specified to support other modules in allocating resources.
#define ATOMCAS_occupy(ir)   ATOMCAS_kid(ir, 3)

//Multiple result modified by this operation.
#define ATOMCAS_multires(ir) ATOMCAS_kid(ir, 4)

#define ATOMCAS_kid(ir, idx)\
    (((CAtomCas*)ir)->opnd[CK_KID_IRC(ir, IR_ATOMCAS, idx)])
class CAtomCas : public IR, public MultiResProp {
    COPY_CONSTRUCTOR(CAtomCas);
public:
    static BYTE const kid_map = 0x7;
    static BYTE const kid_num = 5;
    static UINT const accinfo_num = 2;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return ATOMCAS_kid(ir, idx); }
    static inline IR *& accResList(IR * ir)
    { return ATOMCAS_multires(ir); }
    IR const* getResList() const { return ATOMCAS_multires(this); }
    bool isResList(IR const* exp) const;
};


//This IR is used to represent a dynamic length vector operation.
//By specifying the length, the number of elements in the operation
//can be controlled. Through the 'tail_strategy' field, the behavior of the
//tail elements of the vector can be controlled.
//e.g: For example, specify a length of $len, perform an add operation,
//and keep the tail element of $res unchanged from its original value.
//  stpr $res:vec<s32m1x?>
//    dynlenop:
//      add:vec<s32m1x?>
//        $src1:vec<s32m1x?>
//        $src2:vec<s32m1x?>
//      $len: u64
//      tail_strategy(undisturbed)
//
//Tail element processing strategy.
#define DYNLENOP_tail_strategy(ir) \
    (((CDynLenOp*)ir)->tail_strategy_attr.strategy)

//Dynamic length operation.
#define DYNLENOP_op(ir) DYNLENOP_kid(ir, 0)

//Operating length.
#define DYNLENOP_len(ir) DYNLENOP_kid(ir, 1)

#define DYNLENOP_kid(ir, idx) \
    (((CDynLenOp*)ir)->opnd[CK_KID_IRC(ir, IR_DYNLEN_OP, idx)])

class CDynLenOp : public IR {
    COPY_CONSTRUCTOR(CDynLenOp);
public:
    //Tail strategy (tail agnostic/undisturbed):
    //  undisturbed: Tail elements (beyond vector element's length)
    //                  are left undisturbed.
    //  agnostic: Tail elements may be overwritten with arbitrary values.
    //For example:
    //(1)When using the tail 'undisturbed' strategy, the tail element
    //   maintains its original value after the operation is completed.
    //   Assuming $len=2, the total element size that the vector register
    //   can accommodate is 4, and $opnd0 = {1, 2, 3, 4},
    //   $opnd1 = {1, 2, 3, 4}, $res = {0, 0, 0, 0}.
    //   running:
    //    stpr $res:vec<s32m1x?>
    //      dynlenop:
    //        add:vec<s32m1x?>
    //          $opnd0:vec<s32m1x?>
    //          $opnd1:vec<s32m1x?>
    //        $len: u64
    //        tail_strategy(undisturbed)
    //   result is:
    //     $res = {2, 4, 0, 0};
    //   Note that the values of the third and fourth elements of $res
    //   remain unchanged at 0.
    //(2)When using the 'agnostic' tail strategy, The tail element does not
    //   guarantee that the original value will remain unchanged after
    //   the operation.
    //   Assuming $len=2, the total element size that the vector register
    //   can accommodate is 4, and $opnd0 = {1, 2, 3, 4},
    //   $opnd1 = {1, 2, 3, 4}, $res = {0, 0, 0, 0}.
    //   running:
    //    stpr $res:vec<s32m1x?>
    //      dynlenop:
    //        add:vec<s32m1x?>
    //          $opnd0:vec<s32m1x?>
    //          $opnd1:vec<s32m1x?>
    //        $len: u64
    //        tail_strategy(agnostic)
    //   result may is:
    //     $res = {2, 4, 6(Or other values), 8(Or other values)};
    //   Note that the values of the third and fourth elements of $res cannot
    //   be guaranteed to be 0, they may be 6 or 8.
    typedef enum TAIL_STRATEGY {
        UNDEF = 0,
        UNDISTURBED,
        AGNOSTIC,
        TS_NUM,
    } TAIL_STRATEGY;

    class TailStraAttr {
    public:
        CDynLenOp::TAIL_STRATEGY strategy;
        CHAR const* strategy_name;
    };
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    static UINT const accinfo_num = 1;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];

    //Tail strategy attribute(tail agnostic/undisturbed):
    //  undisturbed: Tail elements (beyond vector element's length)
    //                  are left undisturbed.
    //  agnostic: Tail elements may be overwritten with arbitrary values.
    TailStraAttr tail_strategy_attr;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return DYNLENOP_kid(ir, idx); }

    static CHAR const* getTailStrategyName(TAIL_STRATEGY ts);
};


//This class represent operation that has a mask operand. Usually the operation
//is used to describe selection operation in IR expression tree.
//Generally, by given a full-size vector or tensor operation, using a mask
//operand to select specific elements for processing.
//e.g: The example demostrates that maskop selects element from the result
//of 'add' according to '$mask'.
//  stpr $res:vec<i32x32>
//    maskop:vec<i32x32>
//      add:vec<i32x32>
//        $src1:vec<i32x32>
//        $src2:vec<i32x32>
//      $mask:<boolx32>
//
//However, this IR can also represent the use of mask operands to select
//which positions of elements need to perform operations at a specified
//length, while specifying the behavior of elements at positions with
//a mask value of 0.
//e.g: For example, when the length is '$len', perform an add operation
//and use '$mask' to control which positions of the add operation need to
//be executed. For positions that do not perform operations, ensure that their
//values remain unchanged(this is achieved by
//specifying mask_strategy is undisturbed).
//  stpr $res:vec<s32m1x?>
//    maskop:vec<s32m1x?>
//      dynlenop:
//        add:vec<s32m1x?>
//          $src1:vec<s32m1x?>
//          $src2:vec<s32m1x?>
//        $len: u64
//        tail_strategy(0)
//      $mask:<bool32x?>
//      mask_strategy(undisturbed)
//
//Mask strategy.
#define MASKOP_mask_strategy(ir) \
    (((CMaskOp*)ir)->mask_strategy_attr.strategy)

//Specify size or full length operation.
#define MASKOP_op(ir) MASKOP_kid(ir, 0)

//Mask operand.
#define MASKOP_mask(ir) MASKOP_kid(ir, 1)

#define MASKOP_kid(ir, idx) \
    (((CMaskOp*)ir)->opnd[CK_KID_IRC(ir, IR_MASK_OP, idx)])

class CMaskOp : public IR {
    COPY_CONSTRUCTOR(CMaskOp);
public:
    //Mask strategy(mask agnostic/undisturbed):
    //  undisturbed: Elements with mask=0 are left undisturbed.
    //  agnostic: Elements with mask=0 may be overwritten with
    //               arbitrary values.
    //For example:
    //(1)When using the 'undisturbed' mask strategy, The element at the position
    //   where the mask value is 0 can maintain its original value after
    //   the operation.
    //   Assuming $len=4, the total element size that the vector register
    //   can accommodate is 4, and $opnd0 = {1, 2, 3, 4},
    //   $opnd1 = {1, 2, 3, 4}, $res = {0, 0, 0, 0}, $mask = {0, 1, 0, 1}.
    //   running:
    //    maskop:vec<s32m1x?>
    //      dynlenop:
    //        add:vec<s32m1x?>
    //          $opnd0:vec<s32m1x?>
    //          $opnd1:vec<s32m1x?>
    //        $len: u64
    //        tail_strategy(undisturbed)
    //      $mask:<bool32x?>
    //      mask_strategy(undisturbed)
    //   result is:
    //     $res = {0, 4, 0, 8};
    //   Note that the values of the first and third elements of $res
    //   remain unchanged at 0.
    //(2)When using the 'agnostic' mask strategy, The element at the position
    //   where the mask value is 0 cannot guarantee that the original value
    //   remains unchanged after the operation.
    //   Assuming $len=4, the total element size that the vector register
    //   can accommodate is 4, and $opnd0 = {1, 2, 3, 4},
    //   $opnd1 = {1, 2, 3, 4}, $res = {0, 0, 0, 0}, $mask = {0, 1, 0, 1}.
    //   running:
    //    maskop:vec<s32m1x?>
    //      dynlenop:
    //        add:vec<s32m1x?>
    //          $opnd0:vec<s32m1x?>
    //          $opnd1:vec<s32m1x?>
    //        $len: u64
    //        tail_strategy(undisturbed)
    //      $mask:<bool32x?>
    //      mask_strategy(agnostic)
    //   result may is:
    //     $res = {0(Or other values), 4, 0(Or other values), 8};
    //   Note that the values of the first and third elements of $res cannot
    //   be guaranteed to be 0, they may be 2 or 6.
    typedef enum MASK_STRATEGY {
        UNDEF = 0,
        UNDISTURBED,
        AGNOSTIC,
        MS_NUM,
    } MASK_STRATEGY;

    class MaskStraAttr {
    public:
        CMaskOp::MASK_STRATEGY strategy;
        CHAR const* strategy_name;
    };
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    static UINT const accinfo_num = 1;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];

    //Mask strategy attribute(mask agnostic/undisturbed):
    //  undisturbed: Elements with mask=0 are left undisturbed.
    //  agnostic: Elements with mask=0 may be overwritten with
    //               arbitrary values.
    MaskStraAttr mask_strategy_attr;
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return MASKOP_kid(ir, idx); }

    static CHAR const* getMaskStrategyName(MASK_STRATEGY ms);
};


//This class represent operation that perform elements selection according to
//masks and variable length instructions.
//NOTE: the operation is always used to describe STMT.
//This is used to describe specific selection operation, Used to describe
//the result of placing masked or variable length instructions at certain
//positions in an operand, i.e. with partially modified semantics.
//This instruction is usually used in conjunction with mask instructions
//or variable length instructions.
//There are two main ways to use this IR:
//
//(1)e.g: For example, Used in conjunction with variable length instructions
//   to describe storing a partial result of an instruction at a specified
//   length interval in a specific vector register 'base'. Assuming that the
//   vector register can accommodate four s32 elements, the length of the
//   variable length instruction is set to 2. After calculation, the result
//   becomes the result of the first and second positions of the variable
//   length instruction, concatenated with the result of the third and fourth
//   positions of the original 'base' register.
//   Assuming $opnd0 = {1, 2, 3, 4}, $opnd1 = {1, 2, 3, 4},
//   $res = {0, 0, 1, 1}, $len = 2.
//   running:
//     stpr $res:vec<s32m1x?>
//       select_to_res:vec<s32m1x?>
//         dynlenop:
//           add:vec<s32m1x?>
//             $opnd0:vec<s32m1x?>
//             $opnd1:vec<s32m1x?>
//           $len: u64
//           tail_strategy(undisturbed)
//         $res
//   the result is:
//     $res = {2, 4, 1, 1};
//   Note that the 'base' operand and result register are usually the same.
//
//(2)e.g: For example, Used in conjunction with mask and variable length
//   instructions to describe storing a partial result of an instruction
//   at a specified position in a specific vector register 'base'.
//   Assuming that the vector register can accommodate four s32 elements,
//   the length of the variable length instruction is set to 2.
//   After calculation, the result becomes the result of the first position
//   of the mask instruction, concatenated with the result of the second,
//   the third and fourth positions of the original 'base' register.
//   Assuming $opnd0 = {1, 2, 3, 4}, $opnd1 = {1, 2, 3, 4},
//   $res = {0, 0, 1, 1}, $len = 2, $mask = {1, 0, 0, 0}.
//   running:
//     stpr $res:vec<s32m1x?>
//       select_to_res:vec<s32m1x?>
//         maskop:
//           dynlenop:
//             add:vec<s32m1x?>
//               $opnd0:vec<s32m1x?>
//               $opnd1:vec<s32m1x?>
//             $len: u64
//             tail_strategy(undisturbed)
//           $mask:<bool32x?>
//           mask_strategy(undisturbed)
//         $res
//   the result is:
//     $res = {2, 0, 1, 1};
//   Note that the 'base' operand and result register are usually the same.
//

//Normal mask/dynamic-size operation.
#define SELECTTORES_op(ir) SELECTTORES_kid(ir, 0)

//The operands to be partially modified.
#define SELECTTORES_base(ir) SELECTTORES_kid(ir, 1)

#define SELECTTORES_kid(ir, idx) \
    (((CSelectToRes*)ir)->opnd[CK_KID_IRC(ir, IR_SELECT_TO_RES, idx)])

class CSelectToRes : public IR {
    COPY_CONSTRUCTOR(CSelectToRes);
public:
    static BYTE const kid_map = 0x3;
    static BYTE const kid_num = 2;
    static UINT const accinfo_num = 1;
    static IRFieldAccTab::AccInfo accinfo[accinfo_num];
    IR * opnd[kid_num];
public:
    static inline IR *& accKid(IR * ir, UINT idx)
    { return SELECTTORES_kid(ir, idx); }
};


#ifdef REF_TARGMACH_INFO

class RegPhi;

//This class represents physical register.
//It can only be used to in RegPhi operand list in RegSSA mode.
#define PHYREG_phi(ir) (((CPhyReg*)CK_IRC(ir, IR_PHYREG))->m_phi)
#define PHYREG_reg(ir) (((CPhyReg*)CK_IRC(ir, IR_PHYREG))->m_reg)
class CPhyReg : public IR {
    COPY_CONSTRUCTOR(CPhyReg);
public:
    static BYTE const kid_map = 0x0;
    static BYTE const kid_num = 0;
    xgen::Reg m_reg;
    RegPhi * m_phi;
public:
    RegPhi * getRegPhi() const { return m_phi; }
};

#endif

} //namespace xoc

//Do NOT place extended declarations header files within xoc namespace.
//User should guarantee extended IR are declared within xoc namespace.
#include "targ_decl_ext.h"

#endif
