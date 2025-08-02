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
#ifndef _IR_DESC_H_
#define _IR_DESC_H_

namespace xoc {

class IR;
class IRBB;
class DumpFlag;
class IRDumpCustomBaseFunc;
class IRKidMap;
class Region;
class Var;
class SSAInfo;
class LabelInfo;

template <class CustomFunc = IRDumpCustomBaseFunc> class IRDumpCtx;

#define NO_DUMP_FUNC nullptr
#define NO_VERIFY_FUNC nullptr
#define NO_ACC_RHS_FUNC nullptr
#define NO_ACC_IDINFO_FUNC nullptr
#define NO_ACC_OFST_FUNC nullptr
#define NO_ACC_SSAINFO_FUNC nullptr
#define NO_ACC_PRNO_FUNC nullptr
#define NO_ACC_RESPR_FUNC nullptr
#define NO_ACC_KID_FUNC nullptr
#define NO_ACC_BB_FUNC nullptr
#define NO_ACC_BASE_FUNC nullptr
#define NO_ACC_LAB_FUNC nullptr
#define NO_ACC_DET_FUNC nullptr
#define NO_ACC_SS_FUNC nullptr
#define NO_ACC_RESLIST_FUNC nullptr

typedef void(*IRDumpFuncType)(
    IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
typedef bool(*IRVerifyFuncType)(IR const* ir, Region const* rg);
typedef IR *& (*IRAccRHSFuncType)(IR * t);
typedef Var *& (*IRAccIdinfoFuncType)(IR * ir);
typedef TMWORD & (*IRAccOfstFuncType)(IR * ir);
typedef SSAInfo *& (*IRAccSSAInfoFuncType)(IR * ir);
typedef PRNO & (*IRAccPrnoFuncType)(IR * ir);
typedef IR * (*IRAccResultPRFuncType)(IR * ir);
typedef IR *& (*IRAccKidFuncType)(IR * ir, UINT idx);
typedef IRBB *& (*IRAccBBFuncType)(IR * ir);
typedef IR *& (*IRAccBaseFuncType)(IR * ir);
typedef LabelInfo const*& (*IRAccLabFuncType)(IR * ir);
typedef IR *& (*IRAccDetFuncType)(IR * ir);
typedef StorageSpace & (*IRAccStorageSpaceFuncType)(IR * ir);
typedef IR *& (*IRAccResListFuncType)(IR * ir);

typedef enum tagIRC_ATTR {
    //Describe miscellaneous information for IR.
    IRC_IS_STMT_POS = 0,
    IRC_IS_STMT = 1ULL<<IRC_IS_STMT_POS, //statement.

    IRC_IS_TER_POS = 1,
    IRC_IS_TER = 1ULL<<IRC_IS_TER_POS, //ternary operation.

    IRC_IS_BIN_POS = 2,
    IRC_IS_BIN = 1ULL<<IRC_IS_BIN_POS, //binary operation.

    IRC_IS_UNA_POS = 3,
    IRC_IS_UNA = 1ULL<<IRC_IS_UNA_POS, //unary operation.

    //Memory reference operation. Memory reference indicates all
    //operations which write or load memory object.
    IRC_IS_MEM_REF_POS = 4,
    IRC_IS_MEM_REF = 1ULL<<IRC_IS_MEM_REF_POS,

    //Memory operand indicates all operations which only load memory object.
    IRC_IS_MEM_OPND_POS = 5,
    IRC_IS_MEM_OPND = 1ULL<<IRC_IS_MEM_OPND_POS,

    //Indicates the operation satifies arithmetic associative.
    IRC_IS_ASSOCIATIVE_POS = 6,
    IRC_IS_ASSOCIATIVE = 1ULL<<IRC_IS_ASSOCIATIVE_POS,

    //Indicates the operation satifies arithmetic commutative.
    IRC_IS_COMMUTATIVE_POS = 7,
    IRC_IS_COMMUTATIVE = 1ULL<<IRC_IS_COMMUTATIVE_POS,

    //Indicates the operation is relation operation.
    IRC_IS_RELATION_POS = 8,
    IRC_IS_RELATION = 1ULL<<IRC_IS_RELATION_POS,

    //Indicates the operation is logical operation.
    IRC_IS_LOGICAL_POS = 9,
    IRC_IS_LOGICAL = 1ULL<<IRC_IS_LOGICAL_POS,

    //Indicates the operation does not have any kid.
    IRC_IS_LEAF_POS = 10,
    IRC_IS_LEAF = 1ULL<<IRC_IS_LEAF_POS,

    //Indicates the operation generates output result.
    IRC_HAS_RESULT_POS = 11,
    IRC_HAS_RESULT = 1ULL<<IRC_HAS_RESULT_POS,

    //Indicates the operation can be placed into Basic Block.
    IRC_IS_STMT_IN_BB_POS = 12,
    IRC_IS_STMT_IN_BB = 1ULL<<IRC_IS_STMT_IN_BB_POS,

    //Indicates the operation is nonpr memory operation.
    IRC_IS_NON_PR_MEMREF_POS = 13,
    IRC_IS_NON_PR_MEMREF = 1ULL<<IRC_IS_NON_PR_MEMREF_POS,

    //Indicates the operation has mustref and mayref.
    IRC_HAS_DU_POS = 14,
    IRC_HAS_DU = 1ULL<<IRC_HAS_DU_POS,

    //Indicates the operation is write-pr operation.
    IRC_IS_WRITE_PR_POS = 15,
    IRC_IS_WRITE_PR = 1ULL<<IRC_IS_WRITE_PR_POS,

    //Indicates the operation is write-pr operation and its result will
    //modifying whole PR.
    IRC_IS_WRITE_WHOLE_PR_POS = 16,
    IRC_IS_WRITE_WHOLE_PR = 1ULL<<IRC_IS_WRITE_WHOLE_PR_POS,

    //Indicates the operation is memory operation and has offset.
    IRC_HAS_OFFSET_POS = 17,
    IRC_HAS_OFFSET = 1ULL<<IRC_HAS_OFFSET_POS,

    //Indicates the operation has identifier information.
    IRC_HAS_IDINFO_POS = 18,
    IRC_HAS_IDINFO = 1ULL<<IRC_HAS_IDINFO_POS,

    //Indicates the operation is directly accesssing memory through idinfo.
    IRC_IS_DIRECT_MEM_OP_POS = 19,
    IRC_IS_DIRECT_MEM_OP = 1ULL<<IRC_IS_DIRECT_MEM_OP_POS,

    //Indicates the operation is indirectly accesssing memory through base.
    IRC_IS_INDIRECT_MEM_OP_POS = 20,
    IRC_IS_INDIRECT_MEM_OP = 1ULL<<IRC_IS_INDIRECT_MEM_OP_POS,

    //Indicates the operation is conditional branch operation.
    IRC_IS_CONDITIONAL_BR_POS = 21,
    IRC_IS_CONDITIONAL_BR = 1ULL<<IRC_IS_CONDITIONAL_BR_POS,

    //Indicates the operation is unconditional branch operation.
    IRC_IS_UNCONDITIONAL_BR_POS = 22,
    IRC_IS_UNCONDITIONAL_BR = 1ULL<<IRC_IS_UNCONDITIONAL_BR_POS,

    //Indicates the operation is memory operation that accessing through
    //array style.
    IRC_IS_ARRAY_OP_POS = 23,
    IRC_IS_ARRAY_OP = 1ULL<<IRC_IS_ARRAY_OP_POS,

    //Indicates the operation is memory operation and it has a RHS expression.
    IRC_HAS_RHS_POS = 24,
    IRC_HAS_RHS = 1ULL<<IRC_HAS_RHS_POS,

    //Indicates the operation has determinate expression
    IRC_HAS_JUDGE_TARGET_POS = 25,
    IRC_HAS_JUDGE_TARGET = 1ULL<<IRC_HAS_JUDGE_TARGET_POS,

    //Indicates the operation has a case list.
    IRC_HAS_CASE_LIST_POS = 26,
    IRC_HAS_CASE_LIST = 1ULL<<IRC_HAS_CASE_LIST_POS,

    //Indicates the operation has storage space information.
    IRC_HAS_STORAGE_SPACE_POS = 27,
    IRC_HAS_STORAGE_SPACE = 1ULL<<IRC_HAS_STORAGE_SPACE_POS,

    //Indicates the operation has multiple-result list.
    IRC_HAS_RES_LIST_POS = 28,
    IRC_HAS_RES_LIST = 1ULL<<IRC_HAS_RES_LIST_POS,

    //Indicates the operation is placeholder.
    IRC_MAIN_ATTR_PLACEHOLDER_POS = 100,

    //Indicates the position of the last main attribute.
    IRC_MAIN_ATTR_LAST_POS = IRC_MAIN_ATTR_PLACEHOLDER_POS,

    #include "irc_attr_ext.inc"

    //Indicates the position of the last attribute.
    IRC_ATTR_LAST_POS,

    //NOTE: THERE IS NO ATTR NUMBER SINCE THE POSITIONS OF ATTR ARE NOT
    //CONITNUOUS.
} IRC_ATTR;

////////////////////////////////////////////////////////////////////////////////
//NOTE: If new IR flag value is greater than the bit range that IRDescFlagSeg //
//can express, user should extend the IRDescFlagSegNum value and set the      //
//big flag value at RegionMgr::initIRDescFlagSet(), then invoke the function  //
//right after RegionMgr created.                                              //
////////////////////////////////////////////////////////////////////////////////

//
//START IRDescFlag
//
//The flag-segment indicates the minimum bytesize simplex type that is used to
//establish the flag set.
typedef UINT64 IRDescFlagSeg;

//So far, the number of total flags used to describe IR attributes is
//not more than two FlagSeg.
#define IRDescFlagSegNum 2

class IRDescFlag : public xcom::FlagSet<IRDescFlagSeg, IRDescFlagSegNum> {
    //THE CLASS ALLOWS COPY-CONSTRUCTOR.
public:
    IRDescFlag() {}

    //The function constructs flag with a single flag-segement.
    IRDescFlag(IRDescFlagSeg v) : FlagSet<IRDescFlagSeg, IRDescFlagSegNum>(v) {}

    //The function constructs flag with a pre-constructed byte buffer that
    //record the IRC_ATTR.
    IRDescFlag(BYTE const* vbuf, UINT vbuflen)
        : FlagSet<IRDescFlagSeg, IRDescFlagSegNum>(vbuf, vbuflen) {}

    //The function constructs flag with a list of IRC_ATTR.
    //flag_pos_num: the number of flag-position.
    //...: a list of flag-positions.
    //usage:construct with three irc-attr, IRC_X, IRC_Y, IRC_Z
    //      by IRDescFlag(3, (UINT)IRC_X, (UINT)IRC_Y, (UINT)IRC_Z)
    IRDescFlag(UINT flag_pos_num, ...);
};
//END IRDescFlag


//
//START IRDesc
//
#define IRDES_code(c) (g_ir_desc[c].code)
#define IRDES_name(c) (g_ir_desc[c].name)
#define IRDES_kid_map(c) (g_ir_desc[c].kid_map)
#define IRDES_kid_num(c) (g_ir_desc[c].kid_num)
#define IRDES_attr(c) (g_ir_desc[c].attr)
#define IRDES_is_stmt(c) (IRDES_attr(c).have(IRC_IS_STMT_POS))
#define IRDES_is_ter(c) (IRDES_attr(c).have(IRC_IS_TER_POS))
#define IRDES_is_bin(c) (IRDES_attr(c).have(IRC_IS_BIN_POS))
#define IRDES_is_una(c) (IRDES_attr(c).have(IRC_IS_UNA_POS))
#define IRDES_is_mem_ref(c) (IRDES_attr(c).have(IRC_IS_MEM_REF_POS))
#define IRDES_is_mem_opnd(c) (IRDES_attr(c).have(IRC_IS_MEM_OPND_POS))
#define IRDES_is_associative(c) (IRDES_attr(c).have(IRC_IS_ASSOCIATIVE_POS))
#define IRDES_is_commutative(c) (IRDES_attr(c).have(IRC_IS_COMMUTATIVE_POS))
#define IRDES_is_relation(c) (IRDES_attr(c).have(IRC_IS_RELATION_POS))
#define IRDES_is_logical(c) (IRDES_attr(c).have(IRC_IS_LOGICAL_POS))
#define IRDES_is_leaf(c) (IRDES_attr(c).have(IRC_IS_LEAF_POS))
#define IRDES_is_stmt_in_bb(c) (IRDES_attr(c).have(IRC_IS_STMT_IN_BB_POS))
#define IRDES_is_non_pr_memref(c) \
    (IRDES_attr(c).have(IRC_IS_NON_PR_MEMREF_POS))
#define IRDES_has_result(c) (IRDES_attr(c).have(IRC_HAS_RESULT_POS))
#define IRDES_has_offset(c) (IRDES_attr(c).have(IRC_HAS_OFFSET_POS))
#define IRDES_has_idinfo(c) (IRDES_attr(c).have(IRC_HAS_IDINFO_POS))
#define IRDES_has_du(c) (IRDES_attr(c).have(IRC_HAS_DU_POS))
#define IRDES_has_rhs(c) (IRDES_attr(c).have(IRC_HAS_RHS_POS))
#define IRDES_has_res_list(c) (IRDES_attr(c).have(IRC_HAS_RES_LIST_POS))
#define IRDES_has_judge_target(c) \
    (IRDES_attr(c).have(IRC_HAS_JUDGE_TARGET_POS))
#define IRDES_has_storage_space(c) \
    (IRDES_attr(c).have(IRC_HAS_STORAGE_SPACE_POS))
#define IRDES_has_case_list(c) (IRDES_attr(c).have(IRC_HAS_CASE_LIST_POS))
#define IRDES_is_write_pr(c) (IRDES_attr(c).have(IRC_IS_WRITE_PR_POS))
#define IRDES_is_write_whole_pr(c) \
    (IRDES_attr(c).have(IRC_IS_WRITE_WHOLE_PR_POS))
#define IRDES_is_conditional_br(c) \
    (IRDES_attr(c).have(IRC_IS_CONDITIONAL_BR_POS))
#define IRDES_is_unconditional_br(c) \
    (IRDES_attr(c).have(IRC_IS_UNCONDITIONAL_BR_POS))
#define IRDES_is_array_op(c) \
    (IRDES_attr(c).have(IRC_IS_ARRAY_OP_POS))
#define IRDES_is_direct_mem_op(c) \
    (IRDES_attr(c).have(IRC_IS_DIRECT_MEM_OP_POS))
#define IRDES_is_indirect_mem_op(c) \
    (IRDES_attr(c).have(IRC_IS_INDIRECT_MEM_OP_POS))
#define IRDES_size(c) (g_ir_desc[c].size)
#define IRDES_dumpfunc(c) (g_ir_desc[c].dumpfunc)
#define IRDES_verifyfunc(c) (g_ir_desc[c].verifyfunc)
#define IRDES_accrhsfunc(c) (g_ir_desc[c].accrhsfunc)
#define IRDES_accidinfofunc(c) (g_ir_desc[c].accidinfofunc)
#define IRDES_accofstfunc(c) (g_ir_desc[c].accofstfunc)
#define IRDES_accssainfofunc(c) (g_ir_desc[c].accssainfofunc)
#define IRDES_accprnofunc(c) (g_ir_desc[c].accprnofunc)
#define IRDES_accssfunc(c) (g_ir_desc[c].accssfunc)
#define IRDES_accreslistfunc(c) (g_ir_desc[c].accreslist)
#define IRDES_accresultprfunc(c) (g_ir_desc[c].accresultprfunc)
#define IRDES_acckidfunc(c) (g_ir_desc[c].acckidfunc)
#define IRDES_accbbfunc(c) (g_ir_desc[c].accbbfunc)
#define IRDES_accbasefunc(c) (g_ir_desc[c].accbasefunc)
#define IRDES_acclabfunc(c) (g_ir_desc[c].acclabelfunc)
#define IRDES_accdetfunc(c) (g_ir_desc[c].accjudgedetfunc)
#define IRNAME(ir) (IRDES_name(IR_code(ir)))
#define IRCNAME(irt) (IRDES_name(irt))
#define IRCSIZE(irt) (IRDES_size(irt))
#define IR_MAX_KID_NUM(ir) (IRDES_kid_num(IR_code(ir)))
class IRDesc {
public:
    ///////////////////////////////////////////////////////////////////////////
    //NOTE: DO NOT CHANGE THE LAYOUT OF CLASS MEMBERS BECAUSE THEY ARE       //
    //CORRESPONDING TO THE DEDICATED INITIALIZING VALUE.                     //
    ///////////////////////////////////////////////////////////////////////////
    IR_CODE code; //the unique code of IR.
    CHAR const* name; //the name of IR.

    //The kid_map indicates the existence of each kid of current IR.
    //It is always used in verification of the sanity of IR. Each bit of
    //kid_map indicates whether the corresponding indexed kid can be empty.
    //e.g: the kid_map of IR_IST is 0x3, means the 0th kid and 1th kid can not
    //be emtpy, meanwhile IR_IF's kid_map is 0x1, means only 0th kid can not
    //be emtpy.
    IRKidMap const& kid_map;

    //The number of kid of IR.
    BYTE kid_num;

    //The byte size of the class object of IR.
    UINT size;

    //The attributes of IR.
    IRDescFlag attr;

    //Following function pointer record the corresponding the utility function
    //if exiist for specific IR code.
    IRDumpFuncType dumpfunc; //record the dump function.
    IRVerifyFuncType verifyfunc; //record the verify function.
    IRAccRHSFuncType accrhsfunc; //record the getRHS function.
    IRAccIdinfoFuncType accidinfofunc; //record the getIdinfo function.
    IRAccOfstFuncType accofstfunc; //record the getOfst function.
    IRAccSSAInfoFuncType accssainfofunc; //record the getSSAInfo function.
    IRAccPrnoFuncType accprnofunc; //record the getPrno function.
    IRAccResultPRFuncType accresultprfunc; //record the getResultPR function.
    IRAccKidFuncType acckidfunc; //record the getKid function.
    IRAccBBFuncType accbbfunc; //record the getBB function.
    IRAccBaseFuncType accbasefunc; //record the getBase function.
    IRAccLabFuncType acclabelfunc; //record the getLabel function.
    IRAccDetFuncType accjudgedetfunc; //record the getJudgeDet function.
    IRAccStorageSpaceFuncType accssfunc; //record the getStorageSpace function.
    IRAccResListFuncType accreslist; //record the getResList function.
public:
    //Return true if the No.kididx kid of operation 'irc' can not be NULL.
    static bool mustExist(IR_CODE irc, UINT kididx);
};
//END IRDesc


//Defined rounding type that CVT operation used.
typedef enum _ROUND_TYPE {
    ROUND_UNDEF = 0,

    //Rounding down (or take the floor, or round towards minus infinity)
    ROUND_DOWN,

    //Rounding up (or take the ceiling, or round towards plus infinity)
    ROUND_UP,

    //Rounding towards zero (or truncate, or round away from infinity)
    ROUND_TOWARDS_ZERO,

    //Rounding away from zero (or round towards infinity)
    ROUND_AWAY_FROM_ZERO,

    //Rounding to the nearest integer
    ROUND_TO_NEAREST_INTEGER,

    //Rounding half up
    ROUND_HALF_UP,

    //Rounding half down
    ROUND_HALF_DOWN,

    //Rounding half towards zero
    ROUND_HALF_TOWARDS_ZERO,

    //Rounding half away from zero
    ROUND_HALF_AWAY_FROM_ZERO,

    //Rounding half to even
    ROUND_HALF_TO_EVEN,

    //Rounding half to odd
    ROUND_HALF_TO_ODD,
    ROUND_TYPE_NUM,
} ROUND_TYPE;

#define ROUND_NAME(r) (ROUNDDESC_name(g_round_desc[(r)]))
#define ROUNDDESC_type(r) ((r).type)
#define ROUNDDESC_name(r) ((r).name)
class RoundDesc {
public:
    //Note: do not change the layout of members because they are
    //corresponding to the special initializing value.
    ROUND_TYPE type;
    CHAR const* name;
};

//Exported Variables.
extern IRDesc g_ir_desc[]; //May be modified at initialization of flag set.
extern RoundDesc const g_round_desc[];

bool checkIRDesc();
bool checkRoundDesc();

} //namespace xoc

#endif
