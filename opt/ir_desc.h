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

typedef UINT64 IRDescFlagSeg;
#define IR_DESC_FLAG_BYTE_SIZE (sizeof(IRDescFlagSeg) * 1)

class IR;
class IRBB;
class DumpFlag;
class IRDumpCtx;

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

typedef void(*IRDumpFuncType)(IR const* ir, Region const* rg, IRDumpCtx & ctx);
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

//Describe miscellaneous information for IR.
#define IRC_IS_STMT_SHIFT 0
#define IRC_IS_STMT 1ULL<<IRC_IS_STMT_SHIFT //statement.

#define IRC_IS_TER_SHIFT 1
#define IRC_IS_TER 1ULL<<IRC_IS_TER_SHIFT //ternary operation.

#define IRC_IS_BIN_SHIFT 2
#define IRC_IS_BIN 1ULL<<IRC_IS_BIN_SHIFT //binary operation.

#define IRC_IS_UNA_SHIFT 3
#define IRC_IS_UNA 1ULL<<IRC_IS_UNA_SHIFT //unary operation.

//Memory reference operation. Memory reference indicates all
//operations which write or load memory object.
#define IRC_IS_MEM_REF_SHIFT 4
#define IRC_IS_MEM_REF 1ULL<<IRC_IS_MEM_REF_SHIFT

//Memory operand indicates all operations which only load memory object.
#define IRC_IS_MEM_OPND_SHIFT 5
#define IRC_IS_MEM_OPND 1ULL<<IRC_IS_MEM_OPND_SHIFT

//Indicates the operation satifies arithmetic associative.
#define IRC_IS_ASSOCIATIVE_SHIFT 6
#define IRC_IS_ASSOCIATIVE 1ULL<<IRC_IS_ASSOCIATIVE_SHIFT

//Indicates the operation satifies arithmetic commutative.
#define IRC_IS_COMMUTATIVE_SHIFT 7
#define IRC_IS_COMMUTATIVE 1ULL<<IRC_IS_COMMUTATIVE_SHIFT

//Indicates the operation is relation operation.
#define IRC_IS_RELATION_SHIFT 8
#define IRC_IS_RELATION 1ULL<<IRC_IS_RELATION_SHIFT

//Indicates the operation is logical operation.
#define IRC_IS_LOGICAL_SHIFT 9
#define IRC_IS_LOGICAL 1ULL<<IRC_IS_LOGICAL_SHIFT

//Indicates the operation does not have any kid.
#define IRC_IS_LEAF_SHIFT 10
#define IRC_IS_LEAF 1ULL<<IRC_IS_LEAF_SHIFT

//Indicates the operation generates output result.
#define IRC_HAS_RESULT_SHIFT 11
#define IRC_HAS_RESULT 1ULL<<IRC_HAS_RESULT_SHIFT

//Indicates the operation can be placed into Basic Block.
#define IRC_IS_STMT_IN_BB_SHIFT 12
#define IRC_IS_STMT_IN_BB 1ULL<<IRC_IS_STMT_IN_BB_SHIFT

//Indicates the operation is nonpr memory operation.
#define IRC_IS_NON_PR_MEMREF_SHIFT 13
#define IRC_IS_NON_PR_MEMREF 1ULL<<IRC_IS_NON_PR_MEMREF_SHIFT

//Indicates the operation has mustref and mayref.
#define IRC_HAS_DU_SHIFT 14
#define IRC_HAS_DU 1ULL<<IRC_HAS_DU_SHIFT

//Indicates the operation is write-pr operation.
#define IRC_IS_WRITE_PR_SHIFT 15
#define IRC_IS_WRITE_PR 1ULL<<IRC_IS_WRITE_PR_SHIFT

//Indicates the operation is write-pr operation and its result will
//modifying whole PR.
#define IRC_IS_WRITE_WHOLE_PR_SHIFT 16
#define IRC_IS_WRITE_WHOLE_PR 1ULL<<IRC_IS_WRITE_WHOLE_PR_SHIFT

//Indicates the operation is memory operation and has offset.
#define IRC_HAS_OFFSET_SHIFT 17
#define IRC_HAS_OFFSET 1ULL<<IRC_HAS_OFFSET_SHIFT

//Indicates the operation has identifier information.
#define IRC_HAS_IDINFO_SHIFT 18
#define IRC_HAS_IDINFO 1ULL<<IRC_HAS_IDINFO_SHIFT

//Indicates the operation is directly accesssing memory through idinfo.
#define IRC_IS_DIRECT_MEM_OP_SHIFT 19
#define IRC_IS_DIRECT_MEM_OP 1ULL<<IRC_IS_DIRECT_MEM_OP_SHIFT

//Indicates the operation is indirectly accesssing memory through base.
#define IRC_IS_INDIRECT_MEM_OP_SHIFT 20
#define IRC_IS_INDIRECT_MEM_OP 1ULL<<IRC_IS_INDIRECT_MEM_OP_SHIFT

//Indicates the operation is conditional branch operation.
#define IRC_IS_CONDITIONAL_BR_SHIFT 21
#define IRC_IS_CONDITIONAL_BR 1ULL<<IRC_IS_CONDITIONAL_BR_SHIFT

//Indicates the operation is unconditional branch operation.
#define IRC_IS_UNCONDITIONAL_BR_SHIFT 22
#define IRC_IS_UNCONDITIONAL_BR 1ULL<<IRC_IS_UNCONDITIONAL_BR_SHIFT

//Indicates the operation is memory operation that accessing through
//array style.
#define IRC_IS_ARRAY_OP_SHIFT 23
#define IRC_IS_ARRAY_OP 1ULL<<IRC_IS_ARRAY_OP_SHIFT

//Indicates the operation is memory operation and it has a RHS expression.
#define IRC_HAS_RHS_SHIFT 24
#define IRC_HAS_RHS 1ULL<<IRC_HAS_RHS_SHIFT

//Indicates the operation has determinate expression
#define IRC_HAS_JUDGE_TARGET_SHIFT 25
#define IRC_HAS_JUDGE_TARGET 1ULL<<IRC_HAS_JUDGE_TARGET_SHIFT

//Indicates the operation has a case list.
#define IRC_HAS_CASE_LIST_SHIFT 26
#define IRC_HAS_CASE_LIST 1ULL<<IRC_HAS_CASE_LIST_SHIFT

//Indicates the operation has storage space information.
#define IRC_HAS_STORAGE_SPACE_SHIFT 27
#define IRC_HAS_STORAGE_SPACE 1ULL<<IRC_HAS_STORAGE_SPACE_SHIFT

//Indicates the operation has multiple-result list.
#define IRC_HAS_RES_LIST_SHIFT 28
#define IRC_HAS_RES_LIST 1ULL<<IRC_HAS_RES_LIST_SHIFT

////////////////////////////////////////////////////////////////////////////////
//NOTE: If new IR flag value is greater than the bit range that IRDescFlagSeg //
//can express, user should extend the IR_DESC_FLAG_BYTE_SIZE value and set the//
//big flag value at RegionMgr::initIRDescFlagSet(), then invoke the function  //
//right after RegionMgr created.                                              //
////////////////////////////////////////////////////////////////////////////////

class IRDescFlag : public xcom::FixedSizeBitSet {
protected:
    BYTE m_flagbuf[IR_DESC_FLAG_BYTE_SIZE];
public:
    IRDescFlag() : FixedSizeBitSet(m_flagbuf, IR_DESC_FLAG_BYTE_SIZE) {}
    IRDescFlag(IRDescFlagSeg v) :
        FixedSizeBitSet(m_flagbuf, IR_DESC_FLAG_BYTE_SIZE)
    {
        ASSERT0(IR_DESC_FLAG_BYTE_SIZE >= sizeof(v));
        ::memcpy(m_flagbuf, &v, sizeof(v));
    }

    //Return true if current flagset includes 'v', which 'v' may contain
    //combined flags.
    //e.g: if v is 0x5, it indicates v is combined with 0b100 and 0b1.
    bool have(BSIdx v) const { return is_contain(v); }

    //Return true if current flagset only includes 'v'.
    bool only_have(BSIdx v) const { return have(v) && get_elem_count() == 1; }

    //The function removes flag out of flagset.
    void remove(BSIdx v) { diff(v); }

    //The function adds new flag into flagset.
    void set(BSIdx v) { bunion(v); }
};

#define IRDES_code(c) (g_ir_desc[c].code)
#define IRDES_name(c) (g_ir_desc[c].name)
#define IRDES_kid_map(c) (g_ir_desc[c].kid_map)
#define IRDES_kid_num(c) (g_ir_desc[c].kid_num)
#define IRDES_attr(c) (g_ir_desc[c].attr)
#define IRDES_is_stmt(c) (IRDES_attr(c).have(IRC_IS_STMT_SHIFT))
#define IRDES_is_ter(c) (IRDES_attr(c).have(IRC_IS_TER_SHIFT))
#define IRDES_is_bin(c) (IRDES_attr(c).have(IRC_IS_BIN_SHIFT))
#define IRDES_is_una(c) (IRDES_attr(c).have(IRC_IS_UNA_SHIFT))
#define IRDES_is_mem_ref(c) (IRDES_attr(c).have(IRC_IS_MEM_REF_SHIFT))
#define IRDES_is_mem_opnd(c) (IRDES_attr(c).have(IRC_IS_MEM_OPND_SHIFT))
#define IRDES_is_associative(c) (IRDES_attr(c).have(IRC_IS_ASSOCIATIVE_SHIFT))
#define IRDES_is_commutative(c) (IRDES_attr(c).have(IRC_IS_COMMUTATIVE_SHIFT))
#define IRDES_is_relation(c) (IRDES_attr(c).have(IRC_IS_RELATION_SHIFT))
#define IRDES_is_logical(c) (IRDES_attr(c).have(IRC_IS_LOGICAL_SHIFT))
#define IRDES_is_leaf(c) (IRDES_attr(c).have(IRC_IS_LEAF_SHIFT))
#define IRDES_is_stmt_in_bb(c) (IRDES_attr(c).have(IRC_IS_STMT_IN_BB_SHIFT))
#define IRDES_is_non_pr_memref(c) \
    (IRDES_attr(c).have(IRC_IS_NON_PR_MEMREF_SHIFT))
#define IRDES_has_result(c) (IRDES_attr(c).have(IRC_HAS_RESULT_SHIFT))
#define IRDES_has_offset(c) (IRDES_attr(c).have(IRC_HAS_OFFSET_SHIFT))
#define IRDES_has_idinfo(c) (IRDES_attr(c).have(IRC_HAS_IDINFO_SHIFT))
#define IRDES_has_du(c) (IRDES_attr(c).have(IRC_HAS_DU_SHIFT))
#define IRDES_has_rhs(c) (IRDES_attr(c).have(IRC_HAS_RHS_SHIFT))
#define IRDES_has_res_list(c) (IRDES_attr(c).have(IRC_HAS_RES_LIST_SHIFT))
#define IRDES_has_judge_target(c) \
    (IRDES_attr(c).have(IRC_HAS_JUDGE_TARGET_SHIFT))
#define IRDES_has_storage_space(c) \
    (IRDES_attr(c).have(IRC_HAS_STORAGE_SPACE_SHIFT))
#define IRDES_has_case_list(c) (IRDES_attr(c).have(IRC_HAS_CASE_LIST_SHIFT))
#define IRDES_is_write_pr(c) (IRDES_attr(c).have(IRC_IS_WRITE_PR_SHIFT))
#define IRDES_is_write_whole_pr(c) \
    (IRDES_attr(c).have(IRC_IS_WRITE_WHOLE_PR_SHIFT))
#define IRDES_is_conditional_br(c) \
    (IRDES_attr(c).have(IRC_IS_CONDITIONAL_BR_SHIFT))
#define IRDES_is_unconditional_br(c) \
    (IRDES_attr(c).have(IRC_IS_UNCONDITIONAL_BR_SHIFT))
#define IRDES_is_array_op(c) \
    (IRDES_attr(c).have(IRC_IS_ARRAY_OP_SHIFT))
#define IRDES_is_direct_mem_op(c) \
    (IRDES_attr(c).have(IRC_IS_DIRECT_MEM_OP_SHIFT))
#define IRDES_is_indirect_mem_op(c) \
    (IRDES_attr(c).have(IRC_IS_INDIRECT_MEM_OP_SHIFT))
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
    BYTE kid_map;

    //The number of kid of IR.
    BYTE kid_num;

    //The byte size of the class object of IR.
    BYTE size;

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
