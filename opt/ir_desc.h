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
class IR;
class IRBB;
class DumpFlag;
class IRDumpCtx;

#define NODUMPFUNC nullptr
#define NOVERIFYFUNC nullptr
#define NOACCRHSFUNC nullptr
#define NOACCIDINFOFUNC nullptr
#define NOACCOFSTFUNC nullptr
#define NOACCSSAINFOFUNC nullptr
#define NOACCPRNOFUNC nullptr
#define NOACCRESPRFUNC nullptr
#define NOACCKIDFUNC nullptr
#define NOACCBBFUNC nullptr
#define NOACCBASEFUNC nullptr
#define NOACCLABFUNC nullptr
#define NOACCDETFUNC nullptr
#define NOACCSSFUNC nullptr

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

//Describe miscellaneous information for IR.
enum IRDESC_FLAG {
    IRC_IS_STMT = 0x1, //statement.
    IRC_IS_BIN = 0x2, //binary operation.
    IRC_IS_UNA = 0x4, //unary operation.
    
    //Memory reference operation. Memory reference indicates all
    //operations which write or load memory object.
    IRC_IS_MEM_REF = 0x8,

    //Memory operand indicates all operations which only load memory object.
    IRC_IS_MEM_OPND = 0x10,

    //Indicates the operation satifies arithmetic associative.
    IRC_IS_ASSOCIATIVE = 0x20,

    //Indicates the operation satifies arithmetic commutative.
    IRC_IS_COMMUTATIVE = 0x40,

    //Indicates the operation is relation operation.
    IRC_IS_RELATION = 0x80,

    //Indicates the operation is logical operation.
    IRC_IS_LOGICAL = 0x100,

    //Indicates the operation does not have any kid.
    IRC_IS_LEAF = 0x200,

    //Indicates the operation generates output result.
    IRC_HAS_RESULT = 0x400,

    //Indicates the operation can be placed into Basic Block.
    IRC_IS_STMT_IN_BB = 0x800,

    //Indicates the operation is nonpr memory operation.
    IRC_IS_NON_PR_MEMREF = 0x1000,

    //Indicates the operation has mustref and mayref.
    IRC_HAS_DU = 0x2000,

    //Indicates the operation is write-pr operation.
    IRC_IS_WRITE_PR = 0x4000,

    //Indicates the operation is write-pr operation and its result will
    //modifying whole PR.
    IRC_IS_WRITE_WHOLE_PR = 0x8000,

    //Indicates the operation is memory operation and has offset.
    IRC_HAS_OFFSET = 0x10000,

    //Indicates the operation has identifier information.
    IRC_HAS_IDINFO = 0x20000,

    //Indicates the operation is directly accesssing memory through idinfo.
    IRC_IS_DIRECT_MEM_OP = 0x40000,

    //Indicates the operation is indirectly accesssing memory through base.
    IRC_IS_INDIRECT_MEM_OP = 0x80000,

    //Indicates the operation is conditional branch operation.
    IRC_IS_CONDITIONAL_BR = 0x100000,

    //Indicates the operation is unconditional branch operation.
    IRC_IS_UNCONDITIONAL_BR = 0x200000,

    //Indicates the operation is memory operation that accessing through
    //array style.
    IRC_IS_ARRAY_OP = 0x400000,

    //Indicates the operation is memory operation and it has a RHS expression.
    IRC_HAS_RHS = 0x800000,

    //Indicates the operation has determinate expression
    IRC_HAS_JUDGE_TARGET = 0x1000000,

    //Indicates the operation has a case list.
    IRC_HAS_CASE_LIST = 0x2000000,

    //Indicates the operation has storage space information.
    IRC_HAS_STORAGE_SPACE = 0x4000000,
};

class IRDescFlag : public UFlag {
public:
    IRDescFlag(UINT v) : UFlag(v) {}
};

#define IRDES_code(m) ((m).code)
#define IRDES_name(m) ((m).name)
#define IRDES_kid_map(m) ((m).kid_map)
#define IRDES_kid_num(m) ((m).kid_num)
#define IRDES_is_stmt(m) ((m).attr.have(IRC_IS_STMT))
#define IRDES_is_bin(m) ((m).attr.have(IRC_IS_BIN))
#define IRDES_is_una(m) ((m).attr.have(IRC_IS_UNA))
#define IRDES_is_mem_ref(m) ((m).attr.have(IRC_IS_MEM_REF))
#define IRDES_is_mem_opnd(m) ((m).attr.have(IRC_IS_MEM_OPND))
#define IRDES_is_associative(m) ((m).attr.have(IRC_IS_ASSOCIATIVE))
#define IRDES_is_commutative(m) ((m).attr.have(IRC_IS_COMMUTATIVE))
#define IRDES_is_relation(m) ((m).attr.have(IRC_IS_RELATION))
#define IRDES_is_logical(m) ((m).attr.have(IRC_IS_LOGICAL))
#define IRDES_is_leaf(m) ((m).attr.have(IRC_IS_LEAF))
#define IRDES_is_stmt_in_bb(m) ((m).attr.have(IRC_IS_STMT_IN_BB))
#define IRDES_is_non_pr_memref(m) ((m).attr.have(IRC_IS_NON_PR_MEMREF))
#define IRDES_has_result(m) ((m).attr.have(IRC_HAS_RESULT))
#define IRDES_has_offset(m) ((m).attr.have(IRC_HAS_OFFSET))
#define IRDES_has_idinfo(m) ((m).attr.have(IRC_HAS_IDINFO))
#define IRDES_has_du(m) ((m).attr.have(IRC_HAS_DU))
#define IRDES_has_rhs(m) ((m).attr.have(IRC_HAS_RHS))
#define IRDES_has_judge_target(m) ((m).attr.have(IRC_HAS_JUDGE_TARGET))
#define IRDES_has_storage_space(m) ((m).attr.have(IRC_HAS_STORAGE_SPACE))
#define IRDES_has_case_list(m) ((m).attr.have(IRC_HAS_CASE_LIST))
#define IRDES_is_write_pr(m) ((m).attr.have(IRC_IS_WRITE_PR))
#define IRDES_is_write_whole_pr(m) ((m).attr.have(IRC_IS_WRITE_WHOLE_PR))
#define IRDES_is_conditional_br(m) ((m).attr.have(IRC_IS_CONDITIONAL_BR))
#define IRDES_is_unconditional_br(m) ((m).attr.have(IRC_IS_UNCONDITIONAL_BR))
#define IRDES_is_array_op(m) ((m).attr.have(IRC_IS_ARRAY_OP))
#define IRDES_is_direct_mem_op(m) ((m).attr.have(IRC_IS_DIRECT_MEM_OP))
#define IRDES_is_indirect_mem_op(m) ((m).attr.have(IRC_IS_INDIRECT_MEM_OP))
#define IRDES_size(m) ((m).size)
#define IRDES_dumpfunc(m) ((m).dumpfunc)
#define IRDES_verifyfunc(m) ((m).verifyfunc)
#define IRDES_accrhsfunc(m) ((m).accrhsfunc)
#define IRDES_accidinfofunc(m) ((m).accidinfofunc)
#define IRDES_accofstfunc(m) ((m).accofstfunc)
#define IRDES_accssainfofunc(m) ((m).accssainfofunc)
#define IRDES_accprnofunc(m) ((m).accprnofunc)
#define IRDES_accssfunc(m) ((m).accssfunc)
#define IRDES_accresultprfunc(m) ((m).accresultprfunc)
#define IRDES_acckidfunc(m) ((m).acckidfunc)
#define IRDES_accbbfunc(m) ((m).accbbfunc)
#define IRDES_accbasefunc(m) ((m).accbasefunc)
#define IRDES_acclabfunc(m) ((m).acclabelfunc)
#define IRDES_accdetfunc(m) ((m).accjudgedetfunc)
#define IRNAME(ir) (IRDES_name(g_ir_desc[IR_code(ir)]))
#define IRCNAME(irt) (IRDES_name(g_ir_desc[irt]))
#define IRCSIZE(irt) (IRDES_size(g_ir_desc[irt]))
#define IR_MAX_KID_NUM(ir) (IRDES_kid_num(g_ir_desc[IR_code(ir)]))
class IRDesc {
public:
    ///////////////////////////////////////////////////////////////////////////
    //NOTE: DO NOT CHANGE THE LAYOUT OF CLASS MEMBERS BECAUSE THEY ARE       //
    //CORRESPONDING TO THE DEDICATED INITIALIZING VALUE.                     //
    ///////////////////////////////////////////////////////////////////////////
    IR_CODE code;
    CHAR const* name;
    BYTE kid_map;
    BYTE kid_num;
    BYTE size;
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
extern IRDesc const g_ir_desc[];
extern RoundDesc const g_round_desc[];

bool checkIRDesc();
bool checkRoundDesc();
