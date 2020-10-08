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
#ifndef __IR_H__
#define __IR_H__

namespace xoc {

class SimpCtx;
class IRBB;
class DU;
class SSAInfo;
class MDSSAInfo;
class MDPhi;
class IRCFG;

typedef List<IRBB*> BBList;
typedef xcom::C<IRBB*> * BBListIter;
typedef List<IR const*> ConstIRIter;
typedef List<IR*> IRIter;

//Map IR to its Holder during instrument operation.
typedef xcom::TMap<IR*, xcom::C<IR*>*> IR2Holder;
typedef xcom::EList<IR*, IR2Holder> IRList;

//IR type
typedef enum {
    IR_UNDEF =      0,
    IR_CONST =      1,  //Constant value: include integer, float, string.
    IR_ID =         2,  //Identifier of variable.
    IR_LD =         3,  //Load from variable
    IR_ILD =        4,  //Indirect load.
    IR_PR =         5,  //Temporary Pseudo Register which can NOT be
                        //taken address, and can be regarded as both
                        //register and memory.
    IR_ARRAY =      6,  //Array operation, include base and ofst.
    IR_ST =         7,  //Store to variable.
    IR_STPR =       8,  //Store to PR.
    IR_STARRAY =    9,  //Store to array.
    IR_IST =        10, //Indirect store.
    IR_SETELEM =    11, //Set element of PR, where PR is memory chunk or vector.
    IR_GETELEM =    12, //Get element of PR, where PR is memory chunk or vector.
    IR_CALL =       13, //Direct call.
    IR_ICALL =      14, //Indirect call.
    IR_LDA =        15, //Move variable's address to a register.
    IR_ADD =        16, //Addition.
    IR_SUB =        17, //Substraction.
    IR_MUL =        18, //Multiplication.
    IR_DIV =        19, //Division.
    IR_REM =        20, //Remainder operation
    IR_MOD =        21, //Modulus operation
    IR_LAND =       22, //Logical AND, &&
    IR_LOR =        23, //Logical OR, ||
    IR_BAND =       24, //Bitwise AND, &
    IR_BOR =        25, //Bitwise OR, |
    IR_XOR =        26, //Exclusive OR.
    IR_ASR =        27, //Arithmetic shift right
    IR_LSR =        28, //Logical shift right
    IR_LSL =        29, //Logical shift left
    IR_LT =         30, //Less than.
    IR_LE =         31, //Less than or equal to.
    IR_GT =         32, //Greater than.
    IR_GE =         33, //Greater than or equal to.
    IR_EQ =         34, //Equal to.
    IR_NE =         35, //Not equal to.
    IR_BNOT =       36, //Bitwise not, e.g BNOT(0x0001) = 0xFFFE
    IR_LNOT =       37, //Boolean logical not e.g LNOT(0x0001) = 0x0000
    IR_NEG =        38, //Negative operation.
    IR_CVT =        39, //Data-type convert
    IR_GOTO =       40, //Goto definitely label.
    IR_IGOTO =      41, //Indirect Goto a list of definitely label.
    IR_DO_WHILE =   42, //Do-While loop struct.
    IR_WHILE_DO =   43, //While-Do loop struct.
    IR_DO_LOOP =    44, //A kind of loop with plainly definition of
                        //INIT(low bound), HIGH bound, LOOP-BODY and STEP
                        //of IV.

    IR_IF =         45, //High level IF clasuse, include det,
                        //truebody, and false body

    IR_LABEL =      46, //Describe internal and customer defined label.
    IR_SWITCH =     47, //Switch clause, include determinant expression, a
                        //list of case, and body.

    IR_CASE =       48, //CASE VALUE, this is used only within SWITCH clause.
    IR_TRUEBR =     49, //Branch if determinant express is true.
    IR_FALSEBR =    50, //Branch if determinant express is false.
    IR_RETURN =     51, //Return Statement.

    IR_SELECT =     52, //Conditional select true-exp or false-exp , formalized as :
                        //determinant expression ? true-exp : false-exp

    IR_BREAK =      53, //Terminate current loop end switch execution, which
                        //include do-loop, do-while, while-do, and switch stmt.

    IR_CONTINUE =   54, //Re-execute loop, which
                        //include do-loop, do-while, while-do.

    IR_PHI =        55, //Phi statement.
    IR_REGION =     56, //Region statement.

    //DO NOT ADD NEW IR Type AFTER THIS ONE.
    IR_TYPE_NUM =   57  //The last IR type, the number of IR type.

    //////////////////////////////////////////////////////////////////
    //NOTE: Extend IR::ir_type bit length if type value large than 63.
    //////////////////////////////////////////////////////////////////
} IR_TYPE;

#define SWITCH_CASE_BIN \
    case IR_ADD: \
    case IR_SUB: \
    case IR_MUL: \
    case IR_DIV: \
    case IR_REM: \
    case IR_MOD: \
    case IR_LAND:\
    case IR_LOR: \
    case IR_BAND:\
    case IR_BOR: \
    case IR_XOR: \
    case IR_ASR: \
    case IR_LSR: \
    case IR_LSL: \
    case IR_LT:  \
    case IR_LE:  \
    case IR_GT:  \
    case IR_GE:  \
    case IR_EQ:  \
    case IR_NE

#define SWITCH_CASE_UNA \
    case IR_BNOT: \
    case IR_LNOT: \
    case IR_NEG:  \
    case IR_CVT

#define SWITCH_CASE_STMT_IN_BB_NO_KID \
    case IR_REGION: \
    case IR_GOTO: \
    case IR_LABEL

#define SWITCH_CASE_STMT_NO_KID \
    SWITCH_CASE_STMT_IN_BB_NO_KID: \
    case IR_BREAK: \
    case IR_CONTINUE

#define SWITCH_CASE_EXP_NO_KID \
    case IR_ID: \
    case IR_LD: \
    case IR_PR: \
    case IR_LDA: \
    case IR_CONST

//Describe miscellaneous information for IR.
#define IRT_IS_STMT 0x1 //statement.
#define IRT_IS_BIN 0x2 //binary operation.
#define IRT_IS_UNA 0x4 //unary operation.

//Memory reference operation. memory reference indicate all
//operation which write or load memory object.
#define IRT_IS_MEM_REF 0x8

//Memory operand indicate all operation which only load memory object.
#define IRT_IS_MEM_OPND 0x10
#define IRT_IS_ASSOCIATIVE 0x20
#define IRT_IS_COMMUTATIVE 0x40
#define IRT_IS_RELATION 0x80
#define IRT_IS_LOGICAL 0x100
#define IRT_IS_LEAF 0x200
#define IRT_HAS_RESULT 0x400
#define IRT_IS_STMT_IN_BB 0x800
#define IRT_IS_NON_PR_MEMREF 0x1000
#define IRT_HAS_DU 0x2000
#define IRT_WRITE_PR 0x4000
#define IRT_WRITE_WHOLE_PR 0x8000
#define IRT_HAS_OFFSET 0x10000
#define IRT_HAS_IDINFO 0x20000

#define IRDES_code(m) ((m).code)
#define IRDES_name(m) ((m).name)
#define IRDES_kid_map(m) ((m).kid_map)
#define IRDES_kid_num(m) ((m).kid_num)
#define IRDES_is_stmt(m) (HAVE_FLAG(((m).attr), IRT_IS_STMT))
#define IRDES_is_bin(m) (HAVE_FLAG(((m).attr), IRT_IS_BIN))
#define IRDES_is_una(m) (HAVE_FLAG(((m).attr), IRT_IS_UNA))
#define IRDES_is_mem_ref(m) (HAVE_FLAG(((m).attr), IRT_IS_MEM_REF))
#define IRDES_is_mem_opnd(m) (HAVE_FLAG(((m).attr), IRT_IS_MEM_OPND))
#define IRDES_is_associative(m) (HAVE_FLAG(((m).attr), IRT_IS_ASSOCIATIVE))
#define IRDES_is_commutative(m) (HAVE_FLAG(((m).attr), IRT_IS_COMMUTATIVE))
#define IRDES_is_relation(m) (HAVE_FLAG(((m).attr), IRT_IS_RELATION))
#define IRDES_is_logical(m) (HAVE_FLAG(((m).attr), IRT_IS_LOGICAL))
#define IRDES_is_leaf(m) (HAVE_FLAG(((m).attr), IRT_IS_LEAF))
#define IRDES_is_stmt_in_bb(m) (HAVE_FLAG(((m).attr), IRT_IS_STMT_IN_BB))
#define IRDES_is_non_pr_memref(m) (HAVE_FLAG(((m).attr), IRT_IS_NON_PR_MEMREF))
#define IRDES_has_result(m) (HAVE_FLAG(((m).attr), IRT_HAS_RESULT))
#define IRDES_has_offset(m) (HAVE_FLAG(((m).attr), IRT_HAS_OFFSET))
#define IRDES_has_idinfo(m) (HAVE_FLAG(((m).attr), IRT_HAS_IDINFO))
#define IRDES_has_du(m) (HAVE_FLAG(((m).attr), IRT_HAS_DU))
#define IRDES_is_write_pr(m) (HAVE_FLAG(((m).attr), IRT_WRITE_PR))
#define IRDES_is_write_whole_pr(m) (HAVE_FLAG(((m).attr), IRT_WRITE_WHOLE_PR))
#define IRDES_size(m) ((m).size)
class IRDesc {
public:
    //Note: do not change the layout of members because they are
    //corresponding to the special initializing value.
    IR_TYPE code;
    CHAR const* name;
    BYTE kid_map;
    BYTE kid_num;
    BYTE size;
    UINT attr;
};


typedef Hash<IR*> IRAddressHash;

#ifdef _DEBUG_
INT checkKidNumValid(IR const* ir, UINT n, CHAR const* file, INT lineno);
INT checkKidNumValidCall(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidArray(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidLoop(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumValidBranch(IR const* ir,
                           UINT n,
                           CHAR const* filename,
                           INT line);
INT checkKidNumValidBinary(IR const* ir,
                           UINT n,
                           CHAR const* filename,
                           INT line);
INT checkKidNumValidUnary(IR const* ir, UINT n, CHAR const* filename, INT line);
INT checkKidNumIRtype(IR const* ir,
                      UINT n,
                      IR_TYPE irty,
                      CHAR const* filename,
                      INT line);
IR const* checkIRT(IR const* ir, IR_TYPE irt);
IR const* checkIRTBranch(IR const* ir);
IR const* checkIRTCall(IR const* ir);
IR const* checkIRTArray(IR const* ir);
IR const* checkIRTOnlyCall(IR const* ir);
IR const* checkIRTOnlyICall(IR const* ir);
UINT checkArrayDimension(IR const* ir, UINT n);
#endif

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

#ifdef _DEBUG_
#define CK_KID_NUM(ir, n, f, l) (checkKidNumValid(ir, n, f, l))
#define CK_KID_NUM_IRTY(ir, n, irty, f, l) \
    (checkKidNumIRtype(ir, n, irty, f, l))
#define CK_KID_NUM_UNA(ir, n, f, l) (checkKidNumValidUnary(ir, n, f, l))
#define CK_KID_NUM_BIN(ir, n, f, l) (checkKidNumValidBinary(ir, n, f, l))
#define CK_KID_NUM_BR(ir, n, f, l) (checkKidNumValidBranch(ir, n, f, l))
#define CK_KID_NUM_LOOP(ir, n, f, l) (checkKidNumValidLoop(ir, n, f, l))
#define CK_KID_NUM_CALL(ir, n, f, l) (checkKidNumValidCall(ir, n, f, l))
#define CK_KID_NUM_ARR(ir, n, f, l) (checkKidNumValidArray(ir, n, f, l))
#define CK_IRT(ir, irt) (checkIRT(ir, irt))
#define CK_IRT_BR(ir) (checkIRTBranch(ir))
#define CK_IRT_CALL(ir) (checkIRTCall(ir))
#define CK_IRT_ARR(ir) (checkIRTArray(ir))
#define CK_IRT_ONLY_CALL(ir) (checkIRTOnlyCall(ir))
#define CK_IRT_ONLY_ICALL(ir) (checkIRTOnlyICall(ir))
#define CK_ARRAY_DIM(ir, n) (checkArrayDimension(ir, n))
#else
#define CK_KID_NUM(ir, n, f, l) (n)
#define CK_KID_NUM_IRTY(ir, n, irty, f, l) (n)
#define CK_KID_NUM_UNA(ir, n, f, l) (n)
#define CK_KID_NUM_BIN(ir, n, f, l) (n)
#define CK_KID_NUM_BR(ir, n, f, l) (n)
#define CK_KID_NUM_LOOP(ir, n, f, l) (n)
#define CK_KID_NUM_CALL(ir, n, f, l) (n)
#define CK_KID_NUM_ARR(ir, n, f, l) (n)
#define CK_IRT(ir, irt) (ir)
#define CK_IRT_BR(ir) (ir)
#define CK_IRT_CALL(ir) (ir)
#define CK_IRT_ARR(ir) (ir)
#define CK_IRT_ONLY_CALL(ir) (ir)
#define CK_IRT_ONLY_ICALL(ir) (ir)
#define CK_ARRAY_DIM(ir, n) (n)
#endif

#define CKID_TY(ir, irty, n) CK_KID_NUM_IRTY(ir, n, irty, __FILE__, __LINE__)
#define CKID_BR(ir, n) CK_KID_NUM_BR(ir, n, __FILE__, __LINE__)
#define CKID_LOOP(ir, n) CK_KID_NUM_LOOP(ir, n, __FILE__, __LINE__)
#define CKID_UNA(ir, n) CK_KID_NUM_UNA(ir, n, __FILE__, __LINE__)
#define CKID_BIN(ir, n) CK_KID_NUM_BIN(ir, n, __FILE__, __LINE__)
#define CKID_CALL(ir, n) CK_KID_NUM_CALL(ir, n, __FILE__, __LINE__)
#define CKID_ARR(ir, n) CK_KID_NUM_ARR(ir, n, __FILE__, __LINE__)

//Used by all IR.
#define IR_DUMP_KID 0x1
#define IR_DUMP_SRC_LINE 0x2
#define IR_DUMP_ADDR 0x4
#define IR_DUMP_INNER_REGION 0x8
#define IR_DUMP_VAR_DECL 0x10

//The maximum integer value that can described by bits of IR_TYPE_BIT_SIZE
//should larger than IR_TYPE_NUM.
#define IR_TYPE_BIT_SIZE 6
#define IRNAME(ir) (IRDES_name(g_ir_desc[IR_code(ir)]))
#define IRTNAME(irt) (IRDES_name(g_ir_desc[irt]))
#define IRTSIZE(irt) (IRDES_size(g_ir_desc[irt]))

#define IR_MAX_KID_NUM(ir) (IRDES_kid_num(g_ir_desc[IR_code(ir)]))

//Each IR at same Region has it own unique id.
#define IR_id(ir) ((ir)->uid)

//Record result data type.
#define IR_dt(ir) ((ir)->result_data_type)

//Record if ir might throw exception.
#define IR_may_throw(ir) ((ir)->may_throw_exception)

//Indicate IR will terminate current control flow.
//If this flag is true, the code that followed subsequently is unreachable.
#define IR_is_terminate(ir) ((ir)->is_terminate_control_flow)

//Record IR type.
#define IR_code(ir) ((ir)->code)

//Access parent IR.
#define IR_parent(ir) ((ir)->parent)

//Access next IR.
#define IR_next(ir) ((ir)->next)

//Access prev IR.
#define IR_prev(ir) ((ir)->prev)

//Record attached info container.
#define IR_ai(ir) ((ir)->attach_info_container)

//This flag describe concurrency semantics.
//True if current operation is atomic. If ir is atomic load, atomic write or
//atomic read-modify-write.
//Read barrier: such as ID/LD/ILD/PR/ARRAY may be regarded as read
//barrier if the flag is true.
//Analogously, ST/STPR/IST/STARRAY/CALL may be regarded as write barrier.
//NOTE: do NOT replace replace an atomic operation with a non-atomic operation.
#define IR_is_atomic(ir) ((ir)->is_atomic_op)

//This flag describe concurrency semantics.
//True if current operation is atomic read-modify-write.
//For given variable, RMW operation read the old value, then compare
//with new value, then write the new value to the variable, finally return
//old value.
//NOTE: The write operation may be failed.
//
//If the variable is volatile, one should
//not change the order of this operation with other memory operations.
//The flag can be used to represent safepoint in code generation, and
//if it is, the IR modified/invalided each pointers previous defined,
//and this cuts off the Def-Use chain of those pointers immediately
//after the IR.
//By default, RMW could be simulated by IR_CALL with 3 arguments,
//e.g: call Opcode:i32, OldValueMemory:<valuetype>, NewValue:valuetype;
//where Opcode defined the RMW operations, OldValueMemory indicates
//the memory location with valuetype that hold old-value, and NewValue
//is the value to be set.
#define IR_is_read_mod_write(ir) ((ir)->is_read_mod_write)

//True if ir has sideeffect. This flag often be used to prevent user
//perform incorrect optimization.
//If ir has sideeffect, that means ir can not be removed,
//but it still can be moved.
#define IR_has_sideeffect(ir) ((ir)->has_sideeffect)

//True if ir can not be moved. This flag often be used to prevent user
//perform incorrect optimization, e.g: LICM.
//If ir is immovable, it also can not be removed
#define IR_no_move(ir) ((ir)->no_move)

//Define this marco if we try to search
//ir in free_ir_tab while invoking allocIR().
//#define CONST_IRT_SZ

#ifdef CONST_IRT_SZ
#define IR_irt_size(ir) ((ir)->irt_size)
#endif

//IR, the intermediate language for the XOC compiler, serves as the common
//interface among almost all the components. IR is defined to be capable of
//representing any level of semantics except the level that corresponds to
//the machine instructions. Two different levels of IR are defined, and each
//optimization phase is defined to work at a specific level of IR. The
//front-ends may generate the highest level of IR. Optimization proceeds
//together with the process of continuous simplification, in which a
//simplification of IR is called to translate IR from the current level to
//the next lower level.
//
//High level IR preserve the high level control flow constructs, such as
//DO_LOOP, DO_WHILE, WHILE_DO, SWITCH, IF, BREAK and CONTINUE.
//Operations can be divided into two categories: statements, and expressions.
//Statement implies which variable is defined, or control flow transfering.
//Expression implies which variable is used, or operation without sideeffect,
//and expression does not transfer control flow. Both statement and expression
//node have NEXT and PREV pointers which link them together.
//Statment can not be kid of other statement except control flow structure IR,
//and expression can be kid of both expression and statement.
//In a simple word, statements have side effects, and can be reordered only
//if dependencies preserved. Expressions do not have side effect, expression
//trees hung from statement, and they contain only uses and can be
//aggressively optimized.
//
//Note IR should not have virtual table because all operations are
//distinguished by IR_TYPE. An IR object might represent differet operation
//in specific scene when it is continually freed and allocated. Diverse
//description should be placed in attach-info.
class IR {
    COPY_CONSTRUCTOR(IR);
public:
    #ifdef _DEBUG_
    IR_TYPE code;
    #else
    USHORT code:IR_TYPE_BIT_SIZE;
    #endif

    //True if IR may throw excetion.
    USHORT may_throw_exception:1;

    //True if IR is atomic operation.
    USHORT is_atomic_op:1;

    //True if IR behaved as if it is an atomic operation consist of
    //sequential read, modify, and write.
    USHORT is_read_mod_write:1;

    //True if IR may terminate the control flow, such as throwing an excetion.
    USHORT is_terminate_control_flow:1;

    //True if IR may have side effect.
    USHORT has_sideeffect:1;

    //True if IR can not be moved.
    USHORT no_move:1;

    #ifdef CONST_IRT_SZ
    //Record the specific IR byte size.
    UINT irt_size:6;
    #endif

    UINT uid; //Each IR has unique id.
    //The type of IR can be void, and depend on
    //the dynamic behavior of program.
    Type const* result_data_type;

    //Both of 'next' and 'prev' used by the process of
    //complicated tree level IR construction.
    IR * next;
    IR * prev;

    //Used in all processs at all level IR.
    //This field should be NULL if IR is the top level of stmt.
    IR * parent;

    //IR may have an unique attach info container.
    AIContainer * attach_info_container;
public:
    //Calculate the accumulated offset value from the base of array.
    //e.g: For given array long long p[10][20],
    //the offset of p[i][j] can be computed by i*20 + j, and
    //the offset of p[i] can be computed by i*20.
    //If all the indice are constant value, calcuate the value, storing
    //in 'ofst_val' and return True, otherwise return False that means the
    //Offset can not be predicated.
    bool calcArrayOffset(TMWORD * ofst, TypeMgr * tm) const;

    //Set ir's DU to be NULL, return the DU pointer.
    inline DU * cleanDU();

    //Set ir's PR SSA Info to be NULL.
    //For convenient purpose, this function does not assert
    //when current IR object is not operate on PR.
    inline void clearSSAInfo();
    void cleanRefMD()
    {
        DU * du = getDU();
        if (du == NULL) { return; }
        DU_md(du) = NULL;
    }

    void cleanRefMDSet()
    {
        DU * du = getDU();
        if (du == NULL) { return; }
        DU_mds(du) = NULL;
    }

    void cleanRef()
    {
        DU * du = getDU();
        if (du == NULL) { return; }
        DU_mds(du) = NULL;
        DU_md(du) = NULL;
    }

    //Count memory usage for current IR.
    size_t count_mem() const;

    //Copy memory reference only for current ir node.
    //src: copy MD reference from 'src', it may be different to current ir.
    void copyRef(IR const* src, Region * rg);

    //Copy AttachInfo from 'src' to current ir, not include kid and sibling.
    void copyAI(IR const* src, Region * rg);

    //Copy each memory reference for whole ir tree.
    //'src': copy MD reference from 'src', it must be equal to current ir tree.
    //'copy_kid_ref': copy MD reference for kid recursively.
    void copyRefForTree(IR const* src, Region * rg)
    {
        ASSERT0(src && isIREqual(src, true) && rg);
        ASSERT0(src != this);
        if (isMemoryRef()) {
            setRefMD(src->getRefMD(), rg);
            setRefMDSet(src->getRefMDSet(), rg);
        }

        for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
            IR * kid = getKid(i);
            if (kid == NULL) { continue; }

            IR * srckid = src->getKid(i);
            ASSERT0(srckid);
            for (; kid != NULL; kid = IR_next(kid), srckid = IR_next(srckid)) {
                ASSERT0(srckid);
                kid->copyRefForTree(srckid, rg);
            }
        }
    }

    void dumpRef(Region * ru, UINT indent);

    //Clean all DU-Chain and Defined/Used-MD reference info.
    inline void freeDUset(DefMiscBitSetMgr & sbs_mgr)
    {
        DU * du = getDU();
        if (du == NULL || DU_duset(du) == NULL) { return; }

        //Free DUSet back to DefSegMgr, or it will
        //complain and make an assertion.
        sbs_mgr.freeSBitSetCore(DU_duset(du));
        DU_duset(du) = NULL;
    }

    IR_TYPE getCode() const { return (IR_TYPE)IR_code(this); }
    IR * get_next() const { return IR_next(this); }
    IR * get_prev() const { return IR_prev(this); }
    inline IR * getBase() const; //Get base expression if exist.
    inline UINT getOffset() const; //Get byte offset if any.
    inline Var * getIdinfo() const; //Get idinfo if any.
    IR * getParent() const { return IR_parent(this); }
    inline IR * getKid(UINT idx) const;
    inline IRBB * getBB() const;
    inline DU * getDU() const;

    //Return STMT if current ir is expression.
    //e.g:  st(i32 a)
    //         ld(i32 b)
    //If given expression is ld, this function return st stmt.
    //Note if there are high level stmts, such as:
    //    if (det)
    //      st:i32 a
    //        ld:i32 b
    //    endif
    //This function only return the nearest stmt to ld:i32 b, namely, st:i32 a.
    inline IR * getStmt() const
    {
        ASSERT0(!is_undef());
        ASSERTN(!is_stmt(), ("IR already be stmt, it is performance bug."));
        IR const* ir = this;
        while (IR_parent(ir) != NULL) {
            ir = IR_parent(ir);
            if (ir->is_stmt()) { break; }
        }
        ASSERTN(ir->is_stmt(), ("ir is orphan"));
        return (IR*)ir;
    }

    //Return label info if exist.
    inline LabelInfo const* getLabel() const;

    //Return the byte size of array element.
    inline UINT getArrayElemDtSize(TypeMgr const* tm) const;

    //Return byte size of ir data type.
    UINT getTypeSize(TypeMgr const* tm) const
    { return tm->getByteSize(getType()); }

    DATA_TYPE getDType() const { return TY_dtype(getType()); }

    //Return data type descriptor.
    Type const* getType() const { return IR_dt(this); }

    AIContainer const* getAI() const { return IR_ai(this); }

    //Return rhs if exist. Some stmt has rhs,
    //such as IR_ST, IR_STPR and IR_IST.
    inline IR * getRHS() const;

    //Return the PR no if exist.
    inline UINT getPrno() const;

    //Return the SSAInfo if exist.
    inline SSAInfo * getSSAInfo() const;

    //Return stmt if it writes PR as result. Otherwise return NULL.
    inline IR * getResultPR();

    //Get the stmt accroding to given prno if the stmt writes PR as a result.
    //Otherwise return NULL.
    IR * getResultPR(UINT prno);

    //Find the first PR related to 'prno'. Otherwise return NULL.
    //This function iterate IR tree nonrecursively.
    IR * getOpndPRList(UINT prno);

    //Find the first PR related to 'prno'.
    //This function iterate IR tree nonrecursively.
    //'ii': iterator.
    IR * getOpndPR(UINT prno, IRIter & ii); //Nonrecursively.

    //This function recursively iterate the IR tree to
    //retrieve the PR whose PR_no is equal to given 'prno'.
    //Otherwise return NULL.
    IR * getOpndPR(UINT prno);

    //Get the MD DefUse Set.
    DUSet * getDUSet() const
    {
        DU * const du = getDU();
        return du == NULL ? NULL : DU_duset(du);
    }

    //Return the Memory Descriptor Set for given ir may describe.
    MDSet const* getMayRef() { return getRefMDSet(); }

    //Return the MemoryAddr for 'ir' must be.
    MD const* getMustRef() { return getRefMD(); }

    //Get the MD that IR referrenced.
    MD const* getRefMD() const
    {
        DU * du = getDU();
        return du == NULL ? NULL : DU_md(du);
    }

    //Get the MDSet that IR referrenced.
    MDSet const* getRefMDSet() const
    {
        DU * du = getDU();
        return du == NULL ? NULL : DU_mds(du);
    }

    //Return exact MD if ir defined.
    MD const* getExactRef() const
    {
        MD const* md = getRefMD();
        return (md == NULL || !md->is_exact()) ? NULL : md;
    }

    MD const* getEffectRef() const
    {
        MD const* md = getRefMD();
        ASSERT0(md == NULL || !MD_is_may(md));
        return md;
    }

    //Return determinate expression if any.
    inline IR * getJudgeDet() const;

    //Return true if ir carried sideeffect property.
    bool hasSideEffect() const { return IR_has_sideeffect(this); }

    //Return true if ir compute produce a result.
    bool hasResult() const { return IRDES_has_result(g_ir_desc[getCode()]); }

    //Return true if ir has constant offset.
    bool hasOffset() const { return IRDES_has_offset(g_ir_desc[getCode()]); }

    //Return true if ir has idinfo.
    bool hasIdinfo() const { return IRDES_has_idinfo(g_ir_desc[getCode()]); }

    //Return true if ir has DU Info.
    bool hasDU() const { return IRDES_has_du(g_ir_desc[getCode()]); }

    //Return true if stmt has judge determinate expression.
    inline bool hasJudgeDet() const;

    //Return true if ir is call and does have a return value.
    inline bool hasReturnValue() const;

    UINT id() const { return IR_id(this); }
    void invertLand(Region * rg);
    void invertLor(Region * rg);

    //Return true if current ir does not overlap to ir2.
    //ir2: stmt or expression to be compared.
    //Note this function does not need RefMD information.
    //It just determine overlapping of given two IR according to
    //their data-type and offset.
    bool isNotOverlap(IR const* ir2, Region * rg) const;
    bool isNotOverlapViaMDRef(IR const* ir2) const;
    bool isNoMove() const { return IR_no_move(this); }
    //Return true if current IR may contain memory reference.
    bool isContainMemRef() const
    {
        switch (getCode()) {
        case IR_GOTO:
        case IR_LABEL:
        case IR_CASE:
        case IR_BREAK:
        case IR_CONTINUE:
            return false;
        default:;
        }
        return true;
    }

    //Return true if ir's data type is vector.
    bool is_vec() const { return IR_dt(this)->is_vector(); }

    //Return true if ir's data type is pointer.
    bool is_ptr() const { return IR_dt(this)->is_pointer(); }

    //Return true if ir's data type is string.
    bool is_str() const { return IR_dt(this)->is_string(); }

    //Return true if ir's data type is memory chunk.
    bool is_mc() const { return IR_dt(this)->is_mc(); }

    bool is_any() const { return IR_dt(this)->is_any(); }

    //Return true if ir data type is signed, and the type
    //may be integer or float.
    bool is_signed() const { return IR_dt(this)->is_signed(); }

    //Return true if ir data type is unsigned, and the type
    //may be integer, string, vector.
    bool is_unsigned() const { return IR_dt(this)->is_unsigned(); }

    //Return true if ir data type is signed integer.
    bool is_sint() const { return IR_dt(this)->is_sint(); }

    //Return true if ir data type is unsigned integer.
    bool is_uint() const { return IR_dt(this)->is_uint(); }

    //Return true if ir data type is signed/unsigned integer.
    bool is_int() const { return IR_dt(this)->is_int(); }

    //Return true if ir data type is float.
    bool is_fp() const { return IR_dt(this)->is_fp(); }

    //Return true if ir data type is boolean.
    bool is_bool() const { return IR_dt(this)->is_bool(); }

    //Return true if ir is label.
    bool is_lab() const { return getCode() == IR_LABEL; }

    //Return true if current ir equal to src.
    bool isIREqual(IR const* src, bool is_cmp_kid = true) const;

    //Return true if current ir is both PR and equal to src.
    inline bool isPREqual(IR const* src) const;

    //Return true if ir-list are equivalent.
    bool isIRListEqual(IR const* irs, bool is_cmp_kid = true) const;

    //Return true if IR tree is exactly congruent, or
    //they are parity memory reference.
    bool isMemRefEqual(IR const* src) const;

    //Return true if ir does not have any sibling.
    bool is_single() const { return get_next() == NULL && get_prev() == NULL; }

    //Return true if current ir is memory store operation.
    bool is_store() const
    { return is_st() || is_stpr() || is_ist() || is_starray(); }

    //Return true if current ir is valid type to be phi operand.
    bool is_phi_opnd() const { return is_pr() || is_const(); }

    //Return true if current ir is stmt.
    //Only statement can be chained.
    bool is_stmt() const { return IRDES_is_stmt(g_ir_desc[getCode()]); }

    //Return true if current ir is expression.
    bool is_exp() const { return !is_stmt(); }

    //Return true if k is kid node of right-hand-side of current ir.
    bool is_rhs(IR const* k) const { return !is_lhs(k) && k != this; }

    //Return true if k is the lhs of current ir.
    inline bool is_lhs(IR const* k) const;

    //Return true if ir terminates the control flow.
    bool is_terminate() const { return IR_is_terminate(this); }

    //Return true if ir is volatile.
    inline bool is_volatile() const;

    //Return true if given array has same dimension structure with current ir.
    bool isSameArrayStruct(IR const* ir) const;

    //Record if ir might throw exception.
    bool isMayThrow() const { return IR_may_throw(this); }

    //Return true if current ir is binary operation.
    bool isBinaryOp() const { return IRDES_is_bin(g_ir_desc[getCode()]); }

    //Return true if current ir is unary operation.
    bool isUnaryOp() const { return IRDES_is_una(g_ir_desc[getCode()]); }

    //Return true if ir is constant expression.
    inline bool isConstExp() const;

    //Return true if ir is readonly expression or readonly call stmt.
    //If ir is expression, this function indicates that the expression does
    //not modify any memory.
    //If ir is call, this function indicates that function does not modify any
    //global memory or any memory object that passed through pointer
    //arguments.
    inline bool isReadOnly() const;

    //True if store to specified element of pseduo register.
    //The pseduo register must be D_MC or vector type.
    bool is_setelem() const { return getCode() == IR_SETELEM; }

    //True if picking up specified element of givne PR and store the value
    //to a new PR. The base PR must be D_MC or vector type.
    //And the result PR must be element type of base PR.
    bool is_getelem() const { return getCode() == IR_GETELEM; }
    bool is_undef() const { return getCode() == IR_UNDEF; }
    bool is_dowhile() const { return getCode() == IR_DO_WHILE; }
    bool is_whiledo() const { return getCode() == IR_WHILE_DO; }
    bool is_doloop() const { return getCode() == IR_DO_LOOP; }
    bool is_if() const { return getCode() == IR_IF; }
    bool is_label() const { return getCode() == IR_LABEL; }
    bool is_case() const { return getCode() == IR_CASE; }
    bool is_id() const { return getCode() == IR_ID; }
    bool is_break() const { return getCode() == IR_BREAK; }
    bool is_continue() const { return getCode() == IR_CONTINUE; }
    bool is_const() const { return getCode() == IR_CONST; }
    bool is_ld() const { return getCode() == IR_LD; }
    bool is_st() const { return getCode() == IR_ST; }
    bool is_call() const { return getCode() == IR_CALL; }
    bool is_icall() const { return getCode() == IR_ICALL; }
    bool is_starray() const { return getCode() == IR_STARRAY; }
    bool is_ild() const { return getCode() == IR_ILD; }
    bool is_array() const { return getCode() == IR_ARRAY; }
    bool is_ist() const { return getCode() == IR_IST; }
    bool is_lda() const { return getCode() == IR_LDA; }
    bool is_switch() const { return getCode() == IR_SWITCH; }
    bool is_return() const { return getCode() == IR_RETURN; }
    bool is_cvt() const { return getCode() == IR_CVT; }
    bool is_truebr() const { return getCode() == IR_TRUEBR; }
    bool is_falsebr() const { return getCode() == IR_FALSEBR; }
    bool is_select() const { return getCode() == IR_SELECT; }
    bool is_phi() const { return getCode() == IR_PHI; }
    bool is_region() const { return getCode() == IR_REGION; }
    bool is_goto() const { return getCode() == IR_GOTO; }
    bool is_igoto() const { return getCode() == IR_IGOTO; }
    bool is_add() const { return getCode() == IR_ADD; }
    bool is_sub() const { return getCode() == IR_SUB; }
    bool is_mul() const { return getCode() == IR_MUL; }
    bool is_div() const { return getCode() == IR_DIV; }
    bool is_rem() const { return getCode() == IR_REM; }
    bool is_mod() const { return getCode() == IR_MOD; }
    bool is_land() const { return getCode() == IR_LAND; }
    bool is_lor() const { return getCode() == IR_LOR; }
    bool is_band() const { return getCode() == IR_BAND; }
    bool is_bor() const { return getCode() == IR_BOR; }
    bool is_xor() const { return getCode() == IR_XOR; }
    bool is_asr() const { return getCode() == IR_ASR; }
    bool is_lsr() const { return getCode() == IR_LSR; }
    bool is_lsl() const { return getCode() == IR_LSL; }
    bool is_lt() const { return getCode() == IR_LT; }
    bool is_le() const { return getCode() == IR_LE; }
    bool is_gt() const { return getCode() == IR_GT; }
    bool is_ge() const { return getCode() == IR_GE; }
    bool is_eq() const { return getCode() == IR_EQ; }
    bool is_ne() const { return getCode() == IR_NE; }
    bool is_bnot() const { return getCode() == IR_BNOT; }
    bool is_lnot() const { return getCode() == IR_LNOT; }
    bool is_neg() const { return getCode() == IR_NEG; }

    //True if load from pseudo register.
    bool is_pr() const { return getCode() == IR_PR; }

    //True if store to pseudo register.
    bool is_stpr() const { return getCode() == IR_STPR; }

    //Return true if ir indicate conditional branch to a label.
    bool isConditionalBr() const { return is_truebr() || is_falsebr(); }

    //Return true if ir is operation that read or write to an array element.
    bool isArrayOp() const { return is_array() || is_starray(); }

    //Return true if ir is base expression of array operation.
    inline bool isArrayBase(IR const* ir) const;

    //Return true if ir may jump to multiple target.
    bool isMultiConditionalBr() const { return is_switch(); }

    //Return true if ir is unconditional branch.
    bool isUnconditionalBr() const { return is_goto() || is_igoto(); }

    //Return true if ir is indirect jump to multiple target.
    bool isIndirectBr() const { return is_igoto(); }

    //Return true if ir is branch stmt.
    bool isBranch() const
    { return isConditionalBr() || isMultiConditionalBr() ||
             isUnconditionalBr() || isIndirectBr(); }

    //Return true if ir is indirect memory operation.
    bool isIndirectMemOp() const { return is_ist() || is_ild(); }

    bool isCallStmt() const { return is_call() || is_icall(); }

    //Return true if ir is a call and has a return value.
    bool isCallHasRetVal() const
    { return isCallStmt() && hasReturnValue(); }

    //Return true if stmt modify PR.
    //CALL/ICALL may modify PR if it has a return value.
    bool isWritePR() const { return IRDES_is_write_pr(g_ir_desc[getCode()]); }

    //Return true if current stmt exactly modifies a PR.
    //CALL/ICALL may modify PR if it has a return value.
    //IR_SETELEM and IR_GETELEM may modify part of PR rather than whole.
    bool isWriteWholePR() const
    { return IRDES_is_write_whole_pr(g_ir_desc[getCode()]); }

    //Return true if current stmt read value from PR.
    bool isReadPR() const  { return is_pr(); }

    //Return true if current operation references memory.
    //These kinds of operation always define or use MD.
    bool isMemoryRef() const { return IRDES_is_mem_ref(g_ir_desc[getCode()]); }

    //Return true if current operation references memory, and
    //it is the rhs of stmt.
    //These kinds of operation always use MD.
    bool isMemoryOpnd() const
    { return IRDES_is_mem_opnd(g_ir_desc[getCode()]); }

    //Return true if current ir is integer constant, and the number
    //is equal to 'value'.
    inline bool isConstIntValueEqualTo(HOST_INT value) const;

    //Return true if current operation references memory except
    //the PR memory.
    bool isMemoryRefNotOperatePR() const
    { return IRDES_is_non_pr_memref(g_ir_desc[getCode()]); }

    //Return true if current ir and ir2 represent different memory location,
    //otherwise return false to tell caller we do not know more about these
    //object. Note this function will consider data type that current ir or ir2
    //referrenced.
    bool isDiffMemLoc(IR const* ir2) const;

    //True if ir is atomic operation.
    bool is_atomic() const { return IR_is_atomic(this); }

    //True if ir is read-modify-write.
    bool is_rmw() const { return IR_is_read_mod_write(this); }
    //True if ir is judgement operation.
    bool is_judge() const { return is_relation() || is_logical(); }
    //True if ir is logical operation.
    bool is_logical() const { return IRDES_is_logical(g_ir_desc[getCode()]); }
    //True if ir is relation operation.
    bool is_relation() const { return IRDES_is_relation(g_ir_desc[getCode()]); }

    //IR meet commutative, e.g: a+b = b+a
    bool is_commutative() const
    { return IRDES_is_commutative(g_ir_desc[getCode()]); }

    //IR meet associative, e.g: (a+b)+c = a+(b+c)
    bool is_associative() const
    { return IRDES_is_associative(g_ir_desc[getCode()]); }

    //Return true if current ir is leaf node at IR tree.
    //Leaf node must be expression node and it does not have any kids.
    bool is_leaf() const { return IRDES_is_leaf(g_ir_desc[getCode()]); }

    //Return true if kid is the kid node of current ir.
    inline bool is_kids(IR const* exp) const;

    //Return true if array base is IR_LDA. This exactly clerifies which array
    //we are accessing. In contrast to direct array reference,
    //one can access array via computational expression, which return a pointer,
    //that record the base address of array accessing. We call this
    //indirect array accessing.
    inline bool isDirectArrayRef() const;

    //This function invert the operation accroding to it semantics.
    inline void invertIRType(Region * rg)
    {
        switch (getCode()) {
        case IR_LT: IR_code(this) = IR_GE; break;
        case IR_LE: IR_code(this) = IR_GT; break;
        case IR_GT: IR_code(this) = IR_LE; break;
        case IR_GE: IR_code(this) = IR_LT; break;
        case IR_EQ: IR_code(this) = IR_NE; break;
        case IR_NE: IR_code(this) = IR_EQ; break;
        case IR_TRUEBR: IR_code(this) = IR_FALSEBR; break;
        case IR_FALSEBR: IR_code(this) = IR_TRUEBR; break;
        case IR_LOR:
            invertLor(rg);
            break;
        case IR_LAND:
            invertLand(rg);
            break;
        default: ASSERTN(0, ("unsupport"));
        }
    }

    //Return true if current ir can be placed in BB.
    bool isStmtInBB() const
    { return IRDES_is_stmt_in_bb(g_ir_desc[getCode()]); }

    //Return true if current stmt must modify 'md'.
    inline bool isExactDef(MD const* md) const;
    inline bool isExactDef(MD const* md, MDSet const* mds) const;

    inline void setPrno(UINT prno);
    inline void setOffset(UINT ofst);
    inline void setIdinfo(Var * idinfo);
    inline void setLabel(LabelInfo const* li);
    inline void setBB(IRBB * bb);
    inline void setRHS(IR * rhs);
    inline void setSSAInfo(SSAInfo * ssa);
    inline void setDU(DU * du);

    //Set 'kid' to be 'idx'th child of current ir.
    inline void setKid(UINT idx, IR * kid);

    //Set the relationship between parent and its kid.
    void setParentPointer(bool recur = true);

    //Set current ir to be parent of 'kid'.
    void setParent(IR * kid)
    {
        ASSERT0(kid && is_kids(kid));
        for (IR * k = kid; k != NULL; k = IR_next(k)) {
            IR_parent(k) = this;
        }
    }

    //The current ir is set to pointer type.
    //Note pointer_base_size may be 0.
    inline void setPointerType(UINT pointer_base_size, TypeMgr * tm)
    {
        PointerType d;
        TY_dtype(&d) = D_PTR;
        TY_ptr_base_size(&d) = pointer_base_size;
        IR_dt(this) = TC_type(tm->registerPointer(&d));
    }
    void setRefMD(MD const* md, Region * rg);
    void setRefMDSet(MDSet const* mds, Region * rg);

    //Find and substitute 'newk' for 'oldk'.
    //Return true if replaced the 'oldk'.
    //'recur': set to true if function recusively perform
    //replacement for 'oldk'.
    bool replaceKid(IR * oldk, IR * newk, bool recur)
    {
        for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
            IR * kid = getKid(i);
            if (kid == NULL) { continue; }
            for (IR * x = kid; x != NULL; x = x->get_next()) {
                if (x == oldk) {
                    xcom::replace(&kid, oldk, newk);
                    if (IR_prev(newk) == NULL) {
                        //oldk is the header, and update the kid i.
                        setKid(i, kid);
                    } else {
                        IR_parent(newk) = IR_parent(oldk);
                    }
                    return true;
                }
                if (recur && x->replaceKid(oldk, newk, true)) {
                    return true;
                }
            }
        }
        return false;
    }

    //Get the MD DefUse Set. This function is readonly.
    DUSet const* readDUSet() const { return getDUSet(); }

    //Iterate IR tree to remove SSA du.
    //    e.g: pr1 = ...
    //             = pr1 //S1
    //If S1 will be deleted, pr1 should be removed from its SSA_uses.
    void removeSSAUse();

    //This function only handle Call/ICall stmt, it find PR and remove
    //them out of UseSet.
    //Note this function does not maintain DU chain between call and its use.
    void removePRFromUseset(DefMiscBitSetMgr & sbs_mgr, Region * rg);

    bool verify(Region const* rg) const;
    bool verifyKids() const;
};


//Record float point.
#define CONST_fp_val(ir) (((CConst*)CK_IRT(ir, IR_CONST))->u1.s1.fp_const_value)

//Record the number of mantissa of float-point number.
#define CONST_fp_mant(ir) (((CConst*)CK_IRT(ir, IR_CONST))->u1.s1.fp_mantissa)

//Record integer.
#define CONST_int_val(ir) (((CConst*)CK_IRT(ir, IR_CONST))->u1.int_const_value)

//Record string.
#define CONST_str_val(ir) (((CConst*)CK_IRT(ir, IR_CONST))->u1.str_value)

//Record anonymous value.
#define CONST_anony_val(ir) \
    (((CConst*)CK_IRT(ir, IR_CONST))->u1.anonymous_value)
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


//ID need DU info, some Passes need it, e.g. GVN.
//Note IR_ID should NOT participate in GVN analysis because it does not
//represent a real operation.
#define ID_info(ir) (((CId*)CK_IRT(ir, IR_ID))->id_info)
#define ID_du(ir) (((CId*)CK_IRT(ir, IR_ID))->du)
#define ID_phi(ir) (((CId*)CK_IRT(ir, IR_ID))->phi)
class CId : public DuProp, public VarProp {
    COPY_CONSTRUCTOR(CId);
public:
    MDPhi * phi; //record the MD PHI dummy stmt if ID is operand of MD PHI.
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
    UINT field_offset;
};


//This class represent memory load operation.
//LD_ofst descibe the byte offset that is the addend to variable base address.
//
//usage: ld(i32, ofst:10, s) with LD_ofst = 10 means:
//    Assum a pointer p, it point to the address of variable s.
//    The ld operation loads i32 value from the address (p + 10)
#define LD_ofst(ir) (((CLd*)CK_IRT(ir, IR_LD))->field_offset)
#define LD_idinfo(ir) (((CLd*)CK_IRT(ir, IR_LD))->id_info)
#define LD_du(ir) (((CLd*)CK_IRT(ir, IR_LD))->du)
class CLd : public DuProp, public VarProp, public OffsetProp {
    COPY_CONSTRUCTOR(CLd);
};


//This class represent indirect memory load operation.
//ILD_ofst descibe the byte offset that is the addend to address.
//If ILD_ofst is not 0, the base memory address must add the offset.
//
//usage: ild p, where p is ILD_base, it must be pointer.
//    1. res = ild (p), if ILD_ofst is 0.
//    2. res = ild (p + ILD_ofst) if ILD_ofst is not 0.
#define ILD_ofst(ir) (((CILd*)CK_IRT(ir, IR_ILD))->field_offset)
#define ILD_du(ir) (((CILd*)CK_IRT(ir, IR_ILD))->du)
#define ILD_base(ir) ILD_kid(ir, 0)
#define ILD_kid(ir, idx) (((CILd*)ir)->opnd[CKID_TY(ir, IR_ILD, idx)])
class CILd : public DuProp, public OffsetProp {
    COPY_CONSTRUCTOR(CILd);
public:
    IR * opnd[1];
};


//This class represent properties of stmt.
class StmtProp {
    COPY_CONSTRUCTOR(StmtProp);
public:
    IRBB * bb;
};


//This class represent memory store operation.
//ST_ofst descibe the byte offset that is the addend to address.
//ST_idinfo describe the memory variable.
//If ST_ofst is not 0, the base memory address must add the offset.
//
//usage: st(lhs, rhs), p = &lhs, where p is the memory address of lhs.
//    1. [p] = rhs, if ST_ofst is 0.
//    2. [p + ST_ofst] = rhs if ST_ofst is not 0.
#define ST_bb(ir) (((CSt*)CK_IRT(ir, IR_ST))->bb)
#define ST_idinfo(ir) (((CSt*)CK_IRT(ir, IR_ST))->id_info)
#define ST_ofst(ir) (((CSt*)CK_IRT(ir, IR_ST))->field_offset)
#define ST_du(ir) (((CSt*)CK_IRT(ir, IR_ST))->du)
#define ST_rhs(ir) ST_kid(ir, 0)
#define ST_kid(ir, idx) (((CSt*)ir)->opnd[CKID_TY(ir, IR_ST, idx)])
class CSt: public CLd, public StmtProp {
    COPY_CONSTRUCTOR(CSt);
public:
    IR * opnd[1];
};


//This class represent temporary memory store operation.
//The temporary memory named pseudo register.
//usage: stpr(prno:1, val), will store val to PR1.
#define STPR_bb(ir) (((CStpr*)CK_IRT(ir, IR_STPR))->bb)
#define STPR_no(ir) (((CStpr*)CK_IRT(ir, IR_STPR))->prno)
#define STPR_ssainfo(ir) (((CStpr*)CK_IRT(ir, IR_STPR))->ssainfo)
#define STPR_du(ir) (((CStpr*)CK_IRT(ir, IR_STPR))->du)
#define STPR_rhs(ir) STPR_kid(ir, 0)
#define STPR_kid(ir, idx) (((CStpr*)ir)->opnd[CKID_TY(ir, IR_STPR, idx)])
class CStpr: public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CStpr);
public:
    UINT prno; //PR number.
    SSAInfo * ssainfo; //Present ssa def and use set.
    IR * opnd[1];
};


//This class represent an operation that store value to be part of the section
//of 'base'.
//NOTE: Type of result PR should same with base.
//SETELEM_ofst descibe the byte offset to the start address of result PR.
//The the number of bytes of result PR must be an integer multiple of
//the number of bytes of SETELEM_val if the result data type is vector.
//
//usage: setelem $2(vec<4*i32>) = $3(vec<4*i32>), $1(i32), 4.
//    The result PR is $2.
//    The code stores $1 that is part of $3 to be second element
//    of $2, namely, the section of $2 that offset is 4 bytes.
//
//This operation will store value to the memory which offset to the
//memory chunk or vector's base address.
#define SETELEM_bb(ir) (((CSetElem*)CK_IRT(ir, IR_SETELEM))->bb)
#define SETELEM_prno(ir) (((CSetElem*)CK_IRT(ir, IR_SETELEM))->prno)
#define SETELEM_ssainfo(ir) (((CSetElem*)CK_IRT(ir, IR_SETELEM))->ssainfo)
#define SETELEM_du(ir) (((CSetElem*)CK_IRT(ir, IR_SETELEM))->du)
#define SETELEM_base(ir) SETELEM_kid(ir, 0)
#define SETELEM_val(ir) SETELEM_kid(ir, 1)
#define SETELEM_ofst(ir) SETELEM_kid(ir, 2)
#define SETELEM_kid(ir, idx) \
    (((CSetElem*)ir)->opnd[CKID_TY(ir, IR_SETELEM, idx)])
class CSetElem: public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CSetElem);
public:
    UINT prno; //PR number.
    SSAInfo * ssainfo; //Present ssa def and use set.
    IR * opnd[3];
};


//This class represent an operation that get an element from a base memory
//location and store the element to a PR.
//
//The the number of byte of GETELEM_base must be
//an integer multiple of the number of byte of result
//PR if base is vector.
//
//usage: getelem $1(i32) $2(vec<4*i32>), 4.
//    The base memory location is a PR, which is a vector.
//    The example get the second element of pr2, then store it to pr1.
#define GETELEM_bb(ir) (((CGetElem*)CK_IRT(ir, IR_GETELEM))->bb)
#define GETELEM_prno(ir) (((CGetElem*)CK_IRT(ir, IR_GETELEM))->prno)
#define GETELEM_ssainfo(ir) (((CGetElem*)CK_IRT(ir, IR_GETELEM))->ssainfo)
#define GETELEM_du(ir) (((CGetElem*)CK_IRT(ir, IR_GETELEM))->du)
#define GETELEM_base(ir) GETELEM_kid(ir, 0)
#define GETELEM_ofst(ir) GETELEM_kid(ir, 1)
#define GETELEM_kid(ir, idx)(((CGetElem*)ir)->opnd[CKID_TY(ir, IR_GETELEM, idx)])
class CGetElem : public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CGetElem);
public:
    UINT prno; //PR number.

    //versioned presentation or ssa def and use list in ssa mode.
    //Note this field only avaiable if SSA information is maintained.
    SSAInfo * ssainfo;
    IR * opnd[2];
};


//This class represent indirect memory store operation.
//IST_ofst descibe the byte offset that is the addend to address.
//
//If IST_ofst is not 0, the base memory address must add the offset.
//
//usage: ist = ld p, rhs, where the value of p is the base memory address
//to be stored. The followed code exhibits the behaivor of such usage.
//    1. [p] = rhs, if IST_ofst is 0.
//    2. [p + IST_ofst] = rhs, if IST_ofst is not 0.
#define IST_bb(ir) (((CISt*)CK_IRT(ir, IR_IST))->bb)
#define IST_ofst(ir) (((CISt*)CK_IRT(ir, IR_IST))->field_offset)
#define IST_du(ir) (((CISt*)CK_IRT(ir, IR_IST))->du)
#define IST_base(ir) IST_kid(ir, 0)
#define IST_rhs(ir) IST_kid(ir, 1)
#define IST_kid(ir, idx) (((CISt*)ir)->opnd[CKID_TY(ir, IR_IST, idx)])
class CISt : public DuProp, public OffsetProp, public StmtProp {
    COPY_CONSTRUCTOR(CISt);
public:
    IR * opnd[2];
};


//This class represent the operation to load memory variable address.
//The base of LDA may be ID variable, LABEL variable, STRING variable.
//NOTE: LDA_ofst describe the byte offset that is the addend to the address.
//usage: lda(s) with LDA_ofst = 10 means:
//    pointer p = lda(s)
//    p = p + 10
//    return p
#define LDA_ofst(ir) (((CLda*)CK_IRT(ir, IR_LDA))->field_offset)
#define LDA_idinfo(ir) (((CLda*)CK_IRT(ir, IR_LDA))->id_info)
class CLda : public IR, public VarProp, public OffsetProp {
    COPY_CONSTRUCTOR(CLda);
};


//This class uses bits to describe attributes.
//Represents a direct function call.
//NOTE: 'opnd' must be the last member.
//Record the BB that CALL stmt placed.
#define CALL_bb(ir) (((CCall*)CK_IRT_CALL(ir))->bb)
#define CALL_idinfo(ir) (((CCall*)CK_IRT_ONLY_CALL(ir))->id_info)

//Returned result PR number if any.
#define CALL_prno(ir) (((CCall*)CK_IRT_CALL(ir))->prno)

//SSA info of result PR.
#define CALL_ssainfo(ir) (((CCall*)CK_IRT_CALL(ir))->prssainfo)

//True if this call is intrinsic operation.
#define CALL_is_intrinsic(ir) (((CCall*)CK_IRT_CALL(ir))->is_intrinsic)

//Record intrinsic operator if CALL_is_intrinsic is true.
#define CALL_intrinsic_op(ir) (((CCall*)CK_IRT_CALL(ir))->intrinsic_op)

//Call does not necessarily to be BB boundary.
#define CALL_is_not_bb_bound(ir) (((CCall*)CK_IRT_CALL(ir))->is_not_bb_bound)

//True if this call does not modify any memory.
#define CALL_is_readonly(ir) \
    (VAR_is_readonly(CALL_idinfo((CCall*)CK_IRT_CALL(ir))))

//True if call allocated memory from heap. It always describe functions
//like malloc() or 'new' operator.
#define CALL_is_alloc_heap(ir) (((CCall*)CK_IRT_CALL(ir))->is_alloc_heap)

//Record MD DU information.
#define CALL_du(ir) (((CCall*)CK_IRT_CALL(ir))->du)

//Parameter list of call.
#define CALL_param_list(ir) CALL_kid(ir, 0)
//Record dummy referenced IR.
#define CALL_dummyuse(ir) CALL_kid(ir, 1)
#define CALL_kid(ir, idx) (((CCall*)ir)->opnd[CKID_CALL(ir, idx)])
class CCall : public DuProp, public VarProp, public StmtProp {
    COPY_CONSTRUCTOR(CCall);
public:
    //True if current call is intrinsic call.
    BYTE is_intrinsic:1;

    //True if this call do allocate memory from heap. It always the function
    //like malloc or new.
    BYTE is_alloc_heap:1;

    //True if this call does not necessarily to be basic block boundary.
    //By default, call stmt must be down boundary of basic block, but if
    //the flag is true, the call is always be defined by customer for
    //special purpose, e.g, intrinsic call or customized operation.
    BYTE is_not_bb_bound:1;

    //Record the intrinsic operation.
    UINT intrinsic_op;

    UINT prno; //Result PR number if any.

    SSAInfo * prssainfo; //indicates PR ssa def and use set.

    //NOTE: 'opnd' must be the last member.
    IR * opnd[2];

public:
    CHAR const* getCalleeNameString() const
    { return SYM_name(CALL_idinfo(this)->get_name()); }

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

    //Get the intrinsic operation code.
    UINT getIntrinsicOp()
    {
        ASSERT0(CALL_is_intrinsic(this));
        return CALL_intrinsic_op(this);
    }
};


//Represents a indirect function call.
//This class uses macro operations of CCall.
//Expression to compute the target function address.
//NOTE: 'opnd_pad' must be the first member.

//Indicate the callee function pointer.
#define ICALL_callee(ir) (*(((CICall*)ir)->opnd + CKID_TY(ir, IR_ICALL, 2)))

//True if current call is readonly.
#define ICALL_is_readonly(ir) (((CICall*)CK_IRT_ONLY_ICALL(ir))->is_readonly)
#define ICALL_kid(ir, idx) (((CICall*)ir)->opnd[CKID_TY(ir, IR_ICALL, idx)])
class CICall : public CCall {
    COPY_CONSTRUCTOR(CICall);
public:
    //NOTE: 'opnd_pad' must be the first member.
    IR * opnd_pad[1];

    //True if current call is readonly.
    BYTE is_readonly:1;
};


//Binary Operations, include add, sub, mul, div, rem, mod,
//land, lor, band, bor, xor, lt, le, gt, ge, eq, ne, asr, lsr, lsl.
#define BIN_opnd0(ir) BIN_kid(ir, 0)
#define BIN_opnd1(ir) BIN_kid(ir, 1)
#define BIN_kid(ir, idx) (((CBin*)ir)->opnd[CKID_BIN(ir, idx)])
class CBin : public IR {
    COPY_CONSTRUCTOR(CBin);
public:
    IR * opnd[2];
};


//Unary Operations, include neg, bnot, lnot.
#define UNA_opnd(ir) UNA_kid(ir, 0)
#define UNA_kid(ir, idx) (((CUna*)ir)->opnd[CKID_UNA(ir, idx)])
class CUna : public IR {
    COPY_CONSTRUCTOR(CUna);
public:
    IR * opnd[1];
};


//This class represent goto operation, unconditional jump to target label.
#define GOTO_bb(ir) (((CGoto*)CK_IRT(ir, IR_GOTO))->bb)
#define GOTO_lab(ir) (((CGoto*)CK_IRT(ir, IR_GOTO))->jump_target_lab)
class CGoto : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CGoto);
public:
    LabelInfo const* jump_target_lab;
};


//This class represent indirect goto operation,
//the control flow will unconditional jump to one target label of a list of
//label which determined by value-exp.
//usage: igoto (value-exp) case_list.
#define IGOTO_bb(ir) (((CIGoto*)CK_IRT(ir, IR_IGOTO))->bb)

//Value expression.
#define IGOTO_vexp(ir) IGOTO_kid(ir, 0)

//Record a list pairs of <case-value, jump label>.
#define IGOTO_case_list(ir) IGOTO_kid(ir, 1)

#define IGOTO_kid(ir, idx) (((CIGoto*)ir)->opnd[CKID_TY(ir, IR_IGOTO, idx)])
class CIGoto : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CIGoto);
public:
    IR * opnd[2];
};


//High level control loop operation.
//usage:
//    while (det) {
//      body
//    }
//NOTE:
//    * The member layout should be same as do_while.
//    * 'opnd' must be the last member of CWhileDo.
//Determinate expression. It can NOT be NULL.
#define LOOP_det(ir) LOOP_kid(ir, 0)

//Record stmt list in loop body of IF. It can be NULL.
#define LOOP_body(ir) LOOP_kid(ir, 1)
#define LOOP_kid(ir, idx) (((CWhileDo*)ir)->opnd[CKID_LOOP(ir, idx)])
class CWhileDo : public IR {
    COPY_CONSTRUCTOR(CWhileDo);
public:
    //NOTE: 'opnd' must be the last member of CWhileDo.
    IR * opnd[2];

    //num: the number of IR added.
    void addToBody(UINT num, ...);
};


//High level control loop operation.
//usage:
//    do {
//      body
//    } while (det)
class CDoWhile : public CWhileDo {
    COPY_CONSTRUCTOR(CDoWhile);
};


//High level control loop operation.
//This structure represent a kind of loop with
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
#define LOOP_iv(ir) (*(((CDoLoop*)ir)->opnd + CKID_TY(ir, IR_DO_LOOP, 2)))

//Record the expression that initialize induction variable.
#define LOOP_init(ir) (*(((CDoLoop*)ir)->opnd + CKID_TY(ir, IR_DO_LOOP, 3)))

//Record the expression that update induction variable.
#define LOOP_step(ir) (*(((CDoLoop*)ir)->opnd + CKID_TY(ir, IR_DO_LOOP, 4)))
#define DOLOOP_kid(ir, idx) (((CDoLoop*)ir)->opnd[CKID_TY(ir, IR_DO_LOOP, idx)])
class CDoLoop : public CWhileDo {
    COPY_CONSTRUCTOR(CDoLoop);
public:
    //NOTE: 'opnd_pad' must be the first member of CDoLoop.
    IR * opnd_pad[3];
};


//This class represent high level control IF operation.
//usage:
//    if (det)
//      truebody
//      falsebody
//    endif
//Determinate expression. It can NOT be NULL.
#define IF_det(ir) IF_kid(ir, 0)

//Record stmt list in true body of IF. It can be NULL.
#define IF_truebody(ir) IF_kid(ir, 1)

//Record stmt list in false body of IF. It can be NULL.
#define IF_falsebody(ir) IF_kid(ir, 2)
#define IF_kid(ir, idx) (((CIf*)ir)->opnd[CKID_TY(ir, IR_IF, idx)])
class CIf : public IR {
    COPY_CONSTRUCTOR(CIf);
public:
    IR * opnd[3];
    //num: the number of IR added.
    void addToTrueBody(UINT num, ...);
    //num: the number of IR added.
    void addToFalseBody(UINT num, ...);
};


//This class represent internal and customer defined label.
#define LAB_lab(ir) (((CLab*)CK_IRT(ir, IR_LABEL))->label_info)
class CLab : public IR {
    COPY_CONSTRUCTOR(CLab);
public:
    LabelInfo const* label_info;
};


//This class represent high and middle level control flow switch operation.
//usage:
//    switch (value-exp)
//    case_list
//    body
//    endswitch
#define SWITCH_bb(ir) (((CSwitch*)CK_IRT(ir, IR_SWITCH))->bb)

//Default label.
//This is a label repesent the default jump target of IR_SWITCH.
//The label is optional.
//If there are not any cases matched, the control flow will jump to
//the default label.
#define SWITCH_deflab(ir) (((CSwitch*)CK_IRT(ir, IR_SWITCH))->default_label)

//Value expression.
#define SWITCH_vexp(ir) SWITCH_kid(ir, 0)

//Switch body.
#define SWITCH_body(ir) SWITCH_kid(ir, 1)

//Record a list pair of <case-value, jump label>.
#define SWITCH_case_list(ir) SWITCH_kid(ir, 2)

#define SWITCH_kid(ir, idx)  (((CSwitch*)ir)->opnd[CKID_TY(ir, IR_SWITCH, idx)])
class CSwitch : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CSwitch);
public:
    IR * opnd[3];
    LabelInfo const* default_label;

    //num: the number of IR added.
    void addToBody(UINT num, ...);
};


//This class represent the case value expression and its jump target label.
//NOTE: this class is used only by SWITCH and IGOTO.
#define CASE_lab(ir) (((CCase*)CK_IRT(ir, IR_CASE))->jump_target_label)

//Value expression.
#define CASE_vexp(ir) CASE_kid(ir, 0)
#define CASE_kid(ir, idx) (((CCase*)ir)->opnd[CKID_TY(ir, IR_CASE, idx)])
class CCase : public IR {
    COPY_CONSTRUCTOR(CCase);
public:
    IR * opnd[1]; //case-value
    LabelInfo const* jump_target_label; //jump lable for case.
};


//This class represent array operation.
//Base of array can be LDA, or other computational expression.
//This operation do not perform any array bound diagnositc.
//
//If array base is LDA, it denotes that the array's base is variable with
//array type,
//    e.g: char p[N]; (&p)[i] = ...
//
//If array base is computational expression, it denotes that the array's
//base is pointer, and the pointer point to an array.
//    e.g: char * p; (p+1)[i] = ...
//
//'elem_ty' represent the type of array element.
//Moreover, element may be array as well.
//
//'elem_num' represent the number of array element in current dimension.
//
#define ARR_ofst(ir) (((CArray*)CK_IRT_ARR(ir))->field_offset)
#define ARR_du(ir) (((CArray*)CK_IRT_ARR(ir))->du)
#define ARR_elemtype(ir) (((CArray*)CK_IRT_ARR(ir))->elemtype)

//Get the number of element in each dimension.
//e.g: Given array D_I32 A[10][20], the 0th dimension has 20 elements,
//each element has type D_I32, the 1th dimension has 10 elements,
//each element has type [D_I32*20], and the ARR_elem_num_buf will be [20, 10],
//that is the lowest dimension at the position 0 of the buffer.
//If we have an array accessing, such as A[i][j], the sublist will be
//ld(j)->ld(i), and elem_num list will be 20->10.
//the fininal access address will be (j + 20 * i) * sizeof(D_I32) + lda(A).
//
//Note that if the ARR_elem_num of a dimension is 0, means we can not determine
//the number of element at the dimension.
#define ARR_elem_num(ir, dim) \
    (((CArray*)CK_IRT_ARR(ir))->elem_num[CK_ARRAY_DIM(ir, dim)])
#define ARR_elem_num_buf(ir) (((CArray*)CK_IRT_ARR(ir))->elem_num)

//Array base expression.
#define ARR_base(ir) ARR_kid(ir, 0)

//Array subscript expression.
#define ARR_sub_list(ir) ARR_kid(ir, 1)
#define ARR_kid(ir, idx) (((CArray*)ir)->opnd[CKID_ARR(ir, idx)])
class CArray : public DuProp, public OffsetProp {
    COPY_CONSTRUCTOR(CArray);
public:
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

    //NOTE: 'opnd' must be the last member of CArray.
    IR * opnd[2];
public:
    //Return the number of dimensions.
    UINT getDimNum() const
    {
        ASSERT0(isArrayOp());
        UINT dim = 0;
        for (IR const* s = ARR_sub_list(this); s != NULL; s = s->get_next()) {
            dim++;
        }
        return dim;
    }

    //Return the number of element in given dimension.
    TMWORD getElementNumOfDim(UINT dimension) const
    {
        ASSERT0(ARR_elem_num_buf(this));
        return ARR_elem_num(this, dimension);
    }

    //Return true if exp is array base.
    bool is_base(IR const* exp) const { return exp == ARR_base(this); }

    //Return true if exp is array subscript expression list.
    bool isInSubList(IR const* exp) const
    {
        for (IR const* s = ARR_sub_list(this); s != NULL; s = s->get_next()) {
            if (s == exp || s->is_kids(exp)) { return true; }
        }
        return false;
    }
};


//This class represent the operation storing value to array.
//The most operations and properties are same as CArray.
//
//Base of array can be LDA, or other computational expression.
//This operation do not perform any array bound diagnositc.
//
//If array base is IR_LDA, it denotes that the array's base is variable with
//array type,
//    e.g: char p[N]; (&p)[i] = ...
//
//If array base is computational expression, it denotes that the array's
//base is pointer, and the pointer point to an array.
//    e.g: char * p; (p+1)[i] = ...
//
//'elem_ty' represent the type of array element.
//Moreover, element may be also an array as well.
//
//'elem_num' represent the number of array element in given dimension.
//
#define STARR_bb(ir) (((CStArray*)CK_IRT(ir, IR_STARRAY))->stmtprop.bb)
#define STARR_rhs(ir) \
    (*(((CStArray*)ir)->opnd_pad + CKID_TY(ir, IR_STARRAY, 0)))
class CStArray: public CArray {
    COPY_CONSTRUCTOR(CStArray);
public:
    //NOTE: 'opnd_pad' must be the first member of CStArray.
    IR * opnd_pad[1];

    //DO NOT PLACE MEMBER BEFORE opnd_pad
    StmtProp stmtprop;
};


//This class represent data-type convertion.
//Record the expression to be converted.
#define CVT_exp(ir) (UNA_opnd(ir))
#define CVT_kid(ir, idx) (UNA_kid(ir, idx))
#define CVT_round(ir) (((CCvt*)ir)->round)
class CCvt : public CUna {
    COPY_CONSTRUCTOR(CCvt);
public:
    ROUND_TYPE round;
public:
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
};


//This class represent temporary memory location which named pseudo register.
//It can be used to indicate the Region live-in register. In this case,
//a PR may not have a definition.
//NOTE:
//    1.PR can not be taken address.
//    2.PR is always allocate on stack.
#define PR_no(ir) (((CPr*)CK_IRT(ir, IR_PR))->prno)
#define PR_ssainfo(ir) (((CPr*)CK_IRT(ir, IR_PR))->ssainfo)
#define PR_du(ir) (((CPr*)CK_IRT(ir, IR_PR))->du)
class CPr : public DuProp {
    COPY_CONSTRUCTOR(CPr);
public:
    UINT prno; //PR number.

    //versioned presentation or ssa def and use list in ssa mode.
    //Note this field only avaiable if SSA information is maintained.
    SSAInfo * ssainfo;
};


//This class represent true branch operation.
//Branch if determinant express is true, otherwise control flow does not change.

//NOTE: the lay out of truebr should same as falsebr.
#define BR_bb(ir) (((CTruebr*)CK_IRT_BR(ir))->bb)
#define BR_lab(ir) (((CTruebr*)CK_IRT_BR(ir))->jump_target_lab)

//Determinate expression. It can NOT be NULL.
#define BR_det(ir) BR_kid(ir, 0)
#define BR_kid(ir, idx) (((CTruebr*)ir)->opnd[CKID_BR(ir, idx)])
class CTruebr : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CTruebr);
public:
    IR * opnd[1];
    LabelInfo const* jump_target_lab; //jump target label.
};


//This class represent false branch operation.
//Branch if determinant express is false, otherwise control flow does not change.
//Also use BR_det, BR_lab access.
//NOTE: the lay out of truebr should same as falsebr.
class CFalsebr : public CTruebr {
    COPY_CONSTRUCTOR(CFalsebr);
};


//This class represent function return operation.
//Return value expressions.
//usage: return a;  a is return-value expression.
#define RET_bb(ir) (((CRet*)CK_IRT(ir, IR_RETURN))->bb)
#define RET_exp(ir) RET_kid(ir, 0)
#define RET_kid(ir, idx) (((CRet*)ir)->opnd[CKID_TY(ir, IR_RETURN, idx)])
class CRet : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CRet);
public:
    IR * opnd[1];
};


//This class represent conditional select operation.
//usage: res = select(a > b), (10), (20)
//    means:
//    if (a > b) res = 10;
//    else res = 20;
//  where a > b is predicator expression.
//This operation compute the value accroding to the result of
//predicator expression, if the result value is true, return
//SELECT_trueexp, otherwise return SELECT_falseexp.

//Predicator expression.
#define SELECT_pred(ir) SELECT_kid(ir, 0)

//True part
#define SELECT_trueexp(ir) SELECT_kid(ir, 1)

//False part
#define SELECT_falseexp(ir) SELECT_kid(ir, 2)
#define SELECT_kid(ir, idx) (((CSelect*)ir)->opnd[CKID_TY(ir, IR_SELECT, idx)])
class CSelect : public IR {
    COPY_CONSTRUCTOR(CSelect);
public:
    IR * opnd[3];
};


//This class represent high level control structure, that
//terminate current loop execution immediately without any
//other operations.
//This operation is used by do-loop, do-while, while-do.
class CBreak : public IR {
    COPY_CONSTRUCTOR(CBreak);
};


//This class represent high level control structure, that
//re-execute current loop immediately without any
//other operations.
//This operation is used by do-loop, do-while, while-do.
class CContinue : public IR {
    COPY_CONSTRUCTOR(CContinue);
};


//This class represent phi operation.
#define PHI_bb(ir) (((CPhi*)CK_IRT(ir, IR_PHI))->bb)
#define PHI_prno(ir) (((CPhi*)CK_IRT(ir, IR_PHI))->prno)
#define PHI_ssainfo(ir) (((CPhi*)CK_IRT(ir, IR_PHI))->ssainfo)
#define PHI_opnd_list(ir) PHI_kid(ir, 0)
#define PHI_kid(ir, idx) (((CPhi*)ir)->opnd[CKID_TY(ir, IR_PHI, idx)])
class CPhi : public DuProp, public StmtProp {
    COPY_CONSTRUCTOR(CPhi);
public:
    UINT prno; //PR number.
    SSAInfo * ssainfo; //Present ssa def and use set.
    IR * opnd[1];
public:
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

    void insertOpndBefore(IR * marker, IR * exp)
    {
        ASSERT0(xcom::in_list(PHI_opnd_list(this), marker));
        ASSERT0(!xcom::in_list(PHI_opnd_list(this), exp));
        xcom::insertbefore(&PHI_opnd_list(this), marker, exp);
        IR_parent(exp) = this;
    }
};


//This class represent region operation.
//NOTE: If region is in BB, it must be single entry, single exit, since
//it might be reduced from reducible graph.
#define REGION_bb(ir) (((CRegion*)CK_IRT(ir, IR_REGION))->bb)
#define REGION_ru(ir) (((CRegion*)CK_IRT(ir, IR_REGION))->rg)
class CRegion : public IR, public StmtProp {
    COPY_CONSTRUCTOR(CRegion);
public:
    Region * rg;
public:
    //True if region is readonly.
    //This property is very useful if region is a blackbox.
    //And readonly region will alleviate the burden of optimizor.
    bool is_readonly() const;
};


//Exported Functions

//
//START IR
//
IR * IR::getRHS() const
{
    switch (getCode()) {
    case IR_ST: return ST_rhs(this);
    case IR_STPR: return STPR_rhs(this);
    case IR_STARRAY: return STARR_rhs(this);
    case IR_IST: return IST_rhs(this);
    default: ASSERTN(0, ("not store operation."));
    }
    return NULL;
}


//Return true if ir is base expression of array operation.
bool IR::isArrayBase(IR const* ir) const
{
    ASSERT0(isArrayOp());
    return ((CArray*)this)->is_base(ir);
}


UINT IR::getPrno() const
{
    switch (getCode()) {
    case IR_PR: ASSERT0(PR_no(this) != 0); return PR_no(this);
    case IR_STPR: ASSERT0(STPR_no(this) != 0); return STPR_no(this);
    case IR_GETELEM: ASSERT0(GETELEM_prno(this) != 0); return GETELEM_prno(this);
    case IR_SETELEM: ASSERT0(SETELEM_prno(this) != 0); return SETELEM_prno(this);
    case IR_CALL:
    case IR_ICALL: ASSERT0(CALL_prno(this) != 0); return CALL_prno(this);
    case IR_PHI: ASSERT0(PHI_prno(this) != 0); return PHI_prno(this);
    default: UNREACHABLE();
    }
    return 0;
}


SSAInfo * IR::getSSAInfo() const
{
    switch (getCode()) {
    case IR_PR: return PR_ssainfo(this);
    case IR_STPR: return STPR_ssainfo(this);
    case IR_PHI: return PHI_ssainfo(this);
    case IR_GETELEM: return GETELEM_ssainfo(this);
    case IR_SETELEM: return SETELEM_ssainfo(this);
    case IR_CALL:
    case IR_ICALL: return CALL_ssainfo(this);
    default:
        ASSERTN(!isReadPR() && !isWritePR() && !isWriteWholePR(),
                ("miss switch entry"));
        break;
    }
    return NULL;
}


IR * IR::getKid(UINT idx) const
{
    switch (getCode()) {
    case IR_UNDEF: ASSERTN(0, ("ir should not be undef")); break;
    case IR_ST: return ST_kid(this, idx);
    case IR_STPR: return STPR_kid(this, idx);
    case IR_STARRAY: return ARR_kid(this, idx);
    case IR_SETELEM: return SETELEM_kid(this, idx);
    case IR_GETELEM: return GETELEM_kid(this, idx);
    case IR_ILD: return ILD_kid(this, idx);
    case IR_IST: return IST_kid(this, idx);
    case IR_CALL: return CALL_kid(this, idx);
    case IR_ICALL: return ICALL_kid(this, idx);
    SWITCH_CASE_BIN: return BIN_kid(this, idx);
    SWITCH_CASE_UNA: return UNA_kid(this, idx);
    case IR_IGOTO: return IGOTO_kid(this, idx);
    case IR_DO_WHILE:
    case IR_WHILE_DO: return LOOP_kid(this, idx);
    case IR_DO_LOOP: return DOLOOP_kid(this, idx);
    case IR_IF: return IF_kid(this, idx);
    case IR_SWITCH: return SWITCH_kid(this, idx);
    case IR_CASE: return CASE_kid(this, idx);
    case IR_ARRAY: return ARR_kid(this, idx);
    case IR_TRUEBR:
    case IR_FALSEBR: return BR_kid(this, idx);
    case IR_RETURN: return RET_kid(this, idx);
    case IR_SELECT: return SELECT_kid(this, idx);
    SWITCH_CASE_EXP_NO_KID:
    SWITCH_CASE_STMT_NO_KID: return NULL;
    case IR_PHI: return PHI_kid(this, idx);
    default: ASSERTN(0, ("Unknown IR type"));
    }
    return NULL;
}


IRBB * IR::getBB() const
{
    switch (getCode()) {
    case IR_ST: return ST_bb(this);
    case IR_STPR: return STPR_bb(this);
    case IR_STARRAY: return STARR_bb(this);
    case IR_SETELEM: return SETELEM_bb(this);
    case IR_GETELEM: return GETELEM_bb(this);
    case IR_IST: return IST_bb(this);
    case IR_CALL:
    case IR_ICALL: return CALL_bb(this);
    case IR_GOTO: return GOTO_bb(this);
    case IR_IGOTO: return IGOTO_bb(this);
    case IR_SWITCH: return SWITCH_bb(this);
    case IR_TRUEBR:
    case IR_FALSEBR: return BR_bb(this);
    case IR_RETURN: return RET_bb(this);
    case IR_PHI: return PHI_bb(this);
    case IR_REGION: return REGION_bb(this);
    default: ASSERTN(0, ("This stmt can not be placed in basic block."));
    }
    return NULL;
}


Var * IR::getIdinfo() const
{
    ASSERT0(hasIdinfo());
    //DO NOT ASSERT even if current IR has no offset.
    switch (getCode()) {
    case IR_ID: return ID_info(this);
    case IR_LD: return LD_idinfo(this);
    case IR_LDA: return LDA_idinfo(this);
    case IR_ST: return ST_idinfo(this);
    case IR_CALL: return CALL_idinfo(this);
    default: UNREACHABLE();
    }
    return NULL;
}


void IR::setIdinfo(Var * idinfo)
{
    ASSERT0(hasIdinfo());
    //DO NOT ASSERT even if current IR has no offset.
    switch (getCode()) {
    case IR_ID: ID_info(this) = idinfo; break;
    case IR_LD: LD_idinfo(this) = idinfo; break;
    case IR_LDA: LDA_idinfo(this) = idinfo; break;
    case IR_ST: ST_idinfo(this) = idinfo; break;
    case IR_CALL: CALL_idinfo(this) = idinfo; break;
    default: UNREACHABLE();
    }
}


UINT IR::getOffset() const
{
    //DO NOT ASSERT even if current IR has no offset.
    switch (getCode()) {
    case IR_LD: return LD_ofst(this);
    case IR_ILD: return ILD_ofst(this);
    case IR_ARRAY: return ARR_ofst(this);
    case IR_ST: return ST_ofst(this);
    case IR_STARRAY: return ARR_ofst(this);
    case IR_IST: return IST_ofst(this);
    case IR_LDA: return LDA_ofst(this);
    default: break;
    }
    return 0;
}


IR * IR::getBase() const
{
    //DO NOT ASSERT even if current IR has no offset.
    switch (getCode()) {
    case IR_ILD: return ILD_base(this);
    case IR_ARRAY: return ARR_base(this);
    case IR_STARRAY: return ARR_base(this);
    case IR_IST: return IST_base(this);
    default: break;
    }
    return NULL;
}


//Return label info if exist.
LabelInfo const* IR::getLabel() const
{
    switch (getCode()) {
    case IR_TRUEBR:
    case IR_FALSEBR: return BR_lab(this);
    case IR_GOTO: return GOTO_lab(this);
    case IR_IGOTO: return NULL;
    case IR_LABEL: return LAB_lab(this);
    case IR_CASE: return CASE_lab(this);
    case IR_SWITCH: return SWITCH_deflab(this);
    default:;
    }
    return NULL;
}


UINT IR::getArrayElemDtSize(TypeMgr const* tm) const
{
    ASSERT0(is_array() || is_starray());
    return tm->getByteSize(ARR_elemtype(this));
}


bool IR::isConstExp() const
{
    if (is_const()) { return true; }
    if (is_cvt()) { return CVT_exp(this)->isConstExp(); }
    return false;
}


bool IR::isReadOnly() const
{
    switch (getCode()) {
    case IR_CALL: return CALL_is_readonly(this);
    case IR_ICALL: return ICALL_is_readonly(this);        
    case IR_CVT: return CVT_exp(this)->isReadOnly();
    case IR_LD:
        if (VAR_is_readonly(LD_idinfo(this)) &&
            !VAR_is_volatile(LD_idinfo(this))) {
            return true;
        }
        return false;
    default:;
    }
    return false;
}


bool IR::is_volatile() const
{
    //Describing if IR's address has been taken.
    if (is_id()) {
        Var * id_info = ID_info(this);
        ASSERT0(id_info != NULL);
        return VAR_is_volatile(id_info);
    }
    return false;
}


bool IR::isDirectArrayRef() const
{
    return isArrayOp() && ARR_base(this)->is_lda();
}


void IR::setBB(IRBB * bb)
{
    switch (getCode()) {
    case IR_ST: ST_bb(this) = bb; return;
    case IR_STPR: STPR_bb(this) = bb; return;
    case IR_STARRAY: STARR_bb(this) = bb; return;
    case IR_SETELEM: SETELEM_bb(this) = bb; return;
    case IR_GETELEM: GETELEM_bb(this) = bb; return;
    case IR_IST: IST_bb(this) = bb; return;
    case IR_CALL:
    case IR_ICALL: CALL_bb(this) = bb; return;
    case IR_GOTO: GOTO_bb(this) = bb; return;
    case IR_IGOTO: IGOTO_bb(this) = bb; return;
    case IR_SWITCH: SWITCH_bb(this) = bb; return;
    case IR_TRUEBR:
    case IR_FALSEBR: BR_bb(this) = bb; return;
    case IR_RETURN: RET_bb(this) = bb; return;
    case IR_PHI: PHI_bb(this) = bb; return;
    case IR_REGION: REGION_bb(this) = bb; return;
    default:
        //Do not assert to facilitate coding. Just return.
        //ASSERTN(0, ("Not stmt type"));
        return;
    }
}


void IR::setRHS(IR * rhs)
{
    switch (getCode()) {
    case IR_ST:
        ST_rhs(this) = rhs;
        if (rhs != NULL) {
            IR_parent(rhs) = this;
        }
        return;
    case IR_STPR:
        STPR_rhs(this) = rhs;
        if (rhs != NULL) {
            IR_parent(rhs) = this;
        }
        return;
    case IR_STARRAY:
        STARR_rhs(this) = rhs;
        if (rhs != NULL) {
            IR_parent(rhs) = this;
        }
        return;
    case IR_IST:
        IST_rhs(this) = rhs;
        if (rhs != NULL) {
            IR_parent(rhs) = this;
        }
        return;
    default: ASSERTN(0, ("not store operation."));
    }
}


void IR::setOffset(UINT ofst)
{
    ASSERT0(hasOffset());
    switch (getCode()) {
    case IR_LD: LD_ofst(this) = ofst; return;
    case IR_ST: ST_ofst(this) = ofst; return;
    case IR_ILD: ILD_ofst(this) = ofst; return;
    case IR_STARRAY: ARR_ofst(this) = ofst; return;
    case IR_IST: IST_ofst(this) = ofst; return;
    case IR_LDA: LDA_ofst(this) = ofst; return;
    case IR_ARRAY: ARR_ofst(this) = ofst; return;
    default: return;
    }
}


//Set ir's PR SSA Info to be NULL.
//For convenient purpose, this function does not assert
//when current IR object is not operate on PR.
void IR::clearSSAInfo()
{
    switch (getCode()) {
    case IR_PR: PR_ssainfo(this) = NULL; return;
    case IR_STPR: STPR_ssainfo(this) = NULL; return;
    case IR_SETELEM: SETELEM_ssainfo(this) = NULL; return;
    case IR_GETELEM: GETELEM_ssainfo(this) = NULL; return;
    case IR_CALL:
    case IR_ICALL: CALL_ssainfo(this) = NULL; return;
    case IR_PHI: PHI_ssainfo(this) = NULL; return;
    default: break;
    }
}


void IR::setSSAInfo(SSAInfo * ssa)
{
    switch (getCode()) {
    case IR_PR: PR_ssainfo(this) = ssa; return;
    case IR_STPR: STPR_ssainfo(this) = ssa; return;
    case IR_SETELEM: SETELEM_ssainfo(this) = ssa; return;
    case IR_GETELEM: GETELEM_ssainfo(this) = ssa; return;
    case IR_CALL:
    case IR_ICALL:
        ASSERTN(CALL_prno(this) != 0, ("does not have return value"));
        CALL_ssainfo(this) = ssa;
        return;
    case IR_PHI: PHI_ssainfo(this) = ssa; return;
    default: ASSERTN(0, ("unsupport"));
    }
}


void IR::setPrno(UINT prno)
{
    switch (getCode()) {
    case IR_PR: PR_no(this) = prno; return;
    case IR_STPR: STPR_no(this) = prno; return;
    case IR_SETELEM: SETELEM_prno(this) = prno; return;
    case IR_GETELEM: GETELEM_prno(this) = prno; return;
    case IR_CALL:
    case IR_ICALL: CALL_prno(this) = prno; return;
    case IR_PHI: PHI_prno(this) = prno; return;
    default: ASSERTN(0, ("unsupport"));
    }
}


//Return label or NULL.
void IR::setLabel(LabelInfo const* li)
{
    switch (getCode()) {
    case IR_TRUEBR:
    case IR_FALSEBR: BR_lab(this) = li; return;
    case IR_GOTO: GOTO_lab(this) = li; return;
    case IR_IGOTO:
        ASSERTN(0, ("must specify the specific target label."));
        return;
    case IR_LABEL: LAB_lab(this) = li; return;
    case IR_CASE: CASE_lab(this) = li; return;
    case IR_SWITCH: SWITCH_deflab(this) = li; return;
    default: ASSERTN(0, ("%s has not label", IRTNAME(getCode())));
    }
}


//Set the No.idx child to be 'kid', and update the IR_parent of kid.
void IR::setKid(UINT idx, IR * kid)
{
    switch (getCode()) {
    case IR_UNDEF: ASSERTN(0, ("ir should not be undef")); return;
    case IR_ST: ST_kid(this, idx) = kid; break;
    case IR_STPR: STPR_kid(this, idx) = kid; break;
    case IR_STARRAY: ARR_kid(this, idx) = kid; break;
    case IR_SETELEM: SETELEM_kid(this, idx) = kid; break;
    case IR_GETELEM: GETELEM_kid(this, idx) = kid; break;
    case IR_ILD: ILD_kid(this, idx) = kid; break;
    case IR_IST: IST_kid(this, idx) = kid; break;
    case IR_CALL: CALL_kid(this, idx) = kid; break;
    case IR_ICALL: ICALL_kid(this, idx) = kid; break;
    SWITCH_CASE_BIN: BIN_kid(this, idx) = kid; break;
    SWITCH_CASE_UNA: UNA_kid(this, idx) = kid; break;
    SWITCH_CASE_EXP_NO_KID:
    SWITCH_CASE_STMT_NO_KID: return;
    case IR_IGOTO: IGOTO_kid(this, idx) = kid; break;
    case IR_DO_WHILE:
    case IR_WHILE_DO: LOOP_kid(this, idx) = kid; break;
    case IR_DO_LOOP: DOLOOP_kid(this, idx) = kid; break;
    case IR_IF: IF_kid(this, idx) = kid; break;
    case IR_SWITCH: SWITCH_kid(this, idx) = kid; break;
    case IR_CASE: CASE_kid(this, idx) = kid; break;
    case IR_ARRAY: ARR_kid(this, idx) = kid; break;
    case IR_TRUEBR:
    case IR_FALSEBR: BR_kid(this, idx) = kid; break;
    case IR_RETURN: RET_kid(this, idx) = kid; break;
    case IR_SELECT: SELECT_kid(this, idx) = kid; break;
    case IR_PHI: PHI_kid(this, idx) = kid; break;
    default: ASSERTN(0, ("Unknown IR type"));
    }

    for (IR * k = kid; k != NULL; k = IR_next(k)) {
        IR_parent(k) = this;
    }
}


bool IR::isPREqual(IR const* src) const
{
    ASSERT0(isWritePR() && src->isReadPR());
    return getType() == src->getType() && getPrno() == src->getPrno();
}


//Check if 'exp' is child or grandchildren of current ir.
//Here we only compare equality of two IR pointer to determine and apply
//the DFS searching in tree.
bool IR::is_kids(IR const* exp) const
{
    if (exp == NULL) { return false; }
    IR * tmp;
    for (UINT i = 0; i < IR_MAX_KID_NUM(this); i++) {
        tmp = getKid(i);
        while (tmp != NULL) {
            if (exp == tmp) {
                return true;
            }
            if (tmp->is_kids(exp)) {
                return true;
            }
            tmp = IR_next(tmp);
        } //end while
    } //end for
    return false;
}


//Return true if k is the kid node of current ir.
bool IR::is_lhs(IR const* k) const
{
    ASSERT0(is_stmt());
    switch (getCode()) {
    case IR_ST:
    case IR_STPR:
    case IR_STARRAY:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_CALL:
    case IR_ICALL:
        return k == this;
    case IR_IST:
        return false;
    case IR_GOTO:
    case IR_IGOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_LABEL:
    case IR_CASE:
    case IR_SWITCH:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
    case IR_BREAK:
    case IR_CONTINUE:
        return false;
    case IR_PHI:
        return k == this;
    case IR_REGION:
        return false;
    default: UNREACHABLE();
    } //end switch
    return false;
}


//Return true if ir exactly modified 'md' or element in MDSet 'mds'.
bool IR::isExactDef(MD const* md, MDSet const* mds) const
{
    ASSERT0(is_stmt());

    MD const* cur_ir_defined_md = getRefMD();

    if (cur_ir_defined_md != NULL && cur_ir_defined_md->is_exact()) {
        if (md != NULL &&
            (cur_ir_defined_md == md || cur_ir_defined_md->is_overlap(md))) {
            return true;
        }

        if (mds != NULL && mds->is_contain_pure(MD_id(cur_ir_defined_md))) {
            return true;
        }
    }

    //We can not determine whether current ir is
    //exactly modified md or mds.
    return false;
}


bool IR::isExactDef(MD const* md) const
{
    ASSERT0(is_stmt() && md);
    if (!md->is_exact()) { return false; }

    MD const* cur_ir_defined_md = getRefMD();
    if (cur_ir_defined_md != NULL &&
        cur_ir_defined_md->is_exact() &&
        (cur_ir_defined_md == md || cur_ir_defined_md->is_overlap(md))) {
        return true;
    }
    return false;
}


//Set ir DU to be NULL, return the DU pointer.
DU * IR::cleanDU()
{
    switch (getCode()) {
    case IR_ID:
    case IR_LD:
    case IR_ILD:
    case IR_PR:
    case IR_ARRAY:
    case IR_ST:
    case IR_STPR:
    case IR_STARRAY:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_IST:
    case IR_CALL:
    case IR_ICALL: {
        DU * du = DUPROP_du(this);
        DUPROP_du(this) = NULL;
        return du;
    }
    default:;
    }
    return NULL;
}


//Return stmt if it writes PR as result.
IR * IR::getResultPR()
{
    ASSERT0(is_stmt());
    switch (getCode()) {
    case IR_ST: return NULL;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
        return this;
    case IR_CALL:
    case IR_ICALL:
        return hasReturnValue() ? this : NULL;
    case IR_STARRAY:
    case IR_IST:
    case IR_GOTO:
    case IR_IGOTO:
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_LABEL:
    case IR_SWITCH:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
    case IR_BREAK:
    case IR_CONTINUE:
        return NULL;
    case IR_PHI:
        return this;
    case IR_REGION:
        return NULL;
    default: UNREACHABLE();
    }
    return NULL;
}


//Return true if ir is call and does have a return value.
bool IR::hasReturnValue() const
{
    ASSERT0(isCallStmt());
    return CALL_prno(this) != 0;
}


//Return true if current ir is integer constant, and the number
//is equal to 'value'.
bool IR::isConstIntValueEqualTo(HOST_INT value) const
{
    if (!isConstExp()) { return false; }

    IR const* p = this;
    while (!p->is_const()) {
        ASSERTN(p->is_cvt(), ("const expression only include CVT and CONST."));
        p = CVT_exp(p);
        ASSERT0(p);
    }
    return p->is_int() && CONST_int_val(p) == value;
}


DU * IR::getDU() const
{
    return hasDU() ? DUPROP_du(this) : NULL;
}


void IR::setDU(DU * du)
{
    hasDU() ? DUPROP_du(this) = du : 0;
}


//Return true if stmt has judge determinate expression.
bool IR::hasJudgeDet() const
{
    //Both stmt and exp.
    switch (getCode()) {
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SELECT:
        return true;
    default: return false;
    }
    return false;
}


//Return determinate expression if any.
IR * IR::getJudgeDet() const
{
    //Both stmt and exp.
    switch (getCode()) {
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
        return LOOP_det(this);
    case IR_IF:
        return IF_det(this);
    case IR_TRUEBR:
    case IR_FALSEBR:
        return BR_det(this);
    case IR_SELECT:
        return SELECT_pred(this);
    default: UNREACHABLE();
    }
    return NULL;
}

//END IR


//Exported Functions.
CHAR const* compositeName(Sym const* n, xcom::StrBuf & buf);
void dumpIR(IR const* ir,
            Region const* rg,
            CHAR * attr = NULL,
            UINT dumpflag = IR_DUMP_KID|IR_DUMP_SRC_LINE|IR_DUMP_INNER_REGION|
                            IR_DUMP_VAR_DECL);
void dumpIRListH(IR * ir_list, Region const* rg,
                 CHAR * attr = NULL,
                 UINT dumpflag = IR_DUMP_KID|
                                 IR_DUMP_SRC_LINE|
                                 IR_DUMP_INNER_REGION|
                                 IR_DUMP_VAR_DECL);
void dumpIRList(IR * ir_list,
                Region const* rg,
                CHAR * attr = NULL,
                UINT dumpflag = IR_DUMP_KID|
                                IR_DUMP_SRC_LINE|
                                IR_DUMP_INNER_REGION|
                                IR_DUMP_VAR_DECL);
void dumpIRList(IRList & ir_list, Region const* rg);
void dumpIRList(List<IR*> & ir_list, Region const* rg);

class DumpGRCtx {
public:
    //Dump string literal and ignore the \n.
    bool dump_string_in_one_line;
    bool dump_inner_region;
    IRCFG * cfg;

    DumpGRCtx() { ::memset(this, 0, sizeof(DumpGRCtx)); }
};
void dumpGR(IR const* ir, TypeMgr * tm, DumpGRCtx * ctx);
void dumpGRInBBList(List<IRBB*> * bblist, TypeMgr * tm, DumpGRCtx * ctx);
void dumpGRList(IR * irlist, TypeMgr * tm, DumpGRCtx * ctx);

bool verifyIRList(IR * ir, BitSet * irh, Region const* rg);
bool verifyIRandBB(BBList * ir_bb_list, Region const* rg);
bool verifySimp(IR * ir, SimpCtx & simp);

//Iterative access ir tree. This funtion initialize the iterator.
//'ir': the root ir of the tree.
//'ii': iterator. It should be clean already.
//Readonly function.
inline IR const* iterInitC(IR const* ir, OUT ConstIRIter & ii)
{
    if (ir == NULL) { return NULL; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == NULL) { continue; }
        ii.append_tail(kid);
    }
    if (ir->get_next() != NULL) {
        ii.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access ir tree.
//This function return the next IR node accroding to 'ii'.
//'ii': iterator.
//Readonly function.
inline IR const* iterNextC(IN OUT ConstIRIter & ii)
{
    IR const* ir = ii.remove_head();
    if (ir == NULL) { return NULL; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == NULL) { continue; }
        ii.append_tail(kid);
    }
    if (ir->get_next() != NULL) {
        ii.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it must be stmt.
//'ii': iterator.
//This function is a readonly function.
//Use iterRhsNextC to iter next IR.
inline IR const* iterRhsInitC(IR const* ir, OUT ConstIRIter & ii)
{
    if (ir == NULL) { return NULL; }

    ASSERT0(ir->is_stmt());

    //Other stmt.
    IR const* firstkid = NULL;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR const* kid = ir->getKid(i);
        if (kid == NULL) { continue; }
        if (firstkid == NULL) {
            firstkid = kid;
            continue;
        }
        ii.append_tail(kid);
    }

    //IR const* x = ii.remove_head();
    //if (x == NULL) { return NULL; }

    if (firstkid == NULL) { return NULL; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(firstkid); i++) {
        IR const* kid = firstkid->getKid(i);
        if (kid == NULL) { continue; }
        ii.append_tail(kid);
    }

    if (IR_next(firstkid) != NULL) {
        ii.append_tail(IR_next(firstkid));
    }
    return firstkid;
}


//Iterative access the expression.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it must be expression.
//'ii': iterator.
//Readonly function.
//Use iterRhsNextC to iter next IR.
inline IR const* iterExpInitC(IR const* ir, OUT ConstIRIter & ii)
{
    if (ir == NULL) { return NULL; }
    ASSERT0(ir->is_exp());
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == NULL) { continue; }
        ii.append_tail(kid);
    }
    if (ir->get_next() != NULL) {
        ASSERTN(!ir->get_next()->is_stmt(), ("ir can not be stmt list"));
        ii.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'ii'.
//'ii': iterator.
//Readonly function.
inline IR const* iterRhsNextC(IN OUT ConstIRIter & ii)
{
    return iterNextC(ii);
}


//Iterative access the ir tree that start with 'ir'.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it may be either stmt or expression.
//'ii': iterator. It should be clean already.
//Note this function is NOT readonly, the returnd IR may be modified.
inline IR * iterInit(IN IR * ir, OUT IRIter & ii)
{
    if (ir == NULL) { return NULL; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == NULL) { continue; }
        ii.append_tail(kid);
    }
    if (ir->get_next() != NULL) {
        ii.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the ir tree.
//This funtion return the next IR node accroding to 'ii'.
//'ii': iterator.
//Note this function is NOT readonly, the returnd IR may be modified.
inline IR * iterNext(IN OUT IRIter & ii)
{
    IR * ir = ii.remove_head();
    if (ir == NULL) { return NULL; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == NULL) { continue; }
        ii.append_tail(kid);
    }
    if (ir->get_next() != NULL) {
        ii.append_tail(ir->get_next());
    }
    return ir;
}


//Iterative access the right-hand-side expression of stmt.
//This funtion initialize the iterator.
//'ir': the root ir of the tree, it must be stmt.
//'ii': iterator. It should be clean already.
//Use iterRhsNextC to iter next IR.
inline IR * iterRhsInit(IR * ir, OUT IRIter & ii)
{
    if (ir == NULL) { return NULL; }

    ASSERT0(ir->is_stmt());

    //Other stmt.
    IR * firstkid = NULL;
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == NULL) { continue; }
        if (firstkid == NULL) {
            firstkid = kid;
            continue;
        }
        ii.append_tail(kid);
    }

    if (firstkid == NULL) { return NULL; }

    for (UINT i = 0; i < IR_MAX_KID_NUM(firstkid); i++) {
        IR * kid = firstkid->getKid(i);
        if (kid == NULL) { continue; }
        ii.append_tail(kid);
    }

    if (IR_next(firstkid) != NULL) {
        ii.append_tail(IR_next(firstkid));
    }

    return firstkid;
}


//Iterative access the right-hand-side expression of stmt.
//This function return the next IR node accroding to 'ii'.
//'ii': iterator.
//This is a readonly function.
inline IR * iterRhsNext(IN OUT IRIter & ii)
{
    return iterNext(ii);
}


inline bool is_commutative(IR_TYPE irt)
{ return IRDES_is_commutative(g_ir_desc[irt]); }

inline bool isBinaryOp(IR_TYPE irt)
{ return IRDES_is_bin(g_ir_desc[irt]); }

inline bool isUnaryOp(IR_TYPE irt)
{ return IRDES_is_una(g_ir_desc[irt]); }

inline IR_TYPE invertIRType(IR_TYPE src)
{
    switch(src) {
    case IR_LT: return IR_GE;
    case IR_LE: return IR_GT;
    case IR_GT: return IR_LE;
    case IR_GE: return IR_LT;
    case IR_EQ: return IR_NE;
    case IR_NE: return IR_EQ;
    default: ASSERTN(0, ("unsupport"));
    }
    return IR_UNDEF;
}

void setParentPointerForIRList(IR * ir_list);
UINT getArithPrecedence(IR_TYPE ty);
bool allBeExp(IR * irlst);
bool allBeStmt(IR * irlst);
bool checkMaxIRType();
bool checkIRDesc();
bool checkRoundDesc();
} //namespace xoc
#endif
