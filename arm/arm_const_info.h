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
#ifndef __ARM_CONST_INFO_H__
#define __ARM_CONST_INFO_H__

//Note RegionMgr::verifyPreDefinedInfo will do
//sanity verification before get to work.

#define BIT_PER_BYTE 8
#define BYTE_PER_CHAR 1
#define BYTE_PER_SHORT 2
#define BYTE_PER_INT 4
#define BYTE_PER_LONG 4
#define BYTE_PER_LONGLONG 8
#define BYTE_PER_FLOAT 4
#define BYTE_PER_DOUBLE 8
#define BYTE_PER_POINTER 4

//Define target machine general register byte size.
#define GENERAL_REGISTER_SIZE BYTE_PER_POINTER

//Define the max bit length of HOST_INT that indicates the max integer
//that compiler can represent.
//Note HOST_INT must be signed, and HOST_UINT must be unsigned.
#define HOST_INT LONGLONG
#define HOST_UINT ULONGLONG
#define HOST_FP double
#define HOST_BIT_PER_BYTE 8
#define HOST_BYTE_PER_INT 8

//Defined result SR index of multiple-words-load.
#define PAIR_LOW_RES_IDX 0
#define PAIR_HIGH_RES_IDX 1
#define PAIR_LOW_OPND_IDX 1
#define PAIR_HIGH_OPND_IDX 2

//Describe the maximum byte size that can be allocated on host machine stack.
//The threshold often used in allocating memory via ALLOCA.
#define HOST_STACK_MAX_USABLE_MEMORY_BYTE_SIZE 65536

//Bit size of word length of host machine.
#define WORD_LENGTH_OF_HOST_MACHINE (HOST_BIT_PER_BYTE * HOST_BYTE_PER_INT)

//Bit size of word length of target machine.
#define WORD_LENGTH_OF_TARGET_MACHINE (GENERAL_REGISTER_SIZE * BIT_PER_BYTE)

//Bit size of double word length of target machine.
#define DWORD_LENGTH_OF_TARGET_MACHINE (WORD_LENGTH_OF_TARGET_MACHINE * 2)

//Byte size of word of target machine.
#define BYTESIZE_OF_WORD (GENERAL_REGISTER_SIZE)

//Byte size of double word of target machine.
#define BYTESIZE_OF_DWORD (BYTESIZE_OF_WORD * 2)

//If the number of OR of one BB is larger than following value,
//all local optimizations are disabled.
#define MAX_OR_BB_OPT_BB_LEN 1000

//Define target machine data or register word length.
//Note TMWORD must be unsigned.
#define TMWORD UINT32

//Define target machine instruction word length.
//Note TMIWORD must be unsigned.
#define TMIWORD UINT32

//Define the minimum target machine memory operations alignment.
//The alignment is power of 2 on ARM.
#define MEMORY_ALIGNMENT 4

//Define the minimum target machine stack variable alignment.
//The alignment is power of 2 on ARM.
#define STACK_ALIGNMENT 4

//Define the minimum target machine code alignment.
//The alignment is power of 2 on ARM.
#define CODE_ALIGNMENT 4

//Define target machine stack pointer adjustment operation's alignment.
//The alignment should not less than STACK_ALIGNMENT.
#define SPADJUST_ALIGNMENT 8

//Define default float mantissa in output file, such as GR file.
#define DEFAULT_MANTISSA_NUM 6

//Maximum byte size of target machine stack.
#define MAX_STACK_SPACE (16*1024*1024)

//Define the order to pushing parameter before function call.
//true: from right to left
//false: from left to right
#define PUSH_PARAM_FROM_RIGHT_TO_LEFT true

//Define whether target machine support predicate register.
//Note the first opnd must be predicate register if target support.
#define HAS_PREDICATE_REGISTER true

//Define the operand index of literal offset of Spadjust operation.
#define SPADJUST_OFFSET_INDX 1

//Define the max/min integer value range of target machine.
#ifndef MIN_HOST_INT_VALUE
#define MIN_HOST_INT_VALUE 0x80000000
#endif
#ifndef MAX_HOST_INT_VALUE
#define MAX_HOST_INT_VALUE 0x7fffFFFF
#endif
#define EPSILON 0.000001

//Display/dump integer literal with hex if it is
//greater than this threshold.
#define THRESHOLD_DISPLAY_IN_HEX 0x10000000

//Defined the threshold of Dominator Frontier Density.
//Higher Dominator Frontier Density might make SSAMgr inserting
//ton of PHIs which will blow up memory.
//This is an expirical value.
#define THRESHOLD_HIGH_DOMINATOR_FRONTIER_DENSITY 70000

//Defined macros to skip some special argument registers when passing
//arguments.
#define TO_BE_COMPATIBLE_WITH_ARM_LINUX_GNUEABI
#define CONTINUOUS_REG_NUM 2

//Defined the maximum number of operand of OR.
#define MAX_OR_OPERAND_NUM 8
//Defined the maximum number of result of OR.
#define MAX_OR_RESULT_NUM 4

//Defined the maximum number of OR in Issue-Package.
#define MAX_OR_NUM_IN_ISSUE_PACKAGE 2

//Issue slot for multi-issue architecture.
//FIRST_SLOT must be equal to 0.
typedef enum _SLOT {
    FIRST_SLOT = 0,
    SLOT_G = 0,
    LAST_SLOT = SLOT_G,
    SLOT_NUM = 1,
} SLOT;
#define SLOT_NAME_MAX_LEN 10

//Machine function units for all clusters.
//Note that function unit and issue slot do not have to be one to one mapped.
//e.g: an instruction that issued at slot-A might occupied unit-1, unit-2.
// Some instructions can only be executed on specific function unit.
typedef enum _UNIT {
    UNIT_UNDEF = 0, //Necessary for all target
    UNIT_A = 1, //A
    UNIT_NUM = 2,
} UNIT;

//Clusters.
typedef enum _CLUST {
    CLUST_UNDEF = 0, //Necessary for all target
    CLUST_FIRST = 1,
    CLUST_SCALAR = CLUST_FIRST,
    CLUST_NUM = 2,
} CLUST;

//The ARM processor has a total of 37 registers
//Register files
// D0=S0, S1
// D1=S2, S3
// D2=S4, S5
// D3=S6, S7
// D4=S8, S9
// D5=S10,S11
// D6=S12,S13
// D7=S14,S15
// D8=S16,S17
// D9=S18,S19
// D10=S20,S21
// D11=S22,S23
// D12=S24,S25
// D13=S26,S27
// D14=S28,S29
// D15=S30,S31
//
// Q0=D0,D1
// Q1=D2,D3
// Q2=D4,D5
// Q3=D6,D7
// Q4=D8,D9
// Q5=D10,D11
// Q6=D12,D13
// Q7=D14,D15
// Q8=D16,D17
// Q9=D18,D19
// Q10=D20,D21
// Q11=D22,D23
// Q12=D24,D25
// Q13=D26,D27
// Q14=D28,D29
// Q15=D30,D31
typedef enum _REGFILE {
    RF_UNDEF = 0, //Necessary for all target
    RF_R = 1,     //16 32-bit general register, R0-R15

    //32 64-bit doubleword registers in VFP, D0-D31
    //In VFPv3-D16 and VFPv2, the VFP consists of 16 doubleword registers, and
    //only 16 64-bit doubleword registers, D0-D15, are accessible.
    RF_D = 2,

    //16 128-bit quadword registers, Q0-Q15
    RF_Q = 3,

    //32 32-bit singleword registers, S0-S31
    //Only half of the VFP is accessible in this mode.
    RF_S = 4,
    RF_SP = 5,    //R13, the stack pointer

    //R14, also known as the Link Register
    //When an exception occurs, the appropriate exception mode's
    //version of R14 is set to the exception return
    //address (offset by a small constant for some exceptions).
    RF_LR = 6,

    RF_PC = 7,    //R15, the program counter
    RF_CPSR = 8,  //current program status register

    //Saved program status register,
    //that is used to preserve the value of the
    //CPSR when the associated exception occurs.
    RF_SPSR = 9,

    //Predicate register, includes
    //EQ  Equal
    //NE  Not equal
    //CS  Carry set (identical to HS)
    //HS  Unsigned higher or same (identical to CS, Unsigned GE)
    //CC  Carry clear (identical to LO)
    //LO  Unsigned lower (identical to CC, Unsigned LT)
    //MI  Minus or negative result
    //PL  Positive or zero result
    //VS  Overflow
    //VC  No overflow
    //HI  Unsigned higher
    //LS  Unsigned lower or same
    //GE  Signed greater than or equal
    //LT  Signed less than
    //GT  Signed greater than
    //LE  Signed less than or equal
    //AL  Always (this is the default)
    RF_P = 10,
    RF_NUM = 11,
} REGFILE;

//Define mnemonic for ARM physical register.
#define REG_UNDEF 0 //Reserved undefined physical register id
#define REG_R0 1
#define REG_R1 2
#define REG_R2 3
#define REG_R3 4
#define REG_R4 5
#define REG_R5 6
#define REG_R11 12
#define REG_R15 16
#define REG_FP 8 //R7
#define REG_TMP 13 //R12, Scratch Register, the synonym is IP register.
#define REG_ZERO REG_R0
#define REG_SP 14
#define REG_D0 17
#define REG_D31 48
#define REG_Q0 49
#define REG_Q15 64
#define REG_S0 65
#define REG_S31 96
#define REG_RFLAG_REGISTER 97
#define REG_EQ_PRED 98
#define REG_NE_PRED 99
#define REG_CS_PRED 100
#define REG_HS_PRED 101
#define REG_CC_PRED 102
#define REG_LO_PRED 103
#define REG_MI_PRED 104
#define REG_PL_PRED 105
#define REG_VS_PRED 106
#define REG_VC_PRED 107
#define REG_HI_PRED 108
#define REG_LS_PRED 109
#define REG_GE_PRED 110
#define REG_LT_PRED 111
#define REG_GT_PRED 112
#define REG_LE_PRED 113
#define REG_TRUE_PRED 114
#define REG_RETURN_ADDRESS_REGISTER 15
#define REG_LAST 114 //The last physical register
#define REG_NUM (REG_LAST+1) //The number of physical register

//Define mnemonic for register set for each regfile.
#define RF_R_REG_START REG_R0
#define RF_R_REG_END REG_R15
#define RF_D_REG_START REG_D0
#define RF_D_REG_END REG_D31
#define RF_Q_REG_START REG_Q0
#define RF_Q_REG_END REG_Q15
#define RF_S_REG_START REG_S0
#define RF_S_REG_END REG_S31

//Define reigsters to pass argument.
#define ARG_REG_START REG_R0
#define ARG_REG_END REG_R3

//Define reigsters to pass return value.
#define RETVAL_REG_START REG_R0
#define RETVAL_REG_END REG_R1

//Define callee saved reigsters.
#define CALLEE_SAVED_REG_START REG_R4
#define CALLEE_SAVED_REG_END REG_R11

//Define caller saved reigsters.
#define CALLER_SAVED_REG_START REG_R0
#define CALLER_SAVED_REG_END REG_R3

//Define allocable registers.
#define ALLOCABLE_REG_START REG_R4
#define ALLOCABLE_REG_END REG_R11

typedef enum _BUILTIN_TYPE {
    BUILTIN_UNDEF = 0,
    BUILTIN_BASE_BEGIN = BUILTIN_UNDEF + 1,
    BUILTIN_MEMCPY = BUILTIN_BASE_BEGIN,

    //////////////////////////////////////////////////////////////////////////
    //MOTE: The target specific builtin that inherited from base type should//
    //encode BUILTIN type based on BUILTIN_BASE_END.                        //
    //////////////////////////////////////////////////////////////////////////
    BUILTIN_BASE_END = BUILTIN_MEMCPY,

    //BEGIN: arm specific builtin function.
    BUILTIN_UIMOD = BUILTIN_BASE_END + 1,
    BUILTIN_IMOD,
    BUILTIN_UIDIV,
    BUILTIN_ASHLDI3,
    BUILTIN_LSHRDI3,
    BUILTIN_ASHRDI3,
    BUILTIN_MODSI3,
    BUILTIN_UMODSI3,
    BUILTIN_MODDI3,
    BUILTIN_UMODDI3,
    BUILTIN_ADDSF3,
    BUILTIN_ADDDF3,
    BUILTIN_SUBSF3,
    BUILTIN_SUBDF3,
    BUILTIN_DIVSI3,
    BUILTIN_UDIVSI3,
    BUILTIN_DIVSF3,
    BUILTIN_DIVDI3,
    BUILTIN_UDIVDI3,
    BUILTIN_DIVDF3,
    BUILTIN_MULDI3,
    BUILTIN_MULSF3,
    BUILTIN_MULDF3,
    BUILTIN_LTSF2,
    BUILTIN_GTSF2,
    BUILTIN_GESF2,
    BUILTIN_EQSF2,
    BUILTIN_NESF2,
    BUILTIN_LESF2,
    BUILTIN_LTDF2,
    BUILTIN_GTDF2,
    BUILTIN_GEDF2,
    BUILTIN_EQDF2,
    BUILTIN_NEDF2,
    BUILTIN_LEDF2,
    BUILTIN_FIXSFSI,
    BUILTIN_FIXDFSI,
    BUILTIN_FIXUNSSFSI,
    BUILTIN_FIXUNSDFSI,
    BUILTIN_FIXUNSSFDI,
    BUILTIN_FIXUNSDFDI,
    BUILTIN_TRUNCDFSF2,
    BUILTIN_FLOATSISF,
    BUILTIN_FLOATDISF,
    BUILTIN_FLOATSIDF,
    BUILTIN_FLOATDIDF,
    BUILTIN_FIXSFDI,
    BUILTIN_FIXDFDI,
    BUILTIN_FLOATUNSISF,
    BUILTIN_FLOATUNDISF,
    BUILTIN_FLOATUNSIDF,
    BUILTIN_FLOATUNDIDF,
    BUILTIN_EXTENDSFDF2,

    ///////////////////////////////////////
    //DO NOT ADD NEW TYPE AFTER THIS LINE//
    ///////////////////////////////////////
    BUILTIN_NUM,
} BUILTIN_TYPE;

//Instruction Operation Type.
typedef enum _OR_CODE {
    OR_UNDEF = 0,
    OR_b, //branch to label
    OR_bl, //branch to label, update link register
    OR_blx, //branch to register, update link register and status
    OR_bx, //branch to register, update status

    OR_mov,
    OR_mov_i,
    OR_movw_i, //mov low-16bit of 32bit imm into reigster
    OR_movt_i, //mov high-16bit of 32bit imm into reigster
    OR_mov32_i, //pseduo-instruction, mov 32-bit imm into register.
    OR_mvn, //move reversed data
    OR_mvn_i,

    OR_cmp, //compare
    OR_cmp_i,
    OR_cmn, //compare reversed data
    OR_cmn_i,

    OR_tst, //bit test
    OR_tst_i, //bit test
    OR_teq, //equality test
    OR_teq_i,

    OR_add,
    OR_adds,
    OR_adc,
    OR_adcs,
    OR_sub,
    OR_subs,
    OR_sbc, //carried subtraction
    OR_sbcs, //carried subtraction
    OR_add_i,
    OR_adds_i,
    OR_adc_i,
    OR_adcs_i,
    OR_sub_i,
    OR_subs_i,
    OR_sbc_i, //carried subtraction
    OR_sbcs_i, //carried subtraction

    OR_rsb, //reverse subtract without carry
    OR_rsbs,
    OR_rsc, //reverse subtract with carry
    OR_rscs,
    OR_rsb_i,
    OR_rsbs_i,
    OR_rsc_i,
    OR_rscs_i,

    OR_and, //logical AND
    OR_ands_asr_i, //logical OR with Rn register logical shift right.
    OR_and_i, //logical AND
    OR_orr, //logical OR
    OR_orrs, //logical OR
    OR_orr_i, //logical OR
    OR_orr_lsr_i, //logical OR with Rn register logical shift right.
    OR_orr_lsl_i, //logical OR with Rn register logical shift left.
    OR_eor, //exclusive OR
    OR_eor_i, //exclusive OR
    OR_bic, //bit clear

    OR_mul, //multiply (32-bit by 32-bit, bottom 32-bit result).
    OR_mla, //32bits multiplcation and addition.
    OR_smull, //64bits signed multiplcation
              //signed long multiply (32-bit by 32-bit, 64-bit
              //result or 64-bit accumulator).
    OR_smlal, //64bits signed multiplcation and addition.
              //signed long multiply and accumulate (32-bit by
              //32-bit, 64-bit result or 64-bit accumulator).
              //smlal RdLo, RdHi <- Rs1, Rs2, RdLo, RdHi
    OR_umull, //64bits unsigned multiplcation
              //unsigned long multiply (32-bit by 32-bit,
              //64-bit result or 64-bit accumulator).
    OR_umlal, //64bits unsigned multiplcation and addition.
              //unsigned long multiply and accumulate (32-bit by
              //32-bit, 64-bit result or 64-bit accumulator).
              //umlal RdLo, RdHi <- Rs1, Rs2, RdLo, RdHi
    OR_mrs, //CPSR status register -> gpr
    OR_msr, //gpr -> CPSR status register
    OR_swp, //word data swap
    OR_swpb, //byte data swap
    OR_lsl, //logical shift left
    OR_lsl_i,
    OR_lsl_i32,
    OR_lsr, //logical shift right
    OR_lsr_i,
    OR_lsr_i32,
    OR_asl, //arith shift left
    OR_asl_i,
    OR_asr, //arith shift right
    OR_asr_i,
    OR_asr_i32,
    OR_ror, //cycle shift right
    OR_ror_i,
    OR_rrx, //carried cycle shift right
    OR_neg, //negate the value in register

    //Direct load
    OR_ldm, //load multiple words.
    OR_ldr, //load 4 byte
    OR_ldrb, //load 1 byte, zero extend to 32 bits.
    OR_ldrsb, //load 1 byte, sign extend to 32 bits.
    OR_ldrh, //load 2 bytes, zero extend to 32 bits.
    OR_ldrsh, //load 2 bytes, sign extend to 32 bits.
    OR_ldrd, //load 8 bytes. load double word. 32bit offset.

    //Indirect load via base-register + immdediate-offset.
    OR_ldr_i12, //load word
    OR_ldrb_i12,
    OR_ldrsb_i8,
    OR_ldrh_i8,
    OR_ldrsh_i8,
    OR_ldrd_i8,

    //Direct store
    OR_stm, //store multiple words.
    OR_str, //store 4 byte
    OR_strb, //store 1 byte, zero extend to 32 bits.
    OR_strsb, //store 1 byte, sign extend to 32 bits.
    OR_strh, //store 2 bytes, zero extend to 32 bits.
    OR_strsh, //store 2 bytes, sign extend to 32 bits.
    OR_strd, //store double word to memory.

    //Indirect load via base-register + immdediate-offset.
    OR_str_i12,
    OR_strb_i12,
    OR_strh_i8,
    OR_strd_i8,

    OR_ret,
    OR_ret1,
    OR_ret2,
    OR_ret3,
    OR_ret4,
    OR_nop,
    OR_asm,
    OR_spadjust_i,
    OR_spadjust_r,
    OR_label,
    OR_built_in,
    OR_push,
    OR_pop,
    OR_LAST,
} OR_CODE;
#define OR_NUM OR_LAST

#include "arm_mach_def.h"

#define BYTE_2ND 1
#define BYTE_1ST 2
#define BYTE_3RD 3
#define GET_INST_1ST_BYTE(val) ((val) & 0x000000FF)
#define GET_INST_2ND_BYTE(val) (((val) & 0x0000FF00) >> 8)
#define GET_INST_3RD_BYTE(val) (((val) & 0x00FF0000) >> 16)
#define GET_INST_4TH_BYTE(val) (((val) & 0xFF000000) >> 24)
//Get 1st and 2nd bits.
#define GET_INST_LOW16_BITS(val) ((val) & 0x0000FFFF)
//Get 3rd and 4th bits.
#define GET_INST_HIGH16_BITS(val) (((val) & 0xFFFF0000) >> 16)
#define GET_VALUE_LOW4_BITS(val) ((val) & 0x0000000F)

#endif
