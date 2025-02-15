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
typedef enum {
    IR_UNDEF = 0,
    IR_CONST, //Constant value: include integer, float, string.
    IR_ID, //Identifier of variable.
    IR_LD, //Load from variable
    IR_ILD, //Indirect load.

    //Temporary Pseudo Register which can NOT be taken address, and can be
    //regarded as both register and memory.
    IR_PR,
    IR_ARRAY, //Array operation, include base and ofst.
    IR_ST, //Store to variable.
    IR_STPR, //Store to PR.
    IR_STARRAY, //Store to array.
    IR_IST, //Indirect store.
    IR_SETELEM, //Set element of PR, where PR is memory chunk or vector.
    IR_GETELEM, //Get element of PR, where PR is memory chunk or vector.
    IR_CALL, //Direct call.
    IR_ICALL, //Indirect call.
    IR_LDA, //Move variable's address to a register.
    IR_ADD, //Addition.
    IR_SUB, //Substraction.
    IR_MUL, //Multiplication.
    IR_DIV, //Division.
    IR_REM, //Remainder operation
    IR_MOD, //Modulus operation
    IR_LAND, //Logical AND, &&
    IR_LOR, //Logical OR, ||
    IR_BAND, //Bitwise AND, &
    IR_BOR, //Bitwise OR, |
    IR_XOR, //Exclusive OR.
    IR_ASR, //Arithmetic shift right
    IR_LSR, //Logical shift right
    IR_LSL, //Logical shift left
    IR_LT, //Less than.
    IR_LE, //Less than or equal to.
    IR_GT, //Greater than.
    IR_GE, //Greater than or equal to.
    IR_EQ, //Equal to.
    IR_NE, //Not equal to.
    IR_BNOT, //Bitwise not, e.g BNOT(0x0001) = 0xFFFE
    IR_LNOT, //Boolean logical not e.g LNOT(0x0001) = 0x0000
    IR_NEG, //Negative operation.
    IR_CVT, //Data-type convert
    IR_GOTO, //Goto definitely label.
    IR_IGOTO, //Indirect Goto a list of definitely label.
    IR_DO_WHILE, //Do-While loop struct.
    IR_WHILE_DO, //While-Do loop struct.

    //A kind of loop with plainly definition of INIT(low bound), HIGH bound,
    //LOOP-BODY and STEP of IV.
    IR_DO_LOOP,
    IR_IF, //High level IF clasuse, include det, truebody, and false body
    IR_LABEL, //Describe internal and customer defined label.

    //Switch clause, include determinant expression, a list of case, and body.
    IR_SWITCH,

    //Represent a list expressions that represent dummy-use.
    //e.g: given a call to foo(x), user can build a dummyuse to represent
    //global variables that foo(x) referenced, such as:
    //call foo(x, dummyuse(ld g, ld m)).
    IR_DUMMYUSE,
    IR_CASE, //CASE VALUE, this is used only within SWITCH clause.
    IR_TRUEBR, //Branch if determinant express is true.
    IR_FALSEBR, //Branch if determinant express is false.
    IR_RETURN, //Return Statement.

    //Conditional select true-exp or false-exp , formalized as :
    //determinant expression ? true-exp : false-exp
    IR_SELECT,

    //Terminate current loop end switch execution, which include do-loop,
    //do-while, while-do, and switch stmt.
    IR_BREAK,

    //Re-execute loop, which include do-loop, do-while, while-do.
    IR_CONTINUE,

    //Dynamically allocate stack memory.
    IR_ALLOCA,
    IR_POW, //x^a, caculate the x to a power.
    IR_NROOT, //x^(1/nth_root), caculate the nth-root of x.
    IR_LOG, //log_a(x), caculate the logarithm of x to the base a.
    IR_EXPONENT, //a^(x), caculate the power x of a.
    IR_ABS, //calculate the absolute value.
    IR_SIN, //trigonometric function:sin
    IR_COS, //trigonometric function:cos
    IR_TAN, //trigonometric function:tangent
    IR_ASIN, //trigonometric function:arcsin
    IR_ACOS, //trigonometric function:arccos
    IR_ATAN, //trigonometric function:arctan
    IR_PHI, //Phi statement.
    IR_REGION, //Region statement.

    //dwarf
    IR_CFI_DEF_CFA,
    IR_CFI_SAME_VALUE,
    IR_CFI_OFFSET,
    IR_CFI_RESTORE,
    IR_CFI_DEF_CFA_OFFSET,

    IR_LAST_COMMON_CODE = IR_CFI_DEF_CFA_OFFSET,

    #include "ir_code_ext.inc"

    ///////////////////////////////////////////////////////////////////////////
    //DO NOT ADD NEW IR CODE AFTER IR_CODE_NUM.                              //
    ///////////////////////////////////////////////////////////////////////////
    IR_CODE_NUM //The last IR code, the number of IR code.

    ///////////////////////////////////////////////////////////////////////////
    //NOTE: EXTEND IR::code BIT LENGTH IF THE MAXIMUM CODE IS LARGER THAN    //
    //IR_CODE_BIT_SIZE.                                                      //
    ///////////////////////////////////////////////////////////////////////////
} IR_CODE;
