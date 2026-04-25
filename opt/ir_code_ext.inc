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
//Represents a virtual store to PR.
IR_VSTPR = IR_LAST_COMMON_CODE + 1,

//Represents a virtual store to Var.
IR_VST,

//Represents a virtual indirect store.
IR_VIST,

//Represent broadcast operation.
IR_BROADCAST,

//Represent atomic inc operation of fetch and add on memory.
IR_ATOMINC,

//Represent atomic cas operation of compare and swap on memory.
IR_ATOMCAS,

//Represent operation that has a mask operand. Usually the operation is used
//to describe that by given a full-size vector or tensor operation, using a
//mask operand to select specific elements for processing.
//e.g: st:vec<f32x16> x = maskop:vec<f32x16> (add:vec<f32x16> $a, $b), $mask;
//  'maskop' turned normal full-size add-operation into a masked-add-operation.
IR_MASK_OP,

//This IR is used to represent a scalable vector operation.
//By specifying the length, the number of elements in the operation
//can be controlled. Through the 'tail_strategy' field, the behavior of the
//tail elements of the vector can be controlled.
IR_DYNLEN_OP,

//This class represent operation that perform elements selection according to
//masks and variable length instructions.
//This is used to describe specific selection operation, Used to describe
//the result of placing masked or variable length instructions at certain
//positions in an operand, i.e. with partially modified semantics.
//e.g:
//st:vec<s32m1x?> x =
//  select_to_res:vec<s32m1x?>
//    maskop:vec<s32m1x?>
//      dynlenop:vec<s32m1x?>
//        (add:vec<s32m1x?> $a, $b)
//        $len
//      $mask
//    ld:<s32m1> x;
//The 'select_to_res' operation places the results of activating positions
//in 'maskop' and 'dynlenop' operations in the corresponding position of ld 'x'.
//NOTE:The presence of ld:<s32m1> x is used to describe the DU reference to
//the old  value of 'x'.
//This instruction is usually used in together with mask instructions
//or variable length instructions.
IR_SELECT_TO_RES,

//Represent physical register.
//NOTE: the IR code is available only when REF_TARGMACH_INFO is enabled.
IR_PHYREG,

//Placeholder IR code for corresponding SWITCH-CASE entry.
IR_EXT_UNA_PLACEHOLDER,
IR_EXT_BIN_PLACEHOLDER,

#include "targ_code_ext.inc"
