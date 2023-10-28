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
#ifndef __DEX_CONST_INFO_H__
#define __DEX_CONST_INFO_H__

//Host integer and float point type.
//e.g: Build XOC on x8664, HOST_INT should be 64bit.
//Or build XOC on ARM, HOST_INT should be 32bit,
//of course 64bit is ok if you want.
#define HOST_INT LONGLONG
#define HOST_UINT LONGLONG
#define HOST_FP double
#define HOST_BYTE_PER_INT 8

//Describe the maximum byte size that can be allocated on host machine stack.
//The threshold often used in allocating memory via ALLOCA.
#define HOST_STACK_MAX_USABLE_MEMORY_BYTE_SIZE 32768

//Define signed and unsigned integer type on host machine.
#define INT8 CHAR
#define UINT8 UCHAR
#define INT16 SHORT
#define UINT16 USHORT
#define INT32 INT
#define UINT32 UINT
#define INT64 LONGLONG
#define UINT64 ULONGLONG

//If the number of OR of one BB is larger than following value,
//all local optimizations are disabled.
#define MAX_OR_BB_OPT_BB_LEN 1000

//Define machine word/half-word/byte/bit size
#define BIT_PER_BYTE 8
#define BYTE_PER_CHAR 1
#define BYTE_PER_SHORT 2
#define BYTE_PER_INT 4
#define BYTE_PER_LONG 4
#define BYTE_PER_LONGLONG 8
#define BYTE_PER_FLOAT 4
#define BYTE_PER_DOUBLE 8
#define BYTE_PER_ENUM 4
#define BYTE_PER_POINTER 4
#define GENERAL_REGISTER_SIZE (BYTE_PER_POINTER)

//Bit size of word length of host machine.
#define WORD_LENGTH_OF_HOST_MACHINE (HOST_BIT_PER_BYTE * HOST_BYTE_PER_INT)

//Bit size of word length of target machine.
#define WORD_LENGTH_OF_TARGET_MACHINE (GENERAL_REGISTER_SIZE * BIT_PER_BYTE)

//Define default float mantissa in output file, such as GR file.
#define DEFAULT_MANTISSA_NUM 6

//Represent target machine word with host type.
#define TMWORD UINT

//Define the minimum target machine memory operations alignment.
//The alignment is power of 2.
#define MEMORY_ALIGNMENT 4

//Define the minimum target machine stack variable alignment.
//The alignment is power of 2.
#define STACK_ALIGNMENT 4

//Define the minimum target machine code alignment.
//The alignment is power of 2.
#define CODE_ALIGNMENT 4

//Setting for compiler build-environment. Byte length.
#define HOST_BIT_PER_BYTE 8

//Maximum memory space of stack variables.
#define MAX_STACK_SPACE 16*1024*1024

//The order of pushing parameter when function call.
//true: from right to left
//false: from left to right
#define PUSH_PARAM_FROM_RIGHT_TO_LEFT true

//The number of registers which be used to store return value.
#define NUM_OF_RETURN_VAL_REGISTERS 2

//Define whether target machine support predicate register.
//Note the first opnd must be predicate register if target support.
#define HAS_PREDICATE_REGISTER false

//Define the max/min integer value range of target machine.
#ifndef MIN_HOST_INT_VALUE
#define MIN_HOST_INT_VALUE 0x80000000
#endif
#ifndef MAX_HOST_INT_VALUE
#define MAX_HOST_INT_VALUE 0x7fffFFFF
#endif
#define EPSILON 0.000001

//Defined the threshold of Dominator Frontier Density.
//Higher Dominator Frontier Density might make SSAMgr inserting
//ton of PHIs which will blow up memory.
#define THRESHOLD_HIGH_DOMINATOR_FRONTIER_DENSITY 1000

#define REG_UNDEF ((USHORT)-1) //Reserved undefined physical register id
#endif
