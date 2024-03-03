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
#ifndef __IR_UTILS_EXT_H__
#define __IR_UTILS_EXT_H__

//The file defined a list of utilities to better manipulate IR.

namespace xoc {

//Defined the entry for extended expression ir code.
#define SWITCH_CASE_EXT_EXP \
    case IR_BROADCAST

//Defined the entry for extended virtual stmt ir code.
#define SWITCH_CASE_EXT_VSTMT \
    SWITCH_CASE_EXT_WRITE_PR: \
    SWITCH_CASE_EXT_DIRECT_MEM_VSTMT: \
    SWITCH_CASE_EXT_INDIRECT_MEM_VSTMT

//Defined the entry for extended stmt ir code.
#define SWITCH_CASE_EXT_STMT \
    SWITCH_CASE_EXT_VSTMT

//Defined the entry for extended ir code.
#define SWITCH_CASE_EXT \
    SWITCH_CASE_EXT_STMT: \
    SWITCH_CASE_EXT_EXP

#define SWITCH_CASE_EXT_WRITE_PR \
    case IR_VSTPR

#define SWITCH_CASE_EXT_DIRECT_MEM_VSTMT \
    case IR_VST

//Defined the entry for extended UNA ir code.
#define SWITCH_CASE_EXT_UNA EMPTY_MACRO

//No extended BIN for now.
#define SWITCH_CASE_EXT_BIN EMPTY_MACRO

//In order to conform the compatibility of origin IR code, user can undef
//original SWITCH_CASE_<NAME>, then redefine the same MACRO with new IR code
//and followed by origin IR code.
#include "targ_utils_ext.h"

} //namespace xoc

#endif
