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
#ifndef _REGION_DEPS_H_
#define _REGION_DEPS_H_

//_USE_GCC_ will be defined if host C++ compiler is gcc.
#ifdef _USE_GCC_
//Suppress gcc warning about array boundary checking. The accessing to
//IR's padding operand violates the checking.
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif

#include "data_type.h"
#include "const.h"
//Middle level included files
#include "dbg.h"
#include "var.h"
#include "md.h"
#include "pass.h"
#include "ai.h"
#include "du.h"
#include "ir.h"
#include "ir_helper.h"
#include "ir_bb.h"
#include "loop.h"
#include "pass_mgr.h"
#include "attachinfo_mgr.h"
#include "md_mgr.h"
#include "region_mgr.h"
#include "analysis_instr.h"
#include "region.h"
#endif
