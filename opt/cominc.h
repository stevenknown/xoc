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
#ifndef __COMINC_H__
#define __COMINC_H__

#include "commoninc.h"
#include "data_type.h"

//Middle level included files
#include "dbg.h"
#include "var.h"
#include "md.h"
#include "pass.h"
#include "ai.h"
#include "du.h"
#include "ir.h"
#include "loop.h"
#include "cfg.h"
#include "prssainfo.h"
#include "mdssainfo.h"
#include "ir_bb.h"
#include "ir_refine.h"
#include "ir_simp.h"
#include "pass_mgr.h"
#include "ir_cfg.h"
#include "ir_high_opt.h"
#include "ir_middle_opt.h"
#include "targ_info.h"
#include "region_mgr.h"
#include "region.h"
#include "ir_du.h"
#include "ir_aa.h"
#include "ir_expr_tab.h"
#include "callg.h"

using namespace xoc;
#endif
