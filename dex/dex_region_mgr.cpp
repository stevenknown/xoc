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
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/DexProto.h"
#include "liropcode.h"
#include "lir.h"
#include "d2d_comm.h"
#include "drAlloc.h"
#include "cominc.h"
#include "comopt.h"
#include "dex.h"
#include "gra.h"
#include "dex_hook.h"
#include "dex_util.h"

//Add a variable of CLASS.
VAR * DexRegionMgr::addVarForBuiltin(CHAR const* name)
{
    SYM * sym = addToSymbolTab(name);
    return getVarMgr()->registerVar(sym, getTypeMgr()->getAny(),
        0, VAR_FUNC_DECL|VAR_GLOBAL);
}


void DexRegionMgr::initBuiltin()
{
    for (UINT i = BLTIN_UNDEF + 1; i < BLTIN_LAST; i++) {
        VAR * v = addVarForBuiltin(BLTIN_name((BLTIN_TYPE)i));
        m_var2blt.set(v, i);
        m_blt2var.set(i, v);
        getSym2Var().set(addToSymbolTab(BLTIN_name((BLTIN_TYPE)i)), v);
    }
}


void DexRegionMgr::addBuiltinVarToTab()
{
    ASSERT0(getProgramRegion());
    for (UINT i = BLTIN_UNDEF + 1; i < BLTIN_LAST; i++) {
        VAR * v = getSym2Var().get(addToSymbolTab(BLTIN_name((BLTIN_TYPE)i)));
        ASSERT0(v);
        getProgramRegion()->addToVarTab(v);
    }
}


bool DexRegionMgr::processProgramRegion(Region * program, OptCtx * oc)
{
    ASSERT0(program && program->is_program());

    //Function region has been handled. And call list should be available.
    buildCallGraph(*oc, false, false);
    ASSERT0(OC_is_callg_valid(*oc));


    return true;
}
