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
#include "cominc.h"

namespace xoc {

bool DUSet::verify_def(IR_DU_MGR * du) const
{
    CK_USE(du);
    DU_ITER di = NULL;
    for (UINT d = get_first(&di);
         di != NULL; d = get_next(d, &di)) {
        ASSERT0(du->get_ir(d)->is_stmt());
    }
    return true;
}


bool DUSet::verify_use(IR_DU_MGR * du) const
{
    CK_USE(du);
    DU_ITER di = NULL;
    for (UINT u = get_first(&di);
         di != NULL; u = get_next(u, &di)) {
        ASSERT0(du->get_ir(u)->is_exp());
    }
    return true;
}


//Add define stmt with check if the stmt is unique in list.
void DUSet::add_use(IR const* exp, DefMiscBitSetMgr & m)
{
    ASSERT0(exp && exp->is_exp());
    bunion(IR_id(exp), m);
}


//Add define stmt with check if the stmt is unique in list.
void DUSet::add_def(IR const* stmt, DefMiscBitSetMgr & m)
{
    ASSERT0(stmt && stmt->is_stmt());
    bunion(IR_id(stmt), m);
}


void DUSet::remove_use(IR const* exp, DefMiscBitSetMgr & m)
{
    ASSERT0(exp && exp->is_exp());
    diff(IR_id(exp), m);
}


void DUSet::removeDef(IR const* stmt, DefMiscBitSetMgr & m)
{
    ASSERT0(stmt && stmt->is_stmt());
    diff(IR_id(stmt), m);
}

} //namespace xoc
