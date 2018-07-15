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

DbxMgr * g_dbx_mgr = NULL;

void set_lineno(IR * ir, UINT lineno, Region * rg)
{
    DbxAttachInfo * da;
    ASSERT0(rg);
    if (IR_ai(ir) == NULL) {
        IR_ai(ir) = rg->allocAIContainer();
        da = (DbxAttachInfo*)smpoolMalloc(
                        sizeof(DbxAttachInfo), rg->get_pool());
        ASSERT0(da);
        da->init();
        IR_ai(ir)->set((BaseAttachInfo*)da);
    } else {
        IR_ai(ir)->init();
        da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
        if (da == NULL) {
            da = (DbxAttachInfo*)smpoolMalloc(
                        sizeof(DbxAttachInfo), rg->get_pool());
            ASSERT0(da);
            da->init();
            ASSERT0(da);
            IR_ai(ir)->set((BaseAttachInfo*)da);
        }
    }
    DBX_lineno(&da->dbx) = lineno;
}


//Get line number in source code that corresponding to the IR.
UINT get_lineno(IR const* ir)
{
    if (IR_ai(ir) == NULL || !IR_ai(ir)->is_init()) { return 0; }
    DbxAttachInfo * da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
    if (da == NULL) { return 0; }

    return DBX_lineno(&da->dbx);
}


//Get line number in source code.
UINT get_lineno(Dbx const* dbx)
{
    return DBX_lineno(dbx);
}


//Copy dbx from 'src' to 'tgt'.
void copyDbx(IR * tgt, IR const* src, Region * rg)
{
    ASSERT0(rg);
    if (IR_ai(src) == NULL) { return; }

    DbxAttachInfo * src_da = (DbxAttachInfo*)IR_ai(src)->get(AI_DBX);
    if (IR_ai(tgt) == NULL) {
        if (src_da == NULL) { return; }
        IR_ai(tgt) = rg->allocAIContainer();
    }
    ASSERT0(IR_ai(tgt));
    if (src_da == NULL) {
        IR_ai(tgt)->clean(AI_DBX);
        return;
    }

    DbxAttachInfo * tgt_da = (DbxAttachInfo*)IR_ai(tgt)->get(AI_DBX);
    if (tgt_da == NULL) {
        tgt_da = (DbxAttachInfo*)smpoolMalloc(
                    sizeof(DbxAttachInfo), rg->get_pool());
        ASSERT0(tgt_da);
        tgt_da->init();
        IR_ai(tgt)->set((BaseAttachInfo*)tgt_da);
    }
    tgt_da->dbx.copy(src_da->dbx);
}


Dbx * get_dbx(IR const* ir)
{
    if (IR_ai(ir) == NULL) { return NULL; }
    DbxAttachInfo * da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
    if (da == NULL) { return NULL; }
    return &da->dbx;
}


//
//START DbxMgr
//
void DbxMgr::printSrcLine(IR const* ir)
{
    if (g_tfile == NULL) { return; }
    if (!ir->is_stmt()) { return; }
    Dbx * dbx = ::get_dbx(ir);
    if (dbx != NULL) {
        printSrcLine(dbx);
    }
}
//END DbxMgr

} //namespace xoc
