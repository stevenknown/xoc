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

DbxMgr * g_dbx_mgr = nullptr;

void setLineNum(IR * ir, UINT lineno, Region * rg)
{
    DbxAttachInfo * da = nullptr;
    ASSERT0(rg);
    if (IR_ai(ir) == nullptr) {
        IR_ai(ir) = rg->allocAIContainer();
        da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                          rg->get_pool());
        ASSERT0(da);
        da->init();
        IR_ai(ir)->set((BaseAttachInfo*)da, rg);
    } else {
        IR_ai(ir)->init();
        da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
        if (da == nullptr) {
            da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                              rg->get_pool());
            ASSERT0(da);
            da->init();
            ASSERT0(da);
            IR_ai(ir)->set((BaseAttachInfo*)da, rg);
        }
    }
    DBX_lineno(&da->dbx) = lineno;
}


//Get line number in source code that corresponding to the IR.
UINT getLineNum(IR const* ir)
{
    if (IR_ai(ir) == nullptr || !IR_ai(ir)->is_init()) { return 0; }
    DbxAttachInfo * da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
    if (da == nullptr) { return 0; }
    return DBX_lineno(&da->dbx);
}


//Get line number in source code.
UINT getLineNum(Dbx const* dbx)
{
    return DBX_lineno(dbx);
}


//Copy dbx from 'src' to 'tgt'.
void copyDbx(IR * tgt, IR const* src, Region * rg)
{
    ASSERT0(rg);
    if (IR_ai(src) == nullptr) {
        if (g_is_search_and_copy_dbx &&
            src->is_exp() &&
            src->getParent() != nullptr) {
            //Attempt to copy nearest debug-info.
            //IR exp's parent might be nullptr during simplification.
            copyDbx(tgt, src->getParent(), rg);
        }
        return;
    }

    DbxAttachInfo * src_da = (DbxAttachInfo*)IR_ai(src)->get(AI_DBX);
    if (IR_ai(tgt) == nullptr) {
        if (src_da == nullptr) {
            if (src->is_stmt()) {
                return;
            }
            if (g_is_search_and_copy_dbx && src->getParent() != nullptr) {
                //Attempt to copy nearest debug-info.
                //IR exp's parent might be nullptr during simplification.
                copyDbx(tgt, src->getParent(), rg);
            }
            return;
        }
        IR_ai(tgt) = rg->allocAIContainer();
    }
    ASSERT0(IR_ai(tgt));
    if (src_da == nullptr) {
        IR_ai(tgt)->clean(AI_DBX);
        return;
    }

    DbxAttachInfo * tgt_da = (DbxAttachInfo*)IR_ai(tgt)->get(AI_DBX);
    if (tgt_da == nullptr) {
        tgt_da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                              rg->get_pool());
        ASSERT0(tgt_da);
        tgt_da->init();
        IR_ai(tgt)->set((BaseAttachInfo*)tgt_da, rg);
    }
    tgt_da->dbx.copy(src_da->dbx);
}


Dbx * getDbx(IR const* ir)
{
    if (IR_ai(ir) == nullptr) { return nullptr; }
    DbxAttachInfo * da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
    if (da == nullptr) { return nullptr; }
    return &da->dbx;
}


//
//START DbxMgr
//
void DbxMgr::printSrcLine(IR const* ir, PrtCtx * ctx)
{
    ASSERT0(ir);
    if (ctx->getLogMgr() == nullptr || !ctx->getLogMgr()->is_init()) { return; }
    if (!ir->is_stmt()) { return; }
    Dbx * dbx = ::getDbx(ir);
    if (dbx != nullptr) {
        printSrcLine(dbx, ctx);
    }
}


void DbxMgr::printSrcLine(xcom::StrBuf & output, IR const* ir, PrtCtx * ctx)
{
    ASSERT0(ir);
    if (!ir->is_stmt()) { return; }
    Dbx * dbx = ::getDbx(ir);
    if (dbx != nullptr) {
        printSrcLine(output, dbx, ctx);
    }
}
//END DbxMgr

} //namespace xoc
