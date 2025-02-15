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

void setLineNum(IR * ir, UINT lineno, Region * rg, LangInfo::LANG language)
{
    ASSERT0(ir && rg);
    DbxAttachInfo * da = nullptr;
    ASSERT0(rg);
    DbxMgr * dbx_mgr = rg->getDbxMgr();
    ASSERT0(dbx_mgr);
    if (IR_ai(ir) == nullptr) {
        IR_ai(ir) = rg->allocAIContainer();
        da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                          rg->getCommPool());
        ASSERT0(da);

        //This is the memory allocation for the various attributes of dbx,
        //which is dbx's own memory allocator.
        da->init(dbx_mgr);
        IR_ai(ir)->set((BaseAttachInfo*)da, rg);
    } else {
        IR_ai(ir)->init();
        da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
        if (da == nullptr) {
            da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                              rg->getCommPool());
            ASSERT0(da);

            //This is the memory allocation for the various attributes of dbx,
            //which is dbx's own memory allocator.
            DBX_debug_infos(&da->dbx) = dbx_mgr->allocDbxInfos();
            da->init(dbx_mgr);
            IR_ai(ir)->set((BaseAttachInfo*)da, rg);
        }
    }
    da->dbx.setLine(language, lineno, dbx_mgr);
}


void setLoc(IR * ir, Region * rg, UINT lineno, UINT col,
            UINT file_index, LangInfo::LANG language)
{
    DbxAttachInfo * da = nullptr;
    ASSERT0(rg && ir);
    DbxMgr * dbx_mgr = rg->getDbxMgr();
    ASSERT0(dbx_mgr);
    if (IR_ai(ir) == nullptr) {
        IR_ai(ir) = rg->allocAIContainer();
        da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                          rg->getCommPool());
        ASSERT0(da);

        //This is the memory allocation for the various attributes of dbx,
        //which is dbx's own memory allocator.
        da->init(dbx_mgr);
        IR_ai(ir)->set((BaseAttachInfo*)da, rg);
    } else {
        IR_ai(ir)->init();
        da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
        if (da == nullptr) {
            da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                              rg->getCommPool());
            ASSERT0(da);

            //This is the memory allocation for the various attributes of dbx,
            //which is dbx's own memory allocator.
            DBX_debug_infos(&da->dbx) = dbx_mgr->allocDbxInfos();
            da->init(dbx_mgr);
            IR_ai(ir)->set((BaseAttachInfo*)da, rg);
        }
    }
    ASSERT0(file_index < xcom::
        computeMaxValueFromByteWidth<UINT>(sizeof(UINT)));
    da->dbx.setLine(language, lineno, dbx_mgr);
    da->dbx.setColOffset(language, col, dbx_mgr);
    da->dbx.setFileIndex(language, file_index, dbx_mgr);
}


//Get line number in source code that corresponding to the IR.
UINT getLineNum(IR const* ir, LangInfo::LANG language, DbxMgr * dbx_mgr)
{
    if (IR_ai(ir) == nullptr || !IR_ai(ir)->is_init()) { return 0; }
    DbxAttachInfo * da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
    if (da == nullptr) { return 0; }
    return da->dbx.getLine(language, dbx_mgr);
}


//Get line number in source code.
UINT getLineNum(Dbx const* dbx, LangInfo::LANG language, DbxMgr * dbx_mgr)
{
    return dbx->getLine(language, dbx_mgr);
}


//Copy dbx from 'src' to each element in 'tgt_list'.
void copyDbxForList(IR * tgt_list, IR const* src, Region * rg)
{
    for (IR * tgt = tgt_list; tgt != nullptr; tgt = tgt->get_next()) {
        copyDbx(tgt, src, rg);
    }
}


//Set dbx to 'ir'.
void setDbx(IR * ir, Dbx * dbx, Region * rg)
{
    ASSERT0(ir && dbx && rg);
    DbxMgr * dbx_mgr = rg->getDbxMgr();
    ASSERT0(dbx_mgr);
    if (ir->getAI() == nullptr) {
        IR_ai(ir) = rg->allocAIContainer();
    } else {
        IR_ai(ir)->init();
    }
    ASSERT0(IR_ai(ir));
    DbxAttachInfo * ir_da = nullptr;
    if (ir->getAI()->is_init()) {
        ir_da = (DbxAttachInfo*)ir->getAI()->get(AI_DBX);
    }
    if (ir_da == nullptr) {
        ir_da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                              rg->getCommPool());
        ASSERT0(ir_da);

        //This is the memory allocation for the various attributes of dbx,
        //which is dbx's own memory allocator.
        ir_da->init(dbx_mgr);
        IR_ai(ir)->set((BaseAttachInfo*)ir_da, rg);
    }
    ir_da->dbx.copy(*dbx, dbx_mgr);
}


//Copy dbx from 'src' to 'tgt'.
void copyDbx(IR * tgt, IR const* src, Region * rg)
{
    ASSERT0(tgt && src && rg);
    DbxMgr * dbx_mgr = rg->getDbxMgr();
    ASSERT0(dbx_mgr);
    if (src->getAI() == nullptr) {
        if (g_is_search_and_copy_dbx && src->is_exp() &&
            src->getParent() != nullptr) {
            //Attempt to copy nearest debug-info.
            //IR exp's parent might be nullptr during simplification.
            copyDbx(tgt, src->getParent(), rg);
        }
        return;
    }

    DbxAttachInfo * src_da = (DbxAttachInfo*)src->getAI()->get(AI_DBX);
    if (tgt->getAI() == nullptr) {
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
    } else {
        IR_ai(tgt)->init();
    }
    ASSERT0(IR_ai(tgt));
    if (src_da == nullptr) {
        IR_ai(tgt)->clean(AI_DBX);
        return;
    }

    DbxAttachInfo * tgt_da = nullptr;
    if (tgt->getAI()->is_init()) {
        tgt_da = (DbxAttachInfo*)tgt->getAI()->get(AI_DBX);
    }
    if (tgt_da == nullptr) {
        tgt_da = (DbxAttachInfo*)smpoolMalloc(sizeof(DbxAttachInfo),
                                              rg->getCommPool());
        ASSERT0(tgt_da);

        //This is the memory allocation for the various attributes of dbx,
        //which is dbx's own memory allocator.
        tgt_da->init(dbx_mgr);
        IR_ai(tgt)->set((BaseAttachInfo*)tgt_da, rg);
    }
    tgt_da->dbx.copy(src_da->dbx, dbx_mgr);
}


Dbx * getDbx(IR const* ir)
{
    if (IR_ai(ir) == nullptr || !IR_ai(ir)->is_init()) { return nullptr; }
    DbxAttachInfo * da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
    if (da == nullptr) { return nullptr; }
    return &da->dbx;
}


void Dbx::init(DbxMgr * dbx_mgr)
{
    ASSERT0(dbx_mgr);
    DBX_debug_infos(this) = dbx_mgr->allocDbxInfos();
}


void Dbx::clean(DbxMgr * dbx_mgr)
{
    ASSERT0(dbx_mgr && m_debug_infos);
    ASSERT0(DBXMGR_lang_info(dbx_mgr).getLangNum() < MAX_FRONTEND_LANGUAGES);
    ::memset(m_debug_infos, 0, sizeof(DebugInfo) *
        DBXMGR_lang_info(dbx_mgr).getLangNum());
}


void Dbx::copy(Dbx const& dbx, DbxMgr * dbx_mgr)
{
    ASSERT0(dbx.m_debug_infos && this->m_debug_infos && dbx_mgr);
    ASSERT0(DBXMGR_lang_info(dbx_mgr).getLangNum() < MAX_FRONTEND_LANGUAGES);
    ::memcpy((*this).m_debug_infos, dbx.m_debug_infos, (sizeof(DebugInfo) *
        DBXMGR_lang_info(dbx_mgr).getLangNum()));
}


UINT32 Dbx::getLine(LangInfo::LANG lang, DbxMgr * dbx_mgr) const
{
    ASSERT0(dbx_mgr);
    return m_debug_infos[DBXMGR_lang_info(dbx_mgr).
        getLangIndex(lang)].m_lineno;
}


UINT32 Dbx::getColOffset(LangInfo::LANG lang, DbxMgr * dbx_mgr) const
{
    ASSERT0(dbx_mgr);
    return m_debug_infos[DBXMGR_lang_info(dbx_mgr).
        getLangIndex(lang)].m_col_offset;
}


UINT32 Dbx::getFileIndex(LangInfo::LANG lang, DbxMgr * dbx_mgr) const
{
    ASSERT0(dbx_mgr);
    return m_debug_infos[DBXMGR_lang_info(dbx_mgr).
        getLangIndex(lang)].m_file_index;
}


UINT8 Dbx::getFlag(LangInfo::LANG lang, DbxMgr * dbx_mgr) const
{
    ASSERT0(dbx_mgr);
    return m_debug_infos[DBXMGR_lang_info(dbx_mgr).
        getLangIndex(lang)].m_flag;
}


void Dbx::setLine(LangInfo::LANG lang, UINT32 line, DbxMgr * dbx_mgr)
{
    ASSERT0(dbx_mgr);
    m_debug_infos[DBXMGR_lang_info(dbx_mgr).
        getLangIndex(lang)].m_lineno = line;
}


void Dbx::setColOffset(LangInfo::LANG lang, UINT32 col_offset,
                       DbxMgr * dbx_mgr)
{
    ASSERT0(dbx_mgr);
    m_debug_infos[DBXMGR_lang_info(dbx_mgr).getLangIndex(lang)].
        m_col_offset = col_offset;
}


void Dbx::setFileIndex(LangInfo::LANG lang, UINT32 file_index,
                       DbxMgr * dbx_mgr)
{
    ASSERT0(dbx_mgr);
    m_debug_infos[DBXMGR_lang_info(dbx_mgr).getLangIndex(lang)].
        m_file_index = file_index;
}


void Dbx::setFlag(LangInfo::LANG lang, UINT8 flag, DbxMgr * dbx_mgr)
{
    ASSERT0(dbx_mgr);
    m_debug_infos[DBXMGR_lang_info(dbx_mgr).
        getLangIndex(lang)].m_flag = flag;
}

//
//START LangInfo
//
void LangInfo::setLangIndex(LangInfo::LANG lang, UINT8 index)
{
    ASSERT0(!m_frontend_lang_to_index.find(lang));
    m_frontend_lang_to_index.set(lang, index);
}


UINT8 LangInfo::getLangIndex(LangInfo::LANG lang) const
{
    ASSERT0(m_frontend_lang_to_index.find(lang));
    return m_frontend_lang_to_index.get(lang);
}


UINT8 LangInfo::getLangNum() const
{
    return m_frontend_lang_to_index.get_elem_count();
}

//
//START DbxMgr
//
void DbxMgr::init()
{
    if (m_pool == nullptr) {
        m_pool = smpoolCreate(64, MEM_COMM);
    }
    ASSERT0(m_pool);
}


void DbxMgr::destroy()
{
    if (m_pool != nullptr) {
        smpoolDelete(m_pool);
        m_pool = nullptr;
    }

    Lang2FileIdx2FileNameIter iter;
    FileIdx2FileName * file_vec;
    for (m_lang2fi2fn.get_first(iter, &file_vec); !iter.end();
         m_lang2fi2fn.get_next(iter,&file_vec)) {
        if (file_vec == nullptr) { continue; }
        delete file_vec;
    }
}


void * DbxMgr::xmalloc(size_t size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset(p, 0, size);
    return p;
}


Dbx::DebugInfo * DbxMgr::allocDbxInfos()
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    Dbx::DebugInfo * dbx_infos = (Dbx::DebugInfo*)(this->
        xmalloc(sizeof(Dbx::DebugInfo) *
        DbxMgr::m_lang_info.getLangNum()));
    ASSERT0(dbx_infos);
    return dbx_infos;
}


Dbx * DbxMgr::allocDbx()
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    Dbx * dbx_p = (Dbx*)(this->xmalloc(sizeof(Dbx)));
    ASSERT0(dbx_p);
    DBX_debug_infos(dbx_p) = allocDbxInfos();
    return dbx_p;
}


FileIdx2FileName * DbxMgr::allocFileVec()
{
    return new FileIdx2FileName();
}


void DbxMgr::setLangInfo()
{
    UINT8 index_lang_num = 0;
    if (g_debug_cpp) {
        m_lang_info.setLangIndex(LangInfo::LANG_CPP, index_lang_num++);
    }
    if (g_debug_pyhton) {
        m_lang_info.setLangIndex(LangInfo::LANG_PYTHON, index_lang_num++);
    }
}


void DbxMgr::setFileName(LangInfo::LANG lang, UINT fileidx, xoc::Sym const* s)
{
    ASSERT0(s);
    ASSERTN(fileidx < MAX_FILE_INDEX,
            ("m_fi2fn may be better when using TMap compared to using"
                " Vector if fileindex is sparse"));
    ASSERTN(lang < MAX_FRONTEND_LANGUAGES,
            ("The number of supported languages has been exceeded."));
    FileIdx2FileName * file2_file_name = m_lang2fi2fn.get(lang);
    if (file2_file_name == nullptr) {
        file2_file_name = allocFileVec();
        m_lang2fi2fn.set(lang, file2_file_name);
    }
    ASSERT0(file2_file_name);
    file2_file_name->set(fileidx, s);
}


xoc::Sym const* DbxMgr::getFileName(LangInfo::LANG lang, UINT fileidx) const
{
    ASSERTN(m_lang_info.getLangNum() < MAX_FRONTEND_LANGUAGES,
            ("The number of supported languages has been exceeded."));
    ASSERTN(fileidx < MAX_FILE_INDEX,
            ("m_fi2fn may be better when using TMap compared to using"
                " Vector if fileindex is sparse"));
    ASSERT0(m_lang2fi2fn.get(lang));
    return m_lang2fi2fn.get(lang)->get(fileidx);
}


void DbxMgr::printSrcLine(IR const* ir, PrtCtx * ctx)
{
    ASSERT0(ir);
    if (ctx->getLogMgr() == nullptr || !ctx->getLogMgr()->is_init() ||
        !ir->is_stmt()) {
        return;
    }
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
