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
#ifndef _DBG_H_
#define _DBG_H_

namespace xoc {

class Region;
class DbxMgr;

//This macro defines the maximum number of frontend languages.
#define MAX_FRONTEND_LANGUAGES 4

class LangInfo {
    COPY_CONSTRUCTOR(LangInfo);
public:
    typedef enum tagLANG {
        LANG_UNDEF = 0,
        LANG_CPP,     //C++
        LANG_PYTHON,  //Python
        LANG_NUM
    } LANG;
    typedef xcom::TMap<LANG, UINT8> LANG2INDEX;
public:
    //Record the index for each language.
    LANG2INDEX m_frontend_lang_to_index;
public:
    LangInfo() { m_frontend_lang_to_index.clean(); };
    UINT8 getLangIndex(LangInfo::LANG lang);
    UINT8 getLangNum();
    void setLangIndex(LangInfo::LANG lang, UINT8 index);
};


//Describe debug information.
//Note 0 indicates the line number info is invalid.
//In future, it will be determined that if the line number is 0,
//it means that this IR does not have any debugging information.
#define DBX_UNDEF 0
#define DBX_debug_infos(d)  ((d)->m_debug_infos)
class Dbx {
    Dbx(Dbx const&);
public:
    struct DebugInfo {
        UINT32 m_col_offset;
        UINT32 m_file_index;
        UINT32 m_lineno;  //Note line number cannot be 0.

        //Currently, in the actual project,
        //only two bits are used(Low 1 bit and 3 bit),
        //They are all common terms in compilers.
        //described as follows:
        //Low 1 bit: Flag for stmt.
        //Low 2 bit: Flag for basic block.
        //Low 3 bit: Flag for prologue_end.
        //Low 4 bit: Flag for epilogue_begin.
        //Below are the explanations for each name:
        //Low 1 bit: stmt represents whether it is a line,
        //e.g: .loc 1 55 66 stmt 1, .loc 1 56 88 stmt 1, .loc 1 56 99 stmt 0.
        //It can be observed that the first command is on line 55,
        //with stmt set to 1. The second one is on line 56,
        //with stmt also set to 1. The third one is still on line 56,
        // therefore stmt is set to 0.
        //Low 2 bit: In the current context, the 'loc' instruction
        //comes with a BB attribute but we haven't encountered it yet.
        //Low 3 bit: .loc signifies the end of a stack frame.
        //Low 4 bit: .loc signifies the start of a stack frame.
        UINT8 m_flag;
    };
    DebugInfo * m_debug_infos;
public:
    //The dbxMgr of each region is responsible for allocating memory
    //for the m_debug_infos in dbx, so each dbx must
    //be born and die with the region.
    Dbx() { m_debug_infos = nullptr; }
    ~Dbx() {}
    void clean(DbxMgr * dbx_mgr);
    void copy(Dbx const& dbx, DbxMgr * dbx_mgr);
    UINT32 getLine(LangInfo::LANG lang, DbxMgr * dbx_mgr) const;
    UINT32 getColOffset(LangInfo::LANG lang, DbxMgr * dbx_mgr) const;
    UINT32 getFileIndex(LangInfo::LANG lang, DbxMgr * dbx_mgr) const;
    UINT8 getFlag(LangInfo::LANG lang, DbxMgr * dbx_mgr) const;

    //During initialization, we will allocate memory for the m_debug_infos.
    //Note that we use the pool of dbgMgr.
    void init(DbxMgr * dbx_mgr);
    void setLine(LangInfo::LANG lang, UINT32 line, DbxMgr * dbx_mgr);
    void setColOffset(LangInfo::LANG lang, UINT32 col_offset,
                      DbxMgr * dbx_mgr);
    void setFileIndex(LangInfo::LANG lang, UINT32 file_index,
                      DbxMgr * dbx_mgr);
    void setFlag(LangInfo::LANG lang, UINT8 flag, DbxMgr * dbx_mgr);
};


#define DBXMGR_lang_info(d)  ((d)->m_lang_info)
class DbxMgr {
    COPY_CONSTRUCTOR(DbxMgr);
    SMemPool * m_pool;
public:
    #define PRTCTX_lang(p)  ((p)->m_lang)
    class PrtCtx {
    public:
        //Record the string will be prefix of dbx information.
        char const* prefix;

        //Record the IO handler.
        LogMgr * logmgr;

        //Required language.
        xoc::LangInfo::LANG m_lang;
    public:
        PrtCtx(xoc::LangInfo::LANG lang)
        {
            prefix = nullptr;
            logmgr = nullptr;
            m_lang = lang;
        }

        LogMgr * getLogMgr() const { return logmgr; }
    };
    LangInfo m_lang_info;
public:
    DbxMgr()
    {
        m_pool = nullptr;
        init();
    }
    virtual ~DbxMgr() { destroy(); }

    //Allocate memory for dbx based on the number of front-end languages.
    Dbx::DebugInfo * allocDbxInfos();

    //Allocate memory for dbx.
    Dbx * allocDbx();

    void destroy();

    //Do some prepare work before print source file.
    virtual void doPrepareWorkBeforePrint() {}
    void init();

    //Print source code line in internal stream.
    virtual void printSrcLine(IR const*, PrtCtx * ctx);

    //Print source code line in internal stream.
    virtual void printSrcLine(StrBuf &, IR const*, PrtCtx * ctx);

    //Print source code line in internal stream.
    virtual void printSrcLine(Dbx const*, PrtCtx * ctx)
    {
        //Taget Dependent Code
        //Nothing to do.
    }

    //Print source code line in StrBuf.
    virtual void printSrcLine(StrBuf &, Dbx const*, PrtCtx * ctx)
    {
        //Taget Dependent Code
        //Nothing to do.
    }

    void setLangInfo();
    void * xmalloc(size_t size);
};


//Copy Dbx information from 'src' to 'tgt'.
void copyDbx(IR * tgt, IR const* src, Region * rg);

//Copy dbx from 'src' to each element in 'tgt_list'.
void copyDbxForList(IR * tgt_list, IR const* src, Region * rg);

//Get source file line number of 'ir'.
UINT getLineNum(IR const* ir, LangInfo::LANG language, DbxMgr * dbx_mgr);

//Get source file line number in 'dbx'.
UINT getLineNum(Dbx const* dbx, LangInfo::LANG language, DbxMgr * dbx_mgr);

//Return the dbx of 'ir'.
Dbx * getDbx(IR const* ir);

//Set source file line number to 'ir'.
void setLineNum(IR * ir, UINT lineno, Region * rg,
                LangInfo::LANG language = LangInfo::LANG_CPP);

//Set source file loc to 'ir'.
void setLoc(IR * ir, Region * rg, UINT line, UINT col,
            UINT file_index, LangInfo::LANG language);

//Set dbx to 'ir'.
void setDbx(IR * ir, Dbx * dbx, Region * rg);

} //namespace xoc
#endif
