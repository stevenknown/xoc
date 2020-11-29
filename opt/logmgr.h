/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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
#ifndef _LOG_MGR_H_
#define _LOG_MGR_H_

namespace xoc {

class Region;
class RegionMgr;

#define LOGMGR_INDENT_CHAR ' '
#define LOGMGR_NEWLINE_CHAR '\n'
#define DUMP_INDENT_NUM 4

//This class permits copy-constructing operations.
class LogCtx {
public:
    BYTE replace_newline:1;
    BYTE prt_srcline:1;
    CHAR indent_char;
    INT indent;
    FILE * logfile;
    CHAR const* logfile_name;

public:
    LogCtx() : replace_newline(false), prt_srcline(true),
               indent_char(LOGMGR_INDENT_CHAR), indent(0),
               logfile(nullptr), logfile_name(nullptr) {}
    LogCtx(UINT) { clean(); } //Used as UNDEFINED value in Stack<T>
    LogCtx(bool p_replace_newline,
           bool p_prt_srcline,
           CHAR p_indent_char,
           INT p_indent,
           FILE * p_logfile,
           CHAR const* p_logfile_name)
    {
        replace_newline = p_replace_newline;
        prt_srcline = p_prt_srcline;
        indent_char = p_indent_char;
        indent = p_indent;
        logfile = p_logfile;
        logfile_name = p_logfile_name;
    }

    void clean()
    {
        replace_newline = false;
        prt_srcline = true;
        indent_char = LOGMGR_INDENT_CHAR;
        indent = 0;
        logfile = nullptr;
        logfile_name = nullptr;
    }
};


class LogMgr {
    COPY_CONSTRUCTOR(LogMgr);
protected:
    LogCtx m_ctx;
    Stack<LogCtx> m_ctx_stack;
public:
    LogMgr() { init(nullptr, false); }
    LogMgr(CHAR const* logfilename, bool is_del) { init(logfilename, is_del); }
    ~LogMgr() { fini(); }

    //Decrease indent by value v.
    void decIndent(UINT v) { m_ctx.indent -= (INT)v; }

    //Finalize log file.
    //Note after finialization, log mgr will not dump any information.
    //LogMgr will close IO resource when finializing, user have to guarantee
    //LogMgr operate the correct IO resource when using 'push' and 'pop'.
    void fini();

    FILE * getFileHandler() const { return m_ctx.logfile; }
    CHAR const* getFileName() const { return m_ctx.logfile_name; }
    INT getIndent() const { return m_ctx.indent; }
    CHAR getIndentChar() const { return m_ctx.indent_char; }
   
    //Return true if replace 'newline' charactor with '\l' when
    //dumpping DOT file. 
    bool isReplaceNewline() const { return m_ctx.replace_newline; }

    //Initialze log file.
    //logfilename: the file name of log file.
    //is_del: set to true if mananger expects to remove the file with same name.
    void init(CHAR const* logfilename, bool is_del);

    //Increase indent by value v.
    void incIndent(UINT v) { m_ctx.indent += (INT)v; }

    //Return true if LogMgr has initialized.
    bool is_init() const { return m_ctx.logfile != nullptr; }

    //Save current log file handler and name into stack, and set given handler
    //and filename as current.
    void push(FILE * h, CHAR const* filename);
    void push(LogCtx const& ctx);
    //Restore file handler and filename that is in top of stack.
    void pop();

    //Set indent by value v.
    void setIndent(UINT v) { m_ctx.indent = (INT)v; }
    //Replace 'newline' charactor with '\l' when dumpping DOT file when
    //replace is true.
    void setReplaceNewline(bool replace) { m_ctx.replace_newline = replace; }
};

//Print string with indent chars.
void note(Region const* rg, CHAR const* format, ...);

//Print string with indent chars.
void note(RegionMgr const* rm, CHAR const* format, ...);

//Print string with indent chars.
//lm: Log manager
void note(LogMgr const* lm, CHAR const* format, ...);

//Print string with indent chars.
//h: file handler.
void note(FILE * h, UINT indent, CHAR const* format, ...);

//Helper function to dump formatted string to logfile without indent.
bool prt(Region const* rg, CHAR const* format, ...);

//Helper function to dump formatted string to logfile without indent.
bool prt(RegionMgr const* rm, CHAR const* format, ...);

//Helper function to dump formatted string to logfile without indent.
//lm: Log manager
bool prt(LogMgr const* lm, CHAR const* format, ...);

//Print indent chars.
void prtIndent(Region const* rg, UINT indent);

} //namespace xoc
#endif
