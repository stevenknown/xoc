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
#ifndef _LOG_MGR_H_
#define _LOG_MGR_H_

namespace xoc {

class Region;
class RegionMgr;
class LogMgr;

#define LOGMGR_INDENT_CHAR ' '
#define LOGMGR_NEWLINE_CHAR '\n'
#define DUMP_INDENT_NUM 4
#define LOGCTX_DEFAULT_BUFFER_SIZE 4 //bytes

//This class permits copy-constructing operations.
class LogCtx {
public:
    BYTE replace_newline:1;
    BYTE prt_srcline:1;
    BYTE enable_buffer:1; //record true if current ctx enabled buffer.
    BYTE paused_buffer:1; //record true if current ctx paused buffer.
    CHAR indent_char;
    INT indent;
    FILE * logfile;
    CHAR const* logfile_name;
    StrBuf * buffer;
public:
    LogCtx()
        : replace_newline(false),
          prt_srcline(true),
          enable_buffer(false),
          paused_buffer(false),
          indent_char(LOGMGR_INDENT_CHAR),
          indent(0),
          logfile(nullptr),
          logfile_name(nullptr),
          buffer(nullptr) {}
    LogCtx(UINT) : buffer(nullptr)
    { clean(); } //Used as UNDEFINED value in Stack<T>
    LogCtx(bool p_replace_newline,
           bool p_prt_srcline,
           bool p_enable_buffer,
           bool p_paused_buffer,
           CHAR p_indent_char,
           INT p_indent,
           FILE * p_logfile,
           CHAR const* p_logfile_name)
    {
        replace_newline = p_replace_newline;
        prt_srcline = p_prt_srcline;
        enable_buffer = p_enable_buffer;
        paused_buffer = p_paused_buffer;
        indent_char = p_indent_char;
        indent = p_indent;
        logfile = p_logfile;
        logfile_name = p_logfile_name;
        buffer = nullptr;
    }

    void clean()
    {
        replace_newline = false;
        prt_srcline = true;
        enable_buffer = false;
        paused_buffer = false;
        indent_char = LOGMGR_INDENT_CHAR;
        indent = 0;
        logfile = nullptr;
        logfile_name = nullptr;
        if (buffer != nullptr) {
            buffer->clean();
        }
    }
};


class LogMgr {
    COPY_CONSTRUCTOR(LogMgr);
protected:
    LogCtx m_ctx;
    Stack<LogCtx> m_ctx_stack;
    TTab<StrBuf*> m_buftab;
public:
    LogMgr() { init(nullptr, false); }
    LogMgr(CHAR const* logfilename, bool is_del) { init(logfilename, is_del); }
    ~LogMgr() { fini(); }

    //Clean the dump buffer.
    void cleanBuffer();

    //Decrease indent by value v.
    void decIndent(UINT v) { m_ctx.indent -= (INT)v; }
    void dumpBuffer();

    //Flush out the buffer content into file, and close the buffer.
    void endBuffer();

    //Finalize log file.
    //Note after finialization, log mgr will not dump any information.
    //LogMgr will close IO resource when finializing, user have to guarantee
    //LogMgr operate the correct IO resource when using 'push' and 'pop'.
    void fini();

    //The function flush buffer content to file, whereas clean the buffer.
    void flushBuffer();

    LogCtx const& getCurrentCtx() const { return m_ctx; }
    FILE * getFileHandler() const { return m_ctx.logfile; }
    CHAR const* getFileName() const { return m_ctx.logfile_name; }
    INT getIndent() const { return m_ctx.indent; }
    CHAR getIndentChar() const { return m_ctx.indent_char; }
    StrBuf * getBuffer() { return m_ctx.buffer; }

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

    //Return true if LogMgr enable dump buffer.
    bool isEnableBuffer() const { return m_ctx.enable_buffer; }

    //Stop dump to buffer for temporary purpose. And when buffer paused, the
    //output content will write to file.
    void pauseBuffer();

    //Save current log file handler and name into stack, and set given handler
    //and filename as current.
    //h: file handler
    //filename: file name of h
    void push(FILE * h, CHAR const* filename);
    void push(LogCtx const& ctx);

    //The function will create LogCtx according to given filename, and
    //push on the stack.
    //is_del: true to delete the same name file.
    bool pushAndCreate(CHAR const* filename, bool is_del);

    //The function will pop and discard the top object on the stack and destroy
    //the related resource.
    void popAndDestroy();

    //Restore file handler and filename that is in top of stack.
    void pop();

    //Set indent by value v.
    void setIndent(UINT v) { m_ctx.indent = (INT)v; }

    //Replace 'newline' charactor with '\l' when dumpping DOT file when
    //replace is true.
    void setReplaceNewline(bool replace) { m_ctx.replace_newline = replace; }

    //Enable and clean the dump buffer.
    void startBuffer();
    
    //Resume to dump to buffer. And when buffer resumed, the
    //output content will write to buffer.
    void resumeBuffer();
};


class DumpBufferSwitch {
    BYTE m_is_buffer_enabled_before;
    LogMgr * m_lm;
public:
    DumpBufferSwitch(LogMgr * lm)
    {
        m_lm = lm;
        m_is_buffer_enabled_before = lm->isEnableBuffer();
        if (m_is_buffer_enabled_before) { return; }
        lm->startBuffer();
    }
    ~DumpBufferSwitch() { flush(); }
    void flush()
    {
        if (m_is_buffer_enabled_before) { return; }
        m_lm->endBuffer();
    }
};


//Print string with indent chars.
void note(Region const* rg, CHAR const* format, ...);
void note_args(Region const* rg, CHAR const* format, va_list args);

//Print string with indent chars.
void note(RegionMgr const* rm, CHAR const* format, ...);

//Print string with indent chars.
//lm: Log manager
void note(LogMgr * lm, CHAR const* format, ...);

//Print string with indent chars.
//h: file handler.
void note(FILE * h, UINT indent, CHAR const* format, ...);

//Helper function to dump formatted string to logfile without indent.
bool prt(Region const* rg, CHAR const* format, ...);

//Helper function to dump formatted string to logfile without indent.
bool prt(RegionMgr const* rm, CHAR const* format, ...);

//Helper function to dump formatted string to logfile without indent.
//lm: Log manager
bool prt(LogMgr * lm, CHAR const* format, ...);

//Print indent chars.
void prtIndent(Region const* rg, UINT indent);

} //namespace xoc
#endif
