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
#include "errno.h"
#include "commoninc.h"
#include "region_deps.h"

namespace xoc {

//Find newline '\n' and return the byte offset that corresponding
//to start of 'buf'.
//Return -1 if not find newline.
//start: the byte offset to 'buf'.
static INT find_newline_pos(StrBuf const& buf, UINT buflen, UINT start)
{
    ASSERT0(buflen <= (UINT)buf.strlen());
    UINT i = start;
    while (i < buflen) {
        if (buf.buf[i] == LOGMGR_NEWLINE_CHAR) {
            return (INT)i;
        }
        i++;
    }
    return -1;
}


//Print leading '\n'.
//Return the byte offset of content that related to string buffer.
static size_t prt_leading_newline(LogMgr const* lm, StrBuf const& buf,
                                  UINT buflen, UINT start)
{
    size_t i = start;
    ASSERT0(buf.strlen() <= buflen);
    while (i < buflen) {
        if (buf.buf[i] == LOGMGR_NEWLINE_CHAR) {
            if (lm->isReplaceNewline()) {
                //Print terminate lines that are left
                //justified in DOT file.
                fprintf(lm->getFileHandler(), "\\l");
            } else {
                fprintf(lm->getFileHandler(), "\n");
            }
        } else {
            break;
        }
        i++;
    }
    return i;
}


static void prt_indent(LogMgr const* lm)
{
    if (lm->getFileHandler() == nullptr) { return; }
    UINT indent = lm->getIndent();
    for (; indent > 0; indent--) {
        fprintf(lm->getFileHandler(), "%c", lm->getIndentChar());
    }
    fflush(lm->getFileHandler());
}


static void note_helper(LogMgr const* lm, CHAR const* format, va_list args)
{
    ASSERT0(lm);
    FILE * h = lm->getFileHandler();
    if (h == nullptr || format == nullptr) { return; }

    va_list targs;
    va_copy(targs, args);
    StrBuf buf(64);
    buf.vstrcat(format, targs);
    va_end(targs);

    UINT const buflen = (UINT)buf.strlen();
    //Print leading \n.
    UINT cont_pos = (UINT)prt_leading_newline(lm, buf, buflen, 0);
    //Append indent chars ahead of string.
    ASSERT0(lm->getIndent() >= 0);
    prt_indent(lm);

    INT newline_pos = -1; //record the begin of next newline.
    do {
        //Find next newline char.
        newline_pos = find_newline_pos(buf, buflen, cont_pos);
        if (newline_pos != -1) {
            //The characters between cont_pos and newline_pos
            //is the print content.
            buf.buf[newline_pos] = 0;
        }
        if (cont_pos != buflen) {
            ASSERT0(cont_pos < buflen);
            fprintf(h, "%s", buf.buf + cont_pos);
        } else {
            break;
        }
        if (newline_pos != -1) {
            //Recovery newline char.
            buf.buf[newline_pos] = LOGMGR_NEWLINE_CHAR;

            //Print leading \n.
            cont_pos = (UINT)prt_leading_newline(lm, buf, buflen, newline_pos);
            //Append indent chars ahead of string.
            ASSERT0(lm->getIndent() >= 0);
            prt_indent(lm);
        }
    } while (newline_pos != -1);

    fflush(h);
}


//Print string with indent chars.
static void prt_helper(LogMgr const* lm, CHAR const* format, va_list args)
{
    ASSERT0(lm);
    FILE * h = lm->getFileHandler();
    if (h == nullptr || format == nullptr) { return; }

    va_list targs;
    va_copy(targs, args);
    StrBuf buf(64);
    buf.vstrcat(format, targs);
    va_end(targs);

    UINT buflen = (UINT)buf.strlen();
    size_t i = prt_leading_newline(lm, buf, buflen, 0);
    if (i != buflen) {
        fprintf(h, "%s", buf.buf + i);
    }
    fflush(h);
}


//Helper function to dump formatted string to logfile without indent.
bool prt(Region const* rg, CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    prt_helper(rg->getLogMgr(), format, args);
    va_end(args);
    return true;
}


//Helper function to dump formatted string to logfile without indent.
bool prt(RegionMgr const* rm, CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    prt_helper(const_cast<RegionMgr*>(rm)->getLogMgr(), format, args);
    va_end(args);
    return true;
}


//Helper function to dump formatted string to logfile without indent.
bool prt(LogMgr const* lm, CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    prt_helper(lm, format, args);
    va_end(args);
    return true;
}


//Print string with indent chars.
void note(Region const* rg, CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    note_helper(rg->getLogMgr(), format, args);
    va_end(args);
}


//Print string with indent chars.
void note(RegionMgr const* rm, CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    note_helper(const_cast<RegionMgr*>(rm)->getLogMgr(), format, args);
    va_end(args);
}


//Print string with indent chars.
//lm: Log manager
void note(LogMgr const* lm, CHAR const* format, ...)
{
    ASSERT0(lm);
    va_list args;
    va_start(args, format);
    note_helper(lm, format, args);
    va_end(args);
}


//Print string with indent chars.
//h: file handler.
void note(FILE * h, UINT indent, CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    LogMgr lm;
    lm.push(h, "");
    lm.incIndent(indent);
    note_helper(&lm, format, args);
    lm.pop();
    va_end(args);
}


void prtIndent(Region const* rg, UINT indent)
{
    prt_indent(rg->getLogMgr());
}


//START LogMgr
void LogMgr::init(CHAR const* logfilename, bool is_del)
{
    if (m_ctx.logfile != nullptr) { return; }
    if (logfilename == nullptr) { return; }
    if (is_del) {
        UNLINK(logfilename);
    }
    m_ctx.logfile = fopen(logfilename, "a+");
    if (m_ctx.logfile == nullptr) {
        fprintf(stderr,
                "\ncan not open dump file %s, errno:%d, errstring:\'%s\'\n",
                logfilename, errno, strerror(errno));
    }
}


//Finalize log file.
void LogMgr::fini()
{
    if (m_ctx.logfile != nullptr) {
        fclose(m_ctx.logfile);
        m_ctx.clean();
    }
}


//Save current log file handler and name into stack, and set given handler
//and filename as current.
//h: file handler
//filename: file name of h
void LogMgr::push(FILE * h, CHAR const* filename)
{
    LogCtx ctx;
    ctx.logfile = h;
    ctx.logfile_name = filename;
    push(ctx);
}


//Save current log file handler and name into stack, and set given handler
//and filename as current.
void LogMgr::push(LogCtx const& ctx)
{
    m_ctx_stack.push(m_ctx);
    m_ctx = ctx;
}


//Restore file handler and filename that is in top of stack.
void LogMgr::pop()
{
    m_ctx = m_ctx_stack.pop();
}
//END LogMgr

} //namespace xoc
