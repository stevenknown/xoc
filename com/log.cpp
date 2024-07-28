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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "xcominc.h"

#define LOG_INDENT_CHAR ' '
#define LOG_NEWLINE_CHAR '\n'

namespace xcom {

//Find newline '\n' and return the byte offset that corresponding
//to start of 'buf'.
//Return -1 if not find newline.
//start: the byte offset to 'buf'.
static INT find_newline_pos(xcom::StrBuf const& buf, UINT buflen, UINT start)
{
    ASSERT0(buflen <= (UINT)buf.strlen());
    UINT i = start;
    while (i < buflen) {
        if (buf.buf[i] == LOG_NEWLINE_CHAR) {
            return (INT)i;
        }
        i++;
    }
    return -1;
}


//Print leading '\n'.
//Return the byte offset of content that related to string buffer.
static size_t prt_leading_newline(
    FILE * h, bool is_replace_newline, xcom::StrBuf const& buf, UINT buflen,
    UINT start)
{
    ASSERT0(h);
    size_t i = start;
    ASSERT0(buf.strlen() <= buflen);
    CHAR const* terminate_line_r = nullptr;
    CHAR const* terminate_line = "\n";
    if (is_replace_newline) {
        //Print terminate lines that are left
        //justified in DOT file.
        terminate_line_r = "\\l";
    } else {
        terminate_line_r = "\n";
    }
    while (i < buflen) {
        if (buf.buf[i] == LOG_NEWLINE_CHAR) {
            if (is_replace_newline) {
                //Print terminate lines that are left
                //justified in DOT file.
                fprintf(h, "%s", terminate_line_r);
            } else {
                fprintf(h, "%s", terminate_line);
            }
        } else {
            break;
        }
        i++;
    }
    return i;
}


static void prt_indent(FILE * h, UINT indent)
{
    ASSERT0(h);
    for (; indent > 0; indent--) {
        fprintf(h, "%c", LOG_INDENT_CHAR);
    }
}


static void log(FILE * h, UINT indent, xcom::StrBuf const& buf)
{
    if (h == nullptr) { return; }
    UINT const buflen = (UINT)buf.strlen();
    bool is_replace_newline = false;
    UINT cont_pos = (UINT)prt_leading_newline(
        h, is_replace_newline, buf, buflen, 0);

    //Append indent ahead of string.
    prt_indent(h, indent);
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
            buf.buf[newline_pos] = LOG_NEWLINE_CHAR;

            //Print leading \n.
            cont_pos = (UINT)prt_leading_newline(
                h, is_replace_newline, buf, buflen, newline_pos);

            //Append indent chars ahead of string.
            prt_indent(h, indent);
        }
    } while (newline_pos != -1);
    fflush(h);
}


void log(FILE * h, UINT indent, CHAR const* format, va_list args)
{
    va_list targs;
    va_copy(targs, args);
    xcom::StrBuf buf(64);
    buf.vstrcat(format, targs);
    va_end(targs);
    log(h, indent, buf);
}


//Print string with indent chars.
void log(FILE * h, UINT indent, CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    log(h, indent, format, args);
    va_end(args);
}

} //namespace xcom
