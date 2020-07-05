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
#include "../com/xcominc.h"
#include "commoninc.h"
#include "symtab.h"
#include "label.h"

namespace xoc {

LabelInfo * allocPragmaLabel(Sym const* st, SMemPool * pool)
{
    LabelInfo * li = allocLabel(pool);
    LABEL_INFO_name(li) = st;
    LABEL_INFO_type(li) = L_PRAGMA;
    return li;
}


LabelInfo * allocCustomerLabel(Sym const* st, SMemPool * pool)
{
    LabelInfo * li = allocLabel(pool);
    LABEL_INFO_name(li) = st;
    LABEL_INFO_type(li) = L_CLABEL;
    return li;
}


LabelInfo * allocInternalLabel(SMemPool * pool)
{
    LabelInfo * n = allocLabel(pool);
    LABEL_INFO_type(n) = L_ILABEL;
    return n;
}


LabelInfo * allocLabel(SMemPool * pool)
{
    LabelInfo * p = (LabelInfo*)smpoolMalloc(sizeof(LabelInfo), pool);
    ASSERT0(p);
    ::memset(p, 0, sizeof(LabelInfo));
    return p;
}

void LabelInfo::dumpName() const
{
    if (g_tfile == NULL) { return; }
    if (LABEL_INFO_type(this) == L_ILABEL) {
        prt(ILABEL_STR_FORMAT, ILABEL_CONT(this));
    } else if (LABEL_INFO_type(this) == L_CLABEL) {
        prt("%s", SYM_name(LABEL_INFO_name(this)));
    } else if (LABEL_INFO_type(this) == L_PRAGMA) {
        ASSERT0(LABEL_INFO_pragma(this));
        prt("%s", SYM_name(LABEL_INFO_pragma(this)));
    } else { UNREACHABLE(); }
}

char const* LabelInfo::getName(IN OUT StrBuf * buf) const
{
    if (g_tfile == NULL) { return NULL; }
    if (LABEL_INFO_type(this) == L_ILABEL) {
        buf->sprint(ILABEL_STR_FORMAT, ILABEL_CONT(this));
    } else if (LABEL_INFO_type(this) == L_CLABEL) {
        buf->strcat("%s", SYM_name(LABEL_INFO_name(this)));
    } else if (LABEL_INFO_type(this) == L_PRAGMA) {
        ASSERT0(LABEL_INFO_pragma(this));
        buf->strcat("%s", SYM_name(LABEL_INFO_pragma(this)));
    }
    else { UNREACHABLE(); }
    return buf->buf;
}

void LabelInfo::dump() const
{
    if (g_tfile == NULL) { return; }
    if (LABEL_INFO_type(this) == L_ILABEL) {
        note("\nilabel(" ILABEL_STR_FORMAT ")",
                ILABEL_CONT(this));
    } else if (LABEL_INFO_type(this) == L_CLABEL) {
        note("\nclabel(" CLABEL_STR_FORMAT ")",
                CLABEL_CONT(this));
    } else if (LABEL_INFO_type(this) == L_PRAGMA) {
        ASSERT0(LABEL_INFO_pragma(this));
        note("\npragms(%s)",
                SYM_name(LABEL_INFO_pragma(this)));
    } else { UNREACHABLE(); }

    if (LABEL_INFO_b1(this) != 0) {
        prt("(");
    }
    if (LABEL_INFO_is_try_start(this)) {
        prt("try_start ");
    }
    if (LABEL_INFO_is_try_end(this)) {
        prt("try_end ");
    }
    if (LABEL_INFO_is_catch_start(this)) {
        prt("catch_start ");
    }
    if (LABEL_INFO_b1(this) != 0) {
        prt(")");
    }
    fflush(g_tfile);
}

} //namespace xoc
