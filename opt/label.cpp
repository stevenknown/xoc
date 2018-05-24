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

LabelInfo * allocCustomerLabel(SYM const* st, SMemPool * pool)
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


void dumpLabel(LabelInfo const* li)
{
    if (g_tfile == NULL) return;
    if (LABEL_INFO_type(li) == L_ILABEL) {
        fprintf(g_tfile, "\nilabel(" ILABEL_STR_FORMAT ")",
                ILABEL_CONT(li));
    } else if (LABEL_INFO_type(li) == L_CLABEL) {
        fprintf(g_tfile, "\nclabel(" CLABEL_STR_FORMAT ")",
                CLABEL_CONT(li));
    } else if (LABEL_INFO_type(li) == L_PRAGMA) {
        ASSERT0(LABEL_INFO_pragma(li));
        fprintf(g_tfile, "\npragms(%s)",
                SYM_name(LABEL_INFO_pragma(li)));
    } else { UNREACHABLE(); }

    if (LABEL_INFO_b1(li) != 0) {
        fprintf(g_tfile, "(");
    }
    if (LABEL_INFO_is_try_start(li)) {
        fprintf(g_tfile, "try_start ");
    }
    if (LABEL_INFO_is_try_end(li)) {
        fprintf(g_tfile, "try_end ");
    }
    if (LABEL_INFO_is_catch_start(li)) {
        fprintf(g_tfile, "catch_start ");
    }
    if (LABEL_INFO_b1(li) != 0) {
        fprintf(g_tfile, ")");
    }
    fflush(g_tfile);
}

} //namespace xoc
