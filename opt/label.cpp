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
#include "region_deps.h"

namespace xoc {

LabelInfo * allocPragmaLabel(Sym const* st, SMemPool * pool)
{
    LabelInfo * li = allocLabel(pool);
    LABELINFO_name(li) = st;
    LABELINFO_type(li) = L_PRAGMA;
    return li;
}


LabelInfo * allocCustomerLabel(Sym const* st, SMemPool * pool)
{
    LabelInfo * li = allocLabel(pool);
    LABELINFO_name(li) = st;
    LABELINFO_type(li) = L_CLABEL;
    return li;
}


LabelInfo * allocInternalLabel(SMemPool * pool)
{
    LabelInfo * n = allocLabel(pool);
    LABELINFO_type(n) = L_ILABEL;
    return n;
}


LabelInfo * allocLabel(SMemPool * pool)
{
    LabelInfo * p = (LabelInfo*)smpoolMalloc(sizeof(LabelInfo), pool);
    ASSERT0(p);
    ::memset(p, 0, sizeof(LabelInfo));
    return p;
}


void LabelInfo::dumpName(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    if (LABELINFO_type(this) == L_ILABEL) {
        prt(rg, ILABEL_STR_FORMAT, ILABEL_CONT(this));
    } else if (LABELINFO_type(this) == L_CLABEL) {
        prt(rg, "%s", SYM_name(LABELINFO_name(this)));
    } else if (LABELINFO_type(this) == L_PRAGMA) {
        ASSERT0(LABELINFO_pragma(this));
        prt(rg, "%s", SYM_name(LABELINFO_pragma(this)));
    } else { UNREACHABLE(); }
}


char const* LabelInfo::getName(MOD StrBuf * buf) const
{
    if (LABELINFO_type(this) == L_ILABEL) {
        buf->sprint(ILABEL_STR_FORMAT, ILABEL_CONT(this));
    } else if (LABELINFO_type(this) == L_CLABEL) {
        buf->strcat("%s", SYM_name(LABELINFO_name(this)));
    } else if (LABELINFO_type(this) == L_PRAGMA) {
        ASSERT0(LABELINFO_pragma(this));
        buf->strcat("%s", SYM_name(LABELINFO_pragma(this)));
    }
    else { UNREACHABLE(); }
    return buf->buf;
}


void LabelInfo::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    if (LABELINFO_type(this) == L_ILABEL) {
        note(rg, "\nilabel(" ILABEL_STR_FORMAT ")", ILABEL_CONT(this));
    } else if (LABELINFO_type(this) == L_CLABEL) {
        note(rg, "\nclabel(" CLABEL_STR_FORMAT ")", CLABEL_CONT(this));
    } else if (LABELINFO_type(this) == L_PRAGMA) {
        ASSERT0(LABELINFO_pragma(this));
        note(rg, "\npragms(%s)", SYM_name(LABELINFO_pragma(this)));
    } else { UNREACHABLE(); }

    if (LABELINFO_b1(this) != 0) {
        prt(rg, "(");
    }
    if (LABELINFO_is_try_start(this)) {
        prt(rg, "try_start ");
    }
    if (LABELINFO_is_try_end(this)) {
        prt(rg, "try_end ");
    }
    if (LABELINFO_is_catch_start(this)) {
        prt(rg, "catch_start ");
    }
    if (LABELINFO_b1(this) != 0) {
        prt(rg, ")");
    }
}

} //namespace xoc
