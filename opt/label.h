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
#ifndef __LABEL_H__
#define __LABEL_H__

namespace xoc {

typedef enum {
    L_UNDEF = 0,
    L_CLABEL, //customer defined label
    L_ILABEL, //internal generated label by compiler
    L_PRAGMA, //pragma
} LABEL_TYPE;

#define PREFIX_OF_ILABEL() "_$L"
#define POSTFIX_OF_ILABEL() ""
#define PREFIX_OF_CLABEL() ""
#define POSTFIX_OF_CLABEL() ""
#define PREFIX_OF_PRAGMA() "_PRAGMA"
#define PREFIX_OF_ILABEL_IN_GR "_GR_"

#define ILABEL_STR_FORMAT  "%s%d%s" //prefix label-num postfix
#define ILABEL_CONT(li) \
    PREFIX_OF_ILABEL(),LABELINFO_num(li),POSTFIX_OF_ILABEL()

#define CLABEL_STR_FORMAT  "%s%s%s" //prefix label-name postfix
#define CLABEL_CONT(li) \
    PREFIX_OF_CLABEL(), SYM_name(LABELINFO_name(li)), POSTFIX_OF_CLABEL()

#define PRAGMA_STR_FORMAT  "%s%s%s" //prefix label-name postfix
#define PRAGMA_CONT(li) \
    PREFIX_OF_PRAGMA(), SYM_name(LABELINFO_name(li)), POSTFIX_OF_CLABEL()

#define LABELINFO_type(l) ((l)->ltype)
#define LABELINFO_name(l) ((l)->u1.lab_name)
#define LABELINFO_num(l) ((l)->u1.lab_num)
#define LABELINFO_pragma(l) ((l)->u1.pragma_str)
#define LABELINFO_is_catch_start(l) ((l)->u2.s1.is_catch_start)
#define LABELINFO_is_try_start(l) ((l)->u2.s1.is_try_start)
#define LABELINFO_is_try_end(l) ((l)->u2.s1.is_try_end)
#define LABELINFO_is_terminate(l) ((l)->u2.s1.is_terminate)
#define LABELINFO_is_pragma(l) (LABELINFO_type(l) == L_PRAGMA)
#define LABELINFO_b1(l) ((l)->u2.b1)
class LabelInfo {
    COPY_CONSTRUCTOR(LabelInfo);
public:
    LABEL_TYPE ltype;
    union {
        Sym const* lab_name;
        Sym const* pragma_str;
        UINT lab_num;
    } u1;

    union {
        struct {
            //Set true if current label is the start
            //label of exception catch block.
            BYTE is_catch_start:1;

            //Set true if current label is the start
            //label of exception try block.
            BYTE is_try_start:1;

            //Set true if current label is the end
            //label of exception try block.
            BYTE is_try_end:1;

            //Set true if current label is a placeholer to indicate that
            //program control flow is terminate here.
            BYTE is_terminate:1;
        } s1;
        BYTE b1;
    } u2;
public:
    LabelInfo() {}
    void copy(LabelInfo const& li)
    {
        ltype = li.ltype;
        u1.lab_name = li.u1.lab_name;
        copy_flag(li);
    }
    void copy_flag(LabelInfo const& li) { u2.b1 = li.u2.b1; }

    void dump(Region const* rg) const;
    void dumpName(Region const* rg) const;

    //Format and return label name which is usually used to dump.
    CHAR const* getName(MOD StrBuf * buf) const;

    //Return the original label name without pretty formatting.
    Sym const* getOrgName() const { return LABELINFO_name(this); }

    //Return the label number if current label is ILABEL.
    UINT getNum() const { return LABELINFO_num(this); }

    //Return current label type.
    LABEL_TYPE getType() const { return LABELINFO_type(this); }

    //Return pragma string if current label is pragma.
    Sym const* getPragma() const { return LABELINFO_pragma(this); }

    //Return true if label can not be removed.
    bool hasSideEffect() const
    {
        return is_catch_start() || is_try_start() || is_try_end() ||
               is_pragma() || is_terminate();
    }

    bool is_clabel() const { return getType() == L_CLABEL; }
    bool is_ilabel() const { return getType() == L_ILABEL; }
    bool is_pragma() const { return getType() == L_PRAGMA; }
    bool is_catch_start() const { return LABELINFO_is_catch_start(this); }
    bool is_try_start() const { return LABELINFO_is_try_start(this); }
    bool is_try_end() const { return LABELINFO_is_try_end(this); }
    bool is_terminate() const { return LABELINFO_is_terminate(this); }
};


//Exported Functions
//Simplest method to compute hash value.
inline UINT computeLabelHashValue(LabelInfo const* li)
{
    INT v = 0;
    if (LABELINFO_type(li) == L_CLABEL) {
        CHAR const* p = li->getOrgName()->getStr();
        while (*p != 0) {
            v += *p;
            p++;
        }
    } else {
        ASSERT0(LABELINFO_type(li) == L_ILABEL);
        v = (INT)LABELINFO_num(li);
        v = (INT)((((UINT)(-v)) >> 7) ^ 0xAC5AAC5A);
    }
    return (UINT)v;
}


LabelInfo * allocLabel(SMemPool * pool);
LabelInfo * allocInternalLabel(SMemPool * pool);
LabelInfo * allocCustomerLabel(Sym const* st, SMemPool * pool);
LabelInfo * allocPragmaLabel(Sym const* st, SMemPool * pool);
inline bool isSameLabel(LabelInfo const* li1, LabelInfo const* li2)
{
    ASSERT0(li1 && li2);
    if (li1 == li2) { return true; }
    if (LABELINFO_type(li1) == LABELINFO_type(li2) &&
        LABELINFO_num(li1) == LABELINFO_num(li2)) {
        return true;
    }
    return false;
}


class LabelHashFunc : public HashFuncBase<LabelInfo*> {
public:
    UINT get_hash_value(LabelInfo * li, UINT bucket_size) const
    { return ((UINT)computeLabelHashValue(li)) % bucket_size; }

    bool compare(LabelInfo * li1, LabelInfo * li2) const
    { return isSameLabel(li1, li2); }
};


class CustomerLabelHashFunc : public HashFuncBase<LabelInfo const*> {
public:
    UINT get_hash_value(LabelInfo const* li, UINT bucket_size) const
    { return ((UINT)computeLabelHashValue(li)) % bucket_size; }

    bool compare(LabelInfo const* li1, LabelInfo const* li2) const
    { return isSameLabel(li1, li2); }
};

} //namespace xoc
#endif
