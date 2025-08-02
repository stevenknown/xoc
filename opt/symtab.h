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
#ifndef __SYMTAB_H__
#define __SYMTAB_H__

namespace xoc {

class LogMgr;

//Record a variety of symbols such as user defined variables,
//compiler internal variables, LABEL, ID, TYPE_NAME etc.
#define SYM_name(sym) ((sym)->m_str)
class Sym {
    COPY_CONSTRUCTOR(Sym);
public:
    CHAR const* m_str;
public:
    Sym() { init(); }

    void clean() { SYM_name(this) = nullptr; }
    void dump(MOD LogMgr * lm) const;
    CHAR const* getStr() const { return m_str; }
    UINT getLen() const { return (UINT)::strlen(getStr()); }

    //The function initializes current Sym by given constant string.
    void initByString(CHAR const* s, UINT slen)
    {
        ASSERT0(s);
        DUMMYUSE(slen && s);
        SYM_name(this) = s;
    }
    void init() { m_str = nullptr; }
};


//The class represents extended symbol that records the byte length of string.
//e.g: given "ab\0c", the ::strlen() of Sym's string return the byte length 2.
//Instead, ESym's getLen() will return 4.
#define CASTCONSTSYM(ptr) (const_cast<Sym*>(static_cast<Sym const*>(ptr)))
#define ESYM_len(sym) ((sym)->m_slen)
#define ESYM_name(sym) (SYM_name(CASTCONSTSYM(sym)))
class ESym : public Sym {
    COPY_CONSTRUCTOR(ESym);
public:
    UINT m_slen;
public:
    ESym() { init(); }

    void clean() { ESYM_name(this) = nullptr; ESYM_len(this) = 0; }
    void dump(MOD LogMgr * lm) const;
    UINT getLen() const { return m_slen; }

    //The function initializes current Sym by given constant string.
    void initByString(CHAR const* s, UINT slen)
    {
        ASSERT0(s);
        ESYM_name(this) = const_cast<CHAR*>(s);
        ASSERT0(slen >= ::strlen(s));
        ESYM_len(this) = slen;
    }
    void init() { m_str = nullptr; m_slen = 0; }

    bool verify() const
    {
        if (::strlen(getStr()) != 0) {
            ASSERT0(getLen() != 0);
            //NOTE: ::strlen(getStr()) may not equal to getLen().
            //e.g: string is "ab\0c", strlen() return 2, but getLen()
            //return 4.
        }
        return true;
    }
};


class CompareStringFunc {
public:
    bool is_less(CHAR const* t1, CHAR const* t2) const
    { return ::strcmp(t1, t2) < 0; }

    bool is_equ(CHAR const* t1, CHAR const* t2) const
    { return ::strcmp(t1, t2) == 0; }

    CHAR const* createKey(CHAR const* t) { return t; }
};


class SymbolHashFunc {
public:
    UINT computeCharSum(CHAR const* s) const
    {
        UINT v = 0;
        UINT cnt = 0;
        while ((*s++ != 0) && (cnt < 20)) {
            v += (UINT)(*s);
            cnt++;
        }
        return v;
    }

    UINT get_hash_value(Sym const* s, UINT bs) const
    {
        ASSERT0(xcom::isPowerOf2(bs));
        UINT v = computeCharSum(SYM_name(s));
        return hash32bit(v) & (bs - 1);
    }

    //Note v must be string pointer.
    UINT get_hash_value(OBJTY v, UINT bs) const
    {
        ASSERTN(sizeof(OBJTY) == sizeof(CHAR*),
                ("exception will taken place in type-cast"));
        ASSERT0(xcom::isPowerOf2(bs));
        UINT n = computeCharSum((CHAR*)v);
        return hash32bit(n) & (bs - 1);
    }

    bool compare(Sym const* s1, Sym const* s2) const
    { return ::strcmp(SYM_name(s1), SYM_name(s2)) == 0; }

    bool compare(Sym const* s, OBJTY val) const
    {
        ASSERTN(sizeof(OBJTY) == sizeof(CHAR*),
                ("exception will taken place in type-cast"));
        return (::strcmp(SYM_name(s), (CHAR*)val) == 0);
    }
};


class ConstSymbolHashFunc {
public:
    UINT computeCharSum(CHAR const* s) const
    {
        UINT v = 0 ;
        UINT cnt = 0;
        while ((*s++ != 0) && (cnt < 20)) {
            v += (UINT)(*s);
            cnt++;
        }
        return v;
    }

    UINT get_hash_value(Sym const* s, UINT bs) const
    {
        ASSERT0(xcom::isPowerOf2(bs));
        UINT v = computeCharSum(SYM_name(s));
        return hash32bit(v) & (bs - 1);
    }

    //Note v must be const string pointer.
    UINT get_hash_value(OBJTY v, UINT bs) const
    {
        ASSERTN(sizeof(OBJTY) == sizeof(CHAR const*),
                ("exception will taken place in type-cast"));
        ASSERT0(xcom::isPowerOf2(bs));
        UINT n = computeCharSum((CHAR const*)v);
        return hash32bit(n) & (bs - 1);
    }

    bool compare(Sym const* s1, Sym const* s2) const
    { return ::strcmp(SYM_name(s1),  SYM_name(s2)) == 0; }

    bool compare(Sym const* s, OBJTY val) const
    {
        ASSERTN(sizeof(OBJTY) == sizeof(CHAR const*),
                ("exception will taken place in type-cast"));
        return (::strcmp(SYM_name(s),  (CHAR const*)val) == 0);
    }
};


//
//START SymTabBase based on Hash
//
class SymTabHash : public Hash<Sym const*, SymbolHashFunc> {
    COPY_CONSTRUCTOR(SymTabHash);
    SMemPool * m_pool;
public:
    explicit SymTabHash(UINT bsize) : Hash<Sym const*, SymbolHashFunc>(bsize)
    { m_pool = smpoolCreate(64, MEM_COMM); }
    virtual ~SymTabHash() { smpoolDelete(m_pool); }

    CHAR * strdup(CHAR const* s)
    {
        if (s == nullptr) {
            return nullptr;
        }
        size_t l = strlen(s);
        CHAR * ns = (CHAR*)smpoolMalloc(l + 1, m_pool);
        ::memcpy(ns, s, l);
        ns[l] = 0;
        return ns;
    }

    Sym const* create(OBJTY v)
    {
        Sym * sym = (Sym*)smpoolMalloc(sizeof(Sym), m_pool);
        SYM_name(sym) = strdup((CHAR const*)v);
        return sym;
    }

    //Add const string into symbol table.
    //If the string table is not big enough to hold strings, expand it.
    inline Sym const* add(CHAR const* s)
    {
        UINT sz = Hash<Sym const*, SymbolHashFunc>::get_bucket_size() * 4;
        if (sz < Hash<Sym const*, SymbolHashFunc>::get_elem_count()) {
            Hash<Sym const*, SymbolHashFunc>::grow(sz);
        }
        return Hash<Sym const*, SymbolHashFunc>::append((OBJTY)s);
    }

    Sym const* get(CHAR const* s)
    { return Hash<Sym const*, SymbolHashFunc>::find((OBJTY)s); }
};
//END SymTabHash


//
//START SymTabBase based on Hash
//
class CompareFuncSymTab {
    COPY_CONSTRUCTOR(CompareFuncSymTab);
protected:
    CHAR * xstrdup(CHAR const* s)
    {
        if (s == nullptr) {
            return nullptr;
        }
        size_t l = ::strlen(s);
        CHAR * ns = (CHAR*)smpoolMalloc(l + 1, m_pool);
        ::memcpy(ns, s, l);
        ns[l] = 0;
        return ns;
    }
public:
    SMemPool * m_pool;
public:
    CompareFuncSymTab() {}

    bool is_less(Sym const* t1, Sym const* t2) const
    { return ::strcmp(SYM_name(t1), SYM_name(t2)) < 0; }

    bool is_equ(Sym const* t1, Sym const* t2) const
    { return ::strcmp(SYM_name(t1), SYM_name(t2)) == 0; }

    //Note the function createKey() will modify parameter's contents, thus the
    //'const' qualifier is unusable.
    Sym * createKey(Sym * t)
    {
        SYM_name(t) = xstrdup(SYM_name(t));
        return t;
    }
};


//Note the symbol might be modified by CompareFuncSymTab::createKey(), thus the
//'const' qualifier of 'SymType*' is unusable.
template <class SymType>
class SymTabBaseIter : public xcom::TTabIter<SymType*> {};

template <class SymType, class CompareFuncType>
class SymTabBase : public xcom::TTab<SymType*, CompareFuncType> {
    COPY_CONSTRUCTOR(SymTabBase);
protected:
    SymType * m_free_one;
    SMemPool * m_pool;
public:
    SymTabBase()
    {
        m_pool = smpoolCreate(64, MEM_COMM);
        m_free_one = nullptr;
        xcom::TTab<SymType*, CompareFuncType>::m_ck.m_pool = m_pool;
        ASSERT0(m_pool);
    }
    virtual ~SymTabBase() { smpoolDelete(m_pool); }

    //Add const string into symbol table.
    SymType const* add(CHAR const* s);

    //Add const string into symbol table.
    //NOTE slen may be longer than the result of strlen(s).
    //e.g: given s is "ab\0c", slen is 4.
    SymType const* add(CHAR const* s, UINT slen);

    //Find const string in symbol table.
    //Return the Symbol if string existed.
    SymType * find(CHAR const* s) const;

    //Remove const string from symbol table.
    void remove(CHAR const* s);
};

//Add const string into symbol table.
template <class SymType, class CompareFuncType>
SymType const* SymTabBase<SymType, CompareFuncType>::add(CHAR const* s)
{
    return add(s, (UINT)::strlen(s));
}


//Add const string into symbol table.
template <class SymType, class CompareFuncType>
SymType const* SymTabBase<SymType, CompareFuncType>::add(
    CHAR const* s, UINT slen)
{
    ASSERT0(s);
    SymType * sym = m_free_one;
    if (sym == nullptr) {
        sym = (SymType*)smpoolMalloc(sizeof(SymType), m_pool);
        sym->init();
    }
    sym->initByString(s, slen);
    SymType const* appended_one =
        xcom::TTab<SymType*, CompareFuncType>::append(sym);
    ASSERT0(m_free_one == nullptr || m_free_one == sym);
    if (appended_one != sym) {
        //'s' has already been appended.
        sym->clean();
        m_free_one = sym;
    } else {
        //m_free_one has been inserted into table.
        //'s' is a new string so far.
        m_free_one = nullptr;
    }
    return appended_one;
}


//Find const string in symbol table.
template <class SymType, class CompareFuncType>
SymType * SymTabBase<SymType, CompareFuncType>::find(CHAR const* s) const
{
    ASSERT0(s);
    SymType sym;
    SYM_name(&sym) = const_cast<CHAR*>(s);
    bool find = false;
    return xcom::TTab<SymType*, CompareFuncType>::get(&sym, &find);
}


template <class SymType, class CompareFuncType>
void SymTabBase<SymType, CompareFuncType>::remove(CHAR const* s)
{
    SymType * sym = find(s);
    if (sym == nullptr) { return; }
    xcom::TTab<SymType*, CompareFuncType>::remove(sym);
}
//END SymTabBase


//
//START SymTab
//
class SymTabIter : public SymTabBaseIter<Sym>  {};
class SymTab : public SymTabBase<Sym, CompareFuncSymTab> {
    COPY_CONSTRUCTOR(SymTab);
public:
    SymTab() {}
    void dump(MOD LogMgr * lm) const;
};
//END SymTab


//
//START SymTabWithoutDupString
//
class CompareFuncSymTabWithoutDupString : public CompareFuncSymTab {
    COPY_CONSTRUCTOR(CompareFuncSymTabWithoutDupString);
public:
    CompareFuncSymTabWithoutDupString() {}

    //No need to copy string.
    Sym * createKey(Sym * t) { return t; }
};


class SymTabWithoutDupStringIter : public SymTabBaseIter<Sym> {};
class SymTabWithoutDupString
    : public SymTabBase<Sym, CompareFuncSymTabWithoutDupString> {
    COPY_CONSTRUCTOR(SymTabWithoutDupString);
public:
    SymTabWithoutDupString() {}
    void dump(MOD LogMgr * lm) const;
};
//END SymTabWithoutDupString


//The class represents the comparison bewteen two ESym.
class CompareFuncESymTab {
    COPY_CONSTRUCTOR(CompareFuncESymTab);
protected:
    CHAR * xstrdup(CHAR const* s, UINT slen)
    {
        if (s == nullptr) { return nullptr; }
        CHAR * ns = (CHAR*)smpoolMalloc(slen + 1, m_pool);
        ::memcpy(ns, s, slen);
        ns[slen] = 0;
        return ns;
    }
public:
    SMemPool * m_pool;
public:
    CompareFuncESymTab() {}

    bool is_less(ESym const* t1, ESym const* t2) const
    {
        ASSERT0(t1 && t2);
        ASSERT0(t1->verify() && t2->verify());
        return t1->getLen() < t2->getLen() ||
               (t1->getLen() == t2->getLen() &&
                ::memcmp(ESYM_name(t1), ESYM_name(t2), t1->getLen()) < 0);
    }

    bool is_equ(ESym const* t1, ESym const* t2) const
    {
        ASSERT0(t1 && t2);
        ASSERT0(t1->verify() && t2->verify());
        return t1->getLen() == t2->getLen() &&
               ::memcmp(ESYM_name(t1), ESYM_name(t2), t1->getLen()) == 0;
    }

    //Note the function createKey() will modify parameter's contents, thus the
    //'const' qualifier is unusable.
    ESym * createKey(ESym * t)
    {
        ASSERT0(t && t->verify());
        ESYM_name(t) = xstrdup(t->getStr(), t->getLen());
        ESYM_len(t) = t->getLen();
        return t;
    }
};


//
//START ESymTab
//
class ESymTabIter : public SymTabBaseIter<ESym>  {};
class ESymTab : public xoc::SymTabBase<ESym, CompareFuncESymTab> {
    COPY_CONSTRUCTOR(ESymTab);
public:
    ESymTab() {}
    void dump(MOD LogMgr * lm) const;
};
//END ESymTab

} //namespace xoc
#endif
