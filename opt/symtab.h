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

//Record a variety of symbols such as user defined variables,
//compiler internal variables, LABEL, ID, TYPE_NAME etc.
#define SYM_name(sym)            ((sym)->s)
class SYM {
public:
    CHAR * s;
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

    UINT get_hash_value(SYM const* s, UINT bs) const
    {
        ASSERT0(isPowerOf2(bs));
        UINT v = computeCharSum(SYM_name(s));
        return hash32bit(v) & (bs - 1);
    }

    //Note v must be string pointer.
    UINT get_hash_value(OBJTY v, UINT bs) const
    {
        ASSERT_DUMMYUSE(sizeof(OBJTY) == sizeof(CHAR*),
            ("exception will taken place in type-cast"));
        ASSERT0(isPowerOf2(bs));
        UINT n = computeCharSum((CHAR*)v);
        return hash32bit(n) & (bs - 1);
    }

    bool compare(SYM const* s1, SYM const* s2) const
    { return strcmp(SYM_name(s1), SYM_name(s2)) == 0; }

    bool compare(SYM const* s, OBJTY val) const
    {
        ASSERT_DUMMYUSE(sizeof(OBJTY) == sizeof(CHAR*),
            ("exception will taken place in type-cast"));
        return (strcmp(SYM_name(s), (CHAR*)val) == 0);
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

    UINT get_hash_value(SYM const* s, UINT bs) const
    {
        ASSERT0(isPowerOf2(bs));
        UINT v = computeCharSum(SYM_name(s));
        return hash32bit(v) & (bs - 1);
    }

    //Note v must be const string pointer.
    UINT get_hash_value(OBJTY v, UINT bs) const
    {
        ASSERT_DUMMYUSE(sizeof(OBJTY) == sizeof(CHAR const*),
                ("exception will taken place in type-cast"));
        ASSERT0(isPowerOf2(bs));
        UINT n = computeCharSum((CHAR const*)v);
        return hash32bit(n) & (bs - 1);
    }

    bool compare(SYM const* s1, SYM const* s2) const
    { return strcmp(SYM_name(s1),  SYM_name(s2)) == 0; }

    bool compare(SYM const* s, OBJTY val) const
    {
        ASSERT_DUMMYUSE(sizeof(OBJTY) == sizeof(CHAR const*),
                ("exception will taken place in type-cast"));
        return (strcmp(SYM_name(s),  (CHAR const*)val) == 0);
    }
};


//
//START SymTab based on Hash
//
class SymTabHash : public Hash<SYM*, SymbolHashFunc> {
    SMemPool * m_pool;
public:
    explicit SymTabHash(UINT bsize) : Hash<SYM*, SymbolHashFunc>(bsize)
    { m_pool = smpoolCreate(64, MEM_COMM); }
    virtual ~SymTabHash()
    { smpoolDelete(m_pool); }

    CHAR * strdup(CHAR const* s)
    {
        if (s == NULL) {
            return NULL;
        }
        size_t l = strlen(s);
        CHAR * ns = (CHAR*)smpoolMalloc(l + 1, m_pool);
        ::memcpy(ns, s, l);
        ns[l] = 0;
        return ns;
    }

    SYM * create(OBJTY v)
    {
        SYM * sym = (SYM*)smpoolMalloc(sizeof(SYM), m_pool);
        SYM_name(sym) = strdup((CHAR const*)v);
        return sym;
    }

    //Add const string into symbol table.
    //If the string table is not big enough to hold strings, expand it.
    inline SYM * add(CHAR const* s)
    {
        UINT sz = Hash<SYM*, SymbolHashFunc>::get_bucket_size() * 4;
        if (sz < Hash<SYM*, SymbolHashFunc>::get_elem_count()) {
            Hash<SYM*, SymbolHashFunc>::grow(sz);
        }
        return Hash<SYM*, SymbolHashFunc>::append((OBJTY)s);
    }

    SYM * get(CHAR const* s)
    { return Hash<SYM*, SymbolHashFunc>::find((OBJTY)s); }
};
//END SymTabHash


//
//START SymTab based on Map
//
class CompareSymTab {
    CHAR * xstrdup(CHAR const* s)
    {
        if (s == NULL) {
            return NULL;
        }
        size_t l = ::strlen(s);
        CHAR * ns = (CHAR*)smpoolMalloc(l + 1, m_pool);
        ::memcpy(ns, s, l);
        ns[l] = 0;
        return ns;
    }
public:
    SMemPool * m_pool;

    bool is_less(SYM * t1, SYM * t2) const
    { return ::strcmp(SYM_name(t1), SYM_name(t2)) < 0; }

    bool is_equ(SYM * t1, SYM * t2) const
    { return ::strcmp(SYM_name(t1), SYM_name(t2)) == 0; }

    SYM * createKey(SYM * t)
    {
        SYM_name(t) = xstrdup(SYM_name(t));
        return t;
    }
};


class SymTab : public TTab<SYM*, CompareSymTab> {
    SYM * m_free_one;
    SMemPool * m_pool;
public:
    SymTab()
    {
        m_pool = smpoolCreate(64, MEM_COMM);
        m_free_one = NULL;
        TTab<SYM*, CompareSymTab>::m_ck.m_pool = m_pool;
        ASSERT0(m_pool);

    }
    virtual ~SymTab() { smpoolDelete(m_pool); }

    //Add const string into symbol table.
    SYM * add(CHAR const* s);
};
//END SymTab

} //namespace xoc
#endif
