/*@
Copyright (c) 2026-present, Su Zhenyu steven.known@gmail.com

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
#ifndef _JNODE_H_
#define _JNODE_H_

namespace xinfer {

class JPair;
class JObj;
class JArray;
class JNode;
class JNodeMgr;

typedef enum tagJN_TYPE {
    JN_UNDEF = 0,
    JN_OBJ,
    JN_PAIR,
    JN_ARRAY,
    JN_STRING,
    JN_NUMBER,
    JN_BOOL,
    JN_NULL,
} JN_TYPE;


#define JNODECASTCONST(ptr) (const_cast<JNode*>(static_cast<JNode const*>(ptr)))

#define JNODE_type(j) ((j)->type)
#define JNODE_string(j) ((j)->u.string)
#define JNODE_bool(j) ((j)->u.boolean)
#define JNODE_number(j) ((j)->u.number)
class JNode {
    COPY_CONSTRUCTOR(JNode);
public:
    JN_TYPE type;
    JNode * next;
    JNode * prev;
    union {
        bool boolean;
        double number;
        xoc::Sym const* string;
    } u;
public:
    void dump(xoc::LogMgr * lm) const;

    JN_TYPE getType() const { return type; }
    xoc::Sym const* getStr() const { return u.string; }
    double getNum() const { return u.number; }
    bool getBool() const { return u.boolean; }

    bool is_obj() const { return type == JN_OBJ; }
    bool is_pair() const { return type == JN_PAIR; }
    bool is_array() const { return type == JN_ARRAY; }
    bool is_string() const { return type == JN_STRING; }
    bool is_number() const { return type == JN_NUMBER; }
    bool is_bool() const { return type == JN_BOOL; }
    bool is_null() const { return type == JN_NULL; }

    void setString(JNodeMgr & jm, CHAR const* s);
};

//The pair of key and value.
#define JPAIR_key(j)  (((JPair*)JNODECASTCONST(j))->key)
#define JPAIR_val(j)  (((JPair*)JNODECASTCONST(j))->val)
class JPair  : public JNode {
    COPY_CONSTRUCTOR(JPair);
public:
    xoc::Sym const* key;
    JNode * val;
};

#define JOBJ_pair_list(j)  (((JObj*)JNODECASTCONST(j))->pair_list)
class JObj  : public JNode {
    COPY_CONSTRUCTOR(JObj);
public:
    JNode * pair_list;
};

#define JARRAY_elem_list(j)  (((JArray*)JNODECASTCONST(j))->elem_list)
class JArray : public JNode {
    COPY_CONSTRUCTOR(JArray);
public:
    JNode * elem_list;
};

class JNodeMgr {
    COPY_CONSTRUCTOR(JNodeMgr);
    friend class JPIntl;
protected:
    xcom::SMemPool * m_pool;
    xoc::Sym const* m_f32;
    xoc::Sym const* m_f16;
    xoc::Sym const* m_bf16;
    xoc::Sym const* m_i8;
    xoc::Sym const* m_u8;
    xoc::Sym const* m_i16;
    xoc::Sym const* m_i32;
    xoc::Sym const* m_i64;
    xoc::Sym const* m_bool;
    xoc::Sym const* m_f8_e4m3;
    xoc::Sym const* m_f8_e5m2;
    xoc::ESymTab & m_sym_tab;
protected:
    xoc::DATA_TYPE mapStrToDType(xoc::Sym const* str) const;
protected:
    JNode * allocJNode();
    JPair * allocJPair();
    JObj * allocJObj();
    JArray * allocJArray();
    CHAR const* dupString(CHAR const* str);
    void * xmalloc(UINT size);
public:
    JNodeMgr(xoc::ESymTab & sym_tab);
    ~JNodeMgr();
    xoc::Sym const* addToSymbolTab(CHAR const* s);
    XI_STATUS extractString(JNode const* jn, OUT xoc::Sym const*& str);
    XI_STATUS extractBool(JNode const* jn, OUT bool & bval);
    XI_STATUS extractNumber(JNode const* jn, OUT double & number);
    XI_STATUS extractDType(JNode const* jn, OUT xoc::DATA_TYPE & dt);
};

} //namespace xinfer

#endif
