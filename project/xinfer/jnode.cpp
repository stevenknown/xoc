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
#include "xinfer_cominc.h"

namespace xinfer {

//
//START JNode
//
void JNode::dump(xoc::LogMgr * lm) const
{
    if (lm == nullptr || !lm->is_init()) { return; }
    switch (this->type) {
    case JN_OBJ:
        xoc::note(lm, "\nOBJ:");
        lm->incIndent(2);
        for (JNode const* pair = JOBJ_pair_list(this);
             pair != nullptr; pair = pair->next) {
            pair->dump(lm);
        }
        lm->decIndent(2);
        return;
    case JN_PAIR:
        xoc::note(lm, "\nPAIR:");
        lm->incIndent(2);
        xoc::note(lm, "\nKEY:%s", JPAIR_key(this)->getStr());
        JPAIR_val(this)->dump(lm);
        lm->decIndent(2);
        return;
    case JN_ARRAY:
        xoc::note(lm, "\nARRAY:");
        lm->incIndent(2);
        for (JNode const* elem = JARRAY_elem_list(this);
             elem != nullptr; elem = elem->next) {
            elem->dump(lm);
        }
        lm->decIndent(2);
        return;
    case JN_STRING:
        xoc::note(lm, "\nSTRING:%s", JNODE_string(this)->getStr());
        return;
    case JN_NUMBER:
        xoc::note(lm, "\nNUMBER:%lf", JNODE_number(this));
        return;
    case JN_BOOL:
        xoc::note(lm, "\nBOOL:%u", JNODE_bool(this));
        return;
    case JN_NULL:
        xoc::note(lm, "\nNULL");
        return;
    default:;
    }
}


void JNode::setString(JNodeMgr & jm, CHAR const* s)
{
    JNODE_string(this) = jm.addToSymbolTab(s);
}
//END JNode



//
//START JNodeMgr
//
JNodeMgr::JNodeMgr(xoc::ESymTab & sym_tab) : m_sym_tab(sym_tab)
{
    m_pool = xcom::smpoolCreate(64, MEM_COMM);
    m_f32 = addToSymbolTab("F32");
    m_f16 = addToSymbolTab("F16");
    m_bf16 = addToSymbolTab("BF16");
    m_i8 = addToSymbolTab("I8");
    m_u8 = addToSymbolTab("U8");
    m_i16 = addToSymbolTab("I16");
    m_i32 = addToSymbolTab("I32");
    m_i64 = addToSymbolTab("I64");
    m_bool = addToSymbolTab("BOOL");
    m_f8_e4m3 = addToSymbolTab("F8_E4M3");
    m_f8_e5m2 = addToSymbolTab("F8_E5M2");
}


JNodeMgr::~JNodeMgr()
{
    xcom::smpoolDelete(m_pool);
    m_pool = nullptr;
}


XI_STATUS JNodeMgr::extractNumber(JNode const* jn, OUT double & number)
{
    ASSERT0(jn);
    ASSERT0(jn->is_number());
    number = JNODE_number(jn);
    return XI_SUCC;
}


XI_STATUS JNodeMgr::extractBool(JNode const* jn, OUT bool & bval)
{
    ASSERT0(jn);
    ASSERT0(jn->is_bool());
    bval = JNODE_bool(jn);
    return XI_SUCC;
}


XI_STATUS JNodeMgr::extractDType(JNode const* jn, OUT xoc::DATA_TYPE & dt)
{
    ASSERT0(jn->is_string());
    dt = mapStrToDType(JNODE_string(jn));
    ASSERT0(dt != D_UNDEF);
    return XI_SUCC;
}


XI_STATUS JNodeMgr::extractString(JNode const* jn, OUT xoc::Sym const*& str)
{
    ASSERT0(jn->is_string());
    str = JNODE_string(jn);
    return XI_SUCC;
}


xoc::DATA_TYPE JNodeMgr::mapStrToDType(xoc::Sym const* str) const
{
    if (str == m_f32) { return xoc::D_F32; }
    if (str == m_f16) { return xoc::D_F16; }
    if (str == m_bf16) { return xoc::D_BF16; }
    if (str == m_i8) { return xoc::D_I8; }
    if (str == m_u8) { return xoc::D_U8; }
    if (str == m_i16) { return xoc::D_I16; }
    if (str == m_i32) { return xoc::D_I32; }
    if (str == m_i64) { return xoc::D_I64; }
    if (str == m_bool) { return xoc::D_B; }
    if (str == m_f8_e4m3) { return xoc::D_F8E4M3; }
    if (str == m_f8_e5m2) { return xoc::D_F8E5M2; }
    ASSERTN(0, ("TODO"));
    return xoc::D_UNDEF;
}


void * JNodeMgr::xmalloc(UINT size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = xcom::smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset((void*)p, 0, size);
    return p;
}


JNode * JNodeMgr::allocJNode()
{
    return (JNode*)xmalloc(sizeof(JNode));
}


JPair * JNodeMgr::allocJPair()
{
    JPair * j = (JPair*)xmalloc(sizeof(JPair));
    j->type = JN_PAIR;
    return j;
}


JObj * JNodeMgr::allocJObj()
{
    JObj * j = (JObj*)xmalloc(sizeof(JObj));
    j->type = JN_OBJ;
    return j;
}


JArray * JNodeMgr::allocJArray()
{
    JArray * j = (JArray*)xmalloc(sizeof(JArray));
    j->type = JN_ARRAY;
    return j;
}


xoc::Sym const* JNodeMgr::addToSymbolTab(CHAR const* s)
{
    return m_sym_tab.add(s);
}


CHAR const* JNodeMgr::dupString(CHAR const* str)
{
    UINT len = ::strlen(str) + 1;
    CHAR * newstr = (CHAR*)xmalloc(len);
    ::memcpy(newstr, str, len + 1);
    return newstr;
}
//END JNodeMgr

} //namespace xinfer
