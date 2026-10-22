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
//START Config
//
Config::Config(xoc::RegionMgr * rm, JNodeMgr & jm, xoc::ESymTab & sym_tab)
    : m_jm(jm), m_sym_tab(sym_tab)
{
    m_rm = rm;
}


Config::~Config()
{
}


double Config::extractNum(xoc::Sym const* name) const
{
    JNode const* conf = m_sym2conf.get(name);
    ASSERTN(conf, ("not exist"));
    ASSERT0(conf->is_number());
    return conf->getNum();
}


bool Config::extractBool(xoc::Sym const* name) const
{
    JNode const* conf = m_sym2conf.get(name);
    ASSERTN(conf, ("not exist"));
    ASSERT0(conf->is_bool());
    return conf->getBool();
}


xoc::Sym const* Config::extractStr(xoc::Sym const* name) const
{
    JNode const* conf = m_sym2conf.get(name);
    ASSERTN(conf, ("not exist"));
    ASSERT0(conf->is_string());
    return conf->getStr();
}


void Config::dump() const
{
    if (!m_rm->isLogMgrInit()) { return; }
    if (!g_dump_config) { return; }
    Sym2ConfIter it;
    JNode const* conf;
    note(m_rm, "\n-- DUMP Model Config --");
    m_rm->getLogMgr()->incIndent(2);
    note(m_rm, "\n-- Sym2Config --");
    for (xoc::Sym const* name = m_sym2conf.get_first(it, &conf);
         name != nullptr; name = m_sym2conf.get_next(it, &conf)) {
        note(m_rm, "\n%s:", name->getStr());
        switch (JNODE_type(conf)) {
        case JN_NUMBER: prt(m_rm, "%lf", conf->getNum()); break;
        case JN_STRING: prt(m_rm, "%s", conf->getStr()->getStr()); break;
        case JN_BOOL: prt(m_rm, "%u", conf->getBool()); break;
        case JN_NULL: prt(m_rm, "NULL"); break;
        default: UNREACHABLE();
        }
    }
    m_rm->getLogMgr()->decIndent(2);
}


static JNode const* extractScalarNode(JNode const* jn)
{
    switch (JNODE_type(jn)) {
    case JN_NUMBER:
    case JN_STRING:
    case JN_BOOL:
    case JN_NULL:
        return jn;
    case JN_ARRAY: {
        JNode const* elem = JARRAY_elem_list(jn);
        ASSERT0(elem);
        ASSERT0(xcom::cnt_list(elem) == 1);
        return extractScalarNode(elem);
    }
    default: UNREACHABLE();
    }
    return nullptr;
}


static void extractScalar(
    xoc::Sym const* key, JNode const* val, MOD Sym2Conf & s2c)
{
    switch (JNODE_type(val)) {
    case JN_NUMBER:
    case JN_STRING:
    case JN_BOOL:
    case JN_NULL:
        s2c.set(key, val);
        return;
    default: UNREACHABLE(); //NOT SCALAR.
    }
}


static void extractPair(JNode const* jn, MOD Sym2Conf & s2c)
{
    ASSERT0(jn->is_pair());
    xoc::Sym const* key = JPAIR_key(jn);
    JNode const* val = JPAIR_val(jn);
    switch (JNODE_type(val)) {
    case JN_NUMBER:
    case JN_STRING:
    case JN_BOOL:
    case JN_NULL:
        extractScalar(key, val, s2c);
        return;
    case JN_ARRAY: {
        JNode const* elem = JARRAY_elem_list(val);
        ASSERT0(elem);
        ASSERT0(xcom::cnt_list(elem) == 1);

        //The elem should be salar, or else we have to handle the case.
        extractScalar(key, elem, s2c);
        return;
    }
    case JN_OBJ:
        //The key of 'jn' is useless, extract configuration from inner pair.
        for (JNode const* x = JOBJ_pair_list(val);
             x!= nullptr; x = x->next) {
            ASSERT0(x->is_pair());
            extractPair(x, s2c);
        }
        return;
    default: ASSERT0(0); //TODO:
    }
}


XI_STATUS Config::extractConfig(JNode const* root)
{
    if (!root->is_obj()) { return XI_INVALID_TENSOR_ATTR; }
    for (JNode const* x = JOBJ_pair_list(root);
         x!= nullptr; x = x->next) {
        ASSERT0(x->is_pair());
        extractPair(x, m_sym2conf);
    }
    return XI_SUCC;
}


XI_STATUS Config::readConfig(CHAR const* filename)
{
    xcom::FO_STATUS st;
    bool is_del = false;
    bool is_readonly = false;
    xcom::FileObj file(filename, is_del, is_readonly, &st);
    if (st != xcom::FO_SUCC) {
        xoc::prt2C("error: open \"%s\" failed.\n", filename);
        return XI_ERR;
    }
    size_t filesize = file.getFileSize();
    xoc::BYTEVec jsonbuf;
    jsonbuf.grow(filesize);

    size_t rd = 0;
    st = file.read(jsonbuf.get_vec(), 0, filesize, &rd);
    if (st != xcom::FO_SUCC) {
        xoc::prt2C(
            "error: read %u bytes from '%s' failed.\n", filesize, filename);
        return XI_ERR;
    }
    ASSERT0(rd == filesize);

    JsonParser jp(m_rm->getLogMgr(), getJNodeMgr());
    JNode const* root = jp.parse(
        (CHAR const*)jsonbuf.get_vec(), jsonbuf.get_capacity());
    if (root == nullptr) { return XI_ERR; }
    extractConfig(root);
    return XI_SUCC;
}
//END Config

} //namespace xinfer
