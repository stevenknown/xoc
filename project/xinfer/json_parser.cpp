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
#include "cJSON.h"

namespace xinfer {

class JPIntl {
public:
    static JNode * convertToJNode(JNodeMgr & jm, cJSON const* cnode);
};

JNode * JPIntl::convertToJNode(JNodeMgr & jm, cJSON const* cnode)
{
    if (cnode == nullptr) { return nullptr; }
    if (cJSON_IsObject(cnode)) {
        JObj * obj = jm.allocJObj();
        JNode * last = nullptr;
        cJSON const* child = NULL;
        cJSON_ArrayForEach(child, cnode) {
            JPair * pair = jm.allocJPair();
            xoc::Sym const* keysym = jm.addToSymbolTab(child->string);
            pair->key = keysym;
            pair->val = convertToJNode(jm, child);
            xcom::add_next(&obj->pair_list, &last, (JNode*)pair);
        }
        return obj;
    }
    if (cJSON_IsArray(cnode)) {
        JArray * array = jm.allocJArray();
        JNode * last = nullptr;
        cJSON const* elem = nullptr;
        cJSON_ArrayForEach(elem, cnode) {
            JNode * jelem = convertToJNode(jm, elem);
            xcom::add_next(&array->elem_list, &last, jelem);
        }
        return array;
    }
    if (cJSON_IsString(cnode)) {
        JNode * n = jm.allocJNode();
        n->type = JN_STRING;
        CHAR const* s = cJSON_GetStringValue(cnode);
        n->setString(jm, s);
        return n;
    }
    if (cJSON_IsNumber(cnode)) {
        JNode * n = jm.allocJNode();
        n->type = JN_NUMBER;
        JNODE_number(n) = cJSON_GetNumberValue(cnode);
        return n;
    }
    if (cJSON_IsTrue(cnode)) {
        JNode * n = jm.allocJNode();
        n->type = JN_BOOL;
        JNODE_bool(n) = 1;
        return n;
    }
    if (cJSON_IsFalse(cnode)) {
        JNode * n = jm.allocJNode();
        n->type = JN_BOOL;
        JNODE_bool(n) = 0;
        return n;
    }
    if (cJSON_IsNull(cnode)) {
        JNode * n = jm.allocJNode();
        n->type = JN_NULL;
        return n;
    }
    UNREACHABLE();
    return nullptr;
}


//
//START JsonParser
//
JsonParser::JsonParser(xoc::LogMgr * lm, JNodeMgr & jm) : m_jm(jm)
{
    m_lm = lm;
}


static void deparse(cJSON * root, xoc::LogMgr * lm)
{
    char * dumpstr = cJSON_PrintUnformatted(root);
    if (dumpstr == nullptr) { return; }
    xoc::note(lm, "%s\n", dumpstr);
    cJSON_free(dumpstr);
}


void JsonParser::dump(JNode const* jn) const
{
    if (m_lm == nullptr || !m_lm->is_init() || !g_dump_json_parser) { return; }
    jn->dump(m_lm);
}


JNode const* JsonParser::parse(CHAR const* strbuf, UINT strbuflen)
{
    //cJSON * cjson = cJSON_Parse(strbuf);
    //char * info = cJSON_Print(const cJSON *item);
    char const* parse_end = NULL;
    cJSON * root = cJSON_ParseWithLengthOpts(strbuf, strbuflen, &parse_end, 0);
    if (root == nullptr) {
        //CASE:json parse error, position near: %s\n", parse_end.
        xcom::StrBuf tmp(8);
        tmp.sprint(7, "%s", parse_end);
        dumpAct(m_lm,
            "\nerror:JSON parse failed! file byte offset: %u, "
            "near string: '%s'\n",
            parse_end - strbuf, tmp.getBuf());
        return nullptr;
    }
    if (!cJSON_IsObject(root)) {
        //CASE:json root is not object.
        cJSON_Delete(root);
        return nullptr;
    }
    //deparse(root, m_lm); //ONLY FOR DEBUG.
    DUMMYUSE(deparse);
    JNode * jnode_root = JPIntl::convertToJNode(m_jm, root);
    dump(jnode_root);
    cJSON_Delete(root);
    return jnode_root;
}
//END JsonParser

} //namespace xinfer
