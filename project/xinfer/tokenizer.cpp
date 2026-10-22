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
#include "../../reader/ir_lex.h"

namespace xinfer {

//
//START TokenArrayMgr
//
TokenArrayMgr::~TokenArrayMgr()
{
    for (TokenArray * tokarr = m_tokarr_list.get_head();
         tokarr != nullptr; tokarr = m_tokarr_list.get_next()) {
        ::free(tokarr->array);
        ::free(tokarr);
    }
    m_tokarr_list.clean();
}


TokenArray * TokenArrayMgr::allocTokenArray(int64_t array_elem_num)
{
    ASSERT0(array_elem_num > 0);
    TokenArray * tokarr = (TokenArray*)::malloc(sizeof(TokenArray));
    tokarr->array = (int64_t*)::malloc(sizeof(int64_t) * array_elem_num);
    tokarr->array_elem_num = array_elem_num;
    m_tokarr_list.append_tail(tokarr);
    return tokarr;
}
//END


//
//START Tokenizer
//
Tokenizer::Tokenizer(xoc::RegionMgr * rm, JNodeMgr & jm, xoc::ESymTab & sym_tab)
    : m_jm(jm), m_sym_tab(sym_tab), m_mtp(rm->getLogMgr(), sym_tab)
{
    m_rm = rm;
    m_tokenizer_root = nullptr;
    m_added_tokens_decoder = sym_tab.add("added_tokens_decoder");
    m_additional_special_tokens = sym_tab.add("additional_special_tokens");
}


Tokenizer::~Tokenizer()
{
}


void Tokenizer::dumpVocab() const
{
    note(m_rm, "\n-- Vocab, Total:%u, Sym2Tokenid --",
         m_sym2tokenid.get_elem_count());
    Sym2TokenIdIter it;
    TOKID tokenid;
    bool keydetail = true;
    for (xoc::Sym const* name = m_sym2tokenid.get_first(it, &tokenid);
         name != nullptr; name = m_sym2tokenid.get_next(it, &tokenid)) {
        note(m_rm, "\n%s", name->getStr());
        if (keydetail) {
            prt(m_rm, "[");
            CHAR const* string = name->getStr();
            UINT len = name->getLen();
            prt(m_rm, "0x");
            for (UINT i = 0; i < len; i++) {
                if (i != 0) { prt(m_rm, " "); }
                prt(m_rm, "%02X", (BYTE)string[i]);
            }
            prt(m_rm, "]");
        }
        prt(m_rm, ":%llu", tokenid);
    }
}


void Tokenizer::dumpConfig() const
{
    note(m_rm, "\n-- Sym2Config --");
    Sym2TokenConfIter it;
    JNode const* conf;
    for (xoc::Sym const* name = m_sym2tokenconf.get_first(it, &conf);
         name != nullptr; name = m_sym2tokenconf.get_next(it, &conf)) {
        note(m_rm, "\n%s:", name->getStr());
        switch (JNODE_type(conf)) {
        case JN_NUMBER: prt(m_rm, "%lf", conf->getNum()); break;
        case JN_STRING: prt(m_rm, "%s", conf->getStr()->getStr()); break;
        case JN_BOOL: prt(m_rm, "%u", conf->getBool()); break;
        case JN_NULL: prt(m_rm, "NULL"); break;
        default: UNREACHABLE();
        }
    }
}


void Tokenizer::dump() const
{
    if (!m_rm->isLogMgrInit()) { return; }
    if (!g_dump_tokenizer) { return; }
    note(m_rm, "\n==-- DUMP Tokenizer --==");
    m_rm->getLogMgr()->incIndent(2);

    dumpConfig();
    dumpVocab();
    if (m_tokenizer_root != nullptr) {
        note(m_rm, "\n-- ENTIR SERIALIZED TOKENIZER --");
        m_tokenizer_root->dump(m_rm->getLogMgr());
    }
    m_mtp.dump();

    m_rm->getLogMgr()->decIndent(2);
}


xoc::Sym const* Tokenizer::addToSymbolTab(CHAR const* s)
{
    return m_sym_tab.add(s);
}


static void dumpPieces(
    xcom::List<CHAR const*> const& pieces, xoc::RegionMgr const* rm)
{
    if (!rm->isLogMgrInit()) { return; }
    if (!g_dump_pieces) { return; }
    note(rm, "\n-- DUMP PIECES:%u --", pieces.get_elem_count());
    rm->getLogMgr()->incIndent(2);
    xcom::List<CHAR const*>::Iter it;
    for (CHAR const* s = pieces.get_head(&it);
         s != nullptr; s = pieces.get_next(&it)) {
        note(rm, "\nUTF8 STR:%s", s);

        //Byte val.
        prt(rm, ", [");
        for (CHAR const* sp = s; *sp != 0; sp++) {
            if (sp != s) { prt(rm, ","); }
            prt(rm, "0x%02X", (BYTE)*sp);
        }
        prt(rm, "]");

        //Unicode code point.
        rm->getLogMgr()->incIndent(2);
        CHAR const* byte = s;
        UINT cplen;
        while (*byte != 0) {
            UINT32 cp = xcom::getCodePointByUTF8(byte, &cplen);
            note(rm, "\nU+%04X ", cp);
            byte = byte + cplen;
        }
        rm->getLogMgr()->decIndent(2);
    }
    rm->getLogMgr()->decIndent(2);
}


TokenArray * Tokenizer::mapSubwordToTokenId(
    MOD TokenArrayMgr & tokarrmgr, UINT max_token_num,
    xcom::List<CHAR const*> const& pieces)
{
    TokenArray * ta = tokarrmgr.allocTokenArray(pieces.get_elem_count());
    ASSERT0(ta && ta->array);
    UINT token_cnt = 0;
    xcom::List<CHAR const*>::Iter it;
    for (CHAR const* subword = pieces.get_head(&it);
         subword != nullptr; subword = pieces.get_next(&it)) {
        //subword is utf8 string.
        bool found = false;
        xoc::Sym const* sym = addToSymbolTab(subword);

        //Try to find tokenid from the vocab set that is supplied by the model.
        TOKID tid = getSym2TokenId().get(sym, &found);
        if (!found) {
            //The model doesn't support the merged-subword in vocab.
            dumpAct(m_rm->getLogMgr(),
                "\ncannot find subword in sym2tokenid table, "
                "may be the vocab.json is incomplete.");
            return nullptr;
        }
        if (token_cnt >= max_token_num) {
            return nullptr;
        }
        ta->array[token_cnt] = tid;
        token_cnt++;
    }
    return ta;
}


void Tokenizer::mergeSubwordByBPE(
    MOD xcom::MemPoolMgr & poolmgr, MOD xcom::List<CHAR const*> & pieces)
{
    Sym2Rank const& sym2rank = getMergesTxtParser().getSym2Rank();
    xcom::FixedStrBuf<64> mergedkey;
    for (;;) {
        UINT piece_count = pieces.get_elem_count();
        if (piece_count <= 1) {
            //only one piece, no adjacent pair to merge.
            break;
        }
        UINT best_rank = MAX_RANK;

        //Record index of first element in best pair.
        UINT best_i = (UINT)-1;
        xcom::List<CHAR const*>::Iter best_i_it = nullptr;
        xcom::List<CHAR const*>::Iter best_i_next_it = nullptr;

        //Scan all adjacent pairs.
        xcom::List<CHAR const*>::Iter it;
        xcom::List<CHAR const*>::Iter nextit;
        pieces.get_head(&it);
        nextit = it;
        pieces.get_next(&nextit);
        for (UINT i = 0; i < piece_count - 1; i++) {
            CHAR const* a = it->val();
            CHAR const* b = nextit->val();
            ASSERT0(a && b);
            mergedkey.sprint("%s %s", a, b);
            xoc::Sym const* sym = addToSymbolTab(mergedkey.getBuf());
            bool found = false;
            UINT rank = sym2rank.get(sym, &found);
            if (found) {
                if (rank < best_rank) {
                    best_rank = rank;
                    best_i = i;
                    best_i_it = it;
                    best_i_next_it = nextit;
                    if (best_rank == MIN_RANK) { break; }
                }
            }
            it = nextit;
            pieces.get_next(&nextit);
        }
        if (best_rank == MAX_RANK) {
            //No more mergeable pair, exit bpe loop.
            break;
        }
        ASSERT0((best_i + 1)< piece_count);

        //Merge pieces(best_i) and pieces(best_i+1) into new string.
        CHAR const* best_i_str = best_i_it->val();
        CHAR const* best_i_next_str = best_i_next_it->val();
        ASSERT0(best_i_str && best_i_next_str);
        size_t best_i_str_len = ::strlen(best_i_str);
        size_t best_i_next_str_len = ::strlen(best_i_next_str);
        size_t merge_sz = best_i_str_len + best_i_next_str_len + 1;
        CHAR * merged = (CHAR*)poolmgr.alloc(merge_sz + 1);
        ASSERT0(merged);
        memcpy(merged, best_i_str, best_i_str_len);
        memcpy(merged + best_i_str_len, best_i_next_str, best_i_next_str_len);
        merged[best_i_str_len + best_i_next_str_len] = 0;
        C_val(best_i_it) = merged;
        pieces.remove(best_i_next_it);
    }
}


//NOTE:If text is only contain ASCII, the convertion isn't necessary, because
//it isn't change. The funtion is useful only if text contains Chinese or other
//UTF code.
void Tokenizer::convertRawByteStringToUTF8Subword(
    CHAR const* text, MOD xcom::MemPoolMgr & poolmgr,
    OUT xcom::List<CHAR const*> & pieces)
{
    for (BYTE const* p = (BYTE const*)text; *p != 0; p++) {
        //byte_val is not only ASCII, but also include utf-8 code,
        //because the input might contain unicode.
        BYTE byte_val = *p;

        //Bytelevel map to utf8 string.
        CHAR const* subword = xcom::getUTF8ByRawByte(byte_val);
        size_t subwordlen = ::strlen(subword);
        CHAR * dup = (CHAR*)poolmgr.alloc(subwordlen + 1);
        ASSERT0(dup);
        ::memcpy(dup, subword, subwordlen);
        dup[subwordlen] = 0;
        pieces.append_tail(dup);
    }
}


TOKID Tokenizer::encode(CHAR text)
{
    //Bytelevel map to utf8 string.
    CHAR const* subword_utf8string = xcom::getUTF8ByRawByte((BYTE)text);
    ASSERT0(subword_utf8string);

    //Only one piece, no adjacent pair to merge.
    bool found = false;
    xoc::Sym const* sym = addToSymbolTab(subword_utf8string);

    //Try to find tokenid from the vocab set that is supplied by the model.
    TOKID tid = getSym2TokenId().get(sym, &found);
    if (!found) {
        //The model doesn't support the subword in vocab.
        dumpAct(m_rm->getLogMgr(),
            "\ncannot find subword in sym2tokenid table, "
            "may be the vocab.json is incomplete.");
        return INVALID_TOKEN;
    }
    return tid;
}


TokenArray * Tokenizer::encode(
    CHAR const* text, UINT max_token_num, TokenArrayMgr & tokarrmgr)
{
    ASSERT0(text);
    size_t text_len = ::strlen(text);
    ASSERT0(text_len > 0);
    xcom::MemPoolMgr poolmgr;
    xcom::List<CHAR const*> pieces; //record merged or orinial utf8 string.

    //Split input to raw bytes.
    convertRawByteStringToUTF8Subword(text, poolmgr, pieces);

    //BPE merge.
    mergeSubwordByBPE(poolmgr, pieces);
    dumpPieces(pieces, m_rm);

    //Map subword string to token id.
    return mapSubwordToTokenId(tokarrmgr, max_token_num, pieces);
}


void Tokenizer::decode(
    TOKID tokid, OUT xcom::StrBuf & output, bool output_utf8)
{
    CHAR * dst = output.getBuf();
    UINT outputbuflen = output.getBufLen();
    xoc::Sym const* sym = getTokenId2Sym().get(tokid);
    ASSERT0(sym);

    //A subword is an UTF8 string, concatenated by BPE, thus it contains
    //multiple unicode codepoints.
    //User has to parse each codepoint one by one, and look up the table and
    //recover the raw byte.
    CHAR const* subword = sym->getStr();
    CHAR const* p = subword;
    UINT curlen = 0;
    BYTE tmp_utf8[8];
    while (*p != 0) {
        UINT cplen;
        UINT32 cp = xcom::getCodePointByUTF8(p, &cplen);
        ASSERT0(cplen <= CODEPOINT_MAX_BYTESIZE);
        ASSERT0(cp != INVALID_CODEPOINT);
        UINT nwrite = 0;
        BYTE rawbyte;
        if (output_utf8) {
            //Encode codepoint again to utf8 and output utf8 string.
            nwrite = xcom::getUTF8ByCodePoint(cp, tmp_utf8, sizeof(tmp_utf8));
            ASSERT0(nwrite > 0);
        } else {
            //Output rawbyte.
            rawbyte = xcom::getRawBYTEByCodePoint(cp);
            ASSERTN(rawbyte != INVALID_RAWBYTE,
                    ("unmapped codepoint, table incomplete"));
            nwrite = 1;
        }
        if (curlen + nwrite >= outputbuflen) {
            output.growBuf(outputbuflen * 2);
            outputbuflen = output.getBufLen();
            dst = output.getBuf();
        }
        if (output_utf8) { ::memcpy(dst + curlen, tmp_utf8, nwrite); }
        else { dst[curlen] = (CHAR)rawbyte; }

        curlen += nwrite;
        p += cplen; //go ahead to next utf8 char.
    }
    dst[curlen] = 0;
}


XI_STATUS Tokenizer::extractVocab(JNode const* root)
{
    if (!root->is_obj()) { return XI_INVALID_TENSOR_ATTR; }
    for (JNode const* x = JOBJ_pair_list(root);
         x!= nullptr; x = x->next) {
        ASSERT0(x->is_pair());
        xoc::Sym const* key = JPAIR_key(x);
        JNode const* val = JPAIR_val(x);
        ASSERT0(val->is_number());
        double number;
        XI_STATUS status = m_jm.extractNumber(val, number);
        if (status != XI_SUCC) {
           xinfer::dumpAct(
               m_rm, "extract %s's value in vocab failed.", key->getStr());
           return status;
        }
        TOKID tokid = (TOKID)number;
        ASSERT0(sizeof(TOKID) <= sizeof(VecIdx));
        m_tokenid2sym.set(tokid, key);
        m_sym2tokenid.set(key, tokid);
    }
    return XI_SUCC;
}


XI_STATUS Tokenizer::extractFromArray(
    xoc::Sym const* key, JNode const* val)
{
    if (key == m_additional_special_tokens) {
        m_sym2specialtoken.set(key, val);
        return XI_SUCC;
    }
    ASSERT0(0); //TODO:
    return XI_UNDEF;
}


XI_STATUS Tokenizer::extractFromObj(
    xoc::Sym const* key, JNode const* val)
{
    if (key == m_added_tokens_decoder) {
        ASSERT0(val->is_obj());
        for (JNode const* x = JOBJ_pair_list(val);
             x!= nullptr; x = x->next) {
            ASSERT0(x->is_pair());
            xoc::Sym const* key = JPAIR_key(x);
            JNode const* val = JPAIR_val(x);
            m_sym2specialtoken.set(key, val);
        }
        return XI_SUCC;
    }
    ASSERT0(0); //TODO:
    return XI_UNDEF;
}


XI_STATUS Tokenizer::extractTokenizerConfig(JNode const* root)
{
    if (!root->is_obj()) { return XI_INVALID_TENSOR_ATTR; }
    for (JNode const* x = JOBJ_pair_list(root);
         x!= nullptr; x = x->next) {
        ASSERT0(x->is_pair());
        xoc::Sym const* key = JPAIR_key(x);
        JNode const* val = JPAIR_val(x);
        switch (JNODE_type(val)) {
        case JN_OBJ:
            extractFromObj(key, val);
            break;
        case JN_PAIR:
            ASSERT0(0); //TODO:
            break;
        case JN_ARRAY:
            extractFromArray(key, val);
            break;
        case JN_STRING:
        case JN_NUMBER:
        case JN_BOOL:
        case JN_NULL:
            m_sym2tokenconf.set(key, val);
            break;
        default: ASSERT0(0); //TODO:
        }
    }
    return XI_SUCC;
}


XI_STATUS Tokenizer::readTokenizer(CHAR const* filename)
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
    m_tokenizer_root = root;
    return XI_SUCC;
}


XI_STATUS Tokenizer::readMergesTxt(CHAR const* filename)
{
    xcom::FO_STATUS st;
    bool is_del = false;
    bool is_readonly = false;
    xcom::FileObj file(filename, is_del, is_readonly, &st);
    if (st != xcom::FO_SUCC) {
        xoc::prt2C("error: open \"%s\" failed.\n", filename);
        return XI_ERR;
    }
    //Read txt.
    size_t filesize = file.getFileSize();
    xcom::StrBuf txtbuf(filesize + 1);
    size_t rd = 0;
    st = file.read((BYTE*)txtbuf.getBuf(), 0, filesize, &rd);
    if (st != xcom::FO_SUCC) {
        xoc::prt2C(
            "error: read %u bytes from '%s' failed.\n", filesize, filename);
        return XI_ERR;
    }
    ASSERT0(rd == filesize);
    return m_mtp.parse(txtbuf.getBuf(), rd);
}


XI_STATUS Tokenizer::readVocab(CHAR const* filename)
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
    return extractVocab(root);
}


XI_STATUS Tokenizer::readConfig(CHAR const* filename)
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
    extractTokenizerConfig(root);
    return XI_SUCC;
}
//END Tokenizer

} //namespace xinfer
