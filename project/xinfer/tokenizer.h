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
#ifndef _TOKENIZER_H_
#define _TOKENIZER_H_

extern "C" typedef struct tagTokenArray TokenArray;

namespace xinfer {

typedef xcom::TMapIter<xoc::Sym const*, JNode const*> Sym2TokenConfIter;
typedef xcom::TMap<xoc::Sym const*, JNode const*> Sym2TokenConf;

typedef xcom::TMapIter<xoc::Sym const*, TOKID> Sym2TokenIdIter;
typedef xcom::TMap<xoc::Sym const*, TOKID> Sym2TokenId;
typedef xcom::Vector<xoc::Sym const*> TokenId2Sym;

class TokenArrayMgr {
protected:
    xcom::List<TokenArray*> m_tokarr_list;
public:
    TokenArrayMgr() {}
    ~TokenArrayMgr();
    TokenArray * allocTokenArray(int64_t array_elem_num);
};

class Tokenizer {
    COPY_CONSTRUCTOR(Tokenizer);
protected:
    xoc::Sym const* m_added_tokens_decoder;
    xoc::Sym const* m_additional_special_tokens;
    JNode const* m_tokenizer_root;
    xoc::RegionMgr * m_rm;
    JNodeMgr & m_jm;
    xoc::ESymTab & m_sym_tab;
    Sym2TokenConf m_sym2specialtoken;
    Sym2TokenConf m_sym2tokenconf;
    Sym2TokenId m_sym2tokenid;
    MergesTxtParser m_mtp;
    xcom::Vector<xoc::Sym const*> m_tokenid2sym;
    xcom::Vector<xoc::Sym const*> m_left_txt;
    xcom::Vector<xoc::Sym const*> m_right_txt;
protected:
    xoc::Sym const* addToSymbolTab(CHAR const* s);

    void convertRawByteStringToUTF8Subword(
        CHAR const* text, MOD xcom::MemPoolMgr & poolmgr,
        OUT xcom::List<CHAR const*> & pieces);

    XI_STATUS extractFromArray(xoc::Sym const* key, JNode const* val);
    XI_STATUS extractFromObj(xoc::Sym const* key, JNode const* val);
    XI_STATUS extractTokenizerConfig(JNode const* root);
    XI_STATUS extractVocab(JNode const* root);
    TokenArray * mapSubwordToTokenId(
        MOD TokenArrayMgr & tokarrmgr, UINT max_token_num,
        xcom::List<CHAR const*> const& pieces);
    void mergeSubwordByBPE(
        MOD xcom::MemPoolMgr & poolmgr, MOD xcom::List<CHAR const*> & pieces);
public:
    Tokenizer(xoc::RegionMgr * rm, JNodeMgr & jm, xoc::ESymTab & sym_tab);
    ~Tokenizer();
    void dumpVocab() const;
    void dumpConfig() const;
    void dump() const;

    //Decode token and output the corresponding UTF8 string.
    void decode(TOKID token, OUT xcom::StrBuf & output,
                bool output_utf8 = false);

    //text: input prompt, encoded by utf-8.
    TokenArray * encode(
        CHAR const* text, UINT max_token_num, TokenArrayMgr & tokarrmgr);
    TOKID encode(CHAR text);

    xoc::RegionMgr * getRegionMgr() const { return m_rm ; }
    JNodeMgr & getJNodeMgr() { return m_jm; }
    xoc::ESymTab & getSymTab() { return m_sym_tab; }
    MergesTxtParser const& getMergesTxtParser() const { return m_mtp; }
    Sym2TokenId const& getSym2TokenId() const { return m_sym2tokenid; }
    TokenId2Sym const& getTokenId2Sym() const { return m_tokenid2sym; }

    XI_STATUS readMergesTxt(CHAR const* filename);
    XI_STATUS readConfig(CHAR const* filename);
    XI_STATUS readTokenizer(CHAR const* filename);
    XI_STATUS readVocab(CHAR const* filename);
};

} //namespace xinfer

#endif
