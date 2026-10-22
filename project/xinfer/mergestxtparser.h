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
#ifndef _MERGESTXTPARSER_H_
#define _MERGESTXTPARSER_H_

namespace xoc {
enum _TOKEN : int;
typedef enum _TOKEN TOKEN;
}

namespace xinfer {

#define MIN_RANK 0
#define MAX_RANK ((UINT)-1)

typedef xcom::TMapIter<xoc::Sym const*, UINT> Sym2RankIter;
typedef xcom::TMap<xoc::Sym const*, UINT> Sym2Rank;

class MergesTxtParser {
protected:
    xoc::LogMgr * m_lm;
    xcom::SMemPool * m_pool;
    xoc::ESymTab & m_sym_tab;
    xcom::Vector<CHAR const*> m_left;
    xcom::Vector<CHAR const*> m_right;
    Sym2Rank m_sym2rank;
protected:
    xoc::Sym const* addToSymbolTab(CHAR const* s);
    void * xmalloc(UINT size);
    CHAR const* dupStr(CHAR const* str, UINT slen);
    XI_STATUS parseByCommonLexer(CHAR const* txtbuf, UINT txtlen);
    XI_STATUS parseBySpecLex(MOD CHAR * txtbuf, UINT txtlen);
    xoc::TOKEN skipComments(xoc::Lexer & lexer);
public:
    MergesTxtParser(xoc::LogMgr * lm, xoc::ESymTab & sym_tab);
    ~MergesTxtParser();
    void dump() const;
    Sym2Rank const& getSym2Rank() const { return m_sym2rank; }
    XI_STATUS parse(MOD CHAR * txtbuf, UINT txtlen);
};

} //namespace xinfer

#endif
