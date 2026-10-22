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
//START MergesTxtParser
//
MergesTxtParser::MergesTxtParser(xoc::LogMgr * lm, xoc::ESymTab & sym_tab)
    : m_sym_tab(sym_tab)
{
    m_lm = lm;
    m_pool = xcom::smpoolCreate(64, MEM_COMM);
}


MergesTxtParser::~MergesTxtParser()
{
    xcom::smpoolDelete(m_pool);
    m_pool = nullptr;
}
 

void MergesTxtParser::dump() const
{
    if (m_lm == nullptr || !m_lm->is_init()) { return; }
    note(m_lm, "\n-- MergesTxtParser --");
    m_lm->incIndent(2);
    note(m_lm, "\n---- LEFT RIGH PAIR:TOTAL:%u ----", m_left.get_elem_count());
    for (UINT i = 0; i < m_left.get_elem_count(); i++) {
        CHAR const* l = m_left.get(i);
        CHAR const* r = m_right.get(i);
        note(m_lm, "\n");
        prt(m_lm, "L:%s", l != nullptr ? l : "NULL");
        prt(m_lm, " R:%s", r != nullptr ? r : "NULL");
    }
    note(m_lm, "\n---- MergePair2Rank:TOTAL:%u ----",
         m_sym2rank.get_elem_count());
    Sym2RankIter it;
    UINT rank;
    for (xoc::Sym const* sym = m_sym2rank.get_first(it, &rank);
         sym != nullptr; sym = m_sym2rank.get_next(it, &rank)) {
        note(m_lm, "\n%s:%u", sym->getStr(), rank);
    }
    m_lm->decIndent(2);
}


CHAR const* MergesTxtParser::dupStr(CHAR const* str, UINT slen)
{
    ASSERT0(str && slen > 0);
    CHAR * newstr = (CHAR*)xmalloc(slen + 1);
    ::memcpy(newstr, str, slen);
    return newstr;
}


xoc::TOKEN MergesTxtParser::skipComments(xoc::Lexer & lexer)
{
    ASSERT0(lexer.getCurrentToken() == T_SHARP);
    xoc::TOKEN tok = lexer.getNextToken();
    while (tok != T_NEWLINE && tok != T_END && tok != T_UNDEF) {
        tok = lexer.getNextToken();
    }
    if (tok == T_NEWLINE) {
        //Parsing is successful, prepare the next-token for following parsing.
        tok = lexer.getNextToken();
    }
    return tok;
}


xoc::Sym const* MergesTxtParser::addToSymbolTab(CHAR const* s)
{
    return m_sym_tab.add(s);
}


void * MergesTxtParser::xmalloc(UINT size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = xcom::smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset((void*)p, 0, size);
    return p;
}


XI_STATUS MergesTxtParser::parseByCommonLexer(CHAR const* txtbuf, UINT txtlen)
{
    xoc::Lexer lexer;
    lexer.setSrcBuf(txtbuf, txtlen);
    lexer.setEnableOffsetTab(false);
    lexer.setEnableNewLine(true);
    lexer.setEnableString(false);
    lexer.setEnableCharList(false);
    lexer.setEnableSpace(true);
    UINT max_line_in_merges_txt = 65535;
    xcom::FixedStrBuf<16> tmpbuf;
    UINT i;
    for (i = 0; i < max_line_in_merges_txt; i++) {
        xoc::TOKEN tokl = lexer.getNextToken();
        if (tokl == T_END) { return XI_SUCC; }
        if (tokl == T_SHARP) {
            //Merges.txt uses # as comment-line-header.
            tokl = skipComments(lexer);
        }
        if (tokl == T_END) { return XI_SUCC; }
        if (tokl == T_UNDEF) {
            note(m_lm, "\nparse merges txt left-value failed at line:%u",
                 lexer.getCurrentLineNum());
            return XI_ERR;
        }
        CHAR const* l = dupStr(
            lexer.getCurrentTokenString(),
            lexer.getCurrentTokenStringLen());
 
        xoc::TOKEN tokb = lexer.getNextToken();
        if (tokb != T_SPACE) {
            //Combine tokens into one string buffer.
            tmpbuf.clean();
            tmpbuf.strcat("%s", l);
            while (tokb != T_SPACE) {
                if (tokb == T_END || tokb == T_UNDEF) {
                    note(m_lm,
                         "\nparse merges txt right-value failed at line:%u",
                         lexer.getCurrentLineNum());
                    return XI_ERR;
                }
                tmpbuf.strcat("%s", lexer.getCurrentTokenString());
                tokb = lexer.getNextToken();
            }
            ASSERT0(tokb == T_SPACE);
            //Here, skip the space.
            CHAR const* strbuf = tmpbuf.getBuf();
            l = dupStr(strbuf, ::strlen(strbuf));
        }

        xoc::TOKEN tokr = lexer.getNextToken();
        if (tokr == T_END || tokr == T_UNDEF) {
            note(m_lm, "\nparse merges txt right-value failed at line:%u",
                 lexer.getCurrentLineNum());
            return XI_ERR;
        }
        CHAR const* r = dupStr(
            lexer.getCurrentTokenString(),
            lexer.getCurrentTokenStringLen());

        xoc::TOKEN tokn = lexer.getNextToken();
        if (tokn != T_NEWLINE) {
            //Combine tokens into one string buffer.
            tmpbuf.clean();
            tmpbuf.strcat("%s", r);
            while (tokn != T_NEWLINE) {
                if (tokn == T_END || tokn == T_UNDEF) {
                    note(m_lm,
                         "\nparse merges txt right-value failed at line:%u",
                         lexer.getCurrentLineNum());
                    return XI_ERR;
                }
                tmpbuf.strcat("%s", lexer.getCurrentTokenString());
                tokn = lexer.getNextToken();
            }
            ASSERT0(tokn == T_NEWLINE);
            //Here, skip the newline.
            CHAR const* strbuf = tmpbuf.getBuf();
            r = dupStr(strbuf, ::strlen(strbuf));
        }

        m_left.set(i, l);
        m_right.set(i, r);

        tmpbuf.sprint("%s %s", l, r);
        xoc::Sym const* mergepair = addToSymbolTab(tmpbuf.getBuf());
        ASSERT0(mergepair);
        m_sym2rank.set(mergepair, i);
    }
    ASSERTN(i < max_line_in_merges_txt, ("merges.txt is too big"));
    return XI_SUCC;
}


//NOTE:the function will perform in-place operations to 'txtbuf'.
//The parser is dedicated to merges.txt and thus it is very fast.
XI_STATUS MergesTxtParser::parseBySpecLex(MOD CHAR * txtbuf, UINT txtlen)
{
    ASSERT0(txtbuf && txtlen > 0);
    CHAR * ptr = txtbuf;
    CHAR * bufend = ptr + txtlen;
    xcom::VecIdx i = 0;
    while (ptr < bufend) {
        CHAR * line_end = ::strchr(ptr, '\n');
        if (line_end == nullptr) { break; }
        CHAR * line_start = ptr;

        size_t line_len = line_end - ptr;

        //trim trailing \r
        while (line_len > 0 && ptr[line_len - 1] == '\r') {
            line_len--;
        }

        if (line_len == 0) {
            //Goto next line start.
            ptr = line_end + 1;
            continue;
        }

        // skip comment line start with '#'
        if (ptr[0] == '#' && i == 0) {
            //NOTE:In merges.txt, only the first line is comment.
            //Allow '# #' to be the pair.
            //Goto next line start.
            ptr = line_end + 1;
            continue;
        }

        CHAR org_line_end_char = *line_end;

        //Set string terminator.
        //To facilitate string duplication.
        *line_end = 0;
        CHAR * org_space_ptr = ::strchr(ptr, ' ');
        if (org_space_ptr != nullptr) {
            //Set string terminator.
            //To facilitate string duplication.
            *org_space_ptr = 0;

            CHAR const* left = ptr;
            UINT left_len = org_space_ptr - left;
            CHAR const* right = org_space_ptr + 1;
            UINT right_len = line_end - right;

            //Allocate string memory from smpool
            CHAR const* l = dupStr(left, left_len);
            CHAR const* r = dupStr(right, right_len);
            ASSERT0(l && r);
            m_left.set(i, l);
            m_right.set(i, r);

            //Restore original space char.
            *org_space_ptr = ' ';

            xoc::Sym const* mergepair = addToSymbolTab(line_start);
            ASSERT0(mergepair);
            m_sym2rank.set(mergepair, i);

            i++;
        }

        //Restore original line-end char.
        *line_end = org_line_end_char;

        //Goto next line start.
        ptr = line_end + 1;
    }
    return XI_SUCC;
}


//Parse gpt2 merges.txt, load bpe merge pairs.
//All char string memory are allocated from input smpool.
//merge string pointers are stored inside xcom::Vector<char*>.
//Do NOT use malloc/realloc/free inside this function.
//
//filename: input merges.txt file path.
//pool: memory pool for real string char buffer, caller create/destroy pool.
//out_left_vec: output vector stores left string pointers.
//out_right_vec: output vector stores right string pointers.
//Return XI_SUCC for success, else error status.
XI_STATUS MergesTxtParser::parse(MOD CHAR * txtbuf, UINT txtlen)
{
    bool use_specific_method = true;
    if (use_specific_method) {
        return parseBySpecLex(txtbuf, txtlen);
    }
    return parseByCommonLexer(txtbuf, txtlen);
}
//END MergesTxtParser

} //namespace xinfer
