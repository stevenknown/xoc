/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com
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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _IR_LEX_
#define _IR_LEX_

namespace xoc {

#define ST_ERR      1 //Status if error occur.
#define ST_SUCC     0 //Status if successful.
#define ST_EOF      2 //Status meet End-Of-File.

typedef enum _TOKEN {
    T_NUL = 0,      // NULL
    T_IDENTIFIER,   // (A-Z|a-z)( A-Z|a-z|0-9 )*
    T_IMM,          // 0~9
    T_IMML,         // 0~9L
    T_IMMU,         // Unsigned
    T_IMMUL,        // Unsigned Long
    T_FP,           // double type decimal e.g 3.14
    T_FPF,          // float type decimal e.g 3.14
    T_FPLD,         // long double type decimal e.g 3.14
    T_STRING,       // "abcd"
    T_CHAR_LIST,    // 'abcd'
    T_INTRI_FUN,    // intrinsic function call
    T_INTRI_VAL,    // intrinsic value
    T_LLPAREN,      // {
    T_RLPAREN,      // }
    T_LSPAREN,      // [
    T_RSPAREN,      // ]
    T_ASSIGN,       // =
    T_LPAREN,       // (
    T_RPAREN,       // )
    T_ADD,          // +
    T_SUB,          // -
    T_ASTERISK,     // *
    T_DIV,          // /
    T_AND,          // &&
    T_BITANDEQU,    // &=
    T_OR,           // ||
    T_AT,           // @
    T_BITAND,       // &
    T_BITOR,        // |
    T_BITOREQU,     // |=
    T_LESSTHAN,     // <
    T_MORETHAN,     // >
    T_RSHIFT,       // >>
    T_RSHIFTEQU,    // >>=
    T_LSHIFT,       // <<
    T_LSHIFTEQU,    // <<=
    T_NOMORETHAN,   // <=
    T_NOLESSTHAN,   // >=
    T_NOEQU,        // !=
    T_NOT,          // !
    T_EQU,          // ==
    T_ADDEQU,       // +=
    T_SUBEQU,       // -=
    T_MULEQU,       // *=
    T_DIVEQU,       // /=
    T_XOR,          // ^
    T_XOREQU,       // ^=
    T_REMEQU,       // %=
    T_MOD,          // %
    T_COLON,        // :
    T_SEMI,         // ;
    T_QUOT,         // "
    T_COMMA,        // ,
    T_UNDERLINE,    // _
    T_LANDSCAPE,    // -
    T_REV,          // ~ reverse  e.g:a = ~a
    T_DOT,          // .
    T_QUES_MARK,    // ?
    T_ARROW,        // ->
    T_ADDADD,       // ++
    T_SUBSUB,       // --
    T_DOTDOTDOT,    // ...

    //scalar-type-spec
    T_CHAR,
    T_SHORT,
    T_INT,
    T_LONG,
    T_FLOAT,
    T_DOUBLE,
    T_SIGNED,
    T_UNSIGNED,
    T_LONGLONG,

    //boolean
    T_TRUE,
    T_FALSE,

    //struct-or-union
    T_STRUCT,
    T_UNION,

    //storage-class-spec
    T_AUTO,
    T_REGISTER,
    T_EXTERN,
    T_INLINE,
    T_STATIC,
    T_TYPEDEF,

    //qualifiers-pass
    T_CONST,
    T_VOLATILE,
    T_RESTRICT,

    //unary-operator
    T_SIZEOF,
    T_ENUM,

    //pragma
    T_SHARP,        // #
    T_PRAGMA,       // pragma

    T_NEWLINE,      // \n
    T_DOLLAR,       // $
    ////////////////////////////////////
    //DO NOT ADD Enum AFTER THIS LINE.//
    ////////////////////////////////////
    T_END,        // end of file
} TOKEN;


#define TOKEN_INFO_name(ti)     (ti)->name
#define TOKEN_INFO_token(ti)    (ti)->tok
#define TOKEN_INFO_lineno(ti)   (ti)->u1.lineno
class TokenInfo {
public:
    TOKEN tok;
    CHAR const* name;
    union{
        INT  lineno;
    } u1;
};


#define KEYWORD_INFO_name(ti)   (ti)->name
#define KEYWORD_INFO_token(ti)  (ti)->tok
class KeywordInfo {
public:
    TOKEN tok;
    CHAR const* name;
};

//The maximum byte size for each load from source file.
#define MAX_BUF_LINE            4096
#define MAX_OFST_BUF_LEN        1024

class String2Token : public HMap<CHAR const*, TOKEN, HashFuncString2> {
public:
    String2Token(UINT bsize) : HMap<CHAR const*, TOKEN, HashFuncString2>(bsize) {}
    virtual ~String2Token() {}
};


//record each error msg
class LexErrorMsg {
public:
    CHAR * msg;
    UINT lineno;
};


class Lexer {
protected:
    SMemPool * m_pool;
    TOKEN m_cur_token;

    //The buffer that hold the token content.
    CHAR * m_cur_token_string; //for local used.
    UINT m_cur_token_string_len;
    UINT m_cur_token_string_pos;

    //Indicate current processing character during parsing.
    CHAR m_cur_char;

    //Set true to regard '\n' as token.
    bool m_enable_newline_token;

    //Set true if source line is end with the character '0xd' and '0xa'.
    bool m_is_dos;

    //Inpute source file handler.
    FILE * m_src_file;

    //Record the line number that may be backed tracking from the newest line.
    UINT m_real_line_num;

    //Record the newest line number of src file.
    UINT m_src_line_num;

    //Current parsing line of src file
    CHAR * m_cur_line;

    //Length of current parsing line buffer.
    UINT m_cur_line_len;

    //Record byte offset in src file of each parsed lines.
    LONG * m_ofst_tab;

    //Record length of offset table.
    LONG m_ofst_tab_byte_size;

    //Set true if need lexer to recognize the true and false token.
    bool m_enable_true_false_token;

    //Record current byte offset in src file.
    UINT m_cur_src_ofst;

    //Set true to return the newline charactors as normal character.
    bool m_use_newline_char;

    //Current position in m_cur_line.
    UINT m_cur_line_pos;

    //Length of m_cur_line.
    UINT m_cur_line_num;

    //Buffer to hold the prefected byte from src file.
    CHAR m_file_buf[MAX_BUF_LINE];

    //Current position in m_file_buf.
    UINT m_file_buf_pos;

    //Record the actually byte size that read from src file.
    UINT m_last_read_num;

    //String2Token m_str2token;
    TMap<CHAR const*, TOKEN, CompareStringFunc> m_str2token;
    List<LexErrorMsg*> m_err_msg_list;

protected:
    void initKeyWordTab();

    void checkAndGrowCurTokenString();

    TOKEN getKeyWord(CHAR const* s)
    {
        if (s == NULL) { return T_NUL; }
        return m_str2token.get(s);
    }
    INT getLine();
    CHAR getNextChar();

    TOKEN t_num();
    TOKEN t_string();
    TOKEN t_char_list();
    TOKEN t_id();
    TOKEN t_solidus();
    TOKEN t_dot();

    void * xmalloc(size_t size)
    {
        ASSERTN(m_pool, ("not yet initialized."));
        void * p = smpoolMallocConstSize(size, m_pool);
        ASSERTN(p, ("malloc failed"));
        ::memset(p, 0, size);
        return p;
    }
public:
    Lexer()
    {
        m_enable_newline_token = false;
        m_is_dos = true;
        m_enable_true_false_token = true;
        m_use_newline_char = true;
        m_src_file = NULL;
        m_cur_line = NULL;
        m_cur_line_len = 0;
        m_ofst_tab = NULL;
        m_ofst_tab_byte_size = 0;
        m_file_buf_pos = MAX_BUF_LINE;
        initKeyWordTab();
        m_pool = smpoolCreate(0, MEM_COMM);
        m_cur_token_string = (CHAR*)::malloc(MAX_BUF_LINE);
        m_cur_token_string_len = MAX_BUF_LINE;
        clean();
    }
    COPY_CONSTRUCTOR(Lexer);
    ~Lexer()
    {
        if (m_ofst_tab != NULL) {
            ::free(m_ofst_tab);
            m_ofst_tab = NULL;
        }
        if (m_cur_line != NULL) {
            ::free(m_cur_line);
            m_cur_line = NULL;
        }
        smpoolDelete(m_pool);
        ::free(m_cur_token_string);
        m_cur_token_string_len = 0;
    }

    void clean()
    {
        m_cur_token_string_pos = 0;
        m_cur_token = T_NUL;
        m_src_line_num = 0;
        if (m_cur_line != NULL) {
            m_cur_line[0] = 0;
        }
        if (m_cur_token_string != NULL) {
            m_cur_token_string[0] = 0;
        }
        m_file_buf[0] = 0;
        m_real_line_num = 0;
        m_cur_src_ofst = 0;
        m_cur_line_pos = 0;
        m_cur_line_num = 0;
        m_file_buf_pos = MAX_BUF_LINE;
        m_last_read_num = 0;
        m_cur_char = 0;
    }

    //Dump token string.
    //inputfile: input source file
    //outputfile: dump token to output file
    void dump(CHAR const* input, FILE * output);

    TOKEN getNextToken();
    CHAR const* getTokenName(TOKEN tok) const;
    UINT getCurrentLineNum() const { return m_src_line_num; }
    CHAR const* getCurrentTokenString() const { return m_cur_token_string; }
    TOKEN getCurrentToken() const { return m_cur_token; }
    UINT getOffsetTabLineNum() const
    { return m_ofst_tab_byte_size / sizeof(LONG); }

    //Report error with line number.
    void error(UINT line_num, CHAR const* msg, ...);

    //Set the file handler which need to parse.
    void setSrcFile(FILE * h) { m_src_file = h; }
};

} //namespace xoc
#endif
