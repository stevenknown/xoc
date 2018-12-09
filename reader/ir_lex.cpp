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
#include "../opt/cominc.h"
#include "../com/xcominc.h"
#include "ir_lex.h"

namespace xoc {

//Make sure following Tokens or Keywords is consistent with
//declarations of TOKEN enumeration declared in lex.h.
//CAVEAT: The order of tokens must be consistent
//with declarations order in lex.h.
static TokenInfo g_token_info[] =
{
    { T_NUL,        ""                   },
    { T_IDENTIFIER, "identifier"         },
    { T_IMM,        "imm"               },
    { T_IMML,       "long imm"          },
    { T_IMMU,       "unsigned imm"      },
    { T_IMMUL,      "unsigned long imm" },
    { T_FP,         "double decimal"     },
    { T_FPF,        "float decimal"      },
    { T_FPLD,       "long double decimal"},
    { T_STRING,     "string"             },
    { T_CHAR_LIST,  "char list"          },
    { T_INTRI_FUN,  ""                   },
    { T_INTRI_VAL,  ""                   },
    { T_LLPAREN,    "{"                  },
    { T_RLPAREN,    "}"                  },
    { T_LSPAREN,    "["                  },
    { T_RSPAREN,    "]"                  },
    { T_ASSIGN,     "="                  },
    { T_LPAREN,     "("                  },
    { T_RPAREN,     ")"                  },
    { T_ADD,        "+"                  },
    { T_SUB,        "-"                  },
    { T_ASTERISK,   "*"                  },
    { T_DIV,        "/"                  },
    { T_AND,        "&&"                 },
    { T_BITANDEQU,  "&="                 },
    { T_OR,         "||"                 },
    { T_AT,         "@"                  },
    { T_BITAND,     "&"                  },
    { T_BITOR,      "|"                  },
    { T_BITOREQU,    "|="                },
    { T_LESSTHAN,    "<"                 },
    { T_MORETHAN,    ">"                 },
    { T_RSHIFT,      ">>"                },
    { T_RSHIFTEQU,   ">>="               },
    { T_LSHIFT,      "<<"                },
    { T_LSHIFTEQU,   "<<="               },
    { T_NOMORETHAN,  "<="                },
    { T_NOLESSTHAN,  ">="                },
    { T_NOEQU,       "!="                },
    { T_NOT,         "!"                 },
    { T_EQU,         "=="                },
    { T_ADDEQU,      "+="                },
    { T_SUBEQU,      "-="                },
    { T_MULEQU,      "*="                },
    { T_DIVEQU,      "/="                },
    { T_XOR,         "^"                 },
    { T_XOREQU,      "^="                },
    { T_REMEQU,      "%="                },
    { T_MOD,         "%"                 },
    { T_COLON,       ":"                 },
    { T_SEMI,        ";"                 },
    { T_QUOT,        "\""                },
    { T_COMMA,       ","                 },
    { T_UNDERLINE,   "_"                 },
    { T_LANDSCAPE,   "-"                 },
    { T_REV,         "~"                 },
    { T_DOT,         "."                 },
    { T_QUES_MARK,   "?"                 },
    { T_ARROW,       "->"                },
    { T_ADDADD,      "++"                },
    { T_SUBSUB,      "--"                },
    { T_DOTDOTDOT,   "..."               },

    //scalar-type-spec
    { T_CHAR,        "char"              },
    { T_SHORT,       "short"             },
    { T_INT,         "int"               },
    { T_LONG,        "long"              },
    { T_FLOAT,       "float"             },
    { T_DOUBLE,      "double"            },
    { T_SIGNED,      "signed"            },
    { T_UNSIGNED,    "unsigned"          },
    { T_LONGLONG,    "longlong"          },

    { T_TRUE,        "true"              },
    { T_FALSE,       "false"             },

    //struct-or-union
    { T_STRUCT,      "struct"            },
    { T_UNION,       "union"             },

    //storage-class-spec
    { T_AUTO,        "auto"              },
    { T_REGISTER,    "register"          },
    { T_EXTERN,      "extern"            },
    { T_INLINE,      "inline"            },
    { T_STATIC,      "static"            },
    { T_TYPEDEF,     "typedef"           },

    //qualifiers-pass
    { T_CONST,       "const"             },
    { T_VOLATILE,    "volatile"          },
    { T_RESTRICT,    "restrict"          },

    //unary-operator
    { T_SIZEOF,      "sizeof"            },
    { T_ENUM,        "enum"              },

    //pargma
    { T_SHARP,       "#"                 },
    { T_PRAGMA,      "pragma"            },

    { T_NEWLINE,     "\\n"               },
    { T_DOLLAR,      "$"                 },

    ///////////////////////////////////////
    //DO NOT ADD Enum AFTER THIS LINE./////
    ///////////////////////////////////////
    { T_END,         ""                  },
};


//Define keywords of XOC IR.
static KeywordInfo g_keyword_info[] = {
    //scalar-type-spec
    { T_CHAR,        "char"              },
    { T_SHORT,       "short"             },
    { T_INT,         "int"               },
    { T_LONG,        "long"              },
    { T_FLOAT,       "float"             },
    { T_DOUBLE,      "double"            },
    { T_SIGNED,      "signed"            },
    { T_UNSIGNED,    "unsigned"          },
    { T_LONGLONG,    "longlong"          },
    { T_TRUE,        "true"              },
    { T_FALSE,       "false"             },

    //struct-or-union
    { T_STRUCT,      "struct"            },
    { T_UNION,       "union"             },

    //storage-class-spec
    { T_AUTO,        "auto"              },
    { T_REGISTER,    "register"          },
    { T_EXTERN,      "extern"            },
    { T_INLINE,      "inline"            },
    { T_STATIC,      "static"            },
    { T_TYPEDEF,     "typedef"           },

    //qualifiers-pass
    { T_CONST,       "const"             },
    { T_VOLATILE,    "volatile"          },
    { T_RESTRICT,    "restrict"          },

    //unary-operator
    { T_SIZEOF,      "sizeof"            },
    { T_ENUM,        "enum"              },

    //pragma
    { T_SHARP,       "#"                 },
    { T_PRAGMA,      "pragma"            },
    { T_NEWLINE,     "\\n"               },
};


static UINT g_keyword_num = sizeof(g_keyword_info)/sizeof(g_keyword_info[0]);


//Report error with line number.
void Lexer::error(UINT line_num, CHAR const* msg, ...)
{
    if (msg == NULL) { return; }
    LexErrorMsg * p = NULL;
    StrBuf sbuf(64);
    va_list arg;
    va_start(arg, msg);
    sbuf.vsprint(msg, arg);
    p = (LexErrorMsg*)xmalloc(sizeof(LexErrorMsg));
    p->msg = (CHAR*)xmalloc(sbuf.strlen() + 1);
    ::memcpy(p->msg, sbuf.buf, sbuf.strlen() + 1);
    p->lineno = line_num;
    m_err_msg_list.append_tail(p);
    va_end(arg);
}


//This function read a line from source code buffer.
//Return status, which could be ST_SUCC or ST_ERR.
INT Lexer::getLine()
{
    //Initializing or realloc offset table.
    if (m_ofst_tab == NULL) {
        m_ofst_tab_byte_size = MAX_OFST_BUF_LEN * sizeof(LONG);
        m_ofst_tab = (LONG*)::malloc(m_ofst_tab_byte_size);
        ::memset(m_ofst_tab, 0, m_ofst_tab_byte_size);
    } else if (getOffsetTabLineNum() < (m_src_line_num + 10)) {
        m_ofst_tab = (LONG*)::realloc(m_ofst_tab,
            m_ofst_tab_byte_size + MAX_OFST_BUF_LEN * sizeof(LONG));
        ::memset(((BYTE*)m_ofst_tab) + m_ofst_tab_byte_size,
            0, MAX_OFST_BUF_LEN * sizeof(LONG));
        m_ofst_tab_byte_size += MAX_OFST_BUF_LEN * sizeof(LONG);
    }

    UINT pos = 0;
    bool is_some_chars_in_cur_line = false;
    for (;;) {
        if (m_cur_line == NULL) {
            m_cur_line = (CHAR*)::malloc(MAX_BUF_LINE);
            m_cur_line_len = MAX_BUF_LINE;
            if (m_cur_line == NULL) {
                goto FAILED;
            }
        }

        //Read MAX_BUF_LINE characters from src file.
        if (m_file_buf_pos >= m_last_read_num) {
            ASSERT0(m_src_file != NULL);
            INT dw = (INT)fread(m_file_buf, 1, MAX_BUF_LINE, m_src_file);
            if (dw == 0) {
                if (!is_some_chars_in_cur_line) {
                    //Some characters had been put into 'm_cur_line',
                    //but the last character of 'm_file_buf' is not
                    //'0xD,0xA', so we hould to get there.
                    //However there is nothing more can be read from file,
                    //thus 'dw' is zero. This situation may take place
                    //at that we meet the file that terminate without
                    //'0xD,0xA'.
                    //TODO:Take a consideration of the corner case, we
                    //should process the last characters in 'm_cur_line'
                    //properly and correctly rather than return 'FEOF' directly.
                    goto FEOF;
                } else {
                    goto FIN;
                }
            } else {
                m_last_read_num = dw;
            }
            m_last_read_num = MIN(m_last_read_num, MAX_BUF_LINE);
            m_file_buf_pos = 0;
        }

        //Get one line characters from buffer which end up
        //with '0xd,0xa' in DOS or '0xa' in Linux.
        bool is_0xd_recog = false;
        while (m_file_buf_pos < m_last_read_num) {
            if (m_file_buf[m_file_buf_pos] == 0xd &&
                m_file_buf[m_file_buf_pos + 1] == 0xa) { //DOS ending characters
                m_is_dos = true;
                if (m_use_newline_char) {
                    m_cur_line[pos] = m_file_buf[m_file_buf_pos];
                    pos++;
                    m_file_buf_pos++;
                    m_cur_line[pos] = m_file_buf[m_file_buf_pos];
                    pos++;
                    m_file_buf_pos++;
                } else {
                    m_file_buf_pos += 2;
                }
                m_cur_src_ofst += 2;
                m_src_line_num++;
                goto FIN;
            } else if (m_file_buf[m_file_buf_pos] == 0xa) { //UNIX ending
                if (is_0xd_recog) {
                    //We have met '0xd', the '0xa' is one of
                    //the terminate string '0xd,0xa' under DOS text format.
                    if (m_use_newline_char) {
                        m_cur_line[pos] = m_file_buf[m_file_buf_pos];
                        pos++;
                        m_file_buf_pos++;
                    } else {
                        m_file_buf_pos++; //omit the terminate charactor '0xa'
                    }
                    is_0xd_recog = false;
                } else {
                    m_is_dos = false;
                    if (m_use_newline_char) {
                        m_cur_line[pos] = m_file_buf[m_file_buf_pos];
                        pos++;
                        m_file_buf_pos++;
                    } else {
                        m_file_buf_pos ++;
                    }
                }
                m_cur_src_ofst++;
                m_src_line_num++;
                goto FIN;
            } else if(m_file_buf[m_file_buf_pos] == 0xd && m_is_dos) {
                //0xd is the last charactor in 'm_file_buf', thus 0xa
                //should be recognized in getNextToken() in order to correct
                //the lex parsing.
                is_0xd_recog = 1;
                if (m_use_newline_char) {
                    m_cur_line[pos] = m_file_buf[m_file_buf_pos];
                    pos++;
                    m_file_buf_pos++;
                }else{
                    m_file_buf_pos++;
                }
                m_cur_src_ofst++;
                goto FIN;
            }

            if (pos >= m_cur_line_len) {
                //Escalate line buffer.
                m_cur_line_len += MAX_BUF_LINE;
                m_cur_line = (CHAR*)::realloc(m_cur_line, m_cur_line_len);
            }
            m_cur_line[pos] = m_file_buf[m_file_buf_pos];
            pos++;
            m_file_buf_pos++;
            is_some_chars_in_cur_line = true;
            m_cur_src_ofst++;
        } //end while m_file_buf_pos
    }
FIN:
    ASSERT0((m_src_line_num + 1) < getOffsetTabLineNum());
    m_ofst_tab[m_src_line_num + 1] = m_cur_src_ofst;
    m_cur_line[pos] = 0;
    m_cur_line_num = (UINT)::strlen(m_cur_line);
    m_cur_line_pos = 0;
    return ST_SUCC;

FAILED:
    return ST_ERR;

FEOF:
    m_src_line_num++;
    m_cur_line[pos] = 0;
    m_cur_line_num = 0;
    m_cur_line_pos = 0;
    return ST_EOF;
}


void Lexer::initKeyWordTab()
{
    //m_str2token.init(64); //Must be power of 2 since we use HashFuncString2.
    for (UINT i = 0; i < g_keyword_num; i++) {
        m_str2token.set(KEYWORD_INFO_name(&g_keyword_info[i]),
                        KEYWORD_INFO_token(&g_keyword_info[i]));
    }
}


//Get a charactor from m_cur_line , and if it meet the end of src file,
//the return value is -1, otherwise the ASCI charactor.
CHAR Lexer::getNextChar()
{
    CHAR res = '0';
    INT st = 0;
    if (m_cur_line == NULL) {
        if ((st = getLine()) == ST_SUCC) {
            res = m_cur_line[m_cur_line_pos];
            m_cur_line_pos++;
        } else if(st == ST_EOF) {
            res = ST_EOF;
        }
    } else if (m_cur_line_pos < m_cur_line_num) {
        res = m_cur_line[m_cur_line_pos];
        m_cur_line_pos++;
    } else {
        st = getLine();
        if (st == ST_SUCC) {
            do {
                res = m_cur_line[m_cur_line_pos];
                m_cur_line_pos++;
                if (m_cur_line_num != 0) {
                    //Skip the empty line.
                    break;
                }
                st = getLine();
            } while (st == ST_SUCC);
        } else if (st == ST_EOF) {
              res = ST_EOF;
        }
   }
   return res;
}


///////////////////////////////////////////////////////////////////////
//You should construct the following function accroding to your lexical
//token word.
//
//START HERE.
///////////////////////////////////////////////////////////////////////

//'m_cur_char' hold the current charactor right now.
//You should assign 'm_cur_char' the next valid charactor before
//the function return.
TOKEN Lexer::t_num()
{
    CHAR c = getNextChar();
    CHAR b_is_fp = 0;
    TOKEN t = T_NUL;
    if (m_cur_char == '0' && (c == 'x' || c == 'X')) {
        //hex
        m_cur_token_string[m_cur_token_string_pos++] = c;
        while (xisdigithex(c = getNextChar())) {
            m_cur_token_string[m_cur_token_string_pos++] = c;
        }
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = c;
        t = T_IMM;
        goto FIN;
    }

    if (xisdigit(c) || c == '.') {
        //'c' is decimal.
        if (c == '.') {
            b_is_fp = 1;
        }
        m_cur_token_string[m_cur_token_string_pos++] = c;
         if (b_is_fp) { //there is already present '.'
            while (xisdigit(c = getNextChar())) {
                m_cur_token_string[m_cur_token_string_pos++] = c;
            }
         } else {
             while (xisdigit(c = getNextChar()) || c == '.') {
                if (c == '.') {
                    if (!b_is_fp){
                        b_is_fp=1;
                    } else {
                        break;
                    }
                }
                m_cur_token_string[m_cur_token_string_pos++] = c;
            } //end while
         } //end else
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = c;
        if (b_is_fp) { t = T_FP; }
        else { t = T_IMM; }
    } else {
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = c;
        t = T_IMM; //t is '0','1','2','3','4','5','6','7','8','9'
    }

FIN:
    if (m_cur_char == 'L' || m_cur_char == 'l') {
        //e.g: 1000L
        //t is long integer.
        if (t == T_IMM) {
            t = T_IMML;
            m_cur_char = getNextChar();
            if (m_cur_char == 'L' || m_cur_char == 'l') {
                //e.g: 1000LL
                m_cur_char = getNextChar();
                if (m_cur_char == 'U' || m_cur_char == 'u') {
                    //e.g: 1000LLU
                    m_cur_char = getNextChar();
                }
            } else if (m_cur_char == 'U' || m_cur_char == 'u') {
                //e.g: 1000LU
                m_cur_char = getNextChar();
                t = T_IMMUL;
            }
        } else if (t == T_FP) {
            //If suffixed by the letter l or L, it has type long double.
            m_cur_char = getNextChar();
            t = T_FPLD;
        }
    } else if (m_cur_char == 'U' || m_cur_char == 'u') {
        //imm is unsigned.
        m_cur_char = getNextChar();
        t = T_IMMU;
    } else if (m_cur_char == 'F' || m_cur_char == 'f') {
        //e.g: 1.0F, float
        if (t == T_IMM) {
            error(m_real_line_num, "invalid suffix \"%c\" on integer constant",
                m_cur_char);
        } else {
            ASSERT0(t == T_FP);
            m_cur_char = getNextChar();
            t = T_FPF;
        }
    }
    ASSERT0(m_cur_token_string_pos < m_cur_token_string_len);
    return t;
}


//'m_cur_char' hold the current charactor right now.
//You should assign 'm_cur_char' the next valid charactor before
//the function return.
TOKEN Lexer::t_string()
{
    CHAR c = getNextChar();
    while (c != '"') {
        if (c == '\\') {
            //c is escape char.
            c = getNextChar();
            if (c == 'n') {
                //newline, 0xa
                m_cur_token_string[m_cur_token_string_pos++] = '\n';
                c = getNextChar();
            }
            else if (c == 't') {
                //horizontal tab
                m_cur_token_string[m_cur_token_string_pos++] = '\t';
                c = getNextChar();
            }
            else if (c == 'b') {
                //backspace
                m_cur_token_string[m_cur_token_string_pos++] = '\b';
                c = getNextChar();
            }
            else if (c == 'r') {
                //carriage return, 0xd
                m_cur_token_string[m_cur_token_string_pos++] = '\r';
                c = getNextChar();
            }
            else if (c == 'f') {
                //form feed
                m_cur_token_string[m_cur_token_string_pos++] = '\f';
                c = getNextChar();
            }
            else if (c == '\\') {
                //backslash
                m_cur_token_string[m_cur_token_string_pos++] = '\\';
                c = getNextChar();
            }
            else if (c == '\'') {
                //single quote
                m_cur_token_string[m_cur_token_string_pos++] = '\'';
                c = getNextChar();
            }
            else if (c == '"') {
                //double quote
                m_cur_token_string[m_cur_token_string_pos++] = '"';
                c = getNextChar();
            }
            else if (c >= '0' && c <= '9') {
                //Finally, the escape \ddd consists of the backslash followed
                //by
                // 1. not more than 3 octal digits or
                // 2. not more than 2 hex digits start with 'x' or
                // 3. any length of hex digits
                //which are taken to specify the desired character.
                UINT n = 0;
                while ((c >= '0' && c <= '7') && n < 3) {
                    m_cur_token_string[m_cur_token_string_pos++] = c;
                    n++;
                    c = getNextChar();
                }
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_token_string_pos -= n;

                //long type truncated to char type.
                CHAR o = (CHAR)xatoll(&m_cur_token_string[m_cur_token_string_pos],
                    true);
                m_cur_token_string[m_cur_token_string_pos++] = o;
            }
            else if (c == 'x' || c == 'X' || (c >= 'a' && c <= 'f') ||
                (c >= 'A' && c <= 'Z')) {
                //'\xdd' or '\aabb'
                bool only_allow_two_hex = false;
                if (c == 'x' || c == 'X') {
                    only_allow_two_hex = true;
                    c = getNextChar();
                }
                UINT n = 0;
                while (xisdigithex(c)) {
                    m_cur_token_string[m_cur_token_string_pos++] = c;
                    n++;
                    c = getNextChar();
                }
                if (n > 2 && only_allow_two_hex) {
                    error(m_real_line_num,
                        "constant too big, only permit two hex digits");
                }
            }
            else {
                m_cur_token_string[m_cur_token_string_pos++] = '\\';
                m_cur_token_string[m_cur_token_string_pos++] = c;
                c = getNextChar();
            }
        }
        else {
            m_cur_token_string[m_cur_token_string_pos++] = c;
            c = getNextChar();
        }
        checkAndGrowCurTokenString();
    }
    m_cur_char = getNextChar();
    m_cur_token_string[m_cur_token_string_pos] = 0;
    ASSERT0(m_cur_token_string_pos < m_cur_token_string_len);
    return T_STRING;
}


void Lexer::checkAndGrowCurTokenString()
{
    if (m_cur_token_string_pos + 10 > m_cur_token_string_len) {
        m_cur_token_string_len *= 2;
        m_cur_token_string = (CHAR*)::realloc(m_cur_token_string,
            m_cur_token_string_len);
    }
}


//'m_cur_char' hold the current charactor right now.
//You should assign 'm_cur_char' the next valid charactor before
//the function return.
TOKEN Lexer::t_char_list()
{
    CHAR c = getNextChar();
    while (c != '\'') {
        if (c == '\\') {
            //c is escape char.
            c = getNextChar();
            if (c == 'n' ) {
                //newline, 0xa
                m_cur_token_string[m_cur_token_string_pos++] = '\n';
                c = getNextChar();
            } else if (c == 't') {
                //horizontal tab
                m_cur_token_string[m_cur_token_string_pos++] = '\t';
                c = getNextChar();
            } else if (c == 'b') {
                //backspace
                m_cur_token_string[m_cur_token_string_pos++] = '\b';
                c = getNextChar();
            } else if (c == 'r') {
                //carriage return, 0xd
                m_cur_token_string[m_cur_token_string_pos++] = '\r';
                c = getNextChar();
            } else if (c == 'f') {
                //form feed
                m_cur_token_string[m_cur_token_string_pos++] = '\f';
                c = getNextChar();
            } else if (c == '\\') {
                //backslash
                m_cur_token_string[m_cur_token_string_pos++] = '\\';
                c = getNextChar();
            } else if (c == '\'') {
                //single quote
                m_cur_token_string[m_cur_token_string_pos++] = '\'';
                c = getNextChar();
            } else if (c >= '0' && c <= '9') {
                //Finally, the escape \ddd consists of the backslash followed
                //by
                // 1. not more than 3 octal digits or
                // 2. not more than 2 hex digits start with 'x' or
                // 3. any length of hex digits
                //which are taken to specify the desired character.
                UINT n = 0;
                while ((c >= '0' && c <= '7') && n < 3) {
                    m_cur_token_string[m_cur_token_string_pos++] = c;
                    n++;
                    c = getNextChar();
                }
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_token_string_pos -= n;
                //long type truncated to char type.
                CHAR o = (CHAR)xatoll(&m_cur_token_string[m_cur_token_string_pos],
                    true);
                m_cur_token_string[m_cur_token_string_pos++] = o;
            } else if (c == 'x' || c == 'X' || (c >= 'a' && c <= 'f') ||
                       (c >= 'A' && c <= 'Z')) {
                //'\xdd' or '\aabb'
                bool only_allow_two_hex = false;
                if (c == 'x' || c == 'X') {
                    only_allow_two_hex = true;
                    c = getNextChar();
                }
                UINT n = 0;
                while (xisdigithex(c)) {
                    m_cur_token_string[m_cur_token_string_pos++] = c;
                    n++;
                    c = getNextChar();
                }
                if (n > 2 && only_allow_two_hex) {
                    error(m_real_line_num,
                        "constant too big, only permit two hex digits");
                }
            } else {
                m_cur_token_string[m_cur_token_string_pos++] = '\\';
                m_cur_token_string[m_cur_token_string_pos++] = c;
                c = getNextChar();
            }
        } else {
            m_cur_token_string[m_cur_token_string_pos++] = c;
            c = getNextChar();
        }
        checkAndGrowCurTokenString();
    }
    m_cur_char = getNextChar();
    m_cur_token_string[m_cur_token_string_pos] = 0;
    ASSERT0(m_cur_token_string_pos < m_cur_token_string_len);
    return T_CHAR_LIST;
}


//'m_cur_char' hold the current charactor right now.
//You should assign 'm_cur_char' the next valid charactor before
//the function return.
TOKEN Lexer::t_id()
{
    CHAR c = getNextChar();
    while (xisalpha(c) || c == '_' || xisdigit(c)) {
        m_cur_token_string[m_cur_token_string_pos++] = c;
        c = getNextChar();
    }
    m_cur_char = c;
    m_cur_token_string[m_cur_token_string_pos] = 0;
    TOKEN tok = getKeyWord(m_cur_token_string);
    if (tok != T_NUL) {
        return tok;
    }
    ASSERT0(m_cur_token_string_pos < m_cur_token_string_len);
    return T_IDENTIFIER;
}


//'m_cur_char' hold the current charactor right now.
//You should assign 'm_cur_char' the next valid charactor before
//the function return.
TOKEN Lexer::t_solidus()
{
    TOKEN t = T_NUL;
    INT st;
    CHAR c = getNextChar();
    if (c == '=') { // /=
        t = T_DIVEQU;
        m_cur_token_string[m_cur_token_string_pos++] = '/';
        m_cur_token_string[m_cur_token_string_pos++] = c;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
    } else if (c == '/') { //single comment line
        if((st = getLine()) == ST_SUCC){
            m_cur_char = m_cur_line[m_cur_line_pos];
            m_cur_line_pos++;
            if (m_cur_char == '/'){ // another single comment line
                t = t_solidus();
                goto FIN;
            } else {
                t = getNextToken();
                goto FIN;
             }
        } else if (st == ST_EOF) {
            t = T_END;
            goto FIN;
        }
    } else if (c == '*') {//multi comment line
        UINT cur_line_num = m_src_line_num;
        c = getNextChar();
        CHAR c1 = 0;
        for (;;) {
            cur_line_num = m_src_line_num;
            c1 = getNextChar();
            if (c == '*' && c1 == '/') {
                if (m_src_line_num == cur_line_num) {
                    //We meet the multipul comment terminated token '*/',
                    //so change the parsing state to normal.
                    m_cur_char = getNextChar();
                    t = getNextToken();
                    goto FIN;
                } else {
                    c = c1;
                    continue;
                }
            } else if (c == ST_EOF || c1 == ST_EOF) {
                  t = T_END;
                goto FIN;
            }else{
                  c = c1;
            }
        } //end while
    } else {
        t = T_DIV;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = c;
    }//end elseif
FIN:
    ASSERT0(m_cur_token_string_pos < m_cur_token_string_len);
    return t;
}


//'m_cur_char' hold the current charactor right now.
//You should assign 'm_cur_char' the next valid charactor before
//the function return.
TOKEN Lexer::t_dot()
{
    //Here m_cur_char is '.'
    CHAR c = 0;
    TOKEN t;
    m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
    c = getNextChar();
    if (c == '.') {
        // token string is ..
        m_cur_token_string[m_cur_token_string_pos++] = c;
        c = getNextChar();
        if (c == '.') {
            // token string is ...
            m_cur_token_string[m_cur_token_string_pos++] = c;
            t = T_DOTDOTDOT;
            c = getNextChar();
        } else {
            //Here '..' is a invalid token
            t = T_NUL;
        }
    } else {
        //token string is '.'
        t = T_DOT;
    }
    m_cur_token_string[m_cur_token_string_pos] = 0;
    m_cur_char = c;
    ASSERT0(m_cur_token_string_pos < m_cur_token_string_len);
    return t;
}


TOKEN Lexer::getNextToken()
{
    ASSERT0(m_src_file);
    TOKEN token;
    if (m_cur_token == T_END) {
        return m_cur_token;
    }
    m_cur_token_string_pos = 0;
    m_cur_token_string[0] = 0;
    if (m_cur_char == 0) {
        while (m_cur_char == 0) {
            m_cur_char = getNextChar();
            if (m_cur_char == ST_EOF) {
                token = T_END;
                goto FIN;
            }
         }
    }
    switch(m_cur_char){
    case ST_EOF:
        token = T_END;
        break;
    case 0xa:
    case 0xd:
        //'\n'
        if (m_enable_newline_token && m_cur_char == 0xa) {
            token = T_NEWLINE;
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_token_string[m_cur_token_string_pos] = 0;
            m_cur_char = getNextChar();
        } else  {
            m_cur_char = getNextChar();
            token = getNextToken();
        }
        break;
    case '\t':
        while ((m_cur_char = getNextChar()) == '\t');
        token = getNextToken();
        break;
    case ' ':
        while((m_cur_char = getNextChar()) == ' ');
        token = getNextToken();
        break;
    case '@':
        token = T_AT;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case ';':
        token = T_SEMI;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case ',':
        token = T_COMMA;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char=getNextChar();
        break;
    case '{':
        token = T_LLPAREN;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case '}':
        token = T_RLPAREN;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case '[':
        token = T_LSPAREN;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case ']':
        token = T_RSPAREN;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case '(':
        token = T_LPAREN;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case ')':
        token = T_RPAREN;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case '~':
        token = T_REV;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case '?':
        token = T_QUES_MARK;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case '#':
        token = T_SHARP;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    case '$':
        token = T_DOLLAR;
        m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
        m_cur_token_string[m_cur_token_string_pos] = 0;
        m_cur_char = getNextChar();
        break;
    default:
        if (m_cur_char == '"') { //string
            token = t_string();
        } else if (m_cur_char == '\'') { //char list
            token = t_char_list();
        } else if (xisalpha(m_cur_char) || m_cur_char == '_') { //identifier
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            token = t_id();
            if (m_enable_true_false_token &&
                (token == T_TRUE || token == T_FALSE)) {
                if (token == T_TRUE) {
                    m_cur_token_string[0] = '1';
                    m_cur_token_string[1] = 0;
                } else {
                    m_cur_token_string[0] = '0';
                    m_cur_token_string[1] = 0;
                }
                token = T_IMM;
                m_cur_token_string_pos = 1;
            }
        } else if (xisdigit(m_cur_char) != 0) { //imm
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            token = t_num();
        } else if(m_cur_char == '-') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '=') { //'-='
                token = T_SUBEQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else if(m_cur_char == '>') { //'->'
                token = T_ARROW;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else if (m_cur_char == '-') { //'--'
                token = T_SUBSUB;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { //'-'
                token = T_SUB;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '+') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '=') { //'+='
                token = T_ADDEQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else if (m_cur_char == '+') { //'++'
                token = T_ADDADD;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { //'+'
                token = T_ADD;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if(m_cur_char == '%') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if(m_cur_char == '='){ //'%='
                token = T_REMEQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { //'%'
                token = T_MOD;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '^') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '=') { //'^='
                token = T_XOREQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { //'^'
                token = T_XOR;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '=') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '=') { //'=='
                token = T_EQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { //'='
                token = T_ASSIGN;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '*') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '=') { //'*='
                token = T_MULEQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { //'*'
                token = T_ASTERISK;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '&') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '&') { //'&&'
                token = T_AND;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else if (m_cur_char == '=') { //&=
                token = T_BITANDEQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { //'&'
                token = T_BITAND;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '|') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '|') { //'||'
                token = T_OR;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else if (m_cur_char == '=') { //|=
                token = T_BITOREQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else {  // '|'
                token = T_BITOR;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == ':') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            token = T_COLON;
            m_cur_token_string[m_cur_token_string_pos] = 0;
        } else if (m_cur_char == '>') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '>') {
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_char = getNextChar();
                if (m_cur_char == '=') { // >>=
                    token = T_RSHIFTEQU;
                    m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                    m_cur_token_string[m_cur_token_string_pos] = 0;
                    m_cur_char = getNextChar();
                } else { // >>
                    token = T_RSHIFT;
                    m_cur_token_string[m_cur_token_string_pos] = 0;
                }
            } else if (m_cur_char == '=') { // '>='
                token =    T_NOLESSTHAN;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { //'>'
                token = T_MORETHAN;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '<') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '<') {
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_char = getNextChar();
                if (m_cur_char == '=') { // <<=
                    token = T_LSHIFTEQU;
                    m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                    m_cur_token_string[m_cur_token_string_pos] = 0;
                    m_cur_char = getNextChar();
                } else { // <<
                    token = T_LSHIFT;
                    m_cur_token_string[m_cur_token_string_pos] = 0;
                }
            } else if (m_cur_char == '=') {// '<='
                token =    T_NOMORETHAN;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else {  // '<'
                token = T_LESSTHAN;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '!') {
            m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
            m_cur_char = getNextChar();
            if (m_cur_char == '=') {// '!='
                token =    T_NOEQU;
                m_cur_token_string[m_cur_token_string_pos++] = m_cur_char;
                m_cur_token_string[m_cur_token_string_pos] = 0;
                m_cur_char = getNextChar();
            } else { // '!'
                token = T_NOT;
                m_cur_token_string[m_cur_token_string_pos] = 0;
            }
        } else if (m_cur_char == '/') {
            token = t_solidus();
        } else if (m_cur_char == '.') {
            token = t_dot();
        }
        //Do not add anything after this line.
        else if (m_cur_token == T_END) {
            token = T_END;
        } else {
            token = T_NUL;
        }
    } //end switch
FIN:
    m_cur_token = token;
    ASSERT0(m_cur_token_string_pos < m_cur_token_string_len);
    return token;
}


CHAR const* Lexer::getTokenName(TOKEN tok) const
{
    return TOKEN_INFO_name(&g_token_info[tok]);
}


//Dump token string.
void Lexer::dump(CHAR const* input, FILE * output)
{
    ASSERT0(input && output);
    FILE * h = fopen(input, "r");
    if (h == NULL) { return; }
    setSrcFile(h);
    TOKEN tok = getNextToken();
    while (tok != T_END) {
        UINT linenum = getCurrentLineNum();
        CHAR const* curtokstr = getCurrentTokenString();
        if (tok == T_NUL) {
            if (output != NULL) {
                fprintf(output, "ERROR(%d) Str:%s TokName:%s\n",
                    linenum, curtokstr, g_token_info[tok].name);
            }
            break;
        }
        if (output != NULL) {
            fprintf(output, "Line(%u) Str:%s TokName:%s\n",
                linenum, curtokstr, g_token_info[tok].name);
        }
        tok = getNextToken();
    }
    if (output != NULL) {
        fprintf(output, "\n\n\n");
        fflush(output);
    }
    fclose(h);

}

} //namespace xoc
