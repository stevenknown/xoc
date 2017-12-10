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
#include "../com/xcominc.h"
#include "../opt/cominc.h"
#include "../opt/comopt.h"
#include "ir_lex.h"
#include "ir_parser.h"

namespace xoc {

class XCodeInfo {
public:
    X_CODE code;
    CHAR const* name;
    IR_TYPE irtype;
};


//Define keywords of XOC IR.
static XCodeInfo g_keyword_info[] = {
    { X_UNDEF,       "",                 },
    { X_ID,          "id",               },
    { X_LD,          "ld",               },
    { X_ILD,         "ild",              },
    { X_ARRAY,       "array",            },
    { X_ST,          "st",               },
    { X_STRP,        "stpr",             },
    { X_STARRAY,     "starray",          },
    { X_SETELEM,     "setelem",          },
    { X_GETELEM,     "getelem",          },
    { X_IST,         "ist",              },
    { X_CALL,        "call",             },
    { X_ICALL,       "icall",            },
    { X_LDA,         "lda",              },
    { X_ADD,         "add",              },
    { X_SUB,         "sub",              },
    { X_MUL,         "mul",              },
    { X_DIV,         "div",              },
    { X_REM,         "rem",              },
    { X_MOD,         "mod",              },
    { X_LAND,        "land",             },
    { X_LOR,         "lor",              },
    { X_BAND,        "band",             },
    { X_BOR,         "bor",              },
    { X_XOR,         "xor",              },
    { X_ASR,         "asr",              },
    { X_LSR,         "lsr",              },
    { X_LSL,         "lsl",              },
    { X_LT,          "lt",               },
    { X_LE,          "le",               },
    { X_GT,          "gt",               },
    { X_GE,          "ge",               },
    { X_EQ,          "eq",               },
    { X_NE,          "ne",               },
    { X_BNOT,        "bnot",             },
    { X_LNOT,        "lnot",             },
    { X_NEG,         "neg",              },
    { X_CVT,         "cvt",              },
    { X_GOTO,        "goto",             },
    { X_IGOTO,       "igoto",            },
    { X_DO,          "do",               },
    { X_WHILE,       "while",            },
    { X_DO_LOOP,     "doloop",           },
    { X_LABEL,       "label",            },
    { X_TRUEBR,      "truebr",           },
    { X_FALSEBR,     "falsebr",          },
    { X_SELECT,      "select",           },
    { X_PHI,         "phi",              },
    { X_REGION,      "region",           },
    { X_IF,          "if",               },
    { X_ELSE,        "else",             },
    { X_BREAK,       "break",            },
    { X_RETURN,      "return",           },
    { X_CONTINUE,    "continue",         },
    { X_SWITCH,      "switch",           },
    { X_CASE,        "case",             },
    { X_DEFAULT,     "default",          },
    { X_VAR,         "var",              },
    { X_FUNC,        "func",             },
    { X_PROGRAM,     "program",          },
    { X_I8,          "i8",               },
    { X_U8,          "u8",               },
    { X_I16,         "i16",              },
    { X_U16,         "u16",              },
    { X_I32,         "i32",              },
    { X_U32,         "u32",              },
    { X_I64,         "i64",              },
    { X_U64,         "u64",              },
    { X_I128,        "i128",             },
    { X_U128,        "u128",             },
    { X_F32,         "f32",              },
    { X_F64,         "f64",              },
    { X_F80,         "f80",              },
    { X_F128,        "f128",             },
    { X_MC,          "mc",               },
    { X_STR,         "str",              },
    { X_VEC,         "vec",              },
    { X_BOOL,        "bool",             },
    { X_VOID,        "void",             },
    { X_READONLY,    "readonly",         },
    { X_TRY_START,   "try_start",        },
    { X_TRY_END,     "try_end",          },
    { X_TERMINATE,   "terminate",        },
    { X_CATCH_START, "catch_start",      },
    { X_ATOM,        "atom",             },
    { X_RMW,         "rmw",              },
    { X_THROW,       "throw",            },
    { X_SIDEEFFECT,  "sideeffect",       },
    { X_NOMOVE,      "nomove",           },
    { X_USE,         "use",              },
    { X_PRIVATE,     "private",          },
    { X_RESTRICT,    "restrict",         },
    { X_VOLATILE,    "volatile",         },
    { X_FUNC_DECL,   "func_decl",        },
    { X_FAKE,        "fake",             },
    { X_GLOBAL,      "global",           },
    { X_UNDEFINED,   "undefined",        },
    { X_LAST,        "",                 },
};


//
//START ParseCtx
//
//END ParseCtx


//
//START IRParser
//
static void copyProp(IR * ir, PropertySet & cont)
{
    IR_is_atomic(ir) = cont.atomic;
    IR_may_throw(ir) = cont.throw_exception;
    IR_has_sideeffect(ir) = cont.sideeffect;
    IR_no_move(ir) = cont.nomove;
    IR_is_read_mod_write(ir) = cont.read_modify_write;
    IR_is_terminate(ir) = cont.terminate;
}


IRParser::~IRParser()
{
    for (ParseErrorMsg * msg = m_err_list.get_head();
         msg != NULL; msg = m_err_list.get_next()) {
        delete msg;
    }
}


void IRParser::initKeyWordMap()
{
    for (UINT i = X_UNDEF + 1; i < X_LAST; i++) {
        m_sym2xcode.set(m_rumgr->addToSymbolTab(
            g_keyword_info[i].name), (X_CODE)i);
    }
}


CHAR const* IRParser::getKeywordName(X_CODE code)
{
    ASSERT0(code >= X_UNDEF && code < X_LAST);
    return g_keyword_info[code].name;
}


void IRParser::dump()
{
    ASSERT0(0);
}


////Find VAR in nested region from the inside out.
//Region * IRParser::findRegion(ParseCtx * ctx, SYM const* name)
//{
//    ASSERT0(ctx && ctx->current_region && name);
//    for (Region * region = ctx->current_region->getParent();
//         region != NULL; region = region->getParent()) {
//        if (region->getRegionVar()->get_name() == NULL) {
//            return region;
//        }
//        for (IR * ir = region->getIRList(); ir != NULL; ir = ir->get_next()) {
//            if (ir->is_region() &&
//                REGION_ru(ir)->getRegionVar()->get_name() == name) {
//                return REGION_ru(ir);
//            }
//        }
//    }
//    return NULL;
//}


//Find VAR in nested region from the inside out.
VAR * IRParser::findVar(ParseCtx * ctx, SYM const* name)
{
    ASSERT0(ctx && ctx->current_region && name);
    for (Region * region = ctx->current_region;
         region != NULL; region = region->getParent()) {
        VAR * var = region->findVarViaSymbol(name);
        if (var != NULL) {
            return var;
        }
    }
    return NULL;
}


X_CODE IRParser::getXCode(TOKEN tok, CHAR const* tok_string)
{
    if (tok != T_IDENTIFIER) { return X_UNDEF; }
    SYM const* sym = m_rumgr->addToSymbolTab(tok_string);
    return m_sym2xcode.get(sym);
}


X_CODE IRParser::getCurrentXCode()
{
    return getXCode(m_lexer->getCurrentToken(),
        m_lexer->getCurrentTokenString());
}


void IRParser::error(CHAR const* format, ...)
{
    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vsprint(format, arg);
    prt2C("\nerror(%d):%s", m_lexer->getCurrentLineNum(), buf.buf);
    va_end(arg);

    ParseErrorMsg * msg = new ParseErrorMsg(10);
    m_err_list.append_tail(msg);
}


void IRParser::error(TOKEN tok, CHAR const* format, ...)
{
    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vsprint(format, arg);
    prt2C("\nerror(%d):%s", m_lexer->getCurrentLineNum(), buf.buf);
    va_end(arg);

    ParseErrorMsg * msg = new ParseErrorMsg(10);
    m_err_list.append_tail(msg);
}


void IRParser::error(X_CODE xcode, CHAR const* format, ...)
{
    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vsprint(format, arg);
    prt2C("\nerror(%d):%s", m_lexer->getCurrentLineNum(), buf.buf);
    va_end(arg);

    ParseErrorMsg * msg = new ParseErrorMsg(10);
    m_err_list.append_tail(msg);
}


////'num': pry the followed 'num' number of tokens.
////'...': represent a token list which will to match.
//bool IRParser::peekTokenList(UINT num, ...)
//{
//    ASSERT0(num >= 1);
//    va_list arg;
//    va_start(arg, num);
//    TOKEN v = (TOKEN)va_arg(arg, UINT);
//    if (num == 1) {
//        va_end(arg);
//        return g_real_token == v;
//    }
//
//    CELL * c = g_cell_list.get_head();
//    if (c != NULL) {
//        //append current real token to 'token-list'
//        append_tok_head(g_real_token, g_real_token_string, g_real_line_num);
//
//        //Restart again.
//        c = g_cell_list.get_head();
//        while (num > 0) {
//            if (c) { //match element resided in token_list.
//                TokenInfo * tki = (TokenInfo*)CELL_val(c);
//                if (TOKEN_INFO_token(tki) != v) {
//                    goto UNMATCH;
//                }
//                c = g_cell_list.get_next();
//            } else { //fetch new token to match.
//                gettok();
//                append_tok_tail(g_real_token, g_real_token_string,
//                                g_real_line_num);
//                if (g_real_token != v) {
//                    goto UNMATCH;
//                }
//            }
//            v = (TOKEN)va_arg(arg, INT);
//            num--;
//        }
//
//    } else {
//        //token_list is empty. So fetch new token to match.
//        while (num > 0) {
//            append_tok_tail(g_real_token, g_real_token_string, g_real_line_num);
//            if (g_real_token != v) { goto UNMATCH; }
//            gettok();
//            v = (TOKEN)va_arg(arg, INT);
//            num--;
//        }
//        append_tok_tail(g_real_token, g_real_token_string, g_real_line_num);
//    }
//    va_end(arg);
//    reset_tok();
//    return true;
//UNMATCH:
//    reset_tok();
//    return false;
//}


bool IRParser::declareRegion(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_REGION);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_REGION;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    Region * region = NULL;
    UINT flag = 0;
    X_CODE code = getXCode(tok, m_lexer->getCurrentTokenString());
    switch (code) {
    case X_FUNC:
    case X_SUB:
        region = m_rumgr->newRegion(REGION_FUNC);
        SET_FLAG(flag, VAR_LOCAL);
        break;
    case X_PROGRAM:
        region = m_rumgr->newRegion(REGION_PROGRAM);
        SET_FLAG(flag, VAR_GLOBAL);
        break;
    default:
        error(tok, "miss region type");
        return false;
    }
    m_rumgr->addToRegionTab(region);

    //Region name
    tok = m_lexer->getNextToken();
    if (tok != T_IDENTIFIER) {
        error(tok, "miss region name");
        return false;
    }
    VAR * regionvar = NULL;
    SYM const* sym = m_rumgr->addToSymbolTab(m_lexer->getCurrentTokenString());
    if (ctx != NULL) {
        regionvar = findVar(ctx, sym);
        REGION_parent(region) = ctx->current_region;
    }
    if (regionvar == NULL) {
        regionvar = m_rumgr->getVarMgr()->registerVar(
            m_lexer->getCurrentTokenString(),
            m_rumgr->getTypeMgr()->getMCType(0), 1, flag);
        VAR_is_func_decl(regionvar) = true;
    }
    region->setRegionVar(regionvar);
    if (ctx != NULL) {
        ctx->current_region->addToVarTab(regionvar);
    }
    if (!regionvar->is_func_decl()) {
        error("var %s should be func_decl", SYM_name(sym));
    }

    ParseCtx newctx;
    newctx.current_region = region;
    enterRegion(&newctx);

    //Region parameters
    m_lexer->getNextToken();
    if (!parseParameterList(&newctx)) {
        return false;
    }

    //Region body
    if (!parseRegionBody(&newctx)) {
        return false;
    }

    if (!newctx.has_error) {
        newctx.current_region->setIRList(newctx.stmt_list);
        if (!newctx.has_high_level_ir) {
            newctx.current_region->constructIRBBlist();
            newctx.current_region->setIRList(NULL);

            //dumpBBList(newctx.current_region->getBBList(), newctx.current_region);

            OptCtx oc;
            newctx.current_region->initPassMgr();
            newctx.current_region->checkValidAndRecompute(&oc, PASS_CFG, PASS_UNDEF);
            //newctx.current_region->getCFG()->dump_vcg();
            if (newctx.has_phi) {
                newctx.current_region->getCFG()->
                    buildAndRevisePhiEdge(newctx.getIR2Label());
                PRSSAMgr * prssamgr = (PRSSAMgr*)newctx.current_region->
                    getPassMgr()->registerPass(PASS_PR_SSA_MGR);
                ASSERT0(prssamgr);
                prssamgr->computeSSAInfo();
				prssamgr->verifySSAInfo();
				prssamgr->verifyPhi(false);
            }
        }
    }

    exitRegion(&newctx);

    if (ctx != NULL) {
        ASSERT0(ctx->current_region);
        IR * ir = ctx->current_region->buildRegion(region);
        copyProp(ir, cont);
        ctx->addIR(ir);
    }
    return true;
}


bool IRParser::parseRegionBody(ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getCurrentToken();
    if (tok != T_LLPAREN) {
        error(tok, "region body miss opening '{'");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Var declarations
    for (; tok == T_IDENTIFIER;) {
        X_CODE code = getCurrentXCode();
        if (code == X_VAR) {
            VAR * v;
            if (!declareVar(ctx, &v)) {
                return false;
            }
            tok = m_lexer->getCurrentToken();
            if (tok != T_SEMI) {
                error(tok, "miss ';' after variable declaration");
                return false;
            }
        } else {
            break;
        }

        //Match useless ';'
        for (;tok == T_SEMI;) {
            tok = m_lexer->getNextToken();
        }
    }

    if (!parseStmtList(ctx)) {
        return false;
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "region body miss closing '}'");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseStmtList(ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getCurrentToken();
    bool has_high_level_ir = false;
    bool has_phi = false;
    for (;;) {
        X_CODE code = getXCode(tok, m_lexer->getCurrentTokenString());
        bool res = false;
        //prt2C("\ncode:%s,line:%d", getKeywordName(code), m_lexer->getCurrentLineNum());
        switch (code) {
        case X_REGION:
            res = declareRegion(ctx);
            break;
        case X_ST:
            res = parseStore(ctx);
            break;
        case X_STRP:
            res = parseStorePR(ctx);
            break;
        case X_STARRAY:
            res = parseStoreArray(ctx);
            break;
        case X_SETELEM:
        case X_GETELEM:
            res = parseModifyPR(code, ctx);
            break;
        case X_IST:
            res = parseIStore(ctx);
            break;
        case X_CALL:
            res = parseCallAndICall(true, ctx);
            break;
        case X_ICALL:
            res = parseCallAndICall(false, ctx);
            break;
        case X_GOTO:
            res = parseGoto(ctx);
            break;
        case X_IGOTO:
            res = parseIGoto(ctx);
            break;
        case X_DO:
            res = parseDoWhile(ctx);
            has_high_level_ir = true;
            break;
        case X_WHILE:
            res = parseWhileDo(ctx);
            has_high_level_ir = true;
            break;
        case X_DO_LOOP:
            res = parseDoLoop(ctx);
            has_high_level_ir = true;
            break;
        case X_LABEL:
            res = parseLabel(ctx);
            break;
        case X_TRUEBR:
            res = parseBranch(true, ctx);
            break;
        case X_FALSEBR:
            res = parseBranch(false, ctx);
            break;
        case X_PHI:
            res = parsePhi(ctx);
            has_phi = true;
            break;
        case X_IF:
            res = parseIf(ctx);
            has_high_level_ir = true;
            break;
        case X_BREAK:
            res = parseBreak(ctx);
            has_high_level_ir = true;
            break;
        case X_RETURN:
            res = parseReturn(ctx);
            break;
        case X_CONTINUE:
            res = parseContinue(ctx);
            has_high_level_ir = true;
            break;
        case X_SWITCH:
            res = parseSwitch(ctx);
            break;
        default:
            if (isEndOfScope()) {
                ctx->has_phi |= has_phi;
                ctx->has_high_level_ir |= has_high_level_ir;
                return true;
            }
            error(tok, "not stmt operation");
            res = false;
        }

        if (has_high_level_ir && has_phi) {
            error(tok, "phi can not be compatible with high level ir");
            ctx->has_error = true;
        }

        if (!res) {
            ctx->has_error = true;
            //Error recovery.
            for (; tok != T_SEMI; tok = m_lexer->getNextToken()) {}
        }

        for (tok = m_lexer->getCurrentToken();
             tok == T_SEMI; tok = m_lexer->getNextToken());

        if (isTooManyError()) {
            ctx->has_phi |= has_phi;
            ctx->has_high_level_ir |= has_high_level_ir;
            return false;
        }

        //dump_irs_h(ctx->stmt_list, m_tm);
    }
    UNREACH();
    return true;
}


bool IRParser::isEndOfScope() const
{
    return m_lexer->getCurrentToken() == T_RLPAREN;
}


bool IRParser::parseXOperator(ParseCtx * ctx)
{
    X_CODE code = getCurrentXCode();
    switch (code) {
    case X_ID:
        return parseId(ctx);
    case X_LD:
        return parseLd(ctx);
    case X_ILD:
        return parseIld(ctx);
    case X_ARRAY:
        return parseArray(ctx);
    case X_LDA:
        return parseLda(ctx);
        break;
    case X_ADD:
        return parseBinaryOp(IR_ADD, ctx);
    case X_SUB:
        return parseBinaryOp(IR_SUB, ctx);
    case X_MUL:
        return parseBinaryOp(IR_MUL, ctx);
    case X_DIV:
        return parseBinaryOp(IR_DIV, ctx);
    case X_REM:
        return parseBinaryOp(IR_REM, ctx);
    case X_MOD:
        return parseBinaryOp(IR_MOD, ctx);
    case X_LAND:
        return parseBinaryOp(IR_LAND, ctx);
    case X_LOR:
        return parseBinaryOp(IR_LOR, ctx);
    case X_BAND:
        return parseBinaryOp(IR_BAND, ctx);
    case X_BOR:
        return parseBinaryOp(IR_BOR, ctx);
    case X_XOR:
        return parseBinaryOp(IR_XOR, ctx);
    case X_ASR:
        return parseBinaryOp(IR_ASR, ctx);
    case X_LSR:
        return parseBinaryOp(IR_LSR, ctx);
    case X_LSL:
        return parseBinaryOp(IR_LSL, ctx);
    case X_LT:
        return parseBinaryOp(IR_LT, ctx);
    case X_LE:
        return parseBinaryOp(IR_LE, ctx);
    case X_GT:
        return parseBinaryOp(IR_GT, ctx);
    case X_GE:
        return parseBinaryOp(IR_GE, ctx);
    case X_EQ:
        return parseBinaryOp(IR_EQ, ctx);
    case X_NE:
        return parseBinaryOp(IR_NE, ctx);
    case X_BNOT:
        return parseUnaryOp(IR_BNOT, ctx);
    case X_LNOT:
        return parseUnaryOp(IR_LNOT, ctx);
    case X_NEG:
        return parseUnaryOp(IR_NEG, ctx);
    case X_CVT:
        return parseCvt(ctx);
    case X_SELECT:
        return parseSelect(ctx);
    case X_CASE:
        return parseCase(ctx);
    default:;
    }
    error(code, "not operator");
    return false;
}


bool IRParser::parseCase(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_CASE);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_CASE;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    IR * case_det = NULL;
    if (tok == T_IMM) {
        case_det = ctx->current_region->buildImmInt(
            (HOST_INT)xcom::xatoll(m_lexer->getCurrentTokenString(), false),
            m_tm->getSimplexType(
                m_tm->get_int_dtype(
                    sizeof(HOST_INT)*BIT_PER_BYTE, true)));
        tok = m_lexer->getNextToken();

        Type const* ty = NULL;
        if (tok == T_COLON) {
            tok = m_lexer->getNextToken();
            if (!parseType(ctx, &ty) || ty == NULL) {
                return false;
            }
            IR_dt(case_det) = ty;
            tok = m_lexer->getCurrentToken();
        }
    } else if (tok == T_STRING) {
        case_det = ctx->current_region->buildString(
            m_rumgr->addToSymbolTab(m_lexer->getCurrentTokenString()));
        tok = m_lexer->getNextToken();
    } else if (tok == T_FP) {
        HOST_FP val = atof(m_lexer->getCurrentTokenString());
        tok = m_lexer->getNextToken();
        Type const* ty = NULL;
        if (tok == T_COLON) {
            tok = m_lexer->getNextToken();
            if (!parseType(ctx, &ty) || ty == NULL) {
                return false;
            }
        } else {
            ty = m_tm->getF64();
        }
        case_det = ctx->current_region->buildImmFp(val, ty);
        tok = m_lexer->getCurrentToken();
    } else {
        error(tok, "case determinate must be constant");
        return false;
    }

    //Case expression's target label
    if (tok != T_COMMA) {
        error(tok, "miss ',' after case determinate expression");
        return false;
    }
    tok = m_lexer->getNextToken();
    if (tok != T_IDENTIFIER) {
        error(tok, "illegal target label of case");
        return false;
    }
    SYM const* sym = m_rumgr->addToSymbolTab(m_lexer->getCurrentTokenString());
    LabelInfo * caselab = ctx->mapSym2Label(sym);
    if (caselab == NULL) {
        caselab = ctx->current_region->genCustomLabel(sym);
        ctx->setMapSym2Label(sym, caselab);
    }
    tok = m_lexer->getNextToken();

    IR * case_exp = ctx->current_region->buildCase(case_det, caselab);
    copyProp(case_exp, cont);
    ctx->returned_exp = case_exp;
    return true;
}


bool IRParser::parseSelect(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_SELECT);
    TOKEN tok = m_lexer->getNextToken();
    ctx->returned_exp = NULL;

    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            return false;
        }
    }

    if (!parseExp(ctx)) {
        return false;
    }
    IR * det = ctx->returned_exp;
    ctx->returned_exp = NULL;
    ASSERT0(det);

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ','");
        return false;
    }
    m_lexer->getNextToken();

    if (!parseExp(ctx)) {
        return false;
    }
    IR * truepart = ctx->returned_exp;
    ASSERT0(truepart);
    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ','");
        return false;
    }
    tok = m_lexer->getNextToken();

    ctx->returned_exp = NULL;
    if (!parseExp(ctx)) {
        return false;
    }
    IR * falsepart = ctx->returned_exp;
    ASSERT0(falsepart && ctx->current_region);

    IR * exp = ctx->current_region->buildSelect(det,
        truepart, falsepart, ty == NULL ? m_tm->getVoid() : ty);
    ctx->returned_exp = exp;
    return true;
}


bool IRParser::parsePR(ParseCtx * ctx)
{
    ASSERT(m_lexer->getCurrentToken() == T_DOLLAR,
        ("miss $ before PR expression"));
    TOKEN tok = m_lexer->getNextToken();
    UINT prno = 0;
    if (tok == T_IMM) {
        prno = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        ctx->current_region->setPRCount(
            MAX(ctx->current_region->getPRCount(), prno));
    } else {
        error(tok, "miss PR number");
        return false;
    }

    tok = m_lexer->getNextToken();
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    } else {
        ty = m_tm->getVoid();
    }
    ctx->returned_exp = ctx->current_region->buildPRdedicated(prno, ty);
    return true;
}


bool IRParser::isTerminator(TOKEN tok)
{
    return tok == T_END || tok == T_NUL || tok == T_SEMI;
}


bool IRParser::parseArrayDimension(xcom::List<TMWORD> & elem_dim)
{
    ASSERT0(m_lexer->getCurrentToken() == T_LSPAREN);
    TOKEN tok = m_lexer->getNextToken();

    for (; tok != T_RSPAREN && !isTerminator(tok);) {
        if (tok != T_IMM) {
            error(tok, "dimension must be integer");
            return false;
        }
        TMWORD dim = (TMWORD)xcom::xatoll(
            m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
        elem_dim.append_tail(dim);

        if (tok != T_COMMA && tok != T_RSPAREN) {
            error(tok, "miss ',' in dimension declaration");
            return false;
        } else if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
        }
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_RSPAREN) {
        error(tok, "miss ']' after array dimension declaration");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseId(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_ID);
    TOKEN tok = m_lexer->getNextToken();

    //Id
    if (tok != T_IDENTIFIER) {
        error(tok, "miss variable name");
        return false;
    }
    VAR * var = findVar(ctx, m_rumgr->addToSymbolTab(
        m_lexer->getCurrentTokenString()));
    if (var == NULL) {
        error(tok, "%s is not declared", m_lexer->getCurrentTokenString());
        return false;
    }

    IR * id = ctx->current_region->buildId(var);
    ctx->returned_exp = id;
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseLda(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_LDA);
    TOKEN tok = m_lexer->getNextToken();

    UINT offset = 0;
    if (tok == T_COLON) {
        //Offset
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        offset = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Lda base id
    if (tok != T_IDENTIFIER) {
        error(tok, "miss variable name");
        return false;
    }
    VAR * var = findVar(ctx, m_rumgr->addToSymbolTab(
        m_lexer->getCurrentTokenString()));
    if (var == NULL) {
        error(tok, "%s is not declared", m_lexer->getCurrentTokenString());
        return false;
    }

    IR * lda = ctx->current_region->buildLda(var);
    LDA_ofst(lda) = (UINT)offset;
    IR_dt(lda) = m_tm->getPointerType(m_tm->get_bytesize(var->get_type()));
    ctx->returned_exp = lda;
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseStoreArray(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_STARRAY);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = NULL;
    if (tok == T_COLON) {
        //Type
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            return false;
        }
    }

    tok = m_lexer->getCurrentToken();
    Type const* elem_ty = NULL;
    if (tok == T_COLON) {
        //Array element type
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &elem_ty)) {
            return false;
        }
    }
	if (elem_ty == NULL) {
		elem_ty = ty;
	}

    tok = m_lexer->getCurrentToken();
    UINT offset = 0;
    if (tok == T_COLON) {
        //Offset
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        offset = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    tok = m_lexer->getCurrentToken();
    xcom::List<TMWORD> elem_dim;
    if (tok == T_COLON) {
        //Array dimension declaration
        tok = m_lexer->getNextToken();
        if (tok == T_LSPAREN) {
            if (!parseArrayDimension(elem_dim)) {
                return false;
            }
            ASSERT0(elem_dim.get_elem_count() > 0);
        } else if (tok == T_COLON) {
			;//No dimension declared.
		} else {
            error(tok, "illegal dimension declaration");
            return false;
        }
    }

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_STARRAY;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Array base expression
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * base = ctx->returned_exp;
    ctx->returned_exp = NULL;

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ','");
        return false;
    }
    tok = m_lexer->getNextToken();

    if (tok != T_LPAREN) {
        error(tok, "miss '(' before array subscript expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Array subscript expression list
    if (!parseExpList(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * subscript_list = ctx->returned_exp;
    ctx->returned_exp = NULL;

    tok = m_lexer->getCurrentToken();
     if (tok != T_RPAREN) {
        error(tok, "miss ')' after array subscript expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    TMWORD * elem_dim_buf = NULL;
    if (elem_dim.get_elem_count() > 0) {
        elem_dim_buf = (TMWORD*)ALLOCA(
            elem_dim.get_elem_count() * sizeof(TMWORD));
        xcom::C<TMWORD> * ct = NULL;
        elem_dim.get_head(&ct);
        for (UINT i = 0; i < elem_dim.get_elem_count(); i++) {
            elem_dim_buf[i] = ct->val();
            elem_dim.get_next(&ct);
        }
    }

    if (elem_dim.get_elem_count() != 0 &&
        xcom::cnt_list(subscript_list) != elem_dim.get_elem_count()) {
        error("declare %d dimension array, but %d subscript given",
            elem_dim.get_elem_count(), xcom::cnt_list(subscript_list));
        return false;
    }

    if (tok != T_COMMA) {
        error(tok, "miss ',' after starray sub-expression list");
        return false;
    }
    tok = m_lexer->getNextToken();
    if (!parseExp(ctx)) {
        return false;
    }

    IR * rhs = ctx->returned_exp;
    ASSERT0(rhs);

    IR * ir = ctx->current_region->buildStoreArray(base,
        subscript_list, ty, elem_ty == NULL ? m_tm->getVoid() : elem_ty,
        xcom::cnt_list(subscript_list),
        elem_dim_buf, rhs);
    ctx->addIR(ir);
	ARR_ofst(ir) = offset;
	copyProp(ir, cont);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseArray(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_ARRAY);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    }

    tok = m_lexer->getCurrentToken();
    Type const* elem_ty = NULL;
    if (tok == T_COLON) {
        //Array element type
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &elem_ty)) {
            return false;
        }
    }
    if (elem_ty == NULL) {
        elem_ty = ty;
    }

    tok = m_lexer->getCurrentToken();
    UINT offset = 0;
    if (tok == T_COLON) {
        //Offset
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        offset = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    tok = m_lexer->getCurrentToken();
    xcom::List<TMWORD> elem_dim;
    if (tok == T_COLON) {
        //Array dimension declaration
        tok = m_lexer->getNextToken();
        if (tok == T_LSPAREN) {
            if (!parseArrayDimension(elem_dim)) {
                return false;
            }
            ASSERT0(elem_dim.get_elem_count() > 0);
        } else {
            error(tok, "illegal dimension declaration");
            return false;
        }
    }

    //Array base expression
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * base = ctx->returned_exp;
    ctx->returned_exp = NULL;

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ','");
        return false;
    }
    tok = m_lexer->getNextToken();

    if (tok != T_LPAREN) {
        error(tok, "miss '(' before array subscript expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Array subscript expression list
    if (!parseExpList(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * subscript_list = ctx->returned_exp;
    ctx->returned_exp = NULL;

    tok = m_lexer->getCurrentToken();
     if (tok != T_RPAREN) {
        error(tok, "miss ')' after array subscript expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    TMWORD * elem_dim_buf = NULL;
    if (elem_dim.get_elem_count() > 0) {
        elem_dim_buf = (TMWORD*)ALLOCA(
            elem_dim.get_elem_count() * sizeof(TMWORD));
        xcom::C<TMWORD> * ct = NULL;
        elem_dim.get_head(&ct);
        for (UINT i = 0; i < elem_dim.get_elem_count(); i++) {
            elem_dim_buf[i] = ct->val();
            elem_dim.get_next(&ct);
        }
    }

    if (elem_dim.get_elem_count() != 0 &&
        xcom::cnt_list(subscript_list) != elem_dim.get_elem_count()) {
        error("declare %d dimension array, but %d subscript given",
            elem_dim.get_elem_count(), xcom::cnt_list(subscript_list));
        return false;
    }

    IR * array = ctx->current_region->buildArray(base, subscript_list,
        ty == NULL ? m_tm->getVoid() : ty,
        elem_ty == NULL ? m_tm->getVoid() : elem_ty,
        xcom::cnt_list(subscript_list), elem_dim_buf);
    ARR_ofst(array) = offset;
    ctx->returned_exp = array;
    return true;
}


bool IRParser::parseIld(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_ILD);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = NULL;
    if (tok == T_COLON) {
        //Type
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            return false;
        }
    }
    if (ty == NULL) {
        ty = m_tm->getVoid();
    }

    tok = m_lexer->getCurrentToken();
    UINT offset = 0;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        offset = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Properties
    PropertySet cont;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_ILD;
        if (!parseProperty(cont, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
    }

    if (!parseExp(ctx)) {
        return false;
    }
    if (ctx->returned_exp == NULL) {
        error(tok, "illegal base expression of ild");
        return false;
    }
    IR * base = ctx->returned_exp;
    ctx->returned_exp = NULL;

    IR * ild = ctx->current_region->buildIload(base, ty);
    ILD_ofst(ild) = offset;
    copyProp(ild, cont);
    ctx->returned_exp = ild;
    return true;
}


bool IRParser::parseLd(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_LD);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "invalid type");
            return false;
        }
    }

    tok = m_lexer->getCurrentToken();
    UINT offset = 0;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        offset = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Properties
	PropertySet cont;
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_LD;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	}

	tok = m_lexer->getCurrentToken();
    if (tok != T_IDENTIFIER) {
        error(tok, "miss variable name");
        return false;
    }

    VAR * var = findVar(ctx, m_rumgr->addToSymbolTab(
        m_lexer->getCurrentTokenString()));
    if (var == NULL) {
        error(tok, "%s is not declared", m_lexer->getCurrentTokenString());
        return false;
    }

    IR * ld = ctx->current_region->buildLoad(var,
        ty == NULL ? var->get_type() : ty);
    LD_ofst(ld) = offset;
    ctx->returned_exp = ld;
    copyProp(ld, cont);
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseSignImm(TOKEN tok, ParseCtx * ctx)
{
    m_lexer->getNextToken();
    bool res = parseExp(ctx);
    ASSERT0(ctx->returned_exp);
    switch (tok) {
    case T_ADD:
        return res;
    case T_SUB:
        if (ctx->returned_exp->is_int()) {
            CONST_int_val(ctx->returned_exp) = -CONST_int_val(ctx->returned_exp);
        } else if (ctx->returned_exp->is_fp()) {
            CONST_fp_val(ctx->returned_exp) = -CONST_fp_val(ctx->returned_exp);
        } else {
            error(tok, "illegal arithmetic operation to literal");
            return false;
        }
        break;
    default:
        error(tok, "illegal arithmetic operation to literal");
        return false;
    }
    return res;
}


bool IRParser::parseImm(ParseCtx * ctx)
{
    ASSERT0(m_lexer->getCurrentToken() == T_IMM);

    HOST_INT v = xcom::xatoll(m_lexer->getCurrentTokenString(), false);
    TOKEN tok = m_lexer->getNextToken();
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    } else {
        ty = m_tm->getI32();
    }
    IR * imm = ctx->current_region->buildImmInt(v, ty);
    ctx->returned_exp = imm;
    return true;
}


bool IRParser::parseFp(ParseCtx * ctx)
{
    ASSERT0(m_lexer->getCurrentToken() == T_FP);

    HOST_FP v = atof(m_lexer->getCurrentTokenString());
    TOKEN tok = m_lexer->getNextToken();
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    } else {
        ty = m_tm->getF64();
    }
    IR * fp = ctx->current_region->buildImmFp(v, ty);
    ctx->returned_exp = fp;
    return true;
}


bool IRParser::parseExpList(ParseCtx * ctx)
{
    IR * last = NULL;
    IR * param = NULL;
    for (; isExp();) {
        if (!parseExp(ctx)) {
            return false;
        }
        xcom::add_next(&param, &last, ctx->returned_exp);
        ctx->returned_exp = NULL;
        if (m_lexer->getCurrentToken() == T_COMMA) {
            m_lexer->getNextToken();
        } else {
            ctx->returned_exp = param;
            return true;
        }
    }
    ctx->returned_exp = param;
    return true;
}


bool IRParser::parseExp(ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getCurrentToken();
    switch (tok) {
    case T_LPAREN:
        m_lexer->getNextToken();
        if (!parseExp(ctx)) {
            return false;
        }
        tok = m_lexer->getCurrentToken();
        if (tok != T_RPAREN) {
            error(tok, "miss ')'");
            return false;
        }
        m_lexer->getNextToken();
        return true;
    case T_IDENTIFIER:
        if (getCurrentXCode() != X_UNDEF) {
            return parseXOperator(ctx);
        }
        break;
    case T_ADD:
    case T_SUB:
        return parseSignImm(tok, ctx);
    case T_IMM:
        return parseImm(ctx);
    case T_FP:
        return parseFp(ctx);
    case T_STRING:
        ctx->returned_exp = ctx->current_region->buildString(
            m_rumgr->addToSymbolTab(m_lexer->getCurrentTokenString()));
        return true;
    case T_TRUE:
        ctx->returned_exp = ctx->current_region->buildImmInt(
            1, m_tm->getBool());
        return true;
    case T_FALSE:
        ctx->returned_exp = ctx->current_region->buildImmInt(
            0, m_tm->getBool());
        return true;
    case T_DOLLAR: //$
        return parsePR(ctx);
    default:;
    }

    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::isExp(X_CODE code)
{
    switch (code) {
    case X_ID:
    case X_LD:
    case X_ILD:
    case X_ARRAY:
    case X_LDA:
    case X_ADD:
    case X_SUB:
    case X_MUL:
    case X_DIV:
    case X_REM:
    case X_MOD:
    case X_LAND:
    case X_LOR:
    case X_BAND:
    case X_BOR:
    case X_XOR:
    case X_ASR:
    case X_LSR:
    case X_LSL:
    case X_LT:
    case X_LE:
    case X_GT:
    case X_GE:
    case X_EQ:
    case X_NE:
    case X_BNOT:
    case X_LNOT:
    case X_NEG:
    case X_CVT:
    case X_SELECT:
        return true;
    case X_REGION:
    case X_ST:
    case X_STRP:
    case X_SETELEM:
    case X_GETELEM:
    case X_IST:
    case X_CALL:
    case X_ICALL:
    case X_GOTO:
    case X_IGOTO:
    case X_DO:
    case X_WHILE:
    case X_DO_LOOP:
    case X_LABEL:
    case X_TRUEBR:
    case X_FALSEBR:
    case X_PHI:
    case X_IF:
    case X_BREAK:
    case X_RETURN:
    case X_CONTINUE:
    case X_SWITCH:
        return false;
    default:;
    }
    return true; //normal identifier
}


bool IRParser::isExp()
{
    TOKEN tok = m_lexer->getCurrentToken();
    switch (tok) {
    case T_IDENTIFIER:
        return isExp(getCurrentXCode());
    case T_IMM:
    case T_FP:
    case T_STRING:
    case T_TRUE:
    case T_FALSE:
    case T_ADD:          // +
    case T_SUB:          // -
    case T_ASTERISK:     // *
    case T_DIV:          // /
    case T_AND:          // &&
    case T_OR:           // ||
    case T_BITAND:       // &
    case T_BITOR:        // |
    case T_LESSTHAN:     // <
    case T_MORETHAN:     // >
    case T_RSHIFT:       // >>
    case T_LSHIFT:       // <<
    case T_NOMORETHAN:   // <=
    case T_NOLESSTHAN:   // >=
    case T_NOEQU:        // !=
    case T_NOT:          // !
    case T_EQU:          // ==
    case T_XOR:          // ^
    case T_MOD:          // %
    case T_REV:          // ~ reverse  e.g:a = ~a
    case T_DOLLAR:       //$
        return true;
    default:;
    }
    return false;
}


bool IRParser::parseBinaryOp(IR_TYPE code, ParseCtx * ctx)
{
    ASSERT0(isBinaryOp(code));
    TOKEN tok = m_lexer->getNextToken();
    ctx->returned_exp = NULL;

    //Type
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            return false;
        }
    }

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = code;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	}

    if (!parseExp(ctx)) {
        return false;
    }

    IR * opnd0 = ctx->returned_exp;
    ASSERT0(opnd0);
    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ','");
        return false;
    }

    tok = m_lexer->getNextToken();
    ctx->returned_exp = NULL;
    if (!parseExp(ctx)) {
        return false;
    }
    IR * opnd1 = ctx->returned_exp;
    ASSERT0(opnd1 && ctx->current_region);

    if (ty == NULL) {
        switch (code) {
        case IR_LT:
        case IR_LE:
        case IR_GT:
        case IR_GE:
        case IR_EQ:
        case IR_NE:
        case IR_LAND:
        case IR_LOR:
            ty = m_tm->getBool();
            break;
        default:
            ty = m_tm->getVoid();
        }
    }

    IR * exp = ctx->current_region->buildBinaryOpSimp(code, ty, opnd0, opnd1);
    ctx->returned_exp = exp;
    copyProp(exp, cont);
    return true;
}


bool IRParser::parseCvt(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_CVT);
    TOKEN tok = m_lexer->getNextToken();
    ctx->returned_exp = NULL;

    Type const* ty = NULL;
    if (tok != T_COLON) {
        error(tok, "miss result type");
        return false;
    }
    tok = m_lexer->getNextToken();
    if (!parseType(ctx, &ty) || ty == NULL) {
        error(tok, "illegal result type");
        return false;
    }

    if (!parseExp(ctx)) {
        return false;
    }

    IR * opnd = ctx->returned_exp;
    ASSERT0(opnd);

    if (ty == NULL) {
        ty = m_tm->getVoid();
    }

    IR * exp = ctx->current_region->buildUnaryOp(IR_CVT, ty, opnd);
    ctx->returned_exp = exp;
    return true;
}


bool IRParser::parseUnaryOp(IR_TYPE code, ParseCtx * ctx)
{
    ASSERT0(isUnaryOp(code));
    TOKEN tok = m_lexer->getNextToken();
    ctx->returned_exp = NULL;

    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    }

    if (!parseExp(ctx)) {
        return false;
    }

    IR * opnd = ctx->returned_exp;
    ASSERT0(opnd);

    if (ty == NULL) {
        ty = m_tm->getVoid();
    }

    IR * exp = ctx->current_region->buildUnaryOp(code, ty, opnd);
    ctx->returned_exp = exp;
    return true;
}


bool IRParser::parseOperator(ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getCurrentToken();
    switch (tok) {
    case T_IDENTIFIER:
        if (!parseXOperator(ctx)) {
            return false;
        }
        break;
    case T_ADD:
        if (!parseBinaryOp(IR_ADD, ctx)) {
            return false;
        }
        break;
    case T_SUB:
        if (!parseBinaryOp(IR_SUB, ctx)) {
            return false;
        }
        break;
    case T_ASTERISK:
        if (!parseBinaryOp(IR_MUL, ctx)) {
            return false;
        }
        break;
    case T_DIV:
        if (!parseBinaryOp(IR_DIV, ctx)) {
            return false;
        }
        break;
    case T_AND:
        if (!parseBinaryOp(IR_LAND, ctx)) {
            return false;
        }
        break;
    case T_OR:
        if (!parseBinaryOp(IR_LOR, ctx)) {
            return false;
        }
        break;
    case T_BITOR:
        if (!parseBinaryOp(IR_BOR, ctx)) {
            return false;
        }
        break;
    case T_BITAND:
        if (!parseBinaryOp(IR_BAND, ctx)) {
            return false;
        }
        break;
    case T_LESSTHAN:
        if (!parseBinaryOp(IR_LT, ctx)) {
            return false;
        }
        break;
    case T_MORETHAN:
        if (!parseBinaryOp(IR_GT, ctx)) {
            return false;
        }
        break;
    case T_RSHIFT:       // >>
        if (!parseBinaryOp(IR_LSR, ctx)) {
            return false;
        }
        break;
    case T_LSHIFT:       // <<
        if (!parseBinaryOp(IR_LSL, ctx)) {
            return false;
        }
        break;
    case T_NOMORETHAN:   // <=
        if (!parseBinaryOp(IR_LE, ctx)) {
            return false;
        }
        break;
    case T_NOLESSTHAN:   // >=
        if (!parseBinaryOp(IR_GE, ctx)) {
            return false;
        }
        break;
    case T_NOEQU:        // !=
        if (!parseBinaryOp(IR_NE, ctx)) {
            return false;
        }
        break;
    case T_NOT:          // !
        if (!parseUnaryOp(IR_LNOT, ctx)) {
            return false;
        }
        break;
    case T_EQU:          // ==
        if (!parseBinaryOp(IR_EQ, ctx)) {
            return false;
        }
        break;
    case T_XOR:          // ^
        if (!parseBinaryOp(IR_XOR, ctx)) {
            return false;
        }
        break;
    case T_MOD:          // %
        if (!parseBinaryOp(IR_MOD, ctx)) {
            return false;
        }
        break;
    case T_REV:          // ~ reverse  e.g:a = ~a
        if (!parseUnaryOp(IR_BOR, ctx)) {
            return false;
        }
        break;
    default:
        error(tok, "not operator");
        tok = m_lexer->getNextToken();
    }

    return true;
}

bool IRParser::parseStore(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_ST);
    TOKEN tok = m_lexer->getNextToken();

	//Type
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            return false;
        }
    }

	//Offset
    tok = m_lexer->getCurrentToken();
    UINT offset = 0;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        offset = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

	//Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_ST;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	}

    tok = m_lexer->getCurrentToken();
    if (tok != T_IDENTIFIER) {
        error(tok, "miss variable name to be stored");
        return false;
    }

    VAR * var = findVar(ctx, m_rumgr->addToSymbolTab(
        m_lexer->getCurrentTokenString()));
    if (var == NULL) {
        error(tok, "%s is not declared", m_lexer->getCurrentTokenString());
        return false;
    }

    tok = m_lexer->getNextToken();
    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }

    tok = m_lexer->getNextToken();
    if (!parseExp(ctx)) {
        return false;
    }

    IR * ir = NULL;
    if (ty == NULL) {
        ir = ctx->current_region->buildStore(var, ctx->returned_exp);
    } else {
        ir = ctx->current_region->buildStore(var, ty, ctx->returned_exp);
    }
    ST_ofst(ir) = offset;
    ctx->addIR(ir);
    ctx->returned_exp = NULL;
    copyProp(ir, cont);
    return true;
}


bool IRParser::parseStorePR(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_STRP);
    TOKEN tok = m_lexer->getNextToken();

    if (tok != T_DOLLAR) {
        error(tok, "miss $ specifier");
        return false;
    }

    tok = m_lexer->getNextToken();
    UINT prno = 0;
    if (tok == T_IMM) {
        prno = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        ctx->current_region->setPRCount(
            MAX(ctx->current_region->getPRCount(), prno));
    } else {
        error(tok, "miss PR number");
        return false;
    }

    tok = m_lexer->getNextToken();
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    } else {
        ty = m_tm->getVoid();
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }

    tok = m_lexer->getNextToken();
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * ir = ctx->current_region->buildStorePR(prno, ty, ctx->returned_exp);
    ctx->addIR(ir);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseModifyPR(X_CODE code, ParseCtx * ctx)
{
    ASSERT0(code == X_SETELEM || code == X_GETELEM);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = code == X_SETELEM ? IR_SETELEM : IR_GETELEM;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    if (tok != T_DOLLAR) {
        error(tok, "miss '$' specifier after %s", getKeywordName(code));
        return false;
    }

    tok = m_lexer->getNextToken();
    UINT prno = 0;
    if (tok == T_IMM) {
        prno = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        ctx->current_region->setPRCount(
            MAX(ctx->current_region->getPRCount(), prno));
    } else {
        error(tok, "miss PR number");
        return false;
    }

    tok = m_lexer->getNextToken();
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    } else {
        ty = m_tm->getVoid();
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }

    tok = m_lexer->getNextToken();
    if (!parseExp(ctx)) {
        return false;
    }
    IR * rhs = ctx->returned_exp;

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ','");
        return false;
    }

    tok = m_lexer->getNextToken();
    if (!parseExp(ctx)) {
        return false;
    }
    IR * offset = ctx->returned_exp;

    IR * ir;
    if (code == X_GETELEM) {
        ir = ctx->current_region->buildGetElem(prno, ty, rhs, offset);
    } else {
        ir = ctx->current_region->buildSetElem(prno, ty, rhs, offset);
    }
    ctx->addIR(ir);
    ctx->returned_exp = NULL;
    copyProp(ir, cont);
    return true;
}


bool IRParser::parseIStore(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_IST);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    } else {
        ty = m_tm->getVoid();
    }

	//Offset
    tok = m_lexer->getCurrentToken();
    UINT offset = 0;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        offset = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_IST;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }

    tok = m_lexer->getNextToken();
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * base = ctx->returned_exp;
    ctx->returned_exp = NULL;

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ',' after base expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * rhs = ctx->returned_exp;
    ctx->returned_exp = NULL;

    IR * ir = ctx->current_region->buildIstore(base, rhs, ty);
    copyProp(ir, cont);
    IST_ofst(ir) = offset;
    ctx->addIR(ir);
    return true;
}


bool IRParser::parseCallAndICall(bool is_call, ParseCtx * ctx)
{
    ASSERT0(is_call ? getCurrentXCode() == X_CALL :
            getCurrentXCode() == X_ICALL);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = is_call ? IR_CALL : IR_ICALL;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	}
	tok = m_lexer->getCurrentToken();

    UINT return_prno = 0;
    Type const* return_ty = m_tm->getVoid();
    if (tok == T_DOLLAR) {
        //Expect return value.
        tok = m_lexer->getNextToken();
        if (tok == T_IMM) {
            return_prno = (UINT)xcom::xatoll(
                m_lexer->getCurrentTokenString(), false);
            ctx->current_region->setPRCount(
                MAX(ctx->current_region->getPRCount(), return_prno));
        }
        else {
            error(tok, "miss PR number");
            return false;
        }

        tok = m_lexer->getNextToken();
        if (tok == T_COLON) {
            tok = m_lexer->getNextToken();
            if (!parseType(ctx, &return_ty) || return_ty == NULL) {
                return false;
            }
        } else {
            return_ty = m_tm->getVoid();
        }

        tok = m_lexer->getCurrentToken();
        if (tok != T_ASSIGN) {
            error(tok, "miss '='");
            return false;
        }
        tok = m_lexer->getNextToken();
    }

    VAR * callee_var = NULL;
    IR * callee_exp = NULL;
    if (is_call) {
        if (tok != T_IDENTIFIER) {
            error(tok, "miss callee function name");
            return false;
        }

        SYM const* name = m_rumgr->addToSymbolTab(
            m_lexer->getCurrentTokenString());
        callee_var = findVar(ctx, name);
        if (callee_var == NULL) {
            error(tok, "can not find region %s", SYM_name(name));
            return false;
        }
        if (!callee_var->is_func_decl()) {
            error(tok, "%s is not region", SYM_name(name));
            return false;
        }
    } else {
        if (!parseExp(ctx)) {
            error(tok, "illegal callee expression");
            return false;
        }
        ASSERT0(ctx->returned_exp);
        callee_exp = ctx->returned_exp;
        ctx->returned_exp = NULL;
        if (callee_exp->get_next() != NULL) {
            error(tok, "multiple callee expression");
            return false;
        }
        tok = m_lexer->getCurrentToken();
        if (tok != T_COMMA) {
            error(tok, "miss ',' after callee expression");
            return false;
        }
    }

    tok = m_lexer->getNextToken();
    if (tok != T_LPAREN) {
        error(tok, "miss '('");
        return false;
    }
    tok = m_lexer->getNextToken();
    if (!parseExpList(ctx)) {
        return false;
    }
    if (m_lexer->getCurrentToken() != T_RPAREN) {
        error(tok, "miss ')'");
        return false;
    }
    tok = m_lexer->getNextToken();
    IR * param_list = ctx->returned_exp;

    IR * dummy_use_list = NULL;
    if (tok == T_COLON) {
        //Dummy use.
        tok = m_lexer->getNextToken();
        if (tok != T_LPAREN) {
            error(tok, "miss '('");
            return false;
        }
        tok = m_lexer->getNextToken();
        if (!parseExpList(ctx)) {
            return false;
        }
        if (m_lexer->getCurrentToken() != T_RPAREN) {
            error(tok, "miss ')'");
            return false;
        }
        m_lexer->getNextToken();
        dummy_use_list = ctx->returned_exp;
    }

    IR * ir = NULL;
    if (is_call) {
        ir = ctx->current_region->buildCall(
            callee_var, param_list, return_prno, return_ty);
    } else {
        ir = ctx->current_region->buildIcall(
            callee_exp, param_list, return_prno, return_ty);
    }
    if (cont.ir_use_list != NULL) {
        CALL_dummyuse(ir) = cont.ir_use_list;
        ir->setParent(CALL_dummyuse(ir));
    }
	copyProp(ir, cont);
    ctx->addIR(ir);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseGoto(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_GOTO);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_GOTO;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    if (tok != T_IDENTIFIER) {
        error(tok, "miss label after goto");
        return false;
    }
    SYM const* sym = m_rumgr->addToSymbolTab(
        m_lexer->getCurrentTokenString());
    LabelInfo * label = ctx->mapSym2Label(sym);
    if (label == NULL) {
        label = ctx->current_region->genCustomLabel(sym);
        ctx->setMapSym2Label(sym, label);
    }
    IR * ir = ctx->current_region->buildGoto(label);
    ctx->addIR(ir);
    m_lexer->getNextToken();
    copyProp(ir, cont);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseIGoto(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_IGOTO);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_IGOTO;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    //Igoto determinate expression
    if (!parseExp(ctx)) {
        return false;
    }
    if (ctx->returned_exp == NULL) {
        error(tok, "miss determinate expression");
        return false;
    }
    IR * det = ctx->returned_exp;
    ctx->returned_exp = NULL;
    if (det->get_next() != NULL) {
        error(tok, "multiple determinate expression");
        return false;
    }

    //Case list
    IR * case_list = NULL;
    IR * last = NULL;
    for (;;) {
        if (!parseExp(ctx)) {
            error(tok, "illegal case expression");
            return false;
        }
        IR * case_exp = ctx->returned_exp;
        if (case_exp == NULL) {
            break;
        }
        ctx->returned_exp = NULL;

        xcom::add_next(&case_list, &last, case_exp);

        tok = m_lexer->getCurrentToken();
        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
            continue;
        } else if (isTerminator(tok)) {
            break;
        } else {
            error(tok, "illegal case list");
            return false;
        }
    }
    if (case_list == NULL) {
        error(tok, "miss case list");
        return false;
    }

    IR * ir = ctx->current_region->buildIgoto(det, case_list);
    copyProp(ir, cont);
    ctx->addIR(ir);
    return true;
}


bool IRParser::parseDoWhile(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_DO);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_DO_WHILE;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    //body
    if (tok != T_LLPAREN) {
        error(tok, "miss '{' before if body");
        return false;
    }
    m_lexer->getNextToken();

    IR * oldvalue1;
    IR * oldvalue2;
    ctx->storeValue(&oldvalue1, &oldvalue2);
    parseStmtList(ctx);
    IR * body = ctx->stmt_list;
    ctx->reloadValue(oldvalue1, oldvalue2);

    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "miss '}' after if body");
        return false;
    }
    m_lexer->getNextToken();

    if (getCurrentXCode() != X_WHILE) {
        error(tok, "miss 'while' after loop body");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Det
    if (!parseExp(ctx)) {
        return false;
    }
    IR * det = ctx->returned_exp;
    ASSERT0(det);
    ctx->returned_exp = NULL;

    IR * ir = ctx->current_region->buildDoWhile(det, body);
    ctx->addIR(ir);
    copyProp(ir, cont);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseWhileDo(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_WHILE);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_WHILE_DO;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    if (!parseExp(ctx)) {
        return false;
    }
    IR * det = ctx->returned_exp;
    ASSERT0(det);
    ctx->returned_exp = NULL;

    //body
    tok = m_lexer->getCurrentToken();
    if (tok != T_LLPAREN) {
        error(tok, "miss '{' before if body");
        return false;
    }
    m_lexer->getNextToken();

    IR * oldvalue1;
    IR * oldvalue2;
    ctx->storeValue(&oldvalue1, &oldvalue2);
    parseStmtList(ctx);
    IR * body = ctx->stmt_list;
    ctx->reloadValue(oldvalue1, oldvalue2);

    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "miss '}' after if body");
        return false;
    }
    m_lexer->getNextToken();

    IR * ir = ctx->current_region->buildWhileDo(det, body);
    ctx->addIR(ir);
    copyProp(ir, cont);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseDoLoop(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_DO_LOOP);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_DO_LOOP;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    if (tok != T_LPAREN) {
        error(tok, "miss '(' after doloop operator");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Induction variable
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * iv = ctx->returned_exp;
    ctx->returned_exp = NULL;
    if (!iv->is_id() && !iv->is_pr()) {
        error(tok, "induction variable must be ID or PR");
        return false;
    } else if (iv->get_next() != NULL) {
        error(tok, "multiple induction variable expression");
        return false;
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ',' after induction variable expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Initial value expression
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * init = ctx->returned_exp;
    ctx->returned_exp = NULL;
    if (init->get_next() != NULL) {
        error(tok, "multiple initial value expression");
        return false;
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ',' after initial value expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Determinate expression
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * det = ctx->returned_exp;
    ctx->returned_exp = NULL;
    if (det->get_next() != NULL) {
        error(tok, "multiple determinate expression");
        return false;
    }

    //if (iv->is_id()) {
    //    det = ctx->current_region->buildCmp(IR_LE,
    //        ctx->current_region->buildLoad(ID_info(iv)), det);
    //} else {
    //    ASSERT0(iv->is_pr());
    //    det = ctx->current_region->buildCmp(IR_LE,
    //        ctx->current_region->dupIR(iv), det);
    //}

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ',' after termination expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Stride expression
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * step = ctx->returned_exp;
    ctx->returned_exp = NULL;
    if (step->get_next() != NULL) {
        error(tok, "multiple step expression");
        return false;
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_RPAREN) {
        error(tok, "miss ')' after step expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    //Doloop body
    if (tok != T_LLPAREN) {
        error(tok, "miss '{' before doloop body");
        return false;
    }
    tok = m_lexer->getNextToken();

    IR * oldvalue1;
    IR * oldvalue2;
    ctx->storeValue(&oldvalue1, &oldvalue2);
    parseStmtList(ctx);
    IR * body = ctx->stmt_list;
    ctx->reloadValue(oldvalue1, oldvalue2);

    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "miss '}' after doloop body");
        return false;
    }

    IR * ir = ctx->current_region->buildDoLoop(iv, init, det, step, body);
    ctx->addIR(ir);
    copyProp(ir, cont);
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseLabelProperty(LabelInfo * label)
{
    ASSERT0(m_lexer->getCurrentToken() == T_LPAREN);
    m_lexer->getNextToken();
    for (;;) {
        switch (getCurrentXCode()) {
        case X_TRY_START:
            LABEL_INFO_is_try_start(label) = true;
            m_lexer->getNextToken();
            break;
        case X_TRY_END:
            LABEL_INFO_is_try_end(label) = true;
            m_lexer->getNextToken();
            break;
        case X_TERMINATE:
            LABEL_INFO_is_terminate(label) = true;
            m_lexer->getNextToken();
            break;
        case X_CATCH_START:
            LABEL_INFO_is_catch_start(label) = true;
            m_lexer->getNextToken();
            break;
        default:
            error(m_lexer->getCurrentToken(), "illegal label property");
            return false;
        }

        TOKEN tok = m_lexer->getCurrentToken();
        if (tok == T_COMMA) {
            m_lexer->getNextToken();
            continue;
        } else if (tok == T_RPAREN) {
            m_lexer->getNextToken();
            break;
        } else {
            error(tok, "illegal label declaration");
            return false;
        }
    }
    return true;
}


bool IRParser::parseLabel(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_LABEL);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_LABEL;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    if (tok != T_IDENTIFIER) {
        error(tok, "illegal label");
        return false;
    }
    SYM const* sym = m_rumgr->addToSymbolTab(m_lexer->getCurrentTokenString());
    LabelInfo * label = ctx->mapSym2Label(sym);
    if (label == NULL) {
        label = ctx->current_region->genCustomLabel(sym);
        ctx->setMapSym2Label(sym, label);
    }
    tok = m_lexer->getNextToken();

    if (tok == T_LPAREN) {
        if (!parseLabelProperty(label)) {
            return false;
        }
    }

    IR * ir = ctx->current_region->buildLabel(label);
    ctx->addIR(ir);
    copyProp(ir, cont);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseBranch(bool is_truebr, ParseCtx * ctx)
{
    ASSERT0(is_truebr ? getCurrentXCode() == X_TRUEBR :
            getCurrentXCode() == X_FALSEBR);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = is_truebr ? IR_TRUEBR : IR_FALSEBR;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	}

	tok = m_lexer->getCurrentToken();
    if (!parseExp(ctx)) {
        error(tok, "illegal determinate-expression");
        return false;
    }
    IR * det = ctx->returned_exp;
    ASSERT0(det);
    if (det->get_next() != NULL) {
        error(tok, "multiple determinate-expression");
        return false;
    }
    ctx->returned_exp = NULL;

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ',' after determinant-expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    if (tok != T_IDENTIFIER) {
        error(tok, "miss label after determinate-expression");
        return false;
    }
    SYM const* sym = m_rumgr->addToSymbolTab(
        m_lexer->getCurrentTokenString());
    LabelInfo * label = ctx->mapSym2Label(sym);
    if (label == NULL) {
        label = ctx->current_region->genCustomLabel(sym);
        ctx->setMapSym2Label(sym, label);
    }
    m_lexer->getNextToken();

    IR * ir = ctx->current_region->buildBranch(is_truebr, det, label);
    ctx->addIR(ir);
    copyProp(ir, cont);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parsePhi(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_PHI);
    TOKEN tok = m_lexer->getNextToken();

    //Result
    if (tok != T_DOLLAR) {
        error(tok, "miss $ specifier");
        return false;
    }

    tok = m_lexer->getNextToken();
    UINT prno = 0;
    if (tok == T_IMM) {
        prno = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        ctx->current_region->setPRCount(
            MAX(ctx->current_region->getPRCount(), prno));
    } else {
        error(tok, "miss PR number");
        return false;
    }

    tok = m_lexer->getNextToken();
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == NULL) {
            return false;
        }
    } else {
        ty = m_tm->getVoid();
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }
    tok = m_lexer->getNextToken();

    IR * opnd_list = NULL;
    IR * last = NULL;
    for (;;) {
        if (tok != T_LPAREN) {
            error(tok, "phi operand should be bracketed by '()'");
            return false;
        }
        tok = m_lexer->getNextToken();

        //Phi operand
        if (!parseExp(ctx)) {
            return false;
        }
        ASSERT0(ctx->returned_exp);
        IR * opnd = ctx->returned_exp;
        ctx->returned_exp = NULL;
        if (!opnd->is_pr() && !opnd->is_const()) {
            error(tok, "phi operand should be $PR or constant");
            return false;
        }
        xcom::add_next(&opnd_list, &last, opnd);

        tok = m_lexer->getCurrentToken();
        if (tok != T_COMMA) {
            error(tok, "miss ',' after phi operand");
            return false;
        }
        tok = m_lexer->getNextToken();

        //Label
        if (tok != T_IDENTIFIER) {
            error(tok, "miss label after phi operand");
            return false;
        }
        SYM const* sym = m_rumgr->addToSymbolTab(
            m_lexer->getCurrentTokenString());
        LabelInfo * label = ctx->mapSym2Label(sym);
        if (label == NULL) {
            label = ctx->current_region->genCustomLabel(sym);
            ctx->setMapSym2Label(sym, label);
        }
        tok = m_lexer->getNextToken();

        if (tok != T_RPAREN) {
            error(tok, "miss ')' after phi operand declaration");
            return false;
        }
        tok = m_lexer->getNextToken();

        ctx->setMapIR2Label(opnd, label);

        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
            continue;
        } else if (tok == T_SEMI) {
            break;
        } else {
            error(tok, "illegal %s in phi operation declaration",
                m_lexer->getCurrentTokenString());
        }
    }

    IR * ir = ctx->current_region->buildPhi(prno, ty, opnd_list);
    ctx->addIR(ir);
    return true;
}


bool IRParser::parseIf(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_IF);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_IF;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    if (!parseExp(ctx)) {
        return false;
    }
    IR * det = ctx->returned_exp;
    ASSERT0(det);
    ctx->returned_exp = NULL;
    tok = m_lexer->getCurrentToken();

    //truebody
    if (tok != T_LLPAREN) {
        error(tok, "miss '{' before if body");
        return false;
    }
    m_lexer->getNextToken();

    IR * oldvalue1;
    IR * oldvalue2;
    ctx->storeValue(&oldvalue1, &oldvalue2);
    parseStmtList(ctx);
    IR * truebody = ctx->stmt_list;
    ctx->reloadValue(oldvalue1, oldvalue2);

    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "miss '}' after if body");
        return false;
    }

    IR * falsebody = NULL;
    tok = m_lexer->getNextToken();
    if (tok == T_IDENTIFIER && getCurrentXCode() == X_ELSE) {
        tok = m_lexer->getNextToken();
        if (tok != T_LLPAREN) {
            error(tok, "miss '{' before if-else body");
            return false;
        }
        m_lexer->getNextToken();

        IR * oldvalue3;
        IR * oldvalue4;
        ctx->storeValue(&oldvalue3, &oldvalue4);
        parseStmtList(ctx);
        falsebody = ctx->stmt_list;
        ctx->reloadValue(oldvalue3, oldvalue4);

        tok = m_lexer->getCurrentToken();
        if (tok != T_RLPAREN) {
            error(tok, "miss '}' after if-else body");
            return false;
        }
        m_lexer->getNextToken();
    }

    IR * ir = ctx->current_region->buildIf(det, truebody, falsebody);
    ctx->addIR(ir);
    copyProp(ir, cont);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseBreak(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_BREAK);
    ctx->addIR(ctx->current_region->buildBreak());
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseReturn(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_RETURN);
    m_lexer->getNextToken();
    ctx->returned_exp = NULL;
    if (!parseExp(ctx)) {
        return false;
    }
    IR * ir = ctx->current_region->buildReturn(ctx->returned_exp);
    ctx->addIR(ir);
    ctx->returned_exp = NULL;
    return true;
}


bool IRParser::parseContinue(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_CONTINUE);
    ctx->addIR(ctx->current_region->buildContinue());
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseSwitch(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_SWITCH);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
	PropertySet cont;
	tok = m_lexer->getCurrentToken();
	if (tok == T_COLON) {
		tok = m_lexer->getNextToken();
        ctx->ircode = IR_SWITCH;
		if (!parseProperty(cont, ctx)) {
			error(tok, "illegal property declaration");
			return false;
		}
        ctx->ircode = IR_UNDEF;
	    tok = m_lexer->getCurrentToken();
	}

    //Switch determinate expression
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * det = ctx->returned_exp;
    ctx->returned_exp = NULL;
    if (det->get_next() != NULL) {
        error(tok, "multiple determinate expression");
        return false;
    }

    //Case list and default label.
    IR * case_list = NULL;
    IR * last = NULL;
    LabelInfo * deflab = NULL;
    for (;;) {
        if (getCurrentXCode() == X_DEFAULT) {
            tok = m_lexer->getNextToken();
            if (tok != T_IDENTIFIER) {
                error(tok, "illegal default label");
                return false;
            }
            SYM const* sym = m_rumgr->addToSymbolTab(
                m_lexer->getCurrentTokenString());
            deflab = ctx->mapSym2Label(sym);
            if (deflab == NULL) {
                deflab = ctx->current_region->genCustomLabel(sym);
                ctx->setMapSym2Label(sym, deflab);
            }
            tok = m_lexer->getNextToken();
        } else {
            if (!parseExp(ctx)) {
                error(tok, "illegal case expression");
                return false;
            }
            IR * case_exp = ctx->returned_exp;
            if (case_exp == NULL) {
                break;
            }
            ctx->returned_exp = NULL;
            xcom::add_next(&case_list, &last, case_exp);
        }
        tok = m_lexer->getCurrentToken();
        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
            continue;
        } else if (tok == T_LLPAREN) {
            break;
        } else {
            error(tok, "illegal case list");
            return false;
        }
    }

    //Switch body
    tok = m_lexer->getCurrentToken();
    if (tok != T_LLPAREN) {
        error(tok, "miss '{' before switch body");
        return false;
    }
    tok = m_lexer->getNextToken();

    IR * oldvalue1;
    IR * oldvalue2;
    ctx->storeValue(&oldvalue1, &oldvalue2);
    parseStmtList(ctx);
    IR * body = ctx->stmt_list;
    ctx->reloadValue(oldvalue1, oldvalue2);

    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "miss '}' after doloop body");
        return false;
    }
    m_lexer->getNextToken();

    IR * ir = ctx->current_region->buildSwitch(
        det, case_list, body, deflab);
    copyProp(ir, cont);
    ctx->addIR(ir);
    return true;
}


bool IRParser::parseParameterList(ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getCurrentToken();
    if (tok != T_LPAREN) {
        error(tok, "parameter list miss '('");
        return false;
    }
    tok = m_lexer->getNextToken();

    UINT i = 0;
    for (; tok != T_RPAREN && tok != T_END && tok != T_NUL; i++) {
        VAR * v = NULL;
        if (getCurrentXCode() == X_UNDEFINED) {
            //The parameter is reserved.
            tok = m_lexer->getNextToken();
        } else if(declareVar(ctx, &v)) {
            ASSERT0(v);
            VAR_is_formal_param(v) = true;
            VAR_formal_param_pos(v) = i;
        } else {
            return false;
        }

        tok = m_lexer->getCurrentToken();
        if (tok == T_RPAREN) {
            break;
        }
        if (tok != T_COMMA) {
            error(tok, "miss ',' after parameter declaration");
            return false;
        }
        tok = m_lexer->getNextToken();
    }

    if (tok != T_RPAREN) {
        error(tok, "parameter list miss ')'");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseType(ParseCtx * ctx, Type const** ty)
{
    if (m_lexer->getCurrentToken() == T_ASTERISK) {
        TOKEN tok = m_lexer->getNextToken();
        UINT size = 0;
        if (!parseSize(tok, &size)) {
            error(tok, "pointer type should specify a base size");
            return false;
        }
        *ty = m_tm->getPointerType(size);
        return true;
    }

    X_CODE code = getCurrentXCode();
    UINT size = 0;
    switch (code) {
    TOKEN tok;
    case X_I8:
        *ty = m_tm->getI8();
        m_lexer->getNextToken();
        break;
    case X_U8:
        *ty = m_tm->getU8();
        m_lexer->getNextToken();
        break;
    case X_I16:
        *ty = m_tm->getI16();
        m_lexer->getNextToken();
        break;
    case X_U16:
        *ty = m_tm->getU16();
        m_lexer->getNextToken();
        break;
    case X_I32:
        *ty = m_tm->getI32();
        m_lexer->getNextToken();
        break;
    case X_U32:
        *ty = m_tm->getU32();
        m_lexer->getNextToken();
        break;
    case X_I64:
        *ty = m_tm->getI64();
        m_lexer->getNextToken();
        break;
    case X_U64:
        *ty = m_tm->getU64();
        m_lexer->getNextToken();
        break;
    case X_I128:
        *ty = m_tm->getI128();
        m_lexer->getNextToken();
        break;
    case X_U128:
        *ty = m_tm->getU128();
        m_lexer->getNextToken();
        break;
    case X_F32:
        *ty = m_tm->getF32();
        m_lexer->getNextToken();
        break;
    case X_F64:
        *ty = m_tm->getF64();
        m_lexer->getNextToken();
        break;
    case X_F80:
        *ty = m_tm->getF80();
        m_lexer->getNextToken();
        break;
    case X_F128:
        *ty = m_tm->getF128();
        m_lexer->getNextToken();
        break;
    case X_MC:
        tok = m_lexer->getNextToken();
        size = 0;
        if (!parseSize(tok, &size)) {
            error(tok, "MC type should specify a size");
            return false;
        }
        *ty = m_tm->getMCType(size);
        break;
    case X_STR:
        *ty = m_tm->getString();
        m_lexer->getNextToken();
        break;
    case X_VEC:
        {
            tok = m_lexer->getNextToken();
            if (tok != T_LESSTHAN) {
                error(tok, "miss '<' before size declaration");
                return false;
            }

            tok = m_lexer->getNextToken();
            if (tok != T_IMM) {
                error(tok, "size must be integer");
                return false;
            }

            size = atol(m_lexer->getCurrentTokenString());

            tok = m_lexer->getNextToken();
            if (tok != T_ASTERISK) {
                error(tok, "miss '*' in vector type declaration");
                return false;
            }

            tok = m_lexer->getNextToken();
            Type const* elem_type = NULL;
            if (!parseType(ctx, &elem_type) || elem_type == NULL) {
                error(tok, "illegal element type");
                return false;
            }
            if (!elem_type->is_scalar()) {
                error(tok, "element type must be scalar type");
                return false;
            }

            tok = m_lexer->getCurrentToken();
            if (tok != T_MORETHAN) {
                error(tok, "miss '<' before size declaration");
                return false;
            }
            m_lexer->getNextToken();
            *ty = m_tm->getVectorType(size, TY_dtype(elem_type));
            break;
        }
    case X_BOOL:
        *ty = m_tm->getBool();
        m_lexer->getNextToken();
        break;
    case X_VOID:
        *ty = m_tm->getVoid();
        m_lexer->getNextToken();
        break;
    default:;
    }
    return true;
}


bool IRParser::isType(X_CODE code) const
{
    switch (code) {
    case X_I8:
    case X_U8:
    case X_I16:
    case X_U16:
    case X_I32:
    case X_U32:
    case X_I64:
    case X_U64:
    case X_I128:
    case X_U128:
    case X_F32:
    case X_F64:
    case X_F80:
    case X_F128:
    case X_MC:
    case X_STR:
    case X_VEC:
    case X_VOID:
    case X_BOOL:
        return true;
    default:;
    }
    return false;
}


//<size>
bool IRParser::parseSize(TOKEN tok, UINT * size)
{
    if (tok != T_LESSTHAN) {
        error(tok, "miss '<' before size declaration");
        return false;
    }

    tok = m_lexer->getNextToken();
    if (tok != T_IMM) {
        error(tok, "size must be integer");
        return false;
    }

    *size = atol(m_lexer->getCurrentTokenString());

    tok = m_lexer->getNextToken();
    if (tok != T_MORETHAN) {
        error(tok, "miss '>' after size declaration");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


//List of type and property declaration.
bool IRParser::declareVarProperty(VAR * var, ParseCtx * ctx)
{
    if (m_lexer->getCurrentToken() != T_LPAREN) {
        error(m_lexer->getCurrentToken(),
            "miss '(' before var property declaration");
        return false;
    }
    TOKEN tok = m_lexer->getNextToken();
    for (;;) {
        switch (tok) {
        case T_RPAREN:
        case T_NUL:
        case T_END:
            break;
        case T_IDENTIFIER:
            switch (getCurrentXCode()) {
            case X_READONLY:
                VAR_is_readonly(var) = true;
                tok = m_lexer->getNextToken();
                break;
            case X_PRIVATE:
                VAR_is_private(var) = true;
                tok = m_lexer->getNextToken();
                break;
            case X_VOLATILE:
                VAR_is_volatile(var) = true;
                tok = m_lexer->getNextToken();
                break;
            case X_RESTRICT:
                VAR_is_restrict(var) = true;
                tok = m_lexer->getNextToken();
                break;
            case X_FUNC_DECL:
                VAR_is_func_decl(var) = true;
                tok = m_lexer->getNextToken();
                break;
            case X_FAKE:
                VAR_is_fake(var) = true;
                tok = m_lexer->getNextToken();
                break;
            case X_GLOBAL:
                VAR_is_global(var) = true;
                tok = m_lexer->getNextToken();
                break;
            case X_ARRAY:
                VAR_is_array(var) = true;
                tok = m_lexer->getNextToken();
                break;
            default:
                error(tok, "illegal to use %s in variable type declaration",
                    m_lexer->getCurrentTokenString());
            }
            break;
        default:
            error(tok, "illegal to use %s in variable type declaration",
                m_lexer->getCurrentTokenString());
            return false;
        }

        tok = m_lexer->getCurrentToken();
        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
        } else if (tok == T_RPAREN) {
            break;
        } else {
            error(tok, "miss ',' or ')'");
            break;
        }
    }

    if (tok != T_RPAREN) {
        error(tok, "type declaration miss ')'");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseUseProperty(PropertySet & cont, ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_USE);
    TOKEN tok = m_lexer->getNextToken();
    if (tok != T_LPAREN) {
        error(tok, "miss '(' after 'use'");
        return false;
    }
    tok = m_lexer->getNextToken();
    if (!parseExpList(ctx)) {
        return false;
    }
    if (m_lexer->getCurrentToken() != T_RPAREN) {
        error(tok, "miss ')'");
        return false;
    }
    m_lexer->getNextToken();
    cont.ir_use_list = ctx->returned_exp;
    ctx->returned_exp = NULL;
    return true;
}


//List of property declaration.
bool IRParser::parseProperty(PropertySet & cont, ParseCtx * ctx)
{
    if (m_lexer->getCurrentToken() != T_LPAREN) {
        error(m_lexer->getCurrentToken(),
            "miss '(' before property declaration");
        return false;
    }
    TOKEN tok = m_lexer->getNextToken();
    for (;;) {
        switch (tok) {
        case T_RPAREN:
        case T_NUL:
        case T_END:
            break;
        case T_IDENTIFIER:
            switch (getCurrentXCode()) {
            case X_READONLY:
                cont.readonly = true;
                tok = m_lexer->getNextToken();
				break;
            case X_RMW:
                cont.read_modify_write = true;
                tok = m_lexer->getNextToken();
                break;
            case X_THROW:
                cont.throw_exception = true;
                tok = m_lexer->getNextToken();
                break;
            case X_SIDEEFFECT:
                cont.sideeffect = true;
                tok = m_lexer->getNextToken();
                break;
            case X_NOMOVE:
                cont.nomove = true;
                tok = m_lexer->getNextToken();
                break;
            case X_ATOM:
                cont.atomic = true;
                tok = m_lexer->getNextToken();
                break;
            case X_TERMINATE:
                cont.terminate = true;
                tok = m_lexer->getNextToken();
                break;
			case X_USE:
                ASSERT0(ctx);
				if (ctx->ircode != IR_CALL && ctx->ircode != IR_ICALL) {
					error(tok, "%s does have USE property", IRTNAME(ctx->ircode));
					return false;
				}
				if (!parseUseProperty(cont, ctx)) {
					return false;
				}
				break;
            default:
                error(tok, "illegal to use %s in variable type declaration",
                    m_lexer->getCurrentTokenString());
            }
            break;
        default:
            error(tok, "illegal to use %s in variable type declaration",
                m_lexer->getCurrentTokenString());
            return false;
        }

        tok = m_lexer->getCurrentToken();
        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
        } else if (tok == T_RPAREN) {
            break;
        } else {
            error(tok, "miss ',' or ')'");
            break;
        }
    }

    if (tok != T_RPAREN) {
        error(tok, "type declaration miss ')'");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}




bool IRParser::declareVar(ParseCtx * ctx, VAR ** var)
{
    if (getCurrentXCode() != X_VAR) {
        error(m_lexer->getCurrentToken(), "miss 'var'");
        return false;
    }
    TOKEN tok = m_lexer->getNextToken();
    if (tok != T_IDENTIFIER) {
        error(tok, "miss identifier name");
        return false;
    }
    SYM const* sym = m_rumgr->addToSymbolTab(m_lexer->getCurrentTokenString());

    //Type
    tok = m_lexer->getNextToken();
    Type const* ty = NULL;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            return false;
        }
    }
    if (ty == NULL) {
        ty = m_tm->getVoid();
    }

    ASSERT0(ctx->current_region);
    VAR * v = m_rumgr->getVarMgr()->registerVar(
        sym, ty, 1,
        ctx->current_region->is_program() ? VAR_GLOBAL : VAR_LOCAL);
    ctx->current_region->addToVarTab(v);
	*var = v;

    //Property
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!declareVarProperty(v, ctx)) {
            return false;
        }
    }

    return true;
}


//Dump token string.
//Return true if no error find.
bool IRParser::parse()
{
    START_TIMER(t, "IR Parser");
    ASSERT0(m_lexer);
    TOKEN tok = m_lexer->getNextToken(); //Get first token.
    for (;; tok = m_lexer->getNextToken()) {
        switch (tok) {
        case T_END: return true;
        case T_NUL: return false;
        case T_IDENTIFIER:
            {
                X_CODE code = getCurrentXCode();
                switch (code) {
                case X_REGION:
                    declareRegion(NULL);
                    break;
                default:
                    error(tok, "miss region declaration at top level");
                }
            }
            break;
        default:
            error(tok, "miss region declaration at top level");
        }

        if (isTooManyError()) {
            return false;
        }
    }
    END_TIMER(t, "IR Parser");
    return getErrorMsgList().get_elem_count() == 0;
}
//END IRParser

} //namespace xoc