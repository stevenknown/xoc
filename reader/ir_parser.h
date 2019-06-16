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
#ifndef _IR_PARSER_
#define _IR_PARSER_

namespace xoc {

class Lexer;
class Region;
class RegionMgr;

class ParseErrorMsg {
public:
    TOKEN error_token;
    xcom::StrBuf * error_msg;
public:
    ParseErrorMsg(UINT msglen) { error_msg = new StrBuf(msglen); }
    ~ParseErrorMsg() { delete error_msg; }
};


class ParseCtx {
    xcom::TMap<SYM const*, LabelInfo*> m_sym2label;
    xcom::TMap<IR*, LabelInfo*> m_ir2label;
public:
    Region * current_region;
    IR * returned_exp;
    IR * stmt_list;
    IR * last;
    bool has_phi;
    bool has_high_level_ir;
    bool has_error;
    IR_TYPE ircode; //for temporary used

public:
    ParseCtx()
    {
        current_region = NULL;
        returned_exp = NULL;
        stmt_list = NULL;
        last = NULL;
        has_phi = false;
        has_high_level_ir = false;
        has_error = false;
        ircode = IR_UNDEF;
    }
    ~ParseCtx() {}

    void addIR(IR * stmt) { xcom::add_next(&stmt_list, &last, stmt); }

    xcom::TMap<IR*, LabelInfo*> & getIR2Label() { return m_ir2label; }

    LabelInfo * mapSym2Label(SYM const* sym) const
    { return m_sym2label.get(sym); }
    LabelInfo * mapIR2Label(IR * ir) const
    { return m_ir2label.get(ir); }

    void setMapSym2Label(SYM const* sym, LabelInfo * label)
    { m_sym2label.set(sym, label); }
    void setMapIR2Label(IR * ir, LabelInfo * label)
    { m_ir2label.set(ir, label); }
    void storeValue(IR ** oldvalue1, IR ** oldvalue2)
    {
        *oldvalue1 = stmt_list;
        *oldvalue2 = last;
        stmt_list = NULL;
        last = NULL;
    }

    void reloadValue(IR * oldvalue1, IR * oldvalue2)
    {
        stmt_list = oldvalue1;
        last = oldvalue2;
    }
};


typedef enum {
    X_UNDEF,
    X_ID,
    X_LD,
    X_ILD,
    X_ARRAY,
    X_ST,
    X_STRP,
    X_STARRAY,
    X_SETELEM,
    X_GETELEM,
    X_IST,
    X_CALL,
    X_ICALL,
    X_LDA,
    X_ADD,
    X_SUB,
    X_MUL,
    X_DIV,
    X_REM,
    X_MOD,
    X_LAND,
    X_LOR,
    X_BAND,
    X_BOR,
    X_XOR,
    X_ASR,
    X_LSR,
    X_LSL,
    X_LT,
    X_LE,
    X_GT,
    X_GE,
    X_EQ,
    X_NE,
    X_BNOT,
    X_LNOT,
    X_NEG,
    X_CVT,
    X_GOTO,
    X_IGOTO,
    X_DO,
    X_WHILE,
    X_DO_LOOP,
    X_LABEL,
    X_TRUEBR,
    X_FALSEBR,
    X_SELECT,
    X_PHI,
    X_REGION,
    X_IF,
    X_ELSE,
    X_BREAK,
    X_RETURN,
    X_CONTINUE,
    X_SWITCH,
    X_CASE,
    X_DEFAULT,
    X_VAR,
    X_FUNC,
    X_PROGRAM,
    X_I8,
    X_U8,
    X_I16,
    X_U16,
    X_I32,
    X_U32,
    X_I64,
    X_U64,
    X_I128,
    X_U128,
    X_F32,
    X_F64,
    X_F80,
    X_F128,
    X_MC,
    X_STR,
    X_VEC,
    X_BOOL,
    X_VOID,
    X_READONLY,
    X_TRY_START,
    X_TRY_END,
    X_TERMINATE,
    X_CATCH_START,
    X_ATOM,
    X_RMW, //ReadModifyWrite
    X_THROW,
    X_SIDEEFFECT, //SideEffect
    X_NOMOVE,
    X_USE,
    X_DEF,
    X_PRIVATE,
    X_RESTRICT,
    X_VOLATILE,
    X_FUNC_DECL,
    X_FAKE,
    X_GLOBAL,
    X_UNDEFINED,
    X_STRING,
    X_BYTE,
    X_ELEMTYPE,
    X_DIM,
    X_UNALLOCABLE,
    X_ALIGN,
    X_LAST,
} X_CODE;


class PropertySet {
    List<LabelInfo*> m_labellist;
public:
    bool readonly;
    bool read_modify_write;
    bool throw_exception;
    bool sideeffect;
    bool nomove;
    bool atomic;
    bool terminate;
    bool allocable;
    IR * ir_use_list;
    IR * ir_def_list;
    Type const* elemtype;
    List<TMWORD> * dim_list;

    PropertySet()
    {
        readonly = false;
        read_modify_write = false;
        throw_exception = false;
        sideeffect = false;
        nomove = false;
        atomic = false;
        terminate = false;
        allocable = false;
        ir_use_list = NULL;
        ir_def_list = NULL;
        elemtype = NULL;
        dim_list = NULL;
    }

    List<LabelInfo*> & getLabelList() { return m_labellist; }
};


class IRParser {
protected:
    TMap<CHAR const*, X_CODE, CompareStringFunc> m_str2xcode;
    TMap<CHAR const*, X_CODE, CompareStringFunc> m_prop2xcode;
    TMap<CHAR const*, X_CODE, CompareStringFunc> m_stmt2xcode;
    TMap<CHAR const*, X_CODE, CompareStringFunc> m_exp2xcode;
    TMap<CHAR const*, X_CODE, CompareStringFunc> m_type2xcode;
    TypeMgr * m_tm;
    Lexer * m_lexer;
    RegionMgr * m_rumgr;
    List<ParseErrorMsg*> m_err_list;
protected:
    bool declareType(ParseCtx * ctx);
    bool declareVarProperty(VAR * var, ParseCtx * ctx);
    bool declareVar(ParseCtx * ctx, VAR ** var);
    bool declareRegion(ParseCtx * ctx);

    void enterRegion(ParseCtx *) {}
    void exitRegion(ParseCtx *) {}
    void error(TOKEN tok, CHAR const* format, ...);
    void error(X_CODE xcode, CHAR const* format, ...);
    void error(CHAR const* format, ...);

    VAR * findVar(ParseCtx * ctx, SYM const* name);

    X_CODE getCurrentPropertyCode();
    X_CODE getCurrentStmtCode();
    X_CODE getCurrentExpCode();
    X_CODE getCurrentTypeCode();
    X_CODE getXCode(TOKEN tok, CHAR const* tok_string);
    X_CODE getCurrentXCode();

    void initKeyWordMap();
    bool isTooManyError() const { return m_err_list.get_elem_count() > 10; }
    bool isType(X_CODE code) const;
    bool isEndOfScope() const;
    bool isLabelDeclaration() const;
    bool isExp(X_CODE code);
    bool isExp();
    bool isTerminator(TOKEN tok);

    bool parseDimProperty(PropertySet & cont, ParseCtx * ctx);
    bool parseElemTypeProperty(PropertySet & cont, ParseCtx * ctx);
    bool parseAlign(VAR * var, ParseCtx * ctx);
	bool parseByteValue(VAR * var, ParseCtx * ctx);
    bool parseStringValue(VAR * var, ParseCtx * ctx);
    bool parseThrowTarget(PropertySet & cont, ParseCtx * ctx);
    bool parseDefProperty(PropertySet & cont, ParseCtx * ctx);
    bool parseUseProperty(PropertySet & cont, ParseCtx * ctx);
    bool parseProperty(PropertySet & cont, ParseCtx * ctx);
    bool parseCvt(ParseCtx * ctx);
    bool parseCase(ParseCtx * ctx);
    bool parseId(ParseCtx * ctx);
    bool parseLda(ParseCtx * ctx);
    bool parseArrayDimension(List<TMWORD> & elem_dim);
    bool parseArray(ParseCtx * ctx);
    bool parseILd(ParseCtx * ctx);
    bool parseSelect(ParseCtx * ctx);
    bool parseIStore(ParseCtx * ctx);
    bool parseCallAndICall(bool is_call, ParseCtx * ctx);
    bool parseGoto(ParseCtx * ctx);
    bool parseIGoto(ParseCtx * ctx);
    bool parseDoWhile(ParseCtx * ctx);
    bool parseWhileDo(ParseCtx * ctx);
    bool parseDoLoop(ParseCtx * ctx);
    bool parseLabelProperty(LabelInfo * label);
    bool parseLabel(ParseCtx * ctx);
    bool parseBranch(bool is_truebr, ParseCtx * ctx);
    bool parsePhi(ParseCtx * ctx);
    bool parseBreak(ParseCtx * ctx);
    bool parseContinue(ParseCtx * ctx);
    bool parseSwitch(ParseCtx * ctx);
    bool parseStorePR(ParseCtx * ctx);
    bool parseStoreArray(ParseCtx * ctx);
    bool parseReturn(ParseCtx * ctx);
    bool parseRegionBody(ParseCtx * ctx);
    bool parseModifyPR(X_CODE code, ParseCtx * ctx);
    bool parseBinaryOp(IR_TYPE code, ParseCtx * ctx);
    bool parseUnaryOp(IR_TYPE code, ParseCtx * ctx);
    bool parseLd(ParseCtx * ctx);
    bool parseSignImm(TOKEN tok, ParseCtx * ctx);
    bool parseImm(ParseCtx * ctx);
    bool parseFp(ParseCtx * ctx);
    bool parsePR(ParseCtx * ctx);
    bool parseIf(ParseCtx * ctx);
    bool parseParameterList(ParseCtx * ctx);
    bool parseStmtList(ParseCtx * ctx);
    bool parseStore(ParseCtx * ctx);
    bool parseXOperator(ParseCtx * ctx);
    bool parseOperator(ParseCtx * ctx);
    bool parseSize(TOKEN tok, UINT * size);
    bool parseType(ParseCtx * ctx, Type const** ty);
    bool parseExp(ParseCtx * ctx);
    bool parseExpList(ParseCtx * ctx);
public:
    IRParser(RegionMgr * rumgr) : m_lexer(NULL), m_rumgr(rumgr)
    {
        m_tm = rumgr->getTypeMgr();
        initKeyWordMap();
    }
    COPY_CONSTRUCTOR(IRParser);
    ~IRParser();

    void dump();

    RegionMgr * getRegionMgr() { return m_rumgr; }
    List<ParseErrorMsg*> & getErrorMsgList() { return m_err_list; }
    CHAR const* getKeywordName(X_CODE code);

    void setLexer(Lexer * l) { m_lexer = l; }

    bool parse();
};

} //namespace xoc
#endif
