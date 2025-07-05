/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com
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
#ifndef _IR_PARSER_H_
#define _IR_PARSER_H_

namespace xoc {

#define MAX_PRNO 0x7fffFFFF

class Lexer;
class IRParser;
class Region;
class RegionMgr;

class ParseErrorMsg {
public:
    TOKEN error_token;
    xcom::StrBuf * error_msg;
public:
    ParseErrorMsg(UINT msglen) { error_msg = new xcom::StrBuf(msglen); }
    ~ParseErrorMsg() { delete error_msg; }
};

#define PARSECTX_returned_imm_intval(p) ((p)->s1.u1.returned_imm_intval)
#define PARSECTX_returned_imm_fpval(p) ((p)->s1.u1.returned_imm_fpval)
#define PARSECTX_returned_imm_ty(p) ((p)->s1.returned_imm_ty)
class ParseCtx {
    typedef xcom::TMap<Sym const*, LabelInfo*> Sym2Lab;
    typedef xcom::TMapIter<Sym const*, LabelInfo*> Sym2LabIter;
    typedef xcom::TMap<Sym const*, PRNO> Sym2Prno;
    typedef xcom::TMapIter<Sym const*, PRNO> Sym2PrnoIter;
protected:
    COPY_CONSTRUCTOR(ParseCtx);
    UINT id; //the unique id for each ctx.
    //The following fields are unique to each region.
    //Clean them before parsing of new region body.
    Sym2Lab * m_sym2label;
    IR2Lab * m_ir2label;
    Sym2Prno * m_iden2prno;
public:
    //Top down propagate information.
    //Record the previous declared ParseCtx.
    //User can retrive the related context during parsing.
    ParseCtx * previous_ctx;

    //Top down propagate information.
    //Record the generated region.
    Region * current_region;

    //Top down propagate information.
    IRParser * parser;

    //Bottom up propagate information.
    //Record the generated IR expression.
    IR * returned_exp;

    //Bottom up propagate information.
    //Record the generated IR stmt list.
    IR * stmt_list;

    //Bottom up propagate information.
    //Record the last IR at generated IR stmt list.
    IR * stmt_list_last;

    //Bottom up propagate information.
    //Record whether the current region contained IR_PHI.
    bool has_phi;

    //Bottom up propagate information.
    //Record whether the current region contained Structure Control Flow
    //IR stmt.
    bool has_scf;

    //Bottom up propagate information.
    //Record whether if error occurred.
    bool has_error;

    //Top down propagate information.
    //Record the IR code that has parsed.
    IR_CODE ircode;

    //Bottom up propagate information.
    //Record the generated immediate.
    struct {
        union {
            HOST_INT returned_imm_intval;
            HOST_FP returned_imm_fpval;
        } u1;
        Type const* returned_imm_ty;
    } s1;
public:
    ParseCtx(IRParser * p);
    ParseCtx(ParseCtx * ctx);
    ~ParseCtx();

    void addIR(IR * stmt);

    void clean();
    void copyTopDownInfo(ParseCtx const& ctx);

    //The function reset region local info before parsing the region body.
    void cleanRegionUniqueInfo();

    void dumpWithPrevCtx() const;
    void dump() const;

    IR2Lab & getIR2Label() { return *m_ir2label; }

    LabelInfo * mapSym2Label(Sym const* sym) const
    { return m_sym2label->get(sym); }
    LabelInfo const* mapIR2Label(IR const* ir) const
    { return m_ir2label->get(ir); }
    PRNO mapSym2Prno(Sym const* sym) const { return m_iden2prno->get(sym); }

    void setMapSym2Label(Sym const* sym, LabelInfo * label)
    { m_sym2label->set(sym, label); }
    void setMapIR2Label(IR const* ir, LabelInfo const* label)
    { m_ir2label->set(ir, label); }
    void setMapSym2Prno(Sym const* sym, PRNO prno)
    { m_iden2prno->set(sym, prno); }

    void unionBottomUpInfo(ParseCtx const& ctx);
};


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
public:
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
        ir_use_list = nullptr;
        ir_def_list = nullptr;
        elemtype = nullptr;
        dim_list = nullptr;
    }

    List<LabelInfo*> & getLabelList() { return m_labellist; }
};


typedef xcom::TMap<CHAR const*, X_CODE, CompareStringFunc> String2XCode;
typedef xcom::TMapIter<CHAR const*, X_CODE> String2XCodeIter;

class IRParser {
    COPY_CONSTRUCTOR(IRParser);
protected:
    UINT m_ctx_id; //used to count the number of ParseCtx occurred.
    TypeMgr * m_tm;
    Lexer * m_lexer;
    RegionMgr * m_rm;
    List<ParseErrorMsg*> m_err_list;
    String2XCode m_str2xcode;
    String2XCode m_prop2xcode;
    String2XCode m_stmt2xcode;
    String2XCode m_exp2xcode;
    String2XCode m_type2xcode;
protected:
    //Return true if GRReader allows user defined dedicated PRNO in GR file.
    //e.g: stpr $200 = 0;
    //     GRReader will directly assign Prno 200 to the stmt as a result.
    //TODO: GRReader has to use string and immediate mapping to support the
    //assignment to avoid both string-literal Prno and customized Prno in use.
    //e.g: stpr $200 = 0;
    //          ...  = $xyz;
    bool allowCustomizePrno() { return false; }

    bool checkKeyWordMap();
    bool checkPhiOpndLabel(IR const* ir,
        xcom::TMap<LabelInfo const*, IR const*> const& labtab,
        ParseCtx const& ctx);
    //The function checks label for GR syntax legality.
    bool checkLabel(IR const* irlist, ParseCtx const& ctx);
    //Return false if there is error occur.
    bool constructSSAIfNeed(ParseCtx * ctx);
    void copyProp(IR * ir, PropertySet & ps, ParseCtx * ctx);

    bool declareType(ParseCtx * ctx);
    bool declareVarProperty(Var * var, ParseCtx * ctx);
    bool declareVar(ParseCtx * ctx, Var ** var);
    bool declareRegion(ParseCtx * ctx);

    void error(UINT lineno, CHAR const* format, ...);
    void error(TOKEN tok, CHAR const* format, ...);
    void error(X_CODE xcode, CHAR const* format, ...);
    void error(CHAR const* format, ...);

    Var * findVar(ParseCtx * ctx, Sym const* name);

    X_CODE getCurrentPropertyCode();
    X_CODE getCurrentStmtCode();
    X_CODE getCurrentExpCode();
    X_CODE getCurrentTypeCode();
    X_CODE getXCode(TOKEN tok, CHAR const* tok_string);
    X_CODE getCurrentXCode();

    virtual void initStmtKeyWord();
    virtual void initExpKeyWord();
    virtual void initTypeKeyWord();
    virtual void initPropertyKeyWord();
    void initStr2KeyWord();
    bool isTooManyError() const { return m_err_list.get_elem_count() > 10; }
    bool isType(X_CODE code) const;
    bool isEndOfScope() const
    { return m_lexer->getCurrentToken() == T_RLPAREN; }
    bool isEndOfAll() const
    { return m_lexer->getCurrentToken() == T_END; }
    bool isEndOfAll(TOKEN tok) const { return tok == T_END; }
    bool isLabelDeclaration() const;
    bool isExp(X_CODE code);
    bool isExp();
    bool isTerminator(TOKEN tok);

    PRNO mapIden2Prno(CHAR const* prid, ParseCtx * ctx);

    bool parseTypeWrap(ParseCtx * ctx, OUT Type const*& ty);
    bool parsePropertyWrap(IR_CODE code, ParseCtx * ctx, OUT PropertySet & ps);
    bool parseCommaWrap();
    bool parseExpWrap(ParseCtx * ctx);
    bool parseCustomizedPrno(PRNO * prno, ParseCtx * ctx);
    bool parseStringLiteralPrno(PRNO * prno, CHAR const* str, ParseCtx * ctx);
    bool parseRegionName(Region * region, UFlag & flag, ParseCtx * ctx);
    bool parseRegionProp(OUT PropertySet & ps, ParseCtx * ctx);
    bool parseRegionType(Region ** region, UFlag & flag, ParseCtx * ctx);
    bool parseDimProperty(PropertySet & ps, ParseCtx * ctx);
    bool parseElemTypeProperty(PropertySet & ps, ParseCtx * ctx);
    bool parseAlign(Var * var, ParseCtx * ctx);
    bool parseByteValue(Var * var, ParseCtx * ctx);
    bool parseStringValue(Var * var, ParseCtx * ctx);
    bool parseThrowTarget(PropertySet & ps, ParseCtx * ctx);
    bool parseDefProperty(PropertySet & ps, ParseCtx * ctx);
    bool parseUseProperty(PropertySet & ps, ParseCtx * ctx);
    bool parseProperty(PropertySet & ps, ParseCtx * ctx);
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
    bool parseBinaryOp(IR_CODE code, ParseCtx * ctx);
    bool parseUnaryOp(IR_CODE code, ParseCtx * ctx);
    bool parseLd(ParseCtx * ctx);
    bool parseSignImm(TOKEN tok, ParseCtx * ctx);
    bool parseImmIR(ParseCtx * ctx);
    bool parseImmVal(ParseCtx * ctx);
    bool parseFP(ParseCtx * ctx);
    bool parseString(ParseCtx * ctx);
    bool parseBool(ParseCtx * ctx);
    bool parsePrno(PRNO * prno, ParseCtx * ctx);
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
    virtual bool parseExtXCode(ParseCtx * ctx);

    Type const* reviseTypeByCode(IR_CODE code, Type const* ty);
public:
    IRParser(RegionMgr * rm);
    virtual ~IRParser();

    void dumpMiscString2XCode() const;
    void dumpXCodeInfo() const;
    void dump() const;

    CHAR const* getPassName() const { return "IRParser"; }
    RegionMgr * getRegionMgr() const { return m_rm; }
    List<ParseErrorMsg*> & getErrorMsgList() { return m_err_list; }
    CHAR const* getKeyWordName(X_CODE code) const;
    Lexer * getLexer() const { return m_lexer; }
    UINT genParseCtxId() { return ++m_ctx_id; }

    void initMap();

    void setLexer(Lexer * l) { m_lexer = l; }

    bool parse();
};

} //namespace xoc
#endif
