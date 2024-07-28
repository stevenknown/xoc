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
#include "../com/xcominc.h"
#include "../opt/cominc.h"
#include "../opt/comopt.h"
#include "ir_lex.h"
#include "xcode.h"
#include "ir_parser.h"

namespace xoc {

#define XCODEINFO_code(i) ((i).code)
#define XCODEINFO_name(i) ((i).name)
class XCodeInfo {
public:
    X_CODE code;
    CHAR const* name;
    IR_CODE ircode;
};


//Define keywords of XOC IR.
static XCodeInfo g_keyword_info[] = {
    { X_CODE_UNDEF, "", },
    { X_ID, "id", },
    { X_LD, "ld", },
    { X_ILD, "ild", },
    { X_ARRAY, "array", },
    { X_ST, "st", },
    { X_STRP, "stpr", },
    { X_STARRAY, "starray", },
    { X_SETELEM, "setelem", },
    { X_GETELEM, "getelem", },
    { X_IST, "ist", },
    { X_CALL, "call", },
    { X_ICALL, "icall", },
    { X_LDA, "lda", },
    { X_ADD, "add", },
    { X_SUB, "sub", },
    { X_MUL, "mul", },
    { X_DIV, "div", },
    { X_REM, "rem", },
    { X_MOD, "mod", },
    { X_LAND, "land", },
    { X_LOR, "lor", },
    { X_BAND, "band", },
    { X_BOR, "bor", },
    { X_XOR, "xor", },
    { X_ASR, "asr", },
    { X_LSR, "lsr", },
    { X_LSL, "lsl", },
    { X_LT, "lt", },
    { X_LE, "le", },
    { X_GT, "gt", },
    { X_GE, "ge", },
    { X_EQ, "eq", },
    { X_NE, "ne", },
    { X_BNOT, "bnot", },
    { X_LNOT, "lnot", },
    { X_NEG, "neg", },
    { X_ALLOCA, "alloca", },
    { X_CVT, "cvt", },
    { X_GOTO, "goto", },
    { X_IGOTO, "igoto", },
    { X_DO, "do", },
    { X_WHILE, "while", },
    { X_DO_LOOP, "doloop", },
    { X_LABEL, "label", },
    { X_TRUEBR, "truebr", },
    { X_FALSEBR, "falsebr", },
    { X_SELECT, "select", },
    { X_PHI, "phi", },
    { X_REGION, "region", },
    { X_IF, "if", },
    { X_ELSE, "else", },
    { X_BREAK, "break", },
    { X_RETURN, "return", },
    { X_CONTINUE, "continue", },
    { X_SWITCH, "switch", },
    { X_CASE, "case", },
    { X_DEFAULT, "default", },
    { X_VAR, "var", },
    { X_FUNC, "func", },
    { X_PROGRAM, "program", },
    { X_INNER, "inner", },
    { X_BLACKBOX, "blackbox", },
    { X_I8, "i8", },
    { X_U8, "u8", },
    { X_I16, "i16", },
    { X_U16, "u16", },
    { X_I32, "i32", },
    { X_U32, "u32", },
    { X_I64, "i64", },
    { X_U64, "u64", },
    { X_I128, "i128", },
    { X_U128, "u128", },
    { X_F32, "f32", },
    { X_F64, "f64", },
    { X_F80, "f80", },
    { X_F128, "f128", },
    { X_MC, "mc", },
    { X_STR, "str", },
    { X_VEC, "vec", },
    { X_BOOL, "bool", },
    { X_ANY, "any", },
    { X_READONLY, "readonly", },
    { X_TRY_START, "try_start", },
    { X_TRY_END, "try_end", },
    { X_TERMINATE, "terminate", },
    { X_CATCH_START, "catch_start", },
    { X_ATOM, "atom", },
    { X_RMW, "rmw", },
    { X_THROW, "throw", },
    { X_SIDEEFFECT, "sideeffect", },
    { X_NOMOVE, "nomove", },
    { X_USE, "use", },
    { X_DEF, "def", },
    { X_PRIVATE, "private", },
    { X_RESTRICT, "restrict", },
    { X_VOLATILE, "volatile", },
    { X_FAKE, "fake", },
    { X_GLOBAL, "global", },
    { X_UNDEFINED, "undefined", },
    { X_STRING, "string", },
    { X_BYTE, "byte", },
    { X_ELEMTYPE, "elemtype", },
    { X_DIM, "dim", },
    { X_UNALLOCABLE, "unallocable", },
    { X_ALIGN, "align", },
    { X_DECL, "decl", },
    { X_CODE_LAST, "", },
};


static X_CODE g_property_code [] = {
    X_READONLY,
    X_RMW,
    X_THROW,
    X_SIDEEFFECT,
    X_NOMOVE,
    X_ATOM,
    X_TERMINATE,
    X_USE,
    X_DEF,
    X_ELEMTYPE,
    X_DIM,
    X_UNALLOCABLE,
};


static X_CODE g_stmt_code [] = {
    X_REGION,
    X_ST,
    X_STRP,
    X_STARRAY,
    X_SETELEM,
    X_GETELEM,
    X_IST,
    X_CALL,
    X_ICALL,
    X_GOTO,
    X_IGOTO,
    X_DO,
    X_WHILE,
    X_DO_LOOP,
    X_LABEL,
    X_TRUEBR,
    X_FALSEBR,
    X_PHI,
    X_IF,
    X_BREAK,
    X_RETURN,
    X_CONTINUE,
    X_SWITCH
};


static X_CODE g_exp_code [] = {
    X_ID,
    X_LD,
    X_ILD,
    X_ARRAY,
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
    X_ALLOCA,
    X_CVT,
    X_SELECT,
    X_CASE
};


static X_CODE g_type_code [] = {
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
    X_ANY
};

//
//START ParseCtx
//
ParseCtx::ParseCtx(IRParser * p)
{
    ASSERT0(p);
    clean();
    parser = p;
    m_iden2prno = new Sym2Prno();
    m_ir2label = new IR2Lab();
    m_sym2label = new Sym2Lab();
    id = parser->genParseCtxId();
}


ParseCtx::ParseCtx(ParseCtx * ctx)
{
    ASSERT0(this != ctx);
    clean();
    previous_ctx = ctx;
    copyTopDownInfo(*ctx);
    id = parser->genParseCtxId();
}


ParseCtx::~ParseCtx()
{
    if (previous_ctx != nullptr) {
        previous_ctx->unionBottomUpInfo(*this);
        return;
    }
    ASSERT0(m_iden2prno);
    delete m_iden2prno;
    ASSERT0(m_ir2label);
    delete m_ir2label;
    ASSERT0(m_sym2label);
    delete m_sym2label;
}


void ParseCtx::addIR(IR * stmt)
{
    ASSERT0(stmt->is_stmt());
    xcom::add_next(&stmt_list, stmt);

    //Set lineno for debug info.
    ASSERT0(current_region);
    xoc::setLineNum(stmt, parser->getLexer()->getCurrentLineNum(),
                    current_region, LangInfo::LANG_CPP);
}


void ParseCtx::copyTopDownInfo(ParseCtx const& ctx)
{
    parser = ctx.parser;
    current_region = ctx.current_region;
    m_iden2prno = ctx.m_iden2prno;
    m_ir2label = ctx.m_ir2label;
    m_sym2label = ctx.m_sym2label;
}


void ParseCtx::cleanRegionUniqueInfo()
{
    m_iden2prno->clean();
    m_ir2label->clean();
    m_sym2label->clean();
}


void ParseCtx::unionBottomUpInfo(ParseCtx const& ctx)
{
    ASSERT0(returned_exp == nullptr);
    returned_exp = ctx.returned_exp;
    xcom::add_next(&stmt_list, ctx.stmt_list);
    has_phi |= ctx.has_phi;
    has_scf |= ctx.has_scf;
    has_error |= ctx.has_error;
}


void ParseCtx::clean()
{
    parser = nullptr;
    current_region = nullptr;
    returned_exp = nullptr;
    stmt_list = nullptr;
    previous_ctx = nullptr;
    has_phi = false;
    has_scf = false;
    has_error = false;
    ircode = IR_UNDEF;
    m_iden2prno = nullptr;
    m_ir2label = nullptr;
    m_sym2label = nullptr;
    ::memset((void*)&s1, 0, sizeof(s1));
}


void ParseCtx::dumpWithPrevCtx() const
{
    dump();
    if (previous_ctx != nullptr) {
        ASSERT0(current_region);
        note(current_region, "\n-- PREVIOUS_CTX --");
        previous_ctx->dumpWithPrevCtx();
    }
}


void ParseCtx::dump() const
{
    ASSERT0(current_region);
    Region * rg = current_region;
    LogMgr * lm = rg->getLogMgr();
    if (lm == nullptr || !lm->is_init()) { return; }
    note(rg, "\n==-- DUMP ParseCtx:%u --==", id);
    note(rg, "\ncurrent_region:%s", current_region->getRegionName());
    note(rg, "\nreturned_exp:");
    if (returned_exp != nullptr) {
        lm->incIndent(2);
        xoc::dumpIR(returned_exp, current_region);
        lm->decIndent(2);
    }
    note(rg, "\nstmt_list:");
    if (stmt_list != nullptr) {
        lm->incIndent(2);
        xoc::dumpIRList(stmt_list, current_region);
        lm->decIndent(2);
    }
    note(rg, "\nhas_phi:%s", has_phi ? "true" : "false");
    note(rg, "\nhas_scf:%s", has_scf ? "true" : "false");
    note(rg, "\nhas_error:%s", has_error ? "true" : "false");
    note(rg, "\nir_code:%s", IRCNAME(ircode));
    note(rg, "\nreturned_imm_ty:");
    if (PARSECTX_returned_imm_ty(this) != nullptr) {
        PARSECTX_returned_imm_ty(this)->dump(rg->getTypeMgr());
    }
    xcom::StrBuf outbuf(16);
    Type const* intty = rg->getTypeMgr()->getIntType(
        sizeof(HOST_INT) * HOST_BIT_PER_BYTE, true);
    ASSERT0(intty);
    xoc::dumpHostInt(PARSECTX_returned_imm_intval(this), intty, rg, outbuf);
    note(rg, "\nreturned_imm_intval:%s", outbuf.buf);

    Type const* fpty = rg->getTypeMgr()->getFPType(
        sizeof(HOST_FP) * HOST_BIT_PER_BYTE, false);
    ASSERT0(intty);
    xoc::dumpHostFP(PARSECTX_returned_imm_fpval(this), fpty,
                    DEFAULT_MANTISSA_NUM, rg, outbuf);
    note(rg, "\nreturned_imm_fpval:%s", outbuf.buf);

    note(rg, "\nsym2label:");
    if (m_sym2label != nullptr) {
        lm->incIndent(2);
        Sym2LabIter it;
        LabelInfo * mapped = nullptr;
        for (Sym const* sym = m_sym2label->get_first(it, &mapped);
             sym != nullptr; sym = m_sym2label->get_next(it, &mapped)) {
            ASSERT0(mapped);
            outbuf.clean();
            note(rg, "\n'%s'->%s", sym->getStr(), mapped->getName(outbuf));
        }
        lm->decIndent(2);
    }

    note(rg, "\nir2label:");
    if (m_ir2label != nullptr) {
        lm->incIndent(2);
        IR2LabIter it;
        LabelInfo const* mapped = nullptr;
        for (IR const* ir = m_ir2label->get_first(it, &mapped);
             ir != nullptr; ir = m_ir2label->get_next(it, &mapped)) {
            ASSERT0(mapped);
            outbuf.clean();
            note(rg, "\n%s:%d->%s", IRNAME(ir), ir->id(),
                 mapped->getName(outbuf));
        }
        lm->decIndent(2);
    }

    note(rg, "\niden2prno:");
    if (m_iden2prno != nullptr) {
        lm->incIndent(2);
        Sym2PrnoIter it;
        PRNO mapped = PRNO_UNDEF;
        for (Sym const* iden = m_iden2prno->get_first(it, &mapped);
             iden != nullptr; iden = m_iden2prno->get_next(it, &mapped)) {
            ASSERT0(mapped);
            note(rg, "\n'%s%s'->%s%u", PR_TYPE_CHAR, iden->getStr(),
                 PR_TYPE_CHAR, mapped);
        }
        lm->decIndent(2);
    }
}
//END ParseCtx


//
//START IRParser
//
static void copyProp(IR * ir, PropertySet & ps, ParseCtx * ctx)
{
    IR_is_atomic(ir) = ps.atomic;
    IR_may_throw(ir) = ps.throw_exception;
    IR_has_sideeffect(ir) = ps.sideeffect;
    IR_no_move(ir) = ps.nomove;
    IR_is_read_mod_write(ir) = ps.read_modify_write;
    IR_is_terminate(ir) = ps.terminate;
    if (ir->is_icall()) {
        ICALL_is_readonly(ir) = ps.readonly;
    }
    if (ir->is_region()) {
        REGION_is_readonly(REGION_ru(ir)) = ps.readonly;
    }
    if (!ir->isMayThrow(false)) { return; }

    for (LabelInfo * l = ps.getLabelList().get_tail();
         l != nullptr; l = ps.getLabelList().get_prev()) {
        AIContainer * ai = IR_ai(ir);
        if (ai == nullptr) {
            ai = ctx->current_region->allocAIContainer();
            IR_ai(ir) = ai;
        }

        EHLabelAttachInfo * ehai = (EHLabelAttachInfo*)ai->get(AI_EH_LABEL);
        if (ehai == nullptr) {
            ehai = (EHLabelAttachInfo*)ctx->current_region->
                       xmalloc(sizeof(EHLabelAttachInfo));
            ehai->init(ctx->current_region->getSCPool());
            ai->set(ehai, ctx->current_region);
        }
        ehai->get_labels().append_head(l);
    }
}


IRParser::~IRParser()
{
    for (ParseErrorMsg * msg = m_err_list.get_head();
         msg != nullptr; msg = m_err_list.get_next()) {
        delete msg;
    }
}


bool IRParser::checkKeyWordMap()
{
    for (UINT i = X_CODE_UNDEF; i < X_CODE_LAST; i++) {
        ASSERT0(i == (UINT)XCODEINFO_code(g_keyword_info[i]));
    }
    return true;
}


void IRParser::initKeyWordMap()
{
    for (UINT i = X_CODE_UNDEF + 1; i < X_CODE_LAST; i++) {
        m_str2xcode.set(g_keyword_info[i].name, (X_CODE)i);
    }
    for (UINT i = 0; i < (sizeof(g_property_code) /
         sizeof(g_property_code[0])); i++) {
        m_prop2xcode.set(g_keyword_info[g_property_code[i]].name,
                         g_property_code[i]);
    }
    for (UINT i = 0; i < (sizeof(g_stmt_code) /
         sizeof(g_stmt_code[0])); i++) {
        m_stmt2xcode.set(g_keyword_info[g_stmt_code[i]].name,
                         g_stmt_code[i]);
    }
    for (UINT i = 0; i < (sizeof(g_exp_code) /
         sizeof(g_exp_code[0])); i++) {
        m_exp2xcode.set(g_keyword_info[g_exp_code[i]].name,
                        g_exp_code[i]);
    }
    for (UINT i = 0; i < (sizeof(g_type_code) /
         sizeof(g_type_code[0])); i++) {
        m_type2xcode.set(g_keyword_info[g_type_code[i]].name,
                         g_type_code[i]);
    }
}


CHAR const* IRParser::getKeyWordName(X_CODE code) const
{
    ASSERT0(code >= X_CODE_UNDEF && code < X_CODE_LAST);
    return g_keyword_info[code].name;
}


bool IRParser::dump() const
{
    if (!getRegionMgr()->isLogMgrInit()) { return false; }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegionMgr(), "\n==---- DUMP %s ----==", getPassName());
    getRegionMgr()->getLogMgr()->incIndent(2);
    for (UINT i = 0; i < m_rm->getNumOfRegion(); i++) {
        Region const* rg = m_rm->getRegion(i);
        if (rg == nullptr) { continue; }
        rg->dump(false);
    }
    getRegionMgr()->getLogMgr()->decIndent(2);
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


//Find Var in nested region from the inside out.
Var * IRParser::findVar(ParseCtx * ctx, Sym const* name)
{
    ASSERT0(ctx && ctx->current_region && name);
    for (Region * region = ctx->current_region;
         region != nullptr; region = region->getParent()) {
        Var * var = region->findVarViaSymbol(name);
        if (var != nullptr) {
            return var;
        }
    }
    return nullptr;
}


X_CODE IRParser::getXCode(TOKEN tok, CHAR const* tok_string)
{
    if (tok != T_IDENTIFIER) { return X_CODE_UNDEF; }
    return m_str2xcode.get(tok_string);
}


X_CODE IRParser::getCurrentPropertyCode()
{
    return m_prop2xcode.get(m_lexer->getCurrentTokenString());
}


X_CODE IRParser::getCurrentStmtCode()
{
    return m_stmt2xcode.get(m_lexer->getCurrentTokenString());
}


X_CODE IRParser::getCurrentExpCode()
{
    return m_exp2xcode.get(m_lexer->getCurrentTokenString());
}


X_CODE IRParser::getCurrentTypeCode()
{
    return m_type2xcode.get(m_lexer->getCurrentTokenString());
}


X_CODE IRParser::getCurrentXCode()
{
    return getXCode(m_lexer->getCurrentToken(),
                    m_lexer->getCurrentTokenString());
}


void IRParser::error(UINT lineno, CHAR const* format, ...)
{
    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vsprint(format, arg);
    prt2C("\nerror(%d):%s", lineno, buf.buf);
    va_end(arg);

    ParseErrorMsg * msg = new ParseErrorMsg(10);
    m_err_list.append_tail(msg);
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
    ASSERT0_DUMMYUSE(tok);
    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vsprint(format, arg);
    prt2C("\nerror(%d):'%s', %s", m_lexer->getCurrentLineNum(),
          m_lexer->getCurrentTokenString(), buf.buf);
    va_end(arg);

    ParseErrorMsg * msg = new ParseErrorMsg(10);
    m_err_list.append_tail(msg);
}


void IRParser::error(X_CODE xcode, CHAR const* format, ...)
{
    DUMMYUSE(xcode);
    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vsprint(format, arg);
    prt2C("\nerror(%d):parse %s: %s", m_lexer->getCurrentLineNum(),
          getKeyWordName(xcode), buf.buf);
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
//    Cell * c = g_cell_list.get_head();
//    if (c != nullptr) {
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
//            append_tok_tail(g_real_token,
//                g_real_token_string, g_real_line_num);
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


//Return false if error occur.
bool IRParser::checkPhiOpndLabel(IR const* ir,
    xcom::TMap<LabelInfo const*, IR const*> const& labtab,
    ParseCtx const& ctx)
{
    ASSERT0(ir->is_phi());
    ASSERT0(ctx.current_region);
    DbxMgr * dbx_mgr = ctx.current_region->getDbxMgr();
    ASSERT0(dbx_mgr);
    for (IR * opnd = PHI_opnd_list(ir);
         opnd != nullptr; opnd = opnd->get_next()) {
        LabelInfo const* li = const_cast<ParseCtx&>(ctx).
            getIR2Label().get(opnd);
        if (li == nullptr) {
            error(xoc::getLineNum(opnd, LangInfo::LANG_CPP, dbx_mgr),
                  "no label corresponding to phi operand");
            return false;
        }

        bool find;
        labtab.get(li, &find);
        if (find) { continue; }

        StrBuf buf(32);
        error(xoc::getLineNum(ir, LangInfo::LANG_CPP, dbx_mgr),
              "use undefined label %s", li->getName(buf));
        return false;

    }
    return true;
}


//The function checks label for GR syntax legality.
//Return true if there is no error occur.
bool IRParser::checkLabel(IR const* irlist, ParseCtx const& ctx)
{
    ConstIRIter it;
    xcom::TMap<LabelInfo const*, IR const*> labtab;
    bool error_occur = false;
    if (irlist == nullptr) { return true; }
    ASSERT0(ctx.current_region);
    ASSERT0(ctx.current_region->hasAnaInstrument());
    DbxMgr * dbx_mgr = ctx.current_region->getDbxMgr();
    for (IR const* ir = iterInitC(irlist, it, true);
         ir != nullptr; ir = iterNextC(it, true)) {
        if (!ir->is_label()) { continue; }

        LabelInfo const* lab = ir->getLabel();
        bool find;
        IR const* mapped = labtab.get(lab, &find);
        if (find) {
            ASSERT0(mapped);
            StrBuf buf(32);
            error(xoc::getLineNum(ir, LangInfo::LANG_CPP, dbx_mgr),
                  "duplicated label %s, and has been defined at line:%d",
                  lab->getName(buf),
                  xoc::getLineNum(mapped, LangInfo::LANG_CPP, dbx_mgr));
            error_occur = true;
        }
        labtab.set(lab, ir);
    }
    for (IR const* ir = iterInitC(irlist, it, true);
         ir != nullptr; ir = iterNextC(it, true)) {
        if (ir->is_label()) { continue; }
        if (ir->is_phi()) {
            if (!checkPhiOpndLabel(ir, labtab, ctx)) {
                error_occur = true;
            }
            continue;
        }
        LabelInfo const* lab = ir->getLabel();
        if (lab == nullptr) { continue; }

        bool find;
        labtab.get(lab, &find);
        if (find) { continue; }

        StrBuf buf(32);
        IR const* stmt;
        if (ir->is_stmt()) { stmt = ir; }
        else { stmt = ir->getStmt(); }
        ASSERT0(stmt && stmt->is_stmt());
        error(xoc::getLineNum(stmt, LangInfo::LANG_CPP, dbx_mgr),
              "use undefined label %s", lab->getName(buf));
        error_occur = true;
    }
    return error_occur ? false : true;
}


//Return true if 'tok' can be regarded as identifier.
static bool regardAsId(TOKEN tok)
{
    return tok == T_IDENTIFIER || tok == T_STRING;
}


bool IRParser::parseRegionProp(OUT PropertySet & ps, ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_REGION;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }
    return true;
}


bool IRParser::parseRegionType(Region ** region, UFlag & flag, ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getCurrentToken();
    X_CODE code = getXCode(tok, m_lexer->getCurrentTokenString());
    switch (code) {
    case X_FUNC:
        *region = m_rm->newRegion(REGION_FUNC);
        (*region)->initPassMgr();
        (*region)->initDbxMgr();
        (*region)->initIRMgr();
        (*region)->initIRBBMgr();
        (*region)->initAttachInfoMgr();
        //SET_FLAG(*flag, VAR_LOCAL);
        break;
    case X_PROGRAM:
        *region = m_rm->newRegion(REGION_PROGRAM);
        (*region)->initPassMgr();
        (*region)->initDbxMgr();
        (*region)->initIRMgr();
        (*region)->initIRBBMgr();
        (*region)->initAttachInfoMgr();
        //SET_FLAG(*flag, VAR_GLOBAL);
        break;
    case X_INNER:
        *region = m_rm->newRegion(REGION_INNER);
        (*region)->initPassMgr();
        (*region)->initDbxMgr();
        (*region)->initIRMgr();
        (*region)->initIRBBMgr();
        (*region)->initAttachInfoMgr();
        //SET_FLAG(*flag, VAR_LOCAL);
        break;
    case X_BLACKBOX:
        *region = m_rm->newRegion(REGION_BLACKBOX);
        //SET_FLAG(*flag, VAR_LOCAL);
        break;
    default:
        error(tok, "miss valid region type");
        return false;
    }
    m_rm->addToRegionTab(*region);
    return true;
}


static bool verifyPRSSA(Region const* rg, PassMgr * pm, OptCtx & oc)
{
    OptCtx toc(oc);
    if (!toc.is_dom_valid()) {
        pm->checkValidAndRecompute(&toc, PASS_DOM, PASS_UNDEF);
    }
    ASSERT0(PRSSAMgr::verifyPRSSAInfo(rg, toc));
    return true;
}


//Return false if there is error occur.
bool IRParser::constructSSAIfNeed(ParseCtx * ctx)
{
    if (ctx->has_error || ctx->current_region->is_blackbox()) { return true; }
    ctx->current_region->setIRList(ctx->stmt_list);

    //TODO: build CFG by given parameters.
    if (ctx->has_phi) {
        //GR should not include CFS when PHI is in used.
        ASSERT0(!ctx->has_scf);
    }
    if (!ctx->has_phi) { return true; }
    Region * rg = ctx->current_region;
    rg->constructBBList();
    OptCtx * oc = getRegionMgr()->getAndGenOptCtx(ctx->current_region);
    ASSERT0(oc);
    rg->initPassMgr();
    rg->initDbxMgr();
    rg->initIRMgr();
    rg->initIRBBMgr();
    PassMgr * pm = rg->getPassMgr();
    OptCtx loc(*oc);

    //Do NOT merge label of empty BB before construct SSA mode, because the
    //empty BB may be predecessor of PHI, and merging label will incur the
    //function can not find the phi operand related predecessor.
    OC_do_merge_label(*oc) = false;
    pm->checkValidAndRecompute(oc, PASS_CFG, PASS_UNDEF);
    OC_do_merge_label(*oc) = loc.do_merge_label();

    //Reorder phi operand to match the order of CFG pred.
    SortPredByLab sort(rg->getCFG());
    sort.sort(ctx->getIR2Label());
    ASSERT0(rg->getCFG()->verifyPhiEdge(ctx->getIR2Label()));

    //Generate SSAInfo.
    PRSSAMgr * prssamgr = (PRSSAMgr*)pm->registerPass(PASS_PRSSA_MGR);
    ASSERT0(prssamgr);
    prssamgr->genSSAInfoForRegion();
    ASSERT0(verifyPRSSA(rg, pm, *oc));
    DUMMYUSE(verifyPRSSA);
    prssamgr->refinePhi(*oc);

    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpPRSSAMgr()) {
        START_TIMER(tdump, "IRParser: PRSSA: Dump After Pass");
        prssamgr->dump();
        END_TIMER(tdump, "IRParser: PRSSA: Dump After Pass");
    }
    return true;
}


bool IRParser::parseRegionName(Region * region, UFlag & flag, ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getNextToken();
    if (!regardAsId(tok)) {
        error(tok, "miss region name");
        return false;
    }
    Sym const* sym = m_rm->addToSymbolTab(m_lexer->getCurrentTokenString());
    Var * regionvar = nullptr;
    if (ctx->current_region != nullptr) {
        regionvar = findVar(ctx, sym);
        REGION_parent(region) = ctx->current_region;
    }
    if (regionvar == nullptr) {
        regionvar = m_rm->getVarMgr()->registerVar(sym,
            m_rm->getTypeMgr()->getAny(), CODE_ALIGNMENT, (VarFlag&)flag);
        regionvar->setFlag((VAR_FLAG)(VAR_IS_DECL|VAR_IS_REGION));
        if (region->is_function() || region->is_program()) {
            ASSERT0(regionvar);
            regionvar->setFlag(VAR_IS_FUNC);
        }
    }
    region->setRegionVar(regionvar);
    if (ctx->current_region != nullptr) {
        ctx->current_region->addToVarTab(regionvar);
    }
    if (!regionvar->is_func() &&
        (region->is_function() || region->is_program())) {
        error("var %s should be func_decl", SYM_name(sym));
    }
    return true;
}


bool IRParser::declareRegion(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_REGION);
    ASSERT0(ctx);
    m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    if (!parseRegionProp(ps, ctx)) {
        return false;
    }

    //Region Type
    Region * region = nullptr;
    VarFlag flag(0);
    if (!parseRegionType(&region, flag, ctx)) {
        return false;
    }
    ASSERT0(region);
    //Region name
    if (!parseRegionName(region, flag, ctx)) {
        return false;
    }

    if (region->is_program()) {
        if (m_rm->getProgramRegion() != nullptr) {
            error(m_lexer->getCurrentLineNum(),
                  "duplicated program region %s, previous is %s",
                  region->getRegionName(),
                  m_rm->getProgramRegion()->getRegionName());
            return false;
        }
        m_rm->setProgramRegion(region);
    }

    ParseCtx newctx(this);
    newctx.current_region = region;
    newctx.cleanRegionUniqueInfo();

    //Region Parameters
    m_lexer->getNextToken();
    if (!parseParameterList(&newctx)) {
        return false;
    }

    //Region Body
    START_TIMER_FMT(w, ("Parse Region(%d):%s",
                        region->id(), region->getRegionName()));
    if (!parseRegionBody(&newctx)) {
        END_TIMER_FMT(w, ("Parse Region(%d):%s",
                          region->id(), region->getRegionName()));
        return false;
    }
    END_TIMER_FMT(w,("Parse Region(%d):%s",
                     region->id(), region->getRegionName()));
    if (!checkLabel(newctx.stmt_list, newctx)) {
        return false;
    }

    if (!constructSSAIfNeed(&newctx)) {
        return false;
    }

    if (!newctx.current_region->is_blackbox()) {
        ASSERT0(verifyIRList(newctx.current_region->getIRList(),
                             nullptr, newctx.current_region));
    }
    if (ctx->current_region != nullptr) {
        IR * ir = ctx->current_region->getIRMgr()->buildRegion(region);
        copyProp(ir, ps, ctx);
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
    //START_TIMER(t, "IR Parser:Parsing Variable Declaration");
    for (; tok == T_IDENTIFIER;) {
        X_CODE code = getCurrentXCode();
        if (code == X_VAR) {
            Var * v;
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
    //END_TIMER_FMT(t, ("IR Parser:Parsing Variable Declaration"));

    if (ctx->current_region->is_blackbox()) {
        //Blackbox region does not contain stmt.
        if (!isEndOfScope() && !isEndOfAll()) {
            error(tok, "not valid blackbox region operation");
        }
    } else if (!parseStmtList(ctx)) {
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


//Return true if stmt list is correct.
bool IRParser::parseStmtList(ParseCtx * ctx)
{
    TOKEN tok = m_lexer->getCurrentToken();
    bool has_scf = false;
    bool has_phi = false;
    for (;;) {
        X_CODE code = getCurrentStmtCode();
        bool res = false;
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
            has_scf = true;
            break;
        case X_WHILE:
            res = parseWhileDo(ctx);
            has_scf = true;
            break;
        case X_DO_LOOP:
            res = parseDoLoop(ctx);
            has_scf = true;
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
            has_scf = true;
            break;
        case X_BREAK:
            res = parseBreak(ctx);
            has_scf = true;
            break;
        case X_RETURN:
            res = parseReturn(ctx);
            break;
        case X_CONTINUE:
            res = parseContinue(ctx);
            has_scf = true;
            break;
        case X_SWITCH:
            res = parseSwitch(ctx);
            break;
        default:
            if (isEndOfScope() || isEndOfAll()) {
                ctx->has_phi |= has_phi;
                ctx->has_scf |= has_scf;
                return !ctx->has_error;
            }
            error(tok, "not stmt operation");
            res = false;
        }

        if (has_scf && has_phi) {
            error(tok, "phi can not be compatible with high level ir");
            ctx->has_error = true;
        }

        if (!res) {
            ctx->has_error = true;
            //Error recovery.
            for (; !isTerminator(tok); tok = m_lexer->getNextToken()) {}
        }

        for (tok = m_lexer->getCurrentToken();
             tok == T_SEMI; tok = m_lexer->getNextToken());

        if (isTooManyError()) {
            ctx->has_phi |= has_phi;
            ctx->has_scf |= has_scf;
            return false;
        }

        //dumpIRList(ctx->stmt_list, m_rg);
    }
    UNREACHABLE();
    return !ctx->has_error;
}


bool IRParser::parseXOperator(ParseCtx * ctx)
{
    X_CODE code = getCurrentExpCode();
    switch (code) {
    case X_ID:
        return parseId(ctx);
    case X_LD:
        return parseLd(ctx);
    case X_ILD:
        return parseILd(ctx);
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
    case X_ALLOCA:
        return parseUnaryOp(IR_ALLOCA, ctx);
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
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_CASE;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }

    IR * case_det = nullptr;
    if (tok == T_IMM) {
        //Note if literal is larger than LONGLONG, one have to use longer type
        //instead of LONGLONG.
        ASSERT0(sizeof(HOST_INT) <= sizeof(LONGLONG));
        case_det = ctx->current_region->getIRMgr()->buildImmInt(
            (HOST_INT)xcom::xatoll(m_lexer->getCurrentTokenString(), false),
            m_tm->getSimplexType(m_tm->getIntDType(
                                 sizeof(HOST_INT)*BIT_PER_BYTE, true)));
        tok = m_lexer->getNextToken();

        Type const* ty = nullptr;
        if (tok == T_COLON) {
            tok = m_lexer->getNextToken();
            if (!parseType(ctx, &ty) || ty == nullptr) {
                error(tok, "illegal data type");
                return false;
            }
            IR_dt(case_det) = ty;
            tok = m_lexer->getCurrentToken();
        }
    } else if (tok == T_STRING) {
        case_det = ctx->current_region->getIRMgr()->buildString(
            m_rm->addToSymbolTab(m_lexer->getCurrentTokenString()));
        tok = m_lexer->getNextToken();
    } else if (tok == T_FP) {
        HOST_FP val = atof(m_lexer->getCurrentTokenString());
        tok = m_lexer->getNextToken();
        Type const* ty = nullptr;
        if (tok == T_COLON) {
            tok = m_lexer->getNextToken();
            if (!parseType(ctx, &ty) || ty == nullptr) {
                error(tok, "illegal data type");
                return false;
            }
        } else {
            ty = m_tm->getF64();
        }
        case_det = ctx->current_region->getIRMgr()->buildImmFP(val, ty);
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
    if (!regardAsId(tok)) {
        error(tok, "illegal target label of case");
        return false;
    }
    Sym const* sym = m_rm->addToSymbolTab(m_lexer->getCurrentTokenString());
    LabelInfo * caselab = ctx->mapSym2Label(sym);
    if (caselab == nullptr) {
        caselab = ctx->current_region->genCustomLabel(sym);
        ctx->setMapSym2Label(sym, caselab);
    }
    tok = m_lexer->getNextToken();

    IR * case_exp = ctx->current_region->getIRMgr()->buildCase(case_det,
                                                               caselab);
    copyProp(case_exp, ps, ctx);
    ctx->returned_exp = case_exp;
    return true;
}


bool IRParser::parseSelect(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_SELECT);
    TOKEN tok = m_lexer->getNextToken();
    ctx->returned_exp = nullptr;

    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "illegal data type");
            return false;
        }
    }

    if (!parseExp(ctx)) {
        return false;
    }
    IR * det = ctx->returned_exp;
    ctx->returned_exp = nullptr;
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
    if (truepart == nullptr) {
        error(tok, "select miss true part");
        return false;
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ','");
        return false;
    }
    tok = m_lexer->getNextToken();

    ctx->returned_exp = nullptr;
    if (!parseExp(ctx)) {
        return false;
    }
    IR * falsepart = ctx->returned_exp;
    ASSERT0(ctx->current_region);
    if (falsepart == nullptr) {
        error(tok, "select miss false part");
        return false;
    }

    IR * exp = ctx->current_region->getIRMgr()->buildSelect(det,
        truepart, falsepart, ty == nullptr ? m_tm->getAny() : ty);
    ctx->returned_exp = exp;
    return true;
}


PRNO IRParser::mapIden2Prno(CHAR const* prid, ParseCtx * ctx)
{
    Sym const* sym = m_rm->addToSymbolTab(prid);
    PRNO prno = ctx->mapSym2Prno(sym);
    if (prno == PRNO_UNDEF) {
        prno = ctx->current_region->getIRMgr()->buildPrno(m_tm->getAny());
        ctx->setMapSym2Prno(sym, prno);
    }
    return prno;
}


bool IRParser::parsePR(ParseCtx * ctx)
{
    ASSERT0(xcom::StrBuf::is_equal(PR_TYPE_CHAR, '$'));
    ASSERTN(m_lexer->getCurrentToken() == T_DOLLAR,
            ("miss %s before PR expression", PR_TYPE_CHAR));
    PRNO prno = PRNO_UNDEF;
    if (!parsePrno(&prno, ctx)) {
        return false;
    }

    TOKEN tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "invalide type of PR");
            return false;
        }
    } else {
        ty = m_tm->getAny();
    }
    ctx->returned_exp = ctx->current_region->getIRMgr()->buildPRdedicated(
        prno, ty);
    return true;
}


bool IRParser::isTerminator(TOKEN tok)
{
    return tok == T_END || tok == T_UNDEF || tok == T_SEMI;
}


bool IRParser::parseArrayDimension(List<TMWORD> & elem_dim)
{
    ASSERT0(m_lexer->getCurrentToken() == T_LSPAREN);
    TOKEN tok = m_lexer->getNextToken();

    for (; tok != T_RSPAREN && !isTerminator(tok);) {
        if (tok != T_IMM) {
            error(tok, "dimension must be integer");
            return false;
        }
        //Note if literal is larger than LONGLONG, one have to use longer type
        //instead of LONGLONG.
        ASSERT0(sizeof(TMWORD) <= sizeof(LONGLONG));
        TMWORD dim = (TMWORD)xcom::xatoll(
            m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
        elem_dim.append_tail(dim);

        if (tok != T_COMMA && tok != T_RSPAREN) {
            error(tok, "miss ',' in dimension declaration");
            return false;
        }
        if (tok == T_COMMA) {
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
    if (!regardAsId(tok)) {
        error(tok, "miss variable name");
        return false;
    }
    Sym const* sym = m_rm->addToSymbolTab(m_lexer->getCurrentTokenString());
    ASSERT0(sym);
    Var * var = findVar(ctx, sym);
    if (var == nullptr) {
        error(tok, "%s is not declared", m_lexer->getCurrentTokenString());
        return false;
    }

    IR * id = ctx->current_region->getIRMgr()->buildId(var);
    ctx->returned_exp = id;
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseLda(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_LDA);
    TOKEN tok = m_lexer->getNextToken();

    TMWORD offset = 0;
    if (tok == T_COLON) {
        //Offset
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        //Note if literal is larger than LONGLONG, one have to use longer type
        //instead of LONGLONG.
        ASSERT0(sizeof(TMWORD) <= sizeof(LONGLONG));
        offset = (TMWORD)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Lda base id
    if (!regardAsId(tok)) {
        error(tok, "miss variable name");
        return false;
    }
    Var * var = findVar(ctx, m_rm->addToSymbolTab(
        m_lexer->getCurrentTokenString()));
    if (var == nullptr) {
        error(tok, "%s is not declared", m_lexer->getCurrentTokenString());
        return false;
    }

    IR * lda = ctx->current_region->getIRMgr()->buildLda(var);
    LDA_ofst(lda) = (UINT)offset;
    ASSERT0(lda->getType());
    //IR_dt(lda) = m_tm->getPointerType(m_tm->getByteSize(var->getType()));
    ctx->returned_exp = lda;
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseStoreArray(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_STARRAY);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = nullptr;
    if (tok == T_COLON) {
        //Type
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "illegal data type");
            return false;
        }
    }
    if (ty == nullptr) {
        ty = m_tm->getAny();
    }

    tok = m_lexer->getCurrentToken();
    TMWORD offset = 0;
    if (tok == T_COLON) {
        //Offset
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        //Note if literal is larger than LONGLONG, one have to use longer type
        //instead of LONGLONG.
        ASSERT0(sizeof(TMWORD) <= sizeof(LONGLONG));
        offset = (TMWORD)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    tok = m_lexer->getCurrentToken();

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    List<TMWORD> dim_list;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_STARRAY;
        ps.dim_list = &dim_list;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }

    Type const* elem_ty = ty;
    if (ps.elemtype != nullptr) {
        elem_ty = ps.elemtype;
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
    ctx->returned_exp = nullptr;

    if (!base->isPtr()) {
        error("base expression of starray should be pointer");
        return false;
    }

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
    ctx->returned_exp = nullptr;

    tok = m_lexer->getCurrentToken();
     if (tok != T_RPAREN) {
        error(tok, "miss ')' after array subscript expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    TMWORD * elem_dim_buf = nullptr;
    if (dim_list.get_elem_count() > 0) {
        elem_dim_buf = (TMWORD*)ALLOCA(
            dim_list.get_elem_count() * sizeof(TMWORD));
        xcom::C<TMWORD> * ct = nullptr;
        dim_list.get_head(&ct);
        for (UINT i = 0; i < dim_list.get_elem_count(); i++) {
            elem_dim_buf[i] = ct->val();
            dim_list.get_next(&ct);
        }
    }

    if (dim_list.get_elem_count() != 0 &&
        xcom::cnt_list(subscript_list) != dim_list.get_elem_count()) {
        error("declare %d dimension array, but %d subscript given",
              dim_list.get_elem_count(), xcom::cnt_list(subscript_list));
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

    ASSERT0(ty && elem_ty);
    IR * ir = ctx->current_region->getIRMgr()->buildStoreArray(base,
        subscript_list, ty, elem_ty,
        xcom::cnt_list(subscript_list),
        elem_dim_buf, rhs);
    ctx->addIR(ir);
    ARR_ofst(ir) = offset;
    copyProp(ir, ps, ctx);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseArray(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_ARRAY);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    }

    tok = m_lexer->getCurrentToken();
    TMWORD offset = 0;
    if (tok == T_COLON) {
        //Offset
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        //Note if literal is larger than LONGLONG, one have to use longer type
        //instead of LONGLONG.
        ASSERT0(sizeof(TMWORD) <= sizeof(LONGLONG));
        offset = (TMWORD)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    List<TMWORD> dim_list;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_ARRAY;
        ps.dim_list = &dim_list;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }

    Type const* elem_ty = ty;
    if (ps.elemtype != nullptr) {
        elem_ty = ps.elemtype;
    }

    //Array base expression
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * base = ctx->returned_exp;
    ctx->returned_exp = nullptr;

    if (!base->isPtr()) {
        error("base expression of array should be pointer");
        return false;
    }

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
    ctx->returned_exp = nullptr;

    tok = m_lexer->getCurrentToken();
     if (tok != T_RPAREN) {
        error(tok, "miss ')' after array subscript expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    TMWORD * elem_dim_buf = nullptr;
    if (dim_list.get_elem_count() > 0) {
        elem_dim_buf = (TMWORD*)ALLOCA(
            dim_list.get_elem_count() * sizeof(TMWORD));
        xcom::C<TMWORD> * ct = nullptr;
        dim_list.get_head(&ct);
        for (UINT i = 0; i < dim_list.get_elem_count(); i++) {
            elem_dim_buf[i] = ct->val();
            dim_list.get_next(&ct);
        }
    }

    if (dim_list.get_elem_count() != 0 &&
        xcom::cnt_list(subscript_list) != dim_list.get_elem_count()) {
        error("declare %d dimension array, but %d subscript given",
              dim_list.get_elem_count(), xcom::cnt_list(subscript_list));
        return false;
    }

    IR * array = ctx->current_region->getIRMgr()->buildArray(base,
        subscript_list, ty == nullptr ? m_tm->getAny() : ty,
        elem_ty == nullptr ? m_tm->getAny() : elem_ty,
        xcom::cnt_list(subscript_list), elem_dim_buf);
    ARR_ofst(array) = offset;
    ctx->returned_exp = array;
    return true;
}


bool IRParser::parseILd(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_ILD);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = nullptr;
    if (tok == T_COLON) {
        //Type
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "illegal data type");
            return false;
        }
    }
    if (ty == nullptr) {
        ty = m_tm->getAny();
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
    PropertySet ps;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_ILD;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
    }

    //Parse base expression.
    if (!parseExp(ctx)) {
        return false;
    }
    if (ctx->returned_exp == nullptr) {
        error(tok, "illegal base expression of ild");
        return false;
    }
    IR * base = ctx->returned_exp;
    ctx->returned_exp = nullptr;

    if (!base->isPtr()) {
        error("base expression of ild should be pointer");
        return false;
    }

    IR * ild = ctx->current_region->getIRMgr()->buildILoad(base, ty);
    ILD_ofst(ild) = offset;
    copyProp(ild, ps, ctx);
    ctx->returned_exp = ild;
    return true;
}


bool IRParser::parseLd(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_LD);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "invalid type");
            return false;
        }
    }

    tok = m_lexer->getCurrentToken();
    TMWORD offset = 0;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        //Note if literal is larger than LONGLONG, one have to use longer type
        //instead of LONGLONG.
        ASSERT0(sizeof(TMWORD) <= sizeof(LONGLONG));
        offset = (TMWORD)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Properties
    PropertySet ps;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_LD;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
    }

    tok = m_lexer->getCurrentToken();
    if (!regardAsId(tok)) {
        error(tok, "miss variable name");
        return false;
    }

    Var * var = findVar(ctx, m_rm->addToSymbolTab(
        m_lexer->getCurrentTokenString()));
    if (var == nullptr) {
        error(tok, "%s is not declared", m_lexer->getCurrentTokenString());
        return false;
    }

    IR * ld = ctx->current_region->getIRMgr()->buildLoad(var,
        ty == nullptr ? var->getType() : ty);
    LD_ofst(ld) = offset;
    ctx->returned_exp = ld;
    copyProp(ld, ps, ctx);
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
            CONST_int_val(ctx->returned_exp) =
                -CONST_int_val(ctx->returned_exp);
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


bool IRParser::parseImmIR(ParseCtx * ctx)
{
    ASSERT0(m_lexer->getCurrentToken() == T_IMM);

    StrBuf immstr(8);
    //Note if literal is larger than LONGLONG, one have to use longer type
    //instead of LONGLONG.
    ASSERT0(sizeof(HOST_INT) <= sizeof(LONGLONG));
    HOST_INT v = xcom::xatoll(m_lexer->getCurrentTokenString(), false);
    immstr.strcat(m_lexer->getCurrentTokenString());
    TOKEN tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    } else {
        ty = m_tm->getI32(); //default type is I32.
    }

    IR * imm = nullptr;
    if (ty->is_int()) {
        imm = ctx->current_region->getIRMgr()->buildImmInt(v, ty);
    } else if (ty->is_fp()) {
        HOST_FP b = ::atof(immstr.buf);
        imm = ctx->current_region->getIRMgr()->buildImmFP(b, ty);
    } else if (ty->is_any()) {
        imm = ctx->current_region->getIRMgr()->buildImmAny(v);
    } else {
        StrBuf buf(64);
        error("'%s': illegal type for immediate",
              ctx->current_region->getTypeMgr()->dump_type(ty, buf));

        //Build CONST even if error occur to recover parsing.
        imm = ctx->current_region->getIRMgr()->buildImmInt(v, ty);
    }
    ctx->returned_exp = imm;
    return true;
}


bool IRParser::parseImmVal(ParseCtx * ctx)
{
    ASSERT0(m_lexer->getCurrentToken() == T_IMM);
    ASSERTN(PARSECTX_returned_imm_ty(ctx) == nullptr, ("should be clear"));
    xcom::StrBuf immstr(8);
    //Note if literal is larger than LONGLONG, one have to use longer type
    //instead of LONGLONG.
    ASSERT0(sizeof(HOST_INT) <= sizeof(LONGLONG));
    HOST_INT v = xcom::xatoll(m_lexer->getCurrentTokenString(), false);
    immstr.strcat(m_lexer->getCurrentTokenString());
    TOKEN tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    } else {
        ty = m_tm->getI32();
    }

    ASSERT0(ty);
    if (ty->is_int()) {
        PARSECTX_returned_imm_intval(ctx) = v;
    } else if (ty->is_fp()) {
        PARSECTX_returned_imm_fpval(ctx) = ::atof(immstr.buf);
    } else {
        UNREACHABLE();
    }
    PARSECTX_returned_imm_ty(ctx) = ty;
    return true;
}


bool IRParser::parseBool(ParseCtx * ctx)
{
    ASSERT0(m_lexer->getCurrentToken() == T_TRUE ||
            m_lexer->getCurrentToken() == T_FALSE);
    ctx->returned_exp = ctx->current_region->getIRMgr()->buildImmInt(
        m_lexer->getCurrentToken() == T_TRUE ? 1 : 0, m_tm->getBool());
    TOKEN tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    } else {
        ty = m_tm->getString();
    }
    if (!ty->is_bool()) {
        StrBuf buf(8);
        error("'%s': illegal type for boolean",
              ctx->current_region->getTypeMgr()->dump_type(ty, buf));
        return false;
    }
    return true;
}


bool IRParser::parseString(ParseCtx * ctx)
{
    ASSERT0(m_lexer->getCurrentToken() == T_STRING);
    ctx->returned_exp = ctx->current_region->getIRMgr()->buildString(
        m_rm->addToSymbolTab(m_lexer->getCurrentTokenString()));
    TOKEN tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    } else {
        ty = m_tm->getString();
    }
    if (!ty->is_string()) {
        StrBuf buf(8);
        error("'%s': illegal type for string",
              ctx->current_region->getTypeMgr()->dump_type(ty, buf));
        return false;
    }
    return true;
}


bool IRParser::parseFP(ParseCtx * ctx)
{
    ASSERT0(m_lexer->getCurrentToken() == T_FP);
    HOST_FP v = ::atof(m_lexer->getCurrentTokenString());
    TOKEN tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    } else {
        ty = m_tm->getF64();
    }
    IR * fp = ctx->current_region->getIRMgr()->buildImmFP(v, ty);
    ctx->returned_exp = fp;
    return true;
}


bool IRParser::parseExpList(ParseCtx * ctx)
{
    IR * last = nullptr;
    IR * param = nullptr;
    for (; isExp();) {
        if (!parseExp(ctx)) {
            return false;
        }
        xcom::add_next(&param, &last, ctx->returned_exp);
        ctx->returned_exp = nullptr;
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
        if (getCurrentXCode() != X_CODE_UNDEF) {
            return parseXOperator(ctx);
        }
        break;
    case T_ADD:
    case T_SUB:
        return parseSignImm(tok, ctx);
    case T_IMM:
        return parseImmIR(ctx);
    case T_FP:
        return parseFP(ctx);
    case T_STRING:
        return parseString(ctx);
    case T_TRUE:
    case T_FALSE:
        return parseBool(ctx);
    case T_DOLLAR:
        return parsePR(ctx);
    default:;
    }

    ctx->returned_exp = nullptr;
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
    case X_ALLOCA:
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
    case T_ADD: // +
    case T_SUB: // -
    case T_ASTERISK: // *
    case T_DIV: // /
    case T_AND: // &&
    case T_OR: // ||
    case T_BITAND: // &
    case T_BITOR: // |
    case T_LESSTHAN: // <
    case T_MORETHAN: // >
    case T_RSHIFT: // >>
    case T_LSHIFT: // <<
    case T_NOMORETHAN: // <=
    case T_NOLESSTHAN: // >=
    case T_NOEQU: // !=
    case T_NOT: // !
    case T_EQU: // ==
    case T_XOR: // ^
    case T_MOD: // %
    case T_REV: // ~ reverse  e.g:a = ~a
    case T_DOLLAR: //$
        return true;
    default:;
    }
    return false;
}


bool IRParser::parseBinaryOp(IR_CODE code, ParseCtx * ctx)
{
    ASSERT0(isBinaryOp(code));
    TOKEN tok = m_lexer->getNextToken();
    ctx->returned_exp = nullptr;

    //Type
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "illegal data type");
            return false;
        }
    }

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = code;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
    }

    if (!parseExp(ctx)) {
        return false;
    }

    IR * opnd0 = ctx->returned_exp;
    if (opnd0 == nullptr) {
        error(tok, "miss opnd0 for binary operation");
        return false;
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ','");
        return false;
    }

    tok = m_lexer->getNextToken();
    ctx->returned_exp = nullptr;
    if (!parseExp(ctx)) {
        return false;
    }
    IR * opnd1 = ctx->returned_exp;
    if (opnd1 == nullptr) {
        error(tok, "miss opnd1 for binary operation");
        return false;
    }

    ASSERT0(ctx->current_region);
    if (ty == nullptr) {
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
            ty = m_tm->getAny();
        }
    }

    IR * exp = ctx->current_region->getIRMgr()->buildBinaryOpSimp(
        code, ty, opnd0, opnd1);
    ctx->returned_exp = exp;
    copyProp(exp, ps, ctx);
    return true;
}


bool IRParser::parseCvt(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_CVT);
    TOKEN tok = m_lexer->getNextToken();
    ctx->returned_exp = nullptr;

    Type const* ty = nullptr;
    if (tok != T_COLON) {
        error(tok, "miss result type");
        return false;
    }
    tok = m_lexer->getNextToken();
    if (!parseType(ctx, &ty) || ty == nullptr) {
        error(tok, "illegal result type");
        return false;
    }

    if (!parseExp(ctx)) {
        return false;
    }

    IR * opnd = ctx->returned_exp;
    ASSERT0(opnd);

    if (ty == nullptr) {
        ty = m_tm->getAny();
    }

    IR * exp = ctx->current_region->getIRMgr()->buildUnaryOp(IR_CVT, ty, opnd);
    ctx->returned_exp = exp;
    return true;
}


bool IRParser::parseUnaryOp(IR_CODE code, ParseCtx * ctx)
{
    ASSERT0(isUnaryOp(code));
    TOKEN tok = m_lexer->getNextToken();
    ctx->returned_exp = nullptr;
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    }
    if (!parseExp(ctx)) {
        return false;
    }
    IR * opnd = ctx->returned_exp;
    ASSERT0(opnd);
    if (ty == nullptr) {
        if (IR::mustBeBoolType(code)) {
            ty = m_tm->getBool();
        } else {
            ty = m_tm->getAny();
        }
    }
    IR * exp = ctx->current_region->getIRMgr()->buildUnaryOp(code, ty, opnd);
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
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "illegal data type");
            return false;
        }
    }

    //Offset
    tok = m_lexer->getCurrentToken();
    TMWORD offset = 0;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        //Note if literal is larger than LONGLONG, one have to use longer type
        //instead of LONGLONG.
        ASSERT0(sizeof(TMWORD) <= sizeof(LONGLONG));
        offset = (TMWORD)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_ST;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
    }

    tok = m_lexer->getCurrentToken();
    if (!regardAsId(tok)) {
        error(tok, "miss variable name to be stored");
        return false;
    }

    Var * var = findVar(ctx, m_rm->addToSymbolTab(
        m_lexer->getCurrentTokenString()));
    if (var == nullptr) {
        error(tok, "%s is not declared", m_lexer->getCurrentTokenString());
        return false;
    }
    if (var->is_readonly()) {
        ASSERT0(var->get_name());
        error("can not write readonly variable '%s'",
              var->get_name()->getStr());
        return false;
    }

    tok = m_lexer->getNextToken();
    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }

    tok = m_lexer->getNextToken();
    if (!parseExp(ctx) || ctx->returned_exp == nullptr) {
        error(tok, "illegal rhs of store");
        return false;
    }

    IR * ir = nullptr;
    if (ty == nullptr) {
        ir = ctx->current_region->getIRMgr()->buildStore(var,
                                                         ctx->returned_exp);
    } else {
        ir = ctx->current_region->getIRMgr()->buildStore(var, ty,
                                                         ctx->returned_exp);
    }
    ST_ofst(ir) = offset;
    ctx->addIR(ir);
    ctx->returned_exp = nullptr;
    copyProp(ir, ps, ctx);
    return true;
}


bool IRParser::parseStorePR(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_STRP);
    TOKEN tok = m_lexer->getNextToken();
    ASSERT0(xcom::StrBuf::is_equal(PR_TYPE_CHAR, '$'));
    if (tok != T_DOLLAR) {
        error(tok, "miss %s specifier", PR_TYPE_CHAR);
        return false;
    }

    //PR no
    PRNO prno = PRNO_UNDEF;
    if (!parsePrno(&prno, ctx)) {
        return false;
    }

    //Type
    tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    } else {
        ty = m_tm->getAny();
    }

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_STPR;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }

    //Rhs
    tok = m_lexer->getNextToken();
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);

    IR * ir = ctx->current_region->getIRMgr()->buildStorePR(prno, ty,
                                                            ctx->returned_exp);
    copyProp(ir, ps, ctx);
    ctx->addIR(ir);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseModifyPR(X_CODE code, ParseCtx * ctx)
{
    ASSERT0(code == X_SETELEM || code == X_GETELEM);
    TOKEN tok = m_lexer->getNextToken();
    ASSERT0(xcom::StrBuf::is_equal(PR_TYPE_CHAR, '$'));
    if (tok != T_DOLLAR) {
        error(tok, "miss '$' specifier after %s", getKeyWordName(code));
        return false;
    }
    PRNO prno = PRNO_UNDEF;
    if (!parsePrno(&prno, ctx)) {
        return false;
    }

    tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "illegal data type");
            return false;
        }
    }
    if (ty == nullptr) {
        ty = m_tm->getAny();
    }

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = code == X_SETELEM ? IR_SETELEM : IR_GETELEM;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }

    IR * ir;
    if (code == X_GETELEM) {
        //Parse base of getelem.
        tok = m_lexer->getNextToken();
        if (!parseExp(ctx)) {
            return false;
        }
        IR * base = ctx->returned_exp;

        //Parse comma.
        tok = m_lexer->getCurrentToken();
        if (tok != T_COMMA) {
            error(tok, "miss ','");
            return false;
        }

        //Parse offset of base of getelem.
        tok = m_lexer->getNextToken();
        if (!parseExp(ctx)) {
            return false;
        }
        IR * offset = ctx->returned_exp;

        //Build IR stmt.
        ir = ctx->current_region->getIRMgr()->buildGetElem(prno, ty, base,
                                                           offset);
    } else {
        //Parse base of setelem.
        tok = m_lexer->getNextToken();
        if (!parseExp(ctx)) {
            return false;
        }
        IR * base = ctx->returned_exp;

        //Parse comma.
        tok = m_lexer->getCurrentToken();
        if (tok != T_COMMA) {
            error(code, "miss ','", getKeyWordName(code));
            return false;
        }

        //Parse value that to be set.
        tok = m_lexer->getNextToken();
        if (!parseExp(ctx)) {
            return false;
        }
        IR * val = ctx->returned_exp;

        //Parse comma.
        tok = m_lexer->getCurrentToken();
        if (tok != T_COMMA) {
            error(code, "miss ','");
            return false;
        }

        //Parse offset in base of setelem.
        tok = m_lexer->getNextToken();
        if (!parseExp(ctx)) {
            return false;
        }
        IR * offset = ctx->returned_exp;

        //Build IR stmt.
        ir = ctx->current_region->getIRMgr()->buildSetElem(prno, ty, base, val,
                                                           offset);
    }
    ctx->addIR(ir);
    ctx->returned_exp = nullptr;
    copyProp(ir, ps, ctx);
    return true;
}


bool IRParser::parseIStore(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_IST);
    TOKEN tok = m_lexer->getNextToken();

    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    } else {
        ty = m_tm->getAny();
    }

    //Offset
    tok = m_lexer->getCurrentToken();
    TMWORD offset = 0;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "illegal offset declaration, offset must be integer");
            return false;
        }
        //Note if literal is larger than LONGLONG, one have to use longer type
        //instead of LONGLONG.
        ASSERT0(sizeof(TMWORD) <= sizeof(LONGLONG));
        offset = (TMWORD)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
        tok = m_lexer->getNextToken();
    }

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_IST;
        if (!parseProperty(ps, ctx)) {
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

    //Parse base expression.
    tok = m_lexer->getNextToken();
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * base = ctx->returned_exp;
    ctx->returned_exp = nullptr;

    if (!base->isPtr()) {
        error("base expression of ist should be pointer");
        return false;
    }

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
    ctx->returned_exp = nullptr;

    IR * ir = ctx->current_region->getIRMgr()->buildIStore(base, rhs, ty);
    copyProp(ir, ps, ctx);
    IST_ofst(ir) = offset;
    ctx->addIR(ir);
    return true;
}


bool IRParser::parseStringLiteralPrno(PRNO * prno, CHAR const* str,
                                      ParseCtx * ctx)
{
    *prno = mapIden2Prno(str, ctx);
    if (*prno == PRNO_UNDEF) {
        error("use invalid PR number %u", *prno);
        return false;
    }
    return true;
}


bool IRParser::parseCustomizedPrno(PRNO * prno, ParseCtx * ctx)
{
    *prno = (PRNO)xcom::xatoll(m_lexer->getCurrentTokenString(), false);
    if (*prno > MAX_PRNO) {
        error("too large PR number %u", *prno);
        return false;
    }
    ctx->current_region->setPRCount(
        MAX(ctx->current_region->getPRCount(), *prno + 1));
    if (*prno == PRNO_UNDEF) {
        error("use invalid PR number %u", *prno);
        return false;
    }
    return true;
}


bool IRParser::parsePrno(PRNO * prno, ParseCtx * ctx)
{
    ASSERT0(xcom::StrBuf::is_equal(PR_TYPE_CHAR, '$'));
    ASSERTN(m_lexer->getCurrentToken() == T_DOLLAR,
            ("miss $ before PR expression"));
    TOKEN tok = m_lexer->getNextToken();
    *prno = PRNO_UNDEF;
    if (tok == T_IMM) {
        if (allowCustomizePrno()) {
            return parseCustomizedPrno(prno, ctx);
        }
        //Regard customized Prno as string literal.
        StrBuf buf(8);
        UINT x = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString(),
                                    false);
        buf.strcat("%d", x);
        return parseStringLiteralPrno(prno, buf.buf, ctx);
    }
    if (tok == T_IDENTIFIER) {
        return parseStringLiteralPrno(prno, m_lexer->getCurrentTokenString(),
                                      ctx);
    }
    error(tok, "not PR number");
    return false;
}


bool IRParser::parseCallAndICall(bool is_call, ParseCtx * ctx)
{
    ASSERT0(is_call ? getCurrentXCode() == X_CALL :
            getCurrentXCode() == X_ICALL);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = is_call ? IR_CALL : IR_ICALL;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
    }
    tok = m_lexer->getCurrentToken();

    UINT return_prno = PRNO_UNDEF;
    Type const* return_ty = m_tm->getAny();
    if (tok == T_DOLLAR) {
        //Expect return value.
        if (!parsePrno(&return_prno, ctx)) {
            return false;
        }

        tok = m_lexer->getNextToken();
        if (tok == T_COLON) {
            tok = m_lexer->getNextToken();
            if (!parseType(ctx, &return_ty) || return_ty == nullptr) {
                error(tok, "illegal data type");
                return false;
            }
        } else {
            return_ty = m_tm->getAny();
        }

        tok = m_lexer->getCurrentToken();
        if (tok != T_ASSIGN) {
            error(tok, "miss '='");
            return false;
        }
        tok = m_lexer->getNextToken();
    }

    Var * callee_var = nullptr;
    IR * callee_exp = nullptr;
    if (is_call) {
        if (!regardAsId(tok)) {
            error(tok, "miss callee function name");
            return false;
        }

        Sym const* name = m_rm->addToSymbolTab(
            m_lexer->getCurrentTokenString());
        callee_var = findVar(ctx, name);
        if (callee_var == nullptr) {
            error(tok, "can not find region %s", SYM_name(name));
            return false;
        }
        if (!callee_var->is_func() && !callee_var->is_region()) {
            error(tok, "%s is not region type region", SYM_name(name));
            return false;
        }
        if (ps.readonly && !callee_var->is_readonly()) {
            error(tok, "unmatch function call property, %s is %s",
                  SYM_name(name),
                  callee_var->is_readonly() ? "readonly" : "not readonly");
            return false;
        }
    } else {
        //Icall
        if (!parseExp(ctx)) {
            error(tok, "illegal callee expression");
            return false;
        }
        ASSERT0(ctx->returned_exp);
        callee_exp = ctx->returned_exp;
        ctx->returned_exp = nullptr;
        if (callee_exp->get_next() != nullptr) {
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

    IR * dummy_use_list = nullptr;
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
        DUMMYUSE(dummy_use_list);
    }

    IR * ir = nullptr;
    if (is_call) {
        ir = ctx->current_region->getIRMgr()->buildCall(callee_var, param_list,
                                            return_prno, return_ty);
    } else {
        ir = ctx->current_region->getIRMgr()->buildICall(callee_exp, param_list,
                                             return_prno, return_ty);
    }
    if (ps.ir_use_list != nullptr) {
        CALL_dummyuse(ir) = ps.ir_use_list;
        ir->setParent(CALL_dummyuse(ir));
    }
    copyProp(ir, ps, ctx);
    ctx->addIR(ir);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseGoto(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_GOTO);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_GOTO;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }

    if (!regardAsId(tok)) {
        error(tok, "miss label after goto");
        return false;
    }
    Sym const* sym = m_rm->addToSymbolTab(
        m_lexer->getCurrentTokenString());
    LabelInfo * label = ctx->mapSym2Label(sym);
    if (label == nullptr) {
        label = ctx->current_region->genCustomLabel(sym);
        ctx->setMapSym2Label(sym, label);
    }
    IR * ir = ctx->current_region->getIRMgr()->buildGoto(label);
    ctx->addIR(ir);
    m_lexer->getNextToken();
    copyProp(ir, ps, ctx);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseIGoto(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_IGOTO);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_IGOTO;
        if (!parseProperty(ps, ctx)) {
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
    if (ctx->returned_exp == nullptr) {
        error(tok, "miss determinate expression");
        return false;
    }
    IR * det = ctx->returned_exp;
    ctx->returned_exp = nullptr;
    if (det->get_next() != nullptr) {
        error(tok, "multiple determinate expression");
        return false;
    }

    //Case list
    IR * case_list = nullptr;
    IR * last = nullptr;
    for (;;) {
        if (!parseExp(ctx)) {
            error(tok, "illegal case expression");
            return false;
        }
        IR * case_exp = ctx->returned_exp;
        if (case_exp == nullptr) {
            break;
        }
        ctx->returned_exp = nullptr;

        xcom::add_next(&case_list, &last, case_exp);

        tok = m_lexer->getCurrentToken();
        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
            continue;
        }

        if (isTerminator(tok)) {
            break;
        }

        error(tok, "illegal case list");
        return false;
    }

    if (case_list == nullptr) {
        error(tok, "miss case list");
        return false;
    }

    IR * ir = ctx->current_region->getIRMgr()->buildIgoto(det, case_list);
    copyProp(ir, ps, ctx);
    ctx->addIR(ir);
    return true;
}


bool IRParser::parseDoWhile(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_DO);
    m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    TOKEN tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_DO_WHILE;
        if (!parseProperty(ps, ctx)) {
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

    IR * body = nullptr;
    {
        ParseCtx tmpctx(ctx);
        parseStmtList(&tmpctx);
        body = tmpctx.stmt_list;
        tmpctx.stmt_list = nullptr;
    }

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
    ctx->returned_exp = nullptr;

    IR * ir = ctx->current_region->getIRMgr()->buildDoWhile(det, body);
    ctx->addIR(ir);
    copyProp(ir, ps, ctx);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseWhileDo(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_WHILE);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_WHILE_DO;
        if (!parseProperty(ps, ctx)) {
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
    ctx->returned_exp = nullptr;

    //body
    tok = m_lexer->getCurrentToken();
    if (tok != T_LLPAREN) {
        error(tok, "miss '{' before if body");
        return false;
    }
    m_lexer->getNextToken();

    IR * body = nullptr;
    {
        ParseCtx tmpctx(ctx);
        parseStmtList(&tmpctx);
        body = tmpctx.stmt_list;
        tmpctx.stmt_list = nullptr;
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "miss '}' after if body");
        return false;
    }
    m_lexer->getNextToken();

    IR * ir = ctx->current_region->getIRMgr()->buildWhileDo(det, body);
    ctx->addIR(ir);
    copyProp(ir, ps, ctx);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseDoLoop(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_DO_LOOP);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_DO_LOOP;
        if (!parseProperty(ps, ctx)) {
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
    ctx->returned_exp = nullptr;
    if (!iv->is_id() && !iv->is_pr()) {
        error(tok, "induction variable must be ID or PR");
        return false;
    }

    if (iv->get_next() != nullptr) {
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
    ctx->returned_exp = nullptr;
    if (init->get_next() != nullptr) {
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
    ctx->returned_exp = nullptr;
    if (det->get_next() != nullptr) {
        error(tok, "multiple determinate expression");
        return false;
    }

    //if (iv->is_id()) {
    //    det = ctx->current_region->getIRMgr()->buildCmp(IR_LE,
    //        ctx->current_region->getIRMgr()->buildLoad(ID_info(iv)), det);
    //} else {
    //    ASSERT0(iv->is_pr());
    //    det = ctx->current_region->getIRMgr()->buildCmp(IR_LE,
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
    ctx->returned_exp = nullptr;
    if (step->get_next() != nullptr) {
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

    IR * body = nullptr;
    {
        ParseCtx tmpctx(ctx);
        parseStmtList(&tmpctx);
        body = tmpctx.stmt_list;
        tmpctx.stmt_list = nullptr;
    }
    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "miss '}' after doloop body");
        return false;
    }

    IR * ir = ctx->current_region->getIRMgr()->buildDoLoop(iv, init, det,
                                                           step, body);
    ctx->addIR(ir);
    copyProp(ir, ps, ctx);
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
            LABELINFO_is_try_start(label) = true;
            m_lexer->getNextToken();
            break;
        case X_TRY_END:
            LABELINFO_is_try_end(label) = true;
            m_lexer->getNextToken();
            break;
        case X_TERMINATE:
            LABELINFO_is_terminate(label) = true;
            m_lexer->getNextToken();
            break;
        case X_CATCH_START:
            LABELINFO_is_catch_start(label) = true;
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
        }

        if (tok == T_RPAREN) {
            m_lexer->getNextToken();
            break;
        }

        error(tok, "illegal label declaration or miss ','");
        return false;
    }
    return true;
}


bool IRParser::parseLabel(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_LABEL);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_LABEL;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }

    if (!regardAsId(tok)) {
        error(tok, "illegal label");
        return false;
    }

    Sym const* sym = m_rm->addToSymbolTab(m_lexer->getCurrentTokenString());
    LabelInfo * label = ctx->mapSym2Label(sym);
    if (label == nullptr) {
        label = ctx->current_region->genCustomLabel(sym);
        ctx->setMapSym2Label(sym, label);
    }
    tok = m_lexer->getNextToken();

    if (tok == T_LPAREN) {
        if (!parseLabelProperty(label)) {
            return false;
        }
    }

    IR * ir = ctx->current_region->getIRMgr()->buildLabel(label);
    ctx->addIR(ir);
    copyProp(ir, ps, ctx);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseBranch(bool is_truebr, ParseCtx * ctx)
{
    ASSERT0(is_truebr ? getCurrentXCode() == X_TRUEBR :
            getCurrentXCode() == X_FALSEBR);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = is_truebr ? IR_TRUEBR : IR_FALSEBR;
        if (!parseProperty(ps, ctx)) {
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
    if (det->get_next() != nullptr) {
        error(tok, "multiple determinate-expression");
        return false;
    }
    ctx->returned_exp = nullptr;

    tok = m_lexer->getCurrentToken();
    if (tok != T_COMMA) {
        error(tok, "miss ',' after determinant-expression");
        return false;
    }
    tok = m_lexer->getNextToken();

    if (!regardAsId(tok)) {
        error(tok, "miss label after determinate-expression");
        return false;
    }
    Sym const* sym = m_rm->addToSymbolTab(
        m_lexer->getCurrentTokenString());
    LabelInfo * label = ctx->mapSym2Label(sym);
    if (label == nullptr) {
        label = ctx->current_region->genCustomLabel(sym);
        ctx->setMapSym2Label(sym, label);
    }
    m_lexer->getNextToken();

    IR * ir = ctx->current_region->getIRMgr()->buildBranch(is_truebr, det,
                                                           label);
    ctx->addIR(ir);
    copyProp(ir, ps, ctx);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parsePhi(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_PHI);
    TOKEN tok = m_lexer->getNextToken();

    //Result
    ASSERT0(xcom::StrBuf::is_equal(PR_TYPE_CHAR, '$'));
    if (tok != T_DOLLAR) {
        error(tok, "miss $ specifier");
        return false;
    }

    PRNO prno = PRNO_UNDEF;
    if (!parsePrno(&prno, ctx)) {
        return false;
    }

    tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty) || ty == nullptr) {
            error(tok, "illegal data type");
            return false;
        }
    } else {
        ty = m_tm->getAny();
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_ASSIGN) {
        error(tok, "miss '='");
        return false;
    }
    tok = m_lexer->getNextToken();

    IR * opnd_list = nullptr;
    IR * last = nullptr;
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
        ctx->returned_exp = nullptr;
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
        if (!regardAsId(tok)) {
            error(tok, "miss label after phi operand");
            return false;
        }
        Sym const* sym = m_rm->addToSymbolTab(
            m_lexer->getCurrentTokenString());
        LabelInfo * label = ctx->mapSym2Label(sym);
        if (label == nullptr) {
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
        }

        if (tok == T_SEMI) {
            break;
        }

        error(tok, "illegal %s in phi operation declaration",
              m_lexer->getCurrentTokenString());
    }

    IR * ir = ctx->current_region->getIRMgr()->buildPhi(prno, ty, opnd_list);
    ctx->addIR(ir);
    return true;
}


bool IRParser::parseIf(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_IF);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_IF;
        if (!parseProperty(ps, ctx)) {
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
    ctx->returned_exp = nullptr;
    tok = m_lexer->getCurrentToken();

    //truebody
    if (tok != T_LLPAREN) {
        error(tok, "miss '{' before if body");
        return false;
    }
    m_lexer->getNextToken();

    IR * truebody = nullptr;
    {
        ParseCtx tmpctx(ctx);
        parseStmtList(&tmpctx);
        truebody = tmpctx.stmt_list;
        tmpctx.stmt_list = nullptr;
    }

    tok = m_lexer->getCurrentToken();
    if (tok != T_RLPAREN) {
        error(tok, "miss '}' after if body");
        return false;
    }

    IR * falsebody = nullptr;
    tok = m_lexer->getNextToken();
    if (tok == T_IDENTIFIER && getCurrentXCode() == X_ELSE) {
        tok = m_lexer->getNextToken();
        if (tok != T_LLPAREN) {
            error(tok, "miss '{' before if-else body");
            return false;
        }
        m_lexer->getNextToken();

        {
            ParseCtx tmpctx(ctx);
            parseStmtList(&tmpctx);
            falsebody = tmpctx.stmt_list;
            tmpctx.stmt_list = nullptr;
        }

        tok = m_lexer->getCurrentToken();
        if (tok != T_RLPAREN) {
            error(tok, "miss '}' after if-else body");
            return false;
        }
        m_lexer->getNextToken();
    }

    IR * ir = ctx->current_region->getIRMgr()->buildIf(det, truebody,
                                                       falsebody);
    ctx->addIR(ir);
    copyProp(ir, ps, ctx);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseBreak(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_BREAK);
    ctx->addIR(ctx->current_region->getIRMgr()->buildBreak());
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseReturn(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_RETURN);
    m_lexer->getNextToken();
    ctx->returned_exp = nullptr;
    if (!parseExp(ctx)) {
        return false;
    }
    IR * ir = ctx->current_region->getIRMgr()->buildReturn(ctx->returned_exp);
    ctx->addIR(ir);
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseContinue(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_CONTINUE);
    ctx->addIR(ctx->current_region->getIRMgr()->buildContinue());
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseSwitch(ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_SWITCH);
    TOKEN tok = m_lexer->getNextToken();

    //Properties
    PropertySet ps;
    tok = m_lexer->getCurrentToken();
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        ctx->ircode = IR_SWITCH;
        if (!parseProperty(ps, ctx)) {
            error(tok, "illegal property declaration");
            return false;
        }
        ctx->ircode = IR_UNDEF;
        tok = m_lexer->getCurrentToken();
    }

    //Switch value expression
    if (!parseExp(ctx)) {
        return false;
    }
    ASSERT0(ctx->returned_exp);
    IR * det = ctx->returned_exp;
    ctx->returned_exp = nullptr;
    if (det->get_next() != nullptr) {
        error(tok, "multiple determinate expression");
        return false;
    }

    //Case list and default label.
    IR * case_list = nullptr;
    IR * last = nullptr;
    LabelInfo * deflab = nullptr;
    for (;;) {
        if (getCurrentXCode() == X_DEFAULT) {
            tok = m_lexer->getNextToken();
            if (!regardAsId(tok)) {
                error(tok, "illegal default label");
                return false;
            }
            Sym const* sym = m_rm->addToSymbolTab(
                m_lexer->getCurrentTokenString());
            deflab = ctx->mapSym2Label(sym);
            if (deflab == nullptr) {
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
            if (case_exp != nullptr) {
                xcom::add_next(&case_list, &last, case_exp);
                //break;
            }
            ctx->returned_exp = nullptr;
        }
        tok = m_lexer->getCurrentToken();
        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
            continue;
        }

        if (tok == T_LLPAREN || tok == T_SEMI) {
            break;
        }

        error(tok, "illegal case list");
        return false;
    }

    //Switch body
    tok = m_lexer->getCurrentToken();
    IR * body = nullptr;
    if (tok != T_SEMI) {
        if (tok != T_LLPAREN) {
            error(tok, "miss '{' before switch body");
            return false;
        }
        tok = m_lexer->getNextToken();

        {
            ParseCtx tmpctx(ctx);
            parseStmtList(&tmpctx);
            body = tmpctx.stmt_list;
            tmpctx.stmt_list = nullptr;
        }

        tok = m_lexer->getCurrentToken();
        if (tok != T_RLPAREN) {
            error(tok, "miss '}' after doloop body");
            return false;
        }
        m_lexer->getNextToken();
    }

    if (body != nullptr) {
        ctx->has_scf = true;
    }
    IR * ir = ctx->current_region->getIRMgr()->buildSwitch(det, case_list,
                                                           body, deflab);
    copyProp(ir, ps, ctx);
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
    for (; tok != T_RPAREN && tok != T_END && tok != T_UNDEF; i++) {
        Var * v = nullptr;
        if (getCurrentXCode() == X_UNDEFINED) {
            //The parameter is reserved.
            tok = m_lexer->getNextToken();
        } else if (declareVar(ctx, &v)) {
            ASSERT0(v);
            v->setFlag(VAR_IS_FORMAL_PARAM);
            VAR_formal_param_pos(v) = i;
        } else {
            error(tok, "invalide parameter list");
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

    X_CODE code = getCurrentTypeCode();
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
    case X_VEC: {
        //Begin charator of vecter type.
        tok = m_lexer->getNextToken();
        if (tok != T_LESSTHAN) {
            error(tok, "miss '<' before size declaration");
            return false;
        }

       //Data type of vector element.
        tok = m_lexer->getNextToken();
        Type const* elem_type = nullptr;
        if (!parseType(ctx, &elem_type) || elem_type == nullptr) {
            error(tok, "illegal element type");
            return false;
        }
        if (!elem_type->is_scalar()) {
            error(tok, "element type must be scalar type");
            return false;
        }

        //Multiplier of vector operation.
        tok = m_lexer->getCurrentToken();
        if (tok != T_ASTERISK) {
            error(tok, "miss '*' in vector type declaration");
            return false;
        }

        //Number of vector element.
        tok = m_lexer->getNextToken();
        if (tok != T_IMM) {
            error(tok, "size must be integer");
            return false;
        }
        UINT elemsize = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString());

        //End charator of vecter type.
        tok = m_lexer->getNextToken();
        if (tok != T_MORETHAN) {
            error(tok, "miss '<' before size declaration");
            return false;
        }
        m_lexer->getNextToken();
        *ty = m_tm->getVectorType(elemsize, TY_dtype(elem_type));
        break;
    }
    case X_BOOL:
        *ty = m_tm->getBool();
        m_lexer->getNextToken();
        break;
    case X_ANY:
        *ty = m_tm->getAny();
        m_lexer->getNextToken();
        break;
    default:
        ; //There is no a valid type or no type declared.
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
    case X_ANY:
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

    *size = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString());

    tok = m_lexer->getNextToken();
    if (tok != T_MORETHAN) {
        error(tok, "miss '>' after size declaration");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


//List of type and property declaration.
bool IRParser::declareVarProperty(Var * var, ParseCtx * ctx)
{
    if (m_lexer->getCurrentToken() != T_LPAREN) {
        error(m_lexer->getCurrentToken(),
            "miss '(' before var property declaration");
        return false;
    }
    TOKEN tok = m_lexer->getNextToken();
    for (;;) {
        switch (tok) {
        case T_VOLATILE:
            var->setFlag(VAR_VOLATILE);
            tok = m_lexer->getNextToken();
            break;
        case T_RESTRICT:
            var->setFlag(VAR_IS_RESTRICT);
             tok = m_lexer->getNextToken();
             break;
        case T_RPAREN:
        case T_UNDEF:
        case T_END:
            break;
        case T_IDENTIFIER:
            switch (getCurrentXCode()) {
            case X_READONLY:
                var->setFlag(VAR_READONLY);
                tok = m_lexer->getNextToken();
                break;
            case X_PRIVATE:
                var->setFlag(VAR_PRIVATE);
                tok = m_lexer->getNextToken();
                break;
            case X_VOLATILE:
                var->setFlag(VAR_VOLATILE);
                tok = m_lexer->getNextToken();
                break;
            case X_FUNC:
                var->setFlag((VAR_FLAG)(VAR_IS_FUNC|VAR_IS_REGION));
                tok = m_lexer->getNextToken();
                break;
            case X_FAKE:
                var->setFlag(VAR_FAKE);
                tok = m_lexer->getNextToken();
                break;
            case X_GLOBAL:
                var->setFlag(VAR_GLOBAL);
                tok = m_lexer->getNextToken();
                break;
            case X_ARRAY:
                var->setFlag(VAR_IS_ARRAY);
                tok = m_lexer->getNextToken();
                break;
            case X_RESTRICT:
                var->setFlag(VAR_IS_RESTRICT);
                tok = m_lexer->getNextToken();
                break;
            case X_STRING:
                if (!parseStringValue(var, ctx)) {
                    return false;
                }
                break;
            case X_BYTE:
                if (!parseByteValue(var, ctx)) {
                    return false;
                }
                break;
            case X_UNALLOCABLE:
                var->setFlag(VAR_IS_UNALLOCABLE);
                tok = m_lexer->getNextToken();
                break;
            case X_DECL:
                var->setFlag(VAR_IS_DECL);
                tok = m_lexer->getNextToken();
                break;
            case X_ALIGN:
                if (!parseAlign(var, ctx)) {
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


bool IRParser::parseAlign(Var * var, ParseCtx *)
{
    ASSERT0(getCurrentXCode() == X_ALIGN);
    TOKEN tok = m_lexer->getNextToken();
    if (tok != T_LPAREN) {
        error(tok, "miss '(' after 'align'");
        return false;
    }
    tok = m_lexer->getNextToken();
    if (tok != T_IMM) {
        error(tok, "alignment must be integer");
        return false;
    }
    VAR_align(var) = (UINT)xcom::xatoll(m_lexer->getCurrentTokenString());
    tok = m_lexer->getNextToken();
    if (m_lexer->getCurrentToken() != T_RPAREN) {
        error(tok, "miss ')'");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseByteValue(Var * var, ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_BYTE);
    TOKEN tok = m_lexer->getNextToken();
    if (tok != T_LPAREN) {
        error(tok, "miss '(' after 'byte'");
        return false;
    }
    tok = m_lexer->getNextToken();
    xcom::Vector<BYTE> buf;
    UINT bytesize = 0;
    for (; tok != T_RPAREN && !isTerminator(tok);) {
        if (!parseImmVal(ctx) || PARSECTX_returned_imm_ty(ctx) == nullptr) {
            error(tok, "illegal literal");
            return false;
        }
        if (!PARSECTX_returned_imm_ty(ctx)->is_int()) {
            error(tok, "not integer");
            return false;
        }
        buf.set(bytesize, (BYTE)PARSECTX_returned_imm_intval(ctx));
        PARSECTX_returned_imm_ty(ctx) = nullptr;
        bytesize++;
        tok = m_lexer->getCurrentToken();
        if (tok != T_COMMA && tok != T_RPAREN) {
            error(tok, "miss ','");
            return false;
        }
        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
        }
    }
    VAR_byte_val(var) = ctx->current_region->allocByteBuf(bytesize);
    var->setFlag(VAR_HAS_INIT_VAL);
    ::memcpy(BYTEBUF_buffer(VAR_byte_val(var)), buf.get_vec(), bytesize);
    if (m_lexer->getCurrentToken() != T_RPAREN) {
        error(tok, "miss ')'");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseStringValue(Var * var, ParseCtx *)
{
    ASSERT0(getCurrentXCode() == X_STRING);
    if (!var->is_string()) {
        error(m_lexer->getCurrentToken(), "variable must be string type");
        return false;
    }
    TOKEN tok = m_lexer->getNextToken();
    if (tok != T_LPAREN) {
        error(tok, "miss '(' after 'string'");
        return false;
    }
    tok = m_lexer->getNextToken();
    if (tok != T_STRING) {
        error(m_lexer->getCurrentToken(), "need string literal");
        return false;
    }
    VAR_string(var) = m_rm->addToSymbolTab(m_lexer->getCurrentTokenString());
    var->setFlag(VAR_HAS_INIT_VAL);
    tok = m_lexer->getNextToken();
    if (tok != T_RPAREN) {
        error(tok, "miss ')'");
        return false;
    }
    m_lexer->getNextToken();
    return true;
}


bool IRParser::parseUseProperty(PropertySet & ps, ParseCtx * ctx)
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
    ps.ir_use_list = ctx->returned_exp;
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseElemTypeProperty(PropertySet & ps, ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_ELEMTYPE);
    TOKEN tok = m_lexer->getNextToken();
    Type const* elemtype = nullptr;
    if (tok != T_COLON) {
        error(tok, "miss ':' after elemtype");
        return false;
    }

    //Array element type
    tok = m_lexer->getNextToken();
    if (!parseType(ctx, &elemtype) || elemtype == nullptr) {
        error(tok, "illegal elemtype");
        return false;
    }

    ps.elemtype = elemtype;
    return true;
}


bool IRParser::parseDimProperty(PropertySet & ps, ParseCtx *)
{
    ASSERT0(getCurrentXCode() == X_DIM);
    TOKEN tok = m_lexer->getNextToken();

    //Array dimension declaration
    if (tok != T_LSPAREN) {
        error(tok, "illegal dimension declaration");
        return false;
    }
    ASSERT0(ps.dim_list);
    if (!parseArrayDimension(*ps.dim_list)) {
        return false;
    }
    ASSERT0(ps.dim_list->get_elem_count() > 0);
    return true;
}


bool IRParser::parseDefProperty(PropertySet & ps, ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_DEF);
    TOKEN tok = m_lexer->getNextToken();
    if (tok != T_LPAREN) {
        error(tok, "miss '(' after 'def'");
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
    ps.ir_def_list = ctx->returned_exp;
    ctx->returned_exp = nullptr;
    return true;
}


bool IRParser::parseThrowTarget(PropertySet & ps, ParseCtx * ctx)
{
    ASSERT0(getCurrentXCode() == X_THROW);
    TOKEN tok = m_lexer->getNextToken();
    if (tok != T_LPAREN) {
        error(tok, "miss '(' after 'throw'");
        return false;
    }

    tok = m_lexer->getNextToken();
    ps.getLabelList().clean();
    for (; tok != T_RPAREN && !isTerminator(tok);) {
        if (!regardAsId(tok)) {
            error(tok, "illegal label");
            return false;
        }
        Sym const* sym = m_rm->addToSymbolTab(
            m_lexer->getCurrentTokenString());
        LabelInfo * label = ctx->mapSym2Label(sym);
        if (label == nullptr) {
            label = ctx->current_region->genCustomLabel(sym);
            ctx->setMapSym2Label(sym, label);
        }
        ps.getLabelList().append_tail(label);
        tok = m_lexer->getNextToken();

        if (tok != T_COMMA && tok != T_RPAREN) {
            error(tok, "miss ',' in dimension declaration");
            return false;
        }
        if (tok == T_COMMA) {
            tok = m_lexer->getNextToken();
        }
    }

    if (m_lexer->getCurrentToken() != T_RPAREN) {
        error(tok, "miss ')'");
        return false;
    }

    m_lexer->getNextToken();
    ctx->returned_exp = nullptr;
    return true;
}


//List of property declaration.
bool IRParser::parseProperty(PropertySet & ps, ParseCtx * ctx)
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
        case T_UNDEF:
        case T_END:
            break;
        case T_IDENTIFIER:
            switch (getCurrentPropertyCode()) {
            case X_READONLY:
                ASSERT0(ctx);
                if (ctx->ircode != IR_ICALL &&
                    ctx->ircode != IR_CALL &&
                    ctx->ircode != IR_REGION) {
                    error(tok, "%s does not have READONLY property",
                          IRCNAME(ctx->ircode));
                    return false;
                }
                ps.readonly = true;
                tok = m_lexer->getNextToken();
                break;
            case X_RMW:
                ps.read_modify_write = true;
                tok = m_lexer->getNextToken();
                break;
            case X_THROW:
                ps.throw_exception = true;
                if (!parseThrowTarget(ps, ctx)) {
                    return false;
                }
                break;
            case X_SIDEEFFECT:
                ps.sideeffect = true;
                tok = m_lexer->getNextToken();
                break;
            case X_NOMOVE:
                ps.nomove = true;
                tok = m_lexer->getNextToken();
                break;
            case X_ATOM:
                ps.atomic = true;
                tok = m_lexer->getNextToken();
                break;
            case X_TERMINATE:
                ps.terminate = true;
                tok = m_lexer->getNextToken();
                break;
            case X_USE:
                ASSERT0(ctx);
                if (ctx->ircode != IR_CALL &&
                    ctx->ircode != IR_ICALL &&
                    ctx->ircode != IR_REGION) {
                    error(tok, "%s does have USE property",
                          IRCNAME(ctx->ircode));
                    return false;
                }
                if (!parseUseProperty(ps, ctx)) {
                    return false;
                }
                break;
            case X_DEF:
                ASSERT0(ctx);
                if (ctx->ircode != IR_CALL &&
                    ctx->ircode != IR_ICALL &&
                    ctx->ircode != IR_REGION) {
                    error(tok, "%s does have DEF property",
                          IRCNAME(ctx->ircode));
                    return false;
                }
                if (!parseDefProperty(ps, ctx)) {
                    return false;
                }
                break;
            case X_ELEMTYPE:
                ASSERT0(ctx);
                if (ctx->ircode != IR_STARRAY && ctx->ircode != IR_ARRAY) {
                    error(tok, "%s does have elemtype property",
                        IRCNAME(ctx->ircode));
                    return false;
                }
                if (!parseElemTypeProperty(ps, ctx)) {
                    return false;
                }
                break;
            case X_DIM:
                ASSERT0(ctx);
                if (ctx->ircode != IR_STARRAY && ctx->ircode != IR_ARRAY) {
                    error(tok, "%s does have dim property",
                        IRCNAME(ctx->ircode));
                    return false;
                }
                if (!parseDimProperty(ps, ctx)) {
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


bool IRParser::declareVar(ParseCtx * ctx, Var ** var)
{
    if (getCurrentXCode() != X_VAR) {
        error(m_lexer->getCurrentToken(), "miss 'var'");
        return false;
    }
    TOKEN tok = m_lexer->getNextToken();
    if (!regardAsId(tok)) {
        error(tok, "miss identifier name");
        return false;
    }
    Sym const* sym = m_rm->addToSymbolTab(m_lexer->getCurrentTokenString());

    //Type
    tok = m_lexer->getNextToken();
    Type const* ty = nullptr;
    if (tok == T_COLON) {
        tok = m_lexer->getNextToken();
        if (!parseType(ctx, &ty)) {
            error(tok, "invalide type of variable");
            return false;
        }
    }
    if (ty == nullptr) {
        ty = m_tm->getAny();
    }

    ASSERT0(ctx->current_region);
    Var * v = nullptr;
    if (m_rm->getVarMgr()->isDedicatedStringVar(sym->getStr())) {
        //User's GR file need the flag to be true.
        m_rm->setRegardAllStringAsSameMD(true);
        MD const* md = m_rm->genDedicateStrMD();
        ASSERT0(md);
        v = md->get_base();
    } else {
        v = m_rm->getVarMgr()->registerVar(sym, ty,
            1, //default alignment is 1.
            ctx->current_region->is_program() ? VAR_GLOBAL : VAR_LOCAL);
    }
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
    if (!v->is_unallocable()) {
        if (v->is_func() && (v->get_align() % CODE_ALIGNMENT) != 0) {
            error("function variable alignment should be divided by %d",
                  CODE_ALIGNMENT);
            return false;
        }
        if ((v->get_align() % MEMORY_ALIGNMENT) != 0) {
            error("variable alignment should be divided by %d",
                  MEMORY_ALIGNMENT);
            return false;
        }
    }
    return true;
}


//This function parses tokens that returned by lexer untill meeting file end.
//Return true if no error occur.
bool IRParser::parse()
{
    START_TIMER(t, "IR Parser");
    ASSERT0(m_lexer);
    TOKEN tok = T_UNDEF;
    //Get first token.
    //CASE: There are T_UNDEF token return.
    while ((tok = m_lexer->getNextToken()) == T_UNDEF) {;}
    for (;; tok = m_lexer->getNextToken()) {
        switch (tok) {
        case T_END:
            //Mee the file end.
            goto END;
        case T_UNDEF:
            //There may be error occurred, terminate parsing.
            goto END;
        case T_IDENTIFIER: {
            X_CODE code = getCurrentXCode();
            switch (code) {
            case X_REGION: {
                ParseCtx ctx(this);
                bool succ = declareRegion(&ctx);
                ASSERT0(succ || getErrorMsgList().get_elem_count() > 0);
                DUMMYUSE(succ);
                break;
            }
            default:
                error(tok, "miss region declaration at top level");
                //Still keep parsing, skip current error status in
                //order to catch more errors.
            }
            break;
        }
        default:
            //Still keep parsing, skip current error status in
            //order to catch more errors.
            error(tok, "miss region declaration at top level");
        }

        if (isTooManyError()) {
            return false;
        }
    }
END:
    END_TIMER(t, "IR Parser");
    bool parse_succ = getErrorMsgList().get_elem_count() == 0;
    if (parse_succ && g_dump_opt.isDumpAfterPass() &&
        g_dump_opt.isDumpIRParser()) {
        dump();
    }
    return parse_succ;
}
//END IRParser

} //namespace xoc
