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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

IRDesc * getIRDesc()
{
    //NOTE:The reason for declaring g_ir_desc as a function-static variable
    //is to avoid the uncertain initialization order of global static
    //variables that placed in different files.
    static IRDesc g_ir_desc[] = {
    { IR_UNDEF, "undef", 0x0, 0,
      0, //size of class object
      IRDescFlag(1, (UINT)IRC_MAIN_ATTR_PLACEHOLDER_POS), //attr
      dumpUndef,
      NO_VERIFY_FUNC,
      IRFieldAccTab(), },

    { IR_CONST, "const", CConst::kid_map, CConst::kid_num,
      sizeof(CConst),
      IRDescFlag(1, (UINT)IRC_IS_LEAF_POS),
      dumpConst,
      verifyConst,
      IRFieldAccTab(), },

    { IR_ID, "id", CId::kid_map, CId::kid_num,
      sizeof(CId),
      IRDescFlagSeg(IRC_HAS_IDINFO|IRC_IS_MEM_REF|IRC_IS_LEAF|
          IRC_IS_NON_PR_MEMREF|IRC_IS_MEM_OPND|IRC_HAS_DU),
      dumpGeneral,
      verifyGeneral,
      IRFieldAccTab(NUM_OF_ACC_INFO(CId::accinfo), CId::accinfo), },

    { IR_LD, "ld", CLd::kid_map, CLd::kid_num,
      sizeof(CLd),
      IRDescFlagSeg(IRC_HAS_IDINFO|IRC_IS_MEM_REF|IRC_IS_MEM_OPND|IRC_IS_LEAF|
          IRC_IS_NON_PR_MEMREF|IRC_HAS_DU|IRC_HAS_OFFSET|IRC_IS_DIRECT_MEM_OP|
          IRC_HAS_STORAGE_SPACE|IRC_HAS_VOLATILE),
      dumpGeneral,
      verifyLD,
      IRFieldAccTab(NUM_OF_ACC_INFO(CLd::accinfo), CLd::accinfo), },

    { IR_ILD, "ild", CILd::kid_map, CILd::kid_num,
      sizeof(CILd),
      IRDescFlagSeg(IRC_IS_MEM_REF|IRC_IS_MEM_OPND|
          IRC_IS_NON_PR_MEMREF|IRC_HAS_DU|IRC_HAS_OFFSET|IRC_IS_INDIRECT_MEM_OP|
          IRC_HAS_STORAGE_SPACE|IRC_HAS_VOLATILE),
      dumpGeneral,
      verifyILd,
      IRFieldAccTab(NUM_OF_ACC_INFO(CILd::accinfo), CILd::accinfo), },

    { IR_PR, "pr", CPr::kid_map, CPr::kid_num,
      sizeof(CPr),
      IRDescFlagSeg(IRC_IS_MEM_REF|IRC_IS_MEM_OPND|IRC_IS_LEAF|IRC_HAS_DU),
      dumpReadPR,
      verifyPR,
      IRFieldAccTab(NUM_OF_ACC_INFO(CPr::accinfo), CPr::accinfo), },

    { IR_ARRAY, "array", CArray::kid_map, CArray::kid_num,
      sizeof(CArray),
      IRDescFlagSeg(IRC_IS_MEM_REF|IRC_IS_MEM_OPND|IRC_IS_NON_PR_MEMREF|
          IRC_HAS_DU|IRC_HAS_OFFSET|IRC_IS_ARRAY_OP|IRC_HAS_STORAGE_SPACE|
          IRC_HAS_VOLATILE),
      dumpArray,
      verifyArrayOp,
      IRFieldAccTab(NUM_OF_ACC_INFO(CArray::accinfo), CArray::accinfo), },

    { IR_ST, "st", CSt::kid_map, CSt::kid_num,
      sizeof(CSt),
      IRDescFlagSeg(IRC_HAS_IDINFO|IRC_IS_STMT|IRC_IS_MEM_REF|IRC_HAS_RESULT|
          IRC_IS_STMT_IN_BB|IRC_IS_NON_PR_MEMREF|IRC_HAS_DU|IRC_HAS_OFFSET|
          IRC_IS_DIRECT_MEM_OP|IRC_HAS_RHS|IRC_HAS_STORAGE_SPACE|
          IRC_HAS_VOLATILE),
      dumpGeneral,
      verifySt,
      IRFieldAccTab(NUM_OF_ACC_INFO(CSt::accinfo), CSt::accinfo), },

    { IR_STPR, "stpr", CStpr::kid_map, CStpr::kid_num,
      sizeof(CStpr),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_MEM_REF|IRC_HAS_RESULT|
          IRC_IS_STMT_IN_BB|IRC_HAS_DU|IRC_IS_WRITE_PR|IRC_IS_WRITE_WHOLE_PR|
          IRC_HAS_RHS),
      dumpWritePR,
      verifyStpr,
      IRFieldAccTab(NUM_OF_ACC_INFO(CStpr::accinfo), CStpr::accinfo), },

    { IR_STARRAY, "starray", CStArray::kid_map, CStArray::kid_num,
      sizeof(CStArray),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_MEM_REF|IRC_HAS_RESULT|
          IRC_IS_STMT_IN_BB|IRC_IS_NON_PR_MEMREF|IRC_HAS_DU|IRC_HAS_OFFSET|
          IRC_IS_ARRAY_OP|IRC_HAS_RHS|IRC_HAS_STORAGE_SPACE|IRC_HAS_VOLATILE),
      dumpStArray,
      verifyArrayOp,
      IRFieldAccTab(NUM_OF_ACC_INFO(CStArray::accinfo), CStArray::accinfo), },

    { IR_IST, "ist", CISt::kid_map, CISt::kid_num,
      sizeof(CISt),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_MEM_REF|IRC_HAS_RESULT|
          IRC_IS_STMT_IN_BB|IRC_IS_NON_PR_MEMREF|IRC_HAS_DU|IRC_HAS_OFFSET|
          IRC_IS_INDIRECT_MEM_OP|IRC_HAS_RHS|IRC_HAS_STORAGE_SPACE|
          IRC_HAS_VOLATILE),
      dumpGeneral,
      verifyISt,
      IRFieldAccTab(NUM_OF_ACC_INFO(CISt::accinfo), CISt::accinfo), },

    { IR_SETELEM, "setelem", CSetElem::kid_map, CSetElem::kid_num,
      sizeof(CSetElem),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_MEM_REF|IRC_HAS_RESULT|
          IRC_IS_STMT_IN_BB|IRC_HAS_DU|IRC_IS_WRITE_PR),
      dumpWritePR,
      verifySetElem,
      IRFieldAccTab(NUM_OF_ACC_INFO(CSetElem::accinfo), CSetElem::accinfo), },

    { IR_GETELEM, "getelem", CGetElem::kid_map, CGetElem::kid_num,
      sizeof(CGetElem),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_MEM_REF|IRC_HAS_RESULT|
          IRC_IS_STMT_IN_BB|IRC_HAS_DU|IRC_IS_WRITE_PR|IRC_IS_WRITE_WHOLE_PR),
      dumpWritePR,
      verifyGetElem,
      IRFieldAccTab(NUM_OF_ACC_INFO(CGetElem::accinfo), CGetElem::accinfo), },

    //CALL might not def PR if there is not return value.
    { IR_CALL, "call", CCall::kid_map, CCall::kid_num,
      sizeof(CCall),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_MEM_REF|IRC_HAS_RESULT|
          IRC_HAS_IDINFO|IRC_IS_STMT_IN_BB|IRC_HAS_DU|IRC_HAS_STORAGE_SPACE),
      dumpCallStmt,
      verifyCall,
      IRFieldAccTab(NUM_OF_ACC_INFO(CCall::accinfo), CCall::accinfo), },

    { IR_ICALL, "icall", CICall::kid_map, CICall::kid_num,
      sizeof(CICall),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_MEM_REF|IRC_IS_STMT_IN_BB|IRC_HAS_DU|
          //ICALL might not define PR if there is not return value.
          IRC_HAS_RESULT),
      dumpCallStmt,
      verifyICall,
      IRFieldAccTab(NUM_OF_ACC_INFO(CICall::accinfo), CICall::accinfo), },

    { IR_LDA, "lda", CLda::kid_map, CLda::kid_num,
      sizeof(CLda),
      IRDescFlagSeg(IRC_HAS_IDINFO|IRC_IS_LEAF|IRC_HAS_OFFSET|
          IRC_HAS_STORAGE_SPACE),
      dumpLda,
      verifyLDA,
      IRFieldAccTab(NUM_OF_ACC_INFO(CLda::accinfo), CLda::accinfo), },

    { IR_ADD, "add", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_ASSOCIATIVE|IRC_IS_COMMUTATIVE),
      dumpBinAndUna,
      verifyADD,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_SUB, "sub", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_ASSOCIATIVE),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_MUL, "mul", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_ASSOCIATIVE|IRC_IS_COMMUTATIVE),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_DIV, "div", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_REM, "rem", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_MOD, "mod", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_LAND, "land", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_LOGICAL),
      dumpBinAndUna,
      verifyCompare,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_LOR, "lor", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_LOGICAL),
      dumpBinAndUna,
      verifyCompare,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_BAND, "band", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_ASSOCIATIVE|IRC_IS_COMMUTATIVE),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_BOR, "bor", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_ASSOCIATIVE|IRC_IS_COMMUTATIVE),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_XOR, "xor", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_ASSOCIATIVE|IRC_IS_COMMUTATIVE),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_ASR, "asr", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyShift,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_LSR, "lsr", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyShift,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_LSL, "lsl", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyShift,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_LT, "lt", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_RELATION),
      dumpBinAndUna,
      verifyCompare,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_LE, "le", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_RELATION),
      dumpBinAndUna,
      verifyCompare,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_GT, "gt", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_RELATION),
      dumpBinAndUna,
      verifyCompare,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_GE, "ge", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_RELATION),
      dumpBinAndUna,
      verifyCompare,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_EQ, "eq", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_ASSOCIATIVE|IRC_IS_COMMUTATIVE|
          IRC_IS_RELATION),
      dumpBinAndUna,
      verifyCompare,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_NE, "ne", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN|IRC_IS_ASSOCIATIVE|IRC_IS_COMMUTATIVE|
          IRC_IS_RELATION),
      dumpBinAndUna,
      verifyCompare,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_BNOT, "bnot", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_LNOT, "lnot", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA|IRC_IS_LOGICAL),
      dumpBinAndUna,
      verifyLNOT,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_NEG, "neg", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_CVT, "cvt", CCvt::kid_map, CCvt::kid_num,
      sizeof(CCvt),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyCVT,
      IRFieldAccTab(NUM_OF_ACC_INFO(CCvt::accinfo), CCvt::accinfo), },

    { IR_GOTO, "goto", CGoto::kid_map, CGoto::kid_num,
      sizeof(CGoto),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB|IRC_IS_UNCONDITIONAL_BR),
      dumpBranch,
      verifyGOTO,
      IRFieldAccTab(NUM_OF_ACC_INFO(CGoto::accinfo), CGoto::accinfo), },

    { IR_IGOTO, "igoto", CIGoto::kid_map, CIGoto::kid_num,
      sizeof(CIGoto),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB|IRC_IS_UNCONDITIONAL_BR|
          IRC_HAS_CASE_LIST),
      dumpGeneral,
      verifyIGOTO,
      IRFieldAccTab(NUM_OF_ACC_INFO(CIGoto::accinfo), CIGoto::accinfo), },

    { IR_DO_WHILE, "dowhile", CDoWhile::kid_map, CDoWhile::kid_num,
      sizeof(CDoWhile),
      IRDescFlagSeg(IRC_IS_STMT|IRC_HAS_JUDGE_TARGET),
      dumpDoWhile,
      verifyLoopCFS,
      IRFieldAccTab(NUM_OF_ACC_INFO(CDoWhile::accinfo), CDoWhile::accinfo), },

    { IR_WHILE_DO, "whiledo", CWhileDo::kid_map, CWhileDo::kid_num,
      sizeof(CWhileDo),
      IRDescFlagSeg(IRC_IS_STMT|IRC_HAS_JUDGE_TARGET),
      dumpWhileDo,
      verifyLoopCFS,
      IRFieldAccTab(NUM_OF_ACC_INFO(CWhileDo::accinfo), CWhileDo::accinfo), },

    { IR_DO_LOOP, "doloop", CDoLoop::kid_map, CDoLoop::kid_num,
      sizeof(CDoLoop),
      IRDescFlagSeg(IRC_IS_STMT|IRC_HAS_JUDGE_TARGET),
      dumpDoLoop,
      verifyLoopCFS,
      IRFieldAccTab(NUM_OF_ACC_INFO(CDoLoop::accinfo), CDoLoop::accinfo), },

    { IR_IF, "if", CIf::kid_map, CIf::kid_num,
      sizeof(CIf),
      IRDescFlagSeg(IRC_IS_STMT|IRC_HAS_JUDGE_TARGET),
      dumpIf,
      verifyIF,
      IRFieldAccTab(NUM_OF_ACC_INFO(CIf::accinfo), CIf::accinfo), },

    { IR_LABEL, "label", CLab::kid_map, CLab::kid_num,
      sizeof(CLab),
      IRDescFlagSeg(IRC_IS_STMT),
      dumpLabel,
      verifyNothing,
      IRFieldAccTab(NUM_OF_ACC_INFO(CLab::accinfo), CLab::accinfo), },

    { IR_SWITCH, "switch", CSwitch::kid_map, CSwitch::kid_num,
      sizeof(CSwitch),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB|IRC_HAS_CASE_LIST),
      dumpSWITCH,
      verifySWITCH,
      IRFieldAccTab(NUM_OF_ACC_INFO(CSwitch::accinfo), CSwitch::accinfo), },

    { IR_DUMMYUSE, "dummyuse", CDummyUse::kid_map, CDummyUse::kid_num,
      sizeof(CDummyUse),
      IRDescFlagSeg(0),
      dumpGeneral,
      verifyGeneral,
      IRFieldAccTab(NUM_OF_ACC_INFO(CDummyUse::accinfo), CDummyUse::accinfo), },

    { IR_CASE, "case", CCase::kid_map, CCase::kid_num,
      sizeof(CCase),
      IRDescFlagSeg(0),
      dumpCase,
      verifyCase,
      IRFieldAccTab(NUM_OF_ACC_INFO(CCase::accinfo), CCase::accinfo), },

    { IR_TRUEBR, "truebr", CTruebr::kid_map, CTruebr::kid_num,
      sizeof(CTruebr),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB|IRC_IS_CONDITIONAL_BR|
          IRC_HAS_JUDGE_TARGET),
      dumpBranch,
      verifyBranch,
      IRFieldAccTab(NUM_OF_ACC_INFO(CTruebr::accinfo), CTruebr::accinfo), },

    { IR_FALSEBR, "falsebr", CFalsebr::kid_map, CFalsebr::kid_num,
      sizeof(CFalsebr),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB|IRC_IS_CONDITIONAL_BR|
          IRC_HAS_JUDGE_TARGET),
      dumpBranch,
      verifyBranch,
      IRFieldAccTab(NUM_OF_ACC_INFO(CFalsebr::accinfo), CFalsebr::accinfo), },

    { IR_RETURN, "return", CRet::kid_map, CRet::kid_num,
      sizeof(CRet),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB),
      dumpReturn,
      verifyReturn,
      IRFieldAccTab(NUM_OF_ACC_INFO(CRet::accinfo), CRet::accinfo), },

    { IR_SELECT, "select", CSelect::kid_map, CSelect::kid_num,
      sizeof(CSelect),
      IRDescFlagSeg(IRC_HAS_JUDGE_TARGET),
      dumpSelect,
      verifySelect,
      IRFieldAccTab(NUM_OF_ACC_INFO(CSelect::accinfo), CSelect::accinfo), },

    { IR_BREAK, "break", CBreak::kid_map, CBreak::kid_num,
      sizeof(CBreak),
      IRDescFlagSeg(IRC_IS_STMT),
      dumpGeneralNoType,
      verifyNothing,
      IRFieldAccTab(), },

    { IR_CONTINUE, "continue", CContinue::kid_map, CContinue::kid_num,
      sizeof(CContinue),
      IRDescFlagSeg(IRC_IS_STMT),
      dumpGeneralNoType,
      verifyNothing,
      IRFieldAccTab(), },

    { IR_ALLOCA, "alloca", CAlloca::kid_map, CAlloca::kid_num,
      sizeof(CAlloca),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpAlloca,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CAlloca::accinfo), CAlloca::accinfo), },

    { IR_POW, "pow", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_NROOT, "nroot", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_LOG, "log", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_EXPONENT, "exponent", CBin::kid_map, CBin::kid_num,
      sizeof(CBin),
      IRDescFlagSeg(IRC_IS_BIN),
      dumpBinAndUna,
      verifyBin,
      IRFieldAccTab(NUM_OF_ACC_INFO(CBin::accinfo), CBin::accinfo), },

    { IR_ABS, "abs", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_SIN, "sin", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_COS, "cos", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_TAN, "tan", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_ASIN, "asin", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_ACOS, "acos", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_ATAN, "atan", CUna::kid_map, CUna::kid_num,
      sizeof(CUna),
      IRDescFlagSeg(IRC_IS_UNA),
      dumpBinAndUna,
      verifyUna,
      IRFieldAccTab(NUM_OF_ACC_INFO(CUna::accinfo), CUna::accinfo), },

    { IR_PHI, "phi", CPhi::kid_map, CPhi::kid_num,
      sizeof(CPhi),
      IRDescFlagSeg(IRC_IS_STMT|IRC_HAS_RESULT|IRC_IS_MEM_REF|
          IRC_IS_STMT_IN_BB|IRC_HAS_DU|IRC_IS_WRITE_PR|IRC_IS_WRITE_WHOLE_PR),
      dumpPhi,
      verifyPhi,
      IRFieldAccTab(NUM_OF_ACC_INFO(CPhi::accinfo), CPhi::accinfo), },

    { IR_REGION, "region", CRegion::kid_map, CRegion::kid_num,
      sizeof(CRegion),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB),
      dumpRegion,
      verifyNothing,
      IRFieldAccTab(NUM_OF_ACC_INFO(CRegion::accinfo), CRegion::accinfo), },

    { IR_CFI_DEF_CFA, "cfi_def_cfa", CCFIDefCfa::kid_map, CCFIDefCfa::kid_num,
      sizeof(CCFIDefCfa),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB),
      dumpCFIDefCfa,
      verifyNothing,
      IRFieldAccTab(NUM_OF_ACC_INFO(CCFIDefCfa::accinfo),
                    CCFIDefCfa::accinfo), },

    { IR_CFI_SAME_VALUE, "cfi_same_value",
      CCFISameValue::kid_map, CCFISameValue::kid_num,
      sizeof(CCFISameValue),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB),
      dumpCFISameValue,
      verifyNothing,
      IRFieldAccTab(NUM_OF_ACC_INFO(CCFISameValue::accinfo),
                    CCFISameValue::accinfo), },

    { IR_CFI_OFFSET, "cfi_offset", CCFIOffset::kid_map, CCFIOffset::kid_num,
      sizeof(CCFIOffset),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB),
      dumpCFIOffset,
      verifyNothing,
      IRFieldAccTab(NUM_OF_ACC_INFO(CCFIOffset::accinfo),
                    CCFIOffset::accinfo), },

    { IR_CFI_RESTORE, "cfi_restore",
      CCFIRestore::kid_map, CCFIRestore::kid_num,
      sizeof(CCFIRestore),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB),
      dumpCFIRestore,
      verifyNothing,
      IRFieldAccTab(NUM_OF_ACC_INFO(CCFIRestore::accinfo),
                    CCFIRestore::accinfo), },

    { IR_CFI_DEF_CFA_OFFSET,
      "cfi_def_cfa_offset", CCFIDefCfaOffset::kid_map,
      CCFIDefCfaOffset::kid_num,
      sizeof(CCFIDefCfaOffset),
      IRDescFlagSeg(IRC_IS_STMT|IRC_IS_STMT_IN_BB),
      dumpCFIDefCfaOffst,
      verifyNothing,
      IRFieldAccTab(NUM_OF_ACC_INFO(CCFIDefCfaOffset::accinfo),
                    CCFIDefCfaOffset::accinfo), },

    #include "ir_desc_ext.impl"

    { IR_CODE_NUM, "LAST IR CODE", 0x0, 0, //kid_map, kid_num
      0, //size
      IRDescFlagSeg(0),
      NO_DUMP_FUNC,
      NO_VERIFY_FUNC,
      IRFieldAccTab(), },
    };
    return g_ir_desc;
}


RoundDesc const g_round_desc[] = {
    { ROUND_UNDEF, "undef" },
    { ROUND_DOWN, "down" },
    { ROUND_UP, "up" },
    { ROUND_TOWARDS_ZERO, "twards_zero" },
    { ROUND_RMM, "rmm" },
    { ROUND_AWAY_FROM_ZERO, "awayfromzero" },
    { ROUND_TO_NEAREST_INTEGER, "tonearestinteger" },
    { ROUND_HALF_UP, "halfup" },
    { ROUND_HALF_DOWN, "halfdown" },
    { ROUND_HALF_TOWARDS_ZERO, "halftowardszero" },
    { ROUND_HALF_AWAY_FROM_ZERO, "halfawayfromzero" },
    { ROUND_HALF_TO_EVEN, "halftoeven" },
    { ROUND_HALF_TO_ODD, "halftoodd" },
};

static bool checkIRFieldAccTab()
{
    for (UINT i = IR_UNDEF + 1; i < IR_CODE_NUM; i++) {
        IRDesc const& desc = getIRDesc()[i];
        UINT accinfo_count = 0;
        IRFieldAccTab const& accinfo_tab = desc.field_acc_tab;
        for (UINT k = 0; k < IR_ACC_KIND_NUM; k++) {
            if (accinfo_tab.getAccFunc((IR_ACC_KIND)k) != nullptr) {
                ASSERT0(accinfo_tab.getAccKind((IR_ACC_KIND)k) ==
                        (IR_ACC_KIND)k);
                accinfo_count++;
            }
        }
        ASSERT0(accinfo_count == accinfo_tab.getAccInfoNum());
    }
    return true;
}


//The function only invoked at debug mode.
bool checkIRDesc()
{
    UINT sz = (UINT)(1 << IR_CODE_BIT_SIZE);
    ASSERTN(IR_CODE_NUM <= sz, ("code field is too small"));
    DUMMYUSE(sz);
    //NOTE: Each IRDesc object contains an IR_CODE field that serves as an
    //anchor to ensure the elements in g_ir_desc array correspond to the
    //correct IRDesc for each IR_CODE.
    for (UINT i = IR_UNDEF; i < IR_CODE_NUM; i++) {
        ASSERT0(i == (UINT)IRDES_code(i));
    }
    checkIRFieldAccTab();
    return true;
}


//The function only invoked at debug mode.
bool checkRoundDesc()
{
    for (UINT i = ROUND_UNDEF; i < ROUND_TYPE_NUM; i++) {
        ASSERT0(i == (UINT)ROUNDDESC_type(g_round_desc[i]));
    }
    UINT descnum = sizeof(g_round_desc) / sizeof(g_round_desc[0]);
    ASSERTN_DUMMYUSE(descnum == ROUND_TYPE_NUM, ("miss RoundDesc declaration"));
    return true;
}


//
//START IRDesc
//
bool IRDesc::mustExist(IR_CODE irc, UINT kididx)
{
    return IRDES_kid_map(irc).have(kididx);
}
//END IRDesc


//
//START IRDescFlag
//
IRDescFlag::IRDescFlag(UINT flag_pos_num, ...)
{
    UINT num = 0;
    va_list ptr;
    va_start(ptr, flag_pos_num);

    //Since the enum-type IRC_ATTR might be promoted to different integer
    //type depending on the different compiler, we use UINT type as the POD
    //type in variable-parameter-passing.
    UINT irc_attr_pos = (UINT)va_arg(ptr, UINT);
    while (num < flag_pos_num) {
        ASSERT0(irc_attr_pos <= (UINT)IRC_ATTR_LAST_POS);
        set(irc_attr_pos);
        num++;
        irc_attr_pos = (UINT)va_arg(ptr, UINT);
    }
    va_end(ptr);
}
//END IRDescFlag


//
//START IRFieldAccTab
//
IRFieldAccTab::IRFieldAccTab(UINT num, AccInfo const accinfo_arr[])
{
    ASSERT0(num < IR_ACC_KIND_NUM);
    ::memset(this, 0, sizeof(IRFieldAccTab));
    m_acc_info_num = num;
    for (UINT i = 0; i < num; i++) {
        AccInfo const& accinfo = accinfo_arr[i];
        m_acc_info_tab[accinfo.acc_kind] = accinfo;
    }
}


void * IRFieldAccTab::getAccFunc(IR_ACC_KIND kind) const
{
    ASSERT0(kind < IR_ACC_KIND_NUM);
    return m_acc_info_tab[kind].acc_func;
}


IR_ACC_KIND IRFieldAccTab::getAccKind(IR_ACC_KIND kind) const
{
    ASSERT0(kind < IR_ACC_KIND_NUM);
    return m_acc_info_tab[kind].acc_kind;
}
//END IRFieldAccTab

} //namespace xoc
