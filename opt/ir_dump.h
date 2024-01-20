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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __IR_DUMP_H__
#define __IR_DUMP_H__

namespace xoc {

#define PR_TYPE_CHAR "$"
#define DUMPADDR(ir) \
  do { int x = dump_addr ? prt(rg, " 0x%p", (ir)) : 0; DUMMYUSE(x); } while (0)
#define NUM_ARRELEM(arr) (sizeof(arr)/sizeof(arr[0]))

enum IR_DUMP_FLAG {
    IR_DUMP_DEF = 0x0, //default options to dump ir
    IR_DUMP_KID = 0x1, //dump ir's kid
    IR_DUMP_SRC_LINE = 0x2, //dump source line if dbx info is valid.
    IR_DUMP_ADDR = 0x4, //dump host address of each IR
    IR_DUMP_INNER_REGION = 0x8, //dump inner region.
    IR_DUMP_VAR_DECL = 0x10, //dump variable declaration if exist that given
                             //by user.
    IR_DUMP_NO_NEWLINE = 0x20, //Do NOT dump newline
    IR_DUMP_COMBINE = IR_DUMP_KID|IR_DUMP_SRC_LINE|IR_DUMP_VAR_DECL,
};

class DumpFlag : public UFlag {
public:
    DumpFlag() : UFlag(0) {}
    DumpFlag(UINT v) : UFlag(v) {}
};

void dumpConstContent(IR const* ir, Region const* rg);

//The function dumps IR info into LogCtx of current LogMgr.
void dumpIR(IR const* ir, Region const* rg, CHAR const* attr = nullptr,
            DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE));
inline void dumpIR(IR const* ir, Region const* rg, DumpFlag dumpflag)
{
    dumpIR(ir, rg, nullptr, dumpflag);
}

//The function dumps all IR related info into LogCtx of current LogMgr.
void dumpIRCombine(IR const* ir, Region const* rg);

//The function dumps IR's name and id into the given buffer.
//Return the buffer address.
CHAR const* dumpIRName(IR const* ir, MOD StrBuf & buf);

//The function dump IR info into given buffer.
void dumpIRToBuf(IR const* ir, Region const* rg, OUT StrBuf & outbuf,
                 DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE));
void dumpIRListH(IR const* ir_list, Region const* rg, CHAR * attr = nullptr,
                 DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE));
void dumpIRList(IR const* ir_list, Region const* rg, CHAR * attr = nullptr,
                DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE));
void dumpIRList(IRList const& ir_list, Region const* rg);
void dumpLabelDecl(LabelInfo const* li, RegionMgr const* rm, bool for_gr);
void dumpLabelName(LabelInfo const* li, RegionMgr const* rm, bool for_gr);

class IRDumpCtx {
public:
    UINT dn; //indent
    DumpFlag dumpflag;
    CHAR const* attr;
};

void dumpUNDEF(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpGeneral(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpMemRefGeneral(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpVarGeneral(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpCallStmt(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpWritePR(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpSTARRAY(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpBranch(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpGeneralNoType(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpGeneral(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpReadPR(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpBinAndUna(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpIF(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpDOWHILE(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpWHILEDO(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpDOLOOP(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpLABEL(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpSELECT(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpPHI(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpSWITCH(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpCASE(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpARRAY(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpREGION(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpConst(IR const* ir, Region const* rg, IRDumpCtx & ctx);
void dumpRETURN(IR const* ir, Region const* rg, IRDumpCtx & ctx);

} //namespace xoc
#endif

#include "ir_dump_ext.h"
