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
    IR_DUMP_IRID = 0x40, //dump IR's id.
    IR_DUMP_DWARF = 0x80, //dump all DWARF debug information.
    IR_DUMP_COMBINE =
        IR_DUMP_KID|IR_DUMP_SRC_LINE|IR_DUMP_VAR_DECL|IR_DUMP_IRID,
};

class DumpFlag : public UFlag {
public:
    DumpFlag() : UFlag(0) {}
    DumpFlag(UINT v) : UFlag(v) {}

    //Return the new flag with combining with IRID if g_dump_option
    //enabled the IRID flag.
    static DumpFlag combineIRID(UINT v)
    { return v | (g_dump_opt.isDumpIRID() ? IR_DUMP_IRID : 0); }
};

void dumpConstContent(IR const* ir, Region const* rg);

//The function dumps IR info into LogCtx of current LogMgr.
void dumpIR(IR const* ir, Region const* rg, CHAR const* attr = nullptr,
            DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE));
void dumpIR(IR const* ir, Region const* rg, MOD IRDumpCtx<> & ctx);
inline void dumpIR(IR const* ir, Region const* rg, DumpFlag dumpflag)
{
    dumpIR(ir, rg, nullptr, dumpflag);
}

//The function dumps all IR related info into LogCtx of current LogMgr.
void dumpIRCombine(IR const* ir, Region const* rg);
void dumpIRListCombine(IR const* ir, Region const* rg);

//The function dumps IR's name and id into the given buffer.
//Return the buffer address.
template <class StrBufType>
CHAR const* dumpIRName(IR const* ir, MOD StrBufType & buf)
{
    buf.sprint("%s", IRNAME(ir));
    if (g_dump_opt.isDumpIRID()) {
        buf.strcat(" id:%u", ir->id());
    }
    return buf.getBuf();
}
void dumpIRName(IR const* ir, Region const* rg);

//The function dumps IR_CODE's name and id into the given buffer.
//Return the buffer address.
template <class StrBufType>
CHAR const* dumpIRCodeName(IR_CODE code, MOD StrBufType & buf)
{
    buf.sprint("%s", IR::getIRCodeName(code));
    return buf.getBuf();
}
void dumpIRCodeName(IR_CODE code, Region const* rg);


//The function dump IR info into given buffer.
CHAR const* dumpIRToBuf(IR const* ir, Region const* rg, OUT StrBuf & outbuf,
                        DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE));

//Dump IR info with a headline.
//Dump both its kids and siblings.
void dumpIRListH(IR const* ir_list, Region const* rg,
                 CHAR const* attr = nullptr,
                 DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE));

//Dump IR info with a postfix-attribute-string.
void dumpIRList(IR const* ir_list, Region const* rg,
                CHAR const* attr = nullptr,
                DumpFlag dumpflag = DumpFlag(IR_DUMP_COMBINE));

//Dump IR list.
void dumpIRList(IRList const& ir_list, Region const* rg);

//Dump Const IR list.
void dumpIRList(ConstIRList const& ir_list, Region const* rg);

//Dump IR, and both its kids and siblings.
//ctx: pass the dump options top down.
void dumpIRList(IR const* ir_list, Region const* rg, IRDumpCtx<> & ctx);

//Dump IR, and both its kids and siblings.
//filename: dump IR list into given filename.
//ctx: optional.
void dumpIRList(
    CHAR const* filename, IR const* ir_list, Region const* rg,
    bool dump_inner_region, IRDumpCtx<> * ctx);

//Dump IR, and both its kids and siblings.
//ctx: optional.
void dumpIRList(
    IR const* ir_list, Region const* rg, bool dump_inner_region,
    IRDumpCtx<> * ctx);

void dumpLabelDecl(LabelInfo const* li, RegionMgr const* rm, bool for_gr);
void dumpLabelName(LabelInfo const* li, RegionMgr const* rm, bool for_gr);

//The class represents the default dump-function that includs a list of
//call-back functions during the BB or IR dump.
class IRDumpCustomBaseFunc {
public:
    //Dump IR customized attribute information into given buffer during
    //dumping IR.
    //NOTE:the customized attribute information is usually placed immediately
    //after IR normal information.
    //e.g: code:type customized-information id attact_info
    //NOTE2: The custom function must be virtual.
    virtual void dumpCustomAttr(
        OUT xcom::DefFixedStrBuf &, Region const*, IR const*,
        DumpFlag dumpflag) const
    {
        DUMMYUSE(dumpflag);
        //Target Dependent Code.
        ASSERT0(0);
    }
};

//The class represents the context information during IR or BB dump.
//Note there is a forward declaration of IRDumpCtx placed in ir_desc.h.
//Change the number of template-parameters should both modify the forward
//decalaration and the class definition.
template <class CustomFunc> class IRDumpCtx {
public:
    UINT dn; //indent
    DumpFlag dumpflag;
    CHAR const* attr;

    //Dump IR attribute information into given buffer during dumping given IR.
    //NOTE:the attribute information is usually placed immediately after IR
    //information.
    //e.g: code:type attribute-information id attact_info
    //NOTE: The class use class object pointer to dump customized information
    //rather than using virtual-function. The reason is that dump functions
    //always defines temparary IRDumpCtx objects via copy-constructor which
    //input argument is parent IRDumpCtx object passed in their function.
    //Thus the temparary object can not aware and inherit the
    //parent object's virtual-function-table.
    CustomFunc * custom_func;
public:
    IRDumpCtx(UINT tdn, DumpFlag const& tdumpflag, CHAR const* tattr,
              CustomFunc * cf = nullptr)
        : dn(tdn), dumpflag(tdumpflag), attr(tattr), custom_func(cf) {}
};

class DumpIRName {
    COPY_CONSTRUCTOR(DumpIRName);
    xcom::DefFixedStrBuf m_buf;
public:
    DumpIRName() {}
    CHAR const* dump(IR const* ir) { return xoc::dumpIRName(ir, m_buf); }
};

void dumpAllKids(IR const* ir, Region const* rg, UINT dn, IRDumpCtx<> & ctx);
void dumpVarDecl(IR const* ir, Region const* rg);
void dumpIdinfo(IR const* ir, Region const* rg);
void dumpStorageSpace(IR const* ir, Region const* rg);
void dumpOffset(IR const* ir, Region const* rg);
void dumpUndef(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpGeneral(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpMemRefGeneral(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpVarGeneral(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpCallStmt(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpWritePR(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpStArray(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpBranch(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpGeneralNoType(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpGeneral(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpReadPR(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpBinAndUna(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpIf(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpDoWhile(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpWhileDo(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpDoLoop(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpLabel(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpSelect(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpPhi(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpSWITCH(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpCase(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpArray(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpRegion(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpConst(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpReturn(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpLda(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpCFISameValue(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpCFIDefCfa(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpCFIOffset(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpCFIRestore(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpCFIDefCfaOffst(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);
void dumpAlloca(IR const* ir, Region const* rg, IRDumpCtx<> & ctx);

//The class provides an approach to redirect the result of normal dump
//functions to user's buffer.
//USAGE: To redirect user's dump behaviors to a given buffer, user only need
//to define a new Dump class and public from the 'DumpToBuf'. Then override
//the interface 'dump()' by implementing user's actual dump behaviors. User
//can search for the example in ir_dump.cpp.
class DumpToBuf {
protected:
    UINT m_indent; //the indent to user's dump information.
    Region const* m_rg;
    LogMgr * m_lm;
    xcom::StrBuf & m_outbuf; //records the user's dump-buffer.

    //The indent of user's dump information is set to 0 by default.
    static UINT const g_default_indent = 0;
protected:
    //The function is an interface to encapsulate the user's dump behaviors.
    //User should rewrite the function with normal dump functions without
    //considering how to put info into the buffer 'outbuf'.
    virtual void dumpUserInfo() const
    {
        //Target Dependent Code
        ASSERT0(0);
    }
    Region const* getRegion() const { return m_rg; }
public:
    DumpToBuf(Region const* rg, xcom::StrBuf & outbuf,
              UINT indent = g_default_indent);
    //The function dump user designated information to given buffer.
    //Return the buffer pointer to facilate the scenarios that the returned
    //buffer is used in parameter of a call.
    CHAR const* dump() const;
};

} //namespace xoc
#endif

#include "ir_dump_ext.h"
