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
#ifndef __GR_HELPER_H__
#define __GR_HELPER_H__

namespace xoc {

class BBList;

//The class represents options during GR dump.
class DumpGRCtx {
public:
    //Propagate info top down.
    //Set to true to dump string literal and ignore the \n.
    bool dump_string_in_one_line;

    //Propagate info top down.
    //Set to true to dump inner region.
    bool dump_inner_region;

    //Propagate info top down.
    //Supply CFG when dumpping PHI.
    IRCFG const* cfg;
    Region const* rg;
    TypeMgr const* tm;
    LogMgr * lm;
public:
    DumpGRCtx(Region const* r, LogMgr * tlm, bool dump_inner);
};

class GRDump {
    COPY_CONSTRUCTOR(GRDump);
protected:
    Region const* m_rg;
    RegionMgr const* m_rm;
    TypeMgr const* m_tm;
    IRCFG const* m_cfg;
    LogMgr * m_lm; //LogMgr's Buffer may modified.
protected:
    void dumpAllKids(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpExtOp(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpOffset(IR const* ir) const;
    void dumpProp(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpArrSubList(IR const* ir, UINT dn, DumpGRCtx const* ctx) const;
    void dumpConst(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpPhi(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpST(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpLD(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpILD(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpReadPR(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpID(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpBinAndUna(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpIF(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpDoWhile(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpWhileDo(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpDoLoop(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpLoopIterCFS(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpReturn(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpGoto(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpIgoto(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpLabel(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpSelect(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpLda(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpSwitch(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpCase(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpArray(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpCall(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpCondBr(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpRegion(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpUndef(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpIST(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpStArray(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpGetelem(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpSetelem(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpStpr(IR const* ir, DumpGRCtx const* ctx) const;
public:
    GRDump(Region const* rg);
    virtual ~GRDump() {}

    static CHAR const* compositeName(Sym const* n, xcom::DefFixedStrBuf & buf);

    //ctx: it can be NULL if user is not going to control the dumpping.
    //     But it must be given if 'ir' is PHI because dump PHI will using CFG.
    virtual void dumpIR(IR const* ir, DumpGRCtx const* ctx) const;
    void dumpIRList(IR const* irlist, DumpGRCtx const* ctx) const;
    void dumpBBList(BBList const* bblist, DumpGRCtx const* ctx) const;
    void dumpRegion(bool dump_inner_region) const;
};

} //namespace xoc
#endif
