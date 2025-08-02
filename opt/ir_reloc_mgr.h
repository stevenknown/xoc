/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _IR_RELOC_MGR_H_
#define _IR_RELOC_MGR_H_

namespace xoc {

class LinearScanRA;

class PrologueEpilogueInserter;

class Var2Offset;

class Var2OffsetMgr;

//This class is used to perform relocation for ir.
//
//For some memory access instructions such as load and store, there is a limit
//to the range of immediate that can be accessed.
//
//e.g: load.stack.u64 $0, [stackval0];
//Assume: 1. the offset of 'stackval0' in stack is 65536(0b10000000000000000).
//        2. the immediate range of load is 0~65535(0 ~ 0b1111111111111111)
//
//In this case, the machine instruction of IR_LD doesn't direct access to
//stackval0 because the offset of stackval0 is exceed the range of immediates
//that the MI can represent: 0b10000000000000000 > 0b1111111111111111.
//
//Therefore, it is necessary to use indirect memory access to load the value of
//stackval, like:
//load.stack.u64 $0, [$addr];
//$addr is the address of stackval0, which needs to be calculated first.
//
//The reason why the relocation of local variable is required in the IR stage is
//that some instructions need to be inserted to get the address of the variable.
//In order to achieve the one-to-one requirement of IR and machine instructions,
//the relocation of local variables needs to be completed in the IR stage.
class IRRelocMgr : public Pass {
    COPY_CONSTRUCTOR(IRRelocMgr);
protected:
    TypeMgr * m_tm;
    IRMgr * m_irmgr;
    LinearScanRA * m_ra;
    Var2OffsetMgr * m_var2offset;
    PrologueEpilogueInserter * m_pelog;
protected:
    void performRelocForIR(IR * ir, OUT IRList & new_ir_list);
    void performRelocForBB(IRBB * bb);
public:
    IRRelocMgr(Region * rg) : Pass(rg)
    {
        m_tm = rg->getTypeMgr();
        m_irmgr = rg->getIRMgr();
        m_pelog = (PrologueEpilogueInserter*)m_rg->getPassMgr()->
            registerPass(PASS_PROLOGUE_EPILOGUE);
        m_var2offset = new Var2OffsetMgr(rg);
        m_ra = m_pelog->getLsra();
    }

    virtual ~IRRelocMgr()
    {
        if (m_var2offset != nullptr) {
            delete m_var2offset;
            m_var2offset = nullptr;
        }
    }

    //There are two steps to construct the address of a local variable:
    //1. Load the offset into a register.
    //2. Add the offset and the base address register.
    IR * constructAddressIROfLocalVar(OUT IRList & irlist, Var const* var,
                                      HOST_INT offset);

    //In a function, there may be multiple callees, such as:
    //.func test()
    //{
    //    call test1();
    //    call test2();
    //    call test3();
    //}
    //These callees use the same argument space, and the size of the argument
    //space is determined by the maximum argument space used by all callees.
    //Therefore, at the end of each function call, you need to reset the offset
    //of the argument space to achieve the purpose of reuse.
    //However, there are some special cases, such as some architectures will use
    //external function calls to copy local variables to the argument space when
    //processing arguments, for which there is can't to reset the argument space
    //offset. This interface is used to determine the scenarios that do not need
    //to be reset.
    virtual bool canResetArgSpace(IR const* ir) const
    { return ir->isCallStmt(); }

    virtual bool dump() const;

    //Return local var and the corresponding inner_ir which has idinfo.
    Var const* getStackVar(IR * ir, OUT IR ** ir_has_var) const;

    Var2OffsetMgr * getVar2Offset() const { return m_var2offset; }

    virtual CHAR const* getPassName() const
    { return "IR Reloc Mgr"; }
    PASS_TYPE getPassType() const { return PASS_IRRELOC; }

    //Return true if the offset of var is within the immediate range of
    //the MI corresponding to the IR.
    bool isValidOffset(HOST_INT offset, Var const* var, IR * ir,
                       IR * inner_ir) const;

    //1. Calculate the offset of local variable.
    //2. Use indirect memory_access ir instead of the direct memory_access
    //   ir where the offset of variable is outside the immediate range
    //   of the MI corresponding to the IR.
    void performReloc();

    //Process IRs which the offset of variable has exceed the immediate range
    //of the MI corresponding to the IR. The irlist contains all IRs that
    //come before the current ir within the current basic block. IRs generated
    //while processing the current ir are added to the end of the irlist.
    //irlist: stores the IRs processed within the current basic block.
    //ir: the ir currently needing processing.
    //offset: the offset of the ir.
    void processIR(OUT IRList & irlist, IR * ir, HOST_INT offset);

    virtual bool perform(OptCtx & oc);

    //Generate irlist for IR_ST when the offset of var is outside the range
    //of the immediate number that the store can represent. The irlist
    //contains all IRs that come before the current ir within the current
    //basic block. IRs generated while processing the current ir are added
    //to the end of the irlist.
    //irlist: stores the IRs processed within the current basic block.
    //ir: the ir currently needing processing.
    //offset: the offset of the ir.
    //
    //e.g.: The sum of 'st1' and ST_ofst(ir) exceeds the immediate range of the
    // MI corresponding to IR_ST.
    //Before:
    //     st:u32:offset(65536):storage_space(stack) 'st1' id:15 attachinfo:Dbx
    //         $2:u32 id:14
    //After:
    //     stpr $58:u64 id:258
    //         intconst:u64 131072|0x20000 id:256
    //     stpr $58:u64 id:262
    //         add:u64 id:261
    //             $57:u64 id:260
    //             $58:u64 id:259
    //     ist:u32 id:265
    //         $58:any id:263
    //         $2:u32 id:264
    //NOTE: $58 should be a temporary register.
    //      $57 is the base address register on stack.
    void processStoreIR(OUT IRList & irlist, IR const* ir, HOST_INT offset);

    //Generate irlist for IR_LD when the offset of var is outside the range
    //of the immediate number that the load can represent. The irlist
    //contains all IRs that come before the current ir within the current
    //basic block. IRs generated while processing the current ir are added
    //to the end of the irlist.
    //irlist: stores the IRs processed within the current basic block.
    //ir: the ir currently needing processing.
    //offset: the offset of the ir.
    //
    //e.g.: The sum of 'st1' and LD_ofst(ir) exceeds the immediate range of
    //the MI of IR_LD.
    //Before:
    //     stpr $8:u32 id:42 attachinfo:Dbx
    //         ld:u32:offset(65536):storage_space(stack) 'st1' id:41
    //After:
    //     stpr $64:u64 id:277
    //         intconst:u64 131072|0x20000 id:276
    //     stpr $64:u64 id:281
    //         add:u64 id:280
    //             $63:u64 id:279
    //             $64:u64 id:278
    //     stpr $8:u32 id:284
    //         ild:u32 id:283
    //             $64:any id:282
    //NOTE: $64 should be a temporary register.
    //      $63 is the base address register on stack.
    void processLoadIR(OUT IRList & irlist, IR const* ir, HOST_INT offset);

    //Generate irlist for IR_LDA when the offset of var is outside the range
    //of the immediate number that the lda can represent. The irlist
    //contains all IRs that come before the current ir within the current
    //basic block. IRs generated while processing the current ir are added
    //to the end of the irlist.
    //irlist: stores the IRs processed within the current basic block.
    //ir: the ir currently needing processing.
    //offset: the offset of the ir.
    //
    //e.g: the offset of 'st1' exceeds the immediate range of the MI of IR_LDA.
    //
    //Before:
    //     stpr $6:u64 id:22 attachinfo:Dbx
    //         lda:*<65536> 'st1' id:21
    //After:
    //     stpr $61:u64 id:269
    //         intconst:u64 65536|0x10000 id:268
    //     stpr $6:u64 id:273
    //         add:u64 id:272
    //             $60:u64 id:271
    //             $61:u64 id:270
    //NOTE: $61 should be a temporary register.
    //      $60 is the base address register on stack.
    virtual void processLdaIR(OUT IRList & irlist, IR const* ir,
                              HOST_INT offset);

    //Verify the stack offset of the local var in IR.
    bool verifyLocalVarOffset() const;

protected:
    //Calculate the address directly using the register and offset value, no
    //other operations are needed to calculate the offset.
    virtual IR * computeAddressUseFPAndOffsetDirectly(
        PRNO tmp_prno, PRNO base_prno, HOST_INT offset)
    {
        DUMMYUSE(tmp_prno && base_prno && offset);
        ASSERTN(0, ("Target Dependent Code"));
        return nullptr;
    }

    //Determines whether the offset is outside the range of the immediate number
    //of the corresponding instruction.
    virtual bool hasOffsetExceedImmRange(
        IR const* ir, IR const* inner_ir, HOST_INT offset) const
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    //Whether the offset exceeds the representable range of the load immediate
    //operation.
    virtual bool isValidOffsetOfLoadImm(HOST_INT offset) const
    { ASSERTN(0, ("Target Dependent Code")); return true; }

    virtual void loadOffsetToRegister(PRNO prno, UINT64 val,
                                      OUT IRList & ir_list)
    { ASSERTN(0, ("Target Dependent Code")); }
};
}

#endif
