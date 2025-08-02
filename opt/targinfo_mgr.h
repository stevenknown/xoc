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
#ifndef _TARGINFO_MGR_H_
#define _TARGINFO_MGR_H_

using namespace xgen;

namespace xoc {

class RegDSystem;
class SRegSet;

//The class represents register set and calling convention information for
//target machine. It is an wrapper of interfaces under target/precompile.
class TargInfoMgr {
    COPY_CONSTRUCTOR(TargInfoMgr);
protected:
    Reg m_link;
    RegionMgr const* m_rm;
    RegDSystem * m_rdsys;
protected:
    virtual RegDSystem * allocRegDSystem();

    void initRegDSystem();
    virtual void initAllocableScalar()
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initAllocableVector()
    { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initCalleeScalar() { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initCalleeVector() { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initCallerScalar() { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initCallerVector() { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initParamScalar() { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initParamVector() { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initRetvalScalar() { ASSERTN(0, ("Target Dependent Code")); }
    virtual void initRetvalVector() { ASSERTN(0, ("Target Dependent Code")); }

    //Initialize total caller-saved register set, includes all kind of
    //registers of caller.
    virtual void initCaller() { ASSERTN(0, ("Target Dependent Code")); }

    //Initialize total callee-saved register set, includes all kind of
    //registers of callee.
    virtual void initCallee() { ASSERTN(0, ("Target Dependent Code")); }

    TargInfoMgr const* self() const { return this; }
public:
    TargInfoMgr(RegionMgr const* rm) : m_rm(rm)
    { ASSERT0(rm); m_rdsys = nullptr; }
    virtual ~TargInfoMgr() { destroy(); }

    virtual void dump(Region const* rg) const;
    virtual void destroy();

    virtual REGFILE getRegFile(Reg r) const;

    //Get scalar allocable register set of different architectures.
    virtual xgen::RegSet const* getAllocableScalarRegSet() const;

    //Get vector allocable register set of different architectures.
    virtual xgen::RegSet const* getAllocableVectorRegSet() const;

    //Get scalar callee saved register set of different architectures.
    virtual xgen::RegSet const* getCalleeScalarRegSet() const;

    //Get vector callee saved register set of different architectures.
    virtual xgen::RegSet const* getCalleeVectorRegSet() const;

   //Get scalar caller saved register set of different architectures.
    virtual xgen::RegSet const* getCallerRegSet() const
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Get end scalar caller saved register of different architectures.
    virtual xgen::Reg getCallerScalarEnd() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get scalar caller saved register set of different architectures.
    virtual xgen::RegSet const* getCallerScalarRegSet() const;

    //Get start scalar caller saved register of different architectures.
    virtual xgen::Reg getCallerScalarStart() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get vector caller saved register set of different architectures.
    virtual xgen::RegSet const* getCallerVectorRegSet() const;

    //Get all registers that aliased with 'reg'.
    virtual SRegSet const* getAliasRegSet(Reg reg) const;

    //Get frame pointer register of different architectures.
    virtual xgen::Reg getFP() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get global pointer register of different architectures.
    virtual xgen::Reg getGP() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get the cycle count of load operation on chip memory.
    virtual UINT getLoadOnChipMemCycle() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the cycle count of write operation on chip memory.
    virtual UINT getStoreOnChipMemCycle() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get number of registers of different architectures.
    virtual UINT const getNumOfRegister() const;

    //Get number of param passed by register.
    virtual UINT getNumOfParamByReg() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get scalar parame register set of different architectures.
    virtual xgen::RegSet const* getParamScalarRegSet() const;

    //Get start scalar parameter register of different architectures.
    virtual xgen::Reg getParamScalarStart() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get vector parameter register set of different architectures.
    virtual xgen::RegSet const* getParamVectorRegSet() const;

    //Get program counter register of different architectures.
    virtual xgen::Reg getPC() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get return address of different architectures.
    virtual xgen::Reg getRA() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get the rflag register for different architectures.
    virtual xgen::Reg getRflagRegIsTer() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get the last physical register for different architectures.
    virtual xgen::Reg getRegLast() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get scalar allocable register set of different architectures.
    virtual xgen::RegSet const* getRetvalScalarRegSet() const;

    //Get vector returned value register set of different architectures.
    virtual xgen::RegSet const* getRetvalVectorRegSet() const;

    //Get stack pointer register of different architectures.
    virtual xgen::Reg getSP() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get target address register of function call.
    //Some target machine needs a register to record the address of function
    //when calling a function because the target can not call function via
    //literal address.
    //e.g: %r0 = lda 'foo';
    //     br %r0;
    virtual xgen::Reg getTA() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get temporary register of different architectures.
    virtual xgen::Reg getTempScalar(Type const* ty) const
    {
        ASSERT0(ty->is_scalar() || ty->is_pointer() || ty->is_any());
        ASSERTN(0, ("Target Dependent Code"));
        return (xgen::Reg)REG_UNDEF;
    }
    virtual xgen::Reg getTempVector() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }

    //Get zero register of different architectures.
    Reg getZero(Type const* ty) const
    {
        ASSERT0(ty != nullptr);
        if (ty->isInt() || ty->is_any()) { return getZeroScalar(); }
        if (ty->is_fp()) { return getZeroScalarFP(); }
        if (ty->is_vector()) { return getZeroVector(); }
        ASSERTN(0, ("Target Dependent Code"));
        return (xgen::Reg)REG_UNDEF;
    }
    virtual Reg getZeroScalar() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }
    virtual Reg getZeroScalarFP() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF; }
    virtual Reg getZeroVector() const
    { ASSERTN(0, ("Target Dependent Code")); return (xgen::Reg)REG_UNDEF;}

    Reg getLink() const { return m_link; }
    RegionMgr const* getRegionMgr() const { return m_rm; }
    virtual CHAR const* getRegName(Reg r) const;
    virtual CHAR const* getRegFileName(REGFILE rf) const;
    virtual UINT getBitSize(Reg) const
    {
        ASSERTN(0, ("Target Dependent Code"));
        return WORD_LENGTH_OF_TARGET_MACHINE;
    }
    RegDSystem const* getRegDSystem() const { return m_rdsys; }

    virtual UINT getMInstCycle(MI_CODE)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual UINT getMInstExecUnit(MI_CODE)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual UINT getLoadGlobalMemoryCycle(void)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }
    virtual UINT getStoreGlobalMemoryCycle(void)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    bool isAllocable(Reg r) const
    {
        RegSet const* s = getAllocableScalarRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isCallee(Reg r) const
    {
        RegSet const* s = getCalleeScalarRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isCaller(Reg r) const
    {
        RegSet const* s = getCallerScalarRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isReturnValue(Reg r) const
    {
        RegSet const* s = getRetvalScalarRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isParam(Reg r) const
    {
        RegSet const* s = getParamScalarRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorAllocable(Reg r) const
    {
        RegSet const* s = getAllocableVectorRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorCallee(Reg r) const
    {
        RegSet const* s = getCalleeVectorRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorCaller(Reg r) const
    {
        RegSet const* s = getCallerVectorRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorReturnValue(Reg r) const
    {
        RegSet const* s = getRetvalVectorRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorParam(Reg r) const
    {
        RegSet const* s = getParamVectorRegSet();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isLink(Reg r) const { return getLink() == r; }

    //Return true if register r1 alias to r2.
    virtual bool isAlias(Reg r1, Reg r2) const;

    //Return true if register reg1 exactly cover reg2.
    //e.g: reg1 indicates 32bit physical register eax on x86, and reg2
    //indicates 8bit physical register ax, the function return true.
    virtual bool isExactCover(Reg reg1, Reg reg2) const;
    virtual void init();

    void reset();
};

} //namespace xoc
#endif
