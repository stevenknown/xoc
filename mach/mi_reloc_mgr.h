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

author: Su Zhenyu
@*/
#ifndef _MI_RELOC_H_
#define _MI_RELOC_H_

namespace xoc { class Var2Offset; }
namespace xoc { class Var2OffsetMgr; }

namespace mach {

typedef xcom::TMap<xoc::LabelInfo const*, TMWORD> Label2Offset;
typedef xcom::TMap<MInst *, TMWORD> MI2JumpOffset;

//This class is used to perform relocation for machine instructions.
class MIRelocMgr {
    COPY_CONSTRUCTOR(MIRelocMgr);
protected:
    Region * m_rg;
    TypeMgr * m_tm;
    MInstMgr * m_mimgr;
    Var2OffsetMgr * m_var2offset;
    TMWORD m_code_align;
    TMWORD m_data_align;
    MI2JumpOffset m_jump_offset_map;
    bool m_has_ir_reloc;
protected:
    void computeCodeOffset(MOD MIList & milst,
                           OUT Label2Offset & lab2off);

    //Compute distance between target label and current jump instruction.
    TMWORD computeJumpOff(MInstMgr * mimgr, Label2Offset const& lab2off,
                          MInst const* mi);

    //Whether the distance between target label and current jump instruction
    //need to be subtracted by 1.
    virtual bool const isDistanceNeedSubOne() const
    { ASSERTN(0, ("Target Dependent Code")); return true; }

    //Whether current jump offset is valid for machine instruction.
    virtual bool const jumpOffIsValid(INT64 val, MInst const*)
    { DUMMYUSE(val);ASSERTN(0, ("Target Dependent Code")); return false; }
public:
    MIRelocMgr(Region * rg, MInstMgr * imgr, TMWORD align);
    virtual ~MIRelocMgr();

    void computeDataOffset(MOD MIList & milst,
                           Label2Offset const& lab2off);

    //Get jump offset of br inst.
    TMWORD getJumpOffset(MInst const* mi) const;

    TMWORD getCodeAlign() const { return m_code_align; }
    TMWORD getDataAlign() const { return m_data_align; }

    MInstMgr * getMIMgr() const { return m_mimgr; }

    //Get the alignment value required by the machine instruction.
    virtual UINT getMInstAlign(MI_CODE c) const
    { DUMMYUSE(c); ASSERTN(0, ("Target Dependent Code")); return 0; }

    //GCOVR_EXCL_START
    //Get the offset of the PC for the instruction.
    virtual UINT getMInstPcOffset(
        MIList & milst, MIListIter it, MInst *) const
    {
        DUMMYUSE(milst);
        DUMMYUSE(it);
        ASSERTN(0, ("Target Dependent Code"));
        return 0;
    }
    //GCOVR_EXCL_STOP

    //Set the data or code byte offset according to mi code.
    virtual void setValueViaMICode(OUT MInst * mi, TMWORD offset)
    { DUMMYUSE(mi && offset); ASSERTN(0, ("Target Dependent Code")); }

    //Set the code alignment.
    //The byte offset of each Machine Instruction will aligned in 'align'.
    void setCodeAlign(TMWORD align) { m_code_align = align; }

    //Set the data alignment.
    //The byte offset of each variable will aligned in 'align'.
    void setDataAlign(TMWORD align) { m_data_align = align; }

    void perform(MOD MIList & milst);
};

} //namespace

#endif
