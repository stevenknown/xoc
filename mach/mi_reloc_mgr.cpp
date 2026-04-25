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
#include "machinc.h"
#include "../elf/elfinc.h"
#include "../opt/comopt.h"

namespace mach {

//
//START MIRelocMgr
//
MIRelocMgr::MIRelocMgr(Region * rg, MInstMgr * imgr, TMWORD align) :
    m_rg(rg), m_mimgr(imgr), m_code_align(align), m_data_align(align)
{
    m_tm = m_rg->getTypeMgr();
    VarRelocMgr * var_reloc_mgr =
        (VarRelocMgr*)rg->getPassMgr()->registerPass(PASS_VARRELOC);
    m_var2offset_mgr = var_reloc_mgr->getVar2OffsetMgr();
    ASSERT0(m_var2offset_mgr);
}


//A helper function to compute offset of jump instruction based on PC value of
//target label and current instruction. The formula is as follows:
//   offset = (PC_target_label - PC_jump_instruction) / SIZE_instruction;
//Note that, PC values must be aligned by size of instructions.
TMWORD MIRelocMgr::computeJumpOff(MInstMgr * mimgr,
    Label2Offset const& lab2off, MInst const* mi)
{
    TMWORD label_pc = lab2off.get(LABMI_lab(mi));
    TMWORD inst_pc = MI_pc(mi);
    TMWORD inst_size  = mi->getWordBufLen();

    //Note that, For some architectures, it is inconsistent whether the
    //distance between the target label and the current jump instruction needs
    //to be subtracted by 1.
    ASSERT0(lab2off.find(LABMI_lab(mi)));
    INT64 val = (INT64)(label_pc - inst_pc) -
        isDistanceNeedSubOne() * (UINT)inst_size;
    ASSERT0(jumpOffIsValid(val, mi));
    return (TMWORD)val;
}


void MIRelocMgr::computeDataOffset(MOD MIList & milst,
                                   Label2Offset const& lab2off)
{
    MIListIter it;
    TMWORD offset = 0;

    xoc::MCDwarfMgr * dm = nullptr;
    if (xoc::g_debug) {
        dm = m_rg->getRegionMgr()->getDwarfMgr();
        ASSERT0(dm);
    }

    for (MInst * mi = milst.get_head(&it);
         mi != nullptr; mi = milst.get_next(&it)) {
        if (m_mimgr->isLabel(mi)) {
            if (xoc::g_debug) {
                //We need to update the pc of the MCSymbol for the function.
                Sym const* sym = LABELINFO_pragma(LABMI_lab(mi));
                ASSERT0(sym);
                dm->setFuncLabelOff((UINT)offset, sym);
            }
            continue;
        }
        //The CFI instruction "pc" maintains the pc of the previous
        //chip instruction.
        if (xoc::g_debug && MInstMgr::isCFIInstruction(mi)) {
            MI_pc(mi) = offset;
            continue;
        }

        if (mi->hasLab()) {
            ASSERT0(LABMI_lab(mi));
            TMWORD m_jump_offset = computeJumpOff(m_mimgr, lab2off, mi);
            setValueViaMICode(mi, m_jump_offset);
            m_jump_offset_map.set(mi, m_jump_offset);
        }

        ASSERT0(mi->getInstDesc());
        mach::MInstDesc const* midesc = mi->getInstDesc();
        MI_wordbuflen(mi) = midesc->getTotalFieldByteSize();
        setCodeAlign(mi->getWordBufLen());
        MI_pc(mi) = offset;
        offset += mi->getWordBufLen();
    }
}


TMWORD MIRelocMgr::getJumpOffset(MInst const* mi) const
{
    ASSERT0(mi && mi->hasLab());

    return m_jump_offset_map.get(const_cast<MInst *>(mi));
}


void MIRelocMgr::computeCodeOffset(MOD MIList & milst,
                                   OUT Label2Offset & lab2off)
{
    MIListIter it;
    TMWORD offset = 0;
    for (MInst * mi = milst.get_head(&it);
         mi != nullptr; mi = milst.get_next(&it)) {
        if (m_mimgr->isLabel(mi)) {
            ASSERT0(LABMI_lab(mi));
            lab2off.set(LABMI_lab(mi), offset);
            continue;
        }
        ASSERT0(mi->getInstDesc());
        mach::MInstDesc const* midesc = mi->getInstDesc();
        MI_wordbuflen(mi) = midesc->getTotalFieldByteSize();
        setCodeAlign(mi->getWordBufLen());
        MI_pc(mi) = offset;
        offset += getMInstPcOffset(milst, it, mi);
    }
}


void MIRelocMgr::perform(MOD MIList & milst)
{
    Label2Offset lab2off;
    computeCodeOffset(milst, lab2off);
    computeDataOffset(milst, lab2off);
}
//END MIRelocMgr

} //namespace
