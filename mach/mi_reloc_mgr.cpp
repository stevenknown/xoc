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

static bool hasLocalVar(mach::MInst const* mi)
{
    ASSERT0(mi);
    return (mi->hasVar() && MI_var(mi) != nullptr &&
            MI_var(mi)->is_local());
}

//
//START MIRelocMgr
//
MIRelocMgr::MIRelocMgr(Region * rg, MInstMgr * imgr, TMWORD align) :
    m_rg(rg), m_mimgr(imgr), m_code_align(align), m_data_align(align)
{
    m_tm = m_rg->getTypeMgr();
    IRRelocMgr * ir_reloc_mgr = (IRRelocMgr*)m_rg->getPassMgr()->
        registerPass(PASS_IRRELOC);
    if (ir_reloc_mgr != nullptr && ir_reloc_mgr->getVar2Offset() != nullptr) {
        m_has_ir_reloc = true;
        m_var2offset = ir_reloc_mgr->getVar2Offset();
        return;
    }
    m_has_ir_reloc = false;
    m_var2offset = new Var2OffsetMgr(rg);
}


MIRelocMgr::~MIRelocMgr()
{
    if (!m_has_ir_reloc) {
        delete m_var2offset;
        m_var2offset = nullptr;
    }
}


//A helper function to judge whether current machine instruction is aligned
//by word length of machine instruction of current architecture.
static bool isMIAlignedByWordLength(TMWORD offset, UINT length)
{
    return offset % length == 0;
}


//A helper function to compute offset of jump instruction based on PC value of
//target label and current instruction. The formula is as follows:
//   offset = (PC_target_label - PC_jump_instruction) / SIZE_instruction;
//Note that, PC values must be aligned by size of instructions.
TMWORD MIRelocMgr::computeJumpOff(MInstMgr * mimgr,
    Label2Offset const& lab2off, MInst const* mi)
{
    TMWORD label_pc = lab2off.get(MI_lab(mi));
    TMWORD inst_pc = MI_pc(mi);
    TMWORD inst_size = mi->getWordBufLen();

    ASSERT0(isMIAlignedByWordLength(label_pc, (UINT)inst_size));
    ASSERT0(isMIAlignedByWordLength(inst_pc, (UINT)inst_size));
    ASSERT0(isMIAlignedByWordLength(label_pc - inst_pc, (UINT)inst_size));

    //Note that, For some architectures, it is inconsistent whether the
    //distance between the target label and the current jump instruction needs
    //to be subtracted by 1.
    ASSERT0(lab2off.find(MI_lab(mi)));
    INT64 val = (INT64)(label_pc - inst_pc) / (UINT)inst_size -
        isDistanceNeedSubOne();
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
                Sym const* sym = LABELINFO_pragma(MI_lab(mi));
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
            ASSERT0(MI_lab(mi));
            TMWORD m_jump_offset = computeJumpOff(m_mimgr, lab2off, mi);
            setValueViaMICode(mi, m_jump_offset);
            m_jump_offset_map.set(mi, m_jump_offset);
        }

        if (m_mimgr->isCall(mi)) {
            m_var2offset->resetArgSpaceOffset();
        }

        if (hasLocalVar(mi)) {
            TMWORD var_offset = (TMWORD)m_var2offset->
                computeVarOffset(MI_var(mi));

            //[BUG_FIX] The offset of the memory access instruction needs to be
            //aligned upward according to the instruction type.
            var_offset = (TMWORD)xcom::ceil_align(
                var_offset, getMInstAlign(mi->getCode()));
            setValueViaMICode(mi, var_offset);
        }

        offset = (TMWORD)xcom::ceil_align(offset, getCodeAlign());
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
            ASSERT0(MI_lab(mi));
            lab2off.set(MI_lab(mi), offset);
            continue;
        }
        setCodeAlign(mi->getWordBufLen());
        offset = (TMWORD)xcom::ceil_align(offset, getCodeAlign());
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
