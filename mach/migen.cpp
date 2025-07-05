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
#include "machinc.h"
#include "../elf/elfinc.h"

//MIGen need xoc/opt module and xgen register module.
//xgen register module is used by LSRA pass.
#include "../opt/cominc.h"
#include "../xgen/reg.h"
#include "../xgen/regfile.h"
#include "../opt/comopt.h"

namespace mach {

MIGen::MIGen(Region * rg, elf::ELFMgr * em) : m_rg(rg), m_em(em)
{
    ASSERT0(em && rg && rg->getRegionVar() && rg->getRegionVar()->get_name());
    m_pool = smpoolCreate(64, MEM_COMM);
    m_mfmgr = nullptr;
    m_mimgr = nullptr;
    m_ir2minst = nullptr;
    m_relocmgr = nullptr;
}


void MIGen::destroy()
{
    ASSERT0(m_pool);
    smpoolDelete(m_pool);
    m_pool = nullptr;
    destroyMgr();
}


void MIGen::destroyMgr()
{
    if (m_mfmgr == nullptr) { return; }
    ASSERT0(m_mimgr && m_ir2minst && m_relocmgr);
    delete m_mfmgr;
    delete m_mimgr;
    delete m_ir2minst;
    delete m_relocmgr;
    m_mfmgr = nullptr;
    m_mimgr = nullptr;
    m_ir2minst = nullptr;
    m_relocmgr = nullptr;
}


MFieldMgr * MIGen::allocMFieldMgr()
{
    return new MFieldMgr();
}


MInstMgr * MIGen::allocMInstMgr()
{
    return new MInstMgr(m_rg, m_mfmgr);
}


IR2MInst * MIGen::allocIR2MInst()
{
    return new IR2MInst(m_rg, m_mimgr, m_em);
}


MIRelocMgr * MIGen::allocMIRelocMgr()
{
    return new MIRelocMgr(m_rg, m_mimgr, getMemoryAlignment());
}


void MIGen::initMgr()
{
    if (m_mfmgr != nullptr) { return; }
    m_mfmgr = allocMFieldMgr();
    m_mimgr = allocMInstMgr();
    m_ir2minst = allocIR2MInst();
    m_relocmgr = allocMIRelocMgr();
    ASSERT0(!ELFMGR_symbol_info(m_em).find(m_rg->getRegionVar()->get_name()));
    m_em->initSymbol(m_rg->getRegionVar());
    ASSERT0(m_mfmgr);
}


void * MIGen::xmalloc(UINT size)
{
    ASSERT0(size != 0);
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset((void*)p, 0, size);
    return p;
}


//Create assemble descriptors according to each field of "mi".
//One field, one descriptor.
static void makeAssembleDesc(mach::MInst const* mi,
                             OUT AssembleBinDescVec & asdescvec)
{
    ASSERT0(mi && mi->getInstDesc());
    mach::MInstDesc const* midesc = mi->getInstDesc();

    asdescvec.clean();

    for (UINT i = 0; i < midesc->getFieldNum(); i++) {
        ASSERT0(midesc->getFieldSizeByIdx(i) <=
                sizeof(BinWord) * BITS_PER_BYTE);
        AssembleBinDesc d(midesc->getFieldSizeByIdx(i),
                          mi->getFieldValue(i));
        asdescvec.append(d);
    }
}


void MIGen::setMIBinBuf(MOD mach::MInst * mi,
                        AssembleBinDescVec const& asdescvec)
{
    ASSERT0(mi);
    UINT bytesize = asdescvec.getTotalByteSize();
    ASSERT0(bytesize != 0);
    MI_wordbuf(mi) = (BYTE *)xmalloc(bytesize);
    MI_wordbuflen(mi) = bytesize;
    xcom::AssembleBinBuf as(MI_wordbuf(mi), MI_wordbuflen(mi), asdescvec);
}


void MIGen::dump(MIList const& milst) const
{
    if (!m_rg->isLogMgrInit() || !mach::g_is_dump_migen) { return; }
    xoc::note(m_rg, "\n==---- DUMP AFTER XIR2MI (%d) '%s' ----==\n",
              m_rg->id(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    milst.dump(m_rg->getLogMgr(), *m_mimgr);
    m_rg->getLogMgr()->decIndent(2);
}


void MIGen::convertIR2MI(OUT MIList & milst, MOD IMCtx * cont)
{
    ASSERT0(cont && m_ir2minst);
    RecycMIList recycmilst(&m_ir2minst->getRecycMIListMgr());
    m_ir2minst->convertToMIList(recycmilst, cont);
    milst.move_tail(recycmilst.getList());
    if (xoc::g_dump_opt.isDumpAfterPass()) {
        dump(milst);
    }
}


void MIGen::convertMIListToCode(MIList const& milst)
{
    ASSERT0(m_rg && m_rg->getRegionVar() && m_rg->getRegionVar()->get_name());
    Var const* var = m_rg->getRegionVar();

    //The function initSymbol() has added this symbol.
    ASSERT0(m_em && ELFMGR_symbol_info(m_em).find(var->get_name()));

    //Ensure that codes of this function symbol is empty.
    ASSERT0(m_em->getSymbolCode(var->get_name()).get_elem_count() == 0);

    mach::MIListIter mi_it;
    for (mach::MInst * mi = milst.get_head(&mi_it);
         mi != nullptr; mi = milst.get_next(&mi_it)) {

        //Skip label.
        if (mi->getCode() == MI_label) { continue; }

        AssembleBinDescVec asdescvec;
        makeAssembleDesc(mi, asdescvec);
        setMIBinBuf(mi, asdescvec);

        //Fill the binary buffer of specific ELF section.
        BYTEVec & binvec = m_em->getSymbolCode(var->get_name());
        binvec.append(MI_wordbuf(mi), MI_wordbuflen(mi));
    }

    //Set size of function symbol.
    SYMINFO_size(ELFMGR_symbol_info(m_em).get(var->get_name())) =
        m_em->getSymbolCode(var->get_name()).get_elem_count();
}


void MIGen::performRelocation(MOD MIList & milst, MOD IMCtx * cont)
{
    m_relocmgr->perform(milst);
    if (xoc::g_dump_opt.isDumpAfterPass() && mach::g_is_dump_migen) {
        xoc::note(m_rg, "\n==---- DUMP AFTER RELOCATION (%d)'%s' ----==",
                  m_rg->id(), m_rg->getRegionName());
        milst.dump(m_rg->getLogMgr(), *m_mimgr);
    }
}


void MIGen::collectLinkerRelaxBrInfo(MIList const& milst)
{
    ASSERT0(m_em);
    FunctionInfo * fi = SYMINFO_func(ELFMGR_symbol_info(m_em).get(
        m_rg->getRegionVar()->get_name()));

    MIListIter it;
    for (MInst * mi = milst.get_head(&it);
         mi != nullptr; mi = milst.get_next(&it)) {
        if (m_mimgr->isLabel(mi)) { continue; }
        IR const* ir = mi->getIR();
        ASSERT0(ir);

        if (mi->hasLab()) {
            m_em->addRelaxBrInfo(Addr(MI_pc(mi)),
                Addr(m_relocmgr->getJumpOffset(mi)), fi, ir);
            continue;
        }

        //For case of register indirect jump. Refer to the declaration of
        //function MIGen::collectLinkerRelaxBrInfo.
        //Record location and funcinfo of RelaxBrInfo for indirect jump.
        RelaxBrInfo * relax_br_info = ELFMGR_relax_br_map(m_em).get(ir);
        if (relax_br_info != nullptr) {
            RELAXBRINFO_loc(relax_br_info) = MI_pc(mi);
            RELAXBRINFO_func(relax_br_info) = fi;
        }
    }
}


bool MIGen::perform()
{
    //Dump initial information if needed.
    if (xoc::g_dump_opt.isDumpBeforePass() && mach::g_is_dump_migen) {
        xoc::note(m_rg, "\n==---- DUMP MI GENERATION (%d) '%s' ----==\n",
                  m_rg->id(), m_rg->getRegionName());
        m_rg->dump(false);
    }
    START_TIMER_FMT(t, ("MI GENERATION '%s'", m_rg->getRegionName()));
    MIList milst_all, mcfi_list, milst;
    IMCtx cont;
    initMgr();

    //Convert XIR to MI code. milst_all
    //will include both MI and CFI instructions only if debugging is enabled.
    convertIR2MI(milst_all, &cont);
    ASSERT0(verifyMIListValid(milst_all));

    //Compute and set the offset for stack variables.
    performRelocation(milst_all, &cont);

    //Collect all the calling relocations.
    collectCallRelocation(milst_all);

    //Collect the RelaxBrInfo for linker relaxation.
    collectLinkerRelaxBrInfo(milst_all);

    //If debugging is enabled, process CFI instructions.
    if (g_debug) {
        //Separate CFI instructions from regular MI instructions.
        separateMIListAndCFIList(*m_mimgr, milst_all, milst, mcfi_list);

        //Convert only regular MI instructions to machine code.
        convertMIListToCode(milst);

        //Generate debug information.
        genFrameInfo(mcfi_list);
        genLineInfo(milst);
    } else {
        //Convert all MI instructions to machine code.
        //(CFI instructions will not generate code)
        convertMIListToCode(milst_all);
    }
    END_TIMER_FMT(t, ("MI GENERATION '%s'", m_rg->getRegionName()));
    return true;
}


void MIGen::separateMIListAndCFIList(MOD MInstMgr & imgr,
    MIList const& milst_all, MIList & milst, MIList & mcfi_list)
{
    mach::MIListIter mi_it;

    for (mach::MInst * mi = milst_all.get_head(&mi_it);
         mi != nullptr; mi = milst_all.get_next(&mi_it)) {
        if (MInstMgr::isCFIInstruction(mi)) {
            mcfi_list.append_tail(mi);
            continue;
        }
        milst.append_tail(mi);
    }

    if (xoc::g_dump_opt.isDumpAfterPass() && mach::g_is_dump_migen) {
        xoc::note(getRegion(),
                  "\n==---- DUMP AFTER RELOCATION (%d)'%s' ----==",
                  m_rg->id(), m_rg->getRegionName());
        milst.dump(m_rg->getLogMgr(), imgr);
    }

    if (xoc::g_dump_opt.isDumpAfterPass() && mach::g_is_dump_migen) {
        xoc::note(getRegion(),
                  "\n==---- DUMP AFTER RELOCATION for mcfi_list(%d)'%s' ----==",
                  m_rg->id(), m_rg->getRegionName());
        mcfi_list.dump(m_rg->getLogMgr(), imgr);
    }
}


void MIGen::genFrameInfo(MIList & mcfi_list)
{
    ASSERT0(g_debug);
    xoc::MCDwarfMgr * dm = m_rg->getRegionMgr()->getDwarfMgr();
    ASSERT0(dm);
    DwarfResMgr & dwarf_res_mgr = MCDWARFMGR_dwarf_res_mgr(dm);
    ASSERT0(!MCDWARFMGR_region_frame_info(dm).find(m_rg));
    MCDwarfFrameRegionInfo * frame_info_p = dwarf_res_mgr.
        allocFrameRegionInfo();

    LinearScanRA * lsra = (LinearScanRA*)(m_rg->getPassMgr()
        ->registerPass(PASS_LINEAR_SCAN_RA));
    ASSERT0(lsra);
    frame_info_p->m_fp_reg = (UINT)xgen::tmMapReg2TMWORD(lsra->getFP());
    frame_info_p->m_ra_reg = (UINT)xgen::tmMapReg2TMWORD(lsra->getRA());
    frame_info_p->m_sp_reg = (UINT)xgen::tmMapReg2TMWORD(lsra->getSP());
    MCCFIInstructionVec * instructions = dwarf_res_mgr.
        allocCFIInfoVector();
    mach::MIListIter mi_it;

    //Generate labels outside of the loop
    LabelInfo * cfa_label = m_rg->genPragmaLabel("cfa");
    LabelInfo * same_value_label = m_rg->genPragmaLabel("same_value");
    LabelInfo * cfi_offset_label = m_rg->genPragmaLabel("cfi_offset");
    LabelInfo * cfi_restore_label = m_rg->genPragmaLabel("cfi_restore");
    LabelInfo * cfa_offset_label = m_rg->genPragmaLabel("cfa_offset");

    for (mach::MInst * mi = mcfi_list.get_head(&mi_it); mi != nullptr;
         mi = mcfi_list.get_next(&mi_it)) {
        MI_CODE code = mi->getCode();
        TMWORD pc = MI_pc(mi);
        MCSymbol const* symbol = nullptr;
        switch (code) {
        case MI_cfi_def_cfa:
            symbol = dm->createVectorMCSymbol(m_rg, (UINT)pc, cfa_label);
            instructions->append(dwarf_res_mgr.allocCFIDefCfa(symbol,
                MI_cfi_def_cfa_register(mi), MI_cfi_def_cfa_offset(mi)));
            break;
        case MI_cfi_same_value:
            symbol = dm->createVectorMCSymbol(
                m_rg, (UINT)pc, same_value_label);
            instructions->append(dwarf_res_mgr.allocSameValue(symbol,
                MI_cfi_samevalue_register(mi)));
            break;
        case MI_cfi_offset:
            symbol = dm->createVectorMCSymbol(
                m_rg, (UINT)pc, cfi_offset_label);
            instructions->append(dwarf_res_mgr.allocOffset(symbol,
                MI_cfi_offset_register(mi), MI_cfi_offset_offset(mi)));
            break;
        case MI_cfi_restore:
            symbol = dm->createVectorMCSymbol(
                m_rg, (UINT)pc, cfi_restore_label);
            instructions->append(dwarf_res_mgr.allocRestore(symbol,
                MI_cfi_restore_register(mi)));
            break;
        case MI_cfi_def_cfa_offset:
            symbol = dm->createVectorMCSymbol(
                m_rg, (UINT)pc, cfa_offset_label);
            instructions->append(dwarf_res_mgr.allocDefCfaOffset(symbol,
                MI_cfi_def_cfa_offset_offset(mi)));
            break;
        default:
            UNREACHABLE();
            break;
        }
    }

    frame_info_p->m_instructions = instructions;
    frame_info_p->m_begin = dm->getMCSymbolRegionStart(m_rg);
    frame_info_p->m_end = dm->getMCSymbolRegionEnd(m_rg);
    MCDWARFMGR_region_frame_info(dm).set(m_rg, frame_info_p);
}


void MIGen::genLineInfo(MIList & milst)
{
    ASSERT0(g_debug);
    xoc::MCDwarfMgr * dm = m_rg->getRegionMgr()->getDwarfMgr();
    ASSERT0(dm);
    DwarfResMgr & dwarf_res_mgr = MCDWARFMGR_dwarf_res_mgr(dm);
    ASSERT0(!MCDWARFMGR_region_line_info(dm).find(m_rg));
    MCDwarfLineEntryVec * mc_line_entry = dwarf_res_mgr.
        allocLineEntryVector();
    mach::MIListIter mi_it;

    //Because in the previous stage,
    //the position of the first "loc" was not set to the starting position
    //i.e., where PC equals 0
    //it needs to be adjusted to the position where PC equals 0.
    bool first = true;

    //TODO: Currently,
    //only DWARF2_FLAG_IS_STMT and DWARF2_FLAG_PROLOGUE_END flag is considered.
    //Other flags are not considered for now.
    DbxMgr * dbx_mgr = m_rg->getDbxMgr();
    ASSERT0(dbx_mgr);
    for (mach::MInst * mi = milst.get_head(&mi_it);
         mi != nullptr; mi = milst.get_next(&mi_it)) {
        if (MI_dbx(mi).getLine(LANG_CPP, dbx_mgr) == DBX_UNDEF ||
            mi->getCode() == MI_label) {
            continue;
        }
        //Explain the sentence above.
        if (first) {
            first = false;
            LabelInfo * label = m_rg->genPragmaLabel("loc");
            MCSymbol const* symbol = dm->createVectorMCSymbol(m_rg,
                0, label);
            MCDwarfLineEntry * line_entry = dwarf_res_mgr.
                allocLineEntry(symbol);
            MCDWARFLOC_file_index(line_entry) = MI_dbx(mi).
                getFileIndex(LANG_CPP, dbx_mgr);
            MCDWARFLOC_line(line_entry) = MI_dbx(mi).
                getLine(LANG_CPP, dbx_mgr);
            MCDWARFLOC_column(line_entry) = MI_dbx(mi).
                getColOffset(LANG_CPP, dbx_mgr);
            MCDWARFLOC_flags(line_entry) =
                MI_dbx(mi).getFlag(LANG_CPP, dbx_mgr);
            mc_line_entry->append(line_entry);
            continue;
        }

        LabelInfo * label = m_rg->genPragmaLabel("loc");
        MCSymbol const* symbol = dm->createVectorMCSymbol(
            m_rg, (UINT)MI_pc(mi), label);
        MCDwarfLineEntry * line_entry = dwarf_res_mgr.allocLineEntry(symbol);
        MCDWARFLOC_file_index(line_entry) = MI_dbx(mi).
            getFileIndex(LANG_CPP, dbx_mgr);
        MCDWARFLOC_line(line_entry) = MI_dbx(mi).
            getLine(LANG_CPP, dbx_mgr);
        MCDWARFLOC_column(line_entry) = MI_dbx(mi).
            getColOffset(LANG_CPP, dbx_mgr);
        MCDWARFLOC_flags(line_entry) = MI_dbx(mi).
            getFlag(LANG_CPP, dbx_mgr);
        mc_line_entry->append(line_entry);
    }
    MCDWARFMGR_region_line_info(dm).set(m_rg, mc_line_entry);
}


bool MIGen::verifyMIListValid(MIList & milst) const
{
    MIListIter it;
    for (MInst * mi = milst.get_head(&it);
         mi != nullptr; mi = milst.get_next(&it)) {
        //Some MI_label does not have a corresponding IR.
        if (m_mimgr->isLabel(mi)) { continue; }
        ASSERT0(mi->getIR() && !mi->getIR()->is_undef());
    }

    return true;
}

} //namespace
