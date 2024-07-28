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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

#define DEBUG_ABBREV_SH_NAME  ".debug_abbrev"
#define DEBUG_INFO_SH_NAME    ".debug_info"
#define DEBUG_RANGES_SH_NAME  ".debug_ranges"
#define DEBUG_STR_SH_NAME     ".debug_str"
#define DEBUG_LINE_SH_NAME    ".debug_line"
#define DEBUG_FRAME_SH_NAME   ".debug_frame"


INT64 MCExpr::evaluateAsAbsolute(MCExpr const* expr)
{
    ASSERT0(expr);
    if (MCEXPR_kind(expr) == MCExpr::BINARY) {
        MCBinaryExpr const* expr_b = (MCBinaryExpr const*)expr;
        MCExpr const* lhs_value = MCBINARYEXPR_lhs(expr_b);
        MCExpr const* rhs_value = MCBINARYEXPR_rhs(expr_b);
        MCBinaryExpr::Opcode kind = MCBINARYEXPR_opcode(expr_b);
        switch (kind) {
        case MCBinaryExpr::ADD:
            return evaluateAsAbsolute(lhs_value) +
                   evaluateAsAbsolute(rhs_value);
        case MCBinaryExpr::SUB:
            return evaluateAsAbsolute(lhs_value) -
                   evaluateAsAbsolute(rhs_value);
        default:
            UNREACHABLE();
        }
    }
    if (MCEXPR_kind(expr) == MCExpr::CONSTANT) {
        return MCCONSTANTEXPR_value((MCConstantExpr const*)expr);
    }
    if (MCEXPR_kind(expr) == MCExpr::SYMBOLREF) {
        MCSymbolRefExpr const* expr_r = (MCSymbolRefExpr const*)expr;
        MCSymbol const* mc_symbol = MCSYMBOLREFEXPR_mc_symbol(expr_r);
        ASSERT0(MCSYMBOL_is_registered(mc_symbol));
        ASSERT0(MCSYMBOL_type(mc_symbol) == MCSymbol::SECTION_LABEL ||
                MCSYMBOL_type(mc_symbol) == MCSymbol::FUNC_LABEL);
        return MCSYMBOL_region_offset(mc_symbol);
    }
    UNREACHABLE();
    return 0;
}


MCDwarfMgr::MCDwarfMgr()
{
    m_mc_dwarf_dirs.clean();
    m_mc_dwarf_files.clean();
    MCDWARFMGR_char_out_stream(this).clean();
    MCDWARFMGR_byte_out_stream(this).clean();
}


MCDwarfMgr::~MCDwarfMgr() { destroy(); }


void MCDwarfMgr::overwriteBytesAtOffset(BYTEVec & vec, UINT64 value,
                                        UINT count, UINT offset)
{
    //Ensure the offset and count are within the current vector size
    ASSERT0(offset <= vec.get_elem_count() &&
            offset + count <= vec.get_elem_count());
    ::memcpy(vec.get_vec() + offset, &value, count);
}


void MCDwarfMgr::appendBytes(UINT64 value, CHAR const* name, UINT count)
{
    if (::strcmp(name, DEBUG_INFO_SH_NAME) == 0) {
        appendBytesFromValue(MCDWARFMGR_debug_info_code(this), value, count);
        return;
    }
    if (::strcmp(name, DEBUG_RANGES_SH_NAME) == 0) {
        appendBytesFromValue(MCDWARFMGR_debug_ranges_code(this), value, count);
        return;
    }
    if (::strcmp(name, DEBUG_STR_SH_NAME) == 0) {
        appendBytesFromValue(MCDWARFMGR_debug_str_code(this), value, count);
        return;
    }
    if (::strcmp(name, DEBUG_LINE_SH_NAME) == 0) {
        appendBytesFromValue(MCDWARFMGR_debug_line_code(this), value, count);
        return;
    }
    if (::strcmp(name, DEBUG_FRAME_SH_NAME) == 0) {
        appendBytesFromValue(MCDWARFMGR_debug_frame_code(this), value, count);
        return;
    }
    UNREACHABLE();
}


void MCDwarfMgr::recordFileInfo(UINT32 file_num, xoc::Sym const* all_file_name)
{
    CHAR const* all_dir_path = all_file_name->getStr();

    //Get the file's path
    xcom::StrBuf buf_file_dir_path((UINT)::strlen(all_file_name->getStr()) + 1);
    getFilePath(all_dir_path, buf_file_dir_path.buf,
                buf_file_dir_path.getBufLen());

    //Get the file's name
    CHAR const* file_name = extractRightMostSubString(all_dir_path, '/');
    UINT32 dir_index;
    bool exist_dir = false;
    for (UINT32 i = 0; i < m_mc_dwarf_dirs.get_elem_count(); i++) {
        if (buf_file_dir_path.is_equal(m_mc_dwarf_dirs[i]->getBuf())) {
            exist_dir = true;
            dir_index = i;
            break;
        }
    }

    //Create actual directory and file names.
    xcom::StrBuf * file_name_str = MCDWARFMGR_dwarf_res_mgr(this).
        allocStr(((UINT)::strlen(file_name) + 1));
    ::memcpy(file_name_str->buf, file_name, file_name_str->getBufLen());
    if (!exist_dir) {
        xcom::StrBuf * file_dir_str = MCDWARFMGR_dwarf_res_mgr(this).
            allocStr(((UINT)::strlen(buf_file_dir_path.buf) + 1));
        ::memcpy(file_dir_str->buf, buf_file_dir_path.buf,
                 file_dir_str->getBufLen());
        dir_index = m_mc_dwarf_dirs.get_elem_count();
        m_mc_dwarf_dirs.set(dir_index, file_dir_str);
    }
    xoc::MCDwarfFile file_info;
    file_info.m_name = file_name_str;

    //Directories start from 1.
    dir_index = dir_index + 1;
    file_info.m_dir_index = dir_index;
    m_mc_dwarf_files.set(file_num, file_info);
}


void MCDwarfMgr::setSpecialTagLabel(LabelInfo * label, MCSymbol * mc_symbol_ptr)
{
    Sym const* sym = LABELINFO_pragma(label);
    ASSERT0(sym);

    //If the string contains 'begin'
    //it indicates the beginning of a function
    if (xcom::xstrstr(sym->getStr(), "begin") != -1) {
        MCSYMBOL_is_registered(mc_symbol_ptr) = true;
        MCSYMBOL_region_offset(mc_symbol_ptr) = 0;
    }
}


MCSymbol const* MCDwarfMgr::createVectorMCSymbol(Region const* region,
                                                 UINT region_offset,
                                                 LabelInfo const* label)
{
    ASSERT0(region && label);
    MCSymbol * mc_symbol_ptr = MCDWARFMGR_dwarf_res_mgr(this).allocMCSymbol();
    MCSYMBOL_is_registered(mc_symbol_ptr) = true;
    MCSYMBOL_region(mc_symbol_ptr) = region;
    MCSYMBOL_region_offset(mc_symbol_ptr) = region_offset;
    MCSYMBOL_label(mc_symbol_ptr) = label;
    MCSYMBOL_type(mc_symbol_ptr) = MCSymbol::SECTION_LABEL;
    return mc_symbol_ptr;
}


MCSymbol * MCDwarfMgr::createVectorMCSymbol(Region const* region,
                                            LabelInfo const* label)
{
    //Region could be nullptr, so no need for checking.
    ASSERT0(label);
    MCSymbol * mc_symbol_ptr = MCDWARFMGR_dwarf_res_mgr(this).allocMCSymbol();
    MCSYMBOL_is_registered(mc_symbol_ptr) = false;
    MCSYMBOL_region(mc_symbol_ptr) = region;
    MCSYMBOL_region_offset(mc_symbol_ptr) = 0;
    MCSYMBOL_label(mc_symbol_ptr) = label;
    MCSYMBOL_type(mc_symbol_ptr) = MCSymbol::SECTION_LABEL;
    return mc_symbol_ptr;
}


void MCDwarfMgr::createMCSymbol(Region const* region, UINT region_offset,
                                LabelInfo const* label)
{
    ASSERT0(region && label);
    Sym const* prag_sym = label->getPragma();
    if (m_map_symbol.find(prag_sym)) { return; }
    MCSymbol * mc_symbol_ptr = MCDWARFMGR_dwarf_res_mgr(this).
        allocMCSymbol();
    MCSYMBOL_is_registered(mc_symbol_ptr) = true;
    MCSYMBOL_region(mc_symbol_ptr) = region;
    MCSYMBOL_region_offset(mc_symbol_ptr) = region_offset;
    MCSYMBOL_label(mc_symbol_ptr) = label;
    MCSYMBOL_type(mc_symbol_ptr) = MCSymbol::SECTION_LABEL;
    m_map_symbol.set(prag_sym, mc_symbol_ptr);
}


//Set func label offset.
void MCDwarfMgr::setFuncLabelOff(UINT offset, Sym const* name)
{
    if (!isSymbolfind(name)) { return; }
    MCSymbol * sym = m_map_symbol.get(name);
    if (MCSYMBOL_is_registered(sym)) { return; }
    MCSYMBOL_region_offset(sym) = offset;
    MCSYMBOL_is_registered(sym) = true;
}


void MCDwarfMgr::createMCSymbol(Region const* region, LabelInfo * label)
{
    ASSERT0(region && label);
    Sym const* prag_sym = label->getPragma();
    if (m_map_symbol.find(prag_sym)) { return; }
    MCSymbol * mc_symbol_ptr = MCDWARFMGR_dwarf_res_mgr(this).
        allocMCSymbol();
    MCSYMBOL_is_registered(mc_symbol_ptr) = false;
    MCSYMBOL_region(mc_symbol_ptr) = region;
    MCSYMBOL_region_offset(mc_symbol_ptr) = 0;
    MCSYMBOL_label(mc_symbol_ptr) = label;
    setSpecialTagLabel(label, mc_symbol_ptr);
    MCSYMBOL_type(mc_symbol_ptr) = MCSymbol::FUNC_LABEL;
    m_map_symbol.set(prag_sym, mc_symbol_ptr);
}


//Create a registered label.
void MCDwarfMgr::createMCSymbol(Region const* region, LabelInfo const* label,
                                bool is_registered)
{
    ASSERT0(region && label);
    CHAR const* name = region->getRegionVar()->get_name()->getStr();
    UINT off = 0;

    //Determine offset based on section name
    if (::strcmp(name, DEBUG_INFO_SH_NAME) == 0) {
        off = MCDWARFMGR_debug_info_code(this).get_elem_count();
    } else if (::strcmp(name, DEBUG_RANGES_SH_NAME) == 0) {
        off = MCDWARFMGR_debug_ranges_code(this).get_elem_count();
    } else if (::strcmp(name, DEBUG_STR_SH_NAME) == 0) {
        off = MCDWARFMGR_debug_str_code(this).get_elem_count();
    } else if (::strcmp(name, DEBUG_LINE_SH_NAME) == 0) {
        off = MCDWARFMGR_debug_line_code(this).get_elem_count();
    } else { UNREACHABLE(); }

    Sym const* prag_sym = label->getPragma();
    ASSERT0(prag_sym);

    bool found = false;
    MCSymbol * mc_symbol_ptr = m_map_symbol.get(prag_sym, &found);

    if (!found) {
        mc_symbol_ptr = MCDWARFMGR_dwarf_res_mgr(this).allocMCSymbol();
        MCSYMBOL_is_registered(mc_symbol_ptr) = is_registered;
        m_map_symbol.set(prag_sym, mc_symbol_ptr);
    } else {
        MCSYMBOL_is_registered(mc_symbol_ptr) = true;
    }

    MCSYMBOL_region(mc_symbol_ptr) = region;
    MCSYMBOL_region_offset(mc_symbol_ptr) = off;
    MCSYMBOL_label(mc_symbol_ptr) = label;
    MCSYMBOL_type(mc_symbol_ptr) = MCSymbol::SECTION_LABEL;
}


MCSymbol * MCDwarfMgr::getOrCreateSymbol(Sym const* name)
{
    MCSymbol * mc_symbol_ptr = nullptr;
    bool found = false;
    mc_symbol_ptr = m_map_symbol.get(name, &found);
    if (found) {
        return mc_symbol_ptr;
    }
    mc_symbol_ptr = MCDWARFMGR_dwarf_res_mgr(this).allocMCSymbol();
    MCSYMBOL_is_registered(mc_symbol_ptr) = false;
    MCSYMBOL_region(mc_symbol_ptr) = nullptr;
    MCSYMBOL_region_offset(mc_symbol_ptr) = 0;
    MCSYMBOL_label(mc_symbol_ptr) = nullptr;
    m_map_symbol.set(name, mc_symbol_ptr);
    return mc_symbol_ptr;
}


void MCDwarfMgr::handleGlobalVarSymSingleRefRel(Sym const* name,
                                                CHAR const* region_name,
                                                MCFixupKind kind)
{
    ASSERT0(name && region_name);
    MCSymbol * mc_symbol = nullptr;
    TMap<Region const*, MCSymbol*> * v_symbol = m_map_symbol_var.get(name);
    ASSERT0(v_symbol);
    REGION2MCSYMBOLITER iter;
    MCSymbol * mc_symbol_p = nullptr;
    for (v_symbol->get_first(iter, &mc_symbol_p); !iter.end();
         v_symbol->get_next(iter, &mc_symbol_p)) {
        //The region for global variables must be empty,
        //so use this to filter them.
        if (MCSYMBOL_region(mc_symbol_p) == nullptr) {
            mc_symbol = mc_symbol_p;
            break;
        }
    }
    UINT src_section_off = MCDWARFMGR_debug_info_code(this).get_elem_count();
    MCExpr const* ref_exp = MCDWARFMGR_dwarf_res_mgr(this).
        allocMCSymbolRefExpr(mc_symbol);
    MCFixup * fix = MCDWARFMGR_dwarf_res_mgr(this).
        allocFixup(src_section_off, ref_exp, kind);
    if (::strcmp(region_name, DEBUG_INFO_SH_NAME) == 0) {
        MCDWARFMGR_debug_info_fixups(this).append(fix);
    } else {
        //TODO: In the current context, consider only debug_info
        UNREACHABLE();
    }
    appendBytes(0, region_name, getSizeForFixupKind(kind));
}


void MCDwarfMgr::handleFuncAndSectionLabelSingleRefRel(Sym const* name,
        CHAR const* region_name, MCFixupKind kind)
{
    ASSERT0(name && region_name);

    //Consider func and section label.
    getOrCreateSymbol(name);
    UINT src_section_off = 0;
    if (::strcmp(region_name, DEBUG_INFO_SH_NAME) == 0) {
        src_section_off = MCDWARFMGR_debug_info_code(this).get_elem_count();
    } else if (::strcmp(region_name, DEBUG_RANGES_SH_NAME) == 0) {
        src_section_off = MCDWARFMGR_debug_ranges_code(this).get_elem_count();
    } else {
        //TODO: In the current context, consider only debug_info
        UNREACHABLE();
    }
    MCSymbol const* mc_symbol = m_map_symbol.get(name);
    ASSERT0(mc_symbol);
    MCExpr const* ref_exp = MCDWARFMGR_dwarf_res_mgr(this).
        allocMCSymbolRefExpr(mc_symbol);
    MCFixup * fix = MCDWARFMGR_dwarf_res_mgr(this).
        allocFixup(src_section_off, ref_exp, kind);
    if (::strcmp(region_name, DEBUG_INFO_SH_NAME) == 0) {
        MCDWARFMGR_debug_info_fixups(this).append(fix);
    } else if (::strcmp(region_name, DEBUG_RANGES_SH_NAME) == 0) {
        MCDWARFMGR_debug_ranges_fixups(this).append(fix);
    } else {
        //TODO: In the current context, consider only debug_info
        UNREACHABLE();
    }
    appendBytes(0, region_name, getSizeForFixupKind(kind));
}


void MCDwarfMgr::createSingleRefRel(Sym const* name, CHAR const* region_name,
                                    MCFixupKind kind)
{
    ASSERT0(name);

    //Consider global var and stack var
    if (isVarSymbolPresent(name)) {
        handleGlobalVarSymSingleRefRel(name, region_name, kind);
        return;
    }

    if (::strcmp(region_name, DEBUG_INFO_SH_NAME) == 0 ||
        ::strcmp(region_name, DEBUG_RANGES_SH_NAME) == 0) {
        handleFuncAndSectionLabelSingleRefRel(name, region_name, kind);
        return;
    }

    //TODO: In the current context, consider only debug_info
    UNREACHABLE();
}


//MCDWARFMGR_debug_ranges_fixups
void MCDwarfMgr::createBinaryExprRef(Sym const* name0, Sym const* name1,
                                     CHAR const* region_name,
                                     MCBinaryExpr::Opcode op_type,
                                     MCFixupKind kind)
{
    ASSERT0(name0 && name1);
    getOrCreateSymbol(name0);
    getOrCreateSymbol(name1);
    MCSymbol const* mc_symbol0 = m_map_symbol.get(name0);
    MCExpr const* ref_exp0 = MCDWARFMGR_dwarf_res_mgr(this).
        allocMCSymbolRefExpr(mc_symbol0);
    MCSymbol const* mc_symbol1 = m_map_symbol.get(name1);
    MCExpr const* ref_exp1 = MCDWARFMGR_dwarf_res_mgr(this).
        allocMCSymbolRefExpr(mc_symbol1);
    if (::strcmp(region_name, DEBUG_INFO_SH_NAME) == 0) {
        UINT src_section_off = MCDWARFMGR_debug_info_code(this).
            get_elem_count();
        MCExpr const* binary_exp = MCDWARFMGR_dwarf_res_mgr(this).
            allocMCBinaryExpr(op_type, ref_exp0, ref_exp1);
        MCFixup * fix = MCDWARFMGR_dwarf_res_mgr(this).
            allocFixup(src_section_off, binary_exp, kind);
        MCDWARFMGR_debug_info_fixups(this).append(fix);
        appendBytes(0, region_name, getSizeForFixupKind(kind));
        return;
    }

    //TODO: In the current context, consider only debug_info
    UNREACHABLE();
}


bool MCDwarfMgr::isSymbolRegister(Sym const* name)
{
    return m_map_symbol.find(name) &&
        MCSYMBOL_is_registered(m_map_symbol.get(name));
}


void MCDwarfMgr::createVarRefRel(Region const* func_region,
                                 Sym const* var_name,
                                 CHAR const* region_name,
                                 MCFixupKind kind)
{
    ASSERTN(isVarSymbolFound(var_name, func_region),
            ("Var has not been registered previously. "
             "It must be registered first before it can be used."));
    if (::strcmp(region_name, DEBUG_INFO_SH_NAME) == 0) {
        UINT src_section_off = MCDWARFMGR_debug_info_code(this).
            get_elem_count();
        MCSymbol const* mc_symbol = getVarSymbolInRegion(var_name,
                                                         func_region);
        MCExpr const* ref_exp = MCDWARFMGR_dwarf_res_mgr(this).
            allocMCSymbolRefExpr(mc_symbol);
        MCFixup * fix = MCDWARFMGR_dwarf_res_mgr(this).
            allocFixup(src_section_off, ref_exp, kind);
        MCDWARFMGR_debug_info_fixups(this).append(fix);
        appendBytes(0, region_name, getSizeForFixupKind(kind));
        return;
    }

    //TODO: In the current context, consider only debug_info
    UNREACHABLE();
}


void MCDwarfMgr::updateCfa(UINT cur_cfa, INT cur_cfa_offset)
{
    m_cur_cfa = cur_cfa;
    m_cur_cfa_offset = cur_cfa_offset;
}


void MCDwarfMgr::setVarFinalValue(INT64 fp_offset, Var const* var, UINT fp,
                                  Region const* region)
{
    ASSERT0(var && region);
    Sym const* name = var->get_name();
    if (!isVarSymbolFound(name, region)) {return;}
    MCSymbol * mc_symbol = getVarSymbolInRegion(name, region);
    ASSERT0(mc_symbol);
    if (MCSYMBOL_is_registered(mc_symbol)) {
        return;
    }
    ASSERT0(MCSYMBOL_type(mc_symbol) == MCSymbol::FUNC_VAR);
    MCDWARFMGR_char_out_stream(this).clean();
    xcom::encodeSLEB128(fp_offset, MCDWARFMGR_char_out_stream(this),
                        MCSYMBOL_VAR_STACK_OFF_SIZE);
    note(MCSYMBOL_region(mc_symbol),
         "\nBegin determining the offset of \
         the stack var based on the CFA.:%s",
         name->getStr());
    UINT8 os_size = MCDWARFMGR_char_out_stream(this).get_elem_count();
    ASSERT0(os_size <= MCSYMBOL_VAR_STACK_OFF_SIZE);
    ::memcpy(MCSYMBOL_value(mc_symbol),
        MCDWARFMGR_char_out_stream(this).get_vec(), os_size);
    if (os_size < MCSYMBOL_VAR_STACK_OFF_SIZE) {
        ::memset(MCSYMBOL_value(mc_symbol) + os_size, 0,
            MCSYMBOL_VAR_STACK_OFF_SIZE - os_size);
    }
    MCSYMBOL_is_registered(mc_symbol) = true;
}


bool MCDwarfMgr::isVarSymbolPresent(Sym const* name)
{
    return m_map_symbol_var.find(name);
}


bool MCDwarfMgr::isVarSymbolInVector(Sym const* name, Region const* region)
{
    ASSERT0(m_map_symbol_var.find(name));
    TMap<Region const*, MCSymbol*> * sym_v = m_map_symbol_var.get(name);
    return sym_v->find(region);
}


bool MCDwarfMgr::isVarSymbolFound(Sym const* name, Region const* region)
{
    ASSERT0(region && name);
    if (!isVarSymbolPresent(name)) {
        return false;
    }
    return isVarSymbolInVector(name, region);
}


void MCDwarfMgr::createNewVarSymbol(Region const* region,
                                    Var * var, Sym const* name)
{
    xcom::TMap<const xoc::Region*, xoc::MCSymbol*> * sym_v =
        MCDWARFMGR_dwarf_res_mgr(this).allocMapRegionVar();
    xoc::MCSymbol * mc_symbol_ptr =
        MCDWARFMGR_dwarf_res_mgr(this).allocMCSymbol();
    MCSYMBOL_is_registered(mc_symbol_ptr) = false;

    //Global variable does not have a region
    MCSYMBOL_region(mc_symbol_ptr) = var->is_global() ? nullptr : region;
    MCSYMBOL_value(mc_symbol_ptr)[0] = 0;
    MCSYMBOL_var(mc_symbol_ptr) = var;
    MCSYMBOL_type(mc_symbol_ptr) = MCSymbol::FUNC_VAR;
    sym_v->set(region, mc_symbol_ptr);
    m_map_symbol_var.set(name, sym_v);

    if (var->is_global()) {
        note(region,
             "Registering global %s for function m_map_symbol_var addr %x, "
             "id: %d\n",
             var->get_name()->getStr(), region, REGION_id(region));
    } else {
        ASSERT0(region->getRegionVar()->get_name());
        note(region,
             "Registering %s for function m_map_symbol_var %s, addr %x, "
             "id: %d\n",
             var->get_name()->getStr(),
             region->getRegionVar()->get_name()->getStr(),
             region, REGION_id(region));
    }
}


void MCDwarfMgr::updateExistingVarSymbol(Region const* region,
                                         Var * var, Sym const* name)
{
    xoc::MCSymbol * mc_symbol_ptr =
        MCDWARFMGR_dwarf_res_mgr(this).allocMCSymbol();
    MCSYMBOL_is_registered(mc_symbol_ptr) = false;
    MCSYMBOL_region(mc_symbol_ptr) = region;
    MCSYMBOL_value(mc_symbol_ptr)[0] = 0;
    MCSYMBOL_var(mc_symbol_ptr) = var;
    MCSYMBOL_type(mc_symbol_ptr) = MCSymbol::FUNC_VAR;
    m_map_symbol_var.get(name)->set(region, mc_symbol_ptr);

    ASSERT0(region->getRegionVar()->get_name());
    note(region,
         "Registering %s for function m_map_symbol_var %s, addr %x\n",
         var->get_name()->getStr(),
         region->getRegionVar()->get_name()->getStr(),
         region);
}


void MCDwarfMgr::createVarOrUpdateSymbol(Region const* region, Var * var)
{
    ASSERT0(region && var);
    Sym const* name = var->get_name();

    //Check if the symbol is not present.
    if (!isVarSymbolPresent(name)) {
        createNewVarSymbol(region, var, name);
        return;
    }

    //If the symbol is present but not in the vector, update the symbol.
    if (!isVarSymbolInVector(name, region)) {
        updateExistingVarSymbol(region, var, name);
        return;
    }

    //If the symbol already exists in the vector, assert an error.
    ASSERTN(false, ("symbol already exists"));
}


Region const* MCDwarfMgr::getVarRegion(Sym const* name,
                                       xoc::RegionMgr * region_mgr)
{
    ASSERT0(name && region_mgr);
    RegionMgr::RegionTab * region_tab = region_mgr->getRegionTab();
    SymbolHashFunc c;
    for (UINT i = 0; i < region_tab->get_elem_count(); i++) {
        Region const* region_ptr = nullptr;
        if ((*region_tab)[i] == nullptr) {
            continue;
        }
        region_ptr = (*region_tab)[i];
        if (region_ptr->getRegionVar() == nullptr) {
            continue;
        }

        //TODO: without considering declarations.
        //TODO: The current comparison method may have issues.
        if (region_ptr->getIRList() == nullptr) {
            continue;
        }

        if (c.compare(name, region_ptr->getRegionVar()->get_name())) {
            return (*region_tab)[i];
        }
    }
    return nullptr;
}


MCSymbol * MCDwarfMgr::getVarSymbolInRegion(Sym const* name,
                                            Region const* region) const
{
    ASSERT0(m_map_symbol_var.find(name));
    TMap<Region const*, MCSymbol*> * sym_v = m_map_symbol_var.get(name);
    if (!sym_v->find(region)) { return nullptr; }
    return sym_v->get(region);
}


void MCDwarfMgr::setStageElfSymbol(Sym const* name, UINT offset)
{
    ASSERT0(isVarSymbolPresent(name));
    MCSymbol * mc_symbol = nullptr;
    TMap<Region const*, MCSymbol*> * v_synbol = m_map_symbol_var.get(name);
    ASSERT0(v_synbol);
    REGION2MCSYMBOLITER iter;
    MCSymbol * mc_symbol_p = nullptr;
    for ((*v_synbol).get_first(iter, &mc_symbol_p); !iter.end();
         (*v_synbol).get_next(iter, &mc_symbol_p)) {
        //The region for global variables must be empty,
        //so use this to filter them.
        if (MCSYMBOL_region(mc_symbol_p) == nullptr) {
            mc_symbol = mc_symbol_p;
            break;
        }
    }
    MCSYMBOL_region_offset(mc_symbol) = offset;
    MCSYMBOL_is_registered(mc_symbol) = true;
    MCSYMBOL_region(mc_symbol) = nullptr;
}


MCSymbol const* MCDwarfMgr::getMCSymbolRegionStart(
    Region const* region) const
{
    ASSERT0(region);
    MCSymbol * mc_sym_info = nullptr;
    xcom::TMapIter<Sym const*, MCSymbol*> map_symbol_iter;
    Sym const* syn_name = nullptr;
    for (syn_name = m_map_symbol.get_first(map_symbol_iter, &mc_sym_info);
         !map_symbol_iter.end();
        syn_name = m_map_symbol.get_next(map_symbol_iter, &mc_sym_info)) {
        ASSERT0(MCSYMBOL_region(mc_sym_info));
        if (REGION_id(region) == REGION_id(MCSYMBOL_region(mc_sym_info))) {
            //The starting symbol in the future frontend will definitely
            //contain the substring 'begin'.
            if (xcom::xstrstr(syn_name->getStr(), MCSYMBOL_START_FLAG) != -1) {
                return mc_sym_info;
            }
        }
    }
    return mc_sym_info;
}


MCSymbol const* MCDwarfMgr::getMCSymbolRegionEnd(Region const* region) const
{
    ASSERT0(region);
    MCSymbol * mc_sym_info = nullptr;
    Sym const* syn_name = nullptr;
    xcom::TMapIter<Sym const*, MCSymbol*> map_symbol_iter;
    for (syn_name = m_map_symbol.get_first(map_symbol_iter, &mc_sym_info);
         !map_symbol_iter.end();
        syn_name = m_map_symbol.get_next(map_symbol_iter, &mc_sym_info)) {
        ASSERT0(MCSYMBOL_region(mc_sym_info));
        if (REGION_id(region) == REGION_id(MCSYMBOL_region(mc_sym_info))) {
            //The endding symbol in the future frontend will definitely
            //contain the substring 'end'.
            if (xcom::xstrstr(syn_name->getStr(), MCSYMBOL_END_FLAG) != -1) {
                return mc_sym_info;
            }
        }
    }
    return mc_sym_info;
}


//This helper routine returns an expression of End - Start + IntVal .
inline static MCExpr const* makeEndMinusStartExpr(MCSymbol const* start,
                                                  MCSymbol const* end,
                                                  INT int_val, MCDwarfMgr * dm)
{
    ASSERT0(start && end && dm);
    MCExpr const* res = MCDWARFMGR_dwarf_res_mgr(dm).allocMCSymbolRefExpr(end);
    MCExpr const* rhs = MCDWARFMGR_dwarf_res_mgr(dm).
        allocMCSymbolRefExpr(start);
    MCExpr const* res1 = MCDWARFMGR_dwarf_res_mgr(dm).
        allocMCBinaryExpr(MCBinaryExpr::SUB, res, rhs);
    MCExpr const* res2 = MCDWARFMGR_dwarf_res_mgr(dm).
        allocMCConstantExpr(int_val);
    MCExpr const* res3 = MCDWARFMGR_dwarf_res_mgr(dm).
        allocMCBinaryExpr(MCBinaryExpr::SUB, res1, res2);
    return res3;
}


Region const* MCDwarfMgr::getSectionRegion(RegionMgr * rm, CHAR const* name)
{
    ASSERT0(rm && name);
    for (UINT i = 0; i < rm->getNumOfRegion(); i++) {
        Region * rg = (*(rm->getRegionTab()))[i];
        if (rg == nullptr)
            continue;
        Var * var = (*(rm->getRegionTab()))[i]->getRegionVar();
        if (var == nullptr)
            continue;
        CHAR const* region_name = var->get_name()->getStr();
        if (::strcmp(region_name, name) == 0) {
            return (*(rm->getRegionTab()))[i];
        }
    }
    ASSERT0(false);
    return nullptr;
}


MCSymbol const* MCDwarfMgr::genFrameCIE(Region * region_ptr)
{
    ASSERT0(region_ptr);
    UINT32 size_before = MCDWARFMGR_debug_frame_code(this).get_elem_count();
    UINT32 const DW_CIE_ID = 0xffffffff;

    //Create start mcsymbol of cie
    LabelInfo * label_start_cfi = region_ptr->genPragmaLabel("cfi_start");
    Region const* re =
        getSectionRegion(REGION_region_mgr(region_ptr), DEBUG_FRAME_SH_NAME);
    MCSymbol * symbol_start_cfi =
        (MCSymbol*)(this->createVectorMCSymbol(re, label_start_cfi));
    MCSYMBOL_region_offset(symbol_start_cfi) = 0;
    MCSYMBOL_is_registered(symbol_start_cfi) = true;

    //Create end mcsymbol of cie
    LabelInfo * label_end = region_ptr->genPragmaLabel("cfi_end");
    MCSymbol * symbol_end_cfi =
        (MCSymbol*)(this->createVectorMCSymbol(re, label_end));

    //Record how much data has been placed along the way,
    //then determine whether padding is necessary
    //TODO:Currently, only consider the DWARF32 format.
    INT32 unit_length_bytes = 4;
    MCExpr const* exp_length = makeEndMinusStartExpr(
        symbol_start_cfi, symbol_end_cfi, unit_length_bytes, this);
    UINT src_section_off = MCDWARFMGR_debug_frame_code(this).get_elem_count();
    MCFixup * fix = MCDWARFMGR_dwarf_res_mgr(this).
        allocFixup(src_section_off, exp_length, FK_DATA_4);
    MCDWARFMGR_debug_frame_fixups(this).append(fix);

    //Length size: 4byte
    appendBytes(0, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_4));

    //CIE ID size: 4byte
    UINT32 cie_id = DW_CIE_ID;
    appendBytes(cie_id, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_4));

    //Version is currently 1, size: 1 byte
    UINT8 CIEVersion = 1;
    appendBytes(CIEVersion, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_1));

    //Size: 1byte
    appendBytes(0, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_1));

    //Code Alignment Factor size: 1byte
    appendBytes(1, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_1));

    //Date Alignment Factor size: 1byte
    INT8 data_alin = -8; //TODO:fixed value
    Vector<CHAR> os_data_align;
    xcom::encodeSLEB128(data_alin, os_data_align);
    MCDWARFMGR_debug_frame_code(this).
        append((BYTE*)os_data_align.get_vec(), os_data_align.get_elem_count());

    //Return Address Register size: 1 byte
    UINT ra_num = MCDWARFMGR_region_frame_info(this).get(region_ptr)->m_ra_reg;
    appendBytes(ra_num, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_1));
    UINT sp_num = MCDWARFMGR_region_frame_info(this).get(region_ptr)->m_sp_reg;
    UINT fp_num = MCDWARFMGR_region_frame_info(this).get(region_ptr)->m_fp_reg;

    //Create a collection of initial instructions.
    //DW_CFA_def_cfa: reg_sp +0  size: 3byte
    MCDWARFMGR_debug_frame_code(this).append(DW_CFA_def_cfa);
    MCDWARFMGR_debug_frame_code(this).append(sp_num);
    MCDWARFMGR_debug_frame_code(this).append(0);

    //DW_CFA_same_value: ra_num size: 2byte
    MCDWARFMGR_debug_frame_code(this).append(DW_CFA_same_value);
    MCDWARFMGR_debug_frame_code(this).append(ra_num);

    //DW_CFA_same_value: fp_num size: 2byte
    MCDWARFMGR_debug_frame_code(this).append(DW_CFA_same_value);
    MCDWARFMGR_debug_frame_code(this).append(fp_num);
    UINT32 size_current =
        MCDWARFMGR_debug_frame_code(this).get_elem_count() - size_before;

    //Alignment to 8 bytes is required for padding
    CHAR alig_num = 8;
    UINT32 size_alin = (UINT32)xcom::ceil_align(size_current, alig_num);
    appendBytesFromValue(MCDWARFMGR_debug_frame_code(this), 0,
                         size_alin - size_current);

    //Register cfi end mcsymbol
    MCSYMBOL_region_offset(symbol_end_cfi) =
        MCDWARFMGR_debug_frame_code(this).get_elem_count();
    MCSYMBOL_is_registered(symbol_end_cfi) = true;
    return symbol_start_cfi;
}


void MCDwarfMgr::encodeAdvanceLoc(OUT Vector<UINT8> & os, UINT64 addr_delta)
{
    UINT64 const mask_0 = (UINT64)0x1 << 6;
    UINT64 const mask_1 = (UINT64)0x1 << 8;
    UINT64 const mask_2 = (UINT64)0x1 << 16;
    UINT64 const mask_3 = (UINT64)0x1 << 32;

    if (addr_delta <= mask_0) {
        UINT8 opcode = (UINT8)(DW_CFA_advance_loc | addr_delta);
        os.append(opcode);
        return;
    }

    if (addr_delta <= mask_1) {
        os.append(DW_CFA_advance_loc1);
        os.append(UINT8(addr_delta));
        return;
    }

    if (addr_delta <= mask_2) {
        os.append(DW_CFA_advance_loc2);
        appendBytesFromValue(os, UINT16(addr_delta), 2);
        return;
    }

    if (addr_delta <= mask_3) {
        os.append(DW_CFA_advance_loc4);
        appendBytesFromValue(os, UINT32(addr_delta), 4);
        return;
    }

    ASSERT0(!(addr_delta > mask_3));
}


void MCDwarfMgr::genAdvanceLoc(UINT64 addr_delta)
{
    ASSERT0(addr_delta > 0);
    MCDWARFMGR_byte_out_stream(this).clean();
    encodeAdvanceLoc(MCDWARFMGR_byte_out_stream(this), addr_delta);
    MCDWARFMGR_debug_frame_code(this).
        append((BYTE*)MCDWARFMGR_byte_out_stream(this).get_vec(),
               MCDWARFMGR_byte_out_stream(this).get_elem_count());
}


void MCDwarfMgr::genULEB128AndToV(INT32 value, OUT Vector<BYTE> & dst)
{
    MCDWARFMGR_char_out_stream(this).clean();
    xcom::encodeULEB128(value, MCDWARFMGR_char_out_stream(this));
    dst.append((BYTE*)MCDWARFMGR_char_out_stream(this).get_vec(),
               MCDWARFMGR_char_out_stream(this).get_elem_count());
}


bool MCDwarfMgr::isNullRegion()
{
    return MCDWARFMGR_region_frame_info(this).get_elem_count() == 0;
}


void MCDwarfMgr::genCFIInstruction(MCCFIInstruction * ins)
{
    ASSERT0(ins);
    switch (ins->getOperation()) {
    case MCCFIInstruction::OPDEFCFA: {
        UINT32 reg = ins->getRegister();
        MCDWARFMGR_debug_frame_code(this).append(DW_CFA_def_cfa);
        genULEB128AndToV(reg, MCDWARFMGR_debug_frame_code(this));
        INT32 offset = ins->getOffset();
        genULEB128AndToV(offset, MCDWARFMGR_debug_frame_code(this));
        return;
    }
    case MCCFIInstruction::OPSAMEVALUE: {
        UINT32 reg = ins->getRegister();
        MCDWARFMGR_debug_frame_code(this).append(DW_CFA_same_value);
        genULEB128AndToV(reg, MCDWARFMGR_debug_frame_code(this));
        return;
    }
    case MCCFIInstruction::OPOFFSET: {
        UINT32 reg = ins->getRegister();
        INT32 offset = ins->getOffset();
        INT32 data_alignment_factor = -8;
        offset = offset / data_alignment_factor;
        if (offset < 0) {
            ASSERT0(false);
        } else if (reg < computeMaxBitSizeForValue(6)) {
            MCDWARFMGR_debug_frame_code(this).append(DW_CFA_offset + reg);
            genULEB128AndToV(offset, MCDWARFMGR_debug_frame_code(this));
        } else {
            MCDWARFMGR_debug_frame_code(this).append(DW_CFA_offset_extended);
            genULEB128AndToV(reg, MCDWARFMGR_debug_frame_code(this));
            genULEB128AndToV(offset, MCDWARFMGR_debug_frame_code(this));
        }
        return;
    }
    case MCCFIInstruction::OPRESTORE: {
        UINT32 reg = ins->getRegister();
        if (reg < computeMaxBitSizeForValue(6)) {
            MCDWARFMGR_debug_frame_code(this).append(DW_CFA_restore | reg);
        } else {
            MCDWARFMGR_debug_frame_code(this).append(DW_CFA_restore_extended);
            genULEB128AndToV(reg, MCDWARFMGR_debug_frame_code(this));
        }
        return;
    }
    case MCCFIInstruction::OPDEFCFAOFFSET: {
        MCDWARFMGR_debug_frame_code(this).append(DW_CFA_def_cfa_offset);
        INT32 offset = ins->getOffset();
        genULEB128AndToV(offset, MCDWARFMGR_debug_frame_code(this));
        return;
    }
    default:
        UNREACHABLE();
    }
}


void MCDwarfMgr::genCFIInstructions(
     MCDwarfFrameRegionInfo * region_frame_info)
{
    ASSERT0(region_frame_info);
    MCSymbol const* start_symbol =
        (*(region_frame_info->m_instructions))[0]->getLabel();
    ASSERT0(start_symbol);
    UINT32 pc_before = MCSYMBOL_region_offset(start_symbol);
    ASSERT0(pc_before == 0);
    Vector<MCCFIInstruction*> * inst_v = region_frame_info->m_instructions;
    for (UINT i = 0; i < inst_v->get_elem_count(); i++) {
        MCCFIInstruction * cfi_ins = (*inst_v)[i];
        UINT32 pc = MCSYMBOL_region_offset(cfi_ins->getLabel());

        //Advance row if new location.
        if (pc > pc_before) {
            genAdvanceLoc(pc - pc_before);
        }
        genCFIInstruction(cfi_ins);

        //Update the PC of the current CFI instruction.
        pc_before = pc;
    }
}


void MCDwarfMgr::genFrameFDE(MCSymbol const* cie_mc_symbol,
                             MCDwarfFrameRegionInfo * region_frame_info,
                             Region * region)
{
    ASSERT0(cie_mc_symbol && region_frame_info && region);
    UINT32 size_before = MCDWARFMGR_debug_frame_code(this).get_elem_count();

    //Create start mcsymbol of fde
    LabelInfo * label_start_fde = region->genPragmaLabel("fde_start");
    MCSymbol * symbol_start_fde =
        (MCSymbol*)(this->createVectorMCSymbol(nullptr, label_start_fde));
    MCSYMBOL_region_offset(symbol_start_fde) = size_before;
    MCSYMBOL_is_registered(symbol_start_fde) = true;

    //Create end mcsymbol of fde,
    //Not yet fully registered,
    //awaiting final registration before completion
    LabelInfo * label_end = region->genPragmaLabel("fde_end");
    MCSymbol * symbol_end_fde =
        (MCSymbol*)(this->createVectorMCSymbol(nullptr, label_end));

    //TODO: Currently, only consider the DWARF32 format.
    INT32 unit_length_bytes = 4;
    MCExpr const* fde_length = makeEndMinusStartExpr(
        symbol_start_fde, symbol_end_fde, unit_length_bytes, this);
    UINT src_section_off = MCDWARFMGR_debug_frame_code(this).get_elem_count();
    MCFixup * fix_fde_len = MCDWARFMGR_dwarf_res_mgr(this).
        allocFixup(src_section_off, fde_length, FK_DATA_4);
    MCDWARFMGR_debug_frame_fixups(this).append(fix_fde_len);

    //Length size: 4byte
    appendBytes(0, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_4));

    //CIE Pointer size: 4byte
    src_section_off = MCDWARFMGR_debug_frame_code(this).get_elem_count();
    MCExpr const* cie_exp = MCDWARFMGR_dwarf_res_mgr(this).
            allocMCSymbolRefExpr(cie_mc_symbol);
    MCFixup * fix_cie =  MCDWARFMGR_dwarf_res_mgr(this)
        .allocFixup(src_section_off, cie_exp, FK_DATA_4);
    MCDWARFMGR_debug_frame_fixups(this).append(fix_cie);
    appendBytes(0, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_4));

    //PC Begin size: 8byte
    //In the future,
    //a relocation entry for the beginning of
    //this region will be generated.
    src_section_off = MCDWARFMGR_debug_frame_code(this).get_elem_count();
    MCExpr const* pc_begin = MCDWARFMGR_dwarf_res_mgr(this).
            allocMCSymbolRefExpr(region_frame_info->m_begin);
    MCFixup * fix_pc_begin = MCDWARFMGR_dwarf_res_mgr(this).
        allocFixup(src_section_off, pc_begin, FK_DATA_8);
    MCDWARFMGR_debug_frame_fixups(this).append(fix_pc_begin);
    appendBytes(0, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_8));

    //PC Range size: 8byte
    ASSERT0(MCSYMBOL_is_registered(region_frame_info->m_end) &&
            MCSYMBOL_is_registered(region_frame_info->m_begin));
    UINT64 pc_range_cont = MCSYMBOL_region_offset(region_frame_info->m_end) -
            MCSYMBOL_region_offset(region_frame_info->m_begin);
    appendBytes(pc_range_cont, DEBUG_FRAME_SH_NAME,
                getSizeForFixupKind(FK_DATA_8));

    //Call Frame Instructions
    genCFIInstructions(region_frame_info);
    UINT32 size_current =
        MCDWARFMGR_debug_frame_code(this).get_elem_count() - size_before;

    //Alignment to 8 bytes is required for padding
    CHAR alig_num = 8;
    UINT32 size_alin = (UINT32)xcom::ceil_align(size_current, alig_num);
    appendBytesFromValue(MCDWARFMGR_debug_frame_code(this), 0,
        size_alin - size_current);

    //Register fde end mcsymbol
    MCSYMBOL_region_offset(symbol_end_fde) =
        MCDWARFMGR_debug_frame_code(this).get_elem_count();
    MCSYMBOL_is_registered(symbol_end_fde) = true;
    return;
}


void MCDwarfMgr::genFrameBinary()
{
    bool first_cie = true;
    MCDwarfFrameRegionInfo * mc_dwarf_frame_info = nullptr;
    Region const* region_ptr = nullptr;
    MCSymbol const* cie_symbo = nullptr;
    xcom::TMapIter<Region const*, MCDwarfFrameRegionInfo*>
        mc_dwarf_frame_info_iter;

    //The current file does not have a function region.
    if (isNullRegion()) { return; }

    for (region_ptr = MCDWARFMGR_region_frame_info(this).get_first(
         mc_dwarf_frame_info_iter, &mc_dwarf_frame_info);
         !mc_dwarf_frame_info_iter.end();
         region_ptr = MCDWARFMGR_region_frame_info(this).get_next(
         mc_dwarf_frame_info_iter, &mc_dwarf_frame_info)) {
        if (first_cie) {
            first_cie = false;

            //Return the position of the header of the debug_frame section
            //provided for use by the header of each region.
            cie_symbo = genFrameCIE((Region*)region_ptr);
        }
        ASSERT0(cie_symbo);
        genFrameFDE(cie_symbo, mc_dwarf_frame_info,
            (Region*)region_ptr);
    }
}


void MCDwarfMgr::encode(INT64 line_delta, UINT64 addr_delta,
    OUT Vector<CHAR> & os)
{
    UINT64 temp, opcode;
    bool need_copy = false;

    //The maximum address skip amount that can be encoded with a special op.
    //This is determined by the range of values that can be represented
    //using special opcodes in the DWARF line number program.
    //Given a special op, return the address skip amount (in units of
    //DWARF2_LINE_MIN_INSN_LENGTH).
    //For example, if m_dwarf2_line_opcode_base is 10 and
    //m_dwarf2_line_range is 4, the calculation will be:
    //max_special_addr_delta = (255 - 10) / 4 = 61.25, which means the
    //maximum address skip amount that can be encoded is 61.
    UINT64 max_special_addr_delta =
        (DWARF_MAX_OPCODE - m_dwarf_line_para.m_dwarf2_line_opcode_base) /
        m_dwarf_line_para.m_dwarf2_line_range;

    //A LineDelta of INT64_MAX is a signal that this is actually a
    //DW_LNE_end_sequence. We cannot use special opcodes here,
    //since we want the
    //end_sequence to emit the matrix entry.
    if (line_delta == DW_LNE_END_SEQUENCE_FLAG) {
        if (addr_delta == max_special_addr_delta) {
            os.append(CHAR(DW_LNS_const_add_pc));
        } else if (addr_delta) {
            os.append(CHAR(DW_LNS_advance_pc));
            xcom::encodeULEB128(addr_delta, os);
        }
        os.append(CHAR(DW_LNS_extended_op));
        os.append(CHAR(1));
        os.append(CHAR(DW_LNE_end_sequence));
        return;
    }

    //Bias the line delta by the base.
    temp = line_delta - m_dwarf_line_para.m_dwarf2_line_base;

    //If the line increment is out of range of a special opcode,
    //we must encode
    //it with DW_LNS_advance_line.
    if (temp >= m_dwarf_line_para.m_dwarf2_line_range ||
        temp + m_dwarf_line_para.m_dwarf2_line_opcode_base > DWARF_MAX_OPCODE) {
        os.append(CHAR(DW_LNS_advance_line));
        xcom::encodeSLEB128(line_delta, os);
        line_delta = 0;
        temp = 0 - m_dwarf_line_para.m_dwarf2_line_base;
        need_copy = true;
    }

    //Use DW_LNS_copy instead of a "line +0, addr +0" special opcode.
    if (line_delta == 0 && addr_delta == 0) {
        os.append(CHAR(DW_LNS_copy));
        return;
    }

    //Bias the opcode by the special opcode base.
    temp += m_dwarf_line_para.m_dwarf2_line_opcode_base;

    //Avoid overflow when addr_delta is large.
    if (addr_delta < DWARF_OPCODE_OVERFLOW_THRESHOLD +
        max_special_addr_delta) {
        //Try using a special opcode.
        opcode = temp + addr_delta * m_dwarf_line_para.m_dwarf2_line_range;
        if (opcode <= DWARF_MAX_OPCODE) {
            os.append(CHAR(opcode));
            return;
        }

        //Try using DW_LNS_const_add_pc followed by special op.
        opcode = temp + (addr_delta - max_special_addr_delta) *
                 m_dwarf_line_para.m_dwarf2_line_range;
        if (opcode <= DWARF_MAX_OPCODE) {
            os.append(CHAR(DW_LNS_const_add_pc));
            os.append(CHAR(opcode));
            return;
        }
    }

    //Otherwise use DW_LNS_advance_pc.
    os.append(DW_LNS_advance_pc);
    xcom::encodeULEB128(addr_delta, os);
    if (need_copy) {
        os.append(CHAR(DW_LNS_copy));
    } else {
        ASSERTN(temp <= DWARF_MAX_OPCODE , ("Buggy special opcode encoding."));
        os.append(CHAR(temp));
    }
}


void MCDwarfMgr::genLineAddr(INT64 line_delta, UINT64 addr_delta)
{
    MCDWARFMGR_char_out_stream(this).clean();
    encode(line_delta, addr_delta, MCDWARFMGR_char_out_stream(this));
    MCDWARFMGR_debug_line_code(this).
        append((BYTE*)MCDWARFMGR_char_out_stream(this).get_vec(),
               MCDWARFMGR_char_out_stream(this).get_elem_count());
}


void MCDwarfMgr::genDwarfSetLineAddr(INT64 line_delta, MCSymbol const* label,
                                     UINT32 pointer_size)
{
    ASSERT0(label);

    //emit the sequence to set the address
    MCDWARFMGR_debug_line_code(this).append(DW_LNS_extended_op);
    genULEB128AndToV(pointer_size + 1, MCDWARFMGR_debug_line_code(this));
    MCDWARFMGR_debug_line_code(this).append(DW_LNE_set_address);

    //Set references to the beginning of functions.
    UINT32 src_section_off = MCDWARFMGR_debug_line_code(this).get_elem_count();
    MCExpr const* func_start_exp = MCDWARFMGR_dwarf_res_mgr(this).
            allocMCSymbolRefExpr(label);
    MCFixup * fix_func_start = MCDWARFMGR_dwarf_res_mgr(this).
        allocFixup(src_section_off, func_start_exp, FK_DATA_8);
    MCDWARFMGR_debug_line_fixups(this).append(fix_func_start);
    appendBytes(0, DEBUG_LINE_SH_NAME,
                getSizeForFixupKind(FK_DATA_8));
    genLineAddr(line_delta, 0);
}


void MCDwarfMgr::genDwarfAdvanceLineAddr(INT64 line_delta,
                                         MCSymbol const* last_label,
                                         MCSymbol const* label,
                                         UINT32 pointer_size)
{
    ASSERT0(label);
    if (last_label == nullptr) {
        genDwarfSetLineAddr(line_delta, label, pointer_size);
        return;
    }
    ASSERT0(MCSYMBOL_region(last_label) == MCSYMBOL_region(label));
    UINT64 addr_delta =
        MCSYMBOL_region_offset(label) - MCSYMBOL_region_offset(last_label);
    genLineAddr(line_delta, addr_delta);
}


void MCDwarfMgr::genDwarfLineTable(Vector<MCDwarfLineEntry*> * line_entry_v)
{
    ASSERT0(line_entry_v);
    UINT32 file_num = 1;
    UINT32 last_line = 1;
    UINT32 column = 0;
    UINT32 flags = DWARF2_LINE_DEFAULT_IS_STMT ? DWARF2_FLAG_IS_STMT : 0;
    MCSymbol * last_label = nullptr;
    UINT64 loc_v_num = line_entry_v->get_elem_count();
    CHAR const point_size = 8;
    for (UINT i = 0; i < loc_v_num; ++i) {
        MCDwarfLineEntry * entry = (*line_entry_v)[i];
        INT64 line_delta = MCDWARFLOC_line(entry) - last_line;
        if (file_num != MCDWARFLOC_file_index(entry)) {
            file_num = MCDWARFLOC_file_index(entry);
            MCDWARFMGR_debug_line_code(this).append(DW_LNS_set_file);
            genULEB128AndToV(file_num, MCDWARFMGR_debug_line_code(this));
        }
        if (column != MCDWARFLOC_column(entry)) {
            column = MCDWARFLOC_column(entry);
            MCDWARFMGR_debug_line_code(this).append(DW_LNS_set_column);
            genULEB128AndToV(column, MCDWARFMGR_debug_line_code(this));
        }
        if ((MCDWARFLOC_flags(entry) ^ flags) & DWARF2_FLAG_IS_STMT) {
            flags = MCDWARFLOC_flags(entry);
            MCDWARFMGR_debug_line_code(this).append(DW_LNS_negate_stmt);
        }
        if (MCDWARFLOC_flags(entry) & DWARF2_FLAG_PROLOGUE_END) {
            MCDWARFMGR_debug_line_code(this).append(DW_LNS_prologue_end);
        }
        MCSymbol const* label = MCDWARFLINEENTRY_label(entry);
        genDwarfAdvanceLineAddr(line_delta, last_label, label, point_size);
        last_line = MCDWARFLOC_line(entry);
        last_label = (MCSymbol*)label;
    }

    //Emit a DW_LNE_end_sequence for the end of the section.
    //Use the section end label to
    //compute the address delta and use INT64_MAX
    //as the line delta which is the signal that this is actually a
    //DW_LNE_end_sequence.
    ASSERT0(MCSYMBOL_region(last_label));
    MCSymbol const* section_end =
        getMCSymbolRegionEnd(MCSYMBOL_region(last_label));
    genDwarfAdvanceLineAddr(DW_LNE_END_SEQUENCE_FLAG,
                            last_label, section_end, point_size);
}


void MCDwarfMgr::genV2FileDirTables()
{
    for (UINT32 i = 0; i < MCDWARFMGR_mc_dwarf_dirs(this).get_elem_count();
         ++i) {
        UINT32 buf_len = MCDWARFMGR_mc_dwarf_dirs(this)[i]->getBufLen() - 1;
        CHAR const* buf = MCDWARFMGR_mc_dwarf_dirs(this)[i]->getBuf();

        //The DirectoryName
        MCDWARFMGR_debug_line_code(this).append((BYTE*)buf, buf_len);

        //its null terminator.
        MCDWARFMGR_debug_line_code(this).append(0);
    }

    //Terminate the directory list.
    MCDWARFMGR_debug_line_code(this).append(0);

    //Second the file table.
    for (UINT32 i = 0; i < MCDWARFMGR_mc_dwarf_files(this).get_elem_count();
         ++i) {
        if (MCDWARFMGR_mc_dwarf_files(this)[i].m_name == nullptr) {
            continue;
        }
        UINT32 buf_len =
            MCDWARFMGR_mc_dwarf_files(this)[i].m_name->getBufLen() - 1;
        CHAR const* buf = MCDWARFMGR_mc_dwarf_files(this)[i].m_name->getBuf();

        //The file name
        MCDWARFMGR_debug_line_code(this).append((BYTE*)buf, buf_len);

        //its null terminator.
        MCDWARFMGR_debug_line_code(this).append(0);

        UINT dir_num = MCDWARFMGR_mc_dwarf_files(this)[i].m_dir_index;
        genULEB128AndToV(dir_num, MCDWARFMGR_debug_line_code(this));

        //Last modification timestamp (always 0).
        MCDWARFMGR_debug_line_code(this).append(0);

        //File size (always 0).
        MCDWARFMGR_debug_line_code(this).append(0);
    }
    MCDWARFMGR_debug_line_code(this).append(0); //Terminate the file list
}


MCSymbol * MCDwarfMgr::genLineHeader(Region * region_ptr)
{
    static CHAR const standard_opcode_Lengths[] = {
        0, //length of DW_LNS_copy
        1, //length of DW_LNS_advance_pc
        1, //length of DW_LNS_advance_line
        1, //length of DW_LNS_set_file
        1, //length of DW_LNS_set_column
        0, //length of DW_LNS_negate_stmt
        0, //length of DW_LNS_set_basic_block
        0, //length of DW_LNS_const_add_pc
        1, //length of DW_LNS_fixed_advance_pc
        0, //length of DW_LNS_set_prologue_end
        0, //length of DW_LNS_set_epilogue_begin
        1  //DW_LNS_set_isa
    };
    LabelInfo * label_line_start = region_ptr->genPragmaLabel("line_start");
    Region const* re =
        getSectionRegion(REGION_region_mgr(region_ptr), DEBUG_LINE_SH_NAME);
    MCSymbol * symbol_start_line =
        (MCSymbol*)(this->createVectorMCSymbol(re, label_line_start));
    MCSYMBOL_region_offset(symbol_start_line) = 0;
    MCSYMBOL_is_registered(symbol_start_line) = true;
    LabelInfo * label_line_end = region_ptr->genPragmaLabel("line_end");
    MCSymbol * symbol_end_line =
        (MCSymbol*)(this->createVectorMCSymbol(re, label_line_end));

    //Length size: 4byte
    UINT8 unit_length_bytes = 4; //consider DWARF32 only
    UINT8 offset_size = 4;
    MCExpr const* line_length_exp = makeEndMinusStartExpr(
        symbol_start_line, symbol_end_line, unit_length_bytes, this);
    UINT src_section_off = MCDWARFMGR_debug_line_code(this).get_elem_count();

    //The length field does not include itself
    MCFixup * fix_fde_len = MCDWARFMGR_dwarf_res_mgr(this).
        allocFixup(src_section_off, line_length_exp, FK_DATA_4);
    MCDWARFMGR_debug_line_fixups(this).append(fix_fde_len);
    appendBytesFromValue(MCDWARFMGR_debug_line_code(this), 0, offset_size);

    //Next 2 bytes is the Version.
    //version is 2 of DWARF32
    INT16 version = 2;
    appendBytesFromValue(MCDWARFMGR_debug_line_code(this), version, 2);

    //Next 4 bytes is the line prologue size.
    //Create a symbol for the end of the prologue
    //(to be set when we get there)
    LabelInfo * label_line_prologue_end =
        region_ptr->genPragmaLabel("label_line_prologue_end");
    MCSymbol * symbol_prologue_end_line =
        (MCSymbol*)(this->createVectorMCSymbol(re, label_line_prologue_end));

    //The size of the prologue does not include the preceding length
    //version, and its own size.
    UINT32 current_p = unit_length_bytes + version + offset_size;
    MCExpr const* line_length_prologue = makeEndMinusStartExpr(
        symbol_start_line, symbol_prologue_end_line, current_p, this);
    src_section_off = MCDWARFMGR_debug_line_code(this).get_elem_count();
    MCFixup * fix_prologue_len = MCDWARFMGR_dwarf_res_mgr(this).
        allocFixup(src_section_off, line_length_prologue, FK_DATA_4);
    MCDWARFMGR_debug_line_fixups(this).append(fix_prologue_len);
    appendBytesFromValue(MCDWARFMGR_debug_line_code(this), 0, offset_size);

    //Parameters of the state machine, size: 1byte
    UINT8 InstAlignment = 1;
    MCDWARFMGR_debug_line_code(this).append(InstAlignment);

    //DWARF2_LINE_DEFAULT_IS_STMT 1byte
    MCDWARFMGR_debug_line_code(this).append(DWARF2_LINE_DEFAULT_IS_STMT);

    //m_dwarf2_line_base 1byte
    MCDWARFMGR_debug_line_code(this).
        append(m_dwarf_line_para.m_dwarf2_line_base);
    MCDWARFMGR_debug_line_code(this).
        append(m_dwarf_line_para.m_dwarf2_line_range);
    UINT8 size_standard_opcode_Lengths =
        sizeof(standard_opcode_Lengths) / sizeof(standard_opcode_Lengths[0]);
    MCDWARFMGR_debug_line_code(this).append(size_standard_opcode_Lengths + 1);

    //Standard opcode lengths
    for (UINT i = 0; i < size_standard_opcode_Lengths; ++i) {
        MCDWARFMGR_debug_line_code(this).append(standard_opcode_Lengths[i]);
    }

    //Process file
    genV2FileDirTables();

    //This is the end of the prologue,
    //so set the value of the symbol at the
    //end of the prologue (that was used in a previous expression).
    MCSYMBOL_region_offset(symbol_prologue_end_line) =
        MCDWARFMGR_debug_line_code(this).get_elem_count();
    MCSYMBOL_is_registered(symbol_prologue_end_line) = true;
    return symbol_end_line;
}


void MCDwarfMgr::genLineBinary()
{
    Region const* region_ptr = nullptr;
    xcom::Vector<MCDwarfLineEntry*> * mc_dwarf_line_info;
    xcom::TMapIter<Region*, xcom::Vector<MCDwarfLineEntry*>*>
        mc_dwarf_line_info_iter;
    bool header_gen = true;

    //The current.xxx does not have a function region.
    if (isNullRegion()) { return; }
    MCSymbol * symbol_end_line = nullptr;
    for (region_ptr = MCDWARFMGR_region_line_info(this).get_first(
         mc_dwarf_line_info_iter, &mc_dwarf_line_info);
         !mc_dwarf_line_info_iter.end();
         region_ptr = MCDWARFMGR_region_line_info(this).get_next(
         mc_dwarf_line_info_iter,&mc_dwarf_line_info)) {
        if (header_gen) {
            header_gen = false;
            symbol_end_line = genLineHeader((Region*)region_ptr);
        }
        genDwarfLineTable(mc_dwarf_line_info);
    }

    //This is the end of the section,
    //so set the value of the symbol at the end
    //of this section (that was used in a previous expression).
    ASSERT0(symbol_end_line);
    MCSYMBOL_region_offset(symbol_end_line) =
        MCDWARFMGR_debug_line_code(this).get_elem_count();
    MCSYMBOL_is_registered(symbol_end_line) = true;
}


//
//START DwarfResMgr
//
void DwarfResMgr::init()
{
    if (m_pool == nullptr) {
        m_pool = smpoolCreate(64, MEM_COMM);
    }
    ASSERT0(m_pool);
}


void * DwarfResMgr::xmalloc(size_t size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset(p, 0, size);
    return p;
}


MCExpr const* DwarfResMgr::allocMCBinaryExpr(MCBinaryExpr::Opcode op,
                                             MCExpr const* lhs,
                                             MCExpr const* rhs)
{
    ASSERT0(lhs && rhs);
    MCBinaryExpr * expr = (MCBinaryExpr*)xmalloc(sizeof(MCBinaryExpr));
    ASSERT0(expr);
    MCBINARYEXPR_lhs(expr) = lhs;
    MCBINARYEXPR_rhs(expr) = rhs;
    MCBINARYEXPR_opcode(expr) = op;
    MCEXPR_kind(expr) = MCExpr::BINARY;
    return (MCExpr const*)expr;
}


MCExpr const* DwarfResMgr::allocMCSymbolRefExpr(MCSymbol const* mc_symbol)
{
    ASSERT0(mc_symbol);
    MCSymbolRefExpr * expr = (MCSymbolRefExpr*)xmalloc(sizeof(MCSymbolRefExpr));
    ASSERT0(expr);
    MCSYMBOLREFEXPR_mc_symbol(expr) = mc_symbol;
    MCEXPR_kind(expr) = MCExpr::SYMBOLREF;
    return (MCExpr const*)expr;
}


MCExpr const* DwarfResMgr::allocMCConstantExpr(INT64 value) {

    MCConstantExpr * expr = (MCConstantExpr*)xmalloc(sizeof(MCConstantExpr));
    MCCONSTANTEXPR_value(expr) = value;
    ASSERT0(expr);
    MCEXPR_kind(expr) = MCExpr::CONSTANT;
    return (MCExpr const*)expr;
}


MCFixup * DwarfResMgr::allocFixup(UINT offset, MCExpr const* value,
                                  MCFixupKind kind)
{
    MCFixup * fixup_p = (MCFixup*)xmalloc(sizeof(MCFixup));
    ASSERT0(fixup_p && value);
    MCFIXUP_offset(fixup_p) = offset;
    MCFIXUP_value(fixup_p) = value;
    MCFIXUP_kind(fixup_p) = kind;
    return fixup_p;
}


MCSymbol * DwarfResMgr::allocMCSymbol()
{
    MCSymbol * mc_symbol_p = (MCSymbol*)xmalloc(sizeof(MCSymbol));
    ASSERT0(mc_symbol_p);
    return mc_symbol_p;
}


MCCFIInstruction * DwarfResMgr::allocCFIDefCfa(MCSymbol const* l, UINT32 r,
                                               INT32 offset)
{
    MCCFIInstruction * cfi_ins_p = (MCCFIInstruction*)xmalloc
        (sizeof(MCCFIInstruction));
    ASSERT0(cfi_ins_p);
    MCCFIINSTRUCTION_operation(cfi_ins_p) = MCCFIInstruction::OPDEFCFA;
    MCCFIINSTRUCTION_label(cfi_ins_p) = l;
    MCCFIINSTRUCTION_register(cfi_ins_p) = r;
    MCCFIINSTRUCTION_offset(cfi_ins_p) = offset;
    return cfi_ins_p;
}


MCCFIInstruction * DwarfResMgr::allocSameValue(MCSymbol const* l, UINT32 r)
{
    MCCFIInstruction * cfi_ins_p = (MCCFIInstruction*)xmalloc
        (sizeof(MCCFIInstruction));
    ASSERT0(cfi_ins_p);
    MCCFIINSTRUCTION_operation(cfi_ins_p) = MCCFIInstruction::OPSAMEVALUE;
    MCCFIINSTRUCTION_label(cfi_ins_p) = l;
    MCCFIINSTRUCTION_register(cfi_ins_p) = r;
    MCCFIINSTRUCTION_offset(cfi_ins_p) = 0;
    return cfi_ins_p;
}


MCCFIInstruction * DwarfResMgr::allocOffset(MCSymbol const* l, UINT32 r,
                                            INT32 o)
{
    MCCFIInstruction * cfi_ins_p = (MCCFIInstruction*)xmalloc
        (sizeof(MCCFIInstruction));
    ASSERT0(cfi_ins_p);
    MCCFIINSTRUCTION_operation(cfi_ins_p) = MCCFIInstruction::OPOFFSET;
    MCCFIINSTRUCTION_label(cfi_ins_p) = l;
    MCCFIINSTRUCTION_register(cfi_ins_p) = r;
    MCCFIINSTRUCTION_offset(cfi_ins_p) = o;
    return cfi_ins_p;
}


MCCFIInstruction * DwarfResMgr::allocRestore(MCSymbol const* l, UINT32 r)
{
    MCCFIInstruction * cfi_ins_p = (MCCFIInstruction*)xmalloc
        (sizeof(MCCFIInstruction));
    ASSERT0(cfi_ins_p);
    MCCFIINSTRUCTION_operation(cfi_ins_p) = MCCFIInstruction::OPRESTORE;
    MCCFIINSTRUCTION_label(cfi_ins_p) = l;
    MCCFIINSTRUCTION_register(cfi_ins_p) = r;
    MCCFIINSTRUCTION_offset(cfi_ins_p) = 0;
    return cfi_ins_p;
}


MCCFIInstruction * DwarfResMgr::allocDefCfaOffset(MCSymbol const* l,
                                                  INT32 o)
{
    MCCFIInstruction * cfi_ins_p = (MCCFIInstruction*)xmalloc
        (sizeof(MCCFIInstruction));
    ASSERT0(cfi_ins_p);
    MCCFIINSTRUCTION_operation(cfi_ins_p) = MCCFIInstruction::OPDEFCFAOFFSET;
    MCCFIINSTRUCTION_label(cfi_ins_p) = l;
    MCCFIINSTRUCTION_register(cfi_ins_p) = 0;
    MCCFIINSTRUCTION_offset(cfi_ins_p) = o;
    return cfi_ins_p;
}


MCDwarfLineEntry * DwarfResMgr::allocLineEntry(MCSymbol const* l)
{
    MCDwarfLineEntry * line_entry_p = (MCDwarfLineEntry*)xmalloc
        (sizeof(MCDwarfLineEntry));
    ASSERT0(line_entry_p);
    MCDWARFLINEENTRY_label(line_entry_p) = l;
    return line_entry_p;
}


xcom::StrBuf * DwarfResMgr::allocStr(UINT init_size)
{
    xcom::StrBuf * str_ptr = new xcom::StrBuf(init_size);
    ASSERT0(str_ptr);
    m_str_mgr.append_tail(str_ptr);
    return str_ptr;
}


TMap<Region const*, MCSymbol*> * DwarfResMgr::allocMapRegionVar()
{
    TMap<Region const*, MCSymbol*> * sym_v =
        new TMap<Region const*, MCSymbol*>();
    ASSERT0(sym_v);
    m_map_region_mc_symbol_mgr.append_tail(sym_v);
    return sym_v;
}


Vector<MCCFIInstruction*> * DwarfResMgr::allocCFIInfoVector()
{
    Vector<MCCFIInstruction*> * instructions = new Vector<MCCFIInstruction*>();
    ASSERT0(instructions);
    m_cfi_info_vector_mgr.append_tail(instructions);
    return instructions;
}


MCDwarfFrameRegionInfo* DwarfResMgr::allocFrameRegionInfo()
{
    MCDwarfFrameRegionInfo * frame_info_p = new MCDwarfFrameRegionInfo();
    ASSERT0(frame_info_p);
    m_region_frame_info_mgr.append_tail(frame_info_p);
    return frame_info_p;
}


Vector<MCDwarfLineEntry*> * DwarfResMgr::allocLineEntryVector()
{
    Vector<MCDwarfLineEntry*> * line_entry_p = new Vector<MCDwarfLineEntry*>();
    ASSERT0(line_entry_p);
    m_line_entry_mgr.append_tail(line_entry_p);
    return line_entry_p;
}



Vector<LabelInfo const*> * DwarfResMgr::allocLabelVector()
{
    Vector<LabelInfo const*> * label_p = new Vector<LabelInfo const*>();
    ASSERT0(label_p);
    m_ret_hint_map_region_info_mgr.append_tail(label_p);
    return label_p;
}


void DwarfResMgr::destroy()
{
    if (m_pool != nullptr) {
        smpoolDelete(m_pool);
        m_pool = nullptr;
    }

    for (StrBuf const* r = m_str_mgr.get_head();
         r != nullptr; r = m_str_mgr.get_next()) {
        if (r != nullptr) {
            delete r;
        }
    }
    m_str_mgr.destroy();

    for (TMap<Region const*, MCSymbol*> * v =
         m_map_region_mc_symbol_mgr.get_head();
         v != nullptr; v = m_map_region_mc_symbol_mgr.get_next()) {
        if (v != nullptr) {
            delete v;
        }
    }
    m_map_region_mc_symbol_mgr.destroy();

    for (Vector<MCCFIInstruction*> * v = m_cfi_info_vector_mgr.get_head();
         v != nullptr; v = m_cfi_info_vector_mgr.get_next()) {
        if (v != nullptr) {
            delete v;
        }
    }
    m_cfi_info_vector_mgr.destroy();

    for (MCDwarfFrameRegionInfo * v = m_region_frame_info_mgr.get_head();
         v != nullptr; v = m_region_frame_info_mgr.get_next()) {
        if (v != nullptr) {
            delete v;
        }
    }
    m_region_frame_info_mgr.destroy();

    for (Vector<MCDwarfLineEntry*> * v = m_line_entry_mgr.get_head();
         v != nullptr; v = m_line_entry_mgr.get_next()) {
        if (v != nullptr) {
            delete v;
        }
    }
    m_line_entry_mgr.destroy();

    for (Vector<LabelInfo const*> * v = m_ret_hint_map_region_info_mgr.
         get_head(); v != nullptr; v = m_ret_hint_map_region_info_mgr.
         get_next()) {
        if (v != nullptr) {
            delete v;
        }
    }
    m_ret_hint_map_region_info_mgr.destroy();
}


void MCDwarfMgr::destroy()
{
    m_mc_dwarf_dirs.clean();
    m_mc_dwarf_files.clean();
    m_debug_info_fixups.clean();
    m_map_symbol.clean();
    m_map_symbol_var.clean();
    m_region_frame_info.clean();
    m_region_line_info.clean();
    m_debug_rangse_fixups.clean();
    m_debug_frame_fixups.clean();
    m_debug_line_fixups.clean();
}
} // namespace xoc
