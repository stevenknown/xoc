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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "elfinc.h"
#define STR_UNDEF "" //Common used empty string.
#define PHASE(a)
#define RET_ERR(det,info,retcode) if (det) { PHASE(info); return (retcode); }

namespace elf {

//The option that control the format of generated ELF.
// -elf-device option: Output .o file that need to be relocated with
//   other device file.
// -elf-fatbin option: Output fatbin file that directly executes on device.
//  It will call ld linked multi-file and other external .so file.
ELFOpt g_elf_opt;

typedef struct {
    CHAR const* colname; //the name of the column.
    CHAR const* colfmt; //the print-format of value to the column.
    UINT colwidth; //the maximum print byte width to the column.
} TabCol;


static SectionDesc const g_section_desc[] = {
    //Name(enum), Sect type,  Program header,
    //Flags,      Addr align, Entry size,     Name(str)
    { SH_TYPE_UNDEF, S_UNDEF, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".sh_name_undef" },

    { SH_TYPE_EMPTY, S_UNDEF, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".sh_name_empty" },

    { SH_TYPE_SHSTR, S_STRTAB,  PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, SHDR_1B_ALIGN, ELF_VAL_UNDEF, ".shdr_strtab" },

    { SH_TYPE_SYMSTR, S_STRTAB, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, SHDR_1B_ALIGN, SHDR_SYM_BYTE, ".strtab" },

    { SH_TYPE_TEXT, S_PROGBITS, PH_TYPE_CODE,
      SF_ALLOC|SF_EXECINSTR, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".text" },

    { SH_TYPE_SBSS, S_NOBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".sbss" },

    { SH_TYPE_SDATA, S_PROGBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".sdata" },

    { SH_TYPE_BSS, S_NOBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".bss" },

    { SH_TYPE_DATA, S_PROGBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".data" },

    { SH_TYPE_SYMTAB, S_SYMTAB, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, SHDR_8B_ALIGN, ELF_VAL_UNDEF, ".symtab" },

    { SH_TYPE_SUBTEXT, S_UNDEF, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".text." },

    { SH_TYPE_RELA, S_UNDEF, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".rela.text." },

    { SH_TYPE_CONST, S_PROGBITS, PH_TYPE_DATA,
      SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".const" },

    { SH_TYPE_GOT, S_PROGBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, SHDR_GOT_ALIGN, ELF_VAL_UNDEF, ".got" },

    { SH_TYPE_RELA_DYN, S_RELA, PH_TYPE_CODE,
      SF_ALLOC, SHDR_8B_ALIGN, ELF_VAL_UNDEF, ".rela.dyn" },

    { SH_TYPE_DYNSYM, S_DYNSYM, PH_TYPE_CODE,
      SF_ALLOC, SHDR_SYM_ALIGN, ELF_VAL_UNDEF, ".dynsym" },

    { SH_TYPE_FINI, S_NOBITS, PH_TYPE_CODE,
      SF_ALLOC|SF_EXECINSTR, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".fini" },

    { SH_TYPE_PREINIT_ARRAY, S_NOBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".preinit_array" },

    { SH_TYPE_DYNAMIC, S_DYNAMIC, PH_TYPE_DYNAMIC,
      SF_WRITE|SF_ALLOC, SHDR_DYNAMIC_ALIGN, ELF_VAL_UNDEF, ".dynamic" },

    { SH_TYPE_INTERP, S_PROGBITS, PH_TYPE_CODE,
      SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".interp" },

    { SH_TYPE_DL_TDATA, S_PROGBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".dl_tdata" },

    { SH_TYPE_RODATA, S_PROGBITS, PH_TYPE_CODE,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".rodata" },

    { SH_TYPE_RODATA1, S_PROGBITS, PH_TYPE_CODE,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".rodata1" },

    { SH_TYPE_LDM, S_PROGBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".ldm" },

    { SH_TYPE_DEBUG_INFO, S_PROGBITS, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".debug_info" },

    { SH_TYPE_DEBUG_LINE, S_PROGBITS, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".debug_line" },

    { SH_TYPE_DEBUG_ABBREV, S_PROGBITS, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".debug_abbrev" },

    { SH_TYPE_DEBUG_ARANGES, S_PROGBITS, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".debug_aranges" },

    { SH_TYPE_DEBUG_RANGES, S_PROGBITS, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".debug_ranges" },

    { SH_TYPE_DEBUG_STR, S_PROGBITS, PH_TYPE_UNDEF,
      SF_MERGE | SF_STRINGS, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".debug_str" },

    { SH_TYPE_DEBUG_FRAME, S_PROGBITS, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".debug_frame" },

    { SH_TYPE_DEBUG_LOC, S_PROGBITS, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".debug_loc" },

    { SH_TYPE_EH_FRAME, S_PROGBITS, PH_TYPE_DATA,
      SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".eh_frame" },

    #include "sect_desc_ext.impl"

    { SH_TYPE_MAX_NUM, S_UNDEF, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".sh_max_num" }
};


//Record Sym of section name.
static Sym const* g_section_name_sym[SH_TYPE_MAX_NUM] = {0};


void ELFMgr::initSectionInfo()
{
    for (UINT i = 0;  i < SH_TYPE_MAX_NUM; i++) {
        g_section_name_sym[i] = addToSymTab(g_section_desc[i].m_desc_name_str);
        m_sect_name_type_map.set(g_section_name_sym[i], (SECTION_TYPE)i);
    }
}


//The function only invoked at debug mode.
bool checkSectDesc()
{
    for (UINT i = SH_TYPE_UNDEF; i < SH_TYPE_MAX_NUM; i++) {
        ASSERT0(i == (UINT)SECTDESC_code(i));
    }
    UINT num = sizeof(g_section_desc) / sizeof(g_section_desc[0]);
    ASSERTN_DUMMYUSE(num - 1 == SH_TYPE_MAX_NUM,
        ("miss section desc declaration"));
    return true;
}


static SECTION_TYPE const g_dynamic_section_desc[] = {
    SH_TYPE_RELA_DYN,
    SH_TYPE_SYMTAB,
    SH_TYPE_SYMSTR,
};


static void formatTabColInfo(TabCol * tabcol, UINT tabcolnum,
                             OUT StrBuf & colformat,
                             OUT StrBuf & coltitle)
{
    #define CTI_BUFLEN 128
    CHAR tmp[CTI_BUFLEN];
    colformat.strcat("\n");
    for (UINT i = 0; i < tabcolnum; i++) {
        tmp[0] = 0;
        //::snprintf(tmp, 128, "%%-%ds", tabcol[i].colwidth);
        xcom::xstrcat(tmp, CTI_BUFLEN, "%%-%ds", tabcol[i].colwidth);
        coltitle.strcat(tmp, tabcol[i].colname);
        colformat.strcat(tabcol[i].colfmt);
    }
}


//Symbol name hash function.
//The function accepts a symbol name and returns a value that may be used to
//compute a bucket index.
//The same table layout is used for both the 32-bit and 64-bit file class.
//http://osr507doc.sco.com/en/topics/ELF_hashtbl.html#ELF_symb_hash_tabl
//explain the hash table organization, but they are not part of the
//specification.
static ULONG elf_string_hash(UCHAR const* name)
{
    DUMMYUSE(elf_string_hash);
    ULONG h = 0, g;
    while (*name) {
        h = (h << 4) + *name++;
        if ((g = (h & 0xf0000000))) {
            h ^= g >> 24;
        }
        h &= ~g;
    }
    return h;
}


//Since some symbols have address alignment requirements, the current address
//needs to be aligned upwards to serve as the new address of the current
//symbol. The gap between the aligned address and the original address needs to
//be padded. This function calculates the size of this gap.
static UINT calculatePadZero(UINT aligned_addr, UINT current_addr)
{
    ASSERT0(aligned_addr >= current_addr);
    return aligned_addr - current_addr;
}


ELFSectionInfo::ELFSectionInfo() {
    m_has_sbss = false;
    m_has_sdata = false;
    m_has_bss = false;
    m_has_data = false;
    m_has_spm = false;
    m_has_const = false;

    m_has_debug_abbrev = false;
    m_has_debug_info = false;
    m_has_debug_ranges = false;
    m_has_debug_str = false;
    m_has_debug_line = false;
    m_has_debug_frame = false;

    m_sbss_align = 0;
    m_sdata_align = 0;
    m_bss_align = 0;
    m_data_align = 0;
    m_spm_align = 0;
    m_const_align = 0;

    m_debug_abbrev_align = 0;
    m_debug_info_align = 0;
    m_debug_ranges_align = 0;
    m_debug_str_align = 0;
    m_debug_line_align = 0;
    m_debug_frame_align = 0;

    m_shdr_num = 0;
}


SECTION_TYPE ELFMgr::judgeSymbolSection(Var const* var)
{
    if (var->is_func()) {
        return (!var->is_global() || var->is_extern()) ?
            SH_TYPE_UNDEF : SH_TYPE_TEXT;
    }

    if (var->getStorageSpace() == SS_SPM) { return SH_TYPE_SPM; }

    if (var->is_readonly()) { return SH_TYPE_CONST; }

    if (var->is_vector() || var->is_string() || var->is_mc()) {
        return var->hasInitVal() ? SH_TYPE_DATA : SH_TYPE_BSS;
    }

    return var->hasInitVal() ? SH_TYPE_SDATA : SH_TYPE_SBSS;
}


UINT ELFSectionInfo::getSectionAlign(SECTION_TYPE sect)
{
    switch (sect) {
    //.sbss
    case SH_TYPE_SBSS: return getBssAlign();
    //.sdata
    case SH_TYPE_SDATA: return getSdataAlign();
    //.bss
    case SH_TYPE_BSS: return getBssAlign();
    //.data
    case SH_TYPE_DATA: return getDataAlign();
    //.spm
    case SH_TYPE_SPM: return getSpmAlign();
    //.const
    case SH_TYPE_CONST: return getConstAlign();
    default: UNREACHABLE(); //Variable is not available.
    }
    return (UINT)0;
}


CHAR const* ELFMgr::getFileTypeName() const
{
    switch (m_elf_hdr.e_type) {
    case ET_NONE: return "no file type";
    case ET_REL: return "relocatable file";
    case ET_EXEC: return "executable file";
    case ET_DYN: return "dynamic link file";
    case ET_CORE: return "core file";
    case ET_LOPROC: return "processor specific file types";
    case ET_HIPROC: return "processor specific file types";
    default:;
    }
    return nullptr;
}


CHAR const* ELFMgr::getEndianName() const
{
    return m_elf_hdr.e_data == ED_LITTLE ?
        "little-endian" :
        m_elf_hdr.e_data == ED_BIG ? "big-endian" : "unknown-endian";
}


CHAR const* ELFMgr::getClassName() const
{
    return m_elf_hdr.e_class == EC_32BIT ?
        "32-bit" :
        m_elf_hdr.e_class == EC_64BIT ? "64-bit" : "unknown-bit";
}


static void formatMagicNum(ELFHdr const& h, OUT StrBuf & buf)
{
    for (UINT i = 0; i < EI_MAG_NUM; i++) {
        buf.strcat("%c", h.e_ident[i]);
    }
}


static CHAR const* get_st_shndx(UINT key)
{
    static CHAR const* g_shidx[] = {
        "UNDEF",
        "LORESERVE",
        "LOPROC",
        "HIPROC",
        "ABS",
        "COMMON",
        "HIRESERVE",
    };
    CHAR const* s = nullptr;
    switch (key){
    case SHN_UNDEF: s = g_shidx[0]; break;
    case SHN_LORESERVE: s = g_shidx[1]; break;
    //case SHN_LOPROC: s = g_shidx[2]; break;
    case SHN_HIPROC: s = g_shidx[3]; break;
    case SHN_ABS: s = g_shidx[4];  break;
    case SHN_COMMON: s = g_shidx[5]; break;
    case SHN_HIRESERVE: s = g_shidx[6]; break;
    default:;
    }
    return s;
}


static CHAR const* get_st_bind(UINT key)
{
    static CHAR const* g_symbind[] = {
        "LOCAL",
        "GLOBAL",
        "WEAK",
        "ENTRY",
        "LOPROC",
        "HIPROC",
    };
    CHAR const* b = nullptr;
    switch (key) {
    case STB_LOCAL: b = g_symbind[0]; break;
    case STB_GLOBAL: b = g_symbind[1]; break;
    case STB_WEAK: b = g_symbind[2]; break;
    case STB_ENTRY: b = g_symbind[3]; break;
    case STB_LOPROC: b = g_symbind[4]; break;
    case STB_HIPROC: b = g_symbind[5]; break;
    default:;
    }
    return b;
}


static CHAR const* get_st_type(UINT key)
{
    static CHAR const* g_symtype[] = {
        "NOTYPE",
        "OBJECT",
        "FUNC",
        "SECTION",
        "FILE",
        "IMPORT",
        "LOPROC",
        "HIPROC",
    };
    CHAR const* t = nullptr;
    switch (key) {
    case STT_NOTYPE: t = g_symtype[0]; break;
    case STT_OBJECT: t = g_symtype[1]; break;
    case STT_FUNC: t = g_symtype[2]; break;
    case STT_SECTION: t = g_symtype[3]; break;
    case STT_FILE: t = g_symtype[4]; break;
    case STT_IMPORT: t = g_symtype[5]; break;
    case STT_LOPROC: t = g_symtype[6]; break;
    case STT_HIPROC: t = g_symtype[7]; break;
    default:;
    }
    return t;
}


static CHAR const* get_d_tag(SWord key)
{
    static CHAR const* g_d_tag[] = {
        "UNDEF",
        "NEEDED",
        "PLTRELSZ",
        "PLTGOT",
        "HASH",
        "STRTAB",
        "SYMTAB",
        "RELA",
        "RELASZ",
        "RELAENT",
        "STRSZ",
        "SYMENT",
        "SONAME",
        "REL",
        "RELSZ",
        "RELENT",
        "PLTREL",
        "DEBUG",
        "JMPREL",
        "EXPORT",
        "EXPORTSZ",
        "EXPENT",
        "IMPORT",
        "IMPORTSZ",
        "IMPENT",
        "IT",
        "ITPRTY",
        "INITTERM",
        "PPC_GOT",
        "PPC_GOTSZ",
        "PPC_PLTSZ",
        "LOPROC",
        "HIPROC",
        // As following are old dynamic tags.  Readers
        //should handle these, writers must use the above
        "INIT_O",
        "FINI_O",
        "RPATH_O",
        "SYMBOLIC_O",
        "TEXTREL_O",
        "IT_O",
        "EXPORT_O",
        "EXPORTSZ_O",
        "IMPORT_O",
        "IMPORTSZ_O",
        "GOT_O",
        "GOTSZ_O",
        "PLTSZ_O",
        "ITPRTY_O",
        "LOUSER_O",
        "HIUSER_O",
        "",
    };
    CHAR const* p = nullptr;
    switch (key) {
    case  DT_NULL: p = g_d_tag[0]; break;
    case  DT_NEEDED: p = g_d_tag[1]; break;
    case  DT_PLTRELSZ: p = g_d_tag[2]; break;
    case  DT_PLTGOT: p = g_d_tag[3]; break;
    case  DT_HASH: p = g_d_tag[4]; break;
    case  DT_STRTAB: p = g_d_tag[5]; break;
    case  DT_SYMTAB: p = g_d_tag[6]; break;
    case  DT_RELA: p = g_d_tag[7]; break;
    case  DT_RELASZ: p = g_d_tag[8]; break;
    case  DT_RELAENT: p = g_d_tag[9]; break;
    case  DT_STRSZ: p = g_d_tag[10]; break;
    case  DT_SYMENT: p = g_d_tag[11]; break;
    case  DT_SONAME: p = g_d_tag[12]; break;
    case  DT_REL: p = g_d_tag[13]; break;
    case  DT_RELSZ: p = g_d_tag[14]; break;
    case  DT_RELENT: p = g_d_tag[15]; break;
    case  DT_PLTREL: p = g_d_tag[16]; break;
    case  DT_DEBUG: p = g_d_tag[17]; break;
    case  DT_JMPREL: p = g_d_tag[18]; break;
    case  DT_EXPORT: p = g_d_tag[19]; break;
    case  DT_EXPORTSZ: p = g_d_tag[20]; break;
    case  DT_EXPENT: p = g_d_tag[21]; break;
    case  DT_IMPORT: p = g_d_tag[22]; break;
    case  DT_IMPORTSZ: p = g_d_tag[23]; break;
    case  DT_IMPENT: p = g_d_tag[24]; break;
    case  DT_IT: p = g_d_tag[25]; break;
    case  DT_ITPRTY: p = g_d_tag[26]; break;
    case  DT_INITTERM: p = g_d_tag[27]; break;
    case  DT_PPC_GOT: p = g_d_tag[28]; break;
    case  DT_PPC_GOTSZ: p = g_d_tag[29]; break;
    case  DT_PPC_PLTSZ: p = g_d_tag[30]; break;
    case  DT_LOPROC: p = g_d_tag[31]; break;
    case  DT_HIPROC: p = g_d_tag[32]; break;
    // old dynamic g_d_tags
    case  DT_INIT_O: p = g_d_tag[33]; break;
    case  DT_FINI_O: p = g_d_tag[34]; break;
    case  DT_RPATH_O: p = g_d_tag[35]; break;
    case  DT_SYMBOLIC_O: p = g_d_tag[36]; break;
    case  DT_TEXTREL_O: p = g_d_tag[37]; break;
    case  DT_IT_O: p = g_d_tag[38]; break;
    case  DT_EXPORT_O: p = g_d_tag[39]; break;
    case  DT_EXPORTSZ_O: p = g_d_tag[40]; break;
    case  DT_IMPORT_O: p = g_d_tag[41]; break;
    case  DT_IMPORTSZ_O: p = g_d_tag[42]; break;
    case  DT_GOT_O: p = g_d_tag[43]; break;
    case  DT_GOTSZ_O: p = g_d_tag[44]; break;
    case  DT_PLTSZ_O: p = g_d_tag[45]; break;
    case  DT_ITPRTY_O: p = g_d_tag[46]; break;
    case  DT_LOUSER_O: p = g_d_tag[47]; break;
    case  DT_HIUSER_O: p = g_d_tag[48]; break;
    default:;
    }
    return p;
}


//
//START ELFMgr
//
ELFMgr::ELFMgr() : m_symbol_info(&m_sym_mgr), m_sect_map(this, &m_sect_mgr)
{
    m_file = nullptr;
    m_dump = nullptr;
    m_ti = nullptr;
    m_pool = nullptr;
    m_sym_tab = nullptr;
    m_file_name = nullptr;
    m_have_elf_format = false;
    m_shdr_num = 0;
    m_subtext_num = 0;
    m_global_symbol_begin_index = 0;
    m_reladyn_local_item_num = 0;
    m_global_symbol_begin_index_of_symtab = 0;
    m_global_symbol_begin_index_of_dynsym = 0;
    m_symbol_str_len = 0;
    m_max_offset = 0;
    m_max_addr = 0;
    m_got_elem_num = 0;
    m_symbol_num = 0;
    m_file_base_offset = 0;
    clean();
    m_pool = smpoolCreate(64, MEM_COMM);
    m_sect_info = new ELFSectionInfo();
}


ELFMgr::~ELFMgr()
{
    if (m_file != nullptr) { delete m_file; m_file = nullptr; }
    if (m_dump != nullptr) { delete m_dump; m_dump = nullptr; }
    if (m_ti != nullptr) { delete m_ti; m_ti = nullptr; }
    if (m_sect_info != nullptr) { delete m_sect_info; m_sect_info = nullptr; }
    smpoolDelete(m_pool);
    m_pool = nullptr;
    m_rm = nullptr;
    m_tm = nullptr;
}


void ELFMgr::clean()
{
    ::memset((void*)&m_elf_hdr, 0, sizeof(ELFHdr));
    if (m_ti != nullptr) { delete m_ti; m_ti = nullptr; }
    m_elf_sectheader = nullptr;
    m_elf_phdr = nullptr;
    m_elf_shstrtab = nullptr;
    m_elf_acommontab = nullptr;
    m_elf_scommontab = nullptr;
    m_elf_commenttab = nullptr;
    m_elf_map_section = nullptr;
    m_elf_map_data = nullptr;
    m_execucte_start_map_entry = nullptr;
    m_strtab_tab.clean();
    m_relatab_sect_list.clean();
    m_reltab_sect_list.clean();
    m_symtab_sect_list.clean();
    m_dynsymtab_sect_list.clean();
    m_dyntab_sect_list.clean();
    m_readonly_data_sect_list.clean();
    m_data_sect_list.clean();
    m_text_sect_list.clean();
    m_bss_sect_list.clean();
    if (m_pool != nullptr) {
        smpoolDelete(m_pool);
        m_pool = smpoolCreate(64, MEM_COMM);
    }
}


void * ELFMgr::xmalloc(size_t size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    ASSERTN(size < ELF_SIZE_4GB, ("too big"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset((void*)p, 0, size);
    return p;
}


EM_STATUS ELFMgr::initdumpscr()
{
    closeDump();
    m_dump = new FileObj(stdout);
    return EM_SUCC;
}


EM_STATUS ELFMgr::initdumpfile(FILE * filehandler)
{
    ASSERT0(filehandler);
    closeDump();
    m_dump = new FileObj(filehandler);
    return EM_SUCC;
}


EM_STATUS ELFMgr::initdumpfile(CHAR const* filename, bool is_del)
{
    closeDump();
    if (is_del) { UNLINK(filename); }
    m_dump = new FileObj(filename, false);
    if (m_dump->getFileHandler() == nullptr) {
        delete m_dump;
        m_dump = nullptr;
        return EM_OPEN_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::closeDump()
{
    if (m_dump != nullptr) {
        delete m_dump;
        m_dump = nullptr;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::closeELF()
{
    if (m_file != nullptr) {
        delete m_file;
        m_file = nullptr;
    }
    if (m_ti != nullptr) {
        delete m_ti;
        m_ti = nullptr;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::open(CHAR const* filename)
{
    closeELF();
    ASSERT0(m_file == nullptr);
    m_file = new FileObj(filename, false);
    if (m_file->getFileHandler() == nullptr) {
        delete m_file;
        m_file = nullptr;
        return EM_OPEN_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::read(BYTE * buf, size_t offset, size_t size)
{
    ASSERT0(m_file);

    //This function reads content from ELF via 'offset' and 'size'.
    //The 'offset' is the offset of ELF. If the ELF comes from AR file,
    //the 'offset' needs to be added the base offset of ELF in AR file.
    size_t actual_rd = 0;
    offset += getELFFileOffset();
    if (m_file->read(buf, offset, size, &actual_rd) != xcom::FO_SUCC ||
        actual_rd != size) {
        return EM_RD_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::write(BYTE const* buf, size_t offset, size_t size)
{
    ASSERT0(m_file);
    size_t actual_wr = 0;
    if (m_file->write(buf, offset, size, &actual_wr) != xcom::FO_SUCC ||
        actual_wr != size) {
        return EM_WR_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::append(BYTE const* buf, size_t size)
{
    ASSERT0(m_file);
    size_t actual_wr = 0;
    if (m_file->append(buf, size, &actual_wr) != xcom::FO_SUCC ||
        actual_wr != size) {
        return EM_WR_ERR;
    }
    return EM_SUCC;
}


void ELFMgr::assignDebugSectionIndex(UINT & debug_section_ind)
{
    //If there are relocation sections, two sections are needed.
    //Because it is a section itself,
    //and then there is another section for relocation related to itself,
    //e.g:
    //[15] .debug_line
    //[16] .rela.debug_line
    const UINT8 addition_one = 1;
    const UINT8 addition_two = 2;

    if (m_sect_info->hasDebugAbbrev()) {
        m_symbol_off.debug_abbrev_ind = debug_section_ind;
        debug_section_ind += addition_one;
    }

    if (m_sect_info->hasDebugStr()) {
        m_symbol_off.debug_str_ind = debug_section_ind;
        debug_section_ind += addition_one;
    }

    if (m_sect_info->hasDebugInfo()) {
        m_symbol_off.debug_info_ind = debug_section_ind;
        debug_section_ind += addition_two;
    }

    if (m_sect_info->hasDebugRanges()) {
        m_symbol_off.debug_ranges_ind = debug_section_ind;
        debug_section_ind += addition_two;
    }

    if (m_sect_info->hasDebugLine()) {
        m_symbol_off.debug_line_ind = debug_section_ind;
        debug_section_ind += addition_two;
    }

    if (m_sect_info->hasDebugFrame()) {
        m_symbol_off.debug_frame_ind = debug_section_ind;
        debug_section_ind += addition_two;
    }
}


void ELFMgr::setSectHeaderNameStrTabIdx()
{
    ASSERTN(getHdr().e_shstrndx == 0, ("already set"));
    ASSERTN(m_elf_shstrtab, ("miss section header strtab"));
    getHdr().e_shstrndx = (Half)getSectHeaderIdx(m_elf_shstrtab);
}


void ELFMgr::genSectHeaderNameStrTabContent(OUT CHARVec & charvec,
                                            OUT OffVec & offvec)
{
    StringList strlst;
    ELFHdr & hdr = getHdr();
    for (UINT i = 0; i < hdr.e_shnum; i++) {
        ELFSHdr * shdr = getSectHeader(i);
        if (shdr->s_name_str == nullptr) {
            offvec.set(i, 0);
            continue;
        }
        strlst.append_tail(shdr->s_name_str);
    }
    if (strlst.get_elem_count() == 0) { return; }
    genStrTabContent(charvec, offvec, strlst);
}


void ELFMgr::genStrTabContent(OUT CHARVec & charvec, OUT OffVec & offvec,
                              StringList const& strlst)
{
    size_t sz = 1;
    //Estimitate length.
    StringList::Iter it;
    for (CHAR const* s = strlst.get_head(&it);
         s != nullptr; s = strlst.get_next(&it)) {
        sz += ::strlen(s) + 1;
    }
    charvec.grow((UINT)sz);
    charvec.set(0, 0); //Set the first element to '\0'.
    BYTE * buf = (BYTE*)charvec.get_vec();
    size_t off = charvec.get_last_idx() + 1;
    UINT i = 0;
    for (CHAR const* s = strlst.get_head(&it);
         s != nullptr; s = strlst.get_next(&it), i++) {
        size_t l = ::strlen(s);
        offvec.set(i, (Off)off);
        ::memcpy(buf + off, (BYTE const*)s, l);

        //Here we use set() to write '\0' to update last_idx of vector rather
        //than buf[off + l].
        charvec.set((VecIdx)(off + l), 0);
        off += l + 1;
    }
}


void ELFMgr::setSectContentOffset()
{
    Off off = ELFHdr::getSize(this);
    off += getHdr().e_shnum * ELFSHdr::getSize(this);
    ASSERTN(m_elf_sectheader, ("should build section header info firstly"));
    for (UINT i = 0; i < getHdr().e_shnum; i++) {
        ELFSHdr * shdr = getSectHeader(i);
        off = xcom::ceil_align(off, shdr->s_addr_align);
        shdr->s_offset = off;
        if (shdr->s_type == S_NOBITS) { continue; }
        off += shdr->s_size;
    }
}


//Read ELF header table
EM_STATUS ELFMgr::readELFHeader()
{
    //Preload the ident info and set the ELF file class to help
    //following functions to determine whether the ELF file is 32bit or 64bit.
    ELFIdent ident;
    if (EM_SUCC != read((BYTE*)&ident, 0, sizeof(ELFIdent))) {
        return EM_RD_ERR;
    }
    m_elf_hdr.e_class = ident.e_class;
    m_elf_hdr.e_machine = ident.e_machine;
    allocTargInfo();
    if (m_ti == nullptr) { return EM_UNKNOWN_MACHINE; }

    //Read the ELF header.
    BYTE * buf = (BYTE*)ALLOCA(ELFHdr::getSize(this));
    if (EM_SUCC != read(buf, 0, ELFHdr::getSize(this))) {
        return EM_RD_ERR;
    }
    m_elf_hdr.extract(buf, this);
    if (!m_elf_hdr.isELF()) {
        return EM_NOT_ELF;
    }
    return EM_SUCC;
}


bool ELFMgr::isExecutable() const
{
    return m_elf_hdr.e_type == ET_REL ||
           m_elf_hdr.e_type == ET_EXEC ||
           m_elf_hdr.e_type == ET_DYN;
}


//Read ELF program table
EM_STATUS ELFMgr::readProgramHeader()
{
    if (m_elf_hdr.e_phnum == 0) { return EM_SUCC; }
    UINT sz = ELFPHdr::getMachBitWidth(this);
    if (m_elf_hdr.e_phensize != sz) {
        return EM_INVALID_PHDR;
    }
    m_elf_phdr = (ELFPHdr*)xmalloc(sizeof(ELFPHdr) * m_elf_hdr.e_phnum);
    BYTE * buf = (BYTE*)ALLOCA(ELFPHdr::getMachBitWidth(this));
    ELFPHdr * phdr = m_elf_phdr;
    for (UINT i = 0; i < m_elf_hdr.e_phnum; i++ , phdr++) {
        if (EM_SUCC != read(buf, (size_t)m_elf_hdr.e_phoff + i * sz, sz)) {
            return EM_RD_ERR;
        }
        phdr->extract(buf, this);
    }
    return EM_SUCC;
}


void ELFMgr::recordSectHeader(ELFSHdr * p)
{
    switch (p->s_type) {
    case S_REL:
        //Must see the s_info and s_link to get more precisely relocation
        //information which 's_info' refer to
        m_reltab_sect_list.append_tail(p);
        return;
    case S_RELA:
        //Must see the s_info and s_link to get more precisely relocation
        //information which 's_info' refer to
        m_relatab_sect_list.append_tail(p);
        return;
    case S_SYMTAB:
        m_symtab_sect_list.append_tail(p);
        return;
    case S_DYNAMIC:
        m_dyntab_sect_list.append_tail(p);
        return;
    case S_DYNSYM:
        m_dynsymtab_sect_list.append_tail(p);
        return;
    case S_PROGBITS:
        if (HAVE_FLAG(p->s_flags, SF_ALLOC) &&
            HAVE_FLAG(p->s_flags, SF_EXECINSTR)) {
            m_text_sect_list.append_tail(p);
            return;
        }
        if (HAVE_FLAG(p->s_flags, SF_ALLOC) &&
            HAVE_FLAG(p->s_flags, SF_WRITE)) {
            m_data_sect_list.append_tail(p);
            return;
        }
        if (HAVE_FLAG(p->s_flags, SF_ALLOC) &&
            !HAVE_FLAG(p->s_flags, SF_WRITE)) {
            //.eh_frame has the same property with .rodata.
            m_readonly_data_sect_list.append_tail(p);
            return;
        }
        return;
    case S_NOBITS:
        if (HAVE_FLAG(p->s_flags, SF_ALLOC) &&
            HAVE_FLAG(p->s_flags, SF_WRITE)) {
            m_bss_sect_list.append_tail(p);
        }
        return;
    case S_NOTE:
        ASSERTN(m_elf_commenttab == nullptr, ("comment section is not unique"));
        m_elf_commenttab = p;
        return;
    default:; //Unknown section.
    }
}


void ELFMgr::allocSectHeaderTab(UINT shnum)
{
    ASSERTN(m_elf_sectheader == nullptr, ("already alloc"));
    m_elf_sectheader = (ELFSHdr*)xmalloc(sizeof(ELFSHdr) * shnum);
}


EM_STATUS ELFMgr::readSectHeaderTab()
{
    allocSectHeaderTab(m_elf_hdr.e_shnum);
    UINT sz = ELFSHdr::getSize(this);
    BYTE * buf = (BYTE*)ALLOCA(sz);
    for (UINT i = 0; i < m_elf_hdr.e_shnum; i++) {
        ::memset((void*)buf, 0, sz);
        if (EM_SUCC != read(buf, (size_t)m_elf_hdr.e_shoff + i * sz, sz)) {
            return EM_RD_ERR;
        }
        m_elf_sectheader[i].extract(buf, this);
        recordSectHeader(&m_elf_sectheader[i]);
    }
    //Read section header related string table.
    if (m_elf_shstrtab == nullptr) {
        m_elf_shstrtab = &m_elf_sectheader[m_elf_hdr.e_shstrndx];
    }
    return EM_SUCC;
}


size_t ELFMgr::getSectHeaderIdx(ELFSHdr const* sh) const
{
    ASSERTN(m_elf_sectheader, ("no section header info"));
    ASSERT0(sh - m_elf_sectheader < m_elf_hdr.e_shnum);
    return sh - m_elf_sectheader;
}


EM_STATUS ELFMgr::readSymTabContent()
{
    for (ELFSHdr * p = m_symtab_sect_list.get_head();
         p != nullptr; p  = m_symtab_sect_list.get_next()) {
        if (EM_SUCC != readSectContent(getSectHeaderIdx(p))) {
            return EM_NO_SYM_TAB;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readDynSymTabContent()
{
    for (ELFSHdr * p = m_dynsymtab_sect_list.get_head();
         p != nullptr; p = m_dynsymtab_sect_list.get_next()) {
        if (EM_SUCC != readSectContent(getSectHeaderIdx(p))) {
            return EM_NO_DYNSYM_TAB;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readDynTabContent()
{
    for (ELFSHdr * p = m_dyntab_sect_list.get_head();
         p != nullptr; p = m_dyntab_sect_list.get_next()) {
        if (EM_SUCC != readSectContent(getSectHeaderIdx(p))) {
            return EM_NO_DYNAMIC_TAB;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readRelTabContent()
{
    for (ELFSHdr * p = m_reltab_sect_list.get_head();
         p != nullptr; p = m_reltab_sect_list.get_next()) {
        if (EM_SUCC != readSectContent(getSectHeaderIdx(p))) {
            return EM_NO_RELOC_TAB;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readRelaTabContent()
{
    for (ELFSHdr * p = m_relatab_sect_list.get_head();
         p != nullptr; p = m_relatab_sect_list.get_next()) {
        if (EM_SUCC != readSectContent(getSectHeaderIdx(p))) {
            return EM_NO_RELOCA_TAB;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readStrTabContent(ELFSHdr * strtab)
{
    ASSERT0(strtab);
    if (strtab->s_size == 0 || strtab->s_content != nullptr) {
        //The string table content has been loaded by other section.
        m_strtab_tab.append(strtab);
        return EM_SUCC;
    }
    //Read section data now.
    readSectContent(strtab);
    m_strtab_tab.append(strtab);
    return EM_SUCC;
}


EM_STATUS ELFMgr::readRelatedStrTabContent(ELFSHdr const* symtab)
{
    ASSERT0(symtab->isSymTab());
    ELFSHdr * tabh = symtab->getRelatedStrTab(this);
    //ASSERT0(tabh);
    //CASE:sometimes, there is no string table corresponding to symbol
    //table. All symbols are corresponding to a section name.
    if (tabh == nullptr) { return EM_SUCC; }
    return readStrTabContent(tabh);
}


EM_STATUS ELFMgr::readCommonStrTabContent()
{
    for (ELFSHdr * p = m_symtab_sect_list.get_head();
         p != nullptr; p  = m_symtab_sect_list.get_next()) {
        EM_STATUS st = readRelatedStrTabContent(p);
        if (st != EM_SUCC) { return st; }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readDynSymStrTabContent()
{
    for (ELFSHdr * p = m_dynsymtab_sect_list.get_head();
         p != nullptr; p  = m_dynsymtab_sect_list.get_next()) {
        EM_STATUS st = readRelatedStrTabContent(p);
        if (st != EM_SUCC) { return st; }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readDynStrTabContent()
{
    for (ELFSHdr * p = m_dyntab_sect_list.get_head();
         p != nullptr; p  = m_dyntab_sect_list.get_next()) {
        EM_STATUS st = readRelatedStrTabContent(p);
        if (st != EM_SUCC) { return st; }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readSHStrTabContent()
{
    if (m_elf_shstrtab == nullptr) { return EM_SUCC; }
    return readStrTabContent(m_elf_shstrtab);
}


EM_STATUS ELFMgr::readAllStrTabContent()
{
    EM_STATUS st = readSHStrTabContent();
    if (st != EM_SUCC) { return st; }

    st = readCommonStrTabContent();
    if (st != EM_SUCC) { return st; }

    st = readDynSymStrTabContent();
    if (st != EM_SUCC) { return st; }

    st = readDynStrTabContent();
    if (st != EM_SUCC) { return st; }
    return EM_SUCC;
}


CHAR const* ELFMgr::getSectName(ELFSHdr const* sh) const
{
    return getSectName(getSectHeaderIdx(sh));
}


CHAR const* ELFMgr::getSectName(size_t idx) const
{
    ASSERT0(idx < m_elf_hdr.e_shnum);
    if (m_elf_shstrtab == nullptr || m_elf_shstrtab->s_content == nullptr) {
        //Section header name string content still not load.
        return nullptr;
    }
    ELFSHdr * sh = getSectHeader(idx);
    if (sh->s_name_str != nullptr) { return sh->s_name_str; }
    sh->s_name_str  = ((CHAR*)m_elf_shstrtab->s_content) +
                      m_elf_sectheader[idx].s_name;
    return sh->s_name_str;
}


bool ELFMgr::isSectionAllocable(size_t sectidx) const
{
    return sectidx == SHN_COMMON ||
           (sectidx >= 0 && sectidx < m_elf_hdr.e_shnum &&
            HAVE_FLAG(m_elf_sectheader[sectidx].s_flags, SF_ALLOC));
}


EM_STATUS ELFMgr::readSectContent(ELFSHdr const* sh)
{
    return readSectContent(getSectHeaderIdx(sh));
}


EM_STATUS ELFMgr::readSectContent(size_t idx)
{
    if (idx >= m_elf_hdr.e_shnum) {
        return EM_INVALID_SECTION_INDX;
    }
    if (m_elf_sectheader[idx].s_size == 0) { return EM_SUCC; }

    //Read the section content if it still not yet loaded.
    if (nullptr == m_elf_sectheader[idx].s_content) {
        //ASSERTN(!isSectionAllocable(idx),
        //        ("space loadable section should not be allocated by
        //         xmalloc"));
        //CASE:Sometimes, user expect to read all section contents.

        m_elf_sectheader[idx].s_content = (BYTE*)xmalloc(
            (size_t)m_elf_sectheader[idx].s_size);
    }

    //Read section data.
    if (m_elf_sectheader[idx].s_type == S_NOBITS) { return EM_SUCC; }
    if (EM_SUCC != read((BYTE*)m_elf_sectheader[idx].s_content,
                        (size_t)m_elf_sectheader[idx].s_offset,
                        (size_t)m_elf_sectheader[idx].s_size)) {
        return EM_RD_ERR;
    }
    return EM_SUCC;
}


CHAR const* ELFMgr::getStrFromSymTab(size_t symtab_header_idx, size_t idx) const
{
    return getStrFromSymTab(getSectHeader(symtab_header_idx), idx);
}


CHAR const* ELFMgr::getStrFromSymTab(ELFSHdr const* symtab, size_t idx) const
{
    ASSERT0(symtab && symtab->isSymTab());
    Word symsz = ELFSym::getSize(this);
    ELFSym sym;
    BYTE const* symtabcontent = getSectContent(symtab);
    ASSERTN(symtabcontent, ("no content"));
    sym.extract(symtabcontent + idx * symsz, this);
    ELFSHdr const* strtab = symtab->getRelatedStrTab(this);
    return getSymNameFromStrTabOrDefinedSection(sym, strtab);
}


CHAR const* ELFMgr::getSymNameFromStrTabOrDefinedSection(
    ELFSym const& sym, ELFSHdr const* strtab) const
{
    if (sym.st_name == 0) {
        //The symbol has no name. Thus return the section name that defined the
        //symbol.
        return getSymDefinedSectName(sym);
    }
    ASSERTN(strtab, ("no string table correspond to symbol table"));
    return getStrFromStrTab(strtab, (size_t)sym.st_name);
}


CHAR const* ELFMgr::getStrFromStrTab(ELFSHdr const* strtab, size_t idx) const
{
    ASSERT0(strtab && strtab->isStrTab());
    ASSERTN(strtab->s_content, ("string table still not loaded"));
    return ((CHAR*)strtab->s_content) + idx;
}


BYTE * ELFMgr::getSectContent(ELFSHdr const* sh) const
{
    return m_elf_sectheader[getSectHeaderIdx(sh)].s_content;
}


ELFPHdr * ELFMgr::getProgramHeader(size_t idx) const
{
    ASSERT0(idx < m_elf_hdr.e_phnum);
    ASSERTN(m_elf_phdr, ("no yet allocate"));
    return &m_elf_phdr[idx];
}


ELFSHdr * ELFMgr::getSectHeader(size_t idx) const
{
    ASSERT0(idx < m_elf_hdr.e_shnum);
    ASSERTN(m_elf_sectheader, ("no yet allocate"));
    return &m_elf_sectheader[idx];
}


BYTE * ELFMgr::getSectContent(size_t idx) const
{
    ASSERT0(idx < m_elf_hdr.e_shnum);
    return (BYTE*)m_elf_sectheader[idx].s_content;
}


void ELFMgr::dumpAllStrTabContent() const
{
    StrTabTabIter it;
    for (ELFSHdr const* sh = m_strtab_tab.get_first(it);
         sh != nullptr; sh = m_strtab_tab.get_next(it)) {
        m_dump->prt("\n\n==== STRING TABLE ====");
        dumpStrTabContent((CHAR*)sh->s_content, sh->s_size);
    }
}


void ELFMgr::dumpStrTabContent(CHAR const* strtab, Addr size) const
{
    #define CHAR_IN_LINE 10
    if (strtab == nullptr || size == 0) { return; }
    Word i = 0;
    Word charnum = 0;
    CHAR const* linestart = strtab;
    m_dump->prt("\n");
    CHAR const* p;
    for (p = strtab; i < size; i++, p++) {
        m_dump->prt("%02x ", *p);
        if (charnum < CHAR_IN_LINE) {
            charnum++;
            continue;
        }
        m_dump->prt("| ");
        for (CHAR const* n = linestart; n <= p; n++) {
            m_dump->prt("%c", *n);
        }
        m_dump->prt("\n");
        charnum = 0;
        linestart = p + 1;
    }
    if (i == CHAR_IN_LINE - 1) {
        //No rest char to display.
        return;
    }
    m_dump->prt("| ");
    for (CHAR const* n = linestart; n < p; n++) {
        m_dump->prt("%c", *n);
    }
}


void ELFMgr::dumpCommonSymTabContent() const
{
    List<ELFSHdr*>::Iter it;
    for (ELFSHdr * p = m_symtab_sect_list.get_head(&it);
         p != nullptr; p  = m_symtab_sect_list.get_next(&it)) {
        dumpSymTabContent(p);
    }
}


CHAR const* ELFMgr::getSymDefinedSectName(ELFSym const& sym) const
{
    ASSERT0(sym.st_shndx <= SHN_HIRESERVE);
    if (sym.st_shndx == SHN_UNDEF || sym.st_shndx >= SHN_LORESERVE) {
        return "no-defined-sect";
    }
    return getSectName(sym.st_shndx);
}


void ELFMgr::dumpSymTabContent(ELFSHdr const* symtab) const
{
    ASSERT0(symtab->s_type == S_SYMTAB || symtab->s_type == S_DYNSYM);
    static TabCol tabcol[] = {
        { "No.", "%%-10u", 10, },
        { "NameIdx", "%%-10llu", 10, },
        { "DefSectIdx", "%%-16s", 16, },
        { "DefSectName", "%%-20s", 20, },
        { "Align/Value", "0x%%-10llx", 12, },
        { "SymSize", "%%-10llu", 10, },
        { "SymType", "%%-10s", 10, },
        { "SymBind", "%%-10s", 10, },
        { "Name", "%%-32s", 32, },
    };
    StrBuf colformat(64);
    StrBuf coltitle(64);
    formatTabColInfo(tabcol, sizeof(tabcol) / sizeof(tabcol[0]),
                     colformat, coltitle);
    ASSERT0(symtab);
    m_dump->prt("\n\n==== SYMBOL TABLE ====");
    BYTE const* symtabcontent = getSectContent(symtab);
    if (symtabcontent == nullptr) { return; }

    m_dump->prt("\n%s", coltitle.buf);

    Word ofst = 0;
    Word symsz = ELFSym::getSize(this);

    ELFSHdr * strtab = symtab->getRelatedStrTab(this);
    //ASSERTN(strtab, ("no string table correspond to '%s'",
    //        getSectName(symtab)));
    //In some cases, there is no string table corresponding to symbol table.
    //All symbols are corresponding to a section name.

    #define GSS_BUFLEN 32
    static CHAR sbuf[GSS_BUFLEN];
    for (size_t i = 0; i < symtab->getElemNum(); i++, ofst += symsz) {
        ELFSym sym;
        sym.extract(symtabcontent + ofst, this);
        CHAR const* s = get_st_shndx(sym.st_shndx);
        if (s == nullptr) {
            ::snprintf(sbuf, GSS_BUFLEN, "0x%x", sym.st_shndx);
            s = sbuf;
        }
        CHAR const* defedsect = getSymDefinedSectName(sym);
        CHAR const* b = get_st_bind(sym.st_bind);
        CHAR const* t = get_st_type(sym.st_type);
        m_dump->prt(colformat.buf,
                    i,
                    (UINT64)sym.st_name,
                    s,
                    defedsect,
                    (UINT64)sym.st_align,
                    (UINT64)sym.st_size,
                    t,
                    b,
                    getSymNameFromStrTabOrDefinedSection(sym, strtab));
    }
}


void ELFMgr::dumpDynSymTabContent() const
{
    List<ELFSHdr*>::Iter it;
    for (ELFSHdr * p = m_dynsymtab_sect_list.get_head(&it);
         p != nullptr; p  = m_dynsymtab_sect_list.get_next(&it)) {
        dumpSymTabContent(p);
    }
}


void ELFMgr::dumpSymTabContent() const
{
    ASSERT0(m_dump);
    dumpCommonSymTabContent();
    dumpDynSymTabContent();
}


void ELFMgr::dumpDynTabContent() const
{
    List<ELFSHdr*>::Iter it;
    for (ELFSHdr const* p = m_dyntab_sect_list.get_head(&it);
         p != nullptr; p  = m_dyntab_sect_list.get_next(&it)) {
        dumpDynTabContent(p);
    }
}


void ELFMgr::dumpRelaTabContent() const
{
    List<ELFSHdr*>::Iter it;
    for (ELFSHdr * p = m_relatab_sect_list.get_head(&it);
         p != nullptr; p  = m_relatab_sect_list.get_next(&it)) {
        dumpRelaTabContent(p);
    }
}


void ELFMgr::dumpRelTabContent() const
{
    List<ELFSHdr*>::Iter it;
    for (ELFSHdr * p = m_reltab_sect_list.get_head(&it);
         p != nullptr; p  = m_reltab_sect_list.get_next(&it)) {
        dumpRelTabContent(p);
    }
}


void ELFMgr::dumpDynTabContent(ELFSHdr const* dyntab) const
{
    ASSERT0(dyntab->s_type == S_DYNAMIC);
    static TabCol tabcol[] = {
        { "No.", "%%-5u", 5, },
        { "DynTag", "%%-10s", 10, },
        { "Val/Ptr", "%%-3llu", 3, },
    };
    StrBuf colformat(64);
    StrBuf coltitle(64);
    formatTabColInfo(tabcol, sizeof(tabcol) / sizeof(tabcol[0]),
                     colformat, coltitle);
    ASSERT0(m_dump);
    m_dump->prt("\n\n==== DYNAMIC TABLE ====");
    BYTE const* dyntabcontent = getSectContent(dyntab);
    if (dyntabcontent == nullptr) { return; }

    Word ofst = 0;
    Word dynsz = ELFDyn::getSize(this);
    m_dump->prt("\n%s", coltitle.buf);
    for (size_t i = 0; i < dyntab->getElemNum(); i++, ofst += dynsz) {
        ELFDyn dyn;
        dyn.extract(dyntabcontent + ofst, this);
        CHAR const* d_tag = get_d_tag(dyn.d_tag);
        m_dump->prt(colformat.buf,
                    i,
                    d_tag,
                    dyn.d_ptr);
    }
}


void ELFMgr::dumpELFHeader() const
{
    ASSERT0(m_dump);
    StrBuf buf(32);
    formatMagicNum(m_elf_hdr, buf);
    m_dump->prt("\n==== ELF HEADER ====");
    m_dump->prt("\nmagic number                      %s", buf.buf);
    m_dump->prt("\nclass                             %s",
                getClassName());
    m_dump->prt("\ndata                              %s",
                getEndianName());
    m_dump->prt("\nheader version                    %d",
                m_elf_hdr.e_hversion);
    m_dump->prt("\nelf type                          %s",
                getFileTypeName());
    m_dump->prt("\nmachine type                      %s",
                m_ti != nullptr ? m_ti->getMachineTypeName() : "unknown");
    m_dump->prt("\nobj version                       %d",
                m_elf_hdr.e_version);
    m_dump->prt("\nentry point                       0x%x",
                m_elf_hdr.e_entry);
    m_dump->prt("\nprogram header offset             %d(bytes into file)",
                m_elf_hdr.e_phoff);
    m_dump->prt("\nsection header offset             %d(bytes into file)",
                m_elf_hdr.e_shoff);
    m_dump->prt("\nprocesser special flag            %d",
                m_elf_hdr.e_flags);
    m_dump->prt("\nelf header sizes                  %d(bytes)",
                m_elf_hdr.e_ehsize);
    m_dump->prt("\nprogram entry sizes               %d(bytes)",
                m_elf_hdr.e_phensize);
    m_dump->prt("\nprogram entry counts              %d",
                m_elf_hdr.e_phnum);
    m_dump->prt("\nsection entry sizes               %d(bytes)",
                m_elf_hdr.e_shensize);
    m_dump->prt("\nsection entry counts              %d", m_elf_hdr.e_shnum);
    m_dump->prt("\nsection header string table index %d", m_elf_hdr.e_shstrndx);
    m_dump->prt("\nsizeof elf-header                 %d", sizeof(ELFHdr));
}


void ELFMgr::dumpSectHeaderTab() const
{
    typedef struct {
        Word type;
        CHAR const* name;
    } SectInfo;
    static SectInfo g_sectinfo[] = {
      { S_UNDEF, "UNDEF", },
      { S_PROGBITS, "PROGBITS", },
      { S_SYMTAB, "SYMTAB", },
      { S_STRTAB, "STRTAB", },
      { S_RELA, "RELA", },
      { S_HASH, "HASH", },
      { S_DYNAMIC, "DYNAMIC", },
      { S_NOTE, "NOTE", },
      { S_NOBITS, "NOBITS", },
      { S_REL, "REL", },
      { S_SHLIB, "SHLIB", },
      { S_DYNSYM, "DYNSYM", },
      { S_OS, "OS", },
      { S_IMPORTS, "IMPORTS", },
      { S_EXPORTS, "EXPORTS", },
      { S_RES, "RES", },
      { S_PROGFRAGS, "PROGFRAGS", },
      { S_IDMDLL, "IDMDLL", },
      { S_DEFLIB, "DEFLIB", },
      { S_LOPROC, "LOPROC", },
      { S_HIPROC, "HIPROC", },
      { S_LOUSER, "LOUSER", },
      { S_HIUSER, "HIUSER", },
      { S_VERSYM, "VERSYM", },
      { S_VERNEED, "VERNEED", },
    };
    static UINT g_sectinfo_num = sizeof(g_sectinfo) / sizeof(g_sectinfo[0]);

    typedef struct {
        Word flag;
        CHAR const* name;
    } FlagInfo;
    static FlagInfo g_flaginfo[] = {
      { SF_WRITE, "W", },
      { SF_ALLOC, "A", },
      { SF_EXECINSTR, "X", },
      { SF_MERGE, "M", },
      { SF_STRINGS, "S", },
      { SF_INFO_LINK, "I", },
      { SF_LINK_ORDER, "L", },
      { SF_OS_NONCONFORMING, "O", },
      { SF_GROUP, "G", },
      { SF_MASKOS, "o", },
      { SF_MASKPROC, "p", },
      { SF_BEGIN, "B", },
      { SF_END, "E", },
    };
    static UINT g_flaginfo_num = sizeof(g_flaginfo) / sizeof(g_flaginfo[0]);

    ASSERT0(m_dump);
    static TabCol tabcol[] = {
        { "No.", "%%-5u", 5, },
        { "Align", "%%-6llu", 6, },
        { "Type", "%%-10s", 10, },
        { "Flags", "%%-10s", 10, },
        { "Link", "%%-5u", 5, },
        { "Info", "%%-5u", 5, },
        { "Size", "0x%%-16llx", 18, },
        { "Addr", "0x%%-16llx", 18, },
        { "Offset", "0x%%-16llx", 18, },
        { "EntSize", "0x%%-8llx", 10, },
        { "Name", "%%-11s", 11, },
    };
    m_dump->prt("\n\n==== SECTION TABLE ====");
    if (m_elf_sectheader == nullptr) { return; }
    StrBuf colformat(64);
    StrBuf coltitle(64);
    formatTabColInfo(tabcol, sizeof(tabcol) / sizeof(tabcol[0]),
                     colformat, coltitle);
    m_dump->prt("\n%s", coltitle.buf);

    //Iterating all sections, print info.
    CHAR tmp[128];
    for (UINT i = 0; i < m_elf_hdr.e_shnum; i++) {
        CHAR const* sectty = nullptr;
        for (UINT j = 0 ; j < g_sectinfo_num; j++) {
            if (m_elf_sectheader[i].s_type == (Word32)g_sectinfo[j].type) {
                sectty = g_sectinfo[j].name;
                break;
            }
        }
        if (sectty == nullptr) {
            sectty = ""; //unknown section.
        }

        //Format section flag.
        tmp[0] = 0;
        for (UINT j = 0 ; j < g_flaginfo_num; j++) {
            if (m_elf_sectheader[i].s_flags & g_flaginfo[j].flag) {
                xcom::xstrcat(tmp, 128, "%s", g_flaginfo[j].name);
            }
        }
        CHAR const* sectname = getSectName(i);
        m_dump->prt(colformat.buf,
                    i,
                    (UINT64)m_elf_sectheader[i].s_addr_align,
                    sectty,
                    tmp,
                    m_elf_sectheader[i].s_link,
                    m_elf_sectheader[i].s_info,
                    (UINT64)m_elf_sectheader[i].s_size,
                    (UINT64)m_elf_sectheader[i].s_addr,
                    (UINT64)m_elf_sectheader[i].s_offset,
                    (UINT64)m_elf_sectheader[i].s_entry_size,
                    sectname != nullptr ? sectname : "");
    }
}


void ELFMgr::setSectHeaderNameOffset(OffVec const& offvec)
{
    ELFHdr & hdr = getHdr();
    ASSERT0(offvec.get_elem_count() == hdr.e_shnum);
    for (UINT i = 0; i < hdr.e_shnum; i++) {
        ELFSHdr * shdr = getSectHeader(i);
        shdr->s_name = (Word32)offvec[i];
    }
}


void ELFMgr::setSectHeaderNameStrTabContent(BYTE * content, Addr size)
{
    ASSERT0(m_elf_shstrtab);
    m_elf_shstrtab->s_content = content;
    m_elf_shstrtab->s_size = size;
}


void ELFMgr::formatSymWithAddend(OUT StrBuf & buf, ELFRela const& rela,
                                 ELFSHdr const* symtab)
{
    ELFSHdr const* strtab = symtab->getRelatedStrTab(this);
    ASSERTN(strtab, ("no strint table correspond to '%s'",
            getSectName(symtab)));
    CHAR const* str = getStrFromStrTab(strtab, (size_t)rela.r_sym);
    buf.strcat("%s %d", str, rela.r_addend);
}


void ELFMgr::dumpRelaTabContent(ELFSHdr const* sh) const
{
    ASSERT0(sh->s_type == S_RELA);
    static TabCol tabcol[] = {
        { "No.", "%%-10u", 10, },
        { "Offset", "0x%%-12llx", 14, },
        { "Type", "%%-18s", 18, },
        { "SymIdx", "0x%%-18llx", 20, },
        { "Sym + Addend", "%%-32s", 32, },
    };
    StrBuf colformat(64);
    StrBuf coltitle(64);
    formatTabColInfo(tabcol, sizeof(tabcol) / sizeof(tabcol[0]),
                     colformat, coltitle);

    m_dump->prt("\n\n==== RELOCATION TABLE '%s' ====", getSectName(sh));
    BYTE const* tabcontent = getSectContent(sh);
    if (tabcontent == nullptr) { return; }

    m_dump->prt("\n%s", coltitle.buf);

    Word ofst = 0;
    Word elemsz = ELFRela::getSize(this);
    StrBuf buf(48);
    ELFSHdr const* symtab = sh->getRelatedSymTab(this);
    ASSERTN(symtab, ("no corresponding symbol table"));
    for (size_t i = 0; i < sh->getElemNum(); i++, ofst += elemsz) {
        ELFRela rela;
        rela.extract(tabcontent + ofst, this);
        CHAR const* t = m_ti->getRelTypeName(rela.r_type);
        CHAR const* str = getStrFromSymTab(symtab, (size_t)rela.r_sym);
        buf.clean();
        if (rela.r_addend >= 0) {
            buf.strcat("%s+%d", str, rela.r_addend);
        } else {
            buf.strcat("%s%d", str, rela.r_addend);
        }
        m_dump->prt(colformat.buf,
                    i,
                    (UINT64)rela.r_offset,
                    t,
                    (UINT64)rela.r_sym,
                    buf.buf);
    }
}


void ELFMgr::dumpRelTabContent(ELFSHdr const* sh) const
{
    ASSERT0(sh->s_type == S_REL);
    static TabCol tabcol[] = {
        { "No.", "%%-10u", 10, },
        { "Offset", "0x%%-12llx", 14, },
        { "Type", "%%-18s", 18, },
        { "SymIdx", "0x%%-18llx", 20, },
        { "Sym", "%%-32s", 32, },
    };
    StrBuf colformat(64);
    StrBuf coltitle(64);
    formatTabColInfo(tabcol, sizeof(tabcol) / sizeof(tabcol[0]),
                     colformat, coltitle);

    m_dump->prt("\n\n==== RELOCATION TABLE '%s' ====", getSectName(sh));
    BYTE const* tabcontent = getSectContent(sh);
    if (tabcontent == nullptr) { return; }
    if (sh->getElemNum() == 0) { return; }
    Word ofst = 0;
    Word elemsz = ELFRel::getSize(this);
    ELFSHdr const* symtab = sh->getRelatedSymTab(this);
    ASSERTN(symtab, ("no corresponding symbol table"));
    m_dump->prt("\n%s", coltitle.buf);
    for (size_t i = 0; i < sh->getElemNum(); i++, ofst += elemsz) {
        ELFRel rel;
        rel.extract(tabcontent + ofst, this);
        CHAR const* t = m_ti->getRelTypeName(rel.r_type);
        m_dump->prt(colformat.buf,
                    i,
                    (UINT64)rel.r_offset,
                    t,
                    (UINT64)rel.r_sym,
                    getStrFromSymTab(symtab, (size_t)rel.r_sym));
    }
}


void ELFMgr::dump() const
{
    if (m_dump == nullptr) { return; }
    dumpELFHeader();
    dumpSectHeaderTab();
    dumpSymTabContent();
    dumpDynTabContent();
    dumpAllStrTabContent();
    dumpRelTabContent();
    dumpRelaTabContent();
    m_dump->prt("\n");
}


EM_STATUS ELFMgr::readAllSectContent()
{
    if (m_elf_hdr.e_shnum == 0) { return EM_SUCC; }
    ASSERT0(m_file);
    ASSERTN(m_elf_sectheader, ("miss section header table"));
    for (UINT i = 0; i < m_elf_hdr.e_shnum; i++) {
        ELFSHdr * shdr = getSectHeader(i);
        if (shdr->s_size == 0 || shdr->s_content != nullptr) {
            //If content pointer is not NULL, it has been loaded before.
            continue;
        }
        EM_STATUS st = readSectContent(i);
        if (EM_SUCC != st) {
            return st;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::readELF(CHAR const* filename, bool read_all_content)
{
    EM_STATUS st = open(filename);
    //Read the elf header.
    clean();
    readELFContent(read_all_content);
    return st;
}


EM_STATUS ELFMgr::readELFContent(bool read_all_content, bool close) {
    EM_STATUS st;
    if ((st = readELFHeader()) != EM_SUCC) { return st; }
    if (!isExecutable()) { return EM_UNEXECUTABLE; }

    st = readProgramHeader();
    if (st != EM_SUCC) { return st; }

    st = readSectHeaderTab();
    if (st != EM_SUCC) { return st; }

    if (read_all_content) {
        st = readAllSectContent();
    } else {
        //Only read the section contents that are used to display.
        st = readSymTabContent();
        if (st != EM_SUCC) { return st; }

        st = readDynSymTabContent();
        if (st != EM_SUCC) { return st; }

        st = readDynTabContent();
        if (st != EM_SUCC) { return st; }

        st = readRelTabContent();
        if (st != EM_SUCC) { return st; }

        st = readRelaTabContent();
        if (st != EM_SUCC) { return st; }

        st = readAllStrTabContent();
        if (st != EM_SUCC) { return st; }
    }
    dump();
    //After read a ELF content in AR file, AR file doesn't needed
    //to close, since other ELF content in this AR file may read later.
    if (close) { closeELF(); }
    return st;
}


EM_STATUS ELFMgr::writeELFHeaderAt(Word elfhdr_offset)
{
    BYTE * buf = (BYTE*)ALLOCA(ELFHdr::getSize(this));
    m_elf_hdr.insert(buf, this);
    if (EM_SUCC != write(buf, (size_t)elfhdr_offset, ELFHdr::getSize(this))) {
        return EM_WR_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::writeELFHeader(OUT Word & elfhdr_offset)
{
    BYTE * buf = (BYTE*)ALLOCA(ELFHdr::getSize(this));
    m_elf_hdr.insert(buf, this);
    elfhdr_offset = m_file->getFileSize();
    if (EM_SUCC != append(buf, ELFHdr::getSize(this))) {
        return EM_WR_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::writeProgramHeader()
{
    if (m_elf_hdr.e_phnum == 0) { return EM_SUCC; }
    UINT sz = ELFPHdr::getMachBitWidth(this);
    if (m_elf_hdr.e_phensize != sz) {
        return EM_INVALID_PHDR;
    }
    ASSERTN(m_elf_phdr, ("miss program header"));
    m_elf_hdr.e_phoff = m_file->getFileSize();
    BYTE * buf = (BYTE*)ALLOCA(ELFPHdr::getMachBitWidth(this));
    ELFPHdr * phdr = m_elf_phdr;
    for (UINT i = 0; i < m_elf_hdr.e_phnum; i++ , phdr++) {
        phdr->insert(buf, this);
        if (EM_SUCC != append(buf, sz)) {
            return EM_WR_ERR;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::writePad(size_t padsize)
{
    ASSERT0(m_file);
    #define LEN 32
    BYTE pad[32] = {0};
    size_t i = 0;
    for (; i < padsize / LEN; i += LEN) {
        if (EM_SUCC != append(pad, LEN)) {
            return EM_WR_ERR;
        }
    }
    for (; i < padsize; i ++) {
        if (EM_SUCC != append(&pad[0], 1)) {
            return EM_WR_ERR;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::writeSectContent()
{
    if (m_elf_hdr.e_shnum == 0) { return EM_SUCC; }
    ASSERT0(m_file);
    ASSERTN(m_elf_sectheader, ("miss section header table"));
    for (UINT i = 0; i < m_elf_hdr.e_shnum; i++) {
        ELFSHdr * shdr = getSectHeader(i);
        Off curoffset = m_file->getFileSize();
        shdr->s_offset = MAX((Off)xcom::ceil_align(shdr->s_offset,
            shdr->s_addr_align), curoffset);
        if (shdr->s_offset != curoffset) {
            writePad((size_t)(shdr->s_offset - curoffset));
        }
        if (shdr->s_size == 0) { continue; }
        ASSERTN(shdr->s_content, ("miss section content"));
        ASSERT0(shdr->s_offset == m_file->getFileSize());
        //Update and record the file byte offset to section header.
        //The section headers will be appended to file after its content.
        if (EM_SUCC != append(shdr->s_content, (size_t)shdr->s_size)) {
            return EM_WR_ERR;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::writeSectHeaderTab()
{
    if (m_elf_hdr.e_shnum == 0) { return EM_SUCC; }
    ASSERTN(m_elf_sectheader, ("miss section header table"));

    //Update and record the actual file byte offset of section header table.
    //And the ELF header has to rewrite to file to overwrite the original data.
    m_elf_hdr.e_shoff = m_file->getFileSize();
    UINT sz = ELFSHdr::getSize(this);
    BYTE * buf = (BYTE*)ALLOCA(sz);
    for (UINT i = 0; i < m_elf_hdr.e_shnum; i++) {
        ::memset((void*)buf, 0, sz);
        m_elf_sectheader[i].insert(buf, this);
        if (EM_SUCC != append(buf, sz)) {
            return EM_WR_ERR;
        }
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::writeELF(CHAR const* filename)
{
    allocTargInfo();
    if (m_ti == nullptr) { return EM_UNKNOWN_MACHINE; }
    UNLINK(filename);
    EM_STATUS st = open(filename);
    if (st != EM_SUCC) { return st; }

    Word elfhdr_offset = 0;
    st = writeELFHeader(elfhdr_offset);
    if (st != EM_SUCC) { return st; }

    st = writeProgramHeader();
    if (st != EM_SUCC) { return st; }

    st = writeSectContent();
    if (st != EM_SUCC) { return st; }

    st = writeSectHeaderTab();
    if (st != EM_SUCC) { return st; }

    st = writeELFHeaderAt(elfhdr_offset);
    if (st != EM_SUCC) { return st; }

    closeELF();
    return EM_SUCC;
}


void ELFMgr::constructSymbolNull(OUT BYTEVec & bytevec)
{
    UINT const elf_sym_size = ELFSym::getSize(this);
    BYTEVec space(elf_sym_size);
    ASSERT0(space.get_vec());

    ELFSym sym;
    setSymbolValue(&sym, ELF_VAL_UNDEF, ELF_VAL_UNDEF, SYMBOL_NOTYPE,
                   ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF);
    sym.insert(space.get_vec(), this);

    ::memcpy(bytevec.get_vec(), space.get_vec(), elf_sym_size);
}


void ELFMgr::constructELFHeader(UINT sthr_num)
{
    ELFHdr & hdr = getHdr();
    hdr.setMagic();
    hdr.e_class = EC_64BIT;
    hdr.e_data = ED_LITTLE;
    hdr.e_hversion = ELF_VERSION;
    hdr.e_type = ET_REL;
    hdr.e_machine = getEHMachine();
    hdr.e_version = ELF_VERSION;
    hdr.e_entry = ELF_VAL_UNDEF;
    hdr.e_phoff = ELF_VAL_UNDEF;
    hdr.e_shoff = ELFSHdr::getSize(this);
    hdr.e_flags = getEHFlag();
    hdr.e_ehsize = ELFHdr::getSize(this);
    hdr.e_phensize = ELFPHdr::getMachBitWidth(this);
    hdr.e_phnum = ELF_VAL_UNDEF;
    hdr.e_shensize = ELFSHdr::getSize(this);
    hdr.e_shnum = sthr_num;
    allocSectHeaderTab(sthr_num);
}


void ELFMgr::constructELFNullSection(MOD ELFSHdr * null_shdr)
{
    null_shdr->s_type = S_UNDEF;
    null_shdr->s_addr = ELF_VAL_UNDEF;
    null_shdr->s_size = ELF_VAL_UNDEF;
    null_shdr->s_entry_size = ELF_VAL_UNDEF;
    null_shdr->s_name_str = STR_UNDEF;
}


void ELFMgr::constructELFTextSection(MOD ELFSHdr * text_shdr)
{
    BYTEVec text_char_vec;
    text_char_vec.set(0, ELF_VAL_UNDEF);
    text_shdr->s_type = S_PROGBITS;
    text_shdr->s_addr = ELF_VAL_UNDEF;
    text_shdr->s_size = ELF_VAL_UNDEF;
    text_shdr->s_entry_size = ELF_VAL_UNDEF;
    text_shdr->s_addr_align = getTextSectAlign();
    text_shdr->s_content = (BYTE *)text_char_vec.get_vec();
    text_shdr->s_name_str = TEXT_SH_NAME;
    SET_FLAG(text_shdr->s_flags, SF_ALLOC|SF_EXECINSTR);
}


void ELFMgr::constructELFShStrSection(MOD ELFSHdr * shstr_shdr)
{
    shstr_shdr->s_type = S_STRTAB;
    shstr_shdr->s_name_str = SHSTR_SH_NAME;
    setSectHeaderNameStrTab(shstr_shdr);
}


void ELFMgr::constructELFSymTabSection(MOD ELFSHdr * symtab_shdr,
                                       ELFSHdr const* strtab_shdr,
                                       ELFSHdr const* text_shdr,
                                       BYTEVec & sym)
{
    symtab_shdr->s_type = S_SYMTAB;
    symtab_shdr->s_addr = ELF_VAL_UNDEF;
    symtab_shdr->s_size = (Addr)sym.get_elem_count();
    symtab_shdr->s_link = (Word32)getSectHeaderIdx(strtab_shdr);
    symtab_shdr->s_info = (Word32)getGlobalSymbolBeginIndex();
    symtab_shdr->s_addr_align = (Addr)ELFSym::getAlign(this);
    symtab_shdr->s_entry_size = (Word)ELFSym::getSize(this);
    symtab_shdr->s_content = (BYTE*)sym.get_vec();
    symtab_shdr->s_name_str = SYMTAB_SH_NAME;
}


void ELFMgr::constructELFStrTabSection(MOD ELFSHdr * strtab_shdr,
                                       CHARVec & sym_str)
{
    strtab_shdr->s_type = S_STRTAB;
    strtab_shdr->s_addr = ELF_VAL_UNDEF;
    strtab_shdr->s_size = (Addr)sym_str.get_elem_count();
    strtab_shdr->s_entry_size = ELF_VAL_UNDEF;
    strtab_shdr->s_addr_align = sizeof(BYTE);
    strtab_shdr->s_content = (BYTE *)sym_str.get_vec();
    strtab_shdr->s_name_str = SYMSTR_SH_NAME;
}


void ELFMgr::constructELFSbssSection(MOD ELFSHdr * sbss_shdr,
                                     BYTEVec & sbss,
                                     UINT sbss_align)
{
    sbss_shdr->s_type = S_NOBITS;
    sbss_shdr->s_addr = ELF_VAL_UNDEF;
    sbss_shdr->s_size = (Addr)sbss.get_capacity();
    sbss_shdr->s_entry_size = ELF_VAL_UNDEF;
    sbss_shdr->s_addr_align = (Addr)sbss_align;
    sbss_shdr->s_content = (BYTE *)sbss.get_vec();
    sbss_shdr->s_name_str = SBSS_SH_NAME;
    SET_FLAG(sbss_shdr->s_flags, SF_WRITE|SF_ALLOC);
}


void ELFMgr::constructELFSdataSection(MOD ELFSHdr * sdata_shdr,
                                      BYTEVec & sdata,
                                      UINT sdata_align)
{
    sdata_shdr->s_type = S_PROGBITS;
    sdata_shdr->s_addr = ELF_VAL_UNDEF;
    sdata_shdr->s_size = (Addr)sdata.get_elem_count();
    sdata_shdr->s_entry_size = ELF_VAL_UNDEF;
    sdata_shdr->s_addr_align = (Addr)sdata_align;
    sdata_shdr->s_content = (BYTE*)sdata.get_vec();
    sdata_shdr->s_name_str = SDATA_SH_NAME;
    SET_FLAG(sdata_shdr->s_flags, SF_WRITE|SF_ALLOC);
}


void ELFMgr::constructELFBssSection(MOD ELFSHdr * bss_shdr,
                                    BYTEVec & bss,
                                    UINT bss_align)
{
    bss_shdr->s_type = S_NOBITS;
    bss_shdr->s_addr = ELF_VAL_UNDEF;
    bss_shdr->s_size = (Addr)bss.get_capacity();
    bss_shdr->s_entry_size = ELF_VAL_UNDEF;
    bss_shdr->s_addr_align = (Addr)bss_align;
    bss_shdr->s_content = (BYTE*)bss.get_vec();
    bss_shdr->s_name_str = BSS_SH_NAME;
    SET_FLAG(bss_shdr->s_flags, SF_WRITE|SF_ALLOC);
}


void ELFMgr::constructELFDataSection(MOD ELFSHdr * data_shdr,
                                     BYTEVec & data,
                                     UINT data_align)
{
    data_shdr->s_type = S_PROGBITS;
    data_shdr->s_addr = ELF_VAL_UNDEF;
    data_shdr->s_size = (Addr)data.get_elem_count();
    data_shdr->s_entry_size = ELF_VAL_UNDEF;
    data_shdr->s_addr_align = (Addr)data_align;
    data_shdr->s_content = (BYTE*)data.get_vec();
    data_shdr->s_name_str = DATA_SH_NAME;
    SET_FLAG(data_shdr->s_flags, SF_WRITE|SF_ALLOC);
}


void ELFMgr::constructELFSpmSection(MOD ELFSHdr * spm_shdr,
                                    BYTEVec & spm,
                                    UINT spm_align)
{
    spm_shdr->s_type = S_PROGBITS;
    spm_shdr->s_addr = ELF_VAL_UNDEF;
    spm_shdr->s_size = (Addr)spm.get_elem_count();
    spm_shdr->s_entry_size = ELF_VAL_UNDEF;
    spm_shdr->s_addr_align = (Addr)spm_align;
    spm_shdr->s_content = (BYTE*)spm.get_vec();
    spm_shdr->s_name_str = getSpmSHName();
    SET_FLAG(spm_shdr->s_flags, SF_WRITE|SF_ALLOC);
}


void ELFMgr::constructELFConstSection(MOD ELFSHdr * const_shdr,
                                      BYTEVec & const_data,
                                      UINT const_align)
{
    const_shdr->s_type = S_PROGBITS;
    const_shdr->s_addr = ELF_VAL_UNDEF;
    const_shdr->s_size = (Addr)const_data.get_elem_count();
    const_shdr->s_entry_size = ELF_VAL_UNDEF;
    const_shdr->s_addr_align = (Addr)const_align;
    const_shdr->s_content = (BYTE*)const_data.get_vec();
    const_shdr->s_name_str = CONST_SH_NAME;
    SET_FLAG(const_shdr->s_flags, SF_ALLOC);
}


void ELFMgr::constructELFRelaSdataSection(MOD ELFSHdr * rela_sdata_shdr,
                                          BYTEVec & rela_sdata_data,
                                          ELFSHdr const* sym_shdr,
                                          ELFSHdr const* sdata_shdr)
{
    rela_sdata_shdr->s_type = S_RELA;
    rela_sdata_shdr->s_addr = ELF_VAL_UNDEF;
    rela_sdata_shdr->s_size = rela_sdata_data.get_elem_count();
    rela_sdata_shdr->s_entry_size = ELFRela::getSize(this);
    rela_sdata_shdr->s_link = (Word32)getSectHeaderIdx(sym_shdr);
    rela_sdata_shdr->s_info = (Word32)getSectHeaderIdx(sdata_shdr);
    rela_sdata_shdr->s_addr_align = (Addr)ELFRela::getAlign(this);
    rela_sdata_shdr->s_content = (BYTE*)rela_sdata_data.get_vec();
    rela_sdata_shdr->s_name_str = RELA_SDATA_NAME;
    SET_FLAG(rela_sdata_shdr->s_flags, SF_INFO_LINK);
}


void ELFMgr::constructELFRelaDataSection(MOD ELFSHdr * rela_data_shdr,
                                         BYTEVec & rela_data_data,
                                         ELFSHdr const* sym_shdr,
                                         ELFSHdr const* data_shdr)
{
    rela_data_shdr->s_type = S_RELA;
    rela_data_shdr->s_addr = ELF_VAL_UNDEF;
    rela_data_shdr->s_size = rela_data_data.get_elem_count();
    rela_data_shdr->s_entry_size = ELFRela::getSize(this);
    rela_data_shdr->s_link = (Word32)getSectHeaderIdx(sym_shdr);
    rela_data_shdr->s_info = (Word32)getSectHeaderIdx(data_shdr);
    rela_data_shdr->s_addr_align = (Addr)ELFRela::getAlign(this);
    rela_data_shdr->s_content = (BYTE*)rela_data_data.get_vec();
    rela_data_shdr->s_name_str = RELA_DATA_NAME;
    SET_FLAG(rela_data_shdr->s_flags, SF_INFO_LINK);
}


void ELFMgr::constructELFFuncSection(ELFSHdr * func_shdr, BYTEVec & code,
                                     CHAR const* name, MOD BYTE * text_space,
                                     SymbolInfo const* sym_info)
{
    //Since program region may has multiple functions that need to be wrote
    //into ELF, new space should be created to prevent being released.
    ::memcpy(text_space, (BYTE*)(code.get_vec()), code.get_elem_count());

    func_shdr->s_type = S_PROGBITS;
    func_shdr->s_addr = ELF_VAL_UNDEF;
    func_shdr->s_size = (Addr)code.get_elem_count();
    func_shdr->s_addr_align = sizeof(BYTE);
    func_shdr->s_entry_size = ELF_VAL_UNDEF;
    func_shdr->s_content = text_space;
    func_shdr->s_name_str = name;
    if (xoc::g_stack_on_global &&
        FUNCINFO_is_entry(SYMINFO_func(sym_info))) {
            SET_FLAG(func_shdr->s_flags,
                SF_ALLOC | SF_EXECINSTR |
                getFlagStackHBM());
    } else {
        SET_FLAG(func_shdr->s_flags, SF_ALLOC|SF_EXECINSTR);
    }
}


void ELFMgr::constructELFRelSection(MOD ELFSHdr * rela_shdr,
                                    ELFSHdr const* sym_shdr,
                                    ELFSHdr const* text_func_shdr,
                                    BYTEVec & rel, CHAR const* name,
                                    MOD BYTE * rel_space)
{
    //Since program region may has multiple functions that need to be wrote
    //into ELF, new space should be created to prevent being released.
    ::memcpy(rel_space, (BYTE *)(rel.get_vec()), rel.get_elem_count());

    rela_shdr->s_type = S_RELA;
    rela_shdr->s_addr = ELF_VAL_UNDEF;
    rela_shdr->s_size = rel.get_elem_count();
    rela_shdr->s_entry_size = (Addr)ELFRela::getSize(this);
    rela_shdr->s_link = (Word32)getSectHeaderIdx(sym_shdr);
    rela_shdr->s_info = (Word32)getSectHeaderIdx(text_func_shdr);
    rela_shdr->s_addr_align = (Addr)ELFRela::getAlign(this);
    rela_shdr->s_content = rel_space;
    rela_shdr->s_name_str = name;
    SET_FLAG(rela_shdr->s_flags, SF_INFO_LINK);
}


void ELFMgr::constructELFAttributeSection(MOD ELFSHdr * attr_shdr,
    BYTEVec & attr_content, Addr attr_size, Word32 attr_type,
    CHAR const* attr_name)
{
    attr_shdr->s_type = attr_type;
    attr_shdr->s_addr = ELF_VAL_UNDEF;
    attr_shdr->s_size = attr_size;
    attr_shdr->s_entry_size = ELF_VAL_UNDEF;
    attr_shdr->s_addr_align = 1;
    attr_shdr->s_content = (BYTE*)(attr_content.get_vec());
    attr_shdr->s_name_str = attr_name;
}


void ELFMgr::processELFTextRelSection(ELFSHdr const* symtab_shdr,
                                      OUT UINT & si)
{
    xcom::StrBuf buf0(32);
    xcom::StrBuf buf1(32);

    xcom::List<Sym const*>::Iter iter;

    for (m_symbol_name.get_head(&iter); iter != nullptr;
         m_symbol_name.get_next(&iter)) {

        Sym const* sym_name = iter->val();
        ASSERT0(sym_name);

        SymbolInfo const* sym_info = m_symbol_info.get(sym_name);
        ASSERT0(sym_info);

        if (!SYMINFO_is_func(sym_info) ||
            sym_info->getSymbolCode().get_elem_count() == 0) {
            continue;
        }

        //Get function name and generate section names of .text.xxx and
        //.rel.text.xxx.
        //The section names of kernel function are .aitext.xxx
        //and .rel.aitext.xxx.
        CHAR const* subtext_section_name =
            FUNCINFO_is_entry(SYMINFO_func(sym_info)) ?
            SUBTEXT_ENTRY_SH_PRE : SUBTEXT_SH_PRE;
        CHAR const* rela_section_name =
            FUNCINFO_is_entry(SYMINFO_func(sym_info)) ?
            RELA_KERNEL_SH_NAME : RELA_SH_NAME;

        buf0.clean();
        buf1.clean();

        buf0.strcat(subtext_section_name);
        buf0.strcat(SYMINFO_name(sym_info)->getStr());

        buf1.strcat(rela_section_name);
        buf1.strcat(SYMINFO_name(sym_info)->getStr());

        //Generate content for .text.xxx.
        BYTEVec & text_content = SYMINFO_func(sym_info)->getCode();

        //Generate content for .rela.text.xxx.
        BYTEVec rel_content;
        genRelocContent(rel_content, SYMINFO_reloc(sym_info));

        //Malloc some spaces to save intermediate data of function region data
        //processing. It is necessary because tmporary space will be freed.
        CHAR * text_name_space = (CHAR*)(xmalloc(buf0.buflen));
        ::memcpy(text_name_space, buf0.buf, buf0.buflen);
        CHAR * rel_name_space = (CHAR*)(xmalloc(buf1.buflen));
        ::memcpy(rel_name_space, buf1.buf, buf1.buflen);
        BYTE * text_space = (BYTE*)(xmalloc(text_content.get_elem_count()));
        //Construct .text.xxx and .rel.text.xxx section headers and sections.
        ELFSHdr * func_shdr = getSectHeader(si++);
        constructELFFuncSection(func_shdr, text_content, text_name_space,
                                text_space, sym_info);

        if (rel_content.get_elem_count() == 0) { continue; }

        BYTE * rel_space = (BYTE*)(xmalloc(rel_content.get_elem_count()));
        ELFSHdr * rel_shdr = getSectHeader(si++);
        constructELFRelSection(rel_shdr, symtab_shdr, func_shdr, rel_content,
                               rel_name_space, rel_space);
    }
}


void ELFMgr::constructELFShIndex(OUT CHARVec & charvec,
                                 OUT OffVec & offvec)
{
    genSectHeaderNameStrTabContent(charvec, offvec);
    setSectHeaderNameStrTabIdx();
    setSectHeaderNameOffset(offvec);
    setSectHeaderNameStrTabContent((BYTE*)charvec.get_vec(),
                                   charvec.get_elem_count());
    setSectContentOffset();
}


void ELFMgr::setSectionInfo(SymbolInfo const* sym_info)
{
    switch (SYMINFO_sect_type(sym_info)) {
    //.sbss
    case SH_TYPE_SBSS:
        m_sect_info->setSbss(true);
        m_sect_info->setSbssAlign(
            (UINT)MAX(m_sect_info->getSbssAlign(), SYMINFO_align(sym_info)));
        break;
    //.sdata
    case SH_TYPE_SDATA:
        m_sect_info->setSdata(true);
        m_sect_info->setSdataAlign(
            (UINT)MAX(m_sect_info->getSdataAlign(), SYMINFO_align(sym_info)));
        if (SYMINFO_reloc(sym_info).get_elem_count() != 0) {
            m_sect_info->setRelaSdata(true);
        }
        break;
    //.bss
    case SH_TYPE_BSS:
        m_sect_info->setBss(true);
        m_sect_info->setBssAlign(
            (UINT)MAX(m_sect_info->getBssAlign(), SYMINFO_align(sym_info)));
        break;
    //.data
    case SH_TYPE_DATA:
        m_sect_info->setData(true);
        m_sect_info->setDataAlign(
            (UINT)MAX(m_sect_info->getDataAlign(), SYMINFO_align(sym_info)));
        if (SYMINFO_reloc(sym_info).get_elem_count() != 0) {
            m_sect_info->setRelaData(true);
        }
        break;
    //.spm
    case SH_TYPE_SPM:
        m_sect_info->setSpm(true);
        m_sect_info->setSpmAlign(
            (UINT)MAX(m_sect_info->getSpmAlign(), SYMINFO_align(sym_info)));
        break;
    //.const
    case SH_TYPE_CONST:
        m_sect_info->setConst(true);
        m_sect_info->setConstAlign(
            (UINT)MAX(m_sect_info->getConstAlign(), SYMINFO_align(sym_info)));
        break;
    //.debug_abbrev
    case SH_TYPE_DEBUG_ABBREV:
        m_sect_info->setDebugAbb(true);
        m_sect_info->setDebugAbbAlign((UINT)SYMINFO_align(sym_info));
        break;
    //.debug_str
    case SH_TYPE_DEBUG_STR:
        m_sect_info->setDebugStr(true);
        m_sect_info->setDebugStrAlign((UINT)SYMINFO_align(sym_info));
        break;
    //.debug_info
    case SH_TYPE_DEBUG_INFO:
        m_sect_info->setDebugInfo(true);
        m_sect_info->setDebugInfoAlign((UINT)SYMINFO_align(sym_info));
        break;
    //.debug_ranges
    case SH_TYPE_DEBUG_RANGES:
        m_sect_info->setDebugRanges(true);
        m_sect_info->setDebugRangesAlign((UINT)SYMINFO_align(sym_info));
        break;
    //.debug_line
    case SH_TYPE_DEBUG_LINE:
        m_sect_info->setDebugLine(true);
        m_sect_info->setDebugLineAlign((UINT)SYMINFO_align(sym_info));
        break;
    //.debug_frame
    case SH_TYPE_DEBUG_FRAME:
        m_sect_info->setDebugFrame(true);
        m_sect_info->setDebugFrameAlign((UINT)SYMINFO_align(sym_info));
        break;
    default: break; //Variable is not available.;
    }
}


void ELFMgr::genRelocSdataAndDataContent(OUT BYTEVec & sdata_content,
                                         OUT BYTEVec & data_content)
{
    xcom::List<Sym const*>::Iter iter;

    for (m_symbol_name.get_head(&iter); iter != nullptr;
         m_symbol_name.get_next(&iter)) {

        Sym const* sym_name = iter->val();
        ASSERT0(sym_name);

        SymbolInfo * sym_info = m_symbol_info.get(sym_name);
        ASSERT0(sym_info);

        if (SYMINFO_reloc(sym_info).get_elem_count() == 0 ||
            (SYMINFO_sect_type(sym_info) != SH_TYPE_SDATA &&
             SYMINFO_sect_type(sym_info) != SH_TYPE_DATA)) {
            continue;
        }

        if (SYMINFO_sect_type(sym_info) == SH_TYPE_SDATA) {
            genRelocContent(sdata_content, SYMINFO_reloc(sym_info));
            continue;
        }

        genRelocContent(data_content, SYMINFO_reloc(sym_info));
    }
}


void ELFMgr::genRelocContent(OUT BYTEVec & bytevec,
                             xcom::Vector<RelocInfo*> const& reloc_vec)
{
    UINT elem_num = reloc_vec.get_elem_count();

    if (elem_num == 0) { return; }

    UINT const member_num = ELFRela::getMemberNum();

    //Allocate enough space in advance.
    AssembleBinDescVec rel_desc_vec;
    rel_desc_vec.grow(elem_num * member_num);

    //Iterate through all entries and generate relocation content.
    for (UINT i = 0; i < elem_num; i++) {
        RelocInfo * reloc_info = reloc_vec[i];
        ASSERT0(reloc_info);
        ASSERT0(m_symbol_info.find(RELOCINFO_name(reloc_info)));

        AssembleBinDesc d_off(ELFRela::getOffsetSize(this),
                              SYMINFO_ofst(RELOCINFO_caller_sym(reloc_info)) +
                              RELOCINFO_called_loc(reloc_info));
        AssembleBinDesc d_type(ELFRela::getTypeSize(this),
                               RELOCINFO_type(reloc_info));
        AssembleBinDesc d_sym(ELFRela::getSymbolSize(this),
                              getRelocSymValue(reloc_info));
        AssembleBinDesc d_addend(ELFRela::getAddendSize(this),
                                 RELOCINFO_addend(reloc_info));

        rel_desc_vec.set(i * member_num, d_off);
        rel_desc_vec.set(i * member_num + 1, d_type);
        rel_desc_vec.set(i * member_num + 2, d_sym);
        rel_desc_vec.set(i * member_num + 3, d_addend);
    }

    extractAssBinDescVec(&bytevec, rel_desc_vec);
}


void ELFMgr::collectELFFactor(OUT StringList & sym_name)
{
    //Record number of functions to compute total section number.
    UINT func_num = 0;

    //Record index of symbol.
    UINT sym_ind = 0;

    //Record number of relocation sections.
    UINT func_reloc_sect_num = 0;

    //Record local symbol number.
    UINT local_sym_num = 0;

    xcom::List<Sym const*>::Iter iter;

    for (m_symbol_name.get_head(&iter); iter != nullptr;
         m_symbol_name.get_next(&iter)) {

        Sym const* symbol_name = iter->val();
        ASSERT0(symbol_name);

        SymbolInfo * sym_info = m_symbol_info.get(symbol_name);
        ASSERT0(sym_info);

        //Count function number.
        //Note that there are functions with the same name declared and defined
        //in a source file. Here only the defined functions are written into
        //the symbol table.
        func_num += SYMINFO_is_func(sym_info) &
            (SYMINFO_func(sym_info) != nullptr &&
            sym_info->getSymbolCode().get_elem_count() != 0);

        SYMINFO_index(sym_info) = ++sym_ind;
        local_sym_num += !SYMINFO_is_global(sym_info) &&
                         SYMINFO_sect_type(sym_info) != SH_TYPE_UNDEF;

        //Collector names(string) of all symbols to generate .strtab content.
        sym_name.append_tail(SYMINFO_name(sym_info)->getStr());

        //Set section info based all symbols.
        setSectionInfo(sym_info);

        if (SYMINFO_is_func(sym_info) &&
            SYMINFO_reloc(sym_info).get_elem_count() != 0) {
            func_reloc_sect_num++;
        }
    }

    m_symbol_off.text_ind = BASE_SEC_NUM + m_sect_info->hasSbss() +
        m_sect_info->hasSdata() + m_sect_info->hasBss() +
        m_sect_info->hasData() + m_sect_info->hasSpm() +
        m_sect_info->hasConst();

    //Note: A device func will may generate two sections like .text.xxx and
    //.rel.text.xxx.
    UINT32 debug_section_ind = m_symbol_off.text_ind + func_num +
        func_reloc_sect_num + m_sect_info->hasRelaData() +
        m_sect_info->hasRelaSdata() + isNeedAttributeSection();
    assignDebugSectionIndex(debug_section_ind);
    m_sect_info->setShdrNum(debug_section_ind);

    //Set the index of the first global symbol in the symbol table.
    setGlobalSymbolBeginIndex(local_sym_num + ELF_NULL_SYMBOL_SIZE);
}


void ELFMgr::initSymbol(xoc::Var const* var, Sym const* func_name,
                        SECTION_TYPE sect_type, UINT sect_ofst)
{
    ASSERT0(var && var->get_name() &&
            !m_symbol_info.find(var->get_name()));

    SymbolInfo * sym_info = m_sym_mgr.allocSymbolInfo();
    ASSERT0(sym_info);

    if (var->is_func()) {
        FunctionInfo * func_info = m_func_mgr.allocFunctionInfo();
        ASSERT0(func_info);

        SYMINFO_func(sym_info) = func_info;
        SYMINFO_func_name(sym_info) = var->get_name();
        FUNCINFO_name(SYMINFO_func(sym_info)) = var->get_name();

        //Set whether symbol is function.
        SYMINFO_is_func(sym_info) = true;

        //Set whether symbol is a entry function.
        FUNCINFO_is_entry(SYMINFO_func(sym_info)) = var->is_entry();
    }

    //Set function name or related function name.
    SYMINFO_func_name(sym_info) = func_name != nullptr ?
        func_name : SYMINFO_func_name(sym_info);

    //Set symbol name.
    SYMINFO_name(sym_info) = var->get_name();

    //Set whether symbol is global.
    SYMINFO_is_global(sym_info) = var->is_global();

    //Set whether symbol is visible.
    SYMINFO_is_visible(sym_info) = var->is_visible();

    //Set whether symbol is weak.
    SYMINFO_is_weak(sym_info) = var->is_weak();

    //Set symbol section.
    SYMINFO_sect_type(sym_info) = sect_type != SH_TYPE_UNDEF ?
        sect_type : judgeSymbolSection(var);

    //Set section offset.
    SYMINFO_ofst(sym_info) = sect_ofst != 0 ?
        sect_ofst : SYMINFO_ofst(sym_info);

    m_symbol_info.set(var->get_name(), sym_info);

    //The local symbols are in front of the symbol table, and the global
    //symbols are in the back of the symbol table.
    if (var->is_global()) { m_symbol_name.append_tail(var->get_name()); }
    else { m_symbol_name.append_head(var->get_name()); }
}


void ELFMgr::initSymSection(xoc::Var const* var)
{
    ASSERT0(var && var->get_name() &&
            !m_symbol_info.find(var->get_name()));

    Sym const* sym = var->get_name();
    ASSERT0(sym);
    //Create a new SymbolInfo. Remember to delete it.
    SymbolInfo * sym_info = m_symbol_info.find(var->get_name()) ?
        m_symbol_info.get(var->get_name()) : new SymbolInfo();
    ASSERT0(sym_info);

    //Set symbol name.
    SYMINFO_name(sym_info) = sym;

    //Set whether symbol is extern.
    SYMINFO_is_extern(sym_info) = var->is_extern();

    //Set whether symbol is weak.
    SYMINFO_is_weak(sym_info) = var->is_weak();

    //Set whether symbol is visible.
    SYMINFO_is_visible(sym_info) = var->is_visible();

    //Set whether symbol is global.
    SYMINFO_is_global(sym_info) = var->is_global();

    //Set whether symbol is function.
    SYMINFO_is_func(sym_info) = var->is_func();

    //Set whether symbol is initialized.
    SYMINFO_is_init(sym_info) = var->hasInitString() || var->hasInitVal();

    //TODO：Since m_rm is currently empty, this interface
    //(if (VAR_name(var) == m_rm->addToSymbolTab(DEBUG_ABBREV_SH_NAME)))
    //cannot be invoked.
    if (::strcmp(VAR_name(var)->getStr(), DEBUG_ABBREV_SH_NAME) == 0) {
        //Set symbol align.
        SYMINFO_align(sym_info) = DEBUG_SECTION_ALIGN_ONE;

        //Set symbol section.
        SYMINFO_sect_type(sym_info) = SH_TYPE_DEBUG_ABBREV;
        SYMINFO_is_debug(sym_info) = true;
    }
    if (::strcmp(VAR_name(var)->getStr(), DEBUG_STR_SH_NAME) == 0) {
        //Set symbol align.
        SYMINFO_align(sym_info) = DEBUG_SECTION_ALIGN_ONE;

        //Set symbol section.
        SYMINFO_sect_type(sym_info) = SH_TYPE_DEBUG_STR;
        SYMINFO_is_debug(sym_info) = true;
    }
    if (::strcmp(VAR_name(var)->getStr(), DEBUG_INFO_SH_NAME) == 0) {
        //Set symbol align.
        SYMINFO_align(sym_info) = DEBUG_SECTION_ALIGN_ONE;

        //Set symbol section.
        SYMINFO_sect_type(sym_info) = SH_TYPE_DEBUG_INFO;
        SYMINFO_is_debug(sym_info) = true;
    }
    if (::strcmp(VAR_name(var)->getStr(), DEBUG_RANGES_SH_NAME) == 0) {
        //Set symbol align.
        SYMINFO_align(sym_info) = DEBUG_SECTION_ALIGN_ONE;

        //Set symbol section.
        SYMINFO_sect_type(sym_info) = SH_TYPE_DEBUG_RANGES;
        SYMINFO_is_debug(sym_info) = true;
    }
    if (::strcmp(VAR_name(var)->getStr(), DEBUG_LINE_SH_NAME) == 0) {
        //Set symbol align.
        SYMINFO_align(sym_info) = DEBUG_SECTION_ALIGN_ONE;

        //Set symbol section.
        SYMINFO_sect_type(sym_info) = SH_TYPE_DEBUG_LINE;
        SYMINFO_is_debug(sym_info) = true;
    }
    if (::strcmp(VAR_name(var)->getStr(), DEBUG_FRAME_SH_NAME) == 0) {
        //Set symbol align.
        SYMINFO_align(sym_info) = DEBUG_SECTION_ALIGN_EIGHT;

        //Set symbol section.
        SYMINFO_sect_type(sym_info) = SH_TYPE_DEBUG_FRAME;
        SYMINFO_is_debug(sym_info) = true;
    }
    if (::strcmp(VAR_name(var)->getStr(), DEBUG_LOC_SH_NAME) == 0) {
        //Set symbol align.
        SYMINFO_align(sym_info) = DEBUG_SECTION_ALIGN_ONE;

        //Set symbol section.
        SYMINFO_sect_type(sym_info) = SH_TYPE_DEBUG_LOC;
        SYMINFO_is_debug(sym_info) = true;
    }

    //Add entry.
    m_symbol_info.setAlways(var->get_name(), sym_info);

    if (SYMINFO_is_global(sym_info)) {
        m_symbol_name.append_tail(SYMINFO_name(sym_info));
    } else {
        m_symbol_name.append_head(SYMINFO_name(sym_info));
    }
}


void ELFMgr::extractSymbolExceptUserDefFunc()
{
    VarVec const* var_vec = getRegionMgr()->getVarMgr()->getVarVec();
    ASSERT0(var_vec);

    for (UINT i = 0; i < var_vec->get_elem_count(); i++) {
        xoc::Var * var = var_vec->get(i);

        //Since user-defined functions have been saved in initSymbol(), we
        //need to extract available variables except them.
        if (!isVarAvailable(var)) { continue; }

        bool find = false;
        SymbolInfo * sym_info = m_symbol_info.getAndGen(var->get_name(), &find);
        ASSERT0(sym_info);
        if (find && var->is_func()) { continue; }

        if (var->is_section()) { continue; }
        Sym const* sym = var->get_name();
        ASSERT0(sym);

        //Set symbol name.
        SYMINFO_name(sym_info) = sym;

        //Set whether symbol is extern.
        SYMINFO_is_extern(sym_info) = var->is_extern();

        //Set whether symbol is weak.
        SYMINFO_is_weak(sym_info) = var->is_weak();

        //Set whether symbol is visible.
        SYMINFO_is_visible(sym_info) = var->is_visible();

        //Set whether symbol is global.
        SYMINFO_is_global(sym_info) = var->is_global();

        //Set whether symbol is function.
        SYMINFO_is_func(sym_info) = var->is_func();

        //Set function info.
        if (SYMINFO_is_func(sym_info)) {
            SYMINFO_func(sym_info) = m_func_mgr.allocFunctionInfo();
        }

        //Set whether symbol is initialized.
        SYMINFO_is_init(sym_info) = var->hasInitString() || var->hasInitVal();

        //Set symbol align.
        SYMINFO_align(sym_info) = var->get_align();

        //Set symbol section.
        SYMINFO_sect_type(sym_info) = judgeSymbolSection(var);

        //Set symbol size.
        SYMINFO_size(sym_info) = ((var->is_string() && !var->hasInitString()) ||
            var->is_func()) ? 0 : (var->is_string() && var->hasInitString()) ?
            ::strlen(VAR_string(var)->getStr()) : var->getByteSize(m_tm);

        if (SYMINFO_is_global(sym_info) ||
            SYMINFO_sect_type(sym_info) == SH_TYPE_UNDEF) {
            m_symbol_name.append_tail(SYMINFO_name(sym_info));
        } else {
            m_symbol_name.append_head(SYMINFO_name(sym_info));
        }

        //Set symbol binword or data.
        if (!var->hasInitVal()) { continue; }
        SYMINFO_data_byte(sym_info) = (BYTE*)xmalloc(SYMINFO_size(sym_info));
        void * src_addr = var->is_string() ?
            ((void*)(VAR_string(var)->getStr())) :
            ((void*)(var->getByteValue()->getBuffer()));
        ::memcpy((void*)(SYMINFO_data_byte(sym_info)),
            src_addr, SYMINFO_size(sym_info));
        SYMINFO_is_byte(sym_info) = true;
    }
}


void ELFMgr::genCommonSectionContent(OUT BYTEVec & symtab_content,
                                     OUT BYTEVec & sbss_content,
                                     OUT BYTEVec & sdata_content,
                                     OUT BYTEVec & bss_content,
                                     OUT BYTEVec & data_content,
                                     OUT BYTEVec & const_content,
                                     OUT BYTEVec & spm_content,
                                     OffVec const& sym_str_off)
{
    AssembleBinDescVec sym_desc_vec;
    AssembleBinDescVec sdata_desc_vec;
    AssembleBinDescVec data_desc_vec;
    AssembleBinDescVec const_desc_vec;
    AssembleBinDescVec spm_desc_vec;

    UINT const elf_sym_size = ELFSym::getSize(this);

    symtab_content.grow((m_symbol_info.get_elem_count() + 1) * elf_sym_size);
    symtab_content.set(symtab_content.get_capacity() - 1, 0);

    constructSymbolNull(symtab_content);

    //Skip the first null symbol.
    UINT ind = elf_sym_size;

    BYTEVec space(elf_sym_size);
    ASSERT0(space.get_vec());

    xcom::List<Sym const*>::Iter iter;

    for (m_symbol_name.get_head(&iter); iter != nullptr;
         m_symbol_name.get_next(&iter)) {

        Sym const* sym_name = iter->val();
        ASSERT0(sym_name);

        SymbolInfo * sym_info = m_symbol_info.get(sym_name);
        ASSERT0(sym_info);

        UINT align = (UINT)SYMINFO_align(sym_info);

        //Get some common attributes: name, bind, other, size
        ASSERT0(m_symbol_off.var_name_off < sym_str_off.get_elem_count());
        Word name = sym_str_off[m_symbol_off.var_name_off];
        UCHAR bind = SYMINFO_is_weak(sym_info) ? STB_WEAK :
            SYMINFO_is_global(sym_info) ? STB_GLOBAL : STB_LOCAL;
        UCHAR other = SYMINFO_is_visible(sym_info) ? STV_DEFAULT : STV_HIDDEN;
        Addr size = SYMINFO_size(sym_info);
        ELFSym sym;

        //Set symbol attributes and section content according to symbol info.
        switch (SYMINFO_sect_type(sym_info)) {
        case SH_TYPE_UNDEF: {
            //There is functional variable that needs to be resolved by
            //external variable. Thus the value of 'st_bind' attribute needs
            //to be assigned 'STB_GLOBAL'.
            if (isUndefFuncationalVar(sym_info, bind)) { bind = STB_GLOBAL; }
            setSymbolValue(&sym, name, bind, ELF_VAL_UNDEF,
                           other, ELF_VAL_UNDEF, ELF_VAL_UNDEF,
                           ELF_VAL_UNDEF);
            break;
        }
        case SH_TYPE_SBSS: {
            //If there is extern attribute of the symbol, the value of
            //st_shndx and st_size field are UNDEF(means that the symbol
            //is undefined in this object file).
            UINT off = (UINT)xcom::ceil_align(m_symbol_off.sbss_off,
                                              SYMINFO_align(sym_info));
            Half index = SYMINFO_is_extern(sym_info) ? ELF_VAL_UNDEF :
                BASE_SEC_NUM;
            Addr sz = SYMINFO_is_extern(sym_info) ? ELF_VAL_UNDEF : size;
            setSymbolValue(&sym, name, bind, SYMBOL_OBJECT, other,
                           index, off, sz);

            //Finally determine the various characteristics of the MC symbol.
            //Note: The offset of this symbol has already been added
            //to the symbol table in setSymbolValue, so add 0 here.
            if (xoc::g_debug) {
                if (getDwarfMgr()->isVarSymbolPresent(sym_name)) {
                    getDwarfMgr()->setStageElfSymbol(sym_name, 0);
                }
            }
            m_symbol_off.sbss_off = (UINT)(off + sz);
            break;
        }
        case SH_TYPE_SDATA: {
            ASSERT0(SYMINFO_is_init(sym_info));
            UINT off = (UINT)xcom::ceil_align(m_symbol_off.sdata_off, align);
            assemblePadZeroToContent(sdata_desc_vec,
                calculatePadZero(off, m_symbol_off.sdata_off));
            assembleInitValToContent(sdata_desc_vec, sym_info);
            Half index = BASE_SEC_NUM + m_sect_info->hasSbss();
            setSymbolValue(&sym, name, bind, SYMBOL_OBJECT, other,
                           index, off, size);
            SYMINFO_ofst(sym_info) = off;

            //Finally determine the various characteristics of the MC symbol.
            //Note: The offset of this symbol has already been added
            //to the symbol table in setSymbolValue, so add 0 here.
            if (xoc::g_debug) {
                if (getDwarfMgr()->isVarSymbolPresent(sym_name)) {
                    getDwarfMgr()->setStageElfSymbol(sym_name, 0);
                }
            }
            m_symbol_off.sdata_off = (UINT)(off + size);
            break;
        }
        case SH_TYPE_BSS: {
            //If there is extern attribute of the symbol, the value of
            //st_shndx and st_size field are UNDEF(means that the symbol
            //is undefined in this object file).
            UINT off = (UINT)xcom::ceil_align(m_symbol_off.bss_off, align);
            Half index = SYMINFO_is_extern(sym_info) ? ELF_VAL_UNDEF :
                BASE_SEC_NUM + m_sect_info->hasSbss() +
                m_sect_info->hasSdata();
            Addr sz = SYMINFO_is_extern(sym_info) ? ELF_VAL_UNDEF : size;
            setSymbolValue(&sym, name, bind, SYMBOL_OBJECT, other,
                           index, off, sz);

            //Finally determine the various characteristics of the MC symbol.
            //Note: The offset of this symbol has already been added
            //to the symbol table in setSymbolValue, so add 0 here.
            if (xoc::g_debug) {
                if (getDwarfMgr()->isVarSymbolPresent(sym_name)) {
                    getDwarfMgr()->setStageElfSymbol(sym_name, 0);
                }
            }
            m_symbol_off.bss_off = (UINT)(off + sz);
            break;
        }
        case SH_TYPE_DATA: {
            ASSERT0(SYMINFO_is_init(sym_info));
            UINT off = (UINT)xcom::ceil_align(m_symbol_off.data_off, align);
            assemblePadZeroToContent(data_desc_vec,
                calculatePadZero(off, m_symbol_off.data_off));
            assembleInitValToContent(data_desc_vec, sym_info);
            Half index = BASE_SEC_NUM + m_sect_info->hasSbss() +
                m_sect_info->hasSdata() + m_sect_info->hasBss();
            setSymbolValue(&sym, name, bind, SYMBOL_OBJECT, other,
                           index, off, size);
            SYMINFO_ofst(sym_info) = off;

            //Finally determine the various characteristics of the MC symbol.
            //Note: The offset of this symbol has already been added
            //to the symbol table in setSymbolValue, so add 0 here.
            if (xoc::g_debug) {
                if (getDwarfMgr()->isVarSymbolPresent(sym_name)) {
                    getDwarfMgr()->setStageElfSymbol(sym_name, 0);
                }
            }
            m_symbol_off.data_off = (UINT)(off + size);
            break;
        }
        case SH_TYPE_SPM: {
            UINT off = (UINT)xcom::ceil_align(m_symbol_off.spm_off, align);
            assemblePadZeroToContent(spm_desc_vec,
                calculatePadZero(off, m_symbol_off.spm_off));
            assembleInitValToContent(spm_desc_vec, sym_info);
            Half index = SYMINFO_is_extern(sym_info) ? ELF_VAL_UNDEF :
                BASE_SEC_NUM + m_sect_info->hasSbss() +
                m_sect_info->hasSdata() +
                m_sect_info->hasBss() + m_sect_info->hasData();
            Addr sz = SYMINFO_is_extern(sym_info) ? ELF_VAL_UNDEF : size;
            setSymbolValue(&sym, name, bind, SYMBOL_OBJECT, other,
                           index, off, sz);

            //Finally determine the various characteristics of the MC symbol.
            //Note: The offset of this symbol has already been added
            //to the symbol table in setSymbolValue, so add 0 here.
            if (xoc::g_debug) {
                if (getDwarfMgr()->isVarSymbolPresent(sym_name)) {
                    getDwarfMgr()->setStageElfSymbol(sym_name, 0);
                }
            }
            m_symbol_off.spm_off = (UINT)(off + sz);
            break;
        }
        case SH_TYPE_CONST: {
            ASSERT0(SYMINFO_is_init(sym_info));
            UINT off = (UINT)xcom::ceil_align(m_symbol_off.const_off, align);
            assemblePadZeroToContent(const_desc_vec,
                calculatePadZero(off, m_symbol_off.const_off));
            assembleInitValToContent(const_desc_vec, sym_info);
            Half index = BASE_SEC_NUM + m_sect_info->hasSbss() +
                m_sect_info->hasSdata() + m_sect_info->hasBss() +
                m_sect_info->hasData() + m_sect_info->hasSpm();
            setSymbolValue(&sym, name, STB_GLOBAL, SYMBOL_OBJECT,
                           ELF_VAL_UNDEF, index, off, size);

            //Finally determine the various characteristics of the MC symbol.
            //Note: The offset of this symbol has already been added
            //to the symbol table in setSymbolValue, so add 0 here.
            if (xoc::g_debug) {
                if (getDwarfMgr()->isVarSymbolPresent(sym_name)) {
                    getDwarfMgr()->setStageElfSymbol(sym_name, 0);
                }
            }
            m_symbol_off.const_off = (UINT)(off + size);
            break;
        }
        case SH_TYPE_TEXT: {
            if (!SYMINFO_is_global(sym_info)) {
                Sym const* func = SYMINFO_func_name(sym_info);
                ASSERT0(func && m_symbol_info.find(func) &&
                        SYMINFO_is_func(m_symbol_info.get(func)));

                if (SYMINFO_sect_index(m_symbol_info.get(func)) != 0) {
                    SYMINFO_sect_index(sym_info) =
                        SYMINFO_sect_index(m_symbol_info.get(func));
                } else {
                    SYMINFO_sect_index(sym_info) = m_symbol_off.text_ind;
                    SYMINFO_sect_index(m_symbol_info.get(func)) =
                        m_symbol_off.text_ind;

                    //.text.xxx and .rel.text.xxx section headers are always
                    //together.
                    m_symbol_off.text_ind += 2;
                    m_symbol_off.func_off++;
                }

                setSymbolValue(&sym, name, bind, ELF_VAL_UNDEF, ELF_VAL_UNDEF,
                    SYMINFO_sect_index(sym_info), SYMINFO_ofst(sym_info),
                    ELF_VAL_UNDEF);
                break;
            }

            if (SYMINFO_sect_index(sym_info) == 0) {
                SYMINFO_sect_index(sym_info) = m_symbol_off.text_ind;

                //.text.xxx and .rel.text.xxx section headers are always
                //together.
                m_symbol_off.text_ind += 2;
                m_symbol_off.func_off++;
            }

            setSymbolValue(&sym, name, bind, SYMBOL_FUNC,
                getSymOtherInfo() | other, SYMINFO_sect_index(sym_info),
                ELF_VAL_UNDEF, size);

            break;
        }
        case SH_TYPE_DEBUG_ABBREV: {
            Half index = m_symbol_off.debug_abbrev_ind;
            setSymbolValue(&sym, name, bind, STT_SECTION, ELF_VAL_UNDEF,
                           index, 0, size);
            SYMINFO_sym(sym_info) = sym;
            break;
        }
        case SH_TYPE_DEBUG_INFO: {
            Half index = m_symbol_off.debug_info_ind;
            setSymbolValue(&sym, name, bind, STT_SECTION, ELF_VAL_UNDEF,
                           index, 0, size);
            SYMINFO_sym(sym_info) = sym;
            break;
        }
        case SH_TYPE_DEBUG_RANGES: {
            Half index = m_symbol_off.debug_ranges_ind;
            setSymbolValue(&sym, name, bind, STT_SECTION, ELF_VAL_UNDEF,
                           index, 0, size);
            SYMINFO_sym(sym_info) = sym;
            break;
        }
        case SH_TYPE_DEBUG_STR: {
            Half index = m_symbol_off.debug_str_ind;
            setSymbolValue(&sym, name, bind, STT_SECTION, ELF_VAL_UNDEF,
                           index, 0, size);
            SYMINFO_sym(sym_info) = sym;
            break;
        }
        case SH_TYPE_DEBUG_LINE: {
            Half index = m_symbol_off.debug_line_ind;
            setSymbolValue(&sym, name, bind, STT_SECTION, ELF_VAL_UNDEF,
                           index, 0, size);
            SYMINFO_sym(sym_info) = sym;
            break;
        }
        case SH_TYPE_DEBUG_FRAME: {
            Half index = m_symbol_off.debug_frame_ind;

            setSymbolValue(&sym, name, bind, STT_SECTION, ELF_VAL_UNDEF,
                           index, 0, size);
            SYMINFO_sym(sym_info) = sym;
            break;
        }
        case SH_TYPE_DEBUG_LOC: {
            setSymbolValue(&sym, name, bind, ELF_VAL_UNDEF, ELF_VAL_UNDEF,
                           ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF);
            SYMINFO_sym(sym_info) = sym;
            break;
        }
        default: UNREACHABLE(); //Variable is not available.
        }
        m_symbol_off.var_name_off++;
        sym.insert(space.get_vec(), this);

        ::memcpy(symtab_content.get_vec() + ind, space.get_vec(), elf_sym_size);
        ind += elf_sym_size;
    }

    if (m_sect_info->hasSbss()) {
        sbss_content.grow((UINT)(m_symbol_off.sbss_off));
    }
    if (m_sect_info->hasBss()) {
        bss_content.grow((UINT)(m_symbol_off.bss_off));
    }
    if (m_sect_info->hasSdata()) {
        extractAssBinDescVec(&sdata_content, sdata_desc_vec);
    }
    if (m_sect_info->hasData()) {
        extractAssBinDescVec(&data_content, data_desc_vec);
    }
    if (m_sect_info->hasConst()) {
        extractAssBinDescVec(&const_content, const_desc_vec);
    }
    if (m_sect_info->hasSpm()) {
        extractAssBinDescVec(&spm_content, spm_desc_vec);
    }
}


void ELFMgr::assembleInitValToContent(
    OUT AssembleBinDescVec & content_desc_vec, SymbolInfo const* sym_info)
{
    ASSERT0(sym_info);
    UINT sz = (UINT)SYMINFO_size(sym_info);
    if (SYMINFO_is_byte(sym_info)) {
        AssembleBinDesc asbd(sz * BITS_PER_BYTE, SYMINFO_data_byte(sym_info));
        content_desc_vec.append(asbd);
        return;
    }

    //Current symbol is located on SPM and it does not have init value.
    ASSERT0(SYMINFO_sect_type(sym_info) == SH_TYPE_SPM &&
            !SYMINFO_is_init(sym_info));
    assemblePadZeroToContent(content_desc_vec, sz);
}


void ELFMgr::assemblePadZeroToContent(
    OUT AssembleBinDescVec & content_desc_vec, UINT pad_size)
{
    //If the pad_size is zero, do nothing.
    if (pad_size == 0) { return; }

    if (isSizeValid(pad_size * BITS_PER_BYTE)) {
        AssembleBinDesc asbd(pad_size * BITS_PER_BYTE, BinWord(0));
        content_desc_vec.append(asbd);
        return;
    }

    //Use zero for padding.
    ASSERT0(pad_size < MAX_STACK_SPACE);
    BYTE * arr = (BYTE*)ALLOCA(pad_size);
    ::memset(arr, 0, pad_size);
    content_desc_vec.append(AssembleBinDesc(pad_size * BITS_PER_BYTE, arr));
}


void ELFMgr::write2ELF()
{
    //At this stage, all information has been fully transparent.
    //If debugging is enabled,
    //we will generate the debug_line and debug_frame sections.
    if (xoc::g_debug) {
        getDwarfMgr()->genFrameBinary();
        getDwarfMgr()->genLineBinary();
    }

    //Define some structures recording contents of elf sections.
    CHARVec strtab_content;      //Save content for .strtab.
    BYTEVec symtab_content;      //Save content for .symtab.
    BYTEVec sbss_content;        //Save content for .sbss.
    BYTEVec sdata_content;       //Save content for .sdata.
    BYTEVec bss_content;         //Save content for .bss.
    BYTEVec data_content;        //Save content for .data.
    BYTEVec spm_content;         //Save content for .spm.
    BYTEVec reloc_content;       //Save content for .rel.text.xxx.
    BYTEVec text_content;        //Save content for .text.xxx.
    BYTEVec const_content;       //Save content for .const.
    BYTEVec reloc_sdata_content; //Save content for .rela.sdata.
    BYTEVec reloc_data_content;  //Save content for .rela.data.
    BYTEVec attr_content;        //Save content for .xxx.attributes.
    OffVec symstr_off;           //Save offsets of name of each symbol.
    CHARVec charvec;             //Save names of all section headers.
    OffVec offvec;               //Save offsets of section header names.
    StringList sym_name;         //Save names of all symbols.

    //Bug fix: If declaration of function is before definition of function,
    //data written into ELF will be wrong due to out-of-order.
    extractSymbolExceptUserDefFunc();

    //Collect names of all symbols and compute some factors.
    collectELFFactor(sym_name);

    //Construct ELF header.
    constructELFHeader(m_sect_info->getShdrNum());

    //Generate contents for .strtab section.
    genStrTabContent(strtab_content, symstr_off, sym_name);

    //Generate content for .symtab, .sbss, .bss, .sdata, .data, .const and
    //SPM sections.
    genCommonSectionContent(symtab_content, sbss_content, sdata_content,
                            bss_content, data_content, const_content,
                            spm_content, symstr_off);

    //Generate content for .rela.sdata and .rela.data.
    if (m_sect_info->hasRelaData() || m_sect_info->hasRelaSdata()) {
        genRelocSdataAndDataContent(reloc_sdata_content, reloc_data_content);
    }

    //Construct ELF section headers and sections.
    UINT si = 0;

    //Construct NULL section header and section.
    ELFSHdr * null_shdr = getSectHeader(si++);
    constructELFNullSection(null_shdr);

    //Construct .text section header and section.
    ELFSHdr * text_shdr = getSectHeader(si++);
    constructELFTextSection(text_shdr);

    //Construct .shdr_strtab section header and section.
    ELFSHdr * shstr_shdr = getSectHeader(si++);
    constructELFShStrSection(shstr_shdr);

    //Construct .strtab section header and section.
    ELFSHdr * strtab_shdr = getSectHeader(si++);
    constructELFStrTabSection(strtab_shdr, strtab_content);

    //Construct .symtab section header and section.
    ELFSHdr * symtab_shdr = getSectHeader(si++);
    constructELFSymTabSection(symtab_shdr, strtab_shdr, text_shdr,
                              symtab_content);

    //Construct .sbss section header and section.
    if (m_sect_info->hasSbss()) {
        ELFSHdr * sbss_shdr = getSectHeader(si++);
        constructELFSbssSection(sbss_shdr, sbss_content,
                                m_sect_info->getSbssAlign());
    }

    //Construct .sdata section header and sections.
    ELFSHdr * sdata_shdr = nullptr;
    if (m_sect_info->hasSdata()) {
        sdata_shdr = getSectHeader(si++);
        constructELFSdataSection(sdata_shdr, sdata_content,
                                 m_sect_info->getSdataAlign());
    }

    //Construct .bss section header and sections.
    if (m_sect_info->hasBss()) {
        ELFSHdr * bss_shdr = getSectHeader(si++);
        constructELFBssSection(bss_shdr, bss_content,
                               m_sect_info->getBssAlign());
    }

    //Construct .data section header and sections.
    ELFSHdr * data_shdr = nullptr;
    if (m_sect_info->hasData()) {
        data_shdr = getSectHeader(si++);
        constructELFDataSection(data_shdr, data_content,
                                m_sect_info->getDataAlign());
    }

    //Construct .spm section header and sections.
    if (m_sect_info->hasSpm()) {
        ELFSHdr * spm_shdr = getSectHeader(si++);
        constructELFSpmSection(spm_shdr, spm_content,
                               m_sect_info->getSpmAlign());
    }

    //Construct .const section header and sections.
    if (m_sect_info->hasConst()) {
        ELFSHdr * const_shdr = getSectHeader(si++);
        constructELFConstSection(const_shdr, const_content,
                                 m_sect_info->getConstAlign());
    }

    //Generate contents for .text.xxx and .rel.text.xxx and construct them
    //using generated data.
    processELFTextRelSection(symtab_shdr, si);

    if (m_sect_info->hasRelaSdata()) {
        ASSERT0(sdata_shdr);
        ELFSHdr * rela_sdata_shdr = getSectHeader(si++);
        constructELFRelaSdataSection(rela_sdata_shdr, reloc_sdata_content,
                                     symtab_shdr, sdata_shdr);
    }

    if (m_sect_info->hasRelaData()) {
        ASSERT0(data_shdr);
        ELFSHdr * rela_data_shdr = getSectHeader(si++);
        constructELFRelaDataSection(rela_data_shdr, reloc_data_content,
                                    symtab_shdr, data_shdr);
    }

    if (isNeedAttributeSection()) {
        ELFSHdr * attribute_shdr = getSectHeader(si++);
        getAttributeSectionContent(attr_content);
        constructELFAttributeSection(attribute_shdr, attr_content,
            getAttributeSectionLength(), getAttributeSectionType(),
            getAttributeSectionName());
    }

    //Construct .debug_abb section header and sections.
    ELFSHdr * debug_abb_shdr = nullptr;
    if (m_sect_info->hasDebugAbbrev()) {
        debug_abb_shdr = getSectHeader(si++);
        constructELFDebugAbbrevSection(debug_abb_shdr,
            m_sect_info->getDebugAbbrevAlign());
    }

    //Construct .debug_str section header and sections.
    ELFSHdr * debug_str_shdr = nullptr;
    if (m_sect_info->hasDebugStr()) {
        debug_str_shdr = getSectHeader(si++);
        constructELFDebugStrSection(debug_str_shdr,
            m_sect_info->getDebugStrAlign());
    }

    //Construct .debug_info section header and rela sections.
    ELFSHdr * debug_info_shdr = nullptr;
    if (m_sect_info->hasDebugInfo()) {
        debug_info_shdr = getSectHeader(si++);
        constructELFDebugAndRelaSection(symtab_shdr, debug_info_shdr,
            m_sect_info->getDebugInfoAlign(), si,
            DEBUG_INFO_SH_NAME, DEBUG_RELA_INFO_SH_NAME,
            MCDWARFMGR_debug_info_fixups(getDwarfMgr()),
            MCDWARFMGR_debug_info_code(getDwarfMgr()));
    }

    //Construct .debug_ranges section header and rela sections.
    ELFSHdr * debug_ranges_shdr = nullptr;
    if (m_sect_info->hasDebugRanges()) {
        debug_ranges_shdr = getSectHeader(si++);
        constructELFDebugAndRelaSection(symtab_shdr, debug_ranges_shdr,
            m_sect_info->getDebugRangesAlign(), si,
            DEBUG_RANGES_SH_NAME, DEBUG_RELA_RANGES_SH_NAME,
            MCDWARFMGR_debug_ranges_fixups(getDwarfMgr()),
            MCDWARFMGR_debug_ranges_code(getDwarfMgr()));
    }

    //Construct .debug_line section header and rela sections.
    ELFSHdr * debug_line_shdr = nullptr;
    if (m_sect_info->hasDebugLine() && !m_rm->getDwarfMgr()->isNullRegion()) {
        debug_line_shdr = getSectHeader(si++);
        constructELFDebugAndRelaSection(symtab_shdr, debug_line_shdr,
            m_sect_info->getDebugLineAlign(), si,
            DEBUG_LINE_SH_NAME, DEBUG_RELA_LINE_SH_NAME,
            MCDWARFMGR_debug_line_fixups(getDwarfMgr()),
            MCDWARFMGR_debug_line_code(getDwarfMgr()));
    }

    //Construct .debug_frame section header and rela sections.
    ELFSHdr * debug_frame_shdr = nullptr;
    if (m_sect_info->hasDebugFrame() && !m_rm->getDwarfMgr()->isNullRegion()) {
        debug_frame_shdr = getSectHeader(si++);
        constructELFDebugAndRelaSection(symtab_shdr, debug_frame_shdr,
            m_sect_info->getDebugFrameAlign(), si,
            DEBUG_FRAME_SH_NAME, DEBUG_RELA_FRAME_SH_NAME,
            MCDWARFMGR_debug_frame_fixups(getDwarfMgr()),
            MCDWARFMGR_debug_frame_code(getDwarfMgr()));
    }

    //Construct section header indexs.
    constructELFShIndex(charvec, offvec);

    //Write data into ELF file.
    writeELF(m_output_file_name);
}


void ELFMgr::addSymRelocInfo(MOD SymbolInfo * symbol_info,
    Sym const* other, UINT type, UINT offset, UINT addend)
{
    ASSERT0(symbol_info && other);

    RelocInfo * reloc_info = m_reloc_mgr.allocRelocInfo();
    ASSERT0(reloc_info);

    RELOCINFO_caller_sym(reloc_info) = symbol_info;
    RELOCINFO_name(reloc_info) = other;
    RELOCINFO_type(reloc_info) = type;
    RELOCINFO_called_loc(reloc_info) = offset;
    RELOCINFO_addend(reloc_info) = addend;

    SYMINFO_reloc(symbol_info).append(reloc_info);
}


bool ELFMgr::verifyPreDefinedInfo()
{
    ASSERT0(checkSectDesc());
    return true;
}


Sym const* ELFMgr::getSectionName(SECTION_TYPE sect_type)
{
    ASSERT0(sect_type < SH_TYPE_MAX_NUM);
    return g_section_name_sym[sect_type];
}


SECTION_TYPE ELFMgr::getSectionType(Sym const* sect_name)
{
    ASSERT0(sect_name && m_sect_name_type_map.find(sect_name));
    return m_sect_name_type_map.get(sect_name);
}


SECTION_TYPE ELFMgr::getSectionTypeWithSplit(Sym const* sect_name)
{
    ASSERT0(sect_name);

    //e.g.: 1.given '.rodata'.
    //      2.there are 2 substrs after splited.
    //              ' ' + 'rodata'
    //        index: 0        1
    //      3.the function will return 'SH_TYPE_RODATA' that found from
    //        'm_sect_desc_info' according to '.rodata'.
    //e.g.: 1.given '.rodata.str.1'.
    //      2.there are 4 substrs after splited.
    //              ' ' + 'rodata' + 'str' + '1'
    //        index: 0        1        2      3
    //      3.the function will return 'SH_TYPE_RODATA' that found from
    //        'm_sect_desc_info' according to '.rodata'.
    StrBufVec substr_vec;
    //1.Splited by '.".
    UINT num = xcom::xsplit(sect_name->getStr(), ".", substr_vec);
    ASSERT0(num >= 2);
    //2.Assemble normal section name. 2 represents the length of '.' and '\0'.
    CHAR * name = (CHAR*)xmalloc(
        ::strlen(substr_vec.getStrBuf(1)->getBuf()) + 2);
    ASSERT0(name);
    //The format of name is ".xxx\0".
    ::sprintf(name, ".%s%c", substr_vec.getStrBuf(1)->getBuf(), '\0');
    //3.Get section type according to 'name'.
    return getSectionType(addToSymTab(name));
}


void ELFMgr::readRelaFromRelaTextSect(ELFSHdr const* shdr,
                                      OUT ELFRela & rela, size_t idx)
{
    ASSERT0(shdr);

    BYTE const* rela_content = getSectContent(shdr);
    ASSERTN(rela_content, ("no content"));
    rela.extract(rela_content + idx * ELFRela::getSize(this), this);
}


void ELFMgr::extractAssBinDescVec(OUT BYTEVec * content,
                                  AssembleBinDescVec const& abdv)
{
    ASSERT0(content);
    UINT sz = abdv.getTotalByteSize();
    UINT c_sz = content->get_elem_count();
    UINT total_sz = sz + c_sz;
    BYTEVec tmp(sz);
    xcom::AssembleBinBuf as(&(tmp[0]), sz, abdv);
    if (content->get_capacity() < total_sz) { content->grow(total_sz); }
    content->set((VecIdx)(total_sz - 1), 0);
    ::memcpy(content->get_vec() + c_sz, tmp.get_vec(), sz);
}


void ELFMgr::allocProgramHeader(UINT phnum)
{
    ELFHdr & hdr = getHdr();
    hdr.e_phnum = phnum;
    m_elf_phdr = (ELFPHdr*)xmalloc(ELFPHdr::getMachBitWidth(this) * phnum);
}


void ELFMgr::readSymFromSymtabSect(ELFSHdr const* shdr,
    OUT ELFSym & sym, size_t idx)
{
    ASSERT0(shdr);

    BYTE const* symtab_content = getSectContent(shdr);
    ASSERTN(symtab_content, ("no content"));
    sym.extract(symtab_content + idx * ELFSym::getSize(this), this);
}


UINT ELFMgr::getDynSymItemNum() const
{
    UINT num = 0;
    for (UINT i = 0; i < m_reladyn_info_vec.get_elem_count(); i++) {
        if (RELADYNINFO_is_dynsym(m_reladyn_info_vec[i])) { num++; }
    }
    return num;
}


SectionInfo * ELFMgr::getSection(Sym const* sect_name) const
{
    ASSERT0(m_sect_map.find(sect_name));
    return m_sect_map.get(sect_name);
}


BYTEVec * ELFMgr::getSectionContent(Sym const* sect_name) const
{
    ASSERT0(m_sect_map.find(sect_name));
    return SECTINFO_bytevec(m_sect_map.get(sect_name));
}


CHARVec * ELFMgr::getSectionCharVec(Sym const* sect_name) const
{
    ASSERT0(m_sect_map.find(sect_name));
    return SECTINFO_charvec(m_sect_map.get(sect_name));
}


BYTE ELFMgr::readByteFromSectionContent(Sym const* sect_name, Addr addr)
{
    BYTEVec * bytevec = getSectionContent(sect_name);
    ASSERT0(bytevec);
    return bytevec->get((VecIdx)addr);
}


void ELFMgr::writeSectionContent(Sym const* sect_name, Addr addr,
                                 BYTE const* buf, Word buflen)
{
    ASSERT0(sect_name && buf);

    BYTEVec * bytevec = getSectionContent(sect_name);
    ASSERT0(bytevec);

    //Reset size.
    UINT total_sz = (UINT)(addr + buflen);
    if (bytevec->get_capacity() < total_sz) { bytevec->grow(total_sz); }

    //Set m_last_idx.
    bytevec->set((VecIdx)(total_sz - 1), bytevec->get((VecIdx)(total_sz - 1)));
    //Copy data.
    ::memcpy(((void*)(bytevec->get_vec() + addr)), buf, buflen);
}


Addr ELFMgr::getSectionAddr(Sym const* sect_name) const
{
    ASSERT0(m_sect_map.find(sect_name));
    return SECTINFO_addr(m_sect_map.get(sect_name));
}


UINT ELFMgr::getSectionSize(Sym const* sect_name)
{
    ASSERT0(sect_name && m_sect_map.find(sect_name));
    if (sect_name == getSectionName(SH_TYPE_SHSTR) ||
        sect_name == getSectionName(SH_TYPE_SYMSTR)) {
        return getSectionCharVec(sect_name)->get_elem_count();
    }
    return getSectionContent(sect_name)->get_elem_count();
}


SectionInfo * ELFMgr::getSectionWithGenIfNotExist(Sym const* sect_name)
{
    ASSERT0(sect_name);

    bool find = false;
    SECTION_TYPE sect_type = getSectionTypeWithSplit(sect_name);
    Sym const* sect_name_sym = getSectionName(sect_type);
    ASSERT0(sect_name_sym);

    SectionInfo * si = m_sect_map.getAndGen(sect_name_sym, &find);
    ASSERT0(si);

    if (find) { return si; }
    setSectionImpl(si, sect_type);
    return si;
}


BYTEVec * ELFMgr::getSectContentWithGenIfNotExist(Sym const* sect_name)
{
    ASSERT0(sect_name);

    bool find = false;
    SECTION_TYPE sect_type = getSectionTypeWithSplit(sect_name);
    Sym const* sect_name_sym = getSectionName(sect_type);
    ASSERT0(sect_name_sym);

    SectionInfo * si = m_sect_map.getAndGen(sect_name_sym, &find);
    ASSERT0(si);

    if (find) { return SECTINFO_bytevec(si); }
    setSectionImpl(si, sect_type);
    return SECTINFO_bytevec(si);
}


CHAR const* ELFMgr::getRelaShdrType(CHAR const* shdr_name)
{
    ASSERT0(shdr_name);
    //e.g.: 1.given shdr_name.
    //           ".rela.dl_tdata"
    //      2.there are 3 substrs after splited.
    //          3 substrs: ' ' + 'rela' + 'dl_tdata'
    //      3.the index of these 3 substrs.
    //          3 substrs: ' ' + 'rela' + 'dl_tdata'
    //              index:  0      1          2
    //      4.the function will return '.dl_tdata' according to the param '2'.
    //        '2' represents the index of the substr.
    //e.g.: 1.given shdr_name.
    //          ".rela.text.func_name"
    //      2.there are 4 substrs after splited.
    //          4 substrs: ' ' + 'rela' + 'text' + 'func_name'
    //      3.the index of these 4 substrs.
    //          4 substrs: ' ' + 'rela' + 'text' + 'func_name'
    //              index:  0      1         2         3
    //      4.the function will return '.text' according to the param '2'.
    //        '2' represents the index of the substr.
    return getSubStrWithDelimViaIdxAfterSplited(shdr_name, ".", 2);
}


Sym const* ELFMgr::getShdrType(CHAR const* shdr_name)
{
    StrBufVec str_vec;
    UINT num = xcom::xsplit(shdr_name, ".", str_vec);
    //e.g.: 1.given ".text".
    //      2.there are 2 substrs after splited.
    //              ' ' + 'text'
    //        index: 0      1
    //      3.the function will return '.text'.
    //e.g.: 1.given ".text1.func_name".
    //      2.there are 3 substrs after splited
    //              ' ' + 'text1' + 'func_name'
    //        index: 0      1            2
    //      3.the function will return '.text1'.
    //e.g.: 1.given ".rodata.str1.1".
    //      2.there are 4 substrs after splited.
    //              ' ' + 'rodata' + 'str1' + '1'
    //        index: 0      1          2       3
    //      3.the function will return '.rodata'.
    //e.g.: 1.given ".rodata1.slave___GI____assert.str1.1".
    //      2.there are 5 substrs after splited.
    //              ' ' + 'rodata1' + 'slave_xxx' + 'str1' + '1'
    //        index: 0       1            2           3       4
    //      3.the function will return '.rodata1'.
    switch (num) {
    case 2:
        ASSERTN(getSectionType(addToSymTab(shdr_name)) < SH_TYPE_MAX_NUM,
            ("Invalid sect_type."));
        return addToSymTab(shdr_name);
    case 3:
    case 4:
    case 5: {
        //'1' represents the index of substr after splited.
        CHAR const* name = getSubStrWithDelimViaIdxAfterSplited(
            shdr_name, ".", FIRST_INDEX_OF_SUBSTR);
        ASSERTN(getSectionType(addToSymTab(name)) < SH_TYPE_MAX_NUM,
            ("Invalid sect_type."));
        return addToSymTab(name);
    }
    default:
        UNREACHABLE();
        break;
    }
    UNREACHABLE();
    return nullptr;
}


CHAR const* ELFMgr::getSubStrWithDelimViaIdxAfterSplited(
    CHAR const* str, CHAR const* delim, UINT idx)
{
    ASSERT0(str && delim);
    //e.g.: 1.given ".rela.dl_tdata".
    //      2.there are 3 substrs after splited.
    //              ' ' + 'rela' + 'dl_tdata'
    //         idx:  0      1          2
    //e.g.: 1.given ".rela.text.func_name".
    //      2.there are 4 substrs after splited.
    //              ' ' + 'rela' + 'text' + 'func_name'
    //         idx:  0      1         2          3
    StrBufVec str_vec;
    UINT num = xcom::xsplit(str, delim, str_vec);
    ASSERT0(idx < num);

    //2 represents the length of '.' and '\0'.
    UINT len = (UINT)::strlen(str_vec.getStrBuf(idx)->getBuf()) + 2;
    CHAR * sub_str = (CHAR*)xmalloc(len);
    ASSERT0(sub_str);
    //the sub_str format is ".xxx\0".
    ::sprintf(sub_str, ".%s%c", str_vec.getStrBuf(idx)->getBuf(), '\0');
    return sub_str;
}


SymbolInfo * ELFMgr::getSymbolInfo(Sym const* symbol_name)
{
    ASSERT0(symbol_name);
    ASSERTN(m_symbol_info_map.find(symbol_name),
            ("No symbol %s", symbol_name->getStr()));
    return m_symbol_info_map.get(symbol_name);
}


void ELFMgr::setELFType(UINT type)
{
    ELFHdr & hdr = getHdr();
    hdr.e_type = type;
}


SectionDesc const& ELFMgr::getSectionDescElem(SECTION_TYPE sect_type)
{
    ASSERTN(sect_type < SH_TYPE_MAX_NUM, ("Invalid sect_type."));
    return g_section_desc[sect_type];
}


SECTION_TYPE const* ELFMgr::getDynamicSectionDesc() const
{
    return g_dynamic_section_desc;
}


void ELFMgr::initELFMgrInfo(MOD SymTab * sym_tab, CHAR const* file_path,
                            bool is_elf_format)
{
    ASSERT0(sym_tab);

    setSymTab(sym_tab);
    initSectionInfo();

    m_have_elf_format = is_elf_format;
    m_file_name = (file_path == nullptr) ? nullptr : processELFName(file_path);
}


CHAR const* ELFMgr::getSubStr(CHAR const* str, CHAR const* exclude_str)
{
    ASSERT0(str && exclude_str);

    //e.g.: Get substr "func_name" from ".text1.func_name" or ".text.func_name".
    //      And 'exclude_str' is ".text1." or  ".text.".
    UINT exclude_len = (UINT)::strlen(exclude_str);
    UINT len = (UINT)::strlen(str) - exclude_len + 1;
    CHAR const* substr = (CHAR*)xmalloc(len);
    ASSERT0(substr);
    ::memcpy((void*)substr, (void*)(str + exclude_len), len);
    return substr;
}


CHAR const* ELFMgr::getRelaName(ELFSHdr const* symtab, size_t idx)
{
    ASSERT0(symtab && symtab->isSymTab());

    ELFSym sym;
    BYTE const* symtabcontent = getSectContent(symtab);
    ASSERTN(symtabcontent, ("no content"));
    sym.extract(symtabcontent + idx * ELFSym::getSize(this), this);
    ELFSHdr const* strtab = symtab->getRelatedStrTab(this);
    ASSERT0(strtab);
    return getSymNameFromStrTabOrDefinedSection(sym, strtab);
}


void ELFMgr::setSymbolValueHelper(MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    Word name = ELF_VAL_UNDEF;
    Addr value = ELF_VAL_UNDEF;
    Half shndx = getStShndxOfUndefSection();
    UCHAR type = SYMINFO_is_func(symbol_info) ? STT_FUNC : STT_OBJECT;
    UCHAR bind = SYMINFO_is_weak(symbol_info) ? STB_WEAK :
        (SYMINFO_is_global(symbol_info) ? STB_GLOBAL : STB_LOCAL);
    UCHAR other = SYMINFO_is_visible(symbol_info) ? STV_DEFAULT : STV_HIDDEN;
    other = SYMINFO_is_func(symbol_info) ? (getSymOtherInfo() | other) : other;

    //Set the value of ELFSym.
    setSymbolValue(&(SYMINFO_sym(symbol_info)), name, bind,
        type, other, shndx, value, SYMINFO_size(symbol_info));
    //Update SymbolInfo.
    SYMINFO_index(symbol_info) = shndx;
}


void ELFMgr::collectSymtabInfoFromVar()
{
    xcom::List<Sym const*>::Iter iter;

    for (m_symbol_name.get_head(&iter); iter != nullptr;
         m_symbol_name.get_next(&iter)) {

        Sym const* sym_name = iter->val();
        ASSERT0(sym_name);

        SymbolInfo * symbol_info = m_symbol_info.get(sym_name);
        ASSERT0(symbol_info);

        //Collect debug info of 'symbol_info'.
        if (SYMINFO_is_debug(symbol_info)) {
            collectDebugInfo(symbol_info);
        }

        SYMINFO_sect_name(symbol_info) =
            getSectionNameViaSymbolInfoHelper(symbol_info);
        //Reset sym. Change from SymTab in RegionMgr to SymTab in ELFMgr.
        SYMINFO_name(symbol_info) =
            addToSymTab(SYMINFO_name(symbol_info)->getStr());

        if (SYMINFO_is_func(symbol_info) && !SYMINFO_is_global(symbol_info)) {
            SYMINFO_is_extern(symbol_info) = true;
        }

        //Process SymbolInfo with BSS/SBSS and extern attribute.
        if (SYMINFO_is_extern(symbol_info) &&
            (SYMINFO_sect_type(symbol_info) == SH_TYPE_SBSS ||
             SYMINFO_sect_type(symbol_info) == SH_TYPE_BSS)) {
            SYMINFO_sect_type(symbol_info) = SH_TYPE_EMPTY;
            SYMINFO_sect_name(symbol_info) = nullptr;
        }

        //Set the value of SYMINF_sym(symbol_info).
        setSymbolValueHelper(symbol_info);

        //Process FunctionInfo.
        collectFunctionInfo(symbol_info);

        //Process RelocInfo.
        collectRelocInfo(symbol_info);

        //Add symbol_info into 'm_symbol_info_vec' and 'm_symbol_info_map'.
        setSymbolInfo(symbol_info);
    }
}


void ELFMgr::collectDebugInfo(MOD SymbolInfo const* symbol_info)
{
    xoc::MCDwarfMgr * dwarf_mgr = m_rm->getDwarfMgr();
    ASSERT0(symbol_info && SYMINFO_is_debug(symbol_info) && dwarf_mgr);

    switch (SYMINFO_sect_type(symbol_info)) {
    case SH_TYPE_DEBUG_INFO:
        genDebugReloc(symbol_info, MCDWARFMGR_debug_info_fixups(dwarf_mgr),
            MCDWARFMGR_debug_info_code(dwarf_mgr));
        break;
    case SH_TYPE_DEBUG_LINE:
        genDebugReloc(symbol_info, MCDWARFMGR_debug_line_fixups(dwarf_mgr),
            MCDWARFMGR_debug_line_code(dwarf_mgr));
        break;
    case SH_TYPE_DEBUG_FRAME:
        genDebugReloc(symbol_info, MCDWARFMGR_debug_frame_fixups(dwarf_mgr),
            MCDWARFMGR_debug_frame_code(dwarf_mgr));
        break;
    case SH_TYPE_DEBUG_RANGES:
        genDebugReloc(symbol_info, MCDWARFMGR_debug_ranges_fixups(dwarf_mgr),
            MCDWARFMGR_debug_ranges_code(dwarf_mgr));
        break;
    case SH_TYPE_DEBUG_ABBREV:
    case SH_TYPE_DEBUG_STR:
    case SH_TYPE_DEBUG_LOC:
        break;
    default:
        UNREACHABLE();
        break;
    }
}


void ELFMgr::collectFunctionInfo(SymbolInfo const* symbol_info)
{
    ASSERT0(symbol_info);

    if (!(SYMINFO_is_func(symbol_info))) { return; }

    FunctionInfo * fi = SYMINFO_func(symbol_info);
    ASSERT0(fi);

    FUNCINFO_name(fi) = SYMINFO_name(symbol_info);
    FUNCINFO_sect_type(fi) = getSectNameOfFunc(fi);
    FUNCINFO_sect_name(fi) = getSectNameOfFuncVar(fi);
    FUNCINFO_size(fi) = SYMINFO_size(symbol_info);

    m_func_info.append(fi);
}


void ELFMgr::collectRelocInfo(MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    UINT reloc_info_elem = SYMINFO_reloc(symbol_info).get_elem_count();
    if (reloc_info_elem == 0) { return; }

    for (UINT i = 0; i < reloc_info_elem; i++) {
        RelocInfo * ri = (SYMINFO_reloc(symbol_info)[i]);
        ASSERT0(ri);
        m_reloc_info.append(ri);

        //Re-register RelocInfo name into the SymTab of current ELFMgr.
        RELOCINFO_name(ri) = addToSymTab(RELOCINFO_name(ri)->getStr());
        RELOCINFO_sect_name(ri) =
            addToSymTab(SYMINFO_sect_name(symbol_info)->getStr());
        //Function SymbolInfo.
        if (SYMINFO_is_func(symbol_info)) {
            RELOCINFO_is_func(ri) = true;
            RELOCINFO_caller_func(ri) = SYMINFO_func(symbol_info);
            continue;
        }

        //Object SymbolInfo.
        RELOCINFO_is_object(ri) = true;
        RELOCINFO_caller_sym(ri) = symbol_info;
    }
}


void ELFMgr::initDebugInfo()
{
    ASSERT0(m_rm);
    xoc::MCDwarfMgr * dm = getDwarfMgr();
    ASSERT0(dm);

    //Generate some debug info.
    dm->genFrameBinary();
    dm->genLineBinary();
}


void ELFMgr::collectELFInfoFromVar()
{
    if (xoc::g_debug) { initDebugInfo(); }
    extractSymbolExceptUserDefFunc();

    //Collected symtab info.
    collectSymtabInfoFromVar();
}


void ELFMgr::constructELFSectionHelper()
{
    //Construct sections according to 'm_sect_map'.
    constructELFSection();

    //Process .strtab section.
    OffVec offvec;
    CHARVec * charvec = getSectionCharVec(SH_TYPE_SHSTR);
    ASSERT0(charvec);

    genSectHeaderNameStrTabContent(charvec, offvec);
    setSectHeaderNameStrTabIdx();
    setSectHeaderNameOffset(offvec);
    setSectHeaderNameStrTabContent(
        (BYTE*)charvec->get_vec(), charvec->get_elem_count());
}


void ELFMgr::setSymbolInfo(MOD SymbolInfo * target_symbol_info)
{
    ASSERT0(target_symbol_info);

    //Record file name.
    SYMINFO_file_name(target_symbol_info) =
        (m_file_name == nullptr) ? nullptr : addToSymTab(m_file_name);

    //FIXME: Not all symbols recorded into vector/map ?
    //Check symbol info.
    if (m_symbol_info_map.find(SYMINFO_name(target_symbol_info))) {
        //'target_symbol_info' is extern symbol and defined symbol has
        //been recorded. Thus 'target_symbol_info' don't need to record.
        if (SYMINFO_sym(target_symbol_info).st_shndx == 0 ||
            SYMINFO_is_extern(target_symbol_info)) { return; }
        SymbolInfo * symbol_info =
            m_symbol_info_map.get(SYMINFO_name(target_symbol_info));
        ASSERT0(symbol_info);

        if (SYMINFO_sym(symbol_info).st_shndx == 0 ||
            SYMINFO_is_extern(symbol_info)) {
            //'symbol_info' is extern symbol. Remove it and re-record
            //'target_symbol_info' into vector/map.
            m_symbol_info_map.remove(SYMINFO_name(symbol_info));
        } else {
            prt2C("\nerror: multi definition of '%s'",
                  SYMINFO_name(target_symbol_info)->getStr());
            UNREACHABLE();
        }
    }

    ASSERT0(!m_symbol_info_map.find(SYMINFO_name(target_symbol_info)));
    //Record symbol info into vector/map.
    m_symbol_info_map.set(SYMINFO_name(target_symbol_info), target_symbol_info);
    m_symbol_info_vec.append(target_symbol_info);
}


void ELFMgr::processDataSectionAlign(MOD AssembleBinDescVec & desc_vec,
    MOD SectionInfo * sect_info, MOD SymbolInfo * symbol_info)
{
    ASSERT0(sect_info && symbol_info);
    //Process data align. And pad data if need.
    //1.Get current size(also addr).
    UINT cur_size = (UINT)SECTINFO_size(sect_info);
    //2.Calculate the align size.
    UINT align_sz =
        (UINT)(xcom::ceil_align(cur_size, SYMINFO_align(symbol_info))) -
        cur_size;
    ASSERT0(align_sz >= 0);
    //3.Calculate the total size(add the symbol size for stored
    //  symbol data later).
    UINT sz = (UINT)(SECTINFO_size(sect_info) + align_sz +
              SYMINFO_size(symbol_info));
    //4.Process capacity.
    if (desc_vec.get_capacity() < sz) { desc_vec.grow(sz); }
    //5.Pad.
    for (UINT i = 0; i < align_sz; i++) {
        AssembleBinDesc asbd(BIT_PER_BYTE, BinWord(0));
        desc_vec.set(cur_size + i, asbd);
        SECTINFO_size(sect_info)++;
    }
}


void ELFMgr::setSymbolDataToSection(MOD AssembleBinDescVec & desc_vec,
    MOD SectionInfo * sect_info, MOD SymbolInfo * symbol_info)
{
    ASSERT0(sect_info && symbol_info);

    if (!SYMINFO_is_byte(symbol_info)) {
        //It is scalar data.
        processDataSectionAlign(desc_vec, sect_info, symbol_info);
        //Stored data.
        AssembleBinDesc asbd((UINT)SYMINFO_size(symbol_info) * BIT_PER_BYTE,
                             SYMINFO_data_word(symbol_info));
        desc_vec.append(asbd);
        //Upate addr_align.
        SECTINFO_align(sect_info) =
            MAX(SECTINFO_align(sect_info), SYMINFO_align(symbol_info));
        //Update symbol offset in correspond section.
        SYMINFO_ofst(symbol_info) = SECTINFO_size(sect_info);
        //Update section size.
        SECTINFO_size(sect_info) += SYMINFO_size(symbol_info);
        return;
    }

    //It is vector data.
    processDataSectionAlign(desc_vec, sect_info, symbol_info);
    AssembleBinDesc asbd((UINT)SYMINFO_size(symbol_info) * BIT_PER_BYTE,
                         SYMINFO_data_byte(symbol_info));
    desc_vec.append(asbd);
    //Update addr_align.
    SECTINFO_align(sect_info) =
        MAX(SECTINFO_align(sect_info), SYMINFO_align(symbol_info));
    //Update symbol offset in correspond section.
    SYMINFO_ofst(symbol_info) = SECTINFO_size(sect_info);
    //Update section size.
    SECTINFO_size(sect_info) += SYMINFO_size(symbol_info);
}


void ELFMgr::genSectionContentHelper()
{
    //Generate .strtab content.
    CHARVec * strtab_content = getSectionCharVec(SH_TYPE_SYMSTR);
    ASSERT0(strtab_content);
    genStrTabContent(strtab_content);

    //Generate .symtab content.
    BYTEVec * symtab_content = getSectionContent(SH_TYPE_SYMTAB);
    BYTEVec * dynsym_content = getSectionContent(SH_TYPE_DYNSYM);
    ASSERT0(symtab_content && dynsym_content);

    genSymTabContent(symtab_content, dynsym_content);
    genMiscSectionContent();
}


void ELFMgr::processSectionInfo()
{
    //Create fundamental section.
    //Create null section.
    setSection(SH_TYPE_UNDEF);
    //Create .interp section.
    setSection(SH_TYPE_INTERP);
    //Create .shdr_strtab section.
    setSection(SH_TYPE_SHSTR);
    //Create .strtab section.
    setSection(SH_TYPE_SYMSTR);
    //Create .symtab section.
    setSection(SH_TYPE_SYMTAB);
    //Create .fini section.
    setSection(SH_TYPE_FINI);
    //Create .preinit_array section.
    setSection(SH_TYPE_PREINIT_ARRAY);
    //Create .rela_dyn section.
    setSection(SH_TYPE_RELA_DYN);
    //Create .dynsym section.
    setSection(SH_TYPE_DYNSYM);
    //Create .got section.
    setSection(SH_TYPE_GOT);
    //Create .dynamic section.
    setSection(SH_TYPE_DYNAMIC);
}


void ELFMgr::initProgramHeader()
{
    //The value of p_offset/p_vaddr/p_paddr will be set by the begin section
    //of each program header type after the address of all sections have been
    //set. Now these value just initialied by the max number.
    ELFHdr & hdr = getHdr();
    for (UINT i = 0; i < hdr.e_phnum; i++) {
        ELFPHdr * ph = getProgramHeader(i);
        ASSERT0(ph);
        ph->p_offset = getMaxOffsetOfELF();
        ph->p_vaddr = getMaxAddrOfELF();
        ph->p_paddr = getMaxAddrOfELF();
    }
}


void ELFMgr::processProgramHeader()
{
    allocProgramHeader(getProgramHeaderNum());
    initProgramHeader();

    SectionInfo * sect_info;
    SectionInfoMapIter iter;
    for (m_sect_map.get_first(iter, &sect_info); !iter.end();
         m_sect_map.get_next(iter, &sect_info)) {
        ASSERT0(sect_info);
        //Don't need to process.
        if (SECTINFO_ph_type(sect_info) == PH_TYPE_UNDEF) { continue; }
        size_t ph_index = GET_PH_TYPE_INDEX(SECTINFO_ph_type(sect_info));
        ASSERT0(ph_index >= 0);
        ELFPHdr * ph = getProgramHeader(ph_index);
        ASSERT0(ph);
        //The value of p_offset/p_vaddr/p_paddr set by the begin section of
        //each program header type(e.g. PH_TYPE_CODE/DATA/DYNAMIC).
        ph->p_offset = MIN(ph->p_offset, (Off)SECTINFO_ofst(sect_info));
        ph->p_vaddr = MIN(ph->p_vaddr, (Off)SECTINFO_addr(sect_info));
        ph->p_paddr = MIN(ph->p_paddr, (Off)SECTINFO_addr(sect_info));
        ph->p_filesz += (Word32)SECTINFO_size(sect_info);

        switch (SECTINFO_ph_type(sect_info)) {
        case PH_TYPE_CODE:
            ph->p_type = PT_LOAD;
            ph->p_flags = PF_R | PF_X;
            ph->p_memsz = ph->p_filesz;
            ph->p_align = PHDR_CODE_ALIGN;
            break;
        case PH_TYPE_DATA:
            ph->p_type = PT_LOAD;
            ph->p_flags = PF_R | PF_W | PF_X;
            ph->p_memsz = ph->p_filesz;
            ph->p_align = PHDR_DATA_ALIGN;
            break;
        case PH_TYPE_DYNAMIC:
            //Reset filesz accordiong to section content.
            //Since processDynamic called after sect_total_size set.
            ph->p_filesz = getSectionSize(SECTINFO_name(sect_info));
            ph->p_type = PT_DYNAMIC;
            ph->p_flags = PF_R | PF_W;
            ph->p_memsz = ph->p_filesz;
            ph->p_align = PHDR_DYNAMIC_ALIGN;
            break;
        case PH_TYPE_UNDEF:
            break;
        default:
            UNREACHABLE();
            break;
        }
    }
}


bool ELFMgr::hasBeenRecordedRelaDynInfoItem(
    RelocInfo const* reloc_info, OUT RelocInfo ** out_reloc_info)
{
    ASSERT0(reloc_info);

    for (UINT i = 0; i < m_reladyn_info_vec.get_elem_count(); i++) {
        RelaDynInfo * reladyn = m_reladyn_info_vec[i];
        ASSERT0(reladyn);

        if (RELADYNINFO_sym_name(reladyn) !=
            SYMINFO_name(RELOCINFO_sym(reloc_info))) { continue; }

        if (RELADYNINFO_is_got(reladyn)) {
            *out_reloc_info = RELADYNINFO_reloc_info(reladyn);
            return true;
        }

        if ((RELADYNINFO_type(reladyn) == RELOCINFO_type(reloc_info)) &&
            (RELADYNINFO_addend(reladyn) == RELOCINFO_addend(reloc_info)) &&
            (RELADYNINFO_ofst(reladyn) == (RELOCINFO_called_loc(reloc_info))) &&
            ((RELADYNINFO_caller_sym_name(reladyn) ==
             (RELOCINFO_sect_name(reloc_info))))) {
            *out_reloc_info = RELADYNINFO_reloc_info(reladyn);
            return true;
        }
    }
    return false;
}


void ELFMgr::setRelaDynInfo(MOD RelocInfo * reloc_info,
                            Off & got_ofst, UINT & dynsym_idx)
{
    ASSERT0(reloc_info);

    RelaDynInfo * rela = m_reladyn_mgr.allocRelaDynInfo();
    ASSERT0(rela);

    RELADYNINFO_reloc_info(rela) = reloc_info;
    RELADYNINFO_is_got(rela) = hasGotItem(reloc_info);
    RELADYNINFO_is_dynsym(rela) =
        (!isSymbolWithLocalAttr(RELOCINFO_sym(reloc_info)));

    if (RELADYNINFO_is_got(rela)) {
        RELOCINFO_sect_ofst(reloc_info) = got_ofst;
        rela->m_got_ofst = got_ofst;
        got_ofst += getElemByteSizeInGotSect();
    }

    if (RELADYNINFO_is_dynsym(rela) &&
        !SYMINFO_is_dynsym(RELOCINFO_sym(reloc_info))) {
        SYMINFO_is_dynsym(RELOCINFO_sym(reloc_info)) = true;
        RELADYNINFO_sym_idx(rela) = dynsym_idx;
        ++dynsym_idx;
    }

    m_reladyn_info_vec.append(rela);
}


SWord ELFMgr::processRelaDynAddend(RelocInfo const* reloc_info)
{
    ASSERT0(reloc_info);
    return (isSymbolWithLocalAttr(RELOCINFO_sym(reloc_info))) ?
        (getSymbolAddr(RELOCINFO_sym(reloc_info))) : 0;
}


Addr ELFMgr::processRelaDynOffset(RelocInfo const* reloc_info, UINT & idx)
{
    Addr ofst = (idx++) * getElemByteSizeInGotSect();
    return getSectionAddr(SH_TYPE_GOT) + ofst;
}


void ELFMgr::processRelaDynSectAfterSetSectAddr()
{
    UINT got_item_idx = 0;
    UINT local_idx = 0;
    UINT global_idx = 0;
    UINT rela_sz = ELFRela::getSize(this);
    BYTEVec * content = getSectionContent(SH_TYPE_RELA_DYN);
    ASSERT0(content);
    BYTE * rela = (BYTE*)ALLOCA(rela_sz);
    ASSERT0(rela);

    for (UINT i = 0; i < m_reladyn_info_vec.get_elem_count(); i++) {
        RelaDynInfo * reladyn_info = m_reladyn_info_vec[i];
        ASSERT0(reladyn_info);
        RelocInfo * reloc_info = RELADYNINFO_reloc_info(reladyn_info);
        ASSERT0(reloc_info);

        bool is_local = isSymbolWithLocalAttr(RELOCINFO_sym(reloc_info));

        //Set value to ELFRela.
        ELFRela::setType(rela, processRelaDynType(reloc_info), this);
        ELFRela::setAddend(rela, processRelaDynAddend(reloc_info), this);
        ELFRela::setOffset(rela,
            processRelaDynOffset(reloc_info, got_item_idx), this);
        ELFRela::setSym(rela,
            (is_local ?  0 : RELADYNINFO_sym_idx(reladyn_info)), this);

        //Write to content.
        Addr addr = is_local ?
            (local_idx) : ((getRelaDynLocalItemNum() * rela_sz) + global_idx);
        content->set((VecIdx)(addr + rela_sz - 1), 0);
        ::memcpy((void*)(content->get_vec() + addr), (void*)rela, rela_sz);

        //Update local/global idx.
        local_idx += is_local ? rela_sz : 0;
        global_idx += is_local ? 0 : rela_sz;
    }
}


void ELFMgr::genGotContent()
{
    //There isn't GOT content.
    if (getGotElemNum() == 0) { return; }

    //The data of GOT is loaded by driver in execute phase.
    //Now linker just allocates memory.
    BYTEVec * got_content = getSectionContent(SH_TYPE_GOT);
    ASSERT0(got_content);

    UINT size = (UINT)getGotElemNum() * getElemByteSizeInGotSect();
    if (got_content->get_capacity() < size) { got_content->grow(size); }
    got_content->set(got_content->get_capacity() - 1, 0);
    ::memset((void*)(got_content->get_vec()), 0, size);
}


void ELFMgr::createDynamicElement(OUT AssembleBinDescVec & desc_vec,
                                  SWord tag, Addr val, UINT ind)
{
     ELFDyn dynamic;
     dynamic.d_tag = tag;
     dynamic.d_val = val;

     AssembleBinDesc d0(sizeof(dynamic.d_tag) * BITS_PER_BYTE, dynamic.d_tag);
     AssembleBinDesc d1(sizeof(dynamic.d_val) * BITS_PER_BYTE, dynamic.d_val);

     desc_vec.set(ind * ELF_NUM_OF_ELEM_IN_ELFDYN, d0);
     desc_vec.set(
         ind * ELF_NUM_OF_ELEM_IN_ELFDYN + ELF_FIRST_ELEM_OF_ELFDYN , d1);
}


void ELFMgr::processDynamicSection()
{
    UINT ind = 0;
    AssembleBinDescVec dynamic_desc_vec;
    //Get dynamic section info from description table.
    SECTION_TYPE const* section_desc = getDynamicSectionDesc();
    ASSERT0(section_desc);
    UINT section_num = sizeof(section_desc) / sizeof(section_desc[0]);

    for (UINT i = 0; i < section_num; i++) {
        SectionInfo * sect_info = getSection(section_desc[i]);
        ASSERT0(sect_info);
        Addr addr = getSectionAddr(section_desc[i]);
        switch (section_desc[i]) {
        case SH_TYPE_RELA_DYN: {
            Addr size = (SECTINFO_bytevec(sect_info))->get_elem_count();
            createDynamicElement(dynamic_desc_vec, DT_RELA, addr, ind++);
            createDynamicElement(dynamic_desc_vec, DT_RELASZ, size, ind++);
            createDynamicElement(dynamic_desc_vec, DT_RELAENT,
                ELFRel::getSize(this), ind++);
            break;
        }
        case SH_TYPE_SYMTAB: {
            createDynamicElement(dynamic_desc_vec, DT_SYMTAB, addr, ind++);
            createDynamicElement(dynamic_desc_vec, DT_SYMENT,
                ELFRel::getSize(this), ind++);
            break;
        }
        case SH_TYPE_SYMSTR: {
            Addr size = (SECTINFO_charvec(sect_info))->get_elem_count();
            createDynamicElement(dynamic_desc_vec, DT_STRTAB, addr, ind++);
            createDynamicElement(dynamic_desc_vec, DT_STRSZ, size, ind++);
            break;
        }
        default:
            UNREACHABLE();
            break;
        }
    }

    //Set .dynamic content.
    //The first item of .got section content will be refilled by
    //the address of .dynamic section according to the ELF format.
    BYTEVec * content = getSectionContent(SH_TYPE_DYNAMIC);
    UINT sz = dynamic_desc_vec.getTotalByteSize();
    UINT c_sz = content->get_elem_count();
    ASSERT0(sz == c_sz);
    BYTEVec tmp(sz);
    xcom::AssembleBinBuf as(&(tmp[0]), sz, dynamic_desc_vec);
    content->set(content->get_capacity() - 1, 0);
    ::memcpy(content->get_vec(), tmp.get_vec(), sz);

    //Refill dynamic base address to the first item of
    //.got section according to ELF format required.
    Addr refill_addr = getSectionAddr(SH_TYPE_DYNAMIC);
    writeSectionContent(getSectionName(SH_TYPE_GOT), 0,
        (BYTE*)(&refill_addr), getElemByteSizeInGotSect());
}


void ELFMgr::updateSymOffset(SECTION_TYPE sect_type)
{
    UINT elem_size = ELFSym::getSize(this);
    BYTEVec * bytevec = getSectionContent(sect_type);
    ASSERT0(bytevec);
    UCHAR * content = bytevec->get_vec();
    ASSERT0(content);

    UINT st_value_ofst = getStValueOffsetInELFSym();
    UINT st_shndx_ofst = getStShndxOffsetInELFSym();
    Half st_undef = getStShndxOfUndefSection();
    for (UINT i = 0; i < bytevec->get_elem_count(); i += elem_size) {
        //Read origin value.
        Half index = *(Half*)(content + st_shndx_ofst + i);
        Addr value = *(Addr*)(content + st_value_ofst + i);
        //Get corresponded section begin addr of current symbol.
        if (index == SHN_ABS || index == st_undef) { continue; }
        //New value = origin value + base addr.
        value += getSectionAddr(getSectionNameByIndex(index));
        //Refill.
        *(Addr*)(content + i + st_value_ofst) = value;
    }
}


Addr ELFMgr::getSymbolAddr(SymbolInfo const* symbol_info)
{
    ASSERT0(symbol_info);

    Addr sect_base_addr = getSectionAddr(SYMINFO_sect_type(symbol_info));
    Addr offset = SYMINFO_is_func(symbol_info) ?
        FUNCINFO_code_ofst(SYMINFO_func(symbol_info)) :
        SYMINFO_ofst(symbol_info);
    return sect_base_addr + offset;
}


void ELFMgr::postProcessAfterSetSectAddr()
{
    setEntryPointInELFHdr(SH_TYPE_TEXT);

    //Update element offset in '.dynsym'.
    updateSymOffset(SH_TYPE_DYNSYM);

    //Update element offset in '.symtab'.
    updateSymOffset(SH_TYPE_SYMTAB);
}


void ELFMgr::setSection(SECTION_TYPE sect_type)
{
    bool find = false;
    Sym const* sym_name = getSectionName(sect_type);
    ASSERT0(sym_name);

    SectionInfo * si = m_sect_map.getAndGen(sym_name, &find);
    ASSERT0(si);
    if (find) { return; }

    ASSERT0(sect_type < SH_TYPE_MAX_NUM);
    setSectionImpl(si, sect_type);
}


void ELFMgr::setSection(SECTION_TYPE sect_type,
                        CHAR const* sect_name, UINT sect_index)
{
    ASSERT0(sect_name);

    bool find = false;
    Sym const* sym_name = addToSymTab(sect_name);
    ASSERT0(sym_name);

    SectionInfo * si = m_sect_map.getAndGen(sym_name, &find);
    ASSERT0(si);
    if (find) { return; }

    SectionDesc const& sect_desc = getSectionDescElem(sect_type);

    //Re-set info.
    SECTINFO_type(si) = sect_type;
    SECTINFO_name(si) = sym_name;
    SECTINFO_ph_type(si) = SECTDESC_ph_type(&sect_desc);
    SECTINFO_shdr_type(si) = SECTDESC_shdr_type(&sect_desc);
    SECTINFO_flag(si) = getSectionFlags(&sect_desc);
    SECTINFO_align(si) = SECTDESC_align(&sect_desc);
    SECTINFO_entry_size(si) = SECTDESC_entry_sz(&sect_desc);
    SECTINFO_index(si) = sect_index;
}


void ELFMgr::setSectionImpl(MOD SectionInfo * si, SECTION_TYPE sect_type)
{
    ASSERT0(si);

    SectionDesc const& sect_desc = getSectionDescElem(sect_type);

    SECTINFO_type(si) = sect_type;
    SECTINFO_name(si) = getSectionName(sect_type);
    SECTINFO_ph_type(si) = SECTDESC_ph_type(&sect_desc);
    SECTINFO_shdr_type(si) = SECTDESC_shdr_type(&sect_desc);
    SECTINFO_flag(si) = getSectionFlags(&sect_desc);
    SECTINFO_align(si) = SECTDESC_align(&sect_desc);
    SECTINFO_entry_size(si) = SECTDESC_entry_sz(&sect_desc);
    SECTINFO_index(si) = 0;
}

void ELFMgr::setSectionOrder()
{
    //Set section order according to the section index.
    //Map order_index <-> section_name.
    SectionInfo * sect_info;
    SectionInfoMapIter iter;
    for (m_sect_map.get_first(iter, &sect_info); !iter.end();
         m_sect_map.get_next(iter, &sect_info)) {
        SECTINFO_index(sect_info) += getSectionIndex(SECTINFO_type(sect_info));
        m_sect_layout.set((VecIdx)(SECTINFO_index(sect_info)),
            SECTINFO_name(sect_info));
    }
}


void ELFMgr::processSectionOffset()
{
    Off section_base = 0;
    //Process shdr offset field. Offset value means the section
    //begin addr(offset) in current elf file.
    for (UINT i = 0; i < getShdrNum(); i++) {
        SectionInfo * si = getSection(m_sect_layout.get(i));
        ASSERT0(si);

        switch (SECTINFO_type(si)) {
        case SH_TYPE_UNDEF:
            ASSERT0(i == 0);
            break;
        case SH_TYPE_INTERP:
            //.initerp as base section.
            SECTINFO_ofst(si) = SHDR_OFFSET_ALIGN;
            SECTINFO_size(si) = getSectionSize(m_sect_layout.get(i));
            break;
        case SH_TYPE_PREINIT_ARRAY:
            //.preinit_addry as data segment begin section.
            SECTINFO_ofst(si) =
                xcom::ceil_align(section_base, SHDR_OFFSET_ALIGN);
            SECTINFO_size(si) = getSectionSize(m_sect_layout.get(i));
            break;
        SWITCH_CASE_COMMON_SECT_OFST:
            SECTINFO_ofst(si) =
                xcom::ceil_align(section_base, SECTINFO_align(si));
            SECTINFO_size(si) = (SECTINFO_ofst(si) - section_base) +
                getSectionSize(m_sect_layout.get(i));
            break;
        default:
            UNREACHABLE();
            break;
        }
        section_base = SECTINFO_ofst(si) + getSectionSize(m_sect_layout.get(i));
    }
    setMaxOffsetOfELF(section_base);
}


void ELFMgr::processSectionAddr()
{
    Addr current_addr = 0;
    for (UINT i = 0; i < getShdrNum(); i++) {
        SectionInfo * section_info = getSection(m_sect_layout.get(i));
        ASSERT0(section_info);

        switch (SECTINFO_type(section_info)) {
        case SH_TYPE_UNDEF:
            ASSERT0(i == 0);
            break;
        case SH_TYPE_SHSTR:
        case SH_TYPE_SYMSTR:
        case SH_TYPE_SYMTAB:
        SWITCH_CASE_DEBUG_SECT:
            break;
        case SH_TYPE_PREINIT_ARRAY:
            current_addr = xcom::ceil_align(current_addr,
                SHDR_VIRTUAL_ADDR_ALIGN);
            SECTINFO_addr(section_info) = current_addr;
            current_addr += getSectionSize(m_sect_layout.get(i));
            break;
        SWITCH_CASE_COMMON_SECT_ADDR:
            current_addr = xcom::ceil_align(current_addr,
                SECTINFO_align(section_info));
            SECTINFO_addr(section_info) = current_addr;
            current_addr += getSectionSize(m_sect_layout.get(i));
            break;
        default:
            UNREACHABLE();
            break;
        }
    }
    setMaxAddrOfELF(current_addr);
}


void ELFMgr::collectSymbolInfoSectionName(
    ELFHdr & hdr, MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    //Get current symtab correspond data section.
    ELFSHdr * shdr = getSectHeader(SYMINFO_sym(symbol_info).st_shndx);
    ASSERT0(shdr);
    //Get .shstrtab section.
    ELFSHdr * shstrtab_shdr = getSectHeader(hdr.e_shstrndx);
    ASSERT0(shstrtab_shdr);
    //Get data section name.
    CHAR const* shdr_name = getStrFromStrTab(shstrtab_shdr, shdr->s_name);
    ASSERT0(shdr_name);

    //If it is function symbol, it need to get corresponded section name.
    //Now there are two type of function section: .text and .text1.
    if (SYMINFO_sym(symbol_info).st_type == STT_FUNC) {
        SYMINFO_is_func(symbol_info) = true;
        //It need to get function name if it is function symbol,
        //The function name may be different with symbol name.
        SYMINFO_func_name(symbol_info) = getFunctionName(shdr_name);
        SYMINFO_sect_type(symbol_info) =
            getSectionTypeWithSplit(addToSymTab(shdr_name));
        SYMINFO_sect_name(symbol_info) =
            getSectionName(SYMINFO_sect_type(symbol_info));
        return;
    }

    SYMINFO_sect_type(symbol_info) =
        getSectionTypeWithSplit(addToSymTab(shdr_name));
    SYMINFO_sect_name(symbol_info) =
        getSectionName(SYMINFO_sect_type(symbol_info));
}


bool ELFMgr::existRelaInfo(ELFHdr & hdr, SymbolInfo const* symbol_info)
{
    ASSERT0(symbol_info);

    //Check whether there is reloc info which belong to the 'symbol_info'.
    for (UINT i = 0; i < hdr.e_shnum; i++) {
        ELFSHdr * shdr = getSectHeader(i);
        ASSERT0(shdr);
        if (shdr->s_type == S_RELA &&
            (shdr->s_info == SYMINFO_sym(symbol_info).st_shndx)) {
            return true;
        }
    }
    return false;
}


void ELFMgr::collectFuncInfoForSymbol(
    ELFHdr & hdr, MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info && SYMINFO_sym(symbol_info).st_type == STT_FUNC);
    //'symbol_info' is STT_FUNC type. Thus it need to  record the function
    //info into 'symbol_info'
    ELFSHdr * shstrtab_shdr = getSectHeader(hdr.e_shstrndx);
    ELFSHdr * text_shdr = getSectHeader(SYMINFO_sym(symbol_info).st_shndx);
    ASSERT0(shstrtab_shdr && text_shdr);

    //Collect func code/size.
    FunctionInfo * fi = collectTextInfo(text_shdr, shstrtab_shdr, symbol_info);
    ASSERT0(fi);

    //Update symbol info.
    SYMINFO_func(symbol_info) = fi;
    m_func_info.append(fi);

    if (!existRelaInfo(hdr, symbol_info)) { return; }

    //If there is relocated info of current function,
    //update 'RELOCINFO_caller_func' info.
    for (UINT i = 0; i < m_reloc_info.get_elem_count(); i++) {
        RelocInfo * reloc_info = m_reloc_info[i];
        ASSERT0(reloc_info);
        if (RELOCINFO_shdr_idx(reloc_info) !=
            SYMINFO_sym(symbol_info).st_shndx) { continue; }
        RELOCINFO_caller_func(reloc_info) = fi;
        RELOCINFO_is_func(reloc_info) = true;
    }
}


Sym const* ELFMgr::getFunctionName(CHAR const* text_shdr_name)
{
    ASSERT0(text_shdr_name);

    StrBufVec str_vec;
    UINT num = xcom::xsplit(text_shdr_name, ".", str_vec);
    //e.g.: 1.given ".text" or ".text1".
    //      2.there are 2 substr after splited: ' ' + 'text'
    //      3.the corresponded index of substr:  0       1
    //      4.it will return '.text'.
    //
    //e.g.: 1.given ".text1.func_name".
    //      2.there are 3 substr after splited: ' ' + 'text1' + 'func_name'
    //      3.the corresponded index of substr:  0       1          2
    //      4.it will return 'func_name'.
    //
    //e.g.: 1.given ".text1.func_name.part.1".
    //      2.there are 5 substr after splited:
    //        ' ' + 'text1' + 'func_name' + 'part' + '1'
    //      3.the corresponed index of substr:
    //         0       1          2           3       4
    //      4.it will return 'func_name.part.1'.
    if (num <= 2) { return addToSymTab(text_shdr_name); }

    //Number 3 means the length of 2 '.' and '\0'.
    UINT len = (UINT)::strlen(str_vec.getStrBuf(1)->getBuf()) + 3;
    CHAR * exclude_str = (CHAR*)xmalloc(len);
    ASSERT0(exclude_str);
    //'exclude_str' is '.text.' or '.text1.'.
    ::sprintf(exclude_str, ".%s.%c", str_vec.getStrBuf(1)->getBuf(), '\0');
    return addToSymTab(getSubStr(text_shdr_name, exclude_str));
}


FunctionInfo * ELFMgr::collectTextInfo(ELFSHdr const* shdr,
    ELFSHdr const* strtab_shdr, SymbolInfo const* symbol_info)
{
    ASSERT0(shdr && strtab_shdr && symbol_info);

    //Collect section header name.
    CHAR const* shdr_name = getStrFromStrTab(strtab_shdr, shdr->s_name);
    ASSERT0(shdr_name);

    Addr func_size = SYMINFO_sym(symbol_info).st_size;
    //Allocate FunctionInfo.
    FunctionInfo * fi = m_func_mgr.allocFunctionInfo(func_size);
    ASSERT0(fi);

    if (func_size != 0) {
        //Set function code.
        Addr ofst = SYMINFO_sym(symbol_info).st_value;
        BYTE * code = FUNCINFO_code(fi).get_vec();
        ASSERT0(code);
        ::memcpy((void*)code, (void*)(shdr->s_content + ofst), func_size);
    }

    //Set FunctionInfo.
    FUNCINFO_code_ofst(fi) = 0;
    FUNCINFO_size(fi) = func_size;
    FUNCINFO_file_name(fi) =
        (m_file_name == nullptr) ? nullptr : addToSymTab(m_file_name);
    FUNCINFO_align(fi) = (UINT)shdr->s_addr_align;
    FUNCINFO_name(fi) = getFunctionName(shdr_name);
    FUNCINFO_is_entry(fi) = isEntryFunction(FUNCINFO_sect_type(fi));
    FUNCINFO_sect_type(fi) = getSectionTypeWithSplit(addToSymTab(shdr_name));
    FUNCINFO_sect_name(fi) = getSectionName(FUNCINFO_sect_type(fi));

    return fi;
}


void ELFMgr::constructSymbolUnull(OUT BYTEVec * bytevec,
                                  OUT BYTEVec * dynsym_bytevec)
{
    ASSERT0(bytevec && dynsym_bytevec);

    //The order of symbol in .symtab is sorted by it's 'st_bind' value.
    //'STB_LOCAL' is in the front of '.symtab'. Both 'local_bytevec'
    //and 'gloabl_bytevec' vectors are created to record 'STB_LOCAL'
    //and 'STB_GLOBAL' symbols. After all symbols are constructed, the
    //content of 'local_bytevec' and 'global_bytevec' will be copyed
    //to the section content'bytevec'.
    UINT sym_size = ELFSym::getSize(this);
    BYTEVec local_bytevec, global_bytevec;
    local_bytevec.grow((UINT)getSymbolNum() * sym_size);
    global_bytevec.grow((UINT)getSymbolNum() * sym_size);

    ELFSym sym;
    UINT local_idx = 0;
    UINT global_idx = 0;
    BYTEVec space(sym_size);
    ASSERT0(space.get_vec());

    for (UINT i = 0; i < m_symtab_info_vec.get_elem_count(); i++) {
        SymbolInfoVec * symbol_info_vec = m_symtab_info_vec.get(i);
        ASSERT0(symbol_info_vec);
        for (UINT j = 0; j < symbol_info_vec->get_elem_count(); j++) {
            SymbolInfo * symbol_info = symbol_info_vec->get(j);
            ASSERT0(symbol_info);
            ASSERT0(SYMINFO_sym(symbol_info).st_shndx != SHN_COMMON);

            space.clean();
            //Skip extern attribute symbol. Don't record it.
            if (SYMINFO_is_extern(symbol_info) ||
                (SYMINFO_sym(symbol_info).st_type == STT_SECTION)) { continue; }

            bool is_local = isSymbolWithLocalAttr(symbol_info);
            UINT sect_index = getSectionIndexViaSymbolInfo(symbol_info);

            //For ABS type.
            if (sect_index == SHN_UNDEF) {
                sect_index = SYMINFO_sym(symbol_info).st_shndx;
            }
            //Get symbol offset in corresponded section.
            Addr sect_offset = SYMINFO_is_func(symbol_info) ?
                FUNCINFO_code_ofst(SYMINFO_func(symbol_info)) :
                SYMINFO_ofst(symbol_info);

            //If the 'st_other' attribute of symbol info is 'STV_HIDDEN',
            //it 'st_bind' attribute needed to set to 'STB_LOCAL'.
            if (is_local) { SYMINFO_sym(symbol_info).st_bind = STB_LOCAL; }

            setSymbolValue(&sym, SYMINFO_sym(symbol_info).st_name,
                SYMINFO_sym(symbol_info).st_bind,
                SYMINFO_sym(symbol_info).st_type,
                SYMINFO_sym(symbol_info).st_other,
                sect_index, sect_offset,
                SYMINFO_sym(symbol_info).st_size);
            sym.insert(space.get_vec(), this);

            //Construct symbol content.
            setELFSymToByteVec(&space, symbol_info, &local_bytevec,
                &global_bytevec, dynsym_bytevec, local_idx, global_idx);

            //Update local/global index.
            local_idx += is_local ? sym_size : 0;
            global_idx += is_local ? 0 : sym_size;
        }
    }

    UINT local_sz = local_bytevec.get_elem_count();
    UINT global_sz = global_bytevec.get_elem_count();
    UINT total_sz = global_sz + local_sz + sym_size;

    //Record the begin index of global symbol of '.symtab' section.
    setGlobalSymbolBeginIndexOfSymtab((local_sz + sym_size) / sym_size);

    //Record the begin index of global symbol of '.dynsym' section.
    //Note:It assumes that all elements in '.dynsym' are with STT_GLOBAL
    //attribute. Thus the begin index of global symbol is begin
    //from 1(index 0 is UNDEF symbol).
    setGlobalSymbolBeginIndexOfDynSym(1);

    //Copy 'local_bytevec' and 'global_bytevec' content to 'bytevec'.
    if (bytevec->get_capacity() < total_sz) { bytevec->grow(total_sz); }

    bytevec->set((VecIdx)(sym_size + local_sz - 1), 0);
    ::memcpy((void*)(bytevec->get_vec() + sym_size),
        (void*)(local_bytevec.get_vec()), local_sz);

    bytevec->set((VecIdx)(total_sz - 1), 0);
    ::memcpy((void*)(bytevec->get_vec() + sym_size + local_sz),
        (void*)(global_bytevec.get_vec()), global_sz);
}


void ELFMgr::setELFSymToByteVec(MOD BYTEVec * sym_vec,
    MOD SymbolInfo * symbol_info, MOD BYTEVec * local_vec,
    MOD BYTEVec * global_vec, MOD BYTEVec * dynsym_vec,
    UINT local_idx, UINT global_idx)
{
    ASSERT0(symbol_info && sym_vec && local_vec && global_vec && dynsym_vec);

    UINT sym_size = ELFSym::getSize(this);

    //Record to dynsym bytevec if it is also '.dynsym' symbol.
    if (SYMINFO_is_dynsym(symbol_info)) {
        //NOTE:The value of sh_info in '.dynsym' shdr is pointed to the first
        //element with STT_GLOBAL attribute. Now there it assumes that all
        //elements in '.dynsym' section are with STT_GLOBAL attribute. Thus
        //it needs to be modified if there is element(or 'symbol_info') in
        //'.dynsym' shdr with STT_LOCAL attribute.
        ASSERT0(!isSymbolWithLocalAttr(symbol_info));
        UINT order_idx = (UINT)SYMINFO_dynsym_idx(symbol_info) * sym_size;
        BYTE value = dynsym_vec->get(order_idx + sym_size - 1);
        dynsym_vec->set(order_idx + sym_size - 1, value);
        ::memcpy((void*)(dynsym_vec->get_vec() + order_idx),
            (void*)(sym_vec->get_vec()), sym_size);
    }

    //Record to local bytevec.
    if (isSymbolWithLocalAttr(symbol_info)) {
        local_vec->set(local_idx + sym_size - 1, 0);
        ::memcpy((void*)(local_vec->get_vec() + local_idx),
            (void*)(sym_vec->get_vec()), sym_size);
        return;
    }

    //Record to global bytevec.
    global_vec->set(global_idx + sym_size - 1, 0);
    ::memcpy((void*)(global_vec->get_vec() + global_idx),
        (void*)(sym_vec->get_vec()), sym_size);
}


void ELFMgr::genSymTabContent(OUT BYTEVec * symtab_bytevec,
                              OUT BYTEVec * dynsym_bytevec)
{
    ASSERT0(symtab_bytevec && dynsym_bytevec);

    AssembleBinDescVec sym_desc_vec;
    UINT size = ELFSym::getSize(this);

    //Just allocate the first UNDEF symbol memory space of .symtab.
    symtab_bytevec->grow(size);
    dynsym_bytevec->grow(getDynSymItemNum() * size);

    constructSymbolNull(symtab_bytevec, dynsym_bytevec);
    constructSymbolUnull(symtab_bytevec, dynsym_bytevec);
}


void ELFMgr::setSymbolValue(MOD ELFSym * sym, Word name, xcom::UCHAR bind,
    xcom::UCHAR type, xcom::UCHAR other, Half shndx, Addr value, Addr size)
{
    ASSERT0(sym);

    sym->st_value = value;
    sym->st_size = size;
    sym->st_name = name;
    sym->st_bind = bind;
    sym->st_type = type;
    sym->st_other = other;
    sym->st_shndx = shndx;
}


void ELFMgr::constructELFSection()
{
    //Construct ELF section accoreding to the m_sect info.
    SectionInfo * sect_info;
    SectionInfoMapIter iter;
    for (m_sect_map.get_first(iter, &sect_info); !iter.end();
         m_sect_map.get_next(iter, &sect_info)) {
        ASSERT0(sect_info);
        ELFSHdr * shdr = getSectHeader(SECTINFO_index(sect_info));
        ASSERT0(shdr);

        if (SECTINFO_type(sect_info) == SH_TYPE_SHSTR) {
            setSectHeaderNameStrTab(shdr);
        }
        //Process s_type. e.g.: S_PROGBITS;
        shdr->s_type = SECTINFO_shdr_type(sect_info);
        //Process s_flags. Value from config table(g_section_desc).
        SET_FLAG(shdr->s_flags, SECTINFO_flag(sect_info));
        //Process s_addr.
        shdr->s_addr = getSectionAddr(SECTINFO_name(sect_info));
        //Process s_offset.
        shdr->s_offset = SECTINFO_ofst(sect_info);
        //Process s_size.
        shdr->s_size = getSectionSize(SECTINFO_name(sect_info));
        //Process s_content.
        if ((SECTINFO_type(sect_info) == SH_TYPE_SYMSTR) ||
            (SECTINFO_type(sect_info) == SH_TYPE_SHSTR)) {
            //SH_TYPE_SYMSTR/SH_TYPE_SHSTR is char type.
            CHARVec * charvec = getSectionCharVec(SECTINFO_name(sect_info));
            ASSERT0(charvec);
            shdr->s_content = (BYTE*)charvec->get_vec();
        } else {
            BYTEVec * bytevec = getSectionContent(SECTINFO_name(sect_info));
            ASSERT0(bytevec);
            shdr->s_content = (BYTE*)bytevec->get_vec();
        }
        //Process s_link/s_info and s_entry_size.
        switch (SECTINFO_type(sect_info)) {
        case SH_TYPE_SYMTAB:
            shdr->s_link = (Word32)getSectionIndex(SH_TYPE_SYMSTR);
            //The begin index of gloabl symbol in .symtab section.
            shdr->s_info = getGlobalSymbolBeginIndexOfSymtab();
            shdr->s_entry_size = ELFSym::getSize(this);
            break;
        case SH_TYPE_DYNSYM:
            shdr->s_link = (Word32)getSectionIndex(SH_TYPE_SYMSTR);

            //The begin index of gloabl symbol in .dynsym section.
            shdr->s_info = getGlobalSymbolBeginIndexOfDynSym();
            shdr->s_entry_size = ELFSym::getSize(this);
            break;
        case SH_TYPE_DYNAMIC:
            shdr->s_link = (Word32)getSectionIndex(SH_TYPE_SYMSTR);
            shdr->s_info = 0;
            shdr->s_entry_size = ELFDyn::getSize(this);
            break;
        case SH_TYPE_RELA_DYN:
            shdr->s_link = (Word32)getSectionIndex(SH_TYPE_DYNSYM);
            shdr->s_info = 0;
            shdr->s_entry_size = ELFSym::getSize(this);
            break;
        SWITCH_CASE_COMMON_SECT_CONSTRUCT:
            shdr->s_link = 0;
            shdr->s_info = 0;
            shdr->s_entry_size = SECTINFO_entry_size(sect_info);
            break;
        default:
            UNREACHABLE();
            break;
        }
        //Process s_addr_align.
        shdr->s_addr_align = (SECTINFO_align(sect_info) != ELF_VAL_UNDEF) ?
            SECTINFO_align(sect_info) :
            (getSectionAlign(SECTINFO_name(sect_info)));
        //Process s_name_str.
        shdr->s_name_str = SECTINFO_name(sect_info)->getStr();
    }
}


void ELFMgr::constructSymbolNull(OUT BYTEVec * symtab_bytevec,
                                 OUT BYTEVec * dynsym_bytevec)
{
    ASSERT0(symtab_bytevec && dynsym_bytevec);

    UINT const elf_sym_size = ELFSym::getSize(this);
    BYTEVec space(elf_sym_size);
    ASSERT0(space.get_vec());

    ELFSym sym;
    setSymbolValue(&sym, ELF_VAL_UNDEF, ELF_VAL_UNDEF, SYMBOL_NOTYPE,
        ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF);
    sym.insert(space.get_vec(), this);

    space.set((VecIdx)(elf_sym_size - 1), 0);
    symtab_bytevec->set((VecIdx)(elf_sym_size - 1), 0);
    dynsym_bytevec->set((VecIdx)(elf_sym_size - 1), 0);
    ::memcpy(symtab_bytevec->get_vec(), space.get_vec(), elf_sym_size);
    ::memcpy(dynsym_bytevec->get_vec(), space.get_vec(), elf_sym_size);
}


void ELFMgr::genSectHeaderNameStrTabContent(
    OUT CHARVec * charvec, OUT OffVec & offvec)
{
    StringList strlst;
    ELFHdr & hdr = getHdr();
    for (UINT i = 0; i < hdr.e_shnum; i++) {
        ELFSHdr * shdr = getSectHeader(i);
        ASSERT0(shdr);
        if (shdr->s_name_str == nullptr) { offvec.set(i, 0); continue; }
        strlst.append_tail(shdr->s_name_str);
    }
    if (strlst.get_elem_count() == 0) { return; }
    genStrTabContent(charvec, offvec, strlst);
}


void ELFMgr::genStrTabContent(OUT CHARVec * charvec,
    OUT OffVec & offvec, StringList const& strlst)
{
    //The first byte is '\0' in .strtab/.shstrtab/.dynstr and other section
    //that record string in ELF format. Thus the 'sz' begin from 1 and set
    //the first element to '\0' in 'charvec' manually.
    size_t sz = ELF_STR_ELEM_BEGIN_INDEX;
    //Estimitate content size.
    StringList::Iter it;
    for (CHAR const* s = strlst.get_head(&it);
         s != nullptr; s = strlst.get_next(&it)) { sz += ::strlen(s) + 1; }
    charvec->grow((UINT)sz);
    charvec->set(0, 0); //Set the first element to '\0'.
    size_t off = charvec->get_last_idx() + 1;
    UINT i = 0;
    for (CHAR const* s = strlst.get_head(&it);
         s != nullptr; s = strlst.get_next(&it), i++) {
        size_t l = ::strlen(s);
        offvec.set(i, (Off)off);
        charvec->set((UINT)(off + l - 1), 0);
        ::memcpy((void*)(charvec->get_vec() + off), (BYTE const*)s, l);
        //Write '\0' in the end of st_name.
        charvec->set((VecIdx)(off + l), 0);
        off += l + 1;
    }
}


void ELFMgr::countStrSizeAndSymbolNum()
{
    //The first byte is '\0' in .strtab/.shstrtab/.dynstr and other section
    //that record string in ELF format. Thus the 'sz' begin from 1 and set
    //the first element to '\0' in 'charvec' manually.
    size_t sz = ELF_STR_ELEM_BEGIN_INDEX;
    UINT64 symbol_num = 0;
    //Estimitate content size.
    for (UINT i = 0; i < m_symtab_info_vec.get_elem_count(); i++) {
        SymbolInfoVec * symbol_info_vec = m_symtab_info_vec.get(i);
        ASSERT0(symbol_info_vec);
        for (UINT j = 0; j < symbol_info_vec->get_elem_count(); j++) {
            SymbolInfo * si = symbol_info_vec->get(j);
            ASSERT0(si);
            sz += ::strlen(SYMINFO_name(si)->getStr()) + 1;
            symbol_num++;
        }
    }
    setSymbolNum(symbol_num);
    setStrSize((UINT)sz);
}


void ELFMgr::genStrTabContent(OUT CHARVec * charvec)
{
    countStrSizeAndSymbolNum();
    charvec->grow(getStrSize());
    //Set the first element to '\0'.
    charvec->set(0, 0);
    size_t off = charvec->get_last_idx() + 1;

    for (UINT i = 0; i < m_symtab_info_vec.get_elem_count(); i++) {
        SymbolInfoVec * symbol_info_vec = m_symtab_info_vec.get(i);
        ASSERT0(symbol_info_vec);
        for (UINT j = 0; j < symbol_info_vec->get_elem_count(); j++) {
            SymbolInfo * si = symbol_info_vec->get(j);
            ASSERT0(si);
            size_t l = ::strlen(SYMINFO_name(si)->getStr());
            charvec->set((VecIdx)(off + l - 1), 0);
            ::memcpy((void*)(charvec->get_vec() + off),
                (BYTE const*)(SYMINFO_name(si)->getStr()), l);
            SYMINFO_sym(si).st_name = off;
            //Write '\0' in the end of st_name.
            charvec->set((VecIdx)(off + l), 0);
            off += l + 1;
        }
    }
}


void ELFMgr::collectObjELF()
{
    //Collect ELF info that include symtabInfo and functionInfo.
    //These info are used to generate executed ELF.
    ELFHdr & hdr = getHdr();
    //Get '.shstrtab' section.
    ELFSHdr * shstrtab_shdr = getSectHeader(hdr.e_shstrndx);
    ASSERT0(shstrtab_shdr);

    //All ELF info had been read into memory by readObj();
    //Iter over section header to collect useful info.
    for (UINT i = 0; i < hdr.e_shnum; i++) {
        collectObjELFImpl(hdr, getSectHeader(i), i);
    }
}


void ELFMgr::collectObjELFImpl(ELFHdr & hdr,
    ELFSHdr const* shdr, UINT shdr_idx)
{
    ASSERT0(shdr);

    switch (shdr->s_type) {
    case S_UNDEF:
        ASSERT0(shdr_idx == 0);
        break;
    case S_SYMTAB:
        collectSymtabInfo(hdr, shdr);
        break;
    case S_RELA:
        collectRelocInfo(hdr, shdr);
        break;
    SWITCH_CASE_NONEED_COLLECT:
        ASSERT0(shdr_idx != 0);
        break;
    default:
        UNREACHABLE();
        break;
    }
}


bool ELFMgr::processSpecialShndx(ELFHdr & hdr, MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    Half shndx = SYMINFO_sym(symbol_info).st_shndx;
    UINT sect_num = hdr.e_shnum;

    //It is normal shndx.
    if (shndx > 0 && shndx < sect_num) { return false; }

    //It is special shndx.
    switch (shndx) {
    case SHN_UNDEF:
        //It is external symbol.
        SYMINFO_is_extern(symbol_info) = true;
        return true;
    case SHN_ABS:
        return true;
    case SHN_COMMON:
        //It is 'SHN_COMMON' symbol.
        SYMINFO_align(symbol_info) = SYMINFO_sym(symbol_info).st_align;
        SYMINFO_size(symbol_info) = SYMINFO_sym(symbol_info).st_size;
        return true;
    case SHN_LOPROC: //NOTE: There is same value with SHN_LORESERVE.
    case SHN_HIPROC:
    case SHN_HIRESERVE:
        UNREACHABLE();
        break;
    default:
        UNREACHABLE();
        break;
    }
    return false;
}


void ELFMgr::collectSymtabInfo(ELFHdr & hdr, ELFSHdr const* sym_shdr)
{
    ASSERT0(sym_shdr);

    for (UINT i = 0; i < sym_shdr->getElemNum(); i++) {
        SymbolInfo * symbol_info = m_sym_mgr.allocSymbolInfo();
        ASSERT0(symbol_info);

        SYMINFO_index(symbol_info) = i;
        SYMINFO_name(symbol_info) = addToSymTab(getStrFromSymTab(sym_shdr, i));
        readSymFromSymtabSect(sym_shdr, SYMINFO_sym(symbol_info), i);

        //Record the SymbolInfo to vector and map.
        setSymbolInfo(symbol_info);
        if (isNullSymbol(symbol_info, i)) { continue; }

        //Process some special st_shndx.
        if (processSpecialShndx(hdr, symbol_info)) { continue; }

        //Collect section name.
        collectSymbolInfoSectionName(hdr, symbol_info);

        //If the st_type of symbol is 'STT_FUNC', it need to collect
        //corresponded FunctionInfo.
        if (SYMINFO_sym(symbol_info).st_type == STT_FUNC) {
            collectFuncInfoForSymbol(hdr, symbol_info);
        }

        //There isn't RelocInfo to handle.
        if (!existRelaInfo(hdr, symbol_info)) { continue; }

        //If there is corresponded RelocInfo of SymbolInfo which means
        //that the SymbolInfo needed to be relocated. Thus corresponded
        //RelocInfo is needed to update.
        for (UINT j = 0; j < m_reloc_info.get_elem_count(); j++) {
            RelocInfo * reloc_info = m_reloc_info[j];
            ASSERT0(reloc_info);
            if (RELOCINFO_shdr_idx(reloc_info) !=
                SYMINFO_sym(symbol_info).st_shndx) { continue; }
            RELOCINFO_caller_sym(reloc_info) = symbol_info;
        }
    }
}


void ELFMgr::collectRelocInfo(ELFHdr & hdr, ELFSHdr const* rela_shdr)
{
    ASSERT0(rela_shdr);

    //Collect reloc info from 'rela_hdr' and store it into 'm_reloc_info'.
    ELFSHdr * symtab_shdr = getSectHeader(rela_shdr->s_link);
    ASSERT0(symtab_shdr);
    ELFSHdr * shstrtab_shdr = getSectHeader(hdr.e_shstrndx);
    ASSERT0(shstrtab_shdr);
    CHAR const* shdr_name = getStrFromStrTab(shstrtab_shdr, rela_shdr->s_name);
    ASSERT0(shdr_name);

    //Re-size vector capacity.
    UINT vec_sz = m_reloc_info.get_elem_count();
    UINT total_sz = (UINT)(vec_sz + rela_shdr->getElemNum());
    if (m_reloc_info.get_capacity() < total_sz) { m_reloc_info.grow(total_sz); }

    ELFRela rela_sym;
    for (UINT i = 0; i < rela_shdr->getElemNum(); i++) {
        RelocInfo * reloc_info = m_reloc_mgr.allocRelocInfo();
        ASSERT0(reloc_info);

        readRelaFromRelaTextSect(rela_shdr, rela_sym, i);
        //Update reloc info.
        RELOCINFO_name(reloc_info) =
            addToSymTab(getRelaName(symtab_shdr, rela_sym.r_sym));
        RELOCINFO_called_loc(reloc_info) = rela_sym.r_offset;
        RELOCINFO_type(reloc_info) = (UINT)rela_sym.r_type;
        RELOCINFO_addend(reloc_info) = (UINT)rela_sym.r_addend;
        RELOCINFO_sect_ofst(reloc_info) = 0;
        RELOCINFO_sym_idx(reloc_info) = rela_sym.r_sym;
        RELOCINFO_shdr_idx(reloc_info) = rela_shdr->s_info;
        RELOCINFO_caller_func(reloc_info) = nullptr;
        RELOCINFO_sect_name(reloc_info) =
            addToSymTab(getRelaShdrType(shdr_name));

        m_reloc_info.set(vec_sz + i, reloc_info);
    }
}


CHAR const* ELFMgr::processELFName(CHAR const* fn)
{
    ASSERT0(fn);

    //Use file name as the name of ELFMgr.
    //There may be same file name in different static library file. Thus
    //it will get it's parent directory as the part of the ELFMgr name.
    //e.g. file name         elf_mgr name
    //       xxx.c        -> xxx.c
    //    dir1/dir2/xxx.c -> dir2/xxx.c
    StrBufVec substr_vec;
    //e.g. 1.given dir1/dir2/xxx.c
    //     2.there are 4 substrs after splited.
    //             ' ' + 'dir1' + 'dir2' + 'xxx.c'
    //       index: 0      1        2        3
    UINT num = xcom::xsplit(fn, "/", substr_vec);
    if (num < 2) { return fn; }

    //If num >= 2 means that the file name contains directory name.
    //Get the last two substr. 2 represents the length of '.' and '\0'.
    CHAR * name_str = (CHAR*)xmalloc(
        ::strlen(substr_vec.getStrBuf(num - 2)->getBuf()) +
        ::strlen(substr_vec.getStrBuf(num - 1)->getBuf()) + 2);
    ASSERT0(name_str);
    ::sprintf(name_str, "%s/%s%c", substr_vec.getStrBuf(num - 2)->getBuf(),
        substr_vec.getStrBuf(num - 1)->getBuf(), '\0');
    return name_str;
}


void ELFMgr::mergeBssData(MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    //There isn't data in SymbolInfo with BSS attribute. When the SymbolInfo is
    //merged into section, memory space in ELF will be allocated and assigned 0.
    BYTEVec * content = getSectContentWithGenIfNotExist(
        SYMINFO_sect_name(symbol_info));
    ASSERT0(content);
    SectionInfo * sect_info = getSection(SYMINFO_sect_name(symbol_info));
    ASSERT0(sect_info);

    //Allocate memory for BSS section.
    UINT base_ofst = content->get_elem_count();
    base_ofst = (UINT)xcom::ceil_align(base_ofst, SYMINFO_align(symbol_info));
    UINT sz = (UINT)(base_ofst + SYMINFO_size(symbol_info));
    if (content->get_capacity() < sz) { content->grow(sz); }

    //Assign 0 for BSS section content.
    content->set(content->get_capacity() - 1, 0);
    ::memset(content->get_vec() + base_ofst, 0, SYMINFO_size(symbol_info));

    SYMINFO_ofst(symbol_info) = base_ofst;
    SECTINFO_align(sect_info) =
        MAX(SECTINFO_align(sect_info), SYMINFO_align(symbol_info));
}


void ELFMgr::mergeUnullData(MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    //Merged SymbolInfo data into corresponded section.
    AssembleBinDescVec bin_vec;
    //Record data to 'bin_vec'.
    setSymbolDataToSection(bin_vec, getSectionWithGenIfNotExist(
        SYMINFO_sect_type(symbol_info)), symbol_info);

    if (bin_vec.get_elem_count() == 0) { return; }

    //Write data to section content.
    BYTEVec * bytevec = getSectionContent(SYMINFO_sect_type(symbol_info));
    ASSERT0(bytevec);

    UINT sz = bin_vec.getTotalByteSize();
    UINT bytevec_sz = bytevec->get_elem_count();
    UINT total_sz = bytevec_sz + sz;
    if (bytevec->get_capacity() < total_sz) { bytevec->grow(total_sz); }
    BYTEVec tmp(sz);
    xcom::AssembleBinBuf as(&(tmp[0]), sz, bin_vec);
    bytevec->set(bytevec->get_capacity() - 1, 0);
    ::memcpy((void*)(bytevec->get_vec() + bytevec_sz),
             (void*)(tmp.get_vec()), sz);
}


bool ELFMgr::isNullSymbol(SymbolInfo const* symbol_info, UINT symbol_idx)
{
    ASSERT0(symbol_info);

    return ((symbol_idx == 0) &&
        (SYMINFO_sym(symbol_info).st_name == SHN_UNDEF) &&
        (SYMINFO_sym(symbol_info).st_value == SHN_UNDEF) &&
        (SYMINFO_sym(symbol_info).st_size == SHN_UNDEF) &&
        (SYMINFO_sym(symbol_info).st_type == STT_NOTYPE) &&
        (SYMINFO_sym(symbol_info).st_bind == STB_LOCAL) &&
        (SYMINFO_sym(symbol_info).st_other == STV_DEFAULT) &&
        (SYMINFO_sym(symbol_info).st_shndx == SHN_UNDEF));
}


CHAR const* ELFMgr::genSymbolNameWithIntSuffix(
    MOD SymbolInfo * symbol_info, UINT name_num)
{
    ASSERT0(symbol_info && (name_num > 0));

    Sym const* name = SYMINFO_name(symbol_info);
    ASSERT0(name);

    //Count the length of 'name_num'.
    //e.g.: 1.given name_num '1', and the length is 1.
    //      2.given name_num '22', and the length is 2.
    //      3.given name_num '333', and the length is 3.
    UINT num_width = 0;
    UINT num_tmp = name_num;
    while (num_tmp > 0) { num_tmp /= ELF_NUM_INT_10; num_width++; }

    //'4' represents the length of '.R_' and '\0'.
    CHAR * new_name = (CHAR*)xmalloc(::strlen(name->getStr()) + num_width + 4);
    ASSERT0(new_name);

    //Assemble new symbol name. The format is 'name.name_num'.
    //e.g.: "str.R_1', str.R_22', str.R_333'.
    ::sprintf(new_name, "%s.R_%d%c", name->getStr(), name_num, '\0');
    return new_name;
}


bool ELFMgr::findSymbolWithNoSectionType(SymbolInfo const* origin_symbol,
                                         OUT SymbolInfo ** target_symbol)
{
    ASSERT0(origin_symbol);

    //If the type of 'origin_symbol' is 'STT_SECTION', the function
    //would try to find another same name symbol which the type isn't
    //'STT_SECTION' in the same ELFMgr.
    for (UINT i = 0; i < m_symbol_info_vec.get_elem_count(); i++) {
        SymbolInfo * symbol_info = m_symbol_info_vec[i];
        ASSERT0(symbol_info);

        if (isNullSymbol(symbol_info, i)) { continue; }
        if (SYMINFO_sym(symbol_info).st_type != STT_SECTION &&
            SYMINFO_sym(symbol_info).st_type != STT_NOTYPE &&
            (SYMINFO_sym(origin_symbol).st_shndx ==
             SYMINFO_sym(symbol_info).st_shndx) &&
            (SYMINFO_sym(origin_symbol).st_value ==
             SYMINFO_sym(symbol_info).st_value)) {
            *target_symbol = symbol_info;
            return true;
        }
    }
    return false;
}


void ELFMgr::constructELFDebugAbbrevSection(MOD ELFSHdr * debug_abb_shdr,
                                            UINT debug_abb_align)
{
    ASSERT0(debug_abb_shdr);
    xoc::MCDwarfMgr * dm = getDwarfMgr();
    ASSERT0(dm);
    debug_abb_shdr->s_type = S_PROGBITS;
    debug_abb_shdr->s_addr = ELF_VAL_UNDEF;
    debug_abb_shdr->s_size = MCDWARFMGR_debug_abbrev_code(dm).
        get_elem_count();
    debug_abb_shdr->s_entry_size = ELF_VAL_UNDEF;
    debug_abb_shdr->s_addr_align = debug_abb_align;
    debug_abb_shdr->s_content = (BYTE*)MCDWARFMGR_debug_abbrev_code(dm).
        get_vec();
    debug_abb_shdr->s_name_str = DEBUG_ABBREV_SH_NAME;
}


//Construct .debug_str section.
void ELFMgr::constructELFDebugStrSection(MOD ELFSHdr * debug_str_shdr,
                                         UINT debug_str_align)
{
    ASSERT0(debug_str_shdr);
    xoc::MCDwarfMgr * dm = getDwarfMgr();
    ASSERT0(dm);
    debug_str_shdr->s_type = S_PROGBITS;
    debug_str_shdr->s_addr = ELF_VAL_UNDEF;
    debug_str_shdr->s_size = MCDWARFMGR_debug_str_code(dm).
        get_elem_count();
    debug_str_shdr->s_entry_size = ELF_VAL_UNDEF;
    debug_str_shdr->s_addr_align = debug_str_align;
    debug_str_shdr->s_content = (BYTE*)MCDWARFMGR_debug_str_code(dm).
        get_vec();
    debug_str_shdr->s_name_str = DEBUG_STR_SH_NAME;
    SET_FLAG(debug_str_shdr->s_flags, SF_MERGE|SF_STRINGS);
}


//Construct .debug and rela section.
void ELFMgr::constructELFDebugAndRelaSection(MOD ELFSHdr * symtab_shdr,
    MOD ELFSHdr * debug_shdr, UINT debug_info_align, OUT UINT & si,
    CHAR const* debug_section_name, CHAR const* debug_rela_section_name,
    Vector<MCFixup*> const& fxiups, Vector<BYTE> & debug_code)
{
    ASSERT0(symtab_shdr && debug_shdr);
    //Generate relocation entries.
    //Some specific entries will be fixed,
    //and then the final relocation for EFL will be generated.
    Sym const* caller = m_rm->addToSymbolTab(debug_section_name);
    ASSERT0(m_symbol_info.find(caller));
    SymbolInfo * sym_info = m_symbol_info.get(caller);
    ASSERT0(sym_info);
    genDebugReloc(sym_info, fxiups, debug_code);

    //Begin configuring the section header for debug_info.
    debug_shdr->s_type = S_PROGBITS;
    debug_shdr->s_addr = ELF_VAL_UNDEF;
    debug_shdr->s_size = debug_code.get_elem_count();
    debug_shdr->s_entry_size = ELF_VAL_UNDEF;
    debug_shdr->s_addr_align = debug_info_align;
    debug_shdr->s_content = (BYTE*)debug_code.get_vec();
    debug_shdr->s_name_str = debug_section_name;

    //start process of rela debug_info
    ELFSHdr * debug_rela_shdr = getSectHeader(si++);
    BYTEVec rel_content;
    genRelocContent(rel_content, (SYMINFO_reloc(sym_info)));
    BYTE * rel_space = (BYTE*)(xmalloc(rel_content.get_elem_count()));
    ::memcpy(rel_space, (BYTE*)(rel_content.get_vec()),
        rel_content.get_elem_count());
    debug_rela_shdr->s_type = S_RELA;
    debug_rela_shdr->s_addr = ELF_VAL_UNDEF;
    debug_rela_shdr->s_size = rel_content.get_elem_count();
    debug_rela_shdr->s_entry_size = ELFRela::getSize(this);
    debug_rela_shdr->s_link = (Word32)getSectHeaderIdx(symtab_shdr);
    debug_rela_shdr->s_info = (Word32)getSectHeaderIdx(debug_shdr);
    debug_rela_shdr->s_addr_align = is64bit() ?
        sizeof(Addr64) : sizeof(Addr32);
    debug_rela_shdr->s_content = rel_space;
    debug_rela_shdr->s_name_str = debug_rela_section_name;
    SET_FLAG(debug_rela_shdr->s_flags, SF_INFO_LINK);
}


UINT ELFMgr::getRelTypeFromDwarf(MCFixupKind kind)
{
    UINT type = 0;
    switch (kind) {
    case FK_DATA_4:
        type = get32BitReferRelocType();
        return type;
    case FK_DATA_8:
        type = get64BitReferRelocType();
        return type;
    default:
        UNREACHABLE();
        break;
    }
    return type;
}


void ELFMgr::addRelocForElfSymbol(MCFixup * fixup_entry,
                                  MCSymbol const* mc_symbol,
                                  SymbolInfo const* elf_sym_info,
                                  Sym const* callee)
{
    ASSERT0(callee && mc_symbol && elf_sym_info && fixup_entry);
    UINT type = getRelTypeFromDwarf(MCFIXUP_kind(fixup_entry));
    UINT offset = MCFIXUP_offset(fixup_entry);
    UINT addend = MCSYMBOL_region_offset(mc_symbol);
    addCallRelocation(SYMINFO_name(elf_sym_info),
                      callee, type, offset, addend);
}


void ELFMgr::genSymbolRefReloc(MCExpr const* value,
                               SymbolInfo const* elf_sym_info,
                               MCFixup * fixup_entry,
                               Vector<BYTE> & debug_code)
{
    ASSERT0(value && elf_sym_info && fixup_entry);
    MCSymbol const* mc_symbol = MCSYMBOLREFEXPR_mc_symbol(value);
    ASSERT0(mc_symbol);
    MCSymbol::SymbolType type = MCSYMBOL_type(mc_symbol);
    Sym const* callee = nullptr;

    if (type == MCSymbol::SECTION_LABEL ||
        type == MCSymbol::FUNC_LABEL) {
        ASSERT0(MCSYMBOL_region(mc_symbol));
        callee = MCSYMBOL_region(mc_symbol)->getRegionVar()->get_name();

        //Generate the corresponding relocation entries.
        addRelocForElfSymbol(fixup_entry, mc_symbol, elf_sym_info, callee);
        return;
    }

    if (type == MCSymbol::FUNC_VAR) {
        //Generate the corresponding relocation entries.
        //for global var for HBM and SPM
        if (MCSYMBOL_var(mc_symbol)->is_global()) {
            ASSERT0(MCSYMBOL_region(mc_symbol) == nullptr);
            callee = MCSYMBOL_var(mc_symbol)->get_name();
            ASSERT0(callee);
            addRelocForElfSymbol(fixup_entry, mc_symbol, elf_sym_info, callee);
            return;
        }

        //Go fill in the fixup values.
        if (!MCSYMBOL_is_registered(mc_symbol)) {
            xoc::note(m_rm, "notice: mc_symbol name:%s, not user\n",
                      MCSYMBOL_var(mc_symbol)->get_name()->getStr());
            return;
        }
        ASSERT0(MCSYMBOL_is_registered(mc_symbol));
        CHAR const* st_value = MCSYMBOL_value(mc_symbol);
        UINT debug_frame_offset = MCFIXUP_offset(fixup_entry);

        //st var of static position is must 8 bytes
        for (int i = 0; i < MCSYMBOL_VAR_STACK_OFF_SIZE; ++i) {
            debug_code.set(debug_frame_offset + i, st_value[i]);
        }
        return;
    }

    UNREACHABLE();
}


void ELFMgr::genDebugReloc(SymbolInfo const* elf_sym_info,
                           Vector<MCFixup*> const& fxiups,
                           Vector<BYTE> & debug_code)
{
    ASSERT0(elf_sym_info);
    xoc::MCDwarfMgr * dm = getDwarfMgr();
    ASSERT0(dm);
    UINT32 num_fix = fxiups.get_elem_count();
    for (UINT i = 0; i < num_fix; i++) {
        MCFixup * fixup_entry = fxiups[i];
        MCExpr const* value = MCFIXUP_value(fixup_entry) ;
        MCExpr::ExprKind kind = MCEXPR_kind(value);
        if (kind == MCExpr::SYMBOLREF) {
            genSymbolRefReloc(value, elf_sym_info, fixup_entry, debug_code);
            continue;
        }
        if (kind == MCExpr::BINARY) {
            //Go fill in the fixup values.
            INT64 fix_value = MCExpr::evaluateAsAbsolute(value);
            UINT debug_frame_offset = MCFIXUP_offset(fixup_entry) ;
            dm->overwriteBytesAtOffset(debug_code, fix_value,
                dm->getSizeForFixupKind(MCFIXUP_kind(fixup_entry)),
                debug_frame_offset);
            continue;
        }
        UNREACHABLE();
    }
}


EM_STATUS ELFMgr::readELF(UINT64 offset)
{
    //'offset' is the offset of target ELF in ELFAR file.
    setELFFileOffset(offset);
    readELFContent(true, false);
    //Reset offset to begin position.
    setELFFileOffset(0);
    //Reset m_file.
    resetFileObj();
    return EM_SUCC;
}


UINT ELFMgr::getDynamicSectionSize()
{
    UINT size = 0;
    SECTION_TYPE const* section_desc = getDynamicSectionDesc();
    ASSERT0(section_desc);
    UINT section_num = sizeof(section_desc) / sizeof(section_desc[0]);

    for (UINT i = 0; i < section_num; i++) {
        switch (section_desc[i]) {
        case SH_TYPE_RELA_DYN:
            //It needs to create three item of .dynamic section.
            //These items are: DT_RELA, DT_RELASZ and DT_RELAENT.
            size += ELFDyn::getSize(this) * ELF_RELA_ITEM_NUM_IN_ELFDYN;
            break;
        case SH_TYPE_SYMTAB:
            //It needs to create two item of .dynamic section.
            //These items are: DT_SYMTAB, DT_SYMENT.
            size += ELFDyn::getSize(this) * ELF_SYM_ITEM_NUM_IN_ELFDYN;
            break;
        case SH_TYPE_SYMSTR:
            //It needs to create two item of .dynamic section.
            //These items are: DT_STRTAB, DT_STRSZ.
            size += ELFDyn::getSize(this) * ELF_STR_ITEM_NUM_IN_ELFDYN;
            break;
        default:
            UNREACHABLE();
            break;
        }
    }
    return size;
}


void ELFMgr::preProcessDynamicSection()
{
    SectionInfo * si = getSection(SH_TYPE_DYNSYM);
    ASSERT0(si);

    SECTINFO_size(si) = getDynamicSectionSize();
    BYTEVec * content = getSectionContent(SH_TYPE_DYNAMIC);
    ASSERT0(content);

    if (content->get_elem_count() < SECTINFO_size(si)) {
        content->grow((UINT)SECTINFO_size(si));
    }

    content->set((VecIdx)SECTINFO_size(si) - 1, 0);
    ::memset((void*)(content->get_vec()), 0, SECTINFO_size(si));
}


void ELFMgr::setEntryPointInELFHdr(SECTION_TYPE sect_type)
{
    ELFHdr & hdr = getHdr();

    //In the case of an empty file, no text segment will be created,
    //cannot directly obtain the address of this segment.
    if (!hasSection(sect_type)) {
        hdr.e_entry = 0;
        return;
    }
    hdr.e_entry = getSectionAddr(sect_type);
}


SectionInfo * GenMappedOfSectInfoMap::createMapped(Sym const* s)
{
    ASSERT0(s && m_sect_mgr && m_elf_mgr);
    return m_sect_mgr->allocSectionInfo(m_elf_mgr->getSectionType(s));
}


//
// =========================== ARFILE Start ========================
//
static UINT64 getByteBigEndian(UCHAR const* field, UINT size)
{
    ASSERT0(field && (size > 0) && (size <= ARHDR_INDEX_ELEM_SIZE_8B));
    //e.g.:
    // given original value:  0x8877665544332211
    //     the Byte index is:   7 6 5 4 3 2 1 0
    // return target value:   0x1122334455667788
    //     the Byte index is:   7 6 5 4 3 2 1 0
    UINT64 res = 0;
    for (UINT i = 0; i < size; i++) {
        res |= LEFT_SHIFT_64BITS_VALUE(
            (field[size - i - 1]), (i * BITS_PER_BYTE));
    }
    return res;
}


EM_STATUS ELFAR::open(CHAR const* filename, MOD ELFARMgr * elfar_mgr)
{
    ASSERT0(filename && elfar_mgr && m_file == nullptr);

    m_file = elfar_mgr->allocFileObj(filename, false);
    ASSERT0(m_file);

    if (m_file->getFileHandler() == nullptr) {
        delete m_file;
        m_file = nullptr;
        return EM_OPEN_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFAR::read(OUT BYTE * buf, size_t offset, size_t size)
{
    ASSERT0(buf && m_file);

    size_t actual_rd = 0;
    if (m_file->read(buf, offset, size, &actual_rd) != xcom::FO_SUCC ||
        actual_rd != size) {
        return EM_RD_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFAR::readARIdent()
{
    BYTE * buf = (BYTE*)ALLOCA(sizeof(ARIdent));
    ASSERT0(buf);

    //Read ARIdent info to 'buf'.
    if (EM_SUCC != read(buf, 0, sizeof(ARIdent))) { return EM_RD_ERR; }
    //Update ARFile position.
    m_file_pos += sizeof(ARIdent);

    ARIdent ar_ident;
    ar_ident.extract(buf);

    //Check ARIndent info.
    if (!ar_ident.isARFile()) { return EM_NOT_AR_FILE; }
    return EM_SUCC;
}


EM_STATUS ELFAR::readARHeader()
{
    BYTE * buf = (BYTE*)ALLOCA(sizeof(ARHdr));
    ASSERT0(buf);

    //Read ARHdr info to 'buf'.
    if (EM_SUCC != read(buf, m_file_pos, sizeof(ARHdr))) { return EM_RD_ERR; }
    //Update ARFile position.
    m_file_pos += sizeof(ARHdr);
    //Extract ARHdr info to 'm_ar_hdr'.
    m_ar_hdr.extract(buf);
    return EM_SUCC;
}


EM_STATUS ELFAR::readSymbolIndex()
{
    // Read symbol info from ARFile.
    // format:
    // ----------------
    //      ARHdr
    // ----------------
    //   m_index_num
    //                  Dedicated the number of symbol. It's size(4B or 8B)
    //                  decided by the size of ARHDR_ar_name(ARHDR).
    // ----------------
    //   m_index_array
    //                  Record the index of symbol in ARFile.
    //                  It's size = m_index_num * sizeof(m_index_num).
    // ----------------
    //   m_sym_tab
    //                  Record all symbol.
    // ----------------
    UINT index_elem_size = 0;
    //Set 'index_elem_size' according to the 'ar_name'.
    UINT ar_name_sz = sizeof(ARHDR_ar_name(&m_ar_hdr));
    if (!::strncmp(ARHDR_ar_name(&m_ar_hdr), ARHDR_SYMBOL_START, ar_name_sz)) {
        index_elem_size = ARHDR_INDEX_ELEM_SIZE_4B;
    } else if (!::strncmp(ARHDR_ar_name(&m_ar_hdr),
               ARHDR_SYMBOL_START_64, ar_name_sz)) {
        index_elem_size = ARHDR_INDEX_ELEM_SIZE_8B;
    } else { UNREACHABLE(); }

    //Read 'm_index_num'.
    BYTE * buf = (BYTE*)ALLOCA(index_elem_size);
    ASSERT0(buf);

    if (EM_SUCC != read(buf, m_file_pos, index_elem_size)) { return EM_RD_ERR; }
    m_file_pos += index_elem_size;
    m_index_num = getByteBigEndian(buf, index_elem_size);

    //Read 'index_array' content.
    UINT64 index_array_size = m_index_num * index_elem_size;
    BYTE * index_buf = (BYTE*)ALLOCA(index_array_size);
    ASSERT0(index_buf);

    if (EM_SUCC != read(index_buf, m_file_pos, index_array_size)) {
        return EM_RD_ERR;
    }
    m_file_pos += (index_array_size);

    //Process 'index_array' element.
    m_index_array = (UINT64*)::malloc(m_index_num * sizeof(m_index_array));
    ASSERT0(m_index_array);

    for (UINT i = 0; i < m_index_num; i++) {
        m_index_array[i] = getByteBigEndian(
            (index_buf + i * index_elem_size), index_elem_size);
    }

    //Get 'ar_size'.
    UINT hdr_size = strtoul(ARHDR_ar_size(&m_ar_hdr), nullptr,
        sizeof(ARHDR_ar_size(&m_ar_hdr)));

    //Calculate 'sym_tab_size'.
    m_sym_tab_size = (hdr_size + ARHDR_add_size(hdr_size)) -
        index_elem_size - index_array_size;

    //Read 'm_sym_tab' content.
    m_sym_tab = (CHAR*)::malloc(m_sym_tab_size);
    ASSERT0(m_sym_tab);

    if (EM_SUCC != read((BYTE*)m_sym_tab, m_file_pos, m_sym_tab_size)) {
        return EM_RD_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFAR::getArchiveELF(MOD ELFMgr * elf_mgr, UINT64 offset) const
{
    ASSERT0(elf_mgr);

    //Skip 'ARHdr' content ahead of ELF content.
    offset += sizeof(ARHdr);

    //Read ELF info from 'm_file'.
    //'m_file' is fd(file description) of ARFile to wihch ELF belong.
    elf_mgr->initFileObj(m_file);
    return elf_mgr->readELF(offset);
}


EM_STATUS ELFAR::readARFile(MOD ELFARMgr * elfar_mgr)
{
    ASSERT0(elfar_mgr && m_file_name);

    EM_STATUS st = open(m_file_name, elfar_mgr);

    if ((st = readARIdent()) != EM_SUCC) { return st; }

    if ((st = readARHeader()) != EM_SUCC) { return st; }

    if ((st = readSymbolIndex()) != EM_SUCC) { return st; }
    return st;
}

//
// =========================== ELFARMgr Start ========================
//
ELFARMgr::~ELFARMgr()
{
    for (ELFAR * ar = m_ar_list.get_head();
         ar != nullptr; ar = m_ar_list.get_next()) {
        if (ar != nullptr) { delete ar; }
    }

    for (ELFARInfo * ar_info = m_ar_info_meta_list.get_head();
         ar_info != nullptr; ar_info = m_ar_info_meta_list.get_next()) {
        if (ar_info != nullptr) { delete ar_info; }
    }

    for (FileObj * fo = m_file_obj_list.get_head();
         fo != nullptr; fo = m_file_obj_list.get_next()) {
        if (fo != nullptr) { delete fo; }
    }

    for (ELFARInfoVec * vec = m_elfar_info_vec_list.get_head();
         vec != nullptr; vec = m_elfar_info_vec_list.get_next()) {
        if (vec != nullptr) { delete vec; }
    }
}


ELFAR * ELFARMgr::allocELFAR(CHAR const* file_name)
{
    ASSERT0(file_name);
    ELFAR * ar = new ELFAR(file_name);
    ASSERT0(ar);
    m_ar_list.append_tail(ar);
    return ar;
}


ELFARInfo * ELFARMgr::allocELFARInfo()
{
    ELFARInfo * ar_info = new ELFARInfo();
    ASSERT0(ar_info);
    m_ar_info_meta_list.append_tail(ar_info);
    return ar_info;
}


FileObj * ELFARMgr::allocFileObj(CHAR const* filename, bool is_del)
{
    ASSERT0(filename);
    FileObj * fo = new FileObj(filename, is_del);
    ASSERT0(fo);
    m_file_obj_list.append_tail(fo);
    return fo;
}


ELFARInfoVec * ELFARMgr::allocVectorOfELFARInfo()
{
    ELFARInfoVec * vec = new ELFARInfoVec();
    ASSERT0(vec);
    m_elfar_info_vec_list.append_tail(vec);
    return vec;
}


void ELFARMgr::genVectorELFARInfo(ELFAR const* ar, MOD ELFARInfo * ar_info)
{
    ASSERT0(ar && ar_info);
    ELFARInfoVec * vec = allocVectorOfELFARInfo();
    ASSERT0(vec);
    vec->append(ar_info);
    m_ar_info_map.set(ar, vec);
}


void ELFARMgr::initARFileStack()
{
    //AR files are sorted in the order in which they were read from outside.
    //Thus a stack is used to record the ARFile name/path. The AR file is
    //read in first recorded in the top of the stack.
    m_ar_file_stack.clean();
    for (CHAR const* file_path = m_ar_file_list->get_tail();
         file_path != nullptr; file_path = m_ar_file_list->get_prev()) {
        m_ar_file_stack.push(file_path);
    }
}


ELFAR * ELFARMgr::processARFile()
{
    CHAR const* filename = getARFileName();
    if (filename == nullptr) { return nullptr; }

    ELFAR * ar = allocELFAR(filename);
    ASSERT0(ar);

    ar->readARFile(this);
    return ar;
}


bool ELFARMgr::findELFMgr(ELFAR const* ar, UINT64 idx,
                          OUT ELFMgr ** elf_mgr) const
{
    ASSERT0(ar && elf_mgr);

    if (!m_ar_info_map.find(ar)) { return false; }

    ELFARInfoVec * vec = m_ar_info_map.get(ar);
    ASSERT0(vec);

    for (UINT i = 0; i < vec->get_elem_count(); i++) {
        ELFARInfo * ar_info = vec->get(i);
        ASSERT0(ar_info);

        if (ELFARINFO_elf_mgr_idx(ar_info) != idx) { continue; }
        *elf_mgr = ELFARINFO_elf_mgr(ar_info);
        return true;
    }
    return false;
}


void ELFARMgr::saveARInfo(MOD ELFAR * ar, MOD ELFMgr * elf_mgr, UINT64 idx)
{
    ASSERT0(ar && elf_mgr);

    ELFARInfo * ar_info = allocELFARInfo();
    ASSERT0(ar_info);

    ELFARINFO_ar(ar_info) = ar;
    ELFARINFO_elf_mgr(ar_info) = elf_mgr;
    ELFARINFO_elf_mgr_idx(ar_info) = idx;

    //Current ar has been stored into 'm_ar_info_map'. Just append
    //'ar_info' into the xcom::Vector<ELFARInfo*> of 'm_ar_info_map'.
    if (m_ar_info_map.find(ar)) {
        ELFARInfoVec * vec = m_ar_info_map.get(ar);
        ASSERT0(vec);
        vec->append(ar_info);
        return;
    }

    //Create a new xcom::Vector<ELFARInfo*> to record 'ar_info'
    //and store it into 'm_ar_info_map'.
    genVectorELFARInfo(ar, ar_info);
}


void ELFARMgr::saveSymbolARInfo(
    MOD ELFAR * ar, Sym const* symbol_name, UINT64 idx)
{
    ASSERT0(ar && symbol_name);

    if (m_symbol_ar_info_map.find(symbol_name)) { return; }

    ELFARInfo * ar_info = allocELFARInfo();
    ASSERT0(ar_info);

    ELFARINFO_ar(ar_info) = ar;
    ELFARINFO_elf_mgr(ar_info) = nullptr;
    ELFARINFO_elf_mgr_idx(ar_info) = idx;

    m_symbol_ar_info_map.set(symbol_name, ar_info);
}


//
// =========================== LinkerMgr Start ========================
//
LinkerMgr::LinkerMgr()
{
    m_output_file_name = nullptr;
    m_dump = nullptr;
    m_output_elf_mgr = nullptr;

    if (g_elf_opt.isDumpLink()) {
        initDumpFile(LINKERMGR_DUMP_LOG_FILE_NAME);
    }
}


LinkerMgr::~LinkerMgr()
{
    if (m_output_elf_mgr != nullptr) {
        delete m_output_elf_mgr;
        m_output_elf_mgr = nullptr;
    }

    for (ELFMgr * elf_mgr = m_elf_mgr_meta_list.get_head();
         elf_mgr != nullptr; elf_mgr = m_elf_mgr_meta_list.get_next()) {
        if (elf_mgr != nullptr) { delete elf_mgr; }
    }

    //Free m_dump.
    if (g_elf_opt.isDumpLink()) { closeDump(); }
}


ELFMgr * LinkerMgr::allocELFMgr()
{
    ELFMgr * elf_mgr = genELFMgr();
    ASSERT0(elf_mgr);

    //For link and generated ELF.
    m_elf_mgr_list.append_tail(elf_mgr);

    //For managed elf_mgr object resources.
    m_elf_mgr_meta_list.append_tail(elf_mgr);
    return elf_mgr;
}


EM_STATUS LinkerMgr::initDumpFile(CHAR const* filename)
{
    ASSERT0(filename && m_dump == nullptr);

    m_dump = new FileObj(filename, false);
    ASSERT0(m_dump);

    if (m_dump->getFileHandler() == nullptr) {
        delete m_dump;
        m_dump = nullptr;
        return EM_OPEN_ERR;
    }
    //Initialize the indent of log file.
    m_logmgr.setIndent(2);
    return EM_SUCC;
}


void LinkerMgr::closeDump()
{
    if (m_dump != nullptr) { delete m_dump; m_dump = nullptr; }
}


void LinkerMgr::allocUnresolvedRelocSymbol(
    RelocInfo const* reloc_info, UINT idx)
{
    ASSERT0(reloc_info);

    UnresolvedRelocIdxVec * vec = m_infomgr.allocUnresolvedRelocIdxVec();
    ASSERT0(vec);

    vec->append(idx);
    m_unresolved_reloc_idx_map.set(RELOCINFO_name(reloc_info), vec);
}


void LinkerMgr::processLibPathList(xcom::List<CHAR const*> * lib_path_list)
{
    ASSERT0(lib_path_list);

    //Record 'lib_path_list' into 'm_armgr'.
    m_armgr.setLibFileList(lib_path_list);
    m_armgr.initARFileStack();

    if (g_elf_opt.isDumpLink()) { dumpLibPathList(lib_path_list); }
}


void LinkerMgr::setOutputName(CHAR const* output_name)
{
    //'output_name' comes from the command option '-o'.
    //Defaut value is: 'mi.test.elf'.
    m_output_file_name = (output_name == nullptr) ?
        LINKERMGR_OUTPUT_FILE_NAME : output_name;
}


bool LinkerMgr::hasSameSymbol(ELFMgr const* target_elf_mgr,
    SymbolInfo const* target_symbol_info, bool is_special_elf_mgr)
{
    ASSERT0(target_symbol_info);

    //If 'target_symbol_info' is with extern attribute,
    //it doesn't need to be checked same name.
    if (SYMINFO_is_extern(target_symbol_info)) { return false; }
    Sym const* symbol_name = SYMINFO_name(target_symbol_info);
    ASSERT0(symbol_name);

    for (ELFMgr * elf_mgr = m_elf_mgr_list.get_head();
         elf_mgr != nullptr; elf_mgr = m_elf_mgr_list.get_next()) {
        if (target_elf_mgr == elf_mgr) { continue; }
        if (!ELFMGR_symbol_map(elf_mgr).find(symbol_name)) { continue; }
        //There is SymbolInfo with name of 'symbol_name' in 'elf_mgr'.
        SymbolInfo * symbol_info = ELFMGR_symbol_map(elf_mgr).get(symbol_name);
        ASSERT0(symbol_info);

        //The SymbolInfo is with extern attribute,
        //it doesn't need to be checked same name.
        if (SYMINFO_is_extern(symbol_info)) { continue; }

        //Symbol with STB_GLOBAL/STB_WEAK attribute doesn't need to be renamed.
        if (!is_special_elf_mgr &&
            (SYMINFO_sym(symbol_info).st_bind == STB_GLOBAL ||
             SYMINFO_sym(symbol_info).st_bind == STB_WEAK)) {
            continue;
        }

        ASSERT0(GET_SYM_OTHER_VALUE(
                SYMINFO_sym(symbol_info).st_bind) == STV_HIDDEN ||
                GET_SYM_OTHER_VALUE(
                SYMINFO_sym(symbol_info).st_bind) == STV_DEFAULT ||
                GET_SYM_OTHER_VALUE(
                SYMINFO_sym(symbol_info).st_bind) == STV_INTERNAL);

        //FIXME: It can check redefine symbol and print error msg there.
        return true;
    }
    return false;
}


UINT LinkerMgr::getSameNameNum(SymbolInfo const* symbol_info)
{
    ASSERT0(symbol_info);

    bool find = false;
    return m_same_name_num_map.getAndGen(SYMINFO_name(symbol_info), &find);
}


void LinkerMgr::renameSymbolName(MOD ELFMgr * elf_mgr,
    SymbolInfo const* target_symbol_info, CHAR const* name)
{
    ASSERT0(elf_mgr && target_symbol_info && name);

    //Rename SymbolInfo and RelocInfo in 'elf_mgr'.
    Sym const* origin_sym = SYMINFO_name(target_symbol_info);
    ASSERT0(origin_sym);

    //Rename SymbolInfo.
    for (UINT i = 0; i < ELFMGR_symbol_vec(elf_mgr).get_elem_count(); i++) {
        SymbolInfo * symbol_info = ELFMGR_symbol_vec(elf_mgr)[i];
        ASSERT0(symbol_info);

        if (elf_mgr->isNullSymbol(symbol_info, i) ||
            (SYMINFO_name(symbol_info) != origin_sym)) { continue; }

        ASSERT0(ELFMGR_symbol_map(elf_mgr).find(origin_sym));
        ELFMGR_symbol_map(elf_mgr).remove(origin_sym);

        SYMINFO_name(symbol_info) = elf_mgr->addToSymTab(name);
        ELFMGR_symbol_map(elf_mgr).set(SYMINFO_name(symbol_info), symbol_info);
    }

    //Rename RelocInfo.
    for (UINT i = 0; i < ELFMGR_reloc_vec(elf_mgr).get_elem_count(); i++) {
        RelocInfo * reloc_info = ELFMGR_reloc_vec(elf_mgr)[i];
        ASSERT0(reloc_info);

        if (RELOCINFO_name(reloc_info) != origin_sym) { continue; }
        RELOCINFO_name(reloc_info) = elf_mgr->addToSymTab(name);
    }
}


void LinkerMgr::handleSameNameInDiffFile(MOD ELFMgr * elf_mgr,
                                         bool is_special_elf_mgr)
{
    ASSERT0(elf_mgr);

    //Process SymbolInfo with the same name in different ELFMgr. It will keep
    //track of how many times it's the same name. And the number will be added
    //to the original SymbolInfo name as a suffix. Thus the new name foramt is
    //"original_name + number suffix".
    //e.g.: given three same name 'str'.
    //      the new name is 'str', 'str.R_1', and 'str.R_2'.
    for (UINT i = 0; i < ELFMGR_symbol_vec(elf_mgr).get_elem_count(); i++) {
        SymbolInfo * symbol_info = ELFMGR_symbol_vec(elf_mgr)[i];
        ASSERT0(symbol_info);

        if (elf_mgr->isNullSymbol(symbol_info, i)) { continue; }
        if (!hasSameSymbol(elf_mgr, symbol_info, is_special_elf_mgr)) {
            continue;
        }

        //Get how many SymbolInfo with the same name as 'symbol_info'.
        UINT same_name_num = ELF_INCREASE_VALUE(getSameNameNum(symbol_info));
        //Update the number.
        setSameNameNum(symbol_info, same_name_num);
        //Add the number to original SymbolInfo name
        //as suffix to generate a new name.
        CHAR const* symbol_new_name =
            elf_mgr->genSymbolNameWithIntSuffix(symbol_info, same_name_num);
        ASSERT0(symbol_new_name);

        if (g_elf_opt.isDumpLink()) {
            xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
                "Change name: file %s, from %s to %s\n",
                elf_mgr->m_file_name, SYMINFO_name(symbol_info)->getStr(),
                symbol_new_name);
        }
        //Write new name.
        renameSymbolName(elf_mgr, symbol_info, symbol_new_name);
    }
}


void LinkerMgr::updateELFInfo(MOD ELFMgr * elf_mgr)
{
    ASSERT0(elf_mgr);

    //Record SymbolInfo vec of 'elf_mgr' to 'm_symtab_vec'.
    ELFMGR_symtab_vec(m_output_elf_mgr).append(&(ELFMGR_symbol_vec(elf_mgr)));

    //Handle alias symbol.
    for (UINT i = 0; i < ELFMGR_reloc_vec(elf_mgr).get_elem_count(); i++) {
        RelocInfo * reloc_info = ELFMGR_reloc_vec(elf_mgr)[i];
        ASSERT0(reloc_info);

        bool res = false;
        Sym const* aliasee = ELFMGR_alias_map(m_output_elf_mgr).get(
            RELOCINFO_name(reloc_info), &res);
        if (!res) { continue; }

        //The name of RelocInfo is alias symbol, it needs to be replaced
        //with aliasee symbol according to the 'm_alias_symbol_map' info.
        ASSERT0(aliasee);
        RELOCINFO_name(reloc_info) = aliasee;
    }

    //Process SymbolInfo with the same name in different ELFMgrs.
    //For SymbolInfo with same name, it needs to be generated a new name.
    if (m_elf_mgr_list.get_elem_count() != 0) {
        handleSameNameInDiffFile(elf_mgr, true);
    }

    //Resize 'm_reloc_symbol_vec' capacity.
    UINT vec_sz = m_reloc_symbol_vec.get_elem_count();
    UINT size = ELFMGR_reloc_vec(elf_mgr).get_elem_count();
    UINT total_sz = vec_sz + size;
    if (m_reloc_symbol_vec.get_capacity() < total_sz) {
        m_reloc_symbol_vec.grow(total_sz);
    }
    //Update RelocInfo in 'elf_mgr' to 'm_reloc_symbol_vec'.
    for (UINT i = 0; i < size; i++) {
        RelocInfo * reloc_info = ELFMGR_reloc_vec(elf_mgr)[i];
        ASSERT0(reloc_info);
        m_reloc_symbol_vec.set(vec_sz + i, reloc_info);
    }
    m_elf_mgr_list.append_tail(elf_mgr);
}


void LinkerMgr::resolveRelocInfoViaVar()
{
    //Found target SymbolInfo from ELFMgr that collected from xoc::Var. These
    //ELFMgr mainly come from compile files that directly provided by user.
    //These compile files have the highest priority during the processing of
    //found target SymbolInfo. The function will iterate over all SymbolInfo
    //in 'symbol_info_vec' in all xoc::Var ELFMgr, and check whether the name
    //of SymbolInfo existed in 'm_unresolved_reloc_idx_map'. RelocInfo will be
    //resolved if target SymbolInfo has been found.
    for (ELFMgr * elf_mgr = m_elf_mgr_list.get_head();
         elf_mgr != nullptr; elf_mgr = m_elf_mgr_list.get_next()) {
        SymbolInfoVec & symbol_info_vec = ELFMGR_symbol_vec(elf_mgr);

        for (UINT j = 0; j < symbol_info_vec.get_elem_count(); j++) {
            SymbolInfo * symbol_info = symbol_info_vec[j];
            ASSERT0(symbol_info);

            if (elf_mgr->isNullSymbol(symbol_info, j) ||
                SYMINFO_is_extern(symbol_info) ||
                !m_unresolved_reloc_idx_map.find(SYMINFO_name(symbol_info))) {
                continue;
            }
            //Resolve RelocInfo with SymbolInfo that comes from xoc::Var.
            resolveRelocInfo(SYMINFO_name(symbol_info), symbol_info);
        }
    }

    if (g_elf_opt.isDumpLink()) {
        xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
            "\n\n======== Resolved Internal Done ======== \n\n");
    }
}


void LinkerMgr::doResolve()
{
    initUnResolveSymbol();

    if (g_elf_opt.isDumpLink()) {
        xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
            "\n\n======== Do Resolved ========\n\n");
    }

    //Resolve RelocInfo with SymbolInfo that comes from xoc::Var.
    resolveRelocInfoViaVar();

    //Resolve RelocInfo with SymbolInfo that comes from library file.
    resolveRelocInfoViaARFile();

    if (!hasUnResolvedRelocSymbol(true)) { return; }
    //There is unresolved RelocInfo.
    if (g_elf_opt.isDumpLink()) { dumpLinkResolve(); }
    ASSERTN(0, ("There is unresolved RelocInfo."));
}


ELFMgr * LinkerMgr::getELFMgrWhichSymbolBelongTo(ELFAR * ar, UINT64 index)
{
    ASSERT0(ar);

    ELFMgr * elf_mgr = nullptr;
    //'elf_mgr' has been recorded.
    if (m_armgr.findELFMgr(ar, index, &elf_mgr)) {
        ASSERT0(elf_mgr);
        return elf_mgr;
    }

    //Get 'elf_mgr' from 'ar' via 'index'.
    elf_mgr = getELFMgrFromARViaIdx(ar, index);
    ASSERT0(elf_mgr);

    //Addressing the problem that SymbolInfo with same name in different ELFMgr.
    if (m_elf_mgr_list.get_elem_count() != 0) {
        handleSameNameInDiffFile(elf_mgr, false);
    }

    //Save 'elf_mgr' info.
    m_armgr.saveARInfo(ar, elf_mgr, index);
    ELFMGR_symtab_vec(m_output_elf_mgr).append(&(ELFMGR_symbol_vec(elf_mgr)));

    //Process RelocInfo of 'elf_mgr'.
    processRelocInfo(ar, elf_mgr, index);
    return elf_mgr;
}


void LinkerMgr::resolveRelocInfoViaARFile()
{
    //The function is used to find target SymbolInfo from ELFMgr that collected
    //from AR file. AR file is packaged with numerous ELF. If target symbol has
    //been found, a new ELFMgr will be created according to the ELF info to
    //which this target symbol belong.
    //1.Enter 'while' statement until all unresolved RelocInfo are resovlved or
    //  all AR file have been found. In 'while' statement, AR file is popped
    //  from 'm_ar_file_stack' and a 'for' statement is used to iterate over
    //  global symtab in this AR file.
    //2.'symbol' getting from global symtab will be used to find unresolved
    //  RelocInfo from 'm_unresolved_reloc_idx_map'.
    //3.Skipped if RelocInfo with the same name as 'symbol' has been resolved.
    //  Thus if there are more than one target 'symbol' with the same name as
    //  RelocInfo, only the first target 'symbol' will be used to resolve.
    //4.If there isn't unresolved RelocInfo with the same name as 'symbol' in
    //  'm_unresolved_reloc_idx_map', the 'symbol' still needs to be recorded.
    //  Since RelocInfo in 'm_reloc_sym_vec' is constantly updated according to
    //  the situation, there may be RelocInfo updated later needs this 'symbol'
    //  to resolve. Thus ELFARInfo is used to record the relationship between
    //  the name and index of 'symbol' and AR file, and the ELFARInfo object
    //  will be recorded into 'm_symbol_ar_info_map'.
    //5.If there is unresolved RelocInfo with the same name as 'symbol' in
    //  'm_unresolved_reloc_idx_map', ELFMgr to which 'symbol' belong will be
    //  got by 'findELFMgr' function or generated from AR file according to
    //  ELFARInfo. If a new ELFMgr is generated, SymbolInfo and RelocInfo which
    //  are belong to this ELFMgr need to be recorded into the LinkerMgr.
    //6.RelocInfo will be resolved by this target 'symbol'.
    while (hasUnResolvedRelocSymbol(false)) {
        //Get AR file utill all AR file have been used.
        ELFAR * ar = m_armgr.processARFile();
        if (ar == nullptr) { break; }

        UINT len = 0;
        for (UINT i = 0; i < ELFAR_index_num(ar); i++) {
            CHAR * sym_str = ELFAR_sym_tab(ar) + len;
            ASSERT0(sym_str);
            Sym const* global_sym = m_output_elf_mgr->addToSymTab(sym_str);
            ASSERT0(global_sym);

            UINT64 index = ELFAR_idx_array(ar)[i];
            len += ELF_INCREASE_VALUE((UINT)::strnlen(
                ELFAR_sym_tab(ar) + len, ELFAR_sym_tab_size(ar) - len));

            if (checkHasBeenResolvedReloc(global_sym)) { continue; }
            //Now no RelocInfo needs to be resolved by global_sym. But it may
            //be needed by other RelocInfo that occurs in the funture.
            if (!m_unresolved_reloc_idx_map.find(global_sym)) {
                m_armgr.saveSymbolARInfo(ar, global_sym, index);
                continue;
            }
            //There is unresolved RelocInfo that needs to be resolved
            //by SymbolInfo with the name of 'global_sym'. And the
            //'elf_mgr' to which 'global_sym' belong has been recorded.
            ELFMgr * elf_mgr = getELFMgrWhichSymbolBelongTo(ar, index);
            ASSERT0(elf_mgr);

            resolveRelocInfoWithELFMgr(elf_mgr, global_sym);
        }
    }
}


void LinkerMgr::resolveRelocInfoWithELFMgr(
    MOD ELFMgr * elf_mgr, Sym const* symbol_name)
{
    ASSERT0(elf_mgr && symbol_name);

    //There is RelocInfo that needs to be resolved by the SymbolInfo with the
    //name of 'symbol_name'. And the SymbolInfo will be got from 'elf_mgr'.
    //Resolve RelocInfo with SymbolInfo that comes from 'elf_mgr'.
    if (!m_unresolved_reloc_idx_map.find(symbol_name)) { return; }

    //Get target SymbolInfo from 'elf_mgr'.
    SymbolInfo * target_symbol_info = elf_mgr->getSymbolInfo(symbol_name);
    ASSERT0(target_symbol_info);

    if ((SYMINFO_sym(target_symbol_info).st_bind == STB_GLOBAL ||
         SYMINFO_sym(target_symbol_info).st_bind == STB_WEAK) &&
        SYMINFO_sym(target_symbol_info).st_type != STT_NOTYPE &&
        SYMINFO_sym(target_symbol_info).st_shndx != SHN_UNDEF) {
        //Resolved RelocInfo.
        resolveRelocInfo(symbol_name, target_symbol_info);
        return;
    }
    UNREACHABLE();
}


bool LinkerMgr::checkHasBeenResolvedReloc(Sym const* symbol_name)
{
    ASSERT0(symbol_name);

    //The 'symbol_name' of SymbolInfo isn't used to resolve RelocInfo.
    if (!m_resolved_reloc_idx_map.find(symbol_name)) { return false; }

    //Get the vector of RelocInfo with the same name as 'symbol_name'
    //that has been resolved by the SymbolInfo.
    Vector<UINT> * reloc_symbol_idx =
        m_resolved_reloc_idx_map.get(symbol_name);
    ASSERT0(reloc_symbol_idx && (reloc_symbol_idx->get_elem_count() > 0));
    //Get a RelocInfo from the vector.
    RelocInfo * reloc_info = m_reloc_symbol_vec[reloc_symbol_idx->get(0)];
    ASSERT0(reloc_info);
    //Get target resolved SymbolInfo of RelocInfo.
    SymbolInfo * symbol_info = RELOCINFO_sym(reloc_info);
    ASSERT0(symbol_info);

    //Previous target resolved SymbolInfo is with STT_NOTYPE attribute.
    //Now there is another un-STT_NOTYPE SymbolInfo. It needs to use this
    //SymbolInfo to resolve RelocInfo instead.
    if (SYMINFO_sym(symbol_info).st_type == STT_NOTYPE) {
        //Undo this resolved info.
        for (UINT i = 0; i < reloc_symbol_idx->get_elem_count(); i++) {
            UINT idx = reloc_symbol_idx->get(i);
            RelocInfo * reloc_symbol_info = m_reloc_symbol_vec[idx];
            ASSERT0(reloc_symbol_info);

            RELOCINFO_is_resolved(reloc_symbol_info) = false;
            RELOCINFO_resolved_notype(reloc_symbol_info) = false;
            RELOCINFO_sym(reloc_symbol_info) = nullptr;
        }
        if (g_elf_opt.isDumpLink()) {
            xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
                "Undo notype %s\n", SYMINFO_name(symbol_info)->getStr());
        }
        m_resolved_reloc_idx_map.remove(symbol_name);
        m_unresolved_reloc_idx_map.set(symbol_name, reloc_symbol_idx);
        return false;
    }

    if (SYMINFO_sym(symbol_info).st_bind == STB_WEAK) {
        //TODO: Process 'weak' and 'strong' attribute.
        if (g_elf_opt.isDumpLink()) {
            xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
                "This is Weak symbol\n");
        }
    }
    return true;
}


void LinkerMgr::resolveRelocInfo(Sym const* sym_name,
    MOD SymbolInfo * symbol_info, bool is_notype)
{
    ASSERT0(sym_name && symbol_info);
    ASSERT0(m_unresolved_reloc_idx_map.find(sym_name));

    Vector<UINT> * reloc_idx_vec = m_unresolved_reloc_idx_map.get(sym_name);
    ASSERT0(reloc_idx_vec);

    for (UINT i = 0; i < reloc_idx_vec->get_elem_count(); i++) {
        UINT idx = reloc_idx_vec->get(i);
        RelocInfo * reloc_symbol_info = m_reloc_symbol_vec[idx];
        ASSERT0(reloc_symbol_info);

        RELOCINFO_is_resolved(reloc_symbol_info) = true;
        RELOCINFO_sym(reloc_symbol_info) = symbol_info;
        RELOCINFO_resolved_notype(reloc_symbol_info) = is_notype ? true : false;

        if (g_elf_opt.isDumpLink()) {
            if (is_notype) {
                xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
                    "\nresolved symbol(notype): %d %s\n", idx, sym_name);
                continue;
            }
            xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
                "\nresolved symbol: %d %s\n", idx, sym_name);
            if (SYMINFO_file_name(symbol_info) != nullptr) {
                xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
                    "filename: %s\n", SYMINFO_file_name(symbol_info)->getStr());
            }
        }
    }

    //Update unresolved and resolved RelocInfo map.
    m_unresolved_reloc_idx_map.remove(sym_name);
    ASSERT0(!m_resolved_reloc_idx_map.find(sym_name));
    m_resolved_reloc_idx_map.set(sym_name, reloc_idx_vec);
}


void LinkerMgr::resolveRelocInfoReplaceNoType(
    Sym const* sym_name, MOD SymbolInfo * symbol_info)
{
    ASSERT0(sym_name && symbol_info);

    //SymbolInfo with 'STT_NOTYPE' attribute can be used to resolve RelocInfo
    //too. But it needs to be replaced if there is another same name SymbolInfo
    //with no 'STT_NOTYPE' attribute later.
    //e.g.: Given RelocInfo. Firstly it has been resolved by a SymbolInfo that
    //      its type is STT_NOTYPE. If another SymbolInfo that its type is
    //      STT_OBJECT has been found from other ELFMgr later, the RelocInfo
    //      will be resolved by this STT_OBJECT SymbolInfo instead.
    //      | STT_NOTYPE SymbolInfo replaced by STT_OBJECT SymbolInfo |
    //      |---------------------------------------------------------|
    //      |  Value    Size  Type    Bind     Vis    Ndx    Name     |
    //      | 00000000   0   NOTYPE  GLOBAL  DEFAULT  UND  symbol_str |
    //      | 00000020   8   OBJECT  GLOBAL  HIDDEN   5    symbol_str |
    //      |---------------------------------------------------------|
    Vector<UINT> * reloc_idx_vec = m_resolved_reloc_idx_map.get(sym_name);
    ASSERT0(reloc_idx_vec);

    for (UINT i = 0; i < reloc_idx_vec->get_elem_count(); i++) {
        UINT idx = reloc_idx_vec->get(i);
        RelocInfo * reloc_symbol_info = m_reloc_symbol_vec[idx];
        ASSERT0(reloc_symbol_info &&
            (RELOCINFO_is_resolved(reloc_symbol_info)) &&
            (RELOCINFO_resolved_notype(reloc_symbol_info)));

        RELOCINFO_resolved_notype(reloc_symbol_info) = false;
        RELOCINFO_sym(reloc_symbol_info) = symbol_info;
        if (g_elf_opt.isDumpLink()) {
            xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
                "resolved symbol(instead notype): %d %s \n",
                idx, sym_name->getStr());
        }
    }
}


void LinkerMgr::updateRelocInfoVec(ELFMgr const* elf_mgr)
{
    ASSERT0(elf_mgr);

    //Resize the capacity of 'm_reloc_symbol_vec'.
    UINT vec_sz = m_reloc_symbol_vec.get_elem_count();
    UINT total_sz = ELFMGR_reloc_vec(elf_mgr).get_elem_count() + vec_sz;
    if (m_reloc_symbol_vec.get_capacity() < total_sz) {
        m_reloc_symbol_vec.grow(total_sz);
    }

    //Append RelocInfo in 'elf_mgr' to 'm_reloc_info_vec'.
    for (UINT i = 0; i < ELFMGR_reloc_vec(elf_mgr).get_elem_count(); i++) {
        RelocInfo * reloc_info = ELFMGR_reloc_vec(elf_mgr)[i];
        ASSERT0(reloc_info);
        m_reloc_symbol_vec.set(vec_sz + i, reloc_info);
        //If there is related RelocInfo of 'reloc_info', it need to process.
        //e.g.: The means of reloc_type of 'reloc_info' depended on
        //      the reloc_type of related RelocInfo.
        if (ELF_INCREASE_VALUE(i) >=
            ELFMGR_reloc_vec(elf_mgr).get_elem_count()) { continue; }
        processRelateRelocInfo(reloc_info, m_reloc_symbol_vec.get_last_idx());
    }
}


bool LinkerMgr::checkCurrentRelocInfoHasBeenResolved(
    MOD ELFMgr * elf_mgr, MOD RelocInfo * reloc_info, UINT reloc_index)
{
    ASSERT0(elf_mgr && reloc_info);

    //Current 'reloc_info' doesn't be resolved.
    if (!m_resolved_reloc_idx_map.find(RELOCINFO_name(reloc_info))) {
        return false;
    }

    //RelocInfo with the same name as 'reloc_info' has been resolved.
    //Thus 'reloc_info' can be resolved by the same target resolved
    //symbol of RelocInfo.
    relocInfoHasBeenResolved(reloc_info, reloc_index);

    //Though the 'reloc_info' has been resolved, there may be another
    //target resolved SymbolInfo with un-STT_NOTYPE attribute in 'elf_mgr'.
    //If the SymbolInfo existed, it need to replace the original target
    //resolved SymbolInfo of 'reloc_info'.
    if (!ELFMGR_symbol_map(elf_mgr).find(RELOCINFO_name(reloc_info))) {
        return true;
    }

    //Get the vector of RelocInfo with the same name.
    Vector<UINT> * reloc_symbol_idx =
        m_resolved_reloc_idx_map.get(RELOCINFO_name(reloc_info));
    ASSERT0(reloc_symbol_idx && (reloc_symbol_idx->get_elem_count() > 0));

    //Get RelocInfo from the vector.
    RelocInfo * resolved_reloc_info =
        m_reloc_symbol_vec[reloc_symbol_idx->get(0)];
    ASSERT0(resolved_reloc_info);

    //Get target resolved SymbolInfo from RelocInfo.
    SymbolInfo * resolved_symbol_info = RELOCINFO_sym(resolved_reloc_info);
    ASSERT0(resolved_symbol_info);

    //Get target resolved SymbolInfo from 'elf_mgr'.
    SymbolInfo * symbol_info =
        ELFMGR_symbol_map(elf_mgr).get(RELOCINFO_name(reloc_info));
    ASSERT0(symbol_info);

    //Check.
    if (SYMINFO_sym(symbol_info).st_type != STT_NOTYPE &&
        SYMINFO_sym(resolved_symbol_info).st_type == STT_NOTYPE) {
        resolveRelocInfoReplaceNoType(RELOCINFO_name(reloc_info), symbol_info);
    }
    return true;
}


void LinkerMgr::processRelocInfo(
    ELFAR const* ar, MOD ELFMgr * elf_mgr, UINT64 idx)
{
    ASSERT0(ar && elf_mgr);

    //Record the original size of 'm_reloc_symbol_vec'.
    UINT reloc_index = ELF_INCREASE_VALUE(m_reloc_symbol_vec.get_last_idx());

    //Record RelocInfo of 'elf_mgr' to 'm_reloc_info_vec'.
    updateRelocInfoVec(elf_mgr);

    //Process RelocInfo of 'elf_mgr'. The RelocInfo of 'elf_mgr' may has
    //been resolved or there is reusable info of RelocInfo to help resolve.
    for (UINT i = 0; i < ELFMGR_reloc_vec(elf_mgr).get_elem_count(); i++) {
        RelocInfo * reloc_info = ELFMGR_reloc_vec(elf_mgr)[i];
        ASSERT0(reloc_info);

        //There is SymbolInfo with same name as 'reloc_info' in 'elf_mgr'.
        if (ELFMGR_symbol_map(elf_mgr).find(RELOCINFO_name(reloc_info))) {
            SymbolInfo * symbol_info =
                ELFMGR_symbol_map(elf_mgr).get(RELOCINFO_name(reloc_info));
            ASSERT0(symbol_info);

            //Check whether 'symbol_info' can be used as
            //target SymbolInfo to resolve 'reloc_info'.
            if (RELOCINFO_sym_idx(reloc_info) == SYMINFO_index(symbol_info) &&
                SYMINFO_sym(symbol_info).st_bind == STB_LOCAL &&
                SYMINFO_sym(symbol_info).st_type == STT_SECTION) {
                //RelocInfo with same name as 'reloc_info' has been resolved.
                //Thus 'reloc_info' will be resolved with info from RelocInfo.
                if (checkCurrentRelocInfoHasBeenResolved(
                    elf_mgr, reloc_info, reloc_index + i)) { continue; }

                //'reloc_info' doesn't be resolved. It's info needs to be
                //recorded to 'm_unresolved_reloc_idx_map' before resolved.
                updateUnresolvedRelocInfo(elf_mgr, reloc_info, reloc_index + i);
                //Use 'symbol_info' to resolve 'reloc_info'.
                targetSymbolInfoFoundInCurrentELFMgr(elf_mgr, reloc_info);
                continue;
            }
        }

        //Check whether the RelocInfo has been resolved.
        if (checkCurrentRelocInfoHasBeenResolved(
            elf_mgr, reloc_info, reloc_index + i)) { continue; }

        //Current reloc symbol isn't resolved, it needed to record the
        //'reloc info' to 'm_unresolved_reloc_idx_map'.
        updateUnresolvedRelocInfo(elf_mgr, reloc_info, reloc_index + i);

        //Though reloc symbol isn't resolved, related reusable info
        //(elf_mgr info) has been recorded. Just use the info to resolve.
        if (m_armgr.findFromSymbolARInfoMap(RELOCINFO_name(reloc_info))) {
            relocInfoHasBeenRecorded(ar, elf_mgr, reloc_info, idx);
            continue;
        }

        //There is target resolved symbol of RelocInfo in current 'elf_mgr.
        if (ELFMGR_symbol_map(elf_mgr).find(RELOCINFO_name(reloc_info))) {
            targetSymbolInfoFoundInCurrentELFMgr(elf_mgr, reloc_info);
        }
    }
}


void LinkerMgr::targetSymbolInfoFoundInCurrentELFMgr(
    MOD ELFMgr * elf_mgr, RelocInfo const* reloc_info)
{
    ASSERT0(elf_mgr && reloc_info);

    Sym const* symbol_name = RELOCINFO_name(reloc_info);
    ASSERT0(ELFMGR_symbol_map(elf_mgr).find(symbol_name));

    SymbolInfo * symbol_info = elf_mgr->getSymbolInfo(symbol_name);
    ASSERT0(symbol_info);

    //Process SymbolInfo according to different attribute.
    if (SYMINFO_sym(symbol_info).st_type != STT_NOTYPE &&
        SYMINFO_sym(symbol_info).st_shndx != SHN_UNDEF) {
        //If the 'symbol_info' is 'STT_SECTION' type, it will try to find
        //a SymbolInfo with no 'STT_SECTION' as target resolved SymbolInfo.
        SymbolInfo * sym = nullptr;
        if ((SYMINFO_sym(symbol_info).st_type == STT_SECTION) &&
            elf_mgr->findSymbolWithNoSectionType(symbol_info, &sym)) {
            symbol_info = sym;
        }
        resolveRelocInfo(symbol_name, symbol_info);
        return;
    }

    //The SymbolInfo with 'STT_NOTYPE' attribute also can be
    //used to resolve RelocInfo. But it needed to set a flag.
    resolveRelocInfo(symbol_name, symbol_info, true);
}


void LinkerMgr::updateUnresolvedRelocInfo(
    MOD ELFMgr * elf_mgr, MOD RelocInfo * reloc_info, UINT index)
{
    ASSERT0(elf_mgr && reloc_info);

    //'reloc_info' has been recorded into 'm_unresolved_reloc_idx_map'.
    if (m_unresolved_reloc_idx_map.find(RELOCINFO_name(reloc_info))) {
        //Note: m_symbol_info_vec don't contains the first null element.
        ASSERT0(RELOCINFO_sym_idx(reloc_info) <
                ELFMGR_symbol_vec(elf_mgr).get_elem_count());
        SymbolInfo * symbol_info =
            (ELFMGR_symbol_vec(elf_mgr)[
                (VecIdx)RELOCINFO_sym_idx(reloc_info)]);
        ASSERT0(symbol_info);

        //Upate RelocInfo to 'm_unresolved_reloc_idx_map'.
        Vector<UINT> * vec = m_unresolved_reloc_idx_map.get(
            RELOCINFO_name(reloc_info));
        ASSERT0(vec);
        vec->append(index);
        return;
    }

    //Allocate new item in 'm_unresolved_reloc_idx_map'.
    allocUnresolvedRelocSymbol(reloc_info, index);
}


bool LinkerMgr::targetSymbolInTheSameELFMgr(ELFAR const* ar, UINT64 idx,
    ELFARInfo const* ar_info, MOD ELFMgr * elf_mgr, RelocInfo const* reloc_info)
{
    ASSERT0(ar && ar_info && elf_mgr && reloc_info);

    if (ELFARINFO_ar(ar_info) != ar || ELFARINFO_elf_mgr_idx(ar_info) != idx) {
        return false;
    }

    SymbolInfo * symbol_info =
        elf_mgr->getSymbolInfo(RELOCINFO_name(reloc_info));
    ASSERT0(symbol_info);

    if ((SYMINFO_sym(symbol_info).st_bind == STB_GLOBAL ||
         SYMINFO_sym(symbol_info).st_bind == STB_WEAK) &&
         SYMINFO_sym(symbol_info).st_type != STT_NOTYPE &&
         SYMINFO_sym(symbol_info).st_shndx != 0) {
        //If the symbol is 'STT_SECTION' type. Try to find no 'STT_SECTION'
        //to resolve.
        SymbolInfo * sym = nullptr;
        if ((SYMINFO_sym(symbol_info).st_type == STT_SECTION) &&
            (elf_mgr->findSymbolWithNoSectionType(symbol_info, &sym))) {
            symbol_info = sym;
        }
        resolveRelocInfo(SYMINFO_name(symbol_info), symbol_info);
        return true;
    }
    return false;
}


bool LinkerMgr::targetSymbolNoInTheSameELFMgr(
    ELFARInfo const* ar_info, RelocInfo const* reloc_info)
{
    ASSERT0(ar_info && reloc_info);

    //Try to find target resolved symbol of 'reloc_info' from other ELFMgr.
    if (!m_armgr.findFromARInfoMap(ELFARINFO_ar(ar_info))) { return false; }

    bool elf_mgr_has_been_recorded = false;
    ELFARInfo * elf_mgr_elem = nullptr;
    ELFARInfoVec * vec = m_armgr.getFromARInfoMap(ELFARINFO_ar(ar_info));
    ASSERT0(vec);

    //Find target ELFARInfo according to the idx'.
    for (UINT i = 0; i < vec->get_elem_count(); i++) {
        ELFARInfo * find_ar_info = vec->get(i);
        ASSERT0(find_ar_info);

        if (ELFARINFO_elf_mgr_idx(find_ar_info) ==
            ELFARINFO_elf_mgr_idx(ar_info)) {
            elf_mgr_has_been_recorded = true;
            elf_mgr_elem = find_ar_info;
            break;
        }
    }

    if (!elf_mgr_has_been_recorded) { return false; }

    ELFMgr * elf_mgr = ELFARINFO_elf_mgr(elf_mgr_elem);
    ASSERT0(elf_mgr);
    //Get target resolved symbol of 'reloc_info' from 'elf_mgr'.
    SymbolInfo * target_symbol_info =
        elf_mgr->getSymbolInfo(RELOCINFO_name(reloc_info));
    ASSERT0(target_symbol_info);
    resolveRelocInfo(SYMINFO_name(target_symbol_info), target_symbol_info);

    return true;
}


void LinkerMgr::relocInfoHasBeenRecorded(ELFAR const* ar,
     MOD ELFMgr * elf_mgr, RelocInfo const* reloc_info, UINT64 idx)
{
    ASSERT0(ar && elf_mgr && reloc_info && m_armgr.findFromSymbolARInfoMap(
        RELOCINFO_name(reloc_info)));

    ELFARInfo * ar_info =
        m_armgr.getFromSymbolARInfoMap(RELOCINFO_name(reloc_info));
    ASSERT0(ar_info);

    //Both target resolved SymbolInfo and 'reloc_info' are in the same
    //'elf_mgr'. The 'reloc_info' will be resolved by the SymbolInfo.
    if (targetSymbolInTheSameELFMgr(ar, idx, ar_info, elf_mgr, reloc_info)) {
        return;
    }

    //Target resolved SymbolInfo and 'reloc_info' are not in the same elf_mgr.
    if (targetSymbolNoInTheSameELFMgr(ar_info, reloc_info)) { return; }

    //Target resolved SymbolInfo comes from relocated elf_mgr.
    ELFMgr * elf_mgr_inner = getELFMgrFromARViaIdx(
        ELFARINFO_ar(ar_info), ELFARINFO_elf_mgr_idx(ar_info));
    ASSERT0(elf_mgr_inner);

    //Addressing the problem that SymbolInfo with same name in different ELFMgr.
    if (m_elf_mgr_list.get_elem_count() != 0) {
        handleSameNameInDiffFile(elf_mgr_inner, false);
    }

    //Update info.
    ELFMGR_symtab_vec(m_output_elf_mgr).append(
        &(ELFMGR_symbol_vec(elf_mgr_inner)));
    m_armgr.saveARInfo(
        ELFARINFO_ar(ar_info), elf_mgr_inner, ELFARINFO_elf_mgr_idx(ar_info));

    //Get target resolved SymbolInfo and resolve RelocInfo.
    SymbolInfo * target_symbol_info =
        elf_mgr_inner->getSymbolInfo(RELOCINFO_name(reloc_info));
    ASSERT0(target_symbol_info);
    resolveRelocInfo(SYMINFO_name(target_symbol_info), target_symbol_info);

    //Process RelocInfo in 'elf_mgr_inner'. It is recursive function.
    processRelocInfo(ELFARINFO_ar(ar_info), elf_mgr_inner,
        ELFARINFO_elf_mgr_idx(ar_info));
}


ELFMgr * LinkerMgr::getELFMgrFromARViaIdx(MOD ELFAR * ar, UINT64 index)
{
    ASSERT0(ar);

    ELFMgr * elf_mgr = allocELFMgr();
    ASSERT0(elf_mgr);
    elf_mgr->initELFMgrInfo(&m_sym_tab, ar->m_file_name, true);

    //Open AR file.
    if (!ar->m_file->m_is_opened) { ar->open(ar->m_file_name, &m_armgr); }
    //Get ELF info from AR file according to 'index'(offset in AR file).
    if (ar->getArchiveELF(elf_mgr, index) != EM_SUCC) {
        ASSERTN(0, ("Get ELF failed!"));
    }
    //Collect object info.
    elf_mgr->collectObjELF();
    return elf_mgr;
}


void LinkerMgr::relocInfoHasBeenResolved(
    MOD RelocInfo * reloc_info, UINT index)
{
    ASSERT0(reloc_info && m_resolved_reloc_idx_map.find(
        RELOCINFO_name(reloc_info)));

    //Current RelocInfo has been resolved. Set resolved flag.
    RELOCINFO_is_resolved(reloc_info) = true;

    //Update index vec.
    Vector<UINT> * reloc_symbol_index =
        m_resolved_reloc_idx_map.get(RELOCINFO_name(reloc_info));
    ASSERT0(reloc_symbol_index);
    reloc_symbol_index->append(index);

    //Get target resolved SymbolInfo of RelocInfo.
    RelocInfo * other_same_reloc_info =
        m_reloc_symbol_vec[reloc_symbol_index->get(0)];
    ASSERT0(other_same_reloc_info);

    //Set target resolved SymbolInfo to RelocInfo.
    RELOCINFO_sym(reloc_info) = RELOCINFO_sym(other_same_reloc_info);
    RELOCINFO_resolved_notype(reloc_info) =
        RELOCINFO_resolved_notype(other_same_reloc_info);
}


bool LinkerMgr::hasUnResolvedRelocSymbol(bool is_notype_as_resolved) const
{
    //Check whether there is still unresolved RelocInfo.
    //'is_notype_as_resolved': SymbolInfo with STT_NOTYPE attribute also
    //can be used to resolve RelocInfo. The flag dedicated whether these
    //RelocInfos need to be counted. 'true': RelocInfo that resoved by
    //SymbolInfo with STT_NOTYPE doesn't need to be counted as unresolved
    //RelocInfo. 'false': RelocInfo that resolved by SymbolInfo with
    //STT_NOTYPE needs to be counted as unresolved RelocInfo.
    for (UINT i = 0; i < m_reloc_symbol_vec.get_elem_count(); i++) {
        RelocInfo * reloc_info = m_reloc_symbol_vec[i];
        ASSERT0(reloc_info);

        //Whehter the RelocInfo has been resolved.
        if (RELOCINFO_is_resolved(reloc_info)) {
            //Whether notype-resolved-symbol as resolved symbol.
            if (RELOCINFO_resolved_notype(reloc_info) &&
                !is_notype_as_resolved) { return true; }
            continue;
        }
        return true;
    }
    return false;
}


void LinkerMgr::initUnResolveSymbol()
{
    for (UINT i = 0; i < m_reloc_symbol_vec.get_elem_count(); i++) {
        RelocInfo * reloc_info = m_reloc_symbol_vec[i];
        ASSERT0(reloc_info);

        //Process related RelocInfo. The means of reloc_type of RelocInfo
        //may be depended on the reloc_type of related RelocInfo.
        if (ELF_INCREASE_VALUE(i) < m_reloc_symbol_vec.get_elem_count()) {
            processRelateRelocInfo(reloc_info, i);
        }

        //Unresolve symbol vector has been created in Map.
        //The index of 'reloc_info' just appended to it.
        if (m_unresolved_reloc_idx_map.find(RELOCINFO_name(reloc_info))) {
            Vector<UINT> * vec = m_unresolved_reloc_idx_map.get(
                RELOCINFO_name(reloc_info));
            ASSERT0(vec);
            vec->append(i);
            continue;
        }

        //Create new unresolved symbol vec.
        allocUnresolvedRelocSymbol(reloc_info, i);
    }
}


void LinkerMgr::processOBJFileList(MOD xcom::List<CHAR const*> * obj_file_list,
                                   OUT xcom::List<ELFMgr*> * elf_mgr_list)
{
    ASSERT0(obj_file_list && elf_mgr_list);

    //Collect info from xoc::Var.
    for (ELFMgr * elf_mgr = elf_mgr_list->get_head();
         elf_mgr != nullptr; elf_mgr = elf_mgr_list->get_next()) {
        elf_mgr->initELFMgrInfo(&m_sym_tab, nullptr, false);
        elf_mgr->collectELFInfoFromVar();
    }

    //Collect info from object file.
    for (CHAR const* fn = obj_file_list->get_head();
         fn != nullptr; fn = obj_file_list->get_next()) {
        if (!xcom::FileObj::isFileExist(fn)) {
            prt2C("\nerror: %s doesn't exist.", fn);
            ASSERT0(0);
        }
        ELFMgr * elf_mgr = allocELFMgr();
        ASSERT0(elf_mgr);

        //ELF read into memory.
        if (elf_mgr->readELF(fn, true) != EM_SUCC) {
            prt2C("\nerror: read objfile %s failed.", fn);
            ASSERT0(0);
        }

        //Collect useful info to 'elf_mgr'.
        elf_mgr->initELFMgrInfo(&m_sym_tab, fn, true);
        elf_mgr->collectObjELF();
        elf_mgr_list->append_tail(elf_mgr);
    }
}


void LinkerMgr::collectAliasInfo(MOD xcom::List<ELFMgr*> * elf_mgr_list)
{
    ASSERT0(elf_mgr_list);

    //Collect alias info.
    AliasSymbolMapIter iter;
    for (ELFMgr * elf_mgr = elf_mgr_list->get_head();
         elf_mgr != nullptr; elf_mgr = elf_mgr_list->get_next()) {
        iter.clean();
        Sym const* v;
        //Add alias and it's aliasee name into the SymTab of 'm_output_elf_mgr'.
        for (Sym const* k = ELFMGR_alias_map(elf_mgr).get_first(iter, &v);
             !iter.end(); k = ELFMGR_alias_map(elf_mgr).get_next(iter, &v)) {
            ASSERT0(k && v);
            ELFMGR_alias_map(m_output_elf_mgr).set(
                m_output_elf_mgr->addToSymTab(k->getStr()),
                m_output_elf_mgr->addToSymTab(v->getStr()));
        }
    }
}


void LinkerMgr::outputSharedObjFile(MOD xcom::List<ELFMgr*> * elf_mgr_list)
{
    ASSERT0(elf_mgr_list);

    //Collect alias info from each ELFMgr in 'elf_mgr_list'.
    collectAliasInfo(elf_mgr_list);

    for (ELFMgr * elf_mgr = elf_mgr_list->get_head();
         elf_mgr != nullptr; elf_mgr = elf_mgr_list->get_next()) {
        //Merge all valid info in 'elf_mgr' into LinkerMgr.
        updateELFInfo(elf_mgr);
    }

    //Resolve RelocInfo.
    doResolve();

    //Generate executed ELF.
    processOutputExeELF();
}


bool LinkerMgr::skipSpecificRelaDyn(
    MOD ELFMgr * elf_mgr, MOD RelocInfo const* reloc_info) const
{
    ASSERT0(elf_mgr && reloc_info);

    if (RELOCINFO_sect_name(reloc_info) == nullptr) { return false; }

    SECTION_TYPE sect_type = elf_mgr->getSectionType(
        RELOCINFO_sect_name(reloc_info));
    return (sect_type == SH_TYPE_DEBUG_LOC ||
            sect_type == SH_TYPE_DEBUG_STR ||
            sect_type == SH_TYPE_DEBUG_LINE ||
            sect_type == SH_TYPE_DEBUG_INFO ||
            sect_type == SH_TYPE_DEBUG_FRAME ||
            sect_type == SH_TYPE_DEBUG_ABBREV ||
            sect_type == SH_TYPE_DEBUG_RANGES ||
            sect_type == SH_TYPE_DEBUG_ARANGES);
}


void LinkerMgr::preProcessRelaDynSectBeforeSetSectAddr(MOD ELFMgr * elf_mgr)
{
    ASSERT0(elf_mgr);

    UINT reladyn_item_num = 0;
    Off got_offset = 0;
    //The first element of '.dynsym' is UNDEF and it's index is 0
    //in ELF format. Thus the index of rest elements begin from 1.
    UINT dynsym_idx = ELF_DYNSYM_ELEM_BEGIN_INDEX;
    elf_mgr->setRelaDynLocalItemNum(0);

    for (UINT i = 0; i < m_reloc_symbol_vec.get_elem_count(); i++) {
        RelocInfo * reloc_info = m_reloc_symbol_vec[i];
        ASSERT0(reloc_info);

        if (RELOCINFO_resolved_notype(reloc_info) ||
            !elf_mgr->isRelaDynSymbol(
                RELOCINFO_name(reloc_info), RELOCINFO_type(reloc_info)) ||
            skipSpecificRelaDyn(elf_mgr, reloc_info)) { continue; }

        RelocInfo * recorded;
        if (elf_mgr->isNeededToCreateGotItem(reloc_info) &&
            elf_mgr->hasBeenRecordedRelaDynInfoItem(reloc_info, &recorded)) {
            RELOCINFO_sect_ofst(reloc_info) = RELOCINFO_sect_ofst(recorded);
            continue;
        }

        elf_mgr->setRelaDynInfo(reloc_info, got_offset, dynsym_idx);
        if (elf_mgr->isSymbolWithLocalAttr(RELOCINFO_sym(reloc_info))) {
            UINT local_item_num = elf_mgr->getRelaDynLocalItemNum();
            elf_mgr->setRelaDynLocalItemNum(ELF_INCREASE_VALUE(local_item_num));
        }
        reladyn_item_num++;
    }

    //In this function, the .rela_dyn section is just being calculated the
    //number of rela_dyn items and the size of section content. The section
    //content will be generated until all section address have been set.
    //Since there is d_val field of ELFDyn needs to be set by the address of
    //section. Thus the section content will be alligned 0 temporarily.
    BYTEVec * content = elf_mgr->getSectionContent(SH_TYPE_RELA_DYN);
    ASSERT0(content);
    UINT content_sz = reladyn_item_num * ELFRela::getSize(elf_mgr);
    if (content->get_capacity() < content_sz) { content->grow(content_sz); }
    content->set(content_sz - 1, 0);
    ::memset((void*)(content->get_vec()), 0, content_sz);

    //Set GOT element number.
    elf_mgr->setGotElemNum(got_offset / BITS_PER_BYTE);
}


void LinkerMgr::mergedShdrWithProgBitsType(
    ELFSHdr const* shdr, Sym const* shdr_subname)
{
    ASSERT0(shdr && shdr_subname);

    switch (m_output_elf_mgr->getSectionType(shdr_subname)) {
    case SH_TYPE_TEXT:
    case SH_TYPE_TEXT1:
    case SH_TYPE_DATA:
    case SH_TYPE_DL_TDATA:
    case SH_TYPE_RODATA:
    case SH_TYPE_RODATA1:
    case SH_TYPE_LDM:
    case SH_TYPE_EH_FRAME:
    case SH_TYPE_DEBUG_INFO:
    case SH_TYPE_DEBUG_LINE:
    case SH_TYPE_DEBUG_ABBREV:
    case SH_TYPE_DEBUG_ARANGES:
    case SH_TYPE_DEBUG_RANGES:
    case SH_TYPE_DEBUG_STR:
    case SH_TYPE_DEBUG_FRAME:
        //'false' represents the shdr isn't BSS section.
        mergeShdrImpl(shdr, shdr_subname, false);
        break;
    case SH_TYPE_COMMENT:
    case SH_TYPE_NOTE:
        //TODO: Wait other part.
        break;
    default:
        UNREACHABLE();
        break;
    }
}

void LinkerMgr::mergeShdrImpl(ELFSHdr const* shdr,
    Sym const* shdr_name, bool is_bss_shdr)
{
    ASSERT0(shdr && shdr_name);

    BYTEVec * content =
        m_output_elf_mgr->getSectContentWithGenIfNotExist(shdr_name);
    ASSERT0(content);

    SectionInfo * sect_info = m_output_elf_mgr->getSection(shdr_name);
    ASSERT0(sect_info);

    //Update align/flag info.
    SECTINFO_align(sect_info) =
        MAX(SECTINFO_align(sect_info), shdr->s_addr_align);
    SECTINFO_flag(sect_info) |= shdr->s_flags;

    if (shdr->s_size == 0) { return; }

    //Resize content capacity.
    UINT base_ofst = (UINT)xcom::ceil_align(
        content->get_elem_count(), shdr->s_addr_align);
    UINT total_size = (UINT)(base_ofst + shdr->s_size);
    if (content->get_capacity() < total_size) { content->grow(total_size); }
    content->set((VecIdx)(total_size - 1), 0);

    if (is_bss_shdr) {
        //BSS section needs to be assigned 0.
        ::memset((void*)(content->get_vec() + base_ofst), 0, shdr->s_size);
        return;
    }

    //Copy shdr content to output ELFMgr section.
    ::memcpy((void*)(content->get_vec() + base_ofst),
             (void*)(shdr->s_content), shdr->s_size);
}


void LinkerMgr::mergeSymbolInfoWithCOMAttr(
    MOD ELFMgr * elf_mgr, UINT shdr_idx, Sym const* shdr_subname)
{
    ASSERT0(elf_mgr && shdr_subname);

    //SymbolInfo with SHN_COMMON attribute needs to be allocated memory
    //space and assigned 0 in '.bss' section.
    BYTEVec * content =
        m_output_elf_mgr->getSectContentWithGenIfNotExist(shdr_subname);
    ASSERT0(content);

    SectionInfo * sect_info = m_output_elf_mgr->getSection(shdr_subname);
    ASSERT0(sect_info);

    for (UINT i = 0; i < ELFMGR_symbol_vec(elf_mgr).get_elem_count(); i++) {
        SymbolInfo * symbol_info = ELFMGR_symbol_vec(elf_mgr)[i];
        ASSERT0(symbol_info);

        if (elf_mgr->isNullSymbol(symbol_info, i)) { continue; }
        if (SYMINFO_sym(symbol_info).st_shndx != SHN_COMMON) { continue; }

        //Assign 0 for this section content.
        UINT base_ofst = (UINT)xcom::ceil_align(content->get_elem_count(),
            SYMINFO_sym(symbol_info).st_value);
        UINT sym_sz = (UINT)SYMINFO_sym(symbol_info).st_size;
        UINT total_sz = base_ofst + sym_sz;
        if (content->get_capacity() < total_sz) { content->grow(total_sz); }
        content->set(total_sz - 1, 0);
        ::memset((void*)(content->get_vec() + base_ofst), 0, sym_sz);

        //Update info.
        SECTINFO_align(sect_info) =
             MAX(SECTINFO_align(sect_info), SYMINFO_sym(symbol_info).st_value);
        //Reset the section name to 'SH_TYPE_BSS'.
        SYMINFO_sect_type(symbol_info) = SH_TYPE_BSS;
        SYMINFO_sym(symbol_info).st_shndx = shdr_idx;
        SYMINFO_sect_name(symbol_info) = m_output_elf_mgr->addToSymTab(
            elf_mgr->getSectionName(SH_TYPE_BSS)->getStr());
    }
}


void LinkerMgr::mergedShdrWithNoBitsType(MOD ELFMgr * elf_mgr,
    ELFSHdr const* shdr, UINT shdr_idx, Sym const* shdr_subname)
{
    ASSERT0(elf_mgr && shdr && shdr_subname);

    switch (m_output_elf_mgr->getSectionType(shdr_subname)) {
    case SH_TYPE_BSS:
        //Merge .bss or .bss.xxx section. These sections need to be assigned 0.
        //'true' represents the shdr is BSS section.
        mergeShdrImpl(shdr, shdr_subname, true);
        //For SymbolInfo with SHN_COMMON attribute. It needs to be allocated
        //memory space in BSS section.
        mergeSymbolInfoWithCOMAttr(elf_mgr, shdr_idx, shdr_subname);
        break;
    default:
        UNREACHABLE();
        break;
    }
}


void LinkerMgr::updateRelaOfst(MOD ELFMgr * elf_mgr,
    ELFSHdr const* shdr, CHAR const* shdr_name, UINT sh_idx)
{
    ASSERT0(elf_mgr && shdr && shdr_name);

    //Update RelocInfo offset after corresponded SymbolInfo merged into section.
    for (UINT i = 0; i < ELFMGR_reloc_vec(elf_mgr).get_elem_count(); i++) {
        RelocInfo * reloc_info = ELFMGR_reloc_vec(elf_mgr)[i];
        ASSERT0(reloc_info);

        //Find corresponded SymbolInfo according to 'shndx'.
        if (RELOCINFO_shdr_idx(reloc_info) != sh_idx) { continue; }
        //Process section name.
        Sym const* shdr_subname = elf_mgr->getShdrType(shdr_name);
        ASSERT0(shdr_subname);

        SECTION_TYPE sect_type = elf_mgr->getSectionType(shdr_subname);
        if (!m_output_elf_mgr->hasSection(shdr_subname) &&
            m_output_elf_mgr->isDebugSection(sect_type)) {
            //TODO: Wait other part.
            ASSERT0(0);
        }

        //Update offset info.
        UINT base_ofst =
            (UINT)(m_output_elf_mgr->getSectionSize(shdr_subname) -
                   shdr->s_size);
        RELOCINFO_called_loc(reloc_info) += base_ofst;
    }
}


void LinkerMgr::updateSymbolOfst(
    MOD ELFMgr * elf_mgr, ELFSHdr const* shdr, UINT sh_idx)
{
    //TODO: Use Map to decrease found time.
    ASSERT0(elf_mgr && shdr);

    for (UINT i = 0; i < ELFMGR_symbol_vec(elf_mgr).get_elem_count(); i++) {
        SymbolInfo * symbol_info = ELFMGR_symbol_vec(elf_mgr)[i];
        ASSERT0(symbol_info);

        if ((elf_mgr->isNullSymbol(symbol_info, i)) ||
            (SYMINFO_sym(symbol_info).st_shndx != sh_idx) ||
            (SYMINFO_sect_type(symbol_info) == SH_TYPE_UNDEF) ||
            (SYMINFO_sect_name(symbol_info) == nullptr)) { continue; }

        //Note: Don't need to process.
        if (SYMINFO_sym(symbol_info).st_shndx == SHN_LORESERVE ||
            SYMINFO_sym(symbol_info).st_shndx == SHN_LOPROC ||
            SYMINFO_sym(symbol_info).st_shndx == SHN_HIPROC ||
            SYMINFO_sym(symbol_info).st_shndx == SHN_HIRESERVE) {
            UNREACHABLE();
        }

        SECTION_TYPE sect_type = elf_mgr->getSectionType(
            SYMINFO_sect_name(symbol_info));

        //TODO: Wait other part.
        if (!m_output_elf_mgr->hasSection(SYMINFO_sect_name(symbol_info)) &&
            (sect_type == SH_TYPE_COMMENT || sect_type == SH_TYPE_NOTE)) {
            continue;
        }

        //Update SymbolInfo offset.
        UINT base_ofst = m_output_elf_mgr->getSectionSize(
            SYMINFO_sect_name(symbol_info)) - (UINT)shdr->s_size;
        SYMINFO_ofst(symbol_info) =
            base_ofst + SYMINFO_sym(symbol_info).st_value;
        ASSERT0(SYMINFO_ofst(symbol_info) >= 0);

        if (!SYMINFO_is_func(symbol_info)) { continue; }
        //Process FunctionInfo offset.
        FunctionInfo * func = SYMINFO_func(symbol_info);
        ASSERT0(func);
        FUNCINFO_code_ofst(func) =
            base_ofst + SYMINFO_sym(symbol_info).st_value;
        ASSERT0(FUNCINFO_code_ofst(func) >= 0);
        ASSERT0(FUNCINFO_code_ofst(func) <= (base_ofst + shdr->s_size));
    }
}


void LinkerMgr::mergeCode(MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    FunctionInfo * fi = SYMINFO_func(symbol_info);
    ASSERT0(fi && (FUNCINFO_sect_type(fi) == SH_TYPE_TEXT ||
            FUNCINFO_sect_type(fi) == SH_TYPE_TEXT1));

    BYTEVec * content = m_output_elf_mgr->getSectContentWithGenIfNotExist(
        FUNCINFO_sect_name(fi));
    ASSERT0(content);

    //Resize content capacity.
    UINT base_ofst = (UINT)xcom::ceil_align(
        content->get_elem_count(), FUNCINFO_align(fi));
    UINT func_sz = (UINT)FUNCINFO_size(fi);

    if (func_sz == 0) { return; }

    UINT sz = base_ofst + func_sz;
    if (content->get_capacity() < sz) { content->grow(sz); }
    UCHAR * code = FUNCINFO_code(fi).get_vec();
    ASSERT0(code);

    //Copy code to section content.
    content->set((VecIdx)(sz - 1), 0);
    ::memcpy((void*)(content->get_vec() + base_ofst), (void*)code, func_sz);
    //Update info.
    FUNCINFO_code_ofst(fi) = base_ofst;
    SYMINFO_ofst(symbol_info) = base_ofst;
}


void LinkerMgr::mergeData(MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    switch (SYMINFO_sect_type(symbol_info)) {
    case SH_TYPE_BSS:
        m_output_elf_mgr->mergeBssData(symbol_info);
        break;
    case SH_TYPE_DATA:
    case SH_TYPE_RODATA:
    case SH_TYPE_DL_TDATA:
        m_output_elf_mgr->mergeUnullData(symbol_info);
        break;
    case SH_TYPE_EMPTY:
        //For sbss/bss symbol with extern attribute.
        break;
    default:
        UNREACHABLE();
        break;
    }
}


void LinkerMgr::mergeELFMgrCollectedFromVar()
{
    //Record code and data info in different ELFMgr into the corresponded
    //section of output ELFMgr. The function mainly process ELFMgr that
    //collected from xoc::Var.
    for (ELFMgr * elf_mgr = m_elf_mgr_list.get_head();
         elf_mgr != nullptr; elf_mgr = m_elf_mgr_list.get_next()) {
        if (elf_mgr->isStandardELFFormat()) { continue; }
        //Process SymbolInfo that collected from xoc::Var.
        for (UINT j = 0; j < ELFMGR_symbol_vec(elf_mgr).get_elem_count(); j++) {
            SymbolInfo * symbol_info = ELFMGR_symbol_vec(elf_mgr)[j];
            ASSERT0(symbol_info);

            if (elf_mgr->isNullSymbol(symbol_info, j)) { continue; }
            //Merge code.
            if (SYMINFO_is_func(symbol_info) &&
                !SYMINFO_is_extern(symbol_info)) {
                mergeCode(symbol_info);
                continue;
            }
            //Process debug info.
            if (SYMINFO_is_debug(symbol_info)) {
                processDebugInfo(elf_mgr, symbol_info);
                continue;
            }
            //Process data.
            if (SYMINFO_sym(symbol_info).st_type == STT_OBJECT) {
                mergeData(symbol_info);
                continue;
            }
        }

        //Update RelocInfo offset after code and data info merged.
        for (UINT j = 0; j < ELFMGR_reloc_vec(elf_mgr).get_elem_count(); j++) {
            updateRelaOfst((ELFMGR_reloc_vec(elf_mgr)[j]));
        }
    }

    //Post process after ELFMgr merged.
    postMergeELFMgrCollectedFromVar();
}


void LinkerMgr::processDebugInfo(MOD ELFMgr * elf_mgr,
                                 MOD SymbolInfo * symbol_info)
{
    //Record debug infomation into the corresponded section of output ELFMgr.
    //These debug infomation mainly come from ELFMgr collected from xoc::Var.
    ASSERT0(elf_mgr && symbol_info);

    xoc::MCDwarfMgr * dm = elf_mgr->getDwarfMgr();
    ASSERT0(dm);

    SECTION_TYPE sect_type = SYMINFO_sect_type(symbol_info);

    BYTEVec * content =
        m_output_elf_mgr->getSectContentWithGenIfNotExist(sect_type);
    ASSERT0(content);

    //Get debug content according to different debug type.
    BYTEVec * debug_content = nullptr;
    switch (sect_type) {
    case SH_TYPE_DEBUG_INFO:
        debug_content = &(MCDWARFMGR_debug_info_code(dm));
        break;
    case SH_TYPE_DEBUG_LINE:
        debug_content = &(MCDWARFMGR_debug_line_code(dm));
        break;
    case SH_TYPE_DEBUG_ABBREV:
        debug_content = &(MCDWARFMGR_debug_abbrev_code(dm));
        break;
    case SH_TYPE_DEBUG_RANGES:
        debug_content = &(MCDWARFMGR_debug_ranges_code(dm));
        break;
    case SH_TYPE_DEBUG_FRAME:
        debug_content = &(MCDWARFMGR_debug_frame_code(dm));
        break;
    case SH_TYPE_DEBUG_STR:
        debug_content = &(MCDWARFMGR_debug_str_code(dm));
        break;
    case SH_TYPE_DEBUG_LOC:
        debug_content = &(MCDWARFMGR_debug_loc_code(dm));
        break;
    default:
        UNREACHABLE();
        break;
    }
    ASSERT0(debug_content);

    //Copy debug info to the corresponded section of output ELFMgr.
    UINT base_ofst = (UINT)xcom::ceil_align(
        content->get_elem_count(), SYMINFO_align(symbol_info));
    UINT debug_size = debug_content->get_elem_count();
    UINT total_size = base_ofst + debug_size;
    if (content->get_capacity() < total_size) { content->grow(total_size); }

    //Copy section content.
    if (debug_size != 0) {
        content->set((VecIdx)(total_size - 1), 0);
        ::memcpy(content->get_vec() + base_ofst,
                 debug_content->get_vec(), debug_size);
    }

    //Update info.
    SYMINFO_ofst(symbol_info) = base_ofst;
    SectionInfo * sect_info = m_output_elf_mgr->getSection(sect_type);
    ASSERT0(sect_info);
    SECTINFO_align(sect_info) =
        MAX(SECTINFO_align(sect_info), SYMINFO_align(symbol_info));
}


void LinkerMgr::updateRelaOfst(MOD RelocInfo * reloc_info)
{
    ASSERT0(reloc_info);

    if (RELOCINFO_is_func(reloc_info)) {
        RELOCINFO_called_loc(reloc_info) +=
            FUNCINFO_code_ofst(RELOCINFO_caller_func(reloc_info));
    }
}


void LinkerMgr::mergeELFMgrHelper()
{
    //Process ELFMgr that collected from xoc::Var.
    mergeELFMgrCollectedFromVar();

    //Process ELFMgr that collected from object file.
    mergeELFMgr();
}


void LinkerMgr::mergeELFMgr()
{
    //Record code and data info from different ELFMgr into the corresponded
    //section of output ELFMgr. The function mainly process ELFMgr that
    //collected from object file.
    for (ELFMgr * elf_mgr = m_elf_mgr_list.get_head();
         elf_mgr != nullptr; elf_mgr = m_elf_mgr_list.get_next()) {
        if (!elf_mgr->isStandardELFFormat()) { continue; }
        ELFHdr & hdr = elf_mgr->getHdr();
        ELFSHdr * shstrtab_shdr = elf_mgr->getSectHeader(hdr.e_shstrndx);
        ASSERT0(shstrtab_shdr);

        for (UINT j = 0; j < hdr.e_shnum; j++) {
            ELFSHdr * shdr = elf_mgr->getSectHeader(j);
            ASSERT0(shdr);
            CHAR const* shdr_name =
                elf_mgr->getStrFromStrTab(shstrtab_shdr, shdr->s_name);
            ASSERT0(shdr_name);
            //Process different shdr.
            mergeELFMgrImpl(elf_mgr, shdr, shdr_name, j);
            //Update SymbolInfo offset after section merged.
            updateSymbolOfst(elf_mgr, shdr, j);
            //Update RelocInfo offset after data and code info merged.
            updateRelaOfst(elf_mgr, shdr, shdr_name, j);
        }
    }
}


void LinkerMgr::mergeELFMgrImpl(MOD ELFMgr * elf_mgr,
    ELFSHdr const* shdr, CHAR const* shdr_name, UINT shdr_idx)
{
    ASSERT0(elf_mgr && shdr && shdr_name);

    //Process shdr according to it's type.
    switch (shdr->s_type) {
    case S_UNDEF:
        ASSERT0(shdr_idx == 0);
        break;
    case S_PROGBITS:
        mergedShdrWithProgBitsType(shdr, elf_mgr->getShdrType(shdr_name));
        break;
    case S_NOBITS:
        mergedShdrWithNoBitsType(elf_mgr, shdr,
            shdr_idx, elf_mgr->getShdrType(shdr_name));
        break;
    case S_RELA:
    case S_STRTAB:
    case S_SYMTAB:
    case S_DYNSYM:
    case S_DYNAMIC:
    SWITCH_CASE_NOT_HANDLE:
        if (g_elf_opt.isDumpLink()) {
            xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
                "NOTE: The shdr don't be handled %ld %s.\n",
                shdr->s_type, shdr_name);
        }
        break;
    default:
        UNREACHABLE();
        break;
    }
}


void LinkerMgr::processOutputExeELF()
{
    //Merge section in different ELFMgr.
    mergeELFMgrHelper();

    //Create fundamental section.
    m_output_elf_mgr->processSectionInfo();

    //Set order for each section.
    m_output_elf_mgr->setSectionOrder();

    //Set ELF shdr_num.
    //There isn't .text.xxx section when generated fatbin ELF.
    m_output_elf_mgr->setSubTextNum(0);
    m_output_elf_mgr->setShdrNum(m_output_elf_mgr->getShdrNum());

    //Construct ELF header.
    m_output_elf_mgr->constructELFHeader(m_output_elf_mgr->getShdrNum());

    //Pre-construct .dynamic section.
    m_output_elf_mgr->preProcessDynamicSection();

    //Set ELF type.
    m_output_elf_mgr->setELFType(ET_DYN);

    //Generate rela_dyn section content.
    preProcessRelaDynSectBeforeSetSectAddr(m_output_elf_mgr);

    //Generate GOT section content.
    m_output_elf_mgr->genGotContent();

    //Generate section content.
    m_output_elf_mgr->genSectionContentHelper();

    //Process secton offset.
    m_output_elf_mgr->processSectionOffset();

    //Process section address.
    m_output_elf_mgr->processSectionAddr();

    //Process rela_dyn section after section address has been set.
    m_output_elf_mgr->processRelaDynSectAfterSetSectAddr();

    //Process element offset in '.dynsym' and '.symtab'.
    m_output_elf_mgr->postProcessAfterSetSectAddr();

    //Process .dynamic section.
    m_output_elf_mgr->processDynamicSection();

    //Process program header(segment).
    m_output_elf_mgr->processProgramHeader();

    //Relocate.
    doRelocate(m_output_elf_mgr);

    //Post process after all section info have been set.
    m_output_elf_mgr->postProcessAfterSettingSectionInfo();

    //Construct section and set header info of output ELF.
    m_output_elf_mgr->constructELFSectionHelper();

    //Write data into ELF file.
    m_output_elf_mgr->writeELF(m_output_file_name);
}


void LinkerMgr::dumpLibPathList(xcom::List<CHAR const*> * lib_path_list) const
{
    ASSERT0(lib_path_list);

    xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
        "\n\n======== AR File ======== \n\n");
    for (CHAR const* str = lib_path_list->get_head();
         str != nullptr; str = lib_path_list->get_next()) {
        xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
            "LibPath: %s \n", str);
    }
}


void LinkerMgr::dumpLinkResolve() const
{
    xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
        "\n\n======== UnResolved Symbol ========\n\n");
    for (UINT i = 0; i < m_reloc_symbol_vec.get_elem_count(); i++) {
        RelocInfo * reloc_info = m_reloc_symbol_vec[i];
        ASSERT0(reloc_info);
        if (RELOCINFO_is_resolved(reloc_info)) { continue; }
        xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
            "Error: Unresolved symbol idx: %d name: %s\n",
            i, RELOCINFO_name(reloc_info)->getStr());
    }
    xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
        "\n\n======== UnResolved Symbol PRT Done ========\n\n");
}


void LinkerMgr::dumpLinkRelocate(RelocInfo const* reloc_info, UINT index) const
{
    ASSERT0(reloc_info);

    //Dump RelocInfo.
    xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
        "\n IDX:%d \n \
        Resolved:%s  NAME:%s \n \
        IS_OBJ:%s    IS_FUNC:%s    IS_NOTYPE:%s    Relate_reloc:%d \n \
        LOC:0x%lx    TYPE:%d       ADDEND:%d\n",
        index, (RELOCINFO_is_resolved(reloc_info) ? "TRUE" : "FALSE"),
        RELOCINFO_name(reloc_info),
        (RELOCINFO_is_object(reloc_info) ? "TRUE" : "FALSE"),
        (RELOCINFO_is_func(reloc_info) ? "TRUE" : "FALSE"),
        (RELOCINFO_resolved_notype(reloc_info) ? "TRUE" : "FALSE"),
        RELOCINFO_next(reloc_info),
        RELOCINFO_called_loc(reloc_info), RELOCINFO_type(reloc_info),
        RELOCINFO_addend(reloc_info));
    //Dump resolved SymbolInfo of RelocInfo.
    xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
        "\nResolvedSymbolInfo:%s    IS_FUNC:%d \n \
        st_type:%d    st_size:%d    st_bind:%d    st_shndx:%d\n",
        SYMINFO_name(RELOCINFO_sym(reloc_info))->getStr(),
        SYMINFO_is_func(RELOCINFO_sym(reloc_info)),
        SYMINFO_sym(RELOCINFO_sym(reloc_info)).st_type,
        SYMINFO_sym(RELOCINFO_sym(reloc_info)).st_size,
        SYMINFO_sym(RELOCINFO_sym(reloc_info)).st_bind,
        SYMINFO_sym(RELOCINFO_sym(reloc_info)).st_shndx);
    //Dump caller FunctionInfo of RelocInfo.
    if (!RELOCINFO_caller_func(reloc_info)) { return; }
    xoc::note(m_dump->getFileHandler(), m_logmgr.getIndent(),
        "\nCallerFuncInfo:%s      IS_ENTRY:%d \n \
        SECT:%d    SECT_STR:%s    SIZE:%ld    OFST:0x%lx    ALIGN:%d\n",
        FUNCINFO_name(RELOCINFO_caller_func(reloc_info))->getStr(),
        FUNCINFO_is_entry(RELOCINFO_caller_func(reloc_info)),
        FUNCINFO_sect_type(RELOCINFO_caller_func(reloc_info)),
        FUNCINFO_sect_name(RELOCINFO_caller_func(reloc_info))->getStr(),
        FUNCINFO_size(RELOCINFO_caller_func(reloc_info)),
        FUNCINFO_code_ofst(RELOCINFO_caller_func(reloc_info)),
            FUNCINFO_align(RELOCINFO_caller_func(reloc_info)));
}

} //namespace elf
