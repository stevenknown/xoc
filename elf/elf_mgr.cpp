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
// -elf-fatbin option: Output fatbin file that directy execute on device.
//  It will call ld linked multi-file and other external .so file.
ELFOpt g_elf_opt;

typedef struct {
    CHAR const* colname; //the name of the column.
    CHAR const* colfmt; //the print-format of value to the column.
    UINT colwidth; //the maximum print byte width to the column.
} TabCol;


static SectionNameDesc const g_section_name_desc[] = {
    //Name(enum), Sect type,  Program header,
    //Flags,      Addr align, Entry size,     Name(str)
    { SH_TYPE_UNDEF, S_UNDEF, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, "" },

    { SH_TYPE_SHSTR, S_STRTAB,  PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".shdr_strtab" },

    { SH_TYPE_SYMSTR, S_STRTAB, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, SHDR_SYM_BYTE, ".strtab" },

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
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".symtab" },

    { SH_TYPE_SUBTEXT, S_UNDEF, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".text." },

    { SH_TYPE_RELA, S_UNDEF, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".rela.text." },

    { SH_TYPE_CONST, S_PROGBITS, PH_TYPE_DATA,
      SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".const" },

    { SH_TYPE_GOT, S_PROGBITS, PH_TYPE_DATA,
      SF_WRITE|SF_ALLOC, SHDR_GOT_ALIGN, ELF_VAL_UNDEF, ".got" },

    { SH_TYPE_RELA_DYN, S_RELA, PH_TYPE_CODE,
      SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".rela.dyn" },

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

    { SH_TYPE_EH_FRAME, S_PROGBITS, PH_TYPE_DATA,
      SF_ALLOC, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".eh_frame" },

    { SH_TYPE_COMMENT, S_PROGBITS, PH_TYPE_UNDEF,
      SF_MERGE|SF_STRINGS, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".comment" },

    { SH_TYPE_NOTE, S_PROGBITS, PH_TYPE_UNDEF,
      ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ".note" },
 };

static UINT const g_section_name_num =
    sizeof(g_section_name_desc) / sizeof(g_section_name_desc[0]);


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

    m_sbss_align = 0;
    m_sdata_align = 0;
    m_bss_align = 0;
    m_data_align = 0;
    m_spm_align = 0;
    m_const_align = 0;

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
    clean();
    m_pool = smpoolCreate(64, MEM_COMM);
    m_sect_info = new ELFSectionInfo();
    ELFMgr::initSectionDescInfo();
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
    size_t actual_rd = 0;
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
        if (shdr->s_type == S_NOBITS) { continue; }
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
    hdr.e_machine = EM_SWAI_64;
    hdr.e_version = ELF_VERSION;
    hdr.e_entry = ELF_VAL_UNDEF;
    hdr.e_phoff = ELF_VAL_UNDEF;
    hdr.e_shoff = ELFSHdr::getSize(this);
    hdr.e_flags = ELF_VAL_UNDEF;
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
    symtab_shdr->s_info = (Word32)getSectHeaderIdx(text_shdr);
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
                                     CHAR const* name, MOD BYTE * text_space)
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
    SET_FLAG(func_shdr->s_flags, SF_ALLOC|SF_EXECINSTR);
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


void ELFMgr::processELFTextRelSection(ELFSHdr const* symtab_shdr,
                                      OUT UINT & si)
{
    xcom::StrBuf buf0(32);
    xcom::StrBuf buf1(32);
    SymbolInfoIter iter;
    SymbolInfo * sym_info = nullptr;
    for (m_symbol_info.get_first(iter, &sym_info); !iter.end();
         m_symbol_info.get_next(iter, &sym_info)) {

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
        BYTE * rel_space = (BYTE*)(xmalloc(rel_content.get_elem_count()));

        //Construct .text.xxx and .rel.text.xxx section headers and sections.
        ELFSHdr * func_shdr = getSectHeader(si++);
        constructELFFuncSection(func_shdr, text_content, text_name_space,
                                text_space);

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
    default: break; //Variable is not available.;
    }
}


void ELFMgr::genRelocSdataAndDataContent(OUT BYTEVec & sdata_content,
                                         OUT BYTEVec & data_content)
{
    SymbolInfoIter iter;
    SymbolInfo * sym_info = nullptr;
    for (m_symbol_info.get_first(iter, &sym_info); !iter.end();
         m_symbol_info.get_next(iter, &sym_info)) {

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
                             xcom::Vector<RelocInfo*> & reloc_vec)
{
    UINT elem_num = reloc_vec.get_elem_count();
    ASSERT0(elem_num != 0);

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
                              RELOCINFO_called_loc(reloc_info));
        AssembleBinDesc d_type(ELFRela::getTypeSize(this),
                               RELOCINFO_type(reloc_info));
        AssembleBinDesc d_sym(ELFRela::getSymbolSize(this),
            SYMINFO_index(m_symbol_info.get(RELOCINFO_name(reloc_info))));
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

    SymbolInfoIter iter;
    SymbolInfo * sym_info = nullptr;
    for (m_symbol_info.get_first(iter, &sym_info); !iter.end();
         m_symbol_info.get_next(iter, &sym_info)) {

        ASSERT0(sym_info);

        //Count function number.
        //Note that there are functions with the same name declared and defined
        //in a source file. Here only the defined functions are written into
        //the symbol table.
        func_num += SYMINFO_is_func(sym_info) &
            (SYMINFO_func(sym_info) != nullptr &&
            sym_info->getSymbolCode().get_elem_count() != 0);

        SYMINFO_index(sym_info) = ++sym_ind;

        //Collector names(string) of all symbols to generate .strtab content.
        sym_name.append_tail(SYMINFO_name(sym_info)->getStr());

        //Set section info based all symbols.
        setSectionInfo(sym_info);
    }

    m_symbol_off.text_ind = BASE_SEC_NUM + m_sect_info->hasSbss() +
        m_sect_info->hasSdata() + m_sect_info->hasBss() +
        m_sect_info->hasData() + m_sect_info->hasSpm() +
        m_sect_info->hasConst();

    //Note: "func_num * 2" : A device func will generate
    //two sections like .text.xxx and .rel.text.xxx.
    m_sect_info->setShdrNum(m_symbol_off.text_ind + (func_num * 2) +
        m_sect_info->hasRelaData() + m_sect_info->hasRelaSdata());
}


void ELFMgr::initSymFunc(xoc::Var const* var)
{
    ASSERT0(var && var->get_name() &&
            !m_symbol_info.find(var->get_name()));

    SymbolInfo * sym_info = m_sym_mgr.allocSymbolInfo();
    ASSERT0(sym_info);
    FunctionInfo * func_info = m_func_mgr.allocFunctionInfo();
    ASSERT0(func_info);

    //Set symbol name.
    SYMINFO_name(sym_info) = var->get_name();
    SYMINFO_func(sym_info) = func_info;
    SYMINFO_func_name(sym_info) = var->get_name();
    FUNCINFO_name(SYMINFO_func(sym_info)) = var->get_name();
    //Set whether symbol is function.
    SYMINFO_is_func(sym_info) = true;
    //Set whether symbol is a entry function.
    FUNCINFO_is_entry(SYMINFO_func(sym_info)) = var->is_entry();
    //Set whether symbol is global.
    SYMINFO_is_global(sym_info) = true;
    //Set whether symbol is visible.
    SYMINFO_is_visible(sym_info) = var->is_visible();
    //Set symbol section.
    SYMINFO_sect_type(sym_info) = judgeSymbolSection(var);
    m_symbol_info.set(var->get_name(), sym_info);
}


void ELFMgr::extractSymbolExceptUserDefFunc()
{
    VarVec const* var_vec = getRegionMgr()->getVarMgr()->getVarVec();
    ASSERT0(var_vec);

    for (UINT i = 0; i < var_vec->get_elem_count(); i++) {
        xoc::Var * var = var_vec->get(i);

        //Since user-defined functions have been saved in initSymFunc(), we
        //need to extract available variables except them.
        if (!isVarAvailable(var)) { continue; }

        bool find = false;
        SymbolInfo * sym_info = m_symbol_info.getAndGen(var->get_name(), &find);
        ASSERT0(sym_info);
        if (find && var->is_func()) { continue; }

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

    symtab_content.grow(m_symbol_info.get_elem_count() * elf_sym_size);
    symtab_content.set(symtab_content.get_capacity() + elf_sym_size - 1, 0);
    ASSERT0(symtab_content.get_vec());

    constructSymbolNull(symtab_content);

    UINT ind = elf_sym_size;

    BYTEVec space(elf_sym_size);
    ASSERT0(space.get_vec());

    SymbolInfoIter iter;
    SymbolInfo * sym_info = nullptr;
    for (m_symbol_info.get_first(iter, &sym_info); !iter.end();
         m_symbol_info.get_next(iter, &sym_info)) {

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
            setSymbolValue(&sym, name, STB_GLOBAL, ELF_VAL_UNDEF,
                           ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF,
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
            SYMINFO_ofst(sym_info) = m_symbol_off.data_off;
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
            m_symbol_off.const_off = (UINT)(off + size);
            break;
        }
        case SH_TYPE_TEXT: {
            setSymbolValue(&sym, name, bind, SYMBOL_FUNC,
                           getSymOtherInfo() | other, m_symbol_off.text_ind,
                           ELF_VAL_UNDEF, size);
            //.text.xxx and .rel.text.xxx section headers are always together.
            m_symbol_off.text_ind += 2;
            m_symbol_off.func_off++;
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

    RELOCINFO_name(reloc_info) = other;
    RELOCINFO_type(reloc_info) = type;
    RELOCINFO_called_loc(reloc_info) = offset;
    RELOCINFO_addend(reloc_info) = addend;

    SYMINFO_reloc(symbol_info).append(reloc_info);
}


xcom::CHAR const* ELFMgr::getSectionName(SECTION_TYPE sect_type) const
{
    SectionNameDesc const* desc;
    SectNameDescIter iter;
    for (m_sect_desc_info.get_first(iter, &desc); !iter.end();
         m_sect_desc_info.get_next(iter, &desc)) {
        if (sect_type == SECTDESC_name(desc)) {
            return SECTDESC_name_str(desc);
        }
    }
    ASSERTN(0, ("ERROR: get section name failed!\n"));
    return nullptr;
}


SECTION_TYPE ELFMgr::getSectionType(CHAR const* sect_name) const
{
    ASSERT0(sect_name && m_sect_desc_info.find(sect_name));
    SectionNameDesc const* desc = m_sect_desc_info.get(sect_name);
    ASSERT0(desc);
    return SECTDESC_name(desc);
}


SECTION_TYPE ELFMgr::getSectionTypeWithSplit(xcom::CHAR const* sect_name)
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
    UINT num = xcom::xsplit(sect_name, ".", substr_vec);
    ASSERT0(num >= 2);
    //2.Assemble normal section name. 2 represents the length of '.' and '\0'.
    CHAR * name = (CHAR*)xmalloc(
        ::strlen(substr_vec.getStrBuf(1)->getBuf()) + 2);
    ASSERT0(name);
    //The format of name is ".xxx\0".
    ::sprintf(name, ".%s%c", substr_vec.getStrBuf(1)->getBuf(), '\0');
    //3.Get section type according to 'name'.
    if (m_sect_desc_info.find(name)) {
        SectionNameDesc const* desc = m_sect_desc_info.get(name);
        ASSERT0(desc);
        return SECTDESC_name(desc);
    }
    return SH_TYPE_UNDEF;
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
    BYTEVec tmp(sz);
    xcom::AssembleBinBuf as(&(tmp[0]), sz, abdv);
    content->grow(sz);
    content->set(content->get_capacity() - 1, 0);
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


SectionNameDesc ELFMgr::getSectionDesc(SECTION_TYPE sect_type) const
{
    SectionNameDesc const* desc = getSectionNameDesc();
    for (UINT i = 0; i < getSectionNum(); i++) {
        if (sect_type == SECTDESC_name(&desc[i])) { return desc[i]; }
    }
    UNREACHABLE();
    return desc[0];
}


SectionInfo * ELFMgr::getSection(CHAR const* sect_name) const
{
    ASSERT0(m_sect_map.find(sect_name));
    return m_sect_map.get(sect_name);
}


BYTEVec * ELFMgr::getSectionContent(CHAR const* sect_name) const
{
    ASSERT0(m_sect_map.find(sect_name));
    return SECTINFO_bytevec(m_sect_map.get(sect_name));
}


CHARVec * ELFMgr::getSectionCharVec(CHAR const* sect_name) const
{
    ASSERT0(m_sect_map.find(sect_name));
    return SECTINFO_charvec(m_sect_map.get(sect_name));
}


Addr ELFMgr::readSectionContent(CHAR const* sect_name, Addr addr) const
{
    BYTEVec * bytevec = getSectionContent(sect_name);
    ASSERT0(bytevec);
    return bytevec->get((VecIdx)addr);
}


void ELFMgr::writeSectionContent(CHAR const* sect_name, Addr addr, Addr value)
{
    ASSERT0(sect_name);
    BYTEVec * bytevec = getSectionContent(sect_name);
    ASSERT0(bytevec);
    bytevec->set((VecIdx)addr, (BYTE)value);
}


Addr ELFMgr::getSectionAddr(CHAR const* sect_name) const
{
    ASSERT0(m_sect_map.find(sect_name));
    return SECTINFO_addr(m_sect_map.get(sect_name));
}


UINT ELFMgr::getSectionSize(CHAR const* sect_name) const
{
    ASSERT0(sect_name && m_sect_map.find(sect_name));
    if ((sect_name == getSectionName(SH_TYPE_SHSTR)) ||
        (sect_name == getSectionName(SH_TYPE_SYMSTR))) {
        return getSectionCharVec(sect_name)->get_elem_count();
    }
    return getSectionContent(sect_name)->get_elem_count();
}


SectionInfo * ELFMgr::getSectionWithGenIfNotExist(CHAR const* sect_name)
{
    ASSERT0(sect_name);

    bool find = false;
    SectionInfo * si = m_sect_map.getAndGen(sect_name);
    ASSERT0(si);

    if (find) { return si; }
    setSectionImple(si, getSectionTypeWithSplit(sect_name));
    return si;
}


BYTEVec * ELFMgr::getSectContentWithGenIfNotExist(CHAR const* sect_name)
{
    ASSERT0(sect_name);

    bool find = false;
    SectionInfo * si = m_sect_map.getAndGen(sect_name);
    ASSERT0(si);
    if (find) { return SECTINFO_bytevec(si); }
    setSectionImple(si, getSectionTypeWithSplit(sect_name));
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


CHAR const* ELFMgr::getShdrType(CHAR const* shdr_name)
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
        ASSERT0(haveSection(shdr_name));
        return shdr_name;
    case 3:
    case 4:
    case 5: {
        //'1' represents the index of substr after splited.
        CHAR const* name = getSubStrWithDelimViaIdxAfterSplited(
            shdr_name, ".", FIRST_INDEX_OF_SUBSTR);
        ASSERT0(haveSection(name));
        return name;
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


SymbolInfo * ELFMgr::getSymbolInfo(CHAR const* symbol_name)
{
    ASSERT0(symbol_name);
    ASSERTN(m_symbol_info_map.find(symbol_name),
            ("\nError: No symbol %s\n", symbol_name));
    return m_symbol_info_map.get(symbol_name);
}


void ELFMgr::setELFType(UINT type)
{
    ELFHdr & hdr = getHdr();
    hdr.e_type = type;
}


SectionNameDesc const* ELFMgr::getSectionNameDesc() const
{
    return g_section_name_desc;
}


SECTION_TYPE const* ELFMgr::getDynamicSectionDesc() const
{
    return g_dynamic_section_desc;
}


void ELFMgr::initSectionDescInfo()
{
    SectionNameDesc const* desc = ELFMgr::getSectionNameDesc();
    ASSERT0(desc);

    for (UINT i = 0; i < g_section_name_num; i++) {
        m_sect_desc_info.set(SECTDESC_name_str(&desc[i]), &desc[i]);
    }
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


void ELFMgr::collectSymbolInfo(Var const* var,
    MOD SymbolInfo * symbol_info, MOD UINT & func_ind)
{
    //TODO: Wait other part.
}


void ELFMgr::collectSymtabInfoFromVar()
{
    //TODO: Wait other part.
}


void ELFMgr::genRelocInfoFromVarRelocation(MOD SymbolInfo * symbol_info)
{
    //TODO: Wait other part.
}


void ELFMgr::collectFunctionInfoFromVar()
{
    //TODO: Wait other part.
}


void ELFMgr::collectFunctionInfo(OUT BYTEVec & bytevec, UINT ind)
{
    //TODO: Wait other part.
}


void ELFMgr::collectSymbolData(Var const* var, OUT Word & size,
    OUT BYTE ** byte, OUT Word & bin_word, OUT bool & is_byte)
{
    //TODO: Wait other part.
}


void ELFMgr::collectELFInfoFromVar()
{
    extractSymbolExceptUserDefFunc();
    //Collected symtab info.
    collectSymtabInfoFromVar();
    //Collected function info(incldue func code, func relocation).
    collectFunctionInfoFromVar();
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
    SYMINFO_file_name(target_symbol_info) = m_file_name;

    //FIXME: Not all symbols recorded into vector/map ?
    //Check symbol info.
    if (m_symbol_info_map.find(SYMINFO_name_str(target_symbol_info))) {
        //'target_symbol_info' is extern symbol and defined symbol have
        //been recorded. Thus 'target_symbol_info' don't need to record.
        if (SYMINFO_sym(target_symbol_info).st_shndx == 0 ||
            SYMINFO_is_extern(target_symbol_info)) { return; }
        SymbolInfo * symbol_info =
            m_symbol_info_map.get(SYMINFO_name_str(target_symbol_info));
        ASSERT0(symbol_info);

        if (SYMINFO_sym(symbol_info).st_shndx == 0 ||
            SYMINFO_is_extern(symbol_info)) {
            //'symbol_info' is extern symbol. Remove it and re-record
            //'target_symbol_info' into vector/map.
            m_symbol_info_map.remove(SYMINFO_name_str(symbol_info));
        } else {
            prt2C("\nerror:multi definition of '%s'\n",
                  SYMINFO_name_str(target_symbol_info));
            UNREACHABLE();
        }
    }

    ASSERT0(!m_symbol_info_map.find(SYMINFO_name_str(target_symbol_info)));
    //Record symbol info into vector/map.
    m_symbol_info_map.set(
        SYMINFO_name_str(target_symbol_info), target_symbol_info);
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
            ph->p_filesz = getSectionSize(SECTINFO_type(sect_info));
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


bool ELFMgr::haveBeenRecordedRelaDynInfoItem(
    RelocInfo const* reloc_info, OUT RelocInfo ** out_reloc_info)
{
    ASSERT0(reloc_info);

    for (UINT i = 0; i < m_reladyn_info_vec.get_elem_count(); i++) {
        RelaDynInfo * reladyn = m_reladyn_info_vec[i];
        ASSERT0(reladyn);

        if (RELADYNINFO_sym_name(reladyn) ==
            SYMINFO_name(RELOCINFO_sym(reloc_info))) { continue; }

        if (RELADYNINFO_is_got(reladyn)) {
            *out_reloc_info = RELADYNINFO_reloc_info(reladyn);
            return true;
        }

        if ((RELADYNINFO_type(reladyn) == RELOCINFO_type(reloc_info)) &&
            (RELADYNINFO_addend(reladyn) == RELOCINFO_addend(reloc_info)) &&
            (RELADYNINFO_ofst(reladyn) == (RELOCINFO_called_loc(reloc_info))) &&
            ((RELADYNINFO_caller_sym_name(reladyn) ==
             (RELOCINFO_sect_name_sym(reloc_info))))) {
            *out_reloc_info = RELADYNINFO_reloc_info(reladyn);
            return true;
        }
    }
    return false;
}


void ELFMgr::setRelaDynInfo(MOD RelocInfo * reloc_info,
                            UINT & got_ofst, UINT & dynsym_idx)
{
    ASSERT0(reloc_info);

    RelaDynInfo * rela = m_reladyn_mgr.allocRelaDynInfo();
    ASSERT0(rela);

    RELADYNINFO_reloc_info(rela) = reloc_info;
    RELADYNINFO_is_got(rela) = haveGotItem(reloc_info);
    RELADYNINFO_is_dynsym(rela) =
        (!isSymbolWithLocalAttr(RELOCINFO_sym(reloc_info)));

    if (RELADYNINFO_is_got(rela)) {
        RELOCINFO_sect_ofst(reloc_info) = got_ofst;
        rela->m_got_ofst = got_ofst;
        got_ofst += getElemByteSizeInGotSect();
    }

    if (RELADYNINFO_is_dynsym(rela)) {
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
     desc_vec.set(ind * BYTE_2ND, d0);
     desc_vec.set(ind * BYTE_2ND + BYTE_1ST, d1);
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
    extractAssBinDescVec(getSectionContent(SH_TYPE_DYNAMIC), dynamic_desc_vec);
    //Refill dynamic base addr to the first item of got section.
    refill32ByteContent(getSectionName(SH_TYPE_GOT), 0,
        getSectionAddr(SH_TYPE_DYNAMIC));
}


void ELFMgr::refill32ByteContent(CHAR const* sect_name, Addr addr, Addr value)
{
    ASSERT0(sect_name);
    writeSectionContent(sect_name, addr, GET_INST_1ST_BYTE(value));
    writeSectionContent(sect_name, addr + BYTE_1ST, GET_INST_2ND_BYTE(value));
    writeSectionContent(sect_name, addr + BYTE_2ND, GET_INST_3RD_BYTE(value));
    writeSectionContent(sect_name, addr + BYTE_3RD, GET_INST_4TH_BYTE(value));
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
    for (UINT i = 0; i < bytevec->get_elem_count(); i += elem_size) {
        //Read origin value.
        Half index = *(Half*)(content + st_shndx_ofst + i);
        Addr value = *(Addr*)(content + st_value_ofst + i);
        //Get corresponded section begin addr of current symbol.
        if (index == SHN_ABS) { continue; }
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
    //Update element offset in '.dynsym'.
    updateSymOffset(SH_TYPE_DYNSYM);

    //Update element offset in '.symtab'.
    updateSymOffset(SH_TYPE_SYMTAB);
}


void ELFMgr::setSection(SECTION_TYPE sect_type)
{
    bool find = false;
    SectionInfo * si = m_sect_map.getAndGen(getSectionName(sect_type), &find);
    ASSERT0(si);
    if (find) { return; }

    setSectionImple(si, sect_type);
}


void ELFMgr::setSection(SECTION_TYPE sect_type, CHAR const* name, UINT sect_idx)
{
    ASSERT0(name);

    bool find = false;
    SectionInfo * si = m_sect_map.getAndGen(name, &find);
    ASSERT0(si);
    if (find) { return; }

    setSectionImple(si, sect_type);
    //Re-set info.
    SECTINFO_name_str(si) = name;
    SECTINFO_name_sym(si) = addToSymTab(SECTINFO_name_str(si));
    SECTINFO_index(si) = sect_idx;
}


void ELFMgr::setSectionImple(MOD SectionInfo * si, SECTION_TYPE sect_type)
{
    ASSERT0(si);

    SectionNameDesc const* sect_desc =
        m_sect_desc_info.get(getSectionName(sect_type));
    ASSERT0(sect_desc);

    SECTINFO_type(si) = sect_type;
    SECTINFO_name_str(si) = getSectionName(sect_type);
    SECTINFO_name_sym(si) = addToSymTab(SECTINFO_name_str(si));
    SECTINFO_ph_type(si) = SECTDESC_ph_type(sect_desc);
    SECTINFO_shdr_type(si) = SECTDESC_type(sect_desc);
    SECTINFO_flag(si) = setSectionFlags(sect_desc);
    SECTINFO_align(si) = SECTDESC_align(sect_desc);
    SECTINFO_entry_size(si) = SECTDESC_entry_sz(sect_desc);
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
        m_sect_layout.set(
            (VecIdx)(SECTINFO_index(sect_info)), SECTINFO_name_str(sect_info));
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
        SYMINFO_func_name_str(symbol_info) = getFunctionName(shdr_name);
        SYMINFO_sect_type(symbol_info) = getSectionTypeWithSplit(shdr_name);
        SYMINFO_sect_name_str(symbol_info) =
            getSectionName(SYMINFO_sect_type(symbol_info));
        return;
    }

    SYMINFO_sect_type(symbol_info) = getSectionTypeWithSplit(shdr_name);
    SYMINFO_sect_name_str(symbol_info) =
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


CHAR const* ELFMgr::getFunctionName(CHAR const* text_shdr_name)
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
    if (num <= 2) { return text_shdr_name; }

    //Number 3 means the length of 2 '.' and '\0'.
    UINT len = (UINT)::strlen(str_vec.getStrBuf(1)->getBuf()) + 3;
    CHAR * exclude_str = (CHAR*)xmalloc(len);
    ASSERT0(exclude_str);
    //'exclude_str' is '.text.' or '.text1.'.
    ::sprintf(exclude_str, ".%s.%c", str_vec.getStrBuf(1)->getBuf(), '\0');
    return getSubStr(text_shdr_name, exclude_str);
}


FunctionInfo * ELFMgr::collectTextInfo(ELFSHdr const* shdr,
    ELFSHdr const* strtab_shdr, SymbolInfo const* symbol_info)
{
    ASSERT0(shdr && strtab_shdr && symbol_info);

    //Collect section header name.
    CHAR const* shdr_name = getStrFromStrTab(strtab_shdr, shdr->s_name);
    ASSERT0(shdr_name);

    //Allocate FunctionInfo.
    FunctionInfo * fi = m_func_mgr.allocFunctionInfo(shdr->s_size);
    ASSERT0(fi);

    //Set function code.
    Addr ofst = SYMINFO_sym(symbol_info).st_value;
    Addr func_size = SYMINFO_sym(symbol_info).st_size;
    BYTE * code = FUNCINFO_code(fi).get_vec();
    ASSERT0(code);
    ::memcpy((void*)code, (void*)(shdr->s_content + ofst), func_size);

    //Set FunctionInfo.
    FUNCINFO_code_ofst(fi) = 0;
    FUNCINFO_size(fi) = func_size;
    FUNCINFO_file_name(fi) = m_file_name;
    FUNCINFO_align(fi) = (UINT)shdr->s_addr_align;
    FUNCINFO_name_str(fi) = getFunctionName(shdr_name);
    FUNCINFO_sect_type(fi) = getSectionTypeWithSplit(shdr_name);
    FUNCINFO_sect_name_str(fi) = getSectionName(FUNCINFO_sect_type(fi));
    FUNCINFO_is_entry(fi) = isEntryFunction(FUNCINFO_sect_type(fi));

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
    //Record the begin index of global symbol in '.symtab'.
    setGlobalSymbolBeginIndex((local_sz + sym_size) / sym_size);

    //Copy 'local_bytevec' and 'global_bytevec' content to 'bytevec'.
    if (bytevec->get_capacity() < total_sz) { bytevec->grow(total_sz); }
    bytevec->set(bytevec->get_capacity() - 1, 0);
    ::memcpy((void*)(bytevec->get_vec() + sym_size),
        (void*)(local_bytevec.get_vec()), local_sz);
    ::memcpy((void*)(bytevec->get_vec() + sym_size + local_sz),
        (void*)(global_bytevec.get_vec()), global_sz);
}


void ELFMgr::setELFSymToByteVec(MOD BYTEVec * sym_vec,
    MOD SymbolInfo * symbol_info, MOD BYTEVec * local_vec,
    MOD BYTEVec * global_vec, MOD BYTEVec * dynsym_vec,
    UINT local_idx, UINT global_idx)
{
    ASSERT0(symbol_info && sym_vec && local_vec && global_vec && dynsym_vec);

    UINT sz = ELFSym::getSize(this);

    //Record to dynsym bytevec if it is also '.dynsym' symbol.
    if (SYMINFO_is_dynsym(symbol_info)) {
        UINT order_idx = (UINT)SYMINFO_dynsym_idx(symbol_info) * sz;
        dynsym_vec->set(order_idx + sz - 1, 0);
        ::memcpy((void*)(dynsym_vec->get_vec() + order_idx),
            (void*)(sym_vec->get_vec()), sz);
    }

    //Record to local bytevec.
    if (isSymbolWithLocalAttr(symbol_info)) {
        local_vec->set(local_idx + sz - 1, 0);
        ::memcpy((void*)(local_vec->get_vec() + local_idx),
            (void*)(sym_vec->get_vec()), sz);
        return;
    }

    //Record to global bytevec.
    global_vec->set(global_idx + sz - 1, 0);
    ::memcpy((void*)(global_vec->get_vec() + global_idx),
        (void*)(sym_vec->get_vec()), sz);
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

        if (SECTINFO_type(sect_info) ==  SH_TYPE_SHSTR) {
            setSectHeaderNameStrTab(shdr);
        }
        //Process s_type. e.g.: S_PROGBITS;
        shdr->s_type = SECTINFO_shdr_type(sect_info);
        //Process s_flags. Value from config table(g_section_name_desc).
        SET_FLAG(shdr->s_flags, SECTINFO_flag(sect_info));
        //Process s_addr.
        shdr->s_addr = getSectionAddr(SECTINFO_name_str(sect_info));
        //Process s_offset.
        shdr->s_offset = SECTINFO_ofst(sect_info);
        //Process s_size.
        shdr->s_size = getSectionSize(SECTINFO_name_str(sect_info));
        //Process s_content.
        if ((SECTINFO_type(sect_info) == SH_TYPE_SYMSTR) ||
            (SECTINFO_type(sect_info) == SH_TYPE_SHSTR)) {
            //SH_TYPE_SYMSTR/SH_TYPE_SHSTR is char type.
            CHARVec * charvec = getSectionCharVec(SECTINFO_name_str(sect_info));
            ASSERT0(charvec);
            shdr->s_content = (BYTE*)charvec->get_vec();
        } else {
            BYTEVec * bytevec = getSectionContent(SECTINFO_name_str(sect_info));
            ASSERT0(bytevec);
            shdr->s_content = (BYTE*)bytevec->get_vec();
        }
        //Process s_link/s_info and s_entry_size.
        switch (SECTINFO_type(sect_info)) {
        case SH_TYPE_SYMTAB:
        case SH_TYPE_DYNSYM:
            shdr->s_link = (Word32)getSectionIndex(SH_TYPE_SYMSTR);
            //The begin index of gloabl symbol in .symtab section.
            shdr->s_info = getGlobalSymbolBeginIndex();
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
            (getSectionAlign(SECTINFO_name_str(sect_info)));
        //Process s_name_str.
        shdr->s_name_str = SECTINFO_name_str(sect_info);
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
            sz += ::strlen(SYMINFO_name_str(si)) + 1;
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
            size_t l = ::strlen(SYMINFO_name_str(si));
            charvec->set((VecIdx)(off + l - 1), 0);
            ::memcpy((void*)(charvec->get_vec() + off),
                (BYTE const*)(SYMINFO_name_str(si)), l);
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
        collectObjELFImple(hdr, getSectHeader(i), i);
    }
}


void ELFMgr::collectObjELFImple(ELFHdr & hdr,
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
        //TODO: To support.
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
        SYMINFO_name_str(symbol_info) = getStrFromSymTab(sym_shdr, i);
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
        RELOCINFO_name_str(reloc_info) =
            getRelaName(symtab_shdr, rela_sym.r_sym);
        RELOCINFO_called_loc(reloc_info) = rela_sym.r_offset;
        RELOCINFO_type(reloc_info) = (UINT)rela_sym.r_type;
        RELOCINFO_addend(reloc_info) = (UINT)rela_sym.r_addend;
        RELOCINFO_sect_ofst(reloc_info) = 0;
        RELOCINFO_sym_idx(reloc_info) = rela_sym.r_sym;
        RELOCINFO_shdr_idx(reloc_info) = rela_shdr->s_info;
        RELOCINFO_caller_func(reloc_info) = nullptr;
        RELOCINFO_sect_name_str(reloc_info) = getRelaShdrType(shdr_name);

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


void ELFMgr::processBssData(MOD SymbolInfo * symbol_info)
{
    ASSERT0(symbol_info);

    //There isn't data in SymbolInfo with BSS attribute. When the SymbolInfo is
    //merged into section, memory space in ELF will be allocated and assigned 0.
    BYTEVec * content =
        getSectContentWithGenIfNotExist(SYMINFO_sect_name_str(symbol_info));
    ASSERT0(content);
    SectionInfo * sect_info = getSection(SYMINFO_sect_name_str(symbol_info));
    ASSERT0(sect_info);

    //Allocate memory.
    UINT base_ofst = content->get_elem_count();
    base_ofst = (UINT)xcom::ceil_align(base_ofst, SYMINFO_align(symbol_info));
    UINT sz = (UINT)(base_ofst + SYMINFO_size(symbol_info));
    if (content->get_capacity() < sz) { content->grow(sz); }

    //Assign 0.
    content->set(content->get_capacity() - 1, 0);
    ::memset(content->get_vec() + base_ofst, 0, SYMINFO_size(symbol_info));

    SYMINFO_ofst(symbol_info) = base_ofst;
    SECTINFO_align(sect_info) =
        MAX(SECTINFO_align(sect_info), SYMINFO_align(symbol_info));
}


void ELFMgr::processUnullData(MOD SymbolInfo * symbol_info)
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
    bytevec->set(bytevec->get_capacity() - 1, 0);
    ::memcpy((void*)(bytevec->get_vec() + bytevec_sz),
             (void*)(bin_vec.get_vec()), sz);
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

    CHAR const* name = SYMINFO_name_str(symbol_info);
    ASSERT0(name);

    //Count the length of 'name_num'.
    //e.g.: 1.given name_num '1', and the length is 1.
    //      2.given name_num '22', and the length is 2.
    //      3.given name_num '333', and the length is 3.
    UINT num_width = 0;
    UINT num_tmp = name_num;
    while (num_tmp > 0) { num_tmp /= ELF_NUM_INT_10; num_width++; }

    //'2' represents the length of '.' and '\0'.
    CHAR * new_name = (CHAR*)xmalloc(
        ::strlen(name) + num_width + 2);
    ASSERT0(new_name);

    //Assemble new symbol name. The format is 'name.name_num'.
    //e.g.: 'str.1', 'str.22', 'str.333'.
    ::sprintf(new_name, "%s.%d%c", name, name_num, '\0');
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


EM_STATUS ELFMgr::readELF(UINT64 offset)
{
    //'offset' is the offset of target ELF in ELFAR file.
    setELFFileOffset(offset);
    readELFContent(true, false);
    //Re-set offset to begin position.
    setELFFileOffset(0);
    return EM_SUCC;
}


SectionInfo * GenMappedOfSectInfoMap::createMapped(CHAR const* s)
{
    ASSERT0(s && m_sect_mgr && m_elf_mgr);
    if (m_elf_mgr->addToSymTab(s) ==
        m_elf_mgr->addToSymTab(SUBTEXT_ENTRY_SH_PRE)) {
        return m_sect_mgr->allocSectionInfo(SH_TYPE_TEXT);
    }
    return m_sect_mgr->allocSectionInfo(m_elf_mgr->getSectionType(s));
}


//
// =========================== ARFILE Start ========================
//


//
// =========================== ELFARMgr Start ========================
//
ELFARMgr::ELFARMgr()
{
    m_ar_file_list = nullptr;
}


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


//
// =========================== LinkerMgr Start ========================
//
LinkerMgr::LinkerMgr()
{
    m_output_file_name = nullptr;
    m_dump = nullptr;
    m_output_elf_mgr = nullptr;

    initDumpFile(LINKERMGR_DUMP_LOG_FILE_NAME);
}


LinkerMgr::~LinkerMgr()
{
    for (ELFMgr * elf_mgr = m_elf_mgr_meta_list.get_head();
         elf_mgr != nullptr; elf_mgr = m_elf_mgr_meta_list.get_next()) {
        if (elf_mgr != nullptr) { delete elf_mgr; }
    }

    //Free m_dump.
    closeDump();
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
    return EM_SUCC;
}


void LinkerMgr::closeDump()
{
    if (m_dump != nullptr) { delete m_dump; m_dump = nullptr; }
}


bool LinkerMgr::outputExeELF()
{
    //Wait other part.
    return true;
}

} //namespace elf
