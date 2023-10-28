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
#define ELF_VAL_UNDEF 0 //Common used zero.
#define PHASE(a)
#define RET_ERR(det,info,retcode) if (det) { PHASE(info); return (retcode); }

namespace elf {

typedef struct {
    CHAR const* colname; //the name of the column.
    CHAR const* colfmt; //the print-format of value to the column.
    UINT colwidth; //the maximum print byte width to the column.
} TabCol;

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


static SymbolLinkAttrFlag mapSymbolLinkAttr(
    SymbolLinkAttrMap const& symbol_link_attr_map, xoc::Var const* var)
{
    ASSERT0(var);

    return symbol_link_attr_map.get(var);
}


static SYMBOL_SECTION judgeSymbolSection(
    SymbolLinkAttrMap const& symbol_link_attr_map, Var const* var)
{
    if (var->is_func()) {
        return ((mapSymbolLinkAttr(
                 symbol_link_attr_map, var).have(SYMBOL_ATTR_EXTERN) ||
                 !var->is_global()) ? SYMBOL_DEFAULT : SYMBOL_TEXT);
    }
    if (var->getStorageSpace() == SS_SPM) { return SYMBOL_SPM; }
    if (var->is_readonly()) { return SYMBOL_CONST; }
    if (var->is_vector() || var->is_string() || var->is_mc()) {
        return var->hasInitVal() ? SYMBOL_DATA : SYMBOL_BSS;
    }
    return var->hasInitVal() ? SYMBOL_SDATA : SYMBOL_SBSS;
}


UINT ELFSectionInfo::getVarAlign(SymbolLinkAttrMap const& symbol_link_attr_map,
                                 Var const* var)
{
    switch (judgeSymbolSection(symbol_link_attr_map, var)) {
    //.sbss
    case SYMBOL_SBSS: return getBssAlign();
    //.sdata
    case SYMBOL_SDATA: return getSdataAlign();
    //.bss
    case SYMBOL_BSS: return getBssAlign();
    //.data
    case SYMBOL_DATA: return getDataAlign();
    //.spm
    case SYMBOL_SPM: return getSpmAlign();
    //.const
    case SYMBOL_CONST: return getConstAlign();
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
ELFMgr::ELFMgr()
{
    m_file = nullptr;
    m_dump = nullptr;
    m_ti = nullptr;
    m_pool = nullptr;
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


void ELFMgr::genStrTabContent(OUT CHARVec & charvec,
                              OUT OffVec & offvec,
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
        shdr->s_offset = off;
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
    closeELF();
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
        shdr->s_offset = xcom::ceil_align(curoffset, shdr->s_addr_align);
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


void ELFMgr::extractAssBinDescVec(OUT BYTEVec & content,
                                  AssembleBinDescVec const& abdv)
{
    UINT sz = abdv.getTotalByteSize();
    BYTEVec tmp(sz);
    xcom::AssembleBinBuf as(&(tmp[0]), sz, abdv);
    for (UINT i = 0; i < sz; i++) { content.set(i, tmp[i]); }
}


void ELFMgr::setSymbolValue(BYTE const* space, Word st_name,
                            xcom::UCHAR st_bind, xcom::UCHAR st_type,
                            xcom::UCHAR st_other, Half st_shndx,
                            Addr st_value, Addr st_size)
{
    ELFSym sym;
    sym.st_value = st_value;
    sym.st_size = st_size;
    sym.st_name = st_name;
    sym.st_bind = st_bind;
    sym.st_type = st_type;
    sym.st_other = st_other;
    sym.st_shndx = st_shndx;

    sym.insert(space, this);
}


void ELFMgr::constructRelAssBinDescVec(OUT AssembleBinDescVec & rel_desc_vec,
                                       ELFRela64 const& rela, UINT index)
{
    AssembleBinDesc d0(sizeof(rela.r_offset) * BITS_PER_BYTE, rela.r_offset);
    AssembleBinDesc d1(sizeof(rela.r_type) * BITS_PER_BYTE, rela.r_type);
    AssembleBinDesc d2(sizeof(rela.r_sym) * BITS_PER_BYTE, rela.r_sym);
    AssembleBinDesc d3(sizeof(rela.r_addend) * BITS_PER_BYTE, rela.r_addend);

    rel_desc_vec.set(index * STRUCT_ELFRELA64_MEMBER_NUM, d0);
    rel_desc_vec.set(index * STRUCT_ELFRELA64_MEMBER_NUM + 1, d1);
    rel_desc_vec.set(index * STRUCT_ELFRELA64_MEMBER_NUM + 2, d2);
    rel_desc_vec.set(index * STRUCT_ELFRELA64_MEMBER_NUM + 3, d3);
}


void ELFMgr::constructSymbolNull(OUT BYTEVec & bytevec)
{
    BYTE * sym = (BYTE *)ALLOCA(ELFSym::getSize(this));

    setSymbolValue(sym, ELF_VAL_UNDEF, ELF_VAL_UNDEF, SYMBOL_NOTYPE,
                   ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF);

    for (UINT i = 0; i < ELFSym::getSize(this); i++) {
        bytevec.set(i, sym[i]);
    }
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
    hdr.e_phensize = ELF_VAL_UNDEF;
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
    symtab_shdr->s_size = sym.get_elem_count();
    symtab_shdr->s_link = (Word32)getSectHeaderIdx(strtab_shdr);
    symtab_shdr->s_info = (Word32)getSectHeaderIdx(text_shdr);
    //typedef struct {                    typedef struct {
    //    Word32 st_name;                     Word32 st_name;
    //    ......                              ......
    //    Addr64 st_size;     <-------->      Word32 st_size;
    //    ......                              ......
    //    Half st_shndx;                      Half st_shndx;
    //} ELFSym64;                         } ELFSym32;
    symtab_shdr->s_addr_align = is64bit() ? sizeof(Addr64) : sizeof(Word32);
    symtab_shdr->s_entry_size = ELFSym::getSize(this);
    symtab_shdr->s_content = (BYTE*)sym.get_vec();
    symtab_shdr->s_name_str = SYMTAB_SH_NAME;
}


void ELFMgr::constructELFStrTabSection(MOD ELFSHdr * strtab_shdr,
                                       CHARVec & sym_str)
{
    strtab_shdr->s_type = S_STRTAB;
    strtab_shdr->s_addr = ELF_VAL_UNDEF;
    strtab_shdr->s_size = sym_str.get_elem_count();
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
    sbss_shdr->s_size = sbss.get_capacity();
    sbss_shdr->s_entry_size = ELF_VAL_UNDEF;
    sbss_shdr->s_addr_align = sbss_align;
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
    sdata_shdr->s_size = sdata.get_elem_count();
    sdata_shdr->s_entry_size = ELF_VAL_UNDEF;
    sdata_shdr->s_addr_align = sdata_align;
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
    bss_shdr->s_size = bss.get_capacity();
    bss_shdr->s_entry_size = ELF_VAL_UNDEF;
    bss_shdr->s_addr_align = bss_align;
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
    data_shdr->s_size = data.get_elem_count();
    data_shdr->s_entry_size = ELF_VAL_UNDEF;
    data_shdr->s_addr_align = data_align;
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
    spm_shdr->s_size = spm.get_elem_count();
    spm_shdr->s_entry_size = ELF_VAL_UNDEF;
    spm_shdr->s_addr_align = spm_align;
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
    const_shdr->s_size = const_data.get_elem_count();
    const_shdr->s_entry_size = ELF_VAL_UNDEF;
    const_shdr->s_addr_align = const_align;
    const_shdr->s_content = (BYTE*)const_data.get_vec();
    const_shdr->s_name_str = CONST_SH_NAME;
    SET_FLAG(const_shdr->s_flags, SF_ALLOC);
}


void ELFMgr::constructELFFuncSection(ELFSHdr * func_shdr, BYTEVec & code,
                                     CHAR const* name, MOD BYTE * text_space)
{
    //Since program region may has multiple functions that need to be wrote
    //into ELF, new space should be created to prevent being released.
    ::memcpy(text_space, (BYTE *)(code.get_vec()), code.get_elem_count());

    func_shdr->s_type = S_PROGBITS;
    func_shdr->s_addr = ELF_VAL_UNDEF;
    func_shdr->s_size = code.get_elem_count();
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
    rela_shdr->s_entry_size = ELFRela::getSize(this);
    rela_shdr->s_link = (Word32)getSectHeaderIdx(sym_shdr);
    rela_shdr->s_info = (Word32)getSectHeaderIdx(text_func_shdr);
    //typedef struct {                 typedef struct {
    //    Addr64 r_offset;   <------>      Addr32 r_offset;
    //    Word32 r_type;                   Word32 r_type:8;
    //    Word32 r_sym;                    Word32 r_sym:24;
    //    SWord64 r_addend;                SWord32 r_addend;
    //} ELFRela64;                     } ELFRela32;
    rela_shdr->s_addr_align = is64bit() ? sizeof(Addr64) : sizeof(Addr32);
    rela_shdr->s_content = rel_space;
    rela_shdr->s_name_str = name;
    SET_FLAG(rela_shdr->s_flags, SF_INFO_LINK);
}


void ELFMgr::processELFTextRelSection(ELFSHdr const* symtab_shdr,
                                      StringVec const& func_name,
                                      EntryFuncMap const& entry_func_map,
                                      StringList const& sym_name,
                                      OUT UINT & si)
{
    xcom::Vector<UINT> rel_begin;
    //Compute a vector saving relocation data begin index of function.
    computeFuncRelocIndex(rel_begin);

    for (UINT i = 0; i < func_name.get_elem_count(); i++) {
        CHAR const* name = func_name[i];

        //Get function name and generate section names of .text.xxx and
        //.rel.text.xxx.
        //The section names of kernel function are .aitext.xxx
        //and .rel.aitext.xxx.
        bool is_entry_function = entry_func_map.get(i);
        CHAR const* subtext_section_name = is_entry_function ?
            SUBTEXT_ENTRY_SH_PRE : SUBTEXT_SH_PRE;
        CHAR const* rela_section_name = is_entry_function ?
            RELA_KERNEL_SH_NAME : RELA_SH_NAME;

        xcom::StrBuf buf0(32);
        buf0.strcat(subtext_section_name);
        buf0.strcat(name);

        xcom::StrBuf buf1(32);
        buf1.strcat(rela_section_name);
        buf1.strcat(name);

        //Generate content for .text.xxx.
        BYTEVec text_content;
        genFuncTextContent(text_content, i);

        BYTEVec rel_content;
        genFuncRelContent(rel_content, sym_name, rel_begin, i);

        //Malloc some spaces to save intermediate data of function region data
        //processing. It is necessary because tmporary space will be freed.
        CHAR * text_name_space = (CHAR *)(xmalloc(buf0.buflen));
        ::memcpy(text_name_space, buf0.buf, buf0.buflen);
        CHAR * rel_name_space = (CHAR *)(xmalloc(buf1.buflen));
        ::memcpy(rel_name_space, buf1.buf, buf1.buflen);
        BYTE * text_space = (BYTE *)(xmalloc(text_content.get_elem_count()));
        BYTE * rel_space = (BYTE *)(xmalloc(rel_content.get_elem_count()));

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
    setSectHeaderNameStrTabContent((BYTE *)charvec.get_vec(),
                                   charvec.get_elem_count());
    setSectContentOffset();
}


void ELFMgr::setSymbolLinkAttr(xoc::Var const* var,
                               SYMBOL_LINK_ATTR_FLAG sym_attr)
{
    ASSERT0(var);

    if (m_symbol_link_attr_map.find(var)) {
        SymbolLinkAttrFlag sym_flag =
            mapSymbolLinkAttr(m_symbol_link_attr_map, var);
        sym_flag.set(sym_attr);
        m_symbol_link_attr_map.remove(var);
        m_symbol_link_attr_map.set(var, sym_flag);
    } else {
        m_symbol_link_attr_map.set(var, SymbolLinkAttrFlag(sym_attr));
    }
}


static BinWord extractScalarValue(Var const* var, UINT size)
{
    ASSERT0(var && var->hasInitVal());
    BYTE* buf = var->getByteValue()->getBuffer();
    ASSERT0(buf);

    Type const* tp = var->getType();
    ASSERT0(tp);

    switch (tp->getDType()) {
    case D_BF16:
    case D_F16: return *((UINT16*)buf);
    case D_F32: return *((UINT32*)buf);
    case D_F64:
    case D_U64:
    case D_I64: return *((BinWord*)buf);
    case D_F128:
    case D_U128:
    case D_I128: return (BinWord)(*((UINT128*)buf));
    default: return extractValidValueViaBitWidth<BinWord>(*((BinWord*)buf),
                                                          size);
    }

    return (BinWord)0;
}


void ELFMgr::assembleVarToContent(OUT AssembleBinDescVec & content_desc_vec,
                                  Var const* var)
{
    ASSERT0(var && var->hasInitVal());

    //Size of symbol need to be aligned upwards according to align value of
    //section where symbol located.
    UINT align = m_sect_info->getVarAlign(m_symbol_link_attr_map, var) *
        BITS_PER_BYTE;

    //Class AssembleBinDesc need bit size of given binary value.
    UINT bitsz = 0;

    //String and vector data belong to .data or .const section.
    //Single type data belong to .sdata or .const section.
    if (var->is_string() || var->is_vector() || var->is_mc()) {
        bitsz = (UINT)xcom::ceil_align(var->getByteSize(
            getRegion()->getTypeMgr()) * BITS_PER_BYTE, align);
        ASSERT0(bitsz != 0);

        BYTE const* byte = var->is_string() ?
            ((BYTE*)VAR_string(var)->getStr()) :
            (var->getByteValue()->getBuffer());
        ASSERT0(byte);

        AssembleBinDesc asbd(bitsz, byte);
        content_desc_vec.append(asbd);

        return;
    }

    //Process scalar variables.
    bitsz = var->getByteValue()->getSize() * BITS_PER_BYTE;
    BinWord word = extractScalarValue(var, bitsz);
    bitsz = (UINT)xcom::ceil_align(bitsz, align);

    AssembleBinDesc asbd(bitsz, word);
    content_desc_vec.append(asbd);
}


static Addr computeVarSize(Var const* var, TypeMgr const* tm)
{
    if (var->is_func()) { return 0; }
    if (var->is_string()) {
        return var->getString() == nullptr ? 0 : var->getByteSize(tm);
    }
    return var->getByteSize(tm);
}


static void setSectionInfo(OUT StringVec & func_name,
                           OUT ELFSectionInfo * sect_info,
                           OUT UINT & func_num,
                           OUT EntryFuncMap & entry_func_map,
                           SymbolLinkAttrMap const& symbol_link_attr_map,
                           Var const* var, UINT align)
{
    switch (judgeSymbolSection(symbol_link_attr_map, var)) {
    //.text.xxx
    case SYMBOL_TEXT: {
        func_name.append(var->get_name()->getStr());
        entry_func_map.set(func_num, var->is_entry());
        func_num++;
        break;
    }
    //.sbss
    case SYMBOL_SBSS: {
        sect_info->setSbss(true);
        sect_info->setSbssAlign(MAX(sect_info->getSbssAlign(), align));
        break;
    }
    //.sdata
    case SYMBOL_SDATA: {
        sect_info->setSdata(true);
        sect_info->setSdataAlign(MAX(sect_info->getSdataAlign(), align));
        break;
    }
    //.bss
    case SYMBOL_BSS: {
        sect_info->setBss(true);
        sect_info->setBssAlign(MAX(sect_info->getBssAlign(), align));
        break;
    }
    //.data
    case SYMBOL_DATA: {
        sect_info->setData(true);
        sect_info->setDataAlign(MAX(sect_info->getDataAlign(), align));
        break;
    }
    //.spm
    case SYMBOL_SPM: {
        sect_info->setSpm(true);
        sect_info->setSpmAlign(MAX(sect_info->getSpmAlign(), align));
        break;
    }
    //.const
    case SYMBOL_CONST: {
        sect_info->setConst(true);
        sect_info->setConstAlign(MAX(sect_info->getSpmAlign(), align));
        break;
    }
    default: break; //Variable is not available.;
    }
}


void ELFMgr::setSymbol(MOD BYTE * sym, MOD ELFSymbolOff & symbol_off,
                       Var const* var, ELFSectionInfo const* sect_info,
                       Word const& name, UCHAR const& bind, UCHAR const& other,
                       Addr const& size)
{
    switch (judgeSymbolSection(m_symbol_link_attr_map, var)) {
    case SYMBOL_DEFAULT: {
        setSymbolValue(sym, name, STB_GLOBAL, ELF_VAL_UNDEF, ELF_VAL_UNDEF,
                       ELF_VAL_UNDEF, ELF_VAL_UNDEF, ELF_VAL_UNDEF);
        break;
    }
    case SYMBOL_SBSS: {
        //If there is extern attribute of the symbol, the value of
        //st_shndx and st_size field are UNDEF(means that the symbol
        //is undefined in this object file).
        Half index = mapSymbolLinkAttr(
            m_symbol_link_attr_map, var).have(SYMBOL_ATTR_EXTERN) ?
            ELF_VAL_UNDEF : BASE_SEC_NUM;
        Addr sz = mapSymbolLinkAttr(
            m_symbol_link_attr_map, var).have(SYMBOL_ATTR_EXTERN) ?
            ELF_VAL_UNDEF : size;
        setSymbolValue(sym, name, bind, SYMBOL_OBJECT, other, index,
                       symbol_off.sbss_off, sz);
        symbol_off.sbss_off += (UINT)MAX(sect_info->getSbssAlign(), sz);
        break;
    }
    case SYMBOL_SDATA: {
        Half index = BASE_SEC_NUM + sect_info->hasSbss();
        setSymbolValue(sym, name, bind, SYMBOL_OBJECT, other, index,
                       symbol_off.sdata_off, size);
        symbol_off.sdata_off += (UINT)MAX(sect_info->getSdataAlign(), size);
        break;
    }
    case SYMBOL_BSS: {
        //If there is extern attribute of the symbol, the value of
        //st_shndx and st_size field are UNDEF(means that the symbol
        //is undefined in this object file).
        Half index = mapSymbolLinkAttr(
            m_symbol_link_attr_map, var).have(SYMBOL_ATTR_EXTERN) ?
            ELF_VAL_UNDEF : BASE_SEC_NUM + sect_info->hasSbss() +
            sect_info->hasSdata();
        Addr sz = mapSymbolLinkAttr(
            m_symbol_link_attr_map, var).have(SYMBOL_ATTR_EXTERN) ?
            ELF_VAL_UNDEF : size;
        setSymbolValue(sym, name, bind, SYMBOL_OBJECT, other, index,
                       symbol_off.bss_off, sz);
        symbol_off.bss_off += (UINT)xcom::ceil_align(
            sz, sect_info->getBssAlign());
        break;
    }
    case SYMBOL_DATA: {
        Half index = BASE_SEC_NUM + sect_info->hasSbss() +
            sect_info->hasSdata() + sect_info->hasBss();
        setSymbolValue(sym, name, bind, SYMBOL_OBJECT, other, index,
                       symbol_off.data_off, size);
        symbol_off.data_off +=
            (UINT)xcom::ceil_align(size, sect_info->getDataAlign());
        break;
    }
    case SYMBOL_SPM: {
        Half index = mapSymbolLinkAttr(
            m_symbol_link_attr_map, var).have(SYMBOL_ATTR_EXTERN) ?
            ELF_VAL_UNDEF : BASE_SEC_NUM + sect_info->hasSbss() +
            sect_info->hasSdata() + sect_info->hasBss() +
            sect_info->hasData();
        Addr sz = mapSymbolLinkAttr(
            m_symbol_link_attr_map, var).have(SYMBOL_ATTR_EXTERN) ?
            ELF_VAL_UNDEF : size;

        setSymbolValue(sym, name, bind, SYMBOL_OBJECT, other, index,
                       symbol_off.spm_off, sz);
        symbol_off.spm_off += (UINT)MAX(sect_info->getSpmAlign(), sz);
        break;
    }
    case SYMBOL_CONST: {
        Half index = BASE_SEC_NUM + sect_info->hasSbss() +
            sect_info->hasSdata() + sect_info->hasBss() +
            sect_info->hasData() + sect_info->hasSpm();
        setSymbolValue(sym, name, STB_GLOBAL, SYMBOL_OBJECT, ELF_VAL_UNDEF,
                       index, symbol_off.const_off, size);
        symbol_off.const_off += (UINT)MAX(sect_info->getConstAlign(), size);
        break;
    }
    case SYMBOL_TEXT: {
        setSymbolValue(sym, name, bind, SYMBOL_FUNC,
                       getSymOtherInfo() | other, symbol_off.text_ind,
                       ELF_VAL_UNDEF, m_func_size[symbol_off.func_off]);
        //.text.xxx and .rel.text.xxx section headers are always together.
        symbol_off.text_ind += 2;
        symbol_off.func_off++;
        break;
    }
    default: ASSERT0(0); //Variable is not available.
    }
}


void ELFMgr::constructSymbolUnull(OUT BYTEVec & bytevec,
                                  OffVec const& sym_str_off,
                                  ELFSectionInfo const* sect_info)
{
    ASSERT0(sect_info);

    struct ELFSymbolOff symbol_off;
    symbol_off.text_ind = BASE_SEC_NUM + sect_info->hasSbss() +
        sect_info->hasSdata() + sect_info->hasBss() +
        sect_info->hasData() + sect_info->hasSpm() +
        sect_info->hasConst();

    TMWORD ind = ELFSym::getSize(this);
    for (Var const* var = m_saving_var_list.get_head();
        var != nullptr; var = m_saving_var_list.get_next()) {

        BYTE * sym = (BYTE *)ALLOCA(ELFSym::getSize(this));

        //Get some common attributes: name, bind, other, size
        Word name = sym_str_off[symbol_off.var_name_off];
        UCHAR bind = mapSymbolLinkAttr(
            m_symbol_link_attr_map, var).have(SYMBOL_ATTR_WEAK) ?
            STB_WEAK : (var->is_global() ? STB_GLOBAL : STB_LOCAL);
        UCHAR other = mapSymbolLinkAttr(
            m_symbol_link_attr_map, var).have(SYMBOL_ATTR_VISIBLE) ?
            STV_DEFAULT : STV_HIDDEN;
        Addr size = computeVarSize(var, getRegion()->getTypeMgr());

        //Set info of symbol.
        setSymbol(sym, symbol_off, var, sect_info, name, bind, other, size);

        for (UINT i = 0; i < ELFSym::getSize(this); i++) {
            bytevec.set((VecIdx)(ind + i), sym[i]);
        }

        ind += ELFSym::getSize(this);
        symbol_off.var_name_off++;
    }
}


void ELFMgr::genSymTabContent(OUT BYTEVec & bytevec,
                              OffVec const& offvec,
                              ELFSectionInfo const* sect_info)
{
    AssembleBinDescVec sym_desc_vec;

    bytevec.grow(m_saving_var_list.get_elem_count() * ELFSym::getSize(this));

    constructSymbolNull(bytevec);

    constructSymbolUnull(bytevec, offvec, sect_info);
}


void ELFMgr::genSbssContent(OUT BYTEVec & sbss_content)
{
    HOST_UINT totalbytesize = 0;

    for (Var const* var = m_saving_var_list.get_head();
        var != nullptr; var = m_saving_var_list.get_next()) {

        //Skip variables unuseful.

        if (judgeSymbolSection(m_symbol_link_attr_map, var) != SYMBOL_SBSS) {
            continue;
        }

        totalbytesize += var->getByteSize(getRegion()->getTypeMgr());
    }

    sbss_content.grow((UINT)totalbytesize);
}


void ELFMgr::genSdataContent(OUT BYTEVec & sdata_content)
{
    AssembleBinDescVec sdata_desc_vec;

    for (Var const* var = m_saving_var_list.get_head();
        var != nullptr; var = m_saving_var_list.get_next()) {

        //Skip variables unuseful.
        if (judgeSymbolSection(m_symbol_link_attr_map, var) != SYMBOL_SDATA) {
            continue;
        }

        assembleVarToContent(sdata_desc_vec, var);
    }

    extractAssBinDescVec(sdata_content, sdata_desc_vec);
}


void ELFMgr::genBssContent(OUT BYTEVec & bss_content)
{
    HOST_UINT totalbytesize = 0;

    for (Var const* var = m_saving_var_list.get_head();
        var != nullptr; var = m_saving_var_list.get_next()) {

        //Skip variables unuseful.
        if (judgeSymbolSection(m_symbol_link_attr_map, var) != SYMBOL_BSS) {
            continue;
        }

        totalbytesize += var->getByteSize(getRegion()->getTypeMgr());
    }

    bss_content.grow((UINT)totalbytesize);
}


void ELFMgr::genDataContent(OUT BYTEVec & data_content)
{
    AssembleBinDescVec data_desc_vec;

    for (Var const* var = m_saving_var_list.get_head();
        var != nullptr; var = m_saving_var_list.get_next()) {

        //Skip variables unuseful.
        if (judgeSymbolSection(m_symbol_link_attr_map, var) != SYMBOL_DATA) {
            continue;
        }

        assembleVarToContent(data_desc_vec, var);
    }

    extractAssBinDescVec(data_content, data_desc_vec);
}


void ELFMgr::genSpmContent(OUT BYTEVec & spm_content)
{
    AssembleBinDescVec spm_desc_vec;

    for (Var const* var = m_saving_var_list.get_head();
        var != nullptr; var = m_saving_var_list.get_next()) {

        //Skip variables unuseful.
        if (judgeSymbolSection(m_symbol_link_attr_map, var) != SYMBOL_SPM) {
            continue;
        }

        if (var->is_string() && var->hasInitString()) {
            UINT sz = (UINT)::strlen(
                VAR_string(var)->getStr()) * BITS_PER_BYTE;
            AssembleBinDesc asbd(sz, (BYTE *)(VAR_string(var)->getStr()));
            spm_desc_vec.append(asbd);
        } else if (!var->is_string() && var->hasInitVal()) {
            UINT sz = var->getByteValue()->getSize() * BITS_PER_BYTE;
            AssembleBinDesc asbd(sz, var->getByteValue()->getBuffer());
            spm_desc_vec.append(asbd);
        } else {
            UINT sz = var->getByteSize(getRegion()->getTypeMgr()) *
                BITS_PER_BYTE;
            if (isSizeValid(sz)) {
                AssembleBinDesc asbd(sz, BinWord(0));
                spm_desc_vec.append(asbd);
            } else {
                sz = (UINT)xcom::ceil_align(sz, BIN_WORD_SIZE);
                for (UINT j = 0; j < (sz / BIN_WORD_SIZE); j++) {
                    AssembleBinDesc asbd(BIN_WORD_SIZE, BinWord(0));
                    spm_desc_vec.append(asbd);
                }
            }
        }
    }
    extractAssBinDescVec(spm_content, spm_desc_vec);
}


void ELFMgr::genConstContent(OUT BYTEVec & bytevec)
{
    AssembleBinDescVec data_desc_vec;

    for (Var const* var = m_saving_var_list.get_head();
        var != nullptr; var = m_saving_var_list.get_next()) {

        //Skip variables unuseful.
        if (judgeSymbolSection(m_symbol_link_attr_map, var) != SYMBOL_CONST) {
            continue;
        }

        assembleVarToContent(data_desc_vec, var);
    }

    extractAssBinDescVec(bytevec, data_desc_vec);
}


void ELFMgr::genFuncRelContent(OUT BYTEVec & bytevec,
                               StringList const& names,
                               xcom::Vector<UINT> const& begin,
                               UINT ind)
{
    AssembleBinDescVec rel_desc_vec;

    //Compute begin and end index of relocation info of current function
    //region.
    UINT begin_ind = 0;
    UINT end_ind = 0;
    Addr64 offset = 0;
    Word32 sym_ind = 0;
    Word32 type = 0;
    SWord64 addend = 0;

    //Compute begin and end index of relocation info for different function.
    getFuncRelocIndex(begin_ind, end_ind, begin, ind);

    rel_desc_vec.grow(end_ind * STRUCT_ELFRELA64_MEMBER_NUM);

    for (UINT i = begin_ind; i < end_ind; i++) {
        ELFRela64 rela;
        getRelocInfo(offset, sym_ind, type, addend, names, i);

        rela.r_offset = offset;
        rela.r_sym = sym_ind;
        rela.r_type = type;
        rela.r_addend = addend;

        constructRelAssBinDescVec(rel_desc_vec, rela, i);
    }

    extractAssBinDescVec(bytevec, rel_desc_vec);
}


//Compute a vector saving function and its relocation data begin index.
void ELFMgr::computeFuncRelocIndex(OUT xcom::Vector<UINT> & begin)
{
    CHAR const* name = "";

    for (UINT i = 0; i < m_func_relocation.get_elem_count(); i++) {
        if (::strcmp(m_func_relocation[i].caller, name) != 0) {
            name = m_func_relocation[i].caller;
            begin.append(i);
        }
    }
}


void ELFMgr::getFuncRelocIndex(OUT UINT & begin_ind, OUT UINT & end_ind,
                               xcom::Vector<UINT> const& begin,
                               UINT ind)
{
    //compute begin index
    begin_ind = begin[ind];

    //compute end index
    end_ind = (ind == (begin.get_elem_count() - 1)) ?
        m_func_relocation.get_elem_count() : begin[ind + 1];
}


//Get size of current function region and extract codes of size from all
//generated codes.
void ELFMgr::genFuncTextContent(OUT BYTEVec & bytevec,
                                UINT index)
{
    UINT offset = 0;
    for (UINT i = 0; i < index; i++) { offset += m_func_size[i]; }

    bytevec.grow(m_func_size[index]);

    for (UINT i = 0; i < m_func_size[index]; i++) {
        bytevec.set(i, m_func_code[i + offset]);
    }
}


static UINT findPosition(StringList const& strlst, CHAR const* str)
{
    StringList::Iter it;
    UINT ind = 0;
    ASSERT0(strlst.get_head(&it) && str);

    for (CHAR const* s = strlst.get_head(&it); s != nullptr;
        ind++, s = strlst.get_next(&it)) {
        if (!::strcmp(s, str)) { return ind; }
    }

    UNREACHABLE(); //not find.
    return 0;
}


void ELFMgr::getRelocInfo(OUT Addr64 & offset,
                          OUT Word32 & sym_ind,
                          OUT Word32 & type,
                          OUT SWord64 & addend,
                          StringList const& names,
                          UINT index)
{
    offset = m_func_relocation[index].call_location;
    type = m_func_relocation[index].reloc_type;

    //Index of name of current symbol in all names.
    sym_ind = findPosition(names, m_func_relocation[index].callee) + 1;

    //Get relocation addend value based relocation type.
    addend = getRelocAddend(index);
}


static UINT computeAlignVal(Var const* var, TypeMgr const* tm)
{
    return var->is_func() ? 0 : var->get_align();
}


void ELFMgr::collectELFFactor(OUT StringList & sym_name,
                              OUT StringVec & func_name,
                              OUT EntryFuncMap & entry_func_map,
                              OUT ELFSectionInfo * sect_info)
{
    TypeMgr const* tm = getRegion()->getTypeMgr();
    UINT func_num = 0;

    for (Var const* var = m_saving_var_list.get_head();
        var != nullptr; var = m_saving_var_list.get_next()) {

        //Collector names of all available symbols.
        sym_name.append_tail(var->get_name()->getStr());

        //Compute align value based different type of variable.
        UINT align = computeAlignVal(var, tm);
        //Align value bust be power-of-2 number.
        if (!var->is_func()) { ASSERT0(xcom::isPowerOf2(align)); }
        else { ASSERT0(align == 0); }

        //Set section info based different type of variable.
        setSectionInfo(func_name, sect_info, func_num, entry_func_map,
                       m_symbol_link_attr_map, var, align);
    }

    //Note: "func_num * 2" : A device func will generate
    //two sections like .text.xxx and .rel.text.xxx.
    sect_info->setShdrNum(BASE_SEC_NUM + sect_info->hasSdata() +
        sect_info->hasSbss() + sect_info->hasData() + sect_info->hasBss() +
        sect_info->hasSpm() + sect_info->hasConst() + (func_num * 2));
}


//If a variable is user-defined function, it must be global and not have
//.extern attribute.
bool ELFMgr::isUserDefinedFunction(Var const* var)
{
    return isVarAvailable(var) && var->is_func() && var->is_global() &&
        !mapSymbolLinkAttr(m_symbol_link_attr_map, var).
        have(SYMBOL_ATTR_EXTERN);
}


void ELFMgr::extractSavingVarExceptUserDefFunc()
{
    VarVec const* var_vec = getRegion()->getVarMgr()->getVarVec();
    for (UINT i = 0; i < var_vec->get_elem_count(); i++) {
        Var * var = var_vec->get(i);

        //Since user-defined functions have been wirtten into
        //m_saving_var_vec, we need to extract available variables except them.
        if (isVarAvailable(var) && !isUserDefinedFunction(var)) {
            m_saving_var_list.append_tail(var);
        }
    }
}


void ELFMgr::write2ELF()
{
    //Define some structures recording contents of elf sections.
    CHARVec strtab_content; //Save content for .strtab.
    BYTEVec symtab_content; //Save content for .symtab.
    BYTEVec sbss_content;   //Save content for .sbss.
    BYTEVec sdata_content;  //Save content for .sdata.
    BYTEVec bss_content;    //Save content for .bss.
    BYTEVec data_content;   //Save content for .data.
    BYTEVec spm_content;    //Save content for .spm.
    BYTEVec reloc_content;  //Save content for .rel.text.xxx.
    BYTEVec text_content;   //Save content for .text.xxx.
    BYTEVec const_content;  //Save content for .const.

    OffVec symstr_off;      //Save offsets of name of each symbol.

    CHARVec charvec;        //Save names of all section headers.
    OffVec offvec;          //Save offsets of section header names.

    //Define some structures recording name of symbols.
    StringList sym_name;    //Save names of all symbols.
    StringVec func_name;    //Save names of function symbols.
    EntryFuncMap entry_func_map;    //Save entry functions.
    //Bug fix: If declaration of function is before definition of function,
    //data written into ELF will be wrong due to out-of-order.
    extractSavingVarExceptUserDefFunc();

    //Collect names of all symbols and compute some factors.
    collectELFFactor(sym_name, func_name, entry_func_map, m_sect_info);

    //Construct ELF header.
    constructELFHeader(m_sect_info->getShdrNum());

    //Generate contents for some sections including .symstr, .symtab,
    //.sbss, .sdata, .data and .const.
    genStrTabContent(strtab_content, symstr_off, sym_name);
    genSymTabContent(symtab_content, symstr_off, m_sect_info);

    if (m_sect_info->hasSbss()) { genSbssContent(sbss_content); }
    if (m_sect_info->hasSdata()) { genSdataContent(sdata_content); }
    if (m_sect_info->hasBss()) { genBssContent(bss_content); }
    if (m_sect_info->hasData()) { genDataContent(data_content); }
    if (m_sect_info->hasSpm()) { genSpmContent(spm_content); }
    if (m_sect_info->hasConst()) { genConstContent(const_content); }

    //Construct ELF section headers and sections.
    UINT si = 0;

    // Construct NULL section header and section.
    ELFSHdr * null_shdr = getSectHeader(si++);
    constructELFNullSection(null_shdr);

    // Construct .text section header and section.
    ELFSHdr * text_shdr = getSectHeader(si++);
    constructELFTextSection(text_shdr);

    // Construct .shdr_strtab section header and section.
    ELFSHdr * shstr_shdr = getSectHeader(si++);
    constructELFShStrSection(shstr_shdr);

    // Construct .strtab section header and section.
    ELFSHdr * strtab_shdr = getSectHeader(si++);
    constructELFStrTabSection(strtab_shdr, strtab_content);

    // Construct .symtab section header and section.
    ELFSHdr * symtab_shdr = getSectHeader(si++);
    constructELFSymTabSection(symtab_shdr, strtab_shdr, text_shdr,
                              symtab_content);

    // Construct .sbss section header and section.
    if (m_sect_info->hasSbss()) {
        ELFSHdr * sbss_shdr = getSectHeader(si++);
        constructELFSbssSection(sbss_shdr, sbss_content,
                                m_sect_info->getSbssAlign());
    }

    // Construct .sdata section header and sections.
    if (m_sect_info->hasSdata()) {
        ELFSHdr * sdata_shdr = getSectHeader(si++);
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
    if (m_sect_info->hasData()) {
        ELFSHdr * data_shdr = getSectHeader(si++);
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
    processELFTextRelSection(symtab_shdr, func_name, entry_func_map,
                             sym_name, si);

    //Construct section header indexs.
    constructELFShIndex(charvec, offvec);

    //Write data into ELF file.
    writeELF(m_output_file_name);
}

} //namespace elf
