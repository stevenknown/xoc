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


//Hashing Function
//A hash table of Elf32_Word objects supports symbol table access.
//The same table layout is used for both the 32-bit and 64-bit file class.
//Labels appear below to help explain the hash table organization, but they are
//not part of the specification.
static ULONG elf_hash(UCHAR const* name)
{
    ULONG h = 0, g;
    while (*name) {
        h = (h << 4) + *name++;
        if (g = h & 0xf0000000) {
            h ^= g >> 24;
        }
        h &= ~g;
    }
    return h;
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
}


ELFMgr::~ELFMgr()
{
    if (m_file != nullptr) { delete m_file; m_file = nullptr; }
    if (m_dump != nullptr) { delete m_dump; m_dump = nullptr; }
    if (m_ti != nullptr) { delete m_ti; m_ti = nullptr; }
    smpoolDelete(m_pool);
    m_pool = nullptr;
}


void ELFMgr::clean()
{
    ::memset(&m_elf_hdr, 0, sizeof(ELFHdr));
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
    ::memset(p, 0, size);
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
    if (m_file->read(buf, offset, size, &actual_rd) != FileObj::FO_SUCC ||
        actual_rd != size) {
        return EM_RD_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::write(BYTE const* buf, size_t offset, size_t size)
{
    ASSERT0(m_file);
    size_t actual_wr = 0;
    if (m_file->write(buf, offset, size, &actual_wr) != FileObj::FO_SUCC ||
        actual_wr != size) {
        return EM_WR_ERR;
    }
    return EM_SUCC;
}


EM_STATUS ELFMgr::append(BYTE const* buf, size_t size)
{
    ASSERT0(m_file);
    size_t actual_wr = 0;
    if (m_file->append(buf, size, &actual_wr) != FileObj::FO_SUCC ||
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
    Off off = elf::ELFHdr::getSize(this);
    off += getHdr().e_shnum * elf::ELFSHdr::getSize(this);
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
        ::memset(buf, 0, sz);
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
        //        ("space loadable section should not be allocated by xmalloc"));
        //CASE:Sometimes, user expect to read all section contents.

        m_elf_sectheader[idx].s_content = (BYTE*)xmalloc(
            m_elf_sectheader[idx].s_size);
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
int a=0;;//hack
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
    ASSERTN(strtab, ("no strint table correspond to '%s'", getSectName(symtab)));
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
        readAllSectContent();
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
    return EM_SUCC;
}


EM_STATUS ELFMgr::writeELFHeaderAt(Word elfhdr_offset)
{
    BYTE * buf = (BYTE*)ALLOCA(ELFHdr::getSize(this));
    m_elf_hdr.insert(buf, this);
    if (EM_SUCC != write(buf, elfhdr_offset, ELFHdr::getSize(this))) {
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
            writePad(shdr->s_offset - curoffset);
        }
        if (shdr->s_size == 0) { continue; }
        ASSERTN(shdr->s_content, ("miss section content"));
        ASSERT0(shdr->s_offset == m_file->getFileSize());
        //Update and record the file byte offset to section header.
        //The section headers will be appended to file after its content.
        if (EM_SUCC != append(shdr->s_content, shdr->s_size)) {
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
        ::memset(buf, 0, sz);
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

} //namespace elf
