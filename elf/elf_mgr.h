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
#ifndef _ELF_MGR_H_
#define _ELF_MGR_H_

namespace elf {

#define ELF_SIZE_4GB 0xFFFFffff

typedef xcom::Vector<CHAR> CHARVec;
typedef xcom::Vector<Off> OffVec;
typedef xcom::List<CHAR const*> StringList;
typedef xcom::List<ELFSHdr*> SHdrList;

typedef enum tagEM_STATUS {
    EM_SUCC = 0,
    EM_ERR,
    EM_RD_ERR,
    EM_WR_ERR,
    EM_OPEN_ERR,
    EM_NO_TEXT_TAB,
    EM_NO_DATA,
    EM_NO_RODATA,
    EM_NO_RELOC_TAB,
    EM_NO_RELOCA_TAB,
    EM_NO_PHR,
    EM_INVALID_ELF_HEADER,
    EM_NO_DYNAMIC_TAB,
    EM_INVALID_PHDR,
    EM_NO_BSS_TAB,
    EM_SECTION_PROPERTY_CONFLICT,
    EM_NO_MAP_INFO,
    EM_NO_SYM_TAB,
    EM_BSS_SIZE_ZERO,
    EM_ADDR_ABS,
    EM_SYM_UNDEF,
    EM_UNEXECUTABLE,
    EM_NO_SUCH_APP_FIND,
    EM_INVALID_EXTEN_SYM,
    EM_NO_SECTION_DATA,
    EM_INVALID_SECTION_INDX,
    EM_ADDR_V_OVERFLOW,
    EM_ADDR16_V_OVERFLOW,
    EM_GP_OFST_OVERFLOW,
    EM_UNSUPPORT_RELOC_TYPE,
    EM_GP_NO_INIT,
    EM_STACK_NO_INIT,
    EM_NO_EXECUTE_ENTRY,
    EM_SECTION_NO_RELOCABLE,
    EM_NO_APP_DES,
    EM_UNDEF_PRC,
    EM_NOT_ELF,
    EM_NO_DYNSYM_TAB,
    EM_NO_MEM_SPACE,
    EM_INVALID_SECTION_SIZE,
    EM_ADDR_MISALIGNMENT,
    EM_TOO_LONG_NAME,
    EM_NO_SUCH_GBLK_FIND,
    EM_MBOX_CREATE_FAILED,
    EM_NO_SUCH_MEMPOOL_FIND,
    EM_UNKNOWN_MACHINE,
} EM_STATUS;

class ELFMgr {
    friend class ELFTargInfo;
protected:
    typedef TMap<ELFSHdr*, ELFSHdr*> SectHeader2StrTab;
    typedef TMapIter<ELFSHdr*, ELFSHdr*> SectHeader2StrTabIter;
    typedef TTab<ELFSHdr*> StrTabTab;
    typedef TTabIter<ELFSHdr*> StrTabTabIter;
    FileObj * m_file; //ELF file
    FileObj * m_dump; //Dump file
    ELFTargInfo * m_ti; //target dependent info.
    ELFHdr m_elf_hdr; //ELF header
    ELFPHdr * m_elf_phdr; //Program header

    //Record an array of section that are loaded in read section stage.
    ELFSHdr * m_elf_sectheader;

    //Record a string-table header that describe section header's name.
    ELFSHdr * m_elf_shstrtab;

    //Record the section header of acommon section.
    ELFSHdr * m_elf_acommontab;

    //Record the section header of scommon section.
    ELFSHdr * m_elf_scommontab;

    //Record the section header of dynamic table.
    ELFSHdr * m_elf_dyntab;

    //Record the section header of common table
    ELFSHdr * m_elf_commenttab;

    //Sections symbol mapping handle.
    //Represent the mapping information
    ELFMapSect * m_elf_map_section;

    //Objects or functions symbol mapping handle
    //Represent the mapping information
    ELFMapData * m_elf_map_data;

    //Reserve map entry for '_start' symbol for facitlity utility during
    //course of recording entry point.
    ELFMapSym * m_execucte_start_map_entry;

    SMemPool * m_pool;

    //Record the section header to string table.
    StrTabTab m_strtab_tab;

    //Record all sections that need perform relocation with addend.
    List<ELFSHdr*> m_relatab_sect_list;

    //Record all sections that need perform relocation.
    List<ELFSHdr*> m_reltab_sect_list;

    //Record all sections that has SYMBOL property.
    List<ELFSHdr*> m_symtab_sect_list;

    //Record all sections that has DYNAMIC SYMBOL property.
    List<ELFSHdr*> m_dynsymtab_sect_list;

    //Record all sections that has DYNAMIC property.
    //These kind of sections has dynamic linking information.
    List<ELFSHdr*> m_dyntab_sect_list;

    //Record all sections that indicate READONLY data.
    List<ELFSHdr*> m_readonly_data_sect_list;

    //Record all sections that indicate writable data.
    List<ELFSHdr*> m_data_sect_list;

    //Record all sections that indicate executable code.
    List<ELFSHdr*> m_text_sect_list;

    //Record all sections that indicate writable data.
    List<ELFSHdr*> m_bss_sect_list;
protected:
    virtual void allocTargInfo() = 0;
    EM_STATUS append(BYTE const* buf, size_t size);

    EM_STATUS closeELF();
    EM_STATUS closeDump();
    void clean();

    void formatSymWithAddend(OUT StrBuf & buf, ELFRela const& rela,
                             ELFSHdr const* symtab);

    EM_STATUS open(CHAR const* filename);
    EM_STATUS readELFHeader();
    EM_STATUS read(BYTE * buf, size_t offset, size_t size);
    EM_STATUS write(BYTE const* buf, size_t offset, size_t size);
    bool isSectionAllocable(size_t sectidx) const;
    bool isSectionException(size_t sect_ofst) const;

    //Load section data from ELF file
    //idx: data relatived section index
    EM_STATUS readSectContent(size_t idx);
    EM_STATUS readSectContent(ELFSHdr const* sh);

    //Read ELF program table
    EM_STATUS readProgramHeader();

    //Read all the section table
    EM_STATUS readSectHeaderTab();

    //Read symbol table content from ELF file
    EM_STATUS readSymTabContent();

    //Read all dynmaic symbol table from ELF file
    EM_STATUS readDynSymTabContent();

    //Read all dynmaic table from ELF file.
    EM_STATUS readDynTabContent();

    //Read all relocation table from ELF file.
    EM_STATUS readRelTabContent();

    //Read all relocation-addend table from ELF file.
    EM_STATUS readRelaTabContent();

    //Read all of .shstr, .str .dynstr table from ELF file
    EM_STATUS readAllStrTabContent();
    EM_STATUS readCommonStrTabContent();
    EM_STATUS readDynSymStrTabContent();
    EM_STATUS readDynStrTabContent();
    EM_STATUS readSHStrTabContent();

    //The function read the string table related to 'symtab'.
    //symtab: section header indicates a symbol table.
    EM_STATUS readRelatedStrTabContent(ELFSHdr const* symtab);

    //strtab: section header indicates a string table.
    EM_STATUS readStrTabContent(ELFSHdr * strtab);

    //Record the specific section header entry to facilitate afterward usage.
    void recordSectHeader(ELFSHdr * p);

    void dumpSymTabContent(ELFSHdr const* symtab) const;
    void dumpSymTabContent() const;
    void dumpRelTabContent() const;
    void dumpRelTabContent(ELFSHdr const* sh) const;
    void dumpRelaTabContent(ELFSHdr const* sh) const;
    void dumpRelaTabContent() const;
    void dumpCommonSymTabContent() const;
    void dumpDynSymTabContent() const;
    void dumpAllStrTabContent() const;
    void dumpELFHeader() const;
    void dumpSectHeaderTab() const;
    void dumpDynTabContent(ELFSHdr const* dyntab) const;
    void dumpDynTabContent() const;

    void * xmalloc(size_t size);
public:
    ELFMgr();
    ~ELFMgr();

    void allocSectHeaderTab(UINT shnum);

    void dump() const;
    void dumpStrTabContent(CHAR const* strtab, Addr size) const;

    //The function generate the content of string table by given string list.
    //It will compose string in 'strlst' into a char-vector, and record the
    //byte offset into 'offvec' for each string in 'charvec'.
    //charvec: record a char buffer.
    //offvec: record the byte offset.
    static void genStrTabContent(OUT CHARVec & charvec,
                                 OUT OffVec & offvec,
                                 StringList const& strlst);

    //Extract all section headers' string name and make up a string-table. 
    //charvec: the generated string table.
    //offvec: record the byte offset to each string name.
    void genSectHeaderNameStrTabContent(OUT CHARVec & charvec,
                                        OUT OffVec & offvec);

    //Compute and set the section index in ELFHdr according to the section
    //header pointer.
    void setSectHeaderNameStrTabIdx();
    CHAR const* getSectName(size_t idx) const;
    CHAR const* getSectName(ELFSHdr const* sh) const;
    size_t getSectHeaderIdx(ELFSHdr const* sh) const;
    ELFHdr & getHdr() { return m_elf_hdr; }
    ELFSHdr * getSectHeader(size_t idx) const;

    //Return the section content correspond to 'sh'.
    BYTE * getSectContent(ELFSHdr const* sh) const;

    //Return the section content that correspondint to section header 'idx'.
    BYTE * getSectContent(size_t idx) const;

    //Compute section actual size rely on symbol mapping table
    ULONG getSectActualSize(size_t sectidx) const;

    //Return the name of section that defined the symbol.
    CHAR const* getSymDefinedSectName(ELFSym const& sym) const;

    //Retriving string from symbol table which identified by 'symtab_header_idx'
    //via 'idx'.
    //symtab_header_idx: the index to symbol table section header.
    //idx: the element idx in symbol table.
    CHAR const* getStrFromSymTab(size_t symtab_header_idx, size_t idx) const;

    //Retriving string from symbol table or section name which defined the
    //symbol.
    CHAR const* getSymNameFromStrTabOrDefinedSection(
        ELFSym const& sym, ELFSHdr const* strtab) const;

    //Retriving string from symbol table which identified by 'symtab' via 'idx'.
    //symtab: the symbol table section header.
    //idx: the element idx in symbol table.
    CHAR const* getStrFromSymTab(ELFSHdr const* symtab, size_t idx) const;

    //Retriving string from string table via 'idx'
    //strtab: the string table section header.
    //idx: the byte offset to the begin of the string table content.
    CHAR const* getStrFromStrTab(ELFSHdr const* strtab, size_t idx) const;

    //Return ELF file type.
    CHAR const* getFileTypeName() const;

    //Return ELF file bit-width type.
    CHAR const* getClassName() const;

    //Return endian name.
    CHAR const* getEndianName() const;

    EM_STATUS initdumpfile(CHAR const* filename, bool is_del = false);
    EM_STATUS initdumpfile(FILE * filehandler);
    EM_STATUS initdumpscr();
    bool is64bit() const { return m_elf_hdr.is64bit(); }
    bool is32bit() const { return m_elf_hdr.is32bit(); }
    bool isExecutable() const;

    //Read the ELF information.
    //read_all_content: true to read section content for all section headers.
    //                  Note this may consume much of memory.
    EM_STATUS readELF(CHAR const* filename, bool read_all_content = false);
    EM_STATUS readAllSectContent();

    //Set section header content offset.
    //Note section content size should be ready.
    void setSectContentOffset();
    void setSectHeaderNameStrTabContent(BYTE * content, Addr size);
    void setSectHeaderNameStrTab(ELFSHdr * shdr)
    {
        ASSERT0(m_elf_shstrtab == nullptr);
        m_elf_shstrtab = shdr;
    }
    void setSectHeaderNameOffset(OffVec const& offvec);

    EM_STATUS writeELFHeader(OUT Word & elfhdr_offset);
    EM_STATUS writeProgramHeader();
    EM_STATUS writeSectHeaderTab();
    EM_STATUS writeSectContent();
    EM_STATUS writePad(size_t padsize);
    EM_STATUS writeELFHeaderAt(Word elfhdr_offset);
    EM_STATUS writeELF(CHAR const* filename);
};

}
#endif
