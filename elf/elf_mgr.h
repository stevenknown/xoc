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

#define BASE_SEC_NUM      5  //Number of basic section header. Basic section
                             //header include .null, .text, .shdr_strtab,
                             //.symtab and .strtab.
#define ELF_VERSION       1  //.version field in ELF header.

//Record section names of each section.
#define CONST_SH_NAME           ".const"
#define BSS_SH_NAME             ".bss"
#define DATA_SH_NAME            ".data"
#define ELF_FILE_NAME           "mi.test.elf"
#define RELA_SH_NAME            ".rela.text."
#define RELA_KERNEL_SH_NAME     ".rela.aitext."
#define SBSS_SH_NAME            ".sbss"
#define SDATA_SH_NAME           ".sdata"
#define SHSTR_SH_NAME           ".shdr_strtab"
#define SPM_SH_NAME             ".spm"
#define SUBTEXT_SH_PRE          ".text."
#define SUBTEXT_ENTRY_SH_PRE    ".aitext."
#define SYMSTR_SH_NAME          ".strtab"
#define SYMTAB_SH_NAME          ".symtab"
#define TEXT_SH_NAME            ".text"
//Indicates the number of members contained in the ELFRela64 structure.
//Defined in "pcxac/elf/elf64.h":
//   typedef struct {
//       Addr64 r_offset;
//       Word32 r_type;    //describ the relocation type
//       Word32 r_sym;     //describ the symbol index
//       SWord64 r_addend;
//   } ELFRela64;
#define STRUCT_ELFRELA64_MEMBER_NUM  4

class SymbolLinkAttrFlag;

//Since one function region may call multiple symbols (including function and
//global variables), this structure established a mapping mechanism of each
//calling in program region.
typedef struct {
    CHAR const* caller; //Record name of caller.
    CHAR const* callee; //Record name of callee.
    UINT call_location; //Record the location of callee is called.
    UINT reloc_type;    //Record the relocation type.
} RELOCATION_INFO;

typedef xcom::Vector<CHAR> CHARVec;
typedef xcom::Vector<Off> OffVec;
typedef xcom::Vector<CHAR const*> StringVec;
typedef xcom::Vector<BYTE> BYTEVec;
typedef xcom::List<CHAR const*> StringList;
typedef xcom::List<ELFSHdr*> SHdrList;
typedef xcom::List<Var*> VarList;
//Record the link attribute of symbol for elf .symtab.
typedef xcom::TMap<xoc::Var const*, SymbolLinkAttrFlag> SymbolLinkAttrMap;
//Record number of BYTE of generated machine code of each function.
typedef xcom::Vector<UINT> FUNC_SIZE;
//Record relocation info in program region.
typedef xcom::Vector<RELOCATION_INFO> FUNC_RELOCATION;
//Record whether function is an entry function.
typedef xcom::TMap<UINT, bool> EntryFuncMap;
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

//The link attribute flag of symbol in generating elf .symtab.
typedef enum _SYMBOL_LINK_ATTR_FLAG {
    //Undefine value.
    SYMBOL_ATTR_UNDEF   = 0x0,
    //.weak modifier. Indicate the value of bind field in .symtab.
    SYMBOL_ATTR_WEAK    = 0x1,
    //.visible modifier. Indicate the value of visbility field in .symtab.
    SYMBOL_ATTR_VISIBLE = 0x2,
    //.extern modifier. Indicate the value of st_shndx/st_size field in .symtab.
    SYMBOL_ATTR_EXTERN  = 0x4,
} SYMBOL_LINK_ATTR_FLAG;

//This structure records the type of symbols in region.
typedef enum _SYMBOL_TYPE {
    SYMBOL_NOTYPE = 0, //Symbol of NOTYPE.
    SYMBOL_OBJECT,     //Symbol of type object.
    SYMBOL_FUNC,       //Symnol of type function.
} SYMBOL_TYPE;

//This structure records the addend value of different relocation types.
typedef enum _RELOC_ADDEND {
    RELOC_ADDEND_DEFAULT = 0x0, //Addend value of some relocation type:
                                //LITERAL, SPM_HIGH, SPM_LOW.
    RELOC_ADDEND_LITUSE = 0x3,  //Addend value of relocation type LITUSE.
    RELOC_ADDEND_GPDISP = 0x4,  //Addend value of relocation type GPDISP.
} RELOC_ADDEND;

//This structure store sections where symbols should be saved.
typedef enum _SYMBOL_SECTION {
    SYMBOL_DEFAULT = 0x0,
    SYMBOL_SBSS,  //symbols saving in .sbss section
    SYMBOL_SDATA, //symbols saving in .sdata section
    SYMBOL_BSS,   //symbols saving in .bss section
    SYMBOL_DATA,  //symbols saving in .data section
    SYMBOL_SPM,   //symbols saving in .spm section
    SYMBOL_CONST, //symbols saving in .const section
    SYMBOL_TEXT,  //symbols saving in .text.xxx section
} SYMBOL_SECTION;

class SymbolLinkAttrFlag : public UFlag {
public:
    SymbolLinkAttrFlag() : UFlag(0) {}
    SymbolLinkAttrFlag(UINT v) : UFlag(v) {}
};

//Record section info when generating elf.
class ELFSectionInfo {
private:
    //Record whether section exists.
    bool m_has_sbss;  //Save whether .sbss exists.
    bool m_has_sdata; //Save whether .sdata exists.
    bool m_has_bss;   //Save whether .bss exists.
    bool m_has_data;  //Save whether .data exists.
    bool m_has_spm;   //Save whether .spm exists.
    bool m_has_const; //Save whether .const exists.

    //Record section align.
    UINT m_sbss_align;  //Save align value of .sbss.
    UINT m_sdata_align; //Save align value of .sdata.
    UINT m_bss_align;   //Save align value of .bss.
    UINT m_data_align;  //Save align value of .data.
    UINT m_spm_align;   //Save align value of .spm.
    UINT m_const_align; //Save align value of .const.

    UINT m_shdr_num; //Save section numbers of elf file.

public:
    ELFSectionInfo();
    virtual ~ELFSectionInfo() {}

    UINT getBssAlign() const { return m_bss_align; }
    UINT getConstAlign() const { return m_const_align; }
    UINT getDataAlign() const { return m_data_align; }
    UINT getSbssAlign() const { return m_sbss_align; }
    UINT getSdataAlign() const { return m_sdata_align; }
    UINT getShdrNum() const { return m_shdr_num; }
    UINT getSpmAlign() const { return m_spm_align; }

    //Judge section type of given variable and return align value of this
    //section.
    UINT getVarAlign(SymbolLinkAttrMap const& symbol_link_attr_map,
                     Var const* var);

    bool hasBss() const { return m_has_bss; }
    bool hasConst() const { return m_has_const; }
    bool hasData() const { return m_has_data; }
    bool hasSbss() const { return m_has_sbss; }
    bool hasSdata() const { return m_has_sdata; }
    bool hasSpm() const { return m_has_spm; }

    void setBss(bool v) { m_has_bss = v; }
    void setBssAlign(UINT v) { m_bss_align = v; }

    void setConst(bool v) { m_has_const = v; }
    void setConstAlign(UINT v) { m_const_align = v; }

    void setData(bool v) { m_has_data = v; }
    void setDataAlign(UINT v) { m_data_align = v; }

    void setSbss(bool v) { m_has_sbss = v; }
    void setSbssAlign(UINT v) { m_sbss_align = v; }

    void setSdata(bool v) { m_has_sdata = v; }
    void setSdataAlign(UINT v) { m_sdata_align = v; }

    void setShdrNum(UINT v) { m_shdr_num = v; }

    void setSpm(bool v) { m_has_spm = v; }
    void setSpmAlign(UINT v) { m_spm_align = v; }
};

//Reference struct ELFSym64.
//Record the symbol position info when construct symbol table.
struct ELFSymbolOff {
    UINT var_name_off; //var name index.
    UINT text_ind;     //.text section index.
    UINT func_off;     //func symbol index.
    UINT sdata_off;    //.sdata section offset.
    UINT sbss_off;     //.sbss section offset.
    UINT bss_off;      //.bss section offset.
    UINT data_off;     //.data section offset.
    UINT spm_off;      //.spm section offset.
    UINT const_off;    //.const section offset.

    ELFSymbolOff() {
        ::memset((void*)this, 0, sizeof(ELFSymbolOff));
    }
};

//Record variables in program region need to be wrote into ELF file.
#define ELFMgr_saving_var_list(e) ((e)->m_saving_var_list)

//Record sizes of all functions.
#define ELFMgr_func_size(e) ((e)->m_func_size)

//Record codes generated in current program region.
#define ELFMgr_func_code(e) ((e)->m_func_code)

//Record relocation info in current program region.
#define ELFMgr_func_relocation(e) ((e)->m_func_relocation)
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

    //Record symbol link attr.
    SymbolLinkAttrMap m_symbol_link_attr_map;

    //Record section info for generating elf.
    ELFSectionInfo * m_sect_info;
public:
    ////Record variables in program region need to be wrote into ELF file.
    VarList m_saving_var_list;

    //Record sizes of all functions top-down.
    FUNC_SIZE m_func_size;

    //Record codes generated in current program region.
    BYTEVec m_func_code;

    //Record relocation info in current program region.
    FUNC_RELOCATION m_func_relocation;

    //Record name of generated binary file.
    CHAR const* m_output_file_name;
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
    virtual ~ELFMgr();

    void allocSectHeaderTab(UINT shnum);

    //Assemble symbol to .data .sdata or .const section.
    void assembleVarToContent(OUT AssembleBinDescVec & content_desc_vec,
                              Var const* var);

    //Collector some factors about .sbss, .sdata, .bss, .data, .spm
    //sections and section number of ELF file.
    //sym_name: save names of all symbols.
    //func_name: save names of all function.
    //entry_func_map: record whether function is an entry function.
    //sect_info: save some info of sections.
    void collectELFFactor(OUT StringList & sym_name, OUT StringVec & func_name,
                          EntryFuncMap & entry_func_map,
                          OUT ELFSectionInfo * sect_info);

    //Compute a vector saving relocation data index of each function region.
    void computeFuncRelocIndex(OUT xcom::Vector<UINT> & begin);

    //Construct .bss section.
    void constructELFBssSection(MOD ELFSHdr * bss_shdr, BYTEVec & bss,
                                UINT bss_align);

    //Construct .const section.
    void constructELFConstSection(MOD ELFSHdr * const_shdr,
                                  BYTEVec & const_data,
                                  UINT const_align);

    //Construct .data section.
    void constructELFDataSection(MOD ELFSHdr * data_shdr, BYTEVec & data,
                                 UINT data_align);

    //Construct .text.xxx section.
    void constructELFFuncSection(MOD ELFSHdr * func_shdr, BYTEVec & code,
                                 CHAR const* name, MOD BYTE * text_space);

    //Construct ELF header based on the section header number.
    void constructELFHeader(UINT sthr_num);

    //Construct null section header. It always be first section in all
    //sections and its values are all zero.
    void constructELFNullSection(MOD ELFSHdr * null_shdr);

    //Construct .rel.text.xxx section.
    void constructELFRelSection(MOD ELFSHdr * rel_shdr,
                                ELFSHdr const* sym_shdr,
                                ELFSHdr const* func_shdr, BYTEVec & rel,
                                CHAR const* name, MOD BYTE * rel_space);

    //Construct .sbss section.
    void constructELFSbssSection(MOD ELFSHdr * sbss_shdr, BYTEVec & sbss,
                                 UINT sbss_align);

    //Construct .sdata section.
    void constructELFSdataSection(MOD ELFSHdr * sdata_shdr, BYTEVec & sdata,
                                  UINT sdata_align);

    //Construct section header indexs.
    void constructELFShIndex(OUT CHARVec & charvec, OUT OffVec & offvec);

    //Construct .shdr_strtab section.
    void constructELFShStrSection(MOD ELFSHdr * shstr_shdr);

    //Construct .spm section.
    void constructELFSpmSection(MOD ELFSHdr * spm_shdr, BYTEVec & spm,
                                UINT spm_align);

    //Construct .symstr section.
    void constructELFStrTabSection(MOD ELFSHdr * symstr_shdr,
                                   CHARVec & sym_str);

    //Construct .symtab section.
    void constructELFSymTabSection(MOD ELFSHdr * symtab_shdr,
                                   ELFSHdr const* symstr_shdr,
                                   ELFSHdr const* text_shdr, BYTEVec & sym);

    //Construct .text section.
    void constructELFTextSection(MOD ELFSHdr * text_shdr);

    //A helper function to save rela into rel_desc_vec.
    //Since one function region may have multiple relocation entries, "index"
    //is used to determine where a relocation entry is written to rel_desc_vec.
    void constructRelAssBinDescVec(OUT AssembleBinDescVec & rel_desc_vec,
                                   ELFRela64 const& rela, UINT index);

    //Construct null symbol. It always be the first symbol in all symbols and
    //its values are all zero.
    void constructSymbolNull(OUT BYTEVec & bytevec);

    //Construct unull symbol using info of global variables.
    void constructSymbolUnull(OUT BYTEVec & bytevec, OffVec const& sym_str_off,
                              ELFSectionInfo const* sect_info);

    void dump() const;
    void dumpStrTabContent(CHAR const* strtab, Addr size) const;

    //A helper function to extract info from abdv and save it into content.
    void extractAssBinDescVec(OUT BYTEVec & bytevec,
                              AssembleBinDescVec const& abdv);

    //Since user-defined functions have been wirtten into m_saving_var_vec,
    //we need to extract available variables except them.
    void extractSavingVarExceptUserDefFunc();

    //Generate contents for .bss section.
    void genBssContent(OUT BYTEVec & bytevec);

    //Generate contents for .data section.
    void genDataContent(OUT BYTEVec & bytevec);

    //Generate contents for .spm section.
    void genSpmContent(OUT BYTEVec & bytevec);

    //Generate contents for .const section.
    void genConstContent(OUT BYTEVec & bytevec);

    //Generate contents for .rel.text.xxx section.
    //bytevec: binary code of relocation info of function.
    //names: names of all symbols to help to compute symbol index.
    //begin: relocation data begin index of function regions.
    //index: function index in all functions.
    void genFuncRelContent(OUT BYTEVec & bytevec, StringList const& names,
                           xcom::Vector<UINT> const& begin, UINT index);

    //Generate contents for .text.xxx section.
    //bytevec: binary code of binary code of function.
    //index: function index in all functions.
    void genFuncTextContent(OUT BYTEVec & bytevec, UINT index);

    //Get relocation addend value of relocation type for different arch.
    //Target dependent code.
    //index: index number of current relocation entry in in m_func_relocation.
    virtual UINT getRelocAddend(UINT index)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Generate contents for .sbss section.
    void genSbssContent(OUT BYTEVec & bytevec);

    //Generate contents for .sdata section.
    void genSdataContent(OUT BYTEVec & bytevec);

    //Extract all section headers' string name and make up a string-table.
    //charvec: the generated string table.
    //offvec: record the byte offset to each string name.
    void genSectHeaderNameStrTabContent(OUT CHARVec & charvec,
                                        OUT OffVec & offvec);

    //The function generate the content of string table by given string list.
    //It will compose string in 'strlst' into a char-vector, and record the
    //byte offset into 'offvec' for each string in 'charvec'.
    //charvec: record a char buffer.
    //offvec: record the byte offset.
    void genStrTabContent(OUT CHARVec & charvec, OUT OffVec & offvec,
                          StringList const& strlst);

    //Generate contents for .symtab section.
    //bytevec: binary code of .symtab section content.
    //offvec: record the byte offset.
    //sect_info: save some info of sections.
    void genSymTabContent(OUT BYTEVec & bytevec, OffVec const& offvec,
                          ELFSectionInfo const* sect_info);

    //Return ELF file bit-width type.
    CHAR const* getClassName() const;

    //Return endian name.
    CHAR const* getEndianName() const;

    //Return ELF file type.
    CHAR const* getFileTypeName() const;

    //Get relocation begin and end indexs of function region. These indexs
    //are used to extract relocation info.
    //begin_ind: begin index of relocation entry for current function.
    //end_ind: end index of relocation entry for current function.
    //begin: save relocation info indexs of each function region.
    //ind: index of function in all functions.
    virtual void getFuncRelocIndex(OUT UINT & begin_ind, OUT UINT & end_ind,
                                   xcom::Vector<UINT> const& begin,
                                   UINT ind);

    ELFHdr & getHdr() { return m_elf_hdr; }

    //Get architecture specific region.
    //Target dependent code.
    virtual Region * getRegion()
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Get relocation info based on given index.
    //offset: location that relocation need to be performed.
    //sym_ind: symbol index in all symbols.
    //type: relocation type.
    //addend: relocation addend value.
    //names: save names of all symbols.
    //index: index of current function in all functions.
    void getRelocInfo(OUT Addr64 & offset, OUT Word32 & sym_ind,
                      OUT Word32 & type, OUT SWord64 & addend,
                      StringList const& names, UINT index);

    //Compute section actual size rely on symbol mapping table
    ULONG getSectActualSize(size_t sectidx) const;

    //Return the section content correspond to 'sh'.
    BYTE * getSectContent(ELFSHdr const* sh) const;

    //Return the section content that correspondint to section header 'idx'.
    BYTE * getSectContent(size_t idx) const;

    ELFSHdr * getSectHeader(size_t idx) const;
    size_t getSectHeaderIdx(ELFSHdr const* sh) const;
    CHAR const* getSectName(size_t idx) const;
    CHAR const* getSectName(ELFSHdr const* sh) const;

    //Get spm section name.
    virtual CHAR const* getSpmSHName() const { return SPM_SH_NAME; }

    //Retriving string from symbol table which identified by 'symtab_header_idx'
    //via 'idx'.
    //symtab_header_idx: the index to symbol table section header.
    //idx: the element idx in symbol table.
    CHAR const* getStrFromSymTab(size_t symtab_header_idx, size_t idx) const;

    //Retriving string from symbol table which identified by 'symtab' via 'idx'.
    //symtab: the symbol table section header.
    //idx: the element idx in symbol table.
    CHAR const* getStrFromSymTab(ELFSHdr const* symtab, size_t idx) const;

    //Retriving string from string table via 'idx'
    //strtab: the string table section header.
    //idx: the byte offset to the begin of the string table content.
    CHAR const* getStrFromStrTab(ELFSHdr const* strtab, size_t idx) const;

    //Return the name of section that defined the symbol.
    CHAR const* getSymDefinedSectName(ELFSym const& sym) const;

    //Retriving string from symbol table or section name which defined the
    //symbol.
    CHAR const* getSymNameFromStrTabOrDefinedSection(
        ELFSym const& sym, ELFSHdr const* strtab) const;

    //Get some other info of symbol for different architecture.
    //Target dependent code.
    virtual UINT getSymOtherInfo()
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get align value of .text section.
    virtual Addr getTextSectAlign()
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    EM_STATUS initdumpfile(CHAR const* filename, bool is_del = false);
    EM_STATUS initdumpfile(FILE * filehandler);
    EM_STATUS initdumpscr();

    bool is64bit() const { return m_elf_hdr.is64bit(); }
    bool is32bit() const { return m_elf_hdr.is32bit(); }

    bool isExecutable() const;

    //Whether variable is aligned by given value.
    bool isSizeAligned(UINT sz, UINT val) { return sz % val == 0; }

    //Judge whether size of variable is valid by less than or equal to judging
    //whether the value is less than or equal to BIN_WORD_SIZE;
    bool isSizeValid(UINT sz) { return sz <= BIN_WORD_SIZE; }

    //Whether current variable is user-defined variable.
    bool isUserDefinedFunction(Var const* var);

    //Whether info of var should be wrote into ELF file.
    bool isVarAvailable(Var const* var)
    {
        return var && (var->is_global() || var->is_func()) &&
            !var->is_fake() && !var->is_unallocable();
    }

    //Generate contents for .text.xxx and .rel.text.xxx and construct them
    //using generated data.
    //symtab_shdr: .symtab section header
    //func_name: save names of all functions.
    //entry_func_map: record whether function is an entry function.
    //sym_name: save names of all symbols.
    //si: index of current section header in all section headers.
    void processELFTextRelSection(ELFSHdr const* symtab_shdr,
                                  StringVec const& func_name,
                                  EntryFuncMap const& entry_func_map,
                                  StringList const& sym_name, OUT UINT & si);

    EM_STATUS readAllSectContent();
    //Read the ELF information.
    //read_all_content: true to read section content for all section headers.
    //                  Note this may consume much of memory.
    EM_STATUS readELF(CHAR const* filename, bool read_all_content = false);

    //Set name of generated binary file.
    void setOutputFileName(CHAR const* name)
    {
        m_output_file_name = (CHAR*)xmalloc(::strlen(name) + 1);
        ::memcpy((void*)m_output_file_name, name, ::strlen(name) + 1);
    }

    //Set section header content offset.
    //Note section content size should be ready.
    void setSectContentOffset();
    void setSectHeaderNameOffset(OffVec const& offvec);
    void setSectHeaderNameStrTab(ELFSHdr * shdr)
    {
        ASSERT0(m_elf_shstrtab == nullptr);
        m_elf_shstrtab = shdr;
    }
    void setSectHeaderNameStrTabContent(BYTE * content, Addr size);

    //Compute and set the section index in ELFHdr according to the section
    //header pointer.
    void setSectHeaderNameStrTabIdx();

    //Set symbol info.
    void setSymbol(MOD BYTE * sym, MOD ELFSymbolOff & symbol_off,
                   Var const* var, ELFSectionInfo const* sect_info,
                   Word const& name, UCHAR const& bind, UCHAR const& other,
                   Addr const& size);

    //Set symbol link attribute into attr table.
    void setSymbolLinkAttr(xoc::Var const* var, SYMBOL_LINK_ATTR_FLAG sym_attr);

    //A helper function to set symbol fields using given values.
    void setSymbolValue(BYTE const* sym, Word st_name,
                        xcom::UCHAR st_bind, xcom::UCHAR st_type,
                        xcom::UCHAR st_other, Half st_shndx,
                        Addr st_value, Addr st_size);

    EM_STATUS writeELF(CHAR const* filename);
    EM_STATUS writeELFHeader(OUT Word & elfhdr_offset);
    EM_STATUS writeELFHeaderAt(Word elfhdr_offset);
    EM_STATUS writePad(size_t padsize);
    EM_STATUS writeProgramHeader();
    EM_STATUS writeSectContent();
    EM_STATUS writeSectHeaderTab();

    //An common used ELF generation process for different architectures,
    //different architectures only need to implement the info collection
    //functions.
    void write2ELF();
};

}
#endif
