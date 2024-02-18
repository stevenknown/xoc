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

class SymbolLinkAttrFlag;
class SymbolInfo;
class FunctionInfo;

///////////////// Define some useful macros. /////////////////

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
#define RELA_DATA_NAME          ".rela.data"
#define RELA_SDATA_NAME         ".rela.sdata"
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
//The length of '.rela'.
#define PREFIX_RELA_TEXT_LEN 5
#define ELF_SYM_SH_ALIGN 8
#define ELF_VAL_UNDEF 0  //Common used zero.

///////////////// Define some useful types. /////////////////

class CHARVec : public xcom::Vector<BYTE> {
    COPY_CONSTRUCTOR(CHARVec);
public:
    CHARVec() {}
    CHARVec(UINT size) : xcom::Vector<BYTE>(size) {}
};

class OffVec : public xcom::Vector<Off> {
    COPY_CONSTRUCTOR(OffVec);
public:
    OffVec() {}
    OffVec(UINT size) : xcom::Vector<Off>(size) {}
};

class StringVec : public xcom::Vector<CHAR const*> {
    COPY_CONSTRUCTOR(StringVec);
public:
    StringVec() {}
    StringVec(UINT size) : xcom::Vector<CHAR const*>(size) {}
};

class BYTEVec : public xcom::Vector<BYTE> {
    COPY_CONSTRUCTOR(BYTEVec);
public:
    BYTEVec() {}
    BYTEVec(UINT size) : xcom::Vector<BYTE>(size) {}
};

class SWordVec : public xcom::Vector<SWord> {
    COPY_CONSTRUCTOR(SWordVec);
public:
    SWordVec() {}
    SWordVec(UINT size) : xcom::Vector<SWord>(size) {}
};

class WordVec : public xcom::Vector<Word> {
    COPY_CONSTRUCTOR(WordVec);
public:
    WordVec() {}
    WordVec(UINT size) : xcom::Vector<Word>(size) {}
};

class AddrVec : public xcom::Vector<Addr> {
    COPY_CONSTRUCTOR(AddrVec);
public:
    AddrVec() {}
    AddrVec(UINT size) : xcom::Vector<Addr>(size) {}
};

class SymVec : public xcom::Vector<Sym const*> {
    COPY_CONSTRUCTOR(SymVec);
public:
    SymVec() {}
    SymVec(UINT size) : xcom::Vector<Sym const*>(size) {}
};

//Record the link attribute of symbol for elf .symtab.
typedef xcom::TMap<Sym const*, SymbolLinkAttrFlag> SymbolLinkAttrMap;
typedef xcom::TMap<Sym const*, SymbolInfo*> SymbolInfoMap;
typedef xcom::TMapIter<Sym const*, SymbolInfo*> SymbolInfoIter;
typedef xcom::TMap<Sym const*, SymbolInfoMap*> SymtabInfoMap;
typedef xcom::TMapIter<Sym const*, SymbolInfoMap*> SymtabInfoIter;
typedef xcom::TMap<Sym const*, Vector<FunctionInfo*>*> FuncInfoMap;
typedef xcom::TMapIter<Sym const*, Vector<FunctionInfo*>*> FuncInfoIter;
typedef xcom::List<CHAR const*> StringList;

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

typedef enum _SECTION_NAME {
    SH_NAME_UNDEF = 0,
    SH_NAME_EMPTY = 1,
    SH_NAME_SHSTR,
    SH_NAME_SYMSTR,
    SH_NAME_TEXT,
    SH_NAME_SBSS,
    SH_NAME_SDATA,
    SH_NAME_BSS,
    SH_NAME_DATA,
    SH_NAME_SYMTAB,
    SH_NAME_SUBTEXT,
    SH_NAME_RELA,
    SH_NAME_CONST,
    SH_NAME_SPM,
    SH_NAME_GOT,
    SH_NAME_RELA_DYN,
    SH_NAME_DYNSYM,
    SH_NAME_TEXT1,
    SH_NAME_AITEXT,
    SH_NAME_FINI,
    SH_NAME_PREINIT_ARRAY,
    SH_NAME_DYNAMIC,
    SH_NAME_INTERP,
    SH_NAME_DL_TDATA,
    SH_NAME_RODATA,
    SH_NAME_COMPILER_VERSION,
    SH_NAME_MAX_NUM,
} SECTION_NAME;

typedef enum _PROGRAM_HEADER {
    PH_TYPE_UNDEF = 0,
    PH_TYPE_CODE,
    PH_TYPE_DATA,
    PH_TYPE_DYNAMIC,
} PROGRAM_HEADER;

///////////////// Define some useful structures. /////////////////

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

    ELFSymbolOff()
    {
        ::memset((void*)this, 0, sizeof(ELFSymbolOff));
    }
};


//Descript section configure table infomation. Since there are common
//attribute of all ELF sections, it will provide a configure table to
//descript these attribute. Then 'm_sect_desc_info' will be initialized
//according to the table infomation.
struct SectionNameDesc {
    //Record section name(enum type).
    SECTION_NAME desc_name;
    //Record section name(string type).
    CHAR const* desc_name_str;
    //Record program header type.
    PROGRAM_HEADER desc_ph_type;
    //Record section type(e.g. S_PROGBITS. Refer to 'struct ELFSHdr').
    Word32 desc_type;
    //Record section flags(e.g. W, A, X. Refer to 'struct ELFSHdr').
    Addr desc_flags;
    //Record section addr align(Refer to 'struct ELFSHdr').
    Addr desc_addr_align;
    //Record element size(Refer to 'struct ELFSHdr').
    Word desc_entry_size;
};

//Descript dynsym info.
typedef struct {
    Word dyn_index;               //Record the index of dynsym.
    Word dyn_ofst_in_got;         //Record the offfset in got.
    CHAR const * dyn_caller_func; //Record belong to which function.
} DynsymInfo;

///////////////// Define some useful classes. /////////////////


//
//Start SymbolLinkAttrFlag.
//
class SymbolLinkAttrFlag : public UFlag {
public:
    SymbolLinkAttrFlag() : UFlag(0) {}
    SymbolLinkAttrFlag(UINT v) : UFlag(v) {}
};


//
//Start ELFSectionInfo.
//
//Record section info when generating elf.
class ELFSectionInfo {
    COPY_CONSTRUCTOR(ELFSectionInfo);
private:
    //Record whether section exists.
    bool m_has_sbss;         //Save whether .sbss exists.
    bool m_has_sdata;        //Save whether .sdata exists.
    bool m_has_bss;          //Save whether .bss exists.
    bool m_has_data;         //Save whether .data exists.
    bool m_has_spm;          //Save whether .spm exists.
    bool m_has_const;        //Save whether .const exists.
    bool m_has_rela_sdata;   //Save whether .rela.sdata exists.
    bool m_has_rela_data;    //Save whether .rela.data exists.

    //Record section align.
    UINT m_sbss_align;       //Save align value of .sbss.
    UINT m_sdata_align;      //Save align value of .sdata.
    UINT m_bss_align;        //Save align value of .bss.
    UINT m_data_align;       //Save align value of .data.
    UINT m_spm_align;        //Save align value of .spm.
    UINT m_const_align;      //Save align value of .const.
    UINT m_rela_sdata_align; //Save align value of .rela.sdata.
    UINT m_rela_data_align;  //Save align value of .rela.data.
    UINT m_shdr_num;         //Save section numbers of elf file.

public:
    ELFSectionInfo();
    virtual ~ELFSectionInfo() {}

    UINT getBssAlign() const { return m_bss_align; }
    UINT getConstAlign() const { return m_const_align; }
    UINT getDataAlign() const { return m_data_align; }
    UINT getSbssAlign() const { return m_sbss_align; }
    UINT getSdataAlign() const { return m_sdata_align; }
    //Judge section type of given section.
    UINT getSectionAlign(SECTION_NAME sect);
    UINT getShdrNum() const { return m_shdr_num; }
    UINT getSpmAlign() const { return m_spm_align; }

    bool hasBss() const { return m_has_bss; }
    bool hasConst() const { return m_has_const; }
    bool hasData() const { return m_has_data; }
    bool hasRelaData() const { return m_has_rela_data; }
    bool hasRelaSdata() const { return m_has_rela_sdata; }
    bool hasSbss() const { return m_has_sbss; }
    bool hasSdata() const { return m_has_sdata; }
    bool hasSpm() const { return m_has_spm; }

    void setBss(bool v) { m_has_bss = v; }
    void setBssAlign(UINT v) { m_bss_align = v; }

    void setConst(bool v) { m_has_const = v; }
    void setConstAlign(UINT v) { m_const_align = v; }

    void setData(bool v) { m_has_data = v; }
    void setDataAlign(UINT v) { m_data_align = v; }

    void setRelaData(bool v) { m_has_rela_data = v; }
    void setRelaSdata(bool v) { m_has_rela_sdata = v; }

    void setSbss(bool v) { m_has_sbss = v; }
    void setSbssAlign(UINT v) { m_sbss_align = v; }

    void setSdata(bool v) { m_has_sdata = v; }
    void setSdataAlign(UINT v) { m_sdata_align = v; }

    void setShdrNum(UINT v) { m_shdr_num = v; }

    void setSpm(bool v) { m_has_spm = v; }
    void setSpmAlign(UINT v) { m_spm_align = v; }
};


//
//Start SectionInfo.
//
#define SECTINFO_name(sect) ((sect)->m_sect_name)
#define SECTINFO_ofst(sect) ((sect)->m_sect_offset)
#define SECTINFO_vec(sect)  ((sect)->m_vec)
//Descript section infomation which usd to construct ELF. These section info
//are generted according to the section configure table 'SectionNameDesc' if
//it need to create section. Then the map'm_sect_map' is used to manage all
//section of output ELF.
class SectionInfo {
    COPY_CONSTRUCTOR(SectionInfo);

public:
    //Record section name.
    SECTION_NAME m_sect_name;

    //Record the section belong to which program header type.
    PROGRAM_HEADER m_sect_ph_type;

    //Record section type.(e.g. S_PROGBITS. Refer to 'struct ELFSHdr').
    Word32 m_sect_type;

    //Record section flags.(e.g. W, A, X. Refer to 'struct ELFSHdr').
    Addr m_sect_flags;

    //Record section addr align.(Refer to 'struct ELFSHdr').
    Addr m_sect_addr_align;

    //Record the size of element that stored in section.
    //(Refer to 'struct ELFSHdr').
    Word m_sect_entry_size;

    //Record the offset addr in elf.
    Off m_sect_offset;

    //Record the virtual addr.
    Addr m_sect_base_addr;

    //Record content size.
    Word m_sect_size;

    //Record section string name.
    CHAR const* m_sect_name_str;

    //Record section content. There are two data type of the content.
    //BYTE type used for section that store byte data(e.g .text, .data).
    //CHAR type used for section that store string(e.g. .strtab and
    //.shstrtab). Since the size of CHAR type may be 1Byte or 2Byte, it
    //is different with BYTE type.
    union {
        //BYTE type content.
        BYTEVec * bytevec;
        //CHAR type content.
        CHARVec * charvec;
    } m_vec;

public:
    SectionInfo()
    {
        m_sect_name = SH_NAME_UNDEF;
        m_sect_ph_type = PH_TYPE_UNDEF;
        m_sect_type = S_UNDEF;
        m_sect_flags = S_UNDEF;
        m_sect_addr_align = 0;
        m_sect_entry_size = 0;
        m_sect_offset = 0;
        m_sect_base_addr = 0;
        m_sect_size = 0;
        m_sect_name_str = nullptr;
        m_vec.bytevec = nullptr;
    }

    ~SectionInfo()
    {
        if (m_vec.bytevec != nullptr) {
            delete m_vec.bytevec;
            m_vec.bytevec = nullptr;
        }
    }
};


//
//Start FunctionInfo.
//
#define FUNCINFO_name(func)      ((func)->m_func_name)
#define FUNCINFO_sect_name(func) ((func)->m_sect_name)
#define FUNCINFO_size(func)      ((func)->m_func_size)
#define FUNCINFO_is_entry(func)  ((func)->m_func_is_entry)
//Function info mainly hold code and relocation info. The info came from
//assembler or collected from other object ELF. Each file will generate a
//vector'm_func_info' to stored these info. The info use to construct code
//section(.text) and relocate.
class FunctionInfo {
    COPY_CONSTRUCTOR(FunctionInfo);

public:
    //Record whether it is kernel function.
    bool m_func_is_entry;

    //Record this function belong to which section.
    SECTION_NAME m_sect_name;

    //Record the size of code.
    Word m_func_size;

    //Record offset in corresponded code section.
    Off m_func_code_offset;

    //Record function name.
    Sym const* m_func_name;

    //Record corresponed file name.
    CHAR const* m_func_file_name;

    //Record the code.
    BYTEVec m_func_code;

public:
    FunctionInfo()
    {
        m_func_is_entry = false;
        m_sect_name = SH_NAME_UNDEF;
        m_func_size = 0;
        m_func_code_offset = 0;
        m_func_name = nullptr;
        m_func_file_name = nullptr;
    }

    ~FunctionInfo() {}

    //Return binary codes of current function symbol.
    BYTEVec & getCode() { return m_func_code; }
};


//
//Start RelocationInfo.
//
//Since there may be multiple relocation entries for a symbol, this class is
//used to store all relocation entries for a symbol.
//
//Save relocation symbol.
#define RELOCINFO_sym(reloc)    ((reloc)->m_sym_vec)
//Save relocation type.
#define RELOCINFO_type(reloc)   ((reloc)->m_type_vec)
//Save relocation offset.
#define RELOCINFO_offset(reloc) ((reloc)->m_offset_vec)
//Save relocation addend.
#define RELOCINFO_addend(reloc) ((reloc)->m_addend_vec)
class RelocationInfo {
    COPY_CONSTRUCTOR(RelocationInfo);
protected:
    UINT m_elem_count;     //Record the number of relocation entries.
public:
    SymVec m_sym_vec;      //Record names of all relocation symbols.
    WordVec m_type_vec;    //Record relocation types of all entries. Refer to
                           //the r_type attribute of class ELFRela defined in
                           //"/elf/elf_header.h".
    AddrVec m_offset_vec;  //Record relocation offsets of all entries. Refer to
                           //the r_offset attribute of class ELFRela defined in
                           //"/elf/elf_header.h".
    SWordVec m_addend_vec; //Record relocation addend values of all entries.
                           //Refer to the r_addend attribute of class ELFRela
                           //defined in "/elf/elf_header.h".

public:
    RelocationInfo()
    {
        m_sym_vec.init();
        m_type_vec.init();
        m_offset_vec.init();
        m_addend_vec.init();
        m_elem_count = 0;
    }
    virtual ~RelocationInfo()
    {
        m_sym_vec.clean();
        m_type_vec.clean();
        m_offset_vec.clean();
        m_addend_vec.init();
        m_elem_count = 0;
    }

    //Add basic info of relocation entry.
    //  sym:    Which symbol needs to be relocated to.
    //  type:   Relocation type.
    //  offset: Location where the relocation occurs.
    //  addend: Addend value of symbol relocated to.
    void addEntry(Sym const* sym, UINT type, UINT offset, UINT addend)
    {
        ASSERT0(sym);
        m_sym_vec.append(sym);
        m_type_vec.append((Word)type);
        m_offset_vec.append((Addr)offset);
        m_addend_vec.append((SWord)addend);

        m_elem_count++;
    }

    //Get number of relocation entries.
    UINT getElemCount() const { return m_elem_count; }
};


//
//Start SymbolInfo.
//
#define SYMINFO_name(sym)       ((sym)->m_sym_name)
#define SYMINFO_func_name(sym)  ((sym)->m_func_name)
#define SYMINFO_sect_name(sym)  ((sym)->m_sect_name)
#define SYMINFO_sym(sym)        ((sym)->m_sym_elfsym)
#define SYMINFO_size(sym)       ((sym)->m_sym_size)
#define SYMINFO_func(sym)       ((sym)->m_func_info)
#define SYMINFO_reloc(sym)      ((sym)->m_sym_reloc)
#define SYMINFO_index(sym)      ((sym)->m_sym_index)
#define SYMINFO_align(sym)      ((sym)->m_sym_align)
#define SYMINFO_data(sym)       ((sym)->m_sym_data)
#define SYMINFO_binword(sym)    ((sym)->m_sym_binword)
#define SYMINFO_off(sym)        ((sym)->m_sym_offset)
#define SYMINFO_is_func(sym)    ((sym)->m_sym_is_func)
#define SYMINFO_is_extern(sym)  ((sym)->m_sym_is_extern)
#define SYMINFO_is_weak(sym)    ((sym)->m_sym_is_weak)
#define SYMINFO_is_visible(sym) ((sym)->m_sym_is_visible)
#define SYMINFO_is_init(sym)    ((sym)->m_sym_is_init)
#define SYMINFO_is_global(sym)  ((sym)->m_sym_is_global)
//The class descript the basic info of a symbol. These info came from assembler
//or collected from .symtab of ELF. Symbol info may be 'handle' that control all
//procedures in generated section content and constructed ELF. It will generate
//Map'm_symbol_info' to stored all symbols of each file/ELF.
class SymbolInfo {
    COPY_CONSTRUCTOR(SymbolInfo);

public:
    //Record the data store into s_data.
    bool m_sym_is_byte;

    //Record whether it is .dynsym symbol.
    bool m_sym_is_dynsym;

    //Record whether it is functional symbol.
    bool m_sym_is_func;

    //Record whether it is external symbol.
    bool m_sym_is_extern;

    //Record whether it is weak symbol.
    bool m_sym_is_weak;

    //Record whether it is visible symbol.
    bool m_sym_is_visible;

    //Record whether it is initialized symbol.
    bool m_sym_is_init;

    //Record whether it is global symbol.
    bool m_sym_is_global;

    //Record the symbol belong to which section.
    SECTION_NAME m_sect_name;

    //Record current var align.
    Addr m_sym_align;

    //Record the symbol data size.
    Word m_sym_size;

    //Record the index in original file ELF symtab.
    Word m_sym_index;

    //Record the data of currrent symbol.
    //(e.g. INT/INT64 type).
    BinWord m_sym_binword;

    //Record the symbol offset in correspond section.
    Off m_sym_offset;

    //Record corresponded function name if symbol function.
    //This could be the same as sym_name.
    Sym const* m_func_name;

    //Record the name of symbol.
    Sym const* m_sym_name;

    //Record the name of file.
    CHAR const* m_sym_file_name;

    //Record the data of current symbol(e.g. string).
    BYTE * m_sym_data;

    //Record function info if it is function type.
    FunctionInfo m_func_info;

    //Record ELFSym.
    ELFSym m_sym_elfsym;

    //Record all relocation info of current symbol.
    RelocationInfo m_sym_reloc;

public:
    SymbolInfo()
    {
        m_sym_is_byte = false;
        m_sym_is_dynsym = false;
        m_sym_is_func = false;
        m_sym_is_extern = false;
        m_sym_is_weak = false;
        m_sym_is_visible = false;
        m_sym_is_init = false;
        m_sym_is_global = false;
        m_sect_name = SH_NAME_UNDEF;
        m_sym_align = 0;
        m_sym_size = 0;
        m_sym_index = 0;
        m_sym_binword = 0;
        m_sym_offset = 0;
        m_func_name = nullptr;
        m_sym_name = nullptr;
        m_sym_file_name = nullptr;
        m_sym_data = nullptr;
    }

    ~SymbolInfo() {}

    //Add relocation entry into m_sym_reloc.
    //  other:  Which symbol needs to be relocated to.
    //  type:   Relocation type.
    //  offset: Location where the relocation occurs.
    //  addend: Addend value of symbol relocated to.
    void addRelocation(Sym const* other, UINT type, UINT offset, UINT addend)
    {
        ASSERT0(other);
        m_sym_reloc.addEntry(other, type, offset, addend);
    }

    //Return binary codes of current symbol.
    BYTEVec & getSymbolCode() { return m_func_info.getCode(); }
};


//
//Start ELFMgr.
//
//Record symbols and their information.
#define ELFMGR_symbol_info(e) ((e)->m_symbol_info)
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

    //Region mgr.
    RegionMgr * m_rm;

    //Type mgr.
    TypeMgr * m_tm;

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

    //Record name of generated binary file.
    CHAR const* m_output_file_name;

    //Record symbol info collected.
    SymbolInfoMap m_symbol_info;
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

    //Based on the given symbol, add calling relocation information to it.
    //  current: On which symbol the relocation occurs.
    //  other:   Which symbol to relocate to.
    //  type:    Relocation type.
    //  offset:  Location where the relocation occurs on current.
    //  addend:  Addend value of symbol relocated to.
    void addCallRelocation(Sym const* current, Sym const* other, UINT type,
                           UINT offset, UINT addend)
    {
        ASSERT0(current && other && m_symbol_info.find(current));
        m_symbol_info.get(current)->addRelocation(other, type, offset, addend);
    }

    //Based on the given symbol, add reference relocation information to it.
    //  current: On which symbol the relocation occurs.
    //  other:   Which symbol to relocate to.
    //  offset:  Location where the relocation occurs on current.
    //  addend:  Addend value of symbol relocated to.
    void addReferRelocation(Sym const* current, Sym const* other, UINT offset,
                            UINT addend)
    {
        ASSERT0(current && other);
        if (!m_symbol_info.find(current)) {
            m_symbol_info.set(current, new SymbolInfo());
        }
        m_symbol_info.get(current)->addRelocation(other, getReferRelocType(),
                                                  offset, addend);
    }

    void allocSectHeaderTab(UINT shnum);

    //Assemble symbol to .data .sdata or .const section.
    void assembleVarToContent(OUT AssembleBinDescVec & content_desc_vec,
                              SymbolInfo const* sym_info);

    //Collect following info:
    //(1) Names(string) of all symbols.
    //(2) Section info.
    void collectELFFactor(OUT StringList & sym_name);

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

    //Construct .rela.data section.
    void constructELFRelaDataSection(MOD ELFSHdr * rela_data_shdr,
                                     BYTEVec & rela_data_data,
                                     ELFSHdr const* sym_shdr,
                                     ELFSHdr const* data_shdr);

    //Construct .rela.sdata section.
    void constructELFRelaSdataSection(MOD ELFSHdr * rela_sdata_shdr,
                                      BYTEVec & rela_sdata_data,
                                      ELFSHdr const* sym_shdr,
                                      ELFSHdr const* sdata_shdr);


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

    //Construct null symbol. It always be the first symbol in all symbols and
    //its values are all zero.
    void constructSymbolNull(OUT BYTEVec & bytevec);

    //Construct unull symbol using information of variables.
    void constructSymbolUnull(OUT BYTEVec & bytevec,
                              OffVec const& sym_str_off);

    void dump() const;
    void dumpStrTabContent(CHAR const* strtab, Addr size) const;

    //A helper function to extract info from abdv and save it into content.
    void extractAssBinDescVec(OUT BYTEVec & bytevec,
                              AssembleBinDescVec const& abdv);

    //Since user-defined functions have been saved, this interface only
    //collect un-user-defined symbols.
    void extractSymbolExceptUserDefFunc();

    //Generate contents for .sbss, .bss, .sdata, .data and .const sections.
    void genCommonSectionContent(OUT BYTEVec & sbss_content,
                                 OUT BYTEVec & sdata_content,
                                 OUT BYTEVec & bss_content,
                                 OUT BYTEVec & data_content,
                                 OUT BYTEVec & const_content);

    //Generate contents for .rel.text.xxx section.
    //bytevec: binary code of relocation info of function.
    //reloc:   saved all relocation entries for current symbol.
    void genRelocContent(OUT BYTEVec & bytevec, RelocationInfo const* reloc);

    //Generate contents for .rel.data section.
    void genRelocDataContent(OUT BYTEVec & bytevec);

    //Generate contents for .rel.sdata section.
    void genRelocSdataContent(OUT BYTEVec & bytevec);

    //Extract all section headers' string name and make up a string-table.
    //charvec: the generated string table.
    //offvec: record the byte offset to each string name.
    void genSectHeaderNameStrTabContent(OUT CHARVec & charvec,
                                        OUT OffVec & offvec);

    //Generate contents for .spm section.
    void genSpmContent(OUT BYTEVec & bytevec);

    //The function generate the content of string table by given string list.
    //It will compose string in 'strlst' into a char-vector, and record the
    //byte offset into 'offvec' for each string in 'charvec'.
    //charvec: record a char buffer.
    //offvec: record the byte offset.
    void genStrTabContent(OUT CHARVec & charvec, OUT OffVec & offvec,
                          StringList const& strlst);

    //Returns binary codes of given symbol.
    BYTEVec & getSymbolCode(Sym const* sym)
    {
        ASSERT0(sym && m_symbol_info.find(sym));
        return m_symbol_info.get(sym)->getSymbolCode();
    }

    //Generate contents for .symtab section.
    //bytevec: binary code of .symtab section content.
    //offvec: record the byte offset.
    void genSymTabContent(OUT BYTEVec & bytevec, OffVec const& offvec);

    //Return ELF file bit-width type.
    CHAR const* getClassName() const;

    //Return endian name.
    CHAR const* getEndianName() const;

    //Return ELF file type.
    CHAR const* getFileTypeName() const;

    ELFHdr & getHdr() { return m_elf_hdr; }

    //Get relocation type of reference relocation.
    virtual UINT getReferRelocType() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get region mgr.
    RegionMgr * getRegionMgr() const
    { ASSERT0(m_rm); return m_rm; }

    //Get type mgr.
    TypeMgr * getTypeMgr() const
    { ASSERT0(m_tm); return m_tm; }

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

    //Initialize the symbol information of a function type and save it to
    //m_symbol_info.
    void initSymFunc(xoc::Var const* var);

    bool is64bit() const { return m_elf_hdr.is64bit(); }
    bool is32bit() const { return m_elf_hdr.is32bit(); }

    bool isExecutable() const;

    //Whether variable is aligned by given value.
    bool isSizeAligned(UINT sz, UINT val) { return sz % val == 0; }

    //Judge whether size of variable is valid by less than or equal to judging
    //whether the value is less than or equal to BIN_WORD_SIZE;
    bool isSizeValid(UINT sz) { return sz <= BIN_WORD_SIZE; }

    //Whether current variable is user-defined variable.
    bool isUserDefinedFunction(xoc::Var const* var);

    //Whether info of var should be wrote into ELF file.
    bool isVarAvailable(xoc::Var const* var)
    {
        return var && (var->is_global() || var->is_func()) &&
            !var->is_fake() && !var->is_unallocable();
    }

    //Judge section type of symbols.
    SECTION_NAME judgeSymbolSection(xoc::Var const* var);

    //Generate contents for .text.xxx and .rel.text.xxx and construct them
    //using generated data.
    //symtab_shdr: .symtab section header
    //si: index of current section header in all section headers.
    void processELFTextRelSection(ELFSHdr const* symtab_shdr, OUT UINT & si);

    EM_STATUS readAllSectContent();
    //Read the ELF information.
    //read_all_content: true to read section content for all section headers.
    //                  Note this may consume much of memory.
    EM_STATUS readELF(CHAR const* filename, bool read_all_content = false);

    //Set region mgr.
    void setRegionMgr(RegionMgr * rm)
    {
        ASSERT0(rm);
        m_rm = rm;
        //init type mgr.
        m_tm = rm->getTypeMgr();
    }

    //Set section info based symbol information.
    void setSectionInfo(SymbolInfo const* sym_info);

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
                   OffVec const& sym_str_off, MOD SymbolInfo * sym_info);

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

protected:
    //Record the number of section header.
    UINT m_shdr_num;

    //Record the number of subtext.
    UINT m_subtext_num;

    //Record the begin index of global symbol in .symtab.
    UINT m_global_symbol_begin_index;

    //Generate contents for .const section.
    UINT64 m_got_elem_num;

    //Record the file name of ELFMgr.
    CHAR const* m_file_name;

    //Record function info collected from a ELFMgr.
    Vector<FunctionInfo*> m_func_info;

    //Record symbol offset in corresponded section.
    //e.g. ----------------------------------------
    //     .sbss sect:    var_4B, var_8B, var_32B
    //     ----------------------------------------
    //       .sbss_off:    0,      4,     12
    //     ----------------------------------------
    //     .sdata sect:   var_8B, var_32B, var_16B
    //     ----------------------------------------
    //       .sdata_off:   0,      8,     40
    //     ----------------------------------------
    ELFSymbolOff m_symbol_off;

    //Record all ELF section info.
    xcom::TMap<xcom::CHAR const*, SectionInfo*> m_sect_map;

    //Record section with order.
    //The order info according to getSectionIndex().
    xcom::TMap<UINT, SECTION_NAME> m_sect_layout;

    //Record all function info collected from multi-ELFMgr.
    FuncInfoMap m_file_func_info;

    //Record all symbol info collected from multi-ELFMgr.
    SymtabInfoMap m_symtab_info;

    //Record dynsym name.
    TMap<CHAR const*, DynsymInfo*, CompareStringFunc> m_dynsym_name;

    //Record section name description info.
    TMap<CHAR const*, SectionNameDesc const*,
         CompareStringFunc> m_sect_desc_info;

    //Record static library file relocated symbols.
    Stack<SymbolInfo*> m_static_lib_symbol_info_stack;

public:
    //Alloc program header.
    //Param phnum: the number of program header.
    void allocProgramHeader(UINT phnum)
    {
        //FIXME: Wait other part.
    }

    //Collected function/symbol info with ELFMgr format from specific
    //assembler format.
    void collectELFInfoFromASM(CHAR const* fn);

    //Collect function info from specific format that output by assembler.
    void collectFunctionInfoFromASM()
    {
        //FIXME: Wait other part.
    }

    //Collect the code, code size and offset info of function.
    //Param bytevec: record the binary code of function.
    //Param index: function index in all functions.
    void collectFunctionInfo(OUT BYTEVec * bytevec, MOD UINT & index)
    {
        //FIXME: Wait other pat.
    }

    //Collect function info if the symbol with FUNC type.
    //Param hdr: elf hdr.
    //Param symbol_info: symbol.
    void collectFuncInfoForSymbol(ELFHdr & hdr, SymbolInfo * symbol_info)
    {
        //FIXME: Wait other part.
    }

    //Collect symtab info from specific format that output
    //by assembler(e.g. var info).
    void collectSymtabInfoFromASM()
    {
        //FIXME: Wait other part.
    }

    //Collect symtab info from ELFMgr that have been read with
    //ELFHdr format(e.g. object(.o) file).
    //Param hdr: ELF header.
    //Param symtab_shdr: symtab setion header.
    void collectSymtabInfoFromELF(ELFHdr & hdr, ELFSHdr const* symtab_shdr)
    {
        //FIXME: Wait other part.
    }

    //Collect symbol info from specific format that output
    //by assembler(e.g. var info).
    //Param var: var info.
    //Param sym_info: ELF info.
    //Param func_ind: functional index.
    void collectSymbolInfo(xoc::Var const* var, MOD SymbolInfo * sym_info,
                           MOD UINT & func_ind)
    {
        //FIXME: Wait other part.
    }

    //Collect symbol and function info from ELF that have been read into
    //memory with SHdr format.
    void collectObjELF()
    {
        //FIXME: Wait other part.
    }

    //Collect section name which symbol info belong to.
    //Param hdr: ELF header.
    //Param symtab_shdr: symtab shdr.
    //Param symbol_info: symbol_info.
    void collectSymbolInfoSectionName(ELFHdr & hdr,
        MOD SymbolInfo * symbol_info)
    {
        //FIXME: Wait other part.
    }

    //Collect value which symbol info hold on.
    //Param symbol_info: symbol_info.
    void collectSymbolInfoValue(MOD SymbolInfo * symbol_info)
    {
        //FIXME: Wait other part.
    }

    //Collect function info from ELF that have been read into ELFHdr.
    //Param text_shdr: text shdr.
    //Param strtab_shdr: strtab shdr.
    FunctionInfo * collectTextInfoFromELF(ELFSHdr const* text_shdr,
        ELFSHdr const* strtab_shdr)
    {
        //FIXME: Wait other part.
        return nullptr;
    }

    //Collect rela function info from ELF that have been read into ELFHdr.
    //Param shdr: text shdr.
    //Param fi: function info.
    void collectRelaTextInfoFromELF(ELFSHdr const* shdr, OUT FunctionInfo * fi)
    {
        //FIXME: Wait other part.
    }

    //Collect symbol data from corresponded section.
    //Param size: data size.
    //Param byte: data vec.
    //Param bin_word: data.
    //Param is_byte; flag of data stored type.
    void collectSymbolData(xoc::Var const* var, OUT Word & size,
        OUT BYTE ** byte, OUT Word & bin_word, OUT bool & is_byte)
    {
        //FIXME: Wait other part.
    }

    //Collect SPM symbol data.
    //Param size: data size.
    //Param byte: data vec.
    //Param bin_word: data.
    //Param is_byte; flag of data stored type.
    void collectSpmSymbolData(xoc::Var const* var, OUT Word & size,
        OUT BYTE const* byte, OUT Word & bin_word, OUT bool & is_byte)
    {
        //FIXME: Wait other part.
    }

    //Construct null symbol. It always be the first symbol in all and its
    //values are all zero.
    //Param bytevec: symtab content buffer.
    //Param dynsym: dynsym content buffer.
    void constructSymbolNull(OUT BYTEVec * bytevec,
        OUT BYTEVec * dynsym_bytevec) {
        //FIXME: Wait other part.
    }

    //A helper function of construct ELF section.
    void constructELFSectionHelper();

    //Construct ELF section.
    void constructELFSection();

    //Copy symbol/function name from assembler var. Since the name belong
    //to 'RegionMgr' object, it need to create new memory in 'ELFMgr'.
    //Param: src str.
    //Result: dst str.
    CHAR const* copyNameStrFromVar(CHAR const* src_str);

    //Count reladyn element number.
    UINT countRelaDynElemNum() const
    {
        //FIXME: Wait other part.
        return 0;
    }

    //Create the element of dynamic section. The element is ELFDyn type.
    //Param desc_vec: content buffer.
    //Param tag: d_tag field value.
    //Param val: d_val field value.
    //Param ind: index.
    void createDynamicElement(OUT AssembleBinDescVec & desc_vec, SWord tag,
                              Addr val, UINT ind)
    {
        //FIXME: Wait other part.
    }

    //Construct unull symbol using info of global variables.
    //Param sym_bytevec: symtab content buffer.
    //Param dynsym_bytevec: dynsym content buffer.
    void constructSymbolUnull(OUT BYTEVec * sym_bytevec,
                              OUT BYTEVec * dynsym_bytevec)
    {
        //FIXME: Wait other part.
    }

    //Collect symtab/function info from static lib file.
    //Param file_name: the path of static lib file.
    //Param elf_mgr: the object of elf_mgr.
    void collectStaticLibFileInfo(CHAR const* file_name, ELFMgr * elf_mgr)
    {
        //FIXME: Wait other part.
    }

    //Free all resource.
    void destroy();

    //A helper function to extract info from abdv and save it into content.
    //Param bytevec: content buffer.
    //Param abdv: the binary value.
    void extractAssBinDescVec(OUT BYTEVec * bytevec,
                              AssembleBinDescVec const& abdv);

    //Generate section content helper function.
    void genSectionContentHelper();

    //Generate data section content.
    void genDataSectionContent()
    {
        //FIXME: Wait other part.
    }

    //Generate data section content for .bss section that don't contains data.
    //Param sect_name: section name.
    //Param sect_size: section size.
    void genDataSectionContentForBss(SECTION_NAME sect_name, UINT sect_size)
    {
        //FIXME: Wait other part.
    }

    //Generate data section content for .data section.
    //Param sect_name: section_name.
    //Param desc_vec: data binary buffer.
    void genDataSectionContentForData(SECTION_NAME sect_name,
        MOD AssembleBinDescVec & desc_vec)
    {
        //FIXME: Wait other part.
    }

    //Generate spm section content.
    //Param desc_vec: content buffer.
    //Param var: spm var.
    void genSpmContent(OUT AssembleBinDescVec & content_desc_vec,
                       xoc::Var const* var)
    {
        //FIXME: Wait other part.
    }

    //The function generate the content of string table by given string list.
    //It will compose string in 'strlst' into a char-vector, and record the
    //byte offset into 'offvec' for each string in 'charvec'.
    //charvec: record a char buffer.
    //offvec: record the byte offset.
    //strlst: string name list.
    void genStrTabContent(OUT CHARVec * charvec, OUT OffVec & offvec,
                          StringList const& strlst)
    { genStrTabContent((*charvec), offvec, strlst); }

    //The function generate the content of string table by m_symbol_info.
    //Param charvec: record a char buffer.
    void genStrTabContent(OUT CHARVec * charvec);

    //Generate contents for .symtab section.
    //bytevec: binary code of .symtab section content.
    void genSymTabContent(OUT BYTEVec * bytevec, OUT BYTEVec * dynsym_bytevec);

    //Extract all section headers' string name and make up a string-table.
    //charvec: the generated string table.
    //offvec: record the byte offset to each string name.
    void genSectHeaderNameStrTabContent(OUT CHARVec * charvec,
                                        OUT OffVec & offvec)
    { genSectHeaderNameStrTabContent(*charvec, offvec); }

    //Generate content for .rela_dyn section.
    virtual void genRelaDynContent()
    {
        //FIXME: Wait other part.
    }

    //Generate got content.
    void genGotContent()
    {
        //FIXME: Wait other part.
    }

    //Generate miscellaneous section content.
    virtual void genMiscSectionContent()
    { return; }

    //Generate section info object according to the section name.
    //Param sect_name: section name.
    //Return section info object.
    SectionInfo * genSectionInfo(SECTION_NAME sect_name);

    //Get the section name of functional Var.
    //Param var: functional var.
    //Return section name.
    virtual SECTION_NAME getFunctionalVarSectionName(xoc::Var const* var) const
    {
        ASSERT0(var);
        return SH_NAME_TEXT;
    }

    //Get the element byte size in got section. There may be
    //different size in different architecture.
    //Return size.
    virtual UINT getElemByteSizeInGotSection() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the length of instruction according to the different architecture.
    //Return instruction length.
    virtual UINT getInstLength() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get dynsym element number.
    //Return elemnt number.
    UINT getDynsymElemNum() const;

    //Get relocation addend value of relocation type for different arch.
    //Target dependent code.
    virtual UINT getRelocAddend(Word reloc_type)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the number of subtext(e.g. .text.xxx).
    UINT getSubTextNum() const { return m_subtext_num; }

    //Get section begin addr.
    //Param sect_name: section name.
    //Return addr.
    Addr getSectionAddr(SECTION_NAME sect_name) const;

    //Get the symtab(dynsym) index in .dynsymtab.
    UINT getDynsymIndex(CHAR const* sym_name) const;

    //Get the begin index of global symbol in .symtab.
    UINT getGlobalSymbolBeginIndex() const
    { return m_global_symbol_begin_index; }

    //Get section info.
    //Param sect_name: section name.
    //Return: sectioninfo.
    SectionInfo * getSection(SECTION_NAME sect_name) const;

    //Get section name description.
    //Different arch may own different section description.
    virtual SectionNameDesc const* getSectionNameDesc() const;

    //Get section name.
    //Param sect_name: section name(enum).
    xcom::CHAR const* getSectionName(SECTION_NAME sect_name) const;

    //Get section description info.
    //Param sect_name: section name.
    //Return section name desc.
    SectionNameDesc getSectionDesc(SECTION_NAME sect_name) const;

    //Get section BYTEVec content.
    //Param sect_name: section name.
    BYTEVec * getSectionContent(SECTION_NAME sect_name) const;

    //Get section CHARVec content.
    //Param sect_name: section name.
    CHARVec * getSectionCharVec(SECTION_NAME sect_name) const;

    //Get section order index. Differnet architure may have different section
    //layout(order). This function must be called after all sections have been
    //created by processSectionInfo() or processCode().
    //Param sect_name: section name.
    //Return section index in ELF.
    virtual UINT getSectionIndex(SECTION_NAME sect_name) const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get shdr number.
    UINT getShdrNum() const
    { return getSectionIndex(SH_NAME_MAX_NUM); }

    //Get section name by index.
    //Param index: index.
    //Return: section name.
    SECTION_NAME getSectionNameByIndex(UINT index) const
    { return m_sect_layout.get(index); }

    //Get section align.
    //Param sect_name: section name.
    UINT getSectionAlign(SECTION_NAME sect_name) const
    {
        ASSERT0(m_sect_map.find(getSectionName(sect_name)));
        SectionInfo * si = m_sect_map.get(getSectionName(sect_name));
        ASSERT0(si);
        return (UINT)si->m_sect_addr_align;
    }

    //Get got section element number.
    //Return: element number.
    UINT64 getGotElemNum() const { return m_got_elem_num; }

    //Get program header.
    //Param idx: program header index.
    //Return: PHdr.
    ELFPHdr * getProgramHeader(size_t idx) const;

    //Get the number of program header.
    virtual UINT getProgramHeaderNum() const
    { return 0; }

    //Get PHDR_CODE_ALIGN.
    virtual UINT getPhdrCodeAlign() const { return 0; }

    //Get PHDR_DATA_ALIGN.
    virtual UINT getPhdrDataAlign() const { return 0; }

    //Get PHDR_DYNAMIC_ALIGN.
    virtual UINT getPhdrDynamicAlign() const { return 0; }

    //Get SHDR_OFFSET_ALIGN.
    virtual UINT getShdrOffsetAlign() const { return 0; }

    //Get SHDR_VIRTUAL_ADDR_ALIGN.
    virtual UINT getShdrVirtualAddrAlign() const { return 0; }

    //Get SHDR_DYNAMIC_ALIGN.
    virtual UINT getShdrDynamicAlign() const { return 0; }

    //Get SHDR_GOT_ALIGN.
    virtual UINT getShdrGotAlign() const { return 0; }

    //Get Section number according to different arch.
    virtual UINT getSectionNum() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get definite symbol info from m_symtab_info.
    //Param symbol_name: symbol name.
    //Return symbol info.
    SymbolInfo * getDefiniteSymbolInfoFromSymtabInfo(
        CHAR const* symbol_name) const
    {
        ASSERT0(symbol_name);
        return getSymbolInfoFromSymtabInfo(symbol_name, true);
    }

    //Get symbol info from m_symtab_info.
    //Param symbol_name: symbol_name.
    //Param check_extern:
    //Return symbol info.
    SymbolInfo * getSymbolInfoFromSymtabInfo(CHAR const* symbol_name,
        bool check_extern = false) const
    {
        //FIXME: Wait other part.
        return nullptr;
    }

    //Check whether have specific section.
    //Param sect_name: section name.
    //Return: result.
    bool hasSection(SECTION_NAME sect_name) const
    { return (m_sect_map.find(getSectionName(sect_name))); }

    //Initiali section description info.
    virtual void initSectionDescInfo();

    //Whether current symbol is also dynsym too.
    //Param name: symbol name.
    bool isDynSym(CHAR const* name) const;

    //Whether current symbol is .rela.dyn symbol. There may be different
    //jugement in different arch.
    //Param name: symbol name.
    //Param reloc_type: reloc type.
    virtual bool isRelaDynSymbol(CHAR const* name, UINT reloc_type)
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    //Link static library file with main object file.
    //Param static_file_elf_mgr: the elf_mgr of static_file
    void linkStaticLibFile(ELFMgr * static_file_elf_mgr)
    {
        //FIXME: Wait other part.
    }

    //Merged param 'em' ELF info(collected from ELF file or ASM file) into
    //a processed ELFMgr. These info mainly include function info and symbol
    //info. Then all operations(generated data and symbol, relocated,
    //construct ELF section, etc) will get related info from this ELFMgr.
    //Param em: be merged elf mgr.
    //Return: result.
    bool mergeELF(ELFMgr * em)
    {
        //FIXME: Wait other part.
        return true;
    }

    //Output execute elf. It is entry function of process fatbin ELF.
    //(eg. It will be called if the command option is '-elf-fatbin' or
    //other option).
    bool outputExeELF();

    //Output device elf. It is entry function of process device ELF.
    //it will be called if the command option is '-elf-device'.
    bool outputDeviceELF();

    //Process program header.
    virtual void processProgramHeader()
    {
        //FIXME: Wait other part.
    }

    //Create section accoreding to the collected section info.
    virtual void processSectionInfo();

    //Process dynamic section.
    void processDynamicSection()
    {
        //FIXME: Wait other part.
    }

    //Process section virutal addr and offset in ELF.
    void processSectionAddrHelper();

    //Process the s_offset field of ELFSHdr type after setting the base addr
    //of section. s_offset field dedicated the section offset in ELF file.
    void processSectionOffset()
    {
        //FIXME: Wait other part.
    }

    //Process the s_addr field of ELFSHdr type after setting the base addr
    //of section. s_addr field is the virtual addr of section.
    void processSectionVirtualAddr()
    {
        //FIXME: Wait other part.
    }

    //Set function code into corresponded section content and update the
    //relocate offset after the function code offset modified. There may
    //be different code section type in different arch.
    virtual void processCode()
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //FIXME: Merge with other process output name function.
    //Process output name. Default name is 'mi.test.elf' if there isn't
    //output name that user provided.
    //Param output_name: output name.
    void processOutputName(CHAR const* output_name);

    //The entry function of relocate operation. When output fatbin ELF,
    //it need to relocate .text section to complete ld function. There may
    //be different relocated type and relocated operated in different arch.
    virtual void relocateTextSection()
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //Read section content.
    //Param sect_name: section name.
    //Param addr: target addr.
    //Return section content.
    Addr readSectionContent(SECTION_NAME sect_name, Addr addr) const;

    //Read obj file content.
    //Param fn: file name.
    //Param elf_mgr: elfmgr.
    bool readOBJ(CHAR const* fn, MOD ELFMgr * elf_mgr)
    {
        //FIXME: Wait other part.
        return true;
    }

    //Set the ELFHdr.e_type field.
    //Param elf_type: type.
    void setELFType(UINT elf_type);

    //Set global index in .symtab.
    //Param index: global index.
    void setGlobalSymbolBeginIndex(UINT index)
    { m_global_symbol_begin_index = index; }

    //Set section align.
    //Param sect_name: section name.
    //Param v: align.
    void setSectionAlign(SECTION_NAME sect_name, UINT v)
    {
        ASSERT0(m_sect_map.find(getSectionName(sect_name)));
        SectionInfo * si = getSection(sect_name);
        ASSERT0(si);
        si->m_sect_addr_align = v;
    }

    //Set section header number.
    //Param v: number.
    void setShdrNum(UINT v) { m_shdr_num = v; }

    //Set subText section number.
    //Param v: number.
    void setSubTextNum(UINT v) { m_subtext_num = v; }

    //Create section info.
    //Param sect_name: the section name.
    //Param name: section name(string type).
    void setSection(SECTION_NAME sect_name, CHAR const* name = nullptr);

    //Set the section order after all sections have been set.
    void setSectionOrder();

    //A helper function to set symbol fields using given values.
    //Param sym: ELFSym.
    //Param name: st_name field of ELFSym.
    //Param value: st_vale field of ELFSym.
    //Param size: st_size field of ELFSym.
    //Param type: st_type field of ELFSym.
    //Param bind: st_bind field of ELFSym.
    //Param other: st_other field of ELFSym.
    //Param shndex: st_shndx field of ELFSym.
    void setSymbolValue(MOD ELFSym * sym, Word name, Addr value, Addr size,
                        xcom::UCHAR type, xcom::UCHAR bind,
                        xcom::UCHAR other, Half shndx);

    //Set got elem number.
    //Param v: value.
    void setGotElemNum(UINT64 v) { m_got_elem_num = v; }

    //The relocate type will write into the r_type field of ELFRela
    //type(Ref:elf_header.h). Since the code of relocateion type may
    //be differnt between compiler and driver, it need to translate
    //compiler relocation type to corresponded driver relocation type.
    //Param type: relocate type.
    //Param driver relocate type.
    virtual UINT transferRelaDynRelocType(UINT type)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Update symtab/rela_dyn offset info after setting the
    //base addr of correspond section
    void updateSymbolOffsetAfterSetSectionBaseAddr()
    {
        //FIXME: Wait other part.
    }

};


//
//Start ELFOpt.
//
class ELFOpt {
public:
    //-elf-device option: Output relocatable file (ET_REL type) that need
    //to be relocated with other device file.
    bool is_device_elf;
    //-elf-fatbin option: Output shared object file (ET_DYN type) that direct
    //execute on device. It will linked multi-file and other external .so file.
    bool is_fatbin_elf;

public:
    ELFOpt()
    {
        //FIXME: Now default ELF output format is 'is_device_elf = true'.
        //Removed it after modified testcases CMakefile.
        is_device_elf = true;
        is_fatbin_elf = false;
    }

    bool isDeviceELF() const { return is_device_elf; }
    bool isFatbinELF() const { return is_fatbin_elf; }
};

extern ELFOpt g_elf_opt;

}
#endif
