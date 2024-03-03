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

class SymbolInfo;
class FunctionInfo;

#define ELF_SIZE_4GB            0xFFFFffff
//Number of basic section header. Basic section header include
//.null, .text, .shdr_strtab, .symtab and .strtab.
#define BASE_SEC_NUM            5
//.version field in ELF header.
#define ELF_VERSION             1
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
#define SUBTEXT1_SH_PRE         ".text1."
#define SUBTEXT_ENTRY_SH_PRE    ".aitext."
#define SYMSTR_SH_NAME          ".strtab"
#define SYMTAB_SH_NAME          ".symtab"
#define TEXT_SH_NAME            ".text"
//For program header.
#define PHDR_CODE_ALIGN         0x10000
#define PHDR_DATA_ALIGN         0x10000
#define PHDR_DYNAMIC_ALIGN      0x8
#define PHDR_NUMBER             3
//For section header.
#define SHDR_OFFSET_ALIGN       0x10000
#define SHDR_VIRTUAL_ADDR_ALIGN 0x10000
#define SHDR_DYNAMIC_ALIGN      0x8
#define SHDR_GOT_ALIGN          0x10
#define SHDR_ALIGN_1B           1
#define SHDR_TEXT_CODE_ALIGN    1
#define SHDR_SYM_BYTE           24
#define SHDR_SYM_ALIGN          8
//Common used zero.
#define ELF_VAL_UNDEF           0
//The offset of st_shndx field in ELFSym.
#define ELFSYM32_ST_SHNDX_OFFSET_BYTE     14
#define ELFSYM64_ST_SHNDX_OFFSET_BYTE     6
//The offset of st_value field in ELFSym.
#define ELFSYM32_ST_VALUE_OFFSET_BYTE     4
#define ELFSYM64_ST_VALUE_OFFSET_BYTE     8
//The offset of st_other field in ELFSym.
#define ELFSYM32_ST_OTHER_OFFSET_BYTE     13
#define ELFSYM64_ST_OTHER_OFFSET_BYTE     5
//The first byte is '\0' in .strtab/.shstrtab/.dynstr and
//other section that records string in ELF format. Thus the
//first valid byte index is begin from 1.
#define ELF_STR_ELEM_BEGIN_INDEX          1


typedef xcom::Vector<CHAR> CHARVec;
typedef xcom::Vector<Off> OffVec;
typedef xcom::Vector<CHAR const*> StringVec;
typedef xcom::Vector<BYTE> BYTEVec;
typedef xcom::List<CHAR const*> StringList;
typedef xcom::Vector<SWord> SWordVec;
typedef xcom::Vector<Word> WordVec;
typedef xcom::Vector<Addr> AddrVec;
typedef xcom::Vector<Sym const*> SymVec;
typedef xcom::TMap<Sym const*, SymbolInfo*> SymbolInfoMap;
typedef xcom::TMapIter<Sym const*, SymbolInfo*> SymbolInfoIter;
typedef xcom::TMap<Sym const*, SymbolInfoMap*> SymtabInfoMap;
typedef xcom::TMapIter<Sym const*, SymbolInfoMap*> SymtabInfoIter;
typedef xcom::TMap<Sym const*, Vector<FunctionInfo*>*> FuncInfoMap;
typedef xcom::TMapIter<Sym const*, Vector<FunctionInfo*>*> FuncInfoIter;
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
    SH_NAME_RODATA1,
    SH_NAME_LDM,
    SH_NAME_COMPILER_VERSION,
    SH_NAME_DEBUG_INFO,
    SH_NAME_DEBUG_LINE,
    SH_NAME_DEBUG_ABBREV,
    SH_NAME_DEBUG_ARANGES,
    SH_NAME_EH_FRAME,
    SH_NAME_COMMENT,
    SH_NAME_NOTE, //'.note.GNU-stack'
    SH_NAME_MAX_NUM,
} SECTION_NAME;


#define SWITCH_CASE_COMMON_SECT      \
    case SH_NAME_TEXT:               \
    case SH_NAME_SBSS:               \
    case SH_NAME_SDATA:              \
    case SH_NAME_BSS:                \
    case SH_NAME_DATA:               \
    case SH_NAME_SUBTEXT:            \
    case SH_NAME_RELA:               \
    case SH_NAME_CONST:              \
    case SH_NAME_SPM:                \
    case SH_NAME_GOT:                \
    case SH_NAME_RELA_DYN:           \
    case SH_NAME_DYNSYM:             \
    case SH_NAME_TEXT1:              \
    case SH_NAME_AITEXT:             \
    case SH_NAME_FINI:               \
    case SH_NAME_DYNAMIC:            \
    case SH_NAME_DL_TDATA:           \
    case SH_NAME_RODATA:             \
    case SH_NAME_RODATA1:            \
    case SH_NAME_LDM:                \
    case SH_NAME_COMPILER_VERSION:   \
    case SH_NAME_DEBUG_INFO:         \
    case SH_NAME_DEBUG_LINE:         \
    case SH_NAME_DEBUG_ABBREV:       \
    case SH_NAME_DEBUG_ARANGES:      \
    case SH_NAME_EH_FRAME:           \
    case SH_NAME_COMMENT:            \
    case SH_NAME_NOTE


#define SWITCH_CASE_COMMON_SECT_OFST \
    SWITCH_CASE_COMMON_SECT:         \
    case SH_NAME_SHSTR:              \
    case SH_NAME_SYMSTR:             \
    case SH_NAME_SYMTAB


#define SWITCH_CASE_COMMON_SECT_ADDR \
    SWITCH_CASE_COMMON_SECT:         \
    case SH_NAME_INTERP


#define SWITCH_CASE_SECT_BYTE_TYPE \
    SWITCH_CASE_COMMON_SECT:       \
    case SH_NAME_UNDEF:            \
    case SH_NAME_SYMTAB:           \
    case SH_NAME_INTERP:           \
    case SH_NAME_PREINIT_ARRAY


typedef enum _PROGRAM_HEADER {
    PH_TYPE_UNDEF = 0,
    PH_TYPE_CODE,
    PH_TYPE_DATA,
    PH_TYPE_DYNAMIC,
} PROGRAM_HEADER;


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


#define SECTDESC_name(v)     ((v)->m_desc_name)
#define SECTDESC_type(v)     ((v)->m_desc_type)
#define SECTDESC_ph_type(v)  ((v)->m_desc_ph_type)
#define SECTDESC_flags(v)    ((v)->m_desc_flags)
#define SECTDESC_align(v)    ((v)->m_desc_addr_align)
#define SECTDESC_entry_sz(v) ((v)->m_desc_entry_size)
#define SECTDESC_name_str(v) ((v)->m_desc_name_str)
//Descript section configure table infomation. Since there are common
//attribute of all ELF sections, it will provide a configure table to
//descript these attribute. Then 'm_sect_desc_info' will be initialized
//according to the table infomation. These info are referenced from 'ELFHdr'.
struct SectionNameDesc {
    //Record section name(enum type).
    SECTION_NAME m_desc_name;

    //Record section type(e.g., S_PROGBITS).
    Word32 m_desc_type;

    //Record program header type.
    PROGRAM_HEADER m_desc_ph_type;

    //Record section flags(e.g., W(write), A(alloc)).
    Addr m_desc_flags;

    //Record section addr align.
    Addr m_desc_addr_align;

    //Record element size.
    Word m_desc_entry_size;

    //Record section name(string type).
    CHAR const* m_desc_name_str;
};


//Descript dynsym info.
typedef struct {
    Word dyn_index;               //Record the index of dynsym.
    Word dyn_ofst_in_got;         //Record the offfset in got.
    CHAR const * dyn_caller_func; //Record belong to which function.
} DynsymInfo;


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
#define SECTINFO_name(v)       ((v)->m_sect_name)
#define SECTINFO_name_str(v)   ((v)->m_sect_name_str)
#define SECTINFO_ofst(v)       ((v)->m_sect_offset)
#define SECTINFO_index(v)      ((v)->m_sect_index)
#define SECTINFO_align(v)      ((v)->m_sect_addr_align)
#define SECTINFO_flag(v)       ((v)->m_sect_flags)
#define SECTINFO_addr(v)       ((v)->m_sect_base_addr)
#define SECTINFO_size(v)       ((v)->m_sect_size)
#define SECTINFO_entry_size(v) ((v)->m_sect_entry_size)
#define SECTINFO_type(v)       ((v)->m_sect_type)
#define SECTINFO_ph_type(v)    ((v)->m_sect_ph_type)
#define SECTINFO_bytevec(v)    (((v)->m_vec).bytevec)
#define SECTINFO_charvec(v)    (((v)->m_vec).charvec)

//Descript section infomation which usd to construct ELF. These section info
//are generted according to the section configure table 'SectionNameDesc' if
//it need to create section. Then the map'm_sect_map' is used to manage all
//section of output ELF.
class SectionInfo {
    COPY_CONSTRUCTOR(SectionInfo);
public:
    //Record section name.
    SECTION_NAME m_sect_name;

    //Record program header type to which the section belong.
    PROGRAM_HEADER m_sect_ph_type;

    //Record section type(e.g., S_PROGBITS).
    Word32 m_sect_type;

    //Record section index.
    Word m_sect_index;

    //Record section flags(e.g., W, A, X, etc. Refer to 'ELFSHdr').
    Addr m_sect_flags;

    //Record section addr align.
    Addr m_sect_addr_align;

    //Record the size of element that stored in section.
    Word m_sect_entry_size;

    //Record the offset in ELF.
    Off m_sect_offset;

    //Record the address.
    Addr m_sect_base_addr;

    //Record content size.
    Word m_sect_size;

    //Record section string name.
    CHAR const* m_sect_name_str;

    //Record section content. There are two data type of section content.
    //BYTE type is used for section that record byte data(e.g., .text, .data).
    //CHAR type is used for section that record string(e.g., .strtab,
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
        m_sect_index = 0;
        m_sect_flags = S_UNDEF;
        m_sect_addr_align = 0;
        m_sect_entry_size = 0;
        m_sect_offset = 0;
        m_sect_base_addr = 0;
        m_sect_size = 0;
        m_sect_name_str = nullptr;
        m_vec.bytevec = nullptr;
        m_vec.charvec = nullptr;
    }

    ~SectionInfo() {}
};


//The class manages SectionInfo object resources. It creates SectionInfo
//object and uses xoc::List 'm_list' to record. These resources would be
//freed when the destructor is called.
class SectionInfoMgr {
    COPY_CONSTRUCTOR(SectionInfoMgr);

    //Record SectionInfo object.
    xcom::List<SectionInfo*> m_list;
    //Record BYTEVec object.
    xcom::List<BYTEVec*> m_bytevec_list;
    //Record CHARVec object.
    xcom::List<CHARVec*> m_charvec_list;
public:
    SectionInfoMgr()
    {
        m_list.init();
        m_bytevec_list.init();
        m_charvec_list.init();
    }

    ~SectionInfoMgr()
    {
        for (SectionInfo * si = m_list.get_head();
             si != nullptr; si = m_list.get_next()) {
            if (si != nullptr) { delete si; }
        }

        for (BYTEVec * bv = m_bytevec_list.get_head();
             bv != nullptr; bv = m_bytevec_list.get_next()) {
            if (bv != nullptr) { delete bv; }
        }

        for (CHARVec * cv = m_charvec_list.get_head();
             cv != nullptr; cv = m_charvec_list.get_next()) {
            if (cv != nullptr) { delete cv; }
        }

        m_list.clean();
        m_bytevec_list.clean();
        m_charvec_list.clean();
    }

    SectionInfo * allocSectionInfo(SECTION_NAME sect_name)
    {
        SectionInfo * si = new SectionInfo();
        ASSERT0(si);
        //Allocate charvec or bytevec according to the section name.
        switch (sect_name) {
        case SH_NAME_SYMSTR:
        case SH_NAME_SHSTR:
            SECTINFO_charvec(si) = new CHARVec();
            m_charvec_list.append_tail(SECTINFO_charvec(si));
            break;
        SWITCH_CASE_SECT_BYTE_TYPE:
            SECTINFO_bytevec(si) = new BYTEVec();
            m_bytevec_list.append_tail(SECTINFO_bytevec(si));
            break;
        default:
            UNREACHABLE();
            break;
        }

        m_list.append_tail(si);
        return si;
    }
};


//
//Start FunctionInfo.
//
#define FUNCINFO_name(v)          ((v)->m_func_name)
#define FUNCINFO_sect_name(v)     ((v)->m_sect_name)
#define FUNCINFO_sect_name_str(v) ((v)->m_sect_name_str)
#define FUNCINFO_size(v)          ((v)->m_func_size)
#define FUNCINFO_is_entry(v)      ((v)->m_func_is_entry)
#define FUNCINFO_align(v)         ((v)->m_func_align)
#define FUNCINFO_file_name(v)     ((v)->m_func_file_name)
#define FUNCINFO_code(v)          ((v)->m_func_code)
#define FUNCINFO_code_ofst(v)     ((v)->m_func_code_offset)
//FunctionInfo mainly records code and relocation info. These info came
//from xoc::Var or collected from other object ELF. These info are used
//to construct code section(.text) and relocate RelocInfo.
class FunctionInfo {
    COPY_CONSTRUCTOR(FunctionInfo);
public:
    //Record whether it is kernel function.
    bool m_func_is_entry;

    //Record section name to which the function belong.
    SECTION_NAME m_sect_name;

    //Record current function code align.
    UINT m_func_align;

    //Record the size of code.
    Word m_func_size;

    //Record offset in corresponded code section.
    Off m_func_code_offset;

    //Record function name.
    Sym const* m_func_name;

    //Record file name.
    CHAR const* m_func_file_name;

    //Record section string name.
    CHAR const* m_sect_name_str;

    //Record the code.
    BYTEVec m_func_code;

    FunctionInfo(Addr size)
    {
        m_func_is_entry = false;
        m_sect_name = SH_NAME_UNDEF;
        m_func_align = 0;
        m_func_size = 0;
        m_func_code_offset = 0;
        m_func_name = nullptr;
        m_func_file_name = nullptr;
        m_sect_name_str = nullptr;
        m_func_code.init((UINT)size);
    }

    ~FunctionInfo() {}

    //Return binary codes of current function symbol.
    BYTEVec & getCode() { return m_func_code; }
};


//The class manages FunctionInfo object resources. It creates FunctionInfo
//object and uses xoc::List 'm_list' to record. These resources would be
//freed when the destructor is called.
class FunctionInfoMgr {
    COPY_CONSTRUCTOR(FunctionInfoMgr);

    //Record function info object.
    xcom::List<FunctionInfo*> m_list;
public:
    FunctionInfoMgr() { m_list.init(); }

    ~FunctionInfoMgr()
    {
        for (FunctionInfo * fi = m_list.get_head();
             fi != nullptr; fi = m_list.get_next()) {
            if (fi != nullptr) { delete fi; }
        }
        m_list.clean();
    }

    FunctionInfo * allocFunctionInfo(Addr size = 0)
    {
        FunctionInfo * fi = new FunctionInfo(size);
        ASSERT0(fi);
        m_list.append_tail(fi);
        return fi;
    }
};


//
//Start RelocationInfo.
//
//Since there may be multiple relocation entries for a symbol, this class is
//used to store all relocation entries for a symbol.
//
//Save relocation symbol.
#define RELOCATIONINFO_sym(reloc)    ((reloc)->m_sym_vec)
//Save relocation type.
#define RELOCATIONINFO_type(reloc)   ((reloc)->m_type_vec)
//Save relocation offset.
#define RELOCATIONINFO_offset(reloc) ((reloc)->m_offset_vec)
//Save relocation addend.
#define RELOCATIONINFO_addend(reloc) ((reloc)->m_addend_vec)
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
//Start SymbolInfo
//
#define SYMINFO_name(v)          ((v)->m_sym_name)
#define SYMINFO_sect_name(v)     ((v)->m_sect_name)
#define SYMINFO_sect_name_str(v) ((v)->m_sect_name_str)
#define SYMINFO_sym(v)           ((v)->m_sym_elfsym)
#define SYMINFO_index(v)         ((v)->m_sym_index)
#define SYMINFO_size(v)          ((v)->m_sym_size)
#define SYMINFO_func(v)          ((v)->m_func_info)
#define SYMINFO_reloc(v)         ((v)->m_sym_reloc)
#define SYMINFO_func_name(v)     ((v)->m_func_name)
#define SYMINFO_is_func(v)       ((v)->m_sym_is_func)
#define SYMINFO_is_extern(v)     ((v)->m_sym_is_extern)
#define SYMINFO_is_weak(v)       ((v)->m_sym_is_weak)
#define SYMINFO_is_visible(v)    ((v)->m_sym_is_visible)
#define SYMINFO_is_init(v)       ((v)->m_sym_is_init)
#define SYMINFO_is_global(v)     ((v)->m_sym_is_global)
#define SYMINFO_is_dynsym(v)     ((v)->m_sym_is_dynsym)
#define SYMINFO_file_name(v)     ((v)->m_sym_file_name)
#define SYMINFO_ofst(v)          ((v)->m_sym_offset)
#define SYMINFO_align(v)         ((v)->m_sym_align)
#define SYMINFO_is_byte(v)       ((v)->m_sym_is_byte)
#define SYMINFO_data_byte(v)     ((v)->m_data_byte)
#define SYMINFO_data_word(v)     ((v)->m_data_binword)
#define SYMINFO_dynsym_idx(v)    ((v)->m_dynsym_index)
//The class descripts the basic info of a symbol. These info came from xoc::Var
//or collected from .symtab of ELF. SymbolInfo is core data that will be used
//to generate section content and construct ELF. Map & Vector will be generated
//to record SymbolInfo of each xoc::Var and ELF.
class SymbolInfo {
    COPY_CONSTRUCTOR(SymbolInfo);
public:
    //Record the type of symbol data.
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

    //Record section name to which SymbolInfo belong.
    SECTION_NAME m_sect_name;

    //Record symbol align.
    Addr m_sym_align;

    //Record the size of symbol data.
    Word m_sym_size;

    //Record index in '.dynsym' that would be
    //referenced by the 'r_sym' field of ELFRel.
    Word m_dynsym_index;

    //Record '.symtab' index in original ELF.
    Word m_sym_index;

    //Record symbol offset in corresponded section.
    Off m_sym_offset;

    //Record the data that corresponded to SymbolInfo.
    //It is used to record regular type(e.g., INT, INT64) data.
    Word m_data_binword;

    //Record the data that corresponded to SymbolInfo.
    //It is used to record string or vector type data.
    BYTE * m_data_byte;

    //Record section string name to which SymbolInfo belong.
    CHAR const* m_sect_name_str;

    //Record corresponded function name if it is functional type.
    Sym const* m_func_name;

    //Record symbol name.
    Sym const* m_sym_name;

    //Record file name.
    CHAR const* m_sym_file_name;

    //Record function info if it is function type.
    FunctionInfo * m_func_info;

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
        m_dynsym_index = 0;
        m_sym_index = 0;
        m_sym_offset = 0;
        m_data_binword = 0;
        m_data_byte = nullptr;
        m_sect_name_str = nullptr;
        m_func_name = nullptr;
        m_sym_name = nullptr;
        m_sym_file_name = nullptr;
        m_func_info = nullptr;
    }

    ~SymbolInfo() {}

    //Add relocation entry into m_sym_reloc.
    //other:  Which symbol needs to be relocated to.
    //type:   Relocation type.
    //offset: Location where the relocation occurs.
    //addend: Addend value of symbol relocated to.
    void addRelocation(Sym const* other, UINT type, UINT offset, UINT addend)
    {
        ASSERT0(other);
        m_sym_reloc.addEntry(other, type, offset, addend);
    }

    //Return binary codes of current symbol.
    BYTEVec & getSymbolCode()
    {
        ASSERT0(m_func_info);
        return m_func_info->getCode();
    }
};


//The class manages SymbolInfo object resources. It creates SymbolInfo
//object and uses xoc::List 'm_list' to record. These resources would
//be freed when the destructor is called.
class SymbolInfoMgr {
    COPY_CONSTRUCTOR(SymbolInfoMgr);

    //Record symbol info object.
    xcom::List<SymbolInfo*> m_list;
public:
    SymbolInfoMgr() { m_list.init(); }

    ~SymbolInfoMgr()
    {
        for (SymbolInfo * si = m_list.get_head();
             si != nullptr; si = m_list.get_next()) {
            if (si != nullptr) { delete si; }
        }
        m_list.clean();
    }

    SymbolInfo * allocSymbolInfo()
    {
        SymbolInfo * si = new SymbolInfo();
        ASSERT0(si);
        m_list.append_tail(si);
        return si;
    }
};


#define RELOCINFO_addend(v)          ((v)->m_reloc_addend)
#define RELOCINFO_called_loc(v)      ((v)->m_reloc_called_location)
#define RELOCINFO_caller_func(v)     ((v)->m_reloc_caller_func)
#define RELOCINFO_caller_sym(v)      ((v)->m_reloc_caller_symbol)
#define RELOCINFO_is_resolved(v)     ((v)->m_reloc_resolved)
#define RELOCINFO_is_object(v)       ((v)->m_reloc_is_object)
#define RELOCINFO_is_func(v)         ((v)->m_reloc_is_func)
#define RELOCINFO_name(v)            ((v)->m_reloc_name)
#define RELOCINFO_next(v)            ((v)->m_reloc_next)
#define RELOCINFO_sect_ofst(v)       ((v)->m_reloc_sect_location)
#define RELOCINFO_sym_idx(v)         ((v)->m_reloc_sym_idx)
#define RELOCINFO_shdr_idx(v)        ((v)->m_reloc_shdr_idx)
#define RELOCINFO_sym(v)             ((v)->m_reloc_symbol_info)
#define RELOCINFO_type(v)            ((v)->m_reloc_type)
#define RELOCINFO_resolved_notype(v) ((v)->m_reloc_resolved_is_notype)
#define RELOCINFO_sect_name_str(v)   ((v)->m_reloc_sect_name_str)
class RelocInfo {
    COPY_CONSTRUCTOR(RelocInfo);
public:
    //Record whether relocated symbol haved been resolved.
    bool m_reloc_resolved;

    //Record whether relocated symbol is function type.
    bool m_reloc_is_func;

    //Record whether relocated symbol is object type.
    bool m_reloc_is_object;

    //Record whether relocated symbol is resolved by symbol with STT_NOTYPE.
    bool m_reloc_resolved_is_notype;

    //Record the location of the relocated symbol is called.
    UINT m_reloc_called_location;

    //Record the relocation type.
    UINT m_reloc_type;

    //Record a constant addend that used to compute the
    //value to be stored into the relocatable field.
    UINT m_reloc_addend;

    //Record the index of related RelocInfo in 'm_reloc_symbol_vec'.
    //Since what exactly relocation type means may be dependented
    //by the type of related RelocInfo.
    UINT m_reloc_next;

    //Record the section index in ELF to which the relocated symbol belong.
    UINT m_reloc_shdr_idx;

    //Record the offset of relocated symbol in target section.
    Addr m_reloc_sect_location;

    //Record relocated symbol index in symtab.
    Word m_reloc_sym_idx;

    //Record the name of relocated symbol.
    CHAR const* m_reloc_name;

    //Record the section name of the relocated symbol.
    //e.g. '.rela.text1', '.rela.rodata', '.rela.dl_tdata'
    //           .text1         .rodata         .dl_tdata
    CHAR const* m_reloc_sect_name_str;

    //Record target resolved symbol of the relocated symbol.
    SymbolInfo * m_reloc_symbol_info;

    //Record caller symbol that call the relocated symbol.
    //A caller symbol is needed to relcoated by another symbol.
    //e.g. caller symbol data
    //         ...
    //       relocated symbol_1 position
    //       relocated symbol_2 position
    //         ...
    SymbolInfo * m_reloc_caller_symbol;

    //Record the function info that call the relocated symbol.
    //e.g. function code
    //         ...
    //       relocated symbol_1 position
    //       relocated symbol_2 position
    //         ...
    FunctionInfo * m_reloc_caller_func;
public:
    RelocInfo()
    {
        m_reloc_resolved = false;
        m_reloc_is_func = false;
        m_reloc_is_object = false;
        m_reloc_resolved_is_notype = false;
        m_reloc_called_location = 0;
        m_reloc_type = S_UNDEF;
        m_reloc_addend = 0;
        m_reloc_next = 0;
        m_reloc_shdr_idx = 0;
        m_reloc_sect_location = 0;
        m_reloc_sym_idx = 0;
        m_reloc_name = nullptr;
        m_reloc_sect_name_str = nullptr;
        m_reloc_symbol_info = nullptr;
        m_reloc_caller_symbol = nullptr;
        m_reloc_caller_func = nullptr;
    }

    ~RelocInfo() {}
};


//The class manages RelocInfo object resources. It creates RelocInfo
//object and uses xoc::List 'm_list' to record. These resources whould
//be freed when the destructor is called.
class RelocInfoMgr {
    COPY_CONSTRUCTOR(RelocInfoMgr);

    //Record reloc info object.
    xcom::List<RelocInfo*> m_list;
public:
    RelocInfoMgr() { m_list.clean(); }

    ~RelocInfoMgr()
    {
        for (RelocInfo * ri = m_list.get_head();
             ri != nullptr; ri = m_list.get_next()) {
            if (ri != nullptr) { delete ri; }
        }
        m_list.clean();
    }

    RelocInfo * allocRelocInfo()
    {
        RelocInfo * ri = new RelocInfo();
        ASSERT0(ri);
        m_list.append_tail(ri);
        return ri;
    }
};


#define RELADYNINFO_is_got(v)     ((v)->m_is_got_item)
#define RELADYNINFO_is_dynsym(v)  ((v)->m_is_dynsym_item)
#define RELADYNINFO_reloc_info(v) ((v)->m_reloc_info)
#define RELADYNINFO_addend(v)     (RELOCINFO_addend(((v)->m_reloc_info)))
#define RELADYNINFO_type(v)       (RELOCINFO_type((v)->m_reloc_info))
#define RELADYNINFO_ofst(v)       (RELOCINFO_called_loc((v)->m_reloc_info))
#define RELADYNINFO_sym_idx(v) \
        (SYMINFO_dynsym_idx(RELOCINFO_sym((v)->m_reloc_info)))
#define RELADYNINFO_sym_name(v) \
        (SYMINFO_name(RELOCINFO_sym((v)->m_reloc_info)))
#define RELADYNINFO_caller_sym_name(v) \
        (RELOCINFO_sect_name_str((v)->m_reloc_info))
//The class descripts the basic info of reladyn item which mainly comes from
//RelocInfo object. These info are used to construct '.rela.dyn' sect content.
class RelaDynInfo {
    COPY_CONSTRUCTOR(RelaDynInfo);
public:
    //Record whether .got section need to create a got item as the memory
    //space that will be refilled physical address when program executed
    //according to the reladyn info.
    //e.g.  reladyn item:
    //     Offset  | Info | Type | Sym.value | Sym.name + Addend
    //     0x10000
    //       |--> pointed to .got section content address. The address is
    //            needed to refill physical address when program is executed.
    bool m_is_got_item;

    //Record whether .dynsym section need to create an item. Since the 'r_sym'
    //field of reladyn info may be pointed to the dynsym item if the resolved
    //symbol with 'STB_GLOBAL' attribute. Otherwise, if there isn't dynsym item,
    //it means the value of 'r_sym' is the constant address of resolved symbol.
    bool m_is_dynsym_item;

    //Record the got item offset in '.got' section.
    UINT m_got_ofst;

    //Record reloc info of the reladyn item.
    RelocInfo * m_reloc_info;
public:
    RelaDynInfo()
    {
        m_is_got_item = false;
        m_is_dynsym_item = false;
        m_got_ofst = 0;
        m_reloc_info = nullptr;
    }

    ~RelaDynInfo() {}
};


//The class manages RelaDynInfo object resources. It creates RelaDynInfo
//object and uses xoc::List 'm_list' to record. These resources would be
//freed when the destructor is called.
class RelaDynInfoMgr {
    COPY_CONSTRUCTOR(RelaDynInfoMgr);

    //Record reladyn info object.
    xcom::List<RelaDynInfo*> m_list;
public:
    RelaDynInfoMgr() { m_list.init(); }

    ~RelaDynInfoMgr()
    {
        for (RelaDynInfo * rdi = m_list.get_head();
             rdi != nullptr; rdi = m_list.get_next()) {
            if (rdi != nullptr) { delete rdi; }
        }
        m_list.clean();
    }

    RelaDynInfo * allocRelaDynInfo()
    {
        RelaDynInfo * rdi = new RelaDynInfo();
        ASSERT0(rdi);
        m_list.append_tail(rdi);
        return rdi;
    }
};


//The class decripts generated mapped of SymMap.
class GenMappedOfSymMap {
    COPY_CONSTRUCTOR(GenMappedOfSymMap);
public:
    SymbolInfoMgr * m_sym_mgr;

    GenMappedOfSymMap() {}

    GenMappedOfSymMap(SymbolInfoMgr * sym_mgr) : m_sym_mgr(sym_mgr) {}

    SymbolInfo * createMapped(Sym const* s)
    {
        ASSERT0(m_sym_mgr);
        return m_sym_mgr->allocSymbolInfo();
    }
};


//The class descripts a Map for SymbolInfo. It provides GenMappedOfSymMap to
//decrease the number of lookups RBT when SymbolInfo is recorded or updated.
class SymMap : public xcom::TMap<Sym const*, SymbolInfo*,
    CompareKeyBase<Sym const*>, GenMappedOfSymMap> {
    COPY_CONSTRUCTOR(SymMap);
public:
    SymMap(SymbolInfoMgr * sym_mgr)
    {
        xcom::TMap<Sym const*, SymbolInfo*, CompareKeyBase<Sym const*>,
                   GenMappedOfSymMap>::m_gm.m_sym_mgr = sym_mgr;
    }

    ~SymMap() {}
};


class ELFARMgr;

typedef xcom::Vector<SymbolInfo*> SymbolInfoVec;
typedef xcom::Vector<SymbolInfoVec*> SymtabInfoVec;
//SectNameDescMap.
typedef xcom::TMap<CHAR const*, SectionNameDesc const*,
                   CompareStringFunc> SectionNameDescMap;
typedef xcom::TMapIter<CHAR const*, SectionNameDesc const*> SectNameDescIter;
//SectionInfoMap.
typedef xcom::TMapIter<CHAR const*, SectionInfo*> SectionInfoIter;
typedef xcom::TMap<CHAR const*, SectionInfo*,
                   CompareStringFunc> SectionInfoMap;
//
//Start ELFMgr.
//
//Record symbols and their information.
#define ELFMGR_symbol_info(e) ((e)->m_symbol_info)
#define ELFMGR_symtab_vec(e)  ((e)->m_symtab_info_vec)
#define ELFMGR_symbol_vec(e)  ((e)->m_symbol_info_vec)
#define ELFMGR_output_name(e) ((e)->m_output_file_name)
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

    //Record section info for generating elf.
    ELFSectionInfo * m_sect_info;
public:
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

    //Assemble init value of symbol to .data, .sdata, .const or SPM section.
    //This function will use the size of the symbol to represent the initial
    //value and write it into content_desc_vec.
    void assembleInitValToContent(OUT AssembleBinDescVec & content_desc_vec,
                                  SymbolInfo const* sym_info);

    //Assemble pad zero to .data, .sdata, or .const or SPM section.
    //This function will pad pad_size zeros in the gap between two symbols.
    //For example: Assume that there are two symbols in the same section:
    //
    //    sym1: size-0x2 align-0x2             sym2: size-0x8 align-0x8
    //
    //symbol distribution within section:
    //
    //      |<- sym1 ->|<--- pad zero --->|<-----  sym2  ----->|
    //      |    2B    |        6B        |         8B         |
    //     0x0        0x2                0x8                  0x10
    void assemblePadZeroToContent(OUT AssembleBinDescVec & content_desc_vec,
                                  UINT pad_size);

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

    void dump() const;
    void dumpStrTabContent(CHAR const* strtab, Addr size) const;

    //Since user-defined functions have been saved, this interface only
    //collect un-user-defined symbols.
    void extractSymbolExceptUserDefFunc();

    //Generate contents for .symtab, .sbss, .bss, .sdata, .data, .const
    //and SPM sections.
    void genCommonSectionContent(OUT BYTEVec & symtab_content,
                                 OUT BYTEVec & sbss_content,
                                 OUT BYTEVec & sdata_content,
                                 OUT BYTEVec & bss_content,
                                 OUT BYTEVec & data_content,
                                 OUT BYTEVec & const_content,
                                 OUT BYTEVec & spm_content,
                                 OffVec const& sym_str_off);

    //Generate contents for .rel.text.xxx section.
    //bytevec: binary code of relocation info of function.
    //reloc:   saved all relocation entries for current symbol.
    void genRelocContent(OUT BYTEVec & bytevec, RelocationInfo const* reloc);

    //Generate contents for .rel.sdata and .rel.data sections.
    void genRelocSdataAndDataContent(OUT BYTEVec & sdata_content,
                                     OUT BYTEVec & data_content);

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

    //Returns binary codes of given symbol.
    BYTEVec & getSymbolCode(Sym const* sym)
    {
        ASSERT0(sym && m_symbol_info.find(sym));
        return m_symbol_info.get(sym)->getSymbolCode();
    }

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

    //SymbolInfo Mgr. Create and free SymbolInfo resources.
    SymbolInfoMgr m_sym_mgr;

    //FunctionInfo Mgr. Create and free FunctionInfo resources.
    FunctionInfoMgr m_func_mgr;

    //SectionInfo Mgr. Create and free SectionInfo resources.
    SectionInfoMgr m_sect_mgr;

    //RelocInfo Mgr. Create and free RelocInfo resources.
    RelocInfoMgr m_reloc_mgr;

    //RelaDynInfo Mgr. Create and free ReladynInfo resources.
    RelaDynInfoMgr m_reladyn_mgr;

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
    //Record symbol info collected.
    SymMap m_symbol_info;

    //Alloc program header.
    //Param phnum: the number of program header.
    void allocProgramHeader(UINT phnum)
    {
        //FIXME: Wait other part.
    }

    //Collected function/symbol info with ELFMgr format from xoc::Var.
    void collectELFInfoFromVar(CHAR const* fn);

    //Collect function info from xoc::Var.
    void collectFunctionInfoFromVar()
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

    //Collect symtab info from xoc::Var.
    void collectSymtabInfoFromVar()
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
    //Param bind: st_bind field of ELFSym.
    //Param type: st_type field of ELFSym.
    //Param other: st_other field of ELFSym.
    //Param shndex: st_shndx field of ELFSym.
    //Param value: st_vale field of ELFSym.
    //Param size: st_size field of ELFSym.
    void setSymbolValue(MOD ELFSym * sym, Word name, xcom::UCHAR bind,
                        xcom::UCHAR type, xcom::UCHAR other, Half shndx,
                        Addr value, Addr size);

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


//
//Start ELFAR.
//
#define ELFAR_index_num(e)    ((e)->m_index_num)
#define ELFAR_sym_tab(e)      ((e)->m_sym_tab)
#define ELFAR_sym_tab_size(e) ((e)->m_sym_tab_size)
#define ELFAR_idx_array(e)    ((e)->m_index_array)
//Manage the AR file. It can parse the AR file format, extract valid info.
//The AR file format is referenced from 'ar_header.h'.
class ELFAR {
    COPY_CONSTRUCTOR(ELFAR);
public:
    //Record the total length of global symbol table in AR file.
    //The layout of global symbol table is: str1\0str2\0str3\0...
    UINT64 m_sym_tab_size;

    //Record the number of global symbol element.
    UINT64 m_index_num;

    //Record the file position during parsed AR file.
    UINT64 m_file_pos;

    //Record the value of each gloabl symbol index.
    UINT64 * m_index_array;

    //AR file.
    FileObj * m_file;

    //Record the content of global symbol table.
    CHAR * m_sym_tab;

    //Record the file path/name of AR file.
    CHAR const* m_file_name;

    //Record ARHdr.
    ARHdr m_ar_hdr;
public:
    ELFAR(CHAR const* name)
    {
        m_sym_tab_size = 0;
        m_index_num = 0;
        m_file_pos = 0;
        m_index_array = nullptr;
        m_file = nullptr;
        m_sym_tab = nullptr;
        m_file_name = name;
    }

    ~ELFAR() {}
};


#define ELFARINFO_ar(e)          ((e)->m_ar)
#define ELFARINFO_elf_mgr(e)     ((e)->m_elf_mgr)
#define ELFARINFO_elf_mgr_idx(e) ((e)->m_elf_mgr_idx)
//Record the reusable info in ELFAR file that could be used in linker process.
class ELFARInfo {
    COPY_CONSTRUCTOR(ELFARInfo);
public:
    //Record the index of ELF(ELFMgr) in ELFAR file.
    UINT64 m_elf_mgr_idx;

    //Record ELFMgr.
    ELFMgr * m_elf_mgr;

    //Record ELFAR file.
    ELFAR * m_ar;
public:
    ELFARInfo()
    {
        m_elf_mgr_idx = 0;
        m_elf_mgr = nullptr;
        m_ar = nullptr;
    }

    ~ELFARInfo() {}
};


//
//Start ELFARMgr.
//
typedef xcom::TMap<CHAR const*, ELFARInfo*,
                   CompareStringFunc> SymbolARInfoMap;

typedef xcom::TMap<ELFAR*, xcom::Vector<ELFARInfo*>*> ARInfoMap;

//A class manages ELFAR/ELFARInfo object resources. It creates ELFAR and
//ELFARInfo object and uses 'm_ar_list' or 'm_ar_info_meta_list' to record.
//These resources would be freed when the destructor is called.
class ELFARMgr {
    COPY_CONSTRUCTOR(ELFARMgr);

    //Record ar file path/name with list.
    xcom::List<CHAR const*> * m_ar_file_list;

    //Record all ELFAR object that generated by 'allocELFAR func'.
    xcom::List<ELFAR*> m_ar_list;

    //Record all ELFARInfo object that generated by 'allocELFARInfo func'.
    xcom::List<ELFARInfo*> m_ar_info_meta_list;

    //Record all FileObj that generated by 'allocFileObj func'.
    xcom::List<FileObj*> m_file_obj_list;

    //Record 'symbol name' <-> 'ELFARInfo' info.
    SymbolARInfoMap m_symbol_ar_info_map;

    //Record 'ar' <-> 'ELFARInfo' info.
    ARInfoMap m_ar_info_map;

    //Record ar file path/name with stack.
    xcom::Stack<CHAR const*> m_ar_file_stack;
public:
    ELFARMgr();

    ~ELFARMgr();

    //Allocate ELFAR object and record to 'm_ar_list'.
    ELFAR * allocELFAR(CHAR const* file_name);

    //Allocate ELFARInfo object and record to 'm_ar_info_meta_list'.
    ELFARInfo * allocELFARInfo();

    //Allocate FileObj and record to 'm_file_obj_list'.
    //is_del: 'true' to delete the file with same name.
    FileObj * allocFileObj(CHAR const* filename, bool is_del);
};


//
//Start LinkerMgr.
//
#define LINKERMGR_OUTPUT_FILE_NAME   "mi.test.elf"
#define LINKERMGR_DUMP_LOG_FILE_NAME "dump.log"

typedef xcom::TMap<CHAR const*, Vector<UINT>*,
                   CompareStringFunc> RelocSymbolIdxMap;

typedef xcom::TMap<CHAR const*, UINT, CompareStringFunc> SameNameNumMap;

//The class manages all linker processes. These processes contain merge
//multi-ELFMgr, resolve undefined symbols, relocate symbols and output
//target ELF.
class LinkerMgr {
    COPY_CONSTRUCTOR(LinkerMgr);

    //Record ELFMgr object that generated by LinkerMgr. Some ELFMgr objects
    //that generated in other module don't be recorded in this List.
    xcom::List<ELFMgr*> m_elf_mgr_meta_list;
protected:
    //File name of output ELF.
    CHAR const* m_output_file_name;

    //Manage dump file.
    FileObj * m_dump;

    //ELFMgr to which the output ELF belong.
    ELFMgr * m_output_elf_mgr;

    //Record ELFMgr object. The List contains all ELFMgr objects that would
    //be used in LinkerMgr. It includes the element in 'm_elf_mgr_meta_list'
    //and ELFMgr objects that generated by other module.
    xcom::List<ELFMgr*> m_elf_mgr_list;

    //Record relocated symbol.
    xcom::Vector<RelocInfo*> m_reloc_symbol_vec;

    //Record the index of unresolved relocated symbol in 'm_reloc_symbol_vec'.
    //            key                     value
    //'resolved_reloc_symbol_name' <-> 'Vector<UINT>'.
    RelocSymbolIdxMap m_unresolved_reloc_idx_map;

    //Record the index of resolved relocated symbol in 'm_reloc_symbol_vec'.
    //            key                     value
    //'unresolved_reloc_symbol_name' <-> 'Vector<UINT>'.
    RelocSymbolIdxMap m_resolved_reloc_idx_map;

    //Record the number of symbols with same name.
    SameNameNumMap m_same_name_num_map;

    //Manage ELFARMgr object.
    ELFARMgr m_armgr;
public:
    LinkerMgr();

    virtual ~LinkerMgr();

    //Allocate ELFMgr during linker.
    ELFMgr * allocELFMgr();

    //Allocate Output ELFMgr object.
    void allocOutputELFMgr();

    //Close dump log file.
    void closeDump();

    //Free Output ELFMgr object.
    void freeOutputELFMgr()
    {
        if (m_output_elf_mgr != nullptr) {
            delete m_output_elf_mgr;
            m_output_elf_mgr = nullptr;
        }
    }

    //Generate ELFMgr object.
    virtual ELFMgr * genELFMgr()
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Initialize dump file info.
    EM_STATUS initDumpFile(CHAR const* filename);
};

}
#endif
