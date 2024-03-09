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
//The align of TEXT(CODE) section.
#define ELF_SH_TEXT_ALIGN                 1
//The align of DYNSYM section.
#define ELF_SH_DYNSYM_ALIGN               8
//Get st_other value of ELFSym.
#define GET_SYM_OTHER_VALUE(v)            ((v) & 0x3)
//Get the index of PH_TYPE_XXX in program header.
#define GET_PH_TYPE_INDEX(v)              ((v) - 1)
//Integer 10.
#define ELF_NUM_INT_10                    10
//The first index of substr after splited.
#define FIRST_INDEX_OF_SUBSTR             1


typedef xcom::Vector<CHAR> CHARVec;
typedef xcom::Vector<Off> OffVec;
typedef xcom::Vector<CHAR const*> StringVec;
typedef xcom::Vector<BYTE> BYTEVec;
typedef xcom::List<CHAR const*> StringList;
typedef xcom::Vector<SWord> SWordVec;
typedef xcom::Vector<Word> WordVec;
typedef xcom::Vector<Addr> AddrVec;
typedef xcom::Vector<Sym const*> SymVec;
typedef xcom::TMapIter<Sym const*, SymbolInfo*> SymbolInfoIter;
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


typedef enum _SECTION_TYPE {
    SH_TYPE_UNDEF = 0,
    SH_TYPE_EMPTY = 1,
    SH_TYPE_SHSTR,
    SH_TYPE_SYMSTR,
    SH_TYPE_TEXT,
    SH_TYPE_SBSS,
    SH_TYPE_SDATA,
    SH_TYPE_BSS,
    SH_TYPE_DATA,
    SH_TYPE_SYMTAB,
    SH_TYPE_SUBTEXT,
    SH_TYPE_RELA,
    SH_TYPE_CONST,
    SH_TYPE_SPM,
    SH_TYPE_GOT,
    SH_TYPE_RELA_DYN,
    SH_TYPE_DYNSYM,
    SH_TYPE_TEXT1,
    SH_TYPE_AITEXT,
    SH_TYPE_FINI,
    SH_TYPE_PREINIT_ARRAY,
    SH_TYPE_DYNAMIC,
    SH_TYPE_INTERP,
    SH_TYPE_DL_TDATA,
    SH_TYPE_RODATA,
    SH_TYPE_RODATA1,
    SH_TYPE_LDM,
    SH_TYPE_COMPILER_VERSION,
    SH_TYPE_DEBUG_INFO,
    SH_TYPE_DEBUG_LINE,
    SH_TYPE_DEBUG_ABBREV,
    SH_TYPE_DEBUG_ARANGES,
    SH_TYPE_EH_FRAME,
    SH_TYPE_COMMENT,
    SH_TYPE_NOTE, //'.note.GNU-stack'
    SH_TYPE_MAX_NUM,
} SECTION_TYPE;


#define SWITCH_CASE_DEBUG_SECT  \
    case SH_TYPE_DEBUG_INFO:    \
    case SH_TYPE_DEBUG_LINE:    \
    case SH_TYPE_DEBUG_ABBREV:  \
    case SH_TYPE_DEBUG_ARANGES: \
    case SH_TYPE_EH_FRAME


#define SWITCH_CASE_COMMON_SECT    \
    SWITCH_CASE_DEBUG_SECT:        \
    case SH_TYPE_TEXT:             \
    case SH_TYPE_SBSS:             \
    case SH_TYPE_SDATA:            \
    case SH_TYPE_BSS:              \
    case SH_TYPE_DATA:             \
    case SH_TYPE_SUBTEXT:          \
    case SH_TYPE_RELA:             \
    case SH_TYPE_CONST:            \
    case SH_TYPE_SPM:              \
    case SH_TYPE_GOT:              \
    case SH_TYPE_RELA_DYN:         \
    case SH_TYPE_DYNSYM:           \
    case SH_TYPE_TEXT1:            \
    case SH_TYPE_AITEXT:           \
    case SH_TYPE_FINI:             \
    case SH_TYPE_DYNAMIC:          \
    case SH_TYPE_DL_TDATA:         \
    case SH_TYPE_RODATA:           \
    case SH_TYPE_RODATA1:          \
    case SH_TYPE_LDM:              \
    case SH_TYPE_COMPILER_VERSION: \
    case SH_TYPE_COMMENT:          \
    case SH_TYPE_NOTE


#define SWITCH_CASE_COMMON_SECT_CONSTRUCT \
    SWITCH_CASE_DEBUG_SECT:               \
    case SH_TYPE_UNDEF:                   \
    case SH_TYPE_SHSTR:                   \
    case SH_TYPE_SYMSTR:                  \
    case SH_TYPE_TEXT:                    \
    case SH_TYPE_SBSS:                    \
    case SH_TYPE_SDATA:                   \
    case SH_TYPE_BSS:                     \
    case SH_TYPE_DATA:                    \
    case SH_TYPE_SUBTEXT:                 \
    case SH_TYPE_RELA:                    \
    case SH_TYPE_CONST:                   \
    case SH_TYPE_SPM:                     \
    case SH_TYPE_GOT:                     \
    case SH_TYPE_TEXT1:                   \
    case SH_TYPE_AITEXT:                  \
    case SH_TYPE_FINI:                    \
    case SH_TYPE_PREINIT_ARRAY:           \
    case SH_TYPE_INTERP:                  \
    case SH_TYPE_DL_TDATA:                \
    case SH_TYPE_RODATA:                  \
    case SH_TYPE_RODATA1:                 \
    case SH_TYPE_LDM:                     \
    case SH_TYPE_COMPILER_VERSION:        \
    case SH_TYPE_COMMENT:                 \
    case SH_TYPE_NOTE


#define SWITCH_CASE_COMMON_SECT_OFST \
    SWITCH_CASE_COMMON_SECT:         \
    case SH_TYPE_SHSTR:              \
    case SH_TYPE_SYMSTR:             \
    case SH_TYPE_SYMTAB


#define SWITCH_CASE_COMMON_SECT_ADDR \
    SWITCH_CASE_COMMON_SECT:         \
    case SH_TYPE_INTERP


#define SWITCH_CASE_SECT_BYTE_TYPE \
    SWITCH_CASE_COMMON_SECT:       \
    case SH_TYPE_UNDEF:            \
    case SH_TYPE_SYMTAB:           \
    case SH_TYPE_INTERP:           \
    case SH_TYPE_PREINIT_ARRAY


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
    SECTION_TYPE m_desc_name;

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
    UINT getSectionAlign(SECTION_TYPE sect);
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
#define SECTINFO_type(v)       ((v)->m_sect_type)
#define SECTINFO_name_str(v)   ((v)->m_sect_name_str)
#define SECTINFO_name_sym(v)   ((v)->m_sect_name_sym)
#define SECTINFO_ofst(v)       ((v)->m_sect_offset)
#define SECTINFO_index(v)      ((v)->m_sect_index)
#define SECTINFO_align(v)      ((v)->m_sect_addr_align)
#define SECTINFO_flag(v)       ((v)->m_sect_flags)
#define SECTINFO_addr(v)       ((v)->m_sect_base_addr)
#define SECTINFO_size(v)       ((v)->m_sect_size)
#define SECTINFO_entry_size(v) ((v)->m_sect_entry_size)
#define SECTINFO_shdr_type(v)  ((v)->m_sect_shdr_type)
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
    SECTION_TYPE m_sect_type;

    //Record program header type to which the section belong.
    PROGRAM_HEADER m_sect_ph_type;

    //Record section type(e.g., S_PROGBITS).
    Word32 m_sect_shdr_type;

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

    //Record section sym name.
    Sym const* m_sect_name_sym;

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
        m_sect_type = SH_TYPE_UNDEF;
        m_sect_ph_type = PH_TYPE_UNDEF;
        m_sect_shdr_type = S_UNDEF;
        m_sect_index = 0;
        m_sect_flags = S_UNDEF;
        m_sect_addr_align = 0;
        m_sect_entry_size = 0;
        m_sect_offset = 0;
        m_sect_base_addr = 0;
        m_sect_size = 0;
        m_sect_name_str = nullptr;
        m_sect_name_sym = nullptr;
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

    SectionInfo * allocSectionInfo(SECTION_TYPE sect_type)
    {
        SectionInfo * si = new SectionInfo();
        ASSERT0(si);
        //Allocate charvec or bytevec according to the section name.
        switch (sect_type) {
        case SH_TYPE_SYMSTR:
        case SH_TYPE_SHSTR:
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
#define FUNCINFO_name_str(v)      ((v)->m_func_name_str)
#define FUNCINFO_sect_type(v)     ((v)->m_sect_type)
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
    SECTION_TYPE m_sect_type;

    //Record current function code align.
    UINT m_func_align;

    //Record the size of code.
    Word m_func_size;

    //Record offset in corresponded code section.
    Off m_func_code_offset;

    //Record function name.
    Sym const* m_func_name;

    //Record function name(string type).
    CHAR const* m_func_name_str;

    //Record file name.
    CHAR const* m_func_file_name;

    //Record section string name.
    CHAR const* m_sect_name_str;

    //Record the code.
    BYTEVec m_func_code;

    FunctionInfo(Addr size)
    {
        m_func_is_entry = false;
        m_sect_type = SH_TYPE_UNDEF;
        m_func_align = 0;
        m_func_size = 0;
        m_func_code_offset = 0;
        m_func_name = nullptr;
        m_func_name_str = nullptr;
        m_func_file_name = nullptr;
        m_sect_name_str = nullptr;
        m_func_code.grow((UINT)size);
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


class RelocInfo;
//
//Start SymbolInfo
//
#define SYMINFO_name(v)          ((v)->m_sym_name)
#define SYMINFO_name_str(v)      ((v)->m_sym_name_str)
#define SYMINFO_sect_type(v)     ((v)->m_sect_type)
#define SYMINFO_sect_name_str(v) ((v)->m_sect_name_str)
#define SYMINFO_sym(v)           ((v)->m_sym_elfsym)
#define SYMINFO_index(v)         ((v)->m_sym_index)
#define SYMINFO_size(v)          ((v)->m_sym_size)
#define SYMINFO_func(v)          ((v)->m_func_info)
#define SYMINFO_reloc(v)         ((v)->m_sym_reloc_vec)
#define SYMINFO_func_name(v)     ((v)->m_func_name)
#define SYMINFO_func_name_str(v) ((v)->m_func_name_str)
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

    //Record section type to which SymbolInfo belong.
    SECTION_TYPE m_sect_type;

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

    //Record corresponded function name(string type).
    CHAR const* m_func_name_str;

    //Record symbol name.
    Sym const* m_sym_name;

    //Record symbol name(string type).
    CHAR const* m_sym_name_str;

    //Record file name.
    CHAR const* m_sym_file_name;

    //Record function info if it is function type.
    FunctionInfo * m_func_info;

    //Record ELFSym.
    ELFSym m_sym_elfsym;

    //Record all relocation info of current symbol.
    xcom::Vector<RelocInfo*> m_sym_reloc_vec;
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
        m_sect_type = SH_TYPE_UNDEF;
        m_sym_align = 0;
        m_sym_size = 0;
        m_dynsym_index = 0;
        m_sym_index = 0;
        m_sym_offset = 0;
        m_data_binword = 0;
        m_data_byte = nullptr;
        m_sect_name_str = nullptr;
        m_func_name = nullptr;
        m_func_name_str = nullptr;
        m_sym_name = nullptr;
        m_sym_name_str = nullptr;
        m_sym_file_name = nullptr;
        m_func_info = nullptr;
    }

    ~SymbolInfo() {}

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
#define RELOCINFO_name_str(v)        ((v)->m_reloc_name_str)
#define RELOCINFO_next(v)            ((v)->m_reloc_next)
#define RELOCINFO_sect_ofst(v)       ((v)->m_reloc_sect_location)
#define RELOCINFO_sym_idx(v)         ((v)->m_reloc_sym_idx)
#define RELOCINFO_shdr_idx(v)        ((v)->m_reloc_shdr_idx)
#define RELOCINFO_sym(v)             ((v)->m_reloc_symbol_info)
#define RELOCINFO_type(v)            ((v)->m_reloc_type)
#define RELOCINFO_resolved_notype(v) ((v)->m_reloc_resolved_is_notype)
#define RELOCINFO_sect_name_str(v)   ((v)->m_reloc_sect_name_str)
#define RELOCINFO_sect_name_sym(v)   ((v)->m_reloc_sect_name_sym)
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

    //Record the location of the relocated symbol is called.
    Addr m_reloc_called_location;

    //Record the offset of relocated symbol in target section.
    Addr m_reloc_sect_location;

    //Record relocated symbol index in symtab.
    Word m_reloc_sym_idx;

    //Record the name of relocated symbol.
    Sym const* m_reloc_name;

    //Record the name of relocated symbol(string type).
    CHAR const* m_reloc_name_str;

    //Record the section name of the relocated symbol.
    //e.g. '.rela.text1', '.rela.rodata', '.rela.dl_tdata'
    //           .text1         .rodata         .dl_tdata
    CHAR const* m_reloc_sect_name_str;

    //Record the section name of the relocated symbol.
    Sym const* m_reloc_sect_name_sym;

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
        m_reloc_name_str = nullptr;
        m_reloc_sect_name_str = nullptr;
        m_reloc_sect_name_sym = nullptr;
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
        (RELOCINFO_sect_name_sym((v)->m_reloc_info))
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


class ELFMgr;

class GenMappedOfSectInfoMap {
    COPY_CONSTRUCTOR(GenMappedOfSectInfoMap);
public:
    ELFMgr * m_elf_mgr;

    SectionInfoMgr * m_sect_mgr;

    GenMappedOfSectInfoMap() {}

    GenMappedOfSectInfoMap(ELFMgr * elf_mgr, SectionInfoMgr * sect_mgr) :
        m_elf_mgr(elf_mgr), m_sect_mgr(sect_mgr) {}

    SectionInfo * createMapped(CHAR const* s);
};


class SectionInfoMap : public xcom::TMap<CHAR const*, SectionInfo*,
    CompareKeyBase<CHAR const*>, GenMappedOfSectInfoMap> {
    COPY_CONSTRUCTOR(SectionInfoMap);
public:
    SectionInfoMap(ELFMgr * elf_mgr, SectionInfoMgr * sect_mgr)
    {
        xcom::TMap<CHAR const*, SectionInfo*, CompareKeyBase<CHAR const*>,
                   GenMappedOfSectInfoMap>::m_gm.m_elf_mgr = elf_mgr;

        xcom::TMap<CHAR const*, SectionInfo*, CompareKeyBase<CHAR const*>,
                   GenMappedOfSectInfoMap>::m_gm.m_sect_mgr = sect_mgr;
    }

    ~SectionInfoMap() {}
};


class ELFARMgr;

typedef xcom::Vector<SymbolInfo*> SymbolInfoVec;
typedef xcom::Vector<SymbolInfoVec*> SymtabInfoVec;
typedef xcom::TMapIter<CHAR const*, SymbolInfo*> SymbolInfoMapIter;
typedef xcom::TMap<CHAR const*, SymbolInfo*, CompareStringFunc> SymbolInfoMap;
//SectNameDescMap.
typedef xcom::TMap<CHAR const*, SectionNameDesc const*,
                   CompareStringFunc> SectionNameDescMap;
typedef xcom::TMapIter<CHAR const*, SectionNameDesc const*> SectNameDescIter;
//SectionInfoMap.
typedef xcom::TMapIter<CHAR const*, SectionInfo*> SectionInfoMapIter;
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
        addSymRelocInfo(m_symbol_info.get(current),
            other, type, offset, addend);
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
        addSymRelocInfo(m_symbol_info.get(current),
            other, getReferRelocType(), offset, addend);
    }

    //Allocate and set RelocInfo of 'symbol_info'.
    void addSymRelocInfo(MOD SymbolInfo * symbol_info,
        Sym const* other, UINT type, UINT offset, UINT addend);

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
    void genRelocContent(OUT BYTEVec & bytevec,
                         xcom::Vector<RelocInfo*> & reloc);

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

    //Get specific st_other info of ELFSym for different architecture.
    virtual UINT getSymOtherInfo() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get align value of .text section.
    virtual Addr getTextSectAlign() const
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
    SECTION_TYPE judgeSymbolSection(xoc::Var const* var);

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

    //Read ELF info from opened file(m_pfile) via 'offset'.
    //'offset' is the position of opened file.
    EM_STATUS readELF(UINT64 offset);

    //close: Whether close the file after read ELF content. AR file doesn't
    //needed to close, since other ELF content in this AR file may read later.
    EM_STATUS readELFContent(bool read_all_content, bool close = true);

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
    //Record whether the ELFMgr is with ELF format. 'true' if the ELFMgr
    //info collected from ELF file. 'false' if the ELFMgr info collected
    //from xoc::Var.
    bool m_have_elf_format;

    //Record the number of section header.
    UINT m_shdr_num;

    //Record the number of subtext.
    UINT m_subtext_num;

    //Record the begin index of global symbol in .symtab.
    UINT m_global_symbol_begin_index;

    //Record the number of reladyn item with local attribute.
    UINT m_reladyn_local_item_num;

    //Record the length of string name of all SymbolInfos.
    UINT m_symbol_str_len;

    //Record the max base offset of section of ELF.
    Off m_max_offset;

    //Record the max base address of section of ELF.
    Addr m_max_addr;

    //Record the number of GOT item.
    UINT64 m_got_elem_num;

    //Record the number of SymbolInfo.
    UINT64 m_symbol_num;

    //Record current position when read ELF file.
    UINT64 m_file_base_offset;

    //Record the file name of ELFMgr.
    CHAR const* m_file_name;

    //Sym table.
    SymTab * m_sym_tab;

    //Record section with order. The order info come from getSectionIndex().
    xcom::Vector<CHAR const*> m_sect_layout;

    //Record FunctionInfo.
    xcom::Vector<FunctionInfo*> m_func_info;

    //Record SmbolInfo collected from ELF.
    SymbolInfoMap m_symbol_info_map;

    //Record SymbolInfo collected from ELF.
    SymbolInfoVec m_symbol_info_vec;

    //Record RelocInfo.
    xcom::Vector<RelocInfo*> m_reloc_info;

    //Record all 'm_symbol_info_vec'.
    SymtabInfoVec m_symtab_info_vec;

    //Record reladyn info.
    xcom::Vector<RelaDynInfo*> m_reladyn_info_vec;

    //Record section name description info.
    SectionNameDescMap m_sect_desc_info;

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

    //Record symbol offset in corresponded section.
    //e.g. ----------------------------------------
    //     .sbss sect:    var_4B, var_8B,  var_32B
    //     ----------------------------------------
    //      .sbss_off:      0,      4,        12
    //     ----------------------------------------
    //     .sdata sect:   var_8B, var_32B, var_16B
    //      .sdata_off:     0,      8,        40
    //     ----------------------------------------
    ELFSymbolOff m_symbol_off;
public:
    //Record symbol info collected.
    SymMap m_symbol_info;

    //Record ELF section info.
    SectionInfoMap m_sect_map;

    //Add 's' into 'm_sym_tab'.
    Sym const* addToSymTab(CHAR const* s)
    { ASSERT0(s); return m_sym_tab->add(s); }

    //Allocate program header according to the given 'phnum'.
    void allocProgramHeader(UINT phnum);

    //Collect function/symbol info from xoc::Var.
    void collectELFInfoFromVar();

    //Collect function code/size/relocation info from xoc::Var.
    void collectFunctionInfoFromVar();

    //Collect the code, code size and offset info of function.
    //bytevec: record the binary code of function.
    //index: function index in all functions.
    void collectFunctionInfo(OUT BYTEVec & bytevec, MOD UINT index);

    //Collect function info from 'hdr' if the 'symbol_info' with FUNC type.
    void collectFuncInfoForSymbol(ELFHdr & hdr, MOD SymbolInfo * symbol_info);

    //Collect symtab info from xoc::Var.
    void collectSymtabInfoFromVar();

    //Collect SymbolInfo from xoc::Var.
    //func_ind: functional index in xoc::Var.
    void collectSymbolInfo(xoc::Var const* var, MOD SymbolInfo * sym_info,
                           MOD UINT & func_ind);

    //Collect symbol and function info from ELF that have been read into memory.
    void collectObjELF();

    //The implement function of collected OBJ ELF.
    virtual void collectObjELFImple(ELFHdr & hdr, ELFSHdr const* shdr,
                                    UINT shdr_idx);

    //Collect section name to which SymbolInfo belong.
    void collectSymbolInfoSectionName(
        ELFHdr & hdr, MOD SymbolInfo * symbol_info);

    //Collect function info from 'text_shdr'.
    FunctionInfo * collectTextInfo(ELFSHdr const* text_shdr,
        ELFSHdr const* strtab_shdr, SymbolInfo const* symbol_info);

    //Collect symbol data from corresponded section.
    //byte: the data is BYTE type.
    //binword: the data is word type.
    //is_byte: the flag of data type.
    void collectSymbolData(xoc::Var const* var, OUT Word & size,
        OUT BYTE ** byte, OUT Word & binword, OUT bool & is_byte);

    //Collect RelocInfo from 'hdr'.
    void collectRelocInfo(ELFHdr & hdr, ELFSHdr const* rela_shdr);

    //Collect SymtabInfo from 'hdr'.
    void collectSymtabInfo(ELFHdr & hdr, ELFSHdr const* sym_shdr);

    //Construct null symbol of '.symtab' and '.dynsym'.
    //It is the first symbol in symtab and it's values are all zero.
    void constructSymbolNull(OUT BYTEVec * symtab_bytevec,
                             OUT BYTEVec * dynsym_bytevec);

    //A helper function of construct ELF section.
    void constructELFSectionHelper();

    void constructELFSection();

    void countStrSizeAndSymbolNum();

    //Create the element of dynamic section. The element is ELFDyn type.
    //tag: value of d_tag field of ELFDyn.
    //val: value of d_val field of ELFDyn.
    //idx: dynamic element index.
    void createDynamicElement(OUT AssembleBinDescVec & desc_vec,
                              SWord tag, Addr val, UINT idx);

    //Construct symbol of '.symtab' and '.dynsym'.
    void constructSymbolUnull(OUT BYTEVec * sym_bytevec,
                              OUT BYTEVec * dynsym_bytevec);

    //A helper function to extract info from 'abdv' and save it into
    //section content buffer 'bytevec'.
    void extractAssBinDescVec(OUT BYTEVec * bytevec,
                              AssembleBinDescVec const& abdv);

    //Check whether there is relocation info of 'symbol_info'.
    bool existRelaInfo(ELFHdr & hdr, SymbolInfo const* symbol_info);

    //Find SymbolInfo with no-STT_SECTION type via 'original_symbol'.
    bool findSymbolWithNoSectionType(SymbolInfo const* original_symbol,
                                     OUT SymbolInfo ** target_symbol);

    //Generate section content helper function.
    void genSectionContentHelper();

    //The function generate the content of string table by given string list.
    //It will compose string in 'strlst' into a char-vector, and record the
    //byte offset into 'offvec' for each string in 'charvec'.
    void genStrTabContent(OUT CHARVec * charvec, OUT OffVec & offvec,
                          StringList const& strlst);

    //The function generate the content of string table by m_symbol_info.
    void genStrTabContent(OUT CHARVec * charvec);

    //Generate contents for .symtab section.
    //bytevec: binary code of .symtab section content.
    void genSymTabContent(OUT BYTEVec * bytevec, OUT BYTEVec * dynsym_bytevec);

    //Extract all section headers' string name and make up a string-table.
    //offvec: record the byte offset to each string name.
    void genSectHeaderNameStrTabContent(
        OUT CHARVec * charvec, OUT OffVec & offvec);

    //Generate GOT section content.
    void genGotContent();

    //Generate miscellaneous section content.
    virtual void genMiscSectionContent() { return; }

    //Generate RelocInfo which belong to 'symbol_info' according to
    //xoc::Var 'm_var_relocation'.
    void genRelocInfoFromVarRelocation(MOD SymbolInfo * symbol_info);

    //Generate symbol name with integer suffix via 'name_num'.
    //It is used to distinguish multi-symbols with the same name.
    //e.g.: There are more than one SymbolInfo with 'str' name.
    //      The name of these 'str' are: str.1 with number 1 suffix.
    //                                   str.2 with number 2 suffix.
    //                                   str.x with number x suffix.
    //                                           ...
    //      The number of 'x' in 'str.x' is decided by 'name_num'.
    CHAR const* genSymbolNameWithIntSuffix(
        MOD SymbolInfo * symbol_info, UINT name_num);

    //Get SymbolInfo from 'm_symbol_info_map'.
    SymbolInfo * getSymbolInfo(CHAR const* symbol_name);

    size_t getELFFileOffset() { return (size_t)m_file_base_offset; }

    //Get the section name of functional var.
    virtual SECTION_TYPE getSectNameOfFuncVar(Var const* var) const
    {
        ASSERT0(var);
        return SH_TYPE_TEXT;
    }

    //Get the section string name of functional var.
    virtual CHAR const* getSectStrNameOfFuncVar(FunctionInfo const* fi)
    {
        ASSERT0(fi);
        return getSectionName(SH_TYPE_TEXT);
    }

    //Get the element byte size in got section. There may be
    //different size in different architecture.
    virtual UINT getElemByteSizeInGotSect() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the length of instruction according to the different architecture.
    virtual UINT getInstLength() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get substr from 'str' that exclude 'exclude_str'. The 'exclude_str' is
    //in the begin of 'str'.
    //e.g.: Get substr "func_name" from ".text1.func_name" or ".text.func_name".
    //      And 'exclude_str' is ".text1." or ".text.".
    CHAR const* getSubStr(CHAR const* str, CHAR const* exclude_str);

    //Get rela name from 'symtab' via 'idx'.
    CHAR const* getRelaName(ELFSHdr const* symtab, size_t idx);

    //Get dynsym element number.
    UINT getDynSymItemNum() const;

    //Get relocation addend value of relocation type for different arch.
    virtual UINT getRelocAddend(Word reloc_type)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the number of subtext(e.g. .text.xxx).
    UINT getSubTextNum() const { return m_subtext_num; }

    Addr getSectionAddr(CHAR const* sect_name) const;

    Addr getSectionAddr(SECTION_TYPE sect_type) const
    { return getSectionAddr(getSectionName(sect_type)); }

    UINT getSectionSize(CHAR const* sect_name) const;

    UINT getSectionSize(SECTION_TYPE sect_type) const
    { return getSectionSize(getSectionName(sect_type)); }

    //Get the begin index of global symbol in .symtab.
    UINT getGlobalSymbolBeginIndex() const
    { return m_global_symbol_begin_index; }

    //Get SectionInfo via section name.
    SectionInfo * getSection(CHAR const* sect_name) const;

    //Get SectionInfo via section type.
    SectionInfo * getSection(SECTION_TYPE sect_type) const
    { return getSection(getSectionName(sect_type)); }

    //Get SectionInfo via 'sect_name'. If the SectionInfo doesn't exist,
    //it will be created firstly.
    SectionInfo * getSectionWithGenIfNotExist(CHAR const* sect_name);

    //Get SectionInfo via 'sect_name'. If the SectionInfo doesn't exist,
    //it will be created firstly.
    SectionInfo * getSectionWithGenIfNotExist(SECTION_TYPE sect_type)
    { return getSectionWithGenIfNotExist(getSectionName(sect_type)); }

    //Get section name description.
    //Different architecture may own different section description.
    virtual SectionNameDesc const* getSectionNameDesc() const;

    //Get dynamic section description.
    SECTION_TYPE const* getDynamicSectionDesc() const;

    //Get section name via section type.
    //e.g.: given 'SH_TYPE_BSS' and return '.bss'
    xcom::CHAR const* getSectionName(SECTION_TYPE sect_type) const;

    //Get section type via section name.
    //e.g.: given '.bss' and return SH_TYPE_BSS.
    SECTION_TYPE getSectionType(CHAR const* sect_name) const;

    //Get section type via section name.
    //It will extract valid info from 'sect_name' firstly.
    //e.g.:
    //  1.given '.rodata', it will return 'SH_TYPE_RODATA' according to
    //    the valid info '.rodata'.
    //  2.given '.rodata.str.1', it will return 'SH_TYPE_RODATA' according
    //    to the valid info '.rodata' extracted from '.rodata.str.1'.
    SECTION_TYPE getSectionTypeWithSplit(xcom::CHAR const* sect_name);

    //Get section description info.
    SectionNameDesc getSectionDesc(SECTION_TYPE sect_type) const;

    //Get section BYTEVec content.
    BYTEVec * getSectionContent(CHAR const* sect_name) const;

    //Get section BYTEVec content.
    BYTEVec * getSectionContent(SECTION_TYPE sect_type) const
    { return getSectionContent(getSectionName(sect_type)); }

    //Get section CHARVec content.
    CHARVec * getSectionCharVec(CHAR const* sect_name) const;

    //Get section CHARVec content.
    CHARVec * getSectionCharVec(SECTION_TYPE sect_type) const
    { return getSectionCharVec(getSectionName(sect_type)); }

    //Get section order index. Differnet architecture may have different section
    //layout(order). This function must be called after all sections have been
    //created by processSectionInfo() or processCode().
    virtual UINT getSectionIndex(SECTION_TYPE sect_type) const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get section index via 'symbol_info'.
    virtual UINT getSectionIndexViaSymbolInfo(SymbolInfo const* symbol_info)
    {
        ASSERT0(symbol_info);
        SECTION_TYPE sect_type = SYMINFO_sect_type(symbol_info);
        SectionInfo * si = m_sect_map.get(getSectionName(sect_type));
        ASSERT0(si);
        return (UINT)SECTINFO_index(si);
    }

    UINT getShdrNum() const
    { return m_sect_map.get_elem_count(); }

    //Get section name by 'index'.
    CHAR const* getSectionNameByIndex(UINT index)
    { return m_sect_layout[index]; }

    UINT getSectionAlign(CHAR const* sect_name) const
    {
        ASSERT0(m_sect_map.find(sect_name));
        SectionInfo * si = m_sect_map.get(sect_name);
        ASSERT0(si);
        return (UINT)SECTINFO_align(si);
    }

    UINT getSectionAlign(SECTION_TYPE sect_type) const
    { return getSectionAlign(getSectionName(sect_type)); }

    //Get the number of element in .got section.
    UINT64 getGotElemNum() const { return m_got_elem_num; }

    //Get program header via 'idx'.
    ELFPHdr * getProgramHeader(size_t idx) const;

    //Get the number of program header.
    UINT getProgramHeaderNum() const { return PHDR_NUMBER; }

    //Get the number of section according to different architecture.
    virtual UINT getSectionNum() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get function name from 'text_shdr_name'.
    //e.g.: 1.given ".text1" and return ".text1".
    //      2.given ".text.func_name" and return "func_name".
    virtual CHAR const* getFunctionName(CHAR const* text_shdr_name);

    //Get symbol address.
    //This function must be called after section addr have been set.
    Addr getSymbolAddr(SymbolInfo const* symbol_info);

    //Get section header type.
    //e.g.: given ".text1.func_name" and return ".text1".
    CHAR const* getShdrType(CHAR const* shdr_name);

    //Get rela section header type.
    //e.g.: given ".rela.text.func_name" and return ".text"
    CHAR const* getRelaShdrType(CHAR const* shdr_name);

    //Get substr from 'str' via the 'index' of
    //substr after splited by 'delimiter'.
    //e.g.: given string:         ".rela.dl_tdata"
    //      given delimiter:      '.'
    //      substr after splited: ' ' + 'rela' + 'dl_tdata'
    //      substr index:          0       1         2
    CHAR const* getSubStrWithDelimViaIdxAfterSplited(
        CHAR const* str, CHAR const* delimiter, UINT index);

    //Get section content. If it isn't existed, it will be created firstly.
    BYTEVec * getSectContentWithGenIfNotExist(CHAR const* sect_name);

    //Get section content. If it isn't existed, it will be created firstly.
    BYTEVec * getSectContentWithGenIfNotExist(SECTION_TYPE sect_type)
    { return getSectContentWithGenIfNotExist(getSectionName(sect_type)); }

    Off getMaxOffsetOfELF() const { return m_max_offset; }

    Addr getMaxAddrOfELF() const { return m_max_addr; }

    UINT getRelaDynLocalItemNum() const { return m_reladyn_local_item_num; }

    UINT64 getSymbolNum() const { return m_symbol_num; }

    UINT getStrSize() const { return m_symbol_str_len; }

    UINT getStValueOffsetInELFSym() const
    {
        ASSERT0(this->is64bit() || this->is32bit());
        return this->is64bit() ? ELFSYM64_ST_VALUE_OFFSET_BYTE :
                                 ELFSYM32_ST_VALUE_OFFSET_BYTE;
    }

    UINT getStShndxOffsetInELFSym() const
    {
        ASSERT0(this->is64bit() || this->is32bit());
        return this->is64bit() ? ELFSYM64_ST_SHNDX_OFFSET_BYTE :
                                 ELFSYM32_ST_SHNDX_OFFSET_BYTE;
    }

    UINT getStOtherOffsetInELFSym() const
    {
        ASSERT0(this->is64bit() || this->is32bit());
        return this->is64bit() ? ELFSYM64_ST_OTHER_OFFSET_BYTE :
                                 ELFSYM32_ST_OTHER_OFFSET_BYTE;
    }

    //Get st_shndx of a ELFSym when the corresponded section
    //isn't created. It will return a special value as a marker.
    virtual Half getStShndxOfUndefSection()
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Check whether have specific section.
    bool hasSection(SECTION_TYPE sect_type) const
    { return (m_sect_map.find(getSectionName(sect_type))); }

    //Check whether have specific section.
    bool haveSection(CHAR const* sect_name) const
    { ASSERT0(sect_name); return m_sect_map.find(sect_name); }

    //Whether the corresponded reladyn info of 'reloc_info' has been recorded.
    bool haveBeenRecordedRelaDynInfoItem(RelocInfo const* reloc_info,
        OUT RelocInfo ** out_reloc_info);

    //Whether 'reloc_info' needs to be generated a GOT item. The judgement
    //method depend on relocated type according to different architecture.
    virtual bool haveGotItem(RelocInfo const* reloc_info)
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    //Initialize 'm_file'. 'm_file' is pointed to opened file.
    //'pfile' may be opened by outside object(e.g.: ELFAR object).
    void initFileObj(FileObj * pfile) { ASSERT0(pfile); m_file = pfile; }

    void initELFInfo(CHAR const* file_path, bool is_elf_format)
    {
        ASSERT0(file_path);
        m_have_elf_format = is_elf_format;
        m_file_name = processELFName(file_path);
    }

    //Initialize the value of p_offset/p_vaddr/p_paddr of program header.
    void initProgramHeader();

    //Initialize section description info according to the DescInfo that
    //provided by different architecture.
    virtual void initSectionDescInfo();

    //Check whether it is S_UNDEF symbol.
    bool isNullSymbol(SymbolInfo const* symbol_info, UINT symbol_idx);

    //Whether current SymbolInfo is .rela.dyn symbol. There may be
    //different jugement methods in different architecture.
    virtual bool isRelaDynSymbol(Sym const* name, UINT reloc_type)
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    //Whether it is kernel function.
    virtual bool isEntryFunction(SECTION_TYPE sect_type) const { return false; }

    //Whether SymbolInfo is with local attribute.
    bool isSymbolWithLocalAttr(SymbolInfo const* s) const
    {
        ASSERT0(s);
        return ((SYMINFO_sym(s).st_bind == STB_LOCAL) ||
                (GET_SYM_OTHER_VALUE(SYMINFO_sym(s).st_other) == STV_HIDDEN));
    }

    bool isStandardELFFormat() { return m_have_elf_format; }

    //Post process specific info after setting
    //all section info for different architecture.
    virtual void postProcessAfterSettingSectionInfo() { return; }

    void processProgramHeader();

    //A helper function of create foundamental sections
    //according to different architecture.
    virtual void processSectionInfo();

    void processDynamicSection();

    //Process the s_offset field of ELFSHdr type after setting the base addr
    //of section. s_offset field dedicated the section offset in ELF file.
    void processSectionOffset();

    //Process the s_addr field of ELFSHdr type after setting the base addr
    //of section. s_addr field is the address of section.
    void processSectionAddr();

    //Process ELF name. The 'fn' may be a file path.
    //e.g.: 1.given "elf.name" and return "elf.name".
    //      2.given "xx/xx/parent/elf.name" and return "parent/elf.name".
    CHAR const* processELFName(CHAR const* fn);

    void processDataSectionAlign(MOD AssembleBinDescVec & desc_vec,
        MOD SectionInfo * sect_info, MOD SymbolInfo * symbol_info);

    //Process SymbolInfo BSS data when merged section.
    void processBssData(MOD SymbolInfo * symbol_info);

    //Process SymbolInfo data when merged section.
    void processUnullData(MOD SymbolInfo * symbol_info);

    //Process r_offset field of ELFRela.
    virtual Addr processRelaDynOffset(RelocInfo const* reloc_info, UINT & idx);

    //Process r_type field of ELFRela.
    virtual Word processRelaDynType(RelocInfo const* reloc_info)
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Process r_addend field of ELFRela.
    virtual SWord processRelaDynAddend(RelocInfo const* reloc_info);

    //After all sections address have been set, reladyn item that depend on
    //these address will be created and set to .reladyn section content.
    virtual void processRelaDynSectAfterSetSectAddr();

    //Process SymbolInfo according to different st_shndx value of ELFSym.
    bool processSpecialShndx(ELFHdr & hdr, MOD SymbolInfo * symbol_info);

    //Process element offset after section address have been set.
    void postProcessAfterSetSectAddr();

    //Read ELFSym from .symtab section('shdr') via 'idx'.
    void readSymFromSymtabSect(ELFSHdr const* shdr,
                               OUT ELFSym & sym, size_t idx);

    //Read ELFRela from .rela_dyn section('shdr') via 'idx'.
    void readRelaFromRelaTextSect(ELFSHdr const* shdr,
                                  OUT ELFRela & rela, size_t idx);

    //The helper function of writing 32Byte 'value' into the 'addr'(offset in
    //section) of 'sect_name' section. It will be called by relocated function
    //that need to refill data to specific address.
    void refill32ByteContent(CHAR const* sect_name, Addr addr, Addr value);

    Addr readSectionContent(CHAR const* sect_name, Addr addr) const;

    //Set the ELFHdr.e_type field.
    void setELFType(UINT elf_type);

    //Set global symbol index in .symtab.
    void setGlobalSymbolBeginIndex(UINT index)
    { m_global_symbol_begin_index = index; }

    //Set section align.
    void setSectionAlign(SECTION_TYPE sect_type, UINT v)
    {
        ASSERT0(m_sect_map.find(getSectionName(sect_type)));
        SectionInfo * si = getSection(sect_type);
        ASSERT0(si);
        si->m_sect_addr_align = v;
    }

    //Set section header number.
    void setShdrNum(UINT v) { m_shdr_num = v; }

    //Set subText section number.
    void setSubTextNum(UINT v) { m_subtext_num = v; }

    void setGotElemNum(UINT64 v) { m_got_elem_num = v; }

    void setRelaDynLocalItemNum(UINT v) { m_reladyn_local_item_num = v; }

    void setELFFileOffset(UINT64 v) { m_file_base_offset = v; }

    void setMaxOffsetOfELF(Off v) { m_max_offset = v; }

    void setMaxAddrOfELF(Addr v) { m_max_addr = v; }

    void setSymbolNum(UINT64 v) { m_symbol_num = v; }

    void setStrSize(UINT v) { m_symbol_str_len = v; }

    void setSymTab(SymTab * v) { m_sym_tab = v; }

    //Record SymbolInfo into 'm_symbol_info_map' and 'm_symbol_info_vec'.
    void setSymbolInfo(MOD SymbolInfo * sym_info);

    //Create SectionInfo.
    void setSection(SECTION_TYPE sect_type);

    //Create SectionInfo.
    void setSection(SECTION_TYPE sect_type,
                    CHAR const* sect_name, UINT sect_idx);

    //Set SectionInfo.
    void setSectionImple(MOD SectionInfo * si, SECTION_TYPE sect_type);

    //Set flags. There may be specific section flags for different architecture.
    virtual Addr setSectionFlags(SectionNameDesc const* sect_desc)
    { ASSERT0(sect_desc); return SECTDESC_flags(sect_desc); }

    //Set the section order after all sections have been created.
    void setSectionOrder();

    //Write symbol data to section content.
    void setSymbolDataToSection(MOD AssembleBinDescVec & desc_vec,
        MOD SectionInfo * sect_info, MOD SymbolInfo * symbol_info);

    //A helper function to set ELFSym fields using given values.
    void setSymbolValue(MOD ELFSym * sym, Word name, UCHAR bind,
        UCHAR type, UCHAR other, Half shndx, Addr value, Addr size);

    //Set ELFSym into bytevec content.
    void setELFSymToByteVec(MOD BYTEVec * sym_vec, MOD SymbolInfo * symbol_info,
        MOD BYTEVec * local_vec, MOD BYTEVec * global_vec,
        MOD BYTEVec * dynsym_vec, UINT local_idx, UINT global_idx);

    //Record reladyn info into 'm_reladyn_info_vec'.
    void setRelaDynInfo(MOD RelocInfo * reloc_info, UINT & got_ofst,
                        UINT & dynsym_idx);

    //Update st_value of ELFSym after the base address
    //of .symtab/.dynsym section have been set.
    void updateSymOffset(SECTION_TYPE sect_type);

    //Write 'value' into the 'addr' of section(relative offset).
    void writeSectionContent(CHAR const* sect_name, Addr addr, Addr value);
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
typedef xcom::TMapIter<CHAR const*, Vector<UINT>*> RelocSymbolIdxMapIter;
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

    //Allocate output ELFMgr object.
    virtual void allocOutputELFMgr()
    { ASSERTN(0, ("Target Dependent Code")); }

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

    void mergeELFMgr();

    //Output execute ELF.
    bool outputExeELF();
};

}
#endif
