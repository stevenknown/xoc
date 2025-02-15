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

//For section header.
#define SHDR_DYNAMIC_ALIGN      0x8
#define SHDR_GOT_ALIGN          0x10
#define SHDR_TEXT_CODE_ALIGN    1
#define SHDR_SYM_BYTE           24
#define SHDR_SYM_ALIGN          8

#define ELF_ALIGN_1B            1
#define ELF_ALIGN_8B            8
#define ELF_ALIGN_16B           16

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

//The first element in '.dynsym' is UNDEF in ELF format.
//Thus the index of rest elements in '.dynsym' begin from 1.
#define ELF_DYNSYM_ELEM_BEGIN_INDEX       1

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

//Left shift 'bit' operation on 64bits 'val'.
#define LEFT_SHIFT_64BITS_VALUE(val, bit) (((UINT64)(val)) << (bit))

//Number of element in ELFDyn.
#define ELF_NUM_OF_ELEM_IN_ELFDYN         2

//The first element of ELFDyn.
#define ELF_FIRST_ELEM_OF_ELFDYN          1

//Increase value.
#define ELF_INCREASE_VALUE(v)             ((v) + 1)

//The item number of RELA in .dynamic section.
#define ELF_RELA_ITEM_NUM_IN_ELFDYN       3

//The item number of SYM in .dynamic section.
#define ELF_SYM_ITEM_NUM_IN_ELFDYN        2

//The item number of STR in .dynamic section.
#define ELF_STR_ITEM_NUM_IN_ELFDYN        2

//The item number of PLT in .dynamic section.
#define ELF_PLT_ITEM_NUM_IN_ELFDYN        4

//The size of blank symbols in the symbol table.
#define ELF_NULL_SYMBOL_SIZE              1

//The first element size of .got section which be used for
//set .dynamic section address.
#define ELF_FIRST_ELEMENT_SIZE_OF_GOT_SECT 8

//The first element of .got section is used to store the
//begin address of .dynamic section.
#define ELF_BEGIN_INDEX_OF_GOT_ITEM        1

//The begin offset of relaplt item in .got section.
#define ELF_OFFSET_OF_RELA_PLT_IN_GOT_SECT 16

#define DEBUG_ABBREV_SH_NAME       ".debug_abbrev"
#define DEBUG_INFO_SH_NAME         ".debug_info"
#define DEBUG_RELA_INFO_SH_NAME    ".rela.debug_info"
#define DEBUG_RANGES_SH_NAME       ".debug_ranges"
#define DEBUG_RELA_RANGES_SH_NAME  ".rela.debug_ranges"
#define DEBUG_STR_SH_NAME          ".debug_str"
#define DEBUG_LINE_SH_NAME         ".debug_line"
#define DEBUG_RELA_LINE_SH_NAME    ".rela.debug_line"
#define DEBUG_FRAME_SH_NAME        ".debug_frame"
#define DEBUG_RELA_FRAME_SH_NAME   ".rela.debug_frame"
#define DEBUG_LOC_SH_NAME          ".debug_loc"

#define SECTDESC_code(c) (g_section_desc[c].m_desc_sect_type)

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
    EM_NOT_AR_FILE,
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
    SH_TYPE_RELA_DATA,
    SH_TYPE_RELA_SDATA,
    SH_TYPE_CONST,
    SH_TYPE_GOT,
    SH_TYPE_RELA_DYN,
    SH_TYPE_DYNSYM,
    SH_TYPE_FINI,
    SH_TYPE_PREINIT_ARRAY,
    SH_TYPE_DYNAMIC,
    SH_TYPE_INTERP,
    SH_TYPE_DL_TDATA,
    SH_TYPE_RODATA,
    SH_TYPE_RODATA1,
    SH_TYPE_LDM,
    SH_TYPE_DEBUG_INFO,
    SH_TYPE_RELA_DEBUG_INFO,
    SH_TYPE_DEBUG_LINE,
    SH_TYPE_RELA_DEBUG_LINE,
    SH_TYPE_DEBUG_ABBREV,
    SH_TYPE_DEBUG_ARANGES,
    SH_TYPE_DEBUG_RANGES,
    SH_TYPE_RELA_DEBUG_RANGES,
    SH_TYPE_DEBUG_STR,
    SH_TYPE_DEBUG_FRAME,
    SH_TYPE_RELA_DEBUG_FRAME,
    SH_TYPE_DEBUG_LOC,
    SH_TYPE_RELA_DEBUG_LOC,
    SH_TYPE_EH_FRAME,
    SH_TYPE_PLT,
    SH_TYPE_RELA_PLT,
    SH_TYPE_SRODATA,

    #include "sect_type_ext.inc"

    ///////////////////////////////////////////////////////////////////////////
    //DO NOT ADD NEW SH_TYPE AFTER SH_TYPE_MAX_NUM.                          //
    ///////////////////////////////////////////////////////////////////////////
    SH_TYPE_MAX_NUM //The last SH_TYPE code, the number of SH_TYPE code.
} SECTION_TYPE;


#define SWITCH_CASE_DEBUG_SECT  \
    case SH_TYPE_DEBUG_INFO:    \
    case SH_TYPE_DEBUG_LINE:    \
    case SH_TYPE_DEBUG_ABBREV:  \
    case SH_TYPE_DEBUG_ARANGES: \
    case SH_TYPE_DEBUG_RANGES:  \
    case SH_TYPE_DEBUG_FRAME:   \
    case SH_TYPE_DEBUG_STR:     \
    case SH_TYPE_DEBUG_LOC


#define SWITCH_CASE_COMMON_SECT    \
    case SH_TYPE_EH_FRAME:         \
    case SH_TYPE_TEXT:             \
    case SH_TYPE_SBSS:             \
    case SH_TYPE_SDATA:            \
    case SH_TYPE_BSS:              \
    case SH_TYPE_DATA:             \
    case SH_TYPE_SUBTEXT:          \
    case SH_TYPE_RELA:             \
    case SH_TYPE_CONST:            \
    case SH_TYPE_GOT:              \
    case SH_TYPE_RELA_DYN:         \
    case SH_TYPE_DYNSYM:           \
    case SH_TYPE_FINI:             \
    case SH_TYPE_DYNAMIC:          \
    case SH_TYPE_DL_TDATA:         \
    case SH_TYPE_RODATA:           \
    case SH_TYPE_RODATA1:          \
    case SH_TYPE_LDM:              \
    case SH_TYPE_COMMENT:          \
    case SH_TYPE_NOTE:             \
    case SH_TYPE_RELA_DATA:        \
    case SH_TYPE_RELA_SDATA


#define SWITCH_CASE_COMMON_SECT_CONSTRUCT \
    SWITCH_CASE_DEBUG_SECT:               \
    case SH_TYPE_EH_FRAME:                \
    case SH_TYPE_UNDEF:                   \
    case SH_TYPE_SHSTR:                   \
    case SH_TYPE_SYMSTR:                  \
    case SH_TYPE_TEXT:                    \
    case SH_TYPE_SBSS:                    \
    case SH_TYPE_SDATA:                   \
    case SH_TYPE_BSS:                     \
    case SH_TYPE_DATA:                    \
    case SH_TYPE_SUBTEXT:                 \
    case SH_TYPE_CONST:                   \
    case SH_TYPE_GOT:                     \
    case SH_TYPE_FINI:                    \
    case SH_TYPE_PREINIT_ARRAY:           \
    case SH_TYPE_INTERP:                  \
    case SH_TYPE_DL_TDATA:                \
    case SH_TYPE_RODATA:                  \
    case SH_TYPE_RODATA1:                 \
    case SH_TYPE_LDM:                     \
    case SH_TYPE_COMMENT:                 \
    case SH_TYPE_NOTE


#define SWITCH_CASE_COMMON_RELA_SECT_CONSTRUCT \
    case SH_TYPE_RELA_DATA:                    \
    case SH_TYPE_RELA_SDATA:                   \
    case SH_TYPE_RELA_DEBUG_INFO:              \
    case SH_TYPE_RELA_DEBUG_LINE:              \
    case SH_TYPE_RELA_DEBUG_FRAME:             \
    case SH_TYPE_RELA_DEBUG_RANGES:            \
    case SH_TYPE_RELA_DEBUG_LOC                \


#define SWITCH_CASE_COMMON_SECT_OFST \
    SWITCH_CASE_COMMON_SECT:         \
    SWITCH_CASE_DEBUG_SECT:          \
    case SH_TYPE_SHSTR:              \
    case SH_TYPE_SYMSTR:             \
    case SH_TYPE_SYMTAB


#define SWITCH_CASE_COMMON_SECT_ADDR \
    SWITCH_CASE_COMMON_SECT:         \
    case SH_TYPE_INTERP


#define SWITCH_CASE_SECT_BYTE_TYPE  \
    SWITCH_CASE_COMMON_SECT:        \
    case SH_TYPE_UNDEF:             \
    case SH_TYPE_SYMTAB:            \
    case SH_TYPE_INTERP:            \
    case SH_TYPE_PREINIT_ARRAY:     \
    case SH_TYPE_DEBUG_INFO:        \
    case SH_TYPE_DEBUG_LINE:        \
    case SH_TYPE_DEBUG_ABBREV:      \
    case SH_TYPE_DEBUG_ARANGES:     \
    case SH_TYPE_DEBUG_RANGES:      \
    case SH_TYPE_DEBUG_FRAME:       \
    case SH_TYPE_DEBUG_LOC:         \
    case SH_TYPE_RELA_DEBUG_FRAME:  \
    case SH_TYPE_RELA_DEBUG_LINE:   \
    case SH_TYPE_RELA_DEBUG_RANGES: \
    case SH_TYPE_RELA_DEBUG_INFO:   \
    case SH_TYPE_RELA_DEBUG_LOC     \

typedef enum _PROGRAM_HEADER {
    PH_TYPE_UNDEF = 0,
    PH_TYPE_CODE,
    PH_TYPE_DATA,
    PH_TYPE_DYNAMIC,
    PH_TYPE_RELA_PLT,
    PH_TYPE_RISCV_ATTR,
    PH_TYPE_INTERP,
    PH_TYPE_NUMBER,
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

    UINT debug_abbrev_ind; //Index for Debug Abbreviation section
    UINT debug_str_ind;    //Index for Debug Strings section
    UINT debug_info_ind;   //Index for Debug Information section
    UINT debug_ranges_ind; //Index for Debug Ranges section
    UINT debug_line_ind;   //Index for Debug Line section
    UINT debug_frame_ind;  //Index for Debug Frame section
    ELFSymbolOff()
    {
        ::memset((void*)this, 0, sizeof(ELFSymbolOff));
    }
};


#define SECTDESC_type(v)      ((v)->m_desc_sect_type)
#define SECTDESC_shdr_type(v) ((v)->m_desc_shdr_type)
#define SECTDESC_ph_type(v)   ((v)->m_desc_ph_type)
#define SECTDESC_flags(v)     ((v)->m_desc_flags)
#define SECTDESC_align(v)     ((v)->m_desc_addr_align)
#define SECTDESC_entry_sz(v)  ((v)->m_desc_entry_size)
#define SECTDESC_name(v)      ((v)->m_desc_name_str)
//Describe section configure table infomation. Since there are common
//attribute of all ELF sections, it will provide a configure table to
//describe these attribute. Then 'm_sect_desc_info' will be initialized
//according to the table infomation. These info are referenced from 'ELFHdr'.
struct SectionDesc {
    //Record section type(e.g. SH_TYPE_TEXT).
    SECTION_TYPE m_desc_sect_type;

    //Record section shdr type(e.g. S_PROGBITS).
    Word32 m_desc_shdr_type;

    //Record program header type.
    PROGRAM_HEADER m_desc_ph_type;

    //Record section flags(e.g., W(write), A(alloc)).
    Addr m_desc_flags;

    //Record section addr align.
    Addr m_desc_addr_align;

    //Record element size.
    Word m_desc_entry_size;

    //Record section name.
    CHAR const* m_desc_name_str;
};


//
//Start SectionInfo.
//
#define SECTINFO_type(v)       ((v)->m_sect_type)
#define SECTINFO_name(v)       ((v)->m_sect_name_sym)
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
//Describe section infomation which usd to construct ELF. These section info
//are generted according to the section configure table 'SectionDesc' if it
//need to create section. Then the map'm_sect_map' is used to manage all
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

    virtual ~SectionInfoMgr()
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

    CHARVec * allocCharVec()
    {
        CHARVec * c = new CHARVec();
        ASSERT0(c);
        m_charvec_list.append_tail(c);
        return c;
    }

    BYTEVec * allocByteVec()
    {
        BYTEVec * b = new BYTEVec();
        ASSERT0(b);
        m_bytevec_list.append_tail(b);
        return b;
    }

    SectionInfo * allocSectionInfo()
    {
        SectionInfo * s = new SectionInfo();
        ASSERT0(s);
        m_list.append_tail(s);
        return s;
    }

    void initSectionInfo(MOD SectionInfo * si, SECTION_TYPE sect_type);

    virtual void initExtSectionInfo(MOD SectionInfo * si,
                                    SECTION_TYPE sect_type)
    { ASSERTN(0, ("Target Dependent Code")); }
};


//
//Start FunctionInfo.
//
#define FUNCINFO_name(v)          ((v)->m_func_name)
#define FUNCINFO_sect_type(v)     ((v)->m_sect_type)
#define FUNCINFO_sect_name(v)     ((v)->m_sect_name)
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

    //Record file name.
    Sym const* m_func_file_name;

    //Record section name.
    Sym const* m_sect_name;

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
        m_func_file_name = nullptr;
        m_sect_name = nullptr;
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
#define SYMINFO_sect_index(v)    ((v)->m_sect_index)
#define SYMINFO_sect_type(v)     ((v)->m_sect_type)
#define SYMINFO_sect_name(v)     ((v)->m_sect_name_sym)
#define SYMINFO_sym(v)           ((v)->m_sym_elfsym)
#define SYMINFO_index(v)         ((v)->m_sym_index)
#define SYMINFO_size(v)          ((v)->m_sym_size)
#define SYMINFO_func(v)          ((v)->m_func_info)
#define SYMINFO_reloc(v)         ((v)->m_sym_reloc_vec)
#define SYMINFO_func_name(v)     ((v)->m_func_name)
#define SYMINFO_is_func(v)       ((v)->m_sym_is_func)
#define SYMINFO_is_extern(v)     ((v)->m_sym_is_extern)
#define SYMINFO_is_weak(v)       ((v)->m_sym_is_weak)
#define SYMINFO_is_visible(v)    ((v)->m_sym_is_visible)
#define SYMINFO_is_init(v)       ((v)->m_sym_is_init)
#define SYMINFO_is_global(v)     ((v)->m_sym_is_global)
#define SYMINFO_is_dynsym(v)     ((v)->m_sym_is_dynsym)
#define SYMINFO_is_label(v)      ((v)->m_sym_is_label)
#define SYMINFO_is_debug(v)      ((v)->m_sym_is_debug)
#define SYMINFO_file_name(v)     ((v)->m_sym_file_name)
#define SYMINFO_ofst(v)          ((v)->m_sym_offset)
#define SYMINFO_align(v)         ((v)->m_sym_align)
#define SYMINFO_is_byte(v)       ((v)->m_sym_is_byte)
#define SYMINFO_data_byte(v)     ((v)->m_data_byte)
#define SYMINFO_data_word(v)     ((v)->m_data_binword)
#define SYMINFO_dynsym_idx(v)    ((v)->m_dynsym_index)
//The class describes the basic info of a symbol. These info came from xoc::Var
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

    //Record whether it is debug symbol.
    bool m_sym_is_debug;

    //Record whether it is label symbol.
    bool m_sym_is_label;

    //Record the index of related section.
    UINT m_sect_index;

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

    //Record section sym name to which SymbolInfo belong.
    Sym const* m_sect_name_sym;

    //Record corresponded function name if it is functional type or it is a
    //intermediate variables within the function.
    Sym const* m_func_name;

    //Record symbol name.
    Sym const* m_sym_name;

    //Record file name.
    Sym const* m_sym_file_name;

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
        m_sym_is_label = false;
        m_sect_type = SH_TYPE_UNDEF;
        m_sect_index = 0;
        m_sym_align = 0;
        m_sym_is_debug = false;
        m_sym_size = 0;
        m_dynsym_index = 0;
        m_sym_index = 0;
        m_sym_offset = 0;
        m_data_binword = 0;
        m_data_byte = nullptr;
        m_func_name = nullptr;
        m_sym_name = nullptr;
        m_sym_file_name = nullptr;
        m_func_info = nullptr;
    }

    ~SymbolInfo() {}

    //Return binary codes of current symbol.
    BYTEVec & getSymbolCode() const
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
#define RELOCINFO_sect_name(v)       ((v)->m_reloc_sect_name_sym)
class RelocInfo {
    COPY_CONSTRUCTOR(RelocInfo);
public:
    //Record whether relocated symbol has been resolved.
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

    //Record the section name of the relocated symbol.
    //e.g. '.rela.text1', '.rela.rodata', '.rela.dl_tdata'
    //           .text1         .rodata         .dl_tdata
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
#define RELADYNINFO_got_ofst(v)   ((v)->m_got_ofst)
#define RELADYNINFO_addend(v)     (RELOCINFO_addend(((v)->m_reloc_info)))
#define RELADYNINFO_type(v)       (RELOCINFO_type((v)->m_reloc_info))
#define RELADYNINFO_ofst(v)       (RELOCINFO_called_loc((v)->m_reloc_info))
#define RELADYNINFO_sym_idx(v) \
        (SYMINFO_dynsym_idx(RELOCINFO_sym((v)->m_reloc_info)))
#define RELADYNINFO_sym_name(v) \
        (SYMINFO_name(RELOCINFO_sym((v)->m_reloc_info)))
#define RELADYNINFO_caller_sym_name(v) \
        (RELOCINFO_sect_name((v)->m_reloc_info))
//The class describes the basic info of reladyn item which mainly comes from
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
    Off m_got_ofst;

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


#define PLTINFO_addr(p)          ((p)->m_plt_addr)
#define PLTINFO_reloc(p)         ((p)->m_plt_reloc_info)
#define PLTINFO_is_local(p)      ((p)->m_plt_is_local)
#define PLTINFO_got_ofst(p)      ((p)->m_plt_got_offset)
#define PLTINFO_loc_ofst(p)      ((p)->m_plt_loc_offset)
#define PLTINFO_is_plt_addr(p)   ((p)->m_plt_is_plt_addr)
#define PLTINFO_is_first_elem(p) ((p)->m_plt_is_first_elem)
//The class describes the info of both '.plt' and '.relaplt' section.
class PltInfo {
    COPY_CONSTRUCTOR(PltInfo);
public:
    //Whether it is the first element of '.plt' section. The first element
    //in '.plt' section is the content of runtime resolved function.
    bool m_plt_is_first_elem;

    //Record the type of PltInfo. 'true' represents that this PltInfo doesn't
    //need to be created corresponded plt element in '.plt' section.
    bool m_plt_is_local;

    //Whether 'm_plt_addr' record the address of PltInfo in '.plt' section.
    //e.g.: '0x1d0' is the address of PltInfo.
    //      00001d0 <func@plt>:
    //       1d0: xxxx
    //       1d4: xxxx
    //       ...   ...
    bool m_plt_is_plt_addr;

    //Record target address that need to be refilled in PltInfo. This address
    //is pointed to the item of '.got' section that recoreded the physical
    //function address and will be refilled in runtime.
    Off m_plt_loc_offset;

    //Record the offset of PltInfo that need to be refilled in '.got' section.
    //e.g.: '150' of 'Offset' is the offset of '.got' section.
    //      Offset  Info          Type        Sym. Value   Sym.  Name + Addend
    //       150   00020005  R_RISCV_JUMP_SLOT     00464     set_begin + 0
    Off m_plt_got_offset;

    //Record the address of PltInfo in '.plt' section.
    //e.g.: '0x1d0' is the address of PltInfo.
    //      00001d0 <func@plt>:
    //       1d0: xxxx
    //       1d4: xxxx
    //       ...   ...
    //Or record the offset of PltInfo in target section.
    Addr m_plt_addr;

    //Record the corresponded RelocInfo.
    RelocInfo * m_plt_reloc_info;
public:
    PltInfo()
    {
        m_plt_is_first_elem = false;
        m_plt_is_local = false;
        m_plt_is_plt_addr = false;
        m_plt_loc_offset = 0;
        m_plt_got_offset = 0;
        m_plt_addr = 0;
        m_plt_reloc_info = nullptr;
    }

    ~PltInfo() {}
};


//The class manages PltInfo object resources. It creates PltInfo object
//and uses xoc::List 'm_list' to record. These resources would be freed
//when the destructor is called.
class PltInfoMgr {
    COPY_CONSTRUCTOR(PltInfoMgr);
protected:
    //Record PltInfo object.
    xcom::List<PltInfo*> m_list;
public:
    PltInfoMgr() { m_list.init(); }

    ~PltInfoMgr()
    {
        for (PltInfo * pi = m_list.get_head();
             pi != nullptr; pi = m_list.get_next()) {
            if (pi != nullptr) { delete pi; }
        }
        m_list.clean();
    }

    PltInfo * allocPltInfo()
    {
        PltInfo * pi = new PltInfo();
        ASSERT0(pi);
        m_list.append_tail(pi);
        return pi;
    }
};


//The class describes generated mapped of SymMap.
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


typedef xcom::TMap<Sym const*, SymbolInfo*,
    CompareKeyBase<Sym const*>, GenMappedOfSymMap> SymMapType;
//The class describes a Map for SymbolInfo. It provides GenMappedOfSymMap to
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

    SectionInfo * createMapped(Sym const* s);
};


typedef xcom::TMap<Sym const*, SectionInfo*,
    CompareKeyBase<Sym const*>, GenMappedOfSectInfoMap> SectionInfoMapType;
//The class describes a Map for SectionInfo. It provides GenMappedOfSectInfoMap
//to decrease the number of lookups RBT when SectionInfo is recorded or updated.
class SectionInfoMap : public xcom::TMap<Sym const*, SectionInfo*,
    CompareKeyBase<Sym const*>, GenMappedOfSectInfoMap> {
    COPY_CONSTRUCTOR(SectionInfoMap);
public:
    SectionInfoMap(ELFMgr * elf_mgr, SectionInfoMgr * sect_mgr)
    {
        SectionInfoMapType::m_gm.m_elf_mgr = elf_mgr;

        SectionInfoMapType::m_gm.m_sect_mgr = sect_mgr;
    }

    ~SectionInfoMap() {}
};

class ELFARMgr;

//SymbolInfo.
typedef xcom::Vector<SymbolInfo*> SymbolInfoVec;
typedef xcom::Vector<SymbolInfoVec*> SymtabInfoVec;
typedef xcom::TMap<Sym const*, SymbolInfo*> SymbolInfoMap;
typedef xcom::TMapIter<Sym const*, SymbolInfo*> SymbolInfoMapIter;
//RelocInfo.
typedef xcom::Vector<RelocInfo*> RelocInfoVec;
//SectionInfoMap.
typedef xcom::TMapIter<Sym const*, SectionInfo*> SectionInfoMapIter;
//AliasSymbolMap.
typedef xcom::TMap<Sym const*, Sym const*> AliasSymbolMap;
typedef xcom::TMapIter<Sym const*, Sym const*> AliasSymbolMapIter;
//The map of section name and section type.
typedef xcom::TMap<Sym const*, SECTION_TYPE> SectionNameAndTypeMap;
//Record the map of section and it's index in output ELF.
typedef xcom::TMap<SECTION_TYPE, UINT> SectionIndexMap;
//Program header vector.
typedef xcom::Vector<ELFPHdr*> PhdrVec;
//The map of PltInfo.
typedef xcom::TMap<Sym const*, PltInfo*> PltInfoMap;
//The vector of RelapltInfo.
typedef xcom::Vector<RelaDynInfo*> RelaPltInfoVec;
//The vector of PltInfo.
typedef xcom::Vector<PltInfo*> PltInfoVec;
//
//Start ELFMgr.
//
//Record symbols and their information.
#define ELFMGR_symbol_info(e) ((e)->m_symbol_info)
#define ELFMGR_symtab_vec(e)  ((e)->m_symtab_info_vec)
#define ELFMGR_symbol_vec(e)  ((e)->m_symbol_info_vec)
#define ELFMGR_symbol_map(e)  ((e)->m_symbol_info_map)
#define ELFMGR_reloc_vec(e)   ((e)->m_reloc_info)
#define ELFMGR_alias_map(e)   ((e)->m_alias_symbol_map)
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

    //Dwarf mgr.
    MCDwarfMgr * m_dm;

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

    //Record all symbol names in the order of local symbols first and global
    //symbols last.
    xcom::List<Sym const*> m_symbol_name;
public:
    //Record name of generated binary file.
    CHAR const* m_output_file_name;
protected:
    virtual void allocTargInfo() { ASSERTN(0, ("Target Dependent Code")); }
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

    //Whether another section is needed to describe the architecture
    //information.
    virtual bool isNeedAttributeSection() { return false; }

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
            other, get64BitReferRelocType(), offset, addend);
    }

    //Allocate and set RelocInfo of 'symbol_info'.
    void addSymRelocInfo(MOD SymbolInfo * symbol_info,
        Sym const* other, UINT type, UINT offset, UINT addend);

    //Add the final relocation entry for a certain fixup.
    void addRelocForElfSymbol(MCFixup * fixup_entry, MCSymbol const* mc_symbol,
                              SymbolInfo const* elf_sym_info,
                              Sym const* callee);

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

    //Construct ELF header based on the section header number.
    void constructELFHeader(UINT sthr_num);

    void dump() const;
    void dumpStrTabContent(CHAR const* strtab, Addr size) const;

    //Since user-defined functions have been saved, this interface only
    //collect un-user-defined symbols.
    void extractSymbolExceptUserDefFunc();

    //Generate content of .xxx.attributes section.
    virtual void getAttributeSectionContent(OUT BYTEVec & buf)
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //Get s_size of .xxx.attributes section.
    virtual Addr getAttributeSectionLength()
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get s_name_str of .xxx.attributes section.
    virtual CHAR const* getAttributeSectionName()
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Get s_type of .xxx.attributes section.
    virtual Word32 getAttributeSectionType()
    { ASSERTN(0, ("Target Dependent Code")); return ELF_VAL_UNDEF; }

    //Generate relocation entries for a debug section,
    //not all fixups will generate relocation entries.
    //e.g: for a var, once its position is determined,
    //and it's filled into the corresponding section,
    //we do not need to generate a relocation entry.
    void genDebugReloc(SymbolInfo const* elf_sym_info,
                       Vector<MCFixup*> const& fxiups,
                       BYTEVec & debug_code);

    //Generate relocation entries for MCExpr of type SymbolRef.
    void genSymbolRefReloc(MCExpr const* value, SymbolInfo const* elf_sym_info,
                           MCFixup * fixup_entry, BYTEVec & debug_code);

    //Generate phdr content according to 'm_phdr_vec' info.
    void genPhdrContent();

    //Returns binary codes of given symbol.
    BYTEVec & getSymbolCode(Sym const* sym)
    {
        ASSERT0(sym && m_symbol_info.find(sym));
        return m_symbol_info.get(sym)->getSymbolCode();
    }

    //Return ELF file bit-width type.
    CHAR const* getClassName() const;

    //Get the value of "Flags" field in ELF header.
    virtual Word32 getEHFlag() const
    { ASSERTN(0, ("Target Dependent Code")); return EF_UNDEF; }

    //Get the value of "Machine" field in ELF header.
    virtual Half getEHMachine() const
    { ASSERTN(0, ("Target Dependent Code")); return ET_NONE; }

    //Return endian name.
    CHAR const* getEndianName() const;

    //Return ELF file type.
    CHAR const* getFileTypeName() const;

    ELFHdr & getHdr() { return m_elf_hdr; }

    //get dwarf mgr
    MCDwarfMgr * getDwarfMgr() const { return m_dm; }

    //Get the relocation type for 64-bit reference relocation.
    //This virtual function should be overridden in derived classes to
    //return the type of 64-bit reference relocation specific to the
    //e.g:
    //Offset       Info       Type            Sym. Value  Sym. Name + Addend
    //x              x     R_SWAI_64_REFQUAD    x    x      x    x
    //This function should return the type `R_SWAI_64_REFQUAD`.
    virtual UINT get64BitReferRelocType() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the relocation type for 32-bit reference relocation.
    //This virtual function should be overridden in derived classes to
    //return the type of 32-bit reference relocation specific to the
    //target.
    //eg:
    //Offset       Info       Type            Sym. Value  Sym. Name + Addend
    //x              x     R_SWAI_64_REFLONG   x    x      x    x
    //This function should return the type `R_SWAI_64_REFLONG`.
    virtual UINT get32BitReferRelocType() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get r_sym value of relocation entry.
    virtual Word getRelocSymValue(RelocInfo const* reloc_info)
    { return SYMINFO_index(m_symbol_info.get(RELOCINFO_name(reloc_info))); }

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

    CHAR const* getOutputFileName() const { return m_output_file_name; }

    //Get flag of stack in HBM.
    //This flag will be used to set the flags for the function section
    //in the future (note that only the entry function needs to be set).
    //If set, the stack will be allocated in HBM,
    //otherwise, it would be on the core.
    //which means the stack address space will be wide.
    virtual UINT getFlagStackHBM() const { return 0; }

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

    UINT getRelTypeFromDwarf(MCFixupKind kind);

    //Initialize dump file info. 'filename' is the name of dump file.
    //The flag of 'is_del' indicates whether 'UNLINK()' function will
    //be called to remove old dump file with the same name of 'filename'.
    //Return EM_SUCC if the initialization is successful.
    EM_STATUS initdumpfile(CHAR const* filename, bool is_del = false);

    //Initialize dump file info.
    //'filehandler': the file handler of dump file.
    //Return EM_SUCC if the initialization is successful.
    EM_STATUS initdumpfile(FILE * filehandler);

    //Initialize dump file info. The dump file is 'stdout'.
    //Return EM_SUCC if the initialization is successful.
    EM_STATUS initdumpscr();

    //Initialize the symbol information and save it to m_symbol_info by modules
    //outside the ELFMgr module.
    void initSymbol(xoc::Var const* var, Sym const* func_name = nullptr,
        SECTION_TYPE sect_type = SH_TYPE_UNDEF, UINT sect_ofst = 0,
        UCHAR symbol_type = STT_NOTYPE);

    //Initialize the section's symbol information based on
    //the section var passed from the frontend, such as debug_line.
    void initSymSection(xoc::Var const* var);

    //Initialize the info of ELFMgr.
    void initELFMgr();

    bool is64bit() const { return m_elf_hdr.is64bit(); }
    bool is32bit() const { return m_elf_hdr.is32bit(); }

    bool isExecutable() const;

    //Whether variable is aligned by given value.
    bool isSizeAligned(UINT sz, UINT val) { return sz % val == 0; }

    //Judge whether size of variable is valid by less than or equal to judging
    //whether the value is less than or equal to BIN_WORD_SIZE;
    bool isSizeValid(UINT sz) { return sz <= BIN_WORD_SIZE; }

    //Whether info of var should be wrote into ELF file.
    bool isVarAvailable(xoc::Var const* var)
    {
        return var && (var->is_global() || var->is_func()) &&
            !var->is_fake() && !var->is_unallocable();
    }

    //Judge section type of 'var' according to different architecture.
    virtual SECTION_TYPE judgeSectWhichVarBelongTo(xoc::Var const* var);

    //Judge section type of 'function var' for output ELF with ET_REL type.
    virtual SECTION_TYPE judgeSectWhichFuncBelongTo(xoc::Var const* var)
    { ASSERTN(0, ("Target Dependent Code")); return SH_TYPE_UNDEF; }

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

    //Set dwarf mgr.
    void setDwarfMgr(MCDwarfMgr * dm) { ASSERT0(dm); m_dm = dm; }

    //Set name of generated binary file.
    void setOutputFileName(CHAR const* name)
    {
        m_output_file_name = (CHAR*)xmalloc(::strlen(name) + 1);
        ::memcpy((void*)m_output_file_name, name, ::strlen(name) + 1);
    }

    //Note section content size should be ready.
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

protected:
    //Record whether the ELFMgr is with ELF format. 'true' if the ELFMgr
    //info collected from ELF file. 'false' if the ELFMgr info collected
    //from xoc::Var.
    bool m_have_elf_format;

    //Record the number of section header.
    UINT m_shdr_num;

    //Record the number of subtext.
    UINT m_subtext_num;

    //The order of symbols in '.symtab' are sorted by their 'st_bind' attribute.
    //And symbols with 'STB_GLOBAL' value are placed in the back of '.symtab'.
    //Since the index of the first symbol with 'STB_GLOBAL' value in '.symtab'
    //needs to be recorded into the 's_info' field of '.symtab' shdr, thus this
    //variable is used to record the index.
    //e.g.: Symbol table '.symtab' contains 26 entries:
    //      -----------------------------------------------------------------
    //      Num:    Value    Size    Type    Bind    Vis    Ndx    Name
    //        0:    000000    0      NOTYPE  LOCAL  DEFAULT  UND
    //        1:    000000    0      FILE    LOCAL  DEFAULT  ABS   main.cpp
    //        ...                            LOCAL
    //        8:    000000    0      SECTION LOCAL  DEFAULT   6
    //        9:    000001    1      OBJECT  GLOBAL DEFAULT   3    char_1
    //        ...                            GLOBAL
    //      -----------------------------------------------------------------
    //  The index of the first symbol('char_1') with 'STB_GLOBAL' value is 9.
    UINT m_global_symbol_begin_index_of_symtab;

    //The order of symbols in '.dynsym' are sorted by their 'st_bind' attribute.
    //And symbols with 'STB_GLOBAL' value are placed in the back of '.dynsym'.
    //Since the index of the first symbol with 'STB_GLOBAL' value in '.dynsym'
    //needs to be recorded into the 's_info' field of '.dynsym' shdr, thus this
    //variable is used to record the index.
    //e.g.: Symbol table '.dynsym' contains 4 entries:
    //      -----------------------------------------------------------------
    //      Num:    Value    Size    Type    Bind    Vis    Ndx    Name
    //        0:    000000    0      NOTYPE  LOCAL  DEFAULT  UND
    //        1:    000001    1      OBJECT  GLOBAL DEFAULT   3    char_1
    //        ...                            GLOBAL
    //      -----------------------------------------------------------------
    //  The index of the first symbol('char_1') with 'STB_GLOBAL' value is 1.
    UINT m_global_symbol_begin_index_of_dynsym;

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

    //Record string name for decreasing lookup time.
    SymTab * m_sym_tab;

    //SectionInfo Mgr. Create and free SectionInfo resources.
    SectionInfoMgr * m_sect_mgr;

    //Record section with order. The order info come from getSectionIndex().
    xcom::Vector<Sym const*> m_sect_layout;

    //Record FunctionInfo.
    xcom::Vector<FunctionInfo*> m_func_info;

    //Record reladyn info.
    xcom::Vector<RelaDynInfo*> m_reladyn_info_vec;

    //Record the map of section and it's index.
    SectionIndexMap m_section_index_map;

    //SymbolInfo Mgr. Create and free SymbolInfo resources.
    SymbolInfoMgr m_sym_mgr;

    //FunctionInfo Mgr. Create and free FunctionInfo resources.
    FunctionInfoMgr m_func_mgr;

    //RelocInfo Mgr. Create and free RelocInfo resources.
    RelocInfoMgr m_reloc_mgr;

    //RelaDynInfo Mgr. Create and free ReladynInfo resources.
    RelaDynInfoMgr m_reladyn_mgr;

    //PltInfo Mgr. Create and free PltInfo resources.
    PltInfoMgr m_plt_info_mgr;

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
    //Whether this ELFMgr has been recorded in LinkerMgr.
    bool m_has_recorded_in_linkermgr;

    //Record the index of text section in output ELF with ET_REL type.
    //Since there may be more than one '.function_name.text' section.
    UINT m_text_index;

    //Record the offset in '.got' section. Since the info of '.rela.plt'
    //and 'rela.rela' will be generated element in '.got' section.
    Off m_got_offset;

    //Record the index of dynsym element in '.dynsym' section.
    UINT m_dynsym_idx;

    //Record the file name of ELFMgr.
    CHAR const* m_file_name;

    //Record SymbolInfo collected from xoc::Var.
    SymMap m_symbol_info;

    //Record SmbolInfo collected from ELF and xoc::Var.
    SymbolInfoMap m_symbol_info_map;

    //Record SymbolInfo collected from ELF and xoc::Var.
    SymbolInfoVec m_symbol_info_vec;

    //Record RelocInfo.
    RelocInfoVec m_reloc_info;

    //Record all 'm_symbol_info_vec'.
    SymtabInfoVec m_symtab_info_vec;

    //Record phdr.
    PhdrVec m_phdr_vec;

    //Record relaplt info.
    RelaPltInfoVec m_relaplt_info_vec;

    //Record PltInfo.
    PltInfoVec m_plt_info_vec;

    //Record PltInfo.
    PltInfoMap m_plt_info_map;

    //Record ELF SectionInfo.
    SectionInfoMap * m_sect_map;

    //Alias is a second name of the aliasee symbol. These two symbols are point
    //to the same content and can be used interchangeably. Alias is an important
    //feature for low-level systems programming, but it isn't standardized in C
    //or C++. Thus this syntax is not recommended except for generated by other
    //toolchain modules like GNU C/C++ or LLVM. Here we only support its basic
    //functionality for accomplishing the goal of compiling certain statements.
    //  a.The usage of alias in GNU C/C++ programming. It can defines an alias
    //for variable or function symbol.
    //    e.g.:
    //    For alias variable.
    //       int var_target;
    //       int __attribute__ ((alias ("var_target"))) var_alias;
    //    For alias function.
    //       extern "C" int func() { }
    //       int func_alias() __attribute__((alias("func")));
    //  b.The usage of alias in LLVM programming. Alias has a name and an
    //aliasee that is either a global value or a constant expression.
    //    e.g.:
    //       @<Name> = [option] alias <AliaseeTy>, <AliaseeTy> * @<Aliasee>;
    //  One of the scenarios we encountered was an alias name generated for
    //constructor would be called by other moules unit. And aliasee name was
    //only used by the module unit to which the defined function belong.
    //  c.The alias syntax we support is: ".alias alias, aliasee;". It can only
    //define an alias to existing function symbol.
    //    e.g.:
    //       1. void func_aliasee(param1, param2) { }  //existing function
    //       2. void func_alias(param1, param2);       //declare alias function
    //       3. .alias func_alias, func_aliasee;       //alias statement
    //       4. call func_alias;                       //alias called
    //  d.Alias processing. Firstly, 'func_alias' and it's corresponded
    //'func_aliasee' info will be recorded into a map 'm_alias_symbol_map' to
    //which belong ELFMgr when the frontend parsing encounters alias statement.
    //Except for this statement, other statements are handled as usual in
    //frontend parsing. Then 'm_alias_symbol_map' info in different ELFMgr will
    //be merged into a 'm_alias_symbol_map' to which belong LinkerMgr. Finally,
    //alias symbol will be replaced by aliasee symbol in the beginning of linker
    //phase. Thus the actual function name being called is aliasee name. Since
    //alias function has been declared before alias statement defined, a symbol
    //with alias name and function type is still generated in the '.symtab' of
    //output ELF. But it's content is empty and this symbol shouldn't be used
    //to resolve other RelocInfo.
    //Record alias symbol and it's corresponded aliasee symbol.
    AliasSymbolMap m_alias_symbol_map;

    //Record the map of section name and section type.
    SectionNameAndTypeMap m_sect_name_type_map;

    //Add 's' into 'm_sym_tab'.
    Sym const* addToSymTab(CHAR const* s)
    { ASSERT0(s && m_sym_tab); return m_sym_tab->add(s); }

    //Allocate SectionInfoMgr object according to different architecture.
    virtual SectionInfoMgr * allocSectionInfoMgr()
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Allocate program header according to the given 'phnum'.
    void allocProgramHeader(UINT phnum);

    //Collect function/symbol info from xoc::Var.
    void collectELFInfoFromVar();

    //Collect debug info of 'symbol_info' that come from xoc::Var.
    void collectDebugInfo(MOD SymbolInfo const* symbol_info);

    //Collect the code, code size and offset info of function.
    //bytevec: record the binary code of function.
    //index: function index in all functions.
    void collectFunctionInfo(SymbolInfo const* symbol_info);

    //Collect function info from 'hdr' if the 'symbol_info' with FUNC type.
    void collectFuncInfoForSymbol(ELFHdr & hdr, MOD SymbolInfo * symbol_info);

    //Collect symtab info from xoc::Var.
    void collectSymtabInfoFromVar();

    //Collect symbol and function info from ELF that has been read into memory.
    void collectObjELF();

    //The implement function of collected OBJ ELF.
    virtual void collectObjELFImpl(
        ELFHdr & hdr, ELFSHdr const* shdr, UINT shdr_idx);

    //Collect section name to which SymbolInfo belong.
    void collectSymbolInfoSectionName(
        ELFHdr & hdr, MOD SymbolInfo * symbol_info);

    //Collect function info from 'text_shdr'.
    FunctionInfo * collectTextInfo(ELFSHdr const* text_shdr,
        ELFSHdr const* strtab_shdr, SymbolInfo const* symbol_info);

    //Collect RelocInfo from SymbolInfo.
    void collectRelocInfo(MOD SymbolInfo * symbol_info);

    //Collect RelocInfo from 'hdr'.
    void collectRelocInfo(ELFHdr & hdr, ELFSHdr const* rela_shdr);

    //Collect SymtabInfo from 'hdr'.
    void collectSymtabInfo(ELFHdr & hdr, ELFSHdr const* sym_shdr);

    //TODO:
    //Collect GroupInfo from 'hdr'.
    void collectGroupInfo(ELFHdr & hdr, ELFSHdr const* group_shdr)
    { ASSERT0(group_shdr); return; }

    //Construct null symbol of '.symtab' and '.dynsym'.
    //It is the first symbol in symtab and it's values are all zero.
    void constructSymbolNull(OUT BYTEVec * bytevec);

    //A helper function of construct ELF section.
    void constructELFSectionHelper();

    void constructELFSection();

    //Construct extend ELF section according to different architecture.
    virtual void constructExtELFSection(MOD SectionInfo * sect_info,
        SECTION_TYPE sect_type, MOD ELFSHdr * shdr)
    { ASSERTN(0, ("Target Dependent Code")); }

    void countStrSizeAndSymbolNum();

    //Count the number of section according to the section type.
    virtual UINT countSectNumViaSectType(SECTION_TYPE sect_type);

    //Create the element of dynamic section. The element is ELFDyn type.
    //tag: value of d_tag field of ELFDyn.
    //val: value of d_val field of ELFDyn.
    //idx: dynamic element index.
    void createDynamicElement(OUT AssembleBinDescVec & desc_vec,
                              SWord tag, Addr val, UINT idx);

    //Create '.text' section according to the FunctionInfo.
    void createTextSect(MOD FunctionInfo * fi);

    //Create '.rela.text' section according to SymbolInfo.
    void createRelaTextSect(MOD SymbolInfo * symbol_info);

    //Create '.rela.debug.xx' section according to SymbolInfo.
    void createRelaDebugSect(SymbolInfo const* symbol_info);

    //Create '.rela.data' section according to SymbolInfo.
    void createRelaDataSect(SymbolInfo const* symbol_info);

    //A helper function of create foundamental sections
    //according to different architecture.
    virtual void createFundamentalSection();

    //Construct symbol of '.symtab' and '.dynsym'.
    void constructSymbolUnull();

    //A helper function to extract info from 'abdv' and save it into
    //section content buffer 'bytevec'.
    void extractAssBinDescVec(OUT BYTEVec * bytevec,
                              AssembleBinDescVec const& abdv);

    //Check whether there is relocation info of 'symbol_info'.
    bool existRelaInfo(ELFHdr & hdr, SymbolInfo const* symbol_info);

    //Find SymbolInfo with no-STT_SECTION type via 'original_symbol'.
    bool findSymbolWithNoSectionType(SymbolInfo const* original_symbol,
                                     OUT SymbolInfo ** target_symbol);

    //Find SymbolInfo according to the 'symbol_name'.
    SymbolInfo * findSymbol(Sym const* symbol_name);

    //Generate section content helper function.
    void genSectionContentHelper();

    //The function generate the content of string table by given string list.
    //It will compose string in 'strlst' into a char-vector, and record the
    //byte offset into 'offvec' for each string in 'charvec'.
    void genStrTabContent(OUT CHARVec * charvec, OUT OffVec & offvec,
                          StringList const& strlst);

    //The function generate the content of string table by m_symbol_info.
    void genStrTabContent();

    //Generate contents for .symtab and .dynsym section.
    void genSymTabContent();

    //Extract all section headers' string name and make up a string-table.
    //offvec: record the byte offset to each string name.
    void genSectHeaderNameStrTabContent(
        OUT CHARVec * charvec, OUT OffVec & offvec);

    //Generate GOT section content.
    void genGotContent();

    //Generate miscellaneous section content.
    virtual void genMiscSectionContent() { return; }

    //Generate symbol name with integer suffix via 'name_num'.
    //It is used to distinguish multi-symbols with the same name.
    //e.g.: There are more than one SymbolInfo with 'str' name.
    //      The name of these 'str' are: str.R_1 with number 1 suffix.
    //                                   str.R_2 with number 2 suffix.
    //                                   str.R_x with number x suffix.
    //                                           ...
    //      The number of 'x' in 'str.x' is decided by 'name_num'.
    CHAR const* genSymbolNameWithIntSuffix(
        MOD SymbolInfo * symbol_info, UINT name_num);

    //Generate contents for .rel.text.xxx section.
    //bytevec: binary code of relocation info of function.
    //reloc:   saved all relocation entries for current symbol.
    void genRelocContent(OUT BYTEVec * bytevec, RelocInfoVec const& reloc_vec);

    //The helper function of generated section content.
    void genCommonSectionContentHelper();

    //Generate '.text' and '.rela.text' section content.
    void genRelaTextSectionContent(MOD SymbolInfo * symbol_info);

    //Generate '.rela.xxx' section content.
    void genRelaSectionContent(MOD SymbolInfo * symbol_info);

    //Generate and initialize phdr info.
    ELFPHdr * genAndInitPhdr(PROGRAM_HEADER phdr_type,
                             SectionInfo const* sect_info);

    //Get rela text section type.
    virtual SECTION_TYPE getRelaTextSectType(MOD SymbolInfo * symbol_info)
    { ASSERT0(symbol_info && SYMINFO_func(symbol_info)); return SH_TYPE_RELA; }

    //Get sub-name of rela text section. e.g. '.rela.text.' of rela text
    //section '.rela.text.func_name' and other '.rela.xxx.' sub name of
    //rela text section in different architecture.
    virtual CHAR const* getSubNameOfRelaTextSect(MOD SymbolInfo * symbol_info)
    { ASSERT0(symbol_info && SYMINFO_func(symbol_info)); return RELA_SH_NAME; }

    //Get SymbolInfo from 'm_symbol_info_map'.
    SymbolInfo * getSymbolInfo(Sym const* symbol_name);

    size_t getELFFileOffset() { return (size_t)m_file_base_offset; }

    //Get the section name of function.
    virtual SECTION_TYPE getSectNameOfFunc(FunctionInfo const* fi) const
    { return SH_TYPE_TEXT; }

    //Get the section name of functional var.
    virtual Sym const* getSectNameOfFuncVar(FunctionInfo const* fi)
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

    Addr getSectionAddr(Sym const* sect_name) const;

    Addr getSectionAddr(SECTION_TYPE sect_name)
    { return getSectionAddr(getSectionName(sect_name)); }

    UINT getSectionSize(Sym const* sect_name);

    UINT getSectionSize(SECTION_TYPE sect_type)
    { return getSectionSize(getSectionName(sect_type)); }

    //Get the begin index of global symbol in .symtab section.
    UINT getGlobalSymbolBeginIndexOfSymtab() const
    { return m_global_symbol_begin_index_of_symtab; }

    //Get the begin index of global symbol in .dynsym section.
    UINT getGlobalSymbolBeginIndexOfDynSym() const
    { return m_global_symbol_begin_index_of_dynsym; }

    //Get the begin index of global symbol in .symtab.
    UINT getGlobalSymbolBeginIndex() const
    { return m_global_symbol_begin_index; }

    SectionInfo * getSection(Sym const* sect_name) const;

    //Get SectionInfo via section type.
    SectionInfo * getSection(SECTION_TYPE sect_type)
    { return getSection(getSectionName(sect_type)); }

    //Get SectionInfo via 'sect_name'. If the SectionInfo doesn't exist,
    //it will be created firstly.
    SectionInfo * getSectionWithGenIfNotExist(Sym const* sect_name);

    //Get SectionInfo via 'sect_type'. If the SectionInfo doesn't exist,
    //it will be created firstly.
    SectionInfo * getSectionWithGenIfNotExist(SECTION_TYPE sect_type)
    { return getSectionWithGenIfNotExist(getSectionName(sect_type)); }

    //Get element from section description table.
    SectionDesc const& getSectionDescElem(SECTION_TYPE sect_type);

    //Get dynamic section description.
    virtual SECTION_TYPE const* getDynamicSectionDesc() const;

    //Get the number of dynamic section description
    //element according to different architecture.
    virtual UINT getDynamicSectionDescNum();

    //Get section name via section type.
    //e.g.: given 'SH_TYPE_BSS' and return '.bss'
    Sym const* getSectionName(SECTION_TYPE sect_type);

    //A helper function of getting section name via SymbolInfo. There
    //may be specific section name that needs to be handled according
    //to different architecture.
    //e.g.: given SymbolInfo with 'SH_TYPE_BSS' and return '.bss'.
    virtual Sym const* getSectionNameViaSymbolInfoHelper(
        MOD SymbolInfo * symbol_info)
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Get section type via section name.
    //It will extract valid info from 'sect_name' firstly.
    //e.g.:
    //  1.given '.rodata', it will return 'SH_TYPE_RODATA' according to
    //    the valid info '.rodata'.
    //  2.given '.rodata.str.1', it will return 'SH_TYPE_RODATA' according
    //    to the valid info '.rodata' extracted from '.rodata.str.1'.
    SECTION_TYPE getSectionTypeWithSplit(Sym const* sect_name);

    //Get section type via section name. There may be specific section
    //type that needs to be handled according to different architecture.
    //e.g.: given '.bss' and return 'SH_TYPE_BSS'.
    virtual SECTION_TYPE getSectionType(Sym const* sect_name);

    //Get section BYTEVec content via 'sect_sym_name'.
    BYTEVec * getSectionContent(Sym const* sect_sym_name) const;

    //Get section BYTEVec content via 'sect_type'.
    BYTEVec * getSectionContent(SECTION_TYPE sect_type)
    { return getSectionContent(getSectionName(sect_type)); }

    //Get section CHARVec content via 'sect_sym_name'.
    CHARVec * getSectionCharVec(Sym const* sect_sym_name) const;

    //Get section CHARVec content via 'sect_type'.
    CHARVec * getSectionCharVec(SECTION_TYPE sect_type)
    { return getSectionCharVec(getSectionName(sect_type)); }

    //Get section order index. Differnet architecture may have different section
    //layout(order). This function must be called after all sections have been
    //created by createFundamentalSection() or processCode().
    UINT getSectionIndex(SECTION_TYPE sect_type)
    { return m_section_index_map.get(sect_type); }

    //Get section index via 'symbol_info'.
    virtual UINT getSectionIndexViaSymbolInfo(SymbolInfo const* symbol_info)
    {
        ASSERT0(symbol_info);
        SectionInfo * si = m_sect_map->get(SYMINFO_sect_name(symbol_info));
        ASSERT0(si);
        return (UINT)SECTINFO_index(si);
    }

    UINT getShdrNum() const
    { return m_sect_map->get_elem_count(); }

    //Get section name by 'index'.
    Sym const* getSectionNameByIndex(UINT index)
    { return m_sect_layout[index]; }

    UINT getSectionAlign(Sym const* sect_name) const
    {
        ASSERT0(m_sect_map->find(sect_name));
        SectionInfo * si = m_sect_map->get(sect_name);
        ASSERT0(si);
        return (UINT)SECTINFO_align(si);
    }

    UINT getSectionAlign(SECTION_TYPE sect_type)
    { return getSectionAlign(getSectionName(sect_type)); }

    //Get the number of element in .got section.
    UINT64 getGotElemNum() const { return m_got_elem_num; }

    //Get program header via 'idx'.
    ELFPHdr * getProgramHeader(size_t idx) const;

    //Get program header index according to the 'ph_type'.
    UINT getProgramHeaderIdxInPHdr(UINT ph_type);

    //Get the number of program header.
    virtual UINT getProgramHeaderNum() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get function name from 'text_shdr_name'.
    //e.g.: 1.given ".text1" and return ".text1".
    //      2.given ".text.func_name" and return "func_name".
    Sym const* getFunctionName(CHAR const* text_shdr_name);

    //Get symbol address.
    //This function must be called after section addr have been set.
    Addr getSymbolAddr(SymbolInfo const* symbol_info);

    //Get section header type.
    //e.g.: given ".text1.func_name" and return Sym of ".text1".
    Sym const* getShdrType(CHAR const* shdr_name);

    //Get rela section header type.
    //e.g.: given ".rela.text.func_name" and return ".text"
    CHAR const* getRelaShdrType(CHAR const* shdr_name);

    //Get corresponded section type of 'rela_sect_type'.
    SECTION_TYPE getCorrespondedSectType(SECTION_TYPE rela_sect_type);

    //Get corresponded rela section type of 'sect_type'.
    SECTION_TYPE getCorrespondedRelaSectType(SECTION_TYPE sect_type);

    //Get substr from 'str' via the 'index' of
    //substr after splited by 'delimiter'.
    //e.g.: given string:         ".rela.dl_tdata"
    //      given delimiter:      '.'
    //      substr after splited: ' ' + 'rela' + 'dl_tdata'
    //      substr index:          0       1         2
    CHAR const* getSubStrWithDelimViaIdxAfterSplited(
        CHAR const* str, CHAR const* delimiter, UINT index);

    //Get section content. If it isn't existed, it will be created firstly.
    BYTEVec * getSectContentWithGenIfNotExist(Sym const* sect_name);

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

    //Get the size of .dynamic section content.
    UINT getDynamicSectionSize();

    //Get the index of .text section.
    UINT getTextIndex() const { return m_text_index; }

    //Get the number of .text section.
    UINT getTextNum() const { return getTextIndex(); }

    //Get flags. There may be specific section flags for different architecture.
    virtual Addr getSectionFlags(SectionDesc const* sect_desc)
    { ASSERT0(sect_desc); return SECTDESC_flags(sect_desc); }

    //Get the size of 'section index table'.
    virtual UINT getSectionIndexTableNum() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get 'section index table'.
    virtual SECTION_TYPE const* getSectionIndexTable() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get secton table info from 'section index table'.
    SECTION_TYPE getSectionFromIndexTable(UINT index)
    {
        ASSERT0(index < getSectionIndexTableNum());
        return getSectionIndexTable()[index];
    }

    //Get the type of ELF object file. e.g.: ET_REL, ET_EXEC.
    //Refer to the file 'elf_header.h' for more detailed about ELF type.
    UINT getELFType();

    //Get the index of program header according to the 'ph_type'.
    virtual size_t getPhdrIndex(PROGRAM_HEADER ph_type);

    //Get program header index table according to different architecture.
    virtual PROGRAM_HEADER const* getPhdrIndexTable() const
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Get the align of Phdr with PH_TYPE_CODE type.
    virtual Word32 getPhdrCodeAlign() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the align of Phdr with PH_TYPE_DATA type.
    virtual Word32 getPhdrDataAlign() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the align of Phdr with PH_TYPE_DYNAMIC type.
    virtual Word32 getPhdrDynamicAlign() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the align of 'shdr offset' according to different architecture.
    //'shdr offset' is the field of 'Offset' in section header in ELF.
    virtual Off getShdrOffsetAlign() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the align of 'shdr address' according to different architecture.
    //'shdr address' is the field of 'Address' in section header in ELF.
    virtual Off getShdrAddrAlign() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //The content of 'runtime resolved function' which used for implemented
    //lazy binding is the first element in '.plt' section. And it's offset
    //in this element is pointed to an item in '.got' section which will be
    //refilled by the physical address of 'runtime resolved function' in
    //runtime. This offset maybe not the same in different architectures.
    virtual Off getRuntimeResolvedFuncOfstInPlt() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //The first element of '.plt' section is used for 'runtime resolved
    //function'. This function will return the size of this element.
    virtual UINT getFirstElemSizeOfPlt()
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //The first element of '.plt' section is used for 'runtime resolved
    //function'. This function will return the content of this element
    //according to different architectures.
    virtual UINT const* getFirstElemContentOfPlt()
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //The first element of '.plt' section is used for 'runction resolved
    //function'. It is helper function of getting the first element content
    //of '.plt' section. It will allocate and return space to store the
    //content of first element.
    CHAR const* getFirstElemContentOfPltHelper();

    //Get the size of common element of '.plt' section
    //according to different architectures.
    virtual UINT getCommonElemSizeOfPlt()
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //This function return the content of common element of
    //'.plt' section according to different architectures.
    virtual UINT const* getCommonElemContentOfPlt()
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Helper function of getting the common element of '.plt' section.
    //It will allocate space to store the content of comment element.
    CHAR const* getCommonElemContentOfPltHelper();

    //The offset of PltInfo is pointed to an item in '.got' section that
    //will be refilled by the address of target function in runtime. This
    //offset will be depended on different architecture.
    //e.g.: a common element of '.plt' section
    //      0x0  load a0,  address   <---- offset
    //      0x4  jump a0             <---- jump to target function
    virtual Off getPltOfst() const
    { ASSERTN(0, ("Target Dependent Code")); return 0; }

    //Get the element index of '.dynsym' section.
    UINT getDynsymIdx() const { return m_dynsym_idx; }

    //Both '.rela.dyn' and '.rela.plt' section will require to create items in
    //'.got' section. Thus a 'm_got_offset' variable introduced to record
    //current element offset in '.got' section.
    Off getGotOffset() const { return m_got_offset; }

    //There are two type of RelapltInfo: local and global type. This function
    //will count and return the number of RelaPltInfo with global type.
    UINT getGlobalRelaPltElemNum();

    //Check whether there is specific section in ELFMgr.
    bool hasSection(SECTION_TYPE sect_type)
    { return hasSection(getSectionName(sect_type)); }

    //Check whether there is specific section in ELFMgr.
    bool hasSection(Sym const* sect_sym_name) const
    { ASSERT0(sect_sym_name); return m_sect_map->find(sect_sym_name); }

    //Whether the corresponded reladyn info of 'reloc_info' has been recorded.
    //Since there maybe with the same reladyn for more than one 'reloc_info'.
    bool hasBeenRecordedRelaDynInfoItem(RelocInfo const* reloc_info,
                                        OUT RelocInfo ** out_reloc_info);

    //Whether the corresponded relaplt info of 'reloc_info' has been recorded.
    //Since there maybe with the same relaplt for more than one 'reloc_info'.
    bool hasBeenRecordedRelaPltInfoItem(RelocInfo const* reloc_info,
                                        OUT RelocInfo ** out_reloc_info);

    //Whether 'reloc_info' needs to be generated a GOT item. The judgement
    //method depend on relocated type according to different architecture.
    virtual bool hasGotItem(RelocInfo const* reloc_info)
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    //Return true if there is at least one PltInfo in current ELFMgr .
    virtual bool hasPltInfo() const { return false; }

    //Whether current ELFMgr has been recorded in LinkerMgr.
    bool hasBeenRecordedCurrentELFMgr() const
    { return m_has_recorded_in_linkermgr; }

    //Initialization of debug-related operations.
    //Currently, two operations have been performed:
    //encoding for debug_frame, and encoding for debug_line.
    void initDebugInfo();

    //Initialize 'm_file'. 'm_file' is pointed to opened file.
    //'pfile' may be opened by outside object(e.g.: ELFAR object).
    void initFileObj(FileObj * pfile) { ASSERT0(pfile); m_file = pfile; }

    //'sym_tab': used for 'm_sym_tab' variable.
    //'file_path': used for 'm_file_name' variable.
    //'is_elf_format': used for 'm_have_elf_format' variable.
    void initELFMgrInfo(MOD SymTab * sym_tab, CHAR const* file_path,
        UINT elf_type, bool is_elf_format);

    //Initialize section info according to the section description table.
    void initSectionInfo();

    //Check whether it is S_UNDEF symbol.
    bool isNullSymbol(SymbolInfo const* symbol_info, UINT symbol_idx);

    //The function describes whether a GOT item needs to be created based on
    //the type of 'reloc_info' during the generation of '.reladyn' content.
    virtual bool isNeededToCreateGotItem(RelocInfo const* reloc_info)
    { ASSERT0(reloc_info); return false; }

    //Whether current SymbolInfo is .rela.dyn symbol. There may be
    //different judgement methods in different architectures.
    virtual bool isRelaDynSymbol(RelocInfo const* reloc_ifno)
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    //Whether current SymbolInfo is .rela.plt symbol. There may be
    //different judgement methods in different architectures.
    virtual bool isRelaPltSymbol(RelocInfo const* reloc_info)
    { ASSERTN(0, ("Target Dependent Code")); return false; }

    //Whether it is kernel function.
    virtual bool isEntryFunction(SECTION_TYPE sect_type) const { return false; }

    //The function describes that whether the 'symbol_info' is undefine
    //functional variable that needs to be resolved by external varibale.
    virtual bool isUndefFuncationalVar(SymbolInfo * symbol_info, UCHAR bind)
    { ASSERT0(symbol_info); return false; }

    //Whethe it is mult-section which more than one sections with same
    //section type. e.g.: Both .text.func_name_1 and .text.func_name_2
    //have SH_TYPE_SUBTEXT type.
    virtual bool isMultiSection(SECTION_TYPE sect_type)
    { return SH_TYPE_RELA == sect_type; }

    bool isRelType() { return getELFType() == ET_REL; }

    bool isDynType() { return getELFType() == ET_DYN; }

    //Whether SymbolInfo is with local attribute.
    bool isSymbolWithLocalAttr(SymbolInfo const* s);

    bool isStandardELFFormat() { return m_have_elf_format; }

    bool isDebugSection(SECTION_TYPE sect_type)
    {
        return (sect_type == SH_TYPE_EH_FRAME ||
                sect_type == SH_TYPE_DEBUG_INFO ||
                sect_type == SH_TYPE_DEBUG_LINE ||
                sect_type == SH_TYPE_DEBUG_ABBREV ||
                sect_type == SH_TYPE_DEBUG_RANGES ||
                sect_type == SH_TYPE_DEBUG_FRAME ||
                sect_type == SH_TYPE_DEBUG_LOC ||
                sect_type == SH_TYPE_DEBUG_STR ||
                sect_type == SH_TYPE_DEBUG_ARANGES);
    }

    //FIXME(SWS-6097): Handle some specific function when generated rela.plt.
    virtual bool isSpecificFuncForRelaPlt(RelocInfo const* reloc_info)
    { ASSERT0(reloc_info); return false; }

    //Whether there are two phdrs with the same type.
    bool isSamePhdrType(ELFPHdr const* phdr, SectionInfo const* sect_info,
        PROGRAM_HEADER phdr_type, PROGRAM_HEADER pre_phdr_type) const;

    void increaseDynsymIdx() { m_dynsym_idx++; }

    //Merge BSS SymbolInfo into the corresponded section of output ELF.
    //The BSS SymbolInfo needs to be allocated memory space and assigned
    //0 in corresponded section content.
    void mergeBssData(MOD SymbolInfo * symbol_info);

    //Merge SymbolInfo into the corresponded section of output ELF. The
    //data of SymbolInfo will be copied to corresponded section content.
    void mergeUnullData(MOD SymbolInfo * symbol_info);

    //Post process specific info after setting
    //all section info for different architecture.
    virtual void postProcessAfterSettingSectionInfo() { return; }

    //Process element offset after section address has been set.
    void postProcessAfterSetSectAddr();

    //Generate the first element of '.plt' section content.
    void preProcessFirstElemOfPlt();

    //Generate the common element of '.plt' section content.
    void preProcessCommonElemOfPlt();

    //Since the address of section will be used to generate the item content of
    //.dynamic section, the function will just calculate the size of .dynamic
    //section and allocate section memory. The content of the section will be
    //generated after the address of section have been set later.
    void preProcessDynamicSection();

    //Generate ELFPHdr info of ELFMgr. The Phdr info of each shdr has been
    //written into the configure table of description of section. The function
    //will allocate ELFPHdr memory and iterate over the 'm_sect_map' to collect
    //the Phdr info of each shdr.
    void processProgramHeader();

    //Process extended program header according to different architecture.
    virtual void processExtProgramHeader(
        SectionInfo const* sect_info, MOD ELFPHdr * ph)
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //A function to generate .dynamic section content. There is a dynamic
    //section configure table to control the item info of .dynamic section.
    //The function will iterate over the configure table and create ELFDyn
    //item. And the address of .dynamic section will be written into the
    //first item of .got section according to the ELF format.
    void processDynamicSection();

    //Process the s_offset field of ELFSHdr type after setting the base addr
    //of section. s_offset field dedicated the section offset in ELF file.
    void processSectionOffset();

    //Process extend section offset according to different architecture.
    virtual void processExtSectionOffset(MOD SectionInfo * sect_info,
        SECTION_TYPE sect_type, Off section_base, UINT sect_index)
    { ASSERTN(0, ("Target Dependent Code")); }

    //Process the s_addr field of ELFSHdr type after setting the base addr
    //of section. s_addr field is the address of section.
    void processSectionAddr();

    //Process extend section address according to different architecture.
    virtual void processExtSectionAddr(MOD SectionInfo * sect_info,
        SECTION_TYPE sect_type, Addr & current_addr, UINT sect_index)
    { ASSERTN(0, ("Target Dependent Code")); }

    //Process ELF name. The 'fn' may be a file path.
    //e.g.: 1.given "elf.name" and return "elf.name".
    //      2.given "xx/xx/parent/elf.name" and return "parent/elf.name".
    CHAR const* processELFName(CHAR const* fn);

    //When the data of SymbolInfo merged into corresponded section, it may
    //needs to be aligned according to the info in SymbolInfo. And additional
    //number 0 will be padded into section content if SymbolInfo needs to be
    //allinged.
    void processDataSectionAlign(MOD AssembleBinDescVec & desc_vec,
        MOD SectionInfo * sect_info, MOD SymbolInfo * symbol_info);

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

    //After all sections address have been set, relaplt item that depend on
    //these address will be created and set to .relaplt section content.
    virtual void processRelaPltSectAfterSetSectAddr();

    //Process SymbolInfo according to different st_shndx value of ELFSym.
    bool processSpecialShndx(ELFHdr & hdr, MOD SymbolInfo * symbol_info);

    //Process '.plt' section after set section address. There are location
    //that need to be refilled in the '.plt' section content.
    void processPltSectAfterSetSectAddr();

    //Read ELFSym from .symtab section('shdr') via 'idx'.
    void readSymFromSymtabSect(ELFSHdr const* shdr,
                               OUT ELFSym & sym, size_t idx);

    //Read ELFRela from .rela_dyn section('shdr') via 'idx'.
    void readRelaFromRelaTextSect(ELFSHdr const* shdr,
                                  OUT ELFRela & rela, size_t idx);

    //Read a byte value from section content via 'sect_name' and 'addr'.
    BYTE readByteFromSectionContent(Sym const* sect_name, Addr addr)
    { return readValueFromSectionContent<BYTE>(sect_name, addr); }

    //Read value from section content via 'sect_name' and 'addr'.
    template <class ValueType>
    ValueType readValueFromSectionContent(Sym const* sect_name, Addr addr)
    {
        ASSERT0(sect_name && hasSection(sect_name));
        BYTEVec * bytevec = getSectionContent(sect_name);
        ASSERT0(bytevec);
        return *(ValueType*)(bytevec->get_vec() + addr);
    }

    //Refill the element info of '.plt'. There is position that needs to be
    //refilled by the address of corresponded '.got' item. For first element
    //in '.plt', it's '.got' address will be relocated by the physical address
    //of runtime resolved function in runtime. For comment element in '.plt',
    //it's '.got' address will be relocated by the physical address of target
    //function in runtime.
    virtual void refillElemInfoOfPlt()
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //Refill the address of '.plt' section to the corresponded '.got' item.
    virtual void refillPltSectAddr()
    { ASSERTN(0, ("Target Dependent Code")); return; }

    void resetFileObj() { m_file = nullptr; }

    //Set the ELFHdr.e_type field.
    void setELFType(UINT elf_type);

    //Set 'e_entry' field of ELFHdr after all sections address have been set.
    void setEntryPointInELFHdr(SECTION_TYPE sect_type);

    //Set global symbol index of .symtab section.
    void setGlobalSymbolBeginIndexOfSymtab(UINT index)
    { m_global_symbol_begin_index_of_symtab = index; }

    //Set global symbol index of .dynsym section.
    void setGlobalSymbolBeginIndexOfDynSym(UINT index)
    { m_global_symbol_begin_index_of_dynsym = index; }

    //Set global symbol index in .symtab.
    void setGlobalSymbolBeginIndex(UINT index)
    { m_global_symbol_begin_index = index; }

    //Set section align.
    void setSectionAlign(SECTION_TYPE sect_type, UINT v)
    {
        ASSERT0(m_sect_map->find(getSectionName(sect_type)));
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

    void setSymTab(MOD SymTab * v) { m_sym_tab = v; }

    void setDynsymIdx(UINT v) { m_dynsym_idx = v; }

    //Set the section index after all sections have been created.
    void setSectionIndex();

    void setTextIndex(UINT v) { m_text_index = v; }

    void setRecordedInfoOfCurrentELFMgr(bool v)
    { m_has_recorded_in_linkermgr = v; }

    //Record SymbolInfo into 'm_symbol_info_map' and 'm_symbol_info_vec'.
    virtual void setSymbolInfo(MOD SymbolInfo * sym_info);

    //Create SectionInfo.
    void setSection(SECTION_TYPE sect_type);

    //Create SectionInfo.
    void setSection(SECTION_TYPE sect_type,
                    CHAR const* sect_name, UINT sect_idx);

    //Set SectionInfo.
    void setSectionImpl(MOD SectionInfo * si, SECTION_TYPE sect_type);

    //Set the section order after all sections have been created.
    void setSectionOrder();

    //Write symbol data to section content.
    void setSymbolDataToSection(MOD AssembleBinDescVec & desc_vec,
        MOD SectionInfo * sect_info, MOD SymbolInfo * symbol_info);

    //A helper function of setting the 'm_sym_elfsym' field of SymbolInfo.
    void setSymbolValueHelper(MOD SymbolInfo * symbol_info);

    //A helper function to set ELFSym fields using given values.
    void setSymbolValue(MOD ELFSym * sym, Word name, UCHAR bind,
        UCHAR type, UCHAR other, Half shndx, Addr value, Addr size);

    //Set ELFSym into bytevec content.
    void setELFSymToByteVec(MOD BYTEVec * sym_vec, MOD SymbolInfo * symbol_info,
        MOD BYTEVec * local_vec, MOD BYTEVec * global_vec,
        MOD BYTEVec * dynsym_vec, UINT local_idx, UINT global_idx);

    //Record reladyn info into 'm_reladyn_info_vec'.
    void setRelaDynInfo(MOD RelocInfo * reloc_info);

    //Set the index of multi-section which more than one sections with
    //same section type.
    void setMultiSectionIndex();

    //Set the index of extend multi-section which more than one sections
    //with same section type.
    virtual void setExtMultiSectionIndex(SECTION_TYPE sect_type)
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //Record '.relaplt' info into 'm_relaplt_info_vec'.
    void setRelaPltInfo(MOD RelocInfo * reloc_info);

    //Update st_value of ELFSym after the base address
    //of .symtab/.dynsym section have been set.
    void updateSymOffset(SECTION_TYPE sect_type);

    //Verify predefined infomation.
    bool verifyPreDefinedInfo();

    //Write 'buf' info into the 'addr' of section content.
    //'addr': relative offset in corresponded section.
    //'buflen': length of 'buf'.
    void writeSectionContent(Sym const* sect_name, Addr addr,
                             BYTE const* buf, Word buflen);

    //Since both '.relaplt' and '.reladyn' section would create item in
    //'.got' section, it needs to record the offset of '.got' section.
    void updateGotOffset(UINT offset = 0)
    { m_got_offset += ((offset == 0) ? getElemByteSizeInGotSect() : offset); }

    //Record some useful info according to the RelocInfo.
    virtual void updateUsefulInfoAccordingToRelocInfo(
        RelocInfo const* reloc_info)
    { ASSERTN(0, ("Target Dependent Code")); return; }
};


//
//Start ELFOpt.
//
class ELFOpt {
public:
    //-elf-device option: Output relocatable file (ET_REL type) that need
    //to be relocated with other device file.
    bool m_is_device_elf;
    //-elf-fatbin option: Output shared object file (ET_DYN type) that direct
    //execute on device. It will linked multi-file and other external .so file.
    bool m_is_fatbin_elf;
    //-elf-dumplink option: Dump info during link process.
    bool m_is_dump_link_info;
public:
    ELFOpt()
    {
        //FIXME: Now default ELF output format is 'is_device_elf = true'.
        //Removed it after modified testcases CMakefile.
        m_is_device_elf = true;
        m_is_fatbin_elf = false;
        m_is_dump_link_info = false;
    }

    bool isDeviceELF() const { return m_is_device_elf; }
    bool isFatbinELF() const { return m_is_fatbin_elf; }
    //Use '-elf-dumplink' option to dump linker info.
    //The default log file is 'dump.log'.
    //e.g.: pcxac.exe xxx.pcx -O0 -elf-fatbin -elf-dumplink
    bool isDumpLink() const { return m_is_dump_link_info; }
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

    //Get ELF info from opened ARFile via 'offset'.
    //'elf_mgr': corresponded ELFMgr of ELF.
    EM_STATUS getArchiveELF(MOD ELFMgr * elf_mgr, UINT64 offset) const;

    //Open ARFile via 'filename'.
    //'elfar_mgr': ELFARMgr object. Mange the resource of opened ARFile.
    EM_STATUS open(CHAR const* filename, MOD ELFARMgr * elfar_mgr);

    //Read info to 'buf' from opened ARFile via 'offset' and 'size'.
    EM_STATUS read(OUT BYTE * buf, size_t offset, size_t size);

    //The entry function of read ARFile info.
    //'elfar_mgr': ELFARMgr object. Mange the resource of opened ARFile.
    EM_STATUS readARFile(MOD ELFARMgr * elfar_mgr);

    //Read ARHdr info of ARFile to 'm_ar_hdr'.
    EM_STATUS readARHeader();

    //Read ARIdent info of ARFile and check whther it is valid ARIdent.
    EM_STATUS readARIdent();

    //Read symbol info from ARFile.
    EM_STATUS readSymbolIndex();
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
typedef xcom::Vector<ELFARInfo*> ELFARInfoVec;
typedef xcom::TMap<Sym const*, ELFARInfo*> SymbolARInfoMap;
typedef xcom::TMap<ELFAR const*, ELFARInfoVec*> ARInfoMap;
typedef xcom::TMapIter<ELFAR const*, ELFARInfoVec*> ARInfoMapIter;

//A class manages ELFAR/ELFARInfo object resources. It creates ELFAR and
//ELFARInfo object and uses 'm_ar_list' or 'm_ar_info_meta_list' to record.
//These resources would be freed when the destructor is called.
class ELFARMgr {
    COPY_CONSTRUCTOR(ELFARMgr);

    //Record ARFile path/name with list.
    xcom::List<CHAR const*> * m_ar_file_list;

    //Record all ELFAR object that generated by 'allocELFAR' function.
    xcom::List<ELFAR*> m_ar_list;

    //Record all ELFARInfo object that generated by 'allocELFARInfo' function.
    xcom::List<ELFARInfo*> m_ar_info_meta_list;

    //Record all FileObj that generated by 'allocFileObj' function.
    xcom::List<FileObj*> m_file_obj_list;

    //Record all ELFARInfo vec object.
    xcom::List<ELFARInfoVec*> m_elfar_info_vec_list;

    //Record 'symbol name' <-> 'ELFARInfo' info.
    SymbolARInfoMap m_symbol_ar_info_map;

    //Record 'ar' <-> 'ELFARInfo' info.
    ARInfoMap m_ar_info_map;

    //Record ARFile path/name with stack.
    xcom::Stack<CHAR const*> m_ar_file_stack;
public:
    ELFARMgr() { m_ar_file_list = nullptr; }

    ~ELFARMgr();

    //Allocate ARFile object according to 'file_name'
    //and record to 'm_ar_list'.
    //return: ARFile object.
    ELFAR * allocELFAR(CHAR const* file_name);

    //Allocate ARInfo object and record to 'm_ar_info_meta_list'.
    //return: ARInfo object.
    ELFARInfo * allocELFARInfo();

    //Allocate FileObj via 'filename' and record to 'm_file_obj_list'.
    //is_del: 'true' to delete the file with same name.
    //is_readonly: whether open 'filename' with readonly attribute.
    //return: FileObj.
    FileObj * allocFileObj(CHAR const* filename, bool is_del, bool is_readonly);

    //Allocate ELFARInfoVec object and record to 'm_elfar_info_vec_list'.
    ELFARInfoVec * allocVectorOfELFARInfo();

    //Append 'ar_info' to the vector of ELFARInfoVec that allocated by
    //'allocVectorOfELFARInfo()' function. And record the vector to
    //'m_ar_info_map' with the key of 'ar'.
    void genVectorELFARInfo(ELFAR const* ar, MOD ELFARInfo * ar_info);

    //Find SymbolInfo from 'm_symbol_ar_info_map' according to 'symbol_name'.
    bool findFromSymbolARInfoMap(Sym const* symbol_name) const
    {
        ASSERT0(symbol_name);
        return m_symbol_ar_info_map.find(symbol_name);
    }

    //Find ELFAR from 'm_ar_info_map' according to 'ar'.
    bool findFromARInfoMap(ELFAR const* ar) const
    {
        ASSERT0(ar);
        return m_ar_info_map.find(ar);
    }

    //Get SymbolInfo from 'm_symbol_ar_info_map' according to 'symbol_name'.
    ELFARInfo * getFromSymbolARInfoMap(Sym const* symbol_name) const
    {
        ASSERT0(symbol_name);
        return m_symbol_ar_info_map.get(symbol_name);
    }

    //Get ELFARInfo vec from 'm_ar_info_map' according to 'ar'.
    ELFARInfoVec * getFromARInfoMap(ELFAR const* ar) const
    {
        ASSERT0(ar);
        return m_ar_info_map.get(ar);
    }

    //AR files are sorted in the order in which there were read from outside.
    //The function would pop the top element from 'm_ar_file_stack'.
    CHAR const* getARFileName()
    {
        if (!m_ar_file_stack.get_top()) { return nullptr; }
        return m_ar_file_stack.pop();
    }

    //Find ELFMgr from 'm_ar_info_map' via 'ar' and 'idx'.
    bool findELFMgr(ELFAR const* ar, UINT64 idx, OUT ELFMgr ** elf_mgr) const;

    //AR files are sorted in the order in which they were read from outside.
    //Thus a stack is used to record the ARFile name/path. The AR file is
    //read in first recorded in the top of the stack.
    void initARFileStack();

    //Allocate ELFAR via AR file name that comes from the top element of
    //'m_ar_file_stack'. And related info of the AR file will be read.
    //return ELFAR.
    ELFAR * processARFile();

    void setLibFileList(MOD xcom::List<CHAR const*> * lib_path_list)
    {
        ASSERT0(lib_path_list);
        m_ar_file_list = lib_path_list;
    }

    //Record reusable ARInfo to 'm_ar_info_map'.
    //'ar': AR file.
    //'elf_mgr': ELFMgr.
    //'idx': the index of 'elf_mgr' in AR file.
    void saveARInfo(MOD ELFAR * ar, MOD ELFMgr * elf_mgr, UINT64 idx);

    //Record reusable ARInfo to 'm_symbol_ar_info_map'.
    //'ar': AR file.
    //'symbol_name': the name of symbol in the global symtab of AR file.
    //'idx': the index of ELF to which 'symbol_name' belong in AR file.
    void saveSymbolARInfo(MOD ELFAR * ar, Sym const* symbol_name, UINT64 idx);
};


//
//Start LinkerInfoMgr
//

//The type is used for recording the index of unresolved
//RelocInfo in the vector of 'm_reloc_sym_vec'.
typedef xcom::Vector<UINT> UnresolvedRelocIdxVec;

//A class manages linker info that is used in linker process.
class LinkerInfoMgr {
    COPY_CONSTRUCTOR(LinkerInfoMgr);

    //Record the index vector that is used to
    //record RelocInfo index in 'm_reloc_info'.
    xcom::List<UnresolvedRelocIdxVec*> m_vector_int_meta_list;
public:
    LinkerInfoMgr() {}

    ~LinkerInfoMgr()
    {
        for (xcom::Vector<UINT> * vec = m_vector_int_meta_list.get_head();
             vec != nullptr; vec = m_vector_int_meta_list.get_next()) {
            if (vec != nullptr) { delete vec; }
        }
    }

    UnresolvedRelocIdxVec * allocUnresolvedRelocIdxVec()
    {
        UnresolvedRelocIdxVec * vec = new UnresolvedRelocIdxVec();
        ASSERT0(vec);

        m_vector_int_meta_list.append_tail(vec);
        return vec;
    }
};


//The class describes generated mapped of SameNameNumMap.
class GenMappedOfSameNameNumMap {
    COPY_CONSTRUCTOR(GenMappedOfSameNameNumMap);
public:
    GenMappedOfSameNameNumMap() {}

    UINT createMapped(Sym const* s) const { return 0; }
};


//
//Start LinkerMgr.
//
//LinkerMgr manages all ELFMgr resources and controls the linking process.
//It will output a target ELFMgr which its type is ET_REL(relocatable file),
//ET_EXEC(executable file) or ET_DYN(shared object file) according to the
//required.
//
//The input info of LinkerMgr include 'library path list', 'objfile path list'
//and 'ELFMgr list'. 'library path list' record all library files path. These
//library files path will be read as ELFAR object and managed by ELFARMgr during
//linking process. 'objfile path list' record all object files path. These
//object files will be read as ELFMgr object directly when linking process
//starts. 'ELFMgr list' record ELFMgr that colleted from xoc::Var. xoc::Var is
//provided by user.
//
//When linking process starts, there are some initialization operations that
//need to be handled.
//  a.'m_elf_mgr_list' record all ELFMgr that may be used in link process. Thus
//    ELFMgr in 'ELFMgr list' and generated from 'objfile path list' will be
//    appended to it directly. And ELFMgr from 'library path list' will be
//    generated and appended to 'm_elf_mgr_list' as needed.
//  b.'m_reloc_symbol_vec' record all RelocInfo that need to be resolved in the
//    linking process. It will be initialied by the RelocInfo collected from
//    'ELFMgr list' in the beginning and constantly updated after a new ELFMgr
//    generated from 'library path list' during the linking process.
//  c.'m_unresolved_reloc_idx_map' and 'm_resolved_reloc_idx_map' are used for
//    the processing of resolved. They record the index of RelocInfo in
//    'm_reloc_symbol_vec' and will be constantly updated too.
//
//Handle SymbolInfo with same name in different ELFMgr. e.g.: There may be lots
//of 'str' with local attribute that come from 'printf' statement in different
//ELFMgr. The solution to this is rename these 'str'. The format of new name is
//'str.xxx' with integer suffix. e.g.: 'str.R_1', 'str.R_2', 'str.R_3', ...
//
//Resolve operation. One of the most important operations in linking procss.
//When output shared object file, the RelocInfo need to be resolved via finding
//target SymbolInfo from different ELFMgr. These ELFMgr mainly collected from
//xoc::Var, object file and AR file.
//  a.Firstly, SymbolInfo will be found in ELFMgr that collected from xoc::Var.
//    xoc::Var ELFMgr is directly generated from compile file. These files have
//    the highest priority during the processing of finding target SymbolInfo.
//  b.Then other SymbolInfo of unresolved RelocInfo will be found in ELFMgr that
//    collected from AR file. AR file that read from library file path is sorted
//    in the order they were read in. Thus if there is more than one target
//    SymbolInfo of unresolved RelocInfo in different AR file, the SymbolInfo
//    that found from the first AR file will be used to resolve RelocInfo. And
//    other SymbolInfo are not in use.
//More detailed steps can be found in the description of 'doResolve()' function.
//
//Process output ELF. NOTE: Now LinkerMgr can only output ET_DYN ELF(shared
//object file) and ET_EXEC(executable file). ET_REL ELF needs to be supported
//in the future. After completing resolved, all SymbolInfo, RelocInfo and ELFMgr
//have been collected into LinkerMgr object, the output ELF can be generated
//according to these sufficient information. In order to manage output ELF, an
//output ELFMgr object will be created.
//  a.Merge ELFMgr. Section with same name in different ELFMgr need to be merged
//into a section in output ELFMgr. After merged, the offset of SymbolInfo and
//RelocInfo in corresponded section need to be updated.
//  e.g.: merge section in different ELFMgr and update the offset of SymbolInfo.
//    |-------------|              |-------------|               |-------------|
//    |    ELFMgr   |              |Output ELFMgr|               |    ELFMgr   |
//    |-------------|              |-------------|               |-------------|
//    |     ...     |              |     ...     |               |     ...     |
//    |-------------|              |-------------|               |-------------|
//    |DATA SECTION |              |DATA SECTION |               |DATA SECTION |
//    | | SymbolInfo|<-- 0x0070 -->| | SymbolInfo|   -- 0x0020 ->| | SymbolInfo|
//    | | SymbolInfo|<-- 0x0078 -->| | SymbolInfo|   |  0x0028 ->| | SymbolInfo|
//    | |   ...     |              | | SymbolInfo|<---     |     | |   ...     |
//    |-------------|              | | SymbolInfo|<---------     |-------------|
//    |BSS SECTION  |              | |   ...     |               |BSS SECTION  |
//    | | SymbolInfo|<-- 0x0080 -- |-------------|    ---0x0030->| | SymbolInfo|
//    | | SymbolInfo|<-- 0x0088  | | |BSS SECTION|    | -0x0038->| | SymbolInfo|
//    | |   ...     |       |    ->| | SymbolInfo|    | |        | |   ...     |
//    |-------------|       ------>| | SymbolInfo|    | |        |-------------|
//    |     ...     |              | | SymbolInfo|<---  |        |     ...     |
//    |-------------|              | | SymbolInfo|<-----         |-------------|
//                                 | |   ...     |
//                                 |-------------|
//                                 |     ...     |
//                                 |-------------|
//  b.Generate SectionInfo and sort these SectionInfo according to the section
//    index. The index of section may be different in different architecture.
//    e.g.: the layout of shdr and phdr type of the section of output ELF.
//            |------------------|-----------------|
//            |     SHdr type    |    PHdr type    |
//            |------------------|-----------------|
//            | SH_TYPE_INTERP   |                 |
//            | SH_TYPE_RELA_DYN |                 |
//            | SH_TYPE_DYNSYM   |   PH_TYPE_CODE  |
//            | SH_TYPE_TEXT1    |                 |
//            | SH_TYPE_TEXT     |                 |
//            |       ...        |                 |
//            |------------------|-----------------|
//            | SH_TYPE_GOT      |                 |
//            | SH_TYPE_BSS      |                 |
//            | SH_TYPE_DATA     |   PH_TYPE_DATA  |
//            |       ...        |                 |
//            |------------------------------------|
//            | SH_TYPE_DYNAMIC  | PH_TYPE_DYNAMIC |
//            |       ...        |                 |
//            |------------------------------------|
//            | SH_TYPE_SYMTAB   |                 |
//            | SH_TYPE_SHSTR    | PH_TYPE_UNDEF   |
//            | SH_TYPE_SYMSTR   |                 |
//            |       ...        |                 |
//            |------------------------------------
//  c.Generate SectionInfo content. After completing section merged in 'step a',
//    Data and code have been written into the corresponded section of output
//    ELFMgr. But there are still other section content need to be generated
//    according to the SymbolInfo or RelocInfo. e.g: .rela_dyn section, .dynamic
//    section, .got section and .symtab section. Though these section content
//    can be generated in this step, some info about section address(e.g. s_addr
//    in ELFSHdr) is unkown until all section address have been set in 'step e'.
//    Thus these section content need to be refilled later.
//  d.Process section address and offset in ELF. This step must come after the
//    generated SectionInfo content step. Otherwise the section content size
//    can't be known, and the address and offset of section can't be calculated.
//  e.Refill address to specific section content. st_value of ELFSym, r_offset
//    of ELFRela and d_val of ELFDyn are need to be refilled actual address.
//  f.Relocate. One of the most important operations in linking procss. After
//    resolved RelocInfo, merged EFLMgr and set section address operations, the
//    empty location of relocatable RelocInfo need to be refilled by the actual
//    address of target SymbolInfo. The target address will be caculated
//    according to the different relocating type in specific architecture.
//    e.g.: Given '.rela.debug_info' section, the 'Offset' represents the offset
//          in 'debug_info' section needs to be refilled by an acutal address
//          that can be caculated by the address of 'var_1'. And the formula is
//          determined by the 'Type'.
//    |------------------------------------------------------------------------|
//    |Relocation section '.rela.debug_info' at offset 0x08 contains 6 entries:|
//    |------------------------------------------------------------------------|
//    |    Offset           Info       Type  Sym.  Value    Sym. Name + Addend |
//    | 000000000010    001300000001    1    000000000000       var_1 + 0      |
//    | 000000000020    001300000002    2    000000000000       var_2 + 2      |
//    |                                ...                                     |
//    |------------------------------------------------------------------------|
//  g.Generated ELFSHdr info of output ELFMgr.
//  h.Output ELFMgr to disk.
#define LINKERMGR_OUTPUT_FILE_NAME   "mi.test.elf"
#define LINKERMGR_DUMP_LOG_FILE_NAME "dump.log"

typedef xcom::TMap<Sym const*, Vector<UINT>*> RelocSymbolIdxMap;
typedef xcom::TMapIter<Sym const*, Vector<UINT>*> RelocSymbolIdxMapIter;

typedef xcom::TMap<Sym const*, UINT, CompareKeyBase<Sym const*>,
    GenMappedOfSameNameNumMap> SameNameNumMap;

//The class manages all linking processes. These processes contain merge
//multi-ELFMgr, resolve undefined symbols, relocate symbols and output
//target ELF.
class LinkerMgr {
    COPY_CONSTRUCTOR(LinkerMgr);

protected:
    //File name of output ELF.
    CHAR const* m_output_file_name;

    //Manage dump file.
    FileObj * m_dump;

    //ELFMgr to which the output ELF belong.
    ELFMgr * m_output_elf_mgr;

    //Record the string name of SymbolInfo, RelocInfo, FunctionInfo and other
    //info that may be used in linking process. A Sym object will be generated
    //that contains the string name and recorded into SymTab. It can decrease
    //lookup time by changing from comparing characters to comparing pointers.
    //There is 'm_sym_tab' in ELFMgr too. But the Sym element in 'm_sym_tab'
    //in different ELFMgr can't be directly used in LinkerMgr. Since string
    //with same name in different ELFMgr may be generated Sym object element
    //with different pointers. Thus all string name that may be used in linking
    //process need to be generated a new Sym object element and recorded into
    //'m_sym_tab' in Linkermgr object.
    SymTab m_sym_tab;

    //Record ELFMgr object. The List contains all ELFMgr objects that would
    //be used in LinkerMgr. It includes the element in 'm_elf_mgr_meta_list'
    //and ELFMgr objects that generated by other module.
    xcom::List<ELFMgr*> m_elf_mgr_list;

    //Record ELFMgr object that generated by LinkerMgr. Some ELFMgr objects
    //that generated in other module don't be recorded in this List.
    xcom::List<ELFMgr*> m_elf_mgr_meta_list;

    //Record relocated symbol.
    RelocInfoVec m_reloc_symbol_vec;

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

    //Manage Linker info object.
    LinkerInfoMgr m_infomgr;

    //Manage log file.
    LogMgr m_logmgr;
public:
    LinkerMgr();

    virtual ~LinkerMgr();

    //Allocate ELFMgr during linker.
    virtual ELFMgr * allocELFMgr()
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Allocate output ELFMgr.
    virtual ELFMgr * allocOutputELFMgr()
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    //Allocate vector to record the unresolved RelocInfo.
    //'reloc_info': unresolved RelocInfo.
    //'idx': the index of 'reloc_info' in 'm_reloc_sym_vec'.
    void allocUnresolvedRelocSymbol(RelocInfo const* reloc_info, UINT idx);

    //Close dump log file.
    void closeDump();

    //Check whether the SymbolInfo with the name of
    //'symbol_name' has been used to resolve RelocInfo.
    bool checkHasBeenResolvedReloc(Sym const* symbol_name);

    //Collect alias info from each ELFMgr in 'elf_mgr_list'.
    void collectAliasInfo(MOD xcom::List<ELFMgr*> * elf_mgr_list);

    //Resolve 'reloc_info' if the RelocInfo with the same name
    //has been resolved.
    //'elf_mgr': ELFMgr to which target resolved SymbolInfo belong.
    //'reloc_info': RelocInfo needs to be resolved.
    //'reloc_index': the index of 'reloc_info' in 'm_reloc_sym_vec'.
    bool checkCurrentRelocInfoHasBeenResolved(MOD ELFMgr * elf_mgr,
        MOD RelocInfo * reloc_info, UINT reloc_index);

    //It is entry function of resolving RelocInfo. When output shared object
    //file, the RelocInfo need to be resolved via finding target SymbolInfo from
    //different ELFMgr. These ELFMgr mainly collected from xoc::Var, object file
    //and AR file. Firstly, SymbolInfo will be found in ELFMgr that collected
    //from xoc::Var. xoc::Var ELFMgr is directly generated from compile file.
    //These files have the highest priority during finding target SymbolInfo.
    //Then other SymbolInfo of unresolved RelocInfo will be found from ELFMgr
    //that collected from AR file. AR file that read from library file path is
    //sorted in the order they were read in. Thus if there are more than one
    //target SymbolInfo of unresolved RelocInfo in different AR file, the
    //SymbolInfo that found from the first AR file will be used to resolve
    //RelocInfo. And other target SymbolInfo are not in use.
    //  a.In the beginning, RelocInfo that collected from xoc::Var have been
    //recorded into 'm_reloc_symbol_vec'. 'm_reloc_symbol_vec' record all
    //RelocInfo that need to be resolved. But it will be constantly updated
    //during the processing of resolving. Since except for collecting the target
    //SymbolInfo into LinkerMgr that has been found in AR file, ELFMgr to which
    //SymbolInfo and RelocInfo belong will be collected too.
    //  b.'m_unresolved_reloc_idx_map' record the index of unresolved RelocInfo
    //in 'm_reloc_symbol_vec'. 'm_resolved_reloc_idx_map' record the index of
    //resolved RelocInfo in 'm_reloc_symbol_vec'. 'm_unresolved_reloc_idx_map'
    //will be initialised before the processing of resolving starts.
    //  c.Found target SymbolInfo from ELFMgr collected from xoc::Var. It will
    //iterate over all SymbolInfo in all xoc::Var ELFMgr, and check whether the
    //name of SymbolInfo existed in 'm_unresolved_reloc_dix_map'. RelocInfo will
    //be resolved if target SymbolInfo has been found.
    //  d.Found target SymbolInfo from ELFMgr collected from AR file. It will
    //enter 'while' statement until all unresolved RelocInfo are resovlved or
    //all AR files have been found. In 'while' statement, each AR file will get
    //from 'm_ar_file_stack' in the order their read in. And a 'for' statement
    //is used to iterate over the global symtab in this AR file. Each symbol
    //name getting from global symtab will be used to find unresolved RelocInfo
    //from 'm_unresolved_reloc_idx_map'. If there is unresolved RelocInfo, the
    //symbol will be read in.
    //  e.|     AR file   |    |      global symtab    |    | ELFMgr object |
    //    |---------------|    |-----------------------|    |---------------|
    //    | global symtab |    | symbol(name, ELF idx) |    |  SymbolInfo   |
    //    |      ELF      |    | symbol(name, ELF idx) |    |  RelocInfo    |
    //    |      ELF      |    | symbol(name, ELF idx) |    | FunctionInfo  |
    //    |      ...      |    |         ... ...       |    |               |
    //   If a 'symbol' has been found from AR file to resolve RelocInfo, it
    //needs to generate a 'ELFMgr' that collected from ELF info in AR file via
    //the ELF index of 'symbol'. And a 'SymbolInfo' will be generated aoocrding
    //to 'symbol' info. The 'ELFMgr' to which 'SymbolInfo' belong contains other
    //SymbolInfo, RelocInfo and FunctionInfo. These info need to be used to
    //update info in LinkerMgr. e.g.: 'ELFMgr' will be appended to
    //'m_elf_mgr_meta_list'. RelocInfo will be appended to 'm_reloc_sym_vec'
    //and 'm_unresolved_reloc_idx_map'.
    //  f.Break out 'while' statement until all RelocInfo have been resolved
    //or AR file have been found.
    void doResolve();

    //Dump library file path.
    //'lib_path_list': List that record all library files path.
    void dumpLibPathList(xcom::List<CHAR const*> * lib_path_list) const;

    //If there is unresolved RelocInfo after doResolve() function has
    //been called. These unresolved RelocInfo will be dumped to log file.
    void dumpLinkResolve() const;

    //Dump RelocInfo during relocate.
    //'index': the index of 'reloc_info' in 'm_reloc_symbol_vec'.
    void dumpLinkRelocate(RelocInfo const* reloc_info, UINT index) const;

    //It is entry function of relocating. After completing resolved RelocInfo,
    //merged ELFMgr and setting section address operations, the empty location
    //of relocatable RelocInfo need to be refilled by the actual address of
    //target SymbolInfo. The target address will be calculated according to
    //the different relocating type in specific architecture.
    //e.g.: Given a RelocInfo that has been resolved by SymbolInfo. The
    //      RELOCINFO_called_loc(RelocInfo) needs to be refilled by a actual
    //      address. And the formula of this autual address:
    //        'address = address of SymbolInfo + RELOCINFO_addend(RelocInfo)'
    //      Different formula will be chosen according to the different
    //      RELOCINFO_type(RelocInfo).
    //'elf_mgr': the output ELFMgr.
    virtual void doRelocate(MOD ELFMgr * elf_mgr)
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //Generate ELFMgr object.
    ELFMgr * genELFMgr();

    //Get target ELF info from AR file via 'index'.
    //And collect SymbolInfo and RelocInfo from this target ELF.
    //'ar': AR file.
    //'index': the index of target ELF info in AR file.
    //return target ELFMgr.
    ELFMgr * getELFMgrFromARViaIdx(MOD ELFAR * ar, UINT64 index);

    //Get target ELF info from AR file according to 'index'.
    //It will try to find from 'm_ar_info_map' firstly.
    //'ar': AR file.
    //'index': the index of target ELF info in AR file.
    //return target ELFMgr.
    ELFMgr * getELFMgrWhichSymbolBelongTo(MOD ELFAR * ar, UINT64 index);

    //Get the number of 'symbol_info' with same name.
    UINT getSameNameNum(SymbolInfo const* symbol_info);

    //Process SymbolInfo with the same name in different ELFMgr. It will keep
    //track of how many times this SymbolInfo has the same name. And the number
    //will be added to the original SymbolInfo name as a suffix. Thus the new
    //name foramt is "original_name + number suffix".
    //e.g.: given three SymbolInfo with the same name 'str'. The new name of
    //      these SymbolInfo are 'str', 'str.R_1', and 'str.R_2'.
    //'elf_mgr': ELFMgr currently being operated on.
    //'is_special_elf_mgr': There is special handling for specific ELFMgr
    //formed by symbol, relocation and other standard format info of ELF that
    //directly generated by upper compiler. SymbolInfo to which belong this
    //ELFMgr needs to be renamed later.
    void handleSameNameInDiffFile(MOD ELFMgr * elf_mgr,
                                  bool is_special_elf_mgr);

    //Check whether there is still unresolved RelocInfo.
    //'is_notype_as_resolved': SymbolInfo with STT_NOTYPE attribute also
    //can be used to resolve RelocInfo. The flag dedicated whether these
    //RelocInfos need to be counted. 'true': RelocInfo that resoved by
    //SymbolInfo with STT_NOTYPE doesn't need to be counted as unresolved
    //RelocInfo. 'false': RelocInfo that resolved by SymbolInfo with
    //STT_NOTYPE needs to be counted as unresolved RelocInfo.
    bool hasUnResolvedRelocSymbol(bool is_notype_as_resolved) const;

    //Check whether there is another SymbolInfo in ELFMgr that recorded
    //in 'm_elf_mgr_list' with the same name as 'symbol_info' in 'elf_mgr'.
    //'is_special_elf_mgr': There is special handling for specific ELFMgr
    //formed by symbol, relocation and other standard format info of ELF that
    //directly generated by upper compiler. SymbolInfo to which belong this
    //ELFMgr needs to be renamed later.
    bool hasSameSymbol(ELFMgr const* elf_mgr, SymbolInfo const* symbol_info,
                       bool is_special_elf_mgr);

    //Initialize the info of LinkerMgr. The name of output ELFMgr
    //can be set by 'output_name' that provided by outside user or
    //the default value will be used.
    void initLinkerMgr(CHAR const* output_name, UINT elf_type);

    //Initialize 'm_unresolved_reloc_idx_map'.
    void initUnResolveSymbol();

    //Initialize dump file info.
    //'filename': the name of dump file.
    EM_STATUS initDumpFile(CHAR const* filename);

    //Skip some reladyn items that don't need to be processed.
    //'elf_mgr': the output ELFMgr.
    //'reloc_info': RelocInfo.
    bool skipSpecificRelaDyn(MOD ELFMgr * elf_mgr,
                             RelocInfo const* reloc_info) const;

    //A helper function of merged section from different ELFMgr.
    void mergeELFMgrHelper();

    //Record code and data info in different ELFMgr into the
    //corresponded section of output ELFMgr. The function
    //mainly process ELFMgr that collected from object ELF.
    void mergeELFMgr();

    //Record code and data info in different ELFMgr into the
    //corresponded section of output ELFMgr. The function
    //mainly process ELFMgr that collected from xoc::Var.
    void mergeELFMgrCollectedFromVar();

    //The implement function of recording code and data
    //info into the corresponded section of output ELFMgr.
    //'elf_mgr': ELFMgr currently being operated on.
    //'shdr': ELFSHdr currently being operated on.
    //'shdr_name': the name of 'shdr'.
    //'shdr_idx': the index of 'shdr' in ELF.
    virtual void mergeELFMgrImpl(MOD ELFMgr * elf_mgr,
        ELFSHdr const* shdr, CHAR const* shdr_name, UINT shdr_idx);

    //The implement function of merged the shdr of ELFMgr to output
    //ELFMgr section. BSS section needs to be assigned 0 after merged.
    //Section content with progbits type needs to be copied to output
    //ELFMgr section.
    //'shdr': shdr that needs to be merged.
    //'shdr_name': the name of 'shdr'.
    //'is_bss_shdr': whether the shdr is BSS section.
    void mergeShdrImpl(ELFSHdr const* shdr,
        Sym const* shdr_name, bool is_bss_shdr);

    //Merge ELFSHdr with no-bits type to the
    //corresponded section of output ELFMgr.
    //'elf_mgr': ELFMgr that is currently being merged.
    //'shdr': ELFSHdr with no-bits type.
    //'shdr_idx': the index of 'shdr' in ELF.
    //'shdr_subname': the name of 'shdr'.
    virtual void mergedShdrWithNoBitsType(MOD ELFMgr * elf_mgr,
        ELFSHdr const* shdr, UINT shdr_idx, Sym const* shdr_subname);

    //Merge ELFShdr with prog-bits type to the
    //corresponded section of output ELFMgr.
    //'shdr': ELFMgr with prog-bits type.
    //'shdr_subname': the name of 'shdr'.
    virtual void mergedShdrWithProgBitsType(
        ELFSHdr const* shdr, Sym const* shdr_subname);

    //The helper section of merged code in 'symbol_info'
    //into .text section of output ELFMgr.
    void mergeCodeHelper(MOD SymbolInfo * symbol_info);

    //Merge code in 'symbol_info' into .text
    //Section of ET_DYN type output ELFMgr.
    virtual void mergeDynTypeCodeImpl(MOD SymbolInfo * symbol_info);

    //Merge code in 'symbol_info' into .text
    //Section of ET_REL type output ELFMgr.
    virtual void mergeRelTypeCodeImpl(MOD SymbolInfo * symbol_info);

    //Merge data in 'symbol_info' into .data section of output ELFMgr.
    virtual void mergeData(MOD SymbolInfo * symbol_info);

    //SymbolInfo with SHN_COMMON attribute needs to be allocated memory space
    //and assigned 0 in .bss merged section. And the st_shndx field of ELFSym
    //of SymbolInfo needs to be set to .bss shdr of 'elf_mgr'.
    //'elf_mgr': ELFMgr to which SymbolInfo belong.
    //'shdr_idx': the index of .bss shdr of 'elf_mgr'.
    //'shdr_subname': the name of .bss shdr.
    void mergeSymbolInfoWithCOMAttr(MOD ELFMgr * elf_mgr, UINT shdr_idx,
                                    Sym const* shdr_subname);

    //It is entry function of output shared object ELF file.
    //It will be called if the command option is '-elf-fatbin'.
    //'elf_mgr_list': List that recorded all ELFMgr.
    void outputSharedObjFile(MOD xcom::List<ELFMgr*> * elf_mgr_list);

    //It is entry function of output relocated object ELF file.
    void outputRelocatedObjFile(MOD xcom::List<ELFMgr*> * elf_mgr_list);

    //Count the number of element of '.plt' section and
    //generate '.plt' section content.
    void preProcessPltSectBeforeSetSectAddr(MOD ELFMgr * elf_mgr);

    //It is entry function of generating output executed ELF.
    void processOutputExeELF();

    //'lib_path_list' contains all the path of library files.
    //These files need to be recorded to 'm_armgr' object.
    //'m_armgr' manages all library files.
    void processLibPathList(MOD xcom::List<CHAR const*> * lib_path_list);

    //Before linking process, it needs to collect valid info from all files.
    //The function will generate ELFMgr and read ELF info for each file.
    //'obj_file_list': files come from object file.
    //'elf_mgr_list': files come from xoc::Var.
    void processOBJFileList(MOD xcom::List<CHAR const*> * obj_file_list,
                            OUT xcom::List<ELFMgr*> * elf_mgr_list);

    //There is the meaning of special reloc_type of specific 'reloc_info' that
    //depended on the reloc_type of other related RelocInfo. Thus it needs to
    //be processed according to different architecture.
    //'index': the index of 'reloc_info' in 'm_reloc_symbol_vec'.
    virtual void processRelateRelocInfo(MOD RelocInfo * reloc_info, UINT index)
    { ASSERTN(0, ("Target Dependent Code")); return; }

    //Create reladyn item of .reladyn section. Since the address of all
    //sections have not been set, the value of r_offset filed of ELFRela
    //can't be determined. Now it just allocates memory space and records
    //the number of reladyn item. After setting the address of sections,
    //the value of r_offset will be refilled.
    //'elf_mgr': output ELFMgr.
    void preProcessRelaDynSectBeforeSetSectAddr(MOD ELFMgr * elf_mgr);

    //Create relaplt item of .relaplt section. Since the address of all
    //sections have not been set, the value of r_offset filed of ELFRela
    //can't be determined. Now it just allocates memory space and records
    //the number of reladyn item. After setting the address of sections,
    //the value of r_offset will be refilled.
    //'elf_mgr': output ELFMgr.
    void preProcessRelaPltSectBeforeSetSectAddr(MOD ELFMgr * elf_mgr);

    //There is post-processing operation after ELFMgr that collected from
    //xoc::Var has been merged according to different architecture.
    //e.g.: Some variables need to be sorted before written into section
    //      content. Thus these variables need to be collected to Vector
    //      or Map during the processing of merged ELFMgr firstly. Then
    //      their will be written into section content after completing
    //      the sorting operation in this post-processing function.
    virtual void postMergeELFMgrCollectedFromVar() { return; }

    //Process RelocInfo after new ELFMgr is read. These RelocInfo need to
    //be recorded to 'm_reloc_info_vec'. And their index will be updated
    //to 'm_resolved_idx_map' or 'm_unresolved_idx_map' according to this
    //RelocInfo whether has been resolved.
    //'ar': AR file to which 'elf_mgr' belong.
    //'elf_mgr': ELFMgr to which RelocInfo belong.
    //'idx': the index of ELF in AR file.
    void processRelocInfo(ELFAR const* ar, MOD ELFMgr * elf_mgr, UINT64 idx);

    //Record debug infomation into the corresponded section of output ELFMgr.
    //These debug infomation mainly come from ELFMgr collected from xoc::Var.
    //'elf_mgr': ELFMgr to which debug info belong.
    //'symbol_info': SymbolInfo currently being operated on.
    void processDebugInfo(MOD ELFMgr * elf_mgr, MOD SymbolInfo * symbol_info);

    //Found target SymbolInfo from ELFMgr that collected from xoc::Var. These
    //ELFMgr mainly come from compile files that directly provided by user.
    //These compile files have the highest priority during the processing of
    //found target SymbolInfo. The function will iterate over all SymbolInfo
    //in all xoc::Var ELFMgr, and check whether the name of SymbolInfo existed
    //in 'm_unresolved_reloc_idx_map'. RelocInfo will be resolved if target
    //SymbolInfo has been found.
    void resolveRelocInfoViaVar();

    //The function is used to find target SymbolInfo from ELFMgr that collected
    //from AR file. AR file is packaged with numerous ELF. If target symbol has
    //been found, a new ELFMgr will be created according to the ELF info to
    //which this target symbol belong.
    //1.Enter 'while' statement until all unresolved RelocInfo are resovlved or
    //  all AR file have been found. In 'while' statement, AR file is popped
    //  from 'm_ar_file_stack' and a 'for' statement is used to iterate over
    //  global symtab in the AR file.
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
    //  the name and index of 'symbol' and AR file, and this ELFARInfo object
    //  will be recorded into 'm_symbol_ar_info_map'.
    //5.If there is unresolved RelocInfo with the same name as 'symbol' in
    //  'm_unresolved_reloc_idx_map', ELFMgr to which 'symbol' belong will be
    //  got by 'findELFMgr' function or generated from AR file according to
    //  ELFARInfo. If a new ELFMgr is generated, SymbolInfo and RelocInfo which
    //  are belong to this ELFMgr need to be recorded into the Linkermgr.
    //6.RelocInfo will be resolved by this tharget 'symbol'.
    void resolveRelocInfoViaARFile();

    //Resolve RelocInfo. 'is_notype' dedicated whether 'symbol_info' is
    //with 'STT_NOTYPE' attribute. Since the 'STT_NOTYPE' symbol can be
    //used to resolve RelocInfo too.
    //'sym_name': the name of RelocInfo and SymbolInfo.
    //'is_notype': whether the SymbolInfo is with 'STT_NOTYPE' attribute.
    void resolveRelocInfo(Sym const* sym_name,
        MOD SymbolInfo * symbol_info, bool is_notype = false);

    //Resolve RelocInfo with SymbolInfo that comes from 'elf_mgr'.
    //'symbol_name': the name of RelocInfo and SymbolInfo.
    void resolveRelocInfoWithELFMgr(MOD ELFMgr * elf_mgr,
                                    Sym const* symbol_name);

    //SymbolInfo with 'STT_NOTYPE' attribute can be used to resolve RelocInfo
    //too. But it needs to be replaced if there is another SymbolInfo with the
    //same name and no 'STT_NOTYPE' attribute.
    //'sym_name": the name of RelocInfo and SymbolInfo.
    void resolveRelocInfoReplaceNoType(Sym const* sym_name,
                                       MOD SymbolInfo * symbol_info);

    //RelocInfo with the same name as 'reloc_info' has been resolved.
    //Thus it just needs to get target resolved SymbolInfo from
    //'m_resolved_reloc_idx_map' to resolve the 'reloc_info'.
    //'reloc_info': RelocInfo needs to be resolved.
    //'index': the index of 'reloc_info' in 'm_reloc_sym_vec'.
    void relocInfoHasBeenResolved(MOD RelocInfo * reloc_info, UINT index);

    //The RelocInfo has been recorded. The function will try to
    //find target SymbolInfo from 'elf_mgr' to resolve it.
    //'ar': AR file to which SymbolInfo may belong.
    //'elf_mgr': ELFMgr to which SymbolInfo may belong.
    //'reloc_info': RelocInfo needs to be resolved.
    //'idx': the index of 'elf_mgr' in 'ar'.
    void relocInfoHasBeenRecorded(ELFAR const* ar, MOD ELFMgr * elf_mgr,
                                  RelocInfo const* reloc_info, UINT64 idx);

    //Rename SymbolInfo and RelocInfo in 'elf_mgr'.
    //'elf_mgr': ELFMgr to which SymbolInfo and RelocInfo belong.
    //'target_symbol_info': get old name from this SymbolInfo.
    //'new_name': new name of SymbolInfo and RelocInfo.
    void renameSymbolName(MOD ELFMgr * elf_mgr,
        SymbolInfo const* target_symbol_info, CHAR const* new_name);

    //Set the number of SymbolInfo with the same name.
    //'symbol_info': SymbolInfo with the same name.
    //'v': the number.
    void setSameNameNum(SymbolInfo const* symbol_info, UINT v)
    {
        ASSERT0(symbol_info);
        m_same_name_num_map.setIfFind(SYMINFO_name(symbol_info), v);;
    }

    //Target resolved SymbolInfo exists in 'elf_mgr'. RelocInfo will be
    //resolved by the SymbolInfo.
    //'elf_mgr': ELFMgr to which SymbolInfo belong.
    //'reloc_info': RelocInfo that needs to be resolved.
    void targetSymbolInfoFoundInCurrentELFMgr(
        MOD ELFMgr * elf_mgr, RelocInfo const* reloc_info);

    //Try to find target resolved SymbolInfo of 'reloc_info' from 'elf_mgr'.
    //'ar': AR file to which target SymbolInfo may belong.
    //'idx': the index of 'elf_mgr' in 'ar'.
    //'ar_info': AR info that record the index of ELFMgr in AR file.
    //'elf_mgr': ELFMgr to which target SymbolInfo may belong.
    //'reloc_info': RelocInfo that needs to be resolved.
    bool targetSymbolInTheSameELFMgr(ELFAR const* ar, UINT64 idx,
        ELFARInfo const* ar_info, MOD ELFMgr * elf_mgr,
        RelocInfo const* reloc_info);

    //Try to find target resolved SymbolInfo of 'reloc_info' from other ELFMgr.
    //'ar_info': AR info that record the index of ELFMgr in AR file.
    //'reloc_info': RelocInfo that needs to be resolved.
    bool targetSymbolNoInTheSameELFMgr(
        ELFARInfo const* ar_info, RelocInfo const* reloc_info);

    //Collect valid info from 'elf_mgr' to LinkerMgr object.
    //These info mainly contain SymbolInfo and RelocInfo.
    void updateELFInfo(MOD ELFMgr * elf_mgr);

    //Record RelocInfo in 'elf_mgr' to 'm_reloc_symbol_vec'.
    void updateRelocInfoVec(ELFMgr const* elf_mgr);

    //Update RelocInfo offset after corresponded section merged.
    //'elf_mgr': ELFMgr to which RelocInfo belong.
    //'shdr': ELFSHdr to which RelocInfo belong.
    //'shdr_name': the name of 'shdr', used for getting corresponded section.
    //'shdr_idx': the index of 'shdr' in ELFHdr.
    void updateRelaOfst(MOD ELFMgr * elf_mgr, ELFSHdr const* shdr,
                        CHAR const* shdr_name, UINT sh_idx);

    //Update RELOCINFO_called_loc(reloc_info) after corresponded section merged.
    //It will be updated according to the reloc type in different architecture.
    virtual void updateRelaOfst(MOD RelocInfo * reloc_info);

    //Update SymbolInfo offset after corresponded section merged.
    //'elf_mgr': ELFMgr to which SymbolInfo belong.
    //'shdr': ELFSHdr to which SymbolInfo belong.
    //'sh_idx': the index of 'shdr' in ELFHdr.
    void updateSymbolOfst(MOD ELFMgr * elf_mgr,
                          ELFSHdr const* shdr, UINT sh_idx);

    //Record unresolved RelocInfo into 'm_unresolved_reloc_idx_map'.
    //'elf_mgr': ELFMgr to which RelocInfo belong.
    //'reloc_info': unresolved RelocInfo.
    //'idx': the index of 'reloc_info' in 'm_reloc_symbol_vec', it will
    //       be recorded into 'm_unresolved_reloc_idx_map'.
    void updateUnresolvedRelocInfo(
        MOD ELFMgr * elf_mgr, MOD RelocInfo * reloc_info, UINT idx);
};

}
#endif
