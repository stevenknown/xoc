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
#ifndef _ELF_HEADER_H_
#define _ELF_HEADER_H_

namespace elf {

class ELFMgr;

#define EI_MAG_HEAD 0x7f
#define EI_MAG_NUM  4
#define E_PAD_SIZE  9
#define E_PAD_ZERO  0

typedef UINT16 Half; //unsigned 16bit half
typedef UINT32 Word32; //unsigned 32bit word
typedef UINT64 Word64; //unsigned 64bit word
typedef INT32 SWord32; //signed 32bit word
typedef INT64 SWord64; //signed 64bit word

#include "elf32.h"
#include "elf64.h"

typedef Addr64 Addr; //unsigned largest address type
typedef Off64 Off; //unsigned largest offset type
typedef Word64 Word; //unsigned largest word
typedef SWord64 SWord; //signed largest word

////////////////////////////////////////////////////////////////////////////////
//ELF HEADER                                                                  //
////////////////////////////////////////////////////////////////////////////////
//ELF OBJECT FILE TYPES
#define ET_NONE 0 //no file type
#define ET_REL 1 //relocatable file
#define ET_EXEC 2 //executable file
#define ET_DYN 3 //shared object file
#define ET_CORE 4 //core file
#define ET_LOPROC 0xff00 //processor specific file types
#define ET_HIPROC 0xffff

//ELF MACHINE TYPES
#define EM_NONE 0 //No machine
#define EM_M32 1 //AT&T WE 32100
#define EM_SPARC 2 //SPARC
#define EM_386 3 //Intel 386
#define EM_68K 4 //Motorola 68000
#define EM_88K 5 //Motorola 88000
#define EM_IAMCU 6 //Intel MCU
#define EM_860 7 //Intel 80860
#define EM_MIPS 8 //MIPS R3000
#define EM_S370 9 //IBM System/370
#define EM_MIPS_RS3_LE 10 //MIPS RS3000 Little-endian
#define EM_PARISC 15 //Hewlett-Packard PA-RISC
#define EM_VPP500 17 //Fujitsu VPP500
#define EM_SPARC32PLUS 18 //Enhanced instruction set SPARC
#define EM_960 19 //Intel 80960
#define EM_PPC 20 //PowerPC
#define EM_PPC64 21 //PowerPC64
#define EM_S390 22 //IBM System/390
#define EM_SPU 23 //IBM SPU/SPC
#define EM_V800 36 //NEC V800
#define EM_FR20 37 //Fujitsu FR20
#define EM_RH32 38 //TRW RH-32
#define EM_RCE 39 //Motorola RCE
#define EM_ARM 40 //ARM
#define EM_ALPHA 41 //DEC Alpha
#define EM_SH 42 //Hitachi SH
#define EM_SPARCV9 43 //SPARC V9
#define EM_TRICORE 44 //Siemens TriCore
#define EM_ARC 45 //Argonaut RISC Core
#define EM_H8_300 46 //Hitachi H8/300
#define EM_H8_300H 47 //Hitachi H8/300H
#define EM_H8S 48 //Hitachi H8S
#define EM_H8_500 49 //Hitachi H8/500
#define EM_IA_64 50 //Intel IA-64 processor architecture
#define EM_MIPS_X 51 //Stanford MIPS-X
#define EM_COLDFIRE 52 //Motorola ColdFire
#define EM_68HC12 53 //Motorola M68HC12
#define EM_MMA 54 //Fujitsu MMA Multimedia Accelerator
#define EM_PCP 55 //Siemens PCP
#define EM_NCPU 56 //Sony nCPU embedded RISC processor
#define EM_NDR1 57 //Denso NDR1 microprocessor
#define EM_STARCORE 58 //Motorola Star*Core processor
#define EM_ME16 59 //Toyota ME16 processor
#define EM_ST100 60 //STMicroelectronics ST100 processor
#define EM_TINYJ 61 //Advanced Logic Corp. TinyJ embedded processor family
#define EM_X86_64 62 //AMD x86-64 architecture
#define EM_PDSP 63 //Sony DSP Processor
#define EM_PDP10 64 //Digital Equipment Corp. PDP-10
#define EM_PDP11 65 //Digital Equipment Corp. PDP-11
#define EM_FX66 66 //Siemens FX66 microcontroller
#define EM_ST9PLUS 67 //STMicroelectronics ST9+ 8/16 bit microcontroller
#define EM_ST7 68 //STMicroelectronics ST7 8-bit microcontroller
#define EM_68HC16 69 //Motorola MC68HC16 Microcontroller
#define EM_68HC11 70 //Motorola MC68HC11 Microcontroller
#define EM_68HC08 71 //Motorola MC68HC08 Microcontroller
#define EM_68HC05 72 //Motorola MC68HC05 Microcontroller
#define EM_SVX 73 //Silicon Graphics SVx
#define EM_ST19 74 //STMicroelectronics ST19 8-bit microcontroller
#define EM_VAX 75 //Digital VAX
#define EM_CRIS 76 //Axis Communications 32-bit embedded processor
#define EM_JAVELIN 77 //Infineon Technologies 32-bit embedded processor
#define EM_FIREPATH 78 //Element 14 64-bit DSP Processor
#define EM_ZSP 79 //LSI Logic 16-bit DSP Processor
#define EM_MMIX 80 //Donald Knuth's educational 64-bit processor
#define EM_HUANY 81 //Harvard University machine-independent object files
#define EM_PRISM 82 //SiTera Prism
#define EM_AVR 83 //Atmel AVR 8-bit microcontroller
#define EM_FR30 84 //Fujitsu FR30
#define EM_D10V 85 //Mitsubishi D10V
#define EM_D30V 86 //Mitsubishi D30V
#define EM_V850 87 //NEC v850
#define EM_M32R 88 //Mitsubishi M32R
#define EM_MN10300 89 //Matsushita MN10300
#define EM_MN10200 90 //Matsushita MN10200
#define EM_PJ 91 //picoJava
#define EM_OPENRISC 92 //OpenRISC 32-bit embedded processor
#define EM_ARC_COMPACT 93 //ARC International ARCompact processor (old
                          //spelling/synonym: EM_ARC_A5)
#define EM_XTENSA 94 //Tensilica Xtensa Architecture
#define EM_VIDEOCORE 95 //Alphamosaic VideoCore processor
#define EM_TMM_GPP 96 //Thompson Multimedia General Purpose Processor
#define EM_NS32K 97 //National Semiconductor 32000 series
#define EM_TPC 98 //Tenor Network TPC processor
#define EM_SNP1K 99 //Trebia SNP 1000 processor
#define EM_ST200 100 //STMicroelectronics (www.st.com) ST200
#define EM_IP2K 101 //Ubicom IP2xxx microcontroller family
#define EM_MAX 102 //MAX Processor
#define EM_CR 103 //National Semiconductor CompactRISC microprocessor
#define EM_F2MC16 104 //Fujitsu F2MC16
#define EM_MSP430 105 //Texas Instruments embedded microcontroller msp430
#define EM_BLACKFIN 106 //Analog Devices Blackfin (DSP) processor
#define EM_SE_C33 107 //S1C33 Family of Seiko Epson processors
#define EM_SEP 108 //Sharp embedded microprocessor
#define EM_ARCA 109 //Arca RISC Microprocessor
#define EM_UNICORE 110 //Microprocessor series from PKU-Unity Ltd. and MPRC
                       //of Peking University
#define EM_EXCESS 111 //eXcess: 16/32/64-bit configurable embedded CPU
#define EM_DXP 112 //Icera Semiconductor Inc. Deep Execution Processor
#define EM_ALTERA_NIOS2 113 //Altera Nios II soft-core processor
#define EM_CRX 114 //National Semiconductor CompactRISC CRX
#define EM_XGATE 115 //Motorola XGATE embedded processor
#define EM_C166 116 //Infineon C16x/XC16x processor
#define EM_M16C 117 //Renesas M16C series microprocessors
#define EM_DSPIC30F 118 //Microchip Technology dsPIC30F Digital Signal
                        //Controller
#define EM_CE 119 //Freescale Communication Engine RISC core
#define EM_M32C 120 //Renesas M32C series microprocessors
#define EM_TSK3000 131 //Altium TSK3000 core
#define EM_RS08 132 //Freescale RS08 embedded processor
#define EM_SHARC 133 //Analog Devices SHARC family of 32-bit DSP
                     //processors
#define EM_ECOG2 134 //Cyan Technology eCOG2 microprocessor
#define EM_SCORE7 135 //Sunplus S+core7 RISC processor
#define EM_DSP24 136 //New Japan Radio (NJR) 24-bit DSP Processor
#define EM_VIDEOCORE3 137 //Broadcom VideoCore III processor
#define EM_LATTICEMICO32 138 //RISC processor for Lattice FPGA architecture
#define EM_SE_C17 139 //Seiko Epson C17 family
#define EM_TI_C6000 140 //The Texas Instruments TMS320C6000 DSP family
#define EM_TI_C2000 141 //The Texas Instruments TMS320C2000 DSP family
#define EM_TI_C5500 142 //The Texas Instruments TMS320C55x DSP family
#define EM_MMDSP_PLUS 160 //STMicroelectronics 64bit VLIW Data Signal Processor
#define EM_CYPRESS_M8C 161 //Cypress M8C microprocessor
#define EM_R32C 162 //Renesas R32C series microprocessors
#define EM_TRIMEDIA 163 //NXP Semiconductors TriMedia architecture family
#define EM_HEXAGON 164 //Qualcomm Hexagon processor
#define EM_8051 165 //Intel 8051 and variants
#define EM_STXP7X 166 //STMicroelectronics STxP7x family of configurable
                      //and extensible RISC processors
#define EM_NDS32 167 //Andes Technology compact code size embedded RISC
                     //processor family
#define EM_ECOG1 168 //Cyan Technology eCOG1X family
#define EM_ECOG1X 168 //Cyan Technology eCOG1X family
#define EM_MAXQ30 169 //Dallas Semiconductor MAXQ30 Core Micro-controllers
#define EM_XIMO16 170 //New Japan Radio (NJR) 16-bit DSP Processor
#define EM_MANIK 171 //M2000 Reconfigurable RISC Microprocessor
#define EM_CRAYNV2 172 //Cray Inc. NV2 vector architecture
#define EM_RX 173 //Renesas RX family
#define EM_METAG 174 //Imagination Technologies META processor
                     //architecture
#define EM_MCST_ELBRUS 175 //MCST Elbrus general purpose hardware architecture
#define EM_ECOG16 176 //Cyan Technology eCOG16 family
#define EM_CR16 177 //National Semiconductor CompactRISC CR16 16-bit
                    //microprocessor
#define EM_ETPU 178 //Freescale Extended Time Processing Unit
#define EM_SLE9X 179 //Infineon Technologies SLE9X core
#define EM_L10M 180 //Intel L10M
#define EM_K10M 181 //Intel K10M
#define EM_AARCH64 183 //ARM AArch64
#define EM_AVR32 185 //Atmel Corporation 32-bit microprocessor family
#define EM_STM8 186 //STMicroeletronics STM8 8-bit microcontroller
#define EM_TILE64 187 //Tilera TILE64 multicore architecture family
#define EM_TILEPRO 188 //Tilera TILEPro multicore architecture family
#define EM_MICROBLAZE 189 //Xilinx MicroBlaze 32-bit RISC soft processor core
#define EM_CUDA 190 //NVIDIA CUDA architecture
#define EM_TILEGX 191 //Tilera TILE-Gx multicore architecture family
#define EM_CLOUDSHIELD 192 //CloudShield architecture family
#define EM_COREA_1ST 193 //KIPO-KAIST Core-A 1st generation processor family
#define EM_COREA_2ND 194 //KIPO-KAIST Core-A 2nd generation processor family
#define EM_ARC_COMPACT2 195 //Synopsys ARCompact V2
#define EM_OPEN8 196 //Open8 8-bit RISC soft processor core
#define EM_RL78 197 //Renesas RL78 family
#define EM_VIDEOCORE5 198 //Broadcom VideoCore V processor
#define EM_78KOR 199 //Renesas 78KOR family
#define EM_56800EX 200 //Freescale 56800EX Digital Signal Controller (DSC)
#define EM_BA1 201 //Beyond BA1 CPU architecture
#define EM_BA2 202 //Beyond BA2 CPU architecture
#define EM_XCORE 203 //XMOS xCORE processor family
#define EM_MCHP_PIC 204 //Microchip 8-bit PIC(r) family
#define EM_INTEL205 205 //Reserved by Intel
#define EM_INTEL206 206 //Reserved by Intel
#define EM_INTEL207 207 //Reserved by Intel
#define EM_INTEL208 208 //Reserved by Intel
#define EM_INTEL209 209 //Reserved by Intel
#define EM_KM32 210 //KM211 KM32 32-bit processor
#define EM_KMX32 211 //KM211 KMX32 32-bit processor
#define EM_KMX16 212 //KM211 KMX16 16-bit processor
#define EM_KMX8 213 //KM211 KMX8 8-bit processor
#define EM_KVARC 214 //KM211 KVARC processor
#define EM_CDP 215 //Paneve CDP architecture family
#define EM_COGE 216 //Cognitive Smart Memory Processor
#define EM_COOL 217 //iCelero CoolEngine
#define EM_NORC 218 //Nanoradio Optimized RISC
#define EM_CSR_KALIMBA 219 //CSR Kalimba architecture family
#define EM_AMDGPU 224 //AMD GPU architecture
#define EM_RISCV 243 //RISC-V
#define EM_LANAI 244 //Lanai 32-bit processor
#define EM_BPF 247 //Linux kernel bpf virtual machine
#define EM_VE 251 //NEC SX-Aurora VE
#define EM_CSKY 252 //C-SKY 32-bit processor
#define EM_SWAI_64 0x9906 //SWAI 64-bit processor
#define EM_LAST EM_SWAI_64

#define EC_32BIT 1
#define EC_64BIT 2

#define ED_LITTLE 1
#define ED_BIG 2

#pragma pack(1)
//Preload ident info that is used to determine file class.
typedef struct {
    BYTE e_ident[EI_MAG_NUM]; //Magic number: 0x7f'ELF'
    BYTE e_class; //Address size: 1=32-bit, 2=64-bit
    BYTE e_data; //Endian: 1=little-endian, 2=big-endian
    BYTE e_hversion; //Header version, always 1
    BYTE e_pad[9]; //Auxilliary information

    //ELF file type: 0=no file type 1=relocatable, 2=executable,
    //               3=share object, 4=core image
    Half e_type;

    //Machine detail: 2=SPACE , 3=x86 , 4=68k , 5=88K, 7=Intel_80860,
    //                8=MIPS_RS3000, reserved.
    Half e_machine;
    bool is64bit() const { return e_class == EC_64BIT; }
    bool is32bit() const { return e_class == EC_32BIT; }
} ELFIdent;

class ELFHdr {
public:
    BYTE e_ident[EI_MAG_NUM]; //Magic number: 0x7f'ELF'
    BYTE e_class; //Address size: 1=32-bit, 2=64-bit
    BYTE e_data; //Endian: 1=little-endian, 2=big-endian
    BYTE e_hversion; //Header version, always 1
    BYTE e_pad[9]; //Auxilliary information

    //ELF object file types
    Half e_type;

    //Machine detail.
    Half e_machine;
    Word32 e_version; //Object file version
    Addr e_entry; //Entry point
    Off e_phoff; //Program header table file offset from the start of the file
    Off e_shoff; //Section header table file offset from the start of the file
    Word32 e_flags; //Processor-specific flags
    Half e_ehsize; //ELF header size in bytes
    Half e_phensize; //Program header table entry size
    Half e_phnum;   //Program header table entry count
    Half e_shensize; //Section header table entry size
    Half e_shnum; //Section header table entry count
    Half e_shstrndx; //Section header string table index
public:
    ELFHdr() {}
    //Get the packed byte size of a ELF header structure in 32bit or 64bit
    //machine.
    static UINT getSize(ELFMgr const* mgr);
    BYTE const (*getIdent() const) [EI_MAG_NUM];

    void extract(BYTE const* buf, ELFMgr const* mgr);
    void insert(BYTE const* buf, ELFMgr const* mgr);
    bool isELF() const;
    bool is64bit() const { return e_class == 2; }
    bool is32bit() const { return e_class == 1; }
    void setMagic();
};
#pragma pack()

////////////////////////////////////////////////////////////////////////////////
//PROGRAM HEADER                                                              //
////////////////////////////////////////////////////////////////////////////////
//SEGMENT TYPES
#define PT_NULL 0 //unused segment
#define PT_LOAD 1 //loadable segment
#define PT_DYNAMIC 2 //contains dynamic linking information
#define PT_INTERP 3 //reference to a program interpreter
#define PT_NOTE 4 //comments & auxiliary information
#define PT_SHLIB 5 //here be dragons
#define PT_PHDR 6 //address of prog. header in mem (for interp.)
#define PT_OS 0x60000001 //target os information
#define PT_RES 0x60000002 //read-only resource information
#define PT_LOPROC 0x70000000 //processor specific
#define PT_HIPROC 0x7fffffff //processor specific

//Segment flag bits
#define PF_X 0x1 //seg has execute permissions
#define PF_W 0x2 //seg has write permissions
#define PF_R 0x4 //seg has read permissions
#define PF_S 0x01000000 //segment is shared.
#define PF_MASKPROC 0xf0000000 //processor-specific flag mask

class ELFPHdr {
public:
    Word32 p_type; //Segment type
    Off p_offset; //Segment file offset
    Addr p_vaddr; //Segment virtual address
    Addr p_paddr; //Segment physical address
    Word32 p_filesz; //Segment size in file
    Word32 p_memsz; //Segment size in memory
    Word32 p_flags; //Segment flags
    Word32 p_align; //Segment alignment
public:
    static UINT getMachBitWidth(ELFMgr const* mgr);
    void extract(BYTE const* buf, ELFMgr const* mgr);
    void insert(BYTE const* buf, ELFMgr const* mgr);
};

////////////////////////////////////////////////////////////////////////////////
//SECTION TABLE ENTRY                                                         //
////////////////////////////////////////////////////////////////////////////////
//==============================================================================
//PRE-EXISTING EXTENSIONS
//.sdata .tdesc
//.sbss  .lit4
//.lit8  .reginfo
//.gptab .liblist
//.conflict

//==============================================================================
//NAME        TYPE         ATTRIBUTES
//.bss        SHT_NOBITS   SHF_ALLOC+SHF_WRITE
//.comment    SHT_PROGBITS none
//.data       SHT_PROGBITS SHF_ALLOC+SHF_WRITE
//.data1      SHT_PROGBITS SHF_ALLOC+SHF_WRITE
//.debug      SHT_PROGBITS none
//.dynamic    SHT_DYNAMIC  SHF_ALLOC+(SHF_WRITE)
//.dynstr     SHT_STRTAB   SHF_ALLOC
//.dynsym     SHT_DYNSYM   SHF_ALLOC
//.fini       SHT_PROGBITS SHF_ALLOC+SHF_EXECINSTR
//.got        SHT_PROGBITS see below
//.hash       SHT_HASH     SHF_ALLOC
//.init       SHT_PROGBITS SHF_ALLOC+SHF_EXECINSTR
//.interp     SHT_PROGBITS none
//.line       SHT_PROGBITS none
//.note       SHT_NOTE     none
//.plt        SHT_PROGBITS none
//.rel<name>  SHT_REL      none
//.rela<name> SHT_RELA     none
//.rodata     SHT_PROGBITS SHF_ALLOC
//.rodata1    SHT_PROGBITS SHF_ALLOC
//.shstrtab   SHT_STRTAB   none
//.strtab     SHT_STRTAB   none
//.symtab     SHT_SYMTAB   none
//.text       SHT_PROGBITS SHF_ALLOC+SHF_EXECINSTR

//==============================================================================
//SECTION EXPLANATION.
//------------------------------------------------------------------------------
//.bss
//This section holds uninitialized data that contribute to the program's memory
//image.
//By definition, the system initializes the data with zeros when the program
//begins to run. The section occupies no file space, as indicated by the
//section type, SHT_NOBITS.
//------------------------------------------------------------------------------
//.comment
//This section holds version control information.
//------------------------------------------------------------------------------
//.data and .data1
//These sections hold initialized data that contribute to the program's memory
//image.
//------------------------------------------------------------------------------
//.debug
//This section holds information for symbolic debugging. The contents are
//unspecified.
//All section names with the prefix .debug are reserved for future use in the
//ABI.
//------------------------------------------------------------------------------
//.dynamic
//This section holds dynamic linking information. The section's attributes will
//include the SHF_ALLOC bit. Whether the SHF_WRITE bit is set is processor
//specific.
//------------------------------------------------------------------------------
//.dynstr
//This section holds strings needed for dynamic linking, most commonly the
//strings that represent the names associated with symbol table entries.
//------------------------------------------------------------------------------
//.dynsym
//This section holds the dynamic linking symbol table, as described in Symbol
//Table.
//------------------------------------------------------------------------------
//.fini
//This section holds executable instructions that contribute to the process
//termination code. That is, when a program exits normally, the system
//arranges to execute the code in this section.
//------------------------------------------------------------------------------
//.fini_array
//This section holds an array of function pointers that contributes to a single
//termination array for the executable or shared object containing the section.
//------------------------------------------------------------------------------
//.got
//This section holds the global offset table. Global Offset Table.
//------------------------------------------------------------------------------
//.hash
//This section holds a symbol hash table.
//------------------------------------------------------------------------------
//.init
//This section holds executable instructions that contribute to the process
//initialization code. When a program starts to run, the system arranges
//to execute the code in this section before calling the main program entry
//point (called main for C programs).
//------------------------------------------------------------------------------
//.init_array
//This section holds an array of function pointers that contributes to a
//single initialization array for the executable or shared object containing
//the section.
//------------------------------------------------------------------------------
//.interp
//This section holds the path name of a program interpreter.
//If the file has a loadable segment that includes relocation, the sections's
//attributes will include the SHF_ALLOC bit; otherwise, that bit will be off.
//------------------------------------------------------------------------------
//.line
//This section holds line number information for symbolic debugging, which
//describes the correspondence between the source program and the machine code.
//The contents are unspecified.
//------------------------------------------------------------------------------
//.note
//This section holds information in the format that ``Note Section''.
//------------------------------------------------------------------------------
//.plt
//This section holds the procedure linkage table. Procedure Linkage Table.
//------------------------------------------------------------------------------
//.preinit_array
//This section holds an array of function pointers that contributes to a single
//pre-initialization array for the executable or shared object containing the
//section.
//------------------------------------------------------------------------------
//.rel.name and .rela.name
//These sections hold relocation information, as described in Relocation.
//If the file has a loadable segment that includes relocation, the sections's
//attributes will include the SHF_ALLOC bit; otherwise, that bit will be off.
//A relocation section can reference two other sections:
//  * a symbol table, identified by the s_link section header entry;
//  * and a section to modify, identified by the s_info section header entry.
//Conventionally, name is supplied by the section to which the relocations
//apply.
//e.g: a relocation section for .text normally would have the name .rel.text or
//.rela.text.
//------------------------------------------------------------------------------
//.rodata and .rodata1
//These sections hold read-only data that typically contribute to a non-writable
//segment in the process image.
//------------------------------------------------------------------------------
//.shstrtab
//This section holds section names.
//------------------------------------------------------------------------------
//.strtab
//This section holds strings, most commonly the strings that represent the names
//associated with symbol table entries. If the file has a loadable segment that
//includes the symbol string table, the section's attributes will include the
//SHF_ALLOC bit; otherwise, that bit will be off.
//------------------------------------------------------------------------------
//.symtab
//This section holds a symbol table, as Symbol Table. If the file has a loadable
//segment that includes the symbol table, the section's attributes will include
//the SHF_ALLOC bit; otherwise, that bit will be off.
//------------------------------------------------------------------------------
//.symtab_shndx
//This section holds the special symbol table section index array, as described
//above. The section's attributes will include the SHF_ALLOC bit if the
//associated symbol table section does; otherwise that bit will be off.
//------------------------------------------------------------------------------
//.text
//This section holds the text, or executable instructions, of a program.
//------------------------------------------------------------------------------

//TABLE T_1
//There are two members in the section header, s_link and s_info, hold
//special information, depending on section type.
//s_type     s_link                         s_info
//==============================================================================
//S_DYNAMIC |The section header index of   |0
//          |the string table used by      |
//          |entries in the section.       |
//----------|------------------------------|------------------------------------
//S_HASH    |The section header index of   |0
//          |the SYMBOL table to which the |
//          |hash table applies.           |
//----------|------------------------------|------------------------------------
//S_REL     |The section header index of   |The section header index of
//          |the associated symbol table.  |the section to which the
//          |                              |relocation applies.
//----------|------------------------------|------------------------------------
//S_RELA    |The section header index of   |The section header index of
//          |the associated symbol table.  |the section to which the
//          |                              |relocation applies.
//----------|------------------------------|------------------------------------
//S_SYMTAB  |The section header index of   |One greater than the symbol
//          |the associated string table.  |table index of the last local
//          |                              |symbol (binding STB_LOCAL).
//----------|------------------------------|------------------------------------
//S_DYNSYM  |The section header index of   |One greater than the symbol
//          |the associated string table.  |table index of the last local
//          |                              |symbol (binding STB_LOCAL).
//------------------------------------------------------------------------------

//==============================================================================
//SECTION FLAG VALUE
//  W (write)
//  A (alloc)
//  X (execute)
//  M (merge)
//  S (strings)
//  I (info_link)
//  L (link order)
//  G (group)
//  x (unknown)
//  O (nonconforming OS flags)
//  o (OS specific)
//  p (processor specific)
#define SF_WRITE 0x1 //section writable during execution
#define SF_ALLOC 0x2 //section occupies space during exec.
#define SF_EXECINSTR 0x4 //section contains executable code.

//The data in the section may be merged to eliminate duplication.
//Unless the SF_STRINGS flag is also set, the data elements in the section are
//of a uniform size. The size of each element is specified in the section
//header's s_entry_size field. If the SF_STRINGS flag is also set, the data
//elements consist of null-terminated character strings. The size of each
//character is specified in the section header's s_entry_size field.
//Each element in the section is compared against other elements in sections
// with the same name, type and flags. Elements that would have identical
//values at program run-time may be merged. Relocations referencing elements
//of such sections must be resolved to the merged locations of the
//referenced values. Note that any relocatable values, including values
//that would result in run-time relocations, must be analyzed to
//determine whether the run-time values would actually be identical.
//An ABI-conforming object file may not depend on specific elements being
//merged, and an ABI-conforming link editor may choose not to merge specific
//elements.
#define SF_MERGE 0x10

//The data elements in the section consist of null-terminated character strings.
//The size of each character is specified in the section header's s_entry_size
//field.
#define SF_STRINGS 0x20

//The s_info field of this section header holds a section header table index.
#define SF_INFO_LINK 0x40

//This flag adds special ordering requirements for link editors.
//The requirements apply if the s_link field of this section's header
//references another section (the linked-to section).
//If this section is combined with other sections in the output file,
//it must appear in the same relative order with respect to those sections,
//as the linked-to section appears with respect to sections the linked-to
//section is combined with.
//A typical use of this flag is to build a table that references text or
//data sections in address order.
#define SF_LINK_ORDER 0x80

//This section requires special OS-specific processing
//(beyond the standard linking rules) to avoid incorrect behavior.
//If this section has either an s_type value or contains s_flags bits in the
//OS-specific ranges for those fields, and a link editor processing this
//section does not recognize those values, then the link editor should reject
//the object file containing this section with an error.
#define SF_OS_NONCONFORMING 0x100

//This section is a member (perhaps the only one) of a section group.
//The section must be referenced by a section of type SHT_GROUP.
//The SHF_GROUP flag may be set only for sections contained in relocatable
//objects (objects with the ELF header e_type member set to ET_REL).
#define SF_GROUP 0x200

//All bits included in this mask are reserved for operating system-specific
//semantics.
#define SF_MASKOS 0x0ff00000

//All bits included in this mask are reserved for processor-specific semantics.
//If meanings are specified, the processor supplement explains them.
#define SF_MASKPROC 0xf0000000

//Section to be placed at the beginning of like-named sections by static link.
#define SF_BEGIN 0x01000000

//Section to be placed at the end of like-named sections by static link.
#define SF_END 0x02000000

//==============================================================================
//SECTION TYPES
#define S_UNDEF 0 //inactive, unknown section
#define S_PROGBITS 1 //meaning defined by program
#define S_SYMTAB 2 //symbol table
#define S_STRTAB 3 //string table
#define S_RELA 4 //reloc entries with explicit addends
#define S_HASH 5 //symbol hash table
#define S_DYNAMIC 6 //dynamic linking information
#define S_NOTE 7 //comment information
#define S_NOBITS 8 //like PROGBITS but no space in file.
#define S_REL 9 //as RELA but no explicit addends
#define S_SHLIB 10 //reserved but evil
#define S_DYNSYM 11 //dynamic link symbol table
#define S_OS 0x60000001 //info to identify target OS
#define S_IMPORTS 0x60000002 //info on refs to external symbols
#define S_EXPORTS 0x60000003 //info on symbols exported by ordinal
#define S_RES 0x60000004 //read-only resource data.
#define S_PROGFRAGS 0x60001001 //similar to SHT_PROGBITS
#define S_IDMDLL 0x60001002 //symbol name demangling information
#define S_DEFLIB 0x60001003 //default static libraries
#define S_LOPROC 0x70000000 //processor specific
#define S_HIPROC 0x7fffffff //processor specific
#define S_LOUSER 0x80000000 //user defined sections
#define S_HIUSER 0xffffffff //user defined sections
#define S_VERSYM 0x6fffffff //gnu version
#define S_VERNEED 0x6ffffffe //gnu version needed info
//Legacy section types.
//Readers should handle these legacy type, writers must use the related news.
#define S_OS_O 12 //info to identify target OS
#define S_IMPORTS_O 13 //info on refs to external symbols
#define S_EXPORTS_O 14 //info on symbols exported by ordinal
#define S_RES_O 15 //read-only resource data.

class ELFSHdr {
public:
    Word32 s_name; //name of the section
    Word32 s_type; //section type
    Addr s_flags; //section flags value: W,A,X, etc.
    Addr s_addr; //starting address of section in image
    Off s_offset; //start of section in file
    Addr s_size; //size of section in file.

    //This member holds a section header table index link, whose
    //interpretation depends on the section type.
    //A table T_1 above describes the values.
    Word32 s_link;

    //Another multipurpose field (based on type) in .rel section, it give the
    //relatively section which should be relocated.
    //If the s_flags field for this section header includes the attribute
    //SF_INFO_LINK, then this member represents a section header table index.
    Word32 s_info;

    //Address alignment.
    //Some sections have address alignment constraints. For example, if a
    //section holds a 8byte word, the system must ensure 8byte alignment for
    //the entire section. In this case, the value of s_addr must be congruent
    //to 0, modulo the value of s_addr_align. Currently, only 0 and positive
    //integral powers of 2 are allowed. Values 0 and 1 mean the section
    //has no alignment constraints. Thus the default value can be 0.
    Addr s_addr_align;

    //This member indicates the byte size of each element in the section.
    //Note the member only record entry-size for sections with FIXED sized
    //entries.
    Word s_entry_size;

    //Record the section content.
    //Lazy loaded. Be allocated just while the content is going to use.
    BYTE * s_content;

    //Record the name string of the section.
    CHAR const* s_name_str;
public:
    ELFSHdr() { ::memset((void*)this, 0, sizeof(ELFSHdr)); }

    //Get the packed byte size of a section header structure in 32bit or 64bit
    //machine.
    static UINT getSize(ELFMgr const* mgr);

    //The function return the symbol-table section headder that corresponding
    //to given section header 'sh'.
    //Note not all section headers have a corresponding symbol-table.
    ELFSHdr * getRelatedSymTab(ELFMgr const* mgr) const;

    //The function return the string-table section headder that corresponding
    //to given section header 'sh'.
    //Note not all section headers have a corresponding string-table.
    ELFSHdr * getRelatedStrTab(ELFMgr const* mgr) const;

    //The function return the number of elements in current section.
    //Note the size of element must be FIXED, thus s_entry_size should be ready.
    size_t getElemNum() const
    {
        ASSERTN(s_entry_size != 0, ("miss element byte size"));
        return (size_t)(s_size / s_entry_size);
    }

    //Return true if current section is symbol-table.
    bool isSymTab() const { return s_type == S_SYMTAB || s_type == S_DYNSYM; }

    //Return true if current section is string-table.
    bool isStrTab() const { return s_type == S_STRTAB; }
    void insert(BYTE const* buf, ELFMgr const* mgr);

    void extract(BYTE const* buf, ELFMgr const* mgr);
};

////////////////////////////////////////////////////////////////////////////////
//SYMBOL TABLE ENTRY                                                          //
////////////////////////////////////////////////////////////////////////////////
//SYMBOL SECTION INDEX
//This section table index means the symbol is undefined. When the link
//editor combines this object file with another that defines the indicated
//symbol, this file's references to the symbol will be linked to the actual
//definition.
#define SHN_UNDEF 0

//This value specifies the lower bound of the range of reserved indexes.
#define SHN_LORESERVE 0xff00
#define SHN_LOPROC 0xff00 //reserved for processor-specific semantics
#define SHN_HIPROC 0xff1f //reserved for processor-specific semantics

//This value specifies absolute values for the corresponding reference. For
//example, symbols defined relative to section number SHN_ABS have
//absolute values and are not affected by relocation.
//Indicates references to this section are absolute address, thus this kind
//of symbol does not need to be relocated.
#define SHN_ABS 0xfff1

//Symbols defined relative to this section are common symbols, such as
//FORTRAN COMMON or unallocated C external variables.
//References to this section are common.
//After allocated by linker, the 'COMMON' symbol moved into .bss section.
//If a symbol is SHN_COMMON, it will be allocated by linker that aligned with
//st_align.
#define SHN_COMMON 0xfff2

//This value specifies the upper bound of the range of reserved indexes. The
//system reserves indexes between SHN_LORESERVE and
//SHN_HIRESERVE, inclusive; the values do not reference the section header
//table.That is, the section header table does not contain entries for the
//reserved indexes.
#define SHN_HIRESERVE 0xffff

//==============================================================================
//BIND SUBFIELD CONTENTS
#define STB_LOCAL 0 //symbol has local binding
#define STB_GLOBAL 1 //symbol has global binding
#define STB_WEAK 2 //symbol has weak binding
#define STB_ENTRY 12 //symbol is entry-point for the load module
#define STB_LOPROC 13 //processor specific semantics
#define STB_HIPROC 15

//TYPE SUBFIELD CONTENTS
#define STT_NOTYPE 0 //not specified
#define STT_OBJECT 1 //symbol is a data object
#define STT_FUNC 2 //symbol is a code symbol
#define STT_SECTION 3 //symbol associated with a section
#define STT_FILE 4 //symbol gives name of the source file.
#define STT_IMPORT 11 //reference to a symbol in another module
#define STT_LOPROC 13 //processor specific semantics
#define STT_HIPROC 15

//VISIBLITY SUBFIELD CONTENTS
//
//These attribute flags are the value of Vis field in
//generating elf symtab.
//-------------------- .symtab -----------------------
//Num: | Value | Size | Type | Bind | Vis | Ndx | Name
//----------------------------------------------------
#define STV_DEFAULT   0 //Default symbol visibility rules.
#define STV_INTERNAL  1 //Processor specific hidden class.
#define STV_HIDDEN    2 //Symbol unavailable in other modules.
#define STV_PROTECTED 3 //Not preemptible, not exported.

class ELFSym {
public:
    //If the value is non-zero, it represents a string table index that gives
    //the symbol name. Otherwise, the symbol table entry has no name.
    //Note if st_name is 0, the sym+idx in ELFRela indicates the offset to the
    //section begin that described by st_shndx.
    Word st_name;
    union {
        //NOTE: 'st_value' give the symbol absolute position, while the
        //'st_sect' is 'ABS', namely absolutely in executable image or
        //anything else, and the value will not be changed at any time.
        //If 'st_sect' is 'COMMON', then the 'st_align' give
        //the required alignment granularity, and 'st_size' gives the minimum
        //size, namely, the precisely object or function body length.
        Addr st_value;
        Addr st_align;
    };

    //Symbol size.
    //Note the size is different for each kind of symbol type. The size is the
    //entire object size sometime, whereas it is 0 if the object size is
    //unknown.
    Addr st_size;

    //This member specifies the symbol's type and binding attributes.
    //A list of the values and meanings appears above subfield contents.
    UCHAR st_type:4; //data object, function, section, or special-case file
    UCHAR st_bind:4; //local, global, or weak

    //This member currently holds 0 and has no defined meaning.
    UCHAR st_other;

    //Every symbol table entry is "defined" in relation to some section;
    //this member holds the relevant section header table index.
    //As the SPEC related text describe, some section indexes indicate special
    //meanings.
    //section index is one of ABS, COMMON or UNDEF.
    Half st_shndx;
public:
    void extract(BYTE const* buf, ELFMgr const* mgr);

    //Get the align value of elements in ELFSym.
    //
    // Defined in "elf/elf64.h".          Defined in "elf/elf32.h".
    //   typedef struct {                   typedef struct {
    //       Word32 st_name;                    Word32 st_name;
    //       UCHAR st_type:4;                   UCHAR st_type:4;
    //       UCHAR st_bind:4;                   UCHAR st_bind:4;
    //       UCHAR st_other;                    UCHAR st_other;
    //       Half st_shndx;                     Half st_shndx;
    //       union {                            union {
    //           Addr64 st_value;                   Addr32 st_value;
    //           Addr64 st_align;                   Addr32 st_align;
    //       };                                 };
    //       Addr64 st_size;                    Word32 st_size;
    //   } ELFSym64;                         } ELFSym32;
    // Align with sizeof(Addr64).          Align with sizeof(Word32).
    static UINT getAlign(ELFMgr const* mgr);

    //Get the packed byte size of a symbol structure in 32bit or 64bit machine.
    static UINT getSize(ELFMgr const* mgr);

    void insert(BYTE const* buf, ELFMgr const* mgr);
};

////////////////////////////////////////////////////////////////////////////////
//MAP SYMBOL TO ACTUALLY LOCATION                                             //
////////////////////////////////////////////////////////////////////////////////
typedef struct {
    ELFSym * sym; //object or function relatived symbol
    ULONG ofst; //actually offset refer to the start of segment. e.g .data, .bss
} ELFMapSym;

typedef struct {
    ELFMapSym * data; //relatively mapping information
    ULONG count; //amount of entries
} ELFMapData;

//Map symbol to actually location
typedef struct {
    ELFSym* sym; //section relatived symbol

    //Finially absolutely address for the start of segment/section.
    size_t abs_addr;
    ULONG section_map_size; //mapping data size of segment/section
} ELFMapSect;

////////////////////////////////////////////////////////////////////////////////
//DYNAMIC SEGMENT ENTRY                                                       //
////////////////////////////////////////////////////////////////////////////////
//DYNAMIC ARRAY TAGS, D_TAG
//Name        Value      d_un        Executable  Shared Object
//==============================================================================
//DT_NULL     0          ignored     mandatory   mandatory
//DT_NEEDED   1          d_val       optional    optional
//DT_PLTRELSZ 2          d_val       optional    optional
//DT_PLTGOT   3          d_ptr       optional    optional
//DT_HASH     4          d_ptr       mandatory   mandatory
//DT_STRTAB   5          d_ptr       mandatory   mandatory
//DT_SYMTAB   6          d_ptr       mandatory   mandatory
//DT_RELA     7          d_ptr       mandatory   optional
//DT_RELASZ   8          d_val       mandatory   optional
//DT_RELAENT  9          d_val       mandatory   optional
//DT_STRSZ    10         d_val       mandatory   mandatory
//DT_SYMENT   11         d_val       mandatory   mandatory
//DT_INIT     12         d_ptr       optional    optional
//DT_FINI     13         d_ptr       optional    optional
//DT_SONAME   14         d_val       ignored     optional
//DT_RPATH    15         d_val       optional    ignored
//DT_SYMBOLIC 16         ignored     ignored     optional
//DT_REL      17         d_ptr       mandatory   optional
//DT_RELSZ    18         d_val       mandatory   optional
//DT_RELENT   19         d_val       mandatory   optional
//DT_PLTREL   20         d_val       optional    optional
//DT_DEBUG    21         d_ptr       optional    ignored
//DT_TEXTREL  22         ignored     optional    optional
//DT_JMPREL   23         d_ptr       optional    optional
//DT_LOPROC   0x70000000 unspecified unspecified unspecified
//DT_HIPROC   0x7fffffff unspecified unspecified unspecified
#define DT_NULL 0
#define DT_NEEDED 1 //name of a needed library
#define DT_PLTRELSZ 2 //size of reloc entries for PLT
#define DT_PLTGOT 3 //address with PLT or GOT
#define DT_HASH 4 //symbol hash table address
#define DT_STRTAB 5 //string table address
#define DT_SYMTAB 6 //symbol table address
#define DT_RELA 7 //address of reloc table with addends
#define DT_RELASZ 8 //size of the DT_RELA table
#define DT_RELAENT 9 //size of a DT_RELA entry
#define DT_STRSZ 10 //size of the string table
#define DT_SYMENT 11 //size of a symbol table entry
#define DT_SONAME 14 //shared object name
#define DT_REL 17 //address of reloc table without addends
#define DT_RELSZ 18 //size of the DT_REL table
#define DT_RELENT 19 //size of a DT_REL entry
#define DT_PLTREL 20 //type of reloc entry for PLT
#define DT_DEBUG 21 //for debugging information
#define DT_JMPREL 23 //reloc entries only with PLT
#define DT_EXPORT 0x60000001 //address of export table
#define DT_EXPORTSZ 0x60000002 //size of export table
#define DT_EXPENT 0x60000003 //size of export table entry
#define DT_IMPORT 0x60000004 //address of import table
#define DT_IMPORTSZ 0x60000005 //size of import table
#define DT_IMPENT 0x60000006 //size of import table entry
#define DT_IT 0x60000007 //init and term types for a DLL.

//Relative priority of init and term to other functions.
#define DT_ITPRTY 0x60000008
#define DT_INITTERM 0x60000009 //address of init and term function
#define DT_PPC_GOT 0x70000001 //address of Global Offset Table
#define DT_PPC_GOTSZ 0x70000002 //size of Global Offset Table
#define DT_PPC_PLTSZ 0x70000003 //size of Procedure Linkage Table
#define DT_LOPROC 0x70000000 //range of processor-defined tags
#define DT_HIPROC 0x7FFFFFFF

//Old dynamic tags.
//Readers should handle these, writers must use the above
#define DT_INIT_O 12 //address of initialization function
#define DT_FINI_O 13 //address of finialization function
#define DT_RPATH_O 15 //library search path
#define DT_SYMBOLIC_O 16 //affects dyn. linker's sym. resolution
#define DT_TEXTREL_O 22 //signal we might mod. a non-writable segment
#define DT_IT_O 24 //init and term types for a DLL.
#define DT_EXPORT_O 25 //address of export table
#define DT_EXPORTSZ_O 26 //size of export table
#define DT_IMPORT_O 27 //address of import table
#define DT_IMPORTSZ_O 28 //size of import table
#define DT_GOT_O 29 //address of Global Offset Table
#define DT_GOTSZ_O 30 //size of Global Offset Table
#define DT_PLTSZ_O 32 //size of Procedure Linkage Table
#define DT_ITPRTY_O 33 //relative priority of init and term to other functions
#define DT_LOUSER_O 0x60000000 //range of user-definable tags. will not
#define DT_HIUSER_O 0x6FFFFFFF //conflict with system-defined tags

class ELFDyn {
public:
    SWord d_tag;
    union {
        Addr d_val;
        Addr d_ptr;
    };
public:
    //Get the packed byte size of a dyn structure in 32bit or 64bit machine.
    static UINT getSize(ELFMgr const* mgr);

    void extract(BYTE const* buf, ELFMgr const* mgr);
    void insert(BYTE const* buf, ELFMgr const* mgr);
};

////////////////////////////////////////////////////////////////////////////////
//RELOCATION ENTRY                                                            //
////////////////////////////////////////////////////////////////////////////////
//Relocation is the process of connecting symbolic references with symbolic
//definitions. Relocatable files must have information that describes how to
//modify their section contents. Relocation entries ELFRel and ELFRela are
//these informatins.
class ELFRel {
public:
    //This member gives the location at which to apply the relocation.
    //For a relocatable file, the value is the byte offset from the beginning
    //of the section to the storage unit affected by the relocation.
    //For an executable file or a shared object, the value is the virtual
    //address of the storage unit affected by the relocation.
    Addr r_offset;

    //This member gives both the symbol table index with respect to which the
    //relocation must be made, and the type of relocation to apply.
    //For example, a call instruction's relocation entry would hold the symbol
    //table index of the function being called. If the index is STN_UNDEF,
    //the undefined symbol index, the relocation uses 0 as the 'symbol value'.
    //Relocation types are processor-specific; descriptions of their behavior
    //appear in the processor supplement. When the text below refers to a
    //relocation entry's relocation type or symbol table index, it means the
    //result of applying ELF32_R_TYPE (or ELF64_R_TYPE) or ELF32_R_SYM
    //(or ELF64_R_SYM), respectively, to the entry's r_info member.
    Word r_type;

    //The element index in linked symbol table.
    //Note each relocation entry corresponding to a symbol.
    Word r_sym;
public:
    //Get the packed byte size of a relocation structure in 32bit or 64bit
    //machine.
    static UINT getSize(ELFMgr const* mgr);
    void insert(BYTE const* buf, ELFMgr const* mgr);
    void extract(BYTE const* buf, ELFMgr const* mgr);
};

class ELFRela {
public:
    //This member gives the location at which to apply the relocation.
    //For a relocatable file, the value is the byte offset from the beginning
    //of the section to the storage unit affected by the relocation.
    //For an executable file or a shared object, the value is the virtual
    //address of the storage unit affected by the relocation.
    //--
    //In executable and shared object files, r_offset holds a virtual address.
    //To make these file's relocation entries more useful for the dynamic
    //linker, the section offset in file, gives way to a virtual
    //address (memory interpretation).
    //--
    //The typical application of an relocation is to determine the referenced
    //symbol value, extract the addend (either from the field to be relocated
    //or from the addend field contained in the relocation record, as
    //appropriate for the type of relocation record), apply the expression
    //implied by the relocation type to the symbol and addend, extract the
    //desired part of the expression result, and place it in the field to be
    //relocated.
    Addr r_offset;

    //This member gives both the symbol table index with respect to which the
    //relocation must be made, and the type of relocation to apply.
    //For example, a call instruction's relocation entry would hold the symbol
    //table index of the function being called. If the index is STN_UNDEF,
    //the undefined symbol index, the relocation uses 0 as the 'symbol value'.
    //Relocation types are processor-specific; descriptions of their behavior
    //appear in the processor supplement. When the text below refers to a
    //relocation entry's relocation type or symbol table index, it means the
    //result of applying ELF32_R_TYPE (or ELF64_R_TYPE) or ELF32_R_SYM
    //(or ELF64_R_SYM), respectively, to the entry's r_info member.
    Word r_type;

    //The element index in linked symbol table.
    //Note each relocation entry corresponding to a symbol.
    Word r_sym;

    //This member specifies a constant addend used to compute the value to be
    //stored into the relocatable field.
    SWord r_addend;
public:
    void extract(BYTE const* buf, ELFMgr const* mgr);

    //Get the bit size of the r_addend.
    //Type "SWord64" for elf64 and type "Addr32" for elf32.
    static UINT getAddendSize(ELFMgr const* mgr);

    //Get the align value of elements in ELFRela.
    //
    // Defined in "elf/elf64.h".          Defined in "elf/elf32.h".
    //   typedef struct {                   typedef struct {
    //       Addr64 r_offset;                   Addr32 r_offset;
    //       Word32 r_type;                     Word32 r_type:8;
    //       Word32 r_sym;                      Word32 r_sym:24;
    //       SWord64 r_addend;                  SWord32 r_addend;
    //   } ELFRela64;                       } ELFRela32;
    // Align with sizeof(Addr64).         Align with sizeof(Addr32).
    static UINT getAlign(ELFMgr const* mgr);

    //ELFRela has 4 members.
    static UINT getMemberNum() { return 4; }

    //Get the bit size of the r_offset.
    //Type "Addr64" for elf64 and type "SWord32" for elf32.
    static UINT getOffsetSize(ELFMgr const* mgr);

    //Get the packed byte size of a relocation structure in 32bit or 64bit
    //machine.
    static UINT getSize(ELFMgr const* mgr);

    //Get the bit size of the r_sym.
    //Type "Word32" for elf64 and type "Word32:24" for elf32.
    static UINT getSymbolSize(ELFMgr const* mgr);

    //Get the bit size of the r_type.
    //Type "Word32" for elf64 and type "Word32:8" for elf32.
    static UINT getTypeSize(ELFMgr const* mgr);

    void insert(BYTE const* buf, ELFMgr const* mgr);
};

////////////////////////////////////////////////////////////////////////////////
//Macro defines some specific name of sections for convenient purpose.        //
////////////////////////////////////////////////////////////////////////////////
#define ACOMMON_SECT_NAME ".acommon"
#define SCOMMON_SECT_NAME ".sbss"
#define TEXT_SECT_NAME ".text"
#define DATA_SECT_NAME ".rodata"
#define ENTRY_POINT_NAME "_start"
#define STACK_NAME "_stack"
#define EXCEPTION_SECTION_NAME ".exception"

} //namespace

#endif
