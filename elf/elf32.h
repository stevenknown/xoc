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
#ifndef _ELF32_H_
#define _ELF32_H_

#pragma pack(1)

//ELF 32-bit
//Name    Size Alignment Purpose
//Addr    4    4         Unsigned program address
//Half    2    2         Unsigned medium integer
//Off     4    4         Unsigned file offset
//Sword32 4    4         Signed large integer
//Word32  4    4         Unsigned large integer
//UCHAR   1    1         Unsigned small integer
typedef UINT32 Addr32;
typedef UINT32 Off32;

//ELF Header
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
    Word32 e_version; //Object file version
    Addr32 e_entry; //Entry point
    Off32 e_phoff; //Program header table file offset from the start of the file
    Off32 e_shoff; //Section header table file offset from the start of the file
    Word32 e_flags; //Processor-specific flags
    Half e_ehsize; //ELF header size in bytes
    Half e_phensize; //Program header table entry size
    Half e_phnum;   //Program header table entry count
    Half e_shensize; //Section header table entry size
    Half e_shnum; //Section header table entry count
    Half e_shstrndx; //Section header string table index
} ELFHdr32;

//Program header
typedef struct {
    Word32 p_type; //Segment type
    Off32 p_offset; //Segment file offset
    Addr32 p_vaddr; //Segment virtual address
    Addr32 p_paddr; //Segment physical address
    Word32 p_filesz; //Segment size in file
    Word32 p_memsz; //Segment size in memory
    Word32 p_flags; //Segment flags
    Word32 p_align; //Segment alignment
} ELFPHdr32;

//Section table entry
typedef struct {
    Word32 s_name; //name of the section
    Word32 s_type; //section type
    Addr32 s_flags;
    Addr32 s_addr; //starting address of section in image
    Off32 s_offset; //start of section in file
    Addr32 s_size; //size of section in file.
    Word32 s_link; //multipurpose field   (based on type)

    //Another multipurpose field (based on type)
    //in .rel section, it give the relatively section
    //which should be relocated.
    Word32 s_info;
    Addr32 s_addr_align; //address alignment
    Addr32 s_entry_size; //entry size for sects with fixed sized entries
} ELFSHdr32;

//Symbol table entrty
typedef struct {
    Word32 st_name;
    union {
        //NOTE: 'st_value' give the symbol absolute position, while the
        //'st_sect' is 'ABS', namely absolutely in executable image or
        //anything else, and the value will not be changed at any time.
        //If 'st_sect' is 'COMMON', then the 'st_align' give
        //the required alignment granularity, and 'st_size' gives the minimum
        //size, namely, the precisely object or function body length.
        Addr32 st_value;
        Addr32 st_align;
    };
    Word32 st_size; //symbol size

    //This member specifies the symbol's type and binding attributes.
    //A list of the values and meanings appears below.
    //The following codeshows how to manipulate the values for
    //both 32 and 64-bit objects.
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
} ELFSym32;

typedef struct {
	SWord32 d_tag;
   	union {
   		Word32 d_val;
   		Addr32 d_ptr;
	} d_un;
} ELFDyn32;

typedef struct {
    Addr32 r_offset;
    Word32 r_type:8; //describ the relocation type
    Word32 r_sym:24; //describ the symbol index
} ELFRel32;

typedef struct {
    Addr32 r_offset;
    Word32 r_type:8; //describ the relocation type
    Word32 r_sym:24; //describ the symbol index
    SWord32 r_addend;
} ELFRela32;

#pragma pack()

#endif
