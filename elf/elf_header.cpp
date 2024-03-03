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

namespace elf {

static BYTE g_magic[EI_MAG_NUM] = { EI_MAG_HEAD, 'E', 'L', 'F' };

//START ELFHdr
UINT ELFHdr::getSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(ELFHdr64) : sizeof(ELFHdr32);
}


void ELFHdr::setMagic()
{
    ::memcpy(e_ident, &g_magic, EI_MAG_NUM);
}


BYTE const (*ELFHdr::getIdent() const) [EI_MAG_NUM]
{
    return &e_ident;
}


bool ELFHdr::isELF() const
{
    return ::memcmp(g_magic, e_ident, EI_MAG_NUM) == 0;
}


void ELFHdr::insert(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFHdr64 * hdr = (ELFHdr64*)buf;
        ::memcpy(&hdr->e_ident, &e_ident, EI_MAG_NUM);
        hdr->e_class = e_class;
        hdr->e_class =e_class;
        hdr->e_data = e_data;
        hdr->e_hversion = e_hversion;
        for (UINT i = 0; i < E_PAD_SIZE; i++) {
            hdr->e_pad[i] = E_PAD_ZERO;
        }
        hdr->e_type = e_type;
        hdr->e_machine = e_machine;
        hdr->e_version = e_version;
        hdr->e_entry = e_entry;
        hdr->e_phoff = e_phoff;
        hdr->e_shoff = e_shoff;
        hdr->e_flags = e_flags;
        hdr->e_ehsize = e_ehsize;
        hdr->e_phensize = e_phensize;
        hdr->e_phnum = e_phnum;
        hdr->e_shensize = e_shensize;
        hdr->e_shnum = e_shnum;
        hdr->e_shstrndx = e_shstrndx;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFHdr32 * hdr = (ELFHdr32*)buf;
    ::memcpy(&hdr->e_ident, &e_ident, EI_MAG_NUM);
    hdr->e_class = e_class;
    hdr->e_class = e_class;
    hdr->e_data = e_data;
    hdr->e_hversion = e_hversion;
    hdr->e_type = e_type;
    hdr->e_machine = e_machine;
    hdr->e_version = e_version;
    hdr->e_entry = (Addr32)e_entry;
    hdr->e_phoff = (Off32)e_phoff;
    hdr->e_shoff = (Off32)e_shoff;
    hdr->e_flags = e_flags;
    hdr->e_ehsize = e_ehsize;
    hdr->e_phensize = e_phensize;
    hdr->e_phnum = e_phnum;
    hdr->e_shensize = e_shensize;
    hdr->e_shnum = e_shnum;
    hdr->e_shstrndx = e_shstrndx;
}


void ELFHdr::extract(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFHdr64 * hdr = (ELFHdr64*)buf;
        ::memcpy(&e_ident, &hdr->e_ident, EI_MAG_NUM);
        e_class = hdr->e_class;
        e_class = hdr->e_class;
        e_data = hdr->e_data;
        e_hversion = hdr->e_hversion;
        e_type = hdr->e_type;
        e_machine = hdr->e_machine;
        e_version = hdr->e_version;
        e_entry = hdr->e_entry;
        e_phoff = hdr->e_phoff;
        e_shoff = hdr->e_shoff;
        e_flags = hdr->e_flags;
        e_ehsize = hdr->e_ehsize;
        e_phensize = hdr->e_phensize;
        e_phnum = hdr->e_phnum;
        e_shensize = hdr->e_shensize;
        e_shnum = hdr->e_shnum;
        e_shstrndx = hdr->e_shstrndx;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFHdr32 * hdr = (ELFHdr32*)buf;
    ::memcpy(&e_ident, &hdr->e_ident, EI_MAG_NUM);
    e_class = hdr->e_class;
    e_class = hdr->e_class;
    e_data = hdr->e_data;
    e_hversion = hdr->e_hversion;
    e_type = hdr->e_type;
    e_machine = hdr->e_machine;
    e_version = hdr->e_version;
    e_entry = hdr->e_entry;
    e_phoff = hdr->e_phoff;
    e_shoff = hdr->e_shoff;
    e_flags = hdr->e_flags;
    e_ehsize = hdr->e_ehsize;
    e_phensize = hdr->e_phensize;
    e_phnum = hdr->e_phnum;
    e_shensize = hdr->e_shensize;
    e_shnum = hdr->e_shnum;
    e_shstrndx = hdr->e_shstrndx;
}


//START ELFPHdr
UINT ELFPHdr::getMachBitWidth(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(ELFPHdr64) : sizeof(ELFPHdr32);
}


void ELFPHdr::insert(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFPHdr64 * p = (ELFPHdr64*)buf;
        p->p_type = p_type;
        p->p_offset = p_offset;
        p->p_vaddr = p_vaddr;
        p->p_paddr = p_paddr;
        p->p_filesz = p_filesz;
        p->p_memsz = p_memsz;
        p->p_flags = p_flags;
        p->p_align = p_align;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFPHdr32 * p = (ELFPHdr32*)buf;
    p->p_type = p_type;
    p->p_offset = (Off32)p_offset;
    p->p_vaddr = (Addr32)p_vaddr;
    p->p_paddr = (Addr32)p_paddr;
    p->p_filesz = p_filesz;
    p->p_memsz = p_memsz;
    p->p_flags = p_flags;
    p->p_align = p_align;
}


void ELFPHdr::extract(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFPHdr64 * p = (ELFPHdr64*)buf;
        p_type = p->p_type;
        p_offset = p->p_offset;
        p_vaddr = p->p_vaddr;
        p_paddr = p->p_paddr;
        p_filesz = (Word32)p->p_filesz;
        p_memsz = (Word32)p->p_memsz;
        p_flags = p->p_flags;
        p_align = (Word32)p->p_align;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFPHdr32 * p = (ELFPHdr32*)buf;
    p_type = p->p_type;
    p_offset = p->p_offset;
    p_vaddr = p->p_vaddr;
    p_paddr = p->p_paddr;
    p_filesz = p->p_filesz;
    p_memsz = p->p_memsz;
    p_flags = p->p_flags;
    p_align = p->p_align;
}


//START ELFSHdr
UINT ELFSHdr::getSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(ELFSHdr64) : sizeof(ELFSHdr32);
}


ELFSHdr * ELFSHdr::getRelatedSymTab(ELFMgr const* mgr) const
{
    ASSERTN(s_link != 0, ("no linked table"));
    ELFSHdr * tab = mgr->getSectHeader(s_link);
    ASSERT0(tab->isSymTab());
    return tab;
}


ELFSHdr * ELFSHdr::getRelatedStrTab(ELFMgr const* mgr) const
{
    //ASSERTN(s_link != 0, ("no linked table"));
    //CASE:sometimes, there is no string table corresponding to symbol table.
    //All symbols are corresponding to a section name.
    if (s_link == 0) { return nullptr; }
    ELFSHdr * tab = mgr->getSectHeader(s_link);
    ASSERT0(tab->isStrTab());
    return tab;
}


void ELFSHdr::insert(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFSHdr64 * p = (ELFSHdr64*)buf;
        p->s_name = s_name;
        p->s_type = s_type;
        p->s_flags = s_flags;
        p->s_addr = s_addr;
        p->s_offset = s_offset;
        p->s_size = s_size;
        p->s_link = s_link;
        p->s_info = s_info;
        p->s_addr_align = s_addr_align;
        p->s_entry_size = s_entry_size;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFSHdr32 * p = (ELFSHdr32*)buf;
    p->s_name = s_name;
    p->s_type = s_type;
    p->s_flags = (Addr32)s_flags;
    p->s_addr = (Addr32)s_addr;
    p->s_offset = (Off32)s_offset;
    p->s_size = (Addr32)s_size;
    p->s_link = (Word32)s_link;
    p->s_info = (Word32)s_info;
    p->s_addr_align = (Addr32)s_addr_align;
    p->s_entry_size = (Addr32)s_entry_size;
}


void ELFSHdr::extract(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFSHdr64 * p = (ELFSHdr64*)buf;
        s_name = p->s_name;
        s_type = p->s_type;
        s_flags = p->s_flags;
        s_addr = p->s_addr;
        s_offset = p->s_offset;
        s_size = p->s_size;
        s_link = p->s_link;
        s_info = p->s_info;
        s_addr_align = p->s_addr_align;
        s_entry_size = p->s_entry_size;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFSHdr32 * p = (ELFSHdr32*)buf;
    s_name = p->s_name;
    s_type = p->s_type;
    s_flags = p->s_flags;
    s_addr = p->s_addr;
    s_offset = p->s_offset;
    s_size = p->s_size;
    s_link = p->s_link;
    s_info = p->s_info;
    s_addr_align = p->s_addr_align;
    s_entry_size = p->s_entry_size;
}


//START ELFSym
UINT ELFSym::getSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(ELFSym64) : sizeof(ELFSym32);
}


UINT ELFSym::getAlign(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(Addr64) : sizeof(Word32);
}


void ELFSym::extract(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFSym64 * p = (ELFSym64*)buf;
        st_name = p->st_name;
        st_value = p->st_value;
        st_size = p->st_size;
        st_type = p->st_type;
        st_bind = p->st_bind;
        st_other = p->st_other;
        st_shndx = p->st_shndx;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFSym32 * p = (ELFSym32*)buf;
    st_name = p->st_name;
    st_value = p->st_value;
    st_size = p->st_size;
    st_type = p->st_type;
    st_bind = p->st_bind;
    st_other = p->st_other;
    st_shndx = p->st_shndx;
}


void ELFSym::insert(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFSym64 * p = (ELFSym64*)buf;
        p->st_name = (Word32)st_name;
        p->st_value = st_value;
        p->st_size = st_size;
        p->st_type = st_type;
        p->st_bind = st_bind;
        p->st_other = st_other;
        p->st_shndx = st_shndx;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFSym32 * p = (ELFSym32*)buf;
    p->st_name = (Word32)st_name;
    p->st_value = (Addr32)st_value;
    p->st_size = (Word32)st_size;
    p->st_type = st_type;
    p->st_bind = st_bind;
    p->st_other = st_other;
    p->st_shndx = st_shndx;
}


//START ELFDyn
UINT ELFDyn::getSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(ELFDyn64) : sizeof(ELFDyn32);
}


void ELFDyn::extract(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFDyn64 * p = (ELFDyn64*)buf;
        d_tag = p->d_tag;
        d_val = p->d_un.d_val;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFDyn32 * p = (ELFDyn32*)buf;
    d_tag = p->d_tag;
    d_val = p->d_un.d_val;
}


//START ELFRel
UINT ELFRel::getSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(ELFRel64) : sizeof(ELFRel32);
}


void ELFRel::extract(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFRel64 * p = (ELFRel64*)buf;
        r_offset = p->r_offset;
        r_type = p->r_type;
        r_sym = p->r_sym;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFRel32 * p = (ELFRel32*)buf;
    r_offset = p->r_offset;
    r_type = p->r_type;
    r_sym = p->r_sym;
}


void ELFRel::insert(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFRel64 * p = (ELFRel64*)buf;
        p->r_offset = r_offset;
        p->r_type = (Word32)r_type;
        p->r_sym = (Word32)r_sym;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFRel32 * p = (ELFRel32*)buf;
    p->r_offset = (Addr32)r_offset;
    p->r_sym = r_sym;
}


//START ELFRela
UINT ELFRela::getSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(ELFRela64) : sizeof(ELFRela32);
}


UINT ELFRela::getAlign(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(Addr64) : sizeof(Addr32);
}


UINT ELFRela::getAddendSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(SWord64) * BITS_PER_BYTE :
        sizeof(SWord32) * BITS_PER_BYTE;
}


UINT ELFRela::getOffsetSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    return mgr->is64bit() ? sizeof(Addr64) * BITS_PER_BYTE :
        sizeof(Addr32) * BITS_PER_BYTE;
}


UINT ELFRela::getSymbolSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    UINT const sym_size_32 = 24; //Word32 r_sym:24;
    return mgr->is64bit() ? sizeof(Word32) * BITS_PER_BYTE : sym_size_32;
}


UINT ELFRela::getTypeSize(ELFMgr const* mgr)
{
    ASSERT0(mgr->is64bit() || mgr->is32bit());
    UINT const type_size_32 = 8; //Word32 r_type:8;
    return mgr->is64bit() ? sizeof(Word32) * BITS_PER_BYTE : type_size_32;
}


void ELFRela::extract(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFRela64 * p = (ELFRela64*)buf;
        r_offset = p->r_offset;
        r_type = p->r_type;
        r_sym = p->r_sym;
        r_addend = p->r_addend;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFRela32 * p = (ELFRela32*)buf;
    r_offset = p->r_offset;
    r_type = p->r_type;
    r_sym = p->r_sym;
    r_addend = p->r_addend;
}


void ELFRela::insert(BYTE const* buf, ELFMgr const* mgr)
{
    if (mgr->is64bit()) {
        ELFRela64 * p = (ELFRela64*)buf;
        p->r_offset = r_offset;
        p->r_type = (Word32)r_type;
        p->r_sym = (Word32)r_sym;
        p->r_addend = r_addend;
        return;
    }
    ASSERT0(mgr->is32bit());
    ELFRela32 * p = (ELFRela32*)buf;
    p->r_offset = (Addr32)r_offset;
    p->r_type = r_type;
    p->r_sym = r_sym;
    p->r_addend = (SWord32)r_addend;
}

} //namespace elf
