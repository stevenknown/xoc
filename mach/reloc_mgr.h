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

author: Su Zhenyu
@*/
#ifndef _RELOC_H_
#define _RELOC_H_

namespace mach {

typedef xcom::TMap<xoc::LabelInfo const*, TMWORD> Label2Offset;

typedef xcom::TMapIter<xoc::Var const*, TMWORD> Var2OffsetIter;

class Var2Offset : public xcom::TMap<xoc::Var const*, TMWORD> {
    COPY_CONSTRUCTOR(Var2Offset);
protected:
    TMWORD m_cur_offset;
    TMWORD m_align;
    xoc::TypeMgr const* m_tm;
public:
    explicit Var2Offset(TMWORD align, xoc::TypeMgr const* tm) :
        m_cur_offset(0), m_align(align), m_tm(tm)
    {}

    void dump(OUT StrBuf & buf) const;
    void dump(OUT FileObj & fo) const;

    //The function try to retrieve 'v' in the variable layout, return the offset
    //of 'v' if find. Otherwise, the function will compute the layout of 'v'
    //and add 'v' to current variable table.
    TMWORD getOrAddVarOffset(xoc::Var const* v);
    TMWORD getAlign() const { return m_align; }

    //Set the alignment.
    //The byte offset of each Variable will aligned in 'align'.
    void setAlign(TMWORD align) { m_align = align; }
};


class RelocMgr {
    COPY_CONSTRUCTOR(RelocMgr);
protected:
    Region * m_rg;
    TypeMgr * m_tm;
    MInstMgr * m_mimgr;
    TMWORD m_code_align;
    TMWORD m_data_align;
protected:
    void computeCodeOffset(MOD MIList & milst,
                           OUT Label2Offset & lab2off);
    void computeDataOffset(MOD MIList & milst,
                           OUT Var2Offset & var2off,
                           Label2Offset const& lab2off);
public:
    RelocMgr(Region * rg, MInstMgr * imgr, TMWORD align);
    virtual ~RelocMgr() {}
    MInstMgr * getMIMgr() const { return m_mimgr; }
    TMWORD getCodeAlign() const { return m_code_align; }
    TMWORD getDataAlign() const { return m_data_align; }
    virtual UINT getMInstRelocType(MI_CODE c) = 0;

    //Set the data or code byte offset according to relocation type.
    virtual void setValueViaRelocType(OUT MInst * mi, TMWORD offset) = 0;

    //Set the code alignment.
    //The byte offset of each Machine Instruction will aligned in 'align'.
    void setCodeAlign(TMWORD align) { m_code_align = align; }

    //Set the data alignment.
    //The byte offset of each variable will aligned in 'align'.
    void setDataAlign(TMWORD align) { m_data_align = align; }

    virtual void perform(MOD MIList & milst);
};

} //namespace

#endif
