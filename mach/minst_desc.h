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
#ifndef _MINST_DESC_H_
#define _MINST_DESC_H_

namespace mach {

class MInstDesc {
public:
    MInstMgr const& m_inst_mgr;
    xcom::Vector<FIELD_TYPE> m_field_type;
    UINT m_field_idx[FT_NUM];
public:
    MInstDesc(MInstMgr const& im) : m_inst_mgr(im)
    {
        ASSERT0(FT_NUM < MAX_FT_NUM);
        ::memset((void*)m_field_idx, 0, sizeof(m_field_idx));
    }
    virtual ~MInstDesc() {}

    //Perform initialization for target depdendent code.
    void initFieldIdx()
    {
        for (UINT i = 0; i < getFieldNum(); i++) {
            m_field_idx[m_field_type[i]] = i;
        }
    }

    //Return true if current machine instruction contains field 'ft'.
    virtual bool isContainField(FIELD_TYPE ft) const
    {
        for (UINT i = 0; i < getFieldNum(); i++) {
            if (m_field_type[i] == ft) { return true; }
        }
        return false;
    }

    //Return the index of given field type of curent machine instruction.
    //Note the function will diagnose whether current machine instruction
    //contains given 'ft'.
    //Index start at 0.
    //ft: field type that is ocntained in current machine instruction.
    //e.g: given fieldtype is { FT_A, FT_B, FT_C }, if ft is FT_B, return 1.
    UINT getFieldIdx(FIELD_TYPE ft) const;

    //Return the field type of curent machine instruction.
    //idx: the index of field in current machine instruction.
    //     Note index start at 0.
    //e.g: given fieldtype is { FT_A, FT_B, FT_C }, if idx is 2, return FT_C.
    FIELD_TYPE getFieldType(UINT idx) const
    {
        ASSERT0(idx < getFieldNum());
        return m_field_type[idx];
    }

    //Return the field name of curent machine instruction.
    //idx: the index of field in current machine instruction.
    //     Note index start at 0.
    //e.g: given fieldtype is { FT_A, FT_B, FT_C }, if idx is 2, return "C".
    CHAR const* getFieldName(UINT idx) const;

    //Return the number of fields of current machine instruction.
    //e.g: given fieldtype is { FT_A, FT_B, FT_C }, return 3.
    UINT getFieldNum() const { return m_field_type.get_elem_count(); }

    //Return the bit width for given field type of current machine instruction.
    //ft: field type that is ocntained in current machine instruction.
    //e.g: given fieldtype is { FT_A:1, FT_B:2, FT_C:3 }, if ft is FT_C,
    //     return 3.
    UINT getFieldSizeByFT(FIELD_TYPE ft) const;

    //Return the bit width for given field type indexed by 'idx' of current
    //machine instruction.
    //idx: the index of field in current machine instruction.
    //     Note index start at 0.
    //e.g: given fieldtype is { FT_A:11, FT_B:12, FT_C:13 }, if idx is 1,
    //     return 12.
    UINT getFieldSizeByIdx(UINT idx) const;
};


class LabelInstDesc : public mach::MInstDesc {
public:
    LabelInstDesc(mach::MInstMgr const& im) : MInstDesc(im) {}
};


class MemAccInstDesc : public mach::MInstDesc {
public:
    MemAccInstDesc(mach::MInstMgr const& im) : MInstDesc(im) {}
};

class DwarfCFIInstDesc : public mach::MInstDesc {
public:
    DwarfCFIInstDesc(mach::MInstMgr const& im) : MInstDesc(im) {}
};

} //namespace

#endif
