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
#ifndef _MINST_H_
#define _MINST_H_

namespace mach {

class RecycMIList;
class RecycMIListMgr;
class MInstDesc;
class MInstMgr;

typedef enum {
    MI_FLAG_UNDEF = 0,
    MI_FLAG_HAS_LABEL = 0x1,
    MI_FLAG_HAS_VAR = 0x2,
} MI_FLAG;


class MIFlag : public UFlag {
public:
    MIFlag(UINT v) : UFlag(v) {}
    bool verify() const;
};


class MIFlagDesc {
public:
    ////////////////////////////////////////////////////////////////////
    //NOTE: DO NOT CHANGE THE LAYOUT OF CLASS MEMBERS BECAUSE THEY ARE//
    //CORRESPONDING TO THE SPECIAL INITIALIZING VALUE.                //
    ////////////////////////////////////////////////////////////////////
    MI_FLAG flag;
    CHAR const* name;
public:
    //Get flag's name.
    static CHAR const* getName(MI_FLAG flag);

    //Compute the index of 'flag' in the Desc table.
    static UINT getDescIdx(MI_FLAG flag);

    //Get the flag by given index in enum definition.
    static MI_FLAG getFlag(UINT idx);
};


#define MI_dbx(mi) ((mi)->m_dbx)
#define MI_code(mi) ((mi)->m_code)
#define MI_desc(mi) ((mi)->m_inst_desc)
#define MI_field_vec(mi) ((mi)->m_field_vec)
#define MI_flag(mi) ((mi)->m_flag)

//Record the assembled binary mahcine word.
#define MI_wordbuf(mi) ((mi)->m_word_buf)
#define MI_wordbuflen(mi) ((mi)->m_word_buf_len)
#define MI_pc(mi) ((mi)->m_pc)
class MInst {
    COPY_CONSTRUCTOR(MInst);
public:
    MI_CODE m_code;
    UINT m_word_buf_len;
    TMWORD m_pc;
    MIFlag m_flag;
    MInstDesc const* m_inst_desc;
    BYTE * m_word_buf;
    MField * m_field_vec;
    Dbx m_dbx;
public:
    MInst() : m_flag(0) {}

    void copyDbx(Dbx const& dbx) { MI_dbx(this).copy(dbx); }

    void dump(OUT xcom::StrBuf & buf, MInstMgr const& mgr) const;
    void dump(FileObj & fo, MInstMgr const& mgr) const;
    void dumpWordBuf(OUT StrBuf & strbuf) const;
    void dumpWordBuf(FileObj & fo) const;

    //Return the value of curent machine instruction.
    //idx: the index of field in current machine instruction.
    //e.g: given fieldtype is { FT_A:0x10, FT_B:0x20, FT_C:0x30 },
    //     if idx is 2, return 0x30.
    TMWORD getFieldValue(UINT idx) const;
    TMWORD getFieldValue(FIELD_TYPE ft) const;

    //Return the abstract code descriptor.
    MI_CODE getCode() const { return m_code; }

    MInstDesc const* getInstDesc() const { return m_inst_desc; }
    BYTE * getWordBuf() const { return m_word_buf; }
    UINT getWordBufLen() const { return m_word_buf_len; }
    TMWORD getPC() const { return m_pc; }

    //Return true if the machine instruction corresponding to Label Info.
    bool hasLab() const { return MI_flag(this).have(MI_FLAG_HAS_LABEL); }

    //Return true if the machine instruction corresponding to Variable Info.
    bool hasVar() const { return MI_flag(this).have(MI_FLAG_HAS_VAR); }

    void removeFlag(MI_FLAG f) {MI_flag(this).remove(f); }

    void setFlag(MI_FLAG f) { MI_flag(this).set(f); }
    void setFieldValue(FIELD_TYPE ft, TMWORD val);
};

} //namespace

#endif
