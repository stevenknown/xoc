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
#include "machinc.h"

namespace mach {

MIFlagDesc const g_miflag_desc[] = {
    { MI_FLAG_UNDEF, "undef", }, //idx0
    { MI_FLAG_HAS_LABEL, "haslab", }, //idx1
    { MI_FLAG_HAS_VAR, "hasvar", }, //idx2
};
static UINT g_miflag_num = sizeof(g_miflag_desc) / sizeof(g_miflag_desc[0]);

//
//START MIFlag
//
bool MIFlag::verify() const
{
    MIFlag::Iter it;
    for (MI_FLAG v = (MI_FLAG)get_first_flag(it); !end(it);
         v = (MI_FLAG)get_next_flag(it)) {
        ASSERT0_DUMMYUSE(v > MI_FLAG_UNDEF &&
                         MIFlagDesc::getDescIdx(v) < g_miflag_num);
    }
    return true;
}
//END MIFlag


//
//START MIFlagDesc
//
CHAR const* MIFlagDesc::getName(MI_FLAG flag)
{
    if (flag == MI_FLAG_UNDEF) { return g_miflag_desc[0].name; }
    MIFlag v(flag);
    xcom::ROBitSet bs((BYTE const*)&v.getFlagSet(), v.getFlagSetSize());
    ASSERT0(bs.get_elem_count() == 1);
    UINT idx = bs.get_first() + 1;
    ASSERT0(idx < g_miflag_num);
    return g_miflag_desc[idx].name;
}


UINT MIFlagDesc::getDescIdx(MI_FLAG flag)
{
    if (flag == MI_FLAG_UNDEF) { return 0; }
    MIFlag v(flag);
    xcom::ROBitSet bs((BYTE const*)&v.getFlagSet(), v.getFlagSetSize());
    ASSERT0(bs.get_elem_count() == 1);
    UINT idx = bs.get_first() + 1;
    ASSERT0(idx < g_miflag_num);
    return idx;
}


MI_FLAG MIFlagDesc::getFlag(UINT idx)
{
    ASSERT0(idx < g_miflag_num);
    return g_miflag_desc[idx].flag;
}
//END MIFlagDesc


//
//START MInst
//
void MInst::copyDbx(IR const* ir, DbxMgr * dbx_mgr)
{
    ASSERT0(ir);
    if (IR_ai(ir) == nullptr) { return; }
    DbxAttachInfo * da = (DbxAttachInfo*)IR_ai(ir)->get(AI_DBX);
    if (da == nullptr) { return; }
    MI_dbx(this).copy(da->dbx, dbx_mgr);
}


void MInst::dump(OUT xcom::StrBuf & buf, MInstMgr const& mgr) const
{
    ASSERT0(m_inst_desc);
    buf.strcat("\nMI_CODE:%s, PC:0x%x", mgr.getMInstName(this),
               (UINT)MI_pc(this));
    Dbx const* temp = &m_dbx;
    DbxMgr * dbx_mgr = mgr.getRegion()->getDbxMgr();
    ASSERT0(dbx_mgr);
    ASSERT0(temp);
    if (temp->getLine(LANG_CPP, dbx_mgr) != DBX_UNDEF) {
        buf.strcat(" file_index:%d, row: %d, col: %d, flag: %d",
            temp->getFileIndex(LANG_CPP, dbx_mgr),
            temp->getLine(LANG_CPP, dbx_mgr),
            temp->getColOffset(LANG_CPP, dbx_mgr),
            temp->getFlag(LANG_CPP, dbx_mgr));
    }
    for (UINT i = 0; i < m_inst_desc->getFieldNum(); i++) {
        buf.strcat("\n%s:0x%llx", m_inst_desc->getFieldName(i),
                   (ULONGLONG)getFieldValue(i));
    }
}


void MInst::dumpWordBuf(OUT StrBuf & strbuf) const
{
    xoc::ByteBuf::dump(strbuf, MI_wordbuf(this), MI_wordbuflen(this));
}


void MInst::dump(FileObj & fo, MInstMgr const& mgr) const
{
    xcom::StrBuf buf(32);
    dump(buf, mgr);
    dumpWordBuf(buf);
    fo.prt(buf.buf);
}


TMWORD MInst::getFieldValue(FIELD_TYPE ft) const
{
    return getFieldValue(getInstDesc()->getFieldIdx(ft));
}


TMWORD MInst::getFieldValue(UINT idx) const
{
    ASSERT0(m_inst_desc);
    ASSERT0(idx < m_inst_desc->getFieldNum());
    return m_field_vec[idx].value;
}


void MInst::setFieldValue(FIELD_TYPE ft, TMWORD val)
{
    ASSERT0(m_inst_desc);
    ASSERTN_DUMMYUSE(!xcom::isExceedBitWidth(
                        (ULONGLONG)val, m_inst_desc->getFieldSizeByFT(ft)),
                     ("val %u exceed bitwidth %u",
                      val, m_inst_desc->getFieldSizeByFT(ft)));
    UINT idx = m_inst_desc->getFieldIdx(ft);
    m_field_vec[idx].value = val;
}
//END MInst

} //namespace
