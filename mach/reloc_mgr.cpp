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
#include "../elf/elfinc.h"

namespace mach {

//
//START Var2Offset
//
TMWORD Var2Offset::getOrAddVarOffset(xoc::Var const* v)
{
    bool find = false;
    TMWORD off = get(v, &find);
    if (find) { return off; }

    m_cur_offset = (TMWORD)xcom::ceil_align(m_cur_offset, getAlign());
    off = m_cur_offset;
    set(v, off);
    m_cur_offset += v->getByteSize(m_tm);
    return off; 
}


void Var2Offset::dump(OUT StrBuf & buf) const
{
    Var2OffsetIter it;
    TMWORD off;
    xcom::StrBuf tmp(32);
    for (xoc::Var const* v = get_first(it, &off); !it.end();
         v = get_next(it, &off)) {
        v->dump(tmp, m_tm);
        buf.strcat("%s:OFFSET:0x%x", tmp.buf, (UINT)off);
    }
}


void Var2Offset::dump(OUT FileObj & fo) const
{
    xcom::StrBuf buf(32);
    dump(buf);
    fo.prt("%s", buf.buf);
}
//END Var2Offset


//
//START RelocMgr
//
RelocMgr::RelocMgr(Region * rg, MInstMgr * imgr, TMWORD align) :
    m_rg(rg), m_mimgr(imgr), m_code_align(align), m_data_align(align)
{
    m_tm = m_rg->getTypeMgr();
}


void RelocMgr::computeDataOffset(MOD MIList & milst,
                                 OUT Var2Offset & var2off,
                                 Label2Offset const& lab2off)
{
    MIListIter it;
    TMWORD offset = 0;
    for (MInst * mi = milst.get_head(&it);
         mi != nullptr; mi = milst.get_next(&it)) {
        if (mi->getCode() == MI_label) {
            continue;
        }
        if (mi->hasLab()) {
            ASSERT0(MI_lab(mi));
            TMWORD offset = lab2off.get(MI_lab(mi));
            setValueViaRelocType(mi, offset);
            continue;
        }
        if (mi->hasVar()) {
            ASSERT0(MI_var(mi));
            TMWORD offset = var2off.getOrAddVarOffset(MI_var(mi));
            setValueViaRelocType(mi, offset);
        }
        offset = (TMWORD)xcom::ceil_align(offset, getCodeAlign());
        MI_pc(mi) = offset;
        offset += mi->getWordBufLen();
    }
}


void RelocMgr::computeCodeOffset(MOD MIList & milst,
                                 OUT Label2Offset & lab2off)
{
    MIListIter it;
    UINT mic = 0;
    TMWORD offset = 0;
    for (MInst * mi = milst.get_head(&it);
         mi != nullptr; mi = milst.get_next(&it), mic++) {
        if (mi->getCode() == MI_label) {
            lab2off.set(MI_lab(mi), offset);
            continue;
        }
        offset = (TMWORD)xcom::ceil_align(offset, getCodeAlign());
        MI_pc(mi) = offset;
        offset += mi->getWordBufLen();
    }
}


void RelocMgr::perform(MOD MIList & milst)
{
    Var2Offset var2off(getDataAlign(), m_tm);
    Label2Offset lab2off;
    computeCodeOffset(milst, lab2off);
    computeDataOffset(milst, var2off, lab2off);
}
//END RelocMgr

} //namespace
