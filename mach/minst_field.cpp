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

//Check the layout of field-type description.
bool MFieldMgr::checkFieldDesc()
{
    for (UINT i = 0; i < getFieldDescNum(); i++) {
        ASSERT0(m_fieldtype_desc[i].field_type == i);
        ASSERT0(m_fieldtype_desc[i].end >= m_fieldtype_desc[i].start);
    }
    return true;
}


CHAR const* MFieldMgr::getFieldName(FIELD_TYPE ft) const
{
    ASSERT0((UINT)ft < getFieldDescNum());
    return m_fieldtype_desc[ft].name;
}


MFieldDesc const& MFieldMgr::getFieldDesc(FIELD_TYPE ft) const
{
    ASSERT0((UINT)ft < getFieldDescNum());
    return const_cast<MFieldMgr*>(this)->m_fieldtype_desc.get_vec()[ft];
}


UINT MFieldMgr::getFieldSize(FIELD_TYPE ft) const
{
    ASSERT0((UINT)ft < getFieldDescNum());
    return m_fieldtype_desc[ft].end - m_fieldtype_desc[ft].start + 1;
}

} //namespace
