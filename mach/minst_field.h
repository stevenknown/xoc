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
#ifndef _MINST_FIELD_H_
#define _MINST_FIELD_H_

namespace mach {

class MField {
public:
    TMWORD value;
};


class MFieldDesc {
public:
    FIELD_TYPE field_type;
    CHAR const* name;
    UINT start;
    UINT end;
public:
    MFieldDesc(FIELD_TYPE ft, CHAR const* n, UINT s, UINT e) :
        field_type(ft), name(n), start(s), end(e) {}
};


class MFieldMgr {
protected:
    xcom::Vector<MFieldDesc> m_fieldtype_desc;
protected:
    //Check the layout of field-type description.
    bool checkFieldDesc();
public:
    MFieldMgr() {}
    virtual ~MFieldMgr() {}

    //Get the number of field descriptions.
    UINT getFieldDescNum() const { return m_fieldtype_desc.get_elem_count(); }

    //Get the field name.
    virtual CHAR const* getFieldName(FIELD_TYPE ft) const;

    //Get the field description.
    virtual MFieldDesc const& getFieldDesc(FIELD_TYPE ft) const;

    //Get the byte size of given field.
    virtual UINT getFieldSize(FIELD_TYPE ft) const;
};

} //namespace

#endif
