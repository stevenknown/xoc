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
#ifndef _ACT_MGR_H_
#define _ACT_MGR_H_

namespace xoc {

#define ACT_HANDLER_ID_UNDEF 0

class ActHandler {
    //The class permits copy-constructor.
    UINT m_id;
    #ifdef _DEBUG_
    //NOTE: the variable is only used in DEBUG mode to facilitate user
    //to debug and trace all reported actions.
    UINT m_gid;
    #endif
public:
    xcom::StrBuf * info;
public:
    ActHandler() : m_id(ACT_HANDLER_ID_UNDEF), info(nullptr) {}
    ActHandler(UINT id, xcom::StrBuf * buf);

    UINT id() const { return m_id; }
};


//The class records actions that Passes or Modules performed.
//It is often used to inform user what behaviours have optimizations done,
//and why are some optimizations not performed.
class ActMgr {
protected:
    UINT m_cnt;
    Region const* m_rg;
    xcom::List<xcom::StrBuf*> m_act_list;
protected:
    xcom::StrBuf * allocStrBuf()
    {
        xcom::StrBuf * buf = new xcom::StrBuf(64);
        m_act_list.append_tail(buf);
        return buf;
    }
public:
    ActMgr(Region const* rg) : m_rg(rg) { m_cnt = ACT_HANDLER_ID_UNDEF + 1; }
    ~ActMgr() { clean(); }
    void clean();

    //Dump specific actions.
    ActHandler dump(CHAR const* format, ...);
    ActHandler dump_args(CHAR const* format, va_list args);

    //Dump all actions.
    void dump() const;

    //Return the number of actions.
    UINT getActNum() const { return m_act_list.get_elem_count(); }

    Region const* getRegion() const { return m_rg; }
};

} //namespace xoc
#endif
