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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

#ifdef _DEBUG_
//NOTE: the variable is only used in DEBUG mode to facilitate user
//to debug and trace all reported actions.
//The variable is NOT thread-safe.
static UINT g_global_cnt = ACT_HANDLER_ID_UNDEF + 1;
#endif

//
//START ActHandler
//
ActHandler::ActHandler(UINT id, xcom::StrBuf * buf)
    : m_id(id), info(buf)
{
    ASSERT0(m_id != ACT_HANDLER_ID_UNDEF && buf != nullptr);
    #ifdef _DEBUG_
    m_gid = g_global_cnt;
    #endif
}
//END ActHandler


//
//START ActMgr
//
void ActMgr::clean()
{
    for (xcom::StrBuf * buf = m_act_list.get_head();
         buf != nullptr; buf = m_act_list.get_next()) {
        delete buf;
    }
    m_act_list.clean();
    m_cnt = ACT_HANDLER_ID_UNDEF + 1;
}


ActHandler ActMgr::dump_args(CHAR const* format, va_list args)
{
    if (!m_rg->isLogMgrInit()) { return ActHandler(); }
    xcom::StrBuf * buf = allocStrBuf();

    //Dump action id.
    buf->strcat("ACT%u:", m_cnt);
    #ifdef _DEBUG_
    buf->strcat("GID%u:", g_global_cnt);
    #endif

    //Update action id.
    m_cnt++;
    #ifdef _DEBUG_
    g_global_cnt++;
    #endif

    //Append into action list.
    va_list targs;
    va_copy(targs, args);
    buf->vstrcat(format, targs);
    va_end(targs);
    return ActHandler(m_cnt, buf);
}


ActHandler ActMgr::dump(CHAR const* format, ...)
{
    if (!m_rg->isLogMgrInit()) { return ActHandler(); }
    va_list args;
    va_start(args, format);
    ActHandler ach = dump_args(format, args);
    va_end(args);
    return ach;
}


void ActMgr::dump() const
{
    if (m_act_list.get_elem_count() == 0) { return; }
    note(m_rg, "\n==-- DUMP ALL ACT --==");
    m_rg->getLogMgr()->incIndent(2);
    xcom::List<xcom::StrBuf*>::Iter it;
    for (xcom::StrBuf * buf = m_act_list.get_head(&it);
         buf != nullptr; buf = m_act_list.get_next(&it)) {
        note(m_rg, "\n%s", buf->buf);
    }
    m_rg->getLogMgr()->decIndent(2);
}
//END ActMgr

} //namespace xoc
