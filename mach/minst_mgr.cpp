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

//
//START MIList
//
void MIList::append_tail(MInst * mi)
{
    #ifdef _DEBUG_
    for (MInst * t = get_head(); t != nullptr; t = get_next()) {
        ASSERTN(t != mi, ("already in list."));
    }
    #endif
    List<MInst*>::append_tail(mi);
}


//Move elements in 'mis' to tail of current list.
void MIList::move_tail(MOD MIList & mis)
{
    #ifdef _DEBUG_
    for (MInst * mi = get_head(); mi != nullptr; mi = get_next()) {
        for (MInst * mi2 = mis.get_head();
             mi2 != nullptr; mi2 = mis.get_next()) {
            ASSERTN(mi != mi2, ("already in list."));
        }
    }
    #endif
    List<MInst*>::move_tail(mis);
}


void MIList::dump(LogMgr * lm, MInstMgr const& mgr) const
{
    StrBuf buf(32);
    xcom::FileObj fo(lm->getFileHandler());
    MIListIter it;
    for (MInst * mi = get_head(&it); mi != nullptr; mi = get_next(&it)) {
        buf.clean();
        mi->dump(buf, mgr);
        note(lm, "\n%s", buf.buf);
    }
    note(lm, "\n");
}
//END MIList


//
//START RecycMIList
//
//Lower version (<=4.6) gcc may claim RecycMIList is not a direct base of
//RecycMIList. Thus we avoid invoking constructor at initializing list in
//other constructor.
//e.g:RecycMIList::RecycMIList(IR2MI * ir2mi) :
//      RecycMIList(ir2or->getRecycMIListMgr())
void RecycMIList::init(RecycMIListMgr * mgr)
{
    m_mgr = mgr;
    m_entity = mgr->getFree();
    if (m_entity == nullptr) {
        m_entity = new MIList();
    }
}


RecycMIList::~RecycMIList()
{
    m_mgr->addFree(m_entity);
}
//END RecycMIList


//
//START RecycMIListMgr
//
RecycMIListMgr::~RecycMIListMgr()
{
    C<MIList*> * it;
    for (m_free_list.get_head(&it); it != nullptr; m_free_list.get_next(&it)) {
        delete it->val();
    }
}
//END RecycMIListMgr


//
//START MInstMgr
//
void * MInstMgr::xmalloc(UINT size)
{
    ASSERTN(m_pool != nullptr, ("pool does not initialized"));
    void * p = smpoolMalloc(size, m_pool);
    ASSERT0(p != nullptr);
    ::memset(p, 0, size);
    return p;
}
//END MInstMgr

} //namespace
