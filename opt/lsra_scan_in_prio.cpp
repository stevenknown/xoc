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

class PrioQuickSort : public xcom::QuickSort<LifeTime*> {
protected:
    virtual LifeTime * _max(LifeTime * a, LifeTime * b) const
    { return a->getPriority() > b->getPriority() ? a : b; }

    virtual LifeTime * _min(LifeTime * a, LifeTime * b) const
    { return a->getPriority() < b->getPriority() ? a : b; }

    virtual bool GreatThan(LifeTime * a, LifeTime * b) const
    { return a->getPriority() > b->getPriority(); }

    virtual bool LessThan(LifeTime * a, LifeTime * b) const
    { return a->getPriority() < b->getPriority(); }
};


static void sortInDescreasedPrioOrder(LTList const& org, OUT LTList & res)
{
    Vector<LifeTime*> vec;
    LTListIter it;
    for (LifeTime * lt = org.get_head(&it);
         lt != nullptr; lt = org.get_next(&it)) {
        vec.append(lt);
    }
    PrioQuickSort sort;
    sort.sort(vec);
    for (VecIdx i = 0; i < (VecIdx)vec.get_elem_count(); i++) {
        res.append_head(vec.get(i));
    }
}


//Note lifetimes in 'lst' should have been sorted in decreased order.
static void insertLTInDescreasedPrioOrder(LifeTime * newlt, MOD LTList & lst)
{
    LTListIter it;
    for (LifeTime * lt = lst.get_head(&it);
         lt != nullptr; lt = lst.get_next(&it)) {
        if (newlt->getPriority() > lt->getPriority()) {
            lst.insert_before(newlt, it);
            return;
        }
    }
    lst.append_tail(newlt);
}


void ScanInPrio::perform()
{
    DUMMYUSE(insertLTInDescreasedPrioOrder);
    LTList sorted;
    sortInDescreasedPrioOrder(m_impl.getLTMgr().getLTList(), sorted);
    LTIG ig(m_rg, m_impl.getLTMgr());
    ig.build();
}

} //namespace xoc
