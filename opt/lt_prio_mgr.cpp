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
#include "targinfo_mgr.h"
#include "lifetime.h"
#include "lt_interf_graph.h"
#include "linear_scan.h"
#include "lsra_impl.h"
#include "lsra_scan_in_pos.h"
#include "lt_prio_mgr.h"
#include "lsra_scan_in_prio.h"

namespace xoc {

double LTPriorityMgr::computePriority(LifeTime const* lt) const
{
    LI<IRBB> const* liroot = m_cfg->getLoopInfo();
    OccListIter it = nullptr;
    double prio = 0.0;
    OccList const& occlst = const_cast<LifeTime*>(lt)->getOccList();
    UINT count = 0;
    for (Occ occ  = occlst.get_head(&it); it != occlst.end();
         occ = occlst.get_next(&it)) {
        count++;
        double tprio = 1.0;
        ASSERTN(occ.getIR() && !occ.getIR()->is_undef(), ("ilegal occ"));
        IRBB const* occbb = occ.getBB();
        ASSERT0(occbb);
        UINT nestlevel = 0;
        if (liroot != nullptr &&
            liroot->isInsideLoopTree(occbb->id(), nestlevel, true)) {
            ASSERT0(nestlevel > 0);
            tprio *= 10 * nestlevel;
        }
        prio += tprio;
    }
    ASSERTN(prio != 0.0, ("empty lt, consider split it in entry and exit"));
    if (lt->isDedicated()) {
        prio += count * 10;
    }
    return prio;
}


void LTPriorityMgr::computePriority(LTList const& lst) const
{
    LTListIter it;
    for (LifeTime * lt = lst.get_head(&it);
         lt != nullptr; lt = lst.get_next(&it)) {
        double prio = computePriority(lt);
        lt->setPriority(prio);
    }
}

} //namespace xoc
