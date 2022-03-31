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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

GSCC::GSCC(Region * rg) : Pass(rg), m_scc(rg->getCFG()) {}

//Verify that LoopInfo of CFG should be consistent with SCC info.
bool GSCC::verify()
{
    IRCFG * cfg = (IRCFG*)m_rg->getPassMgr()->queryPass(PASS_CFG);
    if (cfg == nullptr) { return true; }

    LI<IRBB> const* li = cfg->getLoopInfo();

    //Only have to check the outermost loop body.
    for (; li != nullptr; li = li->get_next()) {
        BitSet * bbs = li->getBodyBBSet();
        ASSERT0(bbs);
        for (INT i = bbs->get_first(); i != -1; i = bbs->get_next(i)) {
            ASSERT0(m_scc.isInSCC((UINT)i));
        }
    }
    return true;
}


bool GSCC::perform(OptCtx & oc)
{
    ASSERT0(oc.is_cfg_valid());
    START_TIMER(t, "Compute Graph SCC");
    m_scc.findSCC();
    END_TIMER(t, "Compute Graph SCC");
    OC_is_scc_valid(oc) = true;
    ASSERT0(verify());
    return false;
}

} //namespace xoc
