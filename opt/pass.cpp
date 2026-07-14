/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START PassCtx
//
PassCtx::PassCtx(ActMgr * am)
{
    clean();
    m_am = am;
}


PassCtx::PassCtx(OptCtx * oc, ActMgr * am)
{
    ASSERT0(oc);
    m_oc = oc;
    m_am = am;
    m_rg = oc->getRegion();
    ASSERT0(m_rg);
    m_ivr = (IVR*)m_rg->getPassMgr()->queryPass(PASS_IVR);
    m_gvn = (GVN*)m_rg->getPassMgr()->queryPass(PASS_GVN);
}


void PassCtx::clean()
{
    m_oc = nullptr;
    m_ivr = nullptr;
    m_gvn = nullptr;
    m_am = nullptr;
    m_rg = nullptr;
}


void PassCtx::copy(PassCtx const& src)
{
    m_oc = src.m_oc;
    m_ivr = src.m_ivr;
    m_gvn = src.m_gvn;
    m_am = src.m_am;
    m_rg = src.m_rg;
}


void PassCtx::tryInvalidInfoBeforeFreeIRList(IR const* ir) const
{
    xoc::tryInvalidIVRIfIRIsIV(m_ivr, ir, getOptCtx(), m_am);
    xoc::cleanVNForIRTreeList(m_gvn, ir);
}


void PassCtx::tryInvalidInfoBeforeFreeIR(IR const* ir) const
{
    xoc::tryInvalidIVRIfIRIsIV(m_ivr, ir, getOptCtx(), m_am);
    xoc::cleanVNForIRTree(m_gvn, ir);
}


void PassCtx::tryInvalidPassInfoBeforeFreeIR(IR const* ir) const
{
}
//END PassCtx


//
//START Pass
//
bool Pass::dumpBeforePass() const
{
    if (!m_rg->isLogMgrInit()) { return true; }
    if (!g_dump_opt.isDumpBeforePass()) { return true; }
    if (!g_dump_opt.isDumpPass(getPassType())) { return true; }
    START_TIMER_FMT(t, ("DUMP BEFORE PASS '%s'", getPassName()));
    xoc::note(
        m_rg, "\n==---- DUMP BEFORE '%s' '%s' ----==",
        getPassName(), m_rg->getRegionName());
    m_rg->dump(false);
    END_TIMER_FMT(t, ("DUMP BEFORE PASS '%s'", getPassName()));
    return true;
}


bool Pass::dumpAfterPass() const
{
    if (!m_rg->isLogMgrInit()) { return true; }
    if (!g_dump_opt.isDumpAfterPass()) { return true; }
    if (!g_dump_opt.isDumpPass(getPassType())) { return true; }
    START_TIMER_FMT(t, ("DUMP AFTER PASS '%s'", getPassName()));
    xoc::note(
        m_rg, "\n==---- DUMP AFTER '%s' '%s' ----==",
        getPassName(), m_rg->getRegionName());
    m_rg->dump(false);
    END_TIMER_FMT(t, ("DUMP AFTER PASS '%s'", getPassName()));
    return true;
}


bool Pass::dump() const
{
    //Optimization Dependent Code.
    //The recommended dump headline format is:
    //\n==---- DUMP PassName 'RegionName' ----==
    return dumpAfterPass();
}

//END Pass

} //namespace xoc
