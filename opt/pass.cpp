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


void PassCtx::copy(PassCtx const& src)
{
    ::memcpy(this, &src, sizeof(PassCtx));
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

} //namespace xoc
