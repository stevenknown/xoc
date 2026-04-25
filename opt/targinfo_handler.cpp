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
//START TargInfoHandler
//
TargInfoHandler::TargInfoHandler(Region * rg) : Pass(rg)
{
    m_ramgr = nullptr;
    m_passmgr = m_rg->getPassMgr();
}



RegAllocMgr * TargInfoHandler::queryRegAllocMgr()
{
    #ifdef REF_TARGMACH_INFO
    if (m_ramgr == nullptr) {
        m_ramgr = (RegAllocMgr*)m_passmgr->queryPass(PASS_REGALLOC_MGR);
    }
    #endif
    return m_ramgr;
}


void TargInfoHandler::tryCopyPhyRegIfAny(PRNO src, PRNO tgt)
{
    if (src == PRNO_UNDEF || tgt == PRNO_UNDEF || src == tgt) {
        return;
    }
    #ifdef REF_TARGMACH_INFO
    RegAllocMgr * ramgr = queryRegAllocMgr();
    if (ramgr == nullptr) { return; }

    //If the original prno is pre-assigned and might have been bound to a
    //specific physical register, the newly created prno also needs to be bound
    //to this same physical register as well.
    //e.g: original PRNO is $1(with r1), the renamed PRNO should be $2(with r1
    //too).
    xgen::Reg phyreg = ramgr->getPreAssignedReg(src);
    if (phyreg != REG_UNDEF) {
        ASSERT0(!ramgr->isPreAssigned(tgt));
        ramgr->setPreAssignedReg(tgt, phyreg);
    }
    #endif
}
//END TargInfoHandler

} //namespace xoc
