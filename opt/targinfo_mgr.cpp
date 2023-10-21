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

namespace xoc {

//
//START TargInfoMgr
//
void TargInfoMgr::init()
{
    m_link = REG_RETURN_ADDRESS_REGISTER;
}


void TargInfoMgr::destroy()
{
}


void TargInfoMgr::reset()
{
    destroy();
    init();
}


RegSet const* TargInfoMgr::getAllocable() const
{
    return xgen::tmGetRegSetAllocable();
}


RegSet const* TargInfoMgr::getReturnValue() const
{
    return xgen::tmGetRegSetOfReturnValue();
}


RegSet const* TargInfoMgr::getParam() const
{
    return xgen::tmGetRegSetOfArgument();
}


RegSet const* TargInfoMgr::getCaller() const
{
    return xgen::tmGetRegSetOfCallerSaved();
}


RegSet const* TargInfoMgr::getCallee() const
{
    return xgen::tmGetRegSetOfCalleeSaved();
}


RegSet const* TargInfoMgr::getVectorAllocable() const
{
    return xgen::tmGetVectorRegSetAllocable();
}


RegSet const* TargInfoMgr::getVectorReturnValue() const
{
    return xgen::tmGetVectorRegSetOfReturnValue();
}


RegSet const* TargInfoMgr::getVectorParam() const
{
    return xgen::tmGetVectorRegSetOfArgument();
}


RegSet const* TargInfoMgr::getVectorCaller() const
{
    return xgen::tmGetVectorRegSetOfCallerSaved();
}


RegSet const* TargInfoMgr::getVectorCallee() const
{
    return xgen::tmGetVectorRegSetOfCalleeSaved();
}


void TargInfoMgr::dump(Region const* rg) const
{
    note(rg, "\n==-- DUMP %s --==", "TargInfoMgr");
    TargInfoMgr * pthis = const_cast<TargInfoMgr*>(this);
    rg->getLogMgr()->incIndent(2);
    xcom::StrBuf buf(32);
    if (pthis->getAllocable() != nullptr) {
        pthis->getAllocable()->dump(buf);
    }
    note(rg, "\nALLOCABLE:%s", buf.buf);

    buf.clean();
    if (pthis->getCaller() != nullptr) {
        pthis->getCaller()->dump(buf);
    }
    note(rg, "\nCALLER:%s", buf.buf);

    buf.clean();
    if (pthis->getCallee() != nullptr) {
        pthis->getCallee()->dump(buf);
    }
    note(rg, "\nCALLEE:%s", buf.buf);

    buf.clean();
    if (pthis->getParam() != nullptr) {
        pthis->getParam()->dump(buf);
    }
    note(rg, "\nPARAM:%s", buf.buf);

    buf.clean();
    if (pthis->getReturnValue() != nullptr) {
        pthis->getReturnValue()->dump(buf);
    }
    note(rg, "\nRETURN_VALUE:%s", buf.buf);

    note(rg, "\nLINK:%s", getRegName(getLink()));
    rg->getLogMgr()->decIndent(2);
}


REGFILE TargInfoMgr::getRegFile(Reg r) const
{
    return xgen::tmMapReg2RegFile(r);
}


CHAR const* TargInfoMgr::getRegName(Reg r) const
{
    return xgen::tmGetRegName(r);
}


CHAR const* TargInfoMgr::getRegFileName(REGFILE rf) const
{
    return xgen::tmGetRegFileName(rf);
}
//END TargInfoMgr

} //namespace xoc
