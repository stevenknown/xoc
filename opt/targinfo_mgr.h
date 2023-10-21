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
#ifndef _TARGINFO_MGR_H_
#define _TARGINFO_MGR_H_

//Header files in XGEN is referred by linear_scan.h.
//It looks like backward invoking functions that defined in XGEN.
#include "../xgen/reg.h"
#include "../xgen/regfile.h"
#include "../xgen/targ_interface.h"

using namespace xgen;

namespace xoc {

//The class represents register set and calling convention information for
//target machine. It is an wrapper of interfaces under target/precompile.
class TargInfoMgr {
    COPY_CONSTRUCTOR(TargInfoMgr);
private:
    Reg m_link;
public:
    TargInfoMgr() { init(); }
    virtual ~TargInfoMgr() { destroy(); }

    void dump(Region const* rg) const;
    void destroy();

    REGFILE getRegFile(Reg r) const;
    RegSet const* getAllocable() const;
    RegSet const* getCallee() const;
    RegSet const* getCaller() const;
    RegSet const* getReturnValue() const;
    RegSet const* getParam() const;
    RegSet const* getVectorAllocable() const;
    RegSet const* getVectorReturnValue() const;
    RegSet const* getVectorParam() const;
    RegSet const* getVectorCaller() const;
    RegSet const* getVectorCallee() const;
    Reg getLink() const { return m_link; }
    virtual CHAR const* getRegName(Reg r) const;
    virtual CHAR const* getRegFileName(REGFILE rf) const;

    bool isAllocable(Reg r) const
    {
        RegSet const* s = getAllocable();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isCallee(Reg r) const
    {
        RegSet const* s = getCallee();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isCaller(Reg r) const
    {
        RegSet const* s = getCaller();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isReturnValue(Reg r) const
    {
        RegSet const* s = getReturnValue();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isParam(Reg r) const
    {
        RegSet const* s = getParam();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorAllocable(Reg r) const
    {
        RegSet const* s = getVectorAllocable();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorCallee(Reg r) const
    {
        RegSet const* s = getVectorCallee();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorCaller(Reg r) const
    {
        RegSet const* s = getVectorCaller();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorReturnValue(Reg r) const
    {
        RegSet const* s = getVectorReturnValue();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isVectorParam(Reg r) const
    {
        RegSet const* s = getVectorParam();
        return s == nullptr ? false : s->is_contain(r);
    }
    bool isLink(Reg r) const { return getLink() == r; }
    void init();

    void reset();
};

} //namespace xoc
#endif
