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
#ifndef __CFS_OPT_H__
#define __CFS_OPT_H__

namespace xoc {

class CfsOpt : public Pass {
    COPY_CONSTRUCTOR(CfsOpt);
    TypeMgr * m_tm;
    Refine * m_rf;
private:
    Refine * getRefiner()
    {
        if (m_rf == nullptr) {
            m_rf = (Refine*)m_rg->getPassMgr()->registerPass(PASS_REFINE);
        }
        return m_rf;
    }

    bool transformToDoWhile(IR ** head, IR ** ir);
    bool transformIf1(IR ** head, IR ** ir);
    bool transformIf2(IR ** head, IR ** ir);
    bool transformIf3(IR ** head, IR ** ir);
    bool transformIf4(IR ** head, IR ** ir);
    bool transformIf5(IR ** head, IR ** ir);
    bool hoistLoop(IR ** head, IR ** ir);
    bool hoistIf(IR ** head, IR ** ir);
public:
    CfsOpt(Region * rg) : Pass(rg)
    {
        m_tm = rg->getTypeMgr();
        m_rf = nullptr;
    }
    ~CfsOpt() {}

    bool dump() const { return true; }
    bool doCfsOpt(MOD IR ** ir_list, SimpCtx const& sc);

    virtual CHAR const* getPassName() const { return "CfsOpt"; }

    virtual bool perform(OptCtx &)
    {
        UNREACHABLE();
        return false;
    }
    virtual bool perform(SimpCtx const& simp);
};

} //namespace xoc
#endif
