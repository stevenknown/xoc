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
#ifndef __PR_LIVENESS_MGR_H__
#define __PR_LIVENESS_MGR_H__

namespace xoc {

//Map between Var and PRNO.
class Var2PR : public TMap<Var const*, PRNO, CompareConstVar> {
};

typedef LiveSet PRLiveSet;
typedef LiveSetIter PRLiveSetIter;

class PRLivenessMgr : public LivenessMgr {
    COPY_CONSTRUCTOR(PRLivenessMgr);
protected:
    BYTE m_handle_may:1; //true if consider maydef/mayuse info.
    Var2PR * m_var2pr;
protected:
    virtual void computeExpImpl(
        IR const* exp, MOD LiveSet * use, MOD LiveSet * gen) override;
    virtual void computeStmtImpl(
        IR const* stmt, MOD LiveSet * use, MOD LiveSet * gen) override;

    void processMayDef(PRNO prno, MOD PRLiveSet * gen,
                       MOD PRLiveSet * use);
    void processMay(IR const* pr, MOD PRLiveSet * gen,
                    MOD PRLiveSet * use, bool is_lhs);
    void processMayUse(PRNO prno, MOD PRLiveSet * use);
public:
    PRLivenessMgr(Region * rg);
    ~PRLivenessMgr() { clean(); }

    virtual CHAR const* getPassName() const { return "PRLivenessMgr"; }
    PASS_TYPE getPassType() const { return PASS_PRLIVENESS_MGR; }

    //Set true to handle maydef and mayuse MDSet.
    void set_handle_may(bool handle) { m_handle_may = (BYTE)handle; }

    //Set map structure which used during the processing of maydef and mayuse.
    void setVar2PR(Var2PR * v2p) { m_var2pr = v2p; }
};

} //namespace xoc
#endif
