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
#ifndef __VAR_LIVENESS_MGR_H__
#define __VAR_LIVENESS_MGR_H__

namespace xoc {

typedef LiveSet VarLiveSet;
typedef LiveSetIter VarLiveSetIter;

//
//START VarLivenessMgr
//
class VarLivenessMgr : public LivenessMgr {
    COPY_CONSTRUCTOR(VarLivenessMgr);
protected:
    virtual bool canBeStmtCand(IR const* stmt)
    {
        ASSERT0(stmt && stmt->is_stmt() && stmt->hasIdinfo());
        return true;
    }
    virtual bool canBeExpCand(IR const* stmt, IR const* ir)
    {
        ASSERT0(ir && stmt && ir->is_exp() && stmt->is_stmt());
        ASSERT0(ir->getStmt() == stmt);
        ASSERT0(ir->hasIdinfo());
        return true;
    }
    virtual void computeExpImpl(
        IR const* exp, MOD LiveSet * use, MOD LiveSet * gen) override;
    virtual void computeStmtImpl(
        IR const* stmt, MOD LiveSet * use, MOD LiveSet * gen) override;
public:
    VarLivenessMgr(Region * rg) : LivenessMgr(rg) {}
    ~VarLivenessMgr() { clean(); }
    PASS_TYPE getPassType() const { return PASS_LIVENESS_MGR; }
};
//END VarLivenessMgr

} //namespace xoc
#endif
