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
#ifndef __LOCAL_VAR_LIFETIME_MGR_H__
#define __LOCAL_VAR_LIFETIME_MGR_H__

namespace xoc {

class VarLifeTimeMgr;

typedef xcom::TMap<Var const*, bool> VarCPtr2BoolTy;

class LocalVarLifeTimeMgr : public VarLifeTimeMgr {
    COPY_CONSTRUCTOR(LocalVarLifeTimeMgr);
protected:
    VarCPtr2BoolTy const* m_can_be_reuse_loval_var;
public:
    LocalVarLifeTimeMgr(Region * rg, LinearScanRA & ra,
        LocalVarLivenessMgr * var_liveness) :
        VarLifeTimeMgr(rg, ra, var_liveness)
    {
        ASSERT0(var_liveness);
        m_can_be_reuse_loval_var = var_liveness->getCanBeReuseLocalVars();
    }
    virtual ~LocalVarLifeTimeMgr() {}

    //Return whether the lifetime of the operand on the left needs
    //to be calculated, and return true if it needs to be calculated.
    virtual bool isNeedComputeLHS(IR const* ir) const override;

    //Return whether the lifetime of the operand on the left needs
    //to be calculated, and return true if it needs to be calculated.
    virtual bool isNeedComputeRHS(IR const* ir) const override;
};

} //namespace xoc
#endif

