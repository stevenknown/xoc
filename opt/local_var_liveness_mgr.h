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
@*/
#ifndef __LOCAL_VAR_LIVENESS_MGR_H__
#define __LOCAL_VAR_LIVENESS_MGR_H__

namespace xoc {

class ArgPasser;
class VarLivenessMgr;
class Var;
class CallIR2ArgListMap;

typedef xcom::TMap<Var const*, bool> VarCPtr2BoolTy;
typedef xcom::TMapIter<Var const*, bool> VarCPtr2BoolIterTy;

class LocalVarContext {
    COPY_CONSTRUCTOR(LocalVarContext);
private:
    //Whether only collect stack variables.
    //Default collection of stack variables and spill variables.
    bool m_is_only_collect_stack_vars;
    Region * m_rg;
    LinearScanRA * m_lsra;
    VarCPtr2BoolTy * m_can_be_reuse_loval_vars;
    CallIR2ArgListMap const* m_stack_arg_map;
public:
    LocalVarContext(Region * rg, CallIR2ArgListMap const* stack_arg_map,
        VarCPtr2BoolTy * can_be_reuse_loval_vars,
        bool is_only_collect_stack_vars = false)
    {
        m_rg = rg;
        m_stack_arg_map = stack_arg_map;
        m_can_be_reuse_loval_vars = can_be_reuse_loval_vars;
        m_is_only_collect_stack_vars = is_only_collect_stack_vars;
        if (m_is_only_collect_stack_vars) {
            m_lsra = (LinearScanRA*)(m_rg->getPassMgr()->registerPass(
                PASS_LINEAR_SCAN_RA));
            ASSERT0(m_lsra);
        }
    }
    VarCPtr2BoolTy * getCanBeReuseLocalVar()
    { return m_can_be_reuse_loval_vars; }
    bool getIsOnlyCollectStackVars() { return m_is_only_collect_stack_vars; }
    LinearScanRA const* getLSRA() const { return m_lsra; }
    CallIR2ArgListMap const& getStackArgMap() const
    { return *m_stack_arg_map; }
};


class LocalVarCollecter {
    COPY_CONSTRUCTOR(LocalVarCollecter);
private:
    Region * m_rg;
    LocalVarContext & m_ctx;
public:
    LocalVarCollecter(Region * rg, LocalVarContext & ctx) :
        m_rg(rg), m_ctx(ctx) {}
    void collect() { walkBBOfRegion(); }
private:
    void handle(IR const* ir);
    void handleLd(IR const* ir);
    void handleLda(IR const* ir);
    void handleSt(IR const* ir);
    bool needHandle(IR const* ir) const;
    void walkBBOfRegion();
    void walkIROfBB(IRBB const* bb);
    bool mayBeReuse(Var const* v) const;
};


//This class is used to calculate the live information of
//local variables. Note that the spill variable
//also belongs to the local variable.
class LocalVarLivenessMgr : public VarLivenessMgr {
    COPY_CONSTRUCTOR(LocalVarLivenessMgr);
protected:
    //whether only collect stack variables.
    //Default collection of stack variables and spill variables.
    bool m_is_only_collect_stack_vars;
    VarCPtr2BoolTy m_can_be_reuse_local_vars;
public:
    virtual bool canBeStmtCand(IR const* stmt) override;
    virtual bool canBeExpCand(IR const* stmt, IR const* ir) override;
public:
    LocalVarLivenessMgr(Region * rg, bool only_stack_vars = false) :
        VarLivenessMgr(rg)
    {
        m_is_only_collect_stack_vars = only_stack_vars;
        collect();
    }
    virtual ~LocalVarLivenessMgr() {}
    VarCPtr2BoolTy const* getCanBeReuseLocalVars() const
    { return &m_can_be_reuse_local_vars; }
private:
    void collect();
};

} //namespace xoc

#endif
