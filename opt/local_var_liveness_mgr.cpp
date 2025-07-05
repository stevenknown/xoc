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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START LocalVarCollecter
//
void LocalVarCollecter::handle(IR const* ir)
{
    ASSERT0(ir && ir->is_stmt());
    ASSERT0(ir->is_st() ||
            (ir->hasRHS() && (ir->getRHS()->is_ld() ||
            ir->getRHS()->is_lda())));
    if (ir->is_st()) {
        handleSt(ir);
        return;
    }

    if (ir->getRHS()->is_ld()) {
        handleLd(ir);
        return;
    }

    if (ir->getRHS()->is_lda()) {
        handleLda(ir);
    }
}


void LocalVarCollecter::handleLd(IR const* ir)
{
    ASSERT0(ir && ir->getRHS() && ir->getRHS()->is_ld());
    ASSERT0(ir->getRHS()->hasIdinfo());
    Var * v = ir->getRHS()->getIdinfo();
    ASSERT0(v);
    if (!mayBeReuse(v)) { return; }
    VarCPtr2BoolTy * vars = m_ctx.getCanBeReuseLocalVar();
    if (ir->getRHS()->getOffset() > 0) {
        vars->setAlways(v, false);
        return;
    }
    if (m_ctx.getIsOnlyCollectStackVars() &&
        m_ctx.getLSRA()->isReloadOp(ir)) {
        vars->setAlways(v, false);
        return;
    }
    if (!vars->find(v)) {
        vars->set(v, true);
    }
}


void LocalVarCollecter::handleLda(IR const* ir)
{
    ASSERT0(ir && ir->getRHS() && ir->getRHS()->is_lda());
    ASSERT0(ir->getRHS()->hasIdinfo());
    Var * v = ir->getRHS()->getIdinfo();
    ASSERT0(v);
    if (!mayBeReuse(v)) { return; }
    VarCPtr2BoolTy * vars = m_ctx.getCanBeReuseLocalVar();
    vars->setAlways(v, false);
}


void LocalVarCollecter::handleSt(IR const* ir)
{
    ASSERT0(ir && ir->is_st());
    ASSERT0(ir->hasIdinfo() && ir->getIdinfo());
    Var * v = ir->getIdinfo();
    if (!mayBeReuse(v)) { return; }
    VarCPtr2BoolTy * vars = m_ctx.getCanBeReuseLocalVar();
    if (ir->getOffset() > 0) {
        vars->setAlways(v, false);
        return;
    }
    if (m_ctx.getIsOnlyCollectStackVars() &&
        m_ctx.getLSRA()->isSpillOp(ir)) {
        vars->setAlways(v, false);
        return;
    }
    if (!vars->find(v)) {
        vars->set(v, true);
    }
}


bool LocalVarCollecter::needHandle(IR const* ir) const
{
    ASSERT0(ir && ir->is_stmt());
    if (ir->is_st() || (ir->hasRHS() && (ir->getRHS()->is_ld() ||
        ir->getRHS()->is_lda()))) {
        return true;
    }
    return false;
}


void LocalVarCollecter::walkBBOfRegion()
{
    BBList const* bblst = m_rg->getBBList();
    BBListIter it;
    for (bblst->get_head(&it); it != bblst->end();
         it = bblst->get_next(it)) {
        IRBB const* bb = it->val();
        ASSERT0(bb);
        walkIROfBB(bb);
    }
}


void LocalVarCollecter::walkIROfBB(IRBB const* bb)
{
    ASSERT0(bb);
    BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter irit;
    for (IR const* ir = irlst.get_head(&irit);
         ir != nullptr; ir = irlst.get_next(&irit)) {
        if (!needHandle(ir)) { continue; }
        handle(ir);
    }
}


bool LocalVarCollecter::mayBeReuse(Var const* v) const
{
    ASSERT0(v);
    if (v->getDType() != D_ANY && v->getStorageSpace() == SS_STACK &&
        !m_ctx.getStackArgMap().findVar(v)) {
        return true;
    }
    return false;
}
//END LocalVarCollecter


//
//START LocalVarLivenessMgr
//
void LocalVarLivenessMgr::collect() {
    ArgPasser* arg_passer = (ArgPasser*)(
        m_rg->getPassMgr()->registerPass(PASS_ARGPASSER));
    ASSERT0(arg_passer && arg_passer->getStackArgMap());
    LocalVarContext ctx(m_rg, arg_passer->getStackArgMap(),
        &m_can_be_reuse_local_vars, m_is_only_collect_stack_vars);
    LocalVarCollecter collecter(m_rg, ctx);
    collecter.collect();
}


bool LocalVarLivenessMgr::canBeStmtCand(IR const* stmt)
{
    ASSERT0(stmt != nullptr && stmt->is_stmt() && stmt->hasIdinfo());
    if (!stmt->is_st()) { return false; }
    Var * v = stmt->getIdinfo();
    ASSERT0(v);
    return m_can_be_reuse_local_vars.get(v);
}


bool LocalVarLivenessMgr::canBeExpCand(IR const* stmt, IR const* ir)
{
    ASSERT0(ir != nullptr && stmt != nullptr);
    ASSERT0(ir->is_exp() && stmt->is_stmt());
    ASSERT0(ir->getStmt() == stmt && ir->getIdinfo() != nullptr);
    if (!ir->is_ld()) { return false; }
    ASSERT0(ir->hasIdinfo());
    Var * v = ir->getIdinfo();
    ASSERT0(v);
    return m_can_be_reuse_local_vars.get(v);
}
//END LocalVarLivenessMgr

} //namespace xoc
