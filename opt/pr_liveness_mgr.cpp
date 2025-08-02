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
//START PRLivenessMgr
//
PRLivenessMgr::PRLivenessMgr(Region * rg) : LivenessMgr(rg)
{
    m_md_sys = rg->getMDSystem();
    m_var2pr = nullptr;
    m_handle_may = false;
}


void PRLivenessMgr::processMay(
    IR const* pr, MOD LiveSet * gen, MOD LiveSet * use, bool is_lhs)
{
    if (!m_handle_may) { return; }
    MDSet const* mds = pr->getMayRef();
    if (mds == nullptr) { return; }

    MD const* prmd = pr->getExactRef();
    ASSERT0(prmd);
    MDSetIter iter;
    for (BSIdx i = mds->get_first(&iter);
         i != BS_UNDEF; i = mds->get_next(i, &iter)) {
        MD const* md = m_md_sys->getMD((MDIdx)i);
        ASSERT0(md);
        if (MD_base(md) == MD_base(prmd)) { continue; }

        bool find;
        ASSERT0(m_var2pr); //One should initialize m_var2pr.
        PRNO prno = m_var2pr->get(MD_base(md), &find);
        ASSERT0(find);
        if (is_lhs) {
            processMayDef(prno, gen, use);
            continue;
        }
        processMayUse(prno, use);
    }
}


void PRLivenessMgr::processMayDef(
    PRNO prno, MOD PRLiveSet * gen, MOD PRLiveSet * use)
{
    ASSERT0(gen && use);
    gen->bunion((BSIdx)prno, m_sbs_mgr);
    use->diff((BSIdx)prno, m_sbs_mgr);
}


void PRLivenessMgr::processMayUse(PRNO prno, MOD PRLiveSet * use)
{
    ASSERT0(use);
    updateSetByExp((BSIdx)prno, use);
}


void PRLivenessMgr::computeExpImpl(
    IR const* exp, MOD LiveSet * use, MOD LiveSet * gen)
{
    ASSERT0(exp->is_exp());
    if (!exp->isReadPR()) { return; }
    updateSetByExp((BSIdx)exp->getPrno(), use);
    processMay(exp, gen, use, false);
}


void PRLivenessMgr::computeStmtImpl(
    IR const* stmt, MOD LiveSet * use, MOD LiveSet * gen)
{
    ASSERT0(stmt->is_stmt());
    if (!stmt->isWritePR() && !stmt->isCallStmt() && !stmt->is_region()) {
        return;
    }
    IR * result = const_cast<IR*>(stmt)->getResultPR();
    if (result != nullptr) {
        PRNO prno = result->getPrno();
        ASSERT0(prno != PRNO_UNDEF);
        updateSetByStmt((BSIdx)prno, use, gen);
    }
    processMay(stmt, gen, use, true);
}
//END PRLivenessMgr

} //namespace xoc
