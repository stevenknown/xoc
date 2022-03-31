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
#include "mdliveness_mgr.h"

namespace xoc {

MDSet * MDLivenessMgr::getLiveInMDSet(IRBB * bb)
{
    MDSet * set = m_livein_mds_vec.get(BB_id(bb));
    if (set == NULL) {
        set = getMdsMgr()->alloc();
        m_livein_mds_vec.set(BB_id(bb), set);
    }
    return set;
}


MDSet * MDLivenessMgr::getLiveOutMDSet(IRBB * bb)
{
    MDSet * set = m_liveout_mds_vec.get(BB_id(bb));
    if (set == NULL) {
        set = getMdsMgr()->alloc();
        m_liveout_mds_vec.set(BB_id(bb), set);
    }
    return set;
}


MDSet * MDLivenessMgr::getDefMDSet(IRBB * bb)
{
    MDSet * set = m_def_mds_vec.get(BB_id(bb));
    if (set == NULL) {
        set = getMdsMgr()->alloc();
        m_def_mds_vec.set(BB_id(bb), set);
    }
    return set;
}


MDSet * MDLivenessMgr::getUseMDSet(IRBB * bb)
{
    MDSet * set = m_use_mds_vec.get(BB_id(bb));
    if (set == NULL) {
        set = getMdsMgr()->alloc();
        m_use_mds_vec.set(BB_id(bb), set);
    }
    return set;
}


//Note this function will not clean 'mayuse', thus the result will blend
//with original elements.
void MDLivenessMgr::collectMayUseMDS(IR const* ir, OUT MDSet * mayuse)
{
    ASSERT0(ir->is_stmt());
    ConstIRIter ii;
    xcom::DefMiscBitSetMgr * sbsmgr = getSBSMgr();
    for (IR const* x = iterExpInitC(ir, ii);
         x != NULL; x = iterExpNextC(ii)) {
        if (!x->isMemoryOpnd() || x->is_pr()) { continue; }

        MD const* t = x->getRefMD();
        if (t != NULL) {
            mayuse->bunion_pure(MD_id(t), *sbsmgr);
        }

        MDSet const* mds = x->getRefMDSet();
        if (mds != NULL) {
            mayuse->bunion(*mds, *sbsmgr);
        }
    }
}


void MDLivenessMgr::computeLocalLiveness(IRBB * bb)
{
    MDSet * gen = getDefMDSet(bb);
    MDSet * use = getUseMDSet(bb);
    xcom::DefMiscBitSetMgr * sbsmgr = getSBSMgr();
    MDSystem const* mdsys = m_rg->getMDSystem();
    gen->clean(*sbsmgr);
    use->clean(*sbsmgr);
    for (IR * ir = BB_last_ir(bb); ir != NULL; ir = BB_prev_ir(bb)) {
        MD const* def = ir->getExactRef();
        if (def != NULL) {
            gen->bunion(def->id(), *sbsmgr);
            use->diffAllOverlapped(def->id(), *sbsmgr, mdsys);
        }        
        collectMayUseMDS(ir, use);
    }
}


void MDLivenessMgr::computeGlobalLiveness()
{
    BBList * bbl = m_rg->getBBList();
    xcom::DefMiscBitSetMgr * sbsmgr = getSBSMgr();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        getLiveInMDSet(bb)->clean(*sbsmgr);
        getLiveOutMDSet(bb)->clean(*sbsmgr);
    }
    bool change;
    INT count = 0;
    IRCFG * cfg = m_rg->getCFG();
    MDSet news;
    List<IRBB*> succs;
    do {
        change = false;
        for (IRBB * bb = bbl->get_tail(); bb != NULL; bb = bbl->get_prev()) {
            MDSet * out = getLiveOutMDSet(bb);
            cfg->get_succs(succs, bb);
            news.clean(*getSBSMgr());
            for (IRBB * p = succs.get_head(); p != NULL; p = succs.get_next()) {
                news.bunion(*getLiveInMDSet(p), *getSBSMgr());
            }
            if (!out->is_equal(news)) {
                out->copy(news, *sbsmgr);
                change = true;
            }

            news.copy(*out, *sbsmgr);
            news.diff(*getDefMDSet(bb), *sbsmgr);
            news.bunion(*getUseMDSet(bb), *sbsmgr);
            MDSet * in = getLiveInMDSet(bb);
            in->copy(news, *sbsmgr);
        }
        count++;
    } while (change && count < 220);
    ASSERTN(!change, ("result of equation is convergent slowly"));
    news.clean(*sbsmgr);
}


void MDLivenessMgr::dump(bool with_name) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n==---- DUMP MDLivenessMgr '%s' ----==\n",
         m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    MDLivenessMgr * pthis = const_cast<MDLivenessMgr*>(this);
    BBList * bbl = m_rg->getBBList();
    MDSystem const* sys = m_rg->getMDSystem();
    CHAR const* fmt = with_name ? "MD%d(%s) " : "MD%d ";
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        note(getRegion(), "\n---- BB%d -----", BB_id(bb));
        MDSet * live_in = pthis->getLiveInMDSet(bb);
        MDSet * live_out = pthis->getLiveOutMDSet(bb);
        MDSet * def = pthis->getDefMDSet(bb);
        MDSet * use = pthis->getUseMDSet(bb);
        note(getRegion(), "\nLIVE-IN: ");
        MDSetIter iter;
        for (INT i = live_in->get_first(&iter);
             i != -1; i = live_in->get_next(i, &iter)) {
            MD const* md = const_cast<MDSystem*>(sys)->getMD(i);
            ASSERT0(md);
            prt(getRegion(), fmt, i, with_name ?
                md->get_base()->get_name()->getStr() : NULL);
        }

        note(getRegion(), "\nLIVE-OUT: ");
        for (INT i = live_out->get_first(&iter);
             i != -1; i = live_out->get_next(i, &iter)) {
            MD const* md = const_cast<MDSystem*>(sys)->getMD(i);
            ASSERT0(md);
            prt(getRegion(), fmt, i, with_name ?
                md->get_base()->get_name()->getStr() : NULL);
        }

        note(getRegion(), "\nDEF: ");
        for (INT i = def->get_first(&iter);
             i != -1; i = def->get_next(i, &iter)) {
            MD const* md = const_cast<MDSystem*>(sys)->getMD(i);
            ASSERT0(md);
            prt(getRegion(), fmt, i, with_name ?
                md->get_base()->get_name()->getStr() : NULL);
        }

        note(getRegion(), "\nUSE: ");
        for (INT i = use->get_first(&iter);
             i != -1; i = use->get_next(i, &iter)) {
            MD const* md = const_cast<MDSystem*>(sys)->getMD(i);
            ASSERT0(md);
            prt(getRegion(), fmt, i, with_name ?
                md->get_base()->get_name()->getStr() : NULL);
        }
    }
    m_rg->getLogMgr()->decIndent(2);
    Pass::dump();
}


bool MDLivenessMgr::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        computeLocalLiveness(bb);
    }
    computeGlobalLiveness();
    return false;
}

} //namespace xoc
