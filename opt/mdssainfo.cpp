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

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START MDDef
//
//Return true if n is the Next DEF of current DEF.
bool MDDef::isNext(MDDef const* n) const
{
    return getNextSet() != nullptr ? getNextSet()->find(n) : false;
}


//Return true if n is the Next DEF, and n may not be the immediate-next-def to
//current DEF. The function will access all next DEFs recursively.
bool MDDef::isInNextSet(MDDef const* n, UseDefMgr const* mgr) const
{
    #ifdef _DEBUG_
    xcom::TTab<UINT> visited; //to guarantee there is no cycle in NextSet.
    #endif
    List<MDDef const*> wl;
    wl.append_tail(this);
    MDDef const* def;
    while ((def = wl.remove_head()) != nullptr) {
        #ifdef _DEBUG_
        ASSERT0(!visited.find(def->id()));
        visited.append(def->id());
        #endif
        if (def->isNext(n)) { return true; }
        if (def->getNextSet() == nullptr) { continue; }

        MDDefSetIter nit = nullptr;
        for (INT w = def->getNextSet()->get_first(&nit);
            w >= 0; w = def->getNextSet()->get_next(w, &nit)) {
            MDDef const* next = mgr->getMDDef(w);
            ASSERTN(next, ("not such MDDef"));
            wl.append_tail(next);
        }
    }
    return false;
}


void MDDef::cleanNextSet(UseDefMgr * mgr)
{
    if (getNextSet() != nullptr) {
        getNextSet()->clean(*mgr->getSBSMgr());
    }
}
//END MDDef


//
//START VMDVec
//
//Find the VMD that have MD defined at given BB.
VMD * VMDVec::findVMD(UINT bbid) const
{
    for (INT i = 0; i <= get_last_idx(); i++) {
        VMD * p = get(i);
        if (p == nullptr || p->getDef() == nullptr) { continue; }
        ASSERT0(p->getDef() && p->getDef()->getBB());
        if (p->getDef()->getBB()->id() == bbid) {
            return p;
        }
    }
    return nullptr;
}
//END VMDVec


//
//START MDSSAInfo
//
//Return true if all definition of vopnd can reach 'exp'.
bool MDSSAInfo::isUseReachable(UseDefMgr const* udmgr, IR const* exp) const
{
    ASSERT0(udmgr && exp && exp->is_exp());
    VOpndSetIter iter = nullptr;
    MDSSAInfo * pthis = const_cast<MDSSAInfo*>(this);
    for (INT i = pthis->getVOpndSet()->get_first(&iter);
         i >= 0; i = pthis->getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (!vopnd->getUseSet()->find(exp)) {
            return false;
        }
    }
    return true;
}


void MDSSAInfo::copy(MDSSAInfo const& src, UseDefMgr * mgr)
{
    ASSERT0(this != &src);
    getVOpndSet()->copy(*src.readVOpndSet(), *mgr->getSBSMgr());
}


void MDSSAInfo::cleanVOpndSet(UseDefMgr * mgr)
{
    m_vopnd_set.clean(*mgr->getSBSMgr());
}


//Collect all USE, where USE is IR expression.
//The function will not go through MDPhi operation.
//Note the function will not clear 'set' because caller may perform unify
//operation.
void MDSSAInfo::collectUse(UseDefMgr const* udmgr, OUT IRSet * set) const
{
    //DO NOT CLEAN SET
    ASSERT0(set && udmgr);
    VOpndSetIter iter = nullptr;
    MDSSAInfo * pthis = const_cast<MDSSAInfo*>(this);
    for (INT i = pthis->getVOpndSet()->get_first(&iter);
         i >= 0; i = pthis->getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());

        #ifdef _DEBUG_
        Region * rg = udmgr->getRegion();
        IRSetIter vit = nullptr;
        for (INT i2 = vopnd->getUseSet()->get_first(&vit);
            i2 >= 0; i2 = vopnd->getUseSet()->get_next(i2, &vit)) {
            IR * use = rg->getIR(i2);
            ASSERT0(use && (use->isMemoryRef() || use->is_id()));
        }
        #endif

        set->bunion(*vopnd->getUseSet());
    }
}


static void collectDefThroughDefChain(MDSSAMgr const* mdssamgr,
                                      MDDef const* def,
                                      OUT IRSet * set)
{
    ASSERT0(def);
    ConstMDDefIter it;
    for (MDDef const* d = mdssamgr->iterDefInitC(def, it);
         d != nullptr; d = mdssamgr->iterDefNextC(it)) {
        if (d->is_phi()) {
            //Nothing to do. The DEF of operand will be iterated at
            //iterDefCHelper().
            continue;
        }
        ASSERT0(d->getOcc());
        set->bunion(d->getOcc()->id());

        //TODO:for now, we have to walk alone with DEF chain to
        //mark almost all DEF to be effect. This may lead to
        //traverse the same DEF many times. Apply DP like algo to reduce
        //the traversal time.
    }
}


//Collect all DEF that overlapped with 'ref', where DEF is IR expression.
//Note the function will NOT clear 'set' because caller may perform union
//operation.
//ref: given MD, if it is NULL, the function will collect all DEFs.
//iter_phi_opnd: true if the collection will keep iterating DEF of PHI operand.
void MDSSAInfo::collectDef(MDSSAMgr const* mdssamgr, MD const* ref,
                           bool iter_phi_opnd, OUT IRSet * set) const
{
    //DO NOT CLEAN 'set'.
    UseDefMgr const* udmgr = const_cast<MDSSAMgr*>(mdssamgr)->getUseDefMgr();
    VOpndSetIter it = nullptr;
    for (INT i = readVOpndSet()->get_first(&it);
         i >= 0; i = readVOpndSet()->get_next(i, &it)) {
        VOpnd const* t = udmgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        MDDef * tdef = ((VMD*)t)->getDef();
        if (tdef == nullptr) { continue; }
        if (tdef->is_phi() && iter_phi_opnd) {
            //TODO: iterate phi operands.
            collectDefThroughDefChain(mdssamgr, tdef, set);
            continue;
        }

        IR const* defstmt = tdef->getOcc();
        ASSERT0(defstmt);
        if (defstmt->isCallStmt()) {
            //CASE:call()
            //     ...=USE
            //Call is the only stmt that need to process specially.
            //Because it is not killing-def.
            collectDefThroughDefChain(mdssamgr, tdef, set);
            continue;
        }

        ASSERT0(defstmt->isMemoryRefNonPR());
        MD const* mustdef = defstmt->getRefMD();
        if (ref != nullptr && mustdef != nullptr &&
            ref->is_exact() && mustdef->is_exact() &&
            (mustdef == ref || mustdef->is_exact_cover(ref))) {
            //defstmt is killing definition of 'ref'.
            set->bunion(defstmt->id());
            continue;
        }

        if (ref != nullptr) {
            //TODO:
            //CASE1:DEF=...
            //      ...=USE
            //CASE2:...=
            //      ...=USE
            //Both cases need to collect all DEFs until
            //meeting the killing-def.
            collectDefThroughDefChain(mdssamgr, tdef, set);
            continue;
        }

        //CASE1:...=
        //         =...
        //CASE2:DEF=...
        //         =...
        //Both cases need to collect all DEFs through def-chain.
        collectDefThroughDefChain(mdssamgr, tdef, set);
    }
}


//Remove vopnd from current MDSSAInfo.
void MDSSAInfo::removeVOpnd(VOpnd const* vopnd, UseDefMgr * mgr)
{
    m_vopnd_set.remove(vopnd, *mgr->getSBSMgr());
}


//Remove given IR expression from occurence set.
//exp: IR expression to be removed.
void MDSSAInfo::removeUse(IR const* exp, IN UseDefMgr * mgr)
{
    ASSERT0(exp && exp->is_exp() && mgr);
    VOpndSetIter iter = nullptr;
    for (INT i = getVOpndSet()->get_first(&iter);
         i >= 0; i = getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)mgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->removeUse(exp);
    }
}


void MDSSAInfo::addUseSet(MDSSAInfo const* src, IN UseDefMgr * mgr)
{
    ASSERT0(src);
    getVOpndSet()->bunion(*src->readVOpndSet(), *mgr->getSBSMgr());
}


//Add given IR expression to occurence set.
//exp: IR expression to be added.
void MDSSAInfo::addUse(IR const* exp, IN UseDefMgr * mgr)
{
    ASSERT0(exp && exp->is_exp() && exp->isMemoryRefNonPR() && mgr);
    VOpndSetIter iter = nullptr;
    for (INT i = getVOpndSet()->get_first(&iter);
         i >= 0; i = getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)mgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->addUse(exp);
    }
}


VOpnd * MDSSAInfo::getVOpndForMD(UINT mdid, MDSSAMgr const* mgr) const
{
    //Iterate each VOpnd.
    VOpndSetIter iter = nullptr;
    for (INT i = readVOpndSet()->get_first(&iter);
         i >= 0; i = readVOpndSet()->get_next(i, &iter)) {
        VMD * t = (VMD*)const_cast<MDSSAMgr*>(mgr)->
            getUseDefMgr()->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->mdid() == mdid) { return t; }
    }
    return nullptr;
}


void MDSSAInfo::dump(MDSSAMgr const* mgr) const
{
    if (!mgr->getRegion()->isLogMgrInit()) { return; }
    VOpndSetIter iter = nullptr;
    MDSSAInfo * pthis = const_cast<MDSSAInfo*>(this);
    for (INT i = pthis->getVOpndSet()->get_first(&iter);
         i >= 0; i = pthis->getVOpndSet()->get_next(i, &iter)) {
        note(mgr->getRegion(), "\nREF:");
        VMD const* vopnd = (VMD*)const_cast<MDSSAMgr*>(mgr)->
                           getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->dump(mgr->getRegion(), const_cast<MDSSAMgr*>(mgr)->
                    getUseDefMgr());
    }
}


void MDSSAInfo::addVOpnd(VOpnd const* vopnd, UseDefMgr * mgr)
{
    m_vopnd_set.append(vopnd, *mgr->getSBSMgr());
}
//END MDSSAInfo


//
//START UINT2VMDVec
//
void UINT2VMDVec::set(UINT mdid, VMDVec * vmdvec)
{
    if (mdid < m_threshold) {
        m_mdid2vmdvec_vec.set(mdid, vmdvec);
        return;
    }
    m_mdid2vmdvec_map.set(mdid, vmdvec);
}


size_t UINT2VMDVec::count_mem() const
{
    size_t count = sizeof(UINT2VMDVec);
    count += m_mdid2vmdvec_vec.count_mem();
    count += m_mdid2vmdvec_map.count_mem();
    return count;
}
//END UINT2VMDVec


//
//START VMD
//
//Concisely dump.
void VMD::dump(Region const* rg) const
{
    prt(rg, "MD%dV%d", mdid(), version());
}


void VMD::clean()
{
    VOpnd::clean();
    VMD_def(this) = nullptr;
    VMD_mdid(this) = MD_UNDEF;
    VMD_version(this) = MDSSA_INIT_VERSION;
    VMD_occs(this).clean();
}


void VMD::dump(Region const* rg, UseDefMgr const* mgr) const
{
    if (!rg->isLogMgrInit()) { return; }
    ASSERT0(is_md() && rg);
    prt(rg, "(MD%dV%d", mdid(), version());

    //Dump Def
    if (getDef() != nullptr) {
        //TBD: I think MDDef could be PHI.
        //ASSERT0(!getDef()->is_phi());

        if (getDef()->getPrev() != nullptr) {
            prt(rg, ",PrevDEF:MD%dV%d",
                getDef()->getPrev()->getResult()->mdid(),
                getDef()->getPrev()->getResult()->version());
        } else {
            prt(rg, ",-");
        }

        if (getDef()->getNextSet() != nullptr) {
            MDDefSetIter nit = nullptr;
            bool first = true;
            for (INT w = getDef()->getNextSet()->get_first(&nit);
                w >= 0; w = getDef()->getNextSet()->get_next(w, &nit)) {
                if (first) {
                    first = false;
                } else {
                    prt(rg, ",");
                }

                MDDef const* use = mgr->getMDDef(w);
                ASSERTN(use, ("not such MDDef"));
                ASSERT0(use->getResult());
                ASSERTN(use->getPrev() == getDef(), ("insanity relation"));
                prt(rg, ",NextDEF:MD%dV%d",
                    use->getResult()->mdid(), use->getResult()->version());
            }
        }
    } else {
        prt(rg, ",-");
    }
    prt(rg, ")");

    //Dump OccSet
    prt(rg, "|USESET:");
    IRSetIter vit = nullptr;
    bool first = true;
    VMD * pthis = const_cast<VMD*>(this);
    for (INT i2 = pthis->getUseSet()->get_first(&vit);
        i2 >= 0; i2 = pthis->getUseSet()->get_next(i2, &vit)) {
        if (first) {
            first = false;
        } else {
            prt(rg, ",");
        }

        IR * use = rg->getIR(i2);
        ASSERT0(use && (use->isMemoryRef() || use->is_id()));
        prt(rg, "%s(id:%d)", IRNAME(use), use->id());
    }
}
//END VMD


//
//START MDPhi
//
void MDPhi::replaceOpnd(IR * oldopnd, IR * newopnd)
{
    ASSERT0(oldopnd && newopnd);
    xcom::replace(&MDPHI_opnd_list(this), oldopnd, newopnd);
}


//Facility function to make it easier to get the VOpnd of operand of PHI.
VMD * MDPhi::getOpndVMD(IR const* opnd, UseDefMgr const* mgr) const
{
    ASSERTN(xcom::in_list(getOpndList(), opnd), ("not operand of phi"));
    if (!opnd->is_id() && opnd->isMemoryOpnd()) { return nullptr; }

    ASSERT0(mgr);
    MDSSAInfo * mdssainfo = mgr->getMDSSAInfo(opnd);
    ASSERT0(mdssainfo);
    if (mdssainfo->getVOpndSet()->get_elem_count() == 0) {
        //Note VOpndSet of 'opnd' may be empty after some optimization.
        //It does not happen when MDSSA just constructed. The USE that
        //without real-DEF will have a virtual-DEF that version is 0.
        //During some increment-maintaining of MDSSA, VOpnd may be removed,
        //and VOpndSet become empty.
        //This means the current USE, 'opnd', does not have real-DEF stmt, the
        //value of 'opnd' always coming from parameter of global value.
        //The ID of PHI should not be removed, because it is regarded
        //as a place-holder of PHI operand, and the place-holder indicates
        //the position of related predecessor of current BB of PHI in CFG.
        return nullptr;
    }

    VOpndSetIter iter = nullptr;
    VMD * vopnd = (VMD*)mgr->getVOpnd(mdssainfo->getVOpndSet()->
        get_first(&iter));
    ASSERT0(vopnd->is_md());
    ASSERT0(mdssainfo->getVOpndSet()->get_elem_count() == 1);
    return vopnd;
}


void MDPhi::dumpOpnd(IR const* opnd, IRBB const* pred, Region const* rg,
                     UseDefMgr const* mgr) const
{
    prt(rg, "(");
    switch (opnd->getCode()) {
    case IR_CONST:
        dumpConst(opnd, rg);
        break;
    case IR_LDA:
        prt(rg, "LDA");
        break;
    case IR_ID: {
        VMD * vopnd = getOpndVMD(opnd, mgr);
        if (vopnd == nullptr) {
            MD const* ref = opnd->getRefMD();
            ASSERT0(ref);
            prt(rg, "ID id:%u ????", opnd->id());
        } else {
            MD const* ref = opnd->getRefMD();
            ASSERT0(ref);
            ASSERT0(ref->id() == vopnd->mdid());
            prt(rg, "ID id:%u MD%uV%u", opnd->id(), vopnd->mdid(),
                vopnd->version());
        }
        break;
    }
    default: UNREACHABLE();
    }

    if (pred == nullptr) {
        //Predecessor is not match with PHI, error occurred.
        prt(rg, " BB??");
    } else {
        prt(rg, " BB%d", pred->id());
    }
    prt(rg, ")");
}


static void dumpUseSet(VMD const* vmd, Region * rg)
{
    ASSERT0(vmd);
    note(rg, "|USESET:");
    IRSetIter vit = nullptr;
    for (INT i = const_cast<VMD*>(vmd)->getUseSet()->get_first(&vit);
        i >= 0; i = const_cast<VMD*>(vmd)->getUseSet()->get_next(i, &vit)) {
        IR const* use = rg->getIR(i);
        ASSERT0(use && (use->isMemoryRef() || use->is_id()));
        prt(rg, "(%s id:%d) ", IRNAME(use), use->id());
    }
}


void MDPhi::dump(Region const* rg, UseDefMgr const* mgr) const
{
    ASSERT0(rg);
    ASSERT0(is_phi());
    if (!rg->isLogMgrInit()) { return; }

    List<IRBB*> preds;
    IRCFG * cfg = rg->getCFG();
    ASSERT0(cfg);
    cfg->get_preds(preds, getBB());
    IRBB * pred = preds.get_head();

    ASSERT0(getResult());
    prt(rg, "MDPhi%u: MD%uV%u <- ", id(), getResult()->mdid(),
        getResult()->version());
    for (IR const* opnd = getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        if (opnd != getOpndList()) {
            prt(rg, ", ");
        }
        dumpOpnd(opnd, pred, rg, mgr);
        pred = preds.get_next();
    }
    dumpUseSet(getResult(), const_cast<Region*>(rg));
}
//END MDPhi


//
//START UseDefMgr
//
UseDefMgr::UseDefMgr(Region * rg, MDSSAMgr * mgr) :
    m_rg(rg), m_mdssa_mgr(mgr), m_sbs_mgr(mgr->getSBSMgr())
{
    ASSERT0(m_rg && m_mdssa_mgr);

    m_md_sys = m_rg->getMDSystem();

    //Single List Core need user declared a mempool.
    m_vopnd_sc_pool = smpoolCreate(sizeof(xcom::SC<VOpnd*>) * 4,
        MEM_CONST_SIZE);
    m_phi_pool = smpoolCreate(sizeof(MDPhi) * 2, MEM_CONST_SIZE);
    m_def_pool = smpoolCreate(sizeof(MDDef) * 2, MEM_CONST_SIZE);
    m_defset_pool = smpoolCreate(sizeof(MDDefSet) * 2, MEM_CONST_SIZE);
    m_vconst_pool = smpoolCreate(sizeof(VConst)*2, MEM_CONST_SIZE);
    m_vmd_pool = smpoolCreate(sizeof(VMD)*2, MEM_CONST_SIZE);
    m_philist_pool = smpoolCreate(sizeof(MDPhiList)*2, MEM_CONST_SIZE);
    m_philist_sc_pool = smpoolCreate(sizeof(xcom::SC<MDPhi*>) * 4,
        MEM_CONST_SIZE);
    m_mdssainfo_pool = smpoolCreate(sizeof(MDSSAInfo)*2, MEM_CONST_SIZE);

    m_free_sc_list = nullptr;
    m_def_count = MDDEF_UNDEF + 1;
    m_vopnd_count = VOPND_UNDEF + 1;
    m_philist_vec.set(rg->getBBList()->get_elem_count(), 0);
}


void UseDefMgr::destroyMD2VMDVec()
{
    Vector<VMDVec*> * vec = m_map_md2vmd.getVec();
    if (vec != nullptr) {
        for (INT i = 0; i <= vec->get_last_idx(); i++) {
            Vector<VMD*> * vpv = m_map_md2vmd.get((UINT)i);
            if (vpv != nullptr) {
                delete vpv;
            }
        }
    }

    TMap<UINT, VMDVec*> * map = m_map_md2vmd.getMap();
    if (map != nullptr) {
        TMapIter<UINT, VMDVec*> iter;
        VMDVec * vmdvec;
        for (map->get_first(iter, &vmdvec);
             vmdvec != nullptr; map->get_next(iter, &vmdvec)) {
            delete vmdvec;
        }
    }
}


void UseDefMgr::cleanOrDestroy(bool is_reinit)
{
    ASSERT0(m_rg);

    for (INT i = 0; i <= m_vopnd_vec.get_last_idx(); i++) {
        VOpnd * v = m_vopnd_vec.get((UINT)i);
        if (v != nullptr && v->is_md()) {
            ((VMD*)v)->destroy();
        }
    }

    for (INT i = 0; i <= m_def_vec.get_last_idx(); i++) {
        MDDef * d = m_def_vec.get((UINT)i);
        if (d != nullptr && d->getNextSet() != nullptr) {
            d->getNextSet()->clean(*getSBSMgr());
        }
    }

    for (INT i = 0; i <= m_mdssainfo_vec.get_last_idx(); i++) {
        MDSSAInfo * info = m_mdssainfo_vec.get((UINT)i);
        if (info != nullptr) {
            info->destroy(*getSBSMgr());
        }
    }

    destroyMD2VMDVec();

    if (is_reinit) {
        m_map_md2vmd.destroy();
        m_map_md2vmd.init();
        m_philist_vec.destroy();
        m_philist_vec.init();
        m_def_vec.destroy();
        m_def_vec.init();
        m_mdssainfo_vec.destroy();
        m_mdssainfo_vec.init();
        m_vopnd_vec.destroy();
        m_vopnd_vec.init();
        m_def_count = MDDEF_UNDEF + 1;
        m_vopnd_count = VOPND_UNDEF + 1;
    }

    ASSERT0(m_vopnd_sc_pool);
    smpoolDelete(m_vopnd_sc_pool);

    ASSERT0(m_phi_pool);
    smpoolDelete(m_phi_pool);

    ASSERT0(m_def_pool);
    smpoolDelete(m_def_pool);

    ASSERT0(m_defset_pool);
    smpoolDelete(m_defset_pool);

    ASSERT0(m_vmd_pool);
    smpoolDelete(m_vmd_pool);

    ASSERT0(m_vconst_pool);
    smpoolDelete(m_vconst_pool);

    ASSERT0(m_philist_pool);
    smpoolDelete(m_philist_pool);

    ASSERT0(m_philist_sc_pool);
    smpoolDelete(m_philist_sc_pool);

    ASSERT0(m_mdssainfo_pool);
    smpoolDelete(m_mdssainfo_pool);

    if (is_reinit) {
        m_vopnd_sc_pool = smpoolCreate(sizeof(xcom::SC<VOpnd*>) * 4,
            MEM_CONST_SIZE);
        m_phi_pool = smpoolCreate(sizeof(MDPhi) * 2, MEM_CONST_SIZE);
        m_def_pool = smpoolCreate(sizeof(MDDef) * 2, MEM_CONST_SIZE);
        m_defset_pool = smpoolCreate(sizeof(MDDefSet) * 2, MEM_CONST_SIZE);
        m_vmd_pool = smpoolCreate(sizeof(VMD) * 2, MEM_CONST_SIZE);
        m_vconst_pool = smpoolCreate(sizeof(VConst)*2, MEM_CONST_SIZE);
        m_philist_pool = smpoolCreate(sizeof(MDPhiList)*2, MEM_CONST_SIZE);
        m_philist_sc_pool = smpoolCreate(sizeof(xcom::SC<MDPhi*>) * 4,
            MEM_CONST_SIZE);
        m_mdssainfo_pool = smpoolCreate(sizeof(MDSSAInfo)*4, MEM_CONST_SIZE);
    }
}


void UseDefMgr::setMDSSAInfo(IR * ir, MDSSAInfo * mdssainfo)
{
    ASSERT0(ir && mdssainfo && m_mdssa_mgr->hasMDSSAInfo(ir));
    if (ir->getAI() == nullptr) {
        IR_ai(ir) = m_rg->allocAIContainer();
    }
    IR_ai(ir)->set(mdssainfo, m_rg);
}


void UseDefMgr::cleanMDSSAInfo(IR * ir)
{
    ASSERT0(ir);
    ASSERTN(m_mdssa_mgr->hasMDSSAInfo(ir), ("make decision early"));
    if (ir->getAI() == nullptr) { return; }
    IR_ai(ir)->clean(AI_MD_SSA);
}



//Generate MDSSAInfo for individual memory-ref IR stmt/exp since each IR
//has its own specific MDSSA Memory Reference information.
//It sounds there might be some waste to memory if many IRs mdssa-reference
//could be represented by same MDSSAInfo. Nevertheless, the postulation
//is quite experimentally, and in practical very rarelly.
MDSSAInfo * UseDefMgr::genMDSSAInfo(IR * ir)
{
    ASSERT0(ir && m_mdssa_mgr->hasMDSSAInfo(ir));
    if (ir->getAI() == nullptr) {
        IR_ai(ir) = m_rg->allocAIContainer();
    }
    MDSSAInfo * mdssainfo = (MDSSAInfo*)ir->getAI()->get(AI_MD_SSA);
    if (mdssainfo == nullptr) {
        mdssainfo = allocMDSSAInfo();
        IR_ai(ir)->init();
        IR_ai(ir)->set(mdssainfo, m_rg);
    }
    return mdssainfo;
}


MDSSAInfo * UseDefMgr::getMDSSAInfo(IR const* ir)
{
    ASSERT0(ir && MDSSAMgr::hasMDSSAInfo(ir));
    if (ir->getAI() == nullptr) {
        return nullptr;
    }

    MDSSAInfo * mdssainfo = (MDSSAInfo*)ir->getAI()->get(AI_MD_SSA);
    if (mdssainfo == nullptr) {
        return nullptr;
    }

    return mdssainfo;
}


//Allocate SSAInfo for specified PR indicated by 'mdid'.
MDSSAInfo * UseDefMgr::allocMDSSAInfo()
{
    ASSERT0(m_mdssainfo_pool);
    MDSSAInfo * p = (MDSSAInfo*)smpoolMallocConstSize(sizeof(MDSSAInfo),
                                                      m_mdssainfo_pool);
    ASSERT0(p);
    ::memset(p, 0, sizeof(MDSSAInfo));
    p->init();
    ASSERT0(m_mdssainfo_vec.get_last_idx() == -1 ||
            m_mdssainfo_vec.get_last_idx() >= 0);
    m_mdssainfo_vec.set(m_mdssainfo_vec.get_last_idx() + 1, p);
    return p;
}


//Allocate MDPhi and initialize with the number of operands.
//Each operands has zero version to mdid.
MDPhi * UseDefMgr::allocMDPhi(UINT mdid, UINT num_operands)
{
    ASSERT0(mdid > MD_UNDEF && num_operands > 0);

    //Different from MDDef, MDPhi will be allocated in individual pool.
    MDPhi * phi = (MDPhi*)smpoolMallocConstSize(sizeof(MDPhi), m_phi_pool);
    phi->init();
    MDDEF_id(phi) = m_def_count++;
    m_def_vec.set(MDDEF_id(phi), phi);
    VMD const* vmd = allocVMD(mdid, MDSSA_INIT_VERSION);
    ASSERT0(vmd);

    MD const* md = m_md_sys->getMD(mdid);
    ASSERT0(md);
    IR * last = nullptr;
    ASSERT0(getSBSMgr());

    //Generate operand of PHI.
    for (UINT i = 0; i < num_operands; i++) {
        IR * opnd = m_rg->buildId(md->get_base());
        opnd->setRefMD(md, m_rg);

        //Generate MDSSAInfo to ID.
        MDSSAInfo * mdssainfo = genMDSSAInfo(opnd);

        //Add VOpnd that ID indicated.
        mdssainfo->addVOpnd(vmd, this);

        xcom::add_next(&MDPHI_opnd_list(phi), &last, opnd);

        ID_phi(opnd) = phi; //Record ID's host PHI.
    }
    return phi;
}


void UseDefMgr::removeMDDef(MDDef * def)
{
    ASSERT0(def);
    //No need to destroy def's memory overtly.
    m_def_vec.set(def->id(), nullptr);
    def->clean();
}


MDDef * UseDefMgr::allocMDDef()
{
    MDDef * def = (MDDef*)smpoolMallocConstSize(sizeof(MDDef), m_def_pool);
    def->init(false);
    MDDEF_id(def) = m_def_count++;
    m_def_vec.set(MDDEF_id(def), def);
    return def;
}


MDDefSet * UseDefMgr::allocMDDefSet()
{
    MDDefSet * defset = (MDDefSet*)smpoolMallocConstSize(sizeof(MDDefSet),
                                                         m_defset_pool);
    defset->init();
    return defset;
}


xcom::SC<VOpnd*> * UseDefMgr::allocSCVOpnd(VOpnd * opnd)
{
    xcom::SC<VOpnd*> * sc = xcom::removehead_single_list(&m_free_sc_list);
    if (sc != nullptr) {
        sc->init();
        return sc;
    }

    sc = (xcom::SC<VOpnd*>*)smpoolMallocConstSize(sizeof(xcom::SC<VOpnd*>),
                                                  m_vopnd_sc_pool);
    sc->init();
    SC_val(sc) = opnd;
    return sc;
}


VConst * UseDefMgr::allocVConst(IR const* ir)
{
    ASSERTN(m_vconst_pool, ("not init"));
    VConst * p = (VConst*)smpoolMallocConstSize(sizeof(VConst), m_vconst_pool);
    ASSERT0(p);
    ::memset(p, 0, sizeof(VConst));
    VOPND_code(p) = VOPND_CONST;
    VOPND_id(p) = m_vopnd_count++;
    VCONST_val(p) = ir;
    return p;
}


VMD * UseDefMgr::getVMD(UINT mdid, UINT version) const
{
    ASSERT0(mdid > MD_UNDEF);
    Vector<VMD*> * vec = const_cast<UseDefMgr*>(this)->m_map_md2vmd.get(mdid);
    if (vec == nullptr) {
        return nullptr;
    }
    return vec->get(version);
}


//The function remove and clean all information of 'vmd' from MDSSAMgr.
void UseDefMgr::removeVMD(VMD * vmd)
{
    ASSERT0(vmd);
    m_vopnd_vec.set(vmd->id(), nullptr);
    VMDVec * vec = m_map_md2vmd.get(vmd->mdid());
    ASSERT0(vec);
    vec->set(vmd->version(), nullptr);
    vmd->destroy();
}


//Allocate VMD and ensure it is unique according to 'version' and 'mdid'.
VMD * UseDefMgr::allocVMD(UINT mdid, UINT version)
{
    ASSERT0(mdid > MD_UNDEF);
    VMDVec * vec = m_map_md2vmd.get(mdid);
    if (vec == nullptr) {
        vec = new VMDVec();
        m_map_md2vmd.set(mdid, vec);
    }

    VMD * v = vec->get(version);
    if (v != nullptr) {
        return v;
    }

    ASSERTN(m_vmd_pool, ("not init"));
    v = (VMD*)smpoolMallocConstSize(sizeof(VMD), m_vmd_pool);
    ASSERT0(v);
    ::memset(v, 0, sizeof(VMD));
    v->init(m_rg->getMiscBitSetMgr()->getSegMgr());
    VOPND_code(v) = VOPND_MD;
    VOPND_id(v) = m_vopnd_count++;
    VMD_mdid(v) = mdid;
    VMD_version(v) = version;
    VMD_def(v) = nullptr;
    vec->set(version, v);
    m_vopnd_vec.set(v->id(), v);
    return v;
}


size_t UseDefMgr::count_mem() const
{
    size_t count = smpoolGetPoolSize(m_mdssainfo_pool);
    count += smpoolGetPoolSize(m_phi_pool);
    count += smpoolGetPoolSize(m_def_pool);
    count += smpoolGetPoolSize(m_defset_pool);
    count += smpoolGetPoolSize(m_vconst_pool);
    count += smpoolGetPoolSize(m_vmd_pool);
    count += smpoolGetPoolSize(m_philist_pool);
    count += smpoolGetPoolSize(m_philist_sc_pool);
    count += m_map_md2vmd.count_mem();
    count += m_vopnd_vec.count_mem();
    count += m_def_vec.count_mem();
    count += sizeof(UseDefMgr);
    return count;
}


MDPhiList * UseDefMgr::genBBPhiList(UINT bbid)
{
    MDPhiList * lst = m_philist_vec.get(bbid);
    if (lst != nullptr) { return lst; }

    lst = (MDPhiList*)smpoolMallocConstSize(sizeof(MDPhiList), m_philist_pool);
    ASSERT0(lst);
    lst->init(m_philist_sc_pool);
    m_philist_vec.set(bbid, lst);
    return lst;
}
//END UseDefMgr

} //namespace xoc
