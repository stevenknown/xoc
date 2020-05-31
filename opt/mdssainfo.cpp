/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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
//START MDSSAInfo
//
//Return true if all definition of vopnd can reach 'exp'.
bool MDSSAInfo::isUseReachable(IN UseDefMgr * usedefmgr, IR const* exp)
{
    ASSERT0(usedefmgr && exp && exp->is_exp());
    VOpndSetIter iter = NULL;
    for (INT i = getVOpndSet()->get_first(&iter);
         i >= 0; i = getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)usedefmgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (!vopnd->getUseSet()->is_contain(exp->id())) {
            return false;
        }
    }
    return true;
}


//Collect all USE, where USE is IR expression.
void MDSSAInfo::collectUse(OUT DefSBitSetCore * set,
                           IN UseDefMgr * usedefmgr,
                           IN DefMiscBitSetMgr * bsmgr)
{
    ASSERT0(set && usedefmgr && bsmgr);
    VOpndSetIter iter = NULL;
    Region * rg = usedefmgr->getRegion();
    for (INT i = getVOpndSet()->get_first(&iter);
         i >= 0; i = getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)usedefmgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());

        IRSetIter vit = NULL;
        for (INT i2 = vopnd->getUseSet()->get_first(&vit);
            i2 >= 0; i2 = vopnd->getUseSet()->get_next(i2, &vit)) {
            IR * use = rg->getIR(i2);
            ASSERT0(use && (use->isMemoryRef() || use->is_id()));
            set->bunion(use->id(), *bsmgr);
        }
    }
}


//Remove given IR expression from occurence set.
//exp: IR expression to be removed.
void MDSSAInfo::removeUse(IR const* exp, IN UseDefMgr * usedefmgr)
{
    ASSERT0(exp && exp->is_exp() && usedefmgr);
    VOpndSetIter iter = NULL;
    for (INT i = getVOpndSet()->get_first(&iter);
         i >= 0; i = getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)usedefmgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->getUseSet()->diff(exp->id());
    }
}


void MDSSAInfo::dump(MDSSAMgr const* mgr) const
{
    if (g_tfile == NULL) { return; }
    VOpndSetIter iter = NULL;
    MDSSAInfo * pthis = const_cast<MDSSAInfo*>(this);
    for (INT i = pthis->getVOpndSet()->get_first(&iter);
         i >= 0; i = pthis->getVOpndSet()->get_next(i, &iter)) {
        note("\nREF:");
        VMD const* vopnd = (VMD*)const_cast<MDSSAMgr*>(mgr)->
            getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->dump(mgr->getRegion(), const_cast<MDSSAMgr*>(mgr)->
            getUseDefMgr());
    }
}
//END MDSSAInfo


//
//START UINT2VMDVec
//
void UINT2VMDVec::set(UINT mdid, Vector<VMD*> * vmdvec)
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
void VMD::dump() const
{
    prt("MD%dV%d", mdid(), version());
}


void VMD::dump(Region const* rg, UseDefMgr const* mgr) const
{
    if (g_tfile == NULL) { return; }
    ASSERT0(is_md() && rg);
    prt("(MD%dV%d", mdid(), version());

    //Dump Def
    if (getDef() != NULL) {
        ASSERT0(!getDef()->is_phi());

        if (getDef()->getPrev() != NULL) {
            prt(",PrevDEF:MD%dV%d",
                getDef()->getPrev()->getResult()->mdid(),
                getDef()->getPrev()->getResult()->version());
        } else {
            prt(",-");
        }

        if (getDef()->getNextSet() != NULL) {
            MDDefSetIter nit = NULL;
            bool first = true;
            for (INT w = getDef()->getNextSet()->get_first(&nit);
                w >= 0; w = getDef()->getNextSet()->get_next(w, &nit)) {
                if (first) {
                    first = false;
                } else {
                    prt(",");
                }

                MDDef const* use = mgr->getMDDef(w);
                ASSERTN(use, ("not such MDDef"));
                ASSERT0(use->getResult());
                ASSERTN(use->getPrev() == getDef(), ("insanity relation"));
                prt(",NextDEF:MD%dV%d",
                    use->getResult()->mdid(), use->getResult()->version());
            }
        }
    } else {
        prt(",-");
    }
    prt(")");

    //Dump OccSet
    prt("|UsedBy:");
    IRSetIter vit = NULL;
    bool first = true;
    VMD * pthis = const_cast<VMD*>(this);
    for (INT i2 = pthis->getUseSet()->get_first(&vit);
        i2 >= 0; i2 = pthis->getUseSet()->get_next(i2, &vit)) {
        if (first) {
            first = false;
        } else {
            prt(",");
        }

        IR * use = rg->getIR(i2);
        ASSERT0(use && (use->isMemoryRef() || use->is_id()));
        prt("%s(id:%d)", IRNAME(use), use->id());
    }

    fflush(g_tfile);
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
    if (!opnd->is_id() && opnd->isMemoryOpnd()) { return NULL; }

    ASSERT0(mgr && mgr->getMDSSAInfo(opnd) &&
        mgr->getMDSSAInfo(opnd)->getVOpndSet()->get_elem_count() == 1);

    VOpndSetIter iter = NULL;
    VMD * vopnd = (VMD*)mgr->getVOpnd(mgr->getMDSSAInfo(opnd)->
        getVOpndSet()->get_first(&iter));
    ASSERT0(vopnd->is_md());
    return vopnd;
}


void MDPhi::dump(Region const* rg, UseDefMgr const* mgr) const
{
    ASSERT0(rg);
    ASSERT0(is_phi());
    if (g_tfile == NULL) { return; }

    List<IRBB*> preds;
    IRCFG * cfg = rg->getCFG();
    ASSERT0(cfg);
    cfg->get_preds(preds, getBB());
    IRBB * pred = preds.get_head();

    ASSERT0(getResult());
    prt("Phi: MD%dV%d <- ", getResult()->mdid(), getResult()->version());
    for (IR const* opnd = getOpndList();
         opnd != NULL; opnd = opnd->get_next()) {
        if (opnd != getOpndList()) {
            prt(", ");
        }

        switch (opnd->getCode()) {
        case IR_CONST:
            prt("Const");
            break;
        case IR_LDA:
            prt("Lda");
            break;
        case IR_ID: {
            VMD * vopnd = getOpndVMD(opnd, mgr);
            prt("MD%dV%d(id:%d)", vopnd->mdid(), vopnd->version(), opnd->id());
            break;
        }
        default: UNREACHABLE();
        }

        ASSERT0(pred);
        prt("(BB%d)", pred->id());
        pred = preds.get_next();
    }

    VMD * res = getResult();
    ASSERT0(res);
    prt("|UsedBy:");
    IRSetIter vit = NULL;
    bool first = true;
    for (INT i2 = res->getUseSet()->get_first(&vit);
        i2 >= 0; i2 = res->getUseSet()->get_next(i2, &vit)) {
        if (first) {
            first = false;
        } else {
            prt(",");
        }

        IR const* use = rg->getIR(i2);
        ASSERT0(use && (use->isMemoryRef() || use->is_id()));
        prt("%s(id:%d)", IRNAME(use), use->id());
    }

    fflush(g_tfile);
}
//END MDPhi


//
//START UseDefMgr
//
UseDefMgr::UseDefMgr(Region * rg, MDSSAMgr * mgr) : m_rg(rg), m_mdssa_mgr(mgr)
{
    ASSERT0(m_rg && m_mdssa_mgr);

    m_md_sys = m_rg->getMDSystem();
    m_sbs_mgr = m_rg->getMiscBitSetMgr();

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

    m_free_sc_list = NULL;
    m_def_count = 1;
    m_vopnd_count = 1;
}


void UseDefMgr::destroyMD2VMDVec()
{
    Vector<Vector<VMD*>*> * vec = m_map_md2vmd.getVec();
    if (vec != NULL) {
        for (INT i = 0; i <= vec->get_last_idx(); i++) {
            Vector<VMD*> * vpv = m_map_md2vmd.get((UINT)i);
            if (vpv != NULL) {
                delete vpv;
            }
        }
    }

    TMap<UINT, Vector<VMD*>*> * map = m_map_md2vmd.getMap();
    if (map != NULL) {
        TMapIter<UINT, Vector<VMD*>*> iter;
        Vector<VMD*> * vmdvec;
        for (map->get_first(iter, &vmdvec);
             vmdvec != NULL; map->get_next(iter, &vmdvec)) {
            delete vmdvec;
        }
    }
}


void UseDefMgr::cleanOrDestroy(bool is_reinit)
{
    ASSERT0(m_rg);

    for (INT i = 0; i <= m_vopnd_vec.get_last_idx(); i++) {
        VOpnd * v = m_vopnd_vec.get((UINT)i);
        if (v != NULL && v->is_md()) {
            ((VMD*)v)->destroy();
        }
    }

    for (INT i = 0; i <= m_def_vec.get_last_idx(); i++) {
        MDDef * d = m_def_vec.get((UINT)i);
        if (d != NULL && d->getNextSet() != NULL) {
            d->getNextSet()->clean(*m_sbs_mgr);
        }
    }

    for (INT i = 0; i <= m_mdssainfo_vec.get_last_idx(); i++) {
        MDSSAInfo * info = m_mdssainfo_vec.get((UINT)i);
        if (info != NULL) {
            info->destroy(*m_sbs_mgr);
        }
    }

    destroyMD2VMDVec();

    if (is_reinit) {
        m_map_md2vmd.destroy();
        m_map_md2vmd.init();
        m_philist_vec.clean();
        m_def_vec.clean();
        m_mdssainfo_vec.clean();
        m_vopnd_vec.clean();
        m_def_count = 1;
        m_vopnd_count = 1;
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
    if (ir->getAI() == NULL) {
        IR_ai(ir) = m_rg->allocAIContainer();
    }
    IR_ai(ir)->set(mdssainfo);
}


//Generate MDSSAInfo for individual memory-ref IR stmt/exp since each IR
//has its own specific MDSSA Memory Reference information.
//It sounds there might be some waste to memory if many IRs mdssa-reference
//could be represented by same MDSSAInfo. Nevertheless, the postulation
//is quite experimentally, and in practical very rarelly.
MDSSAInfo * UseDefMgr::genMDSSAInfo(IR * ir)
{
    ASSERT0(ir && m_mdssa_mgr->hasMDSSAInfo(ir));
    if (ir->getAI() == NULL) {
        IR_ai(ir) = m_rg->allocAIContainer();
    }
    MDSSAInfo * mdssainfo = (MDSSAInfo*)ir->getAI()->get(AI_MD_SSA);
    if (mdssainfo == NULL) {
        mdssainfo = allocMDSSAInfo();
        IR_ai(ir)->set(mdssainfo);
    }
    return mdssainfo;
}


MDSSAInfo * UseDefMgr::getMDSSAInfo(IR const* ir) const
{
    ASSERT0(ir && m_mdssa_mgr->hasMDSSAInfo(ir));
    if (ir->getAI() == NULL) {
        return NULL;
    }

    MDSSAInfo * mdssainfo = (MDSSAInfo*)ir->getAI()->get(AI_MD_SSA);
    if (mdssainfo == NULL) {
        return NULL;
    }

    return mdssainfo;
}


//Allocate SSAInfo for specified PR indicated by 'mdid'.
MDSSAInfo * UseDefMgr::allocMDSSAInfo()
{
    ASSERT0(m_mdssainfo_pool);
    MDSSAInfo * p = (MDSSAInfo*)smpoolMallocConstSize(
        sizeof(MDSSAInfo), m_mdssainfo_pool);
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
    ASSERT0(mdid > 0 && num_operands > 0);

    MDPhi * phi = (MDPhi*)smpoolMallocConstSize(sizeof(MDPhi), m_phi_pool);
    phi->init();
    MDDEF_id(phi) = m_def_count++;
    m_def_vec.set(MDDEF_id(phi), phi);
    VMD const* vmd = allocVMD(mdid, 0);
    ASSERT0(vmd);

    MD const* md = m_md_sys->getMD(mdid);
    ASSERT0(md);
    IR * last = NULL;
    for (UINT i = 0; i < num_operands; i++) {
        IR * opnd = m_rg->buildId(md->get_base());
        opnd->setRefMD(md, m_rg);

        MDSSAInfo * mdssainfo = genMDSSAInfo(opnd);

        ASSERT0(m_sbs_mgr);
        mdssainfo->getVOpndSet()->append(vmd, *m_sbs_mgr);

        xcom::add_next(&MDPHI_opnd_list(phi), &last, opnd);

        ID_phi(opnd) = phi;
    }
    return phi;
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
    MDDefSet * defset = (MDDefSet*)smpoolMallocConstSize(
        sizeof(MDDefSet), m_defset_pool);
    defset->init();
    return defset;
}


xcom::SC<VOpnd*> * UseDefMgr::allocSCVOpnd(VOpnd * opnd)
{
    xcom::SC<VOpnd*> * sc = xcom::removehead_single_list(&m_free_sc_list);
    if (sc != NULL) {
        sc->init();
        return sc;
    }

    sc = (xcom::SC<VOpnd*>*)smpoolMallocConstSize(
        sizeof(xcom::SC<VOpnd*>), m_vopnd_sc_pool);
    sc->init();
    SC_val(sc) = opnd;
    return sc;
}


VConst * UseDefMgr::allocVConst(IR const* ir)
{
    ASSERTN(m_vconst_pool, ("not init"));
    VConst * p = (VConst*)smpoolMallocConstSize(
        sizeof(VConst), m_vconst_pool);
    ASSERT0(p);
    ::memset(p, 0, sizeof(VConst));
    VOPND_code(p) = VOPND_CONST;
    VOPND_id(p) = m_vopnd_count++;
    VCONST_val(p) = ir;
    return p;
}


VMD * UseDefMgr::getVMD(UINT mdid, UINT version) const
{
    ASSERT0(mdid > 0);
    Vector<VMD*> * vec = const_cast<UseDefMgr*>(this)->m_map_md2vmd.get(mdid);
    if (vec == NULL) {
        return NULL;
    }
    return vec->get(version);
}


//Allocate VMD and ensure it is unique according to 'version' and 'mdid'.
VMD * UseDefMgr::allocVMD(UINT mdid, UINT version)
{
    ASSERT0(mdid > 0);
    Vector<VMD*> * vec = m_map_md2vmd.get(mdid);
    if (vec == NULL) {
        vec = new Vector<VMD*>();
        m_map_md2vmd.set(mdid, vec);
    }

    VMD * v = vec->get(version);
    if (v != NULL) {
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
    VMD_def(v) = NULL;
    vec->set(version, v);
    m_vopnd_vec.set(v->id(), v);
    return v;
}


size_t UseDefMgr::count_mem()
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
    if (lst != NULL) { return lst; }

    lst = (MDPhiList*)smpoolMallocConstSize(sizeof(MDPhiList), m_philist_pool);
    ASSERT0(lst);
    lst->init(m_philist_sc_pool);
    m_philist_vec.set(bbid, lst);
    return lst;
}
//END UseDefMgr

} //namespace xoc
