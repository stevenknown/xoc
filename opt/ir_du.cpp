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
#include "callg.h"
#include "prdf.h"
#include "prssainfo.h"
#include "ir_ssa.h"

namespace xoc {

//Set the interal data attribute to no sparse
//if you think the analysis objects are few enough,
//and no-sparse set may speed up compilation.
#define SOL_SET_IS_SPARSE    (true)

//Iterative methodology.
//#define WORK_LIST_DRIVE

#define CK_UNKNOWN        0 //Can not determine if memory is overlap.
#define CK_OVERLAP        1 //Can be confirmed memory is overlap.
#define CK_NOT_OVERLAP    2 //Can be confirmed memory is not overlap.

//
//START MDId2IRlist
//
MDId2IRlist::MDId2IRlist(Region * rg)
{
    m_ru = rg;
    m_md_sys = rg->getMDSystem();
    m_tm = rg->getTypeMgr();
    m_du = rg->getDUMgr();
    m_misc_bs_mgr = rg->getMiscBitSetMgr();
    m_are_stmts_defed_ineffect_md = false;
}


MDId2IRlist::~MDId2IRlist()
{
    TMapIter<UINT, DefSBitSetCore*> c;
    DefSBitSetCore * mapped = NULL;
    for (UINT mdid = get_first(c, &mapped);
         mdid != MD_UNDEF; mdid = get_next(c, &mapped)) {
        ASSERT0(mapped);
        mapped->clean(*m_misc_bs_mgr);
        delete mapped;
    }

    m_global_md.clean(*m_misc_bs_mgr);
}


void MDId2IRlist::clean()
{
    TMapIter<UINT, DefSBitSetCore*> c;
    DefSBitSetCore * mapped = NULL;
    for (UINT mdid = get_first(c, &mapped);
         mdid != MD_UNDEF; mdid = get_next(c, &mapped)) {
        ASSERT0(mapped);
        mapped->clean(*m_misc_bs_mgr);
    }

    m_global_md.clean(*m_misc_bs_mgr);
    m_are_stmts_defed_ineffect_md = false;

    //Do not clean DefSBitSet* here, it will incur memory leak.
    //TMap<UINT, DefSBitSetCore*>::clean();
}


//'md' corresponds to unique 'ir'.
void MDId2IRlist::set(UINT mdid, IR * ir)
{
    ASSERT(mdid != MD_GLOBAL_MEM &&
           mdid != MD_FULL_MEM &&
           mdid != MD_IMPORT_VAR,
        ("there is not any md could kill Fake-May-MD."));
    ASSERT0(ir);
    DefSBitSetCore * irtab = TMap<UINT, DefSBitSetCore*>::get(mdid);
    if (irtab == NULL) {
        irtab = new DefSBitSetCore();
        TMap<UINT, DefSBitSetCore*>::set(mdid, irtab);
    } else {
        irtab->clean(*m_misc_bs_mgr);
    }
    irtab->bunion(ir->id(), *m_misc_bs_mgr);
}


//'md' corresponds to multiple 'ir'.
void MDId2IRlist::append(UINT mdid, UINT irid)
{
    DefSBitSetCore * irtab = TMap<UINT, DefSBitSetCore*>::get(mdid);
    if (irtab == NULL) {
        irtab = new DefSBitSetCore();
        TMap<UINT, DefSBitSetCore*>::set(mdid, irtab);
    }
    irtab->bunion(irid, *m_misc_bs_mgr);
}


void MDId2IRlist::dump()
{
    if (g_tfile == NULL) { return; }
    m_md_sys->dumpAllMD();
    fprintf(g_tfile, "\n==-- DUMP MDID2IRLIST --==");
    TMapIter<UINT, DefSBitSetCore*> c;
    for (UINT mdid = get_first(c); mdid != MD_UNDEF; mdid = get_next(c)) {
        MD const * md = m_md_sys->getMD(mdid);
        md->dump(m_md_sys->getTypeMgr());
        DefSBitSetCore * irs = get(mdid);
        if (irs == NULL || irs->get_elem_count() == 0) { continue; }
        SEGIter * sc = NULL;
        for (INT i = irs->get_first(&sc); i >= 0; i = irs->get_next(i, &sc)) {
            IR * d = m_ru->getIR(i);
            g_indent = 4;
            dump_ir(d, m_tm, NULL, false, false, false);

            fprintf(g_tfile, "\n\t\tdef:");
            MDSet const* ms = m_du->getMayDef(d);
            MD const* m = m_du->get_must_def(d);

            if (m != NULL) {
                fprintf(g_tfile, "MMD%d", MD_id(m));
            }

            if (ms != NULL) {
                SEGIter * iter;
                for (INT j = ms->get_first(&iter);
                     j >= 0; j = ms->get_next(j, &iter)) {
                    fprintf(g_tfile, " MD%d", j);
                }
            }
        }
    }
    fflush(g_tfile);
}
//END MDId2IRlist


//
//START IR_DU_MGR
//
IR_DU_MGR::IR_DU_MGR(Region * rg)
{
    m_ru = rg;
    m_tm = rg->getTypeMgr();
    m_md_sys = rg->getMDSystem();
    m_aa = rg->getAA();
    m_cfg = rg->getCFG();
    m_mds_mgr = rg->getMDSetMgr();
    m_mds_hash = rg->getMDSetHash();
    m_pool = smpoolCreate(sizeof(DUSet) * 2, MEM_COMM);
    m_is_init = NULL;
    m_md2irs = NULL;

    //NOTE: call clean() for each object which
    //inheirted from SBitSet or SBitSetCore while destructing the object.
    //That will free SEG back to MiscBitSetMgr.
    m_misc_bs_mgr = rg->getMiscBitSetMgr();

    ASSERT0(m_aa && m_cfg && m_md_sys && m_tm && m_mds_mgr && m_mds_hash);
}


IR_DU_MGR::~IR_DU_MGR()
{
    //Note you must ensure all ir DUSet and MDSet are freed back to
    //m_misc_bs_mgr before Region destructed.
    //Destruct IR_DU_MGR will NOT free DUSet which has been allocated for IR.

    ASSERT0(m_is_init == NULL);
    ASSERT0(m_md2irs == NULL);
    resetGlobalSet(false);
    smpoolDelete(m_pool);

    //Explicitly free SEG to DefSegMgr, or it will complained during destruction.
    m_is_cached_mdset.clean(*m_misc_bs_mgr);
}


//Compute the overlapping MDSet that might overlap ones which 'ir' referred.
//Then set the MDSet to be ir's may-referred MDSet.
//
//e.g: int A[100], there are two referrence of array A: A[i], A[j]
//    A[i] might overlap A[j].
//recompute: true to compute overlapping MDSet even if it has cached.
void IR_DU_MGR::computeOverlapUseMDSet(IR * ir, bool recompute)
{
    bool has_init = false;
    MD const* md = ir->getRefMD();
    MDSet tmpmds;
    if (md != NULL) {
        if (MD_id(md) == MD_GLOBAL_MEM ||
            MD_id(md) == MD_IMPORT_VAR ||
            MD_id(md) == MD_FULL_MEM) {
            return;
        }

        //Compute overlapped md set for must-ref.
        has_init = true;
        if (recompute || !m_is_cached_mdset.is_contain(MD_id(md))) {
            m_md_sys->computeOverlap(m_ru, md, tmpmds,
                m_tab_iter, *m_misc_bs_mgr, true);

            MDSet const* newmds = m_mds_hash->append(tmpmds);
            if (newmds != NULL) {
                m_cached_overlap_mdset.set(md, newmds);
            }

            m_is_cached_mdset.bunion(MD_id(md), *m_misc_bs_mgr);
        } else {
            MDSet const* hashed = m_cached_overlap_mdset.get(md);
            if (hashed != NULL) {
                tmpmds.copy(*hashed, *m_misc_bs_mgr);
            } else {
                tmpmds.clean(*m_misc_bs_mgr);
            }
        }
    }

    //Compute overlapped md set for may-ref, may-ref may contain several MDs.
    MDSet const* mds = ir->getRefMDSet();
    if (mds != NULL) {
        if (!has_init) {
            has_init = true;
            tmpmds.copy(*mds, *m_misc_bs_mgr);
        } else {
            tmpmds.bunion_pure(*mds, *m_misc_bs_mgr);
        }

        m_md_sys->computeOverlap(m_ru, *mds, tmpmds,
            m_tab_iter, *m_misc_bs_mgr, true);
    }

    if (!has_init || tmpmds.is_empty()) {
        ir->cleanRefMDSet();
        tmpmds.clean(*m_misc_bs_mgr);
        return;
    }

    MDSet const* newmds = m_mds_hash->append(tmpmds);
    ASSERT0(newmds);
    ir->setRefMDSet(newmds, m_ru);
    tmpmds.clean(*m_misc_bs_mgr);
}


//Return IR stmt-id set.
DefDBitSetCore * IR_DU_MGR::getMayGenDef(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_may_gen_def.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_may_gen_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * IR_DU_MGR::getMustGenDef(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_must_gen_def.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_must_gen_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * IR_DU_MGR::getAvailInReachDef(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_avail_in_reach_def.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_avail_in_reach_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * IR_DU_MGR::getAvailOutReachDef(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_avail_out_reach_def.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_avail_out_reach_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * IR_DU_MGR::getInReachDef(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_in_reach_def.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_in_reach_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore * IR_DU_MGR::getOutReachDef(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_out_reach_def.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_out_reach_def.set(bbid, set);
    }
    return set;
}


DefDBitSetCore const* IR_DU_MGR::getMustKilledDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_bb_must_killed_def.get(bbid);
}


void IR_DU_MGR::setMustKilledDef(UINT bbid, DefDBitSetCore const* set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_bb_must_killed_def.set(bbid, set);
}


DefDBitSetCore const* IR_DU_MGR::getMayKilledDef(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_bb_may_killed_def.get(bbid);
}


void IR_DU_MGR::setMayKilledDef(UINT bbid, DefDBitSetCore const* set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_bb_may_killed_def.set(bbid, set);
}


//Return IR expression-id set.
DefDBitSetCore * IR_DU_MGR::getGenIRExpr(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_gen_exp.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_gen_exp.set(bbid, set);
    }
    return set;
}


//Return IR expression-id set.
DefDBitSetCore const* IR_DU_MGR::getKilledIRExpr(UINT bbid)
{
    ASSERT0(m_cfg->getBB(bbid));
    return m_bb_killed_exp.get(bbid);
}


//Return IR expression-id set.
void IR_DU_MGR::setKilledIRExpr(UINT bbid, DefDBitSetCore const* set)
{
    ASSERT0(m_cfg->getBB(bbid));
    m_bb_killed_exp.set(bbid, set);
}


//Return livein set for IR expression. Each element in the set is IR id.
DefDBitSetCore * IR_DU_MGR::getAvailInExpr(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_availin_exp.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_availin_exp.set(bbid, set);
    }
    return set;
}


//Return liveout set for IR expression. Each element in the set is IR id.
DefDBitSetCore * IR_DU_MGR::getAvailOutExpr(UINT bbid, DefMiscBitSetMgr * mgr)
{
    ASSERT0(m_cfg->getBB(bbid));
    DefDBitSetCore * set = m_bb_availout_ir_expr.get(bbid);
    if (set == NULL && mgr != NULL) {
        set = mgr->allocDBitSetCore();
        set->set_sparse(SOL_SET_IS_SPARSE);
        m_bb_availout_ir_expr.set(bbid, set);
    }
    return set;
}


//Allocate DUSet for memory reference.
DUSet * IR_DU_MGR::getAndAllocDUSet(IR * ir)
{
    ASSERT0(ir->isContainMemRef());
    DU * du = ir->getDU();
    if (du == NULL) {
        du = m_ru->allocDU();
        ir->setDU(du);
    }

    DUSet * dus = DU_duset(du);
    if (dus == NULL) {
        //Alloc DUSet from Region's MiscBitSetMgr.
        dus = (DUSet*)m_misc_bs_mgr->allocSBitSetCore();
        DU_duset(du) = dus;
    }
    return dus;
}


//Return true if 'def_stmt' is the exact and unique reach-definition
//to the operands of 'use_stmt', otherwise return false.
//
//'def_stmt': should be stmt.
//'use_stmt': should be stmt.
bool IR_DU_MGR::isExactAndUniqueDef(IR const* def, IR const* exp)
{
    ASSERT0(def->is_stmt() && exp->is_exp());
    MD const* def_md = def->getExactRef();
    if (def_md == NULL) { return false; }
    ASSERT0(def->readDUSet()); //At least contains 'exp'.

    MD const* use_md = exp->getExactRef();
    DUSet const* defset = exp->readDUSet();
    if (defset == NULL) { return false; }
    if (use_md == def_md && hasSingleDefToMD(*defset, def_md)) {
        return true;
    }
    return false;
}


//Find and return the exactly and unique stmt that defined 'exp',
//otherwise return NULL.
//'exp': should be exp, and it's use-md must be exact.
IR const* IR_DU_MGR::getExactAndUniqueDef(IR const* exp)
{
    MD const* use_md = exp->getExactRef();
    if (use_md == NULL) { return NULL; }

    DUSet const* defset = exp->readDUSet();
    if (defset == NULL) { return NULL; }

    DUIter di = NULL;
    INT d1 = defset->get_first(&di);
    INT d2 = defset->get_next(d1, &di);
    if (d1 < 0 || (d1 >=0 && d2 >= 0)) { return NULL; }

    IR const* d1ir = m_ru->getIR(d1);
    if (d1ir->isExactDef(use_md)) {
        return d1ir;
    }
    return NULL;
}


//Return true if there is only one stmt in 'defset' modify 'md'.
//Modification include both exact and inexact.
bool IR_DU_MGR::hasSingleDefToMD(DUSet const& defset, MD const* md) const
{
    UINT count = 0;
    IR_DU_MGR * pthis = const_cast<IR_DU_MGR*>(this);
    DUIter di = NULL;
    for (INT i = defset.get_first(&di); i >= 0; i = defset.get_next(i, &di)) {
        IR * def = m_ru->getIR(i);
        if (pthis->get_must_def(def) == md) {
            count++;
        } else {
            MDSet const* def_mds = pthis->getMayDef(def);
            if (def_mds == NULL) { continue; }
            if (def_mds->is_contain(md)) {
                count++;
            }
        }
    }
    return count == 1;
}


//Return true if 'def1' exactly modified md that 'def2' generated.
//'def1': should be stmt.
//'def2': should be stmt.
bool IR_DU_MGR::is_must_kill(IR const* def1, IR const* def2)
{
    ASSERT0(def1->is_stmt() && def2->is_stmt());
    if ((def1->isWritePR() || def1->isCallStmt()) &&
        (def2->isWritePR() || def2->isCallStmt())) {
        if ((def1->isCallStmt() && !def1->hasReturnValue()) ||
            (def2->isCallStmt() && !def2->hasReturnValue())) {
            return false;
        }
        return def1->get_prno() == def2->get_prno();
    }

    MD const* md1 = def1->getExactRef();
    MD const* md2 = def2->getExactRef();
    if (md1 != NULL && md2 != NULL && md1 == md2)  {
        return true;
    }
    return false;
}


bool IR_DU_MGR::is_stpr_may_def(IR const* def, IR const* use, bool is_recur)
{
    ASSERT0(def->is_stpr());
    UINT prno = STPR_no(def);
    if (is_recur) {
        m_citer.clean();
        for (IR const* x = iterInitC(use, m_citer);
             x != NULL; x = iterNextC(m_citer)) {
            if (!x->is_pr()) { continue; }
            if (PR_no(x) == prno) { return true; }
        }
    } else if (use->is_pr() && PR_no(use) == prno) {
        return true;
    }
    return false;
}


static bool is_call_may_def_core(
            IR const* call,
            IR const* use,
            MDSet const* call_maydef)
{
    //MD of use may be exact or inexact.
    MD const* use_md = use->getEffectRef();
    MDSet const* use_mds = use->getRefMDSet();
    if (call->isExactDef(use_md, use_mds)) {
        return true;
    }

    if (call_maydef != NULL) {
        if (use_md != NULL && call_maydef->is_contain(use_md)) {
            return true;
        }

        if (use_mds != NULL &&
            (use_mds == call_maydef || call_maydef->is_intersect(*use_mds))) {
            return true;
        }
    }
    return false;
}


bool IR_DU_MGR::is_call_may_def(IR const* call, IR const* use, bool is_recur)
{
    ASSERT0(call->isCallStmt());

    //MayDef of stmt must involved the overlapped MD with Must Reference,
    //but except Must Reference itself.
    MDSet const* call_maydef = getMayDef(call);
    if (is_recur) {
        m_citer.clean();
        for (IR const* x = iterInitC(use, m_citer);
             x != NULL; x = iterNextC(m_citer)) {
            if (!x->isMemoryOpnd()) { continue; }

            if (is_call_may_def_core(call, x, call_maydef)) {
                return true;
            }
        }
        return false;
    }

    ASSERT0(use->isMemoryOpnd());
    return is_call_may_def_core(call, use, call_maydef);
}


//Return true if 'def' may or must modify md-set that 'use' referenced.
//'def': must be stmt.
//'use': must be expression.
//'is_recur': true if one intend to compute the mayuse MDSet to walk
//    through IR tree recusively.
bool IR_DU_MGR::is_may_def(IR const* def, IR const* use, bool is_recur)
{
    ASSERT0(def->is_stmt() && use->is_exp());
    if (def->is_stpr()) {
        return is_stpr_may_def(def, use, is_recur);
    }
    if (def->isCallStmt()) {
        return is_call_may_def(def, use, is_recur);
    }

    MD const* mustdef = get_must_def(def);
    MDSet const* maydef = getMayDef(def);
    if (is_recur) {
        m_citer.clean();
        for (IR const* x = iterInitC(use, m_citer);
             x != NULL; x = iterNextC(m_citer)) {
            if (!x->isMemoryOpnd()) { continue; }

            MD const* mustuse = get_effect_use_md(x);
            if (mustuse != NULL) {
                if ((mustdef != NULL && mustdef == mustuse) ||
                    (maydef != NULL && maydef->is_contain(mustuse))) {
                    return true;
                }
            }

            MDSet const* mayuse = getMayUse(x);
            if (mayuse != NULL) {
                if ((mustdef != NULL && mayuse->is_contain(mustdef)) ||
                    (maydef != NULL &&
                     (maydef == mayuse || mayuse->is_intersect(*maydef)))) {
                    return true;
                }
            }
        }
        return false;
    }

    MD const* mustuse = get_effect_use_md(use);
    if (mustuse != NULL) {
        if ((mustdef != NULL && mustdef == mustuse) ||
            (maydef != NULL && maydef->is_contain(mustuse))) {
            return true;
        }
    }

    MDSet const* mayuse = getMayUse(use);
    if (mayuse != NULL) {
        if ((mustdef != NULL && mayuse->is_contain(mustdef)) ||
            (maydef != NULL && mayuse->is_intersect(*maydef))) {
            return true;
        }
    }
    return false;
}


//Return true if 'def1' may modify md-set that 'def2' generated.
//'def1': should be stmt.
//'def2': should be stmt.
bool IR_DU_MGR::is_may_kill(IR const* def1, IR const* def2)
{
    ASSERT0(def1->is_stmt() && def2->is_stmt());
    if (def1->is_stpr() && def2->is_stpr() && STPR_no(def1) == STPR_no(def2)) {
        return true;
    }

    MD const* md1 = get_must_def(def1);
    MDSet const* mds1 = getMayDef(def1);
    MD const* md2 = get_must_def(def2);
    MDSet const* mds2 = getMayDef(def2);

    if (md1 != NULL) {
        if (md2 != NULL && md1 == md2) {
            return true;
        }
        if (mds2 != NULL && (mds1 == mds2 || mds2->is_contain(md1))) {
            return true;
        }
        return false;
    }

    if (mds1 != NULL) {
        if (md2 != NULL && mds1->is_contain(md2)) {
            return true;
        }
        if (mds2 != NULL && (mds2 == mds1 || mds1->is_intersect(*mds2))) {
            return true;
        }
    }
    return false;
}


void IR_DU_MGR::computeArrayRef(
        IR * ir,
        OUT MDSet * ret_mds,
        UINT compflag,
        UINT duflag)
{
    ASSERT0(ir->isArrayOp());

    if (HAVE_FLAG(duflag, COMPUTE_NOPR_DU)) {
        ASSERT0((ir->getRefMDSet() && !ir->getRefMDSet()->is_empty()) ||
                 ir->getRefMD());
    }

    if (HAVE_FLAG(compflag, COMP_EXP_RECOMPUTE)) {
        if (HAVE_FLAG(duflag, COMPUTE_NOPR_DU)) {
            computeOverlapUseMDSet(ir, false);
        }

        //Compute referred MDs to subscript expression.
        for (IR * s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
            computeExpression(s, ret_mds, compflag, duflag);
        }
        computeExpression(ARR_base(ir), ret_mds, compflag, duflag);
    } else if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
        for (IR * s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
            computeExpression(s, ret_mds, compflag, duflag);
        }
        computeExpression(ARR_base(ir), ret_mds, compflag, duflag);

        if (HAVE_FLAG(duflag, COMPUTE_NOPR_DU) && ir->is_array()) {
            //USE-MDs may be NULL, if array base is NOT an LDA(ID).
            //e.g: given (*p)[1], p is pointer that point to an array.
            MD const* use = ir->getExactRef();
            if (use != NULL) {
                ret_mds->bunion(use, *m_misc_bs_mgr);
            }
        }
    }
}


//Walk through IR tree to collect referrenced MD.
//'ret_mds': In COMP_EXP_RECOMPUTE mode, it is used as tmp;
//  and in COMP_EXP_COLLECT_MUST_USE mode, it is
//  used to collect MUST-USE MD.
void IR_DU_MGR::computeExpression(
        IR * ir,
        OUT MDSet * ret_mds,
        UINT compflag,
        UINT duflag)
{
    if (ir == NULL) { return; }
    ASSERT0(ir->is_exp());
    switch (ir->get_code()) {
    case IR_CONST: break;
    case IR_ID:
        if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            UNREACH();
        }
        break;
    case IR_LD:
        if (HAVE_FLAG(compflag, COMP_EXP_RECOMPUTE)) {
            if (HAVE_FLAG(duflag, COMPUTE_NOPR_DU)) {
                //The result of ref info should be avaiable.
                ASSERT0(ir->getRefMD());

                //e.g: struct {int a;} s;
                //s = ...
                //s.a = ...
                //Where s and s.a is overlapped.

                computeOverlapUseMDSet(ir, false);
            }
        } else if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            MD const* t = ir->getRefMD();
            ASSERT0(t);
            if (t->is_exact()) {
                ret_mds->bunion_pure(MD_id(t), *m_misc_bs_mgr);
            }
        }
        break;
    case IR_ILD:
        if (HAVE_FLAG(compflag, COMP_EXP_RECOMPUTE)) {
            //Sideeffect information should have been computed by AA.
            //e.g: ... = ild(ld(p)) //p->a, p->b
            //mayref of ild is: {a,b}, and mustref is NULL.
            //mustref of ld is: {p}, and mayref is NULL.
            computeExpression(ILD_base(ir), ret_mds, compflag, duflag);
            if (HAVE_FLAG(duflag, COMPUTE_NOPR_DU)) {
                computeOverlapUseMDSet(ir, false);
            }
        } else if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            MD * use;
            MDSet const* use_mds = ir->getRefMDSet();
            if (use_mds != NULL &&
                (use = use_mds->get_exact_md(m_md_sys)) != NULL) {
                ret_mds->bunion(use, *m_misc_bs_mgr);
            }
            computeExpression(ILD_base(ir), ret_mds, compflag, duflag);
        }
        break;
    case IR_LDA:
        if (HAVE_FLAG(compflag, COMP_EXP_RECOMPUTE)) {
            //LDA do NOT reference any MD.
            //e.g: p=&a; the stmt do not reference MD 'a',
            //just only reference a's address.

            //The result of MD ref should be avaiable.

            //We do not need MD or MDSET information of IR_ID.
            //ASSERT0(ir->getRefMD());
            ASSERT0(ir->getRefMD() == NULL);
            ASSERT0(ir->getRefMDSet() == NULL);
        } else if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            UNREACH();
        }
        break;
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        //Binary operation.
        computeExpression(BIN_opnd0(ir), ret_mds, compflag, duflag);
        computeExpression(BIN_opnd1(ir), ret_mds, compflag, duflag);
        ASSERT0(ir->getDU() == NULL);
        break;
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
        computeExpression(UNA_opnd(ir), ret_mds, compflag, duflag);
        ASSERT0(ir->getDU() == NULL);
        break;
    case IR_LABEL:
        break;
    case IR_ARRAY:
        computeArrayRef(ir, ret_mds, compflag, duflag);
        break;
    case IR_CVT:
        computeExpression(CVT_exp(ir), ret_mds, compflag, duflag);
        break;
    case IR_PR:
        ASSERT0(ir->getRefMDSet() == NULL);
        if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            MD const* t = ir->getRefMD();
            ASSERT0(t);
            ret_mds->bunion(t, *m_misc_bs_mgr);
        }
        break;
    case IR_SELECT:
        computeExpression(SELECT_pred(ir), ret_mds, compflag, duflag);
        computeExpression(SELECT_trueexp(ir), ret_mds, compflag, duflag);
        computeExpression(SELECT_falseexp(ir), ret_mds, compflag, duflag);
        break;
    default: ASSERT(0, ("Unsupport IR code"));
    }
}


void IR_DU_MGR::dumpDUGraph(CHAR const* name, bool detail)
{
    if (g_tfile == NULL) { return; }
    Graph dug;
    dug.set_unique(false);

    class _EDGEINFO {
    public:
        int id;
    };

    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {

            dug.addVertex(ir->id());
            DUSet const* useset = ir->readDUSet();
            if (useset == NULL) { continue; }

            DUIter di = NULL;
            for (INT i = useset->get_first(&di);
                 i >= 0; i = useset->get_next(i, &di)) {
                IR * u = m_ru->getIR(i);
                ASSERT0(u->is_exp());
                IR const* ustmt = u->get_stmt();
                Edge * e = dug.addEdge(ir->id(), IR_id(ustmt));
                _EDGEINFO * ei = (_EDGEINFO*)xmalloc(sizeof(_EDGEINFO));
                ei->id = i;
                EDGE_info(e) = ei;
            }
        }
    }

    if (name == NULL) {
        name = "graph_du.vcg";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERT(h != NULL, ("%s create failed!!!",name));

    FILE * old;
    //Print comment
    //fprintf(h, "\n/*");
    //old = g_tfile;
    //g_tfile = h;
    //dumpBBList(m_ru->getBBList(), m_ru);
    //g_tfile = old;
    //fprintf(h, "\n*/\n");

    fprintf(h, "graph: {"
              "title: \"Graph\"\n"
              "shrink:  15\n"
              "stretch: 27\n"
              "layout_downfactor: 1\n"
              "layout_upfactor: 1\n"
              "layout_nearfactor: 1\n"
              "layout_splinefactor: 70\n"
              "spreadlevel: 1\n"
              "treefactor: 0.500000\n"
              "node_alignment: center\n"
              "orientation: top_to_bottom\n"
              "late_edge_labels: no\n"
              "display_edge_labels: yes\n"
              "dirty_edge_labels: no\n"
              "finetuning: no\n"
              "nearedges: no\n"
              "splines: yes\n"
              "ignoresingles: no\n"
              "straight_phase: no\n"
              "priority_phase: no\n"
              "manhatten_edges: no\n"
              "smanhatten_edges: no\n"
              "port_sharing: no\n"
              "crossingphase2: yes\n"
              "crossingoptimization: yes\n"
              "crossingweight: bary\n"
              "arrow_mode: free\n"
              "layoutalgorithm: mindepthslow\n"
              "node.borderwidth: 2\n"
              "node.color: lightcyan\n"
              "node.textcolor: black\n"
              "node.bordercolor: blue\n"
              "edge.color: darkgreen\n");

    //Print node
    old = g_tfile;
    g_tfile = h;
    g_indent = 0;
    INT c;
    for (Vertex * v = dug.get_first_vertex(c);
         v != NULL; v = dug.get_next_vertex(c)) {
        INT id = VERTEX_id(v);
        if (detail) {
            fprintf(h, "\nnode: { title:\"%d\" shape:box color:gold "
                        "fontname:\"courB\" label:\"", id);
            IR * ir = m_ru->getIR(id);
            ASSERT0(ir != NULL);
            dump_ir(ir, m_tm);
            fprintf(h, "\"}");
        } else {
            fprintf(h, "\nnode: { title:\"%d\" shape:box color:gold "
                        "fontname:\"courB\" label:\"%d\" }", id, id);
        }
    }
    g_tfile = old;

    //Print edge
    for (Edge * e = dug.get_first_edge(c);
         e != NULL; e = dug.get_next_edge(c)) {
        Vertex * from = EDGE_from(e);
        Vertex * to = EDGE_to(e);
        _EDGEINFO * ei = (_EDGEINFO*)EDGE_info(e);
        ASSERT0(ei);
        fprintf(h,
            "\nedge: { sourcename:\"%d\" targetname:\"%d\" label:\"id%d\"}",
            VERTEX_id(from), VERTEX_id(to),  ei->id);
    }
    fprintf(h, "\n}\n");
    fclose(h);
}


//Dump mem usage for each ir DU reference.
void IR_DU_MGR::dumpMemUsageForMDRef()
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile,
        "\n\n==---- DUMP '%s' IR_DU_MGR : Memory Usage for DU Reference ----==",
        m_ru->getRegionName());

    fprintf(g_tfile, "\nMustD: must defined MD");
    fprintf(g_tfile, "\nMayDs: overlapped and may defined MDSet");

    fprintf(g_tfile, "\nMustU: must used MD");

    fprintf(g_tfile, "\nMayUs: overlapped and may used MDSet");
    BBList * bbs = m_ru->getBBList();
    size_t count = 0;
    CHAR const* str = NULL;
    g_indent = 0;
    for (IRBB * bb = bbs->get_head(); bb != NULL; bb = bbs->get_next()) {
        fprintf(g_tfile, "\n--- BB%d ---", BB_id(bb));
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            dump_ir(ir, m_tm);
            fprintf(g_tfile, "\n");
            m_citer.clean();
            for (IR const* x = iterInitC(ir, m_citer);
                 x != NULL; x = iterNextC(m_citer)) {
                fprintf(g_tfile, "\n\t%s(%d)::", IRNAME(x), IR_id(x));
                if (x->is_stmt()) {
                    //MustDef
                    MD const* md = get_must_def(x);
                    if (md != NULL) {
                        fprintf(g_tfile, "MustD%uB, ", (UINT)sizeof(MD));
                        count += sizeof(MD);
                    }

                    //MayDef
                    MDSet const* mds = getMayDef(x);
                    if (mds != NULL) {
                        size_t n = mds->count_mem();
                        if (n < 1024) { str = "B"; }
                        else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
                        else  { n /= 1024*1024; str = "MB"; }

                        SEGIter * iter;
                        fprintf(g_tfile, "MayDs(%lu%s, %d elems, last %d), ",
                            (ULONG)n, str, mds->get_elem_count(),
                            mds->get_last(&iter));
                        count += n;
                    }
                } else {
                    //MustUse
                    MD const* md = get_effect_use_md(x);
                    if (md != NULL) {
                        fprintf(g_tfile, "MustU%dB, ", (INT)sizeof(MD));
                        count += sizeof(MD);
                    }

                    //MayUse
                    MDSet const* mds = getMayUse(x);
                    if (mds != NULL) {
                        size_t n = mds->count_mem();
                        if (n < 1024) { str = "B"; }
                        else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
                        else { n /= 1024*1024; str = "MB"; }

                        SEGIter * iter;
                        fprintf(g_tfile, "MayUs(%lu%s, %d elems, last %d), ",
                                (ULONG)n, str,
                                mds->get_elem_count(),
                                mds->get_last(&iter));
                        count += n;
                    }
                }
            }
         }
    }

    if (count < 1024) { str = "B"; }
    else if (count < 1024 * 1024) { count /= 1024; str = "KB"; }
    else  { count /= 1024*1024; str = "MB"; }
    fprintf(g_tfile, "\nTotal %u%s", (UINT)count, str);
    fflush(g_tfile);
}


//Dump mem usage for each internal set of bb.
void IR_DU_MGR::dumpMemUsageForEachSet()
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile,
        "\n==---- DUMP '%s' IR_DU_MGR : Memory Usage for Value Set of BB ----==",
        m_ru->getRegionName());

    BBList * bbs = m_ru->getBBList();
    size_t count = 0;
    CHAR const* str = NULL;
    for (IRBB * bb = bbs->get_head(); bb != NULL; bb = bbs->get_next()) {
        fprintf(g_tfile, "\n--- BB%d ---", BB_id(bb));
        size_t n;
        SEGIter * st = NULL;
        DefDBitSetCore * irs = m_bb_avail_in_reach_def.get(BB_id(bb));
        if (irs != NULL) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tAvaInReachDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_bb_avail_out_reach_def.get(BB_id(bb));
        if (irs != NULL) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tAvaOutReachDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_bb_in_reach_def.get(BB_id(bb));
        if (irs != NULL) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tInReachDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_bb_out_reach_def.get(BB_id(bb));
        if (irs != NULL) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tOutReachDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_bb_may_gen_def.get(BB_id(bb));
        if (irs != NULL) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tMayGenDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        irs = m_bb_must_gen_def.get(BB_id(bb));
        if (irs != NULL) {
            n = irs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tMustGenDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, irs->get_elem_count(), irs->get_last(&st));
        }

        DefDBitSetCore const* dbs = m_bb_may_killed_def.get(BB_id(bb));
        if (dbs != NULL) {
            n = dbs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tMayKilledDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, dbs->get_elem_count(), dbs->get_last(&st));
        }

        dbs = m_bb_must_killed_def.get(BB_id(bb));
        if (dbs != NULL) {
            n = dbs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tMustKilledDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, dbs->get_elem_count(), dbs->get_last(&st));
        }

        //
        DefDBitSetCore * bs = m_bb_gen_exp.get(BB_id(bb));
        if (bs != NULL) {
            n = bs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tMayKilledDef:%lu%s, %d elems, last %d",
                    (ULONG)n, str, bs->get_elem_count(), bs->get_last(&st));
        }

        dbs = m_bb_killed_exp.get(BB_id(bb));
        if (dbs != NULL) {
            n = dbs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tKilledIrExp:%lu%s, %d elems, last %d",
                    (ULONG)n, str, dbs->get_elem_count(), dbs->get_last(&st));
        }

        bs = m_bb_availin_exp.get(BB_id(bb));
        if (bs != NULL) {
            n = bs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tLiveInIrExp:%lu%s, %d elems, last %d",
                    (ULONG)n, str, bs->get_elem_count(), bs->get_last(&st));
        }

        bs = m_bb_availout_ir_expr.get(BB_id(bb));
        if (bs != NULL) {
            n = bs->count_mem();
            count += n;
            if (n < 1024) { str = "B"; }
            else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
            else  { n /= 1024*1024; str = "MB"; }
            fprintf(g_tfile, "\n\tLiveOutIrExp:%lu%s, %d elems, last %d",
                    (ULONG)n, str, bs->get_elem_count(), bs->get_last(&st));
        }
    }

    if (count < 1024) { str = "B"; }
    else if (count < 1024 * 1024) { count /= 1024; str = "KB"; }
    else  { count /= 1024*1024; str = "MB"; }
    fprintf(g_tfile, "\nTotal %u%s", (UINT)count, str);
    fflush(g_tfile);
}



//Dump Region's IR BB list.
//DUMP ALL BBList DEF/USE/OVERLAP_DEF/OVERLAP_USE"
void IR_DU_MGR::dumpRef(UINT indent)
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n\n==---- DUMP '%s' IR_DU_MGR : DU Reference ----==\n",
            m_ru->getRegionName());
    BBList * bbs = m_ru->getBBList();
    ASSERT0(bbs);
    if (bbs->get_elem_count() != 0) {
        m_md_sys->dumpAllMD();
    }

    //Dump imported variables referenced.
    fprintf(g_tfile, "\n==----==");
    MDSet * ru_maydef = m_ru->getMayDef();
    if (ru_maydef != NULL) {
        fprintf(g_tfile, "\nRegionMayDef(OuterRegion):");
        ru_maydef->dump(m_md_sys, true);
    }

    MDSet * ru_mayuse = m_ru->getMayUse();
    if (ru_mayuse != NULL) {
        fprintf(g_tfile, "\nRegionMayUse(OuterRegion):");
        ru_mayuse->dump(m_md_sys, true);
    }

    for (IRBB * bb = bbs->get_head(); bb != NULL; bb = bbs->get_next()) {
        fprintf(g_tfile, "\n--- BB%d ---", BB_id(bb));
        dumpBBRef(bb, indent);
    }
}


void IR_DU_MGR::dumpBBRef(IN IRBB * bb, UINT indent)
{
    for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
        dumpIRRef(ir, indent);
    }
}


void IR_DU_MGR::dumpIRListRef(IN IR * ir, UINT indent)
{
    for (; ir != NULL; ir = ir->get_next()) {
        dumpIRRef(ir, indent);
    }
}


//Dump IR tree's MD reference, where ir may be stmt or exp.
void IR_DU_MGR::dumpIRRef(IN IR * ir, UINT indent)
{
    if (ir == NULL || g_tfile == NULL || ir->is_const()) { return; }

    g_indent = indent;
    dump_ir(ir, m_tm, NULL, false);

    //Dump mustref MD.
    MD const* md = ir->getRefMD();
    MDSet const* mds = ir->getRefMDSet();

    //MustDef
    bool prt_mustdef = false;
    if (md != NULL) {
        fprintf(g_tfile, "\n");
        note("");
        fprintf(g_tfile, "MMD%d", MD_id(md));
        prt_mustdef = true;
    }

    if (mds != NULL) {
        //MayDef
        if (!prt_mustdef) {
            note("\n"); //dump indent blank.
        }
        fprintf(g_tfile, " : ");
        if (mds != NULL && !mds->is_empty()) {
            mds->dump(m_md_sys);
        }
    }

    if (ir->isCallStmt()) {
        bool doit = false;
        CallGraph * callg = m_ru->getRegionMgr()->getCallGraph();
        if (callg != NULL) {
            Region * rg = callg->mapCall2Region(ir, m_ru);
            if (rg != NULL && REGION_is_mddu_valid(rg)) {
                MDSet const* muse = rg->getMayUse();
                //May use
                fprintf(g_tfile, " <-- ");
                if (muse != NULL && !muse->is_empty()) {
                    muse->dump(m_md_sys);
                    doit = true;
                }
            }
        }
        if (!doit) {
            //MayUse MDSet.
            //Regard MayDef MDSet as MayUse.
            fprintf(g_tfile, " <-- ");
            MDSet const* x = ir->getRefMDSet();
            if (x != NULL && !x->is_empty()) {
                x->dump(m_md_sys);
            }
        }
    }

    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        if (ir->getKid(i) != NULL) {
            dumpIRListRef(ir->getKid(i), indent + 4);
        }
    }
    fflush(g_tfile);
}


void IR_DU_MGR::dumpBBDUChainDetail(UINT bbid)
{
    dumpBBDUChainDetail(m_cfg->getBB(bbid));
}


void IR_DU_MGR::dumpBBDUChainDetail(IRBB * bb)
{
    ASSERT0(bb);
    fprintf(g_tfile, "\n--- BB%d ---", BB_id(bb));

    //Label Info list.
    LabelInfo const* li = bb->getLabelList().get_head();
    if (li != NULL) {
        fprintf(g_tfile, "\nLABEL:");
    }
    for (; li != NULL; li = bb->getLabelList().get_next()) {
        switch (LABEL_INFO_type(li)) {
        case L_CLABEL:
            note(CLABEL_STR_FORMAT, CLABEL_CONT(li));
            break;
        case L_ILABEL:
            note(ILABEL_STR_FORMAT, ILABEL_CONT(li));
            break;
        case L_PRAGMA:
            ASSERT0(LABEL_INFO_pragma(li));
            note("%s", SYM_name(LABEL_INFO_pragma(li)));
            break;
        default: UNREACH();
        }

        if (LABEL_INFO_is_try_start(li) ||
            LABEL_INFO_is_try_end(li) ||
            LABEL_INFO_is_catch_start(li)) {
            fprintf(g_tfile, "(");
            if (LABEL_INFO_is_try_start(li)) {
                fprintf(g_tfile, "try_start,");
            }
            if (LABEL_INFO_is_try_end(li)) {
                fprintf(g_tfile, "try_end,");
            }
            if (LABEL_INFO_is_catch_start(li)) {
                fprintf(g_tfile, "catch_start");
            }
            fprintf(g_tfile, ")");
        }

        fprintf(g_tfile, " ");
    }

    for (IR * ir = BB_irlist(bb).get_head();
         ir != NULL; ir = BB_irlist(bb).get_next()) {
        dump_ir(ir, m_tm);
        fprintf(g_tfile, "\n");

        IRIter ii;
        for (IR * k = iterInit(ir, ii);
            k != NULL; k = iterNext(ii)) {
            if (!k->isMemoryRef() && !k->hasResult() && !k->is_region()) {
                continue;
            }

            fprintf(g_tfile, "\n\t>>%s", IRNAME(k));
            if (k->is_stpr() || k->is_pr()) {
                fprintf(g_tfile, "%d", k->get_prno());
            }
            fprintf(g_tfile, "(id:%d) ", IR_id(k));

            if (k->is_stmt()) {
                fprintf(g_tfile, "Dref:");
            } else {
                fprintf(g_tfile, "Uref:");
            }

            //Dump must ref.
            MD const* md = k->getRefMD();
            if (md != NULL) {
                fprintf(g_tfile, "MMD%d", MD_id(md));
            }

            //Dump may ref.
            MDSet const* mds = k->getRefMDSet();
            if (mds != NULL && !mds->is_empty()) {
                fprintf(g_tfile, ",");
                SEGIter * iter;
                for (INT i = mds->get_first(&iter); i >= 0; ) {
                    fprintf(g_tfile, "MD%d", i);
                    i = mds->get_next(i, &iter);
                    if (i >= 0) { fprintf(g_tfile, ","); }
                }
            }

            //Dump def/use list.
            if (k->is_stmt() || ir->is_lhs(k)) {
                fprintf(g_tfile, "\n\t  USE List:");
            } else {
                fprintf(g_tfile, "\n\t  DEF List:");
            }

            DUSet const* set = k->readDUSet();
            if (set != NULL) {
                DUIter di = NULL;
                for (INT i = set->get_first(&di);
                     i >= 0; ) {
                    IR const* ref = m_ru->getIR(i);
                    fprintf(g_tfile, "%s", IRNAME(ref));
                    if (ref->is_stpr() || ref->is_pr()) {
                        fprintf(g_tfile, "%d", ref->get_prno());
                    }
                    fprintf(g_tfile, "(id:%d)", IR_id(ref));

                    i = set->get_next(i, &di);

                    if (i >= 0) { fprintf(g_tfile, ","); }
                }
            }
        }
        fprintf(g_tfile, "\n");
    } //end for each IR
}


void IR_DU_MGR::dump(CHAR const* name)
{
    FILE * old = NULL;
    if (name != NULL) {
        old = g_tfile;
        //UNLINK(name);
        g_tfile = fopen(name, "a+");
        ASSERT(g_tfile, ("%s create failed!!!", name));
    }

    dumpDUChainDetail();

    fflush(g_tfile);
    if (name != NULL) {
        fclose(g_tfile);
        g_tfile = old;
    }
}


//The difference between this function and dumpDUChain is this function
//will dump du chain for each memory reference.
void IR_DU_MGR::dumpDUChainDetail()
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n\n==---- DUMP '%s' DU chain detail ----==\n",
            m_ru->getRegionName());
    g_indent = 0;
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        dumpBBDUChainDetail(bb);
    }
    fflush(g_tfile);
}


//Dump du chain only for stmt.
//This function collects must and may USE of MD and regard stmt as a whole.
//So this function does not distingwish individual memory operand inside the
//stmt, but if you want, please invoke dumpDUChainDetail().
void IR_DU_MGR::dumpDUChain()
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n\n==---- DUMP '%s' DU chain ----==\n",
            m_ru->getRegionName());
    g_indent = 2;
    MDSet mds;
    DefMiscBitSetMgr bsmgr;
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        fprintf(g_tfile, "\n--- BB%d ---", BB_id(bb));
        for (IR * ir = BB_irlist(bb).get_head();
             ir != NULL; ir = BB_irlist(bb).get_next()) {
            dump_ir(ir, m_tm);
            fprintf(g_tfile, "\n>>");

            //MustDef
            fprintf(g_tfile, "Dref(");
            bool has_prt_something = false;
            MD const* md = get_must_def(ir);
            if (md != NULL) {
                fprintf(g_tfile, "MMD%d", MD_id(md));
                has_prt_something = true;
            }

            //MayDef
            if (getMayDef(ir) != NULL) {
                mds.clean(bsmgr);
                MDSet const* x = getMayDef(ir);
                if (x != NULL) {
                    if (has_prt_something) {
                        fprintf(g_tfile, ",");
                    }

                    SEGIter * iter;
                    for (INT i = x->get_first(&iter); i >= 0;) {
                        fprintf(g_tfile, "MD%d", i);
                        i = x->get_next(i, &iter);
                        if (i >= 0) {
                            fprintf(g_tfile, ",");
                        }
                    }
                    has_prt_something = true;
                }
            }
            if (!has_prt_something) {
                fprintf(g_tfile, "--");
            }
            fprintf(g_tfile, ")");

            //MayUse
            fprintf(g_tfile, " <= ");
            mds.clean(bsmgr);

            fprintf(g_tfile, "Uref(");
            collectMayUseRecursive(ir, mds, true, bsmgr);
            if (!mds.is_empty()) {
                mds.dump(m_md_sys);
            } else {
                fprintf(g_tfile, "--");
            }
            fprintf(g_tfile, ")");

            //Dump def chain.
            m_citer.clean();
            bool first = true;
            for (IR const* x = iterRhsInitC(ir, m_citer);
                 x != NULL; x = iterRhsNextC(m_citer)) {
                 if (!x->isMemoryOpnd()) { continue; }

                DUSet const* defset = x->readDUSet();
                if (defset == NULL || defset->get_elem_count() == 0) {
                    continue;
                }

                if (first) {
                    fprintf(g_tfile, "\n>>DEF List:");
                    first = false;
                }

                DUIter di = NULL;
                for (INT i = defset->get_first(&di);
                     i >= 0; i = defset->get_next(i, &di)) {
                    IR const* def = m_ru->getIR(i);
                    fprintf(g_tfile, "%s(id:%d), ", IRNAME(def), def->id());
                }
            }

            //Dump use chain.
            DUSet const* useset = ir->readDUSet();
            if (useset == NULL || useset->get_elem_count() == 0) { continue; }

            fprintf(g_tfile, "\n>>USE List:");
            DUIter di = NULL;
            for (INT i = useset->get_first(&di);
                 i >= 0; i = useset->get_next(i, &di)) {
                IR const* u = m_ru->getIR(i);
                fprintf(g_tfile, "%s(id:%d), ", IRNAME(u), IR_id(u));
            }
        } //end for each IR
    } //end for each BB
    mds.clean(bsmgr);
    fflush(g_tfile);
}


//'is_bs': true to dump bitset info.
void IR_DU_MGR::dumpSet(bool is_bs)
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n\n==---- DUMP %s() IR_DU_MGR : SET ----==\n",
            m_ru->getRegionName());
    BBList * bbl = m_ru->getBBList();
    C<IRBB*> * cb;
    g_indent = 2;
    for (IRBB * bb = bbl->get_head(&cb);
         bb != NULL; bb = bbl->get_next(&cb)) {

        UINT bbid = BB_id(bb);
        fprintf(g_tfile, "\n---- BB%d ----", bbid);
        DefDBitSetCore * def_in = getInReachDef(bbid, NULL);
        DefDBitSetCore * def_out = getOutReachDef(bbid, NULL);
        DefDBitSetCore * avail_def_in = getAvailInReachDef(bbid, NULL);
        DefDBitSetCore * avail_def_out = getAvailOutReachDef(bbid, NULL);
        DefDBitSetCore * may_def_gen = getMayGenDef(bbid, NULL);
        DefDBitSetCore * must_def_gen = getMustGenDef(bbid, NULL);
        DefDBitSetCore * gen_ir = getGenIRExpr(bbid, NULL);
        DefDBitSetCore * livein_ir = getAvailInExpr(bbid, NULL);
        DefDBitSetCore * liveout_ir = getAvailOutExpr(bbid, NULL);
        DefDBitSetCore const* must_def_kill = getMustKilledDef(bbid);
        DefDBitSetCore const* may_def_kill = getMayKilledDef(bbid);
        DefDBitSetCore const* killed_exp = getKilledIRExpr(bbid);

        INT i;

        SEGIter * st = NULL;
        if (def_in != NULL) {
            fprintf(g_tfile, "\nDEF IN STMT: %lu byte ",
                (ULONG)def_in->count_mem());
            for (i = def_in->get_first(&st);
                 i != -1; i = def_in->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                def_in->dump(g_tfile);
            }
        }

        if (def_out != NULL) {
            fprintf(g_tfile, "\nDEF OUT STMT: %lu byte ",
                (ULONG)def_out->count_mem());
            for (i = def_out->get_first(&st);
                 i != -1; i = def_out->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                def_out->dump(g_tfile);
            }
        }

        if (avail_def_in != NULL) {
            fprintf(g_tfile, "\nDEF AVAIL_IN STMT: %lu byte ",
                (ULONG)avail_def_in->count_mem());
            for (i = avail_def_in->get_first(&st);
                 i != -1; i = avail_def_in->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                avail_def_in->dump(g_tfile);
            }
        }

        if (avail_def_out != NULL) {
            fprintf(g_tfile, "\nDEF AVAIL_OUT STMT: %lu byte ",
                (ULONG)avail_def_out->count_mem());
            for (i = avail_def_out->get_first(&st);
                 i != -1; i = avail_def_out->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                avail_def_out->dump(g_tfile);
            }
        }

        if (may_def_gen != NULL) {
            fprintf(g_tfile, "\nMAY GEN STMT: %lu byte ",
                (ULONG)may_def_gen->count_mem());
            for (i = may_def_gen->get_first(&st);
                 i != -1; i = may_def_gen->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                may_def_gen->dump(g_tfile);
            }
        }

        if (must_def_gen != NULL) {
            fprintf(g_tfile, "\nMUST GEN STMT: %lu byte ",
                (ULONG)must_def_gen->count_mem());
            for (i = must_def_gen->get_first(&st);
                 i != -1; i = must_def_gen->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                must_def_gen->dump(g_tfile);
            }
        }

        if (must_def_kill != NULL) {
            fprintf(g_tfile, "\nMUST KILLED STMT: %lu byte ",
                    (ULONG)must_def_kill->count_mem());
            for (i = must_def_kill->get_first(&st);
                 i != -1; i = must_def_kill->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                must_def_kill->dump(g_tfile);
            }
        }

        if (may_def_kill != NULL) {
            fprintf(g_tfile, "\nMAY KILLED STMT: %lu byte ",
                    (ULONG)may_def_kill->count_mem());
            for (i = may_def_kill->get_first(&st);
                 i != -1; i = may_def_kill->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                may_def_kill->dump(g_tfile);
            }
        }

        if (livein_ir != NULL) {
            fprintf(g_tfile, "\nLIVEIN EXPR: %lu byte ",
                    (ULONG)livein_ir->count_mem());
            for (i = livein_ir->get_first(&st);
                 i != -1; i = livein_ir->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                livein_ir->dump(g_tfile);
            }
        }

        if (liveout_ir != NULL) {
            fprintf(g_tfile, "\nLIVEOUT EXPR: %lu byte ",
                    (ULONG)liveout_ir->count_mem());
            for (i = liveout_ir->get_first(&st);
                 i != -1; i = liveout_ir->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                liveout_ir->dump(g_tfile);
            }
        }

        if (gen_ir != NULL) {
            fprintf(g_tfile, "\nGEN EXPR: %lu byte ", (ULONG)gen_ir->count_mem());
            for (i = gen_ir->get_first(&st);
                 i != -1; i = gen_ir->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                gen_ir->dump(g_tfile);
            }
        }

        if (killed_exp != NULL) {
            fprintf(g_tfile, "\nKILLED EXPR: %lu byte ",
                    (ULONG)killed_exp->count_mem());
            for (i = killed_exp->get_first(&st);
                 i != -1; i = killed_exp->get_next(i, &st)) {
                IR * ir = m_ru->getIR(i);
                ASSERT0(ir != NULL);
                fprintf(g_tfile, "%s(%d), ", IRNAME(ir), ir->id());
            }
            if (is_bs) {
                fprintf(g_tfile, "\n             ");
                killed_exp->dump(g_tfile);
            }
        }
    }
    fflush(g_tfile);
}


//This function copy MustUse and MayUse mds from tree 'to' to tree 'from'
//for memory reference and building new DU chain for 'to'.
//
//This function will process SSA du info if it exist.
//
//copyDUChain: if true to copy DU chain from tree 'from' to tree 'to'.
//    this operation will establish new DU chain between the DEF of 'from' and
//    'to'.
//'to': root node of target tree.
//'from': root node of source tree.
//NOTICE: IR tree 'to' and 'from' must be identical. to and from may be stmt.
void IR_DU_MGR::copyIRTreeDU(IR * to, IR const* from, bool copyDUChain)
{
    if (to == from) { return; }
    ASSERT0(to->isIREqual(from, true));
    m_citer.clean();
    m_iter2.clean();
    IR const* from_ir = iterInitC(from, m_citer);
    for (IR * to_ir = iterInit(to, m_iter2);
         to_ir != NULL;
         to_ir = iterNext(m_iter2),
         from_ir = iterNextC(m_citer)) {
        ASSERT0(to_ir->isIREqual(from_ir, true));
        if (!to_ir->isMemoryRef() && !to_ir->is_id()) {
            //Copy MD for IR_ID, some Passes need it, e.g. GVN.
            continue;
        }

        to_ir->copyRef(from_ir, m_ru);

        if (!copyDUChain) { continue; }

        SSAInfo * ssainfo;
        if ((ssainfo = from_ir->getSSAInfo()) != NULL) {
            if (from_ir->isWritePR() || from_ir->isCallHasRetVal()) {
                ASSERT(0, ("SSA only has one def"));
            }

            ASSERT0(to_ir->isReadPR());
            PR_ssainfo(to_ir) = ssainfo;
            SSA_uses(ssainfo).append(to_ir);
        } else {
            DUSet const* from_du = from_ir->readDUSet();
            if (from_du == NULL || from_du->is_empty()) { continue; }

            DUSet * to_du = getAndAllocDUSet(to_ir);
            to_du->copy(*from_du, *m_misc_bs_mgr);

            //Add new du chain between DEF and USE.
            DUIter di = NULL;
            for (UINT i = from_du->get_first(&di);
                 di != NULL; i = from_du->get_next(i, &di)) {

                //x is stmt if from_ir is expression.
                //x is expression if from_ir is stmt.
                IR const* x = m_ru->getIR(i);
                DUSet * x_du_set = x->getDUSet();
                if (x_du_set == NULL) { continue; }
                x_du_set->add(IR_id(to_ir), *m_misc_bs_mgr);
            }
        }
    }
    ASSERT0(m_iter2.get_elem_count() == 0 && m_iter.get_elem_count() == 0);
}


//Remove 'def' out of ir's DEF set. ir is exp.
void IR_DU_MGR::removeDef(IR const* ir, IR const* def)
{
    ASSERT0(ir->is_exp() && def->is_stmt());
    DUSet * duset = ir->getDUSet();
    if (duset == NULL) { return; }
    duset->removeDef(def, *m_misc_bs_mgr);
}


//Return true if mustdef or maydef overlaped with use's referrence.
bool IR_DU_MGR::isOverlapDefUse(
        MD const* mustdef,
        MDSet const* maydef,
        IR const* use)
{
    if (maydef != NULL) {
        MDSet const* mayuse = getMayUse(use);
        if (mayuse != NULL &&
            (mayuse == maydef ||
             mayuse->is_intersect(*maydef))) {
            return true;
        }

        MD const* mustuse = get_effect_use_md(use);
        if (mustuse != NULL && maydef->is_contain(mustuse)) {
            return true;
        }
    }

    if (mustdef != NULL) {
        MDSet const* mayuse = getMayUse(use);
        if (mustdef != NULL) {
            if (mayuse != NULL && mayuse->is_contain(mustdef)) {
                return true;
            }

            MD const* mustuse = get_effect_use_md(use);
            if (mustuse != NULL &&
                (mustuse == mustdef ||
                 mustuse->is_overlap(mustdef))) {
                return true;
            }
        }
    }

    return false;
}


//Check each USE of stmt, remove the expired one which is not reference
//the memory any more that stmt defined.
bool IR_DU_MGR::removeExpiredDUForStmt(IR * stmt)
{
    bool change = false;
    ASSERT0(stmt->is_stmt());
    SSAInfo * ssainfo = stmt->getSSAInfo();
    if (ssainfo != NULL) {
        SSAUseIter si;
        UINT prno = 0;
        if (SSA_def(ssainfo) != NULL) {
            prno = SSA_def(ssainfo)->get_prno();
        } else {
            //Without DEF.
            return change;
        }

        //Check if use referenced the number of PR which defined by SSA_def.
        INT ni = -1;
        for (INT i = SSA_uses(ssainfo).get_first(&si);
             si != NULL; i = ni) {
            ni = SSA_uses(ssainfo).get_next(i, &si);

            IR const* use = m_ru->getIR(i);

            if (use->is_pr() && PR_no(use) == prno) { continue; }

            SSA_uses(ssainfo).remove(use);

            change = true;
        }

        return change;
    }

    DUSet const* useset = stmt->readDUSet();
    if (useset == NULL) { return false; }

    ASSERT0(stmt->isMemoryRef());

    MDSet const* maydef = getMayDef(stmt);
    MD const* mustdef = get_must_def(stmt);

    DUIter di = NULL;
    UINT next_u;
    for (UINT u = useset->get_first(&di); di != NULL; u = next_u) {
        next_u = useset->get_next(u, &di);

        IR const* use = m_ru->getIR(u);
        ASSERT0(use->is_exp());

        if (isOverlapDefUse(mustdef, maydef, use)) { continue; }

        //There is no du-chain bewteen stmt and use. Cut the MD du.
        removeDUChain(stmt, use);
        change = true;
    }

    return change;
}


bool IR_DU_MGR::removeExpiredDUForOperand(IR * stmt)
{
    bool change = false;
    m_citer.clean();
    for (IR const* k = iterRhsInitC(stmt, m_citer);
         k != NULL; k = iterRhsNextC(m_citer)) {
        if (!k->isMemoryOpnd()) { continue; }

        SSAInfo * ssainfo;
        if (k->isReadPR() && (ssainfo = PR_ssainfo(k)) != NULL) {
            SSAUseIter si;
            UINT prno = 0;
            if (SSA_def(ssainfo) != NULL) {
                prno = SSA_def(ssainfo)->get_prno();
            } else {
                continue;
            }

            INT ni = -1;
            for (INT i = SSA_uses(ssainfo).get_first(&si);
                 si != NULL; i = ni) {
                ni = SSA_uses(ssainfo).get_next(i, &si);
                IR const* use = m_ru->getIR(i);

                if (use->is_pr() && PR_no(use) == prno) { continue; }

                SSA_uses(ssainfo).remove(use);
            }

            continue;
        }

        DUSet const* defset = k->readDUSet();
        if (defset == NULL) { continue; }

        DUIter di = NULL;
        UINT nd;
        for (UINT d = defset->get_first(&di); di != NULL; d = nd) {
            nd = defset->get_next(d, &di);

            IR const* def = m_ru->getIR(d);
            ASSERT0(def->is_stmt());

            if (isOverlapDefUse(get_must_def(def), getMayDef(def), k)) {
                continue;
            }

            //There is no du-chain bewteen d and u. Cut the du chain.
            removeDUChain(def, k);
            change = true;
        }
    }
    return change;
}


//Check if the DEF of stmt's operands still modify the same memory object.
//This function will process SSA info if it exists.
//e.g: Revise DU chain if stmt's rhs has changed.
//    x=10 //S1
//    ...
//    a=x*0 //S2
//  =>
//    x=10 //S1
//    ...
//    a=0 //S2
//Given S1 is def, S2 is use, after ir refinement, x in S2
//is removed, remove the data dependence between S1
//and S2 operand.
bool IR_DU_MGR::removeExpiredDU(IR * stmt)
{
    ASSERT0(stmt->is_stmt());
    bool change = removeExpiredDUForStmt(stmt);
    change |= removeExpiredDUForOperand(stmt);
    return change;
}


//This function check all USE of memory references of ir tree and
//cut its du-chain. 'ir' may be stmt or expression, if ir is stmt,
//check its right-hand-side.
//This function will process SSA info if it exists.
//'ir': indicate the root of IR tree.
//e.g: d1, d2 are def-stmt of stmt's operands.
//this functin cut off du-chain between d1, d2 and their use.
void IR_DU_MGR::removeUseOutFromDefset(IR * ir)
{
    m_citer.clean();
    IR const* k;
    if (ir->is_stmt()) {
        k = iterRhsInitC(ir, m_citer);
    } else {
        k = iterExpInitC(ir, m_citer);
    }

    for (; k != NULL; k = iterRhsNextC(m_citer)) {
        if (!k->isMemoryOpnd()) { continue; }

        SSAInfo * ssainfo;
        if ((ssainfo = k->getSSAInfo()) != NULL) {
            ASSERT0(k->is_pr());
            SSA_uses(ssainfo).remove(k);
            continue;
        }

        DUSet * defset = k->getDUSet();
        if (defset == NULL) { continue; }

        DUIter di = NULL;
        bool doclean = false;
        for (INT i = defset->get_first(&di);
             i >= 0; i = defset->get_next(i, &di)) {
            doclean = true;
            IR const* stmt = m_ru->getIR(i);
            ASSERT0(stmt->is_stmt());

            DUSet * useset = stmt->getDUSet();
            if (useset == NULL) { continue; }
            useset->removeUse(k, *m_misc_bs_mgr);
        }
        if (doclean) {
            defset->clean(*m_misc_bs_mgr);
        }
    }
}


//Note that do NOT use this function to remove SSA def.
//This function handle the MD DU chain and cut
//off the DU chain between MD def and its MD use expression.
//Remove 'def' out from its use's def-list.
//e.g:u1, u2 are its use expressions.
//cut off the du chain between def->u1 and def->u2.
void IR_DU_MGR::removeDefOutFromUseset(IR * def)
{
    ASSERT0(def->is_stmt());

    //Could not just remove the SSA def, you should consider the SSA_uses
    //and make sure they are all removable. Use SSA form related api.
    ASSERT0(def->getSSAInfo() == NULL);

    DUSet * useset = def->getDUSet();
    if (useset == NULL) { return; }

    DUIter di = NULL;
    bool doclean = false;
    for (INT i = useset->get_first(&di);
    //Remove the du chain bewteen DEF and its USE.
         i >= 0; i = useset->get_next(i, &di)) {
        doclean = true;
        IR const* exp = m_ru->getIR(i);
        ASSERT0(exp->is_exp());

        DUSet * du = exp->getDUSet();
        if (du != NULL) { du->removeDef(def, *m_misc_bs_mgr); }
    }

    //Clean USE set.
    if (doclean) {
        useset->clean(*m_misc_bs_mgr);
    }
}


//Remove all du info of ir out from DU mgr.
void IR_DU_MGR::removeIROutFromDUMgr(IR * ir)
{
    ASSERT0(ir->is_stmt());
    removeUseOutFromDefset(ir);

    //If stmt has SSA info, it should be maintained by SSA related api.
    if (ir->getSSAInfo() == NULL) {
        removeDefOutFromUseset(ir);
    }
}


//Count up the memory has been allocated.
size_t IR_DU_MGR::count_mem()
{
    Vector<DefDBitSetCore*> * ptr;

    size_t count = sizeof(m_mds_mgr);
    count += smpoolGetPoolSize(m_pool);

    count += m_bb_avail_in_reach_def.count_mem();
    ptr = &m_bb_avail_in_reach_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_avail_out_reach_def.count_mem();
    ptr = &m_bb_avail_out_reach_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_in_reach_def.count_mem();
    ptr = &m_bb_in_reach_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_out_reach_def.count_mem();
    ptr = &m_bb_out_reach_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_may_gen_def.count_mem();
    ptr = &m_bb_may_gen_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_must_gen_def.count_mem();
    ptr = &m_bb_must_gen_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_must_killed_def.count_mem();
    for (INT i = 0; i <= m_bb_must_killed_def.get_last_idx(); i++) {
        DefDBitSetCore const* dset = m_bb_must_killed_def.get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_may_killed_def.count_mem();
    for (INT i = 0; i <= m_bb_must_killed_def.get_last_idx(); i++) {
        DefDBitSetCore const* dset = m_bb_must_killed_def.get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_gen_exp.count_mem();
    ptr = &m_bb_gen_exp;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_killed_exp.count_mem();
    for (INT i = 0; i <= m_bb_killed_exp.get_last_idx(); i++) {
        DefDBitSetCore const* dset = m_bb_killed_exp.get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_availin_exp.count_mem();
    ptr = &m_bb_availin_exp;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    count += m_bb_availout_ir_expr.count_mem();
    ptr = &m_bb_availout_ir_expr;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) {
            count += dset->count_mem();
        }
    }

    return count;
}


//Count up memory of DUSet for all irs.
size_t IR_DU_MGR::count_mem_duset()
{
    size_t count = 0;
    Vector<IR*> * vec = m_ru->getIRVec();
    INT l = vec->get_last_idx();
    for (INT i = 1; i <= l; i++) {
        IR const* ir = vec->get(i);
        DUSet const* duset = ir->readDUSet();
        if (duset != NULL) {
            count += duset->count_mem();
        }
    }
    return count;
}


//Collect MustUse MDSet for both PR operation and Non-PR operation.
//e.g: = a + b + *p;
//    assume p->w,u, the MustUse is {a,b,p}, not include w,u.
void IR_DU_MGR::collectMustUsedMDs(IR const* ir, OUT MDSet & mustuse)
{
    switch (ir->get_code()) {
    case IR_ST:
        computeExpression(ST_rhs(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU|COMPUTE_NOPR_DU);
        return;
    case IR_STPR:
        computeExpression(STPR_rhs(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        return;
    case IR_IST:
        computeExpression(IST_rhs(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        computeExpression(IST_base(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        return;
    case IR_ICALL:
        computeExpression(ICALL_callee(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
    case IR_CALL:
        for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
            computeExpression(p, &mustuse,
                COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        }
        for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
            computeExpression(p, &mustuse,
                COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        }
        return;
    case IR_GOTO:
    case IR_REGION:
        break;
    case IR_IGOTO:
        computeExpression(IGOTO_vexp(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        return;
    case IR_DO_WHILE:
    case IR_WHILE_DO:
    case IR_DO_LOOP:
    case IR_IF:
    case IR_LABEL:
    case IR_CASE:
        ASSERT(0, ("TODO"));
        break;
    case IR_TRUEBR:
    case IR_FALSEBR:
        computeExpression(BR_det(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        return;
    case IR_SWITCH:
        computeExpression(SWITCH_vexp(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        return;
    case IR_RETURN:
        computeExpression(RET_exp(ir), &mustuse,
            COMP_EXP_COLLECT_MUST_USE, COMPUTE_PR_DU | COMPUTE_NOPR_DU);
        return;
    default: UNREACH();
    } //end switch
}


void IR_DU_MGR::inferStore(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_st() && ir->getRefMD());

    if (HAVE_FLAG(duflag, COMPUTE_NOPR_DU)) {
        //Find ovelapped MD.
        //e.g: struct {int a;} s;
        //s = ...
        //s.a = ...
        //Where s and s.a is overlapped.
        computeOverlapDefMDSet(ir, false);
    }

    computeExpression(ST_rhs(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
}


void IR_DU_MGR::inferStoreArray(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_starray());
    computeArrayRef(ir, NULL, COMP_EXP_RECOMPUTE, duflag);
    computeExpression(STARR_rhs(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
}


void IR_DU_MGR::inferStorePR(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_stpr() && ir->getRefMD() && ir->getRefMDSet() == NULL);
    computeExpression(STPR_rhs(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
}


void IR_DU_MGR::inferSetelem(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_setelem() && ir->getRefMD() && ir->getRefMDSet() == NULL);
    computeExpression(SETELEM_rhs(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
    computeExpression(SETELEM_ofst(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
}


void IR_DU_MGR::inferGetelem(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_getelem() && ir->getRefMD() && ir->getRefMDSet() == NULL);
    computeExpression(GETELEM_base(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
    computeExpression(GETELEM_ofst(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
}


void IR_DU_MGR::inferPhi(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_phi() && ir->getRefMD() && ir->getRefMDSet() == NULL);
    //Set call result list MD.
    IR * r = PHI_opnd_list(ir);
    while (r != NULL) {
        ASSERT0(r->getRefMD() && r->getRefMD()->is_pr());
        ASSERT0(r->getRefMDSet() == NULL);
        computeExpression(r, NULL, COMP_EXP_RECOMPUTE, duflag);
        r = r->get_next();
    }
}


void IR_DU_MGR::inferIstore(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_ist());
    computeExpression(IST_base(ir), NULL, COMP_EXP_RECOMPUTE, duflag);

    if (HAVE_FLAG(duflag, COMPUTE_NOPR_DU)) {
        //Compute DEF mdset. AA should guarantee either mustdef is not NULL or
        //maydef not NULL.
        ASSERT0((ir->getRefMDSet() && !ir->getRefMDSet()->is_empty()) ||
            (ir->getRefMD()));
        computeOverlapDefMDSet(ir, false);
    }
    computeExpression(IST_rhs(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
}


//Inference call's MayDef MD set.
//Call may modify addressable local variables and globals in indefinite ways.
void IR_DU_MGR::inferCallAndIcall(IR * ir, UINT duflag, IN MD2MDSet * mx)
{
    ASSERT0(ir->isCallStmt());
    if (ir->is_icall()) {
        computeExpression(ICALL_callee(ir), NULL, COMP_EXP_RECOMPUTE, duflag);
    }

    if (!HAVE_FLAG(duflag, COMPUTE_NOPR_DU)) {
        //Only compute du chain for PR.
        for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
            computeExpression(p, NULL, COMP_EXP_RECOMPUTE, duflag);
        }
        for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
             computeExpression(p, NULL, COMP_EXP_RECOMPUTE, duflag);
        }
        return;
    }

    MDSet maydefuse;

    //Set MD which parameters pointed to.
    for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
        if (p->is_ptr() || p->is_void()) {
            //Compute the point-to set p pointed to.
            //e.g: foo(p); where p->{a, b}, then foo may use p, a, b.
            //Note that point-to set is only avaiable for the
            //last stmt of BB. The call is just in the situation.
            ASSERT(mx, ("needed by computation of NOPR du chain"));
            ASSERT0(m_aa);
            m_aa->computeMayPointTo(p, mx, maydefuse);
        }

        //Compute USE mdset.
        computeExpression(p, NULL, COMP_EXP_RECOMPUTE, duflag);
    }

    for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
         computeExpression(p, NULL, COMP_EXP_RECOMPUTE, duflag);
         if (p->getRefMD() != NULL) {
            maydefuse.bunion_pure(p->getRefMD()->id(), *m_misc_bs_mgr);
         }
         if (p->getRefMDSet() != NULL && !p->getRefMDSet()->is_empty()) {
            maydefuse.bunion_pure(*p->getRefMDSet(), *m_misc_bs_mgr);
         }
    }

    bool modify_global = true;
    if (ir->isReadOnlyCall()) {
        modify_global = false;
    } else {
        //Utilize calllee's MayDef MD Set if callee region has been processed.
        CallGraph * callg = m_ru->getRegionMgr()->getCallGraph();
        if (callg != NULL) {
            Region * callee = callg->mapCall2Region(ir, m_ru);
            if (callee != NULL && REGION_is_mddu_valid(callee)) {
                MDSet const* maydef = callee->getMayDef();
                if (maydef != NULL && !maydef->is_empty()) {
                    maydefuse.bunion_pure(*maydef, *m_misc_bs_mgr);
                }
                modify_global = false;
            }
        }
    }

    if (modify_global) {
        //For conservative purpose.
        //Set to mod/ref global memor, all imported variables,
        //and all exposed local variables for conservative purpose.
        maydefuse.bunion(m_md_sys->getMD(MD_GLOBAL_MEM), *m_misc_bs_mgr);
        maydefuse.bunion(m_md_sys->getMD(MD_IMPORT_VAR), *m_misc_bs_mgr);
        ASSERT0(m_aa);
        maydefuse.bunion(*m_aa->getMayPointToMDSet(), *m_misc_bs_mgr);
    }

    MDSet tmpmds;
    m_md_sys->computeOverlap(m_ru, maydefuse,
        tmpmds, m_tab_iter, *m_misc_bs_mgr, true);
    maydefuse.bunion_pure(tmpmds, *m_misc_bs_mgr);

    //Register the MD set.
    ir->setRefMDSet(m_mds_hash->append(maydefuse), m_ru);
    maydefuse.clean(*m_misc_bs_mgr);
    tmpmds.clean(*m_misc_bs_mgr);
}


//Collect MD which ir may use, include overlapped MD.
void IR_DU_MGR::collectMayUse(IR const* ir, MDSet & may_use, bool computePR)
{
    m_citer.clean();
    IR const* x = NULL;
    bool const is_stmt = ir->is_stmt();
    if (is_stmt) {
        x = iterRhsInitC(ir, m_citer);
    } else {
        x = iterExpInitC(ir, m_citer);
    }

    for (; x != NULL; x = iterRhsNextC(m_citer)) {
        if (!x->isMemoryOpnd()) { continue; }

        ASSERT0(x->getParent());

        if ((x->is_id() || x->is_ld()) && x->getParent()->is_lda()) {
            continue;
        }

        if (x->is_pr() && computePR) {
            ASSERT0(getMustUse(x));
            may_use.bunion_pure(MD_id(getMustUse(x)), *m_misc_bs_mgr);
            continue;
        }

        MD const* mustref = getMustUse(x);
        MDSet const* mayref = getMayUse(x);

        if (mustref != NULL) {
            may_use.bunion(mustref, *m_misc_bs_mgr);
        }

        if (mayref != NULL) {
            may_use.bunion(*mayref, *m_misc_bs_mgr);
        }
    }

    if (is_stmt) {
        if (ir->isCallStmt()) {
            //Handle CALL/ICALL stmt sideeffect.
            bool done = false;
            CallGraph * callg = m_ru->getRegionMgr()->getCallGraph();
            if (callg != NULL) {
                Region * rg = callg->mapCall2Region(ir, m_ru);
                if (rg != NULL && REGION_is_mddu_valid(rg)) {
                    MDSet const* muse = rg->getMayUse();
                    if (muse != NULL) {
                        may_use.bunion(*muse, *m_misc_bs_mgr);
                        done = true;
                    }
                }
            }

            if (!done) {
                //Regard CALL/ICALL's MayDef MDSet as MayUse.
                MDSet const* muse = ir->getRefMDSet();
                if (muse != NULL) {
                    may_use.bunion(*muse, *m_misc_bs_mgr);
                }
            }
        } else if (ir->is_region()) {
            MDSet const* x2 = REGION_ru(ir)->getMayUse();
            if (x2 != NULL) {
                may_use.bunion(*x2, *m_misc_bs_mgr);
            }
        }
    }
}


//Collect MD which ir may use, include overlapped MD.
void IR_DU_MGR::collectMayUseRecursive(
        IR const* ir,
        OUT MDSet & may_use,
        bool computePR,
        DefMiscBitSetMgr & bsmgr)
{
    if (ir == NULL) { return; }
    switch (ir->get_code()) {
    case IR_CONST:  return;
    case IR_ID:
    case IR_LD:
        ASSERT0(ir->getParent() != NULL);
        if (!ir->getParent()->is_lda()) {
            ASSERT0(getMustUse(ir));
            may_use.bunion(getMustUse(ir), bsmgr);

            MDSet const* ts = getMayUse(ir);
            if (ts != NULL) {
                may_use.bunion(*ts, bsmgr);
            }
        }
        return;
    case IR_ST:
        collectMayUseRecursive(ST_rhs(ir), may_use, computePR, bsmgr);
        return;
    case IR_STPR:
        collectMayUseRecursive(STPR_rhs(ir), may_use, computePR, bsmgr);
        return;
    case IR_SETELEM:
        collectMayUseRecursive(SETELEM_rhs(ir), may_use, computePR, bsmgr);
        collectMayUseRecursive(SETELEM_ofst(ir), may_use, computePR, bsmgr);
        return;
    case IR_GETELEM:
        collectMayUseRecursive(GETELEM_base(ir), may_use, computePR, bsmgr);
        collectMayUseRecursive(GETELEM_ofst(ir), may_use, computePR, bsmgr);
        return;
    case IR_STARRAY:
        for (IR * s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
            collectMayUseRecursive(s, may_use, computePR, bsmgr);
        }
        collectMayUseRecursive(ARR_base(ir), may_use, computePR, bsmgr);
        collectMayUseRecursive(STARR_rhs(ir), may_use, computePR, bsmgr);
        return;
    case IR_ILD:
        collectMayUseRecursive(ILD_base(ir), may_use, computePR, bsmgr);
        ASSERT0(ir->getParent() != NULL);
        if (!ir->getParent()->is_lda()) {
            MD const* t = getMustUse(ir);
            if (t != NULL) {
                may_use.bunion(t, bsmgr);
            }

            MDSet const* ts = getMayUse(ir);
            if (ts != NULL) {
                may_use.bunion(*ts, bsmgr);
            }
        }
        return;
    case IR_IST:
        collectMayUseRecursive(IST_rhs(ir), may_use, computePR, bsmgr);
        collectMayUseRecursive(IST_base(ir), may_use, computePR, bsmgr);
        break;
    case IR_LDA:
        ASSERT0(ir->verify(m_ru));
        return;
    case IR_ICALL:
        collectMayUseRecursive(ICALL_callee(ir), may_use, computePR, bsmgr);
        //Fall through.
    case IR_CALL:
        {
            IR * p = CALL_param_list(ir);
            while (p != NULL) {
                collectMayUseRecursive(p, may_use, computePR, bsmgr);
                p = p->get_next();
            }

            bool done = false;
            CallGraph * callg = m_ru->getRegionMgr()->getCallGraph();
            if (callg != NULL) {
                Region * rg = callg->mapCall2Region(ir, m_ru);
                if (rg != NULL && REGION_is_mddu_valid(rg)) {
                    MDSet const* muse = rg->getMayUse();
                    if (muse != NULL) {
                        may_use.bunion(*muse, bsmgr);
                        done = true;
                    }
                }
            }

            if (!done) {
                //Regard MayDef MDSet as MayUse.
                MDSet const* muse = ir->getRefMDSet();
                if (muse != NULL) {
                    may_use.bunion(*muse, bsmgr);
                }
            }
        }
        return;
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
        //Binary operation.
        collectMayUseRecursive(BIN_opnd0(ir), may_use, computePR, bsmgr);
        collectMayUseRecursive(BIN_opnd1(ir), may_use, computePR, bsmgr);
        return;
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
        collectMayUseRecursive(UNA_opnd(ir), may_use, computePR, bsmgr);
        return;
    case IR_GOTO:
    case IR_LABEL:
        return;
    case IR_IGOTO:
        collectMayUseRecursive(IGOTO_vexp(ir), may_use, computePR, bsmgr);
        return;
    case IR_SWITCH:
        collectMayUseRecursive(SWITCH_vexp(ir), may_use, computePR, bsmgr);
        return;
    case IR_ARRAY:
        {
            ASSERT0(ir->getParent() != NULL);
            MD const* t = getMustUse(ir);
            if (t != NULL) {
                may_use.bunion(t, bsmgr);
            }

            MDSet const* ts = getMayUse(ir);
            if (ts != NULL) {
                may_use.bunion(*ts, bsmgr);
            }

            for (IR * s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
                collectMayUseRecursive(s, may_use, computePR, bsmgr);
            }
            collectMayUseRecursive(ARR_base(ir), may_use, computePR, bsmgr);
        }
        return;
    case IR_CVT:
        //CVT should not has any use-mds. Even if the operation
        //will genrerate different type.
        collectMayUseRecursive(CVT_exp(ir), may_use, computePR, bsmgr);
        return;
    case IR_PR:
        if (!computePR) { return; }

        ASSERT0(getMustUse(ir));
        may_use.bunion_pure(MD_id(getMustUse(ir)), bsmgr);
        return;
    case IR_TRUEBR:
    case IR_FALSEBR:
        ASSERT0(BR_lab(ir));
        collectMayUseRecursive(BR_det(ir), may_use, computePR, bsmgr);
        return;
    case IR_RETURN:
        collectMayUseRecursive(RET_exp(ir), may_use, computePR, bsmgr);
        return;
    case IR_SELECT:
        collectMayUseRecursive(SELECT_pred(ir), may_use, computePR, bsmgr);
        collectMayUseRecursive(SELECT_trueexp(ir), may_use, computePR, bsmgr);
        collectMayUseRecursive(SELECT_falseexp(ir), may_use, computePR, bsmgr);
        return;
    case IR_PHI:
        for (IR * p = PHI_opnd_list(ir); p != NULL; p = p->get_next()) {
            collectMayUseRecursive(p, may_use, computePR, bsmgr);
        }
        return;
    case IR_REGION:
        {
            MDSet const* x = REGION_ru(ir)->getMayUse();
            if (x != NULL && !x->is_empty()) {
                may_use.bunion(*x, bsmgr);
            }
        }
        return;
    default: UNREACH();
    }
}


void IR_DU_MGR::computeMayDef(
        IR const* ir,
        MDSet * bb_maydefmds,
        DefDBitSetCore * maygen_stmt,
        DefMiscBitSetMgr & bsmgr,
        UINT flag)
{
    ASSERT0(ir->is_stmt());
    switch (ir->get_code()) {
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
        if (!HAVE_FLAG(flag, COMPUTE_NOPR_DU)) { return; }
        break;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_CALL:
    case IR_ICALL:
    case IR_PHI:
        if (!HAVE_FLAG(flag, COMPUTE_PR_DU)) { return; }
        break;
    case IR_REGION:
        //Region does not have any def.
        break;
    default: //Handle general stmt.
        ASSERT0(!ir->isMemoryRef());
    }

    if (!ir->isWritePR()) {
        MD const* ref = get_effect_def_md(ir);
        if (ref != NULL && !ref->is_exact()) {
            bb_maydefmds->bunion(ref, bsmgr);
        }

        //Collect maydef mds.
        MDSet const* refs = getMayDef(ir);
        if (refs != NULL && !refs->is_empty()) {
            bb_maydefmds->bunion(*refs, bsmgr);
        }
    }

    //Computing May GEN set of reach-definition.
    //The computation of reach-definition problem is conservative.
    //If we can not say whether a DEF is killed, regard it as lived STMT.
    SEGIter * st = NULL;
    INT ni;
    for (INT i = maygen_stmt->get_first(&st); i != -1; i = ni) {
        ni = maygen_stmt->get_next(i, &st);
        IR * gened_ir = m_ru->getIR(i);
        ASSERT0(gened_ir != NULL && gened_ir->is_stmt());
        if (is_must_kill(ir, gened_ir)) {
            maygen_stmt->diff(i, bsmgr);
        }
    }
    maygen_stmt->bunion(ir->id(), bsmgr);
}


void IR_DU_MGR::computeMustExactDef(
        IR const* ir,
        OUT MDSet * bb_mustdefmds,
        DefDBitSetCore * mustgen_stmt,
        ConstMDIter & mditer,
        DefMiscBitSetMgr & bsmgr,
        UINT flag)
{
    switch (ir->get_code()) {
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
        if (!HAVE_FLAG(flag, COMPUTE_NOPR_DU)) { return; }
        break;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_CALL:
    case IR_ICALL:
    case IR_PHI:
        if (!HAVE_FLAG(flag, COMPUTE_PR_DU)) { return; }
        break;
    case IR_REGION:
        //Region does not have any def.
        break;
    default: //Handle general stmt.
        ASSERT0(!ir->isMemoryRef());
    }

    MD const* x = ir->getExactRef();
    if (x != NULL) {
        //call may not have return value.
        bb_mustdefmds->bunion(x, bsmgr);

        //Add MD which is exact and overlapped with x.
        m_md_sys->computeOverlapExactMD(x, bb_mustdefmds, mditer, bsmgr);
    } else if (ir->isWritePR() ||
               (ir->isCallStmt() && ir->hasReturnValue())) {
        ASSERT0(ir->getRefMD());
        bb_mustdefmds->bunion(ir->getRefMD(), bsmgr);
    }

    //Computing Must GEN set of reach-definition.
    SEGIter * st = NULL;
    INT ni;
    for (INT i = mustgen_stmt->get_first(&st); i != -1; i = ni) {
        ni = mustgen_stmt->get_next(i, &st);
        IR * gened_ir = m_ru->getIR(i);
        ASSERT0(gened_ir != NULL && gened_ir->is_stmt());
        if (is_may_kill(ir, gened_ir)) {
            mustgen_stmt->diff(i, bsmgr);
        }
    }
    mustgen_stmt->bunion(ir->id(), bsmgr);
}


//NOTE: MD referrence must be available.
//mustdefs: record must modified MD for each bb.
//maydefs: record may modified MD for each bb.
//mayuse: record may used MD for each bb.
void IR_DU_MGR::computeMustExactDefMayDefMayUse(
        OUT Vector<MDSet*> * mustdefmds,
        OUT Vector<MDSet*> * maydefmds,
        OUT MDSet * mayusemds,
        UINT flag,
        DefMiscBitSetMgr & bsmgr)
{
    if (HAVE_FLAG(flag, SOL_REACH_DEF) ||
        HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF)) {
        ASSERT0(mustdefmds);
    }

    if (HAVE_FLAG(flag, SOL_REACH_DEF) ||
        HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF) ||
        HAVE_FLAG(flag, SOL_AVAIL_EXPR)) {
        ASSERT0(maydefmds);
    }

    if (HAVE_FLAG(flag, SOL_RU_REF)) {
        ASSERT0(mayusemds);
    }

    ConstMDIter mditer;
    BBList * bbl = m_ru->getBBList();
    C<IRBB*> * ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        DefDBitSetCore * maygen_stmt = NULL;
        DefDBitSetCore * mustgen_stmt = NULL;
        MDSet * bb_mustdefmds = NULL;
        MDSet * bb_maydefmds = NULL;
        UINT bbid = BB_id(bb);
        if (mustdefmds != NULL) {
            bb_mustdefmds = mustdefmds->get(bbid);
            mustgen_stmt = getMustGenDef(bbid, &bsmgr);
            mustgen_stmt->clean(bsmgr);
        }

        if (maydefmds != NULL) {
            bb_maydefmds = maydefmds->get(bbid);
            maygen_stmt = getMayGenDef(bbid, &bsmgr);
            maygen_stmt->clean(bsmgr);
        }

        //may_def_mds, must_def_mds should be already clean.
        C<IR*> * irct;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
            IR const* ir = irct->val();
            ASSERT0(ir);
            if (mayusemds != NULL) {
                collectMayUseRecursive(ir, *mayusemds,
                    HAVE_FLAG(flag, COMPUTE_PR_DU), bsmgr);
                //collectMayUse(ir, *mayusemds, isComputePRDU());
            }

            if (!ir->hasResult()) { continue; }

            //Do not compute MustExactDef/MayDef for PR.
            if (ir->isWritePR() &&
                !HAVE_FLAG(flag, COMPUTE_PR_DU)) {
                continue;
            }

            //Collect mustdef mds.
            if (bb_mustdefmds != NULL) {
                computeMustExactDef(ir, bb_mustdefmds,
                    mustgen_stmt, mditer, bsmgr, flag);
            }

            if (bb_maydefmds != NULL) {
                computeMayDef(ir, bb_maydefmds, maygen_stmt, bsmgr, flag);
            }
        }
    }
}


//Compute Defined, Used md-set, Generated ir-stmt-set, and
//MayDefined md-set for each IR.
void IR_DU_MGR::computeCallRef(UINT duflag)
{
    ASSERT0(m_aa);
    BBList * bbl = m_ru->getBBList();
    C<IRBB*> * bbct;
    for (bbl->get_head(&bbct); bbct != NULL; bbct = bbl->get_next(bbct)) {
        IRBB * bb = bbct->val();
        C<IR*> * irct;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end();
             irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            switch (ir->get_code()) {
            case IR_CALL:
            case IR_ICALL:
                {
                    //Because CALL is always the last ir in BB, the
                    //querying process is only executed once per BB.
                    MD2MDSet * mx = NULL;
                    if (m_aa->isFlowSensitive()) {
                        mx = m_aa->mapBBtoMD2MDSet(BB_id(bb));
                    } else {
                        mx = m_aa->getUniqueMD2MDSet();
                    }
                    inferCallAndIcall(ir, duflag, mx);
                }
                break;
            default:;
            }
        }
    }
}


//Compute Defined, Used md-set, Generated ir-stmt-set, and
//MayDefined md-set for each IR.
void IR_DU_MGR::computeMDRef(IN OUT OptCtx & oc, UINT duflag)
{
    m_cached_overlap_mdset.clean();
    m_is_cached_mdset.clean(*m_misc_bs_mgr);

    ASSERT0(m_aa);
    BBList * bbl = m_ru->getBBList();
    C<IRBB*> * bbct;
    for (bbl->get_head(&bbct); bbct != NULL; bbct = bbl->get_next(bbct)) {
        IRBB * bb = bbct->val();

        C<IR*> * irct;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end();
             irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            switch (ir->get_code()) {
            case IR_ST:
                inferStore(ir, duflag);
                break;
            case IR_STPR:
                inferStorePR(ir, duflag);
                break;
            case IR_SETELEM:
                inferSetelem(ir, duflag);
                break;
            case IR_GETELEM:
                inferGetelem(ir, duflag);
                break;
            case IR_STARRAY:
                inferStoreArray(ir, duflag);
                break;
            case IR_IST:
                inferIstore(ir, duflag);
                break;
            case IR_CALL:
            case IR_ICALL:
                {
                    //Because CALL is always the last ir in BB, the
                    //querying process is only executed once per BB.
                    MD2MDSet * mx = NULL;
                    if (m_aa->isFlowSensitive()) {
                        mx = m_aa->mapBBtoMD2MDSet(BB_id(bb));
                    } else {
                        mx = m_aa->getUniqueMD2MDSet();
                    }
                    inferCallAndIcall(ir, duflag, mx);
                }
                break;
            case IR_RETURN:
                computeExpression(RET_exp(ir), NULL,
                    COMP_EXP_RECOMPUTE, duflag);
                break;
            case IR_TRUEBR:
            case IR_FALSEBR:
                //Compute USE mdset.
                ASSERT0(BR_lab(ir));
                computeExpression(BR_det(ir), NULL,
                    COMP_EXP_RECOMPUTE, duflag);
                break;
            case IR_SWITCH:
                //Compute USE mdset.
                computeExpression(SWITCH_vexp(ir), NULL,
                    COMP_EXP_RECOMPUTE, duflag);
                break;
            case IR_IGOTO:
                //Compute USE mdset.
                computeExpression(IGOTO_vexp(ir), NULL,
                    COMP_EXP_RECOMPUTE, duflag);
                break;
            case IR_GOTO:
                break;
            case IR_PHI:
                inferPhi(ir, duflag);
                break;
            case IR_REGION:
                //The memory reference information of Region
                //should already be avaiable.
                break;
            default: UNREACH();
            }
        }
    }

    OC_is_ref_valid(oc) = true;
    ASSERT0(m_ru->verifyMDRef());
}


//Compute must and may killed stmt.
//mustdefs: record must modified MD for each bb.
//maydefs: record may modified MD for each bb.
//flag: switches.
//NOTE: compute maykill and mustkill both need may-gen-def.
void IR_DU_MGR::computeKillSet(
        DefDBitSetCoreReserveTab & dbitsetchash,
        Vector<MDSet*> const* mustexactdefmds,
        Vector<MDSet*> const* maydefmds,
        DefMiscBitSetMgr & bsmgr)
{
    ASSERT0(mustexactdefmds || maydefmds);

    BBList * ir_bb_list = m_ru->getBBList();
    DefDBitSetCore univers(SOL_SET_IS_SPARSE);
    C<IRBB*> * ct;
    for (ir_bb_list->get_head(&ct);
         ct != ir_bb_list->end(); ct = ir_bb_list->get_next(ct)) {
        IRBB * bb = ct->val();
        univers.bunion(*getMayGenDef(BB_id(bb), &bsmgr), bsmgr);
    }

    DefDBitSetCore tmpstmtset(SOL_SET_IS_SPARSE);
    DefDBitSetCore must_killed_set(SOL_SET_IS_SPARSE);
    DefDBitSetCore may_killed_set(SOL_SET_IS_SPARSE);

    for (ir_bb_list->get_head(&ct);
         ct != ir_bb_list->end(); ct = ir_bb_list->get_next(ct)) {
        IRBB * bb = ct->val();
        UINT bbid = BB_id(bb);
        DefDBitSetCore const* maygendef = getMayGenDef(bbid, &bsmgr);

        bool comp_must = false;
        MDSet const* bb_mustdef_mds = NULL;
        if (mustexactdefmds != NULL) {
            bb_mustdef_mds = mustexactdefmds->get(bbid);
            if (bb_mustdef_mds != NULL && !bb_mustdef_mds->is_empty()) {
                comp_must = true;
            }
        }

        bool comp_may = false;
        MDSet const* bb_maydef_mds = NULL;
        if (maydefmds != NULL) {
            //Compute may killed stmts.
            bb_maydef_mds = maydefmds->get(bbid);
            if (bb_maydef_mds != NULL && !bb_maydef_mds->is_empty()) {
                comp_may = true;
            }
        }

        if (comp_must || comp_may) {
            SEGIter * st = NULL;
            tmpstmtset.copy(univers, bsmgr);
            tmpstmtset.diff(*maygendef, bsmgr);
            for (INT i = tmpstmtset.get_first(&st);
                 i != -1; i = tmpstmtset.get_next(i, &st)) {
                IR const* stmt = m_ru->getIR(i);
                ASSERT0(stmt->is_stmt());
                if (comp_must) {
                    MD const* stmt_mustexactdef_md = stmt->getExactRef();
                    if (stmt_mustexactdef_md == NULL) { continue; }
                    if (bb_mustdef_mds->is_contain(stmt_mustexactdef_md)) {
                        must_killed_set.bunion(i, bsmgr);
                    }
                }

                if (comp_may) {
                    //Compute may killed stmts, for avail-reach-def.
                    MD const* stmt_effectdef_md = stmt->getEffectRef();
                    if (stmt_effectdef_md != NULL &&
                        bb_mustdef_mds->is_contain(stmt_effectdef_md)) {
                        may_killed_set.bunion(i, bsmgr);
                        continue;
                    }

                    MDSet const* maydef = getMayDef(stmt);
                    if (maydef == NULL) { continue; }
                    if (bb_maydef_mds->is_intersect(*maydef)) {
                        may_killed_set.bunion(i, bsmgr);
                    }
                }
            }
        }

        if (comp_must) {
            setMustKilledDef(bbid, dbitsetchash.append(must_killed_set));
        } else {
            setMustKilledDef(bbid, NULL);
        }

        if (comp_may) {
            setMayKilledDef(bbid, dbitsetchash.append(may_killed_set));
        } else {
            setMayKilledDef(bbid, NULL);
        }

        must_killed_set.clean(bsmgr);
        may_killed_set.clean(bsmgr);
        tmpstmtset.clean(bsmgr);
    }

    ASSERT0(must_killed_set.is_empty() &&
            may_killed_set.is_empty() &&
            tmpstmtset.is_empty());

    univers.clean(bsmgr);
}


//Return true if ir can be candidate of live-expr.
bool IR_DU_MGR::canBeLiveExprCand(IR const* ir) const
{
    ASSERT0(ir);
    switch (ir->get_code()) {
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
    case IR_SELECT:
    case IR_PR:
        return true;
    default: break;
    }
    return false;
}


//Compute generated-EXPR for BB.
void IR_DU_MGR::computeGenForBB(
        IRBB * bb,
        OUT DefDBitSetCore & expr_univers,
        DefMiscBitSetMgr & bsmgr)
{
    MDSet tmp;
    DefDBitSetCore * gen_ir_exprs = getGenIRExpr(BB_id(bb), &bsmgr);
    gen_ir_exprs->clean(bsmgr);

    C<IR*> * ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end(); ct = BB_irlist(bb).get_next(ct)) {
        IR const* ir = ct->val();
        ASSERT0(ir->is_stmt());
        switch (ir->get_code()) {
        case IR_ST:
            if (canBeLiveExprCand(ST_rhs(ir))) {
                //Compute the generated expressions set.
                gen_ir_exprs->bunion(IR_id(ST_rhs(ir)), bsmgr);
                expr_univers.bunion(IR_id(ST_rhs(ir)), bsmgr);
            }
            //Fall through.
        case IR_STPR:
            if (ir->is_stpr() && canBeLiveExprCand(STPR_rhs(ir))) {
                //Compute the generated expressions set.
                gen_ir_exprs->bunion(IR_id(STPR_rhs(ir)), bsmgr);
                expr_univers.bunion(IR_id(STPR_rhs(ir)), bsmgr);
            }
            //Fall through.
        case IR_STARRAY:
            if (ir->is_starray() && canBeLiveExprCand(STARR_rhs(ir))) {
                //Compute the generated expressions set.
                gen_ir_exprs->bunion(IR_id(STARR_rhs(ir)), bsmgr);
                expr_univers.bunion(IR_id(STARR_rhs(ir)), bsmgr);
            }
            //Fall through.
        case IR_IST:
            if (ir->is_ist()) {
                //Compute the generated expressions set.
                if (canBeLiveExprCand(IST_rhs(ir))) {
                    gen_ir_exprs->bunion(IR_id(IST_rhs(ir)), bsmgr);
                    expr_univers.bunion(IR_id(IST_rhs(ir)), bsmgr);
                }

                if (canBeLiveExprCand(IST_base(ir))) {
                    //e.g: *(int*)0x1000 = 10, IST_base(ir) is NULL.
                    gen_ir_exprs->bunion(IR_id(IST_base(ir)), bsmgr);
                    expr_univers.bunion(IR_id(IST_base(ir)), bsmgr);
                }
            }

            {
                //Compute lived IR expression after current statement executed.
                //e.g:
                //  i = i + 1 //S1
                //
                //  lhs 'i' killed the rhs expression: 'i + 1', that means
                //  'i + 1' is dead after S1 statement.
                MDSet const* maydef = getMayDef(ir);
                MD const* mustdef = ir->getEffectRef();
                if (maydef != NULL || mustdef != NULL) {
                    SEGIter * st = NULL;
                    for (INT j = gen_ir_exprs->get_first(&st), nj;
                         j != -1; j = nj) {
                        nj = gen_ir_exprs->get_next(j, &st);

                        IR * tir = m_ru->getIR(j);
                        ASSERT0(tir != NULL);

                        if (tir->is_lda() || tir->is_const()) {
                            continue;
                        }

                        tmp.clean(bsmgr);

                        collectMayUseRecursive(tir, tmp, true, bsmgr);
                        //collectMayUse(tir, tmp, true);

                        if ((maydef != NULL &&
                             maydef->is_intersect(tmp)) ||
                            (mustdef != NULL &&
                             tmp.is_contain(mustdef))) {
                            //'ir' killed 'tir'.
                            gen_ir_exprs->diff(j, bsmgr);
                        }
                    }
                }
            }
            break;
        case IR_CALL:
        case IR_ICALL:
            {
                //Compute the generated expressions set.
                if (ir->is_icall()) {
                    ASSERT0(ICALL_callee(ir)->is_ld());
                    if (canBeLiveExprCand(ICALL_callee(ir))) {
                        gen_ir_exprs->bunion(IR_id(ICALL_callee(ir)), bsmgr);
                        expr_univers.bunion(IR_id(ICALL_callee(ir)), bsmgr);
                    }
                }

                for (IR * p = CALL_param_list(ir);
                     p != NULL; p = p->get_next()) {
                    if (canBeLiveExprCand(p)) {
                        gen_ir_exprs->bunion(IR_id(p), bsmgr);
                        expr_univers.bunion(IR_id(p), bsmgr);
                    }
                }

                //Compute lived IR expression after current statement executed.
                //e.g:
                //  i = i + 1 //S1
                //
                //  lhs 'i' killed the rhs expression: 'i + 1', that means
                //  'i + 1' is dead after S1 statement.
                MDSet const* maydef = getMayDef(ir);
                MD const* mustdef = ir->getEffectRef();
                if (maydef != NULL || mustdef != NULL) {
                    SEGIter * st = NULL;
                    for (INT j = gen_ir_exprs->get_first(&st), nj;
                         j != -1; j = nj) {
                        nj = gen_ir_exprs->get_next(j, &st);
                        IR * tir = m_ru->getIR(j);
                        ASSERT0(tir != NULL);
                        if (tir->is_lda() || tir->is_const()) {
                            continue;
                        }

                        tmp.clean(bsmgr);

                        collectMayUseRecursive(tir, tmp, true, bsmgr);
                        //collectMayUse(tir, tmp, true);

                        if ((maydef != NULL && maydef->is_intersect(tmp)) ||
                            (mustdef != NULL && tmp.is_contain(mustdef))) {
                            //'ir' killed 'tir'.
                            gen_ir_exprs->diff(j, bsmgr);
                        }
                    }
                }
            }
            break;
        case IR_REGION:
        case IR_GOTO:
            break;
        case IR_IGOTO:
            //Compute the generated expressions.
            if (canBeLiveExprCand(IGOTO_vexp(ir))) {
                gen_ir_exprs->bunion(IR_id(IGOTO_vexp(ir)), bsmgr);
                expr_univers.bunion(IR_id(IGOTO_vexp(ir)), bsmgr);
            }
            break;
        case IR_DO_WHILE:
        case IR_WHILE_DO:
        case IR_DO_LOOP:
        case IR_IF:
        case IR_LABEL:
        case IR_CASE:
            ASSERT(0, ("TODO"));
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            //Compute the generated expressions.
            if (canBeLiveExprCand(BR_det(ir))) {
                gen_ir_exprs->bunion(IR_id(BR_det(ir)), bsmgr);
                expr_univers.bunion(IR_id(BR_det(ir)), bsmgr);
            }
            break;
        case IR_SWITCH:
            //Compute the generated expressions.
            if (canBeLiveExprCand(SWITCH_vexp(ir))) {
                gen_ir_exprs->bunion(IR_id(SWITCH_vexp(ir)), bsmgr);
                expr_univers.bunion(IR_id(SWITCH_vexp(ir)), bsmgr);
            }
            break;
        case IR_RETURN:
            if (RET_exp(ir) != NULL) {
                if (canBeLiveExprCand(RET_exp(ir))) {
                    gen_ir_exprs->bunion(IR_id(RET_exp(ir)), bsmgr);
                    expr_univers.bunion(IR_id(RET_exp(ir)), bsmgr);
                }
            }
            break;
        case IR_PHI:
            for (IR * p = PHI_opnd_list(ir); p != NULL; p = p->get_next()) {
                if (canBeLiveExprCand(p)) {
                    gen_ir_exprs->bunion(IR_id(p), bsmgr);
                    expr_univers.bunion(IR_id(p), bsmgr);
                }
            }
            break;
        default: UNREACH();
        }
    }
    tmp.clean(bsmgr);
}


//Compute local-gen IR-EXPR set and killed IR-EXPR set.
//'expr_universe': record the universal of all ir-expr of region.
void IR_DU_MGR::computeAuxSetForExpression(
        DefDBitSetCoreReserveTab & dbitsetchash,
        OUT DefDBitSetCore * expr_universe,
        Vector<MDSet*> const* maydefmds,
        DefMiscBitSetMgr & bsmgr)
{
    ASSERT0(expr_universe && maydefmds);
    BBList * bbl = m_ru->getBBList();
    C<IRBB*> * ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        computeGenForBB(bb, *expr_universe, bsmgr);
    }

    //Compute kill-set.
    //The defined MDSet of current ir, killed all
    //other exprs which used MDSet that modified by 'ir'.
    DefDBitSetCore killed_set(SOL_SET_IS_SPARSE);
    MDSet tmp;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        MDSet const* bb_maydef = maydefmds->get(BB_id(bb));
        ASSERT0(bb_maydef != NULL);

        SEGIter * st = NULL;
        for (INT i = expr_universe->get_first(&st);
             i != -1; i = expr_universe->get_next(i, &st)) {
            IR * ir = m_ru->getIR(i);
            ASSERT0(ir->is_exp());
            if (ir->is_lda() || ir->is_const()) { continue; }

            tmp.clean(bsmgr);
            collectMayUseRecursive(ir, tmp, true, bsmgr);
            //collectMayUse(ir, *tmp, true);

            if (bb_maydef->is_intersect(tmp)) {
                killed_set.bunion(i, bsmgr);
            }
        }

        setKilledIRExpr(BB_id(bb), dbitsetchash.append(killed_set));
        killed_set.clean(bsmgr);
    }

    tmp.clean(bsmgr);
}


//This equation needs May Kill Def and Must Gen Def.
bool IR_DU_MGR::ForAvailReachDef(
        UINT bbid,
        List<IRBB*> & preds,
        List<IRBB*> * lst,
        DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    DefDBitSetCore news(SOL_SET_IS_SPARSE);
    DefDBitSetCore * in = getAvailInReachDef(bbid, m_misc_bs_mgr);
    bool first = true;
    C<IRBB*> * ct;
    for (preds.get_head(&ct); ct != preds.end(); ct = preds.get_next(ct)) {
        IRBB * p = ct->val();
        //Intersect
        if (first) {
            first = false;
            in->copy(*getAvailOutReachDef(BB_id(p), &bsmgr), *m_misc_bs_mgr);
        } else {
            in->intersect(*getAvailOutReachDef(BB_id(p), &bsmgr),
                *m_misc_bs_mgr);
            //in->bunion(*getAvailOutReachDef(BB_id(p)), *m_misc_bs_mgr);
        }
    }

    news.copy(*in, bsmgr);
    DefDBitSetCore const* killset = getMayKilledDef(bbid);
    if (killset != NULL) {
        news.diff(*killset, bsmgr);
    }
    news.bunion(*getMustGenDef(bbid, &bsmgr), bsmgr);

    DefDBitSetCore * out = getAvailOutReachDef(bbid, &bsmgr);
    if (!out->is_equal(news)) {
        out->copy(news, bsmgr);
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        Vertex * bbv = m_cfg->get_vertex(bbid);
        EdgeC const* ecs = VERTEX_out_list(bbv);
        while (ecs != NULL) {
            INT succ = VERTEX_id(EDGE_to(EC_edge(ecs)));
            ASSERT0(succ >= 0 && m_cfg->getBB(succ));
            lst->append_tail(m_cfg->getBB(succ));
            ecs = EC_next(ecs);
        }
        #endif
    }
    news.clean(bsmgr);
    return change;
}


bool IR_DU_MGR::ForReachDef(
        UINT bbid,
        List<IRBB*> & preds,
        List<IRBB*> * lst,
        DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    DefDBitSetCore * in_reach_def = getInReachDef(bbid, m_misc_bs_mgr);
    DefDBitSetCore news(SOL_SET_IS_SPARSE);

    bool first = true;
    C<IRBB*> * ct;
    for (preds.get_head(&ct); ct != preds.end(); ct = preds.get_next(ct)) {
        IRBB const* p = ct->val();
        if (first) {
            in_reach_def->copy(*getOutReachDef(BB_id(p), &bsmgr),
                *m_misc_bs_mgr);
            first = false;
        } else {
            in_reach_def->bunion(*getOutReachDef(BB_id(p), &bsmgr),
                *m_misc_bs_mgr);
        }
    }

    if (first) {
        //bb does not have predecessor.
        ASSERT0(in_reach_def->is_empty());
    }

    news.copy(*in_reach_def, bsmgr);
    DefDBitSetCore const* killset = getMustKilledDef(bbid);
    if (killset != NULL) {
        news.diff(*killset, bsmgr);
    }
    news.bunion(*getMayGenDef(bbid, &bsmgr), bsmgr);

    DefDBitSetCore * out_reach_def = getOutReachDef(bbid, &bsmgr);
    if (!out_reach_def->is_equal(news)) {
        out_reach_def->copy(news, bsmgr);
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        Vertex * bbv = m_cfg->get_vertex(bbid);
        EdgeC const* ecs = VERTEX_out_list(bbv);
        while (ecs != NULL) {
            INT succ = VERTEX_id(EDGE_to(EC_edge(ecs)));
            ASSERT0(succ >= 0 && m_cfg->getBB(succ));
            lst->append_tail(m_cfg->getBB(succ));
            ecs = EC_next(ecs);
        }
        #endif
    }

    news.clean(bsmgr);
    return change;
}


bool IR_DU_MGR::ForAvailExpression(
            UINT bbid,
            List<IRBB*> & preds,
            List<IRBB*> * lst,
            DefMiscBitSetMgr & bsmgr)
{
    DUMMYUSE(lst);
    bool change = false;
    DefDBitSetCore news(SOL_SET_IS_SPARSE);

    bool first = true;
    DefDBitSetCore * in = getAvailInExpr(bbid, m_misc_bs_mgr);
    C<IRBB*> * ct;
    for (preds.get_head(&ct); ct != preds.end(); ct = preds.get_next(ct)) {
        IRBB * p = ct->val();
        DefDBitSetCore * liveout = getAvailOutExpr(BB_id(p), &bsmgr);
        if (first) {
            first = false;
            in->copy(*liveout, *m_misc_bs_mgr);
        } else {
            in->intersect(*liveout, *m_misc_bs_mgr);
        }
    }

    news.copy(*in, bsmgr);
    DefDBitSetCore const* set = getKilledIRExpr(bbid);
    if (set != NULL) {
        news.diff(*set, bsmgr);
    }
    news.bunion(*getGenIRExpr(bbid, &bsmgr), bsmgr);
    DefDBitSetCore * out = getAvailOutExpr(bbid, &bsmgr);
    if (!out->is_equal(news)) {
        out->copy(news, bsmgr);
        change = true;

        #ifdef WORK_LIST_DRIVE
        ASSERT0(lst);
        Vertex * bbv = m_cfg->get_vertex(bbid);
        EdgeC const* ecs = VERTEX_out_list(bbv);
        while (ecs != NULL) {
            INT succ = VERTEX_id(EDGE_to(EC_edge(ecs)));
            ASSERT0(succ >= 0 && m_cfg->getBB(succ));
            lst->append_tail(m_cfg->getBB(succ));
            ecs = EC_next(ecs);
        }
        #endif
    }
    news.clean(bsmgr);
    return change;
}


//Solve reaching definitions problem for IR STMT and
//computing LIVE IN and LIVE OUT IR expressions.
//'expr_univers': the Universal SET for ExpRep.
void IR_DU_MGR::solve(
        DefDBitSetCore const& expr_univers,
        UINT const flag,
        DefMiscBitSetMgr & bsmgr)
{
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_tail(); bb != NULL; bb = bbl->get_prev()) {
        UINT bbid = BB_id(bb);
        if (HAVE_FLAG(flag, SOL_REACH_DEF)) {
            //Initialize reach-def IN, reach-def OUT.
            getInReachDef(bbid, m_misc_bs_mgr)->clean(*m_misc_bs_mgr);
        }

        if (HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF)) {
            getAvailInReachDef(bbid, m_misc_bs_mgr)->clean(*m_misc_bs_mgr);
        }

        if (HAVE_FLAG(flag, SOL_AVAIL_EXPR)) {
            //Initialize available in, available out expression.
            //IN-SET of BB must be universal of all IR-expressions.
            DefDBitSetCore * availin = getAvailInExpr(bbid, m_misc_bs_mgr);
            availin->copy(expr_univers, *m_misc_bs_mgr);

            DefDBitSetCore * liveout = getAvailOutExpr(bbid, &bsmgr);
            liveout->copy(*availin, bsmgr);

            DefDBitSetCore const* set = getKilledIRExpr(bbid);
            if (set != NULL) {
                liveout->diff(*set, bsmgr);
            }

            liveout->bunion(*getGenIRExpr(bbid, &bsmgr), bsmgr);
        }
    }

    //Rpo already checked to be available. Here double check again.
    List<IRBB*> * tbbl = m_cfg->getBBListInRPO();
    ASSERT0(tbbl->get_elem_count() == bbl->get_elem_count());
    List<IRBB*> preds;
    List<IRBB*> lst;
#ifdef WORK_LIST_DRIVE
    C<IRBB*> * ct;
    for (tbbl->get_head(&ct); ct != tbbl->end(); ct = tbbl->get_next(ct)) {
        IRBB * p = ct->val();
        lst.append_tail(p);
    }

    UINT count = tbbl->get_elem_count() * 20;
    UINT i = 0; //time of bb accessed.
    do {
        IRBB * bb = lst.remove_head();
        UINT bbid = BB_id(bb);
        preds.clean();
        m_cfg->get_preds(preds, bb);
        if (HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF)) {
            ForAvailReachDef(bbid, preds, &lst, bsmgr);
        }
        if (HAVE_FLAG(flag, SOL_REACH_DEF)) {
            ForReachDef(bbid, preds, &lst, bsmgr);
        }
        if (HAVE_FLAG(flag, SOL_AVAIL_EXPR)) {
            ForAvailExpression(bbid, preds, &lst, bsmgr);
        }
        i++;
    } while (lst.get_elem_count() != 0);
    ASSERT0(i < count);
    DUMMYUSE(count);
#else
    bool change;
    UINT count = 0;
    do {
        change = false;
        C<IRBB*> * ct;
        for (tbbl->get_head(&ct); ct != tbbl->end(); ct = tbbl->get_next(ct)) {
            IRBB * bb = ct->val();
            UINT bbid = BB_id(bb);
            preds.clean();
            m_cfg->get_preds(preds, bb);
            if (HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF)) {
                change |= ForAvailReachDef(bbid, preds, NULL, bsmgr);
            }
            if (HAVE_FLAG(flag, SOL_REACH_DEF)) {
                change |= ForReachDef(bbid, preds, NULL, bsmgr);
            }
            if (HAVE_FLAG(flag, SOL_AVAIL_EXPR)) {
                change |= ForAvailExpression(bbid, preds, NULL, bsmgr);
            }
        }
        count++;
    } while (change && count < 20);
    //UINT i = count * tbbl->get_elem_count(); //time of bb accessed.
    ASSERT0(!change);
#endif
}


//Check if stmt is killing-define to exp.
//This function matchs the pattern, where dt means data-type and
//they must be identical.
//    ist(dt, ofst:n), x = ...
//    ... = ild(dt, ofst:n), x
//Stmt and exp must be in same bb.
UINT IR_DU_MGR::checkIsLocalKillingDef(
        IR const* stmt,
        IR const* exp,
        C<IR*> * expct)
{
    ASSERT0(stmt->getBB() == exp->get_stmt()->getBB());

    if (!exp->is_ild() || !stmt->is_ist()) { return CK_UNKNOWN; }

    IR const* t = ILD_base(exp);

    while (t->is_cvt()) { t = CVT_exp(t); }

    if (!t->is_pr() && !t->is_ld()) { return CK_UNKNOWN; }

    IR const* t2 = IST_base(stmt);

    while (t2->is_cvt()) { t2 = CVT_exp(t2); }

    if (!t2->is_pr() && !t2->is_ld()) { return CK_UNKNOWN; }

    if (t->get_code() != t2->get_code()) { return CK_UNKNOWN; }

    IRBB * curbb = stmt->getBB();

    //Note, the defset of t must be avaiable here.
    //And t could not be defined between stmt and exp.
    //e.g:
    //    *base = ...
    //    ... //base can not be modified in between ist and ild.
    //    = *base
    DUSet const* defset_of_t = t->readDUSet();
    if (defset_of_t != NULL) {
        DUIter di = NULL;
        for (UINT d = defset_of_t->get_first(&di);
             di != NULL; d = defset_of_t->get_next(d, &di)) {
            IR const* def_of_t = m_ru->getIR(d);
            ASSERT0(def_of_t->is_stmt());
            if (def_of_t->getBB() != curbb) { continue; }

            //Find the def that clobber exp after the stmt.
            C<IR*> * localct(expct);
            for (IR * ir = BB_irlist(curbb).get_prev(&localct);
                 ir != NULL; ir = BB_irlist(curbb).get_prev(&localct)) {
                if (ir == stmt) { break; }
                else if (ir == def_of_t) { return CK_UNKNOWN; }
            }
        }
    }

    if (exp->is_void() || stmt->is_void()) {
        return CK_OVERLAP;
    }

    UINT exptysz = exp->get_type_size(m_tm);
    UINT stmttysz = stmt->get_type_size(m_tm);
    if ((((ILD_ofst(exp) + exptysz) <= IST_ofst(stmt)) ||
        ((IST_ofst(stmt) + stmttysz) <= ILD_ofst(exp)))) {
        return CK_NOT_OVERLAP;
    }
    return CK_OVERLAP;
}


//Find nearest killing def to expmd in its bb.
//Here we search exactly killing DEF from current stmt to previous
//for expmd even if it is exact,
//e.g: g is global variable, it is exact.
//x is a pointer that we do not know where it pointed to.
//    1. *x += 1;
//    2. g = 0;
//    3. *x += 2; # *x may overlapped with global variable g.
//    4. return g;
//In the case, the last reference of g in stmt 4 may be defined by
//stmt 2 or 3.
IR const* IR_DU_MGR::findKillingLocalDef(
            IRBB * bb,
            C<IR*> * ct,
            IR const* exp,
            MD const* expmd)
{
    ASSERT(expmd->is_exact() || exp->isReadPR(),
           ("only exact md or PR has killing-def"));

    C<IR*> * localct(ct);
    for (IR * ir = BB_irlist(bb).get_prev(&localct);
         ir != NULL; ir = BB_irlist(bb).get_prev(&localct)) {
        if (!ir->isMemoryRef()) { continue; }

        ASSERT(!ir->isCallStmt(),
               ("call should not appear in local processing"));

        MD const* mustdef = ir->getRefMD();
        if (mustdef != NULL) {
            if (mustdef == expmd || mustdef->is_overlap(expmd)) {
                if (mustdef->is_exact() || ir->isMustWritePR()) {
                    return ir;
                }

                UINT result = checkIsLocalKillingDef(ir, exp, ct);
                if (result == CK_OVERLAP) {
                    return ir;
                } else if (result == CK_NOT_OVERLAP) {
                    continue;
                }

                //ir is neither inexact, nor the nonkilling def of exp.
                return NULL;
            }

            //If both def and exp has must reference, we do not need to
            //check maydef.
            continue;
        }

        //We need to check maydef.
        MDSet const* maydefs = ir->getRefMDSet();
        if (maydefs != NULL && maydefs->is_contain(expmd)) {
            //There is a nonkilling DEF, ir may modified expmd.
            //The subsequent searching is meaningless.
            //We can not find a local killing DEF.
            return NULL;
        }
    }

    //We can not find a local killing DEF.
    return NULL;
}


//Build DU chain for exp(expmd) and local killing def stmt.
//Return true if find local killing def, otherwise means
//there is not local killing def.
bool IR_DU_MGR::buildLocalDUChain(
        IRBB * bb,
        IR const* exp,
        MD const* expmd,
        DUSet * expdu,
        C<IR*> * ct)
{
    IR const* nearest_def = findKillingLocalDef(bb, ct, exp, expmd);
    if (nearest_def == NULL) { return false; }

    ASSERT0(expdu);
    expdu->addDef(nearest_def, *m_misc_bs_mgr);

    DUSet * xdu = nearest_def->getDUSet();
    ASSERT0(xdu);
    if (!m_is_init->is_contain(IR_id(nearest_def))) {
        m_is_init->bunion(IR_id(nearest_def));
        xdu->clean(*m_misc_bs_mgr);
    }
    xdu->addUse(exp, *m_misc_bs_mgr);
    return true;
}


//Check memory operand and build DU chain for them.
//Note we always find the nearest exact def, and build
//the DU between the def and its use.
void IR_DU_MGR::checkAndBuildChainRecursive(
        IRBB * bb,
        IR * exp,
        C<IR*> * ct,
        UINT flag)
{
    ASSERT0(exp && exp->is_exp());
    switch (exp->get_code()) {
    case IR_LD:
        if (!HAVE_FLAG(flag, COMPUTE_NOPR_DU)) { return; }
        break;
    case IR_PR:
        if (!HAVE_FLAG(flag, COMPUTE_PR_DU)) { return; }
        break;
    case IR_ILD:
        checkAndBuildChainRecursive(bb, ILD_base(exp), ct, flag);
        if (!HAVE_FLAG(flag, COMPUTE_NOPR_DU)) { return; }
        break;
    case IR_ARRAY:
        for (IR * sub = ARR_sub_list(exp);
             sub != NULL; sub = sub->get_next()) {
            checkAndBuildChainRecursive(bb, sub, ct, flag);
        }
        checkAndBuildChainRecursive(bb, ARR_base(exp), ct, flag);
        if (!HAVE_FLAG(flag, COMPUTE_NOPR_DU)) { return; }
        break;
    case IR_CONST:
    case IR_LDA:
        return;
    case IR_ADD:
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
    case IR_ASR:
    case IR_LSR:
    case IR_LSL:
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
    case IR_LAND:
    case IR_LOR:
        checkAndBuildChainRecursive(bb, BIN_opnd0(exp), ct, flag);
        checkAndBuildChainRecursive(bb, BIN_opnd1(exp), ct, flag);
        return;
    case IR_SELECT:
        checkAndBuildChainRecursive(bb, SELECT_pred(exp), ct, flag);
        checkAndBuildChainRecursive(bb, SELECT_trueexp(exp), ct, flag);
        checkAndBuildChainRecursive(bb, SELECT_falseexp(exp), ct, flag);
        return;
    case IR_BNOT:
    case IR_LNOT:
    case IR_NEG:
        checkAndBuildChainRecursive(bb, UNA_opnd(exp), ct, flag);
        return;
    case IR_CVT:
        checkAndBuildChainRecursive(bb, CVT_exp(exp), ct, flag);
        return;
    case IR_CASE:
        checkAndBuildChainRecursive(bb, CASE_vexp(exp), ct, flag);
        return;
    default: UNREACH();
    }

    checkAndBuildChainForMemIR(bb, exp, ct);
}


//Check memory operand and build DU chain for them.
//Note we always find the nearest exact def, and build
//the DU between the def and its use.
void IR_DU_MGR::checkAndBuildChainForMemIR(IRBB * bb, IR * exp, C<IR*> * ct)
{
    ASSERT0(exp && exp->is_exp());

    DUSet * expdu = getAndAllocDUSet(exp);
    if (!m_is_init->is_contain(IR_id(exp))) {
        m_is_init->bunion(IR_id(exp));
        expdu->clean(*m_misc_bs_mgr);
    }

    MD const* expmd = getMustUse(exp);
    bool has_local_killing_def = false;
    if ((expmd != NULL && expmd->is_exact()) || exp->isReadPR()) {
        //Only must-exact USE has qualification to compute killing-def.
        has_local_killing_def = buildLocalDUChain(bb, exp, expmd, expdu, ct);
    }

    if (has_local_killing_def) { return; }

    if (expmd != NULL) {
        //Find nonlocal-killing def for exp.
        checkMustMDAndBuildDUChain(exp, expmd, expdu);
    }

    if (expmd == NULL || m_md2irs->hasIneffectDef()) {
        MDSet const* expmds = getMayUse(exp);
        if (expmds != NULL) {
            checkMDSetAndBuildDUChain(exp, expmd, *expmds, expdu);
        }
    }
}


//Check memory operand and build DU chain for them.
//Note we always find the nearest exact def, and build
//the DU between the def and its use.
void IR_DU_MGR::checkAndBuildChain(IR * stmt, C<IR*> * ct, UINT flag)
{
    ASSERT0(stmt->is_stmt());
    switch (stmt->get_code()) {
    case IR_ST:
        {
            if (HAVE_FLAG(flag, COMPUTE_NOPR_DU)) {
                DUSet * du = getAndAllocDUSet(stmt);
                if (!m_is_init->is_contain(IR_id(stmt))) {
                    m_is_init->bunion(IR_id(stmt));
                    du->clean(*m_misc_bs_mgr);
                }
            }

            checkAndBuildChainRecursive(stmt->getBB(),
                stmt->getRHS(), ct, flag);
            return;
        }
    case IR_STPR:
        {
            if (HAVE_FLAG(flag, COMPUTE_PR_DU)) {
                DUSet * du = getAndAllocDUSet(stmt);
                if (!m_is_init->is_contain(IR_id(stmt))) {
                    m_is_init->bunion(IR_id(stmt));
                    du->clean(*m_misc_bs_mgr);
                }
            }

            checkAndBuildChainRecursive(stmt->getBB(),
                stmt->getRHS(), ct, flag);
            return;
        }
    case IR_IST:
        {
            if (HAVE_FLAG(flag, COMPUTE_NOPR_DU)) {
                DUSet * du = getAndAllocDUSet(stmt);
                if (!m_is_init->is_contain(IR_id(stmt))) {
                    m_is_init->bunion(IR_id(stmt));
                    du->clean(*m_misc_bs_mgr);
                }
            }
            checkAndBuildChainRecursive(stmt->getBB(),
                IST_base(stmt), ct, flag);
            checkAndBuildChainRecursive(stmt->getBB(),
                IST_rhs(stmt), ct, flag);
            return;
        }
    case IR_STARRAY:
        {
            if (HAVE_FLAG(flag, COMPUTE_NOPR_DU)) {
                DUSet * du = getAndAllocDUSet(stmt);
                if (!m_is_init->is_contain(IR_id(stmt))) {
                    m_is_init->bunion(IR_id(stmt));
                    du->clean(*m_misc_bs_mgr);
                }
            }

            for (IR * sub = ARR_sub_list(stmt);
                 sub != NULL; sub = sub->get_next()) {
                checkAndBuildChainRecursive(stmt->getBB(), sub, ct, flag);
            }
            checkAndBuildChainRecursive(stmt->getBB(),
                ARR_base(stmt), ct, flag);
            checkAndBuildChainRecursive(stmt->getBB(),
                STARR_rhs(stmt), ct, flag);
            return;
        }
    case IR_CALL:
        if (HAVE_FLAG(flag, COMPUTE_PR_DU)) {
            DUSet * du = getAndAllocDUSet(stmt);
            if (!m_is_init->is_contain(IR_id(stmt))) {
                m_is_init->bunion(IR_id(stmt));
                du->clean(*m_misc_bs_mgr);
            }
        }
        for (IR * p = CALL_param_list(stmt); p != NULL; p = p->get_next()) {
            checkAndBuildChainRecursive(stmt->getBB(), p, ct, flag);
        }
        for (IR * p = CALL_dummyuse(stmt); p != NULL; p = p->get_next()) {
            checkAndBuildChainRecursive(stmt->getBB(), p, ct, flag);
        }
        return;
    case IR_ICALL:
        for (IR * p = CALL_param_list(stmt); p != NULL; p = p->get_next()) {
            checkAndBuildChainRecursive(stmt->getBB(), p, ct, flag);
        }
        for (IR * p = CALL_dummyuse(stmt); p != NULL; p = p->get_next()) {
            checkAndBuildChainRecursive(stmt->getBB(), p, ct, flag);
        }
        checkAndBuildChainRecursive(stmt->getBB(),
            ICALL_callee(stmt), ct, flag);
        return;
    case IR_PHI:
        if (HAVE_FLAG(flag, COMPUTE_PR_DU)) {
            DUSet * du = getAndAllocDUSet(stmt);
            if (!m_is_init->is_contain(IR_id(stmt))) {
                m_is_init->bunion(IR_id(stmt));
                du->clean(*m_misc_bs_mgr);
            }
            for (IR * opnd = PHI_opnd_list(stmt);
                 opnd != NULL; opnd = opnd->get_next()) {
                checkAndBuildChainRecursive(stmt->getBB(), opnd, ct, flag);
            }
        }
        return;
    case IR_REGION:
    case IR_GOTO:
        return;
    case IR_IGOTO:
    case IR_SWITCH:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
        for (UINT i = 0; i < IR_MAX_KID_NUM(stmt); i++) {
            IR * kid = stmt->getKid(i);
            if (kid == NULL) { continue; }
            checkAndBuildChainRecursive(stmt->getBB(), kid, ct, flag);
        }
        return;
    default: ASSERT(0, ("unsupport"));
    }
}


//Check if stmt killing define exp, stmt and exp may be at same bb.
//e.g: BB is loop body,
//    start:
//    = *t
//    ...
//    *t = ...
//    goto start
UINT IR_DU_MGR::checkIsNonLocalKillingDef(IR const* stmt, IR const* exp)
{
    ASSERT0(m_oc);
    if (!OC_is_live_expr_valid(*m_oc)) { return CK_UNKNOWN; }

    if (!exp->is_ild() || !stmt->is_ist()) { return CK_UNKNOWN; }

    IR const* t = ILD_base(exp);
    while (t->is_cvt()) { t = CVT_exp(t); }
    if (!t->is_pr() && !t->is_ld()) { return CK_UNKNOWN; }

    IR const* t2 = IST_base(stmt);
    while (t2->is_cvt()) { t2 = CVT_exp(t2); }
    if (!t2->is_pr() && !t2->is_ld()) { return CK_UNKNOWN; }

    if (t->get_code() != t2->get_code()) { return CK_UNKNOWN; }

    //Note, t could not be modified in the path between stmt and exp.
    //e.g:
    //  *t = ...
    //  ... //t can not be defined.
    //  = *t
    DefDBitSetCore const* lived_in_expr =
        getAvailInExpr(BB_id(exp->get_stmt()->getBB()), m_misc_bs_mgr);
    if (!lived_in_expr->is_contain(IR_id(t2))) { return CK_UNKNOWN; }

    UINT exptysz = exp->get_type_size(m_tm);
    UINT stmttysz = stmt->get_type_size(m_tm);
    if ((((ILD_ofst(exp) + exptysz) <= IST_ofst(stmt)) ||
        ((IST_ofst(stmt) + stmttysz) <= ILD_ofst(exp)))) {
        return CK_NOT_OVERLAP;
    }
    return CK_OVERLAP;
}


void IR_DU_MGR::checkDefSetToBuildDUChain(
        IR const* exp,
        MD const* expmd,
        MDSet const* expmds,
        DUSet * expdu,
        DefSBitSetCore const* defset,
        IRBB * curbb)
{
    SEGIter * sc = NULL;
    if (exp->isReadPR()) {
        //Check DU for PR.
        ASSERT0(expmd);
        UINT const expid = IR_id(exp);
        for (INT d = defset->get_first(&sc);
             d >= 0; d = defset->get_next(d, &sc)) {
            IR * def = m_ru->getIR(d);
            ASSERT0(def->is_stmt());

            MD const* mustdef = get_must_def(def);
            if (expmd != mustdef) { continue; }

            //Build DU chain.
            expdu->add(d, *m_misc_bs_mgr);
            DUSet * def_useset = getAndAllocDUSet(def);
            if (!m_is_init->is_contain(d)) {
                m_is_init->bunion(d);
                def_useset->clean(*m_misc_bs_mgr);
            }
            def_useset->add(expid, *m_misc_bs_mgr);
        }
        return;
    }

    //Check DU for other kind memory reference.
    for (INT d = defset->get_first(&sc);
         d >= 0; d = defset->get_next(d, &sc)) {
        IR * def = m_ru->getIR(d);
        ASSERT0(def->is_stmt());

        bool build_du = false;
        MD const* mustdef = get_must_def(def);
        bool consider_maydef = false;
        if (expmd != NULL && mustdef != NULL) {
            //If def has MustDef (exact|effect) MD, then we do
            //not consider MayDef MDSet if the def is neither CALL|ICALL
            //nor REGION.
            ASSERT(!MD_is_may(mustdef), ("MayMD can not be mustdef."));
            if (expmd == mustdef || expmd->is_overlap(mustdef)) {
                if (mustdef->is_exact()) {
                    build_du = true;
                } else if (def->getBB() == curbb) {
                    //If stmt is at same bb with exp, then
                    //we can not determine whether they are independent,
                    //because if they are, the situation should be processed
                    //in buildLocalDUChain().
                    //Build du chain for conservative purpose.
                    //Nonkilling Def.
                    build_du = true;
                } else {
                    UINT result = checkIsNonLocalKillingDef(def, exp);
                    if (result == CK_OVERLAP || result == CK_UNKNOWN) {
                        //Nonkilling Def.
                        build_du = true;
                    }
                }
            } else if (def->isCallStmt()) {
                //If def is CALL|ICALL which has sideeffect,
                //then we should consider MayDef MDSet as well.
                consider_maydef = true;
            }
        } else {
            consider_maydef = true;
        }

        if (consider_maydef) {
            MDSet const* maydef = getMayDef(def);
            if (maydef != NULL &&
                ((maydef == expmds ||
                  (expmds != NULL && maydef->is_intersect(*expmds))) ||
                 (expmd != NULL && maydef->is_overlap(expmd, m_ru)))) {
                //Nonkilling Def.
                build_du = true;
            } else if (mustdef != NULL &&
                       expmds != NULL &&
                       expmds->is_overlap(mustdef, m_ru)) {
                //Killing Def if mustdef is exact, or else is nonkilling def.
                build_du = true;
            }
        }

        if (build_du) {
            expdu->add(d, *m_misc_bs_mgr);
            DUSet * def_useset = getAndAllocDUSet(def);
            if (!m_is_init->is_contain(d)) {
                m_is_init->bunion(d);
                def_useset->clean(*m_misc_bs_mgr);
            }
            def_useset->add(IR_id(exp), *m_misc_bs_mgr);
        }
    }
}


//Check and build DU chain to IR Expression accroding to MustUse MD.
void IR_DU_MGR::checkMustMDAndBuildDUChain(
        IR const* exp,
        MD const* expmd,
        DUSet * expdu)
{
    ASSERT0(exp && expmd && expdu);
    DefSBitSetCore const* defset = m_md2irs->get(MD_id(expmd));
    if (defset == NULL) { return; }
    IRBB * curbb = exp->get_stmt()->getBB();
    checkDefSetToBuildDUChain(exp, expmd, NULL, expdu, defset, curbb);
    return;
}


//Check and build DU chain to IR Expression accroding to MDSet.
void IR_DU_MGR::checkMDSetAndBuildDUChain(
        IR const* exp,
        MD const* expmd,
        MDSet const& expmds,
        DUSet * expdu)
{
    ASSERT0(expdu);
    IRBB * curbb = exp->get_stmt()->getBB();
    SEGIter * iter;
    for (INT u = expmds.get_first(&iter);
         u >= 0; u = expmds.get_next(u, &iter)) {
        DefSBitSetCore const* defset = m_md2irs->get(u);
        if (defset == NULL) { continue; }
        checkDefSetToBuildDUChain(exp, expmd, &expmds, expdu, defset, curbb);
    }
}


void IR_DU_MGR::updateDefWithMustExactMD(IR * ir, MD const* mustexact)
{
    ASSERT0(mustexact && mustexact->is_exact());

    //Handle exactly killing def. This def kills
    //prior DEF stmt to exact md.
    m_md2irs->set(MD_id(mustexact), ir);

    //Pick off stmt from md's definition-list.
    //e.g: Assume the md of struct {int a;} s is MD13, and s.a is MD4,
    //then MD4 and MD13 are overlapped.
    //    MD13 has overlap-def in list:
    //        s.a = 10;
    //When we meet exact definition to MD4, it kill all lived
    //stmts that exact-def MD4, also include the
    //stmt in MD13's def-list.
    MDSet const* maydef = getMayDef(ir);
    if (maydef == NULL) { return; }

    SEGIter * iter;
    for (INT i = maydef->get_first(&iter);
         i >= 0; i = maydef->get_next(i, &iter)) {
        if (MD_id(mustexact) == (UINT)i) {
            //Already add to md2irs.
            continue;
        }

        ASSERT0(m_md_sys->getMD(i));
        if (!m_md_sys->getMD(i)->is_exact()) {
            //ir is non-killing def related to ith MD.
            m_md2irs->append(i, ir);
            continue;
        }

        DefSBitSetCore * dlst = m_md2irs->get(i);
        if (dlst == NULL) { continue; }

        SEGIter * sc = NULL;
        INT nk;
        for (INT k = dlst->get_first(&sc); k >= 0; k = nk) {
            nk = dlst->get_next(k, &sc);
            ASSERT0(m_ru->getIR(k) && m_ru->getIR(k)->is_stmt());
            MD const* w = m_ru->getIR(k)->getExactRef();
            if (mustexact == w || (w != NULL && mustexact->is_cover(w))) {
                //Current ir stmt killed stmt k as well.
                dlst->diff(k, *m_misc_bs_mgr);
            }
        }
    }
}


void IR_DU_MGR::updateDefWithMustEffectMD(IR * ir, MD const* musteffect)
{
    ASSERT0(musteffect == NULL ||
            (musteffect->is_effect() && !musteffect->is_exact()));

    //Handle inexactly nonkilling DEF.
    //And alloc overlapped MDSet for DEF as maydef.
    if (musteffect != NULL) {
        ASSERT0(musteffect->is_effect());
        m_md2irs->append(musteffect, ir);
    } else {
        m_md2irs->setHasIneffectDef();
    }

    MDSet const* maydef = getMayDef(ir);
    if (maydef == NULL) { return; }

    //ir might kill stmts which has overlapped MDSet with md.
    if (musteffect != NULL) {
        UINT mustid = MD_id(musteffect);
        SEGIter * iter;
        for (INT i = maydef->get_first(&iter);
             i >= 0; i = maydef->get_next(i, &iter)) {
            if (mustid == (UINT)i) {
                //Already add to md2irs.
                continue;
            }

            ASSERT0(m_md_sys->getMD(i));
            m_md2irs->append(i, ir);
        }
    } else {
        SEGIter * iter;
        for (INT i = maydef->get_first(&iter);
             i >= 0; i = maydef->get_next(i, &iter)) {
            ASSERT0(m_md_sys->getMD(i));
            m_md2irs->append(i, ir);
        }
    }
}


void IR_DU_MGR::updateDef(IR * ir, UINT flag)
{
    if (!m_is_init->is_contain(ir->id())) {
        m_is_init->bunion(ir->id());
        DUSet * set = ir->getDUSet();
        if (set != NULL) {
            set->clean(*m_misc_bs_mgr);
        }
    }

    switch (ir->get_code()) {
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
        if (!HAVE_FLAG(flag, COMPUTE_NOPR_DU)) { return; }
    case IR_STPR:
    case IR_PHI:
        if (!HAVE_FLAG(flag, COMPUTE_PR_DU)) { return; }
        break;
    default: if (!ir->hasResult()) { return; }
    }

    ASSERT0(ir->is_st() || ir->is_ist() || ir->is_starray() ||
            ir->is_stpr() || ir->is_phi() || ir->isCallStmt());

    if (!HAVE_FLAG(flag, COMPUTE_PR_DU) && ir->isCallStmt()) {
        updateDefWithMustEffectMD(ir, NULL);
        return;
    }

    MD const* must = ir->getRefMD();
    if (must != NULL && must->is_exact()) {
        updateDefWithMustExactMD(ir, must);
    } else {
        updateDefWithMustEffectMD(ir, must);
    }
}


//Initialize md2maydef_irs for each bb.
void IR_DU_MGR::initMD2IRList(IRBB * bb)
{
    DefDBitSetCore * reachdef_in = getInReachDef(BB_id(bb), m_misc_bs_mgr);
    ASSERT0(reachdef_in);
    m_md2irs->clean();

    //Record DEF IR STMT for each MD.
    SEGIter * st = NULL;
    for (INT i = reachdef_in->get_first(&st);
         i != -1; i = reachdef_in->get_next(i, &st)) {
        IR const* stmt = m_ru->getIR(i);
        //stmt may be PHI, Region, CALL.
        //If stmt is IR_PHI, its maydef is NULL.
        //If stmt is IR_REGION, its mustdef is NULL, but the maydef
        //may be not empty.
        MD const* mustdef = get_must_def(stmt);
        if (mustdef != NULL) {
            //mustdef may be fake object.
            //ASSERT0(mustdef->is_effect());
            m_md2irs->append(mustdef, i);
        }

        //if (mustdef == NULL) {
        //    m_md2irs->setHasIneffectDef();
        //}

        MDSet const* maydef = getMayDef(stmt);
        if (maydef != NULL) {
            SEGIter * iter;
            for (INT j = maydef->get_first(&iter);
                 j != -1; j = maydef->get_next(j, &iter)) {
                if (j == MD_GLOBAL_MEM ||
                    j == MD_IMPORT_VAR ||
                    j == MD_FULL_MEM) {
                    m_md2irs->setHasIneffectDef();
                } else if (mustdef != NULL && MD_id(mustdef) == (UINT)j) {
                    continue;
                }

                ASSERT0(m_md_sys->getMD(j) != NULL);
                m_md2irs->append(j, i);
            }
        }
    }
}


//Compute inexactly DU chain.
//'is_init': record initialized DU.
//NOTICE: The Reach-Definition and Must-Def, May-Def,
//May Use must be avaliable.
void IR_DU_MGR::computeMDDUforBB(IN IRBB * bb, UINT flag)
{
    initMD2IRList(bb);
    C<IR*> * ct;
    for (BB_irlist(bb).get_head(&ct);
         ct != BB_irlist(bb).end();
         ct = BB_irlist(bb).get_next(ct)) {
        IR * ir = ct->val();
        ASSERT0(ir);

        if (!ir->isContainMemRef()) { continue; }

        //Process USE
        checkAndBuildChain(ir, ct, flag);

        //Process DEF.
        updateDef(ir, flag);
    } //end for
}


bool IR_DU_MGR::verifyLiveinExp()
{
    BBList * bbl = m_ru->getBBList();

    C<IRBB*> * ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        SEGIter * st = NULL;
        DefDBitSetCore * x = getAvailInExpr(BB_id(bb), m_misc_bs_mgr);
        for (INT i = x->get_first(&st); i >= 0; i = x->get_next(i, &st)) {
            ASSERT0(m_ru->getIR(i) && m_ru->getIR(i)->is_exp());
        }
    }
    return true;
}


bool IR_DU_MGR::checkIsTruelyDep(IR const* def, IR const* use)
{
    MD const* mustdef = def->getRefMD();
    MDSet const* maydef = def->getRefMDSet();
    MD const* mustuse = use->getRefMD();
    MDSet const* mayuse = use->getRefMDSet();
    if (mustdef != NULL) {
        if (mustuse != NULL) {
            if (def->isCallStmt()) {
                //CALL, ICALL have sideeffect, so process
                //them individually.
                if (mustdef == mustuse || mustdef->is_overlap(mustuse)) {
                    ; //Do nothing.
                } else if (maydef != NULL && maydef->is_overlap(mustuse, m_ru)) {
                    ; //Do nothing.
                } else if (maydef != NULL &&
                           mayuse != NULL &&
                           mayuse->is_intersect(*maydef)) {
                    ; //Do nothing.
                } else {
                    UNREACH();
                }
            } else {
                ASSERT0(mustdef == mustuse || mustdef->is_overlap(mustuse));
            }
        } else if (mayuse != NULL) {
            if (def->isCallStmt()) {
                ASSERT0(mayuse->is_overlap_ex(mustdef, m_ru, m_md_sys) ||
                        (maydef != NULL && mayuse->is_intersect(*maydef)));
            } else {
                ASSERT0(mayuse->is_overlap_ex(mustdef, m_ru, m_md_sys));
            }
        } else {
            ASSERT(0, ("Not a truely dependence"));
        }
    } else if (maydef != NULL) {
        if (mustuse != NULL) {
            ASSERT0(maydef->is_overlap_ex(mustuse, m_ru, m_md_sys));
        } else if (mayuse != NULL) {
            ASSERT0(mayuse->is_intersect(*maydef));
        } else {
            ASSERT(0, ("Not a truely dependence"));
        }
    } else {
        ASSERT(0, ("Not a truely dependence"));
    }
    return true;
}


//Verify if DU chain is correct between each Def and Use of MD.
bool IR_DU_MGR::verifyMDDUChainForIR(IR const* ir, UINT duflag)
{
    bool precision_check = g_verify_level >= VERIFY_LEVEL_2;
    ASSERT0(ir->is_stmt());

    if (ir->getSSAInfo() == NULL ||
        ir->isCallStmt()) { //Need to check memory DU for call
        //ir is in MD DU form.
        if ((HAVE_FLAG(duflag, COMPUTE_PR_DU) && ir->isWritePR()) ||
            (HAVE_FLAG(duflag, COMPUTE_NOPR_DU) && ir->isMemoryRef())) {
            DUSet const* useset = ir->readDUSet();
            if (useset != NULL) {
                DUIter di = NULL;
                for (INT i = useset->get_first(&di);
                     i >= 0; i = useset->get_next(i, &di)) {
                    IR const* use = m_ru->getIR(i);
                    DUMMYUSE(use);

                    ASSERT0(use->is_exp());

                    //Check the existence of 'use'.
                    ASSERT0(use->get_stmt() && use->get_stmt()->getBB());
                    ASSERT0(BB_irlist(
                        use->get_stmt()->getBB()).find(use->get_stmt()));

                    //use must be a memory operation.
                    ASSERT0(use->isMemoryOpnd());

                    //ir must be DEF of 'use'.
                    ASSERT0(use->readDUSet());

                    //Check consistence between ir and use duchain.
                    ASSERT0(use->readDUSet()->is_contain(ir->id()));

                    if (precision_check) {
                        ASSERT0(checkIsTruelyDep(ir, use));
                    }
                }
            }
        }
    }

    m_citer.clean();
    for (IR const* u = iterRhsInitC(ir, m_citer);
         u != NULL; u = iterRhsNextC(m_citer)) {
        ASSERT0(!ir->is_lhs(u) && u->is_exp());

        if (u->getSSAInfo() != NULL) {
            //u is in SSA form, so check it MD du is meaningless.
            continue;
        }

        if ((!HAVE_FLAG(duflag, COMPUTE_PR_DU) && u->isReadPR()) ||
            (!HAVE_FLAG(duflag, COMPUTE_NOPR_DU) && u->isMemoryRef())) {
            continue;
        }

        DUSet const* defset = u->readDUSet();
        if (defset == NULL) { continue; }

        ASSERT(u->isMemoryOpnd(), ("only memory operand has DUSet"));

        DUIter di = NULL;
        for (INT i = defset->get_first(&di);
             i >= 0; i = defset->get_next(i, &di)) {
            IR const* def = m_ru->getIR(i);
            CHECK_DUMMYUSE(def);
            ASSERT0(def->is_stmt());

            //Check the existence to 'def'.
            ASSERT0(def->getBB());
            ASSERT0(BB_irlist(def->getBB()).find(const_cast<IR*>(def)));

            //u must be use of 'def'.
            ASSERT0(def->readDUSet());

            //Check consistence between DEF and USE.
            ASSERT0(def->readDUSet()->is_contain(IR_id(u)));

            if (precision_check) {
                ASSERT0(checkIsTruelyDep(def, u));
            }
        }
    }
    return true;
}


//Verify DU chain's sanity.
bool IR_DU_MGR::verifyMDDUChain(UINT duflag)
{
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head();
         bb != NULL; bb = bbl->get_next()) {
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            verifyMDDUChainForIR(ir, duflag);
         }
    }
    return true;
}


void IR_DU_MGR::resetReachDefInSet(bool cleanMember)
{
    for (INT i = 0; i <= m_bb_in_reach_def.get_last_idx(); i++) {
        DefDBitSetCore * bs = m_bb_in_reach_def.get(i);
        if (bs == NULL) { continue; }
        m_misc_bs_mgr->destroySEGandFreeDBitSetCore(bs);
    }
    if (cleanMember) {
        m_bb_in_reach_def.clean();
    }
}


void IR_DU_MGR::resetAvailReachDefInSet(bool cleanMember)
{
    for (INT i = 0; i <= m_bb_avail_in_reach_def.get_last_idx(); i++) {
        DefDBitSetCore * bs = m_bb_avail_in_reach_def.get(i);
        if (bs == NULL) { continue; }
        //m_misc_bs_mgr->freedc(bs);
        m_misc_bs_mgr->destroySEGandFreeDBitSetCore(bs);
    }
    if (cleanMember) {
        m_bb_avail_in_reach_def.clean();
    }
}


void IR_DU_MGR::resetAvailExpInSet(bool cleanMember)
{
    for (INT i = m_bb_availin_exp.get_first();
         i >= 0; i = m_bb_availin_exp.get_next(i)) {
        DefDBitSetCore * bs = m_bb_availin_exp.get(i);
        ASSERT0(bs);
        //m_misc_bs_mgr->freedc(bs);
        m_misc_bs_mgr->destroySEGandFreeDBitSetCore(bs);
    }
    if (cleanMember) {
        m_bb_availin_exp.clean();
    }
}


void IR_DU_MGR::resetGlobalSet(bool cleanMember)
{
    resetAvailReachDefInSet(cleanMember);
    resetReachDefInSet(cleanMember);
    resetAvailExpInSet(cleanMember);
}


//Free auxiliary data structure used in solving.
void IR_DU_MGR::resetLocalAuxSet(DefMiscBitSetMgr & bsmgr)
{
    Vector<DefDBitSetCore*> * ptr;
    ptr = &m_bb_avail_out_reach_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) { dset->clean(bsmgr); }
    }
    m_bb_avail_out_reach_def.clean();

    ptr = &m_bb_out_reach_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) { dset->clean(bsmgr); }
    }
    m_bb_out_reach_def.clean();

    ptr = &m_bb_may_gen_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) { dset->clean(bsmgr); }
    }
    m_bb_may_gen_def.clean();

    ptr = &m_bb_must_gen_def;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) { dset->clean(bsmgr); }
    }
    m_bb_must_gen_def.clean();

    ptr = &m_bb_gen_exp;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) { dset->clean(bsmgr); }
    }
    m_bb_gen_exp.clean();

    ptr = &m_bb_availout_ir_expr;
    for (INT i = 0; i <= ptr->get_last_idx(); i++) {
        DefDBitSetCore * dset = ptr->get(i);
        if (dset != NULL) { dset->clean(bsmgr); }
    }
    m_bb_availout_ir_expr.clean();

    //Const set.
    m_bb_killed_exp.clean();
    m_bb_must_killed_def.clean();
    m_bb_may_killed_def.clean();
}


//Find the nearest dominated DEF stmt of 'exp'.
//NOTE: RPO of bb of stmt must be available.
//
//'exp': expression
//'exp_stmt': stmt that exp is belong to.
//'expdu': def set of exp.
//'omit_self': true if we do not consider the 'exp_stmt' itself.
IR * IR_DU_MGR::findDomDef(
        IR const* exp,
        IR const* exp_stmt,
        DUSet const* expdefset,
        bool omit_self)
{
    ASSERT0(const_cast<IR_DU_MGR*>(this)->getMayUse(exp) != NULL ||
            const_cast<IR_DU_MGR*>(this)->getMustUse(exp) != NULL);
    IR * last = NULL;
    INT lastrpo = -1;
    DUIter di = NULL;
    for (INT i = expdefset->get_first(&di);
         i >= 0; i = expdefset->get_next(i, &di)) {
        IR * d = m_ru->getIR(i);
        ASSERT0(d->is_stmt());
        if (!is_may_def(d, exp, false)) {
            continue;
        }

        if (omit_self && d == exp_stmt) {
            continue;
        }

        if (last == NULL) {
            last = d;
            lastrpo = BB_rpo(d->getBB());
            ASSERT0(lastrpo >= 0);
            continue;
        }

        IRBB * dbb = d->getBB();
        ASSERT0(dbb);
        ASSERT0(BB_rpo(dbb) >= 0);
        if (BB_rpo(dbb) > lastrpo) {
            last = d;
            lastrpo = BB_rpo(dbb);
        } else if (dbb == last->getBB() && dbb->is_dom(last, d, true)) {
            last = d;
            lastrpo = BB_rpo(dbb);
        }
    }

    if (last == NULL) { return NULL; }
    IRBB * last_bb = last->getBB();
    IRBB * exp_bb = exp_stmt->getBB();
    if (!m_cfg->is_dom(BB_id(last_bb), BB_id(exp_bb))) {
        return NULL;
    }

    //e.g: *p = *p + 1
    //Def and Use in same stmt, in this situation,
    //the stmt can not be regarded as dom-def.
    if (exp_bb == last_bb && !exp_bb->is_dom(last, exp_stmt, true)) {
        return NULL;
    }
    ASSERT0(last != exp_stmt);
    return last;
}


//Compute maydef, mustdef, mayuse information for current region.
void IR_DU_MGR::computeRegionMDDU(
        Vector<MDSet*> const* mustexactdef_mds,
        Vector<MDSet*> const* maydef_mds,
        MDSet const* mayuse_mds)
{
    ASSERT0(mustexactdef_mds && maydef_mds && mayuse_mds);
    m_ru->initRefInfo();

    MDSet * ru_maydef = m_ru->getMayDef();
    ASSERT0(ru_maydef);
    ru_maydef->clean(*m_misc_bs_mgr);

    MDSet * ru_mayuse = m_ru->getMayUse();
    ASSERT0(ru_mayuse);
    ru_mayuse->clean(*m_misc_bs_mgr);

    BBList * bbl = m_ru->getBBList();
    C<IRBB*> * ct;
    VarTab const* vtab = m_ru->getVarTab();
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        MDSet const* mds = mustexactdef_mds->get(BB_id(bb));
        ASSERT0(mds != NULL);
        SEGIter * iter;
        for (INT i = mds->get_first(&iter);
             i >= 0; i = mds->get_next(i, &iter)) {
            MD const* md = m_md_sys->getMD(i);
            ASSERT0(md->get_base());
            if (!md->is_pr() && !vtab->find(md->get_base())) {
                //Only record the VAR defined in outter region.
                ru_maydef->bunion(md, *m_misc_bs_mgr);
            }
        }

        mds = maydef_mds->get(BB_id(bb));
        ASSERT0(mds != NULL);
        for (INT i = mds->get_first(&iter);
             i >= 0; i = mds->get_next(i, &iter)) {
            MD const* md = m_md_sys->getMD(i);
            ASSERT0(md->get_base());
            if (!md->is_pr() && !vtab->find(md->get_base())) {
                //Only record the VAR defined in outter region.
                ru_maydef->bunion(md, *m_misc_bs_mgr);
            }
        }
    }

    SEGIter * iter;
    for (INT i = mayuse_mds->get_first(&iter);
         i >= 0; i = mayuse_mds->get_next(i, &iter)) {
        MD const* md = m_md_sys->getMD(i);
        ASSERT0(md->get_base());
        if (!md->is_pr() && !vtab->find(md->get_base())) {
            //Only record the VAR defined in outter region.
            ru_mayuse->bunion(md, *m_misc_bs_mgr);
        }
    }

    REGION_is_mddu_valid(m_ru) = true;
}


size_t IR_DU_MGR::count_mem_local_data(
        DefDBitSetCore * expr_univers,
        Vector<MDSet*> * maydef_mds,
        Vector<MDSet*> * mustexactdef_mds,
        MDSet * mayuse_mds,
        MDSet mds_arr_for_must[],
        MDSet mds_arr_for_may[],
        UINT elemnum)
{
    size_t count = 0;
    if (expr_univers != NULL) {
        count += expr_univers->count_mem();
    }

    if (mustexactdef_mds != NULL) {
        ASSERT0(mds_arr_for_must);
        count += mustexactdef_mds->count_mem();
        for (UINT i = 0; i < elemnum; i++) {
            count += mds_arr_for_must[i].count_mem();
        }
    }

    if (maydef_mds != NULL) {
        ASSERT0(mds_arr_for_may);
        count += maydef_mds->count_mem();
        for (UINT i = 0; i < elemnum; i++) {
            count += mds_arr_for_may[i].count_mem();
        }
    }

    if (mayuse_mds != NULL) {
        count += mayuse_mds->count_mem();
    }

    return count;
}


//Compute MD reference, May-Must-Def-Use and related IR-Set info.
bool IR_DU_MGR::perform(IN OUT OptCtx & oc, UINT flag)
{
    if (flag == 0) { return true; }

    BBList * bbl = m_ru->getBBList();
    if (bbl->get_elem_count() == 0) { return true; }
    if (bbl->get_elem_count() > g_thres_opt_bb_num) {
        //Adjust g_thres_opt_bb_num to make sure you want to do DU analysis.
        interwarn("IR_DU_MGR::perform() of Region(%d) is not applied.",
                  REGION_id(m_ru));
        return false;
    }

    ASSERT0(OC_is_cfg_valid(oc)); //First, only cfg is needed.

    START_TIMER(t1, "Build DU ref");
    if (HAVE_FLAG(flag, SOL_REF)) {
        ASSERT0(OC_is_aa_valid(oc));
        computeMDRef(oc, flag);
    }
    END_TIMER(t1, "Build DU ref");

    if (HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF) ||
        HAVE_FLAG(flag, SOL_REACH_DEF) ||
        HAVE_FLAG(flag, SOL_RU_REF) ||
        HAVE_FLAG(flag, SOL_AVAIL_EXPR)) {
        ASSERT0(OC_is_ref_valid(oc));

        Vector<MDSet*> * maydef_mds = NULL;
        Vector<MDSet*> * mustexactdef_mds = NULL;
        MDSet * mayuse_mds = NULL;

        if (HAVE_FLAG(flag, SOL_RU_REF)) {
            mayuse_mds = new MDSet();
        }

        MDSet * mds_arr_for_must = NULL;
        MDSet * mds_arr_for_may = NULL;

        //Some system need these set.

        if (HAVE_FLAG(flag, SOL_REACH_DEF) ||
            HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF) ||
            HAVE_FLAG(flag, SOL_RU_REF)) {
            mustexactdef_mds = new Vector<MDSet*>();
        }

        if (HAVE_FLAG(flag, SOL_REACH_DEF) ||
            HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF) ||
            HAVE_FLAG(flag, SOL_AVAIL_EXPR) ||
            HAVE_FLAG(flag, SOL_RU_REF)) {
            maydef_mds = new Vector<MDSet*>();
        }

        START_TIMER(t2, "Allocate May/Must MDS table");

        if (mustexactdef_mds != NULL) {
            mds_arr_for_must = new MDSet[bbl->get_elem_count()]();
        }

        if (maydef_mds != NULL) {
            mds_arr_for_may = new MDSet[bbl->get_elem_count()]();
        }

        UINT i = 0;
        for (IRBB * bb = bbl->get_tail();
             bb != NULL; bb = bbl->get_prev(), i++) {
            if (mustexactdef_mds != NULL) {
                mustexactdef_mds->set(BB_id(bb), &mds_arr_for_must[i]);
            }
            if (maydef_mds != NULL) {
                maydef_mds->set(BB_id(bb), &mds_arr_for_may[i]);
            }
        }
        END_TIMER(t2, "Allocate May/Must MDS table");

        DefMiscBitSetMgr bsmgr;

        START_TIMER(t3, "Build MustDef, MayDef, MayUse");
        computeMustExactDefMayDefMayUse(mustexactdef_mds,
            maydef_mds, mayuse_mds, flag, bsmgr);
        END_TIMER(t3, "Build MustDef, MayDef, MayUse");

        DefDBitSetCoreHashAllocator dbitsetchashallocator(&bsmgr);
        DefDBitSetCoreReserveTab dbitsetchash(&dbitsetchashallocator);
        if (HAVE_FLAG(flag, SOL_REACH_DEF) ||
            HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF)) {
            START_TIMER(t4, "Build KillSet");
            computeKillSet(dbitsetchash, mustexactdef_mds, maydef_mds, bsmgr);
            END_TIMER(t4, "Build KillSet");
        }

        DefDBitSetCore expr_univers(SOL_SET_IS_SPARSE);
        if (HAVE_FLAG(flag, SOL_AVAIL_EXPR)) {
            //Compute GEN, KILL IR-EXPR.
            START_TIMER(t5, "Build AvailableExp");
            computeAuxSetForExpression(dbitsetchash,
                &expr_univers, maydef_mds, bsmgr);
            END_TIMER(t5, "Build AvailableExp");
        }

        if (HAVE_FLAG(flag, SOL_RU_REF)) {
            //Compute DEF,USE mds for Region.
            START_TIMER(t6, "Build Region DefUse MDSet");
            computeRegionMDDU(mustexactdef_mds, maydef_mds, mayuse_mds);
            END_TIMER(t6, "Build Region DefUse MDSet");
        }

        if (HAVE_FLAG(flag, SOL_REACH_DEF) ||
            HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF) ||
            HAVE_FLAG(flag, SOL_AVAIL_EXPR)) {
            m_ru->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
            START_TIMER(t7, "Solve DU set");
            solve(expr_univers, flag, bsmgr);
            END_TIMER(t7, "Solve DU set");
        }

        expr_univers.clean(bsmgr);

        if (HAVE_FLAG(flag, SOL_AVAIL_REACH_DEF)) {
            OC_is_avail_reach_def_valid(oc) = true;
        }

        if (HAVE_FLAG(flag, SOL_REACH_DEF)) {
            OC_is_reach_def_valid(oc) = true;
        }

        if (HAVE_FLAG(flag, SOL_AVAIL_EXPR)) {
            OC_is_live_expr_valid(oc) = true;
        }

        if (mustexactdef_mds != NULL) {
            ASSERT0(mds_arr_for_must);
            for (UINT i2 = 0; i2 < bbl->get_elem_count(); i2++) {
                mds_arr_for_must[i2].clean(bsmgr);
            }
            delete [] mds_arr_for_must;
            delete mustexactdef_mds;
            mustexactdef_mds = NULL;
        }

        if (maydef_mds != NULL) {
            ASSERT0(mds_arr_for_may);
            for (UINT i2 = 0; i2 < bbl->get_elem_count(); i2++) {
                mds_arr_for_may[i2].clean(bsmgr);
            }
            delete [] mds_arr_for_may;
            delete maydef_mds;
            maydef_mds = NULL;
        }

        if (mayuse_mds != NULL) {
            mayuse_mds->clean(bsmgr);
            delete mayuse_mds;
        }

        resetLocalAuxSet(bsmgr);
    }

    if (g_is_dump_mdset_hash) {
        m_mds_hash->dump();
    }

    return true;
}


//Construct inexactly Du, Ud chain.
//NOTICE: Reach-Definition and Must-Def, May-Def,
//May-Use must be avaliable.
//retain_reach_def: true to reserve reach-def stmt set.
void IR_DU_MGR::computeMDDUChain(
        IN OUT OptCtx & oc,
        bool retain_reach_def,
        UINT duflag)
{
    if (m_ru->getBBList()->get_elem_count() == 0) { return; }

    START_TIMER(t, "Build DU-CHAIN");

    ASSERT0(OC_is_ref_valid(oc) && OC_is_reach_def_valid(oc));

    m_oc = &oc;
    BBList * bbl = m_ru->getBBList();
    if (bbl->get_elem_count() > g_thres_opt_bb_num) {
        //There are too many BB. Leave it here.
        END_TIMER(t, "Build DU-CHAIN");
        resetReachDefInSet(true);
        OC_is_reach_def_valid(oc) = false;
        return;
    }

    ASSERT0(OC_is_cfg_valid(oc));

    //Record IRs which may defined these referred MDs.
    ASSERT0(m_md2irs == NULL && m_is_init == NULL);
    m_md2irs = new MDId2IRlist(m_ru);
    m_is_init = new BitSet(MAX(1, (m_ru->getIRVec()->
        get_last_idx()/BITS_PER_BYTE)));

    //Compute the DU chain linearly.
    C<IRBB*> * ct;
    MDIter mditer;

    for (IRBB * bb = bbl->get_tail(&ct);
         bb != NULL; bb = bbl->get_prev(&ct)) {
        computeMDDUforBB(bb, duflag);
    }

    delete m_md2irs;
    m_md2irs = NULL;

    delete m_is_init;
    m_is_init = NULL;

    if (!retain_reach_def) {
        //Reach def info will be cleaned.
        resetReachDefInSet(true);
        OC_is_reach_def_valid(oc) = false;
    }

    OC_is_du_chain_valid(oc) = true;

    if (g_is_dump_du_chain) {
        m_md_sys->dumpAllMD();
        dumpDUChainDetail();
    }

    ASSERT(verifyMDDUChain(duflag), ("verifyMDDUChain failed"));
    END_TIMER(t, "Build DU-CHAIN");
}
//END IR_DU_MGR

} //namespace xoc
