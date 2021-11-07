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

#define CK_UNKNOWN 0 //Can not determine if memory is overlap.
#define CK_OVERLAP 1 //Can be confirmed memory is overlap.
#define CK_NOT_OVERLAP 2 //Can be confirmed memory is not overlap.

//
//START MD2IRSet
//
MD2IRSet::MD2IRSet(Region * rg)
{
    m_rg = rg;
    m_md_sys = rg->getMDSystem();
    m_tm = rg->getTypeMgr();
    m_du = rg->getDUMgr();
    m_sbs_mgr = m_du->getSBSMgr();
    m_are_stmts_defed_ineffect_md = false;
}


MD2IRSet::~MD2IRSet()
{
    TMapIter<UINT, DefSBitSetCore*> c;
    DefSBitSetCore * mapped = nullptr;
    for (UINT mdid = get_first(c, &mapped);
         mdid != MD_UNDEF; mdid = get_next(c, &mapped)) {
        ASSERT0(mapped);
        mapped->clean(*m_sbs_mgr);
        delete mapped;
    }

    m_global_md.clean(*m_sbs_mgr);
}


void MD2IRSet::clean()
{
    TMapIter<UINT, DefSBitSetCore*> c;
    DefSBitSetCore * mapped = nullptr;
    for (UINT mdid = get_first(c, &mapped);
         mdid != MD_UNDEF; mdid = get_next(c, &mapped)) {
        ASSERT0(mapped);
        mapped->clean(*m_sbs_mgr);
    }

    m_global_md.clean(*m_sbs_mgr);
    m_are_stmts_defed_ineffect_md = false;

    //Do not clean DefSBitSet* here, it will incur memory leak.
    //TMap<UINT, DefSBitSetCore*>::clean();
}


//'md' corresponds to unique 'ir'.
void MD2IRSet::set(UINT mdid, IR * ir)
{
    ASSERTN(!xoc::isGlobalSideEffectMD(mdid),
            ("there is not any md could kill Fake-May-MD."));
    ASSERT0(ir);
    DefSBitSetCore * irtab = TMap<UINT, DefSBitSetCore*>::get(mdid);
    if (irtab == nullptr) {
        irtab = new DefSBitSetCore();
        TMap<UINT, DefSBitSetCore*>::set(mdid, irtab);
    } else {
        irtab->clean(*m_sbs_mgr);
    }
    irtab->bunion(ir->id(), *m_sbs_mgr);
}


//'md' corresponds to multiple 'ir'.
void MD2IRSet::append(UINT mdid, UINT irid)
{
    DefSBitSetCore * irtab = TMap<UINT, DefSBitSetCore*>::get(mdid);
    if (irtab == nullptr) {
        irtab = new DefSBitSetCore();
        TMap<UINT, DefSBitSetCore*>::set(mdid, irtab);
    }
    irtab->bunion(irid, *m_sbs_mgr);
}


void MD2IRSet::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    m_md_sys->dump(false);
    note(getRegion(), "\n==-- DUMP MDID2IRLIST --==");
    TMapIter<UINT, DefSBitSetCore*> c;
    for (UINT mdid = get_first(c); mdid != MD_UNDEF; mdid = get_next(c)) {
        MD const * md = m_md_sys->getMD(mdid);
        md->dump(m_md_sys->getTypeMgr());

        DefSBitSetCore * irs = get(mdid);
        if (irs == nullptr || irs->get_elem_count() == 0) { continue; }
        DefSBitSetIter sc = nullptr;
        getRegion()->getLogMgr()->incIndent(2);
        for (INT i = irs->get_first(&sc); i >= 0; i = irs->get_next(i, &sc)) {
            IR * d = m_rg->getIR(i);
            note(getRegion(), "\n------------------");
            dumpIR(d, m_rg, nullptr, 0);
            note(getRegion(), "\n  ");
            if (d->getMustRef() != nullptr) {
                //Dump MustDef ref.
                prt(getRegion(), "EMD%d", d->getMustRef()->id());
            }

            MDSet const* ms = d->getMayRef();
            if (ms != nullptr) {
                //Dump MayDef ref.
                MDSetIter iter;
                for (INT j = ms->get_first(&iter);
                     j >= 0; j = ms->get_next(j, &iter)) {
                    prt(getRegion(), " MD%d", j);
                }
            }
        }
        getRegion()->getLogMgr()->decIndent(2);
    }
}
//END MD2IRSet


//
//START DUMgr
//
DUMgr::DUMgr(Region * rg) : m_solve_set(rg)
{
    m_rg = rg;
    m_tm = rg->getTypeMgr();
    m_md_sys = rg->getMDSystem();
    m_aa = rg->getAA();
    m_cfg = rg->getCFG();
    m_mds_mgr = rg->getMDSetMgr();
    m_mds_hash = rg->getMDSetHash();
    m_pool = smpoolCreate(sizeof(DUSet) * 2, MEM_COMM);
    m_is_init = nullptr;
    m_md2irs = nullptr;
    m_is_cached_mdset = nullptr;
    m_cached_overlap_mdset = nullptr;

    //NOTE: call clean() for each object which
    //inheirted from SBitSet or SBitSetCore while destructing the object.
    //That will free SEG back to MiscBitSetMgr.
    //m_sbs_mgr = rg->getMiscBitSetMgr();

    ASSERT0(m_aa && m_cfg && m_md_sys && m_tm && m_mds_mgr && m_mds_hash);
}


DUMgr::~DUMgr()
{
    //Note you must ensure all ir DUSet and MDSet are freed back to
    //m_sbs_mgr before Region destructed, because destruct DUMgr will NOT free
    //DUSet automatically which has been allocated for IR.

    ASSERT0(m_is_init == nullptr);
    ASSERT0(m_md2irs == nullptr);
    m_solve_set.resetGlobalSet();
    freeDUSetForAllIR();
    smpoolDelete(m_pool);

    //Explicitly free SEG to DefSegMgr,
    //or it will complained during destruction.
    if (m_is_cached_mdset != nullptr) {
        m_is_cached_mdset->clean(*getSBSMgr());
        delete m_is_cached_mdset;
        m_is_cached_mdset = nullptr;
    }
    if (m_cached_overlap_mdset != nullptr) {
        delete m_cached_overlap_mdset;
        m_cached_overlap_mdset = nullptr;
    }
}


void DUMgr::freeDUSetForAllIR()
{
    //Free AIContainer's internal structure.
    //The vector of AIContainer must be destroied explicitly.
    Vector<IR*> * ir_vec = m_rg->getIRVec();
    ASSERT0(ir_vec);
    INT l = ir_vec->get_last_idx();
    for (INT i = 1; i <= l; i++) {
        IR * ir = ir_vec->get(i);
        ASSERT0(ir);
        ir->freeDUset(this);
    }
}


//Compute the overlapping MDSet that might overlap ones which 'ir' referred.
//Then set the MDSet to be ir's may-referred MDSet.
//e.g: int A[100], there are two referrence of array A: A[i], A[j]
//    A[i] might overlap A[j].
//recompute: true to compute overlapping MDSet even if it has cached.
//Note if ir has MustRef, the function will only compute the overlapped MD
//which are overlapped with the MustRef, otherwise it will compute the
//overlapped MD set which is overlapped to every elements in current MayRef
//of ir.
void DUMgr::computeOverlapMDSet(IR * ir, bool recompute)
{
    MD const* must = ir->getMustRef();
    MDSet tmpmds;
    if (must != nullptr) {
        //Note we do NOT compute overlap set of MayRef MDSet if MustRef exist.
        //Because the overlapped MayRef will blow up when we invoke
        //computeOverlapMDSet() multiple times at once.
        //e.g: struct S { int a; int b } s;
        //     Given MayRef MDSet {s.a}, the overlapped MDSet is {s.a, s};
        //     If we compute overlapped set for MDSet {s.a, s}, the result
        //     will be {s.a, s, s.b}, that is over-conservative to the real
        //     result.
        ASSERTN(!ir->isCallStmt(), ("Call should be processed specially"));
        if (xoc::isGlobalSideEffectMD(must->id())) {
            return;
        }

        //Compute overlapped MDSet for must-ref.
        if (recompute ||
            (m_is_cached_mdset != nullptr &&
             !m_is_cached_mdset->is_contain(must->id()))) {
            m_md_sys->computeOverlap(m_rg, must, tmpmds, m_tab_iter,
                                     *getSBSMgr(), true);

            MDSet const* newmds = m_mds_hash->append(tmpmds);
            if (newmds != nullptr && m_cached_overlap_mdset != nullptr) {
                m_cached_overlap_mdset->set(must, newmds);
                ASSERT0(m_is_cached_mdset);
                m_is_cached_mdset->bunion(must->id(), *getSBSMgr());
            }
            if (newmds == nullptr) {
                //If newmds is nullptr, clean may-ref of ir.
                ir->cleanMayRef();
            } else {
                ir->setMayRef(newmds, m_rg);
            }
            tmpmds.clean(*getSBSMgr());
            return;
        }

        ASSERT0(tmpmds.is_empty());
        MDSet const* set = nullptr;
        if (m_cached_overlap_mdset != nullptr) {
            set = m_cached_overlap_mdset->get(must);
        }
        if (set == nullptr) {
            ir->cleanMayRef();
        } else {
            ir->setMayRef(set, m_rg);
        }
        return;
    }

    //Compute overlapped MDSet for may-ref, may-ref may contain several MDs.
    MDSet const* may = ir->getMayRef();
    if (may == nullptr) {
        ASSERT0(tmpmds.is_empty());
        return;
    }

    ASSERTN(!may->is_empty(), ("can not get a hashed MDSet that is empty"));
    tmpmds.copy(*may, *getSBSMgr());
    m_md_sys->computeOverlap(m_rg, *may, tmpmds, m_tab_iter,
                             *getSBSMgr(), true);
    ir->setMayRef(m_mds_hash->append(tmpmds), m_rg);
    tmpmds.clean(*getSBSMgr());
}


//Try allocate DUSet for memory reference.
DUSet * DUMgr::genDUSet(IR * ir)
{
    ASSERT0(ir->isContainMemRef());
    DU * du = ir->getDU();
    if (du == nullptr) {
        du = m_rg->allocDU();
        ir->setDU(du);
    }

    DUSet * dus = DU_duset(du);
    if (dus == nullptr) {
        //Alloc DUSet from Region's MiscBitSetMgr.
        dus = (DUSet*)getSBSMgr()->allocSBitSetCore();
        DU_duset(du) = dus;
    }
    return dus;
}


//Return true if 'def_stmt' is the exact and unique reach-definition
//to the operands of 'use_stmt', otherwise return false.
//
//'def_stmt': should be stmt.
//'use_stmt': should be stmt.
bool DUMgr::isExactAndUniqueDef(IR const* def, IR const* exp)
{
    ASSERT0(def->is_stmt() && exp->is_exp());
    MD const* def_md = def->getExactRef();
    if (def_md == nullptr) { return false; }
    ASSERT0(def->readDUSet()); //At least contains 'exp'.

    MD const* use_md = exp->getExactRef();
    DUSet const* defset = exp->readDUSet();
    if (defset == nullptr) { return false; }
    if (use_md == def_md && hasSingleDefToMD(*defset, def_md)) {
        return true;
    }
    return false;
}


//Find and return the exactly and unique stmt that defined 'exp',
//otherwise return nullptr.
//'exp': should be exp, and it's use-md must be exact.
IR const* DUMgr::getExactAndUniqueDef(IR const* exp) const
{
    MD const* use_md = exp->getExactRef();
    if (use_md == nullptr) { return nullptr; }

    DUSet const* defset = exp->readDUSet();
    if (defset == nullptr) { return nullptr; }

    DUSetIter di = nullptr;
    INT d1 = defset->get_first(&di);
    INT d2 = defset->get_next(d1, &di);
    if (d1 < 0 || (d1 >=0 && d2 >= 0)) { return nullptr; }

    IR const* d1ir = m_rg->getIR(d1);
    if (d1ir->isExactDef(use_md)) {
        return d1ir;
    }
    return nullptr;
}


//Return true if there is only one stmt in 'defset' modify 'md'.
//Modification include both exact and inexact.
bool DUMgr::hasSingleDefToMD(DUSet const& defset, MD const* md) const
{
    UINT count = 0;
    DUMgr * pthis = const_cast<DUMgr*>(this);
    DUSetIter di = nullptr;
    for (INT i = defset.get_first(&di); i >= 0; i = defset.get_next(i, &di)) {
        IR * def = m_rg->getIR(i);
        if (def->getMustRef() == md) {
            count++;
        } else {
            MDSet const* def_mds = def->getMayRef();
            if (def_mds == nullptr) { continue; }
            if (def_mds->is_contain(md)) {
                count++;
            }
        }
    }
    return count == 1;
}


bool DUMgr::isStprMayDef(IR const* def, IR const* use, bool is_recur)
{
    ASSERT0(def->is_stpr());
    UINT prno = STPR_no(def);
    if (is_recur) {
        m_citer.clean();
        for (IR const* x = iterInitC(use, m_citer);
             x != nullptr; x = iterNextC(m_citer)) {
            if (!x->is_pr()) { continue; }
            if (PR_no(x) == prno) { return true; }
        }
    } else if (use->is_pr() && PR_no(use) == prno) {
        return true;
    }
    return false;
}


static bool is_call_may_def_helper(IR const* call, IR const* use,
                                   MDSet const* call_maydef)
{
    //MD of use may be exact or inexact.
    MD const* use_md = use->getMustRef();
    MDSet const* use_mds = use->getMayRef();
    if (call->isExactDef(use_md, use_mds)) {
        return true;
    }

    if (call_maydef != nullptr) {
        if (use_md != nullptr && call_maydef->is_contain(use_md)) {
            return true;
        }

        if (use_mds != nullptr &&
            (use_mds == call_maydef || call_maydef->is_intersect(*use_mds))) {
            return true;
        }
    }
    return false;
}


//Return true if 'call' may or must modify MDSet that 'use' referenced.
//'call': CALL/ICALL stmt.
//'use': must be expression.
//'is_recur': true if one intend to compute the mayuse MDSet to walk
//            through IR tree recusively.
bool DUMgr::isCallMayDef(IR const* call, IR const* use, bool is_recur)
{
    ASSERT0(call->isCallStmt());

    //MayDef of stmt must involved the overlapped MD with Must Reference,
    //but except Must Reference itself.
    MDSet const* call_maydef = const_cast<IR*>(call)->getMayRef();
    if (is_recur) {
        m_citer.clean();
        for (IR const* x = iterInitC(use, m_citer);
             x != nullptr; x = iterNextC(m_citer)) {
            if (!x->isMemoryOpnd()) { continue; }

            if (is_call_may_def_helper(call, x, call_maydef)) {
                return true;
            }
        }
        return false;
    }

    ASSERT0(use->isMemoryOpnd());
    return is_call_may_def_helper(call, use, call_maydef);
}


//Return true if 'def' may or must modify MDSet that 'use' referenced.
//'def': must be stmt.
//'use': must be expression.
//'is_recur': true if one intend to compute the mayuse MDSet to walk
//            through IR tree recusively.
bool DUMgr::isMayDef(IR const* def, IR const* use, bool is_recur)
{
    ASSERT0(def->is_stmt() && use->is_exp());
    if (def->is_stpr()) {
        return isStprMayDef(def, use, is_recur);
    }
    if (def->isCallStmt()) {
        return isCallMayDef(def, use, is_recur);
    }

    MD const* mustdef = const_cast<IR*>(def)->getMustRef();
    MDSet const* maydef = const_cast<IR*>(def)->getMayRef();
    if (is_recur) {
        m_citer.clean();
        for (IR const* x = iterInitC(use, m_citer);
             x != nullptr; x = iterNextC(m_citer)) {
            if (!x->isMemoryOpnd()) { continue; }

            MD const* mustuse = x->getMustRef();
            if (mustuse != nullptr) {
                if ((mustdef != nullptr && mustdef == mustuse) ||
                    (maydef != nullptr && maydef->is_contain(mustuse))) {
                    return true;
                }
            }

            MDSet const* mayuse = const_cast<IR*>(x)->getMayRef();
            if (mayuse != nullptr) {
                if ((mustdef != nullptr && mayuse->is_contain(mustdef)) ||
                    (maydef != nullptr &&
                     (maydef == mayuse || mayuse->is_intersect(*maydef)))) {
                    return true;
                }
            }
        }
        return false;
    }

    MD const* mustuse = use->getMustRef();
    if (mustuse != nullptr) {
        if ((mustdef != nullptr && mustdef == mustuse) ||
            (maydef != nullptr && maydef->is_contain(mustuse))) {
            return true;
        }
    }

    MDSet const* mayuse = const_cast<IR*>(use)->getMayRef();
    if (mayuse != nullptr) {
        if ((mustdef != nullptr && mayuse->is_contain(mustdef)) ||
            (maydef != nullptr &&
             (maydef == mayuse || mayuse->is_intersect(*maydef)))) {
            return true;
        }
    }
    return false;
}


void DUMgr::computeArrayRef(IR * ir, OUT MDSet * ret_mds, UINT compflag,
                            UINT duflag)
{
    ASSERT0(ir->isArrayOp());
    if (HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU)) {
        ASSERT0((ir->getRefMDSet() && !ir->getRefMDSet()->is_empty()) ||
                ir->getRefMD());
    }

    if (HAVE_FLAG(compflag, COMP_EXP_RECOMPUTE)) {
        //if (HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU)) {
        //    computeOverlapMDSet(ir, false);
        //}
        //Always compute overlapping MDSet for both PR and NON-PR MD
        //because DURef also have to compute NON-PR MD even if
        //only ReachDef of PR is required by user.
        computeOverlapMDSet(ir, false);

        //Compute referred MDs to subscript expression.
        for (IR * s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
            computeExpression(s, ret_mds, compflag, duflag);
        }
        computeExpression(ARR_base(ir), ret_mds, compflag, duflag);
        return;
    }

    if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
        for (IR * s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
            computeExpression(s, ret_mds, compflag, duflag);
        }
        computeExpression(ARR_base(ir), ret_mds, compflag, duflag);

        if (HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU) && ir->is_array()) {
            //USE-MDs may be nullptr, if array base is NOT an LDA(ID).
            //e.g: given (*p)[1], p is pointer that point to an array.
            MD const* use = ir->getExactRef();
            if (use != nullptr) {
                ret_mds->bunion(use, *getSBSMgr());
            }
        }
        return;
    }
}


//Walk through IR tree to compute or collect referrenced MD.
//'ret_mds': In COMP_EXP_RECOMPUTE mode, it is used as tmp;
//  and in COMP_EXP_COLLECT_MUST_USE mode, it is
//  used to collect MUST-USE MD.
//Note this function will update ir's RefMD and RefMDSet if duflag contain
//COMP_EXP_RECOMPUTE
void DUMgr::computeExpression(IR * ir, OUT MDSet * ret_mds,
                              UINT compflag, UINT duflag)
{
    if (ir == nullptr) { return; }
    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    case IR_CONST: break;
    case IR_ID:
        if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            ; //It does not use MD.
        }
        break;
    case IR_LD:
        if (HAVE_FLAG(compflag, COMP_EXP_RECOMPUTE)) {
            //The reference MD should be assigned first.
            ASSERT0(ir->getRefMD());

            //e.g: struct {int a;} s;
            //s = ...
            //s.a = ...
            //Where s and s.a is overlapped.

            //Always compute overlapping MDSet for both PR and NON-PR MD
            //because DURef also have to compute NON-PR MD even if
            //only ReachDef of PR is required by user.
            computeOverlapMDSet(ir, false);
        } else if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            //Collect the exact MD that current ir 'must' reference.
            MD const* t = ir->getRefMD();
            ASSERT0(t);
            if (t->is_exact()) {
                ret_mds->bunion_pure(t->id(), *getSBSMgr());
            }
        }
        break;
    case IR_ILD:
        if (HAVE_FLAG(compflag, COMP_EXP_RECOMPUTE)) {
            //Sideeffect information should have been computed by AA.
            //e.g: ... = ild(ld(p)) //p->a, p->b
            //mayref of ild is: {a,b}, and mustref is nullptr.
            //mustref of ld is: {p}, and mayref is nullptr.
            computeExpression(ILD_base(ir), ret_mds, compflag, duflag);
            //if (HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU)) {
            //    computeOverlapMDSet(ir, false);
            //}
            //Always compute overlapping MDSet for both PR and NON-PR MD
            //because DURef also have to compute NON-PR MD even if
            //only ReachDef of PR is required by user.
            computeOverlapMDSet(ir, false);
        } else if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            //Collect the exact MD that current ir 'must' reference.
            MD const* t = ir->getRefMD();
            if (t != nullptr && t->is_exact()) {
                ret_mds->bunion_pure(t->id(), *getSBSMgr());
            }

            MD * use;
            MDSet const* use_mds = ir->getRefMDSet();
            if (use_mds != nullptr &&
                (use = use_mds->get_exact_md(m_md_sys)) != nullptr) {
                //If may-use MDSet only contain one element, there is a
                //'must' reference actually.
                ret_mds->bunion(use, *getSBSMgr());
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
            ASSERT0(ir->getRefMD() == nullptr);
            ASSERT0(ir->getRefMDSet() == nullptr);
        } else if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            ; //LDA does not use MD.
        }
        break;
    SWITCH_CASE_BIN:
        //Binary operation.
        computeExpression(BIN_opnd0(ir), ret_mds, compflag, duflag);
        computeExpression(BIN_opnd1(ir), ret_mds, compflag, duflag);
        ASSERT0(ir->getDU() == nullptr);
        break;
    SWITCH_CASE_UNA:
        computeExpression(UNA_opnd(ir), ret_mds, compflag, duflag);
        ASSERT0(ir->getDU() == nullptr);
        break;
    case IR_LABEL:
        break;
    case IR_ARRAY:
        computeArrayRef(ir, ret_mds, compflag, duflag);
        break;
    case IR_PR:
        ASSERT0(ir->getRefMDSet() == nullptr);
        if (HAVE_FLAG(compflag, COMP_EXP_COLLECT_MUST_USE)) {
            MD const* t = ir->getRefMD();
            ASSERT0(t);
            ret_mds->bunion(t, *getSBSMgr());
        }
        break;
    case IR_SELECT:
        computeExpression(SELECT_pred(ir), ret_mds, compflag, duflag);
        computeExpression(SELECT_trueexp(ir), ret_mds, compflag, duflag);
        computeExpression(SELECT_falseexp(ir), ret_mds, compflag, duflag);
        break;
    default: ASSERTN(0, ("Unsupport IR code"));
    }
}


//Dump mem usage for each ir DU reference.
void DUMgr::dumpMemUsageForMDRef() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(),
         "\n==---- DUMP DUMgr : Memory Usage for DU Reference '%s' ----==",
         m_rg->getRegionName());

    note(getRegion(), "\nMustD: must defined MD");
    note(getRegion(), "\nMayDs: overlapped and may defined MDSet");

    note(getRegion(), "\nMustU: must used MD");

    note(getRegion(), "\nMayUs: overlapped and may used MDSet");
    BBList * bbs = m_rg->getBBList();
    size_t count = 0;
    CHAR const* str = nullptr;
    ConstIRIter citer;
    DUMgr * pthis = const_cast<DUMgr*>(this);
    for (IRBB * bb = bbs->get_head(); bb != nullptr; bb = bbs->get_next()) {
        note(getRegion(), "\n--- BB%d ---", bb->id());
        for (IR * ir = BB_first_ir(bb);
             ir != nullptr; ir = BB_next_ir(bb)) {
            dumpIR(ir, m_rg);
            note(getRegion(), "\n");
            citer.clean();
            for (IR const* x = iterInitC(ir, citer);
                 x != nullptr; x = iterNextC(citer)) {
                note(getRegion(), "\n\t%s(%d)::", IRNAME(x), IR_id(x));
                if (x->is_stmt()) {
                    //MustDef
                    MD const* md = const_cast<IR*>(x)->getMustRef();
                    if (md != nullptr) {
                        prt(getRegion(), "MustD%uB, ", (UINT)sizeof(MD));
                        count += sizeof(MD);
                    }

                    //MayDef
                    MDSet const* mds = const_cast<IR*>(x)->getMayRef();
                    if (mds != nullptr) {
                        size_t n = mds->count_mem();
                        if (n < 1024) { str = "B"; }
                        else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
                        else  { n /= 1024*1024; str = "MB"; }

                        MDSetIter iter;
                        prt(getRegion(), "MayDs(%lu%s, %d elems, last %d), ",
                            (ULONG)n, str, mds->get_elem_count(),
                            mds->get_last(&iter));
                        count += n;
                    }
                } else {
                    //MustUse
                    MD const* md = x->getMustRef();
                    if (md != nullptr) {
                        prt(getRegion(), "MustU%dB, ", (INT)sizeof(MD));
                        count += sizeof(MD);
                    }

                    //MayUse
                    MDSet const* mds = const_cast<IR*>(x)->getMayRef();
                    if (mds != nullptr) {
                        size_t n = mds->count_mem();
                        if (n < 1024) { str = "B"; }
                        else if (n < 1024 * 1024) { n /= 1024; str = "KB"; }
                        else { n /= 1024*1024; str = "MB"; }

                        MDSetIter iter;
                        prt(getRegion(), "MayUs(%lu%s, %d elems, last %d), ",
                            (ULONG)n, str, mds->get_elem_count(),
                            mds->get_last(&iter));
                        count += n;
                    }
                }
            }
         }
    }

    if (count < 1024) { str = "B"; }
    else if (count < 1024 * 1024) { count /= 1024; str = "KB"; }
    else { count /= 1024*1024; str = "MB"; }
    note(getRegion(), "\nTotal %u%s", (UINT)count, str);
}


void DUMgr::dumpBBDUChainDetail(UINT bbid) const
{
    dumpBBDUChainDetail(m_cfg->getBB(bbid));
}


void DUMgr::dumpBBDUChainDetail(IRBB * bb) const
{
    ASSERT0(bb);
    note(getRegion(), "\n--- BB%d ---", bb->id());

    //Label Info list.
    LabelInfo const* li = bb->getLabelList().get_head();
    if (li != nullptr) {
        note(getRegion(), "\nLABEL:");
    }
    for (; li != nullptr; li = bb->getLabelList().get_next()) {
        switch (LABELINFO_type(li)) {
        case L_CLABEL:
            note(getRegion(), CLABEL_STR_FORMAT, CLABEL_CONT(li));
            break;
        case L_ILABEL:
            note(getRegion(), ILABEL_STR_FORMAT, ILABEL_CONT(li));
            break;
        case L_PRAGMA:
            ASSERT0(LABELINFO_pragma(li));
            note(getRegion(), "%s", SYM_name(LABELINFO_pragma(li)));
            break;
        default: UNREACHABLE();
        }

        if (LABELINFO_is_try_start(li) || LABELINFO_is_try_end(li) ||
            LABELINFO_is_catch_start(li)) {
            prt(getRegion(), "(");
            if (LABELINFO_is_try_start(li)) {
                prt(getRegion(), "try_start,");
            }
            if (LABELINFO_is_try_end(li)) {
                prt(getRegion(), "try_end,");
            }
            if (LABELINFO_is_catch_start(li)) {
                prt(getRegion(), "catch_start");
            }
            prt(getRegion(), ")");
        }

        prt(getRegion(), " ");
    }

    for (IR * ir = BB_irlist(bb).get_head();
         ir != nullptr; ir = BB_irlist(bb).get_next()) {
        dumpIR(ir, m_rg);
        note(getRegion(), "\n");

        IRIter ii;
        for (IR * k = iterInit(ir, ii); k != nullptr; k = iterNext(ii)) {
            if (!k->isMemoryRef() && !k->hasResult() && !k->is_region()) {
                continue;
            }

            note(getRegion(), "\n\t>>%s", IRNAME(k));
            if (k->is_stpr() || k->is_pr()) {
                prt(getRegion(), "%d", k->getPrno());
            }
            prt(getRegion(), "(id:%d) ", IR_id(k));

            if (k->is_stmt()) {
                prt(getRegion(), "DEFREF:");
            } else {
                prt(getRegion(), "USEREF:");
            }

            //Dump must ref.
            MD const* md = k->getRefMD();
            if (md != nullptr) {
                prt(getRegion(), "EMD%d", md->id());
            }

            //Dump may ref.
            MDSet const* mds = k->getRefMDSet();
            if (mds != nullptr && !mds->is_empty()) {
                if (md != nullptr) {
                    prt(getRegion(), ",");
                }
                MDSetIter iter;
                for (INT i = mds->get_first(&iter); i >= 0; ) {
                    prt(getRegion(), "MD%d", i);
                    i = mds->get_next(i, &iter);
                    if (i >= 0) { prt(getRegion(), ","); }
                }
            }

            //Dump def/use list.
            if (k->is_stmt() || ir->is_lhs(k)) {
                note(getRegion(), "\n\t  USE-EXP LIST:");
            } else {
                note(getRegion(), "\n\t  DEF-STMT LIST:");
            }

            DUSet const* set = k->readDUSet();
            if (set == nullptr) { continue; }

            DUSetIter di = nullptr;
            for (INT i = set->get_first(&di); i >= 0; ) {
                IR const* ref = m_rg->getIR(i);
                prt(getRegion(), "%s", IRNAME(ref));
                if (ref->is_stpr() || ref->is_pr()) {
                    prt(getRegion(), "%d", ref->getPrno());
                }
                prt(getRegion(), "(id:%d)", IR_id(ref));

                i = set->get_next(i, &di);
                if (i >= 0) { prt(getRegion(), ","); }
            }
        }
        note(getRegion(), "\n");
    } //end for each IR
}


bool DUMgr::dump() const
{
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    if (g_dump_opt.isDumpMDSetHash()) {
        m_mds_hash->dump(getRegion());
    }
    m_rg->dumpRef();
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
    return true;
}


//The difference between this function and dumpDUChain is this function
//will dump DU chain for each memory reference.
void DUMgr::dumpDUChainDetail() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    START_TIMER_FMT(t, ("DUMP DUChain %s", getPassName()));
    note(getRegion(), "\n\n==---- DUMP DUMgr DU CHAIN of '%s' ----==\n",
         m_rg->getRegionName());
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        dumpBBDUChainDetail(bb);
    }
    END_TIMER_FMT(t, ("DUMP DUChain %s", getPassName()));
}


//Dump DU chain only for stmt.
//This function collects must and may USE of MD and regard stmt as a whole.
//So this function does not distingwish individual memory operand inside the
//stmt, but if you want, please invoke dumpDUChainDetail().
void DUMgr::dumpDUChain() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n\n==---- DUMP '%s' DU chain ----==\n",
         m_rg->getRegionName());
    MDSet mds;
    DefMiscBitSetMgr bsmgr;
    BBList * bbl = m_rg->getBBList();
    DUMgr * pthis = const_cast<DUMgr*>(this);
    ConstIRIter citer;
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(getRegion(), "\n--- BB%d ---", bb->id());
        for (IR * ir = BB_irlist(bb).get_head();
             ir != nullptr; ir = BB_irlist(bb).get_next()) {
            dumpIR(ir, m_rg);
            note(getRegion(), "\n>>");

            //MustDef
            prt(getRegion(), "Dref(");
            bool has_prt_something = false;
            MD const* md = ir->getMustRef();
            if (md != nullptr) {
                prt(getRegion(), "EMD%d", md->id());
                has_prt_something = true;
            }

            //MayDef
            if (const_cast<IR*>(ir)->getMayRef() != nullptr) {
                mds.clean(bsmgr);
                MDSet const* x = ir->getMayRef();
                if (x != nullptr) {
                    if (has_prt_something) {
                        prt(getRegion(), ",");
                    }

                    MDSetIter iter;
                    for (INT i = x->get_first(&iter); i >= 0;) {
                        prt(getRegion(), "MD%d", i);
                        i = x->get_next(i, &iter);
                        if (i >= 0) {
                            prt(getRegion(), ",");
                        }
                    }
                    has_prt_something = true;
                }
            }
            if (!has_prt_something) {
                prt(getRegion(), "--");
            }
            prt(getRegion(), ")");

            //MayUse
            prt(getRegion(), " <= ");
            mds.clean(bsmgr);

            prt(getRegion(), "Uref(");
            pthis->collectMayUseRecursive(ir, mds, true, bsmgr);
            if (!mds.is_empty()) {
                mds.dump(m_md_sys);
            } else {
                prt(getRegion(), "--");
            }
            prt(getRegion(), ")");

            //Dump def chain.
            citer.clean();
            bool first = true;
            for (IR const* x = iterRhsInitC(ir, citer);
                 x != nullptr; x = iterRhsNextC(citer)) {
                 if (!x->isMemoryOpnd()) { continue; }

                DUSet const* defset = x->readDUSet();
                if (defset == nullptr || defset->get_elem_count() == 0) {
                    continue;
                }

                if (first) {
                    note(getRegion(), "\n>>DEF List:");
                    first = false;
                }

                DUSetIter di = nullptr;
                for (INT i = defset->get_first(&di);
                     i >= 0; i = defset->get_next(i, &di)) {
                    IR const* def = m_rg->getIR(i);
                    prt(getRegion(), "%s(id:%d), ", IRNAME(def), def->id());
                }
            }

            //Dump use chain.
            DUSet const* useset = ir->readDUSet();
            if (useset == nullptr || useset->get_elem_count() == 0) { continue; }

            note(getRegion(), "\n>>USE List:");
            DUSetIter di = nullptr;
            for (INT i = useset->get_first(&di);
                 i >= 0; i = useset->get_next(i, &di)) {
                IR const* u = m_rg->getIR(i);
                prt(getRegion(), "%s(id:%d), ", IRNAME(u), IR_id(u));
            }
        } //end for each IR
    } //end for each BB
    mds.clean(bsmgr);
}


//The function copy DU chain for 'to' from 'from', which will append
//'to' as an USE of DEFs.
//The function will build DU chain from all DEFs of 'from' to expression 'to'.
//to: root expression of target tree.
//from: root expression of source tree.
//Note IR tree 'to' and 'from' must be identical structure.
//Both 'to' and 'from' must be expression.
void DUMgr::addUse(IR * to, IR const* from)
{
    if (to == from) { return; }
    ASSERT0(to->is_exp() && from->is_exp());
    ASSERT0(to->isIREqual(from, true));
    m_citer.clean();
    m_iter2.clean();
    IR const* from_ir = iterInitC(from, m_citer);
    for (IR * to_ir = iterInit(to, m_iter2); to_ir != nullptr;
         to_ir = iterNext(m_iter2), from_ir = iterNextC(m_citer)) {
        ASSERT0(to_ir->isIREqual(from_ir, false));
        if (!to_ir->isMemoryRef() && !to_ir->is_id()) {
            //Copy MD for IR_ID, some Passes need it, e.g. GVN.
            continue;
        }

        DUSet const* from_du = from_ir->readDUSet();
        if (from_du == nullptr || from_du->is_empty()) { continue; }

        DUSet * to_du = genDUSet(to_ir);
        to_du->copy(*from_du, *getSBSMgr());

        //Add DU chain between DEF and USE.
        DUSetIter di = nullptr;
        for (UINT i = from_du->get_first(&di);
             di != nullptr; i = from_du->get_next(i, &di)) {
            //x is stmt if from_ir is expression.
            //x is expression if from_ir is stmt.
            IR const* x = m_rg->getIR(i);
            DUSet * x_duset = x->getDUSet();
            if (x_duset == nullptr) { continue; }
            x_duset->add(IR_id(to_ir), *getSBSMgr());
        }
    }
    ASSERT0(m_iter2.get_elem_count() == 0 && m_iter.get_elem_count() == 0);
}


//Remove 'def' out of ir's DEF set. ir is exp.
void DUMgr::removeDef(IR const* ir, IR const* def)
{
    ASSERT0(ir->is_exp() && def->is_stmt());
    DUSet * duset = ir->getDUSet();
    if (duset == nullptr) { return; }
    duset->removeDef(def, *getSBSMgr());
}


//Return true if mustdef or maydef overlaped with use's referrence.
bool DUMgr::isOverlappedDefUse(MD const* mustdef, MDSet const* maydef,
                               IR const* use)
{
    if (maydef != nullptr) {
        MDSet const* mayuse = const_cast<IR*>(use)->getMayRef();
        if (mayuse != nullptr &&
            (mayuse == maydef ||
             mayuse->is_intersect(*maydef))) {
            return true;
        }

        MD const* mustuse = const_cast<IR*>(use)->getMustRef();
        if (mustuse != nullptr && maydef->is_contain(mustuse)) {
            return true;
        }
    }

    if (mustdef != nullptr) {
        MDSet const* mayuse = const_cast<IR*>(use)->getMayRef();
        if (mustdef != nullptr) {
            if (mayuse != nullptr && mayuse->is_contain(mustdef)) {
                return true;
            }

            MD const* mustuse = use->getMustRef();
            if (mustuse != nullptr &&
                (mustuse == mustdef || mustuse->is_overlap(mustdef))) {
                return true;
            }
        }
    }

    return false;
}


//Check each DEF of exp, remove the expired stmt which is not the memory
//any more that exp referenced.
//Check if the DEF of stmt's operands still modify the same memory object.
//e.g: Revise DU chain if stmt's rhs has been changed.
//    x=10 //S1
//    ...
//    c=x*0 //S2
//after changed =>
//    x=10 //S1
//    ...
//    c=0 //S2
//where S1 is DEF, S2 is USE, after ir refinement, x in S2
//is removed, remove the data dependence between S1
//and S2's operand.
bool DUMgr::removeExpiredDU(IR * ir)
{
    ASSERT0(ir->isMemoryRef());
    bool change = false;
    ASSERT0(ir);
    DUSet const* set = ir->readDUSet();
    if (set == nullptr) { return false; }

    if (ir->is_stmt()) {
        MDSet const* may = ir->getMayRef();
        MD const* must = ir->getMustRef();
        DUSetIter di = nullptr;
        UINT next_u;
        for (UINT u = set->get_first(&di); di != nullptr; u = next_u) {
            next_u = set->get_next(u, &di);
            IR const* ref = m_rg->getIR(u);
            ASSERT0(ref->is_exp());
            if (DUMgr::isOverlappedDefUse(must, may, ref)) { continue; }
 
            //There is no DU chain bewteen stmt and use. Cutoff the DU.
            removeDUChain(ir, ref);
            change = true;
        }
        return change;
    }

    DUSetIter di = nullptr;
    UINT next_u;
    for (UINT u = set->get_first(&di); di != nullptr; u = next_u) {
        next_u = set->get_next(u, &di);
        IR const* ref = m_rg->getIR(u);
        ASSERT0(ref->is_stmt());
        MDSet const* may = ir->getMayRef();
        MD const* must = ir->getMustRef();
        if (DUMgr::isOverlappedDefUse(must, may, ir)) { continue; }
 
        //There is no DU chain bewteen stmt and use. Cutoff the DU.
        removeDUChain(ref, ir);
        change = true;
    }
    return change;
}


//Check if the DEF of stmt's operands still modify the same memory object.
//e.g: Revise DU chain if stmt's rhs has been changed.
//    x=10 //S1
//    ...
//    c=x*0 //S2
//after changed =>
//    x=10 //S1
//    ...
//    c=0 //S2
//where S1 is DEF, S2 is USE, after ir refinement, x in S2
//is removed, remove the data dependence between S1
//and S2's operand.
bool DUMgr::removeExpiredDUForOperand(IR * stmt)
{
    bool change = false;
    m_citer.clean();
    for (IR const* k = iterRhsInitC(stmt, m_citer);
         k != nullptr; k = iterRhsNextC(m_citer)) {
        if (!k->isMemoryOpnd()) { continue; }

        SSAInfo * ssainfo;
        if (k->isReadPR() && (ssainfo = PR_ssainfo(k)) != nullptr) {
            SSAUseIter si;
            UINT prno = 0;
            if (ssainfo->getDef() != nullptr) {
                prno = ssainfo->getDef()->getPrno();
            } else {
                continue;
            }

            INT ni = -1;
            for (INT i = SSA_uses(ssainfo).get_first(&si);
                 si != nullptr; i = ni) {
                ni = SSA_uses(ssainfo).get_next(i, &si);
                IR const* use = m_rg->getIR(i);

                if (use->is_pr() && PR_no(use) == prno) { continue; }

                ssainfo->removeUse(use);
            }

            continue;
        }

        DUSet const* defset = k->readDUSet();
        if (defset == nullptr) { continue; }

        DUSetIter di = nullptr;
        UINT nd;
        for (UINT d = defset->get_first(&di); di != nullptr; d = nd) {
            nd = defset->get_next(d, &di);

            IR const* def = m_rg->getIR(d);
            ASSERT0(def->is_stmt());

            if (DUMgr::isOverlappedDefUse(const_cast<IR*>(def)->getMustRef(),
                                       const_cast<IR*>(def)->getMayRef(), k)) {
                continue;
            }

            //There is no DU chain bewteen d and u. Cut the DU chain.
            removeDUChain(def, k);
            change = true;
        }
    }
    return change;
}


//Check if the DEF of stmt's operands still modify the same memory object.
//e.g: Revise DU chain if stmt's rhs has been changed.
//    x=10 //S1
//    ...
//    c=x*0 //S2
//after changed =>
//    x=10 //S1
//    ...
//    c=0 //S2
//where S1 is DEF, S2 is USE, after ir refinement, x in S2
//is removed, remove the data dependence between S1
//and S2's operand.
bool DUMgr::removeExpiredDUIRTree(IR * stmt)
{
    ASSERT0(stmt->is_stmt());
    bool change = removeExpiredDU(stmt);
    change |= removeExpiredDUForOperand(stmt);
    return change;
}


//The function checks all USE of memory references of entire IR tree and
//cutoff its DU chain. 'ir' may be stmt or expression, if ir is stmt,
//check its right-hand-side.
//This function will process SSA info if it exists.
//'ir': indicate the root of IR tree.
//e.g: d1, d2 are def-stmt of stmt's operands.
//The functin cutoff DU chain between d1, d2 and their use.
void DUMgr::removeUseFromDefset(IR * ir)
{
    m_citer.clean();
    IR const* k;
    if (ir->is_stmt()) {
        k = iterRhsInitC(ir, m_citer);
    } else {
        k = iterExpInitC(ir, m_citer);
    }

    for (; k != nullptr; k = iterRhsNextC(m_citer)) {
        if (!k->isMemoryOpnd()) { continue; }

        //SSAInfo has been processed in PRSSAMgr::removeUse().
        //SSAInfo * ssainfo;
        //if ((ssainfo = k->getSSAInfo()) != nullptr) {
        //    ASSERT0(k->is_pr());
        //    ssainfo->removeUse(k);
        //    continue;
        //}

        DUSet * defset = k->getDUSet();
        if (defset == nullptr) { continue; }

        DUSetIter di = nullptr;
        bool doclean = false;
        for (INT i = defset->get_first(&di);
             i >= 0; i = defset->get_next(i, &di)) {
            doclean = true;
            IR const* stmt = m_rg->getIR(i);
            ASSERT0(stmt->is_stmt());

            DUSet * useset = stmt->getDUSet();
            if (useset == nullptr) { continue; }
            useset->removeUse(k, *getSBSMgr());
        }
        if (doclean) {
            defset->clean(*getSBSMgr());
        }
    }
}


//Note that do NOT use this function to remove SSA def.
//This function handle the MD DU chain and cut
//off the DU chain between MD def and its MD use expression.
//Remove 'def' from its use's def-list.
//e.g:u1, u2 are its use expressions.
//cutoff the DU chain between def->u1 and def->u2.
void DUMgr::removeDefFromUseset(IR * def)
{
    ASSERT0(def->is_stmt());

    //Can not just remove the SSA def, you should consider the SSA_uses
    //and make sure they are all removable. Use SSA form related api.
    //DO not assert here for convenient to experimental behaviors.
    //PRSSA info should be maintained in PRSSAMgr.
    //ASSERT0(def->getSSAInfo() == nullptr);

    DUSet * useset = def->getDUSet();
    if (useset == nullptr) { return; }

    DUSetIter di = nullptr;
    bool doclean = false;
    //Remove the DU chain bewteen DEF and its USE.
    for (INT i = useset->get_first(&di);
         i >= 0; i = useset->get_next(i, &di)) {
        doclean = true;
        IR const* exp = m_rg->getIR(i);
        ASSERT0(exp->is_exp());

        DUSet * du = exp->getDUSet();
        if (du != nullptr) { du->removeDef(def, *getSBSMgr()); }
    }

    //Clean USE set.
    if (doclean) {
        useset->clean(*getSBSMgr());
    }
}


//Remove all DU info of 'ir' from DU mgr.
void DUMgr::removeIRFromDUMgr(IR * ir)
{
    ASSERT0(ir->is_stmt());
    removeUseFromDefset(ir);

    //If stmt has SSA info, it should be maintained by SSA related api.
    removeDefFromUseset(ir);
}

//Count up the memory has been allocated.
size_t DUMgr::count_mem() const
{
    size_t count = sizeof(m_mds_mgr);
    count += smpoolGetPoolSize(m_pool);
    count += m_solve_set.count_mem();
    return count;
}
 
//Count up memory of DUSet for all irs.
size_t DUMgr::count_mem_duset()
{
    size_t count = 0;
    Vector<IR*> * vec = m_rg->getIRVec();
    INT l = vec->get_last_idx();
    for (INT i = 1; i <= l; i++) {
        IR const* ir = vec->get(i);
        DUSet const* duset = ir->readDUSet();
        if (duset != nullptr) {
            count += duset->count_mem();
        }
    }
    return count;
}


//Coalesce DU chain of 'from' to 'to'.
//This function replace definition of USE of 'from' to defintion of 'to'.
//Just like copy-propagation.
//e.g: to_def =...
//     from = to
//     ...= from_use
//=> after coalescing
//     to_def = ...
//     ------ //removed
//     ... = to
void DUMgr::coalesceDUChain(IR * from, IR * to)
{
    ASSERT0(from && to);
    ASSERT0(from->is_stmt() && to->is_exp() && to->getStmt() == from);
    if (from->getDUSet() == nullptr || to->getDUSet() == nullptr) {
        //Neight from nor to has DU chain.
        return removeIRFromDUMgr(from);
    }

    //Use exp set of 'from'.
    DUSet * uses = from->getDUSet();
    //Def stmt set of 'to'.
    DUSet * defs = to->getDUSet();
    defs->remove(from->id(), *getSBSMgr());
   
    //Iterate each DEF of 'to', remove 'to' from their UseSet.
    DUSetIter it = nullptr;
    for (INT i = defs->get_first(&it); i >= 0; i = defs->get_next(i, &it)) {
        IR const* def = m_rg->getIR((UINT)i);
        ASSERT0(def && def->isMemoryRef());
        DUSet * useset = def->getDUSet();
        ASSERT0(useset);
        useset->removeUse(to, *getSBSMgr());
    }

    //Iterate USE of 'from', change each USE's definition to to_def.
    DUSetIter it1 = nullptr;
    for (INT i = uses->get_first(&it1); i >= 0; i = uses->get_next(i, &it1)) {
        IR const* use = m_rg->getIR((UINT)i);
        ASSERT0(use && use->isMemoryRef());
        DUSet * defs_of_use = use->getDUSet();
        ASSERT0(defs_of_use);
        defs_of_use->removeDef(from, *getSBSMgr());

        DUSetIter it2 = nullptr;
        for (INT j = defs->get_first(&it2);
             j >= 0; j = defs->get_next(j, &it2)) {
            IR const* def = m_rg->getIR(j);
            ASSERT0(def && def->is_stmt());

            DUSet * uses_of_def = def->getDUSet();
            ASSERT0(uses_of_def);
            uses_of_def->removeUse(to, *getSBSMgr());
            if (use != to) {
                uses_of_def->addUse(use, *getSBSMgr());
            }
        }
        defs_of_use->bunion(*defs, *getSBSMgr());
    }
    defs->clean(*getSBSMgr());
    uses->clean(*getSBSMgr());
}


//Collect MustUse MDSet for both PR operation and Non-PR operation.
//e.g: = a + b + *p;
//    assume p->w,u, the MustUse is {a,b,p}, not include w,u.
void DUMgr::collectMustUsedMDs(IR const* ir, OUT MDSet & mustuse)
{
    ASSERT0(ir->is_stmt());
    m_citer.clean();
    for (IR const* x = iterRhsInitC(ir, m_citer);
        x != nullptr; x = iterRhsNextC(m_citer)) {
        if (!x->isMemoryOpnd()) { continue; }
        computeExpression(const_cast<IR*>(x), &mustuse,
                          COMP_EXP_COLLECT_MUST_USE,
                          DUOPT_COMPUTE_PR_DU | DUOPT_COMPUTE_NONPR_DU);
    }
}


void DUMgr::inferStore(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_st() && ir->getRefMD());

    //if (HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU)) {
        //Find ovelapped MD.
        //e.g: struct {int a;} s;
        //s = ...
        //s.a = ...
        //Where s and s.a is overlapped.

        //Always compute overlapping MDSet for both PR and NON-PR MD
        //because DURef also have to compute NON-PR MD even if
        //only ReachDef of PR is required by user.
        computeOverlapMDSet(ir, false);
    //}

    computeExpression(ST_rhs(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
}


void DUMgr::inferStoreArray(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_starray());
    computeArrayRef(ir, nullptr, COMP_EXP_RECOMPUTE, duflag);
    computeExpression(STARR_rhs(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
}


void DUMgr::inferStorePR(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_stpr() && ir->getRefMD() && ir->getRefMDSet() == nullptr);
    computeExpression(STPR_rhs(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
}


void DUMgr::inferSetelem(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_setelem() && ir->getRefMD() && ir->getRefMDSet() == nullptr);
    computeExpression(SETELEM_base(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
    computeExpression(SETELEM_val(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
    computeExpression(SETELEM_ofst(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
}


void DUMgr::inferGetelem(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_getelem() && ir->getRefMD() && ir->getRefMDSet() == nullptr);
    computeExpression(GETELEM_base(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
    computeExpression(GETELEM_ofst(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
}


void DUMgr::inferPhi(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_phi() && ir->getRefMD() && ir->getRefMDSet() == nullptr);
    //Set call result list MD.
    for (IR * r = PHI_opnd_list(ir); r != nullptr; r = r->get_next()) {
        ASSERT0(r->isPhiOpnd());
        computeExpression(r, nullptr, COMP_EXP_RECOMPUTE, duflag);
    }
}


void DUMgr::inferIStore(IR * ir, UINT duflag)
{
    ASSERT0(ir->is_ist());
    computeExpression(IST_base(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);

    //Compute DEF mdset. AA should guarantee either mustdef is not nullptr or
    //maydef not nullptr.
    ASSERT0((ir->getRefMDSet() && !ir->getRefMDSet()->is_empty()) ||
            (ir->getRefMD()));

    //Always compute overlapping MDSet for both PR and NON-PR MD
    //because DURef also have to compute NON-PR MD even if
    //only ReachDef of PR is required by user.
    computeOverlapMDSet(ir, false);

    computeExpression(IST_rhs(ir), nullptr, COMP_EXP_RECOMPUTE, duflag);
}


//Return true if the output result has united call's MayDef.
bool DUMgr::inferCallStmtForNonPRViaCallGraph(IR const* ir,
                                              OUT MDSet & maydefuse)
{
    //Prefer to use calllee's MayDef if callee region has been processed.
    CallGraph * callg = m_rg->getRegionMgr()->getCallGraph();
    if (callg == nullptr) { return false; }

    Region const* callee = callg->mapCall2Region(ir, m_rg);
    if (callee == nullptr || !callee->is_ref_valid()) { return false; }

    MDSet const* maydef = callee->getMayDef();
    if (maydef == nullptr || maydef->is_empty()) { return false; }

    maydefuse.bunion_pure(*maydef, *getSBSMgr());
    return true;
}


//Set given set to be more conservative MD reference set.
//worst: true to set result to the worst case.
void DUMgr::setToWorstCase(IR * ir)
{
    //CASE: conservative purpose.
    //void foo(size_t v)
    //{
    //    int ** w = (int**)v;
    //    int * p = *w;
    //    *p = 20;
    //}
    //int main()
    //{
    //    int i;
    //    i = 1;
    //    int * p;
    //    p = &i;
    //    foo((size_t)&p); //both i and p changed.
    //    return i;
    //}
    //This is a case to WORST PRECISION: A call should not
    //reference all elements in may-point-to set, because local
    //variable that do not be taken address can not be changed by call.
    //In order to improve precision, we have to query MayDef/MayUse
    //info from CallGraph.
    ir->setMayRef(m_aa->getMayPointToMDSet(), m_rg);
}


void DUMgr::setToConservative(OUT MDSet & maydefuse)
{
    //For conservative purpose.
    //Set to mod/ref global memor, all imported variables,
    //and all exposed local variables for conservative purpose.
    maydefuse.bunion(m_md_sys->getMD(MD_GLOBAL_VAR), *getSBSMgr());
    maydefuse.bunion(m_md_sys->getMD(MD_IMPORT_VAR), *getSBSMgr());
}


void DUMgr::genDummyuseForCallStmt(IR * ir, MDSet const* mayref)
{
    //Build dummyuse for call.
    if (!((CCall*)ir)->hasDummyUse() || !CALL_dummyuse(ir)->is_ild()) {
        //TODO:consider append DefUse info on existed dummyuse.
        ((CCall*)ir)->addDummyUse(m_rg);
    }

    ASSERTN(CALL_dummyuse(ir)->is_ild(),
            ("ild can better present MD that based on different VAR"));
    CALL_dummyuse(ir)->setMayRef(mayref, m_rg);
}


void DUMgr::inferCallStmtForNonPR(IR * ir, UINT duflag)
{
    //Compute MD Reference for NonPR.
    MDSet maydefuse;
    MDSet const* worst = nullptr;
    if (m_aa->isWorstCase(ir->getMayRef())) {
        worst = ir->getMayRef();
    } else if (ir->getMayRef() != nullptr) {
        maydefuse.bunion(*ir->getMayRef(), *getSBSMgr());
    }

    //Analysis and set MustRef and MayRef for parameters.
    for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
        computeExpression(p, nullptr, COMP_EXP_RECOMPUTE, duflag);
    }

    //Regard MDSet of dummyuse as the MayRef of current CallStmt.
    for (IR * d = CALL_dummyuse(ir); d != nullptr; d = d->get_next()) {
        computeExpression(d, nullptr, COMP_EXP_RECOMPUTE, duflag);
        if (worst != nullptr) {
            //maydefuse already be the worst case.
            continue;
        }

        if (d->getRefMD() != nullptr) {
            maydefuse.bunion_pure(d->getRefMD()->id(), *getSBSMgr());
        }
        if (d->getRefMDSet() != nullptr && !d->getRefMDSet()->is_empty()) {
            maydefuse.bunion_pure(*d->getRefMDSet(), *getSBSMgr());
        }
    }

    if (ir->isReadOnly() ||
        inferCallStmtForNonPRViaCallGraph(ir, maydefuse)) {
        //Hash the MDSet.
        //Regard both USE and DEF as ir's MayRef.
        //TODO:differetiate the USE set and DEF set of CALL stmt
        //     for better DefUse precision.
        Vector<MD const*> tmp;
        m_md_sys->computeOverlap(m_rg, maydefuse, tmp,
                                 m_tab_iter, *getSBSMgr(), true);
        if (maydefuse.is_empty()) {
            ir->cleanMayRef();
        } else {
            MDSet const* hashed = m_mds_hash->append(maydefuse);
            ir->setMayRef(hashed, m_rg);
            genDummyuseForCallStmt(ir, hashed);
            maydefuse.clean(*getSBSMgr());
        }
        return;
    }

    if (worst == nullptr) {
        setToConservative(maydefuse);
        Vector<MD const*> tmpvec;
        m_md_sys->computeOverlap(m_rg, maydefuse, tmpvec, m_tab_iter,
                                 *getSBSMgr(), true);

        //Register the MDSet.
        //Regard both USE and DEF as ir's RefMDSet.
        //TODO:differetiate the USE set and DEF set of CALL stmt
        //     for better DefUse precision.
        MDSet const* hashed = m_mds_hash->append(maydefuse);
        ir->setMayRef(hashed, m_rg);
    } else {
        //ir already to be worst case.
        ASSERT0(m_aa->isWorstCase(ir->getMayRef()));
        //setToWorstCase(ir);
    }

    genDummyuseForCallStmt(ir, m_aa->getMayPointToMDSet());
    maydefuse.clean(*getSBSMgr());
}


//Inference call's MayDef MDSet.
//Regard both USE and DEF as ir's RefMDSet.
//NOTE: Call/ICall may modify addressable local
//      variables and globals in any indefinite ways.
void DUMgr::inferCallStmt(IR * ir, UINT duflag)
{
    ASSERT0(ir->isCallStmt());
    if (ir->is_icall()) {
        //Analysis callee's RefMDSet of IR_ICALL.
        computeExpression(ICALL_callee(ir), nullptr,
                          COMP_EXP_RECOMPUTE, duflag);
    }

    if (!HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_REF)) {
        //Only compute DU reference for PR.
        for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
            //Analysis RefMDSet of given IR.
            computeExpression(p, nullptr, COMP_EXP_RECOMPUTE, duflag);
        }
        for (IR * p = CALL_dummyuse(ir); p != nullptr; p = p->get_next()) {
            //Analysis RefMDSet of given IR.
            computeExpression(p, nullptr, COMP_EXP_RECOMPUTE, duflag);
        }
        return;
    }

    inferCallStmtForNonPR(ir, duflag);
}


//Collect MD which ir may use, include overlapped MD.
void DUMgr::collectMayUse(IR const* ir, MDSet & mayUse, bool computePR)
{
    m_citer.clean();
    IR const* x = nullptr;
    bool const is_stmt = ir->is_stmt();
    if (is_stmt) {
        x = iterRhsInitC(ir, m_citer);
    } else {
        x = iterExpInitC(ir, m_citer);
    }

    for (; x != nullptr; x = iterRhsNextC(m_citer)) {
        if (!x->isMemoryOpnd()) { continue; }

        ASSERT0(x->getParent());

        if ((x->is_id() || x->is_ld()) && x->getParent()->is_lda()) {
            continue;
        }

        if (x->is_pr() && computePR) {
            ASSERT0(const_cast<IR*>(x)->getMustRef());
            mayUse.bunion_pure(const_cast<IR*>(x)->getMustRef()->id(),
                               *getSBSMgr());
            continue;
        }

        MD const* mustref = const_cast<IR*>(x)->getMustRef();
        MDSet const* mayref = const_cast<IR*>(x)->getMayRef();

        if (mustref != nullptr) {
            mayUse.bunion(mustref, *getSBSMgr());
        }

        if (mayref != nullptr) {
            mayUse.bunion(*mayref, *getSBSMgr());
        }
    }

    if (is_stmt) {
        if (ir->isCallStmt()) {
            //Handle CALL/ICALL stmt sideeffect.
            bool done = false;
            CallGraph * callg = m_rg->getRegionMgr()->getCallGraph();
            if (callg != nullptr) {
                Region * rg = callg->mapCall2Region(ir, m_rg);
                if (rg != nullptr && rg->is_ref_valid()) {
                    MDSet const* muse = rg->getMayUse();
                    if (muse != nullptr) {
                        mayUse.bunion(*muse, *getSBSMgr());
                        done = true;
                    }
                }
            }

            if (!done) {
                //Regard CALL/ICALL's MayDef MDSet as MayUse.
                MDSet const* muse = ir->getRefMDSet();
                if (muse != nullptr) {
                    mayUse.bunion(*muse, *getSBSMgr());
                }
            }
        } else if (ir->is_region()) {
            MDSet const* x2 = REGION_ru(ir)->getMayUse();
            if (x2 != nullptr) {
                mayUse.bunion(*x2, *getSBSMgr());
            }
        }
    }
}


//Collect MD which ir may use, include overlapped MD.
void DUMgr::collectMayUseRecursiveIRList(IR const* ir, OUT MDSet & mayUse,
                                         bool computePR,
                                         DefMiscBitSetMgr & bsmgr)
{
  for (IR const* e = ir; e != nullptr; e = e->get_next()) {
    collectMayUseRecursive(e, mayUse, computePR, bsmgr);
  }
}


//Collect MD which ir may use, include overlapped MD.
void DUMgr::collectMayUseRecursive(IR const* ir,
                                   OUT MDSet & mayUse,
                                   bool computePR,
                                   DefMiscBitSetMgr & bsmgr)
{
    if (ir == nullptr) { return; }
    switch (ir->getCode()) {
    case IR_ID:
        //Note IR_ID may be dummy-USE declared by User to
        //notify DefUse analysis pass.
    case IR_LD:
        ASSERT0(ir->getParent() != nullptr);
        if (!ir->getParent()->is_lda()) {
            ASSERT0(const_cast<IR*>(ir)->getMustRef());
            mayUse.bunion(const_cast<IR*>(ir)->getMustRef(), bsmgr);

            MDSet const* ts = const_cast<IR*>(ir)->getMayRef();
            if (ts != nullptr) {
                mayUse.bunion(*ts, bsmgr);
            }
        }
        return;
    case IR_CONST:
        //CVT should not has any use-mds. Even if the operation
        //will genrerate different type.
        //Fall through
    case IR_ST:
    case IR_STPR:
    case IR_IST:
    case IR_LDA:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_STARRAY:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
    case IR_SELECT:
    case IR_CASE:
    case IR_PHI:
    case IR_GOTO:
    case IR_LABEL:
    case IR_IGOTO:
    case IR_SWITCH:
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == nullptr) { continue; }
            ASSERT0(k->is_exp());
            collectMayUseRecursiveIRList(k, mayUse, computePR, bsmgr);
        }
        return;
    case IR_ILD:
        collectMayUseRecursive(ILD_base(ir), mayUse, computePR, bsmgr);
        ASSERT0(ir->getParent() != nullptr);
        if (!ir->getParent()->is_lda()) {
            MD const* t = const_cast<IR*>(ir)->getMustRef();
            if (t != nullptr) {
                mayUse.bunion(t, bsmgr);
            }

            MDSet const* ts = const_cast<IR*>(ir)->getMayRef();
            if (ts != nullptr) {
                mayUse.bunion(*ts, bsmgr);
            }
        }
        return;
    case IR_ICALL:
    case IR_CALL: {
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == nullptr) { continue; }
            ASSERT0(k->is_exp());
            collectMayUseRecursiveIRList(k, mayUse, computePR, bsmgr);
        }
        bool done = false;
        CallGraph * callg = m_rg->getRegionMgr()->getCallGraph();
        if (callg != nullptr) {
            Region * rg = callg->mapCall2Region(ir, m_rg);
            if (rg != nullptr && rg->is_ref_valid()) {
                MDSet const* muse = rg->getMayUse();
                if (muse != nullptr) {
                    mayUse.bunion(*muse, bsmgr);
                    done = true;
                }
            }
        }
        if (!done) {
            //Regard MayDef MDSet as MayUse.
            MDSet const* muse = ir->getRefMDSet();
            if (muse != nullptr) {
                mayUse.bunion(*muse, bsmgr);
            }
        }
        return;
    }
    case IR_ARRAY: {
        ASSERT0(ir->getParent() != nullptr);
        MD const* t = const_cast<IR*>(ir)->getMustRef();
        if (t != nullptr) {
            mayUse.bunion(t, bsmgr);
        }

        MDSet const* ts = const_cast<IR*>(ir)->getMayRef();
        if (ts != nullptr) {
            mayUse.bunion(*ts, bsmgr);
        }
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == nullptr) { continue; }
            ASSERT0(k->is_exp());
            collectMayUseRecursiveIRList(k, mayUse, computePR, bsmgr);
        }
        return;
    }
    case IR_PR:
        if (!computePR) { return; }
        ASSERT0(const_cast<IR*>(ir)->getMustRef());
        mayUse.bunion_pure(const_cast<IR*>(ir)->getMustRef()->id(), bsmgr);
        return;
    case IR_REGION: {
        MDSet const* x = REGION_ru(ir)->getMayUse();
        if (x != nullptr && !x->is_empty()) {
            mayUse.bunion(*x, bsmgr);
        }
        return;
    }
    default: UNREACHABLE();
    }
}


//Compute Defined, Used md-set, Generated ir-stmt-set, and
//MayDefined md-set for each IR.
void DUMgr::computeCallRef(UINT duflag)
{
    BBList * bbl = m_rg->getBBList();
    BBListIter bbct;
    for (bbl->get_head(&bbct); bbct != nullptr; bbct = bbl->get_next(bbct)) {
        IRBB * bb = bbct->val();
        IRListIter irct;
        for (BB_irlist(bb).get_head(&irct); irct != BB_irlist(bb).end();
             irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            if (ir->isCallStmt()) {
                inferCallStmt(ir, duflag);
            }
        }
    }
}


//Compute Defined, Used md-set, Generated ir-stmt-set, and
//MayDefined md-set for each IR.
void DUMgr::computeMDRef(MOD OptCtx & oc, UINT duflag)
{
    START_TIMER(t1, "Build DU ref");
    ASSERT0(m_is_cached_mdset == nullptr && m_cached_overlap_mdset == nullptr);
    m_is_cached_mdset = new DefSBitSetCore();
    m_cached_overlap_mdset = new TMap<MD const*, MDSet const*>();

    BBList * bbl = m_rg->getBBList();
    BBListIter bbct;
    for (bbl->get_head(&bbct); bbct != nullptr; bbct = bbl->get_next(bbct)) {
        IRBB * bb = bbct->val();

        IRListIter irct;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end();
             irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            switch (ir->getCode()) {
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
                inferIStore(ir, duflag);
                break;
            case IR_CALL:
            case IR_ICALL:
                inferCallStmt(ir, duflag);
                break;
            case IR_RETURN:
                ASSERT0(RET_exp(ir) == nullptr || RET_exp(ir)->is_single());
                computeExpression(RET_exp(ir), nullptr,
                                  COMP_EXP_RECOMPUTE, duflag);
                break;
            case IR_TRUEBR:
            case IR_FALSEBR:
                //Compute USE mdset.
                ASSERT0(BR_lab(ir));
                computeExpression(BR_det(ir), nullptr,
                                  COMP_EXP_RECOMPUTE, duflag);
                break;
            case IR_SWITCH:
                //Compute USE mdset.
                computeExpression(SWITCH_vexp(ir), nullptr,
                                  COMP_EXP_RECOMPUTE, duflag);
                break;
            case IR_IGOTO:
                //Compute USE mdset.
                computeExpression(IGOTO_vexp(ir), nullptr,
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
            default: UNREACHABLE();
            }
            if (ir->is_atomic()) {
                computeAtomMDRef(ir);
            }
        }
    }

    delete m_cached_overlap_mdset;
    m_cached_overlap_mdset = nullptr;

    m_is_cached_mdset->clean(*getSBSMgr());
    delete m_is_cached_mdset;
    m_is_cached_mdset = nullptr;

    OC_is_ref_valid(oc) = true;
    ASSERT0(m_rg->verifyMDRef());
    END_TIMER(t1, "Build DU ref");
}


//This function compute and update MDRef
//according to operand in atom operation.
void DUMgr::computeAtomMDRef(IR * ir)
{
    ASSERT0(ir->is_atomic());
    //By default, RMW could be simulated by IR_CALL with 3 arguments, e.g:
    //call Opcode:i32, OldValueMemory:<valuetype>, NewValue:valuetype;
    //where Opcode defined the RMW operations, OldValueMemory indicates
    //the memory location with valuetype that hold oldvalue, and NewValue
    //is the value to be set.
    ASSERTN(!ir->is_call(), ("Target Dependent Code"));
    IR const* oldvaluemem = CALL_param_list(ir);
    ASSERTN(oldvaluemem, ("Target Dependent Code"));
    MDSet atomdefmds;
    if (ir->getRefMD() != nullptr) {
        atomdefmds.bunion(ir->getRefMD(), *getSBSMgr());
    }
    if (ir->getRefMDSet() != nullptr) {
        atomdefmds.bunion(*ir->getRefMDSet(), *getSBSMgr());
    }
    if (oldvaluemem->getRefMD() != nullptr) {
        atomdefmds.bunion(oldvaluemem->getRefMD(), *getSBSMgr());
    }
    if (oldvaluemem->getRefMDSet() != nullptr) {
        atomdefmds.bunion(*oldvaluemem->getRefMDSet(), *getSBSMgr());
    }
    ir->setMayRef(m_mds_hash->append(atomdefmds), m_rg);
    atomdefmds.clean(*getSBSMgr());
}


//Check if stmt is killing-def to usemd.
//This function matchs the following patterns:
//  CASE1:    st(mc<100>), x = ...
//                      ...  = ld(mc<99>), x
//  CASE2:    stpr %2 = ...
//                ... = pr %2
//Stmt and exp must be in same BB.
bool DUMgr::checkIsLocalKillingDefForDirectAccess(
    MD const* defmd, MD const* usemd, IR const* stmt,
    bool * has_nonkilling_local_def)
{
    if (stmt->isWriteWholePR()) {
        //For stmt that defined PR,
        return true;
    }
    if (defmd->is_exact()) {
        if (defmd == usemd || defmd->is_exact_cover(usemd)) {
            return true;
        }
        *has_nonkilling_local_def = true;
    }
    return false;
}


//Check if stmt is killing-define to exp.
//This function matchs the pattern, where dt means data-type and
//they must be identical.
//    ist(dt, ofst:n), x = ...
//    ... = ild(dt, ofst:n), x
//Stmt and exp must be in same bb.
UINT DUMgr::checkIsLocalKillingDefForIndirectAccess(IR const* stmt,
                                                    IR const* exp,
                                                    xcom::C<IR*> const* expct)
{
    ASSERT0(stmt->getBB() == exp->getStmt()->getBB());

    if (!exp->is_ild() || !stmt->is_ist()) { return CK_UNKNOWN; }

    IR const* t = ILD_base(exp);

    while (t->is_cvt()) { t = CVT_exp(t); }

    if (!t->is_pr() && !t->is_ld()) { return CK_UNKNOWN; }

    IR const* t2 = IST_base(stmt);

    while (t2->is_cvt()) { t2 = CVT_exp(t2); }

    if (!t2->is_pr() && !t2->is_ld()) { return CK_UNKNOWN; }

    if (t->getCode() != t2->getCode()) { return CK_UNKNOWN; }

    IRBB * curbb = stmt->getBB();

    //Note DEF stmt set of t must be avaiable here.
    //And t could not be defined between stmt and exp.
    //e.g:
    //    *base = ...  S1
    //    ... //base can not be modified in between ist(S1) and ild(S2).
    //    = *base      S2
    DUSet const* defset_of_t = t->readDUSet();
    if (defset_of_t != nullptr) {
        DUSetIter di = nullptr;
        for (UINT d = defset_of_t->get_first(&di);
             di != nullptr; d = defset_of_t->get_next(d, &di)) {
            IR const* def_of_t = m_rg->getIR(d);
            ASSERT0(def_of_t->is_stmt());
            if (def_of_t->getBB() != curbb) { continue; }

            //Find the def that clobber exp after the stmt.
            IRListIter localct = const_cast<C<IR*>*>(expct);
            for (IR * ir = BB_irlist(curbb).get_prev(&localct);
                 ir != nullptr; ir = BB_irlist(curbb).get_prev(&localct)) {
                if (ir == stmt) { break; }
                else if (ir == def_of_t) { return CK_UNKNOWN; }
            }
        }
    }

    if (exp->is_any() || stmt->is_any()) {
        return CK_OVERLAP;
    }

    UINT exptysz = exp->getTypeSize(m_tm);
    UINT stmttysz = stmt->getTypeSize(m_tm);
    if ((((ILD_ofst(exp) + exptysz) <= IST_ofst(stmt)) ||
        ((IST_ofst(stmt) + stmttysz) <= ILD_ofst(exp)))) {
        return CK_NOT_OVERLAP;
    }
    return CK_OVERLAP;
}


//Find nearest killing def to expmd in its bb.
//Here we search exactly killing DEF from current stmt to previous
//for expmd even if it is exact,
//has_overlapped_def: record if find local non-killing def(overlapped).
//
//e.g: g is global variable, it is exact.
//x is a pointer that we do not know where it pointed to.
//    1. *x += 1;
//    2. g = 0;
//    3. *x += 2; # *x may overlapped with global variable g.
//    4. return g;
//In the case, the last reference of g in stmt 4 may be defined by
//stmt 2 or 3.
IR const* DUMgr::findKillingLocalDef(IRBB * bb, xcom::C<IR*> const* ct,
                                     IR const* exp, MD const* expmd,
                                     bool * has_local_nonkilling_def)
{
    ASSERTN(expmd->is_exact() || exp->isReadPR(),
            ("only exact md or PR has killing-def"));
    ASSERT0(has_local_nonkilling_def);

    IRListIter localct = const_cast<C<IR*>*>(ct);
    for (IR * ir = BB_irlist(bb).get_prev(&localct);
         ir != nullptr; ir = BB_irlist(bb).get_prev(&localct)) {
        if (!ir->isMemoryRef()) { continue; }

        ASSERTN(!ir->isCallStmt(),
                ("call should not appear in local processing"));

        MD const* defmd = ir->getRefMD();
        if (defmd != nullptr) {
            if (defmd == expmd || defmd->is_overlap(expmd)) {
                if (checkIsLocalKillingDefForDirectAccess(defmd,
                        expmd, ir, has_local_nonkilling_def)) {
                    return ir;
                }

                if (*has_local_nonkilling_def) {
                    //Find an overlapped DEF that is
                    //non-killing current expression.
                    //Thereby, tell caller that we can not get killing def.
                    return nullptr;
                }

                UINT result = checkIsLocalKillingDefForIndirectAccess(
                    ir, exp, ct);
                if (result == CK_OVERLAP) {
                    return ir;
                } else if (result == CK_NOT_OVERLAP) {
                    continue;
                }

                //ir is neither inexact, nor the nonkilling def of exp
                //in current BB.
                return nullptr;
            }

            //If both def and exp has must reference, we do not need to
            //check maydef.
            continue;
        }

        //We need to check maydef.
        MDSet const* maydefs = ir->getRefMDSet();
        if (maydefs != nullptr && maydefs->is_contain(expmd)) {
            //There is a nonkilling DEF, ir may modified expmd.
            //The subsequent searching is meaningless.
            //We can not find a local killing DEF.
            return nullptr;
        }
    }

    //We can not find a local killing DEF.
    return nullptr;
}


//Build DU chain of local non-killing def for exact MD within current BB.
void DUMgr::buildLocalDUChainForNonKillingDef(IRBB * bb, xcom::C<IR*> const* ct,
                                              IR const* exp, MD const* expmd,
                                              DUSet * expdu)
{
    ASSERT0(expmd && exp && expdu && ct && bb);
    ASSERTN(expmd->is_exact() || exp->isReadPR(),
            ("only exact md or PR has killing-def"));

    IR const* nearest_def = nullptr;
    IRListIter localct = const_cast<C<IR*>*>(ct);
    for (IR * ir = BB_irlist(bb).get_prev(&localct);
         ir != nullptr; ir = BB_irlist(bb).get_prev(&localct)) {
        if (!ir->isMemoryRef()) { continue; }
        ASSERTN(!ir->isCallStmt() || !((CCall*)ir)->isMustBBbound(),
                ("call should not appear in local processing"));

        MD const* defmd = ir->getRefMD();
        ASSERTN(defmd != expmd || ir != nearest_def,
                ("should be processed at findKillingDef"));
        if (defmd != nullptr &&
            (defmd == expmd || defmd->is_overlap(expmd))) {
            if (nearest_def == nullptr) {
                //Record the nearest DEF to verify DU processing.
                nearest_def = ir;
            }
            //Stmt is exact, but that is not killing def of exp.
            ASSERT0(expdu);
            expdu->addDef(ir, *getSBSMgr());

            DUSet * xdu = ir->getDUSet();
            ASSERT0(xdu);
            cleanDUSet(ir->id(), xdu);
            xdu->addUse(exp, *getSBSMgr());
        }
    }
}


//Build DU chain for exp(expmd) with local stmt.
//This function first find local killing def stmt, if not exist,
//non-killing but local def stmt will be scanned and processed.
//Return true if find local killing def, otherwise means
//there is not local killing def.
//has_local_nonkilling_def: record the analysis result. True if
//    find non-killing DEF stmt in current 'bb'
//    e.g: s.a = 20; //s.a is non-killing DEF of s.
//         ... = s;
bool DUMgr::buildLocalDUChain(IRBB * bb, IR const* exp, MD const* expmd,
                              DUSet * expdu, IRListIter ct,
                              bool * has_local_nonkilling_def)
{
    ASSERT0(has_local_nonkilling_def);
    IR const* nearest_def = findKillingLocalDef(bb, ct, exp, expmd,
        has_local_nonkilling_def);
    if (nearest_def != nullptr) {
        //Found local killing-def, then build exact DefUse chain.
        ASSERT0(expdu);
        expdu->addDef(nearest_def, *getSBSMgr());

        DUSet * xdu = nearest_def->getDUSet();
        ASSERT0(xdu);
        cleanDUSet(nearest_def->id(), xdu);
        xdu->addUse(exp, *getSBSMgr());
        return true;
    }
    return false;
}


//Check memory operand and build DU chain for them.
//Note we always find the nearest exact def, and build
//the DU between the def and its use.
void DUMgr::checkAndBuildChainRecursiveIRList(IRBB * bb, IR * exp,
                                              IRListIter ct, UINT flag)
{
    for (IR * e = exp; e != nullptr; e = e->get_next()) {
        checkAndBuildChainRecursive(bb, e, ct, flag);
    }
}


//Check memory operand and build DU chain for them.
//Note we always find the nearest exact def, and build
//the DU between the def and its use.
void DUMgr::checkAndBuildChainRecursive(IRBB * bb, IR * exp, IRListIter ct,
                                        UINT flag)
{
    ASSERT0(exp && exp->is_exp());
    switch (exp->getCode()) {
    case IR_LD:
        if (!HAVE_FLAG(flag, DUOPT_COMPUTE_NONPR_DU)) { return; }
        break;
    case IR_PR:
        if (!HAVE_FLAG(flag, DUOPT_COMPUTE_PR_DU)) { return; }
        break;
    case IR_ILD:
    case IR_ARRAY:
    SWITCH_CASE_BIN:
    SWITCH_CASE_UNA:
    case IR_SELECT:
    case IR_CASE:
        for (UINT i = 0; i < IR_MAX_KID_NUM(exp); i++) {
            IR * kid = exp->getKid(i);
            if (kid == nullptr) { continue; }
            checkAndBuildChainRecursiveIRList(bb, kid, ct, flag);
        }
        if (!HAVE_FLAG(flag, DUOPT_COMPUTE_NONPR_DU)) { return; }
        break;
    case IR_CONST:
    case IR_LDA:
    case IR_ID:
        return;
    default: UNREACHABLE();
    }
    if (exp->isContainMemRef()) {
        checkAndBuildChainForMemIR(bb, exp, ct);
    }
}


//Check memory operand and build DU chain for them.
//Note we always find the nearest exact def, and build
//the DU between the def and its use.
void DUMgr::checkAndBuildChainForMemIR(IRBB * bb, IR * exp, IRListIter ct)
{
    ASSERT0(exp && exp->is_exp());

    DUSet * expdu = genDUSet(exp);
    cleanDUSet(exp->id(), expdu);

    MD const* expmd = exp->getMustRef();
    bool has_local_killing_def = false;
    bool has_local_nonkilling_def = false;
    if ((expmd != nullptr && expmd->is_exact()) || exp->isReadPR()) {
        //Only must-exact USE-ref has qualification to compute killing-def.
        has_local_killing_def = buildLocalDUChain(bb, exp, expmd, expdu, ct,
                                                  &has_local_nonkilling_def);
    }

    if (has_local_killing_def) {
        //Find killing local def.
        return;
    }

    if (expmd != nullptr) {
        //expmd might be either exact or inexact.
        //Find non-local/local DEF for exp/expmd.
        checkMustMDAndBuildDUChainForPotentialDefList(exp, expmd, expdu);
        if (has_local_nonkilling_def) {
            //Supplement DU chains for all stmt that overlapped-def exp.
            buildLocalDUChainForNonKillingDef(bb, ct, exp, expmd, expdu);
        }
    }

    if (expmd == nullptr || m_md2irs->hasIneffectDef() ||
        has_local_nonkilling_def) {
        //The following cases requires to scan MayUse:
        //If there is not MustUse of 'exp', then it is necessary to scan MayUse
        //to keep the correctness of DU chain.
        //If has_local_nonkilling_def is true, for the sanity of DU chain,
        //MayUse of 'exp' should to be scanned.
        MDSet const* expmds = exp->getMayRef();
        if (expmds != nullptr) {
            checkMDSetAndBuildDUChain(exp, expmd, *expmds, expdu);
        }
    }
}


//Check memory operand and build DU chain for them.
//Note we always find the nearest exact def, and build
//the DU between the def and its use.
void DUMgr::checkAndBuildChain(IR * stmt, IRListIter ct, UINT flag)
{
    ASSERT0(stmt->is_stmt());
    switch (stmt->getCode()) {
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
        if (HAVE_FLAG(flag, DUOPT_COMPUTE_NONPR_DU)) {
            DUSet * du = genDUSet(stmt);
            cleanDUSet(stmt->id(), du);
        }
        for (UINT i = 0; i < IR_MAX_KID_NUM(stmt); i++) {
            IR * kid = stmt->getKid(i);
            if (kid == nullptr) { continue; }
            checkAndBuildChainRecursiveIRList(stmt->getBB(), kid, ct, flag);
        }
        return;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_ICALL:
    case IR_CALL:
    case IR_PHI:
        if (HAVE_FLAG(flag, DUOPT_COMPUTE_PR_DU)) {
            DUSet * du = genDUSet(stmt);
            cleanDUSet(stmt->id(), du);
        }
        for (UINT i = 0; i < IR_MAX_KID_NUM(stmt); i++) {
            IR * kid = stmt->getKid(i);
            if (kid == nullptr) { continue; }
            checkAndBuildChainRecursiveIRList(stmt->getBB(), kid, ct, flag);
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
            if (kid == nullptr) { continue; }
            checkAndBuildChainRecursive(stmt->getBB(), kid, ct, flag);
        }
        return;
    default: ASSERTN(0, ("unsupport"));
    }
}


//Check if stmt killing define exp, stmt and exp may be at same bb.
//e.g: BB is loop body,
//    start:
//    = *t
//    ...
//    *t = ...
//    goto start
UINT DUMgr::checkIsNonLocalKillingDef(IR const* stmt, IR const* exp)
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

    if (t->getCode() != t2->getCode()) { return CK_UNKNOWN; }

    //Note, t could not be modified in the path between stmt and exp.
    //e.g:
    //  *t = ...
    //  ... //t can not be defined.
    //  = *t
    DefDBitSetCore const* lived_in_expr = m_solve_set.getAvailInExpr(
        exp->getStmt()->getBB()->id());
    if (lived_in_expr == nullptr || !lived_in_expr->is_contain(t2->id())) {
        return CK_UNKNOWN;
    }

    UINT exptysz = exp->getTypeSize(m_tm);
    UINT stmttysz = stmt->getTypeSize(m_tm);
    if ((((ILD_ofst(exp) + exptysz) <= IST_ofst(stmt)) ||
        ((IST_ofst(stmt) + stmttysz) <= ILD_ofst(exp)))) {
        return CK_NOT_OVERLAP;
    }
    return CK_OVERLAP;
}


void DUMgr::checkDefSetToBuildDUChainNonPR(IR const* exp, MD const* expmd,
                                           MDSet const* expmds,
                                           DUSet * expdu,
                                           DefSBitSetCore const* defset,
                                           IRBB * curbb)
{
    DefSBitSetIter sc = nullptr;
    for (INT d = defset->get_first(&sc); d >= 0; d = defset->get_next(d, &sc)) {
        IR * def = m_rg->getIR(d);
        ASSERT0(def->is_stmt());
        bool build_du = false;
        MD const* mustdef = def->getMustRef();
        bool consider_maydef = false;
        if (expmd != nullptr && mustdef != nullptr) {
            //If def has MustDef (exact|effect) MD, then we do
            //not consider MayDef MDSet if def is neither CallStmt
            //nor Region.
            ASSERTN(!mustdef->is_may(), ("MayMD can not be mustdef."));
            if (expmd == mustdef || expmd->is_overlap(mustdef)) {
                if (mustdef->is_exact()) {
                    build_du = true;
                } else if (def->getBB() == curbb) {
                    //If stmt is at same bb with exp, then
                    //we can not determine whether they are independent,
                    //because if they are, the situation should be processed
                    //in buildLocalDUChain().
                    //Build DU chain for conservative purpose.
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
            MDSet const* maydef = def->getMayRef();
            if (maydef != nullptr &&
                ((maydef == expmds ||
                  (expmds != nullptr && maydef->is_intersect(*expmds))) ||
                 (expmd != nullptr && maydef->is_overlap(expmd, m_rg)))) {
                //Nonkilling Def.
                build_du = true;
            } else if (mustdef != nullptr && expmds != nullptr &&
                       expmds->is_overlap(mustdef, m_rg)) {
                //Killing Def if mustdef is exact, or else is nonkilling def.
                build_du = true;
            }
        }

        if (build_du) {
            expdu->add(d, *getSBSMgr());
            DUSet * def_useset = genDUSet(def);
            cleanDUSet(d, def_useset);
            def_useset->add(IR_id(exp), *getSBSMgr());
        }
    }
}


void DUMgr::checkDefSetToBuildDUChain(IR const* exp, MD const* expmd,
                                      MDSet const* expmds, DUSet * expdu,
                                      DefSBitSetCore const* defset,
                                      IRBB * curbb)
{
    DefSBitSetIter sc = nullptr;
    if (exp->isReadPR()) {
        //Check DU for PR.
        ASSERT0(expmd);
        UINT const expid = IR_id(exp);
        for (INT d = defset->get_first(&sc);
             d >= 0; d = defset->get_next(d, &sc)) {
            IR * def = m_rg->getIR(d);
            ASSERT0(def->is_stmt());

            MD const* mustdef = def->getMustRef();
            if (expmd != mustdef) { continue; }

            //Build DU chain.
            expdu->add(d, *getSBSMgr());
            DUSet * def_useset = genDUSet(def);
            cleanDUSet(d, def_useset);
            def_useset->add(expid, *getSBSMgr());
        }
        return;
    }

    //Check DefUse for other kind memory reference.
    checkDefSetToBuildDUChainNonPR(exp, expmd, expmds, expdu, defset, curbb);
}


//Check and build DU chain to IR Expression accroding to MustUse MD.
void DUMgr::checkMustMDAndBuildDUChainForPotentialDefList(IR const* exp,
                                                          MD const* expmd,
                                                          DUSet * expdu)
{
    ASSERT0(exp && expmd && expdu);
    ASSERT0(expmd == const_cast<IR*>(exp)->getMustRef());
    //Get a list of stmts that contained all potential MayDef of expmd.
    DefSBitSetCore const* defset = m_md2irs->get(expmd->id());
    if (defset == nullptr) { return; }
    IRBB * curbb = exp->getStmt()->getBB();
    checkDefSetToBuildDUChain(exp, expmd, nullptr, expdu, defset, curbb);
    return;
}


//Check and build DU chain to IR Expression accroding to MDSet.
void DUMgr::checkMDSetAndBuildDUChain(IR const* exp, MD const* expmd,
                                      MDSet const& expmds, DUSet * expdu)
{
    ASSERT0(expdu);
    IRBB * curbb = exp->getStmt()->getBB();
    MDSetIter iter;
    for (INT u = expmds.get_first(&iter);
         u >= 0; u = expmds.get_next(u, &iter)) {
        DefSBitSetCore const* defset = m_md2irs->get(u);
        if (defset == nullptr) { continue; }
        checkDefSetToBuildDUChain(exp, expmd, &expmds, expdu, defset, curbb);
    }
}


void DUMgr::updateDefWithMustExactMD(IR * ir, MD const* mustexact)
{
    ASSERT0(mustexact && mustexact->is_exact());

    //Handle exactly killing def. This def kills
    //prior DEF stmt to exact md.
    m_md2irs->set(mustexact->id(), ir);
    updateDefSetAccordingToMayRef(ir, mustexact);
}


void DUMgr::updateDefWithMustEffectMD(IR * ir, MD const* musteffect)
{
    ASSERT0(musteffect == nullptr ||
            (musteffect->is_effect() && !musteffect->is_exact()));

    //Handle inexactly nonkilling DEF.
    //And alloc overlapped MDSet for DEF as maydef.
    if (musteffect != nullptr) {
        ASSERT0(musteffect->is_effect());
        m_md2irs->append(musteffect, ir);
    } else {
        m_md2irs->setIneffectDef();
    }

    MDSet const* maydef = ir->getMayRef();
    if (maydef == nullptr) { return; }

    //ir might kill stmts which has overlapped MDSet with md.
    if (musteffect != nullptr) {
        UINT mustid = musteffect->id();
        MDSetIter iter = nullptr;
        for (INT i = maydef->get_first(&iter);
             i >= 0; i = maydef->get_next(i, &iter)) {
            if (mustid == (UINT)i) {
                //Already add to md2irs.
                continue;
            }

            ASSERT0(m_md_sys->getMD(i));
            m_md2irs->append(i, ir);
        }
        return;
    }

    MDSetIter iter = nullptr;
    for (INT i = maydef->get_first(&iter);
         i >= 0; i = maydef->get_next(i, &iter)) {
        ASSERT0(m_md_sys->getMD(i));
        m_md2irs->append(i, ir);
    }
}


void DUMgr::cleanDUSet(UINT irid, DUSet * set)
{
    ASSERTN(m_is_init, ("it is allocated at each perform()"));
    ASSERT0(irid != IRID_UNDEF);
    if (!m_is_init->find(irid)) {
        m_is_init->append(irid);
        if (set != nullptr) {
            set->clean(*getSBSMgr());
        }
    }
}


void DUMgr::updateDefSetAccordingToMayRef(IR * ir, MD const* mustexact)
{
    ASSERT0(mustexact && mustexact->is_exact());
    //Pick off stmt from md's DEF set.
    //e.g: Assume the MD of struct {int a;} s, is MD13, and s.a is MD4,
    //then MD4 and MD13 are overlapped.
    //    MD13 has overlapped-DEF in set:
    //        s.a = 10;
    //When we meet exact definition to MD4, it kill all lived
    //stmts that exact defined MD4, also include the stmt in MD13's DEF set.
    MDSet const* maydef = ir->getMayRef();
    if (maydef == nullptr) { return; }

    MDSetIter iter = nullptr;
    for (INT i = maydef->get_first(&iter);
         i >= 0; i = maydef->get_next(i, &iter)) {
        if (mustexact->id() == (UINT)i) {
            //Already add to md2irs.
            continue;
        }

        //Iterate stmt in def-list of MD in maydef-set.
        //Check if current stmt 'ir' killed stmt in def-list of
        //referred md.
        DefSBitSetCore * dlst = m_md2irs->get(i);
        if (dlst != nullptr) {
            DefSBitSetIter sc = nullptr;
            INT nk;
            for (INT k = dlst->get_first(&sc); k >= 0; k = nk) {
                nk = dlst->get_next(k, &sc);
                ASSERT0(m_rg->getIR(k) && m_rg->getIR(k)->is_stmt());
                MD const* w = m_rg->getIR(k)->getExactRef();
                if (mustexact == w ||
                    (w != nullptr && mustexact->is_exact_cover(w))) {
                    //Current ir stmt killed stmt k as well.
                    dlst->diff(k, *getSBSMgr());
                }
            }
        }

        ASSERT0(m_md_sys->getMD(i));
        if (!m_md_sys->getMD(i)->is_exact()) {
            //Add ir to def-list of inexact MD because
            //we can not determine whether ir has killed other stmts in
            //def-list. ir is non-killing def related to ith MD.
            m_md2irs->append(i, ir);
            continue;
        }
    }
}


void DUMgr::updateDef(IR * ir, UINT flag)
{
    switch (ir->getCode()) {
    case IR_ST:
    case IR_IST:
    case IR_STARRAY:
        if (!HAVE_FLAG(flag, DUOPT_COMPUTE_NONPR_DU)) { return; }
        cleanDUSet(ir->id(), ir->getDUSet());
        break;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_PHI:
        if (!HAVE_FLAG(flag, DUOPT_COMPUTE_PR_DU)) { return; }
        cleanDUSet(ir->id(), ir->getDUSet());
        break;
    case IR_CALL:
    case IR_ICALL:
        cleanDUSet(ir->id(), ir->getDUSet());
        break;
    default: if (!ir->hasResult()) { return; }
    }

    ASSERT0(ir->is_st() || ir->is_ist() || ir->is_starray() ||
            ir->isWritePR() || ir->isCallStmt());

    if (!HAVE_FLAG(flag, DUOPT_COMPUTE_PR_DU) && ir->isCallStmt()) {
        updateDefWithMustEffectMD(ir, nullptr);
        return;
    }

    MD const* must = ir->getMustRef();
    if (must != nullptr && must->is_exact()) {
        updateDefWithMustExactMD(ir, must);
        return;
    }

    updateDefWithMustEffectMD(ir, must);
}


//Initialize md2maydef_ir_list for given BB according Reach-Def-In.
//NOTE this function is used for classic data-flow analysis.
void DUMgr::initMD2IRSet(IRBB * bb)
{
    DefDBitSetCore * reachdef_in = m_solve_set.getInReachDef(bb->id());
    if (reachdef_in == nullptr) { return; }

    m_md2irs->clean();

    //Record IR STMT that might modify given MD.
    DefSBitSetIter st = nullptr;
    for (INT i = reachdef_in->get_first(&st);
         i != -1; i = reachdef_in->get_next(i, &st)) {
        IR const* stmt = m_rg->getIR(i);
        //stmt may be IR_PHI, IR_REGION, IR_CALL.
        //If stmt is IR_PHI, its maydef is NULL.
        //If stmt is IR_REGION, its mustdef is NULL, but the maydef
        //may not be NULL.
        MD const* mustdef = const_cast<IR*>(stmt)->getMustRef();
        if (mustdef != nullptr) {
            //mustdef may be fake object.
            //ASSERT0(mustdef->is_effect());
            m_md2irs->append(mustdef, i);
        }

        //if (mustdef == nullptr) {
        //    m_md2irs->setIneffectDef();
        //}

        MDSet const* maydef = const_cast<IR*>(stmt)->getMayRef();
        if (maydef == nullptr) { continue; }

        //Create the map between MD and STMT,
        //and record InEffect Definition if any.
        MDSetIter iter = nullptr;
        for (INT j = maydef->get_first(&iter);
             j != -1; j = maydef->get_next(j, &iter)) {
            if (mustdef != nullptr && mustdef->id() == (UINT)j) {
                continue;
            }
            if (!m_md2irs->hasIneffectDef() && xoc::isGlobalSideEffectMD(j)) {
                //If MayDef set contains GlobaSideEffect MD, to keep the
                //correctness of DU chain, we set the InEffect flag to
                //inform the following analysis to establish conservative DU
                //chain.
                m_md2irs->setIneffectDef();
            }
            ASSERT0(m_md_sys->getMD(j) != nullptr);
            m_md2irs->append(j, i);
        }
    }
}


//Compute inexactly DU chain.
//'is_init': record initialized DU.
//NOTICE: The Reach-Definition and MustDef, MayDef,
//May Use must be avaliable.
void DUMgr::computeMDDUforBB(IN IRBB * bb, UINT flag)
{
    initMD2IRSet(bb);
    IRListIter ct = nullptr;
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
    }
}


bool DUMgr::verifyLiveinExp()
{
    BBList * bbl = m_rg->getBBList();
    BBListIter ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        DefSBitSetIter st = nullptr;
        DefDBitSetCore * x = m_solve_set.getAvailInExpr(bb->id());
        if (x == nullptr) { continue; }
        for (INT i = x->get_first(&st); i >= 0; i = x->get_next(i, &st)) {
            ASSERT0(m_rg->getIR(i) && m_rg->getIR(i)->is_exp());
        }
    }
    return true;
}


#ifdef _DEBUG_
static bool checkIsTruelyDep(IR const* def, IR const* use, Region const* rg,
                             MDSystem const* ms)
{
    MD const* mustdef = def->getRefMD();
    MDSet const* maydef = def->getRefMDSet();
    MD const* mustuse = use->getRefMD();
    MDSet const* mayuse = use->getRefMDSet();
    if (mustdef != nullptr) {
        if (mustuse != nullptr) {
            if (def->isCallStmt()) {
                //CALL, ICALL may have sideeffect, thus processing
                //them individually.
                if (mustdef == mustuse || mustdef->is_overlap(mustuse)) {
                    return true;
                }

                if (maydef != nullptr && maydef->is_overlap(mustuse, rg)) {
                    return true;
                }

                if (maydef != nullptr &&
                    mayuse != nullptr &&
                    mayuse->is_intersect(*maydef)) {
                    return true;
                }

                UNREACHABLE();
                return false;
            }

            ASSERT0(mustdef == mustuse || mustdef->is_overlap(mustuse));
            return true;
        }

        if (mayuse != nullptr) {
            if (def->isCallStmt()) {
                ASSERT0(mayuse->is_overlap_ex(mustdef, rg, ms) ||
                        maydef == mayuse ||
                        (maydef != nullptr && mayuse->is_intersect(*maydef)));
                return true;
            }

            ASSERT0(mayuse->is_overlap_ex(mustdef, rg, ms));
            return true;
        }

        ASSERTN(0, ("Not a truely dependence"));
        return false;
    }

    if (maydef != nullptr) {
        if (mustuse != nullptr) {
            ASSERT0(maydef->is_overlap_ex(mustuse, rg, ms));
            return true;
        }
        if (mayuse != nullptr) {
            ASSERT0(mayuse == maydef || mayuse->is_intersect(*maydef));
            return true;
        }
        ASSERTN(0, ("Not a truely dependence"));
        return false;
    }

    ASSERTN(0, ("Not a truely dependence"));
    return false;
}
#endif


//Verify if DU chain is correct between each Def and Use of MD.
bool DUMgr::verifyMDDUChainForIR(IR const* ir, UINT duflag)
{
    bool precision_check = g_verify_level >= VERIFY_LEVEL_2;
    ASSERT0(ir->is_stmt());
    //Check stmt's UseSet.
    if ((HAVE_FLAG(duflag, DUOPT_COMPUTE_PR_DU) &&
         (ir->isWritePR() || ir->isCallStmt())) ||
        (HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU) &&
         ir->isMemoryRef())) {
        //Also check memory DU for call stmt.
        //The DUSet of call-stmt is a speical case because the DUSet of call
        //not only record the DefSet but also the UseSet.
        DUSet const* useset = ir->readDUSet();
        if (useset != nullptr) {
            DUSetIter di = nullptr;
            for (INT i = useset->get_first(&di);
                 i >= 0; i = useset->get_next(i, &di)) {
                IR const* use = m_rg->getIR(i);
                DUMMYUSE(use);
                ASSERT0(use->is_exp());
                if (use->is_id()) {
                    //ID is often used as a placeholder of Var or MD, it is
                    //not represent a real occurrence of memory. e.g, MDSSA
                    //use ID as operand of MDPhi.
                    continue;
                }

                //Check the existence of 'use'.
                ASSERT0(use->getStmt() && use->getStmt()->getBB());
                ASSERT0(BB_irlist(use->getStmt()->getBB()).find(
                                  use->getStmt()));

                //use must be a memory operation.
                ASSERT0(use->isMemoryOpnd());

                //ir must be DEF of 'use'.
                ASSERT0(use->readDUSet());

                //Check consistence between ir and use duchain.
                ASSERT0(use->readDUSet()->is_contain(ir->id()));

                if (precision_check) {
                    ASSERT0(checkIsTruelyDep(ir, use, m_rg, m_md_sys));
                }
            }
        }
    }

    m_citer.clean();
    for (IR const* u = iterRhsInitC(ir, m_citer);
         u != nullptr; u = iterRhsNextC(m_citer)) {
        ASSERT0(!ir->is_lhs(u) && u->is_exp());
        if (!u->isReadPR() && !u->isMemoryRef()) { continue; }
        if ((!HAVE_FLAG(duflag, DUOPT_COMPUTE_PR_DU) && u->isReadPR()) ||
            (!HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU) && u->isMemoryRef())) {
            continue;
        }

        DUSet const* defset = u->readDUSet();
        if (defset == nullptr) { continue; }

        ASSERTN(u->isMemoryOpnd(), ("only memory operand has DUSet"));

        DUSetIter di = nullptr;
        for (INT i = defset->get_first(&di);
             i >= 0; i = defset->get_next(i, &di)) {
            IR const* def = m_rg->getIR(i);
            CHECK0_DUMMYUSE(def);
            ASSERT0(def->is_stmt());

            //Check the existence to 'def'.
            ASSERT0(def->getBB());
            ASSERT0(BB_irlist(def->getBB()).find(const_cast<IR*>(def)));

            //u must be use of 'def'.
            ASSERT0(def->readDUSet());

            //Check consistence between DEF and USE.
            ASSERT0(def->readDUSet()->is_contain(IR_id(u)));

            if (precision_check) {
                ASSERT0(checkIsTruelyDep(def, u, m_rg, m_md_sys));
            }
        }
    }
    return true;
}


//Verify DU chain's sanity.
bool verifyMDDUChain(Region * rg, UINT duflag)
{
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr == nullptr) { return true; }
    if (!HAVE_FLAG(duflag, DUOPT_COMPUTE_PR_DU) &&
        !HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU)) {
        return true;
    }
    BBList * bbl = rg->getBBList();
    for (IRBB * bb = bbl->get_head();
         bb != nullptr; bb = bbl->get_next()) {
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            dumgr->verifyMDDUChainForIR(ir, duflag);
        }
    }
    return true;
}


//Find the nearest dominated DEF stmt of 'exp'.
//NOTE: RPO of bb of stmt must be available.
//'exp': expression
//'exp_stmt': stmt that exp is belong to.
//'expdu': def set of exp.
//'omit_self': true if we do not consider the 'exp_stmt' itself.
IR * DUMgr::findNearestDomDef(IR const* exp, IR const* exp_stmt,
                              DUSet const* expdefset)
{
    ASSERT0(exp->is_exp() && exp_stmt->is_stmt());
    ASSERT0(const_cast<IR*>(exp)->getMayRef() ||
            const_cast<IR*>(exp)->getMustRef());
    IR * last = nullptr;
    INT stmt_rpo = exp_stmt->getBB()->rpo();
    ASSERT0(stmt_rpo != RPO_UNDEF);
    INT lastrpo = RPO_UNDEF;
    DUSetIter di = nullptr;
    for (INT i = expdefset->get_first(&di);
         i >= 0; i = expdefset->get_next(i, &di)) {
        IR * d = m_rg->getIR(i);
        ASSERT0(d->is_stmt());
        if (!isMayDef(d, exp, false)) {
            continue;
        }

        if (d == exp_stmt) {
            continue;
        }

        if (last == nullptr) {
            last = d;
            lastrpo = d->getBB()->rpo();
            ASSERT0(lastrpo != RPO_UNDEF);
            continue;
        }

        IRBB * dbb = d->getBB();
        ASSERT0(dbb);
        ASSERT0(dbb->rpo() != RPO_UNDEF);
        if (dbb->rpo() < stmt_rpo && dbb->rpo() > lastrpo) {
            last = d;
            lastrpo = dbb->rpo();
            continue;
        }
        if (dbb == last->getBB() && dbb->is_dom(last, d, true)) {
            last = d;
            lastrpo = dbb->rpo();
        }
    }
    if (last == nullptr) { return nullptr; }

    IRBB const* last_bb = last->getBB();
    IRBB const* exp_bb = exp_stmt->getBB();
    if (exp_bb == last_bb) {
        if (!exp_bb->is_dom(last, exp_stmt, true)) {
            //e.g: *p = *p + 1
            //Def and Use in same stmt, in this situation,
            //the stmt can not be regarded as dom-def.
            return nullptr;
        }
        return last;
    }
    if (!m_cfg->is_dom(last_bb->id(), exp_bb->id())) {
        return nullptr;
    }
    ASSERT0(last != exp_stmt);
    return last;
}


size_t DUMgr::count_mem_local_data(DefDBitSetCore * expr_univers,
                                   Vector<MDSet*> * maydef_mds,
                                   Vector<MDSet*> * mustexactdef_mds,
                                   MDSet * mayuse_mds,
                                   MDSet mds_arr_for_must[],
                                   MDSet mds_arr_for_may[],
                                   UINT elemnum)
{
    size_t count = 0;
    if (expr_univers != nullptr) {
        count += expr_univers->count_mem();
    }

    if (mustexactdef_mds != nullptr) {
        ASSERT0(mds_arr_for_must);
        count += mustexactdef_mds->count_mem();
        for (UINT i = 0; i < elemnum; i++) {
            count += mds_arr_for_must[i].count_mem();
        }
    }

    if (maydef_mds != nullptr) {
        ASSERT0(mds_arr_for_may);
        count += maydef_mds->count_mem();
        for (UINT i = 0; i < elemnum; i++) {
            count += mds_arr_for_may[i].count_mem();
        }
    }

    if (mayuse_mds != nullptr) {
        count += mayuse_mds->count_mem();
    }

    return count;
}


//Return true if stmt dominate use's stmt, otherwise return false.
bool DUMgr::isStmtDomUseInsideLoop(IR const* stmt, IR const* use,
                                   LI<IRBB> const* li) const
{
    IRBB const* usestmtbb = nullptr;
    ASSERT0(use->getStmt());
    usestmtbb = use->getStmt()->getBB();
    ASSERT0(usestmtbb);

    if (!li->isInsideLoop(usestmtbb->id())) {
        //Only check dominiation info inside loop.
        return true;
    }

    IRBB const* defstmtbb = stmt->getBB();
    ASSERT0(defstmtbb);
    if (defstmtbb != usestmtbb &&
        m_cfg->is_dom(defstmtbb->id(), usestmtbb->id())) {
        return true;
    }
    if (defstmtbb == usestmtbb) {
        return defstmtbb->is_dom(stmt, use->getStmt(), true);
    }
    return false;
}


//Return true if ir dominates all its USE expressions which inside loop.
bool DUMgr::isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const
{
    DUSet const* useset = ir->readDUSet();
    if (useset != nullptr) {
        DUSetIter di = nullptr;
        for (INT i = useset->get_first(&di);
             i >= 0; i = useset->get_next(i, &di)) {
            IR const* u = m_rg->getIR(i);
            ASSERT0(u->is_exp() && u->getStmt());
            if (!isStmtDomUseInsideLoop(ir, u, li)) {
                return false;
            }
        }
    }
    return true;
}


//Construct inexactly Du, Ud chain.
//NOTICE: Reach-Definition and MustDef, MayDef,
//May-Use must be avaliable.
//retain_reach_def: true to reserve reach-def stmt set.
void DUMgr::computeMDDUChain(MOD OptCtx & oc, bool retain_reach_def,
                             UINT duflag)
{
    ASSERTN(HAVE_FLAG(duflag, DUOPT_COMPUTE_PR_DU) ||
            HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU), ("at least one DU"));
    if (m_rg->getBBList()->get_elem_count() == 0) { return; }

    START_TIMER(t, "Build DU chain");
    ASSERT0(oc.is_cfg_valid());
    ASSERT0(oc.is_ref_valid() && oc.is_reach_def_valid());
    m_oc = &oc;
    BBList * bbl = m_rg->getBBList();
    if (bbl->get_elem_count() > g_thres_opt_bb_num) {
        //There are too many BB. Leave it here.
        END_TIMER(t, "Build DU chain");
        //Only reach-def-in is useful for computing DU chain.
        m_solve_set.resetGlobalSet();
        OC_is_reach_def_valid(oc) = false;
        return;
    }

    //Record IRs which may defined these referred MDs.
    ASSERT0(m_md2irs == nullptr && m_is_init == nullptr);
    m_md2irs = new MD2IRSet(m_rg);
    m_is_init = new xcom::TTab<UINT>();

    //Compute the DU chain linearly.
    BBListIter ct;
    for (IRBB * bb = bbl->get_tail(&ct);
         bb != nullptr; bb = bbl->get_prev(&ct)) {
        computeMDDUforBB(bb, duflag);
    }
    delete m_md2irs;
    m_md2irs = nullptr;
    delete m_is_init;
    m_is_init = nullptr;

    if (!retain_reach_def) {
        //Reach def info will be cleaned.
        //Only reach-def-in is useful for computing DU chain.
        m_solve_set.resetGlobalSet();
        OC_is_reach_def_valid(oc) = false;
    }
    if (HAVE_FLAG(duflag, DUOPT_COMPUTE_PR_DU)) {
        OC_is_pr_du_chain_valid(oc) = true;
    }
    if (HAVE_FLAG(duflag, DUOPT_COMPUTE_NONPR_DU)) {
        OC_is_nonpr_du_chain_valid(oc) = true;
    }
    END_TIMER(t, "Build DU chain");
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpDUMgr()) {
        START_TIMER_FMT(t, ("DUMP DU Chain"));
        note(getRegion(), "\n==---- DUMP %s '%s' ----==", getPassName(),
             m_rg->getRegionName());
        m_md_sys->dump(true);
        dumpDUChainDetail();
        END_TIMER_FMT(t, ("DUMP DU Chain"));
    }
    ASSERTN(verifyMDDUChain(m_rg, duflag), ("verifyMDDUChain failed"));
}


void DUMgr::computeOverlapSetForWorstCase()
{
    ASSERT0(m_aa);
    MDSet const* worst = m_aa->getMayPointToMDSet();
    if (worst == nullptr) { return; }

    Vector<MD const*> tmpvec;
    MDSet tmpmds;
    m_md_sys->computeOverlap(m_rg, *worst, tmpmds, m_tab_iter,
                             *getSBSMgr(), true);
    if (tmpmds.is_empty()) { return; }

    //Register the MDSet.
    tmpmds.bunion(*worst, *getSBSMgr());
    m_aa->setMayPointToMDSet(m_mds_hash->append(tmpmds));
    tmpmds.clean(*getSBSMgr());
}


void DUMgr::solveSet(MOD OptCtx & oc, UINT flag)
{
    m_solve_set.perform(oc, flag);
}


//Compute MD reference, MustRef, MayRef and related IRSet info.
bool DUMgr::perform(MOD OptCtx & oc, UINT flag)
{
    if (flag == 0) { return true; }

    BBList * bbl = m_rg->getBBList();
    if (bbl->get_elem_count() == 0) { return true; }
    if (bbl->get_elem_count() > g_thres_opt_bb_num) {
        //Adjust g_thres_opt_bb_num to make sure you want to do DU analysis.
        interwarn("DUMgr::perform() of Region(%d) is not applied.", m_rg->id());
        return false;
    }

    ASSERT0(oc.is_cfg_valid()); //First, only cfg is needed.

    computeOverlapSetForWorstCase();
    if (HAVE_FLAG(flag, DUOPT_COMPUTE_PR_REF) ||
        HAVE_FLAG(flag, DUOPT_COMPUTE_NONPR_REF)) {
        ASSERT0(oc.is_aa_valid());
        computeMDRef(oc, flag);
    }

    //Initialize local used resource.
    if (HAVE_FLAG(flag, DUOPT_SOL_AVAIL_REACH_DEF) ||
        HAVE_FLAG(flag, DUOPT_SOL_REACH_DEF) ||
        HAVE_FLAG(flag, DUOPT_SOL_REGION_REF) ||
        HAVE_FLAG(flag, DUOPT_SOL_AVAIL_EXPR)) {
        solveSet(oc, flag);
    }

    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpDUMgr()) {
        dump();
    }
    return true;
}
//END DUMgr

} //namespace xoc
