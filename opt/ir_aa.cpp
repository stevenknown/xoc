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
#include "liveness_mgr.h"
#include "prssainfo.h"
#include "ir_ssa.h"

namespace xoc {

//This pass performs a flow sensitive/insensitive (SSA-based) points-to analysis.
//The resulting analysis information is used to promote variables from
//in-memory addressable objects to non-aliased variables PR that can be renamed
//into SSA form.
//
//START PPSetMgr
//
size_t PPSetMgr::count_mem() const
{
    size_t count = 0;
    for (xcom::SC<PtPairSet*> * sc = m_ppset_list.get_head();
         sc != m_ppset_list.end(); sc = m_ppset_list.get_next(sc)) {
        PtPairSet const* pps = sc->val();
        ASSERT0(pps);
        count += pps->count_mem();
    }
    count += m_ppset_list.count_mem();
    return count;
}
//END PPSetMgr


//
//START PtPairMgr
//
size_t PtPairMgr::count_mem() const
{
    size_t count = 0;
    TMapIter<UINT, TMap<UINT, PtPair*>*> ti;
    TMap<UINT, PtPair*> * v = NULL;
    count += m_from_tmap.count_mem();
    for (m_from_tmap.get_first(ti, &v);
         v != NULL; m_from_tmap.get_next(ti, &v)) {
        count += v->count_mem();
    }

    count += m_id2pt_pair.count_mem();
    count += smpoolGetPoolSize(m_pool_pt_pair);
    count += smpoolGetPoolSize(m_pool_tmap);
    count += sizeof(m_pp_count);
    return count;
}


//Add POINT-TO pair: from -> to.
PtPair * PtPairMgr::add(UINT from, UINT to)
{
    TMap<UINT, PtPair*> * to_tmap = m_from_tmap.get(from);
    if (to_tmap == NULL) {
        to_tmap = xmalloc_tmap();
        to_tmap->init();
        m_from_tmap.set(from, to_tmap);
    }

    PtPair * pp = to_tmap->get(to);
    if (pp == NULL) {
        pp = (PtPair*)xmalloc_pt_pair();
        PP_id(pp) = m_pp_count++;
        PP_from(pp) = from;
        PP_to(pp) = to;
        to_tmap->set(to, pp);
        m_id2pt_pair.set(PP_id(pp), pp);
    }
    return pp;
}
//END PtPairMgr


//Return true if all element in mds are effect and derived from the same Var.
//mustref: record the Unbound MD if the function return true, or meaningless.
static bool isAllElementDerivedFromSameEffectVar(MDSet const& mds,
                                                 MDSystem * mdsys,
                                                 MD const** mustref)
{
    ASSERT0(!mds.is_empty() && mustref);

    MDSetIter iter = NULL;
    INT i = mds.get_first(&iter);
    MD const* md = mdsys->getMD((UINT)i);
    if (!md->is_effect() || MD_is_may(md)) {
        return false;
    }

    Var * base = md->get_base();
    i = mds.get_next((UINT)i, &iter);
    for (; i >= 0; i = mds.get_next((UINT)i, &iter)) {
        MD const* md2 = mdsys->getMD((UINT)i);
        if (md2->get_base() != base || !md2->is_effect() || MD_is_may(md2)) {
            return false;
        }

        if (md2->is_unbound()) {
            *mustref = md2;
        }
    }

    if (*mustref == NULL) {
        ASSERT0(base);
        *mustref = mdsys->registerUnboundMD(base, 1);
    }

    return true;
}


//
//START AliasAnalysis
//
AliasAnalysis::AliasAnalysis(Region * rg)
{
    ASSERT0(rg);
    m_cfg = rg->getCFG();
    m_var_mgr = rg->getVarMgr();
    m_rg = rg;
    m_rgmgr = rg->getRegionMgr();
    m_tm = rg->getTypeMgr();
    m_md_sys = rg->getMDSystem();
    m_mds_mgr = rg->getMDSetMgr();
    m_mds_hash = rg->getMDSetHash();
    ASSERT0(m_cfg && m_mds_hash && m_md_sys && m_tm && m_mds_mgr);
    m_flow_sensitive = true;
    m_pool = smpoolCreate(128, MEM_COMM);
    m_dummy_global = NULL;
    m_maypts = NULL;
}


AliasAnalysis::~AliasAnalysis()
{
    OptCtx oc;
    destroyContext(oc);
    smpoolDelete(m_pool);
}


size_t AliasAnalysis::count_mem()
{
    size_t count = 0;
    count += sizeof(m_cfg);
    count += sizeof(m_var_mgr);
    count += sizeof(m_rg);
    count += m_in_pp_set.count_mem();
    count += m_out_pp_set.count_mem();
    count += smpoolGetPoolSize(m_pool);
    count += sizeof(m_mds_mgr);

    count += m_ppmgr.count_mem();

    if (m_maypts != NULL) {
        count += m_maypts->count_mem();
    }

    count += countMD2MDSetMemory();
    count += m_id2heap_md_map.count_mem();
    count += m_unique_md2mds.count_mem();
    count += sizeof(BYTE);
    return count;
}


size_t AliasAnalysis::countMD2MDSetMemory()
{
    size_t count = 0;
    BBList * bbl = m_rg->getBBList();
    MD2MDSetIter iter;
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        MD2MDSet * mx = m_md2mds_vec.get(bb->id());
        if (mx == NULL) { continue; }
        iter.clean();
        MDSet const* mds = NULL;
        for (UINT mdid = mx->get_first(iter, &mds);
             mdid > 0; mdid = mx->get_next(iter, &mds)) {
            if (mds != NULL) {
                count += mds->count_mem();
            }
        }
        count += mx->count_mem();
    }
    count += m_md2mds_vec.count_mem();
    return count;
}


//Destroy all context data structure.
//DU and another info do not need these info.
//If you query sideeffect md or mdset, these data structure must be recomputed.
void AliasAnalysis::destroyContext(OptCtx & oc)
{
    for (INT i = 0; i <= m_md2mds_vec.get_last_idx(); i++) {
        MD2MDSet * mx = m_md2mds_vec.get((UINT)i);
        if (mx == NULL) { continue; }
        mx->destroy();
    }
    OC_is_aa_valid(oc) = false;
}


//Clean but not destory context data structures.
void AliasAnalysis::cleanContext(OptCtx & oc)
{
    for (INT i = 0; i <= m_md2mds_vec.get_last_idx(); i++) {
        MD2MDSet * mx = m_md2mds_vec.get((UINT)i);
        if (mx == NULL) { continue; }
        mx->clean();
    }
    OC_is_aa_valid(oc) = false;
}


//This function do clean and free operation, but do not
//destroy any data structure. Clean PT_SET for each BB.
//Free MDSet of MDA back to MDSetMgr.
void AliasAnalysis::clean()
{
    m_is_visit.clean();
    m_in_pp_set.clean();
    m_out_pp_set.clean();
    m_unique_md2mds.clean();

    OptCtx oc;
    cleanContext(oc);
    m_id2heap_md_map.clean();
}


void AliasAnalysis::cleanSBSMgr()
{
    getSBSMgr()->destroy();
    getSBSMgr()->init();
}


//MD size should be determined by 'base type' of a pointer.
//e.g:
//    char v[]
//    int * p = &v
//    *p = ...
//
//    The memory size that '*p' modified is 4,
//    the size of pointer-base of *p.
//
//'size': pointer base size.
//NOTICE: MD should be taken address.
void AliasAnalysis::reviseMDSize(IN OUT MDSet & mds, UINT size)
{
    ASSERT0(size > 0);
    xcom::Vector<MD const*> res;
    UINT num = 0;
    bool change = false;
    MDSetIter iter = NULL;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        MD * md = m_md_sys->getMD((UINT)i);
        ASSERT0(md && md->is_effect());

        //We need to register a new MD which size is equal to pointer's base.
        //e.g: int a[100], char * p = &a;
        //Even if size of element of a is 4 bytes, the size of p pointed to
        //is only 1 byte.
        if (MD_size(md) != size) {
            MD tmd(*md);
            MD_size(&tmd) = size;
            MD const* entry = m_md_sys->registerMD(tmd);
            ASSERT0(MD_id(entry) > 0);
            ASSERT0(entry->is_effect());
            res.set(num, entry);
            change = true;
        } else {
            res.set(num, md);
        }
        num++;
    }
    if (change) {
        mds.clean(*getSBSMgr());
        for (INT i = ((INT)num) - 1; i >= 0; i--) {
            mds.bunion(res.get((UINT)i), *getSBSMgr());
        }
    }
}


//Return true if IR is a valid statement that could be handled by AA.
//NOTICE: High level control flow or similar statements are unacceptable here.
bool AliasAnalysis::isValidStmtToAA(IR const* ir) const
{
    switch(ir->getCode()) {
    case IR_ST: //store
    case IR_STPR:
    case IR_STARRAY:
    case IR_IST: //indirective store
    case IR_CALL:
    case IR_ICALL: //indirective call
    case IR_GOTO:
    case IR_IGOTO:
    case IR_LABEL: //Permit it in high level IR analysis.
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_SWITCH:
    case IR_RETURN:
    case IR_PHI:
    case IR_REGION:
    case IR_SETELEM:
    case IR_GETELEM:
        return true;
    default:
        return false;
    } //end switch
}


//Process LDA operator, and generate MD.
//Return MD that describe memory address of LDA.
//Note this function does not handle array's LDA base.
//e.g: y = &x or y = &a[i]
//'mds' : record output memory descriptor of 'ir'
MD const* AliasAnalysis::processLda(IR * ir, IN OUT AACtx * ic)
{
    ASSERT0(ir->is_lda() && ir->is_ptr());
    ASSERT0(ic);

    MD const* t = NULL;
    Var * v = LDA_idinfo(ir);
    if (v->is_string()) {
        t = m_rg->allocStringMD(v->get_name());
    } else {
        t = m_rg->genMDforVAR(v);
    }

    if (t->is_exact()) {
        //Adjust size of MD of LDA to be pointer size.
        MD t2(*t);
        MD_size(&t2) = ir->getTypeSize(m_tm);
        MD const* entry = m_md_sys->registerMD(t2);
        ASSERT0(MD_id(entry) > 0);
        t = entry;
    }

    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        AC_is_mds_mod(ic) = true;
    } else {
        AC_is_mds_mod(ic) = false;
    }

    //Inform the caller that there is MD has been taken address.
    AC_has_comp_lda(ic) = true;

    if (!AC_is_mds_mod(ic) && LDA_ofst(ir) == 0) { return t; }

    //Try to reshape MD.
    bool reshape_md = false;
    if (LDA_ofst(ir) != 0 && t->is_exact()) {
        //Apply byte offset of LDA to output MD.
        reshape_md = true;
    }
    if (ir->getParent()->is_array()) {
        //Reshape MD size to array's element type size
        //instead of LDA's data-type size.
        reshape_md = true;
    }
    if (!reshape_md) { return t; }

    //If LDA is the base of array operation, and LDA ofst may not be 0.
    //e.g: struct S { int a; int b[..]; } s;
    //     ... = s.b[i]
    //The IR could be: ARRAY:4 (LDA s:4, dim[..]), (ld i)
    MD md(*t);
    MD_ofst(&md) += LDA_ofst(ir);
    if (ir->getParent()->is_array() &&
        ((CArray*)ir->getParent())->is_base(ir) && //ir is the
                                                   //base of array.
        !ir->getParent()->is_any() &&
        t->is_exact()) {
        //The output data type of LDA should amended to be the type of
        //array element if array is a part/field of D_MC.
        //e.g: struct S { int a; int b[..]; } s;
        //     ... = s.b[i];
        //generate ARRAY(LDA(s, ofst(4))
        UINT elem_sz = ir->getParent()->getTypeSize(m_tm);
        ASSERT0(elem_sz > 0);
        MD_size(&md) = elem_sz;
    }
    if (!t->is_equ(md)) {
        MD const* entry = m_md_sys->registerMD(md);
        ASSERT0(MD_id(entry) > 0);
        ASSERT0(t->is_effect() && entry->is_effect());
        return entry;
    }
    return t;
}


//Convert type-size.
//e.g:int a; char b;
//    a = (int)b
//'mds' : record memory descriptor of 'ir'.
void AliasAnalysis::processCvt(IR const* ir,
                               IN OUT MDSet & mds,
                               OUT AACtx * ic,
                               OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_cvt());
    inferExpression(CVT_exp(ir), mds, ic, mx);
    if (AC_is_mds_mod(ic)) {
        MDSetIter iter;
        UINT size = ir->getTypeSize(m_tm); //cvt's tgt byte size.
        INT next;
        for (INT i = mds.get_first(&iter); i >= 0; i = next) {
            next = mds.get_next((UINT)i, &iter);
            MD const* l = m_md_sys->getMD((UINT)i);
            ASSERT0(l);

            if (l->is_exact() && MD_size(l) != size) {
                MD md(*l);

                //Convert type-size to 'ir' claimed.
                MD_size(&md) = size;
                MD const* entry = m_md_sys->registerMD(md);
                ASSERT0(MD_id(entry) > 0);
                ASSERT0(l->is_effect() && entry->is_effect());

                mds.diff((UINT)i, *getSBSMgr());
                mds.bunion_pure(MD_id(entry), *getSBSMgr());
            }
        }
    }
}


//Infer the unbounded set.
void AliasAnalysis::inferArrayInfinite(INT ofst,
                                       bool is_ofst_pred,
                                       UINT md_size,
                                       MDSet const& in,
                                       OUT MDSet & out)
{
    MDSetIter iter;
    for (INT i = in.get_first(&iter); i >= 0; i = in.get_next((UINT)i, &iter)) {
        MD const* org = m_md_sys->getMD((UINT)i);
        MD tmd(*org);
        if (is_ofst_pred && tmd.is_exact()) {
            MD_ofst(&tmd) += ofst;
            MD_size(&tmd) = md_size;
        } else {
            MD_ty(&tmd) = MD_UNBOUND;
        }

        if (tmd == *org) {
            out.bunion(org, *getSBSMgr());
            continue;
        }

        MD const* entry = m_md_sys->registerMD(tmd);
        ASSERT0(MD_id(entry) > 0);
        out.bunion(entry, *getSBSMgr());
    }
}


//Compute and update point_to_set with TBAA info.
//pointer: IR expression that pointed to memory.
//point_to_set: POINT_TO set of pointer, genernate new set if TBAA exist.
//Return true if pointer pointed to MAY-POINT-TO set.
MDSet const* AliasAnalysis::computeMayPointToViaTBAA(IR const* pointer,
                                                     MDSet const* point_to_set)
{
    ASSERT0(pointer && point_to_set && !point_to_set->is_empty());
    MD const* typed_md = NULL;

    //TBAA tell us where pointer pointed to.
    if (pointer->getAI() != NULL &&
        (typed_md = computePointToViaType(pointer)) != NULL) {
        //Try to resolve POINT-TO MD through TBAA.
        MDSet tmp;
        tmp.bunion(typed_md, *getSBSMgr());
        MDSet const* p = m_mds_hash->append(tmp);
        tmp.clean(*getSBSMgr());
        return p;
    }

    //We have to determine where does 'pointer' expression point to.
    //Note that the POINT-TO set can not be empty, 'pointer' may be CONST/CVT.
    //So lhs may point to anywhere.
    //if (!point_to_set->is_empty()) {
    //    //If POINT-TO set contain GLOBAL NODE, namely the worst case,
    //    //try to shrink POINT-TO set through TBAA.
    //    if (point_to_set->is_contain_global() &&
    //        pointer->getAI() != NULL &&
    //        (typed_md = computePointToViaType(pointer)) != NULL) {
    //        //Make use of typed pointer info to improve the precsion.
    //        point_to_set->clean(*getSBSMgr());
    //        point_to_set->bunion(typed_md, *getSBSMgr());
    //    }
    //    return m_mds_hash->append(*point_to_set);
    //}
    return point_to_set;
}


//Get to know where the pointer pointed to.
//This function will not clean 'mds' since user may perform union operation.
void AliasAnalysis::computeMayPointTo(IR * pointer, OUT MDSet & mds)
{
    ASSERT0(pointer && (pointer->is_ptr() || pointer->is_any()));

    //Get context.
    MD2MDSet * mx = NULL;
    if (m_flow_sensitive) {
        ASSERT0(pointer->getStmt() && pointer->getStmt()->getBB());
        mx = mapBBtoMD2MDSet(pointer->getStmt()->getBB()->id());
    } else  {
        mx = &m_unique_md2mds;
    }
    computeMayPointTo(pointer, mx, mds, *getSBSMgr());
}


//Get to know where the pointer pointed to.
//This function will not clean 'mds' since caller may
//perform union operations to 'mds'.
//mx: may be NULL.
void AliasAnalysis::computeMayPointTo(IR * pointer,
                                      IN MD2MDSet * mx,
                                      OUT MDSet & mds,
                                      DefMiscBitSetMgr & sbsmgr)
{
    ASSERT0(pointer && (pointer->is_ptr() || pointer->is_any()));
    if (pointer->is_lda()) {
        AACtx ic;
        AC_comp_pt(&ic) = true;
        MD const* t = processLda(pointer, &ic);
        ASSERT0(t);
        if (t->is_exact()) {
            //Reshape MD of LDA to be unbound.
            MD t2(*t);
            MD_ty(&t2) = MD_UNBOUND;
            MD const* entry = m_md_sys->registerMD(t2);
            ASSERT0(MD_id(entry) > 0);
            t = entry;
        }
        mds.bunion(t, sbsmgr);
        return;
    }

    if (mx == NULL) {
        //We do NOT known where p pointed to.
        //e.g: (int*)0x1234
        mds.bunion(*m_maypts, sbsmgr);
        return;
    }

    MD const* emd = pointer->getRefMD();
    if (emd != NULL) {
        //pointer has an exact or effect MD.
        MDSet const* ptset = getPointTo(MD_id(emd), *mx);
        MD const* typed_md = NULL;
        if (ptset != NULL && !ptset->is_empty()) {
            if (pointer->getAI() != NULL &&
                ptset->is_contain_global() &&
                (typed_md = computePointToViaType(pointer)) != NULL) {
                mds.bunion(typed_md, sbsmgr);
            } else {
                mds.bunion(*ptset, sbsmgr);
            }
        } else if (pointer->getAI() != NULL &&
                   (typed_md = computePointToViaType(pointer)) != NULL) {
            mds.bunion(typed_md, sbsmgr);
        } else {
            //We do NOT known where p pointed to.
            mds.bunion(*m_maypts, sbsmgr);
        }
    }

    MDSet const* maymds = pointer->getRefMDSet();
    if (maymds != NULL) {
        //pointer has a set of may-referenced-MDs.
        MDSetIter iter;
        for (INT i = maymds->get_first(&iter);
             i >= 0; i = maymds->get_next((UINT)i, &iter)) {
            MDSet const* ptset = getPointTo((UINT)i, *mx);
            MD const* typed_md = NULL;
            if (ptset != NULL && !ptset->is_empty()) {
                if (pointer->getAI() != NULL &&
                    ptset->is_contain_global() &&
                    (typed_md = computePointToViaType(pointer)) != NULL) {
                    mds.bunion(typed_md, sbsmgr);
                } else {
                    mds.bunion(*ptset, sbsmgr);
                }
            } else if (pointer->getAI() != NULL &&
                       (typed_md = computePointToViaType(pointer)) != NULL) {
                mds.bunion(typed_md, sbsmgr);
            } else {
                //We do NOT known where p pointed to.
                mds.bunion(*m_maypts, sbsmgr);
            }
        }
    }

    if (emd == NULL && maymds == NULL) {
        //We do NOT known where p pointed to.
        //e.g: (int*)0x1234
        mds.bunion(*m_maypts, sbsmgr);
    }
}


//The function compute may memory address or point-to set for array operation.
//'ir': array|starray operator.
//'array_base': base of array, must be pointer type.
//'is_ofst_predicable': true if array element offset is constant.
//This function will set the Ref MD and Ref MD set of array operation.
void AliasAnalysis::inferArrayExpBase(IR * ir,
                                      IR * array_base,
                                      bool is_ofst_predicable,
                                      UINT ofst,
                                      OUT MDSet & mds,
                                      OUT bool * mds_is_may_pt,
                                      IN OUT AACtx * ic,
                                      IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->isArrayOp() && (array_base->is_ptr() || array_base->is_any()));
    MDSet tmp;
    AACtx tic;
    tic.copyTopDownFlag(*ic);

    //Compute point-to set at this function scope,
    //and we can do more evaluation.
    AC_comp_pt(&tic) = false;

    inferExpression(array_base, tmp, &tic, mx);

    if (tmp.is_empty()) {
        //We could not determine which MD the array base refer to,
        //so we could not infer which MD the array accessed.
        MD const* typed_md = NULL;
        if (array_base->getAI() != NULL &&
            (typed_md = computePointToViaType(array_base)) != NULL) {
            mds.clean(*getSBSMgr());
            mds.bunion(typed_md, *getSBSMgr());
        } else {
            mds.copy(*m_maypts, *getSBSMgr());
            *mds_is_may_pt = true;
        }
    } else {
        //Intent to look for original array base that the base-pointer
        //pointed to.
        //e.g: (ld(p))[i], looking for where p pointed to.
        //Each MD in 'tmp' should be pointer.
        mds.clean(*getSBSMgr());
        UINT mdsz = ir->getTypeSize(m_tm);
        MDSetIter iter;
        for (INT i = tmp.get_first(&iter);
             i >= 0; i = tmp.get_next((UINT)i, &iter)) {
            ASSERT0(m_md_sys->getMD((UINT)i));

            //Get to know where the base pointed to.
            MDSet const* pts = getPointTo((UINT)i, *mx);
            MD const* typed_md = NULL;
            if (pts != NULL && !pts->is_empty()) {
                if (array_base->getAI() != NULL &&
                    pts->is_contain_global() &&
                    (typed_md = computePointToViaType(array_base)) != NULL) {
                    setPointToUniqueMD((UINT)i, *mx, typed_md);
                    mds.clean(*getSBSMgr());
                    mds.bunion(typed_md, *getSBSMgr());
                } else {
                    inferArrayInfinite((INT)ofst, is_ofst_predicable,
                                       mdsz, *pts, mds);
                }

            } else if (array_base->getAI() != NULL &&
                       (typed_md = computePointToViaType(array_base)) != NULL) {
                mds.bunion(typed_md, *getSBSMgr());
            } else {
                inferArrayInfinite((INT)ofst, false, 0, *m_maypts, mds);
                *mds_is_may_pt = true;
                break;
            }
        }
    }

    //Set ir's ref-md and ref-mdset.
    AC_is_mds_mod(ic) = true;
    MDSetIter iter = NULL;
    MD const* x = NULL;
    if (mds.get_elem_count() == 1 &&
        !MD_is_may(x = m_md_sys->getMD((UINT)mds.get_first(&iter)))) {
        m_rg->setMustRef(ir, m_md_sys->getMD((UINT)mds.get_first(&iter)));
        ir->cleanRefMDSet();
    } else {
        ir->cleanRefMD();
        m_rg->setMayRef(ir, m_mds_hash->append(mds));
    }

    tmp.clean(*getSBSMgr());
}


//This function infer array element memory address according to
//the LDA base of array operation.
//This function will set the Ref MD and Ref MD set of array operation.
//is_ofst_pred: true if 'ofst' describe the computed byte offset related to
//              'array_base'.
//ofst: byte offset, only valid if 'is_ofst_pred' is true.
MD const* AliasAnalysis::inferArrayLdabase(IR * ir,
                                           IR * array_base,
                                           bool is_ofst_pred,
                                           UINT ofst,
                                           IN OUT AACtx * ic)
{
    ASSERT0(ir->isArrayOp() && array_base->is_lda());
    AACtx tic(*ic);
    MD const* ldamd = processLda(array_base, &tic);
    ASSERT0(ldamd);

    //CASE: struct S {int a; int b;} p[10];
    //      int foo(int i) { return p[1].b; }
    //When processLda() finish, the first iteration of AA
    //will genrete MD11={ofst=8,size=4}. Therefore caller
    //will reshape MD11 to be MD12={ofst=12,size=4} and
    //set MD12 as MustAddr of 'ir'.
    //However if at the second iteration of AA, return MustAddr
    //directly here, caller will still reshape MD12 to be
    //MD13={ofst=16,size=4}.
    //This lead to incorrect MD for the array operation.
    //Thus we should not stop recompute MustAddr even if AC_is_mds_mod
    //unchanged.
    //if (!AC_is_mds_mod(&tic)) {
    //    ASSERTN(ir->getMayRef() == NULL, ("have no mayaddr"));
    //    MD const* x = ir->getMustRef();
    //    ASSERT0(x && x->is_effect());
    //    return x;
    //}

    //Lda's MD changed, thus array base MD have to be recomputed.
    ir->cleanRefMDSet();

    //Compute the MD size and offset if 'ofst' is constant.
    if (!ldamd->is_exact()) {
        m_rg->setMustRef(ir, ldamd);
        return ldamd;
    }

    bool changed = false;
    MD tmd(*ldamd);
    if (is_ofst_pred) {
        //The byte offset of array element is predicable.
        if (ofst != 0) {
            MD_ofst(&tmd) += ofst;
            changed = true;
        }

        //Set MD size to be the size of array element if MD is exact.
        if (MD_size(&tmd) != ir->getTypeSize(m_tm)) {
            MD_size(&tmd) = ir->getTypeSize(m_tm);
            changed = true;
        }
    } else {
        //Set array operation to operate a range type MD.
        //e.g: array:(dim[80]) (lda x);
        //Following code will set tmd to MD with range [0~80].
        changed = true;
        UINT basesz = m_tm->getPointerBaseByteSize(array_base->getType());
        ASSERT0(basesz);

        //The range type MD begin at the byte offset of LDA.
        //e.g: array (lda x:12) indicate the array begin at
        //12 bytes offset to x address.
        MD_ofst(&tmd) = LDA_ofst(array_base);

        //The approximate and conservative size of array.
        //MD_size(&tmd) = basesz - LDA_ofst(array_base);
        MD_size(&tmd) = basesz;
        MD_ty(&tmd) = MD_RANGE;
    }
    if (!changed) {
        //mds is unchanged.
        m_rg->setMustRef(ir, ldamd);
        return ldamd;
    }
    //Reshape MD of LDA.
    MD const* entry = m_md_sys->registerMD(tmd);
    ASSERT0(entry->is_effect());
    ASSERT0(MD_id(entry) > 0);
    m_rg->setMustRef(ir, entry);
    return entry;
}


//Compute the memory address and ONLY record the top level
//ARRAY node's memory address.
//'mds' : record memory descriptor of 'ir'.
//This function will set the Ref MD and Ref MD set of array operation.
void AliasAnalysis::processArray(IR * ir,
                                 IN OUT MDSet & mds,
                                 IN OUT AACtx * ic,
                                 IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->isArrayOp());

    //Scan subscript expression and infer the offset of array element.
    for (IR * s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
        AACtx tic;
        tic.copyTopDownFlag(*ic);
        AC_comp_pt(&tic) = false;
        inferExpression(s, mds, &tic, mx);
    }
    IR * array_base = ARR_base(ir);

    //Next, scaning array base, it may be LDA
    //operation or computational expression.
    TMWORD ofst_val = 0;
    bool is_ofst_predicable = ir->calcArrayOffset(&ofst_val, m_tm);
    bool mds_is_may_pt = false;
    AACtx tic;
    tic.copyTopDownFlag(*ic);
    AC_comp_pt(&tic) = false;

    if (array_base->is_lda()) {
        //Array base is LDA operation.
        MD const* md = inferArrayLdabase(ir, array_base,
                                         is_ofst_predicable,
                                         (UINT)ofst_val, &tic);
        ASSERT0(md);
        mds.clean(*getSBSMgr());
        mds.bunion_pure(md->id(), *getSBSMgr());
    } else {
        //Array base is a computational expression.
        inferArrayExpBase(ir, array_base, is_ofst_predicable,
                          (UINT)ofst_val, mds, &mds_is_may_pt,
                          &tic, mx);
    }
    ic->copyBottomUpFlag(tic);

    ASSERT0(!mds.is_empty());

    //If array offset is not zero, the result data type may not
    //being the element type. Try to infer the actual memory address of
    //array element.
    MDSet tmp;
    if (!ir->is_any() && tryReshapeMDSet(ir, &mds, &tmp)) {
        //Update mds's content when it has been changed and recored in 'tmp'.
        mds.copy(tmp, *getSBSMgr());

        //Assign the address to ir.
        //If the MD in mayaddr is single and exact,
        //regarding ir as referencing single and exact MD,
        //or else regard ir as referencing a set of MD.
        MD const* x = NULL;
        MDSetIter iter2 = NULL;
        if (tmp.get_elem_count() == 1 &&
            !MD_is_may(x = m_md_sys->getMD((UINT)tmp.get_first(&iter2)))) {
            m_rg->setMustRef(ir, x);
            ir->cleanRefMDSet();
        } else {
            ir->cleanRefMD();
            m_rg->setMayRef(ir, m_mds_hash->append(tmp));
        }
    }
    tmp.clean(*getSBSMgr());
    ASSERTN(ir->getMustRef() ||
            (ir->getMayRef() && !ir->getMayRef()->is_empty()),
            ("ir should be assigned DU reference"));

    if (!AC_comp_pt(ic)) { return; }

    //Caller need array's point-to.
    if (mds_is_may_pt) {
        //Do not recompute again. Return mds directly.
        return;
    }

    //Compute the POINT-TO of array element.
    bool has_unified_may_pt = false;
    MDSetIter iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        MDSet const* pts = getPointTo((UINT)i, *mx);
        if (pts != NULL && !pts->is_empty()) {
            tmp.bunion(*pts, *getSBSMgr());
        } else if (!has_unified_may_pt) {
            has_unified_may_pt = true;
            //We do NOT known where p[...] pointed to, use the
            //conservative solution.
            tmp.bunion(*m_maypts, *getSBSMgr());
        }
    }
    mds.copy(tmp, *getSBSMgr());
    tmp.clean(*getSBSMgr());
}


bool AliasAnalysis::evaluateFromLda(IR const* ir)
{
    //Attempt to infer more presicion point-to if ssa info is avaiable.
    if (ir->is_cvt()) { return evaluateFromLda(CVT_exp(ir)); }

    if (!ir->is_pr()) { return false; }

    SSAInfo const* ssainfo = PR_ssainfo(ir);
    if (ssainfo == NULL) { return false; }

    IR * defstmt = SSA_def(ssainfo);
    if (defstmt == NULL || !defstmt->is_stpr()) { return false; }

    IR const* rhs = STPR_rhs(defstmt);
    switch (rhs->getCode()) {
    case IR_LDA: return true;
    case IR_PR: return evaluateFromLda(rhs);
    case IR_CVT: return evaluateFromLda(CVT_exp(rhs));
    default:;
    }

    IR const* r = rhs;
    for (;;) {
        switch (r->getCode()) {
        case IR_ADD:
            {
                //Check the opnd0 if current expresion is : op0 + imm(0)
                IR const* op1 = BIN_opnd1(r);
                if (op1->is_const() && op1->is_int()) {
                    r = BIN_opnd0(r);
                    break;
                } else {
                    return false;
                }
            }
            //Unreachable.
        case IR_LDA: return true;
        case IR_PR: return evaluateFromLda(r);
        default: goto END; //To elim -Wunreachable-code warning.
        }
    }
END:
    return false;
}


//Compute the constant offset for pointer arithmetic.
//Return true if the offset can be confirmed via
//simply calculation, and has been determined by this function.
//Return false if caller need to keep reevaluating the offset of 'opnd0_mds'.
//Return true if this function guarantee applying 'opnd1' on mds successful,
//then caller can use the returned result directly. If the mds in 'opnd0_mds'
//changed, then set 'changed' to true.
//Note this function should not be invoked if 'ir' is placed in
//loop, because each call of this function will generate a set
//of new MD which MD_offset updated according to 'opnd1' constant
//value. For ir that do not be in loop, the MD set generated by this
//function is dependent to the number of iterations of AA solver.
//e.g:given same ir,
//    1th iter: mds = { MD5(ofst=0,size=4) }
//    2nd iter: mds = { MD5(ofst=0,size=4), MD6(ofst=4,size=4) }
//    3rd iter: mds = { MD5(ofst=0,size=4),
//                      MD6(ofst=4,size=4), MD7(ofst=8,size=4) }
//    ...
bool AliasAnalysis::tryComputeConstOffset(IR const* ir,
                                          MDSet const& opnd0_mds,
                                          IR const* opnd1,
                                          IN OUT MDSet & mds,
                                          bool * changed)
{
    ASSERT0(changed);
    *changed = false;
    //Compute the offset for pointer arithmetic.
    if (opnd1->is_const() && opnd1->is_int() && CONST_int_val(opnd1) == 0) {
        return true;
    }

    HOST_UINT const_offset = 0;
    if (opnd1->is_const() && opnd1->is_int()) {
        const_offset = (HOST_UINT)CONST_int_val(opnd1);
    } else if (opnd1->is_pr()) {
        if (!m_rg->evaluateConstInteger(opnd1, (ULONGLONG*)&const_offset)) {
            return false;
        }
    } else {
        return false;
    }
    if (const_offset == 0) {
        return true;
    }

    //In the case: LDA(x) + ofst, we can determine
    //the value of LDA(x) is constant.
    //Keep offset validation unchanged.
    mds.clean(*getSBSMgr());
    MDSetIter iter;
    if (ir->is_add()) {
        for (INT i = opnd0_mds.get_first(&iter);
            i >= 0; i = opnd0_mds.get_next((UINT)i, &iter)) {
            MD * imd = m_md_sys->getMD((UINT)i);
            if (!imd->is_exact()) {
                mds.bunion(imd, *getSBSMgr());
                continue;
            }
            MD const* entry = NULL;
            MD x(*imd);
            //In the case: LDA(x) + ofst, we can determine
            //the value of LDA(x) is constant.
            ; //Keep offset validation unchanged.
            MD_ofst(&x) += (UINT)const_offset;
            entry = m_md_sys->registerMD(x);
            ASSERT0(entry->id() > MD_UNDEF);
            mds.bunion(entry, *getSBSMgr());
        }
        *changed = true;
        return true;
    }

    for (INT i = opnd0_mds.get_first(&iter);
         i >= 0; i = opnd0_mds.get_next((UINT)i, &iter)) {
        MD * imd = m_md_sys->getMD((UINT)i);
        if (!imd->is_exact()) {
            mds.bunion(imd, *getSBSMgr());
            continue;
        }
        MD const* entry = NULL;
        MD x(*imd);
        //case: &x - ofst.
        //Keep offset validation unchanged.
        INT s = ((INT)MD_ofst(&x)) - (INT)const_offset;
        if (s < 0) {
            MD_ty(&x) = MD_UNBOUND;
            MD_size(&x) = 0;
            MD_ofst(&x) = 0;
        } else {
            MD_ofst(&x) = s;
        }
        entry = m_md_sys->registerMD(x);
        ASSERT0(entry->id() > MD_UNDEF);
        mds.bunion(entry, *getSBSMgr());
    }
    *changed = true;
    return true;
}


//Perform pointer arith to compute where ir might point to.
//If we compute the point-to set of p+1, that always equivilate to
//compute the point-to of p, and each element in the set will be
//registered to be unbound. Since if p+1 is placed in a loop,
//we could not determine the exact MD where p pointed to.
//'mds' : record memory descriptor of 'ir'.
void AliasAnalysis::inferPointerArith(IR const* ir,
                                      OUT MDSet & mds,
                                      MDSet const& opnd0_mds,
                                      IN OUT AACtx * opnd0_ic,
                                      IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_add() || ir->is_sub());
    IR * opnd1 = BIN_opnd1(ir);
    bool changed = false;
    if ((BIN_opnd0(ir)->is_lda() || evaluateFromLda(BIN_opnd0(ir))) &&
        tryComputeConstOffset(ir, opnd0_mds, opnd1, mds, &changed)) {
        if (changed) {
            //mds record the expected info.
            AC_returned_pts(opnd0_ic) = NULL;
            ASSERT0(!mds.is_empty());
        } else {
            //opnd0_mds record the expected info.
            ASSERT0(mds.is_empty() && !opnd0_mds.is_empty());
            ASSERT0(AC_returned_pts(opnd0_ic) == NULL);
            mds.copy(opnd0_mds, *getSBSMgr());
        }
        return;
    }

    //Generate MD expression for opnd1.
    AACtx opnd1_tic(*opnd0_ic);
    opnd1_tic.cleanBottomUpFlag();
    AC_comp_pt(&opnd1_tic) = false; //PointToSet of addon is useless.
    inferExpression(opnd1, mds, &opnd1_tic, mx);

    //Bottom-up flag of opnd1 is useless to its parent.

    mds.clean(*getSBSMgr());
    if (AC_has_comp_lda(&opnd1_tic) && AC_has_comp_lda(opnd0_ic)) {
        //In the situation such as: &a - &b.
        ASSERTN(ir->is_sub(), ("only support pointer sub pointer"));
        AC_has_comp_lda(opnd0_ic) = false;
        return;
    }

    ASSERTN(mds.is_empty(), ("output buffer not yet initialized"));
    if (opnd0_mds.is_empty()) {
        ASSERT0(AC_returned_pts(opnd0_ic));
        if (AC_returned_pts(opnd0_ic) == m_maypts) {
            //Point-To set of opnd0 of binary-op is MayPointToSet.
            //Compute constant offset does not make sense to the worst case.
            return;
        }

        if (isInLoop(ir->getStmt())) {
            //Pointer arithmetic causes ambiguous memory access.
            //e.g: while (...) { p = p+1 }
            //Where is p pointing to at all?
            //Set each MD of opnd0 to be UNBOUND even if it is exact
            //to keep the conservation.
            convertExact2Unbound(*AC_returned_pts(opnd0_ic), &mds);
            AC_returned_pts(opnd0_ic) = NULL;
            ASSERT0(!mds.is_empty()); //mds record the expected info.
            return;
        }

        bool changed = false;
        if (!tryComputeConstOffset(ir, *AC_returned_pts(opnd0_ic),
                                   opnd1, mds, &changed)) {
            convertExact2Unbound(*AC_returned_pts(opnd0_ic), &mds);
            AC_returned_pts(opnd0_ic) = NULL;
            ASSERT0(!mds.is_empty()); //mds record the expected info.
            return;
        }
        if (changed) {
            //mds record the expected info.
            AC_returned_pts(opnd0_ic) = NULL;
            ASSERT0(!mds.is_empty());
        } else {
            //AC_returned_pts record the expected info.
            ASSERT0(mds.is_empty());
            ASSERT0(AC_returned_pts(opnd0_ic) &&
                    !AC_returned_pts(opnd0_ic)->is_empty());
        }
        return;
    }

    ASSERT0(AC_returned_pts(opnd0_ic) == NULL);
    
    if (isInLoop(ir->getStmt())) {
        //Pointer arithmetic causes ambiguous memory access.
        //e.g: while (...) { p = p+1 }
        //Where is p pointing to at all?
        //Set each MD of opnd0 to be UNBOUND even if it is exact
        //to keep the conservation.
        convertExact2Unbound(opnd0_mds, &mds);
        ASSERT0(!mds.is_empty()); //mds record the expected info.
        return;
    }

    changed = false;
    if (!tryComputeConstOffset(ir, opnd0_mds, opnd1, mds, &changed)) {
        convertExact2Unbound(opnd0_mds, &mds);
        ASSERT0(!mds.is_empty());
        return;
    }
    if (changed) {
        ASSERT0(!mds.is_empty()); //mds record the expected info.
    } else {
        //opnd0_mds record the expected info.
        ASSERT0(mds.is_empty() && !opnd0_mds.is_empty());
        mds.copy(opnd0_mds, *getSBSMgr());
    }
}


void AliasAnalysis::convertExact2Unbound(MDSet const& src, MDSet * tgt)
{
    MDSetIter iter;
    for (INT i = src.get_first(&iter);
        i >= 0; i = src.get_next((UINT)i, &iter)) {
        MD * imd = m_md_sys->getMD((UINT)i);
        if (!imd->is_exact()) {
            tgt->bunion(imd, *getSBSMgr());
            continue;
        }

        MD x(*imd);
        MD_ty(&x) = MD_UNBOUND;
        MD const* entry = m_md_sys->registerMD(x);
        ASSERT0(MD_id(entry) > 0);
        tgt->bunion_pure(MD_id(entry), *getSBSMgr());
    }
}


bool AliasAnalysis::isInLoop(IR const* ir)
{
    ASSERT0(ir && ir->is_stmt());
    IRBB * bb = ir->getBB();
    ASSERT0(bb);
    LI<IRBB> const* li = m_cfg->getLoopInfo();

    //Only have to check the outermost loop body.
    for (; li != NULL; li = LI_next(li)) {
        if (li->isInsideLoop(bb->id())) {
            return true;
        }
    }
    return false;
}


//Compute the point-to set of expression.
//ir may be pointer arithmetic, even if opnd0 is not pointer type.
//'mds' : record memory-descriptor set or
//    point-to set of 'ir' if AC_comp_pt(ic) is true.
void AliasAnalysis::processPointerArith(IR * ir,
                                        IN OUT MDSet & mds,
                                        IN OUT AACtx * ic,
                                        IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_add() || ir->is_sub());
    IR * opnd0 = BIN_opnd0(ir);
    IR * opnd1 = BIN_opnd1(ir);
    ir->cleanRef();

    //ic may have been set comp_pt to be true.
    AACtx tic(*ic);
    tic.cleanBottomUpFlag();

    MDSet tmp;
    inferExpression(opnd0, tmp, &tic, mx); //Generate MDS of opnd0.

    //For given expression: a + b, we
    //can not tell which memory it descripted.
    if (opnd0->is_ptr()) {
        //This is pointer arithmetic.
        //If p is a pointer, the followed expr is analyzable:
        //    (p +/- n), where n is constant.
        //    (p +/- n + ...), where n is constant.
        //p may be literal, e.g: ((int*)0x1000) + 1.
        if (ir->is_add()) {
            ASSERTN(!opnd1->is_ptr(), ("pointer can not plus pointer"));
        }
        if (!opnd1->is_ptr()) {
            //pointer +/- n still be pointer.
            //ir may be VOID.
            //ASSERT0(ir->is_ptr());
        }

        inferPointerArith(ir, mds, tmp, &tic, mx);
        ic->copyBottomUpFlag(tic);
        tmp.clean(*getSBSMgr());
        return;
    }

    if (ir->is_add()) {
        //opnd1 can not be pointer. e.g: x+&q
        ASSERT0(!opnd1->is_ptr());
    }
    if (AC_comp_pt(&tic)) {
        //tmp already have recorded the POINT-TO set of opnd0.
        //Now, infering the final POINT-TO set according to op0 +/- op1.
        inferPointerArith(ir, mds, tmp, &tic, mx);
    } else {
        //Scan and generate MD of opnd1.
        AACtx ttic(*ic);
        ttic.cleanBottomUpFlag();
        inferExpression(opnd1, mds, &ttic, mx);
        mds.clean(*getSBSMgr()); //Do not remove this code.
    }
    ic->copyBottomUpFlag(tic);
    tmp.clean(*getSBSMgr());
}


//Assign unique MD to pr.
//'mds' : record memory descriptor of 'ir' if AC_comp_pt() is false, or
//        record the POINT-TO set of 'ir' if AC_comp_pt() is true.
//        Note if AC_comp_pt() is true, the returned POINT-TO set may be
//        recorded in 'mds' or AC_returned_pts.
//Return MD of ir.
MD const* AliasAnalysis::assignPRMD(IR * ir,
                                    IN OUT MDSet * mds,
                                    IN OUT AACtx * ic,
                                    IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_pr());
    ASSERT0(mds && ic);
    MD const* tmp = NULL;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());        
        AC_is_mds_mod(ic) = true;
    } else {    
        AC_is_mds_mod(ic) = false;
    }
    tmp = ir->getMustRef();
    ASSERT0(tmp);
    mds->clean(*getSBSMgr());
    AC_returned_pts(ic) = NULL;
    if (!AC_comp_pt(ic)) {
        //The MD should be recorred in 'mds'.
        //Parent expression may retrive 'mds'.
        mds->bunion(tmp, *getSBSMgr());
        return tmp;
    }

    ASSERT0(mx);
    MDSet const* pts = getPointTo(MD_id(tmp), *mx);
    MD const* typed_md = NULL;
    if (pts != NULL && !pts->is_empty()) {
        if (pts->is_contain_global() &&
            ir->getAI() != NULL &&
            (typed_md = computePointToViaType(ir)) != NULL) {
            setPointToUniqueMD(MD_id(tmp), *mx, typed_md);
            mds->bunion(typed_md, *getSBSMgr());
        } else if (pts == m_maypts) {
            AC_returned_pts(ic) = m_maypts;
        } else {
            //CASE: Do we need copy a well-hashed POINT-TO set to
            //'mds', rather than just return the POINT-TO set.
            //mds->copy(*pts, *getSBSMgr());
            AC_returned_pts(ic) = pts;

            //Actually, the function only record POINT-TO set in
            //AC_returned_pts if it is may-point-to set. Also
            //may-point-to set will lead to inaccurate analysis.
            //So we return the POINT-TO set via 'mds' as much as
            //possible, even if there is a cost of set bunion.
            //mds->clean(*getSBSMgr());
            //AC_returned_pts(ic) = m_maypts;
        }
    } else if (ir->getAI() != NULL &&
               (typed_md = computePointToViaType(ir)) != NULL) {
        setPointToMDSetByAddMD(MD_id(tmp), *mx, typed_md);
        mds->bunion(typed_md, *getSBSMgr());
    } else {
        //We do NOT known where p pointed to.
        //mds->copy(*m_maypts, *getSBSMgr());
        AC_returned_pts(ic) = m_maypts;
        if (ir->is_ptr() || ir->is_any()) {
            setPointTo(MD_id(tmp), *mx, m_maypts);
        }
    }
    return tmp;
}


//'mds' : record memory descriptor of 'ir'
MD const* AliasAnalysis::assignLoadMD(IR * ir,
                                      IN OUT MDSet * mds,
                                      IN OUT AACtx * ic,
                                      IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_ld());
    ASSERT0(mds && ic);
    MD const* t = NULL;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        AC_is_mds_mod(ic) = true;
    } else {
        AC_is_mds_mod(ic) = false;
    }
    t = ir->getMustRef();
    ASSERT0(t);
    AC_returned_pts(ic) = NULL;
    mds->clean(*getSBSMgr());
    if (!AC_comp_pt(ic)) {
        //The MD should be recorred in 'mds'.
        //Parent expression may retrive 'mds'.
        mds->bunion(t, *getSBSMgr());
        return t;
    }
    ASSERT0(mx);
    AC_is_mds_mod(ic) = true;
    MDSet const* pts = getPointTo(MD_id(t), *mx);
    
    MD const* typed_md = NULL;
    if (pts != NULL && !pts->is_empty()) {
        if (pts->is_contain_global() &&
            ir->getAI() != NULL &&
            (typed_md = computePointToViaType(ir)) != NULL) {
            setPointToUniqueMD(MD_id(t), *mx, typed_md);
            mds->bunion(typed_md, *getSBSMgr());
        } else {
            AC_returned_pts(ic) = pts;
        }
    } else if (ir->getAI() != NULL &&
               (typed_md = computePointToViaType(ir)) != NULL) {
        setPointToMDSetByAddMD(MD_id(t), *mx, typed_md);
        mds->bunion(typed_md, *getSBSMgr());
    } else {
        //We do NOT known where p pointed to,
        //e.g: If p->? (p+2)->??
        //mds->copy(*m_maypts, *getSBSMgr());
        AC_returned_pts(ic) = m_maypts;
        if (ir->is_ptr() || ir->is_any()) {
            setPointTo(MD_id(t), *mx, m_maypts);
        }
    }
    return t;
}


//Assign unique MD to 'id'.
//'mds': record memory descriptor of 'ir'.
MD const* AliasAnalysis::assignIdMD(IR * ir,
                                    IN OUT MDSet * mds,
                                    IN OUT AACtx * ic)
{
    ASSERT0(ir->is_id());
    ASSERT0(ic && mds);
    MD const* t = NULL;
    AC_returned_pts(ic) = NULL;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());        
        AC_is_mds_mod(ic) = true;
    } else {
        AC_is_mds_mod(ic) = false;
    }
    t = ir->getMustRef();
    ASSERT0(t);
    mds->clean(*getSBSMgr());
    //IR_ID does not generate or transit POINT-TO set.
    //The MD should be recorred in 'mds'.
    //Parent expression may retrive 'mds'.
    mds->bunion(t, *getSBSMgr());
    return t;
}


//Union POINT-TO set for each element in 'mds', and hash the unified result
//return it.
//mds: represents address of current ILD.
MDSet const* AliasAnalysis::unifyPointToSet(MDSet const& mds,
                                            MD2MDSet const* mx)
{
    UINT cnt = mds.get_elem_count();
    if (cnt == 0) { return m_maypts; }
    MDSetIter iter = NULL;
    if (cnt == 1) {
        MDSet const* pts = getPointTo((UINT)mds.get_first(&iter),
                                      *const_cast<MD2MDSet*>(mx));
        if (pts != NULL && !pts->is_empty()) {
            return pts;
        }
        //We do NOT known where current pointer pointed to, return
        //the worst may-point-to set.
        //Empty POINT-TO set may cause solver miss POINT-TO-PAIR,
        //thus set returned pts to be the worst set.
        return m_maypts;
    }

    MDSet tmp;
    iter = NULL;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        MDSet const* pts = getPointTo((UINT)i, *const_cast<MD2MDSet*>(mx));
        if (pts != NULL && !pts->is_empty()) {
            tmp.bunion(*pts, *getSBSMgr());
        } else {
            //We do NOT known where current pointer pointed to, return
            //the worst may-point-to set.
            //Empty POINT-TO set may cause solver miss POINT-TO-PAIR,
            //thus set returned pts to be the worst set.
            tmp.clean(*getSBSMgr());
            return m_maypts;
        }
    }
    MDSet const* p = m_mds_hash->append(tmp);
    tmp.clean(*getSBSMgr());
    return p;
}

//This function set MustAddr or MayAddr of ir by analyszing given MDSet.
//mds: mds may be the MayAddr MDSet. Note mds should have been hashed.
void AliasAnalysis::setMustOrMayAddr(MDSet const* mds, IR * ir)
{
    ASSERT0(mds && ir && m_mds_hash->find(*mds));
    ir->cleanRefMD();
    if (mds->get_elem_count() == 1 && !mds->is_contain_global()) {
        MDSetIter iter = NULL;
        m_rg->setMustRef(ir, m_md_sys->getMD((UINT)mds->get_first(&iter)));
        ir->cleanRefMDSet();
        return;
    }
    m_rg->setMayRef(ir, mds);
}


//refmds: ref MDSet of ir.
//ir: given indirect operation, such as IST, ILD.
//comp_ir_pt: true if caller require to compute the POINT-TO set of ir.
//Return POINT-TO set of ir, if comp_ir_pts is true.
MDSet const* AliasAnalysis::updateIndirectOpAddrAndPointToSet(
        MDSet const* refmds,
        IR * ir,
        bool comp_ir_pts,
        MD2MDSet * mx)
{
    ASSERT0(refmds && ir);
    ASSERT0(ir->is_ist() || ir->is_ild());
    setMustOrMayAddr(refmds, ir);
    if (!comp_ir_pts) { return NULL; }
    //According to the requirement of parent expression to
    //compute the POINT-TO set of ILD itself.
    //For conservatively,
    MDSet const* returned_pts = unifyPointToSet(*refmds, mx);

    //Empty POINT-TO set may cause solver miss POINT-TO-PAIR,
    //thus set returned pts to be the worst set.
    ASSERT0(returned_pts);
    return returned_pts;
}


//'ir': IR expressions that describing memory address.
//'mds': record memory descriptors which 'ir' represented.
//'ic': context of analysis.
void AliasAnalysis::processILoad(IR * ir,
                                 IN OUT MDSet & mds,
                                 IN OUT AACtx * ic,
                                 IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_ild());
    ASSERT0(g_is_support_dynamic_type || ILD_base(ir)->is_ptr());

    //... = *q, if q->x, set ir's MD to be 'x'.
    AACtx tic(*ic);

    //Ask base-expression of ILD to compute what ILD stand for.
    AC_comp_pt(&tic) = true;

    //Compute the memory address that ILD described.
    inferExpression(ILD_base(ir), mds, &tic, mx);

    if (mds.is_empty()) {
        //Compute ILD ref MDSet and POINT-TO set.
        //If mds is empty, the inaccurate POINT-TO set
        //of ILD_base(ir) recorded in AC_returned_pts.
        ASSERTN(AC_returned_pts(&tic), ("mds and returne_pts are alternative"));

        //The POINT-TO set of base-expression indicates what ILD stand for.
        MDSet const* ildrefmds = AC_returned_pts(&tic);

        //If ir has exact type, try to reshape MD to make ir's
        //reference MD more accurate.
        MDSet tmp;
        if (!ir->is_any() && tryReshapeMDSet(ir, ildrefmds, &tmp)) {
            //If MD in 'ildrefmds' changed, set ildrefmds to be new MDSet.
            ildrefmds = m_mds_hash->append(tmp);
        }
        tmp.clean(*getSBSMgr());

        //According to the requirement of parent expression to
        //compute the POINT-TO set of ILD itself.
        AC_returned_pts(ic) = updateIndirectOpAddrAndPointToSet(
            ildrefmds, ir, AC_comp_pt(ic), mx);
        return;
    }

    ASSERTN(AC_returned_pts(ic) == NULL,
            ("mds and returne_pts are alternative"));
    AC_is_mds_mod(ic) |= AC_is_mds_mod(&tic);
    if (!ir->is_any()) {
        //ir has exact type, try to reshape MD to make ir's
        //reference MD more accurate.
        MDSet tmp;
        if (tryReshapeMDSet(ir, &mds, &tmp)) {
            mds.copy(tmp, *getSBSMgr());
        }
        ASSERT0(!mds.is_empty());
        tmp.clean(*getSBSMgr());
    }

    //Set 'mds' as address MDSet of ILD.
    MD const* mustaddr = NULL;
    MDSetIter iter = NULL;
    if (mds.get_elem_count() == 1 &&
        !MD_is_may(mustaddr = m_md_sys->getMD((UINT)mds.get_first(&iter)))) {
        mustaddr = m_md_sys->getMD((UINT)mds.get_first(&iter));
        m_rg->setMustRef(ir, mustaddr);
        ir->cleanRefMDSet();
    } else if (isAllElementDerivedFromSameEffectVar(mds, m_md_sys, &mustaddr)) {
        ASSERT0(mustaddr);
        m_rg->setMustRef(ir, mustaddr);
        ir->cleanRefMDSet();
    } else {
        ir->cleanRefMD();
        m_rg->setMayRef(ir, m_mds_hash->append(mds));
    }

    if (!AC_comp_pt(ic)) { return; }

    //According the requirement of parent function, we have to compute
    //the POINT-TO set of cuurent ILD.

    //Compute the ILD pointed to.
    if (mustaddr != NULL) {
        MDSet const* pts = getPointTo(MD_id(mustaddr), *mx);
        if (pts != NULL && !pts->is_empty()) {
            mds.copy(*pts, *getSBSMgr());
        } else {
            //We do NOT known where p pointed to, and compute
            //the offset as well.
            //e.g: If p->? (p+2)->??
            mds.copy(*m_maypts, *getSBSMgr());
        }
        return;
    }

    MDSet tmp;
    bool has_meet_worst_may_pt = false;
    MDSetIter iter2;
    //'mds' represents the may-address of current ILD.
    for (INT i = mds.get_first(&iter2);
         i >= 0; i = mds.get_next((UINT)i, &iter2)) {
        MDSet const* pts = getPointTo((UINT)i, *mx);
        if (pts != NULL && !pts->is_empty()) {
            tmp.bunion(*pts, *getSBSMgr());
        } else if (!has_meet_worst_may_pt) {
            has_meet_worst_may_pt = true;
            //We do NOT known where p pointed to, and compute
            //the offset as well.
            //e.g: If p->? (p+2)->??
            //tmp.bunion(*m_maypts, *getSBSMgr());
            break;
        }
    }
    if (has_meet_worst_may_pt) {
        mds.clean(*getSBSMgr());
        AC_returned_pts(ic) = m_maypts;
        tmp.clean(*getSBSMgr());
        return;
    }
    mds.copy(tmp, *getSBSMgr());
    tmp.clean(*getSBSMgr());
}


//'mds' : record memory descriptor of 'ir'.
void AliasAnalysis::processConst(IR * ir,
                                 IN OUT MDSet & mds,
                                 IN OUT AACtx * ic)
{
    ASSERT0(ir->is_const());
    ASSERT0(ic);

    mds.clean(*getSBSMgr());
    if (ir->is_str()) {
        //'ir' describes memory address of string const.
        //Add a new Var to describe the string.
        //'mds' : record memory descriptor of 'ir'.
        MD const* t = m_rg->allocStringMD(CONST_str_val(ir));
        ASSERT0(t);
        if (!m_is_visit.is_contain(ir->id())) {
            m_is_visit.bunion(ir->id());
            AC_is_mds_mod(ic) = true;
        } else {
            AC_is_mds_mod(ic) = false;
        }
        //Does the MD should be returned?
        //mds.bunion(t, *getSBSMgr());
    } else {
        AC_is_mds_mod(ic) = false;
    }

    if (AC_comp_pt(ic)) {
        //We do NOT known where const pointed to,
        //e.g: User could regard immediate value 0x1234 as memory address.
        AC_returned_pts(ic) = m_maypts;
    }
}


//Recompute the data type byte-size accroding to stmt type.
//Note we only revise data type if LDA on the RHS expression, and if
//LDA appeared on RHS, its MD should be put in 'mds'.
//For those MDs in 'returned_pts' when 'mds' is empty, we just keep them
//unchanged.
//It have to note that if LDA appeared in RHS, mds should not be empty.
void AliasAnalysis::recomputeDataType(AACtx const& ic,
                                      IR const* ir,
                                      OUT MDSet & mds)
{
    ASSERT0(ir && ir->is_stmt());
    if (!AC_has_comp_lda(&ic) || ir->is_any() || !ir->is_ptr()) { return; }

    //If RHS of 'ir' return the address which taken by IR_LDA.
    //Here we need to reinference the actual memory address according
    //to ir's data type, because the result data type of Array
    //operation may not same as the data type or LDA operation.
    //e.g: st b:ptr<i32> = LDA(a:mc<1024>);
    //     where a is a memory chunk which size is 1024 bytes, but b is
    //     pointer that pointed to an i32 memory space.

    //ir's data type may not be pointer type.
    //e.g: x = (int)&arr[j], the result data type of LDA has been
    //converted to integer. So x will be integer.
    ASSERTN(mds.get_effect_md(m_md_sys),
            ("LDA's base must be effect MD"));

    //CASE: mds may include elements which have been in m_maypts.
    //ASSERT0(!mds->is_intersect(*m_maypts));
    reviseMDSize(mds, m_tm->getPointerBaseByteSize(ir->getType()));
}


//ir: stmt.
//rhs: RHS expression of ir. It's refdu might be updated by this function.
//lhs_md: MD of LHS of 'ir'.
//ic: analysis context.
void AliasAnalysis::inferStoreValue(IR const* ir,
                                    IR * rhs,
                                    MD const* lhs_md,
                                    AACtx const* ic,
                                    IN MD2MDSet * mx)
{
    ASSERT0(ir->is_st() || ir->is_stpr() || ir->is_setelem());

    //Considering the STORE operation, there
    //are three situations should be processed:
    //    1. The RHS's address was taken.
    //    2. The RHS is ILD, that cause indirect POINT-TO generated.
    //        e.g: p=ILD(q), if q->a->w, then p->w.
    //    3. Propagate the MDSet that RHS pointed to.
    //        e.g: p=q, if q->a, then p->a.
    ASSERT0(lhs_md);
    if (!g_is_support_dynamic_type) {
        //lhs of IR_ST may be inexact because IR_ST may be VOID.
        ASSERT0(lhs_md->is_exact());
    }

    //1. p = q, q is pointer, if q->x, add p->x.
    //2. p = q, q is array base (e.g:q[100]), add p->q.
    //3. p = &q, add p->q.
    //4. p = (&q)+n+m, add p->q.
    AACtx rhsic(*ic);
    if (ir->is_ptr() || ir->is_any()) {
        //Regard ir as pointer if its has VOID type.
        //Query and ask subroutine infer and compute the POINT-TO set
        //of 'rhs' expression.
        AC_comp_pt(&rhsic) = true;
    }
    
    MDSet rhsrefmds;
    inferExpression(rhs, rhsrefmds, &rhsic, mx);
    recomputeDataType(rhsic, ir, rhsrefmds);
    updateLHSPointToSet(AC_comp_pt(&rhsic),
                        AC_has_comp_lda(&rhsic),
                        lhs_md,
                        NULL,
                        rhs,
                        rhsrefmds,
                        AC_returned_pts(&rhsic),
                        mx);
    rhsrefmds.clean(*getSBSMgr());
}


//Caculate pointer info accroding to rules for individiual ir, and
//constructing the mapping table that maps MD to an unique Var.
//e.g For given four point-to pairs {p->a,p->b,q->c,q->d}.
//    store can be shown as
//        p = q;
//    this make the point-to set of 'p' to be {p->c, p->d}.
//    and the formally formular form is:
//    MDSet(p) = MDSet(q)
void AliasAnalysis::processStore(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_st());
    MD const* t = ir->getMustRef();
    ASSERT0(t);
    AACtx ic;
    inferStoreValue(ir, ST_rhs(ir), t, &ic, mx);
}


//Caculate pointer info accroding to rules for individiual ir, and
//constructing the mapping table that maps MD to an unique Var.
//e.g For given four point-to pairs {p->a,p->b,q->c,q->d}.
//    store can be shown as
//        p = q;
//    this make the point-to set of 'p' to be {p->c, p->d}.
//    and the formally formular form is:
//    MDSet(p) = MDSet(q)
void AliasAnalysis::processStorePR(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_stpr());
    MD const* t = ir->getMustRef();
    ASSERT0(t);
    AACtx ic;
    inferStoreValue(ir, STPR_rhs(ir), t, &ic, mx);
}


//Compute the point to info for IR_SETELEM.
void AliasAnalysis::processSetelem(IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_setelem());
    MD const* t = ir->getMustRef();
    ASSERT0(t);
    AACtx ic;
    inferStoreValue(ir, SETELEM_val(ir), t, &ic, mx);

    MDSet tmp;
    ic.clean();
    inferExpression(SETELEM_base(ir), tmp, &ic, mx);
    if (!SETELEM_ofst(ir)->is_const()) {
        ic.clean();
        tmp.clean(*getSBSMgr());
        inferExpression(SETELEM_ofst(ir), tmp, &ic, mx);
    }
    tmp.clean(*getSBSMgr());
}


//Compute the point to info for IR_GETELEM.
void AliasAnalysis::processGetelem(IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_getelem() && GETELEM_ofst(ir));
    //Process base field, it must refer to memory object.
    AACtx ic;
    MDSet tmp;
    inferExpression(GETELEM_base(ir), tmp, &ic, mx);

    //Process byte offset to base field.
    ic.clean();
    inferExpression(GETELEM_ofst(ir), tmp, &ic, mx);
    tmp.clean(*getSBSMgr());
}


void AliasAnalysis::processPhi(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_phi());
    MD const* phi_md = ir->getMustRef();
    ASSERT0(phi_md);
    AACtx ic;
    if (ir->is_ptr() || ir->is_any()) {
        AC_comp_pt(&ic) = true;
    }

    bool const comp_pt = AC_comp_pt(&ic);
    MDSet phi_pts;
    bool phi_pts_is_maypts = false;
    if (comp_pt) {
        MDSet const* pts = getPointTo(MD_id(phi_md), *mx);
        if (pts != NULL) {
            if (pts == m_maypts) {
                phi_pts_is_maypts = true;
            } else {
                phi_pts.copy(*pts, *getSBSMgr());
            }
        }
    } else {
        cleanPointTo(MD_id(phi_md), *mx);
    }

    MDSet tmp;
    for (IR * opnd = PHI_opnd_list(ir); opnd != NULL; opnd = opnd->get_next()) {
        AACtx tic(ic);
        inferExpression(opnd, tmp, &tic, mx);
        if (comp_pt) {
            if (phi_pts_is_maypts) {
                tmp.clean(*getSBSMgr());
                continue;
            }

            if (tmp.is_empty()) {
                ASSERT0(AC_returned_pts(&tic));
                phi_pts_is_maypts = true;
                //phi_pts.bunion(*m_maypts, *getSBSMgr());
                continue;
            }

            //phires may point to the union set of each operand.
            phi_pts.bunion(tmp, *getSBSMgr());
        }

        tmp.clean(*getSBSMgr());
    }

    if (comp_pt) {
        ASSERT0(!phi_pts.is_empty() || phi_pts_is_maypts);
        if (phi_pts_is_maypts) {
            setPointTo(MD_id(phi_md), *mx, m_maypts);
        } else {
            setPointTo(MD_id(phi_md), *mx, m_mds_hash->append(phi_pts));
        }
    }

    phi_pts.clean(*getSBSMgr());
}


//This function update LHS's POINT-TO set accroding to RHS.
//is_lhs_pointer: true if transit rhs's POINT-TO set to lhs.
//rhs: RHS expression of stmt, which are IR_ST, IR_IST, IR_STARRAY.
//rhsrefmds: record memory descriptor of 'rhs' if AC_comp_pt() is false, or
//           record the POINT-TO set of 'rhs' if AC_comp_pt() is true.
//           Note if AC_comp_pt() is true, the returned POINT-TO set may be
//           recorded in 'rhsrefmds' or AC_returned_pts.
//returned_pts: record the POINT-TO set of 'rhs' if AC_comp_pt() is true.
//              Note if AC_comp_pt() is true, the returned POINT-TO set may be
//              recorded in 'rhsrefmds' or AC_returned_pts.
void AliasAnalysis::updateLHSPointToSet(bool is_lhs_pointer,
                                        bool rhs_taken_address,
                                        MD const* lhs_mustaddr,
                                        MDSet const* lhs_mayaddr,
                                        IR const* rhs,
                                        MDSet & rhsrefmds,
                                        MDSet const* returned_pts,
                                        MD2MDSet * mx)
{
    if (rhs_taken_address || is_lhs_pointer) {
        //If result type of IST is pointer, and the ptset is empty, then
        //it might point to anywhere.
        //e.g:
        //p = q, q is array base (e.g:q[100]), add p->q.
        //p = &q, add p->q.
        //p = (&q)+n+m, add p->q.
        //
        //e.g: Given p=&q and *p=(int*)0x1000;
        //=> q=0x1000, and q is pointer, so q may point to anywhere.
        //
        // *p = q, if p->{x}, q->{a}, add {x}->{a}
        // *p = q, if p->{x}, q->{¦µ}, add {x}->{¦µ}
        // *p = q, if p->{¦µ}, q->{x}, add {all mem}->{x}
        //
        //Update the POINT-TO of elems in p's point-to set.
        //Aware of whether if the result of IST is pointer.
        if (rhs_taken_address) {
            ASSERT0(rhsrefmds.get_elem_count() == 1);

            //POINT-TO set may include element which also have been in m_maypts.
            //CASE: =&g[i] violate this constraint.
            //ASSERT0(!ptset->is_intersect(m_maypts));
        }

        //'rhsrefmds' should record the POINT-TO set of 'rhs'.
        MDSet const* lhs_pts = NULL;
        if (rhsrefmds.is_empty()) {
            ASSERTN(returned_pts && !returned_pts->is_empty(),
                    ("they are alternatively"));
            ASSERT0(m_mds_hash->find(*returned_pts));
            lhs_pts = computeMayPointToViaTBAA(rhs, returned_pts);
        } else {
            lhs_pts = computeMayPointToViaTBAA(rhs, &rhsrefmds);
            if (lhs_pts == &rhsrefmds) {
                lhs_pts = m_mds_hash->append(rhsrefmds);
            }
        }
        ASSERT0(lhs_pts && !lhs_pts->is_empty());
        setLHSPointToSet(lhs_mustaddr, lhs_mayaddr, lhs_pts, mx);
        return;
    }

    //p = q, q is pointer, if q->x, add p->x.
    //Given a pointer, if its point-to is empty, the pointer
    //points to the worst MAY_POINT_TO set.
    //
    //May be unify MAY_POINT_TO set is correct in comprehension,
    //but it will consume more memory.
    //e.g: Given pr1->MAY_POINT_TO, if we set
    //pr1->NULL, that may cause convert_md2mds_to_ptpair()
    //can not recognize pr1's POINT-TO set, which lead to its pt-pair info
    //missing. That will cause a dead cycle at global iterative solver.
    if (m_flow_sensitive) {
        ASSERT0(!is_lhs_pointer);
        if (lhs_mustaddr != NULL) {
            if (lhs_mustaddr->is_exact()) {
                cleanPointTo(MD_id(lhs_mustaddr), *mx);
            }
        } else {
            ASSERT0(lhs_mayaddr);
            //mayaddr may contain inexact MD.
            ElemCleanExactPointTo(*lhs_mayaddr, mx);
        }
    }
}


void AliasAnalysis::inferIStoreValue(IR const* ir,
                                     AACtx const* ic,
                                     IN MD2MDSet * mx)
{
    ASSERT0(ir->is_ist());
    MDSet const* ist_mayaddr = const_cast<IR*>(ir)->getMayRef();
    MD const* ist_mustaddr = const_cast<IR*>(ir)->getMustRef();
    ASSERTN(ist_mustaddr || (ist_mayaddr && !ist_mayaddr->is_empty()),
            ("Have no idea about what ir is."));

    //Considering the ISTORE operation, there are three
    //situations should be handled:
    //    1. The RHS is LDA.
    //    2. The RHS is ILD, that caused indirect POINT-TO generated.
    //        e.g: *p=ILD(q), and p->x,q->a,a->w, then x->w,
    //    3. Propagate the MDSet that RHS pointed to the LHS.
    //        e.g: *p=q, and p->x,q->a, then x->a.
    AACtx rhsic(*ic);
    if (ir->is_ptr() || ir->is_any()) {
        AC_comp_pt(&rhsic) = true;
    }

    MDSet rhsrefmds;
    inferExpression(IST_rhs(ir), rhsrefmds, &rhsic, mx);
    recomputeDataType(rhsic, ir, rhsrefmds);
    updateLHSPointToSet(AC_comp_pt(&rhsic),
                        AC_has_comp_lda(&rhsic),
                        ist_mustaddr,
                        ist_mayaddr,
                        IST_rhs(ir),
                        rhsrefmds,
                        AC_returned_pts(&rhsic),
                        mx);
    rhsrefmds.clean(*getSBSMgr());
}


//Set the POINT-TO set of LHS MD and LHS MDSet.
//pts: POINT-TO set that have been hashed.
void AliasAnalysis::setLHSPointToSet(MD const* lhs_mustaddr,
                                     MDSet const* lhs_mayaddr,
                                     MDSet const* pts,
                                     MD2MDSet * mx)
{
    ASSERT0(pts && m_mds_hash->find(*pts));
    if (m_flow_sensitive) {
        if (lhs_mustaddr != NULL) {
            if (lhs_mustaddr->is_exact()) {
                setPointTo(MD_id(lhs_mustaddr), *mx, pts);
            } else {
                setPointToMDSetByAddMDSet(MD_id(lhs_mustaddr), *mx, *pts);
            }
        } else {
            ASSERT0(lhs_mayaddr);
            //mayaddr may contain inexact MD.
            ElemCopyAndUnionPointTo(*lhs_mayaddr, *pts, mx);
        }
        return;
    }
    //Flow insensitive.
    if (lhs_mustaddr != NULL) {
        setPointToMDSetByAddMDSet(MD_id(lhs_mustaddr), *mx, *pts);
    } else {
        ASSERT0(lhs_mayaddr);
        ElemUnionPointTo(*lhs_mayaddr, *pts, mx);
    }
}


void AliasAnalysis::inferStoreArrayValue(IR const* ir,
                                         AACtx const* ic,
                                         IN MD2MDSet * mx)
{
    ASSERT0(ir->is_starray());
    MDSet const* lhs_mayaddr = const_cast<IR*>(ir)->getMayRef();
    MD const* lhs_mustaddr = const_cast<IR*>(ir)->getMustRef();
    ASSERTN(lhs_mustaddr != NULL ||
            (lhs_mayaddr != NULL && !lhs_mayaddr->is_empty()),
            ("You need infer the may memory address of array operation"));

    //Propagate the MDSet that RHS pointed to the LHS.
    //e.g: a[x]=q, and q->a, then a[x]->a.
    AACtx rhsic(*ic);
    if (ir->is_ptr() || ir->is_any()) {
        AC_comp_pt(&rhsic) = true;
    }

    MDSet rhsrefmds;
    inferExpression(STARR_rhs(ir), rhsrefmds, &rhsic, mx);
    recomputeDataType(rhsic, ir, rhsrefmds);
    updateLHSPointToSet(AC_comp_pt(&rhsic),
                        AC_has_comp_lda(&rhsic),
                        lhs_mustaddr,
                        lhs_mayaddr,
                        STARR_rhs(ir),
                        rhsrefmds,
                        AC_returned_pts(&rhsic),
                        mx);
    rhsrefmds.clean(*getSBSMgr());
}


//Infer point-to set for array element.
//e.g For given 2 point-to pairs {q->c,q->d}.
//  store array can be demonstrated as
//    a[x] = q;
//  this changes POINT-TO set of a[x] to {a[x]->c, a[x]->d}.
void AliasAnalysis::processStoreArray(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_starray());
    //mem location may pointed to set.
    MDSet mayaddr;
    AACtx ic;
    AC_comp_pt(&ic) = false; //Here we just need to compute the may address.

    //Compute where array element may point to.
    //Note ir should be assign Must-MD or May-MDSet when
    //processArray() return.
    processArray(ir, mayaddr, &ic, mx);

    //mayaddr is useless when processArray() return.
    //If array offset is not zero, the result data type may not
    //being the element type. Try to infer the actual memory address of
    //array element. All of above situations have been handled inside
    //processArray().
    ic.clean();
    mayaddr.clean(*getSBSMgr());

    //We do not need to known where array's elem point-to.
    AC_comp_pt(&ic) = false;

    //Infer the memory address for RHS and POINT-TO set of LHS.
    inferStoreArrayValue(ir, &ic, mx);
}


//Reshape MD in 'mds' if one of them need to be reshape.
//Record reshaped MD in 'newmds' and return true.
//This function will iterate MD in 'mds', new MD will be generated either
//ir's MD size is different to MD in 'mds' or ir has offset.
//Return true if new MD generated, and new MD record in 'newmds'.
bool AliasAnalysis::tryReshapeMDSet(IR const* ir,
                                    MDSet const* mds,
                                    OUT MDSet * newmds)
{
    if (ir->is_any()) { return false; }
    UINT newofst = ir->getOffset();
    UINT newsize = ir->getTypeSize(m_tm);
    ASSERT0(newsize > 0);
    ASSERT0(mds && newmds);
    bool change = false;
    MDSetIter iter = NULL;
    //Note if ir's type is ANY, size is 1, see details in data_type.h.
    //Thus MD indicates an object that is p + ild_ofst + 0.
    for (INT i = mds->get_first(&iter);
         i >= 0; i = mds->get_next((UINT)i, &iter)) {
        MD * l = m_md_sys->getMD((UINT)i);
        ASSERT0(l);
        if (l->is_exact() &&
            (MD_size(l) != newsize || newofst != 0)) {
            //Reshape MD in IR reference.
            MD md(*l);
            MD_ofst(&md) += newofst;
            MD_size(&md) = newsize;
            MD const* entry = m_md_sys->registerMD(md);
            ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
            newmds->bunion(MD_id(entry), *getSBSMgr());
            change = true;
        } else {
            newmds->bunion((UINT)i, *getSBSMgr());
        }
    }
    return change;
}


//Indirect store.
//Analyse pointers according to rules for individiual ir to
//constructe the map-table that maps MD to an unique Var.
void AliasAnalysis::processIStore(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_ist());
    ASSERT0(g_is_support_dynamic_type || IST_base(ir)->is_ptr());

    //mem location that base-expression may pointed to.
    MDSet base_maypts;
    AACtx ic;
    //Compute where IST_base may point to.
    AC_comp_pt(&ic) = true;

    //Compute where IST_base may point to.
    inferExpression(IST_base(ir), base_maypts, &ic, mx);
    //The POINT-TO set of base-expression indicates what IST stands for.
    if (base_maypts.is_empty()) {
        //Compute IST ref MDSet and POINT-TO set.
        //If base_maypts is empty, the inaccurate POINT-TO set
        //of IST_base(ir) recorded in AC_returned_pts.
        ASSERTN(AC_returned_pts(&ic) && !AC_returned_pts(&ic)->is_empty(),
                ("mds and returne_pts are alternative"));

        //The POINT-TO set of base-expression indicates what IST stands for.
        MDSet const* istrefmds = AC_returned_pts(&ic);

        //If ir has exact type, try to reshape MD to make ir's
        //reference MD more accurate.
        MDSet tmp;
        if (!ir->is_any() && tryReshapeMDSet(ir, istrefmds, &tmp)) {
            //If MD in 'ildrefmds' changed, set ildrefmds to be new MDSet.
            istrefmds = m_mds_hash->append(tmp);
        }
        tmp.clean(*getSBSMgr());

        //According to the requirement of parent expression to
        //compute the POINT-TO set of ILD itself.
        //For conservatively,
        AC_returned_pts(&ic) = updateIndirectOpAddrAndPointToSet(
            istrefmds, ir,
            false, //set false to tell function do not need to compute
                   //POINT-TO set, it will be computed by inferIStoreValue()
                   //later. Avoid redundant computation.
            mx);
        AACtx ic2;
        inferIStoreValue(ir, &ic2, mx);
        return;
    }
    ASSERTN(AC_returned_pts(&ic) == NULL,
            ("mds and returne_pts are alternative"));

    //maypts indicates a list of MD that IST may reference.
    //The POINT-TO set of IST recored in AC_returned_pts() if maypts is empty.
    //if (IST_ofst(ir) != 0 ||
    //    ir->getTypeSize(m_tm) != IST_base(ir)->getTypeSize(m_tm)) {
    if (!ir->is_any()) {
        MDSet tmp;
        bool change = tryReshapeMDSet(ir, &base_maypts, &tmp);
        if (change) {
            base_maypts.copy(tmp, *getSBSMgr());
        }
        tmp.clean(*getSBSMgr());
    }

    MD const* x = NULL;
    MDSetIter iter = NULL;
    if (base_maypts.get_elem_count() == 1 &&
        !MD_is_may(x = m_md_sys->getMD((UINT)base_maypts.get_first(&iter)))) {
        if (x->is_exact() &&
            !ir->is_any() &&
            ir->getTypeSize(m_tm) != MD_size(x)) {
            MD md(*x);

            //Note if ir's type is VOID, size is 1.
            //Thus the MD indicates an object that is
            //p + ist_ofst + 0, if ist_ofst exist.
            MD_size(&md) = ir->getTypeSize(m_tm);
            MD const* entry = m_md_sys->registerMD(md);
            ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
            x = entry;
        }
        m_rg->setMustRef(ir, x);
        ir->cleanRefMDSet();
    } else {
        //Set ir has no exact mem-addr for convervative.
        ir->cleanRefMD();
        m_rg->setMayRef(ir, m_mds_hash->append(base_maypts));
    }
    base_maypts.clean(*getSBSMgr());

    AACtx ic2;
    inferIStoreValue(ir, &ic2, mx);
}


//NOTE: The def and use info should be available for region, otherwise
//this function will be costly.
void AliasAnalysis::processRegion(IR const* ir, IN MD2MDSet * mx)
{
    Region * rg = REGION_ru(ir);
    ASSERT0(rg);
    if (rg->is_readonly()) { return; }

    bool has_maydef_info = false;
    //Check if region modify or use MD.
    MDSet const* defmds = rg->getMayDef();
    if (defmds != NULL && !defmds->is_empty()) {
        ElemCopyPointToAndMayPointTo(*defmds, mx);
        has_maydef_info = true;
    }

    if (!has_maydef_info && (rg->is_blackbox() || rg->is_inner())) {
        //For conservative purpose, region may change
        //global variable's point-to and local variable's
        //point-to which are forseeable.
        processRegionSideeffect(*mx);
    }
}


void AliasAnalysis::processRegionSideeffect(IN OUT MD2MDSet & mx)
{
    //Set all mds which are global pointers or parameters which taken
    //address point to maypts.
    MDId2MD const* id2md = m_md_sys->getID2MDMap();
    for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
        MD * t = id2md->get((UINT)j);
        if (t == NULL) {
            //MD j has been allocated but freed and record in free-list.
            continue;
        }

        Var const* v = t->get_base();
        if (v->is_pointer() ||
            v->is_any()) { //v may be pointer if its type is VOID
            setPointTo((UINT)j, mx, m_maypts);

            //Set the point-to set of 't' to be empty in order
            //to enforce its point-to set will be recomputed if
            //necessary.
            //cleanPointTo(t, mx);
        }
    }
}


void AliasAnalysis::processReturn(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_return());
    if (RET_exp(ir) != NULL) {
        MDSet tmp;
        AACtx tic;
        ASSERT0(RET_exp(ir) == NULL || RET_exp(ir)->is_single());
        ASSERT0(RET_exp(ir)->is_single());
        inferExpression(RET_exp(ir), tmp, &tic, mx);
        tmp.clean(*getSBSMgr());
    }
}


void AliasAnalysis::processCallSideeffect(IN OUT MD2MDSet & mx,
                                          MDSet const& by_addr_mds)
{
    //Set all mds which are global pointers or parameters which taken
    //address point to maypts.
    MDId2MD const* id2md = m_md_sys->getID2MDMap();
    for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
        MD const* t = id2md->get((UINT)j);
        if (t == NULL) { continue; }

        Var const* v = t->get_base();
        if (VAR_is_global(v) &&
            (v->is_pointer() ||
             v->is_any())) { //v may be pointer if its type is VOID
            setPointTo((UINT)j, mx, m_maypts);

            //Set the point-to set of 't' to be empty in order
            //to enforce its point-to set will be recomputed if
            //necessary.
            //cleanPointTo(t, mx);
        }
    }

    if (by_addr_mds.is_empty()) { return; }

    MDSetIter iter;
    for (INT j = by_addr_mds.get_first(&iter);
         j >= 0; j = by_addr_mds.get_next((UINT)j, &iter)) {
        MD const* t = m_md_sys->getMD((UINT)j);
        ASSERT0(t != NULL);
        Var const* v = t->get_base();
        if (VAR_is_addr_taken(v) &&
            (v->is_pointer() ||
             v->is_any())) { //v may be pointer if its type is VOID
            setPointTo((UINT)j, mx, m_maypts);

            //Set the point-to set of 't' to be empty in order
            //to enforce its point-to set will be recomputed if
            //necessary.
            //cleanPointTo(t, mx);
        }
    }
}


MD const* AliasAnalysis::allocHeapobj(IR * ir)
{
    MD const* heap_obj = m_ir2heapobj.get(ir);
    if (heap_obj != NULL) {
        return heap_obj;
    }

    CHAR name[128];
    sprintf(name, "heap_obj%d", m_ir2heapobj.get_elem_count());
    ASSERT0(strlen(name) < 128);
    Var * tv = m_rg->getVarMgr()->registerVar(
        name, m_tm->getMCType(0), 0, VAR_GLOBAL);

    //Set the var to be unallocable, means do NOT add
    //var immediately as a memory-variable.
    //For now, it is only be regarded as a placeholder.
    //And set it to allocable if the var is in essence need to be
    //allocated in memory.
    VAR_is_unallocable(tv) = true;

    //Will be freed region destruction.
    m_rg->addToVarTab(tv);

    MD md;
    MD_base(&md) = tv;
    MD_ty(&md) = MD_UNBOUND;
    MD const* entry = m_md_sys->registerMD(md);
    ASSERT0(MD_id(entry) > 0);
    m_ir2heapobj.set(ir, entry);
    return entry;
}


//Compute the point-to set modification when we meet call.
void AliasAnalysis::processCall(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->isCallStmt());

    MDSet tmp;
    if (ir->is_icall()) {
        AACtx tic;
        inferExpression(ICALL_callee(ir), tmp, &tic, mx);
    }

    //Analyze parameters.
    MDSet by_addr_mds;
    for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
        AACtx tic;
        if (p->is_ptr() || p->is_any()) {
            AC_comp_pt(&tic) = true;
        }
        inferExpression(p, tmp, &tic, mx);

        if (ir->isReadOnly()) { continue; }
        if (!AC_comp_pt(&tic) && !AC_has_comp_lda(&tic)) { continue; }
        if (!tmp.is_empty()) {
            by_addr_mds.bunion(tmp, *getSBSMgr());
            continue;
        }
        if (AC_returned_pts(&tic) != NULL &&
            !by_addr_mds.is_equal(*AC_returned_pts(&tic))) {
            by_addr_mds.bunion(*AC_returned_pts(&tic), *getSBSMgr());
        }
    }

    for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
        AACtx tic;
        inferExpression(p, tmp, &tic, mx);
    }

    if (CALL_is_alloc_heap(ir)) {
        if (ir->hasReturnValue()) {
            MD const* t;
            if (!m_is_visit.is_contain(ir->id())) {
                m_is_visit.bunion(ir->id());
                t = m_rg->allocCallResultPRMD(ir);
            } else {
                t = ir->getMustRef();
            }
            setPointToUniqueMD(MD_id(t), *mx, allocHeapobj(ir));
        }

        //The function such as malloc or new function should not modify
        //the memory in XOC scope.
        tmp.clean(*getSBSMgr());
        by_addr_mds.clean(*getSBSMgr());
        return;
    }

    //Analyz return-values.
    if (ir->hasReturnValue()) {
        MD const* t = NULL;
        if (!m_is_visit.is_contain(ir->id())) {
            m_is_visit.bunion(ir->id());
            t = m_rg->allocCallResultPRMD(ir);
        } else {
            t = ir->getMustRef();
        }

        ASSERTN(t, ("result of call miss exact MD."));

        if (ir->is_ptr() || ir->is_any()) {
            //Try to improve the precsion via typed alias info or
            //set ir pointed to May-Point-To set for conservative purpose.
            MD const* typed_md;
            if (ir->getAI() != NULL &&
                (typed_md = computePointToViaType(ir)) != NULL) {
                //Make use of typed pointer info to improve the precsion.
                setPointToUniqueMD(MD_id(t), *mx, typed_md);
            } else {
                //Finally, set result PR points to May-Point-To set.
                setPointTo(MD_id(t), *mx, m_maypts);
            }
        } else {
            cleanPointTo(MD_id(t), *mx);
        }
    }

    if (ir->isReadOnly()) {
        //Readonly call does not modify any point-to informations.
        tmp.clean(*getSBSMgr());
        by_addr_mds.clean(*getSBSMgr());
        return;
    }

    processCallSideeffect(*mx, by_addr_mds);
    tmp.clean(*getSBSMgr());
    by_addr_mds.clean(*getSBSMgr());
}


//Analyze the Tree style memory-address-expression,
//and compute the MDSet for 'expr'.
//'expr': IR expressions that describing memory address.
//'mds': record memory descriptors which 'expr' might express.
//'ic': context of analysis.
void AliasAnalysis::inferExpression(IR * expr,
                                    IN OUT MDSet & mds,
                                    IN OUT AACtx * ic,
                                    IN OUT MD2MDSet * mx)
{
    switch (expr->getCode()) {
    case IR_ID:
        assignIdMD(expr, &mds, ic);
        return;
    case IR_ILD:
        processILoad(expr, mds, ic, mx);
        return;
    case IR_LD:
        assignLoadMD(expr, &mds, ic, mx);
        return;
    case IR_LDA: {
        MD const* md = processLda(expr, ic);
        mds.clean(*getSBSMgr());
        mds.bunion_pure(md->id(), *getSBSMgr());
        return;
    }
    case IR_ARRAY:
        processArray(expr, mds, ic, mx);
        return;
    case IR_CONST:
        processConst(expr, mds, ic);
        return;
    case IR_ADD:
    case IR_SUB:
        processPointerArith(expr, mds, ic, mx);
        return;
    case IR_PR:
        assignPRMD(expr, &mds, ic, mx);
        return;
    case IR_CVT:
        processCvt(expr, mds, ic, mx);
        return;
    case IR_ASR:
    case IR_LSR: //Logical shift right
    case IR_LSL: //Logical shift left
    case IR_MUL:
    case IR_DIV:
    case IR_REM:
    case IR_MOD:
    case IR_LAND:
    case IR_LOR: {
        //CASE: if (p && q)
        //GR: land (ld p:*<2>, ld q:*<2>)
        //ASSERTN(!BIN_opnd0(expr)->is_ptr(),
        //    ("illegal, left operand can not be pointer type"));

        AACtx tic(*ic);
        AC_comp_pt(&tic) = false;
        inferExpression(BIN_opnd1(expr), mds, &tic, mx);

        tic.cleanBottomUpFlag();
        inferExpression(BIN_opnd0(expr), mds, &tic, mx);

        //These expressions does not descripte
        //an accurate memory-address. So, for the
        //conservative purpose, we claim that can
        //not find any MD.
        if (AC_comp_pt(ic)) {
            mds.copy(*m_maypts, *getSBSMgr());
        } else {
            mds.clean(*getSBSMgr());
        }
        return;
    }
    case IR_BAND:
    case IR_BOR:
    case IR_XOR: {
        //opnd0 may be pointer.
        AACtx tic(*ic);
        AC_comp_pt(&tic) = false;
        inferExpression(BIN_opnd1(expr), mds, &tic, mx);

        tic.cleanBottomUpFlag();
        inferExpression(BIN_opnd0(expr), mds, &tic, mx);

        //These expressions does not descripte
        //an accurate memory-address. So, for the
        //conservative purpose, we claim that can
        //not find any MD.
        if (AC_comp_pt(ic)) {
            mds.copy(*m_maypts, *getSBSMgr());
        } else {
            mds.clean(*getSBSMgr());
        }
        return;
    }
    case IR_BNOT:
    case IR_NEG:
    case IR_LNOT: {
        //opnd0 may be pointer.
        AACtx tic(*ic);
        AC_comp_pt(&tic) = false;
        inferExpression(UNA_opnd(expr), mds, &tic, mx);

        //These expressions does not descripte
        //an accurate memory-address. So, for the
        //conservative purpose, we claim that can
        //not find any MD.
        if (AC_comp_pt(ic)) {
            mds.copy(*m_maypts, *getSBSMgr());
        } else {
            mds.clean(*getSBSMgr());
        }
        return;
    }
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE: {
        AACtx tic(*ic);
        AC_comp_pt(&tic) = false;
        inferExpression(BIN_opnd0(expr), mds, &tic, mx);

        tic.cleanBottomUpFlag();
        inferExpression(BIN_opnd1(expr), mds, &tic, mx);
        if (AC_comp_pt(ic)) {
            mds.copy(*m_maypts, *getSBSMgr());
        } else {
            mds.clean(*getSBSMgr());
        }
        return;
    }
    case IR_LABEL:
        return;
    case IR_SELECT: {
        AACtx tic(*ic);
        AC_comp_pt(&tic) = false;

        inferExpression(SELECT_pred(expr), mds, &tic, mx);

        tic.cleanBottomUpFlag();
        inferExpression(SELECT_trueexp(expr), mds, &tic, mx);

        tic.cleanBottomUpFlag();
        inferExpression(SELECT_falseexp(expr), mds, &tic, mx);
        if (AC_comp_pt(ic)) {
            //We do not know if condition is true or false.
            mds.copy(*m_maypts, *getSBSMgr());
        } else {
            mds.clean(*getSBSMgr());
        }
        return;
    }
    default: UNREACHABLE();
    }
}


//Set POINT TO info.
//Set each md in 'mds' add set 'pt_set'.
//pt_set: POINT-TO set that has been hashed.
void AliasAnalysis::ElemUnionPointTo(MDSet const& mds,
                                     MDSet const& pt_set,
                                     IN MD2MDSet * mx)
{
    ASSERT0(m_mds_hash->find(pt_set));
    MDSetIter iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        setPointToMDSetByAddMDSet((UINT)i, *mx, pt_set);
    }
}


//Set POINT TO info.
//Set each md in 'mds' add 'pt_elem'.
void AliasAnalysis::ElemUnionPointTo(MDSet const& mds,
                                     MD const* pt_elem,
                                     IN MD2MDSet * mx)
{
    MDSetIter iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        setPointToMDSetByAddMD((UINT)i, *mx, pt_elem);
    }
}


//Set POINT TO info.
//Set each md in 'mds' points to 'pt_set' if it is exact, or
//else unify the set.
//pt_set: POINT-TO set that have been hashed.
void AliasAnalysis::ElemCopyAndUnionPointTo(MDSet const& mds,
                                            MDSet const& pt_set,
                                            IN MD2MDSet * mx)
{
    ASSERT0(m_mds_hash->find(pt_set));
    MDSetIter iter = NULL;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (m_md_sys->getMD((UINT)i)->is_exact()) {
            setPointTo((UINT)i, *mx, m_maypts);
            continue;
        }
        setPointToMDSetByAddMDSet((UINT)i, *mx, pt_set);
    }
}


//Set POINT TO info.
//Set each md in 'mds' points to 'pt_set'.
void AliasAnalysis::ElemCopyPointTo(MDSet const& mds,
                                    IN MDSet & pt_set,
                                    IN MD2MDSet * mx)
{
    MDSetIter iter = NULL;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        setPointToMDSet((UINT)i, *mx, pt_set);
    }
}


//Set POINT TO info.
//Set each md in 'mds' points to May-Point-To set.
void AliasAnalysis::ElemCopyPointToAndMayPointTo(MDSet const& mds,
                                                 IN MD2MDSet * mx)
{
    MDSetIter iter = NULL;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        setPointTo((UINT)i, *mx, m_maypts);
    }
}


//Set POINT TO info.
//Set md in 'mds' points to NULL if it is exact.
void AliasAnalysis::ElemCleanExactPointTo(MDSet const& mds, IN MD2MDSet * mx)
{
    MDSetIter iter = NULL;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (m_md_sys->getMD((UINT)i)->is_exact()) {
            cleanPointTo((UINT)i, *mx);
        }
    }
}


//Set POINT TO info.
//Set md in 'mds' points to NULL.
void AliasAnalysis::ElemCleanPointTo(MDSet const& mds, IN MD2MDSet * mx)
{
    MDSetIter iter = NULL;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        cleanPointTo((UINT)i, *mx);
    }
}


//Dump IR's point-to of each BB.
void AliasAnalysis::dumpInOutPointToSetForBB() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n==---- DUMP POINT TO INFO ----==");
    BBList * bbl = m_cfg->getBBList();
    AliasAnalysis * pthis = const_cast<AliasAnalysis*>(this);
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        PtPairSet * in_set = pthis->getInPtPairSet(bb);
        PtPairSet * out_set = pthis->getOutPtPairSet(bb);
        note(getRegion(), "\n--- BB%d ---", bb->id());
        note(getRegion(), "\nIN-SET::");
        dumpPtPairSet(*in_set);
        note(getRegion(), "\n\nOUT-SET::");
        dumpPtPairSet(*out_set);
    }
}


//Dump POINT-TO pair record in 'pps'.
void AliasAnalysis::dumpPtPairSet(PtPairSet const& pps) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    StrBuf buf(256);
    UINT k = 0;
    bool detail = true;
    PtPairSetIter iter;
    PtPairMgr & pptmgr = const_cast<PtPairMgr&>(m_ppmgr);
    for (INT i = pps.get_first(&iter);
         i >= 0; i = pps.get_next((UINT)i, &iter), k++) {
        PtPair * pp = pptmgr.get((UINT)i);
        ASSERT0(pp);
        note(getRegion(), "\nMD%u->MD%u,  ", PP_from(pp), PP_to(pp));

        if (!detail) { continue; }

        MD const* from = m_md_sys->getMD(PP_from(pp));
        ASSERT0(from);

        prt(getRegion(), "%s", from->get_base()->dump(buf, m_tm));
        if (from->is_exact()) {
            prt(getRegion(), ":ofst(%u):size(%u)", MD_ofst(from), MD_size(from));
        } else {
            prt(getRegion(), ":ofst(--):size(%u)", MD_size(from));
        }

        prt(getRegion(), " ------> ");

        MD const* to = m_md_sys->getMD(PP_to(pp));

        buf.clean();
        prt(getRegion(), "%s", to->get_base()->dump(buf, m_tm));

        if (to->is_exact()) {
            prt(getRegion(), ":ofst(%u):size(%u)", MD_ofst(to), MD_size(to));
        } else {
            prt(getRegion(), ":ofst(--):size(%u)", MD_size(to));
        }
    }
}


//Dump 'ir' point-to according to 'mx'.
//'dump_kid': dump kid's memory object if it exist.
void AliasAnalysis::dumpIRPointTo(IR const* ir,
                                  bool dump_kid,
                                  MD2MDSet const* mx) const
{
    if (ir == NULL || !m_rg->isLogMgrInit()) { return; }
    MD const* must = const_cast<IR*>(ir)->getMustRef();
    MDSet const* may = const_cast<IR*>(ir)->getMayRef();
    if (must != NULL ||
        (may != NULL && may->get_elem_count() > 0)) {
        dumpIR(ir, m_rg, NULL, 0);
    }
    m_rg->getLogMgr()->incIndent(2);
    switch (ir->getCode()) {
    case IR_ID:
    case IR_LD:
    case IR_PR:
    case IR_ST:
        if (must != NULL) {
            dumpMD2MDSet(must, mx);
        }
        break;
    default:
        if (may != NULL) {
            MDSetIter iter;
            for (INT i = may->get_first(&iter);
                 i >= 0; i = may->get_next((UINT)i, &iter)) {
                MD * md = m_md_sys->getMD((UINT)i);
                ASSERT0(md);
                dumpMD2MDSet(md, mx);
            }
        } else if (must != NULL) {
            dumpMD2MDSet(must, mx);
        }
    }

    if (dump_kid) {
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * kid = ir->getKid(i);
            if (kid != NULL) {
                dumpIRPointTo(kid, dump_kid, mx);
            }
        }
    }
    m_rg->getLogMgr()->decIndent(2);
}


//Dump all relations between IR, MD, and MDSet.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void AliasAnalysis::dumpIRPointToForBB(IRBB const* bb, bool dump_kid) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    m_rg->getLogMgr()->incIndent(1);
    note(getRegion(), "\n\n-- BB%u --", bb->id());
    m_rg->getLogMgr()->incIndent(1);
    IRListIter ct;
    MD2MDSet const* mx;
    if (m_flow_sensitive) {
        mx = mapBBtoMD2MDSet(bb->id());
    } else {
        mx = &m_unique_md2mds;
    }
    if (mx == NULL) {
        //e.g: If one has performed PRE and generated new BB, but
        //not invoked the IRAA::perform(), then the mx of
        //the new BB is not constructed.

        //interwarn("In IRAA, MD2MDSet of BB%u is NULL, may be new "
        //          "bb was generated. One should recall IRAA::perform()",
        //          bb->id());
        note(getRegion(), "\n-- BB%u's MD2MDSet is NULL", bb->id());
        m_rg->getLogMgr()->decIndent(2);
        return;
    }

    note(getRegion(), "\n-- MD2MDSet: --", bb->id());
    dumpMD2MDSet(mx, true);

    if (BB_irlist(bb).get_head(&ct) != NULL) {
        note(getRegion(), "\n\n-- IR POINT-TO: --");
    }
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != NULL; ir = BB_irlist(bb).get_next(&ct)) {
        note(getRegion(), "\n---------------------------------");
        dumpIRList(ir, m_rg, NULL, IR_DUMP_KID | IR_DUMP_SRC_LINE);
        note(getRegion(), "\n");
        ASSERT0(isValidStmtToAA(ir));
        switch (ir->getCode()) {
        case IR_ST:
            prt(getRegion(), "LHS:");
            dumpIRPointTo(ir, false, mx);
            note(getRegion(), "\nRHS:");
            dumpIRPointTo(ST_rhs(ir), false, mx);

            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(ST_rhs(ir), true, mx);
            }
            break;
        case IR_STPR:
            prt(getRegion(), "LHS:");
            dumpIRPointTo(ir, false, mx);
            note(getRegion(), "\nRHS:");
            dumpIRPointTo(STPR_rhs(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(STPR_rhs(ir), true, mx);
            }
            break;
        case IR_STARRAY:
            prt(getRegion(), "LHS:");
            dumpIRPointTo(ir, false, mx);
            note(getRegion(), "\nRHS:");
            dumpIRPointTo(STARR_rhs(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(ARR_base(ir), true, mx);
                dumpIRPointTo(STARR_rhs(ir), true, mx);
                for (IR * p = ARR_sub_list(ir); p != NULL; p = p->get_next()) {
                    dumpIRPointTo(p, true, mx);
                }
            }
            break;
        case IR_SETELEM:
            prt(getRegion(), "LHS:");
            dumpIRPointTo(ir, false, mx);
            note(getRegion(), "\nBASE:");
            dumpIRPointTo(SETELEM_base(ir), false, mx);
            note(getRegion(), "\nVALUE:");
            dumpIRPointTo(SETELEM_val(ir), false, mx);
            note(getRegion(), "\nOFFSET:");
            dumpIRPointTo(SETELEM_ofst(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(SETELEM_base(ir), true, mx);
                dumpIRPointTo(SETELEM_val(ir), true, mx);
                dumpIRPointTo(SETELEM_ofst(ir), true, mx);
            }
            break;
        case IR_GETELEM:
            prt(getRegion(), "LHS:");
            dumpIRPointTo(ir, false, mx);
            note(getRegion(), "\nBASE:");
            dumpIRPointTo(GETELEM_base(ir), false, mx);
            note(getRegion(), "\nOFFSET:");
            dumpIRPointTo(GETELEM_ofst(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(GETELEM_base(ir), true, mx);
                dumpIRPointTo(GETELEM_ofst(ir), true, mx);
            }
            break;
        case IR_IST:
            prt(getRegion(), "LHS:");
            dumpIRPointTo(ir, false, mx);
            note(getRegion(), "\nRHS:");
            dumpIRPointTo(IST_rhs(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(IST_base(ir), true, mx);
                dumpIRPointTo(IST_rhs(ir), true, mx);
            }
            break;
        case IR_CALL: {
            if (ir->hasReturnValue()) {
                prt(getRegion(), "LHS:");
                dumpIRPointTo(ir, false, mx);
            }

            UINT i = 0;
            for (IR * p = CALL_param_list(ir);
                 p != NULL; p = p->get_next()) {
                note(getRegion(), "\nPARAM%u:", i++);
                dumpIRPointTo(p, false, mx);
            }

            i = 0;
            for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
                note(getRegion(), "\nDUMMY%u:", i++);
                dumpIRPointTo(p, false, mx);
            }

            if (dump_kid) {
                if (CALL_param_list(ir) != NULL ||
                    CALL_dummyuse(ir) != NULL) {
                    note(getRegion(), "\n>> MDSet DETAIL:\n");
                }

                for (IR * p = CALL_param_list(ir);
                     p != NULL; p = p->get_next()) {
                    dumpIRPointTo(p, true, mx);
                }

                for (IR * p = CALL_dummyuse(ir);
                     p != NULL; p = p->get_next()) {
                    dumpIRPointTo(p, true, mx);
                }
            }
            break;
        }
        case IR_ICALL: { //indirective call
            if (ir->hasReturnValue()) {
                prt(getRegion(), "LHS:");
                dumpIRPointTo(ir, false, mx);
            }

            ASSERT0(ICALL_callee(ir) != NULL);
            prt(getRegion(), "CALLEE:");
            dumpIRPointTo(ICALL_callee(ir), false, mx);

            if (dump_kid && CALL_param_list(ir) != NULL) {
                note(getRegion(), "\n>> MDSet DETAIL:\n");
                for (IR * p = CALL_param_list(ir); p ; p = p->get_next()) {
                    dumpIRPointTo(p, true, mx);
                }
            }
            break;
        }
        case IR_GOTO:
            break;
        case IR_IGOTO:
            ASSERT0(IGOTO_vexp(ir) != NULL);
            prt(getRegion(), "VEXP:");
            dumpIRPointTo(IGOTO_vexp(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(IGOTO_vexp(ir), true, mx);
            }
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            ASSERT0(BR_det(ir) != NULL);
            prt(getRegion(), "DET:");
            dumpIRPointTo(BR_det(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(BR_det(ir), true, mx);
            }
            break;
        case IR_SELECT:
            ASSERT0(SELECT_pred(ir) != NULL);
            prt(getRegion(), "DET:");
            dumpIRPointTo(SELECT_pred(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(SELECT_pred(ir), true, mx);
            }
            break;
        case IR_SWITCH:
            ASSERT0(SWITCH_vexp(ir) != NULL);
            prt(getRegion(), "VEXP:");
            dumpIRPointTo(SWITCH_vexp(ir), false, mx);
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(SWITCH_vexp(ir), true, mx);
            }
            break;
        case IR_RETURN: {
            if (RET_exp(ir) != NULL) {
                dumpIRPointTo(RET_exp(ir), false, mx);
            }

            if (dump_kid && RET_exp(ir) != NULL) {
                note(getRegion(), "\n>> MDSet DETAIL:");
                dumpIRPointTo(RET_exp(ir), true, mx);
            }
            break;
        }
        case IR_PHI: {
            prt(getRegion(), "LHS:");
            dumpIRPointTo(ir, false, mx);

            for (IR * p = PHI_opnd_list(ir); p; p = p->get_next()) {
                dumpIRPointTo(p, false, mx);
            }

            ASSERT0(PHI_opnd_list(ir));
            if (dump_kid) {
                note(getRegion(), "\n>> MDSet DETAIL:\n");
                for (IR * p = PHI_opnd_list(ir); p; p = p->get_next()) {
                    dumpIRPointTo(p, true, mx);
                }
            }
            break;
        }
        case IR_REGION: break;
        default: UNREACHABLE();
        }
    }
    m_rg->getLogMgr()->decIndent(2);
}


//Dump all relations between IR, MD, and MDSet.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void AliasAnalysis::dumpIRPointToForRegion(bool dump_kid) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n==---- DUMP AliasAnalysis '%s' ----==",
         m_rg->getRegionName());
    m_md_sys->dump(false);

    note(getRegion(), "\n-- DUMP MAY-POINT-TO SET: --");
    ASSERT0(m_maypts);
    m_maypts->dump(m_md_sys, true);

    note(getRegion(), "\n-- DUMP IR POINT-TO: --");    
    BBList * bbl = m_cfg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        dumpIRPointToForBB(bb, dump_kid);
    }
}


void AliasAnalysis::dumpMayPointTo() const
{
    if (!m_rg->isLogMgrInit() || m_maypts == NULL) { return; }

    MDSetIter iter;
    for (INT j = m_maypts->get_first(&iter);
         j >= 0; j = m_maypts->get_next((UINT)j, &iter)) {
        MD * mmd = m_md_sys->getMD((UINT)j);
        ASSERT0(mmd != NULL);
        prt(getRegion(), "MD%u,", MD_id(mmd));
    }
}


bool AliasAnalysis::dump() const
{
    dumpMD2MDSetForRegion(false);
    return true;
}


//Dump MD's point-to for each BB.
void AliasAnalysis::dumpMD2MDSetForRegion(bool dump_pt_graph) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    if (m_flow_sensitive) {
        note(getRegion(),
             "\n==---- DUMP POINT-TO OUT-SET (FLOW SENSITIVE) '%s' ----==",
             m_rg->getRegionName());
        BBList * bbl = m_cfg->getBBList();
        for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
            note(getRegion(), "\n--- BB%u ---", bb->id());
            dumpMD2MDSet(mapBBtoMD2MDSet(bb->id()),
                        false); //each BB has its own graph.
        }
    } else {
        note(getRegion(),
             "\n==---- DUMP POINT-TO OUT-SET (FLOW-INSENSITIVE) '%s' ----==",
             m_rg->getRegionName());
        dumpMD2MDSet(&m_unique_md2mds, dump_pt_graph);
    }
}


//Dump MD's point-to according to individual 'mx'.
//'dump_ptg': dump POINT-TO graph.
void AliasAnalysis::dumpMD2MDSet(MD2MDSet const* mx, bool dump_ptg) const
{
    if (!m_rg->isLogMgrInit() || mx == NULL) { return; }
    xcom::Graph g;
    MDId2MD const* id2md = m_md_sys->getID2MDMap();
    for (INT i = MD_FIRST; i <= id2md->get_last_idx(); i++) {
        if (id2md->get((UINT)i) == NULL) { continue; }

        MDSet const* mds = getPointTo((UINT)i, *mx);
        if (mds != NULL) {
            note(getRegion(), "\nMD%u -- PT_SET: ", (UINT)i);
            MDSetIter iter;
            for (INT j = mds->get_first(&iter);
                 j >= 0; j = mds->get_next((UINT)j, &iter)) {
                ASSERT0(m_md_sys->getMD((UINT)j));
                prt(getRegion(), "MD%u,", (UINT)j);
                if (dump_ptg) {
                    g.addEdge((UINT)i, (UINT)j);
                }
            }
        } else {
            note(getRegion(), "\nMD%u -- NO PT", (UINT)i);
        }
    }

    if (dump_ptg) {
        g.dumpVCG("graph_point_to.vcg");
    }
}


//Dump relations between MD, MDSet.
//'md': candidate to dump.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void AliasAnalysis::dumpMD2MDSet(MD const* md, MD2MDSet const* mx) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    StrBuf buf(64);
    note(getRegion(), "\n%s", md->dump(buf, m_tm));

    //Dump MDSet of 'md'.
    MDSet const* pts = getPointTo(MD_id(md), *mx);
    m_rg->getLogMgr()->incIndent(2);
    note(getRegion(), "\nPOINT TO:");
    if (pts != NULL && !pts->is_empty()) {
        MDSetIter iter;
        m_rg->getLogMgr()->incIndent(2);
        for (INT j = pts->get_first(&iter);
             j >= 0; j = pts->get_next((UINT)j, &iter)) {
            MD const* mmd = m_md_sys->getMD((UINT)j);
            ASSERT0(mmd);
            buf.clean();
            note(getRegion(), "\n%s", mmd->dump(buf, m_tm));
        }
        note(getRegion(), "\n");
        m_rg->getLogMgr()->decIndent(2);
    } else {
        prt(getRegion(), "--");
    }
    m_rg->getLogMgr()->decIndent(2);
}


//Return false if flow sensitive analysis is inproperly.
bool AliasAnalysis::convertMD2MDSet2PT(OUT PtPairSet * pps,
                                       IN PtPairMgr & ppmgr,
                                       IN PPSetMgr & ppsetmgr,
                                       IN MD2MDSet * mx)
{
    //Grow pps before hand with the maximum length needed.
    if (mx->computePtPairNum(*m_md_sys) > g_thres_ptpair_num) {
        return false;
    }

    MD2MDSetIter mxiter;
    MDSet const* from_md_pts = NULL;
    for (UINT fromid = mx->get_first(mxiter, &from_md_pts);
         fromid > 0; fromid = mx->get_next(mxiter, &from_md_pts)) {
        ASSERT0(m_md_sys->getMD(fromid));
        if (from_md_pts == NULL) { continue; }
        if (from_md_pts->is_contain_fullmem()) {
            PtPair const* pp = ppmgr.add(fromid, MD_FULL_MEM);
            ASSERT0(pp);
            pps->bunion(PP_id(pp), *ppsetmgr.getSBSMgr());
        } else {
            MDSetIter segiter;
            for (INT toid = from_md_pts->get_first(&segiter);
                 toid >= 0;
                 toid = from_md_pts->get_next((UINT)toid, &segiter)) {
                ASSERT0(m_md_sys->getMD((UINT)toid));
                PtPair const* pp = ppmgr.add(fromid, (UINT)toid);
                ASSERT0(pp);
                pps->bunion(PP_id(pp), *ppsetmgr.getSBSMgr());
            }
        }
    }
    return true;
}


void AliasAnalysis::convertPT2MD2MDSet(PtPairSet const& pps,
                                       IN PtPairMgr & ppmgr,
                                       IN OUT MD2MDSet * ctx)
{
    PtPairSetIter iter;
    for (INT i = pps.get_first(&iter);
         i >= 0; i = pps.get_next((UINT)i, &iter)) {
        PtPair * pp = ppmgr.get((UINT)i);
        ASSERT0(pp != NULL);
        setPointToMDSetByAddMD(PP_from(pp), *ctx, m_md_sys->getMD(PP_to(pp)));
     }
}


//Solving POINT-TO out set.
//While the function terminiate, OUT info has been recorded
//in related 'mx'.
void AliasAnalysis::computeStmt(IRBB const* bb, IN OUT MD2MDSet * mx)
{
    ASSERT0(mx != NULL);
    IRListIter ct;
    IRBB * readonly_bb = const_cast<IRBB*>(bb); //ensure we do not moidy it.
    for (IR * ir = BB_irlist(readonly_bb).get_head(&ct);
         ir != NULL; ir = BB_irlist(readonly_bb).get_next(&ct)) {
        ASSERT0(isValidStmtToAA(ir));
        switch (ir->getCode()) {
        case IR_ST:            
            processStore(ir, mx);
            break;
        case IR_STPR:
            processStorePR(ir, mx);
            break;
        case IR_STARRAY:
            processStoreArray(ir, mx);
            break;
        case IR_SETELEM:
            processSetelem(ir, mx);
            break;
        case IR_GETELEM:
            processGetelem(ir, mx);
            break;
        case IR_IST:
            processIStore(ir, mx);
            break;
        case IR_CALL:
        case IR_ICALL:
            processCall(ir, mx);
            break;
        case IR_GOTO:
            ASSERT0(ir == BB_last_ir(readonly_bb));
            break;
        case IR_IGOTO: {
            ASSERT0(ir == BB_last_ir(readonly_bb));
            MDSet tmp;
            AACtx ic;
            inferExpression(IGOTO_vexp(ir), tmp, &ic, mx);
            tmp.clean(*getSBSMgr());
            break;
        }
        case IR_PHI:
            processPhi(ir, mx);
            break;
        case IR_REGION:
            processRegion(ir, mx);
            break;
        case IR_TRUEBR:
        case IR_FALSEBR: {
            ASSERT0(ir == BB_last_ir(readonly_bb));
            MDSet tmp;
            AACtx ic;
            inferExpression(BR_det(ir), tmp, &ic, mx);
            tmp.clean(*getSBSMgr());;
            break;
        }
        case IR_RETURN:
            ASSERT0(ir == BB_last_ir(readonly_bb));
            processReturn(ir, mx);
            break;
        case IR_SWITCH: {
            ASSERT0(ir == BB_last_ir(readonly_bb));
            MDSet tmp;
            AACtx ic;
            inferExpression(SWITCH_vexp(ir), tmp, &ic, mx);
            tmp.clean(*getSBSMgr());
            break;
        }
        default: ASSERTN(0, ("unsupported IR type"));
        }
    }
}


bool AliasAnalysis::verifyIR(IR * ir)
{
    switch (ir->getCode()) {
    case IR_ID:
    case IR_LD:
    case IR_PR:
        ASSERT0(ir->getMustRef());
        ASSERT0(ir->getMayRef() == NULL);
        break;
    case IR_ST:
        ASSERT0(ir->getMustRef());
        ASSERT0(ir->getMayRef() == NULL);
        break;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
        ASSERT0(ir->getMustRef());
        ASSERT0(ir->getMayRef() == NULL);
        break;
    case IR_STARRAY: {
        MD const* mustaddr = ir->getMustRef();
        MDSet const* mayaddr = ir->getMayRef();
        ASSERT0(mustaddr ||
                (mayaddr && !mayaddr->is_empty()));
        ASSERT0((mustaddr != NULL) ^
                (mayaddr && !mayaddr->is_empty()));
        if (mustaddr != NULL) {
            //PR's address can not be taken.
            ASSERT0(!mustaddr->is_pr());
        }
        if (mayaddr != NULL && !mayaddr->is_empty()) {
            //PR's address can not be taken.
            MDSetIter iter;
            for (INT i = mayaddr->get_first(&iter);
                 i >= 0; i = mayaddr->get_next((UINT)i, &iter)) {
                MD const* x = m_md_sys->getMD((UINT)i);
                CHECK_DUMMYUSE(x);
                ASSERT0(!x->is_pr());
            }
        }
        break;
    }
    case IR_ARRAY:
        ASSERT0(ir->getParent());
        if (ir->getParent()->is_array()) {
            //Compute the memory address and ONLY
            //record the top level ARRAY node's memory address.
            break;
        }
        //fallthrough
    case IR_ILD:
    case IR_IST: {
        MD const* mustaddr = ir->getMustRef();
        MDSet const* mayaddr = ir->getMayRef();
        ASSERT0(mustaddr ||
                (mayaddr && !mayaddr->is_empty()));
        ASSERT0((mustaddr != NULL) ^
                (mayaddr && !mayaddr->is_empty()));
        if (mustaddr != NULL) {
            //PR's address can not be taken.
            ASSERT0(!mustaddr->is_pr());
        }
        if (mayaddr != NULL && !mayaddr->is_empty()) {
            //PR's address can not be taken.
            MDSetIter iter;
            for (INT i = mayaddr->get_first(&iter);
                 i >= 0; i = mayaddr->get_next((UINT)i, &iter)) {
                MD const* x = m_md_sys->getMD((UINT)i);
                CHECK_DUMMYUSE(x);
                ASSERT0(!x->is_pr());
            }
        }
        break;
    }
    case IR_CALL:
    case IR_ICALL:
        if (ir->hasReturnValue()) {
            ASSERT0(ir->getMustRef());
        }
        for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
            verifyIR(p);
        }
        for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
            verifyIR(p);
        }
        break;
    case IR_PHI:
        ASSERT0(ir->getMustRef());
        ASSERT0(ir->getMayRef() == NULL);
        break;
    default:
        ASSERT0(ir->getMustRef() == NULL);
        ASSERT0(ir->getMayRef() == NULL);
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != NULL) {
            verifyIR(kid);
        }
    }
    return true;
}


bool AliasAnalysis::verify()
{
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head();
         bb != NULL; bb = bbl->get_next()) {
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            verifyIR(ir);
        }
    }
    return true;
}

#define PARTIAL_UPDATE

//This method is accurate than Andersen's algo.
//NOTICE: Do NOT clean 'md2mds' of BB at the last iter,
//it supplied the POINT TO information for subsequently
//optimizations.
//Return false if flow sensitive analysis is inproperly.
bool AliasAnalysis::computeFlowSensitive(List<IRBB*> const& bbl,
                                         PPSetMgr & ppsetmgr)
{
    bool change = true;
    UINT count = 0;
    xcom::BitSet is_bb_changed(m_rg->getBBMgr()->getBBCount() /
                               BITS_PER_BYTE + 1);    
    PtPairSet * tmp = ppsetmgr.allocPtPairSet();
    DefMiscBitSetMgr * sbsmgr = ppsetmgr.getSBSMgr();
    is_bb_changed.set(0xFF);
    for (; change && count < 20;) {
        count++;        
        bool first = count == 1;
        change = false;
        BBListIter ct = NULL;
        for (IRBB const* bb = bbl.get_head(&ct);
             bb != NULL; bb = bbl.get_next(&ct)) {
            #ifdef PARTIAL_UPDATE
            if (!is_bb_changed.is_contain(bb->id())) { continue; }
            #endif

            PtPairSet * pps = getInPtPairSet(bb);
            MD2MDSet * md2mds = genMD2MDSetForBB(bb->id());
            tmp->clean(*sbsmgr);
            xcom::EdgeC * el = m_cfg->getVertex(bb->id())->getInList();
            bool compute = true;
            if (el != NULL) {
                for (; el != NULL; el = el->get_next()) {
                    IRBB * p = m_cfg->getBB(el->getFromId());
                    ASSERT0(p);
                    tmp->bunion(*getOutPtPairSet(p), *sbsmgr);
                }

                #ifdef PARTIAL_UPDATE

                compute = !pps->is_equal(*tmp);
                if (compute || first) {
                    //BB's info may not be changed even if input changed.
                    pps->copy(*tmp, *sbsmgr);
                    change = true;

                    //Regard 'mx' as a reservation table to hold
                    //the tmp info for MD->MDSet during each iteration.
                    //Note that we must preserve MD->MDSet at the last
                    //iteration. And it will be used during computing POINT-TO
                    //info and DU analysis.
                    md2mds->clean();
                    convertPT2MD2MDSet(*pps, m_ppmgr, md2mds);
                } else {
                    is_bb_changed.diff(bb->id());
                }
                
                #else                

                if (!pps->is_equal(*tmp)) {
                    pps->copy(*tmp, *sbsmgr);
                    change = true;

                    //Regard 'mx' as a reservation table to hold
                    //the tmp info for MD->MDSet during each iteration.
                    //Note that we must preserve MD->MDSet at the last
                    //iteration. And it will be used during computing POINT-TO
                    //info and DU analysis.
                    md2mds->clean();
                    convertPT2MD2MDSet(*pps, m_ppmgr, md2mds);
                }                
                #endif
            }

            if (!compute && !first) { continue; }
            computeStmt(bb, md2mds);
            tmp->clean(*sbsmgr);
            if (!convertMD2MDSet2PT(tmp, m_ppmgr, ppsetmgr, md2mds)) {
                return false;
            }
        
            #ifdef _DEBUG_
            //MD2MDSet x;
            //convertPT2MD2MDSet(tmp, m_ppmgr, &x);            
            //dumpMD2MDSet(&x, false);
            #endif
            pps = getOutPtPairSet(bb);
            if (!pps->is_equal(*tmp)) {
                pps->copy(*tmp, *sbsmgr);
                change = true;                
                for (xcom::EdgeC * el = m_cfg->getVertex(bb->id())->
                        getOutList();
                     el != NULL; el = el->get_next()) {
                    IRBB * s = m_cfg->getBB(el->getToId());
                    ASSERT0(s);
                    is_bb_changed.bunion(s->id());
                }
            }            
        } //for each BB
    } //each iter
    ASSERTN(!change, ("Iterate too many times"));
    return true;
}


//This function initialize the POINT-TO set of pointer.
//The pointer includes global pointer and formal parameter pointer.
//'param': formal parameter.
//Note May-POINT-TO set must be available before call this function.
void AliasAnalysis::initGlobalAndParameterVarPtSet(Var * param, MD2MDSet * mx)
{
    MD const* dmd = NULL; //record dedicated MD which parameter pointed to.
    if (VAR_is_restrict(param)) {
        //If parameter is restrict, we allocate an individual variable to
        //distinguish its point-to set with other parameters.
        dmd = m_var2md.get(param);
        if (dmd == NULL) {
            CHAR name[64];
            SNPRINTF(name, 63, "DummyGlobalVarPointedByVAR%u", param->id());
            ASSERT0(::strlen(name) < 64);
            Var * tv = m_rg->getVarMgr()->registerVar(name,
                m_tm->getMCType(0), 0, VAR_GLOBAL|VAR_ADDR_TAKEN);

            //Set the var to be unallocable, means do NOT add
            //var immediately as a memory-variable.
            //For now, it is only be regarded as a pseduo-register.
            //And set it to allocable if the PR is in essence need to be
            //allocated in memory.
            VAR_is_unallocable(tv) = true;
            m_rg->addToVarTab(tv);

            MD md;
            MD_base(&md) = tv;
            MD_ofst(&md) = 0;
            MD_size(&md) = 16; //it is just placeholder, anysize you want.
            MD_ty(&md) = MD_EXACT;
            dmd = m_md_sys->registerMD(md);
            ASSERT0(MD_id(dmd) > 0);
            m_var2md.set(param, dmd);
        }
    }

    MDTab * mdt = m_md_sys->getMDTab(param);
    if (mdt != NULL) {
        MD const* x = mdt->get_effect_md();
        if (x != NULL) {
            if (dmd != NULL) {
                ASSERTN(getPointTo(MD_id(x), *mx) == NULL ||
                        getPointTo(MD_id(x), *mx)->is_empty(),
                        ("should already be clean"));
                setPointToMDSetByAddMD(MD_id(x), *mx, dmd);
            } else {
                setPointTo(MD_id(x), *mx, m_maypts);
            }
        }

        OffsetTab * ofstab = mdt->get_ofst_tab();
        ASSERT0(ofstab);
        if (ofstab->get_elem_count() > 0) {
            ConstMDIter iter;
            for (MD const* md = ofstab->get_first(iter, NULL);
                 md != NULL; md = ofstab->get_next(iter, NULL)) {
                if (dmd != NULL) {
                    ASSERTN(getPointTo(MD_id(md), *mx) == NULL ||
                            getPointTo(MD_id(md), *mx)->is_empty(),
                            ("should already be clean"));
                    setPointToMDSetByAddMD(MD_id(md), *mx, dmd);
                } else {
                    setPointTo(MD_id(md), *mx, m_maypts);
                }
            }
        }
        return;
    }

    MD md;
    MD_base(&md) = param;
    MD_ofst(&md) = 0;
    MD_size(&md) = param->getByteSize(m_tm);
    MD_ty(&md) = MD_EXACT;
    MD const* entry = m_md_sys->registerMD(md);
    if (dmd != NULL) {
        ASSERTN(getPointTo(MD_id(entry), *mx) == NULL ||
                getPointTo(MD_id(entry), *mx)->is_empty(),
                ("should already be clean"));
        setPointToMDSetByAddMD(MD_id(entry), *mx, dmd);
    } else {
        setPointTo(MD_id(entry), *mx, m_maypts);
    }
}


//Determine if flow sensitive analysis is properly.
bool AliasAnalysis::isFlowSensitiveProperly()
{
    ASSERT0(m_cfg->getEntry());
    MD2MDSet * mx = genMD2MDSetForBB(m_cfg->getEntry()->id());
    ASSERTN(mx, ("invoke initEntryPtset before here"));
    UINT num_of_tgt_md = mx->computePtPairNum(*m_md_sys);
    num_of_tgt_md = (num_of_tgt_md * mx->get_elem_count() /
                    HOST_BIT_PER_BYTE + 1) *
                    HOST_BIT_PER_BYTE / HOST_BIT_PER_BYTE;
    return num_of_tgt_md < g_thres_ptpair_num;
}


void AliasAnalysis::initBBPPSet(PPSetMgr & ppsetmgr)
{
    BBList * bblst = m_rg->getBBList();
    //Make vector to accommodate the maximum BB id.
    UINT bbnum = bblst->get_elem_count();
    if (m_in_pp_set.get_capacity() < bbnum) {
        m_in_pp_set.grow(bbnum);
        m_out_pp_set.grow(bbnum);
        m_md2mds_vec.grow(bbnum);
    }
    
    BBListIter ct;
    for (IRBB * bb = bblst->get_head(&ct);
         bb != NULL; bb = bblst->get_next(&ct)) {
        m_in_pp_set.set(bb->id(), ppsetmgr.allocPtPairSet());
        m_out_pp_set.set(bb->id(), ppsetmgr.allocPtPairSet());
    }
}


void AliasAnalysis::initFlowSensitiveEntryPtset(PPSetMgr & ppsetmgr)
{
    ASSERT0(m_cfg->verify());
    ASSERT0(m_rg->getBBList()->get_elem_count() != 0);
    VarTab * vt = m_rg->getVarTab();
    initBBPPSet(ppsetmgr);

    IRBB * entry = m_cfg->getEntry();
    ASSERT0(entry);
    MD2MDSet * mx = genMD2MDSetForBB(entry->id());
    setPointToAllMem(MD_FULL_MEM, *mx);
    setPointToGlobalMem(MD_GLOBAL_VAR, *mx);
    setPointToImportVar(MD_GLOBAL_VAR, *mx);
    setPointToGlobalMem(MD_IMPORT_VAR, *mx);
    setPointToImportVar(MD_IMPORT_VAR, *mx);

    //Initialize POINT-TO set for global Var and parameter Var.
    VarTabIter c;
    for (Var * v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
        if (!VAR_is_global(v) && !VAR_is_formal_param(v)) { continue; }
        if (!v->is_pointer()) { continue; }

        //Variable with void-type may be pointer.
        //Deal with its point-to set while processing Load/PR
        //instead of initializing point-set at once.
        //if (!v->is_pointer() && !v->is_any()) { continue; }

        initGlobalAndParameterVarPtSet(v, mx);
    }
    convertMD2MDSet2PT(getInPtPairSet(entry), m_ppmgr, ppsetmgr, mx);
}


//Initialize POINT_TO set for input MD at the entry of Region.
//e.g:char * q;
//    void f(int * p)
//    {
//        *q='c';
//        *p=0;
//    }
//    where p and q are entry MD.
//ptset_arr: used to record all the PtPair set. It will be deleted by caller.
void AliasAnalysis::initEntryPtset(PPSetMgr & ppsetmgr)
{    
    if (m_flow_sensitive) {
        initFlowSensitiveEntryPtset(ppsetmgr);
        return;
    }
    VarTab * vt = m_rg->getVarTab();
    //Flow insenstive initialzation.
    setPointToAllMem(MD_FULL_MEM, m_unique_md2mds);
    setPointToGlobalMem(MD_GLOBAL_VAR, m_unique_md2mds);
    setPointToImportVar(MD_GLOBAL_VAR, m_unique_md2mds);
    setPointToGlobalMem(MD_IMPORT_VAR, m_unique_md2mds);
    setPointToImportVar(MD_IMPORT_VAR, m_unique_md2mds);

    //Initialize POINT-TO set for global Var and parameter Var.
    VarTabIter c;
    for (Var * v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
        if (!VAR_is_global(v) && !VAR_is_formal_param(v)) { continue; }
        if (!v->is_pointer()) { continue; }

        //Variable with void-type may be pointer.
        //Deal with its point-to set while processing Load/PR
        //instead of initializing point-set at once.
        //if (!v->is_pointer() && !v->is_any()) { continue; }

        initGlobalAndParameterVarPtSet(v, &m_unique_md2mds);
    }
}


//This function initialize May Point-To set.
//Note that this function should only be invoked once.
void AliasAnalysis::initMayPointToSet()
{
    //Record MDs whose address have been takens or it is global variable.
    Region * rg = m_rg;
    RegionMgr * rm = m_rg->getRegionMgr();
    VarTabIter c;
    ConstMDIter iter;
    MDSet tmp;
    for (; !rg->is_program();) {
        VarTab * vtab = rg->getVarTab();
        c.clean();
        for (Var * v = vtab->get_first(c); v != NULL; v = vtab->get_next(c)) {
            if (!VAR_is_addr_taken(v)) { continue; }
            ASSERT0(!v->is_global());

            //Handle dedicated string md which has been taken address.
            MD const* strmd = NULL;
            if (v->is_string() &&
                (strmd = rm->genDedicateStrMD()) != NULL) {
                tmp.bunion_pure(MD_id(strmd), *getSBSMgr());
                continue;
            }

            //General md.
            ASSERT0(m_md_sys->getMDTab(v));
            MD const* x = m_md_sys->getMDTab(v)->get_effect_md();
            if (x != NULL) {
                //Record effect MD of v into the MayPointTo if exist.
                ASSERT0(x->is_unbound() || x->is_range());
                tmp.bunion(x, *getSBSMgr());
            } else {
                MD md;
                MD_base(&md) = v;
                MD_ty(&md) = MD_UNBOUND;
                MD const* entry = m_md_sys->registerMD(md);
                ASSERT0(MD_id(entry) > 0);
                tmp.bunion(entry, *getSBSMgr());
            }

            //Record each exact MD of v into the MayPointTo.
            OffsetTab * ofstab = m_md_sys->getMDTab(v)->get_ofst_tab();
            ASSERT0(ofstab);
            if (ofstab->get_elem_count() > 0) {
                iter.clean();
                for (MD const* md = ofstab->get_first(iter, NULL);
                     md != NULL; md = ofstab->get_next(iter, NULL)) {
                    ASSERT0(md->is_exact() || md->is_range());
                    tmp.bunion(md, *getSBSMgr());
                }
            }
        }

        rg = rg->getParent();
        if (rg->is_program() || rg == NULL) {
            break;
        }
        ASSERT0(rg->is_inner() || rg->is_function() || rg->is_eh());
    }

    tmp.bunion(MD_GLOBAL_VAR, *getSBSMgr());
    tmp.bunion(MD_IMPORT_VAR, *getSBSMgr());
    m_maypts = m_mds_hash->append(tmp);
    tmp.clean(*getSBSMgr());
}


void AliasAnalysis::computeFlowInsensitive()
{
    BBList * bbl = m_cfg->getBBList();
    BBListIter ct = NULL;
    UINT c = 0;
    while (++c < 3) {
        //Compute point-to.
        //Compute which MD memory-op represented.
        for (IRBB const* bb = bbl->get_head(&ct);
             bb != NULL; bb = bbl->get_next(&ct)) {
            computeStmt(bb, &m_unique_md2mds);
        }
    }
}


//Initialize alias analysis.
void AliasAnalysis::initAliasAnalysis()
{
    ASSERTN(!is_init(), ("already initialized"));
    initMayPointToSet();
    set_flow_sensitive(true);
}


//Calculate point-to set.
bool AliasAnalysis::perform(IN OUT OptCtx & oc)
{
    ASSERTN(m_maypts, ("Should invoke initAliasAnalysis() first."));
    if (m_rg->getBBList()->get_elem_count() == 0) { return true; }
    START_TIMER(t, getPassName());

    //Initialization.
    m_ppmgr.init();

    //Clean data structures used for analysis.
    clean();

    //We allocate PtPairSet at each call of AA,
    //because AA would not be invoked frequently.    
    if (m_flow_sensitive) {
        PPSetMgr ppsetmgr;
        m_rg->checkValidAndRecompute(&oc, PASS_LOOP_INFO, PASS_UNDEF);
        ASSERTN(OC_is_loopinfo_valid(oc),
                ("infer pointer arith need loop info"));
        initEntryPtset(ppsetmgr);
        m_rg->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
        List<IRBB*> * tbbl = m_cfg->getRPOBBList();
        ASSERT0(tbbl);
        ASSERT0(tbbl->get_elem_count() == m_rg->getBBList()->get_elem_count());

        START_TIMER_FMT(t2, ("%s:flow sensitive analysis", getPassName()));
        bool is_succ = computeFlowSensitive(*tbbl, ppsetmgr);
        END_TIMER_FMT(t2, ("%s:flow sensitive analysis", getPassName()));

        if (!is_succ) {
            //Flow sensitive is inproperly, perform insensitive analysis.
            m_flow_sensitive = false;
            START_TIMER_FMT(t4, ("%s:flow insensitive analysis",
                                 getPassName()));
            initEntryPtset(ppsetmgr);
            computeFlowInsensitive();
            END_TIMER_FMT(t4, ("%s:flow insensitive analysis", getPassName()));
        }
    } else {
        PPSetMgr ppsetmgr;
        START_TIMER_FMT(t3, ("%s:flow insensitive analysis", getPassName()));
        initEntryPtset(ppsetmgr);
        computeFlowInsensitive();
        END_TIMER_FMT(t3, ("%s:flow insensitive analysis", getPassName()));
    }
    OC_is_aa_valid(oc) = true;

    if (g_is_dump_after_pass && g_dump_opt.isDumpAA()) {
        note(getRegion(), "\n==---- DUMP %s '%s' ----==", getPassName(),
             m_rg->getRegionName());
        m_md_sys->dump(false);
        dumpMD2MDSetForRegion(false);
        //dumpInOutPointToSetForBB();
        dumpIRPointToForRegion(true);
    }
    ASSERT0(verify());

    //DU info does not depend on these data structures.
    //Since AA is not always used, we destroy the data
    //structure to release memory.
    m_ppmgr.clean();
    cleanSBSMgr();
    END_TIMER(t, getPassName());
    return true;
}

} //namespace xoc
