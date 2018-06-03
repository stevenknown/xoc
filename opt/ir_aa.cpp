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
#include "prdf.h"
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
size_t PPSetMgr::count_mem()
{
    UINT count = 0;
    for (xcom::SC<PtPairSet*> * sc = m_pp_set_list.get_head();
         sc != m_pp_set_list.end(); sc = m_pp_set_list.get_next(sc)) {
        PtPairSet * pps = sc->val();
        ASSERT0(pps);
        count += pps->count_mem();
    }
    count += m_pp_set_list.count_mem();
    count += m_free_pp_set.count_mem();
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


//Return true if all element in mds are effect and derived from the same VAR.
//mustref: record the Unbound MD if the function return true, or meaningless.
static bool isAllElementDerivedFromSameEffectVar(
        MDSet const& mds,
        MDSystem * mdsys,
        MD const** mustref)
{
    ASSERT0(!mds.is_empty() && mustref);

    SEGIter * iter;
    INT i = mds.get_first(&iter);
    MD const* md = mdsys->getMD((UINT)i);
    if (!md->is_effect() || MD_is_may(md)) {
        return false;
    }

    VAR * base = md->get_base();
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
//START IR_AA
//
IR_AA::IR_AA(Region * rg)
{
    ASSERT0(rg);
    m_cfg = rg->getCFG();
    m_var_mgr = rg->getVarMgr();
    m_ru = rg;
    m_rumgr = rg->getRegionMgr();
    m_tm = rg->getTypeMgr();
    m_md_sys = rg->getMDSystem();
    m_mds_mgr = rg->getMDSetMgr();
    m_mds_hash = rg->getMDSetHash();
    m_misc_bs_mgr = rg->getMiscBitSetMgr();
    ASSERT0(m_cfg && m_mds_hash && m_md_sys && m_tm && m_mds_mgr);
    m_flow_sensitive = true;
    m_pool = smpoolCreate(128, MEM_COMM);
    m_dummy_global = NULL;
    m_maypts = NULL;
}


IR_AA::~IR_AA()
{
    OptCtx oc;
    destroyContext(oc);
    smpoolDelete(m_pool);
}


size_t IR_AA::count_mem()
{
    size_t count = 0;
    count += sizeof(m_cfg);
    count += sizeof(m_var_mgr);
    count += sizeof(m_ru);
    count += m_in_pp_set.count_mem();
    count += m_out_pp_set.count_mem();
    count += smpoolGetPoolSize(m_pool);
    count += sizeof(m_mds_mgr);

    count += m_pt_pair_mgr.count_mem();

    if (m_maypts != NULL) {
        count += m_maypts->count_mem();
    }

    count += countMD2MDSetMemory();
    count += m_id2heap_md_map.count_mem();
    count += m_unique_md2mds.count_mem();
    count += sizeof(BYTE);
    return count;
}


size_t IR_AA::countMD2MDSetMemory()
{
    size_t count = 0;
    BBList * bbl = m_ru->getBBList();
    MD2MDSetIter iter;
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        MD2MDSet * mx = m_md2mds_vec.get(BB_id(bb));
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
void IR_AA::destroyContext(OptCtx & oc)
{
    for (INT i = 0; i <= m_md2mds_vec.get_last_idx(); i++) {
        MD2MDSet * mx = m_md2mds_vec.get((UINT)i);
        if (mx == NULL) { continue; }
        mx->destroy();
    }
    OC_is_aa_valid(oc) = false;
}


//Clean but not destory context data structures.
void IR_AA::cleanContext(OptCtx & oc)
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
void IR_AA::clean()
{
    m_is_visit.clean();
    m_in_pp_set.clean();
    m_out_pp_set.clean();
    m_unique_md2mds.clean();

    OptCtx oc;
    cleanContext(oc);
    m_id2heap_md_map.clean();
}


//MD size should be determined by 'pointer base type' of a pointer.
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
void IR_AA::reviseMDsize(IN OUT MDSet & mds, UINT size)
{
    ASSERT0(size > 0);
    Vector<MD const*> res;
    UINT num = 0;
    bool change = false;
    SEGIter * iter;
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
        mds.clean(*m_misc_bs_mgr);
        for (INT i = ((INT)num) - 1; i >= 0; i--) {
            mds.bunion(res.get((UINT)i), *m_misc_bs_mgr);
        }
    }
}


//Return true if IR is a valid statement that could be handled by AA.
//NOTICE: High level control flow or similar statements are unacceptable here.
bool IR_AA::isValidStmtToAA(IR * ir)
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
//Note this function does not handle array's LDA base.
//e.g: y = &x or y = &a[i]
//'mds' : record output memory descriptor of 'ir'
void IR_AA::processLda(IR * ir, IN OUT MDSet & mds, IN OUT AACtx * ic)
{
    ASSERT0(ir->is_lda() && ir->is_ptr());
    ASSERT0(ic);

    MD const* t = NULL;
    VAR * v = LDA_idinfo(ir);
    if (v->is_string()) {
        t = allocStringMD(v->get_name());
    } else {
        t = m_ru->genMDforVAR(v);
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

    mds.clean(*m_misc_bs_mgr);
    mds.bunion(t, *m_misc_bs_mgr);

    //Inform the caller that there is MD has been taken address.
    AC_has_comp_lda(ic) = true;

    if (!AC_is_mds_mod(ic) && LDA_ofst(ir) == 0) { return; }

    if ((LDA_ofst(ir) != 0 && t->is_exact()) || ir->getParent()->is_array()) {
        //If LDA is array base, and LDA ofst may not be 0.
        //e.g: struct S { int a; int b[..]; }
        //    access s.b[..]
        MD md(*t);
        MD_ofst(&md) += LDA_ofst(ir);

        if (ir->getParent()->is_array() &&
            ((CArray*)ir->getParent())->is_base(ir) &&
            !ir->getParent()->is_void() &&
            t->is_exact()) {
            //The result data type of LDA will amended to be the type of
            //array element if array is the field of D_MC.
            //e.g: struct S { int a; int b[..]; }
            //    access s.b[..] generate ARRAY(LDA(s, ofst(4))
            UINT elem_sz = ir->getParent()->getTypeSize(m_tm);
            ASSERT0(elem_sz > 0);
            MD_size(&md) = elem_sz;
        }

        if (!t->is_equ(md)) {
            MD const* entry = m_md_sys->registerMD(md);
            ASSERT0(MD_id(entry) > 0);
            ASSERT0(t->is_effect() && entry->is_effect());
            mds.clean(*m_misc_bs_mgr);
            mds.bunion_pure(MD_id(entry), *m_misc_bs_mgr);
        }
    }
}


//Convert type-size.
//e.g: int a; char b;
//    a = (int)b
//'mds' : record memory descriptor of 'ir'.
void IR_AA::processCvt(
        IR const* ir,
        IN OUT MDSet & mds,
        OUT AACtx * ic,
        OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_cvt());
    inferExpression(CVT_exp(ir), mds, ic, mx);
    if (AC_is_mds_mod(ic)) {
        SEGIter * iter;
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

                mds.diff((UINT)i, *m_misc_bs_mgr);
                mds.bunion_pure(MD_id(entry), *m_misc_bs_mgr);
            }
        }
    }
}


//Infer the unbounded set.
void IR_AA::inferArrayInfinite(
        INT ofst,
        bool is_ofst_pred,
        UINT md_size,
        MDSet const& in,
        OUT MDSet & out)
{
    SEGIter * iter;
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
            out.bunion(org, *m_misc_bs_mgr);
            continue;
        }

        MD const* entry = m_md_sys->registerMD(tmd);
        ASSERT0(MD_id(entry) > 0);
        out.bunion(entry, *m_misc_bs_mgr);
    }
}


//Get to know where the pointer pointed to.
//This function will not clean 'mds' since user may be perform union operation.
void IR_AA::computeMayPointTo(IR * pointer, OUT MDSet & mds)
{
    ASSERT0(pointer && (pointer->is_ptr() || pointer->is_void()));

    //Get context.
    MD2MDSet * mx = NULL;
    if (m_flow_sensitive) {
        ASSERT0(pointer->getStmt() && pointer->getStmt()->getBB());
        mx = mapBBtoMD2MDSet(BB_id(pointer->getStmt()->getBB()));
    } else  {
        mx = &m_unique_md2mds;
    }
    computeMayPointTo(pointer, mx, mds);
}


//Get to know where the pointer pointed to.
//This function will not clean 'mds' since caller may
//perform union operations to 'mds'.
//mx: may be NULL.
void IR_AA::computeMayPointTo(IR * pointer, IN MD2MDSet * mx, OUT MDSet & mds)
{
    ASSERT0(pointer && (pointer->is_ptr() || pointer->is_void()));

    if (pointer->is_lda()) {
        AACtx ic;
        AC_comp_pt(&ic) = true;
        MDSet tmp;
        processLda(pointer, tmp, &ic);
        ASSERT0(tmp.get_elem_count() == 1);

        SEGIter * iter;
        MD const* t = m_md_sys->getMD((UINT)tmp.get_first(&iter));
        ASSERT0(t);
        if (t->is_exact()) {
            //Adjust MD of LDA to be unbound.
            MD t2(*t);
            MD_ty(&t2) = MD_UNBOUND;
            MD const* entry = m_md_sys->registerMD(t2);
            ASSERT0(MD_id(entry) > 0);
            t = entry;
        }

        mds.bunion(t, *m_misc_bs_mgr);
        tmp.clean(*m_misc_bs_mgr);
        return;
    }

    if (mx == NULL) {
        //We do NOT known where p pointed to.
        //e.g: (int*)0x1234
        mds.bunion(*m_maypts, *m_misc_bs_mgr);
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
                mds.bunion(typed_md, *m_misc_bs_mgr);
            } else {
                mds.bunion(*ptset, *m_misc_bs_mgr);
            }
        } else if (pointer->getAI() != NULL &&
                   (typed_md = computePointToViaType(pointer)) != NULL) {
            mds.bunion(typed_md, *m_misc_bs_mgr);
        } else {
            //We do NOT known where p pointed to.
            mds.bunion(*m_maypts, *m_misc_bs_mgr);
        }
    }

    MDSet const* maymds = pointer->getRefMDSet();
    if (maymds != NULL) {
        //pointer has a set of may-referenced-MDs.
        SEGIter * iter;
        for (INT i = maymds->get_first(&iter);
             i >= 0; i = maymds->get_next((UINT)i, &iter)) {
            MDSet const* ptset = getPointTo((UINT)i, *mx);
            MD const* typed_md = NULL;
            if (ptset != NULL && !ptset->is_empty()) {
                if (pointer->getAI() != NULL &&
                    ptset->is_contain_global() &&
                    (typed_md = computePointToViaType(pointer)) != NULL) {
                    mds.bunion(typed_md, *m_misc_bs_mgr);
                } else {
                    mds.bunion(*ptset, *m_misc_bs_mgr);
                }
            } else if (pointer->getAI() != NULL &&
                       (typed_md = computePointToViaType(pointer)) != NULL) {
                mds.bunion(typed_md, *m_misc_bs_mgr);
            } else {
                //We do NOT known where p pointed to.
                mds.bunion(*m_maypts, *m_misc_bs_mgr);
            }
        }
    }

    if (emd == NULL && maymds == NULL) {
        //We do NOT known where p pointed to.
        //e.g: (int*)0x1234
        mds.bunion(*m_maypts, *m_misc_bs_mgr);
    }
}


//The function compute may memory address or point-to set for array operation.
//'ir': array|starray operator.
//'array_base': base of array, must be pointer type.
//'is_ofst_predicable': true if array element offset is constant.
//This function will set the Ref MD and Ref MD set of array operation.
void IR_AA::inferArrayExpBase(
        IR * ir,
        IR * array_base,
        bool is_ofst_predicable,
        UINT ofst,
        OUT MDSet & mds,
        OUT bool * mds_is_may_pt,
        IN OUT AACtx * ic,
        IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->isArrayOp() && (array_base->is_ptr() || array_base->is_void()));
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
            mds.clean(*m_misc_bs_mgr);
            mds.bunion(typed_md, *m_misc_bs_mgr);
        } else {
            mds.copy(*m_maypts, *m_misc_bs_mgr);
            *mds_is_may_pt = true;
        }
    } else {
        //Intent to look for original array base that the base-pointer
        //pointed to.
        //e.g: (ld(p))[i], looking for where p pointed to.
        //Each MD in 'tmp' should be pointer.
        mds.clean(*m_misc_bs_mgr);
        UINT mdsz = ir->getTypeSize(m_tm);
        SEGIter * iter;
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
                    mds.clean(*m_misc_bs_mgr);
                    mds.bunion(typed_md, *m_misc_bs_mgr);
                } else {
                    inferArrayInfinite((INT)ofst,
                        is_ofst_predicable, mdsz, *pts, mds);
                }

            } else if (array_base->getAI() != NULL &&
                       (typed_md = computePointToViaType(array_base)) != NULL) {
                mds.bunion(typed_md, *m_misc_bs_mgr);
            } else {
                inferArrayInfinite((INT)ofst, false, 0, *m_maypts, mds);
                *mds_is_may_pt = true;
                break;
            }
        }
    }

    //Set ir's ref-md and ref-mdset.
    AC_is_mds_mod(ic) = true;
    SEGIter * iter;
    MD const* x = NULL;
    if (mds.get_elem_count() == 1 &&
        !MD_is_may(x = m_md_sys->getMD((UINT)mds.get_first(&iter)))) {
        setMustAddr(ir, m_md_sys->getMD((UINT)mds.get_first(&iter)));
        ir->cleanRefMDSet();
    } else {
        ir->cleanRefMD();
        setMayAddr(ir, m_mds_hash->append(mds));
    }

    tmp.clean(*m_misc_bs_mgr);
}


//This function infer array element memory address according to
//the LDA base of array operation.
//This function will set the Ref MD and Ref MD set of array operation.
void IR_AA::inferArrayLdabase(
        IR * ir,
        IR * array_base,
        bool is_ofst_pred,
        UINT ofst,
        OUT MDSet & mds,
        IN OUT AACtx * ic)
{
    ASSERT0(ir->isArrayOp() && array_base->is_lda());
    AACtx tic(*ic);
    processLda(array_base, mds, &tic);

    if (AC_is_mds_mod(&tic)) {
        ir->cleanRefMDSet();

        //Compute the MD size and offset if 'ofst' is constant.
        SEGIter * iter;
        ASSERT0(mds.get_elem_count() == 1);
        MD const* org = m_md_sys->getMD((UINT)mds.get_first(&iter));
        if (!org->is_exact()) {
            setMustAddr(ir, org);
            return;
        }

        bool changed = false;
        MD tmd(*org);
        if (is_ofst_pred) {
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
            changed = true;
            UINT basesz = m_tm->getPointerBaseByteSize(array_base->getType());
            ASSERT0(basesz);

            MD_ofst(&tmd) = LDA_ofst(array_base);

            //The approximate and conservative size of array.
            MD_size(&tmd) = basesz - LDA_ofst(array_base);
            MD_ty(&tmd) = MD_RANGE;
        }

        if (changed) {
            MD const* entry = m_md_sys->registerMD(tmd);
            ASSERT0(entry->is_effect());
            ASSERT0(MD_id(entry) > 0);

            mds.clean(*m_misc_bs_mgr);
            mds.bunion_pure(MD_id(entry), *m_misc_bs_mgr);

            setMustAddr(ir, entry);
            return;
        }

        //mds is unchanged.
        setMustAddr(ir, org);
    } else {
        ASSERT(getMayAddr(ir) == NULL, ("have no mayaddr"));
        MD const* x = getMustAddr(ir);
        ASSERT0(x && x->is_effect());
        mds.clean(*m_misc_bs_mgr);
        mds.bunion_pure(MD_id(x), *m_misc_bs_mgr);
    }
}


//Compute the memory address and ONLY record the top level
//ARRAY node's memory address.
//'mds' : record memory descriptor of 'ir'.
//This function will set the Ref MD and Ref MD set of array operation.
void IR_AA::processArray(
        IR * ir,
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
    UINT ofst_val = 0;
    bool is_ofst_predicable = ir->calcArrayOffset(&ofst_val, m_tm);
    bool mds_is_may_pt = false;
    AACtx tic;
    tic.copyTopDownFlag(*ic);
    AC_comp_pt(&tic) = false;

    if (array_base->is_lda()) {
        //Array base is LDA operation.
        inferArrayLdabase(ir, array_base, is_ofst_predicable,
                          ofst_val, mds, &tic);
    } else {
        //Array base is a computational expression.
        inferArrayExpBase(ir, array_base, is_ofst_predicable,
                          ofst_val, mds, &mds_is_may_pt,
                          &tic, mx);
    }
    ic->copyBottomUpFlag(tic);

    ASSERT0(!mds.is_empty());
    if (AC_comp_pt(ic)) {
        //Caller need array's point-to.
        if (mds_is_may_pt) {
            //Do not recompute again. Return mds directly.
            return;
        }

        //Compute the POINT-TO of array element.
        MDSet tmp;
        bool has_unified_may_pt = false;
        SEGIter * iter;
        for (INT i = mds.get_first(&iter);
             i >= 0; i = mds.get_next((UINT)i, &iter)) {
            MDSet const* pts = getPointTo((UINT)i, *mx);
            if (pts != NULL && !pts->is_empty()) {
                tmp.bunion(*pts, *m_misc_bs_mgr);
            } else if (!has_unified_may_pt) {
                has_unified_may_pt = true;
                //We do NOT known where p[...] pointed to, use the
                //conservative solution.
                tmp.bunion(*m_maypts, *m_misc_bs_mgr);
            }
        }

        mds.copy(tmp, *m_misc_bs_mgr);
        tmp.clean(*m_misc_bs_mgr);
    }
}


//The function generates new MD for given PR.
//It should be called if new PR generated in optimzations.
MD const* IR_AA::allocPRMD(IR * pr)
{
    ASSERT0(pr->is_pr());
    MD const* md = m_ru->genMDforPR(pr);
    setMustAddr(pr, md);
    pr->cleanRefMDSet();
    return md;
}


//The function generates new MD for given PR.
//It should be called if new PR generated in optimzations.
MD const* IR_AA::allocPhiMD(IR * phi)
{
    ASSERT0(phi->is_phi());
    MD const* md = m_ru->genMDforPR(phi);
    setMustAddr(phi, md);
    phi->cleanRefMDSet();
    return md;
}


MD const* IR_AA::allocIdMD(IR * ir)
{
    ASSERT0(ir->is_id());
    MD const* t = m_ru->genMDforId(ir);
    setMustAddr(ir, t);
    ir->cleanRefMDSet();
    return t;
}


MD const* IR_AA::allocLoadMD(IR * ir)
{
    MD const* t = m_ru->genMDforLoad(ir);
    ASSERT0(t);
    ir->cleanRefMDSet();
    if (LD_ofst(ir) != 0) {
        MD t2(*t);
        ASSERT0(t2.is_exact());
        MD_ofst(&t2) += LD_ofst(ir);
        MD_size(&t2) = ir->getTypeSize(m_tm);
        MD const* entry = m_md_sys->registerMD(t2);
        ASSERT(MD_id(entry) > 0, ("Not yet registered"));
        t = entry; //regard MD with offset as return result.
    }
    setMustAddr(ir, t);
    return t;
}


MD const* IR_AA::allocStorePRMD(IR * ir)
{
    ASSERT0(ir->is_stpr());
    MD const* md = m_ru->genMDforPR(ir);
    setMustAddr(ir, md);
    ir->cleanRefMDSet();
    return md;
}


MD const* IR_AA::allocCallResultPRMD(IR * ir)
{
    ASSERT0(ir->isCallStmt());
    MD const* md = m_ru->genMDforPR(ir);
    setMustAddr(ir, md);
    ir->cleanRefMDSet();
    return md;
}


MD const* IR_AA::allocSetelemMD(IR * ir)
{
    ASSERT0(ir->is_setelem());
    MD const* md = m_ru->genMDforPR(ir);
    IR const* ofst = SETELEM_ofst(ir);
    ASSERT0(ofst);
    if (md->is_exact()) {
        if (ofst->is_const()) {
            ASSERT(ofst->is_int(), ("offset of SETELEM must be integer."));

            //Accumulating offset of identifier.
            //e.g: struct {int a,b; } s; s.a = 10
            //generate: st s:offset(4) = 10;
            MD t(*md);
            ASSERT0(ir->getTypeSize(m_tm) > 0);
            MD_ofst(&t) += (UINT)CONST_int_val(ofst);
            MD_size(&t) = ir->getTypeSize(m_tm);
            MD const* entry = m_md_sys->registerMD(t);
            ASSERT(MD_id(entry) > 0, ("Not yet registered"));
            md = entry; //regard MD with offset as return result.
        } else {
            //Offset is variable.
            //e.g: vector<4xi32> v; v[i] = 34;
            //will generate:
            //    st $1 = ld v;
            //    setelem $1 = 34, ld i;
            //    st v = $1;

            MD t(*md);
            ASSERT0(ir->getTypeSize(m_tm) > 0);
            MD_ty(&t) = MD_RANGE;
            MD_ofst(&t) = 0;
            MD_size(&t) = ir->getTypeSize(m_tm);
            MD const* entry = m_md_sys->registerMD(t);
            ASSERT(MD_id(entry) > 0, ("Not yet registered"));
            md = entry; //regard MD with range as return result.
        }
    }

    setMustAddr(ir, md);
    ir->cleanRefMDSet();
    return md;
}


MD const* IR_AA::allocGetelemMD(IR * ir)
{
    ASSERT0(ir->is_getelem());
    MD const* md = m_ru->genMDforPR(ir);
    setMustAddr(ir, md);
    ir->cleanRefMDSet();
    return md;
}


MD const* IR_AA::allocStoreMD(IR * ir)
{
    ASSERT0(ir->is_st());
    MD const* md = m_ru->genMDforStore(ir);
    ASSERT0(md);
    ir->cleanRefMDSet();
    if (ST_ofst(ir) != 0) {
        //Accumulating offset of identifier.
        //e.g: struct {int a,b; } s; s.a = 10
        //generate: st('s', ofst:4) = 10
        MD t(*md);
        ASSERT0(t.is_exact());
        ASSERT0(ir->getTypeSize(m_tm) > 0);
        MD_ofst(&t) += ST_ofst(ir);
        MD_size(&t) = ir->getTypeSize(m_tm);
        MD const* entry = m_md_sys->registerMD(t);
        ASSERT(MD_id(entry) > 0, ("Not yet registered"));
        md = entry; //regard MD with offset as return result.
    }
    setMustAddr(ir, md);
    return md;
}


bool IR_AA::evaluateFromLda(IR const* ir)
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


//Compute the offset for pointer arithmetic.
//Return true if the offset can be confirmed via
//simply calculation, and has been determined by this function.
//Return false if caller need to keep evaluating the offset.
bool IR_AA::computeConstOffset(
    IR const* ir,
    IR const* opnd1,
    IN OUT MDSet & mds,
    IN OUT MDSet & opnd0_mds)
{
    //Compute the offset for pointer arithmetic.
    if (opnd1->is_const() && opnd1->is_int() && CONST_int_val(opnd1) == 0) {
        mds.copy(opnd0_mds, *m_misc_bs_mgr);
        return true;
    }

    HOST_UINT const_offset = 0;
    if (opnd1->is_const() && opnd1->is_int()) {
        const_offset = (HOST_UINT)CONST_int_val(opnd1);
    } else if (opnd1->is_pr()) {
        if (!m_ru->evaluateConstInteger(opnd1, (ULONGLONG*)&const_offset)) {
            return false;
        }
    } else {
        return false;
    }

    mds.clean(*m_misc_bs_mgr);
    if (BIN_opnd0(ir)->is_lda() || evaluateFromLda(BIN_opnd0(ir))) {
        //In the case: LDA(x) + ofst, we can determine
        //the value of LDA(x) is constant.
        //Keep offset validation unchanged.
        SEGIter * iter;
        for (INT i = opnd0_mds.get_first(&iter);
             i >= 0; i = opnd0_mds.get_next((UINT)i, &iter)) {
            MD * imd = m_md_sys->getMD((UINT)i);
            if (imd->is_exact()) {
                MD const* entry = NULL;
                MD x(*imd);
                if (ir->is_add()) {
                    //In the case: LDA(x) + ofst, we can determine
                    //the value of LDA(x) is constant.
                    ; //Keep offset validation unchanged.
                    MD_ofst(&x) += (UINT)const_offset;
                    entry = m_md_sys->registerMD(x);
                    ASSERT0(MD_id(entry) > 0);
                } else {
                    //case: &x - ofst.
                    //Keep offset validation unchanged.
                    INT s = (INT)MD_ofst(&x);
                    s -= (INT)const_offset;
                    if (s < 0) {
                        MD_ty(&x) = MD_UNBOUND;
                        MD_size(&x) = 0;
                        MD_ofst(&x) = 0;
                    }
                    entry = m_md_sys->registerMD(x);
                    ASSERT0(MD_id(entry) > 0);
                }
                mds.bunion(entry, *m_misc_bs_mgr);
            } else {
                mds.bunion(imd, *m_misc_bs_mgr);
            }
        }
        return true;
    }
    return false;
}


//Perform pointer arith to compute where ir might point to.
//If we compute the point-to set of p+1, that always equivilate to
//compute the point-to of p, and each element in the set will be
//registered to be unbound. Since if p+1 is placed in a loop,
//we could not determine the exact MD where p pointed to.
//'mds' : record memory descriptor of 'ir'.
void IR_AA::inferPtArith(
        IR const* ir,
        IN OUT MDSet & mds,
        IN OUT MDSet & opnd0_mds,
        IN OUT AACtx * opnd0_ic,
        IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_add() || ir->is_sub());
    IR * opnd1 = BIN_opnd1(ir);
    if (((opnd1->is_const() && opnd1->is_int()) || opnd1->is_pr()) &&
        computeConstOffset(ir, opnd1, mds, opnd0_mds)) {
        return;
    } else {
        //Generate MD expression for opnd1.
        AACtx opnd1_tic(*opnd0_ic);
        opnd1_tic.cleanBottomUpFlag();
        AC_comp_pt(&opnd1_tic) = false; //PointToSet of addon is useless.
        inferExpression(opnd1, mds, &opnd1_tic, mx);

        //Bottom-up flag of opnd1 is useless to its parent.

        mds.clean(*m_misc_bs_mgr);
        if (AC_has_comp_lda(&opnd1_tic) && AC_has_comp_lda(opnd0_ic)) {
            //In the situation such as: &a - &b.
            ASSERT(ir->is_sub(), ("only support pointer sub pointer"));
            AC_has_comp_lda(opnd0_ic) = false;
            return;
        }
    }

    //Pointer arithmetic causes ambiguous memory access.
    //e.g: while (...) { p = p+1 }
    //Where is p pointing to at all?
    //Set each MD of opnd0 to be UNBOUND even if it is exact
    //to keep the conservation.
    ASSERT(mds.is_empty(), ("output buffer not yet initialized"));

    if (opnd0_mds.is_empty()) {
        //Point-to set of opnd0 of binary-op is MayPointToSet.
        ASSERT0(AC_returned_pts(opnd0_ic));
        return;
    }

    SEGIter * iter;
    for (INT i = opnd0_mds.get_first(&iter);
         i >= 0; i = opnd0_mds.get_next((UINT)i, &iter)) {
        MD * imd = m_md_sys->getMD((UINT)i);
        if (imd->is_exact()) {
            MD x(*imd);
            MD_ty(&x) = MD_UNBOUND;
            MD const* entry = m_md_sys->registerMD(x);
            ASSERT0(MD_id(entry) > 0);
            mds.bunion_pure(MD_id(entry), *m_misc_bs_mgr);
            continue;
        }

        mds.bunion(imd, *m_misc_bs_mgr);
    }
}


//Compute the point-to set of expression.
//ir may be pointer arithmetic, even if opnd0 is not pointer type.
//'mds' : record memory-descriptor set or
//    point-to set of 'ir' if AC_comp_pt(ic) is true.
void IR_AA::processPointerArith(
        IR * ir,
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
            ASSERT(!opnd1->is_ptr(), ("pointer can not plus pointer"));
        }
        if (!opnd1->is_ptr()) {
            //pointer +/- n still be pointer.
            //ir may be VOID.
            //ASSERT0(ir->is_ptr());
        }

        inferPtArith(ir, mds, tmp, &tic, mx);
        ic->copyBottomUpFlag(tic);
    } else {
        if (ir->is_add()) {
            //opnd1 can not be pointer. e.g: x+&q
            ASSERT0(!opnd1->is_ptr());
        }

        if (AC_comp_pt(&tic)) {
            //tmp already have recorded the POINT-TO set of opnd0.
            //Now, infering the final POINT-TO set according to op0 +/- op1.
            inferPtArith(ir, mds, tmp, &tic, mx);
        } else {
            //Scan and generate MD of opnd1.
            AACtx ttic(*ic);
            ttic.cleanBottomUpFlag();
            inferExpression(opnd1, mds, &ttic, mx);
            mds.clean(*m_misc_bs_mgr); //Do not remove this code.
        }

        ic->copyBottomUpFlag(tic);
    }
    tmp.clean(*m_misc_bs_mgr);
}


//Assign unique MD to pr.
//'mds' : record memory descriptor of 'ir'.
MD const* IR_AA::assignPRMD(
        IR * ir,
        IN OUT MDSet * mds,
        IN OUT AACtx * ic,
        IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_pr());
    ASSERT0(mds && ic);
    MD const* tmp;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        tmp = allocPRMD(ir);
        AC_is_mds_mod(ic) = true;
    } else {
        tmp = getMustAddr(ir);
        AC_is_mds_mod(ic) = false;
    }
    ASSERT0(tmp);

    if (AC_comp_pt(ic)) {
        ASSERT0(mx);
        MDSet const* pts = getPointTo(MD_id(tmp), *mx);
        MD const* typed_md = NULL;
        if (pts != NULL && !pts->is_empty()) {
            if (pts->is_contain_global() &&
                ir->getAI() != NULL &&
                (typed_md = computePointToViaType(ir)) != NULL) {
                setPointToUniqueMD(MD_id(tmp), *mx, typed_md);
                mds->clean(*m_misc_bs_mgr);
                mds->bunion(typed_md, *m_misc_bs_mgr);
            } else if (pts == m_maypts) {
                mds->clean(*m_misc_bs_mgr);
                AC_returned_pts(ic) = m_maypts;
            } else {
                mds->copy(*pts, *m_misc_bs_mgr);
            }
        } else if (ir->getAI() != NULL &&
                   (typed_md = computePointToViaType(ir)) != NULL) {
            setPointToMDSetByAddMD(MD_id(tmp), *mx, typed_md);
            mds->clean(*m_misc_bs_mgr);
            mds->bunion(typed_md, *m_misc_bs_mgr);
        } else {
            //We do NOT known where p pointed to, and compute
            //the offset as well.
            //mds->copy(*m_maypts, *m_misc_bs_mgr);
            mds->clean(*m_misc_bs_mgr);
            AC_returned_pts(ic) = m_maypts;
        }
    } else {
        mds->clean(*m_misc_bs_mgr);
        mds->bunion(tmp, *m_misc_bs_mgr);
    }

    return tmp;
}


//'mds' : record memory descriptor of 'ir'
MD const* IR_AA::assignLoadMD(
        IR * ir,
        IN OUT MDSet * mds,
        IN OUT AACtx * ic,
        IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_ld());
    ASSERT0(mds && ic);
    MD const* t;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        t = allocLoadMD(ir);
        AC_is_mds_mod(ic) = true;
    } else {
        t = getMustAddr(ir);
        AC_is_mds_mod(ic) = false;
    }
    ASSERT0(t);

    if (AC_comp_pt(ic)) {
        ASSERT0(mx);
        AC_is_mds_mod(ic) = true;
        MDSet const* pts = getPointTo(MD_id(t), *mx);
        MD const* typed_md = NULL;
        if (pts != NULL && !pts->is_empty()) {
            if (pts->is_contain_global() &&
                ir->getAI() != NULL &&
                (typed_md = computePointToViaType(ir)) != NULL) {
                setPointToUniqueMD(MD_id(t), *mx, typed_md);
                mds->clean(*m_misc_bs_mgr);
                mds->bunion(typed_md, *m_misc_bs_mgr);
            } else if (pts == m_maypts) {
                mds->clean(*m_misc_bs_mgr);
                AC_returned_pts(ic) = m_maypts;
            } else {
                mds->copy(*pts, *m_misc_bs_mgr);
            }
        } else if (ir->getAI() != NULL &&
                   (typed_md = computePointToViaType(ir)) != NULL) {
            setPointToMDSetByAddMD(MD_id(t), *mx, typed_md);
            mds->clean(*m_misc_bs_mgr);
            mds->bunion(typed_md, *m_misc_bs_mgr);
        } else {
            //We do NOT known where p pointed to,
            //e.g: If p->? (p+2)->??
            //mds->copy(*m_maypts, *m_misc_bs_mgr);
            mds->clean(*m_misc_bs_mgr);
            AC_returned_pts(ic) = m_maypts;
        }
    } else {
        mds->clean(*m_misc_bs_mgr);
        mds->bunion(t, *m_misc_bs_mgr);
    }
    return t;
}


//Assign unique MD to 'id'.
//'mds': record memory descriptor of 'ir'.
MD const* IR_AA::assignIdMD(
        IR * ir,
        IN OUT MDSet * mds,
        IN OUT AACtx * ic)
{
    ASSERT0(ir->is_id());
    ASSERT0(ic && mds);
    MD const* t;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        if (ID_info(ir)->is_string()) {
            t = allocStringMD(ID_info(ir)->get_name());
            setMustAddr(ir, t);
            ir->cleanRefMDSet();
        } else {
            t = allocIdMD(ir);
        }
        AC_is_mds_mod(ic) = true;
    } else {
        t = getMustAddr(ir);
        AC_is_mds_mod(ic) = false;
    }
    ASSERT0(t);

    mds->clean(*m_misc_bs_mgr);
    mds->bunion(t, *m_misc_bs_mgr);
    return t;
}


//Alloc MD for const string.
MD const* IR_AA::allocStringMD(SYM const* string)
{
    ASSERT0(string);
    MD const* strmd;
    if ((strmd = m_rumgr->genDedicateStrMD()) != NULL) { return strmd; }

    VAR * v = m_ru->getVarMgr()->registerStringVar(
        NULL, string, MEMORY_ALIGNMENT);
    //Set string address to be taken only if it is base of LDA.
    //VAR_is_addr_taken(v) = true;
    MD md;
    MD_base(&md) = v;
    MD_size(&md) = (UINT)strlen(SYM_name(string)) + 1;
    MD_ofst(&md) = 0;
    MD_ty(&md) = MD_EXACT;
    ASSERT0(v->is_string());

    MD const* e = m_md_sys->registerMD(md);
    ASSERT0(MD_id(e) > 0);
    return e;
}


//'mds' : record memory descriptor of 'ir'.
void IR_AA::processIld(
        IR * ir,
        IN OUT MDSet & mds,
        IN OUT AACtx * ic,
        IN OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_ild());
    ASSERT0(ILD_base(ir)->is_ptr());

    //... = *q, if q->x, set ir's MD to be 'x'.
    AACtx tic(*ic);

    //Compute the address that ILD described.
    AC_comp_pt(&tic) = true;
    inferExpression(ILD_base(ir), mds, &tic, mx);

    if (mds.is_empty()) {
        ASSERT0(AC_returned_pts(&tic));
        ir->cleanRefMD();
        setMayAddr(ir, AC_returned_pts(&tic));
        if (AC_comp_pt(ic)) {
            AC_returned_pts(ic) = m_maypts;
        }
        return;
    }

    AC_is_mds_mod(ic) |= AC_is_mds_mod(&tic);
    if (ILD_ofst(ir) != 0) {
        UINT ild_ofst = ILD_ofst(ir);
        MDSet tmp;
        bool change = false;
        SEGIter * iter;
        for (INT i = mds.get_first(&iter);
             i >= 0; i = mds.get_next((UINT)i, &iter)) {
            MD * l = m_md_sys->getMD((UINT)i);
            ASSERT0(l);

            //Note if ir's type is VOID, size is 0.
            //Thus the MD indicates a object that is p + ild_ofst + 0.
            UINT size = ir->getTypeSize(m_tm);
            if (l->is_exact() && MD_size(l) != size) {
                MD md(*l);
                MD_ofst(&md) += ild_ofst;
                MD_size(&md) = size;
                MD const* entry = m_md_sys->registerMD(md);
                ASSERT(MD_id(entry) > 0, ("Not yet registered"));
                tmp.bunion(MD_id(entry), *m_misc_bs_mgr);
                change = true;
            } else {
                tmp.bunion((UINT)i, *m_misc_bs_mgr);
            }
        }

        if (change) { mds.copy(tmp, *m_misc_bs_mgr); }

        ASSERT0(!mds.is_empty());
        tmp.clean(*m_misc_bs_mgr);
    }

    //Set this set as the address MDSet of ILD.
    MD const* mustaddr = NULL;
    SEGIter * iter;
    if (mds.get_elem_count() == 1 &&
        !MD_is_may(mustaddr = m_md_sys->getMD((UINT)mds.get_first(&iter)))) {
        mustaddr = m_md_sys->getMD((UINT)mds.get_first(&iter));
        setMustAddr(ir, mustaddr);
        ir->cleanRefMDSet();
    } else if (isAllElementDerivedFromSameEffectVar(mds, m_md_sys, &mustaddr)) {
        ASSERT0(mustaddr);
        setMustAddr(ir, mustaddr);
        ir->cleanRefMDSet();
    } else {
        ir->cleanRefMD();
        setMayAddr(ir, m_mds_hash->append(mds));
    }

    if (!AC_comp_pt(ic)) { return; }

    //Compute the ILD pointed to.
    if (mustaddr != NULL) {
        MDSet const* pts = getPointTo(MD_id(mustaddr), *mx);
        if (pts != NULL && !pts->is_empty()) {
            mds.copy(*pts, *m_misc_bs_mgr);
        } else {
            //We do NOT known where p pointed to, and compute
            //the offset as well.
            //e.g: If p->? (p+2)->??
            mds.copy(*m_maypts, *m_misc_bs_mgr);
        }
        return;
    }

    MDSet tmp;
    bool has_unified_may_pt = false;
    SEGIter * iter2;
    for (INT i = mds.get_first(&iter2);
         i >= 0; i = mds.get_next((UINT)i, &iter2)) {
        MDSet const* pts = getPointTo((UINT)i, *mx);
        if (pts != NULL && !pts->is_empty()) {
            tmp.bunion(*pts, *m_misc_bs_mgr);
        } else if (!has_unified_may_pt) {
            has_unified_may_pt = true;
            //We do NOT known where p pointed to, and compute
            //the offset as well.
            //e.g: If p->? (p+2)->??
            tmp.bunion(*m_maypts, *m_misc_bs_mgr);
        }
    }

    mds.copy(tmp, *m_misc_bs_mgr);
    tmp.clean(*m_misc_bs_mgr);
}


//'mds' : record memory descriptor of 'ir'.
void IR_AA::processConst(
        IR * ir,
        IN OUT MDSet & mds,
        IN OUT AACtx * ic)
{
    ASSERT0(ir->is_const());
    ASSERT0(ic);

    mds.clean(*m_misc_bs_mgr);
    if (ir->is_str()) {
        //'ir' describes memory address of string const.
        //Add a new VAR to describe the string.
        //'mds' : record memory descriptor of 'ir'.
        MD const* t = allocStringMD(CONST_str_val(ir));
        ASSERT0(t);
        if (!m_is_visit.is_contain(ir->id())) {
            m_is_visit.bunion(ir->id());
            AC_is_mds_mod(ic) = true;
        } else {
            AC_is_mds_mod(ic) = false;
        }
        mds.bunion(t, *m_misc_bs_mgr);
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
void IR_AA::recomputeDataType(AACtx const& ic, IR const* ir, OUT MDSet & pts)
{
    if (AC_has_comp_lda(&ic) && !ir->is_void()) {
        //If rhs return the address which taken by IR_LDA.
        //Here we need to reinfer the actual memory address according
        //to the stmt data type because the result data type of array
        //operation may not same as the data type or LDA operation.
        //e.g: b:ptr<i32>=LDA(a), a is a memory chunk, but b is
        //a pointer that pointed to an i32 memory space.

        //ir's type may not be pointer type.
        //e.g: x = (int)&arr[j], the result type of LDA has been
        //converted to integer. So x will be integer.

        if (ir->is_ptr()) {
            ASSERT(pts.get_effect_md(m_md_sys), ("LDA's base must be effect MD"));

            //ptset may include element which also be in m_maypts.
            //ASSERT0(!ptset->is_intersect(*m_maypts));

            UINT size = TY_ptr_base_size(ir->getType());
            reviseMDsize(pts, size);
        }
    }
}


void IR_AA::inferStoreValue(
        IN IR * ir,
        IR * rhs,
        MD const* lhs_md,
        IN AACtx * ic,
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
    MDSet tmp;
    AACtx rhsic(*ic);
    if (ir->is_ptr() || ir->is_void()) {
        //Regard ir as pointer if its has VOID type.
        AC_comp_pt(&rhsic) = true;
    }

    inferExpression(rhs, tmp, &rhsic, mx);
    recomputeDataType(rhsic, ir, tmp);

    //Update POINT-TO of LHS.
    if (AC_has_comp_lda(&rhsic) || AC_comp_pt(&rhsic)) {
        //p = q, q is array base (e.g:q[100]), add p->q.
        //p = &q, add p->q.
        //p = (&q)+n+m, add p->q.
        if (AC_has_comp_lda(&rhsic)) {
            ASSERT0(tmp.get_elem_count() == 1);

            //CASE: =&g[i] violate this constrain.
            //ASSERT0(ptset->is_contain_only_exact_and_str(m_md_sys));
        }

        //We need to determine where is the rhs expression pointing to.
        //Note that the point-to set can not be empty, rhs may be const/cvt.
        //So lhs may point to anywhere.
        MD const* typed_md = NULL;
        bool is_maypts = false;
        if (!tmp.is_empty()) {
            if (tmp.is_contain_global() && rhs->getAI() != NULL &&
                (typed_md = computePointToViaType(rhs)) != NULL) {
                //Make use of typed pointer info to improve the precsion.
                tmp.clean(*m_misc_bs_mgr);
                tmp.bunion(typed_md, *m_misc_bs_mgr);
            }
        } else if (rhs->getAI() != NULL &&
                   (typed_md = computePointToViaType(rhs)) != NULL) {
            tmp.bunion(typed_md, *m_misc_bs_mgr);
        } else {
            //We do NOT know exactly where p pointed to.
            //Note do NOT change the content of 'pts' any more.
            is_maypts = true;
        }

        ASSERT0(!tmp.is_empty() || is_maypts);
        if (m_flow_sensitive) {
            if (is_maypts) {
                setPointTo(MD_id(lhs_md), *mx, m_maypts);
            } else {
                setPointToMDSet(MD_id(lhs_md), *mx, tmp);
            }
        } else {
            if (is_maypts) {
                setPointToMDSetByAddMDSet(MD_id(lhs_md), *mx, *m_maypts);
            } else {
                setPointToMDSetByAddMDSet(MD_id(lhs_md), *mx, tmp);
            }
        }

        tmp.clean(*m_misc_bs_mgr);
        return;
    }

    ASSERT0(!ir->is_ptr() && !ir->is_void());
    //1. p = q, q is pointer, if q->x, add p->x.
    //Given a pointer, if its point-to is empty, the pointer
    //points to MAY_POINT_TO_SET.
    //
    //May be unify MAY_PT_SET is correct in comprehension,
    //but it  will occupy more memory.
    //e.g: For pr1->MAY_PT_SET, and if we set
    //pr1->NULL here, that might cause convert_md2mds_to_ptpair()
    //can not recog pr1's POINT-TO set, and its pt-pair info
    //is missing. That will cause a dead cycle at global
    //iterative solver.
    if (m_flow_sensitive) {
        cleanPointTo(MD_id(lhs_md), *mx);
    }

    tmp.clean(*m_misc_bs_mgr);
}


//Caculate pointer info accroding to rules for individiual ir, and
//constructing the mapping table that maps MD to an unique VAR.
//e.g For given four point-to pairs {p->a,p->b,q->c,q->d}.
//    store can be shown as
//        p = q;
//    this make the point-to set of 'p' to be {p->c, p->d}.
//    and the formally formular form is:
//    MDSet(p) = MDSet(q)
void IR_AA::processStore(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_st());
    MD const* t;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        t = allocStoreMD(ir);
    } else {
        t = getMustAddr(ir);
    }

    AACtx ic;
    inferStoreValue(ir, ST_rhs(ir), t, &ic, mx);
}


//Caculate pointer info accroding to rules for individiual ir, and
//constructing the mapping table that maps MD to an unique VAR.
//e.g For given four point-to pairs {p->a,p->b,q->c,q->d}.
//    store can be shown as
//        p = q;
//    this make the point-to set of 'p' to be {p->c, p->d}.
//    and the formally formular form is:
//    MDSet(p) = MDSet(q)
void IR_AA::processStorePR(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_stpr());
    MD const* t;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        t = allocStorePRMD(ir);
    } else {
        t = getMustAddr(ir);
    }

    AACtx ic;
    inferStoreValue(ir, STPR_rhs(ir), t, &ic, mx);
}


//Compute the point to info for IR_SETELEM.
void IR_AA::processSetelem(IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_setelem());
    MD const* t;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        t = allocSetelemMD(ir);
    } else {
        t = getMustAddr(ir);
    }

    AACtx ic;
    inferStoreValue(ir, SETELEM_rhs(ir), t, &ic, mx);

    if (!SETELEM_ofst(ir)->is_const()) {
        ic.clean();
        MDSet tmp;
        inferExpression(SETELEM_ofst(ir), tmp, &ic, mx);
        tmp.clean(*m_misc_bs_mgr);
    }
}


//Compute the point to info for IR_GETELEM.
void IR_AA::processGetelem(IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_getelem() && GETELEM_ofst(ir));
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        allocGetelemMD(ir);
    } else {
        getMustAddr(ir);
    }

    //Process base field, it must refer to memory object.
    AACtx ic;
    MDSet tmp;
    inferExpression(GETELEM_base(ir), tmp, &ic, mx);

    //Process byte offset to base field.
    ic.clean();
    inferExpression(GETELEM_ofst(ir), tmp, &ic, mx);
    tmp.clean(*m_misc_bs_mgr);
}


void IR_AA::processPhi(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_phi());
    MD const* phi_md;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        phi_md = allocPhiMD(ir);
    } else {
        phi_md = getMustAddr(ir);
    }

    AACtx ic;
    if (ir->is_ptr() || ir->is_void()) {
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
                phi_pts.copy(*pts, *m_misc_bs_mgr);
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
                tmp.clean(*m_misc_bs_mgr);
                continue;
            }

            if (tmp.is_empty()) {
                ASSERT0(AC_returned_pts(&tic));
                phi_pts_is_maypts = true;
                //phi_pts.bunion(*m_maypts, *m_misc_bs_mgr);
                continue;
            }

            //phires may point to the union set of each operand.
            phi_pts.bunion(tmp, *m_misc_bs_mgr);
        }

        tmp.clean(*m_misc_bs_mgr);
    }

    if (comp_pt) {
        ASSERT0(!phi_pts.is_empty() || phi_pts_is_maypts);
        if (phi_pts_is_maypts) {
            setPointTo(MD_id(phi_md), *mx, m_maypts);
        } else {
            setPointTo(MD_id(phi_md), *mx, m_mds_hash->append(phi_pts));
        }
    }

    phi_pts.clean(*m_misc_bs_mgr);
}


void IR_AA::inferIstoreValue(IN IR * ir, IN AACtx * ic, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_ist());
    MDSet const* ist_mayaddr = getMayAddr(ir);
    MD const* ist_mustaddr = getMustAddr(ir);
    ASSERT0(ist_mustaddr != NULL ||
            (ist_mayaddr != NULL && !ist_mayaddr->is_empty()));

    //Considering the ISTORE operation, there are three
    //situations should be handled:
    //    1. The RHS is LDA.
    //    2. The RHS is ILD, that caused indirect POINT-TO generated.
    //        e.g: *p=ILD(q), and p->x,q->a,a->w, then x->w,
    //    3. Propagate the MDSet that RHS pointed to the LHS.
    //        e.g: *p=q, and p->x,q->a, then x->a.
    AACtx tic(*ic);
    if (ir->is_ptr() || ir->is_void()) {
        AC_comp_pt(&tic) = true;
    }

    MDSet tmp;
    inferExpression(IST_rhs(ir), tmp, &tic, mx);
    recomputeDataType(tic, ir, tmp);

    if (AC_has_comp_lda(&tic) || AC_comp_pt(&tic)) {
        //If result type of IST is pointer, and the ptset is empty, then
        //it might point to anywhere.
        //e.g: Given p=&q and *p=(int*)0x1000;
        //=> q=0x1000, and q is pointer, so q may point to anywhere.
        //
        // *p = q, if p->{x}, q->{a}, add {x}->{a}
        // *p = q, if p->{x}, q->{¦µ}, add {x}->{¦µ}
        // *p = q, if p->{¦µ}, q->{x}, add {all mem}->{x}
        //
        //Update the POINT-TO of elems in p's point-to set.
        //Aware of whether if the result of IST is pointer.
        if (AC_has_comp_lda(&tic)) {
            ASSERT0(!tmp.is_empty());

            //ptset may include element which also be in m_maypts.
            //ASSERT0(!ptset->is_intersect(m_maypts));
        }

        //We need to determine where is the rhs expression pointed to.
        //Note that the point-to set can not be empty, rhs may be const/cvt.
        //So lhs may point to anywhere.
        MD const* typed_md = NULL;
        bool is_maypts = false;
        if (!tmp.is_empty()) {
            if (tmp.is_contain_global() && IST_rhs(ir)->getAI() != NULL &&
                (typed_md = computePointToViaType(IST_rhs(ir))) != NULL) {
                //Make use of typed pointer info to improve the precsion.
                tmp.clean(*m_misc_bs_mgr);
                tmp.bunion(typed_md, *m_misc_bs_mgr);
            }
        } else if (IST_rhs(ir)->getAI() != NULL &&
                   (typed_md = computePointToViaType(IST_rhs(ir))) != NULL) {
            tmp.bunion(typed_md, *m_misc_bs_mgr);
        } else {
            ASSERT0(AC_returned_pts(&tic));
            ASSERT0(AC_returned_pts(&tic) == m_maypts);

            //We do NOT known where p pointed to, and compute
            //the offset as well.
            //Do NOT modify pts any more.
            is_maypts = true;
        }

        MDSet const* pts2 = is_maypts ? m_maypts : &tmp;

        if (m_flow_sensitive) {
            if (ist_mustaddr != NULL) {
                if (ist_mustaddr->is_exact()) {
                    //istore is killing def.
                    if (is_maypts) {
                        setPointTo(MD_id(ist_mustaddr), *mx, m_maypts);
                    } else {
                        setPointToMDSet(MD_id(ist_mustaddr), *mx, tmp);
                    }
                } else {
                    //istore is nonkilling def.
                    setPointToMDSetByAddMDSet(MD_id(ist_mustaddr), *mx, *pts2);
                }
            } else {
                //mayaddr may contain inexact MD.
                ElemCopyAndUnionPointTo(*ist_mayaddr, *pts2, mx);
            }
        } else {
            //flow insensitive.
            if (ist_mustaddr != NULL) {
                setPointToMDSetByAddMDSet(MD_id(ist_mustaddr), *mx, *pts2);
            } else {
                ElemUnionPointTo(*ist_mayaddr, *pts2, mx);
            }
        }

        tmp.clean(*m_misc_bs_mgr);
        return;
    }

    if (m_flow_sensitive) {
        ASSERT0(!ir->is_ptr() && !ir->is_void());
        if (ist_mustaddr != NULL) {
            if (ist_mustaddr->is_exact()) {
                cleanPointTo(MD_id(ist_mustaddr), *mx);
            }
        } else {
            //mayaddr may contain inexact MD.
            ElemCleanExactPointTo(*ist_mayaddr, mx);
        }
    }

    tmp.clean(*m_misc_bs_mgr);
}


void IR_AA::inferStoreArrayValue(IN IR * ir, IN AACtx * ic, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_starray());
    MDSet const* lhs_mayaddr = getMayAddr(ir);
    MD const* lhs_mustaddr = getMustAddr(ir);
    ASSERT(lhs_mustaddr != NULL ||
           (lhs_mayaddr != NULL && !lhs_mayaddr->is_empty()),
           ("You need infer the may memory address of array operation"));

    //Propagate the MDSet that RHS pointed to the LHS.
    //e.g: a[x]=q, and q->a, then a[x]->a.
    AACtx tic(*ic);
    if (ir->is_ptr() || ir->is_void()) {
        AC_comp_pt(&tic) = true;
    }

    MDSet tmp;
    inferExpression(STARR_rhs(ir), tmp, &tic, mx);
    recomputeDataType(tic, ir, tmp);

    if (AC_has_comp_lda(&tic) || AC_comp_pt(&tic)) {
        //If result data type of array operation is pointer, we need
        //update the POINT-TO of elems.
        if (AC_has_comp_lda(&tic)) {
            ASSERT0(!tmp.is_empty());

            //ptset may include element which also be in m_maypts.
            //ASSERT0(!ptset->is_intersect(m_maypts));
        }

        //We need to determine where are the rhs expression pointed to.
        //Note that the point-to set can not be empty.
        //Rhs may be const/cvt, if that is, lhs may point to anywhere.
        MD const* typed_md = NULL;
        bool is_maypts = false;
        if (!tmp.is_empty()) {
            if (tmp.is_contain_global() &&
                STARR_rhs(ir)->getAI() != NULL &&
                (typed_md = computePointToViaType(STARR_rhs(ir))) != NULL) {
                //Make use of typed pointer info to improve the precsion.
                tmp.clean(*m_misc_bs_mgr);
                tmp.bunion(typed_md, *m_misc_bs_mgr);
            }
        } else if (STARR_rhs(ir)->getAI() != NULL &&
                   (typed_md = computePointToViaType(STARR_rhs(ir))) != NULL) {
            tmp.bunion(typed_md, *m_misc_bs_mgr);
        } else {
            //We do NOT know where rhs pointed to.
            //Do NOT modify pts any more.
            is_maypts = true;
        }

        MDSet const* pts2 = is_maypts ? m_maypts : &tmp;
        if (m_flow_sensitive) {
            if (lhs_mustaddr != NULL) {
                if (lhs_mustaddr->is_exact()) {
                    if (is_maypts) {
                        setPointTo(MD_id(lhs_mustaddr), *mx, m_maypts);
                    } else {
                        setPointToMDSet(MD_id(lhs_mustaddr), *mx, *pts2);
                    }
                } else {
                    setPointToMDSetByAddMDSet(MD_id(lhs_mustaddr), *mx, *pts2);
                }
            } else {
                //mayaddr may contain inexact MD.
                ElemCopyAndUnionPointTo(*lhs_mayaddr, *pts2, mx);
            }
        } else {
            //flow insensitive.
            if (lhs_mustaddr != NULL) {
                setPointToMDSetByAddMDSet(MD_id(lhs_mustaddr), *mx, *pts2);
            } else {
                ElemUnionPointTo(*lhs_mayaddr, *pts2, mx);
            }
        }

        tmp.clean(*m_misc_bs_mgr);
        return;
    }

    if (m_flow_sensitive) {
        ASSERT0(!ir->is_ptr() && !ir->is_void());
        if (lhs_mustaddr != NULL) {
            if (lhs_mustaddr->is_exact()) {
                cleanPointTo(MD_id(lhs_mustaddr), *mx);
            }
        } else {
            //mayaddr may contain inexact MD.
            ElemCleanExactPointTo(*lhs_mayaddr, mx);
        }
    }

    tmp.clean(*m_misc_bs_mgr);
}


//Infer point-to set for array element.
//e.g For given four point-to pairs {q->c,q->d}.
//  store array can be shown as
//      a[x] = q;
//  this make the point-to set of a[x] to be {a[x]->c, a[x]->d}.
void IR_AA::processStoreArray(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_starray());
    //mem location may pointed to set.
    MDSet mayaddr;
    AACtx ic;
    AC_comp_pt(&ic) = false; //Here we just need to compute the may address.

    //Compute where array element may point to.
    processArray(ir, mayaddr, &ic, mx);

    INT sz = -1;

    if (mayaddr.is_empty()) {
        //We can not determine the memory address of array element.
        //Set ir has no exact mem-addr for convervative purpose.
        ir->cleanRefMD();
        setMayAddr(ir, m_maypts);
        goto FIN;
    }

    if (ARR_ofst(ir) != 0 ||
        ((sz = (INT)ir->getTypeSize(m_tm)) !=
         (INT)ir->getTypeSize(m_tm))) {
        //If array offset is not zero, the result data type may not
        //being the element type. Try to infer the actual memory address of
        //array element.
        MDSet tmp;
        bool change = false;
        SEGIter * iter;
        for (INT i = mayaddr.get_first(&iter);
             i >= 0; i = mayaddr.get_next((UINT)i, &iter)) {
            MD * l = m_md_sys->getMD((UINT)i);
            ASSERT0(l);
            if (l->is_exact()) {
                MD md(*l);
                MD_ofst(&md) += ARR_ofst(ir);
                ASSERT0(ir->getTypeSize(m_tm) > 0);
                MD_size(&md) = sz == -1 ? ir->getTypeSize(m_tm) : sz;
                MD const* entry = m_md_sys->registerMD(md);
                ASSERT(MD_id(entry) > 0, ("Not yet registered"));
                tmp.bunion(entry, *m_misc_bs_mgr);
                change = true;
            } else {
                tmp.bunion(l, *m_misc_bs_mgr);
            }
        }

        if (change) {
            mayaddr.copy(tmp, *m_misc_bs_mgr);

            //Assign the address to ir.
            //If the MD in mayaddr is single and exact,
            //regarding ir as referencing single and exact MD,
            //or else regard ir as referencing a set of MD.
            MD const* x;
            SEGIter * iter2;
            if (mayaddr.get_elem_count() == 1 &&
                !MD_is_may(x = m_md_sys->getMD(
                    (UINT)mayaddr.get_first(&iter2)))) {
                setMustAddr(ir, x);
                ir->cleanRefMDSet();
            } else {
                ir->cleanRefMD();
                setMayAddr(ir, m_mds_hash->append(mayaddr));
            }
        }
        tmp.clean(*m_misc_bs_mgr);
    }

FIN:
    ic.clean();

    //We do not need to known where array's elem point-to.
    AC_comp_pt(&ic) = false;

    //Infer the memory address for rhs.
    inferStoreArrayValue(ir, &ic, mx);

    mayaddr.clean(*m_misc_bs_mgr);
}


//Indirect store.
//Analyse pointers according to rules for individiual ir to
//constructe the map-table that maps MD to an unique VAR.
void IR_AA::processIst(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_ist());
    ASSERT0(IST_base(ir)->is_ptr());

    //mem location may pointed to set.
    MDSet ml_may_pt;
    AACtx ic;

    //Compute where IST_base may point to.
    AC_comp_pt(&ic) = true;

    inferExpression(IST_base(ir), ml_may_pt, &ic, mx);
    if (ml_may_pt.is_empty()) {
        //If we can not exactly determine where IST_base pointed to,
        //it may point to any variables which have been taken address.
        ASSERT0(AC_returned_pts(&ic) == m_maypts);
        ir->cleanRefMD();
        setMayAddr(ir, m_maypts);

        AACtx ic2;
        inferIstoreValue(ir, &ic2, mx);
        return;
    }

    UINT ist_size = ir->getTypeSize(m_tm);
    if (IST_ofst(ir) != 0 || ist_size != IST_base(ir)->getTypeSize(m_tm)) {
        UINT ist_ofst = IST_ofst(ir);
        //Compute where IST_base may point to.
        MDSet tmp;
        bool change = false;
        SEGIter * iter;
        for (INT i = ml_may_pt.get_first(&iter);
             i >= 0; i = ml_may_pt.get_next((UINT)i, &iter)) {
            MD * l = m_md_sys->getMD((UINT)i);
            ASSERT0(l);
            if (l->is_exact()) {
                MD md(*l);
                MD_ofst(&md) += ist_ofst;

                //Note if ir's type is VOID, size is 0.
                //Thus the MD indicates a object that is
                //p + ist_ofst + 0, if ist_ofst exist.
                MD_size(&md) = ist_size;
                MD const* entry = m_md_sys->registerMD(md);
                ASSERT(MD_id(entry) > 0, ("Not yet registered"));
                tmp.bunion(entry, *m_misc_bs_mgr);
                change = true;
            } else {
                tmp.bunion(l, *m_misc_bs_mgr);
            }
        }

        if (change) { ml_may_pt.copy(tmp, *m_misc_bs_mgr); }
        tmp.clean(*m_misc_bs_mgr);
    }

    MD const* x;
    SEGIter * iter;
    if (ml_may_pt.get_elem_count() == 1 &&
        !MD_is_may(x = m_md_sys->getMD((UINT)ml_may_pt.get_first(&iter)))) {
        if (x->is_exact() && !ir->is_void() && ist_size != MD_size(x)) {
            MD md(*x);

            //Note if ir's type is VOID, size is 0.
            //Thus the MD indicates a object that is
            //p + ist_ofst + 0, if ist_ofst exist.
            MD_size(&md) = ist_size;
            MD const* entry = m_md_sys->registerMD(md);
            ASSERT(MD_id(entry) > 0, ("Not yet registered"));
            x = entry;
        }
        setMustAddr(ir, x);
        ir->cleanRefMDSet();
    } else {
        //Set ir has no exact mem-addr for convervative.
        ir->cleanRefMD();
        setMayAddr(ir, m_mds_hash->append(ml_may_pt));
    }

    AACtx ic2;
    inferIstoreValue(ir, &ic2, mx);
    ml_may_pt.clean(*m_misc_bs_mgr);
}


//NOTE: The def and use info should be available for region, otherwise
//this function will be costly.
void IR_AA::processRegion(IR const* ir, IN MD2MDSet * mx)
{
    Region * rg = REGION_ru(ir);
    ASSERT0(rg);
    //ASSERT0(REGION_type(rg) == REGION_BLACKBOX || REGION_type(rg) == REGION_INNER);

    if (rg->is_readonly()) { return; }

    bool has_maydef_info = false;
    //Check if region modify or use MD.
    MDSet const* defmds = rg->getMayDef();
    if (defmds != NULL && !defmds->is_empty()) {
        ElemCopyPointToAndMayPointTo(*defmds, mx);
        has_maydef_info = true;
    }

    if (!has_maydef_info && (rg->is_blackbox() || rg->is_subregion())) {
        //For conservative purpose, region may change
        //global variable's point-to and local variable's
        //point-to which are forseeable.
        processRegionSideeffect(*mx);
    }
}


void IR_AA::processRegionSideeffect(IN OUT MD2MDSet & mx)
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

        VAR const* v = t->get_base();
        if (v->is_pointer() ||
            v->is_void()) { //v may be pointer if its type is VOID
            setPointTo((UINT)j, mx, m_maypts);

            //Set the point-to set of 't' to be empty in order
            //to enforce its point-to set will be recomputed if
            //necessary.
            //cleanPointTo(t, mx);
        }
    }
}


void IR_AA::processReturn(IN IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_return());
    if (RET_exp(ir) != NULL) {
        MDSet tmp;
        AACtx tic;
        inferExpression(RET_exp(ir), tmp, &tic, mx);
        tmp.clean(*m_misc_bs_mgr);
    }
}


void IR_AA::processCallSideeffect(IN OUT MD2MDSet & mx, MDSet const& by_addr_mds)
{
    //Set all mds which are global pointers or parameters which taken
    //address point to maypts.
    MDId2MD const* id2md = m_md_sys->getID2MDMap();
    for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
        MD const* t = id2md->get((UINT)j);
        if (t == NULL) { continue; }

        VAR const* v = t->get_base();
        if (VAR_is_global(v) &&
            (v->is_pointer() ||
             v->is_void())) { //v may be pointer if its type is VOID
            setPointTo((UINT)j, mx, m_maypts);

            //Set the point-to set of 't' to be empty in order
            //to enforce its point-to set will be recomputed if
            //necessary.
            //cleanPointTo(t, mx);
        }
    }

    if (by_addr_mds.is_empty()) { return; }

    SEGIter * iter;
    for (INT j = by_addr_mds.get_first(&iter);
         j >= 0; j = by_addr_mds.get_next((UINT)j, &iter)) {
        MD const* t = m_md_sys->getMD((UINT)j);
        ASSERT0(t != NULL);
        VAR const* v = t->get_base();
        if (VAR_is_addr_taken(v) &&
            (v->is_pointer() ||
             v->is_void())) { //v may be pointer if its type is VOID
            setPointTo((UINT)j, mx, m_maypts);

            //Set the point-to set of 't' to be empty in order
            //to enforce its point-to set will be recomputed if
            //necessary.
            //cleanPointTo(t, mx);
        }
    }
}


MD const* IR_AA::allocHeapobj(IR * ir)
{
    MD const* heap_obj = m_ir2heapobj.get(ir);
    if (heap_obj != NULL) {
        return heap_obj;
    }

    CHAR name[128];
    sprintf(name, "heap_obj%d", m_ir2heapobj.get_elem_count());
    ASSERT0(strlen(name) < 128);
    VAR * tv = m_ru->getVarMgr()->registerVar(
        name, m_tm->getMCType(0), 0, VAR_GLOBAL);

    //Set the var to be unallocable, means do NOT add
    //var immediately as a memory-variable.
    //For now, it is only be regarded as a placeholder.
    //And set it to allocable if the var is in essence need to be
    //allocated in memory.
    VAR_is_unallocable(tv) = true;

    //Will be freed region destruction.
    m_ru->addToVarTab(tv);

    MD md;
    MD_base(&md) = tv;
    MD_ty(&md) = MD_UNBOUND;
    MD const* entry = m_md_sys->registerMD(md);
    ASSERT0(MD_id(entry) > 0);
    m_ir2heapobj.set(ir, entry);
    return entry;
}


//Compute the point-to set modification when we meet call.
void IR_AA::processCall(IN IR * ir, IN MD2MDSet * mx)
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
        if (p->is_ptr() || p->is_void()) {
            AC_comp_pt(&tic) = true;
        }

        inferExpression(p, tmp, &tic, mx);

        if (!ir->isReadOnlyCall() &&
            (AC_comp_pt(&tic) || AC_has_comp_lda(&tic))) {
            if (!tmp.is_empty()) {
                by_addr_mds.bunion(tmp, *m_misc_bs_mgr);
            } else if (AC_returned_pts(&tic) != NULL) {
                if (!by_addr_mds.is_equal(*AC_returned_pts(&tic))) {
                    by_addr_mds.bunion(*AC_returned_pts(&tic), *m_misc_bs_mgr);
                }
            } else {
                by_addr_mds.bunion(tmp, *m_misc_bs_mgr);
            }
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
                t = allocCallResultPRMD(ir);
            } else {
                t = getMustAddr(ir);
            }
            setPointToUniqueMD(MD_id(t), *mx, allocHeapobj(ir));
        }

        //The function such as malloc or new function should not modify
        //the memory in XOC scope.
        tmp.clean(*m_misc_bs_mgr);
        by_addr_mds.clean(*m_misc_bs_mgr);;
        return;
    }

    //Analyz return-values.
    if (ir->hasReturnValue()) {
        MD const* t = NULL;
        if (!m_is_visit.is_contain(ir->id())) {
            m_is_visit.bunion(ir->id());
            t = allocCallResultPRMD(ir);
        } else {
            t = getMustAddr(ir);
        }

        ASSERT(t, ("result of call miss exact MD."));

        if (ir->is_ptr() || ir->is_void()) {
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

    if (ir->isReadOnlyCall()) {
        //Readonly call does not modify any point-to informations.
        tmp.clean(*m_misc_bs_mgr);
        by_addr_mds.clean(*m_misc_bs_mgr);
        return;
    }

    processCallSideeffect(*mx, by_addr_mds);
    tmp.clean(*m_misc_bs_mgr);
    by_addr_mds.clean(*m_misc_bs_mgr);
}


//Analyze the Tree style memory-address-expression,
//and compute the MDSet for 'expr'.
//'expr': IR expressions that describing memory address.
//'mds': record memory descriptors which 'expr' might express.
//'ic': context of analysis.
void IR_AA::inferExpression(
        IR * expr,
        IN OUT MDSet & mds,
        IN OUT AACtx * ic,
        IN OUT MD2MDSet * mx)
{
    switch (expr->getCode()) {
    case IR_ID:
        assignIdMD(expr, &mds, ic);
        return;
    case IR_ILD:
        processIld(expr, mds, ic, mx);
        return;
    case IR_LD:
        assignLoadMD(expr, &mds, ic, mx);
        return;
    case IR_LDA:
        processLda(expr, mds, ic);
        return;
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
    case IR_LOR:
        {
            //CASE: if (p && q)
            //GR: land (ld p:*<2>, ld q:*<2>)
            //ASSERT(!BIN_opnd0(expr)->is_ptr(),
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
                mds.copy(*m_maypts, *m_misc_bs_mgr);
            } else {
                mds.clean(*m_misc_bs_mgr);
            }
        }
        return;
    case IR_BAND:
    case IR_BOR:
    case IR_XOR:
        {
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
                mds.copy(*m_maypts, *m_misc_bs_mgr);
            } else {
                mds.clean(*m_misc_bs_mgr);
            }
        }
        return;
    case IR_BNOT:
    case IR_NEG:
    case IR_LNOT:
        {
            //opnd0 may be pointer.
            AACtx tic(*ic);
            AC_comp_pt(&tic) = false;
            inferExpression(UNA_opnd(expr), mds, &tic, mx);

            //These expressions does not descripte
            //an accurate memory-address. So, for the
            //conservative purpose, we claim that can
            //not find any MD.
            if (AC_comp_pt(ic)) {
                mds.copy(*m_maypts, *m_misc_bs_mgr);
            } else {
                mds.clean(*m_misc_bs_mgr);
            }
        }
        return;
    case IR_LT:
    case IR_LE:
    case IR_GT:
    case IR_GE:
    case IR_EQ:
    case IR_NE:
        {
            AACtx tic(*ic);
            AC_comp_pt(&tic) = false;
            inferExpression(BIN_opnd0(expr), mds, &tic, mx);

            tic.cleanBottomUpFlag();
            inferExpression(BIN_opnd1(expr), mds, &tic, mx);
            if (AC_comp_pt(ic)) {
                mds.copy(*m_maypts, *m_misc_bs_mgr);
            } else {
                mds.clean(*m_misc_bs_mgr);
            }
        }
        return;
    case IR_LABEL: return;
    case IR_SELECT:
        {
            AACtx tic(*ic);
            AC_comp_pt(&tic) = false;

            inferExpression(SELECT_pred(expr), mds, &tic, mx);

            tic.cleanBottomUpFlag();
            inferExpression(SELECT_trueexp(expr), mds, &tic, mx);

            tic.cleanBottomUpFlag();
            inferExpression(SELECT_falseexp(expr), mds, &tic, mx);
            if (AC_comp_pt(ic)) {
                //We do not know if condition is true or false.
                mds.copy(*m_maypts, *m_misc_bs_mgr);
            } else {
                mds.clean(*m_misc_bs_mgr);
            }
        }
        return;
    default: UNREACHABLE();
    }
}


//Set POINT TO info.
//Set each md in 'mds' add set 'pt_set'.
void IR_AA::ElemUnionPointTo(
        MDSet const& mds,
        MDSet const& pt_set,
        IN MD2MDSet * mx)
{
    bool set_all = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (isFullMem((UINT)i)) {
            set_all = true;
            MDId2MD const* id2md = m_md_sys->getID2MDMap();
            for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
                ASSERT0(id2md->get((UINT)j));
                setPointToMDSetByAddMDSet((UINT)j, *mx, pt_set);
            }
        } else if (isHeapMem((UINT)i)) {
            if (set_all) { continue; }
            for (INT j = m_id2heap_md_map.get_first();
                 j >= 0; j = m_id2heap_md_map.get_next((UINT)j)) {
                ASSERT0(m_md_sys->getMD((UINT)j));
                setPointToMDSetByAddMDSet((UINT)j, *mx, pt_set);
            }
        } else {
            setPointToMDSetByAddMDSet((UINT)i, *mx, pt_set);
        }
    }
}


//Set POINT TO info.
//Set each md in 'mds' add 'pt_elem'.
void IR_AA::ElemUnionPointTo(
        MDSet const& mds,
        IN MD * pt_elem,
        IN MD2MDSet * mx)
{
    bool set_all = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (isFullMem((UINT)i)) {
            set_all = true;
            MDId2MD const* id2md = m_md_sys->getID2MDMap();
            for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
                ASSERT0(id2md->get((UINT)j));
                setPointToMDSetByAddMD((UINT)j, *mx, pt_elem);
            }
            return;
        } else if (isHeapMem((UINT)i)) {
            if (set_all) { continue; }
            for (INT j = m_id2heap_md_map.get_first();
                 j >= 0; j = m_id2heap_md_map.get_next((UINT)j)) {
                ASSERT0(m_md_sys->getMD((UINT)j));
                setPointToMDSetByAddMD((UINT)j, *mx, pt_elem);
            }
        } else {
            setPointToMDSetByAddMD((UINT)i, *mx, pt_elem);
        }
    }
}


//Set POINT TO info.
//Set each md in 'mds' points to 'pt_set' if it is exact, or
//else unify the set.
void IR_AA::ElemCopyAndUnionPointTo(
        MDSet const& mds,
        MDSet const& pt_set,
        IN MD2MDSet * mx)
{
    bool set_all = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (isFullMem((UINT)i)) {
            set_all = true;

            MDId2MD const* id2md = m_md_sys->getID2MDMap();
            for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
                MD * t = id2md->get((UINT)j);
                if (t->is_exact()) {
                    setPointToMDSet((UINT)j, *mx, pt_set);
                } else {
                    setPointToMDSetByAddMDSet((UINT)j, *mx, pt_set);
                }
            }
            return;
        } else if (isHeapMem((UINT)i)) {
            if (set_all) { continue; }

            for (INT j = m_id2heap_md_map.get_first();
                 j >= 0; j = m_id2heap_md_map.get_next((UINT)j)) {
                MD * t = m_md_sys->getMD((UINT)j);
                ASSERT0(t);
                if (t->is_exact()) {
                    setPointToMDSet(MD_id(t), *mx, pt_set);
                } else {
                    setPointToMDSetByAddMDSet(MD_id(t), *mx, pt_set);
                }
            }
        } else {
            if (m_md_sys->getMD((UINT)i)->is_exact()) {
                if (&pt_set == m_maypts) {
                    setPointTo((UINT)i, *mx, m_maypts);
                } else {
                    setPointToMDSet((UINT)i, *mx, pt_set);
                }
            } else {
                setPointToMDSetByAddMDSet((UINT)i, *mx, pt_set);
            }
        }
    }
}


//Set POINT TO info.
//Set each md in 'mds' points to 'pt_set'.
void IR_AA::ElemCopyPointTo(
        MDSet const& mds,
        IN MDSet & pt_set,
        IN MD2MDSet * mx)
{
    bool set_all = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (isFullMem((UINT)i)) {
            set_all = true;
            MDId2MD const* id2md = m_md_sys->getID2MDMap();
            for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
                ASSERT0(id2md->get((UINT)j));
                setPointToMDSet((UINT)j, *mx, pt_set);
            }
            return;
        } else if (isHeapMem((UINT)i)) {
            if (set_all) { continue; }
            for (INT j = m_id2heap_md_map.get_first();
                 j >= 0; j = m_id2heap_md_map.get_next((UINT)j)) {
                ASSERT0(m_md_sys->getMD((UINT)j));
                setPointToMDSet((UINT)j, *mx, pt_set);
            }
        } else {
            setPointToMDSet((UINT)i, *mx, pt_set);
        }
    }
}


//Set POINT TO info.
//Set each md in 'mds' points to May-Point-To set.
void IR_AA::ElemCopyPointToAndMayPointTo(MDSet const& mds, IN MD2MDSet * mx)
{
    bool set_all = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (isFullMem((UINT)i)) {
            set_all = true;
            MDId2MD const* id2md = m_md_sys->getID2MDMap();
            for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
                ASSERT0(id2md->get((UINT)j));
                setPointTo((UINT)j, *mx, m_maypts);
            }
            return;
        } else if (isHeapMem((UINT)i)) {
            if (set_all) { continue; }
            for (INT j = m_id2heap_md_map.get_first();
                 j >= 0; j = m_id2heap_md_map.get_next((UINT)j)) {
                ASSERT0(m_md_sys->getMD((UINT)j));
                setPointTo((UINT)j, *mx, m_maypts);
            }
        } else {
            setPointTo((UINT)i, *mx, m_maypts);
        }
    }
}


//Set POINT TO info.
//Set md in 'mds' points to NULL if it is exact.
void IR_AA::ElemCleanExactPointTo(MDSet const& mds, IN MD2MDSet * mx)
{
    bool set_all = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (isFullMem((UINT)i)) {
            set_all = true;
            MDId2MD const* id2md = m_md_sys->getID2MDMap();
            for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
                ASSERT0(id2md->get((UINT)j));
                cleanPointTo((UINT)j, *mx);
            }
        } else if (isHeapMem((UINT)i)) {
            if (set_all) { continue; }
            for (INT j = m_id2heap_md_map.get_first();
                 j >= 0; j = m_id2heap_md_map.get_next((UINT)j)) {
                ASSERT0(m_md_sys->getMD((UINT)j));
                cleanPointTo((UINT)j, *mx);
            }
        } else if (m_md_sys->getMD((UINT)i)->is_exact()) {
            cleanPointTo((UINT)i, *mx);
        }
    }
}


//Set POINT TO info.
//Set md in 'mds' points to NULL.
void IR_AA::ElemCleanPointTo(MDSet const& mds, IN MD2MDSet * mx)
{
    bool set_all = false;
    SEGIter * iter;
    for (INT i = mds.get_first(&iter);
         i >= 0; i = mds.get_next((UINT)i, &iter)) {
        ASSERT0(m_md_sys->getMD((UINT)i));
        if (isFullMem((UINT)i)) {
            set_all = true;
            MDId2MD const* id2md = m_md_sys->getID2MDMap();
            for (INT j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
                ASSERT0(id2md->get((UINT)j));
                cleanPointTo((UINT)j, *mx);
            }
            return;
        } else if (isHeapMem((UINT)i)) {
            if (set_all) { continue; }
            for (INT j = m_id2heap_md_map.get_first();
                 j >= 0; j = m_id2heap_md_map.get_next((UINT)j)) {
                ASSERT0(m_md_sys->getMD((UINT)j));
                cleanPointTo((UINT)j, *mx);
            }
        } else {
            cleanPointTo((UINT)i, *mx);
        }
    }
}


//Dump IR's point-to of each BB.
void IR_AA::dumpInOutPointToSetForBB()
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n==---- DUMP POINT TO INFO ----==");
    BBList * bbl = m_cfg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        PtPairSet * in_set = getInPtPairSet(bb);
        PtPairSet * out_set = getOutPtPairSet(bb);
        fprintf(g_tfile, "\n--- BB%d ---", BB_id(bb));
        fprintf(g_tfile, "\nIN-SET::");
        dumpPtPairSet(*in_set);
        fprintf(g_tfile, "\n\nOUT-SET::");
        dumpPtPairSet(*out_set);
    }
    fflush(g_tfile);
}


//Dump POINT-TO pair record in 'pps'.
void IR_AA::dumpPtPairSet(PtPairSet & pps)
{
    if (g_tfile == NULL) return;
    StrBuf buf(256);
    UINT k = 0;
    bool detail = true;
    for (INT i = pps.get_first(); i >= 0; i = pps.get_next((UINT)i), k++) {
        PtPair * pp = m_pt_pair_mgr.get((UINT)i);
        ASSERT0(pp);
        fprintf(g_tfile, "\nMD%u->MD%u,  ", PP_from(pp), PP_to(pp));

        if (!detail) { continue; }

        MD const* from = m_md_sys->getMD(PP_from(pp));
        ASSERT0(from);

        fprintf(g_tfile, "%s", from->get_base()->dump(buf, m_tm));
        if (from->is_exact()) {
            fprintf(g_tfile, ":ofst(%u):size(%u)",
                MD_ofst(from), MD_size(from));
        } else {
            fprintf(g_tfile, ":ofst(--):size(%u)", MD_size(from));
        }

        fprintf(g_tfile, " ------> ");

        MD const* to = m_md_sys->getMD(PP_to(pp));

        buf.clean();
        fprintf(g_tfile, "%s", to->get_base()->dump(buf, m_tm));

        if (to->is_exact()) {
            fprintf(g_tfile, ":ofst(%u):size(%u)", MD_ofst(to), MD_size(to));
        } else {
            fprintf(g_tfile, ":ofst(--):size(%u)", MD_size(to));
        }
    }

    fflush(g_tfile);
}


//Dump 'ir' point-to according to 'mx'.
//'dump_kid': dump kid's memory object if it exist.
void IR_AA::dumpIRPointTo(IN IR * ir, bool dump_kid, IN MD2MDSet * mx)
{
    if (ir == NULL || g_tfile == NULL) return;
    MD const* must = getMustAddr(ir);
    MDSet const* may = getMayAddr(ir);
    if (must != NULL ||
        (may != NULL && may->get_elem_count() > 0)) {
        dump_ir(ir, m_tm, NULL, false, false, false, false);
    }

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
            SEGIter * iter;
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
    fflush(g_tfile);
}


//Dump all relations between IR, MD, and MDSet.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void IR_AA::dumpIRPointToForBB(IRBB * bb, bool dump_kid)
{
    if (g_tfile == NULL) { return; }
    fprintf(g_tfile, "\n\n--- BB%u ---", BB_id(bb));
    xcom::C<IR*> * ct;
    MD2MDSet * mx;
    if (m_flow_sensitive) {
        mx = mapBBtoMD2MDSet(BB_id(bb));
    } else {
        mx = &m_unique_md2mds;
    }
    if (mx == NULL) {
        //e.g: If one has performed PRE and generated new BB, but
        //not invoked the IRAA::perform(), then the mx of
        //the new BB is not constructed.

        //interwarn("In IRAA, MD2MDSet of BB%u is NULL, may be new "
        //          "bb was generated. One should recall IRAA::perform()",
        //          BB_id(bb));
        fprintf(g_tfile, "\n--- BB%u's MD2MDSet is NULL ---", BB_id(bb));
        return;
    }

    dumpMD2MDSet(mx, true);
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != NULL; ir = BB_irlist(bb).get_next(&ct)) {
        fprintf(g_tfile, "\n---------");
        g_indent = 4;
        dump_irs(ir, m_tm, NULL, true, true, false, false);
        fprintf(g_tfile, "\n");

        ASSERT0(isValidStmtToAA(ir));

        switch (ir->getCode()) {
        case IR_ST:
            fprintf(g_tfile, "LHS:");
            dumpIRPointTo(ir, false, mx);
            fprintf(g_tfile, "\nRHS:");
            dumpIRPointTo(ST_rhs(ir), false, mx);

            if (dump_kid) {
                fprintf(g_tfile, "\n>> MDSet DETAIL:");
                dumpIRPointTo(ST_rhs(ir), true, mx);
            }
            break;
        case IR_STPR:
            fprintf(g_tfile, "LHS:");
            dumpIRPointTo(ir, false, mx);
            fprintf(g_tfile, "\nRHS:");
            dumpIRPointTo(STPR_rhs(ir), false, mx);
            if (dump_kid) {
                fprintf(g_tfile, "\n>> MDSet DETAIL:");
                dumpIRPointTo(STPR_rhs(ir), true, mx);
            }
            break;
        case IR_STARRAY:
            fprintf(g_tfile, "LHS:");
            dumpIRPointTo(ir, false, mx);
            fprintf(g_tfile, "\nRHS:");
            dumpIRPointTo(STARR_rhs(ir), false, mx);
            if (dump_kid) {
                fprintf(g_tfile, "\n>> MDSet DETAIL:");
                dumpIRPointTo(ARR_base(ir), true, mx);
                dumpIRPointTo(STARR_rhs(ir), true, mx);
                for (IR * p = ARR_sub_list(ir); p != NULL; p = p->get_next()) {
                    dumpIRPointTo(p, true, mx);
                }
            }
            break;
        case IR_IST:
            fprintf(g_tfile, "LHS:");
            dumpIRPointTo(ir, false, mx);
            fprintf(g_tfile, "\nRHS:");
            dumpIRPointTo(IST_rhs(ir), false, mx);
            if (dump_kid) {
                fprintf(g_tfile, "\n>> MDSet DETAIL:");
                dumpIRPointTo(IST_base(ir), true, mx);
                dumpIRPointTo(IST_rhs(ir), true, mx);
            }
            break;
        case IR_CALL:
            {
                if (ir->hasReturnValue()) {
                    fprintf(g_tfile, "LHS:");
                    dumpIRPointTo(ir, false, mx);
                }

                UINT i = 0;
                for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
                    fprintf(g_tfile, "\nPARAM%u:", i++);
                    dumpIRPointTo(p, false, mx);
                }

                i = 0;
                for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
                    fprintf(g_tfile, "\nDUMMY%u:", i++);
                    dumpIRPointTo(p, false, mx);
                }

                if (dump_kid) {
                    if (CALL_param_list(ir) != NULL ||
                        CALL_dummyuse(ir) != NULL) {
                        fprintf(g_tfile, "\n>> MDSet DETAIL:\n");
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
            }
            break;
        case IR_ICALL: //indirective call
            {
                if (ir->hasReturnValue()) {
                    fprintf(g_tfile, "LHS:");
                    dumpIRPointTo(ir, false, mx);
                }

                ASSERT0(ICALL_callee(ir) != NULL);
                fprintf(g_tfile, "CALLEE:");
                dumpIRPointTo(ICALL_callee(ir), false, mx);

                if (dump_kid && CALL_param_list(ir) != NULL) {
                    fprintf(g_tfile, "\n>> MDSet DETAIL:\n");
                    for (IR * p = CALL_param_list(ir); p ; p = p->get_next()) {
                        dumpIRPointTo(p, true, mx);
                    }
                }
            }
            break;
        case IR_GOTO:
            break;
        case IR_IGOTO:
            ASSERT0(IGOTO_vexp(ir) != NULL);
            fprintf(g_tfile, "VEXP:");
            dumpIRPointTo(IGOTO_vexp(ir), false, mx);
            if (dump_kid) {
                fprintf(g_tfile, "\n>> MDSet DETAIL:");
                dumpIRPointTo(IGOTO_vexp(ir), true, mx);
            }
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            ASSERT0(BR_det(ir) != NULL);
            fprintf(g_tfile, "DET:");
            dumpIRPointTo(BR_det(ir), false, mx);
            if (dump_kid) {
                fprintf(g_tfile, "\n>> MDSet DETAIL:");
                dumpIRPointTo(BR_det(ir), true, mx);
            }
            break;
        case IR_SELECT:
            ASSERT0(SELECT_pred(ir) != NULL);
            fprintf(g_tfile, "DET:");
            dumpIRPointTo(SELECT_pred(ir), false, mx);
            if (dump_kid) {
                fprintf(g_tfile, "\n>> MDSet DETAIL:");
                dumpIRPointTo(SELECT_pred(ir), true, mx);
            }
            break;
        case IR_SWITCH:
            ASSERT0(SWITCH_vexp(ir) != NULL);
            fprintf(g_tfile, "VEXP:");
            dumpIRPointTo(SWITCH_vexp(ir), false, mx);
            if (dump_kid) {
                fprintf(g_tfile, "\n>> MDSet DETAIL:");
                dumpIRPointTo(SWITCH_vexp(ir), true, mx);
            }
            break;
        case IR_RETURN:
            {
                if (RET_exp(ir) != NULL) {
                    dumpIRPointTo(RET_exp(ir), false, mx);
                }

                if (dump_kid && RET_exp(ir) != NULL) {
                    fprintf(g_tfile, "\n>> MDSet DETAIL:");
                    dumpIRPointTo(RET_exp(ir), true, mx);
                }
            }
            break;
        case IR_PHI:
            {
                fprintf(g_tfile, "LHS:");
                dumpIRPointTo(ir, false, mx);

                for (IR * p = PHI_opnd_list(ir); p; p = p->get_next()) {
                    dumpIRPointTo(p, false, mx);
                }

                ASSERT0(PHI_opnd_list(ir));
                if (dump_kid) {
                    fprintf(g_tfile, "\n>> MDSet DETAIL:\n");
                    for (IR * p = PHI_opnd_list(ir); p; p = p->get_next()) {
                        dumpIRPointTo(p, true, mx);
                    }
                }
            }
            break;
        case IR_REGION: break;
        default: UNREACHABLE();
        } //end switch
    }
}


//Dump all relations between IR, MD, and MDSet.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void IR_AA::dumpIRPointToForRegion(bool dump_kid)
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n==--- DUMP '%s' IR_AA : IR ADDRESS and POINT-TO ----==",
            m_ru->getRegionName());
    m_md_sys->dumpAllMD();
    fprintf(g_tfile, "\n\n---- All MD in MAY-POINT-TO SET : ");

    ASSERT0(m_maypts);
    m_maypts->dump(m_md_sys, true);

    BBList * bbl = m_cfg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        dumpIRPointToForBB(bb, dump_kid);
    }
    fflush(g_tfile);
}


void IR_AA::dumpMayPointTo()
{
    if (g_tfile == NULL || m_maypts == NULL) { return; }

    SEGIter * iter;
    for (INT j = m_maypts->get_first(&iter);
         j >= 0; j = m_maypts->get_next((UINT)j, &iter)) {
        MD * mmd = m_md_sys->getMD((UINT)j);
        ASSERT0(mmd != NULL);
        fprintf(g_tfile, "MD%u,", MD_id(mmd));
    }
    fflush(g_tfile);
}


void IR_AA::dump(CHAR const* name)
{
    FILE * old = NULL;
    if (name != NULL) {
        old = g_tfile;
        //UNLINK(name);
        g_tfile = fopen(name, "a+");
        ASSERT(g_tfile, ("%s create failed!!!", name));
    }

    dumpMD2MDSetForRegion(false);

    fflush(g_tfile);
    if (name != NULL) {
        fclose(g_tfile);
        g_tfile = old;
    }
}


//Dump MD's point-to for each BB.
void IR_AA::dumpMD2MDSetForRegion(bool dump_pt_graph)
{
    if (g_tfile == NULL) return;
    if (m_flow_sensitive) {
        fprintf(g_tfile, "\n\n==---- DUMP ALL MD POINT-TO "
                         "OUT-SET (FLOW SENSITIVE) ----==");
        BBList * bbl = m_cfg->getBBList();
        for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
            fprintf(g_tfile, "\n\n--- BB%u ---", BB_id(bb));
            dumpMD2MDSet(mapBBtoMD2MDSet(BB_id(bb)),
                false); //each BB has its own graph.
        }
    } else {
        fprintf(g_tfile, "\n\n==---- DUMP ALL MD POINT-TO "
                         "OUT-SET (FLOW-INSENSITIVE) ----==");
        dumpMD2MDSet(&m_unique_md2mds, dump_pt_graph);
    }
    fflush(g_tfile);
}


//Dump MD's point-to according to individual 'mx'.
//'dump_ptg': dump POINT-TO graph.
void IR_AA::dumpMD2MDSet(IN MD2MDSet * mx, bool dump_ptg)
{
    if (g_tfile == NULL || mx == NULL) return;
    xcom::Graph g;
    MDId2MD const* id2md = m_md_sys->getID2MDMap();
    for (INT i = MD_FIRST; i <= id2md->get_last_idx(); i++) {
        if (id2md->get((UINT)i) == NULL) { continue; }

        MDSet const* mds = getPointTo((UINT)i, *mx);
        if (mds != NULL) {
            fprintf(g_tfile, "\nMD%u -- PT_SET: ", (UINT)i);
            SEGIter * iter;
            for (INT j = mds->get_first(&iter);
                 j >= 0; j = mds->get_next((UINT)j, &iter)) {
                ASSERT0(m_md_sys->getMD((UINT)j));
                fprintf(g_tfile, "MD%u,", (UINT)j);
                if (dump_ptg) {
                    g.addEdge((UINT)i, (UINT)j);
                }
            }
        } else {
            fprintf(g_tfile, "\nMD%u -- NO PT", (UINT)i);
        }
    }

    if (dump_ptg) {
        g.dump_vcg("graph_point_to.vcg");
    }
    fflush(g_tfile);
}


//Dump relations between MD, MDSet.
//'md': candidate to dump.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void IR_AA::dumpMD2MDSet(MD const* md, IN MD2MDSet * mx)
{
    if (g_tfile == NULL) { return; }

    StrBuf buf(64);
    fprintf(g_tfile, "\n\t  %s", md->dump(buf, m_tm));

    //Dump MDSet of 'md'.
    MDSet const* pts = getPointTo(MD_id(md), *mx);
    fprintf(g_tfile, "\n\t\tPOINT TO:");
    if (pts != NULL && !pts->is_empty()) {
        fprintf(g_tfile, "\n");
        SEGIter * iter;
        for (INT j = pts->get_first(&iter);
             j >= 0; j = pts->get_next((UINT)j, &iter)) {
            MD const* mmd = m_md_sys->getMD((UINT)j);
            ASSERT0(mmd);

            buf.clean();
            fprintf(g_tfile, "\t\t\t%s\n", mmd->dump(buf, m_tm));
        }
    } else {
        fprintf(g_tfile, "----");
    }
    fflush(g_tfile);
}


//Return false if flow sensitive analysis is inproperly.
bool IR_AA::convertMD2MDSet2PT(
        OUT PtPairSet & pps,
        IN PtPairMgr & pt_pair_mgr,
        IN MD2MDSet * mx)
{
    MD2MDSetIter mxiter;
    MDSet const* from_md_pts = NULL;

    //Grow pps before hand with the maximum length needed.
    UINT num_of_tgt_md = 0;
    for (UINT fromid = mx->get_first(mxiter, &from_md_pts);
        fromid > 0; fromid = mx->get_next(mxiter, &from_md_pts)) {
        ASSERT0(m_md_sys->getMD(fromid));
        if (from_md_pts == NULL || from_md_pts->is_contain_all()) {
            continue;
        }
        num_of_tgt_md += from_md_pts->get_elem_count();
    }
    if (num_of_tgt_md != 0) {
        num_of_tgt_md = (num_of_tgt_md * mx->get_elem_count() /
            HOST_BIT_PER_BYTE + 1) *
            HOST_BIT_PER_BYTE / HOST_BIT_PER_BYTE;
        if (pps.get_byte_size() < num_of_tgt_md) {
            ASSERT0(pps.is_empty());
            pps.alloc(num_of_tgt_md); //alloc will destroy the original buffer.
        }
    }
    if (num_of_tgt_md > g_thres_flow_sensitive_aa) {
        return false;
    }

    for (UINT fromid = mx->get_first(mxiter, &from_md_pts);
         fromid > 0; fromid = mx->get_next(mxiter, &from_md_pts)) {
        ASSERT0(m_md_sys->getMD(fromid));
        if (from_md_pts == NULL) { continue; }
        if (from_md_pts->is_contain_all()) {
            PtPair const* pp = pt_pair_mgr.add(fromid, MD_FULL_MEM);
            ASSERT0(pp);
            pps.bunion(PP_id(pp));
        } else {
            SEGIter * segiter;
            for (INT toid = from_md_pts->get_first(&segiter);
                 toid >= 0; toid = from_md_pts->get_next((UINT)toid, &segiter)) {
                ASSERT0(m_md_sys->getMD((UINT)toid));
                PtPair const* pp = pt_pair_mgr.add(fromid, (UINT)toid);
                ASSERT0(pp);
                pps.bunion(PP_id(pp));
            }
        }
    }
    return true;
}


void IR_AA::convertPT2MD2MDSet(
        PtPairSet const& pps,
        IN PtPairMgr & pt_pair_mgr,
        IN OUT MD2MDSet * ctx)
{
    for (INT i = pps.get_first(); i >= 0; i = pps.get_next((UINT)i)) {
        PtPair * pp = pt_pair_mgr.get((UINT)i);
        ASSERT0(pp != NULL);
        setPointToMDSetByAddMD(PP_from(pp), *ctx, m_md_sys->getMD(PP_to(pp)));
    }
}


//Solving POINT-TO out set.
//While the function terminiate, OUT info has been recorded
//in related 'mx'.
void IR_AA::computeStmt(IRBB const* bb, IN OUT MD2MDSet * mx)
{
    ASSERT0(mx != NULL);
    xcom::C<IR*> * ct;
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
            processIst(ir, mx);
            break;
        case IR_CALL:
        case IR_ICALL:
            processCall(ir, mx);
            break;
        case IR_GOTO:
            ASSERT0(ir == BB_last_ir(readonly_bb));
            break;
        case IR_IGOTO:
            {
                ASSERT0(ir == BB_last_ir(readonly_bb));
                MDSet tmp;
                AACtx ic;
                inferExpression(IGOTO_vexp(ir), tmp, &ic, mx);
                tmp.clean(*m_misc_bs_mgr);
            }
            break;
        case IR_PHI:
            processPhi(ir, mx);
            break;
        case IR_REGION:
            processRegion(ir, mx);
            break;
        case IR_TRUEBR:
        case IR_FALSEBR:
            {
                ASSERT0(ir == BB_last_ir(readonly_bb));
                MDSet tmp;
                AACtx ic;
                inferExpression(BR_det(ir), tmp, &ic, mx);
                tmp.clean(*m_misc_bs_mgr);;
            }
            break;
        case IR_RETURN:
            ASSERT0(ir == BB_last_ir(readonly_bb));
            processReturn(ir, mx);
            break;
        case IR_SWITCH:
            {
                ASSERT0(ir == BB_last_ir(readonly_bb));
                MDSet tmp;
                AACtx ic;
                inferExpression(SWITCH_vexp(ir), tmp, &ic, mx);
                tmp.clean(*m_misc_bs_mgr);
            }
            break;
        default: ASSERT(0, ("unsupported IR type"));
        } //end switch
    } //end for
}


bool IR_AA::verifyIR(IR * ir)
{
    switch (ir->getCode()) {
    case IR_ID:
    case IR_LD:
    case IR_PR:
        ASSERT0(getMustAddr(ir));
        ASSERT0(getMayAddr(ir) == NULL);
        break;
    case IR_ST:
        ASSERT0(getMustAddr(ir));
        ASSERT0(getMayAddr(ir) == NULL);
        break;
    case IR_STPR:
    case IR_SETELEM:
    case IR_GETELEM:
        ASSERT0(getMustAddr(ir));
        ASSERT0(getMayAddr(ir) == NULL);
        break;
    case IR_STARRAY: {
        MD const* mustaddr = getMustAddr(ir);
        MDSet const* mayaddr = getMayAddr(ir);
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
            SEGIter * iter;
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
        MD const* mustaddr = getMustAddr(ir);
        MDSet const* mayaddr = getMayAddr(ir);
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
            SEGIter * iter;
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
            ASSERT0(getMustAddr(ir));
        }

        for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
            verifyIR(p);
        }

        for (IR * p = CALL_dummyuse(ir); p != NULL; p = p->get_next()) {
            verifyIR(p);
        }
        break;
    case IR_PHI:
        ASSERT0(getMustAddr(ir));
        ASSERT0(getMayAddr(ir) == NULL);
        break;
    default:
        ASSERT0(getMustAddr(ir) == NULL);
        ASSERT0(getMayAddr(ir) == NULL);
    }

    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != NULL) {
            verifyIR(kid);
        }
    }
    return true;
}


bool IR_AA::verify()
{
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head();
         bb != NULL; bb = bbl->get_next()) {
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            verifyIR(ir);
        }
    }
    return true;
}


//This method is accurate than Andersen's algo.
//NOTICE: Do NOT clean 'md2mds' of BB at the last iter,
//it supplied the POINT TO information for subsequently
//optimizations.
//Return false if flow sensitive analysis is inproperly.
bool IR_AA::computeFlowSensitive(List<IRBB*> const& bbl)
{
    bool change = true;
    UINT count = 0;
    PtPairSet tmp;
    for (; change && count < 20;) {
        count++;
        change = false;
        xcom::C<IRBB*> * ct = NULL;
        for (IRBB const* bb = bbl.get_head(&ct);
             bb != NULL; bb = bbl.get_next(&ct)) {
            PtPairSet * pps = getInPtPairSet(bb);

            MD2MDSet * md2mds = allocMD2MDSetForBB(BB_id(bb));
            tmp.clean();
            xcom::Vertex * vex = m_cfg->get_vertex(BB_id(bb));
            xcom::EdgeC * el = VERTEX_in_list(vex);
            if (el != NULL) {
                while (el != NULL) {
                    IRBB * p = m_cfg->getBB(VERTEX_id(EDGE_from(EC_edge(el))));
                    ASSERT0(p);
                    tmp.bunion(*getOutPtPairSet(p));
                    el = EC_next(el);
                }

                if (!pps->is_equal(tmp)) {
                    pps->copy(tmp);
                    change = true;
                }

                //Regard 'mx' as a reservation table to hold
                //the tmp info for MD->MDSet during each iteration.
                //Note that we must preserve MD->MDSet at the last
                //iteration. And it will be used during computing POINT-TO
                //info and DU analysis.
                md2mds->clean();
                convertPT2MD2MDSet(*pps, m_pt_pair_mgr, md2mds);
            }
            computeStmt(bb, md2mds);
            tmp.clean();
            if (!convertMD2MDSet2PT(tmp, m_pt_pair_mgr, md2mds)) {
                return false;
            }

            #ifdef _DEBUG_
            //MD2MDSet x;
            //convertPT2MD2MDSet(*out_set, m_pt_pair_mgr);
            //dumpMD2MDSet(&x, false);
            #endif
            pps = getOutPtPairSet(bb);
            if (!pps->is_equal(tmp)) {
                pps->copy(tmp);
                change = true;
            }
        }
    }
    ASSERT(!change, ("Iterated too many times"));
    return true;
}


//This function initialize the POINT-TO set of pointer.
//The pointer includes global pointer and formal parameter pointer.
//'param': formal parameter.
//Note May-POINT-TO set must be available before call this function.
void IR_AA::initGlobalAndParameterVarPtset(
        VAR * param,
        MD2MDSet * mx,
        ConstMDIter & iter)
{
    MD const* dmd = NULL; //dedicated MD which param pointed to.
    if (VAR_is_restrict(param)) {
        //If parameter is restrict, we allocate an individual variable to
        //distinguish its point-to with other parameters.
        dmd = m_var2md.get(param);
        if (dmd == NULL) {
            CHAR name[64];
            SNPRINTF(name, 63, "DummyGlobalVarPointedByVar%p", param);
            VAR * tv = m_ru->getVarMgr()->registerVar(
                name, m_tm->getMCType(0), 0, VAR_GLOBAL|VAR_ADDR_TAKEN);

            //Set the var to be unallocable, means do NOT add
            //var immediately as a memory-variable.
            //For now, it is only be regarded as a pseduo-register.
            //And set it to allocable if the PR is in essence need to be
            //allocated in memory.
            VAR_is_unallocable(tv) = true;
            m_ru->addToVarTab(tv);

            MD md;
            MD_base(&md) = tv;
            MD_ofst(&md) = 0;
            MD_size(&md) = 16; //anysize you want.
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
                ASSERT(getPointTo(MD_id(x), *mx) == NULL ||
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
            iter.clean();
            for (MD const* md = ofstab->get_first(iter, NULL);
                 md != NULL; md = ofstab->get_next(iter, NULL)) {
                if (dmd != NULL) {
                    ASSERT(getPointTo(MD_id(md), *mx) == NULL ||
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
        ASSERT(getPointTo(MD_id(entry), *mx) == NULL ||
               getPointTo(MD_id(entry), *mx)->is_empty(),
               ("should already be clean"));

        setPointToMDSetByAddMD(MD_id(entry), *mx, dmd);
    } else {
        setPointTo(MD_id(entry), *mx, m_maypts);
    }
}


//Determine if flow sensitive analysis is properly.
bool IR_AA::isFlowSensitiveProperly()
{
    IRBB * entry = m_cfg->get_entry();
    ASSERT0(entry);
    MD2MDSet * mx = allocMD2MDSetForBB(BB_id(entry));
    ASSERT(mx, ("invoke initEntryPtset before here"));
    MD2MDSetIter mxiter;
    MDSet const* from_md_pts = NULL;

    //Grow pps before hand with the maximum length needed.
    UINT num_of_tgt_md = 0;
    for (UINT fromid = mx->get_first(mxiter, &from_md_pts);
        fromid > 0; fromid = mx->get_next(mxiter, &from_md_pts)) {
        ASSERT0(m_md_sys->getMD(fromid));
        if (from_md_pts == NULL || from_md_pts->is_contain_all()) {
            continue;
        }
        num_of_tgt_md += from_md_pts->get_elem_count();
    }
    num_of_tgt_md = (num_of_tgt_md * mx->get_elem_count() /
        HOST_BIT_PER_BYTE + 1) *
        HOST_BIT_PER_BYTE / HOST_BIT_PER_BYTE;
    return num_of_tgt_md < g_thres_flow_sensitive_aa;
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
void IR_AA::initEntryPtset(PtPairSet ** ptset_arr)
{
    ConstMDIter iter;
    VarTab * vt = m_ru->getVarTab();
    if (m_flow_sensitive) {
        ASSERT0(m_cfg->verify());

        ASSERT0(m_ru->getBBList()->get_elem_count() != 0);

        BBList * rubblst = m_ru->getBBList();
        PtPairSet * loc_ptset_arr =
            new PtPairSet[rubblst->get_elem_count() * 2]();

        ASSERT0(ptset_arr);

        *ptset_arr = loc_ptset_arr;

        //Make vector to accommodate the max bb id.
        UINT bbnum = rubblst->get_elem_count();
        if (m_in_pp_set.get_capacity() < bbnum) {
            m_in_pp_set.grow(bbnum);
            m_out_pp_set.grow(bbnum);
            m_md2mds_vec.grow(bbnum);
        }

        UINT i = 0;
        xcom::C<IRBB*> * ct;
        for (IRBB * bb = rubblst->get_head(&ct);
             bb != NULL; bb = rubblst->get_next(&ct)) {
            m_in_pp_set.set(BB_id(bb), &loc_ptset_arr[i]);
            i++;
            m_out_pp_set.set(BB_id(bb), &loc_ptset_arr[i]);
            i++;
        }

        IRBB * entry = m_cfg->get_entry();
        ASSERT0(entry);
        MD2MDSet * mx = allocMD2MDSetForBB(BB_id(entry));
        setPointToAllMem(MD_FULL_MEM, *mx);
        setPointToGlobalMem(MD_GLOBAL_MEM, *mx);
        setPointToImportVar(MD_IMPORT_VAR, *mx);
        VarTabIter c;
        for (VAR * v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
            if (!VAR_is_global(v) && !VAR_is_formal_param(v)) { continue; }
            if (!v->is_pointer()) { continue; }

            //Variable with void-type may be pointer.
            //Deal with its point-to set while processing Load/PR
            //instead of initializing point-set at once.
            //if (!v->is_pointer() && !v->is_void()) { continue; }

            initGlobalAndParameterVarPtset(v, mx, iter);
        }
        convertMD2MDSet2PT(*getInPtPairSet(entry), m_pt_pair_mgr, mx);
    } else {
        setPointToAllMem(MD_FULL_MEM, m_unique_md2mds);
        setPointToGlobalMem(MD_GLOBAL_MEM, m_unique_md2mds);
        setPointToImportVar(MD_IMPORT_VAR, m_unique_md2mds);
        VarTabIter c;
        for (VAR * v = vt->get_first(c); v != NULL; v = vt->get_next(c)) {
            if (!VAR_is_global(v) && !VAR_is_formal_param(v)) { continue; }
            if (!v->is_pointer()) { continue; }

            //Variable with void-type may be pointer.
            //Deal with its point-to set while processing Load/PR
            //instead of initializing point-set at once.
            //if (!v->is_pointer() && !v->is_void()) { continue; }

            initGlobalAndParameterVarPtset(v, &m_unique_md2mds, iter);
        } //end for each VAR
    }
}


//This function initialize May Point-To set.
//Note that this function should only be invoked once.
void IR_AA::initMayPointToSet()
{
    //Record MDs whose address have been takens or it is global variable.
    Region * rg = m_ru;
    RegionMgr * rm = m_ru->getRegionMgr();
    VarTabIter c;
    ConstMDIter iter;
    MDSet tmp;
    for (;!rg->is_program();) {
        VarTab * vtab = rg->getVarTab();
        c.clean();
        for (VAR * v = vtab->get_first(c); v != NULL; v = vtab->get_next(c)) {
            if (!VAR_is_addr_taken(v)) { continue; }
            ASSERT0(!v->is_global());

            //Handle dedicated string md which has been taken address.
            MD const* strmd;
            if (v->is_string() &&
                (strmd = rm->genDedicateStrMD()) != NULL) {
                tmp.bunion_pure(MD_id(strmd), *m_misc_bs_mgr);
                continue;
            }

            //General md.
            ASSERT0(m_md_sys->getMDTab(v));
            MD const* x = m_md_sys->getMDTab(v)->get_effect_md();
            if (x != NULL) {
                //Record effect MD of v into the MayPointTo if exist.
                ASSERT0(x->is_unbound() || x->is_range());
                tmp.bunion(x, *m_misc_bs_mgr);
            } else {
                MD md;
                MD_base(&md) = v;
                MD_ty(&md) = MD_UNBOUND;
                MD const* entry = m_md_sys->registerMD(md);
                ASSERT0(MD_id(entry) > 0);
                tmp.bunion(entry, *m_misc_bs_mgr);
            }

            //Record each exact MD of v into the MayPointTo.
            OffsetTab * ofstab = m_md_sys->getMDTab(v)->get_ofst_tab();
            ASSERT0(ofstab);
            if (ofstab->get_elem_count() > 0) {
                iter.clean();
                for (MD const* md = ofstab->get_first(iter, NULL);
                     md != NULL; md = ofstab->get_next(iter, NULL)) {
                    ASSERT0(md->is_exact());
                    tmp.bunion(md, *m_misc_bs_mgr);
                }
            }
        }

        rg = REGION_parent(rg);
        if (rg->is_program() || rg == NULL) {
            break;
        }
        ASSERT0(rg->is_subregion() || rg->is_function() || rg->is_eh());
    }

    tmp.bunion(MD_GLOBAL_MEM, *m_misc_bs_mgr);
    tmp.bunion(MD_IMPORT_VAR, *m_misc_bs_mgr);
    m_maypts = m_mds_hash->append(tmp);
    tmp.clean(*m_misc_bs_mgr);
}


void IR_AA::computeFlowInsensitive()
{
    BBList * bbl = m_cfg->getBBList();
    xcom::C<IRBB*> * ct = NULL;
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
void IR_AA::initAliasAnalysis()
{
    ASSERT(!is_init(), ("already initialized"));
    initMayPointToSet();
    set_flow_sensitive(true);
}


//Calculate point-to set.
bool IR_AA::perform(IN OUT OptCtx & oc)
{
    ASSERT(m_maypts, ("Should invoke initAliasAnalysis() first."));
    if (m_ru->getBBList()->get_elem_count() == 0) { return true; }

    START_TIMER(t, getPassName());

    //Initialization.
    m_pt_pair_mgr.init();

    //Clean data structures used for analysis.
    clean();

    //We allocate PtPairSet at each invocation of AA,
    //because AA would not be invoked frequently.
    PtPairSet * ptset_arr = NULL;
    if (m_flow_sensitive) {
        initEntryPtset(&ptset_arr);
        m_ru->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
        List<IRBB*> * tbbl = m_cfg->getBBListInRPO();
        ASSERT0(tbbl->get_elem_count() == m_ru->getBBList()->get_elem_count());
        START_TIMER_FMT(t2, ("%s:flow sensitive analysis", getPassName()));
        bool is_succ = computeFlowSensitive(*tbbl);
        END_TIMER_FMT(t2, ("%s:flow sensitive analysis", getPassName()));
        if (!is_succ) {
            m_flow_sensitive = false;
            START_TIMER_FMT(t4, ("%s:flow insensitive analysis", getPassName()));
            initEntryPtset(NULL);
            computeFlowInsensitive();
            END_TIMER_FMT(t4, ("%s:flow insensitive analysis", getPassName()));
        }
    } else {
        START_TIMER_FMT(t3, ("%s:flow insensitive analysis", getPassName()));
        initEntryPtset(NULL);
        computeFlowInsensitive();
        END_TIMER_FMT(t3, ("%s:flow insensitive analysis", getPassName()));
    }
    OC_is_aa_valid(oc) = true;
    if (ptset_arr != NULL) {
        //PtPair information will be unavailable.
        delete [] ptset_arr;
    }

#if 0
    m_md_sys->dumpAllMD();
    dumpMD2MDSetForRegion(false);
    //dumpInOutPointToSetForBB();
    dumpIRPointToForRegion(true);
#endif

    ASSERT0(verify());

    //DU info does not depend on these data structure.
    //Since AA is not always used, we destroy the data
    //structure to release memory.
    m_pt_pair_mgr.clobber();

    END_TIMER(t, getPassName());

    return true;
}

} //namespace xoc
