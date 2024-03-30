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

//#define USE_LOOPINFO
#define PARTIAL_UPDATE

namespace xoc {

//The function check the legality of modified MDSet and Hashed MDSet.
static bool is_legal_set(MDSet const& mds, MDSet const* hashed)
{
    if (mds.is_empty()) {
        ASSERT0(hashed && !hashed->is_empty());
        return true;
    }
    ASSERT0(hashed == nullptr);
    return true;
}


//The function check the legality of modified MDSet and Hashed MDSet.
static bool is_legal_set(MDSet const& mds, AACtx const* ic)
{
    return is_legal_set(mds, ic->get_hashed());
}


static bool is_legal_ref(IR const* ir)
{
    MDSet const* mayref = const_cast<IR*>(ir)->getMayRef();
    MD const* mustref = const_cast<IR*>(ir)->getMustRef();
    ASSERTN_DUMMYUSE(mustref || (mayref && !mayref->is_empty()),
                     ("Have no idea about what ir is."));
    return true;
}


static bool hasElemPointToWorstCase(MDSet const& mds, AliasAnalysis const* aa,
                                    MD2MDSet const& mx)
{
    MDSetIter it;
    for (BSIdx i = mds.get_first(&it);
         i != BS_UNDEF; i = mds.get_next(i, &it)) {
        MDSet const* pts = aa->getPointTo((UINT)i, mx);
        if (pts != nullptr && aa->isWorstCase(pts)) {
            return true;
        }
        if (pts == nullptr) {
            //Given empty POINT-TO, we do NOT known where p pointed to,
            //and compute.
            //e.g: If p->{NULL} (p+2)->??
            return true;
        }
    }
    return false;
}


//This pass performs a flow sensitive/insensitive (SSA-based) POINTS-TO
//analysis.
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
    TMap<UINT, PtPair*> * v = nullptr;
    count += m_from_tmap.count_mem();
    for (m_from_tmap.get_first(ti, &v);
         v != nullptr; m_from_tmap.get_next(ti, &v)) {
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
    if (to_tmap == nullptr) {
        to_tmap = xmalloc_tmap();
        to_tmap->init();
        m_from_tmap.set(from, to_tmap);
    }

    PtPair * pp = to_tmap->get(to);
    if (pp == nullptr) {
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
    MDSetIter iter = nullptr;
    TMWORD ofst = (TMWORD)-1;
    TMWORD size = 0;
    BSIdx i = mds.get_first(&iter);
    MD const* md = mdsys->getMD((UINT)i);
    if (!md->is_effect() || md->is_may()) {
        return false;
    }
    ofst = MIN(ofst, md->getByteOfst());
    size = MAX(size, (md->getByteOfst() + md->getByteSize()));
    Var * base = md->get_base();
    i = mds.get_next(i, &iter);
    for (; i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        MD const* md2 = mdsys->getMD((MDIdx)i);
        if (md2->get_base() != base || !md2->is_effect() || md2->is_may()) {
            return false;
        }
        ofst = MIN(ofst, md2->getByteOfst());
        size = MAX(size, (md->getByteOfst() + md->getByteSize()));
        if (md2->is_unbound()) {
            *mustref = md2;
        }
    }

    MD tmp(*md);
    if (*mustref != nullptr) {
        MD_ty(&tmp) = MD_UNBOUND;
    } else {
        MD_ty(&tmp) = MD_RANGE;
    }
    MD_size(&tmp) = size;
    MD_ofst(&tmp) = ofst;
    *mustref = mdsys->registerMD(tmp);
    return true;
}


//
//START AliasAnalysis
//
AliasAnalysis::AliasAnalysis(Region * rg) : Pass(rg)
{
    ASSERT0(rg);
    m_cfg = rg->getCFG();
    m_vm = rg->getVarMgr();
    m_rgmgr = rg->getRegionMgr();
    m_tm = rg->getTypeMgr();
    m_md_sys = rg->getMDSystem();
    m_mds_hash = rg->getMDSetHash();
    ASSERT0(m_cfg && m_mds_hash && m_md_sys && m_tm);
    m_flow_sensitive = true;
    m_pool = smpoolCreate(128, MEM_COMM);
    m_dummy_global = nullptr;
    m_maypts = nullptr;
    m_scc = nullptr;
    m_prssamgr = nullptr;
}


AliasAnalysis::~AliasAnalysis()
{
    destroyContext();
    smpoolDelete(m_pool);
}


bool AliasAnalysis::usePRSSADU() const
{
    return m_prssamgr != nullptr && m_prssamgr->is_valid();
}


size_t AliasAnalysis::count_mem() const
{
    size_t count = 0;
    count += sizeof(m_cfg);
    count += sizeof(m_scc);
    count += sizeof(m_vm);
    count += sizeof(m_rg);
    count += m_in_pp_set.count_mem();
    count += m_out_pp_set.count_mem();
    count += smpoolGetPoolSize(m_pool);
    count += m_ppmgr.count_mem();
    if (getWorstCase() != nullptr) {
        count += getWorstCase()->count_mem();
    }
    count += countMD2MDSetMemory();
    count += m_id2heap_md_map.count_mem();
    count += m_unique_md2mds.count_mem();
    count += sizeof(BYTE);
    return count;
}


size_t AliasAnalysis::countMD2MDSetMemory() const
{
    size_t count = 0;
    BBList * bbl = m_cfg->getBBList();
    MD2MDSetIter iter;
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        MD2MDSet * mx = m_md2mds_vec.get(bb->id());
        if (mx == nullptr) { continue; }
        iter.clean();
        MDSet const* mds = nullptr;
        for (UINT mdid = mx->get_first(iter, &mds);
             mdid > MD_UNDEF; mdid = mx->get_next(iter, &mds)) {
            if (mds != nullptr) {
                count += mds->count_mem();
            }
        }
        count += mx->count_mem();
    }
    count += m_md2mds_vec.count_mem();
    return count;
}


//Destroy all context data structure.
//DU and another info do not require these info.
//If you query sideeffect md or mdset, these data structure must be recomputed.
void AliasAnalysis::destroyContext()
{
    for (VecIdx i = 0; i <= m_md2mds_vec.get_last_idx(); i++) {
        MD2MDSet * mx = m_md2mds_vec.get((UINT)i);
        if (mx == nullptr) { continue; }
        mx->destroy();
    }

    //There may be lots of md recorded in the vector.
    //Note mx itself is allocated in AA's pool, thus need to free|destroy them.
    m_md2mds_vec.destroy();
    m_md2mds_vec.init();
}


//Clean but not destory context data structures.
void AliasAnalysis::cleanContext()
{
    for (VecIdx i = 0; i <= m_md2mds_vec.get_last_idx(); i++) {
        MD2MDSet * mx = m_md2mds_vec.get((UINT)i);
        if (mx == nullptr) { continue; }
        mx->destroy();
        mx->init();
    }
    //There may be lots of md recorded in the vector.
    //Note mx itself is allocated in AA's pool, thus need to free|destroy them.
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
    cleanContext();
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
void AliasAnalysis::reviseMDSize(MOD MDSet & mds, UINT size)
{
    ASSERT0(size > 0);
    xcom::Vector<MD const*> res;
    UINT num = 0;
    bool change = false;
    MDSetIter iter = nullptr;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        MD * md = m_md_sys->getMD((MDIdx)i);
        ASSERT0(md && md->is_effect());

        //We have to register a new MD which size is equal to pointer's base.
        //e.g: int a[100], char * p = &a;
        //Even if size of element of a is 4 bytes, the size of p pointed to
        //is only 1 byte.
        if (MD_size(md) != size) {
            MD tmd(*md);
            MD_size(&tmd) = size;
            MD const* entry = m_md_sys->registerMD(tmd);
            ASSERT0(entry->id() > MD_UNDEF);
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
        for (VecIdx i = ((VecIdx)num) - 1; i != VEC_UNDEF; i--) {
            mds.bunion(res.get(i), *getSBSMgr());
        }
    }
}


//Process LDA operator, and generate MD.
//Return MD that describe memory address of LDA.
//Note this function does not handle array's LDA base.
//e.g: y = &x or y = &a[i]
//'mds' : record output memory descriptor of 'ir'
MD const* AliasAnalysis::processLda(IR * ir, MOD AACtx * ic)
{
    ASSERT0(ir->is_lda() && ir->is_ptr());
    ASSERT0(ic);
    AC_hashed_mds(ic) = nullptr;
    MD const* t = nullptr;
    Var * v = LDA_idinfo(ir);
    if (v->is_string()) {
        t = m_rg->getMDMgr()->allocStringMD(v->get_name());
        if (t->is_exact()) {
            //Adjust size of MD of LDA to be pointer size.
            MD t2(*t);
            MD_size(&t2) = ir->getTypeSize(m_tm);
            MD_ofst(&t2) += LDA_ofst(ir);
            t = m_md_sys->registerMD(t2);
            ASSERT0(t->id() > MD_UNDEF);
        }
    } else {
        t = m_rg->getMDMgr()->genMDForVar(v, ir->getType(), LDA_ofst(ir));
    }

    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        AC_is_mds_mod(ic) = true;
    } else {
        AC_is_mds_mod(ic) = false;
    }

    //Inform the caller that there is MD has been taken address.
    AC_is_addr_taken(ic) = true;

    if (!ic->is_mds_mod()) { return t; }

    if (!ir->getParent()->is_array()) { return t; }

    //Current ir is base of ARRAY/ST_ARRAY.
    //Try to reshape MD.
    //Reshape MD size to array's element type size
    //instead of LDA's data-type size.

    //If LDA is the base of array operation, whereas LDA_ofst may not be 0.
    //e.g: struct S { int a; int b[..]; } s;
    //     ... = s.b[i]
    //The IR could be: ARRAY:4 (LDA s:4, dim[..]), (ld i)
    MD md(*t);
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
        ASSERT0(entry->id() > MD_UNDEF);
        ASSERT0(t->is_effect() && entry->is_effect());
        return entry;
    }
    return t;
}


//Convert type-size.
//e.g:int a; char b;
//    a = (int)b
//'mds' : record memory descriptor of 'ir'.
void AliasAnalysis::processCvt(IR const* ir, MOD MDSet & mds,
                               OUT AACtx * ic, OUT MD2MDSet * mx)
{
    ASSERT0(ir->is_cvt());
    inferExpression(CVT_exp(ir), mds, ic, mx);
    if (!ic->is_mds_mod()) { return; }

    MDSetIter iter;
    UINT size = ir->getTypeSize(m_tm); //cvt's tgt byte size.
    BSIdx next;
    for (BSIdx i = mds.get_first(&iter); i != BS_UNDEF; i = next) {
        next = mds.get_next(i, &iter);
        MD const* l = m_md_sys->getMD((MDIdx)i);
        ASSERT0(l);

        if (l->is_exact() && MD_size(l) != size) {
            MD md(*l);

            //Convert type-size to 'ir' claimed.
            MD_size(&md) = size;
            MD const* entry = m_md_sys->registerMD(md);
            ASSERT0(entry->id() > MD_UNDEF);
            ASSERT0(l->is_effect() && entry->is_effect());

            mds.diff((UINT)i, *getSBSMgr());
            mds.bunion_pure(entry->id(), *getSBSMgr());
        }
    }
}


//Infer the unbounded set.
void AliasAnalysis::inferArrayInfinite(INT ofst, bool is_ofst_pred,
                                       UINT md_size, MDSet const& in,
                                       OUT MDSet & out)
{
    MDSetIter iter;
    for (BSIdx i = in.get_first(&iter);
         i != BS_UNDEF; i = in.get_next(i, &iter)) {
        MD const* org = m_md_sys->getMD((MDIdx)i);
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
        ASSERT0(entry->id() > MD_UNDEF);
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
    MD const* typed_md = queryTBAA(pointer);

    //TBAA tell us where pointer pointed to.
    if (typed_md != nullptr) {
        //Try to resolve POINT-TO MD through TBAA.
        MDSet tmp;
        tmp.bunion(typed_md, *getSBSMgr());
        MDSet const* p = m_mds_hash->append(tmp);
        tmp.clean(*getSBSMgr());
        return p;
    }
    return point_to_set;
}


static MD const* regardAsSingleMD(MDSet const* mds, Region * rg)
{
    MDSetIter iter = nullptr;
    MD const* x = nullptr;
    if (mds->get_elem_count() == 1 &&
        !(x = rg->getMDSystem()->getMD((UINT)mds->
          get_first(&iter)))->is_may()) {
        return x;
    }
    return nullptr;
}


//Assign the MD reference to ir.
//If the MD in mds is MustExact, regarding ir as referencing single and exact
//MD, or else regard ir as referencing a set of MD.
//hashed_mds: MDSet that already be hashed.
void AliasAnalysis::setIRRefHashed(IR * ir, MDSet const* hashed_mds)
{
    MD const* x = regardAsSingleMD(hashed_mds, m_rg);
    if (x != nullptr) {
        ir->setMustRef(x, m_rg);
        ir->cleanMayRef();
        return;
    }
    ir->cleanMustRef();
    ir->setMayRef(hashed_mds, m_rg);
}


//The function unites POINT-TO set into 'output' for each element in 'mds'.
void AliasAnalysis::collectPointToForElem(MDSet const& mds, MD2MDSet const* mx,
                                          OUT MDSet * united,
                                          OUT MDSet const** hashed)
{
    ASSERT0(united && hashed);
    MDSetIter iter;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        MDSet const* pts = getPointTo((MDIdx)i, *mx);
        if (pts != nullptr && isWorstCase(pts)) {
            //We do NOT known where p[...] pointed to, use the
            //conservative solution.
            *hashed = getWorstCase();
            united->clean(*getSBSMgr());
            return;
        }
    }

    //Collect the POINT-TO of each elements in MDSet.
    *hashed = nullptr;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        MDSet const* pts = getPointTo((MDIdx)i, *mx);
        if (pts != nullptr) {
            united->bunion(*pts, *getSBSMgr());
        }
    }
    if (united->is_empty()) {
        //POINT-TO is empty. We do NOT known where p[...] pointed to, use the
        //conservative solution.
        *hashed = getWorstCase();
    }
}


//Assign the MD reference to ir.
//If the MD in mds is MustExact, regarding ir as referencing single and exact
//MD, or else regard ir as referencing a set of MD.
//mds: MDSet that may NOT be hashed.
void AliasAnalysis::setIRRef(IR * ir, MDSet const* mds)
{
    if (!isWorstCase(mds)) {
        mds = m_mds_hash->append(*mds);
    }
    setIRRefHashed(ir, mds);
}


//The function compute may memory address or point-to set for array operation.
//Note the function handle the worst case when infer Point-To for array base
//expression.
//'ir': array|starray operator.
//'is_ofst_predicable': true if array element offset is constant.
//This function will set the Ref MD and Ref MD set of array operation.
void AliasAnalysis::inferArrayExpBaseHashedMDSet(IR * ir,
                                                 MDSet const* hashed_mds,
                                                 OUT MDSet & mds,
                                                 OUT AACtx * ic)
{
    //We could not determine which MD the array base refer to,
    //thus we could not infer which MD the array accessed. The situation
    //drops into the most conservative case.
    //Record the worst case as result.
    mds.clean(*getSBSMgr());
    AC_hashed_mds(ic) = hashed_mds;

    //Set ir's reference.
    AC_is_mds_mod(ic) = true;
    setIRRefHashed(ir, hashed_mds);
}


MD const* AliasAnalysis::queryTBAA(IR const* ir)
{
    MD const* typed_md;
    if (ir->getAI() != nullptr &&
        (typed_md = computePointToViaType(ir)) != nullptr) {
        return typed_md;
    }
    return nullptr;
}


//The function compute may memory address or point-to set for array operation.
//'ir': array|starray operator.
//'array_base': base of array, must be pointer type.
//'is_ofst_predicable': true if array element offset is constant.
//This function will set the Ref MD and Ref MD set of array operation.
void AliasAnalysis::inferArrayExpBase(IR * ir, IR * array_base,
                                      bool is_ofst_predicable, UINT ofst,
                                      OUT MDSet & mds, MOD AACtx * ic,
                                      MOD MD2MDSet * mx)
{
    ASSERT0(ir->isArrayOp() && array_base->isPtr());
    MDSet tmp;
    AACtx tic;
    tic.copyTopDownFlag(*ic);

    //Compute point-to set at this function scope rather than demand
    //inferExpression for 'array_base', we can do more evaluation.
    AC_comp_pts(&tic) = false;

    inferExpression(array_base, tmp, &tic, mx);

    //Acquire TBAA to determine MD reference.
    MD const* typed_md = queryTBAA(array_base);
    if (typed_md != nullptr) {
        //Record result to give it back to caller.
        mds.clean(*getSBSMgr());
        mds.bunion(typed_md, *getSBSMgr());
        AC_hashed_mds(ic) = nullptr;

        //Set ir's reference.
        AC_is_mds_mod(ic) = true;
        ir->setMustRef(typed_md, m_rg);
        ir->cleanMayRef();
        return;
    }

    if (tmp.is_empty()) {
        if (tic.get_hashed() != nullptr) {
            inferArrayExpBaseHashedMDSet(ir, tic.get_hashed(), mds, ic);
        } else {
            inferArrayExpBaseHashedMDSet(ir, getWorstCase(), mds, ic);
        }
        tmp.clean(*getSBSMgr());
        return;
    }

    //Intent to look for original array base that the base-pointer
    //pointed to.
    //e.g: (ld(p))[i], looking for where p pointed to.
    //Each MD in 'tmp' should be pointer.
    mds.clean(*getSBSMgr());
    UINT mdsz = ir->getTypeSize(m_tm);
    MDSetIter iter;
    for (BSIdx i = tmp.get_first(&iter);
         i != BS_UNDEF; i = tmp.get_next(i, &iter)) {
        ASSERT0(m_md_sys->getMD((MDIdx)i));

        //Get to know where the base pointed to.
        MDSet const* pts = getPointTo((UINT)i, *mx);
        MD const* typed_md = nullptr;
        if (pts != nullptr && !pts->is_empty()) {
            if (pts->is_contain_global() &&
                (typed_md = queryTBAA(array_base)) != nullptr) {
                setPointToUniqueMD((UINT)i, *mx, typed_md);
                mds.clean(*getSBSMgr());
                mds.bunion(typed_md, *getSBSMgr());
            } else {
                inferArrayInfinite((INT)ofst, is_ofst_predicable,
                                   mdsz, *pts, mds);
            }
            continue;
        }

        if ((typed_md = queryTBAA(array_base)) != nullptr) {
            mds.bunion(typed_md, *getSBSMgr());
            continue;
        }

        inferArrayInfinite((INT)ofst, false, 0, *getWorstCase(), mds);
        break;
    }

    ASSERT0(!mds.is_empty());
    //Set ir's ref-md and ref-mdset.
    AC_is_mds_mod(ic) = true;
    setIRRef(ir, &mds);
    tmp.clean(*getSBSMgr());
}


//This function infer array element memory address according to
//the LDA base of array operation.
//This function will set the Ref MD and Ref MD set of array operation.
//is_ofst_pred: true if 'ofst' describe the computed byte offset related to
//              'array_base'.
//ofst: byte offset, only valid if 'is_ofst_pred' is true.
MD const* AliasAnalysis::inferArrayLdabase(MOD IR * ir, IR * array_base,
                                           bool is_ofst_pred, UINT ofst,
                                           MOD AACtx * ic)
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
    //set MD12 as mustref of 'ir'.
    //However if at the second iteration of AA, return mustref
    //directly here, caller will still reshape MD12 to be
    //MD13={ofst=16,size=4}.
    //This lead to incorrect MD for the array operation.
    //Thus we should not stop recompute mustref even if AC_is_mds_mod
    //unchanged.
    //if (!AC_is_mds_mod(&tic)) {
    //    ASSERTN(ir->getMayRef() == nullptr, ("have no mayref"));
    //    MD const* x = ir->getMustRef();
    //    ASSERT0(x && x->is_effect());
    //    return x;
    //}

    //Lda's MD changed, thus array base MD have to be recomputed.
    ir->cleanMayRef();

    //Compute the MD size and offset if 'ofst' is constant.
    if (!ldamd->is_exact()) {
        ir->setMustRef(ldamd, m_rg);
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
        ir->setMustRef(ldamd, m_rg);
        return ldamd;
    }
    //Reshape MD of LDA.
    MD const* entry = m_md_sys->registerMD(tmd);
    ASSERT0(entry->is_effect());
    ASSERT0(entry->id() > MD_UNDEF);
    ir->setMustRef(entry, m_rg);
    return entry;
}


void AliasAnalysis::computeResultSet(MDSet const& input_mds,
                                     MDSet const* input_hashed,
                                     OUT MDSet & output_mds,
                                     OUT AACtx * output_ic)
{
    ASSERT0_DUMMYUSE(is_legal_set(input_mds, input_hashed));
    if (input_hashed != nullptr) {
        AC_hashed_mds(output_ic) = input_hashed;
        output_mds.clean(*getSBSMgr());
    } else {
        AC_hashed_mds(output_ic) = nullptr;
        output_mds.copy(input_mds, *getSBSMgr());
    }
}


void AliasAnalysis::processArrayHashed(MOD IR * ir, MDSet const* hashed,
                                       MD2MDSet const* mx, OUT MDSet & mds,
                                       MOD AACtx * ic)
{
    ASSERT0(hashed && !hashed->is_empty());

    //Record the return value for caller.
    AC_hashed_mds(ic) = hashed;
    mds.clean(*getSBSMgr());

    if (isWorstCase(hashed)) {
        //Use the worst case as result because the united POINT-TO set of
        //the worst case is still worst case.
        setIRRefHashed(ir, hashed);
        return;
    }

    //If array offset is not zero, the result data type may not
    //being the element type. Try to infer the real memory reference of ir.
    MDSet tmp;
    if (!ir->is_any() && //can NOT infer reference for ANY data-type.
        tryReshapeMDSet(ir, hashed, &tmp)) {
        setIRRef(ir, &tmp);

        //Update 'hashed' content when it has been changed and recorded in
        //'tmp'.
        hashed = ir->getMayRef();
        ASSERT0(hashed);
    } else {
        setIRRefHashed(ir, hashed);
    }
    tmp.clean(*getSBSMgr());

    if (!ic->is_comp_pts()) {
        //Caller does not need POINT-TO.
        return;
    }

    //Compute the potential POINT-TO of ir.
    MDSet const* hashed_united = nullptr;
    collectPointToForElem(*hashed, mx, &tmp, &hashed_united);
    computeResultSet(tmp, hashed_united, mds, ic);
    tmp.clean(*getSBSMgr());
}


//Compute the memory address and ONLY record the top level
//ARRAY node's memory address.
//mds: record memory descriptor of 'ir'.
//This function will set the Ref MD and Ref MD set of array operation.
void AliasAnalysis::processArray(MOD IR * ir, MOD MDSet & mds, MOD AACtx * ic,
                                 MOD MD2MDSet * mx)
{
    ASSERT0(ir->isArrayOp());

    //Scan subscript expression and infer the offset of array element.
    for (IR * s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
        AACtx tic;
        tic.copyTopDownFlag(*ic);
        AC_comp_pts(&tic) = false;
        inferExpression(s, mds, &tic, mx);
    }
    IR * array_base = ARR_base(ir);

    //Next, scaning array base, it may be LDA
    //operation or computational expression.
    TMWORD ofst_val = 0;
    bool is_ofst_predicable = ir->calcArrayOffset(&ofst_val, m_tm);
    AACtx tic;
    tic.copyTopDownFlag(*ic);
    AC_comp_pts(&tic) = false;

    if (array_base->is_lda()) {
        //Array base is LDA operation.
        MD const* md = inferArrayLdabase(ir, array_base, is_ofst_predicable,
                                         (UINT)ofst_val, &tic);
        ASSERT0(md);
        mds.clean(*getSBSMgr());
        mds.bunion_pure(md->id(), *getSBSMgr());
    } else {
        //Array base is a computational expression.
        inferArrayExpBase(ir, array_base, is_ofst_predicable,
                          (UINT)ofst_val, mds, &tic, mx);
    }
    ic->copyBottomUpFlag(tic);

    ASSERT0_DUMMYUSE(is_legal_set(mds, &tic));

    if (mds.is_empty()) {
        ASSERT0(tic.get_hashed());
        processArrayHashed(ir, tic.get_hashed(), mx, mds, ic);
        return;
    }

    ASSERT0(tic.get_hashed() == nullptr);
    //The memory reference is recorded in 'mds'.

    //If array offset is not zero, the result data type may not
    //being the element type. Try to infer the real memory reference of ir.
    MDSet tmp;
    if (!ir->is_any() && //can NOT infer reference for ANY data-type.
        tryReshapeMDSet(ir, &mds, &tmp)) {
        //Update mds's content when it has been changed and recorded in 'tmp'.
        mds.copy(tmp, *getSBSMgr());
    }
    setIRRef(ir, &mds);
    tmp.clean(*getSBSMgr());

    if (!ic->is_comp_pts()) {
        //Caller does not need POINT-TO.
        return;
    }

    //Compute the POINT-TO of array element.
    MDSet const* hashed = nullptr;
    collectPointToForElem(mds, mx, &tmp, &hashed);
    computeResultSet(tmp, hashed, mds, ic);
    tmp.clean(*getSBSMgr());
}


bool AliasAnalysis::evaluateViaSSAInfo(IR const* ir)
{
    if (!ir->is_pr() || !usePRSSADU()) { return false; }
    SSAInfo const* ssainfo = PR_ssainfo(ir);
    ASSERTN(ssainfo, ("AA told us SSA is avaiable"));
    IR * defstmt = SSA_def(ssainfo);
    if (defstmt == nullptr || !defstmt->is_stpr() ||
        ir->getStmt() == defstmt) {
        return false;
    }

    IR const* rhs = STPR_rhs(defstmt);
    switch (rhs->getCode()) {
    case IR_LDA: return true;
    SWITCH_CASE_READ_PR: return evaluateFromLda(rhs);
    case IR_CVT: return evaluateFromLda(CVT_exp(rhs));
    default:;
    }

    IR const* r = rhs;
    for (;;) {
        switch (r->getCode()) {
        case IR_ADD: {
            //Check the opnd0 if it is : op0 + imm(0)
            IR const* op1 = BIN_opnd1(r);
            if (op1->is_const() && op1->is_int()) {
                r = BIN_opnd0(r);
                break;
            }
            return false;
        }
        case IR_LDA: return true;
        SWITCH_CASE_READ_PR: return evaluateFromLda(r);
        default: goto END; //To elim -Wunreachable-code warning.
        }
    }
END:
    return false;
}


//Return true if POINT-TO is evaluated from LDA.
bool AliasAnalysis::evaluateFromLda(IR const* ir)
{
    //Attempt to infer more presicion point-to if ssa info is avaiable.
    if (ir->is_cvt()) { return evaluateFromLda(CVT_exp(ir)); }
    return evaluateViaSSAInfo(ir);
}


//Return true if Unsigned integer is overflow.
template <class T>
inline bool doAddAndCheckOverflow(T a, T b, OUT T & res)
{
    res = a + b;
    return res < MAX(a, b);
}


static void computeMDOffsetAdd(bool is_signed, HOST_INT const_ofst,
                               OUT MD * md)
{
    ASSERT0(md->is_exact());
    ASSERT0(sizeof(HOST_INT) >= sizeof(TMWORD));
    if (is_signed) {
        HOST_INT newofst = (HOST_INT)MD_ofst(md) + const_ofst;
        if (newofst < 0) {
            MD_ofst(md) = 0;
            MD_size(md) = 0;
            MD_ty(md) = MD_UNBOUND;
            return;
        }
        MD_ofst(md) = (TMWORD)newofst;
        return;
    }
    HOST_UINT newofst;
    if (doAddAndCheckOverflow((HOST_UINT)MD_ofst(md), (HOST_UINT)const_ofst,
                              newofst)) {
        MD_ofst(md) = 0;
        MD_size(md) = 0;
        MD_ty(md) = MD_UNBOUND;
        return;
    }
    MD_ofst(md) = (TMWORD)newofst;
}


static void computeMDOffsetSub(HOST_INT const_ofst, OUT MD * md)
{
    ASSERT0(sizeof(HOST_INT) >= sizeof(TMWORD));
    HOST_INT newofst = (HOST_INT)MD_ofst(md) - const_ofst;
    if (newofst < 0) {
        MD_ty(md) = MD_UNBOUND;
        MD_size(md) = 0;
        MD_ofst(md) = 0;
    } else {
        MD_ofst(md) = (TMWORD)newofst;
    }
}


static void computeMDOffset(IR const* op, IR const* opnd1,
                            HOST_INT const_ofst, OUT MD * md)
{
    switch (op->getCode()) {
    case IR_ADD:
        computeMDOffsetAdd(opnd1->is_sint(), const_ofst, md);
        return;
    case IR_SUB:
        computeMDOffsetSub(const_ofst, md);
        return;
    default: ASSERTN(0, ("TODO"));
    }
}


//Compute the constant offset for pointer arithmetic.
//Return true if the offset can be confirmed via
//simply calculation, and has been determined by this function.
//Return false if caller has to keep reevaluating the offset of 'opnd0_mds'.
//Return true if this function guarantee applying 'opnd1' on opnd0_mds
//successful, then caller can use the returned result directly.
//If 'opnd0_mds' changed, then set 'changed' to true, and record the new MDSet
//to 'mds'.
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
                                          IR const* opnd1, MOD MDSet & mds,
                                          bool * changed)
{
    ASSERT0(changed);
    mds.clean(*getSBSMgr());
    *changed = false;
    //Compute the offset for pointer arithmetic.
    if (opnd1->is_const() && opnd1->is_int() && CONST_int_val(opnd1) == 0) {
        return true;
    }

    HOST_INT const_offset = 0;
    if (opnd1->is_const() && opnd1->is_int()) {
        const_offset = CONST_int_val(opnd1);
    } else if (opnd1->is_pr()) {
        if (!m_rg->evaluateConstInteger(opnd1, (ULONGLONG*)&const_offset)) {
            return false;
        }
    } else {
        return false;
    }
    if (const_offset == 0) {
        //Keep opnd0_mds unchanged.
        return true;
    }

    //In the case: LDA(x) + ofst, we can determine
    //the value of LDA(x) is constant.
    //Keep offset validation unchanged.
    MDSetIter iter;
    if (ir->is_add()) {
        for (BSIdx i = opnd0_mds.get_first(&iter);
            i != BS_UNDEF; i = opnd0_mds.get_next(i, &iter)) {
            MD * imd = m_md_sys->getMD((MDIdx)i);
            if (!imd->is_exact()) {
                mds.bunion(imd, *getSBSMgr());
                continue;
            }
            MD const* entry = nullptr;
            MD x(*imd);

            //In the case: LDA(x) + ofst, we can determine
            //the value of LDA(x) is constant.
            ; //Keep offset validation unchanged.
            //MD_ofst(&x) += (TMWORD)const_offset;
            computeMDOffset(ir, opnd1, const_offset, &x);

            entry = m_md_sys->registerMD(x);
            ASSERT0(entry->id() > MD_UNDEF);
            mds.bunion(entry, *getSBSMgr());
            *changed = true;
        }
        if (!*changed) {
            //Clean mds if opnd0_mds is unchanged.
            mds.clean(*getSBSMgr());
        }
        return true;
    }

    ASSERT0(ir->is_sub());
    for (BSIdx i = opnd0_mds.get_first(&iter);
         i != BS_UNDEF; i = opnd0_mds.get_next(i, &iter)) {
        MD * imd = m_md_sys->getMD((MDIdx)i);
        if (!imd->is_exact()) {
            mds.bunion(imd, *getSBSMgr());
            continue;
        }
        MD const* entry = nullptr;
        MD x(*imd);

        //case: &x - ofst.
        //Keep offset validatiohanged.
        //HOST_INT offset = (HOST_INT)MD_ofst(&x) - (HOST_INT)const_offset;
        //if (offset < 0) {
        //    MD_ty(&x) = MD_UNBOUND;
        //    MD_size(&x) = 0;
        //    MD_ofst(&x) = 0;
        //} else {
        //    MD_ofst(&x) = offset;
        //}
        computeMDOffset(ir, opnd1, const_offset, &x);

        entry = m_md_sys->registerMD(x);
        ASSERT0(entry->id() > MD_UNDEF);
        mds.bunion(entry, *getSBSMgr());
        *changed = true;
    }
    if (!*changed) {
        //Clean mds if opnd0_mds is unchanged.
        mds.clean(*getSBSMgr());
    }
    return true;
}


//Return true if new POINT_TO info generated.
//opnd0_mds: already have recorded the POINT-TO set of opnd0.
//opnd0_ic: already have recorded the POINT-TO set of opnd0.
bool AliasAnalysis::tryToEvaluateConstOffset(IR const* ir, OUT MDSet & mds,
                                             MDSet const& opnd0_mds,
                                             MOD AACtx * opnd0_ic)
{
    ASSERT0(opnd0_ic->is_comp_pts());
    ASSERT0(ir->is_add() || ir->is_sub());
    IR * opnd0 = BIN_opnd0(ir);
    if (!opnd0->is_lda() && !evaluateFromLda(opnd0)) {
        return false;
    }

    MDSet const* in = nullptr;
    if (opnd0_ic->get_hashed() != nullptr) {
        ASSERT0(!opnd0_ic->get_hashed()->is_empty());
        ASSERT0(opnd0_mds.is_empty());
        in = opnd0_ic->get_hashed();
    } else {
        ASSERT0(!opnd0_mds.is_empty());
        in = &opnd0_mds;
    }
    bool changed = false;
    IR * opnd1 = BIN_opnd1(ir);
    if (!tryComputeConstOffset(ir, *in, opnd1, mds, &changed)) {
        return false;
    }

    if (changed) {
        //POINT_TO set changed, thus disregard the alternative POINT_TO.
        //mds recorded the expected POINT_TO.
        AC_hashed_mds(opnd0_ic) = nullptr;
        ASSERT0(!mds.is_empty());
        return true;
    }

    //'in' recorded the expected POINT_TO.
    ASSERT0(!in->is_empty());
    //CASE:there are two results, one is that mds is empty, the POINT-TO
    //info is recorded in 'in' and unchanged; anthoner one is that mds is
    //not empty, the POINT-TO info is recorded in 'mds' and changed.
    if (mds.is_empty()) {
        return false;
    }
    //New POINT-TO info has generated by this function.
    //Note that tryComputeConstOffset already have unify elements into 'mds'.
    //Thus there is no need to copy 'in' again.
    ASSERT0(mds.is_equal(*in));
    //mds.copy(*in, *getSBSMgr());
    return true;
}


void AliasAnalysis::inferPointerArithByUnHashedPTS(IR const* ir,
                                                   OUT MDSet & mds,
                                                   MDSet const& opnd0_mds)
{
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

    bool changed = false;
    IR * opnd1 = BIN_opnd1(ir);
    if (!tryComputeConstOffset(ir, opnd0_mds, opnd1, mds, &changed)) {
        convertExact2Unbound(opnd0_mds, &mds);
        ASSERT0(!mds.is_empty());
        return;
    }
    if (changed) {
        //POINT_TO set changed, thus disregard the alternative POINT_TO.
        //mds record the expected info.
        ASSERT0(!mds.is_empty()); //mds record the expected info.
        return;
    }
    ASSERT0(!opnd0_mds.is_empty());

    //Here, opnd0_mds's content is unchanged.
    ASSERT0(mds.is_empty());

    //Note that tryComputeConstOffset already have unify elements into
    //'mds' if opnd0_mds need to change. Otherwise mds should be empty.
    //Thus there is no need to copy 'opnd0_mds' to 'mds' here.
    //mds.copy(opnd0_mds, *getSBSMgr());
}


void AliasAnalysis::inferPointerArithByHashedPTS(IR const* ir, OUT MDSet & mds,
                                                 MOD AACtx * opnd0_ic)
{
    ASSERT0(opnd0_ic->get_hashed());
    if (isWorstCase(opnd0_ic->get_hashed())) {
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
        convertExact2Unbound(*opnd0_ic->get_hashed(), &mds);
        AC_hashed_mds(opnd0_ic) = nullptr;
        ASSERT0(!mds.is_empty()); //mds record the expected info.
        return;
    }
    bool changed = false;
    IR * opnd1 = BIN_opnd1(ir);
    if (!tryComputeConstOffset(ir, *opnd0_ic->get_hashed(),
                               opnd1, mds, &changed)) {
        convertExact2Unbound(*opnd0_ic->get_hashed(), &mds);
        AC_hashed_mds(opnd0_ic) = nullptr;
        ASSERT0(!mds.is_empty()); //mds record the expected info.
        return;
    }
    if (changed) {
        //POINT_TO set changed, thus disregard the alternative POINT_TO.
        //mds record the expected info.
        AC_hashed_mds(opnd0_ic) = nullptr;
        ASSERT0(!mds.is_empty());
        return;
    }

    //Make sure AC_hashed_mds is unchanged.
    ASSERT0(opnd0_ic->get_hashed() &&
            !opnd0_ic->get_hashed()->is_empty());

    //tryComputeConstOffset has unify elements into
    //'mds' if opnd0_tic hashed PTS changed.
    ASSERT0(mds.is_empty());
}


//Perform pointer arith to compute where ir might point to.
//If we compute the point-to set of p+1, that always equivilate to
//compute the point-to of p, and each element in the set will be
//registered to be unbound if p+1 is inside loop. Because if p+1 is placed
//inside a loop, we could not determine the exact MD where p pointed to.
//mds: record memory descriptor of 'ir' that computed by this function.
//opnd0_mds: already have recorded the POINT-TO set of opnd0.
//opnd0_ic: already have recorded the POINT-TO set of opnd0.
void AliasAnalysis::inferPointerArith(IR const* ir, OUT MDSet & mds,
                                      MDSet const& opnd0_mds,
                                      MOD AACtx * opnd0_ic,
                                      MOD MD2MDSet * mx)
{
    ASSERT0(opnd0_ic->is_comp_pts());
    ASSERT0(ir->is_add() || ir->is_sub());
    mds.clean(*getSBSMgr());
    if (tryToEvaluateConstOffset(ir, mds, opnd0_mds, opnd0_ic)) {
        return;
    }
    //Bottom-up flag of opnd0, say mds, is useless to its parent.

    //Generate MD expression for opnd1.
    AACtx opnd1_tic(*opnd0_ic);
    opnd1_tic.cleanBottomUpFlag();
    AC_comp_pts(&opnd1_tic) = false; //PointToSet of addon is useless.
    IR * opnd1 = BIN_opnd1(ir);
    inferExpression(opnd1, mds, &opnd1_tic, mx);

    //Bottom-up flag of opnd1, say mds, is useless to its parent.
    mds.clean(*getSBSMgr());

    if (opnd1_tic.is_taken_addr() && opnd0_ic->is_taken_addr()) {
        //In the situation such as: &a - &b.
        ASSERTN(ir->is_sub(), ("only support pointer sub pointer"));
        AC_is_addr_taken(opnd0_ic) = false;
        return;
    }
    if (opnd0_ic->get_hashed() != nullptr) {
        //opnd0_ic's hashed PTS may be outdate, the new generated MDSet will
        //be recorded in 'mds'.
        inferPointerArithByHashedPTS(ir, mds, opnd0_ic);
        ASSERT0_DUMMYUSE(is_legal_set(mds, opnd0_ic));
        return;
    }
    ASSERT0(!opnd0_mds.is_empty());
    //The new generated MDSet will be recorded in 'mds' if opnd0_mds changed.
    inferPointerArithByUnHashedPTS(ir, mds, opnd0_mds);
}


void AliasAnalysis::convertExact2Unbound(MDSet const& src, MDSet * tgt)
{
    MDSetIter iter;
    for (BSIdx i = src.get_first(&iter);
        i != BS_UNDEF; i = src.get_next(i, &iter)) {
        MD * imd = m_md_sys->getMD((MDIdx)i);
        if (!imd->is_exact()) {
            tgt->bunion(imd, *getSBSMgr());
            continue;
        }

        MD x(*imd);
        MD_ty(&x) = MD_UNBOUND;
        MD_ofst(&x) = 0;
        MD const* entry = m_md_sys->registerMD(x);
        ASSERT0(entry->id() > MD_UNDEF);
        tgt->bunion_pure(entry->id(), *getSBSMgr());
    }
}


bool AliasAnalysis::isInLoop(IR const* ir)
{
    ASSERT0(ir && ir->is_stmt());
    IRBB * bb = ir->getBB();
    ASSERT0(bb);
    ASSERT0(m_scc);
    if (m_scc->isInSCC(bb)) { return true; }

    #ifdef USE_LOOPINFO
    //LoopInfo should be subset of SCC.
    LI<IRBB> const* li = m_cfg->getLoopInfo();

    //Only have to check the outermost loop body.
    for (; li != nullptr; li = LI_next(li)) {
        if (li->isInsideLoop(bb->id())) {
            return true;
        }
    }
    #endif

    return false;
}


void AliasAnalysis::processAddSub(IR * ir, MOD MDSet & mds,
                                  MOD AACtx * ic, MOD MD2MDSet * mx)
{
    ASSERT0(ir->is_add() || ir->is_sub());
    ASSERT0(!ic->is_comp_pts());
    IR * opnd0 = BIN_opnd0(ir);
    IR * opnd1 = BIN_opnd1(ir);
    ir->cleanRef();

    //Parent IR may demand the function to compute point-to set of 'ir'.
    AACtx tic(*ic);
    AC_comp_pts(&tic) = false; //Just compute the mdset of opnd0.
    tic.cleanBottomUpFlag();
    inferExpression(opnd0, mds, &tic, mx);
    mds.clean(*getSBSMgr()); //Do not remove this code.

    //Scan and generate MD of opnd1.
    AACtx tic2(*ic);
    AC_comp_pts(&tic2) = false; //Just compute the mdset of opnd1.
    tic2.cleanBottomUpFlag();
    inferExpression(opnd1, mds, &tic2, mx);
    mds.clean(*getSBSMgr()); //Do not remove this code.

    //Have to union the ctx info bottom up to caller.
    //The output MDSet result has already recorded in 'tic' and
    //mds's content is useless.
    ic->copyBottomUpFlag(tic);
}


//Compute the MDSet or POINT-TO set of 'ir'.
//ir may be pointer arithmetic, even if opnd0 is not pointer type.
//mds: record memory-descriptor set or point-to set of 'ir'
//     if AC_comp_pts(ic) is true.
void AliasAnalysis::processPointerArith(IR * ir, MOD MDSet & mds,
                                        MOD AACtx * ic, MOD MD2MDSet * mx)
{
    ASSERT0(ir->is_add() || ir->is_sub());
    ASSERT0(ic->is_comp_pts());
    IR * opnd0 = BIN_opnd0(ir);
    IR * opnd1 = BIN_opnd1(ir);
    ir->cleanRef();

    //Parent IR may demand the function to compute point-to set of 'ir'.
    MDSet opnd0_mds;
    AACtx opnd0_tic(*ic);
    opnd0_tic.cleanBottomUpFlag();
    inferExpression(opnd0, opnd0_mds, &opnd0_tic, mx);
    ASSERT0_DUMMYUSE(is_legal_set(opnd0_mds, &opnd0_tic));
    if (ir->is_add()) {
        //If opnd1 is pointer. e.g: x+&q, it means the arih regard the
        //the opnd1 as an addend to pointer opnd0.
        //ASSERT0(!opnd1->is_ptr());
    }

    //For given expression: a + b, we can not tell which memory it pointed to.
    if (opnd0->is_ptr()) {
        //This is pointer arithmetic.
        //If p is a pointer, the followed expr is analyzable:
        //    (p +/- n), where n is constant.
        //    (p +/- n + ...), where n is constant.
        //p may be literal, e.g: ((int*)0x1000) + 1.
        if (!opnd1->is_ptr()) {
            //pointer +/- n still be pointer.
            //ir may be ANY.
            //ASSERT0(ir->is_ptr());
        }
    }
    //opnd0_mds already have recorded the POINT-TO set of opnd0.
    //Now, infering the final POINT-TO set according to op0 +/- op1.
    mds.clean(*getSBSMgr()); //Do not remove this code.
    inferPointerArith(ir, mds, opnd0_mds, &opnd0_tic, mx);

    //Have to union the ctx info bottom up to caller.
    if (opnd0_tic.get_hashed() == nullptr) {
        if (mds.is_empty()) {
            //The POINT-TO set is recorded in 'opnd0_mds'. If opnd0_mds
            //changed, the final PTS is recorded in 'mds', otherwise mds is
            //emtpy, we should copy opnd0_mds to mds as output result.
            ASSERT0(!opnd0_mds.is_empty());
            mds.copy(opnd0_mds, *getSBSMgr());
        } else {
            //There are two possible situations, one is that opnd0_tic may be
            //clean by inferPointerArith(), and the new result recorded in mds.
            //another is that opnd0_mds changed, the new result recorded
            //in mds.
        }
    } else {
        ASSERT0(opnd0_mds.is_empty());
        //If opnd0_tic's hashed PTS changed, the new result has been recorded
        //in mds, whereas mds is empty if opnt0_tic unchanged.
        if (!mds.is_empty()) {
            //If opnd0_tic's hashed PTS changed, the new result has been
            //recorded in mds, whereas opnd0_tic's PTS is outdated and should
            //be clean.
            AC_hashed_mds(&opnd0_tic) = nullptr;
        } else {
            //mds is empty if opnt0_tic unchanged.
        }
    }
    //Union the hashed PTS to ic if exist.
    ic->copyBottomUpFlag(opnd0_tic);
    opnd0_mds.clean(*getSBSMgr()); //free tmp MDSet.
}


//Assign unique MD to pr.
//'mds' : record memory descriptor of 'ir' if AC_comp_pts() is false, or
//        record the POINT-TO set of 'ir' if AC_comp_pts() is true.
//        Note if AC_comp_pts() is true, the returned POINT-TO set may be
//        recorded in 'mds' or AC_hashed_mds.
//Return MD of ir.
MD const* AliasAnalysis::processPR(IR * ir, MOD MDSet * mds, MOD AACtx * ic,
                                   MOD MD2MDSet * mx)
{
    ASSERT0(ir->is_pr());
    ASSERT0(mds && ic);
    MD const* tmp = nullptr;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        AC_is_mds_mod(ic) = true;
    } else {
        AC_is_mds_mod(ic) = false;
    }
    tmp = ir->getMustRef();
    ASSERT0(tmp);
    mds->clean(*getSBSMgr());
    AC_hashed_mds(ic) = nullptr;
    if (!ic->is_comp_pts()) {
        //The MD should be recorred in 'mds'.
        //Parent expression may retrive 'mds'.
        mds->bunion(tmp, *getSBSMgr());
        return tmp;
    }

    ASSERT0(mx);
    MDSet const* pts = getPointTo(tmp->id(), *mx);
    MD const* typed_md = nullptr;
    if (pts != nullptr) {
        ASSERTN(!pts->is_empty(), ("should not exist empty hashed set"));
        if (pts->is_contain_global() &&
            (typed_md = queryTBAA(ir)) != nullptr) {
            setPointToUniqueMD(tmp->id(), *mx, typed_md);
            mds->bunion(typed_md, *getSBSMgr());
            return tmp;
        }
        if (isWorstCase(pts)) {
            AC_hashed_mds(ic) = getWorstCase();
            return tmp;
        }
        //Just return the POINT-TO set.
        AC_hashed_mds(ic) = pts;
        return tmp;
    }
    if ((typed_md = queryTBAA(ir)) != nullptr) {
        setPointToMDSetByAddMD(tmp->id(), *mx, typed_md);
        mds->bunion(typed_md, *getSBSMgr());
        return tmp;
    }
    //We do NOT known where p pointed to.
    AC_hashed_mds(ic) = getWorstCase();
    if (ir->isPtr()) {
        setPointTo(tmp->id(), *mx, getWorstCase());
    }
    return tmp;
}


//'mds' : record memory descriptor of 'ir'
MD const* AliasAnalysis::assignLoadMD(IR * ir, MOD MDSet * mds, MOD AACtx * ic,
                                      MOD MD2MDSet * mx)
{
    ASSERT0(ir->is_ld());
    ASSERT0(mds && ic);
    MD const* t = nullptr;
    if (!m_is_visit.is_contain(ir->id())) {
        m_is_visit.bunion(ir->id());
        AC_is_mds_mod(ic) = true;
    } else {
        AC_is_mds_mod(ic) = false;
    }
    t = ir->getMustRef();
    ASSERT0(t);
    AC_hashed_mds(ic) = nullptr;
    mds->clean(*getSBSMgr());
    if (!ic->is_comp_pts()) {
        //The MD should be recorred in 'mds'.
        //Parent expression may retrive 'mds'.
        mds->bunion(t, *getSBSMgr());
        return t;
    }

    ASSERT0(mx);
    AC_is_mds_mod(ic) = true;
    MDSet const* pts = getPointTo(t->id(), *mx);
    MD const* typed_md = nullptr;
    if (pts != nullptr) {
        ASSERT0(!pts->is_empty());
        if (pts->is_contain_global() && (typed_md = queryTBAA(ir)) != nullptr) {
            setPointToUniqueMD(t->id(), *mx, typed_md);
            mds->bunion(typed_md, *getSBSMgr());
        } else {
            AC_hashed_mds(ic) = pts;
        }
        return t;
    }

    if ((typed_md = queryTBAA(ir)) != nullptr) {
        setPointToMDSetByAddMD(t->id(), *mx, typed_md);
        mds->bunion(typed_md, *getSBSMgr());
        return t;
    }

    //We do NOT known where p pointed to.
    //e.g: If p->{NULL} (p+2)->??
    AC_hashed_mds(ic) = getWorstCase();
    if (ir->isPtr()) {
        setPointTo(t->id(), *mx, getWorstCase());
    }
    return t;
}


//Assign unique MD to 'id'.
//'mds': record memory descriptor of 'ir'.
MD const* AliasAnalysis::assignIdMD(IR * ir, MOD MDSet * mds, MOD AACtx * ic)
{
    ASSERT0(ir->is_id());
    ASSERT0(ic && mds);
    MD const* t = nullptr;
    AC_hashed_mds(ic) = nullptr;
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
    if (cnt == 0) { return getWorstCase(); }
    MDSetIter iter = nullptr;
    if (cnt == 1) {
        MDSet const* pts = getPointTo((UINT)mds.get_first(&iter),
                                      *const_cast<MD2MDSet*>(mx));
        if (pts != nullptr && !pts->is_empty()) {
            return pts;
        }
        //We do NOT known where current pointer pointed to, return
        //the worst may-point-to set.
        //Empty POINT-TO set may cause solver miss POINT-TO-PAIR,
        //thus set returned pts to be the worst set.
        return getWorstCase();
    }

    MDSet tmp;
    iter = nullptr;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        MDSet const* pts = getPointTo((MDIdx)i, *const_cast<MD2MDSet*>(mx));
        if (pts != nullptr && !pts->is_empty()) {
            tmp.bunion(*pts, *getSBSMgr());
        } else {
            //We do NOT known where current pointer pointed to, return
            //the worst may-point-to set.
            //Empty POINT-TO set may cause solver miss POINT-TO-PAIR,
            //thus set returned pts to be the worst set.
            tmp.clean(*getSBSMgr());
            return getWorstCase();
        }
    }
    MDSet const* p = m_mds_hash->append(tmp);
    tmp.clean(*getSBSMgr());
    return p;
}

//This function set MustAddr or MayAddr of ir by analyszing given MDSet.
//mds: mds may be the MayAddr MDSet. Note mds should have been hashed.
void AliasAnalysis::setMustOrMayAddr(MDSet const* mds, MOD IR * ir)
{
    ASSERT0(mds && ir && m_mds_hash->find(*mds));
    ir->cleanMustRef();
    if (mds->get_elem_count() == 1 && !mds->is_contain_global()) {
        MDSetIter iter = nullptr;
        ir->setMustRef(m_md_sys->getMD((UINT)mds->get_first(&iter)), m_rg);
        ir->cleanMayRef();
        return;
    }
    ir->setMayRef(mds, m_rg);
}


//refmds: ref MDSet of ir.
//ir: given indirect operation, such as IST, ILD.
//comp_ir_pt: true if caller require to compute the POINT-TO set of ir.
//Return POINT-TO set of ir, if comp_ir_pts is true.
MDSet const* AliasAnalysis::updateIndirectOpAddrAndPointToSet(
        MDSet const* refmds, MOD IR * ir, bool comp_ir_pts, MD2MDSet * mx)
{
    ASSERT0(refmds && ir);
    ASSERT0(ir->is_ist() || ir->is_ild());
    setMustOrMayAddr(refmds, ir);
    if (!comp_ir_pts) { return nullptr; }
    //According to the requirement of parent expression to
    //compute the POINT-TO set of ILD itself.
    //For conservatively,
    MDSet const* hashed_mds = unifyPointToSet(*refmds, mx);

    //Empty POINT-TO set may cause solver miss POINT-TO-PAIR,
    //thus set returned pts to be the worst set.
    ASSERT0(hashed_mds);
    return hashed_mds;
}


//'ir': IR expressions that describing memory address.
//'mds': record memory descriptors which 'ir' represented.
//'ic': context of analysis.
void AliasAnalysis::processILoad(IR * ir, MOD MDSet & mds,
                                 MOD AACtx * ic, MOD MD2MDSet * mx)
{
    ASSERT0(ir->is_ild());
    ASSERT0(g_is_support_dynamic_type || ILD_base(ir)->is_ptr());

    //... = *q, if q->x, set ir's MD to be 'x'.
    AACtx tic(*ic);

    //Ask base-expression of ILD to compute what ILD stand for.
    AC_comp_pts(&tic) = true;

    //Compute the memory address that ILD described.
    inferExpression(ILD_base(ir), mds, &tic, mx);

    ASSERT0_DUMMYUSE(is_legal_set(mds, &tic));
    if (mds.is_empty()) {
        //Compute ILD ref MDSet and POINT-TO set.
        //If mds is empty, the inaccurate POINT-TO set
        //of ILD_base(ir) recorded in AC_hashed_mds.
        ASSERTN(tic.get_hashed(), ("mds and returne_pts are alternative"));

        //The POINT-TO set of base-expression indicates what ILD stand for.
        ASSERTN(tic.is_comp_pts(), ("expected POINT-TO"));
        MDSet const* pts_of_base = tic.get_hashed();
        if (!isWorstCase(pts_of_base)) {
            //If ir has exact type, try to reshape MD to make ir's
            //reference MD more accurate.
            MDSet tmp;
            if (!ir->is_any() && tryReshapeMDSet(ir, pts_of_base, &tmp)) {
                //If MD in 'ildrefmds' changed, set ildrefmds to be new MDSet.
                pts_of_base = m_mds_hash->append(tmp);
            }
            tmp.clean(*getSBSMgr());
        }
        //According to the requirement of parent expression to
        //compute the POINT-TO set of ILD itself.
        AC_hashed_mds(ic) = updateIndirectOpAddrAndPointToSet(pts_of_base, ir,
            ic->is_comp_pts(), mx);
        return;
    }

    ASSERTN(ic->get_hashed() == nullptr,
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
    MD const* mustref = regardAsSingleMD(&mds, m_rg);
    if (mustref != nullptr) {
        ir->setMustRef(mustref, m_rg);
        ir->cleanMayRef();
    } else if (isAllElementDerivedFromSameEffectVar(mds, m_md_sys, &mustref)) {
        ASSERT0(mustref);
        ir->setMustRef(mustref, m_rg);
        ir->cleanMayRef();
    } else {
        ir->cleanMustRef();
        ir->setMayRef(m_mds_hash->append(mds), m_rg);
    }

    if (!ic->is_comp_pts()) { return; }

    //According the requirement of parent function, we have to compute
    //the POINT-TO set of cuurent ILD.

    //Compute the ILD pointed to.
    if (mustref != nullptr) {
        MDSet const* pts = getPointTo(mustref->id(), *mx);
        if (pts != nullptr) {
            ASSERT0(!pts->is_empty());
            mds.copy(*pts, *getSBSMgr());
        } else {
            //We do NOT known where p pointed to, and compute
            //the offset as well.
            //e.g: If p->{NULL} (p+2)->??
            mds.clean(*getSBSMgr());
            AC_hashed_mds(ic) = getWorstCase();
        }
        return;
    }

    if (hasElemPointToWorstCase(mds, this, *mx)) {
        mds.clean(*getSBSMgr());
        AC_hashed_mds(ic) = getWorstCase();
        return;
    }

    MDSet tmp;
    MDSetIter it;
    //'mds' represents the may-address of current ILD.
    for (BSIdx i = mds.get_first(&it);
         i != BS_UNDEF; i = mds.get_next(i, &it)) {
        MDSet const* pts = getPointTo((MDIdx)i, *mx);
        ASSERT0(pts && !isWorstCase(pts));
        tmp.bunion(*pts, *getSBSMgr());
    }
    mds.copy(tmp, *getSBSMgr());
    tmp.clean(*getSBSMgr());
}


//'mds' : record memory descriptor of 'ir'.
void AliasAnalysis::processConst(IR const* ir, MOD MDSet & mds, MOD AACtx * ic)
{
    ASSERT0(ir->is_const());
    ASSERT0(ic);

    mds.clean(*getSBSMgr());
    if (ir->is_str()) {
        //'ir' describes memory address of string const.
        //Add a new Var to describe the string.
        //'mds' : record memory descriptor of 'ir'.
        m_rg->getMDMgr()->allocStringMD(CONST_str_val(ir));
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

    if (ic->is_comp_pts()) {
        //We do NOT known where const pointed to,
        //e.g: User could regard immediate value 0x1234 as memory address.
        AC_hashed_mds(ic) = getWorstCase();
    }
}


MD const* AliasAnalysis::reviseMDSize(IR const* ir, MD const* md)
{
    if (md->is_exact() && !ir->is_any() &&
        ir->getTypeSize(m_tm) != md->getByteSize()) {
        MD newmd(*md);

        //Note if ir's type is ANY, size is 1.
        //Thus the MD indicates an object that is
        //p + ist_ofst + 0, if ist_ofst exist.
        MD_size(&newmd) = ir->getTypeSize(m_tm);
        MD const* entry = m_md_sys->registerMD(newmd);
        ASSERTN(entry->id() > MD_UNDEF, ("Not yet registered"));
        md = entry;
    }
    return md;
}


//Recompute the data type byte-size according to stmt type.
//Note we only revise data type if LDA on the RHS expression, and if
//LDA appeared on RHS, its MD will be put in 'mds' if comp_pts is true.
//For those MDs in 'hashed_mds' when 'mds' is empty, we just keep them
//unchanged.
//It have to note that if LDA appeared in RHS, mds should not be empty.
void AliasAnalysis::recomputeDataType(IR const* ir, AACtx const& ic,
                                      MOD MDSet & mds)
{
    ASSERT0(ir && ir->is_stmt());
    if (!ic.is_taken_addr() || ir->is_any() || !ir->is_ptr()) { return; }

    //If RHS of 'ir' return the address which taken by IR_LDA.
    //Here we have to reinference the actual memory address according
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

    //CASE: mds may include elements which have been in getWorstCase().
    //ASSERT0(!mds->is_intersect(*getWorstCase()));
    reviseMDSize(mds, m_tm->getPointerBaseByteSize(ir->getType()));
}


//ir: stmt.
//rhs: RHS expression of ir. It's refdu might be updated by this function.
//lhs_md: MD of LHS of 'ir'.
//ic: analysis context.
void AliasAnalysis::inferStoreValue(IR const* ir, IR * rhs, MD const* mustref,
                                    AACtx const* ic, MOD MD2MDSet * mx)
{
    ASSERT0(ir->isWritePR() || ir->isDirectMemOp());

    //Considering the STORE operation, there
    //are three situations should be processed:
    //    1. The RHS's address was taken.
    //    2. The RHS is ILD, that cause indirect POINT-TO generated.
    //        e.g: p=ILD(q), if q->a->w, then p->w.
    //    3. Propagate the MDSet that RHS pointed to.
    //        e.g: p=q, if q->a, then p->a.
    ASSERT0(mustref);
    if (!g_is_support_dynamic_type) {
        //lhs of IR_ST may be inexact because IR_ST may be ANY.
        ASSERT0(mustref->is_exact());
    }
    inferRHSAndUpdateLHS(ir, rhs, mustref, nullptr, ic, mx);
}


//The function will infer POINT-TO of ir and update the POINT-TO information of
//mustref and mayref.
//rhs: RHS of ir, ir should be stmt. It's reference MD will be computed.
void AliasAnalysis::inferRHSAndUpdateLHS(IR const* ir, IR * rhs,
                                         MD const* mustref,
                                         MDSet const* mayref,
                                         AACtx const* ic,
                                         MOD MD2MDSet * mx)
{
    ASSERT0_DUMMYUSE(is_legal_ref(ir));
    //Considering the indirect operation, there are three
    //situations should be handled:
    // 1.The RHS is LDA.
    // 2.The RHS is ILD, that caused indirect POINT-TO generated.
    //   e.g: *p=ILD(q), and p->x,q->a,a->w, then x->w,
    // 3.Propagate the POINT-TO set of RHS to the LHS.
    //   e.g: *p=q, and p->x,q->a, then x->a.
    //More normal explain:
    // 1. p = q, q is pointer, if q->x, add p->x.
    // 2. p = q, q is array base (e.g:q[100]), add p->q.
    // 3. p = &q, add p->q.
    // 4. p = (&q)+n+m, add p->q.
    AACtx rhsic(*ic);
    if (ir->isPtr()) {
        //Regard ir as pointer if its has ANY type.
        //Query and ask subroutine infer and compute the POINT-TO set
        //of 'rhs' expression.
        AC_comp_pts(&rhsic) = true;
    }

    MDSet rhsrefmds;
    if (rhs != nullptr) {
        inferExpression(rhs, rhsrefmds, &rhsic, mx);
    }
    if (rhsic.is_comp_pts()) {
        ASSERT0_DUMMYUSE(is_legal_set(rhsrefmds, &rhsic));
    } else {
        //Both rhs's mds and hashed_mds are empty if RHS is constant.
    }
    recomputeDataType(ir, rhsic, rhsrefmds);
    updateLHSPointToSet(rhsic.is_comp_pts(), rhsic.is_taken_addr(),
                        mustref, mayref, rhs, rhsrefmds,
                        rhsic.get_hashed(), mx);
    rhsrefmds.clean(*getSBSMgr());
}


//Caculate pointer info according to rules for individiual ir, and
//constructing the mapping table that maps MD to an unique Var.
//e.g For given four point-to pairs {p->a,p->b,q->c,q->d}.
//    store can be shown as
//        p = q;
//    this make the point-to set of 'p' to be {p->c, p->d}.
//    and the formally formular form is:
//    MDSet(p) = MDSet(q)
void AliasAnalysis::processDirectMemOp(IR const* ir, MOD MD2MDSet * mx)
{
    MD const* t = ir->getMustRef();
    ASSERT0(t);
    AACtx ic;
    inferStoreValue(ir, ir->getRHS(), t, &ic, mx);
}


//Compute the point to info for IR_SETELEM.
void AliasAnalysis::processSetElem(IR * ir, MOD MD2MDSet * mx)
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
void AliasAnalysis::processGetElem(IR * ir, MOD MD2MDSet * mx)
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


//TODO:compute point-to set through different phi-operand since phi-operand
//described the direction of each predecessor.
void AliasAnalysis::processPhiOpndPTS(IR const* ir, bool phi_pts_is_worst,
                                      MDSet & phi_pts, MOD MD2MDSet * mx)
{
    MD const* phi_md = ir->getMustRef();
    ASSERT0(phi_md);
    MDSet tmp;
    for (IR * opnd = PHI_opnd_list(ir);
         opnd != nullptr; opnd = opnd->get_next()) {
        AACtx tic;
        AC_comp_pts(&tic) = true;
        inferExpression(opnd, tmp, &tic, mx);
        if (phi_pts_is_worst) {
            tmp.clean(*getSBSMgr());
            continue;
        }

        ASSERT0_DUMMYUSE(is_legal_set(tmp, &tic));
        if (tmp.is_empty()) {
            ASSERT0(tic.get_hashed());
            phi_pts_is_worst = true;
            continue;
        }

        //phi result PR may point to the union set of each operand.
        phi_pts.bunion(tmp, *getSBSMgr());

        tmp.clean(*getSBSMgr());
    }
    ASSERT0(!phi_pts.is_empty() || phi_pts_is_worst);
    if (phi_pts_is_worst) {
        setPointTo(phi_md->id(), *mx, getWorstCase());
    } else {
        setPointTo(phi_md->id(), *mx, m_mds_hash->append(phi_pts));
    }
    tmp.clean(*getSBSMgr());
    phi_pts.clean(*getSBSMgr());
}


void AliasAnalysis::processPhi(IR const* ir, MOD MD2MDSet * mx)
{
    ASSERT0(ir->is_phi());
    MD const* phi_md = ir->getMustRef();
    ASSERT0(phi_md);
    AACtx ic;
    if (ir->isPtr()) {
        AC_comp_pts(&ic) = true;
    }

    MDSet phi_pts;
    bool phi_pts_is_worst = false;
    if (ic.is_comp_pts()) {
        MDSet const* pts = getPointTo(phi_md->id(), *mx);
        if (pts != nullptr) {
            if (isWorstCase(pts)) {
                phi_pts_is_worst = true;
            } else {
                phi_pts.copy(*pts, *getSBSMgr());
            }
        }
    } else {
        cleanPointTo(phi_md->id(), *mx);
    }
    if (ic.is_comp_pts()) {
        processPhiOpndPTS(ir, phi_pts_is_worst, phi_pts, mx);
        return;
    }
    MDSet tmp;
    for (IR * opnd = PHI_opnd_list(ir);
         opnd != nullptr; opnd = opnd->get_next()) {
        AACtx tic(ic);
        inferExpression(opnd, tmp, &tic, mx);
        tmp.clean(*getSBSMgr());
    }
    tmp.clean(*getSBSMgr());
    phi_pts.clean(*getSBSMgr());
}


bool AliasAnalysis::isPointToDedicatedVar(MD const* md, MD2MDSet const& mx)
{
    MDSet const* pts = getPointTo(md->id(), mx);
    if (pts == nullptr) { return false; }

    //If ir's type is ANY, then the size is 1, see details in data_type.h.
    //Thus MD indicates an object that is p + ild_ofst + 0.
    MD const* unique = pts->get_unique_md(m_md_sys);
    if (unique == nullptr) { return false; }
    MD const* dmd = m_dedicated_var2md.get(md->get_base());

    //If 'md' has NOT been assigned a dedicated Var, return false.
    return dmd == unique;
}


//This function update LHS's POINT-TO set according to RHS.
//is_lhs_pointer: true if transit rhs's POINT-TO set to lhs.
//rhs: RHS expression of stmt, which are IR_ST, IR_IST, IR_STARRAY.
//rhsrefmds: record memory descriptor of 'rhs' if AC_comp_pts() is false, or
//           record the POINT-TO set of 'rhs' if AC_comp_pts() is true.
//           Note if AC_comp_pts() is true, the returned POINT-TO set may be
//           recorded in 'rhsrefmds' or AC_hashed_mds.
//hashed_mds: record the POINT-TO set of 'rhs' if AC_comp_pts() is true.
//              Note if AC_comp_pts() is true, the returned POINT-TO set may be
//              recorded in 'rhsrefmds' or AC_hashed_mds.
void AliasAnalysis::updateLHSPointToSet(bool is_lhs_pointer,
                                        bool rhs_taken_address,
                                        MD const* lhs_mustref,
                                        MDSet const* lhs_mayref,
                                        IR const* rhs,
                                        MDSet & rhsrefmds,
                                        MDSet const* hashed_mds,
                                        MD2MDSet * mx)
{
    if (rhs_taken_address || is_lhs_pointer) {
        if (lhs_mustref != nullptr && lhs_mustref->is_restrict()) {
            ASSERTN(isPointToDedicatedVar(lhs_mustref, *mx),
                    ("miss PTS of restrict var"));
            return;
        }
        //If result type of IST is pointer, and the pts is empty, then
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
        if (rhs_taken_address && is_lhs_pointer) {
            ASSERT0(rhsrefmds.get_elem_count() == 1);

            //POINT-TO set may include element which also have been in
            //getWorstCase().
            //CASE: =&g[i] violate this constraint.
            //ASSERT0(!pts->is_intersect(getWorstCase()));
        }

        //'rhsrefmds' should record the POINT-TO set of 'rhs'.
        MDSet const* lhs_pts = nullptr;
        if (rhsrefmds.is_empty()) {
            if (hashed_mds == nullptr) {
                //Since stmt is not pointer, caller did not demand RHS
                //infering the point-to set.
                ASSERT0(!is_lhs_pointer);
                hashed_mds = getWorstCase();
            } else {
                ASSERTN(!hashed_mds->is_empty(), ("they are alternatively"));
                ASSERT0(m_mds_hash->find(*hashed_mds));
            }
            lhs_pts = computeMayPointToViaTBAA(rhs, hashed_mds);
        } else {
            lhs_pts = computeMayPointToViaTBAA(rhs, &rhsrefmds);
            if (lhs_pts == &rhsrefmds) {
                lhs_pts = m_mds_hash->append(rhsrefmds);
            }
        }
        ASSERT0(lhs_pts && !lhs_pts->is_empty());
        setLHSPointToSet(lhs_mustref, lhs_mayref, lhs_pts, mx);
        return;
    }

    //p = q, q is pointer, if q->x, add p->x.
    //Given a pointer, if its point-to is empty, the pointer
    //points to the worst MAY_POINT_TO set.
    //
    //May be unify MAY_POINT_TO set is correct in comprehension,
    //but it will consume more memory.
    //e.g: Given pr1->MAY_POINT_TO, if we set
    //pr1->nullptr, that may cause convertMD2MDSet2PT()
    //can not recognize pr1's POINT-TO set, which lead to its pt-pair info
    //missing. That will cause a dead cycle at global iterative solver.
    if (!m_flow_sensitive) { return; }

    //CASE: LHS is NOT a pointer.
    ASSERT0(!is_lhs_pointer);
    if (lhs_mustref != nullptr) {
        cleanPointTo(lhs_mustref->id(), *mx);
        return;
    }

    ASSERT0(lhs_mayref);
    //Clean POINT-TO even if mayref may contain inexact MD.
    ElemCleanPointTo(*lhs_mayref, mx);
}


//Set the POINT-TO set of LHS MD and LHS MDSet.
//pts: POINT-TO set that have been hashed.
void AliasAnalysis::setLHSPointToSet(MD const* lhs_mustref,
                                     MDSet const* lhs_mayref,
                                     MDSet const* pts, MD2MDSet * mx)
{
    ASSERT0(pts && m_mds_hash->find(*pts));
    if (m_flow_sensitive) {
        if (lhs_mustref != nullptr) {
            if (lhs_mustref->is_exact()) {
                setPointTo(lhs_mustref->id(), *mx, pts);
            } else {
                setPointToMDSetByAddMDSet(lhs_mustref->id(), *mx, *pts);
            }
            return;
        }

        ASSERT0(lhs_mayref);
        //mayref may contain inexact MD.
        ElemCopyAndUnionPointTo(*lhs_mayref, *pts, mx);
        return;
    }

    //Flow insensitive.
    if (lhs_mustref != nullptr) {
        setPointToMDSetByAddMDSet(lhs_mustref->id(), *mx, *pts);
        return;
    }

    ASSERT0(lhs_mayref);
    ElemUnionPointTo(*lhs_mayref, *pts, mx);
}


//Infer point-to set for array element.
//e.g For given 2 point-to pairs {q->c,q->d}.
//  store array can be demonstrated as
//    a[x] = q;
//  this changes POINT-TO set of a[x] to {a[x]->c, a[x]->d}.
void AliasAnalysis::processStoreArray(IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_starray());
    //mem location may pointed to set.
    MDSet mds;
    AACtx ic;
    AC_comp_pts(&ic) = false; //Here we just compute the may reference.

    //Compute where array element may point to.
    //Note ir should be assign MustRef or MayRef when
    //processArray() return.
    processArray(ir, mds, &ic, mx);

    //mds is useless when processArray() return.
    //If array offset is not zero, the result data type may not
    //being the element type. Try to infer the actual memory address of
    //array element. All of above situations have been handled inside
    //processArray().
    ic.clean();
    mds.clean(*getSBSMgr());

    //We do no need to known where array's elem point-to.
    AC_comp_pts(&ic) = false;

    //Infer the memory address for RHS and POINT-TO set of LHS.
    MDSet const* mayref = ir->getMayRef();
    MD const* mustref = ir->getMustRef();
    inferRHSAndUpdateLHS(ir, ir->getRHS(), mustref, mayref, &ic, mx);
}


//Reshape MD in 'mds' if one of them need to be reshaped.
//Record reshaped MD in 'newmds' and return true.
//This function will iterate MD that is in 'mds', new MD will be generated
//either ir's MD size is different to MD in 'mds' or ir has offset.
//Return true if new MD generated, and new MD will be recorded in 'newmds'.
bool AliasAnalysis::tryReshapeMDSet(IR const* ir, MDSet const* mds,
                                    OUT MDSet * newmds)
{
    if (ir->is_any()) { return false; }
    TMWORD newofst = ir->getOffset();
    UINT newsize = ir->getTypeSize(m_tm);
    ASSERT0(newsize > 0);
    ASSERT0(mds && newmds);
    bool change = false;
    MDSetIter iter = nullptr;
    //Note if ir's type is ANY, then the size is 1, see details in data_type.h.
    //Thus MD indicates an object that is p + ild_ofst + 0.
    for (BSIdx i = mds->get_first(&iter);
         i != BS_UNDEF; i = mds->get_next(i, &iter)) {
        MD const* l = m_md_sys->getMD((MDIdx)i);
        ASSERT0(l);
        if (l->is_exact() && (l->getByteSize() != newsize || newofst != 0)) {
            //Reshape MD in IR reference.
            MD md(*l);
            MD_ofst(&md) += newofst;
            MD_size(&md) = newsize;
            MD const* entry = m_md_sys->registerMD(md);
            ASSERTN(entry->id() > MD_UNDEF, ("Not yet registered"));
            newmds->bunion(entry->id(), *getSBSMgr());
            change = true;
            continue;
        }

        newmds->bunion((UINT)i, *getSBSMgr());
    }
    return change;
}


//Indirect store.
//Analyse pointers according to rules for individiual ir to
//constructe the map-table that maps MD to an unique Var.
void AliasAnalysis::processIndirectMemOp(MOD IR * ir, IN MD2MDSet * mx)
{
    IR * base = ir->getBase();
    ASSERT0(g_is_support_dynamic_type || base->is_ptr());

    //mem location that base-expression may pointed to.
    MDSet base_maypts;
    AACtx ic;

    //Compute where base may point to.
    AC_comp_pts(&ic) = true;

    //Compute where base may point to.
    inferExpression(base, base_maypts, &ic, mx);
    ASSERT0_DUMMYUSE(is_legal_set(base_maypts, &ic));

    //In fact, The POINT-TO set of base-expression indicates what IST
    //may reference.
    if (base_maypts.is_empty()) {
        //Compute IST ref MDSet and POINT-TO set.
        //If base_maypts is empty, the inaccurate POINT-TO set
        //of base recorded in AC_hashed_mds.
        //The POINT-TO set of base-expression indicates what IST stands for.
        MDSet const* istrefmds = ic.get_hashed();
        ASSERT0(istrefmds);

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
        AC_hashed_mds(&ic) = updateIndirectOpAddrAndPointToSet(
            istrefmds, ir,
            false, //set false to tell function do not need to compute
                   //POINT-TO set, it will be computed by inferRHSAndUpdateLHS()
                   //later. Avoid redundant computation.
            mx);
        AACtx ic2;
        MDSet const* mayref = ir->getMayRef();
        MD const* mustref = ir->getMustRef();
        inferRHSAndUpdateLHS(ir, ir->getRHS(), mustref, mayref, &ic2, mx);
        return;
    }

    //In fact, The POINT-TO set of base-expression indicates what IST
    //may reference.
    //The POINT-TO set of IST recored in AC_hashed_mds() if maypts is empty.
    //base_maypts and hashed_mds are alternative.
    ASSERT0(ic.get_hashed() == nullptr);
    if (!ir->is_any()) {
        MDSet tmp;
        bool change = tryReshapeMDSet(ir, &base_maypts, &tmp);
        if (change) {
            base_maypts.copy(tmp, *getSBSMgr());
        }
        tmp.clean(*getSBSMgr());
    }
    //Attemp to reduce the MDSet to a single MD if possible to make
    //IR's MD referrence more precise.
    MD const* x = regardAsSingleMD(&base_maypts, m_rg);
    if (x != nullptr) {
        ir->setMustRef(reviseMDSize(ir, x), m_rg);
        ir->cleanMayRef();
    } else {
        //Set ir with inexact mem-addr for convervative purpose.
        ir->cleanMustRef();
        ir->setMayRef(m_mds_hash->append(base_maypts), m_rg);
    }
    base_maypts.clean(*getSBSMgr()); //free tmp MDSet.

    AACtx ic2;
    MDSet const* mayref = ir->getMayRef();
    MD const* mustref = ir->getMustRef();
    inferRHSAndUpdateLHS(ir, ir->getRHS(), mustref, mayref, &ic2, mx);
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
    if (defmds != nullptr) {
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


void AliasAnalysis::processRegionSideeffect(MOD MD2MDSet & mx)
{
    //Set all mds which are global pointers or parameters which taken
    //address point to maypts.
    MDId2MD const* id2md = m_md_sys->getID2MDMap();
    for (VecIdx j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
        MD * t = id2md->get((UINT)j);
        if (t == nullptr) {
            //MD j has been allocated but freed and record in free-list.
            continue;
        }

        Var const* v = t->get_base();
        if (v->is_pointer() ||
            v->is_any()) { //v may be pointer if its type is ANY
            setPointTo((UINT)j, mx, getWorstCase());
        }
    }
}


void AliasAnalysis::processReturn(IR const* ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->is_return());
    if (RET_exp(ir) != nullptr) {
        MDSet tmp;
        AACtx tic;
        ASSERT0(RET_exp(ir) == nullptr || RET_exp(ir)->is_single());
        ASSERT0(RET_exp(ir)->is_single());
        inferExpression(RET_exp(ir), tmp, &tic, mx);
        tmp.clean(*getSBSMgr());
    }
}


void AliasAnalysis::processCallSideeffect(MOD MD2MDSet & mx,
                                          MDSet const& by_addr_mds)
{
    //Set all elem in MDSet which are global pointers or parameters which be
    //taken address points to maypts.
    MDId2MD const* id2md = m_md_sys->getID2MDMap();
    for (VecIdx j = MD_FIRST; j <= id2md->get_last_idx(); j++) {
        MD const* t = id2md->get((UINT)j);
        if (t == nullptr) { continue; }

        Var const* v = t->get_base();
        if ((v->is_global() || v->is_taken_addr()) && v->isPointer()) {
            setPointTo((UINT)j, mx, getWorstCase());

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
    if (heap_obj != nullptr) {
        return heap_obj;
    }

    CHAR name[128];
    sprintf(name, "heap_obj%d", m_ir2heapobj.get_elem_count());
    ASSERT0(::strlen(name) < 128);
    Var * tv = m_rg->getVarMgr()->registerVar(name, m_tm->getMCType(0),
                                              0, VAR_GLOBAL);

    //Set the var to be unallocable, means do NOT add
    //var immediately as a memory-variable.
    //For now, it is only be regarded as a placeholder.
    //And set it to allocable if the var is in essence need to be
    //allocated in memory.
    tv->setFlag(VAR_IS_UNALLOCABLE);

    //Will be freed region destruction.
    m_rg->addToVarTab(tv);

    MD md;
    MD_base(&md) = tv;
    //Use UNBOUND to guarrantee the abstract heap object will not be
    //regared as must-exact object that will confuse killing computation.
    MD_ty(&md) = MD_UNBOUND;
    MD const* entry = m_md_sys->registerMD(md);
    ASSERT0(entry->id() > MD_UNDEF);
    m_ir2heapobj.set(ir, entry);
    return entry;
}


//Regard MDSet that parameters pointed-to as the MDSet that referrenced by call.
static void setMayDefSetForCall(IR * ir, AliasAnalysis * aa)
{

    for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
        if (!p->isPtr()) { continue; }

        //Get POINT-TO that p pointed to.
        //e.g: foo(p); where p->{x, y, z},
        //     then foo() may use {p, x, y, z}.
        //NOTE that POINT-TO is only available for the
        //last stmt of BB. The call is just in the situation.
        //MDSet maydefuse;
        //MDSet const* hashed = nullptr;
        //aa->computeMayPointTo(p, maydefuse, &hashed);
        //////////////////////////////////////////////////////////////
        //CASE1: bar(p); where p->{x,y}, x->{w} we can not only collect
        //the MD that p pointed to, because bar() may modify w through
        //pointer x. Thus there will be a conservative result, the
        //whole worst-case MDSet, namely may-point-to set.
        //
        //CASE2: For conservative purpose.
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
        //This is a case to the WORST PRECISION: A call should not
        //reference all elements in may-point-to set, because local
        //variables that are not be taken address can not be accessed through
        //call. In order to improve precision, we have to query MayDef/MayUse
        //from CallGraph.
        ASSERT0(aa->getWorstCase());
        ir->setMayRef(aa->getWorstCase(), aa->getRegion());
        return;
    }
}


//Compute the point-to set modification when we meet call.
void AliasAnalysis::processCall(MOD IR * ir, IN MD2MDSet * mx)
{
    ASSERT0(ir->isCallStmt());
    if (ir->hasReturnValue() && !m_is_visit.is_contain(ir->id())) {
        //Generate MustRef if call has return-value.
        m_is_visit.bunion(ir->id());
        m_rg->getMDMgr()->allocRef(ir);
    }
    MDSet tmp;
    if (ir->is_icall()) {
        AACtx tic;
        inferExpression(ICALL_callee(ir), tmp, &tic, mx);
    }

    //Analyze the point-to of each parameters.
    MDSet by_addr_mds;
    for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
        AACtx tic;
        if (p->isPtr()) {
            AC_comp_pts(&tic) = true;
        }
        inferExpression(p, tmp, &tic, mx);

        if (ir->isReadOnly()) { continue; }
        if (!tic.is_comp_pts() && !tic.is_taken_addr()) { continue; }
        if (!tmp.is_empty()) {
            by_addr_mds.bunion(tmp, *getSBSMgr());
            continue;
        }
        if (tic.get_hashed() != nullptr &&
            !by_addr_mds.is_equal(*tic.get_hashed())) {
            by_addr_mds.bunion(*tic.get_hashed(), *getSBSMgr());
        }
    }

    //Infer the MD reference of each parameters.
    for (IR * p = CALL_dummyuse(ir); p != nullptr; p = p->get_next()) {
        AACtx tic;
        inferExpression(p, tmp, &tic, mx);
    }

    setMayDefSetForCall(ir, this);

    if (CALL_is_alloc_heap(ir)) {
        if (ir->hasReturnValue()) {
            //The return-value pointed to HEAP memory.
            MD const* t = ir->getMustRef();
            ASSERT0(t);
            setPointToUniqueMD(t->id(), *mx, allocHeapobj(ir));
        }

        //The function such as malloc or new function should not modify
        //the memory in XOC scope.
        tmp.clean(*getSBSMgr());
        by_addr_mds.clean(*getSBSMgr());
        return;
    }
    if (ir->hasReturnValue()) {
        //Analyze return-value.
        MD const* t = ir->getMustRef();
        ASSERTN(t, ("result of call must be exact PR MD."));
        if (ir->isPtr()) {
            //Try to improve the precsion via typed alias info or
            //set ir pointed to May-Point-To set for conservative purpose.
            MD const* typed_md = queryTBAA(ir);
            if (typed_md != nullptr) {
                //Make use of typed pointer info to improve the precsion.
                setPointToUniqueMD(t->id(), *mx, typed_md);
            } else {
                //Finally, set result PR points to May-Point-To set.
                setPointTo(t->id(), *mx, getWorstCase());
            }
        } else {
            cleanPointTo(t->id(), *mx);
        }
    }
    if (ir->isReadOnly()) {
        //Readonly call does not modify any point-to informations.
        tmp.clean(*getSBSMgr());
        ASSERT0(by_addr_mds.is_empty());
        by_addr_mds.clean(*getSBSMgr());
        return;
    }
    processCallSideeffect(*mx, by_addr_mds);
    tmp.clean(*getSBSMgr());
    by_addr_mds.clean(*getSBSMgr());
}


void AliasAnalysis::inferExtExpression(IR * ir, MOD MDSet & mds,
                                       MOD AACtx * ic, MOD MD2MDSet * mx)
{
    ASSERT0(ir->is_exp());
    switch (ir->getCode()) {
    SWITCH_CASE_EXT_EXP: {
        AACtx tic(*ic);
        AC_comp_pts(&tic) = false;
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * kid = ir->getKid(i);
            if (kid != nullptr) {
                tic.cleanBottomUpFlag();
                inferExpression(kid, mds, &tic, mx);
            }
        }
        //CASE: if (p && q)
        //GR: land (ld p:*<2>, ld q:*<2>)
        //ASSERTN(!BIN_opnd0(expr)->is_ptr(),
        //    ("illegal, left operand can not be pointer type"));
        //These expressions does not descripte
        //an accurate memory-address. So, for the
        //conservative purpose, we claim that can
        //not find any MD.
        if (ic->is_comp_pts()) {
            mds.clean(*getSBSMgr());
            AC_hashed_mds(ic) = getWorstCase();
        } else {
            mds.clean(*getSBSMgr());
        }
        return;
    }
    default: UNREACHABLE();
    }
}


//Analyze the Tree style memory-address-expression,
//and compute the MDSet for 'expr'.
//expr: IR expressions that describing memory address.
//mds: record output memory descriptors which 'expr' might express that
//     generated by this function.
//ic: context of analysis.
void AliasAnalysis::inferExpression(IR * expr, MOD MDSet & mds,
                                    MOD AACtx * ic, MOD MD2MDSet * mx)
{
    ASSERT0(expr);
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
        if (ic->is_comp_pts()) {
            processPointerArith(expr, mds, ic, mx);
        } else {
            processAddSub(expr, mds, ic, mx);
        }
        return;
    SWITCH_CASE_READ_PR:
        processPR(expr, &mds, ic, mx);
        return;
    case IR_CVT:
        processCvt(expr, mds, ic, mx);
        return;
    SWITCH_CASE_SHIFT:
    SWITCH_CASE_COMPARE:
    SWITCH_CASE_LOGIC:
    SWITCH_CASE_BITWISE:
    SWITCH_CASE_ARITH_NONLINEAR:
    case IR_SELECT:
    SWITCH_CASE_UNA_TRIGONOMETRIC:
    case IR_NEG:
    case IR_ALLOCA: {
        AACtx tic(*ic);
        AC_comp_pts(&tic) = false;
        for (UINT i = 0; i < IR_MAX_KID_NUM(expr); i++) {
            IR * kid = expr->getKid(i);
            if (kid != nullptr) {
                tic.cleanBottomUpFlag();
                inferExpression(kid, mds, &tic, mx);
            }
        }
        //CASE: if (p && q)
        //GR: land (ld p:*<2>, ld q:*<2>)
        //ASSERTN(!BIN_opnd0(expr)->is_ptr(),
        //    ("illegal, left operand can not be pointer type"));
        //These expressions does not descripte
        //an accurate memory-address. So, for the
        //conservative purpose, we claim that can
        //not find any MD.
        if (ic->is_comp_pts()) {
            mds.clean(*getSBSMgr());
            AC_hashed_mds(ic) = getWorstCase();
        } else {
            mds.clean(*getSBSMgr());
        }
        return;
    }
    case IR_LABEL:
        return;
    default: inferExtExpression(expr, mds, ic, mx);
    }
}


//Set POINT TO info.
//Set each md in 'mds' add set 'pt_set'.
//pt_set: POINT-TO set that has been hashed.
void AliasAnalysis::ElemUnionPointTo(MDSet const& mds, MDSet const& pt_set,
                                     IN MD2MDSet * mx)
{
    ASSERT0(m_mds_hash->find(pt_set));
    MDSetIter iter;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        ASSERT0(m_md_sys->getMD((MDIdx)i));
        setPointToMDSetByAddMDSet((MDIdx)i, *mx, pt_set);
    }
}


//Set POINT TO info.
//Set each md in 'mds' add 'pt_elem'.
void AliasAnalysis::ElemUnionPointTo(MDSet const& mds, MD const* pt_elem,
                                     IN MD2MDSet * mx)
{
    MDSetIter iter;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        ASSERT0(m_md_sys->getMD((MDIdx)i));
        setPointToMDSetByAddMD((MDIdx)i, *mx, pt_elem);
    }
}


//Set POINT TO info.
//Set each MD in 'mds' points to 'pt_set' if it is exact, or
//else unify the MD and 'pt_set', and records the union result as the POINT-TO
//set.
//pt_set: POINT-TO set that have been hashed.
void AliasAnalysis::ElemCopyAndUnionPointTo(MDSet const& mds,
                                            MDSet const& pt_set,
                                            IN MD2MDSet * mx)
{
    ASSERT0(m_mds_hash->find(pt_set));
    if (isWorstCase(&pt_set)) {
        MDSetIter iter = nullptr;
        for (BSIdx i = mds.get_first(&iter);
             i != BS_UNDEF; i = mds.get_next(i, &iter)) {
            ASSERT0(m_md_sys->getMD((MDIdx)i));
            //Both exact and inexact MD points to MayPointToSet.
            setPointTo((UINT)i, *mx, getWorstCase());
        }
        return;
    }

    MDSetIter iter = nullptr;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        ASSERT0(m_md_sys->getMD((MDIdx)i));
        if (m_md_sys->getMD((MDIdx)i)->is_exact()) {
            setPointTo((MDIdx)i, *mx, &pt_set);
            continue;
        }
        setPointToMDSetByAddMDSet((UINT)i, *mx, pt_set);
    }
}


//Set POINT TO info.
//Set each md in 'mds' points to May-Point-To set.
void AliasAnalysis::ElemCopyPointToAndMayPointTo(MDSet const& mds,
                                                 IN MD2MDSet * mx)
{
    MDSetIter iter = nullptr;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        ASSERT0(m_md_sys->getMD((MDIdx)i));
        setPointTo((MDIdx)i, *mx, getWorstCase());
    }
}


//Set POINT TO info.
//Set md in 'mds' points to nullptr if it is exact.
void AliasAnalysis::ElemCleanExactPointTo(MDSet const& mds, IN MD2MDSet * mx)
{
    MDSetIter iter = nullptr;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        ASSERT0(m_md_sys->getMD((MDIdx)i));
        if (m_md_sys->getMD((MDIdx)i)->is_exact()) {
            cleanPointTo((MDIdx)i, *mx);
        }
    }
}


//Set POINT TO info.
//Set md in 'mds' points to nullptr.
void AliasAnalysis::ElemCleanPointTo(MDSet const& mds, IN MD2MDSet * mx)
{
    MDSetIter iter = nullptr;
    for (BSIdx i = mds.get_first(&iter);
         i != BS_UNDEF; i = mds.get_next(i, &iter)) {
        ASSERT0(m_md_sys->getMD((MDIdx)i));
        cleanPointTo((MDIdx)i, *mx);
    }
}


//Dump POINT-TO Pair record in 'pps'.
void AliasAnalysis::dumpPtPairSet(PtPairSet const& pps) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    StrBuf buf(256);
    UINT k = 0;
    bool detail = true;
    PtPairSetIter iter;
    PtPairMgr & pptmgr = const_cast<PtPairMgr&>(m_ppmgr);
    for (BSIdx i = pps.get_first(&iter);
         i != BS_UNDEF; i = pps.get_next(i, &iter), k++) {
        PtPair * pp = pptmgr.get((UINT)i);
        ASSERT0(pp);
        note(getRegion(), "\nMD%u->MD%u,  ", PP_from(pp), PP_to(pp));

        if (!detail) { continue; }

        MD const* from = m_md_sys->getMD(PP_from(pp));
        ASSERT0(from);

        prt(getRegion(), "%s", from->get_base()->dump(buf, m_vm));
        if (from->is_exact()) {
            prt(getRegion(), ":ofst(%u):size(%u)", MD_ofst(from),
                MD_size(from));
        } else {
            prt(getRegion(), ":ofst(--):size(%u)", MD_size(from));
        }

        prt(getRegion(), " ------> ");

        MD const* to = m_md_sys->getMD(PP_to(pp));

        buf.clean();
        prt(getRegion(), "%s", to->get_base()->dump(buf, m_vm));

        if (to->is_exact()) {
            prt(getRegion(), ":ofst(%u):size(%u)", MD_ofst(to), MD_size(to));
        } else {
            prt(getRegion(), ":ofst(--):size(%u)", MD_size(to));
        }
    }
}


//The function collects all MDs that ir may pointed to.
//Return the worst case MDSet if ir may pointed to it. If ir pointed to the
//worst case, the content of 'set' is meaningless, thus can be ignored.
MDSet const* AliasAnalysis::collectMayPointTo(
    OUT MDSet & set, MOD xcom::DefMiscBitSetMgr & sbs,
    IR const* ir, MD2MDSet const& mx) const
{
    ASSERT0(ir);
    MDSet const* worst = nullptr;
    MD const* must = ir->getMustRef();
    if (must != nullptr) {
        MDSet const* pt = getPointTo(must->id(), mx);
        if (isWorstCase(pt)) { return pt; }
        set.bunion_pure(*pt, sbs);
    }
    MDSet const* may = ir->getMayRef();
    if (may == nullptr) { return worst; }
    MDSetIter iter;
    for (BSIdx i = may->get_first(&iter);
         i != BS_UNDEF; i = may->get_next(i, &iter)) {
        MD const* md = m_md_sys->getMD((MDIdx)i);
        ASSERT0(md);
        MDSet const* pt = getPointTo(md->id(), mx);
        if (isWorstCase(pt)) { return pt; }
        set.bunion_pure(*pt, sbs);
    }
    return nullptr;
}


//Dump 'ir' point-to according to 'mx'.
//'dump_kid': dump kid's memory object if it exist.
void AliasAnalysis::dumpIRPointTo(IR const* ir,
                                  bool dump_kid,
                                  MD2MDSet const* mx) const
{
    if (ir == nullptr || !m_rg->isLogMgrInit()) { return; }
    MD const* must = ir->getMustRef();
    MDSet const* may = ir->getMayRef();
    if (must != nullptr ||
        (may != nullptr && may->get_elem_count() > 0)) {
        dumpIR(ir, m_rg, nullptr, 0);
    }
    m_rg->getLogMgr()->incIndent(2);
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_OP:
    case IR_ID:
    SWITCH_CASE_READ_PR:
        if (must != nullptr) {
            dumpMD2MDSet(must, mx);
        }
        break;
    default:
        if (may != nullptr) {
            MDSetIter iter;
            for (BSIdx i = may->get_first(&iter);
                 i != BS_UNDEF; i = may->get_next(i, &iter)) {
                MD * md = m_md_sys->getMD((MDIdx)i);
                ASSERT0(md);
                dumpMD2MDSet(md, mx);
            }
        } else if (must != nullptr) {
            dumpMD2MDSet(must, mx);
        }
    }

    if (dump_kid) {
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * kid = ir->getKid(i);
            if (kid != nullptr) {
                dumpIRPointTo(kid, dump_kid, mx);
            }
        }
    }
    m_rg->getLogMgr()->decIndent(2);
}


void AliasAnalysis::dumpIRPointToForExtIR(IR const* ir, bool dump_kid,
                                          MD2MDSet const* mx) const
{
    switch (ir->getCode()) {
    SWITCH_CASE_EXT:
        if (ir->getCode() == IR_VSTPR) {
            dumpDirectStore(ir, dump_kid, mx);
            return;
        }
    default: UNREACHABLE();
    }
}


void AliasAnalysis::dumpDirectStore(IR const* ir, bool dump_kid,
                                    MD2MDSet const* mx) const
{
    prt(getRegion(), "LHS:");
    dumpIRPointTo(ir, false, mx);
    if (ir->getRHS() == nullptr) { return; }
    note(getRegion(), "\nRHS:");
    dumpIRPointTo(ir->getRHS(), false, mx);
    if (dump_kid) {
        note(getRegion(), "\n>> MDSet DETAIL:");
        dumpIRPointTo(ir->getRHS(), true, mx);
    }
}


void AliasAnalysis::dumpIRPointToForIR(IR const* ir, bool dump_kid,
                                       MD2MDSet const* mx) const
{
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR:
        dumpDirectStore(ir, dump_kid, mx);
        break;
    SWITCH_CASE_WRITE_ARRAY:
        prt(getRegion(), "LHS:");
        dumpIRPointTo(ir, false, mx);
        note(getRegion(), "\nRHS:");
        dumpIRPointTo(ir->getRHS(), false, mx);
        if (dump_kid) {
            note(getRegion(), "\n>> MDSet DETAIL:");
            dumpIRPointTo(ir->getBase(), true, mx);
            dumpIRPointTo(ir->getRHS(), true, mx);
            for (IR * p = ARR_sub_list(ir); p != nullptr; p = p->get_next()) {
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
    SWITCH_CASE_INDIRECT_MEM_STMT:
        prt(getRegion(), "LHS:");
        dumpIRPointTo(ir, false, mx);
        note(getRegion(), "\nRHS:");
        dumpIRPointTo(ir->getRHS(), false, mx);
        if (dump_kid) {
            note(getRegion(), "\n>> MDSet DETAIL:");
            dumpIRPointTo(ir->getBase(), true, mx);
            dumpIRPointTo(ir->getRHS(), true, mx);
        }
        break;
    case IR_CALL: {
        if (ir->hasReturnValue()) {
            prt(getRegion(), "LHS:");
            dumpIRPointTo(ir, false, mx);
        }

        UINT i = 0;
        for (IR * p = CALL_param_list(ir);
             p != nullptr; p = p->get_next()) {
            note(getRegion(), "\nPARAM%u:", i++);
            dumpIRPointTo(p, false, mx);
        }

        i = 0;
        for (IR * p = CALL_dummyuse(ir); p != nullptr; p = p->get_next()) {
            note(getRegion(), "\nDUMMY%u:", i++);
            dumpIRPointTo(p, false, mx);
        }

        if (dump_kid) {
            if (CALL_param_list(ir) != nullptr ||
                CALL_dummyuse(ir) != nullptr) {
                note(getRegion(), "\n>> MDSet DETAIL:\n");
            }

            for (IR * p = CALL_param_list(ir);
                 p != nullptr; p = p->get_next()) {
                dumpIRPointTo(p, true, mx);
            }

            for (IR * p = CALL_dummyuse(ir);
                 p != nullptr; p = p->get_next()) {
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
        ASSERT0(ICALL_callee(ir) != nullptr);
        prt(getRegion(), "CALLEE:");
        dumpIRPointTo(ICALL_callee(ir), false, mx);
        if (dump_kid && CALL_param_list(ir) != nullptr) {
            note(getRegion(), "\n>> MDSet DETAIL:\n");
            for (IR * p = CALL_param_list(ir); p ; p = p->get_next()) {
                dumpIRPointTo(p, true, mx);
            }
        }
        break;
    }
    SWITCH_CASE_CONDITIONAL_BRANCH_OP:
        ASSERT0(ir->getJudgeDet());
        prt(getRegion(), "DET:");
        dumpIRPointTo(ir->getJudgeDet(), false, mx);
        if (dump_kid) {
            note(getRegion(), "\n>> MDSet DETAIL:");
            dumpIRPointTo(ir->getJudgeDet(), true, mx);
        }
        break;
    case IR_SELECT:
        ASSERT0(ir->getJudgeDet());
        prt(getRegion(), "DET:");
        dumpIRPointTo(ir->getJudgeDet(), false, mx);
        if (dump_kid) {
            note(getRegion(), "\n>> MDSet DETAIL:");
            dumpIRPointTo(ir->getJudgeDet(), true, mx);
        }
        break;
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP:
    case IR_IGOTO:
        ASSERT0(ir->getValExp());
        prt(getRegion(), "VEXP:");
        dumpIRPointTo(ir->getValExp(), false, mx);
        if (dump_kid) {
            note(getRegion(), "\n>> MDSet DETAIL:");
            dumpIRPointTo(ir->getValExp(), true, mx);
        }
        break;
    case IR_RETURN:
        dumpIRPointTo(RET_exp(ir), false, mx);
        if (dump_kid && RET_exp(ir) != nullptr) {
            note(getRegion(), "\n>> MDSet DETAIL:");
            dumpIRPointTo(RET_exp(ir), true, mx);
        }
        break;
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
    case IR_GOTO:
    case IR_REGION:
        break;
    default: dumpIRPointToForExtIR(ir, dump_kid, mx);
    }
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
        mx = mapBBToMD2MDSet(bb->id());
    } else {
        mx = &m_unique_md2mds;
    }
    if (mx == nullptr) {
        //e.g: If one has performed PRE and generated new BB, but
        //not invoked the IRAA::perform(), then the mx of
        //the new BB is not constructed.

        //interwarn("In IRAA, MD2MDSet of BB%u is nullptr, may be new "
        //          "bb was generated. One should recall IRAA::perform()",
        //          bb->id());
        note(getRegion(), "\n-- BB%u's MD2MDSet is nullptr", bb->id());
        m_rg->getLogMgr()->decIndent(2);
        return;
    }

    note(getRegion(), "\n-- MD2MDSet: --", bb->id());
    dumpMD2MDSet(mx, false);

    if (BB_irlist(bb).get_head(&ct) != nullptr) {
        note(getRegion(), "\n\n-- IR POINT-TO: --");
    }
    for (IR * ir = BB_irlist(bb).get_head(&ct);
         ir != nullptr; ir = BB_irlist(bb).get_next(&ct)) {
        note(getRegion(), "\n---------------------------------");
        dumpIRList(ir, m_rg, nullptr, IR_DUMP_KID | IR_DUMP_SRC_LINE);
        note(getRegion(), "\n");
        dumpIRPointToForIR(ir, dump_kid, mx);
    }
    m_rg->getLogMgr()->decIndent(2);
}


//Dump all relations between IR, MD, and MDSet.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void AliasAnalysis::dumpIRPointToForRegion(bool dump_kid) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n-- DUMP IR POINT-TO FOR BB --");
    BBList * bbl = m_cfg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        dumpIRPointToForBB(bb, dump_kid);
    }
}


bool AliasAnalysis::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    START_TIMER_FMT(t, ("DUMP %s", getPassName()));
    note(getRegion(), "\n==---- DUMP %s '%s' ----==", getPassName(),
         m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    m_md_sys->dump(m_vm, false);
    dumpWorstCase();
    dumpMD2MDSetForRegion(false);
    dumpIRPointToForRegion(true);
    m_rg->getLogMgr()->decIndent(2);
    Pass::dump();
    END_TIMER_FMT(t, ("DUMP %s", getPassName()));
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
        for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
            note(getRegion(), "\n--- BB%u ---", bb->id());
            dumpMD2MDSet(mapBBToMD2MDSet(bb->id()),
                        false); //each BB has its own graph.
        }
        return;
    }
    note(getRegion(),
         "\n==---- DUMP POINT-TO OUT-SET (FLOW-INSENSITIVE) '%s' ----==",
         m_rg->getRegionName());
    dumpMD2MDSet(&m_unique_md2mds, dump_pt_graph);
}


//Dump MD's point-to according to individual 'mx'.
//'dump_ptg': dump POINT-TO graph.
void AliasAnalysis::dumpMD2MDSet(MD2MDSet const* mx, bool dump_ptg) const
{
    if (!m_rg->isLogMgrInit() || mx == nullptr) { return; }
    xcom::Graph g;
    MDId2MD const* id2md = m_md_sys->getID2MDMap();
    for (VecIdx i = MD_FIRST; i <= id2md->get_last_idx(); i++) {
        if (id2md->get((UINT)i) == nullptr) { continue; }

        MDSet const* mds = getPointTo((UINT)i, *mx);
        if (mds != nullptr) {
            note(getRegion(), "\nMD%u -- PT_SET: ", (UINT)i);
            MDSetIter iter;
            for (BSIdx j = mds->get_first(&iter);
                 j != BS_UNDEF; j = mds->get_next(j, &iter)) {
                ASSERT0(m_md_sys->getMD((MDIdx)j));
                prt(getRegion(), "MD%u,", (MDIdx)j);
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


//Dump the worst case MDSet.
void AliasAnalysis::dumpWorstCase() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n-- DUMP WORST CASE --\n");
    m_rg->getLogMgr()->incIndent(2);
    ASSERT0(getWorstCase());
    getWorstCase()->dump(m_md_sys, m_vm, true);
    m_rg->getLogMgr()->decIndent(2);
}


//Dump relations between MD, MDSet.
//'md': candidate to dump.
//'md2mds': mapping from 'md' to an md-set it pointed to.
void AliasAnalysis::dumpMD2MDSet(MD const* md, MD2MDSet const* mx) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    StrBuf buf(64);
    note(getRegion(), "\n%s", md->dump(buf, m_vm));

    //Dump MDSet of 'md'.
    MDSet const* pts = getPointTo(md->id(), *mx);
    m_rg->getLogMgr()->incIndent(2);
    note(getRegion(), "\nPOINT TO:");
    if (pts != nullptr) {
        ASSERT0(!pts->is_empty());
        MDSetIter iter;
        m_rg->getLogMgr()->incIndent(2);
        for (BSIdx j = pts->get_first(&iter);
             j != BS_UNDEF; j = pts->get_next(j, &iter)) {
            MD const* mmd = m_md_sys->getMD((MDIdx)j);
            ASSERT0(mmd);
            buf.clean();
            note(getRegion(), "\n%s", mmd->dump(buf, m_vm));
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
    MDSet const* from_md_pts = nullptr;
    for (UINT fromid = mx->get_first(mxiter, &from_md_pts);
         fromid > MD_UNDEF; fromid = mx->get_next(mxiter, &from_md_pts)) {
        ASSERT0(m_md_sys->getMD(fromid));
        if (from_md_pts == nullptr) { continue; }
        if (from_md_pts->is_contain_fullmem()) {
            PtPair const* pp = ppmgr.add(fromid, MD_FULL_MEM);
            ASSERT0(pp);
            pps->bunion(PP_id(pp), *ppsetmgr.getSBSMgr());
            continue;
        }
        MDSetIter segiter;
        for (BSIdx toid = from_md_pts->get_first(&segiter);
             toid != BS_UNDEF;
             toid = from_md_pts->get_next(toid, &segiter)) {
            ASSERT0(m_md_sys->getMD((MDIdx)toid));
            PtPair const* pp = ppmgr.add(fromid, (UINT)toid);
            ASSERT0(pp);
            pps->bunion(PP_id(pp), *ppsetmgr.getSBSMgr());
        }
    }
    return true;
}


void AliasAnalysis::convertPT2MD2MDSet(PtPairSet const& pps,
                                       PtPairMgr const& ppmgr,
                                       MOD MD2MDSet * ctx)
{
    PtPairSetIter iter;
    for (BSIdx i = pps.get_first(&iter);
         i != BS_UNDEF; i = pps.get_next(i, &iter)) {
        PtPair * pp = const_cast<PtPairMgr&>(ppmgr).get((UINT)i);
        ASSERT0(pp != nullptr);
        setPointToMDSetByAddMD(PP_from(pp), *ctx, m_md_sys->getMD(PP_to(pp)));
     }
}


void AliasAnalysis::computeExtStmt(MOD IR * ir, MOD MD2MDSet * mx)
{
    ASSERT0(ir->is_stmt());
    switch (ir->getCode()) {
    case IR_VST:
    case IR_VSTPR:
        processDirectMemOp(ir, mx);
        return;
    case IR_VIST:
        processIndirectMemOp(ir, mx);
        return;
    default: UNREACHABLE();
    }
}


void AliasAnalysis::computeStmt(MOD IR * ir, MOD MD2MDSet * mx)
{
    switch (ir->getCode()) {
    SWITCH_CASE_DIRECT_MEM_STMT:
    case IR_STPR:
        processDirectMemOp(ir, mx);
        break;
    SWITCH_CASE_INDIRECT_MEM_STMT:
        processIndirectMemOp(ir, mx);
        break;
    SWITCH_CASE_WRITE_ARRAY:
        processStoreArray(ir, mx);
        break;
    case IR_SETELEM:
        processSetElem(ir, mx);
        break;
    case IR_GETELEM:
        processGetElem(ir, mx);
        break;
    SWITCH_CASE_CALL:
        processCall(ir, mx);
        break;
    case IR_GOTO:
        ASSERT0(ir == ir->getBB()->getLastIR());
        break;
    case IR_IGOTO: {
        ASSERT0(ir == ir->getBB()->getLastIR());
        MDSet tmp;
        AACtx ic;
        inferExpression(ir->getValExp(), tmp, &ic, mx);
        tmp.clean(*getSBSMgr());
        break;
    }
    case IR_PHI:
        processPhi(ir, mx);
        break;
    case IR_REGION:
        processRegion(ir, mx);
        break;
    SWITCH_CASE_CONDITIONAL_BRANCH_OP: {
        ASSERT0(ir == ir->getBB()->getLastIR());
        MDSet tmp;
        AACtx ic;
        inferExpression(ir->getJudgeDet(), tmp, &ic, mx);
        tmp.clean(*getSBSMgr());;
        break;
    }
    case IR_RETURN:
        ASSERT0(ir == ir->getBB()->getLastIR());
        processReturn(ir, mx);
        break;
    SWITCH_CASE_MULTICONDITIONAL_BRANCH_OP: {
        ASSERT0(ir == ir->getBB()->getLastIR());
        MDSet tmp;
        AACtx ic;
        inferExpression(ir->getValExp(), tmp, &ic, mx);
        tmp.clean(*getSBSMgr());
        break;
    }
    default: computeExtStmt(ir, mx);
    }
}


//Solving POINT-TO out set.
//While the function terminiate, OUT info has been recorded
//in related 'mx'.
void AliasAnalysis::computeBB(IRBB const* bb, MOD MD2MDSet * mx)
{
    ASSERT0(mx != nullptr);
    IRListIter ct;
    IRBB * pbb = const_cast<IRBB*>(bb); //ensure we do not moidy it.
    for (IR * ir = pbb->getIRList().get_head(&ct);
         ir != nullptr; ir = pbb->getIRList().get_next(&ct)) {
        computeStmt(ir, mx);
    }
}


bool AliasAnalysis::verifyExtIR(IR * ir)
{
    switch (ir->getCode()) {
    SWITCH_CASE_EXT:
        return true;
    default:;
    }
    return true;
}


bool AliasAnalysis::verifyIR(IR * ir)
{
    switch (ir->getCode()) {
    case IR_ID:
    SWITCH_CASE_PR_OP:
    SWITCH_CASE_DIRECT_MEM_OP:
        ASSERT0(ir->getMustRef());
        ASSERT0(ir->getMayRef() == nullptr);
        break;
    SWITCH_CASE_ARRAY_OP: {
        if (ir->getParent() && ir->getParent()->is_array()) {
            //Compute the memory address and ONLY
            //record the top level ARRAY node's memory address.
            break;
        }
        MD const* mustref = ir->getMustRef();
        MDSet const* mayref = ir->getMayRef();
        ASSERT0(mustref || (mayref && !mayref->is_empty()));
        ASSERT0((mustref != nullptr) ^ (mayref && !mayref->is_empty()));
        if (mustref != nullptr) {
            //PR's address can not be taken.
            ASSERT0(!mustref->is_pr());
        }
        if (mayref != nullptr) {
            //PR's address can not be taken.
            MDSetIter iter;
            for (BSIdx i = mayref->get_first(&iter);
                 i != BS_UNDEF; i = mayref->get_next(i, &iter)) {
                MD const* x = m_md_sys->getMD((MDIdx)i);
                ASSERT0_DUMMYUSE(x);
                ASSERT0(!x->is_pr());
            }
        }
        break;
    }
    SWITCH_CASE_INDIRECT_MEM_OP: {
        MD const* mustref = ir->getMustRef();
        MDSet const* mayref = ir->getMayRef();
        ASSERT0(mustref || (mayref && !mayref->is_empty()));
        ASSERT0((mustref != nullptr) ^ (mayref && !mayref->is_empty()));
        if (mustref != nullptr) {
            //PR's address can not be taken.
            ASSERT0(!mustref->is_pr());
        }
        if (mayref != nullptr) {
            //PR's address can not be taken.
            MDSetIter iter;
            for (BSIdx i = mayref->get_first(&iter);
                 i != BS_UNDEF; i = mayref->get_next(i, &iter)) {
                MD const* x = m_md_sys->getMD((MDIdx)i);
                ASSERT0_DUMMYUSE(x);
                ASSERT0(!x->is_pr());
            }
        }
        break;
    }
    SWITCH_CASE_CALL:
        if (ir->hasReturnValue()) {
            ASSERT0(ir->getMustRef());
        }
        for (IR * p = CALL_param_list(ir); p != nullptr; p = p->get_next()) {
            verifyIR(p);
        }
        for (IR * p = CALL_dummyuse(ir); p != nullptr; p = p->get_next()) {
            verifyIR(p);
        }
        break;
    default:
        ASSERT0(ir->getMustRef() == nullptr);
        ASSERT0(ir->getMayRef() == nullptr);
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid != nullptr) {
            verifyIR(kid);
        }
    }
    return true;
}


bool AliasAnalysis::verify()
{
    BBList * bbl = m_cfg->getBBList();
    for (IRBB * bb = bbl->get_head();
         bb != nullptr; bb = bbl->get_next()) {
        for (IR * ir = BB_first_ir(bb);
             ir != nullptr; ir = BB_next_ir(bb)) {
            verifyIR(ir);
        }
    }
    return true;
}


//Return false if flow sensitive analysis is inproperly.
bool AliasAnalysis::computeFlowSensitiveBB(IRBB const* bb, PPSetMgr & ppsetmgr,
                                           DefMiscBitSetMgr * sbsmgr,
                                           BitSet & is_bb_changed,
                                           PtPairSet * tmp, bool first,
                                           OUT bool & change)
{
    #ifdef PARTIAL_UPDATE
    if (!is_bb_changed.is_contain(bb->id())) { return true; }
    #endif
    PtPairSet * pps = getInPtPairSet(bb);
    MD2MDSet * md2mds = genMD2MDSetForBB(bb->id());
    tmp->clean(*sbsmgr);
    xcom::EdgeC * el = m_cfg->getVertex(bb->id())->getInList();
    bool compute = true;
    if (el != nullptr) {
        for (; el != nullptr; el = el->get_next()) {
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
    if (!compute && !first) { return true; }

    computeBB(bb, md2mds);
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
        for (xcom::EdgeC * el = m_cfg->getVertex(bb->id())->getOutList();
             el != nullptr; el = el->get_next()) {
            IRBB * s = m_cfg->getBB(el->getToId());
            ASSERT0(s);
            is_bb_changed.bunion(s->id());
        }
    }
    return true;
}


//This method is accurate than Andersen's algo.
//NOTICE: Do NOT clean 'md2mds' of BB at the last iter,
//it supplied the POINT TO information for subsequently
//optimizations.
//Return false if flow sensitive analysis is inproperly.
bool AliasAnalysis::computeFlowSensitive(RPOVexList const& vexlst,
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
        RPOVexListIter it = nullptr;
        for (Vertex const* v = vexlst.get_head(&it);
             v != nullptr; v = vexlst.get_next(&it)) {
            if (!computeFlowSensitiveBB(m_cfg->getBB(v->id()), ppsetmgr,
                                        sbsmgr, is_bb_changed, tmp, first,
                                        change)) {
                return false;
            }
        }
    }
    ASSERTN(!change, ("Iterate too many times"));
    return true;
}


MD const* AliasAnalysis::genRestrictDummyVar(Var * var, MD2MDSet * mx)
{
    ASSERT0(var->is_restrict());
    //If variable is restrict, we allocate an individual variable to
    //distinguish its point-to set with other variables.
    MD const* dmd = m_dedicated_var2md.get(var);
    if (dmd != nullptr) { return dmd; }
    CHAR name[64];
    SNPRINTF(name, 63, "DummyGlobalVarPointedByVAR%u", var->id());
    ASSERT0(::strlen(name) < 64);
    Var * tv = m_rg->getVarMgr()->registerVar(name, m_tm->getMCType(0),
                                              0, VAR_GLOBAL|VAR_ADDR_TAKEN);

    //Set the var to be unallocable, means do NOT add
    //var immediately as a memory-variable.
    //For now, it is only be regarded as a pseduo-register.
    //And set it to allocable if the PR is in essence need to be
    //allocated in memory.
    tv->setFlag(VAR_IS_UNALLOCABLE);
    m_rg->addToVarTab(tv);

    MD md;
    MD_base(&md) = tv;
    MD_ofst(&md) = 0;
    MD_ty(&md) = MD_UNBOUND; //restrict target MD is dummy, should be unbound.
    dmd = m_md_sys->registerMD(md);
    ASSERT0(dmd->id() > MD_UNDEF);
    m_dedicated_var2md.set(var, dmd);
    return dmd;
}


void AliasAnalysis::initPTSForRegisterMD(MDTab * mdt, MD const* dmd,
                                         MD2MDSet * mx)
{
    ASSERT0(mdt);
    MD const* x = mdt->get_effect_md();
    if (x != nullptr) {
        if (dmd != nullptr) {
            ASSERTN(getPointTo(x->id(), *mx) == nullptr ||
                    getPointTo(x->id(), *mx)->is_empty(),
                    ("should already be clean"));
            setPointToMDSetByAddMD(x->id(), *mx, dmd);
        } else {
            setPointTo(x->id(), *mx, getWorstCase());
        }
    }

    OffsetTab * ofstab = mdt->get_ofst_tab();
    ASSERT0(ofstab);
    if (ofstab->get_elem_count() == 0) { return; }
    ConstMDIter it;
    for (MD const* md = ofstab->get_first(it, nullptr);
         md != nullptr; md = ofstab->get_next(it, nullptr)) {
        if (dmd != nullptr) {
            ASSERTN(getPointTo(md->id(), *mx) == nullptr ||
                    getPointTo(md->id(), *mx)->is_empty(),
                    ("should already be clean"));
            setPointToMDSetByAddMD(md->id(), *mx, dmd);
            continue;
        }
        setPointTo(md->id(), *mx, getWorstCase());
    }
}


//The function initialize the POINT-TO set of pointer.
//The pointer includes global/local pointer and formal parameter pointer.
//var: the given variable which should be pointer.
//Note May-POINT-TO set must be available before call this function.
void AliasAnalysis::initPTS(Var * var, MD2MDSet * mx)
{
    //Record dedicated MD which 'var' pointed to.
    MD const* dmd = nullptr;
    if (var->is_restrict()) {
        dmd = genRestrictDummyVar(var, mx);
    }
    MDTab * mdt = m_md_sys->getMDTab(var);
    if (mdt != nullptr) {
        initPTSForRegisterMD(mdt, dmd, mx);
        return;
    }
    MD md;
    MD_base(&md) = var;
    MD_ofst(&md) = 0;
    MD_size(&md) = var->getByteSize(m_tm);
    MD_ty(&md) = MD_EXACT;
    MD const* entry = m_md_sys->registerMD(md);
    if (dmd != nullptr) {
        ASSERTN(getPointTo(entry->id(), *mx) == nullptr ||
                getPointTo(entry->id(), *mx)->is_empty(),
                ("should already be clean"));
        setPointToMDSetByAddMD(entry->id(), *mx, dmd);
    } else {
        setPointTo(entry->id(), *mx, getWorstCase());
    }
}


//Determine if flow sensitive analysis is properly.
bool AliasAnalysis::isFlowSensitiveProperly()
{
    ASSERT0(m_cfg->getEntry());
    MD2MDSet * mx = genMD2MDSetForBB(m_cfg->getEntry()->id());
    ASSERTN(mx, ("invoke initEntryPTS before here"));
    UINT num_of_tgt_md = mx->computePtPairNum(*m_md_sys);
    num_of_tgt_md = (num_of_tgt_md * mx->get_elem_count() /
                    HOST_BIT_PER_BYTE + 1) *
                    HOST_BIT_PER_BYTE / HOST_BIT_PER_BYTE;
    return num_of_tgt_md < g_thres_ptpair_num;
}


void AliasAnalysis::initBBPPSet(PPSetMgr & ppsetmgr)
{
    BBList * bblst = m_cfg->getBBList();
    //Make vector to accommodate the maximum BB id.
    UINT bbnum = bblst->get_elem_count();
    if (m_in_pp_set.get_capacity() < bbnum) {
        m_in_pp_set.grow(bbnum);
        m_out_pp_set.grow(bbnum);
        m_md2mds_vec.grow(bbnum);
    }

    BBListIter ct;
    for (IRBB * bb = bblst->get_head(&ct);
         bb != nullptr; bb = bblst->get_next(&ct)) {
        m_in_pp_set.set(bb->id(), ppsetmgr.allocPtPairSet());
        m_out_pp_set.set(bb->id(), ppsetmgr.allocPtPairSet());
    }
}


void AliasAnalysis::initReferencedVarPTS(PPSetMgr & ppsetmgr, MD2MDSet * mx)
{
    BBListIter ct;
    BBList const* bblst = m_cfg->getBBList();
    ConstIRIter irit;
    VarTab visited;
    for (IRBB const* bb = bblst->get_head(&ct);
         bb != nullptr; bb = bblst->get_next(&ct)) {
        BBIRList const& irlst = const_cast<IRBB*>(bb)->getIRList();
        BBIRListIter bbirit;
        for (IR const* ir = irlst.get_head(&bbirit); ir != nullptr;
             ir = irlst.get_next(&bbirit)) {
            irit.clean();
            for (IR const* t = xoc::iterInitC(ir, irit, false);
                 t != nullptr; t = xoc::iterNextC(irit, true)) {
                if (!t->hasIdinfo()) { continue; }
                Var * var = t->getIdinfo();
                ASSERT0(var);
                //Variable with ANY-type may be pointer. However, we deal with
                //its point-to set while processing LD/PR instead of
                //initializing point-to set at once.
                //if (!v->is_any()) { continue; }

                if (!var->is_pointer() || visited.find(var)) { continue; }
                initPTS(var, mx);
                visited.append(var);
            }
        }
    }
}


void AliasAnalysis::initDedicatedVarPTS(PPSetMgr & ppsetmgr, MD2MDSet * mx)
{
    //Set fullmem points to fullmem.
    setPointTo(MD_FULL_MEM, MD_FULL_MEM, *mx);

    setPointTo(MD_GLOBAL_VAR, MD_GLOBAL_VAR, *mx);
    setPointTo(MD_GLOBAL_VAR, MD_IMPORT_VAR, *mx);
    setPointTo(MD_GLOBAL_VAR, MD_LOCAL_MAY_ALIAS, *mx);

    setPointTo(MD_LOCAL_VAR, MD_GLOBAL_VAR, *mx);
    setPointTo(MD_LOCAL_VAR, MD_IMPORT_VAR, *mx);
    setPointTo(MD_LOCAL_VAR, MD_LOCAL_MAY_ALIAS, *mx);

    setPointTo(MD_LOCAL_MAY_ALIAS, MD_GLOBAL_VAR, *mx);
    setPointTo(MD_LOCAL_MAY_ALIAS, MD_IMPORT_VAR, *mx);
    setPointTo(MD_LOCAL_MAY_ALIAS, MD_LOCAL_MAY_ALIAS, *mx);
    initReferencedVarPTS(ppsetmgr, mx);
}


void AliasAnalysis::initFlowSensitiveEntryPTS(PPSetMgr & ppsetmgr)
{
    ASSERT0(m_cfg->verify());
    ASSERT0(m_cfg->getBBList()->get_elem_count() != 0);
    initBBPPSet(ppsetmgr);
    IRBB * entry = m_cfg->getEntry();
    ASSERT0(entry);
    MD2MDSet * mx = genMD2MDSetForBB(entry->id());
    initDedicatedVarPTS(ppsetmgr, mx);
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
//pts_arr: used to record all the PtPair set. It will be deleted by caller.
void AliasAnalysis::initEntryPTS(PPSetMgr & ppsetmgr)
{
    if (m_flow_sensitive) {
        initFlowSensitiveEntryPTS(ppsetmgr);
        return;
    }
    //Flow insenstive initialzation.
    initDedicatedVarPTS(ppsetmgr, &m_unique_md2mds);
}


//This function initialize May Point-To set.
//Note that this function should only be invoked once.
void AliasAnalysis::initMayPointToSet()
{
    //Record MDs whose address have been takens or it is global variable.
    Region * rg = m_rg;
    VarTabIter c;
    bool has_local_var_addr_taken = false;
    bool has_import_var_addr_taken = false;
    for (; !rg->is_program();) {
        VarTab * vtab = rg->getVarTab();
        c.clean();
        for (Var * v = vtab->get_first(c); v != nullptr;
             v = vtab->get_next(c)) {
            if (!v->is_taken_addr()) { continue; }
            if (rg == m_rg) {
                has_local_var_addr_taken = true;
            } else {
                has_import_var_addr_taken = true;
            }
            ASSERT0(!v->is_global());
        }
        rg = rg->getParent();
        if (rg == nullptr || rg->is_program()) {
            break;
        }
        ASSERT0(rg->is_inner() || rg->is_function() || rg->is_eh());
    }
    MDSet tmp;
    tmp.bunion(MD_GLOBAL_VAR, *getSBSMgr());
    if (has_import_var_addr_taken) {
        tmp.bunion(MD_IMPORT_VAR, *getSBSMgr());
    }
    if (has_local_var_addr_taken) {
        tmp.bunion(MD_LOCAL_MAY_ALIAS, *getSBSMgr());
    }
    m_maypts = m_mds_hash->append(tmp);
    tmp.clean(*getSBSMgr());
}


void AliasAnalysis::computeFlowInsensitive()
{
    BBList * bbl = m_cfg->getBBList();
    BBListIter ct = nullptr;
    UINT c = 0;
    while (++c < 3) {
        //Compute point-to.
        //Compute which MD memory-op represented.
        for (IRBB const* bb = bbl->get_head(&ct);
             bb != nullptr; bb = bbl->get_next(&ct)) {
            computeBB(bb, &m_unique_md2mds);
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
bool AliasAnalysis::perform(MOD OptCtx & oc)
{
    ASSERTN(getWorstCase(), ("Should invoke initAliasAnalysis() first."));
    if (m_cfg->getBBList()->get_elem_count() == 0) { return true; }
    START_TIMER(t, getPassName());

    //Initialization.
    m_ppmgr.init();

    //Clean data structures used for analysis.
    clean();

    //Both sensitive and insensitive analysis need loop and scc info to
    //make point-to computation more accurately.
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_LOOP_INFO,
                                               PASS_SCC, PASS_UNDEF);
    ASSERTN(oc.is_loopinfo_valid(),
            ("infer pointer arith need loop info"));
    ASSERT0(oc.is_scc_valid());
    m_scc = (GSCC*)m_rg->getPassMgr()->queryPass(PASS_SCC);
    m_prssamgr = m_rg->getPRSSAMgr();
    ASSERT0(m_scc);

    //We allocate PtPairSet at each call of AA,
    //because AA would not be invoked frequently.
    if (m_flow_sensitive) {
        PPSetMgr ppsetmgr;
        initEntryPTS(ppsetmgr);
        m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_RPO, PASS_UNDEF);
        RPOVexList * tbbl = m_cfg->getRPOVexList();
        ASSERT0(tbbl);
        ASSERT0(tbbl->get_elem_count() == m_cfg->getBBList()->get_elem_count());

        START_TIMER_FMT(t2, ("%s:flow sensitive analysis", getPassName()));
        bool is_succ = computeFlowSensitive(*tbbl, ppsetmgr);
        END_TIMER_FMT(t2, ("%s:flow sensitive analysis", getPassName()));

        if (!is_succ) {
            //Flow sensitive is inproperly, perform insensitive analysis.
            m_flow_sensitive = false;
            START_TIMER_FMT(t4, ("%s:flow insensitive analysis",
                                 getPassName()));
            initEntryPTS(ppsetmgr);
            computeFlowInsensitive();
            END_TIMER_FMT(t4, ("%s:flow insensitive analysis", getPassName()));
        }
    } else {
        PPSetMgr ppsetmgr;
        START_TIMER_FMT(t3, ("%s:flow insensitive analysis", getPassName()));
        initEntryPTS(ppsetmgr);
        computeFlowInsensitive();
        END_TIMER_FMT(t3, ("%s:flow insensitive analysis", getPassName()));
    }
    OC_is_aa_valid(oc) = true;
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpAA()) {
        dump();
    }
    ASSERT0(verify());

    //DU info does not depend on these data structures.
    //Since AA info is not always be queried after perform(), we destroy the
    //data structure to release memory.
    m_ppmgr.clean();
    cleanSBSMgr();
    destroyContext();
    END_TIMER(t, getPassName());
    return true;
}

} //namespace xoc
